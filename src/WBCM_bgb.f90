!:=============================================================================
! This file is part of WBCM (Western Bahia Biomass Model).

! Copyright (C) 2018 Fernando Martins Pimenta
!                    Emily Ane Dionizio

! WBCM is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! WBCM is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with WBCM.  If not, see <http://www.gnu.org/licenses/>.

!:=============================================================================
! About Authors:
! Fernando Martins Pimenta
!  Student of Surveying and Cartographic Engineering
!  Federal University of Viçosa - Brazil
!
!  Bachelor of Biosystems Engineer
!  Federal University of São João del-Rei - Brazil
!
! Emily Ane Dionizio
!  Biologist
!  FATEA - SP - Brazil
!
!  MS in Agricultural Meteorology
!  Federal University of Viçosa - Brazil
!  
!  Doctoral student in Meteorology - UFV
!  Federal University of Viçosa - Brazil
!
! Research Group on Atmosphere-Biosphere Interaction
! Federal University of Viçosa
!
! Contacts: fernando.m.pimenta@gmail.com, fernando.m.pimenta@ufv.br
!           emilyy.ane@gmail.com
!:=============================================================================

subroutine genInitialBGB(bgb_before, pre_bgb, lu_before)
  ! Calculate the below ground biomass for the first year of the landuse time serie

  !Variables
    ! pre_bgb: Below ground biomass in the past
    ! lu_before: Landuse classification of the first year of time serie
    ! avgBGB: Average below ground biomass of the classification dataset
   
    !Output
      ! bgb_before: Below ground biomass of the first year of the landuse time serie

  type(nc2d_float_lld) :: pre_bgb, bgb_before
  type(nc2d_byte_lld) :: lu_before

  integer(kind=4) :: i, j

  bgb_before = pre_bgb

  do i = 1, lu_before%nlons
    !$omp parallel do private(j)
    do j = 1, lu_before%nlats
      if(lu_before%ncdata(i,j).le.3.and.lu_before%ncdata(i,j).ne.lu_before%FillValue) then
        bgb_before%ncdata(i,j) = pre_bgb%ncdata(i,j)
      else if(lu_before%ncdata(i,j).eq.4) then
        bgb_before%ncdata(i,j) = avgBGB(4)
      else if(lu_before%ncdata(i,j).eq.5) then
        bgb_before%ncdata(i,j) = avgBGB(5)
      else if(lu_before%ncdata(i,j).eq.6) then
        bgb_before%ncdata(i,j) = avgBGB(6)
      else if(lu_before%ncdata(i,j).eq.7) then
        bgb_before%ncdata(i,j) = avgBGB(7)
      else if(lu_before%ncdata(i,j).eq.8.or.lu_before%ncdata(i,j).eq.9) then
        bgb_before%ncdata(i,j) = 0.0
      else
        bgb_before%ncdata(i,j) = -9999.0
      end if
    end do
    !$omp end parallel do
  end do

  bgb_before%varunits = 't ha-1'

end subroutine genInitialBGB

!Calcula BGB atual
subroutine genBGB(bgb_after, bgb_before, lu_after, lu_before)
  ! calculates the below-ground biomass of the first year of 
  ! the land use series considering the biomass calculated before.

  !variables
    ! pre_before: below ground biomass of a year before
    ! lu_after: landuse classification of the next year
    ! lu_before: landuse classification of the year before
    ! avgbgb: average below ground biomass of the classification dataset
    
    !output
     ! bgb_after: below ground biomass of next year

  type(nc2d_float_lld) :: bgb_after, bgb_before
  type(nc2d_byte_lld) :: lu_after, lu_before

  integer(kind=4) :: i, j

  bgb_after = bgb_before

  where(lu_after%ncdata.eq.lu_before%ncdata) bgb_after%ncdata = 0.0

  do i = 1, bgb_before%nlons
    !$omp parallel do private(j)
    do j = 1, bgb_before%nlats
    !Change: natural -> natural
      !Forest -> Savanna, grasslands
      if(lu_before%ncdata(i,j).eq.1.and.lu_after%ncdata(i,j).ge.2.and.lu_after%ncdata(i,j).le.3) then
        bgb_after%ncdata(i,j) = avgBGB(lu_after%ncdata(i,j)) - bgb_before%ncdata(i,j)
      !Savanna -> grasslands
      else if(lu_before%ncdata(i,j).eq.2.and.lu_after%ncdata(i,j).eq.3) then
        bgb_after%ncdata(i,j) = avgBGB(lu_after%ncdata(i,j)) - bgb_before%ncdata(i,j)
      !Savanna -> forest
      else if(lu_before%ncdata(i,j).eq.2.and.lu_after%ncdata(i,j).eq.1) then
        bgb_after%ncdata(i,j) = (avgBGB(lu_after%ncdata(i,j)) - bgb_before%ncdata(i,j)) + 0.485*cbGrow(lu_after%ncdata(i,j))
      !Grasslands -> forest, savanna 
      else if(lu_before%ncdata(i,j).eq.3.and.lu_after%ncdata(i,j).eq.1.or.lu_after%ncdata(i,j).eq.2) then
        bgb_after%ncdata(i,j) = (avgBGB(lu_after%ncdata(i,j)) - bgb_before%ncdata(i,j)) + 0.485*cbGrow(lu_after%ncdata(i,j))
    !Change: natural -> crop
      else if(lu_before%ncdata(i,j).le.3.and.lu_after%ncdata(i,j).ge.4.and.lu_after%ncdata(i,j).lt.8) then
        !Não usa taxa de crescimento anual. Foi considerada nula porque o incremento é igual a perda anual
        bgb_after%ncdata(i,j) = avgBGB(lu_after%ncdata(i,j)) - bgb_before%ncdata(i,j)
      !Change: Crop -> crop
      else if(lu_before%ncdata(i,j).ge.4.and.lu_before%ncdata(i,j).lt.8.and. &
              lu_after%ncdata(i,j).ge.4.and.lu_after%ncdata(i,j).lt.8) then
        bgb_after%ncdata(i,j) = avgBGB(lu_after%ncdata(i,j)) - bgb_before%ncdata(i,j)
      !Change: Crop -> natural (vegetation regrowth)
      else if(lu_before%ncdata(i,j).ge.4.and.lu_before%ncdata(i,j).lt.8.and.lu_after%ncdata(i,j).le.3) then
        !Please check NPP units in initial parameters
        bgb_after%ncdata(i,j) = avgBGB(lu_after%ncdata(i,j)) - bgb_before%ncdata(i,j)
      end if
    end do
    !$omp end parallel do
  end do
  
  bgb_before = bgb_after

end subroutine genBGB
