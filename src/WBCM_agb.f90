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

!String to int
!elemental subroutine str2int(str,int,stat)
!  ! Arguments
!  character(len=*),intent(in) :: str
!  integer,intent(out)         :: int
!  integer,intent(out)         :: stat
!
!  read(str,*,iostat=stat)  int
!end subroutine str2int

subroutine genInitialAGB(agb_before, pre_agb, lu_before)
  ! Calculate the above ground biomass for the first year of the landuse time serie

  !Variables
    ! pre_agb: Above ground biomass in the past
    ! lu_before: Landuse classification of the first year of time serie
    ! avgAGB: Average above ground biomass of the classification dataset
   
    !Output
      ! agb_before: Above ground biomass of the first year of the landuse time serie

  type(nc2d_float_lld) :: pre_agb, agb_before
  type(nc2d_byte_lld) :: lu_before

  integer(kind=4) :: i, j

  agb_before = pre_agb

  do i = 1, lu_before%nlons
    !$omp parallel do private(j)
    do j = 1, lu_before%nlats
      if(lu_before%ncdata(i,j).le.3.and.lu_before%ncdata(i,j).ne.lu_before%FillValue) then
        agb_before%ncdata(i,j) = pre_agb%ncdata(i,j)
      else if(lu_before%ncdata(i,j).eq.4) then
        agb_before%ncdata(i,j) = avgAGB(4)
      else if(lu_before%ncdata(i,j).eq.5) then
        agb_before%ncdata(i,j) = avgAGB(5)
      else if(lu_before%ncdata(i,j).eq.6) then
        agb_before%ncdata(i,j) = avgAGB(6)
      else if(lu_before%ncdata(i,j).eq.7) then
        agb_before%ncdata(i,j) = avgAGB(7)
      else if(lu_before%ncdata(i,j).eq.8.or.lu_before%ncdata(i,j).eq.9) then
        agb_before%ncdata(i,j) = 0.0
      else
        agb_before%ncdata(i,j) = -9999.0
      end if
    end do
    !$omp end parallel do
  end do

  agb_before%varunits = 't ha-1'

end subroutine genInitialAGB

!Calcula AGB atual
subroutine genAGB(agb_after, agb_before, lu_after, lu_before)
  ! calculates the above-ground biomass of the first year of 
  ! the land use series considering the biomass calculated before.

  !variables
    ! pre_before: above ground biomass of a year before
    ! lu_after: landuse classification of the next year
    ! lu_before: landuse classification of the year before
    ! avgagb: average above ground biomass of the classification dataset
    
    !output
     ! agb_after: above ground biomass of next year

  type(nc2d_float_lld) :: agb_after, agb_before
  type(nc2d_byte_lld) :: lu_after, lu_before

  integer(kind=4) :: i, j

  agb_after = agb_before

  where(lu_after%ncdata.eq.lu_before%ncdata) agb_after%ncdata = 0.0

  do i = 1, agb_before%nlons
    !$omp parallel do private(j)
    do j = 1, agb_before%nlats
    !Change: natural -> natural
      !Forest -> Savanna, grasslands
      if(lu_before%ncdata(i,j).eq.1.and.lu_after%ncdata(i,j).ge.2.and.lu_after%ncdata(i,j).le.3) then
        agb_after%ncdata(i,j) = avgAGB(lu_after%ncdata(i,j)) - agb_before%ncdata(i,j)
      !Savanna -> grasslands
      else if(lu_before%ncdata(i,j).eq.2.and.lu_after%ncdata(i,j).eq.3) then
        agb_after%ncdata(i,j) = avgAGB(lu_after%ncdata(i,j)) - agb_before%ncdata(i,j)
      !Savanna -> forest
      else if(lu_before%ncdata(i,j).eq.2.and.lu_after%ncdata(i,j).eq.1) then
        agb_after%ncdata(i,j) = (avgAGB(lu_after%ncdata(i,j)) - agb_before%ncdata(i,j)) + 0.485*cGrow(lu_after%ncdata(i,j))
      !Grasslands -> forest, savanna 
      else if(lu_before%ncdata(i,j).eq.3.and.lu_after%ncdata(i,j).eq.1.or.lu_after%ncdata(i,j).eq.2) then
        agb_after%ncdata(i,j) = (avgAGB(lu_after%ncdata(i,j)) - agb_before%ncdata(i,j)) + 0.485*cGrow(lu_after%ncdata(i,j))
    !Change: natural -> crop
      else if(lu_before%ncdata(i,j).le.3.and.lu_after%ncdata(i,j).ge.4.and.lu_after%ncdata(i,j).lt.8) then
        !Não usa taxa de crescimento anual. Foi considerada nula porque o incremento é igual a perda anual
        agb_after%ncdata(i,j) = avgAGB(lu_after%ncdata(i,j)) - agb_before%ncdata(i,j)
      !Change: Crop -> crop
      else if(lu_before%ncdata(i,j).ge.4.and.lu_before%ncdata(i,j).lt.8.and. &
              lu_after%ncdata(i,j).ge.4.and.lu_after%ncdata(i,j).lt.8) then
        agb_after%ncdata(i,j) = avgAGB(lu_after%ncdata(i,j)) - agb_before%ncdata(i,j)
      !Change: Crop -> natural (vegetation regrowth)
      else if(lu_before%ncdata(i,j).ge.4.and.lu_before%ncdata(i,j).lt.8.and.lu_after%ncdata(i,j).le.3) then
        !Please check NPP units in initial parameters
        agb_after%ncdata(i,j) = avgAGB(lu_after%ncdata(i,j)) - agb_before%ncdata(i,j)
      end if
    end do
    !$omp end parallel do
  end do
  
  agb_before = agb_after
end subroutine genAGB
