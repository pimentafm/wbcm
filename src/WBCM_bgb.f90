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

!!!!!!!!!!!! Testing -------------------------
!!!!!!!!!!!! ---------------------------------

subroutine genInitialBGB(bgb_before, pre_bgb, lu_before, avgBGB)
  ! Calculate the below ground biomass for the first year of the landuse time serie

  !Variables
    ! pre_bgb: Below ground biomass in the past
    ! lu_before: Landuse classification of the first year of time serie
    ! avgBGB: Average below ground biomass of the classification dataset
   
    !Output
      ! bgb_before: Below ground biomass of the first year of the landuse time serie

  type(nc2d_float_lld) :: pre_bgb, bgb_before
  type(nc2d_byte_lld) :: lu_before

  real(kind=4), dimension(9) :: avgBGB
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
      else
        bgb_before%ncdata(i,j) = 0.00
      end if
    end do
    !$omp end parallel do
  end do

  bgb_before%varunits = 't C ha-1'
end subroutine genInitialBGB

subroutine genInitialEmissionBGB(emission, pre_bgb, bgb_before)
  ! Add comentários
  integer(kind=4) :: i, j
  type(nc2d_float_lld) :: emission, pre_bgb, bgb_before
  
  do i = 1, emission%nlons
    !$omp parallel do private(j)
    do j = 1, emission%nlats
     if(pre_bgb%ncdata(i,j).ne.pre_bgb%FillValue) then 
       emission%ncdata(i,j) = PRE*pre_bgb%ncdata(i,j) - bgb_before%ncdata(i,j)
     end if
    end do
    !$omp end parallel do
  end do
end subroutine genInitialEmissionBGB

!Calcula emissão atual
subroutine genEmissionBGB(emission, bgb_before, lu_after, lu_before, avgBGB)
  !Add commentários
  integer(kind=4) :: i, j
  type(nc2d_float_lld) :: emission, bgb_before
  type(nc2d_byte_lld) :: lu_after, lu_before
  real(kind=4), dimension(9) :: avgBGB
  
  do i = 1, emission%nlons
    !$omp parallel do private(j)
    do j = 1, emission%nlats
      !No change
      if(lu_before%ncdata(i,j).eq.lu_after%ncdata(i,j).and.emission%ncdata(i,j).ne.emission%FillValue) then
        emission%ncdata(i,j) = 0.0
      else
        emission%ncdata(i,j) = PRE*bgb_before%ncdata(i,j) - avgBGB(lu_after%ncdata(i,j))
      end if
    end do
    !$omp end parallel do
  end do
end subroutine genEmissionBGB


!Calcula BGB atual
subroutine genBGB(emission, bgb_after, bgb_before, lu_after, lu_before, rtime)
  ! calculates the below-ground biomass of the first year of 
  ! the land use series considering the biomass calculated before.

  !variables
    ! pre_before: below ground biomass of a year before
    ! lu_after: landuse classification of the next year
    ! lu_before: landuse classification of the year before
    ! rtime: redidence time of the landuse classes
    ! avgbgb: average below ground biomass of the classification dataset
    
    !output
     ! bgb_after: below ground biomass of next year

  type(nc2d_float_lld) :: bgb_after, bgb_before, rtime, emission
  type(nc2d_byte_lld) :: lu_after, lu_before

  integer(kind=4) :: i, j

  bgb_after = bgb_before

  do i = 1, emission%nlons
    !$omp parallel do private(j)
    do j = 1, emission%nlats
      !Change: natural -> natural or  natural -> agriculture
      if(lu_before%ncdata(i,j).le.3.and.lu_after%ncdata(i,j).le.3.or.lu_before%ncdata(i,j).le.3.and.lu_after%ncdata(i,j).gt.3) then
        bgb_after%ncdata(i,j) = bgb_before%ncdata(i,j) - emission%ncdata(i,j)
      !Change: Agriculture -> agriculture
      else if(lu_before%ncdata(i,j).ge.4.and.lu_before%ncdata(i,j).lt.8.and. &
              lu_after%ncdata(i,j).ge.4.and.lu_after%ncdata(i,j).lt.8) then
        bgb_after%ncdata(i,j) = bgb_before%ncdata(i,j) - emission%ncdata(i,j)
      !Change: Agriculture -> natural (vegetation regrowth)
      else if(lu_before%ncdata(i,j).ge.4.and.lu_before%ncdata(i,j).lt.8.and.lu_after%ncdata(i,j).le.3) then
        !Please check NPP units in initial parameters
        bgb_after%ncdata(i,j) = bgb_before%ncdata(i,j) + ((PRA*10*NPP) - (bgb_before%ncdata(i,j)/rtime%ncdata(i,j)))
      end if
    end do
    !$omp end parallel do
  end do
  
  bgb_before = bgb_after

end subroutine genBGB
