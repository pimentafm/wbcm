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

  agb_before%varunits = 't C ha-1'

end subroutine genInitialAGB

subroutine genInitialEmissionAGB(emission, pre_agb, agb_before)
  integer(kind=4) :: i, j
  type(nc2d_float_lld) :: emission, pre_agb, agb_before
  
  do i = 1, emission%nlons
    !$omp parallel do private(j)
    do j = 1, emission%nlats
     if(pre_agb%ncdata(i,j).ne.pre_agb%FillValue) then 
       emission%ncdata(i,j) = pre_agb%ncdata(i,j) - agb_before%ncdata(i,j)
     end if
    end do
    !$omp end parallel do
  end do
end subroutine genInitialEmissionAGB


!Calcula emissão atual
subroutine genEmissionAGB(emission, agb_before, lu_after, lu_before)
  integer(kind=4) :: i, j
  type(nc2d_float_lld) :: emission, agb_before
  type(nc2d_byte_lld) :: lu_after, lu_before
  
  do i = 1, emission%nlons
    !$omp parallel do private(j)
    do j = 1, emission%nlats
      if(lu_before%ncdata(i,j).eq.lu_after%ncdata(i,j)) then
        emission%ncdata(i,j) = 0.0
      else
        emission%ncdata(i,j) = percATMloss*agb_before%ncdata(i,j) - avgAGB(lu_after%ncdata(i,j))
      end if
    end do
    !$omp end parallel do
  end do

  where(agb_before%ncdata.eq.agb_before%FillValue) emission%ncdata = emission%FillValue
end subroutine genEmissionAGB


!Calcula AGB atual
subroutine genAGB(emission, agb_after, agb_before, lu_after, lu_before, rtime)
  ! calculates the above-ground biomass of the first year of 
  ! the land use series considering the biomass calculated before.

  !variables
    ! pre_before: above ground biomass of a year before
    ! lu_after: landuse classification of the next year
    ! lu_before: landuse classification of the year before
    ! rtime: redidence time of the landuse classes
    ! avgagb: average above ground biomass of the classification dataset
    
    !output
     ! agb_after: above ground biomass of next year

  type(nc2d_float_lld) :: agb_after, agb_before, rtime, emission
  type(nc2d_byte_lld) :: lu_after, lu_before

  integer(kind=4) :: i, j

  agb_after = agb_before

  do i = 1, emission%nlons
    !$omp parallel do private(j)
    do j = 1, emission%nlats
      !Change: natural -> natural
      if(lu_before%ncdata(i,j).le.3.and.lu_after%ncdata(i,j).le.3) then
        agb_after%ncdata(i,j) = agb_before%ncdata(i,j) - emission%ncdata(i,j)
      !Change: natural -> agriculture
      else if(lu_before%ncdata(i,j).le.3.and.lu_after%ncdata(i,j).ge.4.and.lu_after%ncdata(i,j).lt.8) then
        agb_after%ncdata(i,j) = agb_before%ncdata(i,j) - emission%ncdata(i,j) - percSOILloss*agb_before%ncdata(i,j)
      !Change: Agriculture -> agriculture
      else if(lu_before%ncdata(i,j).ge.4.and.lu_before%ncdata(i,j).lt.8.and. &
              lu_after%ncdata(i,j).ge.4.and.lu_after%ncdata(i,j).lt.8) then
        agb_after%ncdata(i,j) = agb_before%ncdata(i,j) - emission%ncdata(i,j) - percSOILloss*agb_before%ncdata(i,j)
      !Change: Agriculture -> natural (vegetation regrowth)
      else if(lu_before%ncdata(i,j).ge.4.and.lu_before%ncdata(i,j).lt.8.and.lu_after%ncdata(i,j).le.3) then
        !Please check NPP units in initial parameters
        agb_after%ncdata(i,j) = (10*NPP - (agb_before%ncdata(i,j)/rtime%ncdata(i,j))) &
                                + agb_before%ncdata(i,j) - emission%ncdata(i,j) - percSOILloss*agb_before%ncdata(i,j)
      end if
    end do
    !$omp end parallel do
  end do
  
  where(emission%ncdata.eq.emission%FillValue) agb_after%ncdata = emission%FillValue
  agb_before = agb_after
end subroutine genAGB
