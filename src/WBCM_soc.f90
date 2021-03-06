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

subroutine genInitialSOC(soc_before, pre_soc, lu_before)
  ! Calculate the below ground biomass for the first year of the landuse time serie

  !Variables
    ! pre_soc: Below ground biomass in the past
    ! lu_before: Landuse classification of the first year of time serie
    ! avgSOC: Average below ground biomass of the classification dataset
   
    !Output
      ! soc_before: Below ground biomass of the first year of the landuse time serie

  type(nc2d_float_lld) :: pre_soc, soc_before
  type(nc2d_byte_lld) :: lu_before

  integer(kind=4) :: i, j

  soc_before = pre_soc

  do i = 1, lu_before%nlons
    !$omp parallel do private(j)
    do j = 1, lu_before%nlats
      if(lu_before%ncdata(i,j).le.3.and.lu_before%ncdata(i,j).ne.lu_before%FillValue) then
        soc_before%ncdata(i,j) = pre_soc%ncdata(i,j)
      else if(lu_before%ncdata(i,j).eq.4) then
        soc_before%ncdata(i,j) = avgSOC(4)
      else if(lu_before%ncdata(i,j).eq.5) then
        soc_before%ncdata(i,j) = avgSOC(5)
      else if(lu_before%ncdata(i,j).eq.6) then
        soc_before%ncdata(i,j) = avgSOC(6)
      else if(lu_before%ncdata(i,j).eq.7) then
        soc_before%ncdata(i,j) = avgSOC(7)
      else if(lu_before%ncdata(i,j).eq.8.or.lu_before%ncdata(i,j).eq.9) then
        soc_before%ncdata(i,j) = 0.0
      else
        soc_before%ncdata(i,j) = -9999.0
      end if
    end do
    !$omp end parallel do
  end do

  soc_before%varunits = 't C ha-1'
end subroutine genInitialSOC

subroutine genInitialEmissionSOC(emission, pre_soc, soc_before, bgb_before)
  ! Add comentários
  integer(kind=4) :: i, j
  type(nc2d_float_lld) :: emission, pre_soc, soc_before, bgb_before
  
  do i = 1, emission%nlons
    !$omp parallel do private(j)
    do j = 1, emission%nlats
     if(pre_soc%ncdata(i,j).ne.pre_soc%FillValue) then 
       emission%ncdata(i,j) = (pre_soc%ncdata(i,j) - soc_before%ncdata(i,j)) + (1-PRE)*0.485*bgb_before%ncdata(i,j)
     end if
    end do
    !$omp end parallel do
  end do
end subroutine genInitialEmissionSOC

!Calcula emissão atual
subroutine genEmissionSOC(emission, soc_before, lu_after, lu_before)
  !Add commentários
  integer(kind=4) :: i, j
  type(nc2d_float_lld) :: emission, soc_before
  type(nc2d_byte_lld) :: lu_after, lu_before
  
  do i = 1, emission%nlons
    !$omp parallel do private(j)
    do j = 1, emission%nlats
      !No change
      if(lu_before%ncdata(i,j).eq.lu_after%ncdata(i,j)) then
        emission%ncdata(i,j) = 0.0
      else
        emission%ncdata(i,j) = soc_before%ncdata(i,j) - avgSOC(lu_after%ncdata(i,j)) - (decayREC + decayRBGB + decayRS)
      end if
    end do
    !$omp end parallel do
  end do

  where(soc_before%ncdata.eq.soc_before%FillValue) emission%ncdata = -9999.0
end subroutine genEmissionSOC


!Calcula SOC atual
subroutine genSOC(emission, soc_after, soc_before, agb_before, bgb_before, lu_after, lu_before, cropcount)
  ! calculates the below-ground biomass of the first year of 
  ! the land use series considering the biomass calculated before.

  !variables
    ! soc_before: soil organic carbon stock of a year before
    ! lu_after: landuse classification of the next year
    ! lu_before: landuse classification of the year before
    ! rtime: redidence time of the landuse classes
    ! avgsoc: average of soil organic carbon stock of the classification dataset
    
    !output
     ! soc_after: soil organic carbon stock of next year

  type(nc2d_float_lld) :: soc_after, soc_before, agb_before, bgb_before, emission
  type(nc2d_byte_lld) :: lu_after, lu_before

  integer(kind=4), dimension(:,:) :: cropCount 

  integer(kind=4) :: i, j
  real(kind=float) :: d

  soc_after = soc_before

  do i = 1, emission%nlons
    !$omp parallel do private(j)
    do j = 1, emission%nlats

      if(lu_after%ncdata(i,j).eq.5) then
        cropCount(i,j) = cropCount(i,j) + 1
      else
        cropCount(i,j) = 0
      end if

      d = 0.0

      if(cropCount(i,j).eq.4)then
        if(trim(adjustl(withDisturb)).eq."yes")then
          d = disturb
        end if
        cropCount(i,j) = 0
      end if

      !Change: natural -> natural
      if(lu_before%ncdata(i,j).le.3.and.lu_after%ncdata(i,j).le.3) then
        soc_after%ncdata(i,j) = soc_before%ncdata(i,j) - emission%ncdata(i,j) & 
                                + 0.485*(1-PRE)*bgb_before%ncdata(i,j)
      !Change: natural -> agriculture
      else if(lu_before%ncdata(i,j).le.3.and.lu_after%ncdata(i,j).ge.4.and.lu_after%ncdata(i,j).lt.8) then
        soc_after%ncdata(i,j) = soc_before%ncdata(i,j) - emission%ncdata(i,j) & 
                                + 0.485*(percSOILloss*agb_before%ncdata(i,j) + (1-PRE)*bgb_before%ncdata(i,j))
      !Change: Agriculture -> agriculture
      else if(lu_before%ncdata(i,j).ge.4.and.lu_before%ncdata(i,j).lt.8.and. &
              lu_after%ncdata(i,j).ge.4.and.lu_after%ncdata(i,j).lt.8) then
        soc_after%ncdata(i,j) = soc_before%ncdata(i,j) - emission%ncdata(i,j) - d*soc_before%ncdata(i,j) &
                                + 0.485*(percSOILloss*agb_before%ncdata(i,j) + (1-PRE)*bgb_before%ncdata(i,j))
      !Change: Agriculture -> natural (vegetation regrowth)
      else if(lu_before%ncdata(i,j).ge.4.and.lu_before%ncdata(i,j).lt.8.and.lu_after%ncdata(i,j).le.3) then
        !Please check NPP units in initial parameters
        soc_after%ncdata(i,j) = soc_before%ncdata(i,j) - emission%ncdata(i,j) & 
                                + 0.485*(percSOILloss*agb_before%ncdata(i,j) + (1-PRE)*bgb_before%ncdata(i,j))
      end if
    end do
    !$omp end parallel do
  end do
  
  where(emission%ncdata.eq.emission%FillValue) soc_after%ncdata = emission%FillValue
  soc_before = soc_after

end subroutine genSOC
