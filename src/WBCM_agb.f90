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

!Calcula AGB atual
subroutine genAGB(agb_after, agb_before, lu_after, lu_before,floresta, &
                  savana, campo, seqpast, sequeiro, irrigado, pastagem)
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
  real(kind=4), dimension(:), allocatable :: floresta, savana, campo, seqpast, sequeiro, irrigado, pastagem

  agb_after = agb_before

  do i = 1, agb_after%nlons
    !$omp parallel do
    do j = 1, agb_after%nlats
      if(lu_after%ncdata(i,j).ne.lu_before%ncdata(i,j)) then
        if(lu_after%ncdata(i,j).eq.1) agb_after%ncdata(i,j) = floresta(random_uniform(0, 100000))
        if(lu_after%ncdata(i,j).eq.2) agb_after%ncdata(i,j) = savana(random_uniform(0, 100000))
        if(lu_after%ncdata(i,j).eq.3) agb_after%ncdata(i,j) = campo(random_uniform(0, 100000))
        if(lu_after%ncdata(i,j).eq.4) agb_after%ncdata(i,j) = seqpast(random_uniform(0, 100000))
        if(lu_after%ncdata(i,j).eq.5) agb_after%ncdata(i,j) = sequeiro(random_uniform(0, 100000))
        if(lu_after%ncdata(i,j).eq.6) agb_after%ncdata(i,j) = irrigado(random_uniform(0, 100000))
        if(lu_after%ncdata(i,j).eq.7) agb_after%ncdata(i,j) = pastagem(random_uniform(0, 100000))
     end if
   end do
   !$omp end parallel do
  end do
end subroutine genAGB
