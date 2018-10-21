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

!:=========================== Calc Carbon test ================================
program calcCarbon
  use omp_lib
  use WBCM
  implicit none

  ! ------- Dataset definitions
  !agb_before: Above ground biomass in the last year (t ha-1)
  !agb_after: Above ground biomass in the present year (t ha-1)
  !rtime: Residence time (year)
  !lu_before: Landuse classes in the last year (class number)
  !lu_after: Landuse classes in the present year (class number)
  !pre_biomass: Above ground biomass in the past (anisomorphy) (t ha-1)
  type(nc2d_float_lld) :: pre_agb, agb_before, agb_after, rtime
  type(nc2d_byte_lld) :: lu_before, lu_after

  ! ------- Parameters
  !input_<other name>: File directory path of the <other name>
  character(len=200) :: input_pre_agb, input_lu_before, input_luafter, input_rtime

  ! ------- Auxiliary variables
  !Initialize the input parameters
  include "parameters.inc"

  input_pre_agb = "database/input/AGB.nc"
  input_lu_before = "database/input/classification1990.nc"
  input_luafter = "database/input/classification1991.nc"
  input_rtime = "database/input/rtime.nc"
 
  lu_before%varname = "class"
  lu_before%lonname = "lon"
  lu_before%latname = "lat"
 
  lu_after%varname = "class"
  lu_after%lonname = "lon"
  lu_after%latname = "lat"
  
  pre_agb%varname = "biomass"
  pre_agb%lonname = "lon"
  pre_agb%latname = "lat"
  
  rtime%varname = "rtime"
  rtime%lonname = "lon"
  rtime%latname = "lat"
  
  call readgrid(input_pre_agb, pre_agb)
  call readgrid(input_lu_before, lu_before)
  call readgrid(input_luafter, lu_after)
  call readgrid(input_rtime, rtime)

  !Gera biomassa inicial 1990 

  write(*,*) "Aboveground biomass  - 1990"
  call genInitialCarbon(agb_before, pre_agb, lu_before, avgAGB)

  call writegrid("database/output/AGB1990.nc", agb_before)

  !do k = 1991, 2018
  write(*,*) "Aboveground biomass  - 1991"
  call genAGB(agb_after, agb_before, lu_after, lu_before, rtime, avgAGB)
  
  call writegrid("database/output/AGB1991.nc", agb_after)
  !end do

end program calcCarbon

