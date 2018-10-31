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
  type(nc2d_float_lld) :: pre_agb, pre_bgb, agb_before, agb_after, &
                          bgb_before, bgb_after, rtime, emissionAGB, emissionBGB
  type(nc2d_byte_lld) :: lu_before, lu_after

  ! ------- Auxiliary variables
  !Initialize the input parameters
  integer(kind=4) :: k, xx, yy
  character(len=4) :: year, last_year

  
  include "parameters.inc"

  lu_before%varname = "class"
  lu_before%lonname = "lon"
  lu_before%latname = "lat"
 
  lu_after%varname = "class"
  lu_after%lonname = "lon"
  lu_after%latname = "lat"
  
  pre_agb%varname = "AGB"
  pre_agb%lonname = "lon"
  pre_agb%latname = "lat"
  
  pre_bgb%varname = "BGB"
  pre_bgb%lonname = "lon"
  pre_bgb%latname = "lat"
  
  rtime%varname = "rtime"
  rtime%lonname = "lon"
  rtime%latname = "lat"
  
  call readgrid(trim(adjustl(input_dir))//"AGB.nc", pre_agb)
  call readgrid(trim(adjustl(input_dir))//"BGB.nc", pre_bgb)

  call readgrid(trim(adjustl(input_dir))//"rtime.nc", rtime)
  
  emissionAGB = pre_agb
  emissionBGB = pre_bgb

  xx = 7587
  yy = 10272

  do k = 1990, 2018
    write(year, '(i4)') k
    
    write(*,*) "Aboveground - Belowground biomass - ", year

    if(k.eq.1990) then
      call readgrid(trim(adjustl(input_dir))//"classification"//year//".nc", lu_before)
      
      !AGB --------------      
      call genInitialCarbon(agb_before, pre_agb, lu_before, avgAGB)
      call genInitialEmission(emissionAGB, pre_agb, agb_before)

      write(*,*) lu_before%ncdata(xx,yy), & 
                 agb_before%ncdata(xx,yy), &
                 emissionAGB%ncdata(xx,yy),pre_agb%ncdata(xx,yy)
            
      call writegrid(trim(adjustl(output_dir))//"AGB"//year//".nc", agb_before)
      call writegrid(trim(adjustl(output_dir))//"emissionAGB"//year//".nc", emissionAGB)

      !BGB --------------      
      call genInitialBGB(bgb_before, pre_bgb, lu_before, avgBGB)
      call genInitialEmissionBGB(emissionBGB, pre_bgb, bgb_before)

      write(*,*) lu_before%ncdata(xx,yy), & 
                 bgb_before%ncdata(xx,yy), &
                 emissionBGB%ncdata(xx,yy),pre_bgb%ncdata(xx,yy)
            
      call writegrid(trim(adjustl(output_dir))//"BGB"//year//".nc", bgb_before)
      call writegrid(trim(adjustl(output_dir))//"emissionBGB"//year//".nc", emissionBGB)
      call dealloc(lu_before)
      call dealloc(agb_before)
      call dealloc(bgb_before)
    else
      !Pools of the next year
      write(last_year, '(i4)') k - 1
    
      !AGB --------------      
      call readgrid(trim(adjustl(input_dir))//"classification"//last_year//".nc", lu_before)
      call readgrid(trim(adjustl(input_dir))//"classification"//year//".nc", lu_after)
      
      call readgrid(trim(adjustl(output_dir))//"AGB"//last_year//".nc", agb_before)

      call genEmission(emissionAGB, agb_before, lu_after, lu_before, avgAGB)
      
      call writegrid(trim(adjustl(output_dir))//"emissionAGB"//year//".nc", emissionAGB)

      call genAGB(emissionAGB, agb_after, agb_before, lu_after, lu_before, rtime)
      

      write(*,*) lu_after%ncdata(xx,yy), lu_before%ncdata(xx,yy), & 
                 agb_after%ncdata(xx,yy), agb_before%ncdata(xx,yy), &
                 emissionAGB%ncdata(xx,yy),pre_agb%ncdata(xx,yy)

      call writegrid(trim(adjustl(output_dir))//"AGB"//year//".nc", agb_after)
      
      !-------------------------BGB --------------      
      !call readgrid(trim(adjustl(input_dir))//"classification"//last_year//".nc", lu_before)
      !call readgrid(trim(adjustl(input_dir))//"classification"//year//".nc", lu_after)
      
      call readgrid(trim(adjustl(output_dir))//"BGB"//last_year//".nc", bgb_before)

      call genEmission(emissionBGB, bgb_before, lu_after, lu_before, avgBGB)
      
      call writegrid(trim(adjustl(output_dir))//"emissionBGB"//year//".nc", emissionBGB)

      call genBGB(emissionBGB, bgb_after, bgb_before, lu_after, lu_before, rtime)
      

      write(*,*) lu_after%ncdata(xx,yy), lu_before%ncdata(xx,yy), & 
                 bgb_after%ncdata(xx,yy), bgb_before%ncdata(xx,yy), &
                 emissionBGB%ncdata(xx,yy),pre_bgb%ncdata(xx,yy)

      call writegrid(trim(adjustl(output_dir))//"BGB"//year//".nc", bgb_after)
!
!      
      call dealloc(lu_before)
      call dealloc(lu_after)
!      call dealloc(agb_after)
      call dealloc(agb_before)
      call dealloc(bgb_before)
    end if
  end do

  close(600)
end program calcCarbon

