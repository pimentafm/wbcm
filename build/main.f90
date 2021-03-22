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
  !lu_before: Landuse classes in the last year (class number)
  !lu_after: Landuse classes in the present year (class number)
  !pre_biomass: Above ground biomass in the past (anisomorphy) (t ha-1)
  type(nc2d_float_lld) :: agb_before, agb_after, &
                          bgb_before, bgb_after, soc_before, soc_after

  type(nc2d_byte_lld) :: lu_before, lu_after

  ! ------- Auxiliary variables
  !Initialize the input parameters
  integer(kind=4) :: i, k, seed
  integer(kind=4), dimension(7) :: nrows
  character(len=4) :: year, last_year

  real(kind=4), dimension(:), allocatable :: afloresta, asavana, acampo, aseqpast, asequeiro, airrigado, apastagem
  real(kind=4), dimension(:), allocatable :: bfloresta, bsavana, bcampo, bseqpast, bsequeiro, birrigado, bpastagem
  real(kind=4), dimension(:), allocatable :: cfloresta, csavana, ccampo, cseqpast, csequeiro, cirrigado, cpastagem

  include "parameters.inc"

  do i = 100, 700, 100
    open(i, file=trim(adjustl(r_dir))//trim(adjustl(agbFiles(i/100))), status="old")
    nrows(i/100) = numRows(i)
  end do

  allocate(afloresta(nrows(1)))
  allocate(asavana(nrows(2)))
  allocate(acampo(nrows(3)))
  allocate(asequeiro(nrows(4)))
  allocate(airrigado(nrows(5)))
  allocate(apastagem(nrows(6)))
  allocate(aseqpast(nrows(7)))


  write(*,*) "Reading randomized AGB data"
  do i = 1, nrows(1)
    read(100, *) afloresta(i)
    read(200, *) asavana(i)
    read(300, *) acampo(i)
    read(400, *) aseqpast(i)
    read(500, *) asequeiro(i)
    read(600, *) airrigado(i)
    read(700, *) apastagem(i)
  end do

  do i = 100, 700, 100
    close(i)
  end do

  do i = 100, 700, 100
    open(i, file=trim(adjustl(r_dir))//trim(adjustl(bgbFiles(i/100))), status="old")
    nrows(i/100) = numRows(i)
  end do

  allocate(bfloresta(nrows(1)))
  allocate(bsavana(nrows(2)))
  allocate(bcampo(nrows(3)))
  allocate(bsequeiro(nrows(4)))
  allocate(birrigado(nrows(5)))
  allocate(bpastagem(nrows(6)))
  allocate(bseqpast(nrows(7)))


  write(*,*) "Reading randomized BGB data"
  do i = 1, nrows(1)
    read(100, *) bfloresta(i)
    read(200, *) bsavana(i)
    read(300, *) bcampo(i)
    read(400, *) bseqpast(i)
    read(500, *) bsequeiro(i)
    read(600, *) birrigado(i)
    read(700, *) bpastagem(i)
  end do

  do i = 100, 700, 100
    close(i)
  end do
  
  do i = 100, 700, 100
    open(i, file=trim(adjustl(r_dir))//trim(adjustl(socFiles(i/100))), status="old")
    nrows(i/100) = numRows(i)
  end do

  allocate(cfloresta(nrows(1)))
  allocate(csavana(nrows(2)))
  allocate(ccampo(nrows(3)))
  allocate(csequeiro(nrows(4)))
  allocate(cirrigado(nrows(5)))
  allocate(cpastagem(nrows(6)))
  allocate(cseqpast(nrows(7)))


  write(*,*) "Reading randomized SOC data"
  do i = 1, nrows(1)
    read(100, *) cfloresta(i)
    read(200, *) csavana(i)
    read(300, *) ccampo(i)
    read(400, *) cseqpast(i)
    read(500, *) csequeiro(i)
    read(600, *) cirrigado(i)
    read(700, *) cpastagem(i)
  end do

  do i = 100, 700, 100
    close(i)
  end do

  lu_before%varname = "class"
  lu_before%lonname = "lon"
  lu_before%latname = "lat"
 
  lu_after%varname = "class"
  lu_after%lonname = "lon"
  lu_after%latname = "lat"
  

  agb_before%varname = "AGB"
  agb_before%lonname = "lon"
  agb_before%latname = "lat"

  agb_after%varname = "AGB"
  agb_after%lonname = "lon"
  agb_after%latname = "lat"


  bgb_before%varname = "BGB"
  bgb_before%lonname = "lon"
  bgb_before%latname = "lat"

  bgb_after%varname = "BGB"
  bgb_after%lonname = "lon"
  bgb_after%latname = "lat"

  soc_before%varname = "SOC"
  soc_before%lonname = "lon"
  soc_before%latname = "lat"

  soc_after%varname = "SOC"
  soc_after%lonname = "lon"
  soc_after%latname = "lat"
  
  do k = 1991, 2018

    call system_clock(seed)
    call random_seed(seed)

    write(year, '(i4)') k
    
    write(*,*) "Aboveground - Belowground biomass - Soil Organic Carbon Stock ", year

    !Pools of the next year
    write(last_year, '(i4)') k - 1
    
    call readgrid(trim(adjustl(input_dir))//"aaf_classification"//last_year//"v30.nc", lu_before)
    call readgrid(trim(adjustl(input_dir))//"aaf_classification"//year//"v30.nc", lu_after)
   
    call readgrid(trim(adjustl(output_dir))//"AGB"//last_year//".nc", agb_before)
    call readgrid(trim(adjustl(output_dir))//"BGB"//last_year//".nc", bgb_before)
    call readgrid(trim(adjustl(output_dir))//"SOC"//last_year//".nc", soc_before)

    call genAGB(agb_after, agb_before, lu_after, lu_before, afloresta, &
                asavana, acampo, aseqpast, asequeiro, airrigado, apastagem)

    call genBGB(bgb_after, bgb_before, lu_after, lu_before, bfloresta, &
                bsavana, bcampo, bseqpast, bsequeiro, birrigado, bpastagem)

    call genSOC(soc_after, soc_before, lu_after, lu_before, cfloresta, &
                csavana, ccampo, cseqpast, csequeiro, cirrigado, cpastagem)

    
    call writegrid(trim(adjustl(output_dir))//"AGB"//year//".nc", agb_after)
    call writegrid(trim(adjustl(output_dir))//"BGB"//year//".nc", bgb_after)
    call writegrid(trim(adjustl(output_dir))//"SOC"//year//".nc", soc_after)

    call dealloc(lu_before)
    call dealloc(lu_after)
    call dealloc(agb_after)
    call dealloc(agb_before)
    call dealloc(bgb_before)
    call dealloc(soc_before)
  end do

end program calcCarbon
