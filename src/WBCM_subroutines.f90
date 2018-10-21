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

subroutine test(num, radius)
  integer, optional, intent(in):: num
  real(kind=8), optional, intent(in):: radius

  write(*,*) fpl_libversion()
  write(*,*) "Earth Radius: ", radius, "A number: ", num

end subroutine test


elemental subroutine str2int(str,int,stat)
  ! Arguments
  character(len=*),intent(in) :: str
  integer,intent(out)         :: int
  integer,intent(out)         :: stat

  read(str,*,iostat=stat)  int
end subroutine str2int

subroutine genInitialCarbon(agb_before, pre_agb, lu_before, avgAGB)
  type(nc2d_float_lld) :: pre_agb, agb_before
  type(nc2d_byte_lld) :: lu_before

  real(kind=4), dimension(9) :: avgAGB
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
      else
        agb_before%ncdata(i,j) = 0.00
      end if
    end do
    !$omp end parallel do
  end do

  agb_before%varunits = 't C ha-1'

end subroutine genInitialCarbon

subroutine genAGB(agb_after, agb_before, lu_after, lu_before, rtime, avgAGB)
  type(nc2d_float_lld) :: agb_after, agb_before, rtime
  type(nc2d_byte_lld) :: lu_after, lu_before


  real(kind=4), dimension(9) :: avgAGB
  integer(kind=4) :: i, j
  
  agb_after = agb_before

  write(*,*) NPP
  do i = 1, lu_after%nlons
    !$omp parallel do private(j)
    do j = 1, lu_after%nlats
      ! conversão de área natural para área natural
      if(lu_before%ncdata(i,j).eq.1.and.lu_after%ncdata(i,j).eq.2) then
        agb_after%ncdata(i,j) = avgAGB(2)
      else if(lu_before%ncdata(i,j).eq.1.and.lu_after%ncdata(i,j).eq.3) then
        agb_after%ncdata(i,j) = avgAGB(3)
      else if(lu_before%ncdata(i,j).eq.2.and.lu_after%ncdata(i,j).eq.1) then
        agb_after%ncdata(i,j) = avgAGB(1)
      else if(lu_before%ncdata(i,j).eq.2.and.lu_after%ncdata(i,j).eq.3) then
        agb_after%ncdata(i,j) = avgAGB(3)
      else if(lu_before%ncdata(i,j).eq.3.and.lu_after%ncdata(i,j).eq.1) then
        agb_after%ncdata(i,j) = avgAGB(1)
      else if(lu_before%ncdata(i,j).eq.3.and.lu_after%ncdata(i,j).eq.2) then
        agb_after%ncdata(i,j) = avgAGB(2)
      ! conversão de área natural para área não-natural
      else if(lu_before%ncdata(i,j).le.3.and.lu_after%ncdata(i,j).eq.4) then
        agb_after%ncdata(i,j) = avgAGB(4)
      else if(lu_before%ncdata(i,j).le.3.and.lu_after%ncdata(i,j).eq.5) then
        agb_after%ncdata(i,j) = avgAGB(5)
      else if(lu_before%ncdata(i,j).le.3.and.lu_after%ncdata(i,j).eq.6) then
        agb_after%ncdata(i,j) = avgAGB(6)
      else if(lu_before%ncdata(i,j).le.3.and.lu_after%ncdata(i,j).eq.7) then
        agb_after%ncdata(i,j) = avgAGB(7)
      ! conversão de área agrícola para área natural (recrescimento da vegetação)
      else if(lu_before%ncdata(i,j).ge.4.and.lu_before%ncdata(i,j).lt.8) then
        ! Please check NPP units in initial parameters
        agb_after%ncdata(i,j) = 10*NPP - (agb_before%ncdata(i,j)/rtime%ncdata(i,j))
      end if
    end do
    !$omp end parallel do
  end do
 
  agb_after%varunits = 't C ha-1'
end subroutine genAGB 







































