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


function random_uniform(mi, ma) result(val)
  real(kind=4) :: r
  integer(kind=4) :: mi, ma, val

  call random_number(r)
  val = int((ma-mi)*r + mi)

end function random_uniform

subroutine random_normal(mu, sigma, array)
 
  INTEGER :: i
  REAL, dimension(:) :: array
  REAL :: pi, temp, mu, sigma

  pi = 4.0*ATAN(1.0)
  CALL RANDOM_NUMBER(array) ! Uniform distribution
 
! Now convert to normal distribution
  do i = 1, size(array)-1, 2
    temp = sigma * SQRT(-2.0*LOG(array(i))) * COS(2*pi*array(i+1)) + mu
    array(i+1) = sigma * SQRT(-2.0*LOG(array(i))) * SIN(2*pi*array(i+1)) + mu
    array(i) = temp
  end do
 
end subroutine random_normal
