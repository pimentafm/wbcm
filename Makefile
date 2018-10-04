#:=============================================================================
# Western Bahia Carbon Model
# Copyright (C) 2018 Fernando Martins Pimenta
#                    Emily Ane Dionizio
# 
#  WBCM is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
# 
#  WBCM is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
# 
#  You should have received a copy of the GNU General Public License
#  along with WBCM.  If not, see <http://www.gnu.org/licenses/>.
# 
#:=============================================================================
#  About Authors:
#  Fernando Martins Pimenta
#   Student of Surveying and Cartographic Engineering
#   Federal University of Viçosa - Brazil
# 
#   Bachelor of Biosystems Engineer
#   Federal University of São João del-Rei - Brazil
# 
#  Emily Ane Dionizio
#   Biologist
#   FATEA - SP - Brazil
#
#   MS in Agricultural Meteorology
#   Federal University of Viçosa - Brazil
#   
#   Doctoral student in Meteorology - UFV
#   Federal University of Viçosa - Brazil
#
#  Research Group on Atmosphere-Biosphere Interaction
#  Federal University of Viçosa
#
#  Contacts: fernando.m.pimenta@gmail.com, fernando.m.pimenta@ufv.br
#            emilyy.ane@gmail.com
#:=============================================================================

#Dependencies:
#             gcc-gfortran
#             netcdf-fortran
#             netcdf-fortran-devel
#             openmpi
#             WBCM - Fortran Processing Library
#             lbs_release

#Check system name version and arch
OS=$(shell lsb_release -si)
VERSION=$(shell lsb_release -sr)
ARCH=$(shell uname -m | sed 's/x86_//;s/i[3-6]86/32/')

#WBCM library and module names
WBCM_lib=libWBCM.so
WBCM_mod=wbcm.mod

#Compilation parameters
COMPILER=gfortran
FLAGS=-Wall -O3 -shared -fPIC -cpp
OPENMP=-fopenmp
FPL_library=-lFPL

#Check distro
#dnf install redhat-lsb

ifeq ($(OS), $(filter $(OS), Fedora Korora))
  $(info "$(OS) $(VERSION) $(ARCH) bits")

  #WBCM source files and directories
  WBCM_srcdir=$(shell pwd)/src/
  WBCM_libdir=/usr/lib64/
  WBCM_moddir=/usr/lib64/gfortran/modules/
  #FPL
  FPL_module=-I/usr/lib64/gfortran/modules/
endif
ifeq ($(OS), $(filter $(OS), Debian Ubuntu))
  $(info "$(OS) $(VERSION) $(ARCH) bits")
  #WBCM source files and directories
  WBCM_srcdir=$(shell pwd)/src/
  WBCM_libdir=/usr/lib/
  WBCM_moddir=/usr/include/
  
  #FPL
  FPL_module=-I/usr/include/
endif

compile:
	$(COMPILER) $(OPENMP) $(FLAGS) -o $(WBCM_lib) $(WBCM_srcdir)WBCM.f90 $(FPL_module) $(FPL_library)
	mv $(WBCM_lib) $(WBCM_libdir)
	mv $(WBCM_mod) $(WBCM_moddir)
