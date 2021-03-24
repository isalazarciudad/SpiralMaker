!    SpiralMaker software (General Node Model)
!    Computational model to simulate embryonic cleavage.
!    Copyright (C) 2014 Miquel Marin-Riera, Miguel Brun-Usan, Roland Zimm, Tommi Välikangas & Isaac Salazar-Ciudad

!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    any later version.

!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.


!***************************************************************************
!***************  MODUL INICIAL ********************************************
!***************************************************************************
module inicial
use general
use genetic    !>>>>>>>>>>>> Is 29-4-13
use aleas
use neighboring
use ic
use io
use nexus !>>Miquel24-9-14

character*8, public :: cdate

contains

!**************************************************************************
subroutine initials
character*200 kk
  itvi=1
  itviactual=1
  call getarg(1,carg)
  if (len_trim(carg)==0.or.carg=="0") then
    !call default_param_values
    call default_ic
    erep=0.0d0
    erepcel=0.0d0
    eyou=0.0d0
    eadh=0.0d0
    etor=0.0d0
    espring=0.0d0
  else
    call iniread
    call readsnap(carg)
    if(nd>1) call neighbor_build  !>>Miquel24-9-14
    if (errorlec==1.and.eva==0) stop  !>>> Is 22-1-14
  end if
  call iniio              ! this is just to allocate and inicializate the matrices for the variable names and stuff
  call inialea3d(nparti)  ! this is to inicialize the partition of random numbers in a sphere
  if(ffu(13)==0)then
    call iniboxes           ! this to inicialize the boxes
  end if
  if(nd>1) call neighbor_build
  call put_param_to_matrix(param)
  paramo=param

  if(aut>0)  call writesnapini    !here we write the output file with the initial conditions              !>>>>Miquel18-11-13

end subroutine

!**************************************************************************
subroutine default_ic

   call default_values          !IS 2-1-14 this is just some default values that get overrided by the ic

!IC for the mechanisms
 ic_load=1
  select case(ic_load)
    case(1);  call mesenchyme!epi_apoptosis            !OK ; CHECKED 25-8-14    
  end select

end subroutine default_ic

end module inicial

