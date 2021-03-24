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




module automaticon
use general
use model
use io

contains

!**************************************************

subroutine auto
integer irf
integer iit_step,inu_it
character*10 cf
character*300 cx
integer*4 pid

iit_step=10
inu_it=100

open(20,file=trim(carg)//"p",iostat=i)
print *,i," in opening file"
pid=getpid()
write (20,*) pid
print *,pid,trim(carg)," pid"
flush(20)
close(20)
10 open(20,file=trim(carg)//"p",iostat=i)
print *,"reading..."
read(20,*,ERR=10) i
print *,i,"pid"
if (i/=pid) goto 10
cx="head "//trim(carg)//"p"
call system(cx)

call get_its(iit_step,inu_it)

do irf=1,inu_it
  call iteracio(iit_step)
  call writesnap
end do

print *,carg,"done"

if (len_trim(carg)/=0) then
  !print *,trim(carg)//trim(noff),"ki"
  !print *,trim(carg)//trim(nofi),"kii"
  open(23,file=trim(carg)//"t",iostat=i)
  print *,"making...",trim(carg)//"t"
  write(23,*,ERR=46) trim(carg)//trim(nofi)
  open(24,file=trim(carg)//"x",iostat=i)
  write(24,*,ERR=46) 0

  flush(24)
  flush(23)
  ret = fsync(fnum(23))

  ! Handle possible error
  if (ret /= 0) then ; print *,"AQUIS" ; call exit(231) ; stop "Error calling FSYNC" ;end if

  open(23,file=trim(carg)//"t",iostat=ii)

  read(23,*,END=45,ERR=48) cx
  !close(23)
  call flush(23)
  print *,"done with iostat",ii
  cx="ls -alrt "//trim(carg)//"t" 
  call system(cx) 
  call exit(231)
45 print *,"end of file error 111"
  call exit(231)
46 print *,"error in writing",trim(carg)//"t"
  call exit(231) 
48 print *,"other end 222"
  call exit(231)
end if

print *,"final 333"
call exit(231)

end subroutine

end module
