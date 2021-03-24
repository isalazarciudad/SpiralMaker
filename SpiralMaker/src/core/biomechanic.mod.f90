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

module biomechanic	!>>>>>>>> by Miquel 17-5-13

use general
use genetic
use neighboring

contains

subroutine iterdiferencial
integer::nodmo,i,j,k,ii
real*8::a,b,c
real*8::ox,oy,oz !miguel4-1-13

  !CALCULATING FORCES AND MOVEMENT VECTORS

  call forces

  a=0.0d0
  do i=1,nd
    if (dex(i)>a) then ; a=dex(i) ; ii=i ; end if
  end do
  
  if (a<epsilod) then ; rtime=rtime+delta ; delta=deltamin; return ; end if  !>>> Is 29-8-13
  if(ffu(12)==0)then
    delta=resmax/a      !delta is adjusted so the node that has the greatest force
                      !will always move the same absolute distance, and the rest will
                      !move proportionally to that force
  else
    delta=deltamin
!    delta=deltamax 
  end if
                      
  if (delta>deltamax) delta=deltamax
  if (delta<deltamin) delta=deltamin

end subroutine iterdiferencial

!***************************************************************************************************

subroutine rungekutta4(d)  ! Runge-Kutta fourth order integration
real*8 d,halfd,sixthd
real*8 ox(nd),oy(nd),oz(nd)
real*8 kux(nd),kuy(nd),kuz(nd)
real*8 kdx(nd),kdy(nd),kdz(nd)
real*8 ktx(nd),kty(nd),ktz(nd)
real*8 kqx(nd),kqy(nd),kqz(nd)

halfd=d*0.5d0
sixthd=d/6.0d0

ox=node(:nd)%x ; oy=node(:nd)%y ; oz=node(:nd)%z 

!k1
kux=px(:nd) ; kuy=py(:nd) ; kuz=pz(:nd)

!k2
node(:nd)%x=node(:nd)%x+halfd*px(:nd)
node(:nd)%y=node(:nd)%y+halfd*py(:nd)
node(:nd)%z=node(:nd)%z+halfd*pz(:nd)

if(nd>1) call neighbor_build
call forces

kdx=px(:nd) ; kdy=py(:nd) ; kdz=pz(:nd)

!k3
node(:nd)%x=node(:nd)%x+halfd*px(:nd)
node(:nd)%y=node(:nd)%y+halfd*py(:nd)
node(:nd)%z=node(:nd)%z+halfd*pz(:nd)

if(nd>1) call neighbor_build
call forces

ktx=px(:nd) ; kty=py(:nd) ; ktz=pz(:nd)

!k4 
node(:nd)%x=node(:nd)%x+d*px(:nd)
node(:nd)%y=node(:nd)%y+d*py(:nd)
node(:nd)%z=node(:nd)%z+d*pz(:nd)

if(nd>1) call neighbor_build
call forces

kqx=px(:nd) ; kqy=py(:nd) ; kqz=pz(:nd)

!final
node(:nd)%x=ox+sixthd*(kux+2*kdx+2*ktx+kqx)
node(:nd)%y=oy+sixthd*(kuy+2*kdy+2*kty+kqy)
node(:nd)%z=oz+sixthd*(kuz+2*kdz+2*ktz+kqz)

end subroutine

!***************************************************************************************************

subroutine adaptive_rungekutta
real*8 ox(nd),oy(nd),oz(nd)
real*8 aux(nd),auy(nd),auz(nd)
real*8 adx(nd),ady(nd),adz(nd)
real*8 r,halfdelta,invdelta,mdx,mdy,mdz,suggesteddelta

37 continue

halfdelta=0.5d0*delta
invdelta=1d0/delta

ox=node(:nd)%x ; oy=node(:nd)%y ; oz=node(:nd)%z

call rungekutta4(delta)

aux=node(:nd)%x ; auy=node(:nd)%y ; auz=node(:nd)%z
node(:nd)%x=ox  ; node(:nd)%y=oy  ; node(:nd)%z=oz

call rungekutta4(halfdelta)
call rungekutta4(halfdelta)

adx=node(:nd)%x ; ady=node(:nd)%y ; adz=node(:nd)%z

mdx=maxval(abs(aux-adx)*invdelta)
mdy=maxval(abs(auy-ady)*invdelta)
mdz=maxval(abs(auz-adz)*invdelta)

if (mdx>=mdy.and.mdx>=mdz) r=mdx
if (mdy>=mdz.and.mdy>=mdz) r=mdy
if (mdz>=mdy.and.mdz>=mdx) r=mdz

suggesteddelta=0.9d0*prec*delta/r

if (r>prec) then !the step is too large
  delta=suggesteddelta
!print *,"NO",delta,r,prec
  goto 37
else
!print *,"SI",delta,r,prec
  ! in theory we should make delta=suggested delta but we prefer delta to be decided based on resmax in each step
  node(:nd)%x=adx
  node(:nd)%y=ady
  node(:nd)%z=adz
end if

end subroutine

!***************************************************************************************************

subroutine forces
real*8   ::ix,iy,iz,dd
real*8   ::a,b,c,d,e,f,g
integer  ::celi,celj,nod
real*8   ::youe,repe,adhe,adho,repcele,deqe,ideqe
real*8   ::younod,repnod,adhnod,repcelnod,reqnod,tornod,stornod    !>>>> Miquel 16-8-13
real*8   ::ax,ay,az,bx,by,bz
real*8   ::ud,udd,uddd
real*8   ::cx,cy,cz,ccx,ccy,ccz,dotp,pesco
real*8   ::icx,icy,icz,idd,iudd,id !>>>>>>>> MIQUEL 4-3-13
real*8   ::mcx,mcy,mcz,md,umd					!>>>>>>>> MIQUEL 30-4-13
real*8   ::nodda,posca
integer  ::ivv		!>>>>>>>> MIQUEL 4-3-13
integer  ::i,j,ii,jj,kk,ic,iii,jjj,kkk,iiii,jjjj,kkkk,iv,kjjj,jkkk
integer  ::nuve,inuve
integer  ::tipi,tipic																!>>>>>>>>>>>>>>>>>>>>>Miquel 23-4-13
integer  ::switch,twoep

integer  ::lateral,vertical !flags that tell if there is a lateral or vertical component to take into account !>>Miquel28-1-14

real*8   ::rvx,rvy,rvz   !the resulting force vector
real*8   ::uvx,uvy,uvz   !unit vector
real*8   ::pox,poy,poz   !polarisation vector (from the cell)

real*8   ::ad,fd  !>>Miquel28-1-14

real*8   ::upr(nd)
integer  ::iupr(nd)
real*8   ::r(nd),er(nd)

real*8   ::rcilx(nd),rcily(nd),rcilz(nd)
real*8   ::rtorx(nd),rtory(nd),rtorz(nd)
real*8   ::rstorx(nd),rstory(nd),rstorz(nd)
real*8   ::rsprx,rspry,rsprz


!integer, parameter :: mnn=200  !declared in general module !>>Miquel24-2-14

                              ! ACHTUNG, WE ASSUME THAT THE MAXIMAL NUMBER OF NODES INTERACTING WITH A NODE IS mnn, otherwise these 
                              ! matrices become unbareably large for large systems
!integer  ::vei(omnn,nd)        ! force storing arrays for optimization    >>>>>>Miquel 7-8-16
real*8   ::vforce(omnn,nd)     ! so we don't calculate each interaction twice
real*8   ::vuvx(omnn,nd)       ! (for now we do it only for cylinder and normal interactions,
real*8   ::vuvy(omnn,nd)       ! not torsion nor springs)
real*8   ::vuvz(omnn,nd)       !
real*8   ::vtorforce(omnn,nd)  ! NOTE THE NON-CONVENTIONAL FORM OF THE ARRAYS:
real*8   ::vtoruvx(omnn,nd)    ! DIM 1 IS THE LISTS THE NEIGHBORS,
real*8   ::vtoruvy(omnn,nd)    ! DIM 2 LISTS THE NODE ID
real*8   ::vtoruvz(omnn,nd)    !
real*8   ::vstorforce(omnn,nd) !
real*8   ::vstoruvx(omnn,nd)   !
real*8   ::vstoruvy(omnn,nd)   !
real*8   ::vstoruvz(omnn,nd)   !
!integer ::nveins(nd)         !
integer  ::epinveins(nd)      ! we store how many same-side epithelial neighbors an epithelial node has !>>>Miquel4-4-14
integer  ::alone              ! 0 if the node is really alone

  vcilx=0 ; vcily=0 ; vcilz=0 ; vsprx=0     !force vectors storage matrices, for different components
  vtorx=0 ; vtory=0 ; vtorz=0 ; vspry=0     !of the resulting force
  vstorx=0 ; vstory=0 ; vstorz=0 ; vsprz=0
  vei=0 ; vforce=0 ; vuvx=0 ; vuvy=0 ; vuvz=0 ; nveins=0  !>>>>>>Miquel 7-8-16
  vtorforce=0 ; vtoruvx=0 ; vtoruvy=0 ; vtoruvz=0         !
  vstorforce=0 ; vstoruvx=0 ; vstoruvy=0 ; vstoruvz=0     !

  fmeanl=0 ; fmeanv=0  !storage vector that makes the balance between compressive and tensile forces within a node    !>>>Miquel23-1-14

  rcilx=0d0  ; rcily=0d0  ; rcilz=0d0  !they store the force components for all the nodes !>>Miquel4-4-14
  rtorx=0d0  ; rtory=0d0  ; rtorz=0d0
  rstorx=0d0 ; rstory=0d0 ; rstorz=0d0
  epinveins=0
  
  do nod=1,nd
    !lonely=0 !fossile? >>Miquel27-2-14

    if (node(nod)%hold==2) then                ! >>> Is 30-6-14
      dex(nod)=0  !module of the force vector ! >>> Is 30-6-14
      px(nod)=0 ; py(nod)=0 ; pz(nod)=0       ! >>> Is 30-6-14
      cycle                                   ! >>> Is 30-6-14
    end if                                    ! >>> Is 30-6-14

    rsprx=0.0d0  ; rspry=0.0d0; rsprz=0.0d0
    ix=node(nod)%x ; iy=node(nod)%y ; iz=node(nod)%z
    tipi=node(nod)%tipus ; celi=node(nod)%icel
    iii=nint(ix*urv)    ; jjj=nint(iy*urv)   ; kkk=nint(iz*urv)	
    rvx=0d0    ; rvy=0d0    ; rvz=0d0
    nuve=0!nuve=nveins(nod) !>>>>>Miquel 7-8-13 : this is not always zero since we fill this matrix from its neighbours
    switch=0
    alone=0

    !SPRINGS
    
      iv=0    !>>>>Miquel17-1-14

    younod=node(nod)%you                                            !>>>>>Miquel 16-8-13
    repnod=node(nod)%rep                                            !
    adhnod=node(nod)%adh !default inespecific adhesion of node nodmo!
    repcelnod=node(nod)%repcel                                      !
    tornod=node(nod)%tor                                            !
    stornod=node(nod)%stor                                          !
    reqnod=node(nod)%req                                            !
    nodda=node(nod)%da

    !NODE's REPULSIONS AND ADHESIONS

    do i=1,nneigh(nod)
      ic=neigh(nod,i)
      if(ic<nod)then !we have calculated that interaction already !>>Miquel4-4-14
        alone=1
        cycle
      end if

      !so it turns out that the nod-ic interactions has not been calculated before

      bx=node(ic)%x   ; by=node(ic)%y    ; bz=node(ic)%z
      ccx=bx-ix       ; ccy=by-iy        ; ccz=bz-iz		
      !d=sqrt(ccx**2+ccy**2+ccz**2)
      d=dneigh(nod,i)
      ud=1d0/d
      tipic=node(ic)%tipus
      twoep=0
      ad=0 ; fd=0 ; ddd=0     !>>Miquel28-1-14

      alone=1      !this is crappy but fast, it makes that lonely nodes are eliminated in squares
       
          fd=d !BOTH NODES ARE NON-EPITHELIAL: we just consider the interactions between nodes as such
          if (fd-nodda-node(ic)%da>epsilod) cycle
          uvx=ccx*ud ; uvy=ccy*ud ; uvz=ccz*ud  !unit vector of the within cilinder force !el modul és el mateix que el vector c
       
300   nuve=nuve+1

      if (nuve>mnn) then; 
        print *,"PANIC!!! PANIC!!!PANIC!!!; this is going to crash because there is too many " ; 
        print *,"neighbors per a node to interact, and vuvx matrix and those are too small"
      end if 

      !ALL THAT WAS JUST TO CALCULATE THE RIGHT DISTANCE BETWEEN NODES, ddd, NOW WE CALCULATE THE ACTUAL ENERGIES
      !if (ffu(2)==1) then IS 25-12-13
        if(node(nod)%icel==node(ic)%icel)then
          deqe=(reqnod+node(ic)%req)             !>>>> Miquel 16-8-13
          if(fd-deqe<-epsilod)then 				
            repe=0.5d0*(repnod+node(ic)%rep)     !>>>> Miquel 16-8-13
            f=2*repe*(fd-deqe)
          else
            youe=(younod+node(ic)%you)          !>>>> Miquel 16-8-13
            f=youe*(fd-deqe)
          end if
        else
          deqe=(reqnod+node(ic)%req)          !>>>> Miquel 16-8-13
          if(fd-deqe<-epsilod)then 
            repcele=(repcelnod+node(ic)%repcel) !>>>> Miquel 16-8-13
            f=repcele*(fd-deqe)       !in fact that is the derivative of the energy
          else
            adhe=0.5d0*(adhnod+node(ic)%adh)          !>>>> Miquel 16-8-13
            if (npag(1)>0) then ! we have adhesion molecules
              do j=1,npag(1)
                k=whonpag(1,j)
                do kjjj=1,npag(1)
                  jkkk=whonpag(1,kjjj)
                  if (gex(nod,k)>0.0d0.and.gex(ic,jkkk)>0.0d0) then     
                    adhe=adhe+gex(nod,k)*gex(ic,jkkk)*kadh(int(gen(k)%wa(1)),int(gen(jkkk)%wa(1))) ! >>> Is 7-6-14    !this is specific adhesion
                  end if
                end do
              end do
            end if
            f=2*adhe*(fd-deqe)          !in fact that is the derivative of the energy
          end if
        end if 
     
        rcilx(nod)=rcilx(nod)+f*uvx ; rcily(nod)=rcily(nod)+f*uvy ; rcilz(nod)=rcilz(nod)+f*uvz  !>>>Miquel 7-8-13
        rcilx(ic)=rcilx(ic)-f*uvx ; rcily(ic)=rcily(ic)-f*uvy ; rcilz(ic)=rcilz(ic)-f*uvz  !>>>Miquel 4-4-14
             
        fmeanl(nod)=fmeanl(nod)+(fd-deqe) ; fmeanl(ic)=fmeanl(ic)+(fd-deqe) ! >>> Is 21-6-14

324   continue
    end do

    if(epinveins(nod)>0)then                  !>>>Miquel24-3-14
      fmeanl(nod)=fmeanl(nod)/epinveins(nod)
    end if

789 if (ffu(8)==1) then   !this is kind of crappy in the sense that 
      if (alone==0) then
        node(nod)%talone=node(nod)%talone+1
      else
        node(nod)%talone=0
      end if
    end if
   
    if (aut==0) then ! if aut==0 there is not display   
      vsprx(nod)=rsprx ; vspry(nod)=rspry ; vsprz(nod)=rsprz                     !putting the force components into storage vectors for display
      vcilx(nod)=rcilx(nod)    ; vcily(nod)=rcily(nod)    ; vcilz(nod)=rcilz(nod)         !this part should be turned down
      vtorx(nod)=rtorx(nod)    ; vtory(nod)=rtory(nod)    ; vtorz(nod)=rtorz(nod)         !when there is no display
      vstorx(nod)=rstorx(nod)  ; vstory(nod)=rstory(nod)  ; vstorz(nod)=rstorz(nod)!
    end if

    if (epinveins(nod)>0) then  ! >>> Is 7-6-14
      a=epinveins(nod) ! >>> Is 7-6-14
      a=1.0d0/a        ! >>> Is 7-6-14
      rvx=rsprx+rcilx(nod)+(rtorx(nod)+rstorx(nod))*a  ! >>> Is 7-6-14 !summing the force components into a net force vector
      rvy=rspry+rcily(nod)+(rtory(nod)+rstory(nod))*a  ! >>> Is 7-6-14
      rvz=rsprz+rcilz(nod)+(rtorz(nod)+rstorz(nod))*a  ! >>> Is 7-6-14
    else
      rvx=rsprx+rcilx(nod)  ! >>> Is 7-6-14 !summing the force components into a net force vector
      rvy=rspry+rcily(nod)  ! >>> Is 7-6-14
      rvz=rsprz+rcilz(nod)  ! >>> Is 7-6-14
    end if

    !Physical boundaries force  !>>>>>Miquel9-1-14
    if(node(nod)%hold==1)then
      uvx=node(nod)%orix-ix
      uvy=node(nod)%oriy-iy
      uvz=node(nod)%oriz-iz
      d=uvx**2+uvy**2+uvz**2 ; if(d>epsilod)then ; ud=1d0/sqrt(d);else;d=0;end if
      a=node(nod)%khold
      rvx=rvx+uvx*a*ud    !now the force is constant        !no need to calculate the unit vector, because here you have the product of the unit vector and the distance,
      rvy=rvy+uvy*a*ud    !to make it a spring remove ud    !wich is the original vector
      rvz=rvz+uvz*a*ud
    end if

    dex(nod)=sqrt(rvx**2+rvy**2+rvz**2)  !module of the force vector
    px(nod)=rvx ; py(nod)=rvy ; pz(nod)=rvz
  end do
  
end subroutine forces

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ordenarepeq(ma,mt,rang)
  integer rang
  real*8 ma(rang)
  integer mt(rang)
  integer i,j,k
  real*8 a
    mt=0
el: do i=1,rang
      a=ma(i) ; k=1
      do j=1,rang ; if (a>ma(j)) k=k+1 ; end do 
      do j=k,rang ; if (mt(j)==0) then ; mt(j)=i ; cycle el ; end if ; end do
    end do el 
end subroutine ordenarepeq


end module biomechanic
