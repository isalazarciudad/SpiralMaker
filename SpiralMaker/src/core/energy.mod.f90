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

module energy
use general
use genetic

contains

!**************************************************************************

subroutine energia(nod) !>REMADE SUBSTANTIALLY Isaac 5-6-13
real*8   ::ix,iy,iz,dd
real*8   ::a,b,c,d,e,f,g
real*8   ::ie,posca
integer  ::nod
real*8   ::ene,ened,youe,repe,adhe,repcele,deqe,ideqe,adhed
real*8   ::reqnod,younod,repnod,adhnod,repcelnod,tornod,stornod
real*8   ::ax,ay,az,bx,by,bz
real*8   ::udd
real*8   ::cx,cy,cz,ccx,ccy,ccz,pesc
real*8   ::icx,icy,icz,idd,iudd,id !>>>>>>>> MIQUEL 4-3-13
real*8   ::mcx,mcy,mcz,md
real*8   ::nodda
integer  ::ivv		!>>>>>>>> MIQUEL 4-3-13
integer  ::i,j,ii,jj,kk,ic,iii,jjj,kkk,iiii,jjjj,kkkk,iv
integer  ::kj,kjjj
integer  ::loi,loj,lok
integer  ::nuve,twoep
real*8   ::ener(nd),enea(nd),enet(nd),iener(nd),ienea(nd)		!we will store the energies calculated for each node, then calculate the shielding and apply to them
real*8   ::enes,inuve
integer  ::tipi,tipic	!>>>>>>>>>>>>>>>>>>>>>Miquel 23-4-13
real*8,  allocatable   ::upr(:)
integer, allocatable  ::iupr(:)
real*8   ::r(nd),er(nd)
real*8   ::dotp,ddd
real*8   ::ad,fd  !>>Miquel28-1-14

  if (node(nod)%hold==2) then  ! >>> Is 30-6-14
    node(nod)%e=huge(a)        ! >>> Is 30-6-14
    return                     ! >>> Is 30-6-14
  end if                       ! >>> Is 30-6-14

  nuve=0
  nodda=node(nod)%da
  ix=node(nod)%x      ; iy=node(nod)%y     ; iz=node(nod)%z
  tipi=node(nod)%tipus											!>>>>>>>>>>>>>>>>>>>MIQUEL 4-3-13
  !iii=nint(ix*urv)    ; jjj=nint(iy*urv)   ; kkk=nint(iz*urv)
  ie=0.0
  ener=0d0 ; iener=0d0 ; ienea=0d0 ; enea=0d0 ; enet=0d0 ; enes=0d0 !energy storing matrices (es guarda l'energia aqui per aplicar després l'apantallament		!>>>>>>>>>>>>>>>>>>>MIQUEL 4-3-13
  younod=node(nod)%you																			
  repnod=node(nod)%rep																			
  adhnod=node(nod)%adh   !default inespecific adhesion of node nodmo																			
  repcelnod=node(nod)%repcel
  tornod=node(nod)%tor                                            !
  stornod=node(nod)%stor                                          !
  reqnod=node(nod)%req

  !NODE's REPULSIONS AND ADHESIONS

  do ii=1,nneigh(nod)
    ic=neigh(nod,ii)
    bx=node(ic)%x   ; by=node(ic)%y    ; bz=node(ic)%z
    ccx=bx-ix       ; ccy=by-iy        ; ccz=bz-iz		
    ddd=0d0  !es necessari?
    d=sqrt(ccx**2+ccy**2+ccz**2)

    tipic=node(ic)%tipus		    
    twoep=0
   
        fd=d !BOTH NODES ARE NON-EPITHELIAL: we just consider the interactions between nodes as such
        if (fd-nodda-node(ic)%da>epsilod) cycle     

301 nuve=nuve+1		!apantallaments
    r(nuve)=fd              

    !ALL THAT WAS JUST TO CALCULATE THE RIGHT DISTANCE BETWEEN NODES, ddd, NOW WE CALCULATE THE ACTUAL ENERGIES
    if(node(nod)%icel==node(ic)%icel)then
      youe=0.5*(younod+node(ic)%you)
      repe=0.5*(repnod+node(ic)%rep)
      deqe=reqnod+node(ic)%req
      ideqe=((nodda+node(ic)%da-deqe)/deqe)**2
      if(fd-deqe<-epsilod)then 				
        ener(nuve)=ener(nuve)+repe*((fd-deqe)/deqe)**2-youe*ideqe !((node(nod)%da-deqe)/deqe)**2 !this is the repulsion energy for entering the cilinder
      else
        enea(nuve)=enea(nuve)+youe*((fd-deqe)/deqe)**2-youe*ideqe !((node(nod)%da-deqe)/deqe)**2 !this is the adhesion or you 
      end if
    else																							
      adhe=0.5d0*(adhnod+node(ic)%adh) !adhesion is necessarily symmetric
      repcele=0.5*(repcelnod+node(ic)%repcel)
      deqe=reqnod+node(ic)%req
      ideqe=((nodda+node(ic)%da-deqe)/deqe)**2
      if (npag(1)>0) then ! we have adhesion molecules
        do j=1,npag(1)
          kj=whonpag(1,j)
          do kjjj=1,npag(1)
            kkkk=whonpag(1,kjjj)
            if (gex(nod,kj)>0.0d0.and.gex(ic,kkkk)>0.0d0) then     
     adhe=adhe+gex(nod,kj)*gex(ic,kkkk)*kadh(int(gen(kj)%wa(1)),int(gen(kkkk)%wa(1)))    !this is specific adhesion
            end if
          end do
        end do
      end if

      if(fd-deqe<-epsilod)then 
        iener(nuve)=iener(nuve)+repcele*((fd-deqe)/deqe)**2-adhe*ideqe !((node(nod)%da-deqe)/deqe)**2!this is the repulsion energy for entering the cilinder
      else
        ienea(nuve)=ienea(nuve)+adhe*((fd-deqe)/deqe)**2-adhe*ideqe !((node(nod)%da-deqe)/deqe)**2 !this is the adhesion or you   
      end if																							
    end if								
    er(nuve)=ener(nuve)+enea(nuve)+iener(nuve)+ienea(nuve)
  
  end do
       
    do i=1,nuve
      ie=ie+ener(i)+iener(i)+enea(i)+ienea(i)+enet(i)		!we may want to apply shielding only to some type of forces
    end do
  !end if
  node(nod)%e=ie
  ! end >>>>>>>>< Is 22-6-13

  erep(nod)=sum(ener)
  erepcel(nod)=sum(iener)
  eyou(nod)=sum(enea)
  eadh(nod)=sum(ienea)
  etor(nod)=sum(enet)
  espring(nod)=enes

end subroutine

end module
