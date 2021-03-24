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

module mitosis  !>>>>>>>MIQUEL MADE MODULE 21-3-13
use general
use neighboring  !veinatge
use io
use aleas
use pola

contains

!*********************************************************************************
subroutine should_I_divide  ! updates fase and checks out if fase=1 and nunodes<minsize_for_div to see if a cell can divide
integer ick,j,k,ii,kk
real*8 a,b,c,s,sx,sy,sz,d 
if(ncels.ge.32)then;return;endif ! modifico
do ick=1,ncels
  s=0.0d0
  do j=1,cels(ick)%nunodes
    ii=cels(ick)%node(j)
    c=1-node(ii)%diffe
    do k=1,npag(nparam_per_node+2)
      kk=whonpag(nparam_per_node+2,k)
      if (gex(ii,kk)>0.0d0) then
        s=s+gex(ii,kk)*gen(kk)%wa(nparam_per_node+2)*c  !wa in units of fase until a fase of 1
      end if
    end do
  end do   
  s=s/real(cels(ick)%nunodes) !this way %fase is independent of cell size !>>>>Miquel2-12-13
  cels(ick)%fase=cels(ick)%fase+s!*delta !>>Miquel2-2-15

      call neighbor_build ; call membrane ; call flow(ick) !>>> Miguel 20-10-14 
      call division(ick) ; call neighbor_build ; call membrane !; call flow(ick) !>>> Miguel 20-10-14
      cels(ick)%fase=cels(ick)%fase-1d0

  if (cels(ick)%nunodes>=cels(ick)%maxsize_for_div) then !>>> Is 23-4-14  !ACHTUNG POSAR UN REQUERIMENT DE AREA???
     write(*,*)'entra por 2?'
    call neighbor_build ; call membrane       ; call flow(ick) !>>> Miguel 20-10-14
    call division(ick)  ; call neighbor_build ; call membrane !; call flow(ick) !>>> Miguel 20-10-14
    cels(ick)%fase=cels(ick)%fase-1d0
  end if
end do
end subroutine

!***********************************************************************************

subroutine recels	!amplia la matriu de celules
integer::i,j
type(cel),allocatable :: ccels(:)

	allocate(ccels(ncals))
	do i=1,ncels
      j=cels(i)%nodela
      allocate(ccels(i)%node(j))
      ccels(i)%nunodes=cels(i)%nunodes
      ccels(i)%minsize_for_div=cels(i)%minsize_for_div
      ccels(i)%maxsize_for_div=cels(i)%maxsize_for_div !>>> Is 5-2-14
      ccels(i)%fase=cels(i)%fase
      ccels(i)%node(:j)=cels(i)%node(:j)
      ccels(i)%nodela=cels(i)%nodela
      ccels(i)%cex=cels(i)%cex ; ccels(i)%cey=cels(i)%cey ; ccels(i)%cez=cels(i)%cez
      ccels(i)%polx=cels(i)%polx ; ccels(i)%poly=cels(i)%poly ; ccels(i)%polz=cels(i)%polz
      ccels(i)%ctipus=cels(i)%ctipus
      ccels(i)%sister=cels(i)%sister
	end do
	deallocate(cels)

	ncals=ncals+20

	allocate(cels(ncals))
	do i=1,ncels
	  allocate(cels(i)%node(ccels(i)%nodela))
	  cels(i)%node=0
	  cels(i)%node(1:ccels(i)%nunodes)=ccels(i)%node(1:ccels(i)%nunodes)
	  cels(i)%nunodes=ccels(i)%nunodes
	  cels(i)%nodela=ccels(i)%nodela
	  cels(i)%minsize_for_div=ccels(i)%minsize_for_div
	  cels(i)%maxsize_for_div=ccels(i)%maxsize_for_div !>>> Is 5-2-14
	  cels(i)%fase=ccels(i)%fase
	  cels(i)%cex=ccels(i)%cex ; cels(i)%cey=ccels(i)%cey ; cels(i)%cez=ccels(i)%cez
	  cels(i)%polx=ccels(i)%polx ; cels(i)%poly=ccels(i)%poly ; cels(i)%polz=ccels(i)%polz
          cels(i)%ctipus=ccels(i)%ctipus
          cels(i)%sister=ccels(i)%sister
	end do

	deallocate(ccels)

end subroutine recels

!***************************************************************************************************

subroutine division(celd) ! miguel 14-10-13
integer::celd,nnod,tipi,nnoda,nnodb
real*8 ::a,b,c,ax,ay,az,bx,by,bz,cx,cy,cz,pesc,ix,iy,iz,iix,iiy,iiz,lambda,modulov,sch1,sch2,sch3
integer,dimension(:)::nodea(cels(celd)%nunodes),nodeb(cels(celd)%nunodes)

    nodea=0;nodeb=0;modulov=0d0
    nnod=cels(celd)%nunodes
    tipi=cels(celd)%ctipus	

    call pol_physic(celd,bx,by,bz) !determining the plane of division (taken from the longest cell axis)
    call pol_special(celd,3)!;endif !3 es subvolumen, 4 es contacto

    a=0 ;b=0 ;c=0   !the centroid of the cell
    cx=0;cy=0;cz=0  !the vector normal to the plane of division (epithelium only)
    ax=0;ay=0;az=0  !the apical-basal vector (epithelium only)    

    do i=1,nnod     !sum of the apical-basl vectors
      j=cels(celd)%node(i)
      if(node(j)%tipus==1)then
        k=node(j)%altre
        ax=ax+node(j)%x-node(k)%x;ay=ay+node(j)%y-node(k)%y;az=az+node(j)%z-node(k)%z
      end if
    end do
    a=a/real(nnod);b=b/real(nnod);c=c/real(nnod)
    if(tipi<3)then	!the plane of division has to contain the apical-basal vector
      d=sqrt(ax**2+ay**2+az**2)
      ax=ax/d;ay=ay/d;az=az/d
      pesc=ax*bx+ay*by+az*bz
      a=bx-ax*pesc       !projection of the random vector on the plane ortogonal to the apical-basal vector
      b=by-ay*pesc
      c=bz-az*pesc    !vector fisico
      d=1d0/sqrt(a**2+b**2+c**2)
      cels(celd)%hpolx=a*d ; cels(celd)%hpoly=b*d ; cels(celd)%hpolz=c*d
    else
      cels(celd)%hpolx=bx ; cels(celd)%hpoly=by ; cels(celd)%hpolz=bz  !vector fisico
    end if         
       
    !!!!!!!!!!!!!!!!!!!!!!!!!! "polarization and cell-cell contact rule" !!!!!!!!!!!!
      cels(celd)%polx=0d0 ; cels(celd)%poly=0d0 ; cels(celd)%polz=0d0 
      ix=0d0 ; iy=0d0 ; iz=0d0
      call polarigen(celd,2)     
      ix=0.1d0*real(ffu(27))*cels(celd)%polx ; iy=0.1d0*real(ffu(27))*cels(celd)%poly ; iz=0.1d0*real(ffu(27))*cels(celd)%polz             
      call polarigen(celd,4)         
      cels(celd)%polx=(0.1d0*real(ffu(28))*cels(celd)%polx)+ix
      cels(celd)%poly=(0.1d0*real(ffu(28))*cels(celd)%poly)+iy
      cels(celd)%polz=(0.1d0*real(ffu(28))*cels(celd)%polz)+iz      
      d=sqrt((cels(celd)%polx)**2+(cels(celd)%poly)**2+(cels(celd)%polz)**2)
      cels(celd)%polx=cels(celd)%polx/d ; cels(celd)%poly=cels(celd)%poly/d ; cels(celd)%polz=cels(celd)%polz/d    
      if((ffu(27).eq.0).and.(ffu(28).eq.0))then
       cels(celd)%polx=0d0 ; cels(celd)%poly=0d0 ; cels(celd)%polz=0d0
      end if   
    !!!!!!!!!!!!!!!!!!!!!!!!!!      

    d=0.0d0
    do kk=1,cels(celd)%nunodes
      j=cels(celd)%node(kk)
      do jj=1,npag(nparam_per_node+11)    !number of genes affecting growth
        k=whonpag(nparam_per_node+11,jj)  !which are those genes
        d=d+gex(j,k)*gen(k)%wa(nparam_per_node+11) !this is the ponderation between the hertwig vector and the polarization
      end do                                       ! wa is then in units like a probability, 0-1
    end do
    !d=1d0/(1d0+d)             !ponderacion entre vector fisico i quimico
    d=0.1d0*real(ffu(26))      !ponderacion entre vector fisico i quimico (pero con ffu(26))
                              
    !d    ; dependence of the gradient vector (how many it affects to polarization vector)
    !d=0  ; polarization vector comes only from its shape  (default mode) 
    !d=1  ; polarization vector comes only from the gradient       
    !d=0.5; polarization vector comes equally from both the gradient and the shape    
  
    cx=((1-d)*cels(celd)%polx)+(d*cels(celd)%hpolx)   !vector resultante para division
    cy=((1-d)*cels(celd)%poly)+(d*cels(celd)%hpoly)
    cz=((1-d)*cels(celd)%polz)+(d*cels(celd)%hpolz)
    
    modulov=sqrt(cx**2+cy**2+cz**2)    
    cx=cx/modulov ; cy=cy/modulov ; cz=cz/modulov

    ix=cels(celd)%cex-cels(cels(celd)%sister)%cex     
    iy=cels(celd)%cey-cels(cels(celd)%sister)%cey
    iz=cels(celd)%cez-cels(cels(celd)%sister)%cez     ! punto medio(y vector) de las dos celulas (para plano)                   

    if(ncels.lt.4)then        !modifico      
      if(ncels.eq.1)then
        cx=1d0  ; cy=1d-4 ; cz=1d-4 
      else
        cx=-1d0*iy ; cy=ix ; cz=0d0
        modulov=sqrt(cx**2+cy**2+cz**2) 
        cx=cx/modulov ; cy=cy/modulov ; cz=cz/modulov
     endif     
    end if

    if(ffu(25).ne.0)then !sach rule
      iix=cx ; iiy=cy ; iiz=cz 
      lambda=-1d0*(ix*iix+iy*iiy+iz*iiz)  !SACH'S RULE
      lambda=lambda/(ix**2+iy**2+iz**2)
      if(ffu(25).eq.10)then ! total sach
        cx=iix+lambda*ix
        cy=iiy+lambda*iy
        cz=iiz+lambda*iz 
      else !if(ffu(25).eq.5)then        ! just a "tendency" towards perpendicularity
        sch1=iix+lambda*ix ; sch2=iiy+lambda*iy ; sch3=iiz+lambda*iz
        modulov=sqrt(sch1**2+sch2**2+sch3**2)
        sch1=sch1/modulov ; sch2=sch2/modulov ; sch3=sch3/modulov
        cx=cx+sch1 ; cy=cy+sch2 ; cz=cz+sch3
        modulov=sqrt(cx**2+cy**2+cz**2) 
        cx=cx/modulov ; cy=cy/modulov ; cz=cz/modulov        
      end if
      if(ncels.eq.1)then;cx=1d0;cy=1d-4;cz=1d-4;endif!cx=cels(1)%spolx;cy=cels(1)%spoly;cz=cels(1)%spolz;endif ! it has not sister cell !       
    end if
   
    !print *,d,"division",getot,'xyz',cx,cy,cz
   if(ncels.ge.4)then
      call assymetric(celd,a,b,c,cx,cy,cz)       
    else
      call assymetricpercent(celd,a,b,c,cx,cy,cz)
    end if
    
    ncels=ncels+1
    nnoda=0;nnodb=0

    norest=nnod
    do i=1,nnod
      j=cels(celd)%node(i)
      jj=node(j)%tipus
      if (jj==1.or.jj==3)then
	ix=node(j)%x-a ;iy=node(j)%y-b ;iz=node(j)%z-c	!vector from centroid to node
	dd=1/sqrt(ix**2+iy**2+iz**2)
	pesc=(ix*cx+iy*cy+iz*cz)*dd
        if(pesc>0) then  ! >>> Is 11-6-14 new cell
          if(jj==1) then	!if it's epithelium, we select the whole pair of nodes
            nnodb=nnodb+2
            nodeb(nnodb)=j            ! >>> Is 21-6-14
            node(j)%icel=ncels        ! >>> Is 21-6-14
            nodeb(nnodb-1)=node(j)%altre  ! >>> Is 21-6-14
            node(node(j)%altre)%icel=ncels! >>> Is 21-6-14
          else
            nnodb=nnodb+1
            nodeb(nnodb)=j
            node(j)%icel=ncels
          end if
        else				    !old cell
          if(jj==1) then	!if it's epithelium, we select the whole pair of nodes
            nnoda=nnoda+2
            nodea(nnoda)=j      ! >>> Is 21-6-14
            nodea(nnoda-1)=node(j)%altre ! >>> Is 21-6-14
          else
            nnoda=nnoda+1
            nodea(nnoda)=j
          end if
        end if
      end if
    end do	

    !print *,nnoda,nnodb,"mitosis",real(nnoda)/real(nnoda+nnodb)

    a=0 ; b=0 ; c=0      !recalculating the centroids
    do i=1,nnoda
      j=nodea(i)
      if(node(j)%tipus==1.or.node(j)%tipus==3)then
        a=a+node(j)%x ; b=b+node(j)%y ; c=c+node(j)%z
      end if
    end do
    if(tipi<3)then
      cels(celd)%cex=2*a/nnoda ; cels(celd)%cey=2*b/nnoda ; cels(celd)%cez=2*c/nnoda
    else
      cels(celd)%cex=a/nnoda ; cels(celd)%cey=b/nnoda ; cels(celd)%cez=c/nnoda
    end if

    a=0 ; b=0 ; c=0
    do i=1,nnodb
      j=nodeb(i)
      if(node(j)%tipus==1.or.node(j)%tipus==3)then
        a=a+node(j)%x ; b=b+node(j)%y ; c=c+node(j)%z
      end if
    end do
    if(tipi<3)then
      cels(ncels)%cex=2*a/nnodb ; cels(ncels)%cey=2*b/nnodb ; cels(ncels)%cez=2*c/nnodb
    else
      cels(ncels)%cex=a/nnodb ; cels(ncels)%cey=b/nnodb ; cels(ncels)%cez=c/nnodb
    end if

    cels(celd)%nunodes=nnoda
    cels(ncels)%nunodes=nnodb
    deallocate(cels(celd)%node)
    cels(celd)%nodela=nnoda+10
    cels(ncels)%nodela=nnodb+10
    allocate(cels(celd)%node(nnoda+10),cels(ncels)%node(nnodb+10))
    cels(celd)%node(:)=0;cels(ncels)%node(:)=0
    cels(celd)%node(1:nnoda)=nodea(1:nnoda)
    cels(ncels)%node(1:nnodb)=nodeb(1:nnodb)

    if(tipi<3)then
      cels(ncels)%ctipus=1
    else
      cels(ncels)%ctipus=3
    end if	
    cels(ncels)%sister=celd !miguel24-11-14
    cels(celd)%sister =ncels !miguel24-11-14
   
    !nuclei     we delete the former nucleus and set the new nuclei as the node closer to the centroid  >>>>Miquel4-10-13
    a=1000
    do i=1,nnoda !celd
      j=cels(celd)%node(i)
      if(node(j)%marge==0) node(j)%marge=1
      d=sqrt((node(j)%x-cels(celd)%cex)**2+(node(j)%y-cels(celd)%cey)**2+(node(j)%z-cels(celd)%cez)**2)
      if(d<a)then;a=d;ii=j;end if
    end do
    if(node(ii)%tipus==1)then
      node(node(ii)%altre)%marge=0
    else
      node(ii)%marge=0
    end if

    a=1000
    do i=1,nnodb !ncels
      j=cels(ncels)%node(i)
      if(node(j)%marge==0) node(j)%marge=1
      d=sqrt((node(j)%x-cels(ncels)%cex)**2+(node(j)%y-cels(ncels)%cey)**2+(node(j)%z-cels(ncels)%cez)**2)
      if(d<a)then;a=d;ii=j;end if
    end do

    if(node(ii)%tipus==1)then
      node(node(ii)%altre)%marge=0
    else
      node(ii)%marge=0
    end if


    cels(ncels)%minsize_for_div=cels(celd)%minsize_for_div
    cels(ncels)%maxsize_for_div=cels(celd)%maxsize_for_div  !>>> Is 5-2-14

    if (ncels>=ncals) call recels

    cels(ncels)%polx=cels(celd)%polx  !>>> Is 5-5-14
    cels(ncels)%poly=cels(celd)%poly  !>>> Is 5-5-14 
    cels(ncels)%polz=cels(celd)%polz  !>>> Is 5-5-14

    counterflow=ffu(24)!counterflowk!miguel9-1-15

!!!!!!!!!!!!!!!!!!!!!rehago el %sister ::(da igual que lo hagan todas, se dividen mas o menos a ala vez...)
do i=1,ncels
   kk=cels(i)%sister
    cels(i)%sisterv(1)=cels(kk)%cex-cels(i)%cex ; cels(i)%sisterv(2)=cels(kk)%cey-cels(i)%cey !miguel9-1-15
    cels(i)%sisterv(3)=cels(kk)%cez-cels(i)%cez 
    a=sqrt((cels(i)%sisterv(1))**2+(cels(i)%sisterv(2))**2+(cels(i)%sisterv(3))**2)
    cels(i)%sisterv=cels(i)%sisterv/a
    cels(kk)%sisterv(1)=cels(i)%cex-cels(kk)%cex  ; cels(kk)%sisterv(2)=cels(i)%cey-cels(kk)%cey  
    cels(kk)%sisterv(3)=cels(i)%cez-cels(kk)%cez  !miguel9-1-15
    a=sqrt((cels(kk)%sisterv(1))**2+(cels(kk)%sisterv(2))**2+(cels(kk)%sisterv(3))**2)
    cels(kk)%sisterv=cels(kk)%sisterv/a
end do
!!!!!!!!!!!!!!!!!!!!!    
    gex(:nd,8)=(node(:nd)%z+minval(node(:)%z))**2 ; agex(:nd,8)=(node(:nd)%z+minval(node(:)%z))**2   
    if(ffu(5)==4)then  !>>>Miguel 28-1-15 "if"                
      agex(:nd,2)=6d0*(node(:nd)%z+minval(node(:)%z))**2!/sqrt(node(:nd)%x**2+node(:nd)%y**2)    
    end if
    !WRITE(*,*)'FINMITOSIS'

end subroutine

!*******************************************************************************
subroutine assymetricpercent(celd,a,b,c,bx,by,bz) !!!!!!!!!!!!!!!!!!!!!!!!!>>>Miguel 28-1-15 subroutine    ! assymetric division !!!!!!!!!!!!!!!!! 
! input: cell index(celd),old centroid(a,b,c),polarization vector(bx,by,bz) ; output: new centroid (a,b,c)
integer::celd,nnod,antn,postn,disc
real*8::a,b,c,d,e,bx,by,bz,ix,iy,iz,alfa,proj,asim,percent                ! miguel 30-5-13          
integer,dimension(cels(celd)%nunodes) :: ntot                     ! ordered nodes along the polarization vector    
real*8,dimension(cels(celd)%nunodes)  :: ctot,ctotaux             ! vectors of node distances alon polarization vector     

        a=cels(celd)%cex ; b=cels(celd)%cey ; c=cels(celd)%cez    !>>>Miguel30-6-14
        percent=0.5d0
        nnod=cels(celd)%nunodes    

	do i=1,nnod                                               !to find the maximum node distances to the plane
	     j=cels(celd)%node(i)
	     ix=node(j)%x-cels(celd)%cex;iy=node(j)%y-cels(celd)%cey;iz=node(j)%z-cels(celd)%cez  !vector from centroid to node 3D     
	     proj=bx*ix+by*iy+bz*iz                               !scalar product  
	     asim=sqrt(ix**2+iy**2+iz**2)                         !module of radial vector
	     alfa=acos(proj/(sqrt(bx**2+by**2+bz**2)*asim))       !angle with polarization vector	       
	     asim=cos(alfa)*asim                                  !module of projected vector	   
	     ctot(i)=asim                                         !it locates the "extreme nodes"	     
	end do	

        ctotaux=maxval(ctot)+1d0                                  ! to order the nodes along the polarization vector
        do i=1,nnod ; ntot(i)=i ; end do                          ! order by default
        do i=1,nnod                                               ! nodes ordered along polarization vector
        x=ctot(i) 
          do j=1,nnod
             if(x.lt.ctotaux(j))then
                do k=nnod,j+1,-1           
                ctotaux(k)=ctotaux(k-1) ; ntot(k)=ntot(k-1)          
                end do
                ctotaux(j)=x ;ntot(j)=i 
             exit;endif     
          end do 
       end do

       do i=1,nnod-1                                             ! it search when the concentrations are equal ...            
         if(real(i)/real(nnod).gt.percent)then
           if(i.gt.1)then
              antn=i-1 ; postn=i ; exit        
            else
              antn=1   ; postn=2 ; exit                 
            end if   
         end if
         if(i.eq.nnod-1)then
            antn=i  ; postn=nnod ; exit         
         end if    
       end do

       if((antn.ne.0).and.(postn.ne.0))then ! esto ha de ir despues de revisar las celulas hijas???
          a=0.5d0*(node(cels(celd)%node(ntot(antn)))%x+node(cels(celd)%node(ntot(postn)))%x)
          b=0.5d0*(node(cels(celd)%node(ntot(antn)))%y+node(cels(celd)%node(ntot(postn)))%y)
          c=0.5d0*(node(cels(celd)%node(ntot(antn)))%z+node(cels(celd)%node(ntot(postn)))%z)
       end if
return
  if(antn.lt.nnod-antn)then
    call island(antn,cels(celd)%node(ntot(1:antn)),disc)          
    if(disc.eq.1)then
       jjj=1
       do while (disc.eq.1)
           call island(antn+jjj,cels(celd)%node(ntot(1:antn+jjj)),disc);jjj=jjj+1          
           !write(*,*)'ant_mod',antn+jjj,postn+jjj ; jjj=jjj+1
       end do
       a=0.5d0*(node(cels(celd)%node(ntot(antn+jjj-1)))%x+node(cels(celd)%node(ntot(postn+jjj-1)))%x)
       b=0.5d0*(node(cels(celd)%node(ntot(antn+jjj-1)))%y+node(cels(celd)%node(ntot(postn+jjj-1)))%y)
       c=0.5d0*(node(cels(celd)%node(ntot(antn+jjj-1)))%z+node(cels(celd)%node(ntot(postn+jjj-1)))%z)
    end if
else if(antn.gt.nnod-antn)then
    call island(nnod-antn,cels(celd)%node(ntot(postn:nnod)),disc) 
    if(disc.eq.1)then
       jjj=1
       do while (disc.eq.1)
           call island(nnod-antn+jjj,cels(celd)%node(ntot(postn-jjj:nnod)),disc);jjj=jjj+1   
           !write(*,*)'post_mod',nnod-antn+jjj,postn-jjj ; jjj=jjj+1                   
       end do 
       a=0.5d0*(node(cels(celd)%node(ntot(antn-jjj+1)))%x+node(cels(celd)%node(ntot(postn-jjj+1)))%x)
       b=0.5d0*(node(cels(celd)%node(ntot(antn-jjj+1)))%y+node(cels(celd)%node(ntot(postn-jjj+1)))%y)
       c=0.5d0*(node(cels(celd)%node(ntot(antn-jjj+1)))%z+node(cels(celd)%node(ntot(postn-jjj+1)))%z)
    end if
end if

end subroutine

!*******************************************************************************
subroutine assymetric(celd,a,b,c,bx,by,bz) !!!!!!!!!!!!!!!!!!!!!!!!! miguel 30-5-13    ! assymetric division !!!!!!!!!!!!!!!!! 
! input: cell index(celd),old centroid(a,b,c),polarization vector(bx,by,bz) ; output: new centroid (a,b,c)
integer::celd,nnod,antn,postn,disc
real*8::a,b,c,d,e,bx,by,bz,ix,iy,iz,alfa,proj,asim                ! miguel 30-5-13          
integer,dimension(cels(celd)%nunodes) :: ntot                     ! ordered nodes along the polarization vector    
real*8,dimension(cels(celd)%nunodes)  :: ctot,ctotaux             ! vectors of node distances alon polarization vector     

        a=cels(celd)%cex ; b=cels(celd)%cey ; c=cels(celd)%cez    !>>>Miguel30-6-14
        nnod=cels(celd)%nunodes    

	do i=1,nnod                                               !to find the maximum node distances to the plane
	     j=cels(celd)%node(i)
	     ix=node(j)%x-cels(celd)%cex;iy=node(j)%y-cels(celd)%cey;iz=node(j)%z-cels(celd)%cez  !vector from centroid to node 3D     
	     proj=bx*ix+by*iy+bz*iz                               !scalar product  
	     asim=sqrt(ix**2+iy**2+iz**2)                         !module of radial vector
	     alfa=acos(proj/(sqrt(bx**2+by**2+bz**2)*asim))       !angle with polarization vector	       
	     asim=cos(alfa)*asim                                  !module of projected vector	   
	     ctot(i)=asim                                         !it locates the "extreme nodes"	     
	end do	

        ctotaux=maxval(ctot)+1d0                                  ! to order the nodes along the polarization vector
        do i=1,nnod ; ntot(i)=i ; end do                          ! order by default
        do i=1,nnod                                               ! nodes ordered along polarization vector
        x=ctot(i) 
          do j=1,nnod
             if(x.lt.ctotaux(j))then
                do k=nnod,j+1,-1           
                ctotaux(k)=ctotaux(k-1) ; ntot(k)=ntot(k-1)          
                end do
                ctotaux(j)=x ;ntot(j)=i 
             exit;endif     
          end do 
       end do

       ctot=0d0 ! total concentration in each node of genes affecting assymetric cell division (I put it in ctot)
       do k=1,npag(nparam_per_node+12)
         kk=7!whonpag(nparam_per_node+12,k)!modifico
         do i=1,nnod
           j=cels(celd)%node(ntot(i))
           if (gex(j,kk)>0.0d0) then
             ctot(i)=ctot(i)+gex(j,kk)!gen(kk)%wa(nparam_per_node+12)  !wa in units of probability more or less !!!modifico
            ! write(*,*)k,kk,i,j,'ctots',ctot(i),gex(j,kk),gen(kk)%wa(nparam_per_node+12)     !modifico
           end if
         end do
       end do

       if(sum(ctot)==0d0) return !if there is no gene expression for assymetric division
       antn=0 ; postn=0 ; disc=0

       do i=1,nnod-1                                             ! it search when the concentrations are equal ...       
       x=sum(ctot(1:i)) ; y=sum(ctot(i+1:nnod))                  ! overall concentration in the two compartiments ...   
       if(x.gt.y)then                                            ! the concentration of gene products is the same (or zero)
          if(i.gt.1)then
            antn=i-1 ; postn=i ; exit        
          else
            antn=1   ; postn=2 ; exit                 
          end if       
       end if   
       if(i.eq.nnod-1)then
          antn=i  ; postn=nnod ; exit         
       end if    
       end do

       if((antn.ne.0).and.(postn.ne.0))then ! esto ha de ir despues de revisar las celulas hijas???
          a=0.5d0*(node(cels(celd)%node(ntot(antn)))%x+node(cels(celd)%node(ntot(postn)))%x)
          b=0.5d0*(node(cels(celd)%node(ntot(antn)))%y+node(cels(celd)%node(ntot(postn)))%y)
          c=0.5d0*(node(cels(celd)%node(ntot(antn)))%z+node(cels(celd)%node(ntot(postn)))%z)
       end if
       !write(*,*)celd,cels(celd)%nunodes,'anterior-posterior',antn,postn

if(antn.lt.nnod-antn)then
    call island(antn,cels(celd)%node(ntot(1:antn)),disc)          
    if(disc.eq.1)then
       jjj=1
       do while (disc.eq.1)
           call island(antn+jjj,cels(celd)%node(ntot(1:antn+jjj)),disc);jjj=jjj+1          
           !write(*,*)'ant_mod',antn+jjj,postn+jjj ; jjj=jjj+1
       end do
       a=0.5d0*(node(cels(celd)%node(ntot(antn+jjj-1)))%x+node(cels(celd)%node(ntot(postn+jjj-1)))%x)
       b=0.5d0*(node(cels(celd)%node(ntot(antn+jjj-1)))%y+node(cels(celd)%node(ntot(postn+jjj-1)))%y)
       c=0.5d0*(node(cels(celd)%node(ntot(antn+jjj-1)))%z+node(cels(celd)%node(ntot(postn+jjj-1)))%z)
    end if
else if(antn.gt.nnod-antn)then
    call island(nnod-antn,cels(celd)%node(ntot(postn:nnod)),disc) 
    if(disc.eq.1)then
       jjj=1
       do while (disc.eq.1)
           call island(nnod-antn+jjj,cels(celd)%node(ntot(postn-jjj:nnod)),disc);jjj=jjj+1   
           !write(*,*)'post_mod',nnod-antn+jjj,postn-jjj ; jjj=jjj+1                   
       end do 

       a=0.5d0*(node(cels(celd)%node(ntot(antn-jjj+1)))%x+node(cels(celd)%node(ntot(postn-jjj+1)))%x)
       b=0.5d0*(node(cels(celd)%node(ntot(antn-jjj+1)))%y+node(cels(celd)%node(ntot(postn-jjj+1)))%y)
       c=0.5d0*(node(cels(celd)%node(ntot(antn-jjj+1)))%z+node(cels(celd)%node(ntot(postn-jjj+1)))%z)
    end if
end if

end subroutine

!***********************************************************************************

subroutine island(kkk,nodek,kk) ! it checks wether the "nodek" list of "kkk" nodes is a continuous graph (kk=1) or not (kk=0)
integer           :: kkk,kk
integer           :: nodek(kkk),noddm(kkk),nodd(kkk,kkk) 

nodd=0 ; noddm=0 ; kk=0
    do i=1,kkk        ! it searchs for intracellular interactions
       j=nodek(i)
       do ii=1,kkk
          jj=nodek(ii)
          if(j.ne.jj)then
             a=sqrt((node(j)%x-node(jj)%x)**2+(node(j)%y-node(jj)%y)**2+(node(j)%z-node(jj)%z)**2) 
             if(a.lt.(node(j)%da+node(jj)%da))then
                nodd(i,ii)=1
             end if                         
          end if
       end do
    end do    
   noddm(:)=nodd(1,:)  ! binary matrix (1 if there is interaction) 
   do k=1,kkk          ! path searching between interacting nodes ... 
   do i=1,kkk
     if(noddm(i).ne.0)then
       do j=1,kkk
         if(nodd(i,j).ne.0)then
         noddm(j)=1
         end if
       end do
     end if
   end do
   end do
 
   if(sum(noddm).lt.kkk)then ;kk=1;endif  ! the graph is discontinuous....
end subroutine

end module mitosis
