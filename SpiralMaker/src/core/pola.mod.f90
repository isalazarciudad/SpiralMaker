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




module pola  !>>>>>>>MIQUEL MADE MODULE 3-4-13
use general
use neighboring
use io
use aleas
use genetic


contains

subroutine pol_physic(celd,vx,vy,vz)		!this subroutine applies a physical criterion to determine the cell polarisation for division to take place
											!(namely it will polarize in the cell's longer axis)
											!a linear regression is calculated using all the cell's nodes and the resulting vector will be normal to the plane of division
integer,intent(in)::celd
integer::num,tipi
real*8::unum
integer::i,j,k,ii,jj,kk
real*8 ::d
real*8::a,b,c,xm,ym,zm,sxx,sxy,syy,sxz,szz,syz,thet,cost,sint
real*8::k11,k22,k12,k10,k01,k00
real*8::c2,c1,c0
real*8::dm,rho,phi,p,q,r
real*8::ka,kb,ku,kv,kw
real*8,intent(out)::vx,vy,vz
real*8,allocatable::px(:),py(:),pz(:)


	tipi=node(cels(celd)%node(1))%tipus

	if(tipi<3)then	!if epithelial, we only calculate the regression for the top layer (we ignore cell depth,because it causes artifacts if we take all the epithelial nodes)
		num=cels(celd)%nunodes
		allocate(px(num),py(num),pz(num))
		ii=0
		do i=1,num				!we load the cell's nodes
			j=cels(celd)%node(i)			
		end do
		num=ii
	else
		num=cels(celd)%nunodes
		unum=1d0/num
		allocate(px(num),py(num),pz(num))
!		print*,"num",num
		do i=1,num				!we load the cell's nodes
			j=cels(celd)%node(i)
			px(i)=node(j)%x
			py(i)=node(j)%y
			pz(i)=node(j)%z
!			print*,"px",px(i),"py",py(i),"pz",pz(i)
		end do
	end if

	unum=1d0/num
!	print*,"num",num

	xm=0;ym=0;zm=0			!averages
	do i=1,num
		xm=xm+px(i)
		ym=ym+py(i)
		zm=zm+pz(i)
	end do

	xm=xm*unum
	ym=ym*unum
	zm=zm*unum

	sxx=0;sxy=0;syy=0;sxz=0;szz=0;syz=0		!covariances
	do i=1,num
		sxx=sxx+px(i)**2
		sxy=sxy+px(i)*py(i)
		syy=syy+py(i)**2
		sxz=sxz+px(i)*pz(i)
		szz=szz+pz(i)**2
		syz=syz+py(i)*pz(i)
	end do
	sxx=sxx*unum-(xm**2)
	sxy=sxy*unum-xm*ym
	syy=syy*unum-(ym**2)
	sxz=sxz*unum-xm*zm
	szz=szz*unum-(zm**2)
	syz=syz*unum-ym*zm
	if(sxx<=epsilod) sxx=1d-5       !if one of the covariances are 0 (which means the surface is perfectly flat on one coordinate plane
    if(sxy<=epsilod) sxy=1d-5       !the algorithm crashes, so we do the trick of assigning a very small value
    if(syy<=epsilod) syy=1d-5
    if(sxz<=epsilod) sxz=1d-5
    if(syz<=epsilod) syz=1d-5
    if(szz<=epsilod) szz=1d-5

	!from here on it's fucking black magic!
	thet=0.5d0*atan(2*sxy/(sxx-Syy))

	cost=cos(thet)
	sint=sin(thet)

!	print*,"thet",thet,"cost",cost,"sint",sint

	k11=(syy+szz)*cost**2+(sxx+szz)*sint**2-2*sxy*cost*sint
	k22=(syy+szz)*sint**2+(sxx+szz)*cost**2+2*sxy*cost*sint
	k12=-sxy*(cost**2-sint**2)+(sxx-syy)*cost*sint
	k10=sxz*cost+syz*sint
	k01=-sxz*sint+syz*cost
	k00=sxx+syy
	c2=-k00-k11-k22
	c1=k00*k11+k00*k22+k11*k22-k01**2-k10**2
	c0=k01**2*k11+k10**2*k22-k00*k11*k22
	p=c1-(c2**2)/3d0
	q=2*(c2**3)/27d0-(c1*c2)/3d0+c0
	r=(q**2)/4d0+(p**3)/27d0


!	print*,"p",p,"q",q,"r",r

	if(r>0)then
		a=-c2/3d0
		b=(-0.5d0*q+sqrt(r))
		c=(-0.5d0*q-sqrt(r))
!		print*,"A",a,"B",b,"C",c
		if(b<0)then
			b=-(-b)**(1d0/3d0)
		end if
		if(c<0)then
			c=-(-c)**(1d0/3d0)
		end if
!		print*,"A",a,"B",b,"C",c
		dm=a+b+c
	else
		rho=sqrt(-p**3/27d0)
		phi=acos(-q/(2*rho))
		a=-c2/3d0+2*(rho**(1d0/3d0))*cos(phi/3d0)
		b=-c2/3d0+2*(rho**(1d0/3d0))*cos((phi+2*pi)/3d0)
		c=-c2/3d0+2*(rho**(1d0/3d0))*cos((phi+4*pi)/3d0)
		dm=min(a,b,c)
	end if
	ka=-k10*cost/(k11-dm)+k01*sint/(k22-dm)
	kb=-k10*sint/(k11-dm)-k01*cost/(k22-dm)

!	print*,"ka",ka,"kb",kb


	ku=((1+kb**2)*xm-ka*kb*ym+ka*zm)/(1+ka**2+kb**2)
	kv=(-ka*kb*xm+(1+ka**2)*ym+kb*zm)/(1+ka**2+kb**2)
	kw=(ka*xm+kb*ym+(ka**2+kb**2)*zm)/(1+ka**2+kb**2)


!	print*,"vectortip",ku,kv,kw
!	print*,"centerpoint",xm,ym,zm

	vx=xm-ku;vy=ym-kv;vz=zm-kw

        d=1d0/sqrt(vx**2+vy**2+vz**2)  ! >>> Is 21-6-14 

        vx=vx*d ; vy=vy*d ; vz=vz*d    ! >>> Is 21-6-14 

end subroutine pol_physic

!!!!!!!!!
!********************************************************************

subroutine polarigen(kceld,kgen)
integer:: kceld,nnod,ggr,ccen,kgen
real*8::a,b,c,d,e,ax,ay,az,bx,by,bz,cx,cy,cz,iwx,iwy,iwz,alfa,s

        nnod=cels(kceld)%nunodes               
        iwy=1d10 ; cx=0d0 ; cy=0d0 ; cz=0d0
	a=cels(kceld)%cex ; b=cels(kceld)%cey ; c=cels(kceld)%cez   

        do i=1,nnod                                                     ! (gen) in the centroid (in the closest node)
          j=cels(kceld)%node(i)        
            d=sqrt((node(j)%x-a)**2+(node(j)%y-b)**2+(node(j)%z-c)**2)      
          if(d.le.iwy)then;iwy=d;ccen=j;endif             
        end do   

        alfa=0.0d0                                                 ! concentration in the central node        
        kk=kgen!whonpag(nparam_per_node+8,k)
        if (gex(ccen,kk)>0.0d0) then
          alfa=alfa+gex(ccen,kk)!*gen(kk)%wa(nparam_per_node+8)   ! wa in units of probability such that it makes things to go from 0 to 1
        end if
       
        iwx=0d0 ; iwy=0d0 ; iwz=0d0                                        ! vector of the gradient within a cell
        do i=1,nnod                                                     
            j=cels(kceld)%node(i)                  
              d=sqrt((node(j)%x-a)**2+(node(j)%y-b)**2+(node(j)%z-c)**2)
              if (d<epsilod) cycle
              d=1d0/d                                                   ! module of radial vectors to get unitary vectors     
              s=0.0d0           
                kk=kgen!whonpag(nparam_per_node+8,k)
                if (gex(j,kk)>0.0d0) then
                  s=s+gex(j,kk)!*gen(kk)%wa(nparam_per_node+8)
                end if           
              iwx=iwx+((node(j)%x-a)*d)*(s-alfa)                   ! and ignore shape/siwze effects
              iwy=iwy+((node(j)%y-b)*d)*(s-alfa)
              iwz=iwz+((node(j)%z-c)*d)*(s-alfa)          
        end do

        if((iwx.eq.0).and.(iwy.eq.0).and.(iwz.eq.0))then            ! if the gene has uniform expresion, the vector is random ! >>>Miguel1-7-14
          call random_number(a)                                  ! >>>Miguel1-7-14
          k=int(a*nvaloq)+1                                      ! >>>Miguel1-7-14
          cels(kceld)%polx=particions_esfera(k,1) ; cels(kceld)%poly=particions_esfera(k,2) 
          cels(kceld)%polz=particions_esfera(k,3)    
        else                                                     ! >>>Miguel1-7-14
          a=iwx**2+iwy**2+iwz**2 
          if(a==0)then
            cels(kceld)%polx=0d0 ; cels(kceld)%poly=0d0 ; cels(kceld)%polz=0d0	! unitary resultant vector (gradient polariwzation)
          else
            d=1d0/sqrt(a)
            cels(kceld)%polx=iwx*d ; cels(kceld)%poly=iwy*d ; cels(kceld)%polz=iwz*d	! unitary resultant vector (gradient polariwzation)
          end if
          if((iwx.eq.0d0).and.(iwy.eq.0d0).and.(iwz.eq.0d0))then                     ! miguel27-11-13
            cels(kceld)%polx=0d0 ; cels(kceld)%poly=0d0 ; cels(kceld)%polz=0d0
          endif   ! miguel27-11-13
        endif                                                    ! >>>Miguel1-7-14     
end subroutine

!*******************************************************************

subroutine pol_special(celd,actin)		! the same as befre, but using only nodes with some amount of certain gene(s) !>>>Miguel 28-1-15

integer,intent(in)::celd
integer::num,tipi,actin
real*8::unum
integer::i,j,k,ii,jj,kk
real*8 ::d
real*8::a,b,c,xm,ym,zm,sxx,sxy,syy,sxz,szz,syz,thet,cost,sint
real*8::k11,k22,k12,k10,k01,k00
real*8::c2,c1,c0
real*8::dm,rho,phi,p,q,r
real*8::ka,kb,ku,kv,kw
real*8::vx,vy,vz
real*8,allocatable::px(:),py(:),pz(:)
integer,allocatable :: which(:)
	
        allocate(which(cels(celd)%nunodes)) ; which=0
        num=0
        do i=1,cels(celd)%nunodes
          j=cels(celd)%node(i)
          if(gex(j,actin).gt.1d1)then                   
            num=num+1 ; which(num)=j 
              !if((ncels.gt.1).and.(gex(j,4).lt.1d1))then
              !num=num-1 ; end if
          end if
        end do	
	!write(*,*)'celd',celd,'nunodes',cels(celd)%nunodes,'num',num
	allocate(px(num),py(num),pz(num))
	do i=1,num				!we load the cell's nodes
		j=which(i)
		px(i)=node(j)%x
		py(i)=node(j)%y
		pz(i)=node(j)%z
	end do

	unum=1d0/num
	xm=0;ym=0;zm=0			!averages
	do i=1,num
		xm=xm+px(i) ; ym=ym+py(i) ; zm=zm+pz(i)
	end do

	xm=xm*unum ; ym=ym*unum ; zm=zm*unum
	sxx=0;sxy=0;syy=0;sxz=0;szz=0;syz=0		!covariances
	do i=1,num
		sxx=sxx+px(i)**2          ; 	sxy=sxy+px(i)*py(i)
		syy=syy+py(i)**2          ; 	sxz=sxz+px(i)*pz(i)
		szz=szz+pz(i)**2          ; 	syz=syz+py(i)*pz(i)
	end do
	sxx=sxx*unum-(xm**2)     ;    sxy=sxy*unum-xm*ym
	syy=syy*unum-(ym**2)     ;    sxz=sxz*unum-xm*zm
	szz=szz*unum-(zm**2)     ;    syz=syz*unum-ym*zm
	if(sxx<=epsilod) sxx=1d-5       !if one of the covariances are 0 (which means the surface is perfectly flat on one coordinate plane
        if(sxy<=epsilod) sxy=1d-5       !the algorithm crashes, so we do the trick of assigning a very small value
        if(syy<=epsilod) syy=1d-5
        if(sxz<=epsilod) sxz=1d-5
        if(syz<=epsilod) syz=1d-5
        if(szz<=epsilod) szz=1d-5

	!from here on it's fucking black magic!
	thet=0.5d0*atan(2*sxy/(sxx-Syy))
	cost=cos(thet) ; sint=sin(thet)
	k11=(syy+szz)*cost**2+(sxx+szz)*sint**2-2*sxy*cost*sint
	k22=(syy+szz)*sint**2+(sxx+szz)*cost**2+2*sxy*cost*sint
	k12=-sxy*(cost**2-sint**2)+(sxx-syy)*cost*sint
	k10=sxz*cost+syz*sint ; k01=-sxz*sint+syz*cost ; k00=sxx+syy
	c2=-k00-k11-k22  ;  c1=k00*k11+k00*k22+k11*k22-k01**2-k10**2
	c0=k01**2*k11+k10**2*k22-k00*k11*k22
	p=c1-(c2**2)/3d0 ; q=2*(c2**3)/27d0-(c1*c2)/3d0+c0
	r=(q**2)/4d0+(p**3)/27d0

	if(r>0)then
		a=-c2/3d0 ; b=(-0.5d0*q+sqrt(r)) ; c=(-0.5d0*q-sqrt(r))
		if(b<0)then ; b=-(-b)**(1d0/3d0) ; end if
		if(c<0)then ; c=-(-c)**(1d0/3d0) ; end if
		dm=a+b+c
	else
		rho=sqrt(-p**3/27d0) ; phi=acos(-q/(2*rho))
		a=-c2/3d0+2*(rho**(1d0/3d0))*cos(phi/3d0)
		b=-c2/3d0+2*(rho**(1d0/3d0))*cos((phi+2*pi)/3d0)
		c=-c2/3d0+2*(rho**(1d0/3d0))*cos((phi+4*pi)/3d0)
		dm=min(a,b,c)
	end if

	ka=-k10*cost/(k11-dm)+k01*sint/(k22-dm)
	kb=-k10*sint/(k11-dm)-k01*cost/(k22-dm)
	ku=((1+kb**2)*xm-ka*kb*ym+ka*zm)/(1+ka**2+kb**2)
	kv=(-ka*kb*xm+(1+ka**2)*ym+kb*zm)/(1+ka**2+kb**2)
	kw=(ka*xm+kb*ym+(ka**2+kb**2)*zm)/(1+ka**2+kb**2)

	vx=xm-ku ; vy=ym-kv ; vz=zm-kw
        d=1d0/sqrt(vx**2+vy**2+vz**2)  ! >>> Is 21-6-14 

        cels(celd)%spolx=vx*d ; cels(celd)%spoly=vy*d ; cels(celd)%spolz=vz*d    ! >>> Is 21-6-14 
       
        deallocate(which)

end subroutine pol_special
!!!!!!!!!

end module pola
