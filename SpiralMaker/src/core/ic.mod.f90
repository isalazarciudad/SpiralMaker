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


! Is 20-5-13
module ic	!the mesenchimal and epithelial i.c. subroutines are now unified. 
use general
use genetic

integer  :: nodecela
integer  :: l, ciclo, cont
real*8   :: x, xx, y, yy, z, zz, de, di, hip              
real*8   :: dx1, dx2, dx3, dy1, dy2, dy3
integer, public  :: radi,radicel,layer,mradi,mradicel,xradi,xlayer
real*8, public   ::  zepi,zmes,radius,zx
integer, public  :: packed    !>>>Miquel10-1-14
integer          :: iccag
integer,public,allocatable :: mesradicel(:)

contains

!*************************************************************************************

subroutine default_values  !IS 2-1-14

    ndmax=100000
    ecmmax=0.25d0
    realtime=0
    ttalone=10
    mnn=2500!1250!500
    dif_req=1.0d0
    min_comp=-1.0d-1
    screen_radius=1.0d0
    reqmin=0.05d0
    df_reqmax=0.50d0
    deltamin=1.0d-3
    prec=0.1  ! Is 26-8-14
    angletor=0.00 !Miquel15-9-14
    ldi=epsilod

end subroutine

!*************************************************************************************
!*************************************************************************

subroutine mesenchyme            !>>>Miguel 28-1-15 subroutine

!******* #1 DEFINING SPATIAL DIMENSIONS *******

	!epithelium's dimension parameters
	radi=0       !number of radial layers of nodes per cell
	radicel=0    !number of radial layers of cells
	zepi=0d0     !z-position of basal layer

	!mesenchyme's dimension parameters
	mradi=2500!1250!1331!729!512!343!216     !number of nodes per cell !modifico
	mradicel=1     !number of radial cell layers
	layer=1        !number of planar cell layers
	zmes=0d0       !z-position of uppermost layer

    !ECM dimension parameters?

!************************************************


    !Initializing dimension parameters
    if(radi>0.and.radicel>0)then
      j=0
      do i=1,radi-1
        j=j+i
      end do
      nodecel=(6*j+1)*2	!number of nodes per cell
      nodecela=2*nodecel+1	!cels()%node array's initial size the same for mesenchyme!>>>>>>>Miquel 21-3-13

      j=0
      do i=1,radicel-1
        j=j+i
      end do
      ncelsepi=(6*j+1)
      ndepi=nodecel*ncelsepi
    else
      ncelsepi=0
      ndepi=0
      nodecel=0
      nodecela=0
    end if

    if(mradi>0.and.mradicel>0.and.layer>0)then
      j=0
      do i=1,mradicel-1
        j=j+i
      end do
      ncelsmes=(6*j+1)*layer  !number of mesenchymal cells
      ndmes=mradi*ncelsmes    !number of mesenchymal nodes 
      if(radi==0.and.radicel==0)then
        nodecel=mradi
        nodecela=2*nodecel+1
      else if(nodecel<mradi)then
        nodecel=radi
        nodecela=nodecel*2+1
      end if
    else
      ndmes=0
      ncelsmes=0
    end if
	nd=ndepi+ndmes
	ncels=ncelsepi+ncelsmes
        nda=nd+10
	ncals=ncels+32
    
    if(radi==1.or.mradi==1) ffu(1)=1
  !End initializing dimension parameters

  print*,"nodecel",nodecel,"ncelsepi",ncelsepi,"ncelsmes",ncelsmes,"ndepi",ndepi,"ndmes",ndmes


    !Allocatations
    if (allocated(node)) deallocate(node)
    if (allocated(cels)) deallocate(cels)
    allocate(node(nda),cels(ncals)) 
    call iniarrays
    !End Allocatations


   !******* #2 DEFINING MODEL PARAMETERS *******

    !implementation and initializations
    getot=0
    itacc=0
    nparti=1000
    idum=-11111
    idumoriginal=idum
    realtime=0

    !nparam=32       !number of params
    nvarglobal_out=5

    !physical

    temp=1d-2 !low value is low temperature	
    desmax=1d-3!1d-4!0
    resmax=1d-2!1d-4   
    prop_noise=1d-1!2!0
    reqmin=0.05d0 
    deltamax=1d-3!2
    deltamin=5d-4
    dmax=1
    screen_radius=0.5d0!1.0d0

    !biological
    !functions used    
    
    ffu=0
 
    !spring of the ellipse
 
    ffu(2)=0  !to quite if there are too many cells
    ffu(3)=0  !screening
    ffu(4)=0  !torsion
    ffu(5)=4  !0 !external signal source     
    ffu(12)=1 !delta fija    
    ffu(22)=1 !0 = noise biased by energies , 1 = unbiased noise
    ffu(23)=0
    ffu(24)=25 ! blastomere rotation (number of iterations it is applied)
    ffu(25)=5 ! sach's rule (0=no sach rule ; 1=sach rule) !modifico
    ffu(26)=0 ! hertwig : 0=0(no hertwig) ; 5=0.5(mitad)      ; 10=1(solo hertwig)
    ffu(27)=10  ! peso del gen (2) 0=0(no Isaac) ; 5=0.5(mitad) ; 10=1(solo Isaac) !(Isaac's rule)
    ffu(28)=0  ! pero del gen (4) 0=0(no Free)  ; 5=0.5(mitad) ; 10=1(solo Free) !(Free-cytoplasm rule)
    ffu(29)=4   !asimetria (%)

   !counterflowk=ffu(24)
   !******* #2 DEFINING NODE MECHANIC PARAMETERS *******
	!Mesenchyme
    if(mradi>0.and.mradicel>0.and.layer>0)then
      do i=ndepi+1,nd
        node(i)%you=3.1d1
        node(i)%adh=1.4d1!1.25d1!1.1d1 	!>>Miquel 26-10-12
        node(i)%rep=5d2;node(i)%repcel=5d2	!>>Miquel 8-10-12!  
        node(i)%req=0.25d0 ; node(i)%reqcr=node(i)%req !;node(i)%reqcel=0.25d0	!>>Miquel 26-10-12!de
        node(i)%reqs=0.5d0
        node(i)%da=node(i)%req*1.63d0!1.35d0
        node(i)%ke=0d0   !only for epithelium
        node(i)%tor=1d0  !only for epithelium
        node(i)%altre=0  !only for epithelium
        node(i)%mo=temp
        node(i)%dmo=desmax        
      end do
    end if

    !General parameters that depend on node parameters
    rv=2*maxval(node(:)%da)

    !Distribution of nodes in space
    if(mradi>0.and.mradicel>0.and.layer>0) call mesenq(mradi,mradicel,layer,zmes,node(ndepi+1)%req)
    allocate(flowplot(nd,3))

    do i=1,nd    
      node(i)%orix=node(i)%x ; node(i)%oriy=node(i)%y ; node(i)%oriz=node(i)%z         
    end do
    !End distribution of nodes in space

    !Setting boundary nodes (they will be subject to an external force keeping them in their initial position)
    node(:)%hold=0

   !******* #3 DEFINING CELL PARAMETERS *******
    !Epithelial
    if(radi>0.and.radicel>0)then
      do i=1,ncelsepi
        cels(i)%fase=0d0
        mmae=node(cels(i)%node(1))%req
        cels(i)%minsize_for_div=cels(i)%nunodes*2
        cels(i)%maxsize_for_div=10000 !>>> Is 5-2-14
      end do
    end if
    !Mesenchymal
    if(mradi>0.and.mradicel>0.and.layer>0)then
      do i=ncelsepi+1,ncels
        cels(i)%fase=0d0
        mmae=node(cels(i)%node(1))%req
        cels(i)%minsize_for_div=cels(i)%nunodes!*2
        cels(i)%maxsize_for_div=10000 !>>> Is 5-2-14
      end do
    end if

!******* #4 DEFINING GENETIC PARAMETERS *******

    !Number of genes

    ng=8 ; cels(1)%sister=1 ;node(:)%exte=0
    call initiate_gene
    !Gene parameters
      gen(1)%diffu=0.5d0 ; gen(1)%kindof=0 ; gen(1)%mu=0.1d0 ! miguel 14-10-13
      gen(2)%diffu=0.5d0 ; gen(2)%kindof=0 ; gen(2)%mu=0.1d0 ! miguel 14-10-13
      gen(3)%diffu=0.5d0 ; gen(3)%kindof=0 ; gen(3)%mu=0.1d0 ! miguel 14-10-13
      gen(4)%diffu=0.5d0 ; gen(4)%kindof=0 ; gen(4)%mu=0.1d0 ! miguel 14-10-13
      gen(6)%diffu=0.5d0 ; gen(6)%kindof=0 ; gen(6)%mu=0.1d0 ! miguel 14-10-13
      gen(7)%diffu=0.5d0 ; gen(7)%kindof=0 ; gen(7)%mu=0.1d0 ! miguel 14-10-13
      gen(5)%diffu=1d-7 ; gen(5)%kindof=0 ; gen(5)%mu=0.1d-5 ! miguel 14-10-13
      gen(8)%diffu=0.5d0 ; gen(8)%kindof=0 ; gen(8)%mu=0.1d0 ! miguel 14-10-13
    !Gene-behavior interactions
       gen(1)%wa(nparam_per_node+1)=0d0!0 growth
       gen(1)%wa(nparam_per_node+2)=2d-3!2d-3!1 !division
       gen(2)%wa(nparam_per_node+8)=1d0!1 !polarizacion
       gen(7)%wa(nparam_per_node+12)=1d0 !div asim (que queden = ...)
       gex(:,1)=1d0
       gex(:,3)=0d0 ! polo animal
       gex(:,4)=0d0 ! contacto blastomeros "hermanos"  
       gex(:,5)=0d0 !solo para visualizar (sea linaje 4d) 
       gex(:,8)=0d0 !solo para visualizar (sea linaje 4d) 
      do i=1,nd
       gex(i,2)=((node(i)%z+minval(node(:)%z))**2) /sqrt(node(i)%x**2+node(i)%y**2) 
      end do
    !Gene-behavior interactions

	ntipusadh=2
    if(ntipusadh>0)then
      if (allocated(kadh)) deallocate(kadh)
      allocate(kadh(ntipusadh,ntipusadh))
      kadh=0d0
      !Adhesion molecules interaction
    end if

    gex(:,6)=0d0 ; gex(:,7)=0d0            ! type 1 cells express gene 1   

    !Gene expression on nodes
    call update_npag
    node(:)%talone=0.0d0
    ramax=maxval(node(:)%da)*3
    node(:)%diffe=0.0d0
    mnn=2500!1250!1250!1000

end subroutine mesenchyme

!**********************************************************************************************************

subroutine mesenq(radi,radicel,layer,zmes,dreq)									!!! miguel 20.11
integer  ::i,j,k,ii,jj,radi,radicel,layer,signo,ent,ont       ! number of layers and concentric hexagons 
real*8   ::rad,der,zmes,de,di,dreq
real*8   :: xx,yy,zz                                                        ! miguel 4-6-13
rad=pi/180d0
de=3d0!dreq*2                    ! call radius
di=2*de+2*de*cos(60d0*rad)

	do i=ncelsepi+1,ncels               
		cels(i)%nunodes=radi				!>>>>>>>>>>>>>>Miquel 21-3-13
		cels(i)%nodela=nodecela
		allocate(cels(i)%node(nodecela))		!>>>>>>>>>>>>>>Miquel 21-3-13
		cels(i)%node=0
        end do

        node(ndepi+1:)%marge=1
	kkk=ndmes/layer
	cont=ncelsepi+1
	ii=ndepi		!node counter
	jj=ncelsepi		!cel counter

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! central nodes (=nucleus) !!!!

   do l=1,layer  
	ii=ii+1
	jj=jj+1
	node(ii)%x=0d0 ; node(ii)%y=0d0 ! Euler packing algorhytm
	node(ii)%z=zmes-di*(l-1)	!zmes marks the z-position of the uppermost layer
        node(ii)%tipus=3 ; node(ii)%icel=jj                        ! origin	
	cels(jj)%node(1)=ii
	ent=ii
	ont=ii
	!fill with the external nodes
	if(radi>1)then
        signo=1                                                              ! miguel 4-6-13
		do k=2,radi
			ii=ii+1
             call random_number(a)
			der=de!(sqrt(de)*sqrt(de*a))                               ! miguel 4-6-13
121         call random_number(a)
            xx=der*(1d0-2*a) ; 
            call random_number(a)
            yy=der*(1d0-2*a)            ! miguel 4-6-13 
            zz=(der**2)-(xx**2)-(yy**2) 									 ! miguel 4-6-13
            if(zz.lt.0)then;goto 121;endif                                   ! miguel 4-6-13
            zz=signo*sqrt(zz) ; signo=-1*signo                               ! miguel 4-6-13  
            if(mod(k-1,3).eq.0)then      ; b=xx ; c=yy ; a=zz                ! miguel 4-6-13
            else if(mod(k-1,3).eq.1)then ; a=xx ; c=yy ; b=zz                ! miguel 4-6-13
  	        else if(mod(k-1,3).eq.2)then ; a=xx ; b=yy ; c=zz ; end if       ! miguel 4-6-13          
			node(ii)%x=node(ent)%x+a
			node(ii)%y=node(ent)%y+b
			node(ii)%z=node(ent)%z+c
		    node(ii)%tipus=3 ; node(ii)%icel=jj
			cels(jj)%node(k)=ii
		end do
	end if

	do j=2,radicel                                                        ! "cicle"
		dx1=0.0 ; dx2=0.0 ; dx3=0.0 ; dy1=0.0 ; dy2=0.0 ; dy3=0.0 
		do i=1,6                                                        ! "sector" of the hexagon
			if(i.eq.1)then   
				dx2=0!*cos(real(i)*60d0*rad) 
				dy2=di*(real(j)-1d0)!*sin(real(i)*60d0*rad);print*,"dx2",dx2,"dy2",dy2
				dx1=di*(real(j)-1d0)*sin(-60d0*rad) ; dy1=di*(real(j)-1d0)*cos(-60d0*rad)!;print*,"dx1",dx1
			else
				hip=di*(real(j)-1d0)                                    ! hipotenusa
				dx1=dx2 ; dy1=dy2
				dx2=hip*sin(real(i-1)*60d0*rad) ; dy2=hip*cos(real(i-1)*60d0*rad)          
			end if            
			ii=ii+1
			jj=jj+1
			node(ii)%x=node(ont)%x+dx2 ; node(ii)%y=node(ont)%y+dy2 ; node(ii)%z=node(ont)%z
			node(ii)%icel=jj ; node(ii)%tipus=3
			cels(jj)%node(1)=ii
			ent=ii
			if(radi>1)then
                signo=1                                                                  ! miguel 4-6-13
				do k=2,radi
					ii=ii+1
                            call random_number(a)
                            der=(sqrt(de)*sqrt(de*a))                               ! miguel 4-6-13
122                         call random_number(a)
                            xx=der*(1d0-2*a) ; 
                            call random_number(a)
                            yy=der*(1d0-2*a)            ! miguel 4-6-13 
                            zz=(der**2)-(xx**2)-(yy**2) 									 ! miguel 4-6-13
                            if(zz.lt.0)then;goto 122;endif                                   ! miguel 4-6-13
                            zz=signo*sqrt(zz) ; signo=-1*signo                               ! miguel 4-6-13  
                            if(mod(k-1,3).eq.0)then      ; b=xx ; c=yy ; a=zz               ! miguel 4-6-13
                            else if(mod(k-1,3).eq.1)then ; a=xx ; c=yy ; b=zz               ! miguel 4-6-13
  	                        else if(mod(k-1,3).eq.2)then ; a=xx ; b=yy ; c=zz ; end if      ! miguel 4-6-13
					node(ii)%x=node(ent)%x+a
					node(ii)%y=node(ent)%y+b
					node(ii)%z=node(ent)%z+c
				        node(ii)%tipus=3 ; node(ii)%icel=jj
					cels(jj)%node(k)=ii
				end do
			end if

			if(j.gt.2)then                                              ! intermediate points
				dx3=dx2-dx1       ; dy3=dy2-dy1                         ! vectors which link "extreme" points
				dx3=dx3/real(j-1) ; dy3=dy3/real(j-1)                   ! sub-vectors                    
				do k=1,j-2                               
					ii=ii+1
					jj=jj+1
					node(ii)%x=node(ont)%x+(dx1+(real(k)*dx3)) 
					node(ii)%y=node(ont)%y+(dy1+(real(k)*dy3))    
					node(ii)%z=node(ont)%z                                                
					node(ii)%icel=jj ; node(ii)%tipus=3 ; cels(jj)%node(1)=ii
					ent=ii
					if(radi>1)then
                        signo=1											            ! miguel 4-6-13
						do kk=2,radi
							ii=ii+1
!							print*,"cel perif, node extern. ent:",ent,ii
                            call random_number(a)
							der=(sqrt(de)*sqrt(de*a))                               ! miguel 4-6-13
123                         call random_number(a) 
                            xx=der*(1d0-2*a) ; 
                            call random_number(a)
                            yy=der*(1d0-2*a)            ! miguel 4-6-13 
                            zz=(der**2)-(xx**2)-(yy**2) 									 ! miguel 4-6-13
                            if(zz.lt.0)then;goto 123;endif                                   ! miguel 4-6-13
                            zz=signo*sqrt(zz) ; signo=-1*signo                               ! miguel 4-6-13  
                            if(mod(ii,3).eq.0)then      ; b=xx ; c=yy ; a=zz               ! miguel 4-6-13
                            else if(mod(ii,3).eq.1)then ; a=xx ; c=yy ; b=zz               ! miguel 4-6-13
  	                        else if(mod(ii,3).eq.2)then ; a=xx ; b=yy ; c=zz ; end if      ! miguel 4-6-13                        
							node(ii)%x=node(ent)%x+a
							node(ii)%y=node(ent)%y+b
							node(ii)%z=node(ent)%z+c
						        node(ii)%tipus=3 ; node(ii)%icel=jj
							cels(jj)%node(kk)=ii
						end do
					end if
				end do
			end if
		end do
	end do
  end do

	!define the cell's centroid and nucleus (margin)			!>>>>>>>>>>>>>> Miquel 14-4-13

	do i=ncelsepi+1,ncels											!>>>>>>>>>>>>>> Miquel 14-4-13
		cels(i)%ctipus=3									!>>>>>>>>>>>>>> Miquel 14-4-13
		cels(i)%cex=0;cels(i)%cey=0;cels(i)%cez=0			!>>>>>>>>>>>>>> Miquel 14-4-13
		do j=1,cels(i)%nunodes								!>>>>>>>>>>>>>> Miquel 14-4-13
			k=cels(i)%node(j)								!>>>>>>>>>>>>>> Miquel 14-4-13
			cels(i)%cex=cels(i)%cex+node(k)%x				!>>>>>>>>>>>>>> Miquel 14-4-13
			cels(i)%cey=cels(i)%cey+node(k)%y				!>>>>>>>>>>>>>> Miquel 14-4-13
			cels(i)%cez=cels(i)%cez+node(k)%z				!>>>>>>>>>>>>>> Miquel 14-4-13
		end do												!>>>>>>>>>>>>>> Miquel 14-4-13
		cels(i)%cex=cels(i)%cex/real(cels(i)%nunodes)		!>>>>>>>>>>>>>> Miquel 14-4-13
		cels(i)%cey=cels(i)%cey/real(cels(i)%nunodes)		!>>>>>>>>>>>>>> Miquel 14-4-13
		cels(i)%cez=cels(i)%cez/real(cels(i)%nunodes)		!>>>>>>>>>>>>>> Miquel 14-4-13
	end do													!>>>>>>>>>>>>>> Miquel 14-4-13

	do i=ncelsepi+1,ncels
          b=1.0d8											!>>>>>>>>>>>>>> Is 14-9-13
   	  do j=1,cels(i)%nunodes								!>>>>>>>>>>>>>> Is 14-9-13
            k=cels(i)%node(j)								!>>>>>>>>>>>>>> Is 14-9-13
   	    a=sqrt((cels(i)%cex-node(k)%x)**2+(cels(i)%cey-node(k)%y)**2+(cels(i)%cez-node(k)%z)**2)
            if (b>a) then ; b=a ; ii=k ; jj=j ; end if
	  end do												!>>>>>>>>>>>>>> Is 14-9-13
          node(ii)%marge=0

          !now we want the nucleus to be the first node in cels%node: this allows diffusion to be faster  
          jjj=cels(i)%node(1)
          cels(i)%node(1)=ii
          cels(i)%node(jj)=jjj
	end do													!>>>>>>>>>>>>>> Is 14-9-13

    node(:)%talone=0.0d0

end subroutine mesenq

!***********************************************************************

subroutine escribe
 open(666,file="nodoscels.dat",action='write')
    do i=1,size(node)
    write(666,*)'node',i,'tipus',node(i)%tipus,'cel',node(i)%icel,'xyz',node(i)%x,node(i)%y,node(i)%z
    end do
    do i=1,size(cels)
    write(666,*)'cel',i,'nodes',cels(i)%node(:)
    end do
  close(666)
end subroutine escribe

!***********************************************************************
end module

