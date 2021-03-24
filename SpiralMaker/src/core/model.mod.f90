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
!***************  MODUL ***************************************************
!***************************************************************************

module model
use general
use aleas
use neighboring
use io
use nexus
use biomechanic
use genetic
use energy ! >>> Is 5-6-14

public:: iteracio

integer, public :: passed
integer, public :: getott !>>>Miguel 8-10-14
integer,public  :: lock   !>>>Miguel29-10-14
real*8,  public, allocatable  :: gext1(:,:,:),gext2(:,:,:),ejex1(:),ejex2(:) ! to store gex(:,:) in time lapses ...!>>>Miguel 8-10-14

contains

!**************************************************************************
subroutine eneinicial
integer::i
  node(:)%e=0
  do i=1,nd
    call energia(i)
  end do
end subroutine eneinicial

!**************************************************************************

subroutine prints
  print *,"iterations",getot!,"nodes",cels(1:ncels)%nunodes!,"nd",nd!,node(nd)%x!,"uuu",maxval(gex(:,8)),"max of gene 1",minval(node(:nd)%diffe)
end subroutine prints

!**************************************************************************

subroutine iteracio(tf)
integer tf,itf,il,status
real*8::rtf,realtf  !>>>>>>>>>>>Miquel 17-6-13
integer::cont       !>>>>>>>>>>>Miquel 17-6-13
real*8::ox,oy,oz !miguel4-1-13
    
  realtf=real(tf)   !>>>>>>>>>>>Miquel 17-6-13
  rtf=0d0           !>>>>>>>>>>>Miquel 17-6-13
  cont=0            !>>>>>>>>>>>Miquel 17-6-13
  geu=0
  itf=0

  if(getot.eq.0)then                                        !>>>Miguel29-10-14
    lock=0 ! matrices gext are filled by default            !>>>Miguel29-10-14         
    if((aut.ne.1).and.(aut.ne.5))then;call printgex1;endif  !>>>Miguel 8-10-14
  end if                                                    !>>>Miguel29-10-14

  if (itviactual<itvi) then  
    call go_iteration_forth(tf)  !recovers remembered iterations before runing new ones
  end if     
  do  !*******this loop does the number of iterations equat to tf

    getot=getot+1
    geu=geu+1
    itf=itf+1
    counterflow=counterflow-1 !>>>Miguel 28-1-15 

    if(getot.eq.1501)then
     if(allocated(flowplot))then;deallocate(flowplot);endif;allocate(flowplot(nd,3))
    end if
    flowplot=0d0              !>>>Miguel 28-1-15 

    !UPDATING CELL CENTROIDS
    do i=1,ncels
      a=0 ; b=0 ; c=0
      do j=1,cels(i)%nunodes
        k=cels(i)%node(j)       
        a=a+node(k)%x ; b=b+node(k)%y ; c=c+node(k)%z
      end do
      d=1d0/real(cels(i)%nunodes)
      cels(i)%cex=a*d ; cels(i)%cey=b*d ; cels(i)%cez=c*d
    end do

    rdiffmax=2*maxval(node(:nd)%da)*dmax !>>Miquel27-2-14

    !calculating neighbors
    if(nd>1) call neighbor_build

    if(ffu(23)==0)then
      call iterdiferencial
    else !forces is disabled
      delta=deltamin
    end if
    call gene_stuff
    call nexe        !nexe should be first >>> Is 13-2-14
    !WRITE(*,*)'SALEDENEXE'

    !UPDATINGS********
    !node positions from forces
    if (ffu(19)==0) then
      if (ffu(9)==0) then !euler numerical integration
        do i=1,nd                                       ! miguel4-11-13
          node(i)%x=node(i)%x+delta*px(i)               ! miguel4-11-13
          node(i)%y=node(i)%y+delta*py(i)               ! miguel4-11-13
          node(i)%z=node(i)%z+delta*pz(i)               ! miguel4-11-13
        end do
      else  ! Runge-Kutta order 4 numerical integration
        call rungekutta4(delta)
      end if
    else
      call adaptive_rungekutta
    end if

    !gene expression
    gex(1:nd,1:ng) = agex(1:nd,1:ng)
    where(gex.lt.0) gex=0.0d0

    !RANDOM NOISE
    if(ffu(23)==0)then
      c=nd*prop_noise*delta/deltamin !now the proportion of nodes is still dynamic, but equal to prop_noise on default  !>>Miquel28-7-14
     !print*,"proportion c",c
      if (c>1) then
        do il=1,int(c)
          call itera                  !****we add noise, though the behaviour of the system it's ok
        end do
      else
        call random_number(a)
        if (a<c) then
          call itera
        end if
      end if
    else  !forces disabled
      do il=1,nd
        call itera
      end do
    end if   

    if (nd<1.or.(ffu(2)==1.and.nd>=ndmax)) then !Is 25-12-13
      out_of_control=1
      status=10 !implies a 20560 exit status  Is 1-10-14
      if (nd>ndmax) then 
        print *,"too many cells, I quit"
        write(0,*) nd,"nd",ndmax,"ndmax too many cells, I quit"
      else
        if (nd<1.or.ncels<1) then  !>>> IS 10-5-14
          print *,"too few nodes, they all die"
          write(0,*) nd,"nd",ndmax,"too few cells, they all die"
        end if
      end if
      open(23,file=trim(carg)//"t")
      print *,"making...",trim(carg)//"t"
      write(23,*) trim(carg)//trim(nofi)
      close(23)
      call exit(status)
      stop
    end if

! physical boundaries !!!!!!!!!!!!!!!!
  if(ffu(14)==1)then
    do i=1,nd
        if(node(i)%z>1.0d0)then
        call random_number(a)
        node(i)%z=1.0d0-desmax*a
      end if
    end do
  end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    rtf=rtf+delta
    rtime=rtime+delta

    call put_param_to_matrix(param)

    if (mod(getot,fprint)==0) call prints
    if (mod(getot,freqsnap).eq.0) then ; if (fsnap==1) then ;call writesnap; end if ; end if
    if (mod(getot,100000).eq.0) print *,getot,nd,"getot nd",node(nd)%x

    if (ffu(12)==0) then
      if (ffu(21)==1) then
        if (rtf>=tf) then 
          call smooth !>>>Miguel 28-1-15
          print *,"the real time is ",rtime,"we now runned",rtf,"the number of iterations runned are",itf,"and",getot,&
          "in total : we have",nd,"nodes and",ncels,"cells" 
          exit 
        end if
      else
        if (itf==tf) then 
          call smooth !>>>Miguel 28-1-15
          print *,"real time is",rtf,"to run a time of ",tf,"delta",delta,"total number of iterations is",getot,"nd",nd,"ncels",ncels
          exit
        end if
      end if
    else 
      if (itf==tf) then 
        call smooth !>>>Miguel 28-1-15
        print *,"the number of iterations is",getot," and we just runned",itf,"we have",nd,"nodes and",ncels,"cells" 
        exit
      end if
    end if
  end do

  if((aut.ne.1).and.(aut.ne.5).and.(lock.eq.0))then  !>>>Miguel8-10-14   !>>>Miguel29-10-14
    call printgex1                   !>>>Miguel8-10-14
  end if                             !>>>Miguel8-10-14

  if (itviactual==itvi) then
    itvi=itvi+1
    if (itvi>mamax) itvi=1
    pnode(itvi,:)=node
    call put_param_to_matrix(param)
    pparam(itvi,:)=param
    pvarglobal_out(itvi,:)=varglobal_out
    itviactual=itvi
  end if

  passed=1

end subroutine

!**************************************************************************
subroutine itera
integer   ::i,accepta,val,ic,t,icc,iccc,nodmo,celi,nnod
real*8    ::ox,oy,oz,oe,de,ae,ocx,ocy,ocz,ax,ay,az,kl
real*8    :: lambda, cx,cy,cz,sx,sy,sz,aktin,radiolimite !>>>Miguel9-1-15
integer   ::nr  !>>> Is 4-3-14

    accepta=0
    recub=0
    movi=0             !>>>> miguel23-7-13
    nr=0
10  call random_number(a)
    nr=nr+1   !>>> Is 4-3-14
    nodmo=int(a*nd)+1 !ORIGNAL

    if (nr>nd*2) return             !>>> Is 4-3-14
    if(node(nodmo)%hold==2) goto 10 !>>> Is 4-3-14

    if(ffu(22)==0) call energia(nodmo)	!energia abans de moure'l !only when noise by energy is activated !>>Miquel28-7-14

    oe=node(nodmo)%e
    !mou-lo
    ox=node(nodmo)%x
    oy=node(nodmo)%y
    oz=node(nodmo)%z

    if (node(nodmo)%tipus<4) then
      celi=node(nodmo)%icel 
      nnod=cels(celi)%nunodes
      ocx=cels(celi)%cex		!the old centroid
      ocy=cels(celi)%cey
      ocz=cels(celi)%cez
    end if

    call random_number(a)
    desplacament=a*node(nodmo)%dmo
    if(desplacament<epsilod) return !no need to run all the energies if the movement is going to be 0 !>>Miquel27-8-14

  !insert biased noise here
    if(npag(nparam_per_node+16)>0.and.node(nodmo)%tipus<4) then !>>> IS 10-5-14
      a=0d0
      do k=1,npag(nparam_per_node+16)
        kk=whonpag(nparam_per_node+16,k)
        if (gex(nodmo,kk)>0.0d0) then
          a=a+gex(nodmo,kk)*gen(kk)%wa(nparam_per_node+16)  !wa in units of probability
        end if
      end do
      call random_number(b)
      k=int(b*nvaloq)+1
      ax=particions_esfera(k,1)+a*cels(celi)%polx  !we need to use a unit vector !>>Miquel28-7-14
      ay=particions_esfera(k,2)+a*cels(celi)%poly
      az=particions_esfera(k,3)+a*cels(celi)%polz
      d=1d0/sqrt(ax**2+ay**2+az**2)
      node(nodmo)%x=node(nodmo)%x+ax*d*desplacament
      node(nodmo)%y=node(nodmo)%y+ay*d*desplacament
      node(nodmo)%z=node(nodmo)%z+az*d*desplacament 
    else      
      call random_number(a)
      k=int(a*nvaloq)+1
      if((ncels.ge.5).and.(counterflow.gt.0))then  !>>>Miguel 28-1-15 here starts the blastomere rotation according to Beloussov rules                 
        !!!!!!!!!!!!!!! it automatically finds where to rotate ... 
          lambda=-1d0*(cels(celi)%sisterv(1)*node(nodmo)%x+cels(celi)%sisterv(2)&
          &*node(nodmo)%y+cels(celi)%sisterv(3)*cels(celi)%sisterv(3))  
          lambda=lambda/(cels(celi)%sisterv(1)**2+cels(celi)%sisterv(2)**2+cels(celi)%sisterv(3)**2)        
          cx=(node(nodmo)%x+lambda*cels(celi)%sisterv(1))
          cy=(node(nodmo)%y+lambda*cels(celi)%sisterv(2)) ; aktin=gex(nodmo,2)
          cz=((node(nodmo)%z-cels(node(nodmo)%icel)%cez)+lambda*cels(celi)%sisterv(3))
          !cx=((node(nodmo)%x-cels(node(nodmo)%icel)%cex)+lambda*cels(celi)%sisterv(1))
          !cy=((node(nodmo)%y-cels(node(nodmo)%icel)%cey)+lambda*cels(celi)%sisterv(2))
          !cz=((node(nodmo)%z-cels(node(nodmo)%icel)%cez)+lambda*cels(celi)%sisterv(3))
          sd=sqrt(cx**2+cy**2+cz**2) ; cx=cx/sd ; cy=cy/sd ; sc=sc/sd 
          sx=cy*cels(celi)%sisterv(3)-cz*cels(celi)%sisterv(2)
          sy=-1d0*(cx*cels(celi)%sisterv(3)-cz*cels(celi)%sisterv(1))
          sz=cx*cels(celi)%sisterv(2)-cy*cels(celi)%sisterv(1)         
          flowplot(nodmo,1)=sx*node(nodmo)%dmo*aktin
 	  flowplot(nodmo,2)=sy*node(nodmo)%dmo*aktin
          flowplot(nodmo,3)=sz*node(nodmo)%dmo*aktin
          node(nodmo)%x=node(nodmo)%x+flowplot(nodmo,1)
          node(nodmo)%y=node(nodmo)%y+flowplot(nodmo,2)
          node(nodmo)%z=node(nodmo)%z+flowplot(nodmo,3) 
       !!!!!!!!!!!!!!!! here finishes blastomere rotation !>>>MIguel 28-1-15         
      else
        node(nodmo)%x=node(nodmo)%x+particions_esfera(k,1)*desplacament
        node(nodmo)%y=node(nodmo)%y+particions_esfera(k,2)*desplacament
        node(nodmo)%z=node(nodmo)%z+particions_esfera(k,3)*desplacament 
      end if
    end if
   
      a=1d0/real(nnod)
      cels(celi)%cex=cels(celi)%cex+(node(nodmo)%x-ox)*a	!recalculate the centroid according to nod's new position
      cels(celi)%cey=cels(celi)%cey+(node(nodmo)%y-oy)*a
      cels(celi)%cez=cels(celi)%cez+(node(nodmo)%z-oz)*a

    if(ffu(22)==1.and.ffu(23)==0) return  !if unbiased nise is activated, we skip all the energy part, the node is simply moved randomly !>>Miquel28-7-14
    !Calculate energy for new position
    if(nd>1) call neighbor_build_node(nodmo)   ! >>> Is 29-6-14  ! THIS COULD BE OPTIMIZED
    call energia(nodmo)

    !acceptes?
    ae=node(nodmo)%e-oe
    if(movi.eq.1)then; goto 432 ; endif   !miguel4-11-13 
    if(ae<-epsilod)then
      accepta=1
      itacc=itacc+1
    else   
      call random_number(a)
      kl=temp+node(nodmo)%mo  ! >>> Is 10-10-14
      if (kl<0) kl=epsilod    ! >>> Is 10-10-14

      if(a<nue**(-ae/kl)) then ! >>> Is 10-10-14                
          accepta=1
          itacc=itacc+1      
      else
432     node(nodmo)%x=ox                    !miguel4-11-13
        node(nodmo)%y=oy
        node(nodmo)%z=oz
        node(nodmo)%e=oe
        if (node(nodmo)%tipus<4) then
          cels(celi)%cex=ocx		!reestablish the old centroid
          cels(celi)%cey=ocy
          cels(celi)%cez=ocz
        end if
        !call neighbor_build  ! >>> Is 29-6-14 !THIS COULD BE OPTIMIZED
        if(nd>1) call neighbor_build_node(nodmo)  ! >>> Is 29-6-14 !THIS COULD BE OPTIMIZED

        goto 150
      end if
    end if
   
150 continue

end subroutine itera

!*******************************************SUBROUTINE********************************************************
subroutine go_iteration_back(it)  !FUNKY MODULE POSITION IT IS CALLED FROM PINTA AND DOES NOT RUN SIMULATION
  integer it
  itviactual=itviactual-it
  if (itviactual<1) then ; itviactual=1 ; print *,"you've gone too far" ; endif
  call move_iteration(it)
end subroutine

!*******************************************SUBROUTINE********************************************************
subroutine go_iteration_forth(it)  !FUNKY MODULE POSITION IT IS CALLED FROM PINTA AND DOES NOT RUN SIMULATION
  integer it
  itviactual=itviactual+it
  if (itviactual>itvi) then ; itviactual=itvi; print *,"now we are were we were" ; endif
  call move_iteration(it)
end subroutine

!*******************************************SUBROUTINE********************************************************
subroutine move_iteration(it)
  node(:nd)=pnode(itviactual,:nd)
  param=pparam(itviactual,:)
  varglobal_out=pvarglobal_out(itviactual,:)
  call get_param_from_matrix(param)
  print *,"read ",getot,"getot",itviactual,"itviactual",itvi,"itvi"
end subroutine

!*************************************************************************************************************
subroutine ordenarepe(ma,mt,rang)
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
end subroutine ordenarepe

subroutine printgex1 !>>>Miguel 8-10-14 made subroutine
integer :: faktor,col1,col2
faktor=10

if(getot.eq.0)then   ; getott=1 
else;getott=getott+1 ; endif         

if(getott.eq.1)then 
   allocate(gext1(nd,ng,faktor))  
   allocate(ejex1(faktor))  
end if

col1=size(gext1(1,1,:)) ; col2=size(gext1(:,1,1))

if((getott.gt.col1).or.(nd.gt.col2))then
  allocate(gext2(col2,ng,col1),ejex2(col1))   
  gext2=gext1 ; ejex2=ejex1
  deallocate(gext1,ejex1) 
  allocate(gext1(nd,ng,getott+faktor),ejex1(getott+faktor))
  gext1(1:col2,1:ng,1:col1)=gext2(1:col2,1:ng,1:col1)
  ejex1(1:col1)=ejex2(1:col1)
  deallocate(gext2,ejex2)
end if
gext1(1:nd,1:ng,getott)=gex(1:nd,1:ng)
ejex1(getott)=getot

end subroutine printgex1

end module model
