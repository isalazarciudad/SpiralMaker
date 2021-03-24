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
!***************  MODUL VEINATGE ********************************************
!***************************************************************************

module neighboring
use general

integer,public, allocatable  :: boxes(:,:,:),list(:)
integer,public               :: nboxes,iextre
integer,public, allocatable  :: borderface(:,:)  !>>Miquel3-2-14
integer,public               :: nborder

contains

!**************************************************************************
subroutine extrem 
    ! troba quina es la cel que esta mes allunyada del centre del embryo
    extre=0.0d0 ; iextre=1 
    do i=1,nd
      a=sqrt((node(i)%x)**2+(node(i)%y)**2+(node(i)%z)**2)
      if (a>extre) then ; extre=a ; iextre=i ; end if ;
    end do
end subroutine extrem

!**************************************************************************
subroutine iniboxes
integer ic,ii,jj,kk
integer,allocatable:: cboxes(:,:,:)

    oextre=0	
    call extrem
    nboxes=nint(extre*urv)+maxval(node(:nd)%dmo)+dmax+1
    if (allocated(list)) deallocate(list)
    allocate(list(nda))	!>>Miquel 14-10-12
    list=0
    if (allocated(boxes)) deallocate(boxes)
    if (allocated(cboxes)) deallocate(cboxes)
    allocate(boxes(-nboxes:nboxes,-nboxes:nboxes,-nboxes:nboxes))
    allocate(cboxes(-nboxes:nboxes,-nboxes:nboxes,-nboxes:nboxes))
    boxes=0
    cboxes=0
    do i=1,nd
      ii=nint(node(i)%x*urv);jj=nint(node(i)%y*urv);kk=nint(node(i)%z*urv)
      list(i)=boxes(ii,jj,kk)
      boxes(ii,jj,kk)=i
      cboxes(ii,jj,kk)=cboxes(ii,jj,kk)+1
    end do
    mnn_dyn=maxval(cboxes)
end subroutine iniboxes

!**************************************************************************
subroutine iniboxesll
    oextre=0
    call extrem

    nboxes=nint(extre*urv)+maxval(node(:nd)%dmo)+dmax+1 !;if(nboxes<7) print*,"NBOXESll",nboxes,"extre",extre
    if (allocated(list)) deallocate(list)
    allocate(list(nda))	!>>Miquel 14-10-12
    list=0
    if (allocated(boxes)) deallocate(boxes)
    allocate(boxes(-nboxes:nboxes,-nboxes:nboxes,-nboxes:nboxes))
    boxes=0

    do i=1,nd 
      ii=nint(node(i)%x*urv);jj=nint(node(i)%y*urv);kk=nint(node(i)%z*urv) !;print*,"ii",ii,"jj",jj,"kk",kk
      list(i)=boxes(ii,jj,kk)
      boxes(ii,jj,kk)=i                 
    end do
end subroutine iniboxesll

!************************************************

subroutine neighbor_build !this subroutine is called at each iteration to assess the neighbors of all the nodes !>>>Miquel24-2-14
integer:: ivv,ii1,ii2,ii3,nbo,iii1,iii2,iii3,ie,ierr,ij,ik,jk,iv
real*8:: ix,iy,iz,dist,udist,nbh

integer::tipi  !>>Miquel31-12-14
real*8::dai,maxlen    !>>Miquel31-12-14

!!!triangulation variables
integer:: npt !number of points
integer:: sizht !size of hash table
integer:: maxbf,maxfc,nbf,nfc,nface,ntetra   !size of arrays
integer,allocatable :: vecinod(:,:),vecic(:)!miguel
real*8,allocatable :: dvecinod(:,:)
real*8,allocatable :: vcl(:,:) !point coordinates
integer,allocatable :: vm(:),ht(:),bf(:,:),fc(:,:) !point indices (the algorithm reorders)
integer,dimension(:) :: border(nd)
real*8::jx,jy,jz,cx,cy,cz  !>>Miquel6-3-14
integer::o                 !>>Miquel6-3-14
integer,allocatable::osneigh(:),sneigh(:),trans_neigh(:,:)
real*8,allocatable::osdneigh(:),sdneigh(:),trans_dneigh(:,:)
integer::snneigh,error
integer::L,RRA2,IR
real*8::RRA

  !>>Miquel31-12-14
  maxlen=sqrt((2*maxval(node(:nd)%reqs))**2+(2*maxval(node(:nd)%da)**2)) !this is the maximal interaction distance between epithelial nodes
  a=2*maxval(node(:nd)%da) !maximal interaction distance between mesenchymal nodes
  if(a>maxlen)then ; rv=a ; urv=1.0d0/a ; else ; rv=maxlen ; urv=1.0d0/maxlen ;end if
  rdiffmax=2*maxval(node(:nd)%da)*dmax

  call iniboxes
    
  mnn_dynam=mnn_dyn*(2*nint(rdiffmax*urv)+1)**3
  allocate(trans_neigh(nd,mnn_dynam),trans_dneigh(nd,mnn_dynam))

  if (ffu(13)==0)then !normal neighboring, extensive search of the boxes
    omnn=0
    do i=1,nd
      if (rdiffmax<2*node(i)%da)then
        nbh=2*node(i)%da  !the neighbor search range for diffusion is the same as for node interactions
      else
        nbh=rdiffmax
      end if
      
      ix=node(i)%x     ; iy=node(i)%y     ; iz=node(i)%z   
      ii1=nint(iz*urv) ; ii2=nint(iy*urv) ; ii3=nint(ix*urv)
      ivv=node(i)%altre

      nbo=nint(nbh*urv) !;print*,"nbo",nbo,"rdiffmax",rdiffmax
      mnn_dynam=mnn_dyn*(2*nbo+1)**3 !;print*,"mnn_dyn def",mnn_dynam !calculating the alleged maximal width of the neigh matrix

      allocate(sdneigh(mnn_dynam),sneigh(mnn_dynam))
      snneigh=0
      sneigh=0 ; sdneigh=0d0

      if(node(i)%tipus<3)then
        snneigh=1
        sneigh(1)=ivv
        sdneigh(1)=sqrt((node(ivv)%x-ix)**2+(node(ivv)%y-iy)**2+(node(ivv)%z-iz)**2)
      end if


      tipi=node(i)%tipus
      dai=node(i)%da
      do i1=-nbo,nbo 
        iii1=ii1+i1
        do i2=-nbo,nbo
          iii2=ii2+i2
          do i3=-nbo,nbo
            iii3=ii3+i3
            ie=boxes(iii3,iii2,iii1)
            do while(ie.ne.0)
              if (ie==i) then ; ie=list(ie) ; cycle ; end if
              if (ie==ivv) then ; ie=list(ie) ; cycle ; end if
              dist=sqrt((node(ie)%x-ix)**2+(node(ie)%y-iy)**2+(node(ie)%z-iz)**2)
              !>>Miquel31-12-14
              a=dai+node(ie)%da
              if(tipi>=3)then
                if(node(ie)%tipus>=3)then !mesench/ECM vs mesench/ECM
                  if(dist>a)then ; ie=list(ie) ; cycle ; end if
                else  !mesench/ECM vs epithelial
                  b=sqrt(2*(a**2))
                  if(dist>b)then ; ie=list(ie) ; cycle ; end if
                end if
              else !epithelial vs epithelial
                b=sqrt(2*(a**2))
                if(tipi==node(ie)%tipus)then !same face epithelials
                  if(b<maxlen) b=maxlen
                  if(dist>b)then ; ie=list(ie) ; cycle ; end if
                else
                  if(dist>b)then ; ie=list(ie) ; cycle ; end if
                end if
              end if
              !>>Miquel31-12-14
              snneigh=snneigh+1
              sneigh(snneigh)=ie
              sdneigh(snneigh)=dist
              ie=list(ie)
            end do
          end do
        end do
      end do
      
      allocate(osdneigh(snneigh),osneigh(snneigh))
      osdneigh(1:snneigh)=sdneigh(1:snneigh)
      osneigh(1:snneigh)=sneigh(1:snneigh)

      if(ffu(3)==1)then
        !screening by Gabriel graph !>>Miquel6-3-14
        !a neighbor connection is deleted if the sphere which diameter is the vector connecting the two nodes contains any other node
        !ordering the neighors with increasing distance

        !sorting algorithm by selection, it's ok
        do j=1,snneigh-1  
          b=osdneigh(j)
          ii=0
          do k=j+1,snneigh
            c=osdneigh(k)
             if(b>c)then
              ii=k ; b=osdneigh(k)
            end if
          end do
          if(ii/=0)then
            !jj=osneigh(j)
            kk=osneigh(ii) ; c=osdneigh(ii) !the swap
            osneigh(ii)=osneigh(j) ; osdneigh(ii)=osdneigh(j)
            osneigh(j)=kk ; osdneigh(j)=c
          end if
        end do

        !the screening
        ii=0 !the number of eliminated connections
        do j=snneigh,1,-1
          jj=osneigh(j)
          if(jj==ivv) cycle
          a=osdneigh(j)*0.5*screen_radius !the radius of the sphere !>>Miquel28-7-14
          jx=node(jj)%x ; jy=node(jj)%y ; jz=node(jj)%z
          cx=(ix+jx)*0.5 ; cy=(iy+jy)*0.5 ; cz=(iz+jz)*0.5 !the midpoint
          do k=j-1,1,-1
            kk=osneigh(k)
            d=sqrt((cx-node(kk)%x)**2+(cy-node(kk)%y)**2+(cz-node(kk)%z)**2)
            if(d-a<epsilod)then !there is one node within the sphere, we must delete this connection
              do l=j,snneigh-ii-1
                osneigh(l)=osneigh(l+1)
                osdneigh(l)=osdneigh(l+1)
              end do
              osneigh(snneigh-ii)=0
              osdneigh(snneigh-ii)=0
              ii=ii+1
              exit
            end if
          end do
        end do
        snneigh=snneigh-ii   
      end if

      trans_neigh(i,1:snneigh)=osneigh(1:snneigh)
      trans_dneigh(i,1:snneigh)=osdneigh(1:snneigh)

      if(snneigh>omnn) omnn=snneigh
      nneigh(i)=snneigh
      deallocate(osdneigh,osneigh,sdneigh,sneigh)

    end do
    if(allocated(neigh)) deallocate(neigh)
    if(allocated(dneigh)) deallocate(dneigh)
    !write(*,*)'alocato en neigh',nda,mnn,omnn
    allocate(neigh(nda,omnn),dneigh(nda,omnn))
    neigh(1:nd,1:omnn)=trans_neigh(1:nd,1:omnn)
    dneigh(1:nd,1:omnn)=trans_dneigh(1:nd,1:omnn)
   
  else   !*****3D triangulation neighooring***************************************************************
    
    npt=nd
    sizht=3*npt
    maxfc=npt**2
    maxbf=npt**2
    allocate(vcl(3,npt),vm(npt))
    allocate(bf(1:3,maxbf),fc(1:7,maxfc))
    allocate(ht(0:sizht-1))
        
    !call extrem !not necessary, but sets some variables that later may be used in pinta and creix !>>Miquel21-3-14    
    
    do i=1,npt  !building the input arrays for the triangualtion function
      vcl(1,i)=node(i)%x ; vcl(2,i)=node(i)%y ; vcl(3,i)=node(i)%z
      vm(i)=i
    end do 
    !calling the triangulation subroutine
    call dtriw3(npt, sizht, maxbf, maxfc, vcl, vm, nbf, nfc, nface, ntetra, bf, fc, ht, ierr)
    if(ierr/=0)then; print*,"error in the triangulation, avort or something",ierr,getot ; endif !call exit(24); end if
    !translating the ouptut into a neighbor matrix

    do i=1,nd !initialize neighbor matrix
      neigh(i,1:)=0  !IS 29-4-14
      nneigh(i)=0
    end do
    do j=1,nfc               ! it passes through all triangles 
      if((fc(1,j)>0).and.(fc(2,j)>0).and.(fc(3,j)>0))then ! it is a "valid" triangle
        ii=fc(1,j) ; jj=fc(2,j) ; kk=fc(3,j)
        iii=vm(ii);jjj=vm(jj);kkk=vm(kk) !; print*,"iii jjj kkk",iii,jjj,kkk
        tiii=node(iii)%tipus ; tjjj=node(jjj)%tipus ; tkkk=node(kkk)%tipus

        ivv=nneigh(iii)
        ij=0 ; ik=0 ; jk=0
        if(ivv==0)then
          d=sqrt((vcl(1,iii)-vcl(1,jjj))**2+(vcl(2,iii)-vcl(2,jjj))**2+(vcl(3,iii)-vcl(3,jjj))**2)
          ivv=1
          trans_neigh(iii,ivv)=jjj ; trans_dneigh(iii,ivv)=d
        else
          do i=1,nneigh(iii)
            if(trans_neigh(iii,i)==jjj)then; ij=1;exit;end if
          end do
          if(ij==0)then
            d=sqrt((vcl(1,iii)-vcl(1,jjj))**2+(vcl(2,iii)-vcl(2,jjj))**2+(vcl(3,iii)-vcl(3,jjj))**2)
            ivv=ivv+1
            trans_neigh(iii,ivv)=jjj ; trans_dneigh(iii,ivv)=d
          end if
        end if
        if(ij==0)then
          !connection jjj-iii*******
          if(nneigh(jjj)==0)then ; nneigh(jjj)=1 ; trans_neigh(jjj,1)=iii ; trans_dneigh(jjj,1)=d
          else; iv=nneigh(jjj)+1 ; trans_neigh(jjj,iv)=iii ; trans_dneigh(jjj,iv)=d ; nneigh(jjj)=iv ; end if
        end if
        !connection iii-kkk*****
        do i=1,nneigh(iii)
          if(trans_neigh(iii,i)==kkk)then; ik=1;exit;end if
        end do
        if(ik==0)then
          d=sqrt((vcl(1,iii)-vcl(1,kkk))**2+(vcl(2,iii)-vcl(2,kkk))**2+(vcl(3,iii)-vcl(3,kkk))**2)
          ivv=ivv+1
          trans_neigh(iii,ivv)=kkk ; trans_dneigh(iii,ivv)=d
          !connection kkk-iii*******
          if(nneigh(kkk)==0)then ; nneigh(kkk)=1 ; trans_neigh(kkk,1)=iii ; trans_dneigh(kkk,1)=d
          else; iv=nneigh(kkk)+1 ; trans_neigh(kkk,iv)=iii ; trans_dneigh(kkk,iv)=d ; nneigh(kkk)=iv ; end if
        end if
        nneigh(iii)=ivv
        !connection jjj-kkk*****
        do i=1,nneigh(jjj)
          if(trans_neigh(jjj,i)==kkk)then; jk=1;exit;end if
        end do
        if(jk==0)then
          d=sqrt((vcl(1,jjj)-vcl(1,kkk))**2+(vcl(2,jjj)-vcl(2,kkk))**2+(vcl(3,jjj)-vcl(3,kkk))**2)
          iv=nneigh(jjj)+1
          trans_neigh(jjj,iv)=kkk ; trans_dneigh(jjj,iv)=d ;nneigh(jjj)=iv
          !connection kkk-jjj*******
          iv=nneigh(kkk)+1 ; trans_neigh(kkk,iv)=jjj ; trans_dneigh(kkk,iv)=d ; nneigh(kkk)=iv
        end if
      end if
    end do
 
    if(ffu(3)==1)then
    
      do i=1,nd
        !Screening by Gabriel graph !>>Miquel6-3-14
        !A neighbor connection is deleted if the sphere which diameter is the vector connecting the two nodes contains any other node
        !Ordering the neighors with increasing distance

        !sorting algorithm by selection, it's ok
        do j=1,nneigh(i)-1  
          b=trans_dneigh(i,j)
          ii=0
          do k=j+1,nneigh(i)
            c=trans_dneigh(i,k)
             if(b>c)then
              ii=k ; b=trans_dneigh(i,k)
            end if
          end do
          if(ii/=0)then
            !jj=osneigh(j)
            kk=trans_neigh(i,ii) ; c=trans_dneigh(i,ii) !the swap
            trans_neigh(i,ii)=trans_neigh(i,j) ; trans_dneigh(i,ii)=trans_dneigh(i,j)
            trans_neigh(i,j)=kk ; trans_dneigh(i,j)=c
          end if
        end do
        
        !the screening
        ii=0 !the number of eliminated connections
        do j=nneigh(i),1,-1
          jj=trans_neigh(i,j)
          if(jj==ivv) cycle
          a=trans_dneigh(i,j)*0.5*screen_radius !the radius of the sphere
          jx=node(jj)%x ; jy=node(jj)%y ; jz=node(jj)%z
          cx=(ix+jx)*0.5 ; cy=(iy+jy)*0.5 ; cz=(iz+jz)*0.5 !the midpoint
          do k=j-1,1,-1
            kk=trans_neigh(i,k)
            d=sqrt((cx-node(kk)%x)**2+(cy-node(kk)%y)**2+(cz-node(kk)%z)**2)
            if(d<a)then !there is one node within the sphere, we must delete this connection
              do l=j,nneigh(i)-ii-1
                trans_neigh(i,l)=trans_neigh(i,l+1)
                trans_dneigh(i,l)=trans_dneigh(i,l+1)
              end do
              trans_neigh(i,nneigh(i)-ii)=0
              trans_dneigh(i,nneigh(i)-ii)=0
              ii=ii+1
              exit
            end if
          end do
        end do
        nneigh(i)=nneigh(i)-ii
      end do
      
    end if
    
    omnn=0
    do i=1,nd
      if(nneigh(i)>omnn) omnn=nneigh(i)
    end do
    if(allocated(neigh)) deallocate(neigh)
    if(allocated(dneigh)) deallocate(dneigh)
    allocate(neigh(nda,omnn),dneigh(nda,omnn))
    neigh(1:nd,1:omnn)=trans_neigh(1:nd,1:omnn)
    dneigh(1:nd,1:omnn)=trans_dneigh(1:nd,1:omnn)
  end if

end subroutine neighbor_build
!************************************************

subroutine neighbor_build_node(i) !this calculates the neighbors for one node onlly, used for random noise !>>Miquel16-12-14
integer:: ivv,ii1,ii2,ii3,nbo,iii1,iii2,iii3,ie,ierr,ij,ik,jk,iv
real*8:: ix,iy,iz,dist,udist,nbh
integer::i

!!!triangulation variables
integer:: npt !number of points  
integer:: sizht !size of hash table
integer:: maxbf,maxfc,nbf,nfc,nface,ntetra   !size of arrays
integer,allocatable :: vecinod(:,:),vecic(:)!miguel
real*8,allocatable :: dvecinod(:,:)
real*8,allocatable :: vcl(:,:) !point coordinates
integer,allocatable :: vm(:),ht(:),bf(:,:),fc(:,:) !point indices (the algorithm reorders)
integer,dimension(:) :: border(nd)
real*8::jx,jy,jz,cx,cy,cz  !>>Miquel6-3-14
integer::o                 !>>Miquel6-3-14
real*8::dai,maxlen
integer,allocatable::osneigh(:),sneigh(:),trans_neigh(:,:)
real*8,allocatable::osdneigh(:),sdneigh(:),trans_dneigh(:,:)
integer::snneigh,error
integer::L,RRA2,IR
real*8::RRA
integer::mnn_dynam


  maxlen=sqrt((2*maxval(node(:nd)%reqs))**2+(2*maxval(node(:nd)%da)**2)) !this is the maximal interaction distance between epithelial nodes
  a=2*maxval(node(:nd)%da) !maximal interaction distance between mesenchymal nodes
  if(a>maxlen)then ; rv=a ; urv=1.0d0/a ; else ; rv=maxlen ; urv=1.0d0/maxlen ;end if

  rdiffmax=2*maxval(node(:nd)%da)*dmax
  
  call iniboxes
 
    if (rdiffmax<2*node(i)%da)then
      nbh=2*node(i)%da  !the neighbor search range for diffusion is the same as for node interactions
    else
      nbh=rdiffmax
    end if
    ix=node(i)%x     ; iy=node(i)%y     ; iz=node(i)%z   
    ii1=nint(iz*urv) ; ii2=nint(iy*urv) ; ii3=nint(ix*urv)
    ivv=node(i)%altre
    
    nbo=nint(nbh*urv) 
    mnn_dynam=mnn_dyn*(2*nbo+1)**3 
    
    do j=1,nneigh(i)                            !!This to erase node i from the neighbor matrix, as it will be ovewritten later  !! >>>Miguel17-12-14
      k=neigh(i,j)                              ! "k" neighbor
      do ii=1,nneigh(k)                         ! it searches in the neighborhood of "k"         
        if(i.eq.neigh(k,ii))then                ! if it (i) appears    
          neigh(k,ii:nneigh(k)-1)=neigh(k,ii+1:nneigh(k))   ; neigh(k,nneigh(k))=0   !  neigh matrix is displaced
          dneigh(k,ii:nneigh(k)-1)=dneigh(k,ii+1:nneigh(k)) ; dneigh(k,nneigh(k))=0  !  dneigh matrix is diaplced
          nneigh(k)=nneigh(k)-1 ; exit                                               !  neighbor counter
        end if 
      end do        
    end do
    neigh(i,:)=0 ; nneigh(i)=0 ; dneigh(i,:)=0d0  !! >>>Miguel17-12-14
    
    allocate(sdneigh(mnn_dynam),sneigh(mnn_dynam))
    snneigh=0
    sneigh=0 ; sdneigh=0d0
    
    
    tipi=node(i)%tipus
    if(node(i)%tipus<3)then
      snneigh=1
      sneigh(1)=ivv 
      sdneigh(1)=sqrt((node(ivv)%x-ix)**2+(node(ivv)%y-iy)**2+(node(ivv)%z-iz)**2)
    end if

    dai=node(i)%da
    do i1=-nbo,nbo 
      iii1=ii1+i1
      do i2=-nbo,nbo
        iii2=ii2+i2
        do i3=-nbo,nbo
          iii3=ii3+i3
          ie=boxes(iii3,iii2,iii1)
          do while(ie.ne.0)
            if (ie==i) then ; ie=list(ie) ; cycle ; end if
            if (ieI==ivv) then ; ie=list(ie) ; cycle ; end if
            dist=sqrt((node(ie)%x-ix)**2+(node(ie)%y-iy)**2+(node(ie)%z-iz)**2)
            !>>Miquel31-12-14
            a=dai+node(ie)%da
            if(tipi>=3)then
              if(node(ie)%tipus>=3)then !mesench/ECM vs mesench/ECM
                if(dist>a)then ; ie=list(ie) ; cycle ; end if
              else  !mesench/ECM vs epithelial
                b=sqrt(2*(a**2))
                if(dist>b)then ; ie=list(ie) ; cycle ; end if
              end if
            else !epithelial vs epithelial
              b=sqrt(2*(a**2))
              if(tipi==node(ie)%tipus)then !same face epithelials
                if(b<maxlen) b=maxlen
                if(dist>b)then ; ie=list(ie) ; cycle ; end if
              else
                if(dist>b)then ; ie=list(ie) ; cycle ; end if
              end if
            end if
            !>>Miquel31-12-14
            snneigh=snneigh+1
            sneigh(snneigh)=ie
            !dneigh(i,nneigh(i))=sqrt((node(ie)%x-ix)**2+(node(ie)%y-iy)**2+(node(ie)%z-iz)**2)
            sdneigh(snneigh)=dist
            ie=list(ie)
          end do
        end do
      end do
    end do
    
    allocate(osdneigh(snneigh),osneigh(snneigh))
    osdneigh(1:snneigh)=sdneigh(1:snneigh)
    osneigh(1:snneigh)=sneigh(1:snneigh)
    
    if((ffu(3)==1).or.(ffu(13)==1))then !! >>>Miguel17-12-14
      !screening by Gabriel graph !>>Miquel6-3-14
      !a neighbor connection is deleted if the sphere which diameter is the vector connecting the two nodes contains any other node
      do j=1,snneigh-1  !ordering the neighors with increasing distance
        b=osdneigh(j)
        ii=0
        do k=j+1,snneigh
          c=osdneigh(k)
           if(b>c)then
            ii=k ; b=osdneigh(k)
          end if
        end do
        if(ii/=0)then
          !jj=neigh(i,j)
          kk=osneigh(ii) ; c=osdneigh(ii) !the swap
          osneigh(ii)=osneigh(j) ; osdneigh(ii)=osdneigh(j)
          osneigh(j)=kk ; osdneigh(j)=c
        end if
      end do
      if(ffu(13).eq.1)then;b=0.75d0;else;b=screen_radius;endif ! >>>Miguel17-12-14
      !the screening
      ii=0 !the number of eliminated connections
      do j=snneigh,1,-1
        jj=osneigh(j)
        if(jj==ivv) cycle
        a=osdneigh(j)*0.5*b !the radius of the sphere !>>Miquel28-7-14   ! >>>Miguel17-12-14       
        jx=node(jj)%x ; jy=node(jj)%y ; jz=node(jj)%z
        cx=(ix+jx)*0.5 ; cy=(iy+jy)*0.5 ; cz=(iz+jz)*0.5 !the midpoint
        do k=j-1,1,-1
          kk=osneigh(k)
          d=sqrt((cx-node(kk)%x)**2+(cy-node(kk)%y)**2+(cz-node(kk)%z)**2)
          if(d-a<epsilod)then !there is one node within the sphere, we must delete this connection
            do l=j,snneigh-ii-1
              osneigh(l)=osneigh(l+1)
              osdneigh(l)=osdneigh(l+1)
            end do
            osneigh(snneigh-ii)=0
            osdneigh(snneigh-ii)=0
            ii=ii+1
            exit
          end if
        end do
      end do
      snneigh=snneigh-ii        
    end if
    
    if(snneigh>omnn)then !the whole matrix has to be reallocated
      ii=omnn
      allocate(trans_neigh(nd,omnn),trans_dneigh(nd,omnn))
      trans_neigh(1:nd,1:omnn)=neigh(1:nd,1:omnn)
      trans_dneigh(1:nd,1:omnn)=dneigh(1:nd,1:omnn)
      deallocate(neigh,dneigh)
      omnn=snneigh
      allocate(neigh(nda,omnn),dneigh(nda,omnn))
      neigh(1:nd,1:ii)=trans_neigh(1:nd,1:ii)
      dneigh(1:nd,1:ii)=trans_dneigh(1:nd,1:ii)
      deallocate(trans_neigh,trans_dneigh)
    end if
    
    nneigh(i)=snneigh
    neigh(i,1:snneigh)=osneigh(1:snneigh)
    dneigh(i,1:snneigh)=osdneigh(1:snneigh)
    
    do j=1,snneigh !here we put i on the reciprocal neighborhood of its neighbors
      k=osneigh(j)
      nneigh(k)=nneigh(k)+1
      if(nneigh(k)>omnn)then  !the whole matrix has to be reallocated
        ii=omnn
        allocate(trans_neigh(nd,omnn),trans_dneigh(nd,omnn))
        trans_neigh(1:nd,1:omnn)=neigh(1:nd,1:omnn)
        trans_dneigh(1:nd,1:omnn)=dneigh(1:nd,1:omnn)
        deallocate(neigh,dneigh)
        omnn=nneigh(k)
        allocate(neigh(nda,omnn),dneigh(nda,omnn))
        neigh(1:nd,1:ii)=trans_neigh(1:nd,1:ii)
        dneigh(1:nd,1:ii)=trans_dneigh(1:nd,1:ii)
        deallocate(trans_neigh,trans_dneigh)
      end if
      neigh(k,nneigh(k))=i
      dneigh(k,nneigh(k))=osdneigh(j)
    end do

end subroutine neighbor_build_node

!************************************************

subroutine membrane   !!>>>Miguel 28-1-15 subroutine, locates external (membrane) nodes and triangles
integer:: ivv,ii1,ii2,ii3,nbo,iii1,iii2,iii3,ie,ierr,ij,ik,jk,iv
real*8:: ix,iy,iz,dist,udist,nbh

!!!triangulation variables
integer:: npt !number of points
integer:: sizht !size of hash table
integer:: maxbf,maxfc,nbf,nfc,nface,ntetra   !size of arrays
integer,allocatable :: vecinod(:,:),vecic(:),bfl(:) !>>>Miguel 10-6-14!miguel
real*8,allocatable :: dvecinod(:,:)
real*8,allocatable :: vcl(:,:) !point coordinates
integer,allocatable :: vm(:),ht(:),bf(:,:),fc(:,:) !point indices (the algorithm reorders)
integer,dimension(:) :: border(nd)
integer,allocatable :: elim(:) !>>>Miguel 10-6-14
real*8::jx,jy,jz,cx,cy,cz,mvn  !>>Miquel6-3-14
real*8::abx,aby,abz,acx,acy,acz,adx,ady,adz,d1,d2,d3,kx,ky,kz !>>>Miguel 10-6-14
integer::o,oo                 !>>Miquel6-3-14!Miguel14-10-14 "oo" 

  rv=2*maxval(node(:nd)%da);urv=1d0/rv !;print*,"RV",rv
  rdiffmax=2*maxval(node(:nd)%da)*dmax

    npt=nd
    sizht=3*npt
    maxfc=npt**2
    maxbf=npt**2
    allocate(vcl(3,npt),vm(npt))
    allocate(bf(1:3,maxbf),fc(1:7,maxfc))
    allocate(ht(0:sizht-1))    
    
    call extrem !not necessary, but sets some variables that later may be used in pinta and creix !>>Miquel21-3-14
        
    do i=1,npt  !building the input arrays for the triangualtion function
      vcl(1,i)=node(i)%x ; vcl(2,i)=node(i)%y ; vcl(3,i)=node(i)%z
      vm(i)=i
    end do !;print*,"pillat pre delau"
    !calling the triangulation subroutine

    call dtriw3(npt, sizht, maxbf, maxfc, vcl, vm, nbf, nfc, nface, ntetra, bf, fc, ht, ierr)
    if(ierr/=0)then; print*,"error in the triangulation, avort or something",ierr,getot ; endif !call exit(24); end if

   do i=1,ncels         
      if(allocated(cels(i)%memb))then   ; deallocate(cels(i)%memb) ; endif
      if(allocated(cels(i)%membt))then  ; deallocate(cels(i)%membt); endif
      if(allocated(cels(i)%amembt))then  ; deallocate(cels(i)%amembt); endif     
      allocate(cels(i)%memb(cels(i)%nunodes**2))    ! external nodes  
      allocate(cels(i)%membt(3,cels(i)%nunodes**2)) ! external triangles  
      allocate(cels(i)%amembt(cels(i)%nunodes**2,3))  ! areas of external triangles
      cels(i)%memb(:)=0                   ! external nodes
      cels(i)%membt(:,:)=0                ! external triangles 
      cels(i)%amembt(:,:)=0d0             ! area of external triangles    
      cels(i)%nmemb=1                     ! number of external nodes
      cels(i)%nmembt=1                    ! number of external triangles    
    end do
        if(allocated(bfl))then  ; deallocate(bfl); endif 
        if(allocated(elim))then  ; deallocate(elim); endif  
        allocate(bfl(nbf**2)) ; bfl=0      ! indexes of boundary triangles (dtriw3 does not provide the immediate list) !miguel 4.3.14 **2(era*3)
        allocate(elim((maxval(cels(:)%nunodes))**2)) ; elim=0 ! miguel4.3.14
    node(:)%exte=0

    if(ncels.gt.1)then                       ! it deleted the shared triangles miguel 4.3.14
      do i=1,nfc                              ! it passes through all triangles 
        ii=fc(1,i)  ; jj=fc(2,i)  ; kk=fc(3,i)! three elements of the triangle   
        jjj=fc(4,i) ; kkk=fc(5,i)             ! neighbours in the adjacent tetrahedra  
        if((ii.gt.0).and.(jj.gt.0).and.(kk.gt.0).and.(jjj.gt.0).and.(kkk.gt.0))then ! it is a "valid" internal triangle
          iii=node(vm(ii))%icel             !; write(*,*)'iii',iii,'ii',ii,'vm(ii)',vm(ii)
          if((iii.eq.node(vm(jj))%icel).and.(iii.eq.node(vm(kk))%icel))then  ! if the triangle belongs to the same cell         
            if(ncels.gt.1)then 
            if((iii.ne.node(vm(jjj))%icel).or.(iii.ne.node(vm(kkk))%icel))then ! the triangle is contacting the adjacent cell ...
              cels(iii)%membt(:,cels(iii)%nmembt)=fc(1:3,i) 
              !!!!! it stores the area of the traingle
              ix=node(vm(jj))%x-node(vm(ii))%x ; iy=node(vm(jj))%y-node(vm(ii))%y 
              iz=node(vm(jj))%z-node(vm(ii))%z ; jx=node(vm(kk))%x-node(vm(ii))%x 
              jy=node(vm(kk))%y-node(vm(ii))%y ; jz=node(vm(kk))%z-node(vm(ii))%z
              kx=(iy*jz)-(iz*jy) ; ky=-1d0*((ix*jz)-(iz*jx)) ; kz=(ix*jy)-(iy*jx)       ! vectorial product !!!thus, the area is calculated
              mvn=sqrt(kx**2+ky**2+kz**2)   ! module of the vectorial product            
              cels(iii)%amembt(cels(iii)%nmembt,1)=kx/mvn 
              cels(iii)%amembt(cels(iii)%nmembt,2)=ky/mvn ;  cels(iii)%amembt(cels(iii)%nmembt,3)=kz/mvn 
             !!!!! nornal pointing outside ??
              oo=0
              if(iii.eq.node(vm(jjj))%icel)then; oo=vm(jjj) ; endif
              if(iii.eq.node(vm(kkk))%icel)then; oo=vm(kkk) ; endif
              if(oo.eq.0)then ; goto 1818 ; endif
                d1=node(oo)%x-node(vm(ii))%x 
                d2=node(oo)%y-node(vm(ii))%y 
                d3=node(oo)%z-node(vm(ii))%z  
                adx=(d1*cels(iii)%amembt(cels(iii)%nmembt,1))+(d2*cels(iii)%amembt(cels(iii)%nmembt,2))&
                &+(d3*cels(iii)%amembt(cels(iii)%nmembt,3))
                adx=adx/sqrt(d1**2+d2**2+d3**2)
                ady=acos(adx)!*57.2957795
                if(ady.lt.pi/2)then
                  cels(iii)%amembt(cels(iii)%nmembt,1)=-1d0*cels(iii)%amembt(cels(iii)%nmembt,1)
                  cels(iii)%amembt(cels(iii)%nmembt,2)=-1d0*cels(iii)%amembt(cels(iii)%nmembt,2)
                  cels(iii)%amembt(cels(iii)%nmembt,3)=-1d0*cels(iii)%amembt(cels(iii)%nmembt,3)
                end if    
              !!!!!
1818             cels(iii)%nmembt=cels(iii)%nmembt+1 ! the new triangle is stored
              do j=1,cels(iii)%nmemb           ! it checks whether the nodes were already "membrain-like" nodes of the cell "iii"
!                write(*,*)j,'ii,jj,kk',ii,jj,kk,'nmemb',cels(iii)%nmemb,cels(iii)%memb(j)
                if(ii.eq.cels(iii)%memb(j))then ; ii=0 ; endif 
                if(jj.eq.cels(iii)%memb(j))then ; jj=0 ; endif 
                if(kk.eq.cels(iii)%memb(j))then ; kk=0 ; endif 
                if(j.eq.cels(iii)%nmemb)then   ! it adds the new nodes found in the "boundary" triangles
                  if(ii.ne.0)then;cels(iii)%memb(cels(iii)%nmemb)=ii ;node(vm(ii))%exte=1; 
                    cels(iii)%nmemb=cels(iii)%nmemb+1;endif 
                  if(jj.ne.0)then;cels(iii)%memb(cels(iii)%nmemb)=jj ;node(vm(jj))%exte=1;
                    cels(iii)%nmemb=cels(iii)%nmemb+1;endif 
                  if(kk.ne.0)then;cels(iii)%memb(cels(iii)%nmemb)=kk ;node(vm(kk))%exte=1; 
                    cels(iii)%nmemb=cels(iii)%nmemb+1;endif 
                endif       
              end do                  
            end if
            end if
          end if
        end if            
      end do          
   endif

    do i=1,nfc!jjjj                             ! it passes through all boundary triangles     
      iiii=i!bfl(i)  
      if(fc(5,i).lt.0)then!(iiii.gt.0)then 
          ii=fc(1,iiii) ; jj=fc(2,iiii) ; kk=fc(3,iiii) ! three elements of the boundary triangle             
          if((ii.gt.0).and.(jj.gt.0).and.(kk.gt.0))then ! it is a "valid" boundary triangle       
          iii=node(vm(ii))%icel         
            if((iii.eq.node(vm(jj))%icel).and.(iii.eq.node(vm(kk))%icel))then ! if the triangle belongs to the same cell      
              cels(iii)%membt(:,cels(iii)%nmembt)=fc(1:3,iiii) 
              !!!!! it stores the area of the traingle
              ix=node(vm(jj))%x-node(vm(ii))%x ; iy=node(vm(jj))%y-node(vm(ii))%y 
              iz=node(vm(jj))%z-node(vm(ii))%z ; jx=node(vm(kk))%x-node(vm(ii))%x 
              jy=node(vm(kk))%y-node(vm(ii))%y ; jz=node(vm(kk))%z-node(vm(ii))%z
              kx=(iy*jz)-(iz*jy) ; ky=-1d0*((ix*jz)-(iz*jx)) ; kz=(ix*jy)-(iy*jx)       ! vectorial product !!!thus, the area is calculated
              mvn=sqrt(kx**2+ky**2+kz**2)   ! module of the vectorial product            
              cels(iii)%amembt(cels(iii)%nmembt,1)=kx/mvn 
              cels(iii)%amembt(cels(iii)%nmembt,2)=ky/mvn ;  cels(iii)%amembt(cels(iii)%nmembt,3)=kz/mvn               
              !!!!! nornal pointing outside ??
                d1=node(vm(fc(4,iiii)))%x-node(vm(ii))%x 
                d2=node(vm(fc(4,iiii)))%y-node(vm(ii))%y 
                d3=node(vm(fc(4,iiii)))%z-node(vm(ii))%z  
                adx=(d1*cels(iii)%amembt(cels(iii)%nmembt,1))+(d2*cels(iii)%amembt(cels(iii)%nmembt,2))&
                &+(d3*cels(iii)%amembt(cels(iii)%nmembt,3))
                adx=adx/sqrt(d1**2+d2**2+d3**2)
                ady=acos(adx)!*57.2957795
                if(ady.lt.pi/2)then
                  cels(iii)%amembt(cels(iii)%nmembt,1)=-1d0*cels(iii)%amembt(cels(iii)%nmembt,1)
                  cels(iii)%amembt(cels(iii)%nmembt,2)=-1d0*cels(iii)%amembt(cels(iii)%nmembt,2)
                  cels(iii)%amembt(cels(iii)%nmembt,3)=-1d0*cels(iii)%amembt(cels(iii)%nmembt,3)
                end if    
              !!!!!
              cels(iii)%nmembt=cels(iii)%nmembt+1
              do j=1,cels(iii)%nmemb           ! it checks whether the nodes were already "membrain-like" nodes of the cell "iii"
                if(ii.eq.cels(iii)%memb(j))then ; ii=0 ; endif 
                if(jj.eq.cels(iii)%memb(j))then ; jj=0 ; endif 
                if(kk.eq.cels(iii)%memb(j))then ; kk=0 ; endif 
                if(j.eq.cels(iii)%nmemb)then   ! it adds the new nodes
                  if(ii.ne.0)then;cels(iii)%memb(cels(iii)%nmemb)=ii ;node(vm(ii))%exte=1; 
                   cels(iii)%nmemb=cels(iii)%nmemb+1;endif 
                  if(jj.ne.0)then;cels(iii)%memb(cels(iii)%nmemb)=jj ;node(vm(jj))%exte=1; 
                   cels(iii)%nmemb=cels(iii)%nmemb+1;endif 
                  if(kk.ne.0)then;cels(iii)%memb(cels(iii)%nmemb)=kk ;node(vm(kk))%exte=1; 
                   cels(iii)%nmemb=cels(iii)%nmemb+1;endif 
                endif       
              end do      
            end if
          end if
      end if
    end do    

end subroutine membrane

!!!!!!!!!!!!!!!!!!!!!!

subroutine flow(i)   !!>>>Miguel 28-1-15 subroutine. Blastomere rotation (old version)
integer ::i
real*8  :: na,nb,nc,naa,nbb,ncc,naaa,nbbb,nccc,nddd,ndddd
       kkk=cels(i)%nmembt-1 !; write(*,*)i,'triangulossss:', kkk
       naaa=0d0 ; nbbb=0d0 ; nccc=0d0        
       do j=1,kkk 
         ii=cels(i)%membt(1,j) ; jj=cels(i)%membt(2,j) ; kk=cels(i)%membt(3,j)
         a=node(ii)%x ; b=node(ii)%y ; c=node(ii)%z
         aa=node(jj)%x ; bb=node(jj)%y ; cc=node(jj)%z
         aaa=node(kk)%x ; bbb=node(kk)%y ; ccc=node(kk)%z         
         na=(a+aa+aaa)/3d0 ; nb=(b+bb+bbb)/3d0 ; nc=(c+cc+ccc)/3d0  ! centro triangulo                   
         ndddd=sqrt((na-cels(i)%cex)**2+(nb-cels(i)%cey)**2+(nc-cels(i)%cez)**2) ! dist. centro-centroide
          !!!!!flow   (gira clockwise)
         nddd=cels(i)%amembt(j,1)
         cels(i)%amembt(j,1)=-1d0*cels(i)%amembt(j,2) ; cels(i)%amembt(j,2)=nddd  
         cels(i)%amembt(j,3)=cels(i)%cez-nc           
         !!!!!! z gradient actin        
         eee=1d0!/ndddd!eee*ndddd       
         cels(i)%amembt(j,1)=cels(i)%amembt(j,1)*eee ; cels(i)%amembt(j,2)=cels(i)%amembt(j,2)*eee 
         cels(i)%amembt(j,3)=cels(i)%amembt(j,3)*eee   ! efecto del eje z                    
         naaa=naaa+cels(i)%amembt(j,1) ; nbbb=nbbb+cels(i)%amembt(j,2) ; nccc=nccc+cels(i)%amembt(j,3)  !sumatorio
       end do
       nddd=sqrt(naaa**2+nbbb**2+nccc**2)       
       cels(i)%flow(1)=naaa/nddd ; cels(i)%flow(2)=nbbb/nddd ; cels(i)%flow(3)=nccc/nddd                    
      
end subroutine flow

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine smooth   !!>>>Miguel 28-1-15 subroutine. Smoothing of cells keeping cell's shape

 integer :: i,j,k,ii,jj,kk,n,nn,n1,n2,ierr,iii,kpar,w,ntrex,tope,seg
 real*8 :: nocop,vx,vy,vz,vd,dmax,kzoom ! to avoid coplanarity
 real*8,allocatable :: xyz1(:,:),xyz2(:,:),xyz3(:,:)
 integer:: maxbf,maxfc,nbf,nfc,nface,ntetra,npt,sizht   !size of arrays 
 real*8,allocatable :: vcl(:,:) !point coordinates
 integer,allocatable :: vm(:),ht(:),bf(:,:),fc(:,:),par2(:,:),trex(:,:) !point ind
!return
call membrane

do i=1,ncels  
  !write(*,*)'celula', i
  n=cels(i)%nunodes ; nn=0 ; dist1=0d0
  do j=1,n ; if(node(cels(i)%node(j))%exte.eq.1)then;nn=nn+1;endif ;end do
  if(allocated(xyz1))then;deallocate(xyz1);endif ; allocate(xyz1(nn,3)) ; nn=0
  if(allocated(xyz3))then;deallocate(xyz3);endif ; allocate(xyz3(nn,3))
  do j=1,n 
    if(node(cels(i)%node(j))%exte.eq.1)then ; nn=nn+1
      xyz1(nn,1)=node(cels(i)%node(j))%x ;  xyz1(nn,2)=node(cels(i)%node(j))%y ;  xyz1(nn,3)=node(cels(i)%node(j))%z 
    end if   
  end do      
  tope=3! ; if(nn.lt.40)then ; tope=3 ; endif !(manually assessed)  
  do w=1,tope      
    npt=nn ;     sizht=3*npt ;   maxfc=npt**2 ;     maxbf=npt**2
    if(allocated(vcl))then;deallocate(vcl,vm,bf,fc,ht);endif
    allocate(vcl(3,npt),vm(npt))
    allocate(bf(1:3,maxbf),fc(1:7,maxfc))
    allocate(ht(0:sizht-1))
    do kk=1,npt  !building the input arrays for the triangualtion function
      vcl(1,kk)=xyz1(kk,1) ; vcl(2,kk)=xyz1(kk,2) ; vcl(3,kk)=xyz1(kk,3)
      vm(kk)=kk
    end do      
    call dtriw3(npt, sizht, maxbf, maxfc, vcl, vm, nbf, nfc, nface, ntetra, bf, fc, ht, ierr)
    if(ierr/=0)then; print*,"error",ierr,getot ; endif
    if(allocated(trex))then;deallocate(trex,par2);endif
    allocate(trex(nfc,3),par2(sizht,2)) ; par2=0 ; par=0; kpar=1 ; trex=0; ntrex=0
    do j=1,nfc                             ! it passes through all boundary triangles     
      iiii=j
      if(fc(5,j).lt.0)then 
        ii=fc(1,iiii) ; jj=fc(2,iiii) ; kk=fc(3,iiii) ! three elements of the boundary triangle             
        if((ii.gt.0).and.(jj.gt.0).and.(kk.gt.0))then          
          if(ii.gt.jj)then;seg=ii;ii=jj;jj=seg;endif ! ordered elements
          if(jj.gt.kk)then;seg=jj;jj=kk;kk=seg;endif ! ordered elements 
          if(ii.gt.jj)then;seg=ii;ii=jj;jj=seg;endif ! ordered elements
          ntrex=ntrex+1 ; trex(ntrex,1)=ii ; trex(ntrex,2)=jj ; trex(ntrex,3)=kk          !;write(*,*)'trex',trex(ntrex,:)   
          do k=1,kpar ; if((ii.eq.par2(k,1)).and.(jj.eq.par2(k,2)))then;exit;endif 
          if(k.eq.kpar)then;par2(k,1)=ii;par2(k,2)=jj;kpar=kpar+1;endif ; end do ! primer edge
          do k=1,kpar ; if((jj.eq.par2(k,1)).and.(kk.eq.par2(k,2)))then;exit;endif 
          if(k.eq.kpar)then;par2(k,1)=jj;par2(k,2)=kk;kpar=kpar+1;endif ; end do ! segund edge
          do k=1,kpar ; if((ii.eq.par2(k,1)).and.(kk.eq.par2(k,2)))then;exit;endif 
          if(k.eq.kpar)then;par2(k,1)=ii;par2(k,2)=kk;kpar=kpar+1;endif ; end do ! tercer edge 
        end if
      end if
    end do  
    kpar=kpar-1 ; n1=0
    if(allocated(xyz2))then;deallocate(xyz2);endif;allocate(xyz2(kpar,3))!**2-kpar)/2,3))
    do j=1,kpar
          n1=n1+1 ; call random_number(nocop) ; nocop=(nocop-0.5d0)*1d-4 ! generation of new, non-coplanar points           
          xyz2(n1,1)=nocop+0.5d0*(vcl(1,par2(j,1))+vcl(1,par2(j,2)))     ! midpoint of the first edge
          xyz2(n1,2)=nocop+0.5d0*(vcl(2,par2(j,1))+vcl(2,par2(j,2)))     ! midpoint of the second edge
          xyz2(n1,3)=nocop+0.5d0*(vcl(3,par2(j,1))+vcl(3,par2(j,2)))     ! midpoint of the third edge                
    end do    
    if(allocated(xyz1))then;deallocate(xyz1);endif!;allocate(xyz1((kpar**2-kpar)/2,3));xyz1=xyz2;nn=(kpar**2-kpar)/2   
    allocate(xyz1(kpar,3));xyz1=xyz2;nn=kpar ;if(nn.gt.5000)then;goto 1351;endif
    if(w.eq.tope)then! guardar el vcl para las posiciones en una matriz de uso general, y los triangulos !!!
1351  if(allocated(cels(i)%membt2))then  ; deallocate(cels(i)%membt2); endif        
      if(allocated(cels(i)%amembt2))then  ; deallocate(cels(i)%amembt2); endif     
      allocate(cels(i)%membt2(ntrex,3))       ! external triangles      
      allocate(cels(i)%amembt2(3,npt));cels(i)%amembt2=vcl
      cels(i)%membt2(1:ntrex,:)=trex(1:ntrex,:)               ! external triangles
      cels(i)%nmembt2=ntrex                                  ! number of external triangles           
      dist2=0d0  !!!!!!!!!!!!!! re-escalo !!
      kzoom=0.5d0  
      do jj=1,npt
        vx=cels(i)%amembt2(1,jj)-cels(i)%cex ; vy=cels(i)%amembt2(2,jj)-cels(i)%cey ; vz=cels(i)%amembt2(3,jj)-cels(i)%cez    
        vd=sqrt(vx**2+vy**2+vz**2) ; vx=vx/vd ; vy=vy/vd ; vz=vz/vd        
        vx=vx*kzoom ; vy=vy*kzoom ; vz=vz*kzoom
        cels(i)%amembt2(1,jj)=cels(i)%amembt2(1,jj)+vx ; cels(i)%amembt2(2,jj)=cels(i)%amembt2(2,jj)+vy 
        cels(i)%amembt2(3,jj)=cels(i)%amembt2(3,jj)+vz 
        
      end do                 
      exit
    end if ! de w.eq.2 
  end do
end do

end subroutine smooth

end module neighboring
