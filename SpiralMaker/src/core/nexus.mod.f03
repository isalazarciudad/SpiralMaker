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




module nexus

  use general
  use genetic
  use mitosis

contains

!******************************************************************

! Declare the interface for POSIX fsync function

subroutine nexe

interface
  function fsync (fd) bind(c,name="fsync")
  use iso_c_binding, only: c_int
     integer(c_int), value :: fd
     integer(c_int) :: fsync
   end function fsync
end interface


  integer i,j,k,ii,jj,kk,ik,ikk,ick,ret
  real*8 a
  real*8 differ
  real*8 kplast,kvol,dvol
  character*300 cx
  real*8 :: minimos(32) !>>>Miguel 28-1-15 ! minimal z-coordinate value for each cell

! GENETIC REGULATION OF CELL BEHAVIOURS


  if(ffu(1)==0)then

    ! cell polarization  THIS ONE SHOULD BE THE FIRST IN HERE
    if (npag(nparam_per_node+8)>0) then ; call polarization ; end if

    !cell division (sincronica)
    if(getot.lt.1500)then
     ! if (npag(nparam_per_node+2)>0) then 
       if(mod(getot,500).eq.0)then 
       call should_I_divide        
      end if
    else
      if(mod(getot-100,500).eq.0)then 
        call should_I_divide ; 
      end if
    end if
    !WRITE(*,*)'SALEDIVNEXUS'
    ! change the size of the cell required for division
    if (npag(nparam_per_node+10)>0) then; call change_minsize_for_div ; end if
    ! change the maximal number of nodes per cell before the cell divides
    if (npag(nparam_per_node+15)>0) then; call change_maxsize_for_div ; end if  !>>> Is 23-3-14
  else 
    
  end if
  !WRITE(*,*)'ENTERAFFU4??'
  if (ffu(5)==1) then ; agex(:nd,1)=abs(node(:nd)%x) ;end if; !external source trick >>> Is 29-6-14
  if (ffu(5)==2) then ; agex(:nd,1)=node(:nd)%z**4    ;end if !external source trick miguel >>> Is 29-6-14

  if(ffu(5)==3)then !horizontal gradient (x-y plane) centered on the origin of coordinates
    do i=1,nd
      agex(i,6)=1/(1+(node(i)%x**2+node(i)%y**2)) !; print*,"i",i,"gex",agex(i,5)
    end do
  end if  
  !WRITE(*,*)'ENTERAFFU5'
  if(ffu(5)==4)then  !>>>Miguel 28-1-15 "if"                
        agex(:nd,2)=6d0*(node(:nd)%z+minval(node(:)%z))**2!/sqrt(node(:nd)%x**2+node(:nd)%y**2)                   
        agex(:nd,3)=0d0 
        if(ncels.eq.4)then;agex(cels(4)%node(1:cels(4)%nunodes),5)=1d2;endif
        !if(mod(getot,10).eq.0)then         
          !agex(:nd,7)=(node(:nd)%z+minval(node(:)%z))**(0.2d0*mod(ffu(29),50)) ! division por gradiente sin cambiar nada
          agex(:nd,7)=(node(:nd)%z+minval(node(:)%z))**(ffu(29)) ! division por gradiente sin cambiar nada !!! modifico          
          agex(:nd,4)=1d2 !; call membrane 
          minimos=0d0 
          do i=1,ncels
            minimos(i)=maxval(node(cels(i)%node(1:cels(i)%nunodes))%z)
            minimos(i)=minimos(i)-minval(node(cels(i)%node(1:cels(i)%nunodes))%z)
            minimos(i)=minval(node(cels(i)%node(1:cels(i)%nunodes))%z)+(0.25d0*minimos(i))
          end do
        !endif        
        do i=1,nd              
          if(node(i)%z.ge.minimos(node(i)%icel))then;agex(i,3)=1d2;endif  
           !if((ncels.ge.2).and.(mod(getot,10).eq.0))then                     
           ! kkk=node(i)%icel            
            !k=cels(kkk)%sister             
            !if(node(i)%exte.eq.1)then
            !  agex(i,4)=1d2
            !  do j=1,nneigh(i)                              
            !    jj=neigh(i,j)           
            !    kk=node(jj)%icel    
            !    d=sqrt((node(i)%x-node(jj)%x)**2+(node(i)%y-node(jj)%y)**2+(node(i)%z-node(jj)%z)**2)                             
            !    if((kkk.ne.kk).and.(d.lt.1d0))then;agex(i,4)=0d0;cycle;endif                              
            !  end do            
           ! end if ! de external
          !end if                                      
             kkk=node(i)%icel    ; k=0
             do j=1,nneigh(i)                            
              jj=neigh(i,j) 
              kk=node(jj)%icel               
              if((kk.eq.kkk).and.(dneigh(i,j).lt.2d0*node(i)%da))then;k=k+1;endif 
              if((j.eq.nneigh(i)).and.(k.le.3))then ; call random_number(d)                  
                node(i)%x=cels(kkk)%cex+0.1d0*d ; node(i)%y=cels(kkk)%cey+0.1d0*d ; node(i)%z=cels(kkk)%cez+0.1d0*d                     
              end if
             end do                     
        end do            
  end if
  !WRITE(*,*)'ENTERAFFU6'
  if (ffu(15)==1)then; !especial conditions, gene 1 is always expressing in the borders !>>Miquel12-5-14
    do i=1,nd
      if(node(i)%hold==1)then
        agex(i,1)=1d0   
      end if
    end do
  end if

  if (ffu(8)==1.and.nd>2) then   ! IS 23-4-13 this eliminates the nodes that get alone for too long
    ik=1
    do while(ik<=nd) !;print*,"node(",ik,")%talone=",node(ik)%talone
      if (node(ik)%talone>ttalone) then !;print*,ik,"entra mort",node(ik)%tipus        
          ikk=node(ik)%icel !;print*,"entra mesenq"                      
        ik=ik+1 ! Is it? >>> Is 16-1-14
      else
        ik=ik+1   ! I know, it is kind of funky but it should be this way, a loop with nd wont do because nd decreases because of apoptosis
      end if
    end do
  end if

! GENETIC REGULATION OF NODE PROPERTIES
  do i=1,nd         ! we update that parameter in each cell that expresses the gene
                    ! WE ONLY UPDATES DE NODES IN WHICH THE GENE IS EXPRESSED, OTHERWISE WE LEAVE IT IS AS IT WAS 
    if(node(i)%hold==1) cycle  !we don't want the border cells to perform behaviours because that would alter and possibly break the border  !>>>>Miquel9-1-14
    differ=1-node(i)%diffe !;print*,"differ",differ

    ! DIFFERENTIATION
    if (npag(25)>0) then
      ii=25;a=0.0; 
      do k=1,npag(ii) 
        kk=whonpag(ii,k) ; 
        if (gex(i,kk)>0.0d0) then ; 
          a=a+gex(i,kk)*gen(kk)%wa(ii)  
        endif
      end do
      node(i)%diffe=node(i)%diffe+a*delta
      if (node(i)%diffe>1.0) node(i)%diffe=1.0 
      if (node(i)%diffe<0.0d0) node(i)%diffe=0.0

      nodeo(i)%diffe=node(i)%diffe !>>Miquel17-9-14
    end if

    if (node(i)%tipus>2)then !this for mesenchyme and ECM

      if (npag(21)>0) then 
        ii=21 ; a=0
        do k=1,npag(ii) ; 
          kk=whonpag(ii,k) 
          if (gex(i,kk)>0.0d0) then ; 
            a=a+gex(i,kk)*gen(kk)%wa(ii)
          endif 
        enddo
        node(i)%reqc=nodeo(i)%reqc+a*differ*delta !wa in req-space units
      else
        node(i)%reqc=0
      end if

      a=node(i)%da-node(i)%req
      node(i)%req=node(i)%reqcr+node(i)%reqc  !now req is the sum of the req components: growth/apoptosis and contraction/deformation
      if(node(i)%req>df_reqmax) node(i)%req=df_reqmax !put an upper an lower boundary on how much  !>>Miquel28-7-14
      if(node(i)%req<reqmin) node(i)%req=reqmin !the req can be deformed
      node(i)%da=node(i)%req+a

      if (npag(6)>0) then;ii=6;a=0 ; do k=1,npag(ii) ; kk=whonpag(ii,k) ; if (gex(i,kk)>0.0d0) then ; 
      a=a+gex(i,kk)*gen(kk)%wa(ii) ;endif;enddo;node(i)%da=nodeo(i)%da+a*differ;if (node(i)%da<0) node(i)%da=0.0;end if
      if (npag(7)>0) then;ii=7;a=0 ; do k=1,npag(ii) ; kk=whonpag(ii,k) ; if (gex(i,kk)>0.0d0) then ; 
      a=a+gex(i,kk)*gen(kk)%wa(ii) ;endif;enddo;node(i)%you=nodeo(i)%you+a*differ;if (node(i)%you<0) node(i)%you=0.0;end if
      if (npag(8)>0) then;ii=8;a=0 ; do k=1,npag(ii) ; kk=whonpag(ii,k) ; if (gex(i,kk)>0.0d0) then ; 
      a=a+gex(i,kk)*gen(kk)%wa(ii) ;endif;enddo;node(i)%adh=nodeo(i)%adh+a*differ;if (node(i)%adh<0) node(i)%adh=0.0;end if
      if (npag(9)>0) then;ii=9;a=0 ; do k=1,npag(ii) ; kk=whonpag(ii,k) ; if (gex(i,kk)>0.0d0) then ; 
      a=a+gex(i,kk)*gen(kk)%wa(ii) ;endif;enddo;node(i)%rep=nodeo(i)%rep+a*differ;if (node(i)%rep<0) node(i)%rep=0.0;end if
      if (npag(10)>0) then;ii=10;a=0 ; do k=1,npag(ii) ; kk=whonpag(ii,k) ; if (gex(i,kk)>0.0d0) then ; 
      a=a+gex(i,kk)*gen(kk)%wa(ii) ;endif;enddo;node(i)%repcel=nodeo(i)%repcel+a;if (node(i)%repcel<0) node(i)%repcel=0.0;end if
      if (npag(11)>0) then;ii=11;a=0 ; do k=1,npag(ii) ; kk=whonpag(ii,k) ; if (gex(i,kk)>0.0d0) then ; 
      a=a+gex(i,kk)*gen(kk)%wa(ii) ;endif;enddo;node(i)%tor=nodeo(i)%tor+a*differ;if (node(i)%tor<0) node(i)%tor=0.0;end if
      if (npag(12)>0) then;ii=12;a=0 ; do k=1,npag(ii) ; kk=whonpag(ii,k) ; if (gex(i,kk)>0.0d0) then ; 
      a=a+gex(i,kk)*gen(kk)%wa(ii) ;endif;enddo;node(i)%stor=nodeo(i)%stor+a*differ;if (node(i)%stor<0) node(i)%stor=0.0;end if
      if (npag(13)>0) then;ii=13;a=0 ; do k=1,npag(ii) ; kk=whonpag(ii,k) ; if (gex(i,kk)>0.0d0) then ; 
      a=a+gex(i,kk)*gen(kk)%wa(ii) ;endif;enddo;node(i)%reqs=nodeo(i)%reqs+a*differ;if (node(i)%reqs<0) node(i)%reqs=0.0;end if
      if (npag(14)>0) then;ii=14;a=0 ; do k=1,npag(ii) ; kk=whonpag(ii,k) ; if (gex(i,kk)>0.0d0) then ; 
      a=a+gex(i,kk)*gen(kk)%wa(ii) ;endif;enddo;node(i)%ke=nodeo(i)%ke+a*differ;if (node(i)%ke<0) node(i)%ke=0.0;end if
      if (npag(15)>0) then;ii=15;a=0 ; do k=1,npag(ii) ; kk=whonpag(ii,k) ; if (gex(i,kk)>0.0d0) then ; 
      a=a+gex(i,kk)*gen(kk)%wa(ii) ;endif;enddo;node(i)%mo=nodeo(i)%mo+a*differ;if (node(i)%mo<0) node(i)%mo=0.0;end if
      if (npag(16)>0) then;ii=16;a=0 ; do k=1,npag(ii) ; kk=whonpag(ii,k) ; if (gex(i,kk)>0.0d0) then ; 
      a=a+gex(i,kk)*gen(kk)%wa(ii);endif;enddo;node(i)%dmo=nodeo(i)%dmo+a*differ;if (node(i)%dmo<0) node(i)%dmo=0.0;end if


    end if
  end do

  ! if a node gets 3 times the original size of node 1 we fucking kill the program
  if (mod(getot,100).eq.0) then !>>> Is 4-2-14
    if (ffu(7)==1) then  ! if a node gets twice its original size we fucking kill the program ! >>> Is 4-2-14
      do i=1,nd                                                                               ! >>> Is 4-2-14 
        if (node(i)%da>ramax) then                        ! >>> Is 4-2-14
          node(i)%da=ramax
        end if                                                                                ! >>> Is 4-2-14       
      end do                                                                                  ! >>> Is 4-2-14
    end if                                                                                    ! >>> Is 4-2-14
  end if                                                                                      ! >>> Is 4-2-14

! We check in here if all cells are already differentiated, if they are we stop the simulations

!>>> Is 4-4-14
if (ffu(10)==1) then
  do i=1,nd   
    if (node(i)%diffe<1.0d0) return
  end do
  print *,""
  print *," THIS IS THE END all cells are differentiated and then the simulations stop",trim(carg)//trim(noff),trim(nofi)
  print *,""
  call writesnap                                                                            !>>> Is 25-2-14
  if (len_trim(carg)/=0) then
    print *,trim(carg)//trim(noff),"ki"
    print *,trim(carg)//trim(nofi),"kii"
    open(23,file=trim(carg)//"t",iostat=i)
    print *,"making...",trim(carg)//"t"
    write(23,*,ERR=46) trim(carg)//trim(nofi)
    flush(23)
    ret = fsync(fnum(23))
          
    ! Handle possible error
    if (ret /= 0) stop "Error calling FSYNC"
    open(23,file=trim(carg)//"t",iostat=ii)
    print *,trim(carg)//"t",ii,"iostat"
    read(23,*,END=45,ERR=48) cx
print *,""
print *,cx,"cx HERE"
print *,""
    !close(23)
    call flush(23)
    print *,"done with iostat",ii
    cx="ls -alrt "//trim(carg)//"t" 
    call system(cx) 
  end if
  stop
45 print *,"end of file error"
  stop
46 print *,"error in writing",trim(carg)//"t"
  stop 
48 print *,"other end"
  stop
end if
!>>> Is 4-4-14

end subroutine

!*******************************************************************

subroutine cellbreak   !>>> 17-1-14
  real*8  ax,ay,az,ida
  integer doapo(nd)
  integer nocon(nd,nd)
  integer i,j,k,ii,jj,kk,iii,jjj,kkk,ik

  ! now we check that the cell is not split in two or more parts
  doapo=0
  nocon=0

  do i=1,ncels

    kkk=0
    do j=1,cels(i)%nunodes
      ii=cels(i)%node(j)
      if (node(ii)%marge==0) then ; kkk=ii ; ida=node(kkk)%da ; exit ; end if
    end do

    if (kkk==0) then !it means that the cell has no nucleus and then IT MUST DIE!!!!   !>>> Is 11-6-14
      do j=1,cels(i)%nunodes  !>>> Is 11-6-14
        ii=cels(i)%node(j)    !>>> Is 29-6-14
!print *,ii,i,j,cels(i)%node(:cels(i)%nunodes),ii,"ii",nd
        doapo(ii)=1           !>>> Is 11-6-14
      end do                  !>>> Is 11-6-14
      kkk=1  ! >>> Is 11-6-14
    else
      do j=1,cels(i)%nunodes
        ii=cels(i)%node(j)
        ax=node(ii)%x ; ay=node(ii)%y ; az=node(ii)%z
        if (sqrt((ax-node(kkk)%x)**2+(ay-node(kkk)%y)**2+(az-node(ii)%z)**2)<node(ii)%da+ida) then
          doapo(ii)=1
        end if
      end do
    end if   ! >>> Is 11-6-14

    doapo(kkk)=1
    do j=1,cels(i)%nunodes
      ii=cels(i)%node(j)
      ax=node(ii)%x ; ay=node(ii)%y ; az=node(ii)%z
      ida=node(ii)%da
      do jj=1,cels(i)%nunodes
        if (j==jj) cycle
        iii=cels(i)%node(jj)
        if (sqrt((ax-node(iii)%x)**2+(ay-node(iii)%y)**2+(az-node(iii)%z)**2)<node(iii)%da+ida) then
          nocon(ii,iii)=1
          nocon(iii,ii)=1
          if (doapo(ii)==1) then
            doapo(iii)=1
          else
            if (doapo(iii)==1) doapo(ii)=1
          end if
        end if
      end do
    end do

    do k=1,cels(i)%nunodes/2
      do j=1,cels(i)%nunodes
        ii=cels(i)%node(j)
        do jj=1,cels(i)%nunodes
          if (j==jj) cycle
          iii=cels(i)%node(jj)
          if (nocon(ii,iii)==1) then
            if (doapo(ii)==1) then
              doapo(iii)=1
            else
              if (doapo(iii)==1) doapo(ii)=1
            end if
          end if
        end do
      end do
    end do
  end do

  ik=1
  do while(ik<=nd)
    if (doapo(ik)==0) then      
      ik=ik+1
    else
      ik=ik+1   ! I know, it is kind of funky but it should be this way, a loop with nd wont do because nd decreases because of apoptosis
    end if
  end do


end subroutine

!********************************************************************

subroutine polarization
integer:: celd,nnod,tipi,ggr,ccen
real*8::a,b,c,d,e,ax,ay,az,bx,by,bz,cx,cy,cz,ix,iy,iz,alfa,s
      do celd=1,ncels
        tipi=cels(celd)%ctipus
        nnod=cels(celd)%nunodes        
        if (nnod==0) cycle      ! >>> Is 10-5-14
        iy=1d10 ; cx=0d0 ; cy=0d0 ; cz=0d0
	a=cels(celd)%cex ; b=cels(celd)%cey ; c=cels(celd)%cez   

        do i=1,nnod                                                     ! (gen) in the centroid (in the closest node)
          j=cels(celd)%node(i)
          if(node(j)%tipus==1.or.node(j)%tipus==3)then !in epithelial cells, polarity is planar, so we only take one layer of nodes >>>Miquel22-10-13
            d=sqrt((node(j)%x-a)**2+(node(j)%y-b)**2+(node(j)%z-c)**2)
          end if
          if(d.le.iy)then;iy=d;ccen=j;endif             
        end do   

        alfa=0.0d0                                                 ! concentration in the central node
        do k=1,npag(nparam_per_node+8)    
          kk=whonpag(nparam_per_node+8,k)
          if (gex(ccen,kk)>0.0d0) then
            alfa=alfa+gex(ccen,kk)*gen(kk)%wa(nparam_per_node+8)   ! wa in units of probability such that it makes things to go from 0 to 1
          end if
        end do  

        ix=0d0 ; iy=0d0 ; iz=0d0                                        ! vector of the gradient within a cell
        do i=1,nnod                                                     
            j=cels(celd)%node(i)
            if(node(j)%tipus==1.or.node(j)%tipus==3)then          
              d=sqrt((node(j)%x-a)**2+(node(j)%y-b)**2+(node(j)%z-c)**2)
              if (d<epsilod) cycle
              d=1d0/d                                                   ! module of radial vectors to get unitary vectors     
              s=0.0d0
              do k=1,npag(nparam_per_node+8)
                kk=whonpag(nparam_per_node+8,k)
                if (gex(j,kk)>0.0d0) then
                  s=s+gex(j,kk)*gen(kk)%wa(nparam_per_node+8)
                end if
              end do
              ix=ix+((node(j)%x-a)*d)*(s-alfa)                   ! and ignore shape/size effects
              iy=iy+((node(j)%y-b)*d)*(s-alfa)
              iz=iz+((node(j)%z-c)*d)*(s-alfa)
            end if
        end do

        if((ix.eq.0).and.(iy.eq.0).and.(iz.eq.0))then            ! if the gene has uniform expresion, the vector is random ! >>>Miguel1-7-14
          call random_number(a)                                  ! >>>Miguel1-7-14
          k=int(a*nvaloq)+1                                      ! >>>Miguel1-7-14
          cels(celd)%polx=particions_esfera(k,1)                 ! >>>Miguel1-7-14
          cels(celd)%poly=particions_esfera(k,2)                 ! >>>Miguel1-7-14
          cels(celd)%polz=particions_esfera(k,3)                 ! >>>Miguel1-7-14
        else                                                     ! >>>Miguel1-7-14
          a=ix**2+iy**2+iz**2 
          if(a==0)then
            cels(celd)%polx=0d0 ; cels(celd)%poly=0d0 ; cels(celd)%polz=0d0	! unitary resultant vector (gradient polarization)
          else
            d=1d0/sqrt(a)
            cels(celd)%polx=ix*d ; cels(celd)%poly=iy*d ; cels(celd)%polz=iz*d	! unitary resultant vector (gradient polarization)
          end if
          if((ix.eq.0d0).and.(iy.eq.0d0).and.(iz.eq.0d0))then                     ! miguel27-11-13
            cels(celd)%polx=0d0 ; cels(celd)%poly=0d0 ; cels(celd)%polz=0d0
          endif   ! miguel27-11-13
        endif                                                    ! >>>Miguel1-7-14
      end do
end subroutine

!*******************************************************************

subroutine polarizationisaac
integer i,j,k,ii,kk
real*8 a,b,c,s,sx,sy,sz,d,aa,bb,cc 

do i=1,ncels
  a=cels(i)%cex ; b=cels(i)%cey ; c=cels(i)%cez
  sx=0.0d0      ; sy=0.0d0      ; sz=0.0d0
 
    if (node(cels(i)%node(1))%tipus==3) then
      do j=1,cels(i)%nunodes
        ii=cels(i)%node(j)
        s=0.0d0
        do k=1,npag(nparam_per_node+8)
          kk=whonpag(nparam_per_node+8,k)
          if (gex(ii,kk)>0.0d0) then
            s=s+gex(ii,kk)*gen(kk)%wa(nparam_per_node+8)
          end if
        end do
        if (s/=0.0d0) then
          aa=node(ii)%x-a ; bb=node(ii)%y-b ; cc=node(ii)%z-c
          sx=sx+s*aa ; sy=sy+s*bb ; sz=sz+s*cc
        end if
      end do
      aa=sqrt(sx**2+sy**2+sz**2)
      if (aa>0.0d0) then  !epsilod) then
        d=1d0/aa
        cels(i)%polx=sx*d ; cels(i)%poly=sy*d ; cels(i)%polz=sz*d  
      else
        cels(i)%polx=0.0d0 ; cels(i)%poly=0.0d0 ; cels(i)%polz=0.0d0   
      end if
    end if

end do
end subroutine

!*************************************************************************************

subroutine change_minsize_for_div  ! updates the size required for dividint according to gene expression nparam_per_node+10
integer ick,j,k,ii,kk
real*8 a,b,c,s,sx,sy,sz,d 

do ick=1,ncels
  s=0.0d0
  do j=1,cels(ick)%nunodes
    ii=cels(ick)%node(j)
    do k=1,npag(nparam_per_node+10)
      kk=whonpag(nparam_per_node+10,k)
      if (gex(ii,kk)>0.0d0) then
        s=s+gex(ii,kk)*gen(kk)%wa(nparam_per_node+10)  !wa in units of number of nodes but it can be roughly understood as space-req units
      end if                                           ! THIS WA CAN BE NEGATIVE
    end do
  end do
  s=s*delta
print *,delta,"ACHTUNG"
!  if (s>0.0d0) then
    s=s/cels(ick)%nunodes !this way %minsize is independent of cell size !>>>>Miquel2-12-13
    cels(ick)%minsize_for_div=cels(ick)%minsize_for_div+s  !this can make a SUDDEN CHANGE
    if (cels(ick)%minsize_for_div<1) cels(ick)%minsize_for_div=1
!  end if
end do

end subroutine

!*************************************************************************************

!>>> Is 5-2-14
subroutine change_maxsize_for_div  ! updates the size required for dividint according to gene expression nparam_per_node+10
integer ick,j,k,ii,kk
real*8 a,b,c,s,sx,sy,sz,d 

do ick=1,ncels
  s=0.0d0
  do j=1,cels(ick)%nunodes
    ii=cels(ick)%node(j)
    do k=1,npag(nparam_per_node+15)
      kk=whonpag(nparam_per_node+15,k)
      if (gex(ii,kk)>0.0d0) then
        s=s+gex(ii,kk)*gen(kk)%wa(nparam_per_node+15)  ! THIS WA MAY BE NEGATIVE
      end if
    end do
  end do
  s=s*delta
!  if (s>0.0d0) then
    s=s/cels(ick)%nunodes !this way %minsize is independent of cell size !>>>>Miquel2-12-13
    cels(ick)%maxsize_for_div=cels(ick)%maxsize_for_div+s  !wa in units of space-req roughly this can make a SUDDEN CHANGE
    if (cels(ick)%maxsize_for_div<1) cels(ick)%maxsize_for_div=1
!  end if
end do

!>>> Is 5-2-14

end subroutine

!*******************************************************************************************************

subroutine diffusion_of_reqcr
  real*8 hreqcr(nd),hreqp(nd),hreqc(nd)
  integer i,j,k,ii,jj,kk
  real*8 a,b,c,d

  do i=1,nd
    a=0.0d0 ; b=0.0 ; c=0.0d0
    do ii=1,nneigh(i)
      k=neigh(i,ii)
      if (node(i)%icel/=node(k)%icel) cycle    ! only within the same cell
      if (node(i)%tipus/=node(k)%tipus) cycle  ! only within the same side of the cell
      if (node(i)%tipus>2) cycle               ! only for epithelial cells
      d=dneigh(i,ii)
      a=a+(node(k)%reqcr-node(i)%reqcr) !/(d+1d0)
    end do
    hreqcr(i)=a*dif_req ! >>> 11-6-14
  end do  
  do i=1,nd
    node(i)%reqcr=node(i)%reqcr+delta*hreqcr(i)
  end do
end subroutine

end module nexus
