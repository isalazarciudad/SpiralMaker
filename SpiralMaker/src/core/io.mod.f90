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

!modified by Isaac 29-4-2013

module io

  use general
  use genetic

  real*8 , public, allocatable :: param(:),pparam(:,:),paramo(:)  ! to record the parameters periodically
  real*8 , public, allocatable :: varglobal_out(:),pvarglobal_out(:,:) !maybe fuse with the params
  character*100 , public, allocatable :: names_param(:),names_varglobal_out(:),names_fu(:)
  integer, public               :: errorlec             ! it becomes 1 if there was a lecture error
  integer, public               :: freqsnap             ! frequency of saving data for movies

  !flags
  integer, public               :: fsnap                ! 1 if we are saving files periodically
  integer, public               :: fmovie               ! 1 if we are saving images periodically
  integer, public               :: flabel               ! 1 if we are adding a label to the file name
  integer, public               :: fappend              ! 1 if we add data append in the file
  integer, public               :: frappend              ! 1 if we add data append in the file
  integer, public               :: ffinal               ! 1 if we want to write only the last interation
  character*35,public           :: label                ! the label to add as such
  integer, parameter            :: mamax=2           ! since of the buffer to save previous iterations
  integer, public               :: itvi,itviactual      !actual number of sub iterations run
  integer, private              :: tofet

  type(nod), public, allocatable:: pnode(:,:)   !Attention this needs to be resized when three is growth or mitosis 
  character*35                  :: nomfit
  character*24,public, allocatable     :: nodeparams(:)
  character*16                  :: cr             ! this is the right format for the output and dynamically considers nparam_per_node
  character*11                  :: rowfmt,rowfmta ! this is the right format for the output and dynamically considers nparam_per_node
  character*40 nofi
  character*75 noff
  character*300, public :: carg
  character*120, public :: nomfinal

  !variables for input config file  !>>Miquel8-9-14
  real*4,public  :: conf_shiftx,conf_shifty,conf_shiftz
  !real*4,public  :: conf_lookfromx,conf_lookfromy,conf_lookfromz
  real*4,public  :: conf_anglex,conf_angley,conf_custom_max,conf_custom_min
  integer :: conf_colorselection,conf_custom_colorselection,conf_rainbow,conf_chogen
  integer :: ic_load
  integer,dimension(40)::conf_flag !>>Miquel2-10-14

  real*4,public  :: conf_custom_amaxval,conf_custom_aminval,conf_custom_smaxval,conf_custom_sminval  !>>Miquel5-11-14
  real*4,public  :: conf_custom_arrowscale,conf_custom_spherescale                                   !>>Miquel5-11-14
  integer :: conf_arrowselection,conf_custom_arrowselection,conf_sphereselection,conf_custom_sphereselection  !>>Miquel5-11-14
  integer,allocatable::conf_oopp(:) !>>Miquel5-11-14
  integer::conf_select_what,conf_nki !>>Miquel5-11-14

contains

!**************************************************************
  subroutine iniio

    if (allocated(param)) deallocate(param)
    allocate(param(nparam))

    if (allocated(paramo)) deallocate(paramo)
    allocate(paramo(nparam))

    if (allocated(pparam)) deallocate(pparam)
    allocate(pparam(mamax,nparam))

    if (allocated(names_param)) deallocate(names_param)
    allocate(names_param(nparam))

    if (allocated(varglobal_out)) deallocate(varglobal_out)
    allocate(varglobal_out(nvarglobal_out))

    if (allocated(pvarglobal_out)) deallocate(pvarglobal_out)
    allocate(pvarglobal_out(mamax,nvarglobal_out))

    if (allocated(names_varglobal_out)) deallocate(names_varglobal_out)
    allocate(names_varglobal_out(nvarglobal_out))

    if (allocated(names_fu)) deallocate(names_fu)
    allocate(names_fu(nfu))

    if (allocated(nodeparams)) deallocate(nodeparams)
    allocate(nodeparams(nparam_per_node))

    urv=1d0/rv 
    if (allocated(pnode)) deallocate(pnode)
    allocate(pnode(mamax,nda))
    pnode(1,:)=node(:)

    if (allocated(nodeo)) deallocate(nodeo)
    allocate(nodeo(nda))
    nodeo=node   !Is 25-5-13 this is just to save the initial conditions

    call s_names_fu
    call s_names_param
    call s_names_varglobal_out
    call s_names_nodeparams

    pparam=0         
    errorlec=0
    fsnap=0
    freqsnap=1000
    flabel=0
    ffinal=0
    fappend=0
    tofet=0
 
    call iniread
 
end subroutine

!*****************************************************************************

subroutine iniread
    character*2  cd
    character*1  cu

    cr="(20es24.16,5I16)"
    write (cd,'(I2)') nparam_per_noder  !this is funky, I write a text into a character variable that then use as format
    cr(2:3)=cd
    write (cu,'(I1)') nparam_per_nodei
    cr(12:12)=cu

    !now the same for ng the number of genes
    rowfmt="(10es24.16)"
    if (ng>9) then
      write (cd,'(I2)') ng !this is funky, I write a text into a character variable that then use as format
      rowfmt(2:3)=cd
    else
      write (cu,'(I1)') ng !this is funky, I write a text into a character variable that then use as format
      rowfmt(2:2)=" "
      rowfmt(3:3)=cu
    end if

    !now the same for nga the number of properties
    rowfmta="(  es24.16)"
    if (nga>9) then
      write (cd,'(I2)') nga !this is funky, I write a text into a character variable that then use as format
      rowfmta(2:3)=cd
    else
      write (cu,'(I1)') nga !this is funky, I write a text into a character variable that then use as format
      rowfmta(2:2)=" "
      rowfmta(3:3)=cu
    end if
  end subroutine

!**************************************************************
  subroutine get_param_from_matrix(para)
    real*8 para(nparam)

    getot=para(1)
    temp=para(2)
    nparti=para(3)
    desmax=para(4)
    mmae=para(5)
    dmax=para(6)
    rv=para(7)
    min_comp=para(8)
    ntipusadh=para(9)
    nd=para(10)
    resmax=para(11)
    prec=para(12)
    ndmax=para(12)
    rtime=para(13)
    ttalone=para(14)
    reqmin=para(15)
    ecmmax=para(16)
    deltamax=para(17)
    ncels=para(18)
    ng=para(19)
    nodecel=para(20)
    prop_noise=para(21)
    !khold=para(25)             !>>>Miquel9-1-14
    mnn=para(22)
    ramax=para(23)
    prec=para(24)   
    deltamin=para(25)  ! >>> Is 29-4-14
    dif_req=para(26)   ! >>> Is 11-6-14
    screen_radius=para(27) !>>Miquel28-7-14
    df_reqmax=para(28) !>>Miquel28-7-14
    angletor=para(29) !>>Miquel15-9-14
    ldi=para(30) !>>Miquel23-10-14

!    if (allocated(kadh)) deallocate(kadh)       !x>>>>Miquel14-11-13
!    allocate(kadh(ntipusadh,ntipusadh))
!    do i=1,ntipusadh
!      do j=1,ntipusadh
!        kadh(i,j)=para(23+i+j)
!      end do
!    end do
    call get_varglobal_from_matrix

  end subroutine

!**************************************************************
  subroutine get_param_from_matrix_read(para)
    real*8 para(nparam)

    getot=para(1)
    temp=para(2)
    nparti=para(3)
    desmax=para(4)
    mmae=para(5)
    dmax=para(6)
    rv=para(7)
    min_comp=para(8)
    ntipusadh=para(9)
    nd=para(10)
    resmax=para(11)
    ndmax=para(12)
    rtime=para(13)
    ttalone=para(14)
    reqmin=para(15)
    ecmmax=para(16)
    deltamax=para(17)
    ncels=para(18)
    ng=para(19)
    nodecel=para(20)
    prop_noise=para(21)
    !khold=para(25)           !>>>Miquel9-1-14
    mnn=para(22)
    ramax=para(23)
    prec=para(24)  !>>Miquel20-3-14
    deltamin=para(25)!>> IS 29-4-14
    dif_req=para(26) ! >>> Is 11-6-14
    screen_radius=para(27) !>>Miquel28-7-14
    df_reqmax=para(28) !>>Miquel28-7-14
    angletor=para(29) !>>Miquel15-9-14
    ldi=para(30) !>>Miquel23-10-14

    call get_varglobal_from_matrix

  end subroutine

!**************************************************************

  subroutine get_varglobal_from_matrix
    itacc=varglobal_out(1)
  end subroutine

!**************************************************************

  subroutine put_varglobal_to_matrix
    varglobal_out(1)=itacc
  end subroutine

!**************************************************************

  subroutine put_param_to_matrix(para)
    real*8 para(nparam)

    para(1)=getot
    para(2)=temp
    para(3)=nparti
    para(4)=desmax
    para(5)=mmae
    para(6)=dmax
    para(7)=rv
    para(8)=min_comp
    para(9)=ntipusadh
    para(10)=nd
    para(11)=resmax
    para(12)=ndmax
    para(13)=rtime
    para(14)=ttalone
    para(15)=reqmin
    para(16)=ecmmax
    para(17)=deltamax
    para(18)=ncels
    para(19)=ng
    para(20)=nodecel
    para(21)=prop_noise
    !para(25)=khold          !>>>Miquel9-1-14
    para(22)=mnn
    para(23)=ramax
    para(24)=prec
    para(25)=deltamin ! >>> Is 25-5-14
    para(26)=dif_req  ! >>> Is 25-5-14
    para(27)=screen_radius  !>>Miquel28-7-14
    para(28)=df_reqmax  !>>Miquel28-7-14
    para(29)=angletor   !>>Miquel15-9-14
    para(30)=ldi

    call put_varglobal_to_matrix
  end subroutine

!***************************************************************

  subroutine s_names_param
    character*12 cf,cg
    names_param=" nothing"
    names_param(1)="Number of iterations run"
    names_param(2)="M(TEM)        : Physical: temperature or 1/temperature"
    names_param(3)="nparti        : Mathematical: number of parititions in the 3D random number ball: TECHNICAL"
    names_param(4)="desmax (not really a param)"
    names_param(5)="M(MAE)        : Mathematical: pEQD all nodes in a cell should have before adding a new node"  
    names_param(6)="M(DIF)        : Numerical   : maximal distance at which to consider diffusion, in number of node radius(da)"
    names_param(7)="rv            : Mathematical: size of the cubes in the grid: TECHNICAL(not a param since it's dynamic)"
    names_param(8)="M(MCO)        : Biological : compression p node"! in a cell to allow to add new nodes: it should be negative and small"
    names_param(9)="number of adh molecules"
    names_param(10)="Total number of nodes"
    names_param(11)="M(RMA)       : Mathematical: maximal displacement per iteration"
    names_param(12)="M(MAN)       : Mathematical: maximal number of nodes allowed: it is only used in L2=1"
    names_param(13)="real time (a variable)"
    names_param(14)="ttalone      : Biological  : time a node can be alone before dying" 
    names_param(15)="M(EMI) & M(MID)?   : the req a newly added node has at least" 
    names_param(16)="M(ECM)       : the amount of matter an extracel node should have before being released" 
    names_param(17)="M(DMA)       : the maximal dynamic delta allowed: no major effect" 
    names_param(18)="Total number of cells" 
    names_param(19)="Total number of genes" 
    names_param(20)="nocedel      : Biological: initial number of nodes per cell"
    names_param(21)="M(NOI)       : Numerical : proportion of nodes subject to noise per dif eq iteration"
    names_param(22)="M(MNN)       : Numerical  : Maximal number of nodes that can interact with a node: if more then crash"
    names_param(23)="M(EMA)       : Numerical  : Maximal radius a node can have, we do not allow more"
    names_param(24)="M(DDA)       : mathematical: accuracy when using the adaptive step: it is only used if L19=1"
    names_param(25)="M(DMI)       : Numerical  : we do not allow a delta smaller than that"
    names_param(26)="dif_req      : Biological : diffusion coefficient of req "
    names_param(27)="M(GAB)       : Numerical : intensity of screening between nodes: 0 = no screening, 1 = full screening"
    names_param(28)="M(DFE)       : Numerical : maximal req value allowed by epithelial deformation"
    names_param(29)="M(AMX)       : Biological: minimum angle for surface tensiob forces to be applied"
    names_param(30)="M(dummy)     : Dummy parameter, does nothing" 
 
  end subroutine

!***************************************************************

  subroutine s_names_varglobal_out
    names_varglobal_out=" nothing"
    names_varglobal_out(1)=" itacc    :  Number of accepted iterations"
  end subroutine

!***************************************************************

  subroutine s_names_fu
    names_fu=""
    if (nfu>0) names_fu(1)="L1 treat each spherical node as a cell and each cylinder as a cell"
    if (nfu>1) names_fu(2)="L2 to stop when there are too many cells"
    if (nfu>2) names_fu(3)="L3 0 no screening, 1 screening by Gabriel method"
    if (nfu>3) names_fu(4)="L4 torsion: scalar product between within cell ellipses"
    if (nfu>4) names_fu(5)="nothing" !external source
    if (nfu>5) names_fu(6)="nothing" !eggshell
    if (nfu>6) names_fu(7)="L5 stop the simulation if a node gets to 3 times the original size of node 1:both for req and da" !>>> Is 4-2-14
    if (nfu>7) names_fu(8)="L6 this forces apoptosis of all the nodes or cells that lose contact with others"
    if (nfu>8) names_fu(9)="L7 0, for Euler numerical int., 1 for Runge-Kutta order 4 for movement, 2 R-K also for genetics"
    if (nfu>9) names_fu(10)="L8 stop the simulation if all nodes are differentiated"
    if (nfu>10) names_fu(11)="L9 epithelial node plastic deformation"
    if (nfu>11) names_fu(12)="L10 dynamic delta (0) / fixed delta (1)"
    if (nfu>12) names_fu(13)="L11 neighboring algorithm: exhaustive by %da sphere (0) / by 3D triangulation (1)"
    if (nfu>13) names_fu(14)="nothing" !physical boundaries (walls)"
    if (nfu>14) names_fu(15)="nothing"
    if (nfu>15) names_fu(16)="L12 apical-apical interaction: 0 original, 1 new"
    if (nfu>16) names_fu(17)="L13 volume conservation in cylinders"
    if (nfu>17) names_fu(18)="L14 diffusion of reqcr"
    if (nfu>18) names_fu(19)="L15 1 for adaptive size step (it uses Runge-Kutta), 2 to make for genetics too"
    if (nfu>19) names_fu(20)="L16 this allows growth to add more than one node per cell per iteration"
    if (nfu>20) names_fu(21)="L17 return the control in real time(1) or in real iterations(0),&
                              & only applies if  ffu(12)=0; dynamic delta"
    if (nfu>21) names_fu(22)="L18 random noise mode: 0 = biased random noise by energies , 1 = unbiased random noise" !>>Miquel28-7-14
    if (nfu>22) names_fu(23)="L19 forces by diff. equations: 0 = activated, 1 = disabled (should go by energies)" !>>Miquel28-7-14
    if (nfu>23) names_fu(24)="Rotation"
    if (nfu>24) names_fu(25)="Sach"
    if (nfu>25) names_fu(26)="Hertwig"
    if (nfu>26) names_fu(27)="Isaac"
    if (nfu>27) names_fu(28)="Free"
    if (nfu>28) names_fu(29)="Assym" 
  end subroutine

!***************************************************************

 subroutine s_names_nodeparams
    nodeparams=""
    nodeparams(1)="x"!
    nodeparams(2)="y"!
    nodeparams(3)="z"!
    nodeparams(4)="U(Energy)"!"e"
    nodeparams(5)="p^EQD" !"req"
    nodeparams(6)="p^ADD" !"da"
    nodeparams(7)="p^YOU" !
    nodeparams(8)="p^ADH" !
    nodeparams(9)="p^REP" !
    nodeparams(10)="p^REC"!"repcel"
    nodeparams(11)="p^EST"!"tor"
    nodeparams(12)="p^ERP"!"stor"
    nodeparams(13)="p^EQS"!"reqs"
    nodeparams(14)="p^HOO"!"ke"
    nodeparams(15)="p^MOV"!"mo"
    nodeparams(16)="p^DMO"!"dmo"
    nodeparams(17)="x(0)" !"orix"
    nodeparams(18)="y(0)" !"oriy"
    nodeparams(19)="z(0)" !"oriz"
    nodeparams(20)="p^ECM"!"acecm"   
    nodeparams(21)="p^COD"!"reqc" 
    nodeparams(22)="p^GRD"!"reqcr"   
    nodeparams(23)="p^PLD"!"reqp"   
    nodeparams(24)="p^VOD"!"reqv"    
    nodeparams(25)="p^DIF"!"diffe"    
    nodeparams(26)="p^KFI"!"khold"   
    nodeparams(27)="p^PLA"!"kplast"   
    nodeparams(28)="p^VOC"!"kvol"     
    !nodeparams(28)="temt"
    nodeparams(29)="type" !"tipus"      ?????  
    nodeparams(30)="cell" !"icel"       ?????
    nodeparams(31)="other"!"altre"     ?????
    nodeparams(32)="marge"
    nodeparams(33)="talone"
    nodeparams(34)="p^FIX"!"hold"        
    nodeparams(35)="P^BOR"!"border"
    nodeparams(36)="EXTE" !>>>Miguel 28-1-15
  end subroutine

!***************************************************************

  subroutine writesnap
    logical::L_EXISTS	!>>>>Miquel 25-4-13

    inquire(file="./output/", EXIST=L_EXISTS)
    if(.not. L_EXISTS)then
!      print*,"./output/ no existeix"
      call system("mkdir output")
    end if

    if (len_trim(carg)==0.or.carg=="0") then
      call system("mkdir output/"//caa)
      nomfit=caa//cazero(3:)//cae//caf//cag
    else
      nomfit(1:len(nomfit))=carg(1:len(nomfit))
    end if
    do i=1,len(nomfit)
      if (nomfit(i:i)==" ") nomfit(i:i)="_"
    end do
    do i=1,len(nomfit)
      if (nomfit(i:i)==":") nomfit(i:i)="_"
    end do

    write (nofi,*) getot
    nofi=adjustl(nofi)
    nofi(len_trim(nofi)+1:len_trim(nofi)+5)=".dat"

    if (fappend==0) then
      if (ffinal==0) then
        if (flabel==1) then
          noff=trim(label)//trim(nofi)
          if (len_trim(carg)==0.or.carg=="0") then
            open(1,file=trim(nomfit)//trim(noff),iostat=i)
          else
            open(1,file=trim(carg)//trim(noff),iostat=i)
          end if
        else
          if (len_trim(carg)==0.or.carg=="0") then
            open(1,file=trim(nomfit)//trim(nofi))
          else
            open(1,file=trim(carg)//trim(nofi),iostat=i)
          end if
        end if
      else
        if (len_trim(carg)==0.or.carg=="0") then
          open(1,file=trim(nomfit)//".final")
        else
          open(1,file=trim(carg)//".final")
        end if
      end if
    else
      if (len_trim(carg)==0.or.carg=="0") then
        open(1,file="final"//trim(nomfit)//"append.dat",access='append')
      else
        open(1,file="final"//trim(carg)//"append.dat",access='append')
      end if
    end if
    call put_param_to_matrix(param)
    call writeassuch(param,nodeo) !>>Miquel17-9-14

    inquire(unit=1,name=nomfinal)
    if (len_trim(carg)==0.or.carg=="0") then
      call system("mv "//nomfinal//" output/"//caa)
    end if
    close(1)
    tofet=1

  end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine writesnapini  !>>>>> by Miquel18-11-13  this sub only writes the output file with initial conditions
    logical::L_EXISTS	!>>>>Miquel 25-4-13

    inquire(file="./output/", EXIST=L_EXISTS)
    if(.not. L_EXISTS)then
!      print*,"./output/ no existeix"
      call system("mkdir output")
    end if

    if (len_trim(carg)==0.or.carg=="0") then
      call system("mkdir output/"//caa)
      nomfit=caa//cazero(3:)//cae//caf//cag
    else
      nomfit(1:len(nomfit))=carg(1:len(nomfit))
    end if
    do i=1,len(nomfit)
      if (nomfit(i:i)==" ") nomfit(i:i)="_"
    end do
    do i=1,len(nomfit)
      if (nomfit(i:i)==":") nomfit(i:i)="_"
    end do

    write (nofi,*) getot
    nofi=adjustl(nofi)
    nofi(len_trim(nofi)+1:len_trim(nofi)+5)=".dat"
    if (len_trim(carg)==0.or.carg=="0") then  !this means we did not have input file
      if (tofet==0) then
        if (fappend==0) then
          if (ffinal==0) then
            if (flabel==1) then
              noff=trim(label)//trim(nofi)
              open(1,file=trim(nomfit)//trim(noff)//"_0.dat",iostat=i)
            else
              open(1,file=trim(nomfit)//"_0.dat")
              open(2,file="name.dat")
              write(2,*) "output/"//caa//"/"//trim(nomfit)//"_0.dat"
              close(2)
            end if
          else
            open(1,file=trim(nomfit)//".final_0.dat")
          end if
        else
          open(1,file=trim(nomfit)//"_0.final_append.dat",access='append')
        end if
        call put_param_to_matrix(paramo)
        call writeassuch(paramo,nodeo)

        if (len_trim(carg)==0.or.carg=="0") then        
          inquire(unit=1,name=nomfinal)
          call system("mv "//nomfinal//" output/"//caa) 
          close(1)
        end if
      end if
    end if
    tofet=1



return
    if (fappend==0) then
      if (ffinal==0) then
        if (flabel==1) then
          noff=trim(label)//trim(nofi)
          if (len_trim(carg)==0.or.carg=="0") then
            open(1,file=trim(nomfit)//trim(noff),iostat=i)
          else
            open(1,file=trim(carg)//trim(noff),iostat=i)
          end if
        else
          if (len_trim(carg)==0.or.carg=="0") then
            open(1,file=trim(nomfit)//trim(nofi))
          else
            open(1,file=trim(carg)//trim(nofi),iostat=i)
          end if
        end if
      else
        if (len_trim(carg)==0.or.carg=="0") then
          open(1,file=trim(nomfit)//".final")
        else
          open(1,file=trim(carg)//".final")
        end if
      end if
    else
      if (len_trim(carg)==0.or.carg=="0") then
        open(1,file="final"//trim(nomfit)//"append.dat",access='append')
      else
        open(1,file="final"//trim(carg)//"append.dat",access='append')
      end if
    end if

    call get_param_from_matrix(param)
    if (eva==1) then
      call writeassuch(param,nodeo)
    else
      call writepara(param)
    end if 

    inquire(unit=1,name=nomfinal)
    if (len_trim(carg)==0.or.carg=="0") then
      call system("mv "//nomfinal//" output/"//caa)
    end if
    close(1)
    tofet=1

  end subroutine

!**********************************************************************************************

  subroutine writeassuch(para,nodi)  !with nodeo
    character*30 nofi
    integer i,j
    real*8 para(nparam)
    type(nod) nodi(nda)    


    node(:)%e=0.0d0

    write (1,*) "THIS FILE WAS WRITTEN IN THE FORMAT OF THE ",version," VERSION"
    write (1,*) winame
    write (1,*) nparam,"number of node parameters"
    write (1,*) nvarglobal_out,"number of global variables"
    write (1,*) 
    write (1,*) nfu,"functions"
    write (1,*) 
    do i=1,nfu
      write (1,*) ffu(i),names_fu(i)
    end do
    write (1,*) 
    write (1,*) "parameters"
    write (1,*) 
    do i=1,nparam
      write (1,"(I2,es24.16,A1,A100)") i,para(i)," ",names_param(i)
    end do
    write (1,*) 
    write (1,*) "random seed at the first iteration"
    write (1,*) 
    write (1,*) idumoriginal
    write (1,*) 
    write (1,*) "random seed at this iteration"
    write (1,*) 
    call random_seed(get=idum)
    write (1,*) idum
    write (1,*) 
    write (1,*) "global output variables"
    write (1,*) 
    do i=1,nvarglobal_out
      write (1,"(es24.16,A100)") varglobal_out(i),names_varglobal_out(i)
    end do

    !genetic information
    if (ng>0) then 
      write (1,*) 
      write (1,*) ng,"genes"
      write (1,*) 
      write (1,*) "W matrix: gene    1      gene 2 etc..."
      write (1,*) 
      do i=1,ng
        write(1,fmt="(A3,I4,"//rowfmt(2:11))  "gene",i,gen(i)%w       !!!!!!!W!!!!!!!!!!!!!!
      end do
      write (1,*) 
      write (1,*) "R matrix: gene"
      write (1,*) 
      do i=1,ng
        write(1,*) i,gen(i)%nww
        if (gen(i)%nww>0) then
          write(1,*)  gen(i)%ww(:gen(i)%nww,1) !!!!!!!WW!!!!!!!!!!!!!
          write(1,*)  gen(i)%ww(:gen(i)%nww,2) !!!!!!!WW!!!!!!!!!!!!!
          do j=1,gen(i)%nww
            write(1,fmt="(es24.16)",ADVANCE='NO') gen(i)%ww(j,3) !!!!!!!WW!!!!!!!!!!!!!
          end do
          write(1,*) ""
        end if
      end do
      write (1,*) 
      write (1,*) "E and C matrix: node prop 1 node prop 2 etc..."
      write (1,*) 
      do i=1,ng
        write(1,fmt="(A3,I4,"//rowfmta(2:11))  "gene",i,gen(i)%wa     !!!!!!!WA!!!!!!!!!!!!!!
      end do
      write (1,*) 
      write (1,*) "other gene characteristics"
      write (1,*) 
      do i=1,ng
        write(1,*) "gene",i !,gen(i)%label
        write(1,'(es24.16,A12)') gen(i)%diffu," diffusivity"
        write(1,'(es24.16,A17)') gen(i)%mu,"degradation rate"
        write(1,'(es24.16,A36)') gen(i)%kindof,"type, 0 TF, 1 modofiable FT, 2 form"
        write(1,'(I4,A20)') gen(i)%npre," number of pre forms"
        if (gen(i)%npre/=0) write(1,*) gen(i)%pre
        write(1,'(I4,A20)') gen(i)%npost," number of post forms"
        if (gen(i)%npost/=0) write(1,*) gen(i)%post
        write (1,*) 
      end do
      if(ntipusadh>0)then                                                 !>>>>Miquel14-11-13
        write (1,*) "B matrix: adhesion molecules"    !!!!!KADH!!!!!!!!!
        do i=1,ntipusadh             
          write(1,fmt="(A3,I4,"//rowfmt(2:11))  "adh",i,kadh(i,:ntipusadh)!>>>Is 12-1-13
        end do                                                            !
      end if                                                              !
      write (1,*)
      write (1,*) "G matrix: gene expression"
      write (1,*)
      write (1,*) "node  gene 1                 gene 2    etc..."
      do i=1,nd
        write(1,fmt="(I6,"//rowfmt(2:11)) i,gex(i,:)   !gex matrix   
      end do
    end if
    write (1,*) 
    write (1,*) "node properties"
    write (1,*) 
    write (1,*) nodeparams
    do i=1,nd  !THIS PART WILL NEED TO BE CHANGED EVERY TIME WE CHANGE WHAT'S IN NODE TYPE
       write(1,cr) node(i)
    end do
    write (1,*) 
    write (1,*) "node properties at time 0 (nodeo)"
    write (1,*) 
    write (1,*) nodeparams
    do i=1,nd  !THIS PART WILL NEED TO BE CHANGED EVERY TIME WE CHANGE WHAT'S IN NODE TYPE
       write(1,cr) nodeo(i)
    end do
    write (1,*) 
    write (1,*) "cell properties"
    write (1,*) 
    write (1,*) ncels,"number of cells"
    write (1,*) 
    do i=1,ncels
      write(1,*) "cell",i
      write(1,"(es24.16,A12)") 0.00
      write(1,"(es24.16,A50)") cels(i)%minsize_for_div,"minimal number of nodes to be able to divide"
      write(1,"(es24.16,A50)") cels(i)%maxsize_for_div,"max number of nodes before forced division"   !>>> Is 5-2-14
      write(1,"(3es24.16,A31)") cels(i)%cex,cels(i)%cey,cels(i)%cez," centroid x,y and z coordinated"
      write(1,"(3es24.16,A43)") cels(i)%polx,cels(i)%poly,cels(i)%polz,"polarization vectors x, y and z components"
      write(1,"(es24.16,A13)") cels(i)%fase," cell's phase"
      write(1,"(es24.16,A13)") cels(i)%temt," counter for epithelial-mesenchymal transition"

      write (1,*) cels(i)%nunodes,"number of nodes in a cell"
      write (1,*) cels(i)%nodela,"actual size of the cels(i)%node matrix"
      write (1,*) cels(i)%ctipus,"cell type"
      write (1,*) cels(i)%sister,"cell sister"!>>>Miguel 28-1-15   
      write (1,*) "list of the nodes in the cell"
      write (1,*) cels(i)%node(:cels(i)%nunodes)
      write (1,*) 
    end do
  end subroutine

!**********************************************************************************************

  subroutine writeassuchold(para,nodi) !without nodeo
    character*30 nofi
    integer i,j
    real*8 para(nparam)
    type(nod) nodi(nda)    


    node(:)%e=0.0d0

    write (1,*) "THIS FILE WAS WRITTEN IN THE FORMAT OF THE ",version," VERSION"
    write (1,*) winame
    write (1,*) nparam,"number of node parameters"
    write (1,*) nvarglobal_out,"number of global variables"
    write (1,*) 
    write (1,*) nfu,"functions"
    write (1,*) 
    do i=1,nfu
      write (1,*) ffu(i),names_fu(i)
    end do
    write (1,*) 
    write (1,*) "parameters"
    write (1,*) 
    do i=1,nparam
      write (1,"(I2,es24.16,A1,A100)") i,para(i)," ",names_param(i)
    end do
    write (1,*) 
    write (1,*) "random seed at the first iteration"
    write (1,*) 
    write (1,*) idumoriginal
    write (1,*) 
    write (1,*) "random seed at this iteration"
    write (1,*) 
    call random_seed(get=idum)
    write (1,*) idum
    write (1,*) 
    write (1,*) "global output variables"
    write (1,*) 
    do i=1,nvarglobal_out
      write (1,"(es24.16,A100)") varglobal_out(i),names_varglobal_out(i)
    end do

    !genetic information
    if (ng>0) then 
      write (1,*) 
      write (1,*) ng,"genes"
      write (1,*) 
      write (1,*) "w matrix: gene    1      gene 2 etc..."
      write (1,*) 
      do i=1,ng
        write(1,fmt="(A3,I4,"//rowfmt(2:11))  "gene",i,gen(i)%w       !!!!!!!W!!!!!!!!!!!!!!
      end do
      write (1,*) 
      write (1,*) "ww matrix: gene"
      write (1,*) 
      do i=1,ng
        write(1,*) i,gen(i)%nww
        if (gen(i)%nww>0) then
          write(1,*)  gen(i)%ww(:gen(i)%nww,1) !!!!!!!WW!!!!!!!!!!!!!
          write(1,*)  gen(i)%ww(:gen(i)%nww,2) !!!!!!!WW!!!!!!!!!!!!!
          do j=1,gen(i)%nww
            write(1,fmt="(es24.16)",ADVANCE='NO') gen(i)%ww(j,3) !!!!!!!WW!!!!!!!!!!!!!
          end do
          write(1,*) ""
        end if
      end do
      write (1,*) 
      write (1,*) "wa matrix: node prop 1 node prop 2 etc..."
      write (1,*) 
      do i=1,ng
        write(1,fmt="(A3,I4,"//rowfmta(2:11))  "gene",i,gen(i)%wa     !!!!!!!WA!!!!!!!!!!!!!!
      end do
      write (1,*) 
      write (1,*) "other gene characteristics"
      write (1,*) 
      do i=1,ng
        write(1,*) "gene",i !,gen(i)%label
        write(1,'(es24.16,A12)') gen(i)%diffu," diffusivity"
        write(1,'(es24.16,A17)') gen(i)%mu,"degradation rate"
        write(1,'(es24.16,A36)') gen(i)%kindof,"type, 0 TF, 1 modofiable FT, 2 form"
        write(1,'(I4,A20)') gen(i)%npre," number of pre forms"
        if (gen(i)%npre/=0) write(1,*) gen(i)%pre
        write(1,'(I4,A20)') gen(i)%npost," number of post forms"
        if (gen(i)%npost/=0) write(1,*) gen(i)%post
        write (1,*) 
      end do
      if(ntipusadh>0)then                                                 !>>>>Miquel14-11-13
        write (1,*) "adhesion molecules: kadh matrix"    !!!!!KADH!!!!!!!!!
        do i=1,ntipusadh             
          write(1,fmt="(A3,I4,"//rowfmt(2:11))  "adh",i,kadh(i,:ntipusadh)!>>>Is 12-1-13
        end do                                                            !
      end if                                                              !
      write (1,*)
      write (1,*) "gene expression"
      write (1,*)
      write (1,*) "node  gene 1                 gene 2    etc..."
      do i=1,nd
        write(1,fmt="(I6,"//rowfmt(2:11)) i,gex(i,:)   !gex matrix   
      end do
    end if
    write (1,*) 
    write (1,*) "node properties"
    write (1,*) 
    write (1,*) nodeparams
    nodi(1:nd)%x=node(1:nd)%x ; nodi(1:nd)%y=node(1:nd)%y ; nodi(1:nd)%z=node(1:nd)%z  !>>Miquel17-9-14
    nodi(1:nd)%icel=node(1:nd)%icel ; nodi(1:nd)%marge=node(1:nd)%marge ; nodi(1:nd)%talone=node(1:nd)%talone  !>>Miquel17-9-14
    nodi(1:nd)%hold=node(1:nd)%hold ; nodi(1:nd)%border=node(1:nd)%border ; nodi(1:nd)%altre=node(1:nd)%altre  !>>Miquel17-9-14
    nodi(1:nd)%exte=node(1:nd)%exte    !>>>Miguel 28-1-15

    do i=1,nd  !THIS PART WILL NEED TO BE CHANGED EVERY TIME WE CHANGE WHAT'S IN NODE TYPE
       write(1,cr) nodi(i)
    end do
    write (1,*) 
    write (1,*) "cell properties"
    write (1,*) 
    write (1,*) ncels,"number of cells"
    write (1,*) 
    do i=1,ncels
      write(1,*) "cell",i
      write(1,"(es24.16,A12)") 0.00
      write(1,"(es24.16,A50)") cels(i)%minsize_for_div,"minimal number of nodes to be able to divide"
      write(1,"(es24.16,A50)") cels(i)%maxsize_for_div,"max number of nodes before forced division"   !>>> Is 5-2-14
      write(1,"(3es24.16,A31)") cels(i)%cex,cels(i)%cey,cels(i)%cez," centroid x,y and z coordinated"
      write(1,"(3es24.16,A43)") cels(i)%polx,cels(i)%poly,cels(i)%polz,"polarization vectors x, y and z components"
      write(1,"(es24.16,A13)") cels(i)%fase," cell's phase"
      write(1,"(es24.16,A13)") cels(i)%temt," counter for epithelial-mesenchymal transition"

      write (1,*) cels(i)%nunodes,"number of nodes in a cell"
      write (1,*) cels(i)%nodela,"actual size of the cels(i)%node matrix"
      write (1,*) cels(i)%ctipus,"cell type"
      write (1,*) cels(i)%sister,"cell sister" !>>>Miguel 28-1-15
      write (1,*) "list of the nodes in the cell"
      write (1,*) cels(i)%node(:cels(i)%nunodes)
      write (1,*) 
    end do
  end subroutine


!**********************************************************************************************

  subroutine writepara(para)  !as writeassuch but without writing node >>> Is 22-1-14
    character*30 nofi
    integer i,j
    real*8 para(nparam)
    type(nod) nodi(nd)    

    write (1,*) "THIS FILE WAS WRITTEN IN THE FORMAT OF THE ",version," VERSION"
    write (1,*) winame
    write (1,*) nparam,"number of node parameters"
    write (1,*) nvarglobal_out,"number of global variables"
    write (1,*) 
    write (1,*) nfu,"functions"
    write (1,*) 
    do i=1,nfu
      write (1,*) ffu(i),names_fu(i)
    end do
    write (1,*) 
    write (1,*) "parameters"
    write (1,*) 
    do i=1,nparam
      write (1,"(I2,es24.16,A1,A100)") i,para(i)," ",names_param(i)
    end do
    write (1,*) 
    write (1,*) "random seed at the first iteration"
    write (1,*) 
    write (1,*) idumoriginal
    write (1,*) 
    write (1,*) "random seed at this iteration"
    write (1,*) 
    call random_seed(get=idum)
    write (1,*) idum
    write (1,*) 
    write (1,*) "global output variables"
    write (1,*) 
    do i=1,nvarglobal_out
      write (1,"(es24.16,A100)") varglobal_out(i),names_varglobal_out(i)
    end do

    !genetic information
    if (ng>0) then 
      write (1,*) 
      write (1,*) ng,"genes"
      write (1,*) 
      write (1,*) "w matrix: gene    1      gene 2 etc..."
      write (1,*) 
      do i=1,ng
        write(1,fmt="(A4,I4,"//rowfmt(2:11))  "gene",i,gen(i)%w       !!!!!!!W!!!!!!!!!!!!!!
      end do
      write (1,*)
      write (1,*) "ww matrix: gene"
      write (1,*) 
      do i=1,ng
        write(1,*) i,gen(i)%nww
        if (gen(i)%nww>0) then
          write(1,*)  gen(i)%ww(:gen(i)%nww,1) !!!!!!!WW!!!!!!!!!!!!!
          write(1,*)  gen(i)%ww(:gen(i)%nww,2) !!!!!!!WW!!!!!!!!!!!!!
          do j=1,gen(i)%nww
            write(1,fmt="(es24.16)",ADVANCE='NO') gen(i)%ww(j,3) !!!!!!!WW!!!!!!!!!!!!!
          end do
          write(1,*) ""
        end if
      end do
      write (1,*) 
      write (1,*) "wa matrix: node prop 1 node prop 2 etc..."
      write (1,*) 
      do i=1,ng
        write(1,fmt="(A4,I4,"//rowfmta(2:11))  "gene",i,gen(i)%wa     !!!!!!!WA!!!!!!!!!!!!!!
      end do
      write (1,*) 
      write (1,*) "other gene characteristics"
      write (1,*) 
      do i=1,ng
        write(1,*) "gene",i
        write(1,'(es24.16,A12)') gen(i)%diffu," diffusivity"
        write(1,'(es24.16,A17)') gen(i)%mu,"degradation rate"
        write(1,'(es24.16,A36)') gen(i)%kindof,"type, 0 TF, 1 modofiable FT, 2 form"
        write(1,'(I4,A20)') gen(i)%npre," number of pre forms"
        if (gen(i)%npre/=0) write(1,*) gen(i)%pre
        write(1,'(I4,A20)') gen(i)%npost," number of post forms"
        if (gen(i)%npost/=0) write(1,*) gen(i)%post
        write (1,*) 
      end do
      if(ntipusadh>0)then                                                 !>>>>Miquel14-11-13
        write (1,*) "adhesion molecules: kadh matrix"    !!!!!KADH!!!!!!!!!
        do i=1,ntipusadh             
          write(1,fmt="(A3,I4,"//rowfmt(2:11))  "adh",i,kadh(i,:ntipusadh)!>>>Is 12-1-13
        end do                                                            !
      end if                                                              !
      write (1,*)
      write (1,*) "gene expression"
      write (1,*)
      write (1,*) "node  gene 1                 gene 2    etc..."
      do i=1,nd
        write(1,fmt="(I6,"//rowfmt(2:11)) i,gex(i,:)   !gex matrix   
      end do
    end if
  end subroutine

!***************************************************************

  subroutine readsnap(nofi) !with nodeo  
    character*140 nofi
    integer io,ko,i,j,k
    character*2  cd
    character*1  cu
    character*3  cax
    character*4  caq
    character*4  duh
    character*120 rversion
    integer, allocatable :: cffu(:)  

    open(1,file=nofi,iostat=io)
    read(1,'(a)') rversion
    read(1,'(a)',ERR=666,END=777) winame
    read(1,*,ERR=666,END=777) !nparam
    read(1,*,ERR=666,END=777) nvarglobal_out
    read(1,*,ERR=666,END=777)
    read(1,*,ERR=666,END=777)
    read(1,*,ERR=666,END=777)
    if (allocated(ffu)) deallocate(ffu)
    allocate(ffu(nfu))
    if (allocated(param)) deallocate(param)
    allocate(param(nparam))
    if (allocated(varglobal_out)) deallocate(varglobal_out)
    allocate(varglobal_out(nvarglobal_out))
    do i=1,nfu
      read(1,*,ERR=666,END=777) ffu(i)
    end do
    allocate(cffu(nfu))
    cffu=ffu
    read(1,*,ERR=666,END=777)
    read(1,*,ERR=666,END=777)
    read(1,*,ERR=666,END=777)
    do i=1,nparam
      read (1,"(I2,es24.16)",ERR=666,END=777) ko,param(i)
print *,ko,param(i)
    end do
    read(1,*) 
print *,0
    read(1,*,ERR=666,END=777)
print *,1
    read(1,*,ERR=666,END=777) 
print *,2
    read(1,*,ERR=666,END=777) idumoriginal(1)
print *,idumoriginal
    read(1,*) 
    read(1,*) 
    read(1,*) 
    read(1,*) idum
print *,idum
    call random_seed(put=idum)
    read(1,*) 
    read(1,*,ERR=666,END=777)
    read(1,*) 
    call get_param_from_matrix_read(param)

    do i=1,nvarglobal_out
      read (1,"(es24.16)",ERR=666,END=777) varglobal_out(i)
    end do
    nda=nd+10
    !genetic information
    if (ng>0) then
      read(1,*,ERR=666,END=777)
      read(1,*,ERR=666,END=777) ng
      read(1,*,ERR=666,END=777)
      call initiate_gene
      if (allocated(gex)) deallocate(gex)
      allocate(gex(nda,ng))    
      if (allocated(gen)) deallocate(gen)
      allocate(gen(ng))
      if(ntipusadh>0)then                    !>>>>>Miquel14-11-13
        if (allocated(kadh)) deallocate(kadh)!
        allocate(kadh(ntipusadh,ntipusadh))  !
      end if                                 !
      do i=1,ng
        if (allocated(gen(i)%w)) deallocate(gen(i)%w)
        allocate(gen(i)%w(ng))    
        if (allocated(gen(i)%ww)) deallocate(gen(i)%ww)
        allocate(gen(i)%ww(ng*ng,3))    
        if (allocated(gen(i)%wa)) deallocate(gen(i)%wa)
        allocate(gen(i)%wa(nga))    
      end do
      !now the same for ng the number of genes
      rowfmt="(10es24.16)"
      if (ng>9) then
        write (cd,'(I2)') ng !this is funky, I write a text into a character variable that then use as format
        rowfmt(2:3)=cd
      else
        write (cu,'(I1)') ng !this is funky, I write a text into a character variable that then use as format
        rowfmt(2:2)=" "
        rowfmt(3:3)=cu
      end if
      read(1,*,ERR=666,END=777)
      read(1,*,ERR=666,END=777)
      do i=1,ng
        read(1,fmt="(A3,I4,"//rowfmt(2:11))  cax,j,gen(i)%w    
      end do
      read(1,*,ERR=666,END=777)
      read(1,*,ERR=666,END=777)
      read(1,*,ERR=666,END=777)
      do i=1,ng
        read(1,*,ERR=666,END=777) j,k
        gen(i)%nww=k
        if (gen(i)%nww>0) then
          read(1,*,ERR=666,END=777) gen(i)%ww(:gen(i)%nww,1)
          read(1,*,ERR=666,END=777) gen(i)%ww(:gen(i)%nww,2)
          do j=1,gen(i)%nww
            read(1,fmt="(es24.16)",ADVANCE='NO') gen(i)%ww(j,3)
          end do
          read(1,*,ERR=666,END=777)
        end if
      end do
      read(1,*,ERR=666,END=777)
      read(1,*,ERR=666,END=777)
      read(1,*,ERR=666,END=777)
      do i=1,ng
        read(1,fmt="(A3,I4,"//rowfmta(2:11))  cax,j,gen(i)%wa    
      end do
      read(1,*,ERR=666,END=777)
      read(1,*,ERR=666,END=777)
      read(1,*,ERR=666,END=777)
      do i=1,ng
        read(1,*,ERR=666,END=777) duh,j !,gen(i)%label
        read(1,'(es24.16,A12)',ERR=666,END=777) gen(i)%diffu
        read(1,'(es24.16,A17)',ERR=666,END=777) gen(i)%mu
        read(1,'(es24.16,A36)',ERR=666,END=777) gen(i)%kindof
        read(1,*) gen(i)%npre !,"number of pre forms"
        if (gen(i)%npre/=0) then
          allocate(gen(i)%pre(gen(i)%npre))
          read(1,*) gen(i)%pre
        end if
        read(1,*) gen(i)%npost !,"number of post forms"
        if (gen(i)%npost/=0) then
          allocate(gen(i)%post(gen(i)%npost))
          read(1,*) gen(i)%post
        end if
        read(1,*)
      end do
      if(ntipusadh>0)then                                                 !>>>>Miquel14-11-13
        read (1,*)                                                        !
        do i=1,ntipusadh                                                  !
          read(1,fmt="(A3,I4,"//rowfmt(2:11)) cax,j,kadh(i,1:ntipusadh)   !
        end do                                                            !
      end if                                                              !
      read (1,*,ERR=666,END=777) 
      read (1,*,ERR=666,END=777) 
      read (1,*,ERR=666,END=777) 
      read (1,*,ERR=666,END=777) 
      do i=1,nd
        read(1,fmt="(I6,"//rowfmt(2:11),ERR=666,END=777) j,gex(i,:)   !gex matrix   
      end do
      call update_npag
    end if
    if (allocated(node)) deallocate(node)
    if (allocated(nodeo)) deallocate(nodeo)
    if (allocated(cels)) deallocate(cels)
    nda=nd+10
    ncals=ncels+10
    allocate(node(nda))
    allocate(nodeo(nda))
    allocate(cels(ncals))

    call iniarrays
    
    ffu=cffu

    read(1,*,ERR=666,END=777)    
    read(1,*,ERR=666,END=777)    
    read(1,*,ERR=666,END=777)    
    read(1,*,ERR=666,END=777)    
    do i=1,nd                 !THIS PART WILL NEED TO BE CHANGED EVERY TIME WE CHANGE WHAT'S IN NODE TYPE
      read(1,cr,ERR=666,END=777) node(i)
    end do
    read(1,*,ERR=666,END=777)    
    read(1,*,ERR=666,END=777)    
    read(1,*,ERR=666,END=777)    
    read(1,*,ERR=666,END=777)    
    do i=1,nd  !THIS PART WILL NEED TO BE CHANGED EVERY TIME WE CHANGE WHAT'S IN NODE TYPE
      read(1,cr,ERR=666,END=777) nodeo(i)
    end do
    read(1,*,ERR=666,END=777)
    read(1,*,ERR=666,END=777)
    read(1,*,ERR=666,END=777)
    read(1,*,ERR=666,END=777) ncels
    read(1,*) 
    ncals=ncels+10
    if (allocated(cels)) deallocate(cels)
    allocate(cels(ncals))
    do i=1,ncels
      read(1,*,ERR=666,END=777)
      read(1,"(es24.16)",ERR=666,END=777) a
      read(1,"(es24.16)",ERR=666,END=777) cels(i)%minsize_for_div !>>> Is 5-2-14 
      read(1,"(es24.16)",ERR=666,END=777) cels(i)%maxsize_for_div 
      read(1,"(3es24.16)",ERR=666,END=777) cels(i)%cex,cels(i)%cey,cels(i)%cez
      read(1,"(3es24.16)",ERR=666,END=777) cels(i)%polx,cels(i)%poly,cels(i)%polz
      read(1,"(es24.16)",ERR=666,END=777) cels(i)%fase
      read(1,"(es24.16)",ERR=666,END=777) cels(i)%temt
      read(1,*,ERR=666,END=777) cels(i)%nunodes
      read(1,*,ERR=666,END=777) cels(i)%nodela		
      read(1,*,ERR=666,END=777) cels(i)%ctipus             
      read(1,*,ERR=666,END=777) cels(i)%sister    !>>>Miguel 28-1-15
      if (allocated(cels(i)%node)) deallocate(cels(i)%node)
      allocate(cels(i)%node(cels(i)%nodela))
      read(1,*,ERR=666,END=777)
      read(1,*,ERR=666,END=777) cels(i)%node(1:cels(i)%nunodes)
      read(1,*) 
    end do

    nodeo=node !>>Miquel17-9-14


    if (rappend/=0) close(1)
    return
666 do i=1,5 ; print*,"" ;end do
    errorlec=1
    print*,"PANIC: error in reading file ",nofi
    do i=1,5 ; print*,"" ;end do
    close(1) ; return
777 do i=1,5 ; print*,"" ;end do
    print *,"EPS: end of file OR NOT SUCH A FILE u wanker"
    do i=1,5 ; print*,"" ;end do
    close(1) ; return
  end subroutine

!***************************************************************

  subroutine readpara(nofi)  !as readsnap but without reading node >>> Is 22-1-14
    character*120 nofi
    integer io,ko,i,j,k
    character*2  cd
    character*1  cu
    character*3  cax
    character*4  caq
    character*120  rversion
    integer, allocatable :: cffu(:)  

    open(1,file=nofi,iostat=io)
    read(1,'(a)') rversion
    read(1,'(a)',ERR=665,END=775) winame
    !read(1,*,ERR=665,END=775) nparam
    read(1,*,ERR=665,END=775) nvarglobal_out
    read(1,*,ERR=665,END=775)
    read(1,*,ERR=665,END=775)
    read(1,*,ERR=665,END=775)
    if (allocated(ffu)) deallocate(ffu)
    allocate(ffu(nfu))
    if (allocated(param)) deallocate(param)
    allocate(param(nparam))
    if (allocated(varglobal_out)) deallocate(varglobal_out)
    allocate(varglobal_out(nvarglobal_out))
    do i=1,nfu
      read(1,*,ERR=665,END=775) k
      ffu(i)=k
    end do
    allocate(cffu(nfu))
    cffu=ffu
    read(1,*,ERR=665,END=775)
    read(1,*,ERR=665,END=775)
    read(1,*,ERR=665,END=775)
    do i=1,nparam
      read (1,"(I2,es24.16)",ERR=665,END=775) ko,param(i)
    end do
    read(1,*) 
    read(1,*,ERR=665,END=775)
    read(1,*) 
    read(1,*) idumoriginal
    read(1,*) 
    read(1,*) 
    read(1,*) 
    read(1,*) idum
    call random_seed(put=idum)
    read(1,*) 
    read(1,*,ERR=665,END=775)
    read(1,*) 
    call get_param_from_matrix_read(param)

    do i=1,nvarglobal_out
      read (1,"(es24.16)",ERR=665,END=775) varglobal_out(i)
    end do

    nda=nd+10

    !genetic information
    if (ng>0) then
      read(1,*,ERR=665,END=775)
      read(1,*,ERR=665,END=775) ng
      read(1,*,ERR=665,END=775)
      call initiate_gene
      if (allocated(gex)) deallocate(gex)
      allocate(gex(nda,ng))    
      if (allocated(gen)) deallocate(gen)
      allocate(gen(ng))
      if(ntipusadh>0)then                    !>>>>>Miquel14-11-13
        if (allocated(kadh)) deallocate(kadh)!
        allocate(kadh(ntipusadh,ntipusadh))  !
      end if                                 !
      do i=1,ng
        if (allocated(gen(i)%w)) deallocate(gen(i)%w)
        allocate(gen(i)%w(ng))    
        if (allocated(gen(i)%wa)) deallocate(gen(i)%wa)
        allocate(gen(i)%wa(nga))    
      end do
      !now the same for ng the number of genes
      rowfmt="(10es24.16)"
      if (ng>9) then
        write (cd,'(I2)') ng !this is funky, I write a text into a character variable that then use as format
        rowfmt(2:3)=cd
      else
        write (cu,'(I1)') ng !this is funky, I write a text into a character variable that then use as format
        rowfmt(2:2)=" "
        rowfmt(3:3)=cu
      end if
      read(1,*,ERR=665,END=775)
      read(1,*,ERR=665,END=775)
      do i=1,ng
        read(1,fmt="(A3,I4,"//rowfmt(2:11))  cax,j,gen(i)%w    
      end do
      read(1,*,ERR=665,END=775)
      read(1,*,ERR=665,END=775)
      read(1,*,ERR=665,END=775)
      do i=1,ng
        read(1,fmt="(A3,I4,"//rowfmta(2:11))  cax,j,gen(i)%wa    
      end do
      read(1,*,ERR=665,END=775)
      read(1,*,ERR=665,END=775)
      read(1,*,ERR=665,END=775)
      do i=1,ng
        read(1,*,ERR=665,END=775)
        read(1,'(es24.16,A12)',ERR=665,END=775) gen(i)%diffu
        read(1,'(es24.16,A17)',ERR=665,END=775) gen(i)%mu
        read(1,'(es24.16,A36)',ERR=665,END=775) gen(i)%kindof
        read(1,*) gen(i)%npre !,"number of pre forms"
        if (gen(i)%npre/=0) then
          allocate(gen(i)%pre(gen(i)%npre))
          read(1,*) gen(i)%pre
        end if
        read(1,*) gen(i)%npost !,"number of post forms"
        if (gen(i)%npost/=0) then
          allocate(gen(i)%post(gen(i)%npost))
          read(1,*) gen(i)%post
        end if
        read(1,*)
      end do
      if(ntipusadh>0)then                                                 !>>>>Miquel14-11-13
        read (1,*)                                                        !
        do i=1,ntipusadh                                                  !
          read(1,fmt="(A3,I4,"//rowfmt(2:11)) cax,j,kadh(i,1:ntipusadh)   !
        end do                                                            !
      end if                                                              !
      read (1,*,ERR=665,END=775) 
      read (1,*,ERR=665,END=775) 
      read (1,*,ERR=665,END=775) 
      read (1,*,ERR=665,END=775) 
      do i=1,nd
        read(1,fmt="(I6,"//rowfmt(2:11),ERR=665,END=775) j,gex(i,:)   !gex matrix   
      end do
      call iniarrays
      ffu=cffu
      call update_npag
    end if
if (rappend/=0) close(1)
    return
665 do i=1,5 ; print*,"" ;end do
    errorlec=1
    print*,"PANIC: error in reading file ",nofi
    do i=1,5 ; print*,"" ;end do
    close(1) ; return
775 do i=1,5 ; print*,"" ;end do
    print *,"EPS: end of file OR NOT SUCH A FILE u wanker"
    do i=1,5 ; print*,"" ;end do
    close(1) ; return
  end subroutine

!**************************************************************************************

subroutine get_its(tti,tfi)
  integer tti,tfi
  character*10 cf  

  call getarg(3,cf)
  if (len_trim(cf)>0) then
    read (cf,*) tti
  end if

  call getarg(4,cf)
  if (len_trim(cf)>0) then
    read (cf,*) tfi
  end if
end subroutine
!***********************************************************************************************************

subroutine read_config_file !config file will set some general environmental variables for the model, like  !>>Miquel8-9-14
                            !visualization settings or which default initial condition to load

   open(7,file="config_file.txt")

   read(7,*)
   read(7,*)
   read(7,*)
   read(7,*)

   read(7,*)ic_load
   read(7,*)
   read(7,*)
   read(7,*)
   read(7,*)
   read(7,*)conf_shiftx,conf_shifty,conf_shiftz
   !read(7,*)conf_lookatx,conf_lookaty,conf_lookatz
   read(7,*)
   read(7,*)conf_anglex
   read(7,*)conf_angley
   read(7,*)
   read(7,*)conf_colorselection
   read(7,*)conf_rainbow
   read(7,*)conf_custom_colorselection
   read(7,*)conf_custom_max
   read(7,*)conf_custom_min
   !read(7,*)conf_chogen
   read(7,*)
   read(7,*)conf_arrowselection !;print*,"conf_arrowselection",conf_arrowselection
   read(7,*)conf_custom_arrowscale
   read(7,*)conf_custom_arrowselection
   read(7,*)conf_custom_amaxval
   read(7,*)conf_custom_aminval
   !read(7,*)conf_chogen
   read(7,*)
   read(7,*)conf_sphereselection !;print*,"conf_arrowselection",conf_sphereselection
   read(7,*)conf_custom_spherescale
   read(7,*)conf_custom_sphereselection
   read(7,*)conf_custom_smaxval
   read(7,*)conf_custom_sminval
   !read(7,*)conf_chogen

   read(7,*)
   read(7,*)
   read(7,*)
   read(7,*)
   read(7,*)

   do i=1,40
     read(7,*)conf_flag(i)
   end do

   read(7,*)
   read(7,*)
   read(7,*)

   read(7,*)conf_select_what

   if(conf_select_what>0)then
     read(7,*)
     read(7,*)conf_nki
     read(7,*)

     if(conf_nki>0)then
       allocate(conf_oopp(conf_nki))
       do i=1,conf_nki
         read(7,*) conf_oopp(i)
       end do
     end if
   end if

   close(7)


end subroutine read_config_file

subroutine no_config_file !>>Miquel2-1-14
   
   ic_load=9
   
   conf_shiftx=0d0 ; conf_shifty=0d0 ; conf_shiftz=5d0
   conf_anglex=0d0
   conf_angley=60d0
   
   conf_colorselection=29
   conf_rainbow=1
   conf_custom_colorselection=0
   conf_custom_max=0
   conf_custom_min=0

   conf_arrowselection=0
   conf_custom_arrowscale=1d0
   conf_custom_arrowselection=0
   conf_custom_amaxval=0d0
   conf_custom_aminval=0d0

   conf_sphereselection=0
   conf_custom_spherescale=1d0
   conf_custom_sphereselection=0
   conf_custom_smaxval=0d0
   conf_custom_sminval=0d0


   conf_flag(1)=0   !cylinder spring 
   conf_flag(2)=0   !3D box grid for neighboring
   conf_flag(3)=0   !nothing
   conf_flag(4)=0   !nothing
   conf_flag(5)=1   !nodes as spheres: radius is p^EQD
   conf_flag(6)=0   !nodes as spheres: radius is p^ADD
   conf_flag(7)=0   !nothing
   conf_flag(8)=0   !no spheres
   conf_flag(9)=1   !epithelial apical nodes
   conf_flag(10)=1   !epithelial basal nodes
   conf_flag(11)=0   !nodes as spheres: small radius
   conf_flag(12)=0   !connexions between cells
   conf_flag(13)=0   !connexions within the cell
   conf_flag(14)=0   !nothing
   conf_flag(15)=0   !nothing
   conf_flag(16)=0   !nothing
   conf_flag(17)=0   !nothing
   conf_flag(18)=1   !box displaying the span of the system
   conf_flag(19)=0   !display cell polarization vectors 
   conf_flag(20)=0   !nothing 
   conf_flag(21)=0   !display cell centroids 
   conf_flag(22)=1   !epithelial nodes as cylinders
   conf_flag(23)=0   !nothing
   conf_flag(24)=0   !nothing
   conf_flag(25)=0   !nothing
   conf_flag(26)=0   !nothing
   conf_flag(27)=0   !nothing
   conf_flag(28)=0   !nothing
   conf_flag(29)=0   !nothing
   conf_flag(30)=1   !display epithelium
   conf_flag(31)=1   !display mesenchyme
   conf_flag(32)=1   !display extracellular matrix
   conf_flag(33)=0   !nothing
   conf_flag(34)=0   !nothing
   conf_flag(35)=0   !nothing
   conf_flag(36)=1   !fixed nodes (node()%hold=1)
   conf_flag(37)=0   !plot cell contour
   conf_flag(38)=0   !plot intercellular contour
   conf_flag(39)=0   !plot displacement of nodes respect initial conditions
   conf_flag(40)=0   !nothing
   !conf_flag(41)=0   !0=dynamic display box,1=fixed display box

end subroutine no_config_file


end module
