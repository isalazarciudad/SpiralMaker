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


    !!!WE ARE ASSUMING A VERY VISCOUS ENVIRONMENT, SO THERE IS NO INERTIA AND THE FORCES JUST AFFECT MOVEMENT ON THE SAME DIFFERENTIAL OF TIME

	
!************************************************************************************************************************************************
!              MODUL GENERAL
!************************************************************************************************************************************************
module general   !ATENCIO: versio optimitzada en la que no sumem per ordre


implicit none

! PARAMETRES  PARAMETRES  PARAMETRES  PARAMETRES  PARAMETRES  PARAMETRES  PARAMETRES  PARAMETRES

! METAPARAMETERS
character*8, public, parameter ::version="26-8-14"  !CHECK MANUALLY date of this version of the code 
integer,public                 ::aut                 !1 if non-graphical-automatic run, 0 otherwise
integer,public, parameter      ::nparam=30           !numero de parametres
integer,public, parameter      ::nparam_per_node=36  !CHECK MANUALLY number of parameters per node 
                                                     !(variables in type nod) IF YOU CHANGE THAT CHANGE ALSO io.mod
integer,public, parameter      ::nparam_per_noder=28 !CHECK MANUALLY which of them are real
integer,public, parameter      ::nparam_per_nodei=8  !CHECK MANUALLY which of them are integer
integer,public, parameter      ::ngcb=16             !CHECK MANUALLY number of genetically affectable cell behaviours ! >>> Is 5-2-14
integer,public, parameter      ::nga=nparam_per_node+ngcb  ! this is the number of genetically affectable node and cell parameters 
                                                     !if the number of parameters that can be affected by genes
integer,public, parameter      ::nfu=29              !CHECK MANUALLY number of basic forces !miguel4-11-13
!integer, public     ::counterflowk!=50!125     ! counter for exert torque forces after mitosis  !!>>>Miguel 28-1-15
integer,public                 ::nvarglobal_out   !numero de vars que no son params
integer,public, allocatable    ::ffu(:)           !matrix of flags for which functions are in use

integer,public, parameter      ::status=10        !it implies a 20560 exit code

! MODEL PARAMETERS
integer, public                ::getot            !number of iterations
integer, public                ::counterflow      !>>>Miguel 28-1-15 counter for exert torque forces after mitosis
real*8 ,public                 ::temp             !temperature
real*8 ,public                 ::rv,urv           !lenght of the box
real*8, public                 ::prop_noise       !proportion of node subject to noise per dif eq iteration
real*8 ,public                 ::reqmin           !the req in the new nodes added by growth, it is not very important
real*8 ,public                 ::ecmmax           !that determines the amount of ecm material that a cell has to accumulate
                                                  !before releasing it to the extracellular space, it is alo the req and reqcel of the resulting node
real*8 ,public                 ::mmae             !maximal radius to add a node when growth
real*8 ,public                 ::dmax             !number of radi at which to consider diffusion
integer,public                 ::ndmax            !maximal number of nodes if there is a maximum (if ffu(2)=1)
real*8 ,public                 ::prec             !numerical accuracy when using adaptive stepsize (optional) ! >>> Is 26-8-14
real*8 ,public                 ::dif_req          !diffusion of req ! Is 11-6-14
real*8 ,public                 ::min_comp         !minimal value of the compression of nodes to allow the addition of new nodes Is 21-6-14
! variables that are directly derived fromt the parameters

! de implementacio MATEMATICA
integer, public                ::mnn              !maximal number of neighbors a node can interact with, if more then PANIC
real*8,  public                ::ramax            !maximal radius a node can have, if ffu(7)==1
real*8,  parameter             ::epsilo=epsilon(temp)  !the epsilon of the machine for real*8, required for the black magic of real comparisons
real*8,  parameter             ::epsilod=epsilon(temp)*10  !the epsilon of the machine for real*4, required for the black magic of real comparisons
real*8,  public                ::deltamax              !maximal delta
real*8,  public                ::deltamin              !minimal delta
real*8,  public                ::desmax                !maximal displacement in the noise energy part
real*8,  public                ::resmax                !maximal displacement per iteration with dif eq
integer, public                ::nparti         ! numero de particions dels numeros aleas
integer, public                ::idum(33),idumoriginal(33)  ! 12 is simply because it is the minim required for the intrinsic f90 random_number
integer, public                ::movi           ! to prevent nodes from leaving the eggshell !miguel22-7-13
real*8,  public                ::ttalone        ! time a node is allowed to be alone before dying
integer, public                ::khold          ! elastic coefficient of the physical border force   !>>>Miquel9-1-14
real*8, public                 ::screen_radius  ! intensity of screening between nodes: 0 = no screening, 1 = full screening  !>>Miquel28-7-14
real*8, public                 ::df_reqmax         ! maximum req possible due to deformation   !>>Miquel28-7-14
real*8, public                 ::angletor       !minimum angle for torsion forces to apply   !>Miquel15-9-14
real*8, public                 ::ldi            !minimal amount of interchange of molecules in diffusion to be different than zero
                                                !it is equal to epsilod
integer,public                 ::mnn_dyn,omnn,mnn_dynam        !to assess the max width of the neighbor matrix dynamically

! VARIABLES VARIABLES VARIABLES VARIABLES VARIABLES VARIABLES VARIABLES VARIABLES VARIABLES
!variables globals importants
integer, public                ::nd       ! number of nodes
integer, public                ::ng       ! number of genes  !>>>>>>>>>>> Is 29-4-13
integer, public                ::ncels    ! number of cells
integer, public                ::ncals    ! es ncels+10
integer, public                ::nda      ! nd+10	>>Miquel 14-10-12
integer, public                ::ndepi,ndmes,ndx,ncelsepi,ncelsmes !nd for epi mes and ncels for epi and mes
real*8 , public                ::extre    ! maximal distance between (0,0,0) and any existing node
integer, public                ::itacc    ! number of accepted iterations
integer, public                ::nodecel  ! standard (initial) number of nodes per cell	!>>>>>>>>>>>>>>>>> Miquel 21-3-13
real*8, public                 ::rtime    ! variable counting the real time (that is the deltas) when using differential equations
real*8, public                 ::delta    !the diferential of numeric integration (will be adaptively calculated at each iteration
real*4, public                 ::realq
real*8, public, allocatable    ::px(:),py(:),pz(:),dex(:)  !vectors for storing the vectors and differentials of movement     !>>>> Miquel 17-6-13
integer, public, allocatable   ::neigh(:,:) !it stores in each interation who is interacting with you
integer, public, allocatable   ::nneigh(:)  !the number of those
real*8 , public, allocatable   ::dneigh(:,:)!the distances between those
real*8 , public, allocatable   ::flowplot(:,:) ! It stores the displacement vectors for the blastomere rotations !>>>Miguel 28-1-15
integer, public, allocatable   ::dif_nneigh(:)  !the number of those neighbors affected by diffusion!  !>>Miquel24-2-14

real*8, public, allocatable    ::vcilx(:),vcily(:),vcilz(:)   !force components storage arrays (differential equations method) !>>>>> Miquel 20-6-13
real*8, public, allocatable    ::vtorx(:),vtory(:),vtorz(:)
real*8, public, allocatable    ::vstorx(:),vstory(:),vstorz(:)
real*8, public, allocatable    ::vsprx(:),vspry(:),vsprz(:)
real*8, public, allocatable    ::erep(:)     !just memory to plot intracell repulsion     
real*8, public, allocatable    ::erepcel(:)  !just memory to plot  intercell repulsion
real*8, public, allocatable    ::eyou(:)     !just memory to plot  intracell adhesion
real*8, public, allocatable    ::eadh(:)     !just memory to plot  intercell adhesion
real*8, public, allocatable    ::etor(:)     !just memory to plot  torsion related energy (only epithelia)
real*8, public, allocatable    ::espring(:)  !just memory to plot  spring related energy (only epihelia)


real*8, public, allocatable    ::fmeanl(:),fmeanv(:) !storage array used to assess the plastic deformations of epithelial nodes (compressive or tensile) !>>Miquel23-1-14

real*8, public :: rdiffmax  !>>Miquel24-2-14

type, public                   ::nod      ! tipus node

     sequence                  !this forces the data to be stored continously in the memory: we could try if it makes it faster

     !********************************************NODE
     !var escribible   25 VARIABLES as 31-8-13
     real*8                    ::x,y,z    !1-3 posicio
     real*8                    ::e        !4 energiy value on the latest iteration

     !casi parametres
     real*8                    ::req      !5 equilibrium distance for nodes in the same face and cell           
     real*8                    ::da       !6 maximum distance for interaction between nodes
     real*8                    ::you      !7 elasticity for nodes in the same cell
     real*8                    ::adh      !8 adhesion (elasticity) between nodes in different cells: this is the inespecific adhesion:then there are genes           
     real*8                    ::rep      !9 repulsion  for nodes in the same cell
     real*8                    ::repcel   !10 repulsion between nodes in different cells  
     real*8                    ::tor      !11 epithelial surface tension lateral(torsion) 
     real*8                    ::stor     !12 epithelial surface tension apical-basal(torsion)
     real*8                    ::reqs     !13 equilibrium distance for the spring of the elipse
     real*8                    ::ke       !14 spring constant for the ellipse
     real*8                    ::mo       !15 node's movility
     real*8                    ::dmo      !16 node's desmax  
     real*8                    ::orix,oriy,oriz !17-19 posicions inicials 
     real*8                    ::acecm    !20 accumulated ecm in that node
     real*8                    ::reqc     !21 equilibrium radius component due to internal contraction and external deformations !>>Miquel27-1-13
     real*8                    ::reqcr    !22 equilibrium radius component due to node growth !>>Miquel27-1-13
     real*8                    ::reqp     !23 equilibrium radius component due to plasticity !>>Miquel27-1-13 
     real*8                    ::reqv     !24 equilibrium radius component correcting for volume conservation 8-5-14
     real*8                    ::diffe    !25 differentiation state of the node, 0 is no differentiation, 1 is total differentiation
     real*8                    ::khold    !26 elastic constant for hold node                                    !>>Miquel21-2-14
     real*8                    ::kplast   !27 plasticity constant for epithelial plastic deformation            !
     real*8                    ::kvol     !28 volume conservation constant for epithelial plastic deformation   !

     integer                   ::tipus    !29 els centres son tipus=2 i el subcentres tipus=4
     integer                   ::icel     !30 diu a quina cel pertany, son tres pels nodes al marge de la cel epi
     integer                   ::altre    !31 l'altre node de la elipse
     integer                   ::marge    !32 1 if it is a node in the margin
     integer                   ::talone   !33 iterations the node has been alone
     integer                   ::hold     !34 sets the node as physical border, applying an external force keeping it in its initial position !>>>>Miquel9-1-14
                                          ! if it is 2 the node never moves
     integer                   ::border   !35 this flag tells whether the node is in the border of the system or not (only with triangulation) !>>>>Miquel3-2-14
     integer                   ::exte     !36 it is 1 if the node belongs to the cell membrane, 0 otherwise !>>>Miguel 28-1-15
end type

!********************************************CELULES
type, public :: cel

     sequence                  !this forces the data to be stored continously in the memory

!     real*8                    ::reqmax         ! THIS IS A MODEL PARAMETER is the maximal req in a cell before one can add a new node
     real*8                    ::minsize_for_div! THIS IS A MODEL PARAMETER the minimal number of nodes one should have to being able to divide
     real*8                    ::maxsize_for_div! THIS IS A MODEL PARAMETER if a cel has more nodes that that it divides !>>> Is 5-2-14

     ! THESE ARE DEDUCIBLE FROM THE NODE'S IN A CELL
     real*8                    ::cex,cey,cez	!cell's centroid														!>>>>>>>>>>>>>>>>>>>>Miquel 12-4-13
     real*8                    ::polx,poly,polz !vector of polarization                         !>>>>>>>>>>>>>> Miquel 3-6-13
     real*8                    ::hpolx,hpoly,hpolz !vector of polarization !for the moment we do not WRITE OR READ THIS                      
     real*8                    ::spolx,spoly,spolz !hertwig special vector (cell subvolume) !>>>Miguel 28-1-15
     ! THESE ARE NOT 
     real*8                    ::fase           !fase in the cell cycle, when 1 it can divide if it has the rigth size, the 1 does not change nothing
     real*8, allocatable       ::amembt(:,:),amembt2(:,:) !  area of the cell (external triangles) !>>>Miguel 28-1-15 
     real*8                    ::flow(3),sisterv(3)       !  vector from flow                      !>>>Miguel 28-1-15
     real*8                    ::temt           !counter to when to make a epithelial mesenchymal transition
     
     integer                   ::nunodes
     integer                   ::nodela			!the real size of the node matrix, like nda but for each cell			!>>>>>>>>>>>>>>>>>>>>Miquel 21-3-13
     integer                   ::ctipus			!cell type																!>>>>>>>>>>>>>>>>>>>>Miquel 12-4-13
     integer                   ::next,nmemb,nmembt,nmembt2,sister      !number of external nodes,membrane nodes and external triangles !>>>Miguel 28-1-15
     integer, allocatable      ::node(:)
     integer, allocatable      ::ext(:),memb(:),membt(:,:),membt2(:,:)     !external nodes,membrane,membrane triangles !>>>Miguel 28-1-15
end type

type(nod), public, allocatable :: node(:),ulte(:),nodeo(:) ! notice nodeo saves the initial conditions and for the new nodes Is 25-5-13
type(cel), public, allocatable :: cels(:)                                                   ! how they are when they are born       

! variables that are directly derived fromt the parameters

!variables de implementacio: contadors 
real*8 , public                ::desplacament,ene

!variables de implementacio: contadors 
integer, public                ::itv

!les tipiques
integer, public                ::i,j,k,ii,jj,kk,iii,jjj,kkk,iiii,jjjj,kkkk,iiiii,jjjjj,kkkkk
real*8 , public                ::a,b,c,d,e,f,g,h,aa,bb,cc,dd,ee,ff,gg,hh
real*8 , public                ::aaa,bbb,ccc,ddd,eee,fff,ggg,hhh
character*11 precaa,cab        !>>> Is 1-2-14
character*17 caa               !>>> Is 1-2-14
character*11 cazero
character*8, public :: cacc,cag,ccag      !>>> Is 2-3-14
character*3 cad,cae,caf
character*41 winame

!de visualitzacio
integer, parameter :: fprint=100

! CONSTANTS  CONSTANTS  CONSTANTS  CONSTANTS  CONSTANTS  CONSTANTS  CONSTANTS  CONSTANTS   CONSTANTS 
! matematiques
real*8, public, parameter      ::nue = 2.71828
real*8, public, parameter      ::pi  = 3.141592653589793d0     !SOME MORE DIGITS ARE NEEDED
real*8, public, parameter      ::pii = 31.41592653589793d-1

! flag for in case this is run in evolution from reva.f90 so that it saves information in a different way
integer, public :: eva

contains

!********************************************************************************************

subroutine iniarrays

    if (allocated(ffu)) deallocate(ffu)
    allocate(ffu(nfu))

    if (allocated(neigh)) deallocate(neigh)
    allocate(neigh(nda,mnn))
    if (allocated(dneigh)) deallocate(dneigh)
    !write(*,*)'alocato en d',nda,mnn,omnn
    allocate(dneigh(nda,mnn))
    if (allocated(nneigh)) deallocate(nneigh)
    allocate(nneigh(nda))
   
    neigh=0
    dneigh=0.0d0
    nneigh=0
    if (allocated(px)) deallocate(px)
    if (allocated(py)) deallocate(py)
    if (allocated(pz)) deallocate(pz)
    if (allocated(dex)) deallocate(dex)
    allocate(px(nda),py(nda),pz(nda),dex(nda))
    px=0 ; py=0 ; pz=0 ; dex=0

   
    if (allocated(vcilx)) deallocate(vcilx)
    if (allocated(vcily)) deallocate(vcily)
    if (allocated(vcilz)) deallocate(vcilz)
    allocate(vcilx(nda),vcily(nda),vcilz(nda))
    vcilx=0 ; vcily=0 ; vcilz=0

    if (allocated(vtorx)) deallocate(vtorx)
    if (allocated(vtory)) deallocate(vtory)
    if (allocated(vtorz)) deallocate(vtorz)
    allocate(vtorx(nda),vtory(nda),vtorz(nda))
    vtorx=0 ; vtory=0 ; vtorz=0

    if (allocated(vstorx)) deallocate(vstorx)
    if (allocated(vstory)) deallocate(vstory)
    if (allocated(vstorz)) deallocate(vstorz)
    allocate(vstorx(nda),vstory(nda),vstorz(nda))
    vstorx=0 ; vstory=0 ; vstorz=0

    if (allocated(vsprx)) deallocate(vsprx)
    if (allocated(vspry)) deallocate(vspry)
    if (allocated(vsprz)) deallocate(vsprz)
    allocate(vsprx(nda),vspry(nda),vsprz(nda))
    vsprx=0 ; vspry=0 ; vsprz=0

    if (allocated(erep)) deallocate(erep)
    if (allocated(erepcel)) deallocate(erepcel)
    if (allocated(eadh)) deallocate(eadh)
    if (allocated(eyou)) deallocate(eyou)
    if (allocated(espring)) deallocate(espring)
    if (allocated(eadh)) deallocate(eadh)
    if (allocated(etor)) deallocate(etor)
    allocate(erep(nda),erepcel(nda),eadh(nda),eyou(nda),espring(nda),etor(nda))

    if (allocated(fmeanl)) deallocate(fmeanl)  !>>Miquel23-1-14
    allocate(fmeanl(nda))                     !>>Miquel23-1-14
    fmeanl=0                                !>>Miquel23-1-14
    
    if (allocated(fmeanv)) deallocate(fmeanv)  !>>Miquel23-1-14
    allocate(fmeanv(nda))                     !>>Miquel23-1-14
    fmeanv=0                                !>>Miquel23-1-14


    !setting all node and cell variables to 0  !>>Miquel28-1-14

     node(:)%x=0 ; node(:)%y=0 ; node(:)%z=0
     node(:)%e=0
     node(:)%req=0
     node(:)%da=0
     node(:)%you=0
     node(:)%adh=0
     node(:)%rep=0
     node(:)%repcel=0
     node(:)%tor=0
     node(:)%stor=0
     node(:)%reqs=0
     node(:)%ke=0
     node(:)%mo=0
     node(:)%dmo=0
     node(:)%orix=0 ; node(:)%oriy=0 ; node(:)%oriz=0
     node(:)%acecm=0
     node(:)%reqc=0
     node(:)%reqcr=0
     node(:)%reqp=0
     node(:)%reqv=0
     node(:)%diffe=0
     node(:)%khold=0
     node(:)%kplast=0
     node(:)%kvol=0
     node(:)%tipus=0
     node(:)%icel=0
     node(:)%altre=0
     node(:)%marge=0
     node(:)%talone=0
     node(:)%hold=0
     node(:)%border=0

     cels(:)%minsize_for_div=10
     cels(:)%minsize_for_div=10000
     cels(:)%cex=0 ; cels(:)%cey=0 ; cels(:)%cez=0 ;
     cels(:)%polx=0 ; cels(:)%poly=0 ; cels(:)%polz=0 ;
     cels(:)%hpolx=0 ; cels(:)%hpoly=0 ; cels(:)%hpolz=0 ;
     cels(:)%fase=0
     cels(:)%temt=0
     cels(:)%nunodes=0
     cels(:)%nodela=0
     cels(:)%ctipus=0

end subroutine iniarrays

end module general



