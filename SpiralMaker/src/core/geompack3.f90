function angle ( xa, ya, xb, yb, xc, yc)
!
!******************************************************************************
!
!! ANGLE computes the size of an angle in 2D.
!
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Purpose: 
!
!    Compute the interior angle in radians at vertex
!    (XB,YB) of the chain formed by the directed edges from
!    (XA,YA) to (XB,YB) to (XC,YC).  The interior is to the
!    left of the two directed edges.
!
!  Parameters:
!
!    Input, double precision XA, YA, XB, YB, XC, YC, the vertex coordinates.
!
!    Output, double precision ANGLE, the angle, between 0 and 2*PI.
!    ANGLE is set to PI/2 in the degenerate case.
!
  implicit none
!
  double precision angle
  double precision pi
  double precision t
  double precision tol
  double precision x1
  double precision x2
  double precision xa
  double precision xb
  double precision xc
  double precision y1
  double precision y2
  double precision ya
  double precision yb
  double precision yc
!
  tol = 100.0D+00 * epsilon ( tol )
  x1 = xa - xb
  y1 = ya - yb
  x2 = xc - xb
  y2 = yc - yb

  t = sqrt ( (x1**2 + y1**2)*(x2**2 + y2**2) )
  if ( t == 0.0d0 ) then
    t = 1.0d0
  end if

  t = (x1*x2 + y1*y2) / t

  if ( abs(t) > 1.0d0 - tol ) then
    t = sign(1.0d0,t)
  end if

  angle = acos(t)

  if ( x2*y1 - y2*x1 < 0.0d0 ) then
    angle = 2.0d0* pi() - angle
  end if

  return
end
function angle3 ( u, v, rtolsq )
!
!******************************************************************************
!
!! ANGLE3 computes the size of a plane angle in 3D.
!
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Purpose: 
!
!    Compute angle in range [0,PI] between 3D vectors U and V.
!
!  Parameters:
!
!    Input, U(1:3), V(1:3) - vectors.
!
!    Input, RTOLSQ - relative tolerance used to detect 0 vector based on
!    square of Euclidean length.
!
!    Output, ANGLE3 - angle between 2 vectors in range [0,PI]
!    If U or V is the 0 vector, ANGLE3 = PI is returned.
!
  implicit none
!
  double precision angle3
  double precision pi
  double precision rtolsq,u(3)
  double precision v(3)
  double precision tol
  double precision dotp,lu,lv,t
!
  tol = 100.0D+00 * epsilon ( tol )
  dotp = u(1)*v(1) + u(2)*v(2) + u(3)*v(3)
  lu = u(1)**2 + u(2)**2 + u(3)**2
  lv = v(1)**2 + v(2)**2 + v(3)**2

  if (lu > rtolsq .and. lv > rtolsq) then
    t = dotp/sqrt(lu*lv)
    if (abs(t) > 1.0d0 - tol) t = sign(1.0d0,t)
    angle3 = acos(t)
  else
    angle3 = pi()
  end if

  return
end
function areapg ( nvrt, xc, yc )
!
!******************************************************************************
!
!! AREAPG computes twice the signed area of a simple polygon.
!
!
!  Purpose: 
!
!    Compute twice the signed area of a simple polygon with
!    vertices given in circular (counter clockwise or CW) order.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NVRT, the number of vertices on the boundary of polygon (>= 3).
!
!    Input, XC(1:NVRT), YC(1:NVRT) - vertex coordinates in counter 
!    clockwise or clockwise order.
!
!    Output, AREAPG - twice the signed area of polygon, positive if 
!    counter clockwise.
!
  implicit none
!
  double precision areapg
  integer nvrt
  double precision xc(nvrt),yc(nvrt)
  integer i
  double precision sum2
!
  sum2 = xc(1)*(yc(2) - yc(nvrt)) + xc(nvrt)*(yc(1) - yc(nvrt-1))
  do i = 2, nvrt-1
    sum2 = sum2 + xc(i)*(yc(i+1) - yc(i-1))
  end do

  areapg = sum2

  return
end
function areatr ( xa, ya, xb, yb, xc, yc )
!
!******************************************************************************
!
!! AREATR computes twice the signed area of a triangle.
!
!
!  Purpose: 
!
!    Compute twice the signed area of the triangle with
!    vertices (XA,YA), (XB,YB), and (XC,YC) in counter clockwise or 
!    clockwise order.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, XA, YA, XB, YB, XC, YC - vertex coordinates.
!
!    Output, double precision AREATR, twice the signed area of triangle,
!    positive if counter clockwise.
!
  implicit none
!
  double precision areatr
  double precision xa
  double precision xb
  double precision xc
  double precision ya
  double precision yb
  double precision yc
!
  areatr = (xb - xa) * (yc - ya) - (xc - xa) * (yb - ya )

  return
end
subroutine availf ( hdavfc, nfc, maxfc, fc, ind, ierr )
!
!******************************************************************************
!
!! AVAILF returns the index of the next available record in the FC array.
!
!
!  Purpose: 
!
!    Return index of next available record in FC array,
!    either HDAVFC or NFC+1.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input/output, HDAVFC - head pointer of available records in FC.
!
!    Input/output, NFC - current number of records used in FC.
!
!    Input, integer MAXFC, the maximum number of records available in FC.
!
!    Input, FC(1:7,1:*) - array of face records; see routine DTRIS3.
!
!    Output, integer IND, the index of available record (if FC not full).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer fc(7,*)
  integer hdavfc
  integer ierr
  integer ind
  integer maxfc
  integer nfc
!
  ierr = 0

  if (hdavfc /= 0) then
    ind = hdavfc
    hdavfc = -fc(1,hdavfc)
  else
    if (nfc >= maxfc) then
      ierr = 11
    else
      nfc = nfc + 1
      ind = nfc
    end if
  end if

  return
end
subroutine availk ( k, hdavfc, nfc, maxfc, fc, pos, ierr )
!
!******************************************************************************
!
!! AVAILK returns the position of the next available record in the FC array,
!
!
!  Purpose: 
!
!    Return position of next available record in FC array,
!    either HDAVFC or NFC+1.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - number of vertices in a face.
!
!    Input/output, HDAVFC - head pointer of available records in FC.
!
!    Input/output, NFC - current number of records used in FC.
!
!    Input, MAXFC - maximum number of records available in FC.
!
!    Input, FC(1:K+4,1:*) - array of face records; see routine DTRISK.
!
!    Output, POS - position of available record (if FC not full).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
!
  integer fc(k+4,*)
  integer hdavfc
  integer ierr
  integer maxfc
  integer nfc
  integer pos
!
  ierr = 0

  if ( hdavfc /= 0 ) then
    pos = hdavfc
    hdavfc = -fc(1,hdavfc)
  else
    if (nfc >= maxfc) then
      ierr = 22
    else
      nfc = nfc + 1
      pos = nfc
    end if
  end if

  return
end
subroutine baryck ( k, ind, vcl, pt, alpha, degen, mat, ipvt )
!
!******************************************************************************
!
!! BARYCK computes the barycentric coordinates of a point in KD.
!
!
!  Purpose: 
!
!    Compute barycentric coordinates of K-D point with respect
!    to K+1 vertices of a K-D simplex.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of points and simplex,
!
!    Input, IND(1:K+1) - indices in VCL of K-D vertices of simplex.
!
!    Input, VCL(1:K,1:*) - K-D vertex coordinate list.
!
!    Input, PT(1:K) - K-D point for which barycentric coordinates computed.
!
!    Output, ALPHA(1:K+1) - barycentric coordinates (if DEGEN = .FALSE.) 
!    such that PT = ALPHA(1)*V[IND(1)] + ... + ALPHA(K+1)*V[IND(K+1)].
!
!    Output, DEGEN - .TRUE. if the K+1 vertices form a degenerate simplex.
!
!    Workspace, MAT(1:K,1:K) - matrix used for solving system of linear 
!    equations.
!
!    Workspace, IPVT(1:K-1) - pivot indices.
!
  implicit none
!
  integer k
!
  double precision alpha(k+1)
  logical degen
  integer i
  integer ind(k+1)
  integer ipvt(k-1)
  integer j
  integer l
  integer m
  double precision mat(k,k)
  double precision pt(k)
  double precision tol
  double precision vcl(k,*)
!
  tol = 100.0D+00 * epsilon ( tol )
  m = ind(k+1)

  do j = 1, k
    l = ind(j)
    do i = 1, k
      mat(i,j) = vcl(i,l) - vcl(i,m)
    end do
  end do

  alpha(1:k) = pt(1:k) - vcl(1:k,m)

  call lufac ( mat, k, k, tol, ipvt, degen )

  if ( .not. degen ) then
    call lusol ( mat, k, k, ipvt, alpha )
    alpha(k+1) = 1.0d0 - sum ( alpha(1:k) )
  end if

  return
end
subroutine baryth ( a, b, c, d, e, alpha, degen )
!
!******************************************************************************
!
!! BARYTH computes barycentric coordinates of a point in 3D.
!
!
!  Purpose: 
!
!    Compute barycentric coordinates of 3D point with respect
!    to four vertices of a tetrahedron.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A(1:3),B(1:3),C(1:3),D(1:3) - 4 vertices of tetrahedron.
!
!    Input, E(1:3) - fifth point for which barycentric coordinates found
!
!    Output, ALPHA(1:4) - scaled barycentric coordinates (if DEGEN = .FALSE.)
!    such that E = (ALPHA(1)*A + ALPHA(2)*B + ALPHA(3)*C +
!    ALPHA(4)*D)/DET where DET = 6 * (volume of tetra ABCD);
!    an ALPHA(I) may be set to 0 after tolerance test to
!    indicate that E is coplanar with a face, so sum of
!    ALPHA(I)/DET may not be 1; if the actual barycentric
!    coordinates rather than just their signs are needed,
!    modify this routine to divide ALPHA(I) by DET.
!
!    Output, DEGEN - .TRUE. iff A,B,C,D are coplanar.
!
  implicit none
!
  double precision a(3)
  double precision alpha(4)
  double precision amax
  double precision b(3)
  double precision bmax
  double precision c(3)
  double precision cmax
  double precision cp1
  double precision cp2
  double precision cp3
  double precision d(3)
  double precision da(3)
  double precision db(3)
  double precision dc(3)
  double precision de(3)
  logical degen
  double precision det
  double precision dmax
  double precision e(3)
  double precision ea(3)
  double precision eb(3)
  double precision ec(3)
  double precision emax
  integer i
  double precision tol
!
  tol = 100.0D+00 * epsilon ( tol )
  degen = .false.

  da(1:3) = a(1:3) - d(1:3)
  db(1:3) = b(1:3) - d(1:3)
  dc(1:3) = c(1:3) - d(1:3)

  amax = max(abs(a(1)),abs(a(2)),abs(a(3)))
  bmax = max(abs(b(1)),abs(b(2)),abs(b(3)))
  cmax = max(abs(c(1)),abs(c(2)),abs(c(3)))
  dmax = max(abs(d(1)),abs(d(2)),abs(d(3)))
  cp1 = db(2)*dc(3) - db(3)*dc(2)
  cp2 = db(3)*dc(1) - db(1)*dc(3)
  cp3 = db(1)*dc(2) - db(2)*dc(1)
  det = da(1)*cp1 + da(2)*cp2 + da(3)*cp3

  if (abs(det) <= 0.01d0*tol*max(amax,bmax,cmax,dmax)) then
    degen = .true.
    return
  end if

  de(1:3) = e(1:3) - d(1:3)
  ea(1:3) = a(1:3) - e(1:3)
  eb(1:3) = b(1:3) - e(1:3)
  ec(1:3) = c(1:3) - e(1:3)

  alpha(1) = de(1)*cp1 + de(2)*cp2 + de(3)*cp3
  cp1 = da(2)*de(3) - da(3)*de(2)
  cp2 = da(3)*de(1) - da(1)*de(3)
  cp3 = da(1)*de(2) - da(2)*de(1)
  alpha(2) = dc(1)*cp1 + dc(2)*cp2 + dc(3)*cp3
  alpha(3) = db(1)*cp1 + db(2)*cp2 + db(3)*cp3
  alpha(4) = ea(1)*(eb(2)*ec(3) - eb(3)*ec(2)) + ea(2)*(eb(3)*ec(1) &
    - eb(1)*ec(3)) + ea(3)*(eb(1)*ec(2) - eb(2)*ec(1))

  if (det < 0.0d0) then
    alpha(1) = -alpha(1)
    alpha(2) = -alpha(2)
    alpha(4) = -alpha(4)
  else
    alpha(3) = -alpha(3)
  end if

  emax = max(abs(e(1)),abs(e(2)),abs(e(3)))

  if ( abs(alpha(1)) <= tol*max(bmax,cmax,dmax,emax)) then
    alpha(1) = 0.0d0
  end if

  if (abs(alpha(2)) <= tol*max(amax,cmax,dmax,emax)) then
    alpha(2) = 0.0d0
  end if

  if (abs(alpha(3)) <= tol*max(amax,bmax,dmax,emax)) then
    alpha(3) = 0.0d0
  end if

  if (abs(alpha(4)) <= tol*max(amax,bmax,cmax,emax)) then
    alpha(4) = 0.0d0
  end if

  return
end
subroutine bcdtri ( rflag, nbf, nbpt, nipt, sizht, maxfc, vcl, vm, nfc, &
  nface, ntetra, fc, ht, ierr )
!
!******************************************************************************
!
!! BCDTRI constructs a boundary-constrained Delaunay triangulation in 3D.
!
!
!  Purpose: 
!
!    Construct boundary-constrained Delaunay triangulation of
!    3D vertices (based on local empty circumsphere criterion) by
!    using incremental approach and local transformations.  Vertices
!    in interior of convex hull are inserted one at a time in order
!    given by end of VM array.  The initial tetrahedra created due
!    to a new vertex are obtained by a walk through triangulation
!    until location of vertex is known.  If there are no interior
!    vertices specified, then one may be added if needed to produce
!    a boundary-constrained triangulation.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, RFLAG - .TRUE. iff return immediately (with input unchanged)
!    when NIPT = 0 and extra mesh vertex not removed.
!
!    Input, NBF - number of boundary faces or triangles.
!
!    Input, NBPT - number of vertices (points) on boundary of convex hull.
!
!    Input, NIPT - number of vertices (points) in interior of convex hull.
!
!    Input, SIZHT - size of hash table HT; a good choice is a prime number
!    which is about 1/8 * NFACE (or 3/2 * NPT for random
!    points from the uniform distribution).
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input/output, VM(1:NPT).  On input, indices of vertices of VCL 
!    being triangulated where NPT = NBPT + MAX(NIPT,1); VM(1:NBPT) 
!    are boundary points, rest are interior points; if NIPT = 0 then 
!    VM(NPT) is an interior point which may be added to triangulation;
!    interior points are inserted in order VM(NBPT+1:NPT).
!    On output, VM(NPT) is set to 0 if NIPT = 0 and extra point not needed.
!
!    Input, FC(1:3,1:NBF) - boundary triangles desired in triangulation;
!    entries are local vertex indices 1:NBPT (indices of VM)
!
!    Output, NFC - number of positions used in FC array; NFC <= MAXFC.
!
!    Output, NFACE - number of faces in triangulation; NFACE <= NFC.
!
!    Output, NTETRA - number of tetrahedra in triangulation.
!
!    Output, FC(1:7,1:NFC) - array of face records which are in linked lists
!    in hash table with direct chaining. Fields are:
!    FC(1:3,*) - A,B,C with 1<=A<B<C<=NPT; indices in VM of 3
!      vertices of face; if A <= 0, record is not used (it is
!      in linked list of avail records with indices <= NFC);
!      internal use: if B <= 0, face in queue, not in triang
!    FC(4:5,*) - D,E; indices in VM of 4th vertex of 1 or 2
!      tetrahedra with face ABC; if ABC is boundary face
!      then E = -1 (note that -E does not point to BF as in
!      routine DTRIW3 since array BF is not needed)
!    FC(6,*) - HTLINK; pointer (index in FC) of next element
!      in linked list (or NULL = 0)
!    FC(7,*) - used internally for QLINK (link for queues or
!      stacks); pointer (index in FC) of next face in queue/
!      stack (or NULL = 0); QLINK = -1 indicates face is not
!      in any queue/stack, and is output value (for records
!      not in avail list), except:
!    FC(7,2) - HDAVFC : head pointer of avail list in FC.
!
!    Output, HT(0:SIZHT-1) - hash table using direct chaining; entries are
!    head pointers of linked lists (indices of FC array)
!    containing the faces and tetrahedra of triangulation.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfc
  integer nbpt
  integer nipt
  integer sizht
!
  integer a
  integer b
  integer back
  integer bf(1)
  integer c
  integer d
  integer fc(7,maxfc)
  integer front
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrc
  integer i
  integer ierr
  integer ifac
  integer ind
  integer ivrt
  integer j
  integer, parameter :: msglvl = 0
  integer nbf
  integer nface
  integer nfc
  integer npt
  integer ntetra
  integer ptr
  logical remov
  logical rflag
  double precision vcl(3,*)
  integer vi
  integer vm(nbpt+nipt)
!
!  Initialize triangulation as first interior vertex joined to all
!  boundary faces.
!
  ierr = 0

  if (5*nbf/2 > maxfc) then
    ierr = 11
    return
  end if

  ht(0:sizht-1) = 0

  npt = nbpt + max(nipt,1)
  d = nbpt + 1
  hdavfc = 0
  nfc = nbf
  ntetra = nbf

  do i = 1,nbf
    call htins(i,fc(1,i),fc(2,i),fc(3,i),d,-1,npt,sizht,fc,ht)
  end do

  if ( msglvl == 4 ) then
    write ( *,600) nbf,nbpt,nipt
  end if

  do i = 1,nbf

    a = fc(1,i)
    b = fc(2,i)
    c = fc(3,i)

    if (msglvl == 4) then
      write ( *,610) a,b,c,d,vm(a),vm(b),vm(c), vm(d)
    end if

    do j = 1,3

      if (j == 2) then
        b = fc(3,i)
        c = fc(2,i)
      else if (j == 3) then
        a = fc(2,i)
        c = fc(1,i)
      end if

      ind = htsrc(a,b,d,npt,sizht,fc,ht)

      if (ind <= 0) then
        nfc = nfc + 1
        call htins(nfc,a,b,d,c,0,npt,sizht,fc,ht)
      else
        fc(5,ind) = c
      end if

    end do

  end do
!
!  If NIPT=0, apply local transformations to try to remove extra point.
!  Then apply local transformations based on local empty sphere criterion.
!  This latter step is also performed for NIPT > 0.  Note that BF is
!  a dummy array in call to SWAPES since BF is not referenced.
!
  if (nipt == 0) then
    call swprem(nbf,nbpt,sizht,maxfc,nfc,ntetra,vcl,vm,fc,ht,remov, ierr )
    if (ierr /= 0) return
    if (remov) vm(npt) = 0
    if (.not. remov .and. rflag) then
      return
    end if
    hdavfc = fc(7,2)
    fc(7,2) = -1
  end if

  front = 0

  do i = nbf+1,nfc

    if (fc(1,i) > 0) then
      if (front == 0) then
        front = i
      else
        fc(7,back) = i
      end if
      back = i
    end if

  end do

  if ( front /= 0 ) then
    fc(7,back) = 0
  end if

  if ( msglvl == 4 ) then
    write ( *,620) d,vm(d)
  end if

  call swapes(.true.,0,npt,sizht,nfc,maxfc,vcl,vm,bf,fc,ht,ntetra, &
    hdavfc,front,back,ind, ierr )

  if ( ierr /= 0 ) then
    return
  end if

  if (nipt <= 1) go to 90
!
!  Insert I-th vertex into pseudo-locally optimal triangulation of first I-1
!  vertices.  Walk through triangulation to find location of vertex I, create
!  new tetrahedra, apply local transformations based on empty sphere criterion.
!
  a = 1
  b = 0
  ifac = nfc

  do while ( fc(1,ifac) <= 0 )
    ifac = ifac - 1
  end do

  do i = nbpt+2,npt

    vi = vm(i)

    if ( msglvl == 4 ) then
      write ( *,620) i,vi
    end if

    if (fc(5,ifac) == i-1) then
      ivrt = 5
    else
      ivrt = 4
    end if

    call walkt3(vcl(1,vi),npt,sizht,ntetra,vcl,vm,fc,ht,ifac,ivrt, ierr )

    if (ierr == 307) then

      ierr = 0

      if (msglvl >= 2) then
        write ( *,630) i
      end if

      call lsrct3(vcl(1,vi),npt,sizht,nfc,vcl,vm,fc,ht,ifac,ivrt, ierr )

      if (ifac == 0) ierr = 331

    end if

    if (ierr /= 0) return

    if (ivrt == 6) then

      if (fc(5,ifac) <= 0) then
        ierr = 331
      else
        call nwthfc(i,ifac,npt,sizht,a,nfc,1,maxfc,bf,fc,ht, &
          ntetra,b,hdavfc,front,back, ierr )
      end if

    else if (ivrt >= 4) then

      call nwthin(i,ifac,ivrt,npt,sizht,nfc,maxfc,fc,ht,ntetra, &
        hdavfc,front,back, ierr )

    else if (ivrt >= 1) then

      call nwthed(i,ifac,ivrt,npt,sizht,a,nfc,1,maxfc,bf,fc,ht, &
        ntetra,b,hdavfc,front,back, ierr )

      if (ierr == 12) ierr = 331

    else

      ierr = 331

    end if

    if (ierr /= 0) then
      return
    end if

    call swapes(.true.,i,npt,sizht,nfc,maxfc,vcl,vm,bf,fc,ht, &
      ntetra,hdavfc,front,back,ind, ierr )

    if (ierr /= 0) return

    if (ind /= 0) ifac = ind

  end do
!
!  Make final pass based on local empty circumsphere criterion.
!
  front = 0

  do i = nbf+1,nfc
    if (fc(1,i) > 0) then
      if (front == 0) then
        front = i
      else
        fc(7,back) = i
      end if
      back = i
    end if
  end do

  if (front /= 0) then
    fc(7,back) = 0
  end if

  call swapes(.true.,0,npt,sizht,nfc,maxfc,vcl,vm,bf,fc,ht,ntetra, &
    hdavfc,front,back,ind,ierr )

  if (ierr /= 0) return

   90 continue

  nface = nfc
  ptr = hdavfc

  do while (ptr /= 0) 
    nface = nface - 1
    ptr = -fc(1,ptr)
  end do

  fc(7,2) = hdavfc

  600 format (/' bcdtri: nbf,nbpt,nipt=',3i7)
  610 format (1x,4i7,' : ',4i7)
  620 format (/1x,'step',i7,':   vertex i =',i7)
  630 format (1x,'linear search required in step',i7)

  return
end
subroutine bnsrt2 ( binexp, n, a, map, bin, iwk )
!
!******************************************************************************
!
!! BNSRT2 bin sorts a set of 2D points.
!
!
!  Purpose: 
!
!    Use bin sort to obtain the permutation of N 2-dimensional
!    double precision points so that points are in increasing bin
!    order, where the N points are assigned to about N**BINEXP bins.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, BINEXP - exponent for number of bins.
!
!    Input, N - number of points.
!
!    Input, A(1:2,1:*) - array of >= N 2D double precision points.
!
!    Input/output, MAP(1:N).  On input, the points of A with indices MAP(1),
!    MAP(2), ..., MAP(N) are to be sorted.  On output, elements are permuted 
!    so that bin of MAP(1) <= bin of MAP(2) <= ... <= bin of MAP(N).
!
!    Workspace, BIN(1:N) - used for bin numbers and permutation of 1 to N.
!
!    Workspace, IWK(1:N) - used for copy of MAP array.
!
  implicit none
!
  integer n
!
  double precision a(2,*)
  integer bin(n)
  double precision binexp
  double precision dx
  double precision dy
  integer i
  integer iwk(n)
  integer j
  integer k
  integer l
  integer map(n)
  integer nside
  double precision xmax
  double precision xmin
  double precision ymax
  double precision ymin
!
  nside = int ( real(n)**(binexp/2.0) + 0.5 )

  if ( nside <= 1 ) then
    return
  end if

  xmin = minval ( a(1,map(1:n)) )
  xmax = maxval ( a(1,map(1:n)) )
  ymin = minval ( a(2,map(1:n)) )
  ymax = minval ( a(2,map(1:n)) )

  dx = 1.0001d0*(xmax - xmin)/dble(nside)
  dy = 1.0001d0*(ymax - ymin)/dble(nside)
  if (dx == 0.0d0) dx = 1.0d0
  if (dy == 0.0d0) dy = 1.0d0

  do i = 1,n
    j = map(i)
    iwk(i) = j
    map(i) = i
    k = int((a(1,j) - xmin)/dx)
    l = int((a(2,j) - ymin)/dy)
    if (mod(k,2) == 0) then
      bin(i) = k*nside + l
    else
      bin(i) = (k+1)*nside - l - 1
    end if
  end do

  call ihpsrt(1,n,1,bin,map)

  bin(1:n) = map(1:n)
  map(1:n) = iwk(bin(1:n))

  return
end
subroutine bnsrt3 ( binexp, n, a, map, bin, iwk )
!
!******************************************************************************
!
!! BNSRT3 bin sorts a set of 3D points.
!
!
!  Purpose: 
!
!    Use bin sort to obtain the permutation of N 3-dimensional
!    double precision points so that points are in increasing bin
!    order, where the N points are assigned to about N**BINEXP bins.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, BINEXP - exponent for number of bins.
!
!    Input, N - number of points.
!
!    Input, A(1:3,1:*) - array of >= N 3D double precision points.
!
!    Input/output, MAP(1:N).  On input, the points of A with indices 
!    MAP(1), MAP(2), ..., MAP(N) are to be sorted.  On output, elements 
!    are permuted so that:
!    bin of MAP(1) <= bin of MAP(2) <= ... <= bin of MAP(N)
!
!    Workspace, BIN(1:N) - used for bin numbers and permutation of 1 to N.
!
!    Workspace, IWK(1:N) - used for copy of MAP array.
!
  implicit none
!
  integer n
!
  double precision a(3,*)
  integer bin(n)
  double precision binexp
  double precision dx
  double precision dy
  double precision dz
  integer i
  integer iwk(n)
  integer j
  integer k
  integer l
  integer m
  integer map(n)
  integer nside
  integer nsidsq
  double precision xmax
  double precision xmin
  double precision ymax
  double precision ymin
  double precision zmax
  double precision zmin
!
  nside = int(real(n)**(binexp/3.0) + 0.5)

  if (nside <= 1) then
    return
  end if

  nsidsq = nside**2

  xmin = minval ( a(1,map(1:n)) )
  xmax = maxval ( a(1,map(1:n)) )
  ymin = minval ( a(2,map(1:n)) )
  ymax = minval ( a(2,map(1:n)) )
  zmin = minval ( a(3,map(1:n)) )
  zmax = minval ( a(3,map(1:n)) )

  dx = 1.0001d0*(xmax - xmin)/dble(nside)
  dy = 1.0001d0*(ymax - ymin)/dble(nside)
  dz = 1.0001d0*(zmax - zmin)/dble(nside)
  if (dx == 0.0d0) dx = 1.0d0
  if (dy == 0.0d0) dy = 1.0d0
  if (dz == 0.0d0) dz = 1.0d0

  do i = 1,n

    j = map(i)
    iwk(i) = j
    map(i) = i
    k = int((a(1,j) - xmin)/dx)
    l = int((a(2,j) - ymin)/dy)
    m = int((a(3,j) - zmin)/dz)

    if (mod(l,2) == 0) then
      bin(i) = l*nside + m
    else
      bin(i) = (l+1)*nside - m - 1
    end if

    if (mod(k,2) == 0) then
      bin(i) = k*nsidsq + bin(i)
    else
      bin(i) = (k+1)*nsidsq - bin(i) - 1
    end if

  end do

  call ihpsrt(1,n,1,bin,map)

  bin(1:n) = map(1:n)
  map(1:n) = iwk(bin(1:n))

  return
end
subroutine bnsrtk ( k, binexp, n, a, map, bin, iwk, dx )
!
!******************************************************************************
!
!! BNSRTK bin sorts a set of KD points.
!
!
!  Purpose: 
!
!    Use bin sort to obtain the permutation of N K-dimensional
!    double precision points so that points are in increasing bin
!    order, where the N points are assigned to about N**BINEXP bins.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of points.
!
!    Input, BINEXP - exponent for number of bins.
!
!    Input, N - number of points.
!
!    Input, A(1:K,1:*) - array of >= N K-D double precision points.
!
!    Input/output, MAP(1:N).  On input, the points of A with indices 
!    MAP(1), MAP(2), ..., MAP(N) are to be sorted.  On output, the elements 
!    are permuted so that 
!      bin of MAP(1) <= bin of MAP(2) <= ... <= bin of MAP(N).
!
!    Workspace, BIN(1:N) - used for bin numbers and permutation of 1 to N.
!
!    Workspace, IWK(1:N) - used for copy of MAP array.
!
!    Workspace, DX(1:K) - used for size of range of coordinates.
!
  implicit none
!
  integer k
  integer n
!
  double precision a(k,*)
  integer b
  integer bin(n)
  double precision binexp
  double precision dx(k)
  integer i
  integer iwk(n)
  integer j
  integer l
  integer m
  integer map(n)
  integer nside
  integer nspow
  double precision xmax
  double precision xmin
!
  nside = int(real(n)**(real(binexp)/real(k)) + 0.5)
  if (nside <= 1) return

  do i = 1,k
    xmin = minval ( a(i,map(1:n)) )
    xmax = maxval ( a(i,map(1:n)) )
    dx(i) = 1.0001d0*(xmax - xmin)/dble(nside)
    if (dx(i) == 0.0d0) dx(i) = 1.0d0
  end do

  do i = 1,n

    j = map(i)
    iwk(i) = j
    map(i) = i
    b = int((a(1,j) - xmin) / dx(1))

    do l = 2, k

      m = int((a(l,j) - xmin) / dx(l))

      if (l == 2) then
        nspow = nside
      else
        nspow = nspow*nside
      end if

      if (mod(m,2) == 0) then
        b = m*nspow + b
      else
        b = (m+1)*nspow - b - 1
      end if

    end do

    bin(i) = b

  end do

  call ihpsrt(1,n,1,bin,map)

  bin(1:n) = map(1:n)
  map(1:n) = iwk(bin(1:n))

  return
end
function ccradi ( a, b, c, d )
!
!******************************************************************************
!
!! CCRADI computes the circumradius of a tetrahedron.
!
!
!  Purpose: 
!
!    Compute circumradius of tetrahedron [actually
!    1/(4*circumradius)**2 is computed].
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A(1:3), B(1:3), C(1:3), D(1:3) - 4 vertices of tetrahedron.
!
!    Output, CCRADI - 1/(4*circumradius)**2.
!
  implicit none
!
  double precision a(3)
  double precision ab(3)
  double precision ac(3)
  double precision ad(3)
  double precision b(3)
  double precision bc(3)
  double precision bd(3)
  double precision c(3)
  double precision ccradi
  double precision cd(3)
  double precision d(3)
  double precision denom
  integer i
  double precision lab
  double precision lac
  double precision lad
  double precision lbc
  double precision lbd
  double precision lcd
  double precision pb
  double precision pc
  double precision pd
  double precision t1
  double precision t2
  double precision vol
!
  ab(1:3) = b(1:3) - a(1:3)
  ac(1:3) = c(1:3) - a(1:3)
  ad(1:3) = d(1:3) - a(1:3)
  bc(1:3) = c(1:3) - b(1:3)
  bd(1:3) = d(1:3) - b(1:3)
  cd(1:3) = d(1:3) - c(1:3)

  lab = ab(1)**2 + ab(2)**2 + ab(3)**2
  lac = ac(1)**2 + ac(2)**2 + ac(3)**2
  lad = ad(1)**2 + ad(2)**2 + ad(3)**2
  lbc = bc(1)**2 + bc(2)**2 + bc(3)**2
  lbd = bd(1)**2 + bd(2)**2 + bd(3)**2
  lcd = cd(1)**2 + cd(2)**2 + cd(3)**2
  pb = sqrt(lab*lcd)
  pc = sqrt(lac*lbd)
  pd = sqrt(lad*lbc)
  t1 = pb + pc
  t2 = pb - pc
  denom = (t1+pd)*(t1-pd)*(pd+t2)*(pd-t2)
 
  if ( denom <= 0.0d0 ) then
    ccradi = 0.0d0
    return
  end if

  vol = ab(1)*(ac(2)*ad(3) - ac(3)*ad(2)) + ab(2)*(ac(3)*ad(1) &
    - ac(1)*ad(3)) + ab(3)*(ac(1)*ad(2) - ac(2)*ad(1))

  ccradi = vol**2 / denom

  return
end
subroutine ccsph ( intest, a, b, c, d, e, center, radsq, in )
!
!******************************************************************************
!
!! CCSPH finds the circumsphere through the vertices of a tetrahedron.
!
!
!  Purpose: 
!
!    Find center and square of radius of circumsphere through
!    four vertices of a tetrahedron, and possibly determine whether
!    a fifth 3D point is inside sphere.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, INTEST - .TRUE. iff test for fifth point in sphere to be made.
!
!    Input, A(1:3), B(1:3), C(1:3), D(1:3) - 4 vertices of tetrahedron.
!
!    Input, E(1:3) - fifth point; referenced iff INTEST is .TRUE.
!
!    Output, CENTER(1:3) - center of sphere; undefined if A,B,C,D coplanar.
!
!    Output, RADSQ - square of radius of sphere; -1 if A,B,C,D coplanar.
!
!    Output, IN - contains following value if INTEST is .TRUE.:
!    2 if A,B,C,D coplanar; 1 if E inside sphere;
!    0 if E on sphere; -1 if E outside sphere
!
  implicit none
!
  double precision a(3)
  double precision b(3)
  double precision c(3)
  double precision center(3)
  double precision cmax
  double precision cp1
  double precision cp2
  double precision cp3
  double precision d(3)
  double precision det
  double precision dsq
  double precision e(3)
  integer i
  integer in
  logical intest
  double precision radsq
  double precision tol

  double precision da(3),db(3),dc(3),rhs(3)
!
  tol = 100.0D+00 * epsilon ( tol )
  da(1:3) = a(1:3) - d(1:3)
  db(1:3) = b(1:3) - d(1:3)
  dc(1:3) = c(1:3) - d(1:3)

  rhs(1) = 0.5d0*(da(1)**2 + da(2)**2 + da(3)**2)
  rhs(2) = 0.5d0*(db(1)**2 + db(2)**2 + db(3)**2)
  rhs(3) = 0.5d0*(dc(1)**2 + dc(2)**2 + dc(3)**2)

  cmax = max ( &
    abs(a(1)), abs(a(2)), abs(a(3)), &
    abs(b(1)), abs(b(2)), abs(b(3)), &
    abs(c(1)), abs(c(2)), abs(c(3)), &
    abs(d(1)), abs(d(2)), abs(d(3)) )

  cp1 = db(2)*dc(3) - dc(2)*db(3)
  cp2 = dc(2)*da(3) - da(2)*dc(3)
  cp3 = da(2)*db(3) - db(2)*da(3)
  det = da(1)*cp1 + db(1)*cp2 + dc(1)*cp3

  if (abs(det) <= 0.01d0*tol*cmax) then
    radsq = -1.0d0
    in = 2
    return
  end if

  center(1) = (rhs(1)*cp1 + rhs(2)*cp2 + rhs(3)*cp3)/det
  cp1 = db(1)*rhs(3) - dc(1)*rhs(2)
  cp2 = dc(1)*rhs(1) - da(1)*rhs(3)
  cp3 = da(1)*rhs(2) - db(1)*rhs(1)
  center(2) = (da(3)*cp1 + db(3)*cp2 + dc(3)*cp3)/det
  center(3) = -(da(2)*cp1 + db(2)*cp2 + dc(2)*cp3)/det
  radsq = center(1)**2 + center(2)**2 + center(3)**2

  center(1:3) = center(1:3) + d(1:3)

  if (intest) then

    dsq = sum ( ( e(1:3) - center(1:3) )**2 )

    if ( dsq > (1.0d0 + tol) * radsq ) then
      in = -1
    else if ( dsq < (1.0d0 - tol) * radsq ) then
      in = 1
    else
      in = 0
    end if

  end if

  return
end
subroutine ccsphk ( k, intest, ind, vcl, pt, center, radsq, in, mat, ipvt )
!
!******************************************************************************
!
!! CCSPHK finds the circumsphere through a simplex in KD.
!
!
!  Purpose: 
!
!    Find center and square of radius of circumsphere through
!    K+1 vertices of a K-D simplex, and possibly determine whether
!    another K-D point is inside (hyper)sphere.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of points and simplex.
!
!    Input, INTEST - .TRUE. iff test for point PT in sphere to be made.
!
!    Input, IND(1:K+1) - indices in VCL of K-D vertices of simplex.
!
!    Input, VCL(1:K,1:*) - K-D vertex coordinate list.
!
!    Input, PT(1:K) - K-D point for which sphere test is applied
!    (referenced iff INTEST is .TRUE.).
!
!    Output, CENTER(1:K) - center of circumsphere; undefined if K+1 vertices
!    of simplex lie on same K-D hyperplane.
!
!    Output, RADSQ - square of radius of sphere; -1 in degenerate case.
!
!    Output, IN - contains following value if INTEST is .TRUE.:
!     2 if degenerate simplex; 
!     1 if PT inside sphere;
!     0 if PT on sphere; 
!    -1 if PT outside sphere
!
!    Workspace, MAT(1:K,1:K) - matrix used for solving system of linear 
!    equations.
!
!    Workspace, IPVT(1:K-1) - pivot indices
!
  implicit none
!
  integer k
!
  double precision center(k)
  double precision dsq
  integer i
  integer in
  integer ind(k+1)
  logical intest
  integer ipvt(k-1)
  integer j
  integer l
  integer m
  double precision mat(k,k)
  double precision pt(k)
  double precision radsq
  logical singlr
  double precision sum2
  double precision tol
  double precision vcl(k,*)
!
  tol = 100.0D+00 * epsilon ( tol )
  m = ind(k+1)

  do i = 1,k
    l = ind(i)
    sum2 = 0.0d0
    do j = 1,k
      mat(i,j) = vcl(j,l) - vcl(j,m)
      sum2 = sum2 + mat(i,j)**2
    end do
    center(i) = 0.5d0*sum2
  end do

  call lufac ( mat, k, k, tol, ipvt, singlr )

  if (singlr) then
    in = 2
    radsq = -1.0d0
  else
    call lusol(mat,k,k,ipvt,center)
    radsq = 0.0d0
    do i = 1,k
      radsq = radsq + center(i)**2
      center(i) = center(i) + vcl(i,m)
    end do
  end if

  if (intest .and. .not. singlr) then

   dsq = 0.0d0
   do i = 1,k
     dsq = dsq + (pt(i) - center(i))**2
   end do

   if (dsq > (1.0d0 + tol)*radsq) then
     in = -1
   else if (dsq < (1.0d0 - tol)*radsq) then
     in = 1
   else
     in = 0
   end if

  end if

  return
end
function cmcirc ( x0, y0, x1, y1, x2, y2, x3, y3 )
!
!******************************************************************************
!
!! CMCIRC determines if a point is in the circumcircle of three points.
!
!
!  Purpose: 
!
!    Determine whether (X0,Y0) is in the circumcircle through
!    the three points (X1,Y1), (X2,Y2), (X3,Y3).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, X0, Y0, X1, Y1, X2, Y2, X3, Y3 - vertex coordinates.
!
!    Output, CMCIRC -  
!     2 if three vertices collinear,
!     1 if (X0,Y0) inside circle,
!     0 if (X0,Y0) on circle,
!    -1 if (X0,Y0) outside circle
!
  implicit none
!
  double precision a11
  double precision a12
  double precision a21
  double precision a22
  double precision b1
  double precision b2
  integer cmcirc
  double precision det
  double precision diff
  double precision rsq
  double precision tol
  double precision tolabs
  double precision x0
  double precision x1
  double precision x2
  double precision x3
  double precision xc
  double precision y0
  double precision y1
  double precision y2
  double precision y3
  double precision yc
!
  tol = 100.0D+00 * epsilon ( tol )
  cmcirc = 2
  a11 = x2 - x1
  a12 = y2 - y1
  a21 = x3 - x1
  a22 = y3 - y1
  tolabs = tol*max(abs(a11),abs(a12),abs(a21),abs(a22))
  det = a11*a22 - a21*a12
  if (abs(det) <= tolabs) return
  b1 = a11**2 + a12**2
  b2 = a21**2 + a22**2
  det = det + det
  xc = (b1*a22 - b2*a12)/det
  yc = (b2*a11 - b1*a21)/det
  rsq = xc**2 + yc**2
  diff = ((x0 - x1 - xc)**2 + (y0 - y1 - yc)**2) - rsq
  tolabs = tol*rsq

  if (diff < -tolabs) then
    cmcirc = 1
  else if (diff > tolabs) then
    cmcirc = -1
  else
    cmcirc = 0
  end if

  return
end
subroutine cutfac ( p, nrmlc, angacc, dtol, nvc, maxvc, vcl, facep, nrml, &
  fvl, eang, nedgc, pedge, nce, cedge, cdang, rflag, ierr )
!
!******************************************************************************
!
!! CUTFAC traces a cut face of a polyhedron from a starting edge.
!
!
!  Purpose: 
!
!    Trace out cut face in polyhedron P given a starting edge,
!    for example, a reflex edge or an edge in the interior of a face.
!    Accept cut face if it creates no small angles, it is an outer
!    boundary and there are no holes (inner polygons) in it. It is
!    assumed starting edge does not lie on a double-occurring face.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, P - polyhedron index.
!
!    Input, NRMLC(1:4) - unit normal vector of cut plane plus right hand
!    side constant term of plane equation.
!
!    Input, ANGACC - minimum acceptable dihedral angle in radians produced by
!    a cut face.
!
!    Input, DTOL - absolute tolerance to determine if point is on cut plane.
!
!    Input, NVC - number of vertex coordinates.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input, FACEP(1:3,1:*) - face pointer list.
!
!    Input, NRML(1:3,1:*) - unit normal vectors for faces.
!
!    Input, FVL(1:6,1:*) - face vertex list.
!
!    Input, EANG(1:*) - edge angles.
!
!    Input/output, NEDGC - number of edges which intersect cut plane.
!
!    Input/output, PEDGE(1:3,1:NEDGC) - edges of polyhedron P that intersect 
!    cut plane excluding starting edge if it is an edge of P or the edge
!    containing CEDGE(1,1) if CEDGE(1,1) > NVC;
!    PEDGE(1,I), PEDGE(2,I) are indices of FVL; if PEDGE(1,I)
!    = A and B = FVL(SUCC,A) then PEDGE(3,I) = 10*CA+CB where
!    CA = 1, 2, or 3 if vertex A in negative half-space, on
!    cut plane, or in positive half-space determined by cut
!    plane, and similarly for CB.
!
!    Input, CEDGE(1:2,0:1) - CEDGE(1,0) = LV, CEDGE(1,1) = LU where LU, LV
!    are indices of VCL and are vertices of starting edge; if
!    called by RESEDG, LU < LV <= NVC are on reflex edge and
!    CEDGE(2,1) = U is index of FVL indicating reflex edge;
!    LV may be NVC+1 and LU may be NVC+1 or NVC+2 to indicate
!    a point on interior of an edge; CEDGE(2,1) is specified
!    as described for output below; if LV > NVC, CEDGE(2,0)
!    takes on the value ABS(CEDGE(2,NCE)) described for output
!    below, else CEDGE(2,0) is not used.
!
!    Input, CDANG(1) - dihedral angle at starting edge determined by cut
!    plane in positive half-space.
!
!    Output, NCE - number of edges in cut polygon; it is assumed there is
!    enough space in the following two arrays.
!
!    Output, CEDGE(1:2,0:NCE) - CEDGE(1,I) is an index of VCL, indices > NVC
!    are new points; CEDGE(2,I) = J indicates that edge of cut
!    face ending at CEDGE(1,I) is edge from J to FVL(SUCC,J)
!    if J > 0; else if J < 0 then edge of cut face ending at
!    CEDGE(1,I) is a new edge and CEDGE(1,I) lies on edge from
!    -J to FVL(SUC,-J) and new edge lies in face FVL(FACN,-J);
!    CEDGE(2,I) always refers to an edge in the subpolyhedron
!    in negative half-space; CEDGE(1,NCE) = CEDGE(1,0).
!
!    Output, CDANG(1:NCE) - dihedral angles created by edges of cut polygon
!    in positive half-space; negative sign for angle I indicates that
!    face containing edge I is oriented clockwise in polyhedron P.
!
!    Output, RFLAG - .TRUE. iff reflex or starting edge is resolved
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer, parameter :: maxev = 20
  integer maxvc
  integer nedgc
!
  integer a
  double precision ang
  double precision angacc
  double precision angr
  integer ca
  integer cb
  integer ccwfl
  double precision cdang(*)
  integer cedge(2,0:*)
  double precision cmax
  double precision cp(3)
  double precision de(3)
  double precision dee(3)
  double precision dir(3)
  double precision dir1(3)
  double precision dirsq
  double precision dir1sq
  double precision dist
  logical dof
  double precision dotp
  double precision dsave(3)
  double precision dtol
  integer e
  double precision eang(*)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer ee
  logical eflag
  integer estrt
  integer estop
  integer ev(maxev)
  integer f
  integer facep(3,*)
  integer, parameter :: facn = 2
  integer fl
  integer fr
  integer fvl(6,*)
  integer i
  double precision iamin
  integer ierr
  integer imin
  integer inout
  double precision intang
  integer isave
  integer j
  integer k
  integer kmax
  integer l
  integer la
  integer lb
  integer, parameter :: loc = 1
  integer lv
  integer lw
  integer lw1
  integer, parameter :: msglvl = 0
  integer n
  integer nce
  integer nev
  double precision nmax
  double precision nrml(3,*)
  double precision nrmlc(4)
  double precision ntol
  integer nvc
  integer p
  integer pedge(3,nedgc)
  double precision pi
  double precision pi2
  integer, parameter :: pred = 4
  logical rflag
  double precision rhs(3)
  double precision s
  integer sf
  integer sgn
  integer sp
  integer, parameter :: succ = 3
  double precision t
  double precision tmin
  double precision tol
  double precision vcl(3,maxvc)
  integer w
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  rflag = .false.
  pi2 = 2.0D+00 * pi()
  n = max(nvc, cedge(1,0), cedge(1,1))
  nce = 1
  w = cedge(2,1)
  lv = cedge(1,0)
  lw = lv
  lw1 = cedge(1,1)

  if (lw1 <= nvc) then
    nev = 1
    ev(nev) = lw1
  else
    nev = 0
  end if

  if (w > 0) then

    fl = fvl(facn,w)

    if (lw > lw1) then
      fr = fvl(facn,fvl(edgc,w))
    else
      fr = fvl(facn,fvl(edga,w))
    end if

  else

    fl = fvl(facn,-w)
    fr = fl

  end if

  dir(1) = vcl(1,lw1) - vcl(1,lw)
  dir(2) = vcl(2,lw1) - vcl(2,lw)
  dir(3) = vcl(3,lw1) - vcl(3,lw)

  kmax = 1
  if (abs(nrmlc(2)) > abs(nrmlc(1))) kmax = 2
  if (abs(nrmlc(3)) > abs(nrmlc(kmax))) kmax = 3

  if (abs(facep(2,fl)) == p) then
    ccwfl = facep(2,fl)
  else
    ccwfl = facep(3,fl)
  end if

  if (ccwfl < 0) then
    cdang(1) = -cdang(1)
  end if
!
!  LW, LW1, FL, FR, DIR are updated before each iteration of loop.
!  CCWFL = P (-P) if FL is counter clockwise (clockwise) according 
!  to SUCC traversal.
!
10 continue

  if (lw1 > nvc) then
!
!  LW1 is new vertex on interior of edge E.  FL is used for
!  previous and next faces containing edges of cut polygon.
!
    e = -cedge(2,nce)
    la = fvl(loc,e)
    lb = fvl(loc,fvl(succ,e))

    if ((lb - la)*ccwfl > 0) then
      ee = fvl(edgc,e)
    else
      ee = fvl(edga,e)
    end if

    fl = fvl(facn,ee)
    dof = (abs(facep(2,fl)) == abs(facep(3,fl)))

    if (dof) then
      l = fvl(loc,ee)
      if (l == la) ccwfl = -ccwfl
    else if (abs(facep(2,fl)) == p) then
      ccwfl = facep(2,fl)
    else
      ccwfl = facep(3,fl)
    end if

    dir(1) = nrmlc(2)*nrml(3,fl) - nrmlc(3)*nrml(2,fl)
    dir(2) = nrmlc(3)*nrml(1,fl) - nrmlc(1)*nrml(3,fl)
    dir(3) = nrmlc(1)*nrml(2,fl) - nrmlc(2)*nrml(1,fl)

    if (abs(facep(2,fl)) /= p .or. dof .and. ccwfl < 0) then
      dir(1) = -dir(1)
      dir(2) = -dir(2)
      dir(3) = -dir(3)
    end if

    go to 70

  else
!
!  LW1 is existing vertex of polyhedron P. FL (FL and FR) is
!  previous face if edge ending at LW1 is new (already exists).
!  In former case, -CEDGE(2,NCE) is the edge of FL incident on
!  LW1 which will lie only in subpolyhedron PL.  In latter case,
!  CEDGE(2,NCE) is an edge of FL.  Cycle through edges, faces counter clockwise
!  (from outside) between edges ESTRT, ESTOP.
!  If LW1 lies on a doubly-occurring face, there are 2 cycles
!  around LW1 and the correct one is chosen based on CCWFL.
!
    iamin = pi2
    imin = 0
    dirsq = dir(1)**2 + dir(2)**2 + dir(3)**2
    eflag = (cedge(2,nce) > 0)

    if (.not. eflag) then

      estrt = -cedge(2,nce)
      sp = ccwfl

      if (ccwfl > 0) then
        estop = fvl(succ,estrt)
      else
        estop = fvl(pred,estrt)
      end if

    else

      w = cedge(2,nce)
      la = fvl(loc,w)

      if (ccwfl > 0) then
        estrt = fvl(pred,w)
      else
        estrt = fvl(succ,w)
      end if

      if (la == lw) then
        l = lw1 - lw
      else
        l = lw - lw1
      end if

      if (l*ccwfl > 0) then
        w = fvl(edgc,w)
      else
        w = fvl(edga,w)
      end if

      if (abs(facep(2,fr)) == abs(facep(3,fr))) then

        lb = fvl(loc,w)

        if (la == lb) then
          sp = -ccwfl
        else
          sp = ccwfl
        end if

      else if (abs(facep(2,fr)) == p) then

        sp = facep(2,fr)

      else

        sp = facep(3,fr)

      end if

      if (sp > 0) then
        estop = fvl(succ,w)
      else
        estop = fvl(pred,w)
      end if

    end if

    la = fvl(loc,estop)
    lb = fvl(loc,fvl(succ,estop))

    if ((lb - la)*sp > 0) then
      estop = fvl(edgc,estop)
    else
      estop = fvl(edga,estop)
    end if

    e = estrt
    sf = ccwfl

20  continue

    if (eflag .or. (e /= estrt .and. e /= estop)) then

      if (fvl(loc,e) == lw1) then
        l = fvl(loc,fvl(succ,e))
      else
        l = fvl(loc,e)
      end if

      dist = nrmlc(1)*vcl(1,l) + nrmlc(2)*vcl(2,l) + &
        nrmlc(3)*vcl(3,l) - nrmlc(4)

      if (abs(dist) <= dtol) then

        dir1(1) = vcl(1,l) - vcl(1,lw1)
        dir1(2) = vcl(2,l) - vcl(2,lw1)
        dir1(3) = vcl(3,l) - vcl(3,lw1)
        dir1sq = dir1(1)**2 + dir1(2)**2 + dir1(3)**2
        dotp = -(dir(1)*dir1(1) + dir(2)*dir1(2) + dir(3)* &
          dir1(3))/sqrt(dirsq*dir1sq)

        if (abs(dotp) > 1.0d0 - tol) then
          dotp = sign(1.0d0,dotp)
        end if

        if (kmax == 1) then
          cp(1) = dir(2)*dir1(3) - dir(3)*dir1(2)
        else if (kmax == 2) then
          cp(2) = dir(3)*dir1(1) - dir(1)*dir1(3)
        else
          cp(3) = dir(1)*dir1(2) - dir(2)*dir1(1)
        end if

        if (abs(cp(kmax)) <= tol*max(abs(dir(1)), &
          abs(dir(2)),abs(dir(3)),abs(dir1(1)), &
          abs(dir1(2)),abs(dir1(3)))) then

          intang = pi()
        else if (cp(kmax)*nrmlc(kmax) > 0.0d0) then
          intang = acos(dotp)
        else
          intang = pi2 - acos(dotp)
        end if

        if (intang < iamin) then
          iamin = intang
          imin = e
          dsave(1) = dir1(1)
          dsave(2) = dir1(2)
          dsave(3) = dir1(3)
        end if

      end if

    end if

    if (e == estop) go to 40
    la = fvl(loc,e)
    lb = fvl(loc,fvl(succ,e))

    if ((lb - la)*sf > 0) then
      e = fvl(edgc,e)
    else
      e = fvl(edga,e)
    end if

    f = fvl(facn,e)
    dof = (abs(facep(2,f)) == abs(facep(3,f)))

    if (dof) then
      l = fvl(loc,e)
      if (l == la) sf = -sf
    else if (abs(facep(2,f)) == p) then
      sf = facep(2,f)
    else
      sf = facep(3,f)
    end if

    if (sf > 0) then
      ee = fvl(pred,e)
      la = fvl(loc,fvl(succ,e))
      lb = fvl(loc,ee)
    else
      ee = fvl(succ,e)
      la = fvl(loc,e)
      lb = fvl(loc,fvl(succ,ee))
    end if

    dir1(1) = nrmlc(2)*nrml(3,f) - nrmlc(3)*nrml(2,f)
    dir1(2) = nrmlc(3)*nrml(1,f) - nrmlc(1)*nrml(3,f)
    dir1(3) = nrmlc(1)*nrml(2,f) - nrmlc(2)*nrml(1,f)

    if (max(abs(dir1(1)), abs(dir1(2)), abs(dir1(3))) <= tol) then
      go to 30
    end if

    sgn = 1

    if (abs(facep(2,f)) /= p .or. dof .and. sf < 0) then
      dir1(1) = -dir1(1)
      dir1(2) = -dir1(2)
      dir1(3) = -dir1(3)
      sgn = -1
    end if

    k = 1
    if (abs(nrml(2,f)) > abs(nrml(1,f))) k = 2
    if (abs(nrml(3,f)) > abs(nrml(k,f))) k = 3
    nmax = sgn*nrml(k,f)
    de(1) = vcl(1,la) - vcl(1,lw1)
    de(2) = vcl(2,la) - vcl(2,lw1)
    de(3) = vcl(3,la) - vcl(3,lw1)
    dee(1) = vcl(1,lb) - vcl(1,lw1)
    dee(2) = vcl(2,lb) - vcl(2,lw1)
    dee(3) = vcl(3,lb) - vcl(3,lw1)
    ntol = tol*max(abs(de(1)), abs(de(2)), abs(de(3)), &
      abs(dee(1)), abs(dee(2)), abs(dee(3)))

    if (k == 1) then
      cp(1) = de(2)*dee(3) - de(3)*dee(2)
    else if (k == 2) then
      cp(2) = de(3)*dee(1) - de(1)*dee(3)
    else
      cp(3) = de(1)*dee(2) - de(2)*dee(1)
    end if

    if (abs(cp(k)) <= ntol .or. cp(k)*nmax > 0.0d0) then
      if (k == 1) cp(1) = de(2)*dir1(3) - de(3)*dir1(2)
      if (k == 2) cp(2) = de(3)*dir1(1) - de(1)*dir1(3)
      if (k == 3) cp(3) = de(1)*dir1(2) - de(2)*dir1(1)
      if (abs(cp(k)) <= ntol .or. cp(k)*nmax < 0.0d0) then
        go to 30
      end if
      if (k == 1) cp(1) = dir1(2)*dee(3) - dir1(3)*dee(2)
      if (k == 2) cp(2) = dir1(3)*dee(1) - dir1(1)*dee(3)
      if (k == 3) cp(3) = dir1(1)*dee(2) - dir1(2)*dee(1)
      if (abs(cp(k)) <= ntol .or. cp(k)*nmax < 0.0d0) then
        go to 30
      end if

    else

      if (k == 1) cp(1) = dir1(2)*de(3) - dir1(3)*de(2)
      if (k == 2) cp(2) = dir1(3)*de(1) - dir1(1)*de(3)
      if (k == 3) cp(3) = dir1(1)*de(2) - dir1(2)*de(1)

      if (abs(cp(k)) <= ntol .or. cp(k)*nmax > 0.0d0) then

        if (k == 1) cp(1) = dee(2)*dir1(3)-dee(3)*dir1(2)
        if (k == 2) cp(2) = dee(3)*dir1(1)-dee(1)*dir1(3)
        if (k == 3) cp(3) = dee(1)*dir1(2)-dee(2)*dir1(1)
        if (abs(cp(k)) <= ntol .or. cp(k)*nmax > 0.0d0) then
          go to 30
        end if

      end if

    end if

    dir1sq = dir1(1)**2 + dir1(2)**2 + dir1(3)**2
    dotp = -(dir(1)*dir1(1) + dir(2)*dir1(2) + dir(3)* &
      dir1(3))/sqrt(dirsq*dir1sq)
    if (abs(dotp) > 1.0d0 - tol) dotp = sign(1.0d0,dotp)

    if (kmax == 1) then
      cp(1) = dir(2)*dir1(3) - dir(3)*dir1(2)
    else if (kmax == 2) then
      cp(2) = dir(3)*dir1(1) - dir(1)*dir1(3)
    else
      cp(3) = dir(1)*dir1(2) - dir(2)*dir1(1)
    end if

    if (abs(cp(kmax)) <= tol*max(abs(dir(1)), &
      abs(dir(2)),abs(dir(3)),abs(dir1(1)), &
      abs(dir1(2)),abs(dir1(3)))) then
      intang = pi()
    else if (cp(kmax)*nrmlc(kmax) > 0.0d0) then
      intang = acos(dotp)
    else
      intang = pi2 - acos(dotp)
    end if

    if (intang < iamin) then
      iamin = intang
      imin = -f
      ccwfl = sf
      dsave(1) = dir1(1)
      dsave(2) = dir1(2)
      dsave(3) = dir1(3)
    end if

30  continue

    e = ee
    go to 20

40  continue

    if (imin == 0) then

      return

    else if (imin > 0) then

      dir(1) = dsave(1)
      dir(2) = dsave(2)
      dir(3) = dsave(3)
      lw = lw1
      la = fvl(loc,imin)
      lb = fvl(loc,fvl(succ,imin))

      if (la == lw1) then
        lw1 = lb
      else
        lw1 = la
      end if

      nce = nce + 1
      cedge(1,nce) = lw1
      cedge(2,nce) = imin
      fl = fvl(facn,imin)
      dof = (abs(facep(2,fl)) == abs(facep(3,fl)))

      if (dof) then

        if (la == lw) then
          ccwfl = -p
        else
          ccwfl = p
        end if

      else if (abs(facep(2,fl)) == p) then

        ccwfl = facep(2,fl)

      else

        ccwfl = facep(3,fl)

      end if

      if ((lb - la)*ccwfl > 0) then
        fr = fvl(facn,fvl(edgc,imin))
      else
        fr = fvl(facn,fvl(edga,imin))
      end if

      k = 1

50    continue

      if (pedge(1,k) == imin .or. pedge(2,k) == imin) then

        do i = 1,3
          j = pedge(i,k)
          pedge(i,k) = pedge(i,nedgc)
          pedge(i,nedgc) = j
        end do

        nedgc = nedgc - 1

      else

        k = k + 1
        go to 50

      end if

      go to 110

    else

      dir(1) = dsave(1)
      dir(2) = dsave(2)
      dir(3) = dsave(3)
      fl = -imin
      go to 70

    end if

  end if
!
!  Determine LW1 from direction DIR in interior of face FL.
!
70 continue

  lw = lw1
  fr = 0
  imin = 0
  tmin = 0.0d0
  k = 1
  if (abs(dir(2)) > abs(dir(1))) k = 2
  if (abs(dir(3)) > abs(dir(k))) k = 3
  ntol = tol*abs(dir(k))

  do i = 1,nedgc

    e = pedge(1,i)
    ee = pedge(2,i)

    if (fvl(facn,e) == fl) then
      a = e
    else if (fvl(facn,ee) == fl) then
      a = ee
    else
      cycle
    end if

    ca = pedge(3,i)/10
    cb = mod(pedge(3,i),10)

    if (ca == 2) then

      la = fvl(loc,a)

      if (cb == 2) then

        lb = fvl(loc,fvl(succ,a))
        s = (vcl(k,la) - vcl(k,lw))/dir(k)
        t = (vcl(k,lb) - vcl(k,lw))/dir(k)

        if (s > 0.0d0) then

          if (min(s,t) < tmin .or. imin == 0) then

            if (s < t) then

              if (ccwfl < 0) then
                imin = a
                lw1 = la
                tmin = s
              end if

            else

              if (ccwfl > 0) then
                imin = a
                lw1 = lb
                tmin = t
              end if

            end if
 
          end if

        end if

      else
 
        l = fvl(loc,e)

        if (l == la .and. ccwfl < 0 .or. l /= la .and. ccwfl > 0) then
  
          t = (vcl(k,l) - vcl(k,lw))/dir(k)

          if (t > ntol) then

            if (t < tmin .or. imin == 0) then
              lw1 = l
              imin = a
              tmin = t
            end if

          end if

        end if

      end if

    else if (cb == 2) then

      la = fvl(loc,a)
      l = fvl(loc,fvl(succ,e))

      if (l == la .and. ccwfl < 0 .or. l /= la .and. ccwfl > 0) then

        t = (vcl(k,l) - vcl(k,lw))/dir(k)
 
        if (t > ntol) then

          if (t < tmin .or. imin == 0) then
            lw1 = l
            imin = a
            tmin = t
          end if

        end if

      end if

    else

      la = fvl(loc,e)
      lb = fvl(loc,fvl(succ,e))
      dir1(1) = vcl(1,la) - vcl(1,lb)
      dir1(2) = vcl(2,la) - vcl(2,lb)
      dir1(3) = vcl(3,la) - vcl(3,lb)
      rhs(1) = vcl(1,la) - vcl(1,lw)
      rhs(2) = vcl(2,la) - vcl(2,lw)
      rhs(3) = vcl(3,la) - vcl(3,lw)
      cp(1) = dir(2)*dir1(3) - dir(3)*dir1(2)
      cp(2) = dir(3)*dir1(1) - dir(1)*dir1(3)
      cp(3) = dir(1)*dir1(2) - dir(2)*dir1(1)
      l = 1
      if (abs(cp(2)) > abs(cp(1))) l = 2
      if (abs(cp(3)) > abs(cp(l))) l = 3

      if (l == 1) then
        t = (rhs(2)*dir1(3) - rhs(3)*dir1(2))/cp(1)
      else if (l == 2) then
        t = (rhs(3)*dir1(1) - rhs(1)*dir1(3))/cp(2)
      else
        t = (rhs(1)*dir1(2) - rhs(2)*dir1(1))/cp(3)
      end if

      if (t > ntol) then

        if (t < tmin .or. imin == 0) then
          imin = -a
          tmin = t
          isave = i
        end if

      end if

    end if

  end do

  if (imin == 0) then
    return
  end if

  if (imin < 0) then

    if (lv > nvc) then

      if (-imin == cedge(2,0)) then
        lw1 = lv
        go to 90
      end if

    end if

    n = n + 1

    if (n > maxvc) then
      ierr = 14
      return
    end if

    lw1 = n
    vcl(1,n) = vcl(1,lw) + tmin*dir(1)
    vcl(2,n) = vcl(2,lw) + tmin*dir(2)
    vcl(3,n) = vcl(3,lw) + tmin*dir(3)

90  continue

    if (abs(facep(2,fl)) /= abs(facep(3,fl))) then
      do i = 1,3
        j = pedge(i,isave)
        pedge(i,isave) = pedge(i,nedgc)
        pedge(i,nedgc) = j
      end do
      nedgc = nedgc - 1
    end if

  end if

  nce = nce + 1
  cedge(1,nce) = lw1
  cedge(2,nce) = -abs(imin)
!
!  If vertex of cut polygon has appeared before, then cut polygon
!  is simply-connected (non-simple), so reject cut plane.
!
110 continue

  if (lw1 == lv) go to 150

  if (lw1 <= nvc) then

    do i = 1,nev
      if (lw1 == ev(i)) then
        if (msglvl == 4) then
          write ( *,600) 'rejected due to simply-connected polygon: case 1'
        end if
        return
      end if
    end do

    if (nev >= maxev) then
      ierr = 328
      return
    end if

    nev = nev + 1
    ev(nev) = lw1

  else

    do i = nvc+1,n-1

      do j = 1,3
        cmax = max( abs(vcl(j,i)),abs(vcl(j,n)) )
        if (abs(vcl(j,i) - vcl(j,n)) > tol*cmax .and. &
          cmax > tol) go to 140
      end do

      if (msglvl == 4) then
        write ( *,600) 'rejected due to simply-connected polygon: case 2'
      end if

      return

140   continue

    end do

  end if
!
!  Compute dihedral angles due to cut plane at edge. If any angle
!  is too small, reject cut plane.
!
150 continue

  if (fr == 0) then

    f = fl
    dof = (abs(facep(2,f)) == abs(facep(3,f)))
    eflag = (abs(facep(2,f)) /= p .or. dof .and. ccwfl < 0)

  else

    f = fr
    dof = (abs(facep(2,f)) == abs(facep(3,f)))
    sf = ccwfl

    if (dof) then

      e = cedge(2,nce)
      la = fvl(loc,e)
      lb = fvl(loc,fvl(succ,e))

      if ((lb - la)*ccwfl > 0) then
        e = fvl(edgc,e)
      else
        e = fvl(edga,e)
      end if

      l = fvl(loc,e)
      if (l == la) sf = -ccwfl

    end if

    eflag = (abs(facep(2,f)) /= p .or. dof .and. sf < 0)

  end if

  dotp = -(nrmlc(1)*nrml(1,f) + nrmlc(2)*nrml(2,f) + nrmlc(3)* &
    nrml(3,f))
  if (abs(dotp) > 1.0d0 - tol) dotp = sign(1.0d0,dotp)
  if (eflag) dotp = -dotp
  angr = pi() - acos(dotp)
  dir1(1) = nrmlc(2)*dir(3) - nrmlc(3)*dir(2)
  dir1(2) = nrmlc(3)*dir(1) - nrmlc(1)*dir(3)
  dir1(3) = nrmlc(1)*dir(2) - nrmlc(2)*dir(1)
  dotp = dir1(1)*nrml(1,f) + dir1(2)*nrml(2,f) + dir1(3)* &
    nrml(3,f)

  if (eflag) dotp = -dotp
  if (dotp > 0.0d0) angr = pi2 - angr

  if (fr == 0) then

    ang = pi()

  else

    a = cedge(2,nce)
    la = fvl(loc,a)
    lb = fvl(loc,fvl(succ,a))

    if ((lb - la)*ccwfl > 0) then
      ang = eang(a)
    else
      ang = eang(fvl(edga,a))
    end if

  end if

  if (angr < angacc .or. ang - angr < angacc) then
    if (msglvl == 4) then
      write ( *,600) 'rejected due to small angle'
    end if
    return
  end if

  if (ccwfl > 0) then
    cdang(nce) = angr
  else
    cdang(nce) = -angr
  end if

  if (lw1 /= lv) go to 10
!
!  Determine if cut polygon is outer or inner boundary by summing
!  the exterior angles (which lie in range (-PI,PI)). A sum of 2*PI
!  (-2*PI) means that polygon is outer (inner). Cut polygon is
!  rejected in the latter case.
!
  s = 0.0d0
  la = cedge(1,nce-1)
  lb = cedge(1,0)
  dir1(1) = vcl(1,lb) - vcl(1,la)
  dir1(2) = vcl(2,lb) - vcl(2,la)
  dir1(3) = vcl(3,lb) - vcl(3,la)
  dir1sq = dir1(1)**2 + dir1(2)**2 + dir1(3)**2

  do i = 0, nce-1

    dir(1) = dir1(1)
    dir(2) = dir1(2)
    dir(3) = dir1(3)
    dirsq = dir1sq
    la = lb
    lb = cedge(1,i+1)
    dir1(1) = vcl(1,lb) - vcl(1,la)
    dir1(2) = vcl(2,lb) - vcl(2,la)
    dir1(3) = vcl(3,lb) - vcl(3,la)
    dir1sq = dir1(1)**2 + dir1(2)**2 + dir1(3)**2
    dotp = (dir(1)*dir1(1) + dir(2)*dir1(2) + dir(3)*dir1(3))/ &
      sqrt(dirsq*dir1sq)

    if ( abs(dotp) > 1.0d0 - tol ) then
      dotp = sign(1.0d0,dotp)
    end if

    ang = acos(dotp)

    if (kmax == 1) then
      cp(1) = dir(2)*dir1(3) - dir(3)*dir1(2)
    else if (kmax == 2) then
      cp(2) = dir(3)*dir1(1) - dir(1)*dir1(3)
    else
      cp(3) = dir(1)*dir1(2) - dir(2)*dir1(1)
    end if

    if (cp(kmax)*nrmlc(kmax) < 0.0d0) then
      ang = -ang
    end if

    s = s + ang

  end do

  if (s < 0.0d0) then
    if (msglvl == 4) then
      write ( *, '(a)' ) 'Rejected due to inner boundary'
      return
    end if
  end if
!
!  Move edges incident on LV (if <= NVC), EV(1:NEV) to end of PEDGE.
!
  if (lv <= nvc) then
    l = lv
    ee = 0
  else
    ee = 1
  end if

  do e = ee, nev

    if ( e > 0 ) then
      l = ev(e)
    end if

    k = 1

    do while ( k <= nedgc )

      a = pedge(1,k)
      la = fvl(loc,a)
      lb = fvl(loc,fvl(succ,a))

      if ( la == l .or. lb == l ) then
        do i = 1,3
          j = pedge(i,k)
          pedge(i,k) = pedge(i,nedgc)
          pedge(i,nedgc) = j
        end do
        nedgc = nedgc - 1
      else
        k = k + 1
      end if

    end do

  end do
!
!  Determine if cut face contains any inner polygons by checking
!  if the remaining edges of PEDGE intersect interior of cut face.
!
  do i = 1,nedgc

    a = pedge(1,i)
    ca = pedge(3,i)/10
    cb = mod(pedge(3,i),10)
    la = fvl(loc,a)
    lb = fvl(loc,fvl(succ,a))

    if (ca == 2) then
      cp(1) = vcl(1,la)
      cp(2) = vcl(2,la)
      cp(3) = vcl(3,la)
    else if (cb == 2) then
      cp(1) = vcl(1,lb)
      cp(2) = vcl(2,lb)
      cp(3) = vcl(3,lb)
    else
      dir(1) = vcl(1,lb) - vcl(1,la)
      dir(2) = vcl(2,lb) - vcl(2,la)
      dir(3) = vcl(3,lb) - vcl(3,la)
      t = (nrmlc(4) - nrmlc(1)*vcl(1,la) - nrmlc(2)*vcl(2,la) - &
           nrmlc(3)*vcl(3,la))/(nrmlc(1)*dir(1) + nrmlc(2)*dir(2) &
           + nrmlc(3)*dir(3))
      cp(1) = vcl(1,la) + t*dir(1)
      cp(2) = vcl(2,la) + t*dir(2)
      cp(3) = vcl(3,la) + t*dir(3)
    end if

    call ptpolg(3,3,nce,2,cedge,vcl,cp,nrmlc,dtol,inout)

    if (inout == 1) then
      if (msglvl == 4) then
        write ( *,600) 'rejected due to hole polygon'
      end if
      return
    end if

  end do

  rflag = .true.

  if (msglvl == 4) then
    write ( *,600) 'cedge(1:2), cdang'
    do i = 1,nce
      write ( *,610) i,cedge(1,i),cedge(2,i),cdang(i)*180.0d0 / pi()
    end do
  end if

  600 format (4x,a)
  610 format (4x,3i7,f12.5)

  return
end
subroutine cvdec2 ( angspc, angtol, nvc, npolg, nvert, maxvc, maxhv, &
  maxpv, maxiw, maxwk, vcl, regnum, hvl, pvl, iang, iwk, wk, ierror )
!
!*******************************************************************************
!
!! CVDEC2 decomposes a polygonal region into convex polygons.
!
!
!  Purpose:
!
!    Decompose general polygonal region (which is decomposed
!    into simple polygons on input) into convex polygons using
!    vertex coordinate list, head vertex list, and polygon vertex
!    list data structures.
!
!  Modified:
!
!    12 July 1999
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, double precision ANGSPC, the angle spacing parameter in radians
!    used in controlling vertices to be considered as an endpoint of 
!    a separator.
!
!    Input, double precision ANGTOL, the angle tolerance parameter in radians
!    used in accepting separator(s).
!
!    Input/output, integer NVC, the number of vertex coordinates or positions
!    used in VCL.
!
!    Input/output, integer NPOLG, the number of polygonal subregions or
!    positions used in HVL array.
!
!    Input/output, integer NVERT, the number of polygon vertices or positions
!    used in PVL array.
!
!    Input, integer MAXVC, the maximum size available for VCL array, should 
!    be >= number of vertex coordinates required for decomposition.
!
!    Input, integer MAXHV, the maximum size available for HVL, REGNUM arrays,
!    should be >= number of polygons required for decomposition.
!
!    Input, integer MAXPV, the maximum size available for PVL, IANG arrays;
!    should be >= number of polygon vertices required for decomposition.
!
!    Input, integer MAXIW, the maximum size available for IWK array; should be
!    about 3 times maximum number of vertices in any polygon.
!
!    Input, integer MAXWK, the maximum size available for WK array; should be
!    about 5 times maximum number of vertices in any polygon.
!
!    Input/output, double precision VCL(1:2,1:NVC), the vertex coordinate list.
!
!    Input/output, integer REGNUM(1:NPOLG), region numbers.
!
!    Input/output, integer HVL(1:NPOLG), the head vertex list.
!
!    Input/output, integer PVL(1:4,1:NVERT), double precision IANG(1:NVERT),
!    the polygon vertex list and interior angles; see routine DSPGDC for
!    more details.  Note that the data structures should be as output from
!    routine SPDEC2.
!
!    Workspace, integer IWK(1:MAXIW).
!
!    Workspace, double precision WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxhv
  integer maxiw
  integer maxpv
  integer maxvc
  integer maxwk
!
  double precision angspc
  double precision angtol
  integer hvl(maxhv)
  double precision iang(maxpv)
  integer ierror
  integer iwk(maxiw)
  integer npolg
  integer nvc
  integer nvert
  double precision pi
  double precision piptol
  integer pvl(4,maxpv)
  integer regnum(maxhv)
  double precision tol
  integer v
  double precision vcl(2,maxvc)
  integer w1
  integer w2
  double precision wk(maxwk)
!
  tol = 100.0D+00 * epsilon ( tol )
  ierror = 0
!
!  For each reflex vertex, resolve it with one or two separators
!  and update VCL, HVL, PVL, IANG.
!
  piptol = pi() + tol
  v = 1

  do

    if ( v > nvert ) then
      exit
    end if

    if ( iang(v) > piptol ) then

      call resvrt ( v, angspc, angtol, nvc, nvert, maxvc, maxpv, maxiw, &
        maxwk, vcl, pvl, iang, w1, w2, iwk, wk, ierror )

      if ( ierror /= 0 ) then
        return
      end if

      call insed2 ( v ,w1, npolg, nvert, maxhv, maxpv, vcl, regnum, hvl, &
        pvl, iang, ierror )

      if ( ierror /= 0 ) then
        return
      end if

      if ( w2 > 0 ) then
        call insed2 ( v, w2, npolg, nvert, maxhv, maxpv, vcl, regnum, hvl, &
          pvl, iang, ierror )
      end if

      if ( ierror /= 0 ) then
        return
      end if

    end if

    v = v + 1

  end do

  return
end
subroutine cvdec3 ( angacc, rdacc, nvc, nface, nvert, npolh, npf, maxvc, &
  maxfp, maxfv, maxhf, maxpf, maxiw, maxwk, vcl, facep, factyp, nrml, fvl, &
  eang, hfl, pfl, iwk, wk, ierr )
!
!******************************************************************************
!
!! CVDEC3 decomposes polyhedra into convex parts.
!
!
!  Purpose: 
!
!    Given one or more polyhedra in polyhedral decomposition
!    data structure, decompose the polyhedra into convex parts.  It
!    is assumed all faces are simple (any faces with holes should be
!    decomposed into simple polygons), each face appears at most
!    once in a polyhedron (double-occurring faces are not allowed),
!    and no interior holes occur in any polyhedra.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, ANGACC - minimum acceptable dihedral angle in radians produced by
!    cut faces.
!
!    Input, RDACC - minimum acceptable relative distance between cut planes
!    and vertices not on plane.
!
!    Input/output, NVC - number of vertex coordinates or positions used in VCL.
!
!    Input/output, NFACE - number of faces or positions used in FACEP array.
!
!    Input/output, NVERT - number of positions used in FVL, EANG arrays.
!
!    Input/output, NPOLH - number of polyhedra or positions used in HFL array.
!
!    Input/output, NPF - number of positions used in PFL array.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXFP - maximum size available for FACEP, FACTYP, NRML arrays.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, MAXHF - maximum size available for HFL array.
!
!    Input, MAXPF - maximum size available for PFL array.
!
!    Input, MAXIW - maximum size available for IWK array; should be about
!    5 times number of edges in any polyhedron.
!
!    Input, MAXWK - maximum size available for WK array; should be about
!    number of edges in any polyhedron.
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input/output, FACEP(1:3,1:NFACE) - face pointer list: row 1 is 
!    head pointer, rows 2 and 3 are signed polyhedron indices.
!
!    Input/output, FACTYP(1:NFACE) - face types: useful for specifying types of
!    boundary faces; entries must be >= 0; any new interior
!    faces (not part of previous face) has face type set to 0.
!
!    Input/output, NRML(1:3,1:NFACE) - unit normal vectors for faces; outward
!    normal corresponds to counter clockwise traversal of 
!    face from polyhedron with index |FACEP(2,F)|.
!
!    Input/output, FVL(1:6,1:NVERT) - face vertex list; see routine DSPHDC.
!
!    Input/output, EANG(1:NVERT) - angles at edges common to 2 faces in 
!    a polyhedron; EANG(J) corresponds to FVL(*,J), determined by EDGC field.
!
!    Input/output, HFL(1:NPOLH) - head pointer to face indices in PFL for each
!    polyhedron.
!
!    Input, PFL(1:2,1:NPF) - list of signed face indices for each polyhedron;
!    row 2 used for link.
!
!    Workspace, integer IWK(1:MAXIW).
!
!    Workspace, double precision WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfp
  integer maxfv
  integer maxhf
  integer maxiw
  integer maxpf
  integer maxvc
  integer maxwk
!
  double precision angacc
  double precision eang(maxfv)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer facep(3,maxfp)
  integer, parameter :: facn = 2
  integer factyp(maxfp)
  integer fvl(6,maxfv)
  integer hfl(maxhf)
  integer ierr
  integer iwk(maxiw)
  integer l
  integer, parameter :: loc = 1
  integer n
  integer nface
  integer npf
  integer npolh
  double precision nrml(3,maxfp)
  integer nvc
  integer nvert
  double precision pi
  double precision piptol
  integer pfl(2,maxpf)
  integer, parameter :: pred = 4
  double precision rdacc
  logical rflag
  integer, parameter :: succ = 3
  double precision tol
  integer u
  double precision vcl(3,maxvc)
  double precision wk(maxwk)
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  piptol = pi() + tol

10 continue

  l = 0
  n = 0
  u = 1

  do

    if (eang(u) > piptol) then

      call resedg(u,angacc,rdacc,nvc,nface,nvert,npolh,npf,maxvc, &
        maxfp,maxfv,maxhf,maxpf,maxiw/3,maxwk,vcl,facep,factyp, &
        nrml,fvl,eang,hfl,pfl,rflag,iwk,wk, ierr )

      if (ierr /= 0) return

      if (rflag) then
        n = n + 1
      else
        if (l == 0) l = u
      end if

    end if

    u = u + 1

    if ( u > nvert ) then
      exit
    end if

  end do

  if (l > 0) then
    if (n == 0) then
      ierr = 327
      return
    else
      go to 10
    end if
  else
    if (n > 0) go to 10
  end if

  return
end
subroutine cvdecf ( aspc2d, atol2d, nvc, nface, nvert, npf, maxvc, maxfp, &
  maxfv, maxpf, maxiw, maxwk, vcl, facep, factyp, nrml, fvl, eang, hfl, pfl, &
  iwk, wk, ierr )
!
!******************************************************************************
!
!! CVDECF updates a polyhedral decomposition.
!
!
!  Purpose: 
!
!    Update polyhedral decomposition data structure for polyhedral
!    region by decomposing each face of decomposition into convex
!    subpolygons using separators from reflex vertices.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, ASPC2D - angle spacing parameter in radians used in controlling
!    vertices to be considered as an endpoint of a separator.
!
!    Input, ATOL2D - angle tolerance parameter in radians used in accepting
!    separator to resolve a reflex vertex on a face.
!
!    Input/output, NVC - number of vertex coordinates or positions used in VCL.
!
!    Input/output, NFACE - number of faces or positions used in FACEP array.
!
!    Input/output, NVERT - number of positions used in FVL, EANG arrays.
!
!    Input/output, NPF - number of positions used in PFL array.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXFP - maximum size available for FACEP, FACTYP, NRML arrays.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, MAXPF - maximum size available for PFL array.
!
!    Input, MAXIW - maximum size available for IWK array; should be about
!    6*NV + 24*NRV where NV is max number of vertices and NRV
!    is maximum number of reflex vertices in a nonconvex face.
!
!    Input, MAXWK - maximum size available for WK array; should be about
!    8*NV + 24*NRV.
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input/output, FACEP(1:3,1:NFACE) - face pointer list: row 1 is head
!    pointer, rows 2 and 3 are signed polyhedron indices.
!
!    Input/output, FACTYP(1:NFACE) - face types: useful for specifying types of
!    boundary faces; entries must be >= 0; any new interior
!    faces (not part of previous face) has face type set to 0.
!
!    Input/output, NRML(1:3,1:NFACE) - unit normal vectors for faces; outward.
!    normal corresponds to counter clockwise traversal of face from polyhedron
!    with index |FACEP(2,F)|.
!
!    Input/output, FVL(1:6,1:NVERT) - face vertex list; see routine DSPHDC.
!
!    Input/output, EANG(1:NVERT) - angles at edges common to 2 faces in a
!    polyhedron; EANG(J) corresponds to FVL(*,J), determined by EDGC field.
!
!    Input, HFL(1:*) - head pointer to face indices in PFL for each polyhedron.
!
!    Input/output, PFL(1:2,1:NPF) - list of signed face indices for each
!    polyhedron; row 2 used for link.
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfp
  integer maxfv
  integer maxiw
  integer maxpf
  integer maxvc
  integer maxwk
!
  integer a
  double precision angle
  double precision aspc2d
  double precision atol2d
  integer ccw
  double precision cp
  double precision cxy
  double precision cyz
  double precision eang(maxfv)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer edgv
  integer facep(3,maxfp)
  integer, parameter :: facn = 2
  integer factyp(maxfp)
  integer fvl(6,maxfv)
  integer hfl(*)
  integer i
  integer iang
  integer ierr
  integer irem
  integer iwk(maxiw)
  integer j
  integer js
  integer k
  integer l
  double precision leng
  integer link
  integer, parameter :: loc = 1
  integer locfv
  integer lp
  integer ls
  integer nf
  integer nface
  integer npf
  double precision nrml(3,maxfp)
  integer nrv
  double precision ntol
  integer nv
  integer nvc
  integer nvert
  integer pfl(2,maxpf)
  double precision pi
  double precision piptol
  integer, parameter :: pred = 4
  double precision pt(2,2)
  double precision r21
  double precision r22
  double precision r31
  double precision r32
  integer s
  integer size
  integer, parameter :: succ = 3
  double precision sxy
  double precision syz
  integer t
  double precision tol
  double precision u(3)
  double precision v(3)
  double precision vcl(3,maxvc)
  integer wrem
  integer w(2)
  double precision wk(maxwk)
  integer x
  integer y
  double precision zr
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  piptol = pi() + tol
  nf = nface

  do i = 1,nf
!
!  Determine number of reflex vertices of face I.
!
    nrv = 0
    nv = 0
    k = 1
    if (abs(nrml(2,i)) > abs(nrml(1,i))) k = 2
    if (abs(nrml(3,i)) > abs(nrml(k,i))) k = 3

    if (facep(2,i) > 0) then
      ccw = succ
    else
      ccw = pred
    end if

    j = facep(1,i)
    l = fvl(loc,j)
    lp = fvl(loc,fvl(7-ccw,j))
    u(1) = vcl(1,l) - vcl(1,lp)
    u(2) = vcl(2,l) - vcl(2,lp)
    u(3) = vcl(3,l) - vcl(3,lp)

10  continue

    nv = nv + 1
    js = fvl(ccw,j)
    ls = fvl(loc,js)
    v(1) = vcl(1,ls) - vcl(1,l)
    v(2) = vcl(2,ls) - vcl(2,l)
    v(3) = vcl(3,ls) - vcl(3,l)
    ntol = tol*max(abs(u(1)), abs(u(2)), abs(u(3)), abs(v(1)), &
      abs(v(2)), abs(v(3)))

    if (k == 1) then
      cp = u(2)*v(3) - u(3)*v(2)
    else if (k == 2) then
      cp = u(3)*v(1) - u(1)*v(3)
    else
      cp = u(1)*v(2) - u(2)*v(1)
    end if

    if (abs(cp) > ntol .and. cp*nrml(k,i) < 0.0d0) then
      nrv = nrv + 1
    end if

    j = js

    if (j /= facep(1,i)) then
      l = ls
      u(1) = v(1)
      u(2) = v(2)
      u(3) = v(3)
      go to 10
    end if

    if (nrv == 0) go to 70
!
!  Set up 2D data structure. Rotate normal vector of face to
!  (0,0,1). Rotation matrix applied to face vertices is
!    [ CXY     -SXY     0   ]
!    [ CYZ*SXY CYZ*CXY -SYZ ]
!    [ SYZ*SXY SYZ*CXY  CYZ ]
!
    size = nv + 8*nrv
    locfv = 1
    link = locfv + size
    edgv = link + size
    irem = edgv + size
    x = 1
    y = x + size
    iang = y + size
    wrem = iang + size

    if (irem > maxiw) then
      ierr = 6
      return
    else if (wrem > maxwk) then
      ierr = 7
      return
    end if

    if (abs(nrml(1,i)) <= tol) then
      leng = nrml(2,i)
      cxy = 1.0d0
      sxy = 0.0d0
    else
      leng = sqrt(nrml(1,i)**2 + nrml(2,i)**2)
      cxy = nrml(2,i)/leng
      sxy = nrml(1,i)/leng
    end if

    cyz = nrml(3,i)
    syz = leng
    r21 = cyz*sxy
    r22 = cyz*cxy
    r31 = nrml(1,i)
    r32 = nrml(2,i)
    zr = r31*vcl(1,ls) + r32*vcl(2,ls) + cyz*vcl(3,ls)
    j = facep(1,i)

    do k = 0,nv-1
      l = fvl(loc,j)
      wk(x+k) = cxy*vcl(1,l) - sxy*vcl(2,l)
      wk(y+k) = r21*vcl(1,l) + r22*vcl(2,l) - syz*vcl(3,l)
      iwk(locfv+k) = j
      iwk(link+k) = k + 2
      iwk(edgv+k) = 0
      j = fvl(ccw,j)
    end do

    iwk(link+nv-1) = 1

    do k = 1,nv-2
      wk(iang+k) = angle(wk(x+k-1),wk(y+k-1), wk(x+k),wk(y+k), &
        wk(x+k+1),wk(y+k+1))
    end do

    wk(iang) = angle(wk(x+nv-1),wk(y+nv-1), wk(x),wk(y), &
      wk(x+1),wk(y+1))

    wk(iang+nv-1) = angle(wk(x+nv-2),wk(y+nv-2), wk(x+nv-1), &
      wk(y+nv-1), wk(x),wk(y))
!
!  Resolve reflex vertices.
!
    j = 1

40  continue

    if (j > nv) go to 70

    if (wk(iang+j-1) > piptol) then

      call resvrf(j,aspc2d,atol2d,maxiw-irem+1,maxwk-wrem+1, &
        wk(x),wk(y),wk(iang),iwk(link),w(1),w(2),pt,pt(1,2), &
        iwk(irem),wk(wrem), ierr )

      if (ierr /= 0) return

      if (w(2) == 0) then
        l = 1
      else
        l = 2
      end if

      do k = l,1,-1

        s = -w(k)

        if (s > 0) then

          wk(x+nv) = pt(1,k)
          wk(y+nv) = pt(2,k)
          wk(iang+nv) = pi()
          iwk(link+nv) = iwk(link+s-1)
          nv = nv + 1
          iwk(link+s-1) = nv

          if (nvc >= maxvc) then
            ierr = 14
            return
          end if

          vcl(1,nvc+1) = cxy*pt(1,k) + r21*pt(2,k) + r31*zr
          vcl(2,nvc+1) = r22*pt(2,k) - sxy*pt(1,k) + r32*zr
          vcl(3,nvc+1) = cyz*zr - syz*pt(2,k)
          a = iwk(locfv+s-1)
          if (ccw == pred) a = fvl(pred,a)
          call insvr3(a,nvc,nvert,maxfv,vcl,fvl,eang,ierr)
          if (ierr /= 0) return
          iwk(locfv+nv-1) = fvl(succ,a)
          t = iwk(edgv+s-1)

          if (t == 0) then
            iwk(edgv+nv-1) = 0
            w(k) = nv
          else
            wk(x+nv) = wk(x+nv-1)
            wk(y+nv) = wk(y+nv-1)
            wk(iang+nv) = pi()
            iwk(link+nv) = iwk(link+t-1)
            nv = nv + 1
            iwk(link+t-1) = nv

            if (iwk(locfv+nv-2) == nvert) then
              iwk(locfv+nv-1) = nvert - 1
            else
              iwk(locfv+nv-1) = nvert
            end if

            iwk(edgv+s-1) = nv
            iwk(edgv+nv-2) = t
            iwk(edgv+t-1) = nv - 1
            iwk(edgv+nv-1) = s
            w(k) = nv - 1
          end if

        end if

      end do

      do k = 1,l

        s = w(k)
        wk(x+nv) = wk(x+j-1)
        wk(y+nv) = wk(y+j-1)
        iwk(link+nv) = iwk(link+j-1)
        nv = nv + 1
        wk(x+nv) = wk(x+s-1)
        wk(y+nv) = wk(y+s-1)
        iwk(link+nv) = iwk(link+s-1)
        nv = nv + 1
        iwk(link+j-1) = nv
        iwk(link+s-1) = nv - 1
        a = iwk(edgv+j-1)
        iwk(edgv+nv-2) = a
        iwk(edgv+j-1) = s
        if (a > 0) iwk(edgv+a-1) = nv - 1
        a = iwk(edgv+s-1)
        iwk(edgv+nv-1) = a
        iwk(edgv+s-1) = j
        if (a > 0) iwk(edgv+a-1) = nv
        a = iwk(locfv+j-1)
        call insed3(a,iwk(locfv+s-1),nface,nvert,npf,maxfp, &
          maxfv,maxpf,facep,factyp,nrml,fvl,eang,hfl,pfl,ierr)
        if (ierr /= 0) return

        if (ccw == succ) then
          iwk(locfv+nv-2) = iwk(locfv+j-1)
          iwk(locfv+nv-1) = iwk(locfv+s-1)
          iwk(locfv+j-1) = nvert - 1
          iwk(locfv+s-1) = nvert
        else
          iwk(locfv+nv-2) = nvert - 1
          iwk(locfv+nv-1) = nvert
        end if

        t = iwk(link+nv-2)
        wk(iang+nv-2) = angle(wk(x+s-1),wk(y+s-1), &
                 wk(x+nv-2),wk(y+nv-2), wk(x+t-1),wk(y+t-1))
        wk(iang+j-1) = wk(iang+j-1) - wk(iang+nv-2)
        t = iwk(link+nv-1)
        wk(iang+nv-1) = angle(wk(x+j-1),wk(y+j-1), &
                 wk(x+nv-1),wk(y+nv-1), wk(x+t-1),wk(y+t-1))
        wk(iang+s-1) = wk(iang+s-1) - wk(iang+nv-1)

      end do

    end if

    j = j + 1
    go to 40

70  continue

  end do

  return
end
subroutine cvdtri ( inter, ldv, nt, vcl, til, tedg, sptr, ierror )
!
!*******************************************************************************
!
!! CVDTRI converts boundary triangles to Delaunay triangles.
!
!
!  Purpose:
!
!    Convert triangles in strip near boundary of polygon
!    or inside polygon to Delaunay triangles.
!
!  Modified:
!
!    12 July 1999
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, logical INTER, is .TRUE. if and only if there is at least
!    one interior mesh vertex.
!
!    Input, integer LDV, the leading dimension of VCL in calling routine.
!
!    Input, integer NT, the number of triangles in strip or polygon.
!
!    Input, VCL(1:2,1:*), the vertex coordinate list.
!
!    Input/output, integer TIL(1:3,1:NT), the triangle incidence list.
!
!    Input/output, integer TEDG(1:3,1:NT) - TEDG(J,I) refers to edge with
!    vertices TIL(J:J+1,I) and contains index of merge edge or
!    > NT for edge of chains.
!
!    Workspace, SPTR(1:NT) - SPTR(I) = -1 if merge edge I is not in LOP stack,
!    else >= 0 and pointer (index of SPTR) to next edge in
!    stack (0 indicates bottom of stack).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer ldv
  integer nt
!
  integer e
  integer ierror
  integer ind(2)
  logical inter
  integer itr(2)
  integer k
  integer mxtr
  logical sflag
  integer sptr(nt)
  integer tedg(3,nt)
  integer til(3,nt)
  integer top
  double precision vcl(ldv,*)
!
  ierror = 0
  sflag = .true.
  sptr(1:nt) = -1

  do k = 1, nt

    mxtr = k + 1

    if ( k == nt ) then
      if ( .not. inter ) then
        return
      end if
      mxtr = nt
      sflag = .false.
    end if

    top = k
    sptr(k) = 0

    do

      e = top
      top = sptr(e)

      call fndtri ( e, mxtr, sflag, tedg, itr, ind, ierror )

      if ( ierror /= 0 ) then
        return
      end if

      call lop ( itr, ind, k, top, ldv, vcl, til, tedg, sptr )

      if ( top <= 0 ) then
        exit
      end if

    end do

  end do

  return
end
subroutine dhpsrt ( k, n, lda, a, map )
!
!******************************************************************************
!
!! DHPSRT sorts a list of double precision points in KD.
!
!
!  Purpose: 
!
!    Use heapsort to obtain the permutation of N K-dimensional
!    double precision points so that the points are in lexicographic
!    increasing order.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of points.
!
!    Input, N - number of points.
!
!    Input, LDA - leading dimension of array A in calling routine; should
!    be >= K.
!
!    Input, A(1:K,1:*) - array of >= N K-D double precision points.
!
!    Input/output, MAP(1:N).  On input, he points of A with indices 
!    MAP(1), MAP(2), ..., MAP(N) are to be sorted.  On output, the elements 
!    are permuted so that A(*,MAP(1)) <= A(*,MAP(2)) <= ... <= A(*,MAP(N)).
!
  implicit none
!
  integer lda
  integer n
!
  double precision a(lda,*)
  integer i
  integer k
  integer map(n)
  integer t
!
  do i = n/2, 1, -1
    call dsftdw(i,n,k,lda,a,map)
  end do

  do i = n, 2, -1
    t = map(1)
    map(1) = map(i)
    map(i) = t
    call dsftdw(1,i-1,k,lda,a,map)
  end do

  return
end
function diaedg ( x0, y0, x1, y1, x2, y2, x3, y3 )
!
!******************************************************************************
!
!! DIAEDG determines which diagonal to use in a quadrilateral.
!
!
!  Purpose: 
!
!    Determine whether 02 or 13 is the diagonal edge chosen
!    based on the circumcircle criterion, where (X0,Y0), (X1,Y1),
!    (X2,Y2), (X3,Y3) are the vertices of a simple quadrilateral
!    in counterclockwise order.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, X0, Y0, X1, Y1, X2, Y2, X3, Y3 - vertex coordinates.
!
!    Output, DIAEDG -  
!     1, if diagonal edge 02 is chosen, i.e. 02 is inside.
!      quadrilateral + vertex 3 is outside circumcircle 012
!    -1, if diagonal edge 13 is chosen, i.e. 13 is inside.
!      quadrilateral + vertex 0 is outside circumcircle 123
!     0, if four vertices are cocircular.
!
  implicit none
!
  double precision ca
  double precision cb
  integer diaedg
  double precision dx10
  double precision dx12
  double precision dx30
  double precision dx32
  double precision dy10
  double precision dy12
  double precision dy30
  double precision dy32
  double precision s
  double precision tol
  double precision tola
  double precision tolb
  double precision x0
  double precision x1
  double precision x2
  double precision x3
  double precision y0
  double precision y1
  double precision y2
  double precision y3
!
  tol = 100.0D+00 * epsilon ( tol )
  dx10 = x1 - x0
  dy10 = y1 - y0
  dx12 = x1 - x2
  dy12 = y1 - y2
  dx30 = x3 - x0
  dy30 = y3 - y0
  dx32 = x3 - x2
  dy32 = y3 - y2
  tola = tol*max(abs(dx10),abs(dy10),abs(dx30),abs(dy30))
  tolb = tol*max(abs(dx12),abs(dy12),abs(dx32),abs(dy32))
  ca = dx10*dx30 + dy10*dy30
  cb = dx12*dx32 + dy12*dy32

  if (ca > tola .and. cb > tolb) then
    diaedg = -1
  else if (ca < -tola .and. cb < -tolb) then
    diaedg = 1
  else
    tola = max(tola,tolb)
    s = (dx10*dy30 - dx30*dy10)*cb + (dx32*dy12 - dx12*dy32)*ca

    if (s > tola) then
      diaedg = -1
    else if (s < -tola) then
      diaedg = 1
    else
      diaedg = 0
    end if

  end if

  return
end
subroutine diam2 ( nvrt, xc, yc, i1, i2, diamsq, ierr )
!
!******************************************************************************
!
!! DIAM2 finds the diameter of a convex polygon.
!
!
!  Purpose: 
!
!    Find the diameter of a convex polygon with vertices
!    given in counter clockwise order and with all interior angles < PI.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NVRT - number of vertices on the boundary of convex polygon.
!
!    Input, XC(1:NVRT), YC(1:NVRT) - vertex coordinates in counter 
!    clockwise order.
!
!    Output, I1,I2 - indices in XC,YC of diameter edge; diameter is from
!    (XC(I1),YC(I1)) to (XC(I2),YC(I2)).
!
!    Output, DIAMSQ - square of diameter.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer nvrt
!
  double precision area1
  double precision area2
  double precision areatr
  double precision c1mtol
  double precision c1ptol
  double precision diamsq
  double precision dist
  logical first
  integer i1
  integer i2
  integer ierr
  integer j
  integer jp1
  integer k
  integer kp1
  integer m
  double precision tol
  double precision xc(nvrt)
  double precision yc(nvrt)
!
!  Find first vertex which is farthest from edge connecting
!  vertices with indices NVRT, 1.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  first = .true.
  c1mtol = 1.0d0 - tol
  c1ptol = 1.0d0 + tol
  j = nvrt
  jp1 = 1
  k = 2
  area1 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(k),yc(k))

  do

    area2 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(k+1),yc(k+1))

    if ( area2 <= area1*c1ptol) then
      exit
    end if

    area1 = area2
    k = k + 1

  end do

  m = k
  diamsq = 0.0d0
!
!  Find diameter = maximum distance of antipodal pairs.
!
20 continue

  kp1 = k + 1

  if (kp1 > nvrt) kp1 = 1

  area2 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(kp1),yc(kp1))

  if (area2 > area1*c1ptol) then
    k = k + 1
    area1 = area2
  else if (area2 < area1*c1mtol) then
    j = jp1
    jp1 = j + 1
    area1 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(k),yc(k))
  else
    k = k + 1
    j = jp1
    jp1 = j + 1
    area1 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(k),yc(k))
  end if

  if (j > m .or. k > nvrt) then

    if (first .and. m > 2) then
!
!  Possibly restart with M decreased by 1.
!
      first = .false.
      m = m - 1
      j = nvrt
      jp1 = 1
      k = m
      area1 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(k),yc(k))
      area2 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(k+1), yc(k+1))

      if (area2 <= area1*(1.0d0+25.0d0*tol)) then
        area1 = area2
        diamsq = 0.0d0
        go to 20
      end if

    end if

    ierr = 200
    return

  end if

  dist = (xc(j) - xc(k))**2 + (yc(j) - yc(k))**2

  if (dist > diamsq) then
    diamsq = dist
    i1 = j
    i2 = k
  end if

  if (j /= m .or. k /= nvrt) go to 20

  return
end
subroutine diam3 ( nvrt, vcl, i1, i2, diamsq )
!
!******************************************************************************
!
!! DIAM3 finds the diameter of a set of 3D points.
!
!
!  Purpose: 
!
!    Compute diameter (largest distance) of set of 3D points,
!    and return two vertex indices realizing diameter.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NVRT - number of vertices.
!
!    Input, VCL(1:3,1:NVRT) - vertex coordinate list.
!
!    Output, I1, I2 - vertex indices realizing diameter (I1 < I2).
!
!    Output, DIAMSQ - square of diameter.
!
  implicit none
!
  integer nvrt
!
  double precision diamsq
  double precision distsq
  integer i
  integer i1
  integer i2
  integer j
  double precision vcl(3,nvrt)
!
  diamsq = -1.0d0

  do i = 1,nvrt-1

    do j = i+1,nvrt

      distsq = (vcl(1,i) - vcl(1,j))**2 + (vcl(2,i) - vcl(2,j))**2 &
        + (vcl(3,i) - vcl(3,j))**2

      if (distsq > diamsq) then
        diamsq = distsq
        i1 = i
        i2 = j
      end if

    end do

  end do

  return
end
function dless ( k, p, q )
!
!******************************************************************************
!
!! DLESS determines the lexicographically lesser of two double precision values.
!
!
!  Purpose: 
!
!    Determine whether P is lexicographically less than Q in
!    floating point arithmetic.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of points.
!
!    Input, P(1:K),Q(1:K) - two K-dimensional double precision points.
!
!    Output, DLESS - .TRUE. if P < Q, .FALSE. otherwise.
!
  implicit none
!
  double precision cmax
  logical dless
  integer i
  integer k
  double precision p(k)
  double precision q(k)
  double precision tol
!
  tol = 100.0D+00 * epsilon ( tol )

  do i = 1, k

    cmax = max(abs(p(i)),abs(q(i)))

    if ( abs(p(i) - q(i)) <= tol*cmax .or. cmax <= tol) then
      cycle
    end if

     if (p(i) < q(i)) then
       dless = .true.
     else
       dless = .false.
     end if

     return

  end do

  dless = .false.

  return
end
subroutine dsconv ( p, headp, facep, nrml, fvl, eang, pfl, ncface, ncvert, &
  chvl, cnrml, cfvl, ceang )
!
!******************************************************************************
!
!! DSCONV converts the representation of a convex polyhedron.
!
!
!  Purpose: 
!
!    Convert the representation of a convex polyhedron in
!    the polyhedral decomposition data structure to the data
!    structure for a single convex polyhedron.
!
!  Discussion:
!
!    It is assumed upper bounds for NCVERT and NCFACE are
!    computed before calling this routine, and there is enough
!    space in CHVL, CNRML, CFVL, CEANG.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, P - polyhedron index.
!
!    Input, HEADP - head pointer to face indices in PFL for polyhedron P.
!
!    Input, FACEP(1:3,1:*) - face pointer list: row 1 is head pointer,
!    rows 2 and 3 are signed polyhedron indices.
!
!    Input, NRML(1:3,1:*) - unit normal vectors for faces; outward
!    normal corresponds to counter clockwise traversal 
!    of face from polyhedron with index |FACEP(2,F)|.
!
!    Input, FVL(1:6,1:*) - face vertex list; see routine DSPHDC,
!
!    Input, EANG(1:*) - angles at edges common to 2 faces in a polyhedron;
!    EANG(J) corresponds to FVL(*,J), determined by EDGC field
!    corresponds to FVL(*,J) and is determined by EDGC field.
!
!    Input, PFL(1:2,1:*) - list of signed face indices for each polyhedron;
!    row 2 used for link.
!
!    Output, NCFACE - number of faces in convex polyhedron.
!
!    Output, NCVERT - size of CFVL, CEANG arrays; 2 * number of edges 
!    of polyhedron.
!
!    Output, CHVL(1:NCFACE) - head vertex list.
!
!    Output, CNRML(1:3,1:NCFACE) - unit outward normals of faces.
!
!    Output, CFVL(1:5,1:NCVERT) - face vertex list; see routine DSCPH.
!
!    Output, CEANG(1:NCVERT) - angles at edges common to 2 faces; CEANG(I)
!    corresponds to CFVL(*,I).
!
  implicit none
!
  integer ccw
  double precision ceang(*)
  integer cfvl(5,*)
  integer chvl(*)
  double precision cnrml(3,*)
  double precision eang(*)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer, parameter :: edgv = 5
  integer f
  integer facep(3,*)
  integer, parameter :: facn = 2
  integer fvl(6,*)
  integer headp
  integer i
  integer j
  integer k
  integer kf
  integer kv
  integer lj
  integer lk
  integer, parameter :: loc = 1
  integer ncface
  integer ncvert
  double precision nrml(3,*)
  integer p
  integer pfl(2,*)
  integer, parameter :: pred = 4
  integer r
  integer s
  integer sf
  integer, parameter :: succ = 3
!
  kf = 0
  kv = 0
  i = headp

10 continue

  sf = pfl(1,i)

  if (sf > 0) then
    ccw = succ
  else
    ccw = pred
  end if

  f = abs(sf)
  kf = kf + 1
  chvl(kf) = kv + 1
  j = facep(1,f)

20 continue

  kv = kv + 1
  lj = fvl(loc,j)
  k = fvl(ccw,j)
  lk = fvl(loc,k)
  cfvl(loc,kv) = lj
  cfvl(facn,kv) = kf
  cfvl(succ,kv) = kv + 1
  cfvl(pred,kv) = kv - 1
  cfvl(edgv,kv) = 0

  if (ccw == succ) then
    fvl(facn,j) = kv
    r = j
  else
    fvl(facn,k) = kv
    s = lj
    lj = lk
    lk = s
    r = k
  end if

  if ((lk - lj)*sf > 0) then
    ceang(kv) = eang(r)
  else
    ceang(kv) = eang(fvl(edga,r))
  end if

  j = fvl(ccw,j)
  if (j /= facep(1,f)) go to 20

  cfvl(succ,kv) = chvl(kf)
  cfvl(pred,chvl(kf)) = kv

  if (abs(facep(2,f)) == p) then
    cnrml(1,kf) = nrml(1,f)
    cnrml(2,kf) = nrml(2,f)
    cnrml(3,kf) = nrml(3,f)
  else
    cnrml(1,kf) = -nrml(1,f)
    cnrml(2,kf) = -nrml(2,f)
    cnrml(3,kf) = -nrml(3,f)
  end if

  i = pfl(2,i)
  if (i /= headp) go to 10

  ncface = kf
  ncvert = kv
!
!  Set CFVL(EDGV,*) field and reset FVL(FACN,*) field.
!
  i = headp

30 continue

  sf = pfl(1,i)
  f = abs(sf)
  j = facep(1,f)

40 continue

  r = fvl(facn,j)
  fvl(facn,j) = f

  if (cfvl(edgv,r) == 0) then

    lj = fvl(loc,j)
    lk = fvl(loc,fvl(succ,j))

    if ((lk - lj)*sf > 0) then
      s = fvl(facn,fvl(edgc,j))
    else
      s = fvl(facn,fvl(edga,j))
    end if

    cfvl(edgv,r) = s
    cfvl(edgv,s) = r
  end if

  j = fvl(succ,j)
  if (j /= facep(1,f)) go to 40

  i = pfl(2,i)
  if (i /= headp) go to 30

  return
end
subroutine dscph ( nvc, nface, vcl, hvl, nrml, fvl, eang, htsiz, maxedg, &
  edge, ht, ierr )
!
!******************************************************************************
!
!! DSCPH initalizes the convex polyhedron data structure.
!
!
!  Purpose:
!
!    Initialize data structure for convex polyhedron. It is
!    assumed head vertex of each face is a strictly convex vertex.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NVC - number of vertex coordinates.
!
!    Input, NFACE - number of faces in convex polyhedron.
!
!    Input, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input, HVL(1:NFACE+1) - head pointer to vertex indices in FVL for each
!    face; 1 = HVL(1) < HVL(2) < ... < HVL(NFACE+1).
!
!    Input, FVL(1,1:*) - vertex indices in counter clockwise order when 
!    viewed from outside polyhedron; those for Ith face are in FVL(1,J)
!    for J = HVL(I),...,HVL(I+1)-1.
!
!    Input, HTSIZ - size of hash table HT; should be a prime number which
!    is >= NVC+2.
!
!    Input, MAXEDG - maximum size available for EDGE array; should be at
!    least number of edges in polyhedron.
!
!    Output, NRML(1:3,1:NFACE) - unit outward normals of polyhedron faces.
!
!    Output, FVL(1:5,1:NVERT), EANG(1:NVERT) - face vertex list, edge angles
!    where NVERT = HVL(NFACE+1)-1, first row of FVL same as
!    input, and HVL(NFACE+1) not needed on output; contains
!    the 6 'arrays' LOC, FACN, SUCC, PRED, EDGV, EANG (first 5
!    are integer arrays, last is a double precision array);
!    the vertices of each face are stored in counter clockwise order (when
!    viewed from outside polyhedron) in doubly circular linked
!    list.  FVL(LOC,V) is location in VCL of the coordinates
!    of 'vertex' (index) V. EANG(V) is edge angle at edge
!    starting at vertex V. FVL(FACN,V) is face number (index
!    of HVL) of face containing V.  FVL(SUCC,V) [FVL(PRED,V)]
!    is index in FVL of the successor (predecessor) vertex of
!    vertex V.  FVL(EDGV,V) gives information about the edge
!    joining vertices V and its successor - it is equal to the
!    index in FVL of the successor vertex as represented in
!    the other face of the polyhedron sharing this edge, i.e.
!    FVL(EDGV,V) != FVL(SUCC,V), FVL(LOC,FVL(EDGV,V)) =
!    FVL(LOC,FVL(SUCC,V)), FVL(EDGV,FVL(EDGV,V)) = V.
!
!    Workspace, HT(0:HTSIZ-1), EDGE(1:4,1:MAXEDG) - hash table and edge records
!    used to determine matching occurrences of polyhedron
!    edges by calling routine EDGHT.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer htsiz
  integer maxedg
  integer nface
  integer nvc
!
  integer a
  double precision ab(3)
  double precision ac(3)
  double precision ang
  integer b
  integer c
  double precision dotp
  double precision eang(*)
  integer edge(4,maxedg)
  integer, parameter :: edgv = 5
  integer f
  integer, parameter :: facn = 2
  integer fvl(5,*)
  integer hdfree
  integer ht(0:htsiz-1)
  integer hvl(nface+1)
  integer i
  integer ierr
  integer j
  integer k
  integer l
  integer last
  double precision leng
  integer, parameter :: loc = 1
  integer nht
  double precision nrml(3,nface)
  double precision pi
  integer, parameter :: pred = 4
  integer, parameter :: succ = 3
  double precision tol
  double precision vcl(3,nvc)
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  hdfree = 0
  last = 0
  nht = 0

  ht(0:htsiz-1) = 0

  do i = 1,nface

    k = hvl(i)
    l = hvl(i+1) - 1

    do j = k,l
      fvl(facn,j) = i
      fvl(succ,j) = j + 1
      fvl(pred,j) = j - 1
    end do

    fvl(succ,l) = k
    fvl(pred,k) = l

    do j = k,l

      call edght ( fvl(loc,j), fvl(loc,fvl(succ,j)), j, nvc, htsiz, &
        maxedg, hdfree, last, ht, edge, a, ierr )

      if (ierr /= 0) return

      if (a > 0) then
         fvl(edgv,j) = a
         fvl(edgv,a) = j
         nht = nht - 1
      else
         nht = nht + 1
      end if

    end do

  end do

  if (nht /= 0) then
    ierr = 310
    return
  end if
!
!  Compute unit outward normals of faces.
!
  do f = 1,nface

    i = hvl(f)
    a = fvl(loc,fvl(pred,i))
    b = fvl(loc,i)
    c = fvl(loc,fvl(succ,i))
    ab(1:3) = vcl(1:3,b) - vcl(1:3,a)
    ac(1:3) = vcl(1:3,c) - vcl(1:3,a)
    nrml(1,f) = ab(2)*ac(3) - ab(3)*ac(2)
    nrml(2,f) = ab(3)*ac(1) - ab(1)*ac(3)
    nrml(3,f) = ab(1)*ac(2) - ab(2)*ac(1)
    leng = sqrt(nrml(1,f)**2 + nrml(2,f)**2 + nrml(3,f)**2)

    if (leng > 0.0d0) then
      nrml(1,f) = nrml(1,f)/leng
      nrml(2,f) = nrml(2,f)/leng
      nrml(3,f) = nrml(3,f)/leng
    end if

  end do
!
!  Compute angles at edges common to 2 faces.
!
  do f = 1,nface

    i = hvl(f)

70  continue

    a = fvl(edgv,i)
    j = fvl(facn,a)

    if ( j >= f ) then
      dotp = nrml(1,f)*nrml(1,j) + nrml(2,f)*nrml(2,j) + nrml(3,f)*nrml(3,j)
      if (abs(dotp) > 1.0d0 - tol) dotp = sign(1.0d0,dotp)
      ang = pi() - acos(dotp)
      eang(i) = ang
      eang(a) = eang(i)
    end if

    i = fvl(succ,i)

    if (i /= hvl(f)) go to 70

  end do

  return
end
subroutine dsftdw ( l, u, k, lda, a, map )
!
!******************************************************************************
!
!! DSFTDW does one step of the heap sort algorithm for double precision data.
!
!
!  Purpose: 
!
!    Sift A(*,MAP(L)) down a heap of size U.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, L, U - lower and upper index of part of heap.
!
!    Input, K - dimension of points.
!
!    Input, LDA - leading dimension of array A in calling routine.
!
!    Input, A(1:K,1:*), see routine DHPSRT.
!
!    Input/output, MAP(1:*) - see routine DHPSRT.
!
  implicit none
!
  integer lda
!
  integer k
  integer l
  integer map(*)
  integer u

  double precision a(lda,*)
  logical dless
  integer i
  integer j
  integer t
!
  i = l
  j = 2*i
  t = map(i)

  do

    if (j > u) then
      exit
    end if

    if (j < u) then
      if (dless(k,a(1,map(j)),a(1,map(j+1)))) then
        j = j + 1
      end if
    end if

    if (dless(k,a(1,map(j)),a(1,t))) then
      exit
    end if

    map(i) = map(j)
    i = j
    j = 2*i

  end do

  map(i) = t

  return
end
subroutine dsmcpr ( nhole, nvbc, vcl, maxhv, maxpv, maxho, nvc, npolg, &
  nvert, nhola, regnum, hvl, pvl, iang, holv, ierr )
!
!******************************************************************************
!
!! DSMCPR initializes the polygonal decomposition data structure.
!
!
!  Purpose: 
!
!    Initialize the polygonal decomposition data structure
!    given a multiply-connected polygonal region with 1 outer
!    boundary curve and 0 or more inner boundary curves of holes.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NHOLE - number of holes in region.
!
!    Input, NVBC(1:NHOLE+1) - number of vertices per boundary curve; first
!    boundary curve is the outer boundary of the region.
!
!    Input, VCL(1:2,1:NVC) - vertex coordinates of boundary curves in counter
!    clockwise order; NVC = NVBC(1) + ... + NVBC(NHOLE+1); positions 1
!    to NVBC(1) of VCL contain the vertex coordinates of the
!    outer boundary in counter clockwise order; positions NVBC(1)+1 to
!    NVBC(1)+NVBC(2) contain the vertex coordinates of the
!    first hole boundary in counter clockwise order, etc.
!
!    Input, MAXHV - maximum size available for HVL, REGNUM arrays, should
!    be >= NHOLE + 1.
!
!    Input, MAXPV - maximum size available for PVL, IANG arrays; should be
!    >= NVC.
!
!    Input, MAXHO - maximum size available for HOLV array; should be
!    >= NHOLE*2.
!
!    Output, NVC - number of vertex coordinates, set to sum of NVBC(I).
!
!    Output, NPOLG - number of polygonal subregions, set to 1.
!
!    Output, NVERT - number of vertices in PVL, set to NVC.
!
!    Output, NHOLA - number of attached holes, set to 0.
!
!    Output, REGNUM(1:1) - region number of only subregion, set to 1.
!
!    [Note: Above 4 parameters are for consistency with DSPGDC.]
!
!    Output, HVL(1:NHOLE+1) - head vertex list; first entry is the head
!    vertex (index in PVL) of outer boundary curve; next
!    NHOLE entries contain the head vertex of a hole.
!
!    Output, PVL(1:4,1:NVC),IANG(1:NVC) - polygon vertex list and interior
!    angles; vertices of outer boundary curve are in counter clockwise order.
!    followed by vertices of each hole in CW hole; vertices
!    of each polygon are in a circular linked list; see
!    routine DSPGDC for more details of this data structure.
!
!    Output, HOLV(1:NHOLE*2) - indices in PVL of top and bottom vertices of
!    holes; first (last) NHOLE entries are for top (bottom)
!    vertices; top (bottom) vertices are sorted in decreasing
!    (increasing) lexicographic (y,x) order of coordinates.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxho
  integer maxpv
  integer nhole
!
  double precision angle
  integer, parameter :: edgv = 4
  integer holv(maxho)
  integer hvl(nhole+1)
  integer i
  double precision iang(maxpv)
  integer ierr
  integer iv
  integer ivs
  integer j
  integer, parameter :: loc = 1
  integer lv
  integer lvp
  integer lvs
  integer maxhv
  integer nhola
  integer npolg
  integer nv
  integer nvbc(nhole+1)
  integer nvc
  integer nvert
  integer nvs
  integer, parameter :: polg = 2
  integer pvl(4,maxpv)
  integer regnum(1)
  integer, parameter :: succ = 3
  double precision vcl(2,*)
!
  ierr = 0
  nvc = sum ( nvbc(1:nhole+1) )

  npolg = 1
  nvert = nvc
  nhola = 0
  regnum(1) = 1

  if (nhole + 1 > maxhv) then
    ierr = 4
    return
  else if (nvc > maxpv) then
    ierr = 5
    return
  else if (nhole + nhole > maxho) then
    ierr = 2
    return
  end if
!
!  Initialize HVL, PVL arrays.
!
20 continue

  hvl(1) = 1
  nv = nvbc(1)

  do i = 1,nv
    pvl(loc,i) = i
    pvl(polg,i) = 1
    pvl(succ,i) = i + 1
    pvl(edgv,i) = 0
  end do

  pvl(succ,nv) = 1

  do j = 1,nhole
    hvl(j+1) = nv + 1
    nvs = nv + nvbc(j+1)
    do i = nv+1,nvs
      pvl(loc,i) = i
      pvl(polg,i) = 1
      pvl(succ,i) = i - 1
      pvl(edgv,i) = 0
    end do
    pvl(succ,nv+1) = nvs
    nv = nvs
  end do
!
!  Initialize IANG array.
!
  do i = 1,nhole+1

    j = hvl(i)
    lvp = pvl(loc,j)
    iv = pvl(succ,j)
    lv = pvl(loc,iv)

    do

      ivs = pvl(succ,iv)
      lvs = pvl(loc,ivs)
      iang(iv) = angle(vcl(1,lvp),vcl(2,lvp),vcl(1,lv),vcl(2,lv), &
        vcl(1,lvs),vcl(2,lvs))

      if ( iv == j ) then
        exit
      end if

      lvp = lv
      iv = ivs
      lv = lvs

    end do

  end do
!
!  Initialize HOLV array.
!
  if (nhole > 0) then
    call holvrt(nhole,vcl,hvl(2),pvl,holv)
  end if

  return
end
subroutine dsmdf2 ( hflag, nvc, npolg, maxwk, vcl, hvl, pvl, iang, ivrt, &
  xivrt, widsq, edgval, vrtval, area, wk, ierr )
!
!******************************************************************************
!
!! DSMDF2 sets up a mesh distribution function data structure in 2D
!
!
!  Purpose: 
!
!    Set up data structure for heuristic mesh distribution
!    function from data structure for convex polygon decomposition
!    if HFLAG is .TRUE., else set up only IVRT and XIVRT.
!    Also compute areas of convex polygons.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, HFLAG - .TRUE. if data structure is to be constructed,
!    .FALSE. if only IVRT, XIVRT, AREA are to be computed.
!
!    Input, NVC - number of vertex coordinates in VCL array.
!
!    Input, NPOLG - number of polygonal subregions in HVL array.
!
!    Input, MAXWK - maximum size available for WK array; should be
!    2 times maximum number of vertices in any polygon.
!
!    Input, VCL(1:2,1:NVC) - vertex coordinate list.
!
!    Input, HVL(1:NPOLG) - head vertex list.
!
!    Input, PVL(1:4,1:*),IANG(1:*) - polygon vertex list, interior angles.
!
!    Output, IVRT(1:*) - indices of polygon vertices in VCL, ordered by
!    polygon; same size as PVL.
!
!    Output, XIVRT(1:NPOLG+1) - pointer to first vertex of each polygon
!    in IVRT; vertices of polygon K are IVRT(I) for I from
!    XIVRT(K) to XIVRT(K+1)-1.
!
!    Output, WIDSQ(1:NPOLG) - square of width of convex polygons.
!
!    Output, EDGVAL(1:*) - value associated with each edge of decomposition;
!    same size as PVL.
!
!    Output, VRTVAL(1:NVC) - value associated with each vertex of decomposition.
!
!    [Note: Above 5 arrays are for heuristic mdf data structure.]
!
!    Output, AREA(1:NPOLG) - area of convex polygons.
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxwk
  integer npolg
  integer nvc
!
  double precision area(npolg)
  double precision areapg
  integer, parameter :: edgv = 4
  double precision edgval(*)
  logical hflag
  integer hvl(npolg)
  integer i
  double precision iang(*)
  integer ierr
  integer il
  integer ivrt(*)
  integer j
  integer jl
  integer k
  integer l
  integer, parameter :: loc = 1
  integer m
  integer nvrt
  double precision pi
  double precision pimtol
  integer, parameter :: polg = 2
  integer pvl(4,*)
  double precision s
  integer, parameter :: succ = 3
  double precision tol
  double precision vcl(2,nvc)
  double precision vrtval(nvc)
  double precision widsq(npolg)
  double precision wk(maxwk)
  integer xc
  integer xivrt(npolg+1)
  integer yc
!
!  Compute area and square of width of polygons.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  pimtol = pi() - tol

  do k = 1,npolg

    nvrt = 0
    i = hvl(k)
10  continue
    if (iang(i) < pimtol) nvrt = nvrt + 1
    i = pvl(succ,i)
    if (i /= hvl(k)) go to 10

    if (nvrt + nvrt > maxwk) then
      ierr = 7
      return
    end if

    xc = 0

20  continue

    if (iang(i) < pimtol) then
      j = pvl(loc,i)
      xc = xc + 1
      wk(xc) = vcl(1,j)
      wk(xc+nvrt) = vcl(2,j)
    end if

    i = pvl(succ,i)
    if (i /= hvl(k)) go to 20

    xc = 1
    yc = xc + nvrt
    area(k) = areapg(nvrt,wk(xc),wk(yc))*0.5d0

    if (hflag) then
      call width2(nvrt,wk(xc),wk(yc),i,j,widsq(k), ierr )
      if (ierr /= 0) return
    end if

  end do
!
!  Set up IVRT, XIVRT, EDGVAL, VRTVAL arrays.
!
  l = 1

  do k = 1,npolg

    xivrt(k) = l
    i = hvl(k)
    il = pvl(loc,i)

40  continue

    ivrt(l) = il
    j = pvl(succ,i)
    jl = pvl(loc,j)

    if (hflag) then
      s = min((vcl(1,jl)-vcl(1,il))**2 + (vcl(2,jl)-vcl(2,il))**2, widsq(k))
      m = pvl(edgv,i)
      if (m > 0) then
        s = min(s,widsq(pvl(polg,m)))
      end if
      edgval(l) = s
    end if

    l = l + 1
    i = j
    il = jl

    if (i /= hvl(k)) then
      go to 40
    end if

  end do

  xivrt(npolg+1) = l
  if (.not. hflag) return

  vrtval(1:nvc) = 0.0d0

  do k = 1,npolg

    j = xivrt(k+1) - 1
    l = j

    do i = xivrt(k),l

      il = ivrt(i)

      if (vrtval(il) == 0.0d0) then
        vrtval(il) = min(edgval(i),edgval(j))
      else
        vrtval(il) = min(vrtval(il),edgval(i),edgval(j))
      end if

      j = i

    end do

  end do

  return
end
subroutine dsmdf3 ( nvc, nface, nvert, npolh, maxiw, maxwk, vcl, facep, &
  nrml, fvl, eang, hfl, pfl, ivrt, xivrt, ifac, xifac, wid, facval, edgval, &
  vrtval, ncface, ncedge, iwk, wk, ierr )
!
!******************************************************************************
!
!! DSMDF3 sets up a mesh distribution function data structure in 3D.
!
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Purpose: 
!
!    Set up data structure for heuristic mesh distribution
!    function from convex polyhedron decomposition data structure.
!
!  Parameters:
!
!    Input, NVC - number of vertex coordinates in VCL array.
!
!    Input, NFACE - number of faces or positions used in FACEP array.
!
!    Input, NVERT - number of positions used in FVL array.
!
!    Input, NPOLH - number of polyhedra or positions used in HFL array.
!
!    Input, MAXIW - maximum size available for IWK array; should be >=
!    MAX(NFACE, NCFACE + 6*NCVERT) where NCFACE = maximum number of
!    faces in a polyhedron, NCVERT = 2 * maximum number edges in a polyhedron.
!
!    Input, MAXWK - maximum size available for WK array; should be >=
!    3*NCFACE + NCVERT.
!
!    Input, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input, FACEP(1:3,1:NFACE) - face pointer list.
!
!    Input, NRML(1:3,1:NFACE) - unit normal vectors for faces.
!
!    Input, FVL(1:6,1:NVERT) - face vertex list.
!
!    Input, EANG(1:NVERT) - edge angles.
!
!    Input, HFL(1:NPOLH) - head pointer to face indices in PFL for each
!    polyhedron.
!
!    Input, PFL(1:2,1:*) - list of signed face indices for each polyhedron.
!
!    Output, IVRT(1:NVERT) - indices of face vertices in VCL, ordered by
!    face.
!
!    Output, XIVRT(1:NFACE+1) - pointer to first vertex of each face in
!    IVRT; vertices of face K are IVRT(I) for I from XIVRT(K)
!    to XIVRT(K+1)-1.
!
!    Output, IFAC(1:*) - indices of polyhedron faces in FACEP, ordered by
!    polyhedron; same size as PFL.
!
!    Output, XIFAC(1:NPOLH+1) - pointer to first face of each polyhedron in
!    IFAC; faces of polyhedron K are IFAC(I) for I from
!    XIFAC(K) to XIFAC(K+1)-1.
!
!    Output, WID(1:NPOLH) - width of convex polyhedra.
!
!    Output, FACVAL(1:NFACE) - value associated with each face of decomposition.
!
!    Output, EDGVAL(1:NVERT) - value associated with each edge of decomposition.
!
!    Output, VRTVAL(1:NVC) - value associated with each vertex of decomposition.
!
!    Output, NCFACE - maximum number of faces in a polyhedron.
!
!    Output, NCEDGE - maximum number of edges in a polyhedron.
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxiw
  integer maxwk
  integer nface
  integer npolh
  integer nvc
  integer nvert
!
  integer ccw
  integer ceang
  integer cfvl
  integer chvl
  integer cnrml
  double precision cxy
  double precision cyz
  double precision dir(3)
  double precision dirsq
  double precision dir1(3)
  double precision dir1sq
  double precision dotp
  double precision eang(nvert)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  double precision edgval(nvert)
  integer f
  integer facep(3,nface)
  integer, parameter :: facn = 2
  double precision facval(nface)
  double precision fmax
  integer fvl(6,nvert)
  integer hfl(npolh)
  integer i
  integer ierr
  integer ifac(*)
  integer irem
  integer ivrt(nvert)
  integer iwk(maxiw)
  integer j
  integer k
  double precision leng
  integer li
  integer lj
  integer, parameter :: loc = 1
  integer ncedge
  integer ncface
  integer ncvert
  double precision nrml(3,nface)
  integer nvrt
  integer p
  integer pfl(2,*)
  integer, parameter :: pred = 4
  double precision r21
  double precision r22
  integer, parameter :: succ = 3
  double precision sxy
  double precision syz
  double precision tol
  double precision vcl(3,nvc)
  double precision vrtval(nvc)
  double precision wid(npolh)
  double precision widsq
  double precision wk(maxwk)
  integer xc
  integer xifac(npolh+1)
  integer xivrt(nface+1)
  integer yc
!
!  Compute width of polyhedra.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )

  if (nface > maxiw) then
    ierr = 6
    return
  end if

  ncface = 0
  ncvert = 0
  nvrt = 0

  do f = 1,nface
    k = 0
    i = facep(1,f)
10  continue
    k = k + 1
    i = fvl(succ,i)
    if (i /= facep(1,f)) go to 10
    iwk(f) = k
    nvrt = max(nvrt,k)
  end do

  do p = 1,npolh
    j = 0
    k = 0
    i = hfl(p)
30  continue
    f = abs(pfl(1,i))
    j = j + 1
    k = k + iwk(f)
    i = pfl(2,i)
    if (i /= hfl(p)) go to 30
    ncface = max(ncface,j)
    ncvert = max(ncvert,k)
  end do

  ncedge = ncvert/2
  chvl = 1
  cfvl = chvl + ncface
  irem = cfvl + 5*ncvert
  cnrml = 1
  ceang = cnrml + 3*ncface

  if (irem + ncvert - 1 > maxiw) then
    ierr = 6
    return
  else if (ceang + ncvert - 1 > maxwk) then
    ierr = 7
    return
  end if

  do i = 1,npolh

    call dsconv(i,hfl(i),facep,nrml,fvl,eang,pfl,ncface,ncvert, &
      iwk(chvl),wk(cnrml),iwk(cfvl),wk(ceang))

    irem = cfvl + 5*ncvert

    call rmcpfc(ncface,ncvert,iwk(chvl),wk(cnrml),iwk(cfvl), &
      wk(ceang),iwk(irem))

    call rmcled(ncface,ncvert,iwk(chvl),iwk(cfvl))

    irem = cfvl + 5*ncvert

    call width3(ncface,vcl,iwk(chvl),wk(cnrml),iwk(cfvl), &
      maxiw-irem+1,j,k,wid(i),iwk(irem), ierr )

    if (ierr /= 0) return

  end do
!
!  Set up FACVAL array.
!  For each face, rotate normal vector to (0,0,1). Rotation matrix is
!    [ CXY     -SXY     0   ]
!    [ CYZ*SXY CYZ*CXY -SYZ ]
!    [ SYZ*SXY SYZ*CXY  CYZ ]
!  Rotate face vertices with int angles < PI before call to WIDTH2.
!
  fmax = 0.0d0
  xc = 1
  yc = xc + nvrt

  do f = 1,nface

    if (abs(nrml(1,f)) <= tol) then
      leng = nrml(2,f)
      cxy = 1.0d0
      sxy = 0.0d0
    else
      leng = sqrt(nrml(1,f)**2 + nrml(2,f)**2)
      cxy = nrml(2,f)/leng
      sxy = nrml(1,f)/leng
    end if

    cyz = nrml(3,f)
    syz = leng
    r21 = cyz*sxy
    r22 = cyz*cxy

    if (facep(2,f) > 0) then
      ccw = succ
    else
      ccw = pred
    end if

    nvrt = 0
    i = facep(1,f)
    li = fvl(loc,i)
    lj = fvl(loc,fvl(7-ccw,i))
    dir(1) = vcl(1,li) - vcl(1,lj)
    dir(2) = vcl(2,li) - vcl(2,lj)
    dir(3) = vcl(3,li) - vcl(3,lj)
    dirsq = dir(1)**2 + dir(2)**2 + dir(3)**2

60  continue

    j = fvl(ccw,i)
    lj = fvl(loc,j)
    dir1(1) = vcl(1,lj) - vcl(1,li)
    dir1(2) = vcl(2,lj) - vcl(2,li)
    dir1(3) = vcl(3,lj) - vcl(3,li)
    dir1sq = dir1(1)**2 + dir1(2)**2 + dir1(3)**2
    dotp = -(dir(1)*dir1(1) + dir(2)*dir1(2) + dir(3)*dir1(3))/ &
      sqrt(dirsq*dir1sq)

    if (dotp > -1.0d0 + tol) then
      wk(xc+nvrt) = cxy*vcl(1,li) - sxy*vcl(2,li)
      wk(yc+nvrt) = r21*vcl(1,li)+r22*vcl(2,li)-syz*vcl(3,li)
      nvrt = nvrt + 1
    end if

    i = j
    li = lj
    dir(1) = dir1(1)
    dir(2) = dir1(2)
    dir(3) = dir1(3)
    dirsq = dir1sq
    if (i /= facep(1,f)) go to 60
    call width2(nvrt,wk(xc),wk(yc),i,j,widsq, ierr )
    if (ierr /= 0) return
    facval(f) = min(sqrt(widsq), wid(abs(facep(2,f))))

    if (facep(3,f) /= 0) then
      facval(f) = min(facval(f), wid(abs(facep(3,f))))
    end if

    fmax = max(fmax,facval(f))

  end do
!
!  Set up IFAC, XIFAC, IVRT, XIVRT arrays.
!
  k = 1

  do p = 1,npolh

    xifac(p) = k
    i = hfl(p)

    do

      ifac(k) = pfl(1,i)
      i = pfl(2,i)
      k = k + 1

      if (i == hfl(p)) then
        exit
      end if

    end do

  end do

  xifac(npolh+1) = k

  k = 1

  do f = 1,nface
    xivrt(f) = k
    i = facep(1,f)
100 continue
    ivrt(k) = fvl(loc,i)
    fvl(pred,i) = k
    i = fvl(succ,i)
    k = k + 1
    if (i /= facep(1,f)) go to 100
  end do

  xivrt(nface+1) = k
!
!  Set up EDGVAL, VRTVAL arrays and reset FVL(PRED,*).
!
  edgval(1:nvert) = 0.0d0

  vrtval(1:nvc) = fmax

  do i = 1,nvert 

    if (edgval(fvl(pred,i)) > 0.0d0) then
      cycle
    end if

    li = fvl(loc,i)
    lj = fvl(loc,fvl(succ,i))
    leng = sqrt((vcl(1,lj) - vcl(1,li))**2 + (vcl(2,lj) - &
      vcl(2,li))**2 + (vcl(3,lj) - vcl(3,li))**2)
    k = i
    j = i

140 continue

    f = fvl(facn,j)
    leng = min(leng,facval(f))

    if (fvl(edga,j) == 0) then

      k = j
      j = fvl(edgc,i)

      do while (j /= 0)
        f = fvl(facn,j)
        leng = min(leng,facval(f))
        j = fvl(edgc,j)
      end do

    else if (fvl(edga,j) /= i) then

      j = fvl(edga,j)
      go to 140

    end if

    j = k

160 continue

    edgval(fvl(pred,j)) = leng
    j = fvl(edgc,j)
    if (j /= k .and. j /= 0) go to 160

    vrtval(li) = min(vrtval(li),leng)
    vrtval(lj) = min(vrtval(lj),leng)

  end do

  do f = 1,nface
    i = facep(1,f)
180 continue
    j = fvl(succ,i)
    fvl(pred,j) = i
    i = j
    if (i /= facep(1,f)) go to 180
  end do

  return
end
subroutine dspgdc ( nvc, vcl, incr, ncur, nvbc, icur, ivrt, maxhv, maxpv, &
  maxho, npolg, nvert, nhole, nhola, regnum, hvl, pvl, iang, holv, htsiz, &
  maxedg, ht, edge, map, ierr )
!
!******************************************************************************
!
!! DSPGDC initializes the polygonal decomposition data structure.
!
!
!  Purpose: 
!
!    Initialize the polygonal decomposition data structure
!    given an initial decomposition of a polygonal region which
!    may have holes and/or cut, separator, and hole interfaces.
!    Holes and hole interfaces must be simple polygons.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NVC - number of distinct vertex coordinates in region.
!
!    Input, VCL(1:2,1:NVC) - vertex coordinates of boundary curves in
!    arbitrary order.
!
!    Input, INCR - a positive integer >= NVC, e.g. 10000, added to some
!    elements of IVRT array.
!
!    Input, NCUR - number of boundary curves (includes outer boundary
!    curves of subregions and boundary curves of holes
!    and hole interfaces).
!
!    Input, NVBC(1:NCUR) - number of vertices per boundary curve.
!
!    Input, ICUR(1:NCUR) - indicates type and location of the curves:
!    ICUR(I) = 0 if Ith curve is outer boundary curve,
!    ICUR(I) = K if Ith curve is a hole and is inside
!    the subregion to the left of Kth curve,
!    ICUR(I) = -K if Ith curve is a hole interface and is
!    inside the subregion to the left of Kth curve.
!    K must be the index of an outer or hole interface
!    boundary curve (hole interfaces may be nested).
!    If the Ith curve is inside more than one subregion
!    due to nesting of hole interfaces, then the subregion
!    to the left of Kth curve must be the smallest
!    subregion containing the Ith curve.
!
!    Input, IVRT(1:NV) - indices in VCL of vertices of boundary curves;
!    NV = NVBC(1) + ... + NVBC(NCUR); the vertices of each
!    boundary curve must be in counter clockwise order; the first NVBC(1)
!    positions of IVRT are used for the first curve; the
!    next NVBC(2) positions are used for second curve, etc.
!    If the Ith curve is the outer boundary of a subregion
!    determined from cut and separator interfaces, then the
!    elements of IVRT which correspond to this curve are used
!    both for an index in VCL and indicating the type of the
!    edge joining a vertex and its successor as follows.
!    Let J be in range of positions used for the Ith curve
!    and K be the index in VCL of the coordinates of a vertex
!    of the Ith curve. Consider the edge originating from this
!    vertex. IVRT(J) = -K if the edge is part of a cut or
!    separator interface (i.e. there is a subregion to right
!    of edge). IVRT(J) = K if the edge is part of the outer
!    boundary of the region (i.e. the unbounded exterior of
!    the region is to the right of edge). IVRT(J) = K + INCR
!    if the edge is part of the boundary of a hole (i.e.
!    there is a bounded area to the right of edge which is
!    not in the region. If the Ith curve is the boundary of
!    a hole or hole interface, then only IVRT(J) = K is used.
!
!    Input, MAXHV - maximum size available for HVL, REGNUM arrays, should
!    be >= NCUR + (number of hole interfaces).
!
!    Input, MAXPV - maximum size available for PVL, IANG arrays; should be
!    >= NVERT (see below).
!
!    Input, MAXHO - maximum size available for HOLV array; should be
!    >= NHOLE*2 + NHOLA (see below).
!
!    Input, HTSIZ - size of hash table HT; should be a prime number which
!    is about NSC/2 where NSC is number of separator and cut
!    interface edges.
!
!    Input, MAXEDG - maximum size available for EDGE array; should be at
!    least NSC.
!
!    Output, NPOLG - number of polygonal subregions, set to number of outer
!    subregion boundaries plus number of hole interfaces.
!
!    Output, NVERT - number of vertices in PVL, set to NV plus number of
!    vertices in holes and hole interfaces (< 2*NV).
!
!    Output, NHOLE - number of holes and hole interfaces.
!
!    Output, NHOLA - number of 'attached' holes; these holes are attached
!    to the outer boundary of a subregion through vertices
!    or cut interfaces and have their edges in consecutive
!    order on the boundary (<= NV/4).
!
!    Output, REGNUM(1:NPOLG) - region numbers to left of outer and hole
!    interface boundary curves, which are set to the indices
!    of ICUR or NVBC; this array may be useful in some
!    applications for identifying which original region a
!    subpolygon belongs to.
!
!    Output, HVL(1:NPOLG+NHOLE) - head vertex list; the first NPOLG
!    positions contain the head vertex (index in PVL) of an
!    outer or hole interface boundary curve in which the
!    vertices of the curve are in counter clockwise order in PVL; next
!    NHOLE positions contain the head vertex of a hole or
!    hole interface in which vertices are in CW order in PVL.
!
!    Output, PVL(1:4,1:NVERT), IANG(1:NVERT) - polygon vertex list and
!    interior angles; contains the 5 'arrays' LOC, POLG, SUCC
!    EDGV, IANG (the first 4 are integer arrays, the last
!    is a double precision array); the vertices of each
!    polygon (except for holes) are stored in counter clockwise order in a
!    circular linked list. PVL(LOC,V) is the location in VCL
!    of the coordinates of 'vertex' (index) V. IANG(V) is
!    the interior angle at vertex V. PVL(POLG,V) is polygon
!    number (index of HVL) of subregion containing vertex V
!    (this entry is different from the polygon index only
!    for holes). PVL(SUCC,V) is index in PVL of successor
!    vertex of vertex V. PVL(EDGV,V) gives information about
!    the edge joining vertices V and its successor - if the
!    edge is part of 1 polygon then PVL(EDGV,V) = 0; if the
!    edge is common to 2 polygons then PVL(EDGV,V) > 0 and
!    is equal to the index in PVL of the successor vertex
!    as represented in the other polygon; i.e. in latter
!    case, PVL(LOC,PVL(EDGV,V)) = PVL(LOC,PVL(SUCC,V)) and
!    PVL(EDGV,PVL(EDGV,V)) = V.
!
!    Output, HOLV(1:NHOLE*2+NHOLA) - indices in PVL of top or bottom vertex
!    of holes; first (next) NHOLE entries are for top (bottom)
!    vertices of holes and hole interfaces, with top (bottom)
!    vertices sorted in decreasing (increasing) lexicographic
!    (y,x) order of coordinate; last NHOLA entries are for attached
!    holes; if bottom vertex of attached hole is a simple
!    vertex of boundary curve containing the hole then entry
!    contains index of bottom vertex otherwise entry contains
!    index of top vertex (which is simple).
!
!    Workspace, MAP(1:NCUR) - used for mapping input boundary curve numbers
!    to polygon numbers.
!
!    Workspace, HT(0:HTSIZ-1), EDGE(1:4,1:MAXEDG) - hash table and edge records
!    used to determine matching occurrences of separator or
!    cut interface edges by calling routine EDGHT.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer htsiz
  integer maxedg
  integer maxho
  integer maxhv
  integer maxpv
  integer ncur
  integer nvc
!
  double precision angle
  integer edge(4,maxedg)
  integer, parameter :: edgv = 4
  logical first
  integer hdfree
  integer holv(maxho)
  integer ht(0:htsiz-1)
  integer hvl(maxhv)
  integer i
  double precision iang(maxpv)
  integer icur(ncur)
  integer ierr
  integer incr
  integer ipoly
  integer iv
  integer ivrt(*)
  integer ivs
  integer j
  integer j1
  integer j2
  integer jend
  integer jstr
  integer k
  integer kmax
  integer kmin
  integer kpoly
  integer l
  integer last
  integer, parameter :: loc = 1
  integer lv
  integer lvp
  integer lvs
  integer map(ncur)
  integer mpoly
  integer nh2
  integer nhola
  integer nhole
  integer nholi
  integer nht
  integer npolg
  integer nv
  integer nvbc(ncur)
  integer nvert
  integer, parameter :: polg = 2
  integer pvl(4,maxpv)
  integer regnum(maxhv)
  integer, parameter :: succ = 3
  double precision vcl(2,nvc)
  double precision x
  double precision xmax
  double precision xmin
  double precision y
  double precision ymax
  double precision ymin
!
  ierr = 0
  nhola = 0
  nhole = 0
  nholi = 0
  nvert = 0

  do i = 1,ncur
    nvert = nvert + nvbc(i)
    if (icur(i) > 0) then
      nhole = nhole + 1
    else if (icur(i) < 0) then
      nholi = nholi + 1
      nvert = nvert + nvbc(i)
    end if
  end do

  npolg = ncur - nhole
  ipoly = 0
  iv = 0
  nv = 0
  hdfree = 0
  last = 0
  nht = 0
  ht(0:htsiz-1) = 0

  if (ncur + nholi > maxhv) then
    ierr = 4
    return
  else if (nvert > maxpv) then
    ierr = 5
    return
  else if ((nhole + nholi)*2 > maxho) then
    ierr = 2
    return
  end if
!
!  Initialize REGNUM, HVL, PVL arrays for outer boundary curves.
!
  do i = 1,ncur

    if (icur(i) /= 0) then
      map(i) = 0
      go to 40
    end if

    ipoly = ipoly + 1
    regnum(ipoly) = i
    hvl(ipoly) = iv + 1
    map(i) = ipoly
    jstr = nv + 1
    jend = nv + nvbc(i)

    do j = jstr,jend

      iv = iv + 1
      pvl(loc,iv) = abs(ivrt(j))
      pvl(polg,iv) = ipoly
      pvl(succ,iv) = iv + 1

      if (ivrt(j) > 0) then
        pvl(edgv,iv) = 0
      else
!
!  The edge originating from current vertex is on a cut or
!  separator interface. Search in hash table for edge, and
!  insert or delete edge. Set EDGV value if possible.
!
        lv = abs(ivrt(j))
        if (lv > incr) lv = lv - incr

        if (j < jend) then
          lvs = abs(ivrt(j+1))
        else
          lvs = abs(ivrt(jstr))
        end if

        if (lvs > incr) lvs = lvs - incr

        call edght ( lv, lvs, iv, nvc, htsiz, maxedg, hdfree, last, ht, &
          edge, ivs, ierr )

        if (ierr /= 0) return

        if (ivs > 0) then
          pvl(edgv,iv) = ivs
          pvl(edgv,ivs) = iv
          nht = nht - 1
        else
          nht = nht + 1
        end if

      end if

    end do

    pvl(succ,iv) = hvl(ipoly)

40  continue

    nv = nv + nvbc(i)

  end do

  if (nht /= 0) then
    ierr = 215
    return
  end if
!
!  Initialize REGNUM, HVL, PVL arrays for the hole interfaces.
!
  if (nholi == 0) go to 100

  do i = 1,ncur
    if (icur(i) < 0) then
      ipoly = ipoly + 1
      map(i) = ipoly
    end if
  end do

  nv = 0

  do i = 1,ncur

    if (icur(i) >= 0) go to 80

    ipoly = ipoly + 1
    kpoly = ipoly - nholi
    mpoly = map(-icur(i))
    regnum(kpoly) = i
    hvl(kpoly) = iv + 1
    hvl(ipoly) = iv + 2
    jstr = nv + 1
    jend = nv + nvbc(i)

    do j = jstr,jend
      iv = iv + 2
      pvl(loc,iv-1) = ivrt(j)
      pvl(polg,iv-1) = kpoly
      pvl(succ,iv-1) = iv + 1
      pvl(edgv,iv-1) = iv + 2
      pvl(loc,iv) = ivrt(j)
      pvl(polg,iv) = mpoly
      pvl(succ,iv) = iv - 2
      pvl(edgv,iv) = iv - 3
    end do

    pvl(succ,iv-1) = hvl(kpoly)
    pvl(edgv,iv-1) = hvl(ipoly)
    pvl(succ,hvl(ipoly)) = iv
    pvl(edgv,hvl(ipoly)) = iv - 1

80  continue

    nv = nv + nvbc(i)

  end do
!
!  Initialize HVL, PVL arrays for the ordinary holes.
!
100 continue

  if (nhole == 0) go to 140

  nv = 0

  do i = 1, ncur

    if ( icur(i) > 0 ) then

      ipoly = ipoly + 1
      mpoly = map(icur(i))
      hvl(ipoly) = iv + 1
      jstr = nv + 1
      jend = nv + nvbc(i)

      do j = jstr, jend
        iv = iv + 1
        pvl(loc,iv) = ivrt(j)
        pvl(polg,iv) = mpoly
        pvl(succ,iv) = iv - 1
        pvl(edgv,iv) = 0
      end do

      pvl(succ,hvl(ipoly)) = iv

    end if

    nv = nv + nvbc(i)

  end do
!
!  Determine bottom or top simple vertex of attached holes.
!
140 continue

  nhole = nhole + nholi
  nh2 = nhole + nhole
  j1 = 0
  j2 = 0

  do i = 1,npolg-nholi

    j = hvl(i)

150 continue

    if (pvl(loc,j) > incr) then
      j = pvl(succ,j)
      if (j /= hvl(i)) then
        go to 150
      else
        ierr = 216
        return
      end if
    end if

    first = .true.

160 continue

    lv = pvl(loc,j)

    if (j1 > 0) then
      if (lv <= incr) then
        j2 = j
      else if (lv - incr == lvs) then
        j2 = j
      else
        pvl(loc,j) = lv - incr
      end if
    else if (lv > incr) then
      j1 = j
      lvs = lv - incr
      pvl(loc,j) = lvs
    end if

    if (j2 > 0) then
!
!  (Part of) hole starts at vertex J1 and ends at J2.
!
      if (lv <= incr .and. lv /= lvs) go to 180

      k = j1

170   continue

      if (k == j1) then

        kmin = k
        kmax = k
        xmin = vcl(1,lvs)
        ymin = vcl(2,lvs)
        xmax = xmin
        ymax = ymin

      else

        l = pvl(loc,k)
        x = vcl(1,l)
        y = vcl(2,l)

        if (y < ymin .or. y == ymin .and. x < xmin) then
          kmin = k
          xmin = x
          ymin = y
        else if (y > ymax .or. y == ymax .and. x > xmax) then
          kmax = k
          xmax = x
          ymax = y
        end if

      end if

      k = pvl(succ,k)
      if (k /= j2) go to 170
      if (kmin == j1) kmin = kmax
      nhola = nhola + 1

      if (nh2 + nhola > maxho) then
        ierr = 2
        return
      end if

      holv(nh2+nhola) = kmin

180   continue

      j1 = 0
      j2 = 0

      if (lv > incr) then
        j1 = j
        pvl(loc,j) = lvs
      end if

    end if

    j = pvl(succ,j)

    if (first) then
      first = .false.
      jend = j
      go to 160
    else if (j /= jend) then
      go to 160
    end if

  end do
!
!  Initialize IANG array.
!
  do i = 1,npolg+nhole

    j = hvl(i)
    lvp = pvl(loc,j)
    iv = pvl(succ,j)
    lv = pvl(loc,iv)

    do

      ivs = pvl(succ,iv)
      lvs = pvl(loc,ivs)
      iang(iv) = angle(vcl(1,lvp),vcl(2,lvp),vcl(1,lv),vcl(2,lv), &
        vcl(1,lvs),vcl(2,lvs))

      if (iv == j) then
        exit
      end if

      lvp = lv
      iv = ivs
      lv = lvs

    end do

  end do
!
!  Initialize HOLV array.
!
  if (nhole > 0) then
    call holvrt(nhole,vcl,hvl(npolg+1),pvl,holv)
  end if

  return
end
subroutine dsphdc ( nvc, nface, npolh, vcl, facep, nrml, fvl, eang, hfl, &
  pfl, htsiz, maxedg, edge, ht, ierr )
!
!******************************************************************************
!
!! DSPHDC initializes the polyhedral decomposition data structure.
!
!
!  Purpose: 
!
!    Initialize the polyhedral decomposition data structure
!    where there are no holes on faces and no interior holes.  It is
!    assumed head vertex of each face is a strictly convex vertex.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NVC - number of vertex coordinates.
!
!    Input, NFACE - number of faces in polyhedral decomposition.
!
!    Input, NPOLH - number of polyhedra in decomposition.
!
!    Input, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input, FACEP(1,1:NFACE+1) - head pointer to vertex indices in FVL for
!    each face; 1 = FACEP(1,1) < ... < FACEP(1,NFACE+1).
!
!    Input, FVL(1,1:*) - vertex indices; those for Ith face are in FVL(1,J)
!    for J = FACEP(1,I),...,FACEP(1,I+1)-1.
!
!    Input, HFL(1:NPOLH+1) - head pointer to face indices in PFL for each
!    polyhedron; 1 = HFL(1) < HFL(2) < ... < HFL(NPOLH+1).
!
!    Input, PFL(1,1:*) - signed face indices; those for Ith polyhedron are in
!    PFL(1,J) for J = HFL(I),...,HFL(I+1)-1; the face index
!    must be negated if the ordering of vertices for the face
!    in FVL is in CW order when viewed from outside Ith polyhedron.
!
!    Input, HTSIZ - size of hash table HT; should be a prime number which
!    is >= NVC+2.
!
!    Input, MAXEDG - maximum size available for EDGE array; should be at
!    least maximum number of edges in a polyhedron of decomposition.
!
!    Output, FACEP(1:3,1:NFACE) - FACEP(1,F) same as input; FACEP(2,F) and
!    FACEP(3,F) are signed indices of 2 polyhedra sharing face
!    F; if F is boundary face then FACEP(3,F) = 0; the sign of
!    the polyhedron index indicates whether face is oriented 
!    counter clockwise (positive) or clockwise (negative) in 
!    FVL when viewed from outside the polyhedron; if interior 
!    face, 2 signs are different.
!
!    Output, NRML(1:3,1:NFACE) - normals at faces; NRML(*,F) is unit outward
!    normal of face F with its vertices oriented counter clockwise when
!    viewed from outside polyhedron |FACEP(2,F)|.
!
!    Output, FVL(1:6,1:NVERT) - face vertex list where NVERT = FACEP(1,
!    NFACE+1)-1; 6 rows are for LOC, FACN, SUCC, PRED, EDGA,
!    EDGC; first 4 fields are the same as that used for the
!    convex polyhedron data structure (see routine DSCPH).
!    EDGA and EDGC give information about the edge UV where
!    V = FVL(SUCC,U). Let LU = FVL(LOC,U), LV = FVL(LOC,V),
!    and SF = +1 (-1) if face containing UV in polyhedron P is
!    oriented counter clockwise (CW) when viewed from outside P. Let WX be
!    edge corresponding to UV in the adjacent face of P, where
!    X = FVL(SUCC,W). If (LV-LU)*SF > 0, then FVL(EDGC,U) = W,
!    FVL(EDGA,W) = U, and EANG(U) is angle at UV between the
!    2 faces inside P; else FVL(EDGA,U) = W, FVL(EDGC,W) = U,
!    and EANG(W) is the edge angle. In other words, if P is
!    viewed from outside with edge UV directed upwards from
!    vertex with smaller LOC value to other vertex, then there
!    is a counter clockwise or CW rotation in P from face containing UV to
!    other face as indicated by EDGA or EDGC, respectively (A
!    for AntiCW, C for clockwise). If the counter clockwise 
!    or clockwise rotation between 2 faces is exterior to the region, 
!    then the EDGA or EDGC value is 0 and EANG value is -1.
!
!    Output, EANG(1:NVERT) - angles at edges common to 2 faces in a polyhedron;
!    EANG(J) corresponds to FVL(*,J) and is determined by
!    EDGC field.
!
!    Output, PFL(1:2,1:NPF) - row 1 same as input and row 2 used for link,
!    where NPF = HFL(NPOLH+1)-1.
!
!    Workspace, HT(0:HTSIZ-1), EDGE(1:4,1:MAXEDG) - hash table and edge records
!    used to determine matching occurrences of polyhedron
!    edges by calling routine EDGHT.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer htsiz
  integer maxedg
  integer nface
  integer npolh
  integer nvc
!
  double precision ab(3)
  double precision ac(3)
  double precision ang
  integer ccw
  double precision dotp
  double precision eang(*)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer edge(4,maxedg)
  double precision en(3)
  integer f
  integer facep(3,nface+1)
  integer, parameter :: facn = 2
  logical fflag
  integer fvl(6,*)
  integer g
  logical gflag
  integer hdfree
  integer hfl(npolh+1)
  integer ht(0:htsiz-1)
  integer i
  integer ierr
  integer j
  integer k
  integer l
  integer la
  integer last
  integer lb
  integer lc
  double precision leng
  integer, parameter :: loc = 1
  integer nht
  double precision nrml(3,nface)
  integer p
  integer pfl(2,*)
  double precision pi
  double precision pi2
  integer, parameter :: pred = 4
  integer sf
  integer, parameter :: succ = 3
  double precision tol
  double precision vcl(3,nvc)
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  pi2 = 2.0d0 * pi()
  hdfree = 0
  last = 0
  nht = 0

  ht(0:htsiz-1) = 0

  do i = 1,nface
    facep(2,i) = 0
    facep(3,i) = 0
    k = facep(1,i)
    l = facep(1,i+1) - 1
    do j = k,l
      fvl(facn,j) = i
      fvl(succ,j) = j + 1
      fvl(pred,j) = j - 1
      fvl(edga,j) = 0
      fvl(edgc,j) = 0
      eang(j) = -1.0d0
    end do
    fvl(succ,l) = k
    fvl(pred,k) = l
  end do

  do i = 1,npolh

    k = hfl(i)
    l = hfl(i+1) - 1

    do j = k,l
      pfl(2,j) = j + 1
      f = pfl(1,j)
      p = sign(i,f)
      f = abs(f)
      if (facep(2,f) == 0) then
        facep(2,f) = p
      else
        facep(3,f) = p
      end if
    end do

    pfl(2,l) = k

  end do

  do f = 1,nface
    if (facep(2,f)*facep(3,f) > 0) then
      ierr = 321
      return
    end if
  end do
!
!  Compute normals for each face from orientation in FACEP(2,*).
!
  do f = 1,nface

    if (facep(2,f) > 0) then
      ccw = succ
    else
      ccw = pred
    end if

    j = facep(1,f)
    lb = fvl(loc,j)
    lc = fvl(loc,fvl(ccw,j))
    la = fvl(loc,fvl(7-ccw,j))
    ab(1:3) = vcl(1:3,lb) - vcl(1:3,la)
    ac(1:3) = vcl(1:3,lc) - vcl(1:3,la)
    nrml(1,f) = ab(2)*ac(3) - ab(3)*ac(2)
    nrml(2,f) = ab(3)*ac(1) - ab(1)*ac(3)
    nrml(3,f) = ab(1)*ac(2) - ab(2)*ac(1)
    leng = sqrt(nrml(1,f)**2 + nrml(2,f)**2 + nrml(3,f)**2)

    if (leng > 0.0d0) then
      nrml(1,f) = nrml(1,f)/leng
      nrml(2,f) = nrml(2,f)/leng
      nrml(3,f) = nrml(3,f)/leng
    end if

  end do
!
!  Determine EDGA, EDGC fields and compute EANG values.
!
  do p = 1,npolh

    nht = 0

    do i = hfl(p),hfl(p+1)-1

      sf = pfl(1,i)
      f = abs(sf)

      do j = facep(1,f),facep(1,f+1)-1

        la = fvl(loc,j)
        lb = fvl(loc,fvl(succ,j))

        call edght ( la, lb, j, nvc, htsiz, maxedg, hdfree, last, ht, &
          edge, k, ierr )

        if (ierr /= 0) then
          return
        end if

        if (k <= 0) then

          nht = nht + 1

        else

          nht = nht - 1
          g = fvl(facn,k)
          dotp = nrml(1,f)*nrml(1,g) + nrml(2,f)*nrml(2,g) + &
            nrml(3,f)*nrml(3,g)
          if (abs(dotp) > 1.0d0-tol) dotp = sign(1.0d0,dotp)
          fflag = (abs(facep(2,f)) == p)
          gflag = (abs(facep(2,g)) == p)
          if (fflag .neqv. gflag) dotp = -dotp
          ang = pi() - acos(dotp)
!
!  Determine whether edge angle is reflex.
!
          ab(1:3) = vcl(1:3,lb) - vcl(1:3,la)
          en(1) = nrml(2,f)*ab(3) - nrml(3,f)*ab(2)
          en(2) = nrml(3,f)*ab(1) - nrml(1,f)*ab(3)
          en(3) = nrml(1,f)*ab(2) - nrml(2,f)*ab(1)

          if (fflag .neqv. (sf > 0)) then
            en(1:3) = -en(1:3)
          end if
!
!  AC = (midpoint of A and B) + EN - A
!
          do l = 1,3
            ac(l) = 0.5d0*(vcl(l,lb) - vcl(l,la)) + en(l)
          end do

          dotp = ac(1)*nrml(1,g)+ac(2)*nrml(2,g)+ac(3)*nrml(3,g)
          if (.not. gflag) dotp = -dotp
          if (dotp > 0.0d0) ang = pi2 - ang

          if ((lb - la)*sf > 0) then
            fvl(edgc,j) = k
            fvl(edga,k) = j
            eang(j) = ang
          else
            fvl(edga,j) = k
            fvl(edgc,k) = j
            eang(k) = ang
          end if

        end if

      end do

    end do

    if (nht /= 0) then
      ierr = 322
      return
    end if

  end do

  return
end
subroutine dsphfh ( aspc2d, atol2d, nvc, nface, nhole, npolh, maxvc, maxfv, &
  maxiw, maxwk, nvert, npf, vcl, facep, factyp, nrml, fvl, eang, hfl, pfl, &
  htsiz, ht, iwk, wk, ierr )
!
!******************************************************************************
!
!! DSPHFH initializes the polyhedral decomposition data structure.
!
!
!  Purpose: 
!
!    Initialize the polyhedral decomposition data structure
!    where there may be non-intersecting holes on boundary faces of
!    polyhedral region.  It is assumed head vertex of outer polygon
!    of each face is a strictly convex vertex, and all polygons are
!    simple.  Faces with holes are decomposed into simple polygons.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, ASPC2D - angle spacing parameter in radians used in controlling
!    vertices to be considered as an endpoint of a separator.
!
!    Input, ATOL2D - angle tolerance parameter in radians used in accepting
!    separator to resolve a hole on a face.
!
!    Input/output, NVC - number of vertex coordinates.
!
!    Input/output, NFACE - number of faces (outer polygon boundaries) in
!    polyhedral decomposition.
!
!    Input, NHOLE - number of holes (inner polygons) on all faces.
!
!    Input, NPOLH - number of polyhedra in decomposition.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, MAXIW - maximum size available for IWK array; should be about
!    max(4*(max number of edges in a polyhedron of decomposition),
!    6*max(NV + 8*NFHOL)) where NV is number of vertices and
!    NFHOL is number of holes on a multiply-connected face.
!
!    Input, MAXWK - maximum size available for WK array; should be about
!    7*max(NV + 8*NFHOL).
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input, FACEP(1,1:NFACE+NHOLE+1) - head pointer to vertex indices in
!    FVL for each polygon (face or hole); 1 = FACEP(1,1) < ...
!    < FACEP(1,NFACE+NHOLE+1).
!
!    Input, FACTYP(1:NFACE) - face types: useful for specifying types of
!    boundary faces; entries must be >= 0.
!
!    Input/output, FACTYP(NFACE+1:NFACE+NHOLE) - for I from NFACE+1 to
!    NFACE+NHOLE, FACTYP(I) = +F or -F where 1 <= F <= NFACE is index of
!    face containing hole and sign is + (-) if hole polygon is
!    oriented counter clockwise (CW) in polyhedron when viewed from outside.
!
!    Input, FVL(1,1:*) - vertex indices; those for Ith polygon are in
!    FVL(1,J) for J = FACEP(1,I),...,FACEP(1,I+1)-1; all those
!    for outer polygons must appear before those for holes,
!    and holes must appear in nondecreasing |FACTYP(I)| order.
!
!    Input, HFL(1:NPOLH+1) - head pointer to face indices in PFL for each
!    polyhedron; 1 = HFL(1) < HFL(2) < ... < HFL(NPOLH+1).
!
!    Input, PFL(1,1:*) - signed face indices; those for Ith polyhedron are in
!    PFL(1,J) for J = HFL(I),...,HFL(I+1)-1; the face index
!    must be negated if the ordering of vertices for the face
!    in FVL is in CW order when viewed from outside Ith polyhedron;
!    indices for holes should not be included.
!
!    Input, HTSIZ - size of hash table HT; should be a prime number which
!    is >= NVC+2.
!
!    Output, NVERT - number of positions used in FVL, EANG arrays.
!
!    Output, NPF - number of positions used in PFL array.
!
!    Output, FACEP(1:3,1:NFACE) - FACEP(1,F) is head pointer to face
!    vertices; FACEP(2,F) and FACEP(3,F) are signed indices of
!    2 polyhedra sharing face F; if F is boundary face then
!    FACEP(3,F) = 0; the sign of the polyhedron index indicates
!    whether face is oriented counter clockwise (positive) or 
!    clockwise (negative) in FVL when viewed from outside 
!    polyhedron; if interior face, 2 signs are different.
!
!    Output, NRML(1:3,1:NFACE) - normals at faces; NRML(*,F) is unit outward
!    normal of face F with its vertices oriented counter clockwise when
!    viewed from outside polyhedron |FACEP(2,F)|.
!
!    Output, FVL(1:6,1:NVERT) - face vertex list; 6 rows are for LOC, FACN,
!    SUCC, PRED, EDGA, EDGC; first 4 fields are same as that
!    used for convex polyhedron data structure (see routine DSCPH).
!    EDGA and EDGC give information about the edge UV where
!    V = FVL(SUCC,U). Let LU = FVL(LOC,U), LV = FVL(LOC,V),
!    and SF = +1 (-1) if face containing UV in polyhedron P is
!    oriented counter clockwise (CW) when viewed from outside P. 
!    Let WX be the edge corresponding to UV in the adjacent face 
!    of P, where X = FVL(SUCC,W).  If (LV-LU)*SF > 0, then 
!    FVL(EDGC,U) = W, FVL(EDGA,W) = U, and EANG(U) is angle at 
!    UV between the 2 faces inside P; else FVL(EDGA,U) = W,
!    FVL(EDGC,W) = U, and EANG(W) is the edge angle.  In other 
!    words, if P is viewed from outside with edge UV directed 
!    upwards from vertex with smaller LOC value to other vertex, 
!    then there is a counter clockwise or clockwise rotation in P 
!    from face containing UV to other face as indicated by EDGA 
!    or EDGC, respectively (A for counter clockwise, C for 
!    clockwise). If the counter clockwise or clockwise rotation
!    between 2 faces is exterior to the region, then the EDGA or EDGC
!    value is 0 and EANG value is -1.
!
!    Output, EANG(1:NVERT) - angles at edges common to 2 faces in a polyhedron;
!    EANG(J) corresponds to FVL(*,J) and is determined by
!    EDGC field.
!
!    Output, PFL(1:2,1:NPF) - list of signed face indices for each polyhedron,
!    row 2 used for link; NPF exceeds the input size by at
!    most NHOLE; it is assumed there is enough space.
!
!    Workspace, HT(0:HTSIZ-1) - hash table used to find matching occurrences
!    of polyhedron edges by calling routine EDGHT.
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer htsiz
  integer maxfv
  integer maxiw
  integer maxvc
  integer maxwk
  integer nface
  integer nhole
  integer npolh
!
  double precision ab(3)
  double precision ac(3)
  double precision ang
  double precision aspc2d
  double precision atol2d
  integer ccw
  double precision d
  double precision dotp
  double precision dtol
  double precision eang(maxfv)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer edgv
  double precision en(3)
  integer f
  integer facep(3,nface+nhole+1)
  integer, parameter :: facn = 2
  integer factyp(nface+nhole)
  integer ff
  logical fflag
  integer fvl(6,maxfv)
  integer g
  logical gflag
  integer hdfree
  integer hfl(npolh+1)
  integer ht(0:htsiz-1)
  integer i
  integer ierr
  integer irem
  integer iwk(maxiw)
  integer j
  integer k
  integer l
  integer la
  integer last
  integer lb
  integer lc
  double precision leng
  integer link
  integer, parameter :: loc = 1
  integer locfv
  integer maxedg
  integer maxpf
  integer, parameter :: msglvl = 0
  integer nfacin
  integer nfhol
  integer nfph
  integer nht
  integer npf
  double precision nrml(3,nface+nhole)
  integer nvc
  integer nvert
  integer p
  integer pfl(2,*)
  double precision pi
  double precision pi2
  integer, parameter :: pred = 4
  integer sf
  integer size
  integer, parameter :: succ = 3
  double precision tol
  double precision vcl(3,maxvc)
  double precision wk(maxwk)
  integer wrem
  integer y
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  pi2 = 2.0d0 * pi()
  maxedg = maxiw/4
  nfph = nface + nhole
  nvert = facep(1,nfph+1) - 1
  npf = hfl(npolh+1) - 1
  maxpf = npf + nhole
  hdfree = 0
  last = 0
  nht = 0

  ht(0:htsiz-1) = 0

  do i = 1,nface
    facep(2,i) = 0
    facep(3,i) = 0
    k = facep(1,i)
    l = facep(1,i+1) - 1
    do j = k,l
      fvl(facn,j) = i
      fvl(succ,j) = j + 1
      fvl(pred,j) = j - 1
      fvl(edga,j) = 0
      fvl(edgc,j) = 0
      eang(j) = -1.0d0
    end do
    fvl(succ,l) = k
    fvl(pred,k) = l
  end do

  do i = 1,npolh

    k = hfl(i)
    l = hfl(i+1) - 1

    do j = k,l
      pfl(2,j) = j + 1
      f = pfl(1,j)
      p = sign(i,f)
      f = abs(f)
      if (facep(2,f) == 0) then
         facep(2,f) = p
      else
         facep(3,f) = p
      end if
    end do

    pfl(2,l) = k

  end do

  do f = 1,nface

    if (facep(2,f)*facep(3,f) > 0) then
      ierr = 321
      return
    end if

  end do
!
!  Process holes.
!
  do i = nface+1,nface+nhole-1
    if (abs(factyp(i)) > abs(factyp(i+1))) then
      ierr = 340
      return
    end if
  end do

  do i = nface+1,nface+nhole

    sf = factyp(i)
    f = abs(sf)

    if (facep(3,f) /= 0) then
      ierr = 341
      return
    end if

    fflag = (facep(2,f)*sf < 0)
    facep(2,i) = facep(2,f)
    k = facep(1,i)
    l = facep(1,i+1) - 1

    do j = k,l

      fvl(facn,j) = f

      if (fflag) then
        fvl(succ,j) = j + 1
        fvl(pred,j) = j - 1
      else
        fvl(succ,j) = j - 1
        fvl(pred,j) = j + 1
      end if

      fvl(edga,j) = 0
      fvl(edgc,j) = 0
      eang(j) = -1.0d0

    end do

    if (fflag) then
      fvl(succ,l) = k
      fvl(pred,k) = l
    else
      fvl(pred,l) = k
      fvl(succ,k) = l
    end if

  end do
!
!  Compute normals for each face from orientation in FACEP(2,*), and
!  check that face vertices (including those on holes) are coplanar.
!
  g = nface + 1

  do f = 1,nface

    if (facep(2,f) > 0) then
      ccw = succ
    else
      ccw = pred
    end if

    j = facep(1,f)
    k = fvl(ccw,j)
    l = fvl(7-ccw,j)
    lb = fvl(loc,j)
    lc = fvl(loc,k)
    la = fvl(loc,l)

    ab(1:3) = vcl(1:3,lb) - vcl(1:3,la)
    ac(1:3) = vcl(1:3,lc) - vcl(1:3,la)

    nrml(1,f) = ab(2)*ac(3) - ab(3)*ac(2)
    nrml(2,f) = ab(3)*ac(1) - ab(1)*ac(3)
    nrml(3,f) = ab(1)*ac(2) - ab(2)*ac(1)

    leng = sqrt(nrml(1,f)**2 + nrml(2,f)**2 + nrml(3,f)**2)

    if (leng > 0.0d0) then
      nrml(1,f) = nrml(1,f)/leng
      nrml(2,f) = nrml(2,f)/leng
      nrml(3,f) = nrml(3,f)/leng
    end if

    d = nrml(1,f)*vcl(1,la)+nrml(2,f)*vcl(2,la)+nrml(3,f)*vcl(3,la)

    if (abs(d) <= 1.0d0) then
      dtol = tol
    else
      dtol = tol*abs(d)
    end if

    gflag = .false.

110 continue

    if (gflag) then

      if (g > nfph) then
        cycle
      end if

      if (abs(factyp(g)) /= f) then
        cycle
      end if

      j = facep(1,g)
      l = j
      g = g + 1

    else

      gflag = .true.
      j = fvl(ccw,k)
      if (j == l) go to 110

    end if

120 continue

    lb = fvl(loc,j)
    dotp = nrml(1,f)*vcl(1,lb) + nrml(2,f)*vcl(2,lb) + &
      nrml(3,f)*vcl(3,lb)

    if (abs(dotp - d) > dtol) then
      ierr = 342
      if (msglvl >= 2) then
        write ( *,600) f,lb
      end if
    end if

    j = fvl(ccw,j)
    if (j /= l) go to 120

    go to 110

  end do

  if (ierr /= 0) return
!
!  Determine EDGA, EDGC fields and compute EANG values.  Temporarily
!  use PFL entries to record hole polygons for polyhedra.
!
  k = npf

  do i = nface+1,nface+nhole
    k = k + 1
    p = abs(facep(2,i))
    j = hfl(p)
    pfl(1,k) = i
    pfl(2,k) = pfl(2,j)
    pfl(2,j) = k
  end do

  do p = 1,npolh

    nht = 0
    i = hfl(p)

150 continue

    if (i <= npf) then
      sf = pfl(1,i)
      f = abs(sf)
      ff = f
    else
      ff = pfl(1,i)
      sf = facep(2,ff)
      f = abs(factyp(ff))
    end if

    do j = facep(1,ff),facep(1,ff+1)-1

      la = fvl(loc,j)
      lb = fvl(loc,fvl(succ,j))

      call edght ( la, lb, j, nvc, htsiz, maxedg, hdfree, last, ht, &
        iwk, k, ierr )

      if (ierr /= 0) then
        ierr = 6
        return
      end if

      if (k <= 0) then

        nht = nht + 1

      else

        nht = nht - 1
        g = fvl(facn,k)
        dotp = nrml(1,f)*nrml(1,g) + nrml(2,f)*nrml(2,g) + &
          nrml(3,f)*nrml(3,g)
        if (abs(dotp) > 1.0d0-tol) dotp = sign(1.0d0,dotp)
        fflag = (abs(facep(2,f)) == p)
        gflag = (abs(facep(2,g)) == p)

        if (fflag .neqv. gflag) then
          dotp = -dotp
        end if

        ang = pi() - acos(dotp)
!
!  Determine whether edge angle is reflex.
!
        ab(1:3) = vcl(1:3,lb) - vcl(1:3,la)
        en(1) = nrml(2,f)*ab(3) - nrml(3,f)*ab(2)
        en(2) = nrml(3,f)*ab(1) - nrml(1,f)*ab(3)
        en(3) = nrml(1,f)*ab(2) - nrml(2,f)*ab(1)

        if (fflag .neqv. (sf > 0)) then
          en(1) = -en(1)
          en(2) = -en(2)
          en(3) = -en(3)
        end if
!
!  AC = (midpoint of A and B) + EN - A
!
        do l = 1,3
          ac(l) = 0.5d0*(vcl(l,lb) - vcl(l,la)) + en(l)
        end do

        dotp = ac(1)*nrml(1,g)+ac(2)*nrml(2,g)+ac(3)*nrml(3,g)
        if (.not. gflag) dotp = -dotp
        if (dotp > 0.0d0) ang = pi2 - ang

        if ((lb - la)*sf > 0) then
          fvl(edgc,j) = k
          fvl(edga,k) = j
          eang(j) = ang
        else
          fvl(edga,j) = k
          fvl(edgc,k) = j
          eang(k) = ang
        end if

      end if

    end do

    i = pfl(2,i)
    if (i /= hfl(p)) go to 150

    if (nht /= 0) then
      ierr = 322
      return
    end if

  end do

  if (nhole > 0) then
    do p = 1,npolh
      j = hfl(p)
      pfl(2,j) = j + 1
    end do
  end if
!
!  Decompose faces containing holes into simple polygons.
!
  nfacin = nface
  g = nface + 1

210 continue

  if (g > nfph) return
  k = g
  f = abs(factyp(g))
  iwk(1) = facep(1,f)
  j = iwk(1)
  size = 0

220 continue

  size = size + 1
  j = fvl(succ,j)
  if (j /= iwk(1)) go to 220

230 continue

  size = size + (facep(1,g+1) - facep(1,g))
  g = g + 1

  if (g <= nfph) then
    if (abs(factyp(g)) == f) go to 230
  end if

  nfhol = g - k
  size = size + 8*nfhol

  if (1 + nfhol + 3*size > maxiw) then
    ierr = 6
    return
  else if (size + size > maxwk) then
    ierr = 7
    return
  end if

  do i = 2,nfhol+1
    iwk(i) = facep(1,k)
    k = k + 1
  end do

  locfv = nfhol + 2
  link = locfv + size
  edgv = link + size
  irem = edgv + size
  y = size + 1
  wrem = y + size

  call spdech(aspc2d,atol2d,nfhol,nvc,nface,nvert,npf,maxvc,nfph, &
    maxfv,maxpf,maxiw-irem+1,maxwk-wrem+1,vcl,facep,factyp,nrml, &
    fvl,eang,hfl,pfl,iwk,wk,wk(y),iwk(locfv),iwk(link), &
    iwk(edgv),iwk(irem),wk(wrem), ierr )

  if (ierr == 0) then
    go to 210
  end if

  600 format (1x,'face ',i5,' contains non-coplanar vertex ',i5)

  return
end
subroutine dsphih ( aspc2d, atol2d, angacc, rdacc, nvc, nface, nvert, &
  npolh, npf, nvch, nfach, ipolh, ifach, holint, maxvc, maxfp, maxfv, &
  maxhf, maxpf, maxiw, maxwk, vcl, facep, factyp, nrml, fvl, eang, hfl, &
  pfl, htsiz, ht, iwk, wk, ierr )
!
!******************************************************************************
!
!! DSPHIH updates the polyhedral decomposition data structure.
!
!
!  Purpose: 
!
!    Update the polyhedral decomposition data structure by
!    adding an interior polyhedron hole to one of the polyhedra,
!    where the hole polyhedron may have holes through it (genus
!    >= 0). The polyhedron containing the hole is decomposed into
!    2 simple polyhedra by joining the hole to the outer boundary
!    using a cut face; this approach is not guaranteed to work.
!    The interior hole may be a hole interface, i.e. the subregion
!    inside the hole is part of the polyhedral region.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, ASPC2D - angle spacing parameter in radians used in controlling
!    vertices to be considered as an endpoint of a separator.
!
!    Input, ATOL2D - angle tolerance parameter in radians used in accepting
!    separator to resolve a hole on a face.
!
!    Input, ANGACC - minimum acceptable dihedral angle in radians produced by
!    a cut face.
!
!    Input, RDACC - minimum acceptable relative distance between a cut
!    plane and vertices not on plane.
!
!    Input/output, NVC - number of vertex coordinates (excluding hole).
!
!    Input/output, NFACE - number of faces in decomposition (excluding hole).
!
!    Input/output, NVERT - number of positions used in FVL array 
!    (excluding hole).
!
!    Input/output, NPOLH - number of polyhedra in decomposition.
!
!    Input/output, NPF - number of positions used in PFL array (excluding hole).
!
!    Input, NVCH - number of vertex coordinates in hole polyhedron.
!
!    Input, NFACH - number of faces in hole polyhedron.
!
!    Input, IPOLH - index of polyhedron containing hole.
!
!    Input, IFACH - index of face of hole (1 <= IFACH <= NFACH) for which
!    attempt is to be made to find a cut face to join with
!    outer boundary; normal vector of cut face is same as
!    that of hole face; it is assumed that plane containing
!    hole face does not intersect any other part of hole polyhedron
!    (such a face does not exist for all polyhedra); IERR is
!    set to 346 if this cut face is not simple (excluding hole
!    face) or does not meet angle or subedge length criteria.
!
!    Input, HOLINT - .TRUE. iff hole polyhedron is a hole interface.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXFP - maximum size available for FACEP, FACTYP, NRML arrays.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, MAXHF - maximum size available for HFL array.
!
!    Input, MAXPF - maximum size available for PFL array.
!
!    Input, MAXIW - maximum size available for IWK array; should be about
!    max(4*(number of edges in hole polyh), 6*(NE+NV)), where NE
!    is the number of edges of polyhedron IPOLH intersecting plane
!    through hole face IFACH and NV is the number of vertices on hole face.
!
!    Input, MAXWK - maximum size available for WK array; should be about
!    7*(NE+NV).
!
!    [The following 8 subarrays are as output by routine DSPHDC or
!    DSPHFH, and do not include the hole polyhedron.]
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input/output, FACEP(1:3,1:NFACE) - face pointer list: row 1 is head
!    pointer, rows 2 and 3 are signed polyhedron indices.
!
!    Input/output, FACTYP(1:NFACE) - face types: useful for specifying types of
!    boundary faces; entries must be >= 0.
!
!    Input/output, NRML(1:3,1:NFACE) - unit normal vectors for faces; outward
!    normal corresponds to counter clockwise traversal of face from polyhedron
!    with index |FACEP(2,F)|.
!
!    Input/output, FVL(1:6,1:NVERT) - face vertex list.
!
!    Input/output, EANG(1:NVERT) - angles at edges common to 2 faces in a
!    polyhedron; EANG(J) corresponds to FVL(*,J), determined by EDGC field.
!
!    Input/output, HFL(1:NPOLH) - head pointer to face indices in PFL for each
!    polyhedron.
!
!    Input/output, PFL(1:2,1:NPF) - list of signed face indices for each
!    polyhedron; row 2 used for link.
!
!    [The following 5 subarrays are similar to input for DSPHDC
!    for the single hole polyhedron, treated as though the region
!    consists of the hole. Input vertex and face indices in range
!    1 to NVCH and 1 to NFACH, respectively, will be incremented
!    by this routine to avoid conflict with those of polyhedral
!    decomposition. Orientation of faces is also changed.]
!
!    Input, VCL(1:3,NVC+1:NVC+NVCH) - vertex coordinate list for hole.
!
!    Input, FACEP(1,NFACE+1:NFACE+NFACH+1) - head pointer to vertex
!    indices in FVL for each hole face; 1 = FACEP(1,NFACE+1)
!    < ... < FACEP(1,NFACE+NFACH+1); head vertex of each face
!    must be a strictly convex vertex.
!
!    Input, FACTYP(NFACE+1:NFACE+NFACH) - face types for faces of hole.
!
!    Input, FVL(1,NVERT+1:*) - vertex indices; those for Ith face of hole
!    are in FVL(1,NVERT+J) for J = FACEP(1,NFACE+I),...,
!    FACEP(1,NFACE+I+1)-1.
!
!    Input, PFL(1,NPF+1:NPF+NFACH) - signed face indices for hole polyhedron;
!    face index must be negated if ordering of vertices for
!    face in FVL is in clockwise order when viewed from outside hole.
!
!    Input, HTSIZ - size of hash table HT; should be a prime number which
!    is >= NVCH+2.
!
!    Workspace, HT(0:HTSIZ-1) - hash table used to find matching occurrences
!    of polyhedron edges by calling routine EDGHT.
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer htsiz
  integer maxfp
  integer maxfv
  integer maxhf
  integer maxiw
  integer maxpf
  integer maxvc
  integer maxwk
!
  double precision ab(3)
  double precision ac(3)
  double precision ang
  double precision angacc
  double precision aspc2d
  double precision atol2d
  integer ccw
  double precision dir(3,3)
  double precision dotp
  double precision eang(maxfv)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  double precision en(3)
  integer f
  integer facep(3,maxfp)
  integer, parameter :: facn = 2
  integer factyp(maxfp)
  logical fflag
  integer fvl(6,maxfv)
  integer g
  logical gflag
  integer headp(0:1)
  integer hdfree
  integer hfhol
  integer hfint
  integer hfl(maxhf)
  logical holint
  integer ht(0:htsiz-1)
  integer i
  integer ierr
  integer iface
  integer ifach
  integer ifhol
  integer ipolh
  integer iwk(maxiw)
  integer j
  integer k
  integer l
  integer la
  integer last
  integer lb
  integer lc
  double precision leng
  integer, parameter :: loc = 1
  integer m
  integer maxedg
  integer nface
  integer nfach
  integer nht
  integer npf
  integer npolh
  double precision nrml(3,maxfp)
  double precision nrmlc(4)
  integer nv
  integer nv2
  integer nv3
  integer nvc
  integer nvch
  integer nvert
  integer p
  integer pfl(2,maxpf)
  double precision pi
  double precision pi2
  integer, parameter :: pred = 4
  double precision pt(3)
  double precision rdacc
  logical rflag
  integer sf
  integer, parameter :: succ = 3
  integer tfhol
  integer tfint
  double precision tol
  double precision vcl(3,maxvc)
  double precision wk(maxwk)
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  pi2 = 2.0d0 * pi()
  maxedg = maxiw/4
  iface = ifach + nface
  hfhol = npf + 1
  ifhol = npf + ifach
  hdfree = 0
  last = 0
  nht = 0

  ht(0:htsiz-1) = 0

  do i = nface+1,nface+nfach+1
    facep(1,i) = facep(1,i) + nvert
  end do

  do i = nface+1,nface+nfach
    facep(2,i) = 0
    facep(3,i) = 0
    k = facep(1,i)
    l = facep(1,i+1) - 1
    do j = k,l
      fvl(loc,j) = fvl(loc,j) + nvc
      fvl(facn,j) = i
      fvl(succ,j) = j + 1
      fvl(pred,j) = j - 1
      fvl(edga,j) = 0
      fvl(edgc,j) = 0
      eang(j) = -1.0d0
    end do
    fvl(succ,l) = k
    fvl(pred,k) = l
  end do

  nvc = nvc + nvch
  npf = npf + nfach

  do j = hfhol,npf

    if (pfl(1,j) > 0) then
      f = -(pfl(1,j) + nface)
    else
      f = -pfl(1,j) + nface
    end if

    pfl(1,j) = f
    pfl(2,j) = j + 1
    p = sign(ipolh,f)
    facep(2,abs(f)) = p

  end do

  if (holint) then

    if (npf + nfach > maxpf) then
      ierr = 17
      return
    end if

    hfint = npf + 1
    tfint = npf + nfach

    do j = hfint,tfint
      pfl(1,j) = -pfl(1,j-nfach)
      pfl(2,j) = j + 1
    end do

    pfl(2,tfint) = hfint

  end if

  nface = nface + nfach
  nvert = facep(1,nface+1) - 1
  tfhol = npf

  if (ifhol == hfhol) then
    hfhol = hfhol + 1
  else if (ifhol == tfhol) then
    tfhol = tfhol - 1
  else
    pfl(2,ifhol-1) = ifhol + 1
  end if
!
!  Compute normals for each hole face from orientation in FACEP(2,*).
!
  do f = nface-nfach+1,nface

    if (facep(2,f) > 0) then
      ccw = succ
    else
      ccw = pred
    end if

    j = facep(1,f)
    lb = fvl(loc,j)
    lc = fvl(loc,fvl(ccw,j))
    la = fvl(loc,fvl(7-ccw,j))
    ab(1:3) = vcl(1:3,lb) - vcl(1:3,la)
    ac(1:3) = vcl(1:3,lc) - vcl(1:3,la)
    nrml(1,f) = ab(2)*ac(3) - ab(3)*ac(2)
    nrml(2,f) = ab(3)*ac(1) - ab(1)*ac(3)
    nrml(3,f) = ab(1)*ac(2) - ab(2)*ac(1)
    leng = sqrt(nrml(1,f)**2 + nrml(2,f)**2 + nrml(3,f)**2)

    if (leng > 0.0d0) then
      nrml(1,f) = nrml(1,f)/leng
      nrml(2,f) = nrml(2,f)/leng
      nrml(3,f) = nrml(3,f)/leng
    end if

  end do
!
!  Determine EDGA, EDGC fields + compute EANG values for hole edges.
!
  nht = 0

  do i = npf-nfach+1,npf

    sf = pfl(1,i)
    f = abs(sf)

    do j = facep(1,f),facep(1,f+1)-1

      la = fvl(loc,j)
      lb = fvl(loc,fvl(succ,j))

      call edght ( la, lb, j, nvc, htsiz, maxedg, hdfree, last, ht, &
        iwk, k, ierr )

      if (ierr /= 0) then
        ierr = 6
        return
      end if

      if (k <= 0) then

        nht = nht + 1

      else

        nht = nht - 1
        g = fvl(facn,k)
        dotp = nrml(1,f)*nrml(1,g) + nrml(2,f)*nrml(2,g) + &
        nrml(3,f)*nrml(3,g)
        if (abs(dotp) > 1.0d0 - tol) dotp = sign(1.0d0,dotp)
        fflag = (abs(facep(2,f)) == p)
        gflag = (abs(facep(2,g)) == p)
        if (fflag .neqv. gflag) dotp = -dotp
        ang = pi() - acos(dotp)
!
!  Determine whether edge angle is reflex.
!
        ab(1:3) = vcl(1:3,lb) - vcl(1:3,la)
        en(1) = nrml(2,f)*ab(3) - nrml(3,f)*ab(2)
        en(2) = nrml(3,f)*ab(1) - nrml(1,f)*ab(3)
        en(3) = nrml(1,f)*ab(2) - nrml(2,f)*ab(1)

        if (fflag .neqv. (sf > 0)) then
          en(1:3) = -en(1:3)
        end if
!
!  AC = (midpoint of A and B) + EN - A
!
        ac(1:3) = 0.5d0*(vcl(1:3,lb) - vcl(1:3,la)) + en(1:3)

        dotp = ac(1)*nrml(1,g)+ac(2)*nrml(2,g)+ac(3)*nrml(3,g)
        if (.not. gflag) dotp = -dotp
        if (dotp > 0.0d0) ang = pi2 - ang

        if ((lb - la)*sf > 0) then
          fvl(edgc,j) = k
          fvl(edga,k) = j
          eang(j) = ang
        else
          fvl(edga,j) = k
          fvl(edgc,k) = j
          eang(k) = ang
        end if

      end if

    end do

  end do

  if (nht /= 0) then
    ierr = 322
    return
  end if
!
!  Determine extreme point of hole face IFACE, and 3 directions on
!  cut plane for routine RESHOL to find starting edge of cut face.
!
  if (holint) then
    npf = tfint
  end if

  nrmlc(1:3) = -nrml(1:3,iface)

  j = 1
  if (abs(nrmlc(2)) > abs(nrmlc(1))) j = 2
  if (abs(nrmlc(3)) > abs(nrmlc(j))) j = 3
  k = j + 1
  l = j - 1
  if (k > 3) k = 1
  if (l < 0) l = 3
  nv = 1
  i = facep(1,iface)
  g = i
  m = fvl(loc,i)
  i = fvl(succ,i)

140 continue

  nv = nv + 1
  j = fvl(loc,i)

  if (vcl(k,j) > vcl(k,m) .or. vcl(k,j) == vcl(k,m) .and. &
    vcl(l,j) > vcl(l,m)) then
    g = i
    m = j
  end if

  i = fvl(succ,i)
  if (i /= facep(1,iface)) go to 140
  pt(1:3) = vcl(1:3,m)
  nrmlc(4) = nrmlc(1)*pt(1) + nrmlc(2)*pt(2) + nrmlc(3)*pt(3)
  la = fvl(loc,fvl(succ,g))
  lb = fvl(loc,fvl(pred,g))

  dir(1:3,1) = pt(1:3) - vcl(1:3,la)
  dir(1:3,2) = pt(1:3) - vcl(1:3,lb)
  dir(1:3,3) = dir(1:3,1) + dir(1:3,2)

  do j = 1,3

    leng = sqrt(dir(1,j)**2 + dir(2,j)**2 + dir(3,j)**2)
    dir(1:3,j) = dir(1:3,j)/leng

    call reshol(ipolh,nrmlc,pt,dir(1,j),angacc,rdacc,nvc,nface, &
      nvert,npolh,npf,maxvc,maxfp,maxfv,maxhf,maxpf,maxiw,maxwk, &
      vcl,facep,factyp,nrml,fvl,eang,hfl,pfl,iwk,wk,rflag, ierr )

    if (ierr /= 348) then
      exit
    end if

  end do

  if ( ierr /= 0 ) then
    return
  end if

  if (.not. rflag) then
    ierr = 346
    return
  else if (nvert + nv > maxfv) then
    ierr = 15
    return
  end if
!
!  Update PFL entries for hole polyhedron + set data structure for SPDECH.
!
  p = facep(2,iface)
  facep(2,iface) = sign(npolh,p)
  k = hfl(npolh)
  pfl(2,ifhol) = pfl(2,k)
  pfl(2,k) = ifhol
  k = hfl(ipolh)
  pfl(2,tfhol) = pfl(2,k)
  pfl(2,k) = hfhol

  if (p > 0) then
    ccw = succ
  else
    ccw = pred
  end if

  headp(0) = facep(1,nface)
  headp(1) = nvert + 1
  nvert = nvert + nv
  i = facep(1,iface)

  do j = headp(1),nvert
    fvl(loc,j) = fvl(loc,i)
    fvl(facn,j) = nface
    fvl(succ,j) = j + 1
    fvl(pred,j) = j - 1
    i = fvl(ccw,i)
  end do

  fvl(succ,nvert) = headp(1)
  fvl(pred,headp(1)) = nvert
  if (ccw == pred) i = fvl(pred,i)

  do j = headp(1),nvert

    k = fvl(edgc,i)

    if (k > 0) then
      fvl(edgc,i) = j
      fvl(edga,j) = i
      fvl(edgc,j) = k
      fvl(edga,k) = j
      eang(j) = eang(i) - pi()
      eang(i) = pi()
    else
      k = fvl(edga,i)
      fvl(edga,i) = j
      fvl(edgc,j) = i
      fvl(edga,j) = k
      fvl(edgc,k) = j
      eang(k) = eang(k) - pi()
      eang(j) = pi()
    end if

    i = fvl(ccw,i)

  end do

  i = headp(0)

220 continue

  nv = nv + 1
  i = fvl(succ,i)
  if (i /= headp(0)) go to 220
  nv = nv + 8
  nv2 = nv + nv
  nv3 = nv2 + nv

  if (nv3 > maxiw) then
    ierr = 6
    return
  else if (nv2 > maxwk) then
    ierr = 7
    return
  end if

  call spdech(aspc2d,atol2d,1,nvc,nface,nvert,npf,maxvc,maxfp,maxfv, &
    maxpf,maxiw-nv3,maxwk-nv2,vcl,facep,factyp,nrml,fvl,eang,hfl, &
    pfl,headp,wk,wk(nv+1),iwk,iwk(nv+1),iwk(nv2+1),iwk(nv3+1), &
    wk(nv2+1), ierr )

  if (.not. holint .or. ierr /= 0) then
    return
  end if
!
!  Add hole interface to data structure
!
  if (npolh >= maxhf) then
    ierr = 18
    return
  end if

  npolh = npolh + 1
  hfl(npolh) = hfint

  do i = hfint,tfint

    f = pfl(1,i)
    facep(3,abs(f)) = sign(npolh,f)
    f = abs(f)
    j = facep(1,f)

230 continue

    if (fvl(edgc,j) == 0) then

      k = fvl(edga,j)
      l = fvl(edga,k)

      if (l == 0) then
        fvl(edgc,j) = k
        fvl(edga,k) = j
        eang(j) = pi2 - eang(k)
      else
        fvl(edgc,j) = l
        fvl(edga,l) = j
        eang(j) = pi2 - (eang(k) + eang(l))
      end if

    end if

    j = fvl(succ,j)
    if (j /= facep(1,f)) go to 230

  end do

  return
end
subroutine dtrimk ( k, npt, sizht, maxbf, maxfc, vcl, vm, nbf, nfc, nbfac, &
  nface, nsmplx, bf, fc, ht, iwk, wk, ierr )
!
!******************************************************************************
!
!! DTRIMK constructs a Delaunay triangulation of points in KD.
!
!
!  Purpose: 
!
!    Construct Delaunay triangulation of K-D vertices using
!    incremental approach and implicit local transformations, i.e.
!    simplices are first all deleted, then all added at each step.
!    Vertices are inserted one at a time in order given by VM array.
!    The initial simplices created due to a new vertex are obtained
!    by a walk through the triangulation until location of vertex is known.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, integer K, the dimension of points and triangulation.
!
!    Input, NPT - number of K-D vertices (points).
!
!    Input, SIZHT - size of hash table HT; a good choice is a prime number
!    which is about 1/8 * NFACE.
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:K,1:*) - vertex coordinate list.
!
!    Input/output, VM(1:NPT).  On input, the indices of vertices of VCL 
!    being triangulated.  On output, the third to (K+1)st elements may 
!    be swapped so that first K+1 vertices are not in same hyperplane.
!
!    Output, NBF - number of positions used in BF array; NBF <= MAXBF.
!
!    Output, NFC - number of positions used in FC array; NFC <= MAXFC.
!
!    Output, NBFAC - number of boundary faces in triangulation; NBFAC <= NBF.
!
!    Output, NFACE - number of faces in triangulation; NFACE <= NFC.
!
!    Output, NSMPLX - number of simplices in triangulation.
!
!    Output, BF(1:K,1:NBF) -  array of boundary face records containing pointers
!    (indices) to FC; if FC(K+2,I) = -J < 0 and FC(1:K,I) =
!    ABC...G, then BF(1,J) points to other boundary face with
!    (K-2)-facet BC...G, BF(2,J) points to other boundary face
!    with facet AC...G, etc.; if BF(1,J) <= 0, record is not
!    used and is in avail list.
!
!    Output, FC(1:K+4,1:NFC) - array of face records which are in linked
!    lists in hash table with direct chaining. Fields are:
!
!    FC(1:K,*) - A,B,C,...,G with 1<=A<B<C<...<G<=NPT; indices
!      in VM of K vertices of face; if A<=0, record not used
!      (in linked list of avail records with indices <= NFC);
!      internal use: if B <= 0, face in queue, not in triang
!    FC(K+1:K+2,*) - D,E; indices in VM of (K+1)st vertex of 1
!      or 2 simplices with face ABC...G; if boundary face
!      then E < 0 and |E| is an index of BF array
!    FC(K+3,*) - HTLINK; pointer (index in FC) of next element
!      in linked list (or NULL = 0)
!    FC(K+4,*) - used internally for QLINK (link for queues or
!      stacks); pointer (index in FC) of next face in queue/
!      stack (or NULL = 0); QLINK = -1 indicates face is not
!      in any queue/stack, and is output value (for records
!      not in avail list), except:
!    FC(K+4,1:2) - HDAVBF,HDAVFC : head pointers of avail list in BF, FC
!
!    Output, HT(0:SIZHT-1) - hash table using direct chaining; entries are
!    head pointers of linked lists (indices of FC array)
!    containing the faces and simplices of triangulation.
!
!    Workspace, integer IWK(3*K+1).
!
!    Workspace, double precision WK(K*K+2*K+1).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer maxbf
  integer maxfc
  integer npt
  integer sizht
!
  integer alpha
  integer back
  integer bf(k,maxbf)
  logical bflag
  integer ctr
  integer fc(k+4,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer i
  integer ierr
  integer ind
  integer indf
  integer ifac
  integer ivrt
  integer iwk(3*k+1)
  integer j
  integer kp1
  integer kp2
  integer kp4
  integer l
  integer loc
  integer mat
  integer, parameter :: msglvl = 0
  integer nbf
  integer nbfac
  integer nface
  integer nfc
  integer nsmplx
  integer ptr
  double precision sum2
  integer top
  double precision vcl(k,*)
  integer vi
  integer vm(npt)
  double precision wk(k*k+2*k+1)
!
!  Find initial valid simplex, and initialize data structures.
!
  ierr = 0
  kp1 = k + 1
  kp2 = k + 2
  kp4 = k + 4
  ctr = 1
  alpha = kp1
  mat = alpha + kp1
  ind = 1
  indf = ind + k
  loc = indf + kp1
  call frsmpx(k,.false.,npt,vcl,vm,iwk,iwk(indf),wk(mat),ierr)
  if (ierr /= 0) return

  if (msglvl == 4) then
    write ( *,600) (vm(i),i=1,kp1)
    write ( *,610) (iwk(i),i=1,k-1)
  end if

  do i = 1,k
    sum2 = vcl(i,vm(1))
    do j = 2,kp1
      sum2 = sum2 + vcl(i,vm(j))
    end do
    wk(i) = sum2 / dble(kp1)
  end do

  ht(0:sizht-1) = 0
  hdavbf = 0
  hdavfc = 0
  nbf = kp1
  nfc = kp1
  nsmplx = 1

  do i = 1,kp1
    j = 0
    do l = 1,kp1
      if (l /= i) then
        j = j + 1
        iwk(j) = l
        bf(j,i) = l
      end if
    end do
    call htinsk(k,i,iwk(ind),i,-i,npt,sizht,fc,ht)
  end do

  ifac = kp1
!
!  Insert Ith vertex into Delaunay triangulation of first I-1 vertices.
!  Walk through triangulation to find location of vertex I, delete simplices
!  whose circumhypersphere contains I in interior, then add simplices
!  involving I by joining I to faces in stack.
!
  do i = kp2,npt

    vi = vm(i)

    if (fc(kp2,ifac) == i-1) then
      ivrt = kp2
    else
      ivrt = kp1
    end if

    call walktk(k,vcl(1,vi),npt,sizht,nsmplx,vcl,vm,fc,ht,ifac, &
      ivrt,iwk(ind),iwk(indf),iwk(loc),wk(alpha),wk(mat), ierr )

    if (ierr /= 0) return

    if (ivrt == 0) then

      if (msglvl == 4) then
        write ( *,620) i,vi,'out'
      end if

      bflag = .true.
      top = 0
      front = ifac
      back = ifac
      call vbfack(k,vcl(1,vi),wk(ctr),vcl,vm,bf,fc,front,0, &
        iwk(ind),wk(mat),wk(alpha))
      ptr = front

60    continue

      l = -fc(kp2,ptr)
      bf(1,l) = -hdavbf
      hdavbf = l
      fc(kp2,ptr) = i
      ptr = fc(kp4,ptr)
      if (ptr /= 0) go to 60

    else if (ivrt == 1) then

      if (msglvl == 4) then
        write ( *,620) i,vi,'vert'
      end if

      ierr = 402

    else

      if (msglvl == 4) then
        if (ivrt >= kp1) then
          write ( *,620) i,vi,'in'
        else if (ivrt == k) then
          write ( *,620) i,vi,'face'
        else if (ivrt < k) then
          write ( *,620) i,vi,'edge'
        end if
      end if

      call lfcini(k,i,ifac,ivrt,iwk(indf),npt,sizht,bf,fc,ht, &
        nsmplx,hdavbf,hdavfc,bflag,front,back,top,iwk(ind), &
        iwk(loc), ierr )

    end if

    if (ierr /= 0) return

    call smpxda(k,i,npt,sizht,nbf,nfc,maxbf,maxfc,vcl,vm,bf,fc,ht, &
      nsmplx,hdavbf,hdavfc,bflag,front,back,top,ifac,iwk(ind), &
      iwk(indf),wk(alpha),wk(mat), ierr )

    if (ierr /= 0) return

  end do

  nface = nfc
  ptr = hdavfc

  do while ( ptr /= 0 ) 
    nface = nface - 1
    ptr = -fc(1,ptr)
  end do

  nbfac = nbf
  ptr = hdavbf

  do while (ptr /= 0)
    nbfac = nbfac - 1
    ptr = -bf(1,ptr)
  end do

  fc(kp4,1) = hdavbf
  fc(kp4,2) = hdavfc

  600 format (/1x,'dtrimk: first simplex: ',7i7)
  610 format (4x,'iswap(3:k+1)=',5i7)
  620 format (/1x,'step',i7,':   vertex i =',i7,3x,a)

  return
end
subroutine dtris2 ( npt, maxst, vcl, ind, ntri, til, tnbr, stack, ierr )
!
!******************************************************************************
!
!! DTRIS2 constructs the Delaunay triangulation of vertices in 2D.
!
!
!  Purpose: 
!
!    Construct Delaunay triangulation of 2D vertices using
!    incremental approach and diagonal edge swaps.  Vertices are
!    first sorted in lexicographically increasing (x,y) order, and
!    then are inserted one at a time from outside the convex hull.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NPT - number of 2D points (vertices).
!
!    Input, MAXST - maximum size available for STACK array; should be about
!    NPT to be safe, but MAX(10,2*LOG2(NPT)) usually enough.
!
!    Input, VCL(1:2,1:*) - coordinates of 2D vertices.
!
!    Input/output, IND(1:NPT) - indices in VCL of vertices to be triangulated.
!    On output, permuted due to sorting.
!
!    Output, NTRI - number of triangles in triangulation; equal to
!    2*NPT - NB - 2 where NB = number of boundary vertices.
!
!    Output, TIL(1:3,1:NTRI) - triangle incidence list; elements are indices.
!    of VCL; vertices of triangles are in counter clockwise order.
!
!    Output, TNBR(1:3,1:NTRI) - triangle neighbor list; positive elements
!    are indices of TIL; negative elements are used for links
!    of counter clockwise linked list of boundary edges; 
!    LINK = -(3*I + J-1) where I, J = triangle, edge index; 
!    TNBR(J,I) refers to the neighbor along edge from vertex 
!    J to J+1 (mod 3).
!
!    Workspace, STACK(1:MAXST) - used for stack of triangles for which
!    circumcircle test must be made.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxst
  integer npt
!
  double precision cmax
  integer e
  integer i
  integer ierr
  integer ind(npt)
  integer j
  integer l
  integer ledg
  integer lr
  integer lrline
  integer ltri
  integer m
  integer m1
  integer m2
  integer, parameter :: msglvl = 0
  integer n
  integer ntri
  integer redg
  integer rtri
  integer stack(maxst)
  integer t
  integer til(3,npt*2)
  integer tnbr(3,npt*2)
  double precision tol
  integer top
  double precision vcl(2,*)
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
!
!  Sort vertices by increasing (x,y).
!
  call dhpsrt ( 2, npt, 2, vcl, ind )
!
!  Check that the data is not degenerate.
!
  m1 = ind(1)

  do i = 2, npt

    m = m1
    m1 = ind(i)

    do j = 1, 2
      cmax = max ( abs(vcl(j,m)), abs(vcl(j,m1)) )
      if (abs(vcl(j,m) - vcl(j,m1)) > tol*cmax .and. cmax > tol) then
        go to 20
      end if
    end do

    ierr = 224
    return

20  continue

  end do
!
!  Staring with points M1 and M2, find the first point M that is
!  "reasonably" non-colinear.
!
  m1 = ind(1)
  m2 = ind(2)
  j = 3

  do

    if ( j > npt ) then
      ierr = 225
      return
    end if

    m = ind(j)
    lr = lrline(vcl(1,m),vcl(2,m),vcl(1,m1),vcl(2,m1),vcl(1,m2), &
      vcl(2,m2),0.0d0)

    if ( lr /= 0) then
      exit
    end if

    j = j + 1

  end do
!
!  Set up the initial triangle information for M1, M2 and M (and any
!  in-between points we may have skipped over while searching for M.
!
  ntri = j - 2

  if (lr == -1) then

    til(1,1) = m1
    til(2,1) = m2
    til(3,1) = m
    tnbr(3,1) = -3

    do i = 2,ntri
      m1 = m2
      m2 = ind(i+1)
      til(1,i) = m1
      til(2,i) = m2
      til(3,i) = m
      tnbr(1,i-1) = -3*i
      tnbr(2,i-1) = i
      tnbr(3,i) = i - 1
    end do

    tnbr(1,ntri) = -3*ntri - 1
    tnbr(2,ntri) = -5
    ledg = 2
    ltri = ntri

  else

    til(1,1) = m2
    til(2,1) = m1
    til(3,1) = m
    tnbr(1,1) = -4

    do i = 2,ntri
      m1 = m2
      m2 = ind(i+1)
      til(1,i) = m2
      til(2,i) = m1
      til(3,i) = m
      tnbr(3,i-1) = i
      tnbr(1,i) = -3*i - 3
      tnbr(2,i) = i - 1
    end do

    tnbr(3,ntri) = -3*ntri
    tnbr(2,1) = -3*ntri - 2
    ledg = 2
    ltri = 1

  end if

  if (msglvl == 4) then
    m2 = ind(1)
    write ( *,'(i7,4f15.7)') 1,vcl(1,m2),vcl(2,m2),vcl(1,m),vcl(2,m)
    do i = 2,j-1
      m1 = m2
      m2 = ind(i)
      write ( *,'(i7,4f15.7)') 1,vcl(1,m1),vcl(2,m1),vcl(1,m2),vcl(2,m2)
      write ( *,'(i7,4f15.7)') 1,vcl(1,m2),vcl(2,m2),vcl(1,m),vcl(2,m)
    end do
  end if
!
!  Insert vertices one at a time from outside convex hull, determine
!  visible boundary edges, and apply diagonal edge swaps until
!  Delaunay triangulation of vertices (so far) is obtained.
!
  top = 0

  do i = j+1,npt

    if (msglvl == 4) then
      write ( *,'(i7,4f15.7)') i
    end if

    m = ind(i)
    m1 = til(ledg,ltri)

    if (ledg <= 2) then
      m2 = til(ledg+1,ltri)
    else
      m2 = til(1,ltri)
    end if

    lr = lrline(vcl(1,m),vcl(2,m),vcl(1,m1),vcl(2,m1),vcl(1,m2), &
      vcl(2,m2),0.0d0)

    if (lr > 0) then
      rtri = ltri
      redg = ledg
      ltri = 0
    else
      l = -tnbr(ledg,ltri)
      rtri = l/3
      redg = mod(l,3) + 1
    end if

    call vbedg(vcl(1,m),vcl(2,m),vcl,til,tnbr,ltri,ledg,rtri,redg)
    n = ntri + 1
    l = -tnbr(ledg,ltri)

    do

      t = l/3
      e = mod(l,3) + 1
      l = -tnbr(e,t)
      m2 = til(e,t)

      if (e <= 2) then
        m1 = til(e+1,t)
      else
        m1 = til(1,t)
      end if

      ntri = ntri + 1
      tnbr(e,t) = ntri
      til(1,ntri) = m1
      til(2,ntri) = m2
      til(3,ntri) = m
      tnbr(1,ntri) = t
      tnbr(2,ntri) = ntri - 1
      tnbr(3,ntri) = ntri + 1
      top = top + 1

      if (top > maxst) then
        ierr = 8
        return
      end if

      stack(top) = ntri

      if ( msglvl == 4 ) then
        write ( *,'(i7,4f15.7)') 1,vcl(1,m),vcl(2,m), vcl(1,m2),vcl(2,m2)
      end if

      if ( t == rtri .and. e == redg ) then
        exit
      end if

    end do

    if (msglvl == 4) then
      write ( *,'(i7,4f15.7)') 1,vcl(1,m),vcl(2,m), vcl(1,m1),vcl(2,m1)
    end if

    tnbr(ledg,ltri) = -3*n - 1
    tnbr(2,n) = -3*ntri - 2
    tnbr(3,ntri) = -l
    ltri = n
    ledg = 2
    call swapec(m,top,maxst,ltri,ledg,vcl,til,tnbr,stack, ierr )

    if (ierr /= 0) then
      return
    end if

  end do

  if ( msglvl == 4 ) then
    write ( *, '(i7,4f15.7)' ) npt+1
  end if

  return
end
subroutine dtris3 ( npt, sizht, maxbf, maxfc, vcl, vm, nbf, nfc, nface, &
  ntetra, bf, fc, ht, ierr )
!
!******************************************************************************
!
!! DTRIS3 constructs a Delaunay triangulation of vertices in 3D.
!
!
!  Purpose: 
!
!    Construct Delaunay triangulation of 3D vertices using
!    incremental approach and local transformations.  Vertices are
!    first sorted in lexicographically increasing (x,y,z) order, and
!    then are inserted one at a time from outside the convex hull.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NPT - number of 3D vertices (points).
!
!    Input, SIZHT - size of hash table HT; a good choice is a prime number
!    which is about 1/8 * NFACE (or 3/2 * NPT for random
!    points from the uniform distribution).
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input/utput, VM(1:NPT) - indices of vertices of VCL being triangulated.
!    On output, indices are permuted, so that VCL(*,VM(1)), ... ,
!    VCL(*,VM(NPT)) are in lexicographic increasing order,
!    with possible slight reordering so first 4 vertices are
!    non-coplanar.
!
!    Output, NBF - number of positions used in BF array; NBF <= MAXBF.
!
!    Output, NFC - number of positions used in FC array; NFC <= MAXFC.
!
!    Output, NFACE - number of faces in triangulation; NFACE <= NFC.
!
!    Output, NTETRA - number of tetrahedra in triangulation.
!
!    Output, BF(1:3,1:NBF) -  array of boundary face records containing pointers
!    (indices) to FC; if FC(5,I) = -J < 0 and FC(1:3,I) = ABC,
!    then BF(1,J) points to other boundary face with edge BC,
!    BF(2,J) points to other boundary face with edge AC, and
!    BF(3,J) points to other boundary face with edge AB;
!    if BF(1,J) <= 0, record is not used and is in avail list.
!
!    Output, FC(1:7,1:NFC) - array of face records which are in linked lists
!    in hash table with direct chaining. Fields are:
!    FC(1:3,*) - A,B,C with 1<=A<B<C<=NPT; indices in VM of 3
!    vertices of face; if A <= 0, record is not used (it is
!    in linked list of avail records with indices <= NFC);
!    internal use: if B <= 0, face in queue, not in triangulation.
!    FC(4:5,*) - D,E; indices in VM of 4th vertex of 1 or 2
!    tetrahedra with face ABC; if ABC is boundary face
!    then E < 0 and |E| is an index of BF array
!    FC(6,*) - HTLINK; pointer (index in FC) of next element
!    in linked list (or NULL = 0)
!    FC(7,*) - used internally for QLINK (link for queues or
!    stacks); pointer (index in FC) of next face in queue/
!    stack (or NULL = 0); QLINK = -1 indicates face is not
!    in any queue/stack, and is output value (for records
!    not in avail list), except:
!    FC(7,1:2) - HDAVBF,HDAVFC : head pointers of avail list in BF, FC.
!
!    Output, HT(0:SIZHT-1) - hash table using direct chaining; entries are
!    head pointers of linked lists (indices of FC array)
!    containing the faces and tetrahedra of triangulation.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxbf
  integer maxfc
  integer npt
  integer sizht
!
  integer a
  integer b
  integer back
  integer bf(3,maxbf)
  integer bfi
  double precision ctr(3)
  integer e
  integer fc(7,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer i
  integer i3
  integer i4
  integer ierr
  integer ip
  integer j
  integer k
  integer, parameter :: msglvl = 0
  integer nbf
  integer nface
  integer nfc
  integer ntetra
  integer op
  integer opside
  integer ptr
  integer topnv
  integer top
  integer va
  integer vb
  integer vc
  double precision vcl(3,*)
  integer vi
  integer vm(npt)
!
!  Permute elements of VM so that vertices are in lexicographic
!  order, and initialize data structures.
!
  ierr = 0

  call dhpsrt(3,npt,3,vcl,vm)

  call frstet(.true.,npt,vcl,vm,i3,i4,ierr)
  if (ierr /= 0) return

  do i = 1,3
    ctr(i) = ( vcl(i,vm(1)) + vcl(i,vm(2)) + vcl(i,vm(3)) + vcl(i,vm(4)) ) &
      / 4.0d0
  end do

  ht(0:sizht-1) = 0
  hdavbf = 0
  hdavfc = 0
  nbf = 4
  nfc = 4
  ntetra = 1
  call htins(1,1,2,3,4,-1,npt,sizht,fc,ht)
  call htins(2,1,2,4,3,-2,npt,sizht,fc,ht)
  call htins(3,1,3,4,2,-3,npt,sizht,fc,ht)
  call htins(4,2,3,4,1,-4,npt,sizht,fc,ht)
  bf(1,1) = 4
  bf(2,1) = 3
  bf(3,1) = 2
  bf(1,2) = 4
  bf(2,2) = 3
  bf(3,2) = 1
  bf(1,3) = 4
  bf(2,3) = 2
  bf(3,3) = 1
  bf(1,4) = 3
  bf(2,4) = 2
  bf(3,4) = 1

  if (msglvl == 4) then
    write ( *,600) (vm(i),i=1,4),i3,i4
  end if
!
!  Insert I-th vertex into Delaunay triangle of first I-1 vertices.
!
  do i = 5,npt

    vi = vm(i)

    if (msglvl == 4) then
      write ( *,610) i,vi
    end if

    ip = i - 1
    if (i == 5) ip = 2
    if (i == i3 + 2) ip = 3
    if (i == i4 + 1) ip = 4
!
!  Form stacks of boundary faces involving vertex IP.
!  TOP is for stack of boundary faces to be tested for visibility.
!  FRONT is for stack of boundary faces visible from vertex I.
!  TOPNV is for stack of boundary faces not visible from I.
!
    front = 0
    topnv = 0

    if (i == 5) then

      top = 4
      a = 3
      b = 2
      if (ip == 2) a = 2
      if (ip <= 3) b = 1
      fc(7,top) = a
      fc(7,a) = b
      fc(7,b) = 0

    else if (ip == i - 1) then

      top = bfi
      fc(7,bfi) = 0
      b = fc(2,bfi)
      ptr = bf(1,-fc(5,bfi))

30    continue

      if (fc(1,ptr) == b) then
        b = fc(2,ptr)
        j = 1
      else
        b = fc(1,ptr)
        j = 2
      end if

      fc(7,ptr) = top
      top = ptr
      ptr = bf(j,-fc(5,ptr))
      if (ptr /= bfi) go to 30

    else

      do k = 1,nbf

        if ( bf(1,k) <= 0 ) then
          cycle
        end if

        do e = 1,3
          ptr = bf(e,k)
          if (fc(1,ptr) == ip) then
            b = fc(2,ptr)
            j = 3
            go to 60
          else if (fc(2,ptr) == ip) then
            b = fc(1,ptr)
            j = 3
            go to 60
          else if (fc(3,ptr) == ip) then
            b = fc(1,ptr)
            j = 2
            go to 60
          end if
        end do

      end do

60    continue

      bfi = ptr
      top = bfi
      fc(7,bfi) = 0
      ptr = bf(j,-fc(5,bfi))

70    continue

      if (fc(1,ptr) == b) then
        j = 1
        if (fc(2,ptr) == ip) then
          b = fc(3,ptr)
        else
          b = fc(2,ptr)
        end if
      else if (fc(2,ptr) == b) then
        j = 2
        if (fc(1,ptr) == ip) then
          b = fc(3,ptr)
        else
          b = fc(1,ptr)
        end if
      else
        j = 3
        if (fc(1,ptr) == ip) then
          b = fc(2,ptr)
        else
          b = fc(1,ptr)
        end if
      end if

      fc(7,ptr) = top
      top = ptr
      ptr = bf(j,-fc(5,ptr))
      if (ptr /= bfi) go to 70

    end if
!
!  Find a boundary face visible from vertex I.
!
80  continue

    if (top == 0) go to 110

    ptr = top
    top = fc(7,ptr)
    va = vm(fc(1,ptr))
    vb = vm(fc(2,ptr))
    vc = vm(fc(3,ptr))
    op = opside(vcl(1,va),vcl(1,vb),vcl(1,vc),ctr,vcl(1,vi))

    if (op == 2) then
      ierr = 301
      return
    end if

    if (op == 1) then
      front = ptr
90    continue
      if (top == 0) go to 110
      ptr = top
      top = fc(7,ptr)
      fc(7,ptr) = -1
      go to 90
    else
      fc(7,ptr) = topnv
      topnv = ptr
    end if

    go to 80

110 continue

    if (front == 0) then
      ierr = 306
      return
    end if
!
!  Find remaining visible boundary faces, add new tetrahedra with
!  vertex I, apply local transformation based on empty sphere criterion.
!
    call vbfac(vcl(1,vi),ctr,vcl,vm,bf,fc,front,topnv)
    if (ierr /= 0) return

    call nwthou(i,npt,sizht,nbf,nfc,maxbf,maxfc,bf,fc,ht,ntetra, &
      hdavbf,hdavfc,front,back,bfi, ierr )
    if (ierr /= 0) return

    call swapes(.false.,i,npt,sizht,nfc,maxfc,vcl,vm,bf,fc,ht, &
      ntetra,hdavfc,front,back,j, ierr )
    if (ierr /= 0) return

  end do

  nface = nfc
  ptr = hdavfc

  do while (ptr /= 0) 
    nface = nface - 1
    ptr = -fc(1,ptr)
  end do

  fc(7,1) = hdavbf
  fc(7,2) = hdavfc

  600 format (/1x,'dtris3: first tetrahedron: ',4i7/4x,'i3, i4 =',2i7)
  610 format (/1x,'step',i7,':   vertex i =',i7)

  return
end
subroutine dtrisk ( k, npt, sizht, maxbf, maxfc, vcl, vm, nbf, nfc, nbfac, &
  nface, nsmplx, bf, fc, ht, iwk, wk, ierr )
!
!******************************************************************************
!
!! DTRISK constructs a Delaunay triangulation of vertices in KD.
!
!
!  Purpose: 
!
!    Construct Delaunay triangulation of K-D vertices using
!    incremental approach and local transformations. Vertices are
!    first sorted in lexicographically increasing order, and
!    then are inserted one at a time from outside the convex hull.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of points and triangulation.
!
!    Input, NPT - number of K-D vertices (points).
!
!    Input, SIZHT - size of hash table HT; a good choice is a prime number
!    which is about 1/8 * NFACE.
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:K,1:*) - vertex coordinate list.
!
!    Input/output, VM(1:NPT).  On input, indices of vertices of VCL being
!    triangulated.  On output, indices are permuted, so that VCL(*,VM(1)), ... ,
!    VCL(*,VM(NPT)) are in lexicographic increasing order,
!    with possible slight reordering so first K+1 vertices
!    are not in same hyperplane
!
!    Output, NBF - number of positions used in BF array; NBF <= MAXBF.
!
!    Output, NFC - number of positions used in FC array; NFC <= MAXFC.
!
!    Output, NBFAC - number of boundary faces in triangulation; NBFAC <= NBF.
!
!    Output, NFACE - number of faces in triangulation; NFACE <= NFC.
!
!    Output, NSMPLX - number of simplices in triangulation.
!
!    Output, BF(1:K,1:NBF) -  array of boundary face records containing pointers
!    (indices) to FC; if FC(K+2,I) = -J < 0 and FC(1:K,I) =
!    ABC...G, then BF(1,J) points to other boundary face with
!    (K-2)-facet BC...G, BF(2,J) points to other boundary face
!    with facet AC...G, etc.; if BF(1,J) <= 0, record is not
!    used and is in avail list.
!
!    Output, FC(1:K+4,1:NFC) - array of face records which are in linked
!    lists in hash table with direct chaining. Fields are:
!    FC(1:K,*) - A,B,C,...,G with 1<=A<B<C<...<G<=NPT; indices
!    in VM of K vertices of face; if A<=0, record not used
!    (in linked list of avail records with indices <= NFC);
!    internal use: if B <= 0, face in queue, not in triang
!    FC(K+1:K+2,*) - D,E; indices in VM of (K+1)st vertex of 1
!    or 2 simplices with face ABC...G; if boundary face
!    then E < 0 and |E| is an index of BF array
!    FC(K+3,*) - HTLINK; pointer (index in FC) of next element
!    in linked list (or NULL = 0)
!    FC(K+4,*) - used internally for QLINK (link for queues or
!    stacks); pointer (index in FC) of next face in queue/
!    stack (or NULL = 0); QLINK = -1 indicates face is not
!    in any queue/stack, and is output value (for records
!    not in avail list), except:
!    FC(K+4,1:2) - HDAVBF,HDAVFC : head pointers of avail list in BF, FC.
!
!    Output, HT(0:SIZHT-1) - hash table using direct chaining; entries are
!    head pointers of linked lists (indices of FC array)
!    containing the faces and simplices of triangulation.
!
!    Workspace, IWK(1:6*K+2).
!
!    Workspace, WK(1:K*K+2*K+1).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer maxbf
  integer maxfc
  integer npt
  integer sizht
!
  integer alpha
  integer back
  integer bf(k,maxbf)
  integer bfi
  integer bfp
  integer ctr
  integer fc(k+4,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer i
  integer ierr
  integer ind
  integer indf
  integer ip
  integer iwk(6*k+2)
  integer j
  integer jj
  integer km1
  integer kp1
  integer kp2
  integer kp4
  integer l
  integer loc
  integer mat
  integer, parameter :: msglvl = 0
  integer mv
  integer nbf
  integer nbfac
  integer nbr
  integer nface
  integer nfc
  integer nsmplx
  integer opsidk
  integer ptr
  integer s
  double precision sum2
  integer top
  integer topnv
  double precision vcl(k,*)
  integer vi
  integer vm(npt)
  double precision wk(k*k+2*k+1)
  integer zpn
!
!  Permute elements of VM so that vertices are in lexicographic
!  order, and initialize data structures.
!
  ierr = 0

  km1 = k - 1
  kp1 = k + 1
  kp2 = k + 2
  kp4 = k + 4
  ctr = 1
  alpha = kp1
  mat = alpha + kp1
  ind = k
  indf = ind + kp1
  mv = indf + kp1
  loc = mv + kp1
  zpn = loc + k

  call dhpsrt(k,npt,k,vcl,vm)

  call frsmpx(k,.true.,npt,vcl,vm,iwk,iwk(ind),wk(mat),ierr)

  if (ierr /= 0) then
    return
  end if

  if (msglvl == 4) then
    write ( *,600) (vm(i),i=1,kp1)
    write ( *,610) (iwk(i),i=1,km1)
  end if

  do i = 1,k
    sum2 = vcl(i,vm(1))
    do j = 2,kp1
      sum2 = sum2 + vcl(i,vm(j))
    end do
    wk(i) = sum2/dble(kp1)
  end do

  ht(0:sizht-1) = 0

  hdavbf = 0
  hdavfc = 0
  nbf = kp1
  nfc = kp1
  nsmplx = 1

  do i = 1,kp1
    j = 0
    do l = 1,kp1
      if (l /= i) then
        j = j + 1
        iwk(km1+j) = l
        bf(j,i) = l
      end if
    end do
    call htinsk(k,i,iwk(ind),i,-i,npt,sizht,fc,ht)
  end do

  do j = 1,km1
    iwk(j) = iwk(j) + k - j
  end do

  s = 1
!
!  Insert I-th vertex into Delaunay triangle of first I-1 vertices.
!
  do i = kp2,npt

    vi = vm(i)

    if (msglvl == 4) then
      write ( *,620) i,vi
    end if

    ip = i - 1
    if (i == kp2) ip = 2
    l = s

    do j = l,km1
      if (i == iwk(j)) then
        ip = j + 2
        s = j + 1
      end if
    end do
!
!  Form stacks of boundary faces involving vertex IP.
!  TOP is for stack of boundary faces to be tested for visibility.
!  FRONT is for stack of boundary faces visible from vertex I.
!  TOPNV is for stack of boundary faces not visible from I.
!
    if (i == kp2) then

      top = 1
      do j = 1,k
        fc(kp4,j) = j + 1
      end do
      fc(kp4,kp1) = 0

    else

      if (ip /= i - 1) then

        do l = 1,nbf

          if ( bf(1,l) <= 0 ) then
            cycle
          end if

          do j = 1,k
            ptr = bf(j,l)
            do jj = 1,k
              if (fc(jj,ptr) == ip) go to 120
            end do
          end do

        end do

120     continue

        bfi = ptr

      end if

        top = bfi
        back = bfi
        fc(kp4,back) = 0
        ptr = top

130     continue

        bfp = -fc(kp2,ptr)

        do j = 1,k
          if ( fc(j,ptr) /= ip ) then
            nbr = bf(j,bfp)
            if (fc(kp4,nbr) == -1) then
              fc(kp4,back) = nbr
              back = nbr
              fc(kp4,back) = 0
            end if
          end if
        end do

        ptr = fc(kp4,ptr)
        if (ptr /= 0) go to 130

      end if
!
!  Find a boundary face visible from vertex I.
!
      front = 0
      topnv = 0

150   continue

      if (top == 0) go to 180

      ptr = top
      top = fc(kp4,ptr)

      do j = 1,k
        iwk(km1+j) = vm(fc(j,ptr))
      end do

      if (opsidk(k,iwk(ind),vcl,.false.,wk(ctr),vcl(1,vi),wk(mat), &
        wk(alpha)) == 1) then

        front = ptr
        fc(kp4,ptr) = -1

170     continue

        if (top == 0) go to 180
        ptr = top
        top = fc(kp4,ptr)
        fc(kp4,ptr) = -1
        go to 170

      else

        fc(kp4,ptr) = topnv
        topnv = ptr

      end if

    go to 150

180 continue

    if (front == 0) then
      ierr = 406
      return
    end if
!
!  Find remaining visible boundary faces, add new simplices with
!  vertex I, apply local transformation based on empty hypersphere crit.
!
    call vbfack(k,vcl(1,vi),wk(ctr),vcl,vm,bf,fc,front,topnv, &
      iwk(ind),wk(mat),wk(alpha))

    call nwsxou(k,i,npt,sizht,nbf,nfc,maxbf,maxfc,bf,fc,ht,nsmplx, &
       hdavbf,hdavfc,front,back,bfi,iwk(ind), ierr )

    if (ierr /= 0) return

    call swaphs(k,i,npt,sizht,nbf,nfc,maxbf,maxfc,vcl,vm,bf,fc,ht, &
      nsmplx,hdavbf,hdavfc,front,back,j,l,iwk(ind),iwk(indf), &
      iwk(mv),iwk(loc),iwk(zpn),wk(alpha),wk(mat), ierr )

    if (ierr /= 0) return
    if (l /= 0) bfi = l

  end do

  nface = nfc
  ptr = hdavfc

200 continue

  if (ptr == 0) go to 210
  nface = nface - 1
  ptr = -fc(1,ptr)
  go to 200

210 continue

  nbfac = nbf
  ptr = hdavbf

220 continue

  if (ptr == 0) go to 230
   nbfac = nbfac - 1
   ptr = -bf(1,ptr)
   go to 220

230 continue

  fc(kp4,1) = hdavbf
  fc(kp4,2) = hdavfc

  600 format (/1x,'dtrisk: first simplex: ',7i7)
  610 format (4x,'ishft(3:k+1)=',5i7)
  620 format (/1x,'step',i7,':   vertex i =',i7)

  return
end
subroutine dtriw2 ( npt, maxst, vcl, ind, ntri, til, tnbr, stack, ierr )
!
!******************************************************************************
!
!! DTRIW2 constructs a Delaunay triangulation of vertices in 2D.
!
!
!  Purpose: 
!
!    Construct Delaunay triangulation of 2D vertices using
!    incremental approach and diagonal edge swaps. Vertices are
!    inserted one at a time in order given by IND array. The initial
!    triangles created due to a new vertex are obtained by a walk
!    through the triangulation until location of vertex is known.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NPT - number of 2D points (vertices).
!
!    Input, MAXST - maximum size available for STACK array; should be about
!    NPT to be safe, but MAX(10,2*LOG2(NPT)) usually enough.
!
!    Input, VCL(1:2,1:*) - coordinates of 2D vertices.
!
!    Input, IND(1:NPT) - indices in VCL of vertices to be triangulated;
!    vertices are inserted in order given by this array
!
!    Output, NTRI - number of triangles in triangulation; equal to
!    2*NPT - NB - 2 where NB = number of boundary vertices.
!
!    Output, TIL(1:3,1:NTRI) - triangle incidence list; elements are indices
!    of VCL; vertices of triangles are in counter clockwise order.
!
!    Output, TNBR(1:3,1:NTRI) - triangle neighbor list; positive elements
!    are indices of TIL; negative elements are used for links
!    of counter clockwise linked list of boundary edges; LINK = -(3*I + J-1)
!    where I, J = triangle, edge index; TNBR(J,I) refers to
!    the neighbor along edge from vertex J to J+1 (mod 3).
!
!    Workspace, STACK(1:MAXST) - used for stack of triangles for which
!    circumcircle test must be made
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxst
  integer npt
!
  double precision cmax
  integer e
  integer em1
  integer ep1
  integer i
  integer i3
  integer ierr
  integer ind(npt)
  integer j
  integer l
  integer ledg
  integer lr
  integer lrline
  integer ltri
  integer m
  integer m1
  integer m2
  integer m3
  integer, parameter :: msglvl = 0
  integer n
  integer ntri
  integer redg
  integer rtri
  integer stack(maxst)
  integer t
  integer til(3,npt*2)
  integer tnbr(3,npt*2)
  double precision tol
  integer top
  double precision vcl(2,*)
!
!  Determine initial triangle.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )

  m1 = ind(1)
  m2 = ind(2)

  do j = 1, 2
    cmax = max ( abs(vcl(j,m1)),abs(vcl(j,m2)))
    if (abs(vcl(j,m1) - vcl(j,m2)) > tol*cmax .and. cmax > tol) then
      go to 20
    end if
  end do

  ierr = 224
  return
   20 continue

  i3 = 3
   30 continue

  if (i3 > npt) then
    ierr = 225
    return
  end if

  m = ind(i3)
  lr = lrline(vcl(1,m),vcl(2,m),vcl(1,m1),vcl(2,m1),vcl(1,m2), &
     vcl(2,m2),0.0d0)

  if (lr == 0) then
    i3 = i3 + 1
    go to 30
  end if

  if (i3 /= 3) then
    ind(i3) = ind(3)
    ind(3) = m
  end if

  ntri = 1

  if (lr == -1) then
    til(1,1) = m1
    til(2,1) = m2
  else
    til(1,1) = m2
    til(2,1) = m1
  end if

  til(3,1) = m
  tnbr(1,1) = -4
  tnbr(2,1) = -5
  tnbr(3,1) = -3

  if (msglvl == 4) then
    write ( *,600) 1,vcl(1,m1),vcl(2,m1),vcl(1,m2),vcl(2,m2)
    write ( *,600) 1,vcl(1,m2),vcl(2,m2),vcl(1,m),vcl(2,m)
    write ( *,600) 1,vcl(1,m),vcl(2,m),vcl(1,m1),vcl(2,m1)
  end if
!
!  Insert vertices one at a time from anywhere, walk through
!  triangulation to determine location of new vertex, and apply
!  diagonal edge swaps until Delaunay triangulation of vertices
!  (so far) is obtained.
!
  top = 0

  do i = 4,npt

    if (msglvl == 4) then
      write ( *,600) i
    end if

    m = ind(i)
    rtri = ntri
    call walkt2(vcl(1,m),vcl(2,m),ntri,vcl,til,tnbr,rtri,redg, ierr)

    if (redg == 0) then

      m1 = til(1,rtri)
      m2 = til(2,rtri)
      m3 = til(3,rtri)
      til(3,rtri) = m

      if (tnbr(1,rtri) > 0) then
        top = 1
        stack(top) = rtri
      end if

      ntri = ntri + 1
      til(1,ntri) = m2
      til(2,ntri) = m3
      til(3,ntri) = m
      n = tnbr(2,rtri)
      tnbr(1,ntri) = n

      if (n > 0) then

        if (tnbr(1,n) == rtri) then
          tnbr(1,n) = ntri
        else if (tnbr(2,n) == rtri) then
          tnbr(2,n) = ntri
        else
          tnbr(3,n) = ntri
        end if

        top = top + 1
        stack(top) = ntri

      end if

      ntri = ntri + 1
      til(1,ntri) = m3
      til(2,ntri) = m1
      til(3,ntri) = m
      n = tnbr(3,rtri)
      tnbr(1,ntri) = n

      if (n > 0) then

        if (tnbr(1,n) == rtri) then
          tnbr(1,n) = ntri
        else if (tnbr(2,n) == rtri) then
          tnbr(2,n) = ntri
        else
          tnbr(3,n) = ntri
        end if

        top = top + 1
        stack(top) = ntri

      end if

      tnbr(2,rtri) = ntri - 1
      tnbr(3,rtri) = ntri
      tnbr(2,ntri-1) = ntri
      tnbr(3,ntri-1) = rtri
      tnbr(2,ntri) = rtri
      tnbr(3,ntri) = ntri - 1

      if (tnbr(1,ntri-1) <= 0) then

        t = rtri
        e = 1

40      continue

        if (tnbr(e,t) > 0) then

          t = tnbr(e,t)

          if (til(1,t) == m2) then
            e = 3
          else if (til(2,t) == m2) then
            e = 1
          else
            e = 2
          end if

          go to 40

        end if

        tnbr(e,t) = -3*ntri + 3

      end if

      if (tnbr(1,ntri) <= 0) then

        t = ntri - 1
        e = 1

50      continue

        if (tnbr(e,t) > 0) then

          t = tnbr(e,t)

          if (til(1,t) == m3) then
            e = 3
          else if (til(2,t) == m3) then
            e = 1
          else
            e = 2
          end if

          go to 50

        end if

        tnbr(e,t) = -3*ntri

      end if

      if (msglvl == 4) then
        write ( *,600) 1,vcl(1,m),vcl(2,m),vcl(1,m1),vcl(2,m1)
        write ( *,600) 1,vcl(1,m),vcl(2,m),vcl(1,m2),vcl(2,m2)
        write ( *,600) 1,vcl(1,m),vcl(2,m),vcl(1,m3),vcl(2,m3)
      end if

    else if (redg < 0) then

      redg = -redg
      ltri = 0

      call vbedg(vcl(1,m),vcl(2,m),vcl,til,tnbr,ltri,ledg,rtri, &
        redg)

      n = ntri + 1
      l = -tnbr(ledg,ltri)

60    continue

      t = l/3
      e = mod(l,3) + 1
      l = -tnbr(e,t)
      m2 = til(e,t)

      if (e <= 2) then
        m1 = til(e+1,t)
      else
        m1 = til(1,t)
      end if

      ntri = ntri + 1
      tnbr(e,t) = ntri
      til(1,ntri) = m1
      til(2,ntri) = m2
      til(3,ntri) = m
      tnbr(1,ntri) = t
      tnbr(2,ntri) = ntri - 1
      tnbr(3,ntri) = ntri + 1
      top = top + 1

      if (top > maxst) then
        ierr = 8
        go to 100
      end if

      stack(top) = ntri

      if (msglvl == 4) then
        write ( *,600) 1,vcl(1,m),vcl(2,m),vcl(1,m2),vcl(2,m2)
      end if

      if (t /= rtri .or. e /= redg) go to 60

      if (msglvl == 4) then
        write ( *,600) 1,vcl(1,m),vcl(2,m), vcl(1,m1),vcl(2,m1)
      end if

      tnbr(ledg,ltri) = -3*n - 1
      tnbr(2,n) = -3*ntri - 2
      tnbr(3,ntri) = -l

    else if (redg <= 3) then

      m1 = til(redg,rtri)

      if (redg == 1) then
        e = 2
        ep1 = 3
      else if (redg == 2) then
        e = 3
        ep1 = 1
      else
        e = 1
        ep1 = 2
      end if

      m2 = til(e,rtri)
      til(e,rtri) = m
      m3 = til(ep1,rtri)

      if (tnbr(ep1,rtri) > 0) then
        top = 1
        stack(top) = rtri
      end if

      ntri = ntri + 1
      til(1,ntri) = m
      til(2,ntri) = m2
      til(3,ntri) = m3
      n = tnbr(e,rtri)
      tnbr(2,ntri) = n
      tnbr(3,ntri) = rtri
      tnbr(e,rtri) = ntri

      if (n > 0) then

        if (tnbr(1,n) == rtri) then
          tnbr(1,n) = ntri
        else if (tnbr(2,n) == rtri) then
          tnbr(2,n) = ntri
        else
          tnbr(3,n) = ntri
        end if

        top = top + 1
        stack(top) = ntri

      end if

      if (msglvl == 4) then
        write ( *,600) 1,vcl(1,m),vcl(2,m),vcl(1,m3),vcl(2,m3)
      end if

      ltri = tnbr(redg,rtri)

      if (ltri <= 0) then

        tnbr(1,ntri) = ltri
        tnbr(redg,rtri) = -3*ntri
        if (tnbr(2,ntri) <= 0) tnbr(1,ntri) = -3*ntri - 1

      else

        tnbr(1,ntri) = ntri + 1
        tnbr(redg,rtri) = ltri

        if (til(1,ltri) == m2) then
          ledg = 1
          em1 = 2
          e = 3
        else if (til(2,ltri) == m2) then
          ledg = 2
          em1 = 3
          e = 1
        else
          ledg = 3
          em1 = 1
          e = 2
        end if

        til(ledg,ltri) = m
        m3 = til(e,ltri)

        if (tnbr(em1,ltri) > 0) then
          top = top + 1
          stack(top) = ltri
        end if

        ntri = ntri + 1
        til(1,ntri) = m2
        til(2,ntri) = m
        til(3,ntri) = m3
        tnbr(1,ntri) = ntri - 1
        tnbr(2,ntri) = ltri
        n = tnbr(e,ltri)
        tnbr(3,ntri) = n
        tnbr(e,ltri) = ntri

        if (n > 0) then

          if (tnbr(1,n) == ltri) then
            tnbr(1,n) = ntri
          else if (tnbr(2,n) == ltri) then
            tnbr(2,n) = ntri
          else
            tnbr(3,n) = ntri
          end if

          top = top + 1
          stack(top) = ntri

        end if

        if (msglvl == 4) then
          write ( *,600) 1,vcl(1,m),vcl(2,m), vcl(1,m3),vcl(2,m3)
        end if

        if (tnbr(2,ntri-1) <= 0) then

          t = ntri
          e = 3

70        continue

          if (tnbr(e,t) > 0) then

            t = tnbr(e,t)

            if (til(1,t) == m2) then
              e = 3
            else if (til(2,t) == m2) then
              e = 1
            else
              e = 2
            end if

            go to 70

          end if

          tnbr(e,t) = -3*ntri + 2

        end if

        if (tnbr(3,ntri) <= 0) then

          t = ltri

          if (ledg <= 2) then
            e = ledg + 1
          else
            e = 1
          end if
 
80        continue

          if (tnbr(e,t) > 0) then

            t = tnbr(e,t)

            if (til(1,t) == m3) then
              e = 3
            else if (til(2,t) == m3) then
              e = 1
            else
              e = 2
            end if

            go to 80

          end if

          tnbr(e,t) = -3*ntri - 2

        end if

      end if

    else
      ierr = 224
      go to 100
    end if

    call swapec(m,top,maxst,0,0,vcl,til,tnbr,stack, ierr )

    if (ierr /= 0) then
      exit
    end if

  end do

100 continue

  if (i3 /= 3) then
    t = ind(i3)
    ind(i3) = ind(3)
    ind(3) = t
  end if

  if (msglvl == 4) then
    write ( *,600) npt+1
  end if

  600 format (1x,i7,4f15.7)

  return
end
subroutine dtriw3 ( npt, sizht, maxbf, maxfc, vcl, vm, nbf, nfc, nface, &
  ntetra, bf, fc, ht, ierr )
!
!******************************************************************************
!
!! DTRIW3 constructs a Delaunay triangulation of vertices in 3D.
!
!
!  Purpose: 
!
!    Construct Delaunay triangulation of 3D vertices using
!    incremental approach and local transformations. Vertices are
!    inserted one at a time in order given by VM array. The initial
!    tetrahedra created due to a new vertex are obtained by a walk
!    through the triangulation until location of vertex is known.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NPT - number of 3D vertices (points).
!
!    Input, SIZHT - size of hash table HT; a good choice is a prime number
!    which is about 1/8 * NFACE (or 3/2 * NPT for random
!    points from the uniform distribution).
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input/output, VM(1:NPT) - indices of vertices of VCL being triangulated;
!    vertices are inserted in order given by VM, except that on output,
!    the third and fourth elements may be swapped so
!    that first 4 vertices are non-coplanar.
!
!    Output, NBF - number of positions used in BF array; NBF <= MAXBF.
!
!    Output, NFC - number of positions used in FC array; NFC <= MAXFC.
!
!    Output, NFACE - number of faces in triangulation; NFACE <= NFC.
!
!    Output, NTETRA - number of tetrahedra in triangulation.
!
!    Output, BF(1:3,1:NBF) -  array of boundary face records containing pointers
!    (indices) to FC; if FC(5,I) = -J < 0 and FC(1:3,I) = ABC,
!    then BF(1,J) points to other boundary face with edge BC,
!    BF(2,J) points to other boundary face with edge AC, and
!    BF(3,J) points to other boundary face with edge AB;
!    if BF(1,J) <= 0, record is not used and is in avail list.
!
!    Output, FC(1:7,1:NFC) - array of face records which are in linked lists
!    in hash table with direct chaining. Fields are:
!    FC(1:3,*) - A,B,C with 1<=A<B<C<=NPT; indices in VM of 3
!    vertices of face; if A <= 0, record is not used (it is
!    in linked list of avail records with indices <= NFC);
!    internal use: if B <= 0, face in queue, not in triang
!    FC(4:5,*) - D,E; indices in VM of 4th vertex of 1 or 2
!    tetrahedra with face ABC; if ABC is boundary face
!    then E < 0 and |E| is an index of BF array
!    FC(6,*) - HTLINK; pointer (index in FC) of next element
!    in linked list (or NULL = 0)
!    FC(7,*) - used internally for QLINK (link for queues or
!    stacks); pointer (index in FC) of next face in queue/
!    stack (or NULL = 0); QLINK = -1 indicates face is not
!    in any queue/stack, and is output value (for records
!    not in avail list), except:
!    FC(7,1:2) - HDAVBF,HDAVFC : head pointers of avail list in BF, FC.
!
!    Output, HT(0:SIZHT-1) - hash table using direct chaining; entries are
!    head pointers of linked lists (indices of FC array)
!    containing the faces and tetrahedra of triangulation
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxbf
  integer maxfc
  integer npt
  integer sizht
!
  integer back
  integer bf(3,maxbf)
  double precision ctr(3)
  integer fc(7,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer i
  integer i3
  integer i4
  integer ierr
  integer ifac
  integer ind
  integer ivrt
  integer, parameter :: msglvl = 0
  integer nbf
  integer nface
  integer nfc
  integer ntetra
  integer ptr
  double precision vcl(3,*)
  integer vi
  integer vm(npt)
!
!  Find initial valid tetrahedron, and initialize data structures.
!
  ierr = 0

  call frstet(.false.,npt,vcl,vm,i3,i4,ierr)
  if (ierr /= 0) return

  do i = 1,3
    ctr(i) = ( vcl(i,vm(1)) + vcl(i,vm(2)) + vcl(i,vm(3)) + &
      vcl(i,vm(4)) )/4.0d0
  end do

  ht(0:sizht-1) = 0
  hdavbf = 0
  hdavfc = 0
  nbf = 4
  nfc = 4
  ntetra = 1
  call htins(1,1,2,3,4,-1,npt,sizht,fc,ht)
  call htins(2,1,2,4,3,-2,npt,sizht,fc,ht)
  call htins(3,1,3,4,2,-3,npt,sizht,fc,ht)
  call htins(4,2,3,4,1,-4,npt,sizht,fc,ht)
  bf(1,1) = 4
  bf(2,1) = 3
  bf(3,1) = 2
  bf(1,2) = 4
  bf(2,2) = 3
  bf(3,2) = 1
  bf(1,3) = 4
  bf(2,3) = 2
  bf(3,3) = 1
  bf(1,4) = 3
  bf(2,4) = 2
  bf(3,4) = 1
  ifac = 4

  if (msglvl == 4) then
    write ( *,600) (vm(i),i=1,4),i3,i4
  end if
!
!  Insert Ith vertex into Delaunay triang of first I-1 vertices.
!  Walk through triang to find location of vertex I, create new
!  tetrahedra, apply local transf based on empty sphere criterion.
!
  do i = 5,npt

    vi = vm(i)

    if (msglvl == 4) then
      write ( *,610) i,vi
    end if

    if (fc(5,ifac) == i-1) then
      ivrt = 5
    else
      ivrt = 4
    end if

    call walkt3(vcl(1,vi),npt,sizht,ntetra,vcl,vm,fc,ht,ifac,ivrt, ierr )
    if (ierr /= 0) return

    if (ivrt == 6) then
 !print*,"tall1"
      call nwthfc(i,ifac,npt,sizht,nbf,nfc,maxbf,maxfc,bf,fc,ht, &
        ntetra,hdavbf,hdavfc,front,back, ierr )

    else if (ivrt >= 4) then
 !print*,"tall2"
      call nwthin(i,ifac,ivrt,npt,sizht,nfc,maxfc,fc,ht,ntetra, &
        hdavfc,front,back, ierr )

    else if (ivrt == 0) then
 !print*,"tall3"
      front = ifac
      call vbfac(vcl(1,vi),ctr,vcl,vm,bf,fc,front,0)

      if (ierr /= 0) then
        return
      end if
 !print*,"tall4"
      call nwthou(i,npt,sizht,nbf,nfc,maxbf,maxfc,bf,fc,ht,ntetra, &
        hdavbf,hdavfc,front,back,ind, ierr )

    else if (ivrt >= 1) then
 !print*,"tall5"
      call nwthed(i,ifac,ivrt,npt,sizht,nbf,nfc,maxbf,maxfc,bf,fc, &
        ht,ntetra,hdavbf,hdavfc,front,back, ierr )
 !print*,"tall6"
    else

      ierr = 302

    end if

    if (ierr /= 0) return

    call swapes(.false.,i,npt,sizht,nfc,maxfc,vcl,vm,bf,fc,ht, &
      ntetra,hdavfc,front,back,ind, ierr )

    if (ierr /= 0) return

    if (ind /= 0) ifac = ind

  end do

  nface = nfc
  ptr = hdavfc

  do while (ptr /= 0)
    nface = nface - 1
    ptr = -fc(1,ptr)
  end do

  fc(7,1) = hdavbf
  fc(7,2) = hdavfc

  600 format (/1x,'dtriw3: first tetrahedron: ',4i7/4x,'i3, i4 =',2i7)
  610 format (/1x,'step',i7,':   vertex i =',i7)

  return
end
subroutine dtriwk ( k, npt, sizht, maxbf, maxfc, vcl, vm, nbf, nfc, nbfac, &
  nface, nsmplx, bf, fc, ht, iwk, wk, ierr )
!
!******************************************************************************
!
!! DTRIWK constructs a Delaunay triangulation of vertices in KD.
!
!
!  Purpose: 
!
!    Construct Delaunay triangulation of K-D vertices using
!    incremental approach and local transformations. Vertices are
!    inserted one at a time in order given by VM array. The initial
!    simplices created due to a new vertex are obtained by a walk
!    through the triangulation until location of vertex is known.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of points and triangulation.
!
!    Input, NPT - number of K-D vertices (points).
!
!    Input, SIZHT - size of hash table HT; a good choice is a prime number
!    which is about 1/8 * NFACE.
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:K,1:*) - vertex coordinate list.
!
!    Input/output, VM(1:NPT).  On input, indices of vertices of VCL being
!    triangulated.  On output, the third to (K+1)st elements may be swapped so
!    that first K+1 vertices are not in same hyperplane.
!
!    Output, NBF - number of positions used in BF array; NBF <= MAXBF.
!
!    Output, NFC - number of positions used in FC array; NFC <= MAXFC.
!
!    Output, NBFAC - number of boundary faces in triangulation; NBFAC <= NBF.
!
!    Output, NFACE - number of faces in triangulation; NFACE <= NFC.
!
!    Output, NSMPLX - number of simplices in triangulation.
!
!    Output, BF(1:K,1:NBF) -  array of boundary face records containing pointers
!    (indices) to FC; if FC(K+2,I) = -J < 0 and FC(1:K,I) =
!    ABC...G, then BF(1,J) points to other boundary face with
!    (K-2)-facet BC...G, BF(2,J) points to other boundary face
!    with facet AC...G, etc.; if BF(1,J) <= 0, record is not
!    used and is in avail list.
!
!    Output, FC(1:K+4,1:NFC) - array of face records which are in linked
!    lists in hash table with direct chaining. Fields are:
!    FC(1:K,*) - A,B,C,...,G with 1<=A<B<C<...<G<=NPT; indices
!    in VM of K vertices of face; if A<=0, record not used
!    (in linked list of avail records with indices <= NFC);
!    internal use: if B <= 0, face in queue, not in triang
!    FC(K+1:K+2,*) - D,E; indices in VM of (K+1)st vertex of 1
!    or 2 simplices with face ABC...G; if boundary face
!    then E < 0 and |E| is an index of BF array
!    FC(K+3,*) - HTLINK; pointer (index in FC) of next element
!    in linked list (or NULL = 0)
!    FC(K+4,*) - used internally for QLINK (link for queues or
!    stacks); pointer (index in FC) of next face in queue/
!    stack (or NULL = 0); QLINK = -1 indicates face is not
!    in any queue/stack, and is output value (for records
!    not in avail list), except:
!    FC(K+4,1:2) - HDAVBF,HDAVFC : head pointers of avail list in BF, FC.
!
!    Output, HT(0:SIZHT-1) - hash table using direct chaining; entries are
!    head pointers of linked lists (indices of FC array)
!    containing the faces and simplices of triangulation.
!
!    Workspace, IWK(1:5*K+3).
!
!    Workspace, WK(1:K*K+2*K+1).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer maxbf
  integer maxfc
  integer npt
  integer sizht
!
  integer alpha
  integer back
  integer bf(k,maxbf)
  integer ctr
  integer fc(k+4,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer i
  integer ierr
  integer ind
  integer indf
  integer ifac
  integer ivrt
  integer iwk(5*k+3)
  integer j
  integer kp1
  integer kp2
  integer kp4
  integer l
  integer loc
  integer mat
  integer, parameter :: msglvl = 0
  integer mv
  integer nbf
  integer nbfac
  integer nface
  integer nfc
  integer nsmplx
  integer ptr
  double precision sum2
  double precision vcl(k,*)
  integer vi
  integer vm(npt)
  double precision wk(k*k+2*k+1)
  integer zpn
!
!  Find initial valid simplex, and initialize data structures.
!
  ierr = 0

  kp1 = k + 1
  kp2 = k + 2
  kp4 = k + 4
  ctr = 1
  alpha = kp1
  mat = alpha + kp1
  ind = 1
  indf = ind + kp1
  mv = indf + kp1
  loc = mv + kp1
  zpn = loc + k

  call frsmpx(k,.false.,npt,vcl,vm,iwk,iwk(indf),wk(mat),ierr)
  if (ierr /= 0) return

  if (msglvl == 4) then
    write ( *,600) (vm(i),i=1,kp1)
    write ( *,610) (iwk(i),i=1,k-1)
  end if

  do i = 1,k
    sum2 = vcl(i,vm(1))
    do j = 2,kp1
      sum2 = sum2 + vcl(i,vm(j))
    end do
    wk(i) = sum2/dble(kp1)
  end do

  ht(0:sizht-1) = 0
  hdavbf = 0
  hdavfc = 0
  nbf = kp1
  nfc = kp1
  nsmplx = 1

  do i = 1,kp1
    j = 0
    do l = 1,kp1
      if (l /= i) then
        j = j + 1
        iwk(j) = l
        bf(j,i) = l
      end if
    end do
    call htinsk(k,i,iwk(ind),i,-i,npt,sizht,fc,ht)
  end do

  ifac = kp1
!
!  Insert I-th vertex into Delaunay triang of first I-1 vertices.
!  Walk through triang to find location of vertex I, create new
!  simplices, apply local transf based on empty hypersphere crit.
!
  do i = kp2,npt

    vi = vm(i)

    if (fc(kp2,ifac) == i-1) then
      ivrt = kp2
    else
      ivrt = kp1
    end if

    call walktk(k,vcl(1,vi),npt,sizht,nsmplx,vcl,vm,fc,ht,ifac, &
      ivrt,iwk(ind),iwk(indf),iwk(mv),wk(alpha),wk(mat), ierr )

    if (ierr /= 0) return

    if (ivrt == 0) then

      if (msglvl == 4) then
        write ( *,620) i,vi,'out'
      end if

      front = ifac
      call vbfack(k,vcl(1,vi),wk(ctr),vcl,vm,bf,fc,front,0, &
        iwk(ind),wk(mat),wk(alpha))

      call nwsxou(k,i,npt,sizht,nbf,nfc,maxbf,maxfc,bf,fc,ht, &
        nsmplx,hdavbf,hdavfc,front,back,j,iwk(ind), ierr)

    else if (ivrt >= kp1) then

      if (msglvl == 4) then
        write ( *,620) i,vi,'in'
      end if

      call nwsxin(k,i,ifac,ivrt,npt,sizht,nfc,maxfc,fc,ht,nsmplx, &
        hdavfc,front,back,iwk(ind), ierr )

    else if (ivrt == k) then

      if (msglvl == 4) then
        write ( *,620) i,vi,'face'
      end if

      call nwsxfc(k,i,ifac,npt,sizht,nbf,nfc,maxbf,maxfc,bf,fc,ht, &
        nsmplx,hdavbf,hdavfc,front,back,iwk(ind), ierr )

    else if (ivrt == 1) then

      if (msglvl == 4) then
        write ( *,620) i,vi,'vert'
      end if

      ierr = 402

    else

      if (msglvl == 4) then
        write ( *,620) i,vi,'edge'
      end if

      call nwsxed(k,i,ifac,ivrt,iwk(indf),npt,sizht,nbf,nfc,maxbf, &
        maxfc,bf,fc,ht,nsmplx,hdavbf,hdavfc,front,back,iwk(ind), &
        iwk(mv), ierr )

    end if

    if (ierr /= 0) return

    call swaphs(k,i,npt,sizht,nbf,nfc,maxbf,maxfc,vcl,vm,bf,fc,ht, &
      nsmplx,hdavbf,hdavfc,front,back,l,j,iwk(ind),iwk(indf), &
      iwk(mv),iwk(loc),iwk(zpn),wk(alpha),wk(mat), ierr )

    if (ierr /= 0) return

    if (l /= 0) then
      ifac = l
    end if

  end do

  nface = nfc
  ptr = hdavfc

  do while ( ptr /= 0 )
    nface = nface - 1
    ptr = -fc(1,ptr)
  end do

  nbfac = nbf
  ptr = hdavbf

  do while ( ptr /= 0 )
    nbfac = nbfac - 1
    ptr = -bf(1,ptr)
  end do

  fc(kp4,1) = hdavbf
  fc(kp4,2) = hdavfc

  600 format (/1x,'dtriwk: first simplex: ',7i7)
  610 format (4x,'iswap(3:k+1)=',5i7)
  620 format (/1x,'step',i7,':   vertex i =',i7,3x,a)

  return
end
subroutine edght ( a, b, v, n, htsiz, maxedg, hdfree, last, ht, edge, w, ierr )
!
!******************************************************************************
!
!! EDGHT searches a hash table for an edge record.
!
!
!  Purpose: 
!
!    Search in hash table HT for record in EDGE containing
!    key (A,B).
!
!  Discussion:
!
!    Before first call to this routine, HDFREE, LAST, and
!    entries of HT should be set to 0.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A, B - vertex indices, > 0, of edge (also key of hash table).
!
!    Input, V - value associated with edge.
!
!    Input, N - upper bound on A, B.
!
!    Input, HTSIZ - size of hash table HT.
!
!    Input, MAXEDG - maximum size available for EDGE array.
!
!    Input/output, HDFREE - head pointer to linked list of free entries of EDGE
!    array due to deletions.
!
!    Input/output, LAST - index of last entry used in EDGE array.
!
!    Input/output, HT(0:HTSIZ-1) - hash table of head pointers (direct chaining
!    with ordered lists is used).  If key with A,B is found then this record 
!    is deleted from hash table, else record is inserted in hash table
!
!    Input/output, EDGE(1:4,1:MAXEDG) - entries of hash table records;
!    EDGE(1,I) = MIN(A,B); EDGE(2,I) = MAX(A,B);
!    EDGE(3,I) = V; EDGE(4,I) = link.
!    If key with A,B is found then this record is deleted
!    from hash table, else record is inserted in hash table
!
!    Output, W - EDGE(3,INDEX), where INDEX is index of record, if found;
!    else 0.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer htsiz
  integer maxedg
!
  integer a
  integer aa
  integer b
  integer bb
  integer bptr
  integer edge(4,maxedg)
  integer hdfree
  integer ht(0:htsiz-1)
  integer ierr
  integer k
  integer last
  integer n
  integer newp
  integer ptr
  integer v
  integer w
!
  ierr = 0

  if (a < b) then
    aa = a
    bb = b
  else
    aa = b
    bb = a
  end if

  k = mod(aa*n + bb, htsiz)
  bptr = -1
  ptr = ht(k)

10 continue

  do while (ptr /= 0) 

    if (edge(1,ptr) > aa) then

      exit

    else if (edge(1,ptr) == aa) then

      if (edge(2,ptr) > bb) then

        exit

      else if (edge(2,ptr) == bb) then

        if (bptr == -1) then
          ht(k) = edge(4,ptr)
        else
          edge(4,bptr) = edge(4,ptr)
        end if

        edge(4,ptr) = hdfree
        hdfree = ptr
        w = edge(3,ptr)
        return

      end if

    end if

    bptr = ptr
    ptr = edge(4,ptr)

  end do

  if (hdfree > 0) then
    newp = hdfree
    hdfree = edge(4,hdfree)
  else
    last = last + 1
    newp = last
    if (last > maxedg) then
      ierr = 1
      return
    end if
  end if

  if (bptr == -1) then
    ht(k) = newp
  else
    edge(4,bptr) = newp
  end if

  edge(1,newp) = aa
  edge(2,newp) = bb
  edge(3,newp) = v
  edge(4,newp) = ptr
  w = 0

  return
end
function emnrth ( a, b, c, d )
!
!******************************************************************************
!
!! EMNRTH computes the mean ratio of a tetrahedron.
!
!
!  Purpose: 
!
!    Compute (eigenvalue) mean ratio of tetrahedron
!    = 12*(3*volume)**(2/3)/(sum of square of edge lengths)
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A(1:3),B(1:3),C(1:3),D(1:3) - 4 vertices of tetrahedron.
!
!    Output, EMNRTH - mean ratio of tetrahedron.
!
  implicit none
!
  double precision a(3)
  double precision ab(3)
  double precision ac(3)
  double precision ad(3)
  double precision b(3)
  double precision bc(3)
  double precision bd(3)
  double precision c(3)
  double precision cd(3)
  double precision d(3)
  double precision denom
  double precision emnrth
  integer i
  double precision lab
  double precision lac
  double precision lad
  double precision lbc
  double precision lbd
  double precision lcd
  double precision vol
!
  ab(1:3) = b(1:3) - a(1:3)
  ac(1:3) = c(1:3) - a(1:3)
  ad(1:3) = d(1:3) - a(1:3)
  bc(1:3) = c(1:3) - b(1:3)
  bd(1:3) = d(1:3) - b(1:3)
  cd(1:3) = d(1:3) - c(1:3)

  lab = ab(1)**2 + ab(2)**2 + ab(3)**2
  lac = ac(1)**2 + ac(2)**2 + ac(3)**2
  lad = ad(1)**2 + ad(2)**2 + ad(3)**2
  lbc = bc(1)**2 + bc(2)**2 + bc(3)**2
  lbd = bd(1)**2 + bd(2)**2 + bd(3)**2
  lcd = cd(1)**2 + cd(2)**2 + cd(3)**2
  vol = abs(ab(1)*(ac(2)*ad(3) - ac(3)*ad(2)) + ab(2)*(ac(3)*ad(1) &
    - ac(1)*ad(3)) + ab(3)*(ac(1)*ad(2) - ac(2)*ad(1)))
  denom = lab + lac + lad + lbc + lbd + lcd

  if (denom == 0.0d0) then
    emnrth = 0.0d0
  else
    emnrth = 12.0d0*(0.5d0*vol)**(2.0d0/3.0d0)/denom
  end if

  return
end
subroutine eqdis2 ( hflag, umdf, kappa, angspc, angtol, dmin, nmin, ntrid, &
  nvc, npolg, nvert, maxvc, maxhv, maxpv, maxiw, maxwk, vcl, regnum, hvl, &
  pvl, iang, area, psi, h, iwk, wk, ierr )
!
!******************************************************************************
!
!! EQDIS2 subdivides convex polygons for equidistribution.
!
!
!  Purpose: 
!
!    Further subdivide convex polygons so that an approximate
!    equidistributing triangular mesh can be constructed with
!    respect to heuristic or user-supplied mesh distribution
!    function, and determine triangle size for each polygon of
!    decomposition.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, HFLAG - .TRUE. if heuristic mdf, .FALSE. if user-supplied mdf.
!
!    Input, UMDF(X,Y) - d.p user-supplied mdf with d.p arguments.
!
!    Input, KAPPA - mesh smoothness parameter in interval [0.0,1.0].
!
!    Input, ANGSPC - angle spacing parameter in radians used to determine
!    extra points as possible endpoints of separators.
!
!    Input, ANGTOL - angle tolerance parameter in radians used in
!    accepting separators.
!
!    Input, DMIN - parameter used to determine if variation of mdf in
!    polygon is 'sufficiently high'.
!
!    Input, NMIN - parameter used to determine if 'sufficiently large'
!    number of triangles in polygon.
!
!    Input, NTRID - desired number of triangles in mesh.
!
!    Input/output, NVC - number of vertex coordinates or positions used in VCL
!    array.
!
!    Input/output, NPOLG - number of polygonal subregions or positions used in
!    HVL array.
!
!    Input/output, NVERT - number of polygon vertices or positions used in PVL
!    array.
!
!    Input, MAXVC - maximum size available for VCL array, should be >=
!    number of vertex coordinates required for decomposition
!    (approximately NVC + 2*NS where NS is expected number of new
!    separators).
!
!    Input, MAXHV - maximum size available for HVL, REGNUM, AREA, PSI, H
!    arrays; should be >= number of polygons required for
!    decomposition (approximately NPOLG + NS).
!
!    Input, MAXPV - maximum size available for PVL, IANG arrays; should be
!    >= number of polygon vertices required for decomposition
!    (approximately NVERT + 5*NS).
!
!    Input, MAXIW - maximum size available for IWK array; should be >=
!    MAX(2*NP, NVERT + NPOLG + 3*NVRT + INT(2*PI/ANGSPC))
!    where NVRT is maximum number of vertices in a convex
!    polygon of the (input) decomposition, NP is expected
!    value of NPOLG on output.
!
!    Input, MAXWK - maximum size available for WK array; should be >=
!    NVC + NVERT + 2*NPOLG + 3*(NVRT + INT(2*PI/ANGSPC)).
!
!    Input/output, VCL(1:2,1:NVC) - vertex coordinate list.
!
!    Input/output, REGNUM(1:NPOLG) - region numbers.
!
!    Input/output, HVL(1:NPOLG) - head vertex list.
!
!    Input/output, PVL(1:4,1:NVERT), IANG(1:NVERT) - polygon vertex list and
!    interior angles; see routine DSPGDC for more details.
!
!    [Note: The data structures should be as output from routine CVDEC2.]
!
!    Output, AREA(1:NPOLG) - area of convex polygons in decomposition.
!
!    Output, PSI(1:NPOLG) - smoothed mean mdf values in the convex polygons.
!
!    Output, H(1:NPOLG) - triangle size for convex polygons
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxhv
  integer maxiw
  integer maxpv
  integer maxvc
  integer maxwk
!
  double precision angspc
  double precision angtol
  double precision area(maxhv)
  double precision dmin
  integer edgval
  double precision h(maxhv)
  logical hflag
  integer hvl(maxhv)
  double precision iang(maxpv)
  integer ierr
  integer ivrt
  integer iwk(maxiw)
  double precision kappa
  integer m
  integer n
  integer nmin
  integer npolg
  integer ntrid
  integer nvc
  integer nvert
  double precision psi(maxhv)
  integer pvl(4,maxpv)
  integer regnum(maxhv)
  double precision, external :: umdf
  double precision vcl(2,maxvc)
  integer vrtval
  integer widsq
  double precision wk(maxwk)
  integer xivrt
!
  ierr = 0
  ivrt = 1
  xivrt = ivrt + nvert
  m = xivrt + npolg

  if (m > maxiw) then
    ierr = 6
    return
  end if

  widsq = 1

  if (hflag) then
    edgval = widsq + npolg
    vrtval = edgval + nvert
    n = npolg + nvert + nvc
    if (n > maxwk) then
      ierr = 7
      return
    end if
  else
    edgval = 1
    vrtval = 1
    n = 0
  end if

  call dsmdf2(hflag,nvc,npolg,maxwk-n,vcl,hvl,pvl,iang,iwk(ivrt), &
    iwk(xivrt),wk(widsq),wk(edgval),wk(vrtval),area,wk(n+1),ierr)

  if (ierr /= 0) return

  call mfdec2(hflag,umdf,kappa,angspc,angtol,dmin,nmin,ntrid,nvc, &
    npolg,nvert,maxvc,maxhv,maxpv,maxiw-m,maxwk-n,vcl,regnum,hvl, &
    pvl,iang,iwk(ivrt),iwk(xivrt),wk(widsq),wk(edgval),wk(vrtval), &
    area,psi,iwk(m+1),wk(n+1), ierr )

  if (ierr /= 0) then
    return
  end if

  if (2*npolg > maxiw) then
    ierr = 6
    return
  end if

  call trisiz(ntrid,npolg,hvl,pvl,area,psi,h,iwk,iwk(npolg+1))

  return
end
subroutine eqdis3 ( hflag, umdf, kappa, angacc, angedg, dmin, nmin, ntetd, &
  nsflag, nvc, nface, nvert, npolh, npf, maxvc, maxfp, maxfv, maxhf, maxpf, &
  maxiw, maxwk, vcl, facep, factyp, nrml, fvl, eang, hfl, pfl, vol, psi, h, &
  iwk, wk, ierr )
!
!******************************************************************************
!
!! EQDIS3 subdivides polyhedra for equidistribution.
!
!
!  Purpose: 
!
!    Further subdivide convex polyhedra so that an approximate
!    equidistributing tetrahedral mesh can be constructed with
!    respect to heuristic or user-supplied mesh distribution
!    function, and determine tetrahedron size for each polyhedron
!    of decomposition.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, HFLAG - .TRUE. if heuristic mdf, .FALSE. if user-supplied mdf.
!
!    Input, UMDF(X,Y,Z) - d.p user-supplied mdf with d.p arguments.
!
!    Input, KAPPA - mesh smoothness parameter in interval [0.0,1.0], used
!    iff HFLAG is .TRUE.
!
!    Input, ANGACC - minimum acceptable dihedral angle in radians produced by
!    cut faces.
!
!    Input, ANGEDG - angle parameter in radians used to determine allowable
!    points on edges as possible endpoints of edges of cut faces.
!
!    Input, DMIN - parameter used to determine if variation of mdf in
!    polyhedron is 'sufficiently high'.
!
!    Input, NMIN - parameter used to determine if 'sufficiently large'
!    number of tetrahedra in polyhedron.
!
!    Input, NTETD - desired number of tetrahedra in mesh.
!
!    Input, NSFLAG - .TRUE. if continue to next polyhedron when no separator
!    face is found for a polyhedron, .FALSE. if terminate with
!    error 336 when no separator face is found for a polyhedron.
!
!    Input/output, NVC - number of vertex coordinates or positions used in VCL.
!
!    Input/output, NFACE - number of faces or positions used in FACEP array.
!
!    Input/output, NVERT - number of positions used in FVL, EANG arrays.
!
!    Input/output, NPOLH - number of polyhedra or positions used in HFL array.
!
!    Input/output, NPF - number of positions used in PFL array.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXFP - maximum size available for FACEP, FACTYP, NRML arrays.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, MAXHF - maximum size available for HFL array.
!
!    Input, MAXPF - maximum size available for PFL array.
!
!    Input, MAXIW - maximum size available for IWK array; should be >=
!    MAX(S*(NVERT+NFACE+NPF+NPOLH+2 + 2*NCVC+5*NCEDGE+NCFACE)
!    + 2*(NE + MAX(NF,NV)), 2*NPOLHout) where
!    S = 1 or 0 if HFLAG is TRUE or FALSE, NVERT to NPOLH are
!    input values, NCVC = max no. of vertices in a polyhedron (of
!    input decomposition), NCEDGE = max no. of edges in a
!    polyhedron, NCFACE = max number of faces in a polyh, NE,NF,NV
!    are max number of edges, faces, vertices in any polyhedron
!    of updated decomposition, NPOLHout is output value of NPOLH.
!
!    Input, MAXWK - maximum size available for WK array; should be >=
!    S*(NPOLH+NFACE+NVERT+NVC + 4*NCFACE+4*NCEDGE) +
!    MAX(NPOLH, NE+MAX(2*NF,3*NV)) where NVC is input value.
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input/output, FACEP(1:3,1:NFACE) - face pointer list: row 1 is head
!    pointer, rows 2 and 3 are signed polyhedron indices.
!
!    Input/output, FACTYP(1:NFACE) - face types: useful for specifying types of
!    boundary faces; entries must be >= 0; any new interior
!    faces (not part of previous face) has face type set to 0.
!
!    Input/output, NRML(1:3,1:NFACE) - unit normal vectors for faces; outward
!    normal corresponds to counter clockwise traversal of face 
!    from polyhedron with index |FACEP(2,F)|.
!
!    Input/output, FVL(1:6,1:NVERT) - face vertex list; see routine DSPHDC.
!
!    Input/output, EANG(1:NVERT) - angles at edges common to 2 faces in a
!    polyhedron; EANG(J) corresponds to FVL(*,J), determined by EDGC field.
!
!    Input/output, HFL(1:NPOLH) - head pointer to face indices in PFL for each
!    polyhedron.
!
!    Input/output, PFL(1:2,1:NPF) - list of signed face indices for each
!    polyhedron; row 2 used for link.
!
!    Output, VOL(1:NPOLH) - volume of convex polyhedra in decomposition.
!
!    Output, PSI(1:NPOLH) - mean mdf values in the convex polyhedra.
!
!    Output, H(1:NPOLH) - tetrahedron size for convex polyhedra.
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfp
  integer maxfv
  integer maxhf
  integer maxiw
  integer maxpf
  integer maxvc
  integer maxwk
!
  double precision angacc
  double precision angedg
  double precision dmin
  double precision eang(maxfv)
  integer edge
  integer edgval
  integer facep(3,maxfp)
  integer factyp(maxfp)
  integer facval
  integer fvl(6,maxfv)
  double precision h(maxhf)
  integer hfl(maxhf)
  logical hflag
  integer ht
  integer htsiz
  integer ierr
  integer ifac
  integer infoev
  integer ivrt
  integer iwk(maxiw)
  double precision kappa
  integer listev
  integer m
  integer n
  integer ncedge
  integer ncface
  integer ncvc
  integer nface
  integer nmin
  integer npf
  integer npolh
  double precision nrml(3,maxfp)
  logical nsflag
  integer ntetd
  integer nvc
  integer nvert
  integer pfl(2,maxpf)
  integer prime
  double precision psi(maxhf)
  double precision, external :: umdf
  double precision vcl(3,maxvc)
  double precision vol(maxhf)
  integer vrtval
  integer wid
  double precision wk(maxwk)
  integer xifac
  integer xivrt
!
  ierr = 0
  ivrt = 1
  wid = 1

  if (hflag) then

    xivrt = ivrt + nvert
    ifac = xivrt + nface + 1
    xifac = ifac + npf
    m = xifac + npolh
    facval = wid + npolh
    edgval = facval + nface
    vrtval = edgval + nvert
    n = vrtval + nvc - 1

    if (m > maxiw) then
      ierr = 6
      return
    else if (n > maxwk) then
      ierr = 7
      return
    end if

    call dsmdf3(nvc,nface,nvert,npolh,maxiw-m,maxwk-n,vcl,facep, &
      nrml,fvl,eang,hfl,pfl,iwk(ivrt),iwk(xivrt),iwk(ifac), &
      iwk(xifac),wk(wid),wk(facval),wk(edgval),wk(vrtval),ncface, &
      ncedge,iwk(m+1),wk(n+1),ierr)

    if (ierr /= 0) return
    ncvc = ncedge - 2
    htsiz = prime(ncvc)
    ht = m + 1
    edge = ht + htsiz
    listev = edge + 4*ncedge
    m = listev + ncface + ncedge + ncvc - 1
    infoev = n + 1

    n = infoev + 4*(ncface + ncedge) - 1

    if (m > maxiw) then
      ierr = 6
      return
    else if (n > maxwk) then
      ierr = 7
      return
    end if

  else

    xivrt = 1
    ifac = 1
    xifac = 1
    facval = 1
    edgval = 1
    vrtval = 1
    htsiz = 1
    ncedge = 1
    ht = 1
    edge = 1
    listev = 1
    infoev = 1
    m = 0
    n = 0

  end if

  call mfdec3(hflag,umdf,kappa,angacc,angedg,dmin,nmin,ntetd,nsflag, &
    nvc,nface,nvert,npolh,npf,maxvc,maxfp,maxfv,maxhf,maxpf, &
    maxiw-m,maxwk-n,vcl,facep,factyp,nrml,fvl,eang,hfl,pfl, &
    iwk(ivrt),iwk(xivrt),iwk(ifac),iwk(xifac),wk(wid),wk(facval), &
    wk(edgval),wk(vrtval),vol,psi,htsiz,ncedge,iwk(ht),iwk(edge), &
    iwk(listev),wk(infoev),iwk(m+1),wk(n+1), ierr )

  if (ierr /= 0) return

  if (npolh + npolh > maxiw) then
    ierr = 6
    return
  end if

  call tetsiz(ntetd,npolh,facep,hfl,pfl,vol,psi,h,iwk,iwk(npolh+1))

  return
end
subroutine fndmsw ( crit, npt, sizht, vcl, vm, fc, ht, a, b, d, e, f, minbef, &
  top, top2, impr, ierr )
!
!******************************************************************************
!
!! FNDMSW finds local transformation that improve a 3D triangulation.
!
!
!  Purpose: 
!
!    Find a sequence of >= 3 local transformations to improve
!    3D triangulation.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, CRIT - criterion code; 1 for (local max-min) solid angle
!    criterion, 2 for radius ratio criterion, 3 for mean ratio
!    criterion, 0 (or anything else) for no swaps.
!
!    Input, NPT - number of 3D vertices (points).
!
!    Input, SIZHT - size of hash table HT.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:NPT) - indices of vertices of VCL being triangulated.
!
!    Input/output, FC(1:7,1:*) - array of face records; see routine DTRIS3.
!    On output, some faces may be added to the list.
!
!    Input, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, A,B,D,E,F - vertices (local indices) in configuration 
!    with T23 face AFB|DE swapped out to produce only 1 tetra AFDE with
!    measure <= MINBEF; try to apply a T32 swap to remove AFDE
!    where AF is the desired edge to be removed.
!
!    Input, MINBEF - (min tetra measure of an existing tetra of swap) + TOL.
!
!    Input, TOP - pointer to list of 2 or 3 faces to be possibly swapped.
!
!    Input/output, TOP2.  On input, pointer to stack of other faces of T32 
!    or T44 swaps.  On output, is set to zero, and stack is emptied.
!
!    Output, TOP - pointer to list of faces to be swapped if IMPR is
!    .TRUE., else TOP = 0 and all faces removed from list.
!
!    Output, IMPR - .TRUE. iff improvement is possible.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer npt
  integer sizht
!
  integer a
  integer aa
  double precision alpha(4)
  integer b
  integer bb
  integer c2
  integer cc
  integer crit
  integer d
  logical degen
  integer e
  integer f
  integer fc(7,*)
  integer g
  integer h
  integer ht(0:sizht-1)
  integer htsrc
  integer i
  integer ierr
  logical impr
  integer ind
  integer ind1
  integer ind2
  integer indx
  integer indy
  integer j
  integer kf
  integer kneg
  integer kzero
  integer, parameter :: maxtf = 13
  double precision minbef
  double precision nu1
  double precision nu2
  double precision nu3
  double precision nu4
  double precision nu5
  integer ptr
  double precision tetmu
  integer top
  integer top2
  character ( len = 3 ) typ2
  character ( len = 3 ) type
  integer va
  double precision vcl(3,*)
  integer vd
  integer ve
  integer vf
  integer vg
  integer vh
  integer vi
  integer vm(npt)
!
  ierr = 0
  kf = 2
  impr = .false.
  ptr = top

10 continue

  if (kf >= maxtf) go to 40
  indx = htsrc(a,f,d,npt,sizht,fc,ht)

  if (indx <= 0) then
    ierr = 300
    return
  end if

  if (fc(4,indx) == b) then
    g = fc(5,indx)
  else
    g = fc(4,indx)
  end if

  ind = htsrc(a,f,e,npt,sizht,fc,ht)

  if (ind <= 0) then
    ierr = 300
    return
  end if

  if (fc(4,ind) == b) then
    h = fc(5,ind)
  else
    h = fc(4,ind)
  end if

  if (g <= 0 .or. h <= 0) go to 40

  if (g /= h) then

    ind1 = htsrc(a,f,h,npt,sizht,fc,ht)

    if (ind1 <= 0) then
      ierr = 300
      return
    end if

    if (fc(4,ind1) /= g .and. fc(5,ind1) /= g) go to 40

    call ifacty(ind1,npt,sizht,vcl,vm,fc,ht,type,aa,bb,cc,ierr)

    if (type /= 't23') then

      ind2 = ind1
      c2 = cc
      typ2 = type
      ind1 = htsrc(a,f,g,npt,sizht,fc,ht)

      if (ind1 <= 0) then
        ierr = 300
        return
      end if

      call ifacty(ind1,npt,sizht,vcl,vm,fc,ht,type,aa,bb,cc,ierr)

      if (type == 't23' .or. type == 'n32') then
        call i_swap ( d, e )
        call i_swap ( g, h )
      else if (typ2 == 'n32') then
        type = typ2
        ind1 = ind2
        cc = c2
      else
        go to 40
      end if

    end if

  end if

  va = vm(a)
  vd = vm(d)
  ve = vm(e)
  vf = vm(f)
  vg = vm(g)

  call baryth(vcl(1,va),vcl(1,vf),vcl(1,vd),vcl(1,ve),vcl(1,vg), &
    alpha,degen)

  if (degen) then
    ierr = 301
    return
  else if (alpha(4) > 0.0d0) then
    ierr = 309
    return
  end if

  kneg = 1
  kzero = 0

  do j = 1,3
    if (alpha(j) < 0.0d0) then
      kneg = kneg + 1
    else if (alpha(j) == 0.0d0) then
      kzero = kzero + 1
    end if
  end do

  if (kneg /= 2 .or. kzero /= 0 .or. alpha(3) >= 0.0d0) then
    go to 40
  end if

  if (fc(7,ind) /= -1 .or. fc(7,indx) /= -1) go to 40
  indy = htsrc(a,f,g,npt,sizht,fc,ht)

  if (indy <= 0) then
    ierr = 300
    return
  end if

  if (fc(7,indy) /= -1) go to 40
  nu1 = tetmu(crit,vcl(1,va),vcl(1,vd),vcl(1,ve),vcl(1,vg),alpha)
  nu2 = tetmu(crit,vcl(1,vf),vcl(1,vd),vcl(1,ve),vcl(1,vg),alpha)
  if (min(nu1,nu2) <= minbef) go to 40
  fc(7,ind) = fc(7,ptr)
  fc(7,ptr) = ind
  kf = kf + 1
!
!  Last face added to middle of list has type T32.
!
  if (g == h) then
    impr = .true.
    go to 50
  end if

  fc(7,indx) = indy
  fc(7,indy) = top2
  top2 = indx
  if (type == 'n32') go to 30
  if (fc(7,ind1) /= -1) go to 40
  vh = vm(h)
  nu3 = tetmu(crit,vcl(1,va),vcl(1,vh),vcl(1,ve),vcl(1,vg),alpha)
  nu4 = tetmu(crit,vcl(1,vf),vcl(1,vh),vcl(1,ve),vcl(1,vg),alpha)
  if (max(nu3,nu4) <= minbef) go to 40
  fc(7,ind1) = fc(7,ptr)
  fc(7,ptr) = ind1
  ptr = ind1
  kf = kf + 1
!
!  Last face added to middle of list has type T23.
!
  if (min(nu3,nu4) > minbef) then
    impr = .true.
    go to 50
  end if

  if (nu4 < nu3) then
    call i_swap ( a, f )
  end if

  b = f
  d = g
  f = h
  go to 10

30 continue

  if (cc == a) then
    call i_swap ( a, f )
    call i_swap ( va, vf )
  end if

  indx = htsrc(a,h,e,npt,sizht,fc,ht)

  if (indx <= 0) then
    ierr = 300
    return
  end if

  if (fc(7,indx) /= -1) go to 40

  if (fc(4,indx) == f) then
    i = fc(5,indx)
  else
    i = fc(4,indx)
  end if

  ind2 = htsrc(a,h,i,npt,sizht,fc,ht)

  if (ind2 <= 0) then
    ierr = 300
    return
  end if

  if (fc(4,ind2) /= g .and. fc(5,ind2) /= g) go to 40
  call ifacty(ind2,npt,sizht,vcl,vm,fc,ht,type,aa,bb,cc,ierr)
  if (type /= 't23') go to 40

  vh = vm(h)
  vi = vm(i)
  nu3 = tetmu(crit,vcl(1,ve),vcl(1,vf),vcl(1,vg),vcl(1,vh),alpha)
  if (nu3 <= minbef) go to 40
  nu4 = tetmu(crit,vcl(1,va),vcl(1,vi),vcl(1,ve),vcl(1,vg),alpha)
  nu5 = tetmu(crit,vcl(1,vh),vcl(1,vi),vcl(1,ve),vcl(1,vg),alpha)
  if (max(nu4,nu5) <= minbef) go to 40
  if (fc(7,ind1) /= -1 .or. fc(7,ind2) /= -1) go to 40
  indy = htsrc(a,h,g,npt,sizht,fc,ht)

  if (indy <= 0) then
    ierr = 300
    return
  end if

  if (fc(7,indy) /= -1) go to 40

  fc(7,ind1) = fc(7,ptr)
  fc(7,ptr) = ind2
  fc(7,ind2) = ind1
  ptr = ind2
  kf = kf + 2
!
!  Last 2 faces added to middle of list have type T23, T32.
!
  if (min(nu4,nu5) > minbef) then
    impr = .true.
    go to 50
  end if

  fc(7,indx) = indy
  fc(7,indy) = top2
  top2 = indx

  if (nu5 < nu4) then
    call i_swap ( a, h )
  end if

  b = h
  d = g
  f = i
  go to 10

40 continue

  ptr = top
  top = fc(7,ptr)
  fc(7,ptr) = -1
  if (top /= 0) go to 40

50 continue

  ptr = top2
  top2 = fc(7,ptr)
  fc(7,ptr) = -1
  if (top2 /= 0) go to 50

  return
end
subroutine fndsep ( angac1, xr, yr, nvrt, xc, yc, ivis, theta, nv, iv,  &
  vcl, pvl, iang, angsep, i1, i2, wkang )
!
!*******************************************************************************
!
!! FNDSEP finds separators to resolve a reflex vertex.
!
!
!  Purpose:
!
!    Find 1 or 2 separators which can resolve a reflex vertex
!    (XR,YR) using a max-min angle criterion from list of vertices
!    in increasing polar angle with respect to the reflex vertex.
!
!    Preference is given to 1 separator.
!
!  Modified:
!
!    12 July 1999
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, double precision ANGAC1, the angle tolerance parameter used
!    for preference in accepting one separator.
!
!    Input, double precision XR, YR, the coordinates of reflex vertex.
!
!    Input, integer NVRT, (number of vertices) - 1.
!
!    Input, double precision XC(0:NVRT), YC(0:NVRT), the vertex coordinates
!    of possible endpoints of a separator.
!
!    Input, integer IVIS(0:NVRT), contains information about the vertices of
!    XC, YC arrays with respect to the polygon vertex list; if
!    IVIS(I) > 0 then vertex (XC(I),YC(I)) has index IVIS(I)
!    in PVL; if IVIS(I) < 0 then vertex (XC(I),YC(I)) is on
!    the edge joining vertices with indices -IVIS(I) and
!    SUCC(-IVIS(I)) in PVL.
!
!    Input, double precision THETA(0:NVRT), the polar angles of vertices
!    in increasing order; THETA(NVRT) is the interior angle of reflex vertex;
!    THETA(I), I >= 0, is the polar angle of (XC(I),YC(I))
!    with respect to reflex vertex.
!
!    Input, integer NV, (number of vertices to be considered as endpoint of a
!    separator) - 1.
!
!    Input, integer IV(0:NV), the indices of vertices in XC, YC arrays to be
!    considered as endpoint of a separator; angle between
!    consecutive vertices is assumed to be < 180 degrees.
!
!    Input, double precision VCL(1:2,1:*), the vertex coordinate list.
!
!    Input, integer PVL(1:4,1:*), double precision IANG(1:*), the polygon
!    vertex list, interior angles.
!
!    Output, double precision ANGSEP, the minimum of the 4 or 7 angles at the
!    boundary resulting from 1 or 2 separators, respectively.
!
!    Output, integer I1, I2, the indices of endpoints of separators in XC,
!    YC arrays; I2 = -1 if there is only one separator, else I1 < I2.
!
!    Workspace, double precision WKANG(0:NV).
!
  implicit none
!
  double precision ang
  double precision angac1
  double precision angsep
  double precision angsp2
  integer i
  integer i1
  integer i2
  double precision iang(*)
  integer ii
  integer k
  integer l
  integer m
  integer nl
  integer nr
  integer nv
  integer nvrt
  integer iv(0:nv)
  integer ivis(0:nvrt)
  double precision minang
  integer p
  double precision phi
  double precision pi
  integer pvl(4,*)
  integer q
  integer r
  double precision theta(0:nvrt)
  double precision tol
  double precision vcl(2,*)
  double precision wkang(0:nv)
  double precision xc(0:nvrt)
  double precision xr
  double precision yc(0:nvrt)
  double precision yr
!
  tol = 100.0D+00 * epsilon ( tol )
!
!  Determine the vertices in the inner cone - indices P to Q.
!
  i = 0
  p = -1
  phi = theta(nvrt) - pi() + tol

  do while ( p < 0 )

    if ( theta(iv(i)) >= phi ) then
      p = i
    else
      i = i + 1
    end if

  end do

  i = nv
  q = -1
  phi = pi() - tol

  do while ( q < 0 )

    if ( theta(iv(i)) <= phi ) then
      q = i
    else
      i = i - 1
    end if

  end do
!
!  Use the max-min angle criterion to find the best separator
!  in inner cone.
!
  angsep = 0.0

  do i = p, q

    k = iv(i)
    ang = minang ( xr, yr, xc(k), yc(k), ivis(k), theta(k), theta(nvrt), &
      vcl, pvl, iang )

    if ( ang > angsep ) then
      angsep = ang
      ii = iv(i)
    end if

  end do

  angsp2 = angsep
  if ( angsep >= angac1 ) then
    go to 110
  end if
!
!  If the best separator in inner cone is not 'good' enough,
!  use max-min angle criterion to try to find a better pair
!  of separators from the right and left cones.
!
  nr = 0
  nl = 0

  do r = 0, p-1

    wkang(r) = 0.0D+00

    if ( theta(iv(r)) > angsep ) then

      k = iv(r)

      ang = minang ( xr, yr, xc(k), yc(k), ivis(k), theta(k), theta(nvrt), &
        vcl, pvl, iang )

      if ( ang > angsep ) then
        nr = nr + 1
        wkang(r) = ang
      end if

    end if

  end do

  if ( nr == 0 ) then
    go to 110
  end if

  phi = theta(nvrt) - angsep

  do l = q+1, nv

    wkang(l) = 0.0D+00

    if ( theta(iv(l)) < phi ) then

      k = iv(l)
      ang = minang ( xr, yr, xc(k), yc(k), ivis(k), theta(k), theta(nvrt), &
        vcl, pvl, iang )

      if ( ang > angsep ) then
        nl = nl + 1
        wkang(l) = ang
      end if

    end if

  end do

  if ( nl == 0 ) then
    go to 110
  end if
!
!  Check all possible pairs for the best pair of separators
!  in the right and left cones.
!
  m = nv

  do r = p-1, 0, -1

     if ( m > q .and. wkang(r) > angsp2 ) then

      phi = theta(iv(r))

80    continue

      if ( m > q .and. ( wkang(m) <= angsp2 .or. &
        theta(iv(m)) - phi > pi() - tol) ) then
        m = m - 1
        go to 80
      end if

      do l = q+1, m

         if ( wkang(l) > angsp2 ) then

            ang = min ( theta(iv(l)) - phi, wkang(r), wkang(l) )

            if ( ang > angsp2 ) then
             angsp2 = ang
             i1 = iv(r)
             i2 = iv(l)
            end if

         end if

       end do

     end if

  end do
!
!  Choose 1 or 2 separators based on max-min angle criterion or
!  ANGAC1 parameter.
!
  110 continue

  if ( angsp2 <= angsep ) then
    i1 = ii
    i2 = -1
  else
    angsep = angsp2
  end if

  return
end
subroutine fndspf ( angac1, xr, yr, nvrt, xc, yc, ivis, theta, nv, iv, x, y, &
  iang, link, angsep, i1, i2, wkang )
!
!******************************************************************************
!
!! FNDSPF finds separators to resolve a reflex vertex.
!
!
!  Purpose: 
!
!    Find 1 or 2 separators which can resolve reflex vertex
!    (XR,YR) using a max-min angle criterion from list of vertices
!    in increasing polar angle with respect to reflex vertex.  Preference
!    is given to 1 separator.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, ANGAC1 - angle tolerance parameter used for preference
!    in accepting one separator.
!
!    Input, XR, YR - coordinates of reflex vertex.
!
!    Input, NVRT - (number of vertices) - 1,
!
!    Input, XC(0:NVRT), YC(0:NVRT) - vertex coordinates of possible
!    endpoints of a separator.
!
!    Input, IVIS(0:NVRT) - contains information about the vertices of
!    XC, YC arrays with respect to X, Y arrays; IVIS(I) = J > 0 if
!    (XC(I),YC(I)) = (X(J),Y(J)), and IVIS(I) = J < 0 if
!    (XC(I),YC(I)) lies in interior of edge starting at
!    (X(-J),Y(-J)).
!
!    Input, THETA(0:NVRT) - polar angles of vertices in increasing order;
!    THETA(NVRT) is the interior angle of reflex vertex;
!    THETA(I), I >= 0, is the polar angle of (XC(I),YC(I))
!    with respect to reflex vertex.
!
!    Input, NV - (number of vertices to be considered as endpoint of a
!    separator) - 1.
!
!    Input, IV(0:NV) - indices of vertices in XC, YC arrays to be
!    considered as endpoint of a separator; angle between
!    consecutive vertices is assumed to be < 180 degrees.
!
!    Input, X(1:*),Y(1:*),IANG(1:*),LINK(1:*) - data structure for simple
!    polygonal region containing reflex vertex; arrays are
!    for x- and y-coordinates, interior angle, counter clockwise link.
!
!    Output, ANGSEP - minimum of the 4 or 7 angles at the boundary
!    resulting from 1 or 2 separators, respectively.
!
!    Output, I1, I2 - indices of endpoints of separators in XC, YC arrays;
!    I2 = -1 if there is only one separator, else I1 < I2.
!
!    Workspace, WKANG(0:NV) - working array for angles.
!
  implicit none
!
  integer nv
  integer nvrt
!
  double precision ang
  double precision angac1
  double precision angle
  double precision angmin
  double precision angsep
  double precision angsp2
  double precision beta
  integer i
  integer i1
  integer i2
  double precision iang(*)
  integer ii
  integer ind
  integer iv(0:nv)
  integer ivis(0:nvrt)
  integer j
  integer k
  integer l
  integer link(*)
  integer m
  integer nl
  integer nr
  integer p
  double precision phi
  double precision pi
  double precision pimtol
  integer q
  integer r
  double precision theta(0:nvrt)
  double precision thetar
  double precision tol
  double precision wkang(0:nv)
  double precision x(*)
  double precision xc(0:nvrt)
  double precision xr
  double precision y(*)
  double precision yc(0:nvrt)
  double precision yr
!
!  Determine the vertices in the inner cone - indices P to Q.
!
  tol = 100.0D+00 * epsilon ( tol )
  i = 0
  p = -1
  thetar = theta(nvrt)
  phi = thetar - pi() + tol

  do while ( p < 0 )

    if (theta(iv(i)) >= phi) then
      p = i
    else
      i = i + 1
    end if

  end do

  i = nv
  q = -1
  phi = pi() - tol

  do while ( q < 0 )

    if (theta(iv(i)) <= phi) then
      q = i
    else
      i = i - 1
    end if

  end do
!
!  Use the max-min angle criterion to find the best separator
!  in inner cone.
!
  angsep = 0.0d0

  do i = p,q

    k = iv(i)
    ind = ivis(k)

    if (ind > 0) then
      j = link(ind)
      ang = iang(ind)
    else
      j = link(-ind)
      ang = pi()
    end if

    beta = angle(xr,yr,xc(k),yc(k),x(j),y(j))
    angmin = min(theta(k), thetar - theta(k), ang - beta, beta)

    if (angmin > angsep) then
      angsep = angmin
      ii = iv(i)
    end if

  end do

  angsp2 = angsep

  if (angsep >= angac1) go to 110
!
!  If the best separator in inner cone is not 'good' enough,
!  use max-min angle criterion to try to find a better pair
!  of separators from the right and left cones.
!
  nr = 0
  nl = 0

  do r = 0,p-1

    wkang(r) = 0.0d0

    if (theta(iv(r)) > angsep) then

      k = iv(r)
      ind = ivis(k)

      if (ind > 0) then
        j = link(ind)
        ang = iang(ind)
      else
        j = link(-ind)
        ang = pi()
      end if

      beta = angle(xr,yr,xc(k),yc(k),x(j),y(j))
      angmin = min(theta(k), ang - beta, beta)

      if (angmin > angsep) then
        nr = nr + 1
        wkang(r) = angmin
      end if

    end if

  end do

  if (nr == 0) go to 110

  phi = thetar - angsep

  do l = q+1,nv

    wkang(l) = 0.0d0

    if (theta(iv(l)) < phi) then

      k = iv(l)
      ind = ivis(k)

      if (ind > 0) then
        j = link(ind)
        ang = iang(ind)
      else
        j = link(-ind)
        ang = pi()
      end if

      beta = angle(xr,yr,xc(k),yc(k),x(j),y(j))
      angmin = min(thetar - theta(k), ang - beta, beta)

      if (angmin > angsep) then
        nl = nl + 1
        wkang(l) = angmin
      end if

    end if

  end do

  if (nl == 0) go to 110
!
!  Check all possible pairs for the best pair of separators
!  in the right and left cones.
!
  m = nv
  pimtol = pi() - tol

  do r = p-1,0,-1

    if (m > q .and. wkang(r) > angsp2) then

      phi = theta(iv(r))

80    continue

      if ( m > q .and. &
        (wkang(m) <= angsp2 .or. theta(iv(m)) - phi > pimtol)) then
        m = m - 1
        go to 80
      end if

      do l = q+1,m
        if (wkang(l) > angsp2) then
          ang = min(theta(iv(l)) - phi, wkang(r), wkang(l))
          if (ang > angsp2) then
            angsp2 = ang
            i1 = iv(r)
            i2 = iv(l)
          end if
        end if
      end do

    end if

  end do
!
!  Choose 1 or 2 separators based on max-min angle criterion or
!  ANGAC1 parameter.
!
110 continue

  if (angsp2 <= angsep) then
    i1 = ii
    i2 = -1
  else
    angsep = angsp2
  end if

  return
end
subroutine fndsph ( xh, yh, nvrt, xc, yc, ivis, theta, nv, iv, x, y, link, &
  angsep, v )
!
!******************************************************************************
!
!! FNDSPH finds a separator from top or bottom hole vertex.
!
!
!  Purpose: 
!
!    Find a separator from top or bottom hole vertex (XH,YH)
!    using a max-min angle criterion from list of vertices in
!    increasing polar angle with respect to horizontal ray through (XH,YH).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, XH, YH - coordinates of hole vertex.
!
!    Input, NVRT - (number of vertices) - 1.
!
!    Input, XC(0:NVRT), YC(0:NVRT) - vertex coordinates of possible
!    endpoints of a separator.
!
!    Input, IVIS(0:NVRT) - contains information about the vertices of
!    XC, YC arrays with respect to X, Y arrays; IVIS(I) = J > 0 if
!    (XC(I),YC(I)) = (X(K),Y(K)) where K = LINK(J), and
!    IVIS(I) = J < 0 if (XC(I),YC(I)) lies in interior of
!    edge starting at (X(-J),Y(-J)).
!
!    Input, THETA(0:NVRT) - polar angles of vertices in increasing order
!    with respect to horizontal ray through (XH,YH); THETA(NVRT) = PI.
!
!    Input, NV - (number of vertices to be considered as endpoint of a
!    separator) - 1.
!
!    Input, IV(0:NV) - indices in increasing order of vertices in XC, YC
!    arrays to be considered as separator endpoint; angle
!    between consecutive vertices is assumed to be < 180
!    degrees; it is also assumed that 0, NVRT not in array.
!
!    Input, X(1:*), Y(1:*), LINK(1:*) - used for 2D representation of
!    decomposition of multiply-connected polygonal face
!    of hole polygons; see routine SPDECH.
!
!    Output, ANGSEP - min of 4 angles at boundary resulting from separator.
!
!    Output, V - index of separator endpoint in XC, YC arrays.
!
  implicit none
!
  integer nv
  integer nvrt
!
  double precision alpha
  double precision angle
  double precision angmin
  double precision angsep
  double precision beta
  integer i
  integer iv(0:nv)
  integer ivis(0:nvrt)
  integer j
  integer k
  integer l
  integer link(*)
  double precision pi
  double precision theta(0:nvrt)
  double precision tol
  integer v
  double precision x(*)
  double precision xc(0:nvrt)
  double precision xh
  double precision y(*)
  double precision yc(0:nvrt)
  double precision yh
!
  angsep = 0.0d0
  tol = 100.0D+00 * epsilon ( tol )

  do k = 0, nv

    i = iv(k)
    j = ivis(i)

    if (j > 0) then
      l = link(link(j))
      alpha = angle(x(j),y(j),xc(i),yc(i),xh,yh)
      beta = angle(xh,yh,xc(i),yc(i),x(l),y(l))
    else
      j = -j
      alpha = angle(x(j),y(j),xc(i),yc(i),xh,yh)
      beta = pi() - alpha
    end if

    angmin = min ( theta(i), pi() - theta(i), alpha, beta )

    if (angmin > angsep) then
      angsep = angmin
      v = i
    end if

  end do

  return
end
subroutine fndtri ( iedg, mxtr, sflag, tedg, itr, ind, ierror )
!
!*******************************************************************************
!
!! FNDTRI finds two triangles containing a given edge.
!
!
!  Purpose:
!
!    Find two triangles containing edge with index IEDG in array TEDG.
!
!  Modified:
!
!    12 July 1999
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, integer IEDG, the index of edge to be searched in TEDG.
!
!    Input, integer MXTR, the maximum index of triangle to be searched in TEDG.
!
!    Input, logical SFLAG, is .TRUE. if and only if the second triangle is to be
!    searched from end of array.
!
!    Input, integer TEDG(1:3,1:MXTR), triangle edge indices; see routine CVDTRI.
!
!    Output, integer ITR(1:2), IND(1:2), indices such that IEDG =
!    TEDG(IND(1),ITR(1)) = TEDG(IND(2),ITR(2)).
!
!    Output, integer IERROR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer mxtr
!
  integer i
  integer iedg
  integer ierror
  integer ind(2)
  integer itr(2)
  integer j
  integer k
  logical sflag
  integer tedg(3,mxtr)
!
!  Search from end of array TEDG.
!
  ierror = 0
  k = 1
  j = 1
  i = mxtr

10 continue

  do

    if ( tedg(j,i) == iedg ) then
      exit
    end if

    j = j + 1

    if ( j > 3 ) then
      j = 1
      i = i - 1
      if ( i <= 0 ) then
        ierror = 231
        return
      end if
    end if

  end do

  itr(k) = i
  ind(k) = j

  if ( k == 2 ) then
    return
  end if

  k = 2

  if ( sflag ) then

    j = 1
    i = i - 1

    if ( i <= 0 ) then
      ierror = 231
      return
    end if

    go to 10

  end if
!
!  Search from beginning of array TEDG for second triangle.
!
  j = 1
  i = 1
   20 continue

  if ( i >= itr(1) ) then
    ierror = 231
    return
  end if

   30 continue

  if ( tedg(j,i) /= iedg ) then
    j = j + 1
    if ( j > 3 ) then
      j = 1
      i = i + 1
      go to 20
    else
      go to 30
    end if
  end if

  itr(2) = i
  ind(2) = j

  return
end
subroutine frsmpx ( k, shift, nv, vcl, map, inds, ipvt, mat, ierr )
!
!******************************************************************************
!
!! FRSMPX shifts vertices to the first K+1 are in general position in KD.
!
!
!  Purpose: 
!
!    Shift or swap vertices if necessary so first K+1 vertices
!    are not in same hyperplane (so first simplex is valid).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of points.
!
!    Input, SHIFT - if .TRUE., MAP(3:K+1) may be updated due to shift,
!    else they may be updated due to swaps; in former case,
!    it is assumed MAP gives vertices in lexicographic order.
!
!    Input, NV - number of vertices.
!
!    Input, VCL(1:K,1:*) - vertex coordinate list.
!
!    Input, MAP(1:NV).  On input, contains vertex indices of VCL.
!    On output, shifted or K-1 swaps applied if necessary so vertices
!    indexed by MAP(1), ..., MAP(K+1) not in same hyperplane.
!
!    Output, INDS(3:K+1) - indices such that MAP_in(INDS(I)) = MAP_out(I).
!
!    Workspace, MAT(1:K,1:K) - matrix used for determining rank.
!
!    Workspace, IPVT(1:K) - pivot indices.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer nv
!
  integer c
  double precision cmax
  integer i
  integer ierr
  integer ii
  integer inds(3:k+1)
  integer ipvt(k)
  integer j
  integer jj
  integer l
  integer m
  integer m1
  integer map(nv)
  double precision mat(k,k)
  double precision mult
  double precision pivot
  double precision rtol
  logical shift
  double precision tol
  double precision vcl(k,*)
!
!  First check that consecutive vertices are not identical.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )

  if (shift) then
    l = nv - 1
  else
    l = 1
  end if

  m1 = map(1)

  do i = 1,l

    m = m1
    m1 = map(i+1)

    do j = 1,k
      cmax = max( abs(vcl(j,m)),abs(vcl(j,m1)) )
      if (abs(vcl(j,m) - vcl(j,m1)) > tol*cmax .and. cmax > tol) then
        go to 20
      end if
    end do

    ierr = 402
    return

20  continue

  end do
!
!  Find indices INDS(3), ..., INDS(K+1).
!
  m1 = map(1)
  cmax = 0.0d0
  c = 1
  i = 1

30 continue

  i = i + 1

  if (i > nv) then
    ierr = 403
    return
  end if

  m = map(i)

  do j = 1,k
    mat(j,c) = vcl(j,m) - vcl(j,m1)
    cmax = max(cmax,abs(mat(j,c)))
  end do

  rtol = tol*cmax

  do jj = 1,c-1
    l = ipvt(jj)
    mult = mat(l,c)
    mat(l,c) = mat(jj,c)
    do ii = jj+1,k
      mat(ii,c) = mat(ii,c) - mult*mat(ii,jj)
    end do
  end do

  l = c

  do j = c+1,k
    if (abs(mat(j,c)) > abs(mat(l,c))) l = j
  end do

  pivot = mat(l,c)

  if (c > 1) then
    if (abs(pivot) < rtol) go to 30
    inds(c+1) = i
  end if

  ipvt(c) = l

  if (l /= c) then
    mat(l,c) = mat(c,c)
    mat(c,c) = pivot
  end if

  do ii = c+1,k
    mat(ii,c) = mat(ii,c)/pivot
  end do

  if (c < k) then
    c = c + 1
    go to 30
  end if
!
!  Shift or swap elements of MAP if necessary.
!
  if (shift) then

    do i = 3,k+1
      if (inds(i) > i) ipvt(i-2) = map(inds(i))
    end do

    do i = k+1,3,-1

      if (inds(i) > i) then

        l = k + 2 - i

        if (i > 3) then
          m = inds(i-1) + 1
        else
          m = 3
        end if

        do j = inds(i)-1,m,-1
          map(j+l) = map(j)
        end do

      end if

    end do

    do i = 3,k+1
      if (inds(i) > i) map(i) = ipvt(i-2)
    end do

  else

    do i = 3,k+1

      if (inds(i) > i) then
        m = map(i)
        map(i) = map(inds(i))
        map(inds(i)) = m
      end if

    end do

  end if

  return
end
subroutine frstet ( shift, nv, vcl, map, i3, i4, ierr )
!
!******************************************************************************
!
!! FRSTET shifts vertices so the first 4 vertices are in general position in 3D.
!
!
!  Purpose: 
!
!    Shift or swap vertices if necessary so first 3 vertices
!    (according to MAP) are not collinear and first 4 vertices are
!    not coplanar (so that first tetrahedron is valid).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, SHIFT - if .TRUE., MAP(3), MAP(4) may be updated due to shift,
!    else they may be updated due to swaps; in former case,
!    it is assumed MAP gives vertices in lexicographic order.
!
!    Input, NV - number of vertices.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input/output, MAP(1:NV), on input, contains vertex indices of VCL.
!    On output, shifted or 2 swaps applied if necessary so that vertices
!    indexed by MAP(1), MAP(2), MAP(3), MAP(4) not coplanar.
!
!    Output, I3, I4 - the indices such that MAP_in(I3) = MAP_out(3) and
!    MAP_in(I4) = MAP_out(4).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer nv
!
  double precision cmax
  double precision cp1
  double precision cp2
  double precision cp3
  double precision dmax
  double precision dotp
  double precision dv2(3)
  double precision dvk(3)
  double precision dvl(3)
  integer i
  integer i3
  integer i4
  integer ierr
  integer k
  integer l
  integer m
  integer m1
  integer m2
  integer map(nv)
  logical shift
  double precision tol
  double precision vcl(3,*)
!
!  First check that consecutive vertices are not identical.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )

  if (shift) then
    l = nv - 1
  else
    l = 1
  end if

  m1 = map(1)

  do i = 1,l

    m = m1
    m1 = map(i+1)
    do k = 1,3
      cmax = max( abs(vcl(k,m)),abs(vcl(k,m1)) )
      if (abs(vcl(k,m) - vcl(k,m1)) > tol*cmax .and. cmax > tol) then
        go to 20
      end if
    end do

    ierr = 302
    return

20  continue

  end do
!
!  Find index K = I3 and L = I4.
!
  m1 = map(1)
  m2 = map(2)
  dv2(1) = vcl(1,m2) - vcl(1,m1)
  dv2(2) = vcl(2,m2) - vcl(2,m1)
  dv2(3) = vcl(3,m2) - vcl(3,m1)
  cmax = max( abs(vcl(1,m1)),abs(vcl(2,m1)),abs(vcl(3,m1)), &
    abs(vcl(1,m2)),abs(vcl(2,m2)),abs(vcl(3,m2)) )
  k = 2

30 continue

  k = k + 1

  if (k > nv) then
    ierr = 303
    return
  end if

  m = map(k)
  dvk(1) = vcl(1,m) - vcl(1,m1)
  dvk(2) = vcl(2,m) - vcl(2,m1)
  dvk(3) = vcl(3,m) - vcl(3,m1)
  dmax = max(cmax, abs(vcl(1,m)),abs(vcl(2,m)),abs(vcl(3,m)) )
  cp1 = dv2(2)*dvk(3) - dv2(3)*dvk(2)
  cp2 = dv2(3)*dvk(1) - dv2(1)*dvk(3)
  cp3 = dv2(1)*dvk(2) - dv2(2)*dvk(1)

  if (max(abs(cp1),abs(cp2),abs(cp3)) <= tol*dmax) then
    go to 30
  end if

  cmax = dmax
  l = k

40 continue

  l = l + 1

  if (l > nv) then
    ierr = 304
    return
  end if

  m = map(l)
  dvl(1) = vcl(1,m) - vcl(1,m1)
  dvl(2) = vcl(2,m) - vcl(2,m1)
  dvl(3) = vcl(3,m) - vcl(3,m1)
  dmax = max(cmax, abs(vcl(1,m)),abs(vcl(2,m)),abs(vcl(3,m)) )
  dotp = dvl(1)*cp1 + dvl(2)*cp2 + dvl(3)*cp3

  if (abs(dotp) <= tol*dmax) go to 40
!
!  Shift or swap elements of MAP if necessary.
!
  if (shift) then

    if (k > 3) then
      m1 = map(k)
    end if

    if (l > 4) then
      m2 = map(l)
      do i = l,k+2,-1
         map(i) = map(i-1)
      end do
      do i = k+1,5,-1
        map(i) = map(i-2)
      end do
      map(4) = m2
    end if

    if (k > 3) map(3) = m1

  else

    if (k > 3) then
      m = map(3)
      map(3) = map(k)
      map(k) = m
    end if

    if (l > 4) then
      m = map(4)
      map(4) = map(l)
      map(l) = m
    end if

  end if

  i3 = k
  i4 = l

  return
end
subroutine gtime ( time )
!
!******************************************************************************
!
!! GTIME returns the current CPU time in seconds.
!
!
!  Modified:
!
!    17 September 2001
!
!  Parameters:
!
!    Output, real TIME, the current CPU time in seconds.
!
  implicit none
!
  real time
!
  call cpu_time ( time )

  return
end
subroutine hexagon_vertices_2d ( x, y )
!
!*******************************************************************************
!
!! HEXAGON_VERTICES_2D returns the vertices of the unit hexagon in 2D.
!
!
!  Diagram:
!
!      120_____60
!        /     \
!    180/       \0
!       \       /
!        \_____/
!      240     300
!
!  Discussion:
!
!    The unit hexagon has maximum radius 1, and is the hull of the points
!
!      (   1,              0 ),
!      (   0.5,   sqrt (3)/2 ),
!      ( - 0.5,   sqrt (3)/2 ),
!      ( - 1,              0 ),
!      ( - 0.5, - sqrt (3)/2 ),
!      (   0.5, - sqrt (3)/2 ).
!
!  Modified:
!
!    21 September 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, double precision X(6), Y(6), the coordinates of the vertices.
!
  implicit none
!
  double precision, parameter :: a = 0.8660254037844386D+00
  double precision x(6)
  double precision y(6)
!
  x(1:6) = (/ 1.0D+00, 0.5D+00, -0.5D+00, -1.0D+00, -0.5D+00,  0.5D+00 /)
  y(1:6) = (/ 0.0D+00, a,        a,        0.0D+00, -a,       -a /)

  return
end
subroutine holvrt ( nhole, vcl, hvl, pvl, holv )
!
!******************************************************************************
!
!! HOLVRT determines top and bottom vertices of holes in polygonal regions.
!
!
!  Purpose: 
!
!    Determine top and bottom vertices of holes in polygonal
!    region(s), and sort top vertices in decreasing (y,x) order
!    and bottom vertices in increasing (y,x) order.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NHOLE - number of holes in region(s).
!
!    Input, VCL(1:2,1:*) - vertex coordinate list.
!
!    Input, HVL(1:NHOLE) - head vertex list; HVL(I) is index in PVL of
!    head vertex of Ith hole.
!
!    Input, PVL(1:4,1:*) - polygon vertex list; see routine DSPGDC.
!
!    Output, HOLV(1:NHOLE*2) - indices in PVL of top and bottom vertices of
!    holes; first (last) NHOLE entries are for top (bottom)
!    vertices; top (bottom) vertices are sorted in decreasing
!    (increasing) lexicographic (y,x) order of coordinates.
!
  implicit none
!
  integer nhole
!
  integer, parameter :: edgv = 4
  integer holv(nhole*2)
  integer hv
  integer hvl(nhole)
  integer i
  integer imax
  integer imin
  integer iv
  integer j
  integer, parameter :: loc = 1
  integer lv
  integer nhp1
  integer, parameter :: polg = 2
  integer pvl(4,*)
  integer, parameter :: succ = 3
  double precision vcl(2,*)
  double precision x
  double precision xmax
  double precision xmin
  double precision y
  double precision ymax
  double precision ymin
!
!  Determine top and bottom vertices of holes.
!
  do i = 1, nhole

    hv = hvl(i)
    iv = hv

    do

      lv = pvl(loc,iv)

      if (iv == hv) then

        imin = iv
        imax = iv
        xmin = vcl(1,lv)
        ymin = vcl(2,lv)
        xmax = xmin
        ymax = ymin

      else

        x = vcl(1,lv)
        y = vcl(2,lv)

        if (y < ymin .or. y == ymin .and. x < xmin) then
          imin = iv
          xmin = x
          ymin = y
        else if (y > ymax .or. y == ymax .and. x > xmax) then
          imax = iv
          xmax = x
          ymax = y
        end if

      end if

      iv = pvl(succ,iv)

      if (iv == hv) then
        exit
      end if

    end do

    holv(i) = imax
    holv(i+nhole) = imin

  end do
!
!  Use linear insertion sort to sort the top vertices of holes
!  in decreasing (y,x) order, then bottom vertices in increasing
!  (y,x) order.  It is assumed NHOLE is small.
!
  do i = 2, nhole

    hv = holv(i)
    lv = pvl(loc,hv)
    x = vcl(1,lv)
    y = vcl(2,lv)
    j = i

30  continue

    iv = holv(j-1)
    lv = pvl(loc,iv)

    if (y > vcl(2,lv) .or. y == vcl(2,lv) .and. x > vcl(1,lv)) then
      holv(j) = iv
      j = j - 1
      if (j > 1) go to 30
    end if

    holv(j) = hv

  end do

  nhp1 = nhole + 1

  do i = nhp1+1, nhole+nhole

    hv = holv(i)
    lv = pvl(loc,hv)
    x = vcl(1,lv)
    y = vcl(2,lv)
    j = i

50  continue

    iv = holv(j-1)
    lv = pvl(loc,iv)

    if (y < vcl(2,lv) .or. y == vcl(2,lv) .and. x < vcl(1,lv)) then
      holv(j) = iv
      j = j - 1
      if (j > nhp1) go to 50
    end if

    holv(j) = hv

  end do

  return
end
subroutine htdel ( ind, n, p, fc, ht )
!
!******************************************************************************
!
!! HTDEL deletes a record from the hash table.
!
!
!  Purpose: 
!
!    Delete record FC(1:7,IND) from hash table HT.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, IND - index of FC array.
!
!    Input, N - upper bound on vertex indices.
!
!    Input, P - size of hash table.
!
!    Input/output, FC(1:7,1:*) - array of face records; see routine DTRIS3.
!    On output, one link in FC is updated.
!
!    Input/output, HT(0:P-1) - hash table using direct chaining.  On output,
!    one link in HT is updated.
!
  implicit none
!
  integer p
!
  integer fc(7,*)
  integer ht(0:p-1)
  integer ind
  integer k
  integer n
  integer ptr
!
  k = mod(fc(1,ind)*n + fc(2,ind), p)
  k = mod(k*n + fc(3,ind), p)
  ptr = ht(k)

  if (ptr == ind) then

    ht(k) = fc(6,ind)

  else

    do while ( fc(6,ptr) /= ind )
      ptr = fc(6,ptr)
    end do
    fc(6,ptr) = fc(6,ind)

  end if

  return
end
subroutine htdelk ( k, pos, n, p, fc, ht )
!
!******************************************************************************
!
!! HTDELK deletes a record from the hash table.
!
!
!  Purpose: 
!
!    Delete record FC(1:K+4,POS) from hash table HT.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - number of vertices in a face.
!
!    Input, POS - position of FC array.
!
!    Input, N - upper bound on vertex indices.
!
!    Input, P - size of hash table.
!
!    Input/output, FC(1:K+4,1:*) - array of face records; see routine DTRISK.
!
!    Input/output, HT(0:P-1) - hash table using direct chaining.
!
  implicit none
!
  integer k
  integer p
!
  integer fc(k+4,*)
  integer h
  integer ht(0:p-1)
  integer i
  integer kp3
  integer n
  integer pos
  integer ptr
!
  kp3 = k + 3
  h = fc(1,pos)

  do i = 2, k
    h = mod(h*n + fc(i,pos), p)
  end do

  ptr = ht(h)

  if (ptr == pos) then

    ht(h) = fc(kp3,pos)

  else

    do while ( fc(kp3,ptr) /= pos ) 
      ptr = fc(kp3,ptr)
    end do

    fc(kp3,ptr) = fc(kp3,pos)

  end if

  return
end
subroutine htins ( ind, a, b, c, d, e, n, p, fc, ht )
!
!******************************************************************************
!
!! HTINS inserts a record into the hash table.
!
!
!  Purpose: 
!
!    Insert record FC(1:7,IND) containing A,B,C,D,E,HTLINK,-1
!    into hash table HT.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, IND - index of FC array.
!
!    Input, A, B, C, D, E - first 5 fields of FC record (or column).
!
!    Input, N - upper bound on vertex indices.
!
!    Input, P - size of hash table.
!
!    Input/output, FC(1:7,1:*) - array of face records; see routine DTRIS3.
!
!    Input/output, HT(0:P-1) - hash table using direct chaining
!
  implicit none
!
  integer p
!
  integer a
  integer aa
  integer b
  integer bb
  integer c
  integer cc
  integer d
  integer e
  integer fc(7,*)
  integer ht(0:p-1)
  integer ind
  integer k
  integer n
!
  aa = a
  bb = b
  cc = c
  call order3(aa,bb,cc)
  k = mod(aa*n + bb, p)
  k = mod(k*n + cc, p)
  fc(1,ind) = aa
  fc(2,ind) = bb
  fc(3,ind) = cc
  fc(4,ind) = d
  fc(5,ind) = e
  fc(6,ind) = ht(k)
  fc(7,ind) = -1
  ht(k) = ind

  return
end
subroutine htinsk ( k, pos, ind, d, e, n, p, fc, ht )
!
!******************************************************************************
!
!! HTINSK inserts a record into the hash table.
!
!
!  Purpose: 
!
!    Insert record FC(1:K+4,POS) containing IND(1:K),D,E,
!    HTLINK,-1 into hash table HT.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - number of vertices in a face.
!
!    Input, POS - position of FC array.
!
!    Input/output, IND(1:K) - vertex indices of face.  On output,
!    sorted into nondecreasing order.
!
!    Input, D, E - fields K+1, K+2 of FC record (or column).
!
!    Input, N - upper bound on vertex indices.
!
!    Input, P - size of hash table.
!
!    Input/output, FC(1:K+4,1:*) - array of face records; see routine 
!    DTRISK.
!
!    Input/output, HT(0:P-1) - hash table using direct chaining.
!
  implicit none
!
  integer k
  integer p
!
  integer d
  integer e
  integer fc(k+4,*)
  integer h
  integer ht(0:p-1)
  integer i
  integer ind(k)
  integer n
  integer pos
!
  call orderk(k,ind)

  h = ind(1)
  do i = 2,k
    h = mod(h*n + ind(i), p)
  end do

  fc(1:k,pos) = ind(1:k)
  fc(k+1,pos) = d
  fc(k+2,pos) = e
  fc(k+3,pos) = ht(h)
  fc(k+4,pos) = -1
  ht(h) = pos

  return
end
subroutine htsdlk ( k, ind, n, p, fc, ht, pos )
!
!******************************************************************************
!
!! HTSDLK searches for a record in the hash table, and deletes it if found.
!
!
!  Purpose: 
!
!    Search for record FC(1:K+4,POS) containing key IND(1:K)
!    in hash table HT and delete it from hash table if found.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - number of vertices in a face.
!
!    Input/output, IND(1:K) - vertex indices of face (in any order).
!    On output, sorted into nondecreasing order.
!
!    Input, N - upper bound on vertex indices.
!
!    Input, P - size of hash table.
!
!    Input/output, FC(1:K+4,1:*) - array of face records; see routine DTRISK.
!
!    Input/output, HT(0:P-1) - hash table using direct chaining.
!
!    Output, POS - position of FC record with key IND(1:K) if found,
!    or 0 if not found.
!
  implicit none
!
  integer k
  integer p
!
  integer fc(k+4,*)
  integer h
  integer ht(0:p-1)
  integer i
  integer ind(k)
  integer kp3
  integer n
  integer pos
  integer ptr
!
  kp3 = k + 3
  call orderk(k,ind)

  h = ind(1)
  do i = 2, k
    h = mod(h*n + ind(i), p)
  end do

  ptr = -1
  pos = ht(h)

20 continue

  if (pos /= 0) then

    i = 1

30  continue

    if (fc(i,pos) /= ind(i)) then
      ptr = pos
      pos = fc(kp3,pos)
      go to 20
    end if

    i = i + 1

    if (i <= k) then
      go to 30
    end if
 
    if (ptr == -1) then
      ht(h) = fc(kp3,pos)
    else
      fc(kp3,ptr) = fc(kp3,pos)
    end if

  end if

  return
end
function htsrc ( a, b, c, n, p, fc, ht )
!
!******************************************************************************
!
!! HTSRC searches for a record in the hash table.
!
!
!  Purpose: 
!
!    Search for record FC(1:7,IND) containing key A,B,C
!    in hash table HT.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A,B,C - first 3 fields of FC record (in any order).
!
!    Input, N - upper bound on vertex indices.
!
!    Input, P - size of hash table.
!
!    Input, FC(1:7,1:*) - array of face records; see routine DTRIS3.
!
!    Input, HT(0:P-1) - hash table using direct chaining.
!
!    Output, HTSRC - index of FC record with key A,B,C if found,
!    or 0 if not found.
!
  implicit none
!
  integer p
!
  integer a
  integer aa
  integer b
  integer bb
  integer c
  integer cc
  integer fc(7,*)
  integer ht(0:p-1)
  integer htsrc
  integer ind
  integer k
  integer n
!
  aa = a
  bb = b
  cc = c
  call order3(aa,bb,cc)
  k = mod(aa*n + bb, p)
  k = mod(k*n + cc, p)
  ind = ht(k)

  do

    if (ind == 0) then
      exit
    end if

    if (fc(1,ind) == aa .and. fc(2,ind) == bb .and. fc(3,ind) == cc) then
      exit
    end if

    ind = fc(6,ind)

  end do

  htsrc = ind

  return
end
function htsrck ( k, ind, n, p, fc, ht )
!
!******************************************************************************
!
!! HTSRCK searches for a record in the hash table.
!
!
!  Purpose: 
!
!    Search for record FC(1:K+4,POS) containing key IND(1:K)
!    in hash table HT.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - number of vertices in a face.
!
!    Input/output, IND(1:K) - vertex indices of face.  On output, these
!    have been sorted into nondecreasing order.
!
!    Input, N - upper bound on vertex indices.
!
!    Input, P - size of hash table.
!
!    Input, FC(1:K+4,1:*) - array of face records; see routine DTRISK.
!
!    Input, HT(0:P-1) - hash table using direct chaining.
!
!    Output, HTSRCK - position of FC record with key IND(1:K) if found,
!    or 0 if not found.
!
  implicit none
!
  integer k
  integer p
!
  integer fc(k+4,*)
  integer h
  integer ht(0:p-1)
  integer htsrck
  integer i
  integer ind(k)
  integer kp3
  integer n
  integer pos
!
  kp3 = k + 3
  call orderk(k,ind)

  h = ind(1)
  do i = 2,k
    h = mod(h*n + ind(i), p)
  end do

  pos = ht(h)

20 continue

  if (pos /= 0) then

    i = 1

30  continue

    if (fc(i,pos) /= ind(i)) then
      pos = fc(kp3,pos)
      go to 20
    end if

    i = i + 1

    if (i <= k) then
      go to 30
    end if

  end if

  htsrck = pos

  return
end
function i_modp ( i, j )
!
!*******************************************************************************
!
!! I_MODP returns the nonnegative remainder of integer division.
!
!
!  Formula:
!
!    If
!      NREM = I_MODP ( I, J )
!      NMULT = ( I - NREM ) / J
!    then
!      I = J * NMULT + NREM
!    where NREM is always nonnegative.
!
!  Comments:
!
!    The MOD function computes a result with the same sign as the
!    quantity being divided.  Thus, suppose you had an angle A,
!    and you wanted to ensure that it was between 0 and 360.
!    Then mod(A,360) would do, if A was positive, but if A
!    was negative, your result would be between -360 and 0.
!
!    On the other hand, I_MODP(A,360) is between 0 and 360, always.
!
!  Examples:
!
!        I     J     MOD  I_MODP    Factorization
!
!      107    50       7       7    107 =  2 *  50 + 7
!      107   -50       7       7    107 = -2 * -50 + 7
!     -107    50      -7      43   -107 = -3 *  50 + 43
!     -107   -50      -7      43   -107 =  3 * -50 + 43
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, the number to be divided.
!
!    Input, integer J, the number that divides I.
!
!    Output, integer I_MODP, the nonnegative remainder when I is
!    divided by J.
!
  implicit none
!
  integer i
  integer i_modp
  integer j
!
  if ( j == 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'I_MODP - Fatal error!'
    write ( *, * ) '  I_MODP ( I, J ) called with J = ', j
    stop
  end if

  i_modp = mod ( i, j )

  if ( i_modp < 0 ) then
    i_modp = i_modp + abs ( j )
  end if

  return
end
subroutine i_random ( ilo, ihi, i )
!
!*******************************************************************************
!
!! I_RANDOM returns a random integer in a given range.
!
!
!  Modified:
!
!    23 September 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ILO, IHI, the minimum and maximum acceptable values.
!
!    Output, integer I, the randomly chosen integer.
!
  integer i
  integer ihi
  integer ilo
  real r
  real, parameter :: rhi = 1.0E+00
  real, parameter :: rlo = 0.0E+00
!
  call r_random ( rlo, rhi, r )

  i = ilo + int ( r * real ( ihi + 1 - ilo ) )
!
!  In case of oddball events at the boundary, enforce the limits.
!
  i = max ( i, ilo )
  i = min ( i, ihi )

  return
end
subroutine i_swap ( i, j )
!
!*******************************************************************************
!
!! I_SWAP swaps two integer values.
!
!
!  Modified:
!
!    30 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer I, J.  On output, the values of I and
!    J have been interchanged.
!
  implicit none
!
  integer i
  integer j
  integer k
!
  k = i
  i = j
  j = k

  return
end
function i_wrap ( ival, ilo, ihi )
!
!*******************************************************************************
!
!! I_WRAP forces an integer to lie between given limits by wrapping.
!
!
!  Example:
!
!    ILO = 4, IHI = 8
!
!    I  I_WRAP
!
!    -2     8
!    -1     4
!     0     5
!     1     6
!     2     7
!     3     8
!     4     4
!     5     5
!     6     6
!     7     7
!     8     8
!     9     4
!    10     5
!    11     6
!    12     7
!    13     8
!    14     4
!
!  Modified:
!
!    15 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer IVAL, an integer value.
!
!    Input, integer ILO, IHI, the desired bounds for the integer value.
!
!    Output, integer I_WRAP, a "wrapped" version of IVAL.
!
  implicit none
!
  integer i_modp
  integer i_wrap
  integer ihi
  integer ilo
  integer ival
  integer wide
!
  wide = ihi + 1 - ilo

  if ( wide == 0 ) then
    i_wrap = ilo
  else
    i_wrap = ilo + i_modp ( ival-ilo, wide )
  end if

  return
end
subroutine ifacty ( ind, npt, sizht, vcl, vm, fc, ht, type, a, b, c, ierr )
!
!******************************************************************************
!
!! IFACTY determines the type of an interior face in a 3D triangulation.
!
!
!  Purpose: 
!
!    Determine type of interior face of 3D triangulation.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, IND - index of FC array, assumed to be an interior face.
!
!    Input, NPT - size of VM array.
!
!    Input, SIZHT - size of HT array.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:NPT) - vertex mapping array, from local to global indices.
!
!    Input, FC(1:7,1:*) - array of face records; see routine DTRIS3.
!
!    Input, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Output, TYPE - 'T23','T32','T22','T44','N32','N44','N40','N30',or 'N20'.
!
!    Output, A, B, C - local indices of interior face; for T32, N32, T22, T44,
!    or N44 face, AB is edge that would get swapped out and C
!    is third vertex; for T23 face, A < B < C; for N40 face,
!    A is interior vertex; for N30 face, A is inside a face
!    with vertex B; for N20 face, A is on an edge.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer npt
  integer sizht
!
  integer a
  double precision alpha(4)
  integer b
  integer c
  integer d
  logical degen
  integer e
  integer f
  integer fc(7,*)
  integer ht(0:sizht-1)
  integer htsrc
  integer ierr
  integer ind
  integer ind1
  integer j
  integer kneg
  integer kzero
  character ( len = 3 ) type
  double precision vcl(3,*)
  integer vm(npt)
!
  ierr = 0
  a = fc(1,ind)
  b = fc(2,ind)
  c = fc(3,ind)
  d = fc(4,ind)
  e = fc(5,ind)

  call baryth(vcl(1,vm(a)),vcl(1,vm(b)),vcl(1,vm(c)),vcl(1,vm(d)), &
    vcl(1,vm(e)),alpha,degen)

  if (degen) then
    ierr = 301
    return
  else if (alpha(4) > 0.0d0) then
    ierr = 309
    return
  end if

  kneg = 1
  kzero = 0

  do j = 1,3
    if (alpha(j) < 0.0d0) then
      kneg = kneg + 1
    else if (alpha(j) == 0.0d0) then
      kzero = kzero + 1
    end if
  end do

  type = 'xxx'

  if (kneg == 1 .and. kzero == 0) then

    type = 't23'

  else if (kneg == 2 .and. kzero == 0) then

    if (alpha(1) < 0.0d0) then
      call i_swap ( a, c )
    else if (alpha(2) < 0.0d0) then
      call i_swap ( b, c )
    end if

    ind1 = htsrc(a,b,d,npt,sizht,fc,ht)

    if (ind1 <= 0) then
      ierr = 300
      return
    else if (fc(4,ind1) == e .or. fc(5,ind1) == e) then
      type = 't32'
    else
      type = 'n32'
    end if

  else if (kneg == 3 .and. kzero == 0) then

    type = 'n40'

    if (alpha(2) > 0.0d0) then
      call i_swap ( a, b )
    else if (alpha(3) > 0.0d0) then
      call i_swap ( a, c )
    end if

  else if (kneg == 1 .and. kzero == 1) then

    if (alpha(1) == 0.0d0) then
      call i_swap ( a, c )
    else if (alpha(2) == 0.0d0) then
      call i_swap ( b, c )
    end if

    ind1 = htsrc(a,b,d,npt,sizht,fc,ht)

    if (ind1 <= 0) then

      ierr = 300
      return

    else if (fc(5,ind1) <= 0) then

      type = 't22'

    else

      if (fc(4,ind1) == c) then
        f = fc(5,ind1)
      else
        f = fc(4,ind1)
      end if

      ind1 = htsrc(a,b,e,npt,sizht,fc,ht)

      if (ind1 <= 0) then
        ierr = 300
        return
      else if (fc(4,ind1) == f .or. fc(5,ind1) == f) then
        type = 't44'
      else
        type = 'n44'
      end if

    end if

  else if (kneg == 2 .and. kzero == 1) then

    type = 'n30'

    if (alpha(1) == 0.0d0) then
      call i_swap ( a, c )
    else if (alpha(2) == 0.0d0) then
      call i_swap ( b, c )
      alpha(2) = alpha(3)
    end if

    if (alpha(2) > 0.0d0) then
      call i_swap ( a, b )
    end if

  else if (kneg == 1 .and. kzero == 2) then

    type = 'n20'

    if (alpha(2) > 0.0d0) then
      call i_swap ( a, b )
    else if (alpha(3) > 0.0d0) then
      call i_swap ( a, c )
     end if

  end if

  return
end
subroutine ihpsrt ( k, n, lda, a, map )
!
!******************************************************************************
!
!! IHPSRT sorts a list of integer points in KD.
!
!
!  Purpose: 
!
!    Use heapsort to obtain the permutation of N K-dimensional
!    integer points so that the points are in lexicographic
!    increasing order.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of points.
!
!    Input, N - number of points.
!
!    Input, LDA - leading dimension of array A in calling routine; should
!    be >= K.
!
!    Input, A(1:K,1:*) - array of >= N K-D integer points.
!
!    Input/output, MAP(1:N).  On input, the points of A with indices 
!    MAP(1), MAP(2), ..., MAP(N) are to be sorted.  On output, elements 
!    are permuted so that A(*,MAP(1)) <= A(*,MAP(2)) <= ... <= A(*,MAP(N))
!
  implicit none
!
  integer lda
  integer n
!
  integer a(lda,*)
  integer i
  integer k
  integer map(n)
  integer t
!
  do i = n/2, 1, -1
    call isftdw(i,n,k,lda,a,map)
  end do

  do i = n, 2, -1
    t = map(1)
    map(1) = map(i)
    map(i) = t
    call isftdw(1,i-1,k,lda,a,map)
  end do

  return
end
function iless ( k, p, q )
!
!******************************************************************************
!
!! ILESS determines the lexicographically lesser of two integer values.
!
!
!  Purpose:
!
!    Determine whether P is lexicographically less than Q.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of points.
!
!    Input, P(1:K), Q(1:K) - two K-dimensional integer points.
!
!    Output, ILESS - .TRUE. if P < Q, .FALSE. otherwise.
!
  implicit none
!
  integer k
!
  integer i
  logical iless
  integer p(k)
  integer q(k)
!
  do i = 1,k

    if (p(i) == q(i)) then
      cycle
    end if

    if (p(i) < q(i)) then
      iless = .true.
    else
      iless = .false.
    end if

    return

  end do

  iless = .false.

  return
end
subroutine imptr3 ( bndcon, postlt, crit, npt, sizht, maxfc, vcl, vm, nfc, &
  ntetra, bf, fc, ht, nface, ierr )
!
!******************************************************************************
!
!! IMPTR3 improves a 3D triangulation.
!
!
!  Purpose: 
!
!    Improve a given 3D triangulation by applying local
!    transformations based on some local criterion.
!
!  Discussion:
!
!    BF, FC, HT should be as output by DTRIS3 or DTRIW3.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, BNDCON - .TRUE. iff boundary faces are constrained (i.e. not
!    swapped by local transformations).
!
!    Input, POSTLT - .TRUE. iff further local transformations applied by
!    postprocessing routine IMPTRF.
!
!    Input, CRIT - criterion code: 1 for (local max-min) solid angle
!    criterion, 2 for radius ratio criterion, 3 for mean ratio
!    criterion, < 1 or > 3 for empty circumsphere criterion.
!
!    Input, NPT - number of 3D vertices (points).
!
!    Input, SIZHT - size of hash table HT; a good choice is a prime number
!    which is about 1/8 * NFACE (or 3/2 * NPT for random
!    points from the uniform distribution).
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:NPT) - indices of vertices of VCL being triangulated.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input/output, NTETRA - number of tetrahedra in triangulation.
!
!    Input/output, BF(1:3,1:*) -  array of boundary face records; see DTRIS3.
!
!    Input/output, FC(1:7,1:MAXFC) - array of face records; see routine DTRIS3.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Output, NFACE - number of faces in triangulation; NFACE <= NFC.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfc
  integer npt
  integer sizht
!
  integer back
  integer bf(3,*)
  logical bndcon
  integer crit
  integer fc(7,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer i
  integer ierr
  integer, parameter :: msglvl = 0
  integer nface
  integer nfc
  integer ntetra
  logical postlt
  integer ptr
  double precision vcl(3,*)
  integer vm(npt)
!
!  Create initial queue of interior faces.
!
  ierr = 0
  hdavbf = fc(7,1)
  hdavfc = fc(7,2)
  fc(7,1) = -1
  fc(7,2) = -1
  front = 0

  do i = 1,nfc
    if (fc(1,i) > 0 .and. fc(5,i) > 0) then
      if (front == 0) then
        front = i
      else
        fc(7,back) = i
      end if
      back = i
    end if
  end do

  if (front /= 0) then
    fc(7,back) = 0
  end if

  if ( msglvl == 4 ) then
    write ( *,600) crit
  end if

  if (crit >= 1 .and. crit <= 3) then
    call swapmu(bndcon,crit,npt,sizht,nfc,maxfc,vcl,vm,bf,fc,ht, &
      ntetra,hdavfc,front,back,i, ierr )
  else
    call swapes(bndcon,0,npt,sizht,nfc,maxfc,vcl,vm,bf,fc,ht, &
      ntetra,hdavfc,front,back,i, ierr )
  end if

  if (ierr /= 0) return

  if (crit >= 1 .and. crit <= 3 .and. postlt) then
    call imptrf(bndcon,crit,npt,sizht,maxfc,vcl,vm,nfc,ntetra, &
      hdavfc,bf,fc,ht,ierr)
  else if (postlt) then
    call imptrd(bndcon,npt,sizht,maxfc,vcl,vm,nfc,ntetra,hdavfc, &
      bf,fc,ht,ierr)
  end if

  if (ierr /= 0) then
    return
  end if

  nface = nfc
  ptr = hdavfc

  do while (ptr /= 0)
    nface = nface - 1
    ptr = -fc(1,ptr)
  end do

  fc(7,1) = hdavbf
  fc(7,2) = hdavfc

  600 format (/1x,'imptr3: criterion =',i3)

  return
end
subroutine imptrd ( bndcon, npt, sizht, maxfc, vcl, vm, nfc, ntetra, hdavfc, &
  bf, fc, ht, ierr )
!
!******************************************************************************
!
!! IMPTRD further improves a 3D triangulation.
!
!
!  Purpose: 
!
!    Further improve given 3D triangulation towards Delaunay
!    one by using combination local transformations (not yet
!    guaranteed to produce Delaunay triangulation).
!
!  Discussion:
!
!    BF, FC, HT should be as output by DTRIS3 or DTRIW3,
!    except it is assumed FC(7,1:2) don't contain HDAVBF, HDAVFC.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, BNDCON - .TRUE. iff boundary faces are constrained (i.e. not
!    swapped by local transformations).
!
!    Input, NPT - number of 3D vertices (points).
!
!    Input, SIZHT - size of hash table HT; a good choice is a prime number
!    which is about 1/8 * NFACE (or 3/2 * NPT for random
!    points from the uniform distribution).
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:NPT) - indices of vertices of VCL being triangulated.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input/output, NTETRA - number of tetrahedra in triangulation.
!
!    Input, HDAVFC - head pointer to available FC records.
!
!    Input/output, BF(1:3,1:*) -  array of boundary face records; see DTRIS3.
!
!    Input/output, FC(1:7,1:MAXFC) - array of face records; see routine DTRIS3.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfc
  integer npt
  integer sizht
!
  integer a
  integer aa
  integer b
  integer back
  integer bb
  integer bf(3,*)
  logical bndcon
  integer c
  integer c2
  integer cc
  double precision center(3)
  double precision ccradi
  integer d
  integer e
  integer f
  integer fc(7,maxfc)
  logical first
  integer front
  integer g
  integer h
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrc
  integer ibeg
  integer ierr
  integer in
  integer ind
  integer ind0
  integer ind1
  integer ind2
  integer ind3
  integer j
  double precision maxaft
  double precision maxbef
  integer, parameter :: msglvl = 0
  double precision mu1
  double precision mu2
  double precision mu3
  double precision mu4
  double precision mu5
  double precision mu6
  double precision mu7
  double precision mu8
  integer nfc
  integer ntetra
  double precision nu1
  double precision nu2
  double precision nu3
  double precision nu4
  double precision nu5
  double precision nu6
  double precision nu7
  logical t1
  logical t2
  double precision tol
  double precision tolp1
  integer top
  character ( len = 3 ) type
  character ( len = 3 ) typ2
  double precision vcl(3,*)
  integer va
  integer vb
  integer vc
  integer vd
  integer ve
  integer vf
  integer vg
  integer vh
  integer vm(npt)
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )

  if (msglvl == 4) then
    write ( *,600)
  end if

  front = 0
  back = 0
  ibeg = 1
  ind = 1
  tolp1 = 1.0d0 + tol

10 continue

  if (fc(1,ind) <= 0 .or. fc(5,ind) <= 0) go to 70

  call ifacty(ind,npt,sizht,vcl,vm,fc,ht,type,a,b,c,ierr)
  if (ierr /= 0) return

  if (type /= 'n32' .and. type /= 'n44') go to 70
  d = fc(4,ind)
  e = fc(5,ind)
  va = vm(a)
  vb = vm(b)
  vc = vm(c)
  vd = vm(d)
  ve = vm(e)

  call ccsph(.true.,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd), &
    vcl(1,ve),center,mu1,in)

  if (in == 2) then
    ierr = 301
    return
  end if

  if (in <= 0) go to 70

  if (msglvl == 4) then
    write ( *,610) type,ind,a,b,c,d,e
  end if

  ind1 = htsrc(a,b,d,npt,sizht,fc,ht)
  if (ind1 <= 0) go to 80

  if (fc(4,ind1) == c) then
    f = fc(5,ind1)
  else
    f = fc(4,ind1)
  end if

  ind2 = htsrc(a,b,f,npt,sizht,fc,ht)
  if (ind2 <= 0) go to 80
  mu1 = 0.0625d0/mu1
  if (type == 'n44') go to 20
!
!  TYPE == 'N32'
!
  if (fc(4,ind2) /= e .and. fc(5,ind2) /= e) go to 70
  call ifacty(ind2,npt,sizht,vcl,vm,fc,ht,type,aa,bb,cc,ierr)

  if (msglvl == 4) then
    write ( *,620) aa,bb,cc,d,e,type
  end if

  if (type /= 't23' .and. type /= 't32' .and. type /= 't44' &
    .and. type /= 'n32') go to 70

  vf = vm(f)
  mu2 = ccradi(vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,ve))
  mu3 = ccradi(vcl(1,va),vcl(1,vb),vcl(1,vf),vcl(1,vd))
  mu4 = ccradi(vcl(1,va),vcl(1,vb),vcl(1,vf),vcl(1,ve))
  nu1 = ccradi(vcl(1,va),vcl(1,vc),vcl(1,vd),vcl(1,ve))
  nu2 = ccradi(vcl(1,vb),vcl(1,vc),vcl(1,vd),vcl(1,ve))
  maxbef = max(mu1,mu2,mu3,mu4)

  if (type == 't23') then

    nu3 = ccradi(vcl(1,va),vcl(1,vf),vcl(1,vd),vcl(1,ve))
    nu4 = ccradi(vcl(1,vb),vcl(1,vf),vcl(1,vd),vcl(1,ve))
    if (max(nu1,nu2,nu3,nu4) <= maxbef*tolp1) go to 70
    top = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0
    go to 60

  else if (type == 't32') then

    if (cc == a) then
      call i_swap ( va, vb )
    end if

    mu5 = ccradi(vcl(1,vf),vcl(1,va),vcl(1,vd),vcl(1,ve))
    nu3 = ccradi(vcl(1,vf),vcl(1,vb),vcl(1,vd),vcl(1,ve))
    if (max(nu1,nu2,nu3) <= max(maxbef,mu5)*tolp1) go to 70
    top = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0
    go to 60

  else if (type == 't44') then

    if (cc == a) then
      call i_swap ( a, b )
      call i_swap ( va, vb )
    end if

    ind1 = htsrc(a,d,f,npt,sizht,fc,ht)
    if (ind1 <= 0) go to 80

    if (fc(4,ind1) == b) then
      g = fc(5,ind1)
    else
      g = fc(4,ind1)
    end if

    vg = vm(g)
    mu5 = ccradi(vcl(1,va),vcl(1,vf),vcl(1,vg),vcl(1,vd))
    mu6 = ccradi(vcl(1,va),vcl(1,vf),vcl(1,vg),vcl(1,ve))
    nu3 = ccradi(vcl(1,vf),vcl(1,vb),vcl(1,vd),vcl(1,ve))
    nu4 = ccradi(vcl(1,va),vcl(1,vg),vcl(1,vd),vcl(1,ve))
    nu5 = ccradi(vcl(1,vf),vcl(1,vg),vcl(1,vd),vcl(1,ve))

    if (max(nu1,nu2,nu3,nu4,nu5) <= max(maxbef,mu5,mu6)*tolp1) then
      go to 70
    end if

    top = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0
    go to 60

  else

    if (cc == a) then
      call i_swap ( a, b )
      call i_swap ( va, vb )
    end if

    ind1 = htsrc(a,f,d,npt,sizht,fc,ht)
    if (ind1 <= 0) go to 80

    if (fc(4,ind1) == b) then
      g = fc(5,ind1)
    else
      g = fc(4,ind1)
    end if

    ind1 = htsrc(a,f,g,npt,sizht,fc,ht)
    if (ind1 <= 0) go to 80
    if (fc(4,ind1) /= e .and. fc(5,ind1) /= e) go to 70
    call ifacty(ind1,npt,sizht,vcl,vm,fc,ht,type,aa,bb,cc,ierr)
    if (type /= 't23' .and. type /= 't32' .and. type /= &
         'n32') go to 70
    vg = vm(g)
    mu5 = ccradi(vcl(1,va),vcl(1,vf),vcl(1,vg),vcl(1,vd))
    mu6 = ccradi(vcl(1,va),vcl(1,vf),vcl(1,vg),vcl(1,ve))
    nu3 = ccradi(vcl(1,vb),vcl(1,vd),vcl(1,ve),vcl(1,vf))

    if (type == 't23') then

      maxbef = max(maxbef,mu5,mu6)
      nu4 = ccradi(vcl(1,va),vcl(1,vg),vcl(1,vd),vcl(1,ve))
      nu5 = ccradi(vcl(1,vf),vcl(1,vg),vcl(1,vd),vcl(1,ve))
      if (max(nu1,nu2,nu3,nu4,nu5) <= maxbef*tolp1) go to 70
      top = ind1

    else if (type == 't32') then

      if (cc == a) then
        call i_swap ( va, vf )
      end if

      mu7 = ccradi(vcl(1,va),vcl(1,vg),vcl(1,vd),vcl(1,ve))
      maxbef = max(maxbef,mu5,mu6,mu7)
      nu4 = ccradi(vcl(1,vf),vcl(1,vg),vcl(1,vd),vcl(1,ve))
      if (max(nu1,nu2,nu3,nu4) <= maxbef*tolp1) go to 70
      top = ind1

    else

      if (g == c) go to 70

      if (cc == a) then
        call i_swap ( a, f )
        call i_swap ( va, vf )
      end if

      ind0 = htsrc(a,g,d,npt,sizht,fc,ht)
      if (ind0 <= 0) go to 80

      if (fc(4,ind0) == f) then
        h = fc(5,ind0)
      else
        h = fc(4,ind0)
      end if

      ind0 = htsrc(a,g,h,npt,sizht,fc,ht)
      if (ind0 <= 0) go to 80
      if (fc(4,ind0) /= e .and. fc(5,ind0) /= e) go to 70
      call ifacty(ind0,npt,sizht,vcl,vm,fc,ht,type,aa,bb,cc,ierr)
      if (type /= 't23') go to 70
      vh = vm(h)
      mu7 = ccradi(vcl(1,va),vcl(1,vg),vcl(1,vh),vcl(1,vd))
      mu8 = ccradi(vcl(1,va),vcl(1,vg),vcl(1,vh),vcl(1,ve))
      maxbef = max(maxbef,mu5,mu6,mu7,mu8)
      nu4 = ccradi(vcl(1,vf),vcl(1,vg),vcl(1,vd),vcl(1,ve))
      nu5 = ccradi(vcl(1,va),vcl(1,vh),vcl(1,vd),vcl(1,ve))
      nu6 = ccradi(vcl(1,vg),vcl(1,vh),vcl(1,vd),vcl(1,ve))
      if (max(nu1,nu2,nu3,nu4,nu5,nu6) <= maxbef*tolp1) then
        go to 70
      end if

      top = ind0
      fc(7,ind0) = ind1

    end if

    fc(7,ind1) = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0
    go to 60

  end if
!
!  TYPE == 'N44'
!
20 continue

  ind3 = ind2

  if (fc(4,ind3) == d) then
    g = fc(5,ind3)
  else
    g = fc(4,ind3)
  end if

  ind2 = htsrc(a,b,g,npt,sizht,fc,ht)
  if (ind2 <= 0) go to 80
  if (fc(4,ind2) /= e .and. fc(5,ind2) /= e) go to 70
  call ifacty(ind2,npt,sizht,vcl,vm,fc,ht,type,aa,bb,cc,ierr)

  if (msglvl == 4) then
    write ( *,620) aa,bb,cc,e,f,type
  end if

  t1 = (type == 't23' .or. type == 't32' .or. type =='t44')
  call ifacty(ind3,npt,sizht,vcl,vm,fc,ht,typ2,aa,bb,c2,ierr)

  if (msglvl == 4) then
    write ( *,620) aa,bb,c2,d,g,typ2
  end if

  t2 = (typ2 == 't23' .or. typ2 == 't32' .or. typ2 =='t44')
  if (.not. t1 .and. .not. t2) go to 70
  first = .true.

30 continue

  if ( .not. t1 ) then

    ind2 = ind3
    type = typ2
    cc = c2
    call i_swap ( d, e )
    call i_swap ( vd, ve )
    call i_swap ( f, g )

  end if

  vf = vm(f)
  vg = vm(g)

  if (first) then
    mu2 = ccradi(vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,ve))
    mu3 = ccradi(vcl(1,va),vcl(1,vb),vcl(1,vf),vcl(1,vd))
    mu4 = ccradi(vcl(1,va),vcl(1,vb),vcl(1,vg),vcl(1,ve))
    mu5 = ccradi(vcl(1,va),vcl(1,vb),vcl(1,vg),vcl(1,vf))
    nu1 = ccradi(vcl(1,va),vcl(1,vc),vcl(1,vd),vcl(1,ve))
    nu2 = ccradi(vcl(1,vb),vcl(1,vc),vcl(1,vd),vcl(1,ve))
    nu3 = ccradi(vcl(1,va),vcl(1,vf),vcl(1,vd),vcl(1,ve))
    nu4 = ccradi(vcl(1,vb),vcl(1,vf),vcl(1,vd),vcl(1,ve))
  else
    nu3 = ccradi(vcl(1,va),vcl(1,vf),vcl(1,vd),vcl(1,ve))
    nu4 = ccradi(vcl(1,vb),vcl(1,vf),vcl(1,vd),vcl(1,ve))
  end if

  if (first) then
    maxbef = max(mu1,mu2,mu3,mu4,mu5)
    maxaft = max(nu1,nu2,nu3,nu4)
  else
    maxaft = max(nu1,nu2,nu3,nu4)
  end if

  if (type == 't23') then

    nu5 = ccradi(vcl(1,va),vcl(1,vg),vcl(1,ve),vcl(1,vf))
    nu6 = ccradi(vcl(1,vb),vcl(1,vg),vcl(1,ve),vcl(1,vf))
    if (max(maxaft,nu5,nu6) <= maxbef*tolp1) go to 50
    top = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0
    go to 60

  else if (type == 't32') then

    if (cc == a) then
      call i_swap ( a, b )
      call i_swap ( va, vb )
    end if

    mu6 = ccradi(vcl(1,va),vcl(1,vg),vcl(1,ve),vcl(1,vf))
    nu5 = ccradi(vcl(1,vb),vcl(1,vg),vcl(1,ve),vcl(1,vf))
    if (max(maxaft,nu5) <= max(maxbef,mu6)*tolp1) go to 50
    top = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0
    go to 60

  else

    if (cc == a) then
      call i_swap ( a, b )
      call i_swap ( va, vb )
    end if

    ind1 = htsrc(a,e,g,npt,sizht,fc,ht)
    if (ind1 <= 0) go to 80

    if (fc(4,ind1) == b) then
      h = fc(5,ind1)
    else
      h = fc(4,ind1)
    end if

    vh = vm(h)
    mu6 = ccradi(vcl(1,va),vcl(1,vg),vcl(1,vh),vcl(1,ve))
    mu7 = ccradi(vcl(1,va),vcl(1,vg),vcl(1,vh),vcl(1,vf))
    nu5 = ccradi(vcl(1,vg),vcl(1,vb),vcl(1,ve),vcl(1,vf))
    nu6 = ccradi(vcl(1,va),vcl(1,vh),vcl(1,ve),vcl(1,vf))
    nu7 = ccradi(vcl(1,vg),vcl(1,vh),vcl(1,ve),vcl(1,vf))

    if (max(maxaft,nu5,nu6,nu7) <= max(maxbef,mu6,mu7)*tolp1) then
      go to 50
    end if

    top = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0
    go to 60

  end if

50 continue

  if (t1 .and. t2) then
    t1 = .false.
    first = .false.
    go to 30
  else
    go to 70
  end if

60 continue

  if (msglvl == 4) then
    write ( *,630) 'combination swaps made'
  end if

  call swaptf(top,npt,sizht,nfc,maxfc,vcl,vm,fc,ht,ntetra,hdavfc, &
    front,back, ierr )
  if (ierr /= 0) return

  call swapes(bndcon,0,npt,sizht,nfc,maxfc,vcl,vm,bf,fc,ht, &
    ntetra,hdavfc,front,back,j, ierr )

  if (ierr /= 0) return

  ind = ind + 1
  if (ind > nfc) ind = 1
  ibeg = ind
  go to 10

70 continue

  ind = ind + 1
  if (ind > nfc) ind = 1
  if (ind /= ibeg) go to 10
  return

80 continue

  ierr = 300

  600 format (/1x,'imptrd')
  610 format (1x,'type ',a3,i7,' : ',5i7)
  620 format (4x,'face',3i7,' | ',2i7,' has type ',a3)
  630 format (4x,a)

  return
end
subroutine imptrf ( bndcon, crit, npt, sizht, maxfc, vcl, vm, nfc, ntetra, &
  hdavfc, bf, fc,ht, ierr )
!
!******************************************************************************
!
!! IMPTRF improves a given triangulation in 3D.
!
!
!  Purpose: 
!
!    Further improve a given 3D triangulation by applying
!    local transformations based on a local criterion.  Combination
!    swaps are used to remove poorly shaped tetrahedra.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Discussion:
!
!    BF, FC, HT should be as output by DTRIS3 or DTRIW3,
!    except it is assumed FC(7,1:2) don't contain HDAVBF, HDAVFC.
!
!  Parameters:
!
!    Input, BNDCON - .TRUE. iff boundary faces are constrained (i.e. not
!    swapped by local transformations).
!
!    Input, CRIT - criterion code; 1 for (local max-min) solid angle
!    criterion, 2 for radius ratio criterion, 3 for mean ratio
!    criterion, 0 (or anything else) for no swaps.
!
!    Input, NPT - number of 3D vertices (points).
!
!    Input, SIZHT - size of hash table HT; a good choice is a prime number
!    which is about 1/8 * NFACE (or 3/2 * NPT for random
!    points from the uniform distribution).
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:NPT) - indices of vertices of VCL being triangulated.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input/output, NTETRA - number of tetrahedra in triangulation.
!
!    Input, HDAVFC - head pointer to available FC records.
!
!    Input/output, BF(1:3,1:*) -  array of boundary face records; see DTRIS3.
!
!    Input/output, FC(1:7,1:MAXFC) - array of face records; see routine DTRIS3.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfc
  integer npt
  integer sizht
!
  integer a
  integer aa
  integer b
  integer back
  integer bb
  integer bf(3,*)
  logical bndcon
  integer c
  integer cc
  integer c2
  integer crit
  integer d
  integer dd
  integer e
  integer ee
  integer f
  integer fc(7,maxfc)
  integer ff
  logical first
  integer front
  integer g
  integer h
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrc
  integer ibeg
  integer ierr
  logical impr
  integer ind
  integer ind1
  integer ind2
  integer ind3
  integer indx
  integer indy
  integer indz
  integer j
  double precision minaft
  double precision minbef
  integer, parameter :: msglvl = 0
  double precision mu1
  double precision mu2
  double precision mu3
  double precision mu4
  double precision mu5
  double precision mu6
  double precision mu7
  integer nfc
  integer ntetra
  double precision nu1
  double precision nu2
  double precision nu3
  double precision nu4
  double precision nu5
  double precision nu6
  double precision nu7
  double precision s(4)
  logical t1
  logical t2
  double precision tetmu
  double precision tol
  integer top
  integer top2
  character ( len = 3 ) typ2
  character ( len = 3 ) type
  double precision vcl(3,*)
  integer va
  integer vb
  integer vc
  integer vd
  integer ve
  integer vf
  integer vg
  integer vh
  integer vm(npt)
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )

  if (msglvl == 4) then
    write ( *,600) crit
  end if

  front = 0
  back = 0
  ibeg = 1
  ind = 1

10 continue

  if (fc(1,ind) <= 0 .or. fc(5,ind) <= 0) go to 70

  call ifacty(ind,npt,sizht,vcl,vm,fc,ht,type,a,b,c,ierr)

  if (ierr /= 0) return
  if (type /= 'n32' .and. type /= 'n44') go to 70
  d = fc(4,ind)
  e = fc(5,ind)
  va = vm(a)
  vb = vm(b)
  vc = vm(c)
  vd = vm(d)
  ve = vm(e)

  if (msglvl == 4) then
    write ( *,610) type,ind,a,b,c,d,e
  end if

  indx = htsrc(a,b,d,npt,sizht,fc,ht)
  if (indx <= 0) go to 80

  if (fc(4,indx) == c) then
    f = fc(5,indx)
  else
    f = fc(4,indx)
  end if

  ind2 = htsrc(a,b,f,npt,sizht,fc,ht)
  if (ind2 <= 0) go to 80
  if (type == 'n44') go to 20
!
!  TYPE == 'N32'
!
  if (fc(4,ind2) /= e .and. fc(5,ind2) /= e) go to 70

  call ifacty(ind2,npt,sizht,vcl,vm,fc,ht,type,aa,bb,cc,ierr)

  if (msglvl == 4) then
    write ( *,620) aa,bb,cc,d,e,type
  end if

  if (type /= 't23' .and. type /= 't32' .and. type /='t44' &
    .and. type /= 'n32') go to 70

  vf = vm(f)
  mu1 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd),s)
  mu2 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,ve),s)
  mu3 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vf),vcl(1,vd),s)
  mu4 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vf),vcl(1,ve),s)
  nu1 = tetmu(crit,vcl(1,va),vcl(1,vc),vcl(1,vd),vcl(1,ve),s)
  nu2 = tetmu(crit,vcl(1,vb),vcl(1,vc),vcl(1,vd),vcl(1,ve),s)
  minbef = min(mu1,mu2,mu3,mu4)

  if (type == 't23') then

    minbef = minbef + tol
    if (min(nu1,nu2) <= minbef) go to 70
    nu3 = tetmu(crit,vcl(1,va),vcl(1,vf),vcl(1,vd),vcl(1,ve),s)
    nu4 = tetmu(crit,vcl(1,vb),vcl(1,vf),vcl(1,vd),vcl(1,ve),s)
    if (max(nu3,nu4) <= minbef) go to 70
    top = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0

    if (min(nu3,nu4) <= minbef) then

      indy = htsrc(a,b,e,npt,sizht,fc,ht)
      if (indy <= 0) go to 80
      top2 = indx
      fc(7,indx) = indy
      fc(7,indy) = 0

      if (nu4 < nu3) then
        call i_swap ( a, b )
      end if

      call fndmsw(crit,npt,sizht,vcl,vm,fc,ht,a,b,d,e,f,minbef, &
        top,top2,impr,ierr)
      if (ierr /= 0) return
      if (.not. impr) go to 70

    end if

    go to 60

  else if (type == 't32') then

    if (cc == a) then
      call i_swap ( va, vb )
    end if

    mu5 = tetmu(crit,vcl(1,vf),vcl(1,va),vcl(1,vd),vcl(1,ve),s)
    nu3 = tetmu(crit,vcl(1,vf),vcl(1,vb),vcl(1,vd),vcl(1,ve),s)
    if (min(nu1,nu2,nu3) <= min(minbef,mu5) + tol) go to 70
    top = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0
    go to 60

  else if (type == 't44') then

    if (cc == a) then
      call i_swap ( a, b )
      call i_swap ( va, vb )
    end if

    ind1 = htsrc(a,d,f,npt,sizht,fc,ht)
    if (ind1 <= 0) go to 80

    if (fc(4,ind1) == b) then
      g = fc(5,ind1)
    else
      g = fc(4,ind1)
    end if

    vg = vm(g)
    mu5 = tetmu(crit,vcl(1,va),vcl(1,vf),vcl(1,vg),vcl(1,vd),s)
    mu6 = tetmu(crit,vcl(1,va),vcl(1,vf),vcl(1,vg),vcl(1,ve),s)
    nu3 = tetmu(crit,vcl(1,vf),vcl(1,vb),vcl(1,vd),vcl(1,ve),s)
    nu4 = tetmu(crit,vcl(1,va),vcl(1,vg),vcl(1,vd),vcl(1,ve),s)
    nu5 = tetmu(crit,vcl(1,vf),vcl(1,vg),vcl(1,vd),vcl(1,ve),s)

    if (min(nu1,nu2,nu3,nu4,nu5) <= min(minbef,mu5,mu6) + tol) then
      go to 70
    end if

    top = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0
    go to 60

  else

    if (cc == a) then
      call i_swap ( a, b )
      call i_swap ( va, vb )
    end if

    ind3 = htsrc(a,f,d,npt,sizht,fc,ht)
    if (ind3 <= 0) go to 80

    if (fc(4,ind3) == b) then
      g = fc(5,ind3)
    else
      g = fc(4,ind3)
    end if

    ind1 = htsrc(a,f,g,npt,sizht,fc,ht)
    if (ind1 <= 0) go to 80
    if (fc(4,ind1) /= e .and. fc(5,ind1) /= e) go to 70
    call ifacty(ind1,npt,sizht,vcl,vm,fc,ht,type,aa,bb,cc,ierr)
    if (type /= 't23') go to 70
    vg = vm(g)
    mu5 = tetmu(crit,vcl(1,va),vcl(1,vf),vcl(1,vg),vcl(1,vd),s)
    mu6 = tetmu(crit,vcl(1,va),vcl(1,vf),vcl(1,vg),vcl(1,ve),s)
    minbef = min(minbef,mu5,mu6) + tol
    nu3 = tetmu(crit,vcl(1,vb),vcl(1,vd),vcl(1,ve),vcl(1,vf),s)
    if (min(nu1,nu2,nu3) <= minbef) go to 70
    nu4 = tetmu(crit,vcl(1,va),vcl(1,vg),vcl(1,vd),vcl(1,ve),s)
    nu5 = tetmu(crit,vcl(1,vf),vcl(1,vg),vcl(1,vd),vcl(1,ve),s)
    if (max(nu4,nu5) <= minbef) go to 70
    top = ind1
    fc(7,ind1) = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0

    if (min(nu4,nu5) <= minbef) then

      indy = htsrc(a,b,e,npt,sizht,fc,ht)
      indz = htsrc(a,f,e,npt,sizht,fc,ht)
      if (indy <= 0 .or. indz <= 0) go to 80
      top2 = indx
      fc(7,indx) = indy
      fc(7,indy) = indz
      fc(7,indz) = ind3
      fc(7,ind3) = 0

      if (nu5 < nu4) then
        call i_swap ( a, f )
      end if

      call fndmsw(crit,npt,sizht,vcl,vm,fc,ht,a,f,d,e,g,minbef, &
        top,top2,impr,ierr)
      if (ierr /= 0) return

      if (.not. impr) go to 70

    end if

    go to 60

  end if
!
!  TYPE == 'N44'
!
20 continue

  ind3 = ind2

  if (fc(4,ind3) == d) then
    g = fc(5,ind3)
  else
    g = fc(4,ind3)
  end if

  ind2 = htsrc(a,b,g,npt,sizht,fc,ht)
  if (ind2 <= 0) go to 80
  if (fc(4,ind2) /= e .and. fc(5,ind2) /= e) go to 70
  call ifacty(ind2,npt,sizht,vcl,vm,fc,ht,type,aa,bb,cc,ierr)

  if (msglvl == 4) then
    write ( *,620) aa,bb,cc,e,f,type
  end if

  t1 = (type == 't23' .or. type == 't32' .or. type =='t44')
  call ifacty(ind3,npt,sizht,vcl,vm,fc,ht,typ2,aa,bb,c2,ierr)

  if (msglvl == 4) then
    write ( *,620) aa,bb,c2,d,g,typ2
  end if

  t2 = (typ2 == 't23' .or. typ2 == 't32' .or. typ2 =='t44')
  if (.not. t1 .and. .not. t2) go to 70
  first = .true.

30 continue

  if (t1) go to 40
  j = ind2
  ind2 = ind3
  ind3 = j
  type = typ2
  cc = c2

  call i_swap ( d, e )
  call i_swap ( vd, ve )
  call i_swap ( f, g )

40 continue

  vf = vm(f)
  vg = vm(g)

  if (first) then
    mu1 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd),s)
    mu2 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,ve),s)
    mu3 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vf),vcl(1,vd),s)
    mu4 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vg),vcl(1,ve),s)
    mu5 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vg),vcl(1,vf),s)
    nu1 = tetmu(crit,vcl(1,va),vcl(1,vc),vcl(1,vd),vcl(1,ve),s)
    nu2 = tetmu(crit,vcl(1,vb),vcl(1,vc),vcl(1,vd),vcl(1,ve),s)
    nu3 = tetmu(crit,vcl(1,va),vcl(1,vf),vcl(1,vd),vcl(1,ve),s)
    nu4 = tetmu(crit,vcl(1,vb),vcl(1,vf),vcl(1,vd),vcl(1,ve),s)
  else
    nu3 = tetmu(crit,vcl(1,va),vcl(1,vf),vcl(1,vd),vcl(1,ve),s)
    nu4 = tetmu(crit,vcl(1,vb),vcl(1,vf),vcl(1,vd),vcl(1,ve),s)
  end if

  if (first) then
    minbef = min(mu1,mu2,mu3,mu4,mu5)
    minaft = min(nu1,nu2,nu3,nu4)
  else
    minaft = min(nu1,nu2,nu3,nu4)
  end if

  if (type == 't23') then

    if (minaft <= minbef + tol) go to 50
    nu5 = tetmu(crit,vcl(1,va),vcl(1,vg),vcl(1,ve),vcl(1,vf),s)
    nu6 = tetmu(crit,vcl(1,vb),vcl(1,vg),vcl(1,ve),vcl(1,vf),s)
    if (max(nu5,nu6) <= minbef + tol) go to 50
    top = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0

    if (min(nu5,nu6) <= minbef + tol) then

      indy = htsrc(a,b,e,npt,sizht,fc,ht)
      if (indy <= 0) go to 80
      top2 = ind3
      fc(7,ind3) = indy
      fc(7,indy) = 0

      if (nu5 <= nu6) then
        aa = a
        bb = b
      else
        aa = b
        bb = a
      end if

      dd = e
      ee = f
      ff = g
      call fndmsw(crit,npt,sizht,vcl,vm,fc,ht,aa,bb,dd,ee,ff, &
        minbef+tol,top,top2,impr,ierr)
      if (ierr /= 0) return
      if (.not. impr) go to 50

    end if

    go to 60

  else if (type == 't32') then

    if (cc == a) then
      call i_swap ( a, b )
      call i_swap ( va, vb )
    end if

    mu6 = tetmu(crit,vcl(1,va),vcl(1,vg),vcl(1,ve),vcl(1,vf),s)
    nu5 = tetmu(crit,vcl(1,vb),vcl(1,vg),vcl(1,ve),vcl(1,vf),s)
    if (min(minaft,nu5) <= min(minbef,mu6) + tol) go to 50
    top = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0
    go to 60

  else

    if (cc == a) then
      call i_swap ( a, b )
      call i_swap ( va, vb )
    end if

    ind1 = htsrc(a,e,g,npt,sizht,fc,ht)
    if (ind1 <= 0) go to 80

    if (fc(4,ind1) == b) then
      h = fc(5,ind1)
    else
      h = fc(4,ind1)
    end if

    vh = vm(h)
    mu6 = tetmu(crit,vcl(1,va),vcl(1,vg),vcl(1,vh),vcl(1,ve),s)
    mu7 = tetmu(crit,vcl(1,va),vcl(1,vg),vcl(1,vh),vcl(1,vf),s)
    nu5 = tetmu(crit,vcl(1,vg),vcl(1,vb),vcl(1,ve),vcl(1,vf),s)
    nu6 = tetmu(crit,vcl(1,va),vcl(1,vh),vcl(1,ve),vcl(1,vf),s)
    nu7 = tetmu(crit,vcl(1,vg),vcl(1,vh),vcl(1,ve),vcl(1,vf),s)

    if (min(minaft,nu5,nu6,nu7) <= min(minbef,mu6,mu7) + tol) then
      go to 50
    end if

    top = ind2
    fc(7,ind2) = ind
    fc(7,ind) = 0
    go to 60

  end if

50 continue

  if (t1 .and. t2) then
    t1 = .false.
    first = .false.
    go to 30
  else
    go to 70
  end if

60 continue

  if (msglvl == 4) then
    write ( *,630) 'combination swaps made'
  end if

  call swaptf(top,npt,sizht,nfc,maxfc,vcl,vm,fc,ht,ntetra,hdavfc, &
    front,back, ierr )

  if (ierr /= 0) return

  call swapmu(bndcon,crit,npt,sizht,nfc,maxfc,vcl,vm,bf,fc,ht, &
    ntetra,hdavfc,front,back,j, ierr )

  if (ierr /= 0) return

  ind = ind + 1
  if (ind > nfc) ind = 1
  ibeg = ind
  go to 10

70  continue

  ind = ind + 1
  if (ind > nfc) ind = 1
  if (ind /= ibeg) go to 10
  return

80 continue

  ierr = 300

  600 format (/1x,'imptrf: criterion =',i3)
  610 format (1x,'type ',a3,i7,' : ',5i7)
  620 format (4x,'face',3i7,' | ',2i7,' has type ',a3)
  630 format (4x,a)

  return
end
subroutine inttri ( nvrt, xc, yc, h, ibot, costh, sinth, ldv, nvc, ntri,  &
  maxvc, maxti, maxcw, vcl, til, ncw, cwalk, ierror )
!
!*******************************************************************************
!
!! INTTRI generates triangles inside a convex polygon.
!
!
!  Purpose:
!
!    Generate triangles inside convex polygon using quasi-uniform grid of
!    spacing H.  It is assumed that the diameter of the polygon is parallel
!    to the Y axis.
!
!  Modified:
!
!    02 May 2001
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, integer NVRT, the number of vertices on the boundary of
!    convex polygon.
!
!    Input, double precision XC(0:NVRT), YC(0:NVRT), the vertex coordinates
!    in counter clockwise order; (XC(0),YC(0)) = (XC(NVRT),YC(NVRT)).
!
!    Input, double precision H, the spacing of mesh vertices in polygon.
!
!    Input, integer IBOT, the index of bottom vertex; diameter contains vertices
!    (XC(0),YC(0)) and (XC(IBOT),YC(IBOT)).
!
!    Input, double precision COSTH, SINTH; COS(THETA), SIN(THETA) where
!    THETA in [-PI,PI] is rotation angle to get diameter parallel to y-axis.
!
!    Input, integer LDV, the leading dimension of VCL in calling routine.
!
!    Input/output, integer NVC, the number of coordinates or positions
!    used in VCL array.
!
!    Input/output, integer NTRI, the number of triangles or positions
!    used in TIL.
!
!    Input, integer MAXVC, the maximum size available for VCL array.
!
!    Input, integer MAXTI, the maximum size available for TIL array.
!
!    Input, integer MAXCW, the maximum size available for CWALK array;
!    assumed to be >= 6*(1 + INT((YC(0) - YC(IBOT))/H)).
!
!    Input/output, double precision VCL(1:2,1:NVC), the vertex coordinate list.
!
!    Input/output, integer TIL(1:3,1:NTRI), the triangle incidence list.
!
!    Output, integer NCW, the number of mesh vertices in closed walk,
!    except NCW = 0 for 1 vertex.
!
!    Output, integer CWALK(0:NCW), indices in VCL of mesh vertices of closed
!    walk; CWALK(0) = CWALK(NCW)
!
!    Output, integer IERROR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer ldv
  integer maxcw
  integer maxti
  integer maxvc
  integer nvrt
!
  double precision a
  double precision b
  double precision costh
  integer cwalk(0:maxcw)
  double precision cy
  double precision h
  integer i
  integer ibot
  integer ierror
  integer il
  integer im1l
  integer im1r
  integer ir
  integer j
  integer k
  integer l
  integer l0
  integer l1
  integer lw
  integer m
  integer n
  integer ncw
  integer ntri
  integer nvc
  integer p
  integer r
  integer r0
  integer r1
  integer rw
  double precision sinth
  double precision sy
  integer til(3,maxti)
  double precision tol
  double precision vcl(ldv,maxvc)
  double precision x
  double precision xc(0:nvrt)
  double precision xj
  double precision xk
  double precision xl
  double precision xm1l
  double precision xm1r
  double precision xr
  double precision y
  double precision yc(0:nvrt)
!
  ierror = 0

  tol = 100.0D+00 * epsilon ( tol )

  n = int ( ( yc(0) - yc(ibot) ) / h )
  y = yc(0) - 0.5D+00 * ( yc(0) - yc(ibot ) - dble ( n ) * h )
  l = 0
  r = nvrt

  do i = 0, n
!
!  Determine left and right x-coordinates of polygon for
!  scan line with y-coordinate Y, and generate mesh vertices.
!
    do while ( yc(l+1) > y )
      l = l + 1
    end do

    do while ( yc(r-1) > y )
      r = r - 1
    end do

    xl = xc(l) + ( xc(l+1) - xc(l) ) * ( y - yc(l) ) / ( yc(l+1) - yc(l) )
    xr = xc(r) + ( xc(r-1) - xc(r) ) * ( y - yc(r) ) / ( yc(r-1) - yc(r) )
    m = int ( ( xr - xl ) / h )
    x = xl + 0.5D+00 * ( xr - xl - dble ( m ) * h )

    if ( nvc + m + 1 > maxvc ) then
      ierror = 3
      return
    end if

    cy = costh * y
    sy = sinth * y
    il = nvc + 1
    xl = x

    do j = 0, m
      nvc = nvc + 1
      vcl(1,nvc) = costh * x + sy
      vcl(2,nvc) = cy - sinth * x
      x = x + h
    end do

    ir = nvc
    xr = x - h

    if ( n == 0 ) then

      ncw = 0
      cwalk(0) = nvc
      return

    else if ( i == 0 ) then

      lw = 0
      cwalk(lw) = il
      rw = maxcw + 1

      do j = il, ir
        rw = rw - 1
        cwalk(rw) = j
      end do

      go to 100

    end if
!
!  Generate triangles between scan lines Y+H and Y.
!
    a = max ( xl, xm1l )
    b = min ( xr, xm1r )

    if ( xm1l == a ) then
      l0 = im1l
      x = ( xm1l - xl ) / h
      j = int(x + tol)
      if ( abs ( x - dble ( j ) ) <= tol ) then
        j = j - 1
      end if
      if ( j < 0 ) then
        j = 0
      end if
      l1 = il + j
    else
      l1 = il
      x = ( xl - xm1l ) / h
      j = int ( x + tol )
      if ( abs ( x - dble ( j ) ) <= tol ) then
        j = j - 1
      end if
      if ( j < 0 ) then
        j = 0
      end if
      l0 = im1l + j
    end if

    if ( xm1r == b ) then
      r0 = im1r
      x = ( xr - xm1r ) / h
      j = int ( x + tol )
      if ( abs ( x - dble ( j ) ) <= tol ) then
        j = j - 1
      end if
      if ( j < 0 ) then
        j = 0
      end if
      r1 = ir - j
    else
      r1 = ir
      x = ( xm1r - xr ) / h
      j = int ( x + tol )
      if ( abs ( x - dble(j) ) <= tol ) then
        j = j - 1
      end if
      if ( j < 0 ) then
        j = 0
      end if
      r0 = im1r - j
    end if

    if ( l0 < r0 .or. l1 < r1 ) then

      j = l0
      k = l1
      xj = xm1l + dble ( j-im1l ) * h
      xk = xl + dble ( k - il ) * h

      do

        if ( k < r1 .and. ( xk <= xj .or. j == r0 ) ) then
          p = k
          k = k + 1
          xk = xk + h
        else
          p = j
          j = j + 1
          xj = xj + h
        end if

        ntri = ntri + 1

        if ( ntri > maxti ) then
          ierror = 9
          return
        end if

        til(1,ntri) = j
        til(2,ntri) = p
        til(3,ntri) = k

        if ( j >= r0 .and. k >= r1 ) then
          exit
        end if

      end do

    end if
!
!  Generate paths of closed walk between scan lines Y+H and Y.
!
    if ( xm1l < xl ) then
      do j = im1l+1, l0
        lw = lw + 1
        cwalk(lw) = j
      end do
      lw = lw + 1
      cwalk(lw) = il
    else
      do j = l1, il, -1
        lw = lw + 1
        cwalk(lw) = j
      end do
    end if

    if ( xm1r > xr ) then
      do j = im1r-1, r0, -1
        rw = rw - 1
        cwalk(rw) = j
      end do
      rw = rw - 1
      cwalk(rw) = ir
    else
      do j = r1, ir
        rw = rw - 1
        cwalk(rw) = j
      end do
    end if

100 continue

    y = y - h
    im1l = il
    im1r = ir
    xm1l = xl
    xm1r = xr

  end do
!
!  Add last path of left walk and shift indices of right walk.
!
  if ( m == 0 ) then
    rw = rw + 1
  else
    do j = il+1, ir-1
      lw = lw + 1
      cwalk(lw) = j
    end do
  end if

  if ( rw <= lw ) then
    ierror = 10
    return
  end if

  do j = rw, maxcw
    lw = lw + 1
    cwalk(lw) = cwalk(j)
  end do

  ncw = lw

  return
end
subroutine insed2 ( v, w, npolg, nvert, maxhv, maxpv, vcl, regnum, hvl, &
  pvl, iang, ierr )
!
!******************************************************************************
!
!! INSED2 inserts an edge into the head and polygon vertex lists.
!
!
!  Purpose: 
!
!    Insert edge joining vertices V, W into head vertex
!    list and polygon vertex list data structures.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, V, W - indices in PVL of vertices which are the endpoints
!    of an edge to be added to decomposition.
!
!    Input, NPOLG - number of positions used in HVL array.
!
!    Input, NVERT - number of positions used in PVL array.
!
!    Input, MAXHV - maximum size available for HVL array.
!
!    Input, MAXPV - maximum size available for PVL array.
!
!    Input, VCL(1:2,1:*) - vertex coordinate list.
!
!    Input/output, REGNUM(1:NPOLG) - region numbers.
!
!    Input/output, HVL(1:NPOLG) - head vertex list.
!
!    Input/output, PVL(1:4,1:NVERT), IANG(1:NVERT) - polygon vertex list 
!    and interior angles.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxhv
  integer maxpv
!
  double precision angle
  integer, parameter :: edgv = 4
  integer hvl(maxhv)
  integer i
  double precision iang(maxpv)
  integer ierr
  integer l
  integer, parameter :: loc = 1
  integer lv
  integer lw
  integer, parameter :: msglvl = 0
  integer npolg
  integer nvert
  integer, parameter :: polg = 2
  integer pvl(4,maxpv)
  integer regnum(maxhv)
  integer, parameter :: succ = 3
  integer v
  double precision vcl(2,*)
  integer vv
  integer w
  integer ww
!
  ierr = 0

  if (npolg >= maxhv) then
    ierr = 4
    return
  else if (nvert+2 > maxpv) then
    ierr = 5
    return
  end if
!
!  Split linked list of vertices of the polygon containing vertices
!  V and W into two linked list of vertices of polygons with common
!  edge joining V and W.
!
  nvert = nvert + 2
  vv = nvert - 1
  ww = nvert
  lv = pvl(loc,v)
  lw = pvl(loc,w)
  pvl(loc,vv) = lv
  pvl(loc,ww) = lw
  pvl(polg,ww) = pvl(polg,v)
  pvl(succ,vv) = pvl(succ,v)
  pvl(succ,ww) = pvl(succ,w)
  pvl(succ,v) = ww
  pvl(succ,w) = vv
  pvl(edgv,vv) = pvl(edgv,v)
  pvl(edgv,ww) = pvl(edgv,w)
  pvl(edgv,v) = w
  pvl(edgv,w) = v
  if (pvl(edgv,vv) > 0) pvl(edgv,pvl(edgv,vv)) = vv
  if (pvl(edgv,ww) > 0) pvl(edgv,pvl(edgv,ww)) = ww
  l = pvl(loc,pvl(succ,vv))
  iang(vv) = angle(vcl(1,lw),vcl(2,lw),vcl(1,lv),vcl(2,lv),vcl(1,l),vcl(2,l))
  iang(v) = iang(v) - iang(vv)
  l = pvl(loc,pvl(succ,ww))
  iang(ww) = angle(vcl(1,lv),vcl(2,lv),vcl(1,lw),vcl(2,lw),vcl(1,l),vcl(2,l))
  iang(w) = iang(w) - iang(ww)
  npolg = npolg + 1
  i = vv

  do

    pvl(polg,i) = npolg
    i = pvl(succ,i)

    if ( i == vv) then
      exit
    end if

  end do

  hvl(pvl(polg,v)) = v
  hvl(npolg) = vv
  regnum(npolg) = regnum(pvl(polg,v))

  if (msglvl == 2) then
    write ( *, '(2i7,4f15.7)' ) v,w,vcl(1,lv),vcl(2,lv), vcl(1,lw),vcl(2,lw)
  end if

  return
end
subroutine insed3 ( a, b, nface, nvert, npf, maxfp, maxfv, maxpf, facep, &
  factyp, nrml, fvl, eang, hfl, pfl, ierr )
!
!******************************************************************************
!
!! INSED3 inserts an edge into the polyhedral decomposition data structure.
!
!
!  Purpose: 
!
!    Insert an edge on a face of polyhedral decomposition data
!    structure.  It is assumed that the edge is entirely inside the face.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input/output, A, B - indices of FVL for nonadjacent vertices on same face.
!    On output, A is the index in FVL of the new edge; LOC field of output
!    A is the same as that of the input A.
!
!    Input/output, NFACE - number of faces or positions used in FACEP array.
!
!    Input/output, NVERT - number of positions used in FVL, EANG arrays.
!
!    Input/output, NPF - number of positions used in PFL array.
!
!    Input, MAXFP - maximum size available for FACEP, FACTYP, NRML arrays.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, MAXPF - maximum size available for PFL array.
!
!    Input/output, FACEP(1:3,1:NFACE) - face pointer list.
!
!    Input/output, FACTYP(1:NFACE) - face types.
!
!    Input/output, NRML(1:3,1:NFACE) - unit normal vectors for faces; outward
!    normal corresponds to counter clockwise traversal of 
!    face from polyhedron with index FACEP(2,F).
!
!    Input/output, FVL(1:6,1:NVERT) - face vertex list.
!
!    Input/output, EANG(1:NVERT) - edge angles.
!
!    Input, HFL(1:*) - head pointer to face indices in PFL for each polyhedron.
!
!    Input/output, PFL(1:2,1:NPF) - list of signed face indices for each
!    polyhedron.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfp
  integer maxfv
  integer maxpf
!
  integer a
  integer b
  double precision eang(maxfv)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer f
  integer facep(3,maxfp)
  integer, parameter :: facn = 2
  integer factyp(maxfp)
  integer fvl(6,maxfv)
  integer g
  integer hfl(*)
  integer i
  integer ierr
  integer j
  integer k
  integer, parameter :: loc = 1
  integer nface
  double precision nrml(3,maxfp)
  integer npf
  integer nvert
  integer pfl(2,maxpf)
  double precision pi
  integer, parameter :: pred = 4
  integer sp
  integer sq
  integer, parameter :: succ = 3
  double precision tol
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )

  f = fvl(facn,a)
  i = nvert + 1
  j = nvert + 2
  nvert = j

  if (nvert > maxfv) then
    ierr = 15
    return
  end if

  fvl(loc,i) = fvl(loc,a)
  fvl(succ,i) = b
  k = fvl(pred,a)
  fvl(pred,i) = k
  fvl(succ,k) = i
  fvl(edga,i) = j
  fvl(edgc,i) = j
  fvl(loc,j) = fvl(loc,b)
  fvl(facn,j) = f
  fvl(succ,j) = a
  k = fvl(pred,b)
  fvl(pred,j) = k
  fvl(succ,k) = j
  fvl(edga,j) = i
  fvl(edgc,j) = i
  fvl(pred,a) = j
  fvl(pred,b) = i
  eang(i) = pi()
  eang(j) = pi()
  nface = nface + 1

  if (nface > maxfp) then
    ierr = 16
    return
  end if

  facep(1,f) = a
  sp = facep(2,f)
  sq = facep(3,f)
  facep(1,nface) = b
  facep(2,nface) = sp
  facep(3,nface) = sq
  factyp(nface) = factyp(f)
  nrml(1,nface) = nrml(1,f)
  nrml(2,nface) = nrml(2,f)
  nrml(3,nface) = nrml(3,f)
  k = b

10 continue

  fvl(facn,k) = nface
  k = fvl(succ,k)
  if (k /= b) go to 10

  g = hfl(abs(sp))
  npf = npf + 1

  if (npf > maxpf) then
    ierr = 17
    return
  end if

  pfl(1,npf) = sign(nface,sp)
  pfl(2,npf) = pfl(2,g)
  pfl(2,g) = npf

  if (sq /= 0) then
    g = hfl(abs(sq))
    npf = npf + 1
    if (npf > maxpf) then
      ierr = 17
      return
    end if
    pfl(1,npf) = sign(nface,sq)
    pfl(2,npf) = pfl(2,g)
    pfl(2,g) = npf
  else
    if ((fvl(loc,b) - fvl(loc,a))*sp > 0) then
      fvl(edga,i) = 0
      fvl(edgc,j) = 0
      eang(j) = -1.0d0
    else
      fvl(edga,j) = 0
      fvl(edgc,i) = 0
      eang(i) = -1.0d0
    end if
  end if

  a = i

  return
end
subroutine inseh3 ( a, b, nvert, maxfv, facep, fvl, eang, ierr )
!
!******************************************************************************
!
!! INSEH3 inserts an edge into the polyhedral decomposition data structure.
!
!
!  Purpose: 
!
!    Insert an edge on a face of polyhedral decomposition data
!    structure that joins a hole on face to outer boundary polygon
!    of face.  It is assumed that edge is entirely inside face.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A, B - indices of FVL for vertices on same face, one on hole and
!    the other on outer boundary.
!
!    Input/output, NVERT - number of positions used in FVL, EANG arrays.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, FACEP(1:3,1:*) - face pointer list.
!
!    Input/output, FVL(1:6,1:NVERT) - face vertex list.
!
!    Input/output, EANG(1:NVERT) - edge angles.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfv
!
  integer a
  integer b
  double precision eang(maxfv)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer f
  integer facep(3,*)
  integer, parameter :: facn = 2
  integer fvl(6,maxfv)
  integer i
  integer ierr
  integer j
  integer k
  integer, parameter :: loc = 1
  integer nvert
  double precision pi
  integer, parameter :: pred = 4
  integer sp
  integer sq
  integer, parameter :: succ = 3
  double precision tol
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  f = fvl(facn,a)
  i = nvert + 1
  j = nvert + 2
  nvert = j

  if (nvert > maxfv) then
    ierr = 15
    return
  end if

  fvl(loc,i) = fvl(loc,a)
  fvl(facn,i) = f
  fvl(succ,i) = b
  k = fvl(pred,a)
  fvl(pred,i) = k
  fvl(succ,k) = i
  fvl(edga,i) = j
  fvl(edgc,i) = j
  fvl(loc,j) = fvl(loc,b)
  fvl(facn,j) = f
  fvl(succ,j) = a
  k = fvl(pred,b)
  fvl(pred,j) = k
  fvl(succ,k) = j
  fvl(edga,j) = i
  fvl(edgc,j) = i
  fvl(pred,a) = j
  fvl(pred,b) = i
  eang(i) = pi()
  eang(j) = pi()
  sp = facep(2,f)
  sq = facep(3,f)

  if (sq == 0) then

    if ((fvl(loc,b) - fvl(loc,a))*sp > 0) then
      fvl(edga,i) = 0
      fvl(edgc,j) = 0
      eang(j) = -1.0d0
    else
      fvl(edga,j) = 0
      fvl(edgc,i) = 0
      eang(i) = -1.0d0
    end if

  end if

  return
end
subroutine insfac ( p, nrmlc, nce, cedge, cdang, nvc, nface, nvert, npolh, &
  npf, maxfp, maxfv, maxhf, maxpf, vcl, facep, factyp, nrml, fvl, eang, &
  hfl, pfl, ierr )
!
!******************************************************************************
!
!! INSFAC inserts a new cut face into a polyhedral decomposition.
!
!
!  Purpose: 
!
!    Insert a new face (cut face) in polyhedral decomposition
!    data structure. It is assumed that interior of face does not
!    intersect any other faces.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, P - polyhedron index.
!
!    Input, NRMLC(1:3) - unit normal vector of cut plane.
!
!    Input, NCE - number of edges in cut face.
!
!    Input/output, CEDGE(1:2,0:NCE).  On input, CEDGE(1,I) is an index of 
!    VCL, indices > NVC are new points; CEDGE(2,I) = J indicates that edge 
!    of cut face ending at CEDGE(1,I) is edge from J to FVL(SUCC,J)
!    if J > 0; else if J < 0 then edge of cut face ending at
!    CEDGE(1,I) is a new edge and CEDGE(1,I) lies on edge from
!    -J to FVL(SUC,-J) and new edge lies in face FVL(FACN,-J);
!    CEDGE(2,I) always refers to an edge in the subpolyhedron
!    in negative half-space; CEDGE(1,NCE) = CEDGE(1,0);
!    CEDGE(2,0) is not input but is used temporarily.
!    On output, CEDGE(1:1,1:NCE) is updated to edges of cut face with respect to
!    positive half-space, CEDGE(2:2,1:NCE) has negative entries updated to 
!    index of new edge.
!
!    Input, CDANG(1:NCE) - dihedral angles created by edges of cut polygon
!    in positive half-space; negative sign for angle I indicates that
!    face containing edge I is oriented CW in polyhedron P.
!
!    Input/output, NVC - number of vertex coordinates (excluding new ones)).
!
!    Input/output, NFACE - number of faces or positions used in FACEP array.
!
!    Input/output, NVERT - number of positions used in FVL, EANG arrays.
!
!    Input/output, NPOLH - number of polyhedra or positions used in HFL array.
!
!    Input/output, NPF - number of positions used in PFL array.
!
!    Input, MAXFP - maximum size available for FACEP, FACTYP, NRML arrays.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, MAXHF - maximum size available for HFL array.
!
!    Input, MAXPF - maximum size available for PFL array.
!
!    Input, VCL(1:3,1:NVC+?) - vertex coordinate list; the new vertices to
!    be inserted as indicated by CEDGE are after column NVC.
!
!    Input/output, FACEP(1:3,1:NFACE) - face pointer list.
!
!    Input/output, FACTYP(1:NFACE) - face types.
!
!    Input/output, NRML(1:3,1:NFACE) - unit normal vectors for faces.
!
!    Input/output, FVL(1:6,1:NVERT) - face vertex list.
!
!    Input/output, EANG(1:NVERT) - edge angles.
!
!    Input/output, HFL(1:NPOLH) - head pointer to face indices in PFL for 
!    each polyhedron.
!
!    Input/output, PFL(1:2,1:NPF) - list of signed face indices for each
!    polyhedron.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfp
  integer maxfv
  integer maxhf
  integer nce
!
  integer a
  double precision ang
  integer b
  integer c
  double precision cdang(nce)
  integer cedge(2,0:nce)
  logical docf
  logical dof
  double precision eang(maxfv)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer f
  integer facep(3,maxfp)
  integer, parameter :: facn = 2
  integer factyp(maxfp)
  integer fp
  integer fvl(6,maxfv)
  integer g
  integer head
  integer hfl(maxhf)
  integer i
  integer ierr
  integer j
  integer k
  integer la
  integer lb
  integer, parameter :: loc = 1
  integer maxpf
  integer, parameter :: msglvl = 0
  integer nface
  integer np1
  integer npf
  integer npolh
  double precision nrml(3,maxfp)
  double precision nrmlc(3)
  integer nv
  integer nvc
  integer nvert
  integer p
  integer pfl(2,maxpf)
  integer pind
  integer, parameter :: pred = 4
  integer ptr
  integer sf
  integer sp
  integer, parameter :: succ = 3
  integer tailn
  integer tailp
  integer tailt
  double precision vcl(3,*)
!
!  Insert new vertices and update CEDGE(2,*).
!
  ierr = 0

  do i = 0,nce-1

    if (cedge(1,i) <= nvc) then
      cycle
    end if

    if (i == 0) then
      j = nce
    else
      j = i
    end if

    a = -cedge(2,j)
    call insvr3(a,nvc,nvert,maxfv,vcl,fvl,eang,ierr)
    if (ierr /= 0) return

    if (cdang(j) < 0.0d0) then
      cedge(2,j) = -fvl(succ,a)
    end if

  end do
!
!  Insert new edges and update CEDGE(2,*).
!
  cedge(2,0) = cedge(2,nce)

  do i = 1,nce

    b = -cedge(2,i)

    if (b < 0) then
      cycle
    end if

    f = fvl(facn,b)
    la = cedge(1,i-1)
    a = cedge(2,i-1)
!
!  This can only occur if I = 1.
!
    if (a < 0) then

      a = -a

      if (fvl(loc,a) == la) then
        a = fvl(pred,a)
      else
        a = fvl(succ,a)
      end if

    end if

20  continue

    if (fvl(loc,a) == la) then
      a = fvl(pred,a)
      j = la - fvl(loc,a)
      sf = p
    else
      a = fvl(succ,a)
      j = fvl(loc,fvl(succ,a)) - la
      sf = -p
    end if

    if (j*sf > 0) then
      a = fvl(edgc,a)
    else
      a = fvl(edga,a)
    end if

    fp = fvl(facn,a)

    if (fp /= f) go to 20

    if (fvl(loc,a) == la) then
      j = a
      a = fvl(succ,b)
      b = j
    else
      a = fvl(succ,a)
    end if

    call insed3(a,b,nface,nvert,npf,maxfp,maxfv,maxpf,facep, &
      factyp,nrml,fvl,eang,hfl,pfl,ierr)

    if (ierr /= 0) return
    cedge(2,i) = a

  end do
!
!  Insert cut face into decomposition data structure. Subpolyhedron
!  in negative half space is numbered P, other is numbered NPOLH.
!
  nface = nface + 1
  npolh = npolh + 1
  npf = npf + 2

  if (nvert + nce > maxfv) then
    ierr = 15
    return
  else if (nface > maxfp) then
    ierr = 16
    return
  else if (npf > maxpf) then
    ierr = 17
    return
  else if (npolh > maxhf) then
    ierr = 18
    return
  end if

  nv = nvert
  facep(1,nface) = nvert + 1
  facep(2,nface) = p
  facep(3,nface) = -npolh
  factyp(nface) = 0
  nrml(1,nface) = nrmlc(1)
  nrml(2,nface) = nrmlc(2)
  nrml(3,nface) = nrmlc(3)

  do i = 0,nce-1
    nvert = nvert + 1
    fvl(loc,nvert) = cedge(1,i)
    fvl(facn,nvert) = nface
    fvl(succ,nvert) = nvert + 1
    fvl(pred,nvert) = nvert - 1
  end do

  fvl(succ,nvert) = facep(1,nface)
  fvl(pred,facep(1,nface)) = nvert
!
!  Set CEDGE(1,*) to edges of face in polyhedron NPOLH (after split). New
!  face is counter clockwise from outside new P which contains edges 
!  of CEDGE(2,*).
!
  do i = 1,nce
    a = cedge(2,i)
    la = fvl(loc,a)
    lb = fvl(loc,fvl(succ,a))
    if ((lb - la)*cdang(i) > 0.0d0) then
      cedge(1,i) = fvl(edgc,a)
    else
      cedge(1,i) = fvl(edga,a)
    end if
  end do
!
!  Determine which faces of old P belong to new P or other new polyhedron,
!  and update HFL, PFL.  NP1 is used as a temporary polyhedron index.
!  Faces of old P are put into FACEP(2,*) field.
!  FACTYP(F) is set to -1 for double occurring faces.
!
  dof = .false.
  np1 = npolh + 1
  j = hfl(p)

60 continue

  f = abs(pfl(1,j))

  if (abs(facep(2,f)) == abs(facep(3,f))) then
    facep(2,f) = np1
    facep(3,f) = -np1
    factyp(f) = -1
    dof = .true.
  else if (abs(facep(2,f)) == p) then
    facep(2,f) = sign(np1,facep(2,f))
  else
    i = facep(2,f)
    facep(2,f) = sign(np1,facep(3,f))
    facep(3,f) = i
    nrml(1,f) = -nrml(1,f)
    nrml(2,f) = -nrml(2,f)
    nrml(3,f) = -nrml(3,f)
  end if

  j = pfl(2,j)

  if (j /= hfl(p)) go to 60

  do i = 1,nce

    j = 2
    f = fvl(facn,cedge(1,i))

    if (factyp(f) == -1) then
      if (fvl(loc,cedge(1,i)) /= fvl(loc,nv+i)) j = 3
    end if

    facep(j,f) = sign(npolh,facep(j,f))
    j = 2
    f = fvl(facn,cedge(2,i))

    if (factyp(f) == -1) then
      if (cdang(i) < 0.0d0) j = 3
    end if

    facep(j,f) = sign(p,facep(j,f))

  end do

  pfl(1,npf-1) = nface
  pfl(1,npf) = -nface
  tailp = npf - 1
  tailn = npf
  tailt = hfl(p)
  ptr = pfl(2,tailt)
  pfl(2,tailt) = 0
  hfl(p) = npf - 1
  hfl(npolh) = npf

80 continue

  if (ptr == 0) go to 110
  j = ptr
  sp = pfl(1,ptr)
  f = abs(sp)
  ptr = pfl(2,ptr)

  if (factyp(f) /= -1 .or. sp > 0) then
    k = 2
  else
    k = 3
  end if

  sf = facep(k,f)

  if (abs(sf) == p) then

    pfl(2,tailp) = j
    tailp = j

  else if (abs(sf) == npolh) then

    pfl(2,tailn) = j
    tailn = j

  else

    a = facep(1,f)
    la = fvl(loc,a)

90  continue

    b = fvl(succ,a)
    lb = fvl(loc,b)

    if ((lb - la)*sp > 0) then
      c = fvl(edgc,a)
    else
      c = fvl(edga,a)
    end if

    g = fvl(facn,c)
    i = 2

    if (factyp(g) == -1) then
      if (fvl(loc,c) == la) then
        if (sp > 0) i = 3
      else
        if (sp < 0) i = 3
      end if
    end if

    if (abs(facep(i,g)) == p) then
      pfl(2,tailp) = j
      tailp = j
      facep(k,f) = sign(p,facep(k,f))
      go to 100
    else if (abs(facep(i,g)) == npolh) then
      pfl(2,tailn) = j
      tailn = j
      facep(k,f) = sign(npolh,facep(k,f))
      go to 100
    end if

    a = b
    la = lb
    if (a /= facep(1,f)) go to 90

    pfl(2,tailt) = j
    pfl(2,j) = 0
    tailt = j

100 continue

  end if

  go to 80

110 continue

  pfl(2,tailp) = npf - 1
  pfl(2,tailn) = npf
!
!  Check whether cut face occurs twice in same polyhedron.
!  Temporarily modify PRED field of edges of cut face.
!
  docf = .false.

  do i = 1,nce
    do j = 1,2
      a = cedge(j,i)
      fvl(pred,a) = -fvl(pred,a)
    end do
  end do

  do i = 1,2

    if (i == 1) then
      head = npf - 1
      pind = p
    else
      head = npf
      pind = npolh
    end if

    ptr = pfl(2,head)

140 continue

    sf = pfl(1,ptr)
    f = abs(sf)
    a = facep(1,f)
    la = fvl(loc,a)

150 continue

    b = fvl(succ,a)
    lb = fvl(loc,b)

    if (fvl(pred,a) > 0) then

      if ((lb - la)*sf > 0) then
        c = fvl(edgc,a)
      else
        c = fvl(edga,a)
      end if

      g = fvl(facn,c)
      k = 2

      if (factyp(g) == -1) then
        if (fvl(loc,c) == la) then
          if (sf > 0) k = 3
        else
          if (sf < 0) k = 3
        end if
      end if

      if (abs(facep(k,g)) /= pind) then
        docf = .true.
        go to 170
      end if

    end if

    a = b
    la = lb
    if (a /= facep(1,f)) go to 150

    ptr = pfl(2,ptr)
    if (ptr /= head) go to 140

  end do
!
!  Reset PRED field of edges of cut face.
!
170 continue

  do i = 1,nce
    do j = 1,2
      a = cedge(j,i)
      fvl(pred,a) = -fvl(pred,a)
    end do
  end do
!
!  Update EDGA, EDGC, and EANG fields.
!
  do i = 1,nce

    a = cedge(2,i)
    c = cedge(1,i)
    la = fvl(loc,a)
    lb = fvl(loc,fvl(succ,a))
    ang = abs(cdang(i))

    if ((lb - la)*cdang(i) > 0.0d0) then
      fvl(edgc,a) = nv + i
      fvl(edga,c) = nv + i
      fvl(edgc,nv+i) = c
      fvl(edga,nv+i) = a
      eang(a) = eang(a) - ang
      eang(nv+i) = ang
    else
      fvl(edgc,c) = nv + i
      fvl(edga,a) = nv + i
      fvl(edgc,nv+i) = a
      fvl(edga,nv+i) = c
      eang(nv+i) = eang(c) - ang
      eang(c) = ang
    end if

  end do
!
!  If DOF, reset FACTYP values of -1 to 0.
!
  if (dof) then

    do i = 1,2

      if (i == 1) then
        head = npf - 1
      else
        head = npf
      end if

      ptr = pfl(2,head)

      do

        f = abs(pfl(1,ptr))

        if (factyp(f) == -1) then
          factyp(f) = 0
        end if

        ptr = pfl(2,ptr)

        if (ptr == head) then
          exit
        end if

      end do

    end do

  end if
!
!  If cut face is double occurring, set all faces to belong to
!  polyhedron P.
!
  if (.not. docf) go to 240

  npolh = npolh - 1
  facep(3,nface) = -p
  tailp = pfl(2,npf-1)
  pfl(2,npf-1) = npf
  ptr = npf

230 continue

  sf = pfl(1,ptr)
  f = abs(sf)

  if (sf*facep(2,f) > 0) then
    facep(2,f) = sign(p,sf)
  else
    facep(3,f) = -p
  end if

  if (pfl(2,ptr) /= npf) then
    ptr = pfl(2,ptr)
    go to 230
  else
    pfl(2,ptr) = tailp
  end if

240 continue

  if (msglvl == 2) then
    write ( *,600) nce,facep(2,nface),facep(3,nface)
    do i = 1,nce
      la = fvl(loc,nv+i)
      write ( *,610) i,la,(vcl(j,la),j=1,3)
    end do
    write ( *,*)
  end if

  600 format (1x,'cut face: #edges, polyh(1:2) =',3i7)
  610 format (1x,2i7,3f15.7)

  return
end
subroutine insph ( a, b, c, d, center, rad )
!
!******************************************************************************
!
!! INSPH finds the center and radius of the insphere of a tetrahedron.
!
!
!  Purpose: 
!
!    Find the center and radius of the insphere of a tetrahedron.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A(1:3), B(1:3), C(1:3), D(1:3) - 4 vertices of tetrahedron.
!
!    Output, CENTER(1:3) - center of insphere; undefined if A,B,C,D coplanar.
!
!    Output, RAD - radius of insphere; 0 if A,B,C,D coplanar.
!
  implicit none
!
  double precision a(3)
  double precision ab(3)
  double precision ac(3)
  double precision ad(3)
  double precision b(3)
  double precision bc(3)
  double precision bd(3)
  double precision c(3)
  double precision center(3)
  double precision d(3)
  integer i
  integer j
  double precision mat(4,4)
  integer r
  double precision rad
  double precision rtol
  logical singlr
  double precision t
  double precision tol
!
!  Compute unit outward (or inward) normals and equations of 4 faces.
!
  tol = 100.0D+00 * epsilon ( tol )
  ab(1:3) = b(1:3) - a(1:3)
  ac(1:3) = c(1:3) - a(1:3)
  ad(1:3) = d(1:3) - a(1:3)
  bc(1:3) = c(1:3) - b(1:3)
  bd(1:3) = d(1:3) - b(1:3)

  rtol = tol*max(abs(a(1)),abs(a(2)),abs(a(3)),abs(b(1)),abs(b(2)), &
    abs(b(3)),abs(c(1)),abs(c(2)),abs(c(3)),abs(d(1)),abs(d(2)), &
    abs(d(3)))

  mat(1,1) = ac(2)*ab(3) - ac(3)*ab(2)
  mat(1,2) = ac(3)*ab(1) - ac(1)*ab(3)
  mat(1,3) = ac(1)*ab(2) - ac(2)*ab(1)
  mat(2,1) = ab(2)*ad(3) - ab(3)*ad(2)
  mat(2,2) = ab(3)*ad(1) - ab(1)*ad(3)
  mat(2,3) = ab(1)*ad(2) - ab(2)*ad(1)
  mat(3,1) = ad(2)*ac(3) - ad(3)*ac(2)
  mat(3,2) = ad(3)*ac(1) - ad(1)*ac(3)
  mat(3,3) = ad(1)*ac(2) - ad(2)*ac(1)
  mat(4,1) = bc(2)*bd(3) - bc(3)*bd(2)
  mat(4,2) = bc(3)*bd(1) - bc(1)*bd(3)
  mat(4,3) = bc(1)*bd(2) - bc(2)*bd(1)
  singlr = .true.

  do i = 4,1,-1

    t = sqrt(mat(i,1)**2 + mat(i,2)**2 + mat(i,3)**2)
    if (t <= rtol) go to 100
    mat(i,1) = mat(i,1)/t
    mat(i,2) = mat(i,2)/t
    mat(i,3) = mat(i,3)/t

    if (i == 4) then
      mat(i,4) = mat(i,1)*b(1) + mat(i,2)*b(2) + mat(i,3)*b(3)
    else
      mat(i,4) = mat(i,1)*a(1) + mat(i,2)*a(2) + mat(i,3)*a(3)
      mat(i,1:4) = mat(i,1:4) - mat(4,1:4)
    end if

  end do
!
!  Use Gaussian elimination with partial pivoting to solve 3 by 3
!  system of linear equations for center of insphere.
!
  r = 1

  do i = 2,3
    if (abs(mat(i,1)) > abs(mat(r,1))) r = i
  end do

  if (abs(mat(r,1)) <= tol) go to 100

  if (r /= 1) then
    do j = 1,4
      t = mat(1,j)
      mat(1,j) = mat(r,j)
      mat(r,j) = t
    end do
  end if

  do i = 2,3
    t = mat(i,1)/mat(1,1)
    mat(i,2:4) = mat(i,2:4) - t*mat(1,2:4)
  end do

  if (abs(mat(3,2)) > abs(mat(2,2))) then

    do j = 2,4
      t = mat(2,j)
      mat(2,j) = mat(3,j)
      mat(3,j) = t
    end do

  end if

  if ( abs(mat(2,2)) > tol ) then

    t = mat(3,2)/mat(2,2)
    mat(3,3:4) = mat(3,3:4) - t*mat(2,3:4)
  
    if (abs(mat(3,3)) > tol) then
      singlr = .false.
    end if

  end if

100 continue

  if (singlr) then
    rad = 0.0d0
  else
    center(3) = mat(3,4)/mat(3,3)
    center(2) = (mat(2,4) - mat(2,3)*center(3))/mat(2,2)
    center(1) = (mat(1,4) - mat(1,3)*center(3) - &
      mat(1,2)*center(2))/mat(1,1)
    rad = abs(mat(4,1)*center(1) + mat(4,2)*center(2) + &
      mat(4,3)*center(3) - mat(4,4))
  end if

  return
end
subroutine insvr2 ( xi, yi, wp, nvc, nvert, maxvc, maxpv, vcl, pvl, iang, &
  w, ierr )
!
!******************************************************************************
!
!! INSVR2 inserts a point into the vertex coordinate and polygon vertex lists.
!
!
!  Purpose: 
!
!    Insert point (XI,YI) into vertex coordinate list and
!    polygon vertex list data structures.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, XI,YI - coordinates of point to be inserted.
!
!    Input, WP - index of vertex in PVL which is to be the predecessor
!    vertex of the inserted vertex.
!
!    Input/output, NVC - number of positions used in VCL array.
!
!    Input/output, NVERT - number of positions used in PVL array.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXPV - maximum size available for PVL array.
!
!    Input/output, VCL(1:2,1:NVC) - vertex coordinate list.
!
!    Input/output, PVL(1:4,1:NVERT) - polygon vertex list.
!
!    Input/output, IANG(1:NVERT) - polygon interior angles.
!
!    Output, W - index of inserted vertex in PVL.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxpv
  integer maxvc
!
  integer, parameter :: edgv = 4
  double precision iang(maxpv)
  integer ierr
  integer, parameter :: loc = 1
  integer nvc
  integer nvert
  double precision pi
  integer, parameter :: polg = 2
  integer pvl(4,maxpv)
  integer, parameter :: succ = 3
  double precision tol
  double precision vcl(2,maxvc)
  integer w
  integer wp
  integer ws
  integer ww
  integer wwp
  integer wws
  double precision xi
  double precision yi
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )

  if (nvc >= maxvc) then
    ierr = 3
    return
  else if (nvert+2 > maxpv) then
    ierr = 5
    return
  end if
!
!  Update linked list of vertices of polygon containing vertex WP.
!
  nvc = nvc + 1
  vcl(1,nvc) = xi
  vcl(2,nvc) = yi
  ws = pvl(succ,wp)
  nvert = nvert + 1
  w = nvert
  pvl(loc,w) = nvc
  pvl(polg,w) = pvl(polg,wp)
  pvl(succ,wp) = w
  pvl(succ,w) = ws
  iang(w) = pi()
  pvl(edgv,w) = pvl(edgv,wp)
!
!  If edge containing (XI,YI) is shared by another polygon,
!  then also update linked list of vertices of that polygon.
!
  if (pvl(edgv,wp) > 0) then
    wws = pvl(edgv,wp)
    wwp = pvl(succ,wws)
    nvert = nvert + 1
    ww = nvert
    pvl(loc,ww) = nvc
    pvl(polg,ww) = pvl(polg,wws)
    pvl(succ,wws) = ww
    pvl(succ,ww) = wwp
    iang(ww) = pi()
    pvl(edgv,wp) = ww
    pvl(edgv,ww) = wp
    pvl(edgv,wws) = w
  end if

  return
end
subroutine insvr3 ( a, nvc, nvert, maxfv, vcl, fvl, eang, ierr )
!
!******************************************************************************
!
!! INSVR3 inserts a point into the polyhedral decomposition database.
!
!
!  Purpose: 
!
!    Insert a vertex on an edge of polyhedral decomposition
!    data structure.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A - index of FVL specifying edge containing inserted vertex.
!
!    Input/output, NVC - number of vertex coordinates.
!
!    Input/output, NVERT - number of positions used in FVL, EANG arrays.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, VCL(1:3,1:NVC+1) - vertex coordinate list; VCL(*,NVC+1) are
!    coordinates of vertex to be inserted.
!
!    Input/output, FVL(1:6,1:NVERT) - face vertex list.
!
!    Input/output, EANG(1:NVERT) - edge angles.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfv
  integer nvc
!
  integer a
  double precision ang
  double precision angnxt
  integer b
  logical bflag
  integer c
  integer d
  double precision eang(maxfv)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer, parameter :: facn = 2
  integer fvl(6,maxfv)
  integer i
  integer ierr
  integer j
  integer k
  integer l
  integer la
  integer lb
  integer li
  integer lj
  integer lnxt
  integer, parameter :: loc = 1
  integer n
  integer nvert
  integer, parameter :: pred = 4
  integer, parameter :: succ = 3
  double precision vcl(3,nvc)
!
  ierr = 0
  nvc = nvc + 1
  b = fvl(succ,a)
  la = fvl(loc,a)
  lb = fvl(loc,b)
  i = a
!
!  Find start edge of FVL if AB lies on boundary of decomposition.
!
  c = i

  do

    d = fvl(edga,c)

    if ( d == 0 ) then
      exit
    end if

    if ( d == i ) then
      exit
    end if

    c = d

  end do

  bflag = (d == 0)
!
!  Insert new entry of FVL for each face containing edge AB.
!
  i = c
  k = nvert

20 continue

  j = fvl(succ,i)
  k = k + 1

  if (k > maxfv) then
    ierr = 15
    return
  end if

  fvl(loc,k) = nvc
  fvl(facn,k) = fvl(facn,i)
  fvl(succ,k) = j
  fvl(pred,k) = i
  fvl(edga,k) = 0
  fvl(edgc,k) = 0
  fvl(succ,i) = k
  fvl(pred,j) = k
  eang(k) = -1.0d0
  i = fvl(edgc,i)
  if (i /= 0 .and. i /= c) go to 20

  nvert = k
!
!  Set EDGA, EDGC, and EANG fields.
!
  i = c
  l = fvl(edgc,i)
  ang = eang(i)

30 continue

  k = fvl(succ,i)
  j = fvl(succ,k)
  li = fvl(loc,i)
  lj = fvl(loc,j)
  n = fvl(succ,l)
  lnxt = fvl(edgc,l)
  angnxt = eang(l)

  if (li < lj) then

    if (fvl(loc,l) == li) then
      fvl(edga,k) = n
      fvl(edgc,n) = k
      eang(n) = ang
    else
      fvl(edgc,i) = n
      fvl(edga,n) = i
      fvl(edga,k) = l
      fvl(edgc,l) = k
      eang(l) = ang
      if (lnxt == 0) fvl(edga,l) = 0
    end if

  else

    if (fvl(loc,l) == li) then
      fvl(edgc,k) = n
      fvl(edga,n) = k
      eang(k) = ang
      fvl(edga,i) = l
      fvl(edgc,l) = i
      eang(l) = ang
      if (lnxt == 0) fvl(edga,l) = 0
    else
      fvl(edgc,k) = l
      fvl(edga,l) = k
      eang(k) = ang
      fvl(edga,i) = n
      fvl(edgc,n) = i
      eang(n) = ang
    end if

    if (bflag .and. i == c) then
      fvl(edgc,i) = 0
      eang(i) = -1.0d0
    end if

  end if

  i = l
  l = lnxt
  ang = angnxt
  if (l /= 0 .and. i /= c) go to 30

  return
end
subroutine intmvg ( nsvc, nface, nvert, svcl, hvl, fvl, ibot, itop, h, &
  maxvc, maxwk, nvc, vcl, wk, ierr )
!
!******************************************************************************
!
!! INTMVG generates interior mesh vertices in a shrunken polyhedron.
!
!
!  Purpose: 
!
!    Generate interior mesh vertices in (shrunken) convex
!    polyhedron. Find intersection of rotated polyhedron (rotated
!    so its diameter is parallel to z-axis) with planes of type z=c
!    at distance H apart. Then generate mesh vertices in these
!    convex polygons using a quasi-uniform grid of spacing H.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NSVC - number of vertex coordinates for convex polyhedron.
!
!    Input, NFACE - number of faces in convex polyhedron.
!
!    Input, NVERT - size of FVL array.
!
!    Input/output, SVCL(1:3,1:NSVC) - vertex coordinate list.
!
!    Input/output, HVL(1:NFACE) - head vertex list.
!
!    Input, FVL(1:5,1:NVERT) - face vertex list; see routine DSCPH; may
!    contain unused columns, indicated by LOC values <= 0.
!
!    Input, IBOT, ITOP - indices in SVCL of 2 vertices realizing diameter.
!
!    Input, H - mesh spacing.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXWK - maximum size available for WK array; should be twice
!    maximum number of vertices in intersection of plane with polyhedron.
!
!    Output, NVC - number of interior mesh vertices generated.
!
!    Output, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxvc
  integer maxwk
  integer nface
  integer nsvc
  integer nvert
!
  double precision costh
  double precision cxy
  double precision cy
  double precision cyz
  double precision dmv(3)
  double precision dz
  integer, parameter :: edgv = 5
  integer f
  integer, parameter :: facn = 2
  integer fvl(5,nvert)
  double precision h
  double precision htol
  integer hvl(nface)
  integer i
  integer i1
  integer i2
  integer ib
  integer ibot
  integer ierr
  integer itop
  integer j
  integer k
  integer l
  double precision leng
  integer li
  integer lj
  integer, parameter :: loc = 1
  integer m
  integer n
  integer np
  integer nvc
  integer nvcold
  integer nvrt
  integer, parameter :: pred = 4
  integer r
  double precision r21
  double precision r22
  double precision r31
  double precision r32
  double precision sinth
  integer, parameter :: succ = 3
  double precision svcl(3,nsvc)
  double precision sxy
  double precision sy
  double precision syz
  double precision t
  double precision tol
  double precision vcl(3,maxvc)
  double precision wk(maxwk)
  double precision x
  integer xc
  double precision xl
  double precision xr
  double precision y
  integer yc
  double precision zr
  double precision zrptol
!
!  Rotate diameter vector to (0,0,1), i.e. rotate vertex coordinates.
!  Rotation matrix is:
!    [ CXY     -SXY     0   ]
!    [ CYZ*SXY CYZ*CXY -SYZ ]
!    [ SYZ*SXY SYZ*CXY  CYZ ]
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  nvc = 0
  xc = 1
  yc = maxwk/2 + 1

  dmv(1:3) = svcl(1:3,itop) - svcl(1:3,ibot)

  leng = sqrt(dmv(1)**2 + dmv(2)**2 + dmv(3)**2)

  dmv(1:3) = dmv(1:3) / leng

  if (abs(dmv(1)) <= tol) then
    leng = dmv(2)
    cxy = 1.0d0
    sxy = 0.0d0
  else
    leng = sqrt(dmv(1)**2 + dmv(2)**2)
    cxy = dmv(2)/leng
    sxy = dmv(1)/leng
  end if

  cyz = dmv(3)
  syz = leng
  r21 = cyz*sxy
  r22 = cyz*cxy
  r31 = dmv(1)
  r32 = dmv(2)

  do i = 1,nsvc
    x = svcl(1,i)
    y = svcl(2,i)
    svcl(1,i) = cxy*x - sxy*y
    svcl(2,i) = r21*x + r22*y - syz*svcl(3,i)
    svcl(3,i) = r31*x + r32*y + cyz*svcl(3,i)
  end do
!
!  Compute number of planes and intersection of polyhedron with each plane.
!
  np = int((svcl(3,itop) - svcl(3,ibot))/h)
  dz = (svcl(3,itop) - svcl(3,ibot) - np*h)*0.5d0
  htol = h*tol

  if (dz <= htol) then
    np = np - 1
    dz = h*0.5d0
  end if

  if (np < 0) then
    if (maxvc < 1) then
      ierr = 14
    else
      nvc = 1
      x = svcl(1,itop)
      y = svcl(2,itop)
      vcl(1,nvc) = cxy*x + r21*y + r31*svcl(3,itop)
      vcl(2,nvc) = r22*y - sxy*x + r32*svcl(3,itop)
      vcl(3,nvc) = cyz*svcl(3,itop) - syz*y
    end if
    return
  end if

  zr = svcl(3,itop) - dz
  f = 0

  do i = 1,nvert
    if (fvl(loc,i) == itop) then
      f = fvl(facn,i)
      hvl(f) = i
      exit
    end if
  end do

  if (f == 0) then
    ierr = 318
    return
  end if

  do k = 0,np

    i = hvl(f)
    li = fvl(loc,i)
    zrptol = zr + htol
!
!  Z-coordinate of vertex I is > ZR + HTOL.
!
60  continue

    j = fvl(succ,i)
    lj = fvl(loc,j)

    if (svcl(3,lj) >= svcl(3,li)) then
      i = fvl(succ,fvl(edgv,i))
      f = fvl(facn,i)
      go to 60
    else if (svcl(3,lj) > zrptol) then
      i = j
      li = lj
      go to 60
    end if
!
!  Trace out convex polygon on plane Z = ZR inside polyhedron.
!
    hvl(f) = i
    nvrt = 0

70  continue

    t = zr - svcl(3,lj)

    if (t <= htol) then

      if (nvrt > 0 .and. svcl(1,lj) == wk(xc) .and. &
        svcl(2,lj) == wk(yc)) then
        go to 100
      end if

      wk(xc+nvrt) = svcl(1,lj)
      wk(yc+nvrt) = svcl(2,lj)

80    continue

      l = fvl(loc,fvl(succ,j))

      if (svcl(3,l) > zrptol) then
        j = fvl(succ,fvl(edgv,j))
        go to 80
      end if

    else

      t = t/(svcl(3,li) - svcl(3,lj))
      wk(xc+nvrt) = svcl(1,lj) + t*(svcl(1,li) - svcl(1,lj))
      wk(yc+nvrt) = svcl(2,lj) + t*(svcl(2,li) - svcl(2,lj))

    end if

    nvrt = nvrt + 1

    if (nvrt >= yc - 1) then
      ierr = 7
      return
    end if

90  continue

    j = fvl(succ,j)
    lj = fvl(loc,j)
    if (svcl(3,lj) <= zrptol) then
      go to 90
    end if

    i = fvl(edgv,fvl(pred,j))
    if (fvl(facn,i) == f) go to 100
    li = fvl(loc,i)
    j = fvl(succ,i)
    lj = fvl(loc,j)
    go to 70

100 continue

    wk(xc+nvrt) = wk(xc)
    wk(yc+nvrt) = wk(yc)
    call diam2(nvrt,wk(xc+1),wk(yc+1),i1,i2,t,ierr)
    if (ierr /= 0) return
    call rotpg(nvrt,wk(xc),wk(yc),i1,i2,ib,costh,sinth)
    n = int((wk(yc) - wk(yc+ib))/h)
    y = wk(yc) - 0.5d0*(wk(yc) - wk(yc+ib) - dble(n)*h)
    l = 0
    r = nvrt
    nvcold = nvc
!
!  Determine left and right x-coordinates of polygon for
!  scan line with y-coordinate Y, and generate mesh vertices.
!
    do i = 0,n

      do while (wk(yc+l+1) > y)
        l = l + 1
      end do

      do while (wk(yc+r-1) > y)
        r = r - 1
      end do

      xl = wk(xc+l) + (wk(xc+l+1) - wk(xc+l))*(y - wk(yc+l))/ &
           (wk(yc+l+1) - wk(yc+l))

      xr = wk(xc+r) + (wk(xc+r-1) - wk(xc+r))*(y - wk(yc+r))/ &
           (wk(yc+r-1) - wk(yc+r))

      m = int((xr - xl)/h)
      x = xl + 0.5d0*(xr - xl - dble(m)*h)
      cy = costh*y
      sy = sinth*y

      if (nvc + m + 1 > maxvc) then
        ierr = 14
        return
      end if

      do j = 0,m
        nvc = nvc + 1
        vcl(1,nvc) = costh*x + sy
        vcl(2,nvc) = cy - sinth*x
        x = x + h
      end do

      y = y - h
      if (y < wk(yc+ib)) y = wk(yc+ib)

    end do

    do i = nvcold+1,nvc
      x = vcl(1,i)
      y = vcl(2,i)
      vcl(1,i) = cxy*x + r21*y + r31*zr
      vcl(2,i) = r22*y - sxy*x + r32*zr
      vcl(3,i) = cyz*zr - syz*y
    end do

    zr = zr - h

  end do

  return
end
subroutine intpg ( nvrt, xc, yc, ctrx, ctry, arpoly, hflag, umdf, wsq, nev, &
  ifv, listev, ivrt, edgval, vrtval, vcl, mdfint, mean, stdv, mdftr )
!
!******************************************************************************
!
!! INTPG integrates a mesh distribution function over a polygon.
!
!
!  Purpose: 
!
!    Compute integral of MDF2(X,Y) [heuristic mdf] or
!    UMDF(X,Y) [user-supplied mdf] in convex polygon.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NVRT - number of vertices in polygon.
!
!    Input, XC(0:NVRT), YC(0:NVRT) - coordinates of polygon vertices in
!    counter clockwise order, translated so that centroid is 
!    at origin; (XC(0),YC(0)) = (XC(NVRT),YC(NVRT)).
!
!    Input, CTRX, CTRY - coordinates of centroid before translation.
!
!    Input, ARPOLY - area of polygon.
!
!    Input, HFLAG - .TRUE. if heuristic mdf, .FALSE. if user-supplied mdf.
!
!    Input, external UMDF(X,Y) - d.p user-supplied mdf with d.p arguments.
!
!    Input, WSQ - square of width of original polygon of decomposition.
!
!    Input, NEV, IFV, LISTEV(1:NEV) - output from routine PRMDF2.
!
!    Input, IVRT(1:*), EDGVAL(1:*), VRTVAL(1:*) - arrays output from DSMDF2;
!    if .NOT. HFLAG then only first array exists.
!
!    Input, VCL(1:2,1:*) - vertex coordinate list.
!
!    Output, MDFINT - integral of mdf in polygon.
!
!    Output, MEAN - mean mdf value in polygon.
!
!    Output, STDV - standard deviation of mdf in polygon.
!
!    Output, MDFTR(0:NVRT-1) - mean mdf value in each triangle of polygon;
!    triangles are determined by polygon vertices and centroid.
!
  implicit none
!
  integer nev
  integer, parameter :: nqpt = 3
  integer nvrt
!
  double precision areatr
  double precision arpoly
  double precision ctrx
  double precision ctry
  double precision d
  double precision edgval(*)
  logical hflag
  integer i
  integer ifv
  integer ivrt(*)
  integer j
  integer k
  integer kp1
  integer l
  integer listev(nev)
  integer m
  double precision mdfint
  double precision mdfsqi
  double precision mdftr(0:nvrt-1)
  double precision mean
  double precision, parameter, dimension (3,nqpt) :: qc = reshape ( &
    (/ 0.6666666666666666d0, 0.1666666666666667d0, &
       0.1666666666666667d0, 0.1666666666666667d0, &
       0.6666666666666666d0, 0.1666666666666667d0, &
       0.1666666666666667d0, 0.1666666666666667d0, &
       0.6666666666666666d0/), (/ 3, nqpt /) )
  double precision s
  double precision stdv
  double precision sum1
  double precision sum2
  double precision temp
  double precision umdf
  double precision val
  double precision vcl(2,*)
  double precision vrtval(*)
  double precision wsq
  double precision, parameter, dimension ( nqpt ) :: wt = (/ &
    0.3333333333333333d0, 0.3333333333333333d0, 0.3333333333333333d0 /)
  double precision x
  double precision x0
  double precision x1
  double precision xc(0:nvrt)
  double precision xx
  double precision y
  double precision y0
  double precision y1
  double precision yc(0:nvrt)
  double precision yy
!
!  NQPT is number of quad points for numerical integration in triangle
!  WT(I) is weight of Ith quadrature point
!  QC(1:3,I) are barycentric coordinates of Ith quadrature point
!
  mdfint = 0.0d0
  mdfsqi = 0.0d0

  do l = 0, nvrt-1

    areatr = 0.5d0*(xc(l)*yc(l+1) - xc(l+1)*yc(l))
    sum1 = 0.0d0
    sum2 = 0.0d0

    do m = 1, nqpt

      xx = qc(1,m)*xc(l) + qc(2,m)*xc(l+1)
      yy = qc(1,m)*yc(l) + qc(2,m)*yc(l+1)
!
!  Insert code for function MDF2 to reduce number of calls.
!
      if (hflag) then

        x = xx + ctrx
        y = yy + ctry
        s = wsq

        do i = 1,nev

          k = listev(i)

          if (k < 0) then

            k = -k
            d = (vcl(1,k) - x)**2 + (vcl(2,k) - y)**2
            d = max(0.25d0*d,vrtval(k))
            s = min(s,d)

          else

            kp1 = k + 1
            if (i == nev .and. ifv > 0) kp1 = ifv
            j = ivrt(kp1)
            x0 = x - vcl(1,j)
            y0 = y - vcl(2,j)
            x1 = vcl(1,ivrt(k)) - vcl(1,j)
            y1 = vcl(2,ivrt(k)) - vcl(2,j)

            if (x0*x1 + y0*y1 <= 0.0d0) then

              d = x0**2 + y0**2

            else

              x0 = x0 - x1
              y0 = y0 - y1

              if (x0*x1 + y0*y1 >= 0.0d0) then
                d = x0**2 + y0**2
              else
                d = (x1*y0 - y1*x0)**2/(x1**2 + y1**2)
              end if

            end if

            d = max(0.25d0*d,edgval(k))
            s = min(s,d)

          end if

        end do

        val = 1.0d0/s

      else

        val = umdf(xx+ctrx,yy+ctry)

      end if

      temp = wt(m)*val
      sum1 = sum1 + temp
      sum2 = sum2 + temp*val

    end do

    mdftr(l) = sum1
    mdfint = mdfint + sum1*areatr
    mdfsqi = mdfsqi + sum2*areatr

  end do

  mean = mdfint / arpoly
  stdv = mdfsqi / arpoly - mean**2
  stdv = sqrt ( max ( stdv, 0.0d0 ) )

  return
end
subroutine intph ( hflag, umdf, headp, widp, nfcev, nedev, nvrev, listev, &
  infoev, ivrt, facval, edgval, vrtval, vcl, facep, fvl, pfl, cntr, mdfint, &
  mean, stdv, volp, nf, indf, meanf, stdvf )
!
!******************************************************************************
!
!! INTPH integrates a mesh distribution function over a polyhedron.
!
!
!  Purpose: 
!
!    Compute integral of MDF3(X,Y,Z) [heuristic mdf] or
!    UMDF(X,Y,Z) [user-supplied mdf] in convex polyhedron.
!
!  Discussion:
!
!    Parameters WIDP to VRTVAL are used only if HFLAG = TRUE.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, HFLAG - .TRUE. if heuristic mdf, .FALSE. if user-supplied mdf.
!
!    Input, UMDF(X,Y,Z) - d.p user-supplied mdf with d.p arguments.
!
!    Input, HEADP - head pointer to face of PFL for convex polyhedron.
!
!    Input, WIDP - width of original polyhedron of decomposition.
!
!    Input, NFCEV, NEDEV, NVREV, LISTEV(1:NFCEV+NEDEV+NVREV),
!    INFOEV(1:4,1:NFCEV+NEDEV) - output from routine PRMDF3.
!
!    Input, IVRT(1:*), FACVAL(1:*), EDGVAL(1:*), VRTVAL(1:*) - arrays output
!    from routine DSMDF3.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, FACEP(1:3,1:*) - face pointer list.
!
!    Input, FVL(1:6,1:*) - face vertex list.
!
!    Input, PFL(1:2,1:*) - list of signed face indices for each polyhedron.
!
!    Input, CNTR(1:3) - weighted centroid of polyhedron.
!
!    Input, NF - number of faces in polyhedron if NF > 1, else NF = 1;
!    in former case INDF, MEANF, STDVF values may be computed.
!
!    Output, MDFINT - integral of mdf in polyhedron.
!
!    Output, MEAN - mean mdf value in polyhedron.
!
!    Output, STDV - standard deviation of mdf in polyhedron.
!
!    Output, VOLP - volume of polyhedron.
!
!    Output, INDF(1:NF) - indices in FACEP of faces of polyhedron.
!
!    Output, MEANF(1:NF) - mean mdf value associated with faces of polyhedron.
!
!    Output, STDVF(1:NF) - standard deviation of mdf associated with faces.
!
!    [Note: Above 3 arrays are computed if NF > 1 and (HFLAG =
!    FALSE or NFCEV + NEDEV + NVREV > 0].
!
  implicit none
!
  integer nf
  integer, parameter :: nqpt = 4
!
  double precision cntr(3)
  double precision cntrf(3)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  double precision edgval(*)
  logical eval
  integer f
  integer facep(3,*)
  integer, parameter :: facn = 2
  double precision facval(*)
  integer fvl(6,*)
  logical hflag
  integer headp
  integer i
  integer indf(nf)
  double precision infoev(4,*)
  integer ivrt(*)
  integer j
  integer k
  integer lj
  integer lk
  integer listev(*)
  integer, parameter :: loc = 1
  integer m
  double precision mdf3
  double precision mdfif
  double precision mdfint
  double precision mdfsf
  double precision mdfsqi
  double precision mean
  double precision meanf(nf)
  integer n
  integer nedev
  integer nfcev
  integer nvrev
  integer pfl(2,*)
  integer, parameter :: pred = 4
  integer q
  double precision, parameter, dimension ( 4, nqpt ) :: qc = reshape ( &
    (/0.5854102d0,0.1381966d0,0.1381966d0,0.1381966d0, &
     0.1381966d0,0.5854102d0,0.1381966d0,0.1381966d0, &
     0.1381966d0,0.1381966d0,0.5854102d0,0.1381966d0, &
     0.1381966d0,0.1381966d0,0.1381966d0,0.5854102d0/), (/ 4, nqpt /) )
  double precision stdv
  double precision stdvf(nf)
  integer, parameter :: succ = 3
  double precision sum1
  double precision sum2
  double precision temp
  double precision umdf
  double precision val
  double precision vcl(3,*)
  double precision volf
  double precision volp
  double precision volt
  double precision volth
  double precision vrtval(*)
  double precision widp
  double precision, parameter, dimension ( nqpt ) :: wt = (/ &
    0.25d0, 0.25d0, 0.25d0, 0.25d0 /)
  double precision x
  double precision y
  double precision z
!
!  NQPT is number of quad points for numerical integration in tetrahedron
!  WT(I) is weight of Ith quadrature point
!  QC(1:4,I) are barycentric coordinates of Ith quadrature point
!
  if (hflag) then
    eval = (nfcev + nedev + nvrev > 0)
  else
    eval = .true.
  end if

  mdfint = 0.0d0
  mdfsqi = 0.0d0
  volp = 0.0d0
  m = 0
  i = headp

10 continue

  f = abs(pfl(1,i))
  n = 0
  cntrf(1:3) = 0.0d0
  j = facep(1,f)

20 continue

  lj = fvl(loc,j)
  cntrf(1:3) = cntrf(1:3) + vcl(1:3,lj)
  n = n + 1
  j = fvl(succ,j)
  if (j /= facep(1,f)) go to 20

  cntrf(1:3) = cntrf(1:3)/dble(n)
  mdfif = 0.0d0
  mdfsf = 0.0d0
  volf = 0.0d0
  lj = fvl(loc,j)

30 continue

  k = fvl(succ,j)
  lk = fvl(loc,k)
  volt = volth(cntr,cntrf,vcl(1,lj),vcl(1,lk))/6.0d0
  volf = volf + volt

  if (eval) then

    sum1 = 0.0d0
    sum2 = 0.0d0

    do q = 1,nqpt

      x = qc(1,q)*cntr(1) + qc(2,q)*cntrf(1) + &
          qc(3,q)*vcl(1,lj) + qc(4,q)*vcl(1,lk)
      y = qc(1,q)*cntr(2) + qc(2,q)*cntrf(2) + &
          qc(3,q)*vcl(2,lj) + qc(4,q)*vcl(2,lk)
      z = qc(1,q)*cntr(3) + qc(2,q)*cntrf(3) + &
          qc(3,q)*vcl(3,lj) + qc(4,q)*vcl(3,lk)

      if (hflag) then
        val = mdf3(x,y,z,widp,nfcev,nedev,nvrev,listev, &
          infoev,ivrt,facval,edgval,vrtval,vcl)
      else
        val = umdf(x,y,z)
      end if

      temp = wt(q)*val
      sum1 = sum1 + temp
      sum2 = sum2 + temp*val

    end do

    mdfif = mdfif + sum1*volt
    mdfsf = mdfsf + sum2*volt

  end if

  j = k
  lj = lk

  if (j /= facep(1,f)) go to 30

  if (eval) then

    mdfint = mdfint + mdfif
    mdfsqi = mdfsqi + mdfsf

    if (nf > 1) then
      m = m + 1
      indf(m) = f
      meanf(m) = mdfif/volf
      temp = mdfsf/volf - meanf(m)**2
      stdvf(m) = sqrt(max(temp,0.0d0))
    end if

  end if

  volp = volp + volf
  i = pfl(2,i)
  if (i /= headp) go to 10

  if ( eval ) then
    mean = mdfint / volp
    stdv = mdfsqi / volp - mean**2
    stdv = sqrt ( max ( stdv, 0.0d0 ) )
  else
    mean = 1.0d0 / widp**3
    mdfint = mean * volp
    stdv = 0.0d0
  end if

  return
end
subroutine isftdw ( l, u, k, lda, a, map )
!
!******************************************************************************
!
!! ISFTDW does one step of the heap sort algorithm for integer data.
!
!
!  Purpose: 
!
!    Sift A(*,MAP(L)) down a heap of size U.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, L, U - lower and upper index of part of heap.
!
!    Input, K - dimension of points.
!
!    Input, LDA - leading dimension of array A in calling routine.
!
!    Input, A(1:K,1:*), see routine IHPSRT.
!
!    Input/output, MAP(1:*) - see routine IHPSRT.
!
  implicit none
!
  integer lda
!
  integer a(lda,*)
  integer i
  logical iless
  integer j
  integer k
  integer l
  integer map(*)
  integer t
  integer u
!
  i = l
  j = 2*i
  t = map(i)

  do while (j <= u)

    if (j < u) then
      if (iless(k,a(1,map(j)),a(1,map(j+1)))) then
        j = j + 1
      end if
    end if

    if (iless(k,a(1,map(j)),a(1,t))) then
      exit
    end if

    map(i) = map(j)
    i = j
    j = 2*i

  end do

  map(i) = t

  return
end
subroutine itris3 ( npt, sizht, maxbf, maxfc, vcl, vm, nbf, nfc, nface, &
  ntetra, bf, fc, ht, ierr )
!
!******************************************************************************
!
!! ITRIS3 constructs an initial triangulation of 3D vertices.
!
!
!  Purpose: 
!
!    Construct initial triangulation of 3D vertices by first
!    sorting them in lexicographically increasing (x,y,z) order and
!    then inserting 1 vertex at a time from outside the convex hull.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NPT - number of 3D vertices (points).
!
!    Input, SIZHT - size of hash table HT; a good choice is a prime number
!    which is about 1/8 * NFACE (or 3/2 * NPT for random
!    points from the uniform distribution).
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input/output, VM(1:NPT) - indices of vertices of VCL being triangulated.
!    On output, indices are permuted, so that VCL(*,VM(1)), ... ,
!    VCL(*,VM(NPT)) are in lexicographic increasing order,
!    with possible slight reordering so first 4 vertices are
!    non-coplanar.
!
!    Output, NBF - number of positions used in BF array; NBF <= MAXBF.
!
!    Output, NFC - number of positions used in FC array; NFC <= MAXFC.
!
!    Output, NFACE - number of faces in triangulation; NFACE <= NFC.
!
!    Output, NTETRA - number of tetrahedra in triangulation.
!
!    Output, BF(1:3,1:NBF) -  array of boundary face records containing pointers
!    (indices) to FC; if FC(5,I) = -J < 0 and FC(1:3,I) = ABC,
!    then BF(1,J) points to other boundary face with edge BC,
!    BF(2,J) points to other boundary face with edge AC, and
!    BF(3,J) points to other boundary face with edge AB;
!    if BF(1,J) <= 0, record is not used and is in avail list.
!
!    Output, FC(1:7,1:NFC) - array of face records which are in linked lists
!    in hash table with direct chaining. Fields are:
!
!    FC(1:3,*) - A,B,C with 1<=A<B<C<=NPT; indices in VM of 3
!    vertices of face; if A <= 0, record is not used (it is
!    in linked list of avail records with indices <= NFC);
!    internal use: if B <= 0, face in queue, not in triang
!    FC(4:5,*) - D,E; indices in VM of 4th vertex of 1 or 2
!    tetrahedra with face ABC; if ABC is boundary face
!    then E < 0 and |E| is an index of BF array
!    FC(6,*) - HTLINK; pointer (index in FC) of next element
!    in linked list (or NULL = 0)
!    FC(7,*) - used internally for QLINK (link for queues or
!    stacks); pointer (index in FC) of next face in queue/
!    stack (or NULL = 0); QLINK = -1 indicates face is not
!    in any queue/stack, and is output value (for records
!    not in avail list), except:
!    FC(7,1:2) - HDAVBF,HDAVFC : head pointers of avail list in BF, FC
!
!    Output, HT(0:SIZHT-1) - hash table using direct chaining; entries are
!    head pointers of linked lists (indices of FC array)
!    containing the faces and tetrahedra of triangulation.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxbf
  integer maxfc
  integer npt
  integer sizht
!
  integer a
  integer b
  integer back
  integer bf(3,maxbf)
  integer bfi
  double precision ctr(3)
  integer e
  integer fc(7,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer i
  integer i3
  integer i4
  integer ierr
  integer ip
  integer j
  integer k
  integer, parameter :: msglvl = 0
  integer nbf
  integer nface
  integer nfc
  integer ntetra
  integer op
  integer opside
  integer ptr
  integer topnv
  integer top
  integer va
  integer vb
  integer vc
  double precision vcl(3,*)
  integer vi
  integer vm(npt)
!
!  Permute elements of VM so that vertices are in lexicographic
!  order, and initialize data structures.
!
  call dhpsrt(3,npt,3,vcl,vm)
  call frstet(.true.,npt,vcl,vm,i3,i4,ierr)

  if (ierr /= 0) then
    return
  end if

  do i = 1,3
    ctr(i) = ( vcl(i,vm(1)) + vcl(i,vm(2)) + vcl(i,vm(3)) + &
      vcl(i,vm(4)) )/4.0d0
  end do

  ht(0:sizht-1) = 0
  hdavbf = 0
  hdavfc = 0
  nbf = 4
  nfc = 4
  ntetra = 1
  call htins(1,1,2,3,4,-1,npt,sizht,fc,ht)
  call htins(2,1,2,4,3,-2,npt,sizht,fc,ht)
  call htins(3,1,3,4,2,-3,npt,sizht,fc,ht)
  call htins(4,2,3,4,1,-4,npt,sizht,fc,ht)

  bf(1,1) = 4
  bf(2,1) = 3
  bf(3,1) = 2
  bf(1,2) = 4
  bf(2,2) = 3
  bf(3,2) = 1
  bf(1,3) = 4
  bf(2,3) = 2
  bf(3,3) = 1
  bf(1,4) = 3
  bf(2,4) = 2
  bf(3,4) = 1

  if (msglvl == 4) then
    write ( *,600) (vm(i),i=1,4),i3,i4
  end if
!
!  Insert I-th vertex into triangulation of first I-1 vertices.
!
  do i = 5,npt

    vi = vm(i)
    if (msglvl == 4) then
      write ( *,610) i,vi
    end if
    ip = i - 1
    if (i == 5) ip = 2
    if (i == i3 + 2) ip = 3
    if (i == i4 + 1) ip = 4
!
!  Form stacks of boundary faces involving vertex IP.
!  TOP is for stack of boundary faces to be tested for visibility.
!  FRONT is for stack of boundary faces visible from vertex I.
!  TOPNV is for stack of boundary faces not visible from I.
!
    front = 0
    topnv = 0

    if (i == 5) then
      top = 4
      a = 3
      b = 2
      if (ip == 2) a = 2
      if (ip <= 3) b = 1
      fc(7,top) = a
      fc(7,a) = b
      fc(7,b) = 0

    else if (ip == i - 1) then

      top = bfi
      fc(7,bfi) = 0
      b = fc(2,bfi)
      ptr = bf(1,-fc(5,bfi))

30    continue

      if (fc(1,ptr) == b) then
        b = fc(2,ptr)
        j = 1
      else
        b = fc(1,ptr)
        j = 2
      end if

      fc(7,ptr) = top
      top = ptr
      ptr = bf(j,-fc(5,ptr))
      if (ptr /= bfi) go to 30

    else

      do k = 1,nbf

        if (bf(1,k) <= 0) then
          cycle
        end if

        do e = 1,3

          ptr = bf(e,k)

          if (fc(1,ptr) == ip) then
            b = fc(2,ptr)
            j = 3
            go to 60
          else if (fc(2,ptr) == ip) then
            b = fc(1,ptr)
            j = 3
            go to 60
          else if (fc(3,ptr) == ip) then
            b = fc(1,ptr)
            j = 2
            go to 60
          end if

        end do

      end do

60    continue

      bfi = ptr
      top = bfi
      fc(7,bfi) = 0
      ptr = bf(j,-fc(5,bfi))

70    continue

      if (fc(1,ptr) == b) then
        j = 1
        if (fc(2,ptr) == ip) then
          b = fc(3,ptr)
        else
          b = fc(2,ptr)
        end if
      else if (fc(2,ptr) == b) then
        j = 2
        if (fc(1,ptr) == ip) then
          b = fc(3,ptr)
        else
          b = fc(1,ptr)
        end if
      else
        j = 3
        if (fc(1,ptr) == ip) then
          b = fc(2,ptr)
        else
          b = fc(1,ptr)
        end if
      end if

      fc(7,ptr) = top
      top = ptr
      ptr = bf(j,-fc(5,ptr))
      if (ptr /= bfi) go to 70

    end if
!
!  Find a boundary face visible from vertex I.
!
80  continue

    if (top == 0) go to 110

    ptr = top
    top = fc(7,ptr)
    va = vm(fc(1,ptr))
    vb = vm(fc(2,ptr))
    vc = vm(fc(3,ptr))
    op = opside(vcl(1,va),vcl(1,vb),vcl(1,vc),ctr,vcl(1,vi))

    if (op == 2) then
      ierr = 301
      return
    end if

    if (op == 1) then
      front = ptr
90    continue
      if (top == 0) go to 110
      ptr = top
      top = fc(7,ptr)
      fc(7,ptr) = -1
      go to 90
    else
      fc(7,ptr) = topnv
      topnv = ptr
    end if

    go to 80

110 continue

    if (front == 0) then
      ierr = 306
      return
    end if
!
!  Find remaining visible boundary faces, add new tetrahedron with vertex I.
!
    call vbfac(vcl(1,vi),ctr,vcl,vm,bf,fc,front,topnv)

    if (ierr /= 0) then
      return
    end if

    call nwthou(i,npt,sizht,nbf,nfc,maxbf,maxfc,bf,fc,ht,ntetra, &
      hdavbf,hdavfc,front,back,bfi, ierr )

    if (ierr /= 0) then
      return
    end if

  end do

  nface = nfc
  ptr = hdavfc

  do while ( ptr /= 0 )
    nface = nface - 1
    ptr = -fc(1,ptr)
  end do

  fc(7,1) = hdavbf
  fc(7,2) = hdavfc

  600 format (/1x,'itris3: first tetrahedron: ',4i7/4x,'i3, i4 =',2i7)
  610 format (/1x,'step',i7,':   vertex i =',i7)

  return
end
subroutine jnhole ( itophv, angspc, angtol, nvc, nvert, maxvc, maxpv, &
  maxiw, maxwk, vcl, hvl, pvl, iang, iwk, wk, ierr )
!
!******************************************************************************
!
!! JNHOLE joins a hole boundary to the boundary of the surrounding polygon.
!
!
!  Purpose: 
!
!    Join hole boundary to boundary of polygon containing hole
!    by finding a cut edge originating from the top vertex of hole
!    to a point on outer polygon boundary above it.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, ITOPHV - index in PVL of top vertex of hole.
!
!    Input, ANGSPC - angle spacing parameter used in controlling the
!    vertices to be considered as an endpoint of a separator.
!
!    Input, ANGTOL - angle tolerance parameter used in accepting
!    separator(s).
!
!    Input/output, NVC - number of positions used in VCL array.
!
!    Input/output, NVERT - number of positions used in PVL array.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXPV - maximum size available for PVL array.
!
!    Input, MAXIW - maximum size available for IWK array; should be about
!    3 times number of vertices in outer polygon.
!
!    Input, MAXWK - maximum size available for WK array; should be about
!    5 times number of vertices in outer polygon.
!
!    Input/output, VCL(1:2,1:NVC) - vertex coordinate list.
!
!    Input, HVL(1:*) - head vertex list.
!
!    Input/output, PVL(1:4,1:NVERT), IANG(1:NVERT) - polygon vertex list and
!    interior angles.
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxiw
  integer maxpv
  integer maxvc
  integer maxwk
!
  double precision angle
  double precision angspc
  double precision angtol
  double precision dy
  integer, parameter :: edgv = 4
  integer hv
  integer hvl(*)
  double precision iang(maxpv)
  integer ierr
  integer ilft
  integer ipoly
  integer irgt
  integer itophv
  integer iv
  integer ivs
  integer iwk(maxiw)
  integer l
  integer, parameter :: loc = 1
  integer lv
  integer lw
  integer, parameter :: msglvl = 0
  double precision pi
  double precision tol
  integer nvc
  integer nvert
  integer, parameter :: polg = 2
  integer pvl(4,maxpv)
  double precision s
  double precision slft
  double precision srgt
  integer, parameter :: succ = 3
  integer succil
  integer succir
  double precision vcl(2,maxvc)
  integer vp
  integer vr
  integer vs
  integer vv
!  integer vv
  integer w
  double precision wk(maxwk)
  integer ww
  double precision xint
  double precision xlft
  double precision xrgt
  double precision xt
  double precision xv
  double precision xvs
  double precision ylft
  double precision yrgt
  double precision yt
  double precision ytmtol
  double precision ytptol
  double precision yv
  double precision yvs
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )

  if (nvc+3 > maxvc) then
    ierr = 3
    return
  else if (nvert+5 > maxpv) then
    ierr = 5
    return
  end if
!
!  Determine 'closest' vertices on outer boundary which are to the
!  left and right of top vertex of hole and on the horizontal line
!  through top vertex. The two closest vertices must be on edges
!  which intersect the horizontal line and are partially above the
!  line. Ties are broken (in the case of a vertex on a cut edge)
!  by choosing the vertex on the edge of maximum or minimum dx/dy
!  slope depending on whether the vertex is to the left or right
!  of top vertex, respectively.
!
  ipoly = pvl(polg,itophv)
  lv = pvl(loc,itophv)
  xt = vcl(1,lv)
  yt = vcl(2,lv)
  dy = 0.0d0
  hv = hvl(ipoly)
  iv = hv
  yv = vcl(2,pvl(loc,iv))

10 continue

  iv = pvl(succ,iv)
  yvs = vcl(2,pvl(loc,iv))
  dy = max(dy,abs(yvs-yv))
  yv = yvs
  if (iv /= hv) go to 10

  ytmtol = yt - tol*dy
  ytptol = yt + tol*dy
  ilft = 0
  irgt = 0
  xlft = 0.0d0
  xrgt = 0.0d0
  hv = hvl(ipoly)
  iv = hv
  lv = pvl(loc,iv)
  xv = vcl(1,lv)
  yv = vcl(2,lv)

20 continue

  ivs = pvl(succ,iv)
  lv = pvl(loc,ivs)
  xvs = vcl(1,lv)
  yvs = vcl(2,lv)

  if (yv <= ytptol .and. yvs > ytptol) then

    if (yv >= ytmtol) then

      if (xv > xt) then

        if (xv < xrgt .or. irgt == 0) then

          irgt = iv
          xrgt = xv
          yrgt = yv
          srgt = (xvs - xv)/(yvs - yv)

        else if (xv == xrgt) then

          s = (xvs - xv)/(yvs - yv)

          if (s < srgt) then
            irgt = iv
            yrgt = yv
            srgt = s
          end if

        end if

      end if

    else

      xint = (yt - yv)*(xvs - xv)/(yvs - yv) + xv

      if (xint > xt) then

        if (xint < xrgt .or. irgt == 0) then
          irgt = iv
          xrgt = xint
          yrgt = yt
        end if

      end if

    end if

  else if (yv > ytptol .and. yvs <= ytptol) then

    if (yvs >= ytmtol) then

      if (xvs < xt) then

        if (xvs > xlft .or. ilft == 0) then

          ilft = iv
          xlft = xvs
          ylft = yvs
          slft = (xvs - xv)/(yvs - yv)

        else if (xvs == xlft) then

          s = (xvs - xv)/(yvs - yv)

          if (s > slft) then
            ilft = iv
            ylft = yvs
            slft = s
          end if

        end if

      end if

    else

      xint = (yt - yv)*(xvs - xv)/(yvs - yv) + xv

      if (xint < xt) then

        if (xint > xlft .or. ilft == 0) then
          ilft = iv
          xlft = xint
          ylft = yt
        end if

      end if

    end if

  end if

  iv = ivs
  xv = xvs
  yv = yvs

  if (iv /= hv) go to 20

  if (ilft == 0 .or. irgt == 0) then
    ierr = 218
    return
  end if
!
!  Temporarily modify PVL to pass the subregion 'above' top vertex
!  of hole to routine RESVRT. The top vertex is the reflex vertex
!  passed to RESVRT (in the temporary subregion, it has interior
!  angle PI). This causes one separator to be chosen by RESVRT
!  and its other endpoint is above the top vertex.
!
  succil = pvl(succ,ilft)
  succir = pvl(succ,irgt)
  vcl(1,nvc+2) = xlft
  vcl(2,nvc+2) = ylft
  vcl(1,nvc+3) = xrgt
  vcl(2,nvc+3) = yrgt
  vp = nvert + 3
  vr = nvert + 4
  vs = nvert + 5
  iang(vr) = angle(xlft,ylft,xt,yt,xrgt,yrgt)

  if (iang(vr) < pi() - tol .or. iang(vr) > pi() + tol) then
    ierr = 219
    return
  end if

  pvl(loc,vp) = nvc + 2
  pvl(polg,vp) = ipoly
  pvl(succ,vp) = vr
  pvl(edgv,vp) = 0
  pvl(loc,vr) = pvl(loc,itophv)
  pvl(polg,vr) = ipoly
  pvl(succ,vr) = vs
  pvl(edgv,vr) = 0
  pvl(loc,vs) = nvc + 3
  pvl(polg,vs) = ipoly
  pvl(succ,vs) = succir
  pvl(edgv,vs) = pvl(edgv,irgt)
  pvl(succ,ilft) = vp
  lv = pvl(loc,ilft)
  iang(vp) = angle(vcl(1,lv),vcl(2,lv),xlft,ylft,xt,yt)
  lv = pvl(loc,succir)
  iang(vs) = angle(xt,yt,xrgt,yrgt,vcl(1,lv),vcl(2,lv))
  w = 0

  call resvrt(vr,angspc,angtol,nvc,nvert,maxvc,maxpv,maxiw,maxwk, &
    vcl,pvl,iang,w,ww,iwk,wk, ierr )
!
!  Remove temporary modification to PVL. There are three cases
!  depending on where the endpoint of separator is located:
!  successor of closest vertex to the right of top vertex,
!  predecessor of closest vertex to the left of top vertex,
!  or neither of these.
!
  if (pvl(succ,vs) == w) then
    pvl(succ,ilft) = succil
    pvl(succ,irgt) = w
    pvl(edgv,irgt) = pvl(edgv,vs)
    if (pvl(edgv,irgt) > 0) pvl(edgv,pvl(edgv,irgt)) = irgt
  else if (pvl(succ,ilft) == w) then
    pvl(succ,w) = succil
  else
    pvl(succ,ilft) = succil
  end if

  if (ierr /= 0) return
!
!  Update PVL with cut edge, i.e. join linked lists of vertices
!  of the hole polygon and the outer boundary polygon into one
!  linked list of vertices by adding the cut edge from the top
!  vertex of hole to the vertex on the outer boundary.
!
  nvert = nvert + 2
  vv = nvert - 1
  ww = nvert
  lv = pvl(loc,itophv)
  lw = pvl(loc,w)
  pvl(loc,vv) = lv
  pvl(loc,ww) = lw
  pvl(polg,vv) = ipoly
  pvl(polg,ww) = ipoly
  pvl(succ,vv) = pvl(succ,itophv)
  pvl(succ,ww) = pvl(succ,w)
  pvl(succ,itophv) = ww
  pvl(succ,w) = vv
  pvl(edgv,vv) = pvl(edgv,itophv)
  pvl(edgv,ww) = pvl(edgv,w)
  pvl(edgv,itophv) = w
  pvl(edgv,w) = itophv
  if (pvl(edgv,vv) > 0) pvl(edgv,pvl(edgv,vv)) = vv
  if (pvl(edgv,ww) > 0) pvl(edgv,pvl(edgv,ww)) = ww
  l = pvl(loc,pvl(succ,vv))
  iang(vv) = angle(vcl(1,lw),vcl(2,lw),vcl(1,lv),vcl(2,lv), &
    vcl(1,l),vcl(2,l))
  iang(itophv) = iang(itophv) - iang(vv)
  l = pvl(loc,pvl(succ,ww))
  iang(ww) = angle(vcl(1,lv),vcl(2,lv),vcl(1,lw),vcl(2,lw), &
    vcl(1,l),vcl(2,l))
  iang(w) = iang(w) - iang(ww)

  if (msglvl == 2) then
    write ( *,600) itophv,w,vcl(1,lv),vcl(2,lv), vcl(1,lw),vcl(2,lw)
  end if

  600 format (1x,2i7,4f15.7)

  return
end
subroutine lfcini ( k, i, ifac, ivrt, indf, npt, sizht, bf, fc, ht, nsmplx, &
  hdavbf, hdavfc, bflag, front, back, top, ind, loc, ierr )
!
!******************************************************************************
!
!! LFCINI initializes two lists of faces.
!
!
!  Purpose: 
!
!    Initialize two lists of faces and delete some simplices,
!    faces from insertion of vertex I in interior or on boundary
!    of K-D triangulation.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of triangulation.
!
!    Input, I - (local) index of next vertex inserted in triangulation.
!
!    Input, IFAC - index of FC indicating simplex or face containing I.
!
!    Input, IVRT - K+1 or K+2 to indicate that FC(IVRT,IFAC) is (K+1)st
!    vertex of simplex containing I in its interior;
!    K if I lies in interior of face FC(*,IFAC); 2 to K-1 if
!    I lies in interior of facet of FC(*,IFAC) of dim IVRT-1.
!
!    Input, INDF(1:IVRT) - if 2 <= IVRT <= K-1 then IVRT elements are
!    local vertex indices in increasing order of (IVRT-1)-
!    facet containing I in its interior, else not referenced.
!
!    Input, NPT - number of K-D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, BF(1:K,1:*) -  array of boundary face records; see DTRISK.
!
!    Input/output, FC(1:K+4,1:*) - array of face records; see routine DTRISK.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NSMPLX - number of simplices in triangulation.
!
!    Input/output, HDAVBF - head pointer to available BF records.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Output, BFLAG - .TRUE. iff vertex I is on boundary of triangulation.
!
!    Output, FRONT, BACK - indices of front and back of queue of interior
!    faces that may form a new simplex with vertex I.
!
!    Output, TOP - index of top of stack of boundary faces that form a new
!    simplex with vertex I.
!
!    Workspace, IND(1:K) - local vertex indices of K-D vertices.
!
!    Workspace, LOC(1:K) - permutation of 1 to K.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer sizht
!
  integer a
  integer back
  integer bf(k,*)
  logical bface
  logical bflag
  integer botd
  integer d
  integer e
  integer fc(k+4,*)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrck
  integer i
  integer ierr
  integer ifac
  integer ii
  integer ind(k)
  integer indf(k-1)
  integer iv
  integer ivp1
  integer ivrt
  integer j
  integer jj
  integer kbf
  integer kif
  integer kp1
  integer kp2
  integer kp4
  integer kv
  integer l
  integer loc(k)
  integer, parameter :: msglvl = 0
  integer npt
  integer nsmplx
  integer pos
  integer ptr
  integer top
  integer topd
!
  ierr = 0
  kp1 = k + 1
  kp2 = k + 2
  kp4 = k + 4
  front = 0
  back = 0
  top = 0
  bflag = .false.
!
!  Vertex I is in interior of simplex.
!
  if (ivrt >= kp1) then

    nsmplx = nsmplx - 1
    d = fc(ivrt,ifac)

    if (msglvl == 4) then
      write ( *,600) (fc(ii,ifac),ii=1,k),d
    end if

    do j = 0,k

      ind(1:k) = fc(1:k,ifac)

      if (j == 0) then

        a = d
        pos = ifac

      else

        a = ind(j)
        ind(j) = d
        pos = htsrck(k,ind,npt,sizht,fc,ht)

        if (pos <= 0) then
          ierr = 400
          return
        end if

      end if

      if (fc(kp1,pos) == a) then
        fc(kp1,pos) = i
      else
        fc(kp2,pos) = i
      end if

      if (fc(kp2,pos) >= 0) then

        if (front == 0) then
          front = pos
        else
          fc(kp4,back) = pos
        end if

        back = pos

      else

        fc(kp4,pos) = top
        top = pos
      end if

    end do
!
!  Vertex I is in interior of face.
!
  else if (ivrt == k) then

    e = fc(kp2,ifac)
    bflag = (e < 0)

    if (bflag) then
      kv = kp1
      nsmplx = nsmplx - 1
      e = -e
      bf(1,e) = -hdavbf
      hdavbf = e
    else
      kv = kp2
      nsmplx = nsmplx - 2
    end if

    do iv = kp1,kv

      d = fc(iv,ifac)
      if (msglvl == 4) then
        write ( *,600) (fc(ii,ifac),ii=1,k),d
      end if

      do j = 1,k

        ind(1:k) = fc(1:k,ifac)
        a = ind(j)
        ind(j) = d
        pos = htsrck(k,ind,npt,sizht,fc,ht)

        if (pos <= 0) then
          ierr = 400
          return
        end if

        if (fc(kp1,pos) == a) then
          fc(kp1,pos) = i
        else
          fc(kp2,pos) = i
        end if

        if (fc(kp2,pos) >= 0) then

          if (front == 0) then
            front = pos
          else
            fc(kp4,back) = pos
          end if
          back = pos
        else
          fc(kp4,pos) = top
          top = pos
        end if

      end do

    end do

    call htdelk(k,ifac,npt,sizht,fc,ht)
    fc(1,ifac) = -hdavfc
    hdavfc = ifac
!
!  Vertex I is in interior of facet of dimension IVRT-1.
!
  else

    ivp1 = ivrt + 1
    kbf = 0
    kif = 0
    topd = ifac
    botd = topd
    ptr = topd
    fc(kp4,topd) = 0

60  continue

    j = 0
    jj = ivrt
    l = 1

    do ii = 1,k

      if (fc(ii,ptr) == indf(l)) then
        j = j + 1
        loc(j) = ii
        if (l < ivrt) l = l + 1
      else
        jj = jj + 1
        loc(jj) = ii
      end if

    end do

    e = fc(kp2,ptr)
    bface = (e < 0)

    if (bface) then
      bflag = .true.
      kv = kp1
      kbf = kbf + 1
      e = -e
      bf(1,e) = -hdavbf
      hdavbf = e
    else
      kv = kp2
      kif = kif + 1
    end if

    do iv = kp1,kv

      d = fc(iv,ptr)

      do j = 1,ivrt

        ind(1:k) = fc(1:k,ptr)
        a = ind(loc(j))
        ind(loc(j)) = d
        pos = htsrck(k,ind,npt,sizht,fc,ht)

        if (pos <= 0) then
          ierr = 400
          return
        end if

        if (j == 1) then

          if (fc(kp1,pos) == i .or. fc(kp2,pos) ==i) then
            go to 100
          else
            if (msglvl == 4) then
              write ( *,600) (fc(ii,ptr),ii=1,k),d
            end if
          end if

        end if

        if (fc(kp1,pos) == a) then
          fc(kp1,pos) = i
        else
          fc(kp2,pos) = i
        end if

        if (fc(kp2,pos) >= 0) then

          if (front == 0) then
            front = pos
          else
            fc(kp4,back) = pos
          end if

          back = pos

        else

          fc(kp4,pos) = top
          top = pos

        end if

      end do

100   continue

      do j = ivp1,k

        ind(1:k) = fc(1:k,ptr)
        ind(loc(j)) = d
        pos = htsrck(k,ind,npt,sizht,fc,ht)

        if (pos <= 0) then
          ierr = 400
          return
        end if

        if (fc(kp4,pos) == -1) then
          fc(kp4,botd) = pos
          fc(kp4,pos) = 0
          botd = pos
        end if

      end do

    end do

    ptr = fc(kp4,ptr)
    if (ptr /= 0) go to 60

    nsmplx = nsmplx - (kbf + kif + kif)/(kp1 - ivrt)

140 continue

    ptr = topd
    topd = fc(kp4,ptr)
    call htdelk(k,ptr,npt,sizht,fc,ht)
    fc(1,ptr) = -hdavfc
    hdavfc = ptr
    if (topd /= 0) go to 140

  end if

  if (front /= 0) fc(kp4,back) = 0

  600 format (1x,'deleted simplex:',9i7)

  return
end
subroutine lop ( itr, ind, mxedg, top, ldv, vcl, til, tedg, sptr )
!
!*******************************************************************************
!
!! LOP applies the local optimization procedure to two triangles.
!
!
!  Purpose:
!
!    Apply local optimization procedure to two triangles
!    indicated by ITR(1) and ITR(2). This may result in swapping
!    diagonal edge of quadrilateral.
!
!  Modified:
!
!    12 July 1999
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, integer ITR(1:2), the indices of triangles for LOP.
!
!    Input, integer IND(1:2), indices indicating common edge of triangles.
!
!    Input, integer MXEDG, the maximum index of edge to be considered for LOP.
!
!    Input/output, integer TOP, the index of SPTR indicating top of stack.
!
!    Input, integer LDV, the leading dimension of VCL in calling routine.
!
!    Input, double precision VCL(1:2,1:*), the vertex coordinate list.
!
!    Input/output, integer TIL(1:3,1:*), the triangle incidence list.
!
!    Input/output, integer TEDG(1:3,1:*), the triangle edge indices;
!    see routine CVDTRI.
!
!    Input/output, integer SPTR(1:*), stack pointers; see routine CVDTRI.
!
  implicit none
!
  integer ldv
!
  integer a
  integer b
  integer c
  integer d
  integer diaedg
  integer i
  integer i_wrap
  integer iedg
  integer in
  integer ind(2)
  integer ind1m1
  integer ind1p1
  integer ind2m1
  integer ind2p1
  integer itr(2)
  integer j
  integer mxedg
  integer top
  integer sptr(*)
  integer tedg(3,*)
  integer til(3,*)
  double precision vcl(ldv,*)
!
!  Common edge is BC, other two vertices are A and D.
!
  iedg = tedg(ind(1),itr(1))
  sptr(iedg) = -1

  ind1m1 = i_wrap ( ind(1) - 1, 1, 3 )
  ind1p1 = i_wrap ( ind(1) + 1, 1, 3 )
  ind2m1 = i_wrap ( ind(2) - 1, 1, 3 )
  ind2p1 = i_wrap ( ind(2) + 1, 1, 3 )

  b = til(ind(1),itr(1))
  c = til(ind1p1,itr(1))
  a = til(ind1m1,itr(1))
  d = til(ind2m1,itr(2))

  in = diaedg ( vcl(1,d), vcl(2,d), vcl(1,c), vcl(2,c), vcl(1,a), vcl(2,a), &
    vcl(1,b), vcl(2,b) )

  if ( in == 1 ) then
!
!  Check if four edges of quadrilateral should be put on LOP
!  stack, and swap edge BC for AD.
!
   i = tedg(ind1m1,itr(1))

   do j = 1, 4

      if ( j == 2 ) then
        i = tedg(ind1p1,itr(1))
      else if ( j == 3 ) then
        i = tedg(ind2m1,itr(2))
      else if ( j == 4 ) then
        i = tedg(ind2p1,itr(2))
      end if

      if ( i <= mxedg ) then
        if ( sptr(i) == -1 ) then
          sptr(i) = top
          top = i
        end if
      end if

    end do

    til(ind1p1,itr(1)) = d
    til(ind2p1,itr(2)) = a
    tedg(ind(1),itr(1)) = tedg(ind2p1,itr(2))
    tedg(ind(2),itr(2)) = tedg(ind1p1,itr(1))
    tedg(ind1p1,itr(1)) = iedg
    tedg(ind2p1,itr(2)) = iedg

  end if

  return
end
function lrline ( xu, yu, xv1, yv1, xv2, yv2, dv )
!
!******************************************************************************
!
!! LRLINE determines whether a point is left, right, or on a directed line.
!
!
!  Purpose: 
!
!    Determine whether a point is to the left of, right of,
!    or on a directed line parallel to a line through given points.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, XU, YU, XV1, YV1, XV2, YV2 - vertex coordinates; the directed
!    line is parallel to and at signed distance DV to the
!    left of the directed line from (XV1,YV1) to (XV2,YV2);
!    (XU,YU) is the vertex for which the position
!    relative to the directed line is to be determined.
!
!    Input, DV - signed distance (positive for left).
!
!    Output, LRLINE - +1, 0, or -1 depending on whether (XU,YU) is
!    to the right of, on, or left of the directed line
!    (0 if line degenerates to a point).
!
  implicit none
!
  double precision dv
  double precision dx
  double precision dxu
  double precision dy
  double precision dyu
  integer lrline
  double precision t
  double precision tol
  double precision tolabs
  double precision xu
  double precision xv1
  double precision xv2
  double precision yu
  double precision yv1
  double precision yv2
!
  tol = 100.0D+00 * epsilon ( tol )
  dx = xv2 - xv1
  dy = yv2 - yv1
  dxu = xu - xv1
  dyu = yu - yv1
  tolabs = tol * max(abs(dx),abs(dy),abs(dxu),abs(dyu),abs(dv))
  t = dy*dxu - dx*dyu

  if (dv /= 0.0d0) then
    t = t + dv*sqrt(dx**2 + dy**2)
  end if

  lrline = int(sign(1.0d0,t))

  if ( abs(t) <= tolabs ) then
    lrline = 0
  end if

  return
end
subroutine lsrct3 ( pt, n, p, nfc, vcl, vm, fc, ht, ifac, ivrt, ierr )
!
!******************************************************************************
!
!! LSRCT3 searches a 3D triangulation for the tetrahedron containing a point.
!
!
!  Purpose: 
!
!    Perform linear search through 3D triangulation to find
!    a tetrahedron containing point PT.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, PT(1:3) - 3D point.
!
!    Input, N - upper bound on vertex indices or size of VM.
!
!    Input, P - size of hash table.
!
!    Input, NFC - number of positions used in FC array.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:N) - vertex mapping list (maps from local indices used in
!    FC to indices of VCL).
!
!    Input, FC(1:7,1:*) - array of face records; see routine DTRIS3.
!
!    Input, HT(0:P-1) - hash table using direct chaining.
!
!    Output, IFAC - index of FC indicating tetrahedron or face containing PT
!    or 0 if PT outside convex hull.
!
!    Output, IVRT - 4 or 5 to indicate that FC(IVRT,IFAC) is 4th vertex of
!    tetrahedron containing PT in its interior; 6 if PT lies
!    in interior of face FC(*,IFAC); 1, 2, or 3 if PT lies on
!    interior of edge of face from vertices FC(IVRT,IFAC) to
!    FC(IVRT mod 3 + 1,IFAC); -1, -2, or -3 if PT is (nearly)
!    vertex FC(-IVRT,IFAC); 0 if PT lies outside convex hull.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer n
  integer p
!
  integer a
  integer aa
  integer b
  integer bb
  integer c
  integer d
  logical degen
  integer f
  integer fc(7,*)
  integer ht(0:p-1)
  integer htsrc
  integer i
  integer ierr
  integer ifac
  integer ivrt
  integer j
  integer k
  integer nfc
  double precision pt(3)
  double precision t(4)
  integer va
  integer vb
  integer vc
  double precision vcl(3,*)
  integer vd
  integer vm(n)
  logical zero(4)
!
  ierr = 0

  do f = 1, nfc

    a = fc(1,f)

    if (a <= 0) then
      cycle
    end if

    b = fc(2,f)
    c = fc(3,f)
    ifac = f

    do i = 4, 5

      d = fc(i,f)

      if (d <= c) then
        cycle
      end if

      va = vm(a)
      vb = vm(b)
      vc = vm(c)
      vd = vm(d)

      call baryth(vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd),pt,t, &
        degen)

      if (degen) then
        ierr = 301
        return
      end if

      if ( t(1) > 0.0d0 .and. t(2) > 0.0d0 .and. &
           t(3) > 0.0d0 .and. t(4) > 0.0d0) then
        ivrt = i
        return
      else if ( t(1) < 0.0d0 .or. t(2) < 0.0d0 .or. &
        t(3) < 0.0d0 .or. t(4) < 0.0d0) then
        cycle
      end if
!
!  All T(J) >= 0.0D0 and at least one T(J) == 0.0D0
!
      k = 0
      do j = 1,4
        zero(j) = (t(j) == 0.0d0)
        if (zero(j)) k = k + 1
      end do

      if (k == 1) then

        ivrt = 6

        if (zero(1)) then
          ifac = htsrc(b,c,d,n,p,fc,ht)
          if (ifac <= 0) go to 40
        else if (zero(2)) then
          ifac = htsrc(a,c,d,n,p,fc,ht)
          if (ifac <= 0) go to 40
        else if (zero(3)) then
          ifac = htsrc(a,b,d,n,p,fc,ht)
          if (ifac <= 0) go to 40
        end if

      else if (k == 2) then

        if (zero(4)) then

          if (zero(3)) then
            ivrt = 1
          else if (zero(1)) then
            ivrt = 2
          else
            ivrt = 3
          end if

        else

          if (zero(3)) then

            ifac = htsrc(a,b,d,n,p,fc,ht)
            if (ifac <= 0) go to 40

            if (zero(2)) then
              aa = a
            else
              aa = b
            end if

          else

            ifac = htsrc(a,c,d,n,p,fc,ht)

            if (ifac <= 0) then
              ierr = 300
              return
            end if

            aa = c
          end if

          bb = d

          if (aa > bb) then
            call i_swap ( aa, bb )
          end if

          if (fc(1,ifac) == aa) then
            if (fc(2,ifac) == bb) then
              ivrt = 1
            else
              ivrt = 3
            end if
          else
            ivrt = 2
          end if

        end if

      else
!
!  K == 3
!
        if (.not. zero(1)) then

          ivrt = -1

        else if (.not. zero(2)) then

          ivrt = -2

        else if (.not. zero(3)) then

          ivrt = -3

        else

          ifac = htsrc(a,b,d,n,p,fc,ht)

          if (ifac <= 0) then
            ierr = 300
            return
          end if

          if (fc(1,ifac) == d) then
            ivrt = -1
          else if (fc(2,ifac) == d) then
            ivrt = -2
          else
            ivrt = -3
          end if

        end if

      end if

      return

    end do

  end do

  ifac = 0
  ivrt = 0
  return

40 continue

  ierr = 300

  return
end
subroutine lufac ( a, lda, n, tol, ipvt, singlr )
!
!******************************************************************************
!
!! LUFAC factors a matrix.
!
!
!  Purpose: 
!
!    Obtain LU factorization of matrix A, i.e. apply Gaussian
!    elimination with partial pivoting to A.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input/output, A(LDA,N).  On input, the N by N matrix to be factored.
!    On output, the upper triangular matrix U and multipliers of unit
!    lower triangular matrix L (if matrix A is nonsingular).
!
!    Input, LDA - leading dimension of array A in calling routine.
!
!    Input, N - order of matrix A.
!
!    Input, TOL - relative tolerance for detecting singularity of A
!
!    Output, IPVT(1:N-1) - pivot indices.
!
!    Output, SINGLR - .TRUE. iff matrix is singular; this occurs when the
!    magnitude of a pivot element is <= TOL*MAX(|A(I,J)|).
!
  implicit none
!
  integer lda
  integer n
!
  double precision a(lda,n)
  integer i
  integer ipvt(n-1)
  integer j
  integer k
  integer kp1
  integer m
  logical singlr
  double precision t
  double precision tol
  double precision tolabs
!
  if ( n < 1 ) then
    return
  end if

  singlr = .true.

  t = maxval ( abs ( a(1:n,1:n) ) )

  tolabs = tol*t

  do k = 1,n-1
    kp1 = k + 1
    m = k
    do i = kp1,n
      if (abs(a(i,k)) > abs(a(m,k))) m = i
    end do
    ipvt(k) = m
    t = a(m,k)
    a(m,k) = a(k,k)
    a(k,k) = t
    if (abs(t) <= tolabs) return
    a(kp1:n,k) = a(kp1:n,k)/t

    do j = kp1,n
      t = a(m,j)
      a(m,j) = a(k,j)
      a(k,j) = t
      a(kp1:n,j) = a(kp1:n,j) - a(kp1:n,k)*t
    end do

  end do

  if (abs(a(n,n)) > tolabs) then
    singlr = .false.
  end if

  return
end
subroutine lusol ( a, lda, n, ipvt, b )
!
!******************************************************************************
!
!! LUSOL solves a linear system involving a matrix factored by LUFAC.
!
!
!  Purpose: 
!
!    Solve linear system A*X = B given LU factorization of A.
!    It is assumed that A is nonsingular.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A(1:N,1:N) - contains factors L, U output from routine LUFAC.
!
!    Input, LDA - leading dimension of array A in calling routine.
!
!    Input, N - order of matrix A.
!
!    Input, IPVT(1:N-1) - pivot indices from routine LUFAC.
!
!    Input/output, B(1:N).  On input, the right hand side vector.
!    On output, the solution vector X
!
  implicit none
!
  integer lda
  integer n
!
  double precision a(lda,n)
  double precision b(n)
  integer i
  integer ipvt(n-1)
  integer k
  integer m
  double precision t
!
!  Forward elimination
!
  do k = 1, n-1
    m = ipvt(k)
    t = b(m)
    b(m) = b(k)
    b(k) = t
    do i = k+1,n
      b(i) = b(i) - a(i,k)*t
    end do

  end do
!
!  Back substitution
!
  do k = n,2,-1
    t = b(k)/a(k,k)
    b(k) = t
    b(1:k-1) = b(1:k-1) - a(1:k-1,k)*t
  end do

  b(1) = b(1) / a(1,1)

  return
end
function mdf2 ( x, y, wsq, nev, ifv, listev, ivrt, edgval, vrtval, vcl )
!
!******************************************************************************
!
!! MDF2 evaluates a heuristic mesh distribution function in 2D.
!
!
!  Purpose: 
!
!    Evaluate heuristic mesh distribution function at (X,Y).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, X, Y - coordinates of point.
!
!    Input, WSQ - square of width of polygon containing (X,Y).
!
!    Input, NEV, IFV, LISTEV(1:NEV) - output from routine PRMDF2.
!
!    Input, IVRT(1:*), EDGVAL(1:*), VRTVAL(1:*) - arrays output from DSMDF2.
!
!    Input, VCL(1:2,1:*) - vertex coordinate list
!
!    Output, MDF2 - reciprocal of square of length scale at (X,Y)
!
  implicit none
!
  integer nev
!
  double precision d
  double precision edgval(*)
  integer i
  integer ifv
  integer ivrt(*)
  integer j 
  integer k
  integer kp1
  integer listev(nev)
  double precision mdf2
  double precision s
  double precision vcl(2,*)
  double precision vrtval(*)
  double precision wsq
  double precision x
  double precision x0
  double precision x1
  double precision y
  double precision y0
  double precision y1
!
  s = wsq

  do i = 1,nev

    k = listev(i)

    if (k < 0) then

      k = -k
      d = (vcl(1,k) - x)**2 + (vcl(2,k) - y)**2
      d = max(0.25d0*d,vrtval(k))
      s = min(s,d)

    else

      kp1 = k + 1
      if (i == nev .and. ifv > 0) kp1 = ifv
      j = ivrt(kp1)
      x0 = x - vcl(1,j)
      y0 = y - vcl(2,j)
      x1 = vcl(1,ivrt(k)) - vcl(1,j)
      y1 = vcl(2,ivrt(k)) - vcl(2,j)

      if (x0*x1 + y0*y1 <= 0.0d0) then
        d = x0**2 + y0**2
      else
        x0 = x0 - x1
        y0 = y0 - y1
        if (x0*x1 + y0*y1 >= 0.0d0) then
          d = x0**2 + y0**2
        else
          d = (x1*y0 - y1*x0)**2/(x1**2 + y1**2)
        end if
      end if

      d = max(0.25d0*d,edgval(k))
      s = min(s,d)

    end if

  end do

  mdf2 = 1.0d0/s

  return
end
function mdf3 ( x, y, z, widp, nfcev, nedev, nvrev, &
  listev, infoev, ivrt, facval, edgval, vrtval, vcl )
!
!******************************************************************************
!
!! MDF3 evaluates a heuristic mesh distribution function in 3D.
!
!
!  Purpose: 
!
!    Evaluate heuristic mesh distribution function at (X,Y,Z).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, X,Y,Z - coordinates of 3D point.
!
!    Input, WIDP - width of polyhedron containing (X,Y,Z).
!
!    Input, NFCEV,NEDEV,NVREV,LISTEV(1:NFCEV+NEDEV+NVREV),
!    INFOEV(1:4,1:NFCEV+NEDEV) - output from routine PRMDF3.
!
!    Input, IVRT(1:*),FACVAL(1:*),EDGVAL(1:*),VRTVAL(1:*) - arrays output
!    from routine DSMDF3.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list
!
!    Output, MDF3 - reciprocal of cube of length scale at (X,Y,Z)
!
  implicit none
!
  double precision cp(3)
  double precision d
  double precision dir(3)
  double precision edgval(*)
  double precision facval(*)
  integer i
  double precision infoev(4,*)
  integer ivrt(*)
  integer j
  integer k
  integer l
  integer listev(*)
  double precision mdf3
  integer nedev
  integer nfcev
  integer nvrev
  double precision s
  double precision vcl(3,*)
  double precision vrtval(*)
  double precision widp
  double precision x
  double precision y
  double precision z
!
  s = widp
  k = 0

  do i = 1,nfcev
    k = k + 1
    d = abs(infoev(1,k)*x + infoev(2,k)*y + infoev(3,k)*z - infoev(4,k))
    d = max(0.5d0*d,facval(listev(k)))
    s = min(s,d)
  end do

  do i = 1,nedev
    k = k + 1
    j = listev(k)
    l = ivrt(j)
    dir(1) = x - vcl(1,l)
    dir(2) = y - vcl(2,l)
    dir(3) = z - vcl(3,l)
    cp(1) = infoev(2,k)*dir(3) - infoev(3,k)*dir(2)
    cp(2) = infoev(3,k)*dir(1) - infoev(1,k)*dir(3)
    cp(3) = infoev(1,k)*dir(2) - infoev(2,k)*dir(1)
    d = sqrt(cp(1)**2 + cp(2)**2 + cp(3)**2)/infoev(4,k)
    d = max(0.5d0*d,edgval(j))
    s = min(s,d)
  end do

  do i = 1,nvrev
    k = k + 1
    j = listev(k)
    d = sqrt((vcl(1,j) - x)**2 + (vcl(2,j) - y)**2 + (vcl(3,j) - z)**2)
    d = max(0.5d0*d,vrtval(j))
    s = min(s,d)
  end do

  mdf3 = 1.0d0/s**3

  return
end
subroutine mfdec2 ( hflag, umdf, kappa, angspc, angtol, dmin, nmin, ntrid, &
  nvc, npolg, nvert, maxvc, maxhv, maxpv, maxiw, maxwk, vcl, regnum, hvl, &
  pvl, iang, ivrt, xivrt, widsq, edgval, vrtval, area, psi, iwk, wk, ierr )
!
!******************************************************************************
!
!! MFDEC2 further divides convex polygons to limit mesh function variation.
!
!
!  Purpose: 
!
!    Further subdivide convex polygons so that the variation
!    of heuristic or user-supplied mesh distribution function in
!    each polygon is limited.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, HFLAG - .TRUE. if heuristic mdf, .FALSE. if user-supplied mdf.
!
!    Input, UMDF(X,Y) - d.p user-supplied mdf with d.p arguments.
!
!    Input, KAPPA - mesh smoothness parameter in interval [0.0,1.0].
!
!    Input, ANGSPC - angle spacing parameter in radians used to determine
!    extra points as possible endpoints of separators.
!
!    Input, ANGTOL - angle tolerance parameter in radians used in
!    accepting separators.
!
!    Input, DMIN - parameter used to determine if variation of mdf in
!    polygon is 'sufficiently high'.
!
!    Input, NMIN - parameter used to determine if 'sufficiently large'
!    number of triangles in polygon.
!
!    Input, NTRID - desired number of triangles in mesh.
!
!    Input/output, NVC - number of vertex coordinates or positions used in VCL
!    array.
!
!    Input/output, NPOLG - number of polygonal subregions or positions used in
!    HVL array.
!
!    Input/output, NVERT - number of polygon vertices or positions used in PVL
!    array.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXHV - maximum size available for HVL,REGNUM,AREA,PSI arrays.
!
!    Input, MAXPV - maximum size available for PVL, IANG arrays.
!
!    Input, MAXIW - maximum size available for IWK array; should be about
!    3*NVRT + INT(2*PI/ANGSPC) where NVRT is maximum number of
!    vertices in a convex polygon of the (input) decomposition.
!
!    Input, MAXWK - maximum size available for WK array; should be about
!    NPOLG + 3*(NVRT + INT(2*PI/ANGSPC)) + 2.
!
!    Input/output, VCL(1:2,1:NVC) - vertex coordinate list.
!
!    Input/output, REGNUM(1:NPOLG) - region numbers.
!
!    Input/output, HVL(1:NPOLG) - head vertex list.
!
!    Input/output, PVL(1:4,1:NVERT), IANG(1:NVERT) - polygon vertex list and
!    interior angles.
!
!    Input, IVRT(1:NVERT), XIVRT(1:NPOLG+1), WIDSQ(1:NPOLG), EDGVAL(1:NVERT),
!    VRTVAL(1:NVC) - arrays output from routine DSMDF2;
!    if .NOT. HFLAG then only first two arrays exist.
!
!    Input/output, AREA(1:NPOLG) - area of convex polygons in decomposition.
!
!    Output, PSI(1:NPOLG) - mean mdf values in the convex polygons.
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxhv
  integer maxiw
  integer maxpv
  integer maxvc
  integer maxwk
  integer npolg
  integer nvc
  integer nvert
!
  double precision alpha
  double precision angsp2
  double precision angspc
  double precision angtol
  double precision area(maxhv)
  double precision areapg
  double precision arearg
  double precision c1
  double precision c2
  double precision cosalp
  double precision ctrx
  double precision ctry
  double precision delta
  double precision dmin
  double precision dx
  double precision dy
  integer, parameter :: edgv = 4
  double precision edgval(nvert)
  logical hflag
  integer hvl(maxhv)
  integer i
  integer i1
  integer i2
  double precision iang(maxpv)
  integer ierr
  integer ifv
  integer ii
  integer inc
  integer indpvl
  double precision intreg
  integer ivrt(nvert)
  integer iwk(maxiw)
  integer j
  integer k
  double precision kappa
  integer l
  integer listev
  integer, parameter :: loc = 1
  integer m
  integer maxn
  double precision mdfint
  integer mdftr
  double precision mean
  integer nev
  integer nmin
  integer np
  integer ntrid
  double precision numer
  integer nvrt
  double precision nwarea
  integer p
  double precision pi
  double precision pi2
  integer, parameter :: polg = 2
  double precision psi(maxhv)
  integer pvl(4,maxpv)
  double precision r
  integer regnum(maxhv)
  double precision sinalp
  double precision stdv
  integer, parameter :: succ = 3
  double precision sumx
  double precision sumy
  double precision theta1
  double precision theta2
  double precision tol
  double precision, external :: umdf
  integer v
  double precision vcl(2,maxvc)
  double precision vrtval(nvc)
  integer w
  double precision widsq(npolg)
  double precision wk(maxwk)
  double precision wsq
  double precision x1
  double precision x2
  integer xc
  integer xivrt(npolg+1)
  double precision y1
  double precision y2
  integer yc
!
!  WK(1:NPOLG) is used for mdf standard deviation in polygons.
!  Compute AREARG = area of region and INTREG = estimated integral
!  of MDF2(X,Y) or UMDF(X,Y).
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  nvrt = 0

  do i = 1, npolg
    nvrt = max(nvrt,xivrt(i+1)-xivrt(i))
  end do

  if (hflag .and. 2*nvrt > maxiw) then
    ierr = 6
    return
  else if (npolg + 3*nvrt + 2 > maxwk) then
    ierr = 7
    return
  end if

  listev = 1
  xc = npolg + 1
  yc = xc + nvrt + 1
  mdftr = yc + nvrt + 1
  arearg = 0.0d0
  intreg = 0.0d0
  nev = -1

  do i = 1,npolg

    if (hflag) then
      wsq = widsq(i)
      call prmdf2(i,wsq,ivrt,xivrt,edgval,vrtval,nev,ifv,iwk(listev))
    end if

    if (nev == 0) then

      psi(i) = 1.0d0/wsq
      wk(i) = 0.0d0
      mdfint = psi(i)*area(i)

    else

      nvrt = xivrt(i+1) - xivrt(i)
      k = xivrt(i)
      sumx = 0.0d0
      sumy = 0.0d0

      do j = 0,nvrt-1
        l = ivrt(k)
        wk(xc+j) = vcl(1,l)
        wk(yc+j) = vcl(2,l)
        sumx = sumx + wk(xc+j)
        sumy = sumy + wk(yc+j)
        k = k + 1
      end do

      ctrx = sumx/dble(nvrt)
      ctry = sumy/dble(nvrt)

      do j = 0,nvrt-1
        wk(xc+j) = wk(xc+j) - ctrx
        wk(yc+j) = wk(yc+j) - ctry
      end do

      wk(xc+nvrt) = wk(xc)
      wk(yc+nvrt) = wk(yc)
      call intpg(nvrt,wk(xc),wk(yc),ctrx,ctry,area(i),hflag,umdf, &
        wsq,nev,ifv,iwk(listev),ivrt,edgval,vrtval,vcl,mdfint, &
        psi(i),wk(i),wk(mdftr))

    end if

    arearg = arearg + area(i)
    intreg = intreg + mdfint

  end do
!
!  If HFLAG, compute mean mdf values from KAPPA, etc.  Scale PSI(I)'s
!  so that integral in region is 1. Determine which polygons need to
!  be further subdivided (indicated by negative PSI(I) value).
!
  if (hflag) then
    c1 = (1.0d0 - kappa)/intreg
    c2 = kappa/arearg
  else
    c1 = 1.0d0/intreg
    c2 = 0.0d0
  end if

  do i = 1,npolg
    psi(i) = psi(i)*c1 + c2
    if (c1*wk(i) > psi(i)*dmin) then
      if (ntrid*psi(i)*area(i) > nmin) then
        psi(i) = -psi(i)
      end if
    end if
  end do
!
!  Further subdivide polygons for which STDV/MEAN > DMIN and
!  (estimated number of triangles) > NMIN.
!
  angsp2 = 2.0d0*angspc
  pi2 = 2.0d0 * pi()
  inc = int(pi2/angspc)
  nev = 0
  np = npolg
  xc = 1

  do i = 1,np

    if (psi(i) < 0.0d0) then

      if (hflag) then
        wsq = widsq(i)
        call prmdf2(i,wsq,ivrt,xivrt,edgval,vrtval,nev,ifv,iwk(listev))
      end if

      l = npolg + 1
      k = i

60    continue

      if (k > npolg) go to 130

70    continue

      if (psi(k) >= 0.0d0) go to 120
      nvrt = 0
      sumx = 0.0d0
      sumy = 0.0d0
      j = hvl(k)

80    continue

      nvrt = nvrt + 1
      m = pvl(loc,j)
      sumx = sumx + vcl(1,m)
      sumy = sumy + vcl(2,m)
      j = pvl(succ,j)
      if (j /= hvl(k)) go to 80

      ctrx = sumx/dble(nvrt)
      ctry = sumy/dble(nvrt)
      maxn = nvrt + inc

      if (nev + maxn + 1 > maxiw) then
        ierr = 6
        return
      else if (3*maxn + 2 > maxwk) then
        ierr = 7
        return
      end if

      yc = xc + maxn + 1
      mdftr = yc + maxn + 1
      indpvl = listev + nev
      nvrt = 0
      m = pvl(loc,j)
      x1 = vcl(1,m) - ctrx
      y1 = vcl(2,m) - ctry
      wk(xc) = x1
      wk(yc) = y1
      theta1 = atan2(y1,x1)
      p = j
      iwk(indpvl) = j

90    continue

      j = pvl(succ,j)
      m = pvl(loc,j)
      x2 = vcl(1,m) - ctrx
      y2 = vcl(2,m) - ctry
      theta2 = atan2(y2,x2)
      if (theta2 < theta1) theta2 = theta2 + pi2
      delta = theta2 - theta1

      if (delta >= angsp2) then

        m = int(delta/angspc)
        delta = delta/dble(m)
        dx = x2 - x1
        dy = y2 - y1
        numer = x1*dy - y1*dx
        alpha = theta1

        do ii = 1,m-1
          alpha = alpha + delta
          cosalp = cos(alpha)
          sinalp = sin(alpha)
          r = numer/(dy*cosalp - dx*sinalp)
          nvrt = nvrt + 1
          wk(xc+nvrt) = r*cosalp
          wk(yc+nvrt) = r*sinalp
          iwk(indpvl+nvrt) = -p
        end do

      end if

      nvrt = nvrt + 1
      wk(xc+nvrt) = x2
      wk(yc+nvrt) = y2
      x1 = x2
      y1 = y2
      theta1 = theta2
      p = j
      iwk(indpvl+nvrt) = j

      if (j /= hvl(k)) go to 90

      call intpg(nvrt,wk(xc),wk(yc),ctrx,ctry,area(k),hflag, &
        umdf,wsq,nev,ifv,iwk(listev),ivrt,edgval,vrtval, &
        vcl,mdfint,mean,stdv,wk(mdftr))

      psi(k) = mean*c1 + c2

      if (c1*stdv > psi(k)*dmin) then

        if (ntrid*psi(k)*area(k) > nmin) then

          call sepmdf(angtol,nvrt,wk(xc),wk(yc),area(k), &
            mean,wk(mdftr),iwk(indpvl),iang,i1,i2)

          if (i1 < 0) then

            if (yc + 3*nvrt > maxwk) then
              ierr = 7
              return
            end if

            call sepshp(angtol,nvrt,wk(xc),wk(yc), &
              iwk(indpvl),iang,i1,i2,wk(yc+nvrt+1), ierr )

            if (ierr /= 0) return

          end if

          if (i1 < 0) then
            ierr = 222
            return
          end if

          v = iwk(indpvl+i1)

          if (v < 0) then
            call insvr2(wk(xc+i1)+ctrx,wk(yc+i1)+ctry,-v, &
              nvc,nvert,maxvc,maxpv,vcl,pvl,iang,v,ierr)
            if (ierr /= 0) return
          end if

          w = iwk(indpvl+i2)

          if (w < 0) then
            call insvr2(wk(xc+i2)+ctrx,wk(yc+i2)+ctry,-w, &
              nvc,nvert,maxvc,maxpv,vcl,pvl,iang,w,ierr)
            if (ierr /= 0) return
          end if

          call insed2(v,w,npolg,nvert,maxhv,maxpv,vcl, &
            regnum,hvl,pvl,iang,ierr)

          if (ierr /= 0) return

          nvrt = 0
          j = hvl(k)

          do

            m = pvl(loc,j)
            wk(xc+nvrt) = vcl(1,m)
            wk(yc+nvrt) = vcl(2,m)
            nvrt = nvrt + 1
            j = pvl(succ,j)
            if (j == hvl(k)) then
              exit
            end if

          end do

          nwarea = areapg(nvrt,wk(xc),wk(yc))*0.5d0
          area(npolg) = area(k) - nwarea
          area(k) = nwarea
          psi(k) = -psi(k)
          psi(npolg) = psi(k)

        end if

      end if

      go to 70

120   continue

      if (k == i) then
        k = l
      else
        k = k + 1
      end if

      go to 60

130   continue

    end if

  end do

  return
end
subroutine mfdec3 ( hflag, umdf, kappa, angacc, angedg, dmin, nmin, ntetd, &
  nsflag, nvc, nface, nvert, npolh, npf, maxvc, maxfp, maxfv, maxhf, maxpf, &
  maxiw, maxwk, vcl, facep, factyp, nrml, fvl, eang, hfl, pfl, ivrt, xivrt, &
  ifac, xifac, wid, facval, edgval, vrtval, vol, psi, htsiz, maxedg, ht, &
  edge, listev, infoev, iwk, wk, ierr )
!
!******************************************************************************
!
!! MFDEC3 subdivides polyhedra to control the mesh distribution function.
!
!
!  Purpose: 
!
!    Further subdivide convex polyhedra so that the variation
!    of heuristic or user-supplied mesh distribution function in
!    each polyhedron is limited.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, HFLAG - .TRUE. if heuristic mdf, .FALSE. if user-supplied mdf.
!
!    Input, UMDF(X,Y,Z) - d.p user-supplied mdf with d.p arguments.
!
!    Input, KAPPA - mesh smoothness parameter in interval [0.0,1.0], used
!    iff HFLAG is .TRUE.
!
!    Input, ANGACC - minimum acceptable dihedral angle in radians produced by
!    cut faces.
!
!    Input, ANGEDG - angle parameter in radians used to determine allowable
!    points on edges as possible endpoints of edges of cut faces.
!
!    Input, DMIN - parameter used to determine if variation of mdf in
!    polyhedron is 'sufficiently high'.
!
!    Input, NMIN - parameter used to determine if 'sufficiently large'
!    number of tetrahedra in polyhedron.
!
!    Input, NTETD - desired number of tetrahedra in mesh.
!
!    Input, NSFLAG - .TRUE. if continue to next polyhedron when no separator
!    face is found for a polyhedron, .FALSE. if terminate with
!    error 336 when no separator face is found for a polyhedron.
!
!    Input/output, NVC - number of vertex coordinates or positions used in VCL.
!
!    Input/output, NFACE - number of faces or positions used in FACEP array.
!
!    Input/output, NVERT - number of positions used in FVL, EANG arrays.
!
!    Input/output, NPOLH - number of polyhedra or positions used in HFL array.
!
!    Input/output, NPF - number of positions used in PFL array.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXFP - maximum size available for FACEP, FACTYP, NRML arrays.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, MAXHF - maximum size available for HFL array.
!
!    Input, MAXPF - maximum size available for PFL array.
!
!    Input, MAXIW - maximum size available for IWK array; should be >=
!    2*(NE + MAX(NF,NV)) where NE, NF, NV are maximum number of
!    edges, faces, vertices in any polyhedron of updated
!    decomposition.
!
!    Input, MAXWK - maximum size available for WK array; should be >=
!    MAX(NPOLH, NE+MAX(2*NF,3*NV)) where NPOLH is input value.
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input/output, FACEP(1:3,1:NFACE) - face pointer list: row 1 is head
!    pointer, rows 2 and 3 are signed polyhedron indices.
!
!    Input/output, FACTYP(1:NFACE) - face types: useful for specifying types of
!    boundary faces; entries must be >= 0; any new interior
!    faces (not part of previous face) has face type set to 0.
!
!    Input/output, NRML(1:3,1:NFACE) - unit normal vectors for faces; outward
!    normal corresponds to counter clockwise traversal of 
!    face from polyhedron with index |FACEP(2,F)|.
!
!    Input/output, FVL(1:6,1:NVERT) - face vertex list; see routine DSPHDC.
!
!    Input/output, EANG(1:NVERT) - angles at edges common to 2 faces in a
!    polyhedron; EANG(J) corresponds to FVL(*,J), determined by EDGC field.
!
!    Input/output, HFL(1:NPOLH) - head pointer to face indices in PFL for each
!    polyhedron.
!
!    Input/output, PFL(1:2,1:NPF) - list of signed face indices for each
!    polyhedron; row 2 used for link.
!
!    Input, IVRT(1:NVERT), XIVRT(1:NFACE+1), IFAC(1:NPF), XIFAC(1:NPOLH+1),
!    WID(1:NPOLH), FACVAL(1:NFACE), EDGVAL(1:NVERT),
!    VRTVAL(1:NVC) - arrays output from routine DSMDF3 if
!    HFLAG is .TRUE.
!
!    Input, HTSIZ - size of hash table HT; should be a prime number which
!    is >= number of vertices in a polyhedron of decomposition.
!
!    Input, MAXEDG - maximum size available for EDGE array; should be >=
!    maximum number of edges in a polyhedron of decomposition.
!
!    Ouptut, VOL(1:NPOLH) - volume of convex polyhedra in decomposition.
!
!    Output, PSI(1:NPOLH) - mean mdf values in the convex polyhedra.
!
!    Workspace, HT(0:HTSIZ-1), EDGE(1:4,1:MAXEDG) - hash table and edge records
!    used to determine entries of LISTEV.
!
!    Workspace, LISTEV(1:*) - used by routines PRMDF3, INTPH; size must be >=
!    NCFACE+NCEDGE+NCVC where NCFACE = max no. of faces in a
!    polyhedron (of input decomposition), NCEDGE = max no. of edges
!    in a polyhedron, NCVC = max no. of vertices in a polyhedron.
!
!    Workspace, INFOEV(1:4,1:*) - used by routines PRMDF3, INTPH; size must be
!    >= NCFACE+NCEDGE.
!
!    [Note: It is assumed there is enough space for the arrays.
!    HT, EDGE, LISTEV and INFOEV are needed only if HFLAG is .TRUE.]
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer htsiz
  integer maxedg
  integer maxfp
  integer maxfv
  integer maxhf
  integer maxiw
  integer maxpf
  integer maxvc
  integer maxwk
  integer nface
  integer npf
  integer npolh
  integer nvc
  integer nvert
!
  logical aflag
  double precision angacc
  double precision angedg
  double precision cntr(3)
  double precision c1
  double precision c2
  integer ccw
  integer cdang
  integer cedge
  double precision dmin
  double precision dtol
  double precision eang(maxfv)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer edge(4,maxedg)
  double precision edgval(nvert)
  integer f
  integer facep(3,maxfp)
  integer, parameter :: facn = 2
  integer factyp(maxfp)
  double precision facval(nface)
  integer fvl(6,maxfv)
  integer hfl(maxhf)
  logical hflag
  integer ht(0:htsiz-1)
  integer i
  integer ierr
  integer ifac(npf)
  integer indf
  double precision infoev(4,*)
  double precision intreg
  integer ivrt(nvert)
  integer iwk(maxiw)
  integer j
  integer k
  double precision kappa
  integer kk
  integer kp
  integer l
  double precision leng
  integer listev(*)
  integer ll
  integer, parameter :: loc = 1
  integer lp
  double precision mdfint
  double precision mean
  integer meanf
  integer, parameter :: msglvl = 0
  double precision mxcos
  integer n
  integer nce
  integer ne
  integer nedev
  integer nf
  integer nfcev
  integer nmin
  integer np
  double precision nrml(3,maxfp)
  double precision nrmlc(4)
  logical nsflag
  integer ntetd
  integer nvcin
  integer nvrev
  integer pfl(2,maxpf)
  integer, parameter :: pred = 4
  double precision psi(maxhf)
  double precision stdv
  integer stdvf
  integer, parameter :: succ = 3
  double precision sum2
  double precision t
  double precision tol
  double precision, external :: umdf
  double precision vcl(3,maxvc)
  double precision vol(maxhf)
  double precision volreg
  double precision vrtval(nvc)
  double precision wid(npolh)
  double precision widp
  double precision wk(maxwk)
  integer xifac(npolh+1)
  integer xivrt(nface+1)
!
!  WK(1:NPOLH) is used for mdf standard deviation in polyhedra.
!  Compute VOLREG = volume of region and INTREG = estimated integral
!  of MDF3(X,Y) or UMDF(X,Y).
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )

  if (npolh > maxwk) then
    ierr = 7
    return
  end if

  nvcin = nvc
  volreg = 0.0d0
  intreg = 0.0d0
  widp = 0.0d0

  do i = 1,npolh

    if (hflag) then
      widp = wid(i)
      call prmdf3(i,widp,nvcin,vcl,nrml,ivrt,xivrt,ifac,xifac, &
        facval,edgval,vrtval,nfcev,nedev,nvrev,listev,infoev, &
        htsiz,maxedg,ht,edge, ierr )
      if (ierr /= 0) return
    end if

    n = 0
    cntr(1:3) = 0.0d0
    j = hfl(i)

10  continue

    f = abs(pfl(1,j))
    k = facep(1,f)

20  continue

    l = fvl(loc,k)
    cntr(1:3) = cntr(1:3) + vcl(1:3,l)
    n = n + 1
    k = fvl(succ,k)
    if (k /= facep(1,f)) go to 20

    j = pfl(2,j)
    if (j /= hfl(i)) go to 10

    cntr(1) = cntr(1)/dble(n)
    cntr(2) = cntr(2)/dble(n)
    cntr(3) = cntr(3)/dble(n)
    call intph(hflag,umdf,hfl(i),widp,nfcev,nedev,nvrev,listev, &
      infoev,ivrt,facval,edgval,vrtval,vcl,facep,fvl,pfl,cntr, &
      mdfint,psi(i),wk(i),vol(i),1,iwk,wk,wk)
    volreg = volreg + vol(i)
    intreg = intreg + mdfint

  end do
!
!  If HFLAG, compute mean mdf values from KAPPA, etc. Scale PSI(I)'s
!  so that integral in region is 1. Determine which polyhedra need to
!  be further subdivided (indicated by negative PSI(I) value).
!
  if (hflag) then
    c1 = (1.0d0 - kappa)/intreg
    c2 = kappa/volreg
  else
    c1 = 1.0d0/intreg
    c2 = 0.0d0
  end if

  do i = 1,npolh
    psi(i) = psi(i)*c1 + c2
    if (c1*wk(i) > psi(i)*dmin) then
      if (ntetd*psi(i)*vol(i) > nmin) then
         psi(i) = -psi(i)
      end if
    end if
  end do
!
!  Further subdivide polygons for which STDV/MEAN > DMIN and
!  (estimated number of tetrahedra) > NMIN.
!
  mxcos = cos(angedg)
  np = npolh
  cedge = 1
  cdang = 1

  do i = 1, np

    if (psi(i) >= 0.0d0) then
      cycle
    end if

    if (hflag) then
      widp = wid(i)
      call prmdf3(i,widp,nvcin,vcl,nrml,ivrt,xivrt,ifac,xifac, &
        facval,edgval,vrtval,nfcev,nedev,nvrev,listev,infoev, &
        htsiz,maxedg,ht,edge, ierr )
      if (ierr /= 0) return
    end if

    lp = npolh + 1
    kp = i

50  continue

    nf = 0
    n = 0
    cntr(1:3) = 0.0d0
    j = hfl(kp)

60  continue

    nf = nf + 1
    f = abs(pfl(1,j))
    k = facep(1,f)

70  continue

    l = fvl(loc,k)
    cntr(1:3) = cntr(1:3) + vcl(1:3,l)
    n = n + 1
    k = fvl(succ,k)
    if (k /= facep(1,f)) go to 70

    j = pfl(2,j)
    if (j /= hfl(kp)) go to 60
    cntr(1:3) = cntr(1:3)/dble(n)
    ne = n/2
    indf = cedge + n
    meanf = ne
    stdvf = meanf + nf

    if (indf + nf + nf - 1 > maxiw) then
      ierr = 6
      return
    else if (stdvf + nf - 1 > maxwk) then
      ierr = 7
      return
    end if

    call intph(hflag,umdf,hfl(kp),widp,nfcev,nedev,nvrev, &
      listev,infoev,ivrt,facval,edgval,vrtval,vcl,facep,fvl, &
      pfl,cntr,mdfint,mean,stdv,vol(kp),nf,iwk(indf), &
      wk(meanf),wk(stdvf))

    psi(kp) = mean*c1 + c2

    if (c1*stdv > psi(kp)*dmin) then

      if (ntetd*psi(kp)*vol(kp) > nmin) then

        sum2 = 0.0d0
        j = hfl(kp)

80      continue

        f = pfl(1,j)

        if (f > 0) then
          ccw = succ
        else
          ccw = pred
          f = -f
        end if

        k = facep(1,f)
        l = fvl(loc,k)

90      continue

        kk = fvl(ccw,k)
        ll = fvl(loc,kk)

        if (l < ll) then
          leng = sqrt((vcl(1,l)-vcl(1,ll))**2+(vcl(2,l) &
            -vcl(2,ll))**2 + (vcl(3,l)-vcl(3,ll))**2)
          sum2 = sum2 + leng
        end if

        k = kk
        l = ll
        if (k /= facep(1,f)) go to 90

        j = pfl(2,j)
        if (j /= hfl(kp)) go to 80

        dtol = tol * sum2 / dble(ne)

        call sfc1mf(kp,cntr,mean,nf,iwk(indf),wk(meanf), &
          angacc,mxcos,dtol,nvc,maxvc,vcl,facep,nrml,fvl, &
          eang,nrmlc,nce,iwk(cedge),wk(cdang),aflag, &
          iwk(indf+nf), ierr )

        if (ierr /= 0) return

        if (aflag) then
          go to 110
        end if

        stdv = -1.0d0

        do j = 0,nf-1

          t = wk(stdvf+j)/wk(meanf+j)

          if (t > stdv) then
            stdv = t
            k = j
          end if

        end do

        f = iwk(indf+k)
        n = ne + 2 - nf

        if (indf + n + n - 1 > maxiw) then
          ierr = 6
          return
        else if (meanf + 3*n - 1 > maxwk) then
          ierr = 7
          return
        end if

        call sfc2mf(kp,f,hflag,umdf,widp,nfcev,nedev,nvrev, &
          listev,infoev,ivrt,facval,edgval,vrtval,cntr, &
          angacc,angedg,mxcos,dtol,nvc,maxvc,vcl,facep,nrml, &
          fvl,eang,nrmlc,nce,iwk(cedge),wk(cdang),aflag, &
          iwk(indf),wk(meanf), ierr )

        if (ierr /= 0) return

        if (aflag) then
          go to 110
        end if

        call sfcshp(kp,hfl(kp),cntr,angacc,mxcos,dtol,nvc, &
          maxvc,vcl,facep,nrml,fvl,eang,pfl,nrmlc,nce, &
          iwk(cedge),wk(cdang),aflag,n,iwk(indf),wk(meanf), ierr )

        if (ierr /= 0) return

        if (.not. aflag) then
          if (nsflag) then
            if (msglvl == 4) then
              write ( *,600)
            end if
            go to 120
          end if
          ierr = 336
          return
        end if

110     continue

        call insfac(kp,nrmlc,nce,iwk(cedge),wk(cdang),nvc, &
          nface,nvert,npolh,npf,maxfp,maxfv,maxhf,maxpf,vcl, &
          facep,factyp,nrml,fvl,eang,hfl,pfl,ierr)

        if (ierr /= 0) return

        psi(kp) = -psi(kp)
        psi(npolh) = psi(kp)

      end if

    end if

    if (psi(kp) < 0.0d0) go to 50

120 continue

    if (kp == i) then
      kp = lp
    else
      kp = kp + 1
    end if

    if (kp <= npolh) go to 50

  end do

  600 format (4x,'*** no separator face found')

  return
end
function minang ( xr, yr, xs, ys, ind, alpha, theta, vcl, pvl, iang )
!
!*******************************************************************************
!
!! MINANG determines the minimum of the boundary angles for a separator.
!
!
!  Purpose:
!
!    Determine the minimum of the 4 angles at the boundary
!    resulting from using edge joining vertices (XR,YR) and
!    (XS,YS) as a separator.
!
!  Modified:
!
!    12 July 1999
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, double precision XR, YR, the coordinates of the reflex vertex.
!
!    Input, double precision XS, YS, the coordinates of other endpoint of
!    possible separator.
!
!    Input, integer IND, if positive then (XS,YS) has index IND in PVL; else
!    (XS,YS) is on edge joining vertices with indices -IND
!    and SUCC(-IND) in PVL.
!
!    Input, double precision ALPHA, the polar angle of (XS,YS) with respect
!    to (XR,YR).
!
!    Input, double precision THETA, the interior angle at reflex vertex.
!
!    Input, double precision VCL(1:2,1:*), the vertex coordinate list.
!
!    Input, integer PVL(1:4,1:*), double precision IANG(1:*), the polygon
!    vertex list, interior angles.
!
!    Output, double precision MINANG, the minimum of the 4 angles in radians.
!
  implicit none
!
  double precision alpha
  double precision ang
  double precision angle
  double precision beta1
  double precision iang(*)
  integer ind
  integer j
  integer l
  integer, parameter :: loc = 1
  double precision minang
  double precision pi
  integer pvl(4,*)
  integer, parameter :: succ = 3
  double precision theta
  double precision vcl(2,*)
  double precision xr
  double precision xs
  double precision yr
  double precision ys
!
  if ( ind > 0 ) then
    j = pvl(succ,ind)
    ang = iang(ind)
  else
    j = pvl(succ,-ind)
    ang = pi()
  end if

  l = pvl(loc,j)
  beta1 = angle ( xr, yr, xs, ys, vcl(1,l), vcl(2,l) )

  minang = min ( alpha, theta - alpha, ang - beta1, beta1 )

  return
end
subroutine mmasep ( angtol, xc, yc, indpvl, iang, v, w, i1, i2 )
!
!******************************************************************************
!
!! MMASEP finds the best of four possible separators.
!
!
!  Purpose: 
!
!    Find best of four possible separators according to
!    max-min angle criterion.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, ANGTOL - angle tolerance parameter (in radians) for accepting
!    separator.
!
!    Input, XC(0:NVRT), YC(0:NVRT) - coordinates of polygon vertices in
!    counter clockwise order where NVRT is number of vertices;
!    (XC(0),YC(0)) = (XC(NVRT),YC(NVRT)).
!
!    Input, INDPVL(0:NVRT) - indices in PVL of vertices; INDPVL(I) = -K if
!    (XC(I),YC(I)) is extra vertex inserted on edge from
!    K to PVL(SUCC,K).
!
!    Input, IANG(1:*) - interior angle array.
!
!    Input, V(1:2), W(1:2) - indices in XC, YC in range 0 to NVRT-1; four
!    possible separators are V(I),W(J), I,J = 1,2.
!
!    Output, I1,I2 - indices in range 0 to NVRT-1 of best separator
!    according to max-min angle criterion; I1 = -1
!    if no satisfactory separator is found.
!
  implicit none
!
  double precision alpha
  double precision angle
  double precision angmax
  double precision angmin
  double precision angtol
  double precision beta
  double precision delta
  double precision gamma
  integer i
  integer i1
  integer i2
  double precision iang(*)
  integer indpvl(0:*)
  integer j
  integer k
  integer l
  integer m
  double precision pi
  double precision tol
  integer v(2)
  integer w(2)
  double precision xc(0:*)
  double precision yc(0:*)
!
  tol = 100.0D+00 * epsilon ( tol )
  angmax = 0.0d0

  do i = 1, 2

    l = v(i)
    k = indpvl(l)

    if ( k > 0 ) then
      alpha = iang(k)
    else
      alpha = pi()
    end if

    do j = 1, 2

      m = w(j)

      if ( l == m ) then
        cycle
      end if

      k = indpvl(m)

      if ( k > 0 ) then
        beta = iang(k)
      else
        beta = pi()
      end if

      gamma = angle(xc(m),yc(m),xc(l),yc(l),xc(l+1),yc(l+1))
      delta = angle(xc(l),yc(l),xc(m),yc(m),xc(m+1),yc(m+1))
      angmin = min(gamma,alpha-gamma,delta,beta-delta)
  
      if ( angmin > angmax ) then
        angmax = angmin
        i1 = l
        i2 = m
      end if

    end do

  end do

  if ( angmax < angtol ) then
    i1 = -1
  end if

  return
end
subroutine mtredg ( utype, i1, i2, i3, ibndry, nt, til, tedg )
!
!*******************************************************************************
!
!! MTREDG sets fields for a triangle as needed by routine TMERGE.
!
!
!  Purpose:
!
!    Set fields for triangle as needed by routine TMERGE.
!
!  Modified:
!
!    12 July 1999
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, logical UTYPE, is .TRUE. iff triangle contains two 'U' vertices.
!
!    Input, integer I1, I2, I3, the indices of 3 triangle vertices in VCL;
!    the first two indices also belong to the next merge edge.
!
!    Input, integer IBNDRY, the index of boundary edge for TEDG.
!
!    Input/output, integer NT, the number of entries in TIL, TEDG so far.
!
!    Input/output, integer TIL(1:NT), the triangle incidence list.
!
!    Input/output, TEDG(1:NT), the triangle edge indices; see routine TMERGE.
!
  implicit none
!
  integer i1
  integer i2
  integer i3
  integer ibndry
  integer nt
  integer tedg(3,*)
  integer til(3,*)
  logical utype
!
  nt = nt + 1
  til(1,nt) = i1
  til(2,nt) = i2
  til(3,nt) = i3
  tedg(1,nt) = nt

  if ( utype ) then
    tedg(2,nt) = nt - 1
    tedg(3,nt) = ibndry
  else
    tedg(2,nt) = ibndry
    tedg(3,nt) = nt - 1
  end if

  return
end
subroutine nwsxed ( k, i, ifac, nv, indf, npt, sizht, nbf, nfc, maxbf, maxfc, &
  bf, fc, ht, nsmplx, hdavbf, hdavfc, front, back, ind, loc, ierr )
!
!******************************************************************************
!
!! NWSXED creates new simplices from insertion of an interior vertex.
!
!
!  Purpose: 
!
!    Create new simplices in K-D triangulation from insertion
!    of vertex I in interior of (NV-1)-facet of FC(*,IFAC).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of triangulation.
!
!    Input, I - (local) index of next vertex inserted in triangulation;
!    it is assumed I is largest index so far.
!
!    Input/output, IFAC - a face containing vertex I is FC(*,IFAC).  On output,
!    new face with vertex I as a vertex.
!
!    Input, NV - number of vertices in facet containing I, 2 <= NV <= K-1.
!
!    Input, INDF(1:NV) - local indices of facet vertices in increasing order.
!
!    Input, NPT - number of K-D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NBF - number of positions used in BF array.
!
!    Input, NFC - number of positions used in FC array.
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input/output, BF(1:K,1:MAXBF) -  array of boundary face records;
!    see DTRISK.
!
!    Input/output, FC(1:K+4,1:MAXFC) - array of face records; see 
!    routine DTRISK.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NSMPLX - number of simplices in triangulation.
!
!    Input/output, HDAVBF - head pointer to available BF records.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Output, FRONT, BACK - indices of front and back of queue of interior
!    faces AB...C such that AB...CI is a new simplex.
!
!    Workspace, IND(1:K) - local vertex indices of K-D vertices.
!
!    Workspace, LOC(1:K) - permutation of 1 to K.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer maxbf
  integer maxfc
  integer nv
  integer sizht
!
  integer a
  integer b
  integer back
  integer bf(k,maxbf)
  logical bface
  integer bot
  integer botn
  integer d
  integer e
  integer fc(k+4,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrck
  integer i
  integer ierr
  integer ifac
  integer ii
  integer ind(k)
  integer indf(nv)
  integer iv
  integer j
  integer jj
  integer km1
  integer kp1
  integer kp2
  integer kp4
  integer kv
  integer l
  integer loc(k)
  integer m
  integer, parameter :: msglvl = 0
  integer nbf
  integer nbr
  integer nbrn
  integer nfc
  integer npt
  integer nsmplx
  integer nvm1
  integer nvp1
  integer pos
  integer ptr
  integer top
  integer topb
  integer topn
!
!  TOP, BOT, PTR are top, bottom, current pointers to list of faces
!  containing given (NV-1)-facet. TOPN, BOTN are top, bottom ptrs to
!  list of new boundary faces, in same relative order as other list.
!
  ierr = 0
  km1 = k - 1
  kp1 = k + 1
  kp2 = k + 2
  kp4 = k + 4
  nvm1 = nv - 1
  nvp1 = nv + 1
  front = 0
  back = 0
  top = ifac
  bot = top
  ptr = top
  fc(kp4,top) = 0
  topn = 0

10 continue

  j = 0
  jj = nv
  l = 1

  do ii = 1,k
    if (fc(ii,ptr) == indf(l)) then
      j = j + 1
      loc(j) = ii
      if (l < nv) l = l + 1
    else
      jj = jj + 1
      loc(jj) = ii
    end if
  end do

  d = fc(kp1,ptr)
  e = fc(kp2,ptr)
  bface = (e <= 0)

  if (bface) then
    kv = kp1
  else
    kv = kp2
  end if

  do j = 1,nv

    call availk(k,hdavfc,nfc,maxfc,fc,pos,ierr)
    if (ierr /= 0) return
    ind(1:k) = fc(1:k,ptr)
    ind(loc(j)) = i
    call htinsk(k,pos,ind,d,e,npt,sizht,fc,ht)
    ifac = pos

    if (bface) then
      if (topn == 0) then
        topn = pos
      else
        fc(kp4,botn) = pos
      end if
      botn = pos
    end if

  end do

  do iv = kp1,kv

    d = fc(iv,ptr)

    do j = 1,nv

      ind(1:k) = fc(1:k,ptr)
      a = ind(loc(j))
      ind(loc(j)) = d
      pos = htsrck(k,ind,npt,sizht,fc,ht)

      if (pos <= 0) then
        ierr = 400
        return
      end if

      if (j == 1) then
        if (fc(kp1,pos) == i .or. fc(kp2,pos) == i) then
          go to 100
        end if
      end if

      if (fc(kp1,pos) == a) then
        fc(kp1,pos) = i
      else
        fc(kp2,pos) = i
      end if

      if (fc(kp2,pos) > 0) then
        if (front == 0) then
          front = pos
        else
          fc(kp4,back) = pos
        end if
        back = pos
      end if

      if (msglvl == 4) then
        write ( *,600) (fc(ii,pos),ii=1,k),i
      end if

    end do

    do j = 1,nvm1
      do jj = j+1,nv
        call availk(k,hdavfc,nfc,maxfc,fc,pos,ierr)
        if (ierr /= 0) return
        ind(1:k) = fc(1:k,ptr)
        a = ind(loc(j))
        ind(loc(j)) = d
        b = ind(loc(jj))
        ind(loc(jj)) = i
        call htinsk(k,pos,ind,a,b,npt,sizht,fc,ht)
      end do
    end do

    nsmplx = nsmplx + nvm1

100 continue

    do j = nvp1,k

      ind(1:k) = fc(1:k,ptr)
      ind(loc(j)) = d
      pos = htsrck(k,ind,npt,sizht,fc,ht)

      if (pos <= 0) then
        ierr = 400
        return
      end if

      if (fc(kp4,pos) == -1) then
        fc(kp4,bot) = pos
        fc(kp4,pos) = 0
        bot = pos
      end if

    end do

  end do

  ptr = fc(kp4,ptr)
  if (ptr /= 0) go to 10

  if (front /= 0) fc(kp4,back) = 0
  if (topn /= 0) fc(kp4,botn) = 0
!
!  Delete faces in TOP-BOT list and process boundary faces.
!
  topb = 0

140 continue

  ptr = top
  top = fc(kp4,top)
  e = fc(kp2,ptr)
  call htdelk(k,ptr,npt,sizht,fc,ht)

  if (e > 0) then

    fc(1,ptr) = -hdavfc
    hdavfc = ptr

  else

    e = -e
    fc(kp2,ptr) = 0
    fc(kp4,ptr) = topb
    topb = ptr
    j = 0
    jj = nv
    l = 1

    do ii = 1,k
      if (fc(ii,ptr) == indf(l)) then
        j = j + 1
        loc(j) = ii
        if (l < nv) l = l + 1
      else
        jj = jj + 1
        loc(jj) = ii
      end if
    end do

    pos = topn

    do j = 1,nv
 
      if (hdavbf /= 0) then
        l = hdavbf
        hdavbf = -bf(1,hdavbf)
      else
        if (nbf >= maxbf) then
          ierr = 23
          return
        end if
        nbf = nbf + 1
        l = nbf
      end if

      ind(j) = topn
      fc(loc(j),ptr) = -topn
      fc(kp2,topn) = -l
      nbr = bf(loc(j),e)
      bf(k,l) = nbr
      m = -fc(kp2,nbr)

      do ii = 1,k
        if (bf(ii,m) == ptr) then
          bf(ii,m) = topn
          exit
        end if
      end do

      topn = fc(kp4,topn)

    end do

    topn = pos

    do j = 1,nv

      l = -fc(kp2,topn)
      iv = nv
      jj = 1
      if (j == jj) jj = 2

      do ii = 1,km1

        if (fc(ii,topn) == indf(jj)) then

          bf(ii,l) = ind(jj)

          if (jj < nv) then
            jj = jj + 1
            if (j == jj .and. jj < nv) jj = jj + 1
          end if

        else

          iv = iv + 1
          nbr = bf(loc(iv),e)

          if (fc(kp2,nbr) < 0) then

            bf(ii,l) = nbr

          else

            a = 0

            do b = 1,k
              nbrn = fc(b,nbr)
              if (nbrn < 0) then
                a = a + 1
                if ( a == j ) then
                  exit
                end if
              end if
            end do

            nbrn = -nbrn
            bf(ii,l) = nbrn
            m = -fc(kp2,nbrn)

            do b = 1,k
              if (bf(b,m) == ptr) then
                bf(b,m) = topn
                go to 220
              end if
            end do

          end if

        end if

220     continue

      end do

      pos = topn
      topn = fc(kp4,topn)
      fc(kp4,pos) = -1

    end do

    bf(1,e) = -hdavbf
    hdavbf = e

  end if

  if (top /= 0) go to 140

  do while (topb > 0) 
    ptr = topb
    topb = fc(kp4,topb)
    fc(1,ptr) = -hdavfc
    hdavfc = ptr
  end do

  600 format (1x,'new simplex: ',9i7)

  return
end
subroutine nwsxfc ( k, i, ifac, npt, sizht, nbf, nfc, maxbf, maxfc, bf, fc, &
  ht, nsmplx, hdavbf, hdavfc, front, back, ind, ierr )
!
!******************************************************************************
!
!! NWSXFC creates new simplices from the insertion of a face vertex.
!
!
!  Purpose: 
!
!    Create new simplices in K-D triangulation from the
!    insertion of vertex I on face FC(*,IFAC).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of triangulation.
!
!    Input, I - (local) index of next vertex inserted in triangulation;
!    it is assumed I is largest index so far.
!
!    Input/output, IFAC - on input, face containing vertex I is FC(*,IFAC).
!    On output, new face with vertex I as a vertex.
!
!    Input, NPT - number of K-D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NBF - number of positions used in BF array.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input/output, BF(1:K,1:MAXBF) -  array of boundary face records; 
!    see DTRISK.
!
!    Input/output, FC(1:K+4,1:MAXFC) - array of face records; see 
!    routine DTRISK.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NSMPLX - number of simplices in triangulation.
!
!    Input/output, HDAVBF - head pointer to available BF records.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Output, FRONT, BACK - indices of front and back of queue of interior
!    faces AB...C such that AB...CI is a new simplex.
!
!    Workspace, IND(1:K) - local vertex indices of K-D vertices.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer maxbf
  integer maxfc
  integer sizht
!
  integer a
  integer b
  integer back
  integer bf(k,maxbf)
  logical bface
  integer d
  integer e
  integer fc(k+4,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrck
  integer i
  integer ierr
  integer ifac
  integer ifacin
  integer ii
  integer ind(k)
  integer iv
  integer j
  integer jj
  integer km1
  integer kp1
  integer kp2
  integer kp4
  integer l
  integer m
  integer, parameter :: msglvl = 0
  integer nbf
  integer nbr
  integer nfc
  integer npt
  integer nsmplx
  integer nv
  integer pos
  integer top
!
  ierr = 0
  km1 = k - 1
  kp1 = k + 1
  kp2 = k + 2
  kp4 = k + 4
  front = 0
  back = 0
  top = 0
  nv = kp2
  e = fc(kp2,ifac)
  bface = (e <= 0)
  if (bface) nv = kp1

  do iv = kp1,nv

    nsmplx = nsmplx + km1
    d = fc(iv,ifac)

    do j = 1,k

      ind(1:k) = fc(1:k,ifac)
      a = ind(j)
      ind(j) = d
      pos = htsrck(k,ind,npt,sizht,fc,ht)

      if (pos <= 0) then
        ierr = 400
        return
      end if

      if (fc(kp1,pos) == a) then
        fc(kp1,pos) = i
      else
        fc(kp2,pos) = i
      end if

      if (fc(kp2,pos) > 0) then
        if (front == 0) then
           front = pos
        else
          fc(kp4,back) = pos
        end if
        back = pos
      end if

      if (msglvl == 4) then
        write ( *,600) (fc(ii,pos),ii=1,k),i
      end if

      if (iv /= kp1) then
        cycle
      end if

      call availk(k,hdavfc,nfc,maxfc,fc,pos,ierr)
      if (ierr /= 0) return
      ind(1:k) = fc(1:k,ifac)
      ind(j) = i
      call htinsk(k,pos,ind,d,e,npt,sizht,fc,ht)

      if (bface) then
        fc(kp4,pos) = top
        top = pos
      end if

    end do

    do j = 1,km1

      do jj = j+1,k

        call availk(k,hdavfc,nfc,maxfc,fc,pos,ierr)
        if (ierr /= 0) return

        l = 0
        do ii = 1,j-1
          l = l + 1
          ind(l) = fc(ii,ifac)
        end do

        do ii = j+1,jj-1
          l = l + 1
          ind(l) = fc(ii,ifac)
        end do

        do ii = jj+1,k
          l = l + 1
          ind(l) = fc(ii,ifac)
        end do

        ind(km1) = d
        ind(k) = i
        a = fc(j,ifac)
        b = fc(jj,ifac)
        call htinsk(k,pos,ind,a,b,npt,sizht,fc,ht)

      end do

    end do

  end do

  if (front /= 0) fc(kp4,back) = 0

  call htdelk(k,ifac,npt,sizht,fc,ht)
  fc(1,ifac) = -hdavfc
  hdavfc = ifac
  ifac = pos
  if (.not. bface) return
  ifacin = hdavfc
  e = -e
  pos = top

  do j = k,1,-1

    if (hdavbf /= 0) then
      l = hdavbf
      hdavbf = -bf(1,hdavbf)
    else
      if (nbf >= maxbf) then
        ierr = 23
        return
      end if
      nbf = nbf + 1
      l = nbf
    end if

    ind(j) = top
    fc(kp2,top) = -l
    nbr = bf(j,e)
    bf(k,l) = nbr
    m = -fc(kp2,nbr)

    do ii = 1,k
      if (bf(ii,m) == ifacin) then
        bf(ii,m) = top
        exit
      end if
    end do

    top = fc(kp4,top)

  end do

  bf(1,e) = -hdavbf
  hdavbf = e
  top = pos

  do j = k,1,-1

    l = -fc(kp2,top)
    ii = 0

    do jj = 1,j-1
      ii = ii + 1
      bf(ii,l) = ind(jj)
    end do

    do jj = j+1,k
      ii = ii + 1
      bf(ii,l) = ind(jj)
    end do

    pos = top
    top = fc(kp4,top)
    fc(kp4,pos) = -1

  end do

  600 format (1x,'new simplex: ',9i7)

  return
end
subroutine nwsxin ( k, i, ifac, ivrt, npt, sizht, nfc, maxfc, fc, ht, nsmplx, &
  hdavfc, front, back, ind, ierr )
!
!******************************************************************************
!
!! NWSXIN creates new simplices from the insertion of an interior vertex.
!
!
!  Purpose: 
!
!    Create new simplices in K-D triangulation from the
!    insertion of vertex I in interior of simplex.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of triangulation.
!
!    Input, I - (local) index of next vertex inserted in triangulation.
!
!    Input, IFAC - face of simplex containing vertex I is FC(*,IFAC).
!
!    Input, IVRT - K+1 or K+2 where K+1st vertex of simplex = FC(IVRT,IFAC).
!
!    Input, NPT - number of K-D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input/output, FC(1:K+4,1:MAXFC) - array of face records; see 
!    routine DTRISK.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NSMPLX - number of simplices in triangulation.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Output, FRONT, BACK - indices of front and back of queue of interior
!    faces AB...C such that AB...CI is a new simplex.
!
!    Workspace, IND(1:K) - local vertex indices of K-D vertices.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer maxfc
  integer sizht
!
  integer a
  integer back
  integer d
  integer e
  integer fc(k+4,maxfc)
  integer front
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrck
  integer i
  integer ierr
  integer ifac
  integer ii
  integer ind(k)
  integer ivrt
  integer j
  integer jj
  integer kp1
  integer kp2
  integer kp4
  integer l
  integer, parameter :: msglvl = 0
  integer nfc
  integer npt
  integer nsmplx
  integer pos
!
  ierr = 0
  kp1 = k + 1
  kp2 = k + 2
  kp4 = k + 4
  front = 0
  back = 0
  nsmplx = nsmplx + k
  d = fc(ivrt,ifac)

  do j = 0,k

    ind(1:k) = fc(1:k,ifac)

    if (j == 0) then
      a = d
      pos = ifac
    else
      a = ind(j)
      ind(j) = d
      pos = htsrck(k,ind,npt,sizht,fc,ht)
      if (pos <= 0) then
        ierr = 400
        return
      end if
    end if

    if (fc(kp1,pos) == a) then
      fc(kp1,pos) = i
    else
      fc(kp2,pos) = i
    end if

    if (fc(kp2,pos) > 0) then
      if (front == 0) then
        front = pos
      else
        fc(kp4,back) = pos
      end if
      back = pos
    end if

    if ( msglvl == 4 ) then
      write ( *,600) (fc(ii,pos),ii=1,k),i
    end if

  end do

  if ( front /= 0 ) then
    fc(kp4,back) = 0
  end if

  a = fc(kp1,ifac)
  fc(kp1,ifac) = d

  do j = 1,k

    do jj = j+1,kp1

      call availk(k,hdavfc,nfc,maxfc,fc,pos,ierr)
      if (ierr /= 0) return

      l = 0
      do ii = 1,j-1
        l = l + 1
        ind(l) = fc(ii,ifac)
      end do

      do ii = j+1,jj-1
        l = l + 1
        ind(l) = fc(ii,ifac)
      end do

      do ii = jj+1,kp1
        l = l + 1
        ind(l) = fc(ii,ifac)
      end do

      ind(k) = i
      d = fc(j,ifac)
      e = fc(jj,ifac)
      call htinsk(k,pos,ind,d,e,npt,sizht,fc,ht)

    end do

  end do

  fc(kp1,ifac) = a

  600 format (1x,'new simplex: ',9i7)

  return
end
subroutine nwsxou ( k, i, npt, sizht, nbf, nfc, maxbf, maxfc, bf, fc, ht, &
  nsmplx, hdavbf, hdavfc, front, back, bfi, ind, ierr )
!
!******************************************************************************
!
!! NWSXOU creates new simplices for vertices outside the current convex hull.
!
!
!  Purpose: 
!
!    Create new simplices in K-D triangulation outside
!    convex hull by joining vertex I to visible boundary faces.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of triangulation.
!
!    Input, I - (local) index of next vertex inserted in triangulation.
!
!    Input, NPT - number of K-D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NBF - number of positions used in BF array.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input/output, BF(1:K,1:MAXBF) -  array of boundary face records; 
!    see DTRISK.
!
!    Input/output, FC(1:K+4,1:MAXFC) - array of face records; see 
!    routine DTRISK.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NSMPLX - number of simplices in triangulation.
!
!    Input/output, HDAVBF - head pointer to available BF records.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Input, FRONT - index of front of queue (or top of stack) of visible
!    boundary faces.
!
!    Output, BACK - index of back of queue (or bottom of stack) of visible
!    boundary faces (which become interior faces).
!
!    Output, BFI - index of FC of a boundary face containing vertex I.
!
!    Workspace, IND(1:K) - local vertex indices of K-D vertices.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer maxbf
  integer maxfc
  integer sizht
!
  integer a
  integer back
  integer bf(k,maxbf)
  integer bfi
  integer bfnew
  integer d
  integer e
  integer fc(k+4,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrck
  integer i
  integer ierr
  integer ii
  integer ind(k)
  integer j
  integer km1
  integer kp1
  integer kp2
  integer kp4
  integer l
  integer m
  integer, parameter :: msglvl = 0
  integer nbf
  integer nbr
  integer nfc
  integer npt
  integer nsmplx
  integer pos
  integer ptr
!
!  For AB...C in queue, form simplex AB...CI + K faces involving I.
!  PTR, NBR, POS are indices of FC; L, M, BFNEW indices of BF.
!
  ierr = 0
  km1 = k - 1
  kp1 = k + 1
  kp2 = k + 2
  kp4 = k + 4
  bfi = 0
  ptr = front

10 continue

  back = ptr
  l = -fc(kp2,ptr)
  fc(kp2,ptr) = i
  nsmplx = nsmplx + 1

  if (msglvl == 4) then
    write ( *,600) (fc(j,ptr),j=1,k),i
  end if

  do e = 1,k

    ind(1:k) = fc(1:k,ptr)
    d = ind(e)
    ind(e) = i
    nbr = bf(e,l)

    if (fc(kp4,nbr) /= -1) then
      if (fc(kp2,nbr) == i) then
        cycle
      end if
    end if

    call availk(k,hdavfc,nfc,maxfc,fc,pos,ierr)
    if (ierr /= 0) return

    m = -fc(kp2,nbr)

    do ii = 1,k
      if (bf(ii,m) == ptr) then
        j = ii
        exit
      end if
    end do

    if (fc(kp4,nbr) /= -1) then

      call htinsk(k,pos,ind,d,fc(j,nbr),npt,sizht,fc,ht)

    else

      if (hdavbf /= 0) then

        bfnew = hdavbf
        hdavbf = -bf(1,hdavbf)

      else

        if (nbf >= maxbf) then
          ierr = 23
          return
        end if

        nbf = nbf + 1
        bfnew = nbf

      end if

      call htinsk(k,pos,ind,d,-bfnew,npt,sizht,fc,ht)
      fc(kp4,pos) = bfi
      bfi = pos
      bf(j,m) = pos
      bf(k,bfnew) = nbr
      bf(1:km1,bfnew) = 0

    end if

  end do

  bf(1,l) = -hdavbf
  hdavbf = l
  ptr = fc(kp4,ptr)
  if (ptr /= 0) go to 10
!
!  Set BF(1:K-1,*) fields for new boundary faces, which are in stack
!  with top pointer BFI.
!
  ptr = bfi

70 continue

  l = -fc(kp2,ptr)

  do e = 1,km1

    if (bf(e,l) > 0) then
      cycle
    end if

    a = fc(e,ptr)
    d = fc(kp1,ptr)

80  continue

    ind(1:k) = fc(1:k,ptr)
    ind(e) = d
    nbr = htsrck(k,ind,npt,sizht,fc,ht)

    if (nbr <= 0) then
      ierr = 400
      return
    end if

    if (fc(kp2,nbr) > 0) then

      if (fc(kp1,nbr) == a) then
        a = d
        d = fc(kp2,nbr)
      else
        a = d
        d = fc(kp1,nbr)
      end if

      go to 80

    end if

    bf(e,l) = nbr
    m = -fc(kp2,nbr)

    do ii = 1,km1
      if (fc(ii,nbr) == d) then
        bf(ii,m) = ptr
        exit
      end if
    end do

  end do

  pos = ptr
  ptr = fc(kp4,ptr)
  fc(kp4,pos) = -1

  if (ptr /= 0) go to 70

  600 format (1x,'new simplex: ',9i7)

  return
end
subroutine nwthed ( i, ifac, iedg, npt, sizht, nbf, nfc, maxbf, maxfc, bf, &
  fc, ht, ntetra, hdavbf, hdavfc, front, back, ierr )
!
!******************************************************************************
!
!! NWTHED creates new tetrahedra from the insertion of a vertex, in 3D.
!
!
!  Purpose: 
!
!    Create new tetrahedra in 3D triangulation from the
!    insertion of vertex I on edge FC(IEDG,IFAC).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, I - (local) index of next vertex inserted in triangulation;
!    it is assumed I is largest index so far.
!
!    Input, IFAC, IEDG - edge containing I is FC(IEDG,IFAC); 1 <= IEDG <= 3.
!
!    Input, NPT - number of 3D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NBF - number of positions used in BF array.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input/output, BF(1:3,1:MAXBF) -  array of boundary face records; 
!    see DTRIS3.
!
!    Input/output, FC(1:7,1:MAXFC) - array of face records; see routine DTRIS3.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NTETRA - number of tetrahedra in triangulation.
!
!    Input/output, HDAVBF - head pointer to available BF records.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Output, FRONT, BACK - indices of front and back of queue of interior
!    faces ABC such that ABCI is a new tetrahedron.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxbf
  integer maxfc
  integer sizht
!
  integer a
  integer aa
  integer b
  integer back
  integer bb
  logical bedge
  integer bf(3,maxbf)
  integer bfn(2)
  integer bfo(2)
  integer c
  integer cp
  integer csav
  integer d
  integer e
  integer fc(7,maxfc)
  integer fcn(2)
  integer fco(2)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrc
  integer i
  integer iedg
  integer ierr
  integer ifac
  integer ind
  integer inew
  integer j
  integer k
  integer l
  integer, parameter :: msglvl = 0
  integer nbf
  integer nbrac
  integer nbrbc
  integer nfc
  integer nfcin
  integer npt
  integer ntetra
!
  ierr = 0
  front = 0
  back = 0

  if (iedg == 1) then
    a = fc(1,ifac)
    b = fc(2,ifac)
    c = fc(3,ifac)
  else if (iedg == 2) then
    a = fc(2,ifac)
    b = fc(3,ifac)
    c = fc(1,ifac)
  else
    a = fc(1,ifac)
    b = fc(3,ifac)
    c = fc(2,ifac)
  end if

  csav = c
  d = fc(4,ifac)
!
!  Determine faces incident on edge AB in circular order, and store
!  their indices at end of FC array.
!
  bedge = .false.
  ind = ifac
  k = 0

  do

    k = k + 1

    if (nfc + k > maxfc) then
      ierr = 11
      return
    end if

    fc(1,nfc+k) = ind
    if (bedge) go to 20
    if (d == csav) go to 50
    ind = htsrc(a,b,d,npt,sizht,fc,ht)

    if (ind <= 0) then
      ierr = 300
      return
    end if

    if (fc(5,ind) <= 0) then
      bedge = .true.
      cp = d
    else if (fc(4,ind) == c) then
      c = d
      d = fc(5,ind)
    else
      c = d
      d = fc(4,ind)
    end if

  end do
!
!  Determine further faces in case of AB being a boundary edge.
!
20 continue

  if (fc(5,ifac) <= 0) go to 50

  l = k

  do j = 1,k/2
    e = fc(1,nfc+j)
    fc(1,nfc+j) = fc(1,nfc+l)
    fc(1,nfc+l) = e
    l = l - 1
  end do

  c = csav
  csav = cp
  d = fc(5,ifac)

  do

    ind = htsrc(a,b,d,npt,sizht,fc,ht)

    if (ind <= 0) then
      ierr = 300
      return
    end if

    k = k + 1

    if (nfc + k > maxfc) then
      ierr = 11
      return
    end if

    fc(1,nfc+k) = ind

    if (fc(5,ind) <= 0) then
      exit
    else if (fc(4,ind) == c) then
      c = d
      d = fc(5,ind)
    else
      c = d
      d = fc(4,ind)
    end if

  end do
!
!  Create new faces and tetrahedra, and add faces to queue.
!
50 continue

  nfcin = nfc
  nfc = nfc + k
  ntetra = ntetra + k

  if (bedge) then
    ntetra = ntetra - 1
    fcn(1) = nfcin + 1
    fcn(2) = nfcin + k
    fco(1) = fc(1,nfcin+1)
    fco(2) = fc(1,nfcin+k)
    bfo(1) = -fc(5,fco(1))
    bfo(2) = -fc(5,fco(2))
  end if

  do j = 1,k

    inew = nfcin + j
    ind = fc(1,inew)

    if (fc(1,ind) == a) then
      if (fc(2,ind) == b) then
        c = fc(3,ind)
      else
        c = fc(2,ind)
      end if
    else
      c = fc(1,ind)
    end if

    d = fc(4,ind)
    e = fc(5,ind)
    call htdel(ind,npt,sizht,fc,ht)
    call htins(ind,a,c,i,d,e,npt,sizht,fc,ht)
    call htins(inew,b,c,i,d,e,npt,sizht,fc,ht)

    if (j == k) then
      if (bedge) then
        cycle
      end if
      d = csav
    else if (j > 1) then
      if (d == cp) d = e
    end if

    call availf(hdavfc,nfc,maxfc,fc,ind,ierr)
    if (ierr /= 0) return
    call htins(ind,c,d,i,a,b,npt,sizht,fc,ht)
    aa = a
    bb = b

    do l = 1,2

      if (l == 2) then
        aa = b
        bb = a
      end if

      ind = htsrc(aa,c,d,npt,sizht,fc,ht)

      if (ind <= 0) then
        ierr = 300
        return
      end if

      if (fc(5,ind) <= 0) then
        fc(4,ind) = i
      else
        if (fc(4,ind) == bb) then
          fc(4,ind) = i
        else
          fc(5,ind) = i
        end if
        if (front == 0) then
          front = ind
        else
          fc(7,back) = ind
        end if
        back = ind
      end if

      if (msglvl == 4) then
        write ( *,600) aa,c,d,i
      end if

    end do

    cp = c

  end do

  if (front /= 0) then
    fc(7,back) = 0
  end if

  if (bedge) then

    d = c
    c = csav

    if (hdavbf /= 0) then

      bfn(1) = hdavbf
      hdavbf = -bf(1,hdavbf)

      if (hdavbf /= 0) then
        bfn(2) = hdavbf
        hdavbf = -bf(1,hdavbf)
      else
        nbf = nbf + 1
        bfn(2) = nbf
      end if

    else

      nbf = nbf + 2
      bfn(1) = nbf - 1
      bfn(2) = nbf

    end if

    if (nbf > maxbf) then
      nbf = maxbf
      ierr = 12
      return
    end if

    fc(5,nfcin+1) = -bfn(1)
    fc(5,nfcin+k) = -bfn(2)

    do j = 1,2

      if (j == 2) c = d

      if (c < a) then
        nbrac = bf(3,bfo(j))
        nbrbc = bf(2,bfo(j))
        bf(1,bfo(j)) = fco(3-j)
        bf(2,bfo(j)) = fcn(j)
        bf(1,bfn(j)) = fcn(3-j)
        bf(2,bfn(j)) = fco(j)
      else if (c < b) then
        nbrac = bf(3,bfo(j))
        nbrbc = bf(1,bfo(j))
        bf(1,bfo(j)) = fcn(j)
        bf(2,bfo(j)) = fco(3-j)
        bf(1,bfn(j)) = fcn(3-j)
        bf(2,bfn(j)) = fco(j)
      else
        nbrac = bf(2,bfo(j))
        nbrbc = bf(1,bfo(j))
        bf(1,bfo(j)) = fcn(j)
        bf(2,bfo(j)) = fco(3-j)
        bf(1,bfn(j)) = fco(j)
        bf(2,bfn(j)) = fcn(3-j)
      end if

      bf(3,bfo(j)) = nbrac
      bf(3,bfn(j)) = nbrbc
      l = -fc(5,nbrbc)

      if (bf(1,l) == fco(j)) then
        bf(1,l) = fcn(j)
      else if (bf(2,l) == fco(j)) then
        bf(2,l) = fcn(j)
      else
        bf(3,l) = fcn(j)
      end if

    end do

  end if

  600 format (1x,'new tetra: ',4i7)

  return
end
subroutine nwthfc ( i, ifac, npt, sizht, nbf, nfc, maxbf, maxfc, bf, fc, ht, &
  ntetra, hdavbf, hdavfc, front, back, ierr )
!
!******************************************************************************
!
!! NWTHFC creates new tetrahedra after the insertion of a new face vertex.
!
!
!  Purpose: 
!
!    Create new tetrahedra in 3D triangulation from the
!    insertion of vertex I on face FC(*,IFAC).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, I - (local) index of next vertex inserted in triangulation;
!    it is assumed I is largest index so far.
!
!    Input, IFAC - face containing vertex I is FC(*,IFAC).
!
!    Input, NPT - number of 3D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NBF - number of positions used in BF array.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input/output, BF(1:3,1:MAXBF) -  array of boundary face records; 
!    see DTRIS3.
!
!    Input/output, FC(1:7,1:MAXFC) - array of face records; see routine DTRIS3.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NTETRA - number of tetrahedra in triangulation.
!
!    Input/output, HDAVBF - head pointer to available BF records.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Output, FRONT, BACK - indices of front and back of queue of interior
!    faces ABC such that ABCI is a new tetrahedron.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxbf
  integer maxfc
  integer sizht
!
  integer a
  integer aa
  integer b
  integer back
  integer bb
  integer bf(3,maxbf)
  integer bf1
  integer bf2
  logical bface
  integer c
  integer cc
  integer d
  integer e
  integer fc(7,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrc
  integer i
  integer ierr
  integer ifac
  integer ind
  integer ind2
  integer iv
  integer j
  integer, parameter :: msglvl = 0
  integer nbf
  integer nbrac
  integer nbrbc
  integer nfc
  integer npt
  integer ntetra
  integer nv
!
  ierr = 0
  front = 0
  back = 0
  nv = 5
  bface = (fc(5,ifac) <= 0)
  if (bface) nv = 4
  a = fc(1,ifac)
  b = fc(2,ifac)
  c = fc(3,ifac)

  do iv = 4,nv

    ntetra = ntetra + 2
    d = fc(iv,ifac)

    do j = 1,3

      if (j == 1) then
        aa = a
        bb = b
        cc = c
      else if (j == 2) then
        aa = b
        bb = c
        cc = a
      else
        aa = c
        bb = a
        cc = b
      end if

      call availf(hdavfc,nfc,maxfc,fc,ind,ierr)
      if (ierr /= 0) return

      call htins(ind,aa,d,i,bb,cc,npt,sizht,fc,ht)
      ind = htsrc(aa,bb,d,npt,sizht,fc,ht)

      if (ind <= 0) then
        ierr = 300
        return
      end if

      if (fc(4,ind) == cc) then
        fc(4,ind) = i
      else
        fc(5,ind) = i
      end if

      if (fc(5,ind) > 0) then
        if (front == 0) then
          front = ind
        else
          fc(7,back) = ind
        end if
        back = ind
      end if

      if (msglvl == 4) then
        write ( *,600) aa,bb,d,i
      end if

    end do

  end do

  if (front /= 0) fc(7,back) = 0

  call availf(hdavfc,nfc,maxfc,fc,ind,ierr)
  call availf(hdavfc,nfc,maxfc,fc,ind2,ierr)
  if (ierr /= 0) return

  if (bface) then
    e = fc(5,ifac)
  else
    e = fc(4,ifac)
  end if

  call htdel(ifac,npt,sizht,fc,ht)
  call htins(ifac,a,b,i,d,e,npt,sizht,fc,ht)
  call htins(ind,a,c,i,d,e,npt,sizht,fc,ht)
  call htins(ind2,b,c,i,d,e,npt,sizht,fc,ht)

  if (bface) then

    e = -e

    if (hdavbf /= 0) then
      bf1 = hdavbf
      hdavbf = -bf(1,hdavbf)
      if (hdavbf /= 0) then
        bf2 = hdavbf
        hdavbf = -bf(1,hdavbf)
      else
        nbf = nbf + 1
        bf2 = nbf
      end if
    else
      nbf = nbf + 2
      bf1 = nbf - 1
      bf2 = nbf
    end if

    if (nbf > maxbf) then
      nbf = maxbf
      ierr = 12
      return
    end if

    fc(5,ind) = -bf1
    fc(5,ind2) = -bf2
    nbrac = bf(2,e)
    nbrbc = bf(1,e)
    bf(1,e) = ind2
    bf(2,e) = ind
    bf(1,bf1) = ind2
    bf(2,bf1) = ifac
    bf(3,bf1) = nbrac
    bf(1,bf2) = ind
    bf(2,bf2) = ifac
    bf(3,bf2) = nbrbc
    j = -fc(5,nbrac)

    if (bf(1,j) == ifac) then
      bf(1,j) = ind
    else if (bf(2,j) == ifac) then
      bf(2,j) = ind
    else
      bf(3,j) = ind
    end if

    j = -fc(5,nbrbc)

    if (bf(1,j) == ifac) then
      bf(1,j) = ind2
    else if (bf(2,j) == ifac) then
      bf(2,j) = ind2
    else
      bf(3,j) = ind2
    end if

  end if

  600 format (1x,'new tetra: ',4i7)

  return
end
subroutine nwthin ( i, ifac, ivrt, npt, sizht, nfc, maxfc, fc, ht, ntetra, &
  hdavfc, front, back, ierr )
!
!******************************************************************************
!
!! NWTHIN creates new tetrahedra after the insertion of an interior vertex.
!
!
!  Purpose: 
!
!    Create new tetrahedra in 3D triangulation from the
!    insertion of vertex I in interior of tetrahedron.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, I - (local) index of next vertex inserted in triangulation.
!
!    Input, IFAC - face of tetrahedron containing vertex I is FC(*,IFAC).
!
!    Input, IVRT - 4 or 5 where 4th vertex of tetrahedron is FC(IVRT,IFAC).
!
!    Input, NPT - number of 3D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input/output, FC(1:7,1:MAXFC) - array of face records; see routine DTRIS3.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NTETRA - number of tetrahedra in triangulation.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Output, FRONT, BACK - indices of front and back of queue of interior
!    faces ABC such that ABCI is a new tetrahedron.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfc
  integer sizht
!
  integer a
  integer aa
  integer b
  integer back
  integer bb
  integer c
  integer cc
  integer d
  integer dd
  integer fc(7,maxfc)
  integer front
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrc
  integer i
  integer ierr
  integer ifac
  integer ind
  integer indx(6)
  integer ivrt
  integer j
  integer, parameter :: msglvl = 0
  integer nfc
  integer npt
  integer ntetra
!
  ierr = 0
  front = 0
  back = 0
  ntetra = ntetra + 3
  a = fc(1,ifac)
  b = fc(2,ifac)
  c = fc(3,ifac)
  d = fc(ivrt,ifac)

  do j = 1,4

    if (j == 1) then

      aa = a
      bb = b
      cc = c
      dd = d
      ind = ifac

    else

      if (j == 2) then
        cc = d
        dd = c
      else if (j == 3) then
        bb = c
        dd = b
      else
        aa = b
        dd = a
      end if

      ind = htsrc(aa,bb,cc,npt,sizht,fc,ht)

      if (ind <= 0) then
        ierr = 300
        return
      end if

    end if

    if (fc(4,ind) == dd) then
      fc(4,ind) = i
    else
      fc(5,ind) = i
    end if

    if (fc(5,ind) > 0) then
      if (front == 0) then
        front = ind
      else
        fc(7,back) = ind
      end if
      back = ind
    end if

    if (msglvl == 4) then
      write ( *,600) aa,bb,cc,i
    end if

  end do

  if (front /= 0) fc(7,back) = 0

  do j = 1,6
    call availf(hdavfc,nfc,maxfc,fc,indx(j),ierr)
    if (ierr /= 0) return
  end do

  call htins(indx(1),a,b,i,c,d,npt,sizht,fc,ht)
  call htins(indx(2),a,c,i,b,d,npt,sizht,fc,ht)
  call htins(indx(3),a,d,i,b,c,npt,sizht,fc,ht)
  call htins(indx(4),b,c,i,a,d,npt,sizht,fc,ht)
  call htins(indx(5),b,d,i,a,c,npt,sizht,fc,ht)
  call htins(indx(6),c,d,i,a,b,npt,sizht,fc,ht)

  600 format (1x,'new tetra: ',4i7)

  return
end
subroutine nwthou ( i, npt, sizht, nbf, nfc, maxbf, maxfc, bf, fc, ht, &
  ntetra, hdavbf, hdavfc, front, back, bfi, ierr )
!
!******************************************************************************
!
!! NWTHOU creates new tetrahedra outside the current convex hull.
!
!
!  Purpose: 
!
!    Create new tetrahedra in 3D triangulation outside
!    convex hull by joining vertex I to visible boundary faces.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, I - (local) index of next vertex inserted in triangulation.
!
!    Input, NPT - number of 3D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NBF - number of positions used in BF array.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input/output, BF(1:3,1:MAXBF) -  array of boundary face records; 
!    see DTRIS3.
!
!    Input/output, FC(1:7,1:MAXFC) - array of face records; see routine DTRIS3.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NTETRA - number of tetrahedra in triangulation.
!
!    Input/output, HDAVBF - head pointer to available BF records.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Input, FRONT - index of front of queue (or top of stack) of visible
!    boundary faces.
!
!    Output, BACK - index of back of queue (or bottom of stack) of visible
!    boundary faces (which become interior faces).
!
!    Output, BFI - index of FC of a boundary face containing vertex I.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxbf
  integer maxfc
  integer sizht
!
  integer a
  integer b
  integer back
  integer bf(3,maxbf)
  integer bfi
  integer bfnew
  integer c
  integer d
  integer e
  integer fc(7,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrc
  integer i
  integer ierr
  integer ind
  integer j
  integer k
  integer l
  integer, parameter :: msglvl = 0
  integer nbf
  integer nbr
  integer nfc
  integer npt
  integer ntetra
  integer ptr
  integer check,counter !>>Miquel26-1-14
!
!  For ABC in queue, form tetrahedron ABCI + add faces ABI, ACI, BCI.
!  PTR, NBR, IND are indices of FC; K, L, BFNEW indices of BF.
!
  ierr = 0
  bfi = 0
  ptr = front
  
  counter=0

10 continue

  back = ptr
  a = fc(1,ptr)
  b = fc(2,ptr)
  c = fc(3,ptr)
  k = -fc(5,ptr)
  fc(5,ptr) = i
  ntetra = ntetra + 1

  if (msglvl == 4) then
    write ( *,600) a,b,c,i
  end if

  do e = 1,3

    if (e == 2) then
      call i_swap ( a, b )
    else if (e == 3) then
      call i_swap ( a, c )
    end if

    nbr = bf(e,k)

    if (fc(7,nbr) /= -1) then
      if (fc(5,nbr) == i) then
        cycle
      end if
    end if

    call availf(hdavfc,nfc,maxfc,fc,ind,ierr)
    if (ierr /= 0) return
    l = -fc(5,nbr)

    if (bf(1,l) == ptr) then
      j = 1
    else if (bf(2,l) == ptr) then
      j = 2
    else
      j = 3
    end if

    if (fc(7,nbr) /= -1) then

      call htins(ind,b,c,i,a,fc(j,nbr),npt,sizht,fc,ht)

    else

      if (hdavbf /= 0) then
        bfnew = hdavbf
        hdavbf = -bf(1,hdavbf)
      else
        if (nbf >= maxbf) then
          ierr = 12
          return
        end if
        nbf = nbf + 1
        bfnew = nbf
      end if

      if (bfi == 0) bfi = ind
      call htins(ind,b,c,i,a,-bfnew,npt,sizht,fc,ht)
      bf(j,l) = ind
      bf(3,bfnew) = nbr

    end if

  end do

  if (k == nbf) then
    nbf = nbf - 1
  else
    bf(1,k) = -hdavbf
    hdavbf = k
  end if

  ptr = fc(7,ptr)
  if (ptr /= 0) go to 10
!
!  Set BF(1:2,BFNEW) fields for new boundary faces.
!
  ptr = bfi
  a = fc(1,ptr)
  j = 2

30 continue

  b = fc(j,ptr)
  c = fc(4,ptr)
  
  counter=counter+1 !>>Miquel3-2-14
!print*,"cut1"
40 continue

  nbr = htsrc(a,c,i,npt,sizht,fc,ht)

  if (nbr <= 0) then
    ierr = 300
    return
  end if
!print*,"cut2",counter

  if (fc(5,nbr) > 0) then
    if (fc(4,nbr) == b) then
      d = fc(5,nbr)
    else
      d = fc(4,nbr)
    end if
    b = c
    c = d
    go to 40
  end if

  k = -fc(5,ptr)
  l = -fc(5,nbr)

  if (fc(1,ptr) == a) then
    bf(2,k) = nbr
  else
    bf(1,k) = nbr
  end if

  if (fc(1,nbr) == a) then
    j = 1
  else
    j = 2
  end if

  bf(3-j,l) = ptr
  a = fc(3-j,nbr)
  ptr = nbr

  if(counter>100)then;print*,"stuck";ierr=666;return;endif
  
  if (ptr /= bfi) go to 30

  600 format (1x,'new tetra: ',4i7)

  return
end
function opside ( a, b, c, d, e )
!
!******************************************************************************
!
!! OPSIDE tests if points are on opposite sides of a triangular face.
!
!
!  Purpose: 
!
!    Test if points D, E are on opposite sides of triangular
!    face with vertices A, B, C.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A(1:3), B(1:3), C(1:3), D(1:3), E(1:3) - five 3D points.
!
!    Output, OPSIDE, the result of the test:
!    +1 if D, E on opposite sides; 
!    -1 if on same side;
!     2 if D is coplanar with face ABC (ABCD is a degenerate tetrahedron); 
!     0 if E is coplanar with face ABC
!
  implicit none
!
  double precision a(3)
  double precision ab(3)
  double precision ac(3)
  double precision b(3)
  double precision c(3)
  double precision d(3)
  double precision ddp
  double precision dmax
  double precision e(3)
  double precision edp
  double precision emax
  integer i
  double precision nrml1
  double precision nrml2
  double precision nrml3
  integer opside
  double precision tol
!
  tol = 100.0D+00 * epsilon ( tol )

  ab(1:3) = b(1:3) - a(1:3)
  ac(1:3) = c(1:3) - a(1:3)

  emax = max(abs(a(1)),abs(a(2)),abs(a(3)),abs(b(1)),abs(b(2)), &
    abs(b(3)),abs(c(1)),abs(c(2)),abs(c(3)))
  dmax = max(emax, abs(d(1)),abs(d(2)),abs(d(3)))
  nrml1 = ab(2)*ac(3) - ab(3)*ac(2)
  nrml2 = ab(3)*ac(1) - ab(1)*ac(3)
  nrml3 = ab(1)*ac(2) - ab(2)*ac(1)
  ddp = (d(1) - a(1))*nrml1 + (d(2) - a(2))*nrml2 + &
    (d(3) - a(3))*nrml3

  if (abs(ddp) <= tol*dmax) then
    opside = 2
    return
  end if

  emax = max ( emax, abs(e(1)),abs(e(2)),abs(e(3)) )
  edp = ( e(1) - a(1) ) * nrml1 + (e(2) - a(2))*nrml2 + &
    (e(3) - a(3))*nrml3

  if ( abs ( edp ) <= tol * emax ) then
    opside = 0
  else if ( ddp * edp < 0.0d0 ) then
    opside = 1
  else
    opside = -1
  end if

  return
end
function opsidk ( k, ind, vcl, eflag, pta, ptb, mat, vec )
!
!******************************************************************************
!
!! OPSIDK tests if points are on opposite sides of a face in KD.
!
!
!  Purpose: 
!
!    Test if points PTA, PTB are on opposite sides of (K-1)-D
!    face formed by K K-D vertices.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of points.
!
!    Input, IND(1:K) - indices in VCL of K-D vertices of face.
!
!    Input, VCL(1:K,1:*) - K-D vertex coordinate list.
!
!    Input, EFLAG - .TRUE. if PTA = PTB: used to determine whether PTA lies
!    in hyperplane containing face, only PTA referenced.
!
!    Input, PTA(1:K), PTB(1:K) - K-D points for which test is applied.
!
!    Workspace, MAT(1:K-1,1:K) - matrix used for solving system of homogeneous
!    linear equations.
!
!    Workspace, VEC(1:K) - vector used for hyperplane normal.
!
!    Output, OPSIDK - 
!    +1 if PTA, PTB on opposite sides; 
!    -1 if on same side;
!     0 if face is degenerate, or PTA or PTB is on same hyperplane as face.
!
  implicit none
!
  integer k
!
  double precision dpa
  double precision dpb
  logical eflag
  integer i
  integer ind(k)
  integer j
  integer km1
  integer l
  integer ll
  integer m
  double precision mat(k-1,k)
  integer opsidk
  double precision pta(k)
  double precision ptb(k)
  integer r
  integer s
  double precision t
  double precision tol
  double precision tolabs
  double precision vcl(k,*)
  double precision vec(k)
!
  tol = 100.0D+00 * epsilon ( tol )
  opsidk = 0
  km1 = k - 1
  m = ind(k)

  t = 0.0d0
  do i = 1, km1
    l = ind(i)
    do j = 1, k
      mat(i,j) = vcl(j,l) - vcl(j,m)
      t = max(t,abs(mat(i,j)))
    end do
  end do

  tolabs = tol * t
!
!  Use Gaussian elimination with partial pivoting to solve K-1 by K
!  homogeneous system. Face is degenerate if 2 zero pivots occur.
!
  r = k
  s = 0

  do l = 1,k-2

30  continue

    ll = l + s
    m = l
    do i = l+1,km1
      if (abs(mat(i,ll)) > abs(mat(m,ll))) m = i
    end do

    t = mat(m,ll)
    mat(m,ll) = mat(l,ll)
    mat(l,ll) = t

    if (abs(t) <= tolabs) then
      if (s == 1) then
        return
      else
        r = l
        s = 1
        go to 30
      end if
    end if

    do i = l+1,km1
      mat(i,ll) = mat(i,ll)/t
    end do

    do j = ll+1,k
      t = mat(m,j)
      mat(m,j) = mat(l,j)
      mat(l,j) = t
      do i = l+1,km1
        mat(i,j) = mat(i,j) - mat(i,ll)*t
      end do
    end do

  end do

  if (abs(mat(km1,km1+s)) <= tolabs) then

    if (s == 1) then
      return
    else
      if (abs(mat(km1,k)) <= tolabs) return
      r = km1
    end if

  end if
!
!  Matrix has full rank. If R <= K-1 then column R has a zero pivot
!  and VEC(R+1:K) = 0. Use VEC(1:R) for opposite test.
!
  vec(r) = -1.0d0
  do l = r-1,1,-1
    t = mat(l,r)/mat(l,l)
    vec(l) = t
    do i = 1,l-1
      mat(i,r) = mat(i,r) - mat(i,l)*t
    end do
  end do

  m = ind(k)
  dpa = 0.0d0

  if (eflag) then

    do i = 1,r
      dpa = dpa + vec(i)*(pta(i) - vcl(i,m))
    end do

    if (abs(dpa) <= tolabs) then
      opsidk = 0
    else
      opsidk = -1
    end if

  else

    dpb = 0.0d0
    do i = 1,r
      dpa = dpa + vec(i)*(pta(i) - vcl(i,m))
      dpb = dpb + vec(i)*(ptb(i) - vcl(i,m))
    end do

    if (abs(dpa) <= tolabs .or. abs(dpb) <= tolabs) then
      opsidk = 0
    else if (dpa*dpb < 0.0d0) then
      opsidk = 1
    else
      opsidk = -1
    end if

  end if

  return
end
subroutine order3 ( i, j, k )
!
!******************************************************************************
!
!! ORDER3 reorders 3 integers into ascending order.
!
!
!  Purpose: 
!
!    Order I, J, K so that I <= J <= K.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input/output, integer I, J, K, on output are sorted into
!    nondecreasing order.
!
  implicit none
!
  integer i
  integer j
  integer k
  integer t
!
  if (j < i) then
    if (k < j) then
      call i_swap ( i, k )
    else if (k < i) then
      t = i
      i = j
      j = k
      k = t
    else
      call i_swap ( i, j )
    end if
  else
    if (k < i) then
      t = i
      i = k
      k = j
      j = t
    else if (k < j) then
      call i_swap ( j, k )
    end if
  end if

  return
end
subroutine orderk ( k, ind )
!
!******************************************************************************
!
!! ORDERK reorders K elements of an array in nondecreasing order.
!
!
!  Purpose: 
!
!    Order K elements of array IND in nondecreasing order.
!    It is assume that K is small, say <= 15, so that insertion sort
!    is used. If K is larger, a faster sort such as heapsort should
!    be used.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - size of array IND.
!
!    Input/output, IND(1:K), an array, which is sorted on output.
!
  implicit none
!
  integer k
!
  integer i
  integer ind(k)
  integer j
  integer s
  integer t
!
  do i = 2, k

    t = ind(i)
    j = i

10  continue

     s = ind(j-1)

     if (t < s) then
       ind(j) = s
       j = j - 1
       if (j > 1) go to 10
     end if

     ind(j) = t

   end do

  return
end
function pi ( )
!
!*******************************************************************************
!
!! PI returns the value of pi.
!
!
!  Modified:
!
!    04 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, double precision PI, the value of pi.
!
  implicit none
!
  double precision pi
!
  pi = 3.14159265358979323846264338327950288419716939937510D+00

  return
end
function prime ( k )
!
!******************************************************************************
!
!! PRIME returns a prime greater than a given value K.
!
!
!  Purpose: 
!
!    Return a prime >= K (if possible) from internal array
!    of primes. More primes can be added if desired.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K, a positive integer.
!
!    Output, PRIME - smallest prime >= K from internal array (or largest
!    in array).
!
  implicit none
!
  integer, parameter :: nprime = 150
!
  integer k
  integer l
  integer m
  integer prime
  integer, parameter, dimension ( nprime ) :: primes = (/ &
    17,31,47,61,79,97,113,127,149,163,179,193,211,227,241, &
    257,271,293,307,331,353,379,401,431,457,479,503,541,563,587, &
    613,641,673,701,727,751,773,797,821,853,877,907,929,953,977, &
    1009,1049,1087,1123,1163,1201,1237,1277,1319,1361,1399,1433, &
    1471,1511,1543,1579,1613,1657,1699,1741,1783,1831,1873,1931, &
    1973,2017,2069,2129,2203,2267,2333,2389,2441,2503,2557,2609, &
    2663,2719,2789,2851,2917,2999,3061,3137,3209,3299,3371,3449, &
    3527,3613,3697,3779,3863,3947,4049,4211,4421,4621,4813,5011, &
    5227,5413,5623,5813,6011,6211,6421,6619,6823,7013,7211,7411, &
    7621,7817,8011,8219,8419,8623,8819,9011,9221,9413,9613,9811, &
    10037,10211,10427,10613,10831,11027,11213,11411,11617,11813, &
    12011,12211,12413,12611,12821,13033,13217,13411,13613,13829, &
    14011/)
  integer u
!
  if (k <= primes(1)) then
    prime = primes(1)
    return
  else if (k >= primes(nprime)) then
    prime = primes(nprime)
    return
  end if
!
!  Use binary search to find prime >= K.
!
  l = 1
  u = nprime

10 continue

  m = (l + u)/2

  if (k < primes(m)) then
    u = m - 1
  else if (k > primes(m)) then
    l = m + 1
  else
    prime = primes(m)
    return
  end if

  if (l <= u) go to 10

  prime = primes(u+1)

  return
end
subroutine prmdf2 ( ipoly, wsq, ivrt, xivrt, edgval, vrtval, nev, ifv, &
  listev )
!
!******************************************************************************
!
!! PRMDF2 does preprocessing for the mesh distribution function evaluation.
!
!
!  Purpose: 
!
!    Preprocessing step for evaluating mesh distribution
!    function in polygon IPOLY - the edges and vertices for
!    which distances must be computed are determined.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, IPOLY - index of polygon.
!
!    Input, WSQ - square of width of polygon IPOLY.
!
!    Input, IVRT(1:*) - indices of polygon vertices in VCL, ordered by
!    polygon.
!
!    Input, XIVRT(1:*) - pointer to first vertex of each polygon in IVRT;
!    vertices of polygon IPOLY are IVRT(I) for I from
!    XIVRT(IPOLY) to XIVRT(IPOLY+1)-1.
!
!    Input, EDGVAL(1:*) - value associated with each edge of decomposition.
!
!    Input, VRTVAL(1:*) - value associated with each vertex of decomposition.
!
!    Output, NEV - number of edges and vertices for which distances must
!    be evaluated.
!
!    Output, IFV - index of first vertex XIVRT(IPOLY) if LISTEV(NEV)
!    = XIVRT(IPOLY+1) - 1; 0 otherwise.
!
!    Output, LISTEV(1:*) - array of length <= [XIVRT(IPOLY+1)-XIVRT(IPOLY)]
!    *2 containing indices of edges and vertices mentioned
!    above; indices of vertices are negated.
!
  implicit none
!
  double precision edgval(*)
  integer i
  integer ifv
  integer im1
  integer ipoly
  integer ivrt(*)
  integer j
  integer l
  integer listev(*)
  integer nev
  double precision vrtval(*)
  double precision wsq
  integer xivrt(*)
!
  ifv = 0
  nev = 0
  im1 = xivrt(ipoly+1) - 1
  l = im1

  do i = xivrt(ipoly),l

    j = ivrt(i)

    if (vrtval(j) < min(edgval(i),edgval(im1))) then
      nev = nev + 1
      listev(nev) = -j
    end if

    if (edgval(i) < wsq) then
      nev = nev + 1
      listev(nev) = i
    end if

    im1 = i

  end do

  if (nev > 0) then
    if (listev(nev) == l) then
      ifv = xivrt(ipoly)
    end if
  end if

  return
end
subroutine prmdf3 ( ipolh, widp, nvc, vcl, nrml, ivrt, xivrt, ifac, xifac, &
  facval, edgval, vrtval, nfcev, nedev, nvrev, listev, infoev, htsiz, &
  maxedg, ht, edge, ierr )
!
!******************************************************************************
!
!! PRMDF3 does preprocessing for the mesh distribution function evaluation.
!
!
!  Purpose: 
!
!    Preprocessing step for evaluating mesh distribution
!    function in polyhedron IPOLH - the faces, edges, vertices for
!    which distances must be computed are determined.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, IPOLH - index of polyhedron.
!
!    Input, WIDP - width of polyhedron IPOLH.
!
!    Input, NVC - number of vertex coordinates in VCL array.
!
!    Input, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input, NRML(1:3,1:*) - unit normal vectors for faces.
!
!    Input, IVRT(1:*) - indices of face vertices in VCL, ordered by face.
!
!    Input, XIVRT(1:*) - pointer to first vertex of each face in IVRT;
!    vertices of face K are IVRT(I) for I from XIVRT(K) to
!    XIVRT(K+1)-1.
!
!    Input, IFAC(1:*) - indices of polyhedron faces in FACEP, ordered by
!    polyhedron.
!
!    Input, XIFAC(1:*) - pointer to first face of each polyhedron in IFAC;
!    faces of polyhedron IPOLH are IFAC(I) for I from
!    XIFAC(IPOLH) to XIFAC(IPOLH+1)-1.
!
!    Input, FACVAL(1:*) - value associated with each face of decomposition.
!
!    Input, EDGVAL(1:*) - value associated with each edge of decomposition.
!
!    Input, VRTVAL(1:*) - value associated with each vertex of decomposition.
!
!    Input, HTSIZ - size of hash table HT; should be a prime number which
!    is >= number of vertices in polyhedron.
!
!    Input, MAXEDG - maximum size available for EDGE array; should be at
!    least number of edges.
!
!    Output, NFCEV - number of faces for which distances must be evaluated.
!
!    Output, NEDEV - number of edges for which distances must be evaluated.
!
!    Output, NVREV - number of vertices for which distances must be evaluated.
!
!    Output, LISTEV(1:NFCEV+NEDEV+NVREV) - indices of above faces, edges,
!    vertices; first are NFCEV indices in FACVAL of faces,
!    then NEDEV indices in EDGVAL of edges, and NVREV indices
!    in VRTVAL of vertices.
!
!    Output, INFOEV(1:4,1:NFCEV+NEDEV) - info for evaluation of distances
!    associated with faces and edges; first NFCEV entries are
!    plane equations with unit normal for above faces:
!    INFOEV(1,I)*X+INFOEV(2,I)*Y+INFOEV(3,I)*Z = INFOEV(4,I);
!    last NEDEV entries are displacement vector (DX,DY,DZ) =
!    INFOEV(1:3,J) of edge from vertex IVRT(LISTEV(J)) and
!    length of edge = INFOEV(4,J).
!
!    [Note: It is assumed there is enough space for above 2 arrays.]
!
!    Workspace, HT(0:HTSIZ-1), EDGE(1:4,1:MAXEDG) - hash table and edge records
!    used to determine entries of LISTEV.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer htsiz
  integer maxedg
!
  integer bptr
  integer e
  integer edge(4,maxedg)
  double precision edgval(*)
  integer f
  double precision facval(*)
  integer g
  integer hdfree
  integer ht(0:htsiz-1)
  integer i
  integer ierr
  integer ifac(*)
  integer ind
  double precision infoev(4,*)
  integer ipolh
  integer ivrt(*)
  integer j
  integer k
  integer l
  integer la
  integer last
  integer lb
  integer listev(*)
  integer nedev
  integer nfcev
  double precision nrml(3,*)
  integer nvc
  integer nvrev
  integer ptr
  double precision vcl(3,*)
  double precision vrtval(*)
  double precision widp
  integer xifac(*)
  integer xivrt(*)
!
  ierr = 0
  k = 0

  do i = xifac(ipolh),xifac(ipolh+1)-1

    f = abs(ifac(i))

    if (facval(f) < widp) then

      k = k + 1
      listev(k) = f
      infoev(1,k) = nrml(1,f)
      infoev(2,k) = nrml(2,f)
      infoev(3,k) = nrml(3,f)
      j = ivrt(xivrt(f))
      infoev(4,k) = nrml(1,f)*vcl(1,j) + nrml(2,f)*vcl(2,j) + &
        nrml(3,f)*vcl(3,j)

    end if

  end do

  nfcev = k

  hdfree = 0
  last = 0
  ht(0:htsiz-1) = 0

  do i = xifac(ipolh),xifac(ipolh+1)-1

    f = abs(ifac(i))
    l = xivrt(f+1) - 1
    e = l
    la = ivrt(l)

    do j = xivrt(f),l

      lb = ivrt(j)

      call edght ( la, lb, f, nvc, htsiz, maxedg, hdfree, last, ht, &
        edge, g, ierr )

      if (ierr /= 0) then
        return
      end if

      if (g > 0) then
        if (edgval(e) < min(facval(f),facval(g))) then
          k = k + 1
          listev(k) = e
          infoev(1,k) = vcl(1,lb) - vcl(1,la)
          infoev(2,k) = vcl(2,lb) - vcl(2,la)
          infoev(3,k) = vcl(3,lb) - vcl(3,la)
          infoev(4,k) = sqrt(infoev(1,k)**2 + infoev(2,k)**2 &
            + infoev(3,k)**2)
        end if
      end if

      la = lb
      e = j

    end do

  end do

  nedev = k - nfcev
!
!  HT, EDGE arrays are used below in a similar way to routine EDGHT
!  except that EDGE(1,*) is not used and no entries are deleted.
!
  last = 0
  ht(0:htsiz-1) = 0

  do i = xifac(ipolh),xifac(ipolh+1)-1

    f = abs(ifac(i))
    l = xivrt(f+1) - 1
    e = l

    do j = xivrt(f),l

      if (edgval(e) < edgval(j)) then
        g = e
      else
        g = j
      end if

      e = j
      lb = ivrt(j)
      ind = mod(lb,htsiz)
      bptr = -1
      ptr = ht(ind)

60    continue

      if (ptr /= 0) then

        if (edge(2,ptr) > lb) then
          go to 70
        else if (edge(2,ptr) < lb) then
          bptr = ptr
          ptr = edge(4,ptr)
          go to 60
        else
          if (edgval(g) < edgval(edge(3,ptr))) edge(3,ptr)=g
          cycle
        end if

      end if

70    continue

      last = last + 1

      if (last > maxedg) then
        ierr = 1
        return
      end if

      if (bptr == -1) then
        ht(ind) = last
      else
        edge(4,bptr) = last
      end if

      edge(2,last) = lb
      edge(3,last) = g
      edge(4,last) = ptr

    end do

  end do

  do i = 1,last
    j = edge(2,i)
    e = edge(3,i)
    if (vrtval(j) < edgval(e) ) then
      k = k + 1
      listev(k) = j
    end if
  end do

  nvrev = k - (nfcev + nedev)

  return
end
subroutine ptpolg ( dim, ldv, nv, inc, pgind, vcl, pt, nrml, dtol, inout )
!
!******************************************************************************
!
!! PTPOLG determines where a point lies with respect to a polygon.
!
!
!  Purpose: 
!
!    Determine whether a point lies inside, outside, or on
!    boundary of a planar polygon in 2 or 3 dimensional space.
!    It is assumed that point lies in plane of polygon.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, DIM - dimension of polygon (2 or 3).
!
!    Input, LDV - leading dimension of VCL array in calling routine.
!
!    Input, NV - number of vertices in polygon.
!
!    Input, INC - increment for PGIND indicating indices of polygon.
!
!    Input, PGIND(0:NV*INC) - indices in VCL of polygon vertices are in
!    PGIND(0), PGIND(INC), ..., PGIND(NV*INC) with first and
!    last vertices identical.
!
!    Input, VCL(1:DIM,1:*) - vertex coordinate list.
!
!    Input, PT(1:DIM) - point for which in/out test is applied.
!
!    Input, NRML(1:3) - unit normal vector of plane containing polygon,
!    with vertices oriented counter clockwise with respect to
!    normal (used iff DIM = 3);
!    normal is assumed to be (0,0,1) if DIM = 2.
!
!    Input, DTOL - absolute tolerance to determine whether a point is on
!    a line or plane.
!
!    Output, INOUT - +1, 0, or -1 depending on whether point PT is inside
!    polygon, on boundary of polygon, or outside polygon;
!    or -2 if error in input parameters.
!
  implicit none
!
  integer dim
  integer ldv
! 
  double precision cp(3)
  double precision da(3)
  double precision db(3)
  double precision dir(3)
  double precision dtol
  integer h
  integer i
  integer inc
  integer inout
  integer j
  integer k
  integer l
  integer la
  integer lb
  integer m
  double precision nr(4)
  double precision nrml(3)
  integer nv
  integer pgind(0:*)
  double precision pt(dim)
  double precision rhs(3)
  integer s
  integer sa
  integer sb
  double precision tol
  double precision vcl(ldv,*)

  double precision area,armax,dist,t,ta
!
  tol = 100.0D+00 * epsilon ( tol )
  inout = -2
  if (dim < 2 .or. dim > 3) return
!
!  Find edge subtending max area with PT as third triangle vertex.
!
  armax = 0.0d0
  h = 0
  lb = pgind(0)
  db(1:dim) = vcl(1:dim,lb) - pt(1:dim)

  do i = 1,nv

    la = lb
    lb = pgind(i*inc)

    do j = 1,dim
      da(j) = db(j)
      db(j) = vcl(j,lb) - pt(j)
      dir(j) = vcl(j,lb) - vcl(j,la)
    end do

    if (dim == 2) then
      area = abs(da(1)*db(2) - db(1)*da(2))
    else
      area = (da(2)*db(3)-db(2)*da(3))**2 + (da(3)*db(1)- &
        db(3)*da(1))**2 + (da(1)*db(2)-db(1)*da(2))**2
    end if

    if (area > armax) then
      h = i
      armax = area
    end if

  end do

  if (dim == 2) armax = armax**2
  if (armax <= dtol**2) return
!
!  Direction of ray is from PT through midpoint of edge subtending
!  max area. NR is unit normal of line or plane containing ray,
!  which is also orthogonal to NRML in 3D case.
!
  la = pgind((h-1)*inc)
  lb = pgind(h*inc)
  dir(1) = 0.5d0*(vcl(1,la) + vcl(1,lb)) - pt(1)
  dir(2) = 0.5d0*(vcl(2,la) + vcl(2,lb)) - pt(2)

  if (dim == 2) then
    dist = sqrt(dir(1)**2 + dir(2)**2)
    dir(1) = dir(1)/dist
    dir(2) = dir(2)/dist
    dir(3) = 0.0d0
    nr(1) = -dir(2)
    nr(2) = dir(1)
    nr(4) = nr(1)*pt(1) + nr(2)*pt(2)
    dist = nr(1)*vcl(1,lb) + nr(2)*vcl(2,lb) - nr(4)
  else
    dir(3) = 0.5d0*(vcl(3,la) + vcl(3,lb)) - pt(3)
    dist = sqrt(dir(1)**2 + dir(2)**2 + dir(3)**2)
    dir(1) = dir(1)/dist
    dir(2) = dir(2)/dist
    dir(3) = dir(3)/dist
    nr(1) = nrml(2)*dir(3) - nrml(3)*dir(2)
    nr(2) = nrml(3)*dir(1) - nrml(1)*dir(3)
    nr(3) = nrml(1)*dir(2) - nrml(2)*dir(1)
    nr(4) = nr(1)*pt(1) + nr(2)*pt(2) + nr(3)*pt(3)
    dist = nr(1)*vcl(1,lb)+nr(2)*vcl(2,lb)+nr(3)*vcl(3,lb) - nr(4)
  end if

  if (dist > 0.0d0) then
    sb = 1
  else
    sb = -1
  end if

  m = 1
  if (abs(dir(2)) > abs(dir(1))) m = 2
  if (abs(dir(3)) > abs(dir(m))) m = 3
  k = 1
!
!  For remaining edges of polygon, check whether ray intersects edge.
!  Vertices or edges lying on ray are handled by looking at preceding
!  and succeeding edges not lying on ray.
!
  k = 1
  i = h + 1
  if (i > nv) i = 1

40 continue

  la = lb
  lb = pgind(i*inc)
  sa = sb

  if (dim == 2) then
    dist = nr(1)*vcl(1,lb) + nr(2)*vcl(2,lb) - nr(4)
  else
    dist = nr(1)*vcl(1,lb) + nr(2)*vcl(2,lb) + nr(3)*vcl(3,lb) - nr(4)
  end if

  if (abs(dist) <= dtol) then
    sb = 0
  else if (dist > 0.0d0) then
    sb = 1
  else
    sb = -1
  end if

  s = sa*sb

  if (s < 0) then

    if (dim == 2) then

      da(1) = vcl(1,la) - vcl(1,lb)
      da(2) = vcl(2,la) - vcl(2,lb)
      rhs(1) = vcl(1,la) - pt(1)
      rhs(2) = vcl(2,la) - pt(2)
      t = (rhs(1)*da(2) - rhs(2)*da(1))/(dir(1)*da(2) - dir(2)*da(1))

    else

      da(1) = vcl(1,la) - vcl(1,lb)
      da(2) = vcl(2,la) - vcl(2,lb)
      da(3) = vcl(3,la) - vcl(3,lb)
      rhs(1) = vcl(1,la) - pt(1)
      rhs(2) = vcl(2,la) - pt(2)
      rhs(3) = vcl(3,la) - pt(3)
      cp(1) = dir(2)*da(3) - dir(3)*da(2)
      cp(2) = dir(3)*da(1) - dir(1)*da(3)
      cp(3) = dir(1)*da(2) - dir(2)*da(1)
      l = 1
      if (abs(cp(2)) > abs(cp(1))) l = 2
      if (abs(cp(3)) > abs(cp(l))) l = 3

      if (l == 1) then
        t = (rhs(2)*da(3) - rhs(3)*da(2))/cp(1)
      else if (l == 2) then
        t = (rhs(3)*da(1) - rhs(1)*da(3))/cp(2)
      else
        t = (rhs(1)*da(2) - rhs(2)*da(1))/cp(3)
      end if

    end if

    if (t > dtol) then
      k = k + 1
    else if (t >= -dtol) then
      inout = 0
      return
    end if

  else if (s == 0) then

    l = lb

50  continue

    i = i + 1
    if (i > nv) i = 1
    if (i == h) return
    la = lb
    lb = pgind(i*inc)

    if (dim == 2) then
      dist = nr(1)*vcl(1,lb) + nr(2)*vcl(2,lb) - nr(4)
    else
      dist = nr(1)*vcl(1,lb) + nr(2)*vcl(2,lb) + nr(3)*vcl(3,lb) - nr(4)
    end if

    if (abs(dist) <= dtol) then
      go to 50
    else if (dist > 0.0d0) then
      sb = 1
    else
      sb = -1
    end if

    t = (vcl(m,l) - pt(m))/dir(m)

    if (abs(t) <= dtol) then
      inout = 0
      return
    end if

    if (la /= l) then
      ta = (vcl(m,la) - pt(m))/dir(m)
      if (abs(ta) <= dtol .or. t*ta < 0.0d0) then
        inout = 0
        return
      end if
    end if

    if (sa*sb < 0 .and. t > 0.0d0) k = k + 1

  end if

  i = i + 1
  if (i > nv) i = 1

  if (i /= h) go to 40
!
!  Point lies inside polygon if number of intersections K is odd.
!
  if (mod(k,2) == 1) then
    inout = 1
  else
    inout = -1
  end if

  return
end
subroutine r_random ( rlo, rhi, r )
!
!*******************************************************************************
!
!! R_RANDOM returns a random real in a given range.
!
!
!  Modified:
!
!    06 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real RLO, RHI, the minimum and maximum values.
!
!    Output, real R, the randomly chosen value.
!
  real r
  real rhi
  real rlo
  real t
!
!  Pick T, a random number in (0,1).
!
  call random_number ( harvest = t )
!
!  Set R in ( RLO, RHI ).
!
  r = ( 1.0E+00 - t ) * rlo + t * rhi

  return
end
function radrth ( a, b, c, d )
!
!******************************************************************************
!
!! RADRTH computes the aspect ratio of a tetrahedron.
!
!
!  Purpose: 
!
!    Compute radius (aspect) ratio of tetrahedron
!    = 3 * inradius / circumradius
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A(1:3), B(1:3), C(1:3), D(1:3) - 4 vertices of tetrahedron
!
!    Output, RADRTH - radius ratio of tetrahedron
!
  implicit none
!
  double precision a(3)
  double precision ab(3)
  double precision ac(3)
  double precision ad(3)
  double precision b(3)
  double precision bc(3)
  double precision bd(3)
  double precision c(3)
  double precision cd(3)
  double precision cp1
  double precision cp2
  double precision cp3
  double precision d(3)
  double precision denom
  double precision fa
  double precision fb
  double precision fc
  double precision fd
  integer i
  double precision lab
  double precision lac
  double precision lad
  double precision lbc
  double precision lbd
  double precision lcd
  double precision pb
  double precision pc
  double precision pd
  double precision radrth
  double precision t1
  double precision t2
  double precision vol
!
  ab(1:3) = b(1:3) - a(1:3)
  ac(1:3) = c(1:3) - a(1:3)
  ad(1:3) = d(1:3) - a(1:3)
  bc(1:3) = c(1:3) - b(1:3)
  bd(1:3) = d(1:3) - b(1:3)
  cd(1:3) = d(1:3) - c(1:3)

  lab = ab(1)**2 + ab(2)**2 + ab(3)**2
  lac = ac(1)**2 + ac(2)**2 + ac(3)**2
  lad = ad(1)**2 + ad(2)**2 + ad(3)**2
  lbc = bc(1)**2 + bc(2)**2 + bc(3)**2
  lbd = bd(1)**2 + bd(2)**2 + bd(3)**2
  lcd = cd(1)**2 + cd(2)**2 + cd(3)**2
  pb = sqrt(lab*lcd)
  pc = sqrt(lac*lbd)
  pd = sqrt(lad*lbc)
  cp1 = ab(2)*ac(3) - ab(3)*ac(2)
  cp2 = ab(3)*ac(1) - ab(1)*ac(3)
  cp3 = ab(1)*ac(2) - ab(2)*ac(1)
  fd = sqrt(cp1**2 + cp2**2 + cp3**2)
  cp1 = ab(2)*ad(3) - ab(3)*ad(2)
  cp2 = ab(3)*ad(1) - ab(1)*ad(3)
  cp3 = ab(1)*ad(2) - ab(2)*ad(1)
  fc = sqrt(cp1**2 + cp2**2 + cp3**2)
  cp1 = bc(2)*bd(3) - bc(3)*bd(2)
  cp2 = bc(3)*bd(1) - bc(1)*bd(3)
  cp3 = bc(1)*bd(2) - bc(2)*bd(1)
  fa = sqrt(cp1**2 + cp2**2 + cp3**2)
  cp1 = ac(2)*ad(3) - ac(3)*ad(2)
  cp2 = ac(3)*ad(1) - ac(1)*ad(3)
  cp3 = ac(1)*ad(2) - ac(2)*ad(1)
  fb = sqrt(cp1**2 + cp2**2 + cp3**2)
  t1 = pb + pc
  t2 = pb - pc
  denom = (fa+fb+fc+fd)*sqrt(abs((t1+pd)*(t1-pd)*(pd+t2)*(pd-t2)))

  if (denom == 0.0d0) then
    radrth = 0.0d0
  else
    vol = ab(1)*cp1 + ab(2)*cp2 + ab(3)*cp3
    radrth = 12.0d0*vol**2/denom
  end if

  return
end
subroutine randpt ( k, n, seed, axis, nptav, scale, trans, lda, a )
!
!******************************************************************************
!
!! RANDPT generates N random points in KD.
!
!
!  Purpose: 
!
!    Generate N random K-dimensional points from the uniform
!    distribution.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of points.
!
!    Input, N - number of random points.
!
!    Input/output, SEED - seed for pseudo random number generator.
!
!    Input, AXIS, NPTAV - if AXIS < 1 or > K, then uniform random points are
!    generated; if 1 <= AXIS <= K then an average of NPTAV
!    uniform random points are generated with the same AXIS
!    coordinate on about N/NPTAV random parallel hyperplanes.
!
!    Input, SCALE(1:K), TRANS(1:K) - scale and translation factors for
!    coordinates 1 to K; Ith coordinate of random point is
!    R*SCALE(I) + TRANS(I) where 0 < R < 1.
!
!    Input, LDA - leading dimension of array A in calling routine; should
!    be >= K.
!
!    Output, A(1:K,1:N) - array of N uniform random K-D points.
!
  implicit none
!
  integer k
  integer lda
  integer n
!
  double precision a(lda,n)
  integer axis
  integer i
  integer j
  integer m
  integer nptav
  double precision r
  double precision scale(k)
  integer seed
  double precision trans(k)
  real urand
!
  if (axis < 1 .or. axis > k) then

    do j = 1,n
      do i = 1,k
        a(i,j) = urand(seed) * scale(i) + trans(i)
      end do
    end do

  else

    m = int(urand(seed)*2.0*nptav + 0.5)
    r = urand(seed)*scale(axis) + trans(axis)

    do j = 1,n
      do i = 1,k
        if (i == axis) then
          a(i,j) = r
        else
          a(i,j) = urand(seed)*scale(i) + trans(i)
        end if
      end do

      m = m - 1

      if (m <= 0) then
        m = int(urand(seed)*2.0*nptav + 0.5)
        r = urand(seed)*scale(axis) + trans(axis)
      end if

    end do

  end if

  return
end
subroutine resedg ( u, angacc, rdacc, nvc, nface, nvert, npolh, npf, maxvc, &
  maxfp, maxfv, maxhf, maxpf, maxiw, maxwk, vcl, facep, factyp, nrml, fvl, &
  eang, hfl, pfl, rflag, iwk, wk, ierr )
!
!******************************************************************************
!
!! RESEDG resolves a reflex edge by a cut polygon.
!
!
!  Purpose: 
!
!    Attempt to resolve reflex edge by a cut polygon which
!    does not create small dihedral angles and does not pass near
!    any vertices not on cut plane.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, U - index in FVL of reflex edge.
!
!    Input, ANGACC - minimum acceptable dihedral angle in radians produced by
!    a cut face.
!
!    Input, RDACC - minimum acceptable relative distance between a cut plane
!    and vertices not on plane.
!
!    Input/output, NVC - number of vertex coordinates or positions used in VCL.
!
!    Input/output, NFACE - number of faces or positions used in FACEP array.
!
!    Input/output, NVERT - number of positions used in FVL, EANG arrays.
!
!    Input/output, NPOLH - number of polyhedra or positions used in HFL array.
!
!    Input/output, NPF - number of positions used in PFL array.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXFP - maximum size available for FACEP, FACTYP, NRML arrays.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, MAXHF - maximum size available for HFL array.
!
!    Input, MAXPF - maximum size available for PFL array.
!
!    Input, MAXIW - maximum size available for IWK array; should be about
!    5/3*NEDG where NEDG is number of edges in polyhedron
!    containing reflex edge.
!
!    Input, MAXWK - maximum size available for WK array; should be >= NEDG
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input/output, FACEP(1:3,1:NFACE) - face pointer list.
!
!    Input/output, FACTYP(1:NFACE) - face types.
!
!    Input/output, NRML(1:3,1:NFACE) - unit normal vectors for faces.
!
!    Input/output, FVL(1:6,1:NVERT) - face vertex list.
!
!    Input/output, EANG(1:NVERT) - edge angles.
!
!    Input/output, HFL(1:NPOLH) - head pointer to face indices in PFL for 
!    each polyhedron.
!
!    Input/output, PFL(1:2,1:NPF) - list of signed face indices for 
!    each polyhedron.
!
!    Output, RFLAG - .TRUE. iff reflex edge is resolved.
!
!    Workspace, IWK(1:3,1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfp
  integer maxfv
  integer maxhf
  integer maxiw
  integer maxpf
  integer maxvc
  integer maxwk
  integer, parameter :: nangmx = 5
!
  integer a
  double precision ang(nangmx)
  double precision angacc
  double precision anghi
  double precision anglo
  integer b
  integer c
  integer ca
  integer cb
  integer ccw
  double precision ce
  double precision cn
  double precision da
  double precision db
  double precision dotp
  double precision dtol
  double precision e(3)
  double precision eang(maxfv)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  double precision enc(3)
  double precision enl(3)
  double precision enr(3)
  double precision ep(3)
  integer f
  integer facep(3,maxfp)
  integer, parameter :: facn = 2
  integer factyp(maxfp)
  logical first
  integer fl
  integer fr
  integer fvl(6,maxfv)
  integer hfl(maxhf)
  integer i
  integer ierr
  integer iwk(3,maxiw)
  integer j
  integer k
  integer la
  integer lb
  integer lc
  double precision leng
  integer, parameter :: loc = 1
  integer lu
  integer luo
  integer lv
  integer lvo
  double precision mindis
  integer, parameter :: msglvl = 0
  integer nang
  integer nce
  integer nedg
  integer nedgc
  integer nface
  integer npf
  integer npolh
  double precision nrml(3,maxfp)
  double precision nrmlc(4)
  integer nvc
  integer nvert
  integer p
  integer pfl(2,maxpf)
  double precision pi
  integer, parameter :: pred = 4
  integer ptr
  double precision rdacc
  logical rflag
  integer sp
  integer, parameter :: succ = 3
  double precision sum2
  double precision t
  double precision tol
  integer u
  integer v
  double precision vcl(3,maxvc)
  double precision wk(maxwk)
!
!  Find faces FL, FR and polyhedron P containing reflex edge UV.
!  Do not resolve UV if FL or FR is a double-occurring face.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  rflag = .false.
  v = fvl(succ,u)
  lu = fvl(loc,u)
  lv = fvl(loc,v)
  fl = fvl(facn,u)
  fr = fvl(facn,fvl(edgc,u))
  if (abs(facep(2,fl)) == abs(facep(3,fl))) return
  if (abs(facep(2,fr)) == abs(facep(3,fr))) return

  if (lu < lv) then

    if (facep(2,fl) > 0) then
      p = facep(2,fl)
    else
      p = facep(3,fl)
    end if

    luo = lu
    lvo = lv

  else

    if (facep(2,fl) < 0) then
      p = -facep(2,fl)
    else
      p = -facep(3,fl)
    end if

    luo = lv
    lvo = lu

  end if

  if (msglvl == 4) then
    write ( *,600) u,v,lu,lv,fl,fr,p
  end if
!
!  Determine edges in polyhedron P and average edge length.
!
  nedg = 0
  sum2 = 0.0d0
  ptr = hfl(p)

10 continue

  f = pfl(1,ptr)

  if (f > 0) then
    ccw = succ
  else
    ccw = pred
    f = -f
  end if

  a = facep(1,f)
  lb = fvl(loc,a)

20 continue

  la = lb
  b = fvl(ccw,a)
  lb = fvl(loc,b)

  if (la < lb) then

    leng = sqrt((vcl(1,la) - vcl(1,lb))**2 + (vcl(2,la) - &
      vcl(2,lb))**2 + (vcl(3,la) - vcl(3,lb))**2)
    sum2 = sum2 + leng

    if (la /= luo .or. lb /= lvo) then

      nedg = nedg + 1

      if (nedg > maxiw) then
        ierr = 6
        return
      end if

      if (ccw == succ) then
        iwk(1,nedg) = a
        iwk(2,nedg) = fvl(edgc,a)
      else
        iwk(1,nedg) = b
        iwk(2,nedg) = fvl(edgc,b)
      end if

    end if

  end if

  a = b
  if (a /= facep(1,f)) go to 20

  ptr = pfl(2,ptr)
  if (ptr /= hfl(p)) go to 10

  sum2 = sum2 / dble(nedg+1)
  mindis = rdacc*sum2
  dtol = tol * sum2

  if (5*nedg > 3*maxiw) then
    ierr = 6
    return
  else if (nedg > maxwk) then
    ierr = 7
    return
  end if
!
!  Compute unit vectors E, ENL, ENR which are directed edge UV and
!  edge normals to UV on faces FL and FR. E is unit normal vector
!  of plane containing ENL, ENR (it is normalization of ENL x ENR).
!
  e(1) = vcl(1,lvo) - vcl(1,luo)
  e(2) = vcl(2,lvo) - vcl(2,luo)
  e(3) = vcl(3,lvo) - vcl(3,luo)
  leng = sqrt(e(1)**2 + e(2)**2 + e(3)**2)
  e(1:3) = e(1:3)/leng
  enl(1) = nrml(2,fl)*e(3) - nrml(3,fl)*e(2)
  enl(2) = nrml(3,fl)*e(1) - nrml(1,fl)*e(3)
  enl(3) = nrml(1,fl)*e(2) - nrml(2,fl)*e(1)
  enr(1) = e(2)*nrml(3,fr) - e(3)*nrml(2,fr)
  enr(2) = e(3)*nrml(1,fr) - e(1)*nrml(3,fr)
  enr(3) = e(1)*nrml(2,fr) - e(2)*nrml(1,fr)

  if (abs(facep(2,fl)) /= p) then
    enl(1:3) = -enl(1:3)
  end if

  if (abs(facep(2,fr)) /= p) then
    enr(1:3) = -enr(1:3)
  end if

  k = 1
  if (abs(e(2)) > abs(e(1))) k = 2
  if (abs(e(3)) > abs(e(k))) k = 3
!
!  Find range of angles [ANGLO, ANGHI] that will resolve reflex edge,
!  and an ordered list of at most NANGMX angles determined by planes
!  containing reflex edge plus an adjacent edge (the bisection and
!  other angles may also be added to list). The list is obtained
!  by cycling through edges incident on U and V. counter clockwise = 1 (-1) if
!  going counter clockwise (CW) around LA (when viewed from 
!  outside polyhedron P).
!
  ang(1) = eang(u) - pi()
  anglo = max(angacc, ang(1))
  anghi = min(pi(), eang(u) - angacc)

  if (anglo > ang(1)) then
    nang = 0
  else
    nang = 2
    ang(2) = pi()
  end if

  do i = 1,2

    ccw = 1

    if (i == 1) then
      la = lu
      b = fvl(pred,u)
      if (lu > lv) ccw = -1
    else
      la = lv
      b = v
      if (lu < lv) ccw = -1
    end if

    first = .true.
    f = fl

30  continue

    if (.not. first) then
      if (fvl(loc,b) == la) then
        b = fvl(pred,b)
      else
        b = fvl(succ,b)
      end if
    end if

    c = fvl(succ,b)

    if (abs(facep(2,f)) == abs(facep(3,f))) then

      if (fvl(loc,b) == la) then
        sp = -ccw*p
      else
        sp = ccw*p
       end if

    else if (abs(facep(2,f)) == p) then

      sp = facep(2,f)

    else

      sp = facep(3,f)

    end if

    if ((fvl(loc,c) - fvl(loc,b))*sp > 0) then
      b = fvl(edgc,b)
    else
      b = fvl(edga,b)
    end if

    f = fvl(facn,b)

    if (f == fr) then
      cycle
    end if

    if (first) then

      first = .false.

    else

      if (fvl(loc,b) == la) then
        c = fvl(succ,b)
      else
        c = b
      end if

      lc = fvl(loc,c)
      ep(1) = vcl(1,lc) - vcl(1,la)
      ep(2) = vcl(2,lc) - vcl(2,la)
      ep(3) = vcl(3,lc) - vcl(3,la)
      nrmlc(1) = ep(2)*e(3) - ep(3)*e(2)
      nrmlc(2) = ep(3)*e(1) - ep(1)*e(3)
      nrmlc(3) = ep(1)*e(2) - ep(2)*e(1)
      leng = sqrt(nrmlc(1)**2 + nrmlc(2)**2 + nrmlc(3)**2)

      if (leng <= tol*max(abs(ep(1)), abs(ep(2)),abs(ep(3)))) go to 30

      nrmlc(1) = nrmlc(1)/leng
      nrmlc(2) = nrmlc(2)/leng
      nrmlc(3) = nrmlc(3)/leng
      enc(1) = e(2)*nrmlc(3) - e(3)*nrmlc(2)
      enc(2) = e(3)*nrmlc(1) - e(1)*nrmlc(3)
      enc(3) = e(1)*nrmlc(2) - e(2)*nrmlc(1)
      nrmlc(1) = enr(2)*enc(3) - enr(3)*enc(2)
      nrmlc(2) = enr(3)*enc(1) - enr(1)*enc(3)
      nrmlc(3) = enr(1)*enc(2) - enr(2)*enc(1)
      leng = sqrt(nrmlc(1)**2 + nrmlc(2)**2 + nrmlc(3)**2)

      if (leng <= tol) go to 30

      t = nrmlc(k)
      nrmlc(1) = enc(2)*enl(3) - enc(3)*enl(2)
      nrmlc(2) = enc(3)*enl(1) - enc(1)*enl(3)
      nrmlc(3) = enc(1)*enl(2) - enc(2)*enl(1)
      leng = sqrt(nrmlc(1)**2 + nrmlc(2)**2 + nrmlc(3)**2)

      if (leng <= tol) go to 30
      if (nrmlc(k)*t < 0.0d0) go to 30

      dotp = enr(1)*enc(1) + enr(2)*enc(2) + enr(3)*enc(3)
      if (e(k)*t < 0.0d0) dotp = -dotp
      if (abs(dotp) > 1.0d0 - tol) dotp = sign(1.0d0,dotp)
      t = acos(dotp)

      if (t < anglo .or. t > anghi) go to 30

      do j = 1,nang
        if (abs(ang(j) - t) <= tol) go to 30
      end do

      nang = nang + 1
      ang(nang) = t
      if (nang >= nangmx) go to 80

    end if

    go to 30
  
  end do

  t = eang(u)*0.5d0

  do j = 1,nang
    if (abs(ang(j) - t) <= tol) go to 70
  end do

  nang = nang + 1
  ang(nang) = t

70 continue

  if (nang <= nangmx - 2 .and. eang(u) <= 1.33d0 * pi() ) then

    nang = nang + 2
    ang(nang-1) = eang(u)*0.25d0
    ang(nang) = eang(u)*0.75d0

    if ( pi() - ang(nang) <= 0.1d0) then
      ang(nang-1) = eang(u)*0.375d0
      ang(nang) = eang(u)*0.625d0
    end if

    if (ang(nang-1) < anglo .or. ang(nang-1) > anghi) then
      ang(nang-1) = ang(nang)
      nang = nang - 1
    end if

    if (ang(nang) < anglo .or. ang(nang) > anghi) then
      nang = nang - 1
    end if

  else if (nang < nangmx .and. eang(u) <= 1.4d0 * pi() ) then

    t = eang(u)*0.375d0

    if (t >= anglo .and. t <= anghi) then
      nang = nang + 1
      ang(nang) = t
    end if

  end if

80 continue

  if (msglvl == 4) then
    write ( *,610) eang(u)*180.0d0/ pi(), (ang(i)*180.0d0 / pi(),i=1,nang)
  end if
!
!  For each angle in ANG array, try to resolve reflex edge as
!  follows. Compute unit normal NRMLC and equation of cut plane; this
!  normal is outward with respect to subpolyhedron containing face FL; NRMLC(4)
!  is right hand side constant term of plane equation.
!  Then determine which edges of polyhedron intersect cut plane;
!  reject plane if there is a vertex within distance MINDIS of plane
!  which does not lie on plane.
!
  do i = 1,nang

    cn = cos(ang(i))
    if (abs(facep(2,fr)) /= p) cn = -cn
    ce = sin(ang(i))
    nrmlc(1) = cn*nrml(1,fr) + ce*enr(1)
    nrmlc(2) = cn*nrml(2,fr) + ce*enr(2)
    nrmlc(3) = cn*nrml(3,fr) + ce*enr(3)
    nrmlc(4) = nrmlc(1)*vcl(1,lu) + nrmlc(2)*vcl(2,lu) + &
      nrmlc(3)*vcl(3,lu)
    nedgc = nedg
    j = 1

90  continue

    la = fvl(loc,iwk(1,j))
    da = nrmlc(1)*vcl(1,la) + nrmlc(2)*vcl(2,la) + &
      nrmlc(3)*vcl(3,la) - nrmlc(4)

    if (abs(da) <= dtol) then
      ca = 0
    else if (abs(da) <= mindis) then
      cycle
    else if (da < 0.0d0) then
      ca = -1
    else
      ca = 1
    end if

    b = fvl(succ,iwk(1,j))
    lb = fvl(loc,b)
    db = nrmlc(1)*vcl(1,lb) + nrmlc(2)*vcl(2,lb) + &
      nrmlc(3)*vcl(3,lb) - nrmlc(4)

    if (abs(db) <= dtol) then
      cb = 0
    else if (abs(db) <= mindis) then
      cycle
    else if (db < 0.0d0) then
      cb = -1
    else
      cb = 1
    end if

    if (ca*cb > 0) then
      do k = 1,2
        c = iwk(k,j)
        iwk(k,j) = iwk(k,nedgc)
        iwk(k,nedgc) = c
      end do
      nedgc = nedgc - 1
    else
      iwk(3,j) = (ca+2)*10 + (cb+2)
      j = j + 1
    end if

    if (j <= nedgc) go to 90

    iwk(1,nedg+1) = lvo
    iwk(3,nedg+1) = luo
    iwk(1,nedg+2) = u
    wk(1) = ang(i)

    if (msglvl == 4) then
      write ( *,620) i,nedgc
    end if

    call cutfac(p,nrmlc,angacc,dtol,nvc,maxvc,vcl,facep,nrml, &
      fvl,eang,nedgc,iwk,nce,iwk(1,nedg+1),wk,rflag,ierr)

    if (ierr /= 0) return

    if (rflag) then
      call insfac(p,nrmlc,nce,iwk(1,nedg+1),wk,nvc,nface,nvert, &
        npolh,npf,maxfp,maxfv,maxhf,maxpf,vcl,facep,factyp,nrml, &
        fvl,eang,hfl,pfl,ierr)
      return
    end if

  end do

  600 format (' resedg: u,v,lu,lv,fl,fr,p =',7i5)
  610 format (4x,'candidate angles (dihedral angle = ',f12.5,')'/ &
     2x,f14.5,4f12.5)
  620 format (4x,'trying angle',i3,'   nedgc=',i5)

  return
end
subroutine reshol ( p, nrmlc, pt, dir, angacc, rdacc, nvc, nface, nvert, &
  npolh, npf, maxvc, maxfp, maxfv, maxhf, maxpf, maxiw, maxwk, vcl, facep, &
  factyp, nrml, fvl, eang, hfl, pfl, iwk, wk, rflag, ierr )
!
!******************************************************************************
!
!! RESHOL finds a cut face in a polyhedron to resolve an interior hole.
!
!
!  Purpose: 
!
!    Find cut face in polyhedron P to resolve interior polyhedral
!    hole where cut plane contains extreme face of hole.
!    Accept cut face if it creates no small angles, it is an outer
!    boundary, there are no holes (inner polygons) in it, etc.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, P - polyhedron index.
!
!    Input, NRMLC(1:4) - unit normal vector of cut plane plus right hand
!    side constant term of plane equation.
!
!    Input, PT(1:3) - point (of hole) on cut plane.
!
!    Input, DIR(1:3) - unit direction of ray on cut plane from PT for which
!    first intersection with boundary is used to start cut face.
!
!    Input, ANGACC - minimum acceptable dihedral angle in radians produced by
!    a cut face.
!
!    Input, RDACC - minimum acceptable relative distance between a cut
!    plane and vertices not on plane.
!
!    Input/output, NVC - number of vertex coordinates.
!
!    Input/output, NFACE - number of faces or positions used in FACEP array.
!
!    Input/output, NVERT - number of positions used in FVL, EANG arrays.
!
!    Input/output, NPOLH - number of polyhedra or positions used in HFL array.
!
!    Input/output, NPF - number of positions used in PFL array.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXFP - maximum size available for FACEP, FACTYP, NRML arrays.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, MAXHF - maximum size available for HFL array.
!
!    Input, MAXPF - maximum size available for PFL array.
!
!    Input, MAXIW - maximum size available for IWK array; should be about
!    max(5*NE, 3*NE+NV) where NE is number of edges of polyhedron
!    intersecting cut plane and NV is maximum number of face vertices.
!
!    Input, MAXWK - maximum size available for WK array; should be about NE.
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input/output, FACEP(1:3,1:NFACE) - face pointer list.
!
!    Input/output, FACTYP(1:NFACE) - face types.
!
!    Input/output, NRML(1:3,1:NFACE) - unit normal vectors for faces.
!
!    Input/output, FVL(1:6,1:NVERT) - face vertex list.
!
!    Input/output, EANG(1:NVERT) - edge angles.
!
!    Input/output, HFL(1:NPOLH) - head pointer to face indices in PFL for 
!    each polyhedron.
!
!    Input/output, PFL(1:2,1:NPF) - list of signed face indices for 
!    each polyhedron.
!
!    Output, RFLAG - .TRUE. iff satisfactory cut polygon is found.
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfp
  integer maxfv
  integer maxhf
  integer maxiw
  integer maxpf
  integer maxvc
  integer maxwk
!
  integer a
  double precision ang
  double precision angacc
  double precision angn
  integer b
  integer ca
  integer cb
  integer ccw
  double precision cmax
  double precision cmin
  double precision cp(3)
  double precision da
  double precision db
  double precision dir(3)
  double precision dotp
  double precision dtol
  double precision eang(maxfv)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer f
  integer facep(3,maxfp)
  integer, parameter :: facn = 2
  integer factyp(maxfp)
  integer fmin
  integer fvl(6,maxfv)
  integer hfl(maxhf)
  integer i
  integer ibeg
  integer iend
  integer ierr
  integer ineg
  integer inout
  integer iomin
  integer ipos
  double precision ipt(3)
  integer iwk(maxiw)
  integer j
  integer k
  integer l(0:1)
  integer la
  integer lb
  double precision leng
  integer link
  integer, parameter :: loc = 1
  double precision mindis
  integer n
  integer nce
  integer ne
  integer nedg
  integer nep1
  integer nface
  logical nflag
  integer npf
  integer npolh
  double precision nrml(3,maxfp)
  double precision nrmlc(4)
  integer nv
  integer nvc
  integer nvert
  integer p
  integer pfl(2,maxpf)
  double precision pi
  integer, parameter :: pred = 4
  double precision pt(3)
  integer ptr
  double precision ray(3)
  double precision rdacc
  logical rflag
  integer sf
  double precision smin
  integer, parameter :: succ = 3
  double precision sum2
  double precision t
  double precision tmin
  double precision tol
  double precision va(3)
  double precision vb(3)
  integer vbeg
  double precision vcl(3,maxvc)
  integer vend
  double precision wk(maxwk)
!
!  Determine edges in polyhedron P and average edge length.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  rflag = .false.
  nedg = 0
  sum2 = 0.0d0
  ptr = hfl(p)

10 continue

  f = pfl(1,ptr)

  if (f > 0) then
    ccw = succ
  else
    ccw = pred
    f = -f
  end if

  a = facep(1,f)
  lb = fvl(loc,a)

20 continue

  la = lb
  b = fvl(ccw,a)
  lb = fvl(loc,b)

  if (la < lb) then
    leng = sqrt((vcl(1,la) - vcl(1,lb))**2 + (vcl(2,la) - &
      vcl(2,lb))**2 + (vcl(3,la) - vcl(3,lb))**2)
    sum2 = sum2 + leng
    nedg = nedg + 1
  end if

  a = b
  if (a /= facep(1,f)) go to 20

  ptr = pfl(2,ptr)
  if (ptr /= hfl(p)) go to 10

  sum2 = sum2 / dble(nedg)
  mindis = rdacc * sum2
  dtol = tol * sum2
!
!  Determine which edges of polyhedron intersect cut plane;
!  reject plane if there is a vertex within distance MINDIS of
!  plane which does not lie on plane.
!
  nedg = 0
  ne = 0
  nv = 0
  ptr = hfl(p)

30 continue

  f = pfl(1,ptr)

  if (f > 0) then
    ccw = succ
  else
    ccw = pred
    f = -f
  end if

  a = facep(1,f)
  lb = fvl(loc,a)
  k = 0

40 continue

  la = lb
  b = fvl(ccw,a)
  lb = fvl(loc,b)
  k = k + 1

  if (la < lb) then

    da = nrmlc(1)*vcl(1,la) + nrmlc(2)*vcl(2,la) + &
      nrmlc(3)*vcl(3,la) - nrmlc(4)

    if (abs(da) <= dtol) then
      ca = 0
    else if (abs(da) <= mindis) then
      return
    else if (da < 0.0d0) then
      ca = -1
    else
      ca = 1
    end if

    db = nrmlc(1)*vcl(1,lb) + nrmlc(2)*vcl(2,lb) + &
      nrmlc(3)*vcl(3,lb) - nrmlc(4)

    if (abs(db) <= dtol) then
      cb = 0
    else if (abs(db) <= mindis) then
      return
    else if (db < 0.0d0) then
      cb = -1
    else
      cb = 1
    end if

    if (ca*cb <= 0) then

      nedg = nedg + 1
      ne = ne + 3

      if (ne > maxiw) then
        ierr = 6
        return
      end if

      if (ccw == succ) then
        iwk(ne-2) = a
        iwk(ne-1) = fvl(edgc,a)
      else
        iwk(ne-2) = b
        iwk(ne-1) = fvl(edgc,b)
      end if

      iwk(ne) = (ca+2)*10 + (cb+2)

    end if

  end if

  a = b
  if (a /= facep(1,f)) go to 40

  nv = max(nv,k)
  ptr = pfl(2,ptr)
  if (ptr /= hfl(p)) go to 30

  if (nedg <= 2) then
    ierr = 347
    return
  else if (ne + max(nv+1, nedg+nedg+2) > maxiw) then
    ierr = 6
    return
  else if (nedg > maxwk) then
    ierr = 7
    return
  else if (nvc + 2 > maxvc) then
    ierr = 14
    return
  end if
!
!  Determine first intersection point of ray PT + T*DIR, T > 0,
!  with boundary of polyhedron P.
!
  nep1 = ne + 1
  fmin = 0
  tmin = 0.0d0
  ptr = hfl(p)

50 continue

  sf = pfl(1,ptr)
  f = abs(sf)
  nflag = (abs(facep(2,f)) /= p)
  dotp = dir(1)*nrml(1,f) + dir(2)*nrml(2,f) + dir(3)*nrml(3,f)
  if (nflag) dotp = -dotp
  if (dotp <= tol) go to 90

  j = facep(1,f)
  la = fvl(loc,j)
  da = nrml(1,f)*vcl(1,la) + nrml(2,f)*vcl(2,la) + &
    nrml(3,f)*vcl(3,la)
  t = (da - nrml(1,f)*pt(1) - nrml(2,f)*pt(2) - nrml(3,f)*pt(3)) /dotp
  if (nflag) t = -t
  if (t <= tol .or. fmin /= 0 .and. t >= tmin) go to 90

  ipt(1:3) = pt(1:3) + t*dir(1:3)

  if (sf < 0 .eqv. nflag) then
    link = succ
  else
    link = pred
  end if

  i = nep1
  iwk(i) = la

70 continue

  j = fvl(link,j)
  i = i + 1
  iwk(i) = fvl(loc,j)
  if (j /= facep(1,f)) go to 70

  call ptpolg(3,3,i-nep1,1,iwk(nep1),vcl,ipt,nrml(1,f),dtol,inout)

  if (inout >= 0) then
    fmin = f
    iomin = inout
    tmin = t
  end if

90 continue

  ptr = pfl(2,ptr)

  if (ptr /= hfl(p)) then
    go to 50
  end if

  if (fmin == 0) then
    ierr = 347
    return
  end if

  ipt(1:3) = pt(1:3) + tmin*dir(1:3)

  if (abs(facep(2,fmin)) == p) then
    sf = facep(2,fmin)
  else
    sf = facep(3,fmin)
  end if

  if (iomin == 1) then
    ibeg = 0
    iend = 1
    vbeg = facep(1,fmin)
    vend = vbeg
    go to 220
  end if
!
!  If IPT lies on boundary of face, determine edge containing IPT
!  and whether edge lies on cut plane.
!
  a = facep(1,fmin)

120 continue

  la = fvl(loc,a)

  do i = 1,3
    cmax = max( abs(vcl(i,la)), abs(ipt(i)) )
    if (abs(vcl(i,la) - ipt(i)) > tol*cmax .and. cmax > tol) then
      go to 140
    end if
  end do

  ierr = 348
  return

140 continue

  a = fvl(succ,a)
  if (a /= facep(1,fmin)) go to 120

  cmin = 2.0d0
  a = facep(1,fmin)
  la = fvl(loc,a)
  va(1:3) = vcl(1:3,la) - ipt(1:3)
  da = va(1)**2 + va(2)**2 + va(3)**2

160 continue

  b = fvl(succ,a)
  lb = fvl(loc,b)
  vb(1:3) = vcl(1:3,lb) - ipt(1:3)
  db = vb(1)**2 + vb(2)**2 + vb(3)**2
  dotp = (va(1)*vb(1) + va(2)*vb(2) + va(3)*vb(3))/sqrt(da*db)

  if (dotp <= -1.0d0 + tol) then
    j = a
    go to 190
  else if (dotp < cmin) then
    cmin = dotp
    j = a
  end if

  a = b
  la = lb
  da = db
  va(1:3) = vb(1:3)
  if (a /= facep(1,fmin)) go to 160

190 continue

  la = fvl(loc,j)
  lb = fvl(loc,fvl(succ,j))
  da = nrmlc(1)*vcl(1,la) + nrmlc(2)*vcl(2,la) + nrmlc(3)*vcl(3,la) &
    - nrmlc(4)
!
!  Starting edge lies on cut plane.  Initialize fields for CUTFAC.
!
  if (abs(da) <= dtol) then

    k = 1
    if (abs(nrmlc(2)) > abs(nrmlc(1))) k = 2
    if (abs(nrmlc(3)) > abs(nrmlc(k))) k = 3

    if (k == 1) then
      cmax = dir(2)*(vcl(3,lb) - vcl(3,la)) - dir(3)* (vcl(2,lb) - vcl(2,la))
    else if (k == 2) then
      cmax = dir(3)*(vcl(1,lb) - vcl(1,la)) - dir(1)* (vcl(3,lb) - vcl(3,la))
    else
      cmax = dir(1)*(vcl(2,lb) - vcl(2,la)) - dir(2)* (vcl(1,lb) - vcl(1,la))
    end if

    if (cmax*nrmlc(k) > 0.0d0) then

      iwk(nep1) = la
      iwk(nep1+2) = lb

      if (sf > 0) then

        if (lb - la > 0) then
          ang = eang(j)
          j = fvl(edgc,j)
        else
          j = fvl(edga,j)
          ang = eang(j)
        end if

      else

        if (lb - la < 0) then
          ang = eang(j)
        else
          ang = eang(fvl(edga,j))
        end if

      end if

    else

      iwk(nep1) = lb
      iwk(nep1+2) = la

      if (sf < 0) then

        if (lb - la < 0) then
          ang = eang(j)
          j = fvl(edgc,j)
        else
          j = fvl(edga,j)
          ang = eang(j)
        end if

      else

        if (lb - la > 0) then
          ang = eang(j)
        else
          ang = eang(fvl(edga,j))
        end if

      end if

    end if

    iwk(nep1+3) = j
    f = fvl(facn,j)
    dotp = nrmlc(1)*nrml(1,f) + nrmlc(2)*nrml(2,f) + nrmlc(3)* &
      nrml(3,f)
    if (abs(dotp) > 1.0d0 - tol) dotp = sign(1.0d0,dotp)
    nflag = (abs(facep(2,f)) /= p)
    if (nflag) dotp = -dotp
    angn = pi() - acos(dotp)
    dotp = dir(1)*nrml(1,f) + dir(2)*nrml(2,f) + dir(3)*nrml(3,f)
    if (nflag) dotp = -dotp
    if (dotp < 0.0d0) angn = 2.0D+00 * pi() - angn
    wk(1) = ang - angn
    if (angn < angacc .or. wk(1) < angacc) return
    i = 1

200 continue

    if (iwk(i) /= j .and. iwk(i+1) /= j) then
      i = i + 3
      go to 200
    end if

    iwk(i) = iwk(ne-2)
    iwk(i+1) = iwk(ne-1)
    iwk(i+2) = iwk(ne)
    nedg = nedg - 1
    go to 300
 
  end if
!
!  Edge containing boundary point IPT does not lie on cut plane.
!
  dotp = nrmlc(1)*(vcl(1,lb) - vcl(1,la)) + nrmlc(2)*(vcl(2,lb) - &
    vcl(2,la)) + nrmlc(3)*(vcl(3,lb) - vcl(3,la))
  if (sf < 0) dotp = -dotp

  if (dotp > 0.0d0) then
    ibeg = 0
    iend = 0
    iwk(nep1+3) = -j
  else
    ibeg = 1
    iend = 1
    iwk(nep1+1) = j
  end if

  k = 1 - ibeg
  l(k) = nvc + k + 1
  vcl(1:3,l(k)) = ipt(1:3)
  vbeg = fvl(succ,j)
  vend = j
!
!  Find 1 or 2 points of intersection of face FMIN with cut plane.
!
220 continue

  ray(1) = nrmlc(2)*nrml(3,fmin) - nrmlc(3)*nrml(2,fmin)
  ray(2) = nrmlc(3)*nrml(1,fmin) - nrmlc(1)*nrml(3,fmin)
  ray(3) = nrmlc(1)*nrml(2,fmin) - nrmlc(2)*nrml(1,fmin)

  if (abs(facep(2,fmin)) /= p) then
    ray(1:3) = -ray(1:3)
  end if

  n = nvc + ibeg + 1
  ineg = 0
  ipos = 0
  smin = 0.0d0
  k = 1
  if (abs(ray(2)) > abs(ray(1))) k = 2
  if (abs(ray(3)) > abs(ray(k))) k = 3
  a = vbeg
  la = fvl(loc,a)
  da = nrmlc(1)*vcl(1,la) + nrmlc(2)*vcl(2,la) + &
    nrmlc(3)*vcl(3,la) - nrmlc(4)

230 continue

  b = fvl(succ,a)
  lb = fvl(loc,b)
  db = nrmlc(1)*vcl(1,lb) + nrmlc(2)*vcl(2,lb) + &
    nrmlc(3)*vcl(3,lb) - nrmlc(4)

  if (abs(da) <= dtol) then

    t = (vcl(k,la) - ipt(k))/ray(k)

    if (t < 0.0d0) then

      if (ibeg == 0) then

        if (ineg == 0 .or. t > smin) then
          ineg = a
          smin = t
          l(0) = la
        end if

      end if

    else

      if (iend == 1) then

        if (ipos == 0 .or. t < tmin) then
          ipos = a
          tmin = t
          l(1) = la
        end if

      end if
    end if

  else if (abs(db) <= dtol) then

    go to 250

  else if (da*db < 0.0d0) then

    va(1:3) = vcl(1:3,la) - ipt(1:3)
    vb(1:3) = vcl(1:3,la) - vcl(1:3,lb)
    cp(1) = ray(2)*vb(3) - ray(3)*vb(2)
    cp(2) = ray(3)*vb(1) - ray(1)*vb(3)
    cp(3) = ray(1)*vb(2) - ray(2)*vb(1)
    j = 1
    if (abs(cp(2)) > abs(cp(1))) j = 2
    if (abs(cp(3)) > abs(cp(j))) j = 3

    if (j == 1) then
      t = (va(2)*vb(3) - va(3)*vb(2))/cp(1)
    else if (j == 2) then
      t = (va(3)*vb(1) - va(1)*vb(3))/cp(2)
    else
      t = (va(1)*vb(2) - va(2)*vb(1))/cp(3)
    end if

    if (t < 0.0d0) then

      if (ibeg == 0) then

        if (ineg == 0 .or. t > smin) then
          ineg = a
          smin = t
          l(0) = n
        end if

      end if

    else

      if (iend == 1) then

        if (ipos == 0 .or. t < tmin) then
          ipos = a
          tmin = t
          l(1) = n
        end if

      end if

    end if

  end if

250 continue

  a = b
  la = lb
  da = db
  if (a /= vend) go to 230

  if (ibeg == 0 .and. ineg == 0 .or. iend == 1 .and. ipos == 0) then
    ierr = 349
    return
  end if

  if (ibeg == 0) then
    if (l(0) > nvc) then
      iwk(nep1+1) = ineg
      vcl(1:3,n) = ipt(1:3) + smin * ray(1:3)
      n = n + 1
    end if
  end if

  if (iend == 1) then

    if (l(1) > nvc) then
      l(1) = n
      iwk(nep1+3) = -ipos
      vcl(1:3,n) = ipt(1:3) + tmin*ray(1:3)
    else if (sf < 0) then
      iwk(nep1+3) = -ipos
    else
      iwk(nep1+3) = -fvl(pred,ipos)
    end if

  end if
!
!  Set or update fields for routine CUTFAC.
!
  iwk(nep1) = l(0)
  iwk(nep1+2) = l(1)

  if (iend == 0 .and. l(0) <= nvc) then

    iwk(nep1+2) = nvc + 1
    vcl(1:3,nvc+1) = vcl(1:3,nvc+2)

  else if (l(0) > nvc) then

    a = iwk(nep1+1)
    la = fvl(loc,a)
    lb = fvl(loc,fvl(succ,a))

    if ((lb - la)*sf > 0) then
      iwk(nep1+1) = fvl(edgc,a)
    else
      iwk(nep1+1) = fvl(edga,a)
    end if

  end if

  if (l(1) > nvc) then

    j = -iwk(nep1+3)
    i = 1

290 continue

    if (iwk(i) /= j .and. iwk(i+1) /= j) then
      i = i + 3
      go to 290
    end if

    iwk(i) = iwk(ne-2)
    iwk(i+1) = iwk(ne-1)
    iwk(i+2) = iwk(ne)
    nedg = nedg - 1

  end if

  dotp = nrmlc(1)*nrml(1,fmin) + nrmlc(2)*nrml(2,fmin) + nrmlc(3)* &
    nrml(3,fmin)

  if (abs(dotp) > 1.0d0 - tol) dotp = sign(1.0d0,dotp)
  if (abs(facep(2,fmin)) /= p) dotp = -dotp
  wk(1) = acos(dotp)
  angn = pi() - wk(1)

  if (angn < angacc .or. wk(1) < angacc) return

300 continue

  call cutfac(p,nrmlc,angacc,dtol,nvc,maxvc,vcl,facep,nrml,fvl,eang, &
     nedg,iwk,nce,iwk(nep1),wk,rflag,ierr)

  if (ierr /= 0) return

  if ( rflag ) then
    call insfac(p,nrmlc,nce,iwk(nep1),wk,nvc,nface,nvert,npolh, &
      npf,maxfp,maxfv,maxhf,maxpf,vcl,facep,factyp,nrml,fvl,eang, &
      hfl,pfl,ierr)
  end if

  return
end
subroutine resvrf ( vr, aspc2d, atol2d, maxiw, maxwk, x, y, iang, link, w1, &
  w2, pt1, pt2, iwk, wk, ierr )
!
!******************************************************************************
!
!! RESVRF resolves a reflex vertex of a simple polygon.
!
!
!  Purpose: 
!
!    Resolve reflex vertex of simple polygon with one or two
!    separators, where polygon is face of polyhedron.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, VR - index in X,Y of reflex vertex.
!
!    Input, ASPC2D - angle spacing parameter in radians used in controlling
!    vertices to be considered as an endpoint of a separator.
!
!    Input, ATOL2D - angle tolerance parameter in radians used in accepting
!    separator(s).
!
!    Input, MAXIW - maximum size available for IWK array; should be about
!    3 times number of vertices in polygon.
!
!    Input, MAXWK - maximum size available for WK array; should be about
!    5 times number of vertices in polygon.
!
!    Input, X(1:*), Y(1:*), IANG(1:*), LINK(1:*) - data structure for simple
!    polygonal region containing reflex vertex; arrays are
!    for x- and y-coordinates, interior angle, counter clockwise link.
!
!    Output, W1 - index in X,Y of vertex which is the endpoint of separator
!    in inner cone or right cone with respect to reflex vertex;
!    on edge starting at (X(-W1),Y(-W1)) if negative.
!
!    Output, W2 - 0 if there is only one separator; else index in X,Y of
!    vertex which is endpoint of 2nd separator in left cone;
!    on edge starting at (X(-W2),Y(-W2)) if negative.
!
!    Output, PT1(1:2) - coordinates of 1st separator endpoint if W1 < 0.
!
!    Output, PT2(1:2) - coordinates of 2nd separator endpoint if W2 < 0.
!
!    Workspace, IWK(1:MAXIW).
!
!    Wokspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxiw
  integer maxwk
!
  double precision angsep
  double precision aspc2d
  double precision atol2d
  double precision iang(*)
  integer i
  integer i1
  integer i2
  integer ierr
  integer ivis
  integer ivor
  integer ivrt
  integer iwk(maxiw)
  integer link(*)
  integer maxn
  integer nvis
  integer nvor
  integer nvrt
  integer nvsvrt
  double precision pt1(2)
  double precision pt2(2)
  integer theta
  integer v
  integer vr
  integer w1
  integer w2
  double precision wk(maxwk)
  integer wkang
  double precision x(*)
  integer xc
  integer xvor
  double precision y(*)
  integer yc
  integer yvor
!
!  Determine number of vertices in polygon containing reflex vertex.
!
  ierr = 0
  nvrt = 0
  v = vr

10 continue

  v = link(v)

  if (v /= vr) then
    nvrt = nvrt + 1
    go to 10
  end if

  maxn = nvrt + int(iang(vr)/aspc2d)
!
!  Set up work arrays for routine VISPOL, and determine whether there
!  is enough workspace. XC, YC are d.p. arrays of length NVRT in WK,
!  used for the coordinates of the polygon containing the reflex
!  vertex. MAXN positions are reserved for XC, YC since this is the
!  maximum space required by routine VISVRT. IVIS is an integer array
!  of length MAXN in IWK. IVRT is an integer array of length NVRT in
!  IWK used temporarily for storing indices of vertices in X,Y.
!
  if (maxn + nvrt > maxiw) then
    ierr = 6
    return
  else if (maxn + maxn > maxwk) then
    ierr = 7
    return
  end if

  ivis = 1
  ivrt = ivis + maxn
  xc = 1
  yc = xc + maxn
  v = link(vr)

  do i = 0,nvrt-1
    wk(xc+i) = x(v)
    wk(yc+i) = y(v)
    iwk(ivrt+i) = v
    v = link(v)
  end do

  call vispol(x(vr),y(vr),nvrt-1,wk(xc),wk(yc),nvis,iwk(ivis), ierr )
  if (ierr /= 0) return
!
!  XC, YC now contain visibility polygon coordinates. Update MAXN
!  and set up d.p. array THETA of length MAXN in WK for routine
!  VISVRT. Elements of IVIS are changed to indices of X,Y after call.
!
  maxn = maxn - nvrt + nvis + 1
  theta = yc + maxn

  if (theta + maxn - 1 > maxwk) then
    ierr = 7
    return
  end if

  call visvrt(aspc2d,x(vr),y(vr),nvis,wk(xc),wk(yc),iwk(ivis), &
    maxn-1,nvsvrt,wk(theta))

  wk(theta+nvsvrt) = iang(vr)

  do i = ivis,ivis+nvsvrt

    v = iwk(i)

    if (v >= 0) then
      iwk(i) = iwk(ivrt+v)
    else
      iwk(i) = -iwk(ivrt-v-1)
    end if

  end do
!
!  XC, YC now contain coord. of visible vertices to be considered
!  as an endpoint of a separator. Set up work arrays for routine
!  VORNBR. Integer array IVOR and d.p. arrays XVOR, YVOR, each of
!  length NVSVRT+1, are added at the end of IWK and WK arrays.
!
  ivor = ivis + nvsvrt + 1
  xvor = theta + nvsvrt + 1
  yvor = xvor + nvsvrt + 1

  if (ivor + nvsvrt > maxiw) then
    ierr = 6
    return
  else if (yvor + nvsvrt > maxwk) then
    ierr = 7
    return
  end if

  call vornbr(x(vr),y(vr),nvsvrt,wk(xc),wk(yc),nvor,iwk(ivor), &
     wk(xvor),wk(yvor), ierr )
  if (ierr /= 0) return
!
!  Set up d.p. array WKANG of length NVOR+1 <= NVSVRT+1 in WK for
!  routine FNDSEP. Only Voronoi neighbors are considered as an
!  endpoint of a separator in the first call to FNDSEP. If the
!  minimum angle created at the boundary by the separator(s) is too
!  small, then a second call is made to FNDSEP in which all visible
!  vertices are considered as an endpoint of a separator.
!
  wkang = xvor

  if (iwk(ivor+nvor) == nvsvrt) then
    nvor = nvor - 1
  end if

  if (iwk(ivor) == 0) then
    ivor = ivor + 1
    nvor = nvor - 1
  end if

  call fndspf(atol2d+atol2d,x(vr),y(vr),nvsvrt,wk(xc),wk(yc), &
     iwk(ivis),wk(theta),nvor,iwk(ivor),x,y,iang,link,angsep,i1,i2, &
     wk(wkang))

  if (angsep < atol2d) then

    ivrt = ivis + nvsvrt + 1

    do i = 1,nvsvrt-1
      iwk(ivrt+i-1) = i
    end do

    call fndspf(atol2d+atol2d,x(vr),y(vr),nvsvrt,wk(xc),wk(yc), &
      iwk(ivis),wk(theta),nvsvrt-2,iwk(ivrt),x,y,iang,link,angsep, &
      i1,i2,wk(wkang))

  end if

  w1 = iwk(ivis+i1)

  if (w1 < 0) then
    pt1(1) = wk(xc+i1)
    pt1(2) = wk(yc+i1)
  end if

  if (i2 < 0) then
    w2 = 0
  else
    w2 = iwk(ivis+i2)
    if (w2 < 0) then
      pt2(1) = wk(xc+i2)
      pt2(2) = wk(yc+i2)
    end if
  end if

  return
end
subroutine resvrh ( xh, yh, aspc2d, atol2d, nvrt, maxn, maxiw, maxwk, x, y, &
  link, xc, yc, ivrt, v, xv, yv, iwk, wk, ierr )
!
!******************************************************************************
!
!! RESVRH resolves a hole vertex on a face by finding a separator.
!
!
!  Purpose: 
!
!    Resolve top or bottom hole vertex on a face by finding
!    a separator above or below vertex, respectively, where the
!    subregion above or below vertex is given.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, XH, YH - coordinates of hole vertex.
!
!    Input, ASPC2D - angle spacing parameter in radians used in controlling
!    vertices to be considered as an endpoint of a separator.
!
!    Input, ATOL2D - angle tolerance parameter in radians used in accepting
!    separator to resolve a hole on a face.
!
!    Input, NVRT - number of vertices in subregion containing hole vertex.
!
!    Input, MAXN - at least NVRT+INT(PI/ASPC2D) indicating space available
!    in XC, YC arrays.
!
!    Input, MAXIW - maximum size available for IWK array; should be about
!    2*MAXN.
!
!    Input, MAXWK - maximum size available for WK array; should be about
!    3*MAXN.
!
!    Input, X(1:*), Y(1:*), LINK(1:*) - used for 2D representation of
!    decomposition of multiply-connected polygonal face
!    of hole polygons; see routine SPDECH.
!
!    Input/output, XC(1:MAXN), YC(1:MAXN).  On input, entries 1 through 
!    NVRT contain coordinates in counter clockwise order of polygon above
!    or below hole vertex; (XH,YH), (XC(1),YC(1)), and (XC(NVRT),YC(NVRT)) 
!    are on horizontal line Y = YH; YC(2), YC(NVRT-1) are both > YH (< YH) 
!    if top (bottom) vertex.  On output, input values are overwritten; it is
!    assumed that MAXN is sufficiently large.
!
!    Input, IVRT(1:NVRT) - indices in X, Y arrays of XC, YC vertices.
!
!    Output, V - index in X, Y arrays of second endpoint of separator if
!    positive; on edge starting at (X(-V),Y(-V)) if negative.
!
!    Output, XV, YV - coordinates of second separator endpoint if V < 0.
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxiw
  integer maxn
  integer maxwk
  integer nvrt
!
  double precision angsep
  double precision aspc2d
  double precision atol2d
  integer i
  integer ierr
  integer iv
  integer ivor
  integer ivrt(nvrt)
  integer iwk(maxiw)
  integer l
  integer link(*)
  integer nmax
  integer nvis
  integer nvor
  integer nvsvrt
  integer v
  double precision wk(maxwk)
  double precision x(*)
  double precision xc(maxn)
  double precision xh
  double precision xv
  integer xvor
  double precision y(*)
  double precision yc(maxn)
  double precision yh
  double precision yv
  integer yvor
!
  ierr = 0

  if (maxn > maxiw) then
    ierr = 6
    return
  end if

  call vispol(xh,yh,nvrt-1,xc,yc,nvis,iwk, ierr )
  if (ierr /= 0) return
  nmax = maxn - nvrt + nvis + 1

  if (nmax > maxwk) then
    ierr = 7
    return
  end if
!
!  XC, YC arrays are now overwritten by visibility polygon vertices,
!  then by visible vertices to be considered as separator endpoint.
!  Elements of IVIS = IWK(1:NVSVRT+1) are changed to indices of X, Y.
!  For I > 1, IWK(I) = J > 0 if (XC(I),YC(I)) = (X(K),Y(K)) where
!  K = LINK(J), and IWK(I) = J < 0 if (XC(I),YC(I)) lies in interior
!  of edge starting at (X(-J),Y(-J)). WK(1:NVSVRT+1) contain angles.
!
  call visvrt(aspc2d,xh,yh,nvis,xc,yc,iwk,nmax-1,nvsvrt,wk)
  iwk(1) = ivrt(1)

  do i = 2,nvsvrt+1

    l = iwk(i)

    if (l >= 0) then
      iwk(i) = ivrt(l)
    else
      iwk(i) = -ivrt(-l)
    end if

  end do
!
!  Determine visible vertices which are Voronoi nbrs of (XH,YH).
!
  ivor = nvsvrt + 2
  xvor = nvsvrt + 2
  yvor = xvor + nvsvrt + 1

  if (ivor + nvsvrt > maxiw) then
    ierr = 6
    return
  else if (yvor + nvsvrt > maxwk) then
    ierr = 7
    return
  end if

  call vornbr(xh,yh,nvsvrt,xc,yc,nvor,iwk(ivor),wk(xvor),wk(yvor), ierr )
  if (ierr /= 0) return
!
!  Try to find acceptable separator from Voronoi neighbors based on
!  max-min angle criterion. If not successful, find separator by
!  considering all visible vertices as possible endpoint.
!
  if (iwk(ivor+nvor) == nvsvrt) nvor = nvor - 1

  if (iwk(ivor) == 0) then
    ivor = ivor + 1
    nvor = nvor - 1
  end if

  call fndsph(xh,yh,nvsvrt,xc,yc,iwk,wk,nvor,iwk(ivor),x,y,link, &
     angsep,v)

  if (angsep < atol2d) then

    iv = nvsvrt + 1

    do i = 1,nvsvrt-1
      iwk(iv+i) = i
    end do

    iv = iv + 1
    call fndsph(xh,yh,nvsvrt,xc,yc,iwk,wk,nvsvrt-2,iwk(iv),x,y, &
      link,angsep,v)

  end if

  v = v + 1

  if (iwk(v) > 0) then
    v = link(iwk(v))
  else
    xv = xc(v)
    yv = yc(v)
    v = iwk(v)
  end if

  return
end
subroutine resvrt ( vr, angspc, angtol, nvc, nvert, maxvc, maxpv, maxiw,  &
  maxwk, vcl, pvl, iang, w1, w2, iwk, wk, ierror )
!
!*******************************************************************************
!
!! RESVRT resolves a reflex vertex of a simply connected polygon.
!
!
!  Purpose:
!
!    Resolve reflex vertex of simply connected polygon with
!    one or two separators. The reflex vertex must be a 'simple'
!    vertex of the polygon.
!
!  Modified:
!
!    12 July 1999
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, integer VR, the index in PVL of reflex vertex.
!
!    Input, double precision ANGSPC, the angle spacing parameter used in
!    controlling the vertices to be considered as an endpoint of a separator.
!
!    Input, double precision ANGTOL, the angle tolerance parameter used in
!    accepting separator(s).
!
!    Input/output, integer NVC, the number of positions used in VCL array.
!
!    Input/output, integer NVERT, the number of positions used in PVL array.
!
!    Input, integer MAXVC, the maximum size available for VCL array.
!
!    Input, integer MAXPV, the maximum size available for PVL array.
!
!    Input, integer MAXIW, the maximum size available for IWK array; should
!    be about 3 times number of vertices in polygon.
!
!    Input, integer MAXWK, the maximum size available for WK array; should
!    be about 5 times number of vertices in polygon.
!
!    Input/output, double precision VCL(1:2,1:NVC), the vertex coordinate list.
!
!    Input/output, integer PVL(1:4,1:NVERT), double precision IANG(1:NVERT),
!    the polygon vertex list and interior angles.
!
!    Output, integer W1, the index in PVL of vertex which is the endpoint
!    of separator in inner cone or right cone with respect to the reflex vertex.
!
!    Output, integer W2, is 0 if there is only one separator; else index
!    in PVL of vertex which is endpoint of second separator in left cone.
!
!    Workspace, integer IWK(1:MAXIW).
!
!    Workspace, double precision WK(1:MAXWK).
!
!    Output, integer IERROR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxiw
  integer maxpv
  integer maxvc
  integer maxwk
!
  double precision angsep
  double precision angspc
  double precision angtol
  double precision iang(maxpv)
  integer ierror
  integer i
  integer i1
  integer i2
  integer ivis
  integer ivor
  integer ivrt
  integer iwk(maxiw)
  integer l
  integer, parameter :: loc = 1
  integer maxn
  integer nvc
  integer nvert
  integer nvis
  integer nvor
  integer nvrt
  integer nvsvrt
  integer pvl(4,maxpv)
  integer, parameter :: succ = 3
  integer theta
  integer v
  double precision vcl(2,maxvc)
  integer vr
  integer w1
  integer w2
  double precision wk(maxwk)
  integer wkang
  integer xc
  double precision xr
  integer xvor
  integer yc
  double precision yr
  integer yvor
!
!  Determine number of vertices in polygon containing reflex vertex.
!
  nvrt = 0
  v = vr

  do

    v = pvl(succ,v)

    if ( v == vr ) then
      exit
    end if

    nvrt = nvrt + 1

  end do

  maxn = nvrt + int ( iang(vr) / angspc )
  l = pvl(loc,vr)
  xr = vcl(1,l)
  yr = vcl(2,l)
!
!  Set up work arrays for routine VISPOL, and determine whether there
!  is enough workspace. XC, YC are d.p. arrays of length NVRT in WK,
!  used for the coordinates of the polygon containing the reflex
!  vertex. MAXN positions are reserved for XC, YC since this is the
!  maximum space required by routine VISVRT. IVIS is an integer array
!  of length MAXN in IWK. IVRT is an integer array of length NVRT in
!  IWK used temporarily for storing indices of vertices in PVL.
!
  if ( maxn + nvrt > maxiw ) then
    write ( *, * ) ' '
    write ( *, * ) 'RESVRT - Fatal error!'
    write ( *, * ) '  MAXN + NVRT > MAXIW.'
    ierror = 6
    return
  end if

  if ( maxn + maxn > maxwk ) then
    write ( *, * ) ' '
    write ( *, * ) 'RESVRT - Fatal error!'
    write ( *, * ) '  MAXN + MAXN > MAXWK.'
    ierror = 7
    return
  end if

  ivis = 1
  ivrt = ivis + maxn
  xc = 1
  yc = xc + maxn
  v = pvl(succ,vr)

  do i = 0, nvrt-1
    l = pvl(loc,v)
    wk(xc+i) = vcl(1,l)
    wk(yc+i) = vcl(2,l)
    iwk(ivrt+i) = v
    v = pvl(succ,v)
  end do

  call vispol ( xr, yr, nvrt-1, wk(xc), wk(yc), nvis, iwk(ivis), ierror )

  if ( ierror /= 0 ) then
    return
  end if
!
!  XC, YC now contain visibility polygon coordinates. Update MAXN
!  and set up d.p. array THETA of length MAXN in WK for routine
!  VISVRT. Elements of IVIS are changed to indices of PVL after call.
!
  maxn = maxn - nvrt + nvis + 1
  theta = yc + maxn

  if ( theta + maxn - 1 > maxwk ) then
    write ( *, * ) ' '
    write ( *, * ) 'RESVRT - Fatal error!'
    write ( *, * ) '  THETA + MAXN - 1 > MAXWK.'
    ierror = 7
    return
  end if

  call visvrt ( angspc, xr, yr, nvis, wk(xc), wk(yc), iwk(ivis), maxn-1, &
    nvsvrt, wk(theta) )

  wk(theta+nvsvrt) = iang(vr)

  do i = ivis, ivis+nvsvrt
    l = iwk(i)
    if ( l >= 0 ) then
      iwk(i) = iwk(ivrt+l)
    else
      iwk(i) = -iwk(ivrt-l-1)
    end if
  end do
!
!  XC, YC now contain coordinates of visible vertices to be considered
!  as an endpoint of a separator. Set up work arrays for routine
!  VORNBR. Integer array IVOR and d.p. arrays XVOR, YVOR, each of
!  length NVSVRT+1, are added at the end of IWK and WK arrays.
!
  ivor = ivis + nvsvrt + 1
  xvor = theta + nvsvrt + 1
  yvor = xvor + nvsvrt + 1

  if ( ivor + nvsvrt > maxiw ) then
    ierror = 6
    return
  end if

  if ( yvor + nvsvrt > maxwk ) then
    ierror = 7
    return
  end if

  call vornbr ( xr, yr, nvsvrt, wk(xc), wk(yc), nvor, iwk(ivor), wk(xvor), &
    wk(yvor), ierror )

  if ( ierror /= 0 ) then
    return
  end if
!
!  Set up the array WKANG of length NVOR+1 <= NVSVRT+1 in WK for
!  routine FNDSEP. Only Voronoi neighbors are considered as an
!  endpoint of a separator in the first call to FNDSEP. If the
!  minimum angle created at the boundary by the separator(s) is too
!  small, then a second call is made to FNDSEP in which all visible
!  vertices are considered as an endpoint of a separator.
!
  wkang = xvor
  if ( iwk(ivor+nvor) == nvsvrt ) then
    nvor = nvor - 1
  end if

  if ( iwk(ivor) == 0 ) then
    ivor = ivor + 1
    nvor = nvor - 1
  end if

  call fndsep ( angtol+angtol, xr, yr, nvsvrt, wk(xc), wk(yc), iwk(ivis), &
    wk(theta), nvor, iwk(ivor), vcl, pvl, iang, angsep, i1, i2, wk(wkang) )

  if ( angsep < angtol ) then

    ivrt = ivis + nvsvrt + 1

    do i = 1, nvsvrt-1
      iwk(ivrt+i-1) = i
    end do

    call fndsep ( angtol+angtol, xr, yr, nvsvrt, wk(xc), wk(yc), iwk(ivis), &
      wk(theta), nvsvrt-2, iwk(ivrt), vcl, pvl, iang, angsep, i1, i2, &
      wk(wkang) )

  end if
!
!  Insert endpoint(s) of separator(s) in vertex coordinate list and
!  polygon vertex list data structures, if they are not yet there.
!
  if ( i2 == -1 ) then
    w2 = 0
  else if ( iwk(ivis+i2) < 0 ) then
    call insvr2 ( wk(xc+i2), wk(yc+i2), -iwk(ivis+i2), nvc, nvert, maxvc, &
      maxpv, vcl, pvl, iang, w2, ierror )
    if ( ierror /= 0 ) then
      return
    end if
  else
    w2 = iwk(ivis+i2)
  end if

  if ( iwk(ivis+i1) < 0 ) then
    call insvr2 ( wk(xc+i1), wk(yc+i1), -iwk(ivis+i1), nvc, nvert, maxvc, &
      maxpv, vcl, pvl, iang, w1, ierror )
    if ( ierror /= 0 ) then
      return
    end if
  else
    w1 = iwk(ivis+i1)
  end if

  return
end
subroutine rmcled ( nface, nvert, hvl, fvl )
!
!******************************************************************************
!
!! RMCLED removes collinear adjacent convex polyhedron edges from the database.
!
!
!  Purpose: 
!
!    Remove collinear adjacent edges from convex polyhedron
!    data structure (assuming no coplanar adjacent faces).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NFACE - number of faces in convex polyhedron.
!
!    Input/output, NVERT - size of FVL; >= twice number of edges of polyhedron.
!
!    Input/output, HVL(1:NFACE) - head vertex list.
!
!    Input/output, FVL(1:5,1:NVERT) - face vertex list; see routine DSCPH; may
!    contain unused columns, indicated by LOC values <= 0.
!
  implicit none
!
  integer nface
  integer nvert
!
  integer a
  integer b
  integer c
  integer, parameter :: edgv = 5
  integer f
  integer, parameter :: facn = 2
  integer fvl(5,nvert)
  integer g
  integer hvl(nface)
  integer i
  integer j
  integer, parameter :: loc = 1
  integer, parameter :: pred = 4
  integer, parameter :: succ = 3
!
  do f = 1,nface

    j = hvl(f)

    do

      i = fvl(pred,j)
      a = fvl(edgv,j)
      b = fvl(edgv,i)

      if (fvl(facn,a) /= fvl(facn,b)) then
        exit
      end if

      j = fvl(succ,j)

    end do

    hvl(f) = j
    i = j
    b = fvl(edgv,i)

30  continue

    j = fvl(succ,i)
    a = fvl(edgv,j)

    if (fvl(facn,a) == fvl(facn,b)) then

      g = fvl(facn,b)
      c = fvl(succ,b)

40    continue

      fvl(loc,j) = 0
      fvl(loc,b) = 0
      j = fvl(succ,j)
      b = a
      a = fvl(edgv,j)
      if (fvl(facn,a) == g) go to 40

      fvl(succ,i) = j
      fvl(pred,j) = i
      fvl(succ,b) = c
      fvl(pred,c) = b
      fvl(edgv,i) = b
      fvl(edgv,b) = i

    end if

    i = j
    b = a
    if (i /= hvl(f)) go to 30

  end do

  do while (fvl(loc,nvert) <= 0)
    nvert = nvert - 1
  end do

  return
end
subroutine rmcpfc ( nface, nvert, hvl, nrml, fvl, eang, iwk )
!
!******************************************************************************
!
!! RMCPFC removes coplanar adjacent polyhedron faces from the data base.
!
!
!  Purpose: 
!
!    Remove coplanar adjacent faces from convex polyhedron
!    data structure.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input/output, NFACE - number of faces in convex polyhedron.
!
!    Input/output, NVERT - size of FVL, EANG arrays; >= twice number of edges
!    of polyhedron.
!
!    Input/output, HVL(1:NFACE) - head vertex list.  On output, first NFACE
!    entries indicate resulting faces.
!
!    Input/output, NRML(1:3,1:NFACE) - unit outward normals of faces.
!
!    Input/output, FVL(1:5,1:NVERT) - face vertex list; see routine DSCPH.
!    On output, may contain some unused columns, indicated by nonpositive 
!    LOC values.
!
!    Input, EANG(1:NVERT) - angles at edges common to 2 faces; EANG(I)
!    corresponds to FVL(*,I).
!
!    Workspace, IWK(1:NVERT).
!
  implicit none
!
  integer nface
  integer nvert
!
  integer a
  integer b
  double precision eang(nvert)
  integer, parameter :: edgv = 5
  integer f
  integer, parameter :: facn = 2
  integer fvl(5,nvert)
  integer g
  integer hvl(nface)
  integer i
  integer ii
  integer iwk(nvert)
  integer j
  integer, parameter :: loc = 1
  integer np
  double precision nrml(3,nface)
  double precision pi
  double precision pitol
  integer, parameter :: pred = 4
  integer, parameter :: succ = 3
  double precision tol
!
  tol = 100.0D+00 * epsilon ( tol )
  pitol = pi() - tol
  np = 0

  do i = 1,nvert
    iwk(i) = 0
    if (fvl(loc,i) <= 0) iwk(i) = 1
  end do

  do ii = 1,nvert

    if (iwk(ii) == 1) then
      cycle
    end if

    i = ii
    a = 0
    b = 0

20  continue

    iwk(i) = 1

    if (eang(i) >= pitol) then
      np = np + 1
      fvl(loc,i) = 0
      a = i
    else
      b = i
    end if

    i = fvl(succ,fvl(edgv,i))
    if (i /= ii) go to 20

    if (a == 0 .or. b == 0) then
      cycle
    end if

    a = b

30  continue

    i = fvl(edgv,a)
    j = fvl(succ,i)

    if (eang(j) >= pitol) then

40    continue

      j = fvl(succ,fvl(edgv,j))
      if (eang(j) >= pitol) go to 40

      fvl(succ,i) = j
      fvl(pred,j) = i

    end if

    a = j
    if (a /= b) go to 30

  end do

  if (np == 0) return

  do i = 1,nvert

    if (iwk(i) == 2 .or. fvl(loc,i) <= 0)then
      cycle
    end if

    f = fvl(facn,i)
    hvl(f) = i
    j = i

60  continue

    iwk(j) = 2
    g = fvl(facn,j)

    if (g /= f) then
      fvl(facn,j) = f
      hvl(g) = 0
    end if

    j = fvl(succ,j)
    if (j /= i) go to 60

  end do

  do i = 1,nface
    if (hvl(i) > 0) then
      if (fvl(loc,hvl(i)) <= 0) hvl(i) = 0
    end if
  end do

  f = 0

  do i = 1,nface

    j = hvl(i)

    if (j > 0) then

      f = f + 1

      if (f < i) then

        hvl(f) = j

90      continue
        fvl(facn,j) = f
        j = fvl(succ,j)
        if (j /= hvl(f)) go to 90

        nrml(1,f) = nrml(1,i)
        nrml(2,f) = nrml(2,i)
        nrml(3,f) = nrml(3,i)

      end if

    end if

  end do

  nface = f

110 continue

  if (fvl(loc,nvert) <= 0) then
    nvert = nvert - 1
    go to 110
  end if

  return
end
subroutine rotiar ( n, arr, shift )
!
!******************************************************************************
!
!! ROTIAR rotates elements of an integer array.
!
!
!  Purpose: 
!
!    Rotate elements of integer array.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!   Input, N - number of elements of array.
!
!   Input/output, ARR(0:N-1) - the integer array, which has been shifted
!   on output.
!
!   Input, SHIFT - amount of (left) shift or rotation; ARR(SHIFT) on input
!   becomes ARR(0) on output.
!
  implicit none
!
  integer n
!
  integer a
  integer arr(0:n-1)
  integer b
  integer i
  integer j
  integer k
  integer l
  integer m
  integer r
  integer sh
  integer shift
  integer t
!
  sh = mod(shift,n)
  if (sh < 0) sh = sh + n
  if (sh == 0) return
  a = n
  b = sh

20 continue

  r = mod(a,b)
  a = b
  b = r
  if (r > 0) go to 20

  m = n/a - 1

  do i = 0,a-1
    t = arr(i)
    k = i
    do j = 1,m
      l = k + sh
      if (l >= n) l = l - n
      arr(k) = arr(l)
      k = l
    end do
    arr(k) = t
  end do

  return
end
subroutine rotipg ( xeye, yeye, nvrt, xc, yc, ierr )
!
!******************************************************************************
!
!! ROTIPG rotates the indices of the vertices of a simple polygon.
!
!
!  Purpose: 
!
!    Rotate the indices of the vertices of a simple polygon
!    and possibly insert one vertex so that (XC(0),YC(0)) is the
!    point on the horizontal line through (XEYE,YEYE) and on the
!    boundary of the polygon which is closest to and to the right
!    of (XEYE,YEYE).  (XEYE,YEYE) is an eyepoint in the interior or
!    blocked exterior of the polygon.  In the former (latter) case,
!    the vertices must be in counter clockwise (CW) order.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, XEYE, YEYE - coordinates of eyepoint.  On output, vertices
!    are in same orientation, but with indices rotated, and possibly
!    (XC(0),YC(0)) = (XC(NVRT),YC(NVRT)) has been added.
!
!    Input/output, NVRT - number of vertices on boundary of simple polygon.
!    On output, increased by 1 from input iff the closest vertex
!    is a new vertex.
!
!    Input, XC(0:NVRT), YC(0:NVRT) - vertices of polygon in counter 
!    clockwise (or clockwise) order if eyepoint is interior (or blocked
!    exterior); (XC(0),YC(0)) = (XC(NVRT),YC(NVRT))
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer nvrt
!
  integer a
  integer b
  double precision dy
  integer i
  integer ierr
  integer irgt
  integer j
  integer k
  integer l
  integer m
  integer n
  integer r
  double precision tol
  double precision xc(0:nvrt+1)
  double precision xeye
  double precision xint
  double precision xrgt
  double precision xt
  double precision yc(0:nvrt+1)
  double precision yeye
  double precision yeyemt
  double precision yeyept
  double precision yt
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )

  dy = 0.0d0
  do i = 0,nvrt-1
    dy = max(dy,abs(yc(i+1)-yc(i)))
  end do

  yeyemt = yeye - tol*dy
  yeyept = yeye + tol*dy
  n = nvrt + 1
  irgt = n
  xrgt = 0.0d0
!
!  Determine closest point on boundary which is to the right of
!  (XEYE,YEYE) and on the horizontal line through (XEYE,YEYE).
!  The closest point must be on an edge which intersects the
!  horizontal line and has (XEYE,YEYE) to its left.
!
  do i = 0,nvrt-1

    if (yc(i) > yeyept .or. yc(i+1) < yeyemt) then
      cycle
    end if

    if (yc(i) < yeyemt .and. yc(i+1) > yeyept) then

      xint = (yeye-yc(i))*(xc(i+1)-xc(i))/(yc(i+1)-yc(i)) + xc(i)

      if (xint > xeye) then
        if (xint < xrgt .or. irgt == n) then
          irgt = -(i + 1)
          xrgt = xint
        end if
      end if

    else if (yc(i) >= yeyemt .and. yc(i+1) > yeyept) then

      if (xc(i) > xeye) then
        if (xc(i) < xrgt .or. irgt == n) then
          irgt = i
          xrgt = xc(i)
        end if
      end if

    else if (yc(i) < yeyemt .and. yc(i+1) <= yeyept) then

      if (xc(i+1) > xeye) then
        if (xc(i+1) < xrgt .or. irgt == n) then
          irgt = i + 1
          xrgt = xc(i+1)
        end if
      end if
    end if

  end do

  if (irgt == n) then
    ierr = 205
    return
  end if

  if (irgt == 0 .or. irgt == nvrt) then
    return
  end if

  if (irgt < 0) then

    irgt = -irgt
    do i = nvrt,irgt,-1
      xc(i+1) = xc(i)
      yc(i+1) = yc(i)
    end do

    xc(irgt) = xrgt
    yc(irgt) = yeye
    nvrt = nvrt + 1

  end if
!
!  Rotate the indices of the vertices so that (XC(IRGT),YC(IRGT))
!  becomes (XC(0),YC(0)). Compute A = GCD(NVRT,IRGT).
!
  a = nvrt
  b = irgt

40 continue

  r = mod(a,b)
  a = b
  b = r
  if (r > 0) go to 40

  m = nvrt/a - 1

  do i = 0,a-1

    xt = xc(i)
    yt = yc(i)
    k = i

    do j = 1,m
      l = k + irgt
      if (l >= nvrt) l = l - nvrt
      xc(k) = xc(l)
      yc(k) = yc(l)
      k = l
    end do

    xc(k) = xt
    yc(k) = yt

  end do

  xc(nvrt) = xc(0)
  yc(nvrt) = yc(0)

  return
end
subroutine rotpg ( nvrt, xc, yc, i1, i2, ibot, costh, sinth )
!
!*******************************************************************************
!
!! ROTPG rotates a convex polygon.
!
!
!  Purpose:
!
!    Rotate convex polygon so that a line segment joining two
!    of its vertices is parallel to y-axis.
!
!  Modified:
!
!    12 July 1999
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, integer NVRT, the number of vertices on the boundary of
!    the convex polygon.
!
!    Input/output, double precision XC(0:NVRT), YC(0:NVRT).  The vertex
!    coordinates in counter clockwise order;
!      (XC(0),YC(0)) = (XC(NVRT),YC(NVRT)).
!    On output, the rotated vertex coordinates; indices are
!    also rotated so that (XC(0),YC(0)) = (XC(NVRT),YC(NVRT))
!    is top vertex and (XC(IBOT),YC(IBOT)) is bottom vertex.
!
!    Input, integer I1, I2, the index of vertices of line segment; I1, I2 > 0.
!
!    Output, integer IBOT, the index of bottom vertex.
!
!    Output, double precision COSTH, SINTH, the values COS(THETA) and
!    SIN(THETA) where THETA in [-PI,PI] is the rotation angle.
!
  implicit none
!
  integer nvrt
!
  integer a
  integer b
  double precision costh
  integer i
  integer i1
  integer i2
  integer ibot
  integer itop
  integer j
  integer k
  integer l
  integer m
  double precision pi
  integer r
  double precision sinth
  double precision theta
  double precision tol
  double precision x0
  double precision xc(0:nvrt)
  double precision y0
  double precision yc(0:nvrt)
!
  tol = 100.0D+00 * epsilon ( tol )

  itop = i1
  ibot = i2

  if ( yc(i1) == yc(i2) ) then
    if ( xc(i1) < xc(i2) ) then
      theta = -pi() / 2.0D+00
    else
      theta = pi() / 2.0D+00
    end if
  else
    if ( yc(i1) < yc(i2) ) then
      itop = i2
      ibot = i1
    end if
    theta = pi() / 2.0D+00 - atan2 ( yc(itop) - yc(ibot), xc(itop) - xc(ibot) )
  end if

  costh = cos(theta)
  sinth = sin(theta)

  do i = 1, nvrt
    x0 = xc(i)
    xc(i) = costh * x0 - sinth * yc(i)
    yc(i) = sinth * x0 + costh * yc(i)
  end do
!
!  Rotate indices.
!
  if ( itop /= nvrt ) then

    a = nvrt
    b = itop

    do

      r = mod ( a, b )
      a = b
      b = r

      if ( r <= 0 ) then
        exit
      end if

    end do

    m = nvrt / a - 1

    do i = 1, a

      x0 = xc(i)
      y0 = yc(i)
      k = i

      do j = 1, m
        l = k + itop
        if ( l > nvrt ) then
          l = l - nvrt
        end if
        xc(k) = xc(l)
        yc(k) = yc(l)
        k = l
      end do

      xc(k) = x0
      yc(k) = y0

    end do

    ibot = ibot - itop
    if ( ibot < 0 ) then
      ibot = ibot + nvrt
    end if

  end if

  xc(0) = xc(nvrt)
  yc(0) = yc(nvrt)

  return
end
function sangmn ( a, b, c, d, sang )
!
!******************************************************************************
!
!! SANGMN computes the minimum solid angle of a tetrahedron.
!
!
!  Purpose: 
!
!    Compute 4 solid angles of tetrahedron and their minimum.
!    Actually, sin(solid angle / 2) is computed.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A(1:3),B(1:3),C(1:3),D(1:3) - 4 vertices of tetrahedron
!
!    Output, SANG(1:4) - 4 solid angles at A,B,C,D, respectively. 
!    (Actually sin(a/2)).
!
!    Output, SANGMN - sin(min solid angle / 2) = min(SANG(1:4))
!    since sum of 4 solid angles <= 2*pi.
!
  implicit none
!
  double precision a(3)
  double precision ab(3)
  double precision ac(3)
  double precision ad(3)
  double precision b(3)
  double precision bc(3)
  double precision bd(3)
  double precision c(3)
  double precision cd(3)
  double precision d(3)
  double precision denom
  integer i
  double precision l1
  double precision l2
  double precision l3
  double precision lab
  double precision lac
  double precision lad
  double precision lbc
  double precision lbd
  double precision lcd
  double precision sang(4)
  double precision sangmn
  double precision vol
!
  ab(1:3) = b(1:3) - a(1:3)
  ac(1:3) = c(1:3) - a(1:3)
  ad(1:3) = d(1:3) - a(1:3)
  bc(1:3) = c(1:3) - b(1:3)
  bd(1:3) = d(1:3) - b(1:3)
  cd(1:3) = d(1:3) - c(1:3)
 
  lab = sqrt(ab(1)**2 + ab(2)**2 + ab(3)**2)
  lac = sqrt(ac(1)**2 + ac(2)**2 + ac(3)**2)
  lad = sqrt(ad(1)**2 + ad(2)**2 + ad(3)**2)
  lbc = sqrt(bc(1)**2 + bc(2)**2 + bc(3)**2)
  lbd = sqrt(bd(1)**2 + bd(2)**2 + bd(3)**2)
  lcd = sqrt(cd(1)**2 + cd(2)**2 + cd(3)**2)
  vol = abs(ab(1)*(ac(2)*ad(3) - ac(3)*ad(2)) + ab(2)*(ac(3)*ad(1) &
    - ac(1)*ad(3)) + ab(3)*(ac(1)*ad(2) - ac(2)*ad(1)))*2.0d0
  l1 = lab + lac
  l2 = lab + lad
  l3 = lac + lad
  denom = (l1+lbc)*(l1-lbc)*(l2+lbd)*(l2-lbd)*(l3+lcd)*(l3-lcd)

  if (denom <= 0.0d0) then
    sang(1) = 0.0d0
  else
    sang(1) = vol/sqrt(denom)
  end if

  l1 = lab + lbc
  l2 = lab + lbd
  l3 = lbc + lbd
  denom = (l1+lac)*(l1-lac)*(l2+lad)*(l2-lad)*(l3+lcd)*(l3-lcd)

  if (denom <= 0.0d0) then
    sang(2) = 0.0d0
  else
    sang(2) = vol/sqrt(denom)
  end if

  l1 = lac + lbc
  l2 = lac + lcd
  l3 = lbc + lcd
  denom = (l1+lab)*(l1-lab)*(l2+lad)*(l2-lad)*(l3+lbd)*(l3-lbd)

  if (denom <= 0.0d0) then
    sang(3) = 0.0d0
  else
    sang(3) = vol/sqrt(denom)
  end if

  l1 = lad + lbd
  l2 = lad + lcd
  l3 = lbd + lcd
  denom = (l1+lab)*(l1-lab)*(l2+lac)*(l2-lac)*(l3+lbc)*(l3-lbc)

  if (denom <= 0.0d0) then
    sang(4) = 0.0d0
  else
    sang(4) = vol/sqrt(denom)
  end if

  sangmn = min(sang(1),sang(2),sang(3),sang(4))

  return
end
subroutine sdang ( a, b, c, d, sang, dang )
!
!******************************************************************************
!
!! SDANG computes the solid and dihedral angles of a tetrahedron.
!
!
!  Purpose: 
!
!    Compute 4 solid angles (+ PI) and 6 dihedral angles of
!    tetrahedron in radians. Solid angle at vertex A is
!    (dihedral angle at edge AB) + (dihedral angle at edge AC)
!    + (dihedral angle at edge AD) - PI
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A(1:3), B(1:3), C(1:3), D(1:3) - 4 vertices of tetrahedron.
!
!    Output, SANG(1:4) - 4 solid angles (+ PI) at A,B,C,D, respectively.
!
!    Output, DANG(1:6) - 6 dihedral angles at AB,AC,AD,BC,BD,CD, respectively.
!
  implicit none
!
  double precision a(3)
  double precision ab(3)
  double precision ac(3)
  double precision ad(3)
  double precision angle3
  double precision b(3)
  double precision bc(3)
  double precision bd(3)
  double precision c(3)
  double precision d(3)
  double precision dang(6)
  integer i
  double precision nrmabc(3)
  double precision nrmabd(3)
  double precision nrmacd(3)
  double precision nrmbcd(3)
  double precision pi
  double precision rtolsq
  double precision sang(4)
  double precision tol
!
!  Compute outward (or inward) normals at 4 faces to get angles.
!
  tol = 100.0D+00 * epsilon ( tol )

  ab(1:3) = b(1:3) - a(1:3)
  ac(1:3) = c(1:3) - a(1:3)
  ad(1:3) = d(1:3) - a(1:3)
  bc(1:3) = c(1:3) - b(1:3)
  bd(1:3) = d(1:3) - b(1:3)
 
  rtolsq = tol*max(abs(a(1)),abs(a(2)),abs(a(3)),abs(b(1)), &
    abs(b(2)),abs(b(3)),abs(c(1)),abs(c(2)),abs(c(3)),abs(d(1)), &
    abs(d(2)),abs(d(3)))

  rtolsq = rtolsq**2
  nrmabc(1) = ac(2)*ab(3) - ac(3)*ab(2)
  nrmabc(2) = ac(3)*ab(1) - ac(1)*ab(3)
  nrmabc(3) = ac(1)*ab(2) - ac(2)*ab(1)
  nrmabd(1) = ab(2)*ad(3) - ab(3)*ad(2)
  nrmabd(2) = ab(3)*ad(1) - ab(1)*ad(3)
  nrmabd(3) = ab(1)*ad(2) - ab(2)*ad(1)
  nrmacd(1) = ad(2)*ac(3) - ad(3)*ac(2)
  nrmacd(2) = ad(3)*ac(1) - ad(1)*ac(3)
  nrmacd(3) = ad(1)*ac(2) - ad(2)*ac(1)
  nrmbcd(1) = bc(2)*bd(3) - bc(3)*bd(2)
  nrmbcd(2) = bc(3)*bd(1) - bc(1)*bd(3)
  nrmbcd(3) = bc(1)*bd(2) - bc(2)*bd(1)
  dang(1) = pi() - angle3(nrmabc,nrmabd,rtolsq)
  dang(2) = pi() - angle3(nrmabc,nrmacd,rtolsq)
  dang(3) = pi() - angle3(nrmabd,nrmacd,rtolsq)
  dang(4) = pi() - angle3(nrmabc,nrmbcd,rtolsq)
  dang(5) = pi() - angle3(nrmabd,nrmbcd,rtolsq)
  dang(6) = pi() - angle3(nrmacd,nrmbcd,rtolsq)
  sang(1) = dang(1) + dang(2) + dang(3)
  sang(2) = dang(1) + dang(4) + dang(5)
  sang(3) = dang(2) + dang(4) + dang(6)
  sang(4) = dang(3) + dang(5) + dang(6)

  return
end
subroutine sepfac ( p, cntr, nrmlc, angacc, mxcos, dtol, nvc, maxvc, vcl, &
  facep, nrml, fvl, eang, nce, cedge, cdang, aflag, ierr )
!
!******************************************************************************
!
!! SEPFAC traces out a separator in a convex polyhedron.
!
!
!  Purpose: 
!
!    Trace out separator or cut face in convex polyhedron P
!    given a starting edge on boundary and normal to cut face.
!    Accept cut face if it creates no small angles or short edges.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, P - polyhedron index.
!
!    Input, CNTR(1:3) - weighted centroid of polyhedron.
!
!    Input, NRMLC(1:4) - unit normal vector of cut plane plus right hand
!    side constant term of plane equation.
!
!    Input, ANGACC - minimum acceptable dihedral angle in radians produced by
!    a cut face.
!
!    Input, MXCOS - maximum cosine of angle allowed for angles subtended by new
!    subedges with respect to centroid.
!
!    Input, DTOL - absolute tolerance to determine if point is on cut plane.
!
!    Input, NVC - number of vertex coordinates.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input/output, VCL(1:3,1:NVC+2) - vertex coordinate list.
!
!    Input, FACEP(1:3,1:*) - face pointer list.
!
!    Input, NRML(1:3,1:*) - unit normal vectors for faces.
!
!    Input, FVL(1:6,1:*) - face vertex list.
!
!    Input, EANG(1:*) - edge angles.
!
!    Input, CEDGE(1:2,0:1) - CEDGE(1,0) = LU, CEDGE(1,1) = LV where LU, LV
!    are indices of VCL and are vertices of starting edge;
!    LU may be NVC+1 and LV may be NVC+1 or NVC+2 to indicate
!    a point on interior of an edge; CEDGE(2,1) is specified
!    as described for output below; if LU > NVC, CEDGE(2,0)
!    takes on the value ABS(CEDGE(2,NCE)) described for output
!    below, else CEDGE(2,0) is not used.
!
!    Input, CDANG(1) - dihedral angle at starting edge determined by cut
!    plane in positive half-space.
!
!    Output, NCE - number of edges in cut polygon (if acceptable); it is
!    assumed there is enough space in the following two arrays.
!
!    Output, CEDGE(1:2,0:NCE) - CEDGE(1,I) is an index of VCL, indices > NVC
!    are new points; CEDGE(2,I) = J indicates that edge of cut
!    face ending at CEDGE(1,I) is edge from J to FVL(SUCC,J)
!    if J > 0; else if J < 0 then edge of cut face ending at
!    CEDGE(1,I) is a new edge and CEDGE(1,I) lies on edge from
!    -J to FVL(SUC,-J) and new edge lies in face FVL(FACN,-J);
!    CEDGE(2,I) always refers to an edge in the subpolyhedron
!    in negative half-space; CEDGE(1,NCE) = CEDGE(1,0);
!    CEDGE(2,0) is not used.
!
!    Output, CDANG(1:NCE) - dihedral angles created by edges of cut polygon
!    in positive half-space; negative sign for angle I indicates that
!    face containing edge I is oriented CW in polyhedron P.
!
!    Output, AFLAG - .TRUE. iff separator face is acceptable.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxvc
!
  integer a
  logical aflag
  double precision ang
  double precision angacc
  double precision angr
  integer b
  integer ccwfl
  double precision cdang(*)
  integer cedge(2,0:*)
  double precision cntr(3)
  double precision cp
  double precision de(3)
  double precision dee(3)
  double precision dir(3)
  double precision dist
  double precision distb
  double precision dotp
  double precision dtol
  integer e
  double precision eang(*)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer ee
  logical eflag
  integer estrt
  integer estop
  integer facep(3,*)
  integer, parameter :: facn = 2
  integer fl
  integer fr
  integer fvl(6,*)
  integer ierr
  integer k
  integer l
  integer la
  integer lb
  double precision leng
  integer, parameter :: loc = 1
  integer lu
  integer lw
  integer lw1
  integer, parameter :: msglvl = 0
  double precision mxcos
  integer n
  integer nce
  double precision nmax
  double precision nrml(3,*)
  double precision nrmlc(4)
  double precision ntol
  integer nvc
  integer p
  double precision pi
  integer, parameter :: pred = 4
  integer sgn
  integer sp
  integer, parameter :: succ = 3
  double precision t
  double precision tol
  double precision vcl(3,maxvc)
  integer w
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  aflag = .false.
  n = max(nvc, cedge(1,0), cedge(1,1))
  nce = 1
  lu = cedge(1,0)
  lw = lu
  lw1 = cedge(1,1)
  w = cedge(2,1)

  if (w > 0) then
    fl = fvl(facn,w)
    if (lw > lw1) then
      fr = fvl(facn,fvl(edgc,w))
    else
      fr = fvl(facn,fvl(edga,w))
    end if
  else
    fl = fvl(facn,-w)
    fr = fl
  end if

  dir(1) = vcl(1,lw1) - vcl(1,lw)
  dir(2) = vcl(2,lw1) - vcl(2,lw)
  dir(3) = vcl(3,lw1) - vcl(3,lw)

  if (abs(facep(2,fl)) == p) then
    ccwfl = facep(2,fl)
  else
    ccwfl = facep(3,fl)
  end if

  if (ccwfl < 0) cdang(1) = -cdang(1)
!
!  LW, LW1, FL, FR, DIR are updated before each iteration of loop.
!  counter clockwiseFL = P (-P) if FL is CCW (CW) according to SUCC traversal.
!
10 continue
!
!  LW1 is new vertex on interior of edge E. FL is used for
!  previous and next faces containing edges of cut polygon.
!
  if (lw1 > nvc) then

    e = -cedge(2,nce)
    la = fvl(loc,e)
    lb = fvl(loc,fvl(succ,e))

    if ((lb - la)*ccwfl > 0) then
      ee = fvl(edgc,e)
    else
      ee = fvl(edga,e)
    end if

    fl = fvl(facn,ee)

    if (abs(facep(2,fl)) == p) then
      ccwfl = facep(2,fl)
    else
      ccwfl = facep(3,fl)
    end if

    dir(1) = nrmlc(2)*nrml(3,fl) - nrmlc(3)*nrml(2,fl)
    dir(2) = nrmlc(3)*nrml(1,fl) - nrmlc(1)*nrml(3,fl)
    dir(3) = nrmlc(1)*nrml(2,fl) - nrmlc(2)*nrml(1,fl)

    if (abs(facep(2,fl)) /= p) then
      dir(1:3) = -dir(1:3)
    end if

  else
!
!  LW1 is existing vertex of polyhedron P. FL (FL and FR) is
!  previous face if edge ending at LW1 is new (already exists).
!  In former case, -CEDGE(2,NCE) is the edge of FL incident on
!  LW1 which will lie only in subpolyhedron PL. In latter case,
!  CEDGE(2,NCE) is an edge of FL. Cycle thru edges, faces counter clockwise
!  (from outside) between edges ESTRT, ESTOP.
!
    eflag = (cedge(2,nce) > 0)

    if (.not. eflag) then

      estrt = -cedge(2,nce)
      sp = ccwfl

      if (ccwfl > 0) then
        estop = fvl(succ,estrt)
      else
        estop = fvl(pred,estrt)
      end if

    else

      w = cedge(2,nce)

      if (ccwfl > 0) then
        estrt = fvl(pred,w)
        l = lw - lw1
      else
        estrt = fvl(succ,w)
        l = lw1 - lw
      end if

      if (l*ccwfl > 0) then
        w = fvl(edgc,w)
      else
        w = fvl(edga,w)
      end if

      if (abs(facep(2,fr)) == p) then
        sp = facep(2,fr)
      else
        sp = facep(3,fr)
      end if

      if (sp > 0) then
        estop = fvl(succ,w)
      else
        estop = fvl(pred,w)
      end if

    end if

    la = fvl(loc,estop)
    lb = fvl(loc,fvl(succ,estop))

    if ((lb - la)*sp > 0) then
      estop = fvl(edgc,estop)
    else
      estop = fvl(edga,estop)
    end if

    e = estrt

20  continue

    if (eflag .or. (e /= estrt .and. e /= estop)) then
!
!  Determine if edge lies on cut plane.
!
      if (fvl(loc,e) == lw1) then
        l = fvl(loc,fvl(succ,e))
      else
        l = fvl(loc,e)
      end if

      dist = nrmlc(1)*vcl(1,l) + nrmlc(2)*vcl(2,l) + &
        nrmlc(3)*vcl(3,l) - nrmlc(4)

      if (abs(dist) <= dtol) then

        dir(1:3) = vcl(1:3,l) - vcl(1:3,lw1)
        lw = lw1
        lw1 = l
        nce = nce + 1
        cedge(1,nce) = lw1
        cedge(2,nce) = e
        fl = fvl(facn,e)

        if (abs(facep(2,fl)) == p) then
          ccwfl = facep(2,fl)
        else
          ccwfl = facep(3,fl)
        end if

        if (ccwfl > 0) then
          l = lw - lw1
        else
          l = lw1 - lw
        end if

        if (l*ccwfl > 0) then
          fr = fvl(facn,fvl(edgc,e))
        else
          fr = fvl(facn,fvl(edga,e))
        end if

        go to 40

      end if

    end if

    if (e == estop) then
      ierr = 334
      return
    end if
!
!  Determine if cut plane intersects interior of face.
!
    la = fvl(loc,e)
    lb = fvl(loc,fvl(succ,e))

    if ((lb - la)*ccwfl > 0) then
      e = fvl(edgc,e)
    else
      e = fvl(edga,e)
    end if

    fl = fvl(facn,e)

    if (abs(facep(2,fl)) == p) then
      ccwfl = facep(2,fl)
    else
      ccwfl = facep(3,fl)
    end if

    if (ccwfl > 0) then

      ee = fvl(pred,e)
      la = fvl(loc,fvl(succ,e))
      lb = fvl(loc,ee)

    else

      ee = fvl(succ,e)
      la = fvl(loc,e)
      lb = fvl(loc,fvl(succ,ee))

    end if

    dir(1) = nrmlc(2)*nrml(3,fl) - nrmlc(3)*nrml(2,fl)
    dir(2) = nrmlc(3)*nrml(1,fl) - nrmlc(1)*nrml(3,fl)
    dir(3) = nrmlc(1)*nrml(2,fl) - nrmlc(2)*nrml(1,fl)
    sgn = 1

    if (abs(facep(2,fl)) /= p) then
      dir(1:3) = -dir(1:3)
      sgn = -1
    end if

    k = 1
    if (abs(nrml(2,fl)) > abs(nrml(1,fl))) k = 2
    if (abs(nrml(3,fl)) > abs(nrml(k,fl))) k = 3
    nmax = sgn*nrml(k,fl)
    de(1) = vcl(1,la) - vcl(1,lw1)
    de(2) = vcl(2,la) - vcl(2,lw1)
    de(3) = vcl(3,la) - vcl(3,lw1)
    dee(1) = vcl(1,lb) - vcl(1,lw1)
    dee(2) = vcl(2,lb) - vcl(2,lw1)
    dee(3) = vcl(3,lb) - vcl(3,lw1)
    ntol = tol*max(abs(de(1)), abs(de(2)), abs(de(3)), &
      abs(dee(1)), abs(dee(2)), abs(dee(3)))
    e = ee

    if (k == 1) then
      cp = de(2)*dir(3) - de(3)*dir(2)
    else if (k == 2) then
      cp = de(3)*dir(1) - de(1)*dir(3)
    else
      cp = de(1)*dir(2) - de(2)*dir(1)
    end if

    if (abs(cp) <= ntol .or. cp*nmax < 0.0d0) go to 20

    if (k == 1) then
      cp = dir(2)*dee(3) - dir(3)*dee(2)
    else if (k == 2) then
      cp = dir(3)*dee(1) - dir(1)*dee(3)
    else
      cp = dir(1)*dee(2) - dir(2)*dee(1)
    end if

    if (abs(cp) <= ntol .or. cp*nmax < 0.0d0) go to 20

  end if
!
!  Next cut edge is in interior of face FL. Determine LW1.
!  Edge EE containing LW has been saved above in both cases.
!
  fr = fl
  lw = lw1

  if (lw > nvc) then
    a = fvl(succ,ee)
    e = ee
  else if (fvl(loc,ee) == lw) then
    a = fvl(succ,ee)
    e = fvl(pred,ee)
  else
    a = fvl(succ,fvl(succ,ee))
    e = ee
  end if

  la = fvl(loc,a)
  dist = nrmlc(4) - nrmlc(1)*vcl(1,la) - nrmlc(2)*vcl(2,la) - &
    nrmlc(3)*vcl(3,la)

30 continue

  b = fvl(succ,a)
  lb = fvl(loc,b)
  distb = nrmlc(4) - nrmlc(1)*vcl(1,lb) - nrmlc(2)*vcl(2,lb) - &
    nrmlc(3)*vcl(3,lb)

  if (b /= e .and. abs(distb) <= dtol) then

    lw1 = lb
    nce = nce + 1
    cedge(1,nce) = lw1

    if (ccwfl > 0) then
      cedge(2,nce) = -a
    else
      cedge(2,nce) = -b
    end if

    go to 40

  else if (dist*distb < 0.0d0) then

    if (lu > nvc) then

      if (a == cedge(2,0)) then
        lw1 = lu
        nce = nce + 1
        cedge(1,nce) = lu
        cedge(2,nce) = -a
        go to 40
      end if

    end if

    de(1) = vcl(1,lb) - vcl(1,la)
    de(2) = vcl(2,lb) - vcl(2,la)
    de(3) = vcl(3,lb) - vcl(3,la)
    t = dist/(nrmlc(1)*de(1)+nrmlc(2)*de(2)+nrmlc(3)*de(3))
    n = n + 1

    if (n > maxvc) then
      ierr = 14
      return
    end if

    vcl(1,n) = vcl(1,la) + t*de(1)
    vcl(2,n) = vcl(2,la) + t*de(2)
    vcl(3,n) = vcl(3,la) + t*de(3)
!
!  If angles subtended from centroid by subedges are too
!  small, reject cut plane.
!
    de(1:3) = vcl(1:3,n) - cntr(1:3)
    leng = de(1)**2 + de(2)**2 + de(3)**2
    dee(1) = vcl(1,la) - cntr(1)
    dee(2) = vcl(2,la) - cntr(2)
    dee(3) = vcl(3,la) - cntr(3)
    t = dee(1)**2 + dee(2)**2 + dee(3)**2
    dotp = (de(1)*dee(1) + de(2)*dee(2) + de(3)*dee(3))/ sqrt(leng*t)

    if (dotp > mxcos) then

      if (msglvl == 4) then
        write ( *,600) 'rejected due to short subedge'
      end if

      return

    end if

    dee(1) = vcl(1,lb) - cntr(1)
    dee(2) = vcl(2,lb) - cntr(2)
    dee(3) = vcl(3,lb) - cntr(3)
    t = dee(1)**2 + dee(2)**2 + dee(3)**2
    dotp = (de(1)*dee(1) + de(2)*dee(2) + de(3)*dee(3))/ sqrt(leng*t)

    if (dotp > mxcos) then

      if (msglvl == 4) then
        write ( *,600) 'rejected due to short subedge'
      end if

      return

    end if

    lw1 = n
    nce = nce + 1
    cedge(1,nce) = n
    cedge(2,nce) = -a
    go to 40

  end if

  if (b /= e) then
    a = b
    la = lb
    dist = distb
    go to 30
  else
    ierr = 335
    return
  end if
!
!  Compute dihedral angles due to cut plane at cut edge. If any
!  angle is too small, reject cut plane.
!
40 continue

  dotp = nrmlc(1)*nrml(1,fr) + nrmlc(2)*nrml(2,fr) + &
    nrmlc(3)*nrml(3,fr)

  if (abs(dotp) > 1.0d0 - tol) dotp = sign(1.0d0,dotp)
  if (abs(facep(2,fr)) /= p) dotp = -dotp
  angr = acos(dotp)

  if (fr == fl) then

    ang = pi()

  else

    a = cedge(2,nce)
    la = fvl(loc,a)
    lb = fvl(loc,fvl(succ,a))

    if ((lb - la)*ccwfl > 0) then
      ang = eang(a)
    else
      ang = eang(fvl(edga,a))
    end if

  end if

  if (angr < angacc .or. ang - angr < angacc) then
    if (msglvl == 4) then
      write ( *,600) 'rejected due to small angle'
    end if
    return
  end if

  if (ccwfl > 0) then
    cdang(nce) = angr
  else
    cdang(nce) = -angr
  end if

  if (lw1 /= lu) go to 10

  aflag = .true.

  if (msglvl == 4) then
    write ( *,600) 'cedge(1:2), cdang'
    do k = 1,nce
      write ( *,610) k,cedge(1,k),cedge(2,k),cdang(k)*180.0d0/pi()
    end do
  end if

  600 format (4x,a)
  610 format (4x,3i7,f12.5)

  return
end
subroutine sepmdf ( angtol, nvrt, xc, yc, arpoly, mean, mdftr, indpvl, &
  iang, i1, i2 )
!
!******************************************************************************
!
!! SEPMDF determines a separator that splits a convex polygon.
!
!
!  Purpose: 
!
!    Determine separator to split convex polygon into two
!    parts based on mesh distribution function.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, ANGTOL - angle tolerance parameter (in radians).
!
!    Input, NVRT - number of vertices in polygon.
!
!    Input, XC(0:NVRT), YC(0:NVRT) - coordinates of polygon vertices in
!    counter clockwise order, translated so that centroid is at origin;
!    (XC(0),YC(0)) = (XC(NVRT),YC(NVRT)).
!
!    Input, ARPOLY - area of polygon.
!
!    Input, MEAN - mean mdf value in polygon.
!
!    Input, MDFTR(0:NVRT-1) - mean mdf value in each triangle of polygon;
!    triangles are determined by polygon vertices and centroid.
!
!    Input, INDPVL(0:NVRT) - indices in PVL of vertices; INDPVL(I) = -K if
!    (XC(I),YC(I)) is extra vertex inserted on edge from
!    K to PVL(SUCC,K).
!
!    Input, IANG(1:*) - interior angle array
!
!    Output, I1, I2 - indices in range 0 to NVRT-1 of best separator
!    according to mdf and max-min angle criterion; I1 = -1
!    if no satisfactory separator is found.
!
  implicit none
!
  integer nvrt
!
  double precision angle
  double precision angtol
  double precision areatr
  double precision arpoly
  integer hi
  integer i
  integer i1
  integer i2
  double precision iang(*)
  integer indpvl(0:nvrt)
  integer l
  integer m
  double precision mdftr(0:nvrt-1)
  double precision mean
  double precision pi
  double precision sum2
  double precision tol
  integer v(2)
  integer w(2)
  double precision xc(0:nvrt)
  double precision yc(0:nvrt)
!
!  Determine triangle with highest mean mesh density; then determine
!  triangles adjacent to this triangle with mesh density >= MEAN
!  such that the area of these triangles is <= ARPOLY/2.
!  Note that twice the triangle area is computed.
!
  tol = 100.0D+00 * epsilon ( tol )
  hi = 0

  do i = 1,nvrt-1
    if (mdftr(i) > mdftr(hi)) hi = i
  end do

  sum2 = xc(hi)*yc(hi+1) - xc(hi+1)*yc(hi)
  l = hi - 1
  if (l < 0) l = nvrt - 1
  m = hi + 1
  if (m >= nvrt) m = 0

20 continue

  if (mdftr(l) >= mdftr(m)) then
    i = l
  else
    i = m
  end if

  if (mdftr(i) < mean) go to 30
  areatr = xc(i)*yc(i+1) - xc(i+1)*yc(i)
  sum2 = sum2 + areatr
  if (sum2 > arpoly) go to 30

  if (i == l) then
    l = l - 1
    if (l < 0) l = nvrt - 1
  else
    m = m + 1
    if (m >= nvrt) m = 0
  end if

  go to 20

30 continue

  l = l + 1
  if (l >= nvrt) l = 0
!
!  Interchange role of L and M depending on angle determined by
!  (XC(M),YC(M)), (0,0), and (XC(L),YC(L)).
!  Possible separators are L,M; L,M+1; L+1,M; L+1,M+1.
!
  if (angle(xc(m),yc(m),0.0d0,0.0d0,xc(l),yc(l)) > pi() ) then
    i = l
    l = m
    m = i
  end if

  v(1) = l
  v(2) = l - 1
  if (v(2) < 0) v(2) = nvrt - 1
  w(1) = m
  w(2) = m + 1
  if (w(2) >= nvrt) w(2) = 0
  call mmasep(angtol,xc,yc,indpvl,iang,v,w,i1,i2)

  return
end
subroutine sepshp ( angtol, nvrt, xc, yc, indpvl, iang, i1, i2, wk, ierr )
!
!******************************************************************************
!
!! SEPSHP determines a separator that splits a convex polygon.
!
!
!  Purpose: 
!
!    Determine separator to split convex polygon into two
!    parts based on shape (diameter) of polygon.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, ANGTOL - angle tolerance parameter (in radians).
!
!    Input, NVRT - number of vertices in polygon.
!
!    Input, XC(0:NVRT), YC(0:NVRT) - coordinates of polygon vertices in
!    counter clockwise order, translated so that centroid is at origin;
!    (XC(0),YC(0)) = (XC(NVRT),YC(NVRT)).
!
!    Input. INDPVL(0:NVRT) - indices in PVL of vertices; INDPVL(I) = -K if
!    (XC(I),YC(I)) is extra vertex inserted on edge from
!    K to PVL(SUCC,K).
!
!    Input, IANG(1:*) - interior angle array
!
!    Output, I1, I2 - indices in range 0 to NVRT-1 of best separator
!    according to shape and max-min angle criterion; I1 = -1
!    if no satisfactory separator is found.
!
!    Workspace, WK(1:2*NVRT).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer nvrt
!
  double precision angtol
  double precision dist
  double precision dx
  double precision dy
  integer i
  integer i1
  integer i2
  double precision iang(*)
  integer ierr
  integer indpvl(0:nvrt)
  integer k
  integer n
  double precision pi
  double precision pimtol
  double precision tol
  integer v(2)
  integer w(2)
  double precision wk(2*nvrt)
  double precision xa
  double precision xc(0:nvrt)
  double precision ya
  double precision yc(0:nvrt)
!
!  Determine diameter of polygon.  Possible separators endpoints (two
!  on each side of polygon) are nearest to perpendicular bisector of
!  diameter.  (XA,YA) and (XA+DX,YA+DY) are on bisector.  Distance to
!  bisector is proportional to two times triangle area.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  pimtol = pi() - tol
  n = 0

  do i = 0,nvrt-1
    k = indpvl(i)
    if (k > 0) then
      if (iang(k) < pimtol) then
        n = n + 1
        wk(n) = xc(i)
        wk(n+nvrt) = yc(i)
      end if
    end if
  end do

  call diam2(n,wk,wk(nvrt+1),i1,i2,dist,ierr)
  if (ierr /= 0) return

  if (i1 > i2) then
    i = i1
    i1 = i2
    i2 = i
  end if

  dx = wk(i2+nvrt) - wk(i1+nvrt)
  dy = wk(i1) - wk(i2)
  xa = 0.5d0*(wk(i1) + wk(i2) - dx)
  ya = 0.5d0*(wk(i1+nvrt) + wk(i2+nvrt) - dy)

  i = i1 - 1

20 continue

  if (xc(i) == wk(i1) .and. yc(i) == wk(i1+nvrt)) then
    i1 = i
  else
    i = i + 1
    go to 20
  end if

  i = max(i2-1,i1+1)

30 continue

  if (xc(i) == wk(i2) .and. yc(i) == wk(i2+nvrt)) then
    i2 = i
  else
    i = i + 1
    go to 30
  end if

  i = i1 + 1

40 continue

  dist = dx*(yc(i) - ya) - dy*(xc(i) - xa)

  if (dist >= 0.0d0) then
    v(1) = i - 1
    v(2) = i
  else
    i = i + 1
    go to 40
  end if

  i = i2 + 1

50 continue

  if (i >= nvrt) i = 0
  dist = dx*(yc(i) - ya) - dy*(xc(i) - xa)

  if (dist <= 0.0d0) then
    w(1) = i - 1
    w(2) = i
    if (i <= 0) w(1) = nvrt - 1
  else
    i = i + 1
    go to 50
  end if

  call mmasep(angtol,xc,yc,indpvl,iang,v,w,i1,i2)

  return
end
subroutine sfc1mf ( p, cntr, mean, nf, indf, meanf, angacc, mxcos, dtol, nvc, &
  maxvc, vcl, facep, nrml, fvl, eang, nrmlc, nce, cedge, cdang, aflag, &
  iwk, ierr )
!
!******************************************************************************
!
!! SFC1MF seeks a separator or cut face in a convex polyhedron.
!
!
!  Purpose: 
!
!    Attempt to find a separator or cut face in convex polyhedron
!    P based on mesh distribution function by starting with an edge
!    of polyhedron with a large enough dihedral angle and big difference
!    between mean mdf in 2 faces incident on edge. Accept cut face
!    if it creates no small angles or short edges.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, P - polyhedron index.
!
!    Input, CNTR(1:3) - weighted centroid of polyhedron.
!
!    Input, MEAN - mean mdf value in polyhedron.
!
!    Input, NF - number of faces in polyhedron.
!
!    Input, INDF(1:NF) - indices in FACEP of faces of polyhedron.
!
!    Input, MEANF(1:NF) - mean mdf value associated with faces of polyhedron.
!
!    Input, ANGACC - min acceptable dihedral angle in radians produced by
!    a cut face.
!
!    Input, MXCOS - max cosine of angle allowed for angles subtended by new
!    subedges with respect to centroid.
!
!    Input, DTOL - absolute tolerance to determine if point is on cut plane.
!
!    Input, NVC - number of vertex coordinates.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.  On output,
!    some temporary or permanent entries may be added at end of array.
!
!    Input, FACEP(1:3,1:*) - face pointer list.
!
!    Input, NRML(1:3,1:*) - unit normal vectors for faces.
!
!    Input, FVL(1:6,1:*) - face vertex list.
!
!    Input, EANG(1:*) - edge angles.
!
!    Output, NRMLC(1:4) - unit normal vector of cut plane plus right hand
!    side constant term of plane equation (if acceptable).
!
!    Output, NCE - number of edges in cut face (if acceptable); it is
!    assumed there is enough space in the following two arrays.
!
!    Output, CEDGE(1:2,0:NCE), CDANG(1:NCE) - information describing cut
!    polygon as output by routine SEPFAC.
!
!    Output, AFLAG - .TRUE. iff separator face is found and acceptable.
!
!    Workspace, IWK(1:NF).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxvc
  integer nf
!
  integer a
  logical aflag
  double precision ang
  double precision angacc
  integer b
  double precision cdang(*)
  double precision ce
  integer cedge(2,0:*)
  double precision cn
  double precision cntr(3)
  double precision diffi
  double precision dtol
  integer e(2)
  double precision eang(*)
  double precision edg(3)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  double precision enr(3)
  integer f
  integer facep(3,*)
  integer, parameter :: facn = 2
  double precision, parameter, dimension ( 3 ) :: fract = (/0.5d0,0.4d0,0.6d0/)
  integer fvl(6,*)
  integer i
  integer ierr
  integer indf(nf)
  integer iwk(nf)
  integer j
  integer k
  integer la
  integer lb
  double precision leng
  integer, parameter :: loc = 1
  double precision mean
  double precision meanf(nf)
  double precision mndang
  integer, parameter :: msglvl = 0
  double precision mxcos
  integer n
  integer nce
  logical neg
  double precision nrml(3,*)
  double precision nrmlc(4)
  integer nvc
  integer p
  double precision pi
  integer, parameter :: pred = 4
  double precision r(2)
  double precision ratio
  integer sp
  integer, parameter :: succ = 3
  double precision tol
  double precision vcl(3,maxvc)
!
!  Find up to 2 candidates for starting edge of cut face.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  aflag = .false.
  mndang = min( pi() * 0.9d0,angacc*3.0d0)
  n = 0

  do i = 1,nf
    f = indf(i)
    iwk(i) = facep(1,f)
    facep(1,f) = i
  end do

  do i = 1,nf

    diffi = meanf(i) - mean
    f = indf(i)

    if (abs(facep(2,f)) == p) then
      sp = facep(2,f)
    else
      sp = facep(3,f)
    end if

    a = iwk(i)
    la = fvl(loc,a)

20  continue

    b = fvl(succ,a)
    lb = fvl(loc,b)

    if ((lb - la)*sp > 0) then

      if (eang(a) < mndang) then
        go to 30
      end if

      f = fvl(facn,fvl(edgc,a))
      j = facep(1,f)
      if (diffi*(meanf(j) - mean) >= 0.0d0) go to 30

      if (diffi > 0.0d0) then
        ratio = meanf(i)/meanf(j)
      else
        ratio = meanf(j)/meanf(i)
      end if

      if (ratio < 1.5d0) go to 30

      if (n <= 1) then

        n = n + 1
        e(n) = a
        r(n) = ratio

      else

        if (r(1) < r(2)) then
          k = 1
        else
          k = 2
        end if

        if (ratio > r(k)) then
          e(k) = a
          r(k) = ratio
        end if

      end if

    end if

30  continue

    a = b
    la = lb
    if (a /= iwk(i)) go to 20

  end do

  do i = 1,nf
    facep(1,indf(i)) = iwk(i)
  end do

  if (n == 2) then
    if (r(1) < r(2)) then
      j = e(1)
      e(1) = e(2)
      e(2) = j
    end if
  end if
!
!  For each candidate edge, try 3 different cut planes.
!
  do k = 1,n

    a = e(k)
    la = fvl(loc,a)
    lb = fvl(loc,fvl(succ,a))

    if (lb < la) then
      j = la
      la = lb
      lb = j
    end if

    cedge(1,0) = lb
    cedge(1,1) = la
    cedge(2,1) = a
    edg(1) = vcl(1,lb) - vcl(1,la)
    edg(2) = vcl(2,lb) - vcl(2,la)
    edg(3) = vcl(3,lb) - vcl(3,la)
    leng = sqrt(edg(1)**2 + edg(2)**2 + edg(3)**2)
    edg(1:3) = edg(1:3)/leng
    f = fvl(facn,fvl(edgc,a))
    enr(1) = edg(2)*nrml(3,f) - edg(3)*nrml(2,f)
    enr(2) = edg(3)*nrml(1,f) - edg(1)*nrml(3,f)
    enr(3) = edg(1)*nrml(2,f) - edg(2)*nrml(1,f)
    neg = (abs(facep(2,f)) /= p)

    if (neg) then
      enr(1:3) = -enr(1:3)
    end if

    do i = 1,3

      ang = eang(a)*fract(i)

      if (msglvl == 4) then
        write ( *,600) a,lb,la,f,p, eang(a)*180.0d0/ pi(),ang*180.0d0/ pi()
      end if

      cdang(1) = ang
      cn = cos(ang)
      if (neg) cn = -cn
      ce = sin(ang)
      nrmlc(1) = cn*nrml(1,f) + ce*enr(1)
      nrmlc(2) = cn*nrml(2,f) + ce*enr(2)
      nrmlc(3) = cn*nrml(3,f) + ce*enr(3)
      nrmlc(4) = nrmlc(1)*vcl(1,la) + nrmlc(2)*vcl(2,la) + &
        nrmlc(3)*vcl(3,la)

      call sepfac(p,cntr,nrmlc,angacc,mxcos,dtol,nvc,maxvc,vcl, &
        facep,nrml,fvl,eang,nce,cedge,cdang,aflag, ierr )

      if (ierr /= 0 .or. aflag) then
        return
      end if

    end do

  end do

  600 format (' sfc1mf: a,lb,la,f,p,eang(a),ang =',5i5,2f9.4)

  return
end
subroutine sfc2mf ( p, f, hflag, umdf, widp, nfcev, nedev, nvrev, listev, &
  infoev, ivrt, facval, edgval, vrtval, cntr, angacc, angedg, mxcos, dtol, &
  nvc, maxvc, vcl, facep, nrml, fvl, eang, nrmlc, nce, cedge, cdang, aflag, &
  indv, val, ierr )
!
!******************************************************************************
!
!! SFC2MF finds a separator or cut face in a convex polyhedron.
!
!
!  Purpose: 
!
!    Attempt to find a separator or cut face in convex polyhedron
!    P based on mesh distribution function by starting with an edge
!    in the interior of face F which separates higher mdf values
!    from lower ones on the face.  Accept cut face if it creates no
!    small angles or short edges.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, P - polyhedron index.
!
!    Input, F - face index.
!
!    Input, HFLAG - .TRUE. if heuristic mdf, .FALSE. if user-supplied mdf.
!
!    Input, UMDF(X,Y,Z) - d.p user-supplied mdf with d.p arguments.
!
!    Input, WIDP - width of original polyhedron of decomposition.
!
!    Input, NFCEV, NEDEV, NVREV, LISTEV(1:NFCEV+NEDEV+NVREV),
!    INFOEV(1:4,1:NFCEV+NEDEV) - output from routine PRMDF3.
!
!    Input, IVRT(1:*),FACVAL(1:*),EDGVAL(1:*),VRTVAL(1:*) - arrays output
!    from routine DSMDF3.
!
!    [Note: Parameters WIDP to VRTVAL are used only if HFLAG = TRUE]
!
!    Input, CNTR(1:3) - weighted centroid of polyhedron.
!
!    Input, ANGACC - minimum acceptable dihedral angle in radians produced by
!    a cut face.
!
!    Input, ANGEDG - angle parameter in radians used to determine allowable
!    points on edges as possible endpoints of edges of cut faces.
!
!    Input, MXCOS - maximum cosine of angle allowed for angles subtended by new
!    subedges with respect to centroid = COS(ANGEDG); exception 
!    is that endpoints of start edge may be midpoints of 
!    existing edges.
!
!    Input, DTOL - absolute tolerance to determine if point is on cut plane.
!
!    Input, NVC - number of vertex coordinates.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input/output, VCL(1:3,1:NVC).  On input, vertex coordinate list.
!    On output, some temporary or permanent entries may be added at end
!    of array
!
!    Input, FACEP(1:3,1:*) - face pointer list.
!
!    Input, NRML(1:3,1:*) - unit normal vectors for faces.
!
!    Input, FVL(1:6,1:*) - face vertex list.
!
!    Input, EANG(1:*) - edge angles.
!
!    Output, NRMLC(1:4) - unit normal vector of cut plane plus right hand
!    side constant term of plane equation (if acceptable).
!
!    Output, NCE - number of edges in cut face (if acceptable); it is
!    assumed there is enough space in the following two arrays.
!
!    Output, CEDGE(1:2,0:NCE), CDANG(1:NCE) - information describing cut
!    polygon as output by routine SEPFAC.
!
!    Output, AFLAG - .TRUE. iff separator face is found and acceptable.
!
!    Workspace, INDV(1:2*NE), where NE is number of edges on face F.
!
!    Workspace, VAL(1:2*NE).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxvc
!
  integer a
  logical aflag
  double precision ang
  double precision angacc
  double precision angedg
  integer b
  double precision cdang(*)
  double precision ce
  integer cedge(2,0:*)
  double precision cn
  double precision cntr(3)
  double precision cosed2
  double precision cosmax
  double precision cosmin
  double precision da
  double precision db
  double precision dotp
  double precision dtol
  double precision eang(*)
  double precision edg(3)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  double precision edgval(*)
  double precision enr(3)
  integer f
  integer facep(3,*)
  integer, parameter :: facn = 2
  double precision facval(*)
  double precision, parameter, dimension ( 3 ) :: fract= (/0.5d0,0.4d0,0.6d0/)
  integer fvl(6,*)
  logical hflag
  integer i
  integer ierr
  integer ii(2)
  integer indv(*)
  double precision infoev(4,*)
  integer ivrt(*)
  integer j
  integer jj(2)
  integer k
  integer kvc
  integer l
  integer la
  integer lb
  integer listev(*)
  integer ll(2)
  integer, parameter :: loc = 1
  integer m
  double precision mdf3
  integer mm(2)
  integer, parameter :: msglvl = 0
  double precision mxcos
  integer n
  integer nce
  integer nedev
  logical neg
  integer nfcev
  double precision nrml(3,*)
  double precision nrmlc(4)
  integer nvc
  integer nvrev
  integer p
  double precision pi
  integer, parameter :: pred = 4
  integer s(2)
  integer sf
  integer, parameter :: succ = 3
  double precision tol
  double precision umdf
  double precision va(3)
  double precision val(*)
  double precision vave
  double precision vb(3)
  double precision vcl(3,maxvc)
  double precision vrtval(*)
  double precision widp
!
!  Evaluate MDF at vertices of face and midpoints of edges
!  subtending large angles with respect to centroid.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  aflag = .false.
  cosed2 = cos(angedg*2.0d0)
  kvc = nvc
  n = 0
  a = facep(1,f)
  la = fvl(loc,a)
  va(1) = vcl(1,la) - cntr(1)
  va(2) = vcl(2,la) - cntr(2)
  va(3) = vcl(3,la) - cntr(3)
  da = va(1)**2 + va(2)**2 + va(3)**2

10 continue

  b = fvl(succ,a)
  lb = fvl(loc,b)
  vb(1) = vcl(1,lb) - cntr(1)
  vb(2) = vcl(2,lb) - cntr(2)
  vb(3) = vcl(3,lb) - cntr(3)
  db = vb(1)**2 + vb(2)**2 + vb(3)**2
  dotp = (va(1)*vb(1) + va(2)*vb(2) + va(3)*vb(3))/sqrt(da*db)

  if (dotp < cosed2) then

    kvc = kvc + 1

    if (kvc > maxvc) then
      ierr = 14
      return
    end if

    vcl(1,kvc) = 0.5d0*(vcl(1,la) + vcl(1,lb))
    vcl(2,kvc) = 0.5d0*(vcl(2,la) + vcl(2,lb))
    vcl(3,kvc) = 0.5d0*(vcl(3,la) + vcl(3,lb))
    n = n + 1
    indv(n) = -kvc

    if (hflag) then
      val(n) = mdf3(vcl(1,kvc),vcl(2,kvc),vcl(3,kvc),widp, &
        nfcev,nedev,nvrev,listev,infoev,ivrt,facval,edgval, &
        vrtval,vcl)
    else
      val(n) = umdf(vcl(1,kvc),vcl(2,kvc),vcl(3,kvc))
    end if

  end if

  n = n + 1
  indv(n) = b

  if (hflag) then
    val(n) = mdf3(vcl(1,lb),vcl(2,lb),vcl(3,lb),widp,nfcev, &
      nedev,nvrev,listev,infoev,ivrt,facval,edgval,vrtval,vcl)
  else
    val(n) = umdf(vcl(1,lb),vcl(2,lb),vcl(3,lb))
  end if

  a = b

  if (a /= facep(1,f)) then
    la = lb
    va(1:3) = vb(1:3)
    da = db
    go to 10
  end if
!
!  Try to find a starting edge based on adjacent high MDF values.
!  Select best of 4 possibilities based on max-min angle criterion.
!
  vave = val(1)
  m = 1

  do i = 2,n
    vave = vave + val(i)
    if (val(i) > val(m)) m = i
  end do

  vave = vave/dble(n)
  l = m
  i = m

30 continue

  i = i + 1
  if (i > n) i = 1

  if (val(i) > vave) then
    m = i
    go to 30
  end if

  i = l

40 continue

  i = i - 1
  if (i < 1) i = n

  if (val(i) > vave) then
    l = i
    go to 40
  end if

  ll(1) = l
  ll(2) = l - 1
  if (ll(2) < 1) ll(2) = n
  mm(1) = m
  mm(2) = m + 1
  if (mm(2) > n) mm(2) = 1
  cosmin = 2.0d0

  do i = 1,2

    ii(1) = ll(i)
    s(1) = indv(ii(1))

    if (s(1) > 0) then
      jj(1) = fvl(loc,s(1))
    else
      jj(1) = -s(1)
    end if

    do j = 1,2

      ii(2) = mm(j)

      if (abs(ii(1) - ii(2)) <= 1) then
        cycle
      end if

      s(2) = indv(ii(2))

      if (s(2) > 0) then
        jj(2) = fvl(loc,s(2))
      else
        jj(2) = -s(2)
      end if

      va(1) = vcl(1,jj(2)) - vcl(1,jj(1))
      va(2) = vcl(2,jj(2)) - vcl(2,jj(1))
      va(3) = vcl(3,jj(2)) - vcl(3,jj(1))
      da = va(1)**2 + va(2)**2 + va(3)**2
      cosmax = -1.0d0

      do k = 1,2

        if (k == 2) then
          va(1:3) = -va(1:3)
        end if

        lb = ii(k) - 1
        if (lb < 1) lb = n
        lb = indv(lb)

        if (lb > 0) then
          lb = fvl(loc,lb)
        else
          lb = -lb
        end if

        vb(1) = vcl(1,lb) - vcl(1,jj(k))
        vb(2) = vcl(2,lb) - vcl(2,jj(k))
        vb(3) = vcl(3,lb) - vcl(3,jj(k))
        db = vb(1)**2 + vb(2)**2 + vb(3)**2
        dotp = (va(1)*vb(1)+va(2)*vb(2)+va(3)*vb(3))/sqrt(da*db)
        cosmax = max(cosmax,dotp)

        if (s(k) < 0) then

          cosmax = max(cosmax,-dotp)

        else

          lb = ii(k) + 1
          if (lb > n) lb = 1
          lb = indv(lb)

          if (lb > 0) then
            lb = fvl(loc,lb)
          else
            lb = -lb
          end if

          vb(1) = vcl(1,lb) - vcl(1,jj(k))
          vb(2) = vcl(2,lb) - vcl(2,jj(k))
          vb(3) = vcl(3,lb) - vcl(3,jj(k))
          db = vb(1)**2 + vb(2)**2 + vb(3)**2
          dotp = (va(1)*vb(1) + va(2)*vb(2) + va(3)*vb(3))/ sqrt(da*db)
          cosmax = max(cosmax,dotp)

        end if

      end do

      if (cosmax < cosmin) then
        cosmin = cosmax
        l = ii(1)
        m = ii(2)
      end if

    end do

  end do

  if (cosmin > max(mxcos,cos(angacc))) then
    return
  end if
!
!  For starting edge, try 3 different cut planes.
!
  neg = (abs(facep(2,f)) /= p)

  if (neg) then
    sf = facep(3,f)
  else
    sf = facep(2,f)
  end if

  kvc = nvc
  j = indv(l)
  k = indv(m)

  if (k < j .and. j < 0) then
    call i_swap ( l, m )
    call i_swap ( j, k )
  end if

  if (k > 0) then

    lb = fvl(loc,k)

  else

    kvc = kvc + 1
    lb = -k
    vcl(1,kvc) = vcl(1,lb)
    vcl(2,kvc) = vcl(2,lb)
    vcl(3,kvc) = vcl(3,lb)
    lb = kvc
    m = m - 1
    if (m < 1) m = n
    k = indv(m)
    la = fvl(loc,k)
    m = fvl(loc,fvl(succ,k))

    if ((m - la)*sf > 0) then
      k = fvl(edgc,k)
    else
      k = fvl(edga,k)
    end if

    cedge(2,0) = k

  end if

  if (j > 0) then
    la = fvl(loc,j)
    if (sf > 0) j = fvl(pred,j)
  else
    kvc = kvc + 1
    la = -j
    vcl(1,kvc) = vcl(1,la)
    vcl(2,kvc) = vcl(2,la)
    vcl(3,kvc) = vcl(3,la)
    la = kvc
    l = l - 1
    if (l < 1) l = n
    j = indv(l)
  end if

  cedge(1,0) = lb
  cedge(1,1) = la
  cedge(2,1) = -j
  edg(1) = vcl(1,lb) - vcl(1,la)
  edg(2) = vcl(2,lb) - vcl(2,la)
  edg(3) = vcl(3,lb) - vcl(3,la)
  da = sqrt(edg(1)**2 + edg(2)**2 + edg(3)**2)
  edg(1:3) = edg(1:3)/da
  enr(1) = edg(2)*nrml(3,f) - edg(3)*nrml(2,f)
  enr(2) = edg(3)*nrml(1,f) - edg(1)*nrml(3,f)
  enr(3) = edg(1)*nrml(2,f) - edg(2)*nrml(1,f)

  if (neg) then
    enr(1:3) = -enr(1:3)
  end if

  do i = 1,3

    ang = pi() * fract(i)

    if (msglvl == 4) then
      write ( *,600) j,lb,la,f,p,ang*180.0d0/ pi()
    end if

    cdang(1) = ang
    cn = cos(ang)
    if (neg) cn = -cn
    ce = sin(ang)
    nrmlc(1) = cn*nrml(1,f) + ce*enr(1)
    nrmlc(2) = cn*nrml(2,f) + ce*enr(2)
    nrmlc(3) = cn*nrml(3,f) + ce*enr(3)
    nrmlc(4) = nrmlc(1)*vcl(1,la) + nrmlc(2)*vcl(2,la) + &
      nrmlc(3)*vcl(3,la)

    call sepfac(p,cntr,nrmlc,angacc,mxcos,dtol,nvc,maxvc,vcl, &
      facep,nrml,fvl,eang,nce,cedge,cdang,aflag, ierr )

    if (ierr /= 0 .or. aflag) then
      return
    end if

  end do

  600 format (' sfc2mf: a,lb,la,f,p,ang =',5i5,f9.4)

  return
end
subroutine sfcshp ( p, headp, cntr, angacc, mxcos, dtol, nvc, maxvc, vcl, &
  facep, nrml, fvl, eang, pfl, nrmlc, nce, cedge, cdang, aflag, nv, indv, &
  wvc, ierr )
!
!******************************************************************************
!
!! SFCSHP seeks a separator or cut face in a convex polyhedron.
!
!
!  Purpose: 
!
!    Attempt to find a separator or cut face in convex polyhedron
!    P based on shape of polyhedron, one which nearly bisects diameter
!    of polyhedron and has normal close to diameter vector.  Accept cut
!    face if it creates no small angles or short edges.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, P - polyhedron index.
!
!    Input, HEADP - head pointer to face of PFL for convex polyhedron P.
!
!    Input, CNTR(1:3) - weighted centroid of polyhedron.
!
!    Input, ANGACC - minimum acceptable dihedral angle in radians produced by
!    a cut face.
!
!    Input, MXCOS - maximum cosine of angle allowed for angles subtended by new
!    subedges with respect to centroid; exception is that 
!    endpoints of start edge may be midpoints of existing edges.
!
!    Input, DTOL - absolute tolerance to determine if point is on cut plane.
!
!    Input, NVC - number of vertex coordinates.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input/output, VCL(1:3,1:NVC).  On input, vertex coordinate list.
!    On output, some temporary or permanent entries may be added at end
!    of array.
!
!    Input, FACEP(1:3,1:*) - face pointer list.
!
!    Input, NRML(1:3,1:*) - unit normal vectors for faces.
!
!    Input, FVL(1:6,1:*) - face vertex list.
!
!    Input, EANG(1:*) - edge angles.
!
!    Input, PFL(1:2,1:*) - list of signed face indices for each polyhedron;
!    row 2 used for link.
!
!    Input, NV - number of vertices in polyhedron.
!
!    Output, NRMLC(1:4) - unit normal vector of cut plane plus right hand
!    side constant term of plane equation (if acceptable).
!
!    Output, NCE - number of edges in cut face (if acceptable); it is
!    assumed there is enough space in the following two arrays.
!
!    Output, CEDGE(1:2,0:NCE), CDANG(1:NCE) - information describing cut
!    polygon as output by routine SEPFAC.
!
!    Output, AFLAG - .TRUE. iff separator face is found and acceptable.
!
!    Workspace, INDV(1:NV).
!
!    Workspace, WVC(1:3,1:NV).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxvc
  integer nv
!
  integer a
  integer aa(2)
  logical aflag
  double precision ang
  double precision angacc
  integer b
  integer bb(2)
  double precision cdang(*)
  double precision ce
  integer cedge(2,0:*)
  double precision cn
  double precision cntr(3)
  double precision cosacc
  double precision cosmax
  double precision d
  double precision da
  double precision db
  double precision diam
  double precision dinrm(4)
  integer dir
  double precision distol
  double precision dotp
  double precision dp
  double precision dtol
  double precision eang(*)
  double precision edg(3)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  double precision enr(3)
  integer f
  integer facep(3,*)
  integer, parameter :: facn = 2
  integer fr
  double precision, parameter, dimension(3) :: fract = (/0.5d0,0.4d0,0.6d0/)
  integer fvl(6,*)
  integer headp
  integer i
  integer ia
  integer ib
  integer ierr
  integer indv(nv)
  integer j
  integer jj
  integer k
  integer kvc
  integer l
  integer la
  integer lb
  integer lbb
  double precision leng
  integer ll(2)
  integer, parameter :: loc = 1
  double precision mndang
  double precision mpt(3,2)
  integer, parameter :: msglvl = 0
  double precision mxcos
  integer nce
  logical neg
  double precision nrml(3,*)
  double precision nrmlc(4)
  integer nvc
  integer nvcp2
  integer p
  integer pfl(2,*)
  double precision pi
  integer, parameter :: pred = 4
  integer s(2)
  integer sf
  integer, parameter :: succ = 3
  double precision tol
  double precision v(3)
  double precision vcl(3,maxvc)
  double precision wvc(3,nv)
!
!  Find diameter and plane which perpendicularly bisects diameter.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  aflag = .false.
  mndang = min( pi()*0.9d0,angacc*3.0d0)
  k = 0
  i = headp

10 continue

  f = abs(pfl(1,i))
  j = facep(1,f)

20 continue

  l = fvl(loc,j)
  jj = 1

30 continue

  if (jj <= k) then

    if (l /= fvl(loc,indv(jj))) then
      jj = jj + 1
      go to 30
    end if

  else

    k = jj
    wvc(1,k) = vcl(1,l)
    wvc(2,k) = vcl(2,l)
    wvc(3,k) = vcl(3,l)
    indv(k) = j
    if (k >= nv) go to 40

  end if

  j = fvl(succ,j)
  if (j /= facep(1,f)) go to 20

  i = pfl(2,i)
  if (i /= headp) go to 10

40 continue

  call diam3(nv,wvc,ia,ib,diam)
  diam = sqrt(diam)
  dinrm(1) = (wvc(1,ib) - wvc(1,ia))/diam
  dinrm(2) = (wvc(2,ib) - wvc(2,ia))/diam
  dinrm(3) = (wvc(3,ib) - wvc(3,ia))/diam
  dinrm(4) = 0.5d0*(dinrm(1)*(wvc(1,ia)+wvc(1,ib)) + dinrm(2)* &
    (wvc(2,ia)+wvc(2,ib)) + dinrm(3)*(wvc(3,ia)+wvc(3,ib)))
  distol = 0.2d0*diam
!
!  Attempt to use the following existing edges as a start edge:
!  both endpoints of edge are within distance DISTOL of bisector
!  plane and angle between edge and diameter vector is between
!  60 and 120 degrees.
!
  i = headp

50 continue

  sf = pfl(1,i)
  f = abs(sf)
  a = facep(1,f)
  la = fvl(loc,a)

60 continue

  b = fvl(succ,a)
  lb = fvl(loc,b)
  lbb = lb
  if ((lb - la)*sf < 0) go to 80
  if (eang(a) < mndang) go to 80

  da = abs(dinrm(1)*vcl(1,la) + dinrm(2)*vcl(2,la) + &
    dinrm(3)*vcl(3,la) - dinrm(4))
  if (da > distol) go to 80

  db = abs(dinrm(1)*vcl(1,lb) + dinrm(2)*vcl(2,lb) + &
    dinrm(3)*vcl(3,lb) - dinrm(4))
  if (db > distol) go to 80

  edg(1) = vcl(1,lb) - vcl(1,la)
  edg(2) = vcl(2,lb) - vcl(2,la)
  edg(3) = vcl(3,lb) - vcl(3,la)
  leng = sqrt(edg(1)**2 + edg(2)**2 + edg(3)**2)
  dotp = abs((edg(1)*dinrm(1) + edg(2)*dinrm(2) + edg(3)* &
    dinrm(3))/leng)
  if (dotp > 0.5d0) go to 80

  if (lb < la) then
    call i_swap ( la, lb )
    edg(1:3) = -edg(1:3)
  end if

  cedge(1,0) = lb
  cedge(1,1) = la
  cedge(2,1) = a

  edg(1) = edg(1)/leng
  edg(2) = edg(2)/leng
  edg(3) = edg(3)/leng

  fr = fvl(facn,fvl(edgc,a))
  enr(1) = edg(2)*nrml(3,fr) - edg(3)*nrml(2,fr)
  enr(2) = edg(3)*nrml(1,fr) - edg(1)*nrml(3,fr)
  enr(3) = edg(1)*nrml(2,fr) - edg(2)*nrml(1,fr)
  neg = (abs(facep(2,fr)) /= p)

  if (neg) then
    enr(1) = -enr(1)
    enr(2) = -enr(2)
    enr(3) = -enr(3)
  end if

  do k = 1,3

    ang = eang(a)*fract(k)

    if (msglvl == 4) then
      write ( *,600) a,lb,la,fr,p,eang(a)*180.0d0/ pi(),ang*180.0d0/ pi()
    end if

    cdang(1) = ang
    cn = cos(ang)
    if (neg) cn = -cn
    ce = sin(ang)
    nrmlc(1) = cn*nrml(1,fr) + ce*enr(1)
    nrmlc(2) = cn*nrml(2,fr) + ce*enr(2)
    nrmlc(3) = cn*nrml(3,fr) + ce*enr(3)
    nrmlc(4) = nrmlc(1)*vcl(1,la) + nrmlc(2)*vcl(2,la) + nrmlc(3)*vcl(3,la)

    call sepfac(p,cntr,nrmlc,angacc,mxcos,dtol,nvc,maxvc,vcl, &
      facep,nrml,fvl,eang,nce,cedge,cdang,aflag, ierr )

    if (ierr /= 0 .or. aflag) then
      return
    end if

  end do

80 continue

  a = b
  la = lbb
  if (a /= facep(1,f)) go to 60

  i = pfl(2,i)
  if (i /= headp) go to 50
!
!  If above strategy cannot find an acceptable cut face, then try
!  to find a start edge in the interior of a face which has vertices
!  at distance >= DISTOL above and below bisector plane; start edge
!  should nearly lie on bisector plane.
!
  cosacc = max(mxcos,cos(angacc))

  if (nvc + 4 > maxvc) then
    ierr = 14
    return
  end if

  nvcp2 = nvc + 2
  i = headp

90 continue

  sf = pfl(1,i)
  f = abs(sf)
  j = facep(1,f)
  l = fvl(loc,j)
  da = dinrm(1)*vcl(1,l) + dinrm(2)*vcl(2,l) + dinrm(3)*vcl(3,l) &
    - dinrm(4)
  db = da
  ia = j
  ib = j
  j = fvl(succ,j)

100 continue

  l = fvl(loc,j)
  d = dinrm(1)*vcl(1,l) + dinrm(2)*vcl(2,l) + dinrm(3)*vcl(3,l) - dinrm(4)

  if (d > da) then
    da = d
    ia = j
  else if (d < db) then
    db = d
    ib = j
  end if

  j = fvl(succ,j)
  if (j /= facep(1,f)) go to 100

  if (da < distol .or. db > -distol) go to 160

  dir = succ

  do k = 1,2

    if (k == 2) dir = pred
    dp = da
    j = fvl(dir,ia)

110 continue

    l = fvl(loc,j)
    d = dinrm(1)*vcl(1,l) + dinrm(2)*vcl(2,l) + dinrm(3)*vcl(3,l) - dinrm(4)

    if (d >= 0.0d0) then
      dp = d
      j = fvl(dir,j)
      go to 110
    end if

    bb(k) = j
    aa(k) = fvl(7-dir,j)

    if (dp - d >= distol) then

      la = fvl(loc,aa(k))
      mpt(1,k) = 0.5d0*(vcl(1,la) + vcl(1,l))
      mpt(2,k) = 0.5d0*(vcl(2,la) + vcl(2,l))
      mpt(3,k) = 0.5d0*(vcl(3,la) + vcl(3,l))

      if (0.5d0*(dp + d) >= 0.0d0) then
        aa(k) = -aa(k)
      else
        bb(k) = -bb(k)
      end if

    end if

  end do

  do j = 1,2

    if (j == 1) then

      if (aa(1) == ia .or. bb(2) == ib) then
        cycle
      end if

      s(1) = aa(1)
      s(2) = bb(2)

    else

      if (bb(1) == ib .or. aa(2) == ia) then
        cycle
      end if

      s(1) = bb(1)
      s(2) = aa(2)

    end if

    if (s(1) > 0) then
      ll(1) = fvl(loc,s(1))
    else
      ll(1) = nvcp2 + 1
      vcl(1,ll(1)) = mpt(1,1)
      vcl(2,ll(1)) = mpt(2,1)
      vcl(3,ll(1)) = mpt(3,1)
    end if

    if (s(2) > 0) then
      ll(2) = fvl(loc,s(2))
    else
      ll(2) = nvcp2 + 2
      vcl(1,ll(2)) = mpt(1,2)
      vcl(2,ll(2)) = mpt(2,2)
      vcl(3,ll(2)) = mpt(3,2)
    end if

    edg(1) = vcl(1,ll(2)) - vcl(1,ll(1))
    edg(2) = vcl(2,ll(2)) - vcl(2,ll(1))
    edg(3) = vcl(3,ll(2)) - vcl(3,ll(1))
    leng = edg(1)**2 + edg(2)**2 + edg(3)**2
    cosmax = -1.0d0

    do k = 1,2

      if (s(k) > 0) then
        l = fvl(loc,fvl(succ,s(k)))
      else
        l = fvl(loc,-s(k))
      end if

      v(1) = vcl(1,l) - vcl(1,ll(k))
      v(2) = vcl(2,l) - vcl(2,ll(k))
      v(3) = vcl(3,l) - vcl(3,ll(k))
      d = v(1)**2 + v(2)**2 + v(3)**2
      dotp = (edg(1)*v(1)+edg(2)*v(2)+edg(3)*v(3))/sqrt(leng*d)
      if (k == 2) dotp = -dotp
      cosmax = max(cosmax,dotp)

      if (s(k) < 0) then
        cosmax = max(cosmax,-dotp)
      else
        l = fvl(loc,fvl(pred,s(k)))
        v(1) = vcl(1,l) - vcl(1,ll(k))
        v(2) = vcl(2,l) - vcl(2,ll(k))
        v(3) = vcl(3,l) - vcl(3,ll(k))
        d = v(1)**2 + v(2)**2 + v(3)**2
        dotp = (edg(1)*v(1) + edg(2)*v(2) + edg(3)*v(3))/ sqrt(leng*d)
        if (k == 2) dotp = -dotp
        cosmax = max(cosmax,dotp)
      end if

    end do

    if (cosmax > cosacc) then
      cycle
    end if

    neg = (abs(facep(2,f)) /= p)
    kvc = nvc

    if (ll(2) <= nvc) then
      lb = ll(2)
    else
      kvc = kvc + 1
      lb = kvc
      vcl(1,kvc) = vcl(1,ll(2))
      vcl(2,kvc) = vcl(2,ll(2))
      vcl(3,kvc) = vcl(3,ll(2))
      b = abs(bb(2))
      k = fvl(loc,b)
      l = fvl(loc,fvl(succ,b))

      if ((l - k)*sf > 0) then
        b = fvl(edgc,b)
      else
        b = fvl(edga,b)
      end if

      cedge(2,0) = b

    end if

    if (ll(1) <= nvc) then
      la = ll(1)
      a = s(1)
      if (sf > 0) a = fvl(pred,a)
    else
      kvc = kvc + 1
      la = kvc
      vcl(1,kvc) = vcl(1,ll(1))
      vcl(2,kvc) = vcl(2,ll(1))
      vcl(3,kvc) = vcl(3,ll(1))
      a = abs(aa(1))
    end if

    cedge(1,0) = lb
    cedge(1,1) = la
    cedge(2,1) = -a
    leng = sqrt(leng)
    edg(1:3) = edg(1:3)/leng
    enr(1) = edg(2)*nrml(3,f) - edg(3)*nrml(2,f)
    enr(2) = edg(3)*nrml(1,f) - edg(1)*nrml(3,f)
    enr(3) = edg(1)*nrml(2,f) - edg(2)*nrml(1,f)

    if (neg) then
      enr(1:3) = -enr(1:3)
    end if

    do k = 1,3

      ang = pi() * fract(k)

      if (msglvl == 4) then
        write ( *,610) a,lb,la,f,p, ang*180.0d0/ pi()
      end if

      cdang(1) = ang
      cn = cos(ang)
      if (neg) cn = -cn
      ce = sin(ang)
      nrmlc(1) = cn*nrml(1,f) + ce*enr(1)
      nrmlc(2) = cn*nrml(2,f) + ce*enr(2)
      nrmlc(3) = cn*nrml(3,f) + ce*enr(3)
      nrmlc(4) = nrmlc(1)*vcl(1,la) + nrmlc(2)*vcl(2,la) + &
        nrmlc(3)*vcl(3,la)

      call sepfac(p,cntr,nrmlc,angacc,mxcos,dtol,nvc,maxvc,vcl, &
        facep,nrml,fvl,eang,nce,cedge,cdang,aflag, ierr )

      if (ierr /= 0 .or. aflag) then
        return
      end if

    end do

  end do

160 continue

  i = pfl(2,i)
  if (i /= headp) go to 90

  600 format (' sfcshp: a,lb,la,f,p,eang(a),ang =',4i5,i6,2f9.4)
  610 format (' sfcshp: a,lb,la,f,p,ang =',5i5,f9.4)

  return
end
subroutine sfdwmf ( l, r, psi, indp, loch )
!
!******************************************************************************
!
!! SFDWMF sifts down a heap.
!
!
!  Purpose: 
!
!    Sift PSI(INDP(L)) down heap which has maximum PSI value
!    at root of heap and is maintained by pointers in INDP.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, L - element of heap to be sifted down.
!
!    Input, R - upper bound of heap.
!
!    Input, PSI(1:*) - key values for heap.
!
!    Input/output, INDP(1:R) - indices of PSI which are maintained in heap.
!
!    Input/output, LOCH(1:*) - location of indices in heap (inverse of INDP).
!
  implicit none
!
  integer r
!
  integer i
  integer indp(r)
  integer j
  integer k
  integer l
  integer loch(*)
  double precision psi(*)
  double precision t
!
  i = l
  j = 2*i
  k = indp(i)
  t = psi(k)

  do

    if (j > r) then
      exit
    end if

    if (j < r) then
      if (psi(indp(j)) < psi(indp(j+1))) then
        j = j + 1
      end if
    end if

    if (t >= psi(indp(j))) then
      exit
    end if

    indp(i) = indp(j)
    loch(indp(i)) = i
    i = j
    j = 2*i

  end do

  indp(i) = k
  loch(k) = i

  return
end
subroutine sfupmf ( r, psi, indp, loch )
!
!******************************************************************************
!
!! SFUPMF sifts up a heap.
!
!
!  Purpose: 
!
!    Sift PSI(INDP(R)) up heap which has maximum PSI value
!    at root of heap and is maintained by pointers in INDP.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, R - element of heap to be sifted up.
!
!    Input, PSI(1:*) - key values for heap.
!
!    Input/output, INDP(1:R) - indices of PSI which are maintained in heap.
!
!    Input/output, LOCH(1:*) - location of indices in heap (inverse of INDP).
!
  implicit none
!
  integer r
!
  integer i
  integer indp(r)
  integer j
  integer k
  integer loch(*)
  double precision psi(*)
  double precision t
!
  i = r
  j = int(i/2)
  k = indp(i)
  t = psi(k)

  do

    if (i <= 1) then
      exit
    end if

    if (t <= psi(indp(j))) then
      exit
    end if

    indp(i) = indp(j)
    loch(indp(i)) = i
    i = j
    j = int(i/2)

  end do

  indp(i) = k
  loch(k) = i

  return
end
subroutine shrnk2 ( nvrt, xc, yc, sdist, nshr, xs, ys, iedge, ierr )
!
!******************************************************************************
!
!! SHRNK2 shrinks a convex polygon.
!
!
!  Purpose: 
!
!    Shrink a convex polygon, with vertices given in counter clockwise
!    order and with all interior angles < PI, by distance SDIST(I)
!    for Ith edge, I = 0,...,NVRT-1.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NVRT - number of vertices on the boundary of convex polygon.
!
!    Input, XC(0:NVRT),YC(0:NVRT) - vertex coordinates in counter 
!    clockwise order; (XC(0),YC(0)) = (XC(NVRT),YC(NVRT)).
!
!    Input, SDIST(0:NVRT-1) - nonnegative shrink distances for edges.
!
!    Output, NSHR - number of vertices on boundary of shrunken polygon;
!    0 if shrunken polygon is empty else 3 <= NSHR <= NVRT.
!
!    Output, XS(0:NSHR),YS(0:NSHR) - coordinates of shrunken polygon 
!    in counter clockwise order if NSHR > 0; 
!    (XS(0),YS(0)) = (XS(NSHR),YS(NSHR)).
!
!    Output, IEDGE(0:NSHR) - indices of edges of original polygon in range
!    0 to NVRT-1 corresponding to each shrunken polygon edge.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer nvrt
!
  double precision alpha
  logical first
  integer i
  integer iedge(0:nvrt)
  integer ierr
  integer j
  integer k
  integer lr
  integer lrline
  integer nshr
  logical parall
  double precision pi
  double precision pi2
  double precision piptol
  double precision sdist(0:nvrt-1)
  double precision theta
  double precision tol
  double precision x
  double precision xc(0:nvrt)
  double precision xs(0:nvrt)
  double precision y
  double precision yc(0:nvrt)
  double precision ys(0:nvrt)
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  pi2 = 2.0d0 * pi()
  piptol = pi() + tol
  alpha = atan2(yc(1)-yc(0),xc(1)-xc(0))
  i = 1
  call xline(xc(0),yc(0),xc(1),yc(1),xc(1),yc(1),xc(2),yc(2), &
    sdist(0),sdist(1),xs(1),ys(1),parall)

  if (parall) then
    if (abs(sdist(0)-sdist(1)) <= max(tol,1.0d-5)*sdist(0)) then
      i = 2
      call xline(xc(0),yc(0),xc(1),yc(1),xc(2),yc(2),xc(3),yc(3), &
        sdist(0),sdist(2),xs(1),ys(1),parall)
    end if
  end if

  if (parall) then
    ierr = 202
    go to 90
  end if

  iedge(0) = 0
  iedge(1) = i
  i = i + 1
  j = 0
  nshr = 1
  first = .true.
!
!  First while loop processes edges subtending angle <= PI
!  with respect to first edge.
!
10 continue

  theta = atan2(yc(i+1)-yc(i),xc(i+1)-xc(i)) - alpha
  if (theta < 0.0d0) theta = theta + pi2

  if (theta > piptol) then
    call xline(xc(0),yc(0),xc(1),yc(1),xc(i),yc(i),xc(i+1),yc(i+1), &
      0.0d0,0.0d0,x,y,parall)
    if (.not. parall) go to 40
  end if

20 continue

  lr = lrline(xs(nshr),ys(nshr),xc(i),yc(i),xc(i+1),yc(i+1),sdist(i))
  if (lr < 0) go to 30
  nshr = nshr - 1
  if (nshr >= 1) go to 20

30 continue

  if (nshr < 1 .and. abs(theta - pi() ) <= tol) go to 90
  k = iedge(nshr)
  nshr = nshr + 1
  call xline(xc(k),yc(k),xc(k+1),yc(k+1),xc(i),yc(i),xc(i+1), &
    yc(i+1),sdist(k),sdist(i),xs(nshr),ys(nshr),parall)
  if (parall) go to 90
  iedge(nshr) = i
  i = i + 1
  go to 10
!
!  Second while loop processes remaining edges.
!
40 continue

  if (first) then
    first = .false.
    go to 50
  end if

  lr = lrline(xs(j),ys(j),xc(i),yc(i),xc(i+1),yc(i+1),sdist(i))
  if (lr <= 0) go to 70

50 continue

  if (nshr <= j) go to 90
  lr = lrline(xs(nshr),ys(nshr),xc(i),yc(i),xc(i+1), &
    yc(i+1),sdist(i))

  if (lr >= 0) then
    nshr = nshr - 1
    go to 50
  end if

  k = iedge(nshr)
  nshr = nshr + 1
  call xline(xc(k),yc(k),xc(k+1),yc(k+1),xc(i),yc(i),xc(i+1), &
    yc(i+1),sdist(k),sdist(i),xs(nshr),ys(nshr),parall)

  if (parall) then
    ierr = 202
    go to 90
  end if

  iedge(nshr) = i

60 continue

  lr = lrline(xs(j+1),ys(j+1),xc(i),yc(i),xc(i+1),yc(i+1), sdist(i))

  if (lr >= 0) then
    j = j + 1
    go to 60
  end if

  k = iedge(j)
  call xline(xc(k),yc(k),xc(k+1),yc(k+1),xc(i),yc(i),xc(i+1), &
    yc(i+1),sdist(k),sdist(i),xs(j),ys(j),parall)

  if (parall) then
    ierr = 202
    go to 90
  end if

  xs(nshr+1) = xs(j)
  ys(nshr+1) = ys(j)
  iedge(nshr+1) = iedge(j)

70 continue

  i = i + 1
  if (i < nvrt) go to 40

  if (j > 0) then
    do i = 0,nshr+1-j
      xs(i) = xs(i+j)
      ys(i) = ys(i+j)
      iedge(i) = iedge(i+j)
    end do
  end if

  nshr = nshr + 1 - j
  return

90 continue

  nshr = 0

  return
end
subroutine shrnk3 ( sdist, nface, vcl, hvl, nrml, fvl, eang, maxsv, maxiw, &
  maxwk, nsvc, nsface, nsvert, svcl, shvl, sfvl, iwk, wk, ierr )
!
!******************************************************************************
!
!! SHRNK3 shrinks a convex polyhedron.
!
!
!  Purpose: 
!
!    Shrink convex polyhedron by distance SDIST, i.e. find
!    intersection of half-spaces determined by planes (corresponding
!    to faces) at distance SDIST inside polyhedron. It is assumed
!    that vertices of each face are oriented counter clockwise when viewed from
!    outside polyhedron, and no 2 adjacent faces are coplanar and
!    no 2 adjacent edges are collinear.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, SDIST- shrink distance.
!
!    Input, NFACE - number of faces in convex polyhedron.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, HVL(1:NFACE) - head vertex list.
!
!    Input, NRML(1:3,1:NFACE) - unit outward normals of faces.
!
!    Input, FVL(1:5,1:*) - face vertex list; see routine DSCPH; FVL
!    may contain some unused columns (with <= 0 LOC value).
!
!    Input, EANG(1:*) - angles at edges common to 2 faces; EANG(I)
!    corresponds to FVL(*,I).
!
!    Input, MAXSV - maximum size available for SVCL and SFVL arrays; should
!    be >= twice number of edges in original or shrunken polyhedron.
!
!    Input, MAXIW - maximum size available for IWK array; should be >= max
!    of MAXSV and NFACE + 2*(max number of edges per face) + 1.
!
!    Input, MAXWK - maximum size available for WK array; should be >=
!    5*(max number of edges per face + 1).
!
!    Output, NSFACE - number of faces in shrunken polyhedron (<= NFACE); if
!    NSFACE < 4 then shrunken polyhedron is degenerate or empty.
!
!    Output, NSVC - size of SVCL array (on output).
!
!    Output, NSVERT - size of SFVL array.
!
!    Output, SVCL(1:3,1:NSVC), SHVL(1:NFACE), SFVL(1:5,1:NSVERT) - similar to
!    VCL, HVL, FVL but for shrunken polyhedron (NSFACE >= 4);
!    faces of original polyhedron having corresponding faces in
!    shrunken polyhedron are numbered 1 to NSFACE in SHVL (in same
!    increasing order as HVL); indices of faces of original
!    polyhedron not having corresponding faces in shrunken polyh
!    are put in SHVL(NSFACE+1:NFACE).
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxiw
  integer maxsv
  integer maxwk
  integer nface
!
  integer a
  double precision ar
  integer b
  double precision br
  double precision cmax
  double precision cxy
  double precision cyz
  double precision dr
  double precision eang(*)
  integer, parameter :: edgv = 5
  logical empty
  logical equal
!  integer eshr
  integer eshr,f
  integer, parameter :: facn = 2
  integer fvl(5,*)
  integer g
  integer hvl(nface)
  integer i
  integer iedge
  integer ierr
  integer ind
  integer iwk(maxiw)
  integer j
  integer k
  integer lb
  integer li
  double precision leng
  integer, parameter :: loc = 1
  logical match
  integer n
  double precision nrml(3,nface)
  integer nsface
  integer nshr
  integer nsvc
  integer nsvert
  double precision pi
  integer, parameter :: pred = 4
  double precision r21
  double precision r22
  double precision r31
  double precision r32
  double precision rhs
  double precision sdist
  integer sfvl(5,maxsv)
  integer shvl(nface)
  integer, parameter :: succ = 3
  double precision svcl(3,maxsv)
  double precision sxy
  double precision syz
  double precision tol
  double precision vcl(3,*)
  double precision wk(maxwk)
  integer xc
  integer xs
  integer yc
  double precision yr
  integer ys
  double precision zr
!
!  For each face, rotate normal vector to (0,0,1). Rotation matrix is
!    [ CXY     -SXY     0   ]
!    [ CYZ*SXY CYZ*CXY -SYZ ]
!    [ SYZ*SXY SYZ*CXY  CYZ ]
!  Rotate face vertices, then apply 2D shrink algorithm,
!  followed by intersection with nonadjacent face-planes.
!  K is index for SFVL and SVCL.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  k = 0
  nsface = 0

  if (nface > maxiw) then
    ierr = 6
    return
  end if

  iedge = nface + 1
  eshr = 1

  do f = 1,nface

    if (abs(nrml(1,f)) <= tol) then
      leng = nrml(2,f)
      cxy = 1.0d0
      sxy = 0.0d0
    else
      leng = sqrt(nrml(1,f)**2 + nrml(2,f)**2)
      cxy = nrml(2,f)/leng
      sxy = nrml(1,f)/leng
    end if

    cyz = nrml(3,f)
    syz = leng
    r21 = cyz*sxy
    r22 = cyz*cxy
    r31 = nrml(1,f)
    r32 = nrml(2,f)
    iwk(1:nface) = 1
    iwk(f) = 0
    i = hvl(f)
    n = 0

20  continue

    n = n + 1
    i = fvl(succ,i)
    if (i /= hvl(f)) go to 20

    if (nface + n + n + 1 > maxiw) then
      ierr = 6
      return
    else if (5*n + 4 > maxwk) then
      ierr = 7
      return
    end if

    ind = iedge + n + 1
    xc = eshr + n
    yc = xc + n + 1
    xs = yc + n + 1
    ys = xs + n + 1
    j = 0

30  continue

    li = fvl(loc,i)
    wk(xc+j) = cxy*vcl(1,li) - sxy*vcl(2,li)
    wk(yc+j) = r21*vcl(1,li) + r22*vcl(2,li) - syz*vcl(3,li)

    if (eang(i) >= pi() - tol) then
      wk(eshr+j) = 0.0d0
    else
      wk(eshr+j) = sdist/tan(eang(i)*0.5d0)
    end if

    g = fvl(facn,fvl(edgv,i))
    iwk(ind+j) = g
    iwk(g) = 0
    j = j + 1
    i = fvl(succ,i)

    if (i /= hvl(f)) go to 30

    wk(xc+n) = wk(xc)
    wk(yc+n) = wk(yc)
    rhs = nrml(1,f)*vcl(1,li) + nrml(2,f)*vcl(2,li) + &
      nrml(3,f)*vcl(3,li)
    zr = rhs - sdist

    call shrnk2(n,wk(xc),wk(yc),wk(eshr),nshr,wk(xs),wk(ys), &
      iwk(iedge), ierr )
    if (ierr /= 0) return

    shvl(f) = 0

    if (nshr == 0) then
      cycle
    end if

    if (k + nshr > maxsv) then
      ierr = 13
      return
    end if

    nsface = nsface + 1
    i = k + 1
    shvl(f) = i

    do j = 0,nshr-1
      k = k + 1
      svcl(1,k) = wk(xs+j)
      svcl(2,k) = wk(ys+j)
      svcl(3,k) = zr
      sfvl(loc,k) = k
      sfvl(facn,k) = f
      sfvl(succ,k) = k + 1
      sfvl(pred,k) = k - 1
      sfvl(edgv,k) = -iwk(ind+iwk(iedge+j))
    end do

    sfvl(succ,k) = i
    sfvl(pred,i) = k
!
!  Intersect shrunken polygon with other half-spaces.
!
    do j = 1,nface

      if (iwk(j) /= 1) then
        cycle
      end if

      ar = cxy*nrml(1,j) - sxy*nrml(2,j)
      br = r21*nrml(1,j) + r22*nrml(2,j) - syz*nrml(3,j)
      dr = r31*nrml(1,j) + r32*nrml(2,j) + cyz*nrml(3,j)
      li = fvl(loc,hvl(j))
      rhs = nrml(1,j)*vcl(1,li) + nrml(2,j)*vcl(2,li) + &
        nrml(3,j)*vcl(3,li)
      dr = rhs - sdist - dr*zr
!
!  Compute intersection of half-plane AR*X + BR*Y <= DR
!  with convex polygon in plane Z = ZR.
!
      call xpghpl(ar,br,dr,j,maxsv,k,shvl(f),svcl,sfvl,empty, ierr )
      if (ierr /= 0) return

      if (empty) then
        nsface = nsface - 1
        go to 70
      end if

    end do

    i = shvl(f)

60  continue

    j = sfvl(loc,i)
    yr = svcl(2,j)
    svcl(2,j) = r22*yr - sxy*svcl(1,j) + r32*zr
    svcl(1,j) = cxy*svcl(1,j) + r21*yr + r31*zr
    svcl(3,j) = cyz*zr - syz*yr
    i = sfvl(succ,i)
    if (i /= shvl(f)) go to 60

70  continue

  end do

  nsvert = k
  nsvc = k
  if (nsface < 4) return
!
!  Determine EDGV values by searching appropriate faces.
!
  match = .true.

  do f = 1,nface

    i = shvl(f)

    if (i == 0) then
      cycle
    end if

80  continue

    a = sfvl(edgv,i)
    if (a > 0) go to 100
    g = -a
    a = shvl(g)

90  continue

    if (sfvl(edgv,a) /= -f) then
    a = sfvl(succ,a)
    if (a /= shvl(g)) go to 90

    match = .false.
    go to 100
    end if
    sfvl(edgv,i) = a
    sfvl(edgv,a) = i
    shvl(g) = sfvl(succ,a)

100 continue

    i = sfvl(succ,i)
    if (i /= shvl(f)) go to 80

  end do

  if (.not. match) then
    ierr = 314
    return
  end if
!
!  Give vertices with same coordinates the same LOC value.
!  Unused (deleted) entries of SFVL have a negative LOC value.
!
  if (nsvert > maxiw) then
    ierr = 6
    return
  end if

  do i = 1,nsvert
    iwk(i) = 1
    if (sfvl(loc,i) < 0) iwk(i) = 0
  end do

  do i = 1,nsvert

    if (iwk(i) == 0) then
      cycle
    end if

    iwk(i) = 0
    li = sfvl(loc,i)
    b = i

130 continue

    b = sfvl(succ,sfvl(edgv,b))

    if (b == i) then
      cycle
    end if

    lb = sfvl(loc,b)
    iwk(b) = 0

    equal = .true.

    do j = 1,3
      cmax = max(abs(svcl(j,li)),abs(svcl(j,lb)))
      if (abs(svcl(j,li) - svcl(j,lb)) > tol*cmax .and. cmax > tol) then
        equal = .false.
        exit
      end if
    end do

    if (equal) then
      sfvl(loc,b) = li
    else
      sfvl(loc,b) = li
      match = .false.
    end if

    go to 130

  end do
!
!  Keep only referenced vertices of SVCL, update LOC field of SFVL.
!
  iwk(1:nsvc) = 0

  do i = 1,nsvert
    li = sfvl(loc,i)
    if (li > 0) iwk(li) = 1
  end do

  j = 0

  do i = 1,nsvc
    if (iwk(i) == 1) then
      j = j + 1
      iwk(i) = j
      svcl(1,j) = svcl(1,i)
      svcl(2,j) = svcl(2,i)
      svcl(3,j) = svcl(3,i)
    end if
  end do

  nsvc = j
  do  i = 1,nsvert
    li = sfvl(loc,i)
    if (li > 0) sfvl(loc,i) = iwk(li)
  end do
!
!  Update SHVL and SFVL(FACN,*) to get consecutive face indices.
!
  if (nsface >= nface) return
  i = 0
  j = 0

  do f = 1,nface

    if (shvl(f) <= 0) then

      i = i + 1
      iwk(i) = f

    else

      j = j + 1

      if (j < f) then

        a = shvl(f)
        shvl(j) = a

210     continue

        sfvl(facn,a) = j
        a = sfvl(succ,a)
        if (a /= shvl(j)) go to 210

      end if

    end if

  end do

  do i = 1,nface-nsface
    shvl(nsface+i) = iwk(i)
  end do

  return
end
subroutine smpxda ( k, i, npt, sizht, nbf, nfc, maxbf, maxfc, vcl, vm, bf, &
  fc, ht, nsmplx, hdavbf, hdavfc, bflag, front, back, top, ifac, ind, indf, &
  center, mat, ierr )
!
!******************************************************************************
!
!! SMPXDA deletes simplices whose circumhypersphere contains a vertex.
!
!
!  Purpose: 
!
!    Delete simplices whose circumhypersphere contains vertex
!    I in interior, then add simplices involving I by joining I to
!    faces in stack, where I is index of new vertex added to triang.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of triangulation.
!
!    Input, I - local index of next vertex inserted in triangulation;
!    it is assumed I is largest index so far.
!
!    Input, NPT - number of K-D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NBF - number of positions used in BF array.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:K,1:*) - vertex coordinate list.
!
!    Input, VM(1:NPT) - indices of vertices of VCL being triangulated.
!
!    Input/output, BF(1:K,1:MAXBF) -  array of boundary face records; 
!    see DTRISK.
!
!    Input/output, FC(1:K+4,1:MAXFC) - array of face records; see 
!    routine DTRISK.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NSMPLX - number of simplices in triangulation.
!
!    Input/output, HDAVBF - head pointer to available BF records.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Input, BFLAG - .TRUE. iff vertex I is on boundary of triangulation.
!
!    Input/output, FRONT, BACK - indices of front and back of queue of interior
!    faces that may form a new simplex with vertex I.
!
!    Input/output, TOP - index of top of stack of faces that form a new simplex
!    with vertex I.
!
!    Output, IFAC - index of face containing vertex I.
!
!    Workspace, IND(1:K) - local vertex indices or pivot indices.
!
!    Workspace, INDF(1:K+1) - indices in VCL.
!
!    Workspace, CENTER(1:K) - hypersphere center or hyperplane normal.
!
!    Workspace, MAT(1:K,1:K) - matrix used for solving system of linear
!    equations.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer maxbf
  integer maxfc
  integer sizht
!
  integer a
  integer b
  integer back
  integer bf(k,maxbf)
  logical bflag
  integer bfp
  double precision center(k)
  integer d
  integer fc(k+4,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrck
  integer i
  integer ierr
  integer ifac
  integer ii
  integer in
  integer ind(k)
  integer indf(k+1)
  integer j
  integer kp1
  integer kp2
  integer kp4
  integer l
  double precision mat(k,k)
  integer, parameter :: msglvl = 0
  integer nbf
  integer nfc
  integer npt
  integer nsmplx
  integer opsidk
  integer pos
  integer ptr
  double precision rsq
  integer top
  integer topn
  double precision vcl(k,*)
  integer vi
  integer vm(npt)
!
  ierr = 0
  kp1 = k + 1
  kp2 = k + 2
  kp4 = k + 4
  vi = vm(i)
  topn = 0

10 continue

  if (front == 0) go to 60

  pos = front
  front = fc(kp4,pos)

  if (fc(2,pos) == 0) then
    fc(1,pos) = -hdavfc
    hdavfc = pos
    go to 10
  end if

  indf(1:k) = vm(fc(1:k,pos))

  if (fc(kp1,pos) == i) then
    d = fc(kp2,pos)
  else
    d = fc(kp1,pos)
  end if

  indf(kp1) = vm(d)
  call ccsphk(k,.true.,indf,vcl,vcl(1,vi),center,rsq,in,mat,ind)

  if (in < 1) then
    fc(kp4,pos) = top
    top = pos
    go to 10
  end if
!
!  Delete simplex and process other faces of simplex.
!
  nsmplx = nsmplx - 1

  if (msglvl == 4) then
    write ( *,600) (fc(ii,pos),ii=1,k),d
  end if

  do j = 1,k

    ind(1:k) = fc(1:k,pos)
    a = ind(j)
    ind(j) = d
    ptr = htsrck(k,ind,npt,sizht,fc,ht)
    if (ptr <= 0) go to 160

    if (fc(kp4,ptr) >= 0) then

      call htdelk(k,ptr,npt,sizht,fc,ht)
      fc(2,ptr) = 0

    else if (fc(kp2,ptr) < 0) then

      if (bflag) then

        indf(1:k) = vm(fc(1:k,ptr))

        if (opsidk(k,indf,vcl,.true.,vcl(1,vi),vcl(1,vi),mat,center) == 0) then
          bfp = -fc(kp2,ptr)
          bf(1,bfp) = -hdavbf
          hdavbf = bfp
          call htdelk(k,ptr,npt,sizht,fc,ht)
          fc(1,ptr) = -hdavfc
          hdavfc = ptr
          cycle
        end if

      end if

      fc(kp1,ptr) = i
      fc(kp4,ptr) = top
      top = ptr

    else

      if (fc(kp1,ptr) == a) then
        fc(kp1,ptr) = i
      else
        fc(kp2,ptr) = i
      end if

      if (front == 0) then
        front = ptr
      else
        fc(kp4,back) = ptr
      end if

      back = ptr
      fc(kp4,back) = 0

    end if

  end do

  call htdelk(k,pos,npt,sizht,fc,ht)
  fc(1,pos) = -hdavfc
  hdavfc = pos
  go to 10
!
!  For faces in stack TOP, form new simplices with vertex I.
!  Then set BF fields for new boundary faces if BFLAG = .TRUE.
!
60 continue

  pos = top
  top = fc(kp4,pos)
  fc(kp4,pos) = -1
  nsmplx = nsmplx + 1

  if (msglvl == 4) then
    write ( *,610) (fc(ii,pos),ii=1,k),i
  end if

  do j = 1,k

    ind(1:k) = fc(1:k,pos)
    a = ind(j)
    ind(j) = i
    ptr = htsrck(k,ind,npt,sizht,fc,ht)

    if (ptr > 0) then

      fc(kp2,ptr) = a

    else

      call availk(k,hdavfc,nfc,maxfc,fc,ptr,ierr)
      if (ierr /= 0) return
      call htinsk(k,ptr,ind,a,0,npt,sizht,fc,ht)

      if (bflag) then
        fc(kp4,ptr) = topn
        topn = ptr
      end if

    end if

  end do

  if (top /= 0) go to 60

  ifac = ptr
  if (.not. bflag) return

90 continue

  if (topn == 0) go to 110

  pos = topn
  topn = fc(kp4,pos)

  if (fc(kp2,pos) /= 0) then
    fc(kp4,pos) = -1
    go to 90
  end if

  if (hdavbf /= 0) then

    bfp = hdavbf
    hdavbf = -bf(1,hdavbf)

  else

    if (nbf >= maxbf) then
      ierr = 23
      return
    else
      nbf = nbf + 1
      bfp = nbf
    end if

  end if

  fc(kp2,pos) = -bfp
  fc(kp4,pos) = top
  top = pos
  bf(1:k,bfp) = 0
  go to 90

110 continue

  pos = top
  top = fc(kp4,pos)
  fc(kp4,pos) = -1
  d = fc(kp1,pos)
  bfp = -fc(kp2,pos)

  do j = 1,k

    if (bf(j,bfp) /= 0) then
      cycle
    end if

    a = fc(j,pos)
    b = d

120 continue

    ind(1:k) = fc(1:k,pos)
    ind(j) = b
    ptr = htsrck(k,ind,npt,sizht,fc,ht)
    if (ptr <= 0) go to 160
    l = fc(kp2,ptr)

    if (l > 0) then

      if (l == a) then
        a = b
        b = fc(kp1,ptr)
      else
        a = b
        b = fc(kp2,ptr)
      end if

      go to 120

    end if

    bf(j,bfp) = ptr
    ii = 1

140 continue

    if (fc(ii,ptr) /= b) then
      ii = ii + 1
      go to 140
    end if

    bf(ii,-l) = pos

  end do

  if (top /= 0) go to 110
  return

160 continue

  ierr = 400

  600 format (1x,'deleted simplex:',9i7)
  610 format (1x,'added simplex:',9i7)

  return
end
subroutine smpxls ( k, nfc, vm, fc, ns, smplx )
!
!******************************************************************************
!
!! SMPXLS constructs a list of simplices from the FC array.
!
!
!  Purpose: 
!
!    Construct list of simplices from FC array. Global vertex
!    indices from VM are produced. The vertex indices for each
!    simplex are sorted in increasing order.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of triangulation.
!
!    Input, NFC - number of positions used in FC array.
!
!    Input, VM(1:*) - indices of vertices of VCL that are triangulated.
!
!    Input, FC(1:K+4,1:NFC) - array of face records; see routine DTRISK.
!
!    Output, NS - number of simplices.
!
!    Output, SMPLX(1:K+1,1:NS) - contains global simplex indices; it is
!    assumed there is enough space.
!
  implicit none
!
  integer k
  integer nfc
!
  integer fc(k+4,nfc)
  integer i
  integer j
  integer kp1
  integer kp2
  integer l
  integer ns
  integer smplx(k+1,*)
  integer vm(*)
!
  kp1 = k + 1
  kp2 = k + 2
  ns = 0

  do i = 1,nfc

    if (fc(1,i) <= 0) then
      cycle
    end if

    do l = kp1,kp2
      if (fc(l,i) > fc(k,i)) then
        ns = ns + 1
        smplx(1:k,ns) = vm(fc(1:k,i))
        smplx(kp1,ns) = vm(fc(l,i))
      end if
    end do

  end do

  do i = 1,ns
    call orderk(kp1,smplx(1,i))
  end do

  return
end
subroutine spdec2 ( angspc, angtol, nvc, npolg, nvert, nhole, nhola, maxvc,  &
  maxhv, maxpv, maxiw, maxwk, holv, vcl, regnum, hvl, pvl, iang, iwk, &
  wk, ierror )
!
!*******************************************************************************
!
!! SPDEC2 decomposes a polygonal region with holes into simple polygons.
!
!
!  Discussion:
!
!    Decompose general polygonal region with interfaces and
!    holes into simple polygons using vertex coordinate list,
!    head vertex list, and polygon vertex list data structures.
!
!  Modified:
!
!    12 July 1999
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, double precision ANGSPC, the angle spacing parameter in radians
!    used in controlling vertices to be considered as an endpoint of a
!    separator.
!
!    Input, double precision ANGTOL, the angle tolerance parameter in radians
!    used in accepting separator(s).
!
!    Input/output, integer NVC, the number of vertex coordinates or positions
!    used in VCL array.
!
!    Input/output, integer NPOLG, the number of polygonal subregions or
!    positions used in HVL array.
!
!    Input/output, integer NVERT, the number of polygon vertices or positions
!    used in PVL array.
!
!    Input, integer NHOLE, the number of holes and hole interfaces.
!
!    Input, integer NHOLA, the number of 'attached' holes; these holes are
!    attached to the outer boundary of a subregion through vertices
!    or cut interfaces and have their edges in consecutive order on the
!    boundary.
!
!    Input, integer MAXVC, the maximum size available for VCL array, should
!    be >= number of vertex coordinates required for decomposition.
!
!    Input, integer MAXHV, the maximum size available for HVL, REGNUM arrays,
!    should be >= number of polygons required for decomposition.
!
!    Input, integer MAXPV, the maximum size available for PVL, IANG arrays;
!    should be >= number of polygon vertices required for decomposition.
!
!    Input, integer MAXIW, the maximum size available for IWK array; should be
!    about 3 times maximum number of vertices in any polygon.
!
!    Input, integer MAXWK, the maximum size available for WK array; should be
!    about 5 times maximum number of vertices in any polygon.
!
!    Input, integer HOLV(1:NHOLE*2+NHOLA), the indices in PVL of bottom or top
!    vertex of holes; first (next) NHOLE entries are for top (bottom)
!    vertices of holes and hole interfaces, with top (bottom)
!    vertices sorted in decreasing (increasing) lexicographic
!    (y,x) order of coord; last NHOLA entries are for attached
!    holes; if bottom vertex of attached hole is a simple
!    vertex of boundary curve containing the hole then entry
!    contains index of bottom vertex otherwise entry contains
!    index of top vertex (which is simple).
!
!    Input/output, double precision VCL(1:2,1:NVC), the vertex coordinate list.
!
!    Input/output, integer REGNUM(1:NPOLG), the region numbers.
!
!    Input/output, integer HVL(1:NPOLG), the head vertex list.
!
!    Input/output, integer PVL(1:4,1:NVERT), double precision IANG(1:NVERT),
!    the polygon vertex list and interior angles; see routine DSPGDC for more
!    details.  Note: The data structures should be as output from routines
!    DSMCPR or DSPGDC.
!
!    Workspace, integer IWK(1:MAXIW).
!
!    Workspace, double precision WK(1:MAXWK).
!
!    Output, integer IERROR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxhv
  integer maxiw
  integer maxpv
  integer maxvc
  integer maxwk
!
  double precision angspc
  double precision angtol
  logical ci
  logical cj
  integer, parameter :: edgv = 4
  integer holv(*)
  integer hvl(maxhv)
  integer i
  double precision iang(maxpv)
  integer ierror
  integer iwk(maxiw)
  integer j
  integer nhola
  integer nhole
  integer npolg
  integer nvc
  integer nvert
  integer p
  double precision pi
  double precision piptol
  integer, parameter :: polg = 2
  integer pvl(4,maxpv)
  integer regnum(maxhv)
  integer, parameter :: succ = 3
  double precision tol
  double precision vcl(2,maxvc)
  integer vr
  integer w1
  integer w2
  double precision wk(maxwk)
!
  tol = 100.0D+00 * epsilon ( tol )
!
!  For each simple hole, find cut edge from top vertex of hole to
!  a point on the outer boundary above top vertex, and update
!  VCL, HVL, PVL, IANG.
!
  piptol = pi() + tol

  do i = 1, nhole

    call jnhole ( holv(i), angspc, angtol, nvc, nvert, maxvc, maxpv, maxiw, &
      maxwk, vcl, hvl, pvl, iang, iwk, wk, ierror )

    if ( ierror /= 0 ) then
      write ( *, * ) ' '
      write ( *, * ) 'SPDEC2 - Fatal error!'
      write ( *, * ) '  JNHOLE returned an error condition.'
      return
    end if

  end do
!
!  Resolve remaining vertices in HOLV array if they are reflex
!  vertices. These vertices may no longer be reflex if they are the
!  endpoint of a cut edge from the top vertex of another hole or
!  of a previous separator.
!
  do i = nhole+1, nhole+nhole+nhola

    vr = holv(i)

    if ( iang(vr) > piptol ) then

      call resvrt ( vr, angspc, angtol, nvc, nvert, maxvc, maxpv, maxiw, &
        maxwk, vcl, pvl, iang, w1, w2, iwk, wk, ierror )

      if ( ierror /= 0 ) then
        return
      end if

      call insed2 ( vr, w1, npolg, nvert, maxhv, maxpv, vcl, regnum, hvl, &
        pvl, iang, ierror )

      if ( ierror /= 0 ) then
        return
      end if

      if ( w2 > 0 ) then
        call insed2 ( vr, w2, npolg, nvert, maxhv, maxpv, &
          vcl, regnum, hvl, pvl, iang, ierror )
        if ( ierror /= 0 ) then
          return
        end if
      end if

    end if

  end do

  if ( nhola == 0 ) then
    return
  end if
!
!  Check that polygons are simple. If polygon is simply-connected and
!  not simple then find a simple reflex vertex in polygon to resolve.
!
  p = 1

30 continue

  if ( p > npolg ) then
    return
  end if

  i = hvl(p)

  do

    if ( pvl(polg,pvl(edgv,i)) == p ) then
      go to 50
    end if

    i = pvl(succ,i)

    if ( i == hvl(p) ) then
      exit
    end if

  end do

  p = p + 1
  go to 30

50 continue

  ci = .true.

  do

    j = pvl(succ,i)
    cj = ( pvl(polg,pvl(edgv,j)) == p )

    if ( .not. ci .and. .not. cj .and. iang(j) > piptol ) then
      exit
    end if

    i = j
    ci = cj

  end do

  vr = j
  call resvrt ( vr, angspc, angtol, nvc, nvert, maxvc, maxpv, maxiw, &
    maxwk, vcl, pvl, iang, w1, w2, iwk, wk, ierror )

  if ( ierror /= 0 ) then
    return
  end if

  call insed2 ( vr, w1, npolg, nvert, maxhv, maxpv, vcl, regnum, hvl, &
    pvl, iang, ierror )

  if ( ierror /= 0 ) then
    return
  end if

  if ( w2 > 0 ) then

    call insed2 ( vr, w2, npolg, nvert, maxhv, maxpv, &
      vcl, regnum, hvl, pvl, iang, ierror )

    if ( ierror /= 0 ) then
      return
    end if

  end if

  go to 30

end
subroutine spdech ( aspc2d, atol2d, nfhol, nvc, nface, nvert, npf, maxvc, &
  maxfp, maxfv, maxpf, maxiw, maxwk, vcl, facep, factyp, nrml, fvl, eang, &
  hfl, pfl, headp, x, y, locfv, link, edgv, iwk, wk, ierr )
!
!******************************************************************************
!
!! SPDECH decomposes a face of a polyhedral region.
!
!
!  Purpose: 
!
!    Decompose face of polyhedral region into simple polygons,
!    where face contains holes and outer and inner boundary
!    polygons of face are simple.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, ASPC2D - angle spacing parameter in radians used in controlling
!    vertices to be considered as an endpoint of a separator.
!
!    Input, ATOL2D - angle tolerance parameter in radians used in accepting
!    separator to resolve a hole on a face.
!
!    Input, NFHOL - number of holes on face (>= 1).
!
!    Input/output, NVC - number of vertex coordinates.
!
!    Input/output, NFACE - number of faces or positions used in FACEP array.
!
!    Input/output, NVERT - number of positions used in FVL, EANG arrays.
!
!    Input/output, NPF - number of positions used in PFL array.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXFP - maximum size available for FACEP, FACTYP, NRML arrays.
!
!    Input, MAXFV - maximum size available for FVL, EANG arrays.
!
!    Input, MAXPF - maximum size available for PFL array.
!
!    Input, MAXIW - maximum size available for IWK array; should be about
!    3*(NV + 8*NFHOL) where NV is number of vertices on face.
!
!    Input, MAXWK - maximum size available for WK array; should be about
!    5*(NV + 8*NFHOL).
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input/output, FACEP(1:3,1:NFACE) - face pointer list.
!
!    Input/output, FACTYP(1:NFACE) - face types.
!
!    Input/output, NRML(1:3,1:NFACE) - unit normal vectors for faces; outward
!    normal corresponds to counter clockwise traversal of 
!    face from polyhedron with index |FACEP(2,F)|.
!
!    Input/output, FVL(1:6,1:NVERT) - face vertex list.
!
!    Input/output, EANG(1:NVERT) - edge angles.
!
!    Input, HFL(1:*) - head pointer to face indices in PFL for each polyhedron.
!
!    Input/output, PFL(1:2,1:NPF) - list of signed face indices for each
!    polyhedron.
!
!    Input, HEADP(0:NFHOL) - first entry is head pointer (index of FVL)
!    of outer polygon of face, other entries are head pointers
!    of hole polygons.
!
!    Workspace, HEADP(0:NFHOL) - input values are overwritten.
!
!    Workspace, X(1:*), Y(1:*), LOCFV(1:*), LINK(1:*), EDGV(1:*) - used for 2D
!    representation of multiply-connected polygon; assumed
!    size of each array is at least NV + 8*NFHOL where NV is
!    the number of vertices in multiply-connected polygon.
!
!    Workspace, IWK(1:MAXIW).
!
!    Workspace, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfp
  integer maxfv
  integer maxiw
  integer maxpf
  integer maxvc
  integer maxwk
  integer nfhol
!
  integer a
  double precision aspc2d
  double precision atol2d
  integer ccw
  double precision cxy
  double precision cyz
  double precision eang(maxfv)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer edgv(*)
  integer f
  integer facep(3,maxfp)
  integer, parameter :: facn = 2
  integer factyp(maxfp)
  integer fvl(6,maxfv)
  integer h
  integer headp(0:nfhol)
  integer hfl(*)
  integer i
  integer ierr
  integer ilft
  integer imax
  integer imin
  integer inc
  integer irgt
  integer iwk(maxiw)
  integer j
  integer k
  integer l
  double precision leng
  integer link(*)
  integer, parameter :: loc = 1
  integer locfv(*)
  integer maxn
  integer nface
  integer nfh
  integer niw
  integer npf
  double precision nrml(3,maxfp)
  integer nv
  integer nvc
  integer nvert
  integer nvrt
  integer pfl(2,maxpf)
  double precision pi
  integer, parameter :: pred = 4
  double precision r21
  double precision r22
  double precision r31
  double precision r32
  integer, parameter :: succ = 3
  double precision sxy
  double precision syz
  double precision tol
  integer v
  double precision vcl(3,maxvc)
  integer w
  double precision wk(maxwk)
  integer wrem
  double precision x(*)
  double precision xh
  double precision xint
  double precision xlft
  double precision xmax
  double precision xmin
  double precision xrgt
  double precision y(*)
  integer yc
  double precision yh
  double precision ymax
  double precision ymin
  double precision zr
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  if (nfhol <= 0) return

  if (nfhol > maxiw) then
    ierr = 6
    return
  end if
!
!  Rotate normal vector of face to (0,0,1). Rotation matrix applied
!  to face vertices is
!    [ CXY     -SXY     0   ]
!    [ CYZ*SXY CYZ*CXY -SYZ ]
!    [ SYZ*SXY SYZ*CXY  CYZ ]
!
  l = fvl(loc,headp(0))
  f = fvl(facn,headp(0))

  if (facep(2,f) > 0) then
    ccw = succ
  else
    ccw = pred
  end if

  if (abs(nrml(1,f)) <= tol) then
    leng = nrml(2,f)
    cxy = 1.0d0
    sxy = 0.0d0
  else
    leng = sqrt(nrml(1,f)**2 + nrml(2,f)**2)
    cxy = nrml(2,f)/leng
    sxy = nrml(1,f)/leng
  end if

  cyz = nrml(3,f)
  syz = leng
  r21 = cyz*sxy
  r22 = cyz*cxy
  r31 = nrml(1,f)
  r32 = nrml(2,f)
  zr = r31*vcl(1,l) + r32*vcl(2,l) + cyz*vcl(3,l)
  k = 1

  do i = 0,nfhol

    h = headp(i)
    j = h
    headp(i) = k

10  continue

    l = fvl(loc,j)
    x(k) = cxy*vcl(1,l) - sxy*vcl(2,l)
    y(k) = r21*vcl(1,l) + r22*vcl(2,l) - syz*vcl(3,l)
    locfv(k) = j
    link(k) = k + 1
    edgv(k) = 0
    k = k + 1
    j = fvl(ccw,j)
    if (j /= h) go to 10

    link(k-1) = headp(i)

  end do

  nv = k - 1
!
!  Determine top and bottom vertices of holes on face, and sort top
!  vertices in decreasing (y,x) order using linear insertion sort.
!
  do i = 1,nfhol

    if (i == nfhol) then
      k = nv
    else
      k = headp(i+1) - 1
    end if

    h = headp(i)
    imin = h
    imax = h
    xmin = x(h)
    ymin = y(h)
    xmax = xmin
    ymax = ymin

    do j = h+1,k

      if (y(j) < ymin .or. y(j) == ymin .and. x(j) < xmin) then
        imin = j
        xmin = x(j)
        ymin = y(j)
      else if (y(j) > ymax .or. y(j) == ymax .and. x(j) > xmax) then
        imax = j
        xmax = x(j)
        ymax = y(j)
      end if

    end do

    headp(i) = imax
    iwk(i) = imin

  end do

  do i = 2,nfhol

    h = headp(i)
    xmax = x(h)
    ymax = y(h)
    imin = iwk(i)
    j = i

50  continue

    l = headp(j-1)

    if (ymax > y(l) .or. ymax == y(l) .and. xmax > x(l)) then
      headp(j) = l
      iwk(j) = iwk(j-1)
      j = j - 1
      if (j > 1) go to 50
    end if

    headp(j) = h
    iwk(j) = imin

  end do
!
!  For each hole, find cut edge from top vertex of hole to a point
!  on outer boundary above top vertex, and update data structures.
!  For each top vertex, find 'closest' vertices on outer boundary
!  which are to left and right of top vertex and on horizontal line
!  through top vertex.  The two closest vertices must be on edges
!  which intersect horizontal line and are partially above line.
!
  inc = int ( pi() / aspc2d)
  xlft = 0.0d0
  xrgt = 0.0d0

  do i = 1,nfhol

    ilft = 0
    irgt = 0
    h = headp(i)
    xh = x(h)
    yh = y(h)
    j = headp(0)

70  continue

    k = link(j)

    if (y(k) > yh .and. y(j) <= yh) then

      if (y(j) == yh) then
        xint = x(j)
      else
        xint = (yh - y(j))*(x(k) - x(j))/(y(k) - y(j)) + x(j)
      end if

      if (xint > xh) then
        if (xint < xrgt .or. irgt == 0) then
          irgt = j
          xrgt = xint
        end if
      end if

    else if (y(j) > yh .and. y(k) <= yh) then

      if (y(k) == yh) then
        xint = x(k)
      else
        xint = (yh - y(j))*(x(k) - x(j))/(y(k) - y(j)) + x(j)
      end if

      if (xint < xh) then
        if (xint > xlft .or. ilft == 0) then
          ilft = j
          xlft = xint
        end if
      end if

    end if

    j = k
    if (j /= headp(0)) go to 70

    if (ilft == 0 .or. irgt == 0) then
      ierr = 344
      return
    end if

    nvrt = 2
    j = irgt

80  continue

    j = link(j)
    nvrt = nvrt + 1
    if (j /= ilft) go to 80

    maxn = nvrt + inc
    niw = nvrt + nfhol

    if (niw > maxiw) then
      ierr = 6
      return
    else if (maxn + maxn > maxwk) then
      ierr = 7
      return
    end if

    yc = maxn + 1
    wrem = yc + maxn
    wk(1) = xrgt
    wk(maxn+1) = yh
    iwk(nfhol+1) = irgt
    wk(nvrt) = xlft
    wk(maxn+nvrt) = yh
    iwk(niw) = link(ilft)
    j = irgt

    do k = 2,nvrt-1
      j = link(j)
      wk(k) = x(j)
      wk(maxn+k) = y(j)
      iwk(nfhol+k) = j
    end do

    call resvrh(xh,yh,aspc2d,atol2d,nvrt,maxn,maxiw-niw, &
      maxwk-wrem+1,x,y,link,wk,wk(yc),iwk(nfhol+1),v,x(nv+1), &
      y(nv+1),iwk(niw+1),wk(wrem), ierr )

    if (ierr /= 0) return

    if (v < 0) then

      v = -v
      nv = nv + 1
      link(nv) = link(v)
      link(v) = nv

      if (nvc >= maxvc) then
         ierr = 14
         return
      end if

      vcl(1,nvc+1) = cxy*x(nv) + r21*y(nv) + r31*zr
      vcl(2,nvc+1) = r22*y(nv) - sxy*x(nv) + r32*zr
      vcl(3,nvc+1) = cyz*zr - syz*y(nv)
      a = locfv(v)
      if (ccw == pred) a = fvl(pred,a)
      call insvr3(a,nvc,nvert,maxfv,vcl,fvl,eang,ierr)
      if (ierr /= 0) return
      locfv(nv) = fvl(succ,a)
      w = edgv(v)

      if (w == 0) then

        edgv(nv) = 0
        v = nv

      else

        nv = nv + 1
        x(nv) = x(nv-1)
        y(nv) = y(nv-1)
        link(nv) = link(w)
        link(w) = nv

        if (locfv(nv-1) == nvert) then
          locfv(nv) = nvert - 1
        else
          locfv(nv) = nvert
        end if

        edgv(v) = nv
        edgv(nv-1) = w
        edgv(w) = nv - 1
        edgv(nv) = v
        v = nv - 1

      end if

    else

      do j = 1,i-1
        if (iwk(j) == v) then
          iwk(j) = -iwk(j)
          exit
        end if
      end do

    end if

    nv = nv + 2
    x(nv-1) = x(h)
    y(nv-1) = y(h)
    link(nv-1) = link(h)
    link(h) = nv
    x(nv) = x(v)
    y(nv) = y(v)
    link(nv) = link(v)
    link(v) = nv - 1
    edgv(nv-1) = edgv(h)
    edgv(h) = v
    edgv(nv) = edgv(v)
    edgv(v) = h
    if (edgv(nv-1) > 0) edgv(edgv(nv-1)) = nv - 1
    if (edgv(nv) > 0) edgv(edgv(nv)) = nv
    call inseh3(locfv(h),locfv(v),nvert,maxfv,facep,fvl,eang,ierr)
    if (ierr /= 0) return

    if (ccw == succ) then
      locfv(nv-1) = locfv(h)
      locfv(nv) = locfv(v)
      locfv(h) = nvert - 1
      locfv(v) = nvert
    else
      locfv(nv-1) = nvert -1
      locfv(nv) = nvert
    end if

  end do
!
!  For each hole, find separator from bottom vertex of hole if
!  bottom vertex is not the endpoint of a cut edge (IWK(I) > 0).
!  First sort holes needing separator into increasing (y,x) order.
!  As for cut edges, 2nd endpt of separator is below bottom vertex.
!
  nfh = 0

  do i = 1,nfhol
    if (iwk(i) > 0) then
      nfh = nfh + 1
      iwk(nfh) = iwk(i)
    end if
  end do

  do i = 2,nfh

    h = iwk(i)
    xmin = x(h)
    ymin = y(h)
    j = i

140 continue

    l = iwk(j-1)

    if (ymin < y(l) .or. ymin == y(l) .and. xmin < x(l)) then
      iwk(j) = l
      j = j - 1
      if (j > 1) go to 140
    end if

    iwk(j) = h

  end do

  do i = 1,nfh

    ilft = 0
    irgt = 0
    h = iwk(i)
    xh = x(h)
    yh = y(h)
    j = link(h)

160 continue

    k = link(j)

    if (y(k) >= yh .and. y(j) < yh) then

      if (y(k) == yh) then
        xint = x(k)
      else
        xint = (yh - y(j))*(x(k) - x(j))/(y(k) - y(j)) + x(j)
      end if

      if (xint > xh) then
        if (xint < xrgt .or. irgt == 0) then
          irgt = j
          xrgt = xint
        end if
      end if

    else if (y(j) >= yh .and. y(k) < yh) then

      if (y(j) == yh) then
        xint = x(j)
      else
        xint = (yh - y(j))*(x(k) - x(j))/(y(k) - y(j)) + x(j)
      end if

      if (xint < xh) then
        if (xint > xlft .or. ilft == 0) then
          ilft = j
          xlft = xint
        end if
      end if

    end if

    j = k
    if (j /= h) go to 160

    if (ilft == 0 .or. irgt == 0) then
      ierr = 344
      return
    end if

    nvrt = 2
    j = ilft

170 continue

    j = link(j)
    nvrt = nvrt + 1
    if (j /= irgt) go to 170

    maxn = nvrt + inc
    niw = nvrt + nfh

    if (niw > maxiw) then
      ierr = 6
      return
    else if (maxn + maxn > maxwk) then
      ierr = 7
      return
    end if

    yc = maxn + 1
    wrem = yc + maxn
    wk(1) = xlft
    wk(maxn+1) = yh
    iwk(nfh+1) = ilft
    wk(nvrt) = xrgt
    wk(maxn+nvrt) = yh
    iwk(niw) = link(irgt)
    j = ilft

    do k = 2,nvrt-1
      j = link(j)
      wk(k) = x(j)
      wk(maxn+k) = y(j)
      iwk(nfh+k) = j
    end do

    call resvrh(xh,yh,aspc2d,atol2d,nvrt,maxn,maxiw-niw, &
      maxwk-wrem+1,x,y,link,wk,wk(yc),iwk(nfh+1),v,x(nv+1), &
      y(nv+1),iwk(niw+1),wk(wrem), ierr )

    if (ierr /= 0) return

    if (v < 0) then

      v = -v
      nv = nv + 1
      link(nv) = link(v)
      link(v) = nv

      if (nvc >= maxvc) then
        ierr = 14
        return
      end if

      vcl(1,nvc+1) = cxy*x(nv) + r21*y(nv) + r31*zr
      vcl(2,nvc+1) = r22*y(nv) - sxy*x(nv) + r32*zr
      vcl(3,nvc+1) = cyz*zr - syz*y(nv)
      a = locfv(v)
      if (ccw == pred) a = fvl(pred,a)
      call insvr3(a,nvc,nvert,maxfv,vcl,fvl,eang,ierr)
      if (ierr /= 0) return
      locfv(nv) = fvl(succ,a)
      w = edgv(v)

      if (w == 0) then

        edgv(nv) = 0
        v = nv

      else

        nv = nv + 1
        x(nv) = x(nv-1)
        y(nv) = y(nv-1)
        link(nv) = link(w)
        link(w) = nv

        if (locfv(nv-1) == nvert) then
          locfv(nv) = nvert - 1
        else
          locfv(nv) = nvert
        end if

        edgv(v) = nv
        edgv(nv-1) = w
        edgv(w) = nv - 1
        edgv(nv) = v
        v = nv - 1

      end if

    end if

    nv = nv + 2
    x(nv-1) = x(h)
    y(nv-1) = y(h)
    link(nv-1) = link(h)
    link(h) = nv
    x(nv) = x(v)
    y(nv) = y(v)
    link(nv) = link(v)
    link(v) = nv - 1
    edgv(nv-1) = edgv(h)
    edgv(h) = v
    edgv(nv) = edgv(v)
    edgv(v) = h
    if (edgv(nv-1) > 0) edgv(edgv(nv-1)) = nv - 1
    if (edgv(nv) > 0) edgv(edgv(nv)) = nv
    a = locfv(h)
    call insed3(a,locfv(v),nface,nvert,npf,maxfp,maxfv,maxpf,facep, &
      factyp,nrml,fvl,eang,hfl,pfl,ierr)
    if (ierr /= 0) return

    if (ccw == succ) then
      locfv(nv-1) = locfv(h)
      locfv(nv) = locfv(v)
      locfv(h) = nvert - 1
      locfv(v) = nvert
    else
      locfv(nv-1) = nvert -1
      locfv(nv) = nvert
    end if

  end do

  return
end
subroutine stats ( n, x, a, h, nf, xmin, xmax, mean, stdv, freq )
!
!******************************************************************************
!
!! STATS computes statistical measurements for data.
!
!
!  Purpose: 
!
!    Compute the following statistical measurements for data
!    X(1:N): minimum, maximum, mean, standard deviation, relative
!    frequency in intervals  X(I) < A+H, A+H <= X(I) < A+2*H, ...,
!    A+(NF-1)*H <= X(I) < A+NF*H, and A+NF*H <= X(I)
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, N - number of data points (measurements).
!
!    Input, X(1:N) - array of N double precision data points.
!
!    Input, A, H - used for determining frequency intervals as above.
!
!    Input, NF - number of frequency intervals - 1.
!
!    Output, XMIN, XMAX, MEAN, STDV - minimum, maximum, mean, standard 
!    deviation.
!
!    Output, FREQ(0:NF) - relative frequencies (fraction of 1) as above.
!
  implicit none
!
  integer n
  integer nf
!
  double precision a
  double precision freq(0:nf)
  double precision h
  integer i
  integer k
  double precision mean
  double precision stdv
  double precision sum2
  double precision xmax
  double precision xmin
  double precision x(n)
!
  freq(0:nf) = 0.0d0

  xmin = x(1)
  xmax = x(1)
  sum2 = 0.0d0

  do i = 1,n
    sum2 = sum2 + x(i)
    xmin = min(xmin,x(i))
    xmax = max(xmax,x(i))
    k = int((x(i) - a)/h)
    k = max(min(k,nf), 0)
    freq(k) = freq(k) + 1.0d0
  end do

  mean = sum2/dble(n)

  sum2 = 0.0d0
  do i = 1,n
    sum2 = sum2 + (x(i) - mean)**2
  end do

  stdv = sqrt(sum2/dble(n-1))

  freq(0:nf) = freq(0:nf)/dble(n)

  return
end
subroutine swapdg ( k, pos, d, i, sneg, spos, szero, alpha, npt, sizht, nbf, &
  nfc, maxbf, maxfc, vcl, vm, bf, fc, ht, nsmplx, hdavbf, hdavfc, front, &
  back, ifac, bfi, ind, indf, mv, loc, zpn, ierr )
!
!******************************************************************************
!
!! SWAPDG applies swaps in a KD triangulation.
!
!
!  Purpose: 
!
!    Apply simultaneous degenerate local transformations or
!    swaps in K-D triangulation, where swap is applied to facets
!    of dimension <= K-2.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of triangulation.
!
!    Input, POS - position of face in FC for which swap is to be applied.
!
!    Input, D, I - local indices of vertices forming simplices with face
!    FC(*,POS); it is assumed I is largest index so far.
!
!    Input, SNEG, SPOS, SZERO - number of negative, positive, zero vertices
!    among FC(1:K,POS) and D,I; each is >= 2.
!
!    Input, ALPHA(1:K) - -1.0, 1.0, or 0.0 to indicate type of vertex
!    FC(J,POS), J = 1, ..., K.
!
!    Input, NPT - number of K-D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NBF - number of positions used in BF array.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:K,1:*) - vertex coordinate list.
!
!    Input, VM(1:NPT) - indices of vertices of VCL being triangulated.
!
!    Input/output, BF(1:K,1:MAXBF) -  array of boundary face records; 
!    see DTRISK.
!
!    Input/output, FC(1:K+4,1:MAXFC) - array of face records; see 
!    routine DTRISK.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NSMPLX - number of simplices in triangulation.
!
!    Input/output, HDAVBF - head pointer to available BF records.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Input/output, FRONT,BACK - indices of front and back of queue of interior
!    faces for which hypersphere test is applied.
!
!    Input/output, IFAC - index of last face for which sphere test applied (if
!    swap then index of new interior face), or 0.
!
!    Input/output, BFI - index of FC of a boundary face containing vertex I 
!    if a degenerate local transformation is applied, or 0.
!
!    Workspace, IND(1:K) - local vertex indices.
!
!    Workspace, INDF(1:K) - face indices.
!
!    Workspace, MV(1:K+1) - missing local vertex indices.
!
!    Workspace, LOC(1:K) - local vertex indices of facet, indices from 1 to K.
!
!    Workspace, ZPN(1:K) - permutation of 1 to K.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer maxbf
  integer maxfc
  integer npt
  integer sizht
!
  integer a
  double precision alpha(k)
  integer b
  integer back
  integer bf(k,maxbf)
  integer bfi
  integer bfn
  integer bfp
  integer bot(2)
  integer d
  integer dind
  integer e
  integer f
  integer fc(k+4,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrck
  integer i
  integer ierr
  integer ifac
  integer ii
  integer ind(k)
  integer indf(k)
  integer j
  integer jj
  integer jn
  integer jp
  integer jz
  integer kbf(2)
  integer kif(2)
  integer km1
  integer kp1
  integer kp2
  integer kp4
  integer kv
  integer l
  integer loc(k)
  integer m
  integer, parameter :: msglvl = 0
  integer mv(k+1)
  integer nbf
  integer nbr
  integer nfc
  integer nsmplx
  integer nstrt
  integer pos
  integer pstrt
  integer ptr
  integer ptr1
  integer ptrn
  integer sn1
  integer sneg
  integer snp
  integer snpm1
  integer spos
  integer spz
  integer spzm1
  integer spzm2
  integer szero
  integer top
  integer topd
  integer topn
  double precision vcl(k,*)
  integer vm(npt)
  integer zpn(k)
!
  ierr = 0
  km1 = k - 1
  kp1 = k + 1
  kp2 = k + 2
  kp4 = k + 4
  sn1 = sneg + 1
  snp = sneg + spos
  snpm1 = snp - 1
  spz = spos + szero
  spzm1 = spz - 1
  spzm2 = spz - 2
  jz = 1
  pstrt = szero + 1
  jp = pstrt
  nstrt = pstrt + spos
  jn = nstrt

  do j = 1,k

    if (alpha(j) == 0.0d0) then
      zpn(jz) = j
      jz = jz + 1
    else if (alpha(j) == 1.0d0) then
      zpn(jp) = j
      jp = jp + 1
    else
      zpn(jn) = j
      jn = jn + 1
    end if

  end do
!
!  Check whether negative degenerate facets are in triangulation,
!  and put their face indices in INDF.
!
  a = fc(zpn(1),pos)

  if (sneg >= 3) then

    jj = 2

    do jn = nstrt,k

      ind(1:k) = fc(1:k,pos)
      ind(zpn(1)) = d
      ind(zpn(jn)) = i
      ptr = htsrck(k,ind,npt,sizht,fc,ht)

      if (ptr <= 0) then
        if (msglvl == 4) then
          write ( *,620) sneg,spos, 'dg: missing facet'
        end if
        return
      else if (fc(kp1,ptr) /= a .and. fc(kp2,ptr) /= a) then
        if (msglvl == 4) then
          write ( *,620) sneg,spos,'dg: other vertex not common'
        end if
        return
      end if

      jj = jj + 1
      indf(jj) = ptr

    end do

  end if

  ind(1:k) = fc(1:k,pos)

  ind(zpn(1)) = i
  ptr = htsrck(k,ind,npt,sizht,fc,ht)
  if (ptr <= 0) go to 550

  indf(1) = ptr
  ind(1:k) = fc(1:k,pos)
  ind(zpn(1)) = d
  ptr = htsrck(k,ind,npt,sizht,fc,ht)
  if (ptr <= 0) go to 550

  indf(2) = ptr
!
!  Find all faces involving the SNEG negative facets.  None of these
!  faces are in queue.  These faces are kept in 2 lists, first for
!  those containing first facet, second for others.
!
  bot(1) = indf(1)
  bot(2) = indf(2)
  fc(kp4,bot(1)) = 0
  fc(kp4,bot(2)) = 0

  do j = 1, sneg

    l = min(j,2)
    kbf(l) = 0
    kif(l) = 0
    ptr = indf(j)

    if (j >= 3) then
      fc(kp4,bot(l)) = ptr
      fc(kp4,ptr) = 0
      bot(l) = ptr
    end if

    jj = 0
    jz = 2

    do ii = 1,k
      if (fc(ii,ptr) == fc(zpn(jz),pos)) then
        if (jz < szero) jz = jz + 1
      else
        jj = jj + 1
        loc(jj) = fc(ii,ptr)
      end if
    end do

70  continue

    if (fc(kp2,ptr) > 0) then
      kif(l) = kif(l) + 1
      kv = kp2
    else
      kbf(l) = kbf(l) + 1
      kv = kp1
    end if

    jn = 1
    jz = snpm1

    do ii = 1,k
      if (fc(ii,ptr) == loc(jn)) then
        if (jn < snpm1) jn = jn + 1
      else
        jz = jz + 1
        loc(jz) = ii
      end if
    end do

    do m = kp1,kv

      e = fc(m,ptr)

      do jz = snp,k

        ind(1:k) = fc(1:k,ptr)
        ind(loc(jz)) = e
        nbr = htsrck(k,ind,npt,sizht,fc,ht)
        if (nbr <= 0) go to 550

        if (fc(kp4,nbr) == -1) then
          fc(kp4,bot(l)) = nbr
          fc(kp4,nbr) = 0
          bot(l) = nbr
        end if

      end do

    end do

    ptr = fc(kp4,ptr)
    if (ptr /= 0) go to 70

    if ( j /= 1) then
      if ( kbf(2) /= kbf(1) .or. kif(2) /= kif(1)) then
        if (msglvl == 4) then
          write ( *,620) sneg,spos, 'dg: diff number of faces'
        end if
        go to 530
      end if
    end if

  end do
!
!  Check if faces containing each facet match. I = FC(K,PTR).
!
  ptr = indf(1)

130 continue

  loc(sneg) = k

  if (sneg > 2) then

    jj = 1
    jn = nstrt

    do ii = 1,km1
      if (fc(ii,ptr) == fc(zpn(jn),pos)) then
        jj = jj + 1
        loc(jj) = ii
        jn = jn + 1
        if (jn > k) go to 150
      end if
    end do

  end if

150 continue

  do jn = 2,sneg
    ind(1:k) = fc(1:k,ptr)
    ind(loc(jn)) = d
    nbr = htsrck(k,ind,npt,sizht,fc,ht)
    if (nbr <= 0) then
      if (msglvl == 4) then
        write ( *,620) sneg,spos, 'dg: unmatched face'
      end if
      go to 530
    end if
  end do

  ptr = fc(kp4,ptr)
  if (ptr /= 0) go to 130
!
!  Apply local transformations. Process old and new faces of
!  degenerate local transformation, then other old interior,
!  other new interior, other boundary faces (for each simplex).
!
  m = (kif(1) + kif(1) + kbf(1))/szero
  nsmplx = nsmplx + m*(spos - sneg)

  if (msglvl == 4) then
    write ( *,630) sneg,spos,'dg: #swaps =',m
  end if

  top = indf(2)

180 continue

  ptr = top
  top = fc(kp4,ptr)

  if (fc(kp2,ptr) > 0) then
    call htdelk(k,ptr,npt,sizht,fc,ht)
    fc(1,ptr) = -hdavfc
    hdavfc = ptr
  end if

  if (top /= 0) go to 180
!
!  TOP, TOPN are used for stacks of old, new boundary faces.
!
  top = 0
  topn = 0
  ptr = indf(1)

190 continue

  jz = 1
  jp = szero
  jn = spz
  j = pstrt

  if (nstrt <= k) then
    jj = nstrt
  else
    jj = pstrt
  end if

  loc(k) = k

  do ii = 1,km1

    if (fc(ii,ptr) == fc(zpn(j),pos)) then
      loc(jp) = ii
      jp = jp + 1
      j = j + 1
      if (j == nstrt) j = j - 1
    else if (fc(ii,ptr) == fc(zpn(jj),pos)) then
      loc(jn) = ii
      jn = jn + 1
      if (jj < k) jj = jj + 1
    else
      loc(jz) = ii
      jz = jz + 1
    end if
  end do

  e = fc(kp1,ptr)
  f = fc(kp2,ptr)

  do jp = szero,spzm1

    call availk(k,hdavfc,nfc,maxfc,fc,ptr1,ierr)
    if (ierr /= 0) return

    if (f < 0) then

      if (hdavbf /= 0) then

        bfp = hdavbf
        hdavbf = -bf(1,hdavbf)

      else

        if (nbf >= maxbf) then
          ierr = 23
          return
        else
          nbf = nbf + 1
          bfp = nbf
        end if

      end if

      f = -bfp
      bfi = ptr1

    end if

    ind(1:k) = fc(1:k,ptr)
    ind(loc(jp)) = d
    call htinsk(k,ptr1,ind,e,f,npt,sizht,fc,ht)

    if (f < 0) then
      fc(kp4,ptr1) = topn
      topn = ptr1
    end if

  end do

  if (f > 0) then
    kv = kp2
  else
    kv = kp1
  end if

  do m = kp1,kv

    if (m == kp2) e = f

    do jn = spz,k

      ind(1:k) = fc(1:k,ptr)
      ind(loc(jn)) = e
      call htsdlk(k,ind,npt,sizht,fc,ht,ptr1)

      if (ptr1 <= 0) then
        if (jn == spz) go to 340
        go to 550
      end if

      if (ptr1 == pos) then
        cycle
      end if

      if (fc(kp4,ptr1) >= 0) then
        fc(2,ptr1) = 0
      else
        fc(1,ptr1) = -hdavfc
        hdavfc = ptr1
      end if

    end do

    do jn = spz,km1
      do jj = jn+1,k

        ind(1:k) = fc(1:k,ptr)
        ind(loc(jn)) = e
        ind(loc(jj)) = d
        call htsdlk(k,ind,npt,sizht,fc,ht,ptr1)
        if (ptr1 <= 0) go to 550

        if (ptr1 == pos) then
          cycle
        end if

        if (fc(kp4,ptr1) >= 0) then
          fc(2,ptr1) = 0
        else
          fc(1,ptr1) = -hdavfc
          hdavfc = ptr1
        end if

      end do
    end do

    do jp = szero,spzm2
      a = fc(loc(jp),ptr)
      do jj = jp+1,spzm1
        call availk(k,hdavfc,nfc,maxfc,fc,ptr1,ierr)
        if (ierr /= 0) return
        ind(1:k) = fc(1:k,ptr)
        ind(loc(jp)) = e
        b = ind(loc(jj))
        ind(loc(jj)) = d
        call htinsk(k,ptr1,ind,a,b,npt,sizht,fc,ht)
      end do
    end do

    ifac = ptr1

    do jp = szero,spzm1

      a = fc(loc(jp),ptr)
      b = d

      do jn = spzm1,k
        ind(1:k) = fc(1:k,ptr)
        ind(loc(jp)) = e
        if (jn > spzm1) then
          b = ind(loc(jn))
          ind(loc(jn)) = d
        end if
        call updatk(k,ind,a,b,i,npt,sizht,front,back,fc,ht, ierr )
        if (ierr /= 0) return
      end do

    end do

340 continue

  end do

  ptr1 = ptr
  ptr = fc(kp4,ptr)

  if (f > 0) then
    call htdelk(k,ptr1,npt,sizht,fc,ht)
    fc(1,ptr1) = -hdavfc
    hdavfc = ptr1
  else
    fc(kp4,ptr1) = top
    top = ptr1
  end if

  if (ptr /= 0) go to 190
  if (top == 0) go to 520
!
!  Set BF fields of new boundary faces.
!  Then delete old boundary faces in TOPD stack.
!
  mv(sneg) = i
  mv(sneg-1) = d

  if (sneg >= 3) then

    j = 0
    do jn = nstrt,k
     j = j + 1
     mv(j) = fc(zpn(jn),pos)
    end do

    j = sneg - 1

360 continue

    if (mv(j-1) > d) then
      mv(j) = mv(j-1)
      j = j - 1
      if (j > 1) go to 360
    end if

    dind = j
    mv(j) = d
  else
    dind = 1
  end if

  j = sneg
  do jp = pstrt,nstrt-1
    j = j + 1
    mv(j) = fc(zpn(jp),pos)
  end do

  topd = 0

380 continue

  ptr = top
  top = fc(kp4,ptr)
  fc(kp4,ptr) = topd
  topd = ptr
  indf(dind) = ptr
  loc(sneg) = k

  if (sneg > 2) then

    jj = 1
    jn = nstrt

    do ii = 1,km1

      if (fc(ii,ptr) == fc(zpn(jn),pos)) then
        jj = jj + 1
        loc(jj) = ii
        jn = jn + 1
        if (jn > k) go to 400
      end if

    end do

  end if

400 continue

  j = 1

  do jn = 2,sneg

    ind(1:k) = fc(1:k,ptr)
    ind(loc(jn)) = d
    nbr = htsrck(k,ind,npt,sizht,fc,ht)
    if (nbr <= 0) go to 550

    if (j == dind) j = j + 1
    indf(j) = nbr
    j = j + 1
    fc(kp4,nbr) = topd
    topd = nbr

  end do

  do j = snp,sn1,-1
    ptr1 = topn
    topn = fc(kp4,ptr1)
    fc(kp4,ptr1) = -1
    indf(j) = ptr1
  end do

  do j = sn1,snp

    a = mv(j)
    ptr1 = indf(j)
    bfp = -fc(kp2,ptr1)
    jn = 1
    jp = sn1
    if (jp == j) jp = jp + 1
    jz = snp

    do jj = 1,k

      b = fc(jj,ptr1)

      if (b == mv(jp)) then

        bf(jj,bfp) = indf(jp)
        jp = jp + 1
        if (jp == j) jp = jp + 1
        if (jp > snp) jp = snp

      else if (b == mv(jn)) then

        ptrn = indf(jn)
        ii = 1

440     continue

        if (fc(ii,ptrn) /= a) then
          ii = ii + 1
          go to 440
        end if

        nbr = bf(ii,-fc(kp2,ptrn))
        bf(jj,bfp) = nbr
        bfn = -fc(kp2,nbr)
        ii = 1

450     continue

        if (bf(ii,bfn) /= ptrn) then
          ii = ii + 1
          go to 450
        end if

        bf(ii,bfn) = ptr1
        if (jn < sneg) jn = jn + 1

      else

        jz = jz + 1

        if (j == sn1) then

          ii = 1

460       continue

          if (fc(ii,ptr) /= b) then
            ii = ii + 1
            go to 460
          end if

          nbr = bf(ii,-fc(kp2,ptr))
          bfn = -fc(kp2,nbr)
          ii = 1

470       continue

          if (bf(ii,bfn) /= ptr) then
            ii = ii + 1
            go to 470
          end if

          f = fc(ii,nbr)
          mv(jz) = f

        else

          f = mv(jz)

        end if

        ind(1:k) = fc(1:k,ptr1)
        ind(jj) = f
        ptrn = htsrck(k,ind,npt,sizht,fc,ht)
        if (ptrn <= 0) go to 550
        bf(jj,bfp) = ptrn

      end if

    end do

  end do

  if (top /= 0) go to 380

510 continue

  ptr = topd
  topd = fc(kp4,ptr)
  bfp = -fc(kp2,ptr)
  call htdelk(k,ptr,npt,sizht,fc,ht)
  fc(1,ptr) = -hdavfc
  hdavfc = ptr
  bf(1,bfp) = -hdavbf
  hdavbf = bfp
  if (topd /= 0) go to 510

520 continue

  fc(1,pos) = -hdavfc
  hdavfc = pos
  return

530 continue

  fc(kp4,bot(1)) = indf(2)
  top = indf(1)

540 continue

  ptr = top
  top = fc(kp4,ptr)
  fc(kp4,ptr) = -1
  if (top /= 0) go to 540
  return

550 continue

  ierr = 400

  620 format (4x,'no swap ',i2,' -',i2,3x,a)
  630 format (4x,'swap ',i2,' -',i2,3x,a,i7)

  return
end
subroutine swapec ( i, top, maxst, btri, bedg, vcl, til, tnbr, stack, ierr )
!
!******************************************************************************
!
!! SWAPEC swaps diagonal edges in a 2D triangulation
!
!
!  Purpose: 
!
!    Swap diagonal edges in 2D triangulation based on empty
!    circumcircle criterion until all triangles are Delaunay, given
!    that I is index of new vertex added to triangulation.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, I - index in VCL of new vertex.
!
!    Input/output, TOP - index of top of stack, >= 0.
!
!    Input, MAXST - maximum size available for STACK array.
!
!    Input/output, BTRI, BEDG - if positive, these are triangle and edge 
!    index of a boundary edge whose updated indices must be recorded.
!
!    Input, VCL(1:2,1:*) - coordinates of 2D vertices.
!
!    Input/output, TIL(1:3,1:*) - triangle incidence list.
!
!    Input/output, TNBR(1:3,1:*) - triangle neighbor list; negative values are
!    used for links of counter clockwise linked list of boundary edges;
!    LINK = -(3*I + J-1) where I, J = triangle, edge index.
!
!    Input, STACK(1:TOP) - index of initial triangles (involving vertex I)
!    put in stack; the edges opposite I should be in interior.
!
!    Workspace, STACK(TOP+1:MAXST) - used as stack.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxst
!
  integer a
  integer b
  integer bedg
  integer btri
  integer c
  integer diaedg
  integer e
  integer ee
  integer em1
  integer ep1
  integer f
  integer fm1
  integer fp1
  integer i
  integer ierr
  integer l
  integer, parameter :: msglvl = 0
  integer r
  integer s
  integer stack(maxst)
  integer swap
  integer t
  integer til(3,*)
  integer tnbr(3,*)
  integer top
  integer tt
  integer u
  double precision x
  double precision y
  double precision vcl(2,*)
!
!  Determine whether triangles in stack are Delaunay, and swap
!  diagonal edge of convex quadrilateral if not.
!
  ierr = 0
  x = vcl(1,i)
  y = vcl(2,i)

10 continue

  if (top <= 0) return
  t = stack(top)
  top = top - 1

  if (til(1,t) == i) then
    e = 2
    b = til(3,t)
  else if (til(2,t) == i) then
    e = 3
    b = til(1,t)
  else
    e = 1
    b = til(2,t)
  end if

  a = til(e,t)
  u = tnbr(e,t)

  if (tnbr(1,u) == t) then
    f = 1
    c = til(3,u)
  else if (tnbr(2,u) == t) then
    f = 2
    c = til(1,u)
  else
    f = 3
    c = til(2,u)
  end if

  swap = diaedg(x,y,vcl(1,a),vcl(2,a),vcl(1,c),vcl(2,c),vcl(1,b), &
    vcl(2,b))

  if (swap == 1) then

    em1 = e - 1
    if (em1 == 0) em1 = 3
    ep1 = e + 1
    if (ep1 == 4) ep1 = 1
    fm1 = f - 1
    if (fm1 == 0) fm1 = 3
    fp1 = f + 1
    if (fp1 == 4) fp1 = 1
    til(ep1,t) = c
    til(fp1,u) = i
    r = tnbr(ep1,t)
    s = tnbr(fp1,u)
    tnbr(ep1,t) = u
    tnbr(fp1,u) = t
    tnbr(e,t) = s
    tnbr(f,u) = r

    if (tnbr(fm1,u) > 0) then
      top = top + 1
      stack(top) = u
    end if

    if (s > 0) then

      if (tnbr(1,s) == u) then
        tnbr(1,s) = t
      else if (tnbr(2,s) == u) then
        tnbr(2,s) = t
      else
        tnbr(3,s) = t
      end if

      top = top + 1

      if (top > maxst) then
        ierr = 8
        return
      end if

      stack(top) = t

    else

      if (u == btri .and. fp1 == bedg) then
        btri = t
        bedg = e
      end if

      l = -(3*t + e-1)
      tt = t
      ee = em1

20    continue

      if (tnbr(ee,tt) > 0) then

        tt = tnbr(ee,tt)

        if (til(1,tt) == a) then
          ee = 3
        else if (til(2,tt) == a) then
          ee = 1
        else
          ee = 2
        end if

        go to 20

      end if

      tnbr(ee,tt) = l

    end if

    if (r > 0) then

      if (tnbr(1,r) == t) then
        tnbr(1,r) = u
      else if (tnbr(2,r) == t) then
        tnbr(2,r) = u
      else
        tnbr(3,r) = u
      end if

    else

      if (t == btri .and. ep1 == bedg) then
        btri = u
        bedg = f
      end if

      l = -(3*u + f-1)
      tt = u
      ee = fm1

30    continue

      if (tnbr(ee,tt) > 0) then

        tt = tnbr(ee,tt)

        if (til(1,tt) == b) then
          ee = 3
        else if (til(2,tt) == b) then
          ee = 1
        else
          ee = 2
        end if

        go to 30

      end if

      tnbr(ee,tt) = l

    end if

    if (msglvl == 4) then
      write ( *,600) 2,vcl(1,a),vcl(2,a), &
           vcl(1,b),vcl(2,b),x,y,vcl(1,c),vcl(2,c)
    end if

  end if

  go to 10

  600 format (1x,i7,4f15.7/8x,4f15.7)

  return
end
subroutine swapes ( bndcon, i, npt, sizht, nfc, maxfc, vcl, vm, bf, fc, ht, &
  ntetra, hdavfc, front, back, ifac, ierr )
!
!******************************************************************************
!
!! SWAPES swaps faces in a 3D triangulation.
!
!
!  Purpose: 
!
!    Swap faces (apply local transformations) in 3D triangulation 
!    based on empty circumsphere criterion until (nearly)
!    all faces are locally optimal, where I is index of new vertex
!    added to triangulation or 0 if initial triangulation given.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, BNDCON - .TRUE. iff boundary faces are constrained (i.e. not
!    swapped by local transformations).
!
!    Input, I - local index of next vertex inserted in triangulation, or 0;
!    if positive, it is assumed I is largest index so far.
!
!    Input, NPT - number of 3D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:NPT) - indices of vertices of VCL being triangulated.
!
!    Input/output, BF(1:3,1:*) -  array of boundary face records; see DTRIS3.
!
!    Input/output, FC(1:7,1:MAXFC) - array of face records; see routine DTRIS3.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NTETRA - number of tetrahedra in triangulation.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Input/output, FRONT, BACK - indices of front and back of queue of interior
!    faces for which sphere test is applied.
!
!    Output, IFAC - index of last face for which sphere test applied, or 0.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfc
  integer npt
  integer sizht
!
  integer a
  integer aa
  double precision alpha(4)
  integer b
  integer back
  integer bf(3,*)
  integer bfx(2)
  logical bndcon
  integer c
  double precision center(3)
  integer d
  integer dd
  logical degen
  integer e
  integer f
  integer fc(7,maxfc)
  integer front
  integer g
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrc
  integer i
  integer ierr
  integer ifac
  integer in
  integer ind
  integer ind1
  integer ind2
  integer indx(2)
  integer j
  integer k
  integer kneg
  integer kzero
  integer, parameter :: msglvl = 0
  integer nbr(2,2)
  integer nfc
  integer ntetra
  double precision radsq
  double precision vcl(3,*)
  integer va
  integer vb
  integer vc
  integer vd
  integer ve
  integer vm(npt)
!
  ierr = 0
  ifac = 0

10 continue

  if (front == 0) return
  ind = front
  front = fc(7,ind)

  if (fc(2,ind) == 0) then

    if (ind == nfc) then
      nfc = nfc - 1
    else
      fc(1,ind) = -hdavfc
      hdavfc = ind
    end if

    go to 10

  end if

  ifac = ind
  fc(7,ind) = -1
  a = fc(1,ind)
  b = fc(2,ind)
  c = fc(3,ind)
  d = fc(4,ind)
  e = fc(5,ind)
  va = vm(a)
  vb = vm(b)
  vc = vm(c)
  vd = vm(d)
  ve = vm(e)

  if (msglvl == 4) then
    write ( *,600) ind,a,b,c,d,e
  end if

  call ccsph(.true.,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd), &
    vcl(1,ve),center,radsq,in)

  if (in == 2) then
    ierr = 301
    return
  end if

  if (in >= 1) then

    call baryth(vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd), &
      vcl(1,ve),alpha,degen)

    if (degen) then
      ierr = 301
      return
    else if (alpha(4) > 0.0d0) then
      ierr = 309
      return
    end if

    kneg = 1
    kzero = 0

    do j = 1,3
      if (alpha(j) < 0.0d0) then
        kneg = kneg + 1
      else if (alpha(j) == 0.0d0) then
        kzero = kzero + 1
      end if
    end do
!
!  Swap 2 tetrahedra for 3.
!
    if (kneg == 1 .and. kzero == 0) then

      call updatf(a,b,d,c,e,i,npt,sizht,front,back,fc,ht, ierr )
      call updatf(a,c,d,b,e,i,npt,sizht,front,back,fc,ht, ierr )
      call updatf(b,c,d,a,e,i,npt,sizht,front,back,fc,ht, ierr )
      call updatf(a,b,e,c,d,i,npt,sizht,front,back,fc,ht, ierr )
      call updatf(a,c,e,b,d,i,npt,sizht,front,back,fc,ht, ierr )
      call updatf(b,c,e,a,d,i,npt,sizht,front,back,fc,ht, ierr )

      if (ierr /= 0) return

      call htdel(ind,npt,sizht,fc,ht)
      call htins(ind,a,d,e,b,c,npt,sizht,fc,ht)
      call availf(hdavfc,nfc,maxfc,fc,ind,ierr)

      if (ierr /= 0) return

      call htins(ind,b,d,e,a,c,npt,sizht,fc,ht)
      call availf(hdavfc,nfc,maxfc,fc,ind,ierr)

      if (ierr /= 0) return

      call htins(ind,c,d,e,a,b,npt,sizht,fc,ht)
      ntetra = ntetra + 1

      if (msglvl == 4) then
        write ( *,610)
      end if
!
!  Swap 3 tetrahedra for 2 if possible. Relabel so edge
!  AB would be deleted. Swap if ABDE is in current triangulation.
!
    else if (kneg == 2 .and. kzero == 0) then

      if (alpha(1) < 0.0d0) then
        call i_swap ( a, c )
      else if (alpha(2) < 0.0d0) then
        call i_swap ( b, c )
      end if

      ind1 = htsrc(a,b,d,npt,sizht,fc,ht)
      if (ind1 <= 0) go to 50

      if (fc(4,ind1) == e .or. fc(5,ind1) == e) then

        call updatf(a,c,d,b,e,i,npt,sizht,front,back,fc,ht, ierr )
        call updatf(a,c,e,b,d,i,npt,sizht,front,back,fc,ht, ierr )
        call updatf(a,d,e,b,c,i,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,c,d,a,e,i,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,c,e,a,d,i,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,d,e,a,c,i,npt,sizht,front,back,fc,ht, ierr )
        if (ierr /= 0) return
        call htdel(ind,npt,sizht,fc,ht)
        call htins(ind,c,d,e,a,b,npt,sizht,fc,ht)
        call htdel(ind1,npt,sizht,fc,ht)

        if (fc(7,ind1) >= 0) then
          fc(2,ind1) = 0
        else
          if (ind1 == nfc) then
            nfc = nfc - 1
          else
            fc(1,ind1) = -hdavfc
            hdavfc = ind1
          end if
        end if

        ind1 = htsrc(a,b,e,npt,sizht,fc,ht)
        if (ind1 <= 0) go to 50
        call htdel(ind1,npt,sizht,fc,ht)

        if (fc(7,ind1) >= 0) then

          fc(2,ind1) = 0

        else

          if (ind1 == nfc) then
            nfc = nfc - 1
          else
            fc(1,ind1) = -hdavfc
            hdavfc = ind1
          end if

        end if

        ntetra = ntetra - 1

        if (msglvl == 4) then
          write ( *,620) c,d,e
        end if

      else

        if (msglvl == 4) then
          write ( *,630) a,b,d,e
        end if

      end if
!
!  Coplanar faces: swap 2 tetrahedra for 2 if boundary faces
!  (and BNDCON is .FALSE.), else do pair of 2 for 2 swaps if
!  possible.  Relabel vertices so that DE intersects AB.
!  Also swap if necessary to make A < B and D < E.
!
    else if (kneg == 1 .and. kzero == 1) then

      if (alpha(1) == 0.0d0) then

        call i_swap ( a, c )

      else if (alpha(2) == 0.0d0) then

        call i_swap ( b, c )

      end if

      if (a > b) then
        call i_swap ( a, b )
      end if

      if (d > e) then
        call i_swap ( d, e )
      end if

      ind1 = htsrc(a,b,d,npt,sizht,fc,ht)
      ind2 = htsrc(a,b,e,npt,sizht,fc,ht)
      if (ind1 <= 0 .or. ind2 <= 0) go to 50

      if (fc(4,ind1) == c) then
        f = fc(5,ind1)
      else
        f = fc(4,ind1)
      end if

      if (fc(4,ind2) == c) then
        g = fc(5,ind2)
      else
        g = fc(4,ind2)
      end if

      if (f <= 0 .and. g <= 0) then

        if (.not. bndcon) then

          call updatf(a,c,d,b,e,i,npt,sizht,front,back,fc,ht, ierr )
          call updatf(a,c,e,b,d,i,npt,sizht,front,back,fc,ht, ierr )
          call updatf(b,c,d,a,e,i,npt,sizht,front,back,fc,ht, ierr )
          call updatf(b,c,e,a,d,i,npt,sizht,front,back,fc,ht, ierr )

          if (ierr /= 0) return

          call htdel(ind,npt,sizht,fc,ht)
          call htins(ind,c,d,e,a,b,npt,sizht,fc,ht)
          call htdel(ind1,npt,sizht,fc,ht)
          call htins(ind1,a,d,e,c,fc(5,ind1),npt,sizht,fc,ht)
          call htdel(ind2,npt,sizht,fc,ht)
          call htins(ind2,b,d,e,c,fc(5,ind2),npt,sizht,fc,ht)
          indx(1) = ind1
          indx(2) = ind2
          bfx(1) = -fc(5,ind1)
          bfx(2) = -fc(5,ind2)
          dd = d

          do j = 1,2

            if (j == 2) dd = e

            if (dd < a) then
              nbr(j,1) = bf(3,bfx(j))
              nbr(j,2) = bf(2,bfx(j))
            else if (dd < b) then
              nbr(j,1) = bf(3,bfx(j))
              nbr(j,2) = bf(1,bfx(j))
            else
              nbr(j,1) = bf(2,bfx(j))
              nbr(j,2) = bf(1,bfx(j))
            end if

          end do

          aa = a
          k = -fc(5,nbr(1,2))

          do j = 1,2

            if (j == 2) then
              aa = b
              k = -fc(5,nbr(2,1))
            end if

            if (aa < d) then
              bf(1,bfx(j)) = indx(3-j)
              bf(2,bfx(j)) = nbr(2,j)
              bf(3,bfx(j)) = nbr(1,j)
            else if (aa < e) then
              bf(1,bfx(j)) = nbr(2,j)
              bf(2,bfx(j)) = indx(3-j)
              bf(3,bfx(j)) = nbr(1,j)
            else
              bf(1,bfx(j)) = nbr(2,j)
              bf(2,bfx(j)) = nbr(1,j)
              bf(3,bfx(j)) = indx(3-j)
            end if

            if (bf(1,k) == indx(j)) then
              bf(1,k) = indx(3-j)
            else if (bf(2,k) == indx(j)) then
              bf(2,k) = indx(3-j)
            else
              bf(3,k) = indx(3-j)
            end if

          end do

          if (msglvl == 4) then
            write ( *,640) a,b,d,e
          end if

        end if

      else if (f == g) then

        call updatf(a,c,d,b,e,i,npt,sizht,front,back,fc,ht, ierr )
        call updatf(a,c,e,b,d,i,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,c,d,a,e,i,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,c,e,a,d,i,npt,sizht,front,back,fc,ht, ierr )
        call updatf(a,d,f,b,e,i,npt,sizht,front,back,fc,ht, ierr )
        call updatf(a,e,f,b,d,i,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,d,f,a,e,i,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,e,f,a,d,i,npt,sizht,front,back,fc,ht, ierr )

        if (ierr /= 0) return

        call htdel(ind,npt,sizht,fc,ht)
        call htins(ind,c,d,e,a,b,npt,sizht,fc,ht)
        ind = htsrc(a,b,f,npt,sizht,fc,ht)
        if (ind <= 0) go to 50
        call htdel(ind,npt,sizht,fc,ht)

        if (fc(7,ind) >= 0) then
          fc(2,ind) = 0
          call availf(hdavfc,nfc,maxfc,fc,ind,ierr)
          if (ierr /= 0) return
        end if

        call htins(ind,d,e,f,a,b,npt,sizht,fc,ht)
        call htdel(ind1,npt,sizht,fc,ht)
        j = fc(7,ind1)
        call htins(ind1,a,d,e,c,f,npt,sizht,fc,ht)
        fc(7,ind1) = j
        call htdel(ind2,npt,sizht,fc,ht)
        j = fc(7,ind2)
        call htins(ind2,b,d,e,c,f,npt,sizht,fc,ht)
        fc(7,ind2) = j

        if (i <= 0 .and. fc(7,ind1) == -1) then
          fc(7,ind1) = 0
          if (front == 0) then
            front = ind1
          else
            fc(7,back) = ind1
          end if
          back = ind1
        end if

        if (i <= 0 .and. fc(7,ind2) == -1) then
          fc(7,ind2) = 0
          if (front == 0) then
            front = ind2
          else
            fc(7,back) = ind2
          end if
          back = ind2
        end if

        if (msglvl == 4) then
          write ( *,650) a,b,d,e,f
        end if

      else

        if (msglvl == 4) then
          write ( *,660) a,b,d,e,f,g
        end if

      end if

    end if

  end if

  go to 10

50 continue

  ierr = 300

  600 format (1x,'index =',i7,' : ',5i7)
  610 format (4x,'swap 2-3')
  620 format (4x,'swap 3-2 with new common face:',3i7)
  630 format (4x,'swap 3-2 not poss, tetra missing:',4i7)
  640 format (4x,'swap 2-2: edge ',2i7,' repl by ',2i7)
  650 format (4x,'swap 4-4: edge ',2i7,' repl by ',2i7,'   f =',i7)
  660 format (4x,'swap 4-4 not poss: a,b,d,e,f,g =',6i7)

  return
end
subroutine swaphs ( k, i, npt, sizht, nbf, nfc, maxbf, maxfc, vcl, vm, bf, &
  fc, ht, nsmplx, hdavbf, hdavfc, front, back, ifac, bfi, ind, indf, mv, loc, &
  zpn, alpha, mat, ierr )
!
!******************************************************************************
!
!! SWAPHS swaps faces in a KD triangulation.
!
!
!  Purpose: 
!
!    Swap faces (apply local transformations) in K-D triangulation 
!    based on empty hypercircumsphere criterion until all
!    faces are locally optimal, where I is index of new vertex
!    added to triangulation.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of triangulation.
!
!    Input, I - local index of next vertex inserted in triangulation;
!    it is assumed I is largest index so far.
!
!    Input, NPT - number of K-D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NBF - number of positions used in BF array.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXBF - maximum size available for BF array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:K,1:*) - vertex coordinate list.
!
!    Input, VM(1:NPT) - indices of vertices of VCL being triangulated.
!
!    Input/output, BF(1:K,1:MAXBF) -  array of boundary face records; 
!    see DTRISK.
!
!    Input/output, FC(1:K+4,1:MAXFC) - array of face records; see 
!    routine DTRISK.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NSMPLX - number of simplices in triangulation.
!
!    Input/output, HDAVBF - head pointer to available BF records.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Input/output, FRONT, BACK - indices of front and back of queue of interior
!    faces for which hypersphere test is applied.
!
!    Output, IFAC - index of last face for which sphere test applied (if
!    swap then index of new interior face), or 0.
!
!    Output, BFI - index of FC of a boundary face containing vertex I if a
!    degenerate local transformation is applied, or 0.
!
!    Workspace, IND(1:K+1) - indices in VCL or local vertex indices.
!
!    Workspace, INDF(1:K+1) - pivot indices or face indices.
!
!    Workspace, MV(1:K+1) - missing local vertex indices used in degen cases.
!
!    Workspace, LOC(1:K) - used by routine SWAPDG.
!
!    Workspace, ZPN(1:K) - used by routine SWAPDG.
!
!    Workspace, ALPHA(1:K+1) - barycentric coordinates or hypersphere center.
!
!    Workspace, MAT(1:K,1:K) - matrix used for solving system of linear
!    equations.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer maxbf
  integer maxfc
  integer npt
  integer sizht
!
  integer a
  double precision alpha(k+1)
  integer b
  integer back
  integer bf(k,maxbf)
  integer bfi
  integer bfn
  integer bfp
  integer c
  integer d
  logical degen
  integer e
  integer f
  integer fc(k+4,maxfc)
  integer front
  integer hdavbf
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrck
  integer i
  integer ierr
  integer ifac
  integer ii
  integer in
  integer ind(k+1)
  integer indf(k+1)
  integer izero
  integer j
  integer jj
  integer jn
  integer jp
  integer km1
  integer kp1
  integer kp2
  integer kp4
  integer loc(k)
  double precision mat(k,k)
  integer, parameter :: msglvl = 0
  integer mv(k+1)
  integer nbf
  integer nbr
  integer nfc
  integer nsmplx
  integer num
  integer pos
  integer ptr
  integer ptrn
  double precision radsq
  integer sneg
  integer spos
  integer szero
  double precision tol
  double precision vcl(k,*)
  integer ve
  integer vm(npt)
  integer zpn(k)
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  km1 = k - 1
  kp1 = k + 1
  kp2 = k + 2
  kp4 = k + 4
  ifac = 0
  bfi = 0

10 continue

  if (front == 0) return

  pos = front
  front = fc(kp4,pos)

  if (fc(2,pos) == 0) then
    fc(1,pos) = -hdavfc
    hdavfc = pos
    go to 10
  end if

  ifac = pos
  fc(kp4,pos) = -1
  ind(1:k) = vm(fc(1:k,pos))
  d = fc(kp1,pos)
  e = fc(kp2,pos)

  if (d == i) then
    d = e
    e = i
  end if

  ind(kp1) = vm(d)
  ve = vm(e)

  if (msglvl == 4) then
    write ( *,600) pos,(fc(ii,pos),ii=1,k),d,e
  end if

  call ccsphk(k,.true.,ind,vcl,vcl(1,ve),alpha,radsq,in,mat,indf)
  if (in < 1) go to 10

  call baryck(k,ind,vcl,vcl(1,ve),alpha,degen,mat,indf)

  if (degen) then
    ierr = 401
    return
  end if

  sneg = 2
  szero = 0

  do j = 1,k

    if (alpha(j) < -tol) then
      alpha(j) = -1.0d0
      sneg = sneg + 1
    else if (alpha(j) <= tol) then
      alpha(j) = 0.0d0
      szero = szero + 1
      izero = j
    else
      alpha(j) = 1.0d0
    end if

  end do

  spos = kp2 - sneg - szero

  if (msglvl == 4) then
    write ( *,610) (alpha(j),j=1,k)
  end if

  if (sneg < 2 .or. spos < 2) then

    ierr = 405
    return
!
!  Swap SNEG simplices for SPOS if possible.  Process old interior,
!  then new interior, then boundary faces. E = I.
!
  else if (szero == 0) then

    if (sneg >= 3) then

      jj = 0

      do j = 1,k

        if (alpha(j) /= -1.0d0) then
          cycle
        end if

        ind(1:k) = fc(1:k,pos)
        ind(j) = d
        ptr = htsrck(k,ind,npt,sizht,fc,ht)
        if (ptr <= 0) go to 450
        jj = jj + 1
        indf(jj) = ptr

        if (fc(kp1,ptr) /= e .and. fc(kp2,ptr) /= e) then
          if (msglvl == 4) then
            write ( *,620) sneg,spos
          end if
          go to 10
        end if

      end do

      jj = 0

      do j = 1,k

        if (alpha(j) /= -1.0d0) then
          cycle
        end if

        jj = jj + 1
        ptr = indf(jj)
        call htdelk(k,ptr,npt,sizht,fc,ht)

        if (fc(kp4,ptr) >= 0) then
          fc(2,ptr) = 0
        else
          fc(1,ptr) = -hdavfc
          hdavfc = ptr
        end if

        ind(1:k) = fc(1:k,pos)
        ind(j) = e
        call htsdlk(k,ind,npt,sizht,fc,ht,ptr)
        if (ptr <= 0) go to 450
        fc(1,ptr) = -hdavfc
        hdavfc = ptr

      end do

    end if

    num = 1
    nsmplx = nsmplx + (spos - sneg)

    if (msglvl == 4) then
      write ( *,630) sneg,spos
    end if
!
!  Swap SNEG simplices for SPOS if possible. Two simultaneous
!  local transformations may be needed. Process old and new
!  faces of degenerate local transformation, then other old
!  interior, other new interior, other boundary faces. E = I.
!
  else if (szero == 1) then

    if (sneg >= 3) then

      jj = 0

      do j = 1,k

        if (alpha(j) /= -1.0d0) then
          cycle
        end if

        ind(1:k) = fc(1:k,pos)
        ind(izero) = d
        ind(j) = e
        ptr = htsrck(k,ind,npt,sizht,fc,ht)

        if (ptr <= 0) then
          if (msglvl == 4) then
            write ( *,620) sneg,spos, 'missing face'
          end if
          go to 10
        end if

        jj = jj + 1
        indf(jj) = ptr
        mv(jj) = fc(j,pos)

      end do

    end if

    ind(1:k) = fc(1:k,pos)
    ind(izero) = e
    ptr = htsrck(k,ind,npt,sizht,fc,ht)
    if (ptr <= 0) go to 450
    indf(sneg-1) = ptr
    ind(1:k) = fc(1:k,pos)
    a = ind(izero)
    ind(izero) = d
    ptr = htsrck(k,ind,npt,sizht,fc,ht)
    if (ptr <= 0) go to 450
    indf(sneg) = ptr
    f = fc(kp2,ptr)

    if (f > 0) then

      if (f == a) f = fc(kp1,ptr)

      do j = 1,sneg-1

        ptr = indf(j)

        do ii = kp1,kp2

          b = fc(ii,ptr)

          if (b /= a .and. b /= f) then
            if (msglvl == 4) then
              write ( *,620) sneg,spos, 'other vertices not common'
            end if
            go to 10
          end if

        end do

      end do

      num = 2

      do j = 1,sneg
        ptr = indf(j)
        call htdelk(k,ptr,npt,sizht,fc,ht)
        fc(1,ptr) = -hdavfc
        hdavfc = ptr
      end do

      do j = 1,k

        if (alpha(j) /= 1.0d0) then
          cycle
        end if

        call availk(k,hdavfc,nfc,maxfc,fc,ptr,ierr)
        if (ierr /= 0) return
        ind(1:k) = fc(1:k,pos)
        ind(izero) = d
        ind(j) = e
        call htinsk(k,ptr,ind,a,f,npt,sizht,fc,ht)

      end do

      ind(1:k) = fc(1:k,pos)
      ind(izero) = f
      call htsdlk(k,ind,npt,sizht,fc,ht,ptr)
      if (ptr <= 0) go to 450

      if (fc(kp4,ptr) >= 0) then
        fc(2,ptr) = 0
      else
        fc(1,ptr) = -hdavfc
        hdavfc = ptr
      end if

      nsmplx = nsmplx + 2*(spos - sneg)

      if (msglvl == 4) then
        write ( *,630) sneg,spos, 'other vertex =',f
      end if

    else

      do j = 1,sneg-2
        b = fc(kp1,indf(j))
        if (b /= a) then
          if (msglvl == 4) then
            write ( *,620) sneg,spos, 'other vertex not common'
          end if
          go to 10
        end if
      end do

      num = 1
      jj = sneg

      do j = 1,k

        if (alpha(j) /= 1.0d0) then
          cycle
        end if

        call availk(k,hdavfc,nfc,maxfc,fc,ptr,ierr)
        if (ierr /= 0) return

        if (hdavbf /= 0) then
          bfp = hdavbf
          hdavbf = -bf(1,hdavbf)
        else
          if (nbf >= maxbf) then
            ierr = 23
            return
          else
            nbf = nbf + 1
            bfp = nbf
          end if
        end if

        ind(1:k) = fc(1:k,pos)
        ind(izero) = d
        ind(j) = e
        call htinsk(k,ptr,ind,a,-bfp,npt,sizht,fc,ht)
        jj = jj + 1
        indf(jj) = ptr
        mv(jj) = fc(j,pos)

      end do

      bfi = ptr
      mv(sneg-1) = d
      mv(sneg) = e

      if (sneg >= 3) then

        j = sneg - 1
        ptr = indf(j)

210     continue

        if (mv(j-1) > d) then
          mv(j) = mv(j-1)
          indf(j) = indf(j-1)
           j = j - 1
          if (j > 1) go to 210
        end if

        mv(j) = d
        indf(j) = ptr

      end if

      do j = sneg+1,kp1

        b = mv(j)
        ptr = indf(j)
        bfp = -fc(kp2,ptr)
        jn = 1
        jp = sneg + 1
        if (jp == j) jp = jp + 1

        do jj = 1,k

          c = fc(jj,ptr)

          if (c == mv(jp)) then

            bf(jj,bfp) = indf(jp)
            jp = jp + 1
            if (jp == j) jp = jp + 1
            if (jp > kp1) jp = kp1

          else

            ptrn = indf(jn)
            bfn = -fc(kp2,ptrn)
            ii = 1

            do while (fc(ii,ptrn) /= b) 
              ii = ii + 1
            end do

            nbr = bf(ii,bfn)
            bf(jj,bfp) = nbr
            bfn = -fc(kp2,nbr)
            ii = 1

            do while (bf(ii,bfn) /= ptrn)
              ii = ii + 1
            end do

            bf(ii,bfn) = ptr
            jn = jn + 1

          end if

        end do

      end do

      do j = 1,sneg
        ptr = indf(j)
        bfp = -fc(kp2,ptr)
        call htdelk(k,ptr,npt,sizht,fc,ht)
        fc(1,ptr) = -hdavfc
        hdavfc = ptr
        bf(1,bfp) = -hdavbf
        hdavbf = bfp
      end do

      nsmplx = nsmplx + (spos - sneg)

      if (msglvl == 4) then
        write ( *,630) sneg,spos, 'boundary degenerate'
      end if

    end if

    if (sneg >= 3) then

      do j = 1,k

        if (alpha(j) /= -1.0d0) then
          cycle
        end if

        do in = 1,num+num

          ind(1:k) = fc(1:k,pos)

          if (in == 1 .or. in == 3) then
            ind(j) = d
          else
            ind(j) = e
          end if

          if (in >= 3) ind(izero) = f
          call htsdlk(k,ind,npt,sizht,fc,ht,ptr)
          if (ptr <= 0) go to 450

          if (fc(kp4,ptr) >= 0) then
            fc(2,ptr) = 0
          else
            fc(1,ptr) = -hdavfc
            hdavfc = ptr
          end if

        end do

      end do

    end if
!
!  Call SWAPDG for case of SZERO >= 2.
!
  else

    call swapdg(k,pos,d,i,sneg,spos,szero,alpha,npt,sizht,nbf, &
      nfc,maxbf,maxfc,vcl,vm,bf,fc,ht,nsmplx,hdavbf,hdavfc, &
      front,back,ifac,bfi,ind,indf,mv,loc,zpn, ierr )
      if (ierr /= 0) return
    go to 10

  end if
!
!  Common code for cases of SZERO = 0 and 1.
!
  if (sneg >= 4) then

    do j = 1,km1

      if (alpha(j) /= -1.0d0) then
        cycle
      end if

      do jj = j+1,k

        if (alpha(jj) /= -1.0d0) then
          cycle
        end if

        do in = 1,num

          ind(1:k) = fc(1:k,pos)
          ind(j) = d
          ind(jj) = e
          if (in == 2) ind(izero) = f
          call htsdlk(k,ind,npt,sizht,fc,ht,ptr)
          if (ptr <= 0) go to 450
          fc(1,ptr) = -hdavfc
          hdavfc = ptr

        end do

      end do

    end do

  end if

  do j = 1,k

    if (alpha(j) /= 1.0d0) then
      cycle
    end if

    a = fc(j,pos)

    do jj = j+1,k

      if (alpha(jj) /= 1.0d0) then
        cycle
      end if

      b = fc(jj,pos)

      do in = 1,num
        call availk(k,hdavfc,nfc,maxfc,fc,ptr,ierr)
        if (ierr /= 0) return
        ind(1:k) = fc(1:k,pos)
        ind(j) = d
        ind(jj) = e
        if (in == 2) ind(izero) = f
        call htinsk(k,ptr,ind,a,b,npt,sizht,fc,ht)

      end do

    end do

  end do

  ifac = ptr

  do j = 1,k

    if (alpha(j) /= 1.0d0) then
      cycle
    end if

    a = fc(j,pos)

    do in = 1,num+num

      ind(1:k) = fc(1:k,pos)

      if (in == 1 .or. in == 3) then
        ind(j) = d
        b = e
      else
        ind(j) = e
        b = d
      end if

      if (in >= 3) ind(izero) = f
      call updatk(k,ind,a,b,i,npt,sizht,front,back,fc,ht, ierr )
      if (ierr /= 0) return

    end do

  end do

  if (sneg >= 3) then

    do j = 1,k

      if (alpha(j) /= 1.0d0) then
        cycle
      end if

      a = fc(j,pos)

      do jj = 1,k

        if (alpha(jj) /= -1.0d0) then
          cycle
        end if

        b = fc(jj,pos)

        do in = 1,num
          ind(1:k) = fc(1:k,pos)
          ind(j) = d
          ind(jj) = e
          if (in == 2) ind(izero) = f
          call updatk(k,ind,a,b,i,npt,sizht,front,back,fc,ht, ierr )
          if (ierr /= 0) return
        end do

      end do

    end do

  end if

  call htdelk(k,pos,npt,sizht,fc,ht)
  fc(1,pos) = -hdavfc
  hdavfc = pos

  go to 10

450 continue

  ierr = 400

  600 format (1x,'index=',i7,':',9i7)
  610 format (4x,'sign(1:k)= ',7f4.0)
  620 format (4x,'no swap ',i2,' -',i2,3x,a)
  630 format (4x,'swap ',i2,' -',i2,3x,a,i7)

  return
end
subroutine swapmu ( bndcon, crit, npt, sizht, nfc, maxfc, vcl, vm, bf, fc, &
  ht, ntetra, hdavfc, front, back, ifac, ierr )
!
!******************************************************************************
!
!! SWAPMU swaps faces in a KD triangulation.
!
!
!  Purpose: 
!
!    Swap faces (apply local transformations) in 3D triangulation 
!    based on local max-min solid angle criterion until all
!    faces are locally optimal.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, BNDCON - .TRUE. iff boundary faces are constrained (i.e. not
!    swapped by local transformations).
!
!    Input, CRIT - criterion code; 1 for (local max-min) solid angle
!    criterion, 2 for radius ratio criterion, 3 for mean ratio
!    criterion, 0 (or anything else) for no swaps.
!
!    Input, NPT - number of 3D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:NPT) - indices of vertices of VCL being triangulated.
!
!    Input/output, BF(1:3,1:*) -  array of boundary face records; see DTRIS3.
!
!    Input/output, FC(1:7,1:MAXFC) - array of face records; see routine DTRIS3.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NTETRA - number of tetrahedra in triangulation.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Input/output, FRONT, BACK - indices of front and back of queue of interior
!    faces for which local optimality test is applied.
!
!    Output, IFAC - index of last face processed from queue, or 0.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfc
  integer npt
  integer sizht
!
  integer a
  integer aa
  double precision alpha(4)
  integer b
  integer back
  double precision beta
  integer bf(3,*)
  logical bndcon
  integer bfx(2)
  integer c
  integer crit
  integer d
  integer dd
  logical degen
  integer e
  integer f
  integer fc(7,maxfc)
  integer front
  integer g
  double precision gama
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrc
  integer ierr
  integer ifac
  integer ind
  integer ind1
  integer ind2
  integer indx(2)
  integer j
  integer k
  integer kneg
  integer kzero
  double precision m1
  double precision m2
  double precision m3
  double precision m4
  integer, parameter :: msglvl = 0
  integer nbr(2,2)
  integer nfc
  integer ntetra
  double precision s(4)
  double precision tetmu
  double precision tol
  double precision vcl(3,*)
  integer va
  integer vb
  integer vc
  integer vd
  integer ve
  integer vf
  integer vm(npt)
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  ifac = 0

10 continue

  if (front == 0) return
  ind = front
  front = fc(7,ind)

  if (fc(2,ind) == 0) then
    if (ind == nfc) then
      nfc = nfc - 1
    else
      fc(1,ind) = -hdavfc
      hdavfc = ind
    end if
    go to 10
  end if

  ifac = ind
  fc(7,ind) = -1
  a = fc(1,ind)
  b = fc(2,ind)
  c = fc(3,ind)
  d = fc(4,ind)
  e = fc(5,ind)
  va = vm(a)
  vb = vm(b)
  vc = vm(c)
  vd = vm(d)
  ve = vm(e)

  if (msglvl == 4) then
    write ( *,600) ind,a,b,c,d,e
  end if

  call baryth(vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd),vcl(1,ve), &
    alpha,degen)

  if (degen) then
    ierr = 301
    return
  else if (alpha(4) > 0.0d0) then
    ierr = 309
    return
  end if

  kneg = 1
  kzero = 0

  do j = 1,3
    if (alpha(j) < 0.0d0) then
      kneg = kneg + 1
    else if (alpha(j) == 0.0d0) then
      kzero = kzero + 1
    end if
  end do

  if (kneg == 1 .and. kzero == 0) then

    m1 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd),s)
    m2 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,ve),s)
    beta = min(m1,m2)
    m1 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vd),vcl(1,ve),s)
    m2 = tetmu(crit,vcl(1,va),vcl(1,vc),vcl(1,vd),vcl(1,ve),s)
    m3 = tetmu(crit,vcl(1,vb),vcl(1,vc),vcl(1,vd),vcl(1,ve),s)
    gama = min(m1,m2,m3)
!
!  Swap 2 tetrahedra for 3.
!
    if (gama > beta + tol) then

      call updatf(a,b,d,c,e,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(a,c,d,b,e,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(b,c,d,a,e,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(a,b,e,c,d,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(a,c,e,b,d,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(b,c,e,a,d,0,npt,sizht,front,back,fc,ht, ierr )

      if (ierr /= 0) return

      call htdel(ind,npt,sizht,fc,ht)
      call htins(ind,a,d,e,b,c,npt,sizht,fc,ht)
      call availf(hdavfc,nfc,maxfc,fc,ind,ierr)

      if (ierr /= 0) return

      call htins(ind,b,d,e,a,c,npt,sizht,fc,ht)
      call availf(hdavfc,nfc,maxfc,fc,ind,ierr)
      if (ierr /= 0) return
      call htins(ind,c,d,e,a,b,npt,sizht,fc,ht)
      ntetra = ntetra + 1

      if (msglvl == 4) then
        write ( *,610)
      end if

    end if
!
!  Relabel so edge AB would be deleted by swap.
!
  else if (kneg == 2 .and. kzero == 0) then

    if (alpha(1) < 0.0d0) then
      call i_swap ( a, c )
    else if (alpha(2) < 0.0d0) then
      call i_swap ( b, c )
    end if

    ind1 = htsrc(a,b,d,npt,sizht,fc,ht)
    if (ind1 <= 0) go to 50

    if (fc(4,ind1) == e .or. fc(5,ind1) == e) then

      va = vm(a)
      vb = vm(b)
      vc = vm(c)
      m1 =tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd),s)
      m2 =tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,ve),s)
      m3 =tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vd),vcl(1,ve),s)
      beta = min(m1,m2,m3)
      m1 =tetmu(crit,vcl(1,va),vcl(1,vc),vcl(1,vd),vcl(1,ve),s)
      m2 =tetmu(crit,vcl(1,vb),vcl(1,vc),vcl(1,vd),vcl(1,ve),s)
      gama = min(m1,m2)
!
!  Swap 3 tetrahedra for 2.
!
      if (gama > beta + tol) then

        call updatf(a,c,d,b,e,0,npt,sizht,front,back,fc,ht, ierr )
        call updatf(a,c,e,b,d,0,npt,sizht,front,back,fc,ht, ierr )
        call updatf(a,d,e,b,c,0,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,c,d,a,e,0,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,c,e,a,d,0,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,d,e,a,c,0,npt,sizht,front,back,fc,ht, ierr )

        if (ierr /= 0) return

        call htdel(ind,npt,sizht,fc,ht)
        call htins(ind,c,d,e,a,b,npt,sizht,fc,ht)
        call htdel(ind1,npt,sizht,fc,ht)

        if (fc(7,ind1) >= 0) then
          fc(2,ind1) = 0
        else
          if (ind1 == nfc) then
            nfc = nfc - 1
          else
            fc(1,ind1) = -hdavfc
            hdavfc = ind1
          end if
        end if

        ind1 = htsrc(a,b,e,npt,sizht,fc,ht)
        if (ind1 <= 0) go to 50
        call htdel(ind1,npt,sizht,fc,ht)

        if (fc(7,ind1) >= 0) then
          fc(2,ind1) = 0
        else
          if (ind1 == nfc) then
            nfc = nfc - 1
          else
            fc(1,ind1) = -hdavfc
            hdavfc = ind1
          end if
        end if

        ntetra = ntetra - 1

        if (msglvl == 4) then
          write ( *,620) c,d,e
        end if

      end if

    end if
!
!  Relabel vertices so that DE intersects AB.
!  Also swap if necessary to make A < B and D < E.
!
  else if (kneg == 1 .and. kzero == 1) then

    if (alpha(1) == 0.0d0) then
      call i_swap ( a, c )
    else if (alpha(2) == 0.0d0) then
      call i_swap ( b, c )
    end if

    if (a > b) then
      call i_swap ( a, b )
    end if

    if (d > e) then
      call i_swap ( d, e )
    end if

    ind1 = htsrc(a,b,d,npt,sizht,fc,ht)
    ind2 = htsrc(a,b,e,npt,sizht,fc,ht)
    if (ind1 <= 0 .or. ind2 <= 0) go to 50

    if (fc(4,ind1) == c) then
      f = fc(5,ind1)
    else
      f = fc(4,ind1)
    end if

    if (fc(4,ind2) == c) then
      g = fc(5,ind2)
    else
      g = fc(4,ind2)
    end if

    if (f <= 0 .and. g <= 0) then

      if (.not. bndcon) then

        va = vm(a)
        vb = vm(b)
        vc = vm(c)
        vd = vm(d)
        ve = vm(e)

        m1 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd),s)
        m2 = tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,ve),s)
        beta = min(m1,m2)
        m1 = tetmu(crit,vcl(1,va),vcl(1,vc),vcl(1,vd),vcl(1,ve),s)
        m2 = tetmu(crit,vcl(1,vb),vcl(1,vc),vcl(1,vd),vcl(1,ve),s)
        gama = min(m1,m2)
!
!  Swap 2 tetrahedra for 2.
!
        if (gama > beta + tol) then

          call updatf(a,c,d,b,e,0,npt,sizht,front,back,fc,ht, ierr )
          call updatf(a,c,e,b,d,0,npt,sizht,front,back,fc,ht, ierr )
          call updatf(b,c,d,a,e,0,npt,sizht,front,back,fc,ht, ierr )
          call updatf(b,c,e,a,d,0,npt,sizht,front,back,fc,ht, ierr )
          if (ierr /= 0) return
          call htdel(ind,npt,sizht,fc,ht)
          call htins(ind,c,d,e,a,b,npt,sizht,fc,ht)
          call htdel(ind1,npt,sizht,fc,ht)
          call htins(ind1,a,d,e,c,fc(5,ind1),npt,sizht,fc,ht)
          call htdel(ind2,npt,sizht,fc,ht)
          call htins(ind2,b,d,e,c,fc(5,ind2),npt,sizht,fc,ht)
          indx(1) = ind1
          indx(2) = ind2
          bfx(1) = -fc(5,ind1)
          bfx(2) = -fc(5,ind2)
          dd = d

          do j = 1,2
            if (j == 2) dd = e
            if (dd < a) then
              nbr(j,1) = bf(3,bfx(j))
              nbr(j,2) = bf(2,bfx(j))
            else if (dd < b) then
              nbr(j,1) = bf(3,bfx(j))
              nbr(j,2) = bf(1,bfx(j))
            else
              nbr(j,1) = bf(2,bfx(j))
              nbr(j,2) = bf(1,bfx(j))
            end if
          end do

          aa = a
          k = -fc(5,nbr(1,2))

          do j = 1,2

            if (j == 2) then
              aa = b
              k = -fc(5,nbr(2,1))
            end if

            if (aa < d) then
              bf(1,bfx(j)) = indx(3-j)
              bf(2,bfx(j)) = nbr(2,j)
              bf(3,bfx(j)) = nbr(1,j)
            else if (aa < e) then
              bf(1,bfx(j)) = nbr(2,j)
              bf(2,bfx(j)) = indx(3-j)
              bf(3,bfx(j)) = nbr(1,j)
            else
              bf(1,bfx(j)) = nbr(2,j)
              bf(2,bfx(j)) = nbr(1,j)
              bf(3,bfx(j)) = indx(3-j)
            end if

            if (bf(1,k) == indx(j)) then
              bf(1,k) = indx(3-j)
            else if (bf(2,k) == indx(j)) then
              bf(2,k) = indx(3-j)
            else
              bf(3,k) = indx(3-j)
            end if

          end do

          if (msglvl == 4) then
            write ( *,630) a,b,d,e
          end if

        end if

      end if

    else if (f == g) then

      va = vm(a)
      vb = vm(b)
      vc = vm(c)
      vd = vm(d)
      ve = vm(e)
      vf = vm(f)
      m1 =tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd),s)
      m2 =tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,ve),s)
      m3 =tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,vd),vcl(1,vf),s)
      m4 =tetmu(crit,vcl(1,va),vcl(1,vb),vcl(1,ve),vcl(1,vf),s)
      beta = min(m1,m2,m3,m4)
      m1 =tetmu(crit,vcl(1,va),vcl(1,vc),vcl(1,vd),vcl(1,ve),s)
      m2 =tetmu(crit,vcl(1,vb),vcl(1,vc),vcl(1,vd),vcl(1,ve),s)
      m3 =tetmu(crit,vcl(1,va),vcl(1,vd),vcl(1,ve),vcl(1,vf),s)
      m4 =tetmu(crit,vcl(1,vb),vcl(1,vd),vcl(1,ve),vcl(1,vf),s)
      gama = min(m1,m2,m3,m4)
!
!  Swap 4 tetrahedra for 4.
!
      if (gama > beta + tol) then

        call updatf(a,c,d,b,e,0,npt,sizht,front,back,fc,ht, ierr )
        call updatf(a,c,e,b,d,0,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,c,d,a,e,0,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,c,e,a,d,0,npt,sizht,front,back,fc,ht, ierr )
        call updatf(a,d,f,b,e,0,npt,sizht,front,back,fc,ht, ierr )
        call updatf(a,e,f,b,d,0,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,d,f,a,e,0,npt,sizht,front,back,fc,ht, ierr )
        call updatf(b,e,f,a,d,0,npt,sizht,front,back,fc,ht, ierr )

        if (ierr /= 0) return
        call htdel(ind,npt,sizht,fc,ht)
        call htins(ind,c,d,e,a,b,npt,sizht,fc,ht)
        ind = htsrc(a,b,f,npt,sizht,fc,ht)
        if (ind <= 0) go to 50
        call htdel(ind,npt,sizht,fc,ht)

        if (fc(7,ind) >= 0) then
          fc(2,ind) = 0
          call availf(hdavfc,nfc,maxfc,fc,ind,ierr)
          if (ierr /= 0) return
        end if

        call htins(ind,d,e,f,a,b,npt,sizht,fc,ht)
        call htdel(ind1,npt,sizht,fc,ht)
        j = fc(7,ind1)
        call htins(ind1,a,d,e,c,f,npt,sizht,fc,ht)
        fc(7,ind1) = j
        call htdel(ind2,npt,sizht,fc,ht)
        j = fc(7,ind2)
        call htins(ind2,b,d,e,c,f,npt,sizht,fc,ht)
        fc(7,ind2) = j

        if (fc(7,ind1) == -1) then

          fc(7,ind1) = 0

          if (front == 0) then
            front = ind1
          else
            fc(7,back) = ind1
          end if

          back = ind1

        end if

        if (fc(7,ind2) == -1) then

          fc(7,ind2) = 0

          if (front == 0) then
            front = ind2
          else
            fc(7,back) = ind2
          end if

          back = ind2

        end if

        if (msglvl == 4) then
          write ( *,640) a,b,d,e,f
        end if

      end if

    end if

  end if

  go to 10

50 continue

  ierr = 300

  600 format (1x,'index =',i7,' : ',5i7)
  610 format (4x,'swap 2-3')
  620 format (4x,'swap 3-2 with new common face:',3i7)
  630 format (4x,'swap 2-2: edge ',2i7,' repl by ',2i7)
  640 format (4x,'swap 4-4: edge ',2i7,' repl by ',2i7,'   f =',i7)

  return
end
subroutine swaptf ( top, npt, sizht, nfc, maxfc, vcl, vm, fc, ht, ntetra, &
  hdavfc, front, back, ierr )
!
!******************************************************************************
!
!! SWAPTF swaps tranformable faces in a 3D triangulation.
!
!
!  Purpose: 
!
!    Swap transformable faces of type T23, T32, or T44 in 3D
!    triangulation in the order given on stack.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input/output, TOP - pointer to stack of faces to be transformed; each face
!    should be transformable upon reaching top of stack.
!
!    Input, NPT - number of 3D points to be triangulated.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input/output, NFC - number of positions used in FC array.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:NPT) - indices of vertices of VCL being triangulated.
!
!    Input/output, FC(1:7,1:MAXFC) - array of face records; see routine DTRIS3.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Input/output, NTETRA - number of tetrahedra in triangulation.
!
!    Input/output, HDAVFC - head pointer to available FC records.
!
!    Input/output, FRONT, BACK - indices of front and back of queue of interior
!    faces for which local optimality test may be applied;
!    this routine adds faces to end of queue.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfc
  integer npt
  integer sizht
!
  integer a
  double precision alpha(4)
  integer b
  integer back
  integer c
  integer d
  logical degen
!  integer d
  integer e
  integer f
  integer fc(7,maxfc)
  integer front
  integer g
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrc
  integer ierr
  integer ind
  integer ind1
  integer ind2
  integer j
  integer kneg
  integer kzero
  integer, parameter :: msglvl = 0
  integer nfc
  integer ntetra
  integer top
  integer va
  integer vb
  integer vc
  double precision vcl(3,*)
  integer vd
  integer ve
  integer vm(npt)
!
  ierr = 0

10 continue

  if (top == 0) then
    return
  end if

  ind = top
  top = fc(7,ind)

  if (fc(2,ind) == 0) then
    ierr = 308
    return
  end if

  fc(7,ind) = -1
  a = fc(1,ind)
  b = fc(2,ind)
  c = fc(3,ind)
  d = fc(4,ind)
  e = fc(5,ind)
  va = vm(a)
  vb = vm(b)
  vc = vm(c)
  vd = vm(d)
  ve = vm(e)

  if (msglvl == 4) then
    write ( *,600) ind,a,b,c,d,e
  end if

  call baryth(vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd),vcl(1,ve), &
    alpha,degen)

  if (degen) then
    ierr = 301
    return
  else if (alpha(4) > 0.0d0) then
    ierr = 309
    return
  end if

  kneg = 1
  kzero = 0

  do j = 1,3
    if (alpha(j) < 0.0d0) then
      kneg = kneg + 1
    else if (alpha(j) == 0.0d0) then
      kzero = kzero + 1
    end if
  end do

  if (kneg == 1 .and. kzero == 0) then
!
!  Swap 2 tetrahedra for 3.
!
    call updatf(a,b,d,c,e,0,npt,sizht,front,back,fc,ht, ierr )
    call updatf(a,c,d,b,e,0,npt,sizht,front,back,fc,ht, ierr )
    call updatf(b,c,d,a,e,0,npt,sizht,front,back,fc,ht, ierr )
    call updatf(a,b,e,c,d,0,npt,sizht,front,back,fc,ht, ierr )
    call updatf(a,c,e,b,d,0,npt,sizht,front,back,fc,ht, ierr )
    call updatf(b,c,e,a,d,0,npt,sizht,front,back,fc,ht, ierr )

    if (ierr /= 0) return

    call htdel(ind,npt,sizht,fc,ht)
    call htins(ind,a,d,e,b,c,npt,sizht,fc,ht)
    call availf(hdavfc,nfc,maxfc,fc,ind,ierr)

    if (ierr /= 0) return

    call htins(ind,b,d,e,a,c,npt,sizht,fc,ht)
    call availf(hdavfc,nfc,maxfc,fc,ind,ierr)

    if (ierr /= 0) return

    call htins(ind,c,d,e,a,b,npt,sizht,fc,ht)
    ntetra = ntetra + 1

    if (msglvl == 4) then
      write ( *,610)
    end if

  else if (kneg == 2 .and. kzero == 0) then
!
!  Relabel so new tetrahedra are ACDE, BCDE (edge AB deleted).
!
    if (alpha(1) < 0.0d0) then
      call i_swap ( a, c )
    else if (alpha(2) < 0.0d0) then
      call i_swap ( b, c )
    end if

    ind1 = htsrc(a,b,d,npt,sizht,fc,ht)
    if (ind1 <= 0) go to 50
!
!  Swap 3 tetrahedra for 2.
!
    if (fc(4,ind1) == e .or. fc(5,ind1) == e) then

      call updatf(a,c,d,b,e,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(a,c,e,b,d,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(a,d,e,b,c,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(b,c,d,a,e,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(b,c,e,a,d,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(b,d,e,a,c,0,npt,sizht,front,back,fc,ht, ierr )

      if (ierr /= 0) return

      call htdel(ind,npt,sizht,fc,ht)
      call htins(ind,c,d,e,a,b,npt,sizht,fc,ht)
      call htdel(ind1,npt,sizht,fc,ht)

      if (fc(7,ind1) >= 0) then
        fc(2,ind1) = 0
      else
        if (ind1 == nfc) then
          nfc = nfc - 1
        else
          fc(1,ind1) = -hdavfc
          hdavfc = ind1
        end if
      end if

      ind1 = htsrc(a,b,e,npt,sizht,fc,ht)
      if (ind1 <= 0) go to 50
      call htdel(ind1,npt,sizht,fc,ht)

      if (fc(7,ind1) >= 0) then
        fc(2,ind1) = 0
      else
        if (ind1 == nfc) then
          nfc = nfc - 1
        else
          fc(1,ind1) = -hdavfc
          hdavfc = ind1
        end if
      end if

      ntetra = ntetra - 1

      if (msglvl == 4) then
        write ( *,620) c,d,e
      end if

    else

      ierr = 308
      return

    end if

  else if (kneg == 1 .and. kzero == 1) then
!
!  Relabel vertices so that DE intersects AB.
!
    if (alpha(1) == 0.0d0) then
      call i_swap ( a, c )
    else if (alpha(2) == 0.0d0) then
      call i_swap ( b, c )
    end if

    ind1 = htsrc(a,b,d,npt,sizht,fc,ht)
    ind2 = htsrc(a,b,e,npt,sizht,fc,ht)
    if (ind1 <= 0 .or. ind2 <= 0) go to 50

    if (fc(4,ind1) == c) then
      f = fc(5,ind1)
    else
      f = fc(4,ind1)
    end if

    if (fc(4,ind2) == c) then
      g = fc(5,ind2)
    else
      g = fc(4,ind2)
    end if

    if (f <= 0 .or. g <= 0 .or. f /= g) then
      ierr = 308
      return
    else
!
!  Swap 4 tetrahedra for 4.
!
      call updatf(a,c,d,b,e,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(a,c,e,b,d,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(b,c,d,a,e,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(b,c,e,a,d,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(a,d,f,b,e,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(a,e,f,b,d,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(b,d,f,a,e,0,npt,sizht,front,back,fc,ht, ierr )
      call updatf(b,e,f,a,d,0,npt,sizht,front,back,fc,ht, ierr )

      if (ierr /= 0) return

      call htdel(ind,npt,sizht,fc,ht)
      call htins(ind,c,d,e,a,b,npt,sizht,fc,ht)
      ind = htsrc(a,b,f,npt,sizht,fc,ht)
      if (ind <= 0) go to 50
      call htdel(ind,npt,sizht,fc,ht)

      if (fc(7,ind) >= 0) then
        fc(2,ind) = 0
        call availf(hdavfc,nfc,maxfc,fc,ind,ierr)
        if (ierr /= 0) return
      end if

      call htins(ind,d,e,f,a,b,npt,sizht,fc,ht)
      call htdel(ind1,npt,sizht,fc,ht)

      if (fc(7,ind1) >= 0) then
        fc(2,ind1) = 0
        call availf(hdavfc,nfc,maxfc,fc,ind1,ierr)
        if (ierr /= 0) return
      end if

      call htins(ind1,a,d,e,c,f,npt,sizht,fc,ht)
      call htdel(ind2,npt,sizht,fc,ht)

      if (fc(7,ind2) >= 0) then
        fc(2,ind2) = 0
        call availf(hdavfc,nfc,maxfc,fc,ind2,ierr)
        if (ierr /= 0) return
      end if

      call htins(ind2,b,d,e,c,f,npt,sizht,fc,ht)

      if (front == 0) then
        front = ind1
      else
        fc(7,back) = ind1
      end if

      fc(7,ind1) = ind2
      fc(7,ind2) = 0
      back = ind2

      if (msglvl == 4) then
        write ( *,630) a,b,d,e,f
      end if

    end if

  else

    ierr = 308
    return

  end if

  go to 10

50 continue
  ierr = 300

  600 format (1x,'index =',i7,' : ',5i7)
  610 format (4x,'swap 2-3')
  620 format (4x,'swap 3-2 with new common face:',3i7)
  630 format (4x,'swap 4-4: edge ',2i7,' repl by ',2i7,'   f =',i7)

  return
end
subroutine swprem ( nbf, nbpt, sizht, maxfc, nfc, ntetra, vcl, vm, fc, ht, &
  remov, ierr )
!
!******************************************************************************
!
!! SWPREM tries to remove an interior vertex.
!
!
!  Purpose: 
!
!    In a boundary-constrained 3D triangulation in which one
!    interior vertex is joined to all boundary faces, apply local
!    transformations to try to remove interior vertex, i.e. get a
!    boundary-constrained triangulation formed from the given
!    boundary faces only.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NBF - number of boundary faces or triangles.
!
!    Input, NBPT - number of vertices (points) on boundary of convex hull.
!
!    Input, SIZHT - size of hash table HT.
!
!    Input, MAXFC - maximum size available for FC array.
!
!    Input/output, NFC - number of positions used in FC array; NFC <= MAXFC.
!
!    Input/output, NTETRA - number of tetrahedra in triangulation.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:NBPT+1) - indices of vertices of VCL being triangulated
!    where first NBPT are boundary points and last is interior.
!
!    Input/output, FC(1:7,1:NFC) - array of face records; see routine BCDTRI;
!    first NBPT columns are for boundary faces, rest are interior.
!
!    Input/output, HT(0:SIZHT-1) - hash table using direct chaining.
!
!    Output, FC(7,2) - HDAVFC : head pointer of avail list in FC.
!
!    Output, REMOV - .TRUE. iff interior point VM(NBPT+1) is removed.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxfc
  integer sizht
!
  integer a
  integer aa
  double precision alpha(4)
  integer b
  integer back
  integer bb
  integer c
  integer d
  logical degen
  integer e
  integer f
  integer fc(7,maxfc)
  integer front
  integer hdavfc
  integer ht(0:sizht-1)
  integer htsrc
  integer i
  integer ierr
  integer ind
  integer ind1
  integer ind2
  integer itet
  integer j
  integer kneg
  integer kzero
  integer, parameter :: msglvl = 0
  integer nbf
  integer nbpt
  integer nfc
  integer ntetra
  integer npt
  logical remov
  double precision vcl(3,*)
  integer va
  integer vb
  integer vc
  integer vd
  integer ve
  integer vi
  integer vm(nbpt+1)
!
!  Put all interior faces in queue for processing. I is index of
!  interior point. ITET is number of tetrahedra incident on I.
!
  ierr = 0

  if (msglvl == 4) then
    write ( *,650)
  end if

  npt = nbpt + 1
  i = npt
  vi = vm(i)
  hdavfc = 0
  itet = nbf
  if (itet == 4) go to 40
  front = nbf + 1

  do j = nbf+1,nfc-1
    fc(7,j) = j + 1
  end do

  fc(7,nfc) = 0
  back = nfc

20 continue

  if (front == 0) go to 40

  ind = front
  front = fc(7,ind)

  if (fc(2,ind) == 0) then

    if (ind == nfc) then
      nfc = nfc - 1
    else
      fc(1,ind) = -hdavfc
      hdavfc = ind
    end if

    go to 20

  end if

  fc(7,ind) = -1
  a = fc(1,ind)
  b = fc(2,ind)
  c = fc(4,ind)
  d = fc(5,ind)
  va = vm(a)
  vb = vm(b)
  vc = vm(c)
  vd = vm(d)

  if (msglvl == 4) then
    write ( *,600) ind,a,b,i,c,d
  end if

  call baryth(vcl(1,va),vcl(1,vb),vcl(1,vi),vcl(1,vc),vcl(1,vd), &
    alpha,degen)

  if (degen) then
    ierr = 301
    return
  else if (alpha(4) > 0.0d0) then
    ierr = 309
    return
  end if

  kneg = 1
  kzero = 0

  do j = 1,3
    if (alpha(j) < 0.0d0) then
      kneg = kneg + 1
    else if (alpha(j) == 0.0d0) then
      kzero = kzero + 1
    end if
  end do
!
!  Swap 2 tetrahedra for 3.
!
  if (kneg == 1 .and. kzero == 0) then

    call updatr(a,b,c,i,d,.false.,npt,sizht,front,back,fc,ht, ierr )
    call updatr(a,b,d,i,c,.false.,npt,sizht,front,back,fc,ht, ierr )
    call updatr(a,c,i,b,d,.true.,npt,sizht,front,back,fc,ht, ierr )
    call updatr(b,c,i,a,d,.true.,npt,sizht,front,back,fc,ht, ierr )
    call updatr(a,d,i,b,c,.true.,npt,sizht,front,back,fc,ht, ierr )
    call updatr(b,d,i,a,c,.true.,npt,sizht,front,back,fc,ht, ierr )

    if (ierr /= 0) return

    call htdel(ind,npt,sizht,fc,ht)
    call htins(ind,a,c,d,b,i,npt,sizht,fc,ht)
    call availf(hdavfc,nfc,maxfc,fc,ind,ierr)

    if (ierr /= 0) return

    call htins(ind,b,c,d,a,i,npt,sizht,fc,ht)
    call availf(hdavfc,nfc,maxfc,fc,ind,ierr)

    if (ierr /= 0) return
    call htins(ind,c,d,i,a,b,npt,sizht,fc,ht)
    ntetra = ntetra + 1

    if (msglvl == 4) then
      write ( *,610)
    end if
!
!  Relabel so that AI intersects BCD.
!
  else if (kneg == 2 .and. kzero == 0) then

    if (alpha(3) < 0.0d0) go to 20

    if (alpha(1) < 0.0d0) then
      call i_swap ( a, b )
    end if

    ind1 = htsrc(a,c,i,npt,sizht,fc,ht)

    if (ind1 <= 0) then
      ierr = 300
      return
    end if
!
!  Swap 3 tetrahedra for 2.
!
    if (fc(4,ind1) == d .or. fc(5,ind1) == d) then

      va = vm(a)
      vb = vm(b)
      call updatr(a,b,c,i,d,.false.,npt,sizht,front,back,fc,ht, ierr )
      call updatr(a,b,d,i,c,.false.,npt,sizht,front,back,fc,ht, ierr )
      call updatr(a,c,d,i,b,.false.,npt,sizht,front,back,fc,ht, ierr )
      call updatr(b,c,i,a,d,.true.,npt,sizht,front,back,fc,ht, ierr )
      call updatr(b,d,i,a,c,.true.,npt,sizht,front,back,fc,ht, ierr )
      call updatr(c,d,i,a,b,.true.,npt,sizht,front,back,fc,ht, ierr )

      if (ierr /= 0) return

      call htdel(ind,npt,sizht,fc,ht)
      call htins(ind,b,c,d,a,i,npt,sizht,fc,ht)
      call htdel(ind1,npt,sizht,fc,ht)

      if (fc(7,ind1) >= 0) then

        fc(2,ind1) = 0

      else

        if (ind1 == nfc) then
          nfc = nfc - 1
        else
          fc(1,ind1) = -hdavfc
          hdavfc = ind1
        end if

      end if

      ind1 = htsrc(a,d,i,npt,sizht,fc,ht)

      if (ind1 <= 0) then
        ierr = 300
        return
      end if

      call htdel(ind1,npt,sizht,fc,ht)

      if (fc(7,ind1) >= 0) then

        fc(2,ind1) = 0

      else

        if (ind1 == nfc) then
          nfc = nfc - 1
        else
          fc(1,ind1) = -hdavfc
          hdavfc = ind1
        end if

      end if

      ntetra = ntetra - 1
      itet = itet - 2

      if (msglvl == 4) then
        write ( *,620) b,c,d
      end if

    end if

  else if (kneg == 1 .and. kzero == 1) then

    if (alpha(3) == 0.0d0) go to 20
!
!  Relabel vertices so that BI intersects CD.
!
    if (alpha(2) == 0.0d0) then
      call i_swap ( a, b )
    end if

    ind1 = htsrc(b,c,i,npt,sizht,fc,ht)
    ind2 = htsrc(b,d,i,npt,sizht,fc,ht)

    if (ind1 <= 0 .or. ind2 <= 0) then
      ierr = 300
      return
    end if

    if (fc(4,ind1) == a) then
      e = fc(5,ind1)
    else
      e = fc(4,ind1)
    end if

    if (fc(4,ind2) == a) then
      f = fc(5,ind2)
    else
      f = fc(4,ind2)
    end if
!
!  Swap 4 tetrahedra for 4.
!
    if (e == f) then

      va = vm(a)
      vb = vm(b)
      ve = vm(e)
      call updatr(a,b,c,i,d,.false.,npt,sizht,front,back,fc,ht, ierr )
      call updatr(a,b,d,i,c,.false.,npt,sizht,front,back,fc,ht, ierr )
      call updatr(b,c,e,i,d,.false.,npt,sizht,front,back,fc,ht, ierr )
      call updatr(b,d,e,i,c,.false.,npt,sizht,front,back,fc,ht, ierr )
      call updatr(a,c,i,b,d,.true.,npt,sizht,front,back,fc,ht, ierr )
      call updatr(a,d,i,b,c,.true.,npt,sizht,front,back,fc,ht, ierr )
      call updatr(c,e,i,b,d,.true.,npt,sizht,front,back,fc,ht, ierr )
      call updatr(d,e,i,b,c,.true.,npt,sizht,front,back,fc,ht, ierr )

      if (ierr /= 0) return

      call htdel(ind,npt,sizht,fc,ht)
      call htins(ind,a,c,d,b,i,npt,sizht,fc,ht)
      ind = htsrc(b,e,i,npt,sizht,fc,ht)

      if (ind <= 0) then
        ierr = 300
        return
      end if

      call htdel(ind,npt,sizht,fc,ht)

      if (fc(7,ind) >= 0) then
        fc(2,ind) = 0
        call availf(hdavfc,nfc,maxfc,fc,ind,ierr)
        if (ierr /= 0) return
      end if

      call htins(ind,c,d,e,b,i,npt,sizht,fc,ht)
      call htdel(ind1,npt,sizht,fc,ht)

      if (fc(7,ind1) >= 0) then
        fc(2,ind1) = 0
        call availf(hdavfc,nfc,maxfc,fc,ind1,ierr)
        if (ierr /= 0) return
      end if

      call htins(ind1,b,c,d,a,e,npt,sizht,fc,ht)
      call htdel(ind2,npt,sizht,fc,ht)
      j = fc(7,ind2)
      call htins(ind2,c,d,i,a,e,npt,sizht,fc,ht)
      fc(7,ind2) = j

      if (fc(7,ind2) == -1) then

        fc(7,ind2) = 0

        if (front == 0) then
          front = ind2
        else
          fc(7,back) = ind2
        end if

        back = ind2

      end if

      itet = itet - 2

      if (msglvl == 4) then
        write ( *,630) b,i,c,d,e
      end if

    end if

  end if

  go to 20

40 continue

  if (msglvl >= 2) then
    write ( *,660) itet,nbf,nbpt
  end if

  if (itet /= 4) then
    remov = .false.
  else
!
!  Replace tetrahedra ABCI, ABDI, ACDI, BCDI by ABCD.
!
    remov = .true.
    ind = nbpt + 1

50  continue

    if (fc(3,ind) /= i .or. fc(1,ind) <= 0) then
      ind = ind + 1
      go to 50
    end if

    a = fc(1,ind)
    b = fc(2,ind)
    c = fc(4,ind)
    d = fc(5,ind)
    call updatr(a,b,c,i,d,.false.,npt,sizht,front,back,fc,ht, ierr )
    call updatr(a,b,d,i,c,.false.,npt,sizht,front,back,fc,ht, ierr )
    call updatr(a,c,d,i,b,.false.,npt,sizht,front,back,fc,ht, ierr )
    call updatr(b,c,d,i,a,.false.,npt,sizht,front,back,fc,ht, ierr )
    if (ierr /= 0) return
    aa = a
    bb = c

    do j = 1,6

      call htdel(ind,npt,sizht,fc,ht)

      if (ind == nfc) then
        nfc = nfc - 1
      else
        fc(1,ind) = -hdavfc
        hdavfc = ind
      end if

      if (j == 6) then
        cycle
      end if

      if (j == 2) then
        bb = d
      else if (j == 3) then
        aa = b
      else if (j == 4) then
        bb = c
      else if (j == 5) then
        aa = d
      end if

      ind = htsrc(aa,bb,i,npt,sizht,fc,ht)

      if (ind <= 0) then
        ierr = 300
        return
      end if

    end do

    ntetra = ntetra - 3

    if (msglvl == 4) then
      write ( *,640) i,a,b,c,d
    end if

  end if

  fc(7,2) = hdavfc
  return

  600 format (1x,'index =',i7,' : ',5i7)
  610 format (4x,'swap 2-3')
  620 format (4x,'swap 3-2 with new common face:',3i7)
  630 format (4x,'swap 4-4: edge ',2i7,' repl by ',2i7,'   e =',i7)
  640 format (1x,'delete ',i7,' from tetra ',4i7)
  650 format (/1x,'swprem')
  660 format (1x,'swprem: itet,nbf,nbpt=',3i5)

  return
end
subroutine tetlst ( nfc, vm, fc, nt, tetra )
!
!******************************************************************************
!
!! TETLST constructs a list of tetrahedra from the FC array.
!
!
!  Purpose: 
!
!    Construct list of tetrahedra from FC array. Global vertex
!    indices from VM are produced. The vertex indices for each
!    tetrahedron are sorted in increasing order.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NFC - number of positions used in FC array.
!
!    Input, VM(1:*) - indices of vertices of VCL that are triangulated.
!
!    Input, FC(1:7,1:NFC) - array of face records; see routine DTRIS3.
!
!    Output, NT - number of tetrahedra.
!
!    Output, TETRA(1:4,1:NT) - contains global tetrahedron indices; it is
!    assumed there is enough space.
!
  implicit none
!
  integer nfc
!
  integer a
  integer fc(7,nfc)
  integer i
  integer j
  integer k
  integer l
  integer nt
  integer t(4)
  integer tetra(4,*)
  integer vm(*)
!
  nt = 0

  do i = 1,nfc

    if (fc(1,i) <= 0)  then
      cycle
    end if

    do k = 4,5
      if (fc(k,i) > fc(3,i)) then
        nt = nt + 1
        tetra(1,nt) = fc(1,i)
        tetra(2,nt) = fc(2,i)
        tetra(3,nt) = fc(3,i)
        tetra(4,nt) = fc(k,i)
      end if
    end do

  end do

  do k = 1,nt

    t(1:4) = vm(tetra(1:4,k))

    do i = 1,3
      l = i
      do j = i+1,4
        if (t(j) < t(l)) l = j
      end do
      a = t(i)
      t(i) = t(l)
      t(l) = a
    end do

    tetra(1:4,k) = t(1:4)

  end do

  return
end
function tetmu ( crit, a, b, c, d, s )
!
!******************************************************************************
!
!! TETMU computes a tetrahedron shape measure.
!
!
!  Purpose: 
!
!    Compute tetrahedron shape measure, scaled so that
!    largest possible value is 1.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, CRIT - criterion number: 1 for sin(min solid angle / 2),
!    2 for radius ratio, 3 for mean ratio.
!
!    Input, A(1:3), B(1:3), C(1:3), D(1:3) - 4 vertices of tetrahedron
!
!    Workspace, S(1:4) - needed for SANGMN routine
!
!    Output, TETMU - SANGMN(ABCD)*SF, RADRTH(ABCD), or EMNRTH(ABCD)
!    if CRIT = 1, 2, or 3 respectively.; else 1.0
!
  implicit none
!
  double precision a(3)
  double precision b(3)
  double precision c(3)
  integer crit
  double precision d(3)
  double precision emnrth
  double precision radrth
  double precision s(4)
  double precision sangmn
  double precision, parameter :: sf = 3.674234614174767D+00
  double precision tetmu
!
  if ( crit == 1 ) then
    tetmu = sangmn(a,b,c,d,s)*sf
  else if (crit == 2) then
    tetmu = radrth(a,b,c,d)
  else if (crit == 3) then
    tetmu = emnrth(a,b,c,d)
  else
    tetmu = 1.0d0
  end if

  return
end
subroutine tetsiz ( ntetd, npolh, facep, hfl, pfl, vol, psi, h, indp, loch )
!
!******************************************************************************
!
!! TETSIZ smooths PSI values and computes tetrahedron sizes.
!
!
!  Purpose: 
!
!    Smooth PSI (mean mesh distribution function) values using
!    heap so that they differ by a factor of at most 8 in adjacent
!    polyhedra, then compute tetrahedron sizes for each polyhedron.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NTETD - desired number of tetrahedra in mesh.
!
!    Input, NPOLH - number of polyhedra or positions used in HFL array.
!
!    Input, FACEP(1:3,1:*) - face pointer list.
!
!    Input, HFL(1:NPOLH) - head pointer to face indices in PFL for each
!    polyhedron.
!
!    Input, PFL(1:2,1:*) - list of signed face indices for each polyhedron.
!
!    Input, VOL(1:NPOLH) - volume of convex polyhedra in decomposition.
!
!    Input/output, PSI(1:NPOLH) - mean mdf values in the convex polyhedra.
!    On output, the values have been smoothed.
!
!    Output, H(1:NPOLH) - tetrahedron size for convex polyhedra.
!
!    Workspace, INDP(1:NPOLH) - indices of polyhedron or PSI which are
!    maintained in heap according to PSI values.
!
!    Workspace, LOCH(1:NPOLH) - location of polyhedron indices in heap.
!
  implicit none
!
  integer npolh
!
  integer f
  integer facep(3,*)
  double precision factor
  double precision h(npolh)
  integer hfl(npolh)
  integer i
  integer indp(npolh)
  integer j
  integer k
  integer l
  integer loch(npolh)
  integer ntetd
  integer pfl(2,*)
  double precision power
  double precision psi(npolh)
  integer r
  double precision sum2
  double precision vol(npolh)
!
  factor = 0.125d0

  do i = 1,npolh
    indp(i) = i
    loch(i) = i
  end do

  k = int(npolh/2)

  do l = k,1,-1
    call sfdwmf(l,npolh,psi,indp,loch)
  end do

  do r = npolh,2,-1

    j = indp(1)
    indp(1) = indp(r)
    loch(indp(1)) = 1
    call sfdwmf(1,r-1,psi,indp,loch)
    i = hfl(j)

30  continue

    f = abs(pfl(1,i))

    if (abs(facep(2,f)) == j) then
      k = abs(facep(3,f))
    else
      k = abs(facep(2,f))
    end if

    if (k > 0) then

      if (psi(k) < psi(j)*factor) then
        psi(k) = psi(j)*factor
        call sfupmf(loch(k),psi,indp,loch)
      end if

    end if

    i = pfl(2,i)

    if (i /= hfl(j)) go to 30

  end do

  sum2 = dot_product ( psi(1:npolh), vol(1:npolh) )

  factor = 6.0d0 / dble(ntetd)
  power = 1.0d0 / 3.0d0

  do i = 1, npolh
    psi(i) = psi(i) / sum2
    h(i) = ( factor / psi(i) )**power
  end do

  return
end
subroutine timestamp ( )
!
!*******************************************************************************
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!
!  Example:
!
!    May 31 2001   9:45:54.872 AM
!
!  Modified:
!
!    31 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none
!
  character ( len = 8 ) ampm
  integer d
  character ( len = 8 ) date
  integer h
  integer m
  integer mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer n
  integer s
  character ( len = 10 )  time
  integer values(8)
  integer y
  character ( len = 5 ) zone
!
  call date_and_time ( date, time, zone, values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    trim ( month(m) ), d, y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
subroutine tmerge ( inter, nbl, ncr, chbl, chcr, ldv, vcl, til, tedg, &
  ierror )
!
!*******************************************************************************
!
!! TMERGE forms triangles near the boundary by merging vertex chains.
!
!
!  Purpose:
!
!    Form triangles in strip near boundary of polygon or
!    inside polygon by merging two chains of vertices.
!
!  Modified:
!
!    12 July 1999
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, logical INTER, is .TRUE. iff at least one interior mesh vertex.
!
!    Input, integer NBL, the number of vertices on boundary cycle if INTER,
!    otherwise on left boundary chain.
!
!    Input, integer NCR, the number of vertices on closed walk if INTER,
!    otherwise on right boundary chain.
!
!    Input, integer CHBL(0:NBL), the indices in VCL of vertices on boundary
!    cycle or left boundary chain; if INTER, CHBL(NBL) = CHBL(0).
!
!    Input, integer CHCR(0:NCR), the indices in VCL of vertices on closed walk
!    or right boundary chain; if INTER, CHCR(NCR) = CHCR(0),
!    otherwise CHCR(0) is not referenced.
!
!    Input, integer LDV, the leading dimension of VCL in calling routine.
!
!    Input, double precision VCL(1:2,1:*), the vertex coordinate list.
!
!    Output, integer TIL(1:3,1:NT), the triangle incidence list, where NT =
!    NBL + NCR - K where K = 0 if INTER, else K = 2.
!
!    Output, integer TEDG(1:3,1:NT), the TEDG(J,I) refers to edge with vertices
!    TIL(J:J+1,I) and contains index of merge edge or NBL+NCR+1 for edge of
!    chains.  Note: It is assumed there is enough space in 2 arrays.
!
!    Output, integer IERROR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer ldv
  integer nbl
  integer ncr
!
  integer chbl(0:nbl)
  integer chcr(0:ncr)
  integer diaedg
  integer i
  integer ibndry
  integer ierror
  integer in
  logical inter
  integer j
  integer lri
  integer lrip1
  integer lrline
  integer nl
  integer nr
  integer nt
  integer tedg(3,nbl+ncr)
  integer til(3,nbl+ncr)
  double precision vcl(ldv,*)
  double precision xi
  double precision xip1
  double precision xj
  double precision xjp1
  double precision yi
  double precision yip1
  double precision yj
  double precision yjp1
!
  ibndry = nbl + ncr + 1
  nt = 0

  if ( inter ) then

    nl = nbl
    nr = ncr
    i = 0
    j = 0

  else

    call mtredg ( .true., chbl(1), chcr(1), chbl(0), ibndry, nt, til, tedg )

    tedg(2,1) = ibndry
    if ( nbl + ncr <= 3 ) then
      return
    end if

    nl = nbl - 1
    nr = ncr - 1
    i = 1
    j = 1
    lri = 1
    lrip1 = 1

  end if
!
!  Main while loop for determining next triangle and edge.
!
10 continue

  if ( i >= nl .or. j >= nr ) then
    go to 20
  end if

  xi = vcl(1,chbl(i))
  yi = vcl(2,chbl(i))
  xip1 = vcl(1,chbl(i+1))
  yip1 = vcl(2,chbl(i+1))
  xj = vcl(1,chcr(j))
  yj = vcl(2,chcr(j))
  xjp1 = vcl(1,chcr(j+1))
  yjp1 = vcl(2,chcr(j+1))
  in = diaedg ( xjp1, yjp1, xj, yj, xi, yi, xip1, yip1 )

  if ( inter ) then
    lri = lrline ( xi, yi, xj, yj, xjp1, yjp1, 0.0D+00 )
    lrip1 = lrline ( xip1, yip1, xj, yj, xjp1, yjp1, 0.0D+00 )
  end if

  if ( in <= 0 .or. lri <= 0 .and. lrip1 <= 0 ) then

    call mtredg ( .true., chbl(i+1), chcr(j), chbl(i), ibndry, nt, til, tedg )
    i = i + 1

  else

    call mtredg ( .false., chbl(i), chcr(j+1), chcr(j), ibndry, nt, til, tedg )
    j = j + 1

  end if

  go to 10
!
!  Add remaining triangles at end of strip or bottom of polygon.
!
   20 continue

  if ( i < nl ) then

    if ( .not. inter .and. j == nr ) then
      nl = nl + 1
    end if

    do

      call mtredg ( .true., chbl(i+1), chcr(j), chbl(i), ibndry, nt, til, tedg )
      i = i + 1

      if ( i >= nl ) then
        exit
      end if

    end do

  else
!
!  J < NR .OR. I = NL = J = NR = 1
!
    if ( .not. inter .and. i == nl ) then
      nr = nr + 1
    end if

40  continue

    call mtredg ( .false., chbl(i), chcr(j+1), chcr(j), ibndry, nt, til, tedg )

    if ( inter ) then

      lri = lrline ( vcl(1,chbl(i)), vcl(2,chbl(i)), &
        vcl(1,chcr(j+1)), vcl(2,chcr(j+1)), vcl(1,chcr(j)), &
        vcl(2,chcr(j)), 0.0D+00 )

      if ( lri >= 0 ) then
        ierror = 230
        return
      end if

    end if

    j = j + 1

    if ( j < nr ) then
      go to 40
    end if

  end if

  if ( inter ) then
    if ( tedg(2,1) == 0 ) then
      tedg(2,1) = nbl + ncr
    else
      tedg(3,1) = nbl + ncr
    end if
  end if

  return
end
subroutine tribfc ( h, nvc, nface, nvert, maxvc, maxbt, maxiw, maxwk, vcl, &
  facep, nrml, fvl, edst, edno, fcst, btst, btl, iwk, wk, ierr )
!
!******************************************************************************
!
!! TRIBFC generates a Delaunay triangulation on polyhedron boundary faces.
!
!
!  Purpose: 
!
!    Generate 2D Delaunay triangulations on convex boundary
!    faces of convex polyhedra in polyhedral decomposition data
!    structure according to mesh spacings in H array.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, H(1:NPOLH) - mesh spacings for NPOLH polyhedron in data structure.
!
!    Input/output, NVC - number of vertex coordinates or positions used in VCL.
!
!    Input, NFACE - number of faces or positions used in FACEP array.
!
!    Input, NVERT - number of positions used in FVL array.
!
!    Input, MAXVC - maximum size available for VCL array.
!
!    Input, MAXBT - maximum size available for BTL array; should be >=
!    number of triangles generated on boundary faces.
!
!    Input, MAXIW - maximum size available for IWK array; should be >=
!    NBC + 1 + (amount needed in routine TRPOLG), where NBC
!    is largest number of mesh vertices on boundary of a face.
!
!    Input, MAXWK - maximum size available for WK array; should be >=
!    2*NVRT + 2 + (amount needed in routine TRPOLG), where
!    NVRT is largest number of vertices of a face.
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.  On output, 
!    updated by mesh vertices generated on boundary faces.
!
!    Input, FACEP(1:3,1:NFACE) - face pointer list.
!
!    Input, NRML(1:3,1:NFACE) - unit normal vectors for faces.
!
!    Input, FVL(1:6,1:NVERT) - face vertex list.
!
!    Input, EANG(1:NVERT) - edge angles.
!
!    Output, EDST(1:NVERT) - start location in VCL for mesh vertices on each
!    edge in FVL if there are any, else 0.
!
!    Output, EDNO(1:NVERT) - no. of mesh vertices on interior of each edge
!    in FVL; entry is negated if mesh vertices are listed in
!    backward order (according to SUCC) in VCL.
!
!    Output, FCST(1:NFACE+1) - start location in VCL for mesh vertices in
!    interior of each face; last entry indicates where mesh
!    vertices in interior of polyhedra start.
!
!    Output, BTST(1:NFACE+1) - start location in BTL for triangles on each
!    face of FACEP; last entry is (total number of triangles + 1).
!
!    Output, BTL(1:3,1:NBT) - boundary triangle list for triangles generated
!    on all faces of decomposition; NBT = BTST(NFACE+1) - 1;
!    entries are indices of VCL.
!
!    Workspace, IWK(1:3,1:MAXIW).
!
!    Workspace, WK(1:MAXWK),
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxbt
  integer maxiw
  integer maxwk
  integer nface
  integer nvc
  integer nvert
!
  integer a
  logical bflag
  integer btl(3,maxbt)
  integer btst(nface+1)
  double precision cxy
  double precision cyz
  double precision dotp
  double precision dx
  double precision dy
  double precision dz
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer edno(nvert)
  integer edst(nvert)
  integer f
  integer facep(3,nface)
  integer, parameter :: facn = 2
  integer fcst(nface+1)
  integer fvl(6,nvert)
  double precision h(*)
  double precision hh
  integer i
  integer ierr
  integer iwk(maxiw)
  integer j
  integer k
  double precision leng
  double precision leng1
  integer li
  integer lj
  integer, parameter :: loc = 1
  integer maxvc
  integer n
  integer nbc
  integer nmv
  double precision nrm1
  double precision nrm2
  double precision nrm3
  double precision nrml(3,nface)
  integer ntri
  integer nvcb
  integer p
  integer, parameter :: pred = 4
  integer q
  integer r
  double precision r21
  double precision r22
  double precision r31
  double precision r32
  integer s
  integer start
  integer, parameter :: succ = 3
  double precision sxy
  double precision syz
  double precision tol
  double precision u(3)
  double precision v(3)
  double precision vcl(3,nvc)
  double precision wk(maxwk)
  integer xc
  double precision xt
  integer yc
  double precision yt
  double precision zr
!
!  Generate mesh vertices on interior of edges of decomposition.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  edst(1:nvert) = -1

  do i = 1,nvert

    if (edst(i) /= -1) then
      cycle
    end if

    a = i
!
!  Determine whether edge is on the boundary of decomposition.
!
20  continue

    j = fvl(edga,a)

    if (j == 0) then
      bflag = .true.
      f = fvl(facn,a)
      p = abs(facep(2,f))
      hh = h(p)
      n = 1
      go to 30
    else if (j == i) then
      bflag = .false.
      a = i
      f = fvl(facn,a)
      p = abs(facep(2,f))
      q = abs(facep(3,f))
      hh = h(p)*h(q)
      n = 2
      go to 30
    end if

    a = j
    go to 20
!
!  Compute mesh spacing for edge from polyhedra containing edge.
!
30  continue

    j = fvl(edgc,a)

40  continue

    k = fvl(edgc,j)

    if (k /= 0 .and. k /= a) then

      f = fvl(facn,j)
      r = abs(facep(2,f))
      s = abs(facep(3,f))

      if (.not. bflag .and. n == 2) then

        if (r == p .or. r == q) then
          p = s
        else
          p = r
        end if

      else

        if (r == p) then
          p = s
        else
          p = r
        end if

      end if

      hh = hh*h(p)
      n = n + 1

    end if

    j = k
    if (j /= 0 .and. j /= a) go to 40
!
!  Generate mesh vertices on edge and set EDST, EDNO entries.
!
    hh = hh**(1.0d0/dble(n))
    li = fvl(loc,i)
    lj = fvl(loc,fvl(succ,i))
    dx = vcl(1,lj) - vcl(1,li)
    dy = vcl(2,lj) - vcl(2,li)
    dz = vcl(3,lj) - vcl(3,li)
    leng = sqrt(dx**2 + dy**2 + dz**2)
    n = int(leng/hh)
    if (leng/hh - n > dble(n)/dble(2*n+1)) n = n + 1
    n = max(0,n-1)
    start = 0

    if (n >= 1) then

      start = nvc + 1
      dx = dx/dble(n+1)
      dy = dy/dble(n+1)
      dz = dz/dble(n+1)

      if (nvc + n > maxvc) then
        ierr = 14
        return
      end if

      do k = 1,n
        nvc = nvc + 1
        vcl(1,nvc) = vcl(1,li) + dble(k)*dx
        vcl(2,nvc) = vcl(2,li) + dble(k)*dy
        vcl(3,nvc) = vcl(3,li) + dble(k)*dz
      end do

    end if

    j = a

60  continue

    edst(j) = start
    lj = fvl(loc,j)

    if (lj == li) then
      edno(j) = n
    else
      edno(j) = -n
    end if

    j = fvl(edgc,j)
    if (j /= 0 .and. j /= a) go to 60

  end do
!
!  Generate Delaunay triangulation on each face of decomposition.
!
  ntri = 0

  do f = 1,nface

    p = abs(facep(2,f))
    q = abs(facep(3,f))

    if (q == 0) then
      hh = h(p)
    else
      hh = sqrt(h(p)*h(q))
    end if

    if (facep(2,f) > 0) then
      nrm1 = nrml(1,f)
      nrm2 = nrml(2,f)
      nrm3 = nrml(3,f)
    else
      nrm1 = -nrml(1,f)
      nrm2 = -nrml(2,f)
      nrm3 = -nrml(3,f)
    end if

    i = facep(1,f)
    li = fvl(loc,i)
    zr = nrm1*vcl(1,li) + nrm2*vcl(2,li) + nrm3*vcl(3,li)
!
!  Equation of face is NRM1*X + NRM2*Y + NRM3*Z = ZR.
!  Rotate normal vector to (0,0,1). Rotation matrix is:
!    [ CXY     -SXY     0   ]
!    [ CYZ*SXY CYZ*CXY -SYZ ]
!    [ SYZ*SXY SYZ*CXY  CYZ ]
!
    if (abs(nrm1) <= tol) then
      leng = nrm2
      cxy = 1.0d0
      sxy = 0.0d0
    else
      leng = sqrt(nrm1**2 + nrm2**2)
      cxy = nrm2/leng
      sxy = nrm1/leng
    end if

    cyz = nrm3
    syz = leng
    r21 = cyz*sxy
    r22 = cyz*cxy
    r31 = nrm1
    r32 = nrm2
!
!  Rotate mesh vertices on boundary of face.
!
    n = 0

80  continue

    n = n + 1
    i = fvl(succ,i)
    if (i /= facep(1,f)) go to 80

    if (n + n + 2 > maxwk) then
      ierr = 7
      return
    end if

    xc = 1
    yc = n + 2
    nvcb = nvc
    n = 0
    i = facep(1,f)
    li = fvl(loc,i)
    lj = fvl(loc,fvl(pred,i))
    u(1) = vcl(1,lj) - vcl(1,li)
    u(2) = vcl(2,lj) - vcl(2,li)
    u(3) = vcl(3,lj) - vcl(3,li)
    leng = u(1)**2 + u(2)**2 + u(3)**2

90  continue

    nvcb = nvcb + 1
    vcl(1,nvcb) = cxy*vcl(1,li) - sxy*vcl(2,li)
    vcl(2,nvcb) = r21*vcl(1,li) + r22*vcl(2,li) - syz*vcl(3,li)
    vcl(3,nvcb) = li
    j = fvl(succ,i)
    lj = fvl(loc,j)
    v(1) = vcl(1,lj) - vcl(1,li)
    v(2) = vcl(2,lj) - vcl(2,li)
    v(3) = vcl(3,lj) - vcl(3,li)
    leng1 = v(1)**2 + v(2)**2 + v(3)**2
    dotp = (u(1)*v(1) + u(2)*v(2) + u(3)*v(3))/sqrt(leng*leng1)

    if (dotp > -1.0d0 + tol) then
      wk(xc+n) = vcl(1,nvcb)
      wk(yc+n) = vcl(2,nvcb)
      n = n + 1
    end if

    if (edno(i) >= 0) then
      k = edst(i)
      s = 1
    else
      k = edst(i) - edno(i) - 1
      s = -1
    end if

    do r = 1,abs(edno(i))
      nvcb = nvcb + 1
      vcl(1,nvcb) = cxy*vcl(1,k) - sxy*vcl(2,k)
      vcl(2,nvcb) = r21*vcl(1,k) + r22*vcl(2,k) - syz*vcl(3,k)
      vcl(3,nvcb) = k
      k = k + s
    end do

    i = j

    if (i /= facep(1,f)) then
      li = lj
      u(1) = -v(1)
      u(2) = -v(2)
      u(3) = -v(3)
      leng = leng1
      go to 90
    end if

    wk(xc+n) = wk(xc)
    wk(yc+n) = wk(yc)
    nmv = nvcb
    nbc = nvcb - nvc

    if (nbc + 1 > maxiw) then
      ierr = 6
      return
    end if

    do i = 1,nbc
      iwk(i) = nvc + i
    end do

    iwk(nbc+1) = iwk(1)
    s = ntri + 1
    btst(f) = s
    fcst(f) = nvc + 1

    call trpolg(n,wk(xc),wk(yc),hh,nbc,iwk,3,nmv,ntri,maxvc,maxbt, &
      maxiw-nbc-1,maxwk-yc-n,vcl,btl,iwk(nbc+2),wk(yc+n+1), ierr )

    if (ierr == 3) ierr = 14
    if (ierr == 9) ierr = 19
    if (ierr /= 0) return
!
!  Fix up indices of BTL and rotate interior mesh vertices.
!
    do i = s,ntri
      do j = 1,3
        r = btl(j,i)
        if (r <= nvcb) then
          btl(j,i) = vcl(3,r)
        else
          btl(j,i) = r - nbc
        end if
      end do
    end do

    nmv = nmv - nvcb

    do i = 1,nmv
      xt = vcl(1,nvcb+i)
      yt = vcl(2,nvcb+i)
      vcl(1,nvc+i) = cxy*xt + r21*yt + r31*zr
      vcl(2,nvc+i) = r22*yt - sxy*xt + r32*zr
      vcl(3,nvc+i) = cyz*zr - syz*yt
    end do

    nvc = nvc + nmv

  end do

  btst(nface+1) = ntri + 1
  fcst(nface+1) = nvc + 1

  return
end
subroutine tripr3 ( h, shrf, crit, nvc, nface, nvert, npolh, maxvc, maxbt, &
  maxfc, maxht, maxvm, maxiw, maxwk, vcl, facep, nrml, fvl, eang, hfl, pfl, &
  edst, edno, fcst, btst, btl, fc, ht, vm, xfhv, ntrif, ntetra, iwk, wk, ierr )
!
!******************************************************************************
!
!! TRIPR3 generates mesh vertices in a decomposed polygonal region.
!
!
!  Purpose: 
!
!    Generate and triangulate mesh vertices in a polyhedral
!    region which has been decomposed into convex polyhedra with
!    convex faces.  In P-th convex polyhedron, the mesh vertices are
!    generated on a quasi-uniform grid of spacing H(P). The initial
!    triangulation in each polyhedron is boundary-constrained based
!    on local empty circumsphere criterion (often Delaunay), and
!    this can be improved by local transformations based on max-min
!    solid angle or ratio of inradius to circumradius criterion.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, H(1:NPOLH) - mesh spacings for convex polyhedra in polyhedral
!    decomposition data structure.
!
!    Input, SHRF - factor for shrinking polyhedron; shrink distance is
!    SHRF*H(P); a value of about 0.8 for SHRF is recommended.
!
!    Input, CRIT - criterion code for improving b-c triangulations: 1 for
!    (local max-min) solid angle criterion, 2 for radius ratio
!    criterion, 3 for mean ratio crit, else no improvement.
!
!    Input/output, NVC - number of vertex coordinates or positions used in VCL.
!
!    Input, NFACE - number of faces or positions used in FACEP array.
!
!    Input, NVERT - number of positions used in FVL, EANG array.
!
!    Input, NPOLH - number of polyhedra or positions used in HFL array.
!
!    Input, MAXVC - maximum size available for VCL array; should be large
!    enough to hold all mesh vertices.
!
!    Input, MAXBT - maximum size available for BTL array; should be >=
!    number of triangles generated on boundary faces.
!
!    Input, MAXFC - maximum size available for FC array; should be >= sum
!    of number of faces in triangulation of each polyhedron.
!
!    Input, MAXHT - maximum size available for HT array; should be >=
!    3/2 * MAXVM.
!
!    Input, MAXVM - maximum size available for VM array; should be >= sum
!    of number of mesh vertices in each polyhedron.
!
!    Input, MAXIW - maximum size available for IWK array; should be >=
!    NVCF + 2*NCFACE + 17*NCVERT where NVCF is number of mesh
!    vertices on all faces of decomposition (excluding those
!    in interior of polyhedra), NCFACE is max number of faces
!    in a polyhedron, NCVERT is 2 * max number of edges in a polyhedron.
!
!    Input, MAXWK - maximum size available for WK array; should be >=
!    3*NCFACE + 8*NCVERT.
!
!    Input/output, VCL(1:3,1:NVC) - vertex coordinate list.
!
!    Input, FACEP(1:3,1:NFACE) - face pointer list: row 1 is head pointer,
!    rows 2 and 3 are signed polyhedron indices.
!
!    Input, NRML(1:3,1:NFACE) - unit normal vectors for faces; outward
!    normal corresponds to counter clockwise traversal of face 
!    from polyhedron with index |FACEP(2,F)|.
!
!    Input, FVL(1:6,1:NVERT) - face vertex list; see routine DSPHDC.
!
!    Input, EANG(1:NVERT) - angles at edges common to 2 faces in a polyhedron;
!    EANG(J) corresponds to FVL(*,J), determined by EDGC field.
!
!    Input, HFL(1:NPOLH) - head pointer to face indices in PFL for each
!    polyhedron.
!
!    Input, PFL(1:2,1:*) - list of signed face indices for each polyhedron;
!    row 2 used for link.
!
!    Output, EDST(1:NVERT) - start location in VCL for mesh vertices on each
!    edge in FVL if there are any, else 0.
!
!    Output, EDNO(1:NVERT) - no. of mesh vertices on interior of each edge
!    in FVL; entry is negated if mesh vertices are listed in
!    backward order (according to SUCC) in VCL.
!
!    Output, FCST(1:NFACE+1) - start location in VCL for mesh vertices in
!    interior of each face; last entry indicates where mesh
!    vertices in interior of polyhedra start.
!
!    Output, BTST(1:NFACE+1) - start location in BTL for triangles on each
!    face of FACEP; last entry is (total number of triangles + 1).
!
!    Output, BTL(1:3,1:NBT) - boundary triangle list for triangles generated
!    on all faces of decomposition; NBT = BTST(NFACE+1) - 1;
!    entries are indices of VCL.
!
!    Output, FC(1:7,1:MAXFC), HT(1:MAXHT), VM(1:MAXVM) - face record array,
!    hash table, and vertex mapping array for triangulations
!    in each convex polyhedron (see routine BCDTRI).
!
!    Output, XFHV(1:3,1:NPOLH+1) - indicates usage of FC, HT, VM arrays for
!    polyhedron P; space used in FC for triangulation in P is from
!    FC(*,J:K) where J=XFHV(1,P) and K=XBFHV(1,P+1)-1; space
!    used for HT, VM are similar using first index 2, 3.
!
!    Output, NTRIF(1:NPOLH) - number of triangular faces in triangulation 
!    of each polyhedron.
!
!    Output, NTETRA(1:NPOLH) - number of tetrahedra in triangulation of 
!    each polyhedron.
!
!    Workspace, IWK(1:MAXIW).
!
!    Worksapce, WK(1:MAXWK).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxbt
  integer maxfc
  integer maxht
  integer maxiw
  integer maxvc
  integer maxvm
  integer maxwk
  integer nface
  integer npolh
  integer nvert
!
  integer btst(nface+1)
  integer btl(3,maxbt)
  integer ceang
  integer cfvl
  integer chvl
  integer cnrml
  integer crit
  double precision ctrx
  double precision ctry
  double precision ctrz
  double precision diamsq
  integer drem
  double precision eang(nvert)
  integer, parameter :: edga = 5
  integer, parameter :: edgc = 6
  integer edno(nvert)
  integer edst(nvert)
  integer f
  integer facep(3,nface)
  integer, parameter :: facn = 2
  integer fc(7,maxfc)
  integer fcst(nface+1)
  integer fvl(6,nvert)
  integer g
  double precision h(npolh)
  integer hfl(npolh)
  integer ht(maxht)
  integer i
  integer ibot
  integer ierr
  integer ifc
  integer irem
  integer itop
  integer ivm
  integer iwk(maxiw)
  integer j
  integer jv
  integer k
  integer kfc
  integer kht
  integer kvm
  integer l
  integer lj
  integer lk
  integer, parameter :: loc = 1
  integer lv
  integer maxsv
  integer n
  integer nbf
  integer nbmv
  integer ncface
  integer ncvert
  integer nfc
  integer nimv
  integer npt
  double precision nrml(3,nface)
  integer nsface
  integer nsvc
  integer nsvert
  integer ntetra(npolh)
  integer ntrif(npolh)
  integer nvc
  integer p
  integer pfl(2,*)
  integer, parameter :: pred = 4
  integer prime
  logical rflag
  integer sf
  integer sfvl
  double precision shrf
  integer shvl
  integer sizht
  integer, parameter :: succ = 3
  integer svcl
  double precision tol
  double precision vcl(3,maxvc)
  integer vm(maxvm)
  double precision wk(maxwk)
  integer xfhv(3,npolh+1)
!
!  Determine NCFACE = maximum number of faces in a polyhedron and NCVERT = 2 *
!  maximum number of edges in a polyhedron.  Array FCST is temporarily used.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  ncface = 0
  ncvert = 0

  do f = 1,nface
    k = 0
    i = facep(1,f)
10  continue
    k = k + 1
    i = fvl(succ,i)
    if (i /= facep(1,f)) go to 10
    fcst(f) = k
  end do

  do p = 1,npolh
    j = 0
    k = 0
    i = hfl(p)
30  continue
    f = abs(pfl(1,i))
    j = j + 1
    k = k + fcst(f)
    i = pfl(2,i)
    if (i /= hfl(p)) go to 30
    ncface = max(ncface,j)
    ncvert = max(ncvert,k)
  end do
!
!  Generate 2D Delaunay triangulations on convex boundary faces
!  of polyhedra.  Then generate interior mesh vertices and tetrahedra
!  for each convex polyhedron.  Start of IWK is used for mapping
!  global vertex indices to local indices for a polyhedron.
!
  call tribfc(h,nvc,nface,nvert,maxvc,maxbt,maxiw,maxwk,vcl,facep, &
     nrml,fvl,edst,edno,fcst,btst,btl,iwk,wk, ierr )

  if (ierr /= 0) return
  chvl = nvc + 1
  cfvl = chvl + ncface
  irem = cfvl + 5*ncvert
  cnrml = 1
  ceang = cnrml + 3*ncface
  drem = ceang + ncvert

  if (irem - 1 > maxiw) then
    ierr = 6
    return
  else if (drem - 1 > maxwk) then
    ierr = 7
    return
  end if

  iwk(1:nvc) = 0
  kfc = 0
  kht = 0
  kvm = 0
  xfhv(1,1) = 1
  xfhv(2,1) = 1
  xfhv(3,1) = 1
  rflag = .true.

  do p = 1,npolh

    call dsconv(p,hfl(p),facep,nrml,fvl,eang,pfl,ncface,ncvert, &
      iwk(chvl),wk(cnrml),iwk(cfvl),wk(ceang))

    irem = cfvl + 5*ncvert

    if (irem + ncvert - 1 > maxiw) then
      ierr = 6
      return
    end if

    call rmcpfc(ncface,ncvert,iwk(chvl),wk(cnrml),iwk(cfvl), &
      wk(ceang),iwk(irem))

    call rmcled(ncface,ncvert,iwk(chvl),iwk(cfvl))
    maxsv = 2*ncvert
    shvl = cfvl + 5*ncvert
    sfvl = shvl + ncface
    irem = sfvl + 5*maxsv
    svcl = ceang + ncvert
    drem = svcl + 3*maxsv

    if (irem > maxiw) then
      ierr = 6
      return
    else if (drem > maxwk) then
      ierr = 7
      return
    end if

    call shrnk3(shrf*h(p),ncface,vcl,iwk(chvl),wk(cnrml),iwk(cfvl), &
      wk(ceang),maxsv,maxiw-irem+1,maxwk-drem+1,nsvc,nsface, &
      nsvert,wk(svcl),iwk(shvl),iwk(sfvl),iwk(irem),wk(drem), ierr )

    if (ierr == 13) ierr = 330
    if (ierr /= 0) return

    if (nsface >= 4) then
      call diam3(nsvc,wk(svcl),ibot,itop,diamsq)
      drem = svcl + 3*nsvc
      call intmvg(nsvc,nsface,nsvert,wk(svcl),iwk(shvl),iwk(sfvl), &
        ibot,itop,h(p),maxvc-nvc,maxwk-drem+1,nimv,vcl(1,nvc+1), &
        wk(drem), ierr )
      if (ierr /= 0) return
      nvc = nvc + nimv
    else
      nimv = 0
    end if

    nbmv = 0
    nbf = 0
!
!  Add vertices of convex polyhedron to VM.
!
    ivm = kvm
    i = hfl(p)

60  continue

    f = abs(pfl(1,i))
    j = facep(1,f)

70  continue

    lj = fvl(loc,j)

    if (iwk(lj) == 0) then

      ivm = ivm + 1

      if (ivm > maxvm) then
        ierr = 20
        return
      end if

      vm(ivm) = lj
      iwk(lj) = ivm - kvm
      nbmv = nbmv + 1

    end if

    j = fvl(succ,j)
    if (j /= facep(1,f)) go to 70

    i = pfl(2,i)
    if (i /= hfl(p)) go to 60
    lv = ivm
!
!  Add mesh vertices on interior of edges and faces to VM.
!
    i = hfl(p)

80  continue

    sf = pfl(1,i)
    f = abs(sf)
    nbf = nbf + (btst(f+1) - btst(f))
    j = facep(1,f)
    lj = fvl(loc,j)

90  continue

    k = fvl(succ,j)
    lk = fvl(loc,k)

    if ((lk - lj)*sf > 0) then
      g = fvl(facn,fvl(edgc,j))
    else
      g = fvl(facn,fvl(edga,j))
    end if

    if (f < g .and. edst(j) > 0) then

      n = abs(edno(j))

      if (ivm + n > maxvm) then
        ierr = 20
        return
      end if

      nbmv = nbmv + n

      do l = edst(j),edst(j)+n-1
        ivm = ivm + 1
        vm(ivm) = l
        iwk(l) = ivm - kvm
      end do

    end if

    j = k
    lj = lk
    if (j /= facep(1,f)) go to 90
    n = fcst(f+1) - fcst(f)

    if (n > 0) then

      if (ivm + n > maxvm) then
        ierr = 20
        return
      end if

      nbmv = nbmv + n

      do l = fcst(f),fcst(f)+n-1
        ivm = ivm + 1
        vm(ivm) = l
        iwk(l) = ivm - kvm
      end do

    end if

    i = pfl(2,i)
    if (i /= hfl(p)) go to 80
!
!  Add mesh vertices in interior of polyhedron to VM.
!
    if (nimv > 0) then

      if (ivm + nimv > maxvm) then
        ierr = 20
        return
      end if

      do l = nvc-nimv+1,nvc
        ivm = ivm + 1
        vm(ivm) = l
      end do

    else

      ivm = ivm + 1

      if (ivm > maxvm) then
        ierr = 20
        return
      else if (nvc >= maxvc) then
        ierr = 14
        return
      end if

      vm(ivm) = nvc + 1
      ctrx = 0.0d0
      ctry = 0.0d0
      ctrz = 0.0d0
      jv = kvm + 1

      do i = jv,ivm-1
        j = vm(i)
        ctrx = ctrx + vcl(1,j)
        ctry = ctry + vcl(2,j)
        ctrz = ctrz + vcl(3,j)
      end do

      ctrx = ctrx/dble(nbmv)
      ctry = ctry/dble(nbmv)
      ctrz = ctrz/dble(nbmv)
      j = vm(jv)
      vcl(1,nvc+1) = 0.5d0*(ctrx + vcl(1,j))
      vcl(2,nvc+1) = 0.5d0*(ctry + vcl(2,j))
      vcl(3,nvc+1) = 0.5d0*(ctrz + vcl(3,j))
      rflag = .true.

    end if

    npt = ivm - kvm
    sizht = prime(3*npt/2)

    if (sizht < 3*npt/2) then
      ierr = 100
      return
    else if (kht + sizht > maxht) then
      ierr = 21
      return
    else if (kfc + nbf > maxfc) then
      ierr = 11
      return
    end if
!
!  Set up boundary faces with local vertex indices.
!
    ifc = kfc
    i = hfl(p)

140 continue

    f = abs(pfl(1,i))

    do k = btst(f),btst(f+1)-1
      ifc = ifc + 1
      fc(1:3,ifc) = iwk(btl(1:3,k))
    end do

    i = pfl(2,i)
    if (i /= hfl(p)) go to 140

    do i = kvm+1,kvm+nbmv
      iwk(vm(i)) = 0
    end do
!
!  Construct boundary-constrained triangulations. Dummy array IWK
!  is used for BF in call to IMPTR3, since BF is not referenced.
!
180 continue

    call bcdtri(rflag,nbf,nbmv,nimv,sizht,maxfc-kfc,vcl,vm(kvm+1), &
      nfc,ntrif(p),ntetra(p),fc(1,kfc+1),ht(kht+1), ierr )

    if (ierr /= 0) return

    if (nimv == 0 .and. vm(ivm) > 0) then

      if (jv < lv) then
        jv = jv + 1
        j = vm(jv)
        vcl(1,nvc+1) = 0.5d0*(ctrx + vcl(1,j))
        vcl(2,nvc+1) = 0.5d0*(ctry + vcl(2,j))
        vcl(3,nvc+1) = 0.5d0*(ctrz + vcl(3,j))
        go to 180
      else if (jv == lv) then
        jv = jv + 1
        vcl(1,nvc+1) = ctrx
        vcl(2,nvc+1) = ctry
        vcl(3,nvc+1) = ctrz
        rflag = .false.
        go to 180
      else
        nvc = nvc + 1
      end if

    end if

    if (crit >= 1 .and. crit <= 3) then

      call imptr3 ( .true., .true., &
        crit, npt, sizht, maxfc-kfc, vcl, vm(kvm+1), nfc, ntetra(p), iwk, &
        fc(1,kfc+1), ht(kht+1), ntrif(p), ierr )

    end if

    if (ierr /= 0) return

    kfc = kfc + nfc
    kht = kht + sizht
    kvm = ivm
    xfhv(1,p+1) = kfc + 1
    xfhv(2,p+1) = kht + 1
    xfhv(3,p+1) = kvm + 1

  end do

  return
end
subroutine trisiz ( ntrid, npolg, hvl, pvl, area, psi, h, indp, loch )
!
!******************************************************************************
!
!! TRISIZ smooths PSI and computes triangle sizes.
!
!
!  Purpose: 
!
!    Smooth PSI (mean mesh distribution function) values using
!    heap so that they differ by a factor of at most 4 in adjacent
!    polygons and then compute triangle sizes for each polygon.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NTRID - desired number of triangles in mesh.
!
!    Input, NPOLG - number of polygons or positions used in HVL array.
!
!    Input, HVL(1:NPOLG) - head vertex list.
!
!    Input, PVL(1:4,1:*) - polygon vertex list.
!
!    Input, AREA(1:NPOLG) - area of convex polygons in decomposition.
!
!    Input/output, PSI(1:NPOLG) - mean mdf values in the convex polygons.
!    On output, values are smoothed.
!
!    Output, H(1:NPOLG) - triangle size for convex polygons.
!
!    Workspace, INDP(1:NPOLG) - indices of polygon or PSI which are maintained
!    in heap according to PSI values.
!
!    Workspace, LOCH(1:NPOLG) - location of polygon indices in heap.
!
  implicit none
!
  integer npolg
!
  double precision area(npolg)
  integer, parameter :: edgv = 4
  double precision factor
  double precision h(npolg)
  integer hvl(npolg)
  integer i
  integer indp(npolg)
  integer j
  integer k
  integer l
  integer, parameter :: loc = 1
  integer loch(npolg)
  integer ntrid
  integer, parameter :: polg = 2
  double precision psi(npolg)
  integer pvl(4,*)
  integer r
  integer, parameter :: succ = 3
  double precision sum2
!
  factor = 0.25d0

  do i = 1,npolg
    indp(i) = i
    loch(i) = i
  end do

  k = int(npolg/2)

  do l = k,1,-1
    call sfdwmf(l,npolg,psi,indp,loch)
  end do

  do r = npolg,2,-1

    j = indp(1)
    indp(1) = indp(r)
    loch(indp(1)) = 1
    call sfdwmf(1,r-1,psi,indp,loch)
    i = hvl(j)

30  continue

    k = pvl(edgv,i)
    if (k > 0) then
      k = pvl(polg,k)
      if (psi(k) < psi(j)*factor) then
        psi(k) = psi(j)*factor
        call sfupmf(loch(k),psi,indp,loch)
      end if
    end if
    i = pvl(succ,i)
    if (i /= hvl(j)) go to 30

  end do

  sum2 = 0.0d0
  do i = 1,npolg
    sum2 = sum2 + psi(i)*area(i)
  end do

  factor = 2.0d0/dble(ntrid)

  do i = 1,npolg
    psi(i) = psi(i)/sum2
    h(i) = sqrt(factor/psi(i))
  end do

  return
end
subroutine trpolg ( nvrt, xc, yc, h, nbc, bndcyc, ldv, nvc, ntri, maxvc,  &
  maxti, maxiw, maxwk, vcl, til, iwk, wk, ierror )
!
!*******************************************************************************
!
!! TRPOLG generates a Delaunay triangular mesh inside a convex polygon.
!
!
!  Discussion:
!
!    A quasi-uniform grid of spacing H is used.
!
!  Modified:
!
!    12 July 1999
!
!  Author:
!
!    Barry Joe,
!    Department of Computing Science,
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!
!  Parameters:
!
!    Input, integer NVRT, the number of vertices on the boundary of
!    convex polygon.
!
!    Input, double precision XC(0:NVRT), YC(0:NVRT), the vertex coordinates
!    in counter clockwise order; (XC(0),YC(0)) = (XC(NVRT),YC(NVRT)); it is
!    assumed that all interior angles are < PI.
!
!    Input, double precision H, the spacing of mesh vertices in polygon.
!
!    Input, integer NBC, the size of BNDCYC.
!
!    Input/output, integer BNDCYC(0:NBC), the indices in VCL of mesh
!    vertices of boundary cycle; BNDCYC(0) = BNDCYC(NBC); contains
!    (XC(I),YC(I)).
!
!    Input, integer LDV, the leading dimension of VCL in calling routine.
!
!    Input/output, integer NVC, the number of coordinates or positions used
!    in VCL array.
!
!    Input/output, integer NTRI, the number of triangles or positions used
!    in TIL.
!
!    Input, integer MAXVC, the maximum size available for VCL array.
!
!    Input, integer MAXTI, the maximum size available for TIL array.
!
!    Input, integer MAXIW, the maximum size available for IWK array, should
!    be >= 6*(1 + INT(DIAM/H)) + 4*(NBC + NCW) where DIAM is
!    diameter of polygon, NCW is number of edges on boundary
!    of interior triangulation.
!
!    Input, integer MAXWK, the maximum size available for WK array, should
!    be >= 3*NVRT+2.
!
!    Input/output, double precision VCL(1:2,1:NVC), the vertex coordinate list.
!
!    Input/output, integer TIL(1:3,1:NTRI), the triangle incidence list.
!
!    Workspace, integer IWK(1:MAXIW).
!
!    Workspace, double precision WK(1:MAXWK).
!
!    Output, integer IERROR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer ldv
  integer maxiw
  integer maxti
  integer maxvc
  integer maxwk
  integer nbc
  integer nvrt
!
  integer bndcyc(0:nbc)
  double precision costh
  integer cwalk
  double precision dist
  double precision h
  double precision hs
  integer i
  integer i1
  integer i2
  integer ibot
  integer iedge
  integer ierror
  integer ind
  logical inter
  integer iwk(maxiw)
  integer maxcw
  integer mbc
  integer ncw
  integer nshr
  integer nt
  integer ntri
  integer nvc
  integer sdist
  double precision sinth
  double precision smdist
  integer sptr
  integer tedg
  integer til(3,maxti)
  double precision vcl(ldv,maxvc)
  double precision wk(maxwk)
  double precision x0
  double precision xc(0:nvrt)
  double precision xi
  integer xs
  double precision y0
  double precision yc(0:nvrt)
  double precision yi
  double precision yr
  integer ys
!
  if ( nvrt + 1 > maxiw ) then
    ierror = 6
    return
  end if

  if ( 3*nvrt + 2 > maxwk ) then
    ierror = 7
    return
  end if

  xs = 1
  ys = xs + nvrt + 1
  sdist = ys + nvrt + 1
  iedge = 1
  hs = h / sqrt ( 2.0D+00 )
  wk(sdist:sdist+nvrt-1) = hs

  call shrnk2 ( nvrt, xc, yc, wk(sdist), nshr, wk(xs), wk(ys), iwk(iedge), &
    ierror )

  if ( ierror /= 0 ) then
    return
  end if

  inter = ( nshr > 0 )

  if ( inter ) then

    call diam2 ( nshr, wk(xs+1), wk(ys+1), i1, i2, dist, ierror )

    if ( ierror /= 0 ) then
      return
    end if

    call rotpg ( nshr, wk(xs), wk(ys), i1, i2, ibot, costh, sinth )

    maxcw = 6 * ( 1 + int ( ( wk(ys) - wk(ys+ibot) ) / h ) )

    if ( maxcw + 1 > maxiw ) then
      ierror = 6
      return
    end if

    cwalk = 1

    call inttri ( nshr, wk(xs), wk(ys), h, ibot, costh, sinth, ldv, nvc, ntri, &
      maxvc, maxti, maxcw, vcl, til, ncw, iwk(cwalk), ierror )

    if ( ierror /= 0 ) then
      return
    end if
!
!  Determine the mesh vertex which should be moved to front of
!  BNDCYC - closest to CWALK(0) and also with y-coordinate >
!  that of CWALK(0) when rotated if NCW > 0.
!
    x0 = vcl(1,iwk(cwalk))
    y0 = vcl(2,iwk(cwalk))

    if ( ncw > 0 ) then
      yr = sinth * x0 + costh * y0
    end if

    smdist = 100000.0D+00 * h**2

    do i = 0, nbc-1

      xi = vcl(1,bndcyc(i))
      yi = vcl(2,bndcyc(i))

      if ( ncw > 0 ) then
        if ( sinth * xi + costh * yi <= yr ) then
          cycle
        end if
      end if

      dist = ( xi - x0 )**2 + ( yi - y0 )**2

      if ( dist < smdist ) then
        smdist = dist
        ind = i
      end if

    end do

    call rotiar ( nbc, bndcyc, ind )
    bndcyc(nbc) = bndcyc(0)
    nt = nbc + ncw
    tedg = cwalk + ncw + 1

  else

    call diam2 ( nvrt, xc(1), yc(1), i1, i2, dist, ierror )

    if ( ierror /= 0 ) then
      return
    end if

    ind = 0

    do

      if ( ind >= nbc ) then
        exit
      end if

      if ( xc(i1) == vcl(1,bndcyc(ind)) .and. &
           yc(i1) == vcl(2,bndcyc(ind)) ) then
        exit
      end if

      ind = ind + 1

    end do

    call rotiar ( nbc, bndcyc, ind )
    bndcyc(nbc) = bndcyc(0)
    mbc = 1

    do

      if ( mbc >= nbc ) then
        exit
      end if

      if ( xc(i2) == vcl(1,bndcyc(mbc)) .and. &
           yc(i2) == vcl(2,bndcyc(mbc)) ) then
        exit
      end if

      mbc = mbc + 1

    end do

    ind = nbc

    do i = mbc+1, mbc+(nbc-mbc-1)/2
      ind = ind - 1
      i1 = bndcyc(i)
      bndcyc(i) = bndcyc(ind)
      bndcyc(ind) = i1
    end do

    bndcyc(nbc) = bndcyc(mbc)
    nt = nbc - 2
    tedg = 1
!
!  Left boundary chain contains mesh vertices BNDCYC(0:MBC)
!  and right chain contains BNDCYC(0,MBC+1:NBC); MBC < NBC.
!
  end if

  if ( ntri + nt > maxti ) then
    ierror = 9
    return
  else if ( tedg + 4*nt - 1 > maxiw ) then
    ierror = 6
    return
  end if

  if ( inter ) then
    call tmerge ( inter, nbc, ncw, bndcyc, iwk(cwalk), ldv, vcl, &
      til(1,ntri+1), iwk(tedg), ierror )
  else
    call tmerge ( inter, mbc, nbc-mbc, bndcyc, bndcyc(mbc), ldv, vcl, &
      til(1,ntri+1), iwk(tedg), ierror )
  end if

  if ( ierror /= 0 ) then
    return
  end if

  sptr = tedg + 3 * nt

  call cvdtri ( inter, ldv, nt, vcl, til(1,ntri+1), iwk(tedg), iwk(sptr), &
    ierror )

  ntri = ntri + nt

  return
end
function umdf2 ( x, y )
!
!******************************************************************************
!
!! UMDF2 is a sample user mesh distribution function for 2D.
!
!
!  Purpose: 
!
!    Dummy user-supplied mesh distribution function which
!    is provided if heuristic mesh distribution function is used.
!
!  Parameters:
!
!    Input, X, Y - coordinates of 2D point.
!
!    Output, UMDF2 - mesh distribution function value at (X,Y).
!
  implicit none
!
  double precision umdf2
  double precision x
  double precision y
!
  umdf2 = 1.0d0

  return
end
function umdf3 ( x, y, z )
!
!******************************************************************************
!
!! UMDF3 is a sample user mesh distribution function for 3D.
!
!
!  Purpose: 
!
!    User-supplied mesh distribution function which may be
!    used in place of heuristic mesh distribution function, or
!    may be a dummy routine.
!
!  Parameters:
!
!    Input, X, Y, Z - coordinates of 3D point
!
!    Output, UMDF3 - mesh distribution function value at (X,Y,Z)
!
  implicit none
!
  double precision umdf3
  double precision x
  double precision y
  double precision z
!
!  Mesh distribution function for airplane exterior region
!
  if (-1.5d0 <= x .and. x <= 38.0d0 .and. &
    abs(y) <= 4.0d0 .and. abs(z) <= 4.0d0) then
     umdf3 = 8.0d0
  else if (11.0d0 <= x .and. x <= 18.0d0 .and. -2.8d0 <= y &
     .and. y <= 2.0d0 .and. abs(z) <= 16.5d0) then
     umdf3 = 8.0d0
  else if (31.0d0 <= x .and. x <= 37.0d0 .and. -2.5d0 <= y &
     .and. y <= 2.0d0 .and. abs(z) <= 9.0d0) then
     umdf3 = 8.0d0
  else if (31.0d0 <= x .and. x <= 37.0d0 .and. 0.0d0 <= y &
     .and. y <= 9.5d0 .and. abs(z) <= 4.0d0) then
     umdf3 = 8.0d0
  else if (-3.0d0 <= x .and. x <= 40.0d0 .and. -9.0d0 <= y &
     .and. y <= 11.0d0 .and. abs(z) <= 18.0d0) then
     umdf3 = 3.0d0
  else
     umdf3 = 1.0d0
  end if

  return
end
subroutine updatf ( a, b, c, d, e, i, n, p, front, back, fc, ht, ierr )
!
!******************************************************************************
!
!! UPDATF updates a record in FC after a local transformation.
!
!
!  Purpose: 
!
!    Update record in FC due to a local transformation.
!    Tetrahedron ABCD becomes ABCE. Add face ABC to queue if it is
!    interior face, not yet in queue, and its largest index isn't I.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A, B, C - first 3 fields of FC record (in any order).
!
!    Input, D, E - fourth vertex indices of old and new tetrahedrons.
!
!    Input, I - vertex index determining whether face put on queue.
!
!    Input, N - upper bound on vertex indices.
!
!    Input, P - size of hash table.
!
!    Input/output, FRONT, BACK - front and back pointers of queue.
!
!    Input, FC(1:7,1:*) - array of face records; see routine DTRIS3.
!
!    Input, HT(0:P-1) - hash table using direct chaining.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer p
!
  integer a
  integer b
  integer back
  integer c
  integer d
  integer e
  integer fc(7,*)
  integer front
  integer ht(0:p-1)
  integer htsrc
  integer i
  integer ierr
  integer ind
  integer n
!
  ierr = 0
  ind = htsrc(a,b,c,n,p,fc,ht)

  if (ind <= 0) then
    ierr = 300
    return
  end if

  if (fc(4,ind) == d) then
    fc(4,ind) = e
  else
    fc(5,ind) = e
  end if

  if (fc(7,ind) == -1 .and. fc(3,ind) /= i .and. fc(5,ind) > 0) then

    fc(7,ind) = 0

    if (front == 0) then
      front = ind
    else
      fc(7,back) = ind
    end if

    back = ind
  end if

  return
end
subroutine updatk ( k, ind, d, e, i, n, p, front, back, fc, ht, ierr )
!
!******************************************************************************
!
!! UPDATK updates a record in FC after a local transformation.
!
!
!  Purpose: 
!
!    Update record in FC due to a local transformation.
!    Simplex with face IND(1:K) and vertex D has D changed to E.
!    Add face IND(1:K) to queue if it is interior face, not yet in
!    queue, and its largest index isn't I.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - number of vertices in a face.
!
!    Input/output, IND(1:K) - vertex indices of face (in any order).
!
!    Input, D, E - (K+1)st vertex indices of old and new simplices.
!
!    Input, I - vertex index determining whether face put on queue.
!
!    Input, N - upper bound on vertex indices.
!
!    Input, P - size of hash table.
!
!    Input/output, FRONT, BACK - front and back pointers of queue.
!
!    Input/output, FC(1:K+4,1:*) - array of face records; see routine DTRISK.
!
!    Input, HT(0:P-1) - hash table using direct chaining.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer p
!
  integer back
  integer d
  integer e
  integer fc(k+4,*)
  integer front
  integer ht(0:p-1)
  integer htsrck
  integer i
  integer ierr
  integer ind(k)
  integer n
  integer pos
!
  ierr = 0
  pos = htsrck(k,ind,n,p,fc,ht)

  if (pos <= 0) then
    ierr = 400
    return
  end if

  if (fc(k+1,pos) == d) then
    fc(k+1,pos) = e
  else
    fc(k+2,pos) = e
  end if

  if (fc(k+4,pos) == -1 .and. fc(k,pos) /= i .and. fc(k+2,pos) > 0) then

    fc(k+4,pos) = 0

    if (front == 0) then
      front = pos
    else
      fc(k+4,back) = pos
    end if

    back = pos

  end if

  return
end
subroutine updatr ( a, b, c, d, e, qflag, n, p, front, back, fc, ht, ierr )
!
!******************************************************************************
!
!! UPDATR updates a record in FC after a local transformation.
!
!
!  Purpose: 
!
!    Update record in FC due to a local transformation.
!    Tetrahedron ABCD becomes ABCE. Add face ABC to queue if it is
!    not yet in queue and QFLAG is .TRUE.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A, B, C - first 3 fields of FC record (in any order).
!
!    Input, D, E - fourth vertex indices of old and new tetrahedrons.
!
!    Input, QFLAG - .TRUE. iff face is to be put on queue.
!
!    Input, N - upper bound on vertex indices.
!
!    Input, P - size of hash table.
!
!    Input/output, FRONT, BACK - front and back pointers of queue.
!
!    Input/output, FC(1:7,1:*) - array of face records; see routine BCDTRI.
!
!    Input, HT(0:P-1) - hash table using direct chaining.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer p
!
  integer a
  integer b
  integer back
  integer c
  integer d
  integer e
  integer fc(7,*)
  integer front
  integer ht(0:p-1)
  integer htsrc
  integer ierr
  integer ind
  integer n
  logical qflag
!
  ierr = 0
  ind = htsrc(a,b,c,n,p,fc,ht)

  if (ind <= 0) then
    ierr = 300
    return
  end if

  if (fc(4,ind) == d) then
    fc(4,ind) = e
  else
    fc(5,ind) = e
  end if

  if (fc(7,ind) == -1 .and. qflag) then
    fc(7,ind) = 0
    if (front == 0) then
      front = ind
    else
      fc(7,back) = ind
    end if
    back = ind
  end if

  return
end
function urand ( iy )
!
!******************************************************************************
!
!! URAND is a uniform random number generator.
!
!
!  Reference:
!
!    Forsythe, Malcolm, Moler, 
!    page 246.
!
!  Discussion:
!
!    URAND is a uniform random number generator based on theory and
!    suggestions given in D. E. Knuth (1969), Vol. 2. The integer IY
!    should be initialized to an arbitrary integer prior to the first
!    call to URAND. The calling program should not alter the value of
!    IY between subsequent calls to URAND. Values of URAND will be
!    returned in the interval (0,1).
!
!  Parameters:
!
!    Input/output, integer IY.
!
!    Output, real URAND.
!
  implicit none
!
  double precision halfm
  integer, save :: ia = 0
  integer, save :: ic = 0
  integer iy
  integer m
  integer, save :: m2 = 0
  integer, save :: mic = 0
  real, save :: s = 0.0E+00
  real urand
!
  if ( m2 == 0 ) then
!
!  If first entry, compute machine integer word length
!
    m = 1

    do

      m2 = m
      m = 2 * m2

      if ( m <= m2 ) then
        exit
      end if

    end do
 
    halfm = m2
!
!  Compute multiplier and increment for linear congruential method
!
    ia = 8 * int(halfm*atan(1.0d0)/8.0d0) + 5
    ic = 2 * int(halfm*(0.5d0-sqrt(3.0d0)/6.0d0)) + 1
    mic = (m2 - ic) + m2
!
!  S is the scale factor for converting to floating point
!
    s = 0.5E+00 / halfm

  end if
!
!  Compute next random number
!
  iy = iy*ia
!
!  The following statement is for computers which do not allow
!  integer overflow on addition
!
  if (iy > mic) then
    iy = (iy - m2) - m2
  end if

  iy = iy + ic
!
!  The following statement is for computers where the word
!  length for addition is greater than for multiplication
!
  if (iy/2 > m2) then
    iy = (iy - m2) - m2
  end if
!
!  The following statement is for computers where integer
!  overflow affects the sign bit
!
  if ( iy < 0 ) then
    iy = (iy + m2) + m2
  end if

  urand = real ( iy ) * s

  return
end
subroutine vbedg ( x, y, vcl, til, tnbr, ltri, ledg, rtri, redg )
!
!******************************************************************************
!
!! VBEDG determines the boundary edges of a 2D triangulation.
!
!
!  Purpose: 
!
!    Determine boundary edges of 2D triangulation which are
!    visible from point (X,Y) outside convex hull.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, X, Y - 2D point outside convex hull.
!
!    Input, VCL(1:2,1:*) - coordinates of 2D vertices.
!
!    Input, TIL(1:3,1:*) - triangle incidence list.
!
!    Input, TNBR(1:3,1:*) - triangle neighbor list; negative values are
!    used for links of counter clockwise linked list of boundary edges;
!    LINK = -(3*I + J-1) where I, J = triangle, edge index.
!
!    Input, LTRI, LEDG - if LTRI /= 0 then they are assumed to be as defined
!    below and are not changed, else they are updated.  LTRI is
!    the index of boundary triangle to left of leftmost boundary
!    triangle visible from (X,Y).  LEDG is the boundary edge of triangle
!    LTRI to left of leftmost boundary edge visible from (X,Y)
!
!    Input/output, RTRI.  On input, the index of boundary triangle to begin
!    search at.  On output, the index of rightmost boundary triangle 
!    visible from (X,Y)
!
!    Input/output, REDG.  On input, the edge of triangle RTRI that is 
!    visible from (X,Y).  On output, the edge of triangle RTRI that is 
!    visible from (X,Y)
!
  implicit none
!
  integer a
  integer b
  integer e
  integer l
  logical ldone
  integer ledg
  integer lr
  integer lrline
  integer ltri
  integer redg
  integer rtri
  integer t
  integer til(3,*)
  integer tnbr(3,*)
  double precision vcl(2,*)
  double precision x
  double precision y
!
!  Find rightmost visible boundary edge using links, then possibly
!  leftmost visible boundary edge using triangle neighbor info.
!
  if (ltri == 0) then
    ldone = .false.
    ltri = rtri
    ledg = redg
  else
    ldone = .true.
  end if

10 continue

  l = -tnbr(redg,rtri)
  t = l/3
  e = mod(l,3) + 1
  a = til(e,t)

  if (e <= 2) then
    b = til(e+1,t)
  else
    b = til(1,t)
  end if

  lr = lrline(x,y,vcl(1,a),vcl(2,a),vcl(1,b),vcl(2,b),0.0d0)

  if (lr > 0) then
    rtri = t
    redg = e
    go to 10
  end if

  if (ldone) return

  t = ltri
  e = ledg

20 continue

  b = til(e,t)

  if (e >= 2) then
    e = e - 1
  else
    e = 3
  end if

  do while (tnbr(e,t) > 0) 

    t = tnbr(e,t)

    if (til(1,t) == b) then
      e = 3
    else if (til(2,t) == b) then
      e = 1
    else
      e = 2
    end if

  end do

  a = til(e,t)
  lr = lrline(x,y,vcl(1,a),vcl(2,a),vcl(1,b),vcl(2,b),0.0d0)
  if (lr > 0) go to 20

  ltri = t
  ledg = e

  return
end
subroutine vbfac ( pt, ctr, vcl, vm, bf, fc, topv, topnv )
!
!******************************************************************************
!
!! VBFAC determines the boundary faces of a 3D triangulation.
!
!
!  Purpose: 
!
!    Determine boundary faces of 3D triangulation visible
!    from point PT, given a starting visible boundary face.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, PT(1:3) - 3D point.
!
!    Input, CTR(1:3) - 3D point in interior of triangulation.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:*) - indices of vertices of VCL being triangulated.
!
!    Input, BF(1:3,1:*) -  array of boundary face records; see DTRIS3.
!
!    Input/output, FC(1:7,1:*) - array of face records; see routine DTRIS3; 
!    row 7 is used for links of 3 stacks in this routine.  On output, 
!    FC(7,*) has been updated, so that only stack of visible boundary 
!    faces remains.
!
!    Input/output, TOPV.  On input, index of FC of visible boundary face.
!    On output, index of top of stack of visible boundary faces
!
!    Input, TOPNV - index of top of stack of boundary faces already found
!    to be not visible from PT, or 0 for empty stack.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer bf(3,*)
  double precision ctr(3)
  integer fc(7,*)
  integer ierr
  integer j
  integer k
  integer nbr
  integer op
  integer opside
  double precision pt(3)
  integer ptr
  integer topn
  integer topnv
  integer topt
  integer topv
  integer va
  integer vb
  integer vc
  integer vm(*)
  double precision vcl(3,*)
!
!  TOPN is index of top of stack of non-visible boundary faces.
!  TOPT is index of top of stack of boundary faces to be tested.
!
  topn = topnv
  topt = 0
  fc(7,topv) = 0
  k = -fc(5,topv)

  do j = 1,3
    nbr = bf(j,k)
    if (fc(7,nbr) == -1) then
      fc(7,nbr) = topt
      topt = nbr
    end if
  end do

  do while (topt /= 0)

    ptr = topt
    topt = fc(7,ptr)
    va = vm(fc(1,ptr))
    vb = vm(fc(2,ptr))
    vc = vm(fc(3,ptr))
    op = opside(vcl(1,va),vcl(1,vb),vcl(1,vc),ctr,pt)

    if (op == 2) then
      ierr = 301
      return
    end if

    if (op == 1) then

      fc(7,ptr) = topv
      topv = ptr
      k = -fc(5,ptr)

      do j = 1,3
        nbr = bf(j,k)
        if (fc(7,nbr) == -1) then
          fc(7,nbr) = topt
          topt = nbr
        end if
      end do

    else

      fc(7,ptr) = topn
      topn = ptr

    end if

  end do
!
!  For boundary faces not visible from PT, set FC(7,*) = -1.
!
  do while (topn /= 0) 
    ptr = topn
    topn = fc(7,ptr)
    fc(7,ptr) = -1
  end do

  return
end
subroutine vbfack ( k, pt, ctr, vcl, vm, bf, fc, topv, topnv, ind, mat, vec )
!
!******************************************************************************
!
!! VBFACK determines the boundary faces of a KD triangulation.
!
!
!  Purpose: 
!
!    Determine boundary faces of K-D triangulation visible
!    from point PT, given a starting visible boundary face.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of triangulation.
!
!    Input, PT(1:K) - K-D point.
!
!    Input, CTR(1:K) - K-D point in interior of triangulation.
!
!    Input, VCL(1:K,1:*) - vertex coordinate list.
!
!    Input, VM(1:*) - indices of vertices of VCL being triangulated.
!
!    Input, BF(1:K,1:*) -  array of boundary face records; see DTRISK.
!
!    Input/output, FC(1:K+4,1:*) - array of face records; see routine 
!    DTRISK; row K+4 is used for links of 3 stacks in this routine.
!    On output, FC(K+4,*) - gets updated; only stack of visible boundary faces
!    remains at end of routine.
!
!    Input/output, TOPV.  On input, index of FC of visible boundary face.
!    On output, index of top of stack of visible boundary faces.
!
!    Input, TOPNV - index of top of stack of boundary faces already found
!    to be not visible from PT, or 0 for empty stack.
!
!    Workspace, IND(1:K) - indices in VCL of K-D vertices.
!
!    Workspace, MAT(1:K-1,1:K) - matrix used for solving system of homogeneous
!    linear equations.
!
!    Workspace, VEC(1:K) - vector used for hyperplane normal.
!
  implicit none
!
  integer k
!
  integer bf(k,*)
  double precision ctr(k)
  integer fc(k+4,*)
  integer i
  integer ind(k)
  integer j
  integer kp2
  integer kp4
  integer l
  double precision mat(k-1,k)
  integer nbr
  integer opsidk
  double precision pt(k)
  integer ptr
  integer topn
  integer topnv
  integer topt
  integer topv
  double precision vcl(k,*)
  double precision vec(k)
  integer vm(*)
!
!  TOPN is index of top of stack of non-visible boundary faces.
!  TOPT is index of top of stack of boundary faces to be tested.
!
  kp2 = k + 2
  kp4 = k + 4
  topn = topnv
  topt = 0
  fc(kp4,topv) = 0
  l = -fc(kp2,topv)

  do j = 1,k
    nbr = bf(j,l)
    if (fc(kp4,nbr) == -1) then
      fc(kp4,nbr) = topt
      topt = nbr
    end if
  end do

  do while ( topt /= 0 )

    ptr = topt
    topt = fc(kp4,ptr)
    ind(1:k) = vm(fc(1:k,ptr))

    if (opsidk(k,ind,vcl,.false.,ctr,pt,mat,vec) == 1) then

      fc(kp4,ptr) = topv
      topv = ptr
      l = -fc(kp2,ptr)

      do j = 1,k
        nbr = bf(j,l)
        if (fc(kp4,nbr) == -1) then
          fc(kp4,nbr) = topt
          topt = nbr
        end if
      end do

    else

      fc(kp4,ptr) = topn
      topn = ptr

    end if

  end do
!
!  For boundary faces not visible from PT, set FC(KP4,*) = -1.
!
  do while (topn /= 0) 
    ptr = topn
    topn = fc(kp4,ptr)
    fc(kp4,ptr) = -1
  end do

  return
end
subroutine vispol ( xeye, yeye, nvrt, xc, yc, nvis, ivis, ierr )
!
!******************************************************************************
!
!! VISPOL computes the visibility polygon from an eyepoint.
!
!
!  Purpose: 
!
!    Compute the visibility polygon VP from an eyepoint in
!    the interior or blocked exterior of a simple polygon P or
!    on the boundary of a simply connected polygonal region P.
!    In the latter case, the interior angles at all vertices must
!    be strictly between 0 and 2*PI.
!
!  Discussion:
!
!    On input, XC and YC contain vertex coordinates of P. During
!    the algorithm, part of XC, YC is used as a stack, which, on
!    output, contains the vertex coordinates of VP.  The stack
!    vertices overwrite the input vertices as the input vertices
!    are scanned.  Elements of IVIS are set when vertices are added
!    to the stack; these values may have +NV or -NV added to them
!    to indicate that stack point has same angle as previous one.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Reference:
!
!    Barry Joe and R. B. Simpson, 
!    BIT 
!    Volume 27, 1987, pages 458-473.
!
!  Parameters:
!
!    Input, XEYE, YEYE - coordinates of eyepoint; must be a simple vertex
!    if it lies on the boundary (i.e. occurs only once).
!
!    Input, NVRT - upper subscript of XC, YC (approx number of vertices).
!
!    Input/output, XC(0:NVRT), YC(0:NVRT).  On input, if eyepoint is interior 
!    or blocked exterior, then arrays contain coordinates in counter 
!    clockwise or CW order, respectively, with (XC(0),YC(0)) = (XC(NVRT),
!    YC(NVRT)); (XC(0),YC(0)) is a vertex visible from
!    (XEYE,YEYE), e.g. as computed by routine ROTIPG.
!    If eyepoint is a vertex of P then arrays contain
!    coordinates in counter clockwise order; (XC(0),YC(0)) is successor
!    vertex of (XEYE,YEYE); (XC(NVRT),YC(NVRT)) is
!    predecessor vertex of (XEYE,YEYE).
!    On output, vertices of VP in counter clockwise order;
!    if eyepoint is interior or blocked exterior then
!    (XC(0),YC(0)) = (XC(NVIS),YC(NVIS)), else (XC(0),YC(0))
!    and (XC(NVIS),YC(NVIS)) are the successor and
!    predecessor vertices of (XEYE,YEYE) in VP.
!
!    Output, NVIS - upper subscript of XC, YC on output (approx number
!    of vertices of VP); NVIS <= NVRT.
!
!    Output, IVIS(0:NVIS) - contains information about the vertices of VP
!    with respect to the vertices of P; IVIS(I) = K if (XC(I),YC(I))
!    is the vertex of index K in the input polygon; IVIS(I)
!    = -K if (XC(I),YC(I)) is on the interior of the edge
!    joining vertices of index K-1 and K in input polygon.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer nvrt
!
  logical beye
  integer cur
  integer i
  integer ierr
  integer ivis(0:nvrt)
  integer lr
  integer lrline
  integer nv
  integer nvis
  integer oper
  integer top
  double precision xc(0:nvrt)
  double precision xe
  double precision xeye
  double precision xw
  double precision yc(0:nvrt)
  double precision ye
  double precision yeye
  double precision yw
!
  common /gvpvar/ nv,oper,cur,top,xe,ye,xw,yw,beye
  save /gvpvar/
!
!  Variables in common block GVPVAR:
!
!        NV - NVRT
!        OPER - operation code 1 to 7 for LEFT, RIGHT, SCANA, SCANB,
!              SCANC, SCAND, FINISH
!        CUR - index of current vertex of P in XC, YC arrays
!        TOP - index of top vertex of stack in XC, YC arrays
!              (TOP <= CUR is always satisfied)
!        XE,YE - XEYE,YEYE
!        XW,YW - coordinates of point on last or second-last edge
!              processed (needed for routines VPSCNB, VPSCNC, VPSCND)
!        BEYE - .TRUE. iff eyepoint is on boundary
!
  ierr = 0
  beye = xc(0) /= xc(nvrt) .or. yc(0) /= yc(nvrt)
  nv = nvrt
  xe = xeye
  ye = yeye
  ivis(0) = 0
  cur = 1
  if (.not. beye) go to 20

10 continue

  lr = lrline(xc(nv-1),yc(nv-1),xe,ye,xc(nv),yc(nv),0.0d0)

  if (lr == 0) then
    nv = nv - 1
    go to 10
  end if

20 continue

  lr = lrline(xc(cur),yc(cur),xe,ye,xc(0),yc(0),0.0d0)

  if (lr == 0) then
    cur = cur + 1
    go to 20
  end if

  if (lr == -1) then

    oper = 1

    if (cur == 1) then

      top = 1
      ivis(1) = cur

    else if (beye) then

      top = 1
      xc(0) = xc(cur-1)
      yc(0) = yc(cur-1)
      ivis(0) = cur - 1
      xc(1) = xc(cur)
      yc(1) = yc(cur)
      ivis(1) = cur

    else

      top = 2
      xc(1) = xc(cur-1)
      yc(1) = yc(cur-1)
      ivis(1) = cur - 1 + nv
      xc(2) = xc(cur)
      yc(2) = yc(cur)
      ivis(2) = cur

    end if

  else

    oper = 3
    top = 0

    if (beye .and. cur > 1) then
      xc(0) = xc(cur-1)
      yc(0) = yc(cur-1)
      ivis(0) = cur - 1
    end if

  end if
!
!  Angular displacement of stack points are in nondecreasing order,
!  with at most two consecutive points having the same displacement.
!
30 continue

  if (oper == 1) then
    call vpleft(xc,yc,ivis)
  else if (oper == 2) then
    call vprght(xc,yc,ivis, ierr )
  else if (oper == 3) then
    call vpscna(xc,yc,ivis, ierr )
  else if (oper == 4) then
    call vpscnb(xc,yc,ivis, ierr )
  else if (oper == 5) then
    call vpscnc(xc,yc,ivis, ierr )
  else
    call vpscnd(xc,yc,ivis, ierr )
  end if

  if (ierr /= 0) then
    nvis = top
    return
  end if

  if (oper <= 6) go to 30
!
!  Add or subtract NV from those IVIS values which are used to
!  indicate that stack point has same angle as previous one.
!
  do i = 1,top
    if (ivis(i) > nv) then
      ivis(i) = ivis(i) - nv
    else if (ivis(i) < -nv) then
      ivis(i) = ivis(i) + nv
    end if
  end do

  nvis = top

  return
end
subroutine visvrt ( angspc, xeye, yeye, nvis, xc, yc, ivis, maxn, nvsvrt, &
  theta )
!
!******************************************************************************
!
!! VISVRT determines a list of visible vertices.
!
!
!  Purpose: 
!
!    Determine a list of visible vertices, ordered by
!    increasing "polar angle", on the boundary of the visibilty
!    polygon from boundary eyepoint (XEYE,YEYE). This list
!    includes the vertices of visibility polygon such that a
!    line segment from (XEYE,YEYE) to vertex lies in interior
!    of polygon, as well as extra points on edges which subtend
!    an angle >= 2*ANGSPC at (XEYE,YEYE). These extra points are
!    at an equal angular spacing of >= ANGSPC and < 2*ANGSPC. The
!    successor and predecessor of eyepoint are included in list.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, ANGSPC - angle spacing parameter in radians which controls
!    how many extra points become visible vertices.
!
!    Input, XEYE,YEYE - coordinates of boundary eyepoint.
!
!    Input, NVIS - (number of vertices of visibility polygon) - 2.
!
!    Input/output, XC(0:NVIS),YC(0:NVIS).  On input, the coordinates of the
!    vertices of visibility polygon in counter clockwise order; 
!    (XC(0),YC(0)) and (XC(NVIS),YC(NVIS)) are the successor and predecessor
!    vertices of eyepoint in visibility polygon; at most 2
!    consecutive vertices have same polar angle with respect to
!    eyepoint.
!    On output, XC(0:NVSVRT),YC(0:NVSVRT) contain coordinates of 
!    visible vertices which overwrite the input coordinates
!
!    Input/output, IVIS(0:NVIS).  On input, contains information about the
!    vertices of XC, YC arrays with respect to the original polygon from
!    which visibility polygon is computed; if IVIS(I) >= 0
!    then (XC(I),YC(I)) has index I in original polygon;
!    if IVIS(I) < 0 then (XC(I),YC(I)) is on the edge
!    ending at vertex of index -IVIS(I) in original polygon;
!    indexing starts at 0 from successor of eyepoint.
!    On output, IVIS(0:NVSVRT) contains information about the output
!    vertices of XC, YC arrays as described above for input
!
!    Input, MAXN - upper bound on NVSVRT; should be at least
!    NVIS + INT(PHI/ANGSPC) where PHI is the interior
!    angle at (XEYE,YEYE).
!
!    Output, NVSVRT - (number of visible vertices) - 1.
!
!    Output, THETA(0:NVSVRT) - polar angles of visible vertices with respect to
!    (XEYE, YEYE) at origin and (XC(0),YC(0)) on positive x-axis.
!
  implicit none
!
  integer maxn
!
  double precision alpha
  double precision ang
  double precision ang1
  double precision ang2
  double precision angdif
  double precision angle
  double precision angsp2
  double precision angspc
  double precision cosang
  integer cur
  double precision diff
  double precision dx
  double precision dy
  integer i
  integer ind
  integer ivis(0:maxn)
  integer k
  integer lr
  integer lrline
  integer n
  double precision numer
  integer nvis
  integer nvsvrt
  double precision r
  double precision sinang
  double precision theta(0:maxn)
  double precision tol
  integer top
  double precision xc(0:maxn)
  double precision xeye
  double precision yc(0:maxn)
  double precision yeye
!
!  Shift input vertices right, and possibly remove first and last
!  vertices due to collinearity with eyepoint.
!
  tol = 100.0D+00 * epsilon ( tol )
  angsp2 = 2.0d0*angspc
  cur = maxn + 1
  n = maxn

  do i = nvis,0,-1
    cur = cur - 1
    xc(cur) = xc(i)
    yc(cur) = yc(i)
    ivis(cur) = ivis(i)
  end do

  lr = lrline(xc(cur+1),yc(cur+1),xeye,yeye,xc(cur),yc(cur),0.0d0)

  if (lr >= 0) then
    cur = cur + 1
    xc(0) = xc(cur)
    yc(0) = yc(cur)
    ivis(0) = ivis(cur)
  end if

  lr = lrline(xc(n-1),yc(n-1),xeye,yeye,xc(n),yc(n),0.0d0)

  if ( lr <= 0 ) then
    n = n - 1
  end if

  alpha = atan2(yc(0)-yeye,xc(0)-xeye)
  ang2 = 0.0d0
  theta(0) = 0.0d0
  top = 0
  cur = cur + 1
!
!  Process edge from vertices of indices CUR-1, CUR.
!
20 continue

  ang1 = ang2
  ang2 = angle(xc(cur),yc(cur),xeye,yeye,xc(0),yc(0))
  angdif = ang2 - ang1

  if (angdif <= tol) then

    diff = ((xc(cur) - xeye)**2 + (yc(cur) - yeye)**2) - &
      ((xc(cur-1) - xeye)**2 + (yc(cur-1) - yeye)**2)

    if (diff < 0.0d0) then
      xc(top) = xc(cur)
      yc(top) = yc(cur)
      ivis(top) = ivis(cur)
      theta(top) = ang2
    end if

  else

    if ( angdif >= angsp2) then

      k = int ( angdif / angspc )
      ind = -abs(ivis(cur))
      angdif = angdif / dble(k)
      dx = xc(cur) - xc(cur-1)
      dy = yc(cur) - yc(cur-1)
      numer = (xc(cur) - xeye)*dy - (yc(cur) - yeye)*dx

      do i = 1,k-1
        top = top + 1
        theta(top) = ang1 + dble(i)*angdif
        ang = theta(top) + alpha
        cosang = cos(ang)
        sinang = sin(ang)
        r = numer/(dy*cosang - dx*sinang)
        xc(top) = r*cosang + xeye
        yc(top) = r*sinang + yeye
        ivis(top) = ind
      end do

    end if

    top = top + 1
    xc(top) = xc(cur)
    yc(top) = yc(cur)
    ivis(top) = ivis(cur)
    theta(top) = ang2

  end if

  cur = cur + 1

  if (cur <= n) then
    go to 20
  end if

  nvsvrt = top

  return
end
function volcph ( nface, vcl, hvl, fvl )
!
!******************************************************************************
!
!! VOLCPH computes the volume of a convex polyhedron.
!
!
!  Purpose: 
!
!    Compute volume of convex polyhedron which is stored in
!    convex polyhedron data structure.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NFACE - number of faces in convex polyhedron.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, HVL(1:NFACE) - head vertex list.
!
!    Input, FVL(1:5,1:*) - face vertex list; see routine DSCPH.
!
!    Output, VOLCPH - volume of convex polyhedron.
!
  implicit none
!
  integer nface
!
  integer a
  integer b
  double precision cntr(3)
  double precision cntrf(3)
  integer, parameter :: edgv = 5
  integer, parameter :: facn = 2
  integer fvl(5,*)
  integer hvl(nface)
  integer i
  integer la
  integer lb
  integer, parameter :: loc = 1
  integer n
  integer, parameter :: pred = 4
  integer, parameter :: succ = 3
  double precision vcl(3,*)
  double precision vol
  double precision volcph
  double precision volth
!
!  Compute point CNTR in polyhedron by taking weighted average of
!  vertex coordinates; weight depends on number of occurrences of vertex.
!
  n = 0
  cntr(1) = 0.0d0
  cntr(2) = 0.0d0
  cntr(3) = 0.0d0

  do i = 1,nface

    a = hvl(i)

10  continue

    la = fvl(loc,a)
    n = n + 1
    cntr(1:3) = cntr(1:3) + vcl(1:3,la)
    a = fvl(succ,a)

    if (a /= hvl(i)) then
      go to 10
    end if

  end do

  cntr(1:3) = cntr(1:3)/dble(n)
!
!  Use CNTR to form tetrahedra with each face, and sum up volume
!  of tetrahedra.
!
  vol = 0.0d0

  do i = 1,nface

    n = 0
    cntrf(1:3) = 0.0d0
    a = hvl(i)

30  continue

    la = fvl(loc,a)
    n = n + 1
    cntrf(1:3) = cntrf(1:3) + vcl(1:3,la)
    a = fvl(succ,a)

    if (a /= hvl(i)) then
      go to 30
    end if

    cntrf(1:3) = cntrf(1:3)/dble(n)
    lb = fvl(loc,a)

40  continue

    la = lb
    b = fvl(succ,a)
    lb = fvl(loc,b)
    vol = vol + volth(cntr,cntrf,vcl(1,la),vcl(1,lb))
    a = b

    if (a /= hvl(i)) then
      go to 40
    end if

  end do

  volcph = vol/6.0d0

  return
end
function volth ( a, b, c, d )
!
!******************************************************************************
!
!! VOLTH computes the volume of a tetrahedron.
!
!
!  Purpose: 
!
!    Compute 6 times volume of tetrahedron.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, A(1:3), B(1:3), C(1:3), D(1:3) - 4 vertices of tetrahedron.
!
!    Output, VOLTH - 6 * volume of tetrahedron.
!
  implicit none
!
  double precision a(3)
  double precision b(3)
  double precision c(3)
  double precision d(3)
  integer i
  double precision u(3)
  double precision v(3)
  double precision volth
  double precision w(3)
!
  u(1:3) = b(1:3) - a(1:3)
  v(1:3) = c(1:3) - a(1:3)
  w(1:3) = d(1:3) - a(1:3)

  volth = abs( u(1)*(v(2)*w(3) - v(3)*w(2)) + u(2)*(v(3)*w(1) - &
    v(1)*w(3)) + u(3)*(v(1)*w(2) - v(2)*w(1)) )

  return
end
subroutine vornbr ( xeye, yeye, nvrt, xc, yc, nvor, ivor, xvor, yvor, ierr )
!
!******************************************************************************
!
!! VORNBR determines the Voronoi neighbors of a point.
!
!
!  Purpose: 
!
!    Determine the Voronoi neighbors of (XEYE,YEYE) from a
!    list of vertices which are in increasing "polar angle" order.
!    The Voronoi neighbors are a sublist of this list. The
!    Voronoi polygon is restricted to the sector formed from the
!    the edges joining (XEYE,YEYE) to the first and last vertices
!    of this list. Each Voronoi neighbor corresponds to an edge
!    of the Voronoi polygon.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, XEYE, YEYE - coordinates of eyepoint.
!
!    Input, NVRT - (number of vertices in list) - 1.
!
!    Input, XC(0:NVRT), YC(0:NVRT) - vertex coordinates from which
!    Voronoi neighbors are determined; (XC(0),YC(0)),...,
!    (XC(NVRT),YC(NVRT)) are in increasing angular
!    displacement order with respect to (XEYE,YEYE).
!
!    Output, NVOR - (number of Voronoi neighbors) - 1 [<= NVRT].
!
!    Output, IVOR(0:NVOR) - indices of Voronoi neighbors in XC, YC
!    arrays; 0 <= IVOR(0) < ... < IVOR(NVOR) <= NVRT.
!
!    Workspace, XVOR(0:NVRT), YVOR(0:NVRT) - arrays for storing the vertex
!    coordinates of the Voronoi polygon.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer nvrt
!
  double precision a11
  double precision a12
  double precision a21
  double precision a22
  double precision b1
  double precision b2
  double precision det
  integer ierr
  integer im
  integer ivor(0:nvrt)
  integer k
  integer lr
  integer lrline
  integer m
  integer nvor
  double precision tol
  double precision tolabs
  double precision xc(0:nvrt)
  double precision xeye
  double precision xi
  double precision xvor(0:nvrt)
  double precision yc(0:nvrt)
  double precision yeye
  double precision yi
  double precision yvor(0:nvrt)
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  k = 1
  m = 0
  ivor(0) = 0
  xvor(0) = (xeye + xc(0))*0.5d0
  yvor(0) = (yeye + yc(0))*0.5d0
!
!  Beginning of main loop
!
  do while ( k <= nvrt) 
!
!  Determine the intersection of the perpendicular bisectors
!  of edges from (XEYE,YEYE) to (XC(K),YC(K)) and from
!  (XEYE,YEYE) to (XC(IM),YC(IM)).
!
    im = ivor(m)
    a11 = xc(k) - xeye
    a12 = yc(k) - yeye
    a21 = xc(im) - xeye
    a22 = yc(im) - yeye
    tolabs = tol*max(abs(a11),abs(a12),abs(a21),abs(a22))
    det = a11*a22 - a21*a12

    if (abs(det) <= tolabs) then
      ierr = 212
      return
    end if

    b1 = (a11**2 + a12**2)*0.5d0
    b2 = (a21**2 + a22**2)*0.5d0
    xi = (b1*a22 - b2*a12)/det
    yi = (b2*a11 - b1*a21)/det
!
!  Determine whether (XVOR(M+1),YVOR(M+1)) is to the left of or
!  on the directed line from (XEYE,YEYE) to (XVOR(M),YVOR(M)).
!
    xvor(m+1) = xi + xeye
    yvor(m+1) = yi + yeye
    lr = lrline(xvor(m+1),yvor(m+1),xeye,yeye,xvor(m),yvor(m), 0.0d0)

    if (lr <= 0) then
      m = m + 1
      ivor(m) = k
      k = k + 1
    else if (m > 0) then
      m = m - 1
    else
!
!  Determine the intersection of edge from (XEYE,YEYE) to
!  (XC(0),YC(0)) and the perpendicular bisector of the edge
!  from (XEYE,YEYE) to (XC(K),YC(K)).
!
      a11 = xc(k) - xeye
      a12 = yc(k) - yeye
      a21 = yc(0) - yeye
      a22 = xeye - xc(0)
      tolabs = tol*max(abs(a11),abs(a12),abs(a21),abs(a22))
      det = a11*a22 - a21*a12

      if (abs(det) <= tolabs) then
        ierr = 212
        return
      end if

      b1 = (a11**2 + a12**2)*0.5d0
      b2 = 0.0d0
      xi = (b1*a22 - b2*a12)/det
      yi = (b2*a11 - b1*a21)/det
      xvor(m) = xi + xeye
      yvor(m) = yi + yeye
      ivor(m) = k
      k = k + 1

    end if

  end do
!
!  The following short loop determines which vertices at the end
!  of list are not Voronoi neighbors.
!
20 continue

  lr = lrline(xvor(m),yvor(m),xeye,yeye,xc(nvrt),yc(nvrt),0.0d0)
  if (lr >= 0) go to 30
  m = m - 1
  if (m >= 0) go to 20

30 continue

  nvor = m

  return
end
subroutine vpleft ( xc, yc, ivis )
!
!******************************************************************************
!
!! VPLEFT is called by VISPOL for the LEFT operation.
!
!
!  Purpose: 
!
!    This routine is called by routine VISPOL for the LEFT
!    operation (OPER = 1).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input/output, XC, YC, IVIS - see comments in routine VISPOL.
!
  implicit none
!
  logical beye
  integer cur
  logical intsct
  integer ivis(0:*)
  integer j
  integer lr
  integer lr1
  integer lr2
  integer lrline
  integer nv
  integer oper
  integer top
  double precision xc(0:*)
  double precision xe
  double precision xu
  double precision xw
  double precision yc(0:*)
  double precision ye
  double precision yu
  double precision yw
!
  common /gvpvar/ nv,oper,cur,top,xe,ye,xw,yw,beye
  save /gvpvar/
!
!  EYE-V(CUR-1)-V(CUR) is a left turn, S(TOP) = V(CUR), TOP <= CUR,
!  S(TOP-1) = V(CUR-1) or on interior of edge V(CUR-1)-V(CUR).
!
10 continue

  if (cur == nv) then
    oper = 7
    return
  end if

  if (.not. beye .and. top <= 2) go to 20
!
!  Check if angular displacement of stack chain >= 2*PI or
!  interior angle at boundary viewpoint.
!
  call xedge(1,xe,ye,xc(nv),yc(nv),xc(top-1),yc(top-1),xc(top), &
     yc(top),xu,yu,intsct)

  if (intsct) then

    oper = 4
    xw = xc(cur)
    yw = yc(cur)
    lr = lrline(xc(top),yc(top),xe,ye,xc(nv),yc(nv),0.0d0)

    if (lr == -1) then
      xc(top) = xu
      yc(top) = yu
      ivis(top) = -cur
    end if

    return

  end if
!
!  Process next edge.
!
20 continue

  lr = lrline(xc(cur+1),yc(cur+1),xe,ye,xc(cur),yc(cur),0.0d0)

  if (lr == -1) then

    cur = cur + 1
    top = top + 1
    xc(top) = xc(cur)
    yc(top) = yc(cur)
    ivis(top) = cur

  else

    j = cur + 1
    lr1 = lrline(xc(j),yc(j),xc(top-1),yc(top-1),xc(cur),yc(cur), 0.0d0)

    if (lr1 == 1) then

      oper = 3
      cur = j

    else

      if (lr == 1) then
        lr2 = 1
        go to 40
      end if

30    continue

      j = j + 1
      lr2 = lrline(xc(j),yc(j),xe,ye,xc(cur),yc(cur),0.0d0)
      if (lr2 == 0) go to 30

40    continue

      if (lr2 == -1) then
        top = top + 1
        xc(top) = xc(j-1)
        yc(top) = yc(j-1)
        ivis(top) = j - 1 + nv
        top = top + 1
        xc(top) = xc(j)
        yc(top) = yc(j)
        ivis(top) = j
      else
        oper = 2
      end if

      cur = j

    end if

  end if
!
!  This test avoids extra subroutine calls.
!
  if (oper == 1) then
    go to 10
  end if

  return
end
subroutine vprght ( xc, yc, ivis, ierr )
!
!******************************************************************************
!
!! VPRGHT is called by VISPOL for the RIGHT operation.
!
!
!  Purpose: 
!
!    This routine is called by routine VISPOL for the RIGHT
!    operation (OPER = 2).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input/output, XC, YC, IVIS - see comments in routine VISPOL.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  logical beye
  integer case
  integer cur
  integer ierr
  logical intsct
  integer ivis(0:*)
  integer j
  integer lr
  integer lr1
  integer lr2
  integer lrline
  integer nv
  integer oper
  integer top
  double precision xc(0:*)
  double precision xe
  double precision xu
  double precision xw
  double precision yc(0:*)
  double precision ye
  double precision yu
  double precision yw
!
  common /gvpvar/ nv,oper,cur,top,xe,ye,xw,yw,beye
  save /gvpvar/
!
!  EYE-V(CUR-1)-V(CUR) is a right turn, EYE-S(TOP)-V(CUR) is a right
!  turn, EYE-S(TOP-1)-S(TOP) is a left turn, TOP < CUR, S(TOP) =
!  V(CUR-1) and S(TOP-1)-S(TOP)-V(CUR) is a left turn or S(TOP) is
!  not on edge V(CUR-1)-V(CUR) and V(CUR-1)-V(CUR) intersects
!  EYE-S(TOP).
!  Pop points from stack. If BEYE, it is not possible for
!  (XC(CUR),YC(CUR)) to be identical to any stack points.
!
  ierr = 0

10 continue

  case = 0
  j = top

20 continue

  if (abs(ivis(j)) <= nv) then

    lr = lrline(xc(cur),yc(cur),xe,ye,xc(j-1),yc(j-1),0.0d0)

    if (lr == -1) then

      case = 1

    else if (lr == 0) then

      if (abs(ivis(j-1)) <= nv) then

        j = j - 1
        case = 2

      else if ((xc(j-2) - xe)**2 + (yc(j-2) - ye)**2 >= &
               (xc(j-1) - xe)**2 + (yc(j-1) - ye)**2) then

        j = j - 2
        case = 2

      else

        case = -1

      end if

    end if

  else if (case == -1) then

    if ((xc(j-1) - xe)**2 + (yc(j-1) - ye)**2 >= &
         (xc(cur) - xe)**2 + (yc(cur) - ye)**2) then
      j = j - 1
      case = 2
    else
      xw = xc(cur)
      yw = yc(cur)
      case = 3
    end if

  else

    call xedge(0,xc(cur-1),yc(cur-1),xc(cur),yc(cur), &
           xc(j-1),yc(j-1),xc(j),yc(j),xw,yw,intsct)

    if (intsct) then
      case = 3
    end if

  end if

  if (case > 0) go to 30
  j = j - 1
  if (j >= 1) go to 20
!
!  Error from no more edges in stack.
!
  ierr = 206
  return
!
!  Process next edge.
!
30 continue

  if (case == 3) then

    oper = 6
    top = j - 1

  else

    top = j
    xw = xc(cur-1)
    yw = yc(cur-1)

    if (case == 1) then
      call xedge(1,xe,ye,xc(cur),yc(cur),xc(top-1),yc(top-1), &
           xc(top),yc(top),xu,yu,intsct)
      xc(top) = xu
      yc(top) = yu
      ivis(top) = -abs(ivis(top))
    end if

    lr = lrline(xc(cur+1),yc(cur+1),xe,ye,xc(cur),yc(cur),0.0d0)

    if (lr == 1) then

      cur = cur + 1

    else

      j = cur + 1
      lr1 = lrline(xc(j),yc(j),xw,yw,xc(cur),yc(cur),0.0d0)

      if (lr1 == -1) then

        oper = 5
        cur = j

      else

        if (lr == -1) then
          lr2 = -1
          go to 50
        end if

40      continue

        j = j + 1
        lr2 = lrline(xc(j),yc(j),xe,ye,xc(cur),yc(cur),0.0d0)
        if (lr2 == 0) go to 40

50      continue

        if (lr2 == -1) then
          oper = 1
          top = top + 1
          xc(top) = xc(j-1)
          yc(top) = yc(j-1)
          ivis(top) = j - 1 + nv
          top = top + 1
          xc(top) = xc(j)
          yc(top) = yc(j)
          ivis(top) = j
        end if

        cur = j

      end if

    end if

  end if
!
!  This test avoids extra subroutine calls.
!
  if (oper == 2) go to 10

  return
end
subroutine vpscna ( xc, yc, ivis, ierr )
!
!******************************************************************************
!
!! VPSCNA is called by VISPOL for the SCANA operation.
!
!
!  Purpose: 
!
!    This routine is called by routine VISPOL for the SCANA
!    operation (OPER = 3).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input/output, XC, YC, IVIS - see comments in routine VISPOL.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  logical beye
  integer case
  integer cur
  integer ierr
  logical intsct
  integer ivis(0:*)
  integer j
  integer k
  integer lr
  integer lr1
  integer lr2
  integer lr3
  integer lrline
  integer nv
  integer oper
  integer top
  double precision xc(0:*)
  double precision xe
  double precision xw
  double precision yc(0:*)
  double precision ye
  double precision yw
!
  common /gvpvar/ nv,oper,cur,top,xe,ye,xw,yw,beye
  save /gvpvar/
!
!  EYE-V(CUR-1)-V(CUR) is a right turn or forward move, S(TOP) =
!  V(CUR-1) or EYE-S(TOP)-V(CUR-1) is a forward move and TOP = 0,
!  TOP < CUR; S(TOP-1)-S(TOP)-V(CUR) is a right turn if TOP >= 1
!  or EYE-S(TOP)-V(CUR) is a right turn if TOP = 0.
!  If BEYE, it is possible that (XC(TOP),YC(TOP)) is a non-simple
!  vertex but any edge incident on this vertex encountered during
!  scan must be invisible from (XE,YE).
!
  ierr = 0
  k = cur

10 continue

  if (xc(k+1) == xc(top) .and. yc(k+1) == yc(top)) then

    k = k + 2

  else

    call xedge(1,xe,ye,xc(top),yc(top),xc(k),yc(k),xc(k+1), &
      yc(k+1),xw,yw,intsct)

    if (intsct) then

      lr = lrline(xc(k+1),yc(k+1),xe,ye,xc(k),yc(k),0.0d0)

      if (lr == 1) then

        if ((xc(top) - xe)**2 + (yc(top) - ye)**2 >= &
            (xw - xe)**2 + (yw - ye)**2) then

          if (top > 0) then
            case = 1
            go to 20
          end if

        else

          lr1 = lrline(xc(k),yc(k),xe,ye,xc(top),yc(top),0.0d0)

          if (lr1 == -1) then
            case = 2
            go to 20
          end if

        end if

      else

        lr1 = lrline(xc(k+1),yc(k+1),xe,ye,xc(top),yc(top), 0.0d0)

        if (lr1 == -1) then
          case = 3
          go to 20
        end if

      end if

    end if

    k = k + 1

  end if

  if (k < nv) go to 10
!
!  Error from unsuccessful scan.
!
  ierr = 207
  return
!
!  Process current edge.
!
20 continue

  if (case == 3) then

    oper = 1
    cur = k + 1
    lr = lrline(xc(k),yc(k),xe,ye,xc(top),yc(top),0.0d0)
    top = top + 1

    if (lr == 0) then
      xc(top) = xc(k)
      yc(top) = yc(k)
      ivis(top) = k + nv
    else
      xc(top) = xw
      yc(top) = yw
      ivis(top) = -(k + 1 + nv)
    end if

    top = top + 1
    xc(top) = xc(cur)
    yc(top) = yc(cur)
    ivis(top) = cur

  else if (case == 1) then

    cur = k + 1
    lr = lrline(xc(cur),yc(cur),xe,ye,xc(top),yc(top),0.0d0)

    if (lr == 1) then

      oper = 2

    else

      j = cur + 1
      lr1 = lrline(xc(j),yc(j),xe,ye,xc(cur),yc(cur),0.0d0)
      lr2 = lrline(xc(j),yc(j),xc(k),yc(k),xc(cur),yc(cur),0.0d0)

      if (lr1 <= 0 .and. lr2 == -1) then

        oper = 5
        xw = xc(k)
        yw = yc(k)
        cur = j

      else

        if (lr1 /= 0) then
          lr3 = lr1
          go to 40
        end if

30      continue

        j = j + 1
        lr3 = lrline(xc(j),yc(j),xe,ye,xc(cur),yc(cur),0.0d0)
        if (lr3 == 0) go to 30

40      continue

        if (lr3 == 1) then
          oper = 2
        else
          oper = 1
          top = top + 1
          xc(top) = xc(j-1)
          yc(top) = yc(j-1)
          ivis(top) = j - 1 + nv
          top = top + 1
          xc(top) = xc(j)
          yc(top) = yc(j)
          ivis(top) = j
        end if

        cur = j

      end if

    end if

  else

    oper = 6
    cur = k + 1
    lr = lrline(xc(cur),yc(cur),xe,ye,xc(top),yc(top),0.0d0)

    if (lr == 0) then
      xw = xc(cur)
      yw = yc(cur)
    end if

  end if

  return
end
subroutine vpscnb ( xc, yc, ivis, ierr )
!
!******************************************************************************
!
!! VPSCNB is called by VISPOL for the SCANB operation.
!
!
!  Purpose: 
!
!    This routine is called by routine VISPOL for the SCANB
!    operation (OPER = 4).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input/output, XC, YC, IVIS - see comments in routine VISPOL.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  logical beye
  integer cur
  integer ierr
  logical intsct
  integer ivis(0:*)
  integer k
  integer lr
  integer lr1
  integer lrline
  integer nv
  integer oper
  double precision tol
  double precision tolabs
  integer top
  double precision xc(0:*)
  double precision xe
  double precision xu
  double precision xw
  double precision yc(0:*)
  double precision ye
  double precision yw
  double precision yu
!
  common /gvpvar/ nv,oper,cur,top,xe,ye,xw,yw,beye
  save /gvpvar/
!
!  EYE-V(CUR-1)-V(CUR) is a left turn, S(TOP) = V(CUR) or S(TOP) is
!  on interior of edge V(CUR-1)-V(CUR), TOP <= CUR, S(TOP) has
!  angular displacement of 2*PI or interior angle at boundary eye.
!  (XW,YW) is the input version of (XC(CUR),YC(CUR)).
!  If BEYE, it is possible that (XC(TOP),YC(TOP)) is a non-simple
!  point but any edge containing this point encountered during scan
!  must be invisible from (XE,YE), except for 1 case where K = CUR.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  tolabs = tol*((xc(nv) - xc(top))**2 + (yc(nv) - yc(top))**2)
  k = cur

  if (ivis(top) < 0 .or. k + 1 == nv) then
    go to 10
  end if

  lr = lrline(xc(k+1),yc(k+1),xe,ye,xc(top),yc(top),0.0d0)
  lr1 = lrline(xc(k+1),yc(k+1),xc(top-1),yc(top-1),xc(top),yc(top), 0.0d0)

  if (lr == 1 .and. lr1 == -1) then
    oper = 2
    cur = k + 1
    return
  else
    k = k + 1
  end if

10 continue

  if (k + 1 == nv) then

    oper = 7
    cur = nv
    top = top + 1
    xc(top) = xc(nv)
    yc(top) = yc(nv)
    ivis(top) = nv
    return

  else

    if (k == cur) then
      call xedge(0,xc(nv),yc(nv),xc(top),yc(top),xw,yw, &
        xc(k+1),yc(k+1),xu,yu,intsct)
    else
      call xedge(0,xc(nv),yc(nv),xc(top),yc(top),xc(k),yc(k), &
        xc(k+1),yc(k+1),xu,yu,intsct)
    end if

    if (intsct) then

      if ((xc(top) - xu)**2 + (yc(top) - yu)**2 <= tolabs) then
        go to 20
      end if

      lr = lrline(xc(k+1),yc(k+1),xe,ye,xc(nv),yc(nv),0.0d0)

      if (lr == 1) then
        oper = 2
        cur = k + 1
        return
      end if

    end if

20  continue

    k = k + 1

  end if

  if (k < nv) then
    go to 10
  end if
!
!  Error from unsuccessful scan.
!
  ierr = 208

  return
end
subroutine vpscnc ( xc, yc, ivis, ierr )
!
!******************************************************************************
!
!! VPSCNC is called by VISPOL for the SCANC operation.
!
!
!  Purpose: 
!
!    This routine is called by routine VISPOL for the SCANC
!    operation (OPER = 5).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input/output, XC, YC, IVIS - see comments in routine VISPOL.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  logical beye
  integer cur
  integer ierr
  logical intsct
  integer ivis(0:*)
  integer j
  integer k
  integer lr
  integer lr1
  integer lr2
  integer lrline
  integer nv
  integer oper
  integer top
  double precision xc(0:*)
  double precision xe
  double precision xp
  double precision xu
  double precision xw
  double precision yc(0:*)
  double precision ye
  double precision yp
  double precision yu
  double precision yw
!
  common /gvpvar/ nv,oper,cur,top,xe,ye,xw,yw,beye
  save /gvpvar/
!
!  EYE-V(CUR-1)-V(CUR) is a left turn or forward move, EYE-V(CUR-2)-
!  V(CUR-1) is a right turn, V(CUR-2)-V(CUR-1)-V(CUR) is a left turn,
!  TOP < CUR-1, W = V(CUR-2), S(TOP) is not on V(CUR-1)-V(CUR), EYE-
!  S(TOP)-V(CUR-1) is a backward move, EYE-S(TOP-1)-S(TOP) is a left
!  turn. If BEYE, it is possible that V(CUR-1) is a non-simple point,
!  but intersection at (XC(TOP),YC(TOP)) cannot occur.
!
  ierr = 0
  xp = xc(cur-1)
  yp = yc(cur-1)
  k = cur

10 continue

  if (xc(k+1) == xp .and. yc(k+1) == yp) then

    go to 40

  else if (xc(k) == xp .and. yc(k) == yp) then

    j = k + 1
    lr = lrline(xc(j),yc(j),xe,ye,xp,yp,0.0d0)
    lr1 = lrline(xc(j),yc(j),xw,yw,xp,yp,0.0d0)

    if (lr <= 0 .and. lr1 == -1) go to 40

    if (lr /= 0) then
      lr2 = lr
      go to 30
    end if

20  continue

    j = j + 1
    lr2 = lrline(xc(j),yc(j),xe,ye,xp,yp,0.0d0)
    if (lr2 == 0) go to 20

30  continue

    if (lr2 == 1) then
      oper = 2
    else
      oper = 1
      top = top + 1
      xc(top) = xc(j-1)
      yc(top) = yc(j-1)
      ivis(top) = j - 1 + nv
      top = top + 1
      xc(top) = xc(j)
      yc(top) = yc(j)
      ivis(top) = j
    end if

    cur = j
    return

  else

    call xedge(0,xp,yp,xc(top),yc(top),xc(k),yc(k),xc(k+1), &
      yc(k+1),xu,yu,intsct)

    if (intsct) then

      lr = lrline(xc(k+1),yc(k+1),xe,ye,xp,yp,0.0d0)

      if (lr == 1) then
        oper = 2
        cur = k + 1
        return
      end if

    end if

  end if

40 continue

  k = k + 1
  if (k < nv) go to 10
!
!  Error from unsuccessful scan.
!
  ierr = 209

  return
end
subroutine vpscnd ( xc, yc, ivis, ierr )
!
!******************************************************************************
!
!! VPSCND is called by VISPOL for the SCAND operation.
!
!
!  Purpose: 
!
!    This routine is called by routine VISPOL for the SCAND
!    operation (OPER = 6).
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input/output, XC, YC, IVIS - see comments in routine VISPOL.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  logical beye
  integer cur
  integer ierr
  logical intsct
  integer ivis(0:*)
  integer k
  integer lr
  integer lr1
  integer lr2
  integer lrline
  integer nv
  integer oper
  integer top
  double precision xc(0:*)
  double precision xe
  double precision xp
  double precision xu
  double precision xw
  double precision yc(0:*)
  double precision ye
  double precision yp
  double precision yu
  double precision yw
!
  common /gvpvar/ nv,oper,cur,top,xe,ye,xw,yw,beye
  save /gvpvar/
!
!  EYE-V(CUR-1)-V(CUR) is a right turn, S(TOP) is a V vertex not on
!  V(CUR-1)-V(CUR), TOP < CUR, W is intersection of V(CUR-1)-V(CUR)
!  and ray EYE-S(TOP), EYE-S(TOP)-W is a forward move, and
!  EYE-S(TOP-1)-S(TOP) is a left turn if TOP >= 1.
!  If BEYE, it is possible that (XW,YW) is a non-simple point,
!  but intersection at (XC(TOP),YC(TOP)) cannot occur.
!
  ierr = 0
  xp = xc(cur-1)
  yp = yc(cur-1)
  k = cur

10 continue

  call xedge(0,xw,yw,xc(top),yc(top),xc(k),yc(k),xc(k+1), &
    yc(k+1),xu,yu,intsct)

  if (intsct) then

    lr = lrline(xc(k+1),yc(k+1),xe,ye,xc(k),yc(k),0.0d0)
    lr1 = lrline(xc(k+1),yc(k+1),xe,ye,xc(top),yc(top),0.0d0)

    if (lr == -1 .and. lr1 == -1) then

      if (xc(k) /= xw .or. yc(k) /= yw) go to 20

      lr2 = lrline(xc(k+1),yc(k+1),xp,yp,xw,yw,0.0d0)
      if (lr2 == -1) go to 30

20    continue

      oper = 1
      cur = k + 1
      lr2 = lrline(xc(k),yc(k),xe,ye,xc(top),yc(top),0.0d0)
      top = top + 1

      if (lr2 == 0) then
        xc(top) = xc(k)
        yc(top) = yc(k)
        ivis(top) = k + nv
      else
        xc(top) = xu
        yc(top) = yu
        ivis(top) = -(k + 1 + nv)
      end if

      top = top + 1
      xc(top) = xc(cur)
      yc(top) = yc(cur)
      ivis(top) = cur
      return

    end if

  end if

30 continue

  k = k + 1
  if (k < nv) go to 10
!
!  Error from unsuccessful scan.
!
  ierr = 210

  return
end
subroutine walkt2 ( x, y, ntri, vcl, til, tnbr, itri, iedg, ierr )
!
!******************************************************************************
!
!! WALKT2 walks through a 2D triangulation searching for a point.
!
!
!  Purpose: 
!
!    Walk through neighboring triangles of 2D (Delaunay)
!    triangulation until a triangle is found containing point (X,Y)
!    or (X,Y) is found to be outside the convex hull. Search is
!    guaranteed to terminate for a Delaunay triangulation, else a
!    cycle may occur.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, X, Y - 2D point.
!
!    Input, NTRI - number of triangles in triangulation; used to detect cycle.
!
!    Input, VCL(1:2,1:*) - coordinates of 2D vertices.
!
!    Input, TIL(1:3,1:*) - triangle incidence list.
!
!    Input, TNBR(1:3,1:*) - triangle neighbor list.
!
!    Input/output, ITRI.  On input, the index of triangle to begin search at.
!    On output, the index of triangle that search ends at.
!
!    Output, IEDG - 0 if (X,Y) is in the interior of triangle ITRI; I = 1,
!    2, or 3 if (X,Y) is on interior of edge I of ITRI;
!    I = 4, 5, or 6 if (X,Y) is (nearly) vertex I-3 of ITRI;
!    I = -1, -2, or -3 if (X,Y) is outside convex hull due
!    to walking past edge -I of triangle ITRI.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer a
  double precision alpha
  integer b
  double precision beta
  integer c
  integer cnt
  double precision det
  double precision dx
  double precision dxa
  double precision dxb
  double precision dy
  double precision dya
  double precision dyb
  double precision gamma
  integer i
  integer iedg
  integer ierr
  integer itri
  integer ntri
  integer til(3,*)
  integer tnbr(3,*)
  double precision tol
  double precision vcl(2,*)
  double precision x
  double precision y
!
!  Use barycentric coordinates to determine where (X,Y) is located.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  cnt = 0
  iedg = 0

10 continue

  cnt = cnt + 1

  if (cnt > ntri) then
    ierr = 226
    return
  end if

  a = til(1,itri)
  b = til(2,itri)
  c = til(3,itri)
  dxa = vcl(1,a) - vcl(1,c)
  dya = vcl(2,a) - vcl(2,c)
  dxb = vcl(1,b) - vcl(1,c)
  dyb = vcl(2,b) - vcl(2,c)
  dx = x - vcl(1,c)
  dy = y - vcl(2,c)
  det = dxa*dyb - dya*dxb
  alpha = (dx*dyb - dy*dxb)/det
  beta = (dxa*dy - dya*dx)/det
  gamma = 1.0d0 - alpha - beta

  if (alpha > tol .and. beta > tol .and. gamma > tol) then

    return

  else if (alpha < -tol) then

    i = tnbr(2,itri)
    if (i <= 0) then
      iedg = -2
      return
    end if

  else if (beta < -tol) then

    i = tnbr(3,itri)
    if (i <= 0) then
      iedg = -3
      return
    end if

  else if (gamma < -tol) then

    i = tnbr(1,itri)
    if (i <= 0) then
      iedg = -1
      return
    end if

  else if (alpha <= tol) then

    if (beta <= tol) then
      iedg = 6
    else if (gamma <= tol) then
      iedg = 5
    else
      iedg = 2
    end if
    return

  else if (beta <= tol) then

    if (gamma <= tol) then
      iedg = 4
    else
      iedg = 3
    end if
    return

  else

    iedg = 1
    return

  end if

  itri = i
  go to 10

end
subroutine walkt3 ( pt, n, p, ntetra, vcl, vm, fc, ht, ifac, ivrt, ierr )
!
!******************************************************************************
!
!! WALKT3 walks through a 3D triangulation searching for a point.
!
!
!  Purpose: 
!
!    Walk through neighboring tetrahedra of 3D (Delaunay)
!    triangulation until a tetrahedron is found containing point PT
!    or PT is found to be outside the convex hull. Search is
!    guaranteed to terminate for a Delaunay triangulation, else a
!    cycle may occur.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, PT(1:3) - 3D point.
!
!    Input, N - upper bound on vertex indices or size of VM.
!
!    Input, P - size of hash table.
!
!    Input, NTETRA - number of tetrahedra in triangulation; used to detect
!    cycle.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:N) - vertex mapping list (maps from local indices used in
!    FC to indices of VCL).
!
!    Input, FC(1:7,1:*) - array of face records; see routine DTRIS3.
!
!    Input, HT(0:P-1) - hash table using direct chaining.
!
!    Input/output, IFAC.  On input, index of face of FC to begin search at.
!    On output, index of FC indicating tetrahedron or face containing PT.
!
!    Input/output, IVRT.  On input, 4 or 5 to indicate tetrahedron to 
!    begin search at.  On output, 4 or 5 to indicate that FC(IVRT,IFAC) is 
!    4th vertex of tetrahedron containing PT in its interior; 6 if PT lies
!    in interior of face FC(*,IFAC); 0 if PT lies outside
!    convex hull on other side of face FC(*,IFAC); 1, 2, or 3
!    if PT lies on interior of edge of face from vertices
!    FC(IVRT,IFAC) to FC(IVRT mod 3 + 1,IFAC); -1, -2, or -3
!    if PT is (nearly) vertex FC(-IVRT,IFAC).
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer n
  integer p
!
  integer a
  integer aa
  integer b
  integer bb
  integer c
  integer cnt
  integer d
  logical degen
  integer fc(7,*)
  integer ht(0:p-1)
  integer htsrc
  integer ierr
  integer ifac
  integer ivrt
  integer j
  integer k
  integer ntetra
  double precision pt(3)
  double precision t(4)
  double precision vcl(3,*)
  integer va
  integer vb
  integer vc
  integer vd
  integer vm(n)
  logical zero(4)
!
  ierr = 0
  cnt = 0

10 continue

  if (fc(ivrt,ifac) <= 0) then
    ivrt = 0
    return
  end if

  cnt = cnt + 1

  if (cnt > ntetra) then
    ierr = 307
    return
  end if

  a = fc(1,ifac)
  b = fc(2,ifac)
  c = fc(3,ifac)
  d = fc(ivrt,ifac)
  va = vm(a)
  vb = vm(b)
  vc = vm(c)
  vd = vm(d)
  call baryth(vcl(1,va),vcl(1,vb),vcl(1,vc),vcl(1,vd),pt,t,degen)

  if (degen) then
    ierr = 301
    return
  end if

  if (t(1) > 0.0d0 .and. t(2) > 0.0d0 .and. &
    t(3) > 0.0d0 .and. t(4) > 0.0d0) then

    return

  else if (t(4) < 0.0d0) then

    ivrt = 9 - ivrt

  else if (t(1) < 0.0d0) then

    ifac = htsrc(b,c,d,n,p,fc,ht)
    if (ifac <= 0) go to 30

    if (fc(4,ifac) == a) then
      ivrt = 5
    else
      ivrt = 4
    end if

  else if (t(2) < 0.0d0) then

    ifac = htsrc(a,c,d,n,p,fc,ht)
    if (ifac <= 0) go to 30

    if (fc(4,ifac) == b) then
      ivrt = 5
    else
      ivrt = 4
    end if

  else if (t(3) < 0.0d0) then

    ifac = htsrc(a,b,d,n,p,fc,ht)

    if (ifac <= 0) go to 30

    if (fc(4,ifac) == c) then
      ivrt = 5
    else
      ivrt = 4
    end if

  else
!
!  All T(J) >= 0.0D0 and at least one T(J) == 0.0D0
!
    k = 0

    do j = 1,4
      zero(j) = (t(j) == 0.0d0)
      if (zero(j)) k = k + 1
    end do

    if (k == 1) then

      ivrt = 6

      if (zero(1)) then
        ifac = htsrc(b,c,d,n,p,fc,ht)
        if (ifac <= 0) go to 30
      else if (zero(2)) then
        ifac = htsrc(a,c,d,n,p,fc,ht)
        if (ifac <= 0) go to 30
      else if (zero(3)) then
        ifac = htsrc(a,b,d,n,p,fc,ht)
        if (ifac <= 0) go to 30
      end if

    else if (k == 2) then

      if (zero(4)) then

        if (zero(3)) then
          ivrt = 1
        else if (zero(1)) then
          ivrt = 2
        else
          ivrt = 3
        end if

      else

        if (zero(3)) then

          ifac = htsrc(a,b,d,n,p,fc,ht)
          if (ifac <= 0) go to 30

          if (zero(2)) then
            aa = a
          else
            aa = b
          end if

        else

          ifac = htsrc(a,c,d,n,p,fc,ht)
          if (ifac <= 0) go to 30
          aa = c

        end if

        bb = d

        if (aa > bb) then
          j = aa
          aa = bb
          bb = j
        end if

        if (fc(1,ifac) == aa) then

          if (fc(2,ifac) == bb) then
            ivrt = 1
          else
            ivrt = 3
          end if

        else

          ivrt = 2

        end if

      end if

    else
!
!  K == 3
!
      if (.not. zero(1)) then

        ivrt = -1

      else if (.not. zero(2)) then

        ivrt = -2
 
      else if (.not. zero(3)) then

        ivrt = -3

      else

        ifac = htsrc(a,b,d,n,p,fc,ht)
        if (ifac <= 0) go to 30

        if (fc(1,ifac) == d) then
          ivrt = -1
        else if (fc(2,ifac) == d) then
          ivrt = -2
        else
          ivrt = -3
        end if

      end if

    end if

    return

  end if

  go to 10

30 continue

  ierr = 300

  return
end
subroutine walktk ( k, pt, n, p, nsmplx, vcl, vm, fc, ht, ifac, ivrt, indl, &
  indv, ipvt, alpha, mat, ierr )
!
!******************************************************************************
!
!! WALKTH finds the Delaunay simplex containing a point by "walking".
!
!
!  Purpose: 
!
!    Walk through neighboring simplices of K-D (Delaunay)
!    triangulation until a simplex is found containing point PT
!    or PT is found to be outside the convex hull. Search is
!    guaranteed to terminate for a unique Delaunay triangulation,
!    else a cycle may occur.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, K - dimension of triangulation.
!
!    Input, PT(1:K) - K-D point.
!
!    Input, N - upper bound on vertex indices or size of VM.
!
!    Input, P - size of hash table.
!
!    Input, NSMPLX - number of simplices in triangulation; used to detect
!    cycle.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, VM(1:N) - vertex mapping list (maps from local indices used in
!    FC to indices of VCL).
!
!    Input, FC(1:K+4,1:*) - array of face records; see routine DTRISK.
!
!    Input, HT(0:P-1) - hash table using direct chaining.
!
!    Input/output, IFAC.  On input, index of face of FC to begin search at.
!    On ouput, index of FC indicating simplex or face containing PT.
!
!    Input/output, IVRT.  On input, K+1 or K+2 to indicate simplex to begin
!    search at.  On output, K+1 or K+2 to indicate that FC(IVRT,IFAC) is
!    (K+1)st vertex of simplex containing PT in its interior; 0 if PT
!    is outside convex hull on other side of face FC(*,IFAC);
!    K if PT lies in interior of face FC(*,IFAC); 1 to K-1 if
!    PT lies in interior of facet of FC(*,IFAC) of dim IVRT-1.
!
!    Output, INDV(1:K-1) - if 1 <= IVRT <= K-1 then first IVRT elements are
!    local vertex indices in increasing order of (IVRT-1)-
!    facet containing PT in its interior.
!
!    Workspace, INDL(1:K) - local vertex indices of K-D vertices.
!
!    Workspace, INDV(1:K+1) - indices in VCL of K-D vertices.
!
!    Workspace, IPVT(1:K-1) - pivot indices.
!
!    Workspace, ALPHA(1:K+1) - barycentric coordinates.
!
!    Workspace, MAT(1:K,1:K) - matrix used for solving system of linear 
!    equations.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer k
  integer n
  integer p
!
  integer a
  double precision alpha(k+1)
  integer cnt
  integer d
  logical degen
  integer fc(k+4,*)
  integer ht(0:p-1)
  integer htsrck
  integer i
  integer ierr
  integer ifac
  integer indl(k)
  integer indv(k+1)
  integer ineg
  integer ipvt(k-1)
  integer ivrt
  integer izero
  integer j
  integer kp1
  integer kp2
  double precision mat(k,k)
  integer npos
  integer nsmplx
  integer nzero
  double precision pt(k)
  double precision tol
  double precision vcl(k,*)
  integer vm(n)
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  kp1 = k + 1
  kp2 = k + 2
  cnt = 0

10 continue

  if (fc(ivrt,ifac) <= 0) then
    ivrt = 0
    return
  end if

  cnt = cnt + 1

  if (cnt > nsmplx) then
    ierr = 407
    return
  end if

  indl(1:k) = fc(1:k,ifac)
  indv(1:k) = vm(indl(1:k))

  d = fc(ivrt,ifac)
  indv(kp1) = vm(d)
  call baryck(k,indv,vcl,pt,alpha,degen,mat,ipvt)

  if (degen) then
    ierr = 401
    return
  end if

  npos = 0
  nzero = 0
  izero = 0
  ineg = 0

  do i = 1,kp1

    if (alpha(i) > tol) then
      npos = npos + 1
    else if (alpha(i) < -tol) then
      ineg = i
    else
      nzero = nzero + 1
      izero = i
    end if

  end do

  if (npos == kp1) then

    return

  else if (ineg == kp1) then

    ivrt = kp1 + kp2 - ivrt

  else if (ineg > 0) then

    a = indl(ineg)
    indl(ineg) = d
    ifac = htsrck(k,indl,n,p,fc,ht)

    if (ifac <= 0) then
      ierr = 400
      return
    end if

    if (fc(kp1,ifac) == a) then
      ivrt = kp2
    else
      ivrt = kp1
    end if

  else if (nzero == 1) then

    ivrt = k

    if (izero < kp1) then
      indl(izero) = d
      ifac = htsrck(k,indl,n,p,fc,ht)
      if (ifac <= 0) ierr = 400
    end if

    return

  else

    ivrt = npos
    j = 0

    do i = 1,k
      if (alpha(i) > tol) then
        j = j + 1
        indv(j) = indl(i)
      end if
    end do

    if (izero < kp1) then

      j = npos

50    continue

      if (j > 1) then

        if (indv(j-1) > d) then
          indv(j) = indv(j-1)
          j = j - 1
          go to 50
        end if

      end if

      indv(j) = d
      indl(izero) = d
      ifac = htsrck(k,indl,n,p,fc,ht)

      if (ifac <= 0) then
        ierr = 400
        return
      end if

    end if

    return

  end if

  go to 10

end
subroutine width2 ( nvrt, xc, yc, i1, i2, widsq, ierr )
!
!******************************************************************************
!
!! WIDTH2 determines the width of a convex polygon.
!
!
!  Purpose: 
!
!    Find the width (minimum breadth) of a convex polygon with
!    vertices given in counter clockwise order and with all interior 
!    angles < PI.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NVRT - number of vertices on the boundary of convex polygon.
!
!    Input, XC(1:NVRT), YC(1:NVRT) - vertex coordinates in counter clockwise
!    order.
!
!    Output, I1, I2 - indices in XC,YC such that width is from vertex
!    (XC(I1),YC(I1)) to line joining (XC(I2),YC(I2)) and
!    (XC(I2+1),YC(I2+1)), where index NVRT+1 is same as 1.
!
!    Output, WIDSQ - square of width.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer nvrt
!
  integer a
  double precision area1
  double precision area2
  double precision areatr
  integer b
  integer c
  double precision c1mtol
  double precision c1ptol
  double precision dist
  double precision dx
  double precision dy
  logical first
  integer i1
  integer i2
  integer ierr
  integer j
  integer jp1
  integer k
  integer kp1
  integer m
  double precision tol
  double precision widsq
  double precision xc(nvrt)
  double precision yc(nvrt)
!
!  Find first vertex which is farthest from edge connecting
!  vertices with indices NVRT, 1.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  first = .true.
  c1mtol = 1.0d0 - tol
  c1ptol = 1.0d0 + tol
  j = nvrt
  jp1 = 1
  k = 2
  area1 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(k),yc(k))

10 continue

  area2 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(k+1),yc(k+1))

  if (area2 > area1*c1ptol) then
    area1 = area2
    k = k + 1
    go to 10
  end if

  m = k
  widsq = 0.0d0
!
!  Find width = min distance of antipodal edge-vertex pairs.
!
20 continue

  kp1 = k + 1
  if (kp1 > nvrt) kp1 = 1
  area2 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(kp1),yc(kp1))

  if (area2 > area1*c1ptol) then
    a = j
    b = k
    k = k + 1
    c = k
    if (c > nvrt) c = 1
    area1 = area2
  else if (area2 < area1*c1mtol) then
    a = k
    b = j
    c = jp1
    j = jp1
    jp1 = j + 1
    area1 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(k),yc(k))
  else
    a = k
    b = j
    c = jp1
    k = k + 1
    j = jp1
    jp1 = j + 1
    area1 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(k),yc(k))
  end if

  if (j > m .or. k > nvrt) then

    if (first .and. m > 2) then
!
!  Possibly restart with M decreased by 1.
!
      first = .false.
      m = m - 1
      j = nvrt
      jp1 = 1
      k = m
      area1 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(k),yc(k))
      area2 = areatr(xc(j),yc(j),xc(jp1),yc(jp1),xc(k+1), &
        yc(k+1))

      if (area2 <= area1*(1.0d0+25.0d0*tol)) then
        area1 = area2
        widsq = 0.0d0
        go to 20
      end if

    end if

    ierr = 201
    return

  end if

  dx = xc(c) - xc(b)
  dy = yc(c) - yc(b)
  dist = ((yc(a) - yc(b))*dx - (xc(a) - xc(b))*dy)**2/ (dx**2 + dy**2)

  if (dist < widsq .or. widsq <= 0.0d0) then
    widsq = dist
    i1 = a
    i2 = b
  end if

  if (j /= m .or. k /= nvrt) go to 20

  return
end
subroutine width3 ( nface, vcl, hvl, nrml, fvl, maxiw, i1, i2, wid, iwk, ierr )
!
!******************************************************************************
!
!! WIDTH3 determines the width of a convex polyhedron.
!
!
!  Purpose: 
!
!    Compute width (minimum breadth) of convex polyhedron
!    which is stored in convex polyhedron data structure.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, NFACE - number of faces in convex polyhedron.
!
!    Input, VCL(1:3,1:*) - vertex coordinate list.
!
!    Input, HVL(1:NFACE) - head vertex list.
!
!    Input, NRML(1:3,1:NFACE) - unit outward normals of faces.
!
!    Input, FVL(1:5,1:*) - face vertex list; see routine DSCPH.
!
!    Input, MAXIW - maximum size available for IWK array; should be at
!    least number of edges in polyhedron.
!
!    Output, I1, I2 - indices realizing width, either face in HVL + vertex
!    in FVL if positive, or 2 edges in FVL if negative.
!
!    Output, WID - width of convex polyhedron.
!
!    Workspace, IWK(1:MAXIW) - used for indices of FVL.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxiw
  integer nface
!
  integer a
  integer b
  integer c
  double precision d
  double precision d1
  double precision dir(3)
  double precision dir1(3)
  double precision dist
  double precision dmax
  double precision dotp1
  double precision dotp2
  double precision dtol
  integer, parameter :: edgv = 5
  double precision en(3)
  integer f
  integer, parameter :: facn = 2
  integer ff
  integer fvl(5,*)
  integer g
  integer gg
  integer hvl(nface)
  integer i
  integer i1
  integer i2
  integer ierr
  integer iv
  integer iwk(maxiw)
  integer j
  integer k
  integer la
  integer lb
  integer lc
  integer ld
  double precision leng
  integer, parameter :: loc = 1
  integer ne
  double precision nrml(3,nface)
  double precision nrmlf(3)
  integer nv
  integer, parameter :: pred = 4
  double precision rhs
  integer, parameter :: succ = 3
  double precision tol
  double precision vcl(3,*)
  double precision wid
  double precision wtol
!
!  Determine list of distinct vertices of polyhedron, and store
!  indices of FVL for these vertices in IWK.
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  nv = 0

  do f = 1,nface

    a = hvl(f)

10  continue

    la = fvl(loc,a)

    do i = 1,nv
      if (la == fvl(loc,iwk(i))) go to 30
    end do

    nv = nv + 1

    if (nv > maxiw) then
      ierr = 6
      return
    end if

    iwk(nv) = a

30  continue

    a = fvl(succ,a)
    if (a /= hvl(f)) go to 10

  end do

  ne = nv + nface - 2

  if (ne > maxiw) then
    ierr = 6
    return
  end if
!
!  For each face, find vertex furthest from face and its distance.
!
  wid = 0.0d0

  do f = 1,nface

    la = fvl(loc,hvl(f))
    rhs = nrml(1,f)*vcl(1,la) + nrml(2,f)*vcl(2,la) + nrml(3,f)*vcl(3,la)
    dist = 0.0d0

    do i = 1,nv

      la = fvl(loc,iwk(i))
      d = rhs - nrml(1,f)*vcl(1,la) - nrml(2,f)*vcl(2,la) &
        - nrml(3,f)*vcl(3,la)

      if (d > dist) then
        dist = d
        iv = iwk(i)
      end if

    end do

    if (dist < wid .or. f == 1) then
      wid = dist
      i1 = f
      i2 = iv
    end if

  end do
!
!  Determine list of edges of polyhedron, and store indices of FVL
!  for these edges in IWK.
!
  ne = 0

  do f = 1,nface

    a = hvl(f)
    lb = fvl(loc,a)

70  continue

    la = lb
    b = fvl(succ,a)
    lb = fvl(loc,b)

    if (la < lb) then
      ne = ne + 1
      iwk(ne) = a
    end if

    a = b

    if (a /= hvl(f)) then
      go to 70
    end if

  end do
!
!  For each pair of edges, find parallel planes through edges and
!  determine whether it is antipodal pair.
!
  wtol = tol*wid

  do i = 1,ne-1

    a = iwk(i)
    f = fvl(facn,a)
    ff = fvl(facn,fvl(edgv,a))
    la = fvl(loc,a)
    lb = fvl(loc,fvl(succ,a))
    dir(1) = vcl(1,lb) - vcl(1,la)
    dir(2) = vcl(2,lb) - vcl(2,la)
    dir(3) = vcl(3,lb) - vcl(3,la)
    dmax = max(abs(dir(1)), abs(dir(2)), abs(dir(3)))

    do j = i+1,ne

      c = iwk(j)
      g = fvl(facn,c)
      gg = fvl(facn,fvl(edgv,c))

      if (g == f .or. g == ff .or. gg == f .or. gg == ff) then
        cycle
      end if

      lc = fvl(loc,c)
      ld = fvl(loc,fvl(succ,c))

      if (lc == la .or. lc == lb .or. ld == la .or. ld == lb)  then
        cycle
      end if

      dir1(1:3) = vcl(1:3,ld) - vcl(1:3,lc)
      dtol = tol*max(dmax, abs(dir1(1)), abs(dir1(2)), abs(dir1(3)))
      nrmlf(1) = dir(2)*dir1(3) - dir(3)*dir1(2)
      nrmlf(2) = dir(3)*dir1(1) - dir(1)*dir1(3)
      nrmlf(3) = dir(1)*dir1(2) - dir(2)*dir1(1)
      k = 1
      if (abs(nrmlf(2)) > abs(nrmlf(1))) k = 2
      if (abs(nrmlf(3)) > abs(nrmlf(k))) k = 3

      if (abs(nrmlf(k)) <= dtol) then
        cycle
      end if

      leng = sqrt(nrmlf(1)**2 + nrmlf(2)**2 + nrmlf(3)**2)
      d = nrmlf(1)*vcl(1,la) + nrmlf(2)*vcl(2,la) + &
           nrmlf(3)*vcl(3,la)
      d1 = nrmlf(1)*vcl(1,lc) + nrmlf(2)*vcl(2,lc) + &
           nrmlf(3)*vcl(3,lc)
      dist = abs(d - d1)/leng

      if (dist <= wtol .or. dist >= wid - wtol) then
        cycle
      end if

      nrmlf(1) = nrmlf(1)/leng
      nrmlf(2) = nrmlf(2)/leng
      nrmlf(3) = nrmlf(3)/leng
      en(1) = nrml(2,f)*dir(3) - nrml(3,f)*dir(2)
      en(2) = nrml(3,f)*dir(1) - nrml(1,f)*dir(3)
      en(3) = nrml(1,f)*dir(2) - nrml(2,f)*dir(1)
      dotp1 = (en(1)*nrmlf(1) + en(2)*nrmlf(2) + en(3)*nrmlf(3)) &
        /sqrt(en(1)**2 + en(2)**2 + en(3)**2)
      if (abs(dotp1) <= tol) dotp1 = 0.0d0
      en(1) = nrml(2,ff)*dir(3) - nrml(3,ff)*dir(2)
      en(2) = nrml(3,ff)*dir(1) - nrml(1,ff)*dir(3)
      en(3) = nrml(1,ff)*dir(2) - nrml(2,ff)*dir(1)
      dotp2 = -(en(1)*nrmlf(1) + en(2)*nrmlf(2) + en(3)*nrmlf(3)) &
        /sqrt(en(1)**2 + en(2)**2 + en(3)**2)

      if (abs(dotp2) <= tol) dotp2 = 0.0d0

      if (dotp1*dotp2 < 0.0d0) then
        cycle
      end if

      en(1) = nrml(2,g)*dir1(3) - nrml(3,g)*dir1(2)
      en(2) = nrml(3,g)*dir1(1) - nrml(1,g)*dir1(3)
      en(3) = nrml(1,g)*dir1(2) - nrml(2,g)*dir1(1)
      dotp1 = (en(1)*nrmlf(1) + en(2)*nrmlf(2) + en(3)*nrmlf(3)) &
        /sqrt(en(1)**2 + en(2)**2 + en(3)**2)
      if (abs(dotp1) <= tol) dotp1 = 0.0d0
      en(1) = nrml(2,gg)*dir1(3) - nrml(3,gg)*dir1(2)
      en(2) = nrml(3,gg)*dir1(1) - nrml(1,gg)*dir1(3)
      en(3) = nrml(1,gg)*dir1(2) - nrml(2,gg)*dir1(1)
      dotp2 = -(en(1)*nrmlf(1) + en(2)*nrmlf(2) + en(3)*nrmlf(3)) &
        /sqrt(en(1)**2 + en(2)**2 + en(3)**2)

      if (abs(dotp2) <= tol) dotp2 = 0.0d0

      if (dotp1*dotp2 >= 0.0d0) then
        wid = dist
        i1 = -a
        i2 = -c
      end if

    end do

  end do

  return
end
subroutine xedge ( mode, xv1, yv1, xv2, yv2, xw1, yw1, xw2, yw2, xu, yu, &
  intsct )
!
!******************************************************************************
!
!! XEDGE determines whether two edges, or an edge and a ray intersect.
!
!
!  Purpose: 
!
!    Determine whether two edges or a ray and an edge
!    intersect and return the intersection point if they do.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, MODE - 0 for two edges, 1 (or nonzero) for a ray and an edge.
!
!    Input, XV1, YV1, XV2, YV2, XW1, YW1, XW2, YW2 - vertex coordinates;
!    an edge (ray) is from (XV1,YV1) to (thru) (XV2,YV2);
!    an edge joins vertices (XW1,YW1) and (XW2,YW2).
!
!    Output, XU, YU - coordinates of the point of intersection iff INTSCT
!    is .TRUE.
!
!    Output, INTSCT - .TRUE. if the edges/ray are nondegenerate, not
!    parallel, and intersect, .FALSE. otherwise.
!
  implicit none
!
  double precision denom
  double precision dxv
  double precision dxw
  double precision dyv
  double precision dyw
  logical intsct
  integer mode
  double precision t
  double precision tol
  double precision tolabs
  double precision xu
  double precision xv1
  double precision xv2
  double precision xw1
  double precision xw2
  double precision yu
  double precision yv1
  double precision yv2
  double precision yw1
  double precision yw2
!
  tol = 100.0D+00 * epsilon ( tol )
  intsct = .false.
  dxv = xv2 - xv1
  dyv = yv2 - yv1
  dxw = xw2 - xw1
  dyw = yw2 - yw1
  tolabs = tol*max(abs(dxv),abs(dyv),abs(dxw),abs(dyw))
  denom = dyv*dxw - dxv*dyw
  if (abs(denom) <= tolabs) return
  t = (dyv*(xv1 - xw1) - dxv*(yv1 - yw1))/denom
  if (t < -tol .or. t > 1.0d0 + tol) return
  xu = xw1 + t*dxw
  yu = yw1 + t*dyw

  if (abs(dxv) >= abs(dyv)) then
    t = (xu - xv1)/dxv
  else
    t = (yu - yv1)/dyv
  end if

  if (mode == 0) then
    if (t >= -tol .and. t <= 1.0d0 + tol) intsct = .true.
  else
    if (t >= -tol) intsct = .true.
  end if

  return
end
subroutine xline ( xv1, yv1, xv2, yv2, xw1, yw1, xw2, yw2, dv, dw, &
  xu, yu, parall )
!
!******************************************************************************
!
!! XLINE intersects two lines parallel to given lines.
!
!
!  Purpose: 
!
!    Determine the intersection point of two lines parallel
!    to lines through given points.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, XV1, YV1, XV2, YV2, XW1, YW1, XW2, YW2 - vertex coordinates;
!    first line is parallel to and at signed distance DV to
!    left of directed line from (XV1,YV1) to (XV2,YV2);
!    second line is parallel to and at signed distance DW to
!    left of directed line from (XW1,YW1) to (XW2,YW2).
!
!    Input, DV, DW - signed distances (positive for left).
!
!    Output, XU, YU - coordinates of the point of intersection iff PARALL
!    is .FALSE.
!
!    Output, PARALL - .TRUE. if the lines are parallel or two points for a
!    line are identical, .FALSE. otherwise.
!
  implicit none
!
  double precision a11
  double precision a12
  double precision a21
  double precision a22
  double precision b1
  double precision b2
  double precision det
  double precision dv
  double precision dw
  logical parall
  double precision tol
  double precision tolabs
  double precision xu
  double precision xv1
  double precision xv2
  double precision xw1
  double precision xw2
  double precision yu
  double precision yv1
  double precision yv2
  double precision yw1
  double precision yw2
!
  tol = 100.0D+00 * epsilon ( tol )
  parall = .true.
  a11 = yv2 - yv1
  a12 = xv1 - xv2
  a21 = yw2 - yw1
  a22 = xw1 - xw2
  tolabs = tol*max(abs(a11),abs(a12),abs(a21),abs(a22))
  det = a11*a22 - a21*a12
  if (abs(det) <= tolabs) return
  b1 = xv1*a11 + yv1*a12

  if (dv /= 0.0d0) then
    b1 = b1 - dv*sqrt(a11**2 + a12**2)
  end if

  b2 = xw1*a21 + yw1*a22

  if (dw /= 0.0d0) then
    b2 = b2 - dw*sqrt(a21**2 + a22**2)
  end if

  xu = (b1*a22 - b2*a12)/det
  yu = (b2*a11 - b1*a21)/det
  parall = .false.

  return
end
subroutine xpghpl ( ar, br, dr, g, maxsv, k, head, svcl, sfvl, empty, ierr )
!
!******************************************************************************
!
!! XPGHPL intersects a convex polygon with a half plane.
!
!
!  Purpose: 
!
!    Determine the intersection of a convex polygon in SVCL, SFVL
!    data structure (of routine SHRNK3) with half-plane determined
!    by line G.
!
!  Author:
!
!    Barry Joe, 
!    Department of Computing Science, 
!    University of Alberta,
!    Edmonton, Alberta, Canada  T6G 2H1
!    Email: barry@cs.ualberta.ca
!
!  Parameters:
!
!    Input, AR, BR, DR - half-plane equation is AR*X + BR*Y <= DR.
!
!    Input, G - index of line (actually projection of plane).
!
!    Input, MAXSV - maximum size available for SVCL, SFVL arrays.
!
!    Input/output, K - index of last entry used in SVCL, SFVL arrays.
!
!    Input/output, HEAD - head pointer (index of SFVL) to vertex of 
!    convex polygon.
!
!    Input/output, SVCL(1:3,1:MAXSV), SFVL(1:5,1:MAXSV) - see routine SHRNK3.
!
!    Output, EMPTY - .TRUE. if intersection is empty or degenerate; else
!    .FALSE.
!
!    Output, integer IERR, error flag, which is zero unless an error occurred.
!
  implicit none
!
  integer maxsv
!
  integer a
  double precision ar
  integer b
  double precision br
  double precision dn
  double precision dp
  double precision dr
  double precision dv
  double precision dx
  double precision dy
  integer, parameter :: edgv = 5
  logical empty
  integer f
  integer, parameter :: facn = 2
  integer fdel
  integer fkeep
  integer g
  integer head
  integer i
  integer ierr
  integer ii
  integer ionl
  integer j
  integer jj
  integer k
  integer, parameter :: loc = 1
  integer, parameter :: pred = 4
  integer sfvl(5,maxsv)
  integer, parameter :: succ = 3
  double precision svcl(3,maxsv)
  double precision t
  double precision tol
!
  ierr = 0
  tol = 100.0D+00 * epsilon ( tol )
  empty = .false.
  f = sfvl(facn,head)
  fdel = 0
  fkeep = 0
  ionl = 0
  dv = abs(dr)*tol
  dn = dr - dv
  dp = dr + dv

  if (abs(dr) <= tol) then
    dp = tol + tol
    dn = -dp
  end if
!
!  Determine if intersection is empty, entire polygon, or neither.
!
  i = head

10 continue

  j = sfvl(loc,i)
  dv = ar*svcl(1,j) + br*svcl(2,j)

  if (dv < dn) then

    if (fkeep == 0) fkeep = i

  else if (dv > dp) then

    if (fdel == 0) fdel = i
    sfvl(loc,i) = -j
    sfvl(facn,i) = 0
!
!  A deleted vertex has a negative LOC value and 0 FACN value.
!
   else
      ionl = i
      sfvl(facn,i) = -f
!
!  A vertex lying on line has a negative FACN value.
!
   end if

  i = sfvl(succ,i)
  if (i /= head) go to 10

  if (fdel == 0) then
    if(ionl /= 0) then
      sfvl(facn,ionl) = f
      i = sfvl(succ,ionl)
      sfvl(facn,i) = f
      i = sfvl(pred,ionl)
      sfvl(facn,i) = f
    end if
    return
  else if (fkeep == 0) then
    empty = .true.
    head = 0
    return
  end if
!
!  Update polygon to include part of line and remove >= 1 vertices.
!
  i = fdel

20 continue

  i = sfvl(pred,i)
  j = sfvl(loc,i)
  if (j < 0) go to 20

  if (sfvl(facn,i) < 0) then

    a = i
    sfvl(facn,i) = f

  else

    ii = sfvl(succ,i)
    jj = -sfvl(loc,ii)
    dx = svcl(1,jj) - svcl(1,j)
    dy = svcl(2,jj) - svcl(2,j)
    t = (dr - ar*svcl(1,j) - br*svcl(2,j))/(ar*dx + br*dy)

    if (t <= tol .or. t >= 1.0d0 - tol) then
      ierr = 313
      return
    end if

    if (ii /= fdel) then

      a = ii

    else

      k = k + 1

      if (k > maxsv) then
        ierr = 13
        return
      end if

      a = k
      sfvl(pred,k) = i
      sfvl(succ,i) = k
      jj = k

    end if

    svcl(1,jj) = svcl(1,j) + t*dx
    svcl(2,jj) = svcl(2,j) + t*dy
    svcl(3,jj) = 0.0d0
    sfvl(loc,a) = jj
    sfvl(facn,a) = f

  end if

  i = fdel

  do

    i = sfvl(succ,i)
    j = sfvl(loc,i)

    if ( j >= 0 ) then
      exit
    end if

  end do

  if ( sfvl(facn,i) < 0 ) then

    b = i
    sfvl(facn,i) = f

  else

    ii = sfvl(pred,i)
    jj = -sfvl(loc,ii)
    b = ii
    dx = svcl(1,jj) - svcl(1,j)
    dy = svcl(2,jj) - svcl(2,j)
    t = ( dr - ar * svcl(1,j) - br * svcl(2,j) ) / ( ar * dx + br * dy )

    if ( t <= tol .or. t >= 1.0d0 - tol ) then
      ierr = 313
      return
    end if

    svcl(1,jj) = svcl(1,j) + t * dx
    svcl(2,jj) = svcl(2,j) + t * dy
    sfvl(loc,b) = jj
    sfvl(facn,b) = f

  end if

  sfvl(succ,a) = b
  sfvl(pred,b) = a
  sfvl(edgv,a) = -g
  head = a

  return
end
