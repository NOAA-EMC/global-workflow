!                               **************************************
!                               *           Module phil2             *
!                               *   R. J. Purser NOAA/NCEP/EMC 2017  *
!                               *       jim.purser@noaa.gov          *
!                               **************************************
!
! Module procedures pertaining to the project, sorting, and density
! estimation by B-spline smoothing the index delta functions of projected
! locations. The particular application that makes this approach potentially
! advantageous compared to conventional alternatives is the estimation
! of the local spatial density of data in those cases where the data tend to be
! inhomogeneously clumped, such as aircraft data, which are typically
! clustered around densely populated areas and along the heavily-trafficked
! flight tracks.
!
! Direct dependencies
! Libraries: psort,pmat
! Modules:   phil,psort,peuc,kinds, pietc
!
!=============================================================================
module phil2
!=============================================================================
! B-spline smoothers of degree 0 and 1 are provided by module procedures
! whose interface names are bsmoo0 and bsmoo1 respectively. The versions
! bsmoo0s and bsmoo1s are special forms that assume that the data locations
! have already been presorted and arranged consecutively; otherwise, the
! versions bsmoo0 and bsmoo1 include an integer index array argument, rank, that
! allows the ordered data to be accessed by indirect addressing.
! The routine, denest, performs such volumetric density estimations in a thin
! spherical shell nrand times and avergaes the results, where nrand can be
! any integerup to 30, or 100, or 300, and uses precomputed pseudo-random
! orientations of the cubic framework within which the hilbert curve is
! constructed over the sphere. The vertical randomization is performed by
! steps that are regular powers of (1/3) times one quarter of the active
! thickness (containing all the data) of the shell. 
! The precomputed random rotations are contained in the ascii file, qsets.asc,
! encoded as quaternions.
!=============================================================================
use kinds, only: dp,i_kind
implicit none
private
public:: bsmoo0,bsmoo1,denest,denest2d,&
         getqset5,getqset7,getqset8,getqset13 ! <- temporarily public

interface bsmoo0;   module procedure bsmoo0,bsmoo0s;     end interface
interface bsmoo1;   module procedure bsmoo1,bsmoo1s;     end interface
interface denest;   module procedure denest,denestx;     end interface
interface denest2d; module procedure denest2d,denest2dx; end interface
interface getqset5; module procedure getqset5;           end interface
interface getqset7; module procedure getqset7;           end interface
interface getqset8; module procedure getqset8;           end interface
interface getqset13;module procedure getqset13;          end interface

contains

!=============================================================================
subroutine bsmoo0(nob,span,sob,rank,dob)!                             [bsmoo0]
!=============================================================================
! Perform a smoothing of an irregular one-dimensional distribution of nob unit
! impulses at parameter locations, sob, using a boxcar function of total width, 
! span. The resulting 'density' at each datum location is dob. Note that this
! is the trivial instance of B-spline smoothing where the degree of the spline
! is zero.
!=============================================================================
implicit none
integer(i_kind),                intent(in   ):: nob
real(dp),                       intent(in   ):: span
real(dp),        dimension(nob),intent(in   ):: sob
integer(i_kind) ,dimension(nob),intent(in   ):: rank
real(dp),        dimension(nob),intent(inout):: dob
!-----------------------------------------------------------------------------
real(dp):: spanh,st,s1,s2
integer(i_kind) :: i,j,i1,i2,L1,L2
!=============================================================================
spanh=span/2.0_dp
i1=1
i2=1
do i=1,nob
   j=rank(i)
   st=sob(j); s1=st-spanh; s2=st+spanh
   L1=i1; L2=i2
   do i1=l1,nob; if(sob(rank(i1))>s1)exit; enddo
   do i2=l2,nob; if(sob(rank(i2))>s2)exit; enddo
   dob(j)=dob(j)+(i2-i1)/span
enddo
end subroutine bsmoo0

!=============================================================================
subroutine bsmoo0s(nob,span,sob,dob)!                                 [bsmoo0]
!=============================================================================
implicit none
integer(i_kind),        intent(in   ):: nob
real(dp),               intent(in   ):: span
real(dp),dimension(nob),intent(in   ):: sob
real(dp),dimension(nob),intent(inout):: dob
!-----------------------------------------------------------------------------
real(dp):: spanh,st,s1,s2
integer(i_kind) :: i,i1,i2,L1,L2
!=============================================================================
spanh=span/2.0_dp
i1=1
i2=1
do i=1,nob
   st=sob(i); s1=st-spanh; s2=st+spanh
   L1=i1; L2=i2
   do i1=l1,nob; if(sob(i1)>s1)exit; enddo
   do i2=l2,nob; if(sob(i2)>s2)exit; enddo
   dob(i)=dob(i)+(i2-i1)/span
enddo
end subroutine bsmoo0s

!=============================================================================
subroutine bsmoo1(nob,span,sob,rank,dob)!                             [bsmoo1]
!=============================================================================
! Perform a smoothing of an irregular one-dimensional distribution of nob unit
! impulses at parameter locations, sob, using a hat function of half-width, 
! span. The resulting 'density' at each datum location is dob. Note that this
! is the instance of a B-spline smoothing where the degree of the spline
! is one.
!
! The general idea of this algorithm is that a piecewise linear spline p(s) 
! continuously links nodes at each sb=sob(rank(ib)), 
! where the slope changes by +1.
! The distribution of delta functions smoothed by the B-spline of half-width
! span leads to a smooth estimate, dob, at each location, s=sb, given by 
!      dob(ib) = [p(sb-span) -2*p(sb) +p(sb+span)]/span**2
!
! However, because this formula, as stated, would tend to involve a "small"
! result coming from differences between approximately equally "large" numbers
! when the count, nob, of data is very large, it is preferable to 
! transform the algorithm into a better-conditioned equivalent one in order
! to keep the round-off errors small. We do this by exploiting the fact that
! the result is formally unchanged by adding ANY linear polynomial uniformly
! to the whole spline function, p(s). We choose this polynomial, at each
! stage, to maintain the adjusted spline function p just left of each targetted 
! datum, ib, the zero function, and therefore the spline function just right
! of it equal to p(s) =  (s-sb), where sb denotes the location of datum ib.
! This keeps the pairs of numerical polynomial coefficients at sa=sb-span and at
! sc=sb+span relatively small. The "slope" coefficient of each linear
! polynomial is always integral, and is therefore not subject to any round-off
! error, but the floating-point constant coefficients, pa0 and pc0, are
! in principle subject to cumulative round-off in cases where the number, nob,
! of data is very large. As a safeguard, we shrink the new pa0 and pc0 values 
! towards zero by a very tiny amount at each new datum, but by a degree that 
! should be enough to counteract any tendency for cumulative round-off to cause
! their values to spuriously diverge.
!=============================================================================
use pietc, only: u1
use kinds, only: dp,i_kind
implicit none
integer(i_kind),                intent(in   ):: nob
real(dp),                       intent(in   ):: span
real(dp),        dimension(nob),intent(in   ):: sob
integer(i_kind), dimension(nob),intent(in   ):: rank
real(dp),        dimension(nob),intent(inout):: dob
!-----------------------------------------------------------------------------
real(dp),parameter:: eps=1.0e-13_dp,shrink=1.0_dp-eps
real(dp)          :: sa,sb,sc,pa0,pc0,sbold,ds,soba,sobc,spansi
integer(i_kind)   :: ia,ib,ic,jb,La,Lc,pa1,pb1,pc1
!=============================================================================
spansi=u1/span**2
ia=1
ic=1
sbold=0
! Initially, all the polynomial coefficients vanish:
pa0=0.0_dp
pa1=0
pb1=0
pc0=0.0_dp
pc1=0
do ib=1,nob
   jb=rank(ib)
   sb=sob(jb); sa=sb-span; sc=sb+span
   ds=sb-sbold; sbold=sb
! Move the new coordinate origin to location, sb=sob(rank(ib)) and
! subtract from polynomials A and C the (old) left-polynomial at b:
   pa1=pa1-pb1
   pc1=pc1-pb1
   pa0=(pa0+ds*pa1)*shrink ! Shrink by imperceptible amount to stabilize
   pc0=(pc0+ds*pc1)*shrink ! huge computations against cumulative round-off
!  New pb0 is always taken implicitly to be zero and new pb1 becomes simply:   
   pb1=1! < (new) rt-polynomial p(s) at B is trivially p=(s-sob(rank(ib)))
! Update the new locations of A and C (straddling B by distance, span) and
! the corresponding polynomial coefficients, {pa0,pa1} and {pc0,pc1} valid 
! there:
   La=ia; Lc=ic
   do ia=La,nob
      soba=sob(rank(ia)); if(soba>sa)exit; pa0=pa0+sb-soba; pa1=pa1+1
   enddo
   do ic=Lc,nob
      sobc=sob(rank(ic)); if(sobc>sc)exit; pc0=pc0+sb-sobc; pc1=pc1+1
   enddo
! The formula, [p(sb-span) -2*p(sb) +p(sb+span)], simplies to: 
   dob(jb)=dob(jb)+(pa0+pc0+span*(pc1-pa1))*spansi
enddo
end subroutine bsmoo1
!=============================================================================
subroutine bsmoo1s(nob,span,sob,dob)!                                 [bsmoo1]
!=============================================================================
use pietc, only: u1
use kinds, only: dp,i_kind
implicit none
integer(i_kind),        intent(in   ):: nob
real(dp),               intent(in   ):: span
real(dp),dimension(nob),intent(in   ):: sob
real(dp),dimension(nob),intent(inout):: dob
!-----------------------------------------------------------------------------
real(dp),parameter:: eps=1.e-13_dp,shrink=1.0_dp-eps
real(dp)          :: sa,sb,sc,pa0,pc0,sbold,ds,spansi
integer(i_kind)           :: ia,ib,ic,La,Lc,pa1,pb1,pc1
!=============================================================================
spansi=u1/span**2
ia=1
ic=1
sbold=0.0_dp
! Initially, all the polynomial coefficients vanish:
pa0=0.0_dp
pa1=0
pb1=0
pc0=0.0_dp
pc1=0
do ib=1,nob
   sb=sob(ib); sa=sb-span; sc=sb+span
   ds=sb-sbold; sbold=sb
! Move the new coordinate origin to location, sb=sob(ib) and
! subtract from polynomials A and C the (old) left-polynomial at b:
   pa1=pa1-pb1
   pc1=pc1-pb1
   pa0=(pa0+ds*pa1)*shrink ! Shrink by imperceptible amount to stabilize
   pc0=(pc0+ds*pc1)*shrink ! huge computations against cumulative round-off
!  New pb0 is always taken implicitly to be zero and new pb1 becomes simply:   
   pb1=1! < (new) rt-polynomial p(s) at B is trivially p=(s-sob(ib))
! Update the new locations of A and C (straddling B by distance, span) and
! the corresponding polynomial coefficients, {pa0,pa1} and {pc0,pc1} valid 
! there:
   La=ia; Lc=ic
   do ia=La,nob; if(sob(ia)>sa)exit; pa0=pa0+sb-sob(ia); pa1=pa1+1; enddo
   do ic=Lc,nob; if(sob(ic)>sc)exit; pc0=pc0+sb-sob(ic); pc1=pc1+1; enddo
! The formula, [p(sb-span) -2*p(sb) +p(sb+span)], simplies to: 
   dob(ib)=dob(ib)+(pa0+pc0+span*(pc1-pa1))*spansi
enddo
end subroutine bsmoo1s

!=============================================================================
subroutine denest(nob,nrand,nor, &!                                   [denest]
                  re,dentrip,hscale,vscale,vmin,vmax,&
                  latob,lonob,altob,wtob)
!=============================================================================
! Use nrand randomized Hilbert curves over a sufficiently thick spherical
! shell to estimate the density of data relative to the "unit" volume
! implied by the vertical smoothing scale time the square of the horizontal
! smoothing scale. 
!
! nob:    the number of data
! nrand:  the number of distinct pseudo-random Hilbert curves
! nor:    the order of the B-spline smoother (either 0 or 1)
! re:     the nominal radius of the earth
! dentrip:density criterion tripping the downward adjustment to the wt factor
! hscale: (in the same units) the characteristic horizontal scale of averaging
! vscale: (in the same units) the characteristic vertical scale of averaging
! vmin:   (in same units) lowest altitude bound for valid data
! vmax:   (in same units) greatest altitude bound for valid data 
! latob:  array of observation latitudes (degrees north)
! lonob:  array of observation longitudes (degrees east)
! altob:  array of observation altitudes (same units as other distance scales)
! troflg: logic parameter to determin wether dentrip in tropic different from
!         other region
! wtob:   Weight factor implied by the estimated data density and dentrip
!
! Internally, it is convenient to relate all distance units to "Hilbert
! parameter units" where 24 of these units fill the 24 fundamental 
! quadrilateral panels that form the horizontal footprint
! of the spherical shell. This means that one Hilbert unit corresponds to
! a horizontal distance of sqrt(pi/6)*re, or about Uh=4610 km. The thickness,
! in Hilbert distance, of the valid vertical range is
! (vmax-vmin)*hscale/(uh*vscale), which must be less than one for the
! present "thin shell" Hilbert curve construction. We inflate this thickness
! by a small margin to leave room for vertical location randomization of
! the Hilbert curves, and round up to the next integer(i_kind) power, ngen4, of 1/2.
! Ngen4 effectively determines the generation at which the Hilbert curve
! transitions from being 2-dimensional (at coarse scales) to being 
! 3-dimensional (at and below the scale of the effective thickness of the 
! shell that the curve fills). For generations, 1--ngen4, the expansion is
! at base-4; at ngen4--ngen the expansion is base-8.
!=============================================================================
use pietc, only: u0,u1,u2,o2,pi,dtor
use kinds, only: dp,i_kind
use peuc,  only: qtorot,mulqq
use phil0, only: xs_to_hil48,hil8_to_r,hil4_to_r
use psort, only: sort, invertperm
use qcmod, only:troflg,lat_c
implicit none
integer(i_kind),        intent(in ):: nob,nrand,nor
real(dp),               intent(in ):: re,dentrip,hscale,vscale,vmin,vmax
real(dp),dimension(nob),intent(in ):: latob,lonob,altob
real(dp),dimension(nob),intent(out):: wtob
!-----------------------------------------------------------------------------
integer(i_kind),parameter         :: ngen=14
real(dp),       dimension(3,nob)  :: xob
real(dp),       dimension(nob)    :: rob,sob,latobc
real(dp),       dimension(0:3)    :: qset
real(dp),       dimension(0:3,3)  :: qset3
real(dp),       dimension(0:3,5)  :: qset5
real(dp),       dimension(0:3,7)  :: qset7
real(dp),       dimension(0:3,13) :: qset13
real(dp),       dimension(3,3)    :: rot,rotnew,rotold
real(dp)                          :: span,uv,uh,vh,vhq,vhp,v,vrand,&
                                     rlat,clat,slat,rlon,clon,slon
integer(i_kind),dimension(nob)    :: rank
integer(i_kind),dimension(0:ngen) :: hilr
integer(i_kind)                   :: i,irand,j,k,L,ngen4,ntri,dentripc
!=============================================================================
uh=sqrt(pi/6.0_dp)*re
uv=(uh*vscale)/hscale
vh=(vmax-vmin)/uv
vhq=vh/4.0_dp
vhp=vh+vhq
if(vhp>=u1)then
   print&
    '("In denest; vmax-vmin too large for thin-shell assumption to be valid")'
   print'("Make this vertical range smaller, or vscale/hscale larger")'
   stop
endif

v=o2
do ngen4=0,ngen-1
   if(v<vhp)exit
   v=v/2.0_dp
enddo
v=v*2.0_dp ! <- Shell thickness in Hilbert units
!print'('' ngen4 = '',i4)',ngen4
! Find the Hilbert curve parameter span that corresponds to the characteristic
! smoothing volumne, hscale**2*vscale:
span=(u2**ngen4)*(hscale/uh)**3

! Find the first power of three that equals or exceeds nrand:
do ntri=0,8
   i=3**ntri
   if(i>=nrand)exit
enddo
vrand=vhq/i
!vrand=3*vhq/i

! Convert lat and lon to more convenient unit cartesian vectors:
do L=1,nob
   rlat=latob(L)*dtor; clat=cos(rlat); slat=sin(rlat)
   if(troflg) latobc(L)=latob(L)
   rlon=lonob(L)*dtor; clon=cos(rlon); slon=sin(rlon)
   xob(:,L)=(/clat*clon,clat*slon,slon/)
   rob(L)=(altob(L)-vmin)/uv-vrand! <- altitudes in hilbert vertical units
enddo
if(nrand<1 .or. nrand>273)stop 'nrand is invalid'
if(nrand>5)then; call getqset7(      qset7); if(nrand>7)call getqset13(qset13)
else;            call getqset5(nrand,qset5)
endif
if(nrand>91)     call getqset5(3,qset3(:,:))


! Project the data onto nrand differently-oriented Hilbert curves and sum
! the different estimated density estimates at the ob points in array wtob:
rotnew=u0; do i=1,3; rotnew(i,i)=u1; enddo ! <- Identity matrix.
wtob=0.0_dp
do irand=1,nrand
   rotold=rotnew
   select case(nrand)
      case(1:5)
         qset=qset5(:,irand)
      case(6:7)
         qset=qset7(:,irand)
      case(8:13)
         qset=qset13(:,irand)
      case(14:91)
         i=1+mod(irand-1,13)
         j=1+(irand-1)/13
         qset=mulqq(qset7(:,j),qset13(:,i))
      case(92:273)
         i=1+mod(irand-1,13)
         j=1+(irand-1)/13
         k=1+(j-1)/7
         j=1+mod(j-1,7)
         qset=mulqq(mulqq(qset13(:,i),qset3(:,k)),qset7(:,j))
   end select

   call qtorot(qset,rotnew) ! convert to an orthogonal matrix
! Form the relative rotation, rot (relative to previous irand iteration):
   rot=matmul(rotnew,transpose(rotold))

! Get a new fraction of vh/4, based on ternary subdivisions such that the
! binary expansions (in units of vh) are non-terminating. This helps ensure
! that the resulting Hilbert curve projections are not accidentally similar.
   do L=1,nob
      xob(:,L)=matmul(rot,xob(:,L))
      rob(L) =rob(L)+vrand ! Randomize by new vertical displacement
   enddo
   do L=1,nob
      sob(L)=0_dp
      call xs_to_hil48(ngen4,ngen,xob(:,L),rob(L), hilr)
      call hil8_to_r(ngen4+1,ngen,hilr(ngen4+1:ngen),sob(L))
      call hil4_to_r(1,ngen4,hilr(1:ngen4),sob(L))
      sob(L)=sob(L)+hilr(0)
   enddo! L
! Sort the data implicitly by assigning each a "rank":
   call sort(sob,rank); call invertperm(rank)
   select case(nor)
      case(0)
         call bsmoo0(nob,span,sob,rank,wtob)
      case(1)
         call bsmoo1(nob,span,sob,rank,wtob)
      case default
         stop 'In denest; this value of B-spline order, nor, is not supported'
   end select
enddo! irand
! Convert the sum of Hilbert-parameter-relative densities to an average,
! and convert the units of density to number-per-span:
wtob=wtob*span/nrand
do L=1,nob
   dentripc=dentrip
   if(troflg) then
      if(abs(latobc(L)) <lat_c) then
         dentripc=2*dentrip
      else
         dentripc=dentrip
      endif
   endif   
   wtob(L)=min(u1,dentripc/wtob(L))
enddo

end subroutine denest
!=============================================================================
subroutine denestx(nob,nrand,nor, &!                                  [denest]
     re,dentrip,hscale,vscale,vmin,vmax,&
     latob,lonob,altob,wtob,denob)
!=============================================================================
! Like denest, but also returns the diagnosed density at all ob points
! denob:  array of observation densities, per unit scale volume, where the
!         scale volume is simply hscale**2*vscale.
!=============================================================================
use pietc, only: u0,u1,u2,o2,pi,dtor
use kinds, only: dp,i_kind
use peuc,  only: qtorot,mulqq
use phil0, only: xs_to_hil48,hil8_to_r,hil4_to_r
use psort, only: sort, invertperm
use qcmod, only: troflg,lat_c
implicit none
integer(i_kind),        intent(in ):: nob,nrand,nor
real(dp),               intent(in ):: re,dentrip,hscale,vscale,vmin,vmax
real(dp),dimension(nob),intent(in ):: latob,lonob,altob
real(dp),dimension(nob),intent(out):: wtob,denob
!-----------------------------------------------------------------------------
integer(i_kind),parameter        :: ngen=14
real(dp),       dimension(3,nob) :: xob
real(dp),       dimension(nob)   :: rob,sob,latobc
real(dp),       dimension(0:3)   :: qset
real(dp),       dimension(0:3,5) :: qset5
real(dp),       dimension(0:3,8) :: qset8
real(dp),       dimension(0:3,13):: qset13
real(dp),       dimension(3,3)   :: rot,rotnew,rotold
real(dp)                         :: span,uv,uh,vh,vhq,vhp,v,vrand,&
                                    rlat,clat,slat,rlon,clon,slon
integer(i_kind),dimension(nob)   :: rank
integer(i_kind),dimension(0:ngen):: hilr
integer(i_kind)                  :: i,irand,j,L,ngen4,ntri,dentripc
!=============================================================================
uh=sqrt(pi/6.0_dp)*re
uv=(uh*vscale)/hscale
vh=(vmax-vmin)/uv
vhq=vh/4.0_dp
vhp=vh+vhq
if(vhp>=u1)then
   print&
    '("In denest; vmax-vmin too large for thin-shell assumption to be valid")'
   print'("Make this vertical range smaller, or vscale/hscale larger")'
   stop
endif

v=o2
do ngen4=0,ngen-1
   if(v<vhp)exit
   v=v/2.0_dp
enddo
v=v*2.0_dp ! <- Shell thickness in Hilbert units
!print'('' ngen4 = '',i4)',ngen4
! Find the Hilbert curve parameter span that corresponds to the characteristic
! smoothing volumne, hscale**2*vscale:
span=(u2**ngen4)*(hscale/uh)**3

! Find the first power of three that equals or exceeds nrand:
do ntri=0,8
   i=3**ntri
   if(i>=nrand)exit
enddo
vrand=3.0_dp*vhq/i

! Convert lat and lon to more convenient unit cartesian vectors:
do L=1,nob
   if(troflg) latobc(L)=latob(L)
   rlat=latob(L)*dtor; clat=cos(rlat); slat=sin(rlat)
   rlon=lonob(L)*dtor; clon=cos(rlon); slon=sin(rlon)
   xob(:,L)=(/clat*clon,clat*slon,slon/)
   rob(L)=(altob(L)-vmin)/uv-vrand! <- altitudes in hilbert vertical units
enddo
if(nrand<1 .or. nrand>104)stop 'nrand is invalid'
if(nrand>5)then; call getqset8(      qset8); if(nrand>8)call getqset13(qset13)
else;            call getqset5(nrand,qset5)
endif

! Project the data onto nrand differently-oriented Hilbert curves and sum
! the different estimated density estimates at the ob points in array denob:
rotnew=u0; do i=1,3; rotnew(i,i)=u1; enddo ! <- Identity matrix.
denob=0_dp
do irand=1,nrand
   rotold=rotnew
   select case(nrand)
      case(1:5)
         qset=qset5(:,irand)
      case(6:8)
         qset=qset8(:,irand)
      case(9:13)
         qset=qset13(:,irand)
      case(14:104)
         i=1+mod(irand-1,13)
         j=1+(irand-1)/13
         qset=mulqq(qset8(:,j),qset13(:,i))
   end select

   call qtorot(qset,rotnew) ! convert to an orthogonal matrix
! Form the relative rotation, rot (relative to previous irand iteration):
   rot=matmul(rotnew,transpose(rotold))

! Get a new fraction of vh/4, based on ternary subdivisions such that the
! binary expansions (in units of vh) are non-terminating. This helps ensure
! that the resulting Hilbert curve projections are not accidentally similar.
   do L=1,nob
      xob(:,L)=matmul(rot,xob(:,L))
      rob(L) =rob(L)+vrand ! Randomize by new vertical displacement
   enddo
   do L=1,nob
      sob(L)=0_dp
      call xs_to_hil48(ngen4,ngen,xob(:,L),rob(L), hilr)
      call hil8_to_r(ngen4+1,ngen,hilr(ngen4+1:ngen),sob(L))
      call hil4_to_r(1,ngen4,hilr(1:ngen4),sob(L))
      sob(L)=sob(L)+hilr(0)
   enddo! L
! Sort the data implicitly by assigning each a "rank":
   call sort(sob,rank); call invertperm(rank)
   select case(nor)
      case(0)
         call bsmoo0(nob,span,sob,rank,denob)
      case(1)
         call bsmoo1(nob,span,sob,rank,denob)
      case default
         stop 'In denest; this value of B-spline order, nor, is not supported'
   end select
enddo! irand
! Convert the sum of Hilbert-parameter-relative densities to an average,
! and convert the units of density to number-per-span:
denob=denob*span/nrand
do L=1,nob
   dentripc=dentrip
   if(troflg) then
      if(abs(latobc(L)) <lat_c) then
         dentripc=2*dentrip
      else
         dentripc=dentrip
      endif
   endif   
   wtob(L)=min(u1,dentripc/wtob(L))
enddo

end subroutine denestx

!=============================================================================
subroutine denest2d(nob,nrand,nor,dentrip,scale,& !                 [denest2d]
                    xmin,xmax,ymin,ymax, &
                    xob,yob,wtob)
!=============================================================================
! Use nrand randomized Hilbert curves over a sufficiently large square tile
! such that, regardless of its orientation when rotated about an axis 
! through a point one third the distance along the tiles diagonal, and through
! the center of the rectangular domain, the entire domain is covered by the 
! tile. The "randomization" comprises rotations of the tile by equal angular
! intervals of 90/nrand degrees. The density of data is estimated by
! B-spline smoothing the index delta-functions of the projected data on
! each tile-inscribed Hilbert curve, averaged over all nrand hilbert curves,
! with the scale of smoothing on the hilbert curves calibrated to match
! the intended spatial averaging scale. In a final step, the implied 
! weighting factor is computed such that, when the estimated data (obs per
! smoothing area, scale**2) is less than dentrip, this weighting factor
! wtob=1, but when the density is greater than dentrip, wtob is proportionately
! reduced.
!
! nob:     the number of data
! nrand:   the number of distinct pseudo-random Hilbert curves
! nor:     the order of the B-spline smoother (either 0 or 1)
! dentrip: the critical density that trips the weight factor reduction 
! scale:   the characteristic horizontal scale of averaging
! xmin,xmax: domain limits in x
! ymin,ymax: domain limits in y
! xob,yob  : arrays of observation locations in x and y
! wtob     : array of implied weight factors 
!=============================================================================
use pietc, only: u1,o3,pih
use kinds, only: dp,i_kind
use phil0,  only: xy_to_hil4,hil4_to_r
use psort, only: sort, invertperm
implicit none
integer(i_kind),        intent(in ):: nob,nrand,nor
real(dp),               intent(in ):: dentrip,scale,xmin,xmax,ymin,ymax
real(dp),dimension(nob),intent(in ):: xob,yob
real(dp),dimension(nob),intent(out):: wtob
!-----------------------------------------------------------------------------
integer(i_kind),parameter       :: ngen=14
real(dp),       parameter       :: eps=1.0e-11_dp
real(dp),       dimension(2,nob):: hob
real(dp),       dimension(nob)  :: sob
real(dp),       dimension(2,2)  :: rot
real(dp),       dimension(2)    :: vo3
real(dp)                        :: xcen,ycen,rad,rad3,span,ang,cang,sang
integer(i_kind),dimension(ngen) :: hilr
integer(i_kind),dimension(nob)  :: rank
integer(i_kind)                  :: irand,L
!==============================================================================
vo3=(/o3,o3/)! Tile-relative axis of rotation is at (1/3, 1/3)
xcen=(xmin+xmax)/2.0_dp; ycen=(ymin+ymax)/2.0_dp
rad=eps+sqrt((xmax-xmin)**2+(ymax-ymin)**2)/2.0_dp
rad3=rad*3.0_dp ! Size of the edge of the Hilbert curve square tile
ang=pih/nrand; cang=cos(ang); sang=sin(ang)
rot(:,1)=(/ cang,sang/)
rot(:,2)=(/-sang,cang/)
span=(scale/rad3)**2 
do L=1,nob
   hob(1,L)=o3+(xob(L)-xcen)/rad3
   hob(2,L)=o3+(yob(L)-ycen)/rad3
enddo

! Project the data onto nrand differently-oriented Hilbert curves and sum
! the different estimated density estimates at the ob points in array wtob:
wtob=0
do irand=1,nrand
   do L=1,nob
      sob(L)=0_dp
      call xy_to_hil4(hob(1,L),hob(2,L),hilr)
      call hil4_to_r(1,ngen,hilr,sob(L))
   enddo
! Sort the data implicitly by assigning each a "rank":
   call sort(sob,rank); call invertperm(rank)
   select case(nor)
      case(0)
         call bsmoo0(nob,span,sob,rank,wtob)
      case(1)
         call bsmoo1(nob,span,sob,rank,wtob)
      case default
         stop 'In denest; this value of B-spline order, nor, is not supported'
   end select
! Rotate the Hilbert tile by a pi/(2*nrand) about its axis at (1/3,1/3):
   if(L<nob)then
      do L=1,nob
         hob(:,L)=vo3+matmul(rot,hob(:,L)-vo3)
      enddo
   endif
enddo! irand
wtob=wtob*span/nrand
do L=1,nob
   wtob(L)=min(u1,dentrip/wtob(L))
enddo
end subroutine denest2d
!=============================================================================
subroutine denest2dx(nob,nrand,nor,dentrip,scale, &!                [denest2d]
     xmin,xmax,ymin,ymax, &
     xob,yob,wtob,denob)
!=============================================================================
! Like denest, but also returns the diagnosed density at all ob points
! denob    : array of estimated data densities (units per scale**2)
!=============================================================================
use pietc, only: u1,o3,pih
use kinds, only: dp,i_kind
use phil0, only: xy_to_hil4,hil4_to_r
use psort, only: sort, invertperm
implicit none
integer(i_kind),                intent(in ):: nob,nrand,nor
real(dp),               intent(in ):: dentrip,scale,xmin,xmax,ymin,ymax
real(dp),dimension(nob),intent(in ):: xob,yob
real(dp),dimension(nob),intent(out):: wtob,denob
!-----------------------------------------------------------------------------
integer(i_kind), parameter       :: ngen=14
real(dp),parameter       :: eps=1.e-11_dp
real(dp),dimension(2,nob):: hob
real(dp),dimension(nob)  :: sob
real(dp),dimension(2,2)  :: rot
real(dp),dimension(2)    :: vo3
real(dp)                 :: xcen,ycen,rad,rad3,span,ang,cang,sang
integer(i_kind),dimension(ngen)  :: hilr
integer(i_kind),dimension(nob)   :: rank
integer(i_kind)                  :: irand,L
!==============================================================================
vo3=(/o3,o3/)! Tile-relative axis of rotation is at (1/3, 1/3)
xcen=(xmin+xmax)/2.0_dp; ycen=(ymin+ymax)/2.0_dp
rad=eps+sqrt((xmax-xmin)**2+(ymax-ymin)**2)/2.0_dp
rad3=rad*3.0_dp ! Size of the edge of the Hilbert curve square tile
ang=pih/nrand; cang=cos(ang); sang=sin(ang)
rot(:,1)=(/ cang,sang/)
rot(:,2)=(/-sang,cang/)
span=(scale/rad3)**2 
do L=1,nob
   hob(1,L)=o3+(xob(L)-xcen)/rad3
   hob(2,L)=o3+(yob(L)-ycen)/rad3
enddo

! Project the data onto nrand differently-oriented Hilbert curves and sum
! the different estimated density estimates at the ob points in array denob:
denob=0_dp
do irand=1,nrand
   do L=1,nob
      sob(L)=0
      call xy_to_hil4(hob(1,L),hob(2,L),hilr)
      call hil4_to_r(1,ngen,hilr,sob(L))
   enddo
! Sort the data implicitly by assigning each a "rank":
   call sort(sob,rank); call invertperm(rank)
   select case(nor)
      case(0)
         call bsmoo0(nob,span,sob,rank,denob)
      case(1)
         call bsmoo1(nob,span,sob,rank,denob)
      case default
         stop 'In denest; this value of B-spline order, nor, is not supported'
   end select
! Rotate the Hilbert tile by a pi/(2*nrand) about its axis at (1/3,1/3):
   if(L<nob)then
      do L=1,nob
         hob(:,L)=vo3+matmul(rot,hob(:,L)-vo3)
      enddo
   endif
enddo! irand
denob=denob*span/nrand
do L=1,nob
   wtob(L)=min(u1,dentrip/denob(L))
enddo
end subroutine denest2dx

!=============================================================================
subroutine getqset5(n,qset5)!                                       [getqset5]
!=============================================================================
! Fill the rows of the n-row array, qset5, with the quaternions that
! represent, for that n, the optimally diverse rotations of a cube in the
! sense that the minimum angular distance between members of the set is as
! large as possible. In each case, the first row contains the identity and
! represents the cube in its standard Cartesian orientation.
!=============================================================================
use kinds, only: dp,i_kind
use pietc, only: u0,u1,u3,o2,r2,or2,phi
implicit none
integer(i_kind),          intent(in ):: n
real(dp),dimension(0:3,n),intent(out):: qset5
!-----------------------------------------------------------------------------
real(dp),parameter:: u8=8.0_dp,or8=u1/sqrt(u8),sig=u1/phi,chi=r2-u1
real(dp)          :: term1,term2,ce,cf,cg,ch,cj,ck,cl
!=============================================================================
qset5(:,1)=(/u1,u0,u0,u0/)
select case(n); case default; stop 'In getqset5; n is outside the valid range'
   case(1)
   case(2:3)
      term1=o2+or8
      term2=-o2+or8
      qset5        (:,2)=(/term1,-or8,term2, or8/)
      if(n==3)qset5(:,3)=(/term1, or8,term2,-or8/)
   case(4)
      term1=(4.0_dp+6.0_dp*r2)/14.0_dp
      term2=(4.0_dp-  r2)/14.0_dp
      qset5(:,2)=(/term1,2_dp*term2,    -term2,     term2/)
      qset5(:,3)=(/term1,     term2,2_dp*term2,    -term2/)
      qset5(:,4)=(/term1,    -term2,     term2,2_dp*term2/)
   case(5)
      ce=u1/(2.0_dp*sig+chi-sqrt(4.0_dp*sig+2_dp*chi-u3))
      cf=chi*ce
      cg=-u1+(-u1+r2*phi)*ce
      ch=-phi+(u1+r2*sig)*ce
      cj=sig/r2+(-u1+r2*sig)*ce
      ck=or2+(-u1+r2*chi*sig)*ce
      cl=phi*or2-ce
      qset5(:,2)=(/ce, cf,-cg, ch/)
      qset5(:,3)=(/ce,-cj, ck, cl/)
      qset5(:,4)=(/ce, ck, cj,-cl/)
      qset5(:,5)=(/ce, cg,-ch,-cf/)
end select
end subroutine getqset5

!==============================================================================
subroutine getqset7(qset7)!                                          [get1set7]
!==============================================================================
! Return a set of 7 quaternions that represent a configuration of cubic
! rotations that are as far (in terms of minimum relative rotation angle)
! from each other as possible. The configuration is formed by rotations of
! the initial standard cube by integer(i_kind) multiples of the angle pi*2/7 about
! an oblique axis, though the angle separating non-adjacent memmbers of this
! cycle is slightly smaller than pi*2/7.
!=============================================================================
use kinds, only: dp,i_kind
use pietc, only: u0,u1,r2,pi
use peuc,  only: mulqq
implicit none
real(dp),dimension(0:3,7),intent(out):: qset7
!-----------------------------------------------------------------------------
real(dp),parameter:: pio7=pi/7
real(dp)          :: k1,k2,k3,a,b,w,x,y,z
!=============================================================================
k1=2.0_dp*cos(  pio7)
k2=2.0_dp*cos(2.0_dp*pio7)
k3=2.0_dp*cos(3.0_dp*pio7)
a=4.0_dp*(5.0_dp-2.0_dp*k3-4.0_dp*k3**2-r2*(k3-1.0_dp))
a=4.0_dp*(6.0_dp*k1-5.0_dp-2.0_dp*k2+r2*(k1-k2-2.0_dp))
b=(u1/k3+r2*k3)
w=(b+sqrt(b**2-a))/a
x=r2*w-k2/2.0_dp
y=(k1-2.0_dp)*r2*w+k2/2.0_dp
z=(2.0_dp+(1.0_dp-k1)*r2)*w-k2/2.0_dp
qset7(:,1)=(/u1,   u0,          u0,             u0             /)
qset7(:,2)=(/k1/2_dp,-z/k1,       -x/k1,           y/k1           /)
qset7(:,3)=(/w,   -(y+z)/r2,    (k2/2_dp-x)/r2,    (y-z)/r2       /)
qset7(:,4)=(/w,    k2*(x-y)/r2,-(k2*z-k3/2_dp)/r2,-(k2*z+k3/2_dp)/r2 /)
qset7(:,5)=(/w,    k2*(x-y)/r2,-(k2*z+k3/2_dp)/r2,-(k2*z-k3/2_dp)/r2 /)
qset7(:,6)=(/w,    (z-y)/r2,   -(k2/2_dp-x)/r2,   -(y+z)/r2       /)
qset7(:,7)=(/k1/2_dp, z/k1,         x/k1,         -y/k1           /)
end subroutine getqset7

!==============================================================================
subroutine getqset8(qset8)!                                          [getqset8]
!==============================================================================
! Return a set of 8 quaternions that represent a configuration of cubically-
! symmetric rotations, from an orientation halfway between qset8(:,1) and 
! qset8(:,2), by an equal angle through axes through the long diagonal of
! that intermediate initial cube. The angle is optimized to make the angular
! distance between the eight resulting cubes as large as possible.
! The initial cube's oriention is chosen to make one of the eight rotations
! of be the standard orientation, coded by qset8(:,1) = the identity.
!==============================================================================
use kinds, only: dp,i_kind
use pietc, only: u1,or2
implicit none
real(dp),dimension(0:3,8),intent(out):: qset8
!------------------------------------------------------------------------------
real(dp),parameter:: o14=u1/14_dp,or98=or2/7_dp
!==============================================================================
qset8(:,1)=(/1_dp,0_dp,0_dp,0_dp/)
qset8(:,2)=(/13_dp,3_dp,3_dp,3_dp/)  *o14
qset8(:,3)=(/13_dp,-5_dp,1_dp,-1_dp/)*o14
qset8(:,4)=(/13_dp,-1_dp,-5_dp,1_dp/)*o14
qset8(:,5)=(/13_dp,1_dp,-1_dp,-5_dp/)*o14
qset8(:,6)=(/9_dp,3_dp,-2_dp,1_dp/)*or98
qset8(:,7)=(/9_dp,2_dp,3_dp,-2_dp/)*or98
qset8(:,8)=(/9_dp,-2_dp,2_dp,3_dp/)*or98
end subroutine getqset8

!=============================================================================
subroutine getqset13(qset13)!                                      [getqset13]
!=============================================================================
! Return a set of 13 quaternions that represent a configuration of
! the identity, together with 12 cubically-symmetric rotations of the
! cube in the identity orientation by an angle about axes through opposite
! edge-midpoints of that original cube. The angle is chosen to maximize the
! minimum angular distance between the resulting configurations.
!=============================================================================
use kinds, only: dp,i_kind
use pietc, only: u0,u1,r2
implicit none
real(dp),dimension(0:3,13),intent(out):: qset13
!-----------------------------------------------------------------------------
real(dp),parameter:: term1=29.0_dp*r2-u1,term2=(11.0_dp+9.0_dp*r2+2.0_dp*sqrt(term1))/41.0_dp,&
                     term3=(30.0_dp-9.0_dp*r2-2.0_dp*sqrt(term1))/82.0_dp,&
                     compa=sqrt(term2),compb=sqrt(term3)
!=============================================================================
qset13(:, 1)=(/u1,u0,u0,u0/)

qset13(:, 2)=(/compa,u0, compb, compb/)
qset13(:, 3)=(/compa,u0, compb,-compb/)
qset13(:, 4)=(/compa,u0,-compb, compb/)
qset13(:, 5)=(/compa,u0,-compb,-compb/)

qset13(:, 6)=(/compa, compb,u0, compb/)
qset13(:, 7)=(/compa,-compb,u0, compb/)
qset13(:, 8)=(/compa, compb,u0,-compb/)
qset13(:, 9)=(/compa,-compb,u0,-compb/)

qset13(:,10)=(/compa, compb, compb,u0/)
qset13(:,11)=(/compa, compb,-compb,u0/)
qset13(:,12)=(/compa,-compb, compb,u0/)
qset13(:,13)=(/compa,-compb,-compb,u0/)
end subroutine getqset13

end module phil2

