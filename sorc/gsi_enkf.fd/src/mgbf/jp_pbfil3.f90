module jp_pbfil3
!$$$  module documentation block
!                .      .    .                                       .
! module:   jp_pbfil3
!   prgmmr: purser           org: NOAA/EMC            date: 2021-08
!
! abstract:  Codes for the beta line filters
!
! module history log:
!
! Subroutines Included:
!   t22_to_3 -
!   t2_to_3 -
!   t3_to_22 -
!   t33_to_6 -
!   t3_to_6 -
!   t6_to_33 -
!   t44_to_10 -
!   t4_to_10 -
!   t10_to_44 -
!   finmomtab -
!   inimomtab -
!   tritform -
!   tritformi -
!   triad -
!   gettrilu -
!   querytcol -
!   hextform -
!   hextformi -
!   hexad -
!   gethexlu -
!   queryhcol -
!   dectform -
!   dectformi -
!   decad -
!   getdeclu -
!   querydcol -
!   standardizeb -
!   hstform -
!   hstformi -
!   blinfil -
!   dibeta -
!   dibetat -
!
! Functions Included:
!
! remarks:
!   The routines of this module mostly involve the beta line filters.
!   Versions of these routines are provided in 2D, 3D and 4D, based respectively
!   on the Triad (3-lines), Hexad (6-lines), and Decad (10-lines) algorithms.
!   Some technical explanations are provided in the series of office notes,
!   ON498, ON499, ON500.
!
!   The style of line filtering is the "Dibeta" combination of two 
!   nonnegatively-weighted consecutive-imteger-half-span beta filters, whose
!   normalization coefficients are stored in the table, "bnorm" and whose
!   second moments (spread**2) are stored in the table "bsprds"; these 
!   moment tables must be initialized in subr. inimomtab before any filtering
!   can be done. The max-halp-span size of the table is set by the user, so 
!   the tables use allocatable space (in module jp_pbfil2); to deallocate this
!   storage, the user must invoke fintabmom once all filtering operations
!   have been completed.
!
!   Aspect tensors in N dimensions are positive-definite and symmetric, and 
!   therefore require M=(N*(N+1))/2 independent components, which we can arrange
!   into a vector of this size. The utility routines tNN_to_M do this; tM_to_NN
!   do the opposite. tN_to_M put the outer-product of an N-vector into the
!   corresponding M-vector.
!
!   The filtering is preceded by a decomposition of the M components of the
!   aspect tensor, at each grid point, into M distinct line-second-moments
!   and the line-generators they each act along, at every grid point. And
!   since, in the general case, the aspect tensor is no longer needed once
!   the line filter specifications have been determined, it ic convenient to
!   over-write the old aspect tensor components with the new line-second-
!   moments ("spread**2"). In other word, we can express the needed action
!   as a formal "transform" (and invert it if ever needed, to recover the 
!   original aspect tensor). The basic decomposition of the aspect tensor
!   into its spread**2 components and line generators is done, at a single
!   grid point using subroutine triad (2D), hexad (3D), decad (4D). Working
!   this into "transform" for a single point, is done in tritform, hextform,
!   dectform, and their respective inverse transforms in tritformi, hextfotmi,
!   dectformi. In the case of the 3D hexad method, although there are 6 active
!   line filters at any given point, each of those lines is associated with
!   one of the 7 different "colors" (our term for the nonnull Galois field 
!   elements) no two of these colors in a given hexad are the same. The
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use mpi
use jp_pkind, only: spi,sp,dp; use jp_pkind2, only: fpi
use jp_pietc, only: T,F,u0,u1,u3,u4,u5,pi2
implicit none
private
public:: t22_to_3,t2_to_3,t3_to_22,t33_to_6,t3_to_6,t6_to_33,&
         t44_to_10,t4_to_10,t10_to_44,                       &
         finmomtab,inimomtab,                                &
         tritform,tritformi,triad,gettrilu,querytcol,        &
         hextform,hextformi,hexad,gethexlu,queryhcol,        &
         dectform,dectformi,decad,getdeclu,querydcol,        &
         hstform,hstformi,blinfil,dibeta,dibetat
integer(spi),dimension(2,0:2):: i2pair
integer(spi),dimension(2,6)  :: i3pair
integer(spi),dimension(2,10) :: i4pair
data i2pair/1,1, 2,2, 1,2/
data i3pair/1,1, 2,2, 3,3, 2,3, 3,1, 1,2/
data i4pair/1,1, 2,2, 3,3, 4,4, 1,2, 1,3, 1,4, 3,4, 2,4, 2,3/

interface t22_to_3;    module procedure i22_to_3, r22_to_3;   end interface
interface t2_to_3;     module procedure i2_to_3,  r2_to_3;    end interface
interface t3_to_22;    module procedure i3_to_22, r3_to_22;   end interface
interface t33_to_6;    module procedure i33_to_6, r33_to_6;   end interface
interface t3_to_6;     module procedure i3_to_6,  r3_to_6;    end interface
interface t6_to_33;    module procedure i6_to_33, r6_to_33;   end interface
interface t44_to_10;   module procedure i44_to_10,r44_to_10;  end interface
interface t4_to_10;    module procedure i4_to_10, r4_to_10;   end interface
interface t10_to_44;   module procedure i10_to_44,r10_to_44;  end interface
!---
interface finmomtab;   module procedure finmomtab;            end interface
interface inimomtab;   module procedure inimomtab;            end interface
interface tritform;    module procedure tritforms,tritform;   end interface
interface tritformi;   module procedure tritformi;            end interface
interface triad;       module procedure triad;                end interface
interface gettrilu;    module procedure gettrilu;             end interface
interface querytcol;   module procedure querytcol;            end interface
interface hextform;    module procedure hextforms,hextform;   end interface
interface hextformi;   module procedure hextformi;            end interface
interface hexad;       module procedure hexad;                end interface
interface gethexlu;    module procedure gethexlu;             end interface
interface queryhcol;   module procedure queryhcol;            end interface
interface dectform;    module procedure dectforms,dectform;   end interface
interface dectformi;   module procedure dectformi;            end interface
interface decad;       module procedure decad;                end interface
interface getdeclu;    module procedure getdeclu;             end interface
interface querydcol;   module procedure querydcol;            end interface
!---
interface standardizeb;module procedure standardizeb;         end interface
interface hstform;     module procedure hstform;              end interface
interface hstformi;    module procedure hstformi;             end interface
interface blinfil;     module procedure blinfil;              end interface
interface dibeta
   module procedure dibeta1,dibeta2,dibeta3,dibeta4, dibetax3,dibetax4, &
        vdibeta1,vdibeta2,vdibeta3,vdibeta4, vdibetax3,vdibetax4
end interface
interface dibetat
   module procedure dibeta1t,dibeta2t,dibeta3t,dibeta4t,dibetax3t, dibetax4t, &
                vdibeta1t,vdibeta2t,vdibeta3t,vdibeta4t,vdibetax3t,vdibetax4t
end interface

contains

!==============================================================================
subroutine i22_to_3(i22,i3)!                                         [t22_to_3]
!==============================================================================
use jp_pkind, only: spi
implicit none
integer(spi),dimension(2,2),intent(in ):: i22
integer(spi),dimension(0:2),intent(out):: i3
!------------------------------------------------------------------------------
integer(spi):: L
!==============================================================================
do L=0,2; i3(L)=i22(i2pair(1,L),i2pair(2,L)); enddo
end subroutine i22_to_3
!==============================================================================
subroutine r22_to_3(r22,r3)!                                         [t22_to_3]
!==============================================================================
use jp_pkind, only: spi,dp
implicit none
real(dp),dimension(2,2),intent(in ):: r22
real(dp),dimension(0:2),intent(out):: r3
!------------------------------------------------------------------------------
integer(spi):: L
!==============================================================================
do L=0,2; r3(L)=r22(i2pair(1,L),i2pair(2,L)); enddo
end subroutine r22_to_3

!==============================================================================
subroutine i2_to_3(i2,i3)!                                            [t2_to_3]
!==============================================================================
use jp_pkind, only: spi
use jp_pmat4, only: outer_product
implicit none
integer(spi),dimension(2),intent(in ):: i2
integer(spi),dimension(3),intent(out):: i3
!------------------------------------------------------------------------------
call t22_to_3(outer_product(i2,i2),i3)
end subroutine i2_to_3
!==============================================================================
subroutine r2_to_3(r2,r3)!                                            [t2_to_3]
!==============================================================================
use jp_pkind, only: dp
use jp_pmat4, only: outer_product
implicit none
real(dp),dimension(2),intent(in ):: r2
real(dp),dimension(3),intent(out):: r3
!------------------------------------------------------------------------------
call t22_to_3(outer_product(r2,r2),r3)
end subroutine r2_to_3

!==============================================================================
subroutine i3_to_22(i3,i22)!                                         [t3_to_22]
!==============================================================================
use jp_pkind, only: spi
implicit none
integer(spi),dimension(0:2),intent(in ):: i3
integer(spi),dimension(2,2),intent(out):: i22
!------------------------------------------------------------------------------
integer(spi):: L
!==============================================================================
do L=0,2
   i22(i2pair(1,L),i2pair(2,L))=i3(L)
   i22(i2pair(2,L),i2pair(1,L))=i3(L)
enddo
end subroutine i3_to_22
!==============================================================================
subroutine r3_to_22(r3,r22)!                                         [t3_to_22]
!==============================================================================
use jp_pkind, only: spi,dp
implicit none
real(dp),dimension(0:2),intent(in ):: r3
real(dp),dimension(2,2),intent(out):: r22
!------------------------------------------------------------------------------
integer(spi):: L
!==============================================================================
do L=0,2
   r22(i2pair(1,L),i2pair(2,L))=r3(L)
   r22(i2pair(2,L),i2pair(1,L))=r3(L)
enddo
end subroutine r3_to_22

!==============================================================================
subroutine i33_to_6(i33,i6)!                                         [t33_to_6]
!==============================================================================
use jp_pkind, only: spi
implicit none
integer(spi),dimension(3,3),intent(in ):: i33
integer(spi),dimension(6)  ,intent(out):: i6
!------------------------------------------------------------------------------
integer(spi):: L
!==============================================================================
do L=1,6; i6(L)=i33(i3pair(1,L),i3pair(2,L)); enddo
end subroutine i33_to_6
!==============================================================================
subroutine r33_to_6(r33,r6)!                                         [t33_to_6]
!==============================================================================
use jp_pkind, only: spi,dp
implicit none
real(dp),dimension(3,3),intent(in ):: r33
real(dp),dimension(6)  ,intent(out):: r6
!------------------------------------------------------------------------------
integer(spi):: L
!==============================================================================
do L=1,6; r6(L)=r33(i3pair(1,L),i3pair(2,L)); enddo
end subroutine r33_to_6

!==============================================================================
subroutine i3_to_6(i3,i6)!                                            [t3_to_6]
!==============================================================================
use jp_pkind, only: spi
use jp_pmat4, only: outer_product
implicit none
integer(spi),dimension(3),intent(in ):: i3
integer(spi),dimension(6),intent(out):: i6
!------------------------------------------------------------------------------
call t33_to_6(outer_product(i3,i3),i6)
end subroutine i3_to_6
!==============================================================================
subroutine r3_to_6(r3,r6)!                                            [t3_to_6]
!==============================================================================
use jp_pkind, only: dp
use jp_pmat4, only: outer_product
implicit none
real(dp),dimension(3),intent(in ):: r3
real(dp),dimension(6),intent(out):: r6
!------------------------------------------------------------------------------
call t33_to_6(outer_product(r3,r3),r6)
end subroutine r3_to_6

!==============================================================================
subroutine i6_to_33(i6,i33)!                                         [t6_to_33]
!==============================================================================
use jp_pkind, only: spi
implicit none
integer(spi),dimension(6),  intent(in ):: i6
integer(spi),dimension(3,3),intent(out):: i33
!------------------------------------------------------------------------------
integer(spi):: L
!==============================================================================
do L=1,6
   i33(i3pair(1,L),i3pair(2,L))=i6(L)
   i33(i3pair(2,L),i3pair(1,L))=i6(L)
enddo
end subroutine i6_to_33
!==============================================================================
subroutine r6_to_33(r6,r33)!                                         [t6_to_33]
!==============================================================================
use jp_pkind, only: spi,dp
implicit none
real(dp),dimension(6),  intent(in ):: r6
real(dp),dimension(3,3),intent(out):: r33
!------------------------------------------------------------------------------
integer(spi):: L
!==============================================================================
do L=1,6
   r33(i3pair(1,L),i3pair(2,L))=r6(L)
   r33(i3pair(2,L),i3pair(1,L))=r6(L)
enddo
end subroutine r6_to_33

!==============================================================================
subroutine i44_to_10(i44,i10)!                                      [t44_to_10]
!==============================================================================
use jp_pkind, only: spi
implicit none
integer(spi),dimension(4,4),intent(in ):: i44
integer(spi),dimension(10) ,intent(out):: i10
!------------------------------------------------------------------------------
integer(spi):: L
!==============================================================================
do L=1,10; i10(L)=i44(i4pair(1,L),i4pair(2,L)); enddo
end subroutine i44_to_10
!==============================================================================
subroutine r44_to_10(r44,r10)!                                      [t44_to_10]
!==============================================================================
use jp_pkind, only: spi,dp
implicit none
real(dp),dimension(4,4),intent(in ):: r44
real(dp),dimension(10) ,intent(out):: r10
!------------------------------------------------------------------------------
integer(spi):: L
!==============================================================================
do L=1,10; r10(L)=r44(i4pair(1,L),i4pair(2,L)); enddo
end subroutine r44_to_10

!==============================================================================
subroutine i4_to_10(i4,i10)!                                         [t4_to_10]
!==============================================================================
use jp_pkind, only: spi
use jp_pmat4, only: outer_product
implicit none
integer(spi),dimension(4), intent(in ):: i4
integer(spi),dimension(10),intent(out):: i10
!------------------------------------------------------------------------------
call t44_to_10(outer_product(i4,i4),i10)
end subroutine i4_to_10
!==============================================================================
subroutine r4_to_10(r4,r10)!                                         [t4_to_10]
!==============================================================================
use jp_pkind, only: dp
use jp_pmat4, only: outer_product
implicit none
real(dp),dimension(4), intent(in ):: r4
real(dp),dimension(10),intent(out):: r10
!------------------------------------------------------------------------------
call t44_to_10(outer_product(r4,r4),r10)
end subroutine r4_to_10

!==============================================================================
subroutine i10_to_44(i10,i44)!                                      [t10_to_44]
!==============================================================================
use jp_pkind, only: spi
implicit none
integer(spi),dimension(10), intent(in ):: i10
integer(spi),dimension(4,4),intent(out):: i44
!------------------------------------------------------------------------------
integer(spi):: L
!==============================================================================
do L=1,10
   i44(i4pair(1,L),i4pair(2,L))=i10(L)
   i44(i4pair(2,L),i4pair(1,L))=i10(L)
enddo
end subroutine i10_to_44
!==============================================================================
subroutine r10_to_44(r10,r44)!                                      [t10_to_44]
!==============================================================================
use jp_pkind, only: spi,dp
implicit none
real(dp),dimension(10), intent(in ):: r10
real(dp),dimension(4,4),intent(out):: r44
!------------------------------------------------------------------------------
integer(spi):: L
!==============================================================================
do L=1,10
   r44(i4pair(1,L),i4pair(2,L))=r10(L)
   r44(i4pair(2,L),i4pair(1,L))=r10(L)
enddo
end subroutine r10_to_44

!--

!================================================================== [finmomtab]
subroutine finmomtab
!==============================================================================
! Finalize the moments table for dibeta filter applications.
! Deallocate the space reserved for moment tables and reset p and nh to their
! zero defaults.
!==============================================================================
use jp_pbfil2, only: p,nh,bnorm,bsprds
implicit none
p=0; nh=0
if(allocated(bnorm))deallocate(bnorm)
if(allocated(bsprds))deallocate(bsprds)
end subroutine finmomtab

!================================================================== [inimomtab]
subroutine inimomtab(p_prescribe,nh_prescribe,ff)
!==============================================================================
! Initialize the moments table for dibeta filter applications.
! For the given beta function exponent index, p, and nh half-spans, initialize
! table of the normalizing coefficients, bnorm, and spread**2s, bsprds.
! The calculation involves computing the continuum approximations, m0 and m2,
! to the 0th and 2nd moments, and using the Euler-Maclaurin expansions
! for the correction terms hm0 and hm2 so that the final corrected moments
! cm0 and cm2 for each integer halfwidth up to nh .
!==============================================================================
use jp_pkind,  only: spi,dp
use jp_pietc,  only: u0,u1,u2
use jp_pbfil2, only: p,nh,bnorm,bsprds
implicit none
integer(spi),intent(in ):: p_prescribe,nh_prescribe
logical,     intent(out):: ff
!------------------------------------------------------------------------------
integer(spi),parameter          :: nk0=2,nk2=nk0+1,np=6,np2p3=np*2+3
real(dp),dimension(-1:np2p3)    :: ffac
real(dp)                        :: x,xx,m0,m2,hm0,hm2,cm0,cm2
integer(spi),dimension(0:nk0,np):: n0pk
integer(spi),dimension(0:nk2,np):: n2pk
integer(spi)                    :: h,i,k,mk0,mk2,p2,p2m1,p2p1,p2p3
data n0pk/                          &
            -1,        0,        0, &
            -1,        0,        0, &
            -5,       14,        0, &
           -63,      240,        0, &
         -1575,     6930,    -2640, &
        -68409,   327600,  -216216/
data n2pk/                                    &
             1,       -5,        0,        0, &
             5,      -21,        0,        0, &
            63,     -285,      126,        0, &
          1575,    -7623,     5280,        0, &
         68409,  -348075,   306306,   -34320, &
       4729725,-24969285, 25552800, -5405400/
!==============================================================================
call finmomtab ! Table arrays bnorm and bsprds must start off deallocated
ff=(p_prescribe<1 .or. p_prescribe>np)
if(ff)then
   print'(" In inimomtab; prescribed exponent p out of bounds")'
   return
endif
ff=(nh_prescribe<2 .or. nh_prescribe>1000)
if(ff)then
   print'(" In inimomtab; prescribed table size nh out of bounds")'
   return
endif
p =p_prescribe
nh=nh_prescribe
allocate(bnorm(nh),bsprds(nh))
! set up the ffac tables (double-factorial function)
p2=p*2; p2m1=p2-1; p2p1=p2+1; p2p3=p2+3
ffac(-1)=u1
ffac(0)=u1
do i=1,np2p3
   ffac(i)=i*ffac(i-2)
enddo
mk0=(p-1)/2
mk2=mk0+1
do h=1,nh
   x=h
   xx=x*x
   m0=u2*ffac(p2)*x/ffac(p2p1)
   m2=u2*ffac(p2)*x**3/ffac(p2p3)
   hm0=u0
   do k=0,mk0
      hm0=hm0+n0pk(k,p)*xx**k
   enddo
   hm2=u0
   do k=0,mk2
      hm2=hm2+n2pk(k,p)*xx**k
   enddo
   cm0=m0+hm0/(ffac(p2p1)*x**p2m1)
   cm2=m2+hm2/(ffac(p2p3)*x**p2m1)
   bnorm(h)=u1/cm0
   bsprds(h)=cm2/cm0
enddo
end subroutine inimomtab

!================================================================== [tritform]
subroutine tritforms(lx,mx, ly,my, aspects, dixs,diys, ff)
!=============================================================================
! Perform direct Triad and hs transforms in a proper subdomain
! domains extents in x, y, are lx:mx, ly:my
! aspects: upon input, these are the 3-vectors of grid-relative aspect tensor
!          upon output, these are the 3 active line-filter half-spans.
! dixs:    x-component of each of the 6 active line generators
! diys:    y-component
! ff:      Logical failure flag, output .true. when failure occurs.
! Note that the integer arrays, doxs, diys, are 1-byte integers.
!==============================================================================

use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
implicit none
integer(spi),                         intent(in   ):: lx,mx,ly,my
real(dp),    dimension(3,lx:mx,ly:my),intent(inout):: aspects
integer(fpi),dimension(lx:mx,ly:my,3),intent(  out):: dixs,diys
logical,                              intent(  out):: ff
!-----------------------------------------------------------------------------
integer(spi)               :: ix,iy
integer(fpi),dimension(2,3):: ltri
!=============================================================================
do iy=ly,my
   do ix=lx,mx
      call tritform(aspects(:,ix,iy),ltri,ff)
      if(ff)then
         print'(" Failure in tritform at ix,iy=",2i5)',ix,iy
         return
      endif
      dixs(ix,iy,:)=ltri(1,:)
      diys(ix,iy,:)=ltri(2,:)
   enddo
enddo
end subroutine tritforms

!=================================================================== [tritform]
subroutine tritform(aspect ,ltri, ff)
!==============================================================================
! Perform the direct Triad and hs transform.
! Take a 3-vector representation of the aspect tensor and
! transform it to the vector of half-spans for the beta line filter
! and 1-byte-integer line generators.
! aspect: input as aspect tensor components, output as spread**2
! ltri   : three active line generators in ascending color order
! ff     : logical failure flag.
!==============================================================================
use jp_pkind,  only: spi,dp; use jp_pkind2, only: fpi
implicit none
real(dp),dimension(3),      intent(inout):: aspect
integer(fpi),dimension(2,3),intent(  out):: ltri
logical,                    intent(  out):: ff
!------------------------------------------------------------------------------
real(dp),    dimension(  3):: wtri
integer(fpi),dimension(2,3):: ltri3
integer(spi)               :: i
!==============================================================================
call triad(aspect, ltri3,wtri,ff)
if(ff)then
   print'(" In tritform; triad failed; check aspect tensor")'
   return
endif
ltri=ltri3
aspect=wtri
do i=1,3
   call hstform(aspect(i),ff)
   if(ff)then
      print'(" In tritform; hstform failed at i=",i2)',i
      print'(" Check that inimomtab has been called to initialize exponent")'
      print'(" p, table size, nh, and the moment tables for line filters")'
      return
   endif
enddo
end subroutine tritform

!================================================================== [tritformi]
subroutine tritformi(aspect ,ltri, ff)
!==============================================================================
! Perform the inverse hs and triad transform.
! Take a 3-vector of the active spreads**2,
! and their line generators, and return the implied
! aspect tensor in the same 3-vector that contained the half-spans
! aspect: input as half-spans; output as aspect tensor components
! ltri  : corresponding successive line generators (using 1-byte integers)
! ff    : logical failure flag.
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
use jp_pmat4, only: outer_product
implicit none
real(dp),dimension(3),intent(inout)      :: aspect
integer(fpi),dimension(2,3),intent(in   ):: ltri
logical,                    intent(  out):: ff
!------------------------------------------------------------------------------
real(dp),dimension(2,2):: a22
real(dp),dimension(2)  :: vec
integer(spi)           :: i
!==============================================================================
a22=u0
do i=1,3
   vec=ltri(:,i)
   call hstformi(aspect(i),ff)
   if(ff)then
      print'(" In tritformi; hstformi failed at i=",i2)',i
      print'(" Check that inimomtab has been called to initialize exponent")'
      print'(" p, table size, nh, and the moment tables for line filters")'
      return
   endif
   a22=a22+outer_product(vec,vec)*aspect(i)
enddo
call t22_to_3(a22,aspect)
end subroutine tritformi

!===================================================================== [triad]
subroutine triad(aspect,ltri,wtri,ff)
!=============================================================================
! A version of the Triad iterative algorithm for resolving a given aspect
! tensor, A, rearranged as the 3-vector,
! Aspect = (/A_11, A_22, A_12/)
! onto a bisis of generator directions, the integer 2-vectors ltri, together
! with their corresponding aspect projections, or "weights", wtri.
!
! Aspect: The given aspect tensor in the form of a 3-vector (see above)
! Ltri:  The three integer 2-vectors whose members define a triad
!        and whose outer-products imply basis 3-vectors into which the aspect
!        is resolved. This matrix of 3-vectors is denoted Lu, but only its
!        inverse, Lui, is needed in this routine.
! wtri:  Real nonnegative weights (projected aspect) corresponding to ltri.
! ff  :  Failure flag, raised on output only when iterations exceed limit.
!=============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pmat4, only: outer_product
implicit none
real(dp),    dimension(3),    intent(in ):: aspect
integer(fpi),dimension(2,0:2),intent(out):: ltri
real(dp),    dimension(0:2)  ,intent(out):: wtri
logical,                      intent(out):: ff
!-----------------------------------------------------------------------------
integer(spi),parameter       :: nit=200
real(dp),    parameter       :: bcmins=-1.e-14_dp
real(dp),    dimension(3,0:2):: rlui
real(dp)                     :: dwtri
integer(spi),dimension(-2:2) :: ssigns
integer(spi),dimension(0:2)  :: signs
integer(fpi),dimension(2,0:2):: defltri ! <- default Ltri
integer(spi),dimension(3,0:2):: deflui  ! <- default Lui
integer(spi),dimension(3,0:2):: lui
integer(spi),dimension(3)    :: dlui
integer(spi),dimension(1)    :: ii
integer(spi)                 :: it,kcol,lcol,mcol
data ssigns/1,1,-1,1,1/
data deflui/1, 0,-1,  0, 1,-1,  0, 0, 1/
data defltri/ 1, 0,  0,1, -1,-1/
!==============================================================================
ltri=defltri; lui=deflui
rlui=lui; wtri=matmul(aspect,rlui)
do it=1,nit
   ii=minloc(wtri)-1; kcol=ii(1); dwtri=wtri(kcol)*2; if(dwtri>=bcmins)exit
   lcol=mod(kcol+1,3); mcol=mod(lcol+1,3); dlui=lui(:,kcol)*2
   Ltri(:,lcol)=-Ltri(:,Lcol); Ltri(:,kcol)=-Ltri(:,Lcol)-Ltri(:,mcol)
   signs=ssigns(-kcol:2-kcol)
   lui=lui+outer_product(dlui,signs)
   wtri=wtri+signs*dwtri
enddo
ff=it>nit
end subroutine triad

!=================================================================== [gettrilu]
subroutine gettrilu(ltri,lu)
!==============================================================================
use jp_pkind, only: spi; use jp_pkind2, only: fpi
implicit none
integer(fpi),dimension(2,0:2),intent(in ):: ltri
integer(fpi),dimension(2,0:2),intent(out):: lu
!-----------------------------------------------------------------------------
integer(spi):: i,L
!==============================================================================
do i=0,2; do L=1,2; lu(L,i)=Ltri(i2pair(1,L),i)*Ltri(i2pair(2,L),i);enddo;enddo
end subroutine gettrilu

!==============================================================================
subroutine querytcol(vin,tcol)!                                     [querytcol]
!==============================================================================
use jp_pkind, only: spi
implicit none
integer(spi),dimension(2),intent(in ):: vin
integer(spi),             intent(out):: tcol
!------------------------------------------------------------------------------
integer(spi),dimension(3):: tcols
integer(spi)             :: i
data tcols/0,1,2/
!==============================================================================
i=modulo(vin(1),2)+2*modulo(vin(2),2)
if(i==0)stop 'In querytcol; invalid 2-vector vin has all components even'
tcol=tcols(i)
end subroutine querytcol

!=================================================================== [hextform]
subroutine hextforms(lx,mx,ly,my,lz,mz, aspects, qcols,dixs,diys,dizs, ff)
!==============================================================================
! Perform direct hexad and hs transforms in a proper subdomain
! domains extents in x, y, z, are lx:mx, ly:my, lz:mz
! aspects: upon input, these are the 6-vectors of grid-relative aspect tensor
!          upon output, these are the six active-line-filter half-spans.
! qcols:   outout as the Galois "colors" of each successive line-filter, listed
!          in ascending order but with zeros at positions 0 and 7 of each list.
! dixs:    x-component of each of the 6 active line generators
! diys:    y-component
! dizs:    z-component
! ff:      Logical failure flag, output .true. when failure occurs.
! Note that the integer arrays, qcols, doxs, diys, dizs, are 1-byte integers.
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
implicit none
integer(spi),                                 intent(in   ):: lx,mx, &
                                                              ly,my, &
                                                              lz,mz
real(dp),    dimension(  6,lx:mx,ly:my,lz:mz),intent(inout):: aspects
integer(fpi),dimension(0:7,lx:mx,ly:my,lz:mz),intent(  out):: qcols
integer(fpi),dimension(lx:mx,ly:my,lz:mz,6),  intent(  out):: dixs,diys,dizs
logical,                                      intent(  out):: ff
!------------------------------------------------------------------------------
integer(spi)               :: ix,iy,iz
integer(fpi),dimension(3,6):: lhex
!==============================================================================
do iz=lz,mz
   do iy=ly,my
      do ix=lx,mx
         call hextform(aspects(:,ix,iy,iz),qcols(:,ix,iy,iz),&
              lhex,ff)
         if(ff)then
            print'(" Failure in hextform at ix,iy,iz=",3i5)',ix,iy,iz
            return
         endif
         dixs(ix,iy,iz,:)=lhex(1,:)
         diys(ix,iy,iz,:)=lhex(2,:)
         dizs(ix,iy,iz,:)=lhex(3,:)
      enddo
   enddo
enddo
end subroutine hextforms

!=================================================================== [hextform]
subroutine hextform(aspect, qcol,lhex, ff)
!==============================================================================
! Perform the direct Hexad and hs transform.
! Take a 6-vector representation of the aspect tensor and
! transform it to the vector of half-spans for the dibeta filter,
! and 1-byte-integer line generators, and color list.
! aspect: input as aspect tensor components, output as half-spans
! qcol   : output as colors of successive active lines, but with
!          "spare" null elements 0 and 7.
! lhex   : six active line generators in ascending color order
! ff     : logical failure flag.
!==============================================================================
use jp_pkind,  only: spi,dp; use jp_pkind2, only: fpi
implicit none
real(dp),dimension(6),      intent(inout):: aspect
integer(fpi),dimension(0:7),intent(  out):: qcol
integer(fpi),dimension(3,6),intent(  out):: lhex
logical,                    intent(  out):: ff
!------------------------------------------------------------------------------
real(dp),    dimension(  7):: whex7
integer(fpi),dimension(3,7):: lhex7
integer(fpi)               :: i,j
!==============================================================================
call hexad(aspect, lhex7,whex7,ff)
if(ff)then
   print'(" In hextform; hexad, failed; check aspect tensor")'
   return
endif
qcol(0)=0; qcol(7)=0
j=1
do i=1,7
   if(sum(abs(lhex7(:,i)))==0)cycle
   qcol(j)=i
   lhex(:,j)=lhex7(:,i)
   aspect(j)=whex7(  i)
   j=j+1_fpi
enddo
do i=1,6
   call hstform(aspect(i),ff)
   if(ff)then
      print'(" In hextform; hstform failed at i=",i2)',i
      print'(" Check that inimomtab has been called to initialize exponent")'
      print'(" p, table size, nh, and the moment tables for line filters")'
      return
   endif
enddo
ff=(j/=7)
if(ff)print'(" In hextform; inconsistent hexad generator set found")'
end subroutine hextform

!================================================================== [hextformi]
subroutine hextformi(aspect, qcol,lhex, ff)
!==============================================================================
! Perform the inverse hs and hexad transform.
! Take a 6-vector of the active half-spans, their respective
! colors, and their line generators, and return the implied
! aspect tensor in the same 6-vector that contained the spreads**2
! aspect: input as spreads**2; output as aspect tensor components
! qcol  : colors of successive active hexad members (using 1-byte integers)
! lhex  : corresponding successive line generators (using 1-byte integers)
! ff    : logical failure flag.
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
use jp_pmat4, only: outer_product
implicit none
real(dp),    dimension(  6),intent(inout):: aspect
integer(fpi),dimension(0:7),intent(in   ):: qcol
integer(fpi),dimension(3,6),intent(in   ):: lhex
logical,                    intent(  out):: ff
!------------------------------------------------------------------------------
real(dp),dimension(3,3):: a33
real(dp),dimension(3)  :: vec
integer(fpi)           :: i,j
!==============================================================================
a33=u0
j=1
do i=1,7
   if(qcol(j)/=i)cycle
   call hstformi(aspect(j),ff)
   if(ff)then
      print'(" In hextformi; hstformi failed at i,j=",2i2)',i,j
      print'(" Check that inimomtab has been called to initialize exponent")'
      print'(" p, table size, nh, and the moment tables for line filters")'
      return
   endif
   vec=lhex(:,j)
   a33=a33+outer_product(vec,vec)*aspect(j)
   j=j+1_fpi
enddo
ff=(j/=7)
if(ff)print'(" In hextformi; Inconsistent qcol")'
call t33_to_6(a33,aspect)
end subroutine hextformi

!====================================================================== [hexad]
subroutine hexad(aspect,lhex7,whex7,ff)
!==============================================================================
! A version of the Hexad iterative algorithm for resolving a given aspect
! tensor, A, rearranged as the 6-vector,
! Aspect= (/ A_11, A_22, A_33,   A_23, A_31, A_12 /)
! onto a basis of generator directions, the integer 3-vectors lhex7, together
! with their corresponding aspect projections, or "weights", whex7.
! Although seven lhex vectors and weights are given (arranged by "colors" 0--6)
! only six of these -- those that do NOT equal the "color" of the hexad
! itself --- are nonzero (and are positive when the hexad is correctly
! resolving the target aspect tensor, Aspect). The style of this algorithm
! is as close as possible to the the description in documentation "Note 7".
!
! Aspect: the given aspect tensor in the form of a 6-vector (see above).
! Lhex7:  The seven integer 3-vectors whose 6 non-null members define a Hexad
!        and whose outer-products imply basis 6-vectors into which the aspect
!        is resolved. This matrix of 6-vectors is denoted Lu, but only its
!        inverse, Lui, is needed in this routine. These seven 3-vectors are
!        arranged in decreasing order of "cardinality", 
!        meaning that the cardinal
!        directions' colors define the first three vectors, the next three have
!        two odd components, and the seventh has all odd components.
! whex7: Seven real nonnegative weights (projected aspect) 
!        corresponding to lhex
!        (zero value in the case of the null vector of lhex7)
! ff   : failure flag, raised only when the iterations exceed their limit.
! The algorithm here benefits from using the symmetry of the Fano plane
! and related GF(8) nonnull elements which, arranged cyclically, imply that
! the Jth "line" comprises points j+line(0), j+line(1), j+line(2), where
! Line = (/ 1, 2, 4/) and j is taken modulo 7.
! Note: the "K-set" of 3 members of the Lhex (indexed hcol+6, hcol+5, hcol+3)
! or equivalently, hcol-line(0),hcol-line(1),hclo-line(2),
! where arithmetic is modulo-7, are sufficient to form a "basis" from which
! the other ("L-set") nonnull members of Lhex are implied. To make the
! iterations efficient, we can iterate just this K-set, because the changes
! made to the effective projection operator, Lui, are, by the Woodbury
! formula, of rank-1 at each iteration, and the whex components change by
! a corresponding pattern of increments that do not need us to find the full
! set of Lhex, nor the explicit Lu, each iteration.
! Note that some integer arrays use 1-byte integer type to save space.
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
use jp_pmat4, only: outer_product
implicit none
real(dp),    dimension(6),    intent(in ):: aspect
integer(fpi),dimension(3,7),  intent(out):: lhex7
real(dp),    dimension(7),    intent(out):: whex7
logical,                      intent(out):: ff
!------------------------------------------------------------------------------
integer(spi),parameter        :: nit=200
real(dp),    parameter        :: bcmins=-1.e-14_dp
real(dp),    dimension(6,0:6) :: rlui
real(dp),    dimension(0:6)   :: whex
real(dp)                      :: dwhex
integer(spi),dimension(0:6)   :: signs
integer(fpi),dimension(3,0:6) :: deflhex
integer(spi),dimension(6,0:6) :: deflui
integer(spi),dimension(-6:6)  :: sstriad
integer(spi),dimension(6)     :: dlui,ttriad
integer(fpi),dimension(3,0:2) :: Kset
integer(fpi),dimension(3,3,6) :: mmats
integer(spi),dimension(0:2)   :: Line
integer(spi),dimension(1)     :: ii
integer(fpi),dimension(3,0:6) :: lhex
integer(spi),dimension(6,0:6) :: lui
integer(spi),dimension(0:6)   :: jcol
integer(spi)                  :: hcol
integer(spi)                  :: i,ip,it,j,kcol,dcol,L
data deflhex/0,0,0,  1,-1,0,  0,1,-1,  0,0,1,  -1,0,1,  0,1,0,  1,0,0/
data deflui/ 6*0,   0, 0, 0, 0, 0,-1,   0, 0, 0,-1, 0, 0,   0, 0, 1, 1, 1, 0, &
                    0, 0, 0, 0,-1, 0,   0, 1, 0, 1, 0, 1,   1, 0, 0, 0, 1, 1/
data Mmats/1, 1,-1,  1, 0, 0,  1, 0,-1,     -1, 1, 0, -1, 1, 1,  0, 1, 0, &
           0,-1, 1,  1,-1, 0,  1, 0, 0,      0, 0, 1,  0,-1, 1,  1,-1, 1, &
          -1, 0, 1,  0, 0, 1, -1, 1, 0,      0, 1, 0,  1, 0,-1,  0, 1,-1/
data ttriad/5,3,3,6,5,6/
data sstriad/-1,-1, 1,-1, 1, 1,   1,-1,-1, 1,-1, 1, 1/
data Line/1,2,4/
data jcol/7,4,6,3,5,2,1/
!==============================================================================
lhex=deflhex; lui=deflui; hcol=0
rlui=lui; whex=matmul(aspect,rlui)
do i=0,2; Kset(:,i)=Lhex(:,modulo(hcol-line(i),7)); enddo
do it=1,nit
   ii=minloc(whex)-1; kcol=ii(1); dwhex=whex(kcol); if(dwhex>=bcmins)exit
   dcol=modulo(kcol-hcol,7); hcol=kcol; L=modulo(hcol+ttriad(dcol),7)
   Kset=matmul(Kset,Mmats(:,:,dcol))
   dlui=lui(:,hcol)
   signs=sstriad(-L:6-L)
   lui =lui+outer_product(dlui,signs)
   whex=whex+signs*dwhex
enddo
ff=it>nit; if(ff)return
do i=0,2; ip=modulo(i+1,3)
   lhex(:,modulo(hcol-line(i),7))=Kset(:,i)
   lhex(:,modulo(hcol+line(i),7))=Kset(:,i)-Kset(:,ip)
enddo
lhex(:,kcol)=0
lhex7=0
whex7=u0
do i=0,6
   j=jcol(i)
   lhex7(:,j)=lhex(:,i)
   whex7(  j)=whex(  i)
enddo

end subroutine hexad

!=================================================================== [gethexlu]
subroutine gethexlu(lhex,lu)
!==============================================================================
use jp_pkind, only: spi; use jp_pkind2, only: fpi
implicit none
integer(fpi),dimension(3,0:6),intent(in ):: lhex
integer(fpi),dimension(6,0:6),intent(out):: lu
!------------------------------------------------------------------------------
integer(spi):: i,L
!==============================================================================
do i=0,6; do L=1,6; lu(L,i)=Lhex(i3pair(1,L),i)*Lhex(i3pair(2,L),i);enddo;enddo
end subroutine gethexlu

!==============================================================================
subroutine queryhcol(vin,hcol)!                                     [queryhcol]
!==============================================================================
use jp_pkind, only: spi
implicit none
integer(spi),dimension(3),intent(in ):: vin
integer(spi),             intent(out):: hcol
!------------------------------------------------------------------------------
integer(spi),dimension(7):: hcols
integer(spi)             :: i
data hcols/6,5,1,3,4,2,0/
!==============================================================================
i=modulo(vin(1),2)+2*modulo(vin(2),2)+4*modulo(vin(3),2)
if(i==0)stop 'In queryhcol; invalid 3-vector Vin has all components even'
hcol=hcols(i)
end subroutine queryhcol

!=================================================================== [dectform]
subroutine dectforms(lx,mx,ly,my,lz,mz,lw,mw,aspects,qcols, &
     dixs,diys,dizs,diws, ff)
!==============================================================================
! Perform direct Decad and ha transforms in a proper subdomain
! domains extents in x, y, z, w, are lx:mx, ly:my, lz:mz, lw:mw
! aspects: upon input, these are the 10-vectors of grid-relative aspect tensor
!          upon output, these are the ten active-line-filter half-spans.
! qcols:   outout as the Galois "colors" of each successive line-filter, listed
!          in ascending order, with zeros at positions 0 and 11 of each list.
! dixs:    x-component of each of the 6 active line generators
! diys:    y-component
! dizs:    z-component
! diws:    w-component
! ff:      Logical failure flag, output .true. when failure occurs.
! Note that the integer arrays, qcols, doxs, diys, dizs, diws, 
! are 1-byte integers.
!
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
implicit none
integer(spi),                                        intent(in   ):: lx,mx,&
                                                                     ly,my,&
                                                                     lz,mz,&
                                                                     lw,mw
real(dp),dimension(10,lx:mx,ly:my,lz:mz,lw:mw),      intent(inout):: aspects
integer(fpi),dimension(0:11,lx:mx,ly:my,lz:mz,lw:mw),intent(  out):: qcols
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw,10),  intent(  out):: dixs,&
                                                                     diys,&
                                                                     dizs,&
                                                                     diws
logical,                                             intent(  out):: ff
!------------------------------------------------------------------------------
integer(spi)                :: ix,iy,iz,iw
integer(fpi),dimension(4,10):: ldec
!==============================================================================
do iw=lw,mw
   do iz=lz,mz
      do iy=ly,my
         do ix=lx,mx
            call dectform(aspects(:,ix,iy,iz,iw),qcols(0:11,ix,iy,iz,iw),&
                 ldec,ff)
            if(ff)then
               print'(" Failure in dectform at ix,iy,iz,iw=",4i5)',&
                    ix,iy,iz,iw
               return
            endif
            dixs(ix,iy,iz,iw,:)=ldec(1,:)
            diys(ix,iy,iz,iw,:)=ldec(2,:)
            dizs(ix,iy,iz,iw,:)=ldec(3,:)
            diws(ix,iy,iz,iw,:)=ldec(4,:)
         enddo
      enddo
   enddo
enddo
end subroutine dectforms

!=================================================================== [dectform]
subroutine dectform(aspect, qcol,ldec, ff)
!==============================================================================
! Perform the direct Decad and hs transform.
! Take a 10-vector representation of the aspect tensor and
! transform it to the vector of half-spans
! and 1-byte-integer line generators, and color list.
! aspect: input as aspect tensor components, output as spread**2
! qcol   : output as colors of successive active lines, but with
!          "spare" null elements 0 and 11.
! ldec   : ten active line generators in ascending color order
! ff     : logical failure flag.
!=============================================================================
use jp_pkind,  only: spi,dp; use jp_pkind2, only: fpi
implicit none
real(dp),dimension(10),      intent(inout):: aspect
integer(fpi),dimension(0:11),intent(  out):: qcol
integer(fpi),dimension(4,10),intent(  out):: ldec
logical,                     intent(  out):: ff
!-----------------------------------------------------------------------------
real(dp),    dimension(  15):: wdec15
integer(fpi),dimension(4,15):: ldec15
integer(fpi)                :: i,j
!=============================================================================
call decad(aspect, ldec15,wdec15,ff)
if(ff)then
   print'(" In dectform; decad, failed; check aspect tensor")'
   return
endif
qcol(0)=0; qcol(11)=0
j=1
do i=1,15
   if(sum(abs(ldec15(:,i)))==0)cycle
   qcol(j)=i
   ldec(:,j)=ldec15(:,i)
   aspect(j)=wdec15(  i)
   j=j+1_fpi
enddo
do i=1,10
   call hstform(aspect(i),ff)
   if(ff)then
      print'(" In dectform; hstform failed at i=",i2)',i
      print'(" Check that inimomtab has been called to initialize exponent")'
      print'(" p, table size, nh, and the moment tables for line filters")'
      return
   endif
enddo

ff=(j/=11)
if(ff)print'(" In dectform; inconsistent decad generator set found")'
end subroutine dectform

!================================================================= [dectformi]
subroutine dectformi(aspect, qcol,ldec, ff)
!=============================================================================
! Perform the inverse hs and decad transform.
! Take a 10-vector of the active half-spans, their respective
! colors, and their line generators, and return the implied
! aspect tensor in the same 10-vector that contained the spreads**2
! aspect: input as spreads**2; output as aspect tensor components
! qcol  : colors of successive active decad members (using 1-byte integers)
! ldec  : corresponding successive line generators (using 1-byte integers)
! ff    : logical failure flag.
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
use jp_pmat4, only: outer_product
implicit none
real(dp),    dimension(  10),intent(inout):: aspect
integer(fpi),dimension(0:11),intent(in   ):: qcol
integer(fpi),dimension(4,10),intent(in   ):: ldec
logical,                     intent(  out):: ff
!------------------------------------------------------------------------------
real(dp),dimension(4,4):: a44
real(dp),dimension(4)  :: vec
integer(spi)           :: i,j
!==============================================================================
a44=u0
j=1
do i=1,15
   if(qcol(j)/=i)cycle
   call hstformi(aspect(j),ff)
   if(ff)then
      print'(" In dectformi; hstformi failed at i,j=",2i3)',i,j
      print'(" Check that inimomtab has been called to initialize exponent")'
      print'(" p, table size, nh, and the moment tables for line filters")'
      return
   endif
    vec=ldec(:,j)
   a44=a44+outer_product(vec,vec)*aspect(j)
   j=j+1
enddo
ff=(j/=11)
if(ff)then
   print'(" In dectformi; Inconsistent qcol")'
   return
endif
call t44_to_10(a44,aspect)
end subroutine dectformi

!====================================================================== [decad]
subroutine decad(aspect,ldec15,wdec15,ff)
!==============================================================================
! This version is derived from $HOMES/on500/decadf.f90
! In this version ALWAYS start from the default decad
! Also, rearrange the 10 active line directions and weights
! into arrays of 15, ordered according the colors of the fundamental
! 3*3*3*3 cube's surface generators' degrees of "cardinality". By this
! we mean that the colors of (1,0,0,0), (0,1,0,0), (0,0,1,0), (0,0,0,1)
! come first, followed by the colors of (1,1,0,0), (1,0,1,0), (1,0,0,1),
! (0,1,1,0), (0,1,0,1), (0,0,1,1), followed by the colors of (1,1,1,0),
! (1,1,0,1), (1,0,1,1), (0,1,1,1), and followed finally by the color
! of the "least cardinal" (or "most diagonal") type of element, (1,1,1,1).
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
use jp_pbfil2,only: dec0,dodec0t,umat10,umat12,umats,nei,dcol10,dcol12,&
     nei0a,jcora,nei0b,jcorb,nei17,nei22,nei33,nei38,  tcors,&
     kcor10a5,kcor10b1,kcor10b2,kcor12b0, &
     kcor17c0,kcor22c0,kcor33c0,kcor38c0,kcor44c0,kcor51c0,kcor53c0,kcor58c0,&
     twt10a5,twt10b1,twt10b2,twt12c0,qwt10a,qwt10b,qwt10c,qwt10d,qwt10e, &
     qwt12a,qwt12b0,tperms,perm10,perm12,perms
use jp_pmat,  only: inv
use jp_pmat4, only: outer_product,det
implicit none
real(dp),dimension(10),      intent(in ):: aspect
integer(fpi),dimension(4,15),intent(out):: ldec15
real(dp),    dimension(  15),intent(out):: wdec15
logical,                     intent(out):: ff
!------------------------------------------------------------------------------
integer(spi), parameter                 :: nit=40
real(dp),parameter                      :: bcmins=-1.e-14_dp
real(dp),dimension(10,0:9)              :: rlui
real(dp),dimension(0:9)                 :: awdec,xwdec,newwdec,wdec
real(dp)                                :: dwdec
integer(spi)                            :: ktyp,dcol ! Redundant?
integer(spi),dimension(0:9)             :: palet     !
integer(spi),dimension(4,0:9)           :: eldec     !
integer(spi),dimension(10,0:9)          :: lu,lui
integer(fpi),dimension(4,0:9)           :: defeldec
integer(spi),dimension(4,0:9)           :: neweldec
integer(spi),dimension(0:9)             :: defpalet
integer(spi),dimension(1)               :: ii
integer(spi),dimension(4,4)             :: tcor
integer(spi)                            :: i,it,j,k,newktyp,newdcol,abscol,&
                                           jcol,kcor,jcor
integer(spi),dimension(4,0:3)           :: newbase
integer(spi),dimension(0:9)             :: perm,qwt,tperm
integer(spi),dimension(0:14)            :: icol15
data icol15/1,2,3,4,5,8,10,12,6,9,11,14,15,13,7/
data defeldec/                                                         &
   0, 0, 1, 0,   0,-1, 0, 0,   1, 0, 0, 0,  -1, 0,-1,-1,   0, 1, 0, 1, &
   0, 0, 0,-1,  -1, 0,-1, 0,   1, 1, 1, 1,  -1,-1, 0,-1,   1, 0, 0, 1/
data defpalet/ 2, 1, 0,13, 9, 3, 8,12, 7,14/
!==============================================================================
eldec=defeldec; palet=defpalet; ktyp=4; dcol=4
do j=0,9; call t4_to_10(eldec(:,j),lu(:,j)); enddo
lui=transpose(lu)
call inv(lui,ff)
if(ff)then
   print'(" In decad, at A; lu cannot be inverted")'
   return
endif
rlui=lui
wdec=matmul(aspect,rlui)
do it=1,nit
   ii=minloc(wdec)-1; k=ii(1); dwdec=wdec(k);
   if(dwdec>=bcmins)exit
!-- The following is translated from the "x" block of old tdecadf:
   newktyp=nei(k,ktyp)
   if(ktyp<12)then
      abscol=modulo(dcol+dcol10(k,ktyp),15)! Anticipated uncorrected abs col
      newbase(:,:)=matmul(eldec(:,0:3),umat10(:,:,k,ktyp))
   else
      if(k<4)then
         abscol=modulo(dcol+dcol12(k,ktyp),15)
         newbase(:,:)=matmul(eldec(:,0:3),umat12(:,:,k,ktyp))/2
      else
         abscol=dcol
         newbase(:,:)=matmul(eldec(:,0:3),umats(:,:,k))/2
      endif
   endif
   jcol=0
   jcor=0
   if(newktyp==11)then
      jcol=abscol/3
      if(jcol>0)then
         jcor=6+jcol
      endif
      abscol=modulo(abscol,3)
   elseif(newktyp>=44)then
      jcol=abscol/5
      if(jcol>0)then
         select case(ktyp)
         case(0:3)
            newktyp=nei0a(jcol,ktyp)
            jcor=jcora(jcol,ktyp)
         case(4:9)
            newktyp=nei0b(jcol,k,ktyp)
            jcor=jcorb(jcol,k,ktyp)
         case(17); newktyp=nei17(jcol); jcor=10+jcol
         case(22); newktyp=nei22(jcol); jcor=10+jcol
         case(33); newktyp=nei33(jcol); jcor=10+jcol
         case(38); newktyp=nei38(jcol); jcor=10+jcol
         case(44); jcor=10+jcol
         case(51); jcor=10+jcol
         case(53); jcor=10+jcol
         case(58); jcor=10+jcol
         case default
            print'(" In decad. Unrecognized ktyp=",i10)',ktyp
            ff=.true.
            return
         end select
      endif
      abscol=modulo(abscol,5)
      if(ktyp<12)then
         newdcol=modulo(abscol-dcol10(k,ktyp),15)
      else
         if(k<4)then
            newdcol=modulo(abscol-dcol12(k,ktyp),15)
         else
            newdcol=dcol
         endif
      endif
   endif
   if(jcor /= 0)then
      tcor=tcors(:,:,jcor)
      newbase=matmul(newbase(:,:),tcor)/2
   endif

   if(ktyp<12)then
      perm=perm10(:,k,ktyp)
      select case(ktyp)
      case(0:3)
         if(k==5)then
            kcor=kcor10a5(jcol,ktyp)
            qwt=twt10a5(:,kcor)
         else
            qwt=qwt10a(:,k)
         endif
      case(4:7)
         if(k==1)then
            kcor=kcor10b1(jcol,ktyp)
            qwt=twt10b1(:,kcor)
         elseif(k==2)then
            kcor=kcor10b2(jcol,ktyp)
            qwt=twt10b2(:,kcor)
         else
            qwt=qwt10b(:,k)
         endif
      case(8:9)
         if(k==1)then
            kcor=kcor10b1(jcol,ktyp)
            qwt=twt10b1(:,kcor)
         elseif(k==2)then
            kcor=kcor10b2(jcol,ktyp)
            qwt=twt10b2(:,kcor)
         else
            qwt=qwt10c(:,k)
         endif
      case(10)
         qwt=qwt10d(:,k)
      case(11)
         qwt=qwt10e(:,k)
      end select
   else
      if(k==0)then
         perm=perm12(:,k,ktyp)
         kcor=kcor12b0(ktyp)
         select case(ktyp)
         case(17); kcor=kcor17c0(jcol); qwt=twt12c0(:,kcor)
         case(22); kcor=kcor22c0(jcol); qwt=twt12c0(:,kcor)
         case(33); kcor=kcor33c0(jcol); qwt=twt12c0(:,kcor)
         case(38); kcor=kcor38c0(jcol); qwt=twt12c0(:,kcor)
         case(44); kcor=kcor44c0(jcol); qwt=twt12c0(:,kcor)
         case(51); kcor=kcor51c0(jcol); qwt=twt12c0(:,kcor)
         case(53); kcor=kcor53c0(jcol); qwt=twt12c0(:,kcor)
         case(58); kcor=kcor58c0(jcol); qwt=twt12c0(:,kcor)
         case default
            qwt=qwt12b0(:,kcor)
         end select
      elseif(k<4)then
         perm=perm12(:,k,ktyp)
         qwt=qwt12a(:,k)
      else
         perm=perms(:,k)
         qwt=qwt12a(:,k)
      endif
   endif
   if(jcor/=0)then
      do i=0,9
         tperm(i)=tperms(perm(i),jcor)
      enddo
      perm=tperm
   endif
   call standardizeb(newbase(:,:),FF)
   if(FF)then
      print'(" In decad, at B;  failure of subr. standardizedb")'
      return
   endif

!--------
   awdec=wdec-qwt*dwdec
   do i=0,9
      newwdec(perm(i))=awdec(i)
   enddo
   if(newktyp<12)then
      neweldec=matmul(newbase,dec0)
   else
      neweldec=matmul(newbase,dodec0t)/2
   endif
   do j=0,9
      call t4_to_10(neweldec(:,j),lu(:,j))
   enddo
   lui=transpose(lu)
   call inv(lui,ff)
   if(ff)then
      print'(" In decad, at C; lu cannot be inverted")'
      return
   endif
   rlui=lui
   xwdec=matmul(aspect,rlui)
!   if(maxval(abs(xwdec-newwdec))>.001)read(*,*)
   eldec=neweldec
   ktyp=newktyp
   dcol=abscol
   wdec=xwdec
enddo
if(it>nit)then
   ff=.true.
   print '(" in decad, at D; failure of decad iterations to converge")'
   return
endif
do j=0,9
   call querydcol(eldec(:,j),palet(j))
enddo
print'(" departing decad having used it = ",i5," iterations.")',it
! Insert the decad into its proper color slots in order of decreasing
! "cardinality:"
wdec15=u0
ldec15=0
do i=0,9
   j=icol15(palet(i))
!   ldec15(:,j)=int(eldec(:,i),kind(fpi))
   ldec15(:,j)=int(eldec(:,i),fpi)
   wdec15(  j)= wdec(  i)
enddo
end subroutine decad

!=================================================================== [getdeclu]
subroutine getdeclu(ldec,lu)
!==============================================================================
use jp_pkind, only: spi; use jp_pkind2, only: fpi
implicit none
integer(spi),dimension( 4,0:14),intent(in ):: ldec
integer(spi),dimension(10,0:14),intent(out):: lu
!------------------------------------------------------------------------------
integer(spi):: i,L
!==============================================================================
do i=0,14;do L=1,10;lu(L,i)=Ldec(i4pair(1,L),i)*Ldec(i4pair(2,L),i);enddo;enddo
end subroutine getdeclu

!==============================================================================
subroutine querydcol(vin,dcol)!                                     [querydcol]
!==============================================================================
use jp_pkind, only: spi; use jp_pkind2, only: fpi
implicit none
integer(spi),dimension(4),intent(in ):: vin
integer(spi),             intent(out):: dcol
!------------------------------------------------------------------------------
integer(spi),dimension(15):: dcols
integer(spi),dimension(4) :: bbbb
integer(spi)              :: i
data dcols/ 0, 1, 4, 2, 8, 5,10, 3,14, 9, 7, 6,13,11,12/
data bbbb/1,2,4,8/
!==============================================================================
i=dot_product(bbbb,modulo(vin,2))
if(i==0)stop 'In querydcol; invalid 4-vector Vin has all components even'
dcol=dcols(i)
end subroutine querydcol

!=============================================================== [standardizeb]
subroutine standardizeb(bases,FF)
!==============================================================================
! Standardize 4*4 bases vectors by making sure the first nonzero component
! of the first column is positive in the standardized version.
! If the first column is null, raise the (logical) failure flag, FF.
!==============================================================================
use jp_pkind, only: spi
implicit none
integer(spi),dimension(4,4),intent(inout):: bases
logical,                    intent(  out):: FF
integer(spi)                             :: i,b
!==============================================================================
FF=.false.
do i=1,4
   b=bases(i,1)
   if(b==0)cycle
   if(b<0)bases=-bases
   return
enddo
print'(" WARNING! In subroutine standardizeb, first column is null:")'
FF=.true.
end subroutine standardizeb

!==================================================================== [hstform]
subroutine hstform(hs,ff)!            
!==============================================================================
! Perform the "hspan transform". For a given spread**2, replace it with the
! corresponding effective half-span corresponding to beta filters of the 
! already-initialized exponent p. Generally, hs>=1, lies between consecutive
! integers, h, h+1 <=nh (nh is also already given in jp_pbfil2.mod). The linear
! interpolation weights at h and h+1 for this target, applied to the
! "interpolation" of the two standardized p-exponent beta distributions of 
! half-spans h and h+1 will also be standardized (sum of gridded responses = 1)
! and will possess exactly the prescribed spread**2, the input hs.
! This transform is obviously invertible (see subr. hstformi).
! But if the given hs does not fit within the range of the
! table, bsprds, return a raised failure flag, ff.
!==============================================================================
use jp_pkind, only: spi,dp
use jp_pietc, only: u0
use jp_pbfil2,only: nh,bsprds
implicit none
real(dp),intent(inout):: hs
logical, intent(  out):: ff
!------------------------------------------------------------------------------
integer(spi):: h
!==============================================================================
ff=hs<u0
do h=2,nh
   if(bsprds(h) >= hs)then
      hs=h-(bsprds(h)-hs)/(bsprds(h)-bsprds(h-1))
      return
   endif
enddo
ff=.true.
end subroutine hstform

!=================================================================== [hstformi]
subroutine hstformi(hs,ff)
!==============================================================================
! Perform the "inverse hspan transform" (inverse function of hstform) so that
! an effective p-exponent beta filter half-span, hs, is replaced by the second
! moment (spread**2) of the dibeta filter this half-span implies.
! If the given half-span is not accommodated by the prepared table, bsprds, of
! module jp_pbfil3, return a raised failure flag, ff.
!==============================================================================
use jp_pkind, only: spi,dp
use jp_pietc, only: u1
use jp_pbfil2,only: nh,bsprds
implicit none
real(dp),intent(inout):: hs
logical, intent(  out):: ff
!------------------------------------------------------------------------------
real(dp)    :: w
integer(spi):: h
!==============================================================================
h=1+int(hs)
ff=(h<2 .or. h>nh)
if(ff)then
   print'(" In hstformi; hs out of bounds")'
   return
endif
! Linearly interpolate the spread**2 from the table bsprds:
w=h-hs
hs=w*bsprds(h-1)+(u1-w)*bsprds(h)
end subroutine hstformi

!==================================================================== [blinfil]
subroutine blinfil(nfil,hspan, h,fil,ff)
!==============================================================================
! Find the discrete halfspan h and the filtering weights, fil(0:h), of
! the normalized dibeta filter of formal real half-span, hspan. The dibeta
! filter is just a weighted combination of two consecutive-halfspan
! beta filters such that the spread**2 of the dibeta is the weighted
! intermediate of the spreads**2 of the pair of beta filters from which it
! is composed.
!
! p: beta filter exponent index
! nh: size of the table listing the normalization factors and spreads**2
! bnorm: table of normalization factors for beta filters of integer halfspan
! bsprds: table of squared-spreads of the beta filters
! hspan: formal real half-span of the dibeta filter
! fil: a real array, [0:nh], sufficient to accommodate one half of the
! symmetric discrete dibeta filter.
! ff: logical failure flag raised when hspan lies outside the table range.
!==============================================================================
use jp_pkind, only: spi,dp
use jp_pietc, only: u1
use jp_pbfil2,only: p,nh,bnorm
implicit none
integer(spi),              intent(in ):: nfil
real(dp),                  intent(in ):: hspan
integer(spi),              intent(out):: h
real(dp),dimension(0:nfil),intent(out):: fil
logical,                   intent(out):: ff
!------------------------------------------------------------------------------
real(dp)    :: wh,whp,z
integer(spi):: hp,i
!==============================================================================
h=int(hspan); hp=h+1; ff=h<1 .or. hp>nh .or. hp>nfil; if(ff)return
whp =(hspan-h)*bnorm(hp)! linear interpolation weight at hp=h+1
wh=(hp-hspan)*bnorm(h)! linear interpolation weight at h
! start with the contribution of the filter of formal halfspan h+1:
do i=0,h;   z=i; z=(z/hp)**2; fil(i)=      whp*(u1-z)**p; enddo
! add the contribution of the filter of formal halfspan h:
do i=0,h-1; z=i; z=(z/h)**2;  fil(i)=fil(i)+wh*(u1-z)**p; enddo
end subroutine blinfil

!-- The following routines share the interface, dibeta:
!===================================================================== [dibeta]
subroutine dibeta1(kx,lx,mx,nx, nfil,dixs,hss,a,ff,ix)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                 intent(in   ):: kx,lx,mx,nx, nfil
integer(fpi),dimension(lx:mx),intent(in   ):: dixs
real(dp),    dimension(lx:mx),intent(in   ):: hss
real(dp),    dimension(kx:nx),intent(inout):: a
logical,                      intent(  out):: ff
integer(spi),                 intent(  out):: ix
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil):: fil
real(dp),dimension(kx:nx) :: b
real(dp)                  :: fili
integer(spi)              :: h,i,dix,dixi
!==============================================================================
b=u0
do ix=lx,mx
   dix=dixs(ix)
   if(dix==0)then;b(ix)=a(ix)
   else
      call blinfil(nfil,hss(ix),h,fil,ff); if(ff)return
      b(ix)=fil(0)*a(ix)
      do i=1,h
         fili=fil(i); dixi=dix*i
         b(ix)=b(ix)+fili*(a(ix+dixi)+a(ix-dixi))
      enddo
   endif
enddo
a=b
end subroutine dibeta1
!===================================================================== [dibeta]
subroutine dibeta2(kx,lx,mx,nx, ky,ly,my,ny, nfil, &
     dixs,diys,hss, a,  ff,ix,iy)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                       intent(in   ):: kx,lx,mx,nx,&
                                                    ky,ly,my,ny,&
                                                    nfil
integer(fpi),dimension(lx:mx,ly:my),intent(in   ):: dixs,diys
real(dp),    dimension(lx:mx,ly:my),intent(in   ):: hss
real(dp),    dimension(kx:nx,ky:ny),intent(inout):: a
logical,                            intent(  out):: ff
integer(spi),                       intent(  out):: ix,iy
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)     :: fil
real(dp),dimension(kx:nx,ky:ny):: b
real(dp)                       :: fili
integer(spi)                   :: h,i,dix,diy,dixi,diyi
!==============================================================================
b=u0
do iy=ly,my; do ix=lx,mx
   dix=dixs(ix,iy); diy=diys(ix,iy)
   if(abs(dix)+abs(diy)==0)then;b(ix,iy)=a(ix,iy)
   else
      call blinfil(nfil,hss(ix,iy),h,fil,ff); if(ff)return
      b(ix,iy)=fil(0)*a(ix,iy)
      do i=1,h
         fili=fil(i); dixi=dix*i; diyi=diy*i
         b(ix,iy)=b(ix,iy)+fili*(a(ix+dixi,iy+diyi)+a(ix-dixi,iy-diyi))
      enddo
   endif
enddo; enddo
a=b
end subroutine dibeta2
!===================================================================== [dibeta]
subroutine dibeta3(kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, nfil, &
     dixs,diys,dizs,hss, a,  ff,ix,iy,iz)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                             intent(in   ):: kx,lx,mx,nx,&
                                                          ky,ly,my,ny,&
                                                          kz,lz,mz,nz,&
                                                          nfil
integer(fpi),dimension(lx:mx,ly:my,lz:mz),intent(in   ):: dixs,diys,dizs
real(dp),    dimension(lx:mx,ly:my,lz:mz),intent(in   ):: hss
real(dp),    dimension(kx:nx,ky:ny,kz:nz),intent(inout):: a
logical,                                  intent(  out):: ff
integer(spi),                             intent(  out):: ix,iy,iz
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)           :: fil
real(dp),dimension(kx:nx,ky:ny,kz:nz):: b
real(dp)                             :: fili
integer(spi)                         :: h,i,         &
                                        dix,diy,diz, &
                                        dixi,diyi,dizi
!==============================================================================
b=u0
do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   dix=dixs(ix,iy,iz); diy=diys(ix,iy,iz); diz=dizs(ix,iy,iz)
   if(abs(dix)+abs(diy)+abs(diz)==0)then;b(ix,iy,iz)=a(ix,iy,iz)
   else
      call blinfil(nfil,hss(ix,iy,iz),h,fil,ff); if(ff)return
      b(ix,iy,iz)=fil(0)*a(ix,iy,iz)
      do i=1,h
         fili=fil(i); dixi=dix*i; diyi=diy*i; dizi=diz*i
         b(ix,iy,iz)=b(ix,iy,iz)+fili*   &
              (a(ix+dixi,iy+diyi,iz+dizi)&
              +a(ix-dixi,iy-diyi,iz-dizi))
      enddo
   endif
enddo; enddo; enddo
a=b
end subroutine dibeta3
!===================================================================== [dibeta]
subroutine dibeta4(kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, kw,lw,mw,nw, &
     nfil, dixs,diys,dizs,diws,hss, a,          ff,ix,iy,iz,iw)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                                   intent(in   ):: kx,lx,mx,nx,&
                                                                ky,ly,my,ny,&
                                                                kz,lz,mz,nz,&
                                                                kw,lw,mw,nw,&
                                                                nfil
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw),intent(in   ):: dixs,diys,&
                                                                dizs,diws
real(dp),    dimension(lx:mx,ly:my,lz:mz,lw:mw),intent(in   ):: hss
real(dp),    dimension(kx:nx,ky:ny,kz:nz,kw:nw),intent(inout):: a
logical,                                        intent(  out):: ff
integer(spi),                                   intent(  out):: ix,iy,iz,iw
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)                 :: fil
real(dp),dimension(kx:nx,ky:ny,kz:nz,kw:nw):: b
real(dp)                                   :: fili
integer(spi)                               :: h,i,             &
                                              dix,diy,diz,diw, &
                                              dixi,diyi,dizi,diwi
!==============================================================================
b=u0
do iw=lw,mw; do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   dix=dixs(ix,iy,iz,iw);diy=diys(ix,iy,iz,iw)
   diz=dizs(ix,iy,iz,iw);diw=diws(ix,iy,iz,iw)
   if(abs(dix)+abs(diy)+abs(diz)+abs(diw)==0)then;b(ix,iy,iz,iw)=a(ix,iy,iz,iw)
   else
      call blinfil(nfil,hss(ix,iy,iz,iw),h,fil,ff); if(ff)return
      b(ix,iy,iz,iw)=fil(0)*a(ix,iy,iz,iw)
      do i=1,h
         fili=fil(i); dixi=dix*i; diyi=diy*i; dizi=diz*i; diwi=diw*i
         b(ix,iy,iz,iw)=b(ix,iy,iz,iw)+fili*     &
              (a(ix+dixi,iy+diyi,iz+dizi,iw+diwi)&
              +a(ix-dixi,iy-diyi,iz-dizi,iw-diwi))
      enddo
   endif
enddo; enddo; enddo; enddo
a=b
end subroutine dibeta4

!===================================================================== [dibeta]
subroutine dibetax3(kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, icol,nfil,&
     qcols,dixs,diys,dizs, jcol,hss,a, ff,ix,iy,iz)
!=============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                                 intent(in   ):: kx,lx,mx,nx, &
                                                              ky,ly,my,ny, &
                                                              kz,lz,mz,nz, &
                                                              icol,nfil
integer(fpi),dimension(0:7,lx:mx,ly:my,lz:mz),intent(in   ):: qcols
integer(fpi),dimension(lx:mx,ly:my,lz:mz,6),  intent(in   ):: dixs,diys,dizs

integer(fpi),dimension(lx:mx,ly:my,lz:mz),    intent(inout):: jcol
real(dp),dimension(6,lx:mx,ly:my,lz:mz),      intent(in   ):: hss
real(dp),dimension(kx:nx,ky:ny,kz:nz),        intent(inout):: a
logical,                                      intent(  out):: ff
integer(spi),                                 intent(  out):: ix,iy,iz
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)           :: fil
real(dp),dimension(kx:nx,ky:ny,kz:nz):: b
real(dp)                             :: fili,hs
integer(spi)                         :: h,i,         &
                                        dix,diy,diz, &
                                        dixi,diyi,dizi
integer(fpi)                         :: j
!==============================================================================
b=u0
if(icol==1)jcol=1
do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   j=jcol(ix,iy,iz)
   if(icol/=qcols(j,ix,iy,iz))then
      b(ix,iy,iz)=a(ix,iy,iz)
      cycle
   else
      jcol(ix,iy,iz)=j+1_fpi
      dix=dixs(ix,iy,iz,j); diy=diys(ix,iy,iz,j); diz=dizs(ix,iy,iz,j)
      hs=hss(j,ix,iy,iz)
      call blinfil(nfil,hs,h,fil,ff); if(ff)return
      b(ix,iy,iz)=fil(0)*a(ix,iy,iz)
      do i=1,h
         fili=fil(i); dixi=dix*i; diyi=diy*i; dizi=diz*i
         b(ix,iy,iz)=b(ix,iy,iz)+fili*  &
              (a(ix+dixi,iy+diyi,iz+dizi)&
              +a(ix-dixi,iy-diyi,iz-dizi))
      enddo
   endif
enddo;       enddo;       enddo
a=b
end subroutine dibetax3
!===================================================================== [dibeta]
subroutine dibetax4(kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, kw,lw,mw,nw, &
     icol,nfil,&
     qcols,dixs,diys,dizs,diws, jcol,hss,a, ff,ix,iy,iz,iw)
!=============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                                   intent(in   ):: kx,lx,mx,nx, &
                                                                ky,ly,my,ny, &
                                                                kz,lz,mz,nz, &
                                                                kw,lw,mw,nw, &
                                                                icol,nfil
integer(fpi),dimension(0:11,lx:mx,ly:my,lz:mz,lw:mw),&
                                                intent(in   ):: qcols
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw,10),&
                                                intent(in   ):: dixs,diys,&
                                                                dizs,diws
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw),intent(inout):: jcol
real(dp),dimension(10,lx:mx,ly:my,lz:mz,lw:mw), intent(in   ):: hss
real(dp),dimension(kx:nx,ky:ny,kz:nz,kw:nw),    intent(inout):: a
logical,                                        intent(  out):: ff
integer(spi),                                   intent(  out):: ix,iy,iz,iw
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)                 :: fil
real(dp),dimension(kx:nx,ky:ny,kz:nz,kw:nw):: b
real(dp)                                   :: fili,hs
integer(spi)                               :: h,i,             &
                                              dix,diy,diz,diw, &
                                              dixi,diyi,dizi,diwi
integer(fpi)                               :: j
!==============================================================================
b=u0
if(icol==1)jcol=1
do iw=lw,mw; do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   j=jcol(ix,iy,iz,iw)
   if(icol/=qcols(j,ix,iy,iz,iw))then
      b(ix,iy,iz,iw)=a(ix,iy,iz,iw)
      cycle
   else
      jcol(ix,iy,iz,iw)=j+1_fpi
      dix=dixs(ix,iy,iz,iw,j); diy=diys(ix,iy,iz,iw,j)
      diz=dizs(ix,iy,iz,iw,j); diw=diws(ix,iy,iz,iw,j)
      hs=hss(j,ix,iy,iz,iw)
      call blinfil(nfil,hs,h,fil,ff); if(ff)return
      b(ix,iy,iz,iw)=fil(0)*a(ix,iy,iz,iw)
      do i=1,h
         fili=fil(i); dixi=dix*i; diyi=diy*i; dizi=diz*i; diwi=diw*i
         b(ix,iy,iz,iw)=b(ix,iy,iz,iw)+fili*  &
              (a(ix+dixi,iy+diyi,iz+dizi,iw+diwi)&
              +a(ix-dixi,iy-diyi,iz-dizi,iw-diwi))
      enddo
   endif
enddo;       enddo;       enddo;     enddo
a=b
end subroutine dibetax4

!===================================================================== [dibeta]
subroutine vdibeta1(nv,kx,lx,mx,nx, nfil,dixs,hss,a,ff,ix)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                 intent(in   ):: nv,kx,lx,mx,nx, nfil
integer(fpi),dimension(lx:mx),intent(in   ):: dixs
real(dp),    dimension(lx:mx),intent(in   ):: hss
real(dp), dimension(nv,kx:nx),intent(inout):: a
logical,                      intent(  out):: ff
integer(spi),                 intent(  out):: ix
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)   :: fil
real(dp),dimension(nv,kx:nx) :: b
real(dp)                     :: fili
integer(spi)                 :: h,i,dix,dixi
!==============================================================================
b=u0
do ix=lx,mx
   dix=dixs(ix)
   if(dix==0)then; b(:,ix)=a(:,ix)
   else
      call blinfil(nfil,hss(ix),h,fil,ff); if(ff)return
      b(:,ix)=fil(0)*a(:,ix)
      do i=1,h
         fili=fil(i); dixi=dix*i
         b(:,ix)=b(:,ix)+fili*(a(:,ix+dixi)+a(:,ix-dixi))
      enddo
   endif
enddo
a=b
end subroutine vdibeta1
!===================================================================== [dibeta]
subroutine vdibeta2(nv, kx,lx,mx,nx, ky,ly,my,ny, nfil, &
     dixs,diys,hss, a,  ff,ix,iy)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                       intent(in   ):: nv, &
                                                    kx,lx,mx,nx,&
                                                    ky,ly,my,ny,&
                                                    nfil
integer(fpi),dimension(lx:mx,ly:my),intent(in   ):: dixs,diys
real(dp),    dimension(lx:mx,ly:my),intent(in   ):: hss
real(dp), dimension(nv,kx:nx,ky:ny),intent(inout):: a
logical,                            intent(  out):: ff
integer(spi),                       intent(  out):: ix,iy
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)        :: fil
real(dp),dimension(nv,kx:nx,ky:ny):: b
real(dp)                          :: fili
integer(spi)                      :: h,i,dix,diy,dixi,diyi
!==============================================================================
b=u0
do iy=ly,my; do ix=lx,mx
   dix=dixs(ix,iy); diy=diys(ix,iy)
    if(abs(dix)+abs(diy)==0)then;b(:,ix,iy)=a(:,ix,iy)
   else
      call blinfil(nfil,hss(ix,iy),h,fil,ff); if(ff)return
      b(:,ix,iy)=fil(0)*a(:,ix,iy)
      do i=1,h
         fili=fil(i); dixi=dix*i; diyi=diy*i
         b(:,ix,iy)=b(:,ix,iy)+fili* &
              (a(:,ix+dixi,iy+diyi)+a(:,ix-dixi,iy-diyi))
      enddo
   endif
enddo; enddo
a=b
end subroutine vdibeta2
!===================================================================== [dibeta]
subroutine vdibeta3(nv, kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, nfil, &
     dixs,diys,dizs,hss, a,  ff,ix,iy,iz)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                             intent(in   ):: nv,         &
                                                          kx,lx,mx,nx,&
                                                          ky,ly,my,ny,&
                                                          kz,lz,mz,nz,&
                                                          nfil
integer(fpi),dimension(lx:mx,ly:my,lz:mz),intent(in   ):: dixs,diys,dizs
real(dp),    dimension(lx:mx,ly:my,lz:mz),intent(in   ):: hss
real(dp), dimension(nv,kx:nx,ky:ny,kz:nz),intent(inout):: a
logical,                                  intent(  out):: ff
integer(spi),                             intent(  out):: ix,iy,iz
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)              :: fil
real(dp),dimension(nv,kx:nx,ky:ny,kz:nz):: b
real(dp)                                :: fili
integer(spi)                            :: h,i,         &
                                           dix,diy,diz, &
                                           dixi,diyi,dizi
!==============================================================================
b=u0
do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   dix=dixs(ix,iy,iz); diy=diys(ix,iy,iz); diz=dizs(ix,iy,iz)
   if(abs(dix)+abs(diy)+abs(diz)==0)then;b(:,ix,iy,iz)=a(:,ix,iy,iz)
   else
      call blinfil(nfil,hss(ix,iy,iz),h,fil,ff); if(ff)return
      b(:,ix,iy,iz)=fil(0)*a(:,ix,iy,iz)
      do i=1,h
         fili=fil(i); dixi=dix*i; diyi=diy*i; dizi=diz*i
         b(:,ix,iy,iz)=b(:,ix,iy,iz)+fili*   &
              (a(:,ix+dixi,iy+diyi,iz+dizi)&
              +a(:,ix-dixi,iy-diyi,iz-dizi))
      enddo
   endif
enddo; enddo; enddo
a=b
end subroutine vdibeta3
!===================================================================== [dibeta]
subroutine vdibeta4(nv, kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, kw,lw,mw,nw, &
     nfil, dixs,diys,dizs,diws,hss, a,          ff,ix,iy,iz,iw)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                                   intent(in   ):: nv,         &
                                                                kx,lx,mx,nx,&
                                                                ky,ly,my,ny,&
                                                                kz,lz,mz,nz,&
                                                                kw,lw,mw,nw,&
                                                                nfil
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw),intent(in   ):: dixs,diys,&
                                                                dizs,diws
real(dp),    dimension(lx:mx,ly:my,lz:mz,lw:mw),intent(in   ):: hss
real(dp), dimension(nv,kx:nx,ky:ny,kz:nz,kw:nw),intent(inout):: a
logical,                                        intent(  out):: ff
integer(spi),                                   intent(  out):: ix,iy,iz,iw
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)                    :: fil
real(dp),dimension(nv,kx:nx,ky:ny,kz:nz,kw:nw):: b
real(dp)                                      :: fili
integer(spi)                                  :: h,i,             &
                                                 dix,diy,diz,diw, &
                                                 dixi,diyi,dizi,diwi
!==============================================================================
b=u0
do iw=lw,mw; do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   dix=dixs(ix,iy,iz,iw);diy=diys(ix,iy,iz,iw)
   diz=dizs(ix,iy,iz,iw);diw=diws(ix,iy,iz,iw)
   if(abs(dix)+abs(diy)+abs(diz)+abs(diw)==0)then
      b(:,ix,iy,iz,iw)=a(:,ix,iy,iz,iw)
   else
      call blinfil(nfil,hss(ix,iy,iz,iw),h,fil,ff); if(ff)return
      b(:,ix,iy,iz,iw)=fil(0)*a(:,ix,iy,iz,iw)
      do i=1,h
         fili=fil(i); dixi=dix*i; diyi=diy*i; dizi=diz*i; diwi=diw*i
         b(:,ix,iy,iz,iw)=b(:,ix,iy,iz,iw)+fili*     &
              (a(:,ix+dixi,iy+diyi,iz+dizi,iw+diwi)&
              +a(:,ix-dixi,iy-diyi,iz-dizi,iw-diwi))
      enddo
   endif
enddo; enddo; enddo; enddo
a=b
end subroutine vdibeta4

!===================================================================== [dibeta]
subroutine vdibetax3(nv,kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, icol,nfil,&
     qcols,dixs,diys,dizs, jcol,hss,a, ff,ix,iy,iz)
!=============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                                 intent(in   ):: nv,          &
                                                              kx,lx,mx,nx, &
                                                              ky,ly,my,ny, &
                                                              kz,lz,mz,nz, &
                                                              icol,nfil
integer(fpi),dimension(0:7,lx:mx,ly:my,lz:mz),intent(in   ):: qcols
integer(fpi),dimension(lx:mx,ly:my,lz:mz,6),  intent(in   ):: dixs,diys,dizs
integer(fpi),dimension(lx:mx,ly:my,lz:mz),    intent(inout):: jcol
real(dp),dimension(6,lx:mx,ly:my,lz:mz),      intent(in   ):: hss
real(dp),dimension(nv,kx:nx,ky:ny,kz:nz),     intent(inout):: a
logical,                                      intent(  out):: ff
integer(spi),                                 intent(  out):: ix,iy,iz
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)              :: fil
real(dp),dimension(nv,kx:nx,ky:ny,kz:nz):: b
real(dp)                                :: fili,hs
integer(spi)                            :: h,i,             &
                                           dix,diy,diz, &
                                           dixi,diyi,dizi
integer(fpi)                            :: j
!==============================================================================
b=u0
if(icol==1)jcol=1
do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   j=jcol(ix,iy,iz)
   if(icol/=qcols(j,ix,iy,iz))then
      b(:,ix,iy,iz)=a(:,ix,iy,iz)
      cycle
   else
      jcol(ix,iy,iz)=j+1_fpi
      dix=dixs(ix,iy,iz,j); diy=diys(ix,iy,iz,j); diz=dizs(ix,iy,iz,j)
      hs=hss(j,ix,iy,iz)
      call blinfil(nfil,hs,h,fil,ff); if(ff)return
      b(:,ix,iy,iz)=fil(0)*a(:,ix,iy,iz)
      do i=1,h
         fili=fil(i); dixi=dix*i; diyi=diy*i; dizi=diz*i
         b(:,ix,iy,iz)=b(:,ix,iy,iz)+fili*  &
              (a(:,ix+dixi,iy+diyi,iz+dizi)&
              +a(:,ix-dixi,iy-diyi,iz-dizi))
      enddo
   endif
enddo;       enddo;       enddo
a=b
end subroutine vdibetax3
!===================================================================== [dibeta]
subroutine vdibetax4(nv,kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, kw,lw,mw,nw, &
     icol,nfil,&
     qcols,dixs,diys,dizs,diws, jcol,hss,a, ff,ix,iy,iz,iw)
!=============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                                   intent(in   ):: nv,          &
                                                                kx,lx,mx,nx, &
                                                                ky,ly,my,ny, &
                                                                kz,lz,mz,nz, &
                                                                kw,lw,mw,nw, &
                                                                icol,nfil
integer(fpi),dimension(0:11,lx:mx,ly:my,lz:mz,lw:mw),&
                                                intent(in   ):: qcols
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw,10),&
                                                intent(in   ):: dixs,diys,&
                                                                dizs,diws
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw),intent(inout):: jcol
real(dp),dimension(10,lx:mx,ly:my,lz:mz,lw:mw), intent(in   ):: hss
real(dp),dimension(nv,kx:nx,ky:ny,kz:nz,kw:nw), intent(inout):: a
logical,                                        intent(  out):: ff
integer(spi),                                   intent(  out):: ix,iy,iz,iw
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)                    :: fil
real(dp),dimension(nv,kx:nx,ky:ny,kz:nz,kw:nw):: b
real(dp)                                      :: fili,hs
integer(spi)                                  :: h,i,             &
                                                 dix,diy,diz,diw, &
                                                 dixi,diyi,dizi,diwi
integer(fpi)                                  :: j
!==============================================================================
b=u0
if(icol==1)jcol=1
do iw=lw,mw; do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   j=jcol(ix,iy,iz,iw)
   if(icol/=qcols(j,ix,iy,iz,iw))then
      b(:,ix,iy,iz,iw)=a(:,ix,iy,iz,iw)
      cycle
   else
      jcol(ix,iy,iz,iw)=j+1_fpi
      dix=dixs(ix,iy,iz,iw,j); diy=diys(ix,iy,iz,iw,j)
      diz=dizs(ix,iy,iz,iw,j); diw=diws(ix,iy,iz,iw,j)
      hs=hss(j,ix,iy,iz,iw)
      call blinfil(nfil,hs,h,fil,ff); if(ff)return
      b(:,ix,iy,iz,iw)=fil(0)*a(:,ix,iy,iz,iw)
      do i=1,h
         fili=fil(i); dixi=dix*i; diyi=diy*i; dizi=diz*i; diwi=diw*i
         b(:,ix,iy,iz,iw)=b(:,ix,iy,iz,iw)+fili*  &
              (a(:,ix+dixi,iy+diyi,iz+dizi,iw+diwi)&
              +a(:,ix-dixi,iy-diyi,iz-dizi,iw-diwi))
      enddo
   endif
enddo;       enddo;       enddo;     enddo
a=b
end subroutine vdibetax4

!--- The following routine share the interface, dibetat:

!==================================================================== [dibetat]
subroutine dibeta1t(kx,lx,mx,nx, nfil, dixs,hss, a,  ff,ix)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                 intent(in   ):: kx,lx,mx,nx,nfil
integer(fpi),dimension(lx:mx),intent(in   ):: dixs
real(dp),    dimension(lx:mx),intent(in   ):: hss
real(dp),    dimension(kx:nx),intent(inout):: a
logical,                      intent(  out):: ff
integer(spi),                 intent(  out):: ix
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil):: fil
real(dp),dimension(kx:nx) :: b
real(dp)                  :: filiat,at
integer(spi)              :: h,i,dix,dixi
!==============================================================================
b=u0
do ix=lx,mx
   at=a(ix)
   dix=dixs(ix)
   if(dix==0)then;b(ix)=b(ix)+at
   else
      call blinfil(nfil,hss(ix),h,fil,ff); if(ff)return
      b(ix)=b(ix)+fil(0)*at
      do i=1,h
         filiat=fil(i)*at; dixi=dix*i
         b(ix+dixi)=b(ix+dixi)+filiat
         b(ix-dixi)=b(ix-dixi)+filiat
      enddo
   endif
enddo
a=b
end subroutine dibeta1t
!==================================================================== [dibetat]
subroutine dibeta2t(kx,lx,mx,nx, ky,ly,my,ny, &
     nfil, dixs,diys,hss, a,  ff,ix,iy)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                       intent(in   ):: kx,lx,mx,nx,&
                                                    ky,ly,my,ny,&
                                                    nfil
integer(fpi),dimension(lx:mx,ly:my),intent(in   ):: dixs,diys
real(dp),    dimension(lx:mx,ly:my),intent(in   ):: hss
real(dp),    dimension(kx:nx,ky:ny),intent(inout):: a
logical,                            intent(  out):: ff
integer(spi),                       intent(  out):: ix,iy
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)     :: fil
real(dp),dimension(kx:nx,ky:ny):: b
real(dp)                       :: filiat,at
integer(spi)                   :: h,i,dix,diy,dixi,diyi
!==============================================================================
b=u0
do iy=ly,my; do ix=lx,mx
   at=a(ix,iy)
   dix=dixs(ix,iy); diy=diys(ix,iy)
   if(abs(dix)+abs(diy)==0)then;b(ix,iy)=b(ix,iy)+at
   else
      call blinfil(nfil,hss(ix,iy),h,fil,ff); if(ff)return
      b(ix,iy)=b(ix,iy)+fil(0)*at
      do i=1,h
         filiat=fil(i)*at; dixi=dix*i; diyi=diy*i
         b(ix+dixi,iy+diyi)=b(ix+dixi,iy+diyi)+filiat
         b(ix-dixi,iy-diyi)=b(ix-dixi,iy-diyi)+filiat
      enddo
   endif
enddo; enddo
a=b
end subroutine dibeta2t
!==================================================================== [dibetat]
subroutine dibeta3t(kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, &
     nfil, dixs,diys,dizs,hss, a,  ff,ix,iy,iz)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                             intent(in   ):: kx,lx,mx,nx,&
                                                          ky,ly,my,ny,&
                                                          kz,lz,mz,nz,&
                                                          nfil
integer(fpi),dimension(lx:mx,ly:my,lz:mz),intent(in   ):: dixs,diys,dizs
real(dp),    dimension(lx:mx,ly:my,lz:mz),intent(in   ):: hss
real(dp),    dimension(kx:nx,ky:ny,kz:nz),intent(inout):: a
logical,                                  intent(  out):: ff
integer(spi),                             intent(  out):: ix,iy,iz
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)           :: fil
real(dp),dimension(kx:nx,ky:ny,kz:nz):: b
real(dp)                             :: filiat,at
integer(spi)                         :: h,i,        &
                                        dix,diy,diz,&
                                        dixi,diyi,dizi
!==============================================================================
b=u0
do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   at=a(ix,iy,iz)
   dix=dixs(ix,iy,iz); diy=diys(ix,iy,iz); diz=dizs(ix,iy,iz)
   if(abs(dix)+abs(diy)+abs(diz)==0)then;b(ix,iy,iz)=b(ix,iy,iz)+at
   else
      call blinfil(nfil,hss(ix,iy,iz),h,fil,ff); if(ff)return
      b(ix,iy,iz)=b(ix,iy,iz)+fil(0)*at
      do i=1,h
         filiat=fil(i)*at; dixi=dix*i; diyi=diy*i; dizi=diz*i
         b(ix+dixi,iy+diyi,iz+dizi)=b(ix+dixi,iy+diyi,iz+dizi)+filiat
         b(ix-dixi,iy-diyi,iz-dizi)=b(ix-dixi,iy-diyi,iz-dizi)+filiat
      enddo
   endif
enddo; enddo; enddo
a=b
end subroutine dibeta3t

!==================================================================== [dibetat]
subroutine dibeta4t(kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, kw,lw,mw,nw, &
     nfil,dixs,diys,dizs,diws,hss, a,ff,ix,iy,iz,iw)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                                   intent(in   ):: kx,lx,mx,nx,&
                                                                ky,ly,my,ny,&
                                                                kz,lz,mz,nz,&
                                                                kw,lw,mw,nw,&
                                                                nfil
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw),intent(in   ):: dixs,diys,&
                                                                dizs,diws
real(dp),    dimension(lx:mx,ly:my,lz:mz,lw:mw),intent(in   ):: hss
real(dp),    dimension(kx:nx,ky:ny,kz:nz,kw:nw),intent(inout):: a
logical,                                        intent(  out):: ff
integer(spi),                                   intent(  out):: ix,iy,iz,iw
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)                 :: fil
real(dp),dimension(kx:nx,ky:ny,kz:nz,kw:nw):: b
real(dp)                                   :: filiat,at
integer(spi)                               :: h,i,                 &
                                              dix,diy,diz,diw,     &
                                              dixi,diyi,dizi,diwi
!==============================================================================
b=u0
do iw=lw,mw; do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   at=a(ix,iy,iz,iw)
   dix=dixs(ix,iy,iz,iw); diy=diys(ix,iy,iz,iw)
   diz=dizs(ix,iy,iz,iw); diw=diws(ix,iy,iz,iw)
   if(abs(dix)+abs(diy)+abs(diz)+abs(diw)==0)then
      b(ix,iy,iz,iw)=b(ix,iy,iz,iw)+at
   else
      call blinfil(nfil,hss(ix,iy,iz,iw),h,fil,ff); if(ff)return
      b(ix,iy,iz,iw)=b(ix,iy,iz,iw)+fil(0)*at
      do i=1,h
         filiat=fil(i)*at; dixi=dix*i; diyi=diy*i; dizi=diz*i; diwi=diw*i
         b(ix+dixi,iy+diyi,iz+dizi,iw+diwi)= &
              b(ix+dixi,iy+diyi,iz+dizi,iw+diwi)+filiat
         b(ix-dixi,iy-diyi,iz-dizi,iw-diwi)= &
              b(ix-dixi,iy-diyi,iz-dizi,iw-diwi)+filiat
      enddo
   endif
enddo; enddo; enddo; enddo
a=b
end subroutine dibeta4t

!==================================================================== [dibetat]
subroutine dibetax3t(kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, icol,nfil,&
     qcols,dixs,diys,dizs, jcol,hss,a, ff,ix,iy,iz)
!=============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                                 intent(in   ):: kx,lx,mx,nx, &
                                                              ky,ly,my,ny, &
                                                              kz,lz,mz,nz, &
                                                              icol,nfil
integer(fpi),dimension(0:7,lx:mx,ly:my,lz:mz),intent(in   ):: qcols
integer(fpi),dimension(lx:mx,ly:my,lz:mz,6),  intent(in   ):: dixs,diys,dizs
integer(fpi),dimension(lx:mx,ly:my,lz:mz),    intent(inout):: jcol
real(dp),dimension(6,lx:mx,ly:my,lz:mz),      intent(in   ):: hss
real(dp),dimension(kx:nx,ky:ny,kz:nz),        intent(inout):: a
logical,                                      intent(  out):: ff
integer(spi),                                 intent(  out):: ix,iy,iz
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)           :: fil
real(dp),dimension(kx:nx,ky:ny,kz:nz):: b
real(dp)                             :: filiat,hs,at
integer(spi)                         :: h,i,         &
                                        dix,diy,diz, &
                                        dixi,diyi,dizi
integer(fpi)                         :: j
!==============================================================================
b=u0
if(icol==7)jcol=6
do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   at=a(ix,iy,iz)
   j=jcol(ix,iy,iz)
   if(icol/=qcols(j,ix,iy,iz))then
      b(ix,iy,iz)=b(ix,iy,iz)+at
      cycle
   else
      jcol(ix,iy,iz)=j-1_fpi
      dix=dixs(ix,iy,iz,j); diy=diys(ix,iy,iz,j); diz=dizs(ix,iy,iz,j)
      hs=hss(j,ix,iy,iz)
      call blinfil(nfil,hs,h,fil,ff); if(ff)return
      b(ix,iy,iz)=b(ix,iy,iz)+fil(0)*at
      do i=1,h
         filiat=fil(i)*at; dixi=dix*i; diyi=diy*i; dizi=diz*i
         b(ix+dixi,iy+diyi,iz+dizi)=b(ix+dixi,iy+diyi,iz+dizi)+filiat
         b(ix-dixi,iy-diyi,iz-dizi)=b(ix-dixi,iy-diyi,iz-dizi)+filiat
      enddo
   endif
enddo;       enddo;       enddo
a=b
end subroutine dibetax3t

!==================================================================== [dibetat]
subroutine dibetax4t(kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, kw,lw,mw,nw, &
     icol,nfil,&
     qcols,dixs,diys,dizs,diws, jcol,hss,a, ff,ix,iy,iz,iw)
!=============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                                       intent(in   ):: kx,lx,mx,nx, &
                                                                    ky,ly,my,ny, &
                                                                    kz,lz,mz,nz, &
                                                                    kw,lw,mw,nw, &
                                                                    icol,nfil
integer(fpi),dimension(0:7,lx:mx,ly:my,lz:mz,lw:mw),intent(in   ):: qcols
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw,6),  intent(in   ):: dixs,diys,&
                                                                    dizs,diws
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw),    intent(inout):: jcol
real(dp),dimension(6,lx:mx,ly:my,lz:mz,lw:mw),      intent(in   ):: hss
real(dp),dimension(kx:nx,ky:ny,kz:nz,kw:nw),        intent(inout):: a
logical,                                            intent(  out):: ff
integer(spi),                                       intent(  out):: ix,iy,iz,iw
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)                 :: fil
real(dp),dimension(kx:nx,ky:ny,kz:nz,kw:nw):: b
real(dp)                                   :: filiat,hs,at
integer(spi)                               :: h,i,             &
                                              dix,diy,diz,diw, &
                                              dixi,diyi,dizi,diwi
integer(fpi)                               :: j
!==============================================================================
b=u0
if(icol==15)jcol=10
do iw=lw,mw; do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   at=a(ix,iy,iz,iw)
   j=jcol(ix,iy,iz,iw)
   if(icol/=qcols(j,ix,iy,iz,iw))then
      b(ix,iy,iz,iw)=b(ix,iy,iz,iw)+at
      cycle
   else
      jcol(ix,iy,iz,iw)=j-1_fpi
      dix=dixs(ix,iy,iz,iw,j); diy=diys(ix,iy,iz,iw,j)
      diz=dizs(ix,iy,iz,iw,j); diw=diws(ix,iy,iz,iw,j)
      hs=hss(j,ix,iy,iz,iw)
      call blinfil(nfil,hs,h,fil,ff); if(ff)return
      b(ix,iy,iz,iw)=b(ix,iy,iz,iw)+fil(0)*at
      do i=1,h
         filiat=fil(i)*at; dixi=dix*i; diyi=diy*i; dizi=diz*i; diwi=diw*i
         b(ix+dixi,iy+diyi,iz+dizi,iw+diwi)= &
              b(ix+dixi,iy+diyi,iz+dizi,iw+diwi)+filiat
         b(ix-dixi,iy-diyi,iz-dizi,iw-diwi)= &
              b(ix-dixi,iy-diyi,iz-dizi,iw-diwi)+filiat
      enddo
   endif
enddo;       enddo;       enddo;    enddo
a=b
end subroutine dibetax4t

!==================================================================== [dibetat]
subroutine vdibeta1t(nv,kx,lx,mx,nx, nfil, dixs,hss, a,  ff,ix)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                 intent(in   ):: nv,kx,lx,mx,nx,nfil
integer(fpi),dimension(lx:mx),intent(in   ):: dixs
real(dp),    dimension(lx:mx),intent(in   ):: hss
real(dp), dimension(nv,kx:nx),intent(inout):: a
logical,                      intent(  out):: ff
integer(spi),                 intent(  out):: ix
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)  :: fil
real(dp),dimension(nv,kx:nx):: b
real(dp),dimension(nv)      :: filiat,at
integer(spi)                :: h,i,dix,dixi
!==============================================================================
b=u0
do ix=lx,mx
   at=a(:,ix)
   dix=dixs(ix)
   if(dix==0)then;b(:,ix)=b(:,ix)+at
   else
      call blinfil(nfil,hss(ix),h,fil,ff); if(ff)return
      b(:,ix)=b(:,ix)+fil(0)*at
      do i=1,h
         filiat=fil(i)*at; dixi=dix*i
         b(:,ix+dixi)=b(:,ix+dixi)+filiat
         b(:,ix-dixi)=b(:,ix-dixi)+filiat
      enddo
   endif
enddo
a=b
end subroutine vdibeta1t
!==================================================================== [dibetat]
subroutine vdibeta2t(nv, kx,lx,mx,nx, ky,ly,my,ny, &
     nfil, dixs,diys,hss, a,  ff,ix,iy)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                       intent(in   ):: nv,&
                                                    kx,lx,mx,nx,&
                                                    ky,ly,my,ny,&
                                                    nfil
integer(fpi),dimension(lx:mx,ly:my),intent(in   ):: dixs,diys
real(dp),    dimension(lx:mx,ly:my),intent(in   ):: hss
real(dp), dimension(nv,kx:nx,ky:ny),intent(inout):: a
logical,                            intent(  out):: ff
integer(spi),                       intent(  out):: ix,iy
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)        :: fil
real(dp),dimension(nv,kx:nx,ky:ny):: b
real(dp),dimension(nv)            :: filiat,at
integer(spi)                      :: h,i,dix,diy,dixi,diyi
!==============================================================================
b=u0
do iy=ly,my; do ix=lx,mx
   at=a(:,ix,iy)
   dix=dixs(ix,iy); diy=diys(ix,iy)
   if(abs(dix)+abs(diy)==0)then;b(:,ix,iy)=b(:,ix,iy)+at
   else
      call blinfil(nfil,hss(ix,iy),h,fil,ff); if(ff)return
      b(:,ix,iy)=b(:,ix,iy)+fil(0)*at
      do i=1,h
         filiat=fil(i)*at; dixi=dix*i; diyi=diy*i
         b(:,ix+dixi,iy+diyi)=b(:,ix+dixi,iy+diyi)+filiat
         b(:,ix-dixi,iy-diyi)=b(:,ix-dixi,iy-diyi)+filiat
      enddo
   endif
enddo; enddo
a=b
end subroutine vdibeta2t
!==================================================================== [dibetat]
subroutine vdibeta3t(nv, kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, &
     nfil, dixs,diys,dizs,hss, a,  ff,ix,iy,iz)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                             intent(in   ):: nv,         &
                                                          kx,lx,mx,nx,&
                                                          ky,ly,my,ny,&
                                                          kz,lz,mz,nz,&
                                                          nfil
integer(fpi),dimension(lx:mx,ly:my,lz:mz),intent(in   ):: dixs,diys,dizs
real(dp),    dimension(lx:mx,ly:my,lz:mz),intent(in   ):: hss
real(dp), dimension(nv,kx:nx,ky:ny,kz:nz),intent(inout):: a
logical,                                  intent(  out):: ff
integer(spi),                             intent(  out):: ix,iy,iz
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)              :: fil
real(dp),dimension(nv,kx:nx,ky:ny,kz:nz):: b
real(dp),dimension(nv)                  :: filiat,at
integer(spi)                            :: h,i,        &
                                           dix,diy,diz,&
                                           dixi,diyi,dizi
!==============================================================================
b=u0
do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   at=a(:,ix,iy,iz)
   dix=dixs(ix,iy,iz); diy=diys(ix,iy,iz); diz=dizs(ix,iy,iz)
   if(abs(dix)+abs(diy)+abs(diz)==0)then;b(:,ix,iy,iz)=b(:,ix,iy,iz)+at
   else
      call blinfil(nfil, hss(ix,iy,iz),h,fil,ff); if(ff)return
      b(:,ix,iy,iz)=b(:,ix,iy,iz)+fil(0)*at
      do i=1,h
         filiat=fil(i)*at; dixi=dix*i; diyi=diy*i; dizi=diz*i
         b(:,ix+dixi,iy+diyi,iz+dizi)=b(:,ix+dixi,iy+diyi,iz+dizi)+filiat
         b(:,ix-dixi,iy-diyi,iz-dizi)=b(:,ix-dixi,iy-diyi,iz-dizi)+filiat
      enddo
   endif
enddo; enddo; enddo
a=b
end subroutine vdibeta3t
!==================================================================== [dibetat]
subroutine vdibeta4t(nv, kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, kw,lw,mw,nw, &
     nfil, dixs,diys,dizs,diws,hss, a,ff,ix,iy,iz,iw)
!==============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                                   intent(in   ):: nv,         &
                                                                kx,lx,mx,nx,&
                                                                ky,ly,my,ny,&
                                                                kz,lz,mz,nz,&
                                                                kw,lw,mw,nw,&
                                                                nfil
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw),intent(in   ):: dixs,diys,&
                                                                dizs,diws
real(dp),    dimension(lx:mx,ly:my,lz:mz,lw:mw),intent(in   ):: hss
real(dp), dimension(nv,kx:nx,ky:ny,kz:nz,kw:nw),intent(inout):: a
logical,                                        intent(  out):: ff
integer(spi),                                   intent(  out):: ix,iy,iz,iw
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)                    :: fil
real(dp),dimension(nv,kx:nx,ky:ny,kz:nz,kw:nw):: b
real(dp),dimension(nv)                        :: filiat,at
integer(spi)                                  :: h,i,                 &
                                                 dix,diy,diz,diw,     &
                                                 dixi,diyi,dizi,diwi
!==============================================================================
b=u0
do iw=lw,mw; do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   at=a(:,ix,iy,iz,iw)
   dix=dixs(ix,iy,iz,iw); diy=diys(ix,iy,iz,iw)
   diz=dizs(ix,iy,iz,iw); diw=diws(ix,iy,iz,iw)
   if(abs(dix)+abs(diy)+abs(diz)+abs(diw)==0)then
      b(:,ix,iy,iz,iw)=b(:,ix,iy,iz,iw)+at
   else
      call blinfil(nfil, hss(ix,iy,iz,iw),h,fil,ff); if(ff)return
      b(:,ix,iy,iz,iw)=b(:,ix,iy,iz,iw)+fil(0)*at
      do i=1,h
         filiat=fil(i)*at; dixi=dix*i; diyi=diy*i; dizi=diz*i; diwi=diw*i
         b(:,ix+dixi,iy+diyi,iz+dizi,iw+diwi)= &
              b(:,ix+dixi,iy+diyi,iz+dizi,iw+diwi)+filiat
         b(:,ix-dixi,iy-diyi,iz-dizi,iw-diwi)= &
              b(:,ix-dixi,iy-diyi,iz-dizi,iw-diwi)+filiat
      enddo
   endif
enddo; enddo; enddo; enddo
a=b
end subroutine vdibeta4t

!==================================================================== [dibetat]
subroutine vdibetax3t(nv,kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, icol,nfil,&
     qcols,dixs,diys,dizs, jcol,hss,a, ff,ix,iy,iz)
!=============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                                 intent(in   ):: nv,          &
                                                              kx,lx,mx,nx, &
                                                              ky,ly,my,ny, &
                                                              kz,lz,mz,nz, &
                                                              icol,nfil
integer(fpi),dimension(0:7,lx:mx,ly:my,lz:mz),intent(in   ):: qcols
integer(fpi),dimension(lx:mx,ly:my,lz:mz,6),  intent(in   ):: dixs,diys,dizs
integer(fpi),dimension(lx:mx,ly:my,lz:mz),    intent(inout):: jcol
real(dp),dimension(6,lx:mx,ly:my,lz:mz),      intent(in   ):: hss
real(dp),dimension(nv,kx:nx,ky:ny,kz:nz),     intent(inout):: a
logical,                                      intent(  out):: ff
integer(spi),                                 intent(  out):: ix,iy,iz
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)              :: fil
real(dp),dimension(nv,kx:nx,ky:ny,kz:nz):: b
real(dp),dimension(nv)                  :: filiat,at
real(dp)                                :: hs
integer(spi)                            :: h,i,         &
                                           dix,diy,diz, &
                                           dixi,diyi,dizi
integer(fpi)                            :: j
!==============================================================================
b=u0
if(icol==7)jcol=6
do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   at=a(:,ix,iy,iz)
   j=jcol(ix,iy,iz)
   if(icol/=qcols(j,ix,iy,iz))then
      b(:,ix,iy,iz)=b(:,ix,iy,iz)+at
      cycle
   else
      jcol(ix,iy,iz)=j-1_fpi
      dix=dixs(ix,iy,iz,j); diy=diys(ix,iy,iz,j); diz=dizs(ix,iy,iz,j)
      hs=hss(j,ix,iy,iz)
      call blinfil(nfil,hs,h,fil,ff); if(ff)return
      b(:,ix,iy,iz)=b(:,ix,iy,iz)+fil(0)*at
      do i=1,h
         filiat=fil(i)*at; dixi=dix*i; diyi=diy*i; dizi=diz*i
         b(:,ix+dixi,iy+diyi,iz+dizi)=b(:,ix+dixi,iy+diyi,iz+dizi)+filiat
         b(:,ix-dixi,iy-diyi,iz-dizi)=b(:,ix-dixi,iy-diyi,iz-dizi)+filiat
      enddo
   endif
enddo;       enddo;       enddo
a=b
end subroutine vdibetax3t

!==================================================================== [dibetat]
subroutine vdibetax4t(nv,kx,lx,mx,nx, ky,ly,my,ny, kz,lz,mz,nz, kw,lw,mw,nw, &
     icol,nfil,&
     qcols,dixs,diys,dizs,diws, jcol,hss,a, ff,ix,iy,iz,iw)
!=============================================================================
use jp_pkind, only: spi,dp; use jp_pkind2, only: fpi
use jp_pietc, only: u0
implicit none
integer(spi),                                       intent(in   ):: nv,          &
                                                                    kx,lx,mx,nx, &
                                                                    ky,ly,my,ny, &
                                                                    kz,lz,mz,nz, &
                                                                    kw,lw,mw,nw, &
                                                                    icol,nfil
integer(fpi),dimension(0:7,lx:mx,ly:my,lz:mz,lw:mw),intent(in   ):: qcols
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw,6),  intent(in   ):: dixs,diys,&
                                                                    dizs,diws
integer(fpi),dimension(lx:mx,ly:my,lz:mz,lw:mw),    intent(inout):: jcol
real(dp),dimension(6,lx:mx,ly:my,lz:mz,lw:mw),      intent(in   ):: hss
real(dp),dimension(nv,kx:nx,ky:ny,kz:nz,kw:nw),     intent(inout):: a
logical,                                            intent(  out):: ff
integer(spi),                                       intent(  out):: ix,iy,iz,iw
!------------------------------------------------------------------------------
real(dp),dimension(0:nfil)                    :: fil
real(dp),dimension(nv,kx:nx,ky:ny,kz:nz,kw:nw):: b
real(dp),dimension(nv)                        :: filiat,at
real(dp)                                      :: hs
integer(spi)                                  :: h,i,             &
                                                 dix,diy,diz,diw, &
                                                 dixi,diyi,dizi,diwi
integer(fpi)                                  :: j
!==============================================================================
b=u0
if(icol==15)jcol=10
do iw=lw,mw; do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   at=a(:,ix,iy,iz,iw)
   j=jcol(ix,iy,iz,iw)
   if(icol/=qcols(j,ix,iy,iz,iw))then
      b(:,ix,iy,iz,iw)=b(:,ix,iy,iz,iw)+at
      cycle
   else
      jcol(ix,iy,iz,iw)=j-1_fpi
      dix=dixs(ix,iy,iz,iw,j); diy=diys(ix,iy,iz,iw,j)
      diz=dizs(ix,iy,iz,iw,j); diw=diws(ix,iy,iz,iw,j)
      hs=hss(j,ix,iy,iz,iw)
      call blinfil(nfil,hs,h,fil,ff); if(ff)return
      b(:,ix,iy,iz,iw)=b(:,ix,iy,iz,iw)+fil(0)*at
      do i=1,h
         filiat=fil(i)*at; dixi=dix*i; diyi=diy*i; dizi=diz*i; diwi=diw*i
         b(:,ix+dixi,iy+diyi,iz+dizi,iw+diwi)= &
              b(:,ix+dixi,iy+diyi,iz+dizi,iw+diwi)+filiat
         b(:,ix-dixi,iy-diyi,iz-dizi,iw-diwi)= &
              b(:,ix-dixi,iy-diyi,iz-dizi,iw-diwi)+filiat
      enddo
   endif
enddo;       enddo;       enddo;    enddo
a=b
end subroutine vdibetax4t

end module jp_pbfil3

!#
