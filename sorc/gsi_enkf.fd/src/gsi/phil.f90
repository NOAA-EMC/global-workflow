!                                  R. J. Purser NOAA/NCEP/EMC 2006
!=============================================================================
module phil
!=============================================================================
!$$$ module documentation block
!           .      .    .                                      .
! module:   phil
!   prgmmr: purser
!
! abstract: Module procedures pertaining to Hilbert curve transformations and
!           representations.
!
! program history log:
!   2009-09-22  lueken - added module doc block
!
! subroutines included:
!   sub bsort_s
!   sub mergeab_s
!   sub hil_to_r_s
!   sub r_to_kil_s
!   sub xy_to_hil_s
!   sub hil_to_xy_s
!   sub xc_to_hil_s
!   sub hil_to_xc_s
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
!=============================================================================
use kinds, only: r_kind,i_kind
use constants, only: zero,one,two,four

implicit none

private
public bsort,mergeab,r_to_hil,hil_to_r,xy_to_hil,hil_to_xy,xc_to_hil,hil_to_xc
interface bsort;     module procedure bsort_s;     end interface
interface mergeab;   module procedure mergeab_s;   end interface
interface hil_to_r;  module procedure hil_to_r_s;  end interface
interface r_to_hil;  module procedure r_to_hil_s;  end interface
interface xy_to_hil; module procedure xy_to_hil_s; end interface
interface hil_to_xy; module procedure hil_to_xy_s; end interface
interface xc_to_hil; module procedure xc_to_hil_s; end interface
interface hil_to_xc; module procedure hil_to_xc_s; end interface

contains
 
!=============================================================================
recursive subroutine bsort_s(n1,n2,v,next,first)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bsort_s
!   prgmmr: purser
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    n1,n2
!    v
!
!   output argument list:
!    next
!    first
!
! note:
! Recursively sort strings by recursive binary "divide and conquer" until
! the divided strings are of length L or less, where L is empirically chosen
! here in the parameter statement (efficiency is found not to be strongly
! sensitive to the L). The values, v, of items in the string are real.
! An integer representation of all the linked-list pointers is used.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
!=============================================================================
implicit none

integer(i_kind),                  intent(IN   ) :: n1,n2
real(r_kind),    dimension(n1:n2),intent(IN   ) :: v
integer(i_kind), dimension(n1:n2),intent(  OUT) :: next
integer(i_kind),                  intent(  OUT) :: first
!-----------------------------------------------------------------------------
integer(i_kind),parameter  :: L=6
integer(i_kind)            :: n,na1,na2,nb1,nb2,i,j,k, &
                                    maxk,maxl,left,firsta,firstb
real(r_kind)               :: maxv
!=============================================================================
n=n2+1-n1
if(n<=L)then
!  Sort the small number of items by an order (n*n) algorithm:
   do i=n1,n2-1
      next(i)=i+1
   enddo
   next(n2)=0
   first=n1
   do i=n-1,1,-1
      k=first
      left=0
      maxv=v(k)
      maxl=left
      maxk=k
      do j=1,i
         left=k
         k=next(k)
         if(v(k)>=maxv)then
            maxv=v(k)
            maxl=left
            maxk=k
         endif
      enddo
      if(k/=maxk)then
         if(maxl==0)then
            first=next(maxk)
         else
            next(maxl)=next(maxk)
         endif
         next(maxk)=next(k)
         next(k)=maxk
      endif
   enddo
   write(11,*) k,next(k)
else
   na1=n1; na2=(n1+n2)/2; nb1=na2+1; nb2=n2
   call bsort(na1,na2,v(na1:na2),next(na1:na2),firsta)
   call bsort(nb1,nb2,v(nb1:nb2),next(nb1:nb2),firstb)
   call mergeab(na1,nb2,firsta,firstb, &
                v(na1:nb2),next(na1:nb2),     first)
   write(11,*) firsta,firstb,first
endif
end subroutine bsort_s

!=============================================================================
subroutine mergeab_s(na1,nb2,firsta,firstb, v,next,first)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mergeab_s
!   prgmmr: purser
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    na1,nb2
!    firsta,firstb
!    v
!    next
!
!   output argument list:
!    next
!    first
!
! note:
! Merge a pair (a and b) of individually pre-sorted strings of real values,
! connected as respective linked-lists, into a unified string with ALL the
! items returned in ascending order of values v.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
!=============================================================================
implicit none

integer(i_kind),                    intent(IN   ) :: na1,nb2, &
                                            firsta,firstb
real(r_kind),dimension(na1:nb2),    intent(IN   ) :: v
integer(i_kind), dimension(na1:nb2),intent(INOUT) :: next
integer(i_kind),                    intent(  OUT) :: first
!-----------------------------------------------------------------------------
integer(i_kind),parameter                        :: hugeint= 1000000
integer(i_kind)                                  :: idum,ia,ib,ic
!=============================================================================
ia=firsta
ib=firstb
if(ia==0)then
   first=firstb
   return
endif
if(ib==0)then
   first=firsta
   return
endif
if(v(ia)<v(ib))then
   first=ia
   ic=ia
   ia=next(ia)
else
   first=ib
   ic=ib
   ib=next(ib)
endif

do idum=1,hugeint
   if(ia==0)then
      next(ic)=ib
      return
   endif
   if(ib==0)then
      next(ic)=ia
      return
   endif
   if(v(ia)<v(ib))then
      next(ic)=ia
      ic=ia
      ia=next(ia)
   else
      next(ic)=ib
      ic=ib
      ib=next(ib)
   endif
enddo
stop 'in mergeab; hugeint too small'
end subroutine mergeab_s
      
!=============================================================================
subroutine hil_to_r_s(lgen,ngen,hil4,r)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hil_to_r_s
!   prgmmr: purser
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    lgen,ngen
!    hil4
!
!   output argument list:
!    r
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

integer(i_kind),                     intent(IN   ) :: lgen,ngen
integer(i_kind),dimension(lgen:ngen),intent(IN   ) :: hil4
real(r_kind),                        intent(  OUT) :: r
!-----------------------------------------------------------------------------
real(r_kind),parameter                          :: f=one/four
real(r_kind)                                    :: p
integer(i_kind)                                 :: i
!=============================================================================
r=zero; if(lgen==0)r=hil4(lgen)
p=f
do i=1,ngen
   r=r+p*hil4(i)
   p=p*f
enddo
end subroutine hil_to_r_s
!=============================================================================
subroutine r_to_hil_s(lgen,ngen,r,hil4)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    r_to_hil_s
!   prgmmr: purser
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    lgen,ngen
!    r
!
!   output argument list:
!    hil4
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

integer(i_kind),                     intent(IN   ) :: lgen,ngen
real(r_kind),                        intent(IN   ) :: r
integer(i_kind),dimension(lgen:ngen),intent(  OUT) :: hil4
!-----------------------------------------------------------------------------
real(r_kind)                                    :: t
integer(i_kind)                                 :: i
!=============================================================================
t=r
if(lgen==0)then
   hil4(0)=t
   t=4*(t-hil4(0))
endif
do i=1,ngen
   hil4(i)=t
   t=4*(t-hil4(i))
enddo
end subroutine r_to_hil_s
!=============================================================================
subroutine xy_to_hil_s(ngen,x,y,hil4)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    xy_to_hil_s
!   prgmmr: purser
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram documentation block
!
!   input argument list:
!    ngen
!    x,y
!
!   output argument list:
!    hil4
!
! note:
! Convert an (x,y)-representation of a point in the proper interior of the
! unit square to an ngen-digit base-4 representation of the parameter of
! a space-filling Hilbert curve.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
!=============================================================================
implicit none

integer(i_kind),                intent(IN   ) :: ngen
real(r_kind),                   intent(IN   ) :: x,y
integer(i_kind),dimension(ngen),intent(  OUT) :: hil4
!-----------------------------------------------------------------------------
real(r_kind)                               :: xr,yr
integer(i_kind),dimension(ngen)            :: dig4
integer(i_kind),dimension(0:3, 0:7)        :: hil4table, presortable
integer(i_kind)                            :: igen,presor
data hil4table  /0,3,1,2, 1,0,2,3, 2,1,3,0, 3,2,0,1, &
                 0,1,3,2, 1,2,0,3, 2,3,1,0, 3,0,2,1/
data presortable/4,6,0,0, 1,7,1,5, 2,2,4,6, 7,3,5,3, &
                 0,4,2,4, 5,5,3,1, 6,0,6,2, 3,1,7,7/
!=============================================================================
if(x< zero)stop 'In xy_to_hil; x< 0'
if(x> one) stop 'In xy_to_hil; x> 1'
if(y< zero)stop 'In xy_to_hil; y< 0'
if(y> one) stop 'In xy_to_hil; y> 1'
! Begin by converting the coordinates (x,y) to a single base-4 number in [0,1)
xr=x
yr=y

do igen=1,ngen
   xr=xr*2
   yr=yr*2
   dig4(igen)=0
   if(xr>=one)then
      dig4(igen)=dig4(igen)+1
      xr=xr-one
   endif
   if(yr>=one)then
      dig4(igen)=dig4(igen)+2
      yr=yr-one
   endif
enddo

! Initialized the present (coarsest scale) orientation code to 0:
presor=0

! At successive refinements, update the present orientation and refine the
! segment on the hilbert curve according to the quadrant of the present
! square occupied by the next generation of refinement's square:
do igen=1,ngen
   hil4(igen)=hil4table(dig4(igen),presor)
   presor    =presortable(dig4(igen),presor)
enddo
end subroutine xy_to_hil_s
!=============================================================================
subroutine hil_to_xy_s(ngen,hil4,x,y)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hil_to_xy_s
!   prgmmr: purser
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    ngen
!    hil4
!
!   output argument list:
!    x,y
!
! note:
! Convert a base-4 representation of a Hilbert curve parameter to an (x,y)-
! location in the unit square
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
!=============================================================================
implicit none

integer(i_kind),                intent(IN   ) :: ngen
integer(i_kind),dimension(ngen),intent(IN   ) :: hil4
real(r_kind),                   intent(  OUT) :: x,y
!-----------------------------------------------------------------------------
real(r_kind)                               :: frac
integer(i_kind),dimension(0:3, 0:7)        :: xtable,ytable,ptable
integer(i_kind)                            :: igen,presor,h
data xtable/0,0,1,1, 1,0,0,1, 1,1,0,0, 0,1,1,0, &
            0,1,1,0, 0,0,1,1, 1,0,0,1, 1,1,0,0/
data ytable/0,1,1,0, 0,0,1,1, 1,0,0,1, 1,1,0,0, &
            0,0,1,1, 1,0,0,1, 1,1,0,0, 0,1,1,0/
data ptable/4,0,0,6, 7,1,1,5, 6,2,2,4, 5,3,3,7, &
            0,4,4,2, 3,5,5,1, 2,6,6,0, 1,7,7,3/
!=============================================================================
presor=0
x=zero
y=zero
frac=one
do igen=1,ngen
   frac=frac/two
   h=hil4(igen)
   x=x+xtable(h,presor)*frac
   y=y+ytable(h,presor)*frac
   presor=ptable(h,presor)
enddo
end subroutine hil_to_xy_s
!=============================================================================
subroutine xc_to_hil_s(ngen,xc,hil4)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    xc_to_hil_s
!   prgmmr: purser
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    ngen
!    xc
!
!   output argument list:
!    hil4
!
!note:
! hil4(0) will contain the octant index, 0,..,7,  according to the rules:
!         xc(1)<0:  hil4(0) = 0,1, 4,5;  xc(1)>0: hil4(0) = 2,3, 6,7
!         xc(2)<0:  hil4(0) = 1,2, 4,7;  xc(2)>0: hil4(0) = 0,3, 5,6
!         xc(3)<0:  hil4(0) = 4,5,6,7;   xc(3)>0: hil4(0) = 0,1,2,3.
!
! Transform from earth-centered cartesians, xc, to an augmented base-4
! representation of the parameter of a surface-filling Hilbert curve. The
! "zeroth" digit of the hilbert curve number is the index [0,7] of the surface
! octant. The remaining ngen digits define the parameter of the portion of the
! Hilbert curve wedged into the denoted octant according to a mapping between
! the unit square and that spherical triangular octant.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
!=============================================================================
implicit none

integer(i_kind),                   intent(IN   ) :: ngen
real(r_kind),dimension(3),         intent(IN   ) :: xc
integer(i_kind), dimension(0:ngen),intent(  OUT) :: hil4
!-----------------------------------------------------------------------------
real(r_kind)                              :: ax,ay,s,t,x,y,z
!=============================================================================
x=xc(1)
y=xc(2)
z=xc(3)
! Choose and perform the appropriate polar stereographic projection:
if(z>=zero)then
   x=x/(one+z)
   y=y/(one+z)
   hil4(0)=0    ! Northern hemisphere
else
   x=-x/(one-z)
   y=-y/(one-z)
   hil4(0)=4 ! Southern hemisphere
endif
ax=abs(x)
ay=abs(y)

! Get tangent, t, of a longitude rotated and reflected into [0,pi/4]:
t=zero
if(ay>ax)then
   if(ay>zero)t=ax/ay
else
   if(ax>zero)t=ay/ax
endif
s=sqrt(one+t*t) ! <- the corresponding secant
! Expand the unit disk to fill the unit square:
x=x*s
y=y*s

! Subdivide into quadrants of this square = octants of the original sphere:
if(y>zero)then
   if(x>zero)then
      hil4(0)=hil4(0)+3
   else
      x=x+one
   endif
else
   if(x>zero)then
      hil4(0)=hil4(0)+2
      t=x
      x=one+y
      y=t
   else
      hil4(0)=hil4(0)+1
      t=x
      x=-y
      y=-t
   endif
endif
call xy_to_hil(ngen,x,y,hil4(1:ngen))
end subroutine xc_to_hil_s
!=============================================================================
subroutine hil_to_xc_s(ngen,hil4,xc)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hil_to_xc_s
!   prgmmr: purser
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    ngen
!    hil4
!
!   output argument list:
!    xc
!
! note:
! Convert an augmented base-4 representation of the parameter of a
! space-filling Hilbert curve to its corresponding earth-centered cartesian
! respresentation on the unit sphere. The "zeroth" digit of the Hilbert curve
! parameter string, hil4, is the base-8 index of the surface octant, the
! remaining ngen digits are the base-4 parameter representations of the
! portion of the Hilbert curve wedged into this octant via a transformation
! mapping the unit square to the spherical triangle.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
!=============================================================================
implicit none

integer(i_kind),                   intent(IN   ) :: ngen
integer(i_kind), dimension(0:ngen),intent(IN   ) :: hil4
real(r_kind),dimension(3),         intent(  OUT) :: xc
!-----------------------------------------------------------------------------
real(r_kind)                                  :: ax,ay,s,t,x,y,z,rr
integer(i_kind)                               :: quad
!=============================================================================
call hil_to_xy(ngen,hil4(1:ngen),x,y)
quad=mod(hil4(0),4)
select case(quad)
   case(0)
      x=x-one
   case(1)
      t=x
      x=-y
      y=-t
   case(2)
      t=x
      x=y
      y=t-one
   case(3)
end select
ax=abs(x)
ay=abs(y)

! Get tangent, t, of a longitude rotated and reflected into [0,pi/4]:
t=zero
if(ay>ax)then
   if(ay>zero)t=ax/ay
else
   if(ax>zero)t=ay/ax
endif
s=sqrt(one+t*t)! <- the corresponding secant
! Shrink the unit square to fit the unit disk:
x=x/s
y=y/s

rr=x*x+y*y
z=(one-rr)/(one+rr)
xc(1)=x*(one+z)
xc(2)=y*(one+z)
xc(3)=z
if(hil4(0)>=4)xc=-xc
end subroutine hil_to_xc_s
!=============================================================================

end module phil
  
