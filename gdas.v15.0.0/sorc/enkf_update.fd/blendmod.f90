module blendmod
!$$$   module documentation block
!                .      .    .                                       .
! module:    blendmod        blend function module
!   prgmmr: parrish          org: np22                date: 2012-11-26
!
! abstract: contains routines needed for polynomial blending function.  This
!            function is defined over the interval 0<=x<=1, with value 0 at
!            x=0, and value 1 at x=1. The order parameter iord defines the
!            order of continuity of the function at the endpoints.
!            For n=0, the blend function = x for 0 <= x <=1, and =1 for x>1, =0 for x<0.
!            In this case, the function is continuous, but the derivatives are discontinuous.
!            For n>0, the function and its first n derivatives are continuous at x=0 and x=1.
!            See the comments in subroutine blend for more details.  Subroutine
!            blend was originally written by Jim Purser.
!
! program history log:
!   2012-11-26  parrish
!
! subroutines included:
!  init_blend
!  blend_f
!  blend_df
!  blend

   use kinds, only: r_kind,i_kind

   implicit none

! set default to private
   private
! set subroutines to public
   public :: init_blend
   public :: blend_f
   public :: blend_df
   public :: blend
! set passed variables to public

   integer(i_kind) mm,iblend(0:40)
   real(r_kind) x0,dxx,dnorm
  
   contains

   subroutine init_blend(xbegin,xend,iord,ierror)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_blend
!   prgmmr: parrish          org: np22                date: 2012-11-26
!
! abstract: initialize private module variables so routines blend_f and blend_df generate the
!            appropriate blend function and 1st derivative for blend function order iord
!            and transition range xbegin to xend.  Note that xbegin can be greater 
!            than xend, but xbegin==xend is not allowed.
!
! program history log:
!   2012-11-26  parrish - initial documentation
!
!   input argument list:
!     xbegin - beginning of blend function interval (  blend_f(xbegin)=0 )
!     xend   - end of blend function interval ( blend_f(xend)=1 )
!     iord   - order of blend function
!     ierror - error return:  =0 for successful return, /=0 only if xbegin==xend.
!               NOTE:  xbegin > xend is allowed.
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

     use constants, only: half,one
     implicit none

     integer(i_kind),intent(in):: iord
     real(r_kind),intent(in):: xbegin,xend
     integer(i_kind),intent(inout):: ierror

     real(r_kind) x1,xmid,y,dy

!      ierror = 0 for normal return, /=0 if xend=xbegin

     if(xend==xbegin) then
        ierror=1
        return
     end if
     mm=iord
     x0=xbegin
     x1=xend
     dxx=one/(x1-x0)
     call blend(mm,iblend)
     xmid=x0+half*(x1-x0)
     dnorm=one
     call blend_df(xmid,y,dy)
     dnorm=one/dy

   end subroutine init_blend

   subroutine blend_f(x_in,y)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    blend_f
!   prgmmr: parrish          org: np22                date: 2012-11-26
!
! abstract:  for input value x_in, return blend function value y, where 0 <=y < =1.
!           Before calling blend_f, make sure that init_blend has been called
!           for the desired values of x where the blend function transitions
!           from 0 to 1.
!
! program history log:
!   2012-11-26  parrish - initial documentation
!
!   input argument list:
!     x_in   - input argument value
!
!   output argument list:
!     y      - output value  blend_f(x_in)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

     use constants, only: zero,one
     implicit none

     real(r_kind),intent(in):: x_in
     real(r_kind),intent(out):: y
     real(r_kind) x
     integer(i_kind) j

     x=(x_in-x0)*dxx
     if(x < zero) then
        y=zero
     elseif(x > one) then
        y=one
     else
        y=iblend(mm)
        do j=mm-1,0,-1
           y=x*y+iblend(j)
        end do
        y=y*x**(mm+1)
     end if

   end subroutine blend_f

   subroutine blend_df(x_in,y,dy)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    blend_df
!   prgmmr: parrish          org: np22                date: 2012-11-26
!
! abstract:  for input value x_in, return blend function in y and 1st derivative
!             of blend function in dy.
!           Before calling blend_df, make sure that init_blend has been called
!           for the desired values of x where the blend function transitions
!           from 0 to 1.
!
! program history log:
!   2012-11-26  parrish - initial documentation
!
!   input argument list:
!     x_in   - input argument value
!
!   output argument list:
!     y      - output value of blend function for input value x_in.
!     dy     - output value of derivative of blend function for input value x_in.
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

     use constants, only: zero,one
     implicit none

     real(r_kind),intent(in):: x_in
     real(r_kind),intent(out):: y,dy
     real(r_kind) x
     integer(i_kind) j

     x=(x_in-x0)*dxx
     if(x < zero) then
        y=zero ; dy=zero
     elseif(x > one) then
        y=one ; dy=zero
     else
        dy=zero
        y=iblend(mm)
        do j=mm-1,0,-1
           dy=x*dy+y
           y=x*y+iblend(j)
        end do
        dy=((mm+one)*y+x*dy)*x**mm
        y=y*x**(mm+1)
     end if
     dy=dy*dnorm

end subroutine blend_df

subroutine blend(n,iblend)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    blend
!   prgmmr: purser           org: w/nmc22     date:1998
!
! abstract: put coefficients for n+1,..,2n+1, into iblend(0),..
!           iblend(n)
!
! program history log:
!   2004-05-13  kleist  documentation
!   2008-04-23  safford - rm unused uses
!   2012-11-26  parrish - move from prewgt.f90 to this module.
!
!   input argument list:
!     n      - number of powers to blend
!
!   output argument list:
!     iblend - blended coefficients
!
! remarks: put the coefficients for powers n+1,..,2n+1, into iblend(0),
!          ..iblend(n),for the "blending polynomial" of continuity-
!          degree n in the interval [0,1].  For example, with n=1, the 
!          blending polynomial has up to 1st derivatives continuous 
!          with y(0)=0, y(1)=1, y'(0)=y'(1)=0, when y(x)=3x^2-2x^3. 
!          Hence iblend={3,-2}
! 
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
  use kinds, only: i_kind
  implicit none

! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: n
  integer(i_kind),dimension(0:n),intent(  out) :: iblend

! Declare local parameters
  integer(i_kind),parameter:: nn=12

! Declare local variables
  integer(i_kind) np,i,j,ib
  integer(i_kind),dimension(0:nn):: ipascal(0:nn)

  if(n>nn)stop
  np=n+1
  do i=0,n
    ipascal(i)=0
  enddo

  ipascal(0)=1
  do i=0,n
     do j=i,1,-1
        ipascal(j)=ipascal(j)-ipascal(j-1)
     enddo
  enddo

  ib=1
  do i=1,n
     ib=(ib*2*(2*i+1))/i
  enddo
  do j=0,n
     iblend(j)=(ib*ipascal(j))/(np+j)
  enddo

  return
end subroutine blend

end module blendmod
