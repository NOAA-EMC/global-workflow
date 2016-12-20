subroutine grdcrd(d,nd,x,nx,flg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grdcrd
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: get grid coordinates from monotonically increasing or
!           decreasing points
!
! program history log:
!   1990-10-11  parrish
!   1998-04-07  weiyu yang
!   2004-05-17  kleist, documentation
!   2004-05-28  kleist, combine increasing/decreasing routines & fctns
!   2005-05-24  pondeca, add the special case nx=1
!
!   input argument list:
!     d      - input points
!     nd     - number of input points
!     x      - grid values
!     nx     - number of reference grid points
!     flg    - marks order of values in x 
!              (1=increasing, -1=decreasing)
!
!   output argument list:
!     d        - points converted to grid units
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: one
  implicit none

  integer(i_kind)           ,intent(in   ) :: nd,nx
  integer(i_kind)           ,intent(in   ) :: flg
  real(r_kind),dimension(nd),intent(inout) :: d
  real(r_kind),dimension(nx),intent(in   ) :: x

  integer(i_kind) id,ix,isrchf

! Treat "normal" case in which nx>1
  if(nx>1) then
     do id=1,nd
        if (flg==1) then

!          Case in which x is in increasing order
           if(d(id)<=x(1)) then
              ix=1
           else
              ix=isrchf(nx-1,x,d(id),flg)-1
           end if
           if(ix==nx) ix=ix-1

        else if (flg==(-1)) then

!          Case in which x is in decreasing order
           if(d(id)>=x(1)) then
              ix=1
           else
              ix=isrchf(nx-1,x,d(id),flg)-1
           end if
        end if
        d(id)=float(ix)+(d(id)-x(ix))/(x(ix+1)-x(ix))
     end do

! Treat special case of nx=1
  elseif (nx==1) then
     do id=1,nd
        d(id) = one
     end do
  endif

  return
end subroutine grdcrd

subroutine grdcrd1(d,x,nx,flg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grdcrd1
!   prgmmr: parrish          org: np22                date: 2013-01-26
!
! abstract: same as grdcrd but for d a single value.  This is added to avoid
!            type mismatch errors with debug compile on WCOSS. 
!
! program history log:
!   2013-01-26  parrish
!
!   input argument list:
!     d      - input point 
!     x      - grid values
!     nx     - number of reference grid points
!     flg    - marks order of values in x 
!              (1=increasing, -1=decreasing)
!
!   output argument list:
!     d        - point converted to grid units
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: one
  implicit none

  integer(i_kind)           ,intent(in   ) :: nx
  integer(i_kind)           ,intent(in   ) :: flg
  real(r_kind)              ,intent(inout) :: d
  real(r_kind),dimension(nx),intent(in   ) :: x

  integer(i_kind) ix,isrchf

! Treat "normal" case in which nx>1
  if(nx>1) then
     if (flg==1) then

!       Case in which x is in increasing order
        if(d<=x(1)) then
           ix=1
        else
           ix=isrchf(nx-1,x,d,flg)-1
        end if
        if(ix==nx) ix=ix-1

     else if (flg==(-1)) then

!       Case in which x is in decreasing order
        if(d>=x(1)) then
           ix=1
        else
           ix=isrchf(nx-1,x,d,flg)-1
        end if
     end if
     d=float(ix)+(d-x(ix))/(x(ix+1)-x(ix))

! Treat special case of nx=1
  elseif (nx==1) then
     d = one
  endif

  return
end subroutine grdcrd1


function isrchf(nx1,x,y,flg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    isrchf
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: get grid coordinates from monotonically increasing or
!           decreasing points
!
! program history log:
!   2005-03-07  treadon - add doc block
!
!   input argument list:
!     nx1    - number of input points
!     x      - grid values
!     y      - target value
!     flg    - marks order of values in x
!              (1=increasing, -1=decreasing)
!
!   output argument list:
!     isrchf  - array index of input grid value near target value
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind)            ,intent(in   ) :: nx1
  integer(i_kind)            ,intent(in   ) :: flg
  real(r_kind)               ,intent(in   ) :: y
  real(r_kind),dimension(nx1),intent(in   ) :: x

  integer(i_kind) k
  integer(i_kind):: isrchf

  if(flg==1) then
     do k=1,nx1
        if(y<=x(k)) then
           isrchf=k
           go to 100
        end if
     end do
  else
     do k=1,nx1
        if(y>=x(k)) then
           isrchf=k
           go to 100
        end if
     end do
  end if

  isrchf=nx1+1
  if(nx1<=0) isrchf=0

100 continue
  return
end function isrchf
