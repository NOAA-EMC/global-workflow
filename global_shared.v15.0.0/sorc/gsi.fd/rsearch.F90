#ifdef ibm_sp
 
subroutine rsearch(km1,z1,km2,z2,l2)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rsearch       search for a surrounding real interval
!   prgmmr: iredell          org: np23                date: 1998-05-01
!
! abstract: This subprogram searches a monotonic sequences of real numbers
!   for intervals that surround a given search set of real numbers.
!   The sequences may be monotonic in either direction; the real numbers
!   may be single or double precision.
!
!   IBM version of routine
!
! program history log:
! 1999-01-05  mark iredell
! 2004-06-16  russ treadon - update documentation
!
!   input argument list:
!     km1    integer number of points in the sequence
!     z1     real (km1) sequence values to search
!            (z1 must be monotonic in either direction)
!     km2    integer number of points to search for
!     z2     real (km2) set of values to search for
!            (z2 need not be monotonic)
!     
!   output argument list:
!     l2     integer (km2) interval locations from 0 to km1
!            (z2 will be between z1(l2) and z1(l2+1))
!
! subprograms called:
!   sbsrch essl binary search
!   dbsrch essl binary search
!
! remarks:
!   Returned values of 0 or km1 indicate that the given search value
!   is outside the range of the sequence.
!
!   Tf a search value is identical to one of the sequence values
!   then the location returned points to the identical value.
!   If the sequence is not strictly monotonic and a search value is
!   identical to more than one of the sequence values, then the
!   location returned may point to any of the identical values.
!
!   If l2(k)=0, then z2(k) is less than the start point z1(1)
!   for ascending sequences (or greater than for descending sequences).
!   If l2(k)=km1, then z2(k) is greater than or equal to the end point
!   z1(km1) for ascending sequences (or less than or equal to for
!   descending sequences).  otherwise z2(k) is between the values
!   z1(l2(k)) and z1(l2(k+1)) and may equal the former.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,r_double,i_kind,i_long
  use constants, only: one
  implicit none

  integer(i_kind),intent(in   ) :: km1,km2
  real(r_kind)   ,intent(in   ) :: z1(km1),z2(km2)
  integer(i_kind),intent(  out) :: l2(km2)

  real(r_double) oned
  integer(i_long) incx,n,incy,m,indx(km2),rc(km2),iopt
  integer(i_kind) k2
  
  oned=1._r_double
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Find the surrounding input interval for each output point.
  if(z1(1)<=z1(km1)) then
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Input coordinate is monotonically ascending.
     incx=1_i_long
     n=km2
     incy=1_i_long
     m=km1
     iopt=1_i_long

!   Use the appropriate ESSL function based on numerical precision of compiled code
     if(digits(one)<digits(oned)) then
        call sbsrch(z2,incx,n,z1,incy,m,indx,rc,iopt)
     else
        call dbsrch(z2,incx,n,z1,incy,m,indx,rc,iopt)
     endif

     do k2=1,km2
        l2(k2)=indx(k2)-rc(k2)
     enddo
  else
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Input coordinate is monotonically descending.
     incx=1_i_long
     n=km2
     incy=-1_i_long
     m=km1
     iopt=0_i_long
     if(digits(one)<digits(oned)) then
        call sbsrch(z2,incx,n,z1,incy,m,indx,rc,iopt)
     else
        call dbsrch(z2,incx,n,z1,incy,m,indx,rc,iopt)
     endif
     do k2=1,km2
        l2(k2)=km1+1-indx(k2)
     enddo
  endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
end subroutine rsearch

#else
!
! SGI version of routine
!
!-----------------------------------------------------------------------
subroutine rsearch(im,km1,ixz1,kxz1,z1,km2,ixz2,kxz2,z2,ixl2,kxl2,l2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rsearch     search for a surrounding real interval
!   prgmmr: iredell          org: np23                date: 1998-05-01
!
! abstract: This subprogram searches monotonic sequences of real numbers
!   for intervals that surround another given search set of real numbers.
!   The sequences may be monotonic in either direction; the real numbers
!   may be single or double precision; the input sequences and sets
!   and the output locations may be arbitrarily dimensioned.
!
! program history log:
!   1999-01-05  mark iredell
!   2008-04-16  safford - rm unused vars
!
!   input argument list:
!     im           integer number of sequences to search
!     km1          integer number of points in each sequence
!     ixz1         integer sequence skip number for z1
!     kxz1         integer point skip number for z1
!     z1           real (1+(im-1)*ixz1+(km1-1)*kxz1)
!                  sequence values to search
!                  (z1 must be monotonic in either direction)
!     km2          integer number of points to search for
!                  in each respective sequence
!     ixz2         integer sequence skip number for z2
!     kxz2         integer point skip number for z2
!     z2           real (1+(im-1)*ixz2+(km2-1)*kxz2)
!                  set of values to search for
!                  (z2 need not be monotonic)
!     ixl2         integer sequence skip number for l2
!     kxl2         integer point skip number for l2
!     
!   output argument list:
!     l2           real (1+(im-1)*ixl2+(km2-1)*kxl2)
!                  interval locations of the set of values
!                  (z2 will be between z1(l2) and z1(l2+1).)
!
! remarks:
!   If the array z1 is dimensioned (im,km1), then the skip numbers are
!   ixz1=1 and kxz1=im; if it is dimensioned (km1,im), then the skip
!   numbers are ixz1=km1 and kxz1=1; if it is dimensioned (im,jm,km1),
!   then the skip numbers are ixz1=1 and kxz1=im*jm; etcetera.
!   similar examples apply to the skip numbers for z2 and l2.
!
!   Returned values of 0 or km1 indicate that the given search value
!   is outside the range of the sequence.
!
!   If a search value is identical to one of the sequence values
!   then the location returned points to the identical value.
!   If the sequence is not strictly monotonic and a search value is
!   identical to more than one of the sequence values, then the
!   location returned may point to any of the identical values.
!
!   To be exact, for each i from 1 to im and for each k from 1 to km2,
!   z=z2(1+(i-1)*ixz2+(k-1)*kxz2) is the search value and
!   l=l2(1+(i-1)*ixl2+(k-1)*kxl2) is the location returned.
!   If l=0, then z is less than the start point z1(1+(i-1)*ixz1)
!   for ascending sequences (or greater than for descending sequences).
!   If l=km1, then z is greater than or equal to the end point
!   z1(1+(i-1)*ixz1+(km1-1)*kxz1) for ascending sequences
!   (or less than or equal to for descending sequences).
!   otherwise z is between the values z1(1+(i-1)*ixz1+(l-1)*kxz1) and
!   z1(1+(i-1)*ixz1+(l-0)*kxz1) and may equal the former.
!
! attributes:
!   language:  f90
!   machine:   RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none
 
  integer(i_kind),intent(in   ) :: im,km1,ixz1,kxz1,km2,ixz2,kxz2,ixl2,kxl2
  real(r_kind)   ,intent(in   ) :: z1(1+(im-1)*ixz1+(km1-1)*kxz1)
  real(r_kind)   ,intent(in   ) :: z2(1+(im-1)*ixz2+(km2-1)*kxz2)
  integer(i_kind),intent(  out) :: l2(1+(im-1)*ixl2+(km2-1)*kxl2)

  integer(i_kind) i,k2,l
  real(r_kind) z
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Find the surrounding input interval for each output point.
  do i=1,im
     if(z1(1+(i-1)*ixz1)<=z1(1+(i-1)*ixz1+(km1-1)*kxz1)) then

!       Input coordinate is monotonically ascending.
        do k2=1,km2
           z=z2(1+(i-1)*ixz2+(k2-1)*kxz2)
           l=0
           do
              if(z<z1(1+(i-1)*ixz1+l*kxz1)) exit
              l=l+1
              if(l==km1) exit
           enddo
           l2(1+(i-1)*ixl2+(k2-1)*kxl2)=l
        enddo

     else

!    Input coordinate is monotonically descending.
        do k2=1,km2
           z=z2(1+(i-1)*ixz2+(k2-1)*kxz2)
           l=0
           do
              if(z>z1(1+(i-1)*ixz1+l*kxz1)) exit
              l=l+1
              if(l==km1) exit
           enddo
           l2(1+(i-1)*ixl2+(k2-1)*kxl2)=l
        enddo
     endif

  enddo
end subroutine rsearch
#endif
