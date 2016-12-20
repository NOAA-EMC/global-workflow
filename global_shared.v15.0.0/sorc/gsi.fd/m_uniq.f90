module m_uniq

!$$$ module documentation block
!           .      .    .                                       .
! module:   m_uniq   return the subscripts of the unique elenments in an array 
!   prgmmr: eliu
!
! abstract: module to return the subscripts of the unique elements in an array 
! assimilation
!
! program history log:
!   1996-10-01  Joiner/Karki - initial coding from NASA/GMAO
!   2012-02-15  eliu         - reformat to use in GSI
!   2013-06-10  treadon      - comment out i_uniq and i_nuniq
!
! SPECIAL NOTE:  GSI devlopers on S4 and WCOSS found that compilation
!    of m_uniq.f90 generated a "catastrophic error" when using certain
!    versions of Intel fortran compilers.  This message is generated
!    with the following Intel compiler versions
!       * ifort 12.1.0 20110811
!       * ifort 12.1.4 20120410
!       * ifort 12.1.5 20120612
!    
!    The catastrophic errors originate from the eoshift lines when
!    compiled with the above compilers with -fp-model strict.   The
!    code successfully compiles with -fp-model precise.   This is a 
!    known bug (Intel issue id DPD200178252).  The problem was 
!    resolved the Intel Composer XE 2013 suite of compilers.
!    
!    The code has been successfully compiled as-is with -fp-model 
!    strict with the following Intel compilers
!       * ifort 12.0.4 20110427
!       * ifort 13.1.1 20130313
!
!    The default WCOSS compiler is 12.1.5 20120612.   Thus, for
!    the time being, comment out i_uniq and i_nuniq.
!    
! subroutines included:
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block


  use kinds,only : i_kind, r_kind

  private
  public uniq
  public nuniq

  interface uniq
    module procedure r_uniq
!!    module procedure i_uniq
  end interface

  interface nuniq
    module procedure r_nuniq
!!    module procedure i_nuniq
  end interface

  contains

  function r_uniq( array, counter, idx ) result (unique)

  use m_find
  implicit NONE

  ! input parameters: 
  ! Array: The array to be scanned.  The type and number of dimensions
  !        of the array are not important.  The array must be sorted
  !        into monotonic order unless the optional parameter Idx is
  !        supplied.
  real(r_kind), dimension(:):: array
  integer(i_kind):: counter          ! number of elements in unique
 
  ! optional input parameters: 
  integer(i_kind), dimension(:), optional        :: idx
  ! idx: This optional parameter is an array of indices into Array
  !      that order the elements into monotonic order.
  !      That is, the expression:
  !
  !         array(idx)
  !
  !      yields an array in which the elements of Array are
  !      rearranged into monotonic order.  If the array is not
  !      already in monotonic order, use the command:
  !
  !         uniq(array, sort(array))
  !
  !      The expression below finds the unique elements of an unsorted
  !      array:
  !
  !         array(uniq(array, sort(array)))
  !

  ! output parameters:
  real(r_kind),    dimension(0:counter-1)        :: unique
  ! An array of indicies into ARRAY is returned.  The expression:
  !
  !    array(uniq(array))
  !
  ! will be a copy of the sorted Array with duplicate adjacent
  ! elements removed.
  !

  real(r_kind), dimension(:), allocatable :: q
  integer(i_kind), dimension(:), allocatable :: indices

  if (present(idx)) then                   !IDX supplied?
     allocate(q(0:size(array)-1))
     q = array(idx)
     allocate(indices(0:counter-1))
     if (counter == 1) then
       indices = 1
     else
       indices = find(q .ne. cshift(q,1), counter)
     endif
     unique = idx(indices)
     deallocate(q)
     deallocate(indices)
  else
     if (count(array .ne. cshift(array, 1)) /= counter .and. &
         counter > 1) then
       print *, 'uniq: error dimensions not correct ',counter, &
         count(array .ne. cshift(array, 1))
       return
     endif
     if (counter == 1) then
       unique = 1
     else
       unique = find(array .ne. cshift(array, 1), counter)
     endif
  endif
  end function r_uniq

!!  function i_uniq( array, counter, idx ) result (unique)
!!  use m_find
!!  implicit none

!!  integer(i_kind), dimension(:)           :: array
!!  integer(i_kind), dimension(:), optional :: idx
!!  integer(i_kind)                         :: counter
!!  integer(i_kind), dimension(0:counter-1) :: unique

!!  integer(i_kind), dimension(:), allocatable :: q
!!  integer(i_kind), dimension(:), allocatable :: indices

!!  if (present(idx)) then                   !IDX supplied?
!!     allocate(q(0:size(array)-1))
!!     q = array(idx)
!!     allocate(indices(0:counter-1))
!!     if (counter == 1) then
!!       indices = 1
!!     else
!!       indices = find(q .ne. eoshift(q,1), counter)
!!     endif
!!     unique = idx(indices)
!!     deallocate(q)
!!     deallocate(indices)
!!  else
!!     if (count(array .ne. eoshift(array, 1)) /= counter &
!!       .and. counter > 1) then
!!       print *, 'uniq: error dimensions not correct ',counter, &
!!         count(array .ne. eoshift(array, 1))
!!       return
!!     endif
!!     if (counter == 1) then
!!       unique = 1
!!     else
!!       unique = find(array .ne. eoshift(array, 1), counter)
!!     endif
!!  endif
!!  end function i_uniq

  function r_nuniq( array, idx ) result (counter)

  implicit NONE

  real(r_kind),    dimension(:)           :: array
  integer(i_kind), dimension(:), optional :: idx
  integer(i_kind)                         :: counter

  real(r_kind), dimension(:), allocatable :: q

  if (present(idx)) then                   !IDX supplied?
     allocate(q(0:size(array)-1))
     q = array(idx)
     counter = count(q .ne. cshift(q,1) )
  else
     counter = count(array .ne. cshift(array,1) )
  endif
  counter = max(1,counter)
  end function r_nuniq

!!  function i_nuniq( array, idx ) result (counter)

!!  implicit NONE

!!  integer(i_kind), dimension(:)           :: array
!!  integer(i_kind), dimension(:), optional :: idx
!!  integer(i_kind)                         :: counter

!!  integer(i_kind), dimension(:), allocatable :: q


  ! Check the arguments.
!!  if (present(idx)) then                   !IDX supplied?
!!     allocate(q(0:size(array)-1))
!!     q = array(idx)
!!     counter = count(q .ne. eoshift(q,1) )
!!  else
!!     counter = count(array .ne. eoshift(array,1) )
!!  endif
!!  counter = max(1,counter)
!!  end function i_nuniq

end module m_uniq
                                                                                                  
