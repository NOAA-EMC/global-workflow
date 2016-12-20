module random_normal
use kinds, only: r_single,i_kind
contains
real(r_single) function rnorm() result( fn_val )

!   Generate a random normal deviate using the polar method.
!   Reference: Marsaglia,G. & Bray,T.A. 'A convenient method for generating
!              normal variables', Siam Rev., vol.6, 260-264, 1964.

implicit none

! Local variables

real(r_single)            :: u, sum
real(r_single), save      :: v, sln
logicaL, save           :: second = .false.
real(r_single), parameter :: one = 1.0_r_single, vsmall = TINY( one )

if (second) then
! If second, use the second random number generated on last call

  second = .false.
  fn_val = v*sln

else
! First call; generate a pair of random normals

  second = .true.
  do
    call random_number( u )
    call random_number( v )
    u = scale( u, 1 ) - one
    v = scale( v, 1 ) - one
    sum = u*u + v*v + vsmall         ! vsmall added to prevent LOG(zero) / zero
    IF(sum < one) exit
  end do
  sln = sqrt(- scale( log(sum), 1 ) / sum)
  fn_val = u*sln
end if

return
end function rnorm

subroutine iran(l,n,ran_int)
! generate an array of N random integers between 0 and L-1.
 real(r_single) :: rnd
 integer(i_kind) :: L,i,N
 integer(i_kind) :: ran_int(N)
 do i = 1,n
    call random_number(rnd)
    ran_int(i) = nint(float(l-1)*rnd)
 end do
end subroutine iran

subroutine set_random_seed ( iseed , myrank)
!
!*******************************************************************************
!
!! SET_RANDOM_SEED initializes the FORTRAN 90 random number generator.
!
!
!  Discussion:
!
!    If ISEED is nonzero, then that value is used to construct a seed.
!
!    If ISEED is zero, then the seed is determined by calling the date 
!    and time routine.  Thus, if the code is run at different times, 
!    different seed values will be set.
!
!  Parameters:
!
!    Input, integer ISEED, is nonzero for a user seed, or 0 if the
!    seed should be determined by this routine.
!
  implicit none
!
  integer(i_kind), intent(in), optional :: myrank
  integer(i_kind) date_time(8)
  logical, parameter :: debug = .false.
  integer(i_kind) i,j,k
  integer(i_kind) iseed
  integer(i_kind), allocatable :: seed(:)

!
!  Initialize the random seed routine.
!
  call random_seed
!
!  Request the size of a typical seed.
!  (On the DEC ALPHA, K is returned as 2.)
!
  call random_seed ( size = k )

  if ( debug ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_RANDOM_SEED:'
    write ( *, '(a,i6)' ) '  Random seed size is K = ', k
  end if
!
!  Set up space for a seed vector.
!
  allocate ( seed(k) )

  if ( iseed /= 0 ) then

    print *,'using input random seed',iseed

    seed(1:k) = iseed

  else

    print *,'using system clock to set random seed...'
!
!  Make up a "random" value based on date and time information.
!
    call date_and_time ( values = date_time )

    do i = 1, k

      seed(i) = 0

      do j = 1, 8
        if (present(myrank)) then
        seed(i) = seed(i) + ( j + i ) * date_time(j) + myrank * 100
        else
        seed(i) = seed(i) + ( j + i ) * date_time(j)
        endif
        seed(i) = ishftc ( seed(i), 4 * ( j - 1 ) )
      end do

    end do

  end if

  if  ( debug ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_RANDOM_SEED:'
    write ( *, '(a)' ) '  The random seed vector:'
    write ( *, '(a)' ) ' '

    do i = 1, k
      write ( *, '(i12)' ) seed(i)
    end do

  end if
!
!  Send this random value back to the RANDOM_SEED routine, to be
!  used as the seed of the random number generator.
!
  call random_seed ( put = seed(1:k) )

  deallocate ( seed )

  end subroutine set_random_seed
end module random_normal
