module sparsearr
!$$$  module documentation block
!            .      .    .                                       .
! module:  sparsearr
!  prgmmr: shlyaeva
!
! abstract: define sparse array (for saving the jacobian for EnKF) and basic routines
!
! program history log:
!   2016-11-29  shlyaeva - initial code
!
! subroutines included:
!   sub new
!   sub delete
!   sub writearray
!   sub readarray
!
! functions included:
!   size
!
! attributes:
!   language: f90
!   machine:
!
!$$$

use kinds, only: r_single, r_kind, i_kind
implicit none
private

public sparr, sparr2
public new, delete, size, raggedarr, init_raggedarr, destroy_raggedarr
public writearray, readarray, fullarray
public assignment(=)

! general sparse array type
! saves all non-zero elements and their indices
type sparr
   integer(i_kind) :: nnz                              ! number of non-zero elements
   real(r_kind), dimension(:), allocatable    :: val  ! values of non-zero elements
   integer(i_kind), dimension(:), allocatable :: ind  ! indices of non-zero elements
end type sparr

type raggedarr
   integer(i_kind) :: nnz
   real(r_kind), dimension(:), allocatable    :: val
end type raggedarr

! sparse array with dense subarrays type
! saves all non-zero elements and start and end indices of the dense
! subarrays
! i.e. for array  
! index 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
! value 0 0 0 1 2 3 0 0 0  0  0  4  5  6  7  0  0  0  0  0
! nind:    2
! st_ind:  4, 12
! end_ind: 6, 15
! val:     1, 2, 3, 4, 5, 6, 7 
type sparr2
   integer(i_kind) :: nnz
   integer(i_kind) :: nind                                   ! number of indices
   integer(i_kind), dimension(:), allocatable :: st_ind      ! start indices for dense subarrays
   integer(i_kind), dimension(:), allocatable :: end_ind     ! end indices for dense subarrays
   real(r_kind), dimension(:), allocatable    :: val         ! values of non-zero elements
end type sparr2

! interfaces
! constructor
interface new
  module procedure init_sparr2
  module procedure init_sparr
end interface

interface assignment(=)
  module procedure sparr2_to_sparr
  module procedure array_to_sparr
end interface

! destructor
interface delete
  module procedure cleanup_sparr2
  module procedure cleanup_sparr
end interface

! writing out sparse array to array
interface writearray
  module procedure writearray_sparr2
  module procedure writearray_r_sparr2
end interface

! reading sparse array from array
interface readarray
  module procedure readarray_sparr2
  module procedure readarray_r_sparr2
end interface

! returns size of the sparse array
interface size
  module procedure size_sparr2
  module procedure size_sparr
end interface

! returns full array from sparse array
interface fullarray
  module procedure fullarray_sparr2
end interface

contains
! private subroutines

! constructor for sparr2
subroutine init_sparr2(this, nnz, nind)
  type(sparr2), intent(inout) :: this
  integer(i_kind), intent(in) :: nnz, nind

  this%nnz  = nnz
  this%nind = nind
  if (allocated(this%st_ind))   deallocate(this%st_ind)
  if (allocated(this%end_ind))  deallocate(this%end_ind)
  if (allocated(this%val))      deallocate(this%val)

  allocate(this%st_ind(nind), this%end_ind(nind), this%val(nnz))

end subroutine init_sparr2

! constructor for sparr
subroutine init_sparr(this, nnz)
  type(sparr), intent(inout)  :: this
  integer(i_kind), intent(in) :: nnz

  this%nnz  = nnz
  if (allocated(this%ind))   deallocate(this%ind)
  if (allocated(this%val))   deallocate(this%val)

  allocate(this%ind(nnz), this%val(nnz))

end subroutine init_sparr

! constructor for raggedarr
subroutine init_raggedarr(this, nnz)
  implicit none
  type(raggedarr), intent(inout)  :: this
  integer(i_kind), intent(in) :: nnz
  this%nnz  = nnz
  if (allocated(this%val))   deallocate(this%val)
  allocate(this%val(nnz))
end subroutine init_raggedarr

! destructor for raggedarr
subroutine destroy_raggedarr(this)
  implicit none
  type(raggedarr), intent(inout)  :: this
  if (allocated(this%val))   deallocate(this%val)
end subroutine destroy_raggedarr

! copying constructor for sparr (from sparr2)
subroutine sparr2_to_sparr(this, sp2)
  type(sparr),  intent(inout) :: this
  type(sparr2), intent(in)    :: sp2

  integer(i_kind) :: inz, nnz
  integer(i_kind) :: i, j
  real(r_kind), dimension(:), allocatable    :: nzval  ! values of non-zero elements
  integer(i_kind), dimension(:), allocatable :: nzind  ! indices of non-zero elements

  allocate(nzval(sp2%nnz), nzind(sp2%nnz))
  nnz = 0
  inz = 1
  do i = 1, sp2%nind
     do j = sp2%st_ind(i), sp2%end_ind(i)
        if (sp2%val(inz) /= 0) then
           nnz = nnz + 1
           nzval(nnz) = sp2%val(inz)
           nzind(nnz) = j
        endif
        inz = inz + 1
     enddo
  enddo

  call init_sparr(this,nnz)

  this%ind = nzind(1:nnz)
  this%val = nzval(1:nnz)
  deallocate(nzval, nzind)

end subroutine sparr2_to_sparr

! copying constructor for sparr (from full array)
subroutine array_to_sparr(this, arr)
  type(sparr), intent(inout) :: this
  real(r_single), dimension(:), intent(in) :: arr

  integer(i_kind) :: i, nnz, n
  real(r_kind), dimension(:), allocatable    :: nzval  ! values of non-zero elements
  integer(i_kind), dimension(:), allocatable :: nzind  ! indices of non-zero elements

  n = size(arr)
  allocate(nzval(n), nzind(n))
  nnz = 0
  do i = 1, n
    if (arr(i) /= 0) then
       nnz = nnz + 1
       nzval(nnz) = arr(i)
       nzind(nnz) = i
    endif  
  enddo
  call init_sparr(this, nnz)
  this%ind = nzind(1:nnz)
  this%val = nzval(1:nnz)

  deallocate(nzind, nzval)

end subroutine array_to_sparr

! destructor for sparr2
subroutine cleanup_sparr2(this)
  type(sparr2), intent(inout) :: this

  if (allocated(this%st_ind))   deallocate(this%st_ind)
  if (allocated(this%end_ind))  deallocate(this%end_ind)
  if (allocated(this%val))      deallocate(this%val)
  this%nnz  = 0
  this%nind = 0
end subroutine cleanup_sparr2

! destructor for sparr
subroutine cleanup_sparr(this)
  type(sparr), intent(inout) :: this

  if (allocated(this%ind))   deallocate(this%ind)
  if (allocated(this%val))   deallocate(this%val)
  this%nnz  = 0
end subroutine cleanup_sparr


! returns "size" (2 + 2*nind + nnz) of sparr2
integer(i_kind) function size_sparr2(this)
  type(sparr2), intent(in) :: this

  size_sparr2 = 2 + this%nnz + 2*this%nind
end function size_sparr2

! returns "size" (1 + 2*nnz) of sparr
integer(i_kind) function size_sparr(this)
  type(sparr), intent(in) :: this

  size_sparr = 1 + 2*this%nnz
end function size_sparr


! writing out sparse array to array
subroutine writearray_sparr2(this, array, ierr)
  type(sparr2), intent(in)                :: this
  real(r_single), dimension(:), intent(inout) :: array
  integer(i_kind), optional, intent(out)  :: ierr
 
  integer(i_kind) :: ind

  if (present(ierr)) ierr = 0
  if (size(array) < size_sparr2(this)) then
    print *, 'Error writing sparse array to array: array size too small'
    if (present(ierr)) ierr = -1
    return
  endif

  ind = 1
  array(ind) = this%nnz
  ind = ind + 1
  array(ind) = this%nind
  ind = ind + 1
  array(ind:ind+this%nind-1) = this%st_ind
  ind = ind + this%nind
  array(ind:ind+this%nind-1) = this%end_ind
  ind = ind + this%nind
  array(ind:ind+this%nnz-1)  = this%val

end subroutine writearray_sparr2

! writing out sparse array to array
subroutine writearray_r_sparr2(this, array, ierr)
  type(sparr2), intent(in)                :: this
  real(r_kind), dimension(:), intent(inout) :: array
  integer(i_kind), optional, intent(out)  :: ierr

  integer(i_kind) :: ind

  if (present(ierr)) ierr = 0
  if (size(array) < size_sparr2(this)) then
    print *, 'Error writing sparse array to array: array size too small'
    if (present(ierr)) ierr = -1
    return
  endif

  ind = 1
  array(ind) = this%nnz
  ind = ind + 1
  array(ind) = this%nind
  ind = ind + 1
  array(ind:ind+this%nind-1) = this%st_ind
  ind = ind + this%nind
  array(ind:ind+this%nind-1) = this%end_ind
  ind = ind + this%nind
  array(ind:ind+this%nnz-1)  = this%val

end subroutine writearray_r_sparr2


! reading sparse array from array
subroutine readarray_sparr2(this, array)
  type(sparr2), intent(inout)              :: this
  real(r_single), dimension(:), intent(in) :: array

  integer(i_kind) :: ind, nnz, nind

  ind = 1
  nnz = array(ind)
  ind = ind + 1
  nind = array(ind)
  ind = ind + 1

  call init_sparr2(this, nnz, nind)

  this%st_ind = array(ind:ind+nind-1)
  ind = ind + nind
  this%end_ind = array(ind:ind+nind-1)
  ind = ind + nind
  this%val = array(ind:ind+nnz-1)

end subroutine readarray_sparr2

! reading sparse array from array
subroutine readarray_r_sparr2(this, array)
  type(sparr2), intent(inout)            :: this
  real(r_kind), dimension(:), intent(in) :: array

  integer(i_kind) :: ind, nnz, nind

  ind = 1
  nnz = array(ind)
  ind = ind + 1
  nind = array(ind)
  ind = ind + 1

  call init_sparr2(this, nnz, nind)

  this%st_ind = array(ind:ind+nind-1)
  ind = ind + nind
  this%end_ind = array(ind:ind+nind-1)
  ind = ind + nind
  this%val = array(ind:ind+nnz-1)

end subroutine readarray_r_sparr2

! returns full array from sparse array
subroutine fullarray_sparr2(this, array, ierr)
  type(sparr2), intent(in)                    :: this
  real(r_single), dimension(:), intent(inout) :: array
  integer(i_kind), optional, intent(out)  :: ierr

  integer(i_kind) :: i, j, inz

  inz = 1
  array = 0._r_single

  ! check if array is appropriate size
  if (present(ierr)) ierr = 0
  if ((size(array) < this%nnz) .or.                    &
      (size(array) < maxval(this%end_ind))) then
    print *, 'Error in saving full array from sparse array: array size too small'
    if (present(ierr)) ierr = -1
    return
  endif

  do i = 1, this%nind
     do j = this%st_ind(i), this%end_ind(i)
        array(j) = this%val(inz)
        inz = inz + 1
     enddo
  enddo

end subroutine fullarray_sparr2

end module sparsearr
