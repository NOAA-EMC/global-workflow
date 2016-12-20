module m_find

!$$$ module documentation block
!           .      .    .                                       .
! module:   m_find   similar to IDL's "where" function, but needs to know the
!                    dimension of the outupt e.g., index=find(mask,count)  
!   prgmmr: eliu
!
! abstract:  module similar to IDL's "where" function 
!
! program history log:
!   1997-08-13  Joiner  - initial coding from NASA/GMAO
!   2012-02-15  eliu    - reformat to use in GSI
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

  interface find
    module procedure find2
    module procedure find1
  end interface

  contains

  function find2(mask,count) result (index)
  implicit none

  !input parameters:
  logical, dimension(:),intent(in) :: mask  ! mask vector
  integer(i_kind),      intent(in) :: count ! size of output vector
  !output parameters:
  integer(i_kind), dimension(count):: index ! vector of indices where mask is true         

  !local variables
  integer(i_kind):: i, j

  j = 1
  index=-1
  if (count > 0) then
     do i=1, size(mask,1)
        if (mask(i)) then
           index(j) = i
           j = j + 1
         endif
     enddo
     j = j - 1
  endif
  if (j /= count .or. count <= 0) then
     print *, 'ERROR!!! in find (dimensions not correct)', j, count
  endif

  end function find2
 
  subroutine findp(mask,counts,index)
  implicit none
  logical, dimension(:), intent(in)      :: mask   ! mask vector
  integer(i_kind),       intent(out)     :: counts ! size of output vector
  integer(i_kind), dimension(:), pointer :: index  ! vector of indices where mask is true                                        

  !local variables
  integer(i_kind) :: i
  integer(i_kind) :: j

  j = 1
  counts = count(mask)
  if (counts > 0) then
     allocate(index(counts))
     do i=1, size(mask,1)
        if (mask(i)) then
           index(j) = i
           j = j + 1
        endif
     enddo
  else
     allocate(index(1))
     index=-1
  endif
  j = j - 1
  if (j /= counts ) then
     print *, 'ERROR!!! in find (dimensions not correct)', j, counts
  endif

  end subroutine findp

  function find1(mask ) result (index)
  implicit none

  logical, dimension(:), intent(in)          :: mask
  !integer(i_kind),      intent(in), optional:: iprt

  integer(i_kind):: n, index    
  integer(i_kind), dimension(1):: temp
  integer(i_kind), dimension(:), allocatable:: temp2

  n = count(mask)
  if (n == 1) then
     temp(1:1)=find2(mask,1)
     index=temp(1)
  else
     !if (present(iprt)) then
     ! if (iprt >= 3) then
     !   print *,'find: WARNING, found more than 1 value'
     !   print *,'taking the first'
     ! endif
     !endif
     if (n > 1) then
        allocate(temp2(n))
        temp2 = find2(mask,n)
        index=temp2(1)
        deallocate(temp2)
     else
        index=-1
     endif
  endif
  end function find1

end module m_find
