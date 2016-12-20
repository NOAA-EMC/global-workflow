module Quicksort
! Quick sort routine from:
! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's
! Guide to
! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
! Modified by Alan Miller to include an associated integer array which
! gives
save
private
public quick_sort

interface quick_sort !
   module procedure quick_sort_real, quick_sort_int
end interface quick_sort

contains

subroutine quick_sort_int(list, n)

IMPLICIT NONE
integer, DIMENSION (:), INTENT(INOUT)  :: list
INTEGER, intent(in), optional :: n

! local float array
real, allocatable :: rlist(:)
integer :: len

if(present(n)) then
   len = n
else
   len = size(list)
endif

allocate(rlist(len))
rlist = list
call quick_sort_real(rlist, len)
list = int(rlist)
deallocate(rlist)

end subroutine quick_sort_int

RECURSIVE SUBROUTINE quick_sort_real(list, n)

IMPLICIT NONE
REAL, DIMENSION (:), INTENT(INOUT)  :: list
INTEGER, intent(in), optional :: n 

if(present(n)) then
  CALL quick_sort_1(1, n)
else
  call  quick_sort_1(1, size(list))
endif

CONTAINS

RECURSIVE SUBROUTINE quick_sort_1(left_end, right_end)

INTEGER, INTENT(IN) :: left_end, right_end

!     Local variables
INTEGER             :: i, j
REAL                :: reference, temp
INTEGER, PARAMETER  :: max_simple_sort_size = 6
IF (right_end < left_end + max_simple_sort_size) THEN
  ! Use interchange sort for small lists
  CALL interchange_sort(left_end, right_end)

ELSE
  ! Use partition ("quick") sort
  reference = list((left_end + right_end)/2)
  i = left_end - 1; j = right_end + 1

  DO
    ! Scan list from left end until element >= reference is found
    DO
      i = i + 1
      IF (list(i) >= reference) EXIT
    END DO
    ! Scan list from right end until element <= reference is found
    DO
      j = j - 1
      IF (list(j) <= reference) EXIT
    END DO


    IF (i < j) THEN
      ! Swap two out-of-order elements
      temp = list(i); list(i) = list(j); list(j) = temp
    ELSE IF (i == j) THEN
      i = i + 1
      EXIT
    ELSE
      EXIT
    END IF
  END DO

  IF (left_end < j) CALL quick_sort_1(left_end, j)
  IF (i < right_end) CALL quick_sort_1(i, right_end)
END IF

END SUBROUTINE quick_sort_1


SUBROUTINE interchange_sort(left_end, right_end)

INTEGER, INTENT(IN) :: left_end, right_end

!     Local variables
INTEGER             :: i, j
REAL                :: temp

DO i = left_end, right_end - 1
  DO j = i+1, right_end
    IF (list(i) > list(j)) THEN
      temp = list(i); list(i) = list(j); list(j) = temp
    END IF
  END DO
END DO

END SUBROUTINE interchange_sort

END SUBROUTINE quick_sort_real

end module  Quicksort
