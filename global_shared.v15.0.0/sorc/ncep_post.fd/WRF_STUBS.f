integer function wrf_sizeof_integer()
  integer i
  wrf_sizeof_integer=STORAGE_SIZE(i)
end function wrf_sizeof_integer

real function wrf_sizeof_real()
  real i
  wrf_sizeof_real=STORAGE_SIZE(i)
end function wrf_sizeof_real

subroutine wrf_debug(n,s)
  integer :: n
  character*(*) :: s
  if(n<2) print '(A)',trim(s)
end subroutine wrf_debug

subroutine wrf_message(s)
  character*(*) :: s
  print '(A)',trim(s)
end subroutine wrf_message

subroutine wrf_error_fatal(s)
  use mpi
  character*(*) :: s
  integer :: i
  write(0,'(A)') 'FATAL ERROR IN WRF DURING POST'
  write(0,'(A)') 'wrf_error_fatal called with mesage:'
  write(0,'(A)') trim(s)
  call MPI_Abort(MPI_COMM_WORLD,1,i)
  stop 1
end subroutine wrf_error_fatal

