 subroutine error_handler(string, rc)

 implicit none

 character(len=*), intent(in)    :: string
 
 integer,          intent(in)    :: rc

 print*,"- FATAL ERROR: ", string
 print*,"- IOSTAT IS: ", rc
 call mpi_abort

 end subroutine error_handler

 subroutine netcdf_err( err, string )

 use netcdf

 implicit none
 integer, intent(in) :: err
 character(len=*), intent(in) :: string
 character(len=256) :: errmsg

 include "mpif.h"

 if( err.EQ.NF90_NOERR )return
 errmsg = NF90_STRERROR(err)
 print*,''
 print*,'FATAL ERROR: ', trim(string), ': ', trim(errmsg)
 print*,'STOP.'
 call mpi_abort(mpi_comm_world, 999)

 return
 end subroutine netcdf_err
