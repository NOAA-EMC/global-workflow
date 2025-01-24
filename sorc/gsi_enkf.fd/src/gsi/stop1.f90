subroutine stop2(ierror_code)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stop2                             abort/stop mpi code
!   prgmmr: yang, weiyu      org: np20                date: 1997-11-06
!
! abstract:  This routine aborts execution of an mpi code.  A passed
!            error code is written to standard out prior to stopping
!            the program.
!
! program history log:
!   1997-11-06 yang, w.
!   2004-06-21 treadon - update documentation
!
!   input argument list:
!     ierror_code - integer error code flag (see gsimain for possible values)
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  use mpimod, only: mpi_comm_world,ierror,mype
  use gsi_io, only: verbose
  implicit none

  integer(i_kind) ierror_code

  if (verbose) then
     write(6,*)'****STOP2****  ABORTING EXECUTION w/code=',ierror_code
     write(0,*)'****STOP2****  ABORTING EXECUTION w/code=',ierror_code
  elseif (mype==0) then
     write(6,*)'****STOP2****  ABORTING EXECUTION w/code=',ierror_code
     write(0,*)'****STOP2****  ABORTING EXECUTION w/code=',ierror_code
  endif
     
  call mpi_abort(mpi_comm_world,ierror_code,ierror)
  stop
  return
end subroutine stop2
