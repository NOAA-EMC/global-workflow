module mpi_interface

  !=======================================================================

  use kinds

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define necessary include files

  include "mpif.h"

  !-----------------------------------------------------------------------

  ! Define global variables

  character                                              :: mpi_nodename(mpi_max_processor_name)
  character                                              :: mpi_noderequest
  logical                                                :: abort_mpi
  integer(kind=4),           dimension(:),   allocatable :: mpi_ranks
  integer(kind=4)                                        :: mpi_errorstatus(mpi_status_size)
  integer(kind=4)                                        :: mpi_masternode
  integer(kind=4)                                        :: mpi_slavenode
  integer(kind=4)                                        :: mpi_ierror
  integer(kind=4)                                        :: mpi_ierrorcode
  integer(kind=4)                                        :: mpi_procid
  integer(kind=4)                                        :: mpi_nprocs
  integer(kind=4)                                        :: mpi_node_source
  integer(kind=4)                                        :: mpi_node_destination
  integer(kind=4)                                        :: mpi_loopcount
  integer(kind=4)                                        :: mpi_request
  integer(kind=4)                                        :: mpi_group_user
  integer(kind=4)                                        :: mpi_group_nprocs
  integer(kind=4)                                        :: mpi_group_procid
  integer(kind=4)                                        :: mpi_group_begin
  integer(kind=4)                                        :: mpi_group_end

  !-----------------------------------------------------------------------

contains

  !=======================================================================
  
  ! mpi_interface_initialize.f90:

  !-----------------------------------------------------------------------

  subroutine mpi_interface_initialize()

    !=====================================================================

    ! Define local variables

    call mpi_init(mpi_ierror)
    call mpi_comm_rank(mpi_comm_world,mpi_procid,mpi_ierror)
    call mpi_comm_size(mpi_comm_world,mpi_nprocs,mpi_ierror)
    mpi_masternode = 0
    abort_mpi      = .false.

    !=====================================================================

  end subroutine mpi_interface_initialize

  !=======================================================================

  ! mpi_interface_terminate.f90:

  !-----------------------------------------------------------------------

  subroutine mpi_interface_terminate()

    !=====================================================================

    ! Define local variables

    !call mpi_abort(mpi_comm_world,ierror_code,mpi_ierror)
    call mpi_finalize(mpi_ierror)

    !=====================================================================
    
  end subroutine mpi_interface_terminate

  !=======================================================================

end module mpi_interface
