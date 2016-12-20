module mpisetup
!$$$  module documentation block
!
! module: mpisetup                     initialize MPI.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: Initialize and finalize MPI, create variables numproc (total # of MPI tasks,
!  same for each task) and nproc (MPI task #, different for each task). MPI
!  subroutine names and constants are imported via the 'mpif.h' include file.
!
! Public Subroutines
!   mpi_initialize: Initialize MPI.
!   mpi_cleanup: Wait for all tasks, then finalize MPI.
!   all the MPI routines (included via the mpif.h include file).
!
! Public Variables:
!   nproc:  MPI task # (root = 0).
!   numproc:  total number of MPI tasks.
!
! program history log:
!   2009-02-23  Initial version.
!
! attributes:
!   language: f95
!
!$$$

use kinds, only: r_kind, r_single, r_double
implicit none
! mpi definitions.
include 'mpif.h'
integer numproc, nproc, numproc_shm, nproc_shm
integer mpi_comm_shmem, mpi_comm_shmemroot
integer mpi_status(mpi_status_size)
integer, public :: mpi_realkind
integer, public :: mpi_comm_io

contains

subroutine mpi_initialize()
use mpimod, only : mpi_comm_world,npe,mype
integer ierr,np,nuse,new_group,old_group,nshmemroot
integer, dimension(:), allocatable :: useprocs, itasks
call mpi_init(ierr)
! nproc is process number, numproc is total number of processes.
call mpi_comm_rank(mpi_comm_world,nproc,ierr)
call mpi_comm_size(mpi_comm_world,numproc,ierr)
! set in GSI mpimod
mype = nproc; npe = numproc
if (nproc == 0) print *,'running on ',numproc,' processors ...'
if (r_kind == r_single) then
   mpi_realkind = mpi_real4
else if (r_kind == r_double) then
   mpi_realkind = mpi_real8
else
   print *,'illegal r_kind (must be single or double)'
   call mpi_cleanup()
endif

#ifdef MPI3
! all the rest below only used for LETKF...

! split into shared memory sub communicators.
CALL MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, &
                         MPI_INFO_NULL, mpi_comm_shmem, ierr)
call MPI_Comm_rank(mpi_comm_shmem, nproc_shm, ierr)
call MPI_Comm_size(mpi_comm_shmem, numproc_shm, ierr)
! create communicator involving just root tasks of each shared
! memory communicator.
allocate(itasks(numproc)); itasks=0
if (nproc_shm == 0) itasks(nproc+1) = 1
call mpi_allreduce(mpi_in_place,itasks,numproc,mpi_integer,mpi_sum,mpi_comm_world,ierr)
nshmemroot = count(itasks == 1)
allocate(useprocs(nshmemroot))
nuse = 0
do np=0,numproc-1
   if (itasks(np+1) == 1) then
     nuse = nuse + 1
     useprocs(nuse) = np
   end if
enddo
!if (nproc .eq. 0) then
!   print *,'nshmemroot',nshmemroot,nuse
!   print *,useprocs
!endif
deallocate(itasks)
call MPI_COMM_GROUP(MPI_COMM_WORLD,old_group,ierr)
call MPI_GROUP_INCL(old_group,nuse,useprocs,new_group,ierr)
deallocate(useprocs)
call MPI_COMM_CREATE(MPI_COMM_WORLD,new_group,mpi_comm_shmemroot,ierr)
!print *,'ierr from mpi_comm_create',ierr,mpi_comm_shmemroot
#endif


end subroutine mpi_initialize

subroutine mpi_initialize_io(nanals)
use mpimod, only : mpi_comm_world,npe,mype
integer ierr,np,nuse,new_group,old_group
integer, intent(in) :: nanals
integer, dimension(:), allocatable :: useprocs, itasks

! create communicator involving just tasks involved in reading
! and writing ensemble members (1st nanals tasks).
allocate(itasks(numproc)); itasks=0
itasks(1:nanals) = 1
allocate(useprocs(nanals))
nuse = 0
do np=0,numproc-1
   if (itasks(np+1) == 1) then
     nuse = nuse + 1
     useprocs(nuse) = np
   end if
enddo
!if (nproc .eq. 0) then
!   print *,'nanals',nanals,nuse
!   print *,useprocs
!endif
deallocate(itasks)
call MPI_COMM_GROUP(MPI_COMM_WORLD,old_group,ierr)
call MPI_GROUP_INCL(old_group,nuse,useprocs,new_group,ierr)
deallocate(useprocs)
call MPI_COMM_CREATE(MPI_COMM_WORLD,new_group,mpi_comm_io,ierr)
!print *,'ierr from mpi_comm_create',ierr,mpi_comm_io
end subroutine mpi_initialize_io

subroutine mpi_cleanup()
integer ierr
flush(6,err=10)
flush(0,err=10)
10 continue
call mpi_barrier(mpi_comm_world,ierr)
if (nproc == 0) write(6,*) 'all done!'
call mpi_finalize(ierr)
if (ierr /= 0) then
 print *, 'MPI_Finalize error status = ',ierr
end if
end subroutine mpi_cleanup

end module mpisetup
