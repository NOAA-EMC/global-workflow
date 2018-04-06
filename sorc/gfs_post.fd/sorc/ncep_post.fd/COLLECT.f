      SUBROUTINE COLLECT (A, B) 
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    COLLECT     GATHERS FROM ALL MPI TASKS
!   PRGRMMR: TUCCILLO        ORG: IBM
!
! ABSTRACT:
!     GATHER "A" FROM ALL MPI TASKS ONTO TASK 0
!   .
!
! PROGRAM HISTORY LOG:
!   00-01-06  TUCCILLO - ORIGINAL
!
! USAGE:    CALL COLLECT(A)
!   INPUT ARGUMENT LIST:
!     A        - ARRAY BEING GATHERED
!
!   OUTPUT ARGUMENT LIST:
!     A        - GATHERED ARRAY - ONLY VALID ON TASK 0
!
!   OUTPUT FILES:
!     STDOUT  - RUN TIME STANDARD OUT.
!
!   SUBPROGRAMS CALLED:
!       MPI_GATHERV
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - CTLBLK.comm
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM RS/6000 SP
!$$$

      use ctlblk_mod, only: num_procs, jsta, icnt, idsp, mpi_comm_comp, im, jm, me
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      implicit none
!
      include 'mpif.h'
!
      real,intent(in)  :: a ( im, jm ) 
      real,intent(out) :: b ( im, jm ) 
!     integer i, j
      integer ierr
!
      if ( num_procs .le. 1 ) then
         b = a
      else
         call mpi_gatherv(a(1,jsta),icnt(me),MPI_REAL,              &
     &                    b,icnt,idsp,MPI_REAL,0,MPI_COMM_COMP,ierr)
      end if
      end               
