      SUBROUTINE MPI_LAST
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    MPI_LAST    SHUTS DOWN THE IO SERVER
!   PRGRMMR: TUCCILLO        ORG: IBM
!
! ABSTRACT:
!     SHUTS DOWN IO SERVER
!   .
!
! PROGRAM HISTORY LOG:
!   00-01-06  TUCCILLO - ORIGINAL
!
! USAGE:    CALL COLLECT(A)
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST:
!
!   OUTPUT FILES:
!     STDOUT  - RUN TIME STANDARD OUT.
!
!   SUBPROGRAMS CALLED:
!       MPI_FINALIZE
!     UTILITIES:
!       NONE
!     LIBRARY:
!       NONE
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM RS/6000 SP
!$$$
      use ctlblk_mod, only: me, num_servers, mpi_comm_inter
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      implicit none
!
      include "mpif.h"
      LOGICAL DONE
      integer ierr
      DATA DONE / .TRUE. /
!
      IF ( ME .EQ. 0 ) THEN
         IF ( NUM_SERVERS .GT. 0 ) THEN
            CALL MPI_SEND(DONE,1,MPI_LOGICAL,0,1,MPI_COMM_INTER,IERR)
         END IF
      END IF
      END
