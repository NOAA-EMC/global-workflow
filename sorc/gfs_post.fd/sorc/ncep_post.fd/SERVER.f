      SUBROUTINE SERVER
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    SERVER      PERFORMS IO TO DISK
!   PRGRMMR: TUCCILLO        ORG: IBM
!
! ABSTRACT:
!     THIS ROUTINE RECEIVES DATA FROM TASK 0 OF MPI_COMM_INTER,
!     THE FIRST TASK PERFORMING THE POST_PROCESSING, AND WRITES
!     THE DATA TO DISK
!   .
!
! PROGRAM HISTORY LOG:
!   01-06-15  TUCCILLO - ORIGINAL
!
! USAGE:    CALL SERVER
!   INPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT FILES:
!     WRITES TO FILE FNAME
!
!   SUBPROGRAMS CALLED:
!       MPI_RECV
!       BAOPEN
!       BACIO
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - CTLBLK.comm
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM RS/6000 SP
!$$$
!
      use CTLBLK_mod, only: mpi_comm_inter
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -     
      implicit none
!
      INCLUDE 'mpif.h'
!
      LOGICAL :: DONE, NEWFILE
      INTEGER :: STATUS(MPI_STATUS_SIZE)
      INTEGER :: IERR, COUNT, LUN,IER
      CHARACTER*255 :: FNAME
      CHARACTER*1, ALLOCATABLE :: BUF(:)
!
!---------------------------------------------------------------------  
!
!     THIS CODE IS EXPECTING THE FOLLOWING MESSAGE STRUCTURE
!
!     VARIABLE     TYPE           DESCRIPTION     TAG
!=====================================================
!     DONE         LOGICAL        ARE WE DONE?    1
!     NEWFILE      LOGICAL        OPEN THE FILE?  2
!     LUN          INTEGER        FORTRAN UNIT #  3
!     FNAME        CHARACTER*255  FILE NAME       4
!     BUF          CHARACTER*1(*) BURF RECORD     5
!
!---------------------------------------------------------------------
!
      PRINT *, ' STARTING UP IO SERVER ...'
666   CONTINUE
!
!     THE FIRST MESSAGE IS A LOGICAL TO TELL US WHETHER WE ARE
!     FINISHED OR NOT
!
      CALL MPI_RECV(DONE,1,MPI_LOGICAL,          &
                    0,1,MPI_COMM_INTER,STATUS,IERR)
!
      IF ( DONE ) THEN
         PRINT *, ' SHUTTING DOWN IO SERVER ...'
         RETURN   !    RETURNING TO MAIN
      END IF
!
!     DO WE NEED TO OPEN THE FILE ?
!
      CALL MPI_RECV(NEWFILE,1,MPI_LOGICAL,       &
                    0,2,MPI_COMM_INTER,STATUS,IERR)
!
!     FORTRAN UNIT NUMBER
!
      CALL MPI_RECV(LUN,1,MPI_INTEGER,           &
                    0,3,MPI_COMM_INTER,STATUS,IERR)
!
!     FILENAME
!
      CALL MPI_RECV(FNAME,255,MPI_CHARACTER,     &
                    0,4,MPI_COMM_INTER,STATUS,IERR)
!
!     OPEN THE FILE, IF NECESSARY
!
      IF ( NEWFILE ) THEN
          CLOSE(LUN)
          CALL BAOPENWT(LUN,FNAME,IER)
          PRINT *, ' FILE ',FNAME,' OPENED AS UNIT ',LUN
      END IF
!
!     DETERMINE THE SIZE OF THE BUFR RECORD AND ALLOCATE A BUFFER FOR IT
!
      CALL MPI_PROBE(0,5,MPI_COMM_INTER,STATUS,IERR)
      CALL MPI_GET_COUNT(STATUS,MPI_CHARACTER,COUNT,IERR)
      ALLOCATE( BUF( COUNT ) )
!   
!     FINALLY, GET THE BUFR RECORD
!
      CALL MPI_RECV(BUF,COUNT,MPI_CHARACTER,      &
                    0,5,MPI_COMM_INTER,STATUS,IERR)
!
!     OUT TO DISK WE GO ...
!
      CALL WRYTE(LUN,COUNT,BUF)
      DEALLOCATE(BUF)
      GOTO 666
      END
