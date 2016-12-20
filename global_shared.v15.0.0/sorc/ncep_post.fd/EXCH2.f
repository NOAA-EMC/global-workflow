!!@PROCESS NOCHECK
!
!--- The 1st line is an inlined compiler directive that turns off -qcheck
!    during compilation, even if it's specified as a compiler option in the
!    makefile (Tuccillo, personal communication;  Ferrier, Feb '02).
!
      SUBROUTINE EXCH2(A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    EXCH        EXCHANGE ONE HALO ROW
!   PRGRMMR: TUCCILLO        ORG: IBM
!
! ABSTRACT:
!     EXCHANGE ONE HALO ROW
!   .
!
! PROGRAM HISTORY LOG:
!   00-01-06  TUCCILLO - ORIGINAL
!
! USAGE:    CALL EXCH(A)
!   INPUT ARGUMENT LIST:
!      A - ARRAY TO HAVE HALOS EXCHANGED
!
!   OUTPUT ARGUMENT LIST:
!      A - ARRAY WITH HALOS EXCHANGED
!
!   OUTPUT FILES:
!     STDOUT  - RUN TIME STANDARD OUT.
!
!   SUBPROGRAMS CALLED:
!       MPI_SENDRECV
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON - CTLBLK.comm
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM RS/6000 SP
!$$$
      use ctlblk_mod, only: num_procs, jend, iup, jsta, idn, mpi_comm_comp, im,&
              jsta_2l, jend_2u
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      include 'mpif.h'
!
      real,intent(inout) ::  a ( im,jsta_2l:jend_2u )
      integer status(MPI_STATUS_SIZE)
      integer ierr
!
      if ( num_procs .le. 1 ) return
!
      call mpi_sendrecv(a(1,jend-1),2*im,MPI_REAL,iup,1,            &
     &                  a(1,jsta-2),2*im,MPI_REAL,idn,1,            &
     &                  MPI_COMM_COMP,status,ierr)
      if ( ierr .ne. 0 ) then
         print *, ' problem with first sendrecv in exch2, ierr = ',ierr
         stop
      end if
      call mpi_sendrecv(a(1,jsta),2*im,MPI_REAL,idn,1,              &
     &                  a(1,jend+1),2*im,MPI_REAL,iup,1,            &
     &                  MPI_COMM_COMP,status,ierr)
      if ( ierr .ne. 0 ) then
         print *, ' problem with second sendrecv in exch2, ierr = ',ierr
         stop
      end if
!
      end

