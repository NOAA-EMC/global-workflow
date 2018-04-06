!!@PROCESS NOCHECK
!
!--- The 1st line is an inlined compiler directive that turns off -qcheck
!    during compilation, even if it's specified as a compiler option in the
!    makefile (Tuccillo, personal communication;  Ferrier, Feb '02).
!
      SUBROUTINE EXCH(A)
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
      real,intent(inout) :: a ( im,jsta_2l:jend_2u )
      integer status(MPI_STATUS_SIZE)
      integer ierr, jstam1, jendp1
!
!     write(0,*) 'mype=',me,'num_procs=',num_procs,'im=',im,'jsta_2l=', &
!             jsta_2l,'jend_2u=',jend_2u,'jend=',jend,'iup=',iup,'jsta=', &
!             jsta,'idn=',idn
      if ( num_procs .le. 1 ) return
!
      jstam1 = max(jsta_2l,jsta-1)                        ! Moorthi
      call mpi_sendrecv(a(1,jend),im,MPI_REAL,iup,1,             &
     &                  a(1,jstam1),im,MPI_REAL,idn,1,           &
     &                  MPI_COMM_COMP,status,ierr)
!      print *,'mype=',me,'in EXCH, after first mpi_sendrecv'
      if ( ierr .ne. 0 ) then
         print *, ' problem with first sendrecv in exch, ierr = ',ierr
         stop
      end if
      jendp1 = min(jend+1,jend_2u)                          ! Moorthi
      call mpi_sendrecv(a(1,jsta),im,MPI_REAL,idn,1,             &
     &                  a(1,jendp1),im,MPI_REAL,iup,1,           &
     &                  MPI_COMM_COMP,status,ierr)
!      print *,'mype=',me,'in EXCH, after second mpi_sendrecv'
      if ( ierr .ne. 0 ) then
         print *, ' problem with second sendrecv in exch, ierr = ',ierr
         stop
      end if
!
      end

!!@PROCESS NOCHECK
!
!--- The 1st line is an inlined compiler directive that turns off -qcheck
!    during compilation, even if it's specified as a compiler option in the
!    makefile (Tuccillo, personal communication;  Ferrier, Feb '02).
!
      subroutine exch_f(a)
 
      use ctlblk_mod, only: num_procs, jend, iup, jsta, idn,    &
     &                      mpi_comm_comp, im, jsta_2l, jend_2u
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      include 'mpif.h'
!
      real,intent(inout) :: a ( im,jsta_2l:jend_2u )
      integer status(MPI_STATUS_SIZE)
      integer ierr, jstam1, jendp1
!
      if ( num_procs .eq. 1 ) return
!
      jstam1 = max(jsta_2l,jsta-1)                       ! Moorthi
      call mpi_sendrecv(a(1,jend),im,MPI_REAL,iup,1,           &
     &                  a(1,jstam1),im,MPI_REAL,idn,1,         &
     &                  MPI_COMM_COMP,status,ierr)
      if ( ierr .ne. 0 ) then
         print *, ' problem with first sendrecv in exch, ierr = ',ierr
         stop
      end if
      jendp1=min(jend+1,jend_2u)                         ! Moorthi
      call mpi_sendrecv(a(1,jsta),im,MPI_REAL,idn,1,           &
     &                  a(1,jendp1),im,MPI_REAL,iup,1,         &
     &                  MPI_COMM_COMP,status,ierr)
      if ( ierr .ne. 0 ) then
         print *, ' problem with second sendrecv in exch, ierr = ',ierr
         stop
      end if
!
      end

