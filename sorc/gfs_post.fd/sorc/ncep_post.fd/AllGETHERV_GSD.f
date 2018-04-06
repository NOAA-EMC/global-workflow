      SUBROUTINE AllGETHERV(GRID1)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    AllGETHERV VERT INTRP OF MODEL LVLS TO PRESSURE
!   PRGRMMR: MING HU           ORG: GSD     DATE: 2012-01-01
!
! ABSTRACT:
!   .
!
! PROGRAM HISTORY LOG:
!
      
     use ctlblk_mod, only : im,jm,num_procs,me,jsta,jend

     implicit none

     include "mpif.h"

!
    integer i,j,ij
    integer ierr

     REAL GRID1(IM,JM)
     REAL ibufrecv(IM*JM)
     REAL ibufsend(im*(jend-jsta+1))
     integer SENDCOUNT,RECVCOUNTS(num_procs),DISPLS(num_procs)
!
!     write(*,*) 'check mpi', im,jm,num_procs,me,jsta,jend
     SENDCOUNT=im*(jend-jsta+1)
     call MPI_ALLGATHER(SENDCOUNT, 1, MPI_INTEGER, RECVCOUNTS,1 , &
                MPI_INTEGER, MPI_COMM_WORLD, ierr)
     DISPLS(1)=0
     do i=2,num_procs
         DISPLS(i)=DISPLS(i-1)+RECVCOUNTS(i-1)
     enddo
!
!     write(*,*) me,'RECVCOUNTS=',RECVCOUNTS
!     write(*,*) me,'DISPLS=',DISPLS
! 
     ij=0
     ibufsend=0.0
     do j=jsta,jend
        do i=1,IM
           ij=ij+1
           ibufsend(ij)=GRID1(i,j)
        enddo
     enddo
     if(ij .ne. RECVCOUNTS(me+1)) then
        write(*,*) 'Error: send account is not equal to receive account',me,ij,RECVCOUNTS(me+1)
     endif
  
     call MPI_ALLGATHERV(ibufsend, ij, MPI_REAL, ibufrecv, RECVCOUNTS,DISPLS, &
                MPI_REAL, MPI_COMM_WORLD, ierr)

     ij=0
     do j=1,JM
        do i=1,IM
           ij=ij+1
           GRID1(i,j)=ibufrecv(ij)
        enddo
     enddo
!
!     END OF ROUTINE.
!
      RETURN
      END
