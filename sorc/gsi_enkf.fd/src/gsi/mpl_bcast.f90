subroutine mpl_bcast(root,klen,pvals)
!$$$  subprogram documentation block
!
! abstract: Simple interface for broadcast
!
! program history log:
!   2007-05-16  tremolet - initial code
!
! argument list:
!   root  - processor braodcasting data
!   klen  - length of array pvals
!   pvals - array of values to be reduced (overwritten)
!$$$
use kinds, only: r_kind,i_kind
use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe
implicit none

! Declare passed variables
integer(i_kind),intent(in   ) :: root
integer(i_kind),intent(in   ) :: klen
real(r_kind)   ,intent(inout) :: pvals(klen)

! ----------------------------------------------------------

if (npe>1.and.klen>0) then
   call mpi_bcast(pvals,klen,mpi_rtype,root,mpi_comm_world,ierror)
   if (ierror/=0) then
      write(6,*)'mpl_bcast: MPI error'
      call stop2(154)
   end if
endif

! ----------------------------------------------------------
return
end subroutine mpl_bcast
