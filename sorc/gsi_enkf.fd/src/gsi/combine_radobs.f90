subroutine combine_radobs(mype_sub,mype_root,&
     npe_sub,mpi_comm_sub,nele,itxmax,nread,ndata,&
     data_all,data_crit,nrec)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    combine_radobs        merge input from multiple tasks
!   prgmmr: treadon          org: np23                date: 2006-06-19
!
! abstract:  This routine combines observation from multile tasks
!            into a single data array.
!
! program history log:
!   2006-06-19  treadon
!   2008-06-21  derber - introduce new algorithm for merging data
!   2009-07-01  kokron - use full size of data_all arrays in mpi_reduce call
!                        and zero out the recv array before the call
!
!   input argument list:
!     mype_sub - mpi task id for mpi_comm_sub
!     mype_root - mpi task id for task which combines data
!     npe_sub   - number of tasks in mpi_comm_sub
!     mpi_comm_sub  - sub-communicator
!     nele     - total number of data elements
!     itxmax   - maximum number of observations
!     data_all - observation data array
!     data_crit- array containing observation "best scores"
!     nread    - task specific number of obesrvations read from data file
!
!   output argument list:
!     nread    - total number of observations read from data file (mype_root)
!     ndata    - total number of observation profiles kept for assimilation in the thinning box (mype_root)
!     data_all - merged observation data array (mype_root)
!     data_crit- merged array containing observation "best scores" (mype_root)
!     
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use mpimod, only: ierror,mpi_rtype,mpi_itype,mpi_sum,mpi_min
  implicit none

! Declare passed variables
  integer(i_kind)                    ,intent(in   ) :: mype_sub
  integer(i_kind)                    ,intent(in   ) :: mype_root
  integer(i_kind)                    ,intent(in   ) :: npe_sub,itxmax
  integer(i_kind)                    ,intent(in   ) :: nele
  integer(i_kind)                    ,intent(in   ) :: mpi_comm_sub
  integer(i_kind)                    ,intent(inout) :: nread
  integer(i_kind)                    ,intent(  out) :: ndata
  integer(i_kind),dimension(itxmax)  ,intent(in   ) :: nrec
  real(r_kind),dimension(itxmax)     ,intent(inout) :: data_crit
  real(r_kind),dimension(nele,itxmax),intent(inout) :: data_all

! Declare local variables
  integer(i_kind):: k,l,ndata1,ndata2,kk
  integer(i_kind):: ncounts,ncounts1

  real(r_kind),allocatable,dimension(:):: data_crit_min
  real(r_kind),allocatable,dimension(:,:):: data_all_in
  integer(i_kind),allocatable,dimension(:):: icrit_min,icrit,nloc

  ndata=0
  if(npe_sub > 1)then
!    Determine total number of data read and retained.
     ncounts=nread
     call mpi_allreduce(ncounts,ncounts1,1,mpi_itype,mpi_sum,mpi_comm_sub,ierror)

!    Set total number of observations summed over all tasks and
!    construct starting location of subset in reduction array

     nread=0
     if (mype_sub==mype_root) nread = ncounts1
     if (ncounts1 <= 0)return

!    Allocate arrays to hold data

     allocate(data_crit_min(itxmax))
!    gather arrays over all tasks in mpi_comm_sub.  Reduction result
!    is only needed on task mype_root
     call mpi_allreduce(data_crit,data_crit_min,itxmax,mpi_rtype,mpi_min,mpi_comm_sub,ierror)

     allocate(nloc(itxmax),icrit(itxmax))
     icrit=1e9
     ndata=0
     ndata1=0
     nloc=0
     do k=1,itxmax
        if(data_crit_min(k) < 5.e9_r_kind)then
           ndata=ndata+1
           if(data_crit_min(k) == data_crit(k)) then
              ndata1=ndata1+1
              nloc(ndata)=k
              icrit(ndata)=nrec(k)
           end if
        end if
     end do
     deallocate(data_crit_min)

     call mpi_allreduce(ndata1,ndata2,1,mpi_itype,mpi_sum,mpi_comm_sub,ierror)

!    Following code only in the circumstance that multiple min crit's in one 
!       grid box are identical on different processors
     if(ndata /= ndata2)then
        allocate(icrit_min(ndata))
        call mpi_allreduce(icrit,icrit_min,ndata,mpi_itype,mpi_min,mpi_comm_sub,ierror)

        do k=1,ndata
           if(nloc(k) /=0 .and. icrit_min(k) /= icrit(k)) nloc(k) = 0
        end do
      
        deallocate(icrit_min)
     end if
     deallocate(icrit)
     allocate(data_all_in(nele,ndata))
!$omp parallel do private(kk,k,l)
     do kk=1,ndata
        k=nloc(kk)
        if(k > 0)then
           do l=1,nele
              data_all_in(l,kk)=data_all(l,k)
           end do
        else
           do l=1,nele
             data_all_in(l,kk)=zero
           end do
        end if
        
     end do
     deallocate(nloc)

!    get all data on process mype_root
!    data_all(:,:) = zero
     call mpi_reduce(data_all_in,data_all,nele*ndata,mpi_rtype,mpi_sum,&
          mype_root,mpi_comm_sub,ierror)
     deallocate(data_all_in)
  else

     if(nread <= 0)return
     do k=1,itxmax
        if(data_crit(k) < 1.e9_r_kind)then
           ndata=ndata+1
           if( k /= ndata)then
              do l=1,nele
                 data_all(l,ndata)=data_all(l,k)
              end do
           end if
        end if
     end do
  end if

! End of routine
  return
end subroutine combine_radobs
