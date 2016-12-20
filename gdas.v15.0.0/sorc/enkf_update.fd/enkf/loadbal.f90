module loadbal
!$$$  module documentation block
!
! module: loadbal          decompose ob priors and horizontal grid points
!                          to minimize load imbalance. Creates various
!                          arrays that define the decomposition.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: 
!
! Public Subroutines:
!  load_balance: set up decomposition (for ob priors and analysis grid points)
!   that minimizes load imbalance.  
!   The decomposition uses "Graham's rule", which simply
!   stated, assigns each new work item to the task that currently has the 
!   smallest load.
!  loadbal_cleanup: deallocate allocated arrays.
!
! Private Subroutines:
!  estimate_work_enkf1: estimate work needed to update each analysis grid
!   point (considering all the observations within the localization radius).
!  estimate_work_enkf2: estimate work needed to update each observation prior
!   (considering all the observations within the localization radius of each
!    observation). 
! Public Variables (all defined by subroutine load_balance):
!  npts_min: (integer scalar) smallest number of grid points assigned to a task.
!  npts_max: (integer scalar) maximum number of grid points assigned to a task.
!  nobs_min: (integer scalar, serial enkf only) smallest number of observation priors assigned to a task.
!  nobs_max: (integer scalar, serial enkf only) maximum number of observation priors assigned to a task.
!  numproc: (integer scalar) total number of MPI tasks (from module mpisetup)
!  nobstot: (integer scalar) total number of obs to be assimilated (from module
!   enkf_obsmod).
!  numobsperproc(numproc): (serial enkf only) integer array containing # of ob priors 
!   assigned to each task.
!  numptsperproc(numproc): integer array containing # of grid points assigned to
!   each task.
!  indxproc(numproc,npts_max): integer array with the indices (1,2,...npts) of 
!   analysis grid points assigned to each task.
!  indxproc_obs(numproc,nobs_max): (serial enkf only) integer array with the indices
!   (1,2,...nobstot) of observation priors assigned to that task.
!  iprocob(nobstot): (serial enkf only) integer array containing the task number that has been
!   assigned to update each observation prior.
!  indxob_chunk(nobstot): (serial enkf only) integer array that maps the index of the ob priors
!   being assimilated (1,2,3...nobstot) to the index of the obs being 
!   updated on that task (1,2,3,...numobsperproc(nproc)) - inverse of
!   indxproc_obs.
!  ensmean_obchunk(nobs_max): (serial enkf only) real array of ensemble mean observation priors
!   being updated on that task (use indxproc_obs to find
!   corresponding index in ensemble_ob).
!  obloc_chunk(3,nobs_max): (serial enkf only) real array of spherical cartesian coordinates
!   of ob priors being updated on this task.
!  grdloc_chunk(3,npts_max): real array of spherical cartesian coordinates
!   of analysis grid points being updated on this task.
!  lnp_chunk(npts_max,ndim): real array of log(pressures) of state variables
!   being updated on this task.
!  oblnp_chunk(nobs_max,ndim): (serial enkf only) real array of log(pressures) of ob priors
!   being updated on this task.
!  obtime_chunk(nobs_max): (serial enkf only) real array of ob times of ob priors
!   being updated on this task (expressed as an offset from the analysis time in
!   hours).
!  anal_obchunk_prior(nanals,nobs_max): (serial enkf only) real array of observation prior 
!   ensemble perturbations to be updated on this task (not used in LETKF).
!  kdtree_grid: pointer to kd-tree structure used for nearest neighbor searches
!   for model grid points (only searches grid points assigned to this task).
!  kdtree_obs: pointer to kd-tree structure used for nearest neighbor searches
!   for observations (only searches ob locations assigned to this task).
!  kdtree_obs2: (LETKF only) pointer to kd-tree structure used for nearest neighbor searches
!   for observations (searches all observations)
!   
!
! Modules Used: mpisetup, params, kinds, constants, enkf_obsmod, gridinfo,
!               kdtree_module, covlocal
!
! program history log:
!   2009-02-23  Initial version.
!   2011-06-21  Added the option of observation box selection for LETKF.
!   2015-07-25  Remove observation box selection (use kdtree instead).
!
! attributes:
!   language: f95
!
!$$$

use mpisetup
use params, only: ndim, datapath, nanals, simple_partition, letkf_flag,&
                  corrlengthnh, corrlengthsh, corrlengthtr, lupd_obspace_serial
use enkf_obsmod, only: nobstot, obloc, oblnp, ensmean_ob, obtime, anal_ob, corrlengthsq
use kinds, only: r_kind, i_kind, r_double, r_single
use kdtree2_module, only: kdtree2, kdtree2_create, kdtree2_destroy, &
                          kdtree2_result, kdtree2_r_nearest
use gridinfo, only: gridloc, logp, latsgrd, nlevs_pres, npts
use constants, only: zero, rad2deg, deg2rad

implicit none
private
public :: load_balance, loadbal_cleanup

real(r_single),public, allocatable, dimension(:,:) :: lnp_chunk, &
                                                      anal_obchunk_prior
! arrays passed to kdtree2 routines need to be single
real(r_single),public, allocatable, dimension(:,:) :: obloc_chunk, grdloc_chunk
real(r_single),public, allocatable, dimension(:) :: oblnp_chunk, &
 obtime_chunk, ensmean_obchunk
integer(i_kind),public, allocatable, dimension(:) :: iprocob, indxob_chunk,&
                          numptsperproc, numobsperproc
integer(i_kind),public, allocatable, dimension(:,:) :: indxproc, indxproc_obs
integer(i_kind),public :: npts_min, npts_max, nobs_min, nobs_max
integer(8) totsize
! kd-tree structures.
type(kdtree2),public,pointer :: kdtree_obs, kdtree_grid, kdtree_obs2


contains

subroutine load_balance()
! set up decomposition (for analysis grid points, and ob priors in serial EnKF)
! that minimizes load imbalance. 
! Uses "Graham's rule", which simply
! stated, assigns each new work item to the task that currently has the 
! smallest load.
implicit none
integer(i_kind), allocatable, dimension(:) :: rtmp,numobs
!real(r_single), allocatable, dimension(:) :: buffer
integer(i_kind) np,i,n,nn,nob1,nob2,ierr
real(r_double) t1
logical test_loadbal

if (letkf_flag) then
   ! used for finding nearest obs to grid point in LETKF.
   ! results are sorted by distance.
   kdtree_obs2  => kdtree2_create(obloc,sort=.true.,rearrange=.true.)
endif

! partition state vector for using Grahams rule..
! ("When a new job arrives, allocate it to the server 
! that currently has the smallest load")
allocate(numobs(npts))
allocate(numptsperproc(numproc))
allocate(rtmp(numproc))
t1 = mpi_wtime()
! assume work load proportional to number of 'nearby' obs
call estimate_work_enkf1(numobs) ! fill numobs array with number of obs per horiz point
! distribute the results of estimate_work to all processors.
call mpi_allreduce(mpi_in_place,numobs,npts,mpi_integer,mpi_sum,mpi_comm_world,ierr)
if (nproc == 0) print *,'time in estimate_work_enkf1 = ',mpi_wtime()-t1,' secs'
if (nproc == 0) print *,'min/max numobs',minval(numobs),maxval(numobs)
! loop over horizontal grid points on analysis grid.
t1 = mpi_wtime()
rtmp = 0
numptsperproc = 0
np = 0
test_loadbal = .false. ! simple partition for testing
do n=1,npts
   if (test_loadbal) then
       ! use simple partition (does not use estimated workload) for testing
       np = np + 1
       if (np > numproc) np = 1
   else
       np = minloc(rtmp,dim=1)
       ! np is processor with the fewest number of obs to process
       ! add this grid point to list for nmin
       rtmp(np) = rtmp(np)+numobs(n)
   endif
   numptsperproc(np) = numptsperproc(np)+1
end do
npts_max = maxval(numptsperproc)
npts_min = minval(numptsperproc)
allocate(indxproc(numproc,npts_max))
! indxproc(np,i) is i'th horiz grid index for processor np.
! there are numptsperpoc(np) i values for processor np
rtmp = 0
numptsperproc = 0
np = 0
do n=1,npts
   if (test_loadbal) then
       ! use simple partition (does not use estimated workload) for testing
       np = np + 1
       if (np > numproc) np = 1
   else
       np = minloc(rtmp,dim=1)
       rtmp(np) = rtmp(np)+numobs(n)
   endif
   numptsperproc(np) = numptsperproc(np)+1 ! recalculate
   indxproc(np,numptsperproc(np)) = n
end do
! print estimated workload for each task
if (nproc == 0) then
   do np=1,numproc
      rtmp(np) = 0
      do n=1,numptsperproc(np)
         rtmp(np) = rtmp(np) + numobs(indxproc(np,n))
      enddo
   enddo
   print *,'min/max estimated work ',&
    minval(rtmp),maxval(rtmp)
endif
deallocate(rtmp,numobs)
if (nproc == 0) then
    print *,'npts = ',npts
    print *,'min/max number of points per proc = ',npts_min,npts_max
    print *,'time to do model space decomp = ',mpi_wtime()-t1
end if
! setup arrays to hold subsets of grid information for each task.
allocate(grdloc_chunk(3,numptsperproc(nproc+1)))
allocate(lnp_chunk(numptsperproc(nproc+1),nlevs_pres))
do i=1,numptsperproc(nproc+1)
   grdloc_chunk(:,i) = gridloc(:,indxproc(nproc+1,i))
   do nn=1,nlevs_pres
      lnp_chunk(i,nn) = logp(indxproc(nproc+1,i),nn)
   end do
end do

! for serial filter, partition obs for observation space update.
if (.not. letkf_flag .or. lupd_obspace_serial) then
   allocate(numobsperproc(numproc))
   allocate(iprocob(nobstot))
   ! default is to partition obs simply, since
   ! speed up from using Graham's rule for observation process
   ! often does not justify cost of estimating workload in ob space.
   if (simple_partition) then
     ! just distribute obs without trying to estimate workload
     t1 = mpi_wtime()
     numobsperproc = 0
     np=0
     do n=1,nobstot
        np=np+1
        if(np > numproc)np = 1
        numobsperproc(np) = numobsperproc(np)+1
        iprocob(n) = np-1
     enddo
   else
     ! use graham's rule
     allocate(numobs(nobstot))
     t1 = mpi_wtime()
     ! assume workload is proportional to number of 'nearby obs' in ob space.
     call estimate_work_enkf2(numobs) ! fill numobs array with number of obs close to each ob
     ! distribute the results of estimate_work to all processors.
     call mpi_allreduce(mpi_in_place,numobs,nobstot,mpi_integer,mpi_sum,mpi_comm_world,ierr)
     if (nproc == 0) print *,'time in estimate_work_enkf2 = ',mpi_wtime()-t1,' secs'
     t1 = mpi_wtime()
     allocate(rtmp(numproc))
     rtmp = 0
     numobsperproc = 0
     np=0
     do n=1,nobstot
        np = minloc(rtmp,dim=1)
        ! np is processor with the fewest number of close obs to process
        rtmp(np) = rtmp(np)+numobs(n)
        numobsperproc(np) = numobsperproc(np)+1
        iprocob(n) = np-1
     enddo
     deallocate(rtmp,numobs)
   end if
   nobs_min = minval(numobsperproc)
   nobs_max = maxval(numobsperproc)
   allocate(indxproc_obs(numproc,nobs_max))
   numobsperproc = 0
   do n=1,nobstot
      np=iprocob(n)+1
      numobsperproc(np) = numobsperproc(np)+1 ! recalculate
      ! indxproc_obs(np,i) is i'th ob index for processor np.
      ! there are numobsperpoc(np) i values for processor np
      indxproc_obs(np,numobsperproc(np)) = n
   end do
   if (nproc == 0) then
       print *,'nobstot = ',nobstot
       print *,'min/max number of obs per proc = ',nobs_min,nobs_max
       print *,'time to do ob space decomp = ',mpi_wtime()-t1
   end if
   ! for serial enkf, send out observation priors to be updated on each processor.
   allocate(anal_obchunk_prior(nanals,nobs_max))
   if(nproc == 0) then
      print *,'sending out observation prior ensemble perts from root ...'
      totsize = nobstot
      totsize = totsize*nanals
      print *,'nobstot*nanals',totsize
      totsize = npts
      totsize = totsize*ndim
      print *,'npts*ndim',totsize
      t1 = mpi_wtime()
      ! send one big message to each task.
      do np=1,numproc-1
         do nob1=1,numobsperproc(np+1)
            nob2 = indxproc_obs(np+1,nob1)
            anal_obchunk_prior(1:nanals,nob1) = anal_ob(1:nanals,nob2)
         end do
         call mpi_send(anal_obchunk_prior,nobs_max*nanals,mpi_real4,np, &
              1,mpi_comm_world,ierr)
      end do
      ! anal_obchunk_prior on root (no send necessary)
      do nob1=1,numobsperproc(1)
         nob2 = indxproc_obs(1,nob1)
         anal_obchunk_prior(1:nanals,nob1) = anal_ob(1:nanals,nob2)
      end do
      ! now we don't need anal_ob anymore for serial EnKF.
      if (.not. lupd_obspace_serial) deallocate(anal_ob)
   else
      ! recv one large message on each task.
      call mpi_recv(anal_obchunk_prior,nobs_max*nanals,mpi_real4,0, &
           1,mpi_comm_world,mpi_status,ierr)
   end if
   call mpi_barrier(mpi_comm_world, ierr)
   if(nproc == 0) print *,'... took ',mpi_wtime()-t1,' secs'
   ! these arrays only needed for serial filter
   ! nob1 is the index of the obs to be processed on this rank
   ! nob2 maps nob1 to 1:nobstot array (nobx)
   allocate(obloc_chunk(3,numobsperproc(nproc+1)))
   allocate(oblnp_chunk(numobsperproc(nproc+1)))
   allocate(obtime_chunk(numobsperproc(nproc+1)))
   allocate(ensmean_obchunk(numobsperproc(nproc+1)))
   allocate(indxob_chunk(nobstot))
   indxob_chunk = -1
   do nob1=1,numobsperproc(nproc+1)
      nob2 = indxproc_obs(nproc+1,nob1)
      oblnp_chunk(nob1) = oblnp(nob2)
      obtime_chunk(nob1) = obtime(nob2)
      indxob_chunk(nob2) = nob1
      ensmean_obchunk(nob1) = ensmean_ob(nob2)
      obloc_chunk(:,nob1) = obloc(:,nob2)
   enddo
   ! set up kd-trees for serial filter to search only the subset
   ! of gridpoints, obs to be updated on this processor..
   if (numptsperproc(nproc+1) >= 3 .and. .not. lupd_obspace_serial) then
      kdtree_grid => kdtree2_create(grdloc_chunk,sort=.false.,rearrange=.true.)
   endif
   if (numobsperproc(nproc+1) >= 3) then
      kdtree_obs  => kdtree2_create(obloc_chunk,sort=.false.,rearrange=.true.)
   end if
end if ! end if (.not. letkf_flag .or. lupd_obspace_serial)

end subroutine load_balance

subroutine estimate_work_enkf1(numobs)
! estimate work needed to update each analysis grid
! point (considering all the observations within the localization radius).
use covlocal, only:  latval

implicit none
integer(i_kind), dimension(:), intent(inout) :: numobs
real(r_single) :: deglat,corrlength,corrsq
type(kdtree2_result),dimension(:),allocatable :: sresults

integer nob,n1,n2,i,ideln

ideln = int(real(npts)/real(numproc))
n1 = 1 + nproc*ideln
n2 = (nproc+1)*ideln
if (nproc == numproc-1) n2 = npts
if (letkf_flag) allocate(sresults(nobstot))

! loop over 'good' obs.
numobs = 1 ! set min # of obs to 1, not 0 (so single ob test behaves)
!$omp parallel do  schedule(dynamic,1) private(nob,i,deglat,corrlength,sresults,corrsq)
obsloop: do i=n1,n2
    if (letkf_flag) then
       deglat = latsgrd(i)*rad2deg
       corrlength=latval(deglat,corrlengthnh,corrlengthtr,corrlengthsh)
       corrsq = corrlength**2
       call kdtree2_r_nearest(tp=kdtree_obs2,qv=gridloc(:,i),r2=corrsq,&
                              nfound=numobs(i),nalloc=nobstot,results=sresults)
    else
       do nob=1,nobstot
          if (sum((obloc(1:3,nob)-gridloc(1:3,i))**2,1) < corrlengthsq(nob)) &
          numobs(i) = numobs(i) + 1
       end do 
    endif
end do obsloop
!$omp end parallel do
if (letkf_flag) deallocate(sresults)

end subroutine estimate_work_enkf1

subroutine estimate_work_enkf2(numobs)
! estimate work needed to update each observation prior
! (considering all the observations within the localization radius of each

implicit none
integer(i_kind), dimension(:), intent(inout) :: numobs

integer(i_kind)  nob,nob2,n1,n2,ideln

if (nobstot > numproc) then
   ideln = int(real(nobstot)/real(numproc))
   n1 = 1 + nproc*ideln
   n2 = (nproc+1)*ideln
   if (nproc == numproc-1) n2 = nobstot
else
   if(nproc < nobstot)then
     n1 = nproc+1
     n2 = n1
   else
     n1=1
     n2=0
   end if
end if

! loop over 'good' obs.
numobs = 0
!$omp parallel do  schedule(dynamic,1) private(nob,nob2)
obsloop: do nob2=n1,n2
    do nob=1,nobstot
    ! find number of obs close to this ob.
       if (sum((obloc(1:3,nob)-obloc(1:3,nob2))**2,1) < corrlengthsq(nob))&
       numobs(nob2) = numobs(nob2) + 1
    end do ! loop over obs on this processor
end do obsloop
!$omp end parallel do

end subroutine estimate_work_enkf2

subroutine loadbal_cleanup()
! deallocate module-level allocatable arrays
if (allocated(obloc_chunk)) deallocate(obloc_chunk)
if (allocated(grdloc_chunk)) deallocate(grdloc_chunk)
if (allocated(lnp_chunk)) deallocate(lnp_chunk)
if (allocated(oblnp_chunk)) deallocate(oblnp_chunk)
if (allocated(obtime_chunk)) deallocate(obtime_chunk)
if (allocated(ensmean_obchunk)) deallocate(ensmean_obchunk)
if (allocated(iprocob)) deallocate(iprocob)
if (allocated(indxob_chunk)) deallocate(indxob_chunk)
if (allocated(indxproc_obs)) deallocate(indxproc_obs)
if (allocated(numptsperproc)) deallocate(numptsperproc)
if (allocated(numobsperproc)) deallocate(numobsperproc)
if (allocated(indxproc)) deallocate(indxproc)
if (associated(kdtree_obs)) call kdtree2_destroy(kdtree_obs)
if (associated(kdtree_obs2)) call kdtree2_destroy(kdtree_obs2)
if (associated(kdtree_grid)) call kdtree2_destroy(kdtree_grid)
end subroutine loadbal_cleanup

end module loadbal
