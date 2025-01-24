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
!  scatter_chunks: distribute ensemble members according to decomposition
!  gather_chunks:  gather ensemble members from decomposed chunks
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
!  lnp_chunk(npts_max,ncdim): real array of log(pressures) of control variables
!   being updated on this task.
!  oblnp_chunk(nobs_max,ncdim): (serial enkf only) real array of log(pressures) of ob priors
!   being updated on this task.
!  obtime_chunk(nobs_max): (serial enkf only) real array of ob times of ob priors
!   being updated on this task (expressed as an offset from the analysis time in
!   hours).
!  anal_obchunk_prior(nanals,nobs_max): (serial enkf only) real array of observation prior 
!   ensemble perturbations to be updated on this task (not used in LETKF).
!  anal_obchunk_modens_prior(nanals*neigv,nobs_max): (serial enkf only) real array of observation prior 
!   modulate ensemble perturbations to be updated on this task when model space localization
!   is enabled (modelspace_vloc=T, neigv > 0, not used in LETKF).
!  kdtree_grid: pointer to kd-tree structure used for nearest neighbor searches
!   for model grid points (only searches grid points assigned to this task).
!  kdtree_obs: pointer to kd-tree structure used for nearest neighbor searches
!   for observations (only searches ob locations assigned to this task).
!  kdtree_obs2: (LETKF only) pointer to kd-tree structure used for nearest neighbor searches
!   for observations (searches all observations)
!  anal_chunk(nanals,npts_max,ncdim,nbackgrounds): real array of ensemble perturbations
!   updated on each task.
!  anal_chunk_prior(nanals,npts_max,ncdim,nbackgrounds): real array of prior ensemble
!   perturbations.  Before analysis anal_chunk=anal_chunk_prior, after
!   analysis anal_chunk contains posterior perturbations.
!  ensmean_chunk(npts_max,ncdim,nbackgrounds): real array containing pieces of ensemble
!   mean to be updated on each task.
!  ensmean_chunk_prior(npts_max,ncdim,nbackgrounds): as above, for ensemble mean prior.
!   Before analysis ensmean_chunk=ensmean_chunk_prior, after analysis
!   ensmean_chunk contains posterior ensemble mean.
!   
!
! Modules Used: mpisetup, params, kinds, constants, enkf_obsmod, gridinfo,
!               kdtree_module, covlocal, controlvec
!
! program history log:
!   2009-02-23  Initial version.
!   2011-06-21  Added the option of observation box selection for LETKF.
!   2015-07-25  Remove observation box selection (use kdtree instead).
!   2016-05-02  shlyaeva: modification for reading state vector from table
!   2016-09-07  shlyaeva: moved distribution of chunks here from controlvec
!
! attributes:
!   language: f95
!
!$$$

use mpimod, only: mpi_comm_world
use mpisetup, only: mpi_real4,mpi_sum,mpi_comm_io,mpi_in_place,numproc,nproc,&
                mpi_integer,mpi_wtime,mpi_status,mpi_real8,mpi_max
use params, only: datapath, nanals, simple_partition, letkf_flag, nobsl_max,&
                  neigv, corrlengthnh, corrlengthsh, corrlengthtr, lupd_obspace_serial,letkf_bruteforce_search
use enkf_obsmod, only: nobstot, obloc, oblnp, ensmean_ob, obtime, &
                       anal_ob, anal_ob_modens, corrlengthsq
use kinds, only: r_kind, i_kind, r_double, r_single
use kdtree2_module, only: kdtree2, kdtree2_create, kdtree2_destroy, &
                          kdtree2_result, kdtree2_r_nearest
use gridinfo, only: gridloc, logp, latsgrd, nlevs_pres, npts
use constants, only: zero, rad2deg, deg2rad

implicit none
private
public :: load_balance, loadbal_cleanup, gather_chunks, scatter_chunks

real(r_single),public, allocatable, dimension(:,:) :: lnp_chunk, &
                                                      anal_obchunk_prior, &
                                                      anal_obchunk_modens_prior
real(r_single),public, allocatable, dimension(:,:,:,:) :: anal_chunk, anal_chunk_prior
real(r_single),public, allocatable, dimension(:,:,:) :: ensmean_chunk, ensmean_chunk_prior

! arrays passed to kdtree2 routines need to be single
real(r_single),public, allocatable, dimension(:,:) :: obloc_chunk, grdloc_chunk
real(r_single),public, allocatable, dimension(:) :: oblnp_chunk, &
 obtime_chunk, ensmean_obchunk
integer(i_kind),public, allocatable, dimension(:) :: iprocob, indxob_chunk,&
                          numptsperproc, numobsperproc
integer(i_kind),public, allocatable, dimension(:,:) :: indxproc, indxproc_obs
integer(i_kind),public :: npts_min, npts_max, nobs_min, nobs_max
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
   if (nobstot >= 3 .and. .not. letkf_bruteforce_search) then
      kdtree_obs2  => kdtree2_create(obloc,sort=.true.,rearrange=.true.)
   endif
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
if (letkf_flag .and. nobsl_max > 0) then
  where(numobs > nobsl_max) numobs = nobsl_max
endif
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
   ! for serial enkf, create observation priors to be updated on each processor.
   allocate(anal_obchunk_prior(nanals,nobs_max))
   do nob1=1,numobsperproc(nproc+1)
      nob2 = indxproc_obs(nproc+1,nob1)
      anal_obchunk_prior(1:nanals,nob1) = anal_ob(1:nanals,nob2)
   end do
   if (neigv > 0) then
      allocate(anal_obchunk_modens_prior(nanals*neigv,nobs_max)) 
      do nob1=1,numobsperproc(nproc+1)
         nob2 = indxproc_obs(nproc+1,nob1)
         anal_obchunk_modens_prior(1:nanals*neigv,nob1) = anal_ob_modens(1:nanals*neigv,nob2)
      end do
   endif
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



subroutine scatter_chunks
! distribute chunks from grdin (read in controlvec) according to
! decomposition from load_balance
use controlvec, only: ncdim, grdin
use params, only: nbackgrounds, ntasks_io, nanals_per_iotask
implicit none

integer(i_kind), allocatable, dimension(:) :: scounts, displs, rcounts
real(r_single), allocatable, dimension(:) :: sendbuf,recvbuf
integer(i_kind) :: np, nb, nn, n, nanal, i, ierr, ne

allocate(scounts(0:numproc-1))
allocate(displs(0:numproc-1))
allocate(rcounts(0:numproc-1))
! allocate array to hold pieces of state vector on each proc.
allocate(anal_chunk(nanals,npts_max,ncdim,nbackgrounds))
if (nproc == 0) print *,'anal_chunk size = ',size(anal_chunk,kind=8)

! only IO tasks send any data.
! scounts is number of data elements to send to processor np.
! rcounts is number of data elements to recv from processor np.
! displs is displacement into send array for data to go to proc np

if (real(numproc)*real(nanals_per_iotask)*real(npts_max)*real(ncdim) < 2_r_kind**32/2_r_kind - 1_r_kind) then
    do np=0,numproc-1
       displs(np) = np*nanals_per_iotask*npts_max*ncdim
    enddo
    if (nproc <= ntasks_io-1) then
       scounts = nanals_per_iotask*npts_max*ncdim
    else
       scounts = 0
    endif
    ! displs is also the displacement into recv array for data to go into anal_chunk
    ! on task np.
    do np=0,numproc-1
       if (np <= ntasks_io-1) then
          rcounts(np) = nanals_per_iotask*npts_max*ncdim
       else
          rcounts(np) = 0
       end if
    enddo
    allocate(sendbuf(numproc*nanals_per_iotask*npts_max*ncdim))
    allocate(recvbuf(numproc*nanals_per_iotask*npts_max*ncdim))

    ! send and receive buffers.
    do nb=1,nbackgrounds ! loop over time levels in background
    
      if (nproc <= ntasks_io-1) then
         ! fill up send buffer.
         do np=1,numproc
          do ne=1,nanals_per_iotask
            do nn=1,ncdim
             do i=1,numptsperproc(np)
              n = ((np-1)*ncdim*nanals_per_iotask + (ne-1)*ncdim + (nn-1))*npts_max + i
              sendbuf(n) = grdin(indxproc(np,i),nn,nb,ne)
            enddo
           enddo
          enddo
         enddo
      end if
      call mpi_alltoallv(sendbuf, scounts, displs, mpi_real4, recvbuf, rcounts, displs,&
                         mpi_real4, mpi_comm_world, ierr)
      
      !==> compute ensemble of first guesses on each task, remove mean from anal.
      !$omp parallel do schedule(dynamic,1)  private(nn,i,nanal,n)
      do nn=1,ncdim
         do i=1,numptsperproc(nproc+1)
            do nanal=1,nanals
               n = ((nanal-1)*ncdim + (nn-1))*npts_max + i
               anal_chunk(nanal,i,nn,nb) = recvbuf(n)
            enddo
         end do
      end do
      !$omp end parallel do
    
    enddo ! loop over nbackgrounds
else
   do np=0,numproc-1
      displs(np) = np*nanals_per_iotask*npts_max
   enddo
   if (nproc <= ntasks_io-1) then
      scounts = nanals_per_iotask*npts_max
   else
      scounts = 0
   endif
   ! displs is also the displacement into recv array for data to go into anal_chunk
   ! on task np.
   do np=0,numproc-1
      if (np <= ntasks_io-1) then
         rcounts(np) = nanals_per_iotask*npts_max
      else
         rcounts(np) = 0
      end if
   enddo
   allocate(sendbuf(numproc*nanals_per_iotask*npts_max))
   allocate(recvbuf(numproc*nanals_per_iotask*npts_max))
   
   ! send and receive buffers.
   do nb=1,nbackgrounds ! loop over time levels in background
     do nn=1,ncdim ! loop over levels
   
       if (nproc <= ntasks_io-1) then
          ! fill up send buffer.
          do np=1,numproc
           do ne=1,nanals_per_iotask
             do i=1,numptsperproc(np)
               n = ((ne-1)*nanals_per_iotask + (np-1))*npts_max + i
               sendbuf(n) = grdin(indxproc(np,i),nn,nb,ne)
             enddo
           enddo
          enddo
       end if
       call mpi_alltoallv(sendbuf, scounts, displs, mpi_real4, recvbuf, rcounts, displs,&
                          mpi_real4, mpi_comm_world, ierr)
       !==> compute ensemble of first guesses on each task, remove mean from anal.
       !$omp parallel do schedule(dynamic,1)  private(i,nanal,n)
       do i=1,numptsperproc(nproc+1)
          do nanal=1,nanals
             n = (nanal-1)*npts_max + i
             anal_chunk(nanal,i,nn,nb) = recvbuf(n)
          enddo
       end do
       !$omp end parallel do
   
     enddo ! end loop over levels
   enddo ! loop over nbackgrounds
endif

deallocate(sendbuf, recvbuf)
allocate(anal_chunk_prior(nanals,npts_max,ncdim,nbackgrounds))
allocate(ensmean_chunk(npts_max,ncdim,nbackgrounds))
allocate(ensmean_chunk_prior(npts_max,ncdim,nbackgrounds))
ensmean_chunk = 0_r_single

!==> compute mean, remove it from anal_chunk
!$omp parallel do schedule(dynamic,1)  private(nn,i,n,nb)
do nb=1,nbackgrounds
  do nn=1,ncdim
     do i=1,numptsperproc(nproc+1)
        ensmean_chunk(i,nn,nb) = sum(anal_chunk(:,i,nn,nb))/float(nanals)
        ensmean_chunk_prior(i,nn,nb) = ensmean_chunk(i,nn,nb)
        ! remove mean from ensemble.
        do nanal=1,nanals
           anal_chunk(nanal,i,nn,nb) = anal_chunk(nanal,i,nn,nb)-ensmean_chunk(i,nn,nb)
           anal_chunk_prior(nanal,i,nn,nb)=anal_chunk(nanal,i,nn,nb)
        end do
     end do
  end do
end do
!$omp end parallel do


end subroutine scatter_chunks

subroutine gather_chunks
! gather chunks into grdin to write out the ensemble members
use controlvec, only: ncdim, grdin
use params, only: nbackgrounds, ntasks_io, nanals_per_iotask
implicit none
integer(i_kind), allocatable, dimension(:) :: scounts, displs, rcounts
real(r_single), allocatable, dimension(:) :: sendbuf,recvbuf
integer(i_kind) :: np, nb, nn, nanal, n, i, ierr, ne

allocate(scounts(0:numproc-1))
allocate(displs(0:numproc-1))
allocate(rcounts(0:numproc-1))
! all tasks send data, but only IO tasks receive any data.
! scounts is number of data elements to send to processor np.
! rcounts is number of data elements to recv from processor np.
! displs is displacement into send array for data to go to proc np

if (real(numproc)*real(nanals_per_iotask)*real(npts_max)*real(ncdim) < 2_r_kind**32/2_r_kind - 1_i_kind) then
   if (nproc <= ntasks_io-1) then
      rcounts = nanals_per_iotask*npts_max*ncdim
   else
      rcounts = 0
   endif
   do np=0,numproc-1
      displs(np) = np*nanals_per_iotask*npts_max*ncdim
      if (np <= ntasks_io-1) then
         scounts(np) = nanals_per_iotask*npts_max*ncdim
      else
         scounts(np) = 0
      end if
   enddo
   allocate(recvbuf(numproc*nanals_per_iotask*npts_max*ncdim))
   allocate(sendbuf(numproc*nanals_per_iotask*npts_max*ncdim))
   do nb=1,nbackgrounds ! loop over time levels in background
     do nn=1,ncdim
       do i=1,numptsperproc(nproc+1)
         do nanal=1,nanals
            n = ((nanal-1)*ncdim + (nn-1))*npts_max + i
            ! add ensemble mean back in.
            sendbuf(n) = anal_chunk(nanal,i,nn,nb)+ensmean_chunk(i,nn,nb)
            ! convert to increment (A-F).
            sendbuf(n) = sendbuf(n)-(anal_chunk_prior(nanal,i,nn,nb)+ensmean_chunk_prior(i,nn,nb))
         enddo
       enddo
     enddo
     call mpi_alltoallv(sendbuf, scounts, displs, mpi_real4, recvbuf, rcounts, displs,&
                        mpi_real4, mpi_comm_world, ierr)
     if (nproc <= ntasks_io-1) then
       do np=1,numproc
        do ne=1,nanals_per_iotask
         do nn=1,ncdim
          do i=1,numptsperproc(np)
            n = ((np-1)*ncdim*nanals_per_iotask + (ne-1)*ncdim + (nn-1))*npts_max + i
            grdin(indxproc(np,i),nn,nb,ne) = recvbuf(n)
          enddo
         enddo
        enddo
       enddo
       !print *,nproc,'min/max ps',minval(grdin(:,ncdim)),maxval(grdin(:,ncdim))
     end if
   enddo ! end loop over background time levels
else
   if (nproc <= ntasks_io-1) then
      rcounts = nanals_per_iotask*npts_max
   else
      rcounts = 0
   endif
   do np=0,numproc-1
      displs(np) = np*nanals_per_iotask*npts_max
      if (np <= ntasks_io-1) then
         scounts(np) = nanals_per_iotask*npts_max
      else
         scounts(np) = 0
      end if
   enddo
   allocate(recvbuf(numproc*nanals_per_iotask*npts_max))
   allocate(sendbuf(numproc*nanals_per_iotask*npts_max))
   do nb=1,nbackgrounds ! loop over time levels in background
     do nn=1,ncdim ! loop over levels
       do i=1,numptsperproc(nproc+1)
         do nanal=1,nanals
            n = (nanal-1)*npts_max + i
            ! add ensemble mean back in.
            sendbuf(n) = anal_chunk(nanal,i,nn,nb)+ensmean_chunk(i,nn,nb)
            ! convert to increment (A-F).
            sendbuf(n) = sendbuf(n)-(anal_chunk_prior(nanal,i,nn,nb)+ensmean_chunk_prior(i,nn,nb))
         enddo
       enddo
       call mpi_alltoallv(sendbuf, scounts, displs, mpi_real4, recvbuf, rcounts, displs,&
                        mpi_real4, mpi_comm_world, ierr)
       if (nproc <= ntasks_io-1) then
         do np=1,numproc
          do ne=1,nanals_per_iotask
            do i=1,numptsperproc(np)
              n = ((ne-1)*nanals_per_iotask + (np-1))*npts_max + i
              grdin(indxproc(np,i),nn,nb,ne) = recvbuf(n)
            enddo
          enddo
         enddo
         !print *,nproc,'min/max ps',minval(grdin(:,ncdim)),maxval(grdin(:,ncdim))
       end if
     enddo ! end loop over levels
   enddo ! end loop over background time levels
endif
deallocate(sendbuf, recvbuf)

end subroutine gather_chunks


subroutine estimate_work_enkf1(numobs)
! estimate work needed to update each analysis grid
! point (considering all the observations within the localization radius).
use covlocal, only:  latval

implicit none
integer(i_kind), dimension(:), intent(inout) :: numobs
real(r_single) :: deglat,corrlength,corrsq,r
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

       if (associated(kdtree_obs2)) then
         call kdtree2_r_nearest(tp=kdtree_obs2,qv=gridloc(:,i),r2=corrsq,&
              nfound=numobs(i),nalloc=nobstot,results=sresults)
       else
         do nob = 1, nobstot
            r = sum( (gridloc(:,i)-obloc(:,nob))**2, 1)
            if (r < corrsq) then
              numobs(i) = numobs(i) + 1
            endif
         enddo
       endif
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
if (allocated(anal_chunk)) deallocate(anal_chunk)
if (allocated(anal_chunk_prior)) deallocate(anal_chunk_prior)
if (allocated(ensmean_chunk)) deallocate(ensmean_chunk)
if (allocated(ensmean_chunk_prior)) deallocate(ensmean_chunk_prior)
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
