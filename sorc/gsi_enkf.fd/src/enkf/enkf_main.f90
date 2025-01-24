program enkf_main
!$$$  main program documentation block
!
! program:  enkf_main                  high level driver program for 
!                                      ensemble kalman filter (EnKF).
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: This is the main program for the EnKF code. It does the following:
!           a) initialize MPI, read namelist from enkf.nml on each task.
!           b) reads observations, observation priors and associated
!              metadata for each ensemble member (from diag file
!              output by GSI forward operator code). Print innovation
!              statistics for prior.
!           c) read horizontal grid information (lat/lon of each grid point) and
!              pressure at each grid point/vertical level. 
!           d) decomposition of horizontal grid points and observation
!              priors to minimize load imbalance.
!           e) read state variables for each ensemble member, distribute 
!              to each task.
!           f) update state variables, observation priors and radiance
!              bias correction coefficients with EnKF. The observation
!              prior perturbations updated by each task are read in
!              from a temp file created in step b).
!           g) inflate posterior ensemble perturbations proportional 
!              to the amount that the variance was reduced by the analysis.
!           h) write out the analysis ensemble and updated radiance bias
!              correction coefficients. Print innovation statistics for
!              posterior.
!           i) deallocate all allocatable arrays, finalize MPI.
!
! program history log:
!   2009-02-23  Initial version.
!   2011-06-03  Added the option for LETKF.
!   2016-02-01  Initialize mpi communicator for IO tasks (1st nanals tasks).
!   2016-05-02  shlyaeva: Modification for reading state vector from table
!   2016-11-29  shlyaeva: Initialize state vector separately from control; 
!               separate routines for scatter and gather chunks; write out diag files
!               with spread
!
! usage:
!   input files:
!     sfg_YYYYMMDDHH_fhr06_mem* - first guess ensemble members, plus 
!                     ensemble mean (sfg_YYYYMMDDHH_fhr06_ensmean).
!     satbias_angle - satellite angle dependent file
!     satbias_in    - satellite bias correction coefficient file
!     satinfo       - satellite channel info file
!     convinfo      - convential data (prepufr) info file
!     ozinfo        - ozone retrieval info file
!     diag_YYYYMMDDHH_ges_mem*  - observation diagnostic files for each ensemble member
!                     created GSI forward operator.
!     hybens_info   - if parameter readin_localization is true, contains 
!                      vertical profile of horizontal and vertical localization
!                      length scales (along with static and ensemble weights
!                      used in hybrid).
!     anavinfo      - state/control variables table
!
!   output files: 
!     sanl_YYYYMMDDHH_mem* - analysis ensemble members. A separate program
!                            may be run to add system noise to these files.
!     covinflate.dat - multiplicative inflation (inflate_ens in module inflation).
!     satbias_out    - output satellite bias correction file.
!                         
! comments:
!
! This program is run after the forward operator code (with saving linearized H) 
! is run on the ensemble mean to create the diag*ensmean input file.
!
! attributes:
!   language: f95
!
!$$$

 use kinds, only: r_kind,r_double,i_kind
 use mpimod, only : mpi_comm_world
 ! reads namelist parameters.
 use params, only : read_namelist,cleanup_namelist,letkf_flag,readin_localization,lupd_satbiasc,&
                    numiter, nanals, lupd_obspace_serial, write_spread_diag,   &
                    lobsdiag_forenkf, netcdf_diag, efsoi_cycling, ntasks_io
 ! mpi functions and variables.
 use mpisetup, only:  mpi_initialize, mpi_initialize_io, mpi_cleanup, nproc, &
                       mpi_wtime
 ! obs and ob priors, associated metadata.
 use enkf_obsmod, only : readobs, write_obsstats, obfit_prior, obsprd_prior, &
                    nobs_sat, obfit_post, obsprd_post, obsmod_cleanup
 ! innovation statistics.
 use innovstats, only: print_innovstats
 ! model control vector 
 use controlvec, only: read_control, write_control, controlvec_cleanup, &
                     init_controlvec
 ! model state vector
 use statevec, only: read_state, statevec_cleanup, init_statevec
 ! EnKF linhx observer
 use observer_enkf, only: init_observer_enkf, destroy_observer_enkf
 ! load balancing
 use loadbal, only: load_balance, loadbal_cleanup, scatter_chunks, gather_chunks
 ! enkf update
 use enkf, only: enkf_update
 ! letkf update
 use letkf, only: letkf_update
 ! radiance bias correction coefficients.
 use radinfo, only: radinfo_write
 ! posterior ensemble inflation.
 use inflation, only: inflate_ens
 ! initialize radinfo variables
 use radinfo, only: init_rad, init_rad_vars
 use omp_lib, only: omp_get_max_threads
 use read_diag, only: set_netcdf_read
 ! Observation sensitivity usage
 use enkf_obs_sensitivity, only: init_ob_sens, print_ob_sens, destroy_ob_sens

 implicit none
 integer(i_kind) nth,ierr
 real(r_double) t1,t2
 logical no_inflate_flag

 ! initialize MPI.
 call mpi_initialize()
 if (nproc==0) call w3tagb('ENKF_ANL',2011,0319,0055,'NP25')

 ! Initial radinfo variables (some flags may be over-ridden in enkf namelist)
 call init_rad()

 ! read namelist.
 call read_namelist()

 ! initialize MPI communicator for IO tasks.
 call mpi_initialize_io(ntasks_io)

 ! Initialize derived radinfo variables
 call init_rad_vars()

 ! Initialize read_diag
 call set_netcdf_read(netcdf_diag)


 nth= omp_get_max_threads()
 if(nproc== 0)write(6,*) 'enkf_main:  number of threads ',nth

 ! Init and read state vector only if needed for linearized Hx
 if (lobsdiag_forenkf) then
    ! read state/control vector info from anavinfo
    call init_statevec()

    ! initialize observer
    call init_observer_enkf()

    ! read in ensemble members
    t1 = mpi_wtime()
    call read_state()
    t2 = mpi_wtime()
    if (nproc == 0) print *,'time in read_state =',t2-t1,'on proc',nproc
 endif

 ! read obs, initial screening.
 t1 = mpi_wtime()
 call readobs()
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in read_obs =',t2-t1,'on proc',nproc

 call mpi_barrier(mpi_comm_world, ierr)

 ! cleanup state vectors after observation operator is done if lin Hx
 if (lobsdiag_forenkf) then
    call statevec_cleanup()
    call destroy_observer_enkf()
 endif

 ! print innovation statistics for prior on root task.
 if (nproc == 0) then
    print *,'innovation statistics for prior:'
    call print_innovstats(obfit_prior, obsprd_prior)
 end if

 ! read state/control vector info from anavinfo
 call init_controlvec()

 ! read in ensemble members
 t1 = mpi_wtime()
 call read_control()
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in read_control =',t2-t1,'on proc',nproc

 ! Initialization for writing
 ! observation sensitivity files
 if(efsoi_cycling) call init_ob_sens()

 ! read in vertical profile of horizontal and vertical localization length
 ! scales, set values for each ob.
 if (readin_localization) call read_locinfo()

 ! do load balancing (partitioning of grid points, observations among
 ! processors)
 t1 = mpi_wtime()
 call load_balance()
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in load_balance =',t2-t1,'on proc',nproc

 ! distribute pieces to each task.
 t1 = mpi_wtime()
 call scatter_chunks()
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in scatter_chunks = ',t2-t1,'on proc',nproc

 t1 = mpi_wtime()
 ! state and bias correction coefficient update iteration.
 if(letkf_flag) then
    ! do ob space update using serial filter if desired
    if (lupd_obspace_serial) call enkf_update()
    call letkf_update()
 else
    call enkf_update()
 end if
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in enkf_update =',t2-t1,'on proc',nproc

 ! Output non-inflated
 ! analyses for FSO
 if(efsoi_cycling) then
    no_inflate_flag=.true.
    t1 = mpi_wtime()
    call gather_chunks()
    call write_control(no_inflate_flag)
    t2 = mpi_wtime()
    if (nproc == 0) print *,'time in write_ensemble wo/inflation =',t2-t1,'on proc',nproc
 end if
 no_inflate_flag=.false.

 ! posterior inflation.
 t1 = mpi_wtime()
 call inflate_ens()
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in inflate_ens =',t2-t1,'on proc',nproc

 if (write_spread_diag) then
    t1 = mpi_wtime()
    call write_obsstats()
    t2 = mpi_wtime()
    if (nproc == 0) print *,'time in write_obsstats =',t2-t1,'on proc',nproc
  endif

 ! print EFSO sensitivity i/o on root task.
 if(efsoi_cycling) call print_ob_sens()

 ! print innovation statistics for posterior on root task.
 if (nproc == 0 .and. numiter > 0) then
    print *,'innovation statistics for posterior:'
    call print_innovstats(obfit_post, obsprd_post)
 ! write out bias coeffs on root.
    if (nobs_sat > 0 .and. lupd_satbiasc) then
       call radinfo_write()
    end if
 end if

 ! free memory (radinfo memory freed in radinfo_write)
 ! and write out analysis ensemble.
 call obsmod_cleanup()

 t1 = mpi_wtime()
 call gather_chunks()
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in gather_chunks =',t2-t1,'on proc',nproc

 t1 = mpi_wtime()
 call write_control(no_inflate_flag)
 t2 = mpi_wtime()
 if (nproc == 0) print *,'time in write_control =',t2-t1,'on proc',nproc

 call controlvec_cleanup()
 call loadbal_cleanup()
 if(efsoi_cycling) call destroy_ob_sens()
 call cleanup_namelist()

 ! write log file (which script can check to verify completion).
 if (nproc .eq. 0) then
    call write_logfile()
 endif

 ! finalize MPI.
 if (nproc==0) call w3tage('ENKF_ANL')
 call mpi_cleanup()

end program enkf_main
