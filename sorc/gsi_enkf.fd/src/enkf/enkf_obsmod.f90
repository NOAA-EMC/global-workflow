module enkf_obsmod
!$$$  module documentation block
!
! module: obsmod           module to read obs, obs metadata and ob priors
!                          (written out by GSI forward operator)
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: holds observation and observation prior data, and associated
! metadata. Includes subroutines for reading the data from files written
! out by GSI forward operator code, and screening out data with gross errors.
!
! Public Subroutines:
!   readobs: reads obs, obs priors and associated metadata from files 
!    diag* files output from GSI forward operator, distributes data to 
!    all tasks. 
!   obsmod_cleanup: deallocate allocated arrays.
!
! Private Subroutines:
!   screenobs: screen out obs with large observation errors or 
!    that fail background check.
!   channelstats: compute number of obs per satellite sensor/channel
!    (numobspersat) and mean observation error (oberrvarmean) - excluding
!    screened obs. Called by public subroutine
!    readobs.
!
! Public Variables (all defined by subroutine readobs):
!   nobs_conv (integer scalar):  number of "conventional" obs (from prepbufr file).
!   nobs_oz (integer scalar): number of sbuv ozone obs.
!   nobs_sat (integer scalar): number of satellite radiance obs.
!   nobstot (integer scalar): total number of obs (=nobs_conv+nobs_oz+nobs_sat)
!   jpch_rad: (integer scalar) total number of satellite sensors/channels
!    (imported from module radinfo).
!   npred: (integer scalar) total number of adaptive bias correction terms
!    (imported from module radinfo).
!   indxsat(nobs_sat): maps satellite radiance observation number
!    (1,2,3...,nobs_sat) to satellite sensor/channel number (1,2,3...jpch_rad).
!    jpch_rad is total number of satellite sensor/channels, defined by module
!    radinfo. 
!   ob(nobstot): real array containing observation values.
!   obloclon(nobstot): real array of observation longitudes (degrees)
!   obloclat(nobstot): real array of observation longitudes (degrees)
!   obpress(nobstot): real array of observation pressures (hPa). For satellite
!    radiances, these are assumed to be the maximum of the weighting function
!    for that sensor/channel.
!   oblnp(nobstot): as above, but log(pressure).
!   obtime(nobstot): real array of observation time offsets (in hours, 
!    relative to analysis time).
!   obtype(nobstot): character(20) array of observation types ('ps','  u','  v',
!    'oz','t','amsua_n15', etc.).
!   oberrvar(nobstot): real array of observation error variances (which have
!    been modified by GSI forward operator).
!   oberrvar_orig(nobstot):  real array of unmodified observation error 
!    variance (as defined by bufr file or external error table).
!   ensmean_ob(nobstot): real array containing ensemble mean ob prior, 
!    including bias correction.
!   ensmean_obnobc(nobstot): as above, but not includeing bias correction.
!   obfit_prior(nobstot): obs(nob)-ensmean_ob(nob) for prior.
!   obfit_post(nobstot): obs(nob)-ensmean_ob(nob) for posterior (not
!    defined until after analysis).
!   obsprd_prior(nobstot): real array of ensemble variance of 
!     observation prior perturbations.
!   obsprd_post(nobstot): real array of ensemble variance of 
!     observation posterior perturbations (not defined until after analysis,
!     and then only on root task).
!   numobspersat(jpch_rad):  number of screened obs for each
!     satellite sensor/channel.
!   oberrvarmean(jpch_rad):  mean observation error variance for
!     each satellite sensor/channel for all screened obs
!   biaspreds(npred+1, nobs_sat):  real array of bias predictors for 
!     each satellite radiance ob (includes non-adaptive scan angle
!     bias correction term in biaspreds(1,1:nobs_sat)).
!     npred from radinfo module
!   deltapredx(npred,jpch_rad): real array of bias coefficient increments
!     (initialized to zero, updated by analysis).
!   obloc(3,nobstot): real array of spherical cartesian coordinates
!     (x,y,z) of screened observation locations.
!   stattype(nobstot):  integer array containing prepbufr report type
!     (e.g. 120 for radiosonde temp) for "conventional" obs (nob <= nobs_conv)
!     and satellite channel number for satellite radiance obs (nob >
!     nobs_conv+nobs_oz). For ozone obs (nobs_conv<nob<nobs_sat), 
!     set to 700 + k, where k is the  level index of the ozone retrieval.
!
! Modules Used: mpisetup, params, kinds, constants, mpi_readobs
!
! program history log:
!   2009-02-23  Initial version.
!   2011-06-20  Added the option of observation box for LETKF.
!   2015-07-25  Removed observation boxes for LETKF (use kdtree instead)
!   2017-08-01  Guoqing.Ge add "init_anasv()" before "init_oz()"
!        otherwise svars3d is not defined such that EnKF does not work right
!        for oz and it crashes EnKF compiled by GNU Fortran
!     NOTE: this requires anavinfo file to be present at running directory
!   2016-11-29  shlyaeva: Added the option of writing out ensemble spread in diag files
!   2019-03-21  CAPS(C. Tong) - added the code for direct reflecitivity DA capability
!
! attributes:
!   language: f95
!
!$$$

use mpimod, only: mpi_comm_world
use mpisetup, only: mpi_real4,mpi_sum,mpi_comm_io,mpi_in_place,numproc,nproc,&
                mpi_integer,mpi_wtime,mpi_status,mpi_real8,mpi_max
use kinds, only : r_kind, r_double, i_kind, r_single
use constants, only: zero, one, deg2rad, rad2deg, rd, cp, pi
use params, only: & 
      letkf_flag,nobsl_max,datestring,datapath,sprd_tol,nanals,saterrfact, &
      lnsigcutoffnh, lnsigcutoffsh, lnsigcutofftr, corrlengthnh,&
      corrlengthtr, corrlengthsh, obtimelnh, obtimeltr, obtimelsh,&
      lnsigcutoffsatnh, lnsigcutoffsatsh, lnsigcutoffsattr,&
      lnsigcutofffednh, lnsigcutofffedsh, lnsigcutofffedtr,&
      corrlengthfednh, corrlengthfedtr, corrlengthfedsh,   &
      varqc, huber, zhuberleft, zhuberright, modelspace_vloc, &
      lnsigcutoffpsnh, lnsigcutoffpssh, lnsigcutoffpstr, neigv, &
      lnsigcutoffrdrnh, lnsigcutoffrdrsh, lnsigcutoffrdrtr,&
      corrlengthrdrnh, corrlengthrdrtr, corrlengthrdrsh,   &
      l_use_enkf_directZDA

use state_vectors, only: init_anasv
use mpi_readobs, only:  mpi_getobs
use, intrinsic :: iso_c_binding

implicit none
private
public :: readobs, obsmod_cleanup, write_obsstats

real(r_single), public, allocatable, dimension(:) :: obsprd_prior, ensmean_obnobc,&
 ensmean_ob, ob, oberrvar, obloclon, obloclat, &
 obpress, obtime, oberrvar_orig,&
 oblnp, obfit_prior, prpgerr, oberrvarmean, probgrosserr, &
 lnsigl,corrlengthsq,obtimel
integer(i_kind), public, allocatable, dimension(:) :: numobspersat
integer(i_kind), allocatable, dimension(:)         :: diagused
! posterior stats computed in enkf_update
real(r_single), public, allocatable, dimension(:) :: obfit_post, obsprd_post
real(r_single), public, allocatable, dimension(:,:) :: biaspreds
real(r_kind), public, allocatable, dimension(:,:) :: deltapredx
! arrays passed to kdtree2 routines must be single.
real(r_single), public, allocatable, dimension(:,:) :: obloc
integer(i_kind), public, allocatable, dimension(:) :: stattype, indxsat
character(len=20), public, allocatable, dimension(:) :: obtype
integer(i_kind), public ::  nobs_sat, nobs_oz, nobs_conv, nobstot
integer(i_kind) :: nobs_convdiag, nobs_ozdiag, nobs_satdiag, nobstotdiag

! ob-space prior ensemble
! pointers used for MPI-3 shared memory manipulations.
! allocated and filled in mpi_readobs
real(r_single),public,pointer, dimension(:,:) :: anal_ob        ! Fortran pointer
type(c_ptr)                             :: anal_ob_cp           ! C pointer
real(r_single),public,pointer, dimension(:,:) :: anal_ob_modens ! Fortran pointer
type(c_ptr)                             :: anal_ob_modens_cp    ! C pointer
integer :: shm_win, shm_win2

! ob-space posterior ensemble, needed for EFSOI
real(r_single),public,allocatable, dimension(:,:) :: anal_ob_post   ! Fortran pointer
! is the observation assimilated? logical would be preferable, but that confuses
! Python
integer(i_kind),public,allocatable, dimension(:) :: assimltd_flag ! Fortran pointer

contains

subroutine readobs()
! reads obs, obs priors and associated metadata from 
! diag* files output from GSI forward operator, distributes data to 
! all tasks.  Ob prior perturbations for each ensemble member
! are written to a temp file, since the entire array can be 
! very large.
use radinfo, only: npred,jpch_rad,radinfo_read,pg_rad
use convinfo, only: convinfo_read, init_convinfo, cvar_pg, nconvtype, ictype,&
                    ioctype
use ozinfo, only: init_oz, ozinfo_read, pg_oz, jpch_oz, nusis_oz, nulev
use covlocal, only: latval
integer nob,j,ierr
real(r_double) t1
real(r_single) tdiff,tdiffmax,deglat,radlat,radlon
! read in conv data info
call init_convinfo()
call convinfo_read()
! read in oz data info
call init_anasv()
call init_oz()
call ozinfo_read()
! read radiance bias correction info (standard out redirected 
! specified unit number)
call radinfo_read()
!==> read in obs, obs metadata and ob priors.
!  (reading occurs on root, data broadcast to other tasks).
! temp files hxprime_ens_YYYYMMDDHH* are created containing 
! ob prior perturbations for each ensemble member.
! make bias predictors unitless and O(1)
! so bias coefficents have same units as radiance obs.
! (by computing RMS values over many analyses)
if (nproc == 0) print*, 'npred  = ', npred
! allocate array for bias correction increments, initialize to zero.
allocate(deltapredx(npred,jpch_rad))
deltapredx = zero
t1 = mpi_wtime()
call mpi_getobs(datapath, datestring, nobs_conv, nobs_oz, nobs_sat, nobstot,  &
                nobs_convdiag,nobs_ozdiag, nobs_satdiag, nobstotdiag,         &
                obsprd_prior, ensmean_obnobc, ensmean_ob, ob,                 &
                oberrvar, obloclon, obloclat, obpress,                        &
                obtime, oberrvar_orig, stattype, obtype, biaspreds, diagused, &
                anal_ob,anal_ob_modens,anal_ob_cp,anal_ob_modens_cp,          &
                shm_win,shm_win2, indxsat, nanals, neigv)

tdiff = mpi_wtime()-t1
call mpi_reduce(tdiff,tdiffmax,1,mpi_real4,mpi_max,0,mpi_comm_world,ierr)
if (nproc == 0) then
 print *,'max time in mpireadobs  = ',tdiffmax
 print *,'total number of obs ',nobstot
 print *,'min/max obtime ',minval(obtime),maxval(obtime)
endif
! if nobsl_max set for LETKF, and the total number of obs < nobsl_max,
! reset nobsl_max to -1
if (letkf_flag .and. nobsl_max > 0 .and. nobstot < nobsl_max) then
   if (nproc == 0) print *,'resetting nobsl_max to -1'
   nobsl_max=-1
endif
allocate(obfit_prior(nobstot))
! screen out some obs by setting ob error to a very large number
! set obfit_prior
call screenobs()

allocate(probgrosserr(nobstot),prpgerr(nobstot))
! initialize prob of gross error to 0.0 (will be reset by analysis if varqc is true)
probgrosserr = zero
if (varqc .and. .not. huber) then
   ! for flat-tail VarQC, read in a-prior prob of gross error.
   prpgerr = zero ! initialize to zero
   do nob=1,nobstot
      if (nob <= nobs_conv) then
         ! search for matching record in convinfo file. 
         ! if match found, set prob. of gross error to nonzero value given in
         ! file.
         do j=1,nconvtype
            if (ictype(j) == stattype(nob) .and. &
                ioctype(j) == obtype(nob)(1:3)) then
                prpgerr(nob) = cvar_pg(j)
                exit
            end if     
         enddo
      else if (nob <= nobs_conv+nobs_oz) then
         do j=1,jpch_oz
            if (stattype(nob)-700 == nulev(j) .and. obtype(nob) == nusis_oz(j)) then
                prpgerr(nob) = pg_oz(j)
                exit
            end if
         enddo
      else
         prpgerr(nob) = pg_rad(indxsat(nob-(nobs_conv+nobs_oz)))
      end if
      !if (nproc == 0) print *,nob,obtype(nob)(1:3),prpgerr(nob)
   enddo
endif
! compute number of usuable obs, average ob error for each satellite sensor/channel.
if (nobs_sat > 0) then
  call channelstats()
end if

! calculate locations of obs that passed initial screening in cartesian coords.
allocate(obloc(3,nobstot))
allocate(oblnp(nobstot)) ! log(p) at ob locations.
allocate(corrlengthsq(nobstot),lnsigl(nobstot),obtimel(nobstot))
lnsigl=1.e10
do nob=1,nobstot
   if (obloclon(nob) < zero) obloclon(nob) = obloclon(nob) + 360._r_single
   radlon=deg2rad*obloclon(nob)
   radlat=deg2rad*obloclat(nob)
! cartesian coordinates of 'good' obs.
   obloc(1,nob) = cos(radlat)*cos(radlon)
   obloc(2,nob) = cos(radlat)*sin(radlon)
   obloc(3,nob) = sin(radlat)
   deglat = obloclat(nob)
!  get limits on corrlength,lnsig,and obtime
   if (.not. modelspace_vloc) then
   if (nob > nobs_conv+nobs_oz) then
      lnsigl(nob) = latval(deglat,lnsigcutoffsatnh,lnsigcutoffsattr,lnsigcutoffsatsh)
   else if (obtype(nob)(1:3) == ' ps') then
      lnsigl(nob) = latval(deglat,lnsigcutoffpsnh,lnsigcutoffpstr,lnsigcutoffpssh)
   else if (obtype(nob)(1:3) == 'fed') then
      lnsigl(nob) = latval(deglat,lnsigcutofffednh,lnsigcutofffedtr,lnsigcutofffedsh)
   else if ( (obtype(nob)(1:3) == 'dbz' .or. obtype(nob)(1:3) == ' rw') .and. l_use_enkf_directZDA ) then
      lnsigl(nob) = latval(deglat,lnsigcutoffrdrnh,lnsigcutoffrdrtr,lnsigcutoffrdrsh)
   else
      lnsigl(nob)=latval(deglat,lnsigcutoffnh,lnsigcutofftr,lnsigcutoffsh)
   end if
   endif
   ! total column ozone has pressure set to zero, set to 0.001Pa
   ! and turn vertical localization off (no effect if modelspace_vloc=T)
   if (obpress(nob) < 0.001 .and. obtype(nob)(1:3) .eq. ' oz') then
      lnsigl(nob) = 1.e30    ! turn ob-space vert localization off
      obpress(nob) = 0.001   ! set to a non-zero value
   endif
   oblnp(nob) = -log(obpress(nob)) ! distance measured in log(p) units
   corrlengthsq(nob)=latval(deglat,corrlengthnh,corrlengthtr,corrlengthsh)**2
   if ( (obtype(nob)(1:3) == 'dbz' .or. obtype(nob)(1:3) == ' rw') .and. l_use_enkf_directZDA ) then
       corrlengthsq(nob)=latval(deglat,corrlengthrdrnh,corrlengthrdrtr,corrlengthrdrsh)**2
   end if
   if (obtype(nob)(1:3) == 'fed') then
       corrlengthsq(nob)=latval(deglat,corrlengthfednh,corrlengthfedtr,corrlengthfedsh)**2
   end if
   obtimel(nob)=latval(deglat,obtimelnh,obtimeltr,obtimelsh)
end do

! these allocated here, but not computed till after the state 
! update in enkf_update.
allocate(obfit_post(nobstot))
allocate(obsprd_post(nobstot))
obsprd_post = zero
end subroutine readobs

subroutine write_obsstats()
use readconvobs, only: write_convobs_data
use readozobs,   only: write_ozobs_data
use readsatobs,  only: write_satobs_data
character(len=10) :: id,id2,gesid2

  id = 'ensmean'
  id2 = 'enssprd'
  if (nproc==0) then
    if (nobs_conv > 0) then
       print *, 'obsprd, conv: ', minval(obsprd_prior(1:nobs_conv)),    &
              maxval(obsprd_prior(1:nobs_conv))
       gesid2 = 'ges'
       call write_convobs_data(datapath, datestring, nobs_conv, nobs_convdiag,  &
             obfit_prior(1:nobs_conv), obsprd_prior(1:nobs_conv),               &
             diagused(1:nobs_convdiag),                                         &
             id, id2, gesid2)
       gesid2 = 'anl'
       call write_convobs_data(datapath, datestring, nobs_conv, nobs_convdiag,  &
             obfit_post(1:nobs_conv), obsprd_post(1:nobs_conv),                 &
             diagused(1:nobs_convdiag),                                         &
             id, id2, gesid2)
    end if
    if (nobs_oz > 0) then
       print *, 'obsprd, oz: ', minval(obsprd_prior(nobs_conv+1:nobs_conv+nobs_oz)), &
              maxval(obsprd_prior(nobs_conv+1:nobs_conv+nobs_oz))
       gesid2 = 'ges'
       call write_ozobs_data(datapath, datestring, nobs_oz, nobs_ozdiag,  &
             obfit_prior(nobs_conv+1:nobs_conv+nobs_oz),                  &
             obsprd_prior(nobs_conv+1:nobs_conv+nobs_oz),                 &
             diagused(nobs_convdiag+1:nobs_convdiag+nobs_ozdiag),         &
             id, id2, gesid2)
       gesid2 = 'anl'
       call write_ozobs_data(datapath, datestring, nobs_oz, nobs_ozdiag,  &
             obfit_post(nobs_conv+1:nobs_conv+nobs_oz),                   &
             obsprd_post(nobs_conv+1:nobs_conv+nobs_oz),                  &
             diagused(nobs_convdiag+1:nobs_convdiag+nobs_ozdiag),         &
             id, id2, gesid2)
    end if
    if (nobs_sat > 0) then
       print *, 'obsprd, sat: ', minval(obsprd_prior(nobs_conv+nobs_oz+1:nobstot)), &
              maxval(obsprd_prior(nobs_conv+nobs_oz+1:nobstot))
       gesid2 = 'ges'
       call write_satobs_data(datapath, datestring, nobs_sat, nobs_satdiag, &
             obfit_prior(nobs_conv+nobs_oz+1:nobstot),                      &
             obsprd_prior(nobs_conv+nobs_oz+1:nobstot),                     & 
             diagused(nobs_convdiag+nobs_ozdiag+1:nobstotdiag),             &
             id, id2, gesid2)
       gesid2 = 'anl'
       call write_satobs_data(datapath, datestring, nobs_sat, nobs_satdiag, &
             obfit_post(nobs_conv+nobs_oz+1:nobstot),                       &
             obsprd_post(nobs_conv+nobs_oz+1:nobstot),                      &
             diagused(nobs_convdiag+nobs_ozdiag+1:nobstotdiag),             &
             id, id2, gesid2)
    end if
  endif


end subroutine write_obsstats

subroutine screenobs()
! screen out obs with large observation errors or 
! that fail background check.  For screened obs oberrvar is set to 1.e31_r_single
!use radbias, only: apply_biascorr
real(r_single) fail,failm
integer nn,nob
fail=1.e31_r_single
failm=1.e30_r_single
! apply bias correction here just for debugging purposes.
!call apply_biascorr()
!==> pre-process obs, obs metadata.
do nob=1,nobstot
  if (nob > nobs_conv+nobs_oz) oberrvar(nob) = saterrfact*oberrvar(nob)
  ! empirical adjustment of obs errors for Huber norm from ECMWF RD tech memo
  if (varqc) oberrvar(nob) = oberrvar(nob)*(min(one,0.5_r_single+0.125_r_single*(zhuberleft+zhuberright)))**2

  obfit_prior(nob) = ob(nob)-ensmean_ob(nob)

  ! gross error check.
  if (obsprd_prior(nob) > 1.e9) then
    oberrvar(nob)= fail
    if (obtype(nob)(1:3) == '  u') oberrvar(nob)=fail
    if (obtype(nob)(1:3) == '  v' .and. oberrvar(nob-1) >= failm) oberrvar(nob-1)=fail
    if(nproc == 0)print *, ' ob rejected due to obsprd_prior > 1.e9 ',obsprd_prior(nob)
    cycle
  end if

  if (obtype(nob) == ' ps' .and. (stattype(nob) == 111 .or. stattype(nob) == 112)) then
      ! tcvitals obs get a free pass for background check.
      if (nproc == 0) then
      print *,'tcvitals ps ob found (lat,lon,time,ob,O-F,sprd,oberr_std):'
      write(6,9101) stattype(nob),obloclat(nob),obloclon(nob),obtime(nob),ob(nob),obfit_prior(nob), &
                    sqrt(obsprd_prior(nob)),sqrt(oberrvar(nob))
      endif
  else
     ! background checks.
     if (abs(obfit_prior(nob)) > sprd_tol*sqrt(obsprd_prior(nob)+oberrvar(nob))) then
       !print *,obtype(nob)(1:3),stattype(nob),ob(nob),ensmean_ob(nob),obsprd_prior(nob),oberrvar(nob)
       if(nproc == 0)print *, 'reject ob due to obsprd_prior > sprd_tol ',&
          obtype(nob)(1:3),obfit_prior(nob),sprd_tol*sqrt(obsprd_prior(nob)+oberrvar(nob))
       oberrvar(nob)= fail
       if (obtype(nob)(1:3) == '  u') oberrvar(nob)=fail
       if (obtype(nob)(1:3) == '  v' .and. oberrvar(nob-1) >= failm) oberrvar(nob-1)=fail
       cycle
     end if
  end if


enddo
if (nproc == 0) then
   nn = 0
   do nob=1,nobstot
     if(oberrvar(nob) >= failm)nn = nn + 1
   end do
   print *,nobstot-nn,'obs kept'
   print *,nn,'total obs rejected'
end if ! nproc=0
9101 format(i3,7(1x,f7.2))

end subroutine screenobs

subroutine channelstats
use radinfo, only: jpch_rad
implicit none
integer(i_kind) nob,nob2,i
! count number of obs per channel/sensor.
allocate(numobspersat(jpch_rad))
allocate(oberrvarmean(jpch_rad))
numobspersat = 0
oberrvarmean = zero
do nob=1,nobs_sat
   nob2=nob+nobs_conv+nobs_oz
   i=indxsat(nob)
   numobspersat(i) = numobspersat(i) + 1
   oberrvarmean(i) = oberrvarmean(i) + oberrvar(nob2)
enddo
!if (nproc == 0) then
!   print *,'numobspersat:',minval(numobspersat),maxval(numobspersat)
!   print *,'oberrvarmean:',minval(oberrvarmean),maxval(oberrvarmean)
!end if
! average ob error for each channel.
do i=1,jpch_rad
   if (numobspersat(i) > 0) then
      oberrvarmean(i) = oberrvarmean(i)/real(numobspersat(i),r_single)
   else
      oberrvarmean(i) = 9.9e31_r_single
   end if
enddo

end subroutine channelstats

subroutine obsmod_cleanup()
integer ierr
! deallocate module-level allocatable arrays
if (allocated(obsprd_prior)) deallocate(obsprd_prior)
if (allocated(obfit_prior)) deallocate(obfit_prior)
if (allocated(obsprd_post)) deallocate(obsprd_post)
if (allocated(obfit_post)) deallocate(obfit_post)
if (allocated(ensmean_ob)) deallocate(ensmean_ob)
if (allocated(ensmean_obnobc)) deallocate(ensmean_obnobc)
if (allocated(ob)) deallocate(ob)
if (allocated(oberrvar)) deallocate(oberrvar)
if (allocated(oberrvar_orig)) deallocate(oberrvar_orig)
if (allocated(oberrvarmean)) deallocate(oberrvarmean)
if (allocated(obloclon)) deallocate(obloclon)
if (allocated(obloclat)) deallocate(obloclat)
if (allocated(obpress)) deallocate(obpress)
if (allocated(obtime)) deallocate(obtime)
if (allocated(oblnp)) deallocate(oblnp)
if (allocated(numobspersat)) deallocate(numobspersat)
if (allocated(obloc)) deallocate(obloc)
if (allocated(biaspreds)) deallocate(biaspreds)
if (allocated(deltapredx)) deallocate(deltapredx)
if (allocated(indxsat)) deallocate(indxsat)
if (allocated(obtype)) deallocate(obtype)
if (allocated(probgrosserr)) deallocate(probgrosserr)
if (allocated(prpgerr)) deallocate(prpgerr)
if (allocated(diagused)) deallocate(diagused)
if (allocated(anal_ob_post)) deallocate(anal_ob_post)
if (allocated(assimltd_flag)) deallocate(assimltd_flag)
! free shared memory segement, fortran pointer to that memory.
nullify(anal_ob)
call MPI_Barrier(mpi_comm_world,ierr)
call MPI_Win_free(shm_win, ierr)
if (neigv > 0) then
   nullify(anal_ob_modens)
   call MPI_Win_free(shm_win2, ierr)
endif
end subroutine obsmod_cleanup


end module enkf_obsmod
