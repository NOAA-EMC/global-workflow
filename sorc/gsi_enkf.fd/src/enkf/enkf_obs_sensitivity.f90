module enkf_obs_sensitivity
!$$$ module documentation block
!           .      .    .                                       .
! module:   obs_sensitivity
!   prgmmr: ota
!
! abstract: Contains variables and routines for computation of
!           forecast sensitivity to observations.
!
! program history log:
!   2011-07-20 ota    - created
!
! Subroutines Included:
!   init_ob_sens    - Initialize variables
!   read_ob_sens    - Read observation diagnostics from the original formatted
!                     information file instead of the standard diagnostic file
!   print_ob_sens   - Print out observation sensitivity informations
!   destroy_ob_sens - Deallocate variables
!
! Variable Definitions:
!   adloc_chunk - Coordinates of observation response
!   obsense_kin - forecast sensitivity on each observations (kinetic energy)
!   obsense_dry - forecast sensitivity on each observations (dry total energy)
!   obsense_moist - forecast sensitivity on each observations (moist total energy)
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
! -----------------------------------------------------------------------------
use mpimod, only: mpi_comm_world
use mpisetup, only: mpi_real4,mpi_sum,mpi_comm_io,mpi_in_place,numproc,nproc,&
                mpi_integer,mpi_wtime,mpi_status,mpi_real8,mpi_max,mpi_realkind
use kinds, only: r_single,r_kind,r_double,i_kind
use params, only: efsoi_flag,latbound,nlevs,nanals,datestring, &
                  lnsigcutoffsatnh,lnsigcutoffsattr,lnsigcutoffsatsh, &
                  lnsigcutoffpsnh,lnsigcutoffpstr,lnsigcutoffpssh, &
                  lnsigcutofffednh,lnsigcutofffedtr,lnsigcutofffedsh, &
                  lnsigcutoffnh,lnsigcutofftr,lnsigcutoffsh, &
                  corrlengthnh,corrlengthtr,corrlengthsh, &
                  obtimelnh,obtimeltr,obtimelsh,letkf_flag, &
                  nbackgrounds,adrate,eft
use constants, only: zero,one,half,rearth,pi,deg2rad,rad2deg
use enkf_obsmod, only: nobstot,nobs_conv,nobs_oz,nobs_sat,obtype,obloclat, &
                       obloclon,obpress,indxsat,oberrvar,stattype,obtime,ob, &
                       ensmean_ob,ensmean_obnobc,obsprd_prior,obfit_prior, &
                       oberrvar_orig,biaspreds,anal_ob_post,nobstot,lnsigl, &
                       corrlengthsq,obtimel,oblnp,obloc ,assimltd_flag
use convinfo, only: convinfo_read,init_convinfo
use ozinfo, only: ozinfo_read,init_oz
use radinfo, only: radinfo_read,jpch_rad,nusis,nuchan,npred
use loadbal, only: indxproc,grdloc_chunk,numptsperproc,npts_max,kdtree_grid
use covlocal, only: latval
use kdtree2_module, only: kdtree2_create

implicit none

private
public init_ob_sens,destroy_ob_sens,print_ob_sens,read_ob_sens,&
       obsense_kin,obsense_dry,obsense_moist,adloc_chunk

real(r_kind),allocatable,dimension(:) :: obsense_kin,obsense_dry,obsense_moist
real(r_single),allocatable,dimension(:,:) :: adloc_chunk

! Structure for observation sensitivity information output
type obsense_header
  sequence
  integer(i_kind) :: idate              ! Base date (initial date)
  integer(i_kind) :: obsnum             ! Observation number (total)
  integer(i_kind) :: convnum            ! Observation number (conventional)
  integer(i_kind) :: oznum              ! Observation number (ozone)
  integer(i_kind) :: satnum             ! Observation number (satellite)
  integer(i_kind) :: npred              ! Number of predictors for bias correction
  integer(i_kind) :: nanals             ! Number of members
end type obsense_header

! Type definition for observation sensitivity information file
type obsense_info
  sequence
  real(r_single)  :: obfit_prior        ! Observation fit to the first guess
  real(r_single)  :: obsprd_prior       ! Spread of observation prior
  real(r_single)  :: ensmean_obnobc     ! Ensemble mean first guess (no bias correction)
  real(r_single)  :: ensmean_ob         ! Ensemble mean first guess (bias corrected)
  real(r_single)  :: ob                 ! Observation value
  real(r_single)  :: oberrvar           ! Observation error variance
  real(r_single)  :: lon                ! Longitude
  real(r_single)  :: lat                ! Latitude
  real(r_single)  :: pres               ! Pressure
  real(r_single)  :: time               ! Observation time
  real(r_single)  :: oberrvar_orig      ! Original error variance
  integer(i_kind) :: stattype           ! Observation type
  character(len=20) :: obtype           ! Observation element / Satellite name
  integer(i_kind) :: indxsat            ! Satellite index (channel)
  integer(i_kind) :: assimltd_flag      ! is assimilated? flag
  real(r_single)  :: osense_kin         ! Observation sensitivity (kinetic energy) [J/kg]
  real(r_single)  :: osense_dry         ! Observation sensitivity (Dry total energy) [J/kg]
  real(r_single)  :: osense_moist       ! Observation sensitivity (Moist total energy) [J/kg]
end type obsense_info

contains


subroutine init_ob_sens
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_ob_sens
!   prgmmr: ota
!
! abstract: Allocate and initialize arrays used for this module
!
! program history log:
!   2011-07-20  ota    - created
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  allocate(obsense_kin(nobstot))
  allocate(obsense_dry(nobstot))
  allocate(obsense_moist(nobstot))
  obsense_kin(1:nobstot) = zero
  obsense_dry(1:nobstot) = zero
  obsense_moist(1:nobstot) = zero
  return

end subroutine init_ob_sens


subroutine read_ob_sens
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_ob_sens
!   prgmmr: ota
!
! abstract: read observation diagnostics from the original formatted
!   information file instead of the standard diagnostic file.
!
! program history log:
!   2011-12-30  ota    - created
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  integer(i_kind) :: nob, nn
  real(r_kind) :: radlon,radlat
  real(r_single) :: deglat
  real(r_double) :: t1, tdiff
  integer(i_kind) :: iunit, iunitout
  real(r_single),allocatable,dimension(:) :: tmpanal_ob,tmpbiaspreds
  type(obsense_header) :: inhead
  type(obsense_info) :: indata
  ! read in conv data info
  call init_convinfo()
  iunitout = 205
  call convinfo_read
  ! read in oz data info
  call init_oz()
  iunitout = 206
  call ozinfo_read
  ! read radiance bias correction info (standard out redirected 
  ! specified unit number)
  iunitout = 207
  call radinfo_read
  ! read observation informations from obsense file
  t1 = mpi_wtime()
  iunit = 10
  open(iunit,file='osense_'//datestring//'.dat',form='unformatted')
  read(iunit) inhead             ! Header record
  nobstot = inhead%obsnum
  nobs_conv = inhead%convnum
  nobs_oz = inhead%oznum
  nobs_sat = inhead%satnum
  if(npred /= inhead%npred) then
     write(6,*) 'READ_OBSENSE:  number of predictor for bias correction is not correct.',npred,inhead%npred
     call stop2(26)
  end if
  if(nanals /= inhead%nanals) then
     write(6,*) 'READ_OBSENSE:  number of members is not correct.',nanals,inhead%nanals
     call stop2(26)
  end if

  if(nproc == 0) write(6,*) 'total number of obs ',nobstot
  if(nproc == 0) write(6,*) 'total number of conv obs ',nobs_conv
  if(nproc == 0) write(6,*) 'total number of oz obs',nobs_oz
  if(nproc == 0) write(6,*) 'total number of sat obs',nobs_sat
  if(nproc == 0) write(6,*) 'npred=',inhead%npred
  if(nproc == 0) write(6,*) 'idate=',inhead%idate
  if(nproc == 0) write(6,*) 'nanals=',inhead%nanals

  ! Allocate arrays
  allocate(obfit_prior(nobstot))
  allocate(obsprd_prior(nobstot))
  allocate(ensmean_obnobc(nobstot))
  allocate(ensmean_ob(nobstot))
  allocate(ob(nobstot))
  allocate(oberrvar(nobstot))
  allocate(obloclon(nobstot))
  allocate(obloclat(nobstot))
  allocate(obpress(nobstot))
  allocate(obtime(nobstot))
  allocate(oberrvar_orig(nobstot))
  allocate(stattype(nobstot))
  allocate(obtype(nobstot))
  allocate(indxsat(nobs_sat))
  allocate(assimltd_flag(nobstot))
  allocate(biaspreds(npred+1,nobs_sat))
  allocate(tmpanal_ob(nanals))
  allocate(tmpbiaspreds(npred+1))
  if(nproc == 0) allocate(anal_ob_post(nanals,nobstot))
  ! Read loop over conventional observations
  do nob=1,nobs_conv+nobs_oz
     read(iunit) indata,tmpanal_ob
     obfit_prior(nob) = real(indata%obfit_prior,r_kind)
     obsprd_prior(nob) = real(indata%obsprd_prior,r_kind)
     ensmean_obnobc(nob) = real(indata%ensmean_obnobc,r_kind)
     ensmean_ob(nob) = real(indata%ensmean_ob,r_kind)
     ob(nob) = real(indata%ob,r_kind)
     oberrvar(nob) = real(indata%oberrvar,r_kind)
     obloclon(nob) = real(indata%lon,r_kind)
     obloclat(nob) = real(indata%lat,r_kind)
     obpress(nob) = real(indata%pres,r_kind)
     obtime(nob) = real(indata%time,r_kind)
     oberrvar_orig(nob) = real(indata%oberrvar_orig,r_kind)
     stattype(nob) = indata%stattype
     obtype(nob) = indata%obtype
     assimltd_flag(nob) = indata%assimltd_flag
     if(nproc == 0) anal_ob_post(1:nanals,nob) = real(tmpanal_ob(1:nanals),r_kind)
  end do
  ! Read loop over satellite radiance observations
  nn = 0
  do nob=nobs_conv+nobs_oz+1,nobstot
     nn = nn + 1
     read(iunit) indata,tmpanal_ob,tmpbiaspreds
     obfit_prior(nob) = real(indata%obfit_prior,r_kind)
     obsprd_prior(nob) = real(indata%obsprd_prior,r_kind)
     ensmean_obnobc(nob) = real(indata%ensmean_obnobc,r_kind)
     ensmean_ob(nob) = real(indata%ensmean_ob,r_kind)
     ob(nob) = real(indata%ob,r_kind)
     oberrvar(nob) = real(indata%oberrvar,r_kind)
     obloclon(nob) = real(indata%lon,r_kind)
     obloclat(nob) = real(indata%lat,r_kind)
     obpress(nob) = real(indata%pres,r_kind)
     obtime(nob) = real(indata%time,r_kind)
     oberrvar_orig(nob) = real(indata%oberrvar_orig,r_kind)
     stattype(nob) = indata%stattype
     obtype(nob) = indata%obtype
     indxsat(nn) = indata%indxsat
     assimltd_flag(nob) = indata%assimltd_flag
     if(nproc == 0) anal_ob_post(1:nanals,nob) = real(tmpanal_ob(1:nanals),r_kind)
     biaspreds(1:npred+1,nn) = real(tmpbiaspreds(1:npred+1),r_kind)
  end do
  if(nn /= nobs_sat) then
     write(6,*) 'READ_OBSENSE:  number of satellite radiance observations does not much the header information.',nn,nobs_sat
     call stop2(26)
  end if
  close(iunit)
  deallocate(tmpanal_ob,tmpbiaspreds)

  tdiff = mpi_wtime()-t1
  if (nproc == 0) print *,'max time in read obsense.dat = ',tdiff
  
  ! calculate locations of obs that passed initial screening in cartesian coords.
  allocate(obloc(3,nobstot))
  allocate(oblnp(nobstot)) ! log(p) at ob locations.
  allocate(corrlengthsq(nobstot),lnsigl(nobstot),obtimel(nobstot))
  do nob=1,nobstot
     oblnp(nob) = -log(obpress(nob)) ! distance measured in log(p) units
     if (obloclon(nob) < zero) obloclon(nob) = obloclon(nob) + 360._r_kind
     radlon=deg2rad*obloclon(nob)
     radlat=deg2rad*obloclat(nob)
     ! cartesian coordinates of 'good' obs.
     obloc(1,nob) = cos(radlat)*cos(radlon)
     obloc(2,nob) = cos(radlat)*sin(radlon)
     obloc(3,nob) = sin(radlat)
     deglat = obloclat(nob)
     !  get limits on corrlength,lnsig,and obtime
     if (nob > nobs_conv+nobs_oz) then
        lnsigl(nob) = latval(deglat,lnsigcutoffsatnh,lnsigcutoffsattr,lnsigcutoffsatsh)
     else if (obtype(nob)(1:3) == ' ps') then
        lnsigl(nob) = latval(deglat,lnsigcutoffpsnh,lnsigcutoffpstr,lnsigcutoffpssh)
     else if (obtype(nob)(1:3) == 'fed') then
        lnsigl(nob) = latval(deglat,lnsigcutofffednh,lnsigcutofffedtr,lnsigcutofffedsh)
     else
        lnsigl(nob)=latval(deglat,lnsigcutoffnh,lnsigcutofftr,lnsigcutoffsh)
     end if
     corrlengthsq(nob)=latval(deglat,corrlengthnh,corrlengthtr,corrlengthsh)**2
     obtimel(nob)=latval(deglat,obtimelnh,obtimeltr,obtimelsh)     
  end do

  return
end subroutine read_ob_sens

subroutine print_ob_sens
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    print_ob_sens
!   prgmmr: ota
!
! abstract: Print out forecast sensitivity to the observations classified by
!           the observation types. Writes out observation impact estimates of
!           each observation with original formatted file.
!
! program history log:
!   2011-07-20  ota    - created
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  integer(i_kind) nob_conv(8,3),nob,iobtyp,ireg
  real(r_kind) sumsense_conv(8,3,3),rate_conv(8,3,3)
  integer(i_kind) nob_sat(jpch_rad),nchan,nn
  real(r_kind) sumsense_sat(jpch_rad,3),rate_sat(jpch_rad,3)
  ! Indices for observation elements
  integer(i_kind) :: itemp=1, ipw=2, ispd=3, igps=4, iqh=5, iwnd=6, ioz=7, ips=8
  character(len=3) :: chartype(8)
  ! Indices for the regions
  integer(i_kind) :: regnh=1, regtr=2, regsh=3
  character(len=2) :: charreg(3)
  ! Indices for the sensitivity measurement types
  integer(i_kind) :: stkin=1, stdry=2, stmoist=3
  ! For MPI
  real(r_kind),allocatable :: recbuf(:)
  integer(i_kind) :: ierr, iunit
  ! For data I/O
  real(r_single),allocatable :: tmpanal_ob(:), tmpbiaspreds(:)
  type(obsense_header) :: outhead
  type(obsense_info) :: outdata
  iunit = 10
  ! Gather observation sensitivity informations to the root
  if(efsoi_flag) then
     allocate(recbuf(nobstot))
     call mpi_reduce(obsense_kin,recbuf,nobstot,mpi_realkind,mpi_sum,0, &
          & mpi_comm_world,ierr)
     if(nproc == 0) obsense_kin(1:nobstot) = recbuf(1:nobstot)
     call mpi_reduce(obsense_dry,recbuf,nobstot,mpi_realkind,mpi_sum,0, &
          & mpi_comm_world,ierr)
     if(nproc == 0) obsense_dry(1:nobstot) = recbuf(1:nobstot)
     call mpi_reduce(obsense_moist,recbuf,nobstot,mpi_realkind,mpi_sum,0, &
          & mpi_comm_world,ierr)
     if(nproc == 0) obsense_moist(1:nobstot) = recbuf(1:nobstot)
     deallocate(recbuf)
  end if
  if(nproc /= 0) return
  ! Open observation sensitivity output
  read(datestring,*) outhead%idate
  outhead%obsnum = nobstot
  outhead%convnum = nobs_conv
  outhead%oznum = nobs_oz
  outhead%satnum = nobs_sat
  outhead%npred = npred
  outhead%nanals = nanals
  allocate(tmpanal_ob(nanals))
  allocate(tmpbiaspreds(npred+1))
  open(iunit,file='osense_'//datestring//'.dat',form='unformatted')
  write(iunit) outhead             ! Header record
  ! Statistics for conventional observations
  if (nobs_conv+nobs_oz > 0) then
     ! Initialize arrays
     nob_conv(:,:) = 0
     sumsense_conv(:,:,:) = zero
     rate_conv(:,:,:) = zero
     chartype(itemp)='  t'
     chartype(ipw)  =' pw'
     chartype(ispd) ='spd'
     chartype(igps) ='gps'
     chartype(iqh)  ='  q'
     chartype(iwnd) =' uv'
     chartype(ioz)  =' oz'
     chartype(ips)  =' ps'
     charreg(regnh) ='NH'
     charreg(regtr) ='TR'
     charreg(regsh) ='SH'
     ! Loop over each observations
     do nob=1,nobs_conv+nobs_oz
        ! Select observation elements
        if (obtype(nob)(1:3) == ' ps') then
           iobtyp=ips
        else if (obtype(nob)(1:3) == '  t' .and. stattype(nob) /= 121) then
           iobtyp=itemp
        else if (obtype(nob)(1:3) == '  u' .or. obtype(nob)(1:3) == '  v') then
           iobtyp=iwnd
        else if (obtype(nob)(1:3) == '  q') then
           iobtyp=iqh
        else if (obtype(nob)(1:3) == 'spd') then
           iobtyp=ispd
        else if (obtype(nob)(1:3) == 'gps') then
           iobtyp=igps
        else if (obtype(nob)(1:3) == ' pw') then
           iobtyp=ipw
        else if (nob > nobs_conv) then
           iobtyp=ioz
        else
           cycle
        end if
        ! Select observation regions
        if (obloclat(nob) > latbound) then
           ireg=regnh
        else if (obloclat(nob) < -latbound) then
           ireg=regsh
        else
           ireg=regtr
        end if
        ! Output individual observation record
        outdata%obfit_prior = real(obfit_prior(nob),r_single)
        outdata%obsprd_prior = real(obsprd_prior(nob),r_single)
        outdata%ensmean_obnobc = real(ensmean_obnobc(nob),r_single)
        outdata%ensmean_ob = real(ensmean_ob(nob),r_single)
        outdata%ob = real(ob(nob),r_single)
        outdata%oberrvar = real(oberrvar(nob),r_single)
        outdata%lon = real(obloclon(nob),r_single)
        outdata%lat = real(obloclat(nob),r_single)
        outdata%pres = real(obpress(nob),r_single)
        outdata%time = real(obtime(nob),r_single)
        outdata%oberrvar_orig = real(oberrvar_orig(nob),r_single)
        outdata%stattype = stattype(nob)
        outdata%obtype = obtype(nob)
        outdata%indxsat = 0
        outdata%assimltd_flag = assimltd_flag(nob)
        if(efsoi_flag) then
           outdata%osense_kin = real(obsense_kin(nob),r_single)
           outdata%osense_dry = real(obsense_dry(nob),r_single)
           outdata%osense_moist = real(obsense_moist(nob),r_single)
        else
           outdata%osense_kin = 9.9e31_r_single
           outdata%osense_dry = 9.9e31_r_single
           outdata%osense_moist = 9.9e31_r_single
        end if
        tmpanal_ob(1:nanals) = real(anal_ob_post(1:nanals,nob),r_single)
        write(iunit) outdata,tmpanal_ob
        if(.not. efsoi_flag) cycle
        ! Sum up
        nob_conv(iobtyp,ireg) = nob_conv(iobtyp,ireg) + 1
        sumsense_conv(iobtyp,ireg,stkin) = sumsense_conv(iobtyp,ireg,stkin) &
             & + obsense_kin(nob)
        sumsense_conv(iobtyp,ireg,stdry) = sumsense_conv(iobtyp,ireg,stdry) &
             & + obsense_dry(nob)
        sumsense_conv(iobtyp,ireg,stmoist) = sumsense_conv(iobtyp,ireg,stmoist) &
             & + obsense_moist(nob)
        if(obsense_kin(nob) < zero) &
             rate_conv(iobtyp,ireg,stkin) = rate_conv(iobtyp,ireg,stkin) + one
        if(obsense_dry(nob) < zero) &
             rate_conv(iobtyp,ireg,stdry) = rate_conv(iobtyp,ireg,stdry) + one
        if(obsense_moist(nob) < zero) &
             rate_conv(iobtyp,ireg,stmoist) = rate_conv(iobtyp,ireg,stmoist) + one
     end do
     ! print out
     if(efsoi_flag) then
        print *,'observation impact for conventional obs'
        print *,'region, obtype, nobs, dJ, positive rate[%]:'
        do iobtyp=1,8
           do ireg=1,3
              if(nob_conv(iobtyp,ireg) > 0) then
                 rate_conv(iobtyp,ireg,1:3) = rate_conv(iobtyp,ireg,1:3) &
                      & / real(nob_conv(iobtyp,ireg),r_kind) * 100._r_kind
                 write(*,'(a,1x,a,i7,3(1x,e12.5),3(1x,f7.2))') charreg(ireg), &
                      & chartype(iobtyp),nob_conv(iobtyp,ireg), &
                      & sumsense_conv(iobtyp,ireg,1:3),rate_conv(iobtyp,ireg,1:3)
              end if
           end do
        end do
     end if
  end if
  ! Statistics for satellite radiances
  if(nobs_sat > 0) then
     ! Initilize arrays
     nob_sat(:) = 0
     sumsense_sat(:,:) = zero
     rate_sat(:,:) = zero
     nn = 0
     ! Loop over each observations
     do nob=nobs_conv+nobs_oz+1,nobs_conv+nobs_oz+nobs_sat
        nn = nn + 1
        nchan=indxsat(nn)
        ! Output individual observation record
        outdata%obfit_prior = real(obfit_prior(nob),r_single)
        outdata%obsprd_prior = real(obsprd_prior(nob),r_single)
        outdata%ensmean_obnobc = real(ensmean_obnobc(nob),r_single)
        outdata%ensmean_ob = real(ensmean_ob(nob),r_single)
        outdata%ob = real(ob(nob),r_single)
        outdata%oberrvar = real(oberrvar(nob),r_single)
        outdata%lon = real(obloclon(nob),r_single)
        outdata%lat = real(obloclat(nob),r_single)
        outdata%pres = real(obpress(nob),r_single)
        outdata%time = real(obtime(nob),r_single)
        outdata%oberrvar_orig = real(oberrvar_orig(nob),r_single)
        outdata%stattype = stattype(nob)
        outdata%obtype = obtype(nob)
        outdata%indxsat = nchan
        outdata%assimltd_flag = assimltd_flag(nob)
        tmpbiaspreds(1:npred+1) = real(biaspreds(1:npred+1,nn),r_single)
        if(efsoi_flag) then
           outdata%osense_kin = real(obsense_kin(nob),r_single)
           outdata%osense_dry = real(obsense_dry(nob),r_single)
           outdata%osense_moist = real(obsense_moist(nob),r_single)
        else
           outdata%osense_kin = 9.9e31_r_single
           outdata%osense_dry = 9.9e31_r_single
           outdata%osense_moist = 9.9e31_r_single
        end if
        tmpanal_ob(1:nanals) = real(anal_ob_post(1:nanals,nob),r_single)
        write(iunit) outdata,tmpanal_ob,tmpbiaspreds
        if(.not. efsoi_flag) cycle
        ! Sum up
        if (oberrvar(nob) < 1.e10_r_kind .and. nchan > 0) then
           nob_sat(nchan) = nob_sat(nchan) + 1
           sumsense_sat(nchan,stkin) = sumsense_sat(nchan,stkin) &
                & + obsense_kin(nob)
           sumsense_sat(nchan,stdry) = sumsense_sat(nchan,stdry) &
                & + obsense_dry(nob)
           sumsense_sat(nchan,stmoist) = sumsense_sat(nchan,stmoist) &
                & + obsense_moist(nob)
           if(obsense_kin(nob) < zero) &
                rate_sat(nchan,stkin) = rate_sat(nchan,stkin) + one
           if(obsense_dry(nob) < zero) &
                rate_sat(nchan,stdry) = rate_sat(nchan,stdry) + one
           if(obsense_moist(nob) < zero) &
                rate_sat(nchan,stmoist) = rate_sat(nchan,stmoist) + one
        end if
     end do
     ! print out
     if(efsoi_flag) then
        print *,'observation impact for satellite brightness temp'
        print *,'instrument, channel #, nobs, dJ, positive rate[%]:'
        do nchan=1,jpch_rad
           if(nob_sat(nchan) > 0) then
              rate_sat(nchan,1:3) = rate_sat(nchan,1:3) &
                   & / real(nob_sat(nchan),r_kind) * 100._r_kind
              write(*,'(a20,1x,i5,i7,3(1x,e12.5),3(1x,f7.2))') &
                   & trim(adjustl(nusis(nchan))), &
                   & nuchan(nchan),nob_sat(nchan),sumsense_sat(nchan,1:3), &
                   & rate_sat(nchan,1:3)
           end if
        end do
     end if
  end if
  close(iunit)
  deallocate(tmpanal_ob,tmpbiaspreds)
  return
end subroutine print_ob_sens

subroutine destroy_ob_sens
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_ob_sens
!   prgmmr: ota
!
! abstract:
!
! program history log:
!   2011-07-20  ota    - created
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  if(allocated(obsense_kin)) deallocate(obsense_kin)
  if(allocated(obsense_dry)) deallocate(obsense_dry)
  if(allocated(obsense_moist)) deallocate(obsense_moist)
  if(allocated(adloc_chunk)) deallocate(adloc_chunk)
  return
end subroutine destroy_ob_sens
end module enkf_obs_sensitivity
