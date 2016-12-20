module radinfo 
!$$$   module documentation block
!                .      .    .                                       .
! module:    radinfo   
!
! abstract:  This module contains variables and routines related
!            to information for the use of satellite radiance data.
!
! program history log:
!   1995-07-06  derber
!   2004-05-13  kleist, documentation
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2004-11-23  treadon - change 110 format statement
!   2004-11-30  xu li   - add array fbias for AVHRR bias correction
!   2004-12-08  xu li   - add logical flag retrieval to module
!   2004-12-22  treadon - rename logical "idiag_rad" as "diag_rad"
!   2005-02-03  xu li   - add SST analysis read and move sub intgrid2 from sst_retrieval to this module
!   2005-03-25  xu li   - modify sub rdgrbsst and remove intgrid2
!   2005-04-18  treadon - make rdgrbsst a separate subroutine
!   2005-09-28  derber  - change radinfo input file add ermax_rad,b_rad and pg_rad
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-04-27  derber  - remove jppf
!   2008-04-23  safford - add standard documentation block
!   2010-04-29  zhu     - add analysis varaince info for radiance bias correction coefficients
!   2010-05-06  zhu     - add option adp_anglebc for variational radiance angle bias correction 
!   2010-05-12  zhu     - add option passive_bc for radiance bias correction for monitored channels
!   2010-07-12  zhu     - add inew_rad
!   2010-10-05  treadon - remove npred1 (not used)
!   2010-10-12  zhu     - combine scaninfo and edgeinfo into one file scaninfo
!   2011-01-04  zhu     - add tlapmean update for new/existing channels when adp_anglebc is turned on
!   2011-04-02  li      - add index tzr_qc,tzr_bufrsave for NSST and QC_tzr
!   2011-07-14  zhu     - add varch_cld for cloudy radiance
!   2012-11-02  collard - add icld_det for greater flexibility in QC step and
!                         add number of scan positions to satang file
!   2012-07-10  sienkiewicz   - add control for SSMIS noise reduction
!   2013-02-19  sienkiewicz   - add adjustable SSMIS bias term weight
!   2013-07-10  zhu     - add option upd_pred for radiance bias update indicator
!   2013-07-19  zhu     - add option emiss_bc for emissivity sensitivity radiance bias predictor
!   2014-04-23   li     - change scan bias correction mode for avhrr and avhrr_navy
!   2014-04-24   li     - apply abs (absolute) to AA and be for safeguarding
!   2015-03-01   li     - add zsea1 & zsea2 to handle the vertical mean temperature based on NSST T-Profile
!   2016-03-10  ejones  - add control for GMI noise reduction
!   2016-03-24  ejones  - add control for AMSR2 noise reduction
!   2016-06-03  Collard - Added changes to allow for historical naming conventions
!   2016-08-12  mahajan - moved nst related variables from radinfo to gsi_nstcouplermod
!
! subroutines included:
!   sub init_rad            - set satellite related variables to defaults
!   sub init_rad_vars       - initialize satellite related variables
!   sub radinfo_read        - read in sat info and biases, including read 
!                             sst_an and avhrr bias correction
!   sub radinfo_write       - write out satellite biases
!
! functions included:
!   fun newchn
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block


! !USES:

  use kinds, only: r_kind,i_kind,r_quad
  use read_diag, only: set_radiag
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_rad
  public :: init_rad_vars
  public :: final_rad_vars
  public :: radinfo_read
  public :: radinfo_write
  public :: angle_cbias
! set passed variables to public
  public :: jpch_rad,npred,b_rad,pg_rad,diag_rad,iuse_rad,nusis,inew_rad
  public :: crtm_coeffs_path,retrieval,predx,ang_rad,newchn,cbias,icld_det
  public :: air_rad,nuchan,numt,varch,varch_cld,fbias,ermax_rad,tlapmean
  public :: ifactq,mype_rad
  public :: ostats,rstats,varA
  public :: adp_anglebc,angord,use_edges, maxscan
  public :: emiss_bc
  public :: passive_bc
  public :: upd_pred
  public :: ssmis_method,gmi_method,amsr2_method
  public :: radstart,radstep
  public :: newpc4pred
  public :: biaspredvar
  public :: radjacnames,radjacindxs,nsigradjac
  public :: tzr_bufrsave,tzr_qc

  public :: radedge1, radedge2
  public :: ssmis_precond
  public :: radinfo_adjust_jacobian
  public :: radinfo_get_rsqrtinv

  integer(i_kind),parameter:: numt = 33   ! size of AVHRR bias correction file
  integer(i_kind),parameter:: ntlapthresh = 100 ! threshhold value of cycles if tlapmean update is needed

  logical diag_rad    ! logical to turn off or on the diagnostic radiance file (true=on)
  logical retrieval   ! logical to turn off or on the SST retrieval with AVHRR data
  logical tzr_bufrsave! logical to turn off or on the bufr file output for Tz retrieval (true=on)

  logical adp_anglebc ! logical to turn off or on the variational radiance angle bias correction
  logical emiss_bc    ! logical to turn off or on the emissivity predictor
  logical passive_bc  ! logical to turn off or on radiance bias correction for monitored channels
  logical use_edges   ! logical to use data on scan edges (.true.=to use)

  integer(i_kind) tzr_qc        ! indicator of Tz retrieval QC tzr
  integer(i_kind) ssmis_method  !  noise reduction method for SSMIS
  integer(i_kind) gmi_method    !  noise reduction method for GMI
  integer(i_kind) amsr2_method  !  noise reduction method for AMSR2

  integer(i_kind) jpch_rad      ! number of channels*sat
  integer(i_kind) npred         ! number of radiance biases predictors
  integer(i_kind) mype_rad      ! task id for writing out radiance diagnostics
  integer(i_kind) angord        ! order of polynomial for angle bias correction
  integer(i_kind) maxscan       ! number of scan positions in satang file 

  real(r_kind),dimension(20):: upd_pred  ! indicator if bias correction coefficients evolve
  real(r_kind) :: ssmis_precond

  real(r_kind),allocatable,dimension(:):: varch       ! variance for clear radiance each satellite channel
  real(r_kind),allocatable,dimension(:):: varch_cld   ! variance for cloudy radiance
  real(r_kind),allocatable,dimension(:):: ermax_rad   ! error maximum (qc)
  real(r_kind),allocatable,dimension(:):: b_rad       ! variational b value
  real(r_kind),allocatable,dimension(:):: pg_rad      ! variational pg value
  real(r_kind),allocatable,dimension(:):: tlapmean    ! mean lapse rate (fixed from input file)
  real(r_kind),allocatable,dimension(:):: tsum_tlapmean  ! err sum of mean lapse rate 
  real(r_kind),allocatable,dimension(:):: ang_rad     ! 0 or 1 depending on iuse_rad (1 - use angle bias correction)
  real(r_kind),allocatable,dimension(:):: air_rad     ! 0 or 1 depending on iuse_rad (1 - use air mass bias correction)          
  real(r_kind),allocatable,dimension(:,:):: fbias     ! bias for AVHRR siumulated radiance
  real(r_kind),allocatable,dimension(:,:):: cbias     ! angle dependent bias for satellite channels
  real(r_kind),allocatable,dimension(:,:):: predx     ! coefficients for predictor part of bias correction

  real(r_kind),allocatable,dimension(:,:):: varA
  real(r_kind),allocatable,dimension(:):: ostats
  real(r_quad),allocatable,dimension(:,:):: rstats

  real(r_kind),allocatable,dimension(:):: radstart    ! starting scan angle
  real(r_kind),allocatable,dimension(:):: radstep     ! step of scan angle
  integer(i_kind),allocatable,dimension(:):: radnstep    ! nstep of scan angle

  integer(i_kind),allocatable,dimension(:):: radedge1    ! cut-off of edge removal
  integer(i_kind),allocatable,dimension(:):: radedge2    ! cut-off of edge removal

  integer(i_kind),allocatable,dimension(:):: count_tlapmean ! the count of tlapmean update 

  integer(i_kind),allocatable,dimension(:):: nuchan    ! satellite channel
  integer(i_kind),allocatable,dimension(:):: iuse_rad  ! use to turn off satellite radiance data
!                                                    = -2 do not use
!                                                    = -1 monitor if diagnostics produced
!                                                    =  0 monitor and use in QC only
!                                                    =  1 use data with complete quality control
!                                                    =  2 use data with no airmass bias correction
!                                                    =  3 use data with no angle dependent bias correction
!                                                    =  4 use data with no bias correction
  integer(i_kind),allocatable,dimension(:):: icld_det  ! Use this channel in cloud detection (only used for
!                                                        certain instruments. Set to greater than zero to use

  logical,allocatable,dimension(:):: inew_rad  ! indicator if it needs initialized for satellite radiance data
  logical,allocatable,dimension(:):: update_tlapmean ! indicator if tlapmean update is needed

  integer(i_kind),allocatable,dimension(:):: ifactq    ! scaling parameter for d(Tb)/dq sensitivity

  character(len=20),allocatable,dimension(:):: nusis   ! sensor/instrument/satellite indicator
  character(len=256),save:: crtm_coeffs_path = "./" ! path of CRTM_Coeffs files

  integer(i_kind) :: nsigradjac
  character(len=20),allocatable,dimension(:):: radjacnames
  integer(i_kind),  allocatable,dimension(:):: radjacindxs

  real(r_kind) :: biaspredvar
  logical,save :: newpc4pred ! controls preconditioning due to sat-bias correction term 

  interface radinfo_adjust_jacobian; module procedure adjust_jac_; end interface
  interface radinfo_get_rsqrtinv; module procedure get_rsqrtinv_; end interface

  character(len=*),parameter :: myname='radinfo'
contains


  subroutine init_rad
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    int_rad
!
!   prgrmmr:     derber      org: np23                date: 1995-07-06
!
! abstract:  This routine sets default values for variables used in
!            the radiance processing routines.
!
! program history log:
!   1995-07-06  derber
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2008-04-23  safford - add standard subprogram doc block
!   2010-05-06  zhu     - add adp_anglebc and angord
!   2010-05-12  zhu     - add passive_bc
!   2010-09-02  zhu     - add use_edges
!   2010-04-25  zhu     - add logical newpc4pred (todling move here)
!   2013-02-13  eliu    - add two additional bias correction predictors for SSMIS 
!   2013-07-19  zhu     - add emiss_bc for emissivity sensitivity bias predictor
!   2016-03-10  ejones  - add gmi_method for using ssmis spatial averaging code
!                         for gmi
!   2016-03-24  ejones  - add amsr2_method for using ssmis spatial averaging code
!                         for amsr2
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block
    use constants, only: one_tenth, one, r0_01

    implicit none

    biaspredvar = one_tenth ! berror var for radiance bias coeffs
    jpch_rad = 0            ! total number of channels over all instruments & satellites
    retrieval = .false.     ! .true. = apply physical SST retrieval with AVHRR data
    diag_rad = .true.       ! .true.=generate radiance diagnostic file
    mype_rad = 0            ! mpi task to collect and print radiance use information on/from
    npred=7                 ! number of bias correction predictors
    tzr_qc = 0              ! 0 = no Tz ret in gsi; 1 = retrieve and applied to QC
    tzr_bufrsave = .false.  ! .true.=generate bufr file for Tz retrieval

    passive_bc = .false.  ! .true.=turn on bias correction for monitored channels
    adp_anglebc = .false. ! .true.=turn on angle bias correction
    emiss_bc = .false.    ! .true.=turn on emissivity bias correction
    angord = 0            ! order of polynomial for angle bias correction
    use_edges = .true.    ! .true.=to use data on scan edges
    upd_pred = one        ! 1.0=bias correction coefficients evolve
    ssmis_method = 1      ! default ssmis smoothing method
    ssmis_precond = r0_01 ! default preconditioner for ssmis bias terms
    gmi_method = 0        ! 4= default gmi smoothing method
    amsr2_method = 0      ! 5= default amsr2 smoothing method
  end subroutine init_rad


  subroutine init_rad_vars
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_rad_vars
!
!   prgrmmr:     derber      org: np23                date: 1995-07-06
!
! abstract:  This routine sets parameters used in the radiance 
!            assimilation.  The parameters below depend on values
!            which may be altered by the SETUP namelist.
!
! program history log:
!   1995-07-06  derber
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2008-04-23  safford -- add standard subprogram doc block
!   2010-05-06  zhu     - add option adp_anglebc
!   2011-05-16  todling - generalized fields in rad-Jacobian
!   2012-04-07  todling - add handle for aerosols
!   2013-10-26  todling - revisit given that metguess now holds upper air
!   2013-11-21  todling - add set_radiag; should be revisited to accommodate all
!                         versions of diag-file, but perhaps done somewhere else
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    use mpimod, only: mype
    use gsi_metguess_mod, only: gsi_metguess_get
    use gsi_chemguess_mod, only: gsi_chemguess_get
    use gridmod, only: nsig
    implicit none

    integer(i_kind) ii,jj,mxlvs,isum,ndim,ib,ie,ier
    integer(i_kind) nvarjac,n_meteo,n_clouds,n_aeros
    integer(i_kind),allocatable,dimension(:)::aux,all_levels
    character(len=20),allocatable,dimension(:)::meteo_names
    character(len=20),allocatable,dimension(:)::clouds_names
    character(len=20),allocatable,dimension(:)::aeros_names

!   the following vars are wired-in until MetGuess handles all guess fiedls
!   character(len=3),parameter :: wirednames(6) = (/ 'tv ','q  ','oz ', 'u  ', 'v  ', 'sst' /)
    character(len=3),parameter :: wirednames(1) = (/ 'sst' /)
    integer(i_kind) ,parameter :: wiredlevs (1) = (/ 1 /)

!   safeguard angord value for option adp_anglebc
    if (adp_anglebc) then 
       if (angord==0) then 
          write(6,*)'INIT_RAD_VARS:  ***ERROR*** error value for angord, reset angord to be 4' 
          angord=4
       end if
    else
       if (angord/=0) angord=0
    end if

    call set_radiag ('version',30303,ier)
    if (adp_anglebc) npred=npred+angord
    if (emiss_bc) then
        npred=npred+1
        call set_radiag ('version',30303,ier)
    endif
    
!   inquire about variables in guess
    mxlvs = 0
    call gsi_metguess_get ( 'dim', ndim, ier )
    if (ndim>0) then
       allocate(all_levels(ndim))
       call gsi_metguess_get ( 'guesses_level', all_levels, ier )
       mxlvs = maxval(all_levels)
       deallocate(all_levels)
    end if

!   Test to ensure that mxlvs == nsig
    if (mxlvs/=nsig)then
       if(mype==0) write(6,*) 'INIT_RAD_VARS:  ***WARNING*** mxlvs from the anavinfo file',mxlvs, &
                      'is different from that of the guess',nsig,'.  Resetting maxlvs to match NSIG from guess.'
       mxlvs=nsig
    endif
!   inquire number of meteorology fields to participate in CRTM calculations
    call gsi_metguess_get ( 'meteo_4crtm_jac::3d', n_meteo, ier )
    n_meteo=max(0,n_meteo)
    allocate(meteo_names(n_meteo))
    if (n_meteo>0) then
        call gsi_metguess_get ( 'meteo_4crtm_jac::3d', meteo_names, ier )
    endif

!   inquire number of clouds to participate in CRTM calculations
    call gsi_metguess_get ( 'clouds_4crtm_jac::3d', n_clouds, ier )
    n_clouds=max(0,n_clouds)

!   inquire number of aerosols to participate in CRTM calculations
    call gsi_chemguess_get ( 'aerosols_4crtm_jac::3d', n_aeros, ier )
    n_aeros=max(0,n_aeros)

    nvarjac=size(wirednames)+n_meteo+n_clouds+n_aeros
    allocate(radjacnames(nvarjac))
    allocate(radjacindxs(nvarjac))

!   Fill in with wired names first
    do ii=1,size(wirednames)
       radjacnames(ii) = trim(wirednames(ii))
       radjacindxs(ii) = wiredlevs(ii)
    enddo
!   Fill in meteo next 
    jj=0
    if (n_meteo>0) then
       ib=size(wirednames)+1
       ie=ib+n_meteo-1
       do ii=ib,ie
          jj=jj+1
          radjacnames(ii) = trim(meteo_names(jj))
          radjacindxs(ii) = mxlvs
       enddo
    endif
!   Fill in clouds next 
    jj=0
    if (n_clouds>0) then
       allocate(clouds_names(n_clouds))
       call gsi_metguess_get ( 'clouds_4crtm_jac::3d', clouds_names, ier )
       ib=size(wirednames)+n_meteo+1
       ie=ib+n_clouds-1
       do ii=ib,ie
          jj=jj+1
          radjacnames(ii) = trim(clouds_names(jj))
          radjacindxs(ii) = mxlvs
       enddo
       deallocate(clouds_names)
    endif
!   Fill in aerosols next 
    jj=0
    if (n_aeros>0) then
        allocate(aeros_names(n_aeros))
        call gsi_chemguess_get ( 'aerosols_4crtm_jac::3d', aeros_names, ier )
       ib=size(wirednames)+n_meteo+n_clouds+1
       ie=ib+n_aeros-1
       do ii=ib,ie
          jj=jj+1
          radjacnames(ii) = trim(aeros_names(jj))
          radjacindxs(ii) = mxlvs
       enddo
       deallocate(aeros_names)
    endif

!   Overwrite levels for certain fields (this must be revisited)
    do ii=1,nvarjac
       if(trim(radjacnames(ii))=='u'.or.trim(radjacnames(ii))=='v'.or.trim(radjacnames(ii))=='sst') then
          radjacindxs(ii) = 1
        endif
    enddo

!   Determine initial pointer location for each var in the Jacobian
    allocate(aux(nvarjac))
    if(size(radjacnames)>0) then
       nsigradjac = sum(radjacindxs)
       isum=0
       do ii=2,nvarjac
          isum=isum+radjacindxs(ii-1)
          aux(ii) = isum
       enddo
       aux(1) = 0
       radjacindxs=aux
    endif
    deallocate(aux)
    deallocate(meteo_names)

    if(mype==0) then
      print*, 'Vars in Rad-Jacobian (dims)'
      print*, '--------------------------'
      do ii=1,nvarjac
         print*, radjacnames(ii), radjacindxs(ii)
      end do
    endif

    return
  end subroutine init_rad_vars

  subroutine final_rad_vars
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    final_rad_vars
!
!   prgrmmr:     todling     org: np23                date: 2011-05-16
!
! abstract:  This routine finalizes this package
!
! program history log:
!   2011-05-16  todling
!   2014-04-13  todling - add call to finalize correlated R related quantities
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    use correlated_obsmod, only: corr_ob_finalize
    implicit none

    call corr_ob_finalize
    if(allocated(radjacindxs)) deallocate(radjacindxs)
    if(allocated(radjacnames)) deallocate(radjacnames)

    return
  end subroutine final_rad_vars


  subroutine radinfo_read
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radinfo_read
!
!   prgrmmr:     yang        org: np20                date: 1998-05-15
!
! abstract:  This routine reads the satinfo, satbias\_angle, and
!            satbias files.  
!
!            The satinfo file contains information about the channels,
!            sensors, and satellites.  It specifies observation error
!            for the channels, how to use the channels (assimilate,
!            monitor, etc), the type of channel (ir or microwave),
!            and other useful information.  
!
!            The satbias\_angle file contains the angle dependent part
!            of the brightness temperature bias for each channel/
!            instrument/satellite.  Also included in this file is 
!            the mean temperature lapse rate for each channels 
!            weighted by the weighting function for the given channel/
!            instrument.
!
!            The satbias\_in file contains the coefficients for the
!            predictive part of the bias correction.
!
!
! program history log:
!   1998-05-15  yang, weiyu
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2004-11-30  xu li- read SST dependent bias for AVHRR radiance (NOAA-16 & NOAA-17) 
!                      and SST analysis when retrieval = .true.
!   2005-02-08  xu li- read SST analysis when retrieval = .true.
!   2005-10-11  treadon - change satinfo read to free format
!   2005-11-30  li - fix a bug in the format to read avhrr SST dependent BC
!   2007-03-13  derber  - modify to allow input bias correction files of different lengths and orders
!   2007-06-29  treadon - determine/build n_sensors and sensorlist from satinfo file
!   2008-04-23  safford - add standard doc block, rm unused vars and uses
!   2010-04-29  zhu     - add analysis varaince info for radiance bias correction coefficients
!   2010-05-06  zhu     - add option adp_anglebc for variational angle bias correction
!   2011-01-04  zhu     - add tlapmean update for new channels when adp_anglebc is turned on
!   2011-04-07  todling - adjust argument list (interface) since newpc4pred is local now
!   2013-01-26  parrish - fix bug caught by WCOSS debug compile -- tlapmean used before allocated.
!                          Move first use of tlapmean to after allocation.
!   2013-02-13  eliu    - change write-out format for iout_rad (for two
!                         additional SSMIS bias correction coefficients)
!   2013-05-14  guo     - add read error messages to alarm user a format change.
!   2014-04-13  todling - add initialization of correlated R-covariance
!   2016-07-14  jung    - mods to make SEVIRI channel numbers consistent with other instruments.
!   2016-07-19  W. Gu   - update the obs error in satinfo for instruments accounted for the correlated R-covariance
!   2016-07-19  W. Gu   - add the hook to scale the bias correction term for inter-channel correlated obs errors.
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:
    use correlated_obsmod, only: corr_ob_initialize, corr_oberr_qc
    use obsmod, only: iout_rad
    use constants, only: zero,one,zero_quad
    use mpimod, only: mype
    use mpeu_util, only: perr,die
    implicit none

! !INPUT PARAMETERS:


    integer(i_kind) i,j,k,ich,lunin,lunout,nlines
    integer(i_kind) ip,istat,n,ichan,nstep,edge1,edge2,ntlapupdate
    real(r_kind),dimension(npred):: predr
    real(r_kind) tlapm
    real(r_kind) tsum
    real(r_kind) ostatsx
    real(r_kind) start,step
    real(r_kind),allocatable::cbiasx(:)
    real(r_kind),dimension(npred)::varx
    character(len=1):: cflg
    character(len=6) :: word
    character(len=120) crecord
    character(len=20) :: isis 
    character(len=20) :: satscan_sis
    real(r_kind),dimension(numt):: fbiasx     ! contains SST dependent bias  for SST retrieval
    integer(i_kind) :: nuchan_temp    ! satellite channel
    character(len=20) :: nusis_temp   ! sensor/instrument/satellite indicator
    integer(i_kind) :: iuse_rad_temp  ! use to turn off satellite radiance data
    real(r_kind) :: varch_temp        ! variance for clear radiance each satellite channel
    real(r_kind) :: varch_cld_temp    ! variance for cloudy radiance
    real(r_kind) :: ermax_rad_temp    ! error maximum (qc)
    real(r_kind) :: b_rad_temp        ! variational b value
    real(r_kind) :: pg_rad_temp       ! variational pg value
    integer(i_kind) :: icld_det_temp  ! Use this channel in cloud detection (only used for
!                                                        certain instruments. Set to greater than zero to use
    logical,allocatable,dimension(:):: nfound
    logical cfound
    logical pcexist
    logical cold_start_seviri         ! flag to fix wrong channel numbers for seviri.  True = fix, false = already correct

    data lunin / 49 /
    data lunout / 51 /

!============================================================================

!   Determine number of entries in satellite information file
    open(lunin,file='satinfo',form='formatted')
    j=0
    nlines=0
    read1:  do
       read(lunin,100,iostat=istat) cflg,crecord
       if (istat /= 0) exit
       nlines=nlines+1
       if (cflg == '!') cycle
       read(crecord,*,iostat=istat) nusis_temp,nuchan_temp,iuse_rad_temp,&
            varch_temp,varch_cld_temp,ermax_rad_temp,b_rad_temp,pg_rad_temp,icld_det_temp  
       if ( .not. diag_rad .and. iuse_rad_temp < 0 .and. icld_det_temp < 0 .and. &
          ( nusis_temp(1:4) == 'cris' .or. nusis_temp(1:4) == 'iasi' .or. nusis_temp(1:4) == 'airs')) cycle
       if ( nusis_temp(1:6) == 'seviri' .and. (nuchan_temp == 1 .or. nuchan_temp == 2 .or. nuchan_temp == 3)) then
          write(6,*) 'RADINFO_READ:  *** ERROR **** This is an obsolete satinfo file '
          write(6,*) 'RADINFO_READ:  Use an updated file or change the SEVIRI channels from 1-8 to 4-11'
          write(6,*) 'RADINFO_READ:  Your bias correction file(s) may also have the wrong channel numbers.'
          write(6,*) 'RADINFO_READ:  You can either edit them to the correct channel numbers (4-11) '
          write(6,*) 'RADINFO_READ:  or the GSI will perform a cold start on the seviri entries.' 
          write(6,*) 'RADINFO_READ:  stop program execution' 
          call stop2(79)
       endif
       j=j+1
    end do read1
    if (istat>0) then
       close(lunin)
       write(6,*)'RADINFO_READ:  ***ERROR*** error reading satinfo, istat=',istat
       write(6,*)'RADINFO_READ:  stop program execution'
       call stop2(79)
    endif
    jpch_rad = j
    if(jpch_rad == 0)then
      close(lunin)
      return
    end if


!   Allocate arrays to hold radiance information
!     nuchan    - channel number
!     nusis     - sensor/instrument/satellite
!     iuse_rad  - use parameter
!     ifactq    - scaling parameter for d(Tb)/dq sensitivity
!     varch     - variance for clear radiance for each channel
!     varch_cld - variance for cloudy radiance for each channel

    allocate(nuchan(jpch_rad),nusis(jpch_rad),iuse_rad(0:jpch_rad), &
         ifactq(jpch_rad),varch(jpch_rad),varch_cld(jpch_rad), &
         ermax_rad(jpch_rad),b_rad(jpch_rad),pg_rad(jpch_rad), &
         ang_rad(jpch_rad),air_rad(jpch_rad),inew_rad(jpch_rad),&
         icld_det(jpch_rad)) 

    allocate(nfound(jpch_rad))

    iuse_rad(0)=-999
    inew_rad=.true.
    ifactq=15
    air_rad=one
    ang_rad=one

!   All mpi tasks open and read radiance information file.
!   Task mype_rad writes information to radiance runtime file

    if (mype==mype_rad) then
       open(iout_rad)
       write(iout_rad,120) jpch_rad
120    format('RADINFO_READ:  jpch_rad=',1x,i6)
    endif
    rewind(lunin)
    j=1
    do k=1,nlines
       read(lunin,100) cflg,crecord
       if (cflg == '!') cycle
       read(crecord,*,iostat=istat) nusis(j),nuchan(j),iuse_rad(j),&
            varch(j),varch_cld(j),ermax_rad(j),b_rad(j),pg_rad(j),icld_det(j) 
             
       ! The following is to sort out some historical naming conventions
       select case (nusis(j)(1:4))
         case ('airs')
            nusis(j)='airs_aqua'
         case ('iasi')
            if (index(nusis(j),'metop-a') /= 0) nusis(j)='iasi_metop-a'
            if (index(nusis(j),'metop-b') /= 0) nusis(j)='iasi_metop-b'
            if (index(nusis(j),'metop-c') /= 0) nusis(j)='iasi_metop-c'
       end select 

       if(istat/=0) then
          call perr('radinfo_read','read(crecord), crecord =',trim(crecord))
          call perr('radinfo_read','                 istat =',istat)
          call  die('radinfo_read')
       endif

       if ( .not. diag_rad .and. iuse_rad(j) < 0 .and. icld_det(j) < 0 .and. &
          ( nusis(j)(1:4) == 'cris' .or. nusis(j)(1:4) == 'iasi' .or. nusis(j)(1:4) == 'airs')) cycle

       if(iuse_rad(j) == 4 .or. iuse_rad(j) == 2) air_rad(j)=zero
       if(iuse_rad(j) == 4 .or. iuse_rad(j) == 3) ang_rad(j)=zero

       if (mype==mype_rad) write(iout_rad,110) j,nusis(j), &
            nuchan(j),varch(j),varch_cld(j),iuse_rad(j),ermax_rad(j), &
            b_rad(j),pg_rad(j),icld_det(j) 

       j=j+1
    end do
    close(lunin)
100 format(a1,a120)
110 format(i4,1x,a20,' chan= ',i4,  &
          ' var= ',f7.3,' varch_cld=',f7.3,' use= ',i2,' ermax= ',F7.3, &
          ' b_rad= ',F7.2,' pg_rad=',F7.2,' icld_det=',I2) 

!   Allocate arrays for additional preconditioning info
!   Read in information for data number and preconditioning

    if (newpc4pred) then
       allocate(ostats(jpch_rad), rstats(npred,jpch_rad),varA(npred,jpch_rad))
       varA = zero
       ostats = zero
       rstats = zero_quad

       inquire(file='satbias_pc',exist=pcexist)
       if (pcexist) then
          open(lunin,file='satbias_pc',form='formatted')
          nfound = .false.
          cold_start_seviri = .false.
          read3: do
             read(lunin,'(I5,1x,A20,1x,I5,e15.7/2(4x,10e15.7/))',iostat=istat) &
                  ich,isis,ichan,ostatsx,(varx(ip),ip=1,npred)
             if (istat/=0) exit
             ! The following is to sort out some historical naming conventions
             select case (isis(1:4))
               case ('airs')
                  isis='airs_aqua'
               case ('iasi')
                  if (index(isis,'metop-a') /= 0) isis='iasi_metop-a'
                  if (index(isis,'metop-b') /= 0) isis='iasi_metop-b'
                  if (index(isis,'metop-c') /= 0) isis='iasi_metop-c'
             end select 
             cfound = .false.

!            Set flag to fix seviri channel mismatch.  Channels should start at 4.  If it is
!            wrong, set falg to zero out entries.
             if( isis(1:6) == 'seviri' .and. ichan < 4 ) cold_start_seviri = .true.

!            If not seviri or seviri channels are correct, proceed. 
             if( .not. cold_start_seviri .or. isis(1:6) /= 'seviri' ) then
                do j =1,jpch_rad
                   if(trim(isis) == trim(nusis(j)) .and. ichan == nuchan(j))then
                      cfound = .true.
                      nfound(j) = .true.
                      do i=1,npred
                         varA(i,j)=varx(i)
                      end do
                      ostats(j)=ostatsx
                      if (any(varx/=zero) .and. iuse_rad(j)>-2) inew_rad(j)=.false.
                      cycle read3
                   end if
                end do
             endif

!            If an entry exists in the satbias_pc file but not in the satinfo file, print an error message.
!            When the diag file is not wanted (diag_rad=.false.),the subset feature in the CRTM is used. 
!            A lot of airs, iasi and/or cris satbias_pc - satinfo entry mismatchs occur which the warning messages are not wanted.
!            The second part of the if statement keeps from printing them.
             if ( .not. cfound ) then
                if ((diag_rad .and. mype ==0) .or. &
                   (.not. diag_rad .and. isis(1:4)/='airs' .and. isis(1:4) /= 'cris' .and. isis(1:4) /= 'iasi')) &
                   write(6,*) '***WARNING instrument/channel ',isis,ichan,'found in satbias_pc file but not found in satinfo'
             endif

          end do read3
          close(lunin)
          if (istat>0) then
             write(6,*)'RADINFO_READ:  ***ERROR*** error reading satbias_pc, istat=',istat
          endif

          if (mype==mype_rad) then
             write(iout_rad,*)'RADINFO_READ:  read satbias_pc file'
             do j=1,jpch_rad
                if(.not. nfound(j))write(iout_rad,*) 'RADINFO_READ: ***WARNING instrument/channel ',&
                     nusis(j),nuchan(j),' not found in satbias_pc file - set to zero '
             end do
          end if
       else
          if (mype==mype_rad) then
             write(iout_rad,*)'RADINFO_READ:  satbias_pc file doesnot exist - set to zero'
          end if
       end if
    end if   ! end newpc4pred


!   Allocate arrays to receive angle dependent bias information.
!   Open file to bias file (satang=satbias_angle).  Read data.

    if (adp_anglebc) then 
       allocate(count_tlapmean(jpch_rad),update_tlapmean(jpch_rad),tsum_tlapmean(jpch_rad))
       count_tlapmean=0
       tsum_tlapmean=zero
       update_tlapmean=.true.
    end if

    maxscan=90  ! Default value for old files
    allocate(cbiasx(maxscan))
    allocate(cbias(maxscan,jpch_rad),tlapmean(jpch_rad))
    cbias=zero
    tlapmean=zero
    if (.not. adp_anglebc) then
       open(lunin,file='satbias_angle',form='formatted',status='old',iostat=istat)
       if (istat /= 0 ) then
          write(6,*)'RADINFO_READ:  ***ERROR*** file "satbias_angle" does not exist'
          call stop2(79)
       endif

       nfound = .false.
       word=""
       read(lunin,'(a6)',iostat=istat) word
       if (istat /= 0 ) then
          write(6,*)'RADINFO_READ:  ***ERROR*** file "satbias_angle" is empty'
          call stop2(79)
       endif

       rewind(lunin)
       if (word == 'nscan=') read(lunin,'(6x,i8)',iostat=istat) maxscan
       if (istat /= 0 .OR. maxscan <= 0 .OR. maxscan > 1000) then
          write(6,*)'RADINFO_READ:  ***ERROR*** error reading satbias_angle, maxscan out of range: ',maxscan
          call stop2(79)
       endif
       read2: do
          read(lunin,'(I5,1x,A20,2x,I4,e15.6)',iostat=istat,end=1111) &
               ich,isis,ichan,tlapm
          if (istat /= 0) exit
          read(lunin,'(100(4x,10f7.3/))',iostat=istat,end=1111) &
               (cbiasx(ip),ip=1,maxscan)
          if (istat /= 0) exit
          cfound = .false.
          ! The following is to sort out some historical naming conventions
          select case (isis(1:4))
             case ('airs')
               isis='airs_aqua'
             case ('iasi')
               if (index(isis,'metop-a') /= 0) isis='iasi_metop-a'
               if (index(isis,'metop-b') /= 0) isis='iasi_metop-b'
               if (index(isis,'metop-c') /= 0) isis='iasi_metop-c'
          end select 
          do j =1,jpch_rad
             if(trim(isis) == trim(nusis(j)) .and. ichan == nuchan(j))then
                cfound = .true.
                nfound(j) = .true.
                do i=1,maxscan
                   cbias(i,j)=cbiasx(i)
                end do
                tlapmean(j)=tlapm
                cycle read2
             end if
          end do
          if(.not. cfound .and. mype == 0) &
               write(6,*) '***WARNING instrument/channel ',isis,ichan, &
               'found in satbias_angle file but not found in satinfo'
       end do read2
1111   continue
       close(lunin)
       if (istat>0) then
          write(6,*)'RADINFO_READ:  ***ERROR*** error reading satbias_angle, istat=',istat
          write(6,*)'RADINFO_READ:  stop program execution'
          call stop2(79)
       endif

       if (mype==mype_rad) then
          write(iout_rad,*)'RADINFO_READ:  read satbias_angle file'
          do j=1,jpch_rad
             if(.not. nfound(j))write(iout_rad,*) 'RADINFO_READ: ***WARNING instrument/channel ',&
                  nusis(j),nuchan(j),' not found in satbias_angle file - set to zero '
          end do
       end if
    end if ! end of .not.adp_anglebc

!   Read in start,step information and cutoff values for scan edges
    allocate(radstart(jpch_rad),radstep(jpch_rad),radnstep(jpch_rad))
    allocate(radedge1(jpch_rad),radedge2(jpch_rad))
    radstart=zero
    radstep =one
    radnstep=maxscan
    radedge1=-1
    radedge2=-1

    inquire(file='scaninfo',exist=pcexist)
    if (pcexist) then
       open(lunin,file='scaninfo',form='formatted')
       do
          read(lunin,1000,IOSTAT=istat,end=1222) cflg,satscan_sis,start,step,nstep,edge1,edge2
          if (istat /= 0) exit
          if (cflg == '!') cycle

          ! The following is to sort out some historical naming conventions
          select case (satscan_sis(1:4))
             case ('airs')
               satscan_sis='airs_aqua'
             case ('iasi')
               if (index(satscan_sis,'metop-a') /= 0) satscan_sis='iasi_metop-a'
               if (index(satscan_sis,'metop-b') /= 0) satscan_sis='iasi_metop-b'
               if (index(satscan_sis,'metop-c') /= 0) satscan_sis='iasi_metop-c'
          end select 

          do j =1,jpch_rad
             if(trim(satscan_sis) == trim(nusis(j)))then
                radstart(j)=start
                radstep(j)=step
                radnstep(j)=nstep
                radedge1(j)=edge1
                radedge2(j)=edge2
             end if
          end do
       end do
1000   format(a1,a20,2f11.3,i10,2i6)
1222   continue
       close(lunin)
    else
       if(mype == 0) write(6,*) '***WARNING file scaninfo not found, use default'

       do j =1,jpch_rad
          call satstep(nusis(j),start,step,nstep,edge1,edge2)
          radstart(j)=start
          radstep(j)=step
          radnstep(j)=nstep
          radedge1(j)=edge1
          radedge2(j)=edge2
       end do
    end if  ! if pcexist


    if ( .not. retrieval ) then

!   Allocate array to hold coefficients for predictive (air mass) part of 
!   bias correction.  Open unit to input file.  Read data.
       allocate(predx(npred,jpch_rad))
       do j=1,jpch_rad
          do i=1,npred
             predx(i,j)=zero
          end do
       end do

       open(lunin,file='satbias_in' ,form='formatted')
       nfound = .false.
       cold_start_seviri = .false.
       read4: do
          if (.not. adp_anglebc) then
             read(lunin,'(I5,1x,A20,1x,I5,10f12.6)',iostat=istat,end=1333) ich,isis,&
                  ichan,(predr(ip),ip=1,npred)
          else
             read(lunin,'(I5,1x,A20,1x,I5,2e15.6,1x,I5/2(4x,10f12.6/))',iostat=istat,end=1333) ich,isis,&
                  ichan,tlapm,tsum,ntlapupdate,(predr(ip),ip=1,npred)
          end if
          if (istat /= 0) exit
          cfound = .false.
          ! The following is to sort out some historical naming conventions
          select case (isis(1:4))
             case ('airs')
               isis='airs_aqua'
             case ('iasi')
               if (index(isis,'metop-a') /= 0) isis='iasi_metop-a'
               if (index(isis,'metop-b') /= 0) isis='iasi_metop-b'
               if (index(isis,'metop-c') /= 0) isis='iasi_metop-c'
          end select 

!         Set flag to fix seviri channel mismatch.  Channels should start at 4.  If it is
!         wrong, set falg to zero out entries.
          if( isis(1:6) == 'seviri' .and. ichan < 4 ) cold_start_seviri = .true.

!         If not seviri or seviri channels are correct, proceed.
          if(  .not. cold_start_seviri .or. isis(1:6) /= 'seviri' ) then
             do j =1,jpch_rad
                if(trim(isis) == trim(nusis(j)) .and. ichan == nuchan(j))then
                   cfound = .true.
                   nfound(j) = .true.
                   do i=1,npred
                      predx(i,j)=predr(i)
                   end do
                   if (adp_anglebc) then 
                      tlapmean(j)=tlapm
                      tsum_tlapmean(j)=tsum
                      count_tlapmean(j)=ntlapupdate
                      if (ntlapupdate > ntlapthresh) update_tlapmean(j)=.false.
                   end if
                   if (any(predr/=zero)) inew_rad(j)=.false.
                   cycle read4
                end if
             end do
          endif

!         If an entry exists in the satbias_in file but not in the satinfo file, print an error message.
!         When the diag file is not wanted (diag_rad=.false.),the subset feature in the CRTM is used. 
!         A lot of airs, iasi and/or cris satbias_in - satinfo entry mismatchs occur which the warning messages are not wanted.
!         The second part of the if statement keeps from printing them.
          if ( .not. cfound ) then
             if ((diag_rad .and. mype ==0) .or. &
                (.not. diag_rad .and. isis(1:4)/='airs' .and. isis(1:4) /= 'cris' .and. isis(1:4) /= 'iasi')) &
                write(6,*) '***WARNING instrument/channel ',isis,ichan,'found in satbias_in file but not found in satinfo'
          endif

       end do read4
1333   continue
       close(lunin)
       if (istat>0) then
          write(6,*)'RADINFO_READ:  ***ERROR*** error reading satbias_in, istat=',istat
          write(6,*)'RADINFO_READ:  stop program execution'
          call stop2(79)
       endif

       if (mype==mype_rad) then
          write(iout_rad,*)'RADINFO_READ:  guess air mass bias correction coefficients below'
          do j=1,jpch_rad
             if (nfound(j)) then
                write(iout_rad,140) j,trim(nusis(j)),(predx(n,j),n=1,npred)
             else
                write(iout_rad,*) '***WARNING instrument/channel ',&
                nusis(j),nuchan(j),' not found in satbias_in file - set to zero '
             endif
          end do
140       format(i4,1x,a20,12f12.6)

       endif


!      Initialize predx if inew_rad and compute angle bias correction and tlapmean
       if (adp_anglebc) then
          call init_predx
          do j=1,jpch_rad
             call angle_cbias(nusis(j),j,cbias(1,j))
          end do

!         check inew_rad again
          do j =1,jpch_rad
             if (inew_rad(j) .and. iuse_rad(j)>=0 .and. all(predx(:,j)==zero)) then
                iuse_rad(j)=-1
             end if
          end do

          if (mype==mype_rad) then
             open(lunout,file='satbias_ang.out',form='formatted')
             write(lunout,'(I5)') maxscan
             do j=1,jpch_rad
                write(lunout,'(I5,1x,A20,2x,I4,e15.6/100(4x,10f7.3/))') &
                     j,nusis(j),nuchan(j),tlapmean(j),(cbias(i,j),i=1,maxscan)
             end do
             close(lunout)
          end if
       end if

    endif

! Read SST dependent radiance bias correction lookup table
    if (retrieval) then
    
       allocate(predx(npred,jpch_rad))
       predx=zero

       allocate(fbias(numt,jpch_rad))
       fbias=zero
       
       if(mype==mype_rad) write(iout_rad,*) &
            'RADINFO_READ:  read SST & D/N dependent bias correction from ',lunin
       open(lunin,file='satbias_sst',form='formatted')
       rewind (lunin)

!      Loop over satellites sensors & channels
       read5: do
          read(lunin,'(I5,1x,a20,1x,I5/3(4x,11f10.3/) )',iostat=istat,end=1444) ich,isis,ichan,(fbiasx(i),i=1,numt)
          if (istat /= 0) exit
          cfound = .false.
          ! The following is to sort out some historical naming conventions
          select case (isis(1:4))
             case ('airs')
               isis='airs_aqua'
             case ('iasi')
               if (index(isis,'metop-a') /= 0) isis='iasi_metop-a'
               if (index(isis,'metop-b') /= 0) isis='iasi_metop-b'
               if (index(isis,'metop-c') /= 0) isis='iasi_metop-c'
          end select 

          do j=1,jpch_rad
             if(trim(isis) == trim(nusis(j)) .and. ichan == nuchan(j))then
                cfound = .true.
                do i=1,numt
                   fbias(i,j)=fbiasx(i)
                end do
                tlapmean(j)=tlapm
                cycle read5
             end if
          end do
          if(.not. cfound)write(6,*) ' WARNING instrument/channel ',isis,ichan, &
             'found in satbias_sst file and not found in satinfo'
       end do read5
1444   continue
       close(lunin)
    endif           ! endif for if (retrieval) then

!   Initialize observation error covariance for 
!   instruments we account for inter-channel correlations
    call corr_ob_initialize
    call corr_oberr_qc(jpch_rad,iuse_rad,nusis,varch)
!   Close unit for runtime output.  Return to calling routine
    if(mype==mype_rad)close(iout_rad)
    return

  end subroutine radinfo_read


  subroutine radinfo_write
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radinfo_write
!
!   prgrmmr:     yang        org: np20                date: 1998-05-15
!
! abstract:  This routine writes an updated version of the predictive
!            part of the bias correction.
!
! program history log:
!   1998-05-15  yang, weiyu
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2008-04-23  safford - add standard subprogram doc block
!   2010-04-29  zhu     - add analysis varaince info for radiance bias correction coefficients
!   2010-05-06  zhu     - add option adp_anglebc
!   2011-04-07  todling - adjust argument list (interface) since newpc4pred is local now
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

    implicit none

    integer(i_kind) lunout,jch,ip,i
    real(r_kind),dimension(npred):: varx
    data lunout / 51 /

!   Open unit to output file.  Write analysis varaince info.  Close unit.
    if (newpc4pred) then
       open(lunout,file='satbias_pc.out',form='formatted')
       rewind lunout
       do jch=1,jpch_rad
          do i=1,npred
             varx(i)=varA(i,jch)
          end do
          write(lunout,'(I5,1x,A20,1x,I5,e15.7/2(4x,10e15.7/))') jch,nusis(jch),&
               nuchan(jch),ostats(jch),(varx(ip),ip=1,npred)
       end do
       close(lunout)
    end if

!   Open unit to output file.  Write updated coefficients.  Close unit.
    open(lunout,file='satbias_out',form='formatted')
    rewind lunout
    if (.not. adp_anglebc) then
       do jch=1,jpch_rad
          write(lunout,'(I5,1x,a20,1x,i5,10f12.6)') jch,nusis(jch),nuchan(jch),&
               (predx(ip,jch),ip=1,npred)
       end do
    else
       do jch=1,jpch_rad
          write(lunout,'(I5,1x,a20,1x,i5,2e15.6,1x,I5/2(4x,10f12.6/))') jch,nusis(jch),nuchan(jch),&
               tlapmean(jch),tsum_tlapmean(jch),count_tlapmean(jch),(predx(ip,jch),ip=1,npred)
       end do
    end if
    close(lunout)

!   Deallocate data arrays for bias correction and those which hold
!   information from satinfo file.

    deallocate (predx,cbias,tlapmean,nuchan,nusis,iuse_rad,air_rad,ang_rad, &
         ifactq,varch,varch_cld,inew_rad,icld_det)

    if (adp_anglebc) deallocate(count_tlapmean,update_tlapmean,tsum_tlapmean)
    if (newpc4pred) deallocate(ostats,rstats,varA)
    deallocate (radstart,radstep,radnstep,radedge1,radedge2)
    return
  end subroutine radinfo_write



  integer(i_kind) function newchn(sis,ichan)   ! "satinfo-relative" index of 
                                               ! (sis,ichan) combination
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    function newchn
!
!   prgrmmr:     derber      org: np23                date: 1997-08-13
!
! abstract:  For a given satellite and channel produce a combined 
!            channel number based on input from the satinfo file.
!            If the requested channel/satellite combination is
!            not found, the function returns a zero value.
!
! program history log:
!   1997-08-13  derber
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2008-04-23  safford - add standard subprogram doc block, rm unused uses
!
!   input argument list:
!     sis     - satellite to search for
!     ichan   - channel number to search for
!
!   return:
!             - combined channel number
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

    implicit none

! !INPUT PARAMETERS:

    character(len=20), intent(in   ) :: sis   ! satellite to search for
    integer(i_kind)  , intent(in   ) :: ichan ! channel number to search for

    integer(i_kind) j

    do j=1,jpch_rad
       if ( nuchan(j)==ichan .and. nusis(j)==sis) then
          newchn=j
          return
       end if
    end do
    write(6,*) 'NEWCHN:  channel=',ichan,' sensor/instrument/satellite=',sis, &
         ' not present in satinfo file'
    newchn=0
    return
  end function newchn
  
   real(r_kind) function rnad_pos(isis,iscan,jch)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    function rnad_pos
!
!   prgrmmr:     zhu      org: np23                date: 2010-05-06
!
! abstract:  For a given satellite/sensor produce the scan angle
!
! program history log:
!   2010-05-06  zhu
!
!   return:
!             - scan angle
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:
   implicit none
   character(len=20),intent(in):: isis
   integer(i_kind),intent(in):: iscan,jch

   integer(i_kind) ifov
   real(r_kind) piece

   if (index(isis,'iasi')/=0) then

      piece=-0.625_r_kind
      if (mod(iscan,2) == 1) piece = 0.625_r_kind
      rnad_pos=radstart(jch)+radstep(jch)*float((iscan-1)/2)+piece

   else

      if (index(isis,'hirs')/=0 .and. (index(isis,'n16')/=0 .or. &
                                       index(isis,'n17')/=0)) then
         ifov=iscan+1
      else if (index(isis,'atms') /= 0 .AND. maxscan < 96) then
         ifov=iscan+3
      else
         ifov=iscan
      end if
      rnad_pos=radstart(jch)+radstep(jch)*float(ifov-1)

   end if

   return
   end function rnad_pos

   subroutine angle_cbias(isis,j,cbiasj)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    angle_cbias
!
!   prgrmmr:     zhu      org: np23                date: 2010-05-06
!
! abstract:  For a given satellite/sensor produce angle bias correction
!
! program history log:
!   2010-05-06  zhu
!   2010-12-16  treadon - recode cbiasj to be consistent with setuprad
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

     use constants, only: zero,deg2rad
     implicit none
     
     character(len=20),intent(in):: isis
     integer(i_kind),intent(in):: j
     real(r_kind),dimension(maxscan),intent(inout):: cbiasj
     
     integer(i_kind) i,k
     real(r_kind),dimension(npred):: pred
     
     pred=zero
     do i=1,min(radnstep(j),maxscan)
        pred(npred)=rnad_pos(isis,i,j)*deg2rad
        do k=2,angord
           pred(npred-k+1)=pred(npred)**k
        end do
        cbiasj(i)=zero
        do k=1,angord
           cbiasj(i) = cbiasj(i)+ predx(npred-k+1,j)*pred(npred-k+1)
        end do
        
     end do
     return
   end subroutine angle_cbias

   subroutine satstep(isis,start,step,nstep,edge1,edge2)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    satstep
!
!   prgrmmr:     zhu      org: np23                date: 2010-05-06
!
! abstract:  This routine sets step, start, and nstart
!
! program history log:
!   2010-05-06  zhu
!
!   input argument list:
!
!   output argument list:
!
! program history log:
!   2015-09-04  J.Jung  - Added mods for CrIS full spectral resolution (FSR)

! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

   use constants, only: zero,one,three
   implicit none

   character(len=20),intent(in) :: isis
   integer(i_kind),intent(out)  :: nstep,edge1,edge2
   real(r_kind),intent(out)     :: start,step

   start=zero
   step =one
   nstep=maxscan
   edge1=-1
   edge2=-1

   if (index(isis,'hirs')/=0) then
      step  = 1.80_r_kind
      start = -49.5_r_kind
      nstep = 56
      edge1 = 7
      edge2 = 50
   else if (index(isis,'msu')/=0) then
      if (index(isis,'amsua')/=0) then
         step  = three + one/three
         start = -48._r_kind - one/three
         nstep = 30
         edge1 = 4
         edge2 = 27
      else if (index(isis,'amsub')/=0) then
         step  = 1.1_r_kind
         start = -48.95_r_kind
         nstep = 90
         edge1 = 10
         edge2 = 81
      else
         step  = 9.474_r_kind
         start = -47.37_r_kind
         nstep = 90
         edge1 = 2
         edge2 = 10
      end if
   else if (index(isis,'atms')/=0) then
      step  = 1.11_r_kind
      start = -52.725_r_kind
      nstep = 96
      if (maxscan < 96) then
        edge1=7
        edge2=84
      else
        edge1 = 10
        edge2 = 87
      endif
   else if (index(isis,'mhs')/=0) then
      step  = 10.0_r_kind/9.0_r_kind
      start = -445.0_r_kind/9.0_r_kind
      nstep = 90
      edge1 = 10
      edge2 = 81
   else if (index(isis,'ssu')/=0) then
      step  = 10.0_r_kind
      start = -35.00_r_kind
      nstep = 90
      edge1 = 2
      edge2 = 7
   else if (index(isis,'airs')/=0) then
      step  = 1.1_r_kind
      start = -48.9_r_kind
      nstep = 90
      edge1 = 10
      edge2 = 81
   else if (index(isis,'hsb')/=0) then
      step  = 1.1_r_kind
      start = -48.95_r_kind
      nstep = 90
      edge1 = 10
      edge2 = 81
   else if (index(isis,'iasi')/=0) then
      step  = 3.334_r_kind
      start = -48.33_r_kind
      nstep = 60
      edge1 = 5
      edge2 = 56
   else if (index(isis,'cris')/=0) then
      step  = 3.322_r_kind
      start = -51.675_r_kind
      nstep = 30
      edge1 = 1
      edge2 = 30
   else if (index(isis,'saphir')/=0) then
      step  = 0.666_r_kind
      start = -42.960_r_kind
      nstep = 130
      edge1 = 1
      edge2 = 130
   end if

   return
   end subroutine satstep


   subroutine init_predx
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_predx
!
!   prgrmmr:     zhu      org: np23                date: 2010-07-13
!
! abstract:  initialize predictor coeficients for a given satellite/sensor 
!
! program history log:
!   2010-07-13  zhu  - modified from global_angupdate
!   2011-04-07  todling - adjust argument list (interface) since newpc4pred is local now
!   2013-01-03  j.jin   - adding logical tmi for mean_only. (radinfo file not yet ready. JJ)
!   2013-07-19  zhu  - unify the weight assignments for both active and passive channels
!   2014-10-01  ejones  - add gmi and amsr2 logical
!   2015-01-16  ejones  - add saphir logical
!   2015-03-23  zaizhong ma - added the Himawari-8 ahi
!   2015-10-22  jung    - changed from using satinfo information in the radstat file to
!                         using information from the satinfo file.
!   2016-07-14  jung    - mods to make SEVIRI channel numbers consistent with other instruments.
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

   use obsmod, only: ndat,dplat,dtype,dsis
   use mpimod, only:  npe,mype,mpi_comm_world,ierror
   use read_diag, only: read_radiag_header,read_radiag_data,diag_header_fix_list,&
        diag_header_chan_list,diag_data_fix_list,diag_data_chan_list,&
        diag_data_extra_list,diag_data_name_list
   use constants, only: zero,one,deg2rad
   implicit none

!  Declare local parameters
   integer(i_kind),parameter:: lndiag = 21
   integer(i_kind),parameter:: lntemp = 51

   integer(i_kind),parameter:: nthreshold = 100
   integer(i_kind),parameter:: maxchn = 3000
   integer(i_kind),parameter:: maxdat = 100

!  Declare local variables
   logical lexist
   logical lverbose 
   logical update
   logical mean_only
   logical ssmi,ssmis,amsre,amsre_low,amsre_mid,amsre_hig,tmi,gmi,amsr2,saphir
   logical ssmis_las,ssmis_uas,ssmis_env,ssmis_img
   logical avhrr,avhrr_navy,goessndr,goes_img,ahi,seviri

   character(len=20):: obstype,platid
   character(len=20):: satsens,satsens_id
   character(len=50):: fdiag_rad,dname,fname

   integer(i_kind):: ix,ii,iii,iich,ndatppe
   integer(i_kind):: i,j,jj,n_chan,k,lunout
   integer(i_kind):: istatus,ispot
   integer(i_kind):: np,new_chan,nc
   integer(i_kind):: counttmp, jjstart, sensor_start, sensor_end
   integer(i_kind):: radedge_min, radedge_max
   integer(i_kind),dimension(maxchn):: ich
   integer(i_kind),dimension(maxdat):: ipoint
 
   real(r_kind):: bias,scan,errinv,rnad,atiny
   real(r_kind):: tlaptmp,tsumtmp,ratio
   real(r_kind),allocatable,dimension(:):: tsum0,tsum,tlap0,tlap1,tlap2,tcnt
   real(r_kind),allocatable,dimension(:,:):: AA
   real(r_kind),allocatable,dimension(:):: be
   real(r_kind),allocatable,dimension(:):: iobs
   real(r_kind),allocatable,dimension(:,:,:):: A
   real(r_kind),allocatable,dimension(:,:):: b
   real(r_kind),allocatable,dimension(:):: pred
   real(r_kind),allocatable,dimension(:):: predr

!  Declare types used for reading satellite data
   type(diag_header_fix_list )             :: header_fix
   type(diag_header_chan_list),allocatable :: header_chan(:)
   type(diag_data_name_list)               :: data_name
   type(diag_data_fix_list   )             :: data_fix
   type(diag_data_chan_list  ),allocatable :: data_chan(:)
   type(diag_data_extra_list ),allocatable :: data_extra(:,:)

   data lunout / 53 / 

!************************************************************************
!  Return if no new channels AND update_tlapmean=.false.
   if (.not. (any(inew_rad) .or. any(update_tlapmean))) return
   if (mype==0) write(6,*) 'INIT_PREDX:  enter routine'

!  Allocate and initialize data arrays
   if (any(update_tlapmean)) then
      allocate(tsum0(jpch_rad),tsum(jpch_rad),tlap0(jpch_rad), &
               tlap1(jpch_rad),tlap2(jpch_rad),tcnt(jpch_rad))
      do j=1,jpch_rad
         tsum(j)=zero
         tsum0(j)=tsum_tlapmean(j)
         tcnt(j)=zero
         tlap1(j)=zero
         tlap2(j)=zero
         tlap0(j)=tlapmean(j)
         if (.not. newpc4pred) tlap0(j)=100.0_r_kind*tlap0(j)  ! convert tlap to its original unit
      end do
   end if

!  Assign each satellite/sensor to a mpi task
   ndatppe=0
   ix=0
   ipoint=0
   do i=1,ndat
      if(ix >= npe )ix=ix-npe
      if(ix == mype)then
         ndatppe=ndatppe+1
         ipoint(ndatppe)=i
      end if
      ix=ix+1
   end do

!  Loop over mpi tasks.  Each task processes a given set of satellite/sensors
   loopf:  do ii=1,ndatppe
      iii=ipoint(ii)
      obstype=dtype(iii)
      platid=dplat(iii)
      satsens_id=dsis(iii)

!     Create diagnostic filename
      fdiag_rad = 'diag_' // trim(dtype(iii)) // '_' // trim(dplat(iii))

!     See if diagnostic file exists
      inquire(file=fdiag_rad,exist=lexist)
      if (.not.lexist) cycle loopf

!     Open file and read header
      open(lndiag,file=fdiag_rad,form='unformatted',status='old',iostat=istatus)
      if (istatus/=0) then
         write(6,*)'INIT_PREDX:  Task ',mype,' problem opening file ',trim(fdiag_rad),' iostat=',istatus
         close(lndiag)
         cycle loopf
      endif

      lverbose=.false. 
      call read_radiag_header(lndiag,npred,retrieval,header_fix,header_chan,data_name,istatus,lverbose) 
      if (istatus/=0) then
         write(6,*)'INIT_PREDX:  Task ',mype,' problem reading file ',trim(fdiag_rad),' header, iostat=',istatus
         close(lndiag)
         cycle loopf
      endif

!     Process file
      if(mype == 0)write(6,*)'INIT_PREDX:  Task ',mype,' processing ',trim(fdiag_rad)
      satsens = header_fix%isis
      n_chan = header_fix%nchan

!     Check for consistency between specified and retrieved satellite id
!     after first sorting out some historical naming conventions
      select case (satsens(1:4))
         case ('airs')
           satsens='airs_aqua'
         case ('iasi')
           if (index(satsens,'metop-a') /= 0) satsens='iasi_metop-a'
           if (index(satsens,'metop-b') /= 0) satsens='iasi_metop-b'
           if (index(satsens,'metop-c') /= 0) satsens='iasi_metop-c'
      end select 
      if (satsens /= satsens_id) then
         write(6,*)'INIT_PREDX:  ***ERROR*** inconsistent satellite ids ',&
              ' fdiag_rad= ',trim(fdiag_rad),' satsens,satsens_id=',satsens,satsens_id
         cycle loopf
      endif

!     Define sensor_satellite channel range from satinfo file
      sensor_start = 0
      sensor_end = 0
      do j = 1, jpch_rad
         if ( trim(nusis(j)) == trim(satsens_id)) then
            if ( sensor_start == 0 ) sensor_start = j
            sensor_end = j
         endif
      end do
      if ( sensor_start == 0 ) cycle loopf

!     Extract satinfo relative index and get new_chan
      new_chan=0
      update=.false.

!     Seviri channels should start at 4.  If the first seviri channel is less than 4
!     do not use this diag* file
      if ( satsens(1:6) == 'seviri' .and. header_chan(1)%nuchan < 4) then
        close(lndiag)
        cycle loopf
      endif

      jjstart = sensor_start
      loop_a: do j=1,n_chan
         do jj=jjstart, sensor_end
            if ( nuchan(jj) == header_chan(j)%nuchan ) then
               jjstart = jj + 1
               if (inew_rad(jj)) then
                  new_chan=new_chan+1
                  ich(new_chan) = jj
               end if
               if (update_tlapmean(jj)) update=.true.
               cycle loop_a
            endif
         end do 
      end do loop_a
       
      if (.not. update .and. new_chan==0) then 
         close(lndiag)
         cycle loopf
      end if

      goessndr   = obstype == 'sndr'  .or. obstype == 'sndrd1' .or.  &
                   obstype == 'sndrd2'.or. obstype == 'sndrd3' .or.  &
                   obstype == 'sndrd4'
      goes_img   = obstype == 'goes_img'
      ahi        = obstype == 'ahi'
      avhrr      = obstype == 'avhrr'
      avhrr_navy = obstype == 'avhrr_navy'
      ssmi       = obstype == 'ssmi'
      amsre_low  = obstype == 'amsre_low'
      amsre_mid  = obstype == 'amsre_mid'
      amsre_hig  = obstype == 'amsre_hig'
      amsre      = amsre_low .or. amsre_mid .or. amsre_hig
      ssmis      = obstype == 'ssmis'
      ssmis_las  = obstype == 'ssmis_las'
      ssmis_uas  = obstype == 'ssmis_uas'
      ssmis_img  = obstype == 'ssmis_img'
      ssmis_env  = obstype == 'ssmis_env'
      ssmis=ssmis_las.or.ssmis_uas.or.ssmis_img.or.ssmis_env.or.ssmis
      seviri     = obstype == 'seviri'
      tmi        = obstype == 'tmi'
      gmi        = obstype == 'gmi'
      saphir     = obstype == 'saphir'
      amsr2      = obstype == 'amsr2'
      mean_only=ssmi .or. ssmis .or. amsre .or. goessndr .or. goes_img & 
                .or. ahi .or. seviri .or. tmi
!     Allocate arrays and initialize
      if (mean_only) then 
         np=1
      else
         np=angord+1
      end if
      if (new_chan/=0) then
         allocate(A(np,np,new_chan),b(np,new_chan))
         allocate(iobs(new_chan),pred(np))
         do j=1,new_chan
            iobs(j)=zero
            do i=1,np
               b(i,j)=zero
               do k=1,np
                  A(i,k,j)=zero
               end do
            end do
         end do
      end if

      radedge_min = 0
      radedge_max = 1000
      do i=sensor_start, sensor_end
         if (radedge1(i)/=-1 .and. radedge2(i)/=-1) then
            radedge_min=radedge1(i)
            radedge_max=radedge2(i)
         end if
         exit 
      end do

!     Loop to read diagnostic file
      istatus = 0
      loopd:  do while (istatus == 0)
 
!        Read a record.  If read flag, istatus does not equal zero, exit loopd
         call read_radiag_data( lndiag,header_fix,retrieval,data_fix,data_chan,data_extra,istatus )
         if( istatus /= 0 ) exit loopd

!        Extract scan angle, lat, lon
         scan   = data_fix%senscn_pos
         ispot  = nint(scan)

!        Exclude data on edges
         if (.not. use_edges .and. (&
              ispot < radedge_min .OR. ispot > radedge_max )) cycle loopd

!        Channel loop
         nc=0
         jjstart = sensor_start
         loopc:  do j = 1, n_chan
            do jj=jjstart, sensor_end
               if ( nuchan(jj) == header_chan(j)%nuchan ) then
                  jjstart = jj + 1
                  if (inew_rad(jj)) nc = nc+1

                  if ((.not. inew_rad(jj)) .and.  &
                     (.not. update_tlapmean(jj))) cycle loopc

!           Check for reasonable obs-ges and observed Tb.
!           If the o-g difference is too large (> 200 K, very genereous!)
!           of the observation is too cold (<50 K) or too warm (>500 K),
!           do not use this observation in computing the update to the
!           angle dependent bias.
                  if( ( abs(data_chan(j)%omgnbc) > 200. .or. &
                       data_chan(j)%tbobs < 50. .or. &
                       data_chan(j)%tbobs > 500. ) ) cycle loopc
 
!           if errinv= (1 /(obs error)) is small (small = less than 1.e-6)
!           the observation did not pass quality control.  In this
!           case, do not use this observation in computing the update
!           to the angle dependent bias
!           if (iuse_rad(jj)>0 .and. data_chan(j)%errinv<1.e-6) cycle loopc

!           errinv=data_chan(j)%errinv
!           if (iuse_rad(jj)<=0) errinv=exp(-(data_chan(j)%omgnbc/3.0_r_kind)**2)
                  errinv=exp(-(data_chan(j)%omgnbc/3.0_r_kind)**2)

                  if (update_tlapmean(jj)) then
                     tlaptmp=data_chan(j)%tlap
                     if (header_fix%inewpc==0) tlaptmp=100.0_r_kind*tlaptmp
                     tlap1(jj)=tlap1(jj)+(tlaptmp-tlap0(jj))*errinv
                     tsum(jj) =tsum(jj)+errinv
                     tcnt(jj) =tcnt(jj)+one
                  end if

                  if (inew_rad(jj)) then
!                    Define predictor
                     pred=zero
                     pred(1) = one
                     if (.not. mean_only) then
                        rnad = rnad_pos(satsens,ispot,jj)*deg2rad
                        do i=1,angord
                           pred(i+1) = rnad**i
                        end do
                     end if

!                    Add values to running sums.
                     iobs(nc) = iobs(nc)+one
                     bias = data_chan(j)%omgnbc
                     do i=1,np
                        b(i,nc) = b(i,nc)+bias*pred(i)*errinv**2
                     end do
                     do k=1,np
                        do i=1,np
                           A(i,k,nc) = A(i,k,nc)+pred(i)*pred(k)*errinv**2
                        end do
                     end do
                  end if
                  cycle loopc
               end if
            end do   
         enddo loopc ! channel loop

!     End of loop over diagnostic file
      enddo loopd

      close(lndiag)

!     Compute tlapmean
      if (update) then
         jjstart = sensor_start
         loop_b: do j = 1,n_chan
            do jj=jjstart, sensor_end
               if ( nuchan(jj) == header_chan(j)%nuchan ) then
                  jjstart = jj + 1
                  if (update_tlapmean(jj)) then
                     if(tcnt(jj) >= nthreshold)  then
                        tsum(jj)=tsum(jj)+tsum0(jj)
!                       tlap2(jj) = tlap0(jj) + wgtlap*tlap1(jj)/tsum(jj)
                        tlap2(jj) = tlap0(jj) + tlap1(jj)/tsum(jj)
                        count_tlapmean(jj)=count_tlapmean(jj)+one
                     elseif (tcnt(jj)>0) then
                        ratio = max(zero,min(tcnt(jj)/float(nthreshold),one))
                        tsum(jj)=ratio*tsum(jj)+tsum0(jj)
!                       tlap2(jj) = tlap0(jj) + ratio*wgtlap*tlap1(jj)/tsum(jj)
                        tlap2(jj) = tlap0(jj) + ratio*tlap1(jj)/tsum(jj)
                        count_tlapmean(jj)=count_tlapmean(jj)+one
                     else
                        tsum(jj)=tsum0(jj)
                        tlap2(jj) = tlap0(jj)
                        count_tlapmean(jj)=count_tlapmean(jj)
                     endif
                     tsum_tlapmean(jj)=tsum(jj)
                     tlapmean(jj)=tlap2(jj)
                     if (.not. newpc4pred) tlapmean(jj)=0.01_r_kind*tlapmean(jj)
                  endif
                  cycle loop_b
               endif 
            end do
         end do loop_b

!        Write updated tlapmean and sample size to scratch files
         dname = 'update_' // trim(obstype) // '_' // trim(platid)
         open(lntemp,file=dname,form='formatted')

         jjstart = sensor_start
         loop_c: do j=1,n_chan
            do jj=jjstart, sensor_end
               if ( nuchan(jj) == header_chan(j)%nuchan ) then
                  jjstart = jj + 1
                  write(lntemp,220) jj,tlapmean(jj),tsum_tlapmean(jj),count_tlapmean(jj)
220               format(I5,1x,2e15.6,1x,I5)
                  cycle loop_c
               endif
            end do
         end do loop_c
         close(lntemp)
      end if

      if (new_chan/=0) then
         if (all(iobs<nthreshold)) then
            deallocate(A,b,iobs,pred)
            cycle loopf
         endif

!        Solve linear system
         atiny=1.0e-10_r_kind
         allocate(AA(np,np),be(np))
         do i=1,new_chan
            if (iobs(i)<nthreshold) cycle
            AA(:,:)=A(:,:,i)
            be(:)  =b(:,i)
            if (all(abs(AA)<atiny)) cycle
            if (all(abs(be)<atiny)) cycle
            call linmm(AA,be,np,1,np,np)

            predx(1,ich(i))=be(1)
            if (.not. mean_only) then
               do j=1,angord
                  predx(npred-j+1,ich(i))=be(j+1)
               end do
            end if
         end do ! end of new_chan
         deallocate(AA,be)

!        write initialized predx to scratch files
         dname = 'init_' // trim(obstype) // '_' // trim(platid)
         open(lntemp,file=dname,form='formatted')
         do i=1,new_chan
            if (iobs(i)<nthreshold) cycle
            write(lntemp,210) ich(i),predx(1,ich(i)),(predx(npred-k+1,ich(i)),k=1,angord)
 210        format(I5,1x,5e13.6)
         end do
         close(lntemp)

         deallocate(A,b)
         deallocate(iobs,pred)
      end if

!  End of loop over satellite/sensor types
   end do loopf

!  Wait for all mpi tasks to finish processing the
!  satellite/sensors assigned to them.
!  write(6,*)'INIT_PREDX:  Wait after satellite/sensor loop'
   call mpi_barrier(mpi_comm_world,ierror)


!  Combine the satellite/sensor specific predx together
   if (any(inew_rad)) then
      allocate(predr(angord+1))
      do i=1,ndat
         fname = 'init_' // trim(dtype(i)) // '_' // trim(dplat(i))
         inquire(file=fname,exist=lexist)

!        Process the scratch file
         if (lexist) then
!           Read data from scratch file
            if(mype == 0)write(6,*) 'INIT_PREDX:  processing update file i=',i,' with fname=',trim(fname)
            open(lntemp,file=fname,form='formatted')
            do
               read(lntemp,210,end=160) iich,(predr(k),k=1,angord+1)
               predx(1,iich)=predr(1)
               do j=1,angord
                  predx(npred-j+1,iich)=predr(j+1)
               end do
            end do
160         continue
            close(lntemp)
         end if  ! end of lexist
      end do ! end of ndat
      deallocate(predr)
   end if

   if (any(update_tlapmean)) then 
      do i=1,ndat
         fname = 'update_' // trim(dtype(i)) // '_' // trim(dplat(i))
         inquire(file=fname,exist=lexist)

!        Process the scratch file
         if (lexist) then
!           Read data from scratch file
            if(mype == 0)write(6,*) 'INIT_PREDX:  processing update file i=',i,' with fname=',trim(fname)
            open(lntemp,file=fname,form='formatted')
            do 
               read(lntemp,220,end=260) jj,tlaptmp,tsumtmp,counttmp
               tlapmean(jj)=tlaptmp
               tsum_tlapmean(jj)=tsumtmp
               count_tlapmean(jj)=counttmp
            end do
260         continue
            close(lntemp)
         end if  ! end of lexist
      end do ! end of ndat

      deallocate(tsum0,tsum,tlap0,tlap1,tlap2,tcnt) 
   end if

   if (mype==mype_rad) then  
      if (any(inew_rad) .or. any(update_tlapmean)) then
         open(lunout,file='satbias_out.int',form='formatted')
         do j=1,jpch_rad
            write(lunout,'(I5,1x,a20,1x,i5,2e15.6,1x,I5/2(4x,10f12.6/))')j,nusis(j),nuchan(j),&
                      tlapmean(j),tsum_tlapmean(j),count_tlapmean(j),(predx(ii,j),ii=1,npred)
         end do
         close(lunout)
      end if
   end if 

!  End of program
   return
   end subroutine init_predx
 logical function adjust_jac_ (iinstr,isis,isfctype,nchanl,nsigradjac,ich,varinv,&
                               depart,obvarinv,adaptinf,wgtjo,jacobian)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    adjust_jac_
!
!   prgrmmr:     todling  org: gmao                date: 2014-04-15
!
! abstract:  provide hook to module handling inter-channel ob correlated errors
!
! program history log:
!   2014-04-15  todling - initial code
!   2016-07-19  todling - change obtype to isis for more flexibity
!   2016-07-19  todling - add wgtjo to arg list
!   2016-07-19  W. Gu - revisit bias handling
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block
   use constants, only: tiny_r_kind,zero,one
   use correlated_obsmod, only: idnames
   use correlated_obsmod, only: corr_ob_amiset
   use correlated_obsmod, only: corr_ob_scale_jac
   use correlated_obsmod, only: GSI_BundleErrorCov 
   use mpeu_util, only: getindex
   use mpeu_util, only: die
   use mpimod, only: mype
   implicit none

   character(len=*),intent(in) :: isis
   integer(i_kind), intent(in) :: isfctype
   integer(i_kind), intent(in) :: nchanl
   integer(i_kind), intent(in) :: nsigradjac
   integer(i_kind), intent(in) :: ich(nchanl)
   real(r_kind), intent(inout) :: varinv(nchanl)
   real(r_kind), intent(inout) :: depart(nchanl)
   real(r_kind), intent(inout) :: obvarinv(nchanl)
   real(r_kind), intent(inout) :: adaptinf(nchanl)
   real(r_kind), intent(inout) :: wgtjo(nchanl)
   real(r_kind), intent(inout) :: jacobian(nsigradjac,nchanl)
   integer(i_kind), intent(out) :: iinstr
   character(len=*),parameter::myname_ = myname//'*adjust_jac_'
   character(len=80) covtype

   adjust_jac_=.false.

   if(.not.allocated(idnames)) then
     return
   endif

   iinstr=-1
   if(isfctype==0)then
      covtype = trim(isis)//':sea'
      iinstr=getindex(idnames,trim(covtype))
   else if(isfctype==1)then
      covtype = trim(isis)//':land'
      iinstr=getindex(idnames,trim(covtype))
   else if(isfctype==2)then
      covtype = trim(isis)//':ice'
      iinstr=getindex(idnames,trim(covtype))
   else if(isfctype==3)then
      covtype = trim(isis)//':snow'
      iinstr=getindex(idnames,trim(covtype))
   else if(isfctype==4)then
      covtype = trim(isis)//':mixed'
      iinstr=getindex(idnames,trim(covtype))
   endif
   if(iinstr<0) return  ! do not use the correlated errors

   if(.not.corr_ob_amiset(GSI_BundleErrorCov(iinstr))) then
      call die(myname_,' improperly set GSI_BundleErrorCov')
   endif

   if( GSI_BundleErrorCov(iinstr)%nch_active < 0) return

   adjust_jac_ = corr_ob_scale_jac(depart,obvarinv,adaptinf,jacobian,nchanl,jpch_rad,varinv,wgtjo, &
                                    iuse_rad,ich,GSI_BundleErrorCov(iinstr))
end function adjust_jac_

subroutine get_rsqrtinv_ (iinstr,nchasm,ich,ichasm,varinv,rsqrtinv)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    get_rsqrtinv_
!
!   prgrmmr:     Wei  org: gmao                date: 2015-03-11
!
! abstract:  provide hook to obtain the inverse of the square-root of R
!
! program history log:
!   2015-03-11  W. Gu - initial code
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block
   use constants, only: tiny_r_kind,zero,one
   use correlated_obsmod, only: corr_ob_rsqrtinv
   use correlated_obsmod, only: GSI_BundleErrorCov
   use mpeu_util, only: getindex
   use mpeu_util, only: die
   use mpimod, only: mype
   implicit none
   integer(i_kind), intent(in) :: iinstr
   integer(i_kind), intent(in) :: nchasm
   integer(i_kind), intent(in) :: ich(nchasm)
   integer(i_kind), intent(in) :: ichasm(nchasm)
   real(r_kind), intent(in) :: varinv(nchasm)    ! inverse of specified ob-error-variance
   real(r_kind), intent(inout) :: rsqrtinv(nchasm,nchasm)

   character(len=*),parameter::myname_ = myname//'*get_rsqrtinv_'

   call corr_ob_rsqrtinv (jpch_rad,iuse_rad,nchasm,ich,ichasm,varinv,&
                          rsqrtinv,GSI_BundleErrorCov(iinstr))

end subroutine get_rsqrtinv_

end module radinfo
