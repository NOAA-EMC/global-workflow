module rapidrefresh_cldsurf_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:  rapid refresh module
! prgmmr:  Ming Hu             org: GSD/AMB           date: 2008-06-04
!
! abstract: 
!      This module contains definition and initialization of variables for RR
!
! program history log:
!   2008-06-03 Hu           initial build
!   2010-03-29 Hu           change to fit the trunk version
!   2015-01-15 Hu        added options i_use_2mq4b,i_use_2mt4b, i_gsdcldanal_type
!                              i_gsdsfc_uselist,i_lightpcp,i_sfct_gross under
!                              rapidrefresh_cldsurf
!   2016-02-29 S.Liu        added options l_use_hydroretrieval_all
!   2015-01-13 Ladwig   added option l_numconc
!   2016-09-01 Hu       added option l_closeobs
!   02-02-2017 Hu       added option i_coastline to turn on the observation
!                              operator for surface observations along the
!                              coastline area
!  04-01-2017 Hu        added option i_gsdqc to turn on special observation qc
!                              from GSD (for RAP/HRRR application)
!   2018-09-12 Ladwig   added options l_precip_clear_only
!   2019-10-10 Zhao     added options l_rtma3d and l_precip_vertical_check (for
!                             RTMA3D only now)
!   2020-04-16 Zhao     change option l_precip_vertical_check to i_precip_vertical_check
!                       option for checking and adjusting the profile of Qr/Qs/Qg/Qnr
!                       retrieved through cloud analysis to reduce the background
!                       reflectivity ghost in analysis. (default is 0)
!   2023-07-30 Zhao     added options for analysis of significant wave height 
!                         (SWH, aka howv in GSI code):
!                         corp_howv:   to set the static background error of howv
!                         hwllp_howv:  to set the de-correlation length scale
!                         i_howv_3dda: control the analysis of howv in 3D analysis (if howv is in anavinfo)
! 
! Subroutines Included:
!   sub init_rapidrefresh_cldsurf  - initialize RR related variables to default values
!
! Variable Definitions:
!   def l_hydrometeor_bkio    - namelist logical for read and write hydrometeor
!                               background fields (=true) 
!   def dfi_radar_latent_heat_time_period - DFI forward integration window in minutes
!   def metar_impact_radius - impact radius for METAR cloud observation
!   def metar_impact_radius_lowCloud - impact radius for METAR cloud observation
!                                      that indicate low cloud base
!   def l_metar_impact_radius_change - if .true. the impact radius will change
!                            with height that set up with the
!                            metar_impact_radius_max, min, max_height,
!                            min_height, (default:false)
!   def metar_impact_radius_max  - The max impact radius of metar cloud
!                            observation in meter (default: 100000 m).
!   def metar_impact_radius_min  - The min impact radius of metar cloud
!                            observation in meter (default: 10000 m).
!   def metar_impact_radius_max_height - The hight above which
!                            metar_impact_radius_max apply
!                            in meter (default: 1200m).
!   def metar_impact_radius_min_height - The hight below which
!                            metar_impact_radius_min apply
!                            in meter (default: 200m).
!   def l_gsd_terrain_match_surfTobs - namelist logical for GSD terrain
!                                       match for  surface temperature observation
!   def l_sfcobserror_ramp_t  - namelist logical for adjusting surface temperature observation error
!   def l_sfcobserror_ramp_q  - namelist logical for adjusting surface moisture observation error
!   def l_pbl_pseudo_surfobst - namelist logical for producing pseudo-obs in PBL 
!                                       layer based on surface obs T
!   def l_pbl_pseudo_surfobsq - namelist logical for producing pseudo-obs in PBL 
!                                       layer based on surface obs Q
!   def l_pbl_pseudo_surfobsuv - namelist logical for producing pseudo-obs in PBL 
!                                       layer based on surface obs UV
!   def pblh_ration - percent of the PBL height within which to add 
!                                       pseudo-obs (default:0.75)
!   def pps_press_incr - pressure increase for each additional pseudo-obs 
!                                       on top of previous level (default:30hPa)
!   def l_gsd_limit_ocean_q      - namelist logical for doing GSD limitation of Q over ocean
!   def l_pw_hgt_adjust      - namelist logical for doing precipitable water (PW) height adjustment
!                                       based on obs vs. model height
!   def l_limit_pw_innov     - namelist logical for limiting size of PW innovation
!   def max_innov_pct        - namelist real for limit size of PW innovation to percent
!                                       of background value (value = 0 to 1)
!   def l_cleansnow_warmts   - namelist logical for doing GSD limitation of using
!                                       retrieved snow over warn area (Ts > 5C)
!   def l_conserve_thetav    - namelist logical for conserving thetaV during moisture
!                                       adjustment in cloud analysis
!   def r_cleansnow_warmts_threshold - namelist threshold for using retrieved snow over warn area
!
!   def i_conserve_thetav_iternum    - namelist iteration number for conserving 
!                                           thetaV during moisture adjustment
!   def l_gsd_soiltq_nudge   - namelist logical for doing GSD soil T and Q nudging 
!                                       based on the lowest t analysis inc
!   def l_cld_bld            - namelist logical for GOES cloud building
!   def cld_bld_hgt          - namelist real for height limit, below which you build clouds
!                                       (default = 1200 meters)
!   def build_cloud_frac_p   - namelist real for GOES cloud building threshold
!   def clear_cloud_frac_p   - namelist real for GOES cloud clearing threshold
!   def nesdis_npts_rad      - NESDIS cloud product impact radiu (grid points) 
!
!   def iclean_hydro_withRef - if =1, then clean hydrometeors if the grid point
!                                     has no echo and maxref=0
!   def iclean_hydro_withRef_allcol - if =1, then clean whole column
!                                 hydrometeors if the observed max ref =0 and
!                                 satellite obs shows clean
!      i_use_2mq4b    -  background used for calculate surface moisture
!                             observation innovation
!                         =0  Use Q from the 1st model level. (default) 
!                         =1  use 2m Q as part of background
!      i_use_2mt4b    -  background used for calculate surface temperature         
!                             observation innovation
!                         =0  Use T from the 1st model level. (default)
!                         =1  use 2m T as part of background 
!      i_gsdcldanal_type    - options for how GSD cloud analysis should be conducted  
!                         =0. no cloud analysis (default)
!                         for ARW
!                         =1.  cloud analysis after var analysis
!                         =3.  cloud analysis only with hybrometeors NETCDF I/O
!                         =5.  skip cloud analysis and NETCDF file update
!                         =6.  cloud analysis only and do hybrometeors NETCDF/O
!                         =7   cloud analysis in observer with I/O
!                         =2  cloud analysis for NAM
!                         =30  cloud analysis for GFS
!                         =99  only read hydrometer fields but no cloud analysis
!      i_gsdsfc_uselist  - options for how to use surface observation use or
!                          rejection list
!                         =0 . EMC method (default)
!                         =1 . GSD method
!      i_lightpcp        - options for how to deal with light precipitation
!                         =0 . don't add light precipitation (default)
!                         =1 . add light precipitation in warm section
!      i_sfct_gross      - if use extended threshold for surface T gross check
!                         =0 use threshold from convinfo (default)
!                         =1 for cold surface, threshold for gross check is
!                         enlarged to bring more large negative innovation into
!                         analysis.
!      l_numconc    - logical for updating cloud water & cloud ice number
!                         concentration 
!      l_closeobs   - logical for pick the observation close to analysis time 
!                         when there are duplicated observations. This will turn
!                         off the obs error inflation from duplication.
!      i_coastline   - options to turn on observation operator for coastline
!                                surface observations
!                         =0. turn off observation operator for coastline
!                         surface observations (default)
!                         =1.  for temperature surface observations
!                         =2.  for moisture surface observations
!                         =3.  for temperature and moisture surface observations
!      i_gsdqc            - option i_gsdqc to turn on special observation qc
!                              from GSD (for RAP/HRRR application)
!                         =0 turn off
!                         =2 turn on
!      qv_max_inc    -  namelist real for limiting the maximum water vapor increment
!      ioption       - interpolation option for satellite mapping 
!                       = 1  if selection is nearest neighbor
!                       = 2  if selection is median of samples
!      l_precip_clear_only  =true, only clear precipatating hydrometeors
!      l_fog_off            =true, turn off the impact of metar visibilty obs on cloud fields 
!      cld_bld_coverage  - namelist real for cloud coverage required for qc/qi building
!                             (default = 0.6)
!      cld_clr_coverage  - namelist real for cloud coverage required for qc/qi clearing
!                             (default = 0.4)
!      i_cloud_q_innovation - integer to choose if and how cloud obs are used
!                           0= no innovations 
!                           1= cloud total innovations
!                           20= cloud build/clear derived water vapor innovations
!                           21= cloud build derived water vapor innovations
!                           22= cloud clear derived water vapor innovations
!                           3= cloud total & water vapor innovations
!      i_ens_mean    - integer for setupcldtot behavior
!                           0=single model run
!                           1=ensemble mean
!                           2=ensemble members
!      i_T_Q_adjust -     =0 no temperature and moisture adjustment in hydrometeor analyis
!                         =1 (default) temperature and moisture are adjusted in hydrometeor analyis
!                         =2 temperature and moisture only adjusted for clearing (warmer, drier)
!      l_saturate_bkCloud - if .true. ensure saturation for all cloud 3-d points
!                        in background where observed cloud cover is missing
!                        (default:true).
!      l_rtma3d      - logical option for turning on configuration for RTMA3D
!                           (default is .FALSE.)
!      i_precip_vertical_check - integer option for checking and adjusting
!                                Qr/Qs/Qg and Qnr after precipitation analysis
!                                to reduce the background reflectivity ghost in
!                                analysis. (default is 0)
!                          = 0(no adjustment)
!                          = 1(Clean off Qg only, where dbz_obs_max<=35dbz in the profile)
!                          = 2(clean Qg as in 1, and adjustment to the retrieved Qr/Qs/Qnr throughout the whole profile)
!                          = 3(similar to 2, but adjustment to Qr/Qs/Qnr only below maximum reflectivity level
!                           and where the dbz_obs is missing);
!      corp_howv      - namelist real, static BE of howv (standard error deviation)
!      hwllp_howv     - namelist real, static BE de-correlation length scale of howv
!      i_howv_3dda    - integer, control the analysis of howv in 3D analysis (either var or hybrid)
!                          = 0 (howv-off: default) : no analysis of howv in 3D analysis.
!                          = 1 (howv-on) : if variable name "howv" is found in anavinfo,
!                                          set it to be 1 to turn on analysis of howv;
!                          note: in hybrid envar run, the static BE is redueced by beta_s (<1.0),
!                                since there is no ensemble of howv currently yet, then no ensemble 
!                                contribution to the total BE of howv, so the total BE of howv is actually
!                                just the reduced static BE of howv. If to make the analysis of howv
!                                in hyrbid run is as similar as the analysis of howv in pure 3dvar run, 
!                                the static BE of howv used in hybrid run needs to be tuned (inflated actually).
!      corp_gust      - namelist real, static BE of gust (standard error deviation)
!                          note: 1. initialised to be an arbitary negative value, in order to skip this 
!                                   negative value, instead to use value (3.0 m/s) set in subroutine 
!                                   berror_read_wgt_reg as default.
!                                2. (3drtma only) if a user-specified value (e.g., 2.0 m/s) is preferred 
!                                   for corp_gust, in GSI namelist session "rapidrefresh_cldsurf",
!                                   set "corp_gust=2.0,"
!      hwllp_gust     - namelist real, static BE de-correlation length scale of gust
!                          note: 1. initialised to be an arbitary negative value, in order to skip this 
!                                   negative value, instead to use value (same value for q) set in
!                                   subroutine berror_read_wgt_reg as default
!                                2. (3drtma only) if a user-specified value (e.g., 100 km) is preferred 
!                                   for hwllp_gust, in GSI namelist session "rapidrefresh_cldsurf",
!                                   set "hwllp_gust=100000.0,"
!      oerr_gust      - namelist real, observation error of gust
!                          note: 1. initialised to be an arbitary negative value, in order to skip this 
!                                   negative value, instead to use value (1.0 m/s) set in read_prepbufr.f90
!                                2. (3drtma only) if a user-specified value (e.g., 1.5 m/s ) is preferred 
!                                   for oerr_gust, in GSI namelist session "rapidrefresh_cldsurf",
!                                   set "oerr_gust=1.5,"
!      i_gust_3dda    - integer, control the analysis of gust in 3D analysis (either var or hybrid)
!                          = 0 (gust-off: default) : no analysis of gust in 3D analysis.
!                          = 1 (gust-on) : if variable name "gust" is found in anavinfo,
!                                          set it to be 1 to turn on analysis of gust;
!
! attributes:
!   language: f90
!   machine:  linux cluster (wjet)
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_rapidrefresh_cldsurf
  public :: l_hydrometeor_bkio 
  public :: dfi_radar_latent_heat_time_period
  public :: metar_impact_radius
  public :: metar_impact_radius_lowCloud
  public :: l_metar_impact_radius_change
  public :: metar_impact_radius_max
  public :: metar_impact_radius_min
  public :: metar_impact_radius_max_height
  public :: metar_impact_radius_min_height
  public :: l_gsd_terrain_match_surfTobs
  public :: l_sfcobserror_ramp_t
  public :: l_sfcobserror_ramp_q
  public :: l_pbl_pseudo_surfobst
  public :: l_pbl_pseudo_surfobsq
  public :: l_pbl_pseudo_surfobsuv
  public :: pblh_ration
  public :: pps_press_incr
  public :: l_gsd_limit_ocean_q
  public :: l_pw_hgt_adjust
  public :: l_limit_pw_innov
  public :: max_innov_pct
  public :: l_cleansnow_warmts
  public :: l_conserve_thetav
  public :: r_cleansnow_warmts_threshold
  public :: i_conserve_thetav_iternum
  public :: l_gsd_soiltq_nudge
  public :: l_cld_bld
  public :: cld_bld_hgt
  public :: build_cloud_frac_p
  public :: clear_cloud_frac_p
  public :: nesdis_npts_rad
  public :: iclean_hydro_withRef
  public :: iclean_hydro_withRef_allcol
  public :: i_use_2mq4b
  public :: i_use_2mt4b
  public :: i_sfct_gross
  public :: i_gsdcldanal_type
  public :: i_gsdsfc_uselist
  public :: i_lightpcp
  public :: l_use_hydroretrieval_all
  public :: l_numconc
  public :: l_closeobs
  public :: i_coastline
  public :: i_gsdqc
  public :: qv_max_inc
  public :: ioption
  public :: l_precip_clear_only
  public :: l_fog_off
  public :: cld_bld_coverage
  public :: cld_clr_coverage
  public :: i_cloud_q_innovation
  public :: i_ens_mean
  public :: DTsTmax 
  public :: i_T_Q_adjust
  public :: l_saturate_bkCloud
  public :: l_rtma3d
  public :: i_precip_vertical_check
  public :: corp_howv, hwllp_howv
  public :: i_howv_3dda
  public :: corp_gust, hwllp_gust, oerr_gust
  public :: i_gust_3dda

  logical l_hydrometeor_bkio
  real(r_kind)  dfi_radar_latent_heat_time_period
  real(r_kind)  metar_impact_radius
  real(r_kind)  metar_impact_radius_lowCloud
  logical l_metar_impact_radius_change
  real(r_kind)  metar_impact_radius_max
  real(r_kind)  metar_impact_radius_min
  real(r_kind)  metar_impact_radius_max_height
  real(r_kind)  metar_impact_radius_min_height
  logical l_gsd_terrain_match_surfTobs
  logical l_sfcobserror_ramp_t
  logical l_sfcobserror_ramp_q
  logical l_pbl_pseudo_surfobst
  logical l_pbl_pseudo_surfobsq
  logical l_pbl_pseudo_surfobsuv
  logical l_gsd_limit_ocean_q
  logical l_use_hydroretrieval_all
  real(r_kind)  pblh_ration
  real(r_kind)  pps_press_incr
  logical l_pw_hgt_adjust
  logical l_limit_pw_innov
  real(r_kind) max_innov_pct
  logical l_cleansnow_warmts
  logical l_conserve_thetav
  real(r_kind)    r_cleansnow_warmts_threshold
  integer(i_kind) i_conserve_thetav_iternum
  logical l_gsd_soiltq_nudge
  logical l_cld_bld
  real(r_kind) cld_bld_hgt
  real(r_kind) build_cloud_frac_p 
  real(r_kind) clear_cloud_frac_p 
  integer(i_kind)      nesdis_npts_rad 
  integer(i_kind)      iclean_hydro_withRef
  integer(i_kind)      iclean_hydro_withRef_allcol
  integer(i_kind)      i_use_2mq4b
  integer(i_kind)      i_use_2mt4b
  integer(i_kind)      i_sfct_gross
  integer(i_kind)      i_gsdcldanal_type
  integer(i_kind)      i_gsdsfc_uselist 
  integer(i_kind)      i_lightpcp
  logical              l_numconc
  logical              l_closeobs
  integer(i_kind)      i_coastline
  integer(i_kind)      i_gsdqc
  real(r_kind)         qv_max_inc
  integer(i_kind)      ioption
  logical              l_precip_clear_only
  logical              l_fog_off
  real(r_kind)         cld_bld_coverage
  real(r_kind)         cld_clr_coverage
  integer(i_kind)      i_cloud_q_innovation
  integer(i_kind)      i_ens_mean
  real(r_kind)         DTsTmax
  integer(i_kind)      i_T_Q_adjust
  logical              l_saturate_bkCloud
  logical              l_rtma3d
  integer(i_kind)      i_precip_vertical_check
  real(r_kind)      :: corp_howv, hwllp_howv
  integer(i_kind)   :: i_howv_3dda
  real(r_kind)      :: corp_gust, hwllp_gust, oerr_gust
  integer(i_kind)   :: i_gust_3dda

contains

  subroutine init_rapidrefresh_cldsurf
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  init_rapidrefresh_cldsurf
! prgmmr:  Ming Hu             org: GSD/AMB           date: 2008-06-04
!
! abstract:  set defaults for RR related variables
!
! program history log:
!   2008-06-03  Hu        initial build for cloud analysis
!   2010-03-29  Hu        change names to init_rapidrefresh_cldsurf
!   2011--5-04  Todling   inquire MetGuess for presence of hyrometeors & set default
!   2023-07-30  Zhao      added code for initialization of some variables used
!                         in analysis of significant wave height
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  liunx cluster (Wjet)
!
!$$$
    use kinds, only: i_kind 
    use gsi_metguess_mod, only: gsi_metguess_get
    use mpimod, only: mype
    use state_vectors, only: ns2d,svars2d

    implicit none
    integer(i_kind) ivar,i,ier
    integer(i_kind) i2
    logical have_hmeteor(5)
    character(len=2),parameter :: hydrometeors(5) = (/ 'qi', &
                                                       'ql', &
                                                       'qr', &
                                                       'qs', &
                                                       'qg'  &
                                                       /)

!   Set logical flag
    dfi_radar_latent_heat_time_period = 30.0_r_kind   ! in minutes
    metar_impact_radius = 10.0_r_kind                 ! in grid
    metar_impact_radius_lowCloud = 4.0_r_kind         ! in grid
    l_metar_impact_radius_change = .false.            ! .true. =radius change vertically
    metar_impact_radius_max        = 50000.0_r_kind   ! in meter
    metar_impact_radius_min        = 20000.0_r_kind   ! in meter
    metar_impact_radius_max_height = 3000.0_r_kind    ! in meter
    metar_impact_radius_min_height = 200.0_r_kind     ! in meter
    l_gsd_terrain_match_surfTobs = .false.            ! .true. = turn on GSD terrain 
                                                      !          match for  surface
                                                      !          temperature observation

!   Figure out if hydrometeors are available in guess, if so, do cloud adjustment as default
    if(size(hydrometeors) == 0)then
       l_hydrometeor_bkio = .false.
    else
      do i=1,size(hydrometeors)
         call gsi_metguess_get ('var::'//trim(hydrometeors(i)),ivar,ier)
         have_hmeteor(i) = (ivar>0)
      enddo
      l_hydrometeor_bkio = all(have_hmeteor)
    end if

    l_sfcobserror_ramp_t  = .false.  ! .true. = turn on GSD surface temperature observation error adjustment
    l_sfcobserror_ramp_q  = .false.  ! .true. = turn on GSD surface moisture observation error adjustment
    l_pbl_pseudo_surfobst  = .false.                  ! .true. = turn on PBL pseudo-obs T
    l_pbl_pseudo_surfobsq  = .false.                  ! .true. = turn on PBL pseudo-obs Q
    l_pbl_pseudo_surfobsuv = .false.                  ! .true. = turn on PBL pseudo-obs UV
    pblh_ration = 0.75_r_kind                         ! in percent
    pps_press_incr = 30.0_r_kind                      ! in hPa
    l_gsd_limit_ocean_q    = .false.                      ! .true. = turn on limitation of Q over ocean
    l_pw_hgt_adjust    = .false.                      ! .true. = turn on PW obs height adjustment
    l_limit_pw_innov   = .false.                      ! .true. = turn on limit for size of PW innovation
    max_innov_pct      = 0.1_r_kind                   ! in percent of background PW
    l_cleansnow_warmts = .false.                      ! .true. = turn on limitation of using snow when Ts>5
    l_conserve_thetav  = .false.                      ! .true. = turn on conserving thetaV
    r_cleansnow_warmts_threshold = 8.0_r_kind         ! Ts threshold for cleaning snow
    i_conserve_thetav_iternum = 3                     ! iteration number for conserving thetaV
    l_gsd_soiltq_nudge = .false.                      ! .true. = turn on soil T and Q nudge
    l_cld_bld          = .false.                      ! .true. = turn on GOES cloud building
    cld_bld_hgt        = 1200.0_r_kind                ! Height (meters) below which to build clouds
    build_cloud_frac_p = 0.95_r_kind                  ! threshold for building cloud from GOES
    clear_cloud_frac_p = 0.10_r_kind                  ! threshold for clearing cloud from GOES
    nesdis_npts_rad  = 1                              !  NESDIS impact radius
    iclean_hydro_withRef = 1                          ! clean based on ref
    iclean_hydro_withRef_allcol = 0                   ! don't clean whole column
    i_use_2mq4b = 0                                   ! 1 = Use 2m Q as part of B
    i_use_2mt4b = 0                                   ! 1 = Use 2m T as part of B
    i_sfct_gross = 0                                  ! 1 = Use extended gross check for sfc T
    i_gsdcldanal_type  = 0                            !  turn cloud analysis off
    i_gsdsfc_uselist   = 0                            !  turn gsd surface uselist off           
    i_lightpcp         = 0                            !  don't add light pcp over warm section           
    l_use_hydroretrieval_all=.false.
    l_numconc = .false.                               ! .true. = update number concentration  
    l_closeobs = .false.                              ! .true. = pick the obs close to analysis time
    i_coastline = 0                                   !  turn coastline surface observation operator off  
    i_gsdqc  = 0                                      !  turn gsd obs QC off
    qv_max_inc   = 0.005_r_kind                       ! maximum water vapor increment in kg/kg
    ioption  = 2                                      ! default is median of samples
    l_precip_clear_only = .false.                     ! .true. only use precip to clear
    l_fog_off = .false.                               ! .true. is to turn off fog updates
    cld_bld_coverage    = 0.6_r_kind                  ! Percentage of cloud coverage for building qc/qi
    cld_clr_coverage    = 0.6_r_kind                  ! Percentage of cloud coverage for clearing qc/qi
    i_cloud_q_innovation = 0                          ! 0 = no increments from cloud obs
    i_ens_mean = 0                                    ! typical ob behavior
    DTsTmax = 20.0_r_kind                             ! maximum allowed difference between Ts and T 1st level
    i_T_Q_adjust= 1                                   ! temperature and moisture are adjusted
    l_saturate_bkCloud= .true.
    l_rtma3d            = .false.                     ! turn configuration for rtma3d off          
    i_precip_vertical_check = 0                       ! No check and adjustment to retrieved Qr/Qs/Qg (default)
    corp_howv           = 0.42_r_kind                 ! 0.42 meters (default)
    hwllp_howv          = 170000.0_r_kind             ! 170,000.0 meters (170km as default for 3DRTMA, 50km is used in 2DRTMA)
    i_howv_3dda         = 0                           ! no analysis of significant wave height (howv) in 3D analysis (default)
    corp_gust           = -1.50_r_kind                ! initialised as negative & void to be skipped, in order to use
                                                      ! the value (3.0 m/s) set in sub berror_read_wgt_reg (as default).
                                                      ! If user-specified value is preferred, set it in session
                                                      ! "rapidrefresh_cldsurf" of GSI namelist file

    hwllp_gust          = -90000.0_r_kind             ! initialised as a value, in order to skip this negative value
                                                      ! and to use the value (used for q) set in sub berror_read_wgt_reg.
                                                      ! If user-specified value is preferred, set it in session
                                                      ! "rapidrefresh_cldsurf" of GSI namelist file

    oerr_gust           = -2.5_r_kind                 ! initialised as a negative value, in order to skip this negative value
                                                      ! and to use the value (1.0 m/s) set in read_prepbufr.f90
                                                      ! If user-specified value is preferred, set it in session
                                                      ! "rapidrefresh_cldsurf" of GSI namelist file

    i_gust_3dda         = 0                           ! no analysis of wind gust (gust) in 3D analysis (default)

!-- searching for specific variable in state variable list (reading from anavinfo)
    do i2=1,ns2d
      if ( trim(svars2d(i2))=='howv' .or. trim(svars2d(i2))=='HOWV'   ) then
        i_howv_3dda = 1
        if ( mype == 0 ) then
          write(6,'(1x,A,1x,A8,1x,A,1x,I4)')"init_rapidrefresh_cldsurf: anavinfo svars2d (state variable): ",trim(adjustl(svars2d(i2))), " is found in anavinfo, set i_howv_3dda = ", i_howv_3dda
        end if
      end if
      if ( trim(svars2d(i2))=='gust' .or. trim(svars2d(i2))=='GUST'   ) then
        i_gust_3dda = 1
        if ( mype == 0 ) then
          write(6,'(1x,A,1x,A8,1x,A,1x,I4)')"init_rapidrefresh_cldsurf: anavinfo svars2d (state variable): ",trim(adjustl(svars2d(i2))), " is found in anavinfo, set i_gust_3dda = ", i_gust_3dda
        end if
      end if
    end do ! i2 : looping over 2-D anasv

    return
  end subroutine init_rapidrefresh_cldsurf

end module rapidrefresh_cldsurf_mod
