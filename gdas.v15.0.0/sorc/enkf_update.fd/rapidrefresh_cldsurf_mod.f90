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
! 
! Subroutines Included:
!   sub init_rapidrefresh_cldsurf  - initialize RR related variables to default values
!
! Variable Definitions:
!   def l_cloud_analysis    - namelist logical for cloud analysis (=true) 
!   def dfi_radar_latent_heat_time_period - DFI forward integration window in minutes
!   def metar_impact_radius - impact radius for METAR cloud observation
!   def metar_impact_radius_lowCloud - impact radius for METAR cloud observation
!                                      that indicate low cloud base
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
!                         =1.  cloud analysis after var analysis
!                         =5.  skip cloud analysis and NETCDF file update
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
  public :: l_cloud_analysis 
  public :: dfi_radar_latent_heat_time_period
  public :: metar_impact_radius
  public :: metar_impact_radius_lowCloud
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

  logical l_cloud_analysis
  real(r_kind)  dfi_radar_latent_heat_time_period
  real(r_kind)  metar_impact_radius
  real(r_kind)  metar_impact_radius_lowCloud
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
    implicit none
    integer(i_kind) ivar,i,ier
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
    l_gsd_terrain_match_surfTobs = .false.            ! .true. = turn on GSD terrain 
                                                      !          match for  surface
                                                      !          temperature observation

!   Figure out if hydrometeors are available in guess, if so, do cloud adjustment as default
    if(size(hydrometeors) == 0)then
       l_cloud_analysis = .false.
    else
      do i=1,size(hydrometeors)
         call gsi_metguess_get ('var::'//trim(hydrometeors(i)),ivar,ier)
         have_hmeteor(i) = (ivar>0)
      enddo
      l_cloud_analysis = all(have_hmeteor)
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

    return
  end subroutine init_rapidrefresh_cldsurf

end module rapidrefresh_cldsurf_mod
