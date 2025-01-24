module qcmod
!$$$  module documentation block
!                .      .    .                                       .
! module:    qcmod
!   prgmmr: kleist           org: w/nmc20             date: 2003-09-30
!
! abstract: module containing data quality control variables
!
! program history log:
!   2003-09-30  kleist
!   2004-05-18  kleist, documentation
!   2004-07-23  derber - modify to include conventional sst
!   2004-10-12  parrish - modifications for nonlinear qc
!   2004-12-02  treadon - initialize b_ref and pg_ref
!   2005-01-20  okamoto - add ermax for ssmi/amsre/ssmis
!   2005-04-07  treadon - add logical flags to indicate nonlinear qc
!                         is on (=.true.) or off (=.false.)
!   2005-05-27  derber  - level output change
!   2005-08-03  derber  - remove qc parameters for conventional data
!   2005-09-29  derber  - remove qc parameters for sat and pcp data, move cg_term to constants 
!   2006-01-31  derber  - correct bug in upprof and dwprof loop logical test
!   2006-05-24  treadon - add vadfile to carry name of vad wind bufr file
!   2006-05-22  su - add noiqc flag
!   2006-07-28  derber  - add dfact1, initialize
!   2006-08-07  treadon - remove nlnqc_oz (not used)
!   2007-04-16       su - add c_varqc for determining the spped to turn on var. qc
!   2008-06-03  treadon - add use_poq7
!   2011-04-03  li      - (1) Add setup_tzr_qc, tz_retrieval for Tz retrieval.  Add QC with Tzr to some QC subroutines
!                       - (2) Introduce tzr_qc to control QC with Tz retrieval
!                       - (3) Modify QC subroutines by adding a few dummy variables for Tz retrieval
!   2011-02-17  todling - add parameter to control O3 Jacobian from IR instruments
!   2011-05-05  mccarty - removed declaration and assignment of repe_dw
!   2011-05-20  mccarty - add qc_atms routine
!   2011-07-08  collard - reverse relaxation of AMSU-A Ch 5 QC introduced at revision 5986.
!   2012-11-10  s.liu   - add logical variable newvad to identify new and old vad wind
!   2013-05-07  tong    - add logical variable tdrerr_inflate for tdr obs err
!                         inflation and tdrgross_fact to adjust tdr gross error
!   2013-07-19  zhu     - tighten quality control for amsua surface sensitive channels when emiss_bc=.t.
!   2013-10-27  todling - add create/destroy
!   2014-01-09  mccarty - do not apply qc to wv channels for amsub (lower quality than mhs)
!   2014-04-27  eliu    - add two qc flags for AMSUA/ATMS precipitation screening 
!   2014-10-10  Akella & Jin - add sfc_speed (wind speed) so as to QC obs at (high) wind speed locations
!   2014-11-18  Akella & Jin - add qc based on surface height. Reject data over water and where abs value of surface height > 0.01
!                              Changed qc_ssmi & qc_avhrr only.
!   2014-05-29  thomas  - add lsingleradob functionality rejection flag
!                         (originally of mccarty)
!   2014-10-06  carley  - add logicals for buddy check
!   2015-01-15  zhu     - apply emissivity sensitivity screening to all-sky AMSUA radiance
!   2015-01-16  ejones  - added qc_gmi
!   2015-03-11  ejones  - added qc_amsr2
!   2015-03-23  ejones  - added qc_saphir
!   2015-03-26  m.kim   - apply extra optional qc for MHS and AIRS using iextra in satinfo file
!   2015-03-31  zhu     - observation error adjustments based on mis-matched
!                         cloud info, diff_clw, scattering and surface wind
!                         speed for AMSUA/ATMS cloudy radiance assimilation
!   2015-05-01  ejones  - modify emissivity regression and check in qc_gmi
!   2015-09-04  J.Jung  - Added mods for CrIS full spectral resolution (FSR)
!   2015-05-29  ejones  - tighten clw threshold for qc_gmi 
!   2015-09-04  J.Jung  - Added mods for CrIS full spectral resolution (FSR)
!   2015-09-20  zhu     - use radmod to generalize cloud and aerosol usages in radiance assimilation
!   2015-09-30  ejones  - add sun glint check in qc_amsr2 
!   2016-04-18  yang    - add logical closest_obs for choosing the obs. timely
!                         closest to the analysis time from multiple surface obs. at a station.
!   2016-05-22  zhu     - add errormod_aircraft
!   2016-09-16  tong    - Remove tdrgross_fact (not used)
!   2016-10-13  zhu     - modified qc_amsua for all-sky ATMS
!   2016-10-20  acollard- Ensure AMSU-A channels 1-6,15 are not assimilated if
!                         any of these are missing.
!   2016-11-22  sienkiewicz - fix a couple of typos in HIRS qc
!   2016-12-14  lippi   - add nml option vadwnd_l2rw_qc.
!   2016-10-13  zhu     - modified qc_amsua for all-sky ATMS
!   2018-02-21  yang    - add namelist variables used in module nltransf 
!   2018-03-22  yang    - remove "logical closest_obs", previously applied to the analysis
!                         of vis and cldch. the option to use the closest ob to the
!                         analysis time only is now handled by Ming Hu's "logical l_closeobs"
!                         for all variables
!   2019-03-27  h. liu  - add ABI QC
!   2019-06-10  h. liu - add Geostationary satellites CSR data QC to replace qc_abi,qc_seviri
!   2019-09-29  X.Su   - add troflg and lat_c for hilbert curve tunning
!   2019-04-19  eliu    - add QC flag for cold-air outbreak 
!   2021-04-29  Jung/Collard - Fix numerics for emissivity check
!
! subroutines included:
!   sub init_qcvars
!   sub create_qcvars
!   sub destroy_qcvars
!   sub errormod
!   sub errormod_hdraob
!   sub errormod_aircraft
!   sub setup_tzr_qc    - set up QC with Tz retrieval
!   sub tz_retrieval    - Apply Tz retrieval
!   sub qc_ssmi         - qc ssmi data
!   sub qc_ssu          - qc ssu data
!   sub qc_avhrr        - qc avhrr data
!   sub qc_goesimg      - qc goesimg data
!   sub qc_msu          - qc msu data
!   sub qc_irsnd        - qc ir sounder data (hirs,goesndr,iasi,airs,cris, cris-fsr)
!   sub qc_amsua        - qc amsua data
!   sub qc_mhs          - qc msu, amsub and hsb data
!   sub qc_atms         - qc atms data
!   sub qc_gmi          - qc gmi data
!   sub qc_amsr2        - qc amsr2 data
!   sub qc_saphir       - qc saphir data
!   sub qc_geocsr       - qc goestationary satellite data
!
! remarks: variable definitions below
!   def dfact           - factor for duplicate obs at same location for conv. data
!   def dfact1          - time factor for duplicate obs at same location for conv. data
!   def erradar_inflate - radar error inflation factor
!   def tdrerr_inflate  - logical variable to inflate obs error for tdr data
!   def npres_print     - number of levels for print
!   def ptop,pbot       - arrays containing top pressure and bottom pressure of print levels
!   def ptopq,pbotq     - arrays containing top pressure and bottom pressure of print levels for q
!   def ptopo3,pboto3   - arrays containing top pressure and bottom pressure of print levels for o3 levels
!   def vadfile         - local name of bufr file containing vad winds (used by read_radar)
!   def use_poq7        - if true, accept sbuv/2 obs with profile ozone quality flag 7
!   def cao_check       - if true, turn on cold-air-outbreak screening
!   def airs_cads       - if true, use the cloud and aerosol detection routine for Aqua/AIRS instrument
!   def cris_cads       - if true, use the cloud and aerosol detection routine for CrIS instruments
!   def iasi_cads       - if true, use the cloud and aerosol detection routine for IASI instruments
!   def iasing_cads     - if true, use the cloud and aerosol detection routine for IASI-NG instruments
!
! following used for nonlinear qc:
!
!   def nlnqc_iter   - logical flag (T=nonlinear qc on, F=nonlinear qc off) for iteration
!   def njqc -  logical flag (T=Purser's nonlinear qc on, F=off)
!   def vqc -  logical flag (T=EC vqc on, F=off)
!   def nvqc -  logical flag (T=new vqc on, F=off)
!   def hub_norm -  logical flag (T=mix vqc model, F=logistic model)
!   def noiqc        - logic flag for oiqc, noiqc='false' with oiqc on
!
! following used for NonLinear TRansformation to visibility and ceiling height
!   def pvis         - power value in non-linear transformation for vis
!   def pcldch       - power value in non-linear transformation for cldch
!   def scale_cv     - scaling constant in meter
!   def estvisoe     - prescribed obs vis error 
!   def estcldchoe   - prescribed obs cldch error 
!   def vis_thres    - threshold value for vis
!   def cldch_thres  - threshold value for cldch
!
!   this for hilbert curve tunning
!   def troflg       - if tro flg is on different region will have different 
!                       down weighting criteria when appling HIlbert curve
!   def lat_c        -  the latitude criteria for different down weighting
!                        criteria
!   def nrand        - hilbert premeter
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind,r_double
  use constants, only: zero,quarter,half,one,two,three,four,five,tiny_r_kind,rd,grav
  use constants, only: r0_01,r0_02,r0_03,r0_04,r0_05,r10,r60,r100,h300,r400,r1000,r2000,r2400,r4000
  use constants, only: deg2rad,rad2deg,t0c,one_tenth,rearth_equator
  use obsmod, only: rmiss_single
  use radinfo, only: iuse_rad,passive_bc,nuchan
  use radinfo, only: tzr_qc
  use radiance_mod, only: rad_obs_type
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_qcvars
  public :: create_qcvars
  public :: destroy_qcvars
  public :: errormod
  public :: errormod_hdraob
  public :: errormod_aircraft
  public :: setup_tzr_qc
  public :: qc_ssmi
  public :: qc_ssu
  public :: qc_goesimg
  public :: qc_geocsr
  public :: qc_msu
  public :: qc_irsnd
  public :: qc_avhrr
  public :: qc_amsua
  public :: qc_mhs
  public :: qc_atms
  public :: qc_noirjaco3
  public :: qc_noirjaco3_pole
  public :: qc_satwnds
  public :: qc_gmi
  public :: qc_amsr2
  public :: qc_saphir

! set passed variables to public
  public :: npres_print,nlnqc_iter,varqc_iter,pbot,ptop,c_varqc,njqc,vqc,nvqc,hub_norm
  public :: use_poq7,noiqc,vadfile,dfact1,dfact,erradar_inflate,gps_jacqc
  public :: pboto3,ptopo3,pbotq,ptopq,newvad,tdrerr_inflate
  public :: igood_qc,ifail_crtm_qc,ifail_satinfo_qc,ifail_interchan_qc,&
            ifail_gross_qc,ifail_cloud_qc,ifail_outside_range,&
            ifail_scanedge_qc, ifail_emiss_qc, ifail_cao_qc
  public :: ifail_iland_det, ifail_isnow_det, ifail_iice_det, ifail_iwater_det,&
            ifail_imix_det, ifail_iomg_det, ifail_isst_det, ifail_itopo_det,&
            ifail_iwndspeed_det
  public :: cao_check 
  public :: buddycheck_t,buddydiag_save
  public :: vadwnd_l2rw_qc
  public :: pvis,pcldch,scale_cv,estvisoe,estcldchoe,vis_thres,cldch_thres
  public :: troflg
  public :: lat_c
  public :: nrand 
  public :: airs_cads, cris_cads, iasi_cads, iasing_cads

  logical nlnqc_iter,njqc,vqc,nvqc,hub_norm
  logical noiqc
  logical use_poq7
  logical qc_noirjaco3
  logical qc_noirjaco3_pole
  logical gps_jacqc
  logical newvad
  logical tdrerr_inflate
  logical qc_satwnds
  logical buddycheck_t
  logical buddydiag_save
  logical vadwnd_l2rw_qc
  logical troflg
  logical cao_check
  logical airs_cads, cris_cads, iasi_cads, iasing_cads

  character(10):: vadfile
  integer(i_kind) npres_print
  integer(i_kind) nrand
  real(r_kind) dfact,dfact1,erradar_inflate,c_varqc
  real(r_kind) varqc_iter
  real(r_kind) lat_c
  real(r_kind) pvis,pcldch,scale_cv,estvisoe,estcldchoe,vis_thres,cldch_thres
  real(r_kind),allocatable,dimension(:)::ptop,pbot,ptopq,pbotq,ptopo3,pboto3

! Declare variables for QC with Tz retrieval
  real(r_kind), private :: e_ts,e_ta,e_qa
  real(r_kind), private :: tzchk      ! threshold of Tz retrieval increment for qc_tzr
  real(r_kind), private :: tschk      ! threshold of d(Tb)/d(Ts) for channels selection in SST/Tz retrieval

!  Definition of id_qc flags
!  Good Observations (0)
  integer(i_kind),parameter:: igood_qc=0

!  SETUPRAD or general flags (0-10)
!  Reject due to flag in radinfo in setuprad
  integer(i_kind),parameter:: ifail_satinfo_qc=1
!  Failure in CRTM in setuprad
  integer(i_kind),parameter:: ifail_crtm_qc=2
!  Reject due to gross check failure in setuprad
  integer(i_kind),parameter:: ifail_gross_qc=3
!  Reject due to interchannel check (i.e., if one channel fails in group whole group thrown out) in setuprad
  integer(i_kind),parameter:: ifail_interchan_qc=4
!  Reject due to not using over this surface in qc routine
  integer(i_kind),parameter:: ifail_surface_qc=5
!  Reject due to gross check in specific qc routine                                                                          
  integer(i_kind),parameter:: ifail_gross_routine_qc=6
!  Reject due to cloud > limit for channel in qc routine
  integer(i_kind),parameter:: ifail_cloud_qc=7
!  Reject due to inaccurate emissivity/surface temperature estimate in qc routine
  integer(i_kind),parameter:: ifail_emiss_qc=8
!  Reject due to observations being out of range in qc routine
  integer(i_kind),parameter:: ifail_range_qc=9
!  Reject because outside the range of lsingleradob
  integer(i_kind),parameter:: ifail_outside_range=11
!  Reject due to cold-air outbreak area check  in setuprad
  integer(i_kind),parameter:: ifail_cao_qc=12
!  Failures specific to qc routine start at 50 and the numbers overlap
!  QC_SSMI failures 
!  Reject due to krain type not equal to 0 in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_krain_qc=50
!  Reject due to ierrret > 0 in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_ierrret_qc=51
!  Reject due to tpwc < 0 in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_tpwc_qc=52
!  Reject due to sgagl < 25. and amsre_low in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_sgagl_qc=53
!  Reject in topography check in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_topo_ssmi_qc=54
!  Reject because varinv < tiny in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_varinv_qc=55
!  Reject because ch2 check in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_ch2_qc=56
!  Reject because scattering over land in subroutine qc_ssmi
  integer(i_kind),parameter:: ifail_scatt_qc=57

!  QC_GMI failures
!  Reject due to krain type not equal to 0 in subroutine qc_gmi
  integer(i_kind),parameter:: ifail_krain_gmi_qc=50
!  Reject scan edges
  integer(i_kind),parameter:: ifail_scanedge_qc=51
!  Reject S1 swath edges
  integer(i_kind),parameter:: ifail_gmi_swathedge_qc=52
!  Reject if latitude is outside of 55N - 55S
  integer(i_kind),parameter:: ifail_lat_qc=53

! QC_AMSR2 failures
!  Reject due to krain type not equal to 0 in subroutine qc_amsr2
  integer(i_kind),parameter:: ifail_krain_amsr2_qc=50
!  Reject due to sun glint in subroutine qc_amsr2
  integer(i_kind),parameter:: ifail_amsr2_glint_qc=51

! QC_SAPHIR failures
!  Reject due to krain type not equal to 0 in subroutine qc_saphir
  integer(i_kind),parameter:: ifail_krain_saphir_qc=50

! QC_IRSND        
!  Reject because wavenumber > 2400 in subroutine qc_irsnd
  integer(i_kind),parameter:: ifail_2400_qc=50
!  Reject because wavenumber > 2000 in subroutine qc_irsnd
  integer(i_kind),parameter:: ifail_2000_qc=51
!  Reject because goes sounder and satellite zenith angle > 60 in subroutine qc_irsnd
  integer(i_kind),parameter:: ifail_satzen_qc=52
!  Reject because of surface emissivity/temperature influence in subroutine qc_irsnd                                     
  integer(i_kind),parameter:: ifail_sfcir_qc=53

! QC_AMSUA          
!  Reject because factch6 > limit in subroutine qc_amsua
  integer(i_kind),parameter:: ifail_factch6_qc=50
!  Reject because factch4 > limit in subroutine qc_amsua
  integer(i_kind),parameter:: ifail_factch4_qc=51
!  Reject because sval > limit in subroutine qc_amsua over open water          
  integer(i_kind),parameter:: ifail_sval_qc=52                         
!  Reject because factch5 > limit in subroutine qc_amsua over open water      
  integer(i_kind),parameter:: ifail_factch5_qc=53                       

! QC_ATMS
!  Reject because factch1617 > limit in subroutine qc_amsua over open water
  integer(i_kind),parameter:: ifail_factch1617_qc=54

! QC_MHS          
!  Reject because fact1 > limit in subroutine qc_mhs
  integer(i_kind),parameter:: ifail_fact1_qc=50

! OPTIONAL EXTRA QC
!  Reject because of iland_det
  integer(i_kind),parameter:: ifail_iland_det=61
!  Reject because of isnow_det
  integer(i_kind),parameter:: ifail_isnow_det=62
!  Reject because of iice_det
  integer(i_kind),parameter:: ifail_iice_det=63
!  Reject because of iwater_det
  integer(i_kind),parameter:: ifail_iwater_det=64
!  Reject because of imix_det
  integer(i_kind),parameter:: ifail_imix_det=65
!  Reject because of iomg_det
  integer(i_kind),parameter:: ifail_iomg_det=66
!  Reject because of isst_det
  integer(i_kind),parameter:: ifail_isst_det=67
!  Reject because of itopo_det
  integer(i_kind),parameter:: ifail_itopo_det=68
!  Reject because of iwndspeed_det
  integer(i_kind),parameter:: ifail_iwndspeed_det=69

! QC_SSU          

! QC_MSU          

! QC_seviri          
! Reject because terrain height > 1km.
  integer(i_kind),parameter:: ifail_terrain_qc=50 

! QC_geocsr       
! Reject because of standard deviation in subroutine qc_geocsr
  integer(i_kind),parameter:: ifail_std_geocsr_qc=50

! QC_avhrr          
!  Reject because of too large surface temperature physical retrieval in qc routine: tz_retrieval (see tzr_qc)
  integer(i_kind),parameter:: ifail_tzr_qc=10
!  Reject because abs(sfc hgt) > 0.01m above water.
  integer(i_kind),parameter:: ifail_sfchgt=60

! Also used (shared w/ other qc-codes):
!  ifail_2400_qc=50
!  ifail_2000_qc=51
!  ifail_cloud_qc=7
!  ifail_sfcir_qc=53
!  ifail_sfchgt=60

! QC_goesimg          
!  Reject because of standard deviation in subroutine qc_goesimg
  integer(i_kind),parameter:: ifail_std_goesimg_qc=50 

contains
 
  subroutine init_qcvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_qcvars
!   prgmmr: kleist           org: np20                date: 2003-09-30
!
! abstract: initialize variables used in data quality control
!
! program history log:
!   2003-09-30  kleist
!   2004-05-18  kleist, documentation
!   2004-07-23  derber  - modify to include conventional sst
!   2005-01-20  okamoto - add ermax for ssmi/amsre/ssmis
!   2005-02-18  treadon - reduce ps gross limit from 10.0 to 5.0
!   2005-03-08  cucurull - reduce gps ro gross limit from 10.0 to 3.0
!   2005-06-03  cucurull - increase gps ro gross limit from 3.0 to 10.0
!   2007-01-09  sienkiewicz - new levels for ozone stat printout
!   2008-04-23  safford  - rm unused parameter
!   2008-09-05  lueken   - merged ed's changes into q1fy09 code
!   2012-07-19  todling - add qc_satwnds to allow bypass of satwind qc
!   2013-10-27  todling - move alloc space to create_qcvars
!   2014-10-06  carley - add logicals for buddy check
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    npres_print = 12
    
    dfact    = zero
    dfact1   = three
    varqc_iter=one

    erradar_inflate   = one
    tdrerr_inflate    = .false.

    nlnqc_iter= .false.
    noiqc = .false.
    njqc=.false.
    vqc=.false.
    nvqc=.false.
    hub_norm=.true.
    c_varqc=one

    vadfile='none'

    use_poq7 = .false.
    cao_check = .false.

    gps_jacqc = .false.     ! Jacobian QC for GNSS RO is off by default

    qc_noirjaco3 = .false.  ! when .f., use O3 Jac from IR instruments
    qc_noirjaco3_pole = .false. ! true=do not use O3 Jac from IR instruments near poles

    qc_satwnds=.true. ! default: remove lots of SatWind at mid-tropospheric levels

    buddycheck_t=.false.   ! When true, run buddy check algorithm on temperature observations
    buddydiag_save=.false. ! When true, output files containing buddy check QC info for all
                           !  obs run through the buddy check

    vadwnd_l2rw_qc=.true.  ! When false, DO NOT run the vadwnd qc on level 2 radial wind obs.
    pvis=one
    pcldch=one
    scale_cv=one
    estvisoe=one
    estcldchoe=one
    vis_thres=16000.0_r_kind
    cldch_thres=16000.0_r_kind

    troflg=.false.
    lat_c=21.0_r_kind
    nrand=13

    airs_cads   = .false.
    cris_cads   = .false.
    iasi_cads   = .false.
    iasing_cads = .false.

    return
  end subroutine init_qcvars

  subroutine create_qcvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_qcvars
!   prgmmr: todling          org: np20                date: 2013-10-27
!
! abstract: allocate memory used in data quality control
!
! program history log:
!   2013-10-27  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    allocate(ptop(npres_print),pbot(npres_print),ptopq(npres_print), &
             pbotq(npres_print),ptopo3(npres_print),pboto3(npres_print))
    
! Set pressure level groupings.  There are npres_print groupings
    ptop(1) = r1000       ;    pbot(1)=  1200.0_r_kind
    ptop(2) = 900.0_r_kind;    pbot(2)=  ptop(1)
    ptop(3) = 800.0_r_kind;    pbot(3)=  ptop(2)
    ptop(4) = 600.0_r_kind;    pbot(4)=  ptop(3)
    ptop(5) = 400.0_r_kind;    pbot(5)=  ptop(4)
    ptop(6) = h300        ;    pbot(6)=  ptop(5)
    ptop(7) = 250.0_r_kind;    pbot(7)=  ptop(6)
    ptop(8) = 200.0_r_kind;    pbot(8)=  ptop(7)
    ptop(9) = 150.0_r_kind;    pbot(9)=  ptop(8)
    ptop(10)= 100.0_r_kind;    pbot(10)= ptop(9)
    ptop(11)= 50.0_r_kind ;    pbot(11)= ptop(10)
    ptop(12)= zero        ;    pbot(12)= 2000.0_r_kind

    ptopq(1)=  r1000       ;   pbotq(1)=  1200.0_r_kind
    ptopq(2)=  950.0_r_kind;   pbotq(2)=  ptopq(1)
    ptopq(3)=  900.0_r_kind;   pbotq(3)=  ptopq(2)
    ptopq(4)=  850.0_r_kind;   pbotq(4)=  ptopq(3)
    ptopq(5)=  800.0_r_kind;   pbotq(5)=  ptopq(4)
    ptopq(6)=  700.0_r_kind;   pbotq(6)=  ptopq(5)
    ptopq(7)=  600.0_r_kind;   pbotq(7)=  ptopq(6)
    ptopq(8)=  500.0_r_kind;   pbotq(8)=  ptopq(7)
    ptopq(9)=  400.0_r_kind;   pbotq(9)=  ptopq(8)
    ptopq(10)= h300        ;   pbotq(10)= ptopq(9)
    ptopq(11)= zero        ;   pbotq(11)= ptopq(10)
    ptopq(12)= zero        ;   pbotq(12)= 2000.0_r_kind

    ptopo3(1) = 120.0_r_kind;  pboto3(1) = h300
    ptopo3(2) =  70.0_r_kind;  pboto3(2) = ptopo3(1)
    ptopo3(3) =  40.0_r_kind;  pboto3(3) = ptopo3(2)
    ptopo3(4) =  25.0_r_kind;  pboto3(4) = ptopo3(3)
    ptopo3(5) =  12.0_r_kind;  pboto3(5) = ptopo3(4)
    ptopo3(6) =   7.0_r_kind;  pboto3(6) = ptopo3(5)
    ptopo3(7) =  four       ;  pboto3(7) = ptopo3(6)
    ptopo3(8) =   2.5_r_kind;  pboto3(8) = ptopo3(7)
    ptopo3(9) =   1.2_r_kind;  pboto3(9) = ptopo3(8)
    ptopo3(10) =  0.7_r_kind;  pboto3(10)= ptopo3(9)
    ptopo3(11) =  0.4_r_kind;  pboto3(11)= ptopo3(10)
    ptopo3(12) = zero       ;  pboto3(12)= 2000.0_r_kind

    return
  end subroutine create_qcvars

  subroutine destroy_qcvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_qcvars
!   prgmmr: todling          org: np20                date: 2013-10-27
!
! abstract: destroy memory used in data quality control
!
! program history log:
!   2013-10-27  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    deallocate(ptop,pbot,ptopq, &
               pbotq,ptopo3,pboto3)
    
    return
  end subroutine destroy_qcvars

  subroutine setup_tzr_qc(obstype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setup_tzr_qc       set up parameters for QC with Tz retrieval
!   prgmmr: xu li            org: np23                date: 2010-06-01
!
! abstract: set up tz retrieval
!
!   input argument list:
!     obstype - type of tb observation
!
!   output argument list:
!
!$$$ end documentation block

    implicit none

! Define parameters

    character(10), intent(in) :: obstype
!
!   Assign error parameters for background (Ts, Ta, Qa)
!
    e_ts = half; e_ta = one; e_qa = 0.85_r_kind

    tzchk = 10.0_r_kind
    if ( obstype == 'amsre_low' .or. obstype == 'amsre_mid' .or. obstype == 'amsre_hig'  ) then
      tzchk = 0.50_r_kind
    elseif ( obstype == 'amsua' .or. obstype == 'ssmis' .or. obstype == 'ssmi' ) then
      tzchk = 0.12_r_kind
    elseif (  obstype == 'avhrr' .or. obstype == 'avhrr_navy' .or. obstype == 'viirs') then 
      tzchk = 0.85_r_kind
    elseif (  obstype == 'hirs2' .or. obstype == 'hirs3' .or. obstype == 'hirs4' .or. & 
              obstype == 'sndr' .or. obstype == 'sndrd1' .or. obstype == 'sndrd2'.or. &
              obstype == 'sndrd3' .or. obstype == 'sndrd4' .or.  obstype == 'goes_img' .or. &
              obstype == 'ahi' .or. obstype == 'airs' .or. obstype == 'iasi' .or. obstype == 'iasi-ng' .or.&
              obstype == 'cris' .or. obstype == 'cris-fsr' .or. obstype == 'seviri'  .or. obstype == 'abi') then
      tzchk = 0.85_r_kind
    endif

    tschk = 0.20_r_kind 

  end subroutine setup_tzr_qc

  subroutine errormod(pq,vq,levs,plevs,errout,k,presl,dpres,nsig,lim_qm)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    errormod
!   prgmmr: derber           org: np23                date: 2003-09-30
!
! abstract: adjust observation error for conventional obs
!
! program history log:
!   2003-09-30  derber
!   2004-05-18  kleist, documentation
!   2004-10-26  kleist - add 0.5 half-layer factor
!   2006-02-15  treadon - add (l==levs,1) exit to upprof and dwprof loops
!   2006-12-20  Sienkiewicz  multiply tiny_r_kind in errout div-by-zero
!                            check by expected largest value for numerator
!                            max(2*vmax) = max(dpres) ~= 5 cb
!   2008-04-23  safford - rm unused vars and uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!
!   input argument list:
!     pq     - pressure quality mark
!     vq     - observation quality mark (t,q,wind)
!     levs   - number of levels in profile for observation
!     plevs  - observation pressures
!     errout - observation error 
!     k      - observation level 
!     presl  - model pressure at half sigma levels
!     dpres  - delta pressure between model pressure levels
!     nsig   - number of vertical levels
!     lim_qm - qc limit 
!
!   output argument list:
!     errout - adjusted observation error
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    integer(i_kind)                     ,intent(in   ) :: levs,k,nsig,lim_qm
    real(r_kind)   ,dimension(*)        ,intent(in   ) :: plevs
    real(r_kind)   ,dimension(nsig)     ,intent(in   ) :: presl
    real(r_kind)   ,dimension(nsig-1)   ,intent(in   ) :: dpres
    integer(i_kind),dimension(*)        ,intent(in   ) :: pq,vq
    real(r_kind)                        ,intent(inout) :: errout

    integer(i_kind) n,l,ilev
    real(r_kind):: vmag,pdiffu,pdiffd,con
    
    errout=one
    if(levs == 1)return
    ilev=1
    do n=2,nsig-1
       if(plevs(k) < presl(n))ilev=n
    end do
    con=grav*500._r_kind/(273._r_kind*rd)
    vmag=min(max(half*dpres(ilev),r0_02*presl(ilev)),con*plevs(k))

!   vmag=max(half*dpres(ilev),r0_02*presl(ilev))
    pdiffu=vmag
    pdiffd=vmag
    if(pq(k) < lim_qm .and. vq(k) < lim_qm)then
! Move up through the profile.  
       l=k

! Array plevs is only defined from l=1 to l=levs.  Hence the check below
       if (l+1<=levs) then
          upprof: do while (abs(plevs(k)-plevs(l+1)) < vmag .and. l <= levs-1) 
             l=l+1
             if(pq(l) < lim_qm .and. vq(l) < lim_qm)then
                pdiffu=abs(plevs(k)-plevs(l))
                exit upprof
             end if
             if (l==levs) exit upprof
          end do upprof
       endif
        
! Reset the level and move down through the profile
       l=k

! The check (l>=2) ensures that plevs(l-1) is defined
       if (l>=2) then
          dwprof: do while (abs(plevs(l-1)-plevs(k)) < vmag .and. l >= 2) 
             l=l-1
             if(pq(l) < lim_qm .and. vq(l) < lim_qm)then
                pdiffd=abs(plevs(l)-plevs(k))
                exit dwprof
             end if
             if (l==1) exit dwprof
          end do dwprof
       endif

! Set adjusted error
       errout=sqrt(two*vmag/max(pdiffd+pdiffu,five*tiny_r_kind))

! Quality marks indicate bad data.  Set error to large value.
    else
       errout=1.e6_r_kind
    end if

    return
end subroutine errormod
  subroutine errormod_hdraob(pq,vq,levs,plevs,errout,k,presl,dpres,nsig,lim_qm)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    errormod
!   prgmmr: derber           org: np23                date: 2003-09-30
!
! abstract: adjust observation error for conventional obs
!
! program history log:
!   2003-09-30  derber
!   2004-05-18  kleist, documentation
!   2004-10-26  kleist - add 0.5 half-layer factor
!   2006-02-15  treadon - add (l==levs,1) exit to upprof and dwprof loops
!   2006-12-20  Sienkiewicz  multiply tiny_r_kind in errout div-by-zero
!                            check by expected largest value for numerator
!                            max(2*vmax) = max(dpres) ~= 5 cb
!   2008-04-23  safford - rm unused vars and uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!
!   input argument list:
!     pq     - pressure quality mark
!     vq     - observation quality mark (t,q,wind)
!     levs   - number of levels in profile for observation
!     plevs  - observation pressures
!     errout - observation error 
!     k      - observation level 
!     presl  - model pressure at half sigma levels
!     dpres  - delta pressure between model pressure levels
!     nsig   - number of vertical levels
!     lim_qm - qc limit 
!
!   output argument list:
!     errout - adjusted observation error
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    integer(i_kind)                     ,intent(in   ) :: levs,k,nsig,lim_qm
    real(r_kind)   ,dimension(*)        ,intent(in   ) :: plevs
    real(r_kind)   ,dimension(nsig)     ,intent(in   ) :: presl
    real(r_kind)   ,dimension(nsig-1)   ,intent(in   ) :: dpres
    integer(i_kind),dimension(*)        ,intent(in   ) :: pq,vq
    real(r_kind)                        ,intent(inout) :: errout

    integer(i_kind) n,l,ilev
    real(r_kind):: vmag,pdiffu,pdiffd
    
    errout=one
    if(levs == 1)return
    ilev=1
    do n=2,nsig-1
       if(plevs(k) < presl(n))ilev=n
    end do
    vmag=abs(dpres(ilev))

    pdiffu=vmag
    pdiffd=vmag
    if(pq(k) < lim_qm .and. vq(k) < lim_qm)then
! Move up through the profile.  
       l=k

! Array plevs is only defined from l=1 to l=levs.  Hence the check below
       if (l+1<=levs) then
          upprof: do while (abs(plevs(k)-plevs(l+1)) < vmag .and. l <= levs-1) 
             l=l+1
             if(pq(l) < lim_qm .and. vq(l) < lim_qm)then
                pdiffu=abs(plevs(k)-plevs(l))
                exit upprof
             end if
             if (l==levs) exit upprof
          end do upprof
       endif
        
! Reset the level and move down through the profile
       l=k

! The check (l>=2) ensures that plevs(l-1) is defined
       if (l>=2) then
          dwprof: do while (abs(plevs(l-1)-plevs(k)) < vmag .and. l >= 2) 
             l=l-1
             if(pq(l) < lim_qm .and. vq(l) < lim_qm)then
                pdiffd=abs(plevs(l)-plevs(k))
                exit dwprof
             end if
             if (l==1) exit dwprof
          end do dwprof
       endif

! Set adjusted error
       if(plevs(k) > presl(1))then
          errout=errout*(dpres(1)+plevs(k)-presl(1))/dpres(1)
       else if(plevs(k) < presl(nsig))then
          errout=errout*(dpres(nsig-1)+presl(nsig)-plevs(k))/dpres(nsig-1)
       else
          errout=sqrt(two*vmag/max(pdiffd+pdiffu,five*tiny_r_kind))
       end if

! Quality marks indicate bad data.  Set error to large value.
    else
       errout=1.e6_r_kind
    end if

    return
end subroutine errormod_hdraob

  subroutine errormod_aircraft(pq,vq,levs,plevs,errout,k,presl,dpres,nsig,lim_qm,hdr3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    errormod_aircraft
!   prgmmr: zhu           org: np23                date: 2016-05-22
!
! abstract: adjust observation error for conventional obs
!
! program history log:
!   2016-05-22  zhu - adapted from errormod
!                   - add hdr3 and horizontal distance check for aircraft data
!
!   input argument list:
!     pq     - pressure quality mark
!     vq     - observation quality mark (t,q,wind)
!     levs   - number of levels in profile for observation
!     plevs  - observation pressures
!     errout - observation error 
!     k      - observation level 
!     presl  - model pressure at half sigma levels
!     dpres  - delta pressure between model pressure levels
!     nsig   - number of vertical levels
!     lim_qm - qc limit 
!     hdr3   - observation lat/lon locations
!
!   output argument list:
!     errout - adjusted observation error
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use aircraftinfo, only: hdist_aircraft
    implicit none

    integer(i_kind)                     ,intent(in   ) :: levs,k,nsig,lim_qm
    real(r_kind)   ,dimension(255)      ,intent(in   ) :: plevs
    real(r_kind)   ,dimension(nsig)     ,intent(in   ) :: presl
    real(r_kind)   ,dimension(nsig-1)   ,intent(in   ) :: dpres
    integer(i_kind),dimension(255)      ,intent(in   ) :: pq,vq
    real(r_kind)                        ,intent(inout) :: errout
    real(r_double),dimension(3,255),optional,intent(in) :: hdr3

    integer(i_kind) n,l,ilev
    real(r_kind):: vmag,pdiffu,pdiffd,con
    real(r_kind):: rlatk,rlonk,rlatl,rlonl,dist1,dist2,dist3,dist
    logical latlon_check

    errout=one
    if(levs == 1)return

    latlon_check=.false.
    if (present(hdr3)) latlon_check=.true.
    if (latlon_check) then
       rlatk=hdr3(2,k)*deg2rad
       rlonk=hdr3(1,k)*deg2rad
    end if

    ilev=1
    do n=2,nsig-1
       if(plevs(k) < presl(n))ilev=n
    end do
    con=grav*500._r_kind/(273._r_kind*rd)
    vmag=min(max(half*dpres(ilev),r0_02*presl(ilev)),con*plevs(k))

!   vmag=max(half*dpres(ilev),r0_02*presl(ilev))
    pdiffu=vmag
    pdiffd=vmag
    if(pq(k) < lim_qm .and. vq(k) < lim_qm)then
! Move up through the profile.  
       l=k

! Array plevs is only defined from l=1 to l=levs.  Hence the check below
       if (l+1<=levs) then
          upprof: do while (abs(plevs(k)-plevs(l+1)) < vmag .and. l <= levs-1)
             l=l+1
             if(pq(l) < lim_qm .and. vq(l) < lim_qm)then
                pdiffu=abs(plevs(k)-plevs(l))
                if (latlon_check) then  ! chord length
                   rlatl=hdr3(2,l)*deg2rad
                   rlonl=hdr3(1,l)*deg2rad
                   dist1=cos(rlatk)*cos(rlonk)-cos(rlatl)*cos(rlonl)
                   dist2=cos(rlatk)*sin(rlonk)-cos(rlatl)*sin(rlonl)
                   dist3=sin(rlatk)-sin(rlatl)
                   dist=min(one,sqrt(dist1*dist1+dist2*dist2+dist3*dist3))
                   dist=rearth_equator*two*asin(dist/two)
                   if (dist>hdist_aircraft) pdiffu=vmag
                end if
                exit upprof
             end if
             if (l==levs) exit upprof
          end do upprof
       endif

! Reset the level and move down through the profile
       l=k

! The check (l>=2) ensures that plevs(l-1) is defined
       if (l>=2) then
          dwprof: do while (abs(plevs(l-1)-plevs(k)) < vmag .and. l >= 2)
             l=l-1
             if(pq(l) < lim_qm .and. vq(l) < lim_qm)then
                pdiffd=abs(plevs(l)-plevs(k))
                if (latlon_check) then
                   rlatl=hdr3(2,l)*deg2rad
                   rlonl=hdr3(1,l)*deg2rad
                   dist1=cos(rlatk)*cos(rlonk)-cos(rlatl)*cos(rlonl)
                   dist2=cos(rlatk)*sin(rlonk)-cos(rlatl)*sin(rlonl)
                   dist3=sin(rlatk)-sin(rlatl)
                   dist=min(one,sqrt(dist1*dist1+dist2*dist2+dist3*dist3))
                   dist=rearth_equator*two*asin(dist/two)
                   if (dist>60000.0_r_kind) pdiffd=vmag
                end if
                exit dwprof
             end if
             if (l==1) exit dwprof
          end do dwprof
       endif

! Set adjusted error
       errout=sqrt(two*vmag/max(pdiffd+pdiffu,five*tiny_r_kind))

! Quality marks indicate bad data.  Set error to large value.
    else
       errout=1.e6_r_kind
    end if

    return
end subroutine errormod_aircraft


subroutine tz_retrieval(nchanl,nsig,ich,irday,temp,wmix,tnoise,varinv,ts,tbc,tzbgr,iud,iall,dtz,ts_ave) 

!subprogram:    tz_retrieval  compute tz retrieval with radiances
!   prgmmr: Xu Li          org: w/nmc2     date: 06-01-2010
!
! abstract:  perform tz retrieval based on input radiative transfer info
!            save bufr output when necessary
!
! program history log:
!
!   input argument list:
!     nsig         - number of model layers
!     nchanl       - number of channels for instruments
!     ich          - channel number
!     irday        - index for the use of shorter IR channel: 0 = no; 1 = yes
!     temp         - d(brightness temperature)/d(temperature)
!     wmix         - d(brightness temperature)/d(mixing ratio)
!     tnoise       - error of observed radiance
!     varinv       - inverse error squared
!     ts           - d(brightness temperature)/d(tz)
!     tbc          - bias corrected (observed - simulated brightness temperatures)
!     tzbgr        - tz used in Radiative transfer and first guess for Tz retrieval
!     iud          - data usage indicator
!     iall         - Tz retrieval done for all pixels or not: 0 = no; 1 = yes
!
!   output argument list:
!     dtz          - retrieved Tz increment
!     ts_ave       - avergae of ts for used channels in Tz retrieval

! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
    use constants, only: ttp

    implicit none

!   Declare passed variables
    integer(i_kind), intent(in) :: nsig
    integer(i_kind), intent(in) :: nchanl
    integer(i_kind), dimension(nchanl), intent(in) :: ich,irday
    real(r_kind),dimension(nsig,nchanl), intent(in) :: wmix,temp
    real(r_kind),dimension(nchanl), intent(in) :: tnoise,varinv,ts,tbc
    real(r_kind),intent(in) :: tzbgr
    integer(i_kind), intent(in) :: iud,iall
    real(r_kind), intent(out) :: dtz,ts_ave

!   Declare local variables
    real(r_kind) :: ws,wa,wq
    integer(i_kind) :: icount,i,k
    real(r_kind), dimension(nchanl) :: tb_ta,tb_qa
    real(r_kind), dimension(nchanl) :: w_rad
    real(r_kind) :: delt,delt1,c1x,c2x,c3x
    real(r_kind) :: a11,a12,a13,a23,a22,a33
    real(r_kind) :: varrad
    real(r_kind) :: vchk

    vchk = tiny_r_kind - real(iall)
!**********************************
! Get tb_ta & tb_qa for selected channels
!**********************************
    do i = 1, nchanl
      if ( iuse_rad(ich(i)) >= iud .and. varinv(i) > vchk .and. irday(i) == 1 .and. ts(i) >= tschk ) then
        tb_ta(i) = temp(1,i)
        tb_qa(i) = wmix(1,i)
        do k = 2, nsig
           tb_ta(i) = tb_ta(i) + temp(k,i)
           tb_qa(i) = tb_qa(i) + wmix(k,i)
        enddo
      endif
    enddo

    ws = one/e_ts**2
    wa = one/e_ta**2
    wq = one/(e_qa*(max((tzbgr-ttp)*0.03_r_kind,zero)+one_tenth))**2

    ts_ave = zero

    a11 = ws                                      ! 1./tserr**2
    a22 = wa                                      ! 1./taerr**2
    a33 = wq                                      ! 1./qaerr**2

    a12 = zero; a13 = zero; a23 = zero
    c1x = zero; c2x = zero; c3x = zero
    delt1 = zero; delt = one; icount = 0

!   delt2 = zero; delt3 = zero

    do i=1,nchanl
!     Get coefficients for linear equations
      if ( iuse_rad(ich(i)) >= iud .and. varinv(i) > vchk .and. irday(i) == 1 .and. ts(i) >= tschk  ) then
        icount = icount+1
        ts_ave = ts_ave + ts(i)
        w_rad(i) = (one/tnoise(i))**2
        a11 = a11 + w_rad(i)*ts(i)**2
        a12 = a12 + w_rad(i)*ts(i)*tb_ta(i)
        a13 = a13 + w_rad(i)*ts(i)*tb_qa(i)
        a22 = a22 + w_rad(i)*tb_ta(i)**2
        a23 = a23 + w_rad(i)*tb_ta(i)*tb_qa(i)
        a33 = a33 + w_rad(i)*tb_qa(i)**2

        varrad=w_rad(i)*tbc(i)
        c1x = c1x + varrad*ts(i)
        c2x = c2x + varrad*tb_ta(i)
        c3x = c3x + varrad*tb_qa(i)
      end if             !  if ( iuse_rad(ich(i)) >= iud ...
    end do               ! do i=1,nchanl

!    Solve linear equations with three unknowns (dtz, dta, dqa)
!    Only dtz is solved since other two are not useful here
     if ( icount >= 1 ) then

        delt  =  a11*(a22*a33-a23*a23) +  &
                 a12*(a13*a23-a12*a33) +  &
                 a13*(a12*a23-a13*a22)

        delt1 =  c1x*(a22*a33-a23*a23) + &
                 c2x*(a13*a23-a12*a33) + &
                 c3x*(a12*a23-a13*a22)

!       delt2 =  c1x*(a13*a23-a12*a33) + &
!                c2x*(a11*a33-a13*a13) + &
!                c3x*(a12*a13-a11*a23)

!       delt3 =  c1x*(a12*a23-a13*a22) + &
!                c2x*(a13*a12-a11*a23) + &
!                c3x*(a11*a22-a12*a12)

        dtz = delt1/delt

!       dta = delt2/delt
!       dqa = delt3/delt

        ts_ave = ts_ave/real(icount)
      end if                 ! if ( icount >= 1 )

end subroutine tz_retrieval

subroutine qc_ssmi(nchanl,nsig,ich,sfchgt,luse,sea,mixed, &
     temp,wmix,ts,pems,ierrret,kraintype,tpwc,clw,sgagl,tzbgr,   &
     tbc,tbcnob,tb_ges,tnoise,ssmi,amsre_low,amsre_mid,amsre_hig,ssmis, &
     varinv,errf,aivals,id_qc)
!    varinv,errf,aivals,id_qc,radmod) ! all-sky

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_ssmi      QC for ssmi/amsre
!
!   prgmmr: okamoto          org: np23            date: 2004-12-01
!
! abstract: set quality control criteria for SSM/I,AMSR-E,SSMIS(UKMO)
!
! program history log:
!     2004-12-01  okamoto 
!     2005-02-17  derber  clean up surface flags
!     2005-03-04  treadon  - correct underflow error for varinv
!     2005-09-05  derber - allow max error to change by channel
!     2005-10-07  Xu & Pawlak - add SSMIS qc code, add documentation
!     2005-10-20  kazumori - add AMSR-E qc code, add documentation
!     2006-02-03  derber  - modify for new obs control and stats         
!     2006-04-26  kazumori  - change clw theshold for AMSR-E
!     2006-04-27  derber - modify to do single profile - fix bug
!     2006-07-27  kazumori - modify AMSR-E qc and input of the subroutine
!     2006-12-01  derber - modify id_qc flags
!     2007-01-24  kazumori - modify SSMIS qc and input of the subroutine
!     2008-04-23  safford  - rm unused vars              
!     2010-07-16  yan      - update the qc criteria for ssmis
!      1) remove 'ssmis_uas,ssmis_las,ssmis_env,ssmis_img' 
!      2) add an input 'tbc' which is used to detect cloud-affected data over land
!      3) update the thresholds of cloud detection for some of the cloud-affected channels
!      4) add a new qc for ssmis data over oceans
!      5) update the qc criteria of the ssmis data over non-ocean surfaces
!      6) realx the qc criteria for the data at channels from 1 to 2
!      7) add two references
!     2013-02-13  eliu     - tighten up the qc criteria for ssmis
!
! input argument list:
!     nchanl  - number of channels per obs
!     ich     - channel number
!     sfchgt  - surface height (not use now)
!     luse    - logical use flag
!     sea     - logical, sea flag
!     mixed   - logical, mixed zone flag
!     wmix         - d(brightness temperature)/d(mixing ratio)
!     ts      - d(Tb)/d(Tz)
!     pems    - surface emissivity
!     ierrret - result flag of retrieval_mi
!     kraintype - [0]no rain, [others]rain ; see retrieval_mi
!     clw     - retrieve clw [kg/m2]
!     sgagl   - sun glint angle [degrees]
!     tzbgr   - water temperature (Tz) of FOV
!     tpwc    - retrieve tpw [kg/m2]
!     tbc     - Obs - Back TBB with bias correction
!     tbcnob  - Obs - Back TBB without bias correction
!     tb_ges  - simulated TBB
!     tnoise  - error of observed radiance
!     ssmi    - logical true if ssmi is processed 
!     ssmis    - logical true if ssmis is processed 
!     amsre_low   - logical true if amsre_low is processed 
!     amsre_mid   - logical true if amsre_mid is processed 
!     amsre_hig   - logical true if amsre_hig is processed 
!
! NOTE! if retrieved clw/tpwc not available over ocean,set -9.99e+11, 
!       but 0 over land/mixed/ice
!
! output argument list:
!     varinv  - observation weight (modified obs var error inverse)
!     errf    - criteria of gross error
!     aivals  - number of data not passing QC
!     id_qc   - qc index - see qcmod definitions
!
!
!
!     ... possibe QC to add ..........................
!     * decrease varinv at last several scan position
!  
!     clwcutofx is used to set cloud qc threshold  (kg/m2) 
!     Refernces:
!     (1) Weng, F. and N. C. Grody, 1994: Retrieval of cloud liquid water using the special sensor microwave
!       imager (SSM/I), J. Geophys. Res., 99, 25,535 -25, 551.
!     (2) Yan, B., F. Weng and J. Derber, 2010: An Effort toward Assimilation of F16 Special Sensor Microwave
!       Imager/Sounder Data into the NCEP Global Forecast System, to be submitted to Journal of Weather and
!       Forecasting
!
!     from Fuzhong Weng (need better reference) 
!     ................................................
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none


! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: nsig
  integer(i_kind)                  ,intent(in   ) :: nchanl
  integer(i_kind),dimension(nchanl),intent(in   ) :: ich
  integer(i_kind)                  ,intent(in   ) :: kraintype,ierrret
  integer(i_kind),dimension(nchanl),intent(inout) :: id_qc

  logical                          ,intent(in   ) :: sea,mixed,luse
  logical                          ,intent(in   ) :: ssmi,amsre_low,amsre_mid,amsre_hig,ssmis

  real(r_kind)                     ,intent(in   ) :: sfchgt,tpwc,clw,sgagl,tzbgr
  real(r_kind)   ,dimension(nchanl),intent(in   ) :: ts,pems,tnoise
  real(r_kind)   ,dimension(nchanl),intent(in   ) :: tbc,tbcnob,tb_ges
  real(r_kind),dimension(nsig,nchanl),intent(in ) :: temp,wmix

  real(r_kind)   ,dimension(nchanl),intent(inout) :: varinv,errf
  real(r_kind)   ,dimension(40)    ,intent(inout) :: aivals

! Declare local variables
  integer(i_kind), dimension(nchanl) :: irday
  integer(i_kind) :: l,i
  real(r_kind) :: efact,vfact,dtempf,dtbf,term
  real(r_kind),dimension(nchanl) :: demisf_mi,clwcutofx 
  real(r_kind) :: pred9,pred10,pred11
  real(r_kind) :: dtz,ts_ave,xindx,tzchks

!------------------------------------------------------------------
  irday = 1

! Set cloud qc criteria  (kg/m2) :  reject when clw>clwcutofx
  if(ssmi) then
     clwcutofx(1:nchanl) =  &  
          (/0.35_r_kind, 0.35_r_kind, 0.27_r_kind, 0.10_r_kind, &
          0.10_r_kind, 0.024_r_kind, 0.024_r_kind/) 
  else if(amsre_low.or.amsre_mid.or.amsre_hig) then
     clwcutofx(1:nchanl) =  &  
          (/0.350_r_kind, 0.350_r_kind, 0.350_r_kind, 0.350_r_kind, &
          0.300_r_kind, 0.300_r_kind, 0.250_r_kind, 0.250_r_kind, &
          0.100_r_kind, 0.100_r_kind, 0.020_r_kind, 0.020_r_kind/) 
!    --- amsre separate channel treatment depend on FOV
     if(amsre_low) varinv(5:12)=zero
     if(amsre_mid) varinv(1:4)=zero
     if(amsre_mid) varinv(11:12)=zero
     if(amsre_hig) varinv(1:10)=zero
  else if(ssmis) then
     clwcutofx(1:nchanl) =  &  !kg/m2  reject when clw>clwcutofx
        (/ 0.10_r_kind, 0.20_r_kind, &
           0.60_r_kind, 2.00_r_kind, &
           2.00_r_kind, 2.00_r_kind, &
           2.00_r_kind, 0.10_r_kind, &
           0.10_r_kind, 0.10_r_kind, &
           0.10_r_kind, 0.20_r_kind, &
           0.20_r_kind, 0.20_r_kind, &
           0.20_r_kind, 0.20_r_kind, &
           0.10_r_kind, 0.10_r_kind, &
           10.0_r_kind,10.0_r_kind, &
           10.0_r_kind,10.0_r_kind, &
           10.0_r_kind,10.0_r_kind  /)
  end if
  dtempf = half
  demisf_mi(1:nchanl) = 0.01_r_kind

! Loop over observations.

  efact     =one
  vfact     =one

!    Over sea               
  if(sea) then 

!    dtb/rain/clw qc using SSM/I RAYTHEON algorithm
     if( ierrret > 0  .or. kraintype /= 0 .or. tpwc<zero ) then 
        efact=zero; vfact=zero
        if(luse) then
           aivals(8) = aivals(8) + one
           
           do i=1,nchanl
              if( id_qc(i)== igood_qc )then
                 if( kraintype/= 0) id_qc(i)=ifail_krain_qc
                 if( ierrret > 0)   id_qc(i)=ifail_ierrret_qc
                 if( tpwc< zero )   id_qc(i)=ifail_tpwc_qc
              end if
           end do 
        end if
     else if (ssmis) then  ! in case of ssmis bad data or cloud-contaminated data
        do i = 1,24
           if( abs(tbcnob(i)) >= 3.5_r_kind) then
              varinv(i) = zero
              id_qc(i) = ifail_gross_routine_qc
           end if
        enddo

     else if(amsre_low .and. sgagl < 25.0_r_kind) then

! ---- sun glint angle qc (for AMSR-E)

        varinv(1:4)=zero
        do i=1,4
           if(id_qc(i) == igood_qc)id_qc(i) = ifail_sgagl_qc
        end do
        if(luse) aivals(11) = aivals(11) + one

     else if(amsre_low .or. amsre_mid .or. amsre_hig)then

! ---- dtb threshold qc for AMSR-E due to an inaccuracy of emis model

        if( abs(tbcnob(1)) > 6.0_r_kind .or. &
            abs(tbcnob(2)) > 6.0_r_kind .or. &
            abs(tbcnob(3)) > 6.0_r_kind .or. &
            abs(tbcnob(4)) > 6.0_r_kind .or. &
            abs(tbcnob(5)) > 6.0_r_kind .or. &
            abs(tbcnob(6)) > 8.0_r_kind .or. &
            abs(tbcnob(7)) > 8.0_r_kind .or. &
            abs(tbcnob(8)) > 10.0_r_kind .or. &
            abs(tbcnob(9)) > 6.0_r_kind .or. &
            abs(tbcnob(10)) > 6.0_r_kind) then
           do i=1,nchanl
              varinv(i)=zero
              id_qc(i)=ifail_emiss_qc
           end do
           if(luse) aivals(13) = aivals(13) + one
        end if

     else if(clw > zero)then

!      If dtb is larger than demissivity and dwmin contribution, 
!      it is assmued to be affected by  rain and cloud, tossing it out
        do l=1,nchanl

!          clw QC using ch-dependent threshold (clwch)
           if( clw > clwcutofx(l) ) then
              varinv(l)=zero
              if(luse) then
                 aivals(10) = aivals(10) + one
                 if(id_qc(l)== igood_qc) then
                    id_qc(l)=ifail_cloud_qc
                    aivals(9)=aivals(9) + one
                 end if
              end if
           end if
        end do  !l_loop
     end if

!    Use data not over over sea
  else  !land,sea ice,mixed

!   Reduce q.c. bounds over higher topography
     if ( .not. ssmis) then
!    demisf_mi=demisf_mi*0.7_r_kind   ! not necessary since data not used
        efact=zero
        vfact=zero
        do i=1,nchanl
           if(id_qc(i)== igood_qc ) id_qc(i)=ifail_surface_qc
        end do

!       if (sfchgt > r2000) then
!          fact = r2000/sfchgt
!          efact = fact*efact
!          vfact = fact*vfact
!       end if

     else  ! for ssmis 
        if (sfchgt > r2000) then
           do i=1,24
              varinv(i)=zero
              if(id_qc(i)== igood_qc) id_qc(i)=ifail_topo_ssmi_qc
           enddo
        else
       !Use dtbc at 52.8 GHz to detect cloud-affected data
           if(mixed) then
              do i=1,3
                 varinv(i)=zero
                 if(id_qc(i)== igood_qc) id_qc(i)=ifail_surface_qc
              end do
              do i=8,18
                 varinv(i)=zero
                 if(id_qc(i)== igood_qc) id_qc(i)=ifail_surface_qc
              end do
           else if (abs(tbc(2)) >= 1.5_r_kind) then  ! the data at cloud-affected channels are not used
              do i =1,2
                 varinv(i)  = zero
                 if(id_qc(i)== igood_qc ) id_qc(i)=ifail_ch2_qc
              end do
              do i =12,16
                 varinv(i)  = zero
                 if(id_qc(i)== igood_qc ) id_qc(i)=ifail_ch2_qc
              end do
           endif
       !General qc criteria for all channels
           do i = 1,24
              if( abs(tbcnob(i)) >= 3.5_r_kind) then
                 varinv(i) = zero
                 if(id_qc(i)== igood_qc ) id_qc(i)=ifail_gross_routine_qc
              end if
           enddo

        end if
!        if (sfchgt > r2000) then
!           varinv(9)=zero
!           if(id_qc(9)== igood_qc) id_qc(9)=ifail_topo_ssmi_qc
!        end if
!        if (sfchgt > r4000) then
!           varinv(3)=zero
!           if(id_qc(3)== igood_qc) id_qc(3)=ifail_topo_ssmi_qc
!           varinv(10)=zero
!           if(id_qc(10)== igood_qc) id_qc(10)=ifail_topo_ssmi_qc
!        end if

     end if
  end if

  if(ssmis)then
  ! scattering affected data removal
     pred9  =271.252327_r_kind - 0.485934_r_kind*tb_ges(17) + 0.473806_r_kind*tb_ges(8)
     pred10 =272.280341_r_kind - 0.413688_r_kind*tb_ges(17) + 0.361549_r_kind*tb_ges(8)
     pred11 =278.824902_r_kind - 0.400882_r_kind*tb_ges(17) + 0.270510_r_kind*tb_ges(8)
     if(pred9  - tbcnob(9)  - tb_ges(9)  > two) then
        varinv(9) =zero
        if(id_qc(9)== igood_qc) id_qc(9)=ifail_scatt_qc
     end if
     if(pred10 - tbcnob(10) - tb_ges(10) > two) then
        varinv(10)=zero
        if(id_qc(10)== igood_qc) id_qc(10)=ifail_scatt_qc
     end if
     if(pred11 - tbcnob(11) - tb_ges(11) > two) then
        varinv(11)=zero
        if(id_qc(11)== igood_qc) id_qc(11)=ifail_scatt_qc
     end if
  end if
!
!    Apply Tz retrieval
!
     if(tzr_qc > 0)then
        dtz = rmiss_single
        if ( sea ) then
           call tz_retrieval(nchanl,nsig,ich,irday,temp,wmix,tnoise,varinv,ts,tbc,tzbgr,1,0,dtz,ts_ave) 
        endif
!
!       Apply QC with Tz retrieval
!
        if (dtz /= rmiss_single ) then
          do i = 1, nchanl
            if ( varinv(i) > tiny_r_kind .and. iuse_rad(ich(i)) >= 1 .and. ts(i) > tschk ) then
              xindx = ((ts(i)-ts_ave)/(one-ts_ave))**3
              tzchks = tzchk*(half)**xindx

              if ( abs(dtz) > tzchks ) then
                 varinv(i) = zero
                 if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_tzr_qc
                 if(luse)aivals(13) = aivals(13) + one
              endif
            endif
          enddo
        endif
     endif

! Generate q.c. bounds and modified variances.
  do l=1,nchanl

     errf(l)   = efact*errf(l)
     varinv(l) = vfact*varinv(l)
     
     if (varinv(l) > tiny_r_kind) then
        dtbf = demisf_mi(l)*abs(pems(l)) + dtempf*abs(ts(l))
        term = dtbf*dtbf
        if(term>tiny_r_kind) varinv(l)=one/(one/varinv(l)+term)
     else if(luse  .and. id_qc(l)== igood_qc )then
        id_qc(l)=ifail_varinv_qc
     endif
        

  end do ! l (ch) loop end
      

  return
end subroutine qc_ssmi

subroutine qc_gmi(nchanl,sfchgt,luse,sea,cenlat, cenlon, &
     kraintype,clw,tsavg5,tbobs,gmi,varinv,aivals,id_qc,lcw4crtm_gmi)
!    kraintype,clw,tsavg5,tbobs,gmi,varinv,aivals,id_qc,radmod) ! all-sky
!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_gmi     QC for gmi TBs
!
!   prgmmr: ejones         org: jcsda            date: 2015-01-16
!
! abstract: set quality control criteria for GMI; check clw against
!           thresholds, calculate and check emissivity, filter out
!           bad obs.
!
! program history log:
!     2015-01-16  ejones  - copied and modified qc_ssmi
!     2015-02-13  ejones  - added swath edge check
!     2015-02-17  ejones  - added emissivity regression and check
!     2016-05-05  ejones  - added check for latitudes above/below 55N/S
!
! input argument list:
!     nchanl       - number of channels per obs
!     sfchgt  - surface height (not use now)
!     luse    - logical use flag
!     sea     - logical, sea flag
!     cenlat   - latitude of observation
!     kraintype - [0]no rain, [others]rain ; see retrieval_mi
!     clw     - retrieve clw [kg/m2]
!     tsavg5       - surface skin temperature
!     tbobs   - brightness temperature observations
!     gmi     - logical true if gmi is processed
!
! output argument list:
!     varinv  - observation weight (modified obs var error inverse)
!     aivals  - number of data not passing QC
!     id_qc   - qc index - see qcmod definitions
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: nchanl
  integer(i_kind)                  ,intent(in   ) :: kraintype
  integer(i_kind),dimension(nchanl),intent(inout) :: id_qc

  logical                          ,intent(in   ) :: sea,luse
  logical                          ,intent(in   ) :: gmi

  real(r_kind)                     ,intent(in   ) :: sfchgt,clw,tsavg5
  real(r_kind)                     ,intent(in   ) :: cenlat,cenlon
  real(r_kind)   ,dimension(nchanl),intent(in   ) :: tbobs

  real(r_kind)   ,dimension(nchanl),intent(inout) :: varinv
  real(r_kind)   ,dimension(40)    ,intent(inout) :: aivals
  logical                          ,intent(in)    :: lcw4crtm_gmi  !MJK

! Declare local variables
  integer(i_kind) :: l,i,idx
  integer(i_kind) :: nch_emrgr                      ! nchan in emissivity regression
  integer(i_kind),dimension(13)  :: idxch_emrgr      ! chan used in emissivity regression
  real(r_kind),dimension(13)     :: rgr_coeff_10h,rgr_coeff_18h,rgr_coeff_36h ! regression coefficients
  real(r_kind),dimension(2)      :: rgr_coeff2_10h,rgr_coeff2_18h,rgr_coeff2_36h ! regression coefficients
  real(r_kind)                  :: em10h,em18h,em36h,em2_10h,em2_18h,em2_36h ! calculated emissivity
  real(r_kind)                  :: diff_em_10h,diff_em_18h,diff_em_36h   !  emissivity differences
! coefficients for regression
  real(r_kind) :: c10h,c18h,c36h,d10h,d18h,d36h                    ! regression constants
  real(r_kind) :: efact,vfact,fact
  real(r_kind),dimension(nchanl) :: clwcutofx
!------------------------------------------------------------------

! Set cloud qc criteria  (kg/m2) :  reject when clw>clwcutofx
  if(gmi) then
     clwcutofx(1:nchanl) =  &
          (/0.05_r_kind, 0.05_r_kind, 0.05_r_kind, 0.05_r_kind, 0.05_r_kind, &
            0.05_r_kind, 0.05_r_kind, 0.05_r_kind, 0.05_r_kind, 0.05_r_kind, &
            0.05_r_kind, 0.05_r_kind, 0.05_r_kind/)
  end if

! Loop over observations.

  efact     =one
  vfact     =one

!    Over sea
  if(sea) then

!    rain qc
     if(.not. lcw4crtm_gmi .and. kraintype /= 0 ) then   !MJK
        efact=zero; vfact=zero
        if(luse) then
           aivals(8) = aivals(8) + one

           do i=1,nchanl
              varinv(i)=zero
              if( id_qc(i)== igood_qc .and. kraintype/= 0) then
                id_qc(i)=ifail_krain_gmi_qc
              endif
           end do
        end if

     else if(.not. lcw4crtm_gmi .and. clw > zero)then
!      If dtb is larger than demissivity and dwmin contribution,
!      it is assmued to be affected by  rain and cloud, tossing it out
        do l=1,nchanl

!          clw QC using ch-dependent threshold (clwch)
           if( clw > clwcutofx(l) ) then
              varinv(l)=zero
              if(luse) then
                 aivals(10) = aivals(10) + one
                 if(id_qc(l)== igood_qc) then
                    id_qc(l)=ifail_cloud_qc
                    aivals(9)=aivals(9) + one
                 end if
              end if
           end if
        end do  !l_loop
     end if


!   Calculate emissivity and flag observations over thresholds
!   Calculations for ch 3,4,5
    nch_emrgr = 13
    idxch_emrgr = (/1,2,3,4,5,6,7,8,9,10,11,12,13/)

    ! Set regression constants and coefficients
    ! first set of constants and coefficients (using all channels)
    c10h = 0.13290_r_kind
    c18h = 0.15627_r_kind
    c36h = 0.30306_r_kind

    rgr_coeff_10h = (/ -0.00548_r_kind, 0.00772_r_kind, 0.00530_r_kind, -0.00425_r_kind, &
                        0.00053_r_kind, 0.00008_r_kind, -0.00003_r_kind, -0.00144_r_kind, &
                        0.00059_r_kind, -0.00016_r_kind, 0.00003_r_kind, -0.00011_r_kind, &
                        0.00017_r_kind /)
    rgr_coeff_18h = (/ -0.01084_r_kind, 0.01194_r_kind, 0.01111_r_kind, -0.00784_r_kind, &
                        0.00060_r_kind, 0.00008_r_kind, -0.00003_r_kind, -0.00248_r_kind, &
                        0.00105_r_kind, -0.00008_r_kind, 0.00000_r_kind, -0.00013_r_kind, &
                        0.00016_r_kind /)
    rgr_coeff_36h = (/ -0.01793_r_kind, 0.01730_r_kind, 0.01784_r_kind, -0.01199_r_kind, &
                        0.00067_r_kind, 0.00013_r_kind, -0.00004_r_kind, -0.00365_r_kind, &
                        0.00154_r_kind, -0.00004_r_kind, -0.00001_r_kind, -0.00015_r_kind, &
                        0.00017_r_kind /)

    ! second set of constants and coefficients (single channel regression)
    d10h = 0.42468_r_kind
    d18h = 0.83807_r_kind
    d36h = 1.24071_r_kind

    rgr_coeff2_10h = (/ 0.00289_r_kind, -0.00142_r_kind /)
    rgr_coeff2_18h = (/ 0.00048_r_kind, -0.00207_r_kind /)
    rgr_coeff2_36h = (/ 0.00068_r_kind, -0.00342_r_kind /)

    ! perform regressions
    ! first set
    em10h = c10h
    em18h = c18h
    em36h = c36h
    do i=1,nch_emrgr
      idx=idxch_emrgr(i)
      em10h=em10h+(tbobs(idx)*rgr_coeff_10h(i))    ! 10h multi-channel emiss
      em18h=em18h+(tbobs(idx)*rgr_coeff_18h(i))    ! 18h multi-channel emiss
      em36h=em36h+(tbobs(idx)*rgr_coeff_36h(i))    ! 36h multi-channel emiss
    end do

    ! second set, using tskin
    ! 10h single-channel emiss
    em2_10h = d10h + ( tbobs(2)*rgr_coeff2_10h(1) ) + ( rgr_coeff2_10h(2) * tsavg5 )
    ! 18h single-channel emiss
    em2_18h = d18h + ( tbobs(4)*rgr_coeff2_18h(1) ) + ( rgr_coeff2_18h(2) * tsavg5 )
    ! 36h single-channel emiss
    em2_36h = d36h + ( tbobs(7)*rgr_coeff2_36h(1) ) + ( rgr_coeff2_36h(2) * tsavg5 )

    ! calculate differences between emissivity regressions
    diff_em_10h = em10h - em2_10h
    diff_em_18h = em18h - em2_18h
    diff_em_36h = em36h - em2_36h

    ! check emissivity difference values against thresholds and assign flag if needed
    if( .not. lcw4crtm_gmi)then
      if ( (diff_em_10h > 0.01_r_kind) .or. (diff_em_18h > 0.035_r_kind) .or. (diff_em_36h > 0.05_r_kind) ) then
         do i=1,13
          varinv(1:13)=zero
          if (id_qc(i) == igood_qc) id_qc(i)=ifail_emiss_qc
         end do
      end if
    else
       do i=1,nchanl
         if(clw < clwcutofx(i) .and. ((diff_em_10h > 0.01_r_kind) .or. (diff_em_18h > 0.035_r_kind) .or. (diff_em_36h > 0.05_r_kind)))  then
            varinv(i)=zero
            if (id_qc(i) == igood_qc) id_qc(i)=ifail_emiss_qc
         endif
       enddo
    endif

    ! check latitude. If obs is south of 55S or north of 55N, don't use it; it
    ! may be affected by sea ice.
    if (abs(cenlat)>55.0_r_kind) then
       do i=1,13
          varinv(i)=zero
          if (id_qc(i) == igood_qc) id_qc(i)=ifail_lat_qc
       end do
    end if

!    Use data not over over sea
  else  !land,sea ice,mixed

!   Reduce q.c. bounds over higher topography
     efact=zero
     vfact=zero

     if (sfchgt > r2000) then
        fact = r2000/sfchgt
        efact = fact*efact
        vfact = fact*vfact
     end if
     varinv(1:13)=zero
  end if

! Check for the observations at the scan edge (where only ch 1-9 are recorded)
! and flag them for QC. These obs will have a clw of 999.0, assigned in
! retrieval_gmi. This is only the case if use_swath_edge is set to true in
! read_gmi.

  if(clw > zero)then
     do l=1,nchanl
        if(clw > 900.0_r_kind) then
          id_qc(l)=ifail_gmi_swathedge_qc
          varinv(l)=zero
        endif
     end do
  end if

  if(tsavg5 < 275.0_r_kind)then
     do l=1,nchanl
          id_qc(l)=ifail_surface_qc
          varinv(l)=zero
     enddo
  endif

 if(cenlat .gt. -20.0_r_kind .and. cenlat .lt. zero .and. cenlon .gt. 25.0_r_kind .and. cenlon .lt. 40.0_r_kind) then
      do l=1,nchanl
       id_qc(l)=ifail_surface_qc
       varinv(l)=zero
     enddo
  endif

  return
end subroutine qc_gmi

subroutine qc_amsr2(nchanl,sfchgt,luse,sea, &
     kraintype,clw,tsavg5,tbobs,solazi,solzen,amsr2,varinv,aivals,id_qc)
!    kraintype,clw,tsavg5,tbobs,solazi,solzen,amsr2,varinv,aivals,id_qc,radmod) ! all-sky
!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_amsr2     QC for amsr2 TBs
!
!   prgmmr: ejones         org: jcsda            date: 2015-03-11
!
! abstract: set quality control criteria for AMSR2; check clw against
!           thresholds, calculate and check emissivity, filter out
!           bad obs.
!
! program history log:
!     2015-01-16  ejones
!     2015-10-02  ejones   - add emissivity retrievals and checks, update clw
!                            check, add sun glint check
!     2015-10-07  ejones   - add bias correction to TBs prior to emissivity
!                            retrievals
!     2015-11-09  ejones   - add an extra check to Ch1 TBs for observations
!                            missed by the CLW check that are probably cloudy
!
! input argument list:
!     nchanl       - number of channels per obs
!     sfchgt  - surface height (not use now)
!     luse    - logical use flag
!     sea     - logical, sea flag
!     kraintype - [0]no rain, [others]rain ; see retrieval_mi
!     clw     - retrieve clw [kg/m2]
!     amsr2   - logical true if gmi is processed
!     solazi  - solar azimuth angle
!     solzen  - solar zenith angle
!     tbobs   - brightness temperatures
!     tsavg5  - skin temp
!
! output argument list:
!     varinv  - observation weight (modified obs var error inverse)
!     aivals  - number of data not passing QC
!     id_qc   - qc index - see qcmod definitions
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: nchanl
  integer(i_kind)                  ,intent(in   ) :: kraintype
  integer(i_kind),dimension(nchanl),intent(inout) :: id_qc

  logical                          ,intent(in   ) :: sea,luse
  logical                          ,intent(in   ) :: amsr2

  real(r_kind)                     ,intent(in   ) :: sfchgt,clw
  real(r_kind)   ,dimension(nchanl),intent(inout) :: varinv
  real(r_kind)   ,dimension(40)    ,intent(inout) :: aivals
  real(r_kind)                     ,intent(in   ) :: solazi,solzen
  real(r_kind)   ,dimension(nchanl),intent(in   ) :: tbobs
  real(r_kind)                     ,intent(in   ) :: tsavg5

! Declare local variables
  integer(i_kind) :: l,i
  real(r_kind) :: efact,vfact,fact

  real(r_kind) :: solel, solazi_rad, solel_rad
  real(r_kind) :: ang, ang_a, ang_b, ang_ab
  real(r_kind),dimension(nchanl) :: clwcutofx

  integer(i_kind)                :: idx, nch_emrgr
  integer(i_kind),dimension(nchanl) :: idxch_emrgr
  real(r_kind),dimension(nchanl) :: rgr_coeff_36h, rgr_coeff_89h, sys_bias, tb_use
  real(r_kind),dimension(2)      :: rgr_coeff2_36h, rgr_coeff2_89h
  real(r_kind) :: c36h, c89h, d36h, d89h
  real(r_kind) :: em36h, em89h, em2_36h, em2_89h, diff_em_36h, diff_em_89h

!------------------------------------------------------------------
! Set cloud qc criteria  (kg/m2) :  reject when clw>clwcutofx
  if (amsr2) then
     clwcutofx(1:nchanl) =  &
         (/ 0.050_r_kind, 0.050_r_kind, 0.050_r_kind, 0.050_r_kind, 0.050_r_kind, &
            0.050_r_kind, 0.050_r_kind, 0.050_r_kind, 0.050_r_kind, 0.050_r_kind, &
            0.050_r_kind, 0.050_r_kind, 0.050_r_kind, 0.050_r_kind /)
  endif

! Loop over observations.

  efact     =one
  vfact     =one

!    Over sea
  if(sea) then

!    rain qc
     if( kraintype /= 0 ) then
        efact=zero; vfact=zero
        if(luse) then
           aivals(8) = aivals(8) + one

           do i=1,nchanl
              varinv(i)=zero
              if( id_qc(i)== igood_qc .and. kraintype/= 0) id_qc(i)=ifail_krain_amsr2_qc
           end do
        end if

     else if(clw > zero)then

!      If dtb is larger than demissivity and dwmin contribution,
!      it is assmued to be affected by  rain and cloud, tossing it out
        do l=1,nchanl

!          clw QC using ch-dependent threshold (clwch)
           if( clw > clwcutofx(l) ) then
              varinv(l)=zero
              if(luse) then
                 aivals(10) = aivals(10) + one
                 if(id_qc(l)== igood_qc) then
                    id_qc(l)=ifail_cloud_qc
                    aivals(9)=aivals(9) + one
                 end if
              end if
           end if
        end do  !l_loop
     end if

! flag points where channel 1 tbs > 200K, these are probably cloud

     if( tbobs(1) > 200.0_r_kind ) then
        do l=1,nchanl
           varinv(l)=zero
           if(luse) then
              aivals(10) = aivals(10) + one
              if(id_qc(l)== igood_qc) then
                 id_qc(l)=ifail_cloud_qc
                 aivals(9)=aivals(9) + one
              end if
           end if
        end do
     end if


! calculate and flag sun glint
    solel = 90.0_r_kind-solzen
    solazi_rad = solazi*deg2rad
    solel_rad = solel*deg2rad

    ang = atan(solel_rad/solazi_rad)
    ang_a = ( (solazi*cos(ang)) - (solel*sin(ang)) )
    ang_b = ( (solazi*sin(ang)) + (solel*cos(ang)) )
    ang_ab = sqrt(ang_a**2 + ang_b**2)

! only flag first 6 channels for sun glint    
!    do l=1,nchanl
    do l=1,6             
       if (ang_ab < 26.0_r_kind) then
         varinv(l)=zero
         if(luse) then
            if(id_qc(l)== igood_qc) then
               id_qc(l)=ifail_amsr2_glint_qc
            endif
         endif
       endif 
    enddo
      
!   Calculate emissivity and flag observations over thresholds
!   Calculations for ch 3,4,5
    nch_emrgr = 14
    idxch_emrgr = (/1,2,3,4,5,6,7,8,9,10,11,12,13,14/)
 
    ! Brightness temperatures used for training emissivity retrievals were
    ! simulated from ECMWF fields collocated with AMSR2 observations. The retrievals
    ! here use actual GMI brightness temperatures, so for best results, a
    ! "systematic bias" (i.e. an average difference between AMSR2 brightness
    ! temperatures and those simulated from ECMWF fields) is removed from AMSR2
    ! brightness temperatures prior to performing retrievals

    ! systematic bias
    sys_bias= (/ 0.4800_r_kind, 3.0737_r_kind, 0.7433_r_kind, 3.6430_r_kind,&
                 3.5304_r_kind, 4.4270_r_kind, 5.1448_r_kind, 5.0785_r_kind,&
                 4.9763_r_kind, 9.3215_r_kind, 2.5789_r_kind, 5.5274_r_kind,&
                 0.6641_r_kind, 1.3674_r_kind /)

    ! brightness temperatures to use
    tb_use(1)=(tbobs(1)-sys_bias(1)); tb_use(2)=(tbobs(2)-sys_bias(2)); tb_use(3)=(tbobs(3)-sys_bias(3))
    tb_use(4)=(tbobs(4)-sys_bias(4)); tb_use(5)=(tbobs(5)-sys_bias(5)); tb_use(6)=(tbobs(6)-sys_bias(6))
    tb_use(7)=(tbobs(7)-sys_bias(7)); tb_use(8)=(tbobs(8)-sys_bias(8)); tb_use(9)=(tbobs(9)-sys_bias(9))
    tb_use(10)=(tbobs(10)-sys_bias(10)); tb_use(11)=(tbobs(11)-sys_bias(11)); tb_use(12)=(tbobs(12)-sys_bias(12))
    tb_use(13)=(tbobs(13)-sys_bias(13)); tb_use(14)=(tbobs(14)-sys_bias(14))

    ! Set regression constants and coefficients
    ! first set of constants and coefficients (using all channels)
    c36h = 1.18467_r_kind
    c89h = 1.73315_r_kind

    rgr_coeff_36h = (/ -0.00098_r_kind, 0.00145_r_kind, -0.00146_r_kind, 0.00055_r_kind, &
                       -0.00232_r_kind, 0.00061_r_kind, 0.00160_r_kind, 0.00001_r_kind, &
                       -0.00053_r_kind, -0.00019_r_kind, -0.00272_r_kind, 0.00104_r_kind, &
                       -0.00026_r_kind, 0.00032_r_kind /)
    rgr_coeff_89h = (/ -0.00141_r_kind, 0.00217_r_kind, -0.00214_r_kind, 0.00070_r_kind, &
                       -0.00358_r_kind, 0.00110_r_kind, 0.00199_r_kind, 0.00002_r_kind, &
                       -0.00131_r_kind, 0.00003_r_kind, -0.00318_r_kind, 0.00122_r_kind, &
                       -0.00043_r_kind, 0.00047_r_kind /)

    ! second set of constants and coefficients (single channel regression)
    d36h = 1.08529_r_kind
    d89h = 1.66380_r_kind

    rgr_coeff2_36h = (/ 0.00017_r_kind, -0.00269_r_kind /)
    rgr_coeff2_89h = (/ 0.00017_r_kind, -0.00433_r_kind /)

    ! perform regressions
    ! first set
    em36h = c36h
    em89h = c89h
    do i=1,nch_emrgr
      idx=idxch_emrgr(i)
      em36h=em36h+(tb_use(idx)*rgr_coeff_36h(i))    ! 36h multi-channel emiss
      em89h=em89h+(tb_use(idx)*rgr_coeff_89h(i))    ! 89h multi-channel emiss
    end do

    ! second set, using tskin
    ! 36h single-channel emiss
    em2_36h = d36h + ( tb_use(12)*rgr_coeff2_36h(1) ) + ( rgr_coeff2_36h(2) * tsavg5 )
    ! 36h single-channel emiss
    em2_89h = d89h + ( tb_use(14)*rgr_coeff2_89h(1) ) + ( rgr_coeff2_89h(2) * tsavg5 )

    ! calculate differences between emissivity regressions
    ! single channel less multiple channel
    diff_em_36h = em2_36h - em36h
    diff_em_89h = em2_89h - em89h

    ! check emissivity difference values against thresholds and assign flag if
    ! needed
!    if ( (diff_em_36h > 0.015_r_kind) .or. (diff_em_89h > 0.015_r_kind) ) then
    if ( (diff_em_36h > 0.008_r_kind) .or. (diff_em_89h > 0.008_r_kind) .or. &
         (diff_em_36h < -0.030_r_kind) .or. (diff_em_89h < -0.030_r_kind) ) then
       do i=1,14
          varinv(1:14)=zero
          if (id_qc(i) == igood_qc) id_qc(i)=ifail_emiss_qc
       end do
    end if

!    Use data not over over sea
  else  !land,sea ice,mixed

!   Reduce q.c. bounds over higher topography
     efact=zero
     vfact=zero

     if (sfchgt > r2000) then
        fact = r2000/sfchgt
        efact = fact*efact
        vfact = fact*vfact
     end if
  end if

  return
end subroutine qc_amsr2

subroutine qc_saphir(nchanl,sfchgt,luse,sea, &
     kraintype,varinv,aivals,id_qc)
!    kraintype,varinv,aivals,id_qc,radmod) ! all-sky
!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_saphir     QC for SAPHIR TBs
!
!   prgmmr: ejones         org: jcsda            date: 2015-03-23
!
! abstract: set quality control criteria for SAPHIR; check for rainy obs
!
! program history log:
!     2015-03-23  ejones
!
! input argument list:
!     nchanl       - number of channels per obs
!     sfchgt  - surface height (not use now)
!     luse    - logical use flag
!     sea     - logical, sea flag
!     kraintype - [0]no rain, [others]rain ; see retrieval_mi
!     saphir  - logical true if saphir is processed
!
! output argument list:
!     varinv  - observation weight (modified obs var error inverse)
!     aivals  - number of data not passing QC
!     id_qc   - qc index - see qcmod definitions
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: nchanl
  integer(i_kind)                  ,intent(in   ) :: kraintype 
  integer(i_kind),dimension(nchanl),intent(inout) :: id_qc

  logical                          ,intent(in   ) :: sea,luse

  real(r_kind)                     ,intent(in   ) :: sfchgt

  real(r_kind)   ,dimension(nchanl),intent(inout) :: varinv !,errf
  real(r_kind)   ,dimension(40)    ,intent(inout) :: aivals

! Declare local variables 
  real(r_kind)    :: efact,vfact,fact
  integer(i_kind) :: i
!------------------------------------------------------------------

! Loop over observations.

  efact     =one
  vfact     =one

!    Over sea
  if(sea) then

!    rain qc
     if( kraintype /= 0 ) then
        efact=zero; vfact=zero
        if(luse) aivals(8) = aivals(8) + one
        do i=1,nchanl
           varinv(i)=zero
           if( id_qc(i)== igood_qc .and. kraintype/= 0) id_qc(i)=ifail_krain_saphir_qc
        end do
     end if

!    Use data not over over sea
  else  !land,sea ice,mixed

!   Reduce q.c. bounds over higher topography
     efact=zero
     vfact=zero

     if (sfchgt > r2000) then
        fact = r2000/sfchgt
        efact = fact*efact
        vfact = fact*vfact
     end if
  end if

  return
end subroutine qc_saphir

subroutine qc_irsnd(nchanl,is,ndat,nsig,ich,sea,land,ice,snow,luse,goessndr,airs,                         &
     cris,iasi,iasing,hirs,zsges,cenlat,frac_sea,pangs,trop5,zasat,tzbgr,tsavg5,tbc,tb_obs,tbcnob,tnoise, &
     wavenumber,ptau5,prsltmp,tvp,temp,wmix,chan_level,emissivity_k,ts,tsim,                   &
     id_qc,aivals,errf,varinv,varinv_use,cld,cldp,kmax,zero_irjaco3_pole,cluster_fraction,    &
     cluster_bt, chan_stdev, model_bt)
!    id_qc,aivals,errf,varinv,varinv_use,cld,cldp,kmax,zero_irjaco3_pole,radmod) ! all-sky

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_irsnd    QC for ir sounder data(hirs,goessndr,airs,iasi,iasing,cris)
!
!   prgmmr: derber           org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for ir sounder data (hirs, 
!          goessndr, airs, iasi, iasing, cris)
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!     2011-08-20  zhu    add cloud qc for passive channels based on the cloud
!                        level determined by channels with irad_use=1 and 0
!     2015-03-26   mkim  add extra qc for sfc sensitive channels
!                        These qc are optional.(On/off by depending on the number in satinfo table)
!
! input argument list:
!     nchanl       - number of channels per obs
!     ich          - channel number
!     is           - integer counter for number of observation types to process
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     luse         - logical use flag
!     goessndr     - logical flag - if goessndr data - true
!     cris         - logical flag - if cris data - true
!     avhrr        - logical flag - if avhrr data - true
!     iasi         - logical flag - if iasi data - true
!     iasing       - logical flag - if iasing data - true
!     zsges        - elevation of guess
!     cenlat       - latitude of observation
!     frac_sea     - fraction of grid box covered with water
!     pangs        - solar zenith angle
!     trop5        - tropopause pressure
!     zasat        - satellite zenith angle
!     tzbgr        - Tz over water
!     tsavg5       - surface skin temperature
!     tbc          - simulated - observed BT with bias correction
!     tsim         - simulated BT 
!     tb_obs       - observed Brightness temperatures
!     tnoise       - channel noise array
!     wavenumber   - array of channel wavenumbers
!     ptau5        - transmittances as a function of level and channel
!     prsltmp      - array of layer pressures in vertical (surface to toa)
!     tvp          - array of temperatures in vertical (surface to toa)
!     temp         - temperature sensitivity array
!     wmix         - moisture sensitivity array
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     varinv_use   - observation weight used(modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     varinv_use   - observation weight used(modified obs var error inverse)
!     cld          - cloud fraction
!     cldp         - cloud pressure
!     zero_irjaco3_pole - logical to control use of ozone jacobians near poles
!     cluster_fraction - size of imager derived cluster to determine clear cloudy profiles, used by CADS
!     cluster_bt   - imager brightness temperature of each cluster, used by CADS
!     chan_stdev   - standard deviation of cluster mean temperatures, used by CADS
!     model_bt     _ brightness temperature derived from the model's clear profile. used by CADS
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use radinfo, only: iomg_det, itopo_det, isst_det
  use crtm_planck_functions, only: crtm_planck_radiance
  use cads, only: cloud_aerosol_detection
  implicit none

! Declare passed variables

  logical,                            intent(in   ) :: sea,land,ice,snow,luse,goessndr,airs,cris,hirs,iasi,iasing
  logical,                            intent(inout) :: zero_irjaco3_pole
  integer(i_kind),                    intent(in   ) :: nsig,nchanl,ndat,is
  integer(i_kind),dimension(nchanl),  intent(in   ) :: ich
  integer(i_kind),dimension(nchanl),  intent(inout) :: id_qc
  integer(i_kind),dimension(nchanl),  intent(in   ) :: kmax
  real(r_kind),                       intent(in   ) :: zsges,cenlat,frac_sea,pangs,trop5
  real(r_kind),                       intent(in   ) :: tzbgr,tsavg5,zasat
  real(r_kind),                       intent(  out) :: cld,cldp
  real(r_kind),dimension(40,ndat),    intent(inout) :: aivals
  real(r_kind),dimension(nchanl),     intent(in   ) :: tbc,emissivity_k,ts,wavenumber,tb_obs,tbcnob
  real(r_kind),dimension(nchanl),     intent(in   ) :: chan_level
  real(r_kind),dimension(nchanl),     intent(in   ) :: tnoise,tsim
  real(r_kind),dimension(nsig,nchanl),intent(in   ) :: ptau5,temp,wmix
  real(r_kind),dimension(nsig),       intent(in   ) :: prsltmp,tvp
  real(r_kind),dimension(nchanl),     intent(inout) :: errf,varinv,varinv_use
  real(r_kind),dimension(7),          intent(in   ) :: cluster_fraction
  real(r_kind),dimension(2,7),        intent(in   ) :: cluster_bt
  real(r_kind),dimension(2),          intent(in   ) :: chan_stdev, model_bt

! Declare local parameters

  real(r_kind),parameter:: oneover400=1.0_r_kind/400.0_r_kind


  real(r_kind) :: demisf,dtempf,efact,dtbf,term,cenlatx,sfchgtfact
  real(r_kind) :: sum1,sum2,sum3,tmp,dts,delta
  integer(i_kind) :: i,j,lcloud,m,isurface_chan
  integer(i_kind), dimension(nchanl) :: irday
  real(r_kind) :: dtz,ts_ave,xindx,tzchks
  real(r_kind),parameter:: tbmax = 550._r_kind
  real(r_kind),parameter:: tbmin = 50._r_kind

! for cloud_aerosol_detect
  integer(i_kind) :: I_Sensor_ID
  integer(i_kind),dimension(nchanl) :: chan_array, i_flag_cloud
  integer(i_kind),dimension(2) :: imager_chans
  integer(i_kind) :: boundary_layer_pres, tropopause_height
  integer(i_kind) :: ichan_10_micron, ichan_12_micron
  real(r_kind),dimension(nchanl) :: tb_bc
  real(r_kind) :: cloud_temperature, radiance_chan, radiance_model, radiance_cloud
  real(r_kind) :: tb_obs_10, tb_obs_12, tb_obs_diff 

! Reduce weight given to obs for shortwave ir if
! solar zenith angle tiny_r_kind
  irday = 1
  if (pangs <= 89.0_r_kind .and. frac_sea > zero) then
!    QC2 in statsrad
     if(luse) aivals(9,is) = aivals(9,is) + one
     do i=1,nchanl
        if(wavenumber(i) > r2000)then
           if(wavenumber(i) > r2400)then
              varinv(i)=zero
              varinv_use(i)=zero
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_2400_qc
              irday(i) = 0
           else
              tmp=one-(wavenumber(i)-r2000)*ptau5(1,i)&
                 *max(zero,cos(pangs*deg2rad))*oneover400
              varinv(i)=tmp*varinv(i)
              varinv_use(i)=tmp*varinv_use(i)
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_2000_qc
           end if
        end if
     end do
  endif

  if(sea)then
     demisf = r0_01
     dtempf = half
  else if(land)then
     demisf = r0_02
     dtempf = two
  else if(ice)then
     demisf = r0_03
     dtempf = four
  else if(snow)then
     demisf = r0_02
     dtempf = two
  else
     demisf = r0_03
     dtempf = four
  end if

! Optionally turn off ozone jacabians near poles
  zero_irjaco3_pole=.false.
  if (qc_noirjaco3_pole .and. (abs(cenlat)>r60)) zero_irjaco3_pole=.true.

! If GOES and lza > 60. do not use
  if( goessndr .and. zasat*rad2deg > r60) then
!    QC5 in statsrad
     if(luse) aivals(12,is) = aivals(12,is) + one
     do i=1,nchanl
        varinv(i) = zero
        varinv_use(i)=zero
        if(id_qc(i) == igood_qc)id_qc(i)=ifail_satzen_qc
     end do
  end if

! Reduce weight for obs over higher topography
  sfchgtfact=one
  if (zsges > r2000) then
!    QC1 in statsrad
     if(luse) aivals(8,is) = aivals(8,is) + one
     sfchgtfact    = (r2000/zsges)**4
  endif

! Generate q.c. bounds and modified variances for height change and ptau5
  sum3=zero
  do i=1,nchanl
     if (tb_obs(i) >= tbmax .or. tb_obs(i) <= tbmin) then
        varinv(i)=zero
        varinv_use(i)=zero
     end if
     tmp=one-(one-sfchgtfact)*ptau5(1,i)
     varinv(i) = varinv(i)*tmp
     varinv_use(i) = varinv_use(i)*tmp

!    Modify error based on transmittance at top of model
     varinv(i)=varinv(i)*ptau5(nsig,i)
     varinv_use(i)=varinv_use(i)*ptau5(nsig,i)
     errf(i)=errf(i)*ptau5(nsig,i)

!    QC based on presence/absence of cloud
     sum3=sum3+tbc(i)*tbc(i)*varinv_use(i)
  end do
  sum3=0.75_r_kind*sum3
  lcloud=0
  cld=zero
  cldp=r10*prsltmp(1)

!  Cloud and aerosol detection routines (ECMWF)
  if (cris .and. cris_cads) then
      I_Sensor_ID = 27
      chan_array = nuchan(ich)                  ! channel numbers
      tb_bc = tbc + tsim                        ! observation BT with bias correction
      boundary_layer_pres = nint(0.8_r_kind*prsltmp(1))  !  boundary layer set to be 80% of surface pressure
      tropopause_height = nint(trop5)
      imager_chans = (/15,16/)                  ! imager channel numbers (from satinfo)
      isurface_chan = 501                       ! surface channel
      ichan_10_micron = 458                     ! ~10.7 micron channel for low level cloud test
      ichan_12_micron = 295                     ! ~12.0 micron channel for low level cloud test

      call cloud_aerosol_detection( I_Sensor_ID, nchanl, chan_array, &
             tropopause_height, boundary_layer_pres, tb_bc, tsim, chan_level, imager_chans, cluster_fraction, &
             cluster_bt, chan_stdev, model_bt, i_flag_cloud, cldp )

  elseif ( iasi .and. iasi_cads ) then
      I_Sensor_ID = 16
      chan_array = nuchan(ich)                  ! channel numbers
      tb_bc = tbc + tsim                        ! observation BT with bias correction
      boundary_layer_pres = nint(0.8_r_kind*prsltmp(1))  !  boundary layer set to be 80% of surface pressure
      tropopause_height = nint(trop5)
      imager_chans = (/2,3/)                    ! imager channel numbers (from satinfo)
      isurface_chan = 1271                      ! surface channel
      ichan_10_micron = 1173                    ! ~10.7 micron channel for low level cloud test
      ichan_12_micron = 756                     ! ~12.0 micron channel for low level cloud test
      
      call cloud_aerosol_detection( I_Sensor_ID, nchanl, chan_array, &
             tropopause_height, boundary_layer_pres, tb_bc, tsim, chan_level, imager_chans, cluster_fraction, &
             cluster_bt, chan_stdev, model_bt, i_flag_cloud, cldp )

  elseif ( iasing .and. iasing_cads ) then
      I_Sensor_ID = 59
      chan_array = nuchan(ich)                  ! channel numbers
      tb_bc = tbc + tsim                        ! observation BT with bias correction
      boundary_layer_pres = nint(0.8_r_kind*prsltmp(1))  !  boundary layer set to be 80% of surface pressure
      tropopause_height = nint(trop5)
      imager_chans = (/18,19/)                  ! imager channel numbers (from satinfo)
      isurface_chan = 2539                      ! surface channel
      ichan_10_micron = 2343                    ! ~10.7 micron channel for low level cloud test
      ichan_12_micron = 1509                    ! ~12.0 micron channel for low level cloud test
      
      call cloud_aerosol_detection( I_Sensor_ID, nchanl, chan_array, &
             tropopause_height, boundary_layer_pres, tb_bc, tsim, chan_level, imager_chans, cluster_fraction, &
             cluster_bt, chan_stdev, model_bt, i_flag_cloud, cldp )

  elseif ( airs .and. airs_cads ) then
      I_Sensor_ID = 11
      chan_array = nuchan(ich)                  ! channel numbers
      tb_bc = tbc + tsim                        ! observation BT with bias correction
      boundary_layer_pres = nint(0.8_r_kind*prsltmp(1))  !  boundary layer set to be 80% of surface pressure
      tropopause_height = nint(trop5)
      isurface_chan = 914                       ! surface channel
      imager_chans = (/0,0/)                    ! imager channel numbers (from satinfo)
      ichan_10_micron = 843                     ! ~10.7 micron channel for low level cloud test
      ichan_12_micron = 587                     ! ~12.0 micron channel for low level cloud test

      call cloud_aerosol_detection( I_Sensor_ID, nchanl, chan_array, &
             tropopause_height, boundary_layer_pres, tb_bc, tsim, chan_level, imager_chans, cluster_fraction, &
             cluster_bt, chan_stdev, model_bt, i_flag_cloud, cldp )

  else
     call emc_legacy_cloud_detect(nchanl,nsig,tsavg5,trop5,prsltmp,tvp,ts,tbc,temp,varinv_use,lcloud,cld,cldp)

  endif    ! end of which cloud test to use

! compute cloud stats 
! If using CADS
  if ((cris .and. cris_cads) .or. (iasi .and. iasi_cads) .or. (airs .and. airs_cads) .or. &
       iasing .and. iasing_cads ) then

!   Reject channels affected by clouds
    do i=1, nchanl
       if ( i_flag_cloud(i) == 1) then
!          QC4 in statsrad
         if(luse) aivals(11,is) = aivals(11,is) + one
         varinv(i) = zero
         varinv_use(i) = zero
         if(id_qc(i) == igood_qc) id_qc(i) = ifail_cloud_qc
       endif
    end do

!   Derive cloud amount for CADS
    cld = zero
    if ( cldp < prsltmp(1) ) then    ! if cloud in this profile exists 
      cloud_layer: do i=2, nsig                   ! determine which layer the cloud exists.
        if (prsltmp(i) < cldp) then
          lcloud = i 
          do j=1, nchanl                      ! use surface channel to derive cloud amount
            m = nuchan(ich(j))
            if ( m == isurface_chan ) then     ! interpolate cloud top temperature
              cloud_temperature = ((tvp(lcloud) -tvp(lcloud -1)/ log(prsltmp(lcloud) / prsltmp(lcloud - 1))) &
                  * log(cldp/prsltmp(lcloud-1))) + tvp(lcloud-1)
              call crtm_planck_radiance(1,m,tb_bc(j),radiance_chan)           ! observation radiance. same as tb_obs + bias correction
              call crtm_planck_radiance(1,m,tsim(j),radiance_model)           ! model derived radiance
              call crtm_planck_radiance(1,m,cloud_temperature,radiance_cloud) ! cloud top temperature radiance
              cld = (radiance_chan - radiance_model) / (radiance_cloud - radiance_model)
              cld = min(max(cld,zero),one)
              cldp = cldp * r10
              exit  cloud_layer ! cloud layer foound and cloud amount computed
            endif   ! surface channel found
          end do    !surface_chan
        endif       ! cloud found  (prsltmp(i) < cldp)
      end do cloud_layer

!     If clear, do a 10.7 - 12 micron test for low level clouds
    else   ! lcloud = 0
      do i=1, nchanl
        if ( nuchan(ich(i)) == ichan_10_micron ) tb_obs_10 = tb_obs(i)
        if ( nuchan(ich(i)) == ichan_12_micron ) tb_obs_12 = tb_obs(i)
      end do 
      if ( tb_obs_10 > zero .and. tb_obs_12 > zero ) then
          tb_obs_diff = tb_obs_10 - tb_obs_12
        if ( tb_obs_diff > 2.20_r_kind ) then  ! Assume a cloud exists
          cldp = prsltmp(1) * r10              ! Assume near surface cloud
          cld = one                            ! Assume overcast cloud
          lcloud = 1
        endif
      endif
    endif

!   If more than 2% of the transmittance comes from the cloud layer, reject the channel (0.02 is a tunable parameter).
!   or CADS flagged a channel to have cloud.
    if ( lcloud > 0 ) then
      do i=1, nchanl
        if ( ptau5(lcloud,i) > 0.02_r_kind )  then
           if(luse) aivals(11,is) = aivals(11,is) + one    ! QC4 in statsrad
           varinv(i) = zero
           varinv_use(i) = zero
          if(id_qc(i) == igood_qc) id_qc(i) = ifail_cloud_qc
        end if
      end do
    endif

! default compute cloud stats, emc_legacy_cloud_detect 
  else  
    if ( lcloud > 0 ) then
      do i=1,nchanl
!       reject channels with iuse_rad(j)=-1 when they are peaking below the cloud
        j=ich(i)
        if (passive_bc .and. iuse_rad(j)==-1) then
          if (lcloud .ge. kmax(i)) then
            if(luse)aivals(11,is)   = aivals(11,is) + one
            varinv(i) = zero
            varinv_use(i) = zero
            if(id_qc(i) == igood_qc)id_qc(i)=ifail_cloud_qc
            cycle
          end if
        end if

!       If more than 2% of the transmittance comes from the cloud layer,
!          reject the channel (0.02 is a tunable parameter)

        if ( ptau5(lcloud,i) > 0.02_r_kind) then
!         QC4 in statsrad
          if(luse) aivals(11,is) = aivals(11,is) + one
          varinv(i) = zero
          varinv_use(i) = zero
          if(id_qc(i) == igood_qc) id_qc(i) = ifail_cloud_qc
        end if
      end do

    else   ! surface consistency and sensitivity chacks. ( if lcoud = 0 )
      sum1=zero
      sum2=zero
      do i=1,nchanl
        if ( varinv_use(i) > tiny_r_kind .and. ts(i) > 0.0001_r_kind) then
          sum1 = sum1 +tbc(i)*ts(i)*varinv_use(i)
          sum2 = sum2+ts(i)*ts(i)*varinv_use(i)
        endif
      end do
      if (abs(sum2) < tiny_r_kind) sum2 = sign(tiny_r_kind,sum2)
      dts=abs(sum1/sum2)
      if(abs(dts) > one)then
        if(.not. sea)then
          dts=min(dtempf,dts)
        else
          dts=min(three,dts)
        end if
        do i=1,nchanl
          delta=max(r0_05*tnoise(i),r0_02)
          if(abs(dts*ts(i)) > delta)then
!           QC3 in statsrad
            if(luse .and. varinv(i) > zero) aivals(10,is) = aivals(10,is) + one
            varinv(i) = zero
            if(id_qc(i) == igood_qc)id_qc(i)=ifail_sfcir_qc
          endif
        enddo
      endif
    endif   

! Temporary additional check for CrIS to reduce influence of land points on window channels (particularly important for bias correction)
    if (cris .and. .not. sea) then
       do i=1,nchanl
          if (ts(i) > 0.2_r_kind) then
           !             QC3 in statsrad
            if(luse .and. varinv(i) > zero) &
                aivals(10,is) = aivals(10,is) + one
            varinv(i) = zero
            if(id_qc(i) == igood_qc) id_qc(i) = ifail_sfcir_qc
          end if
       end do
    end if

  endif  ! derive cloud stats
!
! Apply Tz retrieval
!
  if(tzr_qc > 0)then
     dtz = rmiss_single
     if ( sea ) then
        call tz_retrieval(nchanl,nsig,ich,irday,temp,wmix,tnoise,varinv,ts,tbc,tzbgr,1,0,dtz,ts_ave) 
     endif
!
! Apply QC with Tz retrieval
!
     if (dtz /= rmiss_single ) then
       do i = 1, nchanl
         if ( varinv(i) > tiny_r_kind .and. iuse_rad(ich(i)) >= 1 .and. ts(i) > tschk ) then
           xindx = ((ts(i)-ts_ave)/(one-ts_ave))**3
           tzchks = tzchk*(half)**xindx
   
           if ( abs(dtz) > tzchks ) then
              varinv(i) = zero
              if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_tzr_qc
              if(luse)aivals(13,is) = aivals(13,is) + one
           endif
         endif
       enddo
     endif
  end if

  cenlatx=abs(cenlat)*r0_04     
  if (cenlatx < one) then
     if(luse) aivals(6,is) = aivals(6,is) + one
     efact   = half*(cenlatx+one)
     do i=1,nchanl
        if(varinv(i) > tiny_r_kind) errf(i)=efact*errf(i)
     end do
  endif

! Generate q.c. bounds and modified variances.
  do i=1,nchanl
     if(varinv(i) > tiny_r_kind)then
        dtbf = demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term = dtbf*dtbf
        if(term > tiny_r_kind) varinv(i) = varinv(i)/(one+varinv(i)*term)
     end if
  end do

  do i=1,nchanl
     m=ich(i)
     if (sea .and. isst_det(m) > 0 .and. tsavg5 < 274.0_r_kind) then
        varinv(i) = zero
        if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_isst_det
     endif
  end do


  if(hirs) then
    do i=1,nchanl
      m=ich(i)
      if(iomg_det(m) > 0 .and. i >= 4 .and. i <= 12)then
         if (i < 4 .and. abs(tbcnob(8)) > 40.0_r_kind) then
            varinv(i) = zero
            if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
         else if(i == 12 .and. abs(tbcnob(8)) > 10.0_r_kind)  then
            varinv(i) = zero
            if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
         endif
!90S-60S
         if(cenlat >= -90.0_r_kind .and. cenlat < -60.0_r_kind) then
            if(i == 4 .and. abs(tbcnob(8)) > 12.0_r_kind)  then
              varinv(i) = zero
              if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
            else if(i == 5 .and. abs(tbcnob(8)) > 6.0_r_kind)  then
              varinv(i) = zero
              if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
            else if(i == 6 .and. abs(tbcnob(8)) > 4.0_r_kind)  then
                 varinv(i) = zero
                 if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
            endif
!60S-30S
         else if(cenlat >= -60.0_r_kind .and. cenlat < -30.0_r_kind) then
            if(i == 4 .and. abs(tbcnob(8)) > 10.0_r_kind)  then
               varinv(i) = zero
               if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
            else if( i == 5 .and. abs(tbcnob(8)) > 2.0_r_kind)  then
               varinv(i) = zero
               if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
            else if(i == 6 .and. abs(tbcnob(8)) > 1.5_r_kind)  then
               varinv(i) = zero
               if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
            endif
!30S-30N
         else if(cenlat >= -30.0_r_kind .and. cenlat < 30.0_r_kind ) then
            if(i == 4 .and. abs(tbcnob(8)) > 5.0_r_kind)  then
               varinv(i) = zero
               if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
            else if(i == 5 .and. (tbcnob(8) < -2.0_r_kind .or. tbcnob(8) > 3.0_r_kind))  then
               varinv(i) = zero
               if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
            else if(i == 6 .and. (tbcnob(8) < -1.5_r_kind .or. tbcnob(8) > 3.0_r_kind))  then
               varinv(i) = zero
               if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
            endif
!30N-60N
         else if(cenlat >= 30.0_r_kind .and. cenlat < 60.0_r_kind) then
           if(i == 4 .and. abs(tbcnob(8)) > 8.0_r_kind)  then
              varinv(i) = zero
              if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
           else if(i == 5 .and. abs(tbcnob(8)) > 2.0_r_kind)  then
              varinv(i) = zero
              if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
           else if(i == 6 .and. abs(tbcnob(8)) > 1.0_r_kind)  then
              varinv(i) = zero
              if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_iomg_det
           endif
         endif  !cenlat
      end if

      if (itopo_det(m) > 0 .and. zsges > 1500.0_r_kind) then
         varinv(i) = zero
         if ( id_qc(i) == igood_qc ) id_qc(i) = ifail_itopo_det
      endif
    end do
  endif !! if (hirs)
!---mkim

  return

end subroutine qc_irsnd

subroutine emc_legacy_cloud_detect(nchanl,nsig,tsavg5,trop5,prsltmp,tvp,ts,tbc,temp,varinv_use,lcloud,cld,cldp)

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  emc_legacy_cloud_detect    determine clear/cloudy profiles from hirs,goessndr,airs,iasi,cris instruments
!
!   prgmmr: derber ???        org: np23            date: ???
!
! abstract: determine if a profile is clear/cloudy.  If cloudy, determine model layer of the lcoud.
!           This subroutine is designed for infrared sounders.
!
! program history log:
!     2022-06-20  jung   moved into a subroutine 
!
! input argument list:
!     nchanl       - number of channels per obs
!     nsig         - number of model layers
!     tsavg5       - surface skin temperature
!     trop5        - tropopause pressure
!     prsltmp      - array of layer pressure in vertical (surface to toa)
!     tvp          - array of temperatures in vertical (surface to toa)
!     ts           - skin temperature sensitivity
!     tbc          - simulated - observed BT with bias correction
!     temp         - temperature sensitivity array
!     varinv_use   - observation weight used (modified obs var error inverse)
!     
! output argument list:
!     lcloud       - model layer of cloud
!     cld          - derived cloud amount
!     cldp         - model layer pressure (hPa) of cloud 
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only: i_kind, r_kind
use constants, only: tiny_r_kind, zero, r10
implicit none

integer(i_kind),                 intent(in   ) :: nchanl, nsig
integer(i_kind),                 intent(  out) :: lcloud
real(r_kind),                    intent(in   ) :: tsavg5, trop5
real(r_kind),                    intent(  out) :: cld, cldp
real(r_kind), dimension(nchanl), intent(in   ) :: tbc, ts, varinv_use
real(r_kind), dimension(nsig,nchanl), intent(in   ) :: temp
real(r_kind), dimension(nsig),   intent(in   ) :: tvp, prsltmp

integer(i_kind) :: i, k, kk

real(r_kind) :: sum,sum2,sum3,cloudp,tmp
real(r_kind),dimension(nchanl) :: dtb

  sum3=zero
  do i=1,nchanl
    sum3=sum3+tbc(i)*tbc(i)*varinv_use(i)
  end do
  sum3=0.75_r_kind*sum3
  lcloud=0
  cld=zero
  cldp=r10*prsltmp(1)

  do k=1,nsig
     if(prsltmp(k) > trop5)then
        do i=1,nchanl
           dtb(i)=(tvp(k)-tsavg5)*ts(i)
        end do
        do kk=1,k-1
           do i=1,nchanl
              dtb(i)=dtb(i)+(tvp(k)-tvp(kk))*temp(kk,i)
           end do
        end do
        sum=zero
        sum2=zero
        do i=1,nchanl
           if(varinv_use(i) > tiny_r_kind)then
              sum=sum+tbc(i)*dtb(i)*varinv_use(i)
              sum2=sum2+dtb(i)*dtb(i)*varinv_use(i)
           end if
        end do
        if (abs(sum2) < tiny_r_kind) sum2 = sign(tiny_r_kind,sum2)
        cloudp=min(max(sum/sum2,zero),one)
        sum=zero
        do i=1,nchanl
           if(varinv_use(i) > tiny_r_kind)then
              tmp=tbc(i)-cloudp*dtb(i)
              sum=sum+tmp*tmp*varinv_use(i)
           end if
        end do
        if(sum < sum3)then
           sum3=sum
           lcloud=k
           cld=cloudp
           cldp=r10*prsltmp(k)
        end if
     end if

  end do

end subroutine emc_legacy_cloud_detect



subroutine qc_avhrr(nchanl,is,ndat,nsig,ich,sea,land,ice,snow,luse,   &
     zsges,cenlat,frac_sea,pangs,trop5,tzbgr,tsavg5,tbc,tb_obs,tnoise,     &
     wavenumber,ptau5,prsltmp,tvp,temp,wmix,emissivity_k,ts, &
     id_qc,aivals,errf,varinv,varinv_use,cld,cldp)
!    id_qc,aivals,errf,varinv,varinv_use,cld,cldp,radmod) ! all-sky

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_avhrr    QC for avhrr
!
!   prgmmr: li           org: np23            date: 2011-04-08
!
! abstract: set quality control criteria for avhrr
!
! program history log:
!     2011-04-08  li modified from qc_irsnd
!     2018-02-15  li : Zeroing dtb since it used outside the loop in which is defined
!
! input argument list:
!     nchanl       - number of channels per obs
!     ich          - channel number
!     is           - integer counter for number of observation types to process
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     luse         - logical use flag
!     zsges        - elevation of guess
!     cenlat       - latitude of observation
!     frac_sea     - fraction of grid box covered with water
!     pangs        - solar zenith angle
!     trop5        - tropopause pressure
!     tzbgr        - Tz over water
!     tsavg5       - surface skin temperature
!     tbc          - simulated - observed BT with bias correction
!     tb_obs       - observed Brightness temperatures
!     tnoise       - channel noise array
!     wavenumber   - array of channel wavenumbers
!     ptau5        - transmittances as a function of level and channel
!     prsltmp      - array of layer pressures in vertical (surface to toa)
!     tvp          - array of temperatures in vertical (surface to toa)
!     temp         - temperature sensitivity array
!     wmix         - moisture sensitivity array
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     varinv_use   - observation weight used(modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     varinv_use   - observation weight used(modified obs var error inverse)
!     cld          - cloud fraction
!     cldp         - cloud pressure
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                            intent(in   ) :: sea,land,ice,snow,luse
  integer(i_kind),                    intent(in   ) :: ndat,nsig,nchanl,is
  integer(i_kind),dimension(nchanl),  intent(in   ) :: ich
  integer(i_kind),dimension(nchanl),  intent(inout) :: id_qc
  real(r_kind),                       intent(in   ) :: zsges,cenlat,frac_sea,pangs,trop5
  real(r_kind),                       intent(in   ) :: tzbgr,tsavg5
  real(r_kind),                       intent(  out) :: cld,cldp
  real(r_kind),dimension(40,ndat),    intent(inout) :: aivals
  real(r_kind),dimension(nchanl),     intent(in   ) :: tbc,emissivity_k,ts,wavenumber,tb_obs
  real(r_kind),dimension(nchanl),     intent(in   ) :: tnoise
  real(r_kind),dimension(nsig,nchanl),intent(in   ) :: ptau5,temp,wmix
  real(r_kind),dimension(nsig),       intent(in   ) :: prsltmp,tvp
  real(r_kind),dimension(nchanl),     intent(inout) :: errf,varinv,varinv_use

! Declare local parameters

  real(r_kind),parameter:: oneover400=1.0_r_kind/400.0_r_kind

  real(r_kind) :: demisf,dtempf,efact,dtbf,term,cenlatx,sfchgtfact
  real(r_kind) :: sum1,sum2,sum3,cloudp,tmp,dts
  real(r_kind),dimension(nchanl,nsig) :: dtb
  integer(i_kind) :: i,k,kk,lcloud
  integer(i_kind), dimension(nchanl) :: irday
  real(r_kind) :: dtz,ts_ave,xindx,tzchks


  irday = 1

! Reduce weight given to obs for shortwave ir if
! solar zenith angle tiny_r_kind
  if (pangs <= 89.0_r_kind .and. frac_sea > zero) then
!    QC2 in statsrad
     if(luse)aivals(9,is) = aivals(9,is) + one
     do i=1,nchanl
        if(wavenumber(i) > r2000)then
           if(wavenumber(i) > r2400)then
              varinv(i)=zero
              varinv_use(i)=zero
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_2400_qc
              irday(i) = 0
           else
              tmp=one-(wavenumber(i)-r2000)*ptau5(1,i)&
                   *max(zero,cos(pangs*deg2rad))*oneover400
              varinv(i)=tmp*varinv(i)
              varinv_use(i)=tmp*varinv_use(i)
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_2000_qc
           end if
        end if
     end do
  endif

  if(sea)then
     demisf = r0_01
     dtempf = half
  else if(land)then
     demisf = r0_02
     dtempf = two
  else if(ice)then
     demisf = r0_03
     dtempf = four
  else if(snow)then
     demisf = r0_02
     dtempf = two
  else
     demisf = r0_03
     dtempf = four
  end if

! Reduce weight for obs over higher topography
  sfchgtfact=one
  if (zsges > r2000) then
!    QC1 in statsrad
     if(luse)aivals(8,is) = aivals(8,is) + one
     sfchgtfact    = (r2000/zsges)**4
  endif

! Generate q.c. bounds and modified variances for height change and ptau5
  sum3=zero
  do i=1,nchanl
     if (tb_obs(i) > r1000 .or. tb_obs(i) <= zero) then
         varinv(i)=zero
         varinv_use(i)=zero
     end if
     varinv(i) = varinv(i)*(one-(one-sfchgtfact)*ptau5(1,i))
     varinv_use(i) = varinv_use(i)*(one-(one-sfchgtfact)*ptau5(1,i))

!    Modify error based on transmittance at top of model
     varinv(i)=varinv(i)*ptau5(nsig,i)
     varinv_use(i)=varinv_use(i)*ptau5(nsig,i)
     errf(i)=errf(i)*ptau5(nsig,i)

!    QC based on presence/absence of cloud
     sum3=sum3+tbc(i)*tbc(i)*varinv_use(i)
  end do
  sum3=0.75_r_kind*sum3
  lcloud=0
  cld=zero
  cldp=r10*prsltmp(1)
!
! Zeroing dtb since it used outside the loop in which is defined
!
  dtb=zero

  do k=1,nsig
     if(prsltmp(k) > trop5)then
        sum1=zero
        sum2=zero
        do i=1,nchanl
           if(varinv_use(i) > tiny_r_kind)then
             dtb(i,k)=(tvp(k)-tsavg5)*ts(i)
             do kk=1,k-1
                dtb(i,k)=dtb(i,k)+(tvp(k)-tvp(kk))*temp(kk,i)
             end do
             sum1=sum1+tbc(i)*dtb(i,k)*varinv_use(i)
             sum2=sum2+dtb(i,k)*dtb(i,k)*varinv_use(i)
           end if
        end do
        if (abs(sum2) < tiny_r_kind) sum2 = sign(tiny_r_kind,sum2)
        cloudp=min(max(sum1/sum2,zero),one)
        sum1=zero
        do i=1,nchanl
           if(varinv_use(i) > tiny_r_kind)then
             tmp=tbc(i)-cloudp*dtb(i,k)
             sum1=sum1+tmp*tmp*varinv_use(i)
           end if
        end do
        if(sum1 < sum3)then
           sum3=sum1
           lcloud=k
           cld=cloudp
           cldp=r10*prsltmp(k)
        end if
     end if

  end do

  do i=1,nchanl
       cld_qc: do k=1,lcloud
        if(abs(cld*dtb(i,k)) > tnoise(i))then
!          QC4 in statsrad
           if(luse)aivals(11,is)   = aivals(11,is) + one
           varinv(i) = zero
           varinv_use(i) = zero
           if(id_qc(i) == igood_qc)id_qc(i)=ifail_cloud_qc
           exit cld_qc
        end if
     end do cld_qc
  end do

! If no clouds check surface temperature/emissivity

  sum1=zero
  sum2=zero
  do i=1,nchanl
     sum1=sum1+tbc(i)*ts(i)*varinv_use(i)
     sum2=sum2+ts(i)*ts(i)*varinv_use(i)
  end do
  if (abs(sum2) < tiny_r_kind) sum2 = sign(tiny_r_kind,sum2)
  dts=abs(sum1/sum2)
  if(abs(dts) > one)then
     if(.not. sea)then
        dts=min(dtempf,dts)
     else
        dts=min(three,dts)
     end if
     do i=1,nchanl
        if(abs(dts*ts(i)) > tnoise(i))then
!          QC3 in statsrad
           if(luse .and. varinv(i) > zero) &
           aivals(10,is)   = aivals(10,is) + one
           varinv(i) = zero
           if(id_qc(i) == igood_qc)id_qc(i)=ifail_sfcir_qc
       end if
     end do
  end if

!
! Apply Tz retrieval
!
  if(tzr_qc > 0)then
     dtz = rmiss_single
     if ( sea ) then
        call tz_retrieval(nchanl,nsig,ich,irday,temp,wmix,tnoise,varinv,ts,tbc,tzbgr,1,0,dtz,ts_ave) 
     endif
!
!    Apply QC with Tz retrieval
!
     if (dtz /= rmiss_single ) then
       do i = 1, nchanl
         if ( varinv(i) > tiny_r_kind .and. iuse_rad(ich(i)) >= 1 .and. ts(i) > tschk) then
           xindx = ((ts(i)-ts_ave)/(one-ts_ave))**3
           tzchks = tzchk*(half)**xindx
   
           if ( abs(dtz) > tzchks ) then
              varinv(i) = zero
           if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_tzr_qc
           if(luse)aivals(13,is) = aivals(13,is) + one
           endif
         endif
       enddo
     endif
  end if

  cenlatx=abs(cenlat)*r0_04     
  if (cenlatx < one) then
     if(luse)aivals(6,is) = aivals(6,is) + one
     efact   = half*(cenlatx+one)
     do i=1,nchanl
        if(varinv(i) > tiny_r_kind)errf(i)=efact*errf(i)
     end do
  endif

! Generate q.c. bounds and modified variances.
  do i=1,nchanl
     if(varinv(i) > tiny_r_kind)then
        dtbf = demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term = dtbf*dtbf
        if(term > tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     end if
  end do

  return
end subroutine qc_avhrr

subroutine qc_amsua(nchanl,is,ndat,nsig,npred,sea,land,ice,snow,mixed,luse,   &
     zsges,cenlat,tb_obsbc1,cosza,clw,tbc,ptau5,emissivity_k,ts, &                    
     pred,predchan,id_qc,aivals,errf,errf0,clwp_amsua,varinv,cldeff_obs,cldeff_fg,factch6, &
     cld_rbc_idx,sfc_speed,error0,clw_guess_retrieval,scatp,radmod)                     

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_amsua    QC for amsua data
!
!   prgmmr: derber           org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for AMSU-A data               
!
! program history log:
!     2010-08-10  derber - transfered from setuprad
!     2011-05-04  todling - partially merge Min-Jeong Kim's cloud radiance work
!     2011-05-20  mccarty - generalized routine so that it could be more readily 
!                           applied to atms
!     2011-07-20  collard - routine can now process the AMSU-B/MHS-like channels of ATMS.
!     2011-12-19  collard - ATMS 1-7 is always rejected over ice, snow or mixed surfaces.
!     2012-05-12  todling - revisit opts in gsi_metguess_get (4crtm)
!     2013-07-19  zhu     - tighten qc when emiss_bc=.t.
!     2013-12-10  eliu    - modify AMSU-A QC for all-sky condition
!     2014-01-31  mkim    - revisit qc for all-sky MW radiance data assimilationo
!     2014-04-27  eliu    - add two precipitation screenings for AMSU-A/ATMS 
!     2015-01-15  zhu     - apply emissivity sensitivity screening to all-sky radiance
!     2015-03-31  zhu     - observation error adjustments based on mis-matched
!                           cloud info, diff_clw, scattering and surface wind
!                           speed for AMSUA/ATMS cloudy radiance assimilation
!     2015-09-20  zhu     - add radmod to generalize all-sky condition for radiance
!     2016-10-13  zhu     - add codes for assimilating non-precipitating cloudy ATMS over ocean
!     2019-03-13  eliu    - add codes for assimilating precipitating-affected AMSUA/ATMS over ocean
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     npred        - number of predictors
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     mixed        - logical, mixed flag
!     luse         - logical use flag
!     zsges        - elevation of guess
!     tb_obsbc1    - bias corrected ob for channel 1
!     tzbgr        - Tz water temperature for FOV
!     cosza        - cosine of the satellite zenith angle
!     clw          - cloud liquid water estimate
!     tbc          - simulated - observed BT with bias correction
!     ptau5        - transmittances as a function of level and channel
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     pred         - bias correction predictors
!     predchan     - bias correction coefficients
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     cldeff_obs   - observed cloud effect 
!     factch6      - precipitation screening using channel 6 
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use mpeu_util, only: getindex
  use gsi_metguess_mod, only: gsi_metguess_get
  use radinfo, only: emiss_bc
  implicit none

! Declare passed variables

  logical,                             intent(in   ) :: sea,land,ice,snow,mixed,luse
  integer(i_kind),                     intent(in   ) :: ndat,nsig,npred,nchanl,is
  integer(i_kind),dimension(nchanl),   intent(inout) :: id_qc
  real(r_kind),                        intent(in   ) :: zsges,cenlat,tb_obsbc1
  real(r_kind),dimension(nchanl),      intent(in   ) :: cldeff_obs,cldeff_fg
  real(r_kind),                        intent(in   ) :: cosza,clw,clwp_amsua,clw_guess_retrieval
  real(r_kind),                        intent(in   ) :: sfc_speed,scatp
  real(r_kind),                        intent(inout) :: factch6  
  real(r_kind),dimension(40,ndat),     intent(inout) :: aivals
  real(r_kind),dimension(nchanl),      intent(in   ) :: tbc,emissivity_k,ts
  real(r_kind),dimension(nsig,nchanl), intent(in   ) :: ptau5
  real(r_kind),dimension(npred,nchanl),intent(in   ) :: pred,predchan
  real(r_kind),dimension(nchanl),      intent(inout) :: errf,errf0,varinv
  real(r_kind),dimension(nchanl),      intent(in   ) :: error0
  real(r_kind),dimension(nchanl),      intent(in   ) :: cld_rbc_idx
  type(rad_obs_type),                  intent(in   ) :: radmod

! Declare local parameters


  real(r_kind),parameter:: w1f6=1.0_r_kind/10.0_r_kind
  real(r_kind),parameter:: w2f6=1.0_r_kind/0.8_r_kind
  real(r_kind),parameter:: w1f4=1.0_r_kind/0.3_r_kind
  real(r_kind),parameter:: w2f4=1.0_r_kind/1.8_r_kind

  real(r_kind)    :: demisf,dtempf,efact,dtbf,term,cenlatx,fact
  real(r_kind)    :: efactmc,vfactmc,dtde1,dtde2,dtde3,dtde15,dsval,clwx
  real(r_kind)    :: de1,de2,de3,de15         
  real(r_kind)    :: thrd1,thrd2,thrd3,thrd15
  real(r_kind)    :: factch4
  real(r_kind)    :: ework,clwtmp
  real(r_kind)    :: icol
  integer(i_kind) :: i
  logical qc4emiss
  logical eff_area

  integer(i_kind) :: ich238, ich314, ich503, ich528, ich536 ! set chan indices
  integer(i_kind) :: ich544, ich549, ich890                 ! for amsua/atms
  logical         :: latms, latms_surfaceqc


  if (nchanl == 22) then
      latms  = .true.    ! If there are 22 channels passed along, it's atms
      ich238 =  1
      ich314 =  2
      ich503 =  3
      ich528 =  5
      ich536 =  6
      ich544 =  7
      ich549 =  8
      ich890 = 16
  else
      latms = .false.   ! If \= 16 channels (should be 15), it's amsua  
      ich238 =  1
      ich314 =  2
      ich503 =  3
      ich528 =  4
      ich536 =  5
      ich544 =  6
      ich549 =  7
      ich890 = 15
  endif

  if(sea)then
     demisf = r0_01
     dtempf = half
  else if(land)then
     demisf = r0_02
     dtempf = two
  else if(ice)then
     demisf = 0.015_r_kind  !decrease due to more accurate emiss model AMSU-A+B
     dtempf = one           !decrease due to more accurate emiss model AMSU-A+B
  else if(snow)then
     demisf = r0_02 !decrease due to more accurate emiss model AMSU-A+B
     dtempf = two   !decrease due to more accurate emiss model AMSU-A+B
  else
     demisf = 0.20_r_kind
     dtempf = 4.5_r_kind
  end if

  
! Reduce qc bounds in tropics
  cenlatx=abs(cenlat)*r0_04     
  if (cenlatx < one) then
     if(luse)aivals(6,is) = aivals(6,is) + one
     efact   = cenlatx*quarter+0.75_r_kind
  else
     efact   = one
  endif

  efactmc = one
  vfactmc = one
  dsval=0.80_r_kind
  if(sea)then
     dsval=((2.41_r_kind-0.0098_r_kind*tb_obsbc1)*tbc(ich238) + &
            0.454_r_kind*tbc(ich314)-tbc(ich890))*w1f6
     dsval=max(zero,dsval)
     clwx=cosza*clw*w1f4
  else
     clwx=0.6_r_kind
  end if

! QC6 in statsrad
  if(clwx >= one .and. luse)aivals(13,is) = aivals(13,is) + one
  factch4=clwx**2+(tbc(ich528)*w2f4)**2
! QC7 in statsrad
  if(dsval >= one .and. luse)aivals(14,is) = aivals(14,is) + one
  factch6=dsval**2+(tbc(ich544)*w2f6)**2

! For this conservative initial implementation of ATMS, we will not
! use surface channels over
! a) Mixed surfaces (to minimise and possible issues with re-mapping the FOVs)
! b) Snow and Ice (as the empirical model for these surfaces in CRTM is not 
!                  available for ATMS).
  latms_surfaceqc = (latms .AND. .NOT.(sea .OR. land))


! If window channels are missing, skip the following QC and do not
! assimilate channels 1-6 & 15.

  if (any(abs(tbc((/ ich238, ich314, ich528, ich536, ich544, ich890 /))) &
       > 200.0_r_kind))  then

          errf(1:ich544)=zero
          varinv(1:ich544)=zero
          do i=1,ich544
             if(id_qc(i) == igood_qc)id_qc(i) = ifail_interchan_qc
          end do
          errf(ich890)=zero
          varinv(ich890)=zero
          if(id_qc(ich890) == igood_qc) id_qc(ich890) = ifail_interchan_qc 

          if (latms) then 
             errf(16:22)=zero
             varinv(16:22)=zero
             do i=16,22
                if(id_qc(i) == igood_qc)id_qc(i) = ifail_interchan_qc
             end do
          end if  

  else

! QC for all-sky condition
     if (radmod%lcloud_fwd) then  ! all-sky check
        qc4emiss=.false.
        if(.not. sea) then  
           if(factch6 >= one .or. latms_surfaceqc) then   
              efactmc=zero
              vfactmc=zero
              errf(1:ich544)=zero
              varinv(1:ich544)=zero
              do i=1,ich544
                 if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch6_qc
              end do
              if(id_qc(ich890) == igood_qc)id_qc(ich890)=ifail_factch6_qc
              errf(ich890) = zero
              varinv(ich890) = zero
              if (latms) then
                 do i=17,22   !  AMSU-B/MHS like channels 
                    if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch6_qc
                    errf(i) = zero
                    varinv(i) = zero
                 enddo
              endif
!       QC3 in statsrad
              if(.not. mixed.and. luse)aivals(10,is) = aivals(10,is) + one
              
           else if(factch4 > half) then  
              efactmc=zero
              vfactmc=zero
              do i=1,ich536
                 if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch4_qc
                 varinv(i) = zero
                 errf(i) = zero
              end do
              if(id_qc(ich890) == igood_qc)id_qc(ich890)=ifail_factch4_qc
              errf(ich890) = zero
              varinv(ich890) = zero
              if (latms) then
                 do i=17,22   !  AMSU-B/MHS like channels 
                    if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch4_qc
                    errf(i) = zero
                    varinv(i) = zero
                 enddo
              endif
!         QC1 in statsrad
              if(luse) aivals(8,is) = aivals(8,is) + one
              
           else ! QC based on the sensitivity of Tb to the surface emissivity
              thrd1=0.020_r_kind
              thrd2=0.015_r_kind
              thrd3=0.035_r_kind
              thrd15=0.015_r_kind
              dtde1 = emissivity_k(ich238)
              de1   = zero
              if (dtde1 /= zero) de1=abs(tbc(ich238))/dtde1
              dtde2 = emissivity_k(ich314)
              de2   = zero
              if (dtde2 /= zero) de2=abs(tbc(ich314))/dtde2
              dtde3 = emissivity_k(ich503)
              de3   = zero
              if (dtde3 /= zero) de3=abs(tbc(ich503))/dtde3
              dtde15= emissivity_k(ich890)
              de15  = zero
              if (dtde15 /= zero) de15=abs(tbc(ich890))/dtde15
              qc4emiss= de2>thrd2 .or. de3>thrd3 .or. de1>thrd1 .or. de15>thrd15
           end if
           
        else  !QC for data over open water
!       calcalculate scattering index
!       screen out channels 1 to 6, and 15 if channel 6 is affected by precipitation
           ! for precipitating clouds
           if(radmod%lprecip) then
              if (cldeff_obs(ich536) < -0.50_r_kind .or. cldeff_fg(ich536) < -0.5_r_kind) then
                 efactmc=zero
                 vfactmc=zero
                 errf(1:ich544)=zero
                 varinv(1:ich544)=zero
                 do i=1,ich544
                    if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch5_qc
                 end do
                 if(id_qc(ich890) == igood_qc)id_qc(ich890)=ifail_factch5_qc
                 errf(ich890) = zero
                 varinv(ich890) = zero
                 if (latms) then
                    do i=17,22   !  AMSU-B/MHS like channels
                       if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch5_qc
                       errf(i) = zero
                       varinv(i) = zero
                    enddo
                 endif
              else if (latms) then
                 if (abs(cldeff_obs(16)-cldeff_obs(17))>10.0_r_kind) then
                    if(id_qc(ich890) == igood_qc)id_qc(ich890)=ifail_factch1617_qc
                    errf(ich890) = zero
                    varinv(ich890) = zero
                    do i=17,22   !  AMSU-B/MHS like channels
                       if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch1617_qc
                       errf(i) = zero
                       varinv(i) = zero
                    enddo
                    if (abs(cldeff_obs(16)-cldeff_obs(17))>15.0_r_kind) then
                       efactmc=zero
                       vfactmc=zero
                       errf(1:ich544)=zero
                       varinv(1:ich544)=zero
                       do i=1,ich544
                          if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch1617_qc
                       end do
                    end if
                 end if
                ! test in the future
                ! if (si_mean >= 20.0_r_kind) then
                !    efactmc=zero
                !    vfactmc=zero
                !    if(id_qc(ich890) == igood_qc)id_qc(ich890)=ifail_factch1617_qc
                !    errf(ich890) = zero
                !    varinv(ich890) = zero
                !    do i=17,22   !  AMSU-B/MHS like channels
                !       if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch1617_qc
                !       errf(i) = zero
                !       varinv(i) = zero
                !    enddo
                !    errf(1:ich544)=zero
                !    varinv(1:ich544)=zero
                !    do i=1,ich544
                !       if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch1617_qc
                !    end do
                ! end if
              else ! QC based on the sensitivity of Tb to the surface emissivity
!             de1,de2,de3,de15 become smaller as the observation is more cloudy --
!             i.e., less affected by the surface emissivity quality control check 
                 thrd1=0.025_r_kind
                 thrd2=0.015_r_kind
                 thrd3=0.030_r_kind
                 thrd15=0.030_r_kind
                 dtde1 = emissivity_k(ich238)
                 de1   = zero
                 if (dtde1 /= zero) de1=abs(tbc(ich238))/dtde1*(errf0(ich238)/errf(ich238))*(one-max(one,10.0_r_kind*clwp_amsua))
                 dtde2 = emissivity_k(ich314)
                 de2   = zero
                 if (dtde2 /= zero) de2=abs(tbc(ich314))/dtde2*(errf0(ich314)/errf(ich314))*(one-max(one,10.0_r_kind*clwp_amsua))
                 dtde3 = emissivity_k(ich503)
                 de3   = zero
                 if (dtde3 /= zero) de3=abs(tbc(ich503))/dtde3*(errf0(ich503)/errf(ich503))*(one-max(one,10.0_r_kind*clwp_amsua))
                 dtde15= emissivity_k(ich890)
                 de15  = zero
                 if (dtde15 /= zero) de15=abs(tbc(ich890))/dtde15*(errf0(ich890)/errf(ich890))*(one-max(one,10.0_r_kind*clwp_amsua))
                 qc4emiss= de2>thrd2 .or. de3>thrd3 .or. de1>thrd1 .or. de15>thrd15
              end if
           else
              ! for non-precipitation clouds
              if(factch6 >= one)then
                 efactmc=zero
                 vfactmc=zero
                 errf(1:ich544)=zero
                 varinv(1:ich544)=zero
                 do i=1,ich544
                    if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch6_qc
                 end do
                 if(id_qc(ich890) == igood_qc)id_qc(ich890)=ifail_factch6_qc
                 errf(ich890) = zero
                 varinv(ich890) = zero
                 if (latms) then
                    do i=17,22   !  AMSU-B/MHS like channels
                       if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch6_qc
                       errf(i) = zero
                       varinv(i) = zero
                    enddo
                 endif
                ! QC3 in statsrad
                 if(.not. mixed.and. luse)aivals(10,is) = aivals(10,is) + one
              else if (cldeff_obs(ich536) < -0.50_r_kind) then
                 efactmc=zero
                 vfactmc=zero
                 errf(1:ich544)=zero
                 varinv(1:ich544)=zero
                 do i=1,ich544
                    if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch5_qc
                 end do
                 if(id_qc(ich890) == igood_qc)id_qc(ich890)=ifail_factch5_qc
                 errf(ich890) = zero
                 varinv(ich890) = zero
                 if (latms) then
                    do i=17,22   !  AMSU-B/MHS like channels
                       if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch5_qc
                       errf(i) = zero
                       varinv(i) = zero
                    enddo
                 endif
              else if (latms) then
                 if (abs(cldeff_obs(16)-cldeff_obs(17))>10.0_r_kind) then
                    if(id_qc(ich890) == igood_qc)id_qc(ich890)=ifail_factch1617_qc
                    errf(ich890) = zero
                    varinv(ich890) = zero
                    do i=17,22   !  AMSU-B/MHS like channels
                       if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch1617_qc
                       errf(i) = zero
                       varinv(i) = zero
                    enddo
                    if (abs(cldeff_obs(16)-cldeff_obs(17))>15.0_r_kind) then
                       efactmc=zero
                       vfactmc=zero
                       errf(1:ich544)=zero
                       varinv(1:ich544)=zero
                       do i=1,ich544
                          if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch1617_qc
                       end do
                    end if
                 end if
              else ! QC based on the sensitivity of Tb to the surface emissivity
   !          de1,de2,de3,de15 become smaller as the observation is more cloudy --
   !          i.e., less affected by the surface emissivity quality control check 
                 thrd1=0.025_r_kind
                 thrd2=0.015_r_kind
                 thrd3=0.030_r_kind
                 thrd15=0.030_r_kind
                 dtde1 = emissivity_k(ich238)
                 de1   = zero
                 if (dtde1 /= zero) de1=abs(tbc(ich238))/dtde1*(errf0(ich238)/errf(ich238))*(one-max(one,10.0_r_kind*clwp_amsua))
                 dtde2 = emissivity_k(ich314)
                 de2   = zero
                 if (dtde2 /= zero) de2=abs(tbc(ich314))/dtde2*(errf0(ich314)/errf(ich314))*(one-max(one,10.0_r_kind*clwp_amsua))
                 dtde3 = emissivity_k(ich503)
                 de3   = zero
                 if (dtde3 /= zero) de3=abs(tbc(ich503))/dtde3*(errf0(ich503)/errf(ich503))*(one-max(one,10.0_r_kind*clwp_amsua))
                 dtde15= emissivity_k(ich890)
                 de15  = zero
                 if (dtde15 /= zero) de15=abs(tbc(ich890))/dtde15*(errf0(ich890)/errf(ich890))*(one-max(one,10.0_r_kind*clwp_amsua))
                 qc4emiss= de2>thrd2 .or. de3>thrd3 .or. de1>thrd1 .or. de15>thrd15
              endif
           endif ! radmod%lprecip 
        endif  ! if sea
! QC for clear condition
     else  ! <lcw4crtm>
        qc4emiss=.false.
        if(factch6 >= one .or. latms_surfaceqc)then
           efactmc=zero
           vfactmc=zero
           errf(1:ich544)=zero
           varinv(1:ich544)=zero
           do i=1,ich544
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch6_qc
           end do
           if(id_qc(ich890) == igood_qc)id_qc(ich890)=ifail_factch6_qc
           errf(ich890) = zero
           varinv(ich890) = zero
           if (latms) then
              do i=17,22   !  AMSU-B/MHS like channels 
                 if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch6_qc
                 errf(i) = zero
                 varinv(i) = zero
              enddo
           endif
!       QC3 in statsrad
           if(.not. mixed.and. luse)aivals(10,is) = aivals(10,is) + one
           
        else if(factch4 > half)then
           efactmc=zero
           vfactmc=zero
           do i=1,ich536
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch4_qc
              errf(i) = zero
              varinv(i) = zero
           end do
           if(id_qc(ich890) == igood_qc)id_qc(ich890)=ifail_factch4_qc
           errf(ich890) = zero
           varinv(ich890) = zero
           if (latms) then
              do i=17,22   !  AMSU-B/MHS like channels 
                 if(id_qc(i) == igood_qc)id_qc(i)=ifail_factch4_qc
                 errf(i) = zero
                 varinv(i) = zero
              enddo
           endif
!       QC1 in statsrad
           if(luse) aivals(8,is) = aivals(8,is) + one
           
        else
!       QC based on ratio of obs-ges increment versus the sensitivity of
!       the simulated brightness temperature to the surface emissivity
!       Y2K hurricane season runs by QingFu Liu found the hurricane
!       forecast tracks to be degraded without this QC.
!       (Is this still true?)
           
           if (sea .and. (.not.emiss_bc)) then
              thrd1=r0_05
              thrd2=r0_03
              thrd3=r0_05
           end if
           
           if (emiss_bc) then
              if (sea) then
                 thrd1=0.025_r_kind
                 thrd2=0.015_r_kind
                 thrd3=0.030_r_kind
                 thrd15=0.030_r_kind
              else
                 thrd1=0.020_r_kind
                 thrd2=0.015_r_kind
                 thrd3=0.035_r_kind
                 thrd15=0.015_r_kind
              end if
           end if
           
           if ((sea .and. (.not.emiss_bc)) .or. emiss_bc) then
              dtde1 = emissivity_k(ich238)
              de1   = zero
              if (dtde1 /= zero) de1=abs(tbc(ich238))/dtde1
              dtde2 = emissivity_k(ich314)
              de2   = zero
              if (dtde2 /= zero) de2=abs(tbc(ich314))/dtde2
              dtde3 = emissivity_k(ich503)
              de3   = zero
              if (dtde3 /= zero) de3=abs(tbc(ich503))/dtde3
              
              if (sea .and. (.not.emiss_bc)) then
                 qc4emiss = de2>thrd2 .or. de3>thrd3 .or. de1>thrd1
              end if
              
              if (emiss_bc) then
                 dtde15= emissivity_k(ich890)
                 de15  = zero
                 if (dtde15 /= zero) de15=abs(tbc(ich890))/dtde15
                 
                 qc4emiss= de2>thrd2 .or. de3>thrd3 .or. de1>thrd1 .or. de15>thrd15
              end if
           end if
        end if
     endif ! <lcw4crtm>  all-sky chk

     if (qc4emiss) then
! QC2 in statsrad
        if(luse)aivals(9,is) = aivals(9,is) + one
        efactmc=zero
        vfactmc=zero
        do i=1,ich536
           if(id_qc(i) == igood_qc)id_qc(i)=ifail_emiss_qc
           varinv(i) = zero
           errf(i) = zero
        end do
        if(id_qc(ich890) == igood_qc)id_qc(ich890)=ifail_emiss_qc
        errf(ich890) = zero
        varinv(ich890) = zero
        if (latms) then
           do i=17,22   !  AMSU-B/MHS like channels 
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_emiss_qc
              errf(i) = zero
              varinv(i) = zero
           enddo
        endif
     end if

  end if 

! Apply to both clear and all-sky condition
! Reduce q.c. bounds over higher topography
  if (zsges > r2000) then
     !    QC4 in statsrad
     if(luse)aivals(11,is) = aivals(11,is) + one
     fact                  = r2000/zsges
     efactmc               = fact*efactmc
     errf(ich544)          = fact*errf(ich544)
     vfactmc               = fact*vfactmc
     varinv(ich544)        = fact*varinv(ich544)
     if (latms) then
        do i=17,22   !  AMSU-B/MHS like channels 
           varinv(i)        = fact*varinv(i)
           errf(i)          = fact*errf(i)
        enddo
     endif
     if (zsges > r4000) then
!       QC5 in statsrad
        if(luse)aivals(12,is) = aivals(12,is) + one
        fact                  = r4000/zsges
        errf(ich549)          = fact*errf(ich549)
        varinv(ich549)        = fact*varinv(ich549)
     end if
  end if

! Generate q.c. bounds and modified variances.
  do i=1,nchanl
!    Modify error based on transmittance at top of model
     varinv(i)=varinv(i)*ptau5(nsig,i)
     errf(i)=errf(i)*ptau5(nsig,i)

     if(varinv(i) > tiny_r_kind)then
        dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term=dtbf*dtbf
        if(i <= ich536 .or. i == ich890)then
!          Adjust observation error based on magnitude of liquid
!          water correction.  0.2 is empirical factor
           term=term+0.2_r_kind*(predchan(3,i)*pred(3,i))**2

           errf(i)   = efactmc*errf(i)
           varinv(i) = vfactmc*varinv(i)
        end if
        errf(i)   = efact*errf(i)
        if (term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     end if
  end do

! Observation error adjustment for cloudy radiance based on mis-matched cloud, 
! diff_clw, scattering index, surface wind speed. The coefficient 13.0 for 
! clwtmp may be re-tuned with model physics changes. 
  eff_area=.false.
  if (radmod%lcloud_fwd) then
     eff_area=(radmod%cld_sea_only .and. sea) .or. (.not. radmod%cld_sea_only)
  end if
  if (radmod%lcloud_fwd .and. eff_area) then
     icol=one
     if (any(cld_rbc_idx==zero)) icol=zero
     do i=1,nchanl
        if(varinv(i)>tiny_r_kind .and. (i<=ich536 .or. i>=ich890))  then
           ework = (1.0_r_kind-icol)*abs(tbc(i))
           ework = ework+min(0.002_r_kind*sfc_speed**2*error0(i), 0.5_r_kind*error0(i))
           clwtmp=min(abs(clwp_amsua-clw_guess_retrieval), one)
           ework = ework+min(13.0_r_kind*clwtmp*error0(i), 3.5_r_kind*error0(i))
           if (scatp>9.0_r_kind) then
              ework = ework+min(1.5_r_kind*(scatp-9.0_r_kind)*error0(i), 2.5_r_kind*error0(i))
           end if
           ework=ework**2
           varinv(i)=varinv(i)/(one+varinv(i)*ework)
        endif
     end do
  endif

  return

end subroutine qc_amsua
subroutine qc_mhs(nchanl,ndat,nsig,is,sea,land,ice,snow,mhs,luse,   &
     zsges,tbc,tb_obs,ptau5,emissivity_k,ts,      &
     id_qc,aivals,errf,varinv,dsi,fact1)
!    id_qc,aivals,errf,varinv,dsi,fact1,radmod) ! all-sky

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_mhs    QC for amsub,mhs and hsb data
!
!   prgmmr: derber           org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for amsub, mhs and hsb data   
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     mhs          - logical, mhs flag - true if mhs data
!     luse         - logical use flag
!     zsges        - elevation of guess
!     tbc          - simulated - observed BT with bias correction
!     tb_obs       - observed BT 
!     ptau5        - transmittances as a function of level and channel
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     dsi          - scattering index quality control factor
!     fact1        - fact1 quality control parameter
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                            intent(in   ) :: sea,land,ice,snow,mhs,luse
  integer(i_kind),                    intent(in   ) :: ndat,nsig,nchanl,is
  integer(i_kind),dimension(nchanl),  intent(inout) :: id_qc
  real(r_kind),                       intent(in   ) :: zsges
  real(r_kind),                       intent(inout) :: dsi,fact1
  real(r_kind),dimension(40,ndat),    intent(inout) :: aivals
  real(r_kind),dimension(nchanl),     intent(in   ) :: tbc,tb_obs,emissivity_k,ts
  real(r_kind),dimension(nsig,nchanl),intent(in   ) :: ptau5
  real(r_kind),dimension(nchanl),     intent(inout) :: errf,varinv

! Declare local parameters

  real(r_kind) :: demisf,dtempf,efact,vfact,dtbf,term,fact
  integer(i_kind) :: i

  efact = one
  vfact = one
  if(sea)then
     demisf = 0.015_r_kind
     dtempf = half
  else if(land)then
     demisf = r0_03
     dtempf = two
  else if(ice)then
     demisf = r0_02  !decrease due to more accurate emiss model AMSU-A+B
     dtempf = one    !decrease due to more accurate emiss model AMSU-A+B
  else if(snow)then
     demisf = r0_02  !decrease due to more accurate emiss model AMSU-A+B
     dtempf = two    !decrease due to more accurate emiss model AMSU-A+B
  else
     demisf = quarter
     dtempf = five
  end if
!   For now increase for mhs since emissivity model not as good
  if(mhs .and. .not. sea) then
     demisf = three*demisf
     dtempf = three*dtempf
  end if
  if(sea .or. ice .or. snow)then
     dsi=9.0_r_kind
     if(tb_obs(2) < h300)then
        dsi=0.13_r_kind*(tbc(1)-33.58_r_kind*tbc(2)/(h300-tb_obs(2)))
!       QC3 in statsrad
        if(luse .and. dsi >= one)aivals(10,is) = aivals(10,is) + one
     end if
!    si=42.72_r_kind+0.85_r_kind*tbc(1)-tbc(2)
  else
     dsi=0.85_r_kind*tbc(1)-tbc(2)
!    si=42.72_r_kind+0.85_r_kind*tb_obs(1)-tb_obs(2)
!    QC4 in statsrad
     if(luse .and. dsi >= one)aivals(11,is) = aivals(11,is) + one
  end if
  dsi=max(zero,dsi)
  fact1=((tbc(1)-7.5_r_kind*dsi)/r10)**2+(dsi)**2

  if(fact1 > one)then
     vfact=zero
!    QC1 in statsrad
     if(luse)aivals(8,is) = aivals(8,is) + one
     do i=1,nchanl
        if(id_qc(i) == igood_qc)id_qc(i)=ifail_fact1_qc
     end do
  else
     if (mhs) then  ! wv sounding channels
        do i=3,nchanl
           if (abs(tbc(i)) >= two) then
              varinv(i) = zero
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_gross_routine_qc
           end if
        end do
     end if
     efact = (one-fact1*fact1)*efact
     vfact = (one-fact1*fact1)*vfact
!    Reduce q.c. bounds over higher topography
     if (zsges > r2000) then
!       QC2 in statsrad
        if(luse)aivals(9,is) = aivals(9,is) + one
        fact = r2000/zsges
        efact = fact*efact
        vfact = fact*vfact
     end if
  end if

! Generate q.c. bounds and modified variances.
  do i=1,nchanl
!    Modify error based on transmittance at top of model
     varinv(i)=vfact*varinv(i)*ptau5(nsig,i)
     errf(i)=efact*errf(i)*ptau5(nsig,i)
     if(varinv(i)>tiny_r_kind)then
        dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term=dtbf*dtbf
        if(term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     end if
  end do


  return

end subroutine qc_mhs
subroutine qc_atms(nchanl,is,ndat,nsig,npred,sea,land,ice,snow,mixed,luse,   &
                 zsges,cenlat,tb_obsbc1,cosza,clw,tbc,ptau5,emissivity_k,ts, &                    
                 pred,predchan,id_qc,aivals,errf,errf0,clwp_amsua,varinv,cldeff_obs,cldeff_fg,factch6, &
                 cld_rbc_idx,sfc_speed,error0,clw_guess_retrieval,scatp,radmod)                     

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_atms      QC for atms data
!
!   prgmmr: mccarty           org: gmao            date: 2011-05-17
!
! abstract: set quality control criteria for ATMS data            
!
! program history log:
!     2011-05-17  mccarty - added as QC algorithm for ATMS data
!     2011-05-26  todling - update argumenent list and call within
!     2014-04-27  eliu    - add two precipitation screenings; modify interface           
!     2015-01-15  zhu     - apply emissivity sensitivity screening to all-sky radiance
!     2015-03-31  zhu     - observation error adjustments based on mis-matched
!                           cloud info, diff_clw, scattering and surface wind
!                           speed for AMSUA/ATMS cloudy radiance assimilation
!     2015-09-20  zhu     - use rad_obs_type/radmod for enabling all-sky radiance
!     2016-10-13  zhu     - change cldeff_obs5 to cldeff_obs in interface for
!                           non-precipitaing cloudy radiance assimilation
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     ndat         - total number of observations types to process
!     nsig         - number of model levels
!     npred        - number of predictors
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     mixed        - logical, mixed flag
!     luse         - logical use flag
!     zsges        - elevation of guess
!     tb_obsbc1    - bias corrected ob for channel 1
!     cosza        - cosine of the satellite zenith angle
!     clw          - cloud liquid water estimate
!     tbc          - simulated - observed BT with bias correction
!     ptau5        - transmittances as a function of level and channel
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     pred         - bias correction predictors
!     predchan     - bias correction coefficients
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     cldeff_obs   - observed cloud effect 
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     factch6      - precipitation screening using channel 6 
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                             intent(in   ) :: sea,land,ice,snow,mixed,luse
  integer(i_kind),                     intent(in   ) :: nchanl,is,ndat,nsig,npred
  integer(i_kind),dimension(nchanl),   intent(inout) :: id_qc
  real(r_kind),                        intent(in   ) :: zsges,cenlat,tb_obsbc1
  real(r_kind),dimension(nchanl),      intent(in   ) :: cldeff_obs, cldeff_fg  
  real(r_kind),                        intent(in   ) :: cosza,clw,clwp_amsua,clw_guess_retrieval
  real(r_kind),                        intent(in   ) :: sfc_speed,scatp
  real(r_kind),                        intent(inout) :: factch6 
  real(r_kind),dimension(40,ndat),     intent(inout) :: aivals
  real(r_kind),dimension(nchanl),      intent(in   ) :: tbc,emissivity_k,ts 
  real(r_kind),dimension(nsig,nchanl), intent(in   ) :: ptau5
  real(r_kind),dimension(npred,nchanl),intent(in   ) :: pred,predchan
  real(r_kind),dimension(nchanl),      intent(inout) :: errf,errf0,varinv
  real(r_kind),dimension(nchanl),      intent(in   ) :: error0
  real(r_kind),dimension(nchanl),      intent(in   ) :: cld_rbc_idx
  type(rad_obs_type),                  intent(in   ) :: radmod

! For now, just pass all channels to qc_amsua
  call qc_amsua (nchanl,is,ndat,nsig,npred,sea,land,ice,snow,mixed,luse,   &
                 zsges,cenlat,tb_obsbc1,cosza,clw,tbc,ptau5,emissivity_k,ts, &                   
                 pred,predchan,id_qc,aivals,errf,errf0,clwp_amsua,varinv,cldeff_obs,cldeff_fg,factch6, & 
                 cld_rbc_idx,sfc_speed,error0,clw_guess_retrieval,scatp,radmod)                    

  return

end subroutine qc_atms
subroutine qc_ssu(nchanl,is,ndat,nsig,sea,land,ice,snow,luse,   &
     zsges,cenlat,tb_obs,ptau5,emissivity_k,ts,      &
     id_qc,aivals,errf,varinv)

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_ssu    QC for ssu data
!
!   prgmmr: H. Liu        org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for ssu data               
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     luse         - logical use flag
!     zsges        - elevation of guess
!     cenlat       - latitude of observation
!     tb_obs       - observed BT 
!     ptau5        - transmittances as a function of level and channel
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                            intent(in   ) :: sea,land,ice,snow,luse
  integer(i_kind),                    intent(in   ) :: ndat,nsig,nchanl,is
  integer(i_kind),dimension(nchanl),  intent(inout) :: id_qc
  real(r_kind),                       intent(in   ) :: zsges,cenlat
  real(r_kind),dimension(40,ndat),    intent(inout) :: aivals
  real(r_kind),dimension(nsig,nchanl),intent(in   ) :: ptau5
  real(r_kind),dimension(nchanl),     intent(in   ) :: tb_obs,emissivity_k,ts
  real(r_kind),dimension(nchanl),     intent(inout) :: errf,varinv

! Declare local parameters


  real(r_kind) :: demisf,dtempf,efact,dtbf,term,sfchgtfact,cenlatx
  integer(i_kind) :: i

  if(sea)then
     demisf = r0_01*half
     dtempf = half*half
  else if(land)then
     demisf = r0_02*half
     dtempf = two*half
  else if(ice)then
     demisf = r0_02*half
     dtempf = three*half
  else if(snow)then
     demisf = r0_02*half
     dtempf = three*half
  else
     demisf = r0_02*half
     dtempf = five*half
  end if

! Reduce weight for obs over higher topography
  sfchgtfact=one
  if (zsges > r2000) then
     sfchgtfact    = (r2000/zsges)**4
     if(luse) aivals(11,is)= aivals(11,is) + one
  endif

  do i=1,nchanl

     if (tb_obs(i) > r400 .or. tb_obs(i) <= r100) then
        varinv(i)=zero
        if(luse) aivals(12,is)= aivals(12,is) + one
        if(id_qc(i) == igood_qc)id_qc(i)=ifail_range_qc
     endif
     varinv(i) = varinv(i)*(one-(one-sfchgtfact)*ptau5(1,i))
!    Modify error based on transmittance at top of model
     varinv(i)=varinv(i)*ptau5(nsig,i)
     errf(i)=errf(i)*ptau5(nsig,i)
  end do

! Reduce qc bounds in tropics
  cenlatx=abs(cenlat)*r0_04     
  if (cenlatx < one) then
     if(luse)aivals(6,is) = aivals(6,is) + one
     efact = half*(cenlatx+one)
     do i=1,nchanl
        if(varinv(i) > tiny_r_kind)errf(i)=efact*errf(i)
     end do
  endif

! Generate q.c. bounds and modified variances.
  do i=1,nchanl
     if(varinv(i) > tiny_r_kind)then
        dtbf = demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term = dtbf*dtbf
        if(term > tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     end if
  end do

  return

end subroutine qc_ssu
subroutine qc_msu(nchanl,is,ndat,nsig,sea,land,ice,snow,luse,   &
     zsges,cenlat,tbc,ptau5,emissivity_k,ts,      &
     id_qc,aivals,errf,varinv)

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_msu    QC for msu data
!
!   prgmmr: derber           org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for msu data               
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!
! input argument list:
!     nchanl       - number of channels per obs
!     is           - integer counter for number of observation types to process
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     luse         - logical use flag
!     zsges        - elevation of guess
!     tbc          - simulated - observed BT with bias correction
!     ptau5        - transmittances as a function of level and channel
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                            intent(in   ) :: sea,land,ice,snow,luse
  integer(i_kind),                    intent(in   ) :: nchanl,ndat,nsig,is
  integer(i_kind),dimension(nchanl),  intent(inout) :: id_qc
  real(r_kind),                       intent(in   ) :: zsges,cenlat
  real(r_kind),dimension(40,ndat),    intent(inout) :: aivals
  real(r_kind),dimension(nchanl),     intent(in   ) :: tbc,emissivity_k,ts
  real(r_kind),dimension(nsig,nchanl),intent(in   ) :: ptau5
  real(r_kind),dimension(nchanl),     intent(inout) :: errf,varinv

! Declare local parameters

  real(r_kind) :: demisf,dtempf,efact,vfact,dtbf,term,cenlatx,fact
  integer(i_kind) :: i

  vfact = one

! Reduce qc bounds in tropics
  cenlatx=abs(cenlat)*r0_04     
  if (cenlatx < one) then
     if(luse)aivals(6,is) = aivals(6,is) + one
     efact   = half*(cenlatx+one)
  else
     efact   = one
  endif
  if(sea)then
     demisf = 0.015_r_kind
     dtempf = half
  else if(land)then
     demisf = r0_03
     dtempf = 2.5_r_kind
  else if(ice)then
     demisf = r0_05
     dtempf = three
  else if(snow)then
     demisf = r0_05
     dtempf = three
  else
     demisf = 0.20_r_kind
     dtempf = 4.5_r_kind
  end if

! Apply window test to channel 2 using channel 1
  if (abs(tbc(1)) > five) then
     errf(2) = zero
     varinv(2) = zero
     if(id_qc(2) == igood_qc)id_qc(2)=ifail_gross_routine_qc
!    QC1 in statsrad
     if(luse)aivals(8,is)   = aivals(8,is) + one
  endif

! Reduce q.c. bounds over higher topography
  if (zsges > r2000) then
!    QC2 in statsrad
     if(luse)aivals(9,is)   = aivals(9,is) + one
     fact = r2000/zsges
     errf(1) = fact*errf(1)
     errf(2) = fact*errf(2)
     errf(3) = fact*errf(3)
     vfact = fact
  end if



! Generate q.c. bounds and modified variances.
  errf(3) = two*errf(3)
  errf(4) = two*errf(4)
  do i=1,nchanl

!    Modify error based on transmittance at top of model
     varinv(i)=vfact*varinv(i)*ptau5(nsig,i)
     errf(i)=efact*errf(i)*ptau5(nsig,i)

     if(varinv(i) > tiny_r_kind)then
        dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term = dtbf*dtbf
        if (term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     end if
  end do

  return

end subroutine qc_msu

subroutine qc_goesimg(nchanl,is,ndat,nsig,ich,dplat,sea,land,ice,snow,luse,   &
     zsges,cld,tzbgr,tb_obs,tb_obs_sdv,tbc,tnoise,temp,wmix,emissivity_k,ts,      &
     id_qc,aivals,errf,varinv)
!    id_qc,aivals,errf,varinv,radmod) ! all-sky

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_goesimg   QC for seviri data
!
!   prgmmr: H. Liu           org: np23            date: 2010-08-20
!
! abstract: set quality control criteria for seviri data               
!
! program history log:
!     2010-08-10  derber transfered from setuprad
!
! input argument list:
!     nchanl       - number of channels per obs
!     ich          - channel number
!     is           - integer counter for number of observation types to process
!     dplat        - satellite identifier
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     luse         - logical use flag
!     zsges        - elevation of guess
!     cld          - cloud percentage within averaging box
!     tzbgr        - surface temperature of FOV
!     tb_obs       - observed BT within averaging box
!     tb_obs_sdv   - observed BT standard deviation within averaging box
!     tbc          - bias corrected (observed - simulated brightness temperatures)
!     tnoise       - error of observed radiance
!     temp         - temperature sensitivity array
!     wmix         - moisture sensitivity array
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                          intent(in   ) :: sea,land,ice,snow,luse
  integer(i_kind),                  intent(in   ) :: nchanl,ndat,nsig,is
  integer(i_kind),dimension(nchanl),intent(in   ) :: ich
  integer(i_kind),dimension(nchanl),intent(inout) :: id_qc
  real(r_kind),                     intent(in   ) :: zsges,cld,tzbgr
  real(r_kind),dimension(40,ndat),  intent(inout) :: aivals
  real(r_kind),dimension(nsig,nchanl),intent(in ) :: temp,wmix
  real(r_kind),dimension(nchanl),   intent(in   ) :: tb_obs,tb_obs_sdv,tbc,tnoise,emissivity_k,ts
  real(r_kind),dimension(nchanl),   intent(inout) :: errf,varinv
  character(len=*),                 intent(in   ) :: dplat

! Declare local parameters

  real(r_kind),parameter:: r40=40.0_r_kind
  real(r_kind),parameter:: r70=70.0_r_kind
  real(r_kind),parameter:: r0_3=0.3_r_kind
  real(r_kind),parameter:: r0_4=0.4_r_kind
  real(r_kind),parameter:: r0_6=0.6_r_kind
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r0_8=0.8_r_kind
  real(r_kind),parameter:: r0_9=0.9_r_kind
  real(r_kind),parameter:: r1_1=1.1_r_kind
  real(r_kind),parameter:: r1_3=1.3_r_kind
  real(r_kind),parameter:: r1_4=1.4_r_kind


  real(r_kind) :: demisf,dtempf,efact,vfact,dtbf,term,fact2,fact3,fact4,fact5
  real(r_kind) :: fact
  integer(i_kind) :: i
  integer(i_kind), dimension(nchanl) :: irday
  real(r_kind) :: dtz,ts_ave,xindx,tzchks

  irday = 1


  if(tb_obs(1) > zero .and. tb_obs(2) > zero .and. tb_obs(3) > zero .and. &
     tb_obs(4) > zero)then
     efact = one
     vfact = one
     fact2 = one
     fact3 = one
     fact4 = one
     fact5 = one
     if(sea)then
        demisf = r0_01
        dtempf = half
     else if(land)then
        do i=1,4
           if(i /= 2)then
              varinv(i)=zero
              if(id_qc(i) == igood_qc ) id_qc(i)=ifail_surface_qc                    
           end if
        end do
        demisf = r0_01
        dtempf = two
     else if(ice)then
        do i=1,4
           varinv(i)=zero
           if(id_qc(i) == igood_qc ) id_qc(i)=ifail_surface_qc                    
        end do
        demisf = r0_02
        dtempf = three
     else if(snow)then
        do i=1,4
           varinv(i)=zero
           if(id_qc(i) == igood_qc ) id_qc(i)=ifail_surface_qc                    
        end do
        demisf = r0_02
        dtempf = three
     else
        do i=1,4
           varinv(i)=zero
           if(id_qc(i) == igood_qc ) id_qc(i)=ifail_surface_qc                    
        end do
        demisf = r0_02
        dtempf = five
     end if

!    Filter out data according to clear sky fraction
     if(dplat == 'g10' .and. cld <r40 ) then
        do i=1,4
           varinv(i)=zero
           if(id_qc(i) == igood_qc ) id_qc(i)=ifail_cloud_qc                    
        end do
!       QC7 in statsrad
        if(luse)aivals(14,is)= aivals(14,is) + one
     else if(dplat == 'g12' .and. cld <r70 ) then
        do i=1,4
           varinv(i)=zero
           if(id_qc(i) == igood_qc ) id_qc(i)=ifail_cloud_qc                    
        end do
!       QC7 in statsrad
        if(luse)aivals(14,is)= aivals(14,is) + one
     end if

!    Quality control according to brightness temperature
!    standard deviation from data
     if(tb_obs_sdv(1) >one ) then
        varinv(1)=zero
!       QC3 in statsrad
        if(luse)aivals(10,is)= aivals(10,is) + one
        if(id_qc(1) == igood_qc ) id_qc(1)=ifail_std_goesimg_qc                    
     end if

     if(tb_obs_sdv(2) >1.5_r_kind ) then
        varinv(2)=zero
!       QC4 in statsrad
        if(luse)aivals(11,is)= aivals(11,is) + one
        if(id_qc(2) == igood_qc ) id_qc(2)=ifail_std_goesimg_qc                    
     end if

     if(tb_obs_sdv(3) >one ) then
        varinv(3)=zero
!       QC5 in statsrad
        if(luse)aivals(12,is)= aivals(12,is) + one
        if(id_qc(3) == igood_qc ) id_qc(3)=ifail_std_goesimg_qc                    
     end if

     if(tb_obs_sdv(4) >one ) then
        varinv(4)=zero
!       QC6 in statsrad
        if(luse)aivals(13,is)= aivals(13,is) + one
        if(id_qc(4) == igood_qc ) id_qc(4)=ifail_std_goesimg_qc                    
     end if

!    Reduce weight for obs over higher topography
     if (zsges > r2000) then
        fact    = r2000/zsges
        efact   = fact
        vfact   = fact*vfact
!       QC2 in statsrad
        if(luse)aivals(9,is)= aivals(9,is) + one
     end if
  else
     vfact=zero
  end if
!
!    Apply Tz retrieval
!
     if(tzr_qc > 0)then
        dtz = rmiss_single
        if ( sea ) then
           call tz_retrieval(nchanl,nsig,ich,irday,temp,wmix,tnoise,varinv,ts,tbc,tzbgr,1,0,dtz,ts_ave) 
        endif
!
!       Apply QC with Tz retrieval
!
        if (dtz /= rmiss_single ) then
          do i = 1, nchanl
            if ( varinv(i) > tiny_r_kind .and. iuse_rad(ich(i)) >= 1 .and. ts(i) > tschk ) then
              xindx = ((ts(i)-ts_ave)/(one-ts_ave))**3
              tzchks = tzchk*(half)**xindx

              if ( abs(dtz) > tzchks ) then
                 varinv(i) = zero
                 if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_tzr_qc
                 if(luse)aivals(13,is) = aivals(13,is) + one
              endif
            endif
          enddo
        endif
     endif

! Generate q.c. bounds and modified variances.
  do i=1,nchanl
     varinv(i) = vfact*varinv(i)
     if( dplat == 'g10' .and. i== 2) then
        if (tb_obs_sdv(2) >r0_3 .and. tb_obs_sdv(2) <=r0_6) &
           varinv(i)=varinv(i)/1.05_r_kind
        if (tb_obs_sdv(2) >r0_6 .and. tb_obs_sdv(2) <=r0_7) &
           varinv(i)=varinv(i)/1.15_r_kind
        if (tb_obs_sdv(2) >r0_7 .and. tb_obs_sdv(2) <=r0_8) &
           varinv(i)=varinv(i)/1.24_r_kind
        if (tb_obs_sdv(2) >r0_8 .and. tb_obs_sdv(2) <=r0_9) &
           varinv(i)=varinv(i)/1.28_r_kind
        if (tb_obs_sdv(2) >r0_9 .and. tb_obs_sdv(2) <=one)  &
           varinv(i)=varinv(i)/1.32_r_kind
        if (tb_obs_sdv(2) >one  .and. tb_obs_sdv(2) <=r1_1) &
           varinv(i)=varinv(i)/1.35_r_kind
        if (tb_obs_sdv(2) >r1_1 .and. tb_obs_sdv(2) <=r1_3) &
           varinv(i)=varinv(i)/1.39_r_kind
        if (tb_obs_sdv(2) >r1_4 )                           &     
           varinv(i)=varinv(i)/1.48_r_kind
     else if(dplat == 'g12' .and. i== 2) then
        if (tb_obs_sdv(2) >r0_4 .and. tb_obs_sdv(2) <=half) &
           varinv(i)=varinv(i)/1.05_r_kind
        if (tb_obs_sdv(2) >half .and. tb_obs_sdv(2) <=r0_6) &
           varinv(i)=varinv(i)/1.09_r_kind
        if (tb_obs_sdv(2) >r0_6 .and. tb_obs_sdv(2) <=r0_7) &
           varinv(i)=varinv(i)/1.14_r_kind
        if (tb_obs_sdv(2) >r0_7 .and. tb_obs_sdv(2) <=r0_8) &
           varinv(i)=varinv(i)/1.17_r_kind
        if (tb_obs_sdv(2) >r0_8 .and. tb_obs_sdv(2) <=r1_1) &
           varinv(i)=varinv(i)/1.19_r_kind
        if (tb_obs_sdv(2) >r1_1 .and. tb_obs_sdv(2) <=r1_3) &
           varinv(i)=varinv(i)/1.25_r_kind
        if (tb_obs_sdv(2) >r1_3 )                         &
           varinv(i)=varinv(i)/1.29_r_kind
     end if
     if(varinv(i)>tiny_r_kind)then
        errf(i)   = efact*errf(i)
        dtbf=demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term=dtbf*dtbf
        if (term>tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     endif
  end do

  return

end subroutine qc_goesimg

subroutine qc_geocsr(nchanl,is,ndat,nsig,ich,sea,land,ice,snow,luse,   &
     zsges,trop5,tzbgr,tsavg5,tb_obs_sdv,tbc,tb_obs,tnoise,ptau5,prsltmp,tvp,temp,wmix,emissivity_k,ts,      &
!    id_qc,aivals,errf,varinv,varinv_use,cld,cldp,kmax,zero_irjaco3_pole)
     id_qc,aivals,errf,varinv,varinv_use,cld,cldp,kmax,abi,ahi,seviri)

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  qc_geocsr       QC for CSR data from geostationary instruments ABI, AHI and SEVIRI
!
!   prgmmr: H.Liu           org: np23            date: 2018-05-20
!
! abstract: set quality control criteria for ABI data 
!
! program history log:
!     2018-05-20  H.Liu  initially added into qcmod
!
! input argument list:
!     nchanl       - number of channels per obs
!     ich          - channel number
!     is           - integer counter for number of observation types to process
!     sea          - logical, sea flag
!     land         - logical, land flag
!     ice          - logical, ice flag
!     snow         - logical, snow flag
!     luse         - logical use flag
!     zsges        - elevation of guess
!     trop5        - tropopause pressure
!     tzbgr        - Tz over water

!     tsavg5       - surface skin temperature
!     tbc          - simulated - observed BT with bias correction
!     tb_obs       - observed Brightness temperatures
!     tnoise       - channel noise array
!     ptau5        - transmittances as a function of level and channel


!     prsltmp      - array of layer pressures in vertical (surface to toa)
!     tvp          - array of temperatures in vertical (surface to toa)
!     temp         - temperature sensitivity array
!     wmix         - moisture sensitivity array
!     emissivity_k - surface emissivity sensitivity
!     ts           - skin temperature sensitivity
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     varinv_use   - observation weight used(modified obs var error inverse)
!
! output argument list:
!     id_qc        - qc index - see qcmod definition
!     aivals       - array holding sums for various statistics as a function of obs type
!     errf         - criteria of gross error
!     varinv       - observation weight (modified obs var error inverse)
!     varinv_use   - observation weight used(modified obs var error inverse)
!     cld          - cloud fraction
!     cldp         - cloud pressure
!     zero_irjaco3_pole - logical to control use of ozone jacobians near poles
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  implicit none

! Declare passed variables

  logical,                          intent(in   ) :: abi,ahi,seviri
  logical,                          intent(in   ) :: sea,land,ice,snow,luse
  integer(i_kind),                  intent(in   ) :: nchanl,ndat,nsig,is
  integer(i_kind),dimension(nchanl),intent(in   ) :: ich
  integer(i_kind),dimension(nchanl),intent(inout) :: id_qc
  integer(i_kind),dimension(nchanl),intent(in   ) :: kmax
  real(r_kind),                     intent(in   ) :: zsges
  real(r_kind),                     intent(in   ) :: tzbgr
  real(r_kind),dimension(40,ndat),  intent(inout) :: aivals
  real(r_kind),dimension(nchanl),   intent(in   ) :: tbc,tnoise,emissivity_k,ts
  real(r_kind),dimension(nsig,nchanl),intent(in ) :: temp,wmix
  real(r_kind),dimension(nchanl),   intent(inout) :: errf,varinv,varinv_use
  real(r_kind),                     intent(in   ) :: trop5,tsavg5
  real(r_kind),dimension(nchanl),     intent(in   ) :: tb_obs,tb_obs_sdv
  real(r_kind),dimension(nsig,nchanl),intent(in   ) :: ptau5
  real(r_kind),dimension(nsig),       intent(in   ) :: prsltmp,tvp
  real(r_kind),                       intent(  out) :: cld,cldp
! logical,                            intent(inout) :: zero_irjaco3_pole

! Declare local parameters
  real(r_kind) :: demisf,dtempf,sfchgtfact,term,dtbf,efact,vfact
  real(r_kind) :: sum,sum2,sum3,cloudp,tmp,dts,delta
  real(r_kind),dimension(nchanl) :: dtb
  integer(i_kind) :: i,j,k,kk,lcloud
  integer(i_kind), dimension(nchanl) :: irday
  real(r_kind) :: dtz,ts_ave,xindx,tzchks

  if(sea)then
     demisf = r0_01
     dtempf = half
  else if(land)then
     demisf = r0_02
     dtempf = two
  else if(ice)then
     demisf = r0_03
     dtempf = four
  else if(snow)then
     demisf = r0_02
     dtempf = two
  else
     demisf = r0_03
     dtempf = four
  end if

! Optionally turn off ozone jacabians near poles
! zero_irjaco3_pole=.false.
! if (qc_noirjaco3_pole .and. (abs(cenlat)>r60)) zero_irjaco3_pole=.true.

! Reduce weight for obs over higher topography
  sfchgtfact=one
  if (zsges > r2000) then
!    QC1 in statsrad
     if(luse)aivals(8,is) = aivals(8,is) + one
     sfchgtfact    = (r2000/zsges)**4
  endif

  do i=1,nchanl

!    use chn 2, 3 and 4 over both sea and land while other IR chns only over sea
     if (sea) then
        efact=one
        vfact=one
     else if (land ) then
        if ((abi .or. ahi) .and. (i == 2 .or. i ==3 .or. i==4) ) then
           efact=one
           vfact=one
        else if ((seviri) .and. (i == 2 .or. i ==3) ) then
           efact=one
           vfact=one
        else
           efact=zero
           vfact=zero
           if(id_qc(i) == igood_qc)id_qc(i)=ifail_surface_qc
        end if
     else
        efact=zero
        vfact=zero
        if(id_qc(i) == igood_qc)id_qc(i)=ifail_surface_qc
     end if
     if (seviri) then
!       QC_terrain: If seviri and terrain height > 1km. do not use
        if (zsges > r1000) then
           efact   = zero
           vfact   = zero
           if(id_qc(i) == igood_qc ) id_qc(i)=ifail_terrain_qc
!          QC2 in statsrad
           if(luse)aivals(9,is)= aivals(9,is) + one
        end if
     end if

!    modified variances.
     errf(i)   = efact*errf(i)
     varinv(i) = vfact*varinv(i)
     varinv_use(i) = vfact*varinv_use(i)
  end do

! Generate q.c. bounds and modified variances for height change and ptau5
  sum3=zero
  do i=1,nchanl
     if (tb_obs(i) > r1000 .or. tb_obs(i) <= zero) then
        varinv(i)=zero
        varinv_use(i)=zero
     end if
     tmp=one-(one-sfchgtfact)*ptau5(1,i)
     varinv(i) = varinv(i)*tmp
     varinv_use(i) = varinv_use(i)*tmp

!    Modify error based on transmittance at top of model
     varinv(i)=varinv(i)*ptau5(nsig,i)
     varinv_use(i)=varinv_use(i)*ptau5(nsig,i)
     errf(i)=errf(i)*ptau5(nsig,i)

!    QC based on presence/absence of cloud
     sum3=sum3+tbc(i)*tbc(i)*varinv_use(i)
  end do
  sum3=0.75_r_kind*sum3
  lcloud=0
  cld=zero
  cldp=r10*prsltmp(1)

   do k=1,nsig
     if(prsltmp(k) > trop5)then
        do i=1,nchanl
           dtb(i)=(tvp(k)-tsavg5)*ts(i)
        end do
        do kk=1,k-1
           do i=1,nchanl
              dtb(i)=dtb(i)+(tvp(k)-tvp(kk))*temp(kk,i)
           end do
        end do
        sum=zero
        sum2=zero
        do i=1,nchanl
           if(varinv_use(i) > tiny_r_kind)then
              sum=sum+tbc(i)*dtb(i)*varinv_use(i)
              sum2=sum2+dtb(i)*dtb(i)*varinv_use(i)
           end if
        end do
        if (abs(sum2) < tiny_r_kind) sum2 = sign(tiny_r_kind,sum2)
        cloudp=min(max(sum/sum2,zero),one)
        sum=zero
        do i=1,nchanl
           if(varinv_use(i) > tiny_r_kind)then
              tmp=tbc(i)-cloudp*dtb(i)
              sum=sum+tmp*tmp*varinv_use(i)
           end if
        end do
        if(sum < sum3)then
           sum3=sum
           lcloud=k
           cld=cloudp
           cldp=r10*prsltmp(k)
        end if
     end if

  end do
  if ( lcloud > 0 ) then  ! If cloud detected, reject channels affected by it.
     do i=1,nchanl

!       reject channels with iuse_rad(j)=-1 when they are peaking below the cloud
        j=ich(i)
        if (passive_bc .and. iuse_rad(j)==-1) then
           if (lcloud >= kmax(i)) then
              if(luse)aivals(11,is)   = aivals(11,is) + one
              varinv(i) = zero
              varinv_use(i) = zero
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_cloud_qc
              cycle
           end if
        end if

!       If more than 2% of the transmittance comes from the cloud layer,
!          reject the channel (0.02 is a tunable parameter)

        delta = 0.02_r_kind
        if ( ptau5(lcloud,i) > 0.02_r_kind) then
!          QC4 in statsrad
           if(luse)aivals(11,is)   = aivals(11,is) + one
           varinv(i) = zero
           varinv_use(i) = zero
           if(id_qc(i) == igood_qc)id_qc(i)=ifail_cloud_qc
        end if
     end do

!    If no clouds check surface temperature/emissivity
  else                 ! If no cloud was detected, do surface temp/emiss checks
     sum=zero
     sum2=zero
     do i=1,nchanl
        sum=sum+tbc(i)*ts(i)*varinv_use(i)
        sum2=sum2+ts(i)*ts(i)*varinv_use(i)
     end do
     if (abs(sum2) < tiny_r_kind) sum2 = sign(tiny_r_kind,sum2)
     dts=abs(sum/sum2)
     if(abs(dts) > one)then
        if(.not. sea)then
           dts=min(dtempf,dts)
        else
           dts=min(three,dts)
        end if
        do i=1,nchanl
           delta=max(r0_05*tnoise(i),r0_02)
           if(abs(dts*ts(i)) > delta)then
!             QC3 in statsrad
              if(luse .and. varinv(i) > zero) &
                 aivals(10,is)   = aivals(10,is) + one
              varinv(i) = zero
              if(id_qc(i) == igood_qc)id_qc(i)=ifail_sfcir_qc
           end if
        end do
     end if
  endif

  do i = 1, nchanl

!    Tighter qc for chn7.3: toss data for chn7.3 and surface chns if rclrsky<98% (done in setuprad) or stdev >= 0.5 for chn10.3
     if((abi .or. ahi) .and. tb_obs_sdv(7)>=0.5_r_kind .and. varinv(i) > zero)then
       if(i/=2 .and. i/=3) then
!         QC3 in statsrad
          if(luse)aivals(9,is)= aivals(9,is) + one
          if(id_qc(i) == igood_qc ) id_qc(i)=ifail_std_geocsr_qc                    
          varinv(i)=zero
       end if
     end if
     if(seviri .and. varinv(i) > zero) then
       if(tb_obs_sdv(6)>=0.5_r_kind)then
         if(i/=2) then
!           QC3 in statsrad
            if(luse)aivals(9,is)= aivals(9,is) + one
            if(id_qc(i) == igood_qc ) id_qc(i)=ifail_std_geocsr_qc                    
            varinv(i)=zero
         end if
       end if
!      QC_o-g: If abs(o-g) > 2.0 do not use
       if ( i/=2 .and. abs(tbc(i)) > two ) then
          varinv(i) = zero
          if(id_qc(i) == igood_qc ) id_qc(i)=ifail_gross_routine_qc
!         QC1 in statsrad
          if(luse)aivals(8,is)= aivals(8,is) + one  !hliu check
       end if
     end if

! adjust varinv according to the BT standard deviation
     if( abi .and. i== 2 .or. i==3 .or. i==4 .and. varinv(i) > zero) then
!       if (tb_obs_sdv(2) >0.3_r_kind .and. tb_obs_sdv(2) <=0.4_r_kind) &
        if (tb_obs_sdv(2) >0.4_r_kind .and. tb_obs_sdv(2) <=0.5_r_kind) &
           varinv(i)=varinv(i)/1.32_r_kind
!       if (tb_obs_sdv(2) >0.4_r_kind .and. tb_obs_sdv(2) <=0.6_r_kind) &
        if (tb_obs_sdv(2) >0.5_r_kind .and. tb_obs_sdv(2) <=0.6_r_kind) &
           varinv(i)=varinv(i)/1.67_r_kind
        if (tb_obs_sdv(2) >0.6_r_kind .and. tb_obs_sdv(2) <=0.7_r_kind) &
           varinv(i)=varinv(i)/2.24_r_kind
        if (tb_obs_sdv(2) >0.7_r_kind )  &
           varinv(i)=varinv(i)/2.31_r_kind
     end if
     if(seviri .or. ahi) then
       varinv(i)=varinv(i)
     end if
   
  end do
!
! Apply Tz retrieval
!
  if(tzr_qc > 0)then
     dtz = rmiss_single
     if ( sea ) then
        call tz_retrieval(nchanl,nsig,ich,irday,temp,wmix,tnoise,varinv,ts,tbc,tzbgr,1,0,dtz,ts_ave) 
     endif
!
! Apply QC with Tz retrieval
!
     if (dtz /= rmiss_single ) then
       do i = 1, nchanl
         if ( varinv(i) > tiny_r_kind .and. iuse_rad(ich(i)) >= 1 .and. ts(i) > tschk ) then
           xindx = ((ts(i)-ts_ave)/(one-ts_ave))**3
           tzchks = tzchk*(half)**xindx
   
           if ( abs(dtz) > tzchks ) then
              varinv(i) = zero
              if (  id_qc(i) == igood_qc ) id_qc(i) = ifail_tzr_qc
              if(luse)aivals(13,is) = aivals(13,is) + one
           endif
         endif
       enddo
     endif
  end if

! Generate q.c. bounds and modified variances.
  do i=1,nchanl
     if(varinv(i) > tiny_r_kind)then
        dtbf = demisf*abs(emissivity_k(i))+dtempf*abs(ts(i))
        term = dtbf*dtbf
        if(term > tiny_r_kind)varinv(i)=varinv(i)/(one+varinv(i)*term)
     end if
   end do
 

  return

end subroutine qc_geocsr


end module qcmod
