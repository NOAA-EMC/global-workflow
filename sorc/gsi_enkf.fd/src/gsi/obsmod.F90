module obsmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   obsmod
!   prgmmr: derber      org: np23                date: 2003-09-25
!
! abstract: This module contains variables and arrays pertinent for
!           observational data.
!
! program history log:
!   2003-09-25 derber
!   2004-05-13  kleist, documentation
!   2004-06-15  treadon  - update documentation
!   2004-07-23  derber   - add conventional sst observations
!   2004-11-22  derber   - remove weight, add logical for boundary point
!   2004-11-30  cucurull - add GPS RO data
!   2004-12-22  treadon  - rename logical "idiag_conv" as "diag_conv", 
!                          add write_diag logical, remove conv_diagsave 
!                          from module
!   2005-01-28  cucurull - clean up code handling gps local refractivity
!   2005-03-01  parrish  - nonlinear qc change to account for inflated obs error
!   2005-03-04  derber   - modify for cleaned up surface emissivity sensitivity
!   2005-03-16  derber   - add arrays to hold observation time
!   2005-05-27  derber   - add logical oberrflg
!   2005-06-14  wu       - add OMI oz (ozo)
!   2005-06-21  cucurull - add logical ref_obs (GPS), parameter grids_dim (GPS)
!                        - add GPS bending angle observations
!   2005-08-03  derber   - move var qc parameters b_ and pg_ for conventional 
!                          from qcmod to obsmod
!   2005-09-28  derber   - consolidate weights and locations for observations
!   2005-11-22  wu       - add variables and flag for perturbed conventional
!                          obs and a function routine to generate a random
!                          number with mean:0 variance:1
!   2005-11-29  derber   - move ozmz to guess_grids
!   2005-12-20  parrish  - add variables to enable boundary layer forward model
!                          option for surface temperature observations
!   2005-12-21  treadon  - add arrays and code for gps data
!   2006-02-03  derber   - add new obs control and simplify makecobs call
!   2006-02-17  treadon  - add stat check on all allocate and deallocate
!   2006-03-21  treadon  - modify optional perturbation to observations
!   2006-04-19  treadon  - add logical switch dtbduv_on
!   2006-05-05  treadon  - add maximum time window variable
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2006-09-20  cucurull - remove termt1in for GPS data, add gpstail%b_tkges 
!                          for GPS data, remove b_psges
!   2006-10-25  sienkiewicz - add blacklst flag
!   2007-01-10  sienkiewicz - define level ozone obs types
!   2007-02-02  tremolet - trying things
!   2007-02-13  parrish  - add logical switch offtime_data to allow use of obs files
!                          with ref time different from analysis time
!   2007-03-09        su - add observation perturbation paramters in the
!                          observation position structure
!   2007-03-19  tremolet - binning of observations
!   2007-05-03  todling  - add reference to o3l
!   2007-05-30  h.liu    - change wij to 2d array for oz_ob_type, ozo_ob_type
!   2007-05-31  tremolet - add observation diagnostics structure
!   2007-06-22  cucurull - modify gps_all_ob_type structure
!   2007-06-26  tremolet - observation sensitivity
!   2007-07-11  tremolet - increment sensitivity to obs
!   2007-07-26  cucurull - modify pressure structure in gps_ob_type  
!   2007-10-24  todling  - add nchnperobs to obsdiag
!   2007-11-12  todling  - add interface to destroyobs
!   2008-03-24        wu - add variables and logical switch for adaptive oberror tuning
!   2008-11-19  todling  - define offtime_data default differently when doing 4dvar
!   2009-01-08  todling  - remove reference to ozohead/tail-structure
!   2009-01-09  gayno    - add variable dsfcalc
!   2009-02-02  kleist   - add variables for synthetic tc-mslp observations
!   2009-03-05  meunier  - add lagrangean observation type
!   2009-07-09  park,purser,pondeca - add logical variable hilbert_curve for
!                                     cross-validation in 2dvar
!   2010-02-10  jing     - merge in obs key set (idv,iob,ich) in obs types for unique
!                          run-time identification (in sorting and searching).
!   2010-03-05  pondeca  - set ndat_times=1 for 2dvar mode
!   2010-03-24  tangborn - added carbon monoxide (co) observation type type 
!   2010-04-01  li       - add zob, tz_tr to sst_ob_type
!   2010-05-12  zhu      - add create_passive_obsmod_vars and destroyobs_passive
!   2010-05-26  treadon  - add tcpptr to public list 
!   2010-06-14  huang    - add aerosol variable (*aero*)
!   2010-07-10  todling  - turn aerosol heads/tails public
!   2010-08-18       hu  - add codiags to public declaration
!   2010-10-15 pagowski  - add pm2_5 in-situ
!   2010-10-20 hclin     - use 1d wij for aod in channels
!   2011-02-09      zhu  - add gust,visibility,and pbl height
!   2011-11-14  whitaker - set ndat_times = 1, when assimilation window is less than 6 hours
!   2011-11-14  wu       - add logical for extended forward model on rawinsonde data
!   2012-04-05  todling  - nullify ich in rad_ob_type and aero_ob_type; also dealloc them
!   2012-09-10  wargan   - add  OMI with efficiency factors
!   2013-05-19  zhu      - add pred and idx in t_ob_type for aircraft temperature bias correction
!   2013-09-27  todling - revisit handling of ob-instr/type (now in table of its own)
!   2014-01-31  guo      - removed redundant "type codiags", which is identical to "type odiags".
!                        - renamed odiags to aofp_obs_diag, "array-of-Fortran-pointers of obs_diag",
!                          for explicity.
!                        - removed type(aofp_obs_diag) from public entity list, which is not used
!                          anywhere else, except in this module.  It might be needed to be public
!                          in the future, but atleast not now.
!   2014-03-19  pondeca  - add wspd10m
!   2014-04-10  pondeca  - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca  - add howv
!   2014-06-16  carley/zhu - add tcamt and lcbas
!   2014-10-06  carley - add obs_sub_comm
!   2014-12-03  derber  - ensure obsdiag used for 4dvar and non-pcgsoi
!                         minimizations
!   2015-03-31  wgu      - add isis(sensor/instrument/satellite id) in rad_ob_type to handle
!                          instruments id in intrad inter-channel correlation implementation.
!   2015-07-10  pondeca  - add cloud ceiling height (cldch)
!   2015-08-18  wgu      - add isfctype - mask for surface type - to radiance ob type
!   2015-09-01  guo      - moved type::gps_all_ob_type, type::gps_all_ob_head,
!                          their instances (gps_allhead,gps_alltail), and
!                          related procedures into module m_gpsStats in
!                          file genstats_gps.f90.
!                        - moved variables (radheadm,radtailm) and related
!                          procedures into module m_prad in file prad_bias.f90.
!   2015-09-03  guo      - moved type::obs_handle, its instance yobs, and its
!                          allocation, into m_obsHeadBundle.F90.
!   2016-01-28  mccarty  - add netcdf_diag capability
!   2016-03-07  pondeca  - add uwnd10m,vwnd10m
!   2016-05-04  guo      - moved all ob_type and ob_head type-definitions into
!                          their *own* class-style-modules, including 9 recent
!                          new ob_types.
!                        - cleaned some debris left from this and previous changes.
!   2016-05-18  collard  - Added code to allow for historical naming conventions
!                          for satellite instruments
!   2015-03-31  wgu      - add isis(sensor/instrument/satellite id) in rad_ob_type to handle
!                          instruments id in intrad inter-channel correlation
!                          implementation.
!   2016-07-19  wgu      - add isfctype - mask for surface type - to radiance obtype
!   2016-07-19  kbathmann - add rsqrtinv and use_corr_obs to rad_ob_type
!   2016-07-26  guo      - moved away most cldch_ob_type contents to a new module, m_cldchNode
!   2016-08-20  guo      - moved (stpcnt,ll_jo,ib_jo) to stpjo.f90.
!   2016-09-19  guo      - moved function dfile_format() to m_extOzone.F90
!   2016-02-15 Johnson, Y. Wang, X. Wang - add dbz type for reflectivity DA.
!                                          POC: xuguang.wang@ou.edu
!   2016-11-29 shlyaeva  - add lobsdiag_forenkf option for writing out linearized
!                           H(x) for EnKF
!   2018-01-01  apodaca  - add GOES/GLM lightning observations
!   2019-05-28  guo     - moved all type-constants {i_xx_ob_type} as enumerators of
!                         (1) obsNode types to module m_obsNodeTypeManager.F90 (iobNode_xx); and
!                         (2) obOper Types to module gsi_obOperTypeManager.F90 (iobOper_xx).
!                         Note that a single type specification i_xx_ob_type is now split into two,
!                         one for obsNode types, and another for obOper types.
!                       - moved nobs_type to module gsi_obOperTypeManager.F90 (obOper_count).
!                       - moved cobstype(:) to module gsi_obOperTypeManager.F90.
!                       - moved type obs_diag, obs_diags, aofp_obs_diag, and variable obsdiags(:,:)
!                         with subroutine inquire_obsdiags() into m_obsdiagNode.F90.
!                       - moved obscounts(:) into obs_sensitivity.f90.
!   2019-06-25  Hu       - add diag_radardbz for controling radar reflectivity
!                               diag file
!   2019-07-29  pondeca - add logical variable neutral_stability_windfact_2dvar
!                         to turn on computation of 10m wind factor for near surface
!                         winds using a simple, similarity theory-based approach
!   2019-08-23  pondeca - add logical variable use_similarity_2dvar
!                         to turn on computation of 10m wind factor for near surface
!                         winds the mm5-based sfc model's similarity theory
!  01-27-2020 Winterbottom Moved regression coeffcients for regional
!                          model (e.g., HWRF) aircraft recon dynamic
!                          observation error (DOE) specification to
!                          GSI namelist level.  
!  2020-09-15  Wu        - add option tcp_posmatch to mitigate possibility of erroneous TC initialization
!  2020-09-19  CAPS(J. Park) - add 'vad_near_analtime' flag to assimilate newvad obs around analysis time only
!   2021-11-16 Zhao      - add option l_obsprvdiag (if true) to trigger the output of
!                          observation provider and sub-provider information into
!                          obsdiags files (used for AutoObsQC)
!  2022-03-15  K. Apodaca - add GNSS-R L2 ocean wind speed observations (CYGNSS, Spire) 
!   2023-07-10  Y. Wang, D. Dowell - add variables for flash extent density
!   2023-10-10  H. Wang (GSL) - add variables for flash extent density EnVar DA
! 
! Subroutines Included:
!   sub init_obsmod_dflts   - initialize obs related variables to default values
!   sub init_directories    - create sub-directories for tasks specific i/o
!   sub create_obsmod_vars  - allocate obs related variables
!   sub create_passive_obsmod_vars  - allocate monitored radiance obs related variables
!   sub init_obsmod_vars    - initialize obs related variables to actual values
!   sub destroyobs          - deallocate obs linked lists
!   sub destroyobs_passive  - deallocate monitored radiance obs linked lists
!   sub destroy_obsmod_vars - deallocate obsmod arrays
!   sub destroy_genstats_gps- deallocate linked list for gsp statistics
!   sub inquire_obsdiags
!
! Functions Included:
!   fun ran01dom
!
! Variable Definitions:
!   def oberror_tune - namelist logical to tune (=true) oberror
!   def perturb_obs  - namelist logical to perturb (=true) observations
!   def tcp_posmatch - namelist integer =1 to move TC to guess position,
!                                       =2 set pges to the minimum Psfc 
!   def tcp_box      - namelist integer (=5) to define search box size in gridpoints
!   def perturb_fact - namelist scaling factor for observation perturbations
!   def write_diag   - namelist logical array to compute/write (=true) diag files
!   def diag_radardbz- namelist logical to compute/write (=true) radar
!                                          reflectiivty diag files
!   def diag_fed     - namelist logical to compute/write (=true) flash extent density diag files
!   def innov_use_model_fed - namelist logical. True: use (the FEB in background to calculate innovation
!                                               False: calculate innvation use
!                                               the obs operator in GSI  
!   def if_model_fed - namelist logical. True: Read in FED from background
!                                              including from ensemble.  
!   def r_hgt_fed    - height of fed observations
!   def reduce_diag  - namelist logical to produce reduced radiance diagnostic files
!   def use_limit    - parameter set equal to -1 if diag files produced or 0 if not diag files or reduce_diag
!   def obs_setup    - prefix for files passing pe relative obs data to setup routines
!   def dsfcalc      - specifies method to determine surface fields within a FOV
!                      when equal to one, integrate model fields over FOV. 
!                      when not one, bilinearly interpolate model fields to FOV center.
!   def dfile        - input observation file names
!   def dsis         - sensor/instrument/satellite flag from info files
!   def dtype        - observation types
!   def ditype       - observation group type (set in read_obs, e.g. rad,conv,etc)
!   def time_window  - half time window for obs type (hours)
!   def time_window_max - maximum half time window (hours)
!   def time_window_rad - maximum half time window (hours) for cetain radiance
!   def obsfile_all  - file containing observations after initial read
!   def ndat_types   - number of available data types
!   def ndat_times   - number of available synoptic times
!   def ndat         - total number of data types
!   def ipoint       - pointer to input namelist for particular processor
!   def iadate       - analysis date and time array
!   def iadatemn     - similar to iadate, but with non-zero minute information
!   def ianldate     - analysis date in YYYYMMDDHH variable
!   def time_offset  - analysis relative time offset
!   def dplat        - satellite (platform) id
!   def dthin        - satellite group
!   def nsat1        - number of observations of satellites in each pe
!   def obs_sub_comm - mpi communicator for obs ob pe subdomains (one communicator per obtype)
!   def mype_diaghdr - pe id for writing out satellite diagnostic file
!   def dval         - relative value of each profile within group
!                      relative weight for observation = dval/sum(dval)
!                      within grid box
!   def dmesh        - mesh size (km) for radiance thinning grid (used in satthin)
!   def pshead       - surface pressure linked list head
!   def pstail       - surface pressure linked list tail
!   def thead        - temperature linked list head
!   def ttail        - temperature linked list tail
!   def dwhead       - doppler wind linked list head
!   def dwtail       - doppler wind linked list tail
!   def rwhead       - radial wind linked list head
!   def rwtail       - radial wind linked list tail
!   def whead        - conventional wind linked list head
!   def wtail        - conventional wind linked list tail
!   def qhead        - moisture linked list head
!   def qtail        - moisture linked list tail
!   def ssthead      - sea surface temperature linked list head
!   def ssttail      - sea surface temperature linked list tail
!   def gusthead     - wind gusts linked list head
!   def gusttail     - wind gusts linked list tail
!   def vishead      - visibility linked list head
!   def vistail      - visibility linked list tail
!   def pblhhead     - wind pblhs linked list head
!   def pblhtail     - wind pblhs linked list tail
!   def tcamthead    - total cloud amount linked list head
!   def tcamttail    - total cloud amount linked list tail
!   def lcbashead    - lowest cloud base linked list head
!   def lcbastail    - lowest cloud base linked list tail
!   def pwhead       - precipitable water linked list head
!   def pwtail       - precipitable water linked list tail
!   def ozhead       - sbuv ozone profile linked list head
!   def oztail       - sbuv ozone profile linked list tail
!   def o3lhead      - ozone level data linked list head
!   def o3ltail      - ozone level data linked list tail
!   def colvkhead    - carbon monoxide level data linked list head 
!   def colvktail    - carbon monoxide level data linked list tail 
!   def aerohead     - aerosol profile linked list head
!   def aerotail     - aerosol profile linked list tail
!   def aerolhead    - aerosol level data linked list head
!   def aeroltail    - aerosol level data linked list tail
!   def pm2_5head    - pm2_5 level data linked list head
!   def pm2_5tail    - pm2_5 level data linked list tail
!   def pm10head     - pm10 level data linked list head
!   def pm10tail     - pm10 level data linked list tail
!   def radhead      - radiance linked list head
!   def radtail      - radiance linked list tail
!   def radheadm     - radiance linked list head for monitored radiance data
!   def radtailm     - radiance linked list tail for monitored radiance data
!   def pcphead      - precipitation linked list head
!   def pcptail      - precipitation linked list tail
!   def laghead      - lagrangian data linked list head
!   def lagtail      - lagrangian data linked list tail
!   def wspd10mhead  - 10-wind speed linked list head
!   def wspd10mtail  - 10-wind speed linked list tail
!   def uwnd10mhead  - 10m-uwind linked list head
!   def uwnd10mtail  - 10m-uwind linked list tail
!   def vwnd10mhead  - 10m-uwind linked list head
!   def vwnd10mtail  - 10m-uwind linked list tail
!   def td2mhead     - 2m dew point linked list head
!   def td2mtail     - 2m dew point linked list tail
!   def mxtmhead     - daily maximum temperature linked list head
!   def mxtmtail     - daily maximum temperature linked list tail
!   def mitmhead     - daily minimum temperature linked list head
!   def mitmtail     - daily minimum temperature linked list tail
!   def pmslhead     - pressure at mean sea level linked list head
!   def pmsltail     - pressure at mean sea level linked list tail
!   def howvhead     - significant wave height linked list head
!   def howvtail     - significant wave height linked list tail
!   def cldchhead    - cloud ceiling height linked list head
!   def cldchtail    - cloud ceiling height linked list tail
!   def uwnd10mhead  - 10m-uwind linked list head
!   def uwnd10mtail  - 10m-uwind linked list tail
!   def vwnd10mhead  - 10m-vwind linked list head
!   def vwnd10mtail  - 10m-vwind linked list tail
!   def swcphead     - solid-water content path linked list head
!   def swcptail     - solid-water content path linked list tail
!   def lwcphead     - liquid-water content path linked list head
!   def lwcptail     - liquid-water content path linked list tail
!   def lighthead    - lightning linked list head
!   def lighttail    - lightning linked list tail
!   def lunobs_obs   - unit to save satellite observation
!   def iout_rad     - output unit for satellite stats
!   def iout_pcp     - output unit for precipitation stats
!   def iout_t       - output unit for temperature stats
!   def iout_q       - output unit for moisture stats
!   def iout_uv      - output unit for wind stats
!   def iout_oz      - output unit for ozone stats
!   def iout_co      - output unit for co stats 
!   def iout_aero    - output unit for aerosol stats
!   def iout_ps      - output unit for surface pressure stats
!   def iout_pw      - output unit for precipitable water stats
!   def iout_rw      - output unit for radar wind stats
!   def iout_dw      - output unit for doppler wind stats
!   def iout_gps     - output unit for gps refractivity or bending angle stats
!   def iout_sst     - output unit for conventional sst stats
!   def iout_gust    - output unit for conventional gust stats
!   def iout_vis     - output unit for conventional vis stats
!   def iout_pblh    - output unit for conventional pblh stats
!   def iout_tcamt   - output unit for total cloud amount stats
!   def iout_lcbas   - output unit for lowest cloud base stats
!   def iout_lag     - output unit for conventional lag stats
!   def iout_wspd10m - output unit for conventional 10-m wind speed stats
!   def iout_uwnd10m - output unit for conventional 10-m uwnd stats
!   def iout_vwnd10m - output unit for conventional 10-m vwnd stats
!   def iout_td2m    - output unit for conventional 2-m dew point
!   def iout_mxtm    - output unit for conventional daily maximum temperature
!   def iout_mitm    - output unit for conventional daily minimum temperature
!   def iout_pmsl    - output unit for conventional pressure at mean sea level
!   def iout_howv    - output unit for conventional significant wave height stats
!   def iout_cldch   - output unit for conventional cldch stats
!   def iout_uwnd10m - output unit for conventional 10-m uwind stats
!   def iout_vwnd10m - output unit for conventional 10-m vwind stats
!   def iout_pm2_5   - output unit for pm2_5 stats
!   def iout_pm10    - output unit for pm10 stats
!   def iout_swcp    - output unit for swcp stats
!   def iout_lwcp    - output unit for lwcp stats
!   def iout_light   - output unit for lightning stats
!   def mype_t       - task to handle temperature stats
!   def mype_q       - task to handle moisture stats
!   def mype_uv      - task to handle wind stats
!   def mype_ps      - task to handle surface pressure stats
!   def mype_pw      - task to handle precipitable water stats
!   def mype_rw      - task to handle radar wind stats
!   def mype_dw      - task to handle doppler wind stats
!   def mype_gps     - task to handle gps observation stats
!   def mype_sst     - task to handle conventional sst stats
!   def mype_gust    - task to handle conventional gust stats
!   def mype_vis     - task to handle conventional vis stats
!   def mype_pblh    - task to handle conventional pblh stats
!   def mype_tcamt   - task to handle total cloud amount stats
!   def mype_lcbas   - task to handle lowest cloud base stats
!   def mype_lag     - task to handle conventional lag stats
!   def mype_wspd10m - task to handle conventional 10-m wind speed stats
!   def mype_uwnd10m - task to handle conventional 10-m uwnd stats
!   def mype_vwnd10m - task to handle conventional 10-m vwnd stats
!   def mype_td2m    - task to handle conventional 2-m dew point
!   def mype_mxtm    - task to handle conventional daily maximum temperature 
!   def mype_mitm    - task to handle conventional daily minimum temperature
!   def mype_pmsl    - task to handle conventional pressure at mean seal level
!   def mype_howv    - task to handle conventional significant wave height stats
!   def mype_cldch   - task to handle conventional cloud ceiling height stats
!   def mype_aero    - task to handle aerosol stats
!   def mype_pm2_5   - task to handle pm2_5
!   def mype_pm10    - task to handle pm10
!   def mype_swcp    - task to handle swcp
!   def mype_lwcp    - task to handle lwcp
!   def mype_light   - task to lightning stats
!   def oberrflg     - logical for reading in new observation error table
!                      .true.  will read in obs errors from file 'errtable'
!                      .false. will not read in new obs errors
!   def blacklst     - logical for reading in station blacklist table
!                      .true.  will read in blacklist from file 'blacklist'
!                      .false. will not read in blacklist
!   def ref_obs      - logical for reading type of local GPS observation
!                      .true.  will read refractivity
!                      .false. will read bending angle
!   def nprof_gps    - total number of gps profiles over all tasks
!   def sfcmodel     - logical for switching on boundary model for surface data
!   def dtbduv_on    - logical for switching on (.true.) sensitivity of uv winds
!                      to microwave brightness temperatures
!   def rmiss_single - missing value in diagnostic file
!   def offtime_data - logical, if .true., then allow use of obs files with ref time
!                      different from analysis time.
!
!   def hilbert_curve - logical, if .true., then generate hilbert curve based
!                      cross-validation datasets in 2dvar mode.
!   def lread_obs_save - logical, if .true., then write out collective obs selection info
!   def lread_obs_skip - logical, if .true., then read in collective obs selection info
!   def obs_input_common - scratch file to receive collective obs selection info
!   def lwrite_predterms - logical to write out actual predictor terms in diagnostic files
!                          .true. will write out actual predictor terms (for EnKF)
!                          .false. will write out predicted bias (default)
!   def lwrite_peakwt    - logical to write out approximate peak pressure of weighting 
!                          function to diag file
!                          .true. - uses iextra,jextra to append information to diag file
!                          .false. - write out standard diag file (default)
!   def ext_sonde    - logical for extended forward model on sonde data
!   def bmiss            - parameter to define missing value from bufr
!                      [10e10 on IBM CCS, 10e08 elsewhere]
!   def lrun_subdirs - logical to toggle use of subdirectories at run time for pe specific
!                      files
!   def l_foreaft_thin -   separate TDR fore/aft scan for thinning
!   def dval_use       -   = .true. if any dval weighting is used for satellite
!                           data
!   def obs_sub        - number of observations of each type in each subdomain
!                        (nobs_type,npe)
!   def binary_diag    - trigger binary diag-file output (being phased out)
!   def netcdf_diag    - trigger netcdf diag-file output
!   def l_obsprvdiag   - trigger obs provider info output into obsdiags files
!   def l_wcp_cwm      - namelist logical whether to use operator that
!                        includes cwm for both swcp and lwcp or not
!   def neutral_stability_windfact_2dvar - logical, if .true., then use simple formula representing
!                                          special case from similarity theory to compute the 10m-wind factor
!   def use_similarity_2dvar - logical, if .true., then use similarity theory from mm5
!                              sfc model to compute the 10m-wind factor
!   def aircraft_recon - namelist logibal whether to use DOE for aircraft
!
! attributes:
!   langauge: f90
!   machgine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use gsi_4dvar, only: l4dvar,lsqrtb,lbicg
  use constants, only:  zero,one,two,three,four,five
  use mpimod, only: mpi_max,mpi_itype,mpi_comm_world,ierror,npe,mype
  implicit none

! set default as private
  private
! set subroutines and functions to public
  public :: init_obsmod_dflts
  public :: init_directories
  public :: create_obsmod_vars
  public :: init_obsmod_vars
  public :: destroyobs
  public :: destroy_obsmod_vars
  public :: ran01dom,dval_use
  public :: iout_pcp,iout_rad,iadate,iadatemn,write_diag,reduce_diag,oberrflg,bflag,ndat,dthin,dmesh,l_do_adjoint
  public :: diag_radardbz
  public :: diag_fed
  public :: lsaveobsens
  public :: iout_cldch, mype_cldch
  public :: nprof_gps,time_offset,ianldate,tcp_box
  public :: iout_oz,iout_co,dsis,ref_obs,obsfile_all,lobserver,tcp_posmatch,perturb_obs,ditype,dsfcalc,dplat
  public :: time_window,dval,dtype,dfile,dirname,obs_setup,oberror_tune,offtime_data
  public :: lobsdiagsave,lobsdiag_forenkf,blacklst,hilbert_curve,lobskeep,time_window_max,sfcmodel,ext_sonde
  public :: neutral_stability_windfact_2dvar
  public :: use_similarity_2dvar
  public :: time_window_rad
  public :: perturb_fact,dtbduv_on,nsat1,obs_sub_comm,mype_diaghdr
  public :: ta2tb
  public :: lobsdiag_allocated
  public :: nloz_v8,nloz_v6,nloz_omi,nlco,nobskeep
  public :: rmiss_single,nchan_total,mype_sst,mype_gps
  public :: mype_uv,mype_dw,mype_rw,mype_q,mype_tcp,mype_lag,mype_ps,mype_t
  public :: mype_pw,iout_rw,iout_dw,iout_sst,iout_pw,iout_t,iout_q,iout_tcp
  public :: iout_lag,iout_uv,iout_gps,iout_ps,iout_light,mype_light
  public :: mype_gust,mype_vis,mype_pblh,iout_gust,iout_vis,iout_pblh
  public :: mype_tcamt,mype_lcbas,iout_tcamt,iout_lcbas
  public :: mype_wspd10m,mype_gnssrspd,mype_td2m,iout_wspd10m,iout_gnssrspd,iout_td2m
  public :: mype_uwnd10m,mype_vwnd10m,iout_uwnd10m,iout_vwnd10m 
  public :: mype_mxtm,mype_mitm,iout_mxtm,iout_mitm
  public :: mype_pmsl,mype_howv,iout_pmsl,iout_howv
  public :: mype_swcp,mype_lwcp,iout_swcp,iout_lwcp
  public :: lread_obs_save,obs_input_common,lread_obs_skip
  public :: ndat_times,lwrite_predterms,lwrite_peakwt
  public :: bmiss
  public :: mype_aero,iout_aero,nlaero
  public :: mype_pm2_5,iout_pm2_5
  public :: mype_pm10,iout_pm10
  public :: use_limit,lrun_subdirs
  public :: l_foreaft_thin,luse_obsdiag

  ! ==== DBZ DA ===
  public :: ntilt_radarfiles
  public :: whichradar
  public :: vr_dealisingopt, if_vterminal, if_model_dbz, if_vrobs_raw, if_use_w_vr, l2rwthin
  public :: inflate_dbz_obserr

  public :: doradaroneob,oneoblat,oneoblon
  public :: oneobddiff,oneobvalue,oneobheight,oneobradid
  public :: ens_hx_dbz_cut,static_gsi_nopcp_dbz,rmesh_dbz,zmesh_dbz,rmesh_vr,zmesh_vr,pmot_dbz
  public :: radar_no_thinning,pmot_vr
  public :: mintiltvr,maxtiltvr,minobrangevr,maxobrangevr
  public :: mintiltdbz,maxtiltdbz,minobrangedbz,maxobrangedbz
  public :: debugmode
  public :: missing_to_nopcp

  public :: iout_dbz, mype_dbz
  ! --- DBZ DA ---

  ! ==== FED DA ===
  public :: if_model_fed, innov_use_model_fed
  public :: r_hgt_fed
  public :: iout_fed, mype_fed  
  public :: dofedoneob
  ! --- FED DA ---

  public :: obsmod_init_instr_table
  public :: obsmod_final_instr_table
  public :: nobs_sub

  public :: netcdf_diag, binary_diag
  public :: l_obsprvdiag

  public :: l_wcp_cwm
  public :: aircraft_recon
  public :: hurricane_radar 

  ! The following public variables are the coefficients that describe
  ! the linear regression fits that are used to define the dynamic
  ! observation error (DOE) specifications for all reconnissance
  ! observations collected within hurricanes/tropical cyclones; these
  ! apply only to the regional forecast models (e.g., HWRF); Henry
  ! R. Winterbottom (henry.winterbottom@noaa.gov).

  ! Observation types:

  ! 1/236: HDOB (e.g., flight-level) observations.

  ! 1/237: Dropsonde observations.

  ! 213: SFMR observations.
  
  ! The following correspond to the specific humidity (q)
  ! observations:
  
  public :: q_doe_a_136
  public :: q_doe_a_137
  public :: q_doe_b_136
  public :: q_doe_b_137

  ! The following correspond to the temperature (t) observations:

  public :: t_doe_a_136
  public :: t_doe_a_137
  public :: t_doe_b_136
  public :: t_doe_b_137

  ! The following correspond to the wind (uv) observations:
  
  public :: uv_doe_a_236
  public :: uv_doe_a_237
  public :: uv_doe_a_213
  public :: uv_doe_b_236
  public :: uv_doe_b_237
  public :: uv_doe_b_213

  public :: vad_near_analtime

  ! The following correspond to OMPS LP observations:

  public :: ompslp_mult_fact

  interface obsmod_init_instr_table
          module procedure init_instr_table_
  end interface
  interface obsmod_final_instr_table
          module procedure final_instr_table_
  end interface

! Set parameters
  real(r_single), parameter:: rmiss_single = -999.0_r_single

! Set bufr missing value
#ifdef ibm_sp
  real(r_kind), parameter:: bmiss = 1.0e11_r_kind
#else
  real(r_kind), parameter:: bmiss = 1.0e9_r_kind
#endif

  logical luse_obsdiag
  logical binary_diag, netcdf_diag 
  logical l_obsprvdiag 

! Declare types

! Structure for diagnostics

! Declare interfaces
  interface destroyobs; module procedure destroyobs_; end interface

! Declare global variables

  real(r_kind) perturb_fact,time_window_max,time_offset,time_window_rad
  real(r_kind),dimension(50):: dmesh
  real(r_kind) r_hgt_fed
  integer(i_kind) nchan_total,ianldate
  integer(i_kind) ndat,ndat_types,ndat_times,nprof_gps
  integer(i_kind) lunobs_obs,nloz_v6,nloz_v8,nobskeep,nloz_omi
  integer(i_kind) nlco,use_limit
  integer(i_kind) iout_rad,iout_pcp,iout_t,iout_q,iout_uv, &
                  iout_oz,iout_ps,iout_pw,iout_rw, iout_dbz
  integer(i_kind) iout_dw,iout_gps,iout_sst,iout_tcp,iout_lag
  integer(i_kind) iout_co,iout_gust,iout_vis,iout_pblh,iout_tcamt,iout_lcbas
  integer(i_kind) iout_cldch
  integer(i_kind) iout_wspd10m,iout_gnssrspd,iout_td2m,iout_mxtm,iout_mitm,iout_pmsl,iout_howv
  integer(i_kind) iout_uwnd10m,iout_vwnd10m,iout_fed
  integer(i_kind) mype_t,mype_q,mype_uv,mype_ps,mype_pw, &
                  mype_rw,mype_dw,mype_gps,mype_sst, &
                  mype_tcp,mype_lag,mype_co,mype_gust,mype_vis,mype_pblh, &
                  mype_wspd10m,mype_gnssrspd,mype_td2m,mype_mxtm,mype_mitm,mype_pmsl,mype_howv,&
                  mype_uwnd10m,mype_vwnd10m, mype_tcamt,mype_lcbas, mype_dbz, mype_fed
  integer(i_kind) mype_cldch
  integer(i_kind) iout_swcp, iout_lwcp
  integer(i_kind) mype_swcp, mype_lwcp
  integer(i_kind) nlaero, iout_aero, mype_aero
  integer(i_kind) iout_pm2_5, mype_pm2_5
  integer(i_kind) iout_pm10, mype_pm10
  integer(i_kind) iout_light, mype_light
  integer(i_kind),dimension(5):: iadate
  integer(i_kind),dimension(5):: iadatemn
  integer(i_kind),allocatable,dimension(:):: dsfcalc,dthin,ipoint
  integer(i_kind),allocatable,dimension(:)::  nsat1,mype_diaghdr
  integer(i_kind),allocatable :: nobs_sub(:,:)
  integer(i_kind),allocatable :: obs_sub_comm(:)
  
  character(128) obs_setup
  character(128) dirname
  character(128) obs_input_common
  character(20),allocatable,dimension(:):: obsfile_all
  character(10),allocatable,dimension(:):: dtype,ditype
  character(11),allocatable,dimension(:):: dplat
  character(120),allocatable,dimension(:):: dfile
  character(20),allocatable,dimension(:):: dsis
  real(r_kind) ,allocatable,dimension(:):: dval
  real(r_kind) ,allocatable,dimension(:):: time_window

  integer(i_kind) ntilt_radarfiles,tcp_posmatch,tcp_box,pmot_dbz,pmot_vr

  logical ::  ta2tb
  logical ::  doradaroneob,dofedoneob
  logical :: vr_dealisingopt, if_vterminal, if_model_dbz,if_model_fed, innov_use_model_fed, if_vrobs_raw, if_use_w_vr, l2rwthin
  logical :: inflate_dbz_obserr
  character(4) :: whichradar,oneobradid
  real(r_kind) :: oneoblat,oneoblon,oneobddiff,oneobvalue,oneobheight
  logical :: radar_no_thinning
  logical :: ens_hx_dbz_cut
  real(r_kind) ::static_gsi_nopcp_dbz
  real(r_kind) ::rmesh_dbz,zmesh_dbz
  real(r_kind) ::rmesh_vr,zmesh_vr

  logical :: debugmode
  real(r_kind) :: minobrangevr,maxobrangevr,mintiltvr,maxtiltvr
  real(r_kind) :: minobrangedbz,maxobrangedbz,mintiltdbz,maxtiltdbz
  logical         :: missing_to_nopcp

  logical, save :: obs_instr_initialized_=.false.

  logical oberrflg,bflag,oberror_tune,perturb_obs,ref_obs,sfcmodel,dtbduv_on,dval_use
  logical blacklst,lobsdiagsave,lobsdiag_allocated,lobskeep,lsaveobsens
  logical lobserver,l_do_adjoint, lobsdiag_forenkf
  logical,dimension(0:50):: write_diag
  logical diag_radardbz 
  logical diag_fed
  logical reduce_diag
  logical offtime_data
  logical hilbert_curve
  logical lread_obs_save
  logical lread_obs_skip
  logical lwrite_predterms
  logical lwrite_peakwt
  logical ext_sonde
  logical lrun_subdirs
  logical l_foreaft_thin
  logical neutral_stability_windfact_2dvar
  logical use_similarity_2dvar

  logical l_wcp_cwm
  logical aircraft_recon
  logical hurricane_radar 

  character(len=*),parameter:: myname='obsmod'

  ! The following variable declarations pertain to the coefficients
  ! that describe the linear regression fits that are used to define
  ! the dynamic observation error (DOE) specifications for all
  ! reconnissance observations collected within hurricanes/tropical
  ! cyclones; these apply only to the regional forecast models (e.g.,
  ! HWRF); Henry R. Winterbottom (henry.winterbottom@noaa.gov).

  ! Observation types:

  ! 1/236: HDOB (e.g., flight-level) observations.

  ! 1/237: Dropsonde observations.

  ! 213: SFMR observations.

  ! The following correspond to the specific humidity (q)
  ! observations:
  
  real(r_kind) :: q_doe_a_136, q_doe_b_136
  real(r_kind) :: q_doe_a_137, q_doe_b_137

  ! The following correspond to the temperature (t) observations:
  
  real(r_kind) :: t_doe_a_136, t_doe_b_136
  real(r_kind) :: t_doe_a_137, t_doe_b_137

  ! The following correspond to the wind (uv) observations:
  
  real(r_kind) :: uv_doe_a_236, uv_doe_b_236
  real(r_kind) :: uv_doe_a_237, uv_doe_b_237
  real(r_kind) :: uv_doe_a_213, uv_doe_b_213

  logical vad_near_analtime 
  
 ! The following correspond to OMPS LP observations:

  real(r_kind) :: ompslp_mult_fact

contains

  subroutine init_obsmod_dflts
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_obsmod_dflts
!     prgmmr:    derber            org: np23           date: 2003-09-25
!
! abstract:  set defaults for observation related variables
!
! program history log:
!   2003-09-25  derber
!   2004-05-13  treadon, documentation
!   2004-07-23  derber - add conventional sst observations
!   2004-12-22  treadon - add initialization of write_diag
!   2005-02-07  treadon - change mype_* for obs types 
!   2005-02-18  treadon - change write_diag(1) default to .true.
!   2005-05-27  yanqiu  - added obs_sen
!   2005-05-27  derber  - add oberrflg
!   2005-06-14  wu      - add OMI oz (ozo)
!   2006-10-25  sienkiewicz - introduce blacklst
!   2007-05-03  todling - use values def above as indexes to cobstype
!   2008-11-25  todling - remove line-by-line adj triggers
!   2011-02-09  zhu     - add gust,vis and pblh
!   2013-09-27  todling - initialization of ob-instr/type move to sub init_instr_table_
!   2014-03-19  pondeca - add wspd10m
!   2014-04-10  pondeca - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add howv
!   2014-06-16  carley/zhu - add tcamt and lcbas
!   2015-07-10  pondeca - add cldch
!   2015-10-27  todling - default to luse_obsdiag is true now
!   2016-03-07  pondeca - add uwnd10m,vwnd10m
!   2021-11-16  zhao    - add initialization of l_obsprvdiag (.FALSE. as default)
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block
    implicit none

    integer(i_kind) i

    ntilt_radarfiles=1
    vr_dealisingopt=.false.
    if_vterminal=.false.
    l2rwthin    =.false.  
    if_vrobs_raw=.false.
    if_use_w_vr=.true.
    if_model_dbz=.false.
    if_model_fed=.false.
    innov_use_model_fed=.false.
    inflate_dbz_obserr=.false.
    whichradar="KKKK"

    oneobradid="KKKK"
    doradaroneob=.false.
    r_hgt_fed=6500_r_kind
    dofedoneob=.false.
    oneoblat=-999_r_kind
    oneoblon=-999_r_kind
    oneobddiff=-999_r_kind
    oneobvalue=-999_r_kind
    oneobheight=-999_r_kind
    radar_no_thinning=.false.
    ens_hx_dbz_cut=.false.
    static_gsi_nopcp_dbz=0.0_r_kind
    rmesh_dbz=2
    rmesh_vr=2
!  pmot_dbz values of 0,1,2,3 will save different sets of obs output
!      pmot_dbz - all obs - thin obs
!      pmot_dbz - all obs
!      pmot_dbz - use obs
!      pmot_dbz - use obs + thin obs

    pmot_dbz=0
    pmot_vr=2
    zmesh_dbz=500.0_r_kind
    zmesh_vr=500.0_r_kind
    minobrangedbz=10000.0_r_kind
    maxobrangedbz=200000.0_r_kind
    debugmode=.false.

    mintiltdbz=0.0_r_kind
    maxtiltdbz=20.0_r_kind
    minobrangevr=10000.0_r_kind
    maxobrangevr=200000.0_r_kind
    mintiltvr=0.0_r_kind
    maxtiltvr=20.0_r_kind
    missing_to_nopcp=.false.

!   Set logical flag
    perturb_obs = .false.   ! .true. = perturb observations
    tcp_posmatch = 0     
    tcp_box = 5     
    oberror_tune = .false.   ! .true. = tune oberror
    perturb_fact = one 
    do i=0,50
       write_diag(i)=.false.
    end do
    write_diag(1)=.true.
    diag_radardbz = .false.
    diag_fed = .false.
    reduce_diag = .false.
    use_limit = -1
    lobsdiagsave=.false.
    lobsdiag_allocated=.false.
    lobsdiag_forenkf = .false.
    lobskeep=.false.
    nobskeep=0
    lsaveobsens=.false.
    l_do_adjoint=.true.     ! .true. = apply H^T when in int routines
    ta2tb=.false.           ! .true. = assimilation antenna temperature for
                            !          AMSU-A, ATMS and MHS
    oberrflg  = .false.
    bflag     = .false.     ! 
    sfcmodel  = .false.     ! .false. = do not use boundary layer model 
    dtbduv_on = .true.      ! .true. = use microwave dTb/duv in inner loop
    offtime_data = .false.  ! .false. = code fails if data files contain ref time
                            !            different from analysis time
! moved to create_obsmod_var since l4dvar since before namelist is read
!   if (l4dvar) then
!      offtime_data = .true.   ! .true. = ignore difference in obs ref time
!   endif
    blacklst  = .false.
    lobserver = .false.     ! when .t., calculate departure vectors only
    ext_sonde = .false.     ! .false. = do not use extended forward model for sonde

!   Specify unit numbers to which to write data counts, indication of quality control
!   decisions, and statistics summary of innovations.  For radiance data also write
!   bias correction coefficients to this unit (iout_rad)
    iout_ps=201    ! surface pressure
    iout_uv=202    ! u,v wind components
    iout_t=203     ! virtual temperature
    iout_q=204     ! moisure (specific humidity)
    iout_pw=205    ! total column water (precipitable water)
    iout_oz=206    ! ozone
    iout_rad=207   ! radiance (brightness temperature)
    iout_pcp=208   ! precipitation rate
    iout_rw=209    ! radar radial wind
    iout_dw=210    ! doppler lidar wind
    iout_gps=212   ! gps refractivity or bending angle
    iout_sst=213   ! conventional sst
    iout_tcp=214   ! synthetic tc-mslp
    iout_lag=215   ! lagrangian tracers
    iout_co=216    ! co tracers
    iout_aero=217  ! aerosol product (aod) CURRENTLY NOT USED
    iout_gust=218  ! wind gust
    iout_vis=219   ! visibility
    iout_pblh=221  ! pbl height
    iout_pm2_5=222 ! pm2_5
    iout_wspd10m=223  ! 10-m wind speed
    iout_td2m=224  ! 2-m dew point
    iout_mxtm=225  ! daily maximum temperature
    iout_mitm=226  ! daily minimum temperature
    iout_pmsl=227  ! pressure at mean sea level
    iout_howv=228  ! significant wave height
    iout_tcamt=229 ! total cloud amount
    iout_lcbas=230 ! base height of lowest cloud
    iout_pm10=231  ! pm10
    iout_cldch=232 ! cloud ceiling height
    iout_uwnd10m=233  ! 10-m uwnd
    iout_vwnd10m=234  ! 10-m vwnd
    iout_swcp=235  ! solid-water content path
    iout_lwcp=236  ! liquid-water content path
    iout_light=237 ! lightning
    iout_dbz=238 ! radar reflectivity
    iout_fed=239   ! flash extent density
    iout_gnssrspd=240 ! GNSS-R wind speed

    mype_ps = npe-1          ! surface pressure
    mype_t  = max(0,npe-2)   ! temperature
    mype_q  = max(0,npe-3)   ! moisture
    mype_uv = max(0,npe-4)   ! uv
    mype_pw = max(0,npe-5)   ! precipitable water
    mype_rw = max(0,npe-6)   ! radial winds
    mype_dw = max(0,npe-7)   ! doppler lidar winds
    mype_co = max(0,npe-8)   ! carbon monoxide
    mype_gps= max(0,npe-9)   ! gps refractivity or bending angle
    mype_sst= max(0,npe-10)  ! conventional sst
    mype_tcp= max(0,npe-11)  ! synthetic tc-mslp
    mype_lag= max(0,npe-12)  ! lagrangian tracers
    mype_aero= max(0,npe-13) ! aerosol product (aod)
    mype_gust= max(0,npe-14) ! wind gust
    mype_vis = max(0,npe-15) ! visibility
    mype_pblh= max(0,npe-16) ! pbl height
    mype_pm2_5= max(0,npe-17)! pm2_5
    mype_wspd10m= max(0,npe-18)! wspd10m
    mype_td2m= max(0,npe-19) ! 2m dew point
    mype_mxtm= max(0,npe-20) ! daily maximum temperature
    mype_mitm= max(0,npe-21) ! daily minimum temperature
    mype_pmsl= max(0,npe-22) ! pressure at mean sea level
    mype_howv= max(0,npe-23) ! significant wave height
    mype_tcamt=max(0,npe-24) ! total cloud amount
    mype_lcbas=max(0,npe-25) ! base height of lowest cloud
    mype_pm10= max(0,npe-26) ! pm10
    mype_cldch=max(0,npe-27) ! cloud ceiling height
    mype_uwnd10m= max(0,npe-28)! uwnd10m
    mype_vwnd10m= max(0,npe-29)! vwnd10m
    mype_swcp=max(0,npe-30)  ! solid-water content path
    mype_lwcp=max(0,npe-31)  ! liquid-water content path
    mype_light=max(0,npe-32)! GOES/GLM lightning
    mype_dbz=max(0,npe-33)   ! radar reflectivity
    mype_fed= max(0,npe-34)  ! flash extent density
    mype_gnssrspd= max(0,npe-35) ! surface GNSS-R speed


!   Initialize arrays used in namelist obs_input 
    time_window_max = three ! set maximum time window to +/-three hours
    time_window_rad = three ! set maximum time window to +/-three hours for radiance


!   Other initializations
    nloz_v6 = 12               ! number of "levels" in ozone version8 data
    nloz_v8 = 21               ! number of "levels" in ozone version6 data
    nloz_omi= 11               ! number of "levels" in OMI apriori profile
    nlco    = 10               ! number of "levels" in MOPITT version 4 CO data

    lunobs_obs = 2             ! unit to which to write/read information
                               ! related to brightness temperature and 
                               ! precipitation rate observations

    nprof_gps = 0

    hilbert_curve=.false.
    neutral_stability_windfact_2dvar=.false.
    use_similarity_2dvar=.false.

    obs_input_common = 'obs_input.common'
    lread_obs_save   = .false.
    lread_obs_skip   = .false.
    lwrite_predterms = .false.
    lwrite_peakwt    = .false.
    lrun_subdirs     = .false.
    l_foreaft_thin   = .false.
    luse_obsdiag     = .false.

!   set default on diag writing
    netcdf_diag = .false. ! by default, do not write netcdf_diag
    binary_diag = .true.  ! by default, do write binary diag

!   set default on triggering the output of obs provider info into obsdiags file
    l_obsprvdiag = .false. ! by default, do not write obs provider info

    l_wcp_cwm          = .false.                 ! .true. = use operator that involves cwm
    aircraft_recon     = .false.                 ! .true. = use DOE for aircraft data
    hurricane_radar    = .false.                 ! .true. = use radar data for hurricane application 

    ! The following variable initializations pertain to the
    ! coefficients that describe the linear regression fits that are
    ! used to define the dynamic observation error (DOE)
    ! specifications for all reconnissance observations collected
    ! within hurricanes/tropical cyclones; these apply only to the
    ! regional forecast models (e.g., HWRF); Henry R. Winterbottom
    ! (henry.winterbottom@noaa.gov).

    ! Observation types:
    
    ! 1/236: HDOB (e.g., flight-level) observations.
    
    ! 1/237: Dropsonde observations.
    
    ! 213: SFMR observations.
    
    ! The following correspond to the specific humidity (q)
    ! observations:

    q_doe_a_136 = 1.0_r_kind
    q_doe_b_136 = 0.0_r_kind
    q_doe_a_137 = 1.0_r_kind
    q_doe_b_137 = 0.0_r_kind

    ! The following correspond to the temperature (t) observations:

    t_doe_a_136 = 1.0_r_kind
    t_doe_b_136 = 0.0_r_kind
    t_doe_a_137 = 1.0_r_kind
    t_doe_b_137 = 0.0_r_kind    

    ! The following correspond to the wind (uv) observations:

    uv_doe_a_236 = 1.0_r_kind
    uv_doe_b_236 = 0.0_r_kind
    uv_doe_a_237 = 1.0_r_kind
    uv_doe_b_237 = 0.0_r_kind      
    uv_doe_a_213 = 1.0_r_kind
    uv_doe_b_213 = 0.0_r_kind

    ! This flag was set to assimilate newvad obs around anaylsis time only.
    ! see 'read_prepbufr.f90'
    vad_near_analtime = .false.
    
    ! The following correspond to OMPS LP  observations:

    ompslp_mult_fact = 2.0_r_kind

    return
  end subroutine init_obsmod_dflts
  
  subroutine init_directories(in_pe,num_pe)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create sub-directories
!     prgmmr:    kleist      org: np23                date: 2007-06-08
!
! abstract:  set-up name for and create sub-directories to 
!            hold observation and diagnostic files.   Doing
!            so on IBM SP reduces wall clock and stabilizes
!            run times
!
! program history log:
!   2007-06-08  kleist
!   2008-06-02  safford - add doc block end
!
!   input argument list:
!      mype - pe task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block
#ifdef __INTEL_COMPILER
    use IFPORT
#endif
    implicit none

    integer(i_kind),intent(in   ) :: in_pe
    integer(i_kind),intent(in   ) :: num_pe
    logical :: l_mkdir_stat, l_dir_exist

    character(len=144):: command
    character(len=8):: pe_name, loc_pe_name
    character(len=128):: loc_dirname
    integer(i_kind) :: i, ierror

    if (lrun_subdirs) then
       write(pe_name,'(i4.4)') in_pe
       dirname = 'dir.'//trim(pe_name)//'/'
! Only create directories on one PE
       if(in_pe == 0) then
          do i = 0, num_pe
             write(loc_pe_name,'(i4.4)') i
             loc_dirname = 'dir.'//trim(loc_pe_name)
#ifdef __INTEL_COMPILER
             INQUIRE(directory=trim(loc_dirname), exist=l_dir_exist)
             if (.not.l_dir_exist) then
                l_mkdir_stat = MAKEDIRQQ(trim(loc_dirname))
                if(.not.l_mkdir_stat) then
                   ierror=GETLASTERRORQQ()
                   write(6, *) "INIT_DIRECTORIES:  ***ERROR** Failed to create directory ", &
                        trim(loc_dirname)," for PE ", loc_pe_name, ' ierror= ', ierror
                   call stop2(678)
                endif
             endif
#else
             command = 'mkdir -p -m 755 ' // trim(loc_dirname)
             call system(command)
#endif
          enddo
       endif
    else
       write(pe_name,100) in_pe
100 format('pe',i4.4,'.')
       dirname= trim(pe_name)
    end if

    return
  end subroutine init_directories
  
  subroutine create_obsmod_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_obsmod_vars
!     prgmmr:    derber            org: np23           date: 2003-09-25
!
! abstract:  allocate arrays to hold observation related information
!
! program history log:
!   2003-09-25  derber
!   2004-05-13  treadon, documentation
!   2014-10-06  carley - add obs_sub_comm
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block
    use mpimod, only: mype
    implicit none

    if (l4dvar) then
       offtime_data = .true.   ! .true. = ignore difference in obs ref time
    endif
    if (l4dvar .or. lsqrtb .or. lbicg) then
       luse_obsdiag = .true.
       if (mype==0) then
          write(6,*)'NOTICE: resetting luse_obsdiag to true '
       endif
    endif
    if(.not. luse_obsdiag) then
      if(lsaveobsens .or. lobsdiagsave)then
          write(6,*)'incompatabile luse_obsdiag and lsaveobsens or lobsdiagsave ', &
             luse_obsdiag,lsaveobsens,lobsdiagsave
          call stop2(843)
      end if
    end if

    allocate (nsat1(ndat),mype_diaghdr(ndat),obs_sub_comm(ndat))


    return
  end subroutine create_obsmod_vars

! ----------------------------------------------------------------------
  subroutine init_obsmod_vars(nhr_assim,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_obsmod_vars
!     prgmmr:    tremolet          org: GMAO           date: 2007-02-02
!
! abstract:  set values for observation related variables
!
! program history log:
!   2007-02-02  tremolet
!   2012-09-27  todling - remove dmesh from loop below; slight revision
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!
!$$$  end documentation block
    use gridmod, only: twodvar_regional
    implicit none

    integer(i_kind),intent(in   ) :: nhr_assim
    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) ii,jj,ioff
    character(len=2) :: cind
    logical :: limit

!   if(regional .and. .not.twodvar_regional)ndat_times = nhr_assim/3
    if(twodvar_regional)ndat_times = 1

!   The following was in gsimain.
!   Ensure time window specified in obs_input does not exceed
!   specified maximum value
    limit=.false.
    do ii=1,ndat_types
       if (time_window(ii)>time_window_max) then
          time_window(ii) = time_window_max
          limit = .true.
       endif
! for cris, iasi, atms, regional analysis may want shorter time window
       if (index(dtype(ii),'cris') /= 0 .or. index(dtype(ii),'atms') /= 0 .or. &
           index(dtype(ii),'iasi') /= 0 .or. index(dtype(ii),'iasi-ng') /= 0) then
          if(time_window(ii)>time_window_rad) then
             time_window(ii) = time_window_rad
             if (mype==0) write(6,*) 'INIT_OBSMOD_VARS: reset time window for ',dtype(ii),&
                               ' to ',time_window_rad
          endif
       endif
!
    end do
    if (mype==0 .and. limit) &
       write(6,*)'INIT_OBSMOD_VARS: reset time window for one or ',&
                 'more OBS_INPUT entries to ',time_window_max

!   Initialize arrays in obs_input if more than one synoptic time
    IF (ndat_times>1) THEN
!      Copy other things
       DO ii=2,ndat_times
          write(cind,'(i2.2)')ii
          ioff=(ii-1)*ndat_types
          DO jj=1,ndat_types
             dfile (ioff+jj) = trim(dfile(jj))//'.'//cind
             dtype (ioff+jj) = dtype(jj)
             ditype(ioff+jj) = ditype(jj)
             dplat (ioff+jj) = dplat(jj)
             dsis  (ioff+jj) = dsis(jj)
             ipoint(ioff+jj) = ipoint(jj)
             dthin (ioff+jj) = dthin(jj)
             dval  (ioff+jj) = dval(jj)
             dsfcalc(ioff+jj)= dsfcalc(jj)
             obsfile_all(ioff+jj) = trim(obsfile_all(jj))//'.'//cind
             time_window(ioff+jj) = time_window(jj)
          ENDDO
       ENDDO
!      Then change name for first time slot
       IF (ndat_times>1) THEN
          DO jj=1,ndat_types
             obsfile_all(jj) = trim(obsfile_all(jj))//'.01'
             dfile(jj) = trim(dfile(jj))//'.01'
          ENDDO
       ENDIF
    ENDIF

    IF (mype==0) THEN
       write(6,*)'INIT_OBSMOD_VARS: ndat_times,ndat_types,ndat=', &
                                  & ndat_times,ndat_types,ndat
       write(6,*)'INIT_OBSMOD_VARS: nhr_assimilation=',nhr_assim
    ENDIF

    return
  end subroutine init_obsmod_vars
! ----------------------------------------------------------------------


  subroutine destroyobs_()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroyobs
!     prgmmr:    derber            org: np23           date: 2003-09-25
!
! abstract:  deallocate arrays that hold observation information for
!            use in outer and inner loops
!
! program history log:
!   2003-09-25  derber
!   2004-05-13  treadon, documentation
!   2004-07-23  derber - add conventional sst observations
!   2005-06-14  wu      - add OMI oz (ozo)
!   2015-09-03  guo     - removed most of its functionality, but for obscounts
!   2016-04-27  guo     - removed argument skipit, and some remaining debris.
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$  end documentation block

    implicit none

    if (allocated(nobs_sub)) deallocate(nobs_sub) 

    return
  end subroutine destroyobs_
  
! ----------------------------------------------------------------------

  subroutine destroy_obsmod_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroyb_obsmod_vars
!     prgmmr:    derber            org: np23           date: 2003-09-25
!
! abstract:  deallocate arrays that hold observation information
!
! program history log:
!   2003-09-25  derber
!   2004-05-13  treadon, documentation
!   2014-10-06  carley - add obs_sub_comm
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$  end documentation block
    implicit none

    deallocate(nsat1,mype_diaghdr,obs_sub_comm)
    return
  end subroutine destroy_obsmod_vars

  real(r_kind) function ran01dom()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ran01dom    generate a random number with mean:0 variance:1
!   prgmmr: wu               org: np22                date: 2005-10-27
!
! abstract:  generate a random number with mean:0 variance:1
!
! program history log:
!   2005-10-27  wu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block

    use constants, only: five
    implicit none

    integer(i_kind):: j
    real(r_kind),dimension(10):: a

    call random_number(a)
    ran01dom=zero
    do j=1,10
       ran01dom=ran01dom+a(j)
    enddo
    ran01dom=(ran01dom-five)/0.912345_r_kind
    return
  end function ran01dom

! ----------------------------------------------------------------------
subroutine init_instr_table_ (nhr_assim,nall,iamroot,rcname)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   subroutine init_instr_table_()
!   prgmmr:     todling
!      org:     NASA/GSFC, Global Modeling and Assimilation Office, 610.1
!     date:     2013-02-04
!
! abstract: - read instrument table from file
!
! program history log:
!   2013-09-27  todling  - initial code
!   2014-11-13  pondeca  - put back ndat_times=1 exception for twodvar_regional
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block
use file_utility, only : get_lun
use mpeu_util, only: gettablesize
use mpeu_util, only: gettable
use mpeu_util, only: getindex
use gridmod, only: twodvar_regional
use mrmsmod,only: l_mrms_run,mrms_listfile
use mrmsmod,only: load_mrms_data_info
implicit none

integer(i_kind),intent(in)  :: nhr_assim       ! number of assimilation hours
integer(i_kind),intent(out) :: nall            ! number of data_type*assim_intervals
logical,optional,intent(in) :: iamroot         ! optional root processor id
character(len=*),optional,intent(in) :: rcname ! optional input filename

character(len=*),parameter::myname_=myname//'*init_instr_table_'
character(len=*),parameter:: tbname='OBS_INPUT::'
integer(i_kind) luin,ii,ntot,nrows,luin_mrms
character(len=256),allocatable,dimension(:):: utable
logical iamroot_
integer (i_kind)::nrows0
integer(i_kind) ntot_mrms,nrows_mrms

nall=0
if(obs_instr_initialized_) return

inquire(file=trim(mrms_listfile), exist=l_mrms_run)

iamroot_=mype==0
if(present(iamroot)) iamroot_=iamroot 

! load file
if (present(rcname)) then
   luin=get_lun()
   open(luin,file=trim(rcname),form='formatted')
else
   luin=5
endif

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,nrows)
if(nrows==0) then
   if(luin/=5) close(luin)
   if (.not.l_mrms_run) return
endif

nrows0=nrows
if (l_mrms_run) then  ! a run with radar ref data from MRMS
 luin_mrms=get_lun()
 open(luin_mrms,file=trim(mrms_listfile),form='formatted')
 call gettablesize(mrms_listfile,luin_mrms,ntot_mrms,nrows_mrms)
 nrows0=nrows
 nrows=nrows+nrows_mrms
 if(luin_mrms/=5) close(luin_mrms )
endif

! Get contents of table
allocate(utable(nrows))
call gettable(tbname,luin,ntot,nrows,utable)

! release file unit
if(luin/=5) close(luin)

! Because obs come in 6-hour batches
ndat_times=max(1,nhr_assim/6)
if(twodvar_regional)ndat_times = 1
ndat_types=nrows
nall=ndat_times*ndat_types

! allocate space for entries from table
allocate(dfile(nall),dtype(nall),dplat(nall),&
         dsis(nall),dval(nall),dthin(nall),dsfcalc(nall),&
         time_window(nall),obsfile_all(nall))

! things not in table, but dependent on nrows ... move somewhere else !_RTodling
! reality is that these things are not a function of nrows
allocate(ditype(nall),ipoint(nall))

! Retrieve each token of interest from table and define
! variables participating in state vector
dval_use = .false. 
do ii=1,nrows0
     read(utable(ii),*) dfile(ii),& ! local file name from which to read observatinal data
                        dtype(ii),& ! character string identifying type of observatio
                        dplat(ii),& ! currently contains satellite id (no meaning for non-sat data)
                        dsis(ii), & ! sensor/instrument/satellite identifier for info files
                        dval(ii), & ! 
                        dthin(ii),& ! thinning flag (1=thinning on; otherwise off)
                        dsfcalc(ii) ! use orig bilinear FOV surface calculation (routine deter_sfc)

   ! The following is to sort out some historical naming conventions
   select case (dsis(ii)(1:4))
      case ('airs')
         dsis(ii)='airs_aqua'
      case ('iasi')
         if (index(dsis(ii),'metop-a') /= 0) dsis(ii)='iasi_metop-a'
         if (index(dsis(ii),'metop-b') /= 0) dsis(ii)='iasi_metop-b'
         if (index(dsis(ii),'metop-c') /= 0) dsis(ii)='iasi_metop-c'
   end select

   if(trim(dplat(ii))=='null') dplat(ii)=' '
   if(dval(ii) > 0.0) dval_use = .true.
   ditype(ii)= ' '                    ! character string identifying group type of ob (see read_obs)
   ipoint(ii)= 0                      ! default pointer (values set in gsisub) _RT: This is never needed
   time_window(ii) = time_window_max  ! default to maximum time window
   write(obsfile_all(ii),'(a,i4.4)') 'obs_input.', ii      ! name of scratch file to hold obs data
   
enddo

deallocate(utable)

if (l_mrms_run) then
  if(present(rcname)) then
    call load_mrms_data_info(mrms_listfile,nrows0,ntot_mrms,nrows_mrms,nrows,obsfile_all,dfile,dtype,ditype,dplat,dsis,dval,dthin,ipoint,dsfcalc,time_window,rcname)
  else
    call load_mrms_data_info(mrms_listfile,nrows0,ntot_mrms,nrows_mrms,nrows,obsfile_all,dfile,dtype,ditype,dplat,dsis,dval,dthin,ipoint,dsfcalc,time_window)
  endif
endif

obs_instr_initialized_=.true.

end subroutine init_instr_table_

subroutine final_instr_table_

! clean up things initialized in init_instr_table_

if(.not.obs_instr_initialized_) return

deallocate(ditype,ipoint)

deallocate(dfile,dtype,dplat,&
           dsis,dval,dthin,dsfcalc,&
           time_window,obsfile_all)

obs_instr_initialized_ = .false.

end subroutine final_instr_table_

end module obsmod
