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
!   2015-07-10  pondeca  - add could ceiling height (cldch)
!   2016-05-18  collard  - Added code to allow for historical naming conventions
!                          for satellite instruments
!   2015-03-31  wgu      - add isis(sensor/instrument/satellite id) in rad_ob_type to handle
!                          instruments id in intrad inter-channel correlation
!                          implementation.
!   2016-07-19  wgu      - add isfctype - mask for surface type - to radiance obtype
!   2016-07-19  kbathmann - add rsqrtinv and use_corr_obs to rad_ob_type
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
!   def perturb_fact - namelist scaling factor for observation perturbations
!   def write_diag   - namelist logical array to compute/write (=true) diag files
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
!   def obsfile_all  - file containing observations after initial read
!   def ndat_types   - number of available data types
!   def ndat_times   - number of available synoptic times
!   def ndat         - total number of data types
!   def ipoint       - pointer to input namelist for particular processor
!   def iadate       - analysis date and time array
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
!   def srwhead      - superobed radial wind linked list head
!   def srwtail      - superobed radial wind linked list tail
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
!   def iout_srw     - output unit for radar superob wind stats
!   def iout_gps     - output unit for gps refractivity or bending angle stats
!   def iout_sst     - output unit for conventional sst stats
!   def iout_gust    - output unit for conventional gust stats
!   def iout_vis     - output unit for conventional vis stats
!   def iout_pblh    - output unit for conventional pblh stats
!   def iout_tcamt   - output unit for total cloud amount stats
!   def iout_lcbas   - output unit for lowest cloud base stats
!   def iout_lag     - output unit for conventional lag stats
!   def iout_wspd10m - output unit for conventional 10-m wind speed stats
!   def iout_td2m    - output unit for conventional 2-m dew point
!   def iout_mxtm    - output unit for conventional daily maximum temperature
!   def iout_mitm    - output unit for conventional daily minimum temperature
!   def iout_pmsl    - output unit for conventional pressure at mean sea level
!   def iout_howv    - output unit for conventional significant wave height stats
!   def iout_cldch   - output unit for conventional cldch stats
!   def iout_pm2_5   - output unit for pm2_5 stats
!   def iout_pm10    - output unit for pm10 stats
!   def mype_t       - task to handle temperature stats
!   def mype_q       - task to handle moisture stats
!   def mype_uv      - task to handle wind stats
!   def mype_ps      - task to handle surface pressure stats
!   def mype_pw      - task to handle precipitable water stats
!   def mype_rw      - task to handle radar wind stats
!   def mype_dw      - task to handle doppler wind stats
!   def mype_srw     - task to handle radar superob wind stats
!   def mype_gps     - task to handle gps observation stats
!   def mype_sst     - task to handle conventional sst stats
!   def mype_gust    - task to handle conventional gust stats
!   def mype_vis     - task to handle conventional vis stats
!   def mype_pblh    - task to handle conventional pblh stats
!   def mype_tcamt   - task to handle total cloud amount stats
!   def mype_lcbas   - task to handle lowest cloud base stats
!   def mype_lag     - task to handle conventional lag stats
!   def mype_wspd10m - task to handle conventional 10-m wind speed stats
!   def mype_td2m    - task to handle conventional 2-m dew point
!   def mype_mxtm    - task to handle conventional daily maximum temperature 
!   def mype_mitm    - task to handle conventional daily minimum temperature
!   def mype_pmsl    - task to handle conventional pressure at mean seal level
!   def mype_howv    - task to handle conventional significant wave height stats
!   def mype_cldch   - task to handle conventional cloud ceiling height stats
!   def mype_aero    - task to handle aerosol stats
!   def mype_pm2_5   - task to handle pm2_5
!   def mype_pm10    - task to handle pm10
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
!   def stpcnt         - number of non-zero obs types (including time domain) on
!                        processor - used for threading of stpjo
!   def ll_jo          - points at ob type for location in stpcnt - used for
!                        threading of stpjo
!   def ll_ib          - points at time bin for location in stpcnt - used for
!                        threading of stpjo
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
  public :: create_passive_obsmod_vars
  public :: init_obsmod_vars
  public :: destroyobs
  public :: destroyobs_passive
  public :: destroy_obsmod_vars
  public :: ran01dom,dval_use
  public :: destroy_genstats_gps
  public :: inquire_obsdiags
  public :: dfile_format
! set passed variables to public
  public :: iout_pcp,iout_rad,iadate,write_diag,reduce_diag,oberrflg,bflag,ndat,dthin,dmesh,l_do_adjoint
  public :: lsaveobsens,lag_ob_type,o3l_ob_type,oz_ob_type,colvk_ob_type,pcp_ob_type,dw_ob_type
  public :: sst_ob_type,srw_ob_type,spd_ob_type,rw_ob_type,gps_ob_type,gps_all_ob_type,tcp_ob_type
  public :: gust_ob_type,vis_ob_type,pblh_ob_type,wspd10m_ob_type,td2m_ob_type
  public :: mxtm_ob_type,mitm_ob_type,pmsl_ob_type,howv_ob_type,tcamt_ob_type,lcbas_ob_type,cldch_ob_type
  public :: rad_ob_type,q_ob_type,pw_ob_type,ps_ob_type,w_ob_type,t_ob_type
  public :: obs_handle,yobs,i_ps_ob_type,i_t_ob_type,i_w_ob_type,i_q_ob_type
  public :: i_spd_ob_type,i_srw_ob_type,i_rw_ob_type,i_dw_ob_type,i_sst_ob_type
  public :: i_gust_ob_type,i_vis_ob_type,i_pblh_ob_type,i_wspd10m_ob_type,i_td2m_ob_type
  public :: i_mxtm_ob_type,i_mitm_ob_type,i_pmsl_ob_type,i_howv_ob_type,i_tcamt_ob_type,i_lcbas_ob_type,i_cldch_ob_type
  public :: i_pw_ob_type,i_pcp_ob_type,i_oz_ob_type,i_o3l_ob_type,i_colvk_ob_type,i_gps_ob_type
  public :: i_rad_ob_type,i_tcp_ob_type,i_lag_ob_type,obscounts,obsptr,nobs_type,obsdiags
  public :: cobstype,gpsptr,obs_diag,nprof_gps,gps_allhead,gps_allptr,time_offset,ianldate
  public :: iout_oz,iout_co,dsis,ref_obs,obsfile_all,lobserver,perturb_obs,ditype,dsfcalc,dplat
  public :: time_window,dval,dtype,dfile,dirname,obs_setup,oberror_tune,offtime_data
  public :: lobsdiagsave,blacklst,hilbert_curve,lobskeep,time_window_max,sfcmodel,ext_sonde
  public :: perturb_fact,dtbduv_on,nsat1,obs_sub_comm,mype_diaghdr,wptr,whead,psptr,pshead
  public :: qptr,qhead,tptr,thead,lobsdiag_allocated,pstail,ttail,wtail,qtail,spdtail
  public :: spdhead,srwtail,srwhead,rwtail,rwhead,dwtail,dwhead,ssttail,ssthead,pwtail
  public :: pwhead,oztail,ozhead,o3ltail,o3lhead,colvktail,colvkhead,pcptail,pcphead,gpstail,gpshead
  public :: gusttail,gusthead,vistail,vishead,pblhtail,pblhhead,wspd10mtail,wspd10mhead,td2mtail,td2mhead
  public :: mxtmtail,mxtmhead,mitmtail,mitmhead,pmsltail,pmslhead,howvtail,howvhead,tcamttail,tcamthead,lcbastail,lcbashead
  public :: cldchtail,cldchhead
  public :: aero_ob_head,aero_ob_type,aerohead,aerotail,i_aero_ob_type
  public :: aerol_ob_head,aerol_ob_type,aerolhead,aeroltail,i_aerol_ob_type
  public :: pm2_5_ob_head,pm2_5_ob_type,i_pm2_5_ob_type,pm2_5head,pm2_5tail
  public :: pm10_ob_head,pm10_ob_type,i_pm10_ob_type,pm10head,pm10tail
  public :: radptr,radtail,radhead,lagtail,laghead,nloz_v8,nloz_v6,nloz_omi,nlco,nobskeep,gps_alltail
  public :: radptrm,radtailm,radheadm
  public :: grids_dim,rmiss_single,nchan_total,tcpptr,tcphead,tcptail,mype_sst,mype_gps
  public :: mype_uv,mype_dw,mype_rw,mype_srw,mype_q,mype_tcp,mype_lag,mype_ps,mype_t
  public :: mype_pw,iout_rw,iout_dw,iout_srw,iout_sst,iout_pw,iout_t,iout_q,iout_tcp
  public :: iout_lag,iout_uv,iout_gps,iout_ps,spdptr,srwptr,rwptr,dwptr,sstptr,pwptr
  public :: ozptr,o3lptr,coptr,pcpptr,lagptr,lread_obs_save,obs_input_common,lread_obs_skip
  public :: aeroptr,aerolptr,pm2_5ptr,pm10ptr
  public :: mype_gust,mype_vis,mype_pblh,iout_gust,iout_vis,iout_pblh,gustptr,visptr,pblhptr
  public :: mype_tcamt,mype_lcbas,iout_tcamt,iout_lcbas,tcamtptr,lcbasptr
  public :: mype_wspd10m,mype_td2m,iout_wspd10m,iout_td2m,wspd10mptr,td2mptr
  public :: mype_mxtm,mype_mitm,iout_mxtm,iout_mitm,mxtmptr,mitmptr
  public :: mype_pmsl,mype_howv,iout_pmsl,iout_howv,pmslptr,howvptr
  public :: mype_cldch,iout_cldch,cldchptr
  public :: ndat_times,lwrite_predterms,lwrite_peakwt
  public :: bmiss
!
  public :: obs_diags,gps_all_ob_head,w_ob_head,ps_ob_head,q_ob_head
  public :: t_ob_head,spd_ob_head,rw_ob_head,dw_ob_head,sst_ob_head
  public :: gust_ob_head,vis_ob_head,pblh_ob_head
  public :: wspd10m_ob_head,td2m_ob_head,mxtm_ob_head
  public :: mitm_ob_head,pmsl_ob_head,howv_ob_head
  public :: pcp_ob_head,o3l_ob_head,gps_ob_head
  public :: lag_ob_head,srw_ob_head,pw_ob_head,oz_ob_head,rad_ob_head
  public :: tcamt_ob_head,lcbas_ob_head,cldch_ob_head
  public :: tcp_ob_head,colvk_ob_head
  public :: mype_aero,iout_aero,nlaero
  public :: mype_pm2_5,iout_pm2_5
  public :: mype_pm10,iout_pm10
  public :: use_limit,lrun_subdirs
  public :: l_foreaft_thin,luse_obsdiag

  public :: obsmod_init_instr_table
  public :: obsmod_final_instr_table
  public :: nobs_sub
  public :: ll_jo,ib_jo,stpcnt

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
! Declare types

  integer(i_kind),parameter::  i_ps_ob_type= 1    ! ps_ob_type
  integer(i_kind),parameter::   i_t_ob_type= 2    ! t_ob_type
  integer(i_kind),parameter::   i_w_ob_type= 3    ! w_ob_type
  integer(i_kind),parameter::   i_q_ob_type= 4    ! q_ob_type
  integer(i_kind),parameter:: i_spd_ob_type= 5    ! spd_ob_type
  integer(i_kind),parameter:: i_srw_ob_type= 6    ! srw_ob_type
  integer(i_kind),parameter::  i_rw_ob_type= 7    ! rw_ob_type
  integer(i_kind),parameter::  i_dw_ob_type= 8    ! dw_ob_type
  integer(i_kind),parameter:: i_sst_ob_type= 9    ! sst_ob_type
  integer(i_kind),parameter::  i_pw_ob_type=10    ! pw_ob_type
  integer(i_kind),parameter:: i_pcp_ob_type=11    ! pcp_ob_type
  integer(i_kind),parameter::  i_oz_ob_type=12    ! oz_ob_type
  integer(i_kind),parameter:: i_o3l_ob_type=13    ! o3l_ob_type
  integer(i_kind),parameter:: i_gps_ob_type=14    ! gps_ob_type
  integer(i_kind),parameter:: i_rad_ob_type=15    ! rad_ob_type
  integer(i_kind),parameter:: i_tcp_ob_type=16    ! tcp_ob_type
  integer(i_kind),parameter:: i_lag_ob_type=17    ! lag_ob_type
  integer(i_kind),parameter:: i_colvk_ob_type= 18 ! colvk_ob_type
  integer(i_kind),parameter:: i_aero_ob_type =19  ! aero_ob_type
  integer(i_kind),parameter:: i_aerol_ob_type=20  ! aerol_ob_type
  integer(i_kind),parameter:: i_pm2_5_ob_type=21  ! pm2_5_ob_type
  integer(i_kind),parameter:: i_gust_ob_type=22   ! gust_ob_type
  integer(i_kind),parameter:: i_vis_ob_type=23    ! vis_ob_type
  integer(i_kind),parameter:: i_pblh_ob_type=24   ! pblh_ob_type
  integer(i_kind),parameter:: i_wspd10m_ob_type=25! wspd10m_ob_type
  integer(i_kind),parameter:: i_td2m_ob_type=26   ! td2m_ob_type
  integer(i_kind),parameter:: i_mxtm_ob_type=27   ! mxtm_ob_type
  integer(i_kind),parameter:: i_mitm_ob_type=28   ! mitm_ob_type
  integer(i_kind),parameter:: i_pmsl_ob_type=29   ! pmsl_ob_type
  integer(i_kind),parameter:: i_howv_ob_type=30   ! howv_ob_type
  integer(i_kind),parameter:: i_tcamt_ob_type=31  ! tcamt_ob_type
  integer(i_kind),parameter:: i_lcbas_ob_type=32  ! lcbas_ob_type  
  integer(i_kind),parameter:: i_pm10_ob_type=33   ! pm10_ob_type
  integer(i_kind),parameter:: i_cldch_ob_type=34  ! cldch_ob_type

  integer(i_kind),parameter:: nobs_type = 34      ! number of observation types

! Structure for diagnostics

  type obs_diag
     type(obs_diag), pointer :: next => NULL()
     real(r_kind), pointer :: nldepart(:)    ! (miter+1)
     real(r_kind), pointer :: tldepart(:)    ! (miter)
     real(r_kind), pointer :: obssen(:)      ! (miter)
     real(r_kind) :: wgtjo
     integer(i_kind) :: indxglb
     integer(i_kind) :: nchnperobs           ! number of channels per observations
                                             !  (dummy, except for radiances)
     integer(i_kind) :: idv,iob,ich   ! device id and obs index for verification
     logical, pointer :: muse(:)             ! (miter)
     logical :: luse

  end type obs_diag

  type obs_diags
     integer(i_kind):: n_alloc=0
     type(obs_diag), pointer :: head => NULL()
     type(obs_diag), pointer :: tail => NULL()
  end type obs_diags

  type aofp_obs_diag   ! array-of-Fortran-pointers of type(obs_diag)
     type(obs_diag), pointer :: ptr => NULL()
  end type aofp_obs_diag

! Main observation data structure

  type ps_ob_type
     type(ps_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  surface pressure residual
     real(r_kind)    :: err2          !  surface pressure error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: jb            !  variational quality control parameter(Purser's scheme)
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind)    :: ppertb        !  random number adding to the obs
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: kx            !  ob type
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type ps_ob_type
 
  type ps_ob_head
     integer(i_kind):: n_alloc=0
     type(ps_ob_type),pointer :: head => NULL()
  end type ps_ob_head

  type tcp_ob_type
     type(tcp_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  surface pressure residual
     real(r_kind)    :: err2          !  surface pressure error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind)    :: ppertb        !  random number adding to the obs
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: kx            !  ob type
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type tcp_ob_type

  type tcp_ob_head
     integer(i_kind):: n_alloc=0
     type(tcp_ob_type),pointer :: head => NULL()
  end type tcp_ob_head

  type t_ob_type
     type(t_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL() 
     real(r_kind)    :: res           !  temperature residual
     real(r_kind)    :: err2          !  temperature error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: jb            !  variational quality control parameter(Purser's scheme)
     real(r_kind)    :: tlm_tsfc(6)   !  sensitivity vector for sfc temp 
                                      !  forward model
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     real(r_kind)    :: tpertb        !  random number adding to the obs
     real(r_kind),dimension(:),pointer :: pred => NULL() 
                                      !  predictor for aircraft temperature bias 
     integer(i_kind) :: idx           !  index of tail number
     integer(i_kind) :: k1            !  level of errtable 1-33
     integer(i_kind) :: kx            !  ob type
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting

     logical         :: luse          !  flag indicating if ob is used in pen.
     logical         :: use_sfc_model !  logical flag for using boundary model
     logical         :: tv_ob         !  logical flag for virtual temperature or
  end type t_ob_type

  type t_ob_head
     integer(i_kind):: n_alloc=0
     type(t_ob_type),pointer :: head => NULL()
  end type t_ob_head
  
  type w_ob_type
     type(w_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diagu => NULL()
     type(obs_diag), pointer :: diagv => NULL()
     real(r_kind)    :: ures          !  u component residual
     real(r_kind)    :: vres          !  v component residual
     real(r_kind)    :: err2          !  surface pressure error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: jb            !  variational quality control parameter(Purser's scheme)
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     real(r_kind)    :: upertb        !  random number adding to the obs
     real(r_kind)    :: vpertb        !  random number adding to the obs
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: k1            !  level of errtable 1-33
     integer(i_kind) :: kx            !  ob type
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type w_ob_type

  type w_ob_head
     integer(i_kind):: n_alloc=0
     type(w_ob_type),pointer :: head => NULL()
  end type w_ob_head

  type q_ob_type
     type(q_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL() 
     real(r_kind)    :: res           !  moisture residual
     real(r_kind)    :: err2          !  moisture error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: jb            !  variational quality control parameter(Purser's scheme)
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     real(r_kind)    :: qpertb        !  random number adding to the obs
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: k1            !  level of errtable 1-33
     integer(i_kind) :: kx            !  ob type
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type q_ob_type

  type q_ob_head
     integer(i_kind):: n_alloc=0
     type(q_ob_type),pointer :: head => NULL()
  end type q_ob_head

  type spd_ob_type
     type(spd_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  speed observation
     real(r_kind)    :: err2          !  speed error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind)    :: uges          !  guess u value        
     real(r_kind)    :: vges          !  guess v value        
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type spd_ob_type

  type spd_ob_head
     integer(i_kind):: n_alloc=0
     type(spd_ob_type),pointer :: head => NULL()
  end type spd_ob_head

  type srw_ob_type
     type(srw_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diagu => NULL()
     type(obs_diag), pointer :: diagv => NULL()
     real(r_kind)    :: res1          !  first component residual
     real(r_kind)    :: res2          !  second component residual
     real(r_kind)    :: err2          !  surface pressure error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: ges1          !  first component guess
     real(r_kind)    :: ges2          !  second component guess
     real(r_kind)    :: rsrw(4)       !  forward model for radar superob wind 
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type srw_ob_type

  type srw_ob_head
     integer(i_kind):: n_alloc=0
     type(srw_ob_type),pointer :: head => NULL()
  end type srw_ob_head

  type rw_ob_type
     type(rw_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()     
     real(r_kind)    :: res           !  radial wind residual
     real(r_kind)    :: err2          !  radial wind error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: cosazm        !  v factor
     real(r_kind)    :: sinazm        !  u factor
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type rw_ob_type

  type rw_ob_head    
     integer(i_kind):: n_alloc=0
     type(rw_ob_type),pointer :: head => NULL()
  end type rw_ob_head

  type dw_ob_type
     type(dw_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  doppler wind residual
     real(r_kind)    :: err2          !  radial wind error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: cosazm        !  v factor
     real(r_kind)    :: sinazm        !  u factor
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type dw_ob_type

  type dw_ob_head
     integer(i_kind):: n_alloc=0
     type(dw_ob_type),pointer :: head => NULL()
  end type dw_ob_head

  type sst_ob_type
     type(sst_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  sst residual
     real(r_kind)    :: err2          !  sst error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind)    :: zob           !  observation depth in meter
     real(r_kind)    :: tz_tr         !  sensitivity of tob to tref : d(Tz)/d(Tr)
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type sst_ob_type

  type sst_ob_head
     integer(i_kind):: n_alloc=0
     type(sst_ob_type),pointer :: head => NULL()
  end type sst_ob_head

  type pw_ob_type
     type(pw_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  precipitable water residual
     real(r_kind)    :: err2          !  precipitable water error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind),dimension(:),pointer :: dp  => NULL()
                                      !  delta pressure at mid layers at obs locations
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type pw_ob_type

  type pw_ob_head
     integer(i_kind):: n_alloc=0
     type(pw_ob_type),pointer :: head => NULL()
  end type pw_ob_head

  type oz_ob_type
     type(oz_ob_type),pointer :: llpoint => NULL()
     type(aofp_obs_diag), dimension(:), pointer :: diags => NULL()
     real(r_kind),dimension(:),pointer :: res => NULL()
                                      !  ozone residual
     real(r_kind),dimension(:),pointer :: err2 => NULL()
                                      !  ozone error squared
     real(r_kind),dimension(:),pointer :: raterr2 => NULL()
                                      !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind),dimension(:,:),pointer :: wij => NULL()
                                      !  horizontal interpolation weights
     real(r_kind),dimension(:),pointer :: prs => NULL()
                                      !  pressure levels
     real(r_kind),dimension(:),pointer :: apriori    ! OMI retrieval first guess
     real(r_kind),dimension(:),pointer :: efficiency ! OMI efficiency factor
     integer(i_kind),dimension(:),pointer :: ipos  => NULL()
     integer(i_kind) :: nloz          ! number of levels for this profile
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type oz_ob_type

  type oz_ob_head    
     integer(i_kind):: n_alloc=0
     type(oz_ob_type),pointer :: head => NULL()
  end type oz_ob_head

  type o3l_ob_type
     type(o3l_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  ozone residual
     real(r_kind)    :: err2          !  ozone obs error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type o3l_ob_type

  type o3l_ob_head
     integer(i_kind):: n_alloc=0
     type(o3l_ob_type),pointer :: head => NULL()
  end type o3l_ob_head

  type colvk_ob_type
     type(colvk_ob_type),pointer :: llpoint => NULL()
     type(aofp_obs_diag), dimension(:), pointer :: diags => NULL()
     real(r_kind),dimension(:),pointer :: res => NULL()
                                      !  co residual
     real(r_kind),dimension(:),pointer :: err2 => NULL()
                                      !  co error squared
     real(r_kind),dimension(:),pointer :: raterr2 => NULL()
                                      !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind),dimension(:,:),pointer :: wij => NULL()
                                      !  horizontal interpolation weights
     real(r_kind),dimension(:),pointer :: prs => NULL()
                                      !  pressure levels
     real(r_kind),dimension(:,:),pointer :: ak  => NULL()   
                                      ! MOPITT vertical averaging kernel
     real(r_kind),dimension(:),pointer :: ap  => NULL()   
                                      ! MOPITT a priori
     real(r_kind),dimension(:),pointer   :: wkk1 => NULL()
     real(r_kind),dimension(:),pointer   :: wkk2 => NULL()
                                      ! vertical intropolation weights for MOPITT

     integer(i_kind),dimension(:),pointer :: ipos  => NULL()
     integer(i_kind) :: nlco          ! number of levels for this profile
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob         ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type colvk_ob_type

  type colvk_ob_head
     integer(i_kind):: n_alloc=0
     type(colvk_ob_type),pointer :: head => NULL()
  end type colvk_ob_head

  type aero_ob_type
     type(aero_ob_type),pointer :: llpoint => NULL()
     type(aofp_obs_diag), dimension(:), pointer :: diags => NULL()
     real(r_kind),dimension(:),pointer    :: res  => NULL()    !  aerosol property residual
     real(r_kind),dimension(:),pointer    :: err2 => NULL()    !  aerosol property error squared
     real(r_kind),dimension(:),pointer    :: raterr2 => NULL() !  square of ratio of final obs error
                                                               !  to original obs error
     real(r_kind)                         :: time              !  observation time in sec
     real(r_kind)    :: wij(4)                                 !  horizontal interpolation weights
     real(r_kind),dimension(:,:),pointer :: daod_dvar => NULL() ! jacobians_aero (nsig*n_aerosols,nchan)
     real(r_kind),dimension(:),pointer    :: prs => NULL()     !  pressure levels
     integer(i_kind),dimension(:),pointer :: ipos  => NULL()
     integer(i_kind),dimension(:),pointer :: icx  => NULL()
     integer(i_kind) :: ij(4)                                  !  horizontal locations
     integer(i_kind) :: nlaero                                 !  number of channels
     integer(i_kind) :: idv,iob                                !  device id and obs index for sorting
     integer(i_kind),dimension(:),pointer :: ich => NULL()
     logical         :: luse                                   !  flag indicating if ob is used in pen.
  end type aero_ob_type

  type aero_ob_head
     type(aero_ob_type),pointer :: head => NULL()
  end type aero_ob_head

  type aerol_ob_type
     type(aerol_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  aerosol residual
     real(r_kind)    :: err2          !  aerosol obs error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: idv,iob         ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type aerol_ob_type

  type aerol_ob_head
     type(aerol_ob_type),pointer :: head => NULL()
  end type aerol_ob_head

  type pm2_5_ob_type
! to avoid separate coding for pm2_5 profile e.g. from aircraft or 
! soundings obs weights are coded as 
! wij(8) even though for surface pm2_5 wij(4) would be sufficient.
! also surface pm2_5 may be treated differently than now for vertical
! interpolation

     type(pm2_5_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  pm2_5 residual
     real(r_kind)    :: err2          !  pm2_5 obs error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
!  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.
     
  end type pm2_5_ob_type
  
  type pm2_5_ob_head
     integer(i_kind):: n_alloc=0
     type(pm2_5_ob_type),pointer :: head => NULL()
  end type pm2_5_ob_head

  type pm10_ob_type
! to avoid separate coding for pm10 profile e.g. from aircraft or 
! soundings obs weights are coded as 
! wij(8) even though for surface pm10 wij(4) would be sufficient.
! also surface pm10 may be treated differently than now for vertical
! interpolation

     type(pm10_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  pm10 residual
     real(r_kind)    :: err2          !  pm10 obs error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
!  to original obs error
     real(r_kind)    :: time          !  observation time
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.
     
  end type pm10_ob_type
  
  type pm10_ob_head
     integer(i_kind):: n_alloc=0
     type(pm10_ob_type),pointer :: head => NULL()
  end type pm10_ob_head


  type gust_ob_type
     type(gust_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  gust residual
     real(r_kind)    :: err2          !  gust error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type gust_ob_type

  type gust_ob_head
     integer(i_kind):: n_alloc=0
     type(gust_ob_type),pointer :: head => NULL()
  end type gust_ob_head

  type vis_ob_type
     type(vis_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  vis residual
     real(r_kind)    :: err2          !  vis error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type vis_ob_type

  type vis_ob_head
     integer(i_kind):: n_alloc=0
     type(vis_ob_type),pointer :: head => NULL()
  end type vis_ob_head

  type pblh_ob_type
     type(pblh_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  pblh residual
     real(r_kind)    :: err2          !  pblh error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type pblh_ob_type

  type pblh_ob_head
     integer(i_kind):: n_alloc=0
     type(pblh_ob_type),pointer :: head => NULL()
  end type pblh_ob_head

  type wspd10m_ob_type
     type(wspd10m_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  wspd10m residual
     real(r_kind)    :: err2          !  wspd10m error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type wspd10m_ob_type

  type wspd10m_ob_head
     integer(i_kind):: n_alloc=0
     type(wspd10m_ob_type),pointer :: head => NULL()
  end type wspd10m_ob_head

  type td2m_ob_type
     type(td2m_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  td2m residual
     real(r_kind)    :: err2          !  td2m error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type td2m_ob_type

  type td2m_ob_head
     integer(i_kind):: n_alloc=0
     type(td2m_ob_type),pointer :: head => NULL()
  end type td2m_ob_head

  type mxtm_ob_type
     type(mxtm_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  mxtm residual
     real(r_kind)    :: err2          !  mxtm error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type mxtm_ob_type

  type mxtm_ob_head
     integer(i_kind):: n_alloc=0
     type(mxtm_ob_type),pointer :: head => NULL()
  end type mxtm_ob_head

  type mitm_ob_type
     type(mitm_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  mitm residual
     real(r_kind)    :: err2          !  mitm error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type mitm_ob_type

  type mitm_ob_head
     integer(i_kind):: n_alloc=0
     type(mitm_ob_type),pointer :: head => NULL()
  end type mitm_ob_head

  type pmsl_ob_type
     type(pmsl_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  pmsl residual
     real(r_kind)    :: err2          !  pmsl error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type pmsl_ob_type

  type pmsl_ob_head
     integer(i_kind):: n_alloc=0
     type(pmsl_ob_type),pointer :: head => NULL()
  end type pmsl_ob_head

  type howv_ob_type
     type(howv_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  howv residual
     real(r_kind)    :: err2          !  howv error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type howv_ob_type

  type howv_ob_head
     integer(i_kind):: n_alloc=0
     type(howv_ob_type),pointer :: head => NULL()
  end type howv_ob_head

  type tcamt_ob_type     
     type(tcamt_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  tcamt residual
     real(r_kind)    :: err2          !  tcamt error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type tcamt_ob_type

  type tcamt_ob_head 
     integer(i_kind):: n_alloc=0
     type(tcamt_ob_type),pointer :: head => NULL()
  end type tcamt_ob_head

  type lcbas_ob_type     
     type(lcbas_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  lcbas residual
     real(r_kind)    :: err2          !  lcbas error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.
  end type lcbas_ob_type

  type lcbas_ob_head 
     integer(i_kind):: n_alloc=0
     type(lcbas_ob_type),pointer :: head => NULL()
  end type lcbas_ob_head

  type cldch_ob_type
     type(cldch_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  cldch residual
     real(r_kind)    :: err2          !  cldch error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type cldch_ob_type

  type cldch_ob_head
     integer(i_kind):: n_alloc=0
     type(cldch_ob_type),pointer :: head => NULL()
  end type cldch_ob_head
  
  type gps_ob_type
     type(gps_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res           !  gps residual
     real(r_kind)    :: err2          !  gps error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights

     real(r_kind),dimension(:),pointer :: jac_q => NULL()
                                      !  q jacobian 
     real(r_kind),dimension(:),pointer :: jac_t => NULL()
                                      !  t jacobian 
     real(r_kind),dimension(:),pointer :: jac_p => NULL()
                                      !  p jacobian
     integer(i_kind),dimension(:,:),pointer :: ij  => NULL()
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type gps_ob_type

  type gps_ob_head
     integer(i_kind):: n_alloc=0
     type(gps_ob_type),pointer :: head => NULL()
  end type gps_ob_head

  type gps_all_ob_type
     type(gps_all_ob_type),pointer :: llpoint => NULL()
     type(gps_ob_type),pointer :: mmpoint => NULL()
     real(r_kind)    :: ratio_err                        
     real(r_kind)    :: obserr                        
     real(r_kind)    :: dataerr                       
     real(r_kind)    :: pg                       
     real(r_kind)    :: b                      
     real(r_kind)    :: loc                    
     real(r_kind)    :: type               

     real(r_kind),dimension(:),pointer :: rdiag => NULL()
     integer(i_kind) :: kprof
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     character(8)    :: cdiag
     logical         :: luse          !  flag indicating if ob is used in pen.

     logical         :: muse          !  flag indicating if ob is used in pen.

  end type gps_all_ob_type

  type gps_all_ob_head
     integer(i_kind):: n_alloc=0
     type(gps_all_ob_type),pointer :: head => NULL()
  end type gps_all_ob_head

  type rad_ob_type
     type(rad_ob_type),pointer :: llpoint => NULL()
     type(aofp_obs_diag), dimension(:), pointer :: diags => NULL()
     real(r_kind),dimension(:),pointer :: res => NULL()
                                      !  error variances squared (nchan)
     real(r_kind),dimension(:),pointer :: err2 => NULL()
                                      !  error variances squared (nchan)
     real(r_kind),dimension(:),pointer :: raterr2 => NULL()
                                      !  ratio of error variances squared (nchan)
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind),dimension(:,:),pointer :: pred => NULL()
                                      !  predictors (npred,nchan)
     real(r_kind),dimension(:,:),pointer :: dtb_dvar => NULL()
     real(r_kind),dimension(:),pointer :: rsqrtinv => NULL()
                                      !square root of inverse of R, only used
                                      !if using correlated obs
                                      !  error variances squared (nsigradjac,nchan)
     integer(i_kind),dimension(:),pointer :: icx  => NULL()
     integer(i_kind) :: nchan         !  number of channels for this profile
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: isfctype      ! surf mask: ocean=0, land=1, ice=2, snow=3, mixed=4
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     integer(i_kind),dimension(:),pointer :: ich => NULL()
     logical         :: luse          !  flag indicating if ob is used in pen.
     logical         :: use_corr_obs  !logical to indicate if using correlated obs
     character(20) :: isis            ! sensor/instrument/satellite id,e.g.amsua_n15

  end type rad_ob_type

  type rad_ob_head   
     integer(i_kind):: n_alloc=0
     type(rad_ob_type),pointer :: head => NULL()
  end type rad_ob_head

  type pcp_ob_type
     type(pcp_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: obs           !  observed precipitation value 
     real(r_kind)    :: err2          !  error variances squared
     real(r_kind)    :: raterr2       !  ratio of error variances squared 
     real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: ges           !  guess observation value
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind),dimension(:),pointer :: predp => NULL()
                                      !  predictors (npredp)
     real(r_kind),dimension(:),pointer :: dpcp_dvar => NULL()
                                      !  error variances squared (nsig5)
     integer(i_kind) :: ij(4)         !  horizontal locations
     integer(i_kind) :: icxp          !  type of precipitation rate observation
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          !  flag indicating if ob is used in pen.

  end type pcp_ob_type

  type pcp_ob_head
     integer(i_kind):: n_alloc=0
     type(pcp_ob_type),pointer :: head => NULL()
  end type pcp_ob_head
 

  type lag_ob_type
     type(lag_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diag_lon => NULL()
     type(obs_diag), pointer :: diag_lat => NULL()
     real(r_kind)    :: res_lon       ! residual
     real(r_kind)    :: res_lat       ! residual
     real(r_kind)    :: err2_lon      ! error squared
     real(r_kind)    :: err2_lat      ! error squared
     real(r_kind)    :: raterr2       ! square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: obslon        ! observed longitude (rad)
     real(r_kind)    :: obslat        ! observed latitude  (rad)
     real(r_kind)    :: geslon        ! guessed longitude (rad)
     real(r_kind)    :: geslat        ! guessed latitude  (rad)
     real(r_kind)   ,dimension(:),allocatable :: specr  ! TL parameter
     real(r_kind)    :: time          ! observation time in sec     
     real(r_kind)    :: b             ! variational quality control parameter
     real(r_kind)    :: pg            ! variational quality control parameter
     integer(i_kind),dimension(:),allocatable :: speci  ! TL parameter
     integer(i_kind) :: intnum        ! internal number of balloon
     integer(i_kind) :: idv,iob       ! device id and obs index for sorting
     logical         :: luse          ! flag indicating if ob is used in pen.

  end type lag_ob_type

  type lag_ob_head
     integer(i_kind):: n_alloc=0
     type(lag_ob_type),pointer :: head => NULL()
  end type lag_ob_head
  ! lfm --------------------------------------------------------------------

  type obs_handle
     type(ps_ob_type),pointer    :: ps  => NULL() 
     type(t_ob_type),pointer     :: t   => NULL()
     type(w_ob_type),pointer     :: w   => NULL()
     type(q_ob_type),pointer     :: q   => NULL()
     type(spd_ob_type),pointer   :: spd => NULL()
     type(srw_ob_type),pointer   :: srw => NULL()
     type(rw_ob_type),pointer    :: rw  => NULL()
     type(dw_ob_type),pointer    :: dw  => NULL()
     type(sst_ob_type),pointer   :: sst => NULL()
     type(pw_ob_type),pointer    :: pw  => NULL()
     type(oz_ob_type),pointer    :: oz  => NULL()
     type(o3l_ob_type),pointer   :: o3l => NULL()
     type(gps_ob_type),pointer   :: gps => NULL()
     type(rad_ob_type),pointer   :: rad => NULL()
     type(pcp_ob_type),pointer   :: pcp => NULL()
     type(tcp_ob_type),pointer   :: tcp => NULL()
     type(lag_ob_type),pointer   :: lag => NULL()
     type(colvk_ob_type),pointer :: colvk => NULL()
     type(aero_ob_type),pointer  :: aero  => NULL()
     type(aerol_ob_type),pointer :: aerol => NULL()
     type(pm2_5_ob_type),pointer :: pm2_5  => NULL()
     type(pm10_ob_type),pointer :: pm10  => NULL()
     type(gust_ob_type),pointer  :: gust => NULL()
     type(vis_ob_type),pointer   :: vis => NULL()
     type(pblh_ob_type),pointer  :: pblh => NULL()
     type(wspd10m_ob_type),pointer :: wspd10m => NULL()
     type(td2m_ob_type),pointer  :: td2m => NULL()
     type(mxtm_ob_type),pointer ::  mxtm => NULL()
     type(mitm_ob_type),pointer ::  mitm => NULL()
     type(pmsl_ob_type),pointer  :: pmsl => NULL()
     type(howv_ob_type),pointer  :: howv => NULL()
     type(tcamt_ob_type),pointer :: tcamt => NULL()
     type(lcbas_ob_type),pointer :: lcbas => NULL()
     type(cldch_ob_type),pointer::  cldch => NULL()


  end type obs_handle

! Declare types

  type(ps_ob_head),dimension(:),allocatable :: pshead
  type(ps_ob_head),dimension(:),allocatable :: pstail
  type(ps_ob_type),pointer :: psptr => NULL()
  type(tcp_ob_head),dimension(:),allocatable :: tcphead            
  type(tcp_ob_head),dimension(:),allocatable :: tcptail             
  type(tcp_ob_type),pointer :: tcpptr => NULL()
  type(t_ob_head),dimension(:),allocatable :: thead
  type(t_ob_head),dimension(:),allocatable :: ttail
  type(t_ob_type),pointer :: tptr => NULL()
  type(w_ob_head),dimension(:),pointer :: whead
  type(w_ob_head),dimension(:),pointer :: wtail
  type(w_ob_type),pointer :: wptr => NULL()
  type(q_ob_head),dimension(:),pointer :: qhead
  type(q_ob_head),dimension(:),pointer :: qtail
  type(q_ob_type),pointer :: qptr => NULL()
  type(spd_ob_head),dimension(:),pointer :: spdhead
  type(spd_ob_head),dimension(:),pointer :: spdtail
  type(spd_ob_type),pointer :: spdptr => NULL()
  type(srw_ob_head),dimension(:),pointer :: srwhead
  type(srw_ob_head),dimension(:),pointer :: srwtail
  type(srw_ob_type),pointer :: srwptr => NULL()
  type(rw_ob_head),dimension(:),pointer :: rwhead
  type(rw_ob_head),dimension(:),pointer :: rwtail
  type(rw_ob_type),pointer :: rwptr => NULL()
  type(dw_ob_head),dimension(:),pointer :: dwhead
  type(dw_ob_head),dimension(:),pointer :: dwtail
  type(dw_ob_type),pointer :: dwptr => NULL()
  type(sst_ob_head),dimension(:),pointer :: ssthead
  type(sst_ob_head),dimension(:),pointer :: ssttail
  type(sst_ob_type),pointer :: sstptr => NULL()
  type(pcp_ob_head),dimension(:),pointer :: pcphead
  type(pcp_ob_head),dimension(:),pointer :: pcptail
  type(pcp_ob_type),pointer :: pcpptr => NULL()
  type(pw_ob_head),dimension(:),pointer :: pwhead
  type(pw_ob_head),dimension(:),pointer :: pwtail
  type(pw_ob_type),pointer :: pwptr => NULL()
  type(oz_ob_head),dimension(:),pointer :: ozhead
  type(oz_ob_head),dimension(:),pointer :: oztail
  type(oz_ob_type),pointer :: ozptr => NULL()
  type(o3l_ob_head),dimension(:),pointer :: o3lhead
  type(o3l_ob_head),dimension(:),pointer :: o3ltail
  type(o3l_ob_type),pointer :: o3lptr => NULL()
  type(aero_ob_head),dimension(:),pointer :: aerohead => NULL()
  type(aero_ob_head),dimension(:),pointer :: aerotail => NULL()
  type(aero_ob_type),pointer :: aeroptr => NULL()
  type(aerol_ob_head),dimension(:),pointer :: aerolhead
  type(aerol_ob_head),dimension(:),pointer :: aeroltail
  type(aerol_ob_type),pointer :: aerolptr => NULL()
  type(pm2_5_ob_head),dimension(:),pointer :: pm2_5head
  type(pm2_5_ob_head),dimension(:),pointer :: pm2_5tail
  type(pm2_5_ob_type),pointer :: pm2_5ptr => NULL()
  type(pm10_ob_head),dimension(:),pointer :: pm10head
  type(pm10_ob_head),dimension(:),pointer :: pm10tail
  type(pm10_ob_type),pointer :: pm10ptr => NULL()
  type(gps_ob_head),dimension(:),pointer :: gpshead
  type(gps_ob_head),dimension(:),pointer :: gpstail
  type(gps_ob_type),pointer :: gpsptr => NULL()
  type(gps_all_ob_head),dimension(:),pointer :: gps_allhead
  type(gps_all_ob_head),dimension(:),pointer :: gps_alltail
  type(gps_all_ob_type),pointer :: gps_allptr => NULL()
  type(rad_ob_head),dimension(:),pointer :: radhead
  type(rad_ob_head),dimension(:),pointer :: radtail
  type(rad_ob_type),pointer :: radptr => NULL()
  type(rad_ob_head),dimension(:),pointer :: radheadm
  type(rad_ob_head),dimension(:),pointer :: radtailm
  type(rad_ob_type),pointer :: radptrm => NULL()
  type(lag_ob_head),dimension(:),pointer :: laghead
  type(lag_ob_head),dimension(:),pointer :: lagtail
  type(lag_ob_type),pointer :: lagptr => NULL()
  type(colvk_ob_head),dimension(:),pointer :: colvkhead
  type(colvk_ob_head),dimension(:),pointer :: colvktail
  type(colvk_ob_type),pointer :: coptr => NULL()
  type(gust_ob_head),dimension(:),pointer :: gusthead
  type(gust_ob_head),dimension(:),pointer :: gusttail
  type(gust_ob_type),pointer :: gustptr => NULL()
  type(vis_ob_head),dimension(:),pointer :: vishead
  type(vis_ob_head),dimension(:),pointer :: vistail
  type(vis_ob_type),pointer :: visptr => NULL()
  type(pblh_ob_head),dimension(:),pointer :: pblhhead
  type(pblh_ob_head),dimension(:),pointer :: pblhtail
  type(pblh_ob_type),pointer :: pblhptr => NULL()
  type(wspd10m_ob_head),dimension(:),pointer :: wspd10mhead
  type(wspd10m_ob_head),dimension(:),pointer :: wspd10mtail
  type(wspd10m_ob_type),pointer :: wspd10mptr => NULL()
  type(td2m_ob_head),dimension(:),pointer :: td2mhead
  type(td2m_ob_head),dimension(:),pointer :: td2mtail
  type(td2m_ob_type),pointer :: td2mptr => NULL()
  type(mxtm_ob_head),dimension(:),pointer :: mxtmhead
  type(mxtm_ob_head),dimension(:),pointer :: mxtmtail
  type(mxtm_ob_type),pointer :: mxtmptr => NULL()
  type(mitm_ob_head),dimension(:),pointer :: mitmhead
  type(mitm_ob_head),dimension(:),pointer :: mitmtail
  type(mitm_ob_type),pointer :: mitmptr => NULL()
  type(pmsl_ob_head),dimension(:),pointer :: pmslhead
  type(pmsl_ob_head),dimension(:),pointer :: pmsltail
  type(pmsl_ob_type),pointer :: pmslptr => NULL()
  type(howv_ob_head),dimension(:),pointer :: howvhead
  type(howv_ob_head),dimension(:),pointer :: howvtail
  type(howv_ob_type),pointer :: howvptr => NULL()
  type(tcamt_ob_head),dimension(:),pointer :: tcamthead
  type(tcamt_ob_head),dimension(:),pointer :: tcamttail
  type(tcamt_ob_type),pointer :: tcamtptr => NULL()
  type(lcbas_ob_head),dimension(:),pointer :: lcbashead
  type(lcbas_ob_head),dimension(:),pointer :: lcbastail
  type(lcbas_ob_type),pointer :: lcbasptr => NULL()
  type(cldch_ob_head),dimension(:),pointer :: cldchhead
  type(cldch_ob_head),dimension(:),pointer :: cldchtail
  type(cldch_ob_type),pointer :: cldchptr => NULL()


  type(obs_handle),dimension(:),pointer :: yobs

  type(obs_diags), pointer :: obsdiags(:,:)  ! (nobs_type,nobs_bins)
  type(obs_diag), pointer :: obsptr

! Declare interfaces
  interface destroyobs; module procedure destroyobs_; end interface

! Declare global variables

  real(r_kind) perturb_fact,time_window_max,time_offset
  real(r_kind),dimension(50):: dmesh

  integer(i_kind) grids_dim,nchan_total,ianldate
  integer(i_kind) ndat,ndat_types,ndat_times,nprof_gps
  integer(i_kind) lunobs_obs,nloz_v6,nloz_v8,nobskeep,nloz_omi
  integer(i_kind) nlco,use_limit,stpcnt
  integer(i_kind) iout_rad,iout_pcp,iout_t,iout_q,iout_uv, &
                  iout_oz,iout_ps,iout_pw,iout_rw
  integer(i_kind) iout_dw,iout_srw,iout_gps,iout_sst,iout_tcp,iout_lag
  integer(i_kind) iout_co,iout_gust,iout_vis,iout_pblh,iout_tcamt,iout_lcbas,iout_cldch
  integer(i_kind) iout_wspd10m,iout_td2m,iout_mxtm,iout_mitm,iout_pmsl,iout_howv
  integer(i_kind) mype_t,mype_q,mype_uv,mype_ps,mype_pw, &
                  mype_rw,mype_dw,mype_srw,mype_gps,mype_sst, &
                  mype_tcp,mype_lag,mype_co,mype_gust,mype_vis,mype_pblh, &
                  mype_wspd10m,mype_td2m,mype_mxtm,mype_mitm,mype_pmsl,mype_howv,&
                  mype_tcamt,mype_lcbas,mype_cldch
  integer(i_kind) nlaero, iout_aero, mype_aero
  integer(i_kind) iout_pm2_5, mype_pm2_5
  integer(i_kind) iout_pm10, mype_pm10
  integer(i_kind),dimension(5):: iadate
  integer(i_kind),allocatable,dimension(:):: dsfcalc,dthin,ipoint
  integer(i_kind),allocatable,dimension(:)::  nsat1,mype_diaghdr
  integer(i_kind),allocatable :: nobs_sub(:,:)
  integer(i_kind),allocatable,dimension(:)::ll_jo,ib_jo
  integer(i_kind),allocatable :: obscounts(:,:)
  integer(i_kind),allocatable :: obs_sub_comm(:)
  
  character(128) obs_setup
  character(128) dirname
  character(128) obs_input_common
  character(20),allocatable,dimension(:):: obsfile_all
  character(10),allocatable,dimension(:):: dtype,ditype,dplat
  character(20),allocatable,dimension(:):: dfile
  character(20),allocatable,dimension(:):: dsis
  real(r_kind) ,allocatable,dimension(:):: dval
  real(r_kind) ,allocatable,dimension(:):: time_window
  character(len=20) :: cobstype(nobs_type)

  logical, save :: obs_instr_initialized_=.false.

  logical oberrflg,bflag,oberror_tune,perturb_obs,ref_obs,sfcmodel,dtbduv_on,dval_use
  logical blacklst,lobsdiagsave,lobsdiag_allocated,lobskeep,lsaveobsens
  logical lobserver,l_do_adjoint
  logical,dimension(0:50):: write_diag
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

  character(len=*),parameter:: myname='obsmod'
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


!   Set logical flag
    perturb_obs = .false.   ! .true. = perturb observations
    oberror_tune = .false.   ! .true. = tune oberror
    perturb_fact = one 
    do i=0,50
       write_diag(i)=.false.
    end do
    write_diag(1)=.true.
    reduce_diag = .false.
    use_limit = -1
    lobsdiagsave=.false.
    lobsdiag_allocated=.false.
    lobskeep=.false.
    nobskeep=0
    lsaveobsens=.false.
    l_do_adjoint=.true.     ! .true. = apply H^T when in int routines
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
    iout_srw=211   ! radar superob wind
    iout_gps=212   ! gps refractivity or bending angle
    iout_sst=213   ! conventional sst
    iout_tcp=214   ! synthetic tc-mslp
    iout_lag=215   ! lagrangian tracers
    iout_co=216    ! co tracers
    iout_aero=217  ! aerosol product (aod)
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

    mype_ps = npe-1          ! surface pressure
    mype_uv = max(0,npe-2)   ! u,v wind components
    mype_t  = max(0,npe-3)   ! virtual temperature
    mype_q  = max(0,npe-4)   ! moisture (specific humidity)
    mype_pw = max(0,npe-5)   ! total column water
    mype_rw = max(0,npe-6)   ! radar radial wind
    mype_dw = max(0,npe-7)   ! doppler lidar wind
    mype_srw= max(0,npe-8)   ! radar superob wind
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

!   Initialize arrays used in namelist obs_input 
    time_window_max = three ! set maximum time window to +/-three hours


!   Other initializations
    nloz_v6 = 12               ! number of "levels" in ozone version8 data
    nloz_v8 = 21               ! number of "levels" in ozone version6 data
    nloz_omi= 11               ! number of "levels" in OMI apriori profile
    nlco    = 10               ! number of "levels" in MOPITT version 4 CO data

    lunobs_obs = 2             ! unit to which to write/read information
                               ! related to brightness temperature and 
                               ! precipitation rate observations

    grids_dim= 80              ! grid points for integration of GPS bend

    nprof_gps = 0

!   Define a name for obs types
    cobstype( i_ps_ob_type)  ="surface pressure    " ! ps_ob_type
    cobstype(  i_t_ob_type)  ="temperature         " ! t_ob_type
    cobstype(  i_w_ob_type)  ="wind                " ! w_ob_type
    cobstype(  i_q_ob_type)  ="moisture            " ! q_ob_type
    cobstype(i_spd_ob_type)  ="wind speed          " ! spd_ob_type
    cobstype(i_srw_ob_type)  ="srw                 " ! srw_ob_type
    cobstype( i_rw_ob_type)  ="radial wind         " ! rw_ob_type
    cobstype( i_dw_ob_type)  ="doppler wind        " ! dw_ob_type
    cobstype(i_sst_ob_type)  ="sst                 " ! sst_ob_type
    cobstype( i_pw_ob_type)  ="precipitable water  " ! pw_ob_type
    cobstype(i_pcp_ob_type)  ="precipitation       " ! pcp_ob_type
    cobstype( i_oz_ob_type)  ="ozone               " ! oz_ob_type
    cobstype(i_o3l_ob_type)  ="level ozone         " ! o3l_ob_type
    cobstype(i_gps_ob_type)  ="gps                 " ! gps_ob_type
    cobstype(i_rad_ob_type)  ="radiance            " ! rad_ob_type
    cobstype(i_tcp_ob_type)  ="tcp (tropic cyclone)" ! tcp_ob_type
    cobstype(i_lag_ob_type)  ="lagrangian tracer   " ! lag_ob_type
    cobstype(i_colvk_ob_type)="carbon monoxide     " ! colvk_ob_type
    cobstype( i_aero_ob_type)="aerosol aod         " ! aero_ob_type
    cobstype(i_aerol_ob_type)="level aero aod      " ! aerol_ob_type
    cobstype( i_pm2_5_ob_type)="in-situ pm2_5 obs  " ! pm2_5_ob_type
    cobstype( i_pm10_ob_type)="in-situ pm10 obs    " ! pm10_ob_type
    cobstype(i_gust_ob_type) ="gust                " ! gust_ob_type
    cobstype(i_vis_ob_type)  ="vis                 " ! vis_ob_type
    cobstype(i_pblh_ob_type) ="pblh                " ! pblh_ob_type
    cobstype(i_wspd10m_ob_type) ="wspd10m             " ! wspd10m_ob_type
    cobstype(i_td2m_ob_type) ="td2m                " ! td2m_ob_type
    cobstype(i_mxtm_ob_type) ="mxtm                " ! mxtm_ob_type
    cobstype(i_mitm_ob_type) ="mitm                " ! mitm_ob_type
    cobstype(i_pmsl_ob_type) ="pmsl                " ! pmsl_ob_type
    cobstype(i_howv_ob_type) ="howv                " ! howv_ob_type
    cobstype(i_tcamt_ob_type)="tcamt               " ! tcamt_ob_type
    cobstype(i_lcbas_ob_type)="lcbas               " ! lcbas_ob_type
    cobstype(i_cldch_ob_type)="cldch               " ! cldch_ob_type


    hilbert_curve=.false.

    obs_input_common = 'obs_input.common'
    lread_obs_save   = .false.
    lread_obs_skip   = .false.
    lwrite_predterms = .false.
    lwrite_peakwt    = .false.
    lrun_subdirs     = .false.
    l_foreaft_thin   = .false.
    luse_obsdiag     = .false.

    return
  end subroutine init_obsmod_dflts
  
  subroutine init_directories(mype)
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
    implicit none

    integer(i_kind),intent(in   ) :: mype

    character(len=144):: command
    character(len=8):: pe_name

    if (lrun_subdirs) then
       write(pe_name,'(i4.4)') mype
       dirname = 'dir.'//trim(pe_name)//'/'
       command = 'mkdir -m 755 ' // trim(dirname)
       call system(command)
    else
       write(pe_name,100) mype
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
    use gsi_4dvar, only: nobs_bins
    use mpimod, only: npe
    implicit none

    if (l4dvar) then
       offtime_data = .true.   ! .true. = ignore difference in obs ref time
    endif
    if (l4dvar .or. lsqrtb .or. lbicg) then
       luse_obsdiag = .true.
    endif
    if(.not. luse_obsdiag) then
      if(lsaveobsens .or. lobsdiagsave)then
          write(6,*)'incompatabile luse_obsdiag and lsaveobsens or lobsdiagsave ', &
             luse_obsdiag,lsaveobsens,lobsdiagsave
          call stop2(843)
      end if
    end if

    allocate (nsat1(ndat),mype_diaghdr(ndat),obs_sub_comm(ndat))

    ALLOCATE(thead  (nobs_bins))
    ALLOCATE(ttail  (nobs_bins))
    ALLOCATE(pshead (nobs_bins))
    ALLOCATE(pstail (nobs_bins))
    ALLOCATE(tcphead(nobs_bins))
    ALLOCATE(tcptail(nobs_bins))
    ALLOCATE(whead  (nobs_bins))
    ALLOCATE(wtail  (nobs_bins))
    ALLOCATE(qhead  (nobs_bins))
    ALLOCATE(qtail  (nobs_bins))
    ALLOCATE(spdhead(nobs_bins))
    ALLOCATE(spdtail(nobs_bins))
    ALLOCATE(srwhead(nobs_bins))
    ALLOCATE(srwtail(nobs_bins))
    ALLOCATE(rwhead (nobs_bins))
    ALLOCATE(rwtail (nobs_bins))
    ALLOCATE(dwhead (nobs_bins))
    ALLOCATE(dwtail (nobs_bins))
    ALLOCATE(ssthead(nobs_bins))
    ALLOCATE(ssttail(nobs_bins))
    ALLOCATE(pcphead(nobs_bins))
    ALLOCATE(pcptail(nobs_bins))
    ALLOCATE(pwhead (nobs_bins))
    ALLOCATE(pwtail (nobs_bins))
    ALLOCATE(ozhead (nobs_bins))
    ALLOCATE(oztail (nobs_bins))
    ALLOCATE(o3lhead(nobs_bins))
    ALLOCATE(o3ltail(nobs_bins))
    ALLOCATE(aerohead (nobs_bins))
    ALLOCATE(aerotail (nobs_bins))
    ALLOCATE(aerolhead(nobs_bins))
    ALLOCATE(aeroltail(nobs_bins))
    ALLOCATE(pm2_5head(nobs_bins))
    ALLOCATE(pm2_5tail(nobs_bins))
    ALLOCATE(pm10head(nobs_bins))
    ALLOCATE(pm10tail(nobs_bins))
    ALLOCATE(radhead(nobs_bins))
    ALLOCATE(radtail(nobs_bins))
    ALLOCATE(gpshead(nobs_bins))
    ALLOCATE(gpstail(nobs_bins))
    ALLOCATE(gps_allhead(nobs_bins))
    ALLOCATE(gps_alltail(nobs_bins))
    ALLOCATE(laghead(nobs_bins))
    ALLOCATE(lagtail(nobs_bins))
    ALLOCATE(colvkhead(nobs_bins))
    ALLOCATE(colvktail(nobs_bins))
    ALLOCATE(gusthead(nobs_bins))
    ALLOCATE(gusttail(nobs_bins))
    ALLOCATE(vishead(nobs_bins))
    ALLOCATE(vistail(nobs_bins))
    ALLOCATE(pblhhead(nobs_bins))
    ALLOCATE(pblhtail(nobs_bins))
    ALLOCATE(wspd10mhead(nobs_bins))
    ALLOCATE(wspd10mtail(nobs_bins))
    ALLOCATE(td2mhead(nobs_bins))
    ALLOCATE(td2mtail(nobs_bins))
    ALLOCATE(mxtmhead(nobs_bins))
    ALLOCATE(mxtmtail(nobs_bins))
    ALLOCATE(mitmhead(nobs_bins))
    ALLOCATE(mitmtail(nobs_bins))
    ALLOCATE(pmslhead(nobs_bins))
    ALLOCATE(pmsltail(nobs_bins))
    ALLOCATE(howvhead(nobs_bins))
    ALLOCATE(howvtail(nobs_bins))
    ALLOCATE(tcamthead(nobs_bins))
    ALLOCATE(tcamttail(nobs_bins))
    ALLOCATE(lcbashead(nobs_bins))
    ALLOCATE(lcbastail(nobs_bins))
    ALLOCATE(cldchhead(nobs_bins))
    ALLOCATE(cldchtail(nobs_bins))

    ALLOCATE(yobs(nobs_bins))
    allocate(ll_jo(nobs_bins*nobs_type),ib_jo(nobs_bins*nobs_type))
    ll_jo=0
    ib_jo=0
    stpcnt=0

    if(luse_obsdiag)ALLOCATE(obsdiags(nobs_type,nobs_bins))

    return
  end subroutine create_obsmod_vars

! ----------------------------------------------------------------------
  subroutine create_passive_obsmod_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_passive_obsmod_vars
!     prgmmr:    zhu            org: np23           date: 2010-05-12
!
! abstract:  allocate arrays to hold observation related information
!
! program history log:
!   2010-05-12 zhu
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
    use gsi_4dvar, only: nobs_bins
    implicit none

    ALLOCATE(radheadm(nobs_bins))
    ALLOCATE(radtailm(nobs_bins))

    return
  end subroutine create_passive_obsmod_vars

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
    use gridmod, only: regional,twodvar_regional
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


  subroutine destroyobs_ ( skipit )
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
!
!   input argument list:
!    skipit
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$  end documentation block
    use gsi_4dvar, only: nobs_bins

    implicit none

    logical,optional,intent(in   ) :: skipit

    integer(i_kind) :: ii,jj,istatus
    logical :: skipit_

    skipit_=.false.
    if (present(skipit)) then
       skipit_=skipit
    endif
    if(.not. luse_obsdiag)skipit_ = .true.
    if (.not. skipit_) then
       do ii=1,nobs_bins
          do jj=1,nobs_type
             obsptr => obsdiags(jj,ii)%head
             do while (associated(obsptr))
                obsdiags(jj,ii)%head => obsptr%next
                deallocate(obsptr%nldepart,obsptr%tldepart,obsptr%obssen,obsptr%muse)
                deallocate(obsptr)
                obsptr => obsdiags(jj,ii)%head
             enddo
          enddo
       enddo
       lobsdiag_allocated=.false.
    endif

    do ii=1,nobs_bins
       ttail(ii)%head => thead(ii)%head
       do while (associated(ttail(ii)%head))
          thead(ii)%head => ttail(ii)%head%llpoint
          deallocate(ttail(ii)%head%pred,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for t arrays, istatus=',istatus
          deallocate(ttail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for t, istatus=',istatus
          ttail(ii)%head => thead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       pwtail(ii)%head => pwhead(ii)%head
       do while (associated(pwtail(ii)%head))
          pwhead(ii)%head => pwtail(ii)%head%llpoint
          deallocate(pwtail(ii)%head%dp,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for pw arrays, istatus=',istatus
          deallocate(pwtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for pw, istatus=',istatus
          pwtail(ii)%head => pwhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       pstail(ii)%head => pshead(ii)%head
       do while (associated(pstail(ii)%head))
          pshead(ii)%head => pstail(ii)%head%llpoint
          deallocate(pstail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for ps, istatus=',istatus
          pstail(ii)%head => pshead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       wtail(ii)%head => whead(ii)%head
       do while (associated(wtail(ii)%head))
          whead(ii)%head => wtail(ii)%head%llpoint
          deallocate(wtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for w, istatus=',istatus
          wtail(ii)%head => whead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       qtail(ii)%head => qhead(ii)%head
       do while (associated(qtail(ii)%head))
          qhead(ii)%head => qtail(ii)%head%llpoint
          deallocate(qtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for q, istatus=',istatus
          qtail(ii)%head => qhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       spdtail(ii)%head => spdhead(ii)%head
       do while (associated(spdtail(ii)%head))
          spdhead(ii)%head => spdtail(ii)%head%llpoint
          deallocate(spdtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for spd, istatus=',istatus
          spdtail(ii)%head => spdhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       srwtail(ii)%head => srwhead(ii)%head
       do while (associated(srwtail(ii)%head))
          srwhead(ii)%head => srwtail(ii)%head%llpoint
          deallocate(srwtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for srw, istatus=',istatus
          srwtail(ii)%head => srwhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       rwtail(ii)%head => rwhead(ii)%head
       do while (associated(rwtail(ii)%head))
          rwhead(ii)%head => rwtail(ii)%head%llpoint
          deallocate(rwtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for rw, istatus=',istatus
          rwtail(ii)%head => rwhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       dwtail(ii)%head => dwhead(ii)%head
       do while (associated(dwtail(ii)%head))
          dwhead(ii)%head => dwtail(ii)%head%llpoint
          deallocate(dwtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for dw, istatus=',istatus
          dwtail(ii)%head => dwhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       ssttail(ii)%head => ssthead(ii)%head
       do while (associated(ssttail(ii)%head))
          ssthead(ii)%head => ssttail(ii)%head%llpoint
          deallocate(ssttail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for sst, istatus=',istatus
          ssttail(ii)%head => ssthead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       oztail(ii)%head => ozhead(ii)%head
       do while (associated(oztail(ii)%head))
          ozhead(ii)%head => oztail(ii)%head%llpoint
          deallocate(oztail(ii)%head%res, oztail(ii)%head%wij,&
                     oztail(ii)%head%err2,oztail(ii)%head%raterr2, &
                     oztail(ii)%head%prs,oztail(ii)%head%ipos, &
                     oztail(ii)%head%apriori,&
                     oztail(ii)%head%efficiency, stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for oz arrays, istatus=',istatus
          deallocate(oztail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for oz, istatus=',istatus
          oztail(ii)%head => ozhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       o3ltail(ii)%head => o3lhead(ii)%head
       do while (associated(o3ltail(ii)%head))
          o3lhead(ii)%head => o3ltail(ii)%head%llpoint
          deallocate(o3ltail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for o3l, istatus=',istatus
          o3ltail(ii)%head => o3lhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
      aerotail(ii)%head => aerohead(ii)%head
      do while (associated(aerotail(ii)%head))
        aerohead(ii)%head => aerotail(ii)%head%llpoint
        deallocate(aerotail(ii)%head%res, &
                   aerotail(ii)%head%err2,aerotail(ii)%head%raterr2, &
                   aerotail(ii)%head%daod_dvar,&
                   aerotail(ii)%head%ich, &
                   aerotail(ii)%head%icx,stat=istatus)
        if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for aero arrays, istatus=',istatus
        deallocate(aerotail(ii)%head,stat=istatus)
        if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for aero, istatus=',istatus
        aerotail(ii)%head => aerohead(ii)%head
      end do
    end do

    do ii=1,nobs_bins
      aeroltail(ii)%head => aerolhead(ii)%head
      do while (associated(aeroltail(ii)%head))
        aerolhead(ii)%head => aeroltail(ii)%head%llpoint
        deallocate(aeroltail(ii)%head,stat=istatus)
        if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for aerol, istatus=',istatus
        aeroltail(ii)%head => aerolhead(ii)%head
      end do
    end do

    do ii=1,nobs_bins
       pm2_5tail(ii)%head => pm2_5head(ii)%head
       do while (associated(pm2_5tail(ii)%head))
          pm2_5head(ii)%head => pm2_5tail(ii)%head%llpoint
          deallocate(pm2_5tail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for pm2_5, istatus=',istatus
          pm2_5tail(ii)%head => pm2_5head(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       pm10tail(ii)%head => pm10head(ii)%head
       do while (associated(pm10tail(ii)%head))
          pm10head(ii)%head => pm10tail(ii)%head%llpoint
          deallocate(pm10tail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for pm10, istatus=',istatus
          pm10tail(ii)%head => pm10head(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       gusttail(ii)%head => gusthead(ii)%head
       do while (associated(gusttail(ii)%head))
          gusthead(ii)%head => gusttail(ii)%head%llpoint
          deallocate(gusttail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for gust, istatus=',istatus
          gusttail(ii)%head => gusthead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       vistail(ii)%head => vishead(ii)%head
       do while (associated(vistail(ii)%head))
          vishead(ii)%head => vistail(ii)%head%llpoint
          deallocate(vistail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for vis, istatus=',istatus
          vistail(ii)%head => vishead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       pblhtail(ii)%head => pblhhead(ii)%head
       do while (associated(pblhtail(ii)%head))
          pblhhead(ii)%head => pblhtail(ii)%head%llpoint
          deallocate(pblhtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for pblh, istatus=',istatus
          pblhtail(ii)%head => pblhhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       wspd10mtail(ii)%head => wspd10mhead(ii)%head
       do while (associated(wspd10mtail(ii)%head))
          wspd10mhead(ii)%head => wspd10mtail(ii)%head%llpoint
          deallocate(wspd10mtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for wspd10m, istatus=',istatus
          wspd10mtail(ii)%head => wspd10mhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       td2mtail(ii)%head => td2mhead(ii)%head
       do while (associated(td2mtail(ii)%head))
          td2mhead(ii)%head => td2mtail(ii)%head%llpoint
          deallocate(td2mtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for td2m, istatus=',istatus
          td2mtail(ii)%head => td2mhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       mxtmtail(ii)%head => mxtmhead(ii)%head
       do while (associated(mxtmtail(ii)%head))
          mxtmhead(ii)%head => mxtmtail(ii)%head%llpoint
          deallocate(mxtmtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for mxtm, istatus=',istatus
          mxtmtail(ii)%head => mxtmhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       mitmtail(ii)%head => mitmhead(ii)%head
       do while (associated(mitmtail(ii)%head))
          mitmhead(ii)%head => mitmtail(ii)%head%llpoint
          deallocate(mitmtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for mitm, istatus=',istatus
          mitmtail(ii)%head => mitmhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       pmsltail(ii)%head => pmslhead(ii)%head
       do while (associated(pmsltail(ii)%head))
          pmslhead(ii)%head => pmsltail(ii)%head%llpoint
          deallocate(pmsltail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for pmsl, istatus=',istatus
          pmsltail(ii)%head => pmslhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       howvtail(ii)%head => howvhead(ii)%head
       do while (associated(howvtail(ii)%head))
          howvhead(ii)%head => howvtail(ii)%head%llpoint
          deallocate(howvtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for howv, istatus=',istatus
          howvtail(ii)%head => howvhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       tcamttail(ii)%head => tcamthead(ii)%head
       do while (associated(tcamttail(ii)%head))
          tcamthead(ii)%head => tcamttail(ii)%head%llpoint
          deallocate(tcamttail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for tcamt, istatus=',istatus
          tcamttail(ii)%head => tcamthead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       lcbastail(ii)%head => lcbashead(ii)%head
       do while (associated(lcbastail(ii)%head))
          lcbashead(ii)%head => lcbastail(ii)%head%llpoint
          deallocate(lcbastail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for lcbas, istatus=',istatus
          lcbastail(ii)%head => lcbashead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       cldchtail(ii)%head => cldchhead(ii)%head
       do while (associated(cldchtail(ii)%head))
          cldchhead(ii)%head => cldchtail(ii)%head%llpoint
          deallocate(cldchtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for cldch, istatus=',istatus
          cldchtail(ii)%head => cldchhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       gpstail(ii)%head => gpshead(ii)%head
       do while (associated(gpstail(ii)%head))
          gpshead(ii)%head => gpstail(ii)%head%llpoint
          deallocate(gpstail(ii)%head%jac_q,gpstail(ii)%head%jac_t, &
                     gpstail(ii)%head%jac_p, &
                     gpstail(ii)%head%ij,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for gps arrays, istatus=',istatus
          deallocate(gpstail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for gps, istatus=',istatus
          gpstail(ii)%head => gpshead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       radtail(ii)%head => radhead(ii)%head
       do while (associated(radtail(ii)%head))
          radhead(ii)%head => radtail(ii)%head%llpoint
          if (radtail(ii)%head%use_corr_obs) deallocate(radtail(ii)%head%rsqrtinv, stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for rad rsqrtinv, istatus=',istatus
          deallocate(radtail(ii)%head%res,radtail(ii)%head%err2, &
                     radtail(ii)%head%raterr2,radtail(ii)%head%pred, &
                     radtail(ii)%head%dtb_dvar,&
                     radtail(ii)%head%ich, &
                     radtail(ii)%head%icx,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for rad arrays, istatus=',istatus
          deallocate(radtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for rad, istatus=',istatus
          radtail(ii)%head => radhead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       pcptail(ii)%head => pcphead(ii)%head
       do while (associated(pcptail(ii)%head))
          pcphead(ii)%head => pcptail(ii)%head%llpoint
          deallocate(pcptail(ii)%head%predp,pcptail(ii)%head%dpcp_dvar,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for pcp arrays, istatus=',istatus
          deallocate(pcptail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for pcp, istatus=',istatus
          pcptail(ii)%head => pcphead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       tcptail(ii)%head => tcphead(ii)%head
       do while (associated(tcptail(ii)%head))
          tcphead(ii)%head => tcptail(ii)%head%llpoint
          deallocate(tcptail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for tcp, istatus=',istatus
          tcptail(ii)%head => tcphead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       lagtail(ii)%head => laghead(ii)%head
       do while (associated(lagtail(ii)%head))
          laghead(ii)%head => lagtail(ii)%head%llpoint
          deallocate(lagtail(ii)%head%speci,lagtail(ii)%head%specr,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for lag arrays, istatus=',istatus
          deallocate(lagtail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for lag, istatus=',istatus
          lagtail(ii)%head => laghead(ii)%head
       end do
    end do

    do ii=1,nobs_bins
       colvktail(ii)%head => colvkhead(ii)%head
       do while (associated(colvktail(ii)%head))
          colvkhead(ii)%head => colvktail(ii)%head%llpoint
          deallocate(colvktail(ii)%head%res, colvktail(ii)%head%wij,&
                     colvktail(ii)%head%err2,colvktail(ii)%head%raterr2, &
                     colvktail(ii)%head%prs,colvktail(ii)%head%ipos, &
                     colvktail(ii)%head%ak, colvktail(ii)%head%ap, &
                     colvktail(ii)%head%wkk1,colvktail(ii)%head%wkk2, &
                     stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for co arrays, istatus=',istatus
          deallocate(colvktail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS:  deallocate error for co, istatus=',istatus
          colvktail(ii)%head => colvkhead(ii)%head
       end do
    end do


    if (allocated(obscounts)) deallocate(obscounts) 
    if (allocated(nobs_sub)) deallocate(nobs_sub) 

    return
  end subroutine destroyobs_
  
! ----------------------------------------------------------------------

  subroutine destroyobs_passive
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroyobs_passive
!     prgmmr:    zhu            org: np23           date: 2010-05-12
!
! abstract:  deallocate arrays that hold observation information for
!            use in outer and inner loops
!
! program history log:
!   2010-05-12  zhu
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
    use gsi_4dvar, only: nobs_bins

    implicit none

    integer(i_kind) :: ii,istatus

    do ii=1,nobs_bins
       radtailm(ii)%head => radheadm(ii)%head
       do while (associated(radtailm(ii)%head))
          radheadm(ii)%head => radtailm(ii)%head%llpoint
          deallocate(radtailm(ii)%head%res,radtailm(ii)%head%err2, &
                     radtailm(ii)%head%raterr2,radtailm(ii)%head%pred, &
                     radtailm(ii)%head%ich,&
                     radtailm(ii)%head%icx,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS_PASSIVE:  deallocate error for rad arrays, istatus=',istatus
          deallocate(radtailm(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROYOBS_PASSIVE:  deallocate error for rad, istatus=',istatus
          radtailm(ii)%head => radheadm(ii)%head
       end do
    end do

    return
  end subroutine destroyobs_passive


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

  subroutine destroy_genstats_gps
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_genstats_gps
!     prgmmr:    treadon     org: np20                date: 2005-12-21
!
! abstract:  deallocate arrays holding gps information
!
! program history log:
!   2005-12-21  treadon
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
    use gsi_4dvar, only: nobs_bins
    implicit none

    integer(i_kind):: istatus,ii

    do ii=1,nobs_bins
       gps_alltail(ii)%head => gps_allhead(ii)%head
       do while (associated(gps_alltail(ii)%head))
          gps_allhead(ii)%head => gps_alltail(ii)%head%llpoint
          deallocate(gps_alltail(ii)%head%rdiag)
          deallocate(gps_alltail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROY_GENSTATS_GPS: deallocate error for gps_all, istatus=',istatus
          gps_alltail(ii)%head => gps_allhead(ii)%head
       end do
    end do

    return
  end subroutine destroy_genstats_gps

! ----------------------------------------------------------------------
subroutine inquire_obsdiags(kiter)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inquire_obsdiags
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-07  lueken - added  subprogram doc block
!
!   input argument list:
!    kiter
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

integer(i_kind), intent(in   ) :: kiter

real(r_kind) :: sizei, sizer, sizel, sizep, ziter, zsize, ztot
integer(i_kind) :: ii,jj,iobsa(2),iobsb(2)

! Any better way to determine size or i_kind, r_kind, etc... ?
sizei=four
sizer=8.0_r_kind
sizel=one
sizep=four

iobsa(:)=0
do ii=1,size(obsdiags,2)
   do jj=1,size(obsdiags,1)
      obsptr => obsdiags(jj,ii)%head
      do while (associated(obsptr))
         iobsa(1)=iobsa(1)+1
         if (ANY(obsptr%muse(:))) iobsa(2)=iobsa(2)+1
         obsptr => obsptr%next
      enddo
   enddo
enddo

call mpi_reduce(iobsa,iobsb,2,mpi_itype,mpi_max,0,mpi_comm_world,ierror)

if (mype==0) then
   ziter=real(kiter,r_kind)
   zsize = sizer*(three*ziter+two) + sizei + sizel*(ziter+one) + sizep*five
   ztot=real(iobsb(1),r_kind)*zsize
   ztot=ztot/(1024.0_r_kind*1024.0_r_kind)
 
   write(6,*)'obsdiags: Bytes per element=',NINT(zsize)
   write(6,*)'obsdiags: length total, used=',iobsb(1),iobsb(2)
   write(6,'(A,F8.1,A)')'obsdiags: Estimated memory usage= ',ztot,' Mb'
endif

end subroutine inquire_obsdiags
! ----------------------------------------------------------------------
function dfile_format(dfile) result(dform)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    function dfile_format
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 610.1
!     date:      2013-02-04
!
! abstract: - check filename suffix to guess its format
!
! program history log:
!   2013-02-04  j guo   - added this document block
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

! function interface:

  implicit none

  character(len=len('unknown')):: dform ! a 2-4 byte code for a format guess,
  ! from a list of filename suffixes, 'bufr', 'text' (also for 'txt',
  ! 'tcvitle', or 'vitl'), 'nc', or return a default value 'unknown'.  One
  ! can extend the list to other suffixes, such as 'hdf', 'hdf4', 'hdf5',
  ! etc., if they are needed in the future.
  character(len=*),intent(in):: dfile  ! a given filename

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=myname//'::dfile_format'
  integer(i_kind):: i,l

  dform='unknown'
  l=len_trim(dfile)

  i=max(0,l-6)+1      ! 6 byte code?
  select case(dfile(i:l))
  case ('tcvitl')
    dform='text'
  end select
  if(dform/='unknown') return

  i=max(0,l-4)+1! 4 byte code?
  select case(dfile(i:l))
  case ('bufr')
    dform='bufr'
  case ('text','vitl')
    dform='text'
  end select
  if(dform/='unknown') return

  i=max(0,l-3)+1   ! 3 byte code?
  select case(dfile(i:l))
  case ('txt')    ! a short
    dform='text'
  end select
  if(dform/='unknown') return

  i=max(0,l-2)+1    ! 2 byte code?
  select case(dfile(i:l))
  case ('nc')
    dform='nc'
  end select

return
end function dfile_format

subroutine init_instr_table_ (nhr_assim,nall,iamroot,rcname)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   function dfile_format
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
implicit none

integer(i_kind),intent(in)  :: nhr_assim       ! number of assimilation hours
integer(i_kind),intent(out) :: nall            ! number of data_type*assim_intervals
logical,optional,intent(in) :: iamroot         ! optional root processor id
character(len=*),optional,intent(in) :: rcname ! optional input filename

character(len=*),parameter::myname_=myname//'*init_instr_table_'
character(len=*),parameter:: tbname='OBS_INPUT::'
integer(i_kind) luin,ii,ntot,nrows
character(len=256),allocatable,dimension(:):: utable
logical iamroot_

nall=0
if(obs_instr_initialized_) return

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
   return
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
do ii=1,nrows
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
