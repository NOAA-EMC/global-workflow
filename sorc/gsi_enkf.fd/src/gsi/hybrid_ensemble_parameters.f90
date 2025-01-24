module hybrid_ensemble_parameters
!$$$   module documentation block
!                .      .    .                                       .
! module:    hybrid_ensemble_parameters  parms for hybrid ensemble
!   prgmmr: parrish          org: np22                date: 2009-09-16
!
! abstract: contains parameters which define the details of the
!             hybrid 3dvar ensemble option.
!   the following is a brief description of the hybrid 3dvar ensemble option:
!=====================================================================================================
!initial documentation for hybrid ensemble option (2009-12-16):
!
!  Only implemented for 3dvar full B version of GSI.  Future extension to sqrt(B) and 4dvar will
!   require collaboration with GMAO.  As long as hybrid ensemble option is turned off, 4dvar option
!   should still work.  This is an initial working formulation.  It is expected that many changes
!   will be made over the next several years.
!
! This formulation is based on 
!    Wang, X.,  D. M. Barker, C. Snyder, and T. M. Hamill, 2008:  A Hybrid ETKF.3DVAR 
!	Data Assimilation Scheme for the WRF Model.  Part I: Observing System 
!	Simulation Experiment.  Mon. Wea. Rev., 136, 5116-5131.
!
!  only difference is that preconditioning is based on full B instead of sqrt(B):
!
!  To introduce ensemble information into the background, a new control variable a (a_en in code), with
!    corresponding background error A, is added to the cost function.
!
!       J(x1,a) = .5*x1_trans*Bs_inv*x1 + .5*a_trans*Ae_inv*a + Jo(x,yobs)
!
!   where Bs is the weighted static background error covariance and
!         Ae is the weighted ensemble amplitude localization covariance.
!
!   Bs = diag(sqrt(beta_s)*B*diag(sqrt(beta_s)
!
!   Ae = diag(sqrt(beta_e)*A*diag(sqrt(beta_e)
!
!   beta_s and beta_e are currently allowed to vary in the vertical.
!
!   The 1st version of the hybrid ensemble code had beta_s, beta_e a single
!   constant, with the constraint 
!     
!        0 <= beta_s <= 1 , and  beta_e = 1 - beta_s
!
!   Now, vertical profiles of beta_s, beta_e are read in from file beta_profile
!    If file beta_profile is not available, then beta_s = beta_s0 and beta_e = 1 - beta_s0
!
!  The state variable x is recovered from the control variable pair (x1,a) using
!
!         x = L*(x1 + sum(n=1,n_ensemble (a(n) o x_ensemble(n)))  
!
!          where L is tlnmc or identity.
!
!                x_ensemble(n)  are n_ensemble sets of ensemble perturbations
!
!            and x o y is elementwise (Shur) product
!
!          Each a(n) is technically the same length as x_ensemble(n), but in practice
!            the actual stored size is one 3D grid.
!
!   Conversion from x1,a to x is implemented by  subroutine ensemble_forward_model, 
!                located in file hybrid_ensemble_isotropic.f90
!
!
!  A = diag(S,S,...,S) is a block diagonal matrix, and each S is a correlation matrix, applied
!  identically to each a(n).  In the current version, S is implemented using recursive filters
!  in horizontal and vertical (spectral in horizontal for global application).
!
!  Full background precondioning is used:
!
!         x1 = Bs*y1
!
!         a  = Ae*b
!
!  The resulting cost function:
!
!       J = .5*x1_trans*y1 + .5*a_trans*b  + Jo(x,yobs)
!
!   How to control the hybrid ensemble option:
!
!   NAMELIST HYBRID_ENSEMBLE:
!
!      l_hyb_ens:  logical variable, if .true., then turn on hybrid ensemble option, default = .false. 
!      n_ens:      ensemble size, default = 0
!      nlon_ens            - number of longitudes to use for ensemble members and ensemble control vector
!      nlat_ens            - number of latitudes to use for ensemble members and ensemble control vector
!      beta_s0:    value between 0 and 1, default = 1.0, 
!                  relative weight given to static background B when (readin_beta=.false.)
!                  when (readin_beta=.true.), the vertical weighting parameters are read from a file,
!                  instead of being defined based on beta_s0 namelist or default value. 
!      beta_e0 - default weight given to ensemble background error covariance
!                (if .not. readin_beta). if beta_e0<0, then it is set to
!                1.-beta_s0 (this is the default)
!      s_ens_h: horizontal localization correlation length of Gaussian exp(-0.5*(r/L)**2)
!               (units of km), default = 2828.0
!      s_ens_v: vertical localization correlation length of Gaussian exp(-0.5*(r/L)**2)
!               (grid units if s_ens_v>=0, or units of ln(p) if s_ens_v<0), default = 30.0
!                  in scale/variable/time-dependent localization (SDL/VDL/TDL),
!                  localization length for i-th scale, j-th variable, and k-th time is
!                     s_ens_[hv]( i + nsclgrp*(j-1) + nsclgrp*ngvarloc*(k-1) )
!                        in SDL(nsclgrp>1),         i = 1(largest scale)  .. nsclgrp(smallest scale)
!                        in VDL(ngvarloc=2),        j = 1(itracer<=10)    .. 2(itracer>=11)
!                        in TDL(l_timloc_opt=true), k = 1(first time bin) .. ntlevs_ens(last time bin)
!                  in SDL, scale separation length for i-th scale is also set here as
!                     s_ens_[hv]( naensgrp+i ) - naensgrp is the total number of localization lengths for SDL/VDL/TDL
!                        in applying SDL only horizontally, set s_ens_v(naensgrp+i)=0.0
!      generate_ens:  if .true., generate ensemble perturbations internally as random samples of background B.
!                       (used primarily for testing/debugging)
!                     if .false., read external ensemble perturbations
!      aniso_a_en: if .true., then allow anisotropic localization correlation (not active yet)
!      uv_hyb_ens: if .true., then ensemble perturbation wind stored as u,v
!                  if .false., ensemble perturbation wind stored as psi,chi.
!                   (this is useful for regional application, where there is ambiguity in how to
!                      define psi,chi from u,v)
!      q_hyb_ens:  if .true., then use specific humidity ensemble perturbations
!                  if .false. (default), use relative humidity
!      readin_localization:  if .true., then read in localization information from external file
!      use_localization_grid: if true, then use extra lower res gaussian grid for horizontal localization
!                                   (global runs only--allows possiblity for non-gaussian ensemble grid)
!      oz_univ_static:  if .true., ozone perturbations are zeroed out to make the ozone analysis
!                       univariate, and defaults back to static B (no ensemble component)
!      eqspace_ensgrid: if .true., then ensemble grid is equal spaced, staggered 1/2 grid unit off
!                               poles.  if .false., then gaussian grid assumed for ensemble (global only)
!      pwgtflg: if true, vertical integration function for ensemble contribution on Psfc
!      full_ensemble: if true, first ensemble member perturbed on first guess
!                    if false, first member perturbed on ensemble mean as the rest of the menbers
!      grid_ratio_ens: ratio of ensemble grid resolution to analysis resolution (default value is 1)
!      vvlocal:  logical variable, if .true., then horizontal localization length
!                function of z, default = .false. 
!      ensemble_path: path to ensemble members; default './'
!      ens_fast_read: read ensemble in parallel; default '.false.'
!      parallelization_over_ensmembers: parallelly read ensemble members for FV3-LAM; default '.false'      
!      sst_staticB:   if .true. (default) uses only static part of B error covariance for SST
!      nsclgrp:         number of scale-dependent localization lengths
!      l_timloc_opt:    if true, then turn on time-dependent localization
!      ngvarloc:        number of variable-dependent localization lengths
!      naensloc:        total number of spatial localization lengths and scale separation lengths (should be naensgrp+nsclgrp-1)
!      r_ensloccov4tim: factor multiplying to cross-time covariance
!                         For example,
!                         =0.0: cross-time covariance is decreased to zero
!                         =0.5: cross-time covariance is decreased to half
!                         =1.0: cross-time covariance is retained
!      r_ensloccov4var: factor multiplying to cross-variable covariance
!                         For example,
!                         =0.0: cross-variable covariance is decreased to zero
!                         =0.5: cross-variable covariance is decreased to half
!                         =1.0: cross-variable covariance is retained
!      r_ensloccov4scl: factor multiplying to cross-scale covariance
!                         For example,
!                         =0.0: cross-scale covariance is decreased to zero
!                         =0.5: cross-scale covariance is decreased to half
!                         =1.0: cross-scale covariance is retained
!      l_mgbf_loc: if true, multi-grid beta filter is used for localization instead of recursive filter
!=====================================================================================================
!
!
! program history log:
!   2009-09-16  parrish
!   2010-02-20  parrish - add changes to allow dual resolution capability.
!   2010-04-06  parrish - add 2nd option for units of vertical localization:
!                             if s_ens_v < 0, then abs(s_ens_v) is vertical localization length scale in
!                             units of ln(p).  otherwise, when s_ens_v > 0, localization is in vertical
!                             grid units.  The ln(p) distance measurement is approximate, based on a
!                             fixed surface pressure of 1000mb.  This is because at the point where filter
!                             constants are currently generated, the background 3d pressure field is not
!                             yet available.  A later update will correct this.
!                             For the current s_ens_v > 0, the measure is vertical grid units.
!                             s_ens_v = 20 and s_ens_v = -0.44 are roughly comparable, and
!                             connection of .44 is .44 = (sqrt(.15)/sqrt(2))*1.6, where 1.6 is the value used
!                             by Jeff Whitaker for his distance in which the Gaspari-Cohn function 1st = 0.
!   2010-09-25  parrish - add logical parameter gefs_in_regional to signal use gefs for regional hybens.
!   2010-10-13  parrish - add parameter write_ens_sprd to allow option of writing global ensemble spread
!                             in byte addressable format for plotting with grads.
!   2011-09-15  todling - add use_gfs_ens to control when to use other
!   2012-01-17  wu      - add switches and arrays for new options: full_ensemble,pwgtflg
!   2012-01-17  parrish - add integer parameter regional_ensemble_option which currently takes values
!                              1-5.  See def below for details. 
!   2012-02-07  tong    - remove logical parameter gefs_in_regional and reduce regional_ensemble_option
!                         to 4 options.
!   12-05-2012  el akkraoui - revised comments related to change in vertically varying beta weights
!   2013-01-20  parrish - move initialization of beta_s, beta_e, pwgt to after allocation.
!   2013-11-22  kleist  - add option for q perturbations
!   2014-05-14  wu      - add logical variable vvlocal for vertically verying horizontal localization length in regional
!   2015-01-22  Hu      - add flag i_en_perts_io to control reading ensemble perturbation.
!   2015-02-11  Hu      - add flag l_ens_in_diff_time to force GSI hybrid use ensembles not available at analysis time
!   2015-09-18  todling - add sst_staticB to control use of ensemble SST error covariance 
!   2022-09-15  yokota  - add scale/variable/time-dependent localization
!   2024-02-20  yokota  - add MGBF-based localization
!
! subroutines included:

! Variable Definitions:
!   def l_hyb_ens          - logical switch to turn on hybrid ensemble 3dvar
!   def uv_hyb_ens         - if true, then ensemble perturbation wind represented by u,v
!                               otherwise, ensemble perturbation wind represented by stream, pot. functions
!   def q_hyb_ens          - if true, use specific humidity
!   def aniso_a_en    - if true, then use anisotropic rf for localization
!   def generate_ens   - if true, then create ensemble members internally
!                              using sqrt of static background error acting on N(0,1) random vectors
!   def n_ens               - number of ensemble members
!   def nlon_ens            - number of longitudes to use for ensemble members and ensemble control vector
!   def nlat_ens            - number of latitudes to use for ensemble members and ensemble control vector
!   def jcap_ens            - for global spectral coef input, spectral truncation.
!   def jcap_ens_test       - for global spectral coef input, test spectral truncation.
!   def beta_s0             - default weight given to static background error covariance
!                             =1, then ensemble information turned off
!                             =0, then static background turned off
!                             the weights are applied per vertical level such that : 
!                                   beta_s(:) = beta_s0     , vertically varying weights given to B ; 
!                                   beta_e(:) = 1 - beta_s0 , vertically varying weights given to A.
!                            If (readin_beta) then beta_s and beta_e are read from a file and beta_s0 is not used.
!   def s_ens_h             - horizontal localization correlation length of Gaussian exp(-0.5*(r/L)**2)
!                             (units of km), default = 2828.0
!   def s_ens_v             - vertical localization correlation length of Gaussian exp(-0.5*(r/L)**2)
!                             (grid units if s_ens_v>=0, or units of ln(p) if s_ens_v<0), default = 30.0
!   def readin_localization - flag to read (.true.)external localization information file
!   def eqspace_ensgrid     - if .true., then ensemble grid is equal spaced, staggered 1/2 grid unit off
!                               poles.  if .false., then Gaussian grid assumed for ensemble (global only)
!   def grd_ens             - structure variable which is initialized by general_sub2grid_create_info in
!                              module general_sub2grid_mod.f90.  the information stored in grd_ens is
!                              used for subroutines general_grid2sub, general_sub2grid.  this has
!                              been created to make it easier to have two different resolution grids,
!                              one for analysis, and one for ensemble part.
!   def grd_loc             - specifically used for ensemble control variable a_en
!   def grd_sploc           - used for optional localization grid which is coarser than ensemble grid
!                                  (global runs only)
!   def grd_anl             - same as grd_ens, but on analysis grid
!   def grd_a1              - same as grd_anl, but with communication set up for a single 3d grid
!   def grd_e1              - same as grd_ens, but with communication set up for a single 3d grid
!   def sp_ens              - spectral structure variable, for use with reading gefs spectral files.
!   def sp_loc              - spectral structure variable, for use with spectral localization filter.
!   def p_e2a               - structure variable for interpolation from ensemble grid to analysis grid
!                              when in dual resolution mode.
!   def p_sploc2ens         - structure variable for interpolation from localization grid to ensemble grid
!                                  (global runs only)
!   def dual_res            - if true, then ensemble grid is different from analysis grid.
!   def pseudo_hybens       - if true, read in ensemble member from pseudo ensemble library and merge
!                             the pseudo ensemble perturbations with global ensemble perturbations.
!   def merge_two_grid_ensperts  - if true, merge ensemble perturbations from two forecast domains
!                                  to analysis domain (one way to deal with hybrid DA for HWRF moving nest)
!   def regional_ensemble_option - integer, used to select type of ensemble to read in for regional
!                                application.  Currently takes values from 1 to 5.
!                                 =1: use GEFS internally interpolated to ensemble grid.
!                                 =2: ensembles are WRF NMM format.
!                                 =3: ensembles are ARW netcdf format.
!                                 =4: ensembles are NEMS NMMB format.
!   def ntlevs_ens          - integer number of time levels for ensemble perturbations.  Default is 1, but
!                             will be equal to nobs_bins (4DVAR) when running in 4d-ensemble-var mode
!   def full_ensemble       - logical switch to use ensemble perturbation on first guess or on ensemble mean
!                              for the first member of ensemble
!   def   beta_s            - vertical weighting function for static B
!   def   beta_e            - vertical weighting function for localization A
!   def sqrt_beta_s            - sqrt(beta_s)
!   def sqrt_beta_e            - sqrt(beta_e)
!   def pwgt                - vertical integration function for influence of ensemble on Psfc
!   def pwgtflg             - logical switch to use vertical integration function for ensemble contribution on Psfc
!   def grid_ratio_ens:     - ratio of ensemble grid resolution to analysis resolution (default value is 1)
!   def use_localization_grid - if true, then use extra lower res gaussian grid for horizontal localization
!                                   (global runs only--allows possiblity for non-gaussian ensemble grid)
!   def vvlocal             - logical switch for vertically varying horizontal localization length
!   def i_en_perts_io       - flag to write out and read in ensemble perturbations in ensemble grid.   
!                             This is to speed up RAP/HRRR hybrid runs because the
!                             same ensemble perturbations are used in 6 cycles    
!                               =0:  No ensemble perturbations IO (default)
!                               =2:  skip get_gefs_for_regional and read in ensemble
!                                     perturbations from saved files.
!   def  l_ens_in_diff_time  -  if use ensembles that are available at different       
!                               time from analysis time.
!                             =false: only ensembles available at analysis time
!                                      can be used for hybrid. (default)
!                             =true: ensembles available time can be different
!                                      from analysis time in hybrid analysis
!   def ensemble_path        - path to ensemble members; default './'
!   def ens_fast_read        - read ensemble in parallel; default '.false.'
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind,r_single
  use general_sub2grid_mod, only: sub2grid_info
  use general_specmod, only: spec_vars
  use egrid2agrid_mod, only: egrid2agrid_parm
  use gsi_bundlemod, only: gsi_bundle

  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_hybrid_ensemble_parameters,create_hybens_localization_parameters,&
       destroy_hybens_localization_parameters
! set passed variables to public
  public :: generate_ens,n_ens,nlon_ens,nlat_ens,jcap_ens,jcap_ens_test,l_hyb_ens,&
       s_ens_h,oz_univ_static,vvlocal
  public :: n_ens_gfs,n_ens_fv3sar
  public :: weight_ens_gfs,weight_ens_fv3sar
  public :: uv_hyb_ens,q_hyb_ens,s_ens_v,beta_s0,beta_e0,aniso_a_en,s_ens_hv,s_ens_vv
  public :: readin_beta,beta_s,beta_e
  public :: readin_localization
  public :: eqspace_ensgrid,grid_ratio_ens
  public :: sqrt_beta_s,sqrt_beta_e,pwgt,full_ensemble,pwgtflg
  public :: grd_ens
  public :: grd_e1
  public :: grd_loc
  public :: grd_sploc
  public :: grd_anl
  public :: grd_a1
  public :: sp_ens
  public :: sp_loc
  public :: p_e2a
  public :: p_sploc2ens
  public :: use_localization_grid
  public :: use_gfs_ens
  public :: dual_res
  public :: pseudo_hybens
  public :: merge_two_grid_ensperts
  public :: regional_ensemble_option
  public :: fv3sar_ensemble_opt 
  
  public :: write_ens_sprd
  public :: nval_lenz_en
  public :: ntlevs_ens
  public :: i_en_perts_io
  public :: l_ens_in_diff_time
  public :: ensemble_path
  public :: nelen
  public :: en_perts,ps_bar
  public :: region_lat_ens,region_lon_ens
  public :: region_dx_ens,region_dy_ens
  public :: naensgrp,ntotensgrp,nsclgrp,naensloc,ngvarloc
  public :: ensgrp2aensgrp
  public :: ensloccov4tim,ensloccov4var,ensloccov4scl
  public :: alphacvarsclgrpmat
  public :: l_timloc_opt
  public :: r_ensloccov4tim,r_ensloccov4var,r_ensloccov4scl
  public :: l_mgbf_loc
  public :: idaen3d,idaen2d
  public :: ens_fast_read
  public :: parallelization_over_ensmembers
  public :: l_both_fv3sar_gfs_ens 
  public :: sst_staticB
  public :: limqens

  public :: spc_multwgt
  public :: spcwgt_params
  public :: vdl_scale,vloc_varlist
  public :: global_spectral_filter_sd
  public :: assign_vdl_nml

  logical l_hyb_ens,uv_hyb_ens,q_hyb_ens,oz_univ_static,sst_staticB
  logical l_timloc_opt
  logical l_mgbf_loc
  logical aniso_a_en
  logical full_ensemble,pwgtflg
  logical generate_ens
  logical dual_res
  logical pseudo_hybens
  logical merge_two_grid_ensperts
  logical write_ens_sprd
  logical readin_localization
  logical readin_beta
  logical use_localization_grid
  logical use_gfs_ens
  logical eqspace_ensgrid
  logical vvlocal
  logical l_ens_in_diff_time
  logical ens_fast_read
  logical parallelization_over_ensmembers
  logical l_both_fv3sar_gfs_ens
  integer(i_kind) i_en_perts_io
  integer(i_kind) n_ens,nlon_ens,nlat_ens,jcap_ens,jcap_ens_test
  integer(i_kind) n_ens_gfs,n_ens_fv3sar
  real(r_kind) weight_ens_gfs,weight_ens_fv3sar
  real(r_kind) beta_s0,beta_e0,grid_ratio_ens
  integer(i_kind),parameter::max_naensloc=20
  integer(i_kind),parameter::max_nvars=100
  real(r_kind) s_ens_h(max_naensloc)
  real(r_kind) s_ens_v(max_naensloc)
  type(sub2grid_info),save :: grd_ens,grd_loc,grd_sploc,grd_anl,grd_e1,grd_a1
  type(spec_vars),save :: sp_ens,sp_loc
  type(egrid2agrid_parm),save :: p_e2a,p_sploc2ens
  real(r_kind),allocatable,dimension(:,:) :: s_ens_vv
  real(r_kind),allocatable,dimension(:,:) :: s_ens_hv
  real(r_kind),allocatable,dimension(:) :: sqrt_beta_s,sqrt_beta_e
  real(r_kind),allocatable,dimension(:) :: beta_s,beta_e
  real(r_kind),allocatable,dimension(:,:,:) :: pwgt
!    nval_lenz_en is total length of ensemble extended control variable for sqrt
!    minimization mode
!NOTE:   for sqrt minimization, nval_lenz_en =
!nhoriz*(grd_loc%kend_alloc-grd_loc%kbegin_loc+1)
!      and nhoriz = grd_loc%nlat*grd_loc%nlon for regional,
!          nhoriz = (sp_loc%jcap+1)*(sp_loc%jcap+2) for global
  integer(i_kind) nval_lenz_en
  integer(i_kind) ntlevs_ens
  integer(i_kind) regional_ensemble_option
  integer(i_kind) fv3sar_ensemble_opt 
  character(len=512),save :: ensemble_path
  real(r_kind),allocatable,dimension(:,:) :: alphacvarsclgrpmat
  integer(i_kind),allocatable,dimension(:,:,:) :: ensgrp2aensgrp
  real(r_kind),allocatable,dimension(:) :: ensloccov4tim,ensloccov4var,ensloccov4scl
  integer(i_kind) :: nsclgrp=1
  integer(i_kind) :: naensgrp=1
  integer(i_kind) :: ntotensgrp=1
  integer(i_kind) :: naensloc=1
  integer(i_kind) :: ngvarloc=1
  real(r_kind) :: r_ensloccov4tim
  real(r_kind) :: r_ensloccov4var
  real(r_kind) :: r_ensloccov4scl
  integer(i_kind),allocatable,dimension(:) :: idaen3d,idaen2d

  real(r_kind),allocatable,dimension(:,:) :: spc_multwgt
  real(r_kind),allocatable,dimension(:,:) :: spcwgt_params
  character(len=3) vloc_varlist(max_naensloc,max_nvars)
  integer(i_kind) vdl_scale(max_naensloc)
  logical :: global_spectral_filter_sd
  logical :: assign_vdl_nml

! following is for storage of ensemble perturbations:

!   def en_perts            - array of ensemble perturbations
!   def nelen               - length of one ensemble perturbation vector

  integer(i_kind) nelen
  type(gsi_bundle),save,allocatable :: en_perts(:,:,:)
  real(r_single),dimension(:,:,:),allocatable:: ps_bar
  real(r_single):: limqens

!    following is for interpolation of global ensemble to regional ensemble grid

  real(r_kind),allocatable:: region_lat_ens(:,:),region_lon_ens(:,:)
  real(r_kind),allocatable:: region_dx_ens(:,:),region_dy_ens(:,:)


contains

subroutine init_hybrid_ensemble_parameters
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_hybrid_ensemble_parameters
!   prgmmr: parrish
!
! abstract: initialize hybird 3dvar hybrid ensemble parameters
!
! program history log:
!   
!   2010-01-13  lueken - added subprogram doc block
!   12-05-2012  el akkraoui - hybrid beta parameters now vertically varying
!   2014-09-15  carley - moved the init of variables sqrt_beta_s, sqrt_beta_e, and
!                         pwgt, to routine  create_hybens_localization_parameters.
!                         Otherwise these variables were referenced prior to
!                         memory allocation. 
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: one
  implicit none

  l_hyb_ens=.false.
  l_timloc_opt=.false.
  l_mgbf_loc=.false.
  full_ensemble=.false.
  pwgtflg=.false.
  uv_hyb_ens=.false.
  q_hyb_ens=.false.
  oz_univ_static=.false.
  sst_staticB=.true.
  aniso_a_en=.false.
  generate_ens=.true.
  pseudo_hybens=.false.
  merge_two_grid_ensperts=.false.
  regional_ensemble_option=0
  fv3sar_ensemble_opt=0
  write_ens_sprd=.false.
  readin_localization=.false.
  readin_beta=.false.
  use_localization_grid=.false.
  use_gfs_ens=.true.         ! when global: default is to read ensemble from GFS
  eqspace_ensgrid=.false.
  vvlocal=.false.
  l_ens_in_diff_time=.false.
  n_ens=0
  nlat_ens=0
  jcap_ens=0
  jcap_ens_test=0
  nlon_ens=0
  beta_s0=one
  beta_e0=-one
  grid_ratio_ens=one
  s_ens_h = 2828._r_kind     !  km (this was optimal value in 
                             !   Wang, X.,D. M. Barker, C. Snyder, and T. M. Hamill, 2008: A hybrid
                             !      ETKF.3DVAR data assimilation scheme for the WRF Model. Part II: 
                             !      Observing system simulation experiment. Mon.  Wea. Rev., 136, 5132-5147.)

  s_ens_v = 30._r_kind       ! grid units
  nval_lenz_en=-1            ! initialize dimension to absurd value
  ntlevs_ens=1               ! default for number of time levels for ensemble perturbations
  i_en_perts_io=0            ! default for en_pert IO. 0 is no IO
  ensemble_path = './'       ! default for path to ensemble members
  ens_fast_read=.false.
  parallelization_over_ensmembers=.false.
  limqens=1.0_r_single       ! default for limiting ensemble RH (+/-)
  l_both_fv3sar_gfs_ens=.false.
  n_ens_gfs=0 
  n_ens_fv3sar=0
  weight_ens_gfs=one
  weight_ens_fv3sar=one
  r_ensloccov4tim=one
  r_ensloccov4var=one
  r_ensloccov4scl=one
  vdl_scale = 0
  vloc_varlist = 'aaa'
  global_spectral_filter_sd=.false.
  assign_vdl_nml=.false.

end subroutine init_hybrid_ensemble_parameters

subroutine create_hybens_localization_parameters
  use constants, only: one
  use constants, only: zero
  implicit none
  
  allocate( s_ens_hv(grd_ens%nsig,naensloc),s_ens_vv(grd_ens%nsig,naensloc) )
  allocate( beta_s(grd_ens%nsig),beta_e(grd_ens%nsig))
  allocate( sqrt_beta_s(grd_ens%nsig),sqrt_beta_e(grd_ens%nsig) )
  allocate( pwgt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig) )
  allocate( alphacvarsclgrpmat(naensgrp,naensgrp) )
  allocate( ensgrp2aensgrp(ntotensgrp,max_nvars,ntlevs_ens) )
  allocate( ensloccov4tim(ntlevs_ens),ensloccov4var(ngvarloc),ensloccov4scl(nsclgrp) )
  beta_s  =one
  beta_e  =zero
  sqrt_beta_s=one
  sqrt_beta_e=zero
  pwgt=zero
  alphacvarsclgrpmat=one
  ensgrp2aensgrp=1
  ensloccov4tim=one
  ensloccov4var=one
  ensloccov4scl=one
  
end subroutine create_hybens_localization_parameters

subroutine destroy_hybens_localization_parameters
  implicit none
  
  deallocate(s_ens_vv,s_ens_hv) 
  deallocate(beta_s,beta_e)
  deallocate(sqrt_beta_s,sqrt_beta_e,pwgt)
  deallocate(alphacvarsclgrpmat)
  deallocate(ensgrp2aensgrp)

end subroutine destroy_hybens_localization_parameters

end module hybrid_ensemble_parameters
