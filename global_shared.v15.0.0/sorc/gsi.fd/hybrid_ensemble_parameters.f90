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
!       J(x1,a) = .5*betas*x1_trans*B_inv*x1 + .5*betae*a_trans*A_inv*a + Jo(x,yobs)
!
!   where betas and betae are tuning parameters that are constrained (default) by
!     
!          1/betae = 1 - 1/betas   , and 0 <= betas_inv <= 1
!
!   (1/betas) = 1, then no influence from ensemble perturbations
!
!   (1/betas) = 0, then no influence from static background B

!   betas and beta2 are allowed to vary vertically.
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
!         x1 = (1/betas)*B*y1
!
!         a  = (1/betae)*A*b
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
!      beta1_inv:  value between 0 and 1, default = 1.0, 
!                  relative weight given to static background B when (readin_beta=.false.)
!                  when (readin_beta=.true.), the vertical weighting parameters are read from a file, instead of 
!                  being defined based on beta1_inv namelist or default value. 
!      s_ens_h:    horizontal localization correlation length (units of km), default = 2828.0
!      s_ens_v:    vertical localization correlation length (grid units), default = 30.0
!      generate_ens:  if .true., generate ensemble perturbations internally as random samples of background B.
!                       (used primarily for testing/debugging)
!                     if .false., read external ensemble perturbations (not active yet)
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
!     betaflg: if true, use vertical weighting function for beta1_inv and beta2_inv
!     coef_bw: fraction of weight given to the vertical boundaries when betaflg is true
!     pwgtflg: if true, vertical integration function for ensemble contribution on Psfc
!     full_ensemble: if true, first ensemble member perturbed on first guess
!                    if false, first member perturbed on ensemble mean as the rest of the menbers
!     grid_ratio_ens: ratio of ensemble grid resolution to analysis resolution (default value is 1)
!     enspreproc:      if .true., read in preprocessed ensemble members (in
!                      files already subsetted for subdomains on each task).
!     vvlocal:  logical variable, if .true., then horizontal localization length
!               function of z, default = .false. 
!     ensemble_path: path to ensemble members; default './'
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
!   2012-01-17  wu      - add switches and arrays for new options: full_ensemble,betaflg,pwgtflg
!   2012-01-17  parrish - add integer parameter regional_ensemble_option which currently takes values
!                              1-5.  See def below for details. 
!   2012-02-07  tong    - remove logical parameter gefs_in_regional and reduce regional_ensemble_option
!                         to 4 options.
!   12-05-2012  el akkraoui - revised comments related to change in vertically varying beta weights
!   2013-01-20  parrish - move initialization of beta1wgt, beta2wgt, pwgt to after allocation.
!   2013-11-22  kleist  - add option for q perturbations
!   2013-12-03  wu      - add parameter coef_bw for option:betaflg
!   2014-05-14  wu      - add logical variable vvlocal for vertically verying horizontal localization length in regional
!   2015-01-22  Hu      - add flag i_en_perts_io to control reading ensemble perturbation.
!   2015-02-11  Hu      - add flag l_ens_in_diff_time to force GSI hybrid use ensembles not available at analysis time
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
!   def beta1_inv           - 1/beta1, default weight given to static background error covariance
!                             =1, then ensemble information turned off
!                             =0, then static background turned off
!                            the weights are applied per vertical level such that : 
!                                        betas_inv(:) = beta1_inv     , vertically varying weights given to static B ; 
!                                        betae_inv(:) = 1 - beta1_inv , vertically varying weights given ensemble derived covariance.
!                            If (readin_beta) then betas_inv and betae_inv are read from a file and beta1_inv is not used.
!   def s_ens_h             - homogeneous isotropic horizontal ensemble localization scale (km)
!   def s_ens_v             - vertical localization scale (grid units for now)
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
!   def beta1wgt            - vertical weighting function for beta1_inv (regional)
!   def beta2wgt            - vertical weighting function for beta2_inv (regional)
!   def betaflg             - logical switch to use vertical weighting function for beta2_inv and beta1_inv (regional)
!   def coef_bw             - fraction of weight given to the vertical boundaries when betaflg is true
!   def pwgt                - vertical integration function for beta2_inv a_en on Psfc
!   def pwgtflg             - logical switch to use vertical integration function for ensemble contribution on Psfc
!   def grid_ratio_ens:     - ratio of ensemble grid resolution to analysis resolution (default value is 1)
!   def use_localization_grid - if true, then use extra lower res gaussian grid for horizontal localization
!                                   (global runs only--allows possiblity for non-gaussian ensemble grid)
!   def enspreproc           - flag to read (.true.) already subsetted ensemble data.
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
  public :: uv_hyb_ens,q_hyb_ens,s_ens_v,beta1_inv,aniso_a_en,s_ens_hv,s_ens_vv
  public :: readin_beta,betas_inv,betae_inv
  public :: readin_localization
  public :: eqspace_ensgrid,grid_ratio_ens
  public :: beta1wgt,beta2wgt,pwgt,full_ensemble,pwgtflg,betaflg,coef_bw
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
  public :: write_ens_sprd
  public :: nval_lenz_en
  public :: ntlevs_ens
  public :: enspreproc
  public :: i_en_perts_io
  public :: l_ens_in_diff_time
  public :: ensemble_path
  public :: nelen
  public :: en_perts,ps_bar
  public :: region_lat_ens,region_lon_ens
  public :: region_dx_ens,region_dy_ens

  logical l_hyb_ens,uv_hyb_ens,q_hyb_ens,oz_univ_static
  logical enspreproc
  logical aniso_a_en
  logical full_ensemble,pwgtflg,betaflg
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
  integer(i_kind) i_en_perts_io
  integer(i_kind) n_ens,nlon_ens,nlat_ens,jcap_ens,jcap_ens_test
  real(r_kind) beta1_inv,s_ens_h,s_ens_v,grid_ratio_ens,coef_bw
  type(sub2grid_info),save :: grd_ens,grd_loc,grd_sploc,grd_anl,grd_e1,grd_a1
  type(spec_vars),save :: sp_ens,sp_loc
  type(egrid2agrid_parm),save :: p_e2a,p_sploc2ens
  real(r_kind),allocatable,dimension(:) :: s_ens_hv,s_ens_vv
  real(r_kind),allocatable,dimension(:) :: beta1wgt,beta2wgt
  real(r_kind),allocatable,dimension(:) :: betas_inv,betae_inv
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
  character(len=512),save :: ensemble_path

! following is for storage of ensemble perturbations:

!   def en_perts            - array of ensemble perturbations
!   def nelen               - length of one ensemble perturbation vector

  integer(i_kind) nelen
  type(gsi_bundle),save,allocatable :: en_perts(:,:)
  real(r_single),dimension(:,:,:),allocatable:: ps_bar

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
!   2014-09-15  carley - moved the init of variables beta1wgt, beta2wgt, and
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
  full_ensemble=.false.
  pwgtflg=.false.
  betaflg=.false.
  uv_hyb_ens=.false.
  q_hyb_ens=.false.
  oz_univ_static=.false.
  aniso_a_en=.false.
  generate_ens=.true.
  pseudo_hybens=.false.
  merge_two_grid_ensperts=.false.
  regional_ensemble_option=0
  write_ens_sprd=.false.
  readin_localization=.false.
  readin_beta=.false.
  use_localization_grid=.false.
  use_gfs_ens=.true.         ! when global: default is to read ensemble from GFS
  eqspace_ensgrid=.false.
  enspreproc=.false.
  vvlocal=.false.
  l_ens_in_diff_time=.false.
  coef_bw=0.9_r_kind
  n_ens=0
  nlat_ens=0
  jcap_ens=0
  jcap_ens_test=0
  nlon_ens=0
  beta1_inv=one
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

end subroutine init_hybrid_ensemble_parameters

subroutine create_hybens_localization_parameters
  use constants, only: one
  use constants, only: zero
  implicit none
  
  allocate( s_ens_hv(grd_ens%nsig),s_ens_vv(grd_ens%nsig) )
  allocate( betas_inv(grd_ens%nsig),betae_inv(grd_ens%nsig))
  allocate( beta1wgt(grd_ens%nsig),beta2wgt(grd_ens%nsig),pwgt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig) )
  beta1wgt=one
  beta2wgt=zero
  pwgt=zero
  
end subroutine create_hybens_localization_parameters

subroutine destroy_hybens_localization_parameters
  implicit none
  
  deallocate(s_ens_vv,s_ens_hv) 
  deallocate(betas_inv,betae_inv)
  deallocate(beta1wgt,beta2wgt,pwgt)

end subroutine destroy_hybens_localization_parameters

end module hybrid_ensemble_parameters
