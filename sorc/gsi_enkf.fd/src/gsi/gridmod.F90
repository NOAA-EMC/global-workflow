!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  gridmod --- GSI grid related variable declarations
!
! !INTERFACE:
!
module gridmod

! !USES:

  use kinds, only: i_byte,r_kind,r_single,i_kind
  use general_specmod, only: spec_vars,general_init_spec_vars,general_destroy_spec_vars
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use omp_lib, only: omp_get_max_threads
  use mpeu_util, only: die
  implicit none

! !DESCRIPTION: module containing grid related variable declarations
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2003-xx-xx  parrish,wu  regional components added
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-12-23  treadon - add routines get_ij and get_ijk
!   2005-01-20  okamoto - add nsig5p1
!   2005-03-07  dee     - add gmao_intfc option for gmao interface
!   2005-05-24  pondeca - regional surface component added
!   2005-06-01  treadon - add variables msig and array nlayers
!   2005-09-28  derber  - put grid calculations into get_ij and get_ijk
!   2006-01-09  derber  - add sigsum
!   2006-02-01  parrish - correct error to dx_an, dy_an when using filled_grid
!   2006-04-14  treadon - remove global sigi,sigl; add ntracter,ncloud,ck5
!   2006-04-17  treadon - remove regional sigi_ll,sigl_ll
!   2006-10-17  kleist  - add lat dependent coriolis parameter
!   2007-05-07  treadon - add ncepgfs_head(v)
!   2007-05-08  kleist  - add variables for fully generalized vertical coordinate
!   2007-10-24  parrish - fix error in wind rotation reference angle field
!   2009-01-28  todling - remove original GMAO interface
!   2009-01-09  gayno   - added variables lpl_gfs and dx_gfs
!   2010-03-06  parrish - add logical flag use_gfs_ozone for option to read gfs ozone for regional run
!   2010-03-09  parrish - add logical flag check_gfs_ozone_date--if true, date check against analysis time
!   2010-03-10  lueken  - remove hires_b variables, section, and subroutines
!   2010-03-15  parrish - add logical flag regional_ozone to turn on ozone in regional analysis
!   2010-03-10  zhu     - make variable vlevs public and general
!   2010-03-30  treadon - move jcap, jcap_b, hires_b, and spectral transform initialization and
!                         destroy from specmod to gridmod; add grd_a and grd_b structures
!   2010-04-01  treadon - move routines reorder, reorder2, strip_single, strip,
!                         vectosub, reload, and strip_periodic from mpimod to gridmod
!   2010-07-19  lueken  - make required changes to use general_deter_subdomain
!   2010-08-10  wu      - add number of types of vegetation for regional: nvege_type
!   2010-09-08  parrish - introduce new more robust method for computing reference wind rotation angle
!                           field, as represented on the analysis grid by cos_beta_ref and sin_beta_ref.
!                           This corrects an error in the reference angle for analysis grids in the
!                           tropics or southern hemisphere.
!   2010-09-08  parrish - remove subroutine check_rotate_wind.  This was a debug routine introduced when
!                           the reference wind rotation angle was stored as an angle, beta_ref.  This field
!                           had a discontinuity at the date line (180E), which resulted in erroneous wind
!                           rotation angles for a small number of winds whose rotation angle was interpolated
!                           from beta_ref values across the discontinuity.  This was fixed by replacing the
!                           beta_ref field with cos_beta_ref, sin_beta_ref.
!   2010-10-18  hcHuang - add flag use_gfs_nemsio to determine whether to use NEMSIO to read global first guess field
!   2010-10-19  parrish - correct bug in subroutine init_reg_glob_ll.  When running with
!                           wrf_nmm_regional=.true. and filled_grid=.true., all obs get tossed.  This was
!                           fixed by replacing region_lat and region_lon with glat_an, glon_an
!                           at lines 1068-1069, "call rpolar2ll" and removing the following do loop which
!                           copies region_lat, region_lon to glat_an, glon_an.
!   2010-11-03  derber  - added initialization of threading j loops for use in calctends
!   2010-11-14 pagowski - added CMAQ
!   2011-04-07 todling  - create/destroy_mapping no longer public; add final_grid_vars
!   2011-11-14 whitaker - added a fix to sign_pole for large domain (rlat0max > 37N and rlat0min < 37S)
!   2012-01-24 parrish  - correct bug in definition of region_dx, region_dy.
!   2013-05-14 guo      - added "only" declaration to "use omp_lib", and removed
!                         a redundant "use omp_lib".
!   2013-10-24 todling  - general interface to strip routine
!                       - move vars ltosj/i to commvars and corresponding load routines
!   2012-12-04 s.liu    - added use_reflectivity flag
!   2014-03-12  Hu     - Code for GSI analysis on Mass grid larger than background mass grid   
!   08-18-2014 tong      add jcap_gfs, nlon_gfs, nlat_gfs for regional analysis,
!                        when running with use_gfs_ozone = .true. or use_gfs_stratosphere = .true.,
!                        to allow spectral to grid transformation to a lower resolution grid
!   2015-02-03 todling - update max nlayers to 200
!   2016-03-02  s.liu/carley - remove use_reflectivity and use i_gsdcldanal_type
!   2017-03-23  Hu      - add code to get eta2_ll and aeta2_ll ready for hybrid vertical coodinate in WRF MASS CORE
!   2017-08-31  Li      - add sfcnst_comb to handle surface and nsst combined file
!   2018-02-15  wu      - add fv3_regional & grid_ratio_fv3_regional
!   2019-03-05  martin  - add wgtfactlats for factqmin/factqmax scaling
!   2019-04-19  martin  - add use_fv3_aero option to distingiush between NGAC and FV3-Chem
!   2019-09-04  martin  - add write_fv3_incr to write netCDF increment rather than analysis in NEMSIO format
!   2019-09-23  martin  - add use_gfs_ncio to read global first guess from netCDF file
!   2020-12-18  Hu      - add grid_type_fv3_regional
!   2021-12-30  Hu      - add fv3_io_layout_y
!   2022-03-01  X.Lu & X.Wang - add corresponding variables for dual ens for HAFS. POC: xuguang.wang@ou.edu
!
!                        
!
!
! !AUTHOR: 
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------

! set default to private
  private
! set subroutines to public
  public :: init_grid
  public :: init_grid_vars
  public :: final_grid_vars
  public :: init_subdomain_vars
  public :: create_grid_vars
  public :: destroy_grid_vars
  public :: init_reg_glob_ll
  public :: init_general_transform
  public :: tll2xy
  public :: txy2ll
  public :: nearest_3
  public :: get_xytilde_domain
  public :: half_nmm_grid2a
  public :: fill_nmm_grid2a3
  public :: rotate_wind_ll2xy
  public :: rotate_wind_xy2ll
  public :: get_ij
  public :: get_ijk
  public :: reorder
  public :: reorder2
  public :: strip
  public :: vectosub
  public :: reload
  public :: strip_periodic
  public :: minmype

! set passed variables to public
  public :: nnnn1o,iglobal,itotsub,ijn,ijn_s,lat2,lon2,lat1,lon1,nsig,nsig_soil
  public :: ncloud,nlat,nlon,ntracer,displs_s,displs_g
  public :: bk5,regional,latlon11,latlon1n,twodvar_regional
  public :: netcdf,nems_nmmb_regional,wrf_mass_regional,wrf_nmm_regional,cmaq_regional
  public :: aeta2_ll,pdtop_ll,pt_ll,eta1_ll,eta2_ll,aeta1_ll,idsl5,ck5,ak5
  public :: tref5,idvc5,nlayers,msig,jstart,istart,region_lat,vlevs,nsig1o,rlats
  public :: region_dy,region_dx,region_lon,rlat_min_dd,rlat_max_dd,rlon_max_dd
  public :: rlon_min_dd,coslon,sinlon,rlons,ird_s,irc_s,periodic,idthrm5
  public :: cp5,idvm5,ncepgfs_head,idpsfc5,nlon_sfc,nlat_sfc
  public :: rlons_sfc,rlats_sfc,jlon1,ilat1,periodic_s,latlon1n1
  public :: regional_fhr,region_dyi,coeffx,region_dxi,coeffy,nsig_hlf,regional_fmin
  public :: nsig2,wgtlats,corlats,rbs2,ncepgfs_headv,regional_time,wgtfactlats
  public :: nlat_regional,nlon_regional,update_regsfc,half_grid,gencode
  public :: nlat_regionalens,nlon_regionalens
  public :: diagnostic_reg,nmmb_reference_grid,filled_grid
  public :: grid_ratio_nmmb,isd_g,isc_g,dx_gfs,lpl_gfs,nsig5,nmmb_verttype
  public :: grid_ratio_fv3_regional,fv3_io_layout_y,fv3_regional,fv3_cmaq_regional,grid_type_fv3_regional
  public :: l_reg_update_hydro_delz
  public :: nsig3,nsig4,grid_ratio_wrfmass
  public :: use_gfs_ozone,check_gfs_ozone_date,regional_ozone,nvege_type
  public :: jcap,jcap_b,hires_b,sp_a,grd_a
  public :: jtstart,jtstop,nthreads
  public :: use_gfs_nemsio
  public :: use_gfs_ncio
  public :: fv3_full_hydro  
  public :: use_fv3_aero
  public :: sfcnst_comb
  public :: use_readin_anl_sfcmask
  public :: jcap_gfs,nlat_gfs,nlon_gfs
  public :: use_sp_eqspace,jcap_cut
  public :: wrf_mass_hybridcord
  public :: write_fv3_incr

  interface strip
     module procedure strip_single_rank33_
     module procedure strip_single_rank21_
     module procedure strip_double_rank33_
     module procedure strip_double_rank22_
     module procedure strip_double_rank32_
     module procedure strip_double_rank21_
     module procedure strip_double_rank11_
  end interface

  logical regional          ! .t. for regional background/analysis
  logical diagnostic_reg    ! .t. to activate regional analysis diagnostics

  logical wrf_nmm_regional  !
  logical fv3_regional      ! .t. to run with fv3 regional model
  logical fv3_cmaq_regional ! .t. to run with fv3_cmaq_regional model
  logical l_reg_update_hydro_delz  ! .true. to update delz in fv3 model
  logical nems_nmmb_regional! .t. to run with NEMS NMMB model
  logical wrf_mass_regional !
  logical wrf_mass_hybridcord
  logical cmaq_regional     ! .t. to run with cmaq
  logical twodvar_regional  ! .t. to run code in regional 2D-var mode
  logical use_gfs_ozone     ! .t. to use gfs ozone in regional analysis
  logical check_gfs_ozone_date ! .t. to date check gfs ozone against regional
  logical regional_ozone    !    .t. to turn on ozone for regional analysis
  logical netcdf            ! .t. for regional netcdf i/o

  logical filled_grid       ! 
  logical half_grid         !
  logical update_regsfc     !
  logical hires_b           ! .t. when jcap_b requires double FFT
  logical use_gfs_nemsio    ! .t. for using NEMSIO to real global first guess
  logical use_gfs_ncio      ! .t. for using netCDF to real global first guess
  logical fv3_full_hydro    ! .t. for using NEMSIO to real global first guess
  logical use_fv3_aero      ! .t. for using FV3 Aerosols, .f. for NGAC
  logical sfcnst_comb       ! .t. for using combined sfc & nst file
  logical use_sp_eqspace    ! .t. use equally-space grid in spectral transforms
  logical write_fv3_incr    ! .t. write netCDF increment rather than NEMSIO analysis

  logical use_readin_anl_sfcmask        ! .t. for using readin surface mask
  character(1) nmmb_reference_grid      ! ='H': use nmmb H grid as reference for analysis grid
                                        ! ='V': use nmmb V grid as reference for analysis grid
  real(r_kind) grid_ratio_fv3_regional  ! ratio of analysis grid to fv3 model grid in fv3 grid units.
  integer(i_kind) fv3_io_layout_y       ! = io_layout(2) of fv3 regional model (subdomain y direction).
  integer(i_kind) grid_type_fv3_regional! type of fv3 model grid (grid orientation).
  real(r_kind) grid_ratio_nmmb ! ratio of analysis grid to nmmb model grid in nmmb model grid units.
  real(r_kind) grid_ratio_wrfmass ! ratio of analysis grid to wrf model grid in wrf mass grid units.
  character(3) nmmb_verttype   !   'OLD' for old vertical coordinate definition
                               !                old def: p = eta1*pdtop+eta2*(psfc-pdtop-ptop)+ptop
                               !   'NEW' for new vertical coordinate definition
                               !                new def: p = eta1*pdtop+eta2*(psfc-ptop)+ptop

  integer(i_kind) vlevs             ! no. of levels distributed on all processors
  integer(i_kind) nsig1o            ! max no. of levels distributed on each processor
  integer(i_kind) nnnn1o            ! actual of levels distributed on current processor
  integer(i_kind) nlat              ! no. of latitudes
  integer(i_kind) nlon              ! no. of longitudes
  integer(i_kind) nlat_sfc          ! no. of latitudes surface files
  integer(i_kind) nlon_sfc          ! no. of longitudes surface files
  integer(i_kind) nsig              ! no. of levels
  integer(i_kind) nsig_soil         ! no. of levels of soil model
  integer(i_kind) idvc5             ! vertical coordinate identifier
  integer(i_kind) nvege_type        ! no. of types of vegetation; old=24, IGBP=20
!                                        1: sigma
!                                        2: sigma-pressure
!                                        3: sigma-pressure-theta
  integer(i_kind) idvm5             
  integer(i_kind) idpsfc5           ! surface pressure identifier
!                                      0/1: ln(ps)
!                                        2: ps
  integer(i_kind) idthrm5           ! thermodynamic variable identifier
!                                      0/1: virtual temperature
!                                        2: sensible temperature
!                                        3: enthalpy (CpT)
  integer(i_kind) idsl5             ! midlayer pressure definition
!                                        1: Philips
!                                        2: average
  integer(i_kind) nsig2             ! 2 times number of levels
  integer(i_kind) nsig3             ! 3 times number of levels
  integer(i_kind) nsig4             ! 4 times number of levels
  integer(i_kind) nsig5             ! 5 times number of levels
  integer(i_kind) nsig5p1           ! 5 times number of levels plus 1
  integer(i_kind) nsig_hlf          ! half number of levels

  integer(i_kind) ntracer           ! number of tracers
  integer(i_kind) ncloud            ! number of cloud types

  integer(i_kind) ns1               ! 2 times number of levels plus 1
  integer(i_kind) lat1              ! no. of lats on subdomain (no buffer)
  integer(i_kind) lon1              ! no. of lons on subdomain (no buffer)
  integer(i_kind) lat2              ! no. of lats on subdomain (buffer points on ends)
  integer(i_kind) lon2              ! no. of lons on subdomain (buffer points on ends)
  integer(i_kind) latlon11          ! horizontal points in subdomain (with buffer)
  integer(i_kind) latlon1n          ! no. of points in subdomain (with buffer)
  integer(i_kind) latlon1n1         ! no. of points in subdomain for 3d prs (with buffer)
  integer(i_kind) iglobal           ! number of horizontal points on global grid
  integer(i_kind) itotsub           ! number of horizontal points of all subdomains combined
  integer(i_kind) msig              ! number of profile layers to use when calling RTM

  integer(i_kind) jcap_cut          ! spectral triangular truncation beyond which you recalculate pln and plntop - default 600 - used to save memory
  integer(i_kind) jcap              ! spectral triangular truncation of ncep global analysis
  integer(i_kind) jcap_b            ! spectral triangular truncation of ncep global background
  integer(i_kind) nthreads          ! number of threads used (currently only used in calctends routines)
  integer(i_kind) minmype           ! processor with minimum size subdomain


  logical periodic                              ! logical flag for periodic e/w domains
  logical,allocatable,dimension(:):: periodic_s ! logical flag for periodic e/w subdomain (all tasks)

  integer(i_kind),allocatable,dimension(:):: lpl_gfs ! number grid points for each row, GFS grid
  integer(i_kind),allocatable,dimension(:):: jstart  ! start lon of the whole array on each pe
  integer(i_kind),allocatable,dimension(:):: istart  ! start lat of the whole array on each pe
  integer(i_kind),allocatable,dimension(:):: ilat1   ! no. of lats for each subdomain (no buffer)
  integer(i_kind),allocatable,dimension(:):: jlon1   ! no. of lons for each subdomain (no buffer)
  integer(i_kind),allocatable,dimension(:):: ijn_s   ! no. of horiz. points for each subdomain (with buffer)
  integer(i_kind),allocatable,dimension(:):: ijn     ! no. of horiz. points for each subdomain (no buffer)
  integer(i_kind),allocatable,dimension(:):: isc_g   ! no. array, count for send to global; size of subdomain

                                               ! comm. array ...
  integer(i_kind),allocatable,dimension(:):: irc_s     !   count for receive on subdomain
  integer(i_kind),allocatable,dimension(:):: ird_s     !   displacement for receive on subdomain
  integer(i_kind),allocatable,dimension(:):: isd_g     !   displacement for send to global
  integer(i_kind),allocatable,dimension(:):: displs_s  !   displacement for send from subdomain
  integer(i_kind),allocatable,dimension(:):: displs_g  !   displacement for receive on global grid

  integer(i_kind),dimension(200):: nlayers        ! number of RTM layers per model layer
                                                  ! (k=1 is near surface layer), default is 1
  integer(i_kind), allocatable, dimension(:)::  jtstart,jtstop ! starting and ending indicies for j threading


  real(r_kind) gencode

  real(r_kind),allocatable,dimension(:):: dx_gfs  ! resolution of GFS grid in degrees
  real(r_kind),allocatable,dimension(:):: rlats   ! grid latitudes (radians)
  real(r_kind),allocatable,dimension(:):: rlons   ! grid longitudes (radians)
  real(r_kind),allocatable,dimension(:):: rlats_sfc   ! grid latitudes (radians) surface
  real(r_kind),allocatable,dimension(:):: rlons_sfc   ! grid longitudes (radians) surface
  real(r_kind),allocatable,dimension(:):: ak5,bk5,ck5,tref5 ! coefficients for generalized vertical coordinate
  real(r_kind),allocatable,dimension(:):: cp5     ! specific heat for tracers
  real(r_kind),allocatable,dimension(:):: coslon  ! cos(grid longitudes (radians))
  real(r_kind),allocatable,dimension(:):: sinlon  ! sin(grid longitudes (radians))
  real(r_kind),allocatable,dimension(:):: wgtlats !  gaussian integration weights
  real(r_kind),allocatable,dimension(:):: wgtfactlats !  gaussian integration weights if global, 1 if regional
  real(r_kind),allocatable,dimension(:):: corlats ! coriolis parameter by latitude
  real(r_kind),allocatable,dimension(:):: rbs2    ! 1./sin(grid latitudes))**2

! additional variables for regional mode
  real(r_kind),allocatable::  eta1_ll(:)          !
  real(r_kind),allocatable:: aeta1_ll(:)          !
  real(r_kind),allocatable::  eta2_ll(:)          !
  real(r_kind),allocatable:: aeta2_ll(:)          !
  real(r_kind),allocatable::region_lon(:,:)       !
  real(r_kind),allocatable::region_lat(:,:)       !
  real(r_kind),allocatable::region_dx(:,:)        !
  real(r_kind),allocatable::region_dy(:,:)        !
  real(r_kind),allocatable::region_dxi(:,:)       !
  real(r_kind),allocatable::region_dyi(:,:)       !
  real(r_kind),allocatable::coeffx(:,:)           !
  real(r_kind),allocatable::coeffy(:,:)           !

  real(r_kind) rlon_min_ll,rlon_max_ll,rlat_min_ll,rlat_max_ll
  real(r_kind) rlon_min_dd,rlon_max_dd,rlat_min_dd,rlat_max_dd
  real(r_kind) dt_ll,pdtop_ll,pt_ll

  integer(i_kind) nlon_regional,nlat_regional,nlon_regionalens,nlat_regionalens
  real(r_kind) regional_fhr,regional_fmin
  integer(i_kind) regional_time(6)
  integer(i_kind) jcap_gfs,nlat_gfs,nlon_gfs

! The following is for the generalized transform
  real(r_kind) pihalf,sign_pole,rlambda0
  real(r_kind) atilde_x,btilde_x,atilde_y,btilde_y
  real(r_kind) btilde_xinv,btilde_yinv
  integer(i_kind) nxtilde,nytilde
  real(r_kind),allocatable::xtilde0(:,:),ytilde0(:,:)
  real(r_kind),allocatable::cos_beta_ref(:,:),sin_beta_ref(:,:)
  integer(i_kind),allocatable::i0_tilde(:,:),j0_tilde(:,:)
  integer(i_byte),allocatable::ip_tilde(:,:),jp_tilde(:,:)

! Define structure to hold NCEP sigio header information
  type:: ncepgfs_head
     integer(i_kind):: ivs
     integer(i_kind):: version
     real(r_single) :: fhour
     integer(i_kind):: idate(4)
     integer(i_kind):: nrec
     integer(i_kind):: latb
     integer(i_kind):: lonb
     integer(i_kind):: levs
     integer(i_kind):: jcap
     integer(i_kind):: itrun
     integer(i_kind):: iorder
     integer(i_kind):: irealf
     integer(i_kind):: igen
     integer(i_kind):: latf
     integer(i_kind):: lonf
     integer(i_kind):: latr
     integer(i_kind):: lonr
     integer(i_kind):: ntrac
     integer(i_kind):: icen2
     integer(i_kind):: iens(2)
     integer(i_kind):: idpp
     integer(i_kind):: idsl
     integer(i_kind):: idvc
     integer(i_kind):: idvm
     integer(i_kind):: idvt
     integer(i_kind):: idrun
     integer(i_kind):: idusr
     real(r_single) :: pdryini
     integer(i_kind):: ncldt
     integer(i_kind):: ixgr
     integer(i_kind):: nvcoord
     integer(i_kind):: idrt
  end type ncepgfs_head

  type:: ncepgfs_headv
     real(r_single),allocatable:: vcoord(:,:)
     real(r_single),allocatable:: cpi(:)    
  end type ncepgfs_headv

  type(spec_vars),save:: sp_a
  type(sub2grid_info),save:: grd_a

  character(len=*),parameter::myname='gridmod'
contains
   
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  init_grid --- Initialize defaults for grid related variables
!
! !INTERFACE:
!
  subroutine init_grid

! !DESCRIPTION: initialize defaults for grid related variables
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-03-03  treadon - add implicit none
!   2005-06-01  treadon - add initialization of msig and nlayers
!   2010-03-06  parrish - add initialization of use_gfs_ozone flag
!   2010-03-09  parrish - add initialization of check_gfs_ozone_date flag
!   2010-03-15  parrish - add initialization of regional_ozone flag
!   2010-08-10  wu      - add initialization of nvege_type          
!   2010-10-14  pagowski- add CMAQ
!   2010-10-18  hcHuang - add flag use_gfs_nemsio to determine whether to use NEMSIO to read global first guess field
!   2011-09-14  todling - add use_sp_eqspace to better control lat/lon grid case
!   2016-08-28       li - tic591: add use_readin_anl_sfcmask for consistent sfcmask
!                         between analysis grids and others
!   2019-04-19  martin  - add use_fv3_aero option for NGAC vs FV3-Chem
!   2019-09-23  martin  - add flag use_gfs_ncio to determine whether to use netCDF to read global first gues field
!   2021-01-05  x.zhang/lei  - add code for updating delz analysis in regional da
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    use constants, only: one,two
    use gsi_io, only: verbose
    implicit none

    integer(i_kind) k

    nsig = 42
    nsig_soil = 6
    nsig1o = 7
    nlat = 96
    nlon = 384
    idvc5 = 1
    idvm5 = 0
    idpsfc5 = 1
    idthrm5 = 1
    idsl5 = 1
    ntracer = 1
    ncloud = 0
    gencode = 80
    regional = .false.
    periodic = .false.
    wrf_nmm_regional = .false.
    wrf_mass_regional = .false.
    wrf_mass_hybridcord = .false.
    cmaq_regional=.false.
    fv3_regional=.false.
    fv3_cmaq_regional=.false.
    l_reg_update_hydro_delz=.false.
    nems_nmmb_regional = .false.
    twodvar_regional = .false. 
    use_gfs_ozone = .false.
    check_gfs_ozone_date = .false.
    regional_ozone = .false.
    netcdf = .false.
    filled_grid = .false.
    half_grid = .false.
    grid_ratio_fv3_regional = one
    fv3_io_layout_y = 1
    grid_type_fv3_regional = 0
    grid_ratio_nmmb = sqrt(two)
    grid_ratio_wrfmass = one
    nmmb_reference_grid = 'H'
    nmmb_verttype = 'OLD'
    lat1 = nlat
    lon1 = nlon
    lat2 = lat1+2
    lon2 = lon1+2

    diagnostic_reg = .false.
    if(verbose)diagnostic_reg = .true.
    update_regsfc = .false.
    nlon_regional = 0
    nlat_regional = 0
    nlon_regionalens = 0
    nlat_regionalens = 0

    msig = nsig
    do k=1,size(nlayers)
       nlayers(k) = 1
    end do

    jcap_cut=600
    jcap=62
    jcap_b=62
    hires_b=.false.
    ! -------------------------------------------------------!
    ! nvege_type set to 20 (i.e. default vege type is IGBP)  !
    ! Updated in EXP-crtm_sfc_intrface branch: June 16, 2016 !
    ! -------------------------------------------------------!
    nvege_type = 20
    nthreads = 1  ! initialize the number of threads

    use_gfs_nemsio  = .false.
    use_gfs_ncio = .false.
    fv3_full_hydro  = .false. 
    use_fv3_aero  = .false.
    sfcnst_comb = .false.
    use_readin_anl_sfcmask = .false.

    use_sp_eqspace = .false.

    jcap_gfs=1534
    nlat_gfs=1538
    nlon_gfs=3072

    return
  end subroutine init_grid
  
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  init_grid_vars --- Set grid related variables
!
! !INTERFACE:
!
  subroutine init_grid_vars(jcap,npe,cvars3d,cvars2d,cvars,mype)

! !USES:

    use mpeu_util, only: getindex
    use general_specmod, only: spec_cut
    use gsi_io, only: verbose
    use gsi_metguess_mod, only: gsi_metguess_get
    implicit none

! !INPUT PARAMETERS:

   integer(i_kind) ,intent(in  ) :: jcap   ! spectral truncation
   integer(i_kind) ,intent(in  ) :: npe    ! number of mpi tasks
   character(len=*),intent(in  ) :: cvars3d(:)
   character(len=*),intent(in  ) :: cvars2d(:)
   character(len=*),intent(in  ) :: cvars  (:)
   integer(i_kind) ,intent(in  ) :: mype   ! mpi task id

! !DESCRIPTION: set grid related variables (post namelist read)
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-06-01  treadon - add computation of msig
!   2010-03-15  zhu - add nrf3 and nvars for generalized control variable
!   2010-06-04  todling - revisit Zhu's general CV settings, and vector fields
!   2010-11-08  treadon - call create_mapping; perform init_subdomain_vars initializations
!   2012-15-04  todling - revisit call to general_init_spec_vars
!
!   input argument list:
!
!   output argument list:
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    character(len=*),parameter::myname_=myname//'*init_grid_vars'
    integer(i_kind) i,k,inner_vars,num_fields
    integer(i_kind) n3d,n2d,nvars,tid,nth
    integer(i_kind) ipsf,ipvp,jpsf,jpvp,isfb,isfe,ivpb,ivpe
    integer(i_kind) istatus,icw,iql,iqi
    integer(i_kind) icw_cv,iql_cv,iqi_cv,minmax
    logical,allocatable,dimension(:):: vector
    logical print_verbose

    print_verbose=.false. .and. mype == 0
    if(verbose .and. mype == 0)print_verbose=.true.
    spec_cut=jcap_cut
    if(jcap==62) gencode=80.0_r_kind
    ns1=2*nsig+1
    nsig2=2*nsig
    nsig3=3*nsig
    nsig4=4*nsig
    nsig5=5*nsig
    nsig5p1=5*nsig+1
    nsig_hlf=nsig/2
    iglobal=nlat*nlon

    n3d  =size(cvars3d)
    n2d  =size(cvars2d)
    nvars=size(cvars)

    icw_cv = getindex(cvars3d(1:n3d),'cw')
    iql_cv = getindex(cvars3d(1:n3d),'ql')
    iqi_cv = getindex(cvars3d(1:n3d),'qi')
    call gsi_metguess_get('var::cw', icw, istatus)
    call gsi_metguess_get('var::ql', iql, istatus)
    call gsi_metguess_get('var::qi', iqi, istatus)
    fv3_full_hydro = ( iql>0    .and. iqi>0    .and. (.not. icw>0)   ) .and. &
                     ( iql_cv>0 .and. iqi_cv>0 .and. (.not. icw_cv>0))

    if (mype==0) write(6,*) myname, ' fv3_full_hydro ', fv3_full_hydro

! Allocate and initialize variables for mapping between global
! domain and subdomains
    call create_mapping(npe)

! Initialize nsig1o to distribute levs/variables
! as evenly as possible over the tasks
    vlevs=(n3d*nsig)+nvars-n3d
    nsig1o=vlevs/npe
    if(mod(vlevs,npe)/=0) nsig1o=nsig1o+1
    nnnn1o=nsig1o                  ! temporarily set the number of levels to nsig1o

! Sum total number of vertical layers for RTM call
    msig = 0
    if(size(nlayers)<nsig) call die(myname_,'insufficient size of nlayers',99)
    do k=1,nsig
       msig = msig + nlayers(k)
    end do

! Initialize structure(s) for spectral <--> grid transforms
    if (.not.regional) then
!      Call general specmod for analysis grid
       call general_init_spec_vars(sp_a,jcap,jcap,nlat,nlon,eqspace=use_sp_eqspace)

       if (mype==0) &
            write(6,*) 'INIT_GRID_VARS:  allocate and load sp_a with jcap,imax,jmax=',&
            sp_a%jcap,sp_a%imax,sp_a%jmax
       
    endif

! Initialize structures for grid(s)
    inner_vars=1
    num_fields=n3d*nsig+n2d
    allocate(vector(num_fields))
    vector=.false.

! Find and flag vector fields (assumes motley at the end)
    ipsf = getindex(cvars(1:n3d+n2d),'sf')
    ipvp = getindex(cvars(1:n3d+n2d),'vp')
    if(ipsf>0.and.ipvp>0) then
!      The following assumes 3d-fields come first in CV

!      is it a 3d-vector field?
       jpsf = getindex(cvars3d,'sf')
       jpvp = getindex(cvars3d,'vp')
       if(jpsf>0.and.jpvp>0) then
          isfb=(ipsf-1)*nsig+1
          isfe= isfb+nsig-1
          vector(isfb:isfe)=.true.
          ivpb=(ipvp-1)*nsig+1
          ivpe= ivpb+nsig-1
          vector(ivpb:ivpe)=.true.
       endif
!      is it a 2d-vector field?
       jpsf = getindex(cvars2d,'sf')
       jpvp = getindex(cvars2d,'vp')
       if(jpsf>0.and.jpvp>0) then
          isfb= n3d*nsig+ipsf
          isfe= isfb
          vector(isfb:isfe)=.true.
          ivpb= n3d*nsig+ipvp
          ivpe= ivpb
          vector(ivpb:ivpe)=.true.
       endif

    endif
    call general_sub2grid_create_info(grd_a,inner_vars,nlat,nlon,nsig,num_fields, &
         regional,vector)
    deallocate(vector)

! Set values from grd_a to pertinent gridmod variables 
    lat1=grd_a%lat1 
    lat2=grd_a%lat2 
    lon1=grd_a%lon1 
    lon2=grd_a%lon2 

    latlon11 = lat2*lon2
    latlon1n = latlon11*nsig
    latlon1n1= latlon1n+latlon11

    periodic=grd_a%periodic

    minmype=0
    minmax=grd_a%ilat1(1)*grd_a%jlon1(1)
    do i=1,npe
       istart(i)    =grd_a%istart(i)
       jstart(i)    =grd_a%jstart(i)
       periodic_s(i)=grd_a%periodic_s(i)
       ilat1(i)     =grd_a%ilat1(i)
       jlon1(i)     =grd_a%jlon1(i)
       ijn_s(i)     =grd_a%ijn_s(i)
       irc_s(i)     =grd_a%irc_s(i)
       ird_s(i)     =grd_a%ird_s(i)
       displs_s(i)  =grd_a%displs_s(i)
       ijn(i)       =grd_a%ijn(i)
       displs_g(i)  =grd_a%displs_g(i)
       if(grd_a%ilat1(i)*grd_a%jlon1(i)< minmax)then
         minmax=grd_a%ilat1(i)*grd_a%jlon1(i)
         minmype=i-1
       end if
    end do
    if(mype == minmype) write(6,*) ' minmype = ',minmype

!#omp parallel private(nth,tid)
    nth = omp_get_max_threads()
!#omp end parallel
    nthreads=nth
    if(print_verbose)write(6,*) 'INIT_GRID_VARS:  number of threads ',nthreads
    allocate(jtstart(nthreads),jtstop(nthreads))
    do tid=1,nthreads
       call looplimits(tid-1, nthreads, 1, lon2, jtstart(tid), jtstop(tid))
       if(print_verbose)write(6,*)'INIT_GRID_VARS:  for thread ',tid,  &
            ' jtstart,jtstop = ',jtstart(tid),jtstop(tid)
    end do

    return

  end subroutine init_grid_vars

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: final_grid_vars --- Finalize grid related variables
!
! !INTERFACE:
!
  subroutine final_grid_vars

! !USES:

    implicit none

! !INPUT PARAMETERS:

! !DESCRIPTION: finalize grid related variables
!
! !REVISION HISTORY:
!   2011-04-07  todling - create for consistency with init_grid_vars
!
!   input argument list:
!
!   output argument list:
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   todling           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------

    call destroy_mapping

    deallocate(jtstart,jtstop)

    return

  end subroutine final_grid_vars

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  init_subdomain_vars --- Initialize variables related to subdomains
!
! !INTERFACE:
!
  subroutine init_subdomain_vars

! !DESCRIPTION: initialize variables related to subdomains
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-03-03  treadon - add implicit none
!   2008-11-28  todling - latlon1n1 (for 3d prs)
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    implicit none

    lat2 = lat1+2
    lon2 = lon1+2
    latlon11 = lat2*lon2
    latlon1n = latlon11*nsig
    latlon1n1= latlon1n+latlon11

    return
  end subroutine init_subdomain_vars

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_grid_vars --- Allocate memory for grid related variables
!
! !INTERFACE:
!
  subroutine create_grid_vars

! !DESCRIPTION: allocate memory for grid related variables
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-03-03  treadon - add implicit none
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    implicit none

    allocate(rlats(nlat),rlons(nlon),coslon(nlon),sinlon(nlon),&
             wgtlats(nlat),rbs2(nlat),corlats(nlat),wgtfactlats(nlat))
    allocate(ak5(nsig+1),bk5(nsig+1),ck5(nsig+1),tref5(nsig))
    return
  end subroutine create_grid_vars
    
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  destroy_grid_vars --- Deallocate memory for grid related variables
!
! !INTERFACE:
!
  subroutine destroy_grid_vars

! !DESCRIPTION: deallocate memory for grid related variables
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-03-03  treadon - add implicit none
!   2009-12-20  gayno - add variable lpl_gfs
!   2011-01-04  pagowski - deallocate regional grid arrays
!   2013-10-27  todling - move final_grid_vars call to where init takes place (gsimod)
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    implicit none

    deallocate(rlats,rlons,corlats,coslon,sinlon,wgtlats,wgtfactlats,rbs2)
    deallocate(ak5,bk5,ck5,tref5)
    if (allocated(cp5)) deallocate(cp5)
    if (allocated(dx_gfs)) deallocate(dx_gfs)
    if (allocated(lpl_gfs)) deallocate(lpl_gfs)
    if (allocated(region_lat)) deallocate(region_lat)    
    if (allocated(region_lon)) deallocate(region_lon)    
    if (allocated(region_dx)) deallocate(region_dx)
    if (allocated(region_dy)) deallocate(region_dy)
    if (allocated(region_dxi)) deallocate(region_dxi)
    if (allocated(region_dyi)) deallocate(region_dyi)    
    if (allocated(coeffx)) deallocate(coeffx)
    if (allocated(coeffy)) deallocate(coeffy)

    call general_destroy_spec_vars(sp_a)
    return
  end subroutine destroy_grid_vars

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_mapping --- Init vars mapping between global domain/subd.
!
! !INTERFACE:
!
  subroutine create_mapping(npe)

! !USES:

    implicit none

! !INPUT PARAMETERS:

    integer(i_kind),intent(in   ) :: npe   ! number of mpi tasks

! !DESCRIPTION: allocate and initialize variables that create mapping
!           between global domain and subdomains
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2010-11-08  treadon, move grd_a initialization to init_grid_vars
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    integer(i_kind) i

    allocate(periodic_s(npe),jstart(npe),istart(npe),&
         ilat1(npe),jlon1(npe),&
       ijn_s(npe),irc_s(npe),ird_s(npe),displs_s(npe),&
       ijn(npe),isc_g(npe),isd_g(npe),displs_g(npe))

    do i=1,npe
       periodic_s(i)= .false.
       jstart(i)    = 0
       istart(i)    = 0
       ilat1(i)     = 0
       jlon1(i)     = 0
       ijn_s(i)     = 0
       irc_s(i)     = 0
       ird_s(i)     = 0
       displs_s(i)  = 0
       ijn(i)       = 0
       isc_g(i)     = 0
       isd_g(i)     = 0
       displs_g(i)  = 0
    end do

    return
  end subroutine create_mapping
  
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  destroy_mapping --- Dealloc global/subdomain mapping arrays
!
! !INTERFACE:
!
  subroutine destroy_mapping

! !DESCRIPTION: deallocate memory for global/subdomain mapping variables
!
! !REVISION HISTORY:
!   2003-09-25  kleist
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-03-03  treadon - add implicit none
!   2007-02-20  todling - somehow dealloc for irc_s,ird_s got lost
!   2011-04-07  todling - move dealloc of jtstart,jtstop to final_grid_vars
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist           org: np20                date: 2003-09-25
!
!EOP
!-------------------------------------------------------------------------
    implicit none

    deallocate(periodic_s,jstart,istart,ilat1,jlon1,&
       ijn_s,irc_s,ird_s,displs_s,&
       ijn,isc_g,isd_g,displs_g)

    return
  end subroutine destroy_mapping


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  init_reg_glob_ll --- In case regional, initialize setting
!
! !INTERFACE:
!
  subroutine init_reg_glob_ll(mype,lendian_in)

! !USES:

    use constants, only: zero, one, three, deg2rad,half, two,r0_01
    use mod_nmmb_to_a, only: init_nmmb_to_a,nxa,nya,nmmb_h_to_a8,ratio_x,ratio_y
    use mod_wrfmass_to_a, only: init_wrfmass_to_a,nxa_wrfmass,nya_wrfmass
    use mod_wrfmass_to_a, only: wrfmass_h_to_a
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind), intent(in   ) :: mype          ! mpi task id
    integer(i_kind), intent(in   ) :: lendian_in    ! unit number reserved for
                                                    !  little endian input

! !DESCRIPTION: decide if regional run or not, and initialize constants 
!           required for rotation transformation
!
!
!   output argument list:
!
!   Notes about grid definition:
!   \begin{enumerate}
!   \item  The origin of the analysis coordinate system is always $rlon=180.$, $rlat=0.$, 
!          whether this is a global or regional run.  The point $rlon=180$, $rlat=0$ in 
!          the rotated coordinate coincides with the point rlon0\_origin, rlat0\_origin 
!          in earth coordinates.  This is why $rlon0_origin=180$. in the global case.
!
!   \item  For regional runs, the rotated coordinate origin and extent of the domain are read
!          in from the NMM restart file.
!
!   \item  The reason for having the longitude of the origin = 180 is because there are
!          places in the global analysis that depend on $0 < lon < 360$.  So to minimize changes
!          to the global code, this approach has been adopted.
!
!   \item  The regional analysis domain is larger than the corresponding NMM grid.  A halo is included
!          whose width is a function of the interpolation order for transfers between grids.
!          This is so the analysis increment is always being interpolated and added on to the
!          full model domain.
!   \end{enumerate}
!
! !REVISION HISTORY:
!   2003-08-28  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-12-15  treadon - explicity set value for inges
!   2005-05-24  pondeca - add the surface analysis option
!   2006-04-06  middlecoff - changed inges from 21 to lendian_in so it can be set to little endian.
!   2009-01-02  todling - remove unused vars
!   2012-01-24  parrish  - correct bug in definition of region_dx, region_dy.
!   2014-03-12  Hu     - Code for GSI analysis on Mass grid larger than background mass grid   
!   2017-10-10  Wu,W   - setup FV3
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   parrish          org: np22                date: 2003-08-28
!
!EOP
!-------------------------------------------------------------------------

    logical fexist
    integer(i_kind) i,k
    real(r_single)pt,pdtop
    real(r_single),allocatable:: deta1(:),aeta1(:),eta1(:),deta2(:),aeta2(:),eta2(:)
    real(r_single) dlmd,dphd
    real(r_single),allocatable:: glat(:,:),glon(:,:)
    real(r_kind),allocatable:: glata(:,:),glona(:,:)
    real(r_kind),allocatable:: glat8(:,:),glon8(:,:)
    real(r_single),allocatable:: dx_nmm(:,:),dy_nmm(:,:)
    real(r_single),allocatable:: dx_mc(:,:),dy_mc(:,:)
    real(r_kind),allocatable:: dx_mca(:,:),dy_mca(:,:)

    real(r_kind),parameter:: r1_5=1.5_r_kind
    real(r_kind),parameter:: six=6.0_r_kind
    real(r_kind),parameter:: r90=90.0_r_kind
    real(r_kind),parameter:: r360=360.0_r_kind

    real(r_kind),allocatable::glat_an(:,:),glon_an(:,:)
    real(r_kind),allocatable:: dx_an(:,:),dy_an(:,:)
    character(6) filename
    integer(i_kind) ihr,i0,j0,nskip,ihr1,ihr2,ihrmid
    real(r_kind),allocatable::gxtemp(:,:),gytemp(:,:)
    real(r_kind),allocatable::gxtemp_an(:,:),gytemp_an(:,:)

    if(.not.regional) then
! This is global run
       rlat_min_ll=-r90*deg2rad
       rlat_max_ll=r90*deg2rad
       rlon_min_ll=zero*deg2rad
       rlon_max_ll=r360*deg2rad
       rlon_min_dd=rlon_min_ll-deg2rad
       rlon_max_dd=rlon_max_ll+deg2rad
       rlat_min_dd=rlat_min_ll-deg2rad
       rlat_max_dd=rlat_max_ll+deg2rad
       dt_ll=zero
    end if


    if(fv3_regional) then     ! begin fv3 regional section
       if(diagnostic_reg.and.mype==0) write(6,*)' in init_reg_glob_ll for FV3 '
       rlon_min_ll=one
       rlat_min_ll=one
       rlon_max_ll=nlon
       rlat_max_ll=nlat
       rlat_min_dd=rlat_min_ll+r1_5/grid_ratio_fv3_regional
       rlat_max_dd=rlat_max_ll-r1_5/grid_ratio_fv3_regional
       rlon_min_dd=rlon_min_ll+r1_5/grid_ratio_fv3_regional
       rlon_max_dd=rlon_max_ll-r1_5/grid_ratio_fv3_regional
       pt_ll=zero
    endif    !  fv3_regional

    if(wrf_nmm_regional) then     ! begin wrf_nmm section
! This is a wrf_nmm regional run.
       if(diagnostic_reg.and.mype==0)  &
          write(6,*)' in init_reg_glob_ll, initializing for wrf nmm regional run'

! Get regional constants
       ihr=-999
       do i=0,12
          write(filename,'("sigf",i2.2)')i
          inquire(file=filename,exist=fexist)
          if(fexist) then
             if (ihr < 0) ihr1=i
             ihr=i
          end if
       end do
       if(ihr<0) then
          write(6,*)' NO INPUT FILE AVAILABLE FOR REGIONAL (WRFNMM) ANALYSIS.  PROGRAM STOPS'
          call stop2(99)
       end if
       ihr2 = ihr
       ihrmid = (ihr1+ihr2)/2

       if(diagnostic_reg.and.mype==0) write(6,*)' in init_reg_glob_ll, lendian_in=',lendian_in
       write(filename,'("sigf",i2.2)') ihrmid
       if(diagnostic_reg.and.mype==0) write(6,*)' in init_reg_glob_ll, filename  =',filename       
       open(lendian_in,file=filename,form='unformatted')
       rewind lendian_in
       read(lendian_in) regional_time,nlon_regional,nlat_regional,nsig, &
                   dlmd,dphd,pt,pdtop
       if(mype==0) write(6,*)'GRIDMOD: inunit: FLNAME: regional_time',lendian_in,filename,  &
              (regional_time (i),i=1,6)       
       
       regional_fhr=zero  !  with wrf nmm fcst hr is not currently available.

       if(diagnostic_reg.and.mype==0) then
           write(6,'(" in init_reg_glob_ll, yr,mn,dy,h,m,s=",6i6)') &
                regional_time
           write(6,'(" in init_reg_glob_ll, nlon_regional=",i6)') &
                nlon_regional
           write(6,'(" in init_reg_glob_ll, nlat_regional=",i6)') &
                nlat_regional
           write(6,'(" in init_reg_glob_ll, nsig=",i6)') nsig 
       end if
 
! Get vertical info for hybrid coordinate and sigma coordinate we will interpolate to
       allocate(aeta1_ll(nsig),eta1_ll(nsig+1),aeta2_ll(nsig),eta2_ll(nsig+1))
       allocate(deta1(nsig),aeta1(nsig),eta1(nsig+1),deta2(nsig),aeta2(nsig),eta2(nsig+1))
       allocate(glat(nlon_regional,nlat_regional),glon(nlon_regional,nlat_regional))
       allocate(dx_nmm(nlon_regional,nlat_regional),dy_nmm(nlon_regional,nlat_regional))
       read(lendian_in) deta1
       read(lendian_in) aeta1
       read(lendian_in) eta1
       read(lendian_in) deta2
       read(lendian_in) aeta2
       read(lendian_in) eta2

       if(diagnostic_reg.and.mype==0) then
          write(6,*)' in init_reg_glob_ll, pdtop,pt=',pdtop,pt
          write(6,*)' in init_reg_glob_ll, aeta1 aeta2 follow:'
          do k=1,nsig
             write(6,'(" k,aeta1,aeta2=",i3,2f10.4)') k,aeta1(k),aeta2(k)
          end do
          write(6,*)' in init_reg_glob_ll, deta1 deta2 follow:'
          do k=1,nsig
             write(6,'(" k,deta1,deta2=",i3,2f10.4)') k,deta1(k),deta2(k)
          end do
!         write(6,*)' in init_reg_glob_ll, deta1 deta2 follow:' 
          write(6,*)' in init_reg_glob_ll,  eta1  eta2 follow:'   
          do k=1,nsig+1
             write(6,'(" k,eta1,eta2=",i3,2f10.4)') k,eta1(k),eta2(k)
          end do
       end if

       pdtop_ll=r0_01*real(pdtop,r_kind)            !  check units--this converts to mb
       pt_ll=r0_01*real(pt,r_kind)                  !  same here
 
       if(diagnostic_reg.and.mype==0) write(6,*)' in init_reg_glob_ll, pdtop_ll,pt_ll=',pdtop_ll,pt_ll
       eta1_ll=real(eta1,r_kind)
       aeta1_ll=real(aeta1,r_kind)
       eta2_ll=real(eta2,r_kind)
       aeta2_ll=real(aeta2,r_kind)
       read(lendian_in) glat,dx_nmm
       read(lendian_in) glon,dy_nmm
       close(lendian_in)

       rlon_min_ll=one
       rlat_min_ll=one
       if(filled_grid) then
          nlon=2*nlon_regional-1
          nlat=nlat_regional
          rlon_max_ll=nlon
          rlat_max_ll=nlat
          rlat_min_dd=rlat_min_ll+three
          rlat_max_dd=rlat_max_ll-three
          rlon_min_dd=rlon_min_ll+six
          rlon_max_dd=rlon_max_ll-six
       end if
       if(half_grid) then
          nlon=nlon_regional
          nlat=1+nlat_regional/2
          rlon_max_ll=nlon
          rlat_max_ll=nlat
          rlat_min_dd=rlat_min_ll+r1_5
          rlat_max_dd=rlat_max_ll-r1_5
          rlon_min_dd=rlon_min_ll+three
          rlon_max_dd=rlon_max_ll-three
       end if

       if(diagnostic_reg.and.mype==0) then
          write(6,*)' in init_reg_glob_ll, rlat_min_dd=',rlat_min_dd
          write(6,*)' in init_reg_glob_ll, rlat_max_dd=',rlat_max_dd
          write(6,*)' in init_reg_glob_ll, rlon_min_dd=',rlon_min_dd
          write(6,*)' in init_reg_glob_ll, rlon_max_dd=',rlon_max_dd
          write(6,*)' in init_reg_glob_ll, rlat_min_ll=',rlat_min_ll
          write(6,*)' in init_reg_glob_ll, rlat_max_ll=',rlat_max_ll
          write(6,*)' in init_reg_glob_ll, rlon_min_ll=',rlon_min_ll
          write(6,*)' in init_reg_glob_ll, rlon_max_ll=',rlon_max_ll
          write(6,*)' in init_reg_glob_ll, filled_grid,half_grid=',filled_grid,half_grid
          write(6,*)' in init_reg_glob_ll, nlon,nlat=',nlon,nlat
       end if

       allocate(region_lat(nlat,nlon),region_lon(nlat,nlon))
       allocate(region_dy(nlat,nlon),region_dx(nlat,nlon))
       allocate(region_dyi(nlat,nlon),region_dxi(nlat,nlon))
       allocate(coeffy(nlat,nlon),coeffx(nlat,nlon))

!   generate earth lats and lons on analysis grid

       allocate(glat_an(nlon,nlat),glon_an(nlon,nlat))
       allocate(dx_an(nlon,nlat),dy_an(nlon,nlat))
 
       if(half_grid) then
          call half_nmm_grid2a(glon,nlon_regional,nlat_regional,glon_an,1)
          call half_nmm_grid2a(glat,nlon_regional,nlat_regional,glat_an,1)
          call half_nmm_grid2a(dx_nmm,nlon_regional,nlat_regional,dx_an,1)
          call half_nmm_grid2a(dy_nmm,nlon_regional,nlat_regional,dy_an,1)
          dx_an=two*dx_an
          dy_an=two*dy_an
       end if

       if(filled_grid) then
          allocate(gxtemp(nlon_regional,nlat_regional))
          allocate(gytemp(nlon_regional,nlat_regional))
          allocate(glon8(nlon_regional,nlat_regional))
          allocate(glat8(nlon_regional,nlat_regional))
          glon8=real(glon,r_kind)
          glat8=real(glat,r_kind)
          i0=nlon_regional/2
          j0=nlat_regional/2
          call ll2rpolar(glat8,glon8,nlon_regional*nlat_regional, &
                         gxtemp,gytemp,glat8(i0,j0),glon8(i0,j0),zero)
          allocate(gxtemp_an(nlon,nlat))
          allocate(gytemp_an(nlon,nlat))
          call fill_nmm_grid2a3(gxtemp,nlon_regional,nlat_regional,gxtemp_an)
          call fill_nmm_grid2a3(gytemp,nlon_regional,nlat_regional,gytemp_an)
          call rpolar2ll(gxtemp_an,gytemp_an,nlon*nlat, &
                         glat_an,glon_an,glat8(i0,j0),glon8(i0,j0),zero)
          gxtemp=real(dx_nmm,r_kind)
          gytemp=real(dy_nmm,r_kind)
          call fill_nmm_grid2a3(gxtemp,nlon_regional,nlat_regional,dx_an)
          call fill_nmm_grid2a3(gytemp,nlon_regional,nlat_regional,dy_an)
          deallocate(gxtemp,gytemp,gxtemp_an,gytemp_an,glon8,glat8)
       end if

       do k=1,nlon
          do i=1,nlat
             region_lat(i,k)=glat_an(k,i)
             region_lon(i,k)=glon_an(k,i)
             region_dy(i,k)=dy_an(k,i)
             region_dx(i,k)=dx_an(k,i)
             region_dyi(i,k)=one/dy_an(k,i)
             region_dxi(i,k)=one/dx_an(k,i)
             coeffy(i,k)=half/dy_an(k,i)
             coeffx(i,k)=half/dx_an(k,i)
          end do
       end do

! ???????  later change glat_an,glon_an to region_lat,region_lon, with dimensions flipped
       call init_general_transform(glat_an,glon_an)

       deallocate(deta1,aeta1,eta1,deta2,aeta2,eta2,glat,glon,glat_an,glon_an)
       deallocate(dx_nmm,dy_nmm,dx_an,dy_an)

    end if   ! end if wrf_nmm section

    if(wrf_mass_regional) then     ! begin wrf mass core section
! This is a wrf_mass regional run.
       if(diagnostic_reg.and.mype==0) &
          write(6,*)' in init_reg_glob_ll, initializing for wrf mass core regional run'

! Get regional constants
       ihr=-999
       do i=0,12
          write(filename,'("sigf",i2.2)')i
          inquire(file=filename,exist=fexist)
          if(fexist) then
             if (ihr < 0) ihr1=i
             ihr=i
          end if
       end do
       if(ihr<0) then
          write(6,*)' NO INPUT FILE AVAILABLE FOR REGIONAL (WRF MASS CORE) ANALYSIS.  PROGRAM STOPS'
          call stop2(99)
       end if
       ihr2 = ihr
       ihrmid = (ihr1+ihr2)/2
       if(diagnostic_reg.and.mype==0) write(6,*)' in init_reg_glob_ll, lendian_in=',lendian_in
       write(filename,'("sigf",i2.2)') ihrmid
       open(lendian_in,file=filename,form='unformatted')
       rewind lendian_in
       read(lendian_in) regional_time,nlon_regional,nlat_regional,nsig,pt,nsig_soil 
       regional_fhr=zero  !  with wrf mass core fcst hr is not currently available.

       if(diagnostic_reg.and.mype==0) then
          write(6,'(" in init_reg_glob_ll, yr,mn,dy,h,m,s=",6i6)') regional_time
          write(6,'(" in init_reg_glob_ll, nlon_regional=",i6)') nlon_regional
          write(6,'(" in init_reg_glob_ll, nlat_regional=",i6)') nlat_regional
          write(6,'(" in init_reg_glob_ll, nsig=",i6)') nsig 
          write(6,'(" in init_reg_glob_ll, nsig_soil=",i6)') nsig_soil 
       end if
 
       call init_wrfmass_to_a(grid_ratio_wrfmass,nlon_regional,nlat_regional)
       nlon=nxa_wrfmass ; nlat=nya_wrfmass

! Get vertical info for wrf mass core
       allocate(aeta1_ll(nsig),eta1_ll(nsig+1))
       allocate(aeta1(nsig),eta1(nsig+1))
       allocate(aeta2_ll(nsig),eta2_ll(nsig+1))
       allocate(aeta2(nsig),eta2(nsig+1))
       allocate(glat(nlon_regional,nlat_regional),glon(nlon_regional,nlat_regional))
       allocate(dx_mc(nlon_regional,nlat_regional),dy_mc(nlon_regional,nlat_regional))
       read(lendian_in) aeta1,aeta2
       read(lendian_in) eta1,eta2
 
       pt_ll=r0_01*pt                    !  check units--this converts to mb
       if(diagnostic_reg.and.mype==0) then
          write(6,*)' in init_reg_glob_ll, pt=',pt
          write(6,*)' in init_reg_glob_ll, aeta1,aeta2 follows:'
          do k=1,nsig
             write(6,'(" k,aeta1=",i3,2f10.4)') k,aeta1(k),aeta2(k)
          end do
          write(6,*)' in init_reg_glob_ll, eta1,eta2 follows:'
          do k=1,nsig+1
             write(6,'(" k,eta1=",i3,2f10.4)') k,eta1(k),eta2(k)
          end do
          write(6,*)' in init_reg_glob_ll, pt_ll=',pt_ll
       end if


       eta1_ll=eta1
       aeta1_ll=aeta1
       eta2_ll=eta2*r0_01
       aeta2_ll=aeta2*r0_01

       read(lendian_in) glat,dx_mc
       read(lendian_in) glon,dy_mc
       close(lendian_in)
 
       rlon_min_ll=one
       rlat_min_ll=one
!       nlon=nlon_regional
!       nlat=nlat_regional
       rlon_max_ll=nlon
       rlat_max_ll=nlat
       rlat_min_dd=rlat_min_ll+r1_5/grid_ratio_wrfmass
       rlat_max_dd=rlat_max_ll-r1_5/grid_ratio_wrfmass
       rlon_min_dd=rlon_min_ll+r1_5/grid_ratio_wrfmass
       rlon_max_dd=rlon_max_ll-r1_5/grid_ratio_wrfmass
 
       if(diagnostic_reg.and.mype==0) then
          write(6,*)' in init_reg_glob_ll, rlat_min_dd=',rlat_min_dd
          write(6,*)' in init_reg_glob_ll, rlat_max_dd=',rlat_max_dd
          write(6,*)' in init_reg_glob_ll, rlon_min_dd=',rlon_min_dd
          write(6,*)' in init_reg_glob_ll, rlon_max_dd=',rlon_max_dd
          write(6,*)' in init_reg_glob_ll, rlat_min_ll=',rlat_min_ll
          write(6,*)' in init_reg_glob_ll, rlat_max_ll=',rlat_max_ll
          write(6,*)' in init_reg_glob_ll, rlon_min_ll=',rlon_min_ll
          write(6,*)' in init_reg_glob_ll, rlon_max_ll=',rlon_max_ll
          write(6,*)' in init_reg_glob_ll, nlon,nlat=',nlon,nlat
       end if

       allocate(glata(nlat,nlon),glona(nlat,nlon))
       allocate(dx_mca(nlat,nlon),dy_mca(nlat,nlon))
       call wrfmass_h_to_a(glat,glata)
       call wrfmass_h_to_a(glon,glona)

       call wrfmass_h_to_a(dx_mc,dx_mca)
       call wrfmass_h_to_a(dy_mc,dy_mca)
       dx_mca=grid_ratio_wrfmass*dx_mca
       dy_mca=grid_ratio_wrfmass*dy_mca

       allocate(region_lat(nlat,nlon),region_lon(nlat,nlon))
       allocate(region_dy(nlat,nlon),region_dx(nlat,nlon))
       allocate(region_dyi(nlat,nlon),region_dxi(nlat,nlon))
       allocate(coeffy(nlat,nlon),coeffx(nlat,nlon))

!  trasfer earth lats and lons to arrays region_lat, region_lon
!  NOTE: The glat_an and glon_an are the latlon values for ensemble perturbation grid

       allocate(glat_an(nlon,nlat),glon_an(nlon,nlat))
       do k=1,nlon
          do i=1,nlat
             glat_an(k,i)=glata(i,k)
             glon_an(k,i)=glona(i,k)
             region_lat(i,k)=glata(i,k)
             region_lon(i,k)=glona(i,k)
             region_dx(i,k)=dx_mca(i,k)
             region_dy(i,k)=dy_mca(i,k)
             region_dxi(i,k)=one/dx_mca(i,k)
             region_dyi(i,k)=one/dy_mca(i,k)
             coeffx(i,k)=half/dx_mca(i,k)
             coeffy(i,k)=half/dy_mca(i,k)
          end do
       end do

! ???????  later change glat_an,glon_an to region_lat,region_lon, with dimensions flipped
       call init_general_transform(glat_an,glon_an)

       deallocate(aeta1,eta1,glat,glon,glat_an,glon_an)
       deallocate(dx_mc,dy_mc)
       deallocate(glata,glona,dx_mca,dy_mca)
 
    end if   ! end if wrf mass core section

    if(nems_nmmb_regional) then     ! begin nems nmmb section
! This is a nems_nmmb regional run.
       if(diagnostic_reg.and.mype==0)  &
          write(6,*)' in init_reg_glob_ll, initializing for nems nmmb regional run'

! Get regional constants
       ihr=-999
       do i=0,12
          write(filename,'("sigf",i2.2)')i
          inquire(file=filename,exist=fexist)
          if(fexist) then
             if (ihr < 0) ihr1=i
             ihr=i
          end if
       end do
       if(ihr<0) then
          write(6,*)' NO INPUT FILE AVAILABLE FOR REGIONAL (NEMS NMMB) ANALYSIS.  PROGRAM STOPS'
          call stop2(99)
       end if
       ihr2 = ihr
       ihrmid = (ihr1+ihr2)/2

       if(diagnostic_reg.and.mype==0) write(6,*)' in init_reg_glob_ll, lendian_in=',lendian_in
       write(filename,'("sigf",i2.2)') ihrmid
       open(lendian_in,file=filename,form='unformatted')
       rewind lendian_in
       read(lendian_in) regional_time,regional_fhr,regional_fmin,nlon_regional,nlat_regional,nsig, &
                   dlmd,dphd,pt,pdtop,nmmb_verttype
 
       if(diagnostic_reg.and.mype==0) then
          write(6,'(" in init_reg_glob_ll, yr,mn,dy,h,m,s=",6i6)') regional_time
          write(6,'(" in init_reg_glob_ll, nlon_regional=",i6)') nlon_regional
          write(6,'(" in init_reg_glob_ll, nlat_regional=",i6)') nlat_regional
          write(6,'(" in init_reg_glob_ll, nsig=",i6)') nsig
       end if
 
       call init_nmmb_to_a(nmmb_reference_grid,grid_ratio_nmmb,nlon_regional,nlat_regional)
       nlon=nxa ; nlat=nya
 
       rlon_min_ll=one
       rlat_min_ll=one
       rlon_max_ll=nlon
       rlat_max_ll=nlat
    !  rlat_min_dd=rlat_min_ll+three/grid_ratio_nmmb
    !  rlat_max_dd=rlat_max_ll-three/grid_ratio_nmmb
    !  rlon_min_dd=rlon_min_ll+six/grid_ratio_nmmb
    !  rlon_max_dd=rlon_max_ll-six/grid_ratio_nmmb
       rlat_min_dd=rlat_min_ll+r1_5*1.412_r_kind/grid_ratio_nmmb
       rlat_max_dd=rlat_max_ll-r1_5*1.412_r_kind/grid_ratio_nmmb
       rlon_min_dd=rlon_min_ll+three*1.412_r_kind/grid_ratio_nmmb
       rlon_max_dd=rlon_max_ll-three*1.412_r_kind/grid_ratio_nmmb

       if(diagnostic_reg.and.mype==0) then
          write(6,*)' in init_reg_glob_ll, rlat_min_dd=',rlat_min_dd
          write(6,*)' in init_reg_glob_ll, rlat_max_dd=',rlat_max_dd
          write(6,*)' in init_reg_glob_ll, rlon_min_dd=',rlon_min_dd
          write(6,*)' in init_reg_glob_ll, rlon_max_dd=',rlon_max_dd
          write(6,*)' in init_reg_glob_ll, rlat_min_ll=',rlat_min_ll
          write(6,*)' in init_reg_glob_ll, rlat_max_ll=',rlat_max_ll
          write(6,*)' in init_reg_glob_ll, rlon_min_ll=',rlon_min_ll
          write(6,*)' in init_reg_glob_ll, rlon_max_ll=',rlon_max_ll
          write(6,*)' in init_reg_glob_ll, nmmb_reference_grid=',nmmb_reference_grid
          write(6,*)' in init_reg_glob_ll, grid_ratio_nmmb=',grid_ratio_nmmb
          write(6,*)' in init_reg_glob_ll, nlon,nlat=',nlon,nlat
       end if

! Get vertical info for hybrid coordinate and sigma coordinate we will interpolate to
       allocate(aeta1_ll(nsig),eta1_ll(nsig+1),aeta2_ll(nsig),eta2_ll(nsig+1))
       allocate(deta1(nsig),aeta1(nsig),eta1(nsig+1),deta2(nsig),aeta2(nsig),eta2(nsig+1))
       allocate(glat(nlon_regional,nlat_regional),glon(nlon_regional,nlat_regional))
       allocate(dx_nmm(nlon_regional,nlat_regional),dy_nmm(nlon_regional,nlat_regional))
       read(lendian_in) deta1
       read(lendian_in) aeta1
       read(lendian_in) eta1
       read(lendian_in) deta2
       read(lendian_in) aeta2
       read(lendian_in) eta2

       if(diagnostic_reg.and.mype==0) then
          write(6,*)' in init_reg_glob_ll, pdtop,pt=',pdtop,pt
          write(6,*)' in init_reg_glob_ll, aeta1 aeta2 follow:'
          do k=1,nsig
             write(6,'(" k,aeta1,aeta2=",i3,2f10.4)') k,aeta1(k),aeta2(k)
          end do
          write(6,*)' in init_reg_glob_ll, deta1 deta2 follow:'
          do k=1,nsig
             write(6,'(" k,deta1,deta2=",i3,2f10.4)') k,deta1(k),deta2(k)
          end do
          write(6,*)' in init_reg_glob_ll, deta1 deta2 follow:'
          do k=1,nsig+1
             write(6,'(" k,eta1,eta2=",i3,2f10.4)') k,eta1(k),eta2(k)
          end do
       end if

       pdtop_ll=r0_01*pdtop                    !  check units--this converts to mb
       pt_ll=r0_01*pt                          !  same here

       if(diagnostic_reg.and.mype==0) write(6,*)' in init_reg_glob_ll, pdtop_ll,pt_ll=',pdtop_ll,pt_ll
       eta1_ll=eta1
       aeta1_ll=aeta1
       eta2_ll=eta2
       aeta2_ll=aeta2
       read(lendian_in) glat,dx_nmm
       read(lendian_in) glon,dy_nmm
       close(lendian_in)

       allocate(region_lat(nlat,nlon),region_lon(nlat,nlon))
       allocate(region_dy(nlat,nlon),region_dx(nlat,nlon))
       allocate(region_dyi(nlat,nlon),region_dxi(nlat,nlon))
       allocate(coeffy(nlat,nlon),coeffx(nlat,nlon))

!   generate earth lats and lons on analysis grid

       allocate(glat_an(nlon,nlat),glon_an(nlon,nlat))
       allocate(dx_an(nlat,nlon),dy_an(nlat,nlon))
 
       allocate(gxtemp(nlon_regional,nlat_regional))
       allocate(gytemp(nlon_regional,nlat_regional))
       allocate(gxtemp_an(nlat,nlon))
       allocate(gytemp_an(nlat,nlon))
       allocate(glon8(nlon_regional,nlat_regional))
       allocate(glat8(nlon_regional,nlat_regional))
       glon8=glon
       glat8=glat
       i0=nlon_regional/2
       j0=nlat_regional/2
       call ll2rpolar(glat8,glon8,nlon_regional*nlat_regional, &
                      gxtemp,gytemp,glat8(i0,j0),glon8(i0,j0),zero)
       call nmmb_h_to_a8(gxtemp,gxtemp_an)
       call nmmb_h_to_a8(gytemp,gytemp_an)
       call rpolar2ll(gxtemp_an,gytemp_an,nlon*nlat, &
                      region_lat,region_lon,glat8(i0,j0),glon8(i0,j0),zero)
       do k=1,nlon
          do i=1,nlat
             glat_an(k,i)=region_lat(i,k)
             glon_an(k,i)=region_lon(i,k)
          end do
       end do
       gxtemp=dx_nmm
       gytemp=dy_nmm
       call nmmb_h_to_a8(gxtemp,dx_an)
       call nmmb_h_to_a8(gytemp,dy_an)
                       if(mype==0) write(6,*)' in init_reg_glob_ll, ratio_x,ratio_y,grid_ratio_nmmb=',&
                                                                    ratio_x,ratio_y,grid_ratio_nmmb
       dx_an=ratio_x*dx_an
       dy_an=ratio_y*dy_an
       deallocate(gxtemp,gytemp,gxtemp_an,gytemp_an,glon8,glat8)

       do k=1,nlon
          do i=1,nlat
             region_dy(i,k)=dy_an(i,k)
             region_dx(i,k)=dx_an(i,k)
             region_dyi(i,k)=one/dy_an(i,k)
             region_dxi(i,k)=one/dx_an(i,k)
             coeffy(i,k)=half/dy_an(i,k)
             coeffx(i,k)=half/dx_an(i,k)
          end do
       end do

! ???????  later change glat_an,glon_an to region_lat,region_lon, with dimensions flipped
       call init_general_transform(glat_an,glon_an)

       deallocate(deta1,aeta1,eta1,deta2,aeta2,eta2,glat,glon,glat_an,glon_an)
       deallocate(dx_nmm,dy_nmm,dx_an,dy_an)

    end if   ! end if nems nmmb section

    if (cmaq_regional) then     ! begin cmaq core section

       if(diagnostic_reg.and.mype==0) &
            write(6,*)' in init_reg_glob_ll, initializing for cmaq regional run'

! get regional constants
    
       ihr=-999
  
       do i=0,12
          write(filename,'("sigf",i2.2)')i
          inquire(file=filename,exist=fexist)
          if(fexist) then!
             if (ihr < 0) ihr1=i
             ihr=i
          end if
       end do
    
       if(ihr<0) then
          write(6,*)' no input file available for regional cmaq analysis.  program stops'
          call stop2(99)
       end if
       ihr2 = ihr
       ihrmid = (ihr1+ihr2)/2

       if(diagnostic_reg.and.mype==0) then    
          write(6,*)' in read_cmaq_grid lendian_in=',lendian_in
          write(6,*)' in read_cmaq_grid, lendian_in=',lendian_in
       endif
    
       write(filename,'("sigf",i2.2)') ihrmid
       open(lendian_in,file=filename,form='unformatted')
       rewind(lendian_in)
       read(lendian_in) nskip
       read(lendian_in) regional_time,nlon_regional,nlat_regional,nsig,pt,pdtop   !1 to skip
    
       if(diagnostic_reg.and.mype==0) then
          write(6,'(" in read_cmaq_grid, yr,mn,dy,h,m,s=",6i6)') &
               regional_time
          write(6,'(" in read_cmaq_grid, nlon_regional=",i6)') &
               nlon_regional
          write(6,'(" in read_cmaq_grid, nlat_regional=",i6)') &
               nlat_regional
          write(6,'(" in read_cmaq_grid, nsig=",i6)') nsig 
       endif

       allocate(aeta1(nsig),eta1(nsig+1))
       allocate(aeta1_ll(nsig),eta1_ll(nsig+1))  
       allocate(aeta2(nsig),eta2(nsig+1))
       allocate(aeta2_ll(nsig),eta2_ll(nsig+1))  
       allocate(glon(nlon_regional,nlat_regional),&
          dx_mc(nlon_regional,nlat_regional))
       allocate(glat(nlon_regional,nlat_regional),&
          dy_mc(nlon_regional,nlat_regional))
    
       regional_fhr=zero !that is not available/nor seems currently necessary 
     
       read(lendian_in) aeta1,aeta2 ! 2 to skip
       read(lendian_in) eta1,eta2  ! 3 to skip
    
       pt_ll=r0_01*pt                    !  check units--this converts to mb
       pdtop_ll=r0_01*pdtop
       
       eta1_ll=eta1
       aeta1_ll=aeta1
       eta2_ll=eta2
       aeta2_ll=aeta2

!cmaq input arrays are dimensioned (nlat_regional,nlon_regional)
    
       read(lendian_in) glat  ! 4 to skip
       read(lendian_in) dx_mc ! 5 to skip
       read(lendian_in) glon  ! 6 to skip
       read(lendian_in) dy_mc ! 7 to skip
       close(lendian_in) 
    
       nlon=nlon_regional
       nlat=nlat_regional
     
       rlon_min_ll=one
       rlat_min_ll=one
       rlon_max_ll=nlon
       rlat_max_ll=nlat
       rlat_min_dd=rlat_min_ll+r1_5
       rlat_max_dd=rlat_max_ll-r1_5
       rlon_min_dd=rlon_min_ll+r1_5
       rlon_max_dd=rlon_max_ll-r1_5
    
       if(diagnostic_reg.and.mype==0) then
          write(6,*)' in read_cmaq_grid, rlat_min_dd=',rlat_min_dd
          write(6,*)' in read_cmaq_grid, rlat_max_dd=',rlat_max_dd
          write(6,*)' in read_cmaq_grid, rlon_min_dd=',rlon_min_dd
          write(6,*)' in read_cmaq_grid, rlon_max_dd=',rlon_max_dd
          write(6,*)' in read_cmaq_grid, rlat_min_ll=',rlat_min_ll
          write(6,*)' in read_cmaq_grid, rlat_max_ll=',rlat_max_ll
          write(6,*)' in read_cmaq_grid, rlon_min_ll=',rlon_min_ll
          write(6,*)' in read_cmaq_grid, rlon_max_ll=',rlon_max_ll
          write(6,*)' in read_cmaq_grid, nlon,nlat=',nlon,nlat
       end if
    
    
!note different allocation for glat_an,glat_an
!need to flip cmaq input glat,glon
  

       allocate(region_lat(nlat,nlon),region_lon(nlat,nlon))
       allocate(region_dy(nlat,nlon),region_dx(nlat,nlon))
       allocate(region_dyi(nlat,nlon),region_dxi(nlat,nlon))
       allocate(coeffy(nlat,nlon),coeffx(nlat,nlon))
       allocate(glat_an(nlon,nlat),glon_an(nlon,nlat))
    
       do k=1,nlon
         do i=1,nlat
            glat_an(k,i)=glat(k,i)*deg2rad
            glon_an(k,i)=glon(k,i)*deg2rad
            region_lat(i,k)=glat(k,i)*deg2rad
            region_lon(i,k)=glon(k,i)*deg2rad
            region_dx(i,k)=dx_mc(k,i)
            region_dy(i,k)=dy_mc(k,i)
            region_dxi(i,k)=one/dx_mc(k,i)
            region_dyi(i,k)=one/dy_mc(k,i)
            coeffx(i,k)=half/dx_mc(k,i)
            coeffy(i,k)=half/dy_mc(k,i)
         end do
       end do


       call init_general_transform(glat_an,glon_an)

       deallocate(aeta1,eta1,aeta2,eta2,glat,glon,glat_an,glon_an,dx_mc,dy_mc)

    end if   ! end cmaq section



!   Begin surface analysis section (regional 2D-var)
    if(twodvar_regional) then 

! This is a surface analysis regional run.
       if(diagnostic_reg.and.mype==0) &
          write(6,*)' in init_reg_glob_ll, initializing for surface analysis regional run'

! Get regional constants
       ihr=-999
       do i=0,12
          write(filename,'("sigf",i2.2)')i
          inquire(file=filename,exist=fexist)
          if(fexist) then                     !Note: for the twodvar_regional option,
             ihr=i                            !the first 'sigfnn' file is the one that
             exit                             !is valid at the analysis time. hence,
          end if                              !there is no need for the ihrmid variable
       end do                                 !that is used for the other options
       if(ihr<0) then
          write(6,*)' NO INPUT FILE AVAILABLE FOR REGIONAL (SURFACE) ANALYSIS.  PROGRAM STOPS'
          call stop2(99)
       end if
       if(diagnostic_reg.and.mype==0) write(6,*)' in init_reg_glob_ll, lendian_in=',lendian_in
       open(lendian_in,file=filename,form='unformatted')
       rewind lendian_in
       read(lendian_in) regional_time,nlon_regional,nlat_regional,nsig
       regional_fhr=zero  !  with twodvar analysis fcst hr is not currently available.
 
       if(diagnostic_reg.and.mype==0) then
           write(6,'(" in init_reg_glob_ll, yr,mn,dy,h,m,s=",6i6)')regional_time
           write(6,'(" in init_reg_glob_ll, nlon_regional=",i6)') nlon_regional
           write(6,'(" in init_reg_glob_ll, nlat_regional=",i6)') nlat_regional
           write(6,'(" in init_reg_glob_ll, nsig=",i6)') nsig 
       end if
 
! Get vertical info 
       allocate(aeta1_ll(nsig),eta1_ll(nsig+1))
       allocate(aeta1(nsig),eta1(nsig+1))
       allocate(glat(nlon_regional,nlat_regional),glon(nlon_regional,nlat_regional))
       allocate(dx_mc(nlon_regional,nlat_regional),dy_mc(nlon_regional,nlat_regional))
 
       aeta1=one                ! set to this value for convenience
       eta1=one                 ! set to this value for convenience
       pt=0._r_single           ! set to this value for convenience

       pt_ll=r0_01*pt
       eta1_ll=eta1
       aeta1_ll=aeta1
 
       read(lendian_in) glat,dx_mc
       read(lendian_in) glon,dy_mc
       close(lendian_in)
 
       rlon_min_ll=one
       rlat_min_ll=one
       nlon=nlon_regional
       nlat=nlat_regional
       rlon_max_ll=nlon
       rlat_max_ll=nlat
       rlat_min_dd=rlat_min_ll+r1_5
       rlat_max_dd=rlat_max_ll-r1_5
       rlon_min_dd=rlon_min_ll+r1_5
       rlon_max_dd=rlon_max_ll-r1_5

       if(diagnostic_reg.and.mype==0) then
          write(6,*)' in init_reg_glob_ll, rlat_min_dd=',rlat_min_dd
          write(6,*)' in init_reg_glob_ll, rlat_max_dd=',rlat_max_dd
          write(6,*)' in init_reg_glob_ll, rlon_min_dd=',rlon_min_dd
          write(6,*)' in init_reg_glob_ll, rlon_max_dd=',rlon_max_dd
          write(6,*)' in init_reg_glob_ll, rlat_min_ll=',rlat_min_ll
          write(6,*)' in init_reg_glob_ll, rlat_max_ll=',rlat_max_ll
          write(6,*)' in init_reg_glob_ll, rlon_min_ll=',rlon_min_ll
          write(6,*)' in init_reg_glob_ll, rlon_max_ll=',rlon_max_ll
          write(6,*)' in init_reg_glob_ll, nlon,nlat=',nlon,nlat
       end if

       allocate(region_lat(nlat,nlon),region_lon(nlat,nlon))
       allocate(region_dy(nlat,nlon),region_dx(nlat,nlon))
       allocate(region_dyi(nlat,nlon),region_dxi(nlat,nlon))
       allocate(coeffy(nlat,nlon),coeffx(nlat,nlon))

!   transfer earth lats and lons to arrays region_lat, region_lon

       allocate(glat_an(nlon,nlat),glon_an(nlon,nlat))
       do k=1,nlon
          do i=1,nlat
             glat_an(k,i)=glat(k,i)
             glon_an(k,i)=glon(k,i)
             region_lat(i,k)=glat(k,i)
             region_lon(i,k)=glon(k,i)
             region_dx(i,k)=dx_mc(k,i)
             region_dy(i,k)=dy_mc(k,i)
             region_dxi(i,k)=one/dx_mc(k,i)
             region_dyi(i,k)=one/dy_mc(k,i)
             coeffx(i,k)=half/dx_mc(k,i)
             coeffy(i,k)=half/dy_mc(k,i)
          end do
       end do

! ???????  later change glat_an,glon_an to region_lat,region_lon, with dimensions flipped
       call init_general_transform(glat_an,glon_an)

       deallocate(aeta1,eta1,glat,glon,glat_an,glon_an)
       deallocate(dx_mc,dy_mc)

    end if   ! end if twodvar analysis section

    return
  end subroutine init_reg_glob_ll

 subroutine init_general_transform(glats,glons)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_general_transform
!   prgmmr:  parrish
!
! abstract:  set up constants to allow conversion between earth lat lon and analysis grid units.
!     There is no need to specify details of the analysis grid projection.  All that is required
!     is the earth latitude and longitude in radians of each analysis grid point.
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2010-09-08  parrish - replace computation of wind rotation reference angle cos_beta_ref,sin_beta_ref
!                          with new, more accurate and robust version which works for any orientation
!                          of the analysis grid on the sphere (only restriction for now is that
!                          x-y coordinate of analysis grid is right handed).
!
!   input argument list:
!    glons,glats - lons,lats of input grid points of dimesion nlon,nlat
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero,one,half,pi
  implicit none

  real(r_kind)   ,intent(in   ) :: glats(nlon,nlat),glons(nlon,nlat)

  real(r_kind),parameter:: rbig =1.0e30_r_kind
  real(r_kind) xbar_min,xbar_max,ybar_min,ybar_max
  real(r_kind) clon,slon,r_of_lat,xbar,ybar
  integer(i_kind) i,j,istart0,iend,iinc,itemp,ilast,jlast
  real(r_kind),allocatable:: clata(:,:),slata(:,:),clona(:,:),slona(:,:)
  real(r_kind) clat0,slat0,clon0,slon0
  real(r_kind) clat_m1,slat_m1,clon_m1,slon_m1
  real(r_kind) clat_p1,slat_p1,clon_p1,slon_p1
  real(r_kind) x,y,z,xt,yt,zt,xb,yb,zb
  real(r_kind) rlonb_m1,clonb_m1,slonb_m1
  real(r_kind) rlonb_p1,clonb_p1,slonb_p1
  real(r_kind) crot,srot

  pihalf=half*pi

!  define xtilde, ytilde grid, transform

!      glons,glats are lons, lats of input grid points of dimension nlon,nlat
  call get_xytilde_domain(nlon,nlat,glons,glats,nxtilde,nytilde, &
                   xbar_min,xbar_max,ybar_min,ybar_max)
  allocate(i0_tilde(nxtilde,nytilde),j0_tilde(nxtilde,nytilde))
  allocate(ip_tilde(nxtilde,nytilde),jp_tilde(nxtilde,nytilde))
  allocate(xtilde0(nlon,nlat),ytilde0(nlon,nlat))

! define atilde_x, btilde_x, atilde_y, btilde_y

  btilde_x   =(nxtilde -one     )/(xbar_max-xbar_min)
  btilde_xinv=(xbar_max-xbar_min)/(nxtilde -one     )
  atilde_x   =one-btilde_x*xbar_min
  btilde_y   =(nytilde -one     )/(ybar_max-ybar_min)
  btilde_yinv=(ybar_max-ybar_min)/(nytilde -one     )
  atilde_y   =one-btilde_y*ybar_min

! define xtilde0,ytilde0
  do j=1,nlat
     do i=1,nlon
        r_of_lat=pihalf+sign_pole*glats(i,j)
        clon=cos(glons(i,j)+rlambda0)
        slon=sin(glons(i,j)+rlambda0)
        xbar=r_of_lat*clon
        ybar=r_of_lat*slon
        xtilde0(i,j)=atilde_x+btilde_x*xbar
        ytilde0(i,j)=atilde_y+btilde_y*ybar
     end do
  end do

!  now get i0_tilde, j0_tilde, ip_tilde,jp_tilde
  ilast=1 ; jlast=1
  istart0=nxtilde
  iend=1
  iinc=-1
  do j=1,nytilde
     itemp=istart0
     istart0=iend
     iend=itemp
     iinc=-iinc
     ybar=j
     do i=istart0,iend,iinc
        xbar=i
        call nearest_3(ilast,jlast,i0_tilde(i,j),j0_tilde(i,j), &
                       ip_tilde(i,j),jp_tilde(i,j),xbar,ybar,nlon,nlat,xtilde0,ytilde0)
     end do
  end do

!   new, more accurate and robust computation of cos_beta_ref and sin_beta_ref which is independent
!     of sign_pole and works for any orientation of grid on sphere (only restriction for now is that
!     x-y coordinate of analysis grid is right handed).
  allocate(clata(nlon,nlat),slata(nlon,nlat),clona(nlon,nlat),slona(nlon,nlat))
  allocate(cos_beta_ref(nlon,nlat),sin_beta_ref(nlon,nlat))
  do j=1,nlat
     do i=1,nlon
        clata(i,j)=cos(glats(i,j))
        slata(i,j)=sin(glats(i,j))
        clona(i,j)=cos(glons(i,j))
        slona(i,j)=sin(glons(i,j))
     end do
  end do
  do j=1,nlat
     do i=2,nlon-1

!     do all interior lon points to 2nd order accuracy

!   transform so pole is at rlat0,rlon0 and 0 meridian is tangent to earth latitude at rlat0,rlon0.

        clat0=clata(i,j) ; slat0=slata(i,j) ; clon0=clona(i,j) ; slon0=slona(i,j)

!    now obtain new coordinates for m1 and p1 points.

        clat_m1=clata(i-1,j) ; slat_m1=slata(i-1,j) ; clon_m1=clona(i-1,j) ; slon_m1=slona(i-1,j)
        clat_p1=clata(i+1,j) ; slat_p1=slata(i+1,j) ; clon_p1=clona(i+1,j) ; slon_p1=slona(i+1,j)

        x=clat_m1*clon_m1 ; y=clat_m1*slon_m1 ; z=slat_m1
        xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
        yb=zt*clat0-xt*slat0
        xb=yt
        zb=xt*clat0+zt*slat0

        rlonb_m1=atan2(-yb,-xb)   !  the minus signs here are so line for m1 is directed same
        clonb_m1=cos(rlonb_m1)
        slonb_m1=sin(rlonb_m1)

        x=clat_p1*clon_p1 ; y=clat_p1*slon_p1 ; z=slat_p1
        xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
        yb=zt*clat0-xt*slat0
        xb=yt
        zb=xt*clat0+zt*slat0
        rlonb_p1=atan2(yb,xb)
        clonb_p1=cos(rlonb_p1)
        slonb_p1=sin(rlonb_p1)
        crot=half*(clonb_m1+clonb_p1)
        srot=half*(slonb_m1+slonb_p1)
        cos_beta_ref(i,j)=crot*clon0-srot*slon0
        sin_beta_ref(i,j)=srot*clon0+crot*slon0
     end do
!               now do i=1 and i=nlon at 1st order accuracy
     i=1

!   transform so pole is at rlat0,rlon0 and 0 meridian is tangent to earth latitude at rlat0,rlon0.

        clat0=clata(i,j) ; slat0=slata(i,j) ; clon0=clona(i,j) ; slon0=slona(i,j)
!    now obtain new coordinates for m1 and p1 points.

        clat_p1=clata(i+1,j) ; slat_p1=slata(i+1,j) ; clon_p1=clona(i+1,j) ; slon_p1=slona(i+1,j)

        x=clat_p1*clon_p1 ; y=clat_p1*slon_p1 ; z=slat_p1
        xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
        yb=zt*clat0-xt*slat0
        xb=yt
        zb=xt*clat0+zt*slat0
        rlonb_p1=atan2(yb,xb)
        clonb_p1=cos(rlonb_p1)
        slonb_p1=sin(rlonb_p1)
        crot=clonb_p1
        srot=slonb_p1
        cos_beta_ref(i,j)=crot*clon0-srot*slon0
        sin_beta_ref(i,j)=srot*clon0+crot*slon0

     i=nlon

!   transform so pole is at rlat0,rlon0 and 0 meridian is tangent to earth latitude at rlat0,rlon0.

        clat0=clata(i,j) ; slat0=slata(i,j) ; clon0=clona(i,j) ; slon0=slona(i,j)

!    now obtain new coordinates for m1 and p1 points.

        clat_m1=clata(i-1,j) ; slat_m1=slata(i-1,j) ; clon_m1=clona(i-1,j) ; slon_m1=slona(i-1,j)

        x=clat_m1*clon_m1 ; y=clat_m1*slon_m1 ; z=slat_m1
        xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
        yb=zt*clat0-xt*slat0
        xb=yt
        zb=xt*clat0+zt*slat0

        rlonb_m1=atan2(-yb,-xb)   !  the minus signs here are so line for m1 is directed same
        clonb_m1=cos(rlonb_m1)
        slonb_m1=sin(rlonb_m1)

        crot=clonb_m1
        srot=slonb_m1
        cos_beta_ref(i,j)=crot*clon0-srot*slon0
        sin_beta_ref(i,j)=srot*clon0+crot*slon0
  end do

end subroutine init_general_transform

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  tll2xy --- convert earth lon-lat to x-y grid coordinates
!
! !INTERFACE:
!
  subroutine tll2xy(rlon,rlat,x,y,outside)

! !USES:

    use constants, only: one
    implicit none

    real(r_kind),intent(in   ) :: rlon  ! earth longitude (radians)
    real(r_kind),intent(in   ) :: rlat  ! earth latitude  (radians)

! !OUTPUT PARAMETERS:

    real(r_kind),intent(  out) :: x  ! x-grid coordinate (grid units)
    real(r_kind),intent(  out) :: y  ! y-grid coordinate (grid units)
    logical     ,intent(  out) :: outside     ! .false., then point is inside x-y domain
                                              ! .true.,  then point is outside x-y domain

! !DESCRIPTION: to convert earth lon-lat to x-y grid units of a 
!           general regional rectangular domain.  Also, decide if
!           point is inside this domain.  As a result, there is
!           no restriction on type of horizontal coordinate for
!           a regional run, other than that it not have periodicity
!           or polar singularities.
!           This is done by first converting rlon, rlat to an
!           intermediate coordinate xtilde,ytilde, which has
!           precomputed pointers and constants for final conversion
!           to the desired x,y via 3 point inverse interpolation.
!           All of the information needed is derived from arrays
!           specifying earth latitude and longitude of every point
!           on the input grid.  Currently, the input x-y grid that
!           this is based on must be non-staggered.  This restriction
!           will eventually be lifted so we can run directly from
!           model grids that are staggered without first resorting
!           to interpolation of the guess to a non-staggered grid.
!
! !REVISION HISTORY:
!   2003-08-28  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-23  parrish - new routine
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-08-28
!
!EOP
!-------------------------------------------------------------------------

    real(r_kind) clon,slon,r_of_lat,xtilde,ytilde
    real(r_kind) dtilde,etilde
    real(r_kind) d1tilde,d2tilde,e1tilde,e2tilde,detinv
    integer(i_kind) itilde,jtilde
    integer(i_kind) i0,j0,ip,jp

!   first compute xtilde, ytilde

    clon=cos(rlon+rlambda0)
    slon=sin(rlon+rlambda0)
    r_of_lat=pihalf+sign_pole*rlat

    xtilde=atilde_x+btilde_x*r_of_lat*clon
    ytilde=atilde_y+btilde_y*r_of_lat*slon

!  next get interpolation information

    itilde=max(1,min(nint(xtilde),nxtilde))
    jtilde=max(1,min(nint(ytilde),nytilde))

    i0     =   i0_tilde(itilde,jtilde)
    j0     =   j0_tilde(itilde,jtilde)
    ip     =i0+ip_tilde(itilde,jtilde)
    jp     =j0+jp_tilde(itilde,jtilde)
    dtilde =xtilde-xtilde0(i0,j0)
    etilde =ytilde-ytilde0(i0,j0)
    d1tilde=(xtilde0(ip,j0)-xtilde0(i0,j0))*(ip-i0)
    d2tilde=(xtilde0(i0,jp)-xtilde0(i0,j0))*(jp-j0)
    e1tilde=(ytilde0(ip,j0)-ytilde0(i0,j0))*(ip-i0)
    e2tilde=(ytilde0(i0,jp)-ytilde0(i0,j0))*(jp-j0)
    detinv =one/(d1tilde*e2tilde-d2tilde*e1tilde)
    x = i0+detinv*(e2tilde*dtilde-d2tilde*etilde)
    y = j0+detinv*(d1tilde*etilde-e1tilde*dtilde)
    if (i0 == ip .and. j0 == jp) then ! ob at center of domain. 
       x = i0; y = j0 
    else 
       x = i0+detinv*(e2tilde*dtilde-d2tilde*etilde) 
       y = j0+detinv*(d1tilde*etilde-e1tilde*dtilde) 
    endif

    outside=x < rlon_min_dd .or. x > rlon_max_dd .or. &
            y < rlat_min_dd .or. y > rlat_max_dd

 end subroutine tll2xy

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  txy2ll ---  convert x-y grid units to earth lat-lon coordinates
!
! !INTERFACE:
!
  subroutine txy2ll(x,y,rlon,rlat)

! !USES:

    use constants, only: one
    implicit none

! !INPUT PARAMETERS:

    real(r_kind),intent(in   ) :: x      ! x-grid coordinate (grid units)
    real(r_kind),intent(in   ) :: y      ! y_grid coordinate (grid units)

! !OUTPUT PARAMETERS:

    real(r_kind),intent(  out) :: rlon   ! earth longitude (radians)
    real(r_kind),intent(  out) :: rlat   ! earth latitude  (radians)

! !DESCRIPTION: to convert earth lon-lat to x-y grid units of a
!           general regional rectangular domain.  Also, decide if
!           point is inside this domain.  As a result, there is
!           no restriction on type of horizontal coordinate for
!           a regional run, other than that it not have periodicity
!           or polar singularities.
!           This is done by first converting rlon, rlat to an
!           intermediate coordinate xtilde,ytilde, which has
!           precomputed pointers and constants for final conversion
!           to the desired x,y via 3 point inverse interpolation.
!           All of the information needed is derived from arrays
!           specifying earth latitude and longitude of every point
!           on the input grid.  Currently, the input x-y grid that
!           this is based on must be non-staggered.  This restriction
!           will eventually be lifted so we can run directly from
!           model grids that are staggered without first resorting
!           to interpolation of the guess to a non-staggered grid.
!
! !REVISION HISTORY:
!   2003-08-28  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-20  todling, fixed description
!   2004-07-23  parrish - new routine
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-08-28
!
!EOP
!-------------------------------------------------------------------------

    real(r_kind) r_of_lat,xtilde,ytilde
    real(r_kind) dtilde,etilde,xbar,ybar
    real(r_kind) d1tilde,d2tilde,e1tilde,e2tilde
    integer(i_kind) i0,j0,ip,jp

    i0=nint(x)
    j0=nint(y)
    i0=max(1,min(i0,nlon))
    j0=max(1,min(j0,nlat))
    ip=i0+nint(sign(one,x-i0))
    jp=j0+nint(sign(one,y-j0))
    if(ip<1) then
       i0=2
       ip=1
    end if
    if(jp<1) then
       j0=2
       jp=1
    end if
    if(ip>nlon) then
       i0=nlon-1
       ip=nlon
    end if
    if(jp>nlat) then
       j0=nlat-1
       jp=nlat
    end if
    d1tilde=(xtilde0(ip,j0)-xtilde0(i0,j0))*(ip-i0)
    d2tilde=(xtilde0(i0,jp)-xtilde0(i0,j0))*(jp-j0)
    e1tilde=(ytilde0(ip,j0)-ytilde0(i0,j0))*(ip-i0)
    e2tilde=(ytilde0(i0,jp)-ytilde0(i0,j0))*(jp-j0)
    dtilde =d1tilde*(x-i0) +d2tilde*(y-j0)
    etilde =e1tilde*(x-i0) +e2tilde*(y-j0)
    xtilde =dtilde         +xtilde0(i0,j0)
    ytilde =etilde         +ytilde0(i0,j0)

    xbar=(xtilde-atilde_x)*btilde_xinv
    ybar=(ytilde-atilde_y)*btilde_yinv
    r_of_lat=sqrt(xbar**2+ybar**2)
    rlat=(r_of_lat-pihalf)*sign_pole
    rlon=atan2(ybar,xbar)-rlambda0

 end subroutine txy2ll

 subroutine nearest_3(ilast,jlast,i0,j0,ip,jp,x,y,nx0,ny0,x0,y0)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nearest_3
!   prgmmr:
!
! abstract: find closest 3 points to (x,y) on grid defined by x0,y0
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ilast,jlast
!    nx0,ny0
!    x,y
!    x0,y0
!
!   output argument list:
!    ilast,jlast
!    i0,j0
!    ip,jp
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  integer(i_kind),intent(inout) :: ilast,jlast
  integer(i_kind),intent(  out) :: i0,j0
  integer(i_byte),intent(  out) :: ip,jp
  integer(i_kind),intent(in   ) :: nx0,ny0
  real(r_kind)   ,intent(in   ) :: x,y
  real(r_kind)   ,intent(in   ) :: x0(nx0,ny0),y0(nx0,ny0)
 
  real(r_kind) dista,distb,dist2,dist2min
  integer(i_kind) i,inext,j,jnext

  do
     i0=ilast
     j0=jlast
     dist2min=huge(dist2min)
     inext=0
     jnext=0
     do j=max(j0-1,1),min(j0+1,ny0)
        do i=max(i0-1,1),min(i0+1,nx0)
           dist2=(x-x0(i,j))**2+(y-y0(i,j))**2
           if(dist2<dist2min) then
              dist2min=dist2
              inext=i
              jnext=j
           end if
        end do
     end do
     if(inext==i0.and.jnext==j0) exit
     ilast=inext
     jlast=jnext
  end do

!  now find which way to go in x for second point

  ip=0
  if(i0==nx0)  ip=-1
  if(i0==1) ip=1
  if(ip==0) then
     dista=(x-x0(i0-1,j0))**2+(y-y0(i0-1,j0))**2
     distb=(x-x0(i0+1,j0))**2+(y-y0(i0+1,j0))**2
     if(distb<dista) then
        ip=1
     else
        ip=-1
     end if
  end if

!  repeat for y for 3rd point

  jp=0
  if(j0==ny0  ) jp=-1
  if(j0==1 ) jp=1
  if(jp==0) then
     dista=(x-x0(i0,j0-1))**2+(y-y0(i0,j0-1))**2
     distb=(x-x0(i0,j0+1))**2+(y-y0(i0,j0+1))**2
     if(distb<dista) then
        jp=1
     else
        jp=-1
     end if
  end if

  ilast=i0
  jlast=j0
    
 end subroutine nearest_3

 subroutine get_xytilde_domain(nx0,ny0,rlons0,rlats0, &
                                  nx,ny,xminout,xmaxout,yminout,ymaxout)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_xytilde_domain
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    nx0,ny0
!    rlons0,rlats0
!
!   output argument list:
!    nx,ny
!    xminout,xmaxout,yminout,ymaxout
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   use constants, only: one,deg2rad,half,zero,r10
!  define parameters for xy domain which optimally overlays input grid

  implicit none
  integer(i_kind),intent(in   ) :: nx0,ny0
  real(r_kind)   ,intent(in   ) :: rlons0(nx0,ny0),rlats0(nx0,ny0)

  integer(i_kind),intent(  out) :: nx,ny
  real(r_kind)   ,intent(  out) :: xminout,xmaxout,yminout,ymaxout

  real(r_kind),parameter:: r37=37.0_r_kind

  real(r_kind) area,areamax,areamin,extra,rlats0max,rlats0min,testlambda
  real(r_kind) xthis,ythis
  integer(i_kind) i,ip1,j,jp1,m

  real(r_kind) coslon0(nx0,ny0),sinlon0(nx0,ny0)
  real(r_kind) coslat0(nx0,ny0),sinlat0(nx0,ny0)
  real(r_kind) count,delbar
  real(r_kind) dx,dy,disti,distj,distmin,distmax
  real(r_kind) xmin,xmax,ymin,ymax

!  get range of lats for input grid

  rlats0max=maxval(rlats0) ; rlats0min=minval(rlats0)

!   assign hemisphere ( parameter sign_pole )

  sign_pole = zero
  if(rlats0min>-r37*deg2rad) sign_pole=-one   !  northern hemisphere xy domain
  if(rlats0max< r37*deg2rad) sign_pole= one   !  southern hemisphere xy domain
  ! if neither condition satisfied (rlat0max > 37N, rlat0min < 37S), try 
  ! this... 
  if (sign_pole == zero) then 
     if (abs(rlats0max) > abs(rlats0min)) then 
        sign_pole=-one  ! NH domain 
     else 
        sign_pole=one   ! SH 
     endif 
  endif


!   get optimum rotation angle rlambda0

  areamin= huge(areamin)
  areamax=-huge(areamax)
  do m=0,359
     testlambda=m*deg2rad
     xmax=-huge(xmax)
     xmin= huge(xmin)
     ymax=-huge(ymax)
     ymin= huge(ymin)
     do j=1,ny0,ny0-1
        do i=1,nx0
           xthis=(pihalf+sign_pole*rlats0(i,j))*cos(rlons0(i,j)+testlambda)
           ythis=(pihalf+sign_pole*rlats0(i,j))*sin(rlons0(i,j)+testlambda)
           xmax=max(xmax,xthis)
           ymax=max(ymax,ythis)
           xmin=min(xmin,xthis)
           ymin=min(ymin,ythis)
        end do
     end do
     do j=1,ny0
        do i=1,nx0,nx0-1
           xthis=(pihalf+sign_pole*rlats0(i,j))*cos(rlons0(i,j)+testlambda)
           ythis=(pihalf+sign_pole*rlats0(i,j))*sin(rlons0(i,j)+testlambda)
           xmax=max(xmax,xthis)
           ymax=max(ymax,ythis)
           xmin=min(xmin,xthis)
           ymin=min(ymin,ythis)
        end do
     end do
     area=(xmax-xmin)*(ymax-ymin)
     areamax=max(area,areamax)
     if(area<areamin) then
        areamin =area
        rlambda0=testlambda
        xmaxout =xmax
        xminout =xmin
        ymaxout =ymax
        yminout =ymin
     end if
  end do


!   now determine resolution of input grid and choose nx,ny of xy grid accordingly
!                 (currently hard-wired at 1/2 the average input grid increment)

  do j=1,ny0
     do i=1,nx0
        coslon0(i,j)=cos(one*rlons0(i,j)) ; sinlon0(i,j)=sin(one*rlons0(i,j))
        coslat0(i,j)=cos(one*rlats0(i,j)) ; sinlat0(i,j)=sin(one*rlats0(i,j))
     end do
  end do

  delbar=zero
  count =zero
  do j=1,ny0-1
     jp1=j+1
     do i=1,nx0-1
        ip1=i+1
        disti=acos(sinlat0(i,j)*sinlat0(ip1,j)+coslat0(i,j)*coslat0(ip1,j)* &
                  (sinlon0(i,j)*sinlon0(ip1,j)+coslon0(i,j)*coslon0(ip1,j)))
        distj=acos(sinlat0(i,j)*sinlat0(i,jp1)+coslat0(i,j)*coslat0(i,jp1)* &
                  (sinlon0(i,j)*sinlon0(i,jp1)+coslon0(i,j)*coslon0(i,jp1)))
        distmax=max(disti,distj)
        distmin=min(disti,distj)
        delbar=delbar+distmax
        count=count+one
     end do
  end do
  delbar=delbar/count
  dx=half*delbar
  dy=dx

!   add extra space to computational grid to push any boundary problems away from
!     area of interest

  extra=r10*dx
  xmaxout=xmaxout+extra
  xminout=xminout-extra
  ymaxout=ymaxout+extra
  yminout=yminout-extra
  nx=1+(xmaxout-xminout)/dx
  ny=1+(ymaxout-yminout)/dy
 
 end subroutine get_xytilde_domain

 subroutine half_nmm_grid2a(gin,nx,ny,gout,igtype)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    half_nmm_grid2a same as half_nmm_grid2, but output not reorganized
!   prgmmr: parrish         org: w/emc2               date: 2004-06-22
!
! abstract: creates an unstaggered A grid from the staggered E grid used by the wrf nmm.
!           This is done by keeping every other row of the original E grid.  If this 
!           is a mass variable (igtype=1), then no interpolation is required.  If this
!           is a wind variable (igtype=2), then interpolation is necessary.  This procedure
!           is necessary because the gsi is not yet able to work with anything other than
!           unstaggered grids.  This solution introduces greater interpolation error
!           compared to the option fill_nmm_grid2, but has the advantage of 4 times fewer
!           grid points compared to the output of fill_nmm__grid2.  This routine will be
!           eliminated when the gsi has the capability to work directly with staggered grids.
!
! program history log:
!   2004-06-22  parrish, document
!   2005-03-03  treadon - add implicit none
!
!   input argument list:
!     gin      - input staggered E grid field over entire horizontal domain
!     nx,ny    - input grid dimensions
!     igtype   - =1, then (1,1) on staggered grid is at corner of grid (mass point for nmm)
!              - =2, then (1,1) is staggered (wind point for nmm, see illustration below)

!                   igtype=1:
!
!
!
!       ^   3             x     x     x     x
!       |
!       y   2                x     x     x     x
!
!           1             x     x     x     x
!
!                         1     2     3
!
!                           x -->

!                   igtype=2:
!
!
!
!       ^   3             x     x     x     x
!       |
!       y   2          x     x     x     x
!
!           1             x     x     x     x
!
!                         1     2     3
!
!                           x -->
!
!   output argument list
!     gout     - output unstaggered half grid  (reorganized for distibution to local domains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use constants, only: quarter
  implicit none
 
  integer(i_kind),intent(in   ) :: nx,ny,igtype
  real(r_single) ,intent(in   ) :: gin(nx,ny)
  real(r_kind)   ,intent(  out) :: gout(nx,*)

  integer(i_kind) i,i0,im,j,jj,jm,jp

  if(igtype==1) then
     jj=0
     do j=1,ny,2
        jj=jj+1
        do i=1,nx
           gout(i,jj)=real(gin(i,j),r_kind)
        end do
     end do
  else
     jj=0
     do j=1,ny,2
        jj=jj+1
        jp=j+1 ; if(jp>ny)   jp=j-1
        jm=j-1 ; if(jm<1) jm=j+1
        do i=1,nx
           im=i-1 ; if(im<1) im=i
           i0=i      ; if(i==nx)   i0=im
           gout(i,jj)=quarter*(real(gin(im,j)+gin(i0,j)+gin(i,jp)+gin(i,jm),r_kind))
        end do
     end do
  end if

 end subroutine half_nmm_grid2a

 subroutine fill_nmm_grid2a3(gin,nx,ny,gout)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fill_nmm_grid2a3
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    nx,ny
!    gin
!
!   output argument list:
!    gout
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  integer(i_kind),intent(in   ) :: nx,ny
  real(r_kind)   ,intent(in   ) :: gin(nx,ny)
  real(r_kind)   ,intent(  out) :: gout(2*nx-1,ny)
 
  integer(i_kind) i,j
  integer(i_kind) i1a(2*nx-1),i2a(2*nx-1)
  integer(i_kind) i3a(2*nx-1),i4a(2*nx-1)
  real(r_kind) r1a(2*nx-1),r2a(2*nx-1)
  real(r_kind) r3a(2*nx-1),r4a(2*nx-1)
  real(r_kind) x,x1,x2,x3,x4

!  first transfer all staggered points to appropriate
!   points on filled output grid

  do j=1,ny,2
     do i=1,nx
        gout(2*i-1,j)=gin(i,j)
     end do
  end do
  do j=2,ny,2
     do i=1,nx-1
        gout(2*i,j)=gin(i,j)
     end do
  end do

!   compute all interpolation constants for even x points on odd y rows

  i=2
  i1a(i)=i-1 ; i2a(i)=i+1 ; i3a(i)=i+3 ; i4a(i)=i+5
  x=i        ; x1=i1a(i)  ; x2=i2a(i)  ; x3=i3a(i)      ; x4=i4a(i)
  r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
  r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
  r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
  r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )

  do i=4,2*nx-4,2
     i1a(i)=i-3 ; i2a(i)=i-1 ; i3a(i)=i+1 ; i4a(i)=i+3
     x=i        ; x1=i1a(i)  ; x2=i2a(i)  ; x3=i3a(i)      ; x4=i4a(i)
     r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
     r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
     r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
     r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )
  end do

  i=2*nx-2
  i1a(i)=i-5 ; i2a(i)=i-3 ; i3a(i)=i-1 ; i4a(i)=i+1
  x=i        ; x1=i1a(i)  ; x2=i2a(i)  ; x3=i3a(i)     ; x4=i4a(i)
  r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
  r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
  r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
  r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )

!   now get all interpolation constants for odd x points on even y rows

  i=1
  i1a(i)=i+1 ; i2a(i)=i+3 ; i3a(i)=i+5 ; i4a(i)=i+7
  x=i        ; x1=i1a(i)  ; x2=i2a(i)  ; x3=i3a(i)     ; x4=i4a(i)
  r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
  r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
  r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
  r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )

  i=3
  i1a(i)=i-1 ; i2a(i)=i+1 ; i3a(i)=i+3 ; i4a(i)=i+5
  x=i        ; x1=i1a(i)  ; x2=i2a(i)  ; x3=i3a(i)         ; x4=i4a(i)
  r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
  r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
  r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
  r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )

  do i=5,2*nx-5,2
     i1a(i)=i-3 ; i2a(i)=i-1 ; i3a(i)=i+1 ; i4a(i)=i+3
     x=i        ; x1=i1a(i)  ; x2=i2a(i)  ; x3=i3a(i)         ; x4=i4a(i)
     r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
     r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
     r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
     r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )
  end do

  i=2*nx-3
  i1a(i)=i-5 ; i2a(i)=i-3 ; i3a(i)=i-1 ; i4a(i)=i+1
  x=i        ; x1=i1a(i)  ; x2=i2a(i)  ; x3=i3a(i)     ; x4=i4a(i)
  r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
  r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
  r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
  r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )

  i=2*nx-1
  i1a(i)=i-7 ; i2a(i)=i-5 ; i3a(i)=i-3 ; i4a(i)=i-1
  x=i        ; x1=i1a(i)  ; x2=i2a(i)  ; x3=i3a(i) ; x4=i4a(i)
  r1a(i)=       (x-x2)*(x-x3)*(x-x4)/(        (x1-x2)*(x1-x3)*(x1-x4))
  r2a(i)=(x-x1)       *(x-x3)*(x-x4)/((x2-x1)        *(x2-x3)*(x2-x4))
  r3a(i)=(x-x1)*(x-x2)       *(x-x4)/((x3-x1)*(x3-x2)        *(x3-x4))
  r4a(i)=(x-x1)*(x-x2)*(x-x3)       /((x4-x1)*(x4-x2)*(x4-x3)        )

  do j=1,ny,2
     do i=2,2*nx-2,2
        gout(i,j)=r1a(i)*gout(i1a(i),j)+r2a(i)*gout(i2a(i),j)+ &
                  r3a(i)*gout(i3a(i),j)+r4a(i)*gout(i4a(i),j)
     end do
  end do
  do j=2,ny,2
     do i=1,2*nx-1,2
        gout(i,j)=r1a(i)*gout(i1a(i),j)+r2a(i)*gout(i2a(i),j)+ &
                  r3a(i)*gout(i3a(i),j)+r4a(i)*gout(i4a(i),j)
     end do
  end do

 end subroutine fill_nmm_grid2a3

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  rotate_wind_ll2xy ---  Rotate earth vector wind
!
! !INTERFACE:
!
  subroutine rotate_wind_ll2xy(u0,v0,u,v,rlon0,x,y)

! !USES:

    use constants, only: one,two,one_tenth
    implicit none

! !INPUT PARAMETERS:

    real(r_kind),intent(in   ) :: u0,v0        ! earth wind component
    real(r_kind),intent(in   ) :: rlon0        ! earth   lon (radians)
    real(r_kind),intent(in   ) :: x,y          ! local x,y coordinate (grid units)

! !OUTPUT PARAMETERS:

    real(r_kind),intent(  out) :: u,v          ! rotated coordinate of winds

! !DESCRIPTION: to convert earth vector wind components to corresponding
!           local x,y coordinate
!
! !REVISION HISTORY:
!   2003-09-30  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2010-09-08  parrish, remove sign_pole variable--no longer needed, due to more accurate and
!                 robust computation of reference wind rotation angle defined by
!                 cos_beta_ref, sin_beta_ref.
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-09-30
!
!EOP
!-------------------------------------------------------------------------

  real(r_kind) beta,delx,delxp,dely,delyp
  real(r_kind) sin_beta,cos_beta
  integer(i_kind) ix,iy

!  interpolate departure from longitude part of angle between earth positive east and local positive x

  ix=x
  iy=y
  ix=max(1,min(ix,nlon-1))
  iy=max(1,min(iy,nlat-1))
  delx=x-ix
  dely=y-iy
  delxp=one-delx
  delyp=one-dely
  cos_beta=cos_beta_ref(ix  ,iy  )*delxp*delyp+cos_beta_ref(ix+1,iy  )*delx *delyp+ &
           cos_beta_ref(ix  ,iy+1)*delxp*dely +cos_beta_ref(ix+1,iy+1)*delx *dely
  sin_beta=sin_beta_ref(ix  ,iy  )*delxp*delyp+sin_beta_ref(ix+1,iy  )*delx *delyp+ &
           sin_beta_ref(ix  ,iy+1)*delxp*dely +sin_beta_ref(ix+1,iy+1)*delx *dely
  beta=atan2(sin_beta,cos_beta)

!  now rotate;

  u= u0*cos(beta-rlon0)+v0*sin(beta-rlon0)
  v=-u0*sin(beta-rlon0)+v0*cos(beta-rlon0)

 end subroutine rotate_wind_ll2xy

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  rotate_wind_xy2ll ---  Unrotate earth vector wind
!
! !INTERFACE:
!
  subroutine rotate_wind_xy2ll(u,v,u0,v0,rlon0,x,y)

! !USES:

    use constants, only: one
    implicit none

! !INPUT PARAMETERS:

    real(r_kind),intent(in   ) :: u,v         ! rotated coordinate winds
    real(r_kind),intent(in   ) :: rlon0       ! earth   lon     (radians)
    real(r_kind),intent(in   ) :: x,y         ! rotated lon/lat (radians)

! !OUTPUT PARAMETERS:

    real(r_kind),intent(  out) :: u0,v0       ! earth winds

! !DESCRIPTION: rotate u,v in local x,y coordinate to u0,v0 in earth 
!           lat, lon coordinate
!
! !REVISION HISTORY:
!   2003-09-30  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-20  todling, fixed description
!   2010-09-08  parrish, remove sign_pole variable--no longer needed, due to more accurate and
!                 robust computation of reference wind rotation angle defined by
!                 cos_beta_ref, sin_beta_ref.
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-09-30
!
!EOP
!-------------------------------------------------------------------------
  real(r_kind) beta,delx,delxp,dely,delyp
  real(r_kind) sin_beta,cos_beta
  integer(i_kind) ix,iy

!  interpolate departure from longitude part of angle between earth 
!  positive east and local positive x

  ix=x
  iy=y
  ix=max(1,min(ix,nlon-1))
  iy=max(1,min(iy,nlat-1))
  delx=x-ix
  dely=y-iy
  delxp=one-delx
  delyp=one-dely
  cos_beta=cos_beta_ref(ix  ,iy  )*delxp*delyp+cos_beta_ref(ix+1,iy  )*delx *delyp+ &
           cos_beta_ref(ix  ,iy+1)*delxp*dely +cos_beta_ref(ix+1,iy+1)*delx *dely
  sin_beta=sin_beta_ref(ix  ,iy  )*delxp*delyp+sin_beta_ref(ix+1,iy  )*delx *delyp+ &
           sin_beta_ref(ix  ,iy+1)*delxp*dely +sin_beta_ref(ix+1,iy+1)*delx *dely
  beta=atan2(sin_beta,cos_beta)

!  now rotate;

  u0= u*cos(beta-rlon0)-v*sin(beta-rlon0)
  v0= u*sin(beta-rlon0)+v*cos(beta-rlon0)

 end subroutine rotate_wind_xy2ll

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  get_ij --- get (i,j) grid indices and interpolation weights
!
! !INTERFACE:
!
 subroutine get_ij(mm1,obs_lat,obs_lon,jgrd,wgrd,jjlat,jjlon)

! !USES:

   use constants, only: one
   implicit none

! !INPUT PARAMETERS:

   integer(i_kind)             ,intent(in   ) ::  mm1
   integer(i_kind),dimension(4),intent(  out) :: jgrd
   integer(i_kind),optional    ,intent(  out) :: jjlat,jjlon

   real(r_kind)                ,intent(in   ) :: obs_lat,obs_lon
   real(r_kind),dimension(4)   ,intent(  out) :: wgrd

   integer(i_kind):: jlat,jlon
   real(r_kind):: dx,dy,dx1,dy1

! !DESCRIPTION: This routine returns the sub-domain grid relative 
!               i,j index of a given observation (lat,lon).  The
!               routine also returns weights needed for bilinear
!               from the four surrounding analysis grid points to
!               the observation location.
!
! !REVISION HISTORY:
!   2004-12-23  treadon
!   2006-01-06  treadon - add optional arguments jjlat,jjlon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------

!  Set (i,j) indices of guess gridpoint that bound obs location
   jlat = obs_lat
   jlon = obs_lon

!  Compute weights for bilinear interpolation
   dy  = obs_lat-jlat
   dx  = obs_lon-jlon
   dx1 = one-dx
   dy1 = one-dy

!  Bound lat and lon indices to fall within analysis grid limits   
   jlat = min(max(1,jlat),nlat)
   jlon = min(max(0,jlon),nlon)

!  Handle special case of e/w periodicity
   if (jstart(mm1)==1 .and. jlon==nlon) jlon=0
   if (jstart(mm1)+jlon1(mm1)==nlon+1 .and. jlon==0) jlon=nlon

!  Convert global (i,j) indices to sub-domain specific (i,j) indices
   jlat=jlat-istart(mm1)+2
   jlon=jlon-jstart(mm1)+2

   jgrd(1)=jlat+(jlon-1)*lat2
   jgrd(2)=jgrd(1)+1
   jgrd(3)=jgrd(1)+lat2
   jgrd(4)=jgrd(3)+1

   wgrd(1)=dx1*dy1
   wgrd(2)=dx1*dy
   wgrd(3)=dx *dy1
   wgrd(4)=dx *dy

   if (present(jjlat)) jjlat=jlat
   if (present(jjlon)) jjlon=jlon

   return
 end subroutine get_ij

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  get_ijk --- get (i,j,k) grid indices and interpolation weights
!
! !INTERFACE:
!
 subroutine get_ijk(mm1,obs_lat,obs_lon,obs_sig,jgrd,wgrd)

! !USES:

   use constants, only: one
   implicit none

! !INPUT PARAMETERS:

   integer(i_kind)             ,intent(in   ) ::  mm1
   integer(i_kind),dimension(8),intent(  out) :: jgrd

   real(r_kind)                ,intent(in   ) :: obs_lat,obs_lon,obs_sig
   real(r_kind)   ,dimension(8),intent(  out) :: wgrd

   integer(i_kind):: jlat,jlon,jsig,latlon11_l
   real(r_kind) :: dx,dy,dx1,dy1,ds,ds1

! !DESCRIPTION: This routine returns the sub-domain grid relative
!               i,j,k index of a given observation (lat,lon,sig).  
!               The routine also returns weights needed for bilinear
!               from the eight surrounding analysis grid points to
!               the observation location
!
! !REVISION HISTORY:
!   2004-12-23  treadon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-08-27
!
!EOP
!-------------------------------------------------------------------------
!  Declare local variables
   real(r_kind) obs_s


!  Special handling for vertical coordinate
   obs_s = obs_sig
   if (obs_s < one) obs_s = one

!  Set (i,j,k) indices of guess gridpoint that bound obs location
   jlat = obs_lat
   jlon = obs_lon
   jsig = obs_s

!  Compute weights for bilinear interpolation
   dy  = obs_lat-jlat
   dx  = obs_lon-jlon
   ds  = obs_s-jsig

   dx1 = one-dx
   dy1 = one-dy
   ds1 = one-ds

!  Bound lat and lon indices to fall within analysis grid limits   
   jlat = min(max(1,jlat),nlat)
   jlon = min(max(0,jlon),nlon)

!  Handle special case of e/w periodicity
   if (jstart(mm1)==1 .and. jlon==nlon) jlon=0
   if (jstart(mm1)+jlon1(mm1)==nlon+1 .and. jlon==0) jlon=nlon

!  Convert global (i,j) indices to sub-domain specific (i,j) indices
   jlat=jlat-istart(mm1)+2
   jlon=jlon-jstart(mm1)+2

!  Set number of points on horizontal layer
   latlon11_l = latlon11
   if(jsig==nsig) latlon11_l=0
   jgrd(1)=jlat+(jlon-1)*lat2+(jsig-1)*latlon11
   jgrd(2)=jgrd(1)+1
   jgrd(3)=jgrd(1)+lat2
   jgrd(4)=jgrd(3)+1
   jgrd(5)=jgrd(1)+latlon11_l
   jgrd(6)=jgrd(5)+1
   jgrd(7)=jgrd(5)+lat2
   jgrd(8)=jgrd(7)+1

   wgrd(1)=dx1*dy1*ds1
   wgrd(2)=dx1*dy *ds1
   wgrd(3)=dx *dy1*ds1
   wgrd(4)=dx *dy *ds1
   wgrd(5)=dx1*dy1*ds
   wgrd(6)=dx1*dy *ds
   wgrd(7)=dx *dy1*ds
   wgrd(8)=dx *dy *ds

   return
 end subroutine get_ijk

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  reorder --- reorder work array post mpi communication
!
! !INTERFACE:
!
  subroutine reorder(work,k_in,k_use)

! !USES:

    use kinds, only: r_kind
    use constants, only: zero
    use mpimod, only: npe
    implicit none

! !INPUT PARAMETERS:

   integer(i_kind)                                  , intent(in   ) ::  k_in, k_use    ! number of levs in work array

! !INPUT/OUTPUT PARAMETERS:

   real(r_kind),dimension(max(iglobal,itotsub)*k_in), intent(inout) :: work ! array to reorder

! !OUTPUT PARAMETERS:

! !DESCRIPTION: reorder work array post mpi communication
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-03-30  treadon - replace itotsub with max(iglobal,itotsub) in work dimension
!
! !REMAKRS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) iloc,iskip,i,k,n
    real(r_kind),dimension(max(iglobal,itotsub),k_use):: temp

! Zero out temp array
!   do k=1,k_use
!      do i=1,itotsub
!         temp(i,k)=zero
!      end do
!   end do
 
! Load temp array in desired order
    do k=1,k_use
       iskip=0
       iloc=0
       do n=1,npe
          do i=1,ijn(n)
             iloc=iloc+1
             temp(iloc,k)=work(i + iskip + (k-1)*ijn(n))
          end do
          iskip=iskip+ijn(n)*k_in
       end do
    end do

! Load the temp array back into work
    iloc=0
    do k=1,k_use
       do i=1,itotsub
          iloc=iloc+1
          work(iloc)=temp(i,k)
       end do
    end do

    return
  end subroutine reorder

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  reorder2 --- reorder work array post mpi communication
!
! !INTERFACE:
!

  subroutine reorder2(work,k_in,k_use)

! !USES:

    use kinds, only: r_kind
    use mpimod, only: npe
    implicit none


! !INPUT PARAMETERS:

   integer(i_kind)                     , intent(in   ) ::  k_in,k_use    ! number of levs in work array

! !INPUT/OUTPUT PARAMETERS:

   real(r_kind),dimension(itotsub,k_in), intent(inout) :: work

! !OUTPUT PARAMETERS:

! !DESCRIPTION: reorder work array pre mpi communication
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) iloc,iskip,i,k,n
    real(r_kind),dimension(itotsub*k_in):: temp

! Load temp array in order of subdomains
    iloc=0
    iskip=0
    do n=1,npe

       do k=1,k_use
          do i=1,ijn_s(n)
             temp(iloc+i)=work(iskip+i,k)
          end do
          iloc=iloc+ijn_s(n)
       end do
       iloc=iloc+(k_in-k_use)*ijn_s(n)
       iskip=iskip+ijn_s(n)
    end do

! Now load the tmp array back into work
    iloc=0
    do k=1,k_in
       do i=1,itotsub
          iloc=iloc+1
          work(i,k)=temp(iloc)
       end do
    end do

    return
  end subroutine reorder2

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  strip_single_rank33 --- strip off buffer points froms subdomains
!                       for mpi comm purposes (works with 4 byte reals)
!
! !INTERFACE:
!
  subroutine strip_single_rank33_(field_in,field_out,nz)

! !USES:

    use kinds, only: r_single
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind)                       , intent(in   ) :: nz         !  number of levs in subdomain array
    real(r_single),dimension(lat2,lon2,nz), intent(in   ) :: field_in   ! full subdomain 
                                                                        !    array containing 
                                                                        !    buffer points
! !OUTPUT PARAMETERS:

    real(r_single),dimension(lat1,lon1,nz), intent(  out) :: field_out ! subdomain array
                                                                       !   with buffer points
                                                                       !   stripped off

! !DESCRIPTION: strip off buffer points froms subdomains for mpi comm
!               purposes
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,k,jp1

    do k=1,nz
       do j=1,lon1
          jp1 = j+1
          do i=1,lat1
             field_out(i,j,k)=field_in(i+1,jp1,k)
          end do
       end do
    end do

    return
  end subroutine strip_single_rank33_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  strip_single_rank21 --- strip off buffer points froms subdomains
!                       for mpi comm purposes (works with 4 byte reals)
!
! !INTERFACE:
!
  subroutine strip_single_rank21_(field_in,field_out)

! !USES:

    use kinds, only: r_single
    implicit none

! !INPUT PARAMETERS:

    real(r_single),dimension(lat2,lon2), intent(in   ) :: field_in   ! full subdomain 
                                                                     !    array containing 
                                                                     !    buffer points
! !OUTPUT PARAMETERS:

    real(r_single),dimension(lat1*lon1), intent(  out) :: field_out ! subdomain array
                                                                    !   with buffer points
                                                                    !   stripped off

! !DESCRIPTION: strip off buffer points froms subdomains for mpi comm
!               purposes
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,ij,jp1

    ij=0
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ij = ij+1
          field_out(ij)=field_in(i+1,jp1)
       end do
    end do

    return
  end subroutine strip_single_rank21_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  strip_double_rank33 --- strip off buffer points froms subdomains
!                       for mpi comm purposes
!
! !INTERFACE:
!
  subroutine strip_double_rank33_(field_in,field_out,nz)

! !USES:

    use kinds, only: r_double
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind)                       , intent(in   ) :: nz          !  number of levs in subdomain array
    real(r_double),dimension(lat2,lon2,nz), intent(in   ) :: field_in    ! full subdomain 
                                                                         !    array containing 
                                                                         !    buffer points
! !OUTPUT PARAMETERS:

    real(r_double),dimension(lat1,lon1,nz), intent(  out) :: field_out  ! subdomain array
                                                                        !   with buffer points
                                                                        !   stripped off

! !DESCRIPTION: strip off buffer points froms subdomains for mpi comm
!               purposes
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2013-10-24  todling create general interface (single/double)
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,k,jp1

    do k=1,nz
       do j=1,lon1
          jp1 = j+1
          do i=1,lat1
             field_out(i,j,k)=field_in(i+1,jp1,k)
          end do
       end do
    end do

    return
  end subroutine strip_double_rank33_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  strip_double_rank22 --- strip off buffer points froms subdomains
!                       for mpi comm purposes
!
! !INTERFACE:
!
  subroutine strip_double_rank22_(field_in,field_out)

! !USES:

    use kinds, only: r_double
    implicit none

! !INPUT PARAMETERS:

    real(r_double),dimension(lat2,lon2), intent(in   ) :: field_in    ! full subdomain 
                                                                         !    array containing 
                                                                         !    buffer points
! !OUTPUT PARAMETERS:

    real(r_double),dimension(lat1,lon1), intent(  out) :: field_out  ! subdomain array
                                                                        !   with buffer points
                                                                        !   stripped off

! !DESCRIPTION: strip off buffer points froms subdomains for mpi comm
!               purposes
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2013-10-24  todling create general interface (single/double)
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,jp1

    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          field_out(i,j)=field_in(i+1,jp1)
       end do
    end do

    return
  end subroutine strip_double_rank22_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  strip_double_rank32 --- strip off buffer points froms subdomains
!                       for mpi comm purposes
!
! !INTERFACE:
!
  subroutine strip_double_rank32_(field_in,field_out,nz)

! !USES:

    use kinds, only: r_double
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind)                       , intent(in   ) :: nz          !  number of levs in subdomain array
    real(r_double),dimension(lat2,lon2,nz), intent(in   ) :: field_in    ! full subdomain 
                                                                         !    array containing 
                                                                         !    buffer points
! !OUTPUT PARAMETERS:

    real(r_double),dimension(lat1*lon1,nz), intent(  out) :: field_out  ! subdomain array
                                                                        !   with buffer points
                                                                        !   stripped off

! !DESCRIPTION: strip off buffer points froms subdomains for mpi comm
!               purposes
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2013-10-24  todling create general interface (single/double)
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,k,ij,jp1

    do k=1,nz
       ij=0
       do j=1,lon1
          jp1 = j+1
          do i=1,lat1
             ij = ij+1
             field_out(ij,k)=field_in(i+1,jp1,k)
          end do
       end do
    end do

    return
  end subroutine strip_double_rank32_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  strip_double_rank21 --- strip off buffer points froms subdomains
!                       for mpi comm purposes
!
! !INTERFACE:
!
  subroutine strip_double_rank21_(field_in,field_out)

! !USES:

    use kinds, only: r_double
    implicit none

! !INPUT PARAMETERS:

    real(r_double),dimension(lat2,lon2), intent(in   ) :: field_in    ! full subdomain 
                                                                         !    array containing 
                                                                         !    buffer points
! !OUTPUT PARAMETERS:

    real(r_double),dimension(lat1*lon1), intent(  out) :: field_out  ! subdomain array
                                                                        !   with buffer points
                                                                        !   stripped off

! !DESCRIPTION: strip off buffer points froms subdomains for mpi comm
!               purposes
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2013-10-24  todling create general interface (single/double)
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,ij,jp1

    ij=0
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ij = ij+1
          field_out(ij)=field_in(i+1,jp1)
       end do
    end do

    return
  end subroutine strip_double_rank21_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  strip_double_rank11 --- strip off buffer points froms subdomains
!                       for mpi comm purposes
!
! !INTERFACE:
!
  subroutine strip_double_rank11_(field_in,field_out)

! !USES:

    use kinds, only: r_double
    implicit none

! !INPUT PARAMETERS:

    real(r_double),dimension(lat2*lon2), intent(in   ) :: field_in    ! full subdomain 
                                                                         !    array containing 
                                                                         !    buffer points
! !OUTPUT PARAMETERS:

    real(r_double),dimension(lat1*lon1), intent(  out) :: field_out  ! subdomain array
                                                                        !   with buffer points
                                                                        !   stripped off

! !DESCRIPTION: strip off buffer points froms subdomains for mpi comm
!               purposes
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2013-10-24  todling create general interface (single/double)
!   2014-08-21  pondeca - replace lat1 with lat2 in calculation for iji
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!    kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,ijo,iji,jp1

    ijo=0
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ijo = ijo+1
          iji = (i+1)+(jp1-1)*lat2
          field_out(ijo)=field_in(iji) !(i+1,jp1)
       end do
    end do

    return
  end subroutine strip_double_rank11_


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  vectosub --- transform vector array into three dimensional 
!                          subdomain array
!
! !INTERFACE:
!
  subroutine vectosub(fld_in,npts,fld_out)

    use kinds, only: r_kind
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind)             , intent(in   ) :: npts   ! number of levs in subdomain array
    real(r_kind),dimension(npts), intent(in   ) :: fld_in ! subdomain array 
                                                          !   in vector form

! !OUTPUT PARAMETERS:

    real(r_kind),dimension(npts), intent(  out) :: fld_out ! three dimensional 
                                                           !  subdomain variable array

! !DESCRIPTION: Transform vector array into three dimensional subdomain
!               array
!
! !REVISION HISTORY:
!
!   2004-01-25  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!   kleist           org: np20                date: 2004-01-25
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) k

    do k=1,npts
       fld_out(k)=fld_in(k)
    end do

    return
  end subroutine vectosub

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  reload --- Transfer contents of 2-d array to 3-d array
!
! !INTERFACE:
!
subroutine reload(work_in,work_out)

! !USES:

  use kinds, only: r_kind
  implicit none

! !INPUT PARAMETERS:

  real(r_kind),dimension(lat2*lon2,nsig),intent(in   ) :: work_in   ! 2-d array

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(lat2,lon2,nsig),intent(  out) :: work_out  ! 3-d array

! !DESCRIPTION: Transfer contents of 2-d array to 3-d array
!
! !REVISION HISTORY:
!   2004-05-14  treadon
!   2004-07-15  todling, protex-compliant prologue
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR: 
!   treadon          org: np23                date: 2004-05-14
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind) i,j,k,ij

  do k=1,nsig
     ij=0
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           work_out(i,j,k)=work_in(ij,k)
        end do
     end do
  end do
  return
end subroutine reload

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  strip_periodic --- strip off buffer points from periodic
!                       subdomains for mpi comm purposes
!
! !INTERFACE:
!
  subroutine strip_periodic(field_in,field_out,nz)

! !USES:

    use kinds, only: r_kind
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind)                     , intent(in   ) :: nz         !  number of levs in subdomain array
    real(r_kind),dimension(lat2,lon2,nz), intent(in   ) :: field_in   ! full subdomain
                                                                      !    array containing
                                                                      !    buffer points
! !OUTPUT PARAMETERS:

    real(r_kind),dimension(lat1,lon1,nz), intent(  out) :: field_out ! subdomain array
                                                                     !   with buffer points
                                                                     !   stripped off

! !DESCRIPTION: strip off buffer points froms subdomains for mpi comm
!               purposes
!
! !REVISION HISTORY:
!
!   2004-07-23  treadon
!   2004-08-04  treadon - protex-compliant prologue
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!    treadon           org: np20                date: 2004-07-23
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,k,jp1

    do k=1,nz
       do j=1,lon1
          jp1 = j+1
          do i=1,lat1
             field_out(i,j,k)=field_in(i+1,jp1,k)
          end do
       end do
    end do
    do k=1,nz
       do i=1,lat1
          field_out(i,1,k)    = field_out(i,1,k)    + field_in(i+1,lon2,k)
          field_out(i,lon1,k) = field_out(i,lon1,k) + field_in(i+1,1,k)
       end do
    end do

    return
  end subroutine strip_periodic

end module gridmod

  subroutine ll2rpolar(rlat,rlon,n,x_rpolar,y_rpolar,rlat0,rlon0,rotate3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ll2rpolar  convert earth lat-lon to rotated polar stereo
!   prgmmr:
!
! abstract:  Convert earth latitude and longitude to polar stereographic coordinates where
!               the reference pole is centered at earth coordinates rlat0,rlon0.
!               The polar stereographic positive x axis is oriented counterclockwise to 
!               earth direction south at rlat0,rlon0 by angle rotate3.  The transformed
!               lat-lon coordinate which is the basis of the polar stereographic coordinate consists
!               of a sequence of 3 rotations in 3-d x-y-z space with origin at center of earth, where
!               the x-y plane intersects the equator with the positive x axis intersecting the 0 meridian.
!               and the positive y axis the 90E meridian.  The positive z axis intersects the north pole.
!               1st rotation: counterclockwise in x-y plane by amount rlon0.
!               2nd rotation: counterclockwise in z-x plane by amount pi/2 - rlat0.
!               3rd rotation: counterclockwise in x-y plane by amount rotate3.
!
! program history log:
!   2010-09-09  parrish - initial documentation
!
!   input argument list:
!    rlat,rlon:    input earth latitude and longitude coordinates in radians
!    n:            number of points to compute transform coordinates
!    rlat0,rlon0:  earth coordinates of north pole of new coordinate system
!    rotate3:      angle counterclockwise from earth direction south at rlat0,rlon0 to positive x axis
!                     of output coordinates
!
!   output argument list:
!    x_rpolar,y_rpolar:  x and y polar stereographic coordinates of new coordinate with origin at
!                           rlat0,rlon0.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   use kinds, only: r_kind,i_kind
   use constants, only: zero,one,two

   implicit none

   integer(i_kind),intent(in)::n
   real(r_kind),intent(in)::rlat(n),rlon(n)
   real(r_kind),intent(in)::rlat0,rlon0,rotate3
   real(r_kind),intent(out)::x_rpolar(n),y_rpolar(n)

!  Declare local variables
   integer(i_kind) i
   real(r_kind) clat0,slat0,clon0,slon0
   real(r_kind) clat(n),slat(n),clon(n),slon(n)
   real(r_kind) x,y,z,xt,yt,zt,x2,y2,z2,rlat2,rlon2,epstest,r_polar

   epstest=epsilon(epstest)
   clat0=cos(rlat0) ; slat0=sin(rlat0)
   clon0=cos(rlon0) ; slon0=sin(rlon0)
   clat =cos(rlat ) ; slat =sin(rlat )
   clon =cos(rlon ) ; slon =sin(rlon )

   do i=1,n
      x=clat(i)*clon(i) ; y=clat(i)*slon(i) ; z=slat(i)
      xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
      z2=zt*slat0+xt*clat0 ; x2 = -zt*clat0 + xt*slat0 ; y2 = yt
      z2=min(one,max(-one,z2))
      rlat2=asin(z2)
      if(sqrt(y2**2+x2**2) < epstest) then
         rlon2=zero
      else
         rlon2=atan2(y2,x2)
      end if
      r_polar=cos(rlat2)/(one+sin(rlat2))
      x_rpolar(i)=r_polar*cos(rlon2-rotate3)
      y_rpolar(i)=r_polar*sin(rlon2-rotate3)

   end do

  end subroutine ll2rpolar

  subroutine rpolar2ll(x_rpolar,y_rpolar,n,rlat,rlon,rlat0,rlon0,rotate3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rpolar2ll  inverse of ll2rpolar
!   prgmmr:
!
! abstract:  Inverse transformation of subroutine ll2rpolar.
!
! program history log:
!   2010-09-09  parrish - initial documentation
!
!   input argument list:
!    x_rpolar,y_rpolar:  x and y polar stereographic coordinates of new coordinate with origin at
!                           rlat0,rlon0.
!    n:            number of points to compute transform coordinates
!    rlat0,rlon0:  earth coordinates of north pole of new coordinate system
!    rotate3:      angle counterclockwise from earth direction south at rlat0,rlon0 to positive x axis
!                     of output coordinates
!
!   output argument list:
!    rlat,rlon:    input earth latitude and longitude coordinates in radians
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   use kinds, only: r_kind,i_kind
   use constants, only: zero,one,two,quarter,pi

   implicit none

   integer(i_kind),intent(in)::n
   real(r_kind),intent(in)::x_rpolar(n),y_rpolar(n)
   real(r_kind),intent(in)::rlat0,rlon0,rotate3
   real(r_kind),intent(out)::rlat(n),rlon(n)

!  Declare local variables
   integer(i_kind) i
   real(r_kind) clat0,slat0,clon0,slon0
   real(r_kind) x,y,z,xt,yt,zt,x2,y2,z2,rlat2,rlon2,epstest,r_polar,pi_quarter
   real(r_kind) slat2,clat2,slon2,clon2

   epstest=epsilon(epstest)
   pi_quarter=quarter*pi
   clat0=cos(rlat0) ; slat0=sin(rlat0)
   clon0=cos(rlon0) ; slon0=sin(rlon0)

   do i=1,n
      r_polar=sqrt(x_rpolar(i)**2+y_rpolar(i)**2)
      rlat2=two*(pi_quarter-atan(r_polar))
      slat2=sin(rlat2) ; clat2=cos(rlat2)
      if(r_polar < epstest) then
         rlon2=rotate3
      else
         rlon2=atan2(y_rpolar(i),x_rpolar(i))+rotate3
      end if
      slon2=sin(rlon2) ; clon2=cos(rlon2)
      x2=clat2*clon2 ; y2=clat2*slon2 ; z2=slat2
      zt=slat0*z2-clat0*x2 ; xt=clat0*z2+slat0*x2 ; yt=y2
      x=xt*clon0-yt*slon0 ; y=xt*slon0+yt*clon0 ; z=zt
      z=min(one,max(-one,z))
      rlat(i)=asin(z)
      if(sqrt(x**2+y**2) < epstest) then
         rlon(i)=zero
      else
         rlon(i)=atan2(y,x)
      end if
   end do

  end subroutine rpolar2ll
