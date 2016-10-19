!***********************************************************************
!*                   GNU General Public License                        *
!* This file is a part of fvGFS.                                       *
!*                                                                     *
!* fvGFS is free software; you can redistribute it and/or modify it    *
!* and are expected to follow the terms of the GNU General Public      *
!* License as published by the Free Software Foundation; either        *
!* version 2 of the License, or (at your option) any later version.    *
!*                                                                     *
!* fvGFS is distributed in the hope that it will be useful, but        *
!* WITHOUT ANY WARRANTY; without even the implied warranty of          *
!* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
!* General Public License for more details.                            *
!*                                                                     *
!* For the full text of the GNU General Public License,                *
!* write to: Free Software Foundation, Inc.,                           *
!*           675 Mass Ave, Cambridge, MA 02139, USA.                   *
!* or see:   http://www.gnu.org/licenses/gpl.html                      *
!***********************************************************************

module gfs_physics_driver_mod

!-----------------------------------------------------------------------
!    gfs_physics_driver_mod defines the GFS physics routines used by
!    the GFDL FMS system to obtain tendencies and boundary fluxes due 
!    to the physical parameterizations and processes that drive 
!    atmospheric time tendencies for use by other components, namely
!    the atmospheric dynamical core.
!
!    NOTE: This module currently supports only the operational GFS
!          parameterizations as of September 2015.  Further development
!          is needed to support the full suite of physical 
!          parameterizations present in the GFS physics package.
!-----------------------------------------------------------------------
!
!--- FMS/GFDL modules ---
  use block_control_mod,  only: block_control_type
  use diag_manager_mod,   only: register_diag_field, send_data, &
                                diag_send_complete_extra
  use mpp_mod,            only: input_nml_file, mpp_pe, mpp_root_pe, &
                                mpp_error, mpp_chksum, mpp_min, mpp_max
  use field_manager_mod,  only: MODEL_ATMOS
  use fms_mod,            only: fms_init, stdout, stdlog, string,     &
                                open_namelist_file, check_nml_error,  &
                                file_exist, open_file, close_file,    &
                                error_mesg, FATAL, WARNING, NOTE,     &
                                write_version_number, field_exist
  use fms_io_mod,         only: restart_file_type, free_restart_type, &
                                register_restart_field,               &
                                restore_state, save_restart,          &
                                get_mosaic_tile_file, read_data
  use mpp_domains_mod,    only: domain2d
  use time_manager_mod,   only: time_type, get_date, get_time, operator(-)
  use tracer_manager_mod, only: get_number_tracers

  use machine,            only: kind_phys
!
!--- NUOPC GFS Physics module routines ---
  use nuopc_physics,      only: nuopc_phys_init, nuopc_phys_run, &
                                nuopc_rad_run, nuopc_rad_update
!
!--- NUOPC GFS Physics module datatypes ---
  use nuopc_physics,      only: state_fields_in, state_fields_out,      &
                                model_parameters, dynamic_parameters,  &
                                sfc_properties, cloud_properties,       &
                                radiation_tendencies, interface_fields, &
                                diagnostics, tbd_ddt
!
!--- GFS Physics share module ---
  use physcons,           only: pi => con_pi
  use physcons,           only: max_lon, max_lat
  use physcons,           only: min_lon, min_lat
  use physcons,           only: dxmax, dxmin, dxinv, con_g
  use physparam,          only: ipsd0
  use mersenne_twister,   only: random_setseed, random_index, random_stat
  use ozne_def,           only: pl_pres, ozplin
!
!--- variables needed for calculating 'sncovr'
  use namelist_soilveg,   only: salp_data, snupx
!
!-----------------------------------------------------------------------
  implicit none
  private
!
!--- public interfaces ---
  public  phys_rad_driver_init, phys_rad_setup_step, radiation_driver, &
          physics_driver, phys_rad_driver_restart, phys_rad_driver_end
!
!--- public debug interfaces ---
  public skin_temp
!
!--- public NUOPC GFS datatypes and data typing ---
  public  state_fields_in, state_fields_out, kind_phys
!
!--- data type definition needed for prognostic ozone interpolation
  type ozone_data
    private
    integer,              dimension(:), allocatable :: j1
    integer,              dimension(:), allocatable :: j2
    real(kind=kind_phys), dimension(:), allocatable :: ddy
    real(kind=kind_phys), dimension(:), allocatable :: gaul
  end type ozone_data
!
  type(ozone_data), dimension(:), allocatable :: O3dat
!
!-----------------------------------------------------------------------
!--- MODULE PRIVATE DATA ---
!-----------------------------------------------------------------------
!
!--- NUOPC data types
  type(model_parameters) :: Mdl_parms
  type(tbd_ddt),              dimension(:), allocatable :: Tbd_data
  type(dynamic_parameters),   dimension(:), allocatable :: Dyn_parms
  type(diagnostics),          dimension(:), allocatable :: Gfs_diags
  type(sfc_properties),       dimension(:), allocatable :: Sfc_props
  type(cloud_properties),     dimension(:), allocatable :: Cld_props
  type(radiation_tendencies), dimension(:), allocatable :: Rad_tends
  type(interface_fields),     dimension(:), allocatable :: Intr_flds
!
!--- GFDL FMS netcdf restart data types
  type(restart_file_type) :: Oro_restart
  type(restart_file_type) :: Sfc_restart
!
!--- GFDL FMS restart containers
  character(len=32),    allocatable,         dimension(:)       :: oro_name2, sfc_name2, sfc_name3
  real(kind=kind_phys), allocatable, target, dimension(:,:,:)   :: oro_var2, sfc_var2
  real(kind=kind_phys), allocatable, target, dimension(:,:,:,:) :: sfc_var3
  real(kind=kind_phys), allocatable, target, dimension(:,:,:)   :: phy_f2d
  real(kind=kind_phys), allocatable, target, dimension(:,:,:,:) :: phy_f3d
!
!--- data type definition for use as a subtype within gfdl_diag_type
  type diag_data_type
    integer :: is
    integer :: js
    real(kind=kind_phys), dimension(:,:),   pointer :: var2 => NULL()
    real(kind=kind_phys), dimension(:,:),   pointer :: var21 => NULL()
  end type diag_data_type
!
!--- data type definition for use with GFDL FMS diagnostic manager
  type gfdl_diag_type
    private
    integer :: id
    integer :: axes
    character(len=64)  :: mod_name
    character(len=64)  :: name
    character(len=128) :: desc
    character(len=64)  :: unit
    real(kind=kind_phys) :: cnvfac
    type(diag_data_type), dimension(:), allocatable  :: data
   end type gfdl_diag_type
!
   integer :: tot_diag_idx = 0 
   integer, parameter :: DIAG_SIZE = 250
   type(gfdl_diag_type), dimension(DIAG_SIZE) :: Diag
   real(kind=kind_phys), parameter :: missing_value = 1.d30     ! netcdf missing value
!
!--- miscellaneous other variables
  logical :: module_is_initialized = .FALSE.
  integer :: lat_cs, lon_cs
!
!--- GFS physics NAMELIST/CONFIGURATION parameters
!--- convenience variables
    integer :: lonr, latr
    integer :: nsswr, nslwr   ! trigger aid for radiation
    integer :: nscyc          ! trigger aid for surface cycle
!
!--- GFS initialization variables
    integer :: ipt = 1 
    integer :: latgfs = 1
    real(kind=kind_phys) :: xkzm_m = 1.0     ! SJL note: backbround momentum diffusion
    real(kind=kind_phys) :: xkzm_h = 1.0     ! SJL note:              heat/q diffusion
    real(kind=kind_phys) :: xkzm_s = 1.0
    real(kind=kind_phys) :: evpco = 2.0e-5
    real(kind=kind_phys) :: psautco(2) = (/6.0e-4,3.0e-4/)
    real(kind=kind_phys) :: prautco(2) = (/1.0e-4, 1.0e-4/)
    real(kind=kind_phys) :: wminco(2) = (/1.0e-5,1.0e-5/)
    real(kind=kind_phys) :: clstp
    real(kind=kind_phys) :: sup = 1.1
    real(kind=kind_phys) :: fhswr = 3600.
    real(kind=kind_phys) :: fhlwr = 3600.
    logical :: lprnt = .false.   ! control flag for diagnostic print out (rad)
    logical :: lssav = .true.    ! logical flag for store 3-d cloud field
!
!--- GFS standard namelist parameters
!--- for full functionality, would need to be included in the namelist definition
    integer :: NFXR     = 39
    integer :: ncld     = 1
    integer :: ntcw     = 2
    integer :: ntoz     = 4
    logical :: ozcalc   = .false.
    logical :: nocnv    = .false.
    integer :: levs     = 63
    integer :: levr     = 63
    integer :: ncols    = 3538944         ! total number of 13km physics columns
    integer :: me                         ! set by call to mpp_pe
    integer :: lsoil    = 4
    integer :: lsm      = 1               ! NOAH LSM
    integer :: nmtvr    = 14
    integer :: nrcm     = 2               ! when using ras, will be computed
    integer :: levozp   = 80              ! read from global_o3prdlos.f77
    integer :: jcap     = 1               ! should not matter it is used by spherical 
    integer :: num_p3d  = 4               ! Ferrier:3  Zhao:4 
    integer :: num_p2d  = 3               ! Ferrier:1  Zhao:3
    integer :: npdf3d   = 0               ! Zhao & pdfcld=.T.:3  -  else:0
    integer :: pl_coeff = 4
    integer :: ncw(2)   = (/20,120/)
    real (kind=kind_phys) :: flgmin(2) = (/0.180,0.220/)
    real (kind=kind_phys) :: crtrh(3) = (/0.90,0.90,0.90/)
    real (kind=kind_phys) :: cdmbgwd(2) = (/2.0,0.25/)  ! GWD - adjust for sens
    real (kind=kind_phys) :: ccwf(2) = (/1.0,1.0/)
    real (kind=kind_phys) :: dlqf(2) = (/0.0,0.0/)
    real (kind=kind_phys) :: ctei_rm(2) = (/10.0,10.0/)
    real (kind=kind_phys) :: cgwf(2) = (/0.5,0.05/)
    real (kind=kind_phys) :: prslrd0 = 200.
    logical :: ras          = .false.
    logical :: pre_rad      = .false.
    logical :: ldiag3d      = .false.
    logical :: lgocart      = .false. 
    logical :: cplflx       = .false.  
    logical :: lssav_cpl    = .false.
    logical :: flipv        = .true.
    logical :: old_monin    = .false. 
    logical :: cnvgwd       = .true.
    logical :: shal_cnv     = .false.
    logical :: sashal       = .true.
    logical :: newsas       = .true.
    logical :: cal_pre      = .true.
    logical :: mom4ice      = .false.
    logical :: mstrat       = .false.
    logical :: trans_trac   = .true.
    integer :: nst_fcst     = 0
    logical :: moist_adj    = .false.     ! must be true to turn on moist convective
    integer :: thermodyn_id = 1           ! idvm/10
    integer :: sfcpress_id  = 2           ! idvm-(idvm/10)*10
    logical :: gen_coord_hybrid = .false. ! in scrpt, could be T or F
    logical :: lsidea       = .false.  
    logical :: pdfcld       = .false.
    logical :: shcnvcw      = .false.
    logical :: redrag       = .true.
    logical :: hybedmf      = .false.
    logical :: dspheat      = .false.
    logical :: cscnv        = .false.
    integer :: nctp       = 20
    integer :: ntke       = 0
    logical :: do_shoc    = .false.
    logical :: shocaftcnv = .false.
    integer :: ntot3d     = 4
    integer :: ntot2d     = 3
    logical :: shoc_cld   = .false.
!
!--- Radiation option control parameters
    integer :: ictm     = 1 
    integer :: isol     = 2
    integer :: ico2     = 2 
    integer :: iaer     = 111
    integer :: ialb     = 0 
    integer :: iems     = 0 
    integer :: iovr_sw  = 1
    integer :: iovr_lw  = 1
    integer :: isubc_sw = 2
    integer :: isubc_lw = 2
    logical :: crick_proof  = .false.
    logical :: ccnorm       = .false.
    logical :: norad_precip = .false.     ! This is effective only for Ferrier/Moorthi
!
!--- rad_save
    integer :: iflip = 1                  ! surface to toa
!
!--- interface props
    logical :: SW0 = .false.
    logical :: SWB = .false.
    logical :: LW0 = .false.
    logical :: LWB = .false.
!
!--- standard debug flag 
    logical :: debug = .false.
!
!--- controls for GFS diagnostic output
    real(kind=kind_phys), dimension(2048) :: fdiag
    real(kind=kind_phys) :: fhzero = 6.
!
!--- control for surface data cycling
    real(kind=kind_phys) :: fhcyc = 0.
!
!---
    logical :: use_ufo = .true.
    logical :: nst_anl = .false.
!
!--- namelist definition ---
   namelist /gfs_physics_nml/ norad_precip,debug,levs,fhswr,fhlwr,ntoz,ntcw,     &
                              ozcalc,cdmbgwd,fdiag,fhzero,fhcyc,use_ufo,nst_anl, &
                              prslrd0,xkzm_m,xkzm_h,xkzm_s,nocnv,ncols,dspheat,  &
                              hybedmf,shal_cnv
!-----------------------------------------------------------------------

  CONTAINS

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!                     PUBLIC SUBROUTINES
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!-------------------------------------------------------------------------      
!--- phys_rad_driver_init ---
!-------------------------------------------------------------------------      
!    constructor for gfs_physics_driver_mod
!-------------------------------------------------------------------------      
  subroutine phys_rad_driver_init (Time, Time_init, lon, lat, glon, glat, npz, axes, area,  &
                                   dt_phys, Atm_block, State_in, State_out, fv_domain)
!--- subroutine interface variable definitions
    type(time_type),           intent(in) :: Time, Time_init
    type (block_control_type), intent(in) :: Atm_block
    !--  set "one"-based arrays to domain-based
    real(kind=kind_phys), dimension(Atm_block%isc:,Atm_block%jsc:), intent(in) :: lon, lat, area
    integer,                   intent(in) :: glon, glat, npz
    integer, dimension(4),     intent(in) :: axes
    real (kind=kind_phys),     intent(in) :: dt_phys
    type (state_fields_in),    dimension(:), intent(inout) :: State_in
    type (state_fields_out),   dimension(:), intent(inout) :: State_out
    type (domain2d),           intent(in) :: fv_domain
!--- local variables
    integer :: ierr, io, unit, logunit, outunit
    integer :: nb, ibs, ibe, jbs, jbe, ngptc
    integer :: i, j, ix, ntrac, ntp, sec, days, kdt, nnp
    integer :: id1, id2
    integer :: jdate(8) = (/1, 1, 1, 0, 0, 0, 0, 0/)
    integer :: idate(4) = (/0, 1, 1, 1/)
    real(kind=kind_phys) :: dt_diag
    real(kind=kind_phys) :: solhr = 0.0   
    real(kind=kind_phys) :: fhour = 0.
    logical :: sas_shal
    real (kind=kind_phys) :: si(64)
    data si  /1.000000,      0.984375,      0.968750,      &
              0.953125,      0.937500,      0.921875,      &
              0.906250,      0.890625,      0.875000,      &
              0.859375,      0.843750,      0.828125,      &
              0.812500,      0.796875,      0.781250,      &
              0.765625,      0.750000,      0.734375,      &
              0.718750,      0.703125,      0.687500,      &
              0.671875,      0.656250,      0.640625,      &
              0.625000,      0.609375,      0.593750,      &
              0.578125,      0.562500,      0.546875,      &
              0.531250,      0.515625,      0.500000,      &
              0.484375,      0.468750,      0.453125,      &
              0.437500,      0.421875,      0.406250,      &
              0.390625,      0.375000,      0.359375,      &
              0.343750,      0.328125,      0.312500,      &
              0.296875,      0.281250,      0.265625,      &
              0.250000,      0.234375,      0.218750,      &
              0.203125,      0.187500,      0.171875,      &
              0.156250,      0.140625,      0.125000,      &
              0.109375,      9.375000E-002, 7.812500E-002, &
              6.250000E-002, 4.687500E-002, 3.125000E-002, &
              1.562500E-002  / 
!
!--- if routine has already been executed, return
    if (module_is_initialized) return
!
!--- verify that the GFDL FMS  modules used by this module which
!--- are called later in this subroutine have already been initialized
    call fms_init
!
!--- set up various time/date related control variables
    call get_time(Time - Time_init, sec, days=days)
    fhour = real(days)*24.d0 + real(sec)/3600.d0
    kdt = fhour*3600.d0/dt_phys
    nnp = kdt
    jdate = 0 
    call get_date (Time, jdate(1), jdate(2), jdate(3),  &
                         jdate(5), jdate(6), jdate(7))
    call get_date (Time_init, idate(4), idate(2), idate(3), idate(1), id1, id2)
!
!--- set the local variable for "lat" & "lon" per tile
    lat_cs = glat
    lon_cs = glon
!--- set the GFS variable for "lat" & "lon" for the global system
    latr = 2*lat_cs
    lonr = 4*lon_cs
!
!--- initialize fdiags to zero
    fdiag = 0.
!
!--- read namelist  ---
#ifdef INTERNAL_FILE_NML
    read (input_nml_file, nml=gfs_physics_nml, iostat=io)
    ierr = check_nml_error(io,"gfs_physics_nml")
#else
    if ( file_exist('input.nml')) then
      unit = open_namelist_file ()
      ierr=1; do while (ierr /= 0)
      read  (unit, nml=gfs_physics_nml, iostat=io, end=10)
      ierr = check_nml_error(io, 'gfs_physics_nml')
      enddo
 10   call close_file (unit)
    endif
#endif
!
!--- check fdiag to see if it is an interval or a list
    if (nint(fdiag(2)) == 0) then
      dt_diag = fdiag(1)
      fdiag(1) = fhour
      do i = 2, size(fdiag,1)
        fdiag(i) = fdiag(i-1) + dt_diag
      enddo
    endif
!
!--- check to see if prognostic ozone calculation active
    if (ozcalc) then
      if (mpp_pe() == mpp_root_pe() ) write(6,*) 'OZONE is being calculated'
    else
      if (mpp_pe() == mpp_root_pe() ) write(6,*) 'OZONE is NOT being calculated'
    endif
!
!--- check to see if deep convection parameterization active
    if (nocnv) then
      if (mpp_pe() == mpp_root_pe() ) write(6,*) 'DEEP CONVECTION is NOT being parameterized'
    else
      if (mpp_pe() == mpp_root_pe() ) write(6,*) 'DEEP CONVECTION is being parameterized'
    endif
!
!--- write version number and namelist to log file ---
    call write_version_number ('vers 1', 'gfs_physics_driver_mod')
    logunit = stdlog()
    if (mpp_pe() == mpp_root_pe() ) write(logunit, nml=gfs_physics_nml)
!
!--- set some configurational switches/parameters that are derived from inputs
    nsswr = nint(fhswr/dt_phys)
    nslwr = nint(fhlwr/dt_phys)
    nscyc = nint(fhcyc*3600./dt_phys)
    sas_shal = (sashal .and. (.not. ras))
!
!--- read in ozone datasets for prognostic ozone interpolation
!--- sets values for levozp, pl_coeff, pl_pres(=>Tbd_data%poz), ozplin
    call read_o3data (levozp, pl_coeff)
!
!--- get atmospheric tracer information
    call get_number_tracers (MODEL_ATMOS, num_tracers=ntrac, num_prog=ntp)
!
!--- find an open unit number for use by the nuopc physics initialization
    unit = open_namelist_file ()
!
!--- define MPI-rank number
    me = mpp_pe()
!
!--- grid related values have been defined consistent with original NCEP/EMC definitions
    dxmin = log(1.0_kind_phys/(min_lon*min_lat))
    dxmax = log(1.0_kind_phys/(max_lon*max_lat))
    dxinv = 1.0_kind_phys/(dxmax-dxmin)
!
!--- initialize the physics/radiation using the nuopc interface
    call nuopc_phys_init (Mdl_parms, ntcw, ncld, ntoz, ntrac, npz, me, lsoil, lsm, nmtvr, nrcm, levozp,  &
                          lonr, latr, jcap, num_p3d, num_p2d, npdf3d, pl_coeff, ncw, crtrh, cdmbgwd,  &
                          ccwf, dlqf, ctei_rm, cgwf, prslrd0, ras, pre_rad, ldiag3d, lgocart,  &
                          lssav_cpl, flipv, old_monin, cnvgwd, shal_cnv, sashal, newsas, cal_pre, mom4ice,  &
                          mstrat, trans_trac, nst_fcst, moist_adj, thermodyn_id, sfcpress_id,  &
                          gen_coord_hybrid, npz, lsidea, pdfcld, shcnvcw, redrag, hybedmf, dspheat, &
                          dxmax, dxmin, dxinv, ozcalc,nocnv,&
                          ! NEW from nems_slg_shoc
                          cscnv, nctp, ntke, do_shoc, shocaftcnv, ntot3d, ntot2d,   &
                          ! For radiation
                          si, ictm, isol, ico2, iaer, ialb, iems,                    &
                          iovr_sw,iovr_lw,isubc_sw,isubc_lw,shoc_cld,   &
                          sas_shal,crick_proof,ccnorm,norad_precip,idate,iflip,dt_phys,unit)
!
!--- return file unit to the general pool
    call close_file (unit)
!
!--- allocate the block storage containers needed by GFS physics/radiation
    allocate (  Tbd_data(Atm_block%nblks) )
    allocate ( Dyn_parms(Atm_block%nblks) )
    allocate ( Gfs_diags(Atm_block%nblks) )
    allocate ( Sfc_props(Atm_block%nblks) )
    allocate ( Cld_props(Atm_block%nblks) )
    allocate ( Rad_tends(Atm_block%nblks) )
    allocate ( Intr_flds(Atm_block%nblks) )
    if (ozcalc) allocate ( O3dat(Atm_block%nblks) )
!
!--- allocate/initialize the various nuopc physics containers per block (including ozone container)
    do nb = 1, Atm_block%nblks
      ibs = Atm_block%ibs(nb)
      ibe = Atm_block%ibe(nb)
      jbs = Atm_block%jbs(nb)
      jbe = Atm_block%jbe(nb)
      ngptc = (ibe - ibs + 1) * (jbe - jbs + 1) 
!
!--- allocate elements of O3dat for prognostic ozone
      if (ozcalc) Then
        allocate ( O3dat(nb)%j1  (ngptc) )
        allocate ( O3dat(nb)%j2  (ngptc) )
        allocate ( O3dat(nb)%ddy (ngptc) )
        allocate ( O3dat(nb)%gaul(ngptc) )
      endif

      call Tbd_data(nb)%set      (ngptc, Mdl_parms, xkzm_m, xkzm_h, xkzm_s, &
                                  evpco, psautco, prautco, wminco, pl_pres)
      call Dyn_parms(nb)%setrad  (ngptc, ngptc, kdt, jdate, solhr, fhlwr, fhswr, &
                                  lssav, ipt, lprnt, dt_phys)
      call Dyn_parms(nb)%setphys (ngptc, ngptc, solhr, kdt, lssav, latgfs, &
                                  dt_phys, dt_phys, clstp, nnp, fhour, ncols)
      call Gfs_diags(nb)%setrad  (ngptc, NFXR)
      call Gfs_diags(nb)%setphys (ngptc, Mdl_parms)
      call Sfc_props(nb)%setrad  (ngptc, Mdl_parms, .FALSE.)  ! last argument determines gsm vs atmos-only
      call Sfc_props(nb)%setphys (ngptc, Mdl_parms)
      call Cld_props(nb)%setrad  (ngptc, Mdl_parms, sup)
      call Cld_props(nb)%setphys (ngptc, Mdl_parms, sup, flgmin)
      call Rad_tends(nb)%set     (ngptc, Mdl_parms)
      call Intr_flds(nb)%setrad  (ngptc, Mdl_parms, SW0, SWB, LW0, LWB)
      call Intr_flds(nb)%setphys (ngptc, Mdl_parms)
      call State_out(nb)%setphys (ngptc, Mdl_parms)
      ! setphys must be called prior to setrad
      call State_in(nb)%setphys  (ngptc, Mdl_parms)
      call State_in(nb)%setrad   (ngptc, Mdl_parms)

      !  populate static values that are grid dependent
      do j=jbs,jbe
       do i=ibs,ibe
        ix = Atm_block%ix(nb)%ix(i,j)
!--- define various grid parameters
        Dyn_parms(nb)%area(ix) = area(i,j)
        Dyn_parms(nb)%dx(ix)   = sqrt(area(i,j))
        Dyn_parms(nb)%dy(ix)   = Dyn_parms(nb)%dx(ix)
        Dyn_parms(nb)%xlat(ix) = lat(i,j)
        Dyn_parms(nb)%xlon(ix) = lon(i,j)
        Dyn_parms(nb)%sinlat(ix) = sin(lat(i,j))
        Dyn_parms(nb)%coslat(ix) = sqrt(1.0_kind_phys - Dyn_parms(nb)%sinlat(ix)*Dyn_parms(nb)%sinlat(ix))
!--- needed for setindxoz
        if (ozcalc) O3dat(nb)%gaul(ix) = lat(i,j)*180.0_kind_phys/pi
       enddo
      enddo
    enddo
!
!--- set up interpolation indices and weights for prognostic ozone interpolation
    if (ozcalc) then
      do nb = 1, Atm_block%nblks
        call setindxoz (ngptc, ngptc, O3dat(nb)%gaul, O3dat(nb)%j1, O3dat(nb)%j2, O3dat(nb)%ddy)
      enddo
    endif
!
!--- read in surface data from chgres 
    call surface_props_input (Atm_block, fv_domain)
!
!--- initialize register diagnostics with GFDL FMS diagnostic manager
    call gfs_diag_register(Time, Atm_block, axes, NFXR)
!
!--- mark the module as initialized ---
      module_is_initialized = .true.
!
!--- output various GFS physics parameters
      if (me==0) then
         print *, "in DRIVER AFTER setup"
         print *, "ntcw             : ", Mdl_parms%ntcw
         print *, "ncld             : ", Mdl_parms%ncld
         print *, "ntoz             : ", Mdl_parms%ntoz
         print *, "NTRAC            : ", Mdl_parms%NTRAC
         print *, "levs             : ", Mdl_parms%levs
         print *, "me               : ", Mdl_parms%me
         print *, "lsoil            : ", Mdl_parms%lsoil
         print *, "lsm              : ", Mdl_parms%lsm
         print *, "nmtvr            : ", Mdl_parms%nmtvr
         print *, "nrcm             : ", Mdl_parms%nrcm
         print *, "levozp           : ", Mdl_parms%levozp
         print *, "lonr             : ", Mdl_parms%lonr
         print *, "latr             : ", Mdl_parms%latr
         print *, "jcap             : ", Mdl_parms%jcap
         print *, "num_p3d          : ", Mdl_parms%num_p3d
         print *, "num_p2d          : ", Mdl_parms%num_p2d
         print *, "npdf3d           : ", Mdl_parms%npdf3d
         print *, "pl_coeff         : ", Mdl_parms%pl_coeff
         print *, "ncw              : ", Mdl_parms%ncw
         print *, "crtrh            : ", Mdl_parms%crtrh
         print *, "cdmbgwd          : ", Mdl_parms%cdmbgwd
         print *, "ccwf             : ", Mdl_parms%ccwf
         print *, "dlqf             : ", Mdl_parms%dlqf
         print *, "ctei_rm          : ", Mdl_parms%ctei_rm
         print *, "cgwf             : ", Mdl_parms%cgwf
         print *, "prslrd0          : ", Mdl_parms%prslrd0
         print *, "ras              : ", Mdl_parms%ras
         print *, "pre_rad          : ", Mdl_parms%pre_rad
         print *, "ldiag3d          : ", Mdl_parms%ldiag3d
         print *, "lgocart          : ", Mdl_parms%lgocart
         print *, "lssav_cpl        : ", Mdl_parms%lssav_cpl
         print *, "flipv            : ", Mdl_parms%flipv
         print *, "old_monin        : ", Mdl_parms%old_monin
         print *, "cnvgwd           : ", Mdl_parms%cnvgwd
         print *, "shal_cnv         : ", Mdl_parms%shal_cnv
         print *, "sashal           : ", Mdl_parms%sashal
         print *, "newsas           : ", Mdl_parms%newsas
         print *, "cal_pre          : ", Mdl_parms%cal_pre
         print *, "mom4ice          : ", Mdl_parms%mom4ice
         print *, "mstrat           : ", Mdl_parms%mstrat
         print *, "trans_trac       : ", Mdl_parms%trans_trac
         print *, "nst_fcst         : ", Mdl_parms%nst_fcst
         print *, "moist_adj        : ", Mdl_parms%moist_adj
         print *, "thermodyn_id     : ", Mdl_parms%thermodyn_id
         print *, "sfcpress_id      : ", Mdl_parms%sfcpress_id
         print *, "gen_coord_hybrid : ", Mdl_parms%gen_coord_hybrid
         print *, "levr             : ", Mdl_parms%levr
         print *, "lsidea           : ", Mdl_parms%lsidea
         print *, "pdfcld           : ", Mdl_parms%pdfcld
         print *, "shcnvcw          : ", Mdl_parms%shcnvcw
         print *, "redrag           : ", Mdl_parms%redrag
         print *, "hybedmf          : ", Mdl_parms%hybedmf
         print *, "dspheat          : ", Mdl_parms%dspheat
         print *, "dxmax            : ", dxmax
         print *, "dxmin            : ", dxmin
         print *, "dxinv            : ", dxinv
         print *, "si               : ", si
         print *, "ictm             : ", ictm
         print *, "isol             : ", isol
         print *, "ico2             : ", ico2
         print *, "iaer             : ", iaer
         print *, "ialb             : ", ialb
         print *, "iems             : ", iems
         print *, "iovr_sw          : ", iovr_sw
         print *, "iovr_lw          : ", iovr_lw
         print *, "isubc_sw         : ", isubc_sw
         print *, "isubc_lw         : ", isubc_lw
         print *, "sas_shal         : ", sas_shal
         print *, "crick_proof      : ", crick_proof
         print *, "ccnorm           : ", ccnorm
         print *, "norad_precip     : ", norad_precip
         print *, "idate            : ", Mdl_parms%idate
         print *, "iflip            : ", iflip
         print *, "cscnv            : ", Mdl_parms%cscnv
         print *, "nctp             : ", Mdl_parms%nctp
         print *, "ntke             : ", Mdl_parms%ntke
         print *, "do_shoc          : ", Mdl_parms%do_shoc
         print *, "shocaftcnv       : ", Mdl_parms%shocaftcnv
         print *, "ntot3d           : ", Mdl_parms%ntot3d
         print *, "ntot2d           : ", Mdl_parms%ntot2d
         print *, "shoc_cld         : ", Mdl_parms%shoc_cld
         print *, "nocnv            : ", nocnv
         print *, "ozcalc           : ", ozcalc
       end if ! parameter output

  end subroutine phys_rad_driver_init
!-------------------------------------------------------------------------      


!-------------------------------------------------------------------------      
!--- phys_rad_setup_step ---
!-------------------------------------------------------------------------      
!    routine called prior to radiation and physics steps to handle the
!    the gloopr/gloopb logic not encapsulated in the GFS NUOPC interface
!      1) sets up various time/date variables
!      2) sets up various triggers
!      3) defines random seed indices for radiation (in a reproducible way)
!      4) populate Dyn_parms container with random seed indices
!      5) interpolates coefficients for prognostic ozone calculation
!      6) performs surface data cycling via the GFS gcycle routine
!-------------------------------------------------------------------------      
  subroutine phys_rad_setup_step (Time_init, Time_prev, Time, Atm_block)
!--- subroutine interface variable definitions
    type(time_type),            intent(in) :: Time_init, Time_prev, Time
    type (block_control_type),  intent(in) :: Atm_block
!--- local variables
    integer,              parameter :: ipsdlim = 1.0e8      ! upper limit for random seeds
    real(kind=kind_phys), parameter :: cons_24 = 24.0_kind_phys
    integer :: i, j, k, nb, ix
    integer :: ibs, ibe, jbs, jbe, nx, ny, ngptc
    integer :: sec, ipseed, fms_date(8)
    integer :: numrdm(lon_cs*lat_cs*2)
    logical :: lsswr, lslwr
    type (random_stat) :: stat
    real(kind=kind_phys) :: phour, fhour
    real(kind=kind_phys) :: work1, work2
!
!--- update various date/time variables
    call get_date (Time_prev, fms_date(1), fms_date(2), fms_date(3),  &
                         fms_date(5), fms_date(6), fms_date(7))

    call get_time(Time_prev - Time_init, sec)
    phour = real(sec)/3600.
    call get_time(Time - Time_init, sec)
    fhour = real(sec)/3600.
    lsswr = (mod(Dyn_parms(1)%kdt+1, nsswr) == 1)
    lslwr = (mod(Dyn_parms(1)%kdt+1, nslwr) == 1)

    if (debug .and. mpp_pe() == mpp_root_pe()) then
      print *,'   kdt ', Dyn_parms(1)%kdt + 1
      print *,' nsswr ', nsswr
      print *,' nslwr ', nslwr
      print *,' nscyc ', nscyc
      print *,' lsswr ', lsswr
      print *,' lslwr ', lslwr
      print *,' fhour ', fhour
      print *,' phour ', phour
      print *,' solhr ', mod(phour+Mdl_parms%idate(1),cons_24)
    endif
!
!--- set up random seed index for the whole tile in a reproducible way
    if (lsswr .or. lslwr) then
      if (isubc_lw==2 .or. isubc_sw==2) then
        ipseed = mod(nint(100.0*sqrt(real(sec))), ipsdlim) + 1 + ipsd0
        call random_setseed (ipseed, stat)
        call random_index (ipsdlim, numrdm, stat)
      endif
    endif

!$OMP PARALLEL DO default(none) &
!$OMP              shared(Atm_block,Dyn_parms,fhour,fms_date,phour,Mdl_parms,nsswr,  &
!$OMP                     nslwr,isubc_lw,isubc_sw,numrdm,lon_cs,lat_cs,dxmin,dxinv,  &
!$OMP                     Cld_props,flgmin,ozcalc,O3dat,ozplin,Tbd_data,lsswr,lslwr) &
!$OMP             private(nb,ibs,ibe,jbs,jbe,nx,ny,ngptc,ix,j,i,work1)
    do nb = 1, Atm_block%nblks
      ibs = Atm_block%ibs(nb)
      ibe = Atm_block%ibe(nb)
      jbs = Atm_block%jbs(nb)
      jbe = Atm_block%jbe(nb)
      nx = ibe-ibs+1
      ny = jbe-jbs+1
      ngptc = nx*ny

!--- increment the time step number
      Dyn_parms(nb)%kdt      = Dyn_parms(nb)%kdt + 1
      Dyn_parms(nb)%nnp      = Dyn_parms(nb)%nnp + 1
!--- set the current forecast hour
      Dyn_parms(nb)%fhour    = fhour
      Dyn_parms(nb)%jdate(1) = fms_date(1)
      Dyn_parms(nb)%jdate(2) = fms_date(2)
      Dyn_parms(nb)%jdate(3) = fms_date(3)
      Dyn_parms(nb)%jdate(5) = fms_date(5)
      Dyn_parms(nb)%jdate(6) = fms_date(6)
      Dyn_parms(nb)%jdate(7) = fms_date(7)
!--- set the solar hour based on a combination of phour and time initial hour
      Dyn_parms(nb)%solhr = mod(phour+Mdl_parms%idate(1),cons_24)
!--- radiation triggers
      Dyn_parms(nb)%lsswr = lsswr
      Dyn_parms(nb)%lslwr = lslwr
! **************  Ken Campana Stuff  ********************************
!...  set switch for saving convective clouds
      if(Dyn_parms(nb)%lsswr) then
        Dyn_parms(nb)%clstp = 1100                           !initialize,accumulate
      else
        Dyn_parms(nb)%clstp = 0100                           !accumulate
      endif
! **************  Ken Campana Stuff  ********************************
!
!--- set the random seeds for each column
      if (lsswr .or. lslwr) then
        ix = 0 
        do j = 1,ny
          do i = 1,nx
            ix = ix + 1
            if (isubc_lw==2 .or. isubc_sw==2) then
              !for testing purposes, replace numrdm with '100'
              Dyn_parms(nb)%icsdsw(ix) = numrdm(i+ibs-1 + (j+jbs-2)*lon_cs)
              Dyn_parms(nb)%icsdlw(ix) = numrdm(i+ibs-1 + (j+jbs-2)*lon_cs + lat_cs*lon_cs)
            endif

            if (Mdl_parms%num_p3d == 3) then
              work1 = (log(Dyn_parms(nb)%coslat(ix)/Dyn_parms(nb)%nlons(ix)) - dxmin) * dxinv
              work1 = max(0.0, min(1.0,work1))
              Cld_props(nb)%flgmin(ix) = flgmin(1)*work1 + flgmin(2)*(1.0-work1)
            else
              Cld_props(nb)%flgmin(ix) = 0.0
            endif
          enddo
        enddo
      endif
!
!--- interpolate coefficients for prognostic ozone calculation
      if (ozcalc) then
        call ozinterpol(Mdl_parms%me, ngptc, ngptc, Mdl_parms%idate, fhour, &
                        O3dat(nb)%j1, O3dat(nb)%j2, ozplin, Tbd_data(nb)%prdout, &
                        O3dat(nb)%ddy)
      endif
    enddo
!
!--- repopulate specific time-varying sfc properties for AMIP/forecast runs
    if (nscyc >  0) then
      if (mod(Dyn_parms(1)%kdt,nscyc) == 1) THEN
        ngptc = (Atm_block%iec-Atm_block%isc+1)*(Atm_block%jec-Atm_block%jsc+1)
        call gcycle(Mdl_parms%me, ngptc, Atm_block%nblks, Mdl_parms%lsoil, &
                    Mdl_parms%idate, phour, fhcyc, Dyn_parms, Sfc_props,   &
                    Cld_props, Tbd_data, ialb, use_ufo, nst_anl)
      endif
    endif
!
!--- debug statements
    if (debug) then
      if (mpp_pe() == mpp_root_pe()) then
        write(6,100) 'timestep ',Dyn_parms(1)%kdt,  ', fhour ',fhour, &
                     '  lsswr ',Dyn_parms(1)%lsswr,' lslwr ',Dyn_parms(1)%lslwr
      endif
      if (Mdl_parms%me == 0 ) write(6,*) '  after phys_rad_setup_step '
      call checksum (Atm_block)
    endif
 100 format (a,i5.5,a,f10.4,a,L1,a,L1)

  end subroutine phys_rad_setup_step
!-------------------------------------------------------------------------      


!-------------------------------------------------------------------------      
!--- skin temp ---
!-------------------------------------------------------------------------      
!    debug routine to output various skin temperature properties
!-------------------------------------------------------------------------      
  subroutine skin_temp(tmin, tmax, timax, timin, nblks)
!--- subroutine interface variable definitions
    real(kind=kind_phys), intent(inout) :: tmin, tmax, timin, timax
    integer, intent(in) :: nblks
!--- local variables
    integer :: nb
    real(kind=kind_phys) :: tminl, tmaxl, timinl, timaxl

    tmax = -99999.0
    tmin = +99999.0
    timax = -99999.0
    timin = +99999.0
    do nb = 1, nblks
      tmaxl = maxval(Sfc_props(nb)%tsfc)
      tminl = minval(Sfc_props(nb)%tsfc)
      tmax = max (tmax, tmaxl)
      tmin = min (tmin, tminl)
      timaxl = maxval(Sfc_props(nb)%tisfc)
      timinl = minval(Sfc_props(nb)%tisfc)
      timax = max (timax, timaxl)
      timin = min (timin, timinl)
    enddo
    call mpp_max(tmax)
    call mpp_min(tmin)
    call mpp_max(timax)
    call mpp_min(timin)
  end subroutine skin_temp


!-------------------------------------------------------------------------      
!--- radiation_driver ---
!-------------------------------------------------------------------------      
!    calls nuopc_rad_update
!    calls nuopc_rad_run
!-------------------------------------------------------------------------      
  subroutine radiation_driver (Atm_block, Statein)
!--- subroutine interface variable definitions
    type (block_control_type),               intent(in) :: Atm_block
    type(state_fields_in),     dimension(:), intent(in) :: Statein
!--- local variables
    integer :: nb

!
!--- test to determine if a radiation step
    if (Dyn_parms(1)%lsswr .or. Dyn_parms(1)%lslwr) then
!
!--- call the nuopc radiation routine for time-varying data ---
      do nb = 1, Atm_block%nblks
        if ((Mdl_parms%me == 0) .and. (nb /= 1)) then
          Mdl_parms%me = -99
        endif
        call nuopc_rad_update (Mdl_parms, Dyn_parms(nb))
      enddo
      if (mpp_pe() == mpp_root_pe()) Mdl_parms%me = mpp_pe()
!
!--- call the nuopc radiation loop---
!$OMP parallel do default (none) &
!$OMP          schedule (dynamic,1) & 
!$OMP          shared  (Atm_block, Dyn_parms, Statein, Sfc_props,   &
!$OMP                   Gfs_diags, Intr_flds, Cld_props, Rad_tends) &
!$OMP          firstprivate (Mdl_parms)  &
!$OMP          private (nb)
      do nb = 1, Atm_block%nblks
        if ((Mdl_parms%me == 0) .and. (nb /= 1)) then
          Mdl_parms%me = -99
        endif
        call nuopc_rad_run (Statein(nb), Sfc_props(nb), Gfs_diags(nb), &
                            Intr_flds(nb), Cld_props(nb), Rad_tends(nb), &
                            Mdl_parms, Dyn_parms(nb))
      enddo
    endif
    if (mpp_pe() == mpp_root_pe()) Mdl_parms%me = mpp_pe()
!
!--- debut statements
    if (debug) then
      if (Mdl_parms%me == 0 ) write(6,*) '  after radiation '
      call checksum (Atm_block, Statein)
    endif
  end subroutine radiation_driver
!-------------------------------------------------------------------------      
 


!-------------------------------------------------------------------------      
!--- physics_driver ---
!-------------------------------------------------------------------------      
!    calls nuopc_phys_run
!-------------------------------------------------------------------------      
  subroutine physics_driver (Time_diag, Time_init, Atm_block, Statein, Stateout)
!--- subroutine interface variable definitions
    type(time_type),                         intent(in)    :: Time_diag, Time_init
    type (block_control_type),               intent(in)    :: Atm_block
    type(state_fields_in),     dimension(:), intent(in)    :: Statein
    type(state_fields_out),    dimension(:), intent(inout) :: Stateout
!--- local variables
    integer :: nb, nx, ny
    real(kind=kind_phys) :: fhour
    integer :: yr, mo, dy, hr, min, sc

    fhour = Dyn_parms(1)%fhour
!
!--- call the nuopc physics loop---
!$OMP parallel do default (none) &
!$OMP          schedule (dynamic,1) & 
!$OMP          shared  (Atm_block, Dyn_parms, Statein, Sfc_props, &
!$OMP                   Gfs_diags, Intr_flds, Cld_props, Rad_tends, Tbd_data, &
!$OMP                   Stateout, fdiag, fhzero, levs) &
!$OMP          firstprivate (Mdl_parms, fhour, Time_diag)  &
!$OMP          private (nb, nx, ny)
    do nb = 1, Atm_block%nblks

      if ((Mdl_parms%me == 0) .and. (nb /= 1)) then
        Mdl_parms%me = -99
      endif

      Tbd_data(nb)%dpshc(:) = 0.3d0 * Statein(nb)%prsi(:,1)

      call nuopc_phys_run (Statein(nb), Stateout(nb), Sfc_props(nb), &
                           Gfs_diags(nb), Intr_flds(nb), Cld_props(nb), &
                           Rad_tends(nb), Mdl_parms, Tbd_data(nb), &
                           Dyn_parms(nb))

      if (mpp_pe() == mpp_root_pe()) Mdl_parms%me = mpp_pe()
!
!--- check the diagnostics output trigger
      if (ANY(fdiag == fhour)) then
        if (mpp_pe() == mpp_root_pe().and.nb==1) write(6,*) 'DIAG STEP', fhour
        nx = Atm_block%ibe(nb) - Atm_block%ibs(nb) + 1
        ny = Atm_block%jbe(nb) - Atm_block%jbs(nb) + 1
        call gfs_diag_output (Time_diag, Gfs_diags(nb), Statein(nb), Stateout(nb), &
                              Atm_block, nb, nx, ny, levs,                         &
                              Mdl_parms%ntcw, Mdl_parms%ntoz, Dyn_parms(nb)%dtp)
      endif
    enddo
!
!----- Indicate to diag_manager to write diagnostics to file (if needed)
!----- This is needed for a threaded run.
    if (ANY(fdiag == fhour)) then
      call diag_send_complete_extra(Time_diag)
    endif
!
!--- reset the time averaged quantities to zero (actually all quantities)
    if (mod(fhour,fhzero) == 0) then
!$OMP parallel do default (none) &
!$             shared  (Gfs_diags, Atm_block) &
!$             private (nb)
      do nb = 1, Atm_block%nblks
        call Gfs_diags(nb)%setrad ()
        call Gfs_diags(nb)%setphys ()
      enddo
    endif
!
!--- debug statements
    if (debug) then
      if (Mdl_parms%me == 0 ) write(6,*) '  after physics '
      call checksum (Atm_block, Statein)
    endif
  end subroutine physics_driver
!-------------------------------------------------------------------------      


!-------------------------------------------------------------------------      
!--- phys_rad_driver_restart---
!-------------------------------------------------------------------------      
!    calls surface_props_output with a timestamp for intermediate restarts
!-------------------------------------------------------------------------      
  subroutine phys_rad_driver_restart (Atm_block, fv_domain, timestamp)
!--- subroutine interface variable definitions
    type (block_control_type),   intent(in) :: Atm_block
    type (domain2d),             intent(in) :: fv_domain
    character(len=32), optional, intent(in) :: timestamp

    call surface_props_output (Atm_block, fv_domain, timestamp)

  end subroutine phys_rad_driver_restart
!-------------------------------------------------------------------------      


!-------------------------------------------------------------------------      
!--- phys_rad_driver_end---
!-------------------------------------------------------------------------      
!    calls surface_props_output w/o a timestamp for end-of-run restarts
!-------------------------------------------------------------------------      
  subroutine phys_rad_driver_end (Atm_block, fv_domain)
!--- subroutine interface variable definitions
    type (block_control_type),   intent(in) :: Atm_block
    type (domain2d),             intent(in) :: fv_domain

    call surface_props_output (Atm_block, fv_domain)
  end subroutine phys_rad_driver_end
!-------------------------------------------------------------------------      



!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!                     PRIVATE SUBROUTINES
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!-------------------------------------------------------------------------      
!--- surface_props_input ---
!-------------------------------------------------------------------------      
!    creates and populates a data type which is then used to "register"
!    restart variables with the GFDL FMS restart subsystem.
!    calls a GFDL FMS routine to restore the data from a restart file.
!    calculates sncovr if it is not present in the restart file.
!
!    calls:  register_restart_field, restart_state, free_restart
!   
!    opens:  oro_data.tile?.nc, sfc_data.tile?.nc
!   
!-------------------------------------------------------------------------      
  subroutine surface_props_input (Atm_block, fv_domain, GSM)
!--- subroutine interface variable definitions
    type (block_control_type), intent(in) :: Atm_block
    type (domain2d),           intent(in) :: fv_domain
    logical, intent(in), optional :: GSM
!--- local variables
    integer :: i, j, k, ii, jj, ibs, ibe, jbs, jbe, nb, ix, lsoil, num
    integer :: isc, iec, jsc, jec, nx, ny, npz, ngptc
    integer :: id_restart
    integer :: nvar_o2, nvar_s2, nvar_s3
    character(len=32)  :: fn_oro = 'oro_data.nc'
    character(len=32)  :: fn_srf = 'sfc_data.nc'
    character(len=2)   :: c2 = ''
    real(kind=kind_phys), pointer, dimension(:,:)   :: var2_p => NULL()
    real(kind=kind_phys), pointer, dimension(:,:,:) :: var3_p => NULL()
    logical :: exists
    real :: tsmin, tsmax, timin, timax
!--- local variables for sncovr calculation
    real(kind=kind_phys) :: vegtyp, rsnow
    
    nvar_o2 = 17
    nvar_s2 = 32
    nvar_s3 = 3

    isc = Atm_block%isc
    iec = Atm_block%iec
    jsc = Atm_block%jsc
    jec = Atm_block%jec
    npz = Atm_block%npz
    nx = (iec - isc + 1)
    ny = (jec - jsc + 1)
!
!--- OROGRAPHY FILE
    if (.not. allocated(oro_name2)) then
!--- allocate the various containers needed for orography data
      allocate(oro_name2(nvar_o2))
      allocate(oro_var2(nx,ny,nvar_o2))
      oro_var2 = -9999._kind_phys

      oro_name2(1)  = 'stddev'     ! hprim
      oro_name2(2)  = 'stddev'     ! hprime2(ix,1)
      oro_name2(3)  = 'convexity'  ! hprime2(ix,2)
      oro_name2(4)  = 'oa1'        ! hprime2(ix,3)
      oro_name2(5)  = 'oa2'        ! hprime2(ix,4)
      oro_name2(6)  = 'oa3'        ! hprime2(ix,5)
      oro_name2(7)  = 'oa4'        ! hprime2(ix,6)
      oro_name2(8)  = 'ol1'        ! hprime2(ix,7)
      oro_name2(9)  = 'ol2'        ! hprime2(ix,8)
      oro_name2(10) = 'ol3'        ! hprime2(ix,9)
      oro_name2(11) = 'ol4'        ! hprime2(ix,10)
      oro_name2(12) = 'theta'      ! hprime2(ix,11)
      oro_name2(13) = 'gamma'      ! hprime2(ix,12)
      oro_name2(14) = 'sigma'      ! hprime2(ix,13)
      oro_name2(15) = 'elvmax'     ! hprime2(ix,14)
      oro_name2(16) = 'orog_filt'  ! oro
      oro_name2(17) = 'orog_raw'   ! oro_uf
!
!--- register the 2D fields
      do num = 1,nvar_o2
        var2_p => oro_var2(:,:,num)
        id_restart = register_restart_field(Oro_restart, fn_oro, oro_name2(num), var2_p, domain=fv_domain)
      enddo
      nullify(var2_p)
    endif
!
!--- read the orography restart/data
    call restore_state(Oro_restart)
!
!--- copy data into GFS NUOPC containers
    do nb = 1, Atm_block%nblks
      ibs = Atm_block%ibs(nb)
      ibe = Atm_block%ibe(nb)
      jbs = Atm_block%jbs(nb)
      jbe = Atm_block%jbe(nb)
!--- 2D variables
      do jj=jbs,jbe
        j = jj - jsc + 1
        do ii=ibs,ibe
          i = ii - isc + 1
          ix = Atm_block%ix(nb)%ix(ii,jj)
          !--- stddev
          Sfc_props(nb)%hprim(ix)      = oro_var2(i,j,1)
          !--- hprime2(1:14)
          Sfc_props(nb)%hprime2(ix,1)  = oro_var2(i,j,2)
          Sfc_props(nb)%hprime2(ix,2)  = oro_var2(i,j,3)
          Sfc_props(nb)%hprime2(ix,3)  = oro_var2(i,j,4)
          Sfc_props(nb)%hprime2(ix,4)  = oro_var2(i,j,5)
          Sfc_props(nb)%hprime2(ix,5)  = oro_var2(i,j,6)
          Sfc_props(nb)%hprime2(ix,6)  = oro_var2(i,j,7)
          Sfc_props(nb)%hprime2(ix,7)  = oro_var2(i,j,8)
          Sfc_props(nb)%hprime2(ix,8)  = oro_var2(i,j,9)
          Sfc_props(nb)%hprime2(ix,9)  = oro_var2(i,j,10)
          Sfc_props(nb)%hprime2(ix,10) = oro_var2(i,j,11)
          Sfc_props(nb)%hprime2(ix,11) = oro_var2(i,j,12)
          Sfc_props(nb)%hprime2(ix,12) = oro_var2(i,j,13)
          Sfc_props(nb)%hprime2(ix,13) = oro_var2(i,j,14)
          Sfc_props(nb)%hprime2(ix,14) = oro_var2(i,j,15)
          !--- oro
          Sfc_props(nb)%oro(ix)        = oro_var2(i,j,16)
          !--- oro_uf
          Sfc_props(nb)%oro_uf(ix)     = oro_var2(i,j,17)
        enddo
      enddo
    enddo
!
!--- deallocate containers and free restart container
    deallocate(oro_name2, oro_var2)
    call free_restart_type(Oro_restart)
!
!--- SURFACE FILE
    if (.not. allocated(sfc_name2)) then
!--- allocate the various containers needed for restarts
      allocate(sfc_name2(nvar_s2))
      allocate(sfc_name3(nvar_s3))
      allocate(sfc_var2(nx,ny,nvar_s2))
      allocate(sfc_var3(nx,ny,Mdl_parms%lsoil,nvar_s3))
      sfc_var2 = -9999._kind_phys
      sfc_var3 = -9999._kind_phys
!
!--- names of the 2D variables to save
      sfc_name2(1)  = 'slmsk'
      sfc_name2(2)  = 'tsea'    !tsfc
      sfc_name2(3)  = 'sheleg'  !weasd
      sfc_name2(4)  = 'tg3'
      sfc_name2(5)  = 'zorl'
      sfc_name2(6)  = 'alvsf'
      sfc_name2(7)  = 'alvwf'
      sfc_name2(8)  = 'alnsf'
      sfc_name2(9)  = 'alnwf'
      sfc_name2(10) = 'facsf'
      sfc_name2(11) = 'facwf'
      sfc_name2(12) = 'vfrac'
      sfc_name2(13) = 'canopy'
      sfc_name2(14) = 'f10m'
      sfc_name2(15) = 't2m'
      sfc_name2(16) = 'q2m'
      sfc_name2(17) = 'vtype'
      sfc_name2(18) = 'stype'
      sfc_name2(19) = 'uustar'
      sfc_name2(20) = 'ffmm'
      sfc_name2(21) = 'ffhh'
      sfc_name2(22) = 'hice'
      sfc_name2(23) = 'fice'
      sfc_name2(24) = 'tisfc'
      sfc_name2(25) = 'tprcp'
      sfc_name2(26) = 'srflag'
      sfc_name2(27) = 'snwdph'  !snowd
      sfc_name2(28) = 'shdmin'
      sfc_name2(29) = 'shdmax'
      sfc_name2(30) = 'slope'
      sfc_name2(31) = 'snoalb'
      sfc_name2(32) = 'sncovr'
!
!--- register the 2D fields
      do num = 1,nvar_s2
        var2_p => sfc_var2(:,:,num)
        if (trim(sfc_name2(num)) == 'sncovr') then
          id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name2(num), var2_p, domain=fv_domain, mandatory=.false.)
        else
          id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name2(num), var2_p, domain=fv_domain)
        endif
      enddo
      nullify(var2_p)
!
!--- names of the 2D variables to save
      sfc_name3(1) = 'stc'
      sfc_name3(2) = 'smc'
      sfc_name3(3) = 'slc'
!
!--- register the 3D fields
      do num = 1,nvar_s3
        var3_p => sfc_var3(:,:,:,num)
        id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name3(num), var3_p, domain=fv_domain)
      enddo
      nullify(var3_p)
    endif
!
!--- register the phy_f*d variables
    if (.not. allocated(phy_f2d)) then
      allocate(phy_f2d(nx,ny,Mdl_parms%num_p2d))
      allocate(phy_f3d(nx,ny,npz,Mdl_parms%num_p3d+Mdl_parms%npdf3d))
      phy_f2d = 0.0_kind_phys
      phy_f3d = 0.0_kind_phys
      id_restart = register_restart_field(Sfc_restart, fn_srf, 'phy_f2d', phy_f2d, domain=fv_domain, mandatory=.false.)
      do num = 1,size(phy_f3d,4)
        write(c2,'(i2.2)') num
        var3_p => phy_f3d(:,:,:,num)
        id_restart = register_restart_field(Sfc_restart, fn_srf, 'phy_f3d_'//c2, var3_p, domain=fv_domain, mandatory=.false.)
      enddo
      nullify(var3_p)
    endif
!
!--- read the surface restart/data
    call restore_state(Sfc_restart)
!
!--- place the data into the block GFS containers
    do nb = 1, Atm_block%nblks
      ibs = Atm_block%ibs(nb)
      ibe = Atm_block%ibe(nb)
      jbs = Atm_block%jbs(nb)
      jbe = Atm_block%jbe(nb)
!--- 2D variables
      do jj=jbs,jbe
        j = jj - jsc + 1
        do ii=ibs,ibe
          i = ii - isc + 1
          ix = Atm_block%ix(nb)%ix(ii,jj)
          !--- slmsk
          Sfc_props(nb)%slmsk(ix)  = sfc_var2(i,j,1)
          !--- tsfc (tsea in sfc file)
          Sfc_props(nb)%tsfc(ix)   = sfc_var2(i,j,2)
          !--- weasd (sheleg in sfc file)
          Sfc_props(nb)%weasd(ix)  = sfc_var2(i,j,3)
          !--- tg3
          Sfc_props(nb)%tg3(ix)    = sfc_var2(i,j,4)
          !--- zorl
          Sfc_props(nb)%zorl(ix)   = sfc_var2(i,j,5)
          !--- alvsf
          Sfc_props(nb)%alvsf(ix)  = sfc_var2(i,j,6)
          !--- alvwf
          Sfc_props(nb)%alvwf(ix)  = sfc_var2(i,j,7)
          !--- alnsf
          Sfc_props(nb)%alnsf(ix)  = sfc_var2(i,j,8)
          !--- alnwf
          Sfc_props(nb)%alnwf(ix)  = sfc_var2(i,j,9)
          !--- facsf
          Sfc_props(nb)%facsf(ix)  = sfc_var2(i,j,10)
          !--- facwf
          Sfc_props(nb)%facwf(ix)  = sfc_var2(i,j,11)
          !--- vfrac
          Sfc_props(nb)%vfrac(ix)  = sfc_var2(i,j,12)
          !--- canopy
          Sfc_props(nb)%canopy(ix) = sfc_var2(i,j,13)
          !--- f10m
          Sfc_props(nb)%f10m(ix)   = sfc_var2(i,j,14)
          !--- t2m
          Sfc_props(nb)%t2m(ix)    = sfc_var2(i,j,15)
          !--- q2m
          Sfc_props(nb)%q2m(ix)    = sfc_var2(i,j,16)
          !--- vtype
          Sfc_props(nb)%vtype(ix)  = sfc_var2(i,j,17)
          !--- stype
          Sfc_props(nb)%stype(ix)  = sfc_var2(i,j,18)
          !--- uustar
          Sfc_props(nb)%uustar(ix) = sfc_var2(i,j,19)
          !--- ffmm
          Sfc_props(nb)%ffmm(ix)   = sfc_var2(i,j,20)
          !--- ffhh
          Sfc_props(nb)%ffhh(ix)   = sfc_var2(i,j,21)
          !--- hice
          Sfc_props(nb)%hice(ix)   = sfc_var2(i,j,22)
          !--- fice
          Sfc_props(nb)%fice(ix)   = sfc_var2(i,j,23)
          !--- tisfc
          Sfc_props(nb)%tisfc(ix)  = sfc_var2(i,j,24)
          !--- tprcp
          Tbd_data(nb)%tprcp(ix)   = sfc_var2(i,j,25)
          !--- srflag
          Tbd_data(nb)%srflag(ix)  = sfc_var2(i,j,26)
          !--- snowd (snwdph in the file)
          Sfc_props(nb)%snowd(ix)  = sfc_var2(i,j,27)
          !--- shdmin
          Sfc_props(nb)%shdmin(ix) = sfc_var2(i,j,28)
          !--- shdmax
          Sfc_props(nb)%shdmax(ix) = sfc_var2(i,j,29)
          !--- slope
          Sfc_props(nb)%slope(ix)  = sfc_var2(i,j,30)
          !--- snoalb
          Sfc_props(nb)%snoalb(ix) = sfc_var2(i,j,31)
          !--- sncovr
          Sfc_props(nb)%sncovr(ix) = sfc_var2(i,j,32)
        enddo
      enddo
!
!--- 3D variables
      do lsoil = 1,Mdl_parms%lsoil
        do jj=jbs,jbe
          j = jj - jsc + 1
          do ii=ibs,ibe
            i = ii - isc + 1
            ix = Atm_block%ix(nb)%ix(ii,jj)
            !--- stc
            Tbd_data(nb)%stc(ix,lsoil) = sfc_var3(i,j,lsoil,1)
            !--- smc
            Tbd_data(nb)%smc(ix,lsoil) = sfc_var3(i,j,lsoil,2)
            !--- slc
            Tbd_data(nb)%slc(ix,lsoil) = sfc_var3(i,j,lsoil,3)
          enddo
        enddo
      enddo
!
!--- phy_f*d variables
      do num = 1,size(phy_f2d,3)
        do jj=jbs,jbe
          j = jj - jsc + 1
          do ii=ibs,ibe
            i = ii - isc + 1
            ix = Atm_block%ix(nb)%ix(ii,jj)
            Tbd_data(nb)%phy_f2d(ix,num) = phy_f2d(i,j,num)
          enddo
        enddo
      enddo
      do num = 1,size(phy_f3d,4)
        do k=1,npz
          do jj=jbs,jbe
            j = jj - jsc + 1
            do ii=ibs,ibe
              i = ii - isc + 1
              ix = Atm_block%ix(nb)%ix(ii,jj)
              Tbd_data(nb)%phy_f3d(ix,k,num) = phy_f3d(i,j,k,num)
            enddo
          enddo
        enddo
      enddo
    enddo
!
!--- if sncovr does not exist in the restart, need to create it
    if (nint(sfc_var2(1,1,32)) == -9999) then
      if (Mdl_parms%me == 0 ) call mpp_error(NOTE, 'gfs_driver::surface_props_input - computing sncovr') 
!--- compute sncovr from existing variables
!--- code taken directly from read_fix.f
      do nb = 1, Atm_block%nblks
        ibs = Atm_block%ibs(nb)
        ibe = Atm_block%ibe(nb)
        jbs = Atm_block%jbs(nb)
        jbe = Atm_block%jbe(nb)
        nx = (ibe - ibs + 1)
        ny = (jbe - jbs + 1)
        ngptc = nx * ny
        do i=1,ngptc
          Sfc_props(nb)%sncovr(i) = 0.0
          if (Sfc_props(nb)%slmsk(i) > 0.001 .AND. abs(Sfc_props(nb)%vtype(i)) >= 0.5 ) then
            vegtyp = Sfc_props(nb)%vtype(i)
            rsnow  = 0.001*Sfc_props(nb)%weasd(i)/snupx(vegtyp)
            if (0.001*Sfc_props(nb)%weasd(i) < snupx(vegtyp)) then
              Sfc_props(nb)%sncovr(i) = 1.0 - ( exp(-salp_data*rsnow) - rsnow*exp(-salp_data))
            else
              Sfc_props(nb)%sncovr(i) = 1.0
            endif
          endif
        enddo
      enddo
    endif

  end subroutine surface_props_input
!-------------------------------------------------------------------------      


!-------------------------------------------------------------------------      
!--- gfs_diag_register ---
!-------------------------------------------------------------------------      
!    creates and populates a data type which is then used to "register"
!    GFS physics diagnostic variables with the GFDL FMS diagnostic manager.
!    includes short & long names, units, conversion factors, etc.
!    there is no copying of data, but instead a clever use of pointers.
!    calls a GFDL FMS routine to register diagnositcs and compare against
!    the diag_table to determine what variables are to be output.
!
!    calls:  register_diag_field
!-------------------------------------------------------------------------      
!    Current sizes
!    13+NFXR - radiation
!    76+pl_coeff - physics
!-------------------------------------------------------------------------      
  subroutine gfs_diag_register(Time, Atm_block, axes, NFXR)
!--- subroutine interface variable definitions
    type(time_type),           intent(in) :: Time
    type (block_control_type), intent(in) :: Atm_block
    integer, dimension(4),     intent(in) :: axes
    integer,                   intent(in) :: NFXR
!--- local variables
    integer :: idx, num, nb, nblks, nx, ny, ngptc, k
    character(len=2) :: xtra
    real(kind=kind_phys), parameter :: cn_one = 1._kind_phys
    real(kind=kind_phys), parameter :: cn_100 = 100._kind_phys
    real(kind=kind_phys), parameter :: cn_th  = 1000._kind_phys
    real(kind=kind_phys), parameter :: cn_hr  = 3600._kind_phys

    nblks = Atm_block%nblks

    Diag(:)%id = -99
    Diag(:)%axes = -99
    Diag(:)%cnvfac = 1.0_kind_phys

    do idx = 1,DIAG_SIZE
      allocate(Diag(idx)%data(nblks))
      do nb = 1,nblks
        Diag(idx)%data(nb)%is = Atm_block%ibs(nb)-Atm_block%isc+1
        Diag(idx)%data(nb)%js = Atm_block%jbs(nb)-Atm_block%jsc+1
      enddo
    enddo

    idx = 0 

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ALBDOsfc'
    Diag(idx)%desc = 'surface albedo (%)'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_100
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2 (1:nx,1:ny) => Gfs_diags(nb)%fluxr(1:ngptc,3)
      Diag(idx)%data(nb)%var21(1:nx,1:ny) => Gfs_diags(nb)%fluxr(1:ngptc,4)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'DLWRFsfc'
    Diag(idx)%desc = 'surface downward longwave flux [W/m**2]'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dlwsfc(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ULWRFsfc'
    Diag(idx)%desc = 'surface upward longwave flux [W/m**2]'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%ulwsfc(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'DSWRFsfc'
    Diag(idx)%desc = 'surface downward showrtwave flux [W/m**2]'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%fluxr(1:ngptc,4)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'USWRFsfc'
    Diag(idx)%desc = 'surface upward shortwave flux [W/m**2]'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%fluxr(1:ngptc,3)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'DSWRFtoa'
    Diag(idx)%desc = 'top of atmos downward shortwave flux [W/m**2]'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%fluxr(1:ngptc,23)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'USWRFtoa'
    Diag(idx)%desc = 'top of atmos upward shortwave flux [W/m**2]'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%fluxr(1:ngptc,2)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ULWRFtoa'
    Diag(idx)%desc = 'top of atmos upward longwave flux [W/m**2]'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%fluxr(1:ngptc,1)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TCDCclm'
    Diag(idx)%desc = 'atmos column total cloud cover [%]'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_100/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%fluxr(1:ngptc,17)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TCDChcl'
    Diag(idx)%desc = 'high cloud level total cloud cover [%]'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_100/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%fluxr(1:ngptc,5)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TCDClcl'
    Diag(idx)%desc = 'low cloud level total cloud cover [%]'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_100/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%fluxr(1:ngptc,7)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TCDCmcl'
    Diag(idx)%desc = 'mid cloud level total cloud cover [%]'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_100/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%fluxr(1:ngptc,6)
    enddo

!--- accumulated diagnostics ---
    do num = 1,NFXR
      write (xtra,'(I2.2)') num 
      idx = idx + 1
      Diag(idx)%axes = 2
      Diag(idx)%name = 'fluxr_'//trim(xtra)
      Diag(idx)%desc = 'fluxr diagnostic '//trim(xtra)//' - GFS radiation'
      Diag(idx)%unit = 'XXX'
      Diag(idx)%mod_name = 'gfs_phys'
      do nb = 1,nblks
        nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
        ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
        ngptc = nx*ny
        Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%fluxr(1:ngptc,num)
      enddo
    enddo

!--- the next two appear to be appear to be coupling fields in gloopr
!--- each has four elements
    do num = 1,4
      write (xtra,'(I1)') num 
      idx = idx + 1
      Diag(idx)%axes = 2
      Diag(idx)%name = 'dswcmp_'//trim(xtra)
      Diag(idx)%desc = 'dswcmp dagnostic '//trim(xtra)//' - GFS radiation'
      Diag(idx)%unit = 'XXX'
      Diag(idx)%mod_name = 'gfs_phys'
      do nb = 1,nblks
        nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
        ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
        ngptc = nx*ny
        Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dswcmp(1:ngptc,num)
      enddo
    enddo

    do num = 1,4
      write (xtra,'(I1)') num 
      idx = idx + 1
      Diag(idx)%axes = 2
      Diag(idx)%name = 'uswcmp_'//trim(xtra)
      Diag(idx)%desc = 'uswcmp dagnostic '//trim(xtra)//' - GFS radiation'
      Diag(idx)%unit = 'XXX'
      Diag(idx)%mod_name = 'gfs_phys'
      do nb = 1,nblks
        nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
        ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
        ngptc = nx*ny
        Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%uswcmp(1:ngptc,num)
      enddo
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'sw_upfxc'
    Diag(idx)%desc = 'total sky upward sw flux at toa - GFS radiation'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%topfsw(1:ngptc)%upfxc
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'sw_dnfxc'
    Diag(idx)%desc = 'total sky downward sw flux at toa - GFS radiation'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%topfsw(1:ngptc)%dnfxc
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'sw_upfx0'
    Diag(idx)%desc = 'clear sky upward sw flux at toa - GFS radiation'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%topfsw(1:ngptc)%upfx0
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'lw_upfxc'
    Diag(idx)%desc = 'total sky upward lw flux at toa - GFS radiation'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%topflw(1:ngptc)%upfxc
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'lw_upfx0'
    Diag(idx)%desc = 'clear sky upward lw flux at toa - GFS radiation'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%topflw(1:ngptc)%upfx0
    enddo

!--- physics accumulated diagnostics ---
    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'srunoff'
    Diag(idx)%desc = 'surface water runoff - GFS lsm'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%srunoff(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'evbsa'
    Diag(idx)%desc = 'evbsa - GFS lsm'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%evbsa(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'evcwa'
    Diag(idx)%desc = 'evcwa - GFS lsm'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%evcwa(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'snohfa'
    Diag(idx)%desc = 'snohfa - GFS lsm'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%snohfa(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'transa'
    Diag(idx)%desc = 'transa - GFS lsm'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%transa(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'sbsnoa'
    Diag(idx)%desc = 'sbsnoa - GFS lsm'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%sbsnoa(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'snowca'
    Diag(idx)%desc = 'snowca - GFS lsm'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%snowca(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'soilm'
    Diag(idx)%desc = 'total column soil moisture content [kg/m**2]'
    Diag(idx)%unit = 'kg/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_th
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2 (1:nx,1:ny) => Gfs_diags(nb)%soilm(1:ngptc)
      Diag(idx)%data(nb)%var21(1:nx,1:ny) => Sfc_props(nb)%slmsk(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tmpmin'
    Diag(idx)%desc = 'min temperature at 2m height'
    Diag(idx)%unit = 'k'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%tmpmin(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tmpmax'
    Diag(idx)%desc = 'max temperature at 2m height'
    Diag(idx)%unit = 'k'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%tmpmax(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dusfc'
    Diag(idx)%desc = 'surface zonal momentum flux [N/m**2]'
    Diag(idx)%unit = 'N/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dusfc(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dvsfc'
    Diag(idx)%desc = 'surface meridional momentum flux [N/m**2]'
    Diag(idx)%unit = 'N/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dvsfc(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dtsfc'
    Diag(idx)%desc = 'surface sensible heat flux [W/m**2]'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dtsfc(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dqsfc'
    Diag(idx)%desc = 'surface latent heat flux [W/m**2]'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dqsfc(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'totprcp'
    Diag(idx)%desc = 'surface precipitation rate [kg/m**2/s]'
    Diag(idx)%unit = 'kg/m**2/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_th/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%totprcp(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'gflux'
    Diag(idx)%desc = 'surface ground heat flux [W/m**2]'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2 (1:nx,1:ny) => Gfs_diags(nb)%gflux(1:ngptc)
      Diag(idx)%data(nb)%var21(1:nx,1:ny) => Sfc_props(nb)%slmsk(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dlwsfc'
    Diag(idx)%desc = 'time accumulated downward lw flux at surface- GFS physics'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dlwsfc(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ulwsfc'
    Diag(idx)%desc = 'time accumulated upward lw flux at surface- GFS physics'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%ulwsfc(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'suntim'
    Diag(idx)%desc = 'sunshine duration time'
    Diag(idx)%unit = 's'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%suntim(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'runoff'
    Diag(idx)%desc = 'total water runoff'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%runoff(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ep'
    Diag(idx)%desc = 'potential evaporation'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%ep(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'cldwrk'
    Diag(idx)%desc = 'cloud workfunction (valid only with sas)'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%cldwrk(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dugwd'
    Diag(idx)%desc = 'surface zonal gravity wave stress [N/m**2]'
    Diag(idx)%unit = 'N/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dugwd(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dvgwd'
    Diag(idx)%desc = 'surface meridional gravity wave stress [N/m**2]'
    Diag(idx)%unit = 'N/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dvgwd(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'psmean'
    Diag(idx)%desc = 'surface pressure'
    Diag(idx)%unit = 'kPa'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%psmean(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'cnvprcp'
    Diag(idx)%desc = 'surface convective precipitation rate [kg/m**2/s]'
    Diag(idx)%unit = 'kg/m**2/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_th/cn_hr/fhzero
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%cnvprcp(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'spfhmin'
    Diag(idx)%desc = 'minimum specific humidity'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%spfhmin(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'spfhmax'
    Diag(idx)%desc = 'maximum specific humidity'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%spfhmax(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'rain'
    Diag(idx)%desc = 'total rain at this time step'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%rain(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'rainc'
    Diag(idx)%desc = 'convective rain at this time step'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%rainc(1:ngptc)
    enddo

!--- physics instantaneous diagnostics ---
    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'u10m'
    Diag(idx)%desc = '10 meter u wind [m/s]'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%u10m(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'v10m'
    Diag(idx)%desc = '10 meter v wind [m/s]'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%v10m(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'zlvl'
    Diag(idx)%desc = 'layer 1 height'
    Diag(idx)%unit = 'm'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%zlvl(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'psurf'
    Diag(idx)%desc = 'surface pressure [Pa]'
    Diag(idx)%unit = 'Pa'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%psurf(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'hpbl'
    Diag(idx)%desc = 'surface planetary boundary layer height [m]'
    Diag(idx)%unit = 'm'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%hpbl(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'pwat'
    Diag(idx)%desc = 'atmos column precipitable water [kg/m**2]'
    Diag(idx)%unit = 'kg/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%pwat(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 't1'
    Diag(idx)%desc = 'layer 1 temperature'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%t1(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'q1'
    Diag(idx)%desc = 'layer 1 specific humidity'
    Diag(idx)%unit = 'kg/kg'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%q1(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'u1'
    Diag(idx)%desc = 'layer 1 zonal wind'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%u1(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'v1'
    Diag(idx)%desc = 'layer 1 meridional wind'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%v1(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'chh'
    Diag(idx)%desc = 'thermal exchange coefficient'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%chh(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'cmm'
    Diag(idx)%desc = 'momentum exchange coefficient'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%cmm(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dlwsfci'
    Diag(idx)%desc = 'instantaneous sfc downward lw flux'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dlwsfci(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ulwsfci'
    Diag(idx)%desc = 'instantaneous sfc upward lw flux'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%ulwsfci(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dswsfci'
    Diag(idx)%desc = 'instantaneous sfc downward sw flux'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dswsfci(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'uswsfci'
    Diag(idx)%desc = 'instantaneous sfc upward sw flux'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%uswsfci(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dusfci'
    Diag(idx)%desc = 'instantaneous u component of surface stress'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dusfci(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dvsfci'
    Diag(idx)%desc = 'instantaneous v component of surface stress'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dvsfci(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dtsfci'
    Diag(idx)%desc = 'instantaneous surface sensible heat flux'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dtsfci(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dqsfci'
    Diag(idx)%desc = 'instantaneous surface latent heat flux'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%dqsfci(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'gfluxi'
    Diag(idx)%desc = 'instantaneous surface ground heat flux'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%gfluxi(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'epi'
    Diag(idx)%desc = 'instantaneous surface potential evaporation'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%epi(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'smcwlt2'
    Diag(idx)%desc = 'wiltimg point (volumetric)'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%smcwlt2(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'smcref2'
    Diag(idx)%desc = 'soil moisture threshold (volumetric)'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%smcref2(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'wet1'
    Diag(idx)%desc = 'normalized soil wetness'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%wet1(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'sr'
    Diag(idx)%desc = 'ratio of snow to total precipitation'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Gfs_diags(nb)%sr(1:ngptc)
    enddo

!--- three-dimensional variables that need to be handled special when writing 
    do num = 1,6
      write (xtra,'(I1)') num 
      idx = idx + 1
      Diag(idx)%axes = 3
      Diag(idx)%name = 'dt3dt_'//trim(xtra)
      Diag(idx)%desc = 'temperature change due to physics '//trim(xtra)//''
      Diag(idx)%unit = 'XXX'
      Diag(idx)%mod_name = 'gfs_phys'
    enddo

    do num = 1,5+Mdl_parms%pl_coeff
      write (xtra,'(I1)') num 
      idx = idx + 1
      Diag(idx)%axes = 3
      Diag(idx)%name = 'dq3dt_'//trim(xtra)
      Diag(idx)%desc = 'moisture change due to physics '//trim(xtra)//''
      Diag(idx)%unit = 'XXX'
      Diag(idx)%mod_name = 'gfs_phys'
    enddo

    do num = 1,4
      write (xtra,'(I1)') num 
      idx = idx + 1
      Diag(idx)%axes = 3
      Diag(idx)%name = 'du3dt_'//trim(xtra)
      Diag(idx)%desc = 'u momentum change due to physics '//trim(xtra)//''
      Diag(idx)%unit = 'XXX'
      Diag(idx)%mod_name = 'gfs_phys'
    enddo

    do num = 1,4
      write (xtra,'(I1)') num 
      idx = idx + 1
      Diag(idx)%axes = 3
      Diag(idx)%name = 'dv3dt_'//trim(xtra)
      Diag(idx)%desc = 'v momentum change due to physics '//trim(xtra)//''
      Diag(idx)%unit = 'XXX'
      Diag(idx)%mod_name = 'gfs_phys'
    enddo

    idx = idx + 1
    Diag(idx)%axes = 3
    Diag(idx)%name = 'dqdt_v'
    Diag(idx)%desc = 'total moisture tendency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'

!--- Surface diagnostics in gfs_sfc
    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'alnsf'
    Diag(idx)%desc = 'mean nir albedo with strong cosz dependency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%alnsf(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'alnwf'
    Diag(idx)%desc = 'mean nir albedo with weak cosz dependency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%alnwf(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'alvsf'
    Diag(idx)%desc = 'mean vis albedo with strong cosz dependency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%alvsf(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'alvwf'
    Diag(idx)%desc = 'mean vis albedo with weak cosz dependency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%alvwf(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'canopy'
    Diag(idx)%desc = 'canopy water (cnwat in gfs data)'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%canopy(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'f10m'
    Diag(idx)%desc = '10-meter wind speed divided by lowest model wind speed'
    Diag(idx)%unit = 'N/A'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%f10m(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'facsf'
    Diag(idx)%desc = 'fractional coverage with strong cosz dependency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%facsf(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'facwf'
    Diag(idx)%desc = 'fractional coverage with weak cosz dependency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%facwf(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ffhh'
    Diag(idx)%desc = 'fh parameter from PBL scheme'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%ffhh(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ffmm'
    Diag(idx)%desc = 'fm parameter from PBL scheme'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%ffmm(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'fice'
    Diag(idx)%desc = 'surface ice concentration (ice=1; no ice=0) [fraction]'
    Diag(idx)%unit = 'fraction'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%fice(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'hice'
    Diag(idx)%desc = 'sea ice thickness (icetk in gfs_data)'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%hice(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'snoalb'
    Diag(idx)%desc = 'maximum snow albedo in fraction (salbd?? in gfs data)'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%snoalb(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'shdmax'
    Diag(idx)%desc = 'maximum fractional coverage of green vegetation'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%shdmax(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'shdmin'
    Diag(idx)%desc = 'minimum fractional coverage of green vegetation'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%shdmin(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'snowd'
    Diag(idx)%desc = 'surface snow depth [m]'
    Diag(idx)%unit = 'm'
    Diag(idx)%mod_name = 'gfs_sfc'
    Diag(idx)%cnvfac = cn_one/cn_th
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%snowd(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'stype'
    Diag(idx)%desc = 'soil type in integer 1-9'
    Diag(idx)%unit = 'N/A'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%stype(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'q2m'
    Diag(idx)%desc = '2m specific humidity [kg/kg]'
    Diag(idx)%unit = 'kg/kg'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%q2m(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 't2m'
    Diag(idx)%desc = '2m temperature [K]'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%t2m(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tsfc'
    Diag(idx)%desc = 'surface temperature [K]'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%tsfc(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tg3'
    Diag(idx)%desc = 'deep soil temperature'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%tg3(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tisfc'
    Diag(idx)%desc = 'surface temperature over ice fraction'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%tisfc(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tprcp'
    Diag(idx)%desc = 'total precipitation'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Tbd_data(nb)%tprcp(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'vtype'
    Diag(idx)%desc = 'vegetation type in integer 1-13'
    Diag(idx)%unit = 'N/A'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%vtype(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'weasd'
    Diag(idx)%desc = 'surface snow water equivalent [kg/m**2]'
    Diag(idx)%unit = 'kg/m**2'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%weasd(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'HGTsfc'
    Diag(idx)%desc = 'surface geopotential height [gpm]'
    Diag(idx)%unit = 'gpm'
    Diag(idx)%mod_name = 'gfs_sfc'
    Diag(idx)%cnvfac = con_g
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%oro(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'SLMSKsfc'
    Diag(idx)%desc = 'sea-land-ice mask (0-sea, 1-land, 2-ice)'
    Diag(idx)%unit = 'N/A'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%slmsk(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ZORLsfc'
    Diag(idx)%desc = 'surface roughness [m]'
    Diag(idx)%unit = 'm'
    Diag(idx)%mod_name = 'gfs_sfc'
    Diag(idx)%cnvfac = cn_one/cn_100
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%zorl(1:ngptc)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'VFRACsfc'
    Diag(idx)%desc = 'vegetation fraction'
    Diag(idx)%unit = 'N/A'
    Diag(idx)%mod_name = 'gfs_sfc'
    Diag(idx)%cnvfac = cn_100
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Sfc_props(nb)%vfrac(1:ngptc)
    enddo

    do num = 1,4
      write (xtra,'(I1)') num 
      idx = idx + 1
      Diag(idx)%axes = 2
      Diag(idx)%name = 'slc_'//trim(xtra)
      Diag(idx)%desc = 'liquid soil mositure at layer-'//trim(xtra)
      Diag(idx)%unit = 'XXX'
      Diag(idx)%mod_name = 'gfs_sfc'
      do nb = 1,nblks
        nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
        ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
        ngptc = nx*ny
        Diag(idx)%data(nb)%var2(1:nx,1:ny) => Tbd_data(nb)%slc(1:ngptc,num)
      enddo
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'SOILW1'
    Diag(idx)%desc = 'volumetric soil moisture 0-10cm [fraction]'
    Diag(idx)%unit = 'fraction'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Tbd_data(nb)%smc(1:ngptc,1)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'SOILW2'
    Diag(idx)%desc = 'volumetric soil moisture 10-40cm [fraction]'
    Diag(idx)%unit = 'fraction'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Tbd_data(nb)%smc(1:ngptc,2)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'SOILW3'
    Diag(idx)%desc = 'volumetric soil moisture 40-100cm [fraction]'
    Diag(idx)%unit = 'fraction'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Tbd_data(nb)%smc(1:ngptc,3)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'SOILW4'
    Diag(idx)%desc = 'volumetric soil moisture 100-200cm [fraction]'
    Diag(idx)%unit = 'fraction'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Tbd_data(nb)%smc(1:ngptc,4)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'SOILT1'
    Diag(idx)%desc = 'soil temperature 0-10cm [K]' 
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Tbd_data(nb)%stc(1:ngptc,1)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'SOILT2'
    Diag(idx)%desc = 'soil temperature 10-40cm [K]' 
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Tbd_data(nb)%stc(1:ngptc,2)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'SOILT3'
    Diag(idx)%desc = 'soil temperature 40-100cm [K]' 
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Tbd_data(nb)%stc(1:ngptc,3)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'SOILT4'
    Diag(idx)%desc = 'soil temperature 100-200cm [K]' 
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_sfc'
    do nb = 1,nblks
      nx = Atm_block%ibe(nb)-Atm_block%ibs(nb)+1
      ny = Atm_block%jbe(nb)-Atm_block%jbs(nb)+1
      ngptc = nx*ny
      Diag(idx)%data(nb)%var2(1:nx,1:ny) => Tbd_data(nb)%stc(1:ngptc,4)
    enddo

!--- prognostic variable tendencies (T, u, v, sph, clwmr, o3)
    idx = idx + 1
    Diag(idx)%axes = 3
    Diag(idx)%name = 'dtemp_dt'
    Diag(idx)%desc = 'GFS radiation/physics temperature tendency'
    Diag(idx)%unit = 'K/s'
    Diag(idx)%mod_name = 'gfs_phys'

    idx = idx + 1
    Diag(idx)%axes = 3
    Diag(idx)%name = 'du_dt'
    Diag(idx)%desc = 'GFS radiation/physics horizontal wind component tendency'
    Diag(idx)%unit = 'm/s/s'
    Diag(idx)%mod_name = 'gfs_phys'

    idx = idx + 1
    Diag(idx)%axes = 3
    Diag(idx)%name = 'dv_dt'
    Diag(idx)%desc = 'GFS radiation/physics meridional wind component tendency'
    Diag(idx)%unit = 'm/s/s'
    Diag(idx)%mod_name = 'gfs_phys'

    idx = idx + 1
    Diag(idx)%axes = 3
    Diag(idx)%name = 'dsphum_dt'
    Diag(idx)%desc = 'GFS radiation/physics specific humidity tendency'
    Diag(idx)%unit = 'kg/kg/s'
    Diag(idx)%mod_name = 'gfs_phys'

    idx = idx + 1
    Diag(idx)%axes = 3
    Diag(idx)%name = 'dclwmr_dt'
    Diag(idx)%desc = 'GFS radiation/radiation cloud water mixing ratio tendency'
    Diag(idx)%unit = 'kg/kg/s'
    Diag(idx)%mod_name = 'gfs_phys'

    idx = idx + 1
    Diag(idx)%axes = 3
    Diag(idx)%name = 'do3mr_dt'
    Diag(idx)%desc = 'GFS radiation/radiation ozone mixing ratio tendency'
    Diag(idx)%unit = 'kg/kg/s'
    Diag(idx)%mod_name = 'gfs_phys'

    tot_diag_idx = idx

    if (idx > DIAG_SIZE) then
      call mpp_error(FATAL, 'gfs_driver::gfs_diag_register - need to increase DIAG_SIZE') 
    endif

    do idx = 1,tot_diag_idx
      if (diag(idx)%axes == -99) then
        call mpp_error(FATAL, 'gfs_driver::gfs_diag_register - attempt to register an undefined variable') 
      endif
      Diag(idx)%id = register_diag_field (trim(Diag(idx)%mod_name), trim(Diag(idx)%name),  &
                                           axes(1:Diag(idx)%axes), Time, trim(Diag(idx)%desc), &
                                           trim(Diag(idx)%unit), missing_value=real(missing_value))
    enddo
!!!#endif

  end subroutine gfs_diag_register
!-------------------------------------------------------------------------      


!-------------------------------------------------------------------------      
!--- gfs_diag_output ---
!-------------------------------------------------------------------------      
!    routine to transfer the diagnostic data to the GFDL FMS diagnostic 
!    manager for eventual output to the history files.
!
!    calls:  send_data
!-------------------------------------------------------------------------      
  subroutine gfs_diag_output(Time, Gfs_diags, Statein, Stateout, Atm_block, &
                             nb, nx, ny, levs, ntcw, ntoz, dt)
!--- subroutine interface variable definitions
    type(time_type),           intent(in) :: Time
    type(diagnostics),         intent(in) :: Gfs_diags
    type(state_fields_in),     intent(in) :: Statein
    type(state_fields_out),    intent(in) :: Stateout
    type (block_control_type), intent(in) :: Atm_block
    integer,                   intent(in) :: nb, nx, ny, levs, ntcw, ntoz
    real(kind=kind_phys),      intent(in) :: dt
!--- local variables
    integer :: i, j, ngptc, idx, num
    character(len=2) :: xtra
    real(kind=kind_phys), dimension(nx,ny) :: var2
    real(kind=kind_phys), dimension(nx,ny,levs) :: var3
    logical :: used

     ngptc = nx*ny

     do idx = 1,tot_diag_idx
       if (Diag(idx)%id > 0) then
         if (Diag(idx)%axes == 2) then
           if (trim(Diag(idx)%name) == 'ALBDOsfc') then
             !--- albedos are actually a ratio of two radiation surface properties
             var2 = 0._kind_phys
             where (Diag(idx)%data(nb)%var21 > 0._kind_phys) &
                   var2 = max(0._kind_phys,Diag(idx)%data(nb)%var2/Diag(idx)%data(nb)%var21)*Diag(idx)%cnvfac
             used=send_data(Diag(idx)%id, var2, Time,    &
                            is_in=Diag(idx)%data(nb)%is, &
                            js_in=Diag(idx)%data(nb)%js) 
           elseif (trim(Diag(idx)%name) == 'gflux') then
             !--- need to "mask" gflux to output valid data over land/ice only
             var2(1:nx,1:ny) = missing_value
             where (Diag(idx)%data(nb)%var21 /= 0) var2 = Diag(idx)%data(nb)%var2*Diag(idx)%cnvfac
             used=send_data(Diag(idx)%id, var2, Time,    &
                            is_in=Diag(idx)%data(nb)%is, &
                            js_in=Diag(idx)%data(nb)%js) 
           elseif (trim(Diag(idx)%name) == 'soilm') then
             !--- need to "mask" soilm to have value only over land
             var2(1:nx,1:ny) = missing_value
             where (Diag(idx)%data(nb)%var21 == 1) var2 = Diag(idx)%data(nb)%var2*Diag(idx)%cnvfac
             used=send_data(Diag(idx)%id, var2, Time,    &
                            is_in=Diag(idx)%data(nb)%is, &
                            js_in=Diag(idx)%data(nb)%js) 
           else
             used=send_data(Diag(idx)%id, Diag(idx)%data(nb)%var2*Diag(idx)%cnvfac, Time, &
                            is_in=Diag(idx)%data(nb)%is, &
                            js_in=Diag(idx)%data(nb)%js) 
           endif
         elseif (Diag(idx)%axes == 3) then
           !--- dt3dt variables
           do num = 1,6
             write(xtra,'(i1)') num
             if (trim(Diag(idx)%name) == 'dt3dt_'//trim(xtra)) then
               var3(1:nx,1:ny,1:levs) = RESHAPE(Gfs_diags%dt3dt(1:ngptc,levs:1:-1,num:num), (/nx,ny,levs/))
               used=send_data(Diag(idx)%id, var3, Time,    &
                              is_in=Diag(idx)%data(nb)%is, &
                              js_in=Diag(idx)%data(nb)%js, &
                              ks_in=1) 
             endif
           enddo
           !--- dq3dt variables
           do num = 1,5+Mdl_parms%pl_coeff
             write(xtra,'(i1)') num
             if (trim(Diag(idx)%name) == 'dq3dt_'//trim(xtra)) then
               var3(1:nx,1:ny,1:levs) = RESHAPE(Gfs_diags%dq3dt(1:ngptc,levs:1-1,num:num), (/nx,ny,levs/))
               used=send_data(Diag(idx)%id, var3, Time,    &
                              is_in=Diag(idx)%data(nb)%is, &
                              js_in=Diag(idx)%data(nb)%js, &
                              ks_in=1) 
             endif
           enddo
           !--- du3dt and dv3dt variables
           do num = 1,4
             write(xtra,'(i1)') num
             if (trim(Diag(idx)%name) == 'du3dt_'//trim(xtra)) then
               var3(1:nx,1:ny,1:levs) = RESHAPE(Gfs_diags%du3dt(1:ngptc,levs:1:-1,num:num), (/nx,ny,levs/))
               used=send_data(Diag(idx)%id, var3, Time,    &
                              is_in=Diag(idx)%data(nb)%is, &
                              js_in=Diag(idx)%data(nb)%js, &
                              ks_in=1) 
             endif
             if (trim(Diag(idx)%name) == 'dv3dt_'//trim(xtra)) then
               var3(1:nx,1:ny,1:levs) = RESHAPE(Gfs_diags%dv3dt(1:ngptc,levs:1:-1,num:num), (/nx,ny,levs/))
               used=send_data(Diag(idx)%id, var3, Time,    &
                              is_in=Diag(idx)%data(nb)%is, &
                              js_in=Diag(idx)%data(nb)%js, &
                              ks_in=1) 
             endif
           enddo
           if (trim(Diag(idx)%name) == 'dqdt_v') then
             var3(1:nx,1:ny,1:levs) = RESHAPE(Gfs_diags%dqdt_v(1:ngptc,levs:1:-1), (/nx,ny,levs/))
             used=send_data(Diag(idx)%id, var3, Time,    &
                            is_in=Diag(idx)%data(nb)%is, &
                            js_in=Diag(idx)%data(nb)%js, &
                              ks_in=1) 
           endif
           !--- temperature tendency
           if (trim(Diag(idx)%name) == 'dtemp_dt') then
             var3(1:nx,1:ny,1:levs) =  RESHAPE(Statein%tgrs(1:ngptc,levs:1:-1), (/nx,ny,levs/))
             var3(1:nx,1:ny,1:levs) = (RESHAPE(Stateout%gt0(1:ngptc,levs:1:-1), (/nx,ny,levs/))  &
                                        - var3(1:nx,1:ny,1:levs))/dt
             used=send_data(Diag(idx)%id, var3, Time,    &
                            is_in=Diag(idx)%data(nb)%is, &
                            js_in=Diag(idx)%data(nb)%js, &
                            ks_in=1) 
           endif
           !--- horizontal wind component tendency
           if (trim(Diag(idx)%name) == 'du_dt') then
             var3(1:nx,1:ny,1:levs) =  RESHAPE(Statein%ugrs(1:ngptc,levs:1:-1), (/nx,ny,levs/))
             var3(1:nx,1:ny,1:levs) = (RESHAPE(Stateout%gu0(1:ngptc,levs:1:-1), (/nx,ny,levs/))  &
                                        - var3(1:nx,1:ny,1:levs))/dt
             used=send_data(Diag(idx)%id, var3, Time,    &
                            is_in=Diag(idx)%data(nb)%is, &
                            js_in=Diag(idx)%data(nb)%js, &
                            ks_in=1) 
           endif
           !--- meridional wind component tendency
           if (trim(Diag(idx)%name) == 'dv_dt') then
             var3(1:nx,1:ny,1:levs) =  RESHAPE(Statein%vgrs(1:ngptc,levs:1:-1), (/nx,ny,levs/))
             var3(1:nx,1:ny,1:levs) = (RESHAPE(Stateout%gv0(1:ngptc,levs:1:-1), (/nx,ny,levs/))  &
                                        - var3(1:nx,1:ny,1:levs))/dt
             used=send_data(Diag(idx)%id, var3, Time,    &
                            is_in=Diag(idx)%data(nb)%is, &
                            js_in=Diag(idx)%data(nb)%js, &
                            ks_in=1) 
           endif
           !--- specific humidity tendency
           if (trim(Diag(idx)%name) == 'dsphum_dt') then
             var3(1:nx,1:ny,1:levs) =  RESHAPE(Statein%qgrs(1:ngptc,levs:1:-1,1:1), (/nx,ny,levs/))
             var3(1:nx,1:ny,1:levs) = (RESHAPE(Stateout%gq0(1:ngptc,levs:1:-1,1:1), (/nx,ny,levs/))  &
                                        - var3(1:nx,1:ny,1:levs))/dt
             used=send_data(Diag(idx)%id, var3, Time,    &
                            is_in=Diag(idx)%data(nb)%is, &
                            js_in=Diag(idx)%data(nb)%js, &
                            ks_in=1) 
           endif
           !--- cloud water mixing ration tendency
           if (trim(Diag(idx)%name) == 'dclwmr_dt') then
             var3(1:nx,1:ny,1:levs) =  RESHAPE(Statein%qgrs(1:ngptc,levs:1:-1,ntcw:ntcw), (/nx,ny,levs/))
             var3(1:nx,1:ny,1:levs) = (RESHAPE(Stateout%gq0(1:ngptc,levs:1:-1,ntcw:ntcw), (/nx,ny,levs/))  &
                                        - var3(1:nx,1:ny,1:levs))/dt
             used=send_data(Diag(idx)%id, var3, Time,    &
                            is_in=Diag(idx)%data(nb)%is, &
                            js_in=Diag(idx)%data(nb)%js, &
                            ks_in=1) 
           endif
           !--- ozone mixing ration tendency
           if (trim(Diag(idx)%name) == 'do3mr_dt') then
             var3(1:nx,1:ny,1:levs) =  RESHAPE(Statein%qgrs(1:ngptc,levs:1:-1,ntoz:ntoz), (/nx,ny,levs/))
             var3(1:nx,1:ny,1:levs) = (RESHAPE(Stateout%gq0(1:ngptc,levs:1:-1,ntoz:ntoz), (/nx,ny,levs/))  &
                                        - var3(1:nx,1:ny,1:levs))/dt
             used=send_data(Diag(idx)%id, var3, Time,    &
                            is_in=Diag(idx)%data(nb)%is, &
                            js_in=Diag(idx)%data(nb)%js, &
                            ks_in=1) 
           endif
         endif
       endif
     enddo


  end subroutine gfs_diag_output
!-------------------------------------------------------------------------      


!-------------------------------------------------------------------------      
!--- surface_props_output ---
!-------------------------------------------------------------------------      
!    routine to write out GFS physics restarts via the GFDL FMS restart
!    subsystem.
!    takes an optional argument to append timestamps for intermediate 
!    restarts.
!
!    calls:  register_restart_field, save_restart
!-------------------------------------------------------------------------      
  subroutine surface_props_output (Atm_block, fv_domain, timestamp)
!--- subroutine interface variable definitions
    type (block_control_type),   intent(in) :: Atm_block
    type (domain2d),             intent(in) :: fv_domain
    character(len=32), optional, intent(in) :: timestamp
!--- local variables
    integer :: i, j, k, ii, jj, ibs, ibe, jbs, jbe, nb, ix, lsoil, num
    integer :: isc, iec, jsc, jec, nx, ny, npz
    integer :: id_restart
    integer :: nvar2, nvar3
    character(len=32) :: fn_srf = 'sfc_data.nc'
    character(len=2)  :: c2 = ''
    real(kind=kind_phys), pointer, dimension(:,:)   :: var2_p => NULL()
    real(kind=kind_phys), pointer, dimension(:,:,:) :: var3_p => NULL()

    nvar2 = 32
    nvar3 = 3

    isc = Atm_block%isc
    iec = Atm_block%iec
    jsc = Atm_block%jsc
    jec = Atm_block%jec
    npz = Atm_block%npz
    nx = (iec - isc + 1)
    ny = (jec - jsc + 1)

    if (.not. allocated(sfc_name2)) then
!--- allocate the various containers needed for restarts
      allocate(sfc_name2(nvar2))
      allocate(sfc_name3(nvar3))
      allocate(sfc_var2(nx,ny,nvar2))
      allocate(sfc_var3(nx,ny,Mdl_parms%lsoil,nvar3))

!--- names of the 2D variables to save
      sfc_name2(1)  = 'slmsk'
      sfc_name2(2)  = 'tsea'    !tsfc
      sfc_name2(3)  = 'sheleg'  !weasd
      sfc_name2(4)  = 'tg3'
      sfc_name2(5)  = 'zorl'
      sfc_name2(6)  = 'alvsf'
      sfc_name2(7)  = 'alvwf'
      sfc_name2(8)  = 'alnsf'
      sfc_name2(9)  = 'alnwf'
      sfc_name2(10) = 'facsf'
      sfc_name2(11) = 'facwf'
      sfc_name2(12) = 'vfrac'
      sfc_name2(13) = 'canopy'
      sfc_name2(14) = 'f10m'
      sfc_name2(15) = 't2m'
      sfc_name2(16) = 'q2m'
      sfc_name2(17) = 'vtype'
      sfc_name2(18) = 'stype'
      sfc_name2(19) = 'uustar'
      sfc_name2(20) = 'ffmm'
      sfc_name2(21) = 'ffhh'
      sfc_name2(22) = 'hice'
      sfc_name2(23) = 'fice'
      sfc_name2(24) = 'tisfc'
      sfc_name2(25) = 'tprcp'
      sfc_name2(26) = 'srflag'
      sfc_name2(27) = 'snwdph'  !snowd
      sfc_name2(28) = 'shdmin'
      sfc_name2(29) = 'shdmax'
      sfc_name2(30) = 'slope'
      sfc_name2(31) = 'snoalb'
      sfc_name2(32) = 'sncovr'
!
!--- register the 2D fields
      do num = 1,nvar2
        var2_p => sfc_var2(:,:,num)
        id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name2(num), var2_p, domain=fv_domain)
      enddo
      nullify(var2_p)
!
!--- names of the 2D variables to save
      sfc_name3(1) = 'stc'
      sfc_name3(2) = 'smc'
      sfc_name3(3) = 'slc'
!
!--- register the 3D fields
      do num = 1,nvar3
        var3_p => sfc_var3(:,:,:,num)
        id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name3(num), var3_p, domain=fv_domain)
      enddo
      nullify(var3_p)
    endif
    if (.not. allocated(phy_f2d)) then
      allocate(phy_f2d(nx,ny,Mdl_parms%num_p2d))
      allocate(phy_f3d(nx,ny,npz,Mdl_parms%num_p3d+Mdl_parms%npdf3d))
      id_restart = register_restart_field(Sfc_restart, fn_srf, 'phy_f2d', phy_f2d, domain=fv_domain)
      do num = 1,size(phy_f3d,4)
        write(c2,'(i2.2)') num
        var3_p => phy_f3d(:,:,:,num)
        id_restart = register_restart_field(Sfc_restart, fn_srf, 'phy_f3d_'//c2, var3_p, domain=fv_domain)
      enddo
      nullify(var3_p)
    endif
   
    do nb = 1, Atm_block%nblks
      ibs = Atm_block%ibs(nb)
      ibe = Atm_block%ibe(nb)
      jbs = Atm_block%jbs(nb)
      jbe = Atm_block%jbe(nb)
!--- 2D variables
      do jj=jbs,jbe
        j = jj - jsc + 1
        do ii=ibs,ibe
          i = ii - isc + 1
          ix = Atm_block%ix(nb)%ix(ii,jj)
          !--- slmsk
          sfc_var2(i,j,1)  = Sfc_props(nb)%slmsk(ix)
          !--- tsfc (tsea in sfc file)
          sfc_var2(i,j,2)  = Sfc_props(nb)%tsfc(ix)
          !--- weasd (sheleg in sfc file)
          sfc_var2(i,j,3)  = Sfc_props(nb)%weasd(ix)
          !--- tg3
          sfc_var2(i,j,4)  = Sfc_props(nb)%tg3(ix)
          !--- zorl
          sfc_var2(i,j,5)  = Sfc_props(nb)%zorl(ix)
          !--- alvsf
          sfc_var2(i,j,6)  = Sfc_props(nb)%alvsf(ix)
          !--- alvwf
          sfc_var2(i,j,7)  = Sfc_props(nb)%alvwf(ix)
          !--- alnsf
          sfc_var2(i,j,8)  = Sfc_props(nb)%alnsf(ix)
          !--- alnwf
          sfc_var2(i,j,9)  = Sfc_props(nb)%alnwf(ix)
          !--- facsf
          sfc_var2(i,j,10) = Sfc_props(nb)%facsf(ix)
          !--- facwf
          sfc_var2(i,j,11) = Sfc_props(nb)%facwf(ix)
          !--- vfrac
          sfc_var2(i,j,12) = Sfc_props(nb)%vfrac(ix)
          !--- canopy
          sfc_var2(i,j,13) = Sfc_props(nb)%canopy(ix)
          !--- f10m
          sfc_var2(i,j,14) = Sfc_props(nb)%f10m(ix)
          !--- t2m
          sfc_var2(i,j,15) = Sfc_props(nb)%t2m(ix)
          !--- q2m
          sfc_var2(i,j,16) = Sfc_props(nb)%q2m(ix)
          !--- vtype
          sfc_var2(i,j,17) = Sfc_props(nb)%vtype(ix)
          !--- stype
          sfc_var2(i,j,18) = Sfc_props(nb)%stype(ix)
          !--- uustar
          sfc_var2(i,j,19) = Sfc_props(nb)%uustar(ix)
          !--- ffmm
          sfc_var2(i,j,20) = Sfc_props(nb)%ffmm(ix)
          !--- ffhh
          sfc_var2(i,j,21) = Sfc_props(nb)%ffhh(ix)
          !--- hice
          sfc_var2(i,j,22) = Sfc_props(nb)%hice(ix)
          !--- fice
          sfc_var2(i,j,23) = Sfc_props(nb)%fice(ix)
          !--- tisfc
          sfc_var2(i,j,24) = Sfc_props(nb)%tisfc(ix)
          !--- tprcp
          sfc_var2(i,j,25) = Tbd_data(nb)%tprcp(ix)
          !--- srflag
          sfc_var2(i,j,26) = Tbd_data(nb)%srflag(ix)
          !--- snowd (snwdph in the file)
          sfc_var2(i,j,27) = Sfc_props(nb)%snowd(ix)
          !--- shdmin
          sfc_var2(i,j,28) = Sfc_props(nb)%shdmin(ix)
          !--- shdmax
          sfc_var2(i,j,29) = Sfc_props(nb)%shdmax(ix)
          !--- slope
          sfc_var2(i,j,30) = Sfc_props(nb)%slope(ix)
          !--- snoalb
          sfc_var2(i,j,31) = Sfc_props(nb)%snoalb(ix)
          !--- sncovr
          sfc_var2(i,j,32) = Sfc_props(nb)%sncovr(ix)
        enddo
      enddo
!
!--- 3D variables
      do lsoil = 1,Mdl_parms%lsoil
        do jj=jbs,jbe
          j = jj - jsc + 1
          do ii=ibs,ibe
            i = ii - isc + 1
            ix = Atm_block%ix(nb)%ix(ii,jj)
            !--- stc
            sfc_var3(i,j,lsoil,1) = Tbd_data(nb)%stc(ix,lsoil)
            !--- smc
            sfc_var3(i,j,lsoil,2) = Tbd_data(nb)%smc(ix,lsoil)
            !--- slc
            sfc_var3(i,j,lsoil,3) = Tbd_data(nb)%slc(ix,lsoil)
          enddo
        enddo
      enddo
!
!--- phy_f*d variables
      do num = 1,size(phy_f2d,3)
        do jj=jbs,jbe
          j = jj - jsc + 1
          do ii=ibs,ibe
            i = ii - isc + 1
            ix = Atm_block%ix(nb)%ix(ii,jj)
            phy_f2d(i,j,num) = Tbd_data(nb)%phy_f2d(ix,num)
          enddo
        enddo
      enddo
      do num = 1,size(phy_f3d,4)
        do k=1,npz
          do jj=jbs,jbe
            j = jj - jsc + 1
            do ii=ibs,ibe
              i = ii - isc + 1
              ix = Atm_block%ix(nb)%ix(ii,jj)
              phy_f3d(i,j,k,num) = Tbd_data(nb)%phy_f3d(ix,k,num)
            enddo
          enddo
        enddo
      enddo
    enddo

    call save_restart(Sfc_restart, timestamp)

  end subroutine surface_props_output
!-------------------------------------------------------------------------      


!-------------------------------------------------------------------------      
!--- checksum ---
!-------------------------------------------------------------------------      
!    routine to checksum GFS physics variables for across MPI-ranks for
!    debugging purposes.
!
!    calls:  mpp_chksum
!-------------------------------------------------------------------------      
  subroutine checksum(Atm_block, Statein)
!--- subroutine interface variable definitions
   type (block_control_type),   intent(in) :: Atm_block
   type(state_fields_in), dimension(:), intent(in),optional :: Statein
!--- local variables
   integer :: outunit, j, i, ix, jbs, jbe, ibs, ibe, nb
   real :: temp2d(Atm_block%isc:Atm_block%iec,Atm_block%jsc:Atm_block%jec,83)
   real :: temp3d(Atm_block%isc:Atm_block%iec,Atm_block%jsc:Atm_block%jec,Atm_block%npz,23)
   character(len=32) :: name

   temp2d = 0.
   temp3d = 0.

   do nb = 1,Atm_block%nblks
     ibs = Atm_block%ibs(nb)
     ibe = Atm_block%ibe(nb)
     jbs = Atm_block%jbs(nb)
     jbe = Atm_block%jbe(nb)

     do j=jbs,jbe
       do i=ibs,ibe
         ix = Atm_block%ix(nb)%ix(i,j)
         temp2d(i,j, 1) = Tbd_data(nb)%slc(ix,1)
         temp2d(i,j, 2) = Tbd_data(nb)%slc(ix,2)
         temp2d(i,j, 3) = Tbd_data(nb)%slc(ix,3)
         temp2d(i,j, 4) = Tbd_data(nb)%slc(ix,4)
         temp2d(i,j, 5) = Tbd_data(nb)%smc(ix,1)
         temp2d(i,j, 6) = Tbd_data(nb)%smc(ix,2)
         temp2d(i,j, 7) = Tbd_data(nb)%smc(ix,3)
         temp2d(i,j, 8) = Tbd_data(nb)%smc(ix,4)
         temp2d(i,j, 9) = Tbd_data(nb)%stc(ix,1)
         temp2d(i,j,10) = Tbd_data(nb)%stc(ix,2)
         temp2d(i,j,11) = Tbd_data(nb)%stc(ix,3)
         temp2d(i,j,12) = Tbd_data(nb)%stc(ix,4)
         temp2d(i,j,13) = Tbd_data(nb)%tprcp(ix)
         if (Mdl_parms%nst_fcst > 0 )  then
           temp2d(i,j,14) = Tbd_data(nb)%tref(ix)
           temp2d(i,j,15) = Tbd_data(nb)%z_c(ix)
           temp2d(i,j,16) = Tbd_data(nb)%c_0(ix)
           temp2d(i,j,17) = Tbd_data(nb)%c_d(ix)
           temp2d(i,j,18) = Tbd_data(nb)%w_0(ix)
           temp2d(i,j,19) = Tbd_data(nb)%w_d(ix)
         endif
         temp2d(i,j,20) = Tbd_data(nb)%fscav(ix)
         temp2d(i,j,21) = Tbd_data(nb)%fswtr(ix)

         temp2d(i,j,22) = Sfc_props(nb)%slmsk(ix)
         temp2d(i,j,23) = Sfc_props(nb)%tsfc(ix)
         temp2d(i,j,24) = Sfc_props(nb)%snowd(ix)
         temp2d(i,j,25) = Sfc_props(nb)%sncovr(ix)
         temp2d(i,j,26) = Sfc_props(nb)%snoalb(ix)
         temp2d(i,j,27) = Sfc_props(nb)%zorl(ix)
         temp2d(i,j,28) = Sfc_props(nb)%hprim(ix)
         temp2d(i,j,29) = Sfc_props(nb)%fice(ix)
         temp2d(i,j,30) = Sfc_props(nb)%tisfc(ix)
         temp2d(i,j,31) = Sfc_props(nb)%alvsf(ix)
         temp2d(i,j,32) = Sfc_props(nb)%alnsf(ix)
         temp2d(i,j,33) = Sfc_props(nb)%alvwf(ix)
         temp2d(i,j,34) = Sfc_props(nb)%alnwf(ix)
         temp2d(i,j,35) = Sfc_props(nb)%facsf(ix)
         temp2d(i,j,36) = Sfc_props(nb)%facwf(ix)
         temp2d(i,j,37) = Sfc_props(nb)%slope(ix)
         temp2d(i,j,38) = Sfc_props(nb)%shdmin(ix)
         temp2d(i,j,39) = Sfc_props(nb)%shdmax(ix)
         temp2d(i,j,40) = Sfc_props(nb)%tg3(ix)
         temp2d(i,j,41) = Sfc_props(nb)%vfrac(ix)
         temp2d(i,j,42) = Sfc_props(nb)%vtype(ix)
         temp2d(i,j,43) = Sfc_props(nb)%stype(ix)
         temp2d(i,j,44) = Sfc_props(nb)%uustar(ix)
         temp2d(i,j,45) = Sfc_props(nb)%oro(ix)
         temp2d(i,j,46) = Sfc_props(nb)%oro_uf(ix)
         temp2d(i,j,47) = Sfc_props(nb)%hice(ix)
         temp2d(i,j,48) = Sfc_props(nb)%weasd(ix)
         temp2d(i,j,49) = Sfc_props(nb)%canopy(ix)
         temp2d(i,j,50) = Sfc_props(nb)%ffmm(ix)
         temp2d(i,j,51) = Sfc_props(nb)%ffhh(ix)
         temp2d(i,j,52) = Sfc_props(nb)%f10m(ix)
         temp2d(i,j,53) = Sfc_props(nb)%t2m(ix)

         temp2d(i,j,54) = Intr_flds(nb)%sfcdsw(ix)
         temp2d(i,j,55) = Intr_flds(nb)%sfcnsw(ix)
         temp2d(i,j,56) = Intr_flds(nb)%sfcdlw(ix)
         temp2d(i,j,57) = Intr_flds(nb)%sfcfsw(ix)%upfxc
         temp2d(i,j,58) = Intr_flds(nb)%sfcfsw(ix)%dnfxc
         temp2d(i,j,59) = Intr_flds(nb)%sfcfsw(ix)%upfx0
         temp2d(i,j,60) = Intr_flds(nb)%sfcfsw(ix)%dnfx0
         temp2d(i,j,61) = Intr_flds(nb)%sfcflw(ix)%upfxc
         temp2d(i,j,62) = Intr_flds(nb)%sfcflw(ix)%upfx0
         temp2d(i,j,63) = Intr_flds(nb)%sfcflw(ix)%dnfxc
         temp2d(i,j,64) = Intr_flds(nb)%sfcflw(ix)%dnfx0

         temp2d(i,j,65) = Cld_props(nb)%cv(ix)
         temp2d(i,j,66) = Cld_props(nb)%cvt(ix)
         temp2d(i,j,67) = Cld_props(nb)%cvb(ix)
         temp2d(i,j,68) = Cld_props(nb)%flgmin(ix)

         temp2d(i,j,69) = Rad_tends(nb)%sfalb(ix)
         temp2d(i,j,70) = Rad_tends(nb)%coszen(ix)
         temp2d(i,j,71) = Rad_tends(nb)%tsflw(ix)
         temp2d(i,j,72) = Rad_tends(nb)%semis(ix)
         temp2d(i,j,73) = Rad_tends(nb)%coszdg(ix)
         temp2d(i,j,74) = Rad_tends(nb)%rqtk(ix)

         temp2d(i,j,75) = Dyn_parms(nb)%xlon(ix)
         temp2d(i,j,76) = Dyn_parms(nb)%xlat(ix)
         temp2d(i,j,77) = Dyn_parms(nb)%area(ix)
         temp2d(i,j,78) = Dyn_parms(nb)%dx(ix)
         temp2d(i,j,79) = Dyn_parms(nb)%dy(ix)
         temp2d(i,j,80) = Dyn_parms(nb)%sinlat(ix)
         temp2d(i,j,81) = Dyn_parms(nb)%coslat(ix)
         temp2d(i,j,82) = Dyn_parms(nb)%icsdsw(ix)
         temp2d(i,j,83) = Dyn_parms(nb)%icsdlw(ix)

         if (present(Statein)) then
           temp3d(i,j,:, 1) = Statein(nb)%qgrs_rad(ix,:)
           temp3d(i,j,:, 2) = Statein(nb)%tracer(ix,:,1)
           temp3d(i,j,:, 3) = Statein(nb)%tracer(ix,:,2)
           temp3d(i,j,:, 4) = Statein(nb)%vvl(ix,:)
           temp3d(i,j,:, 5) = Statein(nb)%prsi(ix,:)
           temp3d(i,j,:, 6) = Statein(nb)%prsl(ix,:)
           temp3d(i,j,:, 7) = Statein(nb)%prslk(ix,:)
           temp3d(i,j,:, 8) = Statein(nb)%prsik(ix,:)
           temp3d(i,j,:, 9) = Statein(nb)%phii(ix,:)
           temp3d(i,j,:,10) = Statein(nb)%phil(ix,:)
         endif

         temp3d(i,j,:,11) = Cld_props(nb)%fcice(ix,:)
         temp3d(i,j,:,12) = Cld_props(nb)%frain(ix,:)
         temp3d(i,j,:,13) = Cld_props(nb)%rrime(ix,:)
         temp3d(i,j,:,14) = Cld_props(nb)%deltaq(ix,:)
         temp3d(i,j,:,15) = Cld_props(nb)%cnvw(ix,:)
         temp3d(i,j,:,16) = Cld_props(nb)%cnvc(ix,:)
         temp3d(i,j,:,17) = Cld_props(nb)%cnvqc_v(ix,:)
         temp3d(i,j,:,18) = Cld_props(nb)%cldcov(ix,:)

         temp3d(i,j,:,19) = Rad_tends(nb)%htrsw(ix,:)
         temp3d(i,j,:,20) = Rad_tends(nb)%htrlw(ix,:)
         temp3d(i,j,:,21) = Rad_tends(nb)%dtdtr(ix,:)
         temp3d(i,j,:,22) = Rad_tends(nb)%swhc(ix,:)
         temp3d(i,j,:,23) = Rad_tends(nb)%hlwc(ix,:)

       enddo
     enddo
   enddo

   outunit = stdout()
   do i = 1, 83
     write (name, '(i2.2,3x,4a)') i, ' 2d '
     write(outunit,100) name, mpp_chksum(temp2d(:,:,i:i))
   enddo
   do i = 1, 23
     write (name, '(i2.2,3x,4a)') i, ' 3d '
     write(outunit,100) name, mpp_chksum(temp3d(:,:,:,i:i))
   enddo
100 format("CHECKSUM::",A32," = ",Z20)
   end subroutine checksum
!-------------------------------------------------------------------------      

end module gfs_physics_driver_mod
