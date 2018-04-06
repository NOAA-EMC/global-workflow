module FV3GFS_io_mod

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
!--- FMS/GFDL modules
  use block_control_mod,  only: block_control_type
  use mpp_mod,            only: mpp_error,  mpp_pe, mpp_root_pe, &
                                mpp_chksum, NOTE,   FATAL
  use fms_mod,            only: file_exist, stdout
  use fms_io_mod,         only: restart_file_type, free_restart_type, &
                                register_restart_field,               &
                                restore_state, save_restart
  use mpp_domains_mod,    only: domain1d, domain2d, domainUG
  use time_manager_mod,   only: time_type
  use diag_manager_mod,   only: register_diag_field, send_data
  use diag_axis_mod,      only: get_axis_global_length, get_diag_axis
  use diag_data_mod,      only: output_fields, max_output_fields
  use diag_util_mod,      only: find_input_field
  use constants_mod,      only: grav, rdgas
!
!--- GFS physics modules
!--- variables needed for calculating 'sncovr'
  use namelist_soilveg,   only: salp_data, snupx
!
!--- GFS_typedefs
  use GFS_typedefs,       only: GFS_sfcprop_type, GFS_diag_type, &
                                GFS_cldprop_type, GFS_grid_type
!
!--- IPD typdefs
  use IPD_typedefs,       only: IPD_control_type, IPD_data_type, &
                                IPD_restart_type, kind_phys
!
!-----------------------------------------------------------------------
  implicit none
  private
 
  !--- public interfaces ---
  public  FV3GFS_restart_read, FV3GFS_restart_write
  public  FV3GFS_IPD_checksum
  public  gfdl_diag_register, gfdl_diag_output
#ifdef use_WRTCOMP
  public  fv_phys_bundle_setup
#endif

  !--- GFDL filenames
  character(len=32)  :: fn_oro = 'oro_data.nc'
  character(len=32)  :: fn_srf = 'sfc_data.nc'
  character(len=32)  :: fn_phy = 'phy_data.nc'

  !--- GFDL FMS netcdf restart data types
  type(restart_file_type) :: Oro_restart
  type(restart_file_type) :: Sfc_restart
  type(restart_file_type) :: Phy_restart
 
  !--- GFDL FMS restart containers
  character(len=32),    allocatable,         dimension(:)       :: oro_name2, sfc_name2, sfc_name3
  real(kind=kind_phys), allocatable, target, dimension(:,:,:)   :: oro_var2, sfc_var2, phy_var2
  real(kind=kind_phys), allocatable, target, dimension(:,:,:,:) :: sfc_var3, phy_var3

!-RAB
  type data_subtype
    real(kind=kind_phys), dimension(:),   pointer :: var2  => NULL()
    real(kind=kind_phys), dimension(:),   pointer :: var21 => NULL()
    real(kind=kind_phys), dimension(:,:), pointer :: var3  => NULL()
  end type data_subtype
  !--- data type definition for use with GFDL FMS diagnostic manager until write component is working
  type gfdl_diag_type
    private
    integer :: id
    integer :: axes
    logical :: time_avg
    logical :: full_time_avg                       !no bucket
    character(len=64)    :: mod_name
    character(len=64)    :: name
    character(len=128)   :: desc
    character(len=64)    :: unit
    character(len=64)    :: mask
    character(len=64)    :: intpl_method
    character(len=128)   :: output_file
    real(kind=kind_phys) :: cnvfac
    type(data_subtype), dimension(:), allocatable :: data
!rab    real(kind=kind_phys), dimension(:),   pointer :: var2 => NULL()
!rab    real(kind=kind_phys), dimension(:),   pointer :: var21 => NULL()
   end type gfdl_diag_type
   real(kind=kind_phys) :: zhour
!
   integer :: tot_diag_idx = 0
   integer :: total_outputlevel = 0
   integer :: isco,ieco,jsco,jeco
   integer :: fhzero, ncld, nsoil, imp_physics
   real(4) :: dtp
   logical :: lprecip_accu
   character(len=64)  :: Sprecip_accu
   integer,dimension(:), allocatable :: nstt, nstt_vctbl
   real(4), dimension(:,:,:), allocatable, target   :: buffer_phys_bl
   real(4), dimension(:,:,:), allocatable, target   :: buffer_phys_nb
   real(4), dimension(:,:,:,:), allocatable, target :: buffer_phys_windvect
   real(kind=kind_phys),dimension(:,:),allocatable  :: lon
   real(kind=kind_phys),dimension(:,:),allocatable  :: lat
   real(kind=kind_phys),dimension(:,:),allocatable  :: uwork
   logical :: uwork_set = .false.
   character(128) :: uwindname
   integer, parameter :: DIAG_SIZE = 500
!   real(kind=kind_phys), parameter :: missing_value = 1.d30
   real(kind=kind_phys), parameter :: missing_value = 9.99e20
   real, parameter:: stndrd_atmos_ps = 101325.
   real, parameter:: stndrd_atmos_lapse = 0.0065
   type(gfdl_diag_type), dimension(DIAG_SIZE) :: Diag
!-RAB

 
!--- miscellaneous other variables
  logical :: use_wrtgridcomp_output = .FALSE.
  logical :: module_is_initialized  = .FALSE.

  CONTAINS

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!                     PUBLIC SUBROUTINES
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!--------------------
! FV3GFS_restart_read
!--------------------
  subroutine FV3GFS_restart_read (IPD_Data, IPD_Restart, Atm_block, Model, fv_domain)
    type(IPD_data_type),      intent(inout) :: IPD_Data(:)
    type(IPD_restart_type),   intent(inout) :: IPD_Restart
    type(block_control_type), intent(in)    :: Atm_block
    type(IPD_control_type),   intent(in)    :: Model
    type(domain2d),           intent(in)    :: fv_domain
 
    !--- read in surface data from chgres 
    call sfc_prop_restart_read (IPD_Data%Sfcprop, Atm_block, Model, fv_domain)
 
    !--- read in physics restart data
    call phys_restart_read (IPD_Restart, Atm_block, Model, fv_domain)

  end subroutine FV3GFS_restart_read

!---------------------
! FV3GFS_restart_write
!---------------------
  subroutine FV3GFS_restart_write (IPD_Data, IPD_Restart, Atm_block, Model, fv_domain, timestamp)
    type(IPD_data_type),         intent(inout) :: IPD_Data(:)
    type(IPD_restart_type),      intent(inout) :: IPD_Restart
    type(block_control_type),    intent(in)    :: Atm_block
    type(IPD_control_type),      intent(in)    :: Model
    type(domain2d),              intent(in)    :: fv_domain
    character(len=32), optional, intent(in)    :: timestamp
 
    !--- read in surface data from chgres 
    call sfc_prop_restart_write (IPD_Data%Sfcprop, Atm_block, Model, fv_domain, timestamp)
 
    !--- read in physics restart data
    call phys_restart_write (IPD_Restart, Atm_block, Model, fv_domain, timestamp)

  end subroutine FV3GFS_restart_write


!--------------------
! FV3GFS_IPD_checksum
!--------------------
 subroutine FV3GFS_IPD_checksum (Model, IPD_Data, Atm_block)
   !--- interface variables
   type(IPD_control_type),    intent(in) :: Model
   type(IPD_data_type),       intent(in) :: IPD_Data(:)
   type (block_control_type), intent(in) :: Atm_block
   !--- local variables
   integer :: outunit, j, i, ix, nb, isc, iec, jsc, jec, lev, ct, l, ntr
   real(kind=kind_phys), allocatable :: temp2d(:,:,:)
   real(kind=kind_phys), allocatable :: temp3d(:,:,:,:)
   character(len=32) :: name

   isc = Model%isc
   iec = Model%isc+Model%nx-1
   jsc = Model%jsc
   jec = Model%jsc+Model%ny-1
   lev = Model%levs

   ntr = size(IPD_Data(1)%Statein%qgrs,3)
   allocate (temp2d(isc:iec,jsc:jec,100+Model%ntot3d+Model%nctp))
   allocate (temp3d(isc:iec,jsc:jec,1:lev,17+Model%ntot3d+2*ntr))

   temp2d = 0.
   temp3d = 0.

   do j=jsc,jec
     do i=isc,iec
       nb = Atm_block%blkno(i,j) 
       ix = Atm_block%ixp(i,j) 
       !--- statein pressure
       temp2d(i,j, 1) = IPD_Data(nb)%Statein%pgr(ix)
       temp2d(i,j, 2) = IPD_Data(nb)%Sfcprop%slmsk(ix)
       temp2d(i,j, 3) = IPD_Data(nb)%Sfcprop%tsfc(ix)
       temp2d(i,j, 4) = IPD_Data(nb)%Sfcprop%tisfc(ix)
       temp2d(i,j, 5) = IPD_Data(nb)%Sfcprop%snowd(ix)
       temp2d(i,j, 6) = IPD_Data(nb)%Sfcprop%zorl(ix)
       temp2d(i,j, 7) = IPD_Data(nb)%Sfcprop%fice(ix)
       temp2d(i,j, 8) = IPD_Data(nb)%Sfcprop%hprim(ix)
       temp2d(i,j, 9) = IPD_Data(nb)%Sfcprop%sncovr(ix)
       temp2d(i,j,10) = IPD_Data(nb)%Sfcprop%snoalb(ix)
       temp2d(i,j,11) = IPD_Data(nb)%Sfcprop%alvsf(ix)
       temp2d(i,j,12) = IPD_Data(nb)%Sfcprop%alnsf(ix)
       temp2d(i,j,13) = IPD_Data(nb)%Sfcprop%alvwf(ix)
       temp2d(i,j,14) = IPD_Data(nb)%Sfcprop%alnwf(ix)
       temp2d(i,j,15) = IPD_Data(nb)%Sfcprop%facsf(ix)
       temp2d(i,j,16) = IPD_Data(nb)%Sfcprop%facwf(ix)
       temp2d(i,j,17) = IPD_Data(nb)%Sfcprop%slope(ix)
       temp2d(i,j,18) = IPD_Data(nb)%Sfcprop%shdmin(ix)
       temp2d(i,j,19) = IPD_Data(nb)%Sfcprop%shdmax(ix)
       temp2d(i,j,20) = IPD_Data(nb)%Sfcprop%tg3(ix)
       temp2d(i,j,21) = IPD_Data(nb)%Sfcprop%vfrac(ix)
       temp2d(i,j,22) = IPD_Data(nb)%Sfcprop%vtype(ix)
       temp2d(i,j,23) = IPD_Data(nb)%Sfcprop%stype(ix)
       temp2d(i,j,24) = IPD_Data(nb)%Sfcprop%uustar(ix)
       temp2d(i,j,25) = IPD_Data(nb)%Sfcprop%oro(ix)
       temp2d(i,j,26) = IPD_Data(nb)%Sfcprop%oro_uf(ix)
       temp2d(i,j,27) = IPD_Data(nb)%Sfcprop%hice(ix)
       temp2d(i,j,28) = IPD_Data(nb)%Sfcprop%weasd(ix)
       temp2d(i,j,29) = IPD_Data(nb)%Sfcprop%canopy(ix)
       temp2d(i,j,30) = IPD_Data(nb)%Sfcprop%ffmm(ix)
       temp2d(i,j,31) = IPD_Data(nb)%Sfcprop%ffhh(ix)
       temp2d(i,j,32) = IPD_Data(nb)%Sfcprop%f10m(ix)
       temp2d(i,j,33) = IPD_Data(nb)%Sfcprop%tprcp(ix)
       temp2d(i,j,34) = IPD_Data(nb)%Sfcprop%srflag(ix)
       temp2d(i,j,35) = IPD_Data(nb)%Sfcprop%slc(ix,1)
       temp2d(i,j,36) = IPD_Data(nb)%Sfcprop%slc(ix,2)
       temp2d(i,j,37) = IPD_Data(nb)%Sfcprop%slc(ix,3)
       temp2d(i,j,38) = IPD_Data(nb)%Sfcprop%slc(ix,4)
       temp2d(i,j,39) = IPD_Data(nb)%Sfcprop%smc(ix,1)
       temp2d(i,j,40) = IPD_Data(nb)%Sfcprop%smc(ix,2)
       temp2d(i,j,41) = IPD_Data(nb)%Sfcprop%smc(ix,3)
       temp2d(i,j,42) = IPD_Data(nb)%Sfcprop%smc(ix,4)
       temp2d(i,j,43) = IPD_Data(nb)%Sfcprop%stc(ix,1)
       temp2d(i,j,44) = IPD_Data(nb)%Sfcprop%stc(ix,2)
       temp2d(i,j,45) = IPD_Data(nb)%Sfcprop%stc(ix,3)
       temp2d(i,j,46) = IPD_Data(nb)%Sfcprop%stc(ix,4)
       temp2d(i,j,47) = IPD_Data(nb)%Sfcprop%t2m(ix)
       temp2d(i,j,48) = IPD_Data(nb)%Sfcprop%q2m(ix)
       temp2d(i,j,49) = IPD_Data(nb)%Coupling%nirbmdi(ix)
       temp2d(i,j,50) = IPD_Data(nb)%Coupling%nirdfdi(ix)
       temp2d(i,j,51) = IPD_Data(nb)%Coupling%visbmdi(ix)
       temp2d(i,j,52) = IPD_Data(nb)%Coupling%visdfdi(ix)
       temp2d(i,j,53) = IPD_Data(nb)%Coupling%nirbmui(ix)
       temp2d(i,j,54) = IPD_Data(nb)%Coupling%nirdfui(ix)
       temp2d(i,j,55) = IPD_Data(nb)%Coupling%visbmui(ix)
       temp2d(i,j,56) = IPD_Data(nb)%Coupling%visdfui(ix)
       temp2d(i,j,57) = IPD_Data(nb)%Coupling%sfcdsw(ix)
       temp2d(i,j,59) = IPD_Data(nb)%Coupling%sfcnsw(ix)
       temp2d(i,j,59) = IPD_Data(nb)%Coupling%sfcdlw(ix)
       temp2d(i,j,60) = IPD_Data(nb)%Grid%xlon(ix)
       temp2d(i,j,61) = IPD_Data(nb)%Grid%xlat(ix)
       temp2d(i,j,62) = IPD_Data(nb)%Grid%xlat_d(ix)
       temp2d(i,j,63) = IPD_Data(nb)%Grid%sinlat(ix)
       temp2d(i,j,64) = IPD_Data(nb)%Grid%coslat(ix)
       temp2d(i,j,65) = IPD_Data(nb)%Grid%area(ix)
       temp2d(i,j,66) = IPD_Data(nb)%Grid%dx(ix)
       if (Model%ntoz > 0) then
         temp2d(i,j,67) = IPD_Data(nb)%Grid%ddy_o3(ix)
       endif
       if (Model%h2o_phys) then
         temp2d(i,j,68) = IPD_Data(nb)%Grid%ddy_h(ix)
       endif
       temp2d(i,j,69) = IPD_Data(nb)%Cldprop%cv(ix)
       temp2d(i,j,70) = IPD_Data(nb)%Cldprop%cvt(ix)
       temp2d(i,j,71) = IPD_Data(nb)%Cldprop%cvb(ix)
       temp2d(i,j,72) = IPD_Data(nb)%Radtend%sfalb(ix)
       temp2d(i,j,73) = IPD_Data(nb)%Radtend%coszen(ix)
       temp2d(i,j,74) = IPD_Data(nb)%Radtend%tsflw(ix)
       temp2d(i,j,75) = IPD_Data(nb)%Radtend%semis(ix)
       temp2d(i,j,76) = IPD_Data(nb)%Radtend%coszdg(ix)
       temp2d(i,j,77) = IPD_Data(nb)%Radtend%sfcfsw(ix)%upfxc
       temp2d(i,j,78) = IPD_Data(nb)%Radtend%sfcfsw(ix)%upfx0
       temp2d(i,j,79) = IPD_Data(nb)%Radtend%sfcfsw(ix)%dnfxc
       temp2d(i,j,80) = IPD_Data(nb)%Radtend%sfcfsw(ix)%dnfx0
       temp2d(i,j,81) = IPD_Data(nb)%Radtend%sfcflw(ix)%upfxc
       temp2d(i,j,82) = IPD_Data(nb)%Radtend%sfcflw(ix)%upfx0
       temp2d(i,j,83) = IPD_Data(nb)%Radtend%sfcflw(ix)%dnfxc
       temp2d(i,j,84) = IPD_Data(nb)%Radtend%sfcflw(ix)%dnfx0
       if (Model%nstf_name(1) > 0) then
         temp2d(i,j,85) = IPD_Data(nb)%Sfcprop%tref(ix)
         temp2d(i,j,86) = IPD_Data(nb)%Sfcprop%z_c(ix)
         temp2d(i,j,87) = IPD_Data(nb)%Sfcprop%c_0(ix)
         temp2d(i,j,88) = IPD_Data(nb)%Sfcprop%c_d(ix)
         temp2d(i,j,89) = IPD_Data(nb)%Sfcprop%w_0(ix)
         temp2d(i,j,90) = IPD_Data(nb)%Sfcprop%w_d(ix)
         temp2d(i,j,91) = IPD_Data(nb)%Sfcprop%xt(ix)
         temp2d(i,j,92) = IPD_Data(nb)%Sfcprop%xs(ix)
         temp2d(i,j,93) = IPD_Data(nb)%Sfcprop%xu(ix)
         temp2d(i,j,94) = IPD_Data(nb)%Sfcprop%xz(ix)
         temp2d(i,j,95) = IPD_Data(nb)%Sfcprop%zm(ix)
         temp2d(i,j,96) = IPD_Data(nb)%Sfcprop%xtts(ix)
         temp2d(i,j,97) = IPD_Data(nb)%Sfcprop%xzts(ix)
         temp2d(i,j,98) = IPD_Data(nb)%Sfcprop%ifd(ix)
         temp2d(i,j,99) = IPD_Data(nb)%Sfcprop%dt_cool(ix)
         temp2d(i,j,100) = IPD_Data(nb)%Sfcprop%qrain(ix)
       endif

       do l = 1,Model%ntot2d
         temp2d(i,j,100+l) = IPD_Data(nb)%Tbd%phy_f2d(ix,l)
       enddo

       do l = 1,Model%nctp
         temp2d(i,j,100+Model%ntot2d+l) = IPD_Data(nb)%Tbd%phy_fctd(ix,l)
       enddo

       temp3d(i,j,:, 1) = IPD_Data(nb)%Statein%phii(ix,:)
       temp3d(i,j,:, 2) = IPD_Data(nb)%Statein%prsi(ix,:)
       temp3d(i,j,:, 3) = IPD_Data(nb)%Statein%prsik(ix,:)
       temp3d(i,j,:, 4) = IPD_Data(nb)%Statein%phil(ix,:)
       temp3d(i,j,:, 5) = IPD_Data(nb)%Statein%prsl(ix,:)
       temp3d(i,j,:, 6) = IPD_Data(nb)%Statein%prslk(ix,:)
       temp3d(i,j,:, 7) = IPD_Data(nb)%Statein%ugrs(ix,:)
       temp3d(i,j,:, 8) = IPD_Data(nb)%Statein%vgrs(ix,:)
       temp3d(i,j,:, 9) = IPD_Data(nb)%Statein%vvl(ix,:)
       temp3d(i,j,:,10) = IPD_Data(nb)%Statein%tgrs(ix,:)
       temp3d(i,j,:,11) = IPD_Data(nb)%Stateout%gu0(ix,:)
       temp3d(i,j,:,12) = IPD_Data(nb)%Stateout%gv0(ix,:)
       temp3d(i,j,:,13) = IPD_Data(nb)%Stateout%gt0(ix,:)
       temp3d(i,j,:,14) = IPD_Data(nb)%Radtend%htrsw(ix,:)
       temp3d(i,j,:,15) = IPD_Data(nb)%Radtend%htrlw(ix,:)
       temp3d(i,j,:,16) = IPD_Data(nb)%Radtend%swhc(ix,:)
       temp3d(i,j,:,17) = IPD_Data(nb)%Radtend%lwhc(ix,:)
       do l = 1,Model%ntot3d
         temp3d(i,j,:,17+l) = IPD_Data(nb)%Tbd%phy_f3d(ix,:,l)
       enddo
       do l = 1,ntr
         temp3d(i,j,:,17+Model%ntot3d+l)     = IPD_Data(nb)%Statein%qgrs(ix,:,l)
         temp3d(i,j,:,17+Model%ntot3d+ntr+l) = IPD_Data(nb)%Stateout%gq0(ix,:,l)
       enddo
     enddo
   enddo

   outunit = stdout()
   do i = 1,100+Model%ntot2d+Model%nctp
     write (name, '(i3.3,3x,4a)') i, ' 2d '
     write(outunit,100) name, mpp_chksum(temp2d(:,:,i:i))
   enddo
   do i = 1,17+Model%ntot3d+2*ntr
     write (name, '(i2.2,3x,4a)') i, ' 3d '
     write(outunit,100) name, mpp_chksum(temp3d(:,:,:,i:i))
   enddo
100 format("CHECKSUM::",A32," = ",Z20)

   end subroutine FV3GFS_IPD_checksum

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!                     PRIVATE SUBROUTINES
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!----------------------------------------------------------------------      
! sfc_prop_restart_read
!----------------------------------------------------------------------      
!    creates and populates a data type which is then used to "register"
!    restart variables with the GFDL FMS restart subsystem.
!    calls a GFDL FMS routine to restore the data from a restart file.
!    calculates sncovr if it is not present in the restart file.
!
!    calls:  register_restart_field, restart_state, free_restart
!   
!    opens:  oro_data.tile?.nc, sfc_data.tile?.nc
!   
!----------------------------------------------------------------------      
  subroutine sfc_prop_restart_read (Sfcprop, Atm_block, Model, fv_domain)
    !--- interface variable definitions
    type(GFS_sfcprop_type),    intent(inout) :: Sfcprop(:)
    type (block_control_type), intent(in)    :: Atm_block
    type(IPD_control_type),    intent(in)    :: Model
    type (domain2d),           intent(in)    :: fv_domain
    !--- local variables
    integer :: i, j, k, ix, lsoil, num, nb
    integer :: isc, iec, jsc, jec, npz, nx, ny
    integer :: id_restart
    integer :: nvar_o2, nvar_s2m, nvar_s2o, nvar_s3
    real(kind=kind_phys), pointer, dimension(:,:)   :: var2_p => NULL()
    real(kind=kind_phys), pointer, dimension(:,:,:) :: var3_p => NULL()
    !--- local variables for sncovr calculation
    integer :: vegtyp
    logical :: mand
    real(kind=kind_phys) :: rsnow
    
    nvar_o2  = 17
    nvar_s2m = 32
    nvar_s2o = 18
    nvar_s3  = 3

    isc = Atm_block%isc
    iec = Atm_block%iec
    jsc = Atm_block%jsc
    jec = Atm_block%jec
    npz = Atm_block%npz
    nx = (iec - isc + 1)
    ny = (jec - jsc + 1)
 
    !--- OROGRAPHY FILE
    if (.not. allocated(oro_name2)) then
    !--- allocate the various containers needed for orography data
      allocate(oro_name2(nvar_o2))
      allocate(oro_var2(nx,ny,nvar_o2))
      oro_var2 = -9999._kind_phys

      oro_name2(1)  = 'stddev'     ! hprim
      oro_name2(2)  = 'stddev'     ! hprime(ix,1)
      oro_name2(3)  = 'convexity'  ! hprime(ix,2)
      oro_name2(4)  = 'oa1'        ! hprime(ix,3)
      oro_name2(5)  = 'oa2'        ! hprime(ix,4)
      oro_name2(6)  = 'oa3'        ! hprime(ix,5)
      oro_name2(7)  = 'oa4'        ! hprime(ix,6)
      oro_name2(8)  = 'ol1'        ! hprime(ix,7)
      oro_name2(9)  = 'ol2'        ! hprime(ix,8)
      oro_name2(10) = 'ol3'        ! hprime(ix,9)
      oro_name2(11) = 'ol4'        ! hprime(ix,10)
      oro_name2(12) = 'theta'      ! hprime(ix,11)
      oro_name2(13) = 'gamma'      ! hprime(ix,12)
      oro_name2(14) = 'sigma'      ! hprime(ix,13)
      oro_name2(15) = 'elvmax'     ! hprime(ix,14)
      oro_name2(16) = 'orog_filt'  ! oro
      oro_name2(17) = 'orog_raw'   ! oro_uf
      !--- register the 2D fields
      do num = 1,nvar_o2
        var2_p => oro_var2(:,:,num)
        id_restart = register_restart_field(Oro_restart, fn_oro, oro_name2(num), var2_p, domain=fv_domain)
      enddo
      nullify(var2_p)
    endif

    !--- read the orography restart/data
    call mpp_error(NOTE,'reading topographic/orographic information from INPUT/oro_data.tile*.nc')
    call restore_state(Oro_restart)

    !--- copy data into GFS containers
    do nb = 1, Atm_block%nblks
      !--- 2D variables
      do ix = 1, Atm_block%blksz(nb)
        i = Atm_block%index(nb)%ii(ix) - isc + 1
        j = Atm_block%index(nb)%jj(ix) - jsc + 1
        !--- stddev
        Sfcprop(nb)%hprim(ix)      = oro_var2(i,j,1)
        !--- hprime(1:14)
        Sfcprop(nb)%hprime(ix,1)  = oro_var2(i,j,2)
        Sfcprop(nb)%hprime(ix,2)  = oro_var2(i,j,3)
        Sfcprop(nb)%hprime(ix,3)  = oro_var2(i,j,4)
        Sfcprop(nb)%hprime(ix,4)  = oro_var2(i,j,5)
        Sfcprop(nb)%hprime(ix,5)  = oro_var2(i,j,6)
        Sfcprop(nb)%hprime(ix,6)  = oro_var2(i,j,7)
        Sfcprop(nb)%hprime(ix,7)  = oro_var2(i,j,8)
        Sfcprop(nb)%hprime(ix,8)  = oro_var2(i,j,9)
        Sfcprop(nb)%hprime(ix,9)  = oro_var2(i,j,10)
        Sfcprop(nb)%hprime(ix,10) = oro_var2(i,j,11)
        Sfcprop(nb)%hprime(ix,11) = oro_var2(i,j,12)
        Sfcprop(nb)%hprime(ix,12) = oro_var2(i,j,13)
        Sfcprop(nb)%hprime(ix,13) = oro_var2(i,j,14)
        Sfcprop(nb)%hprime(ix,14) = oro_var2(i,j,15)
        !--- oro
        Sfcprop(nb)%oro(ix)        = oro_var2(i,j,16)
        !--- oro_uf
        Sfcprop(nb)%oro_uf(ix)     = oro_var2(i,j,17)
      enddo
    enddo
 
    !--- deallocate containers and free restart container
    deallocate(oro_name2, oro_var2)
    call free_restart_type(Oro_restart)
 
    !--- SURFACE FILE
    if (.not. allocated(sfc_name2)) then
      !--- allocate the various containers needed for restarts
      allocate(sfc_name2(nvar_s2m+nvar_s2o))
      allocate(sfc_name3(nvar_s3))
      allocate(sfc_var2(nx,ny,nvar_s2m+nvar_s2o))
      allocate(sfc_var3(nx,ny,Model%lsoil,nvar_s3))
      sfc_var2 = -9999._kind_phys
      sfc_var3 = -9999._kind_phys
 
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
      !--- below here all variables are optional
      sfc_name2(32) = 'sncovr'
      !--- NSSTM inputs only needed when (nstf_name(1) > 0) .and. (nstf_name(2)) == 0) 
      sfc_name2(33) = 'tref'
      sfc_name2(34) = 'z_c'
      sfc_name2(35) = 'c_0'
      sfc_name2(36) = 'c_d'
      sfc_name2(37) = 'w_0'
      sfc_name2(38) = 'w_d'
      sfc_name2(39) = 'xt'
      sfc_name2(40) = 'xs'
      sfc_name2(41) = 'xu'
      sfc_name2(42) = 'xv'
      sfc_name2(43) = 'xz'
      sfc_name2(44) = 'zm'
      sfc_name2(45) = 'xtts'
      sfc_name2(46) = 'xzts'
      sfc_name2(47) = 'd_conv'
      sfc_name2(48) = 'ifd'
      sfc_name2(49) = 'dt_cool'
      sfc_name2(50) = 'qrain'
 
      !--- register the 2D fields
      do num = 1,nvar_s2m
        var2_p => sfc_var2(:,:,num)
        if (trim(sfc_name2(num)) == 'sncovr') then
          id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name2(num), var2_p, domain=fv_domain, mandatory=.false.)
        else
          id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name2(num), var2_p, domain=fv_domain)
        endif
      enddo
      if (Model%nstf_name(1) > 0) then
        mand = .false.
        if (Model%nstf_name(2) == 0) mand = .true.
        do num = nvar_s2m+1,nvar_s2m+nvar_s2o
          var2_p => sfc_var2(:,:,num)
          id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name2(num), var2_p, domain=fv_domain, mandatory=mand)
        enddo
      endif
      nullify(var2_p)
 
      !--- names of the 2D variables to save
      sfc_name3(1) = 'stc'
      sfc_name3(2) = 'smc'
      sfc_name3(3) = 'slc'
 
      !--- register the 3D fields
      do num = 1,nvar_s3
        var3_p => sfc_var3(:,:,:,num)
        id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name3(num), var3_p, domain=fv_domain)
      enddo
      nullify(var3_p)
    endif
 
    !--- read the surface restart/data
    call mpp_error(NOTE,'reading surface properties data from INPUT/sfc_data.tile*.nc')
    call restore_state(Sfc_restart)
 
    !--- place the data into the block GFS containers
    do nb = 1, Atm_block%nblks
      do ix = 1, Atm_block%blksz(nb)
        i = Atm_block%index(nb)%ii(ix) - isc + 1
        j = Atm_block%index(nb)%jj(ix) - jsc + 1
        !--- 2D variables
        !--- slmsk
        Sfcprop(nb)%slmsk(ix)  = sfc_var2(i,j,1)
        !--- tsfc (tsea in sfc file)
        Sfcprop(nb)%tsfc(ix)   = sfc_var2(i,j,2)
        !--- weasd (sheleg in sfc file)
        Sfcprop(nb)%weasd(ix)  = sfc_var2(i,j,3)
        !--- tg3
        Sfcprop(nb)%tg3(ix)    = sfc_var2(i,j,4)
        !--- zorl
        Sfcprop(nb)%zorl(ix)   = sfc_var2(i,j,5)
        !--- alvsf
        Sfcprop(nb)%alvsf(ix)  = sfc_var2(i,j,6)
        !--- alvwf
        Sfcprop(nb)%alvwf(ix)  = sfc_var2(i,j,7)
        !--- alnsf
        Sfcprop(nb)%alnsf(ix)  = sfc_var2(i,j,8)
        !--- alnwf
        Sfcprop(nb)%alnwf(ix)  = sfc_var2(i,j,9)
        !--- facsf
        Sfcprop(nb)%facsf(ix)  = sfc_var2(i,j,10)
        !--- facwf
        Sfcprop(nb)%facwf(ix)  = sfc_var2(i,j,11)
        !--- vfrac
        Sfcprop(nb)%vfrac(ix)  = sfc_var2(i,j,12)
        !--- canopy
        Sfcprop(nb)%canopy(ix) = sfc_var2(i,j,13)
        !--- f10m
        Sfcprop(nb)%f10m(ix)   = sfc_var2(i,j,14)
        !--- t2m
        Sfcprop(nb)%t2m(ix)    = sfc_var2(i,j,15)
        !--- q2m
        Sfcprop(nb)%q2m(ix)    = sfc_var2(i,j,16)
        !--- vtype
        Sfcprop(nb)%vtype(ix)  = sfc_var2(i,j,17)
        !--- stype
        Sfcprop(nb)%stype(ix)  = sfc_var2(i,j,18)
        !--- uustar
        Sfcprop(nb)%uustar(ix) = sfc_var2(i,j,19)
        !--- ffmm
        Sfcprop(nb)%ffmm(ix)   = sfc_var2(i,j,20)
        !--- ffhh
        Sfcprop(nb)%ffhh(ix)   = sfc_var2(i,j,21)
        !--- hice
        Sfcprop(nb)%hice(ix)   = sfc_var2(i,j,22)
        !--- fice
        Sfcprop(nb)%fice(ix)   = sfc_var2(i,j,23)
        !--- tisfc
        Sfcprop(nb)%tisfc(ix)  = sfc_var2(i,j,24)
        !--- tprcp
        Sfcprop(nb)%tprcp(ix)  = sfc_var2(i,j,25)
        !--- srflag
        Sfcprop(nb)%srflag(ix) = sfc_var2(i,j,26)
        !--- snowd (snwdph in the file)
        Sfcprop(nb)%snowd(ix)  = sfc_var2(i,j,27)
        !--- shdmin
        Sfcprop(nb)%shdmin(ix) = sfc_var2(i,j,28)
        !--- shdmax
        Sfcprop(nb)%shdmax(ix) = sfc_var2(i,j,29)
        !--- slope
        Sfcprop(nb)%slope(ix)  = sfc_var2(i,j,30)
        !--- snoalb
        Sfcprop(nb)%snoalb(ix) = sfc_var2(i,j,31)
        !--- sncovr
        Sfcprop(nb)%sncovr(ix) = sfc_var2(i,j,32)
        !
        !--- NSSTM variables
        if ((Model%nstf_name(1) > 0) .and. (Model%nstf_name(2) == 1)) then
          !--- nsstm tref
          Sfcprop(nb)%tref(ix)    = Sfcprop(nb)%tsfc(ix)
          Sfcprop(nb)%xz(ix)      = 30.0d0
        endif
        if ((Model%nstf_name(1) > 0) .and. (Model%nstf_name(2) == 0)) then
          !--- nsstm tref
          Sfcprop(nb)%tref(ix)    = sfc_var2(i,j,33)
          !--- nsstm z_c
          Sfcprop(nb)%z_c(ix)     = sfc_var2(i,j,34)
          !--- nsstm c_0
          Sfcprop(nb)%c_0(ix)     = sfc_var2(i,j,35)
          !--- nsstm c_d
          Sfcprop(nb)%c_d(ix)     = sfc_var2(i,j,36)
          !--- nsstm w_0
          Sfcprop(nb)%w_0(ix)     = sfc_var2(i,j,37)
          !--- nsstm w_d
          Sfcprop(nb)%w_d(ix)     = sfc_var2(i,j,38)
          !--- nsstm xt
          Sfcprop(nb)%xt(ix)      = sfc_var2(i,j,39)
          !--- nsstm xs
          Sfcprop(nb)%xs(ix)      = sfc_var2(i,j,40)
          !--- nsstm xu
          Sfcprop(nb)%xu(ix)      = sfc_var2(i,j,41)
          !--- nsstm xv
          Sfcprop(nb)%xv(ix)      = sfc_var2(i,j,42)
          !--- nsstm xz
          Sfcprop(nb)%xz(ix)      = sfc_var2(i,j,43)
          !--- nsstm zm
          Sfcprop(nb)%zm(ix)      = sfc_var2(i,j,44)
          !--- nsstm xtts
          Sfcprop(nb)%xtts(ix)    = sfc_var2(i,j,45)
          !--- nsstm xzts
          Sfcprop(nb)%xzts(ix)    = sfc_var2(i,j,46)
          !--- nsstm d_conv
          Sfcprop(nb)%d_conv(ix)  = sfc_var2(i,j,47)
          !--- nsstm ifd
          Sfcprop(nb)%ifd(ix)     = sfc_var2(i,j,48)
          !--- nsstm dt_cool
          Sfcprop(nb)%dt_cool(ix) = sfc_var2(i,j,49)
          !--- nsstm qrain
          Sfcprop(nb)%qrain(ix)   = sfc_var2(i,j,50)
        endif

        !--- 3D variables
        do lsoil = 1,Model%lsoil
            !--- stc
            Sfcprop(nb)%stc(ix,lsoil) = sfc_var3(i,j,lsoil,1)
            !--- smc
            Sfcprop(nb)%smc(ix,lsoil) = sfc_var3(i,j,lsoil,2)
            !--- slc
            Sfcprop(nb)%slc(ix,lsoil) = sfc_var3(i,j,lsoil,3)
        enddo
      enddo
    enddo

    !--- if sncovr does not exist in the restart, need to create it
    if (nint(sfc_var2(1,1,32)) == -9999) then
      if (Model%me == Model%master ) call mpp_error(NOTE, 'gfs_driver::surface_props_input - computing sncovr') 
      !--- compute sncovr from existing variables
      !--- code taken directly from read_fix.f
      do nb = 1, Atm_block%nblks
        do ix = 1, Atm_block%blksz(nb)
          Sfcprop(nb)%sncovr(ix) = 0.0
          if (Sfcprop(nb)%slmsk(ix) > 0.001) then
            vegtyp = Sfcprop(nb)%vtype(ix)
            if (vegtyp == 0) vegtyp = 7
            rsnow  = 0.001*Sfcprop(nb)%weasd(ix)/snupx(vegtyp)
            if (0.001*Sfcprop(nb)%weasd(ix) < snupx(vegtyp)) then
              Sfcprop(nb)%sncovr(ix) = 1.0 - (exp(-salp_data*rsnow) - rsnow*exp(-salp_data))
            else
              Sfcprop(nb)%sncovr(ix) = 1.0
            endif
          endif
        enddo
      enddo
    endif

  end subroutine sfc_prop_restart_read


!----------------------------------------------------------------------      
! sfc_prop_restart_write
!----------------------------------------------------------------------      
!    routine to write out GFS surface restarts via the GFDL FMS restart
!    subsystem.
!    takes an optional argument to append timestamps for intermediate 
!    restarts.
!
!    calls:  register_restart_field, save_restart
!----------------------------------------------------------------------      
  subroutine sfc_prop_restart_write (Sfcprop, Atm_block, Model, fv_domain, timestamp)
    !--- interface variable definitions
    type(GFS_sfcprop_type),      intent(in) :: Sfcprop(:)
    type(block_control_type),    intent(in) :: Atm_block
    type(IPD_control_type),      intent(in) :: Model
    type(domain2d),              intent(in) :: fv_domain
    character(len=32), optional, intent(in) :: timestamp
    !--- local variables
    integer :: i, j, k, nb, ix, lsoil, num
    integer :: isc, iec, jsc, jec, npz, nx, ny
    integer :: id_restart
    integer :: nvar2m, nvar2o, nvar3
    logical :: mand
    character(len=32) :: fn_srf = 'sfc_data.nc'
    real(kind=kind_phys), pointer, dimension(:,:)   :: var2_p => NULL()
    real(kind=kind_phys), pointer, dimension(:,:,:) :: var3_p => NULL()

    nvar2m = 32
    nvar2o = 18
    nvar3  = 3

    isc = Atm_block%isc
    iec = Atm_block%iec
    jsc = Atm_block%jsc
    jec = Atm_block%jec
    npz = Atm_block%npz
    nx = (iec - isc + 1)
    ny = (jec - jsc + 1)

    if (.not. allocated(sfc_name2)) then
      !--- allocate the various containers needed for restarts
      allocate(sfc_name2(nvar2m+nvar2o))
      allocate(sfc_name3(nvar3))
      allocate(sfc_var2(nx,ny,nvar2m+nvar2o))
      allocate(sfc_var3(nx,ny,Model%lsoil,nvar3))
      sfc_var2 = -9999._kind_phys
      sfc_var3 = -9999._kind_phys

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
      !--- below here all variables are optional
      sfc_name2(32) = 'sncovr'
      !--- NSSTM inputs only needed when (nstf_name(1) > 0) .and. (nstf_name(2)) == 0)
      sfc_name2(33) = 'tref'
      sfc_name2(34) = 'z_c'
      sfc_name2(35) = 'c_0'
      sfc_name2(36) = 'c_d'
      sfc_name2(37) = 'w_0'
      sfc_name2(38) = 'w_d'
      sfc_name2(39) = 'xt'
      sfc_name2(40) = 'xs'
      sfc_name2(41) = 'xu'
      sfc_name2(42) = 'xv'
      sfc_name2(43) = 'xz'
      sfc_name2(44) = 'zm'
      sfc_name2(45) = 'xtts'
      sfc_name2(46) = 'xzts'
      sfc_name2(47) = 'd_conv'
      sfc_name2(48) = 'ifd'
      sfc_name2(49) = 'dt_cool'
      sfc_name2(50) = 'qrain'
 
      !--- register the 2D fields
      do num = 1,nvar2m
        var2_p => sfc_var2(:,:,num)
        if (trim(sfc_name2(num)) == 'sncovr') then
          id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name2(num), var2_p, domain=fv_domain, mandatory=.false.)
        else
          id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name2(num), var2_p, domain=fv_domain)
        endif
      enddo
      if (Model%nstf_name(1) > 0) then
        mand = .false.
        if (Model%nstf_name(2) ==0) mand = .true.
        do num = nvar2m+1,nvar2m+nvar2o
          var2_p => sfc_var2(:,:,num)
          id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name2(num), var2_p, domain=fv_domain, mandatory=mand)
        enddo
      endif
      nullify(var2_p)
 
      !--- names of the 2D variables to save
      sfc_name3(1) = 'stc'
      sfc_name3(2) = 'smc'
      sfc_name3(3) = 'slc'
 
      !--- register the 3D fields
      do num = 1,nvar3
        var3_p => sfc_var3(:,:,:,num)
        id_restart = register_restart_field(Sfc_restart, fn_srf, sfc_name3(num), var3_p, domain=fv_domain)
      enddo
      nullify(var3_p)
    endif
   
    do nb = 1, Atm_block%nblks
      do ix = 1, Atm_block%blksz(nb)
        !--- 2D variables
        i = Atm_block%index(nb)%ii(ix) - isc + 1
        j = Atm_block%index(nb)%jj(ix) - jsc + 1
        !--- slmsk
        sfc_var2(i,j,1)  = Sfcprop(nb)%slmsk(ix)
        !--- tsfc (tsea in sfc file)
        sfc_var2(i,j,2)  = Sfcprop(nb)%tsfc(ix)
        !--- weasd (sheleg in sfc file)
        sfc_var2(i,j,3)  = Sfcprop(nb)%weasd(ix)
        !--- tg3
        sfc_var2(i,j,4)  = Sfcprop(nb)%tg3(ix)
        !--- zorl
        sfc_var2(i,j,5)  = Sfcprop(nb)%zorl(ix)
        !--- alvsf
        sfc_var2(i,j,6)  = Sfcprop(nb)%alvsf(ix)
        !--- alvwf
        sfc_var2(i,j,7)  = Sfcprop(nb)%alvwf(ix)
        !--- alnsf
        sfc_var2(i,j,8)  = Sfcprop(nb)%alnsf(ix)
        !--- alnwf
        sfc_var2(i,j,9)  = Sfcprop(nb)%alnwf(ix)
        !--- facsf
        sfc_var2(i,j,10) = Sfcprop(nb)%facsf(ix)
        !--- facwf
        sfc_var2(i,j,11) = Sfcprop(nb)%facwf(ix)
        !--- vfrac
        sfc_var2(i,j,12) = Sfcprop(nb)%vfrac(ix)
        !--- canopy
        sfc_var2(i,j,13) = Sfcprop(nb)%canopy(ix)
        !--- f10m
        sfc_var2(i,j,14) = Sfcprop(nb)%f10m(ix)
        !--- t2m
        sfc_var2(i,j,15) = Sfcprop(nb)%t2m(ix)
        !--- q2m
        sfc_var2(i,j,16) = Sfcprop(nb)%q2m(ix)
        !--- vtype
        sfc_var2(i,j,17) = Sfcprop(nb)%vtype(ix)
        !--- stype
        sfc_var2(i,j,18) = Sfcprop(nb)%stype(ix)
        !--- uustar
        sfc_var2(i,j,19) = Sfcprop(nb)%uustar(ix)
        !--- ffmm
        sfc_var2(i,j,20) = Sfcprop(nb)%ffmm(ix)
        !--- ffhh
        sfc_var2(i,j,21) = Sfcprop(nb)%ffhh(ix)
        !--- hice
        sfc_var2(i,j,22) = Sfcprop(nb)%hice(ix)
        !--- fice
        sfc_var2(i,j,23) = Sfcprop(nb)%fice(ix)
        !--- tisfc
        sfc_var2(i,j,24) = Sfcprop(nb)%tisfc(ix)
        !--- tprcp
        sfc_var2(i,j,25) = Sfcprop(nb)%tprcp(ix)
        !--- srflag
        sfc_var2(i,j,26) = Sfcprop(nb)%srflag(ix)
        !--- snowd (snwdph in the file)
        sfc_var2(i,j,27) = Sfcprop(nb)%snowd(ix)
        !--- shdmin
        sfc_var2(i,j,28) = Sfcprop(nb)%shdmin(ix)
        !--- shdmax
        sfc_var2(i,j,29) = Sfcprop(nb)%shdmax(ix)
        !--- slope
        sfc_var2(i,j,30) = Sfcprop(nb)%slope(ix)
        !--- snoalb
        sfc_var2(i,j,31) = Sfcprop(nb)%snoalb(ix)
        !--- sncovr
        sfc_var2(i,j,32) = Sfcprop(nb)%sncovr(ix)
        !--- NSSTM variables
        if (Model%nstf_name(1) > 0) then
          !--- nsstm tref
          sfc_var2(i,j,33) = Sfcprop(nb)%tref(ix)
          !--- nsstm z_c
          sfc_var2(i,j,34) = Sfcprop(nb)%z_c(ix)
          !--- nsstm c_0
          sfc_var2(i,j,35) = Sfcprop(nb)%c_0(ix)
          !--- nsstm c_d
          sfc_var2(i,j,36) = Sfcprop(nb)%c_d(ix)
          !--- nsstm w_0
          sfc_var2(i,j,37) = Sfcprop(nb)%w_0(ix)
          !--- nsstm w_d
          sfc_var2(i,j,38) = Sfcprop(nb)%w_d(ix)
          !--- nsstm xt
          sfc_var2(i,j,39) = Sfcprop(nb)%xt(ix)
          !--- nsstm xs
          sfc_var2(i,j,40) = Sfcprop(nb)%xs(ix)
          !--- nsstm xu
          sfc_var2(i,j,41) = Sfcprop(nb)%xu(ix)
          !--- nsstm xv
          sfc_var2(i,j,42) = Sfcprop(nb)%xv(ix)
          !--- nsstm xz
          sfc_var2(i,j,43) = Sfcprop(nb)%xz(ix)
          !--- nsstm zm
          sfc_var2(i,j,44) = Sfcprop(nb)%zm(ix)
          !--- nsstm xtts
          sfc_var2(i,j,45) = Sfcprop(nb)%xtts(ix)
          !--- nsstm xzts
          sfc_var2(i,j,46) = Sfcprop(nb)%xzts(ix)
          !--- nsstm d_conv
          sfc_var2(i,j,47) = Sfcprop(nb)%d_conv(ix)
          !--- nsstm ifd
          sfc_var2(i,j,48) = Sfcprop(nb)%ifd(ix)
          !--- nsstm dt_cool
          sfc_var2(i,j,49) = Sfcprop(nb)%dt_cool(ix)
          !--- nsstm qrain
          sfc_var2(i,j,50) = Sfcprop(nb)%qrain(ix)
        endif
 
        !--- 3D variables
        do lsoil = 1,Model%lsoil
          !--- stc
          sfc_var3(i,j,lsoil,1) = Sfcprop(nb)%stc(ix,lsoil)
          !--- smc
          sfc_var3(i,j,lsoil,2) = Sfcprop(nb)%smc(ix,lsoil)
          !--- slc
          sfc_var3(i,j,lsoil,3) = Sfcprop(nb)%slc(ix,lsoil)
        enddo
      enddo
    enddo

    call save_restart(Sfc_restart, timestamp)

  end subroutine sfc_prop_restart_write


!----------------------------------------------------------------------      
! phys_restart_read
!----------------------------------------------------------------------      
!    creates and populates a data type which is then used to "register"
!    restart variables with the GFDL FMS restart subsystem.
!    calls a GFDL FMS routine to restore the data from a restart file.
!    calculates sncovr if it is not present in the restart file.
!
!    calls:  register_restart_field, restart_state, free_restart
!   
!    opens:  phys_data.tile?.nc
!   
!----------------------------------------------------------------------      
  subroutine phys_restart_read (IPD_Restart, Atm_block, Model, fv_domain)
    !--- interface variable definitions
    type(IPD_restart_type),      intent(in) :: IPD_Restart
    type(block_control_type),    intent(in) :: Atm_block
    type(IPD_control_type),      intent(in) :: Model
    type(domain2d),              intent(in) :: fv_domain
    !--- local variables
    integer :: i, j, k, nb, ix, num
    integer :: isc, iec, jsc, jec, npz, nx, ny
    integer :: id_restart
    integer :: nvar2d, nvar3d
    character(len=64) :: fname
    real(kind=kind_phys), pointer, dimension(:,:)   :: var2_p => NULL()
    real(kind=kind_phys), pointer, dimension(:,:,:) :: var3_p => NULL()


    isc = Atm_block%isc
    iec = Atm_block%iec
    jsc = Atm_block%jsc
    jec = Atm_block%jec
    npz = Atm_block%npz
    nx = (iec - isc + 1)
    ny = (jec - jsc + 1)
    nvar2d = IPD_Restart%num2d
    nvar3d = IPD_Restart%num3d
 
    !--- register the restart fields
    if (.not. allocated(phy_var2)) then
      allocate (phy_var2(nx,ny,nvar2d))
      allocate (phy_var3(nx,ny,npz,nvar3d))
      phy_var2 = 0.0_kind_phys
      phy_var3 = 0.0_kind_phys
      
      do num = 1,nvar2d
        var2_p => phy_var2(:,:,num)
        id_restart = register_restart_field (Phy_restart, fn_phy, trim(IPD_Restart%name2d(num)), &
                                             var2_p, domain=fv_domain, mandatory=.false.)
      enddo
      do num = 1,nvar3d
        var3_p => phy_var3(:,:,:,num)
        id_restart = register_restart_field (Phy_restart, fn_phy, trim(IPD_restart%name3d(num)), &
                                             var3_p, domain=fv_domain, mandatory=.false.)
      enddo
      nullify(var2_p)
      nullify(var3_p)
    endif

    fname = 'INPUT/'//trim(fn_phy)
    if (file_exist(fname)) then
      !--- read the surface restart/data
      call mpp_error(NOTE,'reading physics restart data from INPUT/phy_data.tile*.nc')
      call restore_state(Phy_restart)
    else
      call mpp_error(NOTE,'No physics restarts - cold starting physical parameterizations')
      return
    endif
 
    !--- place the data into the block GFS containers
    !--- phy_var* variables
    do num = 1,nvar2d
      do nb = 1,Atm_block%nblks
        do ix = 1, Atm_block%blksz(nb)            
          i = Atm_block%index(nb)%ii(ix) - isc + 1
          j = Atm_block%index(nb)%jj(ix) - jsc + 1
          IPD_Restart%data(nb,num)%var2p(ix) = phy_var2(i,j,num)
        enddo
      enddo
    enddo
    do num = 1,nvar3d
      do nb = 1,Atm_block%nblks
        do k=1,npz
          do ix = 1, Atm_block%blksz(nb)            
            i = Atm_block%index(nb)%ii(ix) - isc + 1
            j = Atm_block%index(nb)%jj(ix) - jsc + 1
            IPD_Restart%data(nb,num)%var3p(ix,k) = phy_var3(i,j,k,num)
          enddo
        enddo
      enddo
    enddo

  end subroutine phys_restart_read


!----------------------------------------------------------------------      
! phys_restart_write
!----------------------------------------------------------------------      
!    routine to write out GFS surface restarts via the GFDL FMS restart
!    subsystem.
!    takes an optional argument to append timestamps for intermediate 
!    restarts.
!
!    calls:  register_restart_field, save_restart
!----------------------------------------------------------------------      
  subroutine phys_restart_write (IPD_Restart, Atm_block, Model, fv_domain, timestamp)
    !--- interface variable definitions
    type(IPD_restart_type),      intent(in) :: IPD_Restart
    type(block_control_type),    intent(in) :: Atm_block
    type(IPD_control_type),      intent(in) :: Model
    type(domain2d),              intent(in) :: fv_domain
    character(len=32), optional, intent(in) :: timestamp
    !--- local variables
    integer :: i, j, k, nb, ix, num
    integer :: isc, iec, jsc, jec, npz, nx, ny
    integer :: id_restart
    integer :: nvar2d, nvar3d
    real(kind=kind_phys), pointer, dimension(:,:)   :: var2_p => NULL()
    real(kind=kind_phys), pointer, dimension(:,:,:) :: var3_p => NULL()


    isc = Atm_block%isc
    iec = Atm_block%iec
    jsc = Atm_block%jsc
    jec = Atm_block%jec
    npz = Atm_block%npz
    nx = (iec - isc + 1)
    ny = (jec - jsc + 1)
    nvar2d = IPD_Restart%num2d
    nvar3d = IPD_Restart%num3d

    !--- register the restart fields 
    if (.not. allocated(phy_var2)) then
      allocate (phy_var2(nx,ny,nvar2d))
      allocate (phy_var3(nx,ny,npz,nvar3d))
      phy_var2 = 0.0_kind_phys
      phy_var3 = 0.0_kind_phys
      
      do num = 1,nvar2d
        var2_p => phy_var2(:,:,num)
        id_restart = register_restart_field (Phy_restart, fn_phy, trim(IPD_Restart%name2d(num)), &
                                             var2_p, domain=fv_domain, mandatory=.false.)
      enddo
      do num = 1,nvar3d
        var3_p => phy_var3(:,:,:,num)
        id_restart = register_restart_field (Phy_restart, fn_phy, trim(IPD_restart%name3d(num)), &
                                             var3_p, domain=fv_domain, mandatory=.false.)
      enddo
      nullify(var2_p)
      nullify(var3_p)
    endif

    !--- 2D variables
    do num = 1,nvar2d
      do nb = 1,Atm_block%nblks
        do ix = 1, Atm_block%blksz(nb)            
          i = Atm_block%index(nb)%ii(ix) - isc + 1
          j = Atm_block%index(nb)%jj(ix) - jsc + 1
          phy_var2(i,j,num) = IPD_Restart%data(nb,num)%var2p(ix)
        enddo
      enddo
    enddo
    !--- 3D variables
    do num = 1,nvar3d
      do nb = 1,Atm_block%nblks
        do k=1,npz
          do ix = 1, Atm_block%blksz(nb)            
            i = Atm_block%index(nb)%ii(ix) - isc + 1
            j = Atm_block%index(nb)%jj(ix) - jsc + 1
            phy_var3(i,j,k,num) = IPD_Restart%data(nb,num)%var3p(ix,k)
          enddo
        enddo
      enddo
    enddo

    call save_restart(Phy_restart, timestamp)

  end subroutine phys_restart_write

!-------------------------------------------------------------------------      
!--- gfdl_diag_register ---
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
!    13+NFXR - radiation, 16 nsst
!    76+pl_coeff - physics
!-------------------------------------------------------------------------      
  subroutine gfdl_diag_register(Time, Sfcprop, Cldprop, Gfs_diag, Gfs_grid, Atm_block, Model, axes)
    use physcons,  only: con_g
!--- subroutine interface variable definitions
    type(time_type),           intent(in) :: Time
    type(GFS_sfcprop_type),    intent(in) :: Sfcprop(:)
    type(GFS_cldprop_type),    intent(in) :: Cldprop(:)
    type(GFS_diag_type),       intent(in) :: Gfs_diag(:)
    type(GFS_grid_type),       intent(in) :: Gfs_grid(:)
    type (block_control_type), intent(in) :: Atm_block
    type(IPD_control_type),    intent(in) :: Model
    integer, dimension(4),     intent(in) :: axes
!--- local variables
    integer :: i, j, ix
    integer :: idx, num, nb, nblks, nx, ny, k, nrgst_bl, nrgst_nb, nrgst_vctbl, NFXR
    integer, allocatable :: blksz(:)
    character(len=2) :: xtra
    real(kind=kind_phys), parameter :: cn_one = 1._kind_phys
    real(kind=kind_phys), parameter :: cn_100 = 100._kind_phys
    real(kind=kind_phys), parameter :: cn_th  = 1000._kind_phys
    real(kind=kind_phys), parameter :: cn_hr  = 3600._kind_phys

    NFXR = Model%NFXR
    nblks = Atm_block%nblks
    allocate (blksz(nblks))
    blksz(:) = Atm_block%blksz(:)

    isco = Atm_block%isc
    ieco = Atm_block%iec
    jsco = Atm_block%jsc
    jeco = Atm_block%jec
    fhzero = nint(Model%fhzero)
    ncld   = Model%ncld
    nsoil  = Model%lsoil
    dtp    = Model%dtp
    imp_physics  = Model%imp_physics
    lprecip_accu = Model%lprecip_accu
    if( lprecip_accu ) then
      Sprecip_accu = "yes" 
    else
      Sprecip_accu = "no" 
    endif
!    print *,'in gfdl_diag_register,ncld=',Model%ncld,Model%lsoil,Model%imp_physics, &
!      'lprecip_accu=', lprecip_accu,' dtp=',dtp
!
!save lon/lat for vector interpolation
    allocate(lon(isco:ieco,jsco:jeco))
    allocate(lat(isco:ieco,jsco:jeco))
    do j=jsco,jeco
      do i=isco,ieco
        nb = Atm_block%blkno(i,j)
        ix = Atm_block%ixp(i,j)
        lon(i,j) = Gfs_grid(nb)%xlon(ix)
        lat(i,j) = Gfs_grid(nb)%xlat(ix)
      enddo
    enddo

    Diag(:)%id = -99
    Diag(:)%axes = -99
    Diag(:)%cnvfac = 1.0_kind_phys
    Diag(:)%time_avg = .FALSE.
    Diag(:)%full_time_avg = .FALSE.
    Diag(:)%mask = ''
    Diag(:)%intpl_method = 'nearest_stod'
    Diag(:)%output_file = ''

    idx = 0 

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ALBDO_ave'
    Diag(idx)%desc = 'surface albedo'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_100
    Diag(idx)%mask = 'positive_flux'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2  => Gfs_diag(nb)%fluxr(:,3)
      Diag(idx)%data(nb)%var21 => Gfs_diag(nb)%fluxr(:,4)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'DLWRF'
    Diag(idx)%desc = 'surface downward longwave flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dlwsfc(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'DLWRFI'
    Diag(idx)%desc = 'instantaneous surface downward longwave flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dlwsfci(:)
    enddo


    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ULWRF'
    Diag(idx)%desc = 'surface upward longwave flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%ulwsfc(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ULWRFI'
    Diag(idx)%desc = 'instantaneous surface upward longwave flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%ulwsfci(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'DSWRF'
    Diag(idx)%desc = 'averaged surface downward shortwave flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,4)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'DSWRFI'
    Diag(idx)%desc = 'instantaneous surface downward shortwave flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dswsfci(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'USWRF'
    Diag(idx)%desc = 'averaged surface upward shortwave flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,3)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'USWRFI'
    Diag(idx)%desc = 'instantaneous surface upward shortwave flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%uswsfci(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'duvb_ave'
    Diag(idx)%desc = 'UV-B Downward Solar Flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,21)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'cduvb_ave'
    Diag(idx)%desc = 'Clear sky UV-B Downward Solar Flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,22)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'vbdsf_ave'
    Diag(idx)%desc = 'Visible Beam Downward Solar Flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,24)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'vddsf_ave'
    Diag(idx)%desc = 'Visible Diffuse Downward Solar Flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,25)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'nbdsf_ave'
    Diag(idx)%desc = 'Near IR Beam Downward Solar Flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,26)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'nddsf_ave'
    Diag(idx)%desc = 'Near IR Diffuse Downward Solar Flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,27)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'csulf_avetoa'
    Diag(idx)%desc = 'Clear Sky Upward Long Wave Flux at toa'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,28)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'csusf_avetoa'
    Diag(idx)%desc = 'Clear Sky Upward Short Wave Flux at toa'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,29)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'csdlf_ave'
    Diag(idx)%desc = 'Clear Sky Downward Long Wave Flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,30)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'csusf_ave'
    Diag(idx)%desc = 'Clear Sky Upward Short Wave Flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,31)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'csdsf_ave'
    Diag(idx)%desc = 'Clear Sky Downward Short Wave Flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,32)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'csulf_ave'
    Diag(idx)%desc = 'Clear Sky Upward Long Wave Flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,33)
    enddo


    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'DSWRFtoa'
    Diag(idx)%desc = 'top of atmos downward shortwave flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,23)
    enddo


    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'USWRFtoa'
    Diag(idx)%desc = 'top of atmos upward shortwave flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,2)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ULWRFtoa'
    Diag(idx)%desc = 'top of atmos upward longwave flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,1)
    enddo
!    if(mpp_pe()==mpp_root_pe())print *,'in gfdl_diag_register,bf ULWRFtoa,idx=',idx

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TCDC_aveclm'
    Diag(idx)%desc = 'atmos column total cloud cover'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_100
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,17)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TCDC_avebndcl'
    Diag(idx)%desc = 'boundary layer cloud layer total cloud cover'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_100
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,18)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TCDCcnvcl'
    Diag(idx)%desc = 'convective cloud layer total cloud cover'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_100
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Cldprop(nb)%cv(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'PREScnvclt'
    Diag(idx)%desc = 'pressure at convective cloud top level'
    Diag(idx)%unit = 'pa'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%mask = 'cldmask'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Cldprop(nb)%cvt(:)
      Diag(idx)%data(nb)%var21 => Cldprop(nb)%cv(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'PREScnvclb'
    Diag(idx)%desc = 'pressure at convective cloud bottom level'
    Diag(idx)%unit = 'pa'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%mask = 'cldmask'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Cldprop(nb)%cvb(:)
      Diag(idx)%data(nb)%var21 => Cldprop(nb)%cv(:)
    enddo
!    if(mpp_pe()==mpp_root_pe())print *,'in gfdl_diag_register,af PREScnvclb,idx=',idx


    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TCDC_avehcl'
    Diag(idx)%desc = 'high cloud level total cloud cover'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_100
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,5)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'PRES_avehct'
    Diag(idx)%desc = 'pressure high cloud top level'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%mask = "cldmask_ratio"
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,8)
      Diag(idx)%data(nb)%var21 => Gfs_diag(nb)%fluxr(:,5)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'PRES_avehcb'
    Diag(idx)%desc = 'pressure high cloud bottom level'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%mask = "cldmask_ratio"
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,11)
      Diag(idx)%data(nb)%var21 => Gfs_diag(nb)%fluxr(:,5)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TEMP_avehct'
    Diag(idx)%desc = 'temperature high cloud top level'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%mask = "cldmask_ratio"
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,14)
      Diag(idx)%data(nb)%var21 => Gfs_diag(nb)%fluxr(:,5)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TCDC_avemcl'
    Diag(idx)%desc = 'mid cloud level total cloud cover'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_100
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,6)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'PRES_avemct'
    Diag(idx)%desc = 'pressure middle cloud top level'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%mask = "cldmask_ratio"
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,9)
      Diag(idx)%data(nb)%var21 => Gfs_diag(nb)%fluxr(:,6)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'PRES_avemcb'
    Diag(idx)%desc = 'pressure middle cloud bottom level'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%mask = "cldmask_ratio"
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,12)
      Diag(idx)%data(nb)%var21 => Gfs_diag(nb)%fluxr(:,6)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TEMP_avemct'
    Diag(idx)%desc = 'temperature middle cloud top level'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%mask = "cldmask_ratio"
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,15)
      Diag(idx)%data(nb)%var21 => Gfs_diag(nb)%fluxr(:,6)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TCDC_avelcl'
    Diag(idx)%desc = 'low cloud level total cloud cover'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_100
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,7)
    enddo

   idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'PRES_avelct'
    Diag(idx)%desc = 'pressure low cloud top level'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%mask = "cldmask_ratio"
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,10)
      Diag(idx)%data(nb)%var21 => Gfs_diag(nb)%fluxr(:,7)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'PRES_avelcb'
    Diag(idx)%desc = 'pressure low cloud bottom level'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%mask = "cldmask_ratio"
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,13)
      Diag(idx)%data(nb)%var21 => Gfs_diag(nb)%fluxr(:,7)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'TEMP_avelct'
    Diag(idx)%desc = 'temperature low cloud top level'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%mask = "cldmask_ratio"
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,16)
      Diag(idx)%data(nb)%var21 => Gfs_diag(nb)%fluxr(:,7)
    enddo
!    if(mpp_pe()==mpp_root_pe())print *,'in gfdl_diag_register,af TEMP_avelct,idx=',idx

!
!--- accumulated diagnostics ---
    do num = 1,NFXR
      write (xtra,'(I2.2)') num 
      idx = idx + 1
      Diag(idx)%axes = 2
      Diag(idx)%name = 'fluxr_'//trim(xtra)
      Diag(idx)%desc = 'fluxr diagnostic '//trim(xtra)//' - GFS radiation'
      Diag(idx)%unit = 'XXX'
      Diag(idx)%mod_name = 'gfs_phys'
      allocate (Diag(idx)%data(nblks))
      do nb = 1,nblks
        Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%fluxr(:,num)
      enddo
    enddo

!--- the next two appear to be appear to be coupling fields in gloopr
!--- each has four elements
!rab    do num = 1,4
!rab      write (xtra,'(I1)') num 
!rab      idx = idx + 1
!rab      Diag(idx)%axes = 2
!rab      Diag(idx)%name = 'dswcmp_'//trim(xtra)
!rab      Diag(idx)%desc = 'dswcmp dagnostic '//trim(xtra)//' - GFS radiation'
!rab      Diag(idx)%unit = 'XXX'
!rab      Diag(idx)%mod_name = 'gfs_phys'
!rab      do nb = 1,nblks
!rab        Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dswcmp(:,num)
!rab      enddo
!rab    enddo
!rab
!rab    do num = 1,4
!rab      write (xtra,'(I1)') num 
!rab      idx = idx + 1
!rab      Diag(idx)%axes = 2
!rab      Diag(idx)%name = 'uswcmp_'//trim(xtra)
!rab      Diag(idx)%desc = 'uswcmp dagnostic '//trim(xtra)//' - GFS radiation'
!rab      Diag(idx)%unit = 'XXX'
!rab      Diag(idx)%mod_name = 'gfs_phys'
!rab      do nb = 1,nblks
!rab        Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%uswcmp(:,num)
!rab      enddo
!rab    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'sw_upfxc'
    Diag(idx)%desc = 'total sky upward sw flux at toa - GFS radiation'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%topfsw(:)%upfxc
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'sw_dnfxc'
    Diag(idx)%desc = 'total sky downward sw flux at toa - GFS radiation'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%topfsw(:)%dnfxc
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'sw_upfx0'
    Diag(idx)%desc = 'clear sky upward sw flux at toa - GFS radiation'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%topfsw(:)%upfx0
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'lw_upfxc'
    Diag(idx)%desc = 'total sky upward lw flux at toa - GFS radiation'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%topflw(:)%upfxc
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'lw_upfx0'
    Diag(idx)%desc = 'clear sky upward lw flux at toa - GFS radiation'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%topflw(:)%upfx0
    enddo

!--- physics accumulated diagnostics ---
    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ssrun_acc'
    Diag(idx)%desc = 'surface storm water runoff - GFS lsm'
    Diag(idx)%unit = 'kg/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_th
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%srunoff(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'evbs_ave'
    Diag(idx)%desc = 'Direct Evaporation from Bare Soil - GFS lsm'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%evbsa(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'evcw_ave'
    Diag(idx)%desc = 'Canopy water evaporation - GFS lsm'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%evcwa(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'snohf'
    Diag(idx)%desc = 'Snow Phase Change Heat Flux - GFS lsm'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%snohfa(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'trans_ave'
    Diag(idx)%desc = 'transpiration - GFS lsm'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%transa(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'sbsno_ave'
    Diag(idx)%desc = 'Sublimation (evaporation from snow) - GFS lsm'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%sbsnoa(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'snowc_ave'
    Diag(idx)%desc = 'snow cover - GFS lsm'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%snowca(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'soilm'
    Diag(idx)%desc = 'total column soil moisture content'
    Diag(idx)%unit = 'kg/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_th
    Diag(idx)%mask = "land_only"
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2  => Gfs_diag(nb)%soilm(:)
      Diag(idx)%data(nb)%var21 => Sfcprop(nb)%slmsk(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tmpmin2m'
    Diag(idx)%desc = 'min temperature at 2m height'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%tmpmin(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tmpmax2m'
    Diag(idx)%desc = 'max temperature at 2m height'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%tmpmax(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dusfc'
    Diag(idx)%desc = 'surface zonal momentum flux'
    Diag(idx)%unit = 'N/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dusfc(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dvsfc'
    Diag(idx)%desc = 'surface meridional momentum flux'
    Diag(idx)%unit = 'N/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dvsfc(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'shtfl_ave'
    Diag(idx)%desc = 'surface sensible heat flux'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dtsfc(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'lhtfl_ave'
    Diag(idx)%desc = 'surface latent heat flux'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dqsfc(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'totprcp_ave'
    Diag(idx)%desc = 'surface precipitation rate'
    Diag(idx)%unit = 'kg/m**2/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_th
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%full_time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%totprcp(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'gflux_ave'
    Diag(idx)%desc = 'surface ground heat flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%mask = "land_ice_only"
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2  => Gfs_diag(nb)%gflux(:)
      Diag(idx)%data(nb)%var21 => Sfcprop(nb)%slmsk(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dlwsfc'
    Diag(idx)%desc = 'time accumulated downward lw flux at surface- GFS physics'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dlwsfc(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ulwsfc'
    Diag(idx)%desc = 'time accumulated upward lw flux at surface- GFS physics'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%ulwsfc(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'sunsd_acc'
    Diag(idx)%desc = 'Sunshine Duration'
    Diag(idx)%unit = 's'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%suntim(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'watr_acc'
    Diag(idx)%desc = 'total water runoff'
    Diag(idx)%unit = 'kg/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%runoff(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'pevpr_ave'
    Diag(idx)%desc = 'averaged potential evaporation rate'
    Diag(idx)%unit = 'W/M**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%ep(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'cwork_ave'
    Diag(idx)%desc = 'cloud work function (valid only with sas)'
    Diag(idx)%unit = 'J/kg'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%cldwrk(:)
    enddo


    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'u-gwd_ave'
    Diag(idx)%desc = 'surface zonal gravity wave stress'
    Diag(idx)%unit = 'N/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dugwd(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'v-gwd_ave'
    Diag(idx)%desc = 'surface meridional gravity wave stress'
    Diag(idx)%unit = 'N/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dvgwd(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'psmean'
    Diag(idx)%desc = 'surface pressure'
    Diag(idx)%unit = 'kPa'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%psmean(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'cnvprcp_ave'
    Diag(idx)%desc = 'averaged surface convective precipitation rate'
    Diag(idx)%unit = 'kg/m**2/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_th
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%full_time_avg = .TRUE.
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%cnvprcp(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'cnvprcp'
    Diag(idx)%desc = 'surface convective precipitation rate'
    Diag(idx)%unit = 'kg/m**2/s'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%cnvprcp(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'spfhmin2m'
    Diag(idx)%desc = 'minimum specific humidity'
    Diag(idx)%unit = 'kg/kg'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%spfhmin(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'spfhmax2m'
    Diag(idx)%desc = 'maximum specific humidity'
    Diag(idx)%unit = 'kg/kg'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%spfhmax(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'u10mmax'
    Diag(idx)%desc = 'maximum (magnitude) u-wind'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'vector_bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%u10mmax(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'v10mmax'
    Diag(idx)%desc = 'maximum (magnitude) v-wind'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'vector_bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%v10mmax(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'wind10mmax'
    Diag(idx)%desc = 'maximum wind speed'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%wind10mmax(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'rain'
    Diag(idx)%desc = 'total rain at this time step'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%rain(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'rainc'
    Diag(idx)%desc = 'convective rain at this time step'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%rainc(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ice'
    Diag(idx)%desc = 'ice fall at this time step'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%ice(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'snow'
    Diag(idx)%desc = 'snow fall at this time step'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%snow(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'graupel'
    Diag(idx)%desc = 'graupel fall at this time step'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%graupel(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'totice_ave'
    Diag(idx)%desc = 'surface ice precipitation rate'
    Diag(idx)%unit = 'kg/m**2/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_th
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%full_time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%totice(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'totsnw_ave'
    Diag(idx)%desc = 'surface snow precipitation rate'
    Diag(idx)%unit = 'kg/m**2/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_th
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%full_time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%totsnw(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'totgrp_ave'
    Diag(idx)%desc = 'surface graupel precipitation rate'
    Diag(idx)%unit = 'kg/m**2/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%cnvfac = cn_th
    Diag(idx)%time_avg = .TRUE.
    Diag(idx)%full_time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%totgrp(:)
    enddo
!    if(mpp_pe()==mpp_root_pe())print *,'in gfdl_diag_register,af totgrp,idx=',idx

!--- physics instantaneous diagnostics ---
    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'u10m'
    Diag(idx)%desc = '10 meter u wind'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'vector_bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%u10m(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'v10m'
    Diag(idx)%desc = '10 meter v wind'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'vector_bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%v10m(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dpt2m'
    Diag(idx)%desc = '2 meter dew point temperature'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dpt2m(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'hgt_hyblev1'
    Diag(idx)%desc = 'layer 1 height'
    Diag(idx)%unit = 'm'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%zlvl(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'psurf'
    Diag(idx)%desc = 'surface pressure'
    Diag(idx)%unit = 'Pa'
    Diag(idx)%mask = 'pseudo_ps'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%psurf(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'hpbl'
    Diag(idx)%desc = 'surface planetary boundary layer height'
    Diag(idx)%unit = 'm'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%hpbl(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'pwat'
    Diag(idx)%desc = 'atmos column precipitable water'
    Diag(idx)%unit = 'kg/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%pwat(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tmp_hyblev1'
    Diag(idx)%desc = 'layer 1 temperature'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%t1(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'spfh_hyblev1'
    Diag(idx)%desc = 'layer 1 specific humidity'
    Diag(idx)%unit = 'kg/kg'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%q1(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ugrd_hyblev1'
    Diag(idx)%desc = 'layer 1 zonal wind'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'vector_bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%u1(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'vgrd_hyblev1'
    Diag(idx)%desc = 'layer 1 meridional wind'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'vector_bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%v1(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'sfexc'
    Diag(idx)%desc = 'Exchange Coefficient'
    Diag(idx)%unit = 'kg/m2/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%chh(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'acond'
    Diag(idx)%desc = 'Aerodynamic conductance'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%cmm(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dlwsfci'
    Diag(idx)%desc = 'instantaneous sfc downward lw flux'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dlwsfci(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ulwsfci'
    Diag(idx)%desc = 'instantaneous sfc upward lw flux'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%ulwsfci(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dswsfci'
    Diag(idx)%desc = 'instantaneous sfc downward sw flux'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dswsfci(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'uswsfci'
    Diag(idx)%desc = 'instantaneous sfc upward sw flux'
    Diag(idx)%unit = 'w/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%uswsfci(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dusfci'
    Diag(idx)%desc = 'instantaneous u component of surface stress'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dusfci(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'dvsfci'
    Diag(idx)%desc = 'instantaneous v component of surface stress'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dvsfci(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'shtfl'
    Diag(idx)%desc = 'instantaneous surface sensible heat net flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dtsfci(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'lhtfl'
    Diag(idx)%desc = 'instantaneous surface latent heat net flux'
    Diag(idx)%unit = 'W/m**2'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%dqsfci(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'gfluxi'
    Diag(idx)%desc = 'instantaneous surface ground heat flux'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%gfluxi(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'pevpr'
    Diag(idx)%desc = 'instantaneous surface potential evaporation'
    Diag(idx)%unit = 'W/M**2'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%epi(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'wilt'
    Diag(idx)%desc = 'wiltimg point (volumetric)'
    Diag(idx)%unit = 'Proportion'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%smcwlt2(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'fldcp'
    Diag(idx)%desc = 'Field Capacity (volumetric)'
    Diag(idx)%unit = 'fraction'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%smcref2(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'wet1'
    Diag(idx)%desc = 'normalized soil wetness'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%wet1(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'cpofp'
    Diag(idx)%desc = 'Percent frozen precipitation'
    Diag(idx)%unit = '%'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%sr(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'crain_ave'
    Diag(idx)%desc = 'averaged categorical rain'
    Diag(idx)%unit = 'number'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%tdomr(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'csnow_ave'
    Diag(idx)%desc = 'averaged categorical snow'
    Diag(idx)%unit = 'number'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%tdoms(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'cfrzr_ave'
    Diag(idx)%desc = 'averaged categorical freezing rain'
    Diag(idx)%unit = 'number'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%tdomzr(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'cicep_ave'
    Diag(idx)%desc = 'averaged categorical sleet'
    Diag(idx)%unit = 'number'
    Diag(idx)%mod_name = 'gfs_phys'
    Diag(idx)%intpl_method = 'bilinear'
    Diag(idx)%cnvfac = cn_one
    Diag(idx)%time_avg = .TRUE.
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%tdomip(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 3
    Diag(idx)%name = 'skebu_wts'
    Diag(idx)%desc = 'perturbation velocity'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var3 => Gfs_diag(nb)%skebu_wts(:,:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 3
    Diag(idx)%name = 'skebv_wts'
    Diag(idx)%desc = 'perturbation velocity'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var3 => Gfs_diag(nb)%skebv_wts(:,:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'zmtnblck'
    Diag(idx)%desc = 'level of dividing streamline'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Gfs_diag(nb)%zmtnblck(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 3
    Diag(idx)%name = 'sppt_wts'
    Diag(idx)%desc = 'perturbation velocity'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var3 => Gfs_diag(nb)%sppt_wts(:,:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 3
    Diag(idx)%name = 'shum_wts'
    Diag(idx)%desc = 'perturbation velocity'
    Diag(idx)%unit = 'm/s'
    Diag(idx)%mod_name = 'gfs_phys'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var3 => Gfs_diag(nb)%shum_wts(:,:)
    enddo
!    if(mpp_pe()==mpp_root_pe())print *,'in gfdl_diag_register,af shum_wts,idx=',idx

!--- three-dimensional variables that need to be handled special when writing 
!rab    do num = 1,6
!rab      write (xtra,'(I1)') num 
!rab      idx = idx + 1
!rab      Diag(idx)%axes = 3
!rab      Diag(idx)%name = 'dt3dt_'//trim(xtra)
!rab      Diag(idx)%desc = 'temperature change due to physics '//trim(xtra)//''
!rab      Diag(idx)%unit = 'XXX'
!rab      Diag(idx)%mod_name = 'gfs_phys'
!rab    enddo
!rab
!rab    do num = 1,5+Mdl_parms%pl_coeff
!rab      write (xtra,'(I1)') num 
!rab      idx = idx + 1
!rab      Diag(idx)%axes = 3
!rab      Diag(idx)%name = 'dq3dt_'//trim(xtra)
!rab      Diag(idx)%desc = 'moisture change due to physics '//trim(xtra)//''
!rab      Diag(idx)%unit = 'XXX'
!rab      Diag(idx)%mod_name = 'gfs_phys'
!rab    enddo
!rab
!rab    do num = 1,4
!rab      write (xtra,'(I1)') num 
!rab      idx = idx + 1
!rab      Diag(idx)%axes = 3
!rab      Diag(idx)%name = 'du3dt_'//trim(xtra)
!rab      Diag(idx)%desc = 'u momentum change due to physics '//trim(xtra)//''
!rab      Diag(idx)%unit = 'XXX'
!rab      Diag(idx)%mod_name = 'gfs_phys'
!rab    enddo
!rab
!rab    do num = 1,4
!rab      write (xtra,'(I1)') num 
!rab      idx = idx + 1
!rab      Diag(idx)%axes = 3
!rab      Diag(idx)%name = 'dv3dt_'//trim(xtra)
!rab      Diag(idx)%desc = 'v momentum change due to physics '//trim(xtra)//''
!rab      Diag(idx)%unit = 'XXX'
!rab      Diag(idx)%mod_name = 'gfs_phys'
!rab    enddo
!rab
!rab    idx = idx + 1
!rab    Diag(idx)%axes = 3
!rab    !Requires lgocart = .T.
!rab    Diag(idx)%name = 'dqdt_v'
!rab    Diag(idx)%desc = 'instantaneous total moisture tendency'
!rab    Diag(idx)%unit = 'XXX'
!rab    Diag(idx)%mod_name = 'gfs_phys'

!--- Surface diagnostics in gfs_sfc
    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'alnsf'
    Diag(idx)%desc = 'mean nir albedo with strong cosz dependency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%alnsf(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'alnwf'
    Diag(idx)%desc = 'mean nir albedo with weak cosz dependency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%alnwf(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'alvsf'
    Diag(idx)%desc = 'mean vis albedo with strong cosz dependency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%alvsf(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'alvwf'
    Diag(idx)%desc = 'mean vis albedo with weak cosz dependency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%alvwf(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'canopy'
    Diag(idx)%desc = 'canopy water (cnwat in gfs data)'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%canopy(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'f10m'
    Diag(idx)%desc = '10-meter wind speed divided by lowest model wind speed'
    Diag(idx)%unit = 'N/A'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%f10m(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'facsf'
    Diag(idx)%desc = 'fractional coverage with strong cosz dependency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%facsf(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'facwf'
    Diag(idx)%desc = 'fractional coverage with weak cosz dependency'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%facwf(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ffhh'
    Diag(idx)%desc = 'fh parameter from PBL scheme'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%ffhh(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'ffmm'
    Diag(idx)%desc = 'fm parameter from PBL scheme'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%ffmm(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'uustar'
    Diag(idx)%desc = 'uustar surface frictional wind'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%uustar(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'slope'
    Diag(idx)%desc = 'surface slope type'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%slope(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'fice'
    Diag(idx)%desc = 'surface ice concentration (ice=1; no ice=0)'
    Diag(idx)%unit = 'fraction'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%fice(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'hice'
    Diag(idx)%desc = 'sea ice thickness (icetk in gfs_data)'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%hice(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'snoalb'
    Diag(idx)%desc = 'maximum snow albedo in fraction (salbd?? in gfs data)'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%snoalb(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'shdmax'
    Diag(idx)%desc = 'maximum fractional coverage of green vegetation'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%shdmax(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'shdmin'
    Diag(idx)%desc = 'minimum fractional coverage of green vegetation'
    Diag(idx)%unit = 'XXX'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%shdmin(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'snowd'
    Diag(idx)%desc = 'surface snow depth'
    Diag(idx)%unit = 'm'
    Diag(idx)%mod_name = 'gfs_sfc'
    Diag(idx)%cnvfac = cn_one/cn_th
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%snowd(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'crain'
    Diag(idx)%desc = 'instantaneous categorical rain'
    Diag(idx)%unit = 'number'
    Diag(idx)%mod_name = 'gfs_sfc'
    Diag(idx)%cnvfac = cn_one
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%srflag(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'stype'
    Diag(idx)%desc = 'soil type in integer 1-9'
    Diag(idx)%unit = 'number'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%stype(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'q2m'
    Diag(idx)%desc = '2m specific humidity'
    Diag(idx)%unit = 'kg/kg'
    Diag(idx)%mod_name = 'gfs_sfc'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%q2m(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 't2m'
    Diag(idx)%desc = '2m temperature'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_sfc'
    Diag(idx)%intpl_method = 'bilinear'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%t2m(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tsfc'
    Diag(idx)%desc = 'surface temperature'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%tsfc(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tg3'
    Diag(idx)%desc = 'deep soil temperature'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%tg3(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tisfc'
    Diag(idx)%desc = 'surface temperature over ice fraction'
    Diag(idx)%unit = 'K'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%tisfc(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'tprcp'
    Diag(idx)%desc = 'total precipitation'
    Diag(idx)%unit = 'kg/m**2'
    Diag(idx)%mod_name = 'gfs_sfc'
    allocate (Diag(idx)%data(nblks))
    do nb = 1,nblks
      Diag(idx)%data(nb)%var2 => Sfcprop(nb)%tprcp(:)
    enddo

    idx = idx + 1
    Diag(idx)%axes = 2
    Diag(idx)%name = 'vtype'
    Diag(idx)%desc = 'vegetation type in integer 1-13'
    Diag(idx)%unit = 'number'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%vtype(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'weasd'
    diag(idx)%desc = 'surface snow water equivalent'
    diag(idx)%unit = 'kg/m**2'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%weasd(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'hgtsfc'
    diag(idx)%desc = 'surface geopotential height'
    diag(idx)%unit = 'gpm'
    diag(idx)%mod_name = 'gfs_sfc'
    diag(idx)%cnvfac = cn_one
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%oro(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'slmsksfc'
    diag(idx)%desc = 'sea-land-ice mask (0-sea, 1-land, 2-ice)'
    diag(idx)%unit = 'numerical'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%slmsk(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'zorlsfc'
    diag(idx)%desc = 'surface roughness'
    diag(idx)%unit = 'm'
    diag(idx)%mod_name = 'gfs_sfc'
    diag(idx)%cnvfac = cn_one/cn_100
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%zorl(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'vfracsfc'
    diag(idx)%desc = 'vegetation fraction'
    diag(idx)%unit = 'fraction'
    diag(idx)%mod_name = 'gfs_sfc'
    diag(idx)%cnvfac = cn_100
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%vfrac(:)
    enddo

    do num = 1,4
      write (xtra,'(i1)') num 
      idx = idx + 1
      diag(idx)%axes = 2
      diag(idx)%name = 'slc_'//trim(xtra)
      diag(idx)%desc = 'liquid soil mositure at layer-'//trim(xtra)
      diag(idx)%unit = 'xxx'
      diag(idx)%mod_name = 'gfs_sfc'
      allocate (diag(idx)%data(nblks))
      do nb = 1,nblks
        diag(idx)%data(nb)%var2 => sfcprop(nb)%slc(:,num)
      enddo
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'soilw1'
    diag(idx)%desc = 'volumetric soil moisture 0-10cm'
    diag(idx)%unit = 'fraction'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%smc(:,1)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'soilw2'
    diag(idx)%desc = 'volumetric soil moisture 10-40cm'
    diag(idx)%unit = 'fraction'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%smc(:,2)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'soilw3'
    diag(idx)%desc = 'volumetric soil moisture 40-100cm'
    diag(idx)%unit = 'fraction'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%smc(:,3)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'soilw4'
    diag(idx)%desc = 'volumetric soil moisture 100-200cm'
    diag(idx)%unit = 'fraction'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%smc(:,4)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'soilt1'
    diag(idx)%desc = 'soil temperature 0-10cm' 
    diag(idx)%unit = 'K'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%stc(:,1)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'soilt2'
    diag(idx)%desc = 'soil temperature 10-40cm' 
    diag(idx)%unit = 'K'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%stc(:,2)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'soilt3'
    diag(idx)%desc = 'soil temperature 40-100cm' 
    diag(idx)%unit = 'K'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%stc(:,3)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'soilt4'
    diag(idx)%desc = 'soil temperature 100-200cm' 
    diag(idx)%unit = 'K'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%stc(:,4)
    enddo
!    print *,'in gfdl_diag_register,af soilt4,idx=',idx,model%nstf_name(1)

!--------------------------nsst variables
  if (model%nstf_name(1) > 0) then
!--------------------------nsst variables

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'tref'
    diag(idx)%desc = 'nsst reference or foundation temperature'
    diag(idx)%unit = 'K'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%tref(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'z_c'
    diag(idx)%desc = 'nsst sub-layer cooling thickness'
    diag(idx)%unit = 'm'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%z_c(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'c_0'
    diag(idx)%desc = 'nsst coefficient1 to calculate d(tz)/d(ts)'
    diag(idx)%unit = 'numerical'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%c_0(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'c_d'
    diag(idx)%desc = 'nsst coefficient2 to calculate d(tz)/d(ts)'
    diag(idx)%unit = 'n/a'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%c_d(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'w_0'
    diag(idx)%desc = 'nsst coefficient3 to calculate d(tz)/d(ts)'
    diag(idx)%unit = 'n/a'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%w_0(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'w_d'
    diag(idx)%desc = 'nsst coefficient4 to calculate d(tz)/d(ts)'
    diag(idx)%unit = 'n/a'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%w_d(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'xt'
    diag(idx)%desc = 'nsst heat content in diurnal thermocline layer'
    diag(idx)%unit = 'k*m'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%xt(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'xs'
    diag(idx)%desc = 'nsst salinity content in diurnal thermocline layer'
    diag(idx)%unit = 'n/a'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%xs(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'xu'
    diag(idx)%desc = 'nsst u-current content in diurnal thermocline layer'
    diag(idx)%unit = 'm2/s'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%xu(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'xv'
    diag(idx)%desc = 'nsst v-current content in diurnal thermocline layer'
    diag(idx)%unit = 'm2/s'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%xv(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'xz'
    diag(idx)%desc = 'nsst diurnal thermocline layer thickness'
    diag(idx)%unit = 'm'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%xz(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'zm'
    diag(idx)%desc = 'nsst mixed layer thickness'
    diag(idx)%unit = 'm'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%zm(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'xtts'
    diag(idx)%desc = 'nsst d(xt)/d(ts)'
    diag(idx)%unit = 'm'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%xtts(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'xzts'
    diag(idx)%desc = 'nsst d(xt)/d(ts)'
    diag(idx)%unit = 'm/k'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%xzts(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'd_conv'
    diag(idx)%desc = 'nsst thickness of free convection layer'
    diag(idx)%unit = 'm'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%d_conv(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'ifd'
    diag(idx)%desc = 'nsst index to start dtlm run or not'
    diag(idx)%unit = 'n/a'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%ifd(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'dt_cool'
    diag(idx)%desc = 'nsst sub-layer cooling amount'
    diag(idx)%unit = 'k'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%dt_cool(:)
    enddo

    idx = idx + 1
    diag(idx)%axes = 2
    diag(idx)%name = 'qrain'
    diag(idx)%desc = 'nsst sensible heat flux due to rainfall'
    diag(idx)%unit = 'w/m2'
    diag(idx)%mod_name = 'gfs_sfc'
    allocate (diag(idx)%data(nblks))
    do nb = 1,nblks
      diag(idx)%data(nb)%var2 => sfcprop(nb)%qrain(:)
    enddo
!--------------------------nsst variables
  endif
!--------------------------nsst variables
!    if(mpp_pe()==mpp_root_pe())print *,'in gfdl_diag_register,af qrain,idx=',idx


!--- prognostic variable tendencies (t, u, v, sph, clwmr, o3)
!rab    idx = idx + 1
!rab    diag(idx)%axes = 3
!rab    diag(idx)%name = 'dtemp_dt'
!rab    diag(idx)%desc = 'gfs radiation/physics temperature tendency'
!rab    diag(idx)%unit = 'k/s'
!rab    diag(idx)%mod_name = 'gfs_phys'
!rab
!rab    idx = idx + 1
!rab    diag(idx)%axes = 3
!rab    diag(idx)%name = 'du_dt'
!rab    diag(idx)%desc = 'gfs radiation/physics horizontal wind component tendency'
!rab    diag(idx)%unit = 'm/s/s'
!rab    diag(idx)%mod_name = 'gfs_phys'
!rab
!rab    idx = idx + 1
!rab    diag(idx)%axes = 3
!rab    diag(idx)%name = 'dv_dt'
!rab    diag(idx)%desc = 'gfs radiation/physics meridional wind component tendency'
!rab    diag(idx)%unit = 'm/s/s'
!rab    diag(idx)%mod_name = 'gfs_phys'
!rab
!rab    idx = idx + 1
!rab    diag(idx)%axes = 3
!rab    diag(idx)%name = 'dsphum_dt'
!rab    diag(idx)%desc = 'gfs radiation/physics specific humidity tendency'
!rab    diag(idx)%unit = 'kg/kg/s'
!rab    diag(idx)%mod_name = 'gfs_phys'
!rab
!rab    idx = idx + 1
!rab    diag(idx)%axes = 3
!rab    diag(idx)%name = 'dclwmr_dt'
!rab    diag(idx)%desc = 'gfs radiation/radiation cloud water mixing ratio tendency'
!rab    diag(idx)%unit = 'kg/kg/s'
!rab    diag(idx)%mod_name = 'gfs_phys'
!rab
!rab    idx = idx + 1
!rab    diag(idx)%axes = 3
!rab    diag(idx)%name = 'do3mr_dt'
!rab    diag(idx)%desc = 'gfs radiation/radiation ozone mixing ratio tendency'
!rab    diag(idx)%unit = 'kg/kg/s'
!rab    diag(idx)%mod_name = 'gfs_phys'

    tot_diag_idx = idx

    if (idx > diag_size) then
      call mpp_error(fatal, 'gfs_driver::gfs_diag_register - need to increase diag_size') 
    endif

    allocate(nstt(tot_diag_idx), nstt_vctbl(tot_diag_idx))
    nstt = 0
    nstt_vctbl = 0
    nrgst_bl = 0
    nrgst_nb = 0
    nrgst_vctbl = 0
    do idx = 1,tot_diag_idx
      if (diag(idx)%axes == -99) then
        call mpp_error(fatal, 'gfs_driver::gfs_diag_register - attempt to register an undefined variable')
      endif
      diag(idx)%id = register_diag_field (trim(diag(idx)%mod_name), trim(diag(idx)%name),  &
                                          axes(1:diag(idx)%axes), time, trim(diag(idx)%desc), &
                                          trim(diag(idx)%unit), missing_value=real(missing_value))
      if(diag(idx)%id > 0) then
        if (diag(idx)%axes == 2) then
           if( index(trim(diag(idx)%intpl_method),'bilinear') > 0 ) then
             nrgst_bl = nrgst_bl + 1
             nstt(idx) = nrgst_bl
           else if (trim(diag(idx)%intpl_method) == 'nearest_stod' ) then
             nrgst_nb = nrgst_nb + 1
             nstt(idx) = nrgst_nb
           endif
           if(trim(diag(idx)%intpl_method) == 'vector_bilinear') then
             if(diag(idx)%name(1:1) == 'v' .or. diag(idx)%name(1:1) == 'v') then
               nrgst_vctbl = nrgst_vctbl + 1
               nstt_vctbl(idx) = nrgst_vctbl
!             print *,'in phy_setup, vector_bilinear, name=', trim(diag(idx)%name),' nstt_vctbl=', nstt_vctbl(idx), 'idx=',idx
             endif
           endif
!        elif (diag(idx)%axes == 3) then
!           nrgst = nrgst+levs
        endif
      endif

    enddo

    total_outputlevel = nrgst_bl + nrgst_nb
    allocate(buffer_phys_bl(isco:ieco,jsco:jeco,nrgst_bl))
    allocate(buffer_phys_nb(isco:ieco,jsco:jeco,nrgst_nb))
    allocate(buffer_phys_windvect(3,isco:ieco,jsco:jeco,nrgst_vctbl))
    buffer_phys_bl = 0.
    buffer_phys_nb = 0.
    buffer_phys_windvect = 0.
    if(mpp_pe() == mpp_root_pe()) print *,'in gfdl_diag_register, nrgst_bl=',nrgst_bl,' nrgst_nb=',nrgst_nb, &
       ' nrgst_vctbl=',nrgst_vctbl, 'isco=',isco,ieco,'jsco=',jsco,jeco

  end subroutine gfdl_diag_register
!-------------------------------------------------------------------------      


!-------------------------------------------------------------------------      
!--- gfs_diag_output ---
!-------------------------------------------------------------------------      
!    routine to transfer the diagnostic data to the gfdl fms diagnostic 
!    manager for eventual output to the history files.
!
!    calls:  send_data
!-------------------------------------------------------------------------      
!rab  subroutine gfdl_diag_output(time, gfs_diag, statein, stateout, atm_block, &
!rab                             nx, ny, levs, ntcw, ntoz, dt, time_int)
  subroutine gfdl_diag_output(time, atm_block, nx, ny, levs, ntcw, ntoz, &
                              dt, time_int, time_intfull)
!--- subroutine interface variable definitions
    type(time_type),           intent(in) :: time
!rab    type(diagnostics),         intent(in) :: gfs_diag
!rab    type(state_fields_in),     intent(in) :: statein
!rab    type(state_fields_out),    intent(in) :: stateout
    type (block_control_type), intent(in) :: atm_block
    integer,                   intent(in) :: nx, ny, levs, ntcw, ntoz
    real(kind=kind_phys),      intent(in) :: dt
    real(kind=kind_phys),      intent(in) :: time_int
    real(kind=kind_phys),      intent(in) :: time_intfull
!--- local variables
    integer :: i, j, idx, nblks, nb, ix, ii, jj
    integer :: is_in, js_in, isc, jsc
    character(len=2) :: xtra
    real(kind=kind_phys), dimension(nx*ny)      :: var2p
    real(kind=kind_phys), dimension(nx*ny,levs) :: var3p
    real(kind=kind_phys), dimension(nx,ny)      :: var2
    real(kind=kind_phys), dimension(nx,ny,levs) :: var3
    real(kind=kind_phys) :: rdt, rtime_int, rtime_intfull, lcnvfac
    logical :: used

     nblks = atm_block%nblks
     rdt = 1.0d0/dt
     rtime_int = 1.0d0/time_int
     rtime_intfull = 1.0d0/time_intfull

     isc = atm_block%isc
     jsc = atm_block%jsc
     is_in = atm_block%isc
     js_in = atm_block%jsc

     do idx = 1,tot_diag_idx
       if (diag(idx)%id > 0) then
         lcnvfac = diag(idx)%cnvfac
         if (diag(idx)%time_avg) then
           if ( diag(idx)%full_time_avg .and. lprecip_accu ) then
             lcnvfac = lcnvfac*rtime_intfull
           else
             lcnvfac = lcnvfac*rtime_int
           endif
         endif
         if (diag(idx)%axes == 2) then
           if (trim(diag(idx)%mask) == 'positive_flux') then
             !--- albedos are actually a ratio of two radiation surface properties
             var2(1:nx,1:ny) = 0._kind_phys
             do j = 1, ny
               jj = j + jsc -1
               do i = 1, nx
                 ii = i + isc -1
                 nb = Atm_block%blkno(ii,jj)
                 ix = Atm_block%ixp(ii,jj)
                 if (Diag(idx)%data(nb)%var21(ix) > 0._kind_phys) &
                   var2(i,j) = max(0._kind_phys,min(1._kind_phys,Diag(idx)%data(nb)%var2(ix)/Diag(idx)%data(nb)%var21(ix)))*lcnvfac
               enddo
             enddo
           elseif (trim(Diag(idx)%mask) == 'land_ice_only') then
             !--- need to "mask" gflux to output valid data over land/ice only
             var2(1:nx,1:ny) = missing_value
             do j = 1, ny
               jj = j + jsc -1
               do i = 1, nx
                 ii = i + isc -1
                 nb = Atm_block%blkno(ii,jj)
                 ix = Atm_block%ixp(ii,jj)
                  if (Diag(idx)%data(nb)%var21(ix) /= 0) var2(i,j) = Diag(idx)%data(nb)%var2(ix)*lcnvfac
               enddo
             enddo
           elseif (trim(Diag(idx)%mask) == 'land_only') then
             !--- need to "mask" soilm to have value only over land
             var2(1:nx,1:ny) = missing_value
             do j = 1, ny
               jj = j + jsc -1
               do i = 1, nx
                 ii = i + isc -1
                 nb = Atm_block%blkno(ii,jj)
                 ix = Atm_block%ixp(ii,jj)
                 if (Diag(idx)%data(nb)%var21(ix) == 1) var2(i,j) = Diag(idx)%data(nb)%var2(ix)*lcnvfac
               enddo
             enddo
           elseif (trim(Diag(idx)%mask) == 'cldmask') then
             !--- need to "mask" soilm to have value only over land
             var2(1:nx,1:ny) = missing_value
             do j = 1, ny
               jj = j + jsc -1
               do i = 1, nx
                 ii = i + isc -1
                 nb = Atm_block%blkno(ii,jj)
                 ix = Atm_block%ixp(ii,jj)
                 if (Diag(idx)%data(nb)%var21(ix)*100. > 0.5) var2(i,j) = Diag(idx)%data(nb)%var2(ix)*lcnvfac
               enddo
             enddo
           elseif (trim(Diag(idx)%mask) == 'cldmask_ratio') then
             !--- need to "mask" soilm to have value only over land
             var2(1:nx,1:ny) = missing_value
             do j = 1, ny
               jj = j + jsc -1
               do i = 1, nx
                 ii = i + isc -1
                 nb = Atm_block%blkno(ii,jj)
                 ix = Atm_block%ixp(ii,jj)
                 if (Diag(idx)%data(nb)%var21(ix)*100.*lcnvfac > 0.5) var2(i,j) = Diag(idx)%data(nb)%var2(ix)/ &
                     Diag(idx)%data(nb)%var21(ix)
               enddo
             enddo
           elseif (trim(Diag(idx)%mask) == 'pseudo_ps') then
             if ( use_wrtgridcomp_output ) then
               do j = 1, ny
                 jj = j + jsc -1
                 do i = 1, nx
                   ii = i + isc -1
                   nb = Atm_block%blkno(ii,jj)
                   ix = Atm_block%ixp(ii,jj)
                   var2(i,j) = (Diag(idx)%data(nb)%var2(ix)/stndrd_atmos_ps)**(rdgas/grav*stndrd_atmos_lapse)
                 enddo
               enddo
             else
               do j = 1, ny
                 jj = j + jsc -1
                 do i = 1, nx
                   ii = i + isc -1
                   nb = Atm_block%blkno(ii,jj)
                   ix = Atm_block%ixp(ii,jj)
                   var2(i,j) = Diag(idx)%data(nb)%var2(ix)
                 enddo
               enddo
             endif
           elseif (trim(Diag(idx)%mask) == '') then
             do j = 1, ny
               jj = j + jsc -1
               do i = 1, nx
                 ii = i + isc -1
                 nb = Atm_block%blkno(ii,jj)
                 ix = Atm_block%ixp(ii,jj)
                 var2(i,j) = Diag(idx)%data(nb)%var2(ix)*lcnvfac
               enddo
             enddo
           endif
!rab           used=send_data(Diag(idx)%id, var2, Time, is_in=is_in, js_in=js_in)
!           used=send_data(Diag(idx)%id, var2, Time)
!           print *,'in phys, after store_data, idx=',idx,' var=', trim(Diag(idx)%name)
           call store_data(Diag(idx)%id, var2, Time, idx, Diag(idx)%intpl_method, Diag(idx)%name)
!           if(trim(Diag(idx)%name) == 'totprcp_ave' ) print *,'in gfs_io, totprcp=',Diag(idx)%data(1)%var2(1:3), &
!             ' lcnvfac=', lcnvfac
         elseif (Diag(idx)%axes == 3) then
         !---
         !--- skipping the 3D variables with the following else statement
         !---
             do j = 1, ny
               jj = j + jsc -1
               do i = 1, nx
                 ii = i + isc -1
                 nb = Atm_block%blkno(ii,jj)
                 ix = Atm_block%ixp(ii,jj)
                 var3(i,j,1:levs) = Diag(idx)%data(nb)%var3(ix,1:levs)*lcnvfac
               enddo
             enddo
           used=send_data(Diag(idx)%id, var3, Time)
#ifdef JUNK
         else
           !--- dt3dt variables
           do num = 1,6
             write(xtra,'(i1)') num
             if (trim(Diag(idx)%name) == 'dt3dt_'//trim(xtra)) then
               var3(1:nx,1:ny,1:levs) = RESHAPE(Gfs_diag%dt3dt(1:ngptc,levs:1:-1,num:num), (/nx,ny,levs/))
               used=send_data(Diag(idx)%id, var3, Time, is_in=is_in, js_in=js_in, ks_in=1) 
             endif
           enddo
           !--- dq3dt variables
           do num = 1,5+Mdl_parms%pl_coeff
             write(xtra,'(i1)') num
             if (trim(Diag(idx)%name) == 'dq3dt_'//trim(xtra)) then
               var3(1:nx,1:ny,1:levs) = RESHAPE(Gfs_diag%dq3dt(1:ngptc,levs:1-1,num:num), (/nx,ny,levs/))
               used=send_data(Diag(idx)%id, var3, Time, is_in=is_in, js_in=js_in, ks_in=1) 
             endif
           enddo
           !--- du3dt and dv3dt variables
           do num = 1,4
             write(xtra,'(i1)') num
             if (trim(Diag(idx)%name) == 'du3dt_'//trim(xtra)) then
               var3(1:nx,1:ny,1:levs) = RESHAPE(Gfs_diag%du3dt(1:ngptc,levs:1:-1,num:num), (/nx,ny,levs/))
               used=send_data(Diag(idx)%id, var3, Time, is_in=is_in, js_in=js_in, ks_in=1) 
             endif
             if (trim(Diag(idx)%name) == 'dv3dt_'//trim(xtra)) then
               var3(1:nx,1:ny,1:levs) = RESHAPE(Gfs_diag%dv3dt(1:ngptc,levs:1:-1,num:num), (/nx,ny,levs/))
               used=send_data(Diag(idx)%id, var3, Time, is_in=is_in, js_in=js_in, ks_in=1) 
             endif
           enddo
           if (trim(Diag(idx)%name) == 'dqdt_v') then
             var3(1:nx,1:ny,1:levs) = RESHAPE(Gfs_diag%dqdt_v(1:ngptc,levs:1:-1), (/nx,ny,levs/))
             used=send_data(Diag(idx)%id, var3, Time, is_in=is_in, js_in=js_in, ks_in=1) 
           endif
           !--- temperature tendency
           if (trim(Diag(idx)%name) == 'dtemp_dt') then
             var3(1:nx,1:ny,1:levs) =  RESHAPE(Statein%tgrs(1:ngptc,levs:1:-1), (/nx,ny,levs/))
             var3(1:nx,1:ny,1:levs) = (RESHAPE(Stateout%gt0(1:ngptc,levs:1:-1), (/nx,ny,levs/))  &
                                        - var3(1:nx,1:ny,1:levs))*rdt
             used=send_data(Diag(idx)%id, var3, Time, is_in=is_in, js_in=js_in, ks_in=1) 
           endif
           !--- horizontal wind component tendency
           if (trim(Diag(idx)%name) == 'du_dt') then
             var3(1:nx,1:ny,1:levs) =  RESHAPE(Statein%ugrs(1:ngptc,levs:1:-1), (/nx,ny,levs/))
             var3(1:nx,1:ny,1:levs) = (RESHAPE(Stateout%gu0(1:ngptc,levs:1:-1), (/nx,ny,levs/))  &
                                        - var3(1:nx,1:ny,1:levs))*rdt
             used=send_data(Diag(idx)%id, var3, Time, is_in=is_in, js_in=js_in, ks_in=1) 
           endif
           !--- meridional wind component tendency
           if (trim(Diag(idx)%name) == 'dv_dt') then
             var3(1:nx,1:ny,1:levs) =  RESHAPE(Statein%vgrs(1:ngptc,levs:1:-1), (/nx,ny,levs/))
             var3(1:nx,1:ny,1:levs) = (RESHAPE(Stateout%gv0(1:ngptc,levs:1:-1), (/nx,ny,levs/))  &
                                        - var3(1:nx,1:ny,1:levs))*rdt
             used=send_data(Diag(idx)%id, var3, Time, is_in=is_in, js_in=js_in, ks_in=1) 
           endif
           !--- specific humidity tendency
           if (trim(Diag(idx)%name) == 'dsphum_dt') then
             var3(1:nx,1:ny,1:levs) =  RESHAPE(Statein%qgrs(1:ngptc,levs:1:-1,1:1), (/nx,ny,levs/))
             var3(1:nx,1:ny,1:levs) = (RESHAPE(Stateout%gq0(1:ngptc,levs:1:-1,1:1), (/nx,ny,levs/))  &
                                        - var3(1:nx,1:ny,1:levs))*rdt
             used=send_data(Diag(idx)%id, var3, Time, is_in=is_in, js_in=js_in, ks_in=1) 
           endif
           !--- cloud water mixing ration tendency
           if (trim(Diag(idx)%name) == 'dclwmr_dt') then
             var3(1:nx,1:ny,1:levs) =  RESHAPE(Statein%qgrs(1:ngptc,levs:1:-1,ntcw:ntcw), (/nx,ny,levs/))
             var3(1:nx,1:ny,1:levs) = (RESHAPE(Stateout%gq0(1:ngptc,levs:1:-1,ntcw:ntcw), (/nx,ny,levs/))  &
                                        - var3(1:nx,1:ny,1:levs))*rdt
             used=send_data(Diag(idx)%id, var3, Time, is_in=is_in, js_in=js_in, ks_in=1) 
           endif
           !--- ozone mixing ration tendency
           if (trim(Diag(idx)%name) == 'do3mr_dt') then
             var3(1:nx,1:ny,1:levs) =  RESHAPE(Statein%qgrs(1:ngptc,levs:1:-1,ntoz:ntoz), (/nx,ny,levs/))
             var3(1:nx,1:ny,1:levs) = (RESHAPE(Stateout%gq0(1:ngptc,levs:1:-1,ntoz:ntoz), (/nx,ny,levs/))  &
                                        - var3(1:nx,1:ny,1:levs))*rdt
             used=send_data(Diag(idx)%id, var3, Time, is_in=is_in, js_in=js_in, ks_in=1) 
           endif
#endif
         endif
       endif
     enddo


  end subroutine gfdl_diag_output
!
!-------------------------------------------------------------------------
  subroutine store_data(id, work, Time, idx, intpl_method, fldname)
    integer, intent(in)                 :: id
    integer, intent(in)                 :: idx
    real(kind=kind_phys), intent(in)    :: work(ieco-isco+1,jeco-jsco+1)
    type(time_type), intent(in)         :: Time
    character(*), intent(in)            :: intpl_method
    character(*), intent(in)            :: fldname
!
    integer k,j,i,kb
    logical used
!
    if( id > 0 ) then
      if( use_wrtgridcomp_output ) then
        if( trim(intpl_method) == 'bilinear') then
          do j= jsco,jeco
            do i= isco,ieco
              buffer_phys_bl(i,j,nstt(idx)) = work(i-isco+1,j-jsco+1)
            enddo
          enddo
        else if(trim(intpl_method) == 'nearest_stod') then
          do j= jsco,jeco
            do i= isco,ieco
              buffer_phys_nb(i,j,nstt(idx)) = work(i-isco+1,j-jsco+1)
            enddo
          enddo
        else if(trim(intpl_method) == 'vector_bilinear') then
!first save the data
          do j= jsco,jeco
            do i= isco,ieco
              buffer_phys_bl(i,j,nstt(idx)) = work(i-isco+1,j-jsco+1)
            enddo
          enddo
          if( fldname(1:1) == 'u' .or. fldname(1:1) == 'U') then
            if(.not.allocated(uwork)) allocate(uwork(isco:ieco,jsco:jeco))
            do j= jsco,jeco
              do i= isco,ieco
                uwork(i,j) = work(i-isco+1,j-jsco+1)
              enddo
            enddo
            uwindname = fldname
            uwork_set = .true.
          endif
          if( fldname(1:1) == 'v' .or. fldname(1:1) == 'V') then
!set up wind vector
            if( uwork_set .and. trim(uwindname(2:)) == trim(fldname(2:))) then
              do j= jsco,jeco
                do i= isco,ieco
                   buffer_phys_windvect(1,i,j,nstt_vctbl(idx)) = uwork(i,j)*cos(lon(i,j)) - work(i-isco+1,j-jsco+1)*sin(lat(i,j))*sin(lon(i,j))
                   buffer_phys_windvect(2,i,j,nstt_vctbl(idx)) = uwork(i,j)*sin(lon(i,j)) + work(i-isco+1,j-jsco+1)*sin(lat(i,j))*cos(lon(i,j))
                   buffer_phys_windvect(3,i,j,nstt_vctbl(idx)) = work(i-isco+1,j-jsco+1)*cos(lat(i,j))
                enddo
              enddo
            endif
            uwork = 0.
            uwindname = ''
            uwork_set = .false.
          endif

        endif
      else
        used = send_data(id, work, Time)
      endif
    endif
!
 end subroutine store_data
!-------------------------------------------------------------------------
!
#ifdef use_WRTCOMP

 subroutine fv_phys_bundle_setup(axes, phys_bundle, fcst_grid, quilting, nbdlphys)
!
!-------------------------------------------------------------
!*** set esmf bundle for dyn output fields
!------------------------------------------------------------
!
   use esmf
   use diag_data_mod, ONLY:  diag_atttype
!
   implicit none
!
   integer, intent(in)                         :: axes(:)
   type(ESMF_FieldBundle),intent(inout)        :: phys_bundle(:)
   type(ESMF_Grid),intent(inout)               :: fcst_grid
   logical,intent(in)                          :: quilting
   integer, intent(in)                         :: nbdlphys
!
!*** local variables
   integer i, j, k, n, rc, idx, ibdl, nbdl
   integer num_axes, id, axis_length, direction, edges, axis_typ
   integer num_attributes, num_field_dyn
   integer currdate(6)
   character(2) axis_id
   character(255)    :: axis_name, units, long_name, cart_name, axesname
   character(128)    :: output_name, physbdl_name, outputfile1
   logical           :: lput2physbdl, loutputfile, l2dvector
   type(domain1d)    :: Domain
   type(domainUG)    :: DomainU
   type(ESMF_Field)  :: field
   real,dimension(:),allocatable               :: axis_data
   character(128),dimension(:), allocatable    :: bdl_intplmethod, outputfile
   type(diag_atttype),dimension(:),allocatable :: attributes
   real(4),dimension(:,:),pointer              :: dataPtr2d
!
!------------------------------------------------------------
!--- use wrte grid component for output
   use_wrtgridcomp_output = quilting
!   if(mpp_pe()==mpp_root_pe())print *,'in fv_phys bundle,use_wrtgridcomp_output=',use_wrtgridcomp_output, &
!   print *,'in fv_phys bundle,use_wrtgridcomp_output=',use_wrtgridcomp_output, &
!       'isco=',isco,ieco,'jsco=',jsco,jeco,'tot_diag_idx=',tot_diag_idx
!
!------------------------------------------------------------
!*** add attributes to the bundle such as subdomain limtis,
!*** axes, output time, etc
!------------------------------------------------------------
!
   allocate(bdl_intplmethod(nbdlphys), outputfile(nbdlphys))
   if(mpp_pe()==mpp_root_pe())print *,'in fv_phys bundle,nbdl=',nbdlphys
   do ibdl = 1, nbdlphys
     loutputfile = .false.
     call ESMF_FieldBundleGet(phys_bundle(ibdl), name=physbdl_name,rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     idx = index(physbdl_name,'_bilinear')
     if(idx > 0) then
       outputfile(ibdl) = physbdl_name(1:idx-1)
       bdl_intplmethod(ibdl) = 'bilinear'
       loutputfile = .true.
     endif
     idx = index(physbdl_name,'_nearest_stod')
     if(idx > 0) then
       outputfile(ibdl) = physbdl_name(1:idx-1)
       bdl_intplmethod(ibdl) = 'nearest_stod'
       loutputfile = .true.
     endif
     if( .not. loutputfile) then
       outputfile(ibdl) = 'phy'
       bdl_intplmethod(ibdl) = 'nearest_stod'
     endif
!    print *,'in fv_phys bundle,i=',ibdl,'outputfile=',trim(outputfile(ibdl)), &
!      'bdl_intplmethod=',trim(bdl_intplmethod(ibdl))

     call ESMF_AttributeAdd(phys_bundle(ibdl), convention="NetCDF", purpose="FV3", &
       attrList=(/"fhzero", "ncld", "nsoil", "imp_physics", "dtp", "lprecip_accu"/), rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call ESMF_AttributeSet(phys_bundle(ibdl), convention="NetCDF", purpose="FV3", &
       name="fhzero", value=fhzero, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call ESMF_AttributeSet(phys_bundle(ibdl), convention="NetCDF", purpose="FV3", &
       name="ncld", value=ncld, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call ESMF_AttributeSet(phys_bundle(ibdl), convention="NetCDF", purpose="FV3", &
       name="nsoil", value=nsoil, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call ESMF_AttributeSet(phys_bundle(ibdl), convention="NetCDF", purpose="FV3", &
       name="imp_physics", value=imp_physics, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call ESMF_AttributeSet(phys_bundle(ibdl), convention="NetCDF", purpose="FV3", &
       name="dtp", value=dtp, rc=rc)
!     print *,'in fcst gfdl diag, dtp=',dtp,' ibdl=',ibdl
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call ESMF_AttributeSet(phys_bundle(ibdl), convention="NetCDF", purpose="FV3", &
       name="lprecip_accu", value=trim(Sprecip_accu), rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

!end ibdl
   enddo
!
!*** add attributes (for phys, set axes to 2)
   num_axes = 2
   do id = 1,num_axes
     axis_length =  get_axis_global_length(axes(id))
     allocate(axis_data(axis_length))
     call get_diag_axis( axes(id), axis_name, units, long_name, cart_name, &
                         direction, edges, Domain, DomainU, axis_data,     &
                         num_attributes=num_attributes,              &
                         attributes=attributes)
!
     deallocate(axis_data)
   enddo
!
!-----------------------------------------------------------------------------------------
!*** add esmf fields
!
   do idx= 1,tot_diag_idx

     lput2physbdl = .false.
     do ibdl = 1, nbdlphys

       if( index(trim(Diag(idx)%intpl_method),trim(bdl_intplmethod(ibdl))) > 0) then
         lput2physbdl = .true.
         if( Diag(idx)%id > 0 ) then
           call find_output_name(trim(Diag(idx)%mod_name),trim(Diag(idx)%name),output_name)

!add origin field
           call add_field_to_phybundle(trim(output_name),trim(Diag(idx)%desc),trim(Diag(idx)%unit), "time: point", &
             axes(1:Diag(idx)%axes), fcst_grid, nstt(idx),phys_bundle(ibdl), outputfile(ibdl),   &
             bdl_intplmethod(ibdl), rcd=rc)
!           if( mpp_root_pe()==0) print *,'phys, add field,',trim(Diag(idx)%name),'idx=',idx,'ibdl=',ibdl
!
           if( index(trim(Diag(idx)%intpl_method), "vector") > 0) then
             l2dvector = .true.
             if (nstt_vctbl(idx) > 0) then
               output_name = 'wind'//trim(output_name)//'vector'
               outputfile1 = 'none'
               call add_field_to_phybundle(trim(output_name),trim(Diag(idx)%desc),trim(Diag(idx)%unit), "time: point", &
                 axes(1:Diag(idx)%axes), fcst_grid, nstt_vctbl(idx),phys_bundle(ibdl), outputfile1, &
                 bdl_intplmethod(ibdl),l2dvector=l2dvector,  rcd=rc)
!               if( mpp_root_pe()==0) print *,'in phys, add vector field,',trim(Diag(idx)%name),' idx=',idx,' ibdl=',ibdl
             endif
           endif

         endif
       endif
     enddo
     if( .not. lput2physbdl ) then
         if( mpp_root_pe()==0) print *,'WARNING: not matching interpolation method, field ',trim(Diag(idx)%name), &
           ' is not added to phys bundle '
     endif

   enddo

 end subroutine fv_phys_bundle_setup
!
!-----------------------------------------------------------------------------------------
 subroutine add_field_to_phybundle(var_name,long_name,units,cell_methods, axes,phys_grid, &
                                   kstt,phys_bundle,output_file,intpl_method,range,l2dvector,rcd)
!
   use esmf
!
   implicit none

   character(*), intent(in)             :: var_name, long_name, units, cell_methods
   character(*), intent(in)             :: output_file, intpl_method
   integer, intent(in)                  :: axes(:)
   type(esmf_grid), intent(in)          :: phys_grid
   integer, intent(in)                  :: kstt
   type(esmf_fieldbundle),intent(inout) :: phys_bundle
   real, intent(in), optional           :: range(2)
   logical, intent(in), optional        :: l2dvector
   integer, intent(out), optional       :: rcd
!
!*** local variable
   type(ESMF_Field)         :: field
   type(ESMF_DataCopy_Flag) :: copyflag=ESMF_DATACOPY_REFERENCE
   integer rc
   real(4),dimension(:,:),pointer   :: temp_r2d
   real(4),dimension(:,:,:),pointer :: temp_r3d
!
!*** create esmf field
   if( present(l2dvector) .and. l2dvector ) then
     temp_r3d => buffer_phys_windvect(1:3,isco:ieco,jsco:jeco,kstt)
!     if( mpp_root_pe() == 0) print *,'phys, create wind vector esmf field'
     call ESMF_LogWrite('bf create winde vector esmf field '//trim(var_name), ESMF_LOGMSG_INFO, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!datacopyflag=ESMF_DATACOPY_VALUE, &
     field = ESMF_FieldCreate(phys_grid, temp_r3d, datacopyflag=ESMF_DATACOPY_REFERENCE, &
                            gridToFieldMap=(/2,3/), ungriddedLBound=(/1/), ungriddedUBound=(/3/), &
                            name=var_name, indexFlag=ESMF_INDEX_DELOCAL, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
     call ESMF_LogWrite('af winde vector esmf field create '//trim(var_name), ESMF_LOGMSG_INFO, rc=rc)

     call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"output_file"/), rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
     call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='output_file',value=trim(output_file),rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
     call ESMF_LogWrite('before winde vector esmf field add output_file', ESMF_LOGMSG_INFO, rc=rc)

!     if( mpp_root_pe() == 0)print *,'phys, aftercreate wind vector esmf field'
     call ESMF_FieldBundleAdd(phys_bundle,(/field/), rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
     if( present(rcd)) rcd=rc
     call ESMF_LogWrite('aft winde vector esmf field add to fieldbundle'//trim(var_name), ESMF_LOGMSG_INFO, rc=rc)
     return
   else if( trim(intpl_method) == 'nearest_stod' ) then
     temp_r2d => buffer_phys_nb(isco:ieco,jsco:jeco,kstt)
     field = ESMF_FieldCreate(phys_grid, temp_r2d, datacopyflag=copyflag, &
                            name=var_name, indexFlag=ESMF_INDEX_DELOCAL, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     if( mpp_root_pe() == 0) print *,'add field to after nearest_stod, fld=', trim(var_name)
   else if( trim(intpl_method) == 'bilinear' ) then
     temp_r2d => buffer_phys_bl(isco:ieco,jsco:jeco,kstt)
     field = ESMF_FieldCreate(phys_grid, temp_r2d, datacopyflag=copyflag, &
                            name=var_name, indexFlag=ESMF_INDEX_DELOCAL, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
!     if( mpp_root_pe() == 0) print *,'add field to after bilinear, fld=', trim(var_name)
   endif
!
!*** add field attributes
   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"long_name"/), rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='long_name',value=trim(long_name),rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"units"/), rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='units',value=trim(units),rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"missing_value"/), rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='missing_value',value=missing_value,rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"_FillValue"/), rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='_FillValue',value=missing_value,rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"cell_methods"/), rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='cell_methods',value=trim(cell_methods),rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"output_file"/), rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     call ESMF_Finalize(endflag=ESMF_END_ABORT)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='output_file',value=trim(output_file),rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     call ESMF_Finalize(endflag=ESMF_END_ABORT)

!*** add field into bundle
   call ESMF_FieldBundleAdd(phys_bundle,(/field/), rc=rc)
   if( present(rcd)) rcd=rc
!
   call ESMF_LogWrite('phys field add to fieldbundle'//trim(var_name), ESMF_LOGMSG_INFO, rc=rc)

 end subroutine add_field_to_phybundle
!
!
 subroutine find_output_name(module_name,field_name,output_name)
   character(*), intent(in)     :: module_name
   character(*), intent(in)     :: field_name
   character(*), intent(out)    :: output_name
!
   integer i,in_num, out_num
   integer tile_count
!
   tile_count=1
   in_num = find_input_field(module_name, field_name, tile_count)
!
   output_name=''
   do i=1, max_output_fields
     if(output_fields(i)%input_field == in_num) then
       output_name=output_fields(i)%output_name
       exit
     endif
   enddo
   if(output_name=='') then
     print *,'Error, cant find out put name'
   endif

 end subroutine find_output_name
#endif
!-------------------------------------------------------------------------      

end module FV3GFS_io_mod
