!$$$  Module Documentation Block
!
! Module: nuopc_physics
! Author: Patrick Tripp - NOAA/NCEP/EMC
! Email: patrick.tripp@noaa.gov
! Abstract: Wrapper for GFS physics
! History Log: Tripp - 5/2015 - Initial release
!
! Public Variables:
!   
!       Derived Data Types (DDTs)
!       state_fields_in        ! basic inputs of radiation and physics parameters
!       state_fields_out       ! basic outputs from radiation and physics
!       sfc_properties         ! surface fields
!       diagnostics            ! fields typically only used for diagnostic output
!       cloud_properties       ! cloud data and parameters
!       radiation_tendencies   ! radiation data
!       model_parameters       ! non-changing model parameters - set once in nuopc_phys_init
!       interface_fields       ! data used for coupling (e.g. land and ocean)
!       dynamic_parameters     ! data that changes frequently (e.g. inner loops)
!       tbd_ddt                ! to be determined data that has not been pigeonholed
!
!       use_nuopc              ! flag to use this driver
!
! Public Subroutines:
!       nuopc_phys_init        ! initialize routine, run once at beginning to set model options, etc.
!       nuopc_phys_run         ! wrapper for gbphys - the core physics component
!       nuopc_rad_run          ! wrapper for grrad - the main radiation driver
!       nuopc_rad_update       ! wrapper for radupdate - update values between timesteps, solar, etc.
!
!       DDT type bound procedures
!       [DDT]%set              ! define scalar values and assign pointers to array data
!       [DDT]%setrad           ! set the items required by radiation
!       [DDT]%setphys          ! set the items required by physics
!       [DDT]%print            ! for printing debug output - only partially implemented
!
!       ! Subroutines for standalone test driver
!       phys_init_savein       ! write/read inputs to nuopc_phys_init
!       phys_init_readin
!
!       phys_run_savein        ! write/read inputs to nuopc_phys_run
!       phys_run_readin
!       phys_run_saveout       ! write/read outputs from nuopc_phys_run
!       phys_run_readout
!
!       rad_run_savein         ! write/read inputs to nuopc_rad_run
!       rad_run_readin
!       rad_run_saveout        ! write/read outputs from nuopc_rad_ru
!       rad_run_readout
!
! Private
!       dbgprint               ! Simple debug print subroutine

       module nuopc_physics

       use machine, only: kind_phys
       use physcons, only: dxmax, dxmin, dxinv     ! lon lat dependant variables set in initialize
 

       use module_radiation_driver,  only : grrad, radupdate
       use module_radsw_parameters,  only : topfsw_type, sfcfsw_type
       use module_radlw_parameters,  only : topflw_type, sfcflw_type

       implicit none

       private

       ! Derived Data Types
       public :: state_fields_in        ! basic inputs of radiation and physics parameters
       public :: state_fields_out       ! basic outputs from radiation and physics
       public :: sfc_properties         ! surface fields
       public :: diagnostics            ! fields typically only used for diagnostic output
       public :: cloud_properties       ! cloud data and parameters
       public :: radiation_tendencies   ! radiation data
       public :: model_parameters       ! non-changing model parameters - set once in initialize
       public :: interface_fields       ! data used for coupling (e.g. land and ocean)
       public :: dynamic_parameters     ! data that changes frequently (e.g. inner loops)
       public :: tbd_ddt                ! to be determined data that has not been pigeonholed

       public :: use_nuopc              ! flag to use this driver

       ! Main Subroutines
       public :: nuopc_phys_init        ! initialize routine
       public :: nuopc_phys_run         ! wrapper for gbphys
       public :: nuopc_rad_run          ! wrapper for grrad
       public :: nuopc_rad_update       ! wrapper for radupdate - updates some fields between timesteps


       ! Subroutines for standalone test driver
       public :: phys_init_savein       ! write/read inputs to nuopc_phys_init
       public :: phys_init_readin

       public :: phys_run_savein        ! write/read inputs to nuopc_phys_run
       public :: phys_run_readin
       public :: phys_run_saveout       ! write/read outputs from nuopc_phys_run
       public :: phys_run_readout

       public :: rad_run_savein         ! write/read inputs to nuopc_rad_run
       public :: rad_run_readin
       public :: rad_run_saveout        ! write/read outputs from nuopc_rad_ru
       public :: rad_run_readout


       integer :: myme                              ! My mpi mpe - set in initialize
       ! logical, parameter :: debug = .true.         ! Flag toggle for debug print output
       logical, parameter :: debug = .false.
       ! logical, parameter :: use_nuopc = .false.
       logical, parameter :: use_nuopc = .true.     ! Flag to use this wrapper

!       type local_datatype
!         private
!         integer :: myme                            ! My mpi mpe - set in initialize
!         character(len=4), public :: mdl_selector = 'gsm'
!         ! CHARACTER*(*) arg !declares a dummy argument
!       end type
!
!       ! Local data instances
!       type(local_datatype) :: local_data



!******************************************
! Type tbd_ddt
!******************************************
       type tbd_ddt

         private

! In
         real (kind=kind_phys), pointer :: dpshc (:) => null()     !              maximum pressure depth for shallow convection
         real (kind=kind_phys), pointer :: prdout(:,:,:) => null() ! ozplout_v    ozone forcing data
         real (kind=kind_phys), pointer :: poz(:) => null()        ! pl_pres      ozone forcing data level pressure (ln(Pa))
         real (kind=kind_phys), pointer :: rann(:,:) => null()     !              random number array (0-1)
         real (kind=kind_phys) :: xkzm_m                           ! bkgd_vdif_m  background vertical diffusion for momentum
         real (kind=kind_phys) :: xkzm_h                           ! bkgd_vdif_h  background vertical diffusion for heat q
         real (kind=kind_phys) :: xkzm_s                           ! bkgd_vdif_s  sigma threshold for background mom. diffusn
         real (kind=kind_phys) :: psautco(2)                       ! auto conversion coeff from ice to snow
         real (kind=kind_phys) :: prautco(2)                       ! auto conversion coeff from cloud to rain
         real (kind=kind_phys) :: evpco                            ! coeff for evaporation of largescale rain
         real (kind=kind_phys) :: wminco(2)                        ! water and ice minimum threshold for Zhao

! Moved to dyn_param
!         ! These might be able to be computed elsewhere
!         real (kind=kind_phys) :: slag  ! equation of time ( radian )
!         real (kind=kind_phys) :: sdec  ! sin of the solar declination angle
!         real (kind=kind_phys) :: cdec  ! cos of the solar declination angle

! In/Out
         real (kind=kind_phys), pointer :: acv (:) => null()        ! array containing accumulated convective clouds
         real (kind=kind_phys), pointer :: acvb(:) => null()        ! arrays used by cnvc90 bottom
         real (kind=kind_phys), pointer :: acvt(:) => null()        ! arrays used by cnvc9 top
         real (kind=kind_phys), pointer :: slc(:,:) => null()       ! liquid soil moisture
         real (kind=kind_phys), pointer :: smc(:,:) => null()       ! total soil moisture
         real (kind=kind_phys), pointer :: stc(:,:) => null()       ! soil temperature
         real (kind=kind_phys), pointer :: upd_mf(:,:) => null()    ! convective updraft mass flux
         real (kind=kind_phys), pointer :: dwn_mf(:,:) => null()    ! convective downdraft mass flux
         real (kind=kind_phys), pointer :: det_mf(:,:) => null()    ! convective detrainment mass flux
         real (kind=kind_phys), pointer :: phy_f3d(:,:,:) => null() ! 3d arrays saved for restart
         real (kind=kind_phys), pointer :: phy_f2d(:,:) => null()   ! 2d arrays save for restart
         real (kind=kind_phys), pointer :: tprcp (:) => null()      ! sfc_fld%tprcp   total precipitation
         real (kind=kind_phys), pointer :: srflag(:) => null()      ! sfc_fld%srflag  snow/rain flag for precipitation

! Out
         real (kind=kind_phys), pointer :: tref(:) => null()  ! nst_fld%Tref  Reference Temperature
         real (kind=kind_phys), pointer :: z_c(:) => null()   ! nst_fld%z_c   Sub layer cooling thickness
         real (kind=kind_phys), pointer :: c_0(:) => null()   ! nst_fld%c_0   coefficient1 to calculate d(Tz)/d(Ts)
         real (kind=kind_phys), pointer :: c_d(:) => null()   ! nst_fld%c_d   coefficient2 to calculate d(Tz)/d(Ts)
         real (kind=kind_phys), pointer :: w_0(:) => null()   ! nst_fld%w_0   coefficient3 to calculate d(Tz)/d(Ts)
         real (kind=kind_phys), pointer :: w_d(:) => null()   ! nst_fld%w_d   coefficient4 to calculate d(Tz)/d(Ts)

         contains
           procedure set => tbd_set

       end type
!******************************************


!! Type example complex derived data type
!
!       type model_data_in
!         private
!           real :: vara
!           real :: varb
!       end type
!
!       type model_data
!         private
!
!         type (model_data_in) :: data_in
!         type (model_data_out) :: data_out
!         type (model_data_inout) :: data_inout
!
!         contains
!           procedure setin => set_model_in
!           procedure setout => set_model_out
!           procedure setinout => set_model_inout 
!
!       end type

!! Type phys_restart
!       type phys_restart
!         private
!
!         integer :: dim1, dim2
!         real (kind=kind_phys), public, pointer :: restart_data (:,:)   ! Contains what exactly?
!                                                                        ! Data needed between timesteps to physics
!       end type




!******************************************
! Type state_fields in and out
!******************************************
       type state_fields_in

         private

         !! Inputs (also in rad)
         real (kind=kind_phys), pointer :: prsi  (:,:) => null()    ! model level pressure in Pa                     !
         real (kind=kind_phys), pointer :: prsl  (:,:) => null()    ! model layer mean pressure Pa                   !
         real (kind=kind_phys), pointer :: prslk (:,:) => null()    ! exner function = (p/p0)**rocp                  !
         real (kind=kind_phys), pointer :: tgrs  (:,:) => null()    ! model layer mean temperature in k              !

         ! Only in grrad
         real (kind=kind_phys), pointer :: qgrs_rad(:,:) => null()  ! layer specific humidity in gm/gm : qgrs in gloopr
         real (kind=kind_phys), pointer :: tracer(:,:,:) => null()  ! layer prognostic tracer amount/mixing-ratio    !
                                                                    ! incl: oz, cwc, aeros, etc.                     !
         real (kind=kind_phys), pointer :: vvl(:,:)      => null()  ! layer mean vertical velocity in pa/sec         !

         !! Inputs only in phys
         real (kind=kind_phys), pointer :: pgr   (:)     => null()  ! surface pressure (Pa) real                       
         real (kind=kind_phys), pointer :: ugrs  (:,:)   => null()  ! u/v component of layer wind              
         real (kind=kind_phys), pointer :: vgrs  (:,:)   => null()  ! u/v component of layer wind              
         real (kind=kind_phys), pointer :: qgrs  (:,:,:) => null()  ! layer mean tracer concentration
         real (kind=kind_phys), pointer :: prsik (:,:)   => null()  ! Exner function at layer
         real (kind=kind_phys), pointer :: phii  (:,:)   => null()  ! interface geopotential height
         real (kind=kind_phys), pointer :: phil  (:,:)   => null()  ! layer geopotential height
         real (kind=kind_phys), pointer :: adjtrc(:)     => null()  ! dynamics adjustments to tracers

         contains
           procedure setrad  => state_fld_setrad_in
           procedure setphys => state_fld_setphys_in
           procedure print   => state_fld_in_print
       end type
!******************************************





!******************************************
! Type state_fields out
!******************************************
       type state_fields_out

         private

         !! Outputs (only in physics)
         real (kind=kind_phys), pointer :: gt0 (:,:)   => null()  ! updated temperature 
         real (kind=kind_phys), pointer :: gq0 (:,:,:) => null()  ! updated tracers
         real (kind=kind_phys), pointer :: gu0 (:,:)   => null()  ! updated zonal wind
         real (kind=kind_phys), pointer :: gv0 (:,:)   => null()  ! updated meridional wind

         contains
           procedure setphys => state_fld_setphys_out
           procedure print => state_fld_out_print
       end type
!******************************************





!******************************************
! Type sfc_properties
!******************************************
       type sfc_properties

         private

         ! Inputs
         real (kind=kind_phys), pointer :: slmsk (:) => null()  ! sea/land mask array (sea:0,land:1,sea-ice:2)   !
         real (kind=kind_phys), pointer :: tsfc  (:) => null()  ! surface temperature in k                       ! same as tsea
         real (kind=kind_phys), pointer :: snowd (:) => null()  ! snow depth water equivalent in mm              ! same as snwdph
         real (kind=kind_phys), pointer :: sncovr(:) => null()  ! snow cover in fraction                         !
         real (kind=kind_phys), pointer :: snoalb(:) => null()  ! maximum snow albedo in fraction                !
         real (kind=kind_phys), pointer :: zorl  (:) => null()  ! surface roughness in cm                        !
         real (kind=kind_phys), pointer :: hprim (:) => null()  ! topographic standard deviation in m            !
         real (kind=kind_phys), pointer :: fice  (:) => null()  ! ice fraction over open water grid              !
         real (kind=kind_phys), pointer :: tisfc (:) => null()  ! surface temperature over ice fraction          !
         real (kind=kind_phys), pointer :: alvsf (:) => null()  ! mean vis albedo with strong cosz dependency    !
         real (kind=kind_phys), pointer :: alnsf (:) => null()  ! mean nir albedo with strong cosz dependency    !
         real (kind=kind_phys), pointer :: alvwf (:) => null()  ! mean vis albedo with weak cosz dependency      !
         real (kind=kind_phys), pointer :: alnwf (:) => null()  ! mean nir albedo with weak cosz dependency      !
         real (kind=kind_phys), pointer :: facsf (:) => null()  ! fractional coverage with strong cosz dependen  !
         real (kind=kind_phys), pointer :: facwf (:) => null()  ! fractional coverage with weak cosz dependency  !


         ! gbphys only
         ! hprim is in rad but only 1D
         ! in gloopb is 2D  hprime_v(ngptc,nmtvr) - Can FIX later
         real (kind=kind_phys), pointer :: hprime2(:,:) => null()  ! orographic std dev
         real (kind=kind_phys), pointer :: slope  (:)   => null()  ! sfc slope type for lsm  
         real (kind=kind_phys), pointer :: shdmin (:)   => null()  ! min fractional coverage of green veg
         real (kind=kind_phys), pointer :: shdmax (:)   => null()  ! max fractnl cover of green veg (not used) 
         real (kind=kind_phys), pointer :: tg3    (:)   => null()  ! deep soil temperature 
         real (kind=kind_phys), pointer :: vfrac  (:)   => null()  ! vegetation fraction 
         real (kind=kind_phys), pointer :: vtype  (:)   => null()  ! vegetation type
         real (kind=kind_phys), pointer :: stype  (:)   => null()  ! soil type 
         real (kind=kind_phys), pointer :: uustar (:)   => null()  ! boundary layer parameter 
         real (kind=kind_phys), pointer :: oro    (:)   => null()  ! orography 
         real (kind=kind_phys), pointer :: oro_uf (:)   => null()  ! unfiltered orography

         ! In/Out
         real (kind=kind_phys), pointer :: hice  (:) => null()   ! sea ice thickness
         real (kind=kind_phys), pointer :: weasd (:) => null()   ! sheleg  water equiv of accumulated snow depth (kg/m**2) over land and sea ice
         real (kind=kind_phys), pointer :: canopy(:) => null()   ! canopy water
         real (kind=kind_phys), pointer :: ffmm  (:) => null()   ! fm parameter from PBL scheme
         real (kind=kind_phys), pointer :: ffhh  (:) => null()   ! fh parameter from PBL scheme
         real (kind=kind_phys), pointer :: f10m  (:) => null()   ! fm at 10m - Ratio of sigma level 1 wind and 10m wind

         ! Outputs
         real (kind=kind_phys), pointer :: t2m(:) => null()   ! 2 meter temperature 
         real (kind=kind_phys), pointer :: q2m(:) => null()   ! 2 meter humidity 

         contains
           procedure setrad  => sfc_prop_setrad
           procedure setphys => sfc_prop_setphys
           !procedure print => sfc_prop_print
       end type
!******************************************





!******************************************
! Type diagnostics
!******************************************
       type diagnostics

         private

         integer :: NFXR                                           ! second dimension of input/output array fluxr   !

         !! Input/Output only in radiation
         real (kind=kind_phys), pointer :: fluxr (:,:) => null()   ! to save time accumulated 2-d fields defined as:!
                                                                   ! hardcoded field indices, opt. includes aerosols!

         real (kind=kind_phys), pointer :: dswcmp(:,:) => null()
         real (kind=kind_phys), pointer :: uswcmp(:,:) => null()

         type (topfsw_type), pointer :: topfsw(:) => null()     ! sw radiation fluxes at toa, components:        !
                                         !       %upfxc           - total sky upward sw flux at toa (w/m**2)     !
                                         !       %dnflx           - total sky downward sw flux at toa (w/m**2)   !
                                         !       %upfx0           - clear sky upward sw flux at toa (w/m**2)     !

         type (topflw_type), pointer :: topflw(:) => null()      ! lw radiation fluxes at top, component:        !
                                         !       %upfxc           - total sky upward lw flux at toa (w/m**2)     !
                                         !       %upfx0           - clear sky upward lw flux at toa (w/m**2)     !

         ! Input/output - used by physics
         real (kind=kind_phys), pointer :: srunoff(:)  => null()  ! flx_fld%srunoff  surface water runoff (from lsm)
         real (kind=kind_phys), pointer :: evbsa  (:)  => null()  ! flx_fld%evbsa    noah lsm diagnostics
         real (kind=kind_phys), pointer :: evcwa  (:)  => null()  ! flx_fld%evcwa    noah lsm diagnostics
         real (kind=kind_phys), pointer :: snohfa (:)  => null()  ! flx_fld%snohfa   noah lsm diagnostics
         real (kind=kind_phys), pointer :: transa (:)  => null()  ! flx_fld%transa   noah lsm diagnostics
         real (kind=kind_phys), pointer :: sbsnoa (:)  => null()  ! flx_fld%sbsnoa   noah lsm diagnostics
         real (kind=kind_phys), pointer :: snowca (:)  => null()  ! flx_fld%snowca   noah lsm diagnostics
         real (kind=kind_phys), pointer :: soilm  (:)  => null()  ! flx_fld%soilm    soil moisture
         real (kind=kind_phys), pointer :: tmpmin (:)  => null()  ! flx_fld%tmpmin   min temperature at 2m height (k)
         real (kind=kind_phys), pointer :: tmpmax (:)  => null()  ! flx_fld%tmpmax   max temperature at 2m height (k)
         real (kind=kind_phys), pointer :: dusfc  (:)  => null()  ! flx_fld%dusfc    u component of surface stress
         real (kind=kind_phys), pointer :: dvsfc  (:)  => null()  ! flx_fld%dvsfc    v component of surface stress
         real (kind=kind_phys), pointer :: dtsfc  (:)  => null()  ! flx_fld%dtsfc    sensible heat flux (w/m2)
         real (kind=kind_phys), pointer :: dqsfc  (:)  => null()  ! flx_fld%dqsfc    latent heat flux (w/m2)
         real (kind=kind_phys), pointer :: totprcp(:)  => null()  ! flx_fld%geshem   accumulated total precipitation (kg/m2)
         real (kind=kind_phys), pointer :: gflux  (:)  => null()  ! flx_fld%gflux    groud conductive heat flux
         real (kind=kind_phys), pointer :: dlwsfc (:)  => null()  ! flx_fld%dlwsfc   time accumulated sfc dn lw flux ( w/m**2 )
         real (kind=kind_phys), pointer :: ulwsfc (:)  => null()  ! flx_fld%ulwsfc   time accumulated sfc up lw flux ( w/m**2 )
         real (kind=kind_phys), pointer :: suntim (:)  => null()  ! flx_fld%suntim   sunshine duration time (s)
         real (kind=kind_phys), pointer :: runoff (:)  => null()  ! flx_fld%runoff   total water runoff
         real (kind=kind_phys), pointer :: ep     (:)  => null()  ! flx_fld%ep       potential evaporation
         real (kind=kind_phys), pointer :: cldwrk (:)  => null()  ! flx_fld%cldwrk   cloud workfunction (valid only with sas)
         real (kind=kind_phys), pointer :: dugwd  (:)  => null()  ! flx_fld%dugwd    vertically integrated u change by OGWD
         real (kind=kind_phys), pointer :: dvgwd  (:)  => null()  ! flx_fld%dvgwd    vertically integrated v change by OGWD
         real (kind=kind_phys), pointer :: psmean (:)  => null()  ! flx_fld%psmean   surface pressure (kPa)
         real (kind=kind_phys), pointer :: cnvprcp(:)  => null()  ! flx_fld%bengsh   accumulated convective precipitation (kg/m2)
         real (kind=kind_phys), pointer :: spfhmin(:)  => null()  ! flx_fld%spfhmin  minimum specific humidity
         real (kind=kind_phys), pointer :: spfhmax(:)  => null()  ! flx_fld%spfhmax  maximum specific humidity
         real (kind=kind_phys), pointer :: rain   (:)  => null()  ! flx_fld%rain     total rain at this time step
         real (kind=kind_phys), pointer :: rainc  (:)  => null()  ! flx_fld%rainc    convective rain at this time step

         real (kind=kind_phys), pointer :: dt3dt (:,:,:) => null()  ! ix,levs,6            temperature change due to physics
         real (kind=kind_phys), pointer :: dq3dt (:,:,:) => null()  ! ix,levs,5+pl_coeff   moisture change due to physics
         real (kind=kind_phys), pointer :: du3dt (:,:,:) => null()  ! ix,levs,4            u momentum change due to physics
         real (kind=kind_phys), pointer :: dv3dt (:,:,:) => null()  ! ix,levs,4            v momentum change due to physics
         real (kind=kind_phys), pointer :: dqdt_v(:,:)   => null()  ! ix,levs              total moisture tendency (kg/kg/s)

         ! Output - only in physics
         real (kind=kind_phys), pointer :: u10m   (:) => null()  ! flx_fld%u10m     10 meater u/v wind speed
         real (kind=kind_phys), pointer :: v10m   (:) => null()  ! flx_fld%v10m     10 meater u/v wind speed
         real (kind=kind_phys), pointer :: zlvl   (:) => null()  ! flx_fld%zlvl     layer 1 height (m)
         real (kind=kind_phys), pointer :: psurf  (:) => null()  ! flx_fld%psurf    surface pressure (Pa)
         real (kind=kind_phys), pointer :: hpbl   (:) => null()  ! flx_fld%hpbl     pbl height (m)
         real (kind=kind_phys), pointer :: pwat   (:) => null()  ! flx_fld%pwat     precipitable water
         real (kind=kind_phys), pointer :: t1     (:) => null()  ! flx_fld%t1       layer 1 temperature (K)
         real (kind=kind_phys), pointer :: q1     (:) => null()  ! flx_fld%q1       layer 1 specific humidity (kg/kg)
         real (kind=kind_phys), pointer :: u1     (:) => null()  ! flx_fld%u1       layer 1 zonal wind (m/s)
         real (kind=kind_phys), pointer :: v1     (:) => null()  ! flx_fld%v1       layer 1 merdional wind (m/s)
         real (kind=kind_phys), pointer :: chh    (:) => null()  ! flx_fld%chh      thermal exchange coefficient
         real (kind=kind_phys), pointer :: cmm    (:) => null()  ! flx_fld%cmm      momentum exchange coefficient
         real (kind=kind_phys), pointer :: dlwsfci(:) => null()  ! flx_fld%dlwsfci  instantaneous sfc dnwd lw flux ( w/m**2 )
         real (kind=kind_phys), pointer :: ulwsfci(:) => null()  ! flx_fld%ulwsfci  instantaneous sfc upwd lw flux ( w/m**2 )
         real (kind=kind_phys), pointer :: dswsfci(:) => null()  ! flx_fld%dswsfci  instantaneous sfc dnwd sw flux ( w/m**2 )
         real (kind=kind_phys), pointer :: uswsfci(:) => null()  ! flx_fld%uswsfci  instantaneous sfc upwd sw flux ( w/m**2 )
         real (kind=kind_phys), pointer :: dusfci (:) => null()  ! flx_fld%dusfci   instantaneous u component of surface stress
         real (kind=kind_phys), pointer :: dvsfci (:) => null()  ! flx_fld%dvsfci   instantaneous v component of surface stress
         real (kind=kind_phys), pointer :: dtsfci (:) => null()  ! flx_fld%dtsfci   instantaneous sfc sensible heat flux
         real (kind=kind_phys), pointer :: dqsfci (:) => null()  ! flx_fld%dqsfci   instantaneous sfc latent heat flux
         real (kind=kind_phys), pointer :: gfluxi (:) => null()  ! flx_fld%gfluxi   instantaneous sfc ground heat flux
         real (kind=kind_phys), pointer :: epi    (:) => null()  ! flx_fld%epi      instantaneous sfc potential evaporation
         real (kind=kind_phys), pointer :: smcwlt2(:) => null()  ! flx_fld%smcwlt2  wilting point (volumetric)
         real (kind=kind_phys), pointer :: smcref2(:) => null()  ! flx_fld%smcref2  soil moisture threshold (volumetric)
         real (kind=kind_phys), pointer :: wet1   (:) => null()  ! flx_fld%wet1     normalized soil wetness
         real (kind=kind_phys), pointer :: sr     (:) => null()  ! flx_fld%sr       snow ratio : ratio of snow to total precipitation

         contains
           procedure setrad => diagnostics_setrad
           procedure setphys => diagnostics_setphys
           !procedure print => diagnostics_print
       end type





!******************************************
! Type interface_fields
!******************************************
       type interface_fields

         private

         ! Radiation output only
         type (sfcfsw_type), pointer :: sfcfsw(:) => null()      ! sw radiation fluxes at sfc, components:        !
                                !       (check module_radsw_parameters for definition)   !
                                !       %upfxc           - total sky upward sw flux at sfc (w/m**2)     !
                                !       %dnfxc           - total sky downward sw flux at sfc (w/m**2)   !
                                !       %upfx0           - clear sky upward sw flux at sfc (w/m**2)     !
                                !       %dnfx0           - clear sky downward sw flux at sfc (w/m**2)   !

         type (sfcflw_type), pointer :: sfcflw(:) => null()      ! lw radiation fluxes at sfc, component:         !
                                !                        (check module_radlw_paramters for definition)  !
                                !       %upfxc           - total sky upward lw flux at sfc (w/m**2)     !
                                !       %upfx0           - clear sky upward lw flux at sfc (w/m**2)     !
                                !       %dnfxc           - total sky downward lw flux at sfc (w/m**2)   !
                                !       %dnfx0           - clear sky downward lw flux at sfc (w/m**2)   !

         !! Optional radiation output variables: - Used by WAM
         real (kind=kind_phys), pointer :: htrswb(:,:,:) => null()  ! Spectral heating rate
         real (kind=kind_phys), pointer :: htrlwb(:,:,:) => null()  ! Spectral heating rate

         !! Optional radiation output variables:
         real (kind=kind_phys), pointer :: htrlw0(:,:) => null()  ! NEW
         real (kind=kind_phys), pointer :: htrsw0(:,:) => null()  ! NEW

         ! Inputs - physics only
         real (kind=kind_phys), pointer :: sfcdsw(:) => null()  ! flx_fld%sfcdsw  total sky sfc downward sw flux ( w/m**2 )  
         real (kind=kind_phys), pointer :: sfcnsw(:) => null()  ! flx_fld%sfcnsw  total sky sfc netsw flx into ground(w/m**2)
         real (kind=kind_phys), pointer :: sfcdlw(:) => null()  ! flx_fld%sfcdlw  total sky sfc downward lw flux ( w/m**2 )  

         ! Inputs moved from - TBD physics only
         real (kind=kind_phys), pointer :: sfcnirbmu(:) => null()  ! aoi_fld%nirbmui  sfc nir beam sw upward flux (w/m2)
         real (kind=kind_phys), pointer :: sfcnirdfu(:) => null()  ! aoi_fld%nirdfui  sfc nir diff sw upward flux (w/m2)
         real (kind=kind_phys), pointer :: sfcvisbmu(:) => null()  ! aoi_fld%visbmui  sfc uv+vis beam sw upward flux (w/m2)
         real (kind=kind_phys), pointer :: sfcvisdfu(:) => null()  ! aoi_fld%visdfui  sfc uv+vis diff sw upward flux (w/m2)

         ! Inputs moved from diagnostics - physics only
         real (kind=kind_phys), pointer :: sfcnirbmd(:) => null() ! aoi_fld%nirbmdi  sfc nir beam sw downward flux (w/m2)
         real (kind=kind_phys), pointer :: sfcnirdfd(:) => null() ! aoi_fld%nirdfdi  sfc nir diff sw downward flux (w/m2)
         real (kind=kind_phys), pointer :: sfcvisbmd(:) => null() ! aoi_fld%visbmdi  sfc uv+vis beam sw downward flux (w/m2)
         real (kind=kind_phys), pointer :: sfcvisdfd(:) => null() ! aoi_fld%visdfdi  sfc uv+vis diff sw downward flux (w/m2)

         ! Input/Output - physics only
         real (kind=kind_phys), pointer :: dusfc_cpl (:) => null()  ! aoi_fld%dusfc   sfc u momentum flux       for A/O/I coupling
         real (kind=kind_phys), pointer :: dvsfc_cpl (:) => null()  ! aoi_flddvsfc    sfc v momentum flux       for A/O/I coupling
         real (kind=kind_phys), pointer :: dtsfc_cpl (:) => null()  ! aoi_fld%dtsfc   sfc sensible heat flux    for A/O/I coupling
         real (kind=kind_phys), pointer :: dqsfc_cpl (:) => null()  ! aoi_flddqsfc    sfc latent heat flux      for A/O/I coupling
         real (kind=kind_phys), pointer :: dlwsfc_cpl(:) => null()  ! aoi_fld%dlwsfc  sfc dnwd lw flux (w/m**2) for A/O/I coupling
         real (kind=kind_phys), pointer :: dswsfc_cpl(:) => null()  ! aoi_fld%dswsfc  sfc dnwd sw flux (w/m**2) for A/O/I coupling
         real (kind=kind_phys), pointer :: dnirbm_cpl(:) => null()  ! aoi_fld%dnirbm  sfc nir beam dnwd sw rad flux (w/m**2)      
         real (kind=kind_phys), pointer :: dnirdf_cpl(:) => null()  ! aoi_fld%dnirdf  sfc nir diff dnwd sw rad flux (w/m**2)      
         real (kind=kind_phys), pointer :: dvisbm_cpl(:) => null()  ! aoi_fld%dvisbm  sfc uv+vis beam dnwd sw rad flux (w/m**2)   
         real (kind=kind_phys), pointer :: dvisdf_cpl(:) => null()  ! aoi_fld%dvisdf  sfc uv+vis diff dnwd sw rad flux (w/m**2)   
         real (kind=kind_phys), pointer :: rain_cpl  (:) => null()  ! aoi_fld%rain    total precipitation       for A/O/I coupling
         real (kind=kind_phys), pointer :: nlwsfc_cpl(:) => null()  ! aoi_fld%nlwsfc  net dnwd lw flux (w/m**2) for A/O/I coupling
         real (kind=kind_phys), pointer :: nswsfc_cpl(:) => null()  ! aoi_fld%nswsfc  net dnwd sw flux (w/m**2) for A/O/I coupling
         real (kind=kind_phys), pointer :: nnirbm_cpl(:) => null()  ! aoi_fld%nnirbm  net nir beam dnwd sw rad flux (w/m**2)      
         real (kind=kind_phys), pointer :: nnirdf_cpl(:) => null()  ! aoi_fld%nnirdf  net nir diff dnwd sw rad flux (w/m**2)      
         real (kind=kind_phys), pointer :: nvisbm_cpl(:) => null()  ! aoi_fld%nvisbm  net uv+vis beam dnwd sw rad flux (w/m**2)   
         real (kind=kind_phys), pointer :: nvisdf_cpl(:) => null()  ! aoi_fld%nvisdf  net uv+vis diff dnwd sw rad flux (w/m**2)   

         real (kind=kind_phys), pointer :: xt     (:) => null()  ! nst_fld%xt      heat content in DTL                        
         real (kind=kind_phys), pointer :: xs     (:) => null()  ! nst_fld%xs      salinity  content in DTL                   
         real (kind=kind_phys), pointer :: xu     (:) => null()  ! nst_fld%xu      u current content in DTL                   
         real (kind=kind_phys), pointer :: xv     (:) => null()  ! nst_fld%xv      v current content in DTL                   
         real (kind=kind_phys), pointer :: xz     (:) => null()  ! nst_fld%xz      DTL thickness                              
         real (kind=kind_phys), pointer :: zm     (:) => null()  ! nst_fld%zm      MXL thickness                              
         real (kind=kind_phys), pointer :: xtts   (:) => null()  ! nst_fld%xtts    d(xt)/d(ts)                                
         real (kind=kind_phys), pointer :: xzts   (:) => null()  ! nst_fld%xzts    d(xz)/d(ts)                                
         real (kind=kind_phys), pointer :: d_conv (:) => null()  ! nst_fld%d_conv  thickness of Free Convection Layer (FCL)   
         real (kind=kind_phys), pointer :: ifd    (:) => null()  ! nst_fld%ifd     index to start DTM run or not              
         real (kind=kind_phys), pointer :: dt_cool(:) => null()  ! nst_fld%dt_cool Sub layer cooling amount                   
         real (kind=kind_phys), pointer :: Qrain  (:) => null()  ! nst_fld%Qrain   sensible heat flux due to rainfall (watts) 

         ! Outputs - physics only
         real (kind=kind_phys), pointer :: dusfci_cpl (:) => null()  ! aoi_fld%dusfci   sfc u momentum flux at time step AOI cpl 
         real (kind=kind_phys), pointer :: dvsfci_cpl (:) => null()  ! aoi_flddvsfci    sfc v momentum flux at time step AOI cpl 
         real (kind=kind_phys), pointer :: dtsfci_cpl (:) => null()  ! aoi_fld%dtsfci   sfc sensib heat flux at time step AOI cpl
         real (kind=kind_phys), pointer :: dqsfci_cpl (:) => null()  ! aoi_flddqsfci    sfc latent heat flux at time step AOI cpl
         real (kind=kind_phys), pointer :: dlwsfci_cpl(:) => null()  ! aoi_fld%dlwsfci  sfc dnwd lw flux at time step AOI cpl    
         real (kind=kind_phys), pointer :: dswsfci_cpl(:) => null()  ! aoi_fld%dswsfci  sfc dnwd sw flux at time step AOI cpl    
         real (kind=kind_phys), pointer :: dnirbmi_cpl(:) => null()  ! aoi_fld%dnirbmi  sfc nir beam dnwd sw flx rad at time step
         real (kind=kind_phys), pointer :: dnirdfi_cpl(:) => null()  ! aoi_fld%dnirdfi  sfc nir diff dnwd sw flx rad at time step
         real (kind=kind_phys), pointer :: dvisbmi_cpl(:) => null()  ! aoi_fld%dvisbmi  sfc uv+vis beam dnwd sw flx at time step 
         real (kind=kind_phys), pointer :: dvisdfi_cpl(:) => null()  ! aoi_fld%dvisdfi  sfc uv+vis diff dnwd sw flx at time step 
         real (kind=kind_phys), pointer :: nlwsfci_cpl(:) => null()  ! aoi_fld%nlwsfci  net sfc dnwd lw flux at time step AOI cpl
         real (kind=kind_phys), pointer :: nswsfci_cpl(:) => null()  ! aoi_fld%nswsfci  net sfc dnwd sw flux at time step AOI cpl
         real (kind=kind_phys), pointer :: nnirbmi_cpl(:) => null()  ! aoi_fld%nnirbmi  net nir beam dnwd sw flx rad at time step
         real (kind=kind_phys), pointer :: nnirdfi_cpl(:) => null()  ! aoi_fld%nnirdfi  net nir diff dnwd sw flx rad at time step
         real (kind=kind_phys), pointer :: nvisbmi_cpl(:) => null()  ! aoi_fld%nvisbmi  net uv+vis beam dnwd sw flx at time step 
         real (kind=kind_phys), pointer :: nvisdfi_cpl(:) => null()  ! aoi_fld%nvisdfi  net uv+vis diff dnwd sw flx at time step 
         real (kind=kind_phys), pointer :: t2mi_cpl   (:) => null()  ! aoi_fld%t2mi     T2m at time step AOI cpl                 
         real (kind=kind_phys), pointer :: q2mi_cpl   (:) => null()  ! aoi_fld%q2mi     Q2m at time step AOI cpl                 
         real (kind=kind_phys), pointer :: u10mi_cpl  (:) => null()  ! aoi_fld%u10mi    U10m at time step AOI cpl                
         real (kind=kind_phys), pointer :: v10mi_cpl  (:) => null()  ! aoi_fld%v10mi    V10m at time step AOI cpl                
         real (kind=kind_phys), pointer :: tseai_cpl  (:) => null()  ! aoi_fld%tseai    sfc temp at time step AOI cpl            
         real (kind=kind_phys), pointer :: psurfi_cpl (:) => null()  ! aoi_fld%psurfi   sfc pressure at time step AOI cpl        
         real (kind=kind_phys), pointer :: oro_cpl    (:) => null()  ! aoi_fld%oro      orography AOI cpl                        
         real (kind=kind_phys), pointer :: slmsk_cpl  (:) => null()  ! aoi_fld%slimsk   Land/Sea/Ice AOI cpl                     

       contains
         procedure setrad => interface_fld_setrad
         procedure setphys => interface_fld_setphys

       end type





!******************************************
! Type cloud_properties
! ******************************************
       type cloud_properties

         private

         ! grrad in
         ! gbphys inout
         real (kind=kind_phys), pointer :: cv  (:) => null()      ! fraction of convective cloud                   !  phys
         real (kind=kind_phys), pointer :: cvt (:) => null()      ! convective cloud top pressure in pa            !  phys
         real (kind=kind_phys), pointer :: cvb (:) => null()      ! convective cloud bottom pressure in pa         !  phys  cnvc90

         ! grrad in
         real (kind=kind_phys), pointer :: fcice(:,:) => null()   ! fraction of cloud ice  (in ferrier scheme)     !
         real (kind=kind_phys), pointer :: frain(:,:) => null()   ! fraction of rain water (in ferrier scheme)     !
         real (kind=kind_phys), pointer :: rrime(:,:) => null()   ! mass ratio of total to unrimed ice ( >= 1 )    !

         ! grrad, gbphys in
         real (kind=kind_phys), pointer :: flgmin(:) => null()    ! minimim large ice fraction                     !  phys

         ! grrad in
         real (kind=kind_phys), pointer :: deltaq(:,:) => null()  ! half width of uniform total water distribution
         real (kind=kind_phys), pointer :: cnvw  (:,:) => null()  ! layer convective cloud water
         real (kind=kind_phys), pointer :: cnvc  (:,:) => null()  ! layer convective cloud cover
   
         ! grrad in
         ! gbphys inout
         real (kind=kind_phys), pointer :: sup  => null()         ! supersaturation in pdf cloud when t is very low

         ! gbphys inout
         real (kind=kind_phys), pointer :: cnvqc_v(:,:) => null() ! total convective conensate (kg/kg)
         
         !! grrad out
         real (kind=kind_phys), pointer :: cldcov(:,:) => null()  ! 3-d cloud fraction (IX,levs)                     !

         contains
           procedure setrad => cld_prop_setrad
           procedure setphys => cld_prop_setphys
           !procedure print => cld_prop_print
       end type

!******************************************







!******************************************
! Type radiation_tendencies
!******************************************
       type radiation_tendencies

         private

         !! Outputs from grrad
         real (kind=kind_phys), pointer :: htrsw (:,:) => null()  ! swh  total sky sw heating rate in k/sec
         real (kind=kind_phys), pointer :: htrlw (:,:) => null()  ! hlw  total sky lw heating rate in k/sec
         real (kind=kind_phys), pointer :: sfalb (:)   => null()  ! mean surface diffused sw albedo

         real (kind=kind_phys), pointer :: coszen(:)   => null()  ! mean cos of zenith angle over rad call period
         real (kind=kind_phys), pointer :: tsflw (:)   => null()  ! surface air temp during lw calculation in k
         real (kind=kind_phys), pointer :: semis (:)   => null()  ! surface lw emissivity in fraction

         ! Used only by grrad
         real (kind=kind_phys), pointer :: coszdg(:)   => null()  ! daytime mean cosz over rad call period

         ! Used only by gbphys
         real (kind=kind_phys), pointer :: rqtk (:)     => null()  ! mass change due to moisture variation
         real (kind=kind_phys), pointer :: hlwd (:,:,:) => null()  ! idea sky lw heating rates ( k/s )
         real (kind=kind_phys), pointer :: dtdtr(:,:)   => null()  ! temperature change due to radiative heating per time step (K)

         real (kind=kind_phys), pointer :: swhc (:,:)   => null()  ! clear sky sw heating rates ( k/s ) 
         real (kind=kind_phys), pointer :: hlwc (:,:)   => null()  ! clear sky lw heating rates ( k/s ) 

         contains
           procedure set => rad_tend_set
           !procedure print => rad_tend_print
       end type






!******************************************
! Type dynamic parameters
!******************************************
       type dynamic_parameters

         private

         real (kind=kind_phys), pointer :: xlon  (:) => null()  ! grid longitude in radians, ok for both 0->2pi  !
                                                                ! or -pi -> +pi ranges                           !
         real (kind=kind_phys), pointer :: xlat  (:) => null()  ! grid latitude in radians, default to pi/2 ->   !
                                                                ! -pi/2 range, otherwise adj in subr called      !
         real (kind=kind_phys), pointer :: sinlat(:) => null()  ! sine of the grids corresponding latitudes      !
         real (kind=kind_phys), pointer :: coslat(:) => null()  ! cosine of the grids corresponding latitudes    !
         real (kind=kind_phys)          :: solhr                ! hour time after 00z at the t-step              !

         integer                        :: IX                   ! horizontal dimension - ngptc in gloopb
         integer                        :: IM                   ! number of used points - njeff in gloopb
         integer                        :: kdt                  ! time-step number  
         logical                        :: lssav                ! logical flag for store 3-d cloud field

         ! Radiation only
         integer, pointer :: icsdsw(:) => null()  ! auxiliary cloud control arrays passed to main  !
         integer, pointer :: icsdlw(:) => null()  ! radiations. if isubcsw/isubclw (input to init) !
                                                  ! are set to 2, the arrays contains provided     !
                                                  ! random seeds for sub-column clouds generators  !

         integer               :: jdate(8)  ! current forecast date and time                !
                                            ! (yr, mon, day, t-zone, hr, min, sec, mil-sec)  !
         real (kind=kind_phys) :: solcon    ! solar constant (sun-earth distant adjusted)    !

         real (kind=kind_phys) :: dtlw      ! fhlwr - time duration for lw radiation call in sec
         real (kind=kind_phys) :: dtsw      ! fhswr - time duration for sw radiation call in sec
         logical               :: lsswr     ! logical flags for sw radiation calls
         logical               :: lslwr     ! logical flags for lw radiation calls
         integer               :: ipt       ! index for diagnostic printout point
         logical               :: lprnt     ! control flag for diagnostic print out 
         real (kind=kind_phys) :: deltim    ! timestep in seconds
         ! End Radiation only

         ! These might be able to be computed elsewhere
         real (kind=kind_phys) :: slag  ! equation of time ( radian )
         real (kind=kind_phys) :: sdec  ! sin of the solar declination angle
         real (kind=kind_phys) :: cdec  ! cos of the solar declination angle


         ! Physics only
         integer               :: lat       ! latitude index  used for debug prints
         real (kind=kind_phys) :: dtp       ! physics time step in seconds
         real (kind=kind_phys) :: dtf       ! dynamics time step in seconds
         real (kind=kind_phys) :: clstp     ! index used by cnvc90 (for convective clouds) legacy stuff  does not affect forecast
         integer               :: nnp       ! physics substep number
         integer, pointer      :: nlons(:)  ! number of total grid points in a latitude circle through a point
         real (kind=kind_phys) :: fhour     ! forecast hour
         ! End Physics only

       contains
         procedure :: setrad => dyn_param_setrad
         procedure :: setphys => dyn_param_setphys

       end type





!******************************************
! Type model parameters
!******************************************
       type model_parameters

         private

         integer :: ntcw        ! =0 no cloud condensate calculated
                                ! >0 array index location for cloud condensate
         integer :: ncld        ! only used when ntcw .gt. 0
         integer :: ntoz        ! =0 climatological ozone profile
                                ! >0 interactive ozone profile
         integer :: NTRAC       ! dimension veriable for array oz
         integer :: levs        ! vertical layer dimension
         integer :: me          ! control flag for parallel process
         integer :: lsoil       ! number of soil layers
         integer :: lsm         ! flag for land surface model to use =0  for osu lsm; =1  for noah lsm
         integer :: nmtvr       ! number of topographic variables such as variance etc used in the GWD parameterization
         integer :: nrcm        ! second dimension for the random number array rann
         integer :: levozp      ! ko3 - number of layers for ozone data
         integer :: lonr        ! number of lon/lat points
         integer :: latr        ! number of lon/lat points
         integer :: jcap        ! number of spectral wave trancation used only by sascnv shalcnv
         integer :: num_p3d     ! number of 3D arrays needed for microphysics
         integer :: num_p2d     ! number of 2D arrays needed for microphysics
         integer :: npdf3d      ! number of 3d arrays associated with pdf based clouds/microphysics
         integer :: pl_coeff    ! number coefficients in ozone forcing
         integer :: ncw(2)      ! range of droplet number concentrations for Ferrier microphysics - from compns_physics.f
         integer :: idate(4)    ! initial date
         integer :: idat(8)     ! initial date but different size and ordering
                                ! used by radupdate

         real (kind=kind_phys) :: crtrh(3)    ! critical relative humidity at the surface  PBL top and at the top of the atmosphere
         real (kind=kind_phys) :: cdmbgwd(2)  ! multiplication factors for cdmb and gwd
         real (kind=kind_phys) :: ccwf(2)     ! multiplication factor for critical cloud  workfunction for RAS
         real (kind=kind_phys) :: dlqf(2)     ! factor for cloud condensate detrainment from cloud edges (RAS)
         real (kind=kind_phys) :: ctei_rm(2)  ! critical cloud top entrainment instability criteria (used if mstrat=.true.)
         real (kind=kind_phys) :: cgwf(2)     ! multiplication factor for convective GWD
         real (kind=kind_phys) :: prslrd0     ! pressure level from which Rayleigh Damping is applied

         logical :: ras                ! flag for ras convection scheme
         logical :: pre_rad            ! flag for testing purpose
         logical :: ldiag3d            ! flag for 3d diagnostic fields
         logical :: lgocart            ! flag for 3d diagnostic fields for gocart 1
         logical :: lssav_cpl          ! flag for save data for A/O/I coupling
         logical :: flipv              ! flag for vertical direction flip (ras)
         logical :: old_monin          ! flag for diff monin schemes
         logical :: cnvgwd             ! flag for conv gravity wave drag

         logical :: shal_cnv           ! flag for calling shallow convection
         logical :: sashal             ! flag for new shallow conv scheme

         logical :: newsas             ! flag for new sas conv scheme
         logical :: cal_pre            ! flag controls precip type algorithm
         logical :: mom4ice            ! flag controls mom4 sea ice
         logical :: mstrat             ! flag for moorthi approach for stratus
         logical :: trans_trac         ! flag for convective transport of tracers
         integer :: nst_fcst           ! flag 0 for no nst  1 for uncoupled nst  and 2 for coupled NST
         logical :: moist_adj          ! flag for moist convective adjustment
         integer :: thermodyn_id       ! valid for GFS only for get_prs/phi
         integer :: sfcpress_id        ! valid for GFS only for get_prs/phi
         logical :: gen_coord_hybrid   ! for Henry's gen coord
         integer :: levr               ! the number of layers GFS Radiative heating calculated at 1
         logical :: lsidea             ! flag for idea
         logical :: pdfcld             ! flag for pdfcld
         logical :: shcnvcw            ! flag for shallow convective cloud
         logical :: redrag             ! flag for reduced drag coeff. over sea
         logical :: hybedmf            ! flag for hybrid edmf pbl scheme
         logical :: dspheat            ! flag for tke dissipative heating

         ! real(kind=kind_phys) :: dxmax, dxmin, dxinv    ! based on max lon/lat contained in physcons.f module

         contains

           procedure :: print => mdl_param_print

       end type
!******************************************




! Module Subroutines and Type bound procedures
!******************************************
       contains  ! module
!******************************************




!******************************************
! tbd_ddt methods
!******************************************

      subroutine tbd_set ( this, dpshc, prdout, poz, rann, xkzm_m, xkzm_h, xkzm_s, psautco,  &
                 prautco, evpco, wminco, &
                 acv, acvb, acvt, slc, smc, stc,  &
                 upd_mf, dwn_mf, det_mf, phy_f3d, phy_f2d, tprcp,  &
                 srflag, tref, z_c, c_0, c_d, w_0, w_d  ) 

         class(tbd_ddt) :: this

! In
         real (kind=kind_phys), target :: dpshc (:)     !            maximum pressure depth for shallow convection
         real (kind=kind_phys), target :: prdout(:,:,:) ! ozplout_v  ozone forcing data
         real (kind=kind_phys), target :: poz(:)        ! pl_pres    ozone forcing data level pressure (ln(Pa))
         real (kind=kind_phys), target :: rann(:,:)     !            random number array (0-1)

         real (kind=kind_phys) :: xkzm_m      ! bkgd_vdif_m  background vertical diffusion for momentum
         real (kind=kind_phys) :: xkzm_h      ! bkgd_vdif_h  background vertical diffusion for heat q
         real (kind=kind_phys) :: xkzm_s      ! bkgd_vdif_s  sigma threshold for background mom. diffusn
         real (kind=kind_phys) :: psautco(2)  !              auto conversion coeff from ice to snow
         real (kind=kind_phys) :: prautco(2)  !              auto conversion coeff from cloud to rain
         real (kind=kind_phys) :: evpco       !              coeff for evaporation of largescale rain
         real (kind=kind_phys) :: wminco(2)   !              water and ice minimum threshold for Zhao

! Moved to interface fields
!         real (kind=kind_phys), target :: sfcnirbmu(:) ! aoi_fld%nirbmui  sfc nir beam sw upward flux (w/m2)
!         real (kind=kind_phys), target :: sfcnirdfu(:) ! aoi_fld%nirdfui  sfc nir diff sw upward flux (w/m2)
!         real (kind=kind_phys), target :: sfcvisbmu(:) ! aoi_fld%visbmui  sfc uv+vis beam sw upward flux (w/m2)
!         real (kind=kind_phys), target :: sfcvisdfu(:) ! aoi_fld%visdfui  sfc uv+vis diff sw upward flux (w/m2)

! Moved to dyn_parm
!         real (kind=kind_phys) :: slag  ! equation of time ( radian )
!         real (kind=kind_phys) :: sdec  ! sin of the solar declination angle
!         real (kind=kind_phys) :: cdec  ! cos of the solar declination angle

! In/Out
         real (kind=kind_phys), target :: acv (:)      ! array containing accumulated convective clouds
         real (kind=kind_phys), target :: acvb(:)      ! arrays used by cnvc90 bottom
         real (kind=kind_phys), target :: acvt(:)      ! arrays used by cnvc9 top
         real (kind=kind_phys), target :: slc(:,:)     ! liquid soil moisture
         real (kind=kind_phys), target :: smc(:,:)     ! total soil moisture
         real (kind=kind_phys), target :: stc(:,:)     ! soil temperature
         real (kind=kind_phys), target :: upd_mf(:,:)   ! convective updraft mass flux
         real (kind=kind_phys), target :: dwn_mf(:,:)   ! convective downdraft mass flux
         real (kind=kind_phys), target :: det_mf(:,:)   ! convective detrainment mass flux
         real (kind=kind_phys), target :: phy_f3d(:,:,:) ! 3d arrays saved for restart
         real (kind=kind_phys), target :: phy_f2d(:,:)   ! phy_f2dv     2d arrays save for restart
         real (kind=kind_phys), target :: tprcp (:)      ! sfc_fld%tprcp   total precipitation
         real (kind=kind_phys), target :: srflag(:)      ! sfc_fld%srflag  snow/rain flag for precipitation

! Out
         real (kind=kind_phys), target :: tref(:) ! nst_fld%Tref  Reference Temperature
         real (kind=kind_phys), target :: z_c(:)  ! nst_fld%z_c   Sub layer cooling thickness
         real (kind=kind_phys), target :: c_0(:)  ! nst_fld%c_0   coefficient1 to calculate d(Tz)/d(Ts)
         real (kind=kind_phys), target :: c_d(:)  ! nst_fld%c_d   coefficient2 to calculate d(Tz)/d(Ts)
         real (kind=kind_phys), target :: w_0(:)  ! nst_fld%w_0   coefficient3 to calculate d(Tz)/d(Ts)
         real (kind=kind_phys), target :: w_d(:)  ! nst_fld%w_d   coefficient4 to calculate d(Tz)/d(Ts)

         this%dpshc => dpshc
         this%prdout => prdout
         this%poz => poz
         this%rann => rann

         this%xkzm_m = xkzm_m
         this%xkzm_h = xkzm_h
         this%xkzm_s = xkzm_s
         this%psautco = psautco
         this%prautco = prautco
         this%evpco = evpco
         this%wminco = wminco

         this%acv => acv
         this%acvb => acvb
         this%acvt => acvt
         this%slc => slc
         this%smc => smc
         this%stc => stc
         this%upd_mf => upd_mf
         this%dwn_mf => dwn_mf
         this%det_mf => det_mf
         this%phy_f3d => phy_f3d
         this%phy_f2d => phy_f2d
         this%tprcp => tprcp
         this%srflag => srflag

         this%tref => tref
         this%z_c => z_c
         this%c_0 => c_0
         this%c_d => c_d
         this%w_0 => w_0
         this%w_d => w_d

      end subroutine





!******************************************
! State field methods
!******************************************

       subroutine state_fld_setrad_in (this, prsi, prsl, prslk, tgrs, qgrs_rad, tracer, vvl)

         implicit none

         class(state_fields_in) :: this

         real (kind=kind_phys), intent(in), target :: prsi (:,:)
         real (kind=kind_phys), target :: prsl  (:,:)
         real (kind=kind_phys), target :: prslk (:,:)
         real (kind=kind_phys), target :: tgrs  (:,:)
         real (kind=kind_phys), target :: qgrs_rad (:,:)
         real (kind=kind_phys), target :: tracer(:,:,:)
         real (kind=kind_phys), target :: vvl  (:,:)

         call dbgprint("state_fld_setradin")

         select type (this)
           class is (state_fields_in)
             this%prsi   => prsi
             this%prsl   => prsl
             this%prslk  => prslk
             this%tgrs   => tgrs
             this%qgrs_rad   => qgrs_rad
             this%tracer => tracer
             this%vvl    => vvl
         end select

       end subroutine



       subroutine state_fld_setphys_in (this, prsi, prsl, prslk, tgrs, qgrs, vvl,       &
                                     pgr, ugrs, vgrs, prsik, phii, phil, adjtrc)

         implicit none

         class(state_fields_in) :: this

         real (kind=kind_phys), target :: prsi  (:,:)
         real (kind=kind_phys), target :: prsl  (:,:)
         real (kind=kind_phys), target :: prslk (:,:)
         real (kind=kind_phys), target :: tgrs  (:,:)
         real (kind=kind_phys), target :: qgrs (:,:,:)
         real (kind=kind_phys), target :: vvl (:,:)

         !! The following not in radiation
         real (kind=kind_phys), target :: pgr    (:)
         real (kind=kind_phys), target :: ugrs   (:,:)
         real (kind=kind_phys), target :: vgrs   (:,:)
         real (kind=kind_phys), target :: prsik  (:,:)
         real (kind=kind_phys), target :: phii   (:,:)
         real (kind=kind_phys), target :: phil   (:,:)

         real (kind=kind_phys), target :: adjtrc (:)

         call dbgprint("state_fld_setphys_in")

         select type (this)
           class is (state_fields_in)

             this%prsi   => prsi
             this%prsl   => prsl
             this%prslk  => prslk
             this%tgrs   => tgrs
             this%qgrs   => qgrs
             this%vvl    => vvl

             this%pgr => pgr
             this%ugrs => ugrs
             this%vgrs => vgrs
             this%prsik => prsik
             this%phii => phii
             this%phil => phil

             this%adjtrc => adjtrc

           ! class default
           ! print *, "class default"
         end select

       end subroutine



       subroutine state_fld_setphys_out (this, gt0, gq0, gu0, gv0)

         implicit none

         class(state_fields_out) :: this

         !! Outputs (only in physics)
         real (kind=kind_phys), intent(out), target :: gt0    (:,:)
         real (kind=kind_phys), target :: gq0    (:,:,:)
         real (kind=kind_phys), target :: gu0    (:,:)
         real (kind=kind_phys), target :: gv0    (:,:)

         call dbgprint("state_fld_setphys_out")

         select type (this)
           class is (state_fields_out)
             this%gt0 => gt0
             this%gq0 => gq0
             this%gu0 => gu0
             this%gv0 => gv0
         end select

      end subroutine


      subroutine state_fld_out_print (this)

         implicit none

         class (state_fields_out) :: this


         print *, "STATE FIELDS OUT"
         print *, "----- ------ ---"
         print *, this%gt0
         print *, this%gq0
         print *, this%gu0
         print *, this%gv0

      end subroutine


      subroutine state_fld_in_print (this)

        implicit none

        class(state_fields_in) :: this

        print *, "stub : state_fields_in print"

      end subroutine


!******************************************
! Surface properties methods
!******************************************
       subroutine sfc_prop_setrad (this, slmsk, tsfc, snowd, sncovr, snoalb,      &
                                zorl, hprim, fice, tisfc,                      &
                                alvsf, alnsf, alvwf, alnwf, facsf, facwf )
                                
                                
         implicit none

         class(sfc_properties) :: this

         real (kind=kind_phys), target :: slmsk (:)
         real (kind=kind_phys), target :: tsfc  (:)
         real (kind=kind_phys), target :: snowd (:)
         real (kind=kind_phys), target :: sncovr(:)
         real (kind=kind_phys), target :: snoalb(:)
         real (kind=kind_phys), target :: zorl  (:)
         real (kind=kind_phys), target :: hprim (:)
         real (kind=kind_phys), target :: fice  (:)
         real (kind=kind_phys), target :: tisfc (:)

         !!! GSM grrad.f ONLY
         real (kind=kind_phys), optional, target :: alvsf (:)
         real (kind=kind_phys), optional, target :: alnsf (:)
         real (kind=kind_phys), optional, target :: alvwf (:)
         real (kind=kind_phys), optional, target :: alnwf (:)
         real (kind=kind_phys), optional, target :: facsf (:)
         real (kind=kind_phys), optional, target :: facwf (:)
         !!! END

         call dbgprint("sfc_prop_set")

         select type (this)
           class is (sfc_properties)
             this%slmsk  => slmsk
             this%tsfc   => tsfc
             this%snowd  => snowd
             this%sncovr => sncovr
             this%snoalb => snoalb
             this%zorl   => zorl
             this%hprim  => hprim
             this%fice   => fice
             this%tisfc  => tisfc

             !!! GSM 
             if (present(alvsf)) this%alvsf  => alvsf
             if (present(alnsf)) this%alnsf  => alnsf
             if (present(alvwf)) this%alvwf  => alvwf
             if (present(alnwf)) this%alnwf  => alnwf
             if (present(facsf)) this%facsf  => facsf
             if (present(facwf)) this%facwf  => facwf

         end select

       end subroutine


       subroutine sfc_prop_setphys ( this,                                   &
                 hprime2, slope, shdmin, shdmax, snoalb, tg3, slmsk, vfrac,  &
                 vtype, stype, uustar, oro, oro_uf, hice, fice, tisfc,  &
                 tsea, snwdph, weasd, sncovr, zorl, canopy, ffmm, ffhh,  &
                 f10m, t2m, q2m                                          &
                 )
         implicit none

         class(sfc_properties) :: this

         ! In
         real (kind=kind_phys), target :: hprime2(:,:) 
         real (kind=kind_phys), target :: slope(:) 
         real (kind=kind_phys), target :: shdmin(:) 
         real (kind=kind_phys), target :: shdmax(:) 
         real (kind=kind_phys), target :: snoalb(:) 
         real (kind=kind_phys), target :: tg3(:) 
         real (kind=kind_phys), target :: slmsk(:) 
         real (kind=kind_phys), target :: vfrac(:) 
         real (kind=kind_phys), target :: vtype(:) 
         real (kind=kind_phys), target :: stype(:) 
         real (kind=kind_phys), target :: uustar(:) 
         real (kind=kind_phys), target :: oro(:) 
         real (kind=kind_phys), target :: oro_uf(:) 

         ! In/Out
         real (kind=kind_phys), target :: hice(:) 
         real (kind=kind_phys), target :: fice(:) 
         real (kind=kind_phys), target :: tisfc(:) 
         real (kind=kind_phys), target :: tsea(:) 
         real (kind=kind_phys), target :: snwdph(:) 
         real (kind=kind_phys), target :: weasd(:) 
         real (kind=kind_phys), target :: sncovr(:) 
         real (kind=kind_phys), target :: zorl(:) 
         real (kind=kind_phys), target :: canopy(:) 
         real (kind=kind_phys), target :: ffmm(:) 
         real (kind=kind_phys), target :: ffhh(:) 
         real (kind=kind_phys), target :: f10m(:) 

         ! Outputs
         real (kind=kind_phys), target :: t2m(:) 
         real (kind=kind_phys), target :: q2m(:)

         this%hprime2 => hprime2
         this%slope => slope
         this%shdmin => shdmin
         this%shdmax => shdmax
         this%snoalb => snoalb
         this%tg3 => tg3
         this%slmsk => slmsk
         this%vfrac => vfrac
         this%vtype => vtype
         this%stype => stype
         this%uustar => uustar
         this%oro => oro
         this%oro_uf => oro_uf
         this%hice => hice
         this%fice => fice
         this%tisfc => tisfc
         this%tsfc => tsea
         this%snowd => snwdph   ! snowd
         this%weasd => weasd
         this%sncovr => sncovr
         this%zorl => zorl
         this%canopy => canopy
         this%ffmm => ffmm
         this%ffhh => ffhh
         this%f10m => f10m
         this%t2m => t2m
         this%q2m => q2m


       end subroutine

!******************************************
! Diagnostics methods
!******************************************
       subroutine diagnostics_setrad (this, NFXR, fluxr, topfsw, topflw, dswcmp, uswcmp)

         implicit none

         class(diagnostics) :: this

         integer :: NFXR

         real (kind=kind_phys), target :: fluxr (:,:)
         type (topfsw_type), target :: topfsw(:)
         type (topflw_type), target :: topflw(:)

         real (kind=kind_phys), target :: dswcmp(:,:) 
         real (kind=kind_phys), target :: uswcmp(:,:) 

         call dbgprint("diagnostics_setrad")

         select type (this)
           class is (diagnostics)

             this%NFXR = NFXR
             this%fluxr => fluxr
             this%topfsw => topfsw
             this%topflw => topflw
             this%dswcmp => dswcmp
             this%uswcmp => uswcmp

         end select

       end subroutine


       subroutine diagnostics_setphys ( this, srunoff, evbsa, evcwa,                     &
                          snohfa, transa, sbsnoa, snowca, soilm,                         &
                          tmpmin, tmpmax, dusfc, dvsfc, dtsfc, dqsfc, totprcp, gflux,    &
                          dlwsfc, ulwsfc, suntim, runoff, ep, cldwrk, dugwd, dvgwd,      &
                          psmean, cnvprcp, spfhmin, spfhmax, rain, rainc,                &
                          dt3dt, dq3dt, du3dt, dv3dt, dqdt_v,                            &
                          u10m, v10m, zlvl, psurf, hpbl, pwat, t1, q1,                   &
                          u1, v1, chh, cmm, dlwsfci, ulwsfci, dswsfci, uswsfci,          &
                          dusfci, dvsfci, dtsfci, dqsfci, gfluxi, epi, smcwlt2, smcref2, &
                          wet1, sr )

         implicit none

         class(diagnostics) :: this


         ! Inputs

         ! Moved to interface fields ddt
         ! real (kind=kind_phys), target :: sfcnirbmd(:)
         ! real (kind=kind_phys), target :: sfcnirdfd(:)
         ! real (kind=kind_phys), target :: sfcvisbmd(:)
         ! real (kind=kind_phys), target :: sfcvisdfd(:)

         ! In/Out
         real (kind=kind_phys), target :: srunoff (:)
         real (kind=kind_phys), target :: evbsa (:)
         real (kind=kind_phys), target :: evcwa (:)
         real (kind=kind_phys), target :: snohfa (:)
         real (kind=kind_phys), target :: transa (:)
         real (kind=kind_phys), target :: sbsnoa (:)
         real (kind=kind_phys), target :: snowca (:)
         real (kind=kind_phys), target :: soilm (:)
         real (kind=kind_phys), target :: tmpmin (:)
         real (kind=kind_phys), target :: tmpmax (:)
         real (kind=kind_phys), target :: dusfc (:)
         real (kind=kind_phys), target :: dvsfc (:)
         real (kind=kind_phys), target :: dtsfc (:)
         real (kind=kind_phys), target :: dqsfc (:)
         real (kind=kind_phys), target :: totprcp (:)
         real (kind=kind_phys), target :: gflux (:)
         real (kind=kind_phys), target :: dlwsfc (:)
         real (kind=kind_phys), target :: ulwsfc (:)
         real (kind=kind_phys), target :: suntim (:)
         real (kind=kind_phys), target :: runoff (:)
         real (kind=kind_phys), target :: ep (:)
         real (kind=kind_phys), target :: cldwrk (:)
         real (kind=kind_phys), target :: dugwd (:)
         real (kind=kind_phys), target :: dvgwd (:)
         real (kind=kind_phys), target :: psmean (:)
         real (kind=kind_phys), target :: cnvprcp (:)
         real (kind=kind_phys), target :: spfhmin (:)
         real (kind=kind_phys), target :: spfhmax (:)
         real (kind=kind_phys), target :: rain (:)
         real (kind=kind_phys), target :: rainc (:)
         real (kind=kind_phys), target :: dt3dt (:,:,:)
         real (kind=kind_phys), target :: dq3dt (:,:,:)
         real (kind=kind_phys), target :: du3dt (:,:,:)
         real (kind=kind_phys), target :: dv3dt (:,:,:)
         real (kind=kind_phys), target :: dqdt_v(:,:)

         ! Outputs
         real (kind=kind_phys), target :: u10m(:) 
         real (kind=kind_phys), target :: v10m(:) 
         real (kind=kind_phys), target :: zlvl(:) 
         real (kind=kind_phys), target :: psurf(:) 
         real (kind=kind_phys), target :: hpbl(:) 
         real (kind=kind_phys), target :: pwat(:) 
         real (kind=kind_phys), target :: t1(:) 
         real (kind=kind_phys), target :: q1(:) 
         real (kind=kind_phys), target :: u1(:) 
         real (kind=kind_phys), target :: v1(:) 
         real (kind=kind_phys), target :: chh(:)
         real (kind=kind_phys), target :: cmm(:) 
         real (kind=kind_phys), target :: dlwsfci(:) 
         real (kind=kind_phys), target :: ulwsfci(:) 
         real (kind=kind_phys), target :: dswsfci(:) 
         real (kind=kind_phys), target :: uswsfci(:) 
         real (kind=kind_phys), target :: dusfci(:) 
         real (kind=kind_phys), target :: dvsfci(:) 
         real (kind=kind_phys), target :: dtsfci(:) 
         real (kind=kind_phys), target :: dqsfci(:) 
         real (kind=kind_phys), target :: gfluxi(:) 
         real (kind=kind_phys), target :: epi(:) 
         real (kind=kind_phys), target :: smcwlt2(:) 
         real (kind=kind_phys), target :: smcref2(:) 
         real (kind=kind_phys), target :: wet1(:) 
         real (kind=kind_phys), target :: sr(:) 


         call dbgprint("diagnostics_setphys")

         ! In/Out
         this%srunoff => srunoff
         this%evbsa => evbsa
         this%evcwa => evcwa
         this%snohfa => snohfa
         this%transa => transa
         this%sbsnoa => sbsnoa
         this%snowca => snowca
         this%soilm => soilm
         this%tmpmin => tmpmin
         this%tmpmax => tmpmax
         this%dusfc => dusfc
         this%dvsfc => dvsfc
         this%dtsfc => dtsfc
         this%dqsfc => dqsfc
         this%totprcp => totprcp
         this%gflux => gflux
         this%dlwsfc => dlwsfc
         this%ulwsfc => ulwsfc
         this%suntim => suntim
         this%runoff => runoff
         this%ep => ep
         this%cldwrk => cldwrk
         this%dugwd => dugwd
         this%dvgwd => dvgwd
         this%psmean => psmean
         this%cnvprcp => cnvprcp
         this%spfhmin => spfhmin
         this%spfhmax => spfhmax
         this%rain => rain
         this%rainc => rainc
         this%dt3dt => dt3dt
         this%dq3dt => dq3dt
         this%du3dt => du3dt
         this%dv3dt => dv3dt
         this%dqdt_v => dqdt_v

         ! Output
         this%u10m => u10m
         this%v10m => v10m
         this%zlvl => zlvl
         this%psurf => psurf
         this%hpbl => hpbl
         this%pwat => pwat
         this%t1 => t1
         this%q1 => q1
         this%u1 => u1
         this%v1 => v1
         this%chh => chh
         this%cmm => cmm
         this%dlwsfci => dlwsfci
         this%ulwsfci => ulwsfci
         this%dswsfci => dswsfci
         this%uswsfci => uswsfci
         this%dusfci => dusfci
         this%dvsfci => dvsfci
         this%dtsfci => dtsfci
         this%dqsfci => dqsfci
         this%gfluxi => gfluxi
         this%epi => epi
         this%smcwlt2 => smcwlt2
         this%smcref2 => smcref2
         this%wet1 => wet1
         this%sr => sr

       end subroutine



!******************************************
! Interface Fields methods
!******************************************
       subroutine interface_fld_setrad (this, sfcfsw, sfcflw, htrlw0, htrsw0, htrswb, htrlwb)

         implicit none

         class(interface_fields) :: this

         type (sfcfsw_type), target :: sfcfsw(:)
         type (sfcflw_type), target :: sfcflw(:)

         !! Optional output variables:
         real (kind=kind_phys), target, optional :: htrlw0(:,:)
         real (kind=kind_phys), target, optional :: htrsw0(:,:)

         real (kind=kind_phys), target, optional :: htrswb(:,:,:)
         real (kind=kind_phys), target, optional :: htrlwb(:,:,:)

         call dbgprint("interface_fld_setrad")

         select type (this)
           class is (interface_fields)
           
             this%sfcfsw => sfcfsw
             this%sfcflw => sfcflw

             if (present(htrswb)) this%htrswb => htrswb
             if (present(htrlwb)) this%htrlwb => htrlwb

             if (present(htrlw0)) this%htrlw0 => htrlw0
             if (present(htrsw0)) this%htrsw0 => htrsw0

         end select

       end subroutine



       subroutine interface_fld_setphys(this, sfcdsw, sfcnsw, sfcdlw, sfcnirbmu, sfcnirdfu, sfcvisbmu,         &
                 sfcvisdfu, sfcnirbmd, sfcnirdfd, sfcvisbmd, sfcvisdfd,                                        &
                 dusfc_cpl, dvsfc_cpl, dtsfc_cpl, dqsfc_cpl, dlwsfc_cpl, dswsfc_cpl, dnirbm_cpl,               &
                 dnirdf_cpl, dvisbm_cpl, dvisdf_cpl, rain_cpl, nlwsfc_cpl, nswsfc_cpl, nnirbm_cpl, nnirdf_cpl, &
                 nvisbm_cpl, nvisdf_cpl, xt, xs, xu, xv, xz, zm, xtts, xzts, d_conv, ifd, dt_cool, Qrain,      &
                 dusfci_cpl, dvsfci_cpl, dtsfci_cpl, dqsfci_cpl, dlwsfci_cpl, dswsfci_cpl, dnirbmi_cpl, dnirdfi_cpl,      &
                 dvisbmi_cpl, dvisdfi_cpl, nlwsfci_cpl, nswsfci_cpl, nnirbmi_cpl, nnirdfi_cpl, nvisbmi_cpl, nvisdfi_cpl,  &
                 t2mi_cpl, q2mi_cpl, u10mi_cpl, v10mi_cpl, tseai_cpl, psurfi_cpl, oro_cpl, slmsk_cpl)

         implicit none

         class(interface_fields) :: this

         ! Inputs
         real (kind=kind_phys), target :: sfcdsw(:)
         real (kind=kind_phys), target :: sfcnsw(:)
         real (kind=kind_phys), target :: sfcdlw(:)

         ! Inputs moved from TBD physics only
         real (kind=kind_phys), target :: sfcnirbmu(:) ! aoi_fld%nirbmui  sfc nir beam sw upward flux (w/m2)
         real (kind=kind_phys), target :: sfcnirdfu(:) ! aoi_fld%nirdfui  sfc nir diff sw upward flux (w/m2)
         real (kind=kind_phys), target :: sfcvisbmu(:) ! aoi_fld%visbmui  sfc uv+vis beam sw upward flux (w/m2)
         real (kind=kind_phys), target :: sfcvisdfu(:) ! aoi_fld%visdfui  sfc uv+vis diff sw upward flux (w/m2)

         ! Inputs moved from diagnostics - physics only
         real (kind=kind_phys), target :: sfcnirbmd(:) ! aoi_fld%nirbmdi  sfc nir beam sw downward flux (w/m2)
         real (kind=kind_phys), target :: sfcnirdfd(:) ! aoi_fld%nirdfdi  sfc nir diff sw downward flux (w/m2)
         real (kind=kind_phys), target :: sfcvisbmd(:) ! aoi_fld%visbmdi  sfc uv+vis beam sw downward flux (w/m2)
         real (kind=kind_phys), target :: sfcvisdfd(:) ! aoi_fld%visdfdi  sfc uv+vis diff sw downward flux (w/m2)

         ! In/out
         real (kind=kind_phys), target :: dusfc_cpl(:)
         real (kind=kind_phys), target :: dvsfc_cpl(:)
         real (kind=kind_phys), target :: dtsfc_cpl(:)
         real (kind=kind_phys), target :: dqsfc_cpl(:)
         real (kind=kind_phys), target :: dlwsfc_cpl(:)
         real (kind=kind_phys), target :: dswsfc_cpl(:)
         real (kind=kind_phys), target :: dnirbm_cpl(:)
         real (kind=kind_phys), target :: dnirdf_cpl(:)
         real (kind=kind_phys), target :: dvisbm_cpl(:)
         real (kind=kind_phys), target :: dvisdf_cpl(:)
         real (kind=kind_phys), target :: rain_cpl(:)
         real (kind=kind_phys), target :: nlwsfc_cpl(:)
         real (kind=kind_phys), target :: nswsfc_cpl(:)
         real (kind=kind_phys), target :: nnirbm_cpl(:)
         real (kind=kind_phys), target :: nnirdf_cpl(:)
         real (kind=kind_phys), target :: nvisbm_cpl(:)
         real (kind=kind_phys), target :: nvisdf_cpl(:)
         real (kind=kind_phys), target :: xt(:)
         real (kind=kind_phys), target :: xs(:)
         real (kind=kind_phys), target :: xu(:)
         real (kind=kind_phys), target :: xv(:)
         real (kind=kind_phys), target :: xz(:)
         real (kind=kind_phys), target :: zm(:)
         real (kind=kind_phys), target :: xtts(:)
         real (kind=kind_phys), target :: xzts(:)
         real (kind=kind_phys), target :: d_conv(:)
         real (kind=kind_phys), target :: ifd(:)
         real (kind=kind_phys), target :: dt_cool(:)
         real (kind=kind_phys), target :: Qrain(:)

         ! Outputs
         real (kind=kind_phys), optional, target :: dusfci_cpl(:)
         real (kind=kind_phys), optional, target :: dvsfci_cpl(:)
         real (kind=kind_phys), optional, target :: dtsfci_cpl(:)
         real (kind=kind_phys), optional, target :: dqsfci_cpl(:)
         real (kind=kind_phys), optional, target :: dlwsfci_cpl(:)
         real (kind=kind_phys), optional, target :: dswsfci_cpl(:)
         real (kind=kind_phys), optional, target :: dnirbmi_cpl(:)
         real (kind=kind_phys), optional, target :: dnirdfi_cpl(:)
         real (kind=kind_phys), optional, target :: dvisbmi_cpl(:)
         real (kind=kind_phys), optional, target :: dvisdfi_cpl(:)
         real (kind=kind_phys), optional, target :: nlwsfci_cpl(:)
         real (kind=kind_phys), optional, target :: nswsfci_cpl(:)
         real (kind=kind_phys), optional, target :: nnirbmi_cpl(:)
         real (kind=kind_phys), optional, target :: nnirdfi_cpl(:)
         real (kind=kind_phys), optional, target :: nvisbmi_cpl(:)
         real (kind=kind_phys), optional, target :: nvisdfi_cpl(:)
         real (kind=kind_phys), optional, target :: t2mi_cpl(:)
         real (kind=kind_phys), optional, target :: q2mi_cpl(:)
         real (kind=kind_phys), optional, target :: u10mi_cpl(:)
         real (kind=kind_phys), optional, target :: v10mi_cpl(:)
         real (kind=kind_phys), optional, target :: tseai_cpl(:)
         real (kind=kind_phys), optional, target :: psurfi_cpl(:)
         real (kind=kind_phys), optional, target :: oro_cpl(:)
         real (kind=kind_phys), optional, target :: slmsk_cpl(:)


         call dbgprint("interface_fld_setphys")

         ! Input
         this%sfcdsw => sfcdsw
         this%sfcnsw => sfcnsw
         this%sfcdlw => sfcdlw

         this%sfcnirbmu => sfcnirbmu
         this%sfcnirdfu => sfcnirdfu
         this%sfcvisbmu => sfcvisbmu
         this%sfcvisdfu => sfcvisdfu

         this%sfcnirbmd => sfcnirbmd
         this%sfcnirdfd => sfcnirdfd
         this%sfcvisbmd => sfcvisbmd
         this%sfcvisdfd => sfcvisdfd

         ! In/Out
         this%dusfc_cpl => dusfc_cpl
         this%dvsfc_cpl => dvsfc_cpl
         this%dtsfc_cpl => dtsfc_cpl
         this%dqsfc_cpl => dqsfc_cpl
         this%dlwsfc_cpl => dlwsfc_cpl
         this%dswsfc_cpl => dswsfc_cpl
         this%dnirbm_cpl => dnirbm_cpl
         this%dnirdf_cpl => dnirdf_cpl
         this%dvisbm_cpl => dvisbm_cpl
         this%dvisdf_cpl => dvisdf_cpl
         this%rain_cpl => rain_cpl
         this%nlwsfc_cpl => nlwsfc_cpl
         this%nswsfc_cpl => nswsfc_cpl
         this%nnirbm_cpl => nnirbm_cpl
         this%nnirdf_cpl => nnirdf_cpl
         this%nvisbm_cpl => nvisbm_cpl
         this%nvisdf_cpl => nvisdf_cpl

         this%xt => xt
         this%xs => xs
         this%xu => xu
         this%xv => xv
         this%xz => xz
         this%zm => zm
         this%xtts => xtts
         this%xzts => xzts
         this%d_conv => d_conv
         this%ifd => ifd
         this%dt_cool => dt_cool
         this%Qrain => Qrain

         ! Output
         if (present(dusfci_cpl)) this%dusfci_cpl => dusfci_cpl
         if (present(dvsfci_cpl)) this%dvsfci_cpl => dvsfci_cpl
         if (present(dtsfci_cpl)) this%dtsfci_cpl => dtsfci_cpl
         if (present(dqsfci_cpl)) this%dqsfci_cpl => dqsfci_cpl
         if (present(dlwsfci_cpl)) this%dlwsfci_cpl => dlwsfci_cpl
         if (present(dswsfci_cpl)) this%dswsfci_cpl => dswsfci_cpl
         if (present(dnirbmi_cpl)) this%dnirbmi_cpl => dnirbmi_cpl
         if (present(dnirdfi_cpl)) this%dnirdfi_cpl => dnirdfi_cpl
         if (present(dvisbmi_cpl)) this%dvisbmi_cpl => dvisbmi_cpl
         if (present(dvisdfi_cpl)) this%dvisdfi_cpl => dvisdfi_cpl
         if (present(nlwsfci_cpl)) this%nlwsfci_cpl => nlwsfci_cpl
         if (present(nswsfci_cpl)) this%nswsfci_cpl => nswsfci_cpl
         if (present(nnirbmi_cpl)) this%nnirbmi_cpl => nnirbmi_cpl
         if (present(nnirdfi_cpl)) this%nnirdfi_cpl => nnirdfi_cpl
         if (present(nvisbmi_cpl)) this%nvisbmi_cpl => nvisbmi_cpl
         if (present(nvisdfi_cpl)) this%nvisdfi_cpl => nvisdfi_cpl
         if (present(t2mi_cpl)) this%t2mi_cpl => t2mi_cpl
         if (present(q2mi_cpl)) this%q2mi_cpl => q2mi_cpl
         if (present(u10mi_cpl)) this%u10mi_cpl => u10mi_cpl
         if (present(v10mi_cpl)) this%v10mi_cpl => v10mi_cpl
         if (present(tseai_cpl)) this%tseai_cpl => tseai_cpl
         if (present(psurfi_cpl)) this%psurfi_cpl => psurfi_cpl
         if (present(oro_cpl)) this%oro_cpl => oro_cpl
         if (present(slmsk_cpl)) this%slmsk_cpl => slmsk_cpl

       end subroutine


!******************************************
! Cloud properties methods
!******************************************
       subroutine cld_prop_setrad (this, cv, cvt, cvb, fcice, frain, rrime, flgmin, &
                                   cldcov, deltaq, sup, cnvw, cnvc)

         implicit none

         class(cloud_properties) :: this
        
         ! PT intent ? 
         real (kind=kind_phys), target :: cv  (:)
         real (kind=kind_phys), target :: cvt (:)
         real (kind=kind_phys), target :: cvb (:)
         real (kind=kind_phys), target :: fcice (:,:)
         real (kind=kind_phys), target :: frain (:,:)
         real (kind=kind_phys), target :: rrime (:,:)
         real (kind=kind_phys), target :: flgmin (:)

         real (kind=kind_phys), target :: deltaq(:,:)
         real (kind=kind_phys), target :: sup
         real (kind=kind_phys), target :: cnvw(:,:)
         real (kind=kind_phys), target :: cnvc(:,:)

         !! Outputs from radiation
         real (kind=kind_phys), target :: cldcov(:,:)

         call dbgprint("cld_prop_set")

         select type (this)
           class is (cloud_properties)
             this%cv     => cv
             this%cvt    => cvt
             this%cvb    => cvb
             this%fcice  => fcice
             this%frain  => frain
             this%rrime  => rrime
             this%flgmin => flgmin
             this%cldcov => cldcov
             this%deltaq => deltaq
             this%sup => sup
             this%cnvw => cnvw
             this%cnvc => cnvc
         end select         

       end subroutine


       subroutine cld_prop_setphys (this, flgmin, cv, cvt, cvb, cnvqc_v, sup )

         implicit none

         class(cloud_properties) :: this

         ! Input
         real (kind=kind_phys), target :: flgmin (:)

         ! Input/output
         real (kind=kind_phys), target :: cv  (:)
         real (kind=kind_phys), target :: cvt (:)
         real (kind=kind_phys), target :: cvb (:)
         real (kind=kind_phys), target :: cnvqc_v(:,:)
         real (kind=kind_phys), target :: sup

         call dbgprint("cld_prop_setphys")

         this%cv     => cv
         this%cvt    => cvt
         this%cvb    => cvb
         this%flgmin => flgmin
         this%cnvqc_v => cnvqc_v
         this%sup     => sup

       end subroutine


!******************************************
! Radiation tendencies methods
!******************************************

                                      ! swh                 ! hlw
       subroutine rad_tend_set (this, htrsw, sfalb, coszen, htrlw, tsflw, semis, coszdg,  &
                                rqtk, hlwd, dtdtr, swhc, hlwc )
                                
         implicit none

         class(radiation_tendencies) :: this

         !! Outputs from grrad
         real (kind=kind_phys), target :: htrsw (:,:)
         real (kind=kind_phys), target :: sfalb (:)

         real (kind=kind_phys), target :: coszen(:)
         real (kind=kind_phys), target :: htrlw (:,:)
         real (kind=kind_phys), target :: tsflw (:)
         real (kind=kind_phys), target :: semis (:)

         ! Optional, used only by grrad
         real (kind=kind_phys), optional, target :: coszdg(:)

         ! Optional, used only by gbphys
         real (kind=kind_phys), optional, target :: rqtk (:)
         real (kind=kind_phys), optional, target :: hlwd (:,:,:)
         real (kind=kind_phys), optional, target :: dtdtr(:,:)

         real (kind=kind_phys), optional, target :: swhc (:,:)
         real (kind=kind_phys), optional, target :: hlwc (:,:)

         call dbgprint("rad_tend_set")

         select type (this)
           class is (radiation_tendencies)
             this%htrsw => htrsw
             this%sfalb => sfalb
             this%coszen => coszen
             this%htrlw => htrlw
             this%tsflw => tsflw
             this%semis => semis

             if (present(coszdg)) this%coszdg => coszdg

             if (present(rqtk)) this%rqtk => rqtk
             if (present(hlwd)) this%hlwd => hlwd
             if (present(dtdtr)) this%dtdtr => dtdtr

             if (present(swhc)) this%swhc => swhc
             if (present(hlwc)) this%hlwc => hlwc
         end select

       end subroutine rad_tend_set




!******************************************
! Model parameters methods
!******************************************

       subroutine mdl_param_print (this, msg)

         class(model_parameters) :: this
         character*(*) msg                 !declares a dummy argument

         if (this%me .eq. 0) then
           print *, msg
           print *, "MODEL PARAMETERS"
           print *, "----- ----------"
           print *,"ntcw : ", this%ntcw
           print *,"ncld : ", this%ncld
           print *,"ntoz : ", this%ntoz
           print *,"NTRAC : ", this%NTRAC
           print *,"levs : ", this%levs
           print *,"me : ", this%me
           print *,"lsoil : ", this%lsoil
           print *,"lsm : ", this%lsm
           print *,"nmtvr : ", this%nmtvr
           print *,"nrcm : ", this%nrcm
           print *,"levozp : ", this%levozp
           print *,"lonr : ", this%lonr
           print *,"latr : ", this%latr
           print *,"jcap : ", this%jcap
           print *,"num_p3d : ", this%num_p3d
           print *,"num_p2d : ", this%num_p2d
           print *,"npdf3d : ", this%npdf3d
           print *,"pl_coeff : ", this%pl_coeff
           print *,"ncw : ", this%ncw
           print *,"idate : ", this%idate
           print *,"idat : ", this%idat
           print *,"crtrh : ", this%crtrh
           print *,"cdmbgwd : ", this%cdmbgwd
           print *,"ccwf : ", this%ccwf
           print *,"dlqf : ", this%dlqf
           print *,"ctei_rm : ", this%ctei_rm
           print *,"cgwf : ", this%cgwf
           print *,"prslrd0 : ", this%prslrd0
           print *,"ras : ", this%ras
           print *,"pre_rad : ", this%pre_rad
           print *,"ldiag3d : ", this%ldiag3d
           print *,"lgocart : ", this%lgocart
           print *,"lssav_cpl : ", this%lssav_cpl
           print *,"flipv : ", this%flipv
           print *,"old_monin : ", this%old_monin
           print *,"cnvgwd : ", this%cnvgwd
           print *,"shal_cnv : ", this%shal_cnv
           print *,"sashal : ", this%sashal
           print *,"newsas : ", this%newsas
           print *,"cal_pre : ", this%cal_pre
           print *,"mom4ice : ", this%mom4ice
           print *,"mstrat : ", this%mstrat
           print *,"trans_trac : ", this%trans_trac
           print *,"nst_fcst : ", this%nst_fcst
           print *,"moist_adj : ", this%moist_adj
           print *,"thermodyn_id : ", this%thermodyn_id
           print *,"sfcpress_id : ", this%sfcpress_id
           print *,"gen_coord_hybrid : ", this%gen_coord_hybrid
           print *,"levr : ", this%levr
           print *,"lsidea : ", this%lsidea
           print *,"pdfcld : ", this%pdfcld
           print *,"shcnvcw : ", this%shcnvcw
           print *,"redrag : ", this%redrag
           print *,"hybedmf : ", this%hybedmf
           print *,"dspheat : ", this%dspheat

         endif

       end subroutine



!******************************************
! Dynamic_parameter methods
!******************************************

       subroutine dyn_param_setrad (this, xlon, xlat, sinlat, coslat, solhr, &
                                    IX, IM, kdt, jdate, solcon, icsdsw, icsdlw, &
                                    dtlw, dtsw, lsswr, lslwr, lssav, ipt, lprnt, deltim, &
                                    slag, sdec, cdec)
 
         implicit none

         class(dynamic_parameters) :: this

         real (kind=kind_phys), target :: xlon(:)
         real (kind=kind_phys), target :: xlat(:)
         real (kind=kind_phys), target :: sinlat(:)
         real (kind=kind_phys), target :: coslat(:)
         real (kind=kind_phys) :: solhr
         integer :: IX
         integer :: IM
         integer :: kdt
         logical :: lssav

         ! Radiation only
         integer :: jdate (8)
         real (kind=kind_phys) :: solcon

         integer, target :: icsdsw(:)
         integer, target :: icsdlw(:)
         real (kind=kind_phys)  :: dtlw
         real (kind=kind_phys)  :: dtsw 
         logical                :: lsswr 
         logical                :: lslwr 
         integer :: ipt                 
         logical :: lprnt
         real (kind=kind_phys) :: deltim

         ! These might be able to be computed elsewhere
         real (kind=kind_phys) :: slag  ! equation of time ( radian )
         real (kind=kind_phys) :: sdec  ! sin of the solar declination angle
         real (kind=kind_phys) :: cdec  ! cos of the solar declination angle
         ! End Radiation only

         call dbgprint("dyn_param_setrad")

         select type (this)
           class is (dynamic_parameters)
             this%xlon             => xlon
             this%xlat             => xlat
             this%sinlat           => sinlat
             this%coslat           => coslat
             this%solhr            = solhr
             this%IX               = IX
             this%IM               = IM
             this%kdt              = kdt
             this%jdate            = jdate
             this%solcon           = solcon
             this%icsdsw           => icsdsw
             this%icsdlw           => icsdlw
             this%dtlw             = dtlw
             this%dtsw             = dtsw
             this%lsswr            = lsswr
             this%lslwr            = lslwr
             this%lssav            = lssav
             this%ipt              = ipt
             this%lprnt            = lprnt
             this%deltim           = deltim

             this%slag = slag
             this%sdec = sdec
             this%cdec = cdec

           class default
             print *, "class default"
         end select

       end subroutine


       subroutine dyn_param_setphys (this, xlon, xlat, sinlat, coslat, solhr, &
                                     IX, IM, kdt, lssav, lat, dtp, dtf, clstp,       &
                                     nnp, nlons, fhour, slag, sdec, cdec )
         implicit none

         class(dynamic_parameters) :: this

         real (kind=kind_phys), target :: xlon(:)
         real (kind=kind_phys), target :: xlat(:)
         real (kind=kind_phys), target :: sinlat(:)
         real (kind=kind_phys), target :: coslat(:)
         real (kind=kind_phys) :: solhr
         integer :: IX
         integer :: IM
         integer :: kdt
         logical :: lssav


         ! Physics only
         integer :: lat
         real (kind=kind_phys) :: dtp
         real (kind=kind_phys) :: dtf
         real (kind=kind_phys) :: clstp
         integer :: nnp
         integer, target :: nlons(:)
         real (kind=kind_phys) :: fhour


         ! These might be able to be computed elsewhere
         real (kind=kind_phys) :: slag  ! equation of time ( radian )
         real (kind=kind_phys) :: sdec  ! sin of the solar declination angle
         real (kind=kind_phys) :: cdec  ! cos of the solar declination angle

         call dbgprint("dyn_param_setphys")

         select type (this)
           class is (dynamic_parameters)
             this%xlon             => xlon
             this%xlat             => xlat
             this%sinlat           => sinlat
             this%coslat           => coslat
             this%solhr            = solhr
             this%IX               = IX
             this%IM               = IM
             this%kdt              = kdt
             this%lssav            = lssav
             this%lat              = lat
             this%dtp              = dtp
             this%dtf              = dtf
             this%clstp            = clstp
             this%nnp              = nnp
             this%nlons            => nlons
             this%fhour            = fhour

             this%slag = slag
             this%sdec = sdec
             this%cdec = cdec

           class default
             print *, "class default"
         end select

       end subroutine




!******************************************
! Non type-bound procedures methods
!******************************************


       subroutine nuopc_phys_init (mdl, ntcw, ncld, ntoz, NTRAC, levs, me, lsoil, lsm, nmtvr, nrcm, levozp,  &
                                 lonr, latr, jcap, num_p3d, num_p2d, npdf3d, pl_coeff, ncw, crtrh, cdmbgwd,  &
                                 ccwf, dlqf, ctei_rm, cgwf, prslrd0, ras, pre_rad, ldiag3d, lgocart,  &
                                 lssav_cpl, flipv, old_monin, cnvgwd, shal_cnv, sashal, newsas, cal_pre, mom4ice,  &
                                 mstrat, trans_trac, nst_fcst, moist_adj, thermodyn_id, sfcpress_id,  &
                                 gen_coord_hybrid, levr, lsidea, pdfcld, shcnvcw, redrag, hybedmf, dspheat, &
                                 dxmaxin, dxminin, dxinvin, &
                                 ! For radiation
                                 si, ictm, isol, ico2, iaer, ialb, iems,                    &
                                 iovr_sw,iovr_lw,isubc_sw,isubc_lw,   &
                                 sas_shal,crick_proof,ccnorm,norad_precip,idate,iflip, nlunit)

         ! use physcons, only: dxmin, dxmax, dxinv

         implicit none

         ! Add intent ?
         type (model_parameters) :: mdl

         integer :: ntcw
         integer :: ncld
         integer :: ntoz
         integer :: NTRAC
         integer :: levs
         integer :: me
         integer :: lsoil
         integer :: lsm
         integer :: nmtvr
         integer :: nrcm
         integer :: levozp
         integer :: lonr
         integer :: latr
         integer :: jcap
         integer :: num_p3d
         integer :: num_p2d
         integer :: npdf3d
         integer :: pl_coeff
         integer :: ncw(2)

         real (kind=kind_phys) :: crtrh(3)
         real (kind=kind_phys) :: cdmbgwd(2)
         real (kind=kind_phys) :: ccwf(2)
         real (kind=kind_phys) :: dlqf(2)
         real (kind=kind_phys) :: ctei_rm(2)
         real (kind=kind_phys) :: cgwf(2)
         real (kind=kind_phys) :: prslrd0

         logical :: ras
         logical :: pre_rad
         logical :: ldiag3d
         logical :: lgocart
         logical :: lssav_cpl
         logical :: flipv
         logical :: old_monin
         logical :: cnvgwd
         logical :: shal_cnv
         logical :: sashal
         logical :: newsas
         logical :: cal_pre
         logical :: mom4ice
         logical :: mstrat
         logical :: trans_trac
         integer :: nst_fcst
         logical :: moist_adj
         integer :: thermodyn_id
         integer :: sfcpress_id
         logical :: gen_coord_hybrid
         integer :: levr
         logical :: lsidea
         logical :: pdfcld
         logical :: shcnvcw
         logical :: redrag
         logical :: hybedmf
         logical :: dspheat

         real(kind=kind_phys) :: dxmaxin, dxminin, dxinvin

         ! For rad_initialize
         real (kind=kind_phys), intent(in) :: si(levr+1)
         integer, intent(in) :: ictm, isol, ico2, iaer, ialb, iems
         integer, intent(in) :: iovr_sw, iovr_lw, isubc_sw, isubc_lw


! idate - in date_def in gfs_physics_initialize - set way up in the esmf gc
! iflip - is not the same as flipv iflip is integer, flipv is logical
         integer, intent(in) :: idate(4), iflip
         integer :: idat(8)    ! Different scheme for idate

! sas_shal is NOT the same as sashal :  sas_shal = sashal .and.  (.not. ras) 
         logical, intent(in) :: sas_shal, crick_proof, ccnorm, norad_precip

         integer, intent(in) :: nlunit      ! local namelist unit for set_soilveg, legacy, unused


         ! set up idat, used by radupdate
         idat = 0
         idat(1) = idate(4)
         idat(2) = idate(2)
         idat(3) = idate(3)
         idat(5) = idate(1)

         myme = me
         call dbgprint("entering nuopc_phys_init")

         mdl%idate            = idate
         mdl%idat             = idat
         mdl%ntcw             = ntcw
         mdl%ncld             = ncld
         mdl%ntoz             = ntoz
         mdl%NTRAC            = NTRAC
         mdl%levs             = levs
         mdl%me               = me
         mdl%lsoil            = lsoil
         mdl%lsm              = lsm
         mdl%nmtvr            = nmtvr
         mdl%nrcm             = nrcm
         mdl%levozp           = levozp
         mdl%lonr             = lonr
         mdl%latr             = latr
         mdl%jcap             = jcap
         mdl%num_p3d          = num_p3d
         mdl%num_p2d          = num_p2d
         mdl%npdf3d           = npdf3d
         mdl%pl_coeff         = pl_coeff
         mdl%ncw              = ncw
         mdl%crtrh            = crtrh
         mdl%cdmbgwd          = cdmbgwd
         mdl%ccwf             = ccwf
         mdl%dlqf             = dlqf
         mdl%ctei_rm          = ctei_rm
         mdl%cgwf             = cgwf
         mdl%prslrd0          = prslrd0
         mdl%ras              = ras
         mdl%pre_rad          = pre_rad
         mdl%ldiag3d          = ldiag3d
         mdl%lgocart          = lgocart
         mdl%lssav_cpl        = lssav_cpl
         mdl%flipv            = flipv
         mdl%old_monin        = old_monin
         mdl%cnvgwd           = cnvgwd
         mdl%shal_cnv         = shal_cnv
         mdl%sashal           = sashal
         mdl%newsas           = newsas
         mdl%cal_pre          = cal_pre
         mdl%mom4ice          = mom4ice
         mdl%mstrat           = mstrat
         mdl%trans_trac       = trans_trac
         mdl%nst_fcst         = nst_fcst
         mdl%moist_adj        = moist_adj
         mdl%thermodyn_id     = thermodyn_id
         mdl%sfcpress_id      = sfcpress_id
         mdl%gen_coord_hybrid = gen_coord_hybrid
         mdl%levr             = levr
         mdl%lsidea           = lsidea
         mdl%pdfcld           = pdfcld
         mdl%shcnvcw          = shcnvcw
         mdl%redrag           = redrag
         mdl%hybedmf          = hybedmf
         mdl%dspheat          = dspheat

         ! physcons module variables
         dxmax = dxmaxin
         dxmin = dxminin
         dxinv = dxinvin

        call rad_initialize                                             &
!  ---  inputs:
     &     ( si,levr,ictm,isol,ico2,iaer,ialb,iems,ntcw,                &
     &       num_p3d,npdf3d,ntoz,iovr_sw,iovr_lw,isubc_sw,isubc_lw,     &
     &       sas_shal,crick_proof,ccnorm,norad_precip,idate,iflip,me )
!  ---  outputs: ( none )

         call set_soilveg(me,nlunit)

       end subroutine



       subroutine phys_init_savein ( levr, ntcw, ncld, ntoz, NTRAC, levs, me, lsoil, lsm, nmtvr, nrcm, levozp,  &
                                 lonr, latr, jcap, num_p3d, num_p2d, npdf3d, pl_coeff, ncw, nst_fcst, &
                                 thermodyn_id, sfcpress_id, crtrh, cdmbgwd,  &
                                 ccwf, dlqf, ctei_rm, cgwf, prslrd0, ras, pre_rad, ldiag3d, lgocart,  &
                                 lssav_cpl, flipv, old_monin, cnvgwd, shal_cnv, sashal, newsas, cal_pre, mom4ice,  &
                                 mstrat, trans_trac, moist_adj, &
                                 gen_coord_hybrid, lsidea, pdfcld, shcnvcw, redrag, hybedmf, dspheat, &
                                 dxmax, dxmin, dxinv,  &
                                 ! For radiation
                                 si, ictm, isol, ico2, iaer, ialb, iems,                    &
                                 iovr_sw,iovr_lw,isubc_sw,isubc_lw,   &
                                 sas_shal,crick_proof,ccnorm,norad_precip,idate,iflip, nlunit)

         implicit none

         ! need a file unit and filename to write to
         integer, parameter :: funit = 99
         integer :: ios

         integer, intent(in) :: levr     ! This must be provided by caller - used as dimension for si
         integer :: ntcw
         integer :: ncld
         integer :: ntoz
         integer :: NTRAC
         integer :: levs
         integer :: me
         integer :: lsoil
         integer :: lsm
         integer :: nmtvr
         integer :: nrcm
         integer :: levozp
         integer :: lonr
         integer :: latr
         integer :: jcap
         integer :: num_p3d
         integer :: num_p2d
         integer :: npdf3d
         integer :: pl_coeff
         integer :: ncw(2)

         integer :: nst_fcst
         integer :: thermodyn_id
         integer :: sfcpress_id


         real (kind=kind_phys) :: crtrh(3)
         real (kind=kind_phys) :: cdmbgwd(2)
         real (kind=kind_phys) :: ccwf(2)
         real (kind=kind_phys) :: dlqf(2)
         real (kind=kind_phys) :: ctei_rm(2)
         real (kind=kind_phys) :: cgwf(2)
         real (kind=kind_phys) :: prslrd0

         logical :: ras
         logical :: pre_rad
         logical :: ldiag3d
         logical :: lgocart
         logical :: lssav_cpl
         logical :: flipv
         logical :: old_monin
         logical :: cnvgwd
         logical :: shal_cnv
         logical :: sashal
         logical :: newsas
         logical :: cal_pre
         logical :: mom4ice
         logical :: mstrat
         logical :: trans_trac
         logical :: moist_adj
         logical :: gen_coord_hybrid
         logical :: lsidea
         logical :: pdfcld
         logical :: shcnvcw
         logical :: redrag
         logical :: hybedmf
         logical :: dspheat

         real(kind=kind_phys) :: dxmax, dxmin, dxinv    ! based on max lon/lat

         ! Radiation option control parameters
         real (kind=kind_phys), intent(in) :: si(levr+1)
         integer, intent(in) :: ictm, isol, ico2, iaer, ialb, iems
         integer, intent(in) :: iovr_sw, iovr_lw, isubc_sw, isubc_lw
         integer, intent(in) :: idate(4), iflip
         logical, intent(in) :: sas_shal, crick_proof, ccnorm, norad_precip

         ! For set_vegtype - not needed
         integer, intent(in) :: nlunit

         call dbgprint("phys_init_savein")

         ! print *, "NUOPC DBG before write"

         ! open file
         open (funit, file='init_savein.dat', status='new', form='unformatted', iostat=ios)

         ! write each record
         write (funit, iostat=ios, err=900) levr

         write (funit, iostat=ios, err=900) ntcw, ncld, ntoz, NTRAC, levs, me, lsoil, lsm, nmtvr, nrcm, levozp,  &
                                 lonr, latr, jcap, num_p3d, num_p2d, npdf3d, pl_coeff, ncw, nst_fcst, &
                                 thermodyn_id, sfcpress_id, crtrh, cdmbgwd,  &
                                 ccwf, dlqf, ctei_rm, cgwf, prslrd0, ras, pre_rad, ldiag3d, lgocart,  &
                                 lssav_cpl, flipv, old_monin, cnvgwd, shal_cnv, sashal, newsas, cal_pre, mom4ice,  &
                                 mstrat, trans_trac, moist_adj, &
                                 gen_coord_hybrid, lsidea, pdfcld, shcnvcw, redrag, hybedmf, dspheat, &
                                 dxmax, dxmin, dxinv

         ! For radiation
         write (funit, iostat=ios, err=900) si
         write (funit, iostat=ios, err=900) ictm
         write (funit, iostat=ios, err=900) isol
         write (funit, iostat=ios, err=900) ico2
         write (funit, iostat=ios, err=900) iaer
         write (funit, iostat=ios, err=900) ialb
         write (funit, iostat=ios, err=900) iems
         write (funit, iostat=ios, err=900) iovr_sw
         write (funit, iostat=ios, err=900) iovr_lw
         write (funit, iostat=ios, err=900) isubc_sw
         write (funit, iostat=ios, err=900) isubc_lw
         write (funit, iostat=ios, err=900) sas_shal
         write (funit, iostat=ios, err=900) crick_proof
         write (funit, iostat=ios, err=900) ccnorm
         write (funit, iostat=ios, err=900) norad_precip
         write (funit, iostat=ios, err=900) idate
         write (funit, iostat=ios, err=900) iflip
         write (funit, iostat=ios, err=900) nlunit

         flush (funit, iostat=ios, err=900)

         ! close file
         close (funit, iostat=ios)

         ! report any errors
900      if (ios .ne. 0 ) print *, "DBG NUOPC IOERR : ", ios

       end subroutine


! Need to pass in the known or expected levr
       subroutine phys_init_readin ( levr, ntcw, ncld, ntoz, NTRAC, levs, me, lsoil, lsm, nmtvr, nrcm, levozp,  &
                                 lonr, latr, jcap, num_p3d, num_p2d, npdf3d, pl_coeff, ncw, nst_fcst, &
                                 thermodyn_id, sfcpress_id, crtrh, cdmbgwd,  &
                                 ccwf, dlqf, ctei_rm, cgwf, prslrd0, ras, pre_rad, ldiag3d, lgocart,  &
                                 lssav_cpl, flipv, old_monin, cnvgwd, shal_cnv, sashal, newsas, cal_pre, mom4ice,  &
                                 mstrat, trans_trac, moist_adj, &
                                 gen_coord_hybrid, lsidea, pdfcld, shcnvcw, redrag, hybedmf, dspheat, &
                                 dxmaxin, dxminin, dxinvin,  &
                                 ! For radiation
                                 si, ictm, isol, ico2, iaer, ialb, iems,                    &
                                 iovr_sw,iovr_lw,isubc_sw,isubc_lw,   &
                                 sas_shal,crick_proof,ccnorm,norad_precip,idate,iflip,nlunit)

         ! use physcons, only: dxmax, dxmin, dxinv

         implicit none

         ! file unit and filename to write to
         integer, parameter :: funit = 98
         integer :: ios

         integer :: levr_loc             ! local instance of levr for read
         integer, intent(in) :: levr     ! This must be provided by caller - used as dimension
         integer :: ntcw
         integer :: ncld
         integer :: ntoz
         integer :: NTRAC
         integer :: levs
         integer :: me
         integer :: lsoil
         integer :: lsm
         integer :: nmtvr
         integer :: nrcm
         integer :: levozp
         integer :: lonr
         integer :: latr
         integer :: jcap
         integer :: num_p3d
         integer :: num_p2d
         integer :: npdf3d
         integer :: pl_coeff
         integer :: ncw(2)

         integer :: nst_fcst
         integer :: thermodyn_id
         integer :: sfcpress_id

         real (kind=kind_phys) :: crtrh(3)
         real (kind=kind_phys) :: cdmbgwd(2)
         real (kind=kind_phys) :: ccwf(2)
         real (kind=kind_phys) :: dlqf(2)
         real (kind=kind_phys) :: ctei_rm(2)
         real (kind=kind_phys) :: cgwf(2)
         real (kind=kind_phys) :: prslrd0

         logical :: ras
         logical :: pre_rad
         logical :: ldiag3d
         logical :: lgocart
         logical :: lssav_cpl
         logical :: flipv
         logical :: old_monin
         logical :: cnvgwd
         logical :: shal_cnv
         logical :: sashal
         logical :: newsas
         logical :: cal_pre
         logical :: mom4ice
         logical :: mstrat
         logical :: trans_trac
         logical :: moist_adj
         logical :: gen_coord_hybrid
         logical :: lsidea
         logical :: pdfcld
         logical :: shcnvcw
         logical :: redrag
         logical :: hybedmf
         logical :: dspheat

         real(kind=kind_phys) :: dxmaxin, dxminin, dxinvin    ! based on max lon/lat

         ! Radiation option control parameters
         real (kind=kind_phys) :: si(levr+1)
         integer :: ictm, isol, ico2, iaer, ialb, iems
         integer :: iovr_sw, iovr_lw, isubc_sw, isubc_lw
         integer :: idate(4), iflip
         logical :: sas_shal, crick_proof, ccnorm, norad_precip

         ! for set_vegtype
         integer :: nlunit

         integer :: kindphys

         real (kind=kind_phys) :: si_loc(levr+1)

         call dbgprint("phys_init_readin")

         if (debug) then
         print *, "BEFORE READ"

         print *, "ntcw : ", ntcw
         print *, "ncld : ", ncld
         print *, "ntoz : ", ntoz
         print *, "NTRAC : ", NTRAC
         print *, "levs : ", levs
         print *, "me : ", me
         print *, "lsoil : ", lsoil
         print *, "lsm : ", lsm
         print *, "nmtvr : ", nmtvr
         print *, "nrcm : ", nrcm
         print *, "levozp : ", levozp
         print *, "lonr : ", lonr
         print *, "latr : ", latr
         print *, "jcap : ", jcap
         print *, "num_p3d : ", num_p3d
         print *, "num_p2d : ", num_p2d
         print *, "npdf3d : ", npdf3d
         print *, "pl_coeff : ", pl_coeff
         print *, "ncw : ", ncw
         print *, "crtrh : ", crtrh
         print *, "cdmbgwd : ", cdmbgwd
         print *, "ccwf : ", ccwf
         print *, "dlqf : ", dlqf
         print *, "ctei_rm : ", ctei_rm
         print *, "cgwf : ", cgwf
         print *, "prslrd0 : ", prslrd0
         print *, "ras : ", ras
         print *, "pre_rad : ", pre_rad
         print *, "ldiag3d : ", ldiag3d
         print *, "lgocart : ", lgocart
         print *, "lssav_cpl : ", lssav_cpl
         print *, "flipv : ", flipv
         print *, "old_monin : ", old_monin
         print *, "cnvgwd : ", cnvgwd
         print *, "shal_cnv : ", shal_cnv
         print *, "sashal : ", sashal
         print *, "newsas : ", newsas
         print *, "cal_pre : ", cal_pre
         print *, "mom4ice : ", mom4ice
         print *, "mstrat : ", mstrat
         print *, "trans_trac : ", trans_trac
         print *, "nst_fcst : ", nst_fcst
         print *, "moist_adj : ", moist_adj
         print *, "thermodyn_id : ", thermodyn_id
         print *, "sfcpress_id : ", sfcpress_id
         print *, "gen_coord_hybrid : ", gen_coord_hybrid
         print *, "levr : ", levr
         print *, "lsidea : ", lsidea
         print *, "pdfcld : ", pdfcld
         print *, "shcnvcw : ", shcnvcw
         print *, "redrag : ", redrag
         print *, "hybedmf : ", hybedmf
         print *, "dspheat : ", dspheat
         print *, "dxmaxin : ", dxmaxin
         print *, "dxminin : ", dxminin
         print *, "dxinvin : ", dxinvin
         print *, "si : ", si
         print *, "ictm : ", ictm
         print *, "isol : ", isol
         print *, "ico2 : ", ico2
         print *, "iaer : ", iaer
         print *, "ialb : ", ialb
         print *, "iems : ", iems
         print *, "iovr_sw : ", iovr_sw
         print *, "iovr_lw : ", iovr_lw
         print *, "isubc_sw : ", isubc_sw
         print *, "isubc_lw : ", isubc_lw
         print *, "sas_shal : ", sas_shal
         print *, "crick_proof : ", crick_proof
         print *, "ccnorm : ", ccnorm
         print *, "norad_precip : ", norad_precip
         print *, "idate : ", idate
         print *, "iflip : ", iflip
         end if


         ! open file
         open (funit, file='init_savein.dat', status='old', form='unformatted', iostat=ios)

         ! read each record
         read (funit, iostat=ios, err=900) levr_loc

         read (funit, iostat=ios, err=900) ntcw, ncld, ntoz, NTRAC, levs, me, lsoil, lsm, nmtvr, nrcm, levozp,  &
                                 lonr, latr, jcap, num_p3d, num_p2d, npdf3d, pl_coeff, ncw, nst_fcst, &
                                 thermodyn_id, sfcpress_id, crtrh, cdmbgwd,  &
                                 ccwf, dlqf, ctei_rm, cgwf, prslrd0, ras, pre_rad, ldiag3d, lgocart,  &
                                 lssav_cpl, flipv, old_monin, cnvgwd, shal_cnv, sashal, newsas, cal_pre, mom4ice,  &
                                 mstrat, trans_trac, moist_adj, &
                                 gen_coord_hybrid, lsidea, pdfcld, shcnvcw, redrag, hybedmf, dspheat, &
                                 dxmaxin, dxminin, dxinvin

         read (funit, iostat=ios, err=900) si
         read (funit, iostat=ios, err=900) ictm
         read (funit, iostat=ios, err=900) isol
         read (funit, iostat=ios, err=900) ico2
         read (funit, iostat=ios, err=900) iaer
         read (funit, iostat=ios, err=900) ialb
         read (funit, iostat=ios, err=900) iems
         read (funit, iostat=ios, err=900) iovr_sw
         read (funit, iostat=ios, err=900) iovr_lw
         read (funit, iostat=ios, err=900) isubc_sw
         read (funit, iostat=ios, err=900) isubc_lw
         read (funit, iostat=ios, err=900) sas_shal
         read (funit, iostat=ios, err=900) crick_proof
         read (funit, iostat=ios, err=900) ccnorm
         read (funit, iostat=ios, err=900) norad_precip
         read (funit, iostat=ios, err=900) idate
         read (funit, iostat=ios, err=900) iflip
         read (funit, iostat=ios, err=900) nlunit

         ! close file
         close (funit, iostat=ios)

         ! Set physcons module variables
         dxmax = dxmaxin
         dxmin = dxminin
         dxinv = dxinvin

         if (debug) then
         print *, "DEBUG IN DRIVER AFTER READ"
         print *, "ntcw : ", ntcw
         print *, "ncld : ", ncld
         print *, "ntoz : ", ntoz
         print *, "NTRAC : ", NTRAC
         print *, "levs : ", levs
         print *, "me : ", me
         print *, "lsoil : ", lsoil
         print *, "lsm : ", lsm
         print *, "nmtvr : ", nmtvr
         print *, "nrcm : ", nrcm
         print *, "levozp : ", levozp
         print *, "lonr : ", lonr
         print *, "latr : ", latr
         print *, "jcap : ", jcap
         print *, "num_p3d : ", num_p3d
         print *, "num_p2d : ", num_p2d
         print *, "npdf3d : ", npdf3d
         print *, "pl_coeff : ", pl_coeff
         print *, "ncw : ", ncw
         print *, "crtrh : ", crtrh
         print *, "cdmbgwd : ", cdmbgwd
         print *, "ccwf : ", ccwf
         print *, "dlqf : ", dlqf
         print *, "ctei_rm : ", ctei_rm
         print *, "cgwf : ", cgwf
         print *, "prslrd0 : ", prslrd0
         print *, "ras : ", ras
         print *, "pre_rad : ", pre_rad
         print *, "ldiag3d : ", ldiag3d
         print *, "lgocart : ", lgocart
         print *, "lssav_cpl : ", lssav_cpl
         print *, "flipv : ", flipv
         print *, "old_monin : ", old_monin
         print *, "cnvgwd : ", cnvgwd
         print *, "shal_cnv : ", shal_cnv
         print *, "sashal : ", sashal
         print *, "newsas : ", newsas
         print *, "cal_pre : ", cal_pre
         print *, "mom4ice : ", mom4ice
         print *, "mstrat : ", mstrat
         print *, "trans_trac : ", trans_trac
         print *, "nst_fcst : ", nst_fcst
         print *, "moist_adj : ", moist_adj
         print *, "thermodyn_id : ", thermodyn_id
         print *, "sfcpress_id : ", sfcpress_id
         print *, "gen_coord_hybrid : ", gen_coord_hybrid
         print *, "levr : ", levr
         print *, "levr_loc : ", levr_loc
         print *, "lsidea : ", lsidea
         print *, "pdfcld : ", pdfcld
         print *, "shcnvcw : ", shcnvcw
         print *, "redrag : ", redrag
         print *, "hybedmf : ", hybedmf
         print *, "dspheat : ", dspheat
         print *, "dxmaxin : ", dxmaxin
         print *, "dxminin : ", dxminin
         print *, "dxinvin : ", dxinvin
         print *, "si : ", si
         print *, "ictm : ", ictm
         print *, "isol : ", isol
         print *, "ico2 : ", ico2
         print *, "iaer : ", iaer
         print *, "ialb : ", ialb
         print *, "iems : ", iems
         print *, "iovr_sw : ", iovr_sw
         print *, "iovr_lw : ", iovr_lw
         print *, "isubc_sw : ", isubc_sw
         print *, "isubc_lw : ", isubc_lw
         print *, "sas_shal : ", sas_shal
         print *, "crick_proof : ", crick_proof
         print *, "ccnorm : ", ccnorm
         print *, "norad_precip : ", norad_precip
         print *, "idate : ", idate
         print *, "iflip : ", iflip
         end if ! debug

         ! report any errors
900      if (ios .ne. 0 ) print *, "DBG NUOPC IOERR : ", ios

       end subroutine




       subroutine nuopc_rad_update(mdl, dyn)

         implicit none

         type(model_parameters)     :: mdl
         type(dynamic_parameters)   :: dyn

         call radupdate ( mdl%idat, dyn%jdate, dyn%dtsw, dyn%deltim, dyn%lsswr, &
                          mdl%me, dyn%slag, dyn%sdec, dyn%cdec, dyn%solcon )

       end subroutine



       subroutine nuopc_rad_run (statein, sfc_prop, diags, intrfc_fld, cld_prop, rad_tend, mdl_parm, dyn_parm)

         implicit none

         type(state_fields_in)      :: statein
         type(sfc_properties)       :: sfc_prop
         type(diagnostics)          :: diags
         type(interface_fields)     :: intrfc_fld
         type(cloud_properties)     :: cld_prop
         type(radiation_tendencies) :: rad_tend
         type(model_parameters)     :: mdl_parm
         type(dynamic_parameters)   :: dyn_parm

         ! class(model_parameters)    :: mdl_parm    ! (Polymorphic type)

         ! call to grrad, using all of the nuopc field variables

         call dbgprint("entering nuopc_rad_run")


!!  ---  inputs:
!     &     ( prsi,prsl,prslk,tgrs,qgrs,tracer,vvl,slmsk,                &
!     &       xlon,xlat,tsfc,snowd,sncovr,snoalb,zorl,hprim,             &
!     &       alvsf,alnsf,alvwf,alnwf,facsf,facwf,fice,tisfc,            &
!     &       sinlat,coslat,solhr,jdate,solcon,                          &
!     &       cv,cvt,cvb,fcice,frain,rrime,flgmin,                       &
!     &       icsdsw,icsdlw, ntcw,ncld,ntoz, NTRAC,NFXR,                 &
!     &       dtlw,dtsw, lsswr,lslwr,lssav,                              &
!     &       IX, IM, LM, me, lprnt, ipt, kdt, deltaq,sup,cnvw,cnvc,     &
!!  ---  outputs:
!     &       htrsw,topfsw,sfcfsw,dswcmp,uswcmp,sfalb,coszen,coszdg,     &
!     &       htrlw,topflw,sfcflw,tsflw,semis,cldcov,                    &
!!  ---  input/output:
!     &       fluxr                                                      &
!!! ---  optional outputs:
!     &,      htrlw0,htrsw0,htrswb,htrlwb                                &
!     &     )

             call grrad(statein%prsi, statein%prsl, statein%prslk, statein%tgrs, statein%qgrs_rad,   &
                        statein%tracer, statein%vvl, sfc_prop%slmsk, dyn_parm%xlon, dyn_parm%xlat,     &
                        sfc_prop%tsfc, sfc_prop%snowd, sfc_prop%sncovr, sfc_prop%snoalb, sfc_prop%zorl,    &
                        sfc_prop%hprim, sfc_prop%alvsf, sfc_prop%alnsf, sfc_prop%alvwf, sfc_prop%alnwf,    &
                        sfc_prop%facsf, sfc_prop%facwf, sfc_prop%fice, sfc_prop%tisfc, dyn_parm%sinlat,    &
                        dyn_parm%coslat, dyn_parm%solhr, dyn_parm%jdate, dyn_parm%solcon, cld_prop%cv,     &
                        cld_prop%cvt, cld_prop%cvb, cld_prop%fcice, cld_prop%frain, cld_prop%rrime,        &
                        cld_prop%flgmin, dyn_parm%icsdsw, dyn_parm%icsdlw,                                 &
                        mdl_parm%ntcw-1, mdl_parm%ncld, mdl_parm%ntoz-1,  &
                        mdl_parm%NTRAC, diags%NFXR, dyn_parm%dtlw, dyn_parm%dtsw,       &
                        dyn_parm%lsswr, dyn_parm%lslwr, dyn_parm%lssav, dyn_parm%IX, dyn_parm%IM,          &
                        mdl_parm%levs, mdl_parm%me, dyn_parm%lprnt, dyn_parm%ipt, dyn_parm%kdt,              &
                        cld_prop%deltaq, cld_prop%sup, cld_prop%cnvw, cld_prop%cnvc,                       &
! Outputs
                        rad_tend%htrsw, diags%topfsw, intrfc_fld%sfcfsw, diags%dswcmp, diags%uswcmp,    &
                        rad_tend%sfalb, &
                        rad_tend%coszen, rad_tend%coszdg, rad_tend%htrlw, diags%topflw, intrfc_fld%sfcflw,&
                        rad_tend%tsflw, rad_tend%semis, cld_prop%cldcov, &
! In/Out
                        diags%fluxr)

         call dbgprint("leaving nuopc_rad_run")

       end subroutine







       subroutine nuopc_phys_run (statein, stateout, sfc, diag, intr, cld, rad, mdl, tbd, dyn)

         implicit none

         type(state_fields_in)      :: statein
         type(state_fields_out)     :: stateout
         type(sfc_properties)       :: sfc
         type(diagnostics)          :: diag
         type(interface_fields)     :: intr
         type(cloud_properties)     :: cld
         type(radiation_tendencies) :: rad
         type(model_parameters)     :: mdl
         type(tbd_ddt)              :: tbd
         type(dynamic_parameters)   :: dyn


         if (mdl%me .eq. 1) call dbgprint("entering nuopc_phys_run")

         call gbphys ( dyn%im, dyn%ix, mdl%levs, mdl%lsoil, mdl%lsm, mdl%ntrac,  &
                 mdl%ncld, mdl%ntoz, mdl%ntcw, mdl%nmtvr, mdl%nrcm, mdl%levozp,  &
                 mdl%lonr, mdl%latr, mdl%jcap, mdl%num_p3d, mdl%num_p2d, mdl%npdf3d, dyn%kdt,  &
                 dyn%lat, mdl%me, mdl%pl_coeff, dyn%nlons, mdl%ncw, cld%flgmin,  &
                 mdl%crtrh, mdl%cdmbgwd, mdl%ccwf, mdl%dlqf, mdl%ctei_rm, dyn%clstp,  &
                 mdl%cgwf, mdl%prslrd0, dyn%dtp, dyn%dtf, dyn%fhour, dyn%solhr,  &
                 dyn%slag, dyn%sdec, dyn%cdec, dyn%sinlat, dyn%coslat, statein%pgr,  &
                 statein%ugrs, statein%vgrs, statein%tgrs, statein%qgrs, statein%vvl, statein%prsi,  &
                 statein%prsl, statein%prslk, statein%prsik, statein%phii, statein%phil, tbd%rann,  &
                 tbd%prdout, tbd%poz, tbd%dpshc, sfc%hprime2, dyn%xlon, dyn%xlat,  &
                 sfc%slope, sfc%shdmin, sfc%shdmax, sfc%snoalb, sfc%tg3, sfc%slmsk,  &
                 sfc%vfrac, sfc%vtype, sfc%stype, sfc%uustar, sfc%oro, sfc%oro_uf,  &
                 rad%coszen, intr%sfcdsw, intr%sfcnsw, intr%sfcnirbmd, intr%sfcnirdfd, intr%sfcvisbmd,  &
                 intr%sfcvisdfd, intr%sfcnirbmu, intr%sfcnirdfu, intr%sfcvisbmu, intr%sfcvisdfu, intr%sfcdlw,  &
                 rad%tsflw, rad%semis, rad%sfalb, rad%htrsw, rad%swhc, rad%htrlw, rad%hlwc, rad%hlwd, mdl%lsidea, mdl%ras,  &
                 mdl%pre_rad, mdl%ldiag3d, mdl%lgocart, dyn%lssav,  &
                 mdl%lssav_cpl, tbd%xkzm_m, tbd%xkzm_h, tbd%xkzm_s, tbd%psautco, tbd%prautco,  &
                 tbd%evpco, tbd%wminco, mdl%pdfcld, mdl%shcnvcw, cld%sup, mdl%redrag, mdl%hybedmf, mdl%dspheat,  &
                 mdl%flipv, mdl%old_monin, mdl%cnvgwd, mdl%shal_cnv,  &
                 mdl%sashal, mdl%newsas, mdl%cal_pre, mdl%mom4ice, mdl%mstrat, mdl%trans_trac,  &
                 mdl%nst_fcst, mdl%moist_adj, mdl%thermodyn_id, mdl%sfcpress_id, mdl%gen_coord_hybrid,  &
                 mdl%levr, statein%adjtrc, dyn%nnp,    &
! In/Out
                 sfc%hice, sfc%fice, sfc%tisfc, sfc%tsfc, tbd%tprcp, cld%cv,  &
                 cld%cvb, cld%cvt, tbd%srflag, sfc%snowd, sfc%weasd, sfc%sncovr,  &
                 sfc%zorl, sfc%canopy, sfc%ffmm, sfc%ffhh, sfc%f10m, diag%srunoff,  &
                 diag%evbsa, diag%evcwa, diag%snohfa, diag%transa, diag%sbsnoa, diag%snowca,  &
                 diag%soilm, diag%tmpmin, diag%tmpmax, diag%dusfc, diag%dvsfc, diag%dtsfc,  &
                 diag%dqsfc, diag%totprcp, diag%gflux, diag%dlwsfc, diag%ulwsfc, diag%suntim,  &
                 diag%runoff, diag%ep, diag%cldwrk, diag%dugwd, diag%dvgwd, diag%psmean,  &
                 diag%cnvprcp, diag%spfhmin, diag%spfhmax, diag%rain, diag%rainc, diag%dt3dt,  &
                 diag%dq3dt, diag%du3dt, diag%dv3dt, diag%dqdt_v, cld%cnvqc_v, tbd%acv, tbd%acvb,  &
                 tbd%acvt, tbd%slc, tbd%smc, tbd%stc, tbd%upd_mf, tbd%dwn_mf,  &
                 tbd%det_mf, tbd%phy_f3d, tbd%phy_f2d, &
                 intr%dusfc_cpl, intr%dvsfc_cpl, intr%dtsfc_cpl, intr%dqsfc_cpl, intr%dlwsfc_cpl,  &
                 intr%dswsfc_cpl, intr%dnirbm_cpl, intr%dnirdf_cpl, intr%dvisbm_cpl, intr%dvisdf_cpl, intr%rain_cpl,  &
                 intr%nlwsfc_cpl, intr%nswsfc_cpl, intr%nnirbm_cpl, intr%nnirdf_cpl, intr%nvisbm_cpl, intr%nvisdf_cpl,  &
                 intr%xt, intr%xs, intr%xu, intr%xv, intr%xz, intr%zm,  &
                 intr%xtts, intr%xzts, intr%d_conv, intr%ifd, intr%dt_cool, intr%Qrain,  &
! Out
                 stateout%gt0, stateout%gq0, stateout%gu0, stateout%gv0, sfc%t2m, sfc%q2m,  &
                 diag%u10m, diag%v10m, diag%zlvl, diag%psurf, diag%hpbl, diag%pwat,  &
                 diag%t1, diag%q1, diag%u1, diag%v1, diag%chh, diag%cmm,  &
                 diag%dlwsfci, diag%ulwsfci, diag%dswsfci, diag%uswsfci, diag%dusfci, diag%dvsfci,  &
                 diag%dtsfci, diag%dqsfci, diag%gfluxi, diag%epi, diag%smcwlt2, diag%smcref2,  &
                 diag%wet1, diag%sr, rad%rqtk, rad%dtdtr, intr%dusfci_cpl, intr%dvsfci_cpl, intr%dtsfci_cpl,  &
                 intr%dqsfci_cpl, intr%dlwsfci_cpl, intr%dswsfci_cpl, intr%dnirbmi_cpl, intr%dnirdfi_cpl, intr%dvisbmi_cpl,  &
                 intr%dvisdfi_cpl, intr%nlwsfci_cpl, intr%nswsfci_cpl, intr%nnirbmi_cpl, intr%nnirdfi_cpl, intr%nvisbmi_cpl,  &
                 intr%nvisdfi_cpl, intr%t2mi_cpl, intr%q2mi_cpl, intr%u10mi_cpl, intr%v10mi_cpl, intr%tseai_cpl,  &
                 intr%psurfi_cpl, tbd%tref, tbd%z_c, tbd%c_0,  &
                 tbd%c_d, tbd%w_0, tbd%w_d  &
         )

       end subroutine




       subroutine rad_run_savein (statein, sfc_prop, diags, intrfc_fld, cld_prop, rad_tend, mdl_parm, dyn_parm)

         implicit none

         type(state_fields_in)      :: statein
         type(sfc_properties)       :: sfc_prop
         type(diagnostics)          :: diags
         type(interface_fields)     :: intrfc_fld
         type(cloud_properties)     :: cld_prop
         type(radiation_tendencies) :: rad_tend
         type(model_parameters)     :: mdl_parm
         type(dynamic_parameters)   :: dyn_parm

         ! need a file unit and filename to write to
         integer, parameter :: funit = 99
         integer :: ios

         ! open file
         open (funit, file='radrun_savein.dat', status='new', form='unformatted', iostat=ios)

         ! write each record
         write (funit, iostat=ios, err=900)   &
                        statein%prsi, statein%prsl, statein%prslk, statein%tgrs, statein%qgrs_rad,   &
                        statein%tracer, statein%vvl, sfc_prop%slmsk, dyn_parm%xlon, dyn_parm%xlat,     &
                        sfc_prop%tsfc, sfc_prop%snowd, sfc_prop%sncovr, sfc_prop%snoalb, sfc_prop%zorl,    &
                        sfc_prop%hprim, sfc_prop%alvsf, sfc_prop%alnsf, sfc_prop%alvwf, sfc_prop%alnwf,    &
                        sfc_prop%facsf, sfc_prop%facwf, sfc_prop%fice, sfc_prop%tisfc, dyn_parm%sinlat,    &
                        dyn_parm%coslat, dyn_parm%solhr, dyn_parm%jdate, dyn_parm%solcon, cld_prop%cv,     &
                        cld_prop%cvt, cld_prop%cvb, cld_prop%fcice, cld_prop%frain, cld_prop%rrime,        &
                        cld_prop%flgmin, dyn_parm%icsdsw, dyn_parm%icsdlw,                                 &
                        mdl_parm%ntcw, mdl_parm%ncld, mdl_parm%ntoz,  &
                        mdl_parm%NTRAC, diags%NFXR, dyn_parm%dtlw, dyn_parm%dtsw,       &
                        dyn_parm%lsswr, dyn_parm%lslwr, dyn_parm%lssav, dyn_parm%IX, dyn_parm%IM,          &
                        mdl_parm%levs, mdl_parm%me, dyn_parm%lprnt, dyn_parm%ipt, dyn_parm%kdt,              &
                        cld_prop%deltaq, cld_prop%sup, cld_prop%cnvw, cld_prop%cnvc, dyn_parm%deltim

         flush (funit, iostat=ios, err=900)

         ! close file
         close (funit, iostat=ios)

         ! report any errors
900      if (ios .ne. 0 ) print *, "DBG NUOPC IOERR : ", ios

       end subroutine


       subroutine rad_run_readin (statein, sfc_prop, diags, intrfc_fld, cld_prop, rad_tend, mdl_parm, dyn_parm)

         implicit none

         type(state_fields_in)      :: statein
         type(sfc_properties)       :: sfc_prop
         type(diagnostics)          :: diags
         type(interface_fields)     :: intrfc_fld
         type(cloud_properties)     :: cld_prop
         type(radiation_tendencies) :: rad_tend
         type(model_parameters)     :: mdl_parm
         type(dynamic_parameters)   :: dyn_parm

         integer, parameter :: funit = 98
         integer :: ios

         ! open file
         open (funit, file='radrun_savein.dat', status='old', form='unformatted', iostat=ios)

         ! read each record
         read (funit, iostat=ios, err=900)   &
                        statein%prsi, statein%prsl, statein%prslk, statein%tgrs, statein%qgrs_rad,   &
                        statein%tracer, statein%vvl, sfc_prop%slmsk, dyn_parm%xlon, dyn_parm%xlat,     &
                        sfc_prop%tsfc, sfc_prop%snowd, sfc_prop%sncovr, sfc_prop%snoalb, sfc_prop%zorl,    &
                        sfc_prop%hprim, sfc_prop%alvsf, sfc_prop%alnsf, sfc_prop%alvwf, sfc_prop%alnwf,    &
                        sfc_prop%facsf, sfc_prop%facwf, sfc_prop%fice, sfc_prop%tisfc, dyn_parm%sinlat,    &
                        dyn_parm%coslat, dyn_parm%solhr, dyn_parm%jdate, dyn_parm%solcon, cld_prop%cv,     &
                        cld_prop%cvt, cld_prop%cvb, cld_prop%fcice, cld_prop%frain, cld_prop%rrime,        &
                        cld_prop%flgmin, dyn_parm%icsdsw, dyn_parm%icsdlw,                                 &
                        mdl_parm%ntcw, mdl_parm%ncld, mdl_parm%ntoz,  &
                        mdl_parm%NTRAC, diags%NFXR, dyn_parm%dtlw, dyn_parm%dtsw,       &
                        dyn_parm%lsswr, dyn_parm%lslwr, dyn_parm%lssav, dyn_parm%IX, dyn_parm%IM,          &
                        mdl_parm%levs, mdl_parm%me, dyn_parm%lprnt, dyn_parm%ipt, dyn_parm%kdt,              &
                        cld_prop%deltaq, cld_prop%sup, cld_prop%cnvw, cld_prop%cnvc, dyn_parm%deltim

         ! close file
         close (funit, iostat=ios)

         ! report any errors
900      if (ios .ne. 0 ) print *, "DBG NUOPC IOERR : ", ios

       end subroutine


       subroutine rad_run_saveout (diags, intrfc_fld, cld_prop, rad_tend)

         implicit none

         type(diagnostics)          :: diags
         type(interface_fields)     :: intrfc_fld
         type(cloud_properties)     :: cld_prop
         type(radiation_tendencies) :: rad_tend

         ! need a file unit and filename to write to
         integer, parameter :: funit = 99
         integer :: ios

         ! open file
         open (funit, file='radrun_saveout.dat', status='new', form='unformatted', iostat=ios)

!         print *, "DBG NUOPC SAVE: ", 

         ! write each record
         write (funit, iostat=ios, err=900)   &
! Outputs
                        rad_tend%htrsw, diags%topfsw, intrfc_fld%sfcfsw, diags%dswcmp, diags%uswcmp,    &
                        rad_tend%sfalb, &
                        rad_tend%coszen, rad_tend%coszdg, rad_tend%htrlw, diags%topflw, intrfc_fld%sfcflw,&
                        rad_tend%tsflw, rad_tend%semis, cld_prop%cldcov, &
! In/Out
                        diags%fluxr

         flush (funit, iostat=ios, err=900)

         ! close file
         close (funit, iostat=ios)

         ! report any errors
900      if (ios .ne. 0 ) print *, "DBG NUOPC IOERR : ", ios

       end subroutine




       subroutine rad_run_readout (diags, intrfc_fld, cld_prop, rad_tend)

         implicit none
                        
         type(diagnostics)          :: diags
         type(interface_fields)     :: intrfc_fld
         type(cloud_properties)     :: cld_prop
         type(radiation_tendencies) :: rad_tend

         integer, parameter :: funit = 98
         integer :: ios

         ! open file
         open (funit, file='radrun_saveout.dat', status='old', form='unformatted', iostat=ios)

         ! read each record
         read (funit, iostat=ios, err=900)   &
! Outputs
                        rad_tend%htrsw, diags%topfsw, intrfc_fld%sfcfsw, diags%dswcmp, diags%uswcmp,    &
                        rad_tend%sfalb, &
                        rad_tend%coszen, rad_tend%coszdg, rad_tend%htrlw, diags%topflw, intrfc_fld%sfcflw,&
                        rad_tend%tsflw, rad_tend%semis, cld_prop%cldcov, &
! In/Out
                        diags%fluxr

         ! close file
         close (funit, iostat=ios)

         ! report any errors
900      if (ios .ne. 0 ) print *, "DBG NUOPC IOERR : ", ios

       end subroutine


       subroutine phys_run_savein (statein, sfc, diag, intr, cld, rad, mdl, tbd, dyn)

         implicit none

         type(state_fields_in)      :: statein
         type(sfc_properties)       :: sfc
         type(diagnostics)          :: diag
         type(interface_fields)     :: intr
         type(cloud_properties)     :: cld
         type(radiation_tendencies) :: rad
         type(model_parameters)     :: mdl
         type(tbd_ddt)              :: tbd
         type(dynamic_parameters)   :: dyn


         ! need a file unit and filename to write to
         integer, parameter :: funit = 99
         integer :: ios
         
         ! open file
         open (funit, file='physrun_savein.dat', status='new', form='unformatted', iostat=ios)

         ! write each record
         write (funit, iostat=ios, err=900) dyn%im, dyn%ix, mdl%levs, mdl%lsoil, mdl%lsm, mdl%ntrac,   &
                 mdl%ncld, mdl%ntoz, mdl%ntcw, mdl%nmtvr, mdl%nrcm, mdl%levozp,  &
                 mdl%lonr, mdl%latr, mdl%jcap, mdl%num_p3d, mdl%num_p2d, mdl%npdf3d, dyn%kdt,  &
                 dyn%lat, mdl%me, mdl%pl_coeff, dyn%nlons, mdl%ncw, cld%flgmin,  &
                 mdl%crtrh, mdl%cdmbgwd, mdl%ccwf, mdl%dlqf, mdl%ctei_rm, dyn%clstp,  &
                 mdl%cgwf, mdl%prslrd0, dyn%dtp, dyn%dtf, dyn%fhour, dyn%solhr,  &
                 dyn%slag, dyn%sdec, dyn%cdec, dyn%sinlat, dyn%coslat, statein%pgr,  &
                 statein%ugrs, statein%vgrs, statein%tgrs, statein%qgrs, statein%vvl, statein%prsi,  &
                 statein%prsl, statein%prslk, statein%prsik, statein%phii, statein%phil, tbd%rann,  &
                 tbd%prdout, tbd%poz, tbd%dpshc, sfc%hprime2, dyn%xlon, dyn%xlat,  &
                 sfc%slope, sfc%shdmin, sfc%shdmax, sfc%snoalb, sfc%tg3, sfc%slmsk,  &
                 sfc%vfrac, sfc%vtype, sfc%stype, sfc%uustar, sfc%oro, sfc%oro_uf,  &
                 rad%coszen, intr%sfcdsw, intr%sfcnsw, intr%sfcnirbmd, intr%sfcnirdfd, intr%sfcvisbmd,  &
                 intr%sfcvisdfd, intr%sfcnirbmu, intr%sfcnirdfu, intr%sfcvisbmu, intr%sfcvisdfu, intr%sfcdlw,  &
                 rad%tsflw, rad%semis, rad%sfalb, rad%htrsw, rad%swhc, rad%htrlw, rad%hlwc, rad%hlwd, mdl%lsidea, mdl%ras,  &
                 mdl%pre_rad, mdl%ldiag3d, mdl%lgocart, dyn%lssav,  &
                 mdl%lssav_cpl, tbd%xkzm_m, tbd%xkzm_h, tbd%xkzm_s, tbd%psautco, tbd%prautco,  &
                 tbd%evpco, tbd%wminco, mdl%pdfcld, mdl%shcnvcw, cld%sup, mdl%redrag, mdl%hybedmf, mdl%dspheat,  &
                 mdl%flipv, mdl%old_monin, mdl%cnvgwd, mdl%shal_cnv,  &
                 mdl%sashal, mdl%newsas, mdl%cal_pre, mdl%mom4ice, mdl%mstrat, mdl%trans_trac,  &
                 mdl%nst_fcst, mdl%moist_adj, mdl%thermodyn_id, mdl%sfcpress_id, mdl%gen_coord_hybrid,  &
                 mdl%levr, statein%adjtrc, dyn%nnp,    &
!! In/Out
                 sfc%hice, sfc%fice, sfc%tisfc, sfc%tsfc, tbd%tprcp, cld%cv,  &
                 cld%cvb, cld%cvt, tbd%srflag, sfc%snowd, sfc%weasd, sfc%sncovr,  &
                 sfc%zorl, sfc%canopy, sfc%ffmm, sfc%ffhh, sfc%f10m, diag%srunoff,  &
                 diag%evbsa, diag%evcwa, diag%snohfa, diag%transa, diag%sbsnoa, diag%snowca,  &
                 diag%soilm, diag%tmpmin, diag%tmpmax, diag%dusfc, diag%dvsfc, diag%dtsfc,  &
                 diag%dqsfc, diag%totprcp, diag%gflux, diag%dlwsfc, diag%ulwsfc, diag%suntim,  &
                 diag%runoff, diag%ep, diag%cldwrk, diag%dugwd, diag%dvgwd, diag%psmean,  &
                 diag%cnvprcp, diag%spfhmin, diag%spfhmax, diag%rain, diag%rainc, diag%dt3dt,  &
                 diag%dq3dt, diag%du3dt, diag%dv3dt, diag%dqdt_v, cld%cnvqc_v, &
                 tbd%acv, tbd%acvb, tbd%acvt, &

                 tbd%slc, tbd%smc, tbd%stc, tbd%upd_mf, tbd%dwn_mf,  &
                 tbd%det_mf, tbd%phy_f3d, tbd%phy_f2d, &
                 intr%dusfc_cpl, intr%dvsfc_cpl, intr%dtsfc_cpl, intr%dqsfc_cpl, intr%dlwsfc_cpl,  &
                 intr%dswsfc_cpl, intr%dnirbm_cpl, intr%dnirdf_cpl, intr%dvisbm_cpl, intr%dvisdf_cpl, intr%rain_cpl,  &
                 intr%nlwsfc_cpl, intr%nswsfc_cpl, intr%nnirbm_cpl, intr%nnirdf_cpl, intr%nvisbm_cpl, intr%nvisdf_cpl,  &
                 intr%xt, intr%xs, intr%xu, intr%xv, intr%xz, intr%zm,  &
                 intr%xtts, intr%xzts, intr%d_conv, intr%ifd, intr%dt_cool, intr%Qrain


         flush (funit, iostat=ios, err=900)

         ! close file
         close (funit, iostat=ios)

         ! report any errors
900      if (ios .ne. 0 ) print *, "DBG NUOPC IOERR : ", ios
       

       end subroutine




       subroutine phys_run_readin (statein, sfc, diag, intr, cld, rad, mdl, tbd, dyn)

         implicit none

         type(state_fields_in)      :: statein
         type(sfc_properties)       :: sfc
         type(diagnostics)          :: diag
         type(interface_fields)     :: intr
         type(cloud_properties)     :: cld
         type(radiation_tendencies) :: rad
         type(model_parameters)     :: mdl
         type(tbd_ddt)              :: tbd
         type(dynamic_parameters)   :: dyn


         integer, parameter :: funit = 98
         integer :: ios

         ! open file
         open (funit, file='physrun_savein.dat', status='old', form='unformatted', iostat=ios)

         ! read each record
         read (funit, iostat=ios, err=900) dyn%im, dyn%ix, mdl%levs, mdl%lsoil, mdl%lsm, mdl%ntrac,   &
                 mdl%ncld, mdl%ntoz, mdl%ntcw, mdl%nmtvr, mdl%nrcm, mdl%levozp,  &
                 mdl%lonr, mdl%latr, mdl%jcap, mdl%num_p3d, mdl%num_p2d, mdl%npdf3d, dyn%kdt,  &
                 dyn%lat, mdl%me, mdl%pl_coeff, dyn%nlons, mdl%ncw, cld%flgmin,  &
                 mdl%crtrh, mdl%cdmbgwd, mdl%ccwf, mdl%dlqf, mdl%ctei_rm, dyn%clstp,  &
                 mdl%cgwf, mdl%prslrd0, dyn%dtp, dyn%dtf, dyn%fhour, dyn%solhr,  &
                 dyn%slag, dyn%sdec, dyn%cdec, dyn%sinlat, dyn%coslat, statein%pgr,  &
                 statein%ugrs, statein%vgrs, statein%tgrs, statein%qgrs, statein%vvl, statein%prsi,  &
                 statein%prsl, statein%prslk, statein%prsik, statein%phii, statein%phil, tbd%rann,  &
                 tbd%prdout, tbd%poz, tbd%dpshc, sfc%hprime2, dyn%xlon, dyn%xlat,  &
                 sfc%slope, sfc%shdmin, sfc%shdmax, sfc%snoalb, sfc%tg3, sfc%slmsk,  &
                 sfc%vfrac, sfc%vtype, sfc%stype, sfc%uustar, sfc%oro, sfc%oro_uf,  &
                 rad%coszen, intr%sfcdsw, intr%sfcnsw, intr%sfcnirbmd, intr%sfcnirdfd, intr%sfcvisbmd,  &
                 intr%sfcvisdfd, intr%sfcnirbmu, intr%sfcnirdfu, intr%sfcvisbmu, intr%sfcvisdfu, intr%sfcdlw,  &
                 rad%tsflw, rad%semis, rad%sfalb, rad%htrsw, rad%swhc, rad%htrlw, rad%hlwc, rad%hlwd, mdl%lsidea, mdl%ras,  &
                 mdl%pre_rad, mdl%ldiag3d, mdl%lgocart, dyn%lssav,  &
                 mdl%lssav_cpl, tbd%xkzm_m, tbd%xkzm_h, tbd%xkzm_s, tbd%psautco, tbd%prautco,  &
                 tbd%evpco, tbd%wminco, mdl%pdfcld, mdl%shcnvcw, cld%sup, mdl%redrag, mdl%hybedmf, mdl%dspheat,  &
                 mdl%flipv, mdl%old_monin, mdl%cnvgwd, mdl%shal_cnv,  &
                 mdl%sashal, mdl%newsas, mdl%cal_pre, mdl%mom4ice, mdl%mstrat, mdl%trans_trac,  &
                 mdl%nst_fcst, mdl%moist_adj, mdl%thermodyn_id, mdl%sfcpress_id, mdl%gen_coord_hybrid,  &
                 mdl%levr, statein%adjtrc, dyn%nnp,    &
!! In/Out
                 sfc%hice, sfc%fice, sfc%tisfc, sfc%tsfc, tbd%tprcp, cld%cv,  &
                 cld%cvb, cld%cvt, tbd%srflag, sfc%snowd, sfc%weasd, sfc%sncovr,  &
                 sfc%zorl, sfc%canopy, sfc%ffmm, sfc%ffhh, sfc%f10m, diag%srunoff,  &
                 diag%evbsa, diag%evcwa, diag%snohfa, diag%transa, diag%sbsnoa, diag%snowca,  &
                 diag%soilm, diag%tmpmin, diag%tmpmax, diag%dusfc, diag%dvsfc, diag%dtsfc,  &
                 diag%dqsfc, diag%totprcp, diag%gflux, diag%dlwsfc, diag%ulwsfc, diag%suntim,  &
                 diag%runoff, diag%ep, diag%cldwrk, diag%dugwd, diag%dvgwd, diag%psmean,  &
                 diag%cnvprcp, diag%spfhmin, diag%spfhmax, diag%rain, diag%rainc, diag%dt3dt,  &
                 diag%dq3dt, diag%du3dt, diag%dv3dt, diag%dqdt_v, cld%cnvqc_v, tbd%acv, tbd%acvb,  &
                 tbd%acvt, tbd%slc, tbd%smc, tbd%stc, tbd%upd_mf, tbd%dwn_mf,  &
                 tbd%det_mf, tbd%phy_f3d, tbd%phy_f2d, &
                 intr%dusfc_cpl, intr%dvsfc_cpl, intr%dtsfc_cpl, intr%dqsfc_cpl, intr%dlwsfc_cpl,  &
                 intr%dswsfc_cpl, intr%dnirbm_cpl, intr%dnirdf_cpl, intr%dvisbm_cpl, intr%dvisdf_cpl, intr%rain_cpl,  &
                 intr%nlwsfc_cpl, intr%nswsfc_cpl, intr%nnirbm_cpl, intr%nnirdf_cpl, intr%nvisbm_cpl, intr%nvisdf_cpl,  &
                 intr%xt, intr%xs, intr%xu, intr%xv, intr%xz, intr%zm,  &
                 intr%xtts, intr%xzts, intr%d_conv, intr%ifd, intr%dt_cool, intr%Qrain

         ! close file
         close (funit, iostat=ios)

         ! report any errors
900      if (ios .ne. 0 ) print *, "DBG NUOPC IOERR : ", ios

       end subroutine




       subroutine phys_run_saveout (stateout, sfc, diag, intr, cld, rad, tbd)

         implicit none

         type(state_fields_out)     :: stateout
         type(sfc_properties)       :: sfc
         type(diagnostics)          :: diag
         type(interface_fields)     :: intr
         type(cloud_properties)     :: cld
         type(radiation_tendencies) :: rad
         type(tbd_ddt)              :: tbd


         ! need a file unit and filename to write to
         integer, parameter :: funit = 99
         integer :: ios

         ! open file
         open (funit, file='physrun_saveout.dat', status='new', form='unformatted', iostat=ios)

         write (funit, iostat=ios, err=900)   &
! In/Out
                 sfc%hice, sfc%fice, sfc%tisfc, sfc%tsfc, tbd%tprcp, cld%cv,  &
                 cld%cvb, cld%cvt, tbd%srflag, sfc%snowd, sfc%weasd, sfc%sncovr,  &
                 sfc%zorl, sfc%canopy, sfc%ffmm, sfc%ffhh, sfc%f10m, diag%srunoff,  &
                 diag%evbsa, diag%evcwa, diag%snohfa, diag%transa, diag%sbsnoa, diag%snowca,  &
                 diag%soilm, diag%tmpmin, diag%tmpmax, diag%dusfc, diag%dvsfc, diag%dtsfc,  &
                 diag%dqsfc, diag%totprcp, diag%gflux, diag%dlwsfc, diag%ulwsfc, diag%suntim,  &
                 diag%runoff, diag%ep, diag%cldwrk, diag%dugwd, diag%dvgwd, diag%psmean,  &
                 diag%cnvprcp, diag%spfhmin, diag%spfhmax, diag%rain, diag%rainc, diag%dt3dt,  &
                 diag%dq3dt, diag%du3dt, diag%dv3dt, diag%dqdt_v, cld%cnvqc_v, &
! PT - Possible bug in gsm for these fields
                 tbd%acv, tbd%acvb, tbd%acvt, &
                 tbd%slc, tbd%smc, tbd%stc, tbd%upd_mf, tbd%dwn_mf,  &
                 tbd%det_mf, tbd%phy_f3d, tbd%phy_f2d, &
                 intr%dusfc_cpl, intr%dvsfc_cpl, intr%dtsfc_cpl, intr%dqsfc_cpl, intr%dlwsfc_cpl,  &
                 intr%dswsfc_cpl, intr%dnirbm_cpl, intr%dnirdf_cpl, intr%dvisbm_cpl, intr%dvisdf_cpl, intr%rain_cpl,  &
                 intr%nlwsfc_cpl, intr%nswsfc_cpl, intr%nnirbm_cpl, intr%nnirdf_cpl, intr%nvisbm_cpl, intr%nvisdf_cpl,  &
                 intr%xt, intr%xs, intr%xu, intr%xv, intr%xz, intr%zm,  &
                 intr%xtts, intr%xzts, intr%d_conv, intr%ifd, intr%dt_cool, intr%Qrain,  &
! Out
                 stateout%gt0, stateout%gq0, stateout%gu0, stateout%gv0, sfc%t2m, sfc%q2m,  &
                 diag%u10m, diag%v10m, diag%zlvl, diag%psurf, diag%hpbl, diag%pwat,  &
                 diag%t1, diag%q1, diag%u1, diag%v1, diag%chh, diag%cmm,  &
                 diag%dlwsfci, diag%ulwsfci, diag%dswsfci, diag%uswsfci, diag%dusfci, diag%dvsfci,  &
                 diag%dtsfci, diag%dqsfci, diag%gfluxi, diag%epi, diag%smcwlt2, diag%smcref2,  &
                 diag%wet1, diag%sr, rad%rqtk, rad%dtdtr, intr%dusfci_cpl, intr%dvsfci_cpl, intr%dtsfci_cpl,  &
                 intr%dqsfci_cpl, intr%dlwsfci_cpl, intr%dswsfci_cpl, intr%dnirbmi_cpl, intr%dnirdfi_cpl, intr%dvisbmi_cpl,  &
                 intr%dvisdfi_cpl, intr%nlwsfci_cpl, intr%nswsfci_cpl, intr%nnirbmi_cpl, intr%nnirdfi_cpl, intr%nvisbmi_cpl,  &
                 intr%nvisdfi_cpl, intr%t2mi_cpl, intr%q2mi_cpl, intr%u10mi_cpl, intr%v10mi_cpl, intr%tseai_cpl,  &
                 intr%psurfi_cpl, tbd%tref, tbd%z_c, tbd%c_0,  &
                 tbd%c_d, tbd%w_0, tbd%w_d

         flush (funit, iostat=ios, err=900)

         ! close file
         close (funit, iostat=ios)

         ! report any errors
900      if (ios .ne. 0 ) print *, "DBG NUOPC IOERR : ", ios


         if (debug) then
           print *, "DBG phys_run_saveout - AFTER SAVE"
           print *, "stateout%gt0 : ", stateout%gt0
           print *, "stateout%gq0 : ", stateout%gq0
           print *, "stateout%gu0 : ", stateout%gu0
           print *, "stateout%gv0 : ", stateout%gv0
           print *, "sfc%t2m : ", sfc%t2m
           print *, "sfc%q2m : ", sfc%q2m
           print *, "diag%u10m : ", diag%u10m
           print *, "diag%v10m : ", diag%v10m
           print *, "diag%zlvl : ", diag%zlvl
           print *, "diag%psurf : ", diag%psurf
           print *, "diag%hpbl : ", diag%hpbl
           print *, "diag%pwat : ", diag%pwat
           print *, "diag%t1 : ", diag%t1
           print *, "diag%q1 : ", diag%q1
           print *, "diag%u1 : ", diag%u1
           print *, "diag%v1 : ", diag%v1
           print *, "diag%chh : ", diag%chh
           print *, "diag%cmm : ", diag%cmm
           print *, "diag%dlwsfci : ", diag%dlwsfci
           print *, "diag%ulwsfci : ", diag%ulwsfci
           print *, "diag%dswsfci : ", diag%dswsfci
           print *, "diag%uswsfci : ", diag%uswsfci
           print *, "diag%dusfci : ", diag%dusfci
           print *, "diag%dvsfci : ", diag%dvsfci
           print *, "diag%dtsfci : ", diag%dtsfci
           print *, "diag%dqsfci : ", diag%dqsfci
           print *, "diag%gfluxi : ", diag%gfluxi
           print *, "diag%epi : ", diag%epi
           print *, "diag%smcwlt2 : ", diag%smcwlt2
           print *, "diag%smcref2 : ", diag%smcref2
           print *, "diag%wet1 : ", diag%wet1
           print *, "diag%sr : ", diag%sr
           print *, "rad%rqtk : ", rad%rqtk
           print *, "rad%dtdtr : ", rad%dtdtr
           print *, "intr%dusfci_cpl : ", intr%dusfci_cpl
           print *, "intr%dvsfci_cpl : ", intr%dvsfci_cpl
           print *, "intr%dtsfci_cpl : ", intr%dtsfci_cpl
           print *, "intr%dqsfci_cpl : ", intr%dqsfci_cpl
           print *, "intr%dlwsfci_cpl : ", intr%dlwsfci_cpl
           print *, "intr%dswsfci_cpl : ", intr%dswsfci_cpl
           print *, "intr%dnirbmi_cpl : ", intr%dnirbmi_cpl
           print *, "intr%dnirdfi_cpl : ", intr%dnirdfi_cpl
           print *, "intr%dvisbmi_cpl : ", intr%dvisbmi_cpl
           print *, "intr%dvisdfi_cpl : ", intr%dvisdfi_cpl
           print *, "intr%nlwsfci_cpl : ", intr%nlwsfci_cpl
           print *, "intr%nswsfci_cpl : ", intr%nswsfci_cpl
           print *, "intr%nnirbmi_cpl : ", intr%nnirbmi_cpl
           print *, "intr%nnirdfi_cpl : ", intr%nnirdfi_cpl
           print *, "intr%nvisbmi_cpl : ", intr%nvisbmi_cpl
           print *, "intr%nvisdfi_cpl : ", intr%nvisdfi_cpl
           print *, "intr%t2mi_cpl : ", intr%t2mi_cpl
           print *, "intr%q2mi_cpl : ", intr%q2mi_cpl
           print *, "intr%u10mi_cpl : ", intr%u10mi_cpl
           print *, "intr%v10mi_cpl : ", intr%v10mi_cpl
           print *, "intr%tseai_cpl : ", intr%tseai_cpl
           print *, "intr%psurfi_cpl : ", intr%psurfi_cpl
           print *, "tbd%tref : ", tbd%tref
           print *, "tbd%z_c : ", tbd%z_c
           print *, "tbd%c_0 : ", tbd%c_0
           print *, "tbd%c_d : ", tbd%c_d
           print *, "tbd%w_0 : ", tbd%w_0
           print *, "tbd%w_d : ", tbd%w_d
         end if  ! debug

       end subroutine




       subroutine phys_run_readout (stateout, sfc, diag, intr, cld, rad, tbd)

         implicit none

         type(state_fields_out)     :: stateout
         type(sfc_properties)       :: sfc
         type(diagnostics)          :: diag
         type(interface_fields)     :: intr
         type(cloud_properties)     :: cld
         type(radiation_tendencies) :: rad
         type(tbd_ddt)              :: tbd


         integer, parameter :: funit = 98
         integer :: ios

         ! open file
         open (funit, file='physrun_saveout.dat', status='old', form='unformatted', iostat=ios)

         read (funit, iostat=ios, err=900)   &
! In/Out
                 sfc%hice, sfc%fice, sfc%tisfc, sfc%tsfc, tbd%tprcp, cld%cv,  &
                 cld%cvb, cld%cvt, tbd%srflag, sfc%snowd, sfc%weasd, sfc%sncovr,  &
                 sfc%zorl, sfc%canopy, sfc%ffmm, sfc%ffhh, sfc%f10m, diag%srunoff,  &
                 diag%evbsa, diag%evcwa, diag%snohfa, diag%transa, diag%sbsnoa, diag%snowca,  &
                 diag%soilm, diag%tmpmin, diag%tmpmax, diag%dusfc, diag%dvsfc, diag%dtsfc,  &
                 diag%dqsfc, diag%totprcp, diag%gflux, diag%dlwsfc, diag%ulwsfc, diag%suntim,  &
                 diag%runoff, diag%ep, diag%cldwrk, diag%dugwd, diag%dvgwd, diag%psmean,  &
                 diag%cnvprcp, diag%spfhmin, diag%spfhmax, diag%rain, diag%rainc, diag%dt3dt,  &
                 diag%dq3dt, diag%du3dt, diag%dv3dt, diag%dqdt_v, cld%cnvqc_v, tbd%acv, tbd%acvb,  &
                 tbd%acvt, tbd%slc, tbd%smc, tbd%stc, tbd%upd_mf, tbd%dwn_mf,  &
                 tbd%det_mf, tbd%phy_f3d, tbd%phy_f2d, &
                 intr%dusfc_cpl, intr%dvsfc_cpl, intr%dtsfc_cpl, intr%dqsfc_cpl, intr%dlwsfc_cpl,  &
                 intr%dswsfc_cpl, intr%dnirbm_cpl, intr%dnirdf_cpl, intr%dvisbm_cpl, intr%dvisdf_cpl, intr%rain_cpl,  &
                 intr%nlwsfc_cpl, intr%nswsfc_cpl, intr%nnirbm_cpl, intr%nnirdf_cpl, intr%nvisbm_cpl, intr%nvisdf_cpl,  &
                 intr%xt, intr%xs, intr%xu, intr%xv, intr%xz, intr%zm,  &
                 intr%xtts, intr%xzts, intr%d_conv, intr%ifd, intr%dt_cool, intr%Qrain,  &
! Out
                 stateout%gt0, stateout%gq0, stateout%gu0, stateout%gv0, sfc%t2m, sfc%q2m,  &
                 diag%u10m, diag%v10m, diag%zlvl, diag%psurf, diag%hpbl, diag%pwat,  &
                 diag%t1, diag%q1, diag%u1, diag%v1, diag%chh, diag%cmm,  &
                 diag%dlwsfci, diag%ulwsfci, diag%dswsfci, diag%uswsfci, diag%dusfci, diag%dvsfci,  &
                 diag%dtsfci, diag%dqsfci, diag%gfluxi, diag%epi, diag%smcwlt2, diag%smcref2,  &
                 diag%wet1, diag%sr, rad%rqtk, rad%dtdtr, intr%dusfci_cpl, intr%dvsfci_cpl, intr%dtsfci_cpl,  &
                 intr%dqsfci_cpl, intr%dlwsfci_cpl, intr%dswsfci_cpl, intr%dnirbmi_cpl, intr%dnirdfi_cpl, intr%dvisbmi_cpl,  &
                 intr%dvisdfi_cpl, intr%nlwsfci_cpl, intr%nswsfci_cpl, intr%nnirbmi_cpl, intr%nnirdfi_cpl, intr%nvisbmi_cpl,  &
                 intr%nvisdfi_cpl, intr%t2mi_cpl, intr%q2mi_cpl, intr%u10mi_cpl, intr%v10mi_cpl, intr%tseai_cpl,  &
                 intr%psurfi_cpl, tbd%tref, tbd%z_c, tbd%c_0,  &
                 tbd%c_d, tbd%w_0, tbd%w_d

         ! close file
         close (funit, iostat=ios)

         ! report any errors
900      if (ios .ne. 0 ) print *, "DBG NUOPC IOERR : ", ios

       end subroutine



       subroutine dbgprint(msg)
 
         implicit none
 
         integer :: me 
         character*(*) msg

         me = myme

         if (debug .and. (me .eq. 0)) then
           print *, me, ":DBG NUOPC: ", msg
         end if
 
       end subroutine 

       end module
