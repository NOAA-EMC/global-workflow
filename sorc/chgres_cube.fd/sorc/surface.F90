 module surface

!--------------------------------------------------------------------------
! Module surface
!
! Abstract: Process surface and nst fields.  Interpolates fields from
!    the input to target grids.  Adjusts soil temperature according
!    to differences in input and target grid terrain.  Rescales
!    soil moisture for soil type differences between input and target
!    grid.  Computes frozen portion of total soil moisture.
!
! Public Subroutines:
! -----------------
! surface_driver          Driver routine to process surface/nst data
!
! Public variables:
! -----------------
! Defined below.  "target" indicates field associated with the target grid.
! "input" indicates field associated with the input grid.
!
!--------------------------------------------------------------------------

 use esmf

 implicit none

 private

! noah land ice option is applied at these vegetation types.
 integer, parameter                 :: veg_type_landice_target = 15

! surface fields (not including nst)
 type(esmf_field), public           :: canopy_mc_target_grid
                                       ! canopy moisture content
 type(esmf_field), public           :: f10m_target_grid
                                       ! log((z0+10)*1/z0)
                                       ! See sfc_diff.f for details
 type(esmf_field), public           :: ffmm_target_grid
                                       ! log((z0+z1)*1/z0)
                                       ! See sfc_diff.f for details
 type(esmf_field), public           :: q2m_target_grid
                                       ! 2-m specific humidity
 type(esmf_field), public           :: seaice_depth_target_grid
                                       ! sea ice depth
 type(esmf_field), public           :: seaice_fract_target_grid
                                       ! sea ice fraction
 type(esmf_field), public           :: seaice_skin_temp_target_grid
                                       ! sea ice skin temperature
 type(esmf_field), public           :: skin_temp_target_grid
                                       ! skin temperature/sst
 type(esmf_field), public           :: srflag_target_grid
                                       ! snow/rain flag
 type(esmf_field), public           :: snow_liq_equiv_target_grid
                                       ! liquid equiv snow depth
 type(esmf_field), public           :: snow_depth_target_grid
                                       ! physical snow depth
 type(esmf_field), public           :: soil_temp_target_grid
                                       ! 3-d soil temperature
 type(esmf_field), public           :: soilm_liq_target_grid
                                       ! 3-d liquid soil moisture
 type(esmf_field), public           :: soilm_tot_target_grid
                                       ! 3-d total soil moisture
 type(esmf_field), public           :: t2m_target_grid
                                       ! 2-m temperatrure
 type(esmf_field), public           :: tprcp_target_grid
                                       ! precip
 type(esmf_field), public           :: ustar_target_grid
                                       ! friction velocity
 type(esmf_field), public           :: z0_target_grid
                                       ! roughness length

! nst fields
 type(esmf_field), public           :: c_d_target_grid
 type(esmf_field), public           :: c_0_target_grid
 type(esmf_field), public           :: d_conv_target_grid
 type(esmf_field), public           :: dt_cool_target_grid
 type(esmf_field), public           :: ifd_target_grid
 type(esmf_field), public           :: qrain_target_grid
 type(esmf_field), public           :: tref_target_grid
                                       ! reference temperature
 type(esmf_field), public           :: w_d_target_grid
 type(esmf_field), public           :: w_0_target_grid
 type(esmf_field), public           :: xs_target_grid
 type(esmf_field), public           :: xt_target_grid
 type(esmf_field), public           :: xu_target_grid
 type(esmf_field), public           :: xv_target_grid
 type(esmf_field), public           :: xz_target_grid
 type(esmf_field), public           :: xtts_target_grid
 type(esmf_field), public           :: xzts_target_grid
 type(esmf_field), public           :: z_c_target_grid
 type(esmf_field), public           :: zm_target_grid

 type(esmf_field)                   :: soil_type_from_input_grid
                                       ! soil type interpolated from
                                       ! input grid
 type(esmf_field)                   :: terrain_from_input_grid
                                       ! terrain height interpolated
                                       ! from input grid

 real, parameter, private           :: blim        = 5.5
                                       ! soil 'b' parameter limit
 real, parameter, private           :: frz_h2o     = 273.15
                                       ! melting pt water
 real, parameter, private           :: frz_ice     = 271.21
                                       ! melting pt sea ice
 real, parameter, private           :: grav        = 9.81
                                       ! gravity
 real, parameter, private           :: hlice       = 3.335E5
                                       ! latent heat of fusion

 public :: surface_driver

 contains

 subroutine surface_driver(localpet)

 use input_data, only                : cleanup_input_sfc_data, &
                                       cleanup_input_nst_data, &
                                       read_input_sfc_data, &
                                       read_input_nst_data

 use program_setup, only             : calc_soil_params_driver, &
                                       convert_nst

 use static_data, only               : get_static_fields, &
                                       cleanup_static_fields

 implicit none

 integer, intent(in)                :: localpet

!-----------------------------------------------------------------------
! Compute soil-based parameters.
!-----------------------------------------------------------------------

 call calc_soil_params_driver(localpet)

!-----------------------------------------------------------------------
! Get static data (like vegetation type) on the target grid.
!-----------------------------------------------------------------------

 call get_static_fields(localpet)

!-----------------------------------------------------------------------
! Read surface data on input grid.
!-----------------------------------------------------------------------

 call read_input_sfc_data(localpet)

!-----------------------------------------------------------------------
! Read nst data on input grid.
!-----------------------------------------------------------------------

 if (convert_nst) call read_input_nst_data(localpet)

!-----------------------------------------------------------------------
! Create surface field objects for target grid.
!-----------------------------------------------------------------------

 call create_surface_esmf_fields

!-----------------------------------------------------------------------
! Create nst field objects for target grid.
!-----------------------------------------------------------------------

 if (convert_nst) call create_nst_esmf_fields

!-----------------------------------------------------------------------
! Horizontally interpolate fields.
!-----------------------------------------------------------------------

 call interp(localpet)

!---------------------------------------------------------------------------------------------
! Adjust soil/landice column temperatures for any change in elevation between the
! input and target grids.
!---------------------------------------------------------------------------------------------

 call adjust_soilt_for_terrain

!---------------------------------------------------------------------------------------------
! Rescale soil moisture for changes in soil type between the input and target grids.
!---------------------------------------------------------------------------------------------

 call rescale_soil_moisture

!---------------------------------------------------------------------------------------------
! Compute liquid portion of total soil moisture.
!---------------------------------------------------------------------------------------------

 call calc_liq_soil_moisture

!---------------------------------------------------------------------------------------------
! Set z0 at land and sea ice.
!---------------------------------------------------------------------------------------------

 call roughness

!---------------------------------------------------------------------------------------------
! Perform some final qc checks.
!---------------------------------------------------------------------------------------------

 call qc_check

!---------------------------------------------------------------------------------------------
! Set flag values at land for nst fields.
!---------------------------------------------------------------------------------------------

 if (convert_nst) call nst_land_fill

!---------------------------------------------------------------------------------------------
! Free up memory.
!---------------------------------------------------------------------------------------------

 call cleanup_input_sfc_data

 if (convert_nst) call cleanup_input_nst_data

!---------------------------------------------------------------------------------------------
! Write data to file.
!---------------------------------------------------------------------------------------------

 call write_fv3_sfc_data_netcdf(localpet)

!---------------------------------------------------------------------------------------------
! Free up memory.
!---------------------------------------------------------------------------------------------

 if (convert_nst) call cleanup_target_nst_data

 call cleanup_target_sfc_data

 call cleanup_static_fields

 return

 end subroutine surface_driver

!---------------------------------------------------------------------------------------------
! Horizontally interpolate surface fields using esmf routines.
!---------------------------------------------------------------------------------------------

 subroutine interp(localpet)

 use esmf

 use input_data, only                : canopy_mc_input_grid,  &
                                       f10m_input_grid,  &
                                       ffmm_input_grid,  &
                                       landsea_mask_input_grid, &
                                       q2m_input_grid,  &
                                       seaice_depth_input_grid, &
                                       seaice_fract_input_grid, &
                                       seaice_skin_temp_input_grid, &
                                       skin_temp_input_grid, &
                                       snow_depth_input_grid, &
                                       snow_liq_equiv_input_grid, &
                                       soil_temp_input_grid, &
                                       soil_type_input_grid, &
                                       soilm_tot_input_grid, &
                                       srflag_input_grid, &
                                       t2m_input_grid,  &
                                       tprcp_input_grid,  &
                                       ustar_input_grid,  &
                                       veg_type_input_grid, &
                                       z0_input_grid, &
                                       c_d_input_grid, &
                                       c_0_input_grid, &
                                       d_conv_input_grid, &
                                       dt_cool_input_grid, &
                                       ifd_input_grid, &
                                       qrain_input_grid, &
                                       tref_input_grid, &
                                       w_d_input_grid, &
                                       w_0_input_grid, &
                                       xs_input_grid, &
                                       xt_input_grid, &
                                       xu_input_grid, &
                                       xv_input_grid, &
                                       xz_input_grid, &
                                       xtts_input_grid, &
                                       xzts_input_grid, &
                                       z_c_input_grid, &
                                       zm_input_grid, terrain_input_grid, &
                                       veg_type_landice_input

 use model_grid, only                : input_grid, target_grid, &
                                       i_target, j_target, &
                                       lsoil_target, &
                                       num_tiles_target_grid, &
                                       landmask_target_grid, &
                                       seamask_target_grid,  &
                                       latitude_target_grid

 use program_setup, only             : convert_nst

 use static_data, only               : veg_type_target_grid

 use search_util

 implicit none

 include 'mpif.h'

 integer, intent(in)                :: localpet

 integer                            :: l(1), u(1)
 integer                            :: i, j, ij, rc, tile
 integer                            :: clb_target(2), cub_target(2)
 integer                            :: isrctermprocessing
 integer(esmf_kind_i4), pointer     :: unmapped_ptr(:)
 integer(esmf_kind_i4), pointer     :: mask_input_ptr(:,:)
 integer(esmf_kind_i4), pointer     :: mask_target_ptr(:,:)
 integer(esmf_kind_i8), pointer     :: landmask_target_ptr(:,:)
 integer(esmf_kind_i8), allocatable :: mask_target_one_tile(:,:)
 integer(esmf_kind_i8), allocatable :: water_target_one_tile(:,:)
 integer(esmf_kind_i8), allocatable :: land_target_one_tile(:,:)
 integer(esmf_kind_i8), pointer     :: seamask_target_ptr(:,:)

 real(esmf_kind_r8), allocatable    :: data_one_tile(:,:)
 real(esmf_kind_r8), allocatable    :: data_one_tile_3d(:,:,:)
 real(esmf_kind_r8), allocatable    :: latitude_one_tile(:,:)
 real(esmf_kind_r8), pointer        :: canopy_mc_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: c_d_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: c_0_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: d_conv_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: dt_cool_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: ifd_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: qrain_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: tref_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: w_d_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: w_0_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: xs_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: xt_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: xu_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: xv_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: xz_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: xtts_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: xzts_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: z_c_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: zm_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: seaice_depth_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: seaice_fract_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: seaice_skin_temp_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: skin_temp_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: snow_depth_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: snow_liq_equiv_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: soil_temp_target_ptr(:,:,:)
 real(esmf_kind_r8), pointer        :: soil_type_from_input_ptr(:,:)
 real(esmf_kind_r8), pointer        :: soilm_tot_target_ptr(:,:,:)
 real(esmf_kind_r8), pointer        :: srflag_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: terrain_from_input_ptr(:,:)
 real(esmf_kind_r8), pointer        :: veg_type_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: z0_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: landmask_input_ptr(:,:)
 real(esmf_kind_r8), pointer        :: veg_type_input_ptr(:,:)
 real(esmf_kind_r8), allocatable    :: veg_type_target_one_tile(:,:)

 type(esmf_regridmethod_flag)       :: method
 type(esmf_routehandle)             :: regrid_bl_no_mask
 type(esmf_routehandle)             :: regrid_all_land
 type(esmf_routehandle)             :: regrid_land
 type(esmf_routehandle)             :: regrid_landice
 type(esmf_routehandle)             :: regrid_nonland
 type(esmf_routehandle)             :: regrid_seaice
 type(esmf_routehandle)             :: regrid_water

!-----------------------------------------------------------------------
! Interpolate fieids that do not require 'masked' interpolation.
!-----------------------------------------------------------------------

 method=ESMF_REGRIDMETHOD_BILINEAR

 isrctermprocessing = 1

 print*,"- CALL FieldRegridStore FOR NON-MASKED BILINEAR INTERPOLATION."
 call ESMF_FieldRegridStore(t2m_input_grid, &
                            t2m_target_grid, &
                            polemethod=ESMF_POLEMETHOD_NONE, &
                            srctermprocessing=isrctermprocessing, &
                            routehandle=regrid_bl_no_mask, &
                            regridmethod=method, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridStore", rc)

 print*,"- CALL Field_Regrid T2M."
 call ESMF_FieldRegrid(t2m_input_grid, &
                       t2m_target_grid, &
                       routehandle=regrid_bl_no_mask, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid Q2M."
 call ESMF_FieldRegrid(q2m_input_grid, &
                       q2m_target_grid, &
                       routehandle=regrid_bl_no_mask, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid TPRCP."
 call ESMF_FieldRegrid(tprcp_input_grid, &
                       tprcp_target_grid, &
                       routehandle=regrid_bl_no_mask, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ,  rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid F10M."
 call ESMF_FieldRegrid(f10m_input_grid, &
                       f10m_target_grid, &
                       routehandle=regrid_bl_no_mask, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ,  rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid FFMM."
 call ESMF_FieldRegrid(ffmm_input_grid, &
                       ffmm_target_grid, &
                       routehandle=regrid_bl_no_mask, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ,  rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid USTAR."
 call ESMF_FieldRegrid(ustar_input_grid, &
                       ustar_target_grid, &
                       routehandle=regrid_bl_no_mask, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ,  rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid SRFLAG."
 call ESMF_FieldRegrid(srflag_input_grid, &
                       srflag_target_grid, &
                       routehandle=regrid_bl_no_mask, & 
                       termorderflag=ESMF_TERMORDER_SRCSEQ,  rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL FieldGet FOR SRFLAG."
 call ESMF_FieldGet(srflag_target_grid, &
                    farrayPtr=srflag_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

!-----------------------------------------------------------------------
! This is a flag field.  Using neighbor was expensive.  So use
! bilinear and 'nint'.
!-----------------------------------------------------------------------

 srflag_target_ptr = nint(srflag_target_ptr)

 print*,"- CALL FieldRegridRelease."
 call ESMF_FieldRegridRelease(routehandle=regrid_bl_no_mask, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridRelease", rc)

!-----------------------------------------------------------------------
! Next, determine the sea ice fraction on target grid. 
!
! First, set the mask on the target and input grids.
!-----------------------------------------------------------------------

 print*,"- CALL GridAddItem FOR TARGET GRID."
 call ESMF_GridAddItem(target_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddItem", rc)

 print*,"- CALL GridGetItem FOR TARGET GRID."
 call ESMF_GridGetItem(target_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       farrayPtr=mask_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetItem", rc)

 print*,"- CALL FieldGet FOR TARGET GRID SEAMASK."
 call ESMF_FieldGet(seamask_target_grid, &
                    computationalLBound=clb_target, &
                    computationalUBound=cub_target, &
                    farrayPtr=seamask_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 mask_target_ptr = seamask_target_ptr

 print*,"- CALL GridAddItem FOR INPUT GRID SEAMASK."
 call ESMF_GridAddItem(input_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddItem", rc)

 print*,"- CALL FieldGet FOR INPUT GRID LANDMASK."
 call ESMF_FieldGet(landsea_mask_input_grid, &
                    farrayPtr=landmask_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL GridGetItem FOR INPUT GRID LANDMASK."
 call ESMF_GridGetItem(input_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       farrayPtr=mask_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetItem", rc)

 mask_input_ptr = 1
 where (nint(landmask_input_ptr) == 1) mask_input_ptr = 0

!-----------------------------------------------------------------------
! Interpolate.
!-----------------------------------------------------------------------

 method=ESMF_REGRIDMETHOD_CONSERVE

 isrctermprocessing = 1

 print*,"- CALL FieldRegridStore for sea ice fraction."
 call ESMF_FieldRegridStore(seaice_fract_input_grid, &
                            seaice_fract_target_grid, &
                            srcmaskvalues=(/0/), &
                            dstmaskvalues=(/0/), &
                            polemethod=ESMF_POLEMETHOD_NONE, &
                            srctermprocessing=isrctermprocessing, &
                            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
                            normtype=ESMF_NORMTYPE_FRACAREA, &
                            routehandle=regrid_nonland, &
                            regridmethod=method, &
                            unmappedDstList=unmapped_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridStore", rc)

 print*,"- CALL Field_Regrid for sea ice fraction."
 call ESMF_FieldRegrid(seaice_fract_input_grid, &
                       seaice_fract_target_grid, &
                       routehandle=regrid_nonland, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 if (localpet == 0) then
   allocate(data_one_tile(i_target,j_target))
   allocate(data_one_tile_3d(i_target,j_target,lsoil_target))
   allocate(mask_target_one_tile(i_target,j_target))
 else
   allocate(data_one_tile(0,0))
   allocate(data_one_tile_3d(0,0,0))
   allocate(mask_target_one_tile(0,0))
 endif

 print*,"- CALL FieldGet FOR TARGET grid sea ice fraction."
 call ESMF_FieldGet(seaice_fract_target_grid, &
                    farrayPtr=seaice_fract_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 l = lbound(unmapped_ptr)
 u = ubound(unmapped_ptr)

 do ij = l(1), u(1)
   call ij_to_i_j(unmapped_ptr(ij), i_target, j_target, i, j)
   seaice_fract_target_ptr(i,j) = -9999.9 ! flag value for missing point
                               ! which will be replaced in routine
                               ! "search".
 enddo

 if (localpet == 0) then
   allocate(latitude_one_tile(i_target,j_target))
 else
   allocate(latitude_one_tile(0,0))
 endif

 do tile = 1, num_tiles_target_grid

   print*,"- CALL FieldGather FOR TARGET GRID SEAICE FRACTION TILE: ", tile
   call ESMF_FieldGather(seaice_fract_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   print*,"- CALL FieldGather FOR TARGET GRID MASK TILE: ", tile
   call ESMF_FieldGather(seamask_target_grid, mask_target_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   print*,"- CALL FieldGather FOR TARGET LATITUDE TILE: ", tile
   call ESMF_FieldGather(latitude_target_grid, latitude_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     call search(data_one_tile, mask_target_one_tile, i_target, j_target, tile, 91, &
                 latitude=latitude_one_tile)
   endif

   print*,"- CALL FieldGather FOR TARGET LANDMASK TILE: ", tile
   call ESMF_FieldGather(landmask_target_grid, mask_target_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     do j = 1, j_target
     do i = 1, i_target
       if (data_one_tile(i,j) < 0.15) data_one_tile(i,j) = 0.0
       if (data_one_tile(i,j) >= 0.15) mask_target_one_tile(i,j) = 2
     enddo
     enddo
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SEAICE FRACTION TILE: ", tile
   call ESMF_FieldScatter(seaice_fract_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldScatter FOR TARGET LANDMASK TILE: ", tile
   call ESMF_FieldScatter(landmask_target_grid, mask_target_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

 enddo

 deallocate(latitude_one_tile)

 print*,"- CALL FieldRegridRelease."
 call ESMF_FieldRegridRelease(routehandle=regrid_nonland, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridRelease", rc)

!---------------------------------------------------------------------------------------------
! Now interpolate other sea ice related fields.  Since we know what points are ice on
! the target grid, reset the target grid mask.
!---------------------------------------------------------------------------------------------

 mask_input_ptr = 0
 where (nint(landmask_input_ptr) == 2) mask_input_ptr = 1

 print*,"- CALL FieldGet FOR TARGET land sea mask."
 call ESMF_FieldGet(landmask_target_grid, &
                    farrayPtr=landmask_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 mask_target_ptr = 0 
 do j = clb_target(2), cub_target(2)
 do i = clb_target(1), cub_target(1)
   if (landmask_target_ptr(i,j) == 2) mask_target_ptr(i,j) = 1
 enddo
 enddo

 method=ESMF_REGRIDMETHOD_NEAREST_STOD
 isrctermprocessing = 1

 print*,"- CALL FieldRegridStore for 3d seaice fields."
 call ESMF_FieldRegridStore(soil_temp_input_grid, &
                            soil_temp_target_grid, &
                            srcmaskvalues=(/0/), &
                            dstmaskvalues=(/0/), &
                            polemethod=ESMF_POLEMETHOD_NONE, &
                            srctermprocessing=isrctermprocessing, &
                            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
                            normtype=ESMF_NORMTYPE_FRACAREA, &
                            routehandle=regrid_seaice, &
                            regridmethod=method, &
                            unmappedDstList=unmapped_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridStore", rc)

 print*,"- CALL Field_Regrid for soil temperature over seaice."
 call ESMF_FieldRegrid(soil_temp_input_grid, &
                       soil_temp_target_grid, &
                       routehandle=regrid_seaice, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)
  
 print*,"- CALL FieldGet FOR TARGET grid soil temperature over seaice."
 call ESMF_FieldGet(soil_temp_target_grid, &
                    farrayPtr=soil_temp_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL Field_Regrid for sea ice depth."
 call ESMF_FieldRegrid(seaice_depth_input_grid, &
                       seaice_depth_target_grid, &
                       routehandle=regrid_seaice, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL FieldGet FOR TARGET grid sea ice depth."
 call ESMF_FieldGet(seaice_depth_target_grid, &
                    farrayPtr=seaice_depth_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL Field_Regrid for snow depth."
 call ESMF_FieldRegrid(snow_depth_input_grid, &
                       snow_depth_target_grid, &
                       routehandle=regrid_seaice, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL FieldGet FOR TARGET grid snow depth."
 call ESMF_FieldGet(snow_depth_target_grid, &
                    farrayPtr=snow_depth_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL Field_Regrid for snow liq equiv."
 call ESMF_FieldRegrid(snow_liq_equiv_input_grid, &
                       snow_liq_equiv_target_grid, &
                       routehandle=regrid_seaice, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL FieldGet FOR TARGET grid snow liq equiv."
 call ESMF_FieldGet(snow_liq_equiv_target_grid, &
                    farrayPtr=snow_liq_equiv_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL Field_Regrid for sea ice skin temp."
 call ESMF_FieldRegrid(seaice_skin_temp_input_grid, &
                       seaice_skin_temp_target_grid, &
                       routehandle=regrid_seaice, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL FieldGet FOR TARGET grid sea ice skin temp."
 call ESMF_FieldGet(seaice_skin_temp_target_grid, &
                    farrayPtr=seaice_skin_temp_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 l = lbound(unmapped_ptr)
 u = ubound(unmapped_ptr)

 do ij = l(1), u(1)
   call ij_to_i_j(unmapped_ptr(ij), i_target, j_target, i, j)
   seaice_depth_target_ptr(i,j) = -9999.9 
   snow_depth_target_ptr(i,j) = -9999.9 
   snow_liq_equiv_target_ptr(i,j) = -9999.9 
   seaice_skin_temp_target_ptr(i,j) = -9999.9 
   soil_temp_target_ptr(i,j,:) = -9999.9
 enddo

 do tile = 1, num_tiles_target_grid

   print*,"- CALL FieldGather FOR TARGET GRID SEAICE DEPTH TILE: ", tile
   call ESMF_FieldGather(seaice_depth_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   print*,"- CALL FieldGather FOR TARGET LANDMASK TILE: ", tile
   call ESMF_FieldGather(landmask_target_grid, mask_target_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     where(mask_target_one_tile == 1) mask_target_one_tile = 0
     where(mask_target_one_tile == 2) mask_target_one_tile = 1
     call search(data_one_tile, mask_target_one_tile, i_target, j_target, tile, 92)
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SEAICE DEPTH TILE: ", tile
   call ESMF_FieldScatter(seaice_depth_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldGather FOR TARGET GRID SNOW DEPTH TILE: ", tile
   call ESMF_FieldGather(snow_depth_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     call search(data_one_tile, mask_target_one_tile, i_target, j_target, tile, 66)
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SNOW DEPTH TILE: ", tile
   call ESMF_FieldScatter(snow_depth_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldGather FOR TARGET GRID SNOW LIQ EQUIV TILE: ", tile
   call ESMF_FieldGather(snow_liq_equiv_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     call search(data_one_tile, mask_target_one_tile, i_target, j_target, tile, 65)
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SNOW LIQ EQUIV TILE: ", tile
   call ESMF_FieldScatter(snow_liq_equiv_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldGather FOR TARGET GRID SEAICE SKIN TEMP: ", tile
   call ESMF_FieldGather(seaice_skin_temp_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     call search(data_one_tile, mask_target_one_tile, i_target, j_target, tile, 21)
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SEAICE SKIN TEMP: ", tile
   call ESMF_FieldScatter(seaice_skin_temp_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldGather FOR TARGET GRID SEAICE COLUMN TEMP: ", tile
   call ESMF_FieldGather(soil_temp_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     do j = 1, lsoil_target
       data_one_tile = data_one_tile_3d(:,:,j)
       call search(data_one_tile, mask_target_one_tile, i_target, j_target, tile, 21)
       data_one_tile_3d(:,:,j) = data_one_tile
     enddo
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SEAICE COLUMN TEMP: ", tile
   call ESMF_FieldScatter(soil_temp_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

 enddo

 print*,"- CALL FieldRegridRelease."
 call ESMF_FieldRegridRelease(routehandle=regrid_seaice, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridRelease", rc)

!---------------------------------------------------------------------------------------------
! Now interpolate water fields. 
!---------------------------------------------------------------------------------------------

 mask_input_ptr = 0
 where (nint(landmask_input_ptr) == 0) mask_input_ptr = 1

 mask_target_ptr = 0
 where (landmask_target_ptr == 0) mask_target_ptr = 1

 method=ESMF_REGRIDMETHOD_CONSERVE
 isrctermprocessing = 1

 print*,"- CALL FieldRegridStore for water fields."
 call ESMF_FieldRegridStore(skin_temp_input_grid, &
                            skin_temp_target_grid, &
                            srcmaskvalues=(/0/), &
                            dstmaskvalues=(/0/), &
                            polemethod=ESMF_POLEMETHOD_NONE, &
                            srctermprocessing=isrctermprocessing, &
                            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
                            normtype=ESMF_NORMTYPE_FRACAREA, &
                            routehandle=regrid_water, &
                            regridmethod=method, &
                            unmappedDstList=unmapped_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridStore", rc)
 
 print*,"- CALL Field_Regrid for skin temperature over water."
 call ESMF_FieldRegrid(skin_temp_input_grid, &
                       skin_temp_target_grid, &
                       routehandle=regrid_water, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL FieldGet FOR TARGET skin temperature."
 call ESMF_FieldGet(skin_temp_target_grid, &
                    farrayPtr=skin_temp_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL Field_Regrid for z0 over water."
 call ESMF_FieldRegrid(z0_input_grid, &
                       z0_target_grid, &
                       routehandle=regrid_water, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL FieldGet FOR TARGET z0."
 call ESMF_FieldGet(z0_target_grid, &
                    farrayPtr=z0_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 l = lbound(unmapped_ptr)
 u = ubound(unmapped_ptr)

 do ij = l(1), u(1)
   call ij_to_i_j(unmapped_ptr(ij), i_target, j_target, i, j)
   skin_temp_target_ptr(i,j) = -9999.9 
   z0_target_ptr(i,j)        = -9999.9 
 enddo

 if (convert_nst) then

   print*,"- CALL Field_Regrid for c_d over water."
   call ESMF_FieldRegrid(c_d_input_grid, &
                         c_d_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for c_0 over water."
   call ESMF_FieldRegrid(c_0_input_grid, &
                         c_0_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for d_conv over water."
   call ESMF_FieldRegrid(d_conv_input_grid, &
                         d_conv_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for dt_cool over water."
   call ESMF_FieldRegrid(dt_cool_input_grid, &
                         dt_cool_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for ifd over water."
   call ESMF_FieldRegrid(ifd_input_grid, &
                         ifd_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for qrain over water."
   call ESMF_FieldRegrid(qrain_input_grid, &
                         qrain_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for tref over water."
   call ESMF_FieldRegrid(tref_input_grid, &
                         tref_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for w_d over water."
   call ESMF_FieldRegrid(w_d_input_grid, &
                         w_d_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for w_0 over water."
   call ESMF_FieldRegrid(w_0_input_grid, &
                         w_0_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for xs over water."
   call ESMF_FieldRegrid(xs_input_grid, &
                         xs_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for xt over water."
   call ESMF_FieldRegrid(xt_input_grid, &
                         xt_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for xu over water."
   call ESMF_FieldRegrid(xu_input_grid, &
                         xu_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for xv over water."
   call ESMF_FieldRegrid(xv_input_grid, &
                         xv_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for xz over water."
   call ESMF_FieldRegrid(xz_input_grid, &
                         xz_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for xtts over water."
   call ESMF_FieldRegrid(xtts_input_grid, &
                         xtts_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for xzts over water."
   call ESMF_FieldRegrid(xzts_input_grid, &
                         xzts_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for z_c over water."
   call ESMF_FieldRegrid(z_c_input_grid, &
                         z_c_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

   print*,"- CALL Field_Regrid for zm over water."
   call ESMF_FieldRegrid(zm_input_grid, &
                         zm_target_grid, &
                         routehandle=regrid_water, &
                         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldRegrid", rc)

! Tag unmapped points

   print*,"- CALL FieldGet FOR TARGET c_d."
   call ESMF_FieldGet(c_d_target_grid, &
                      farrayPtr=c_d_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET c_0."
   call ESMF_FieldGet(c_0_target_grid, &
                      farrayPtr=c_0_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET d_conv."
   call ESMF_FieldGet(d_conv_target_grid, &
                      farrayPtr=d_conv_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET dt_cool."
   call ESMF_FieldGet(dt_cool_target_grid, &
                      farrayPtr=dt_cool_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET ifd."
   call ESMF_FieldGet(ifd_target_grid, &
                      farrayPtr=ifd_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   ifd_target_ptr = float(nint(ifd_target_ptr))

   print*,"- CALL FieldGet FOR TARGET qrain."
   call ESMF_FieldGet(qrain_target_grid, &
                      farrayPtr=qrain_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET tref."
   call ESMF_FieldGet(tref_target_grid, &
                      farrayPtr=tref_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET w_d."
   call ESMF_FieldGet(w_d_target_grid, &
                      farrayPtr=w_d_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET w_0."
   call ESMF_FieldGet(w_0_target_grid, &
                      farrayPtr=w_0_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET xs."
   call ESMF_FieldGet(xs_target_grid, &
                      farrayPtr=xs_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET xt."
   call ESMF_FieldGet(xt_target_grid, &
                      farrayPtr=xt_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET xu."
   call ESMF_FieldGet(xu_target_grid, &
                      farrayPtr=xu_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET xv."
   call ESMF_FieldGet(xv_target_grid, &
                      farrayPtr=xv_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET xz."
   call ESMF_FieldGet(xz_target_grid, &
                      farrayPtr=xz_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET xtts."
   call ESMF_FieldGet(xtts_target_grid, &
                      farrayPtr=xtts_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET xzts."
   call ESMF_FieldGet(xzts_target_grid, &
                      farrayPtr=xzts_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET z_c."
   call ESMF_FieldGet(z_c_target_grid, &
                      farrayPtr=z_c_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   print*,"- CALL FieldGet FOR TARGET zm."
   call ESMF_FieldGet(zm_target_grid, &
                      farrayPtr=zm_target_ptr, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGet", rc)

   do ij = l(1), u(1)
     call ij_to_i_j(unmapped_ptr(ij), i_target, j_target, i, j)
     c_d_target_ptr(i,j) = -9999.9 
     c_0_target_ptr(i,j) = -9999.9 
     d_conv_target_ptr(i,j) = -9999.9 
     dt_cool_target_ptr(i,j) = -9999.9 
     ifd_target_ptr(i,j) = -9999.9 
     qrain_target_ptr(i,j) = -9999.9 
     tref_target_ptr(i,j) = -9999.9 
     w_d_target_ptr(i,j) = -9999.9 
     w_0_target_ptr(i,j) = -9999.9 
     xs_target_ptr(i,j) = -9999.9 
     xt_target_ptr(i,j) = -9999.9 
     xu_target_ptr(i,j) = -9999.9 
     xv_target_ptr(i,j) = -9999.9 
     xz_target_ptr(i,j) = -9999.9 
     xtts_target_ptr(i,j) = -9999.9 
     xzts_target_ptr(i,j) = -9999.9 
     z_c_target_ptr(i,j) = -9999.9 
     zm_target_ptr(i,j) = -9999.9 
   enddo

 endif

 if (localpet == 0) then
   allocate(latitude_one_tile(i_target,j_target))
 else
   allocate(latitude_one_tile(0,0))
 endif

 do tile = 1, num_tiles_target_grid

! skin temp

   print*,"- CALL FieldGather FOR TARGET GRID SKIN TEMPERATURE TILE: ", tile
   call ESMF_FieldGather(skin_temp_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   print*,"- CALL FieldGather FOR TARGET LANDMASK TILE: ", tile
   call ESMF_FieldGather(landmask_target_grid, mask_target_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   print*,"- CALL FieldGather FOR TARGET LATITUDE TILE: ", tile
   call ESMF_FieldGather(latitude_target_grid, latitude_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     allocate(water_target_one_tile(i_target,j_target))
     water_target_one_tile = 0
     where(mask_target_one_tile == 0) water_target_one_tile = 1
     call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 11, &
                 latitude=latitude_one_tile)
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SKIN TEMP: ", tile
   call ESMF_FieldScatter(skin_temp_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

! z0

   print*,"- CALL FieldGather FOR TARGET GRID Z0 TILE: ", tile
   call ESMF_FieldGather(z0_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 83)
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID Z0: ", tile
   call ESMF_FieldScatter(z0_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   if (convert_nst) then

! c_d

     print*,"- CALL FieldGather FOR TARGET GRID C_D TILE: ", tile
     call ESMF_FieldGather(c_d_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID C_D: ", tile
     call ESMF_FieldScatter(c_d_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! c_0

     print*,"- CALL FieldGather FOR TARGET GRID C_0 TILE: ", tile
     call ESMF_FieldGather(c_0_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID C_0: ", tile
     call ESMF_FieldScatter(c_0_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! d_conv

     print*,"- CALL FieldGather FOR TARGET GRID D_CONV TILE: ", tile
     call ESMF_FieldGather(d_conv_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID D_CONV: ", tile
     call ESMF_FieldScatter(d_conv_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! dt_cool

     print*,"- CALL FieldGather FOR TARGET GRID DT_COOL TILE: ", tile
     call ESMF_FieldGather(dt_cool_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID DT_COOL: ", tile
     call ESMF_FieldScatter(dt_cool_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! ifd

     print*,"- CALL FieldGather FOR TARGET GRID IFD TILE: ", tile
     call ESMF_FieldGather(ifd_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 1)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID IFD: ", tile
     call ESMF_FieldScatter(ifd_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! qrain

     print*,"- CALL FieldGather FOR TARGET GRID QRAIN TILE: ", tile
     call ESMF_FieldGather(qrain_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID QRAIN: ", tile
     call ESMF_FieldScatter(qrain_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! tref

     print*,"- CALL FieldGather FOR TARGET GRID TREF TILE: ", tile
     call ESMF_FieldGather(tref_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 11, &
                   latitude=latitude_one_tile)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID TREF: ", tile
     call ESMF_FieldScatter(tref_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! w_d

     print*,"- CALL FieldGather FOR TARGET GRID W_D TILE: ", tile
     call ESMF_FieldGather(w_d_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID W_D: ", tile
     call ESMF_FieldScatter(w_d_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! w_0

     print*,"- CALL FieldGather FOR TARGET GRID W_0 TILE: ", tile
     call ESMF_FieldGather(w_0_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID W_0: ", tile
     call ESMF_FieldScatter(w_0_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! xs

     print*,"- CALL FieldGather FOR TARGET GRID XS TILE: ", tile
     call ESMF_FieldGather(xs_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID XS: ", tile
     call ESMF_FieldScatter(xs_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! xt

     print*,"- CALL FieldGather FOR TARGET GRID XT TILE: ", tile
     call ESMF_FieldGather(xt_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID XT: ", tile
     call ESMF_FieldScatter(xt_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! xu

     print*,"- CALL FieldGather FOR TARGET GRID XU TILE: ", tile
     call ESMF_FieldGather(xu_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID XU: ", tile
     call ESMF_FieldScatter(xu_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! xv

     print*,"- CALL FieldGather FOR TARGET GRID XV TILE: ", tile
     call ESMF_FieldGather(xv_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID XV: ", tile
     call ESMF_FieldScatter(xv_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! xz

     print*,"- CALL FieldGather FOR TARGET GRID XZ TILE: ", tile
     call ESMF_FieldGather(xz_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 30)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID XZ: ", tile
     call ESMF_FieldScatter(xz_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! xtts

     print*,"- CALL FieldGather FOR TARGET GRID XTTS TILE: ", tile
     call ESMF_FieldGather(xtts_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID XTTS: ", tile
     call ESMF_FieldScatter(xtts_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! xzts

     print*,"- CALL FieldGather FOR TARGET GRID XZTS TILE: ", tile
     call ESMF_FieldGather(xzts_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID XZTS: ", tile
     call ESMF_FieldScatter(xzts_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! z_c

     print*,"- CALL FieldGather FOR TARGET GRID Z_C TILE: ", tile
     call ESMF_FieldGather(z_c_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID Z_C: ", tile
     call ESMF_FieldScatter(z_c_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

! zm 

     print*,"- CALL FieldGather FOR TARGET GRID ZM TILE: ", tile
     call ESMF_FieldGather(zm_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", rc)

     if (localpet == 0) then
       call search(data_one_tile, water_target_one_tile, i_target, j_target, tile, 0)
     endif

     print*,"- CALL FieldScatter FOR TARGET GRID ZM: ", tile
     call ESMF_FieldScatter(zm_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
     if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldScatter", rc)

   endif

   if (localpet == 0) deallocate(water_target_one_tile)

 enddo

 deallocate(latitude_one_tile)

 print*,"- CALL FieldRegridRelease."
 call ESMF_FieldRegridRelease(routehandle=regrid_water, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridRelease", rc)

!---------------------------------------------------------------------------------------------
! Now interpolate "all land" to "all land".  Here, "all land" means landice and non-land ice.
!---------------------------------------------------------------------------------------------

 mask_input_ptr = 0
 where (nint(landmask_input_ptr) == 1) mask_input_ptr = 1

 mask_target_ptr = 0
 where (landmask_target_ptr == 1) mask_target_ptr = 1

 method=ESMF_REGRIDMETHOD_CONSERVE
 isrctermprocessing = 1

 print*,"- CALL FieldRegridStore for land fields."
 call ESMF_FieldRegridStore(snow_depth_input_grid, &
                            snow_depth_target_grid, &
                            srcmaskvalues=(/0/), &
                            dstmaskvalues=(/0/), &
                            polemethod=ESMF_POLEMETHOD_NONE, &
                            srctermprocessing=isrctermprocessing, &
                            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
                            normtype=ESMF_NORMTYPE_FRACAREA, &
                            routehandle=regrid_all_land, &
                            regridmethod=method, &
                            unmappedDstList=unmapped_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridStore", rc)

 print*,"- CALL Field_Regrid for snow depth over land."
 call ESMF_FieldRegrid(snow_depth_input_grid, &
                       snow_depth_target_grid, &
                       routehandle=regrid_all_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, &
                       zeroregion=ESMF_REGION_SELECT, &  ! flag needed so snow over sea 
                                                         ! ice is not zeroed out.
                       rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for snow liq equiv over land."
 call ESMF_FieldRegrid(snow_liq_equiv_input_grid, &
                       snow_liq_equiv_target_grid, &
                       routehandle=regrid_all_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, &
                       zeroregion=ESMF_REGION_SELECT, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for canopy mc."
 call ESMF_FieldRegrid(canopy_mc_input_grid, &
                       canopy_mc_target_grid, &
                       routehandle=regrid_all_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL FieldGet FOR TARGET snow depth."
 call ESMF_FieldGet(snow_depth_target_grid, &
                   farrayPtr=snow_depth_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET snow liq equiv."
 call ESMF_FieldGet(snow_liq_equiv_target_grid, &
                    farrayPtr=snow_liq_equiv_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET canopy moisture."
 call ESMF_FieldGet(canopy_mc_target_grid, &
                  farrayPtr=canopy_mc_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 l = lbound(unmapped_ptr)
 u = ubound(unmapped_ptr)

 do ij = l(1), u(1)
   call ij_to_i_j(unmapped_ptr(ij), i_target, j_target, i, j)
   snow_depth_target_ptr(i,j) = -9999.9 
   snow_liq_equiv_target_ptr(i,j) = -9999.9 
   canopy_mc_target_ptr(i,j) = -9999.9 
 enddo

 do tile = 1, num_tiles_target_grid

   print*,"- CALL FieldGather FOR TARGET GRID SNOW DEPTH TILE: ", tile
   call ESMF_FieldGather(snow_depth_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   print*,"- CALL FieldGather FOR TARGET LANDMASK TILE: ", tile
   call ESMF_FieldGather(landmask_target_grid, mask_target_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     allocate(land_target_one_tile(i_target,j_target))
     land_target_one_tile = 0
     where(mask_target_one_tile == 1) land_target_one_tile = 1
     call search(data_one_tile, land_target_one_tile, i_target, j_target, tile, 66)
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SNOW DEPTH: ", tile
   call ESMF_FieldScatter(snow_depth_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldGather FOR TARGET GRID SNOW LIQUID EQUIV: ", tile
   call ESMF_FieldGather(snow_liq_equiv_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     call search(data_one_tile, land_target_one_tile, i_target, j_target, tile, 65)
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SNOW LIQUID EQUIV: ", tile
   call ESMF_FieldScatter(snow_liq_equiv_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldGather FOR TARGET GRID CANOPY MC: ", tile
   call ESMF_FieldGather(canopy_mc_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     call search(data_one_tile, land_target_one_tile, i_target, j_target, tile, 223)
     deallocate(land_target_one_tile)
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID CANOPY MC: ", tile
   call ESMF_FieldScatter(canopy_mc_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

 enddo

 print*,"- CALL FieldRegridRelease."
 call ESMF_FieldRegridRelease(routehandle=regrid_all_land, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridRelease", rc)

!---------------------------------------------------------------------------------------------
! Now interpolate landice points to landice points.
!---------------------------------------------------------------------------------------------

 print*,"- CALL FieldGet FOR INPUT GRID VEG TYPE."
 call ESMF_FieldGet(veg_type_input_grid, &
                    farrayPtr=veg_type_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,'land ice check ',veg_type_landice_input

 mask_input_ptr = 0
 where (nint(veg_type_input_ptr) == veg_type_landice_input) mask_input_ptr = 1

 print*,"- CALL FieldGet FOR TARGET GRID VEG TYPE."
 call ESMF_FieldGet(veg_type_target_grid, &
                    farrayPtr=veg_type_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 mask_target_ptr = 0
 where (nint(veg_type_target_ptr) == veg_type_landice_target) mask_target_ptr = 1

 method=ESMF_REGRIDMETHOD_NEAREST_STOD
 isrctermprocessing = 1

 print*,"- CALL FieldRegridStore for landice fields."
 call ESMF_FieldRegridStore(soil_temp_input_grid, &
                            soil_temp_target_grid, &
                            srcmaskvalues=(/0/), &
                            dstmaskvalues=(/0/), &
                            polemethod=ESMF_POLEMETHOD_NONE, &
                            srctermprocessing=isrctermprocessing, &
                            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
                            normtype=ESMF_NORMTYPE_FRACAREA, &
                            routehandle=regrid_landice, &
                            regridmethod=method, &
                            unmappedDstList=unmapped_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridStore", rc)

 print*,"- CALL Field_Regrid for soil temperature over landice."
 call ESMF_FieldRegrid(soil_temp_input_grid, &
                       soil_temp_target_grid, &
                       routehandle=regrid_landice, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, &
                       zeroregion=ESMF_REGION_SELECT, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for skin temperature over landice."
 call ESMF_FieldRegrid(skin_temp_input_grid, &
                       skin_temp_target_grid, &
                       routehandle=regrid_landice, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, &
                       zeroregion=ESMF_REGION_SELECT, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for terrain over landice."
 call ESMF_FieldRegrid(terrain_input_grid, &
                       terrain_from_input_grid, &
                       routehandle=regrid_landice, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, &
                       zeroregion=ESMF_REGION_SELECT, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL FieldGet FOR TARGET grid column temperature over landice."
 call ESMF_FieldGet(soil_temp_target_grid, &
                    farrayPtr=soil_temp_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET skin temperature."
 call ESMF_FieldGet(skin_temp_target_grid, &
                    farrayPtr=skin_temp_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR terrain from input grid."
 call ESMF_FieldGet(terrain_from_input_grid, &
                    farrayPtr=terrain_from_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 l = lbound(unmapped_ptr)
 u = ubound(unmapped_ptr)

 do ij = l(1), u(1)
   call ij_to_i_j(unmapped_ptr(ij), i_target, j_target, i, j)
   soil_temp_target_ptr(i,j,:) = -9999.9 
   skin_temp_target_ptr(i,j) = -9999.9 
   terrain_from_input_ptr(i,j) = -9999.9 
 enddo

 if (localpet == 0) then
   allocate (veg_type_target_one_tile(i_target,j_target))
   allocate (land_target_one_tile(i_target,j_target))
 else
   allocate (veg_type_target_one_tile(0,0))
   allocate (land_target_one_tile(0,0))
 endif

 do tile = 1, num_tiles_target_grid

   print*,"- CALL FieldGather FOR TARGET GRID SKIN TEMP TILE: ", tile
   call ESMF_FieldGather(skin_temp_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   print*,"- CALL FieldGather FOR TARGET VEG TYPE TILE: ", tile
   call ESMF_FieldGather(veg_type_target_grid, veg_type_target_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     land_target_one_tile = 0
     where(nint(veg_type_target_one_tile) == veg_type_landice_target) land_target_one_tile = 1
     call search(data_one_tile, land_target_one_tile, i_target, j_target, tile, 21)
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SKIN TEMP, TILE: ", tile
   call ESMF_FieldScatter(skin_temp_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldGather FOR TERRAIN FROM INPUT GRID, TILE: ", tile
   call ESMF_FieldGather(terrain_from_input_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     call search(data_one_tile, land_target_one_tile, i_target, j_target, tile, 7)
   endif

   print*,"- CALL FieldScatter FOR TERRAIN FROM INPUT GRID, TILE: ", tile
   call ESMF_FieldScatter(terrain_from_input_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldGather FOR TARGET GRID LANDICE COLUMN TEMP: ", tile
   call ESMF_FieldGather(soil_temp_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     do j = 1, lsoil_target
       data_one_tile = data_one_tile_3d(:,:,j)
       call search(data_one_tile, land_target_one_tile, i_target, j_target, tile, 21)
       data_one_tile_3d(:,:,j) = data_one_tile
     enddo
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SEAICE COLUMN TEMP: ", tile
   call ESMF_FieldScatter(soil_temp_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

 enddo

 deallocate (veg_type_target_one_tile)
 deallocate (land_target_one_tile)

 print*,"- CALL FieldRegridRelease."
 call ESMF_FieldRegridRelease(routehandle=regrid_landice, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridRelease", rc)

!---------------------------------------------------------------------------------------------
! Now interpolate land (not including landice pts) to land (not including landice).
!---------------------------------------------------------------------------------------------

 mask_input_ptr = 0
 where (nint(landmask_input_ptr) == 1) mask_input_ptr = 1
 where (nint(veg_type_input_ptr) == veg_type_landice_input) mask_input_ptr = 0

 mask_target_ptr = 0
 where (landmask_target_ptr == 1) mask_target_ptr = 1
 where (nint(veg_type_target_ptr) == veg_type_landice_target) mask_target_ptr = 0

 method=ESMF_REGRIDMETHOD_NEAREST_STOD
 isrctermprocessing = 1

 print*,"- CALL FieldRegridStore for 3d land (but no land ice) fields."
 call ESMF_FieldRegridStore(soilm_tot_input_grid, &
                            soilm_tot_target_grid, &
                            srcmaskvalues=(/0/), &
                            dstmaskvalues=(/0/), &
                            polemethod=ESMF_POLEMETHOD_NONE, &
                            srctermprocessing=isrctermprocessing, &
                            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
                            normtype=ESMF_NORMTYPE_FRACAREA, &
                            routehandle=regrid_land, &
                            regridmethod=method, &
                            unmappedDstList=unmapped_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridStore", rc)

 print*,"- CALL Field_Regrid for total soil moisture over land."
 call ESMF_FieldRegrid(soilm_tot_input_grid, &
                       soilm_tot_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for soil temperature over land."
 call ESMF_FieldRegrid(soil_temp_input_grid, &
                       soil_temp_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, &
                       zeroregion=ESMF_REGION_SELECT, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for skin temperature over land."
 call ESMF_FieldRegrid(skin_temp_input_grid, &
                       skin_temp_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, &
                       zeroregion=ESMF_REGION_SELECT, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for terrain over land."
 call ESMF_FieldRegrid(terrain_input_grid, &
                       terrain_from_input_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, &
                       zeroregion=ESMF_REGION_SELECT, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for soil type over land."
 call ESMF_FieldRegrid(soil_type_input_grid, &
                       soil_type_from_input_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL FieldGet FOR TARGET grid total soil moisture over land."
 call ESMF_FieldGet(soilm_tot_target_grid, &
                    farrayPtr=soilm_tot_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET grid soil temp over ice."
 call ESMF_FieldGet(soil_temp_target_grid, &
                    farrayPtr=soil_temp_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET skin temperature."
 call ESMF_FieldGet(skin_temp_target_grid, &
                    farrayPtr=skin_temp_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR terrain from input grid."
 call ESMF_FieldGet(terrain_from_input_grid, &
                    farrayPtr=terrain_from_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR soil type from input grid."
 call ESMF_FieldGet(soil_type_from_input_grid, &
                    farrayPtr=soil_type_from_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 l = lbound(unmapped_ptr)
 u = ubound(unmapped_ptr)

 do ij = l(1), u(1)
   call ij_to_i_j(unmapped_ptr(ij), i_target, j_target, i, j)
   soilm_tot_target_ptr(i,j,:) = -9999.9 
   soil_temp_target_ptr(i,j,:) = -9999.9 
   skin_temp_target_ptr(i,j) = -9999.9 
   terrain_from_input_ptr(i,j) = -9999.9 
   soil_type_from_input_ptr(i,j) = -9999.9 
 enddo

 if (localpet == 0) then
   allocate (veg_type_target_one_tile(i_target,j_target))
 else
   allocate (veg_type_target_one_tile(0,0))
 endif

 do tile = 1, num_tiles_target_grid

   print*,"- CALL FieldGather FOR TARGET LANDMASK TILE: ", tile
   call ESMF_FieldGather(landmask_target_grid, mask_target_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   print*,"- CALL FieldGather FOR TARGET VEG TYPE TILE: ", tile
   call ESMF_FieldGather(veg_type_target_grid, veg_type_target_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   print*,"- CALL FieldGather FOR TERRAIN FROM INPUT GRID, TILE: ", tile
   call ESMF_FieldGather(terrain_from_input_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     where(nint(veg_type_target_one_tile) == veg_type_landice_target) mask_target_one_tile = 0
     call search(data_one_tile, mask_target_one_tile, i_target, j_target, tile, 7)
   endif

   print*,"- CALL FieldScatter FOR TERRAIN FROM INPUT GRID, TILE: ", tile
   call ESMF_FieldScatter(terrain_from_input_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldGather FOR TARGET GRID SKIN TEMPERATURE, TILE: ", tile
   call ESMF_FieldGather(skin_temp_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     call search(data_one_tile, mask_target_one_tile, i_target, j_target, tile, 85)
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SKIN TEMPERATURE, TILE: ", tile
   call ESMF_FieldScatter(skin_temp_target_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldGather FOR SOIL TYPE FROM INPUT GRID, TILE: ", tile
   call ESMF_FieldGather(soil_type_from_input_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     call search(data_one_tile, mask_target_one_tile, i_target, j_target, tile, 224)
   endif

   print*,"- CALL FieldScatter FOR SOIL TYPE FROM INPUT GRID, TILE: ", tile
   call ESMF_FieldScatter(soil_type_from_input_grid, data_one_tile, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldGather FOR TARGET GRID TOTAL SOIL MOISTURE, TILE: ", tile
   call ESMF_FieldGather(soilm_tot_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     do j = 1, lsoil_target
       data_one_tile = data_one_tile_3d(:,:,j)
       call search(data_one_tile, mask_target_one_tile, i_target, j_target, tile, 86)
       data_one_tile_3d(:,:,j) = data_one_tile
     enddo
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID TOTAL SOIL MOISTURE, TILE: ", tile
   call ESMF_FieldScatter(soilm_tot_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldGather FOR TARGET GRID SOIL TEMPERATURE, TILE: ", tile
   call ESMF_FieldGather(soil_temp_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", rc)

   if (localpet == 0) then
     do j = 1, lsoil_target
       data_one_tile = data_one_tile_3d(:,:,j)
       call search(data_one_tile, mask_target_one_tile, i_target, j_target, tile, 85)
       data_one_tile_3d(:,:,j) = data_one_tile
     enddo
   endif

   print*,"- CALL FieldScatter FOR TARGET GRID SOIL TEMPERATURE, TILE: ", tile
   call ESMF_FieldScatter(soil_temp_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", rc)

 enddo

 print*,"- CALL FieldRegridRelease."
 call ESMF_FieldRegridRelease(routehandle=regrid_land, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridRelease", rc)

 deallocate(veg_type_target_one_tile)

 deallocate(data_one_tile)
 deallocate(data_one_tile_3d)
 deallocate(mask_target_one_tile)

 return

 end subroutine interp
 
!---------------------------------------------------------------------------------------------
! Compute liquid portion of the total soil moisture.
!---------------------------------------------------------------------------------------------

 subroutine calc_liq_soil_moisture

 use esmf

 use model_grid, only                : landmask_target_grid

 use program_setup, only             : maxsmc_target, &
                                       bb_target, &
                                       satpsi_target

 use static_data, only               : soil_type_target_grid, &
                                       veg_type_target_grid

 implicit none
 
 integer                            :: clb(3), cub(3), rc
 integer                            :: i, j, n, soil_type

 integer(esmf_kind_i8), pointer     :: landmask_ptr(:,:)

 real                               :: bx, fk
 real(esmf_kind_r8), pointer        :: soilm_liq_ptr(:,:,:)
 real(esmf_kind_r8), pointer        :: soilm_tot_ptr(:,:,:)
 real(esmf_kind_r8), pointer        :: soil_temp_ptr(:,:,:)
 real(esmf_kind_r8), pointer        :: soil_type_ptr(:,:)
 real(esmf_kind_r8), pointer        :: veg_type_ptr(:,:)

 print*,"- COMPUTE LIQUID PORTION OF TOTAL SOIL MOISTURE."

 print*,"- CALL FieldGet FOR TOTAL SOIL MOISTURE."
 call ESMF_FieldGet(soilm_tot_target_grid, &
                    computationalLBound=clb, &
                    computationalUBound=cub, &
                    farrayPtr=soilm_tot_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR LIQUID SOIL MOISTURE."
 call ESMF_FieldGet(soilm_liq_target_grid, &
                    farrayPtr=soilm_liq_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR SOIL TEMPERATURE."
 call ESMF_FieldGet(soil_temp_target_grid, &
                    farrayPtr=soil_temp_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR VEGETATION TYPE."
 call ESMF_FieldGet(veg_type_target_grid, &
                    farrayPtr=veg_type_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR SOIL TYPE."
 call ESMF_FieldGet(soil_type_target_grid, &
                    farrayPtr=soil_type_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR LANDMASK."
 call ESMF_FieldGet(landmask_target_grid, &
                    farrayPtr=landmask_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
   do i = clb(1), cub(1)

!---------------------------------------------------------------------------------------------
! Check land points that are not permanent land ice.  
!---------------------------------------------------------------------------------------------

     if (landmask_ptr(i,j) == 1 .and. nint(veg_type_ptr(i,j)) /= veg_type_landice_target) then

       soil_type = nint(soil_type_ptr(i,j))

       do n = clb(3), cub(3) 

         if (soil_temp_ptr(i,j,n) < (frz_h2o-0.0001)) then

           bx = bb_target(soil_type)

           if (bx .gt. blim) bx = blim

           fk=(((hlice/(grav*(-satpsi_target(soil_type))))*           &
            ((soil_temp_ptr(i,j,n)-frz_h2o)/soil_temp_ptr(i,j,n)))**             &
            (-1/bx))*maxsmc_target(soil_type)

           if (fk .lt. 0.02) fk = 0.02

           soilm_liq_ptr(i,j,n) = min ( fk, soilm_tot_ptr(i,j,n) )

!-----------------------------------------------------------------------
! now use iterative solution for liquid soil water content using
! FUNCTION FRH2O with the initial guess for SH2O from above explicit
! first guess.
!-----------------------------------------------------------------------

           soilm_liq_ptr(i,j,n) = frh2O(soil_temp_ptr(i,j,n),                        &
                           soilm_tot_ptr(i,j,n), soilm_liq_ptr(i,j,n),             &
                           maxsmc_target(soil_type),bb_target(soil_type),    &
                           satpsi_target(soil_type))

         else  ! temp above freezing. all moisture is liquid

           soilm_liq_ptr(i,j,n) = soilm_tot_ptr(i,j,n)

         end if  ! is soil layer below freezing?

       enddo ! soil layer

     end if ! is this point land?

   enddo
 enddo

 end subroutine calc_liq_soil_moisture

 FUNCTION FRH2O (TKELV,SMC,SH2O,SMCMAX,BEXP,PSIS)
!$$$  function documentation block
!
! function:   frh2o
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  calculate supercooled soil moisture
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: x = frh2o (tkelv,smc,sh2o,smcmax,bexp,psis)
!
!   input argument list: 
!     tkelv        - temperature (Kelvin)
!     smc          - total soil moisture content (volumetric)
!     sh2O         - liquid soil moisture content (volumetric)
!     smcmax       - saturation soil moisture content
!     b            - soil type "b" parameter
!     psis         - saturated soil matric potential
!
!   output argument list: 
!     frh2O        - supercooled liquid water content
!
! remarks: stolen from noah lsm code
!
!   CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT IF
!   TEMPERATURE IS BELOW 273.15K (T0).  REQUIRES NEWTON-TYPE ITERATION TO
!   SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF KOREN ET AL
!   (1999, JGR, VOL 104(D16), 19569-19585).
! 
!   NEW VERSION (JUNE 2001): MUCH FASTER AND MORE ACCURATE NEWTON
!   ITERATION ACHIEVED BY FIRST TAKING LOG OF EQN CITED ABOVE -- LESS THAN
!   4 (TYPICALLY 1 OR 2) ITERATIONS ACHIEVES CONVERGENCE.  ALSO, EXPLICIT
!   1-STEP SOLUTION OPTION FOR SPECIAL CASE OF PARAMETER CK=0, WHICH
!   REDUCES THE ORIGINAL IMPLICIT EQUATION TO A SIMPLER EXPLICIT FORM,
!   KNOWN AS THE "FLERCHINGER EQN". IMPROVED HANDLING OF SOLUTION IN THE
!   LIMIT OF FREEZING POINT TEMPERATURE [AT0.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 use esmf

 IMPLICIT NONE

 INTEGER NLOG
 INTEGER KCOUNT

 REAL BEXP
 REAL BX
 REAL DENOM
 REAL DF
 REAL DSWL
 REAL FK
 REAL FRH2O
 REAL PSIS
 REAL(esmf_kind_r8) :: SH2O
 REAL(esmf_kind_r8) :: SMC
 REAL SMCMAX
 REAL SWL
 REAL SWLK
 REAL(esmf_kind_r8) :: TKELV

 REAL, PARAMETER                  :: CK    = 8.0
 REAL, PARAMETER                  :: ERROR = 0.005

! ----------------------------------------------------------------------
! LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)
! SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT IS
! NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES.
! ----------------------------------------------------------------------

 BX = BEXP
 IF (BEXP .GT. BLIM) BX = BLIM

! ----------------------------------------------------------------------
! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
! ----------------------------------------------------------------------

 NLOG=0
 KCOUNT=0

 IF (CK .NE. 0.0) THEN

! ----------------------------------------------------------------------
! OPTION 1: ITERATED SOLUTION FOR NONZERO CK
! IN KOREN ET AL, JGR, 1999, EQN 17
! ----------------------------------------------------------------------
! INITIAL GUESS FOR SWL (frozen content)
! ----------------------------------------------------------------------

   SWL = SMC-SH2O

! ----------------------------------------------------------------------
! KEEP WITHIN BOUNDS.
! ----------------------------------------------------------------------

   IF (SWL .GT. (SMC-0.02)) SWL = SMC-0.02
   IF (SWL .LT. 0.) SWL = 0.

! ----------------------------------------------------------------------
!  START OF ITERATIONS
! ----------------------------------------------------------------------

   DO WHILE ( (NLOG .LT. 10) .AND. (KCOUNT .EQ. 0) )

     NLOG = NLOG+1
     DF = ALOG(( PSIS*GRAV/HLICE ) * ( ( 1.+CK*SWL )**2. ) *      &
        ( SMCMAX/(SMC-SWL) )**BX) - ALOG(-(TKELV-frz_h2o)/TKELV)
     DENOM = 2. * CK / ( 1.+CK*SWL ) + BX / ( SMC - SWL )
     SWLK = SWL - DF/DENOM

! ----------------------------------------------------------------------
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
! ----------------------------------------------------------------------

     IF (SWLK .GT. (SMC-0.02)) SWLK = SMC - 0.02
     IF (SWLK .LT. 0.) SWLK = 0.

! ----------------------------------------------------------------------
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
! ----------------------------------------------------------------------

     DSWL = ABS(SWLK-SWL)
     SWL = SWLK

! ----------------------------------------------------------------------
! IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
! WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
! ----------------------------------------------------------------------

     IF ( DSWL .LE. ERROR )  THEN
       KCOUNT = KCOUNT+1
     ENDIF

   END DO

! ----------------------------------------------------------------------
!  END OF ITERATIONS
! ----------------------------------------------------------------------
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
! ----------------------------------------------------------------------

   FRH2O = SMC - SWL

! ----------------------------------------------------------------------
! END OPTION 1
! ----------------------------------------------------------------------

 ENDIF

!-----------------------------------------------------------------------
! OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0
! IN KOREN ET AL., JGR, 1999, EQN 17
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
! ----------------------------------------------------------------------

 IF (KCOUNT .EQ. 0) THEN

   FK = (((HLICE/(GRAV*(-PSIS)))*                  &
        ((TKELV-frz_h2o)/TKELV))**(-1/BX))*SMCMAX

   IF (FK .LT. 0.02) FK = 0.02

   FRH2O = MIN (FK, SMC)

 ENDIF

 RETURN

 END function frh2o

!---------------------------------------------------------------------------------------------
! Adjust soil moisture for changes in soil type between the input and target grids.
!---------------------------------------------------------------------------------------------

 subroutine rescale_soil_moisture

 use esmf

 use model_grid, only                : landmask_target_grid

 use program_setup, only             : drysmc_input, drysmc_target, &
                                       maxsmc_input, maxsmc_target, &
                                       refsmc_input, refsmc_target, &
                                       wltsmc_input, wltsmc_target

 use static_data, only               : soil_type_target_grid, &
                                       veg_greenness_target_grid, &
                                       veg_type_target_grid

 implicit none

 integer                            :: clb(3), cub(3), i, j, k, rc
 integer                            :: soilt_input, soilt_target
 integer(esmf_kind_i8), pointer     :: landmask_ptr(:,:)

 real(esmf_kind_r8), pointer        :: soilm_tot_ptr(:,:,:)
 real(esmf_kind_r8), pointer        :: soil_type_input_ptr(:,:)
 real(esmf_kind_r8), pointer        :: soil_type_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: veg_greenness_ptr(:,:)
 real(esmf_kind_r8), pointer        :: veg_type_ptr(:,:)
 real                               :: f1, fn, smcdir, smctra

 print*,"- RESCALE SOIL MOISTURE FOR CHANGES IN SOIL TYPE."

 print*,"- CALL FieldGet FOR TOTAL SOIL MOISTURE."
 call ESMF_FieldGet(soilm_tot_target_grid, &
                    computationalLBound=clb, &
                    computationalUBound=cub, &
                    farrayPtr=soilm_tot_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR LAND MASK."
 call ESMF_FieldGet(landmask_target_grid, &
                    farrayPtr=landmask_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR VEGETATION TYPE."
 call ESMF_FieldGet(veg_type_target_grid, &
                    farrayPtr=veg_type_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR VEGETATION GREENNESS."
 call ESMF_FieldGet(veg_greenness_target_grid, &
                    farrayPtr=veg_greenness_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET GRID SOIL TYPE."
 call ESMF_FieldGet(soil_type_target_grid, &
                    farrayPtr=soil_type_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR SOIL TYPE FROM INPUT GRID."
 call ESMF_FieldGet(soil_type_from_input_grid, &
                    farrayPtr=soil_type_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
   do i = clb(1), cub(1)

!---------------------------------------------------------------------------------------------
! Check land points that are not permanent land ice.  
!---------------------------------------------------------------------------------------------

     if (landmask_ptr(i,j) == 1 .and. nint(veg_type_ptr(i,j)) /= veg_type_landice_target) then

        soilt_target = nint(soil_type_target_ptr(i,j))
        soilt_input  = nint(soil_type_input_ptr(i,j))

!---------------------------------------------------------------------------------------------
! Rescale soil moisture at points where the soil type between the input and output
! grids is different.  Caution, this logic assumes the input and target grids use the same
! soil type dataset.
!---------------------------------------------------------------------------------------------

        if (soilt_target /= soilt_input) then

!---------------------------------------------------------------------------------------------
! Rescale top layer.  First, determine direct evaporation part:
!---------------------------------------------------------------------------------------------

          f1=(soilm_tot_ptr(i,j,1)-drysmc_input(soilt_input)) /    &
             (maxsmc_input(soilt_input)-drysmc_input(soilt_input))

          smcdir=drysmc_target(soilt_target) + f1 *        &
                (maxsmc_target(soilt_target) - drysmc_target(soilt_target))

!---------------------------------------------------------------------------------------------
! Continue top layer rescale.  Now determine transpiration part:
!---------------------------------------------------------------------------------------------

          if (soilm_tot_ptr(i,j,1) < refsmc_input(soilt_input)) then
            f1=(soilm_tot_ptr(i,j,1) - wltsmc_input(soilt_input)) /       &
               (refsmc_input(soilt_input) - wltsmc_input(soilt_input))
            smctra=wltsmc_target(soilt_target) + f1  *     &
                  (refsmc_target(soilt_target) - wltsmc_target(soilt_target))
          else
            f1=(soilm_tot_ptr(i,j,1) - refsmc_input(soilt_input)) /        &
               (maxsmc_input(soilt_input) - refsmc_input(soilt_input))
            smctra=refsmc_target(soilt_target) + f1 *      &
                  (maxsmc_target(soilt_target) - refsmc_target(soilt_target))
          endif

!---------------------------------------------------------------------------------------------
! Top layer is weighted by green vegetation fraction:
!---------------------------------------------------------------------------------------------

          soilm_tot_ptr(i,j,1) = ((1.0 - veg_greenness_ptr(i,j)) * smcdir)  + &
                                  (veg_greenness_ptr(i,j) * smctra)

!---------------------------------------------------------------------------------------------
! Rescale bottom layers as follows:
!
! - Rescale between wilting point and reference value when wilting < soil m < reference, or
! - Rescale between reference point and maximum value when reference < soil m < max.
!---------------------------------------------------------------------------------------------

          do k = 2, cub(3)
            if (soilm_tot_ptr(i,j,k) < refsmc_input(soilt_input)) then
              fn = (soilm_tot_ptr(i,j,k) - wltsmc_input(soilt_input)) /        &
                (refsmc_input(soilt_input) - wltsmc_input(soilt_input))
              soilm_tot_ptr(i,j,k) = wltsmc_target(soilt_target) + fn *         &
                (refsmc_target(soilt_target) - wltsmc_target(soilt_target))
            else
              fn = (soilm_tot_ptr(i,j,k) - refsmc_input(soilt_input)) /         &
                (maxsmc_input(soilt_input) - refsmc_input(soilt_input))
              soilm_tot_ptr(i,j,k) = refsmc_target(soilt_target) + fn *         &
                (maxsmc_target(soilt_target) - refsmc_target(soilt_target))
            endif
          enddo

        endif ! is soil type different?

!---------------------------------------------------------------------------------------------
! Range check all layers.
!---------------------------------------------------------------------------------------------

        soilm_tot_ptr(i,j,1)=min(soilm_tot_ptr(i,j,1),maxsmc_target(soilt_target))
        soilm_tot_ptr(i,j,1)=max(drysmc_target(soilt_target),soilm_tot_ptr(i,j,1))

        do k = 2, cub(3)
          soilm_tot_ptr(i,j,k)=min(soilm_tot_ptr(i,j,k),maxsmc_target(soilt_target))
          soilm_tot_ptr(i,j,k)=max(wltsmc_target(soilt_target),soilm_tot_ptr(i,j,k))
        enddo

     endif ! is this a land point?

   enddo
 enddo

 return

 end subroutine rescale_soil_moisture

!---------------------------------------------------------------------------------------------
! Adjust soil temperature for changes in terrain height between the input and
! target grids.
!---------------------------------------------------------------------------------------------

 subroutine adjust_soilt_for_terrain

 use model_grid, only                : landmask_target_grid,  &
                                       terrain_target_grid

 use static_data, only               : veg_type_target_grid

 implicit none

 integer                            :: clb(3), cub(3), i, j, k, rc
 integer(esmf_kind_i8), pointer     :: landmask_ptr(:,:)

 real, parameter                    :: lapse_rate  = 6.5e-03
 real                               :: terrain_diff
 real(esmf_kind_r8), pointer        :: terrain_input_ptr(:,:)
 real(esmf_kind_r8), pointer        :: terrain_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: veg_type_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: soil_temp_target_ptr(:,:,:)

 print*,"- CALL FieldGet FOR TARGET GRID LAND-SEA MASK."
 call ESMF_FieldGet(landmask_target_grid, &
                    farrayPtr=landmask_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET GRID VEGETATION TYPE."
 call ESMF_FieldGet(veg_type_target_grid, &
                    farrayPtr=veg_type_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET GRID TERRAIN."
 call ESMF_FieldGet(terrain_target_grid, &
                    farrayPtr=terrain_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TERRAIN INTERP TO TARGET GRID."
 call ESMF_FieldGet(terrain_from_input_grid, &
                    farrayPtr=terrain_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR SOIL TEMP TARGET GRID."
 call ESMF_FieldGet(soil_temp_target_grid, &
                    computationalLBound=clb, &
                    computationalUBound=cub, &
                    farrayPtr=soil_temp_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)
 
 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) == 1) then
     terrain_diff = abs(terrain_input_ptr(i,j) - terrain_target_ptr(i,j))
     if (terrain_diff > 100.0) then
       do k = clb(3), cub(3)
         soil_temp_target_ptr(i,j,k) = soil_temp_target_ptr(i,j,k) + &
              ((terrain_input_ptr(i,j) - terrain_target_ptr(i,j)) * lapse_rate)
         if (nint(veg_type_target_ptr(i,j)) == veg_type_landice_target) then
           soil_temp_target_ptr(i,j,k) = min(soil_temp_target_ptr(i,j,k), 273.16)
         endif
       enddo
     endif
   endif
 enddo
 enddo

 end subroutine adjust_soilt_for_terrain

!---------------------------------------------------------------------------------------------
! Set roughness at land and sea ice.
!---------------------------------------------------------------------------------------------
 
 subroutine roughness

 use model_grid, only                : landmask_target_grid
 use static_data, only               : veg_type_target_grid

 implicit none

 integer                            :: clb(2), cub(2), i, j, rc
 integer(esmf_kind_i8), pointer     :: landmask_ptr(:,:)

 real                               :: z0_igbp(20)
 real(esmf_kind_r8), pointer        :: data_ptr(:,:)
 real(esmf_kind_r8), pointer        :: veg_type_ptr(:,:)

 data z0_igbp /1.089, 2.653, 0.854, 0.826, 0.800, 0.050,  &
               0.030, 0.856, 0.856, 0.150, 0.040, 0.130,  &
               1.000, 0.250, 0.011, 0.011, 0.001, 0.076,  &
               0.050, 0.030/

 print*,"- CALL FieldGet FOR TARGET GRID LAND-SEA MASK."
 call ESMF_FieldGet(landmask_target_grid, &
                    computationalLBound=clb, &
                    computationalUBound=cub, &
                    farrayPtr=landmask_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET GRID VEGETATION TYPE."
 call ESMF_FieldGet(veg_type_target_grid, &
                    farrayPtr=veg_type_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET GRID Z0."
 call ESMF_FieldGet(z0_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) == 2) then
     data_ptr(i,j) = 1.0
   elseif (landmask_ptr(i,j) == 1) then
     data_ptr(i,j) = z0_igbp(nint(veg_type_ptr(i,j))) * 100.0
   endif
 enddo
 enddo

 end subroutine roughness

!---------------------------------------------------------------------------------------------
! QC data before output.
!---------------------------------------------------------------------------------------------

 subroutine qc_check

 use model_grid, only                : landmask_target_grid

 use static_data, only               : alvsf_target_grid, &
                                       alvwf_target_grid, &
                                       alnsf_target_grid, &
                                       alnwf_target_grid, &
                                       facsf_target_grid, &
                                       facwf_target_grid, &
                                       mxsno_albedo_target_grid, &
                                       max_veg_greenness_target_grid, &
                                       min_veg_greenness_target_grid, &
                                       slope_type_target_grid, &
                                       soil_type_target_grid, &
                                       substrate_temp_target_grid, &
                                       veg_greenness_target_grid, &
                                       veg_type_target_grid

 implicit none

 integer                            :: clb(2), cub(2), i, j, rc
 integer(esmf_kind_i8), pointer     :: landmask_ptr(:,:)

 real(esmf_kind_r8), pointer        :: data_ptr(:,:)
 real(esmf_kind_r8), pointer        :: data3d_ptr(:,:,:)
 real(esmf_kind_r8), pointer        :: soilmt_ptr(:,:,:)
 real(esmf_kind_r8), pointer        :: soilml_ptr(:,:,:)
 real(esmf_kind_r8), pointer        :: veg_greenness_ptr(:,:)
 real(esmf_kind_r8), pointer        :: veg_type_ptr(:,:)
 real(esmf_kind_r8), pointer        :: seaice_skint_ptr(:,:)
 real(esmf_kind_r8), pointer        :: skint_ptr(:,:)
 real(esmf_kind_r8), pointer        :: fice_ptr(:,:)

 print*,"- CALL FieldGet FOR TARGET GRID LAND-SEA MASK."
 call ESMF_FieldGet(landmask_target_grid, &
                    computationalLBound=clb, &
                    computationalUBound=cub, &
                    farrayPtr=landmask_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

!---------------------------------------------------------------------------------------------
! Set slope type flag value at non-land points.
!---------------------------------------------------------------------------------------------

 print*,"- CALL FieldGet FOR TARGET GRID SLOPE TYPE."
 call ESMF_FieldGet(slope_type_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) data_ptr(i,j) = 0.0
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID SOIL TYPE."
 call ESMF_FieldGet(soil_type_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) data_ptr(i,j) = 0.0
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID VEGETATION TYPE."
 call ESMF_FieldGet(veg_type_target_grid, &
                    farrayPtr=veg_type_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) veg_type_ptr(i,j) = 0.0
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID ALVSF."
 call ESMF_FieldGet(alvsf_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) data_ptr(i,j) = 0.06 ! gfs physics flag value
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID ALVWF."
 call ESMF_FieldGet(alvwf_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) data_ptr(i,j) = 0.06 ! gfs physics flag value
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID ALNSF."
 call ESMF_FieldGet(alnsf_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) data_ptr(i,j) = 0.06 ! gfs physics flag value
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID ALNWF."
 call ESMF_FieldGet(alnwf_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) data_ptr(i,j) = 0.06 ! gfs physics flag value
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID FACSF."
 call ESMF_FieldGet(facsf_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) data_ptr(i,j) = 0.0
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID FAWSF."
 call ESMF_FieldGet(facwf_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) data_ptr(i,j) = 0.0
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID MAXIMUM GREENNESS."
 call ESMF_FieldGet(max_veg_greenness_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) data_ptr(i,j) = 0.0
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID MINIMUM GREENNESS."
 call ESMF_FieldGet(min_veg_greenness_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) data_ptr(i,j) = 0.0
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID VEGETATION GREENNESS."
 call ESMF_FieldGet(veg_greenness_target_grid, &
                    farrayPtr=veg_greenness_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) veg_greenness_ptr(i,j) = 0.0
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID MAX SNOW ALBEDO."
 call ESMF_FieldGet(mxsno_albedo_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) /= 1) data_ptr(i,j) = 0.0
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID CANOPY MOISTURE CONTENT."
 call ESMF_FieldGet(canopy_mc_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (veg_greenness_ptr(i,j) <= 0.01) data_ptr(i,j) = 0.0
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID ICE SKIN TEMP."
 call ESMF_FieldGet(seaice_skin_temp_target_grid, &
                    farrayPtr=seaice_skint_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET GRID SKIN TEMP."
 call ESMF_FieldGet(skin_temp_target_grid, &
                    farrayPtr=skint_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET GRID SEA ICE FRACTION."
 call ESMF_FieldGet(seaice_fract_target_grid, &
                    farrayPtr=fice_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (fice_ptr(i,j) > 0.0) then
     skint_ptr(i,j) = (fice_ptr(i,j) * seaice_skint_ptr(i,j)) +  &
                      ( (1.0 - fice_ptr(i,j)) * frz_ice )
   else
     seaice_skint_ptr(i,j) = skint_ptr(i,j)
   endif
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID SUBSTRATE TEMP."
 call ESMF_FieldGet(substrate_temp_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) == 2) then  ! sea ice
     data_ptr(i,j) = frz_ice
   elseif (landmask_ptr(i,j) == 0) then  ! open water flag value.
     data_ptr(i,j) = skint_ptr(i,j)
   endif
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID TOTAL SOIL MOISTURE."
 call ESMF_FieldGet(soilm_tot_target_grid, &
                    farrayPtr=soilmt_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET GRID LIQUID SOIL MOISTURE."
 call ESMF_FieldGet(soilm_liq_target_grid, &
                    farrayPtr=soilml_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) == 2 .or. landmask_ptr(i,j) == 0 .or. &
       nint(veg_type_ptr(i,j)) == veg_type_landice_target) then
     soilmt_ptr(i,j,:) = 1.0
     soilml_ptr(i,j,:) = 1.0
   endif
 enddo
 enddo

 print*,"- CALL FieldGet FOR TARGET GRID SOIL TEMPERATURE."
 call ESMF_FieldGet(soil_temp_target_grid, &
                    farrayPtr=data3d_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
 do i = clb(1), cub(1)
   if (landmask_ptr(i,j) == 0) then
     data3d_ptr(i,j,:) = skint_ptr(i,j)  ! open water flag value.
   endif
 enddo
 enddo

 return

 end subroutine qc_check

!---------------------------------------------------------------------------------------------
! nst is not active at land or sea ice points.  Set nst fields to flag values at these
! points.
!---------------------------------------------------------------------------------------------

 subroutine nst_land_fill

 use model_grid, only         : landmask_target_grid

 implicit none

 integer(esmf_kind_i8), pointer     :: mask_ptr(:,:)
 integer                            :: rc

 real(esmf_kind_r8), pointer        :: data_ptr(:,:)
 real(esmf_kind_r8), pointer        :: skint_ptr(:,:)

 print*,"- CALL FieldGet FOR TARGET GRID LANDMASK."
 call ESMF_FieldGet(landmask_target_grid, &
                    farrayPtr=mask_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

! c_d

 print*,"- CALL FieldGet FOR C_D."
 call ESMF_FieldGet(c_d_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! c_0

 print*,"- CALL FieldGet FOR C_0."
 call ESMF_FieldGet(c_0_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! d_conv

 print*,"- CALL FieldGet FOR D_CONV."
 call ESMF_FieldGet(d_conv_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! dt_cool

 print*,"- CALL FieldGet FOR DT_COOL."
 call ESMF_FieldGet(dt_cool_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! ifd

 print*,"- CALL FieldGet FOR IFD."
 call ESMF_FieldGet(ifd_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! qrain

 print*,"- CALL FieldGet FOR QRAIN."
 call ESMF_FieldGet(qrain_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! tref

 print*,"- CALL FieldGet FOR TREF."
 call ESMF_FieldGet(tref_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR SKIN T."
 call ESMF_FieldGet(skin_temp_target_grid, &
                    farrayPtr=skint_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = skint_ptr

! w_d

 print*,"- CALL FieldGet FOR W_D."
 call ESMF_FieldGet(w_d_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! w_0

 print*,"- CALL FieldGet FOR W_0."
 call ESMF_FieldGet(w_0_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! xs

 print*,"- CALL FieldGet FOR XS."
 call ESMF_FieldGet(xs_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! xt

 print*,"- CALL FieldGet FOR XT."
 call ESMF_FieldGet(xt_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! xu

 print*,"- CALL FieldGet FOR XU."
 call ESMF_FieldGet(xu_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! xv

 print*,"- CALL FieldGet FOR XV."
 call ESMF_FieldGet(xv_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! xz

 print*,"- CALL FieldGet FOR XZ."
 call ESMF_FieldGet(xz_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 30.0

! xtts

 print*,"- CALL FieldGet FOR XTTS."
 call ESMF_FieldGet(xtts_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! xzts

 print*,"- CALL FieldGet FOR XZTS."
 call ESMF_FieldGet(xzts_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! z_c

 print*,"- CALL FieldGet FOR Z_C."
 call ESMF_FieldGet(z_c_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

! zm

 print*,"- CALL FieldGet FOR ZM."
 call ESMF_FieldGet(zm_target_grid, &
                    farrayPtr=data_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 where(mask_ptr /= 0) data_ptr = 0.0

 end subroutine nst_land_fill

 subroutine create_surface_esmf_fields

 use model_grid, only         : target_grid, lsoil_target

 implicit none

 integer                     :: rc

 print*,"- CALL FieldCreate FOR TARGET GRID T2M."
 t2m_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID Q2M."
 q2m_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID TPRCP."
 tprcp_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID F10M."
 f10m_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID FFMM."
 ffmm_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID USTAR."
 ustar_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID SNOW LIQ EQUIV."
 snow_liq_equiv_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID SNOW DEPTH."
 snow_depth_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID SEA ICE FRACTION."
 seaice_fract_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID SEA ICE DEPTH."
 seaice_depth_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID SEA ICE SKIN TEMP."
 seaice_skin_temp_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID SRFLAG."
 srflag_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID SKIN TEMPERATURE."
 skin_temp_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID CANOPY MOISTURE CONTENT."
 canopy_mc_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID Z0."
 z0_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID TERRAIN."
 terrain_from_input_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID SOIL TYPE."
 soil_type_from_input_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID SOIL TEMPERATURE."
 soil_temp_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_target/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID TOTAL SOIL MOISTURE."
 soilm_tot_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_target/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID LIQUID SOIL MOISTURE."
 soilm_liq_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_target/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 end subroutine create_surface_esmf_fields

 subroutine create_nst_esmf_fields

 use model_grid, only               : target_grid

 implicit none

 integer                           :: rc

 print*,"- CALL FieldCreate FOR TARGET GRID C_D."
 c_d_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID C_0."
 c_0_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID D_CONV."
 d_conv_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID DT_COOL."
 dt_cool_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID IFD."
 ifd_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID QRAIN."
 qrain_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID TREF."
 tref_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID W_D."
 w_d_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID W_0."
 w_0_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID XS."
 xs_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID XT."
 xt_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID XU."
 xu_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID XV."
 xv_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID XZ."
 xz_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID XTTS."
 xtts_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID XZTS."
 xzts_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID Z_C."
 z_c_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID ZM."
 zm_target_grid = ESMF_FieldCreate(target_grid, &
                                    typekind=ESMF_TYPEKIND_R8, &
                                    staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 end subroutine create_nst_esmf_fields

 subroutine ij_to_i_j(ij, itile, jtile, i, j)

 implicit none

 integer(esmf_kind_i4), intent(in)  :: ij
 integer              , intent(in)  :: itile, jtile

 integer              , intent(out) :: i, j

 integer                            :: tile_num
 integer                            :: pt_loc_this_tile

 tile_num = ((ij-1) / (itile*jtile)) ! tile number minus 1
 pt_loc_this_tile = ij - (tile_num * itile * jtile)
                                     ! "ij" location of point within tile.

 j = (pt_loc_this_tile - 1) / itile + 1
 i = mod(pt_loc_this_tile, itile)

 if (i==0) i = itile

 return

 end subroutine ij_to_i_j

 subroutine cleanup_target_sfc_data

 implicit none

 integer                     :: rc

 print*,"- DESTROY TARGET GRID SURFACE FIELDS."

 call ESMF_FieldDestroy(t2m_target_grid, rc=rc)
 call ESMF_FieldDestroy(q2m_target_grid, rc=rc)
 call ESMF_FieldDestroy(tprcp_target_grid, rc=rc)
 call ESMF_FieldDestroy(f10m_target_grid, rc=rc)
 call ESMF_FieldDestroy(ffmm_target_grid, rc=rc)
 call ESMF_FieldDestroy(ustar_target_grid, rc=rc)
 call ESMF_FieldDestroy(snow_liq_equiv_target_grid, rc=rc)
 call ESMF_FieldDestroy(snow_depth_target_grid, rc=rc)
 call ESMF_FieldDestroy(seaice_fract_target_grid, rc=rc)
 call ESMF_FieldDestroy(seaice_depth_target_grid, rc=rc)
 call ESMF_FieldDestroy(seaice_skin_temp_target_grid, rc=rc)
 call ESMF_FieldDestroy(srflag_target_grid, rc=rc)
 call ESMF_FieldDestroy(skin_temp_target_grid, rc=rc)
 call ESMF_FieldDestroy(canopy_mc_target_grid, rc=rc)
 call ESMF_FieldDestroy(z0_target_grid, rc=rc)
 call ESMF_FieldDestroy(terrain_from_input_grid, rc=rc)
 call ESMF_FieldDestroy(soil_type_from_input_grid, rc=rc)
 call ESMF_FieldDestroy(soil_temp_target_grid, rc=rc)
 call ESMF_FieldDestroy(soilm_tot_target_grid, rc=rc)
 call ESMF_FieldDestroy(soilm_liq_target_grid, rc=rc)

 end subroutine cleanup_target_sfc_data

 subroutine cleanup_target_nst_data

 implicit none

 integer                            :: rc

 print*,"- DESTROY TARGET GRID NST DATA."

 call ESMF_FieldDestroy(c_d_target_grid, rc=rc)
 call ESMF_FieldDestroy(c_0_target_grid, rc=rc)
 call ESMF_FieldDestroy(d_conv_target_grid, rc=rc)
 call ESMF_FieldDestroy(dt_cool_target_grid, rc=rc)
 call ESMF_FieldDestroy(ifd_target_grid, rc=rc)
 call ESMF_FieldDestroy(qrain_target_grid, rc=rc)
 call ESMF_FieldDestroy(tref_target_grid, rc=rc)
 call ESMF_FieldDestroy(w_d_target_grid, rc=rc)
 call ESMF_FieldDestroy(w_0_target_grid, rc=rc)
 call ESMF_FieldDestroy(xs_target_grid, rc=rc)
 call ESMF_FieldDestroy(xt_target_grid, rc=rc)
 call ESMF_FieldDestroy(xu_target_grid, rc=rc)
 call ESMF_FieldDestroy(xv_target_grid, rc=rc)
 call ESMF_FieldDestroy(xz_target_grid, rc=rc)
 call ESMF_FieldDestroy(xtts_target_grid, rc=rc)
 call ESMF_FieldDestroy(xzts_target_grid, rc=rc)
 call ESMF_FieldDestroy(z_c_target_grid, rc=rc)
 call ESMF_FieldDestroy(zm_target_grid, rc=rc)

 end subroutine cleanup_target_nst_data

 end module surface
