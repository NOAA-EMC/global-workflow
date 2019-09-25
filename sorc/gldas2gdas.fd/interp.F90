 module interp

 use esmf
 use input_data
 use model_grid

 type(esmf_field), public           :: soil_type_from_input_grid
 type(esmf_field), public           :: soil_type_target_grid
 type(esmf_field), public           :: lsmask_target_grid
 type(esmf_field), public           :: soilm_tot_target_grid
 type(esmf_field), public           :: soilm_liq_target_grid
 type(esmf_field), public           :: soil_temp_target_grid
 type(esmf_field), public           :: veg_greenness_target_grid

 contains

 subroutine interp_sfc(localpet)

 implicit none

 integer, intent(in) :: localpet

 integer             :: rc, isrctermprocessing

 integer(esmf_kind_i4), pointer     :: lsmask_target_ptr(:,:)
 integer(esmf_kind_i4), pointer     :: lsmask_input_ptr(:,:)
 integer(esmf_kind_i4), pointer     :: mask_target_ptr(:,:)
 integer(esmf_kind_i4), pointer     :: mask_input_ptr(:,:)
 integer(esmf_kind_i4), pointer      :: unmapped_ptr(:)

 type(esmf_regridmethod_flag)        :: method
 type(esmf_routehandle)              :: regrid_land

 call set_gdas_mask(localpet)

! This is the gldas soil type interpolated to the gdas grid.
! Needed to rescale soil moisture.

 print*,"- CALL FieldCreate FOR interpolated soil type."
 soil_type_from_input_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR target gdas smc."
 soilm_tot_target_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, &
                                     ungriddedLBound=(/1/), &
                                     ungriddedUBound=(/lsoil/),rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR target gdas slc."
 soilm_liq_target_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, &
                                     ungriddedLBound=(/1/), &
                                     ungriddedUBound=(/lsoil/),rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR target gdas stc."
 soil_temp_target_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, &
                                     ungriddedLBound=(/1/), &
                                     ungriddedUBound=(/lsoil/),rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

!----------------------------------------------------------------------------
! Interpolate all land to all land
!----------------------------------------------------------------------------

 print*,"- CALL GridAddItem FOR TARGET GRID."
 call ESMF_GridAddItem(gdas_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddItem", rc)

 print*,"- CALL GridGetItem FOR TARGET GRID."
 nullify(mask_target_ptr)
 call ESMF_GridGetItem(gdas_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       farrayPtr=mask_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetItem", rc)

 call ESMF_FieldGet(lsmask_target_grid, farrayPtr=lsmask_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetItem", rc)

!mask_target_ptr = 1  
 mask_target_ptr = lsmask_target_ptr

 print*,"- CALL GridAddItem FOR INPUT GRID."
 call ESMF_GridAddItem(gldas_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddItem", rc)

 print*,"- CALL GridGetItem FOR INPUT GRID."
 nullify(mask_input_ptr)
 call ESMF_GridGetItem(gldas_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       farrayPtr=mask_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetItem", rc)

 call ESMF_FieldGet(lsmask_input_grid, farrayPtr=lsmask_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetItem", rc)

!mask_input_ptr = 1
 mask_input_ptr = lsmask_input_ptr

 method=ESMF_REGRIDMETHOD_NEAREST_STOD
 isrctermprocessing = 1

 print*,"- CALL FieldRegridStore for land to land."
 call ESMF_FieldRegridStore(soil_type_input_grid, &
                            soil_type_from_input_grid, &
                            srcmaskvalues=(/0/), &
                            dstmaskvalues=(/0/), &
                            polemethod=ESMF_POLEMETHOD_NONE, &
                            srctermprocessing=isrctermprocessing, &
                            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
                            routehandle=regrid_land, &
                            regridmethod=method, &
                            unmappedDstList=unmapped_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridStore", rc)

 print*,"- CALL Field_Regrid for soil type."
 call ESMF_FieldRegrid(soil_type_input_grid, &
                       soil_type_from_input_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for soil temp."
 call ESMF_FieldRegrid(soil_temp_input_grid, &
                       soil_temp_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for slc."
 call ESMF_FieldRegrid(soilm_liq_input_grid, &
                       soilm_liq_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for smc."
 call ESMF_FieldRegrid(soilm_tot_input_grid, &
                       soilm_tot_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

! rescale soil moisture

 call rescale_soil_moisture(localpet)

 end subroutine interp_sfc

!---------------------------------------------------
! Set the interpolation mask for the gdas grid.
!---------------------------------------------------

 subroutine set_gdas_mask(localpet)

 use netcdf 

 implicit none

 integer, intent(in) :: localpet

 character(len=100)  :: gdas_files(6)

 integer :: rc, error, ncid, tile, id_var, i, j

 integer (esmf_kind_i4), allocatable :: final_mask_one_tile(:,:)

 real (kind=4), allocatable :: mask_one_tile(:,:)
 real (esmf_kind_r8), allocatable :: soilt_one_tile(:,:), green_one_tile(:,:)
 real (kind=4), allocatable :: snow_one_tile(:,:)
 real (kind=4), allocatable :: soilm_one_tile(:,:,:)

 print*,"- CALL FieldCreate FOR target gdas greenness."
 veg_greenness_target_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR target landmask."
 lsmask_target_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_I4, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR target soil type."
 soil_type_target_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 if (localpet == 0) then
   allocate (final_mask_one_tile(i_gdas, j_gdas))
   allocate (mask_one_tile(i_gdas, j_gdas))
   allocate (green_one_tile(i_gdas, j_gdas))
   allocate (soilm_one_tile(i_gdas, j_gdas, lsoil))
   allocate (snow_one_tile(i_gdas, j_gdas))
   allocate (soilt_one_tile(i_gdas, j_gdas))
 else
   allocate (final_mask_one_tile(0,0))
   allocate (mask_one_tile(0,0))
   allocate (green_one_tile(0,0))
   allocate (soilm_one_tile(0,0,0))
   allocate (snow_one_tile(0,0))
   allocate (soilt_one_tile(0,0))
 endif

 gdas_files(1) = "./sfc_data.tile1.nc"
 gdas_files(2) = "./sfc_data.tile2.nc"
 gdas_files(3) = "./sfc_data.tile3.nc"
 gdas_files(4) = "./sfc_data.tile4.nc"
 gdas_files(5) = "./sfc_data.tile5.nc"
 gdas_files(6) = "./sfc_data.tile6.nc"
 
 do tile = 1, 6

   if(localpet == 0) then

     print*,'open ',trim(gdas_files(tile))
     error = nf90_open(trim(gdas_files(tile)), nf90_nowrite, ncid)
     call netcdf_err(error, 'opening gdas surface file')

     error = nf90_inq_varid(ncid, 'vfrac', id_var)
     call netcdf_err(error, 'getting vfrac id')
     error = nf90_get_var(ncid, id_var, green_one_tile)
     call netcdf_err(error, 'reading vfrac')
     print*,'gdas vfrac ',maxval(green_one_tile),minval(green_one_tile)

     error = nf90_inq_varid(ncid, 'slmsk', id_var)
     call netcdf_err(error, 'getting mask id')
     error = nf90_get_var(ncid, id_var, mask_one_tile)
     call netcdf_err(error, 'reading mask')
     print*,'gdas mask ',maxval(mask_one_tile),minval(mask_one_tile)

     error = nf90_inq_varid(ncid, 'stype', id_var)
     call netcdf_err(error, 'getting stype id')
     error = nf90_get_var(ncid, id_var, soilt_one_tile)
     call netcdf_err(error, 'reading stype')
     print*,'gdas stype ',maxval(soilt_one_tile),minval(soilt_one_tile)

     error = nf90_inq_varid(ncid, 'sheleg', id_var)
     call netcdf_err(error, 'getting sheleg id')
     error = nf90_get_var(ncid, id_var, snow_one_tile)
     call netcdf_err(error, 'reading snow')
     print*,'gdas snow ',maxval(snow_one_tile),minval(snow_one_tile)

     error = nf90_inq_varid(ncid, 'smc', id_var)
     call netcdf_err(error, 'getting smc id')
     error = nf90_get_var(ncid, id_var, soilm_one_tile)
     call netcdf_err(error, 'reading soilm')
     print*,'gdas soilm ',maxval(soilm_one_tile),minval(soilm_one_tile)

! Set gdas interpolation mask.  Only update points that are snow free
! and not glacial ice.

     final_mask_one_tile = 1

     do j = 1, j_gdas
     do i = 1, i_gdas
       if (mask_one_tile(i,j) < 0.5 .or. mask_one_tile(i,j) > 1.5) then  ! dont includ ice or water
         final_mask_one_tile(i,j) = 0
       endif
       if (soilm_one_tile(i,j,1) > 0.9) then  ! dont include glacial ice
         final_mask_one_tile(i,j) = 0
       endif
       if (snow_one_tile(i,j) > 0.0) then  ! dont include snow
         final_mask_one_tile(i,j) = 0
       endif
     enddo
     enddo

     error = nf90_close (ncid)

   endif  ! is this localpet 0?

   print*,"- CALL FieldScatter FOR gdas mask."
   call ESMF_FieldScatter(lsmask_target_grid, final_mask_one_tile, rootpet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)
   
   print*,"- CALL FieldScatter FOR gdas soil type."
   call ESMF_FieldScatter(soil_type_target_grid, soilt_one_tile, rootpet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

   print*,"- CALL FieldScatter FOR gdas green."
   call ESMF_FieldScatter(veg_greenness_target_grid, green_one_tile, rootpet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

 enddo  ! tile 

 deallocate (soilt_one_tile, mask_one_tile, snow_one_tile, soilm_one_tile, final_mask_one_tile, green_one_tile)

 end subroutine set_gdas_mask

 subroutine rescale_soil_moisture(localpet)

 use esmf

 implicit none

 integer, intent(in) :: localpet
 integer                            :: clb(3), cub(3), i, j, k, rc
 integer                            :: soilt_input, soilt_target
 integer, parameter                 :: num_soil_cats = 16
 integer(esmf_kind_i4), pointer     :: landmask_ptr(:,:)

 real(esmf_kind_r8), pointer        :: soilm_liq_ptr(:,:,:)
 real(esmf_kind_r8), pointer        :: soilm_tot_ptr(:,:,:)
 real(esmf_kind_r8), pointer        :: soil_type_input_ptr(:,:)
 real(esmf_kind_r8), pointer        :: soil_type_target_ptr(:,:)
 real(esmf_kind_r8), pointer        :: veg_greenness_ptr(:,:)
 real                               :: percent_frozen(4)
 real                               :: f1, fn, smcdir, smctra
 real                               :: bb(num_soil_cats)
 real                               :: maxsmc(num_soil_cats)
 real                               :: satdk(num_soil_cats)
 real                               :: satpsi(num_soil_cats)
 real                               :: satdw(num_soil_cats)
 real                               :: f11(num_soil_cats)
 real                               :: refsmc(num_soil_cats)
 real                               :: refsmc1, smhigh, smlow
 real                               :: wltsmc(num_soil_cats)
 real                               :: wltsmc1
 real                               :: drysmc(num_soil_cats)

 data smlow /0.5/

 data smhigh /6.0/

 data bb /4.05, 4.26, 4.74, 5.33, 5.33, 5.25, &
          6.77, 8.72, 8.17, 10.73, 10.39, 11.55, &
          5.25, -9.99, 4.05, 4.26/

 data maxsmc /0.395, 0.421, 0.434, 0.476, 0.476, 0.439, &
              0.404, 0.464, 0.465, 0.406, 0.468, 0.457, &
              0.464, -9.99, 0.200, 0.421/

 data satdk /1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6, &
             3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6, &
             1.3444e-6, 9.7384e-7, 3.3770e-6,     -9.99, 1.4078e-5, &
             1.4078e-5/

 data satpsi /0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548, &
              0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677, &
              0.3548, -9.99,  0.0350, 0.0363/

 do i = 1, num_soil_cats

   if (maxsmc(i) > 0.0) then

   SATDW(I)  = BB(I)*SATDK(I)*(SATPSI(I)/MAXSMC(I))
   F11(I) = ALOG10(SATPSI(I)) + BB(I)*ALOG10(MAXSMC(I)) + 2.0
   REFSMC1 = MAXSMC(I)*(5.79E-9/SATDK(I)) **(1.0/(2.0*BB(I)+3.0))
   REFSMC(I) = REFSMC1 + (MAXSMC(I)-REFSMC1) / SMHIGH
   WLTSMC1 = MAXSMC(I) * (200.0/SATPSI(I))**(-1.0/BB(I))
   WLTSMC(I) = WLTSMC1 - SMLOW * WLTSMC1

!----------------------------------------------------------------------
!  CURRENT VERSION DRYSMC VALUES THAT EQUATE TO WLTSMC.
!  FUTURE VERSION COULD LET DRYSMC BE INDEPENDENTLY SET VIA NAMELIST.
!----------------------------------------------------------------------

   DRYSMC(I) = WLTSMC(I)

   end if

 END DO

 if (localpet == 0) then
 print*,'maxsmc ',maxsmc
 print*,'refsmc ',refsmc
 print*,'wltsmc ',wltsmc
 print*,'drysmc ',drysmc
 endif

 print*,"- RESCALE SOIL MOISTURE FOR CHANGES IN SOIL TYPE."

 print*,"- CALL FieldGet FOR liquid SOIL MOISTURE."
 call ESMF_FieldGet(soilm_liq_target_grid, &
                    computationalLBound=clb, &
                    computationalUBound=cub, &
                    farrayPtr=soilm_liq_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TOTAL SOIL MOISTURE."
 call ESMF_FieldGet(soilm_tot_target_grid, &
                    computationalLBound=clb, &
                    computationalUBound=cub, &
                    farrayPtr=soilm_tot_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

! this is the mask of points processed by this code - i.e., not including snow
! or glacial points.

 print*,"- CALL FieldGet FOR LAND MASK."
 call ESMF_FieldGet(lsmask_target_grid, &
                    farrayPtr=landmask_ptr, rc=rc)
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
! Check land points.
!---------------------------------------------------------------------------------------------

     if (landmask_ptr(i,j) == 1) then

        do k = 1, 4
          percent_frozen(k) = soilm_liq_ptr(i,j,k) / soilm_tot_ptr(i,j,k) 
        enddo

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

          f1=(soilm_tot_ptr(i,j,1)-drysmc(soilt_input)) /    &
             (maxsmc(soilt_input)-drysmc(soilt_input))

          smcdir=drysmc(soilt_target) + f1 *        &
                (maxsmc(soilt_target) - drysmc(soilt_target))

!---------------------------------------------------------------------------------------------
! Continue top layer rescale.  Now determine transpiration part:
!---------------------------------------------------------------------------------------------

          if (soilm_tot_ptr(i,j,1) < refsmc(soilt_input)) then
            f1=(soilm_tot_ptr(i,j,1) - wltsmc(soilt_input)) /       &
               (refsmc(soilt_input) - wltsmc(soilt_input))
            smctra=wltsmc(soilt_target) + f1  *     &
                  (refsmc(soilt_target) - wltsmc(soilt_target))
          else
            f1=(soilm_tot_ptr(i,j,1) - refsmc(soilt_input)) /        &
               (maxsmc(soilt_input) - refsmc(soilt_input))
            smctra=refsmc(soilt_target) + f1 *      &
                  (maxsmc(soilt_target) - refsmc(soilt_target))
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
            if (soilm_tot_ptr(i,j,k) < refsmc(soilt_input)) then
              fn = (soilm_tot_ptr(i,j,k) - wltsmc(soilt_input)) /        &
                (refsmc(soilt_input) - wltsmc(soilt_input))
              soilm_tot_ptr(i,j,k) = wltsmc(soilt_target) + fn *         &
                (refsmc(soilt_target) - wltsmc(soilt_target))
            else
              fn = (soilm_tot_ptr(i,j,k) - refsmc(soilt_input)) /         &
                (maxsmc(soilt_input) - refsmc(soilt_input))
              soilm_tot_ptr(i,j,k) = refsmc(soilt_target) + fn *         &
                (maxsmc(soilt_target) - refsmc(soilt_target))
            endif
          enddo

        endif ! is soil type different?

!---------------------------------------------------------------------------------------------
! Range check all layers.
!---------------------------------------------------------------------------------------------

 
        soilm_tot_ptr(i,j,1)=min(soilm_tot_ptr(i,j,1),maxsmc(soilt_target))
        soilm_tot_ptr(i,j,1)=max(drysmc(soilt_target),soilm_tot_ptr(i,j,1))

        soilm_liq_ptr(i,j,1) =  percent_frozen(1) * soilm_tot_ptr(i,j,1)

        do k = 2, cub(3)
          soilm_tot_ptr(i,j,k)=min(soilm_tot_ptr(i,j,k),maxsmc(soilt_target))
          soilm_tot_ptr(i,j,k)=max(wltsmc(soilt_target),soilm_tot_ptr(i,j,k))
          soilm_liq_ptr(i,j,k) =  percent_frozen(k) * soilm_tot_ptr(i,j,k)
        enddo
  
     endif ! is this a land point?

   enddo
 enddo

 return

 end subroutine rescale_soil_moisture

 end module interp
