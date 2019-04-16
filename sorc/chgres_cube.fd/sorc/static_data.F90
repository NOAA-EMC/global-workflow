 module static_data

!--------------------------------------------------------------------------
! Module static data
!
! Abstract: Read pre-computed static/climatological data on the fv3 
!    target grid.  Time interpolate if necessary (for example a
!    monthly climo fields).
!
! Public Subroutines:
! -------------------
! get_static_fields            Driver routine to read/time interpolate
!                              static/climo fields on the fv3 target
!                              grid.
! cleanup_static_fields        Free up memory for fields in this module.
!
! Public variables:
! -----------------
! alnsf_target_grid                 near ir black sky albedo
! alnwf_target_grid                 near ir white sky albedo
! alvsf_target_grid                 visible black sky albedo
! alvwf_target_grid                 visible white sky albedo
! facsf_target_grid                 fractional coverage for strong
!                                   zenith angle dependent albedo
! facwf_target_grid                 fractional coverage for weak
!                                   zenith angle dependent albedo
! max_veg_greenness_target_grid     maximum annual greenness fraction
! min_veg_greenness_target_grid     minimum annual greenness fraction
! mxsno_albedo_target_grid          maximum snow albedo
! slope_type_target_grid            slope type
! soil_type_target_grid             soil type
! substrate_temp_target_grid        soil subtrate temperature
! veg_greenness_target_grid         vegetation greenness fraction
! veg_type_targe_grid               vegetation type
!
!--------------------------------------------------------------------------

 use esmf

 implicit none

 private

 type(esmf_field), public           :: alvsf_target_grid
 type(esmf_field), public           :: alvwf_target_grid
 type(esmf_field), public           :: alnsf_target_grid
 type(esmf_field), public           :: alnwf_target_grid
 type(esmf_field), public           :: facsf_target_grid
 type(esmf_field), public           :: facwf_target_grid
 type(esmf_field), public           :: max_veg_greenness_target_grid
 type(esmf_field), public           :: min_veg_greenness_target_grid
 type(esmf_field), public           :: mxsno_albedo_target_grid
 type(esmf_field), public           :: slope_type_target_grid
 type(esmf_field), public           :: soil_type_target_grid
 type(esmf_field), public           :: substrate_temp_target_grid
 type(esmf_field), public           :: veg_greenness_target_grid
 type(esmf_field), public           :: veg_type_target_grid

 public :: get_static_fields
 public :: cleanup_static_fields

 contains

!------------------------------------------------------------------------------
! Read static fields on the target grid.
!------------------------------------------------------------------------------

 subroutine get_static_fields(localpet)

 use model_grid, only               : target_grid, &
                                      num_tiles_target_grid, &
                                      i_target, j_target

 implicit none

 integer, intent(in)                :: localpet

 integer                            :: error, tile, i, j

 real(esmf_kind_r8), allocatable    :: data_one_tile(:,:)
 real(esmf_kind_r8), allocatable    :: max_data_one_tile(:,:)
 real(esmf_kind_r8), allocatable    :: min_data_one_tile(:,:)

 if (localpet==0) then
   allocate(data_one_tile(i_target,j_target))
 else
   allocate(data_one_tile(0,0))
 endif

!------------------------------------------------------------------------------
! Slope type
!------------------------------------------------------------------------------

 print*,"- CALL FieldCreate FOR TARGET GRID SLOPE TYPE."
 slope_type_target_grid = ESMF_FieldCreate(target_grid, &
                                           typekind=ESMF_TYPEKIND_R8, &
                                           staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 do tile = 1, num_tiles_target_grid
   if (localpet == 0) then
     call read_static_file('slope_type', i_target, j_target, tile, data_one_tile)
   endif
   print*,"- CALL FieldScatter FOR TARGET GRID SLOPE TYPE."
   call ESMF_FieldScatter(slope_type_target_grid, data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
 enddo

!------------------------------------------------------------------------------
! Maximum snow albedo.
!------------------------------------------------------------------------------

 print*,"- CALL FieldCreate FOR TARGET GRID MAXIMUM SNOW ALBEDO."
 mxsno_albedo_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 do tile = 1, num_tiles_target_grid
   if (localpet == 0) then
     call read_static_file('maximum_snow_albedo', i_target, j_target, tile, data_one_tile)
   endif
   print*,"- CALL FieldScatter FOR TARGET GRID MAXIMUM SNOW ALBEDO."
   call ESMF_FieldScatter(mxsno_albedo_target_grid, data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
 enddo

!------------------------------------------------------------------------------
! Soil type
!------------------------------------------------------------------------------

 print*,"- CALL FieldCreate FOR TARGET GRID SOIL TYPE."
 soil_type_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 do tile = 1, num_tiles_target_grid
   if (localpet == 0) then
     call read_static_file('soil_type', i_target, j_target, tile, data_one_tile)
   endif
   print*,"- CALL FieldScatter FOR TARGET GRID SOIL TYPE."
   call ESMF_FieldScatter(soil_type_target_grid, data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
 enddo

!------------------------------------------------------------------------------
! Vegetation type
!------------------------------------------------------------------------------

 print*,"- CALL FieldCreate FOR TARGET GRID VEGETATION TYPE."
 veg_type_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 do tile = 1, num_tiles_target_grid
   if (localpet == 0) then
     call read_static_file('vegetation_type', i_target, j_target, tile, data_one_tile)
   endif
   print*,"- CALL FieldScatter FOR TARGET GRID VEGETATION TYPE."
   call ESMF_FieldScatter(veg_type_target_grid, data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
 enddo

!------------------------------------------------------------------------------
! Vegetation greenness
!------------------------------------------------------------------------------

 print*,"- CALL FieldCreate FOR TARGET GRID VEGETATION GREENNESS."
 veg_greenness_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR TARGET GRID MAXIMUM VEGETATION GREENNESS."
 max_veg_greenness_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR TARGET GRID MINIMUM VEGETATION GREENNESS."
 min_veg_greenness_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 if (localpet == 0) then
   allocate(max_data_one_tile(i_target,j_target))
   allocate(min_data_one_tile(i_target,j_target))
 else
   allocate(max_data_one_tile(0,0))
   allocate(min_data_one_tile(0,0))
 endif

 do tile = 1, num_tiles_target_grid
   if (localpet == 0) then
     call read_static_file('vegetation_greenness', i_target, j_target, tile, data_one_tile, &
                            max_data_one_tile, min_data_one_tile)
   endif
   print*,"- CALL FieldScatter FOR TARGET GRID VEGETATION GREENNESS."
   call ESMF_FieldScatter(veg_greenness_target_grid, data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR TARGET GRID MAXIMUM VEGETATION GREENNESS."
   call ESMF_FieldScatter(max_veg_greenness_target_grid, max_data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR TARGET GRID MINIMUM VEGETATION GREENNESS."
   call ESMF_FieldScatter(min_veg_greenness_target_grid, min_data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
 enddo

 deallocate(max_data_one_tile, min_data_one_tile)

!------------------------------------------------------------------------------
! Soil substrate temperature
!------------------------------------------------------------------------------

 print*,"- CALL FieldCreate FOR TARGET GRID SUBSTRATE TEMPERATURE."
 substrate_temp_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 do tile = 1, num_tiles_target_grid
   if (localpet == 0) then
     call read_static_file('substrate_temperature', i_target, j_target, tile, data_one_tile)
   endif
   print*,"- CALL FieldScatter FOR TARGET GRID SUBSTRATE TEMPERATURE."
   call ESMF_FieldScatter(substrate_temp_target_grid, data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
 enddo

!------------------------------------------------------------------------------
! Four-component albedo.
!------------------------------------------------------------------------------

 print*,"- CALL FieldCreate FOR ALVSF."
 alvsf_target_grid = ESMF_FieldCreate(target_grid, &
                                      typekind=ESMF_TYPEKIND_R8, &
                                      staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 do tile = 1, num_tiles_target_grid
   if (localpet == 0) then
     call read_static_file('visible_black_sky_albedo', i_target, j_target, tile, data_one_tile)
   endif
   print*,"- CALL FieldScatter FOR TARGET GRID ALVSF."
   call ESMF_FieldScatter(alvsf_target_grid, data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
 enddo

 print*,"- CALL FieldCreate FOR ALVWF."
 alvwf_target_grid = ESMF_FieldCreate(target_grid, &
                                      typekind=ESMF_TYPEKIND_R8, &
                                      staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 do tile = 1, num_tiles_target_grid
   if (localpet == 0) then
     call read_static_file('visible_white_sky_albedo', i_target, j_target, tile, data_one_tile)
   endif
   print*,"- CALL FieldScatter FOR TARGET GRID ALVWF."
   call ESMF_FieldScatter(alvwf_target_grid, data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
 enddo

 print*,"- CALL FieldCreate FOR ALNSF."
 alnsf_target_grid = ESMF_FieldCreate(target_grid, &
                                      typekind=ESMF_TYPEKIND_R8, &
                                      staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 do tile = 1, num_tiles_target_grid
   if (localpet == 0) then
     call read_static_file('near_IR_black_sky_albedo', i_target, j_target, tile, data_one_tile)
   endif
   print*,"- CALL FieldScatter FOR TARGET GRID ALNSF."
   call ESMF_FieldScatter(alnsf_target_grid, data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
 enddo

 print*,"- CALL FieldCreate FOR ALNWF."
 alnwf_target_grid = ESMF_FieldCreate(target_grid, &
                                      typekind=ESMF_TYPEKIND_R8, &
                                      staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 do tile = 1, num_tiles_target_grid
   if (localpet == 0) then
     call read_static_file('near_IR_white_sky_albedo', i_target, j_target, tile, data_one_tile)
   endif
   print*,"- CALL FieldScatter FOR TARGET GRID ALNWF."
   call ESMF_FieldScatter(alnwf_target_grid, data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
 enddo

!------------------------------------------------------------------------------
! facsf and facwf
!------------------------------------------------------------------------------

 print*,"- CALL FieldCreate FOR TARGET GRID FACSF."
 facsf_target_grid = ESMF_FieldCreate(target_grid, &
                                      typekind=ESMF_TYPEKIND_R8, &
                                      staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR TARGET GRID FACWF."
 facwf_target_grid = ESMF_FieldCreate(target_grid, &
                                      typekind=ESMF_TYPEKIND_R8, &
                                      staggerloc=ESMF_STAGGERLOC_CENTER, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 do tile = 1, num_tiles_target_grid
   if (localpet == 0) then
     call read_static_file('facsf', i_target, j_target, tile, data_one_tile)
   endif
   print*,"- CALL FieldScatter FOR TARGET GRID FACSF."
   call ESMF_FieldScatter(facsf_target_grid, data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
   if (localpet == 0) then
     do j = 1, j_target
     do i = 1, i_target
       if (data_one_tile(i,j) >= 0.0) then
         data_one_tile(i,j) = 1.0 - data_one_tile(i,j)
       endif
     enddo
     enddo
   endif
   call ESMF_FieldScatter(facwf_target_grid, data_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
 enddo

 deallocate(data_one_tile)

 end subroutine get_static_fields

!------------------------------------------------------------------------------
! Read data file.
!------------------------------------------------------------------------------

 subroutine read_static_file(field, i_target, j_target, tile, &
                             data_one_tile, max_data_one_tile, &
                             min_data_one_tile)
 
 use netcdf
 use model_grid, only               : tiles_target_grid
 use program_setup, only            : fix_dir_target_grid, cres_target_grid, &
                                      cycle_mon, cycle_day, cycle_hour

 implicit none

 character(len=*), intent(in)       :: field
 character(len=100)                 :: filename
 character(len=500)                 :: the_file

 integer, intent(in)                :: i_target, j_target, tile

 real(esmf_kind_r8), intent(out)    :: data_one_tile(i_target,j_target)
 real(esmf_kind_r8), intent(out), optional    :: max_data_one_tile(i_target,j_target)
 real(esmf_kind_r8), intent(out), optional    :: min_data_one_tile(i_target,j_target)

 integer                            :: bound1, bound2
 integer                            :: error, ncid, id_var, n
 integer                            :: i, j, id_time, num_times
 integer                            :: idat(8), jdat(8)
 integer, allocatable               :: days_since(:)

 real(kind=4), allocatable          :: dummy(:,:,:)
 real(esmf_kind_r8)                 :: num_days, num_days_rec1, rinc(5)
 real(esmf_kind_r8)                 :: weight_rec1, weight_rec2

 if (trim(field) == 'facsf') filename = "/" // trim(cres_target_grid) // ".facsf." // trim(tiles_target_grid(tile)) // ".nc"
 if (trim(field) == 'maximum_snow_albedo') filename = "/" // trim(cres_target_grid) // ".maximum_snow_albedo." // trim(tiles_target_grid(tile)) // ".nc"
 if (trim(field) == 'slope_type') filename = "/" // trim(cres_target_grid) // ".slope_type." // trim(tiles_target_grid(tile)) // ".nc"
 if (trim(field) == 'soil_type') filename = "/" // trim(cres_target_grid) // ".soil_type." // trim(tiles_target_grid(tile)) // ".nc"
 if (trim(field) == 'substrate_temperature') filename = "/" // trim(cres_target_grid) // ".substrate_temperature." // trim(tiles_target_grid(tile)) // ".nc"
 if (trim(field) == 'vegetation_greenness') filename = "/" // trim(cres_target_grid) // ".vegetation_greenness." // trim(tiles_target_grid(tile)) // ".nc"
 if (trim(field) == 'vegetation_type') filename = "/" // trim(cres_target_grid) // ".vegetation_type." // trim(tiles_target_grid(tile)) // ".nc"
 if (trim(field) == 'visible_black_sky_albedo') filename = "/" // trim(cres_target_grid) // ".snowfree_albedo." // trim(tiles_target_grid(tile)) // ".nc"
 if (trim(field) == 'visible_white_sky_albedo') filename = "/" // trim(cres_target_grid) // ".snowfree_albedo." // trim(tiles_target_grid(tile)) // ".nc"
 if (trim(field) == 'near_IR_black_sky_albedo') filename = "/" // trim(cres_target_grid) // ".snowfree_albedo." // trim(tiles_target_grid(tile)) // ".nc"
 if (trim(field) == 'near_IR_white_sky_albedo') filename = "/" // trim(cres_target_grid) // ".snowfree_albedo." // trim(tiles_target_grid(tile)) // ".nc"

 the_file = trim(fix_dir_target_grid) // trim(filename)

 print*,'- OPEN FILE ',trim(the_file)
 error=nf90_open(trim(the_file),nf90_nowrite,ncid)
 call netcdf_err(error, 'OPENING: '//trim(the_file) )

 error=nf90_inq_dimid(ncid, 'time', id_time)
 call netcdf_err(error, 'INQ TIME DIMENSION')
 error=nf90_inquire_dimension(ncid, id_time, len=num_times)
 call netcdf_err(error, 'READING TIME DIMENSION')
 print*,'- FILE CONTAINS ', num_times, ' TIME RECORDS.'

 allocate(dummy(i_target,j_target,num_times))
 error=nf90_inq_varid(ncid, field, id_var)
 call netcdf_err(error, 'READING FIELD ID' )
 error=nf90_get_var(ncid, id_var, dummy)
 call netcdf_err(error, 'READING FIELD' )

 if (num_times > 1) then
   allocate (days_since(num_times))
   error=nf90_inq_varid(ncid, 'time', id_time)
   error=nf90_get_var(ncid, id_time, days_since)
   print*,'- TIME RECORDS (DAYS SINCE): ', days_since
   idat = 0
   idat(1) = 2015
   idat(2) = 1
   idat(3) = 1
   idat(5) = 0
   jdat = 0
   jdat(1) = 2015
   jdat(2) = cycle_mon 
   jdat(3) = cycle_day
   jdat(5) = cycle_hour
   call w3difdat(jdat,idat,1,rinc)
   do n = 1, num_times
     if (rinc(1) <= days_since(n)) exit
   enddo
   bound2 = n
   bound1 = n - 1
   if (bound1 == 0) bound1 = num_times
   if (bound2 == num_times+1) bound2 = 1
   print*,"- BOUNDING TIME RECORDS: ", bound1, bound2
   if (bound2 /= 1) then
     num_days = float(days_since(bound2)) - float(days_since(bound1))
     num_days_rec1 = rinc(1) - float(days_since(bound1))
     weight_rec2 = num_days_rec1 / num_days
     weight_rec1 = 1.0 - weight_rec2
     print*,"- BOUNDING WEIGHTS ", weight_rec1, weight_rec2
   else
     num_days = (float(days_since(bound2)) + 1.0) + (365.0 - float(days_since(bound1)) - 1.0)
     if (rinc(1) >= days_since(bound1)) then
       num_days_rec1 = rinc(1) - float(days_since(bound1))
     else
       num_days_rec1 = (365.0 - float(days_since(bound1))) + rinc(1)
     endif
     weight_rec2 = num_days_rec1 / num_days
     weight_rec1 = 1.0 - weight_rec2
     print*,"- BOUNDING WEIGHTS ", weight_rec1, weight_rec2
   endif

   do j = 1, j_target
   do i = 1, i_target
     data_one_tile(i,j) = (weight_rec1*dummy(i,j,bound1)) + (weight_rec2*dummy(i,j,bound2))
   enddo
   enddo

   deallocate(days_since)

 else ! file contains only one time record

   data_one_tile = dummy(:,:,1)

 endif

 if (trim(field) == 'vegetation_greenness') then

   do j = 1, j_target
   do i = 1, i_target
     max_data_one_tile(i,j) = maxval(dummy(i,j,:))
     min_data_one_tile(i,j) = minval(dummy(i,j,:))
   enddo
   enddo

 endif

 deallocate(dummy)

 error = nf90_close(ncid)

 end subroutine read_static_file
 
 subroutine cleanup_static_fields

 implicit none

 integer                 :: rc

 print*,"- DESTROY STATIC FIELDS."

 call ESMF_FieldDestroy(alvsf_target_grid, rc=rc)
 call ESMF_FieldDestroy(alvwf_target_grid, rc=rc)
 call ESMF_FieldDestroy(alnsf_target_grid, rc=rc)
 call ESMF_FieldDestroy(alnwf_target_grid, rc=rc)
 call ESMF_FieldDestroy(facsf_target_grid, rc=rc)
 call ESMF_FieldDestroy(facwf_target_grid, rc=rc)
 call ESMF_FieldDestroy(max_veg_greenness_target_grid, rc=rc)
 call ESMF_FieldDestroy(min_veg_greenness_target_grid, rc=rc)
 call ESMF_FieldDestroy(mxsno_albedo_target_grid, rc=rc)
 call ESMF_FieldDestroy(slope_type_target_grid, rc=rc)
 call ESMF_FieldDestroy(soil_type_target_grid, rc=rc)
 call ESMF_FieldDestroy(substrate_temp_target_grid, rc=rc)
 call ESMF_FieldDestroy(veg_greenness_target_grid, rc=rc)
 call ESMF_FieldDestroy(veg_type_target_grid, rc=rc)

 end subroutine cleanup_static_fields

 end module static_data
