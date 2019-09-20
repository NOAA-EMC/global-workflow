 module input_data

! Read input gdas data on tiles.

 use esmf

 implicit none

 private

 character(len=200) :: data_dir_input_grid = "NULL"
 character(len=200) :: sfc_files_input_grid(6) = "NULL"

 integer, public :: i_input, j_input
 integer, public :: lsoil_input = 4

 type(esmf_field), public        :: landsea_mask_input_grid
 type(esmf_field), public        :: soil_type_input_grid    ! soil type
 type(esmf_field), public        :: snow_liq_equiv_input_grid
 type(esmf_field), public        :: snow_depth_input_grid
 type(esmf_field), public        :: soilm_liq_input_grid
 type(esmf_field), public        :: soilm_tot_input_grid
 type(esmf_field), public        :: soil_temp_input_grid

 public :: read_input_data

 contains

 subroutine read_input_data(localpet)

 use model_grid

 implicit none

 integer, intent(in) :: localpet

 integer :: rc

 print*,"- CALL FieldCreate FOR INPUT GRID LANDSEA MASK."
 landsea_mask_input_grid = ESMF_FieldCreate(input_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT SOIL TYPE."
 soil_type_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT SNOW LIQ EQUIV."
 snow_liq_equiv_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT PHYSICAL SNOW DEPTH."
 snow_depth_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT SOIL TEMPERATURE."
 soil_temp_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_input/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT TOTAL SOIL MOISTURE."
 soilm_tot_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_input/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT LIQ SOIL MOISTURE."
 soilm_liq_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_input/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)


 call read_input_sfc_restart_file(localpet)


 end subroutine read_input_data

 subroutine read_input_sfc_restart_file(localpet)

 use netcdf

 implicit none

 integer, intent(in) :: localpet

 character(len=400) :: tilefile

 integer :: ncid, id_dim, error, tile, rc, i, j

 real(esmf_kind_r8), allocatable :: data_one_tile(:,:), soilm(:,:,:)
 real(esmf_kind_r8), allocatable :: mask(:,:), snow_liq(:,:), snow_d(:,:)
 real(esmf_kind_r8), allocatable :: data_one_tile_3d(:,:,:)

 namelist /config/ data_dir_input_grid, sfc_files_input_grid

 open(41, file="./fort.41", iostat=rc)
 if (rc /= 0) call error_handler("OPENING SETUP NAMELIST.", rc)
 read(41, nml=config, iostat=rc)
 if (rc /= 0) call error_handler("READING SETUP NAMELIST.", rc)
 close (41)

 data_dir_input_grid=trim(data_dir_input_grid) // '/'

 tilefile = trim(data_dir_input_grid) // "/" // trim(sfc_files_input_grid(1))
 print*,"- READ GRID DIMENSIONS FROM: ", trim(tilefile)
 error=nf90_open(trim(tilefile),nf90_nowrite,ncid)
 call netcdf_err(error, 'opening: '//trim(tilefile) )

 error=nf90_inq_dimid(ncid, 'xaxis_1', id_dim)
 call netcdf_err(error, 'reading xaxis_1 id' )
 error=nf90_inquire_dimension(ncid,id_dim,len=i_input)
 call netcdf_err(error, 'reading xaxis_1 value' )

 error=nf90_inq_dimid(ncid, 'yaxis_1', id_dim)
 call netcdf_err(error, 'reading yaxis_1 id' )
 error=nf90_inquire_dimension(ncid,id_dim,len=j_input)
 call netcdf_err(error, 'reading yaxis_1 value' )

 print*,'- INPUT GRID DIMENSIONS ARE: ', i_input, j_input

 if (localpet == 0) then
   allocate(data_one_tile(i_input,j_input))
   allocate(mask(i_input,j_input))
   allocate(snow_liq(i_input,j_input))
   allocate(snow_d(i_input,j_input))
   allocate(data_one_tile_3d(i_input,j_input,lsoil_input))
   allocate(soilm(i_input,j_input,lsoil_input))
 else
   allocate(data_one_tile(0,0))
   allocate(mask(0,0))
   allocate(snow_liq(0,0))
   allocate(snow_d(0,0))
   allocate(data_one_tile_3d(0,0,0))
   allocate(soilm(0,0,0))
 endif


 TILE_LOOP : do tile = 1, 6

! Soil type

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('stype', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input soil type for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID SOIL TYPE."
  call ESMF_FieldScatter(soil_type_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! snow liq equiv

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('sheleg', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=snow_liq)
    print*,'input snow liq equiv for tile ',tile,maxval(snow_liq),  &
                                                 minval(snow_liq)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID SNOW LIQ EQUIV."
  call ESMF_FieldScatter(snow_liq_equiv_input_grid, snow_liq, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! snow depth

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('snwdph', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=snow_d)
    print*,'input snow depth for tile ',tile, maxval(snow_d), minval(snow_d)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID SNOW DEPTH."
  call ESMF_FieldScatter(snow_depth_input_grid, snow_d, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! soil temp

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('stc', tile, i_input, j_input, &
                                   lsoil_input, sfcdata_3d=data_one_tile_3d)
!   data_one_tile_3d=444.
    print*,'input soil temp for tile ',tile, maxval(data_one_tile_3d), minval(data_one_tile_3d)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID SOIL TEMP."
  call ESMF_FieldScatter(soil_temp_input_grid, data_one_tile_3d, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! liq soil m

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('slc', tile, i_input, j_input, &
                                   lsoil_input, sfcdata_3d=data_one_tile_3d)
!   data_one_tile_3d=.777
    print*,'input soilm liq for tile ',tile, maxval(data_one_tile_3d), minval(data_one_tile_3d)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID SOILM LIQ."
  call ESMF_FieldScatter(soilm_liq_input_grid, data_one_tile_3d, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! tot soil m

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('smc', tile, i_input, j_input, &
                                   lsoil_input, sfcdata_3d=soilm)
!   soilm=.888
    print*,'input soilm tot for tile ',tile, maxval(soilm), minval(soilm)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID SOILM TOT."
  call ESMF_FieldScatter(soilm_tot_input_grid, soilm, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! land mask

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('slmsk', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=mask)
    print*,'input slmsk for tile ',tile, maxval(mask), minval(mask)
  endif

  if (localpet == 0) then
    data_one_tile = 1.0  ! gdas point to process
    do j = 1, j_input
    do i = 1, i_input
      if (nint(mask(i,j)) == 0) then
        data_one_tile(i,j) = 0.0  ! don't process water
      endif
      if (nint(mask(i,j)) == 2) then
        data_one_tile(i,j) = 0.0  ! don't process sea ice
      endif
      if (snow_d(i,j) > 0.0) then
        data_one_tile(i,j) = 0.0  ! don't process snow
      endif
      if (snow_liq(i,j) > 0.0) then
        data_one_tile(i,j) = 0.0  ! don't process snow
      endif
      if (soilm(i,j,1) > 0.9) then
        data_one_tile(i,j) = 0.0  ! don't process land ice
      endif
    enddo
    enddo
  endif

  print*,"- CALL FieldScatter FOR INPUT LANDSEA MASK."
  call ESMF_FieldScatter(landsea_mask_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

 enddo TILE_LOOP

 deallocate (data_one_tile, data_one_tile_3d, mask, snow_liq, snow_d, soilm)

 end subroutine read_input_sfc_restart_file

 SUBROUTINE READ_FV3_GRID_DATA_NETCDF(FIELD,TILE_NUM,IMO,JMO,LMO, &
                                      SFCDATA, SFCDATA_3D)

 use netcdf 

 IMPLICIT NONE

 CHARACTER(LEN=*),INTENT(IN)      :: FIELD

 INTEGER, INTENT(IN)   :: IMO, JMO, LMO, TILE_NUM

 REAL(ESMF_KIND_R8), INTENT(OUT), OPTIONAL     :: SFCDATA(IMO,JMO)
 REAL(ESMF_KIND_R8), INTENT(OUT), OPTIONAL     :: SFCDATA_3D(IMO,JMO,LMO)

 CHARACTER(LEN=256)    :: TILEFILE

 INTEGER               :: ERROR, NCID, ID_VAR

 TILEFILE = TRIM(DATA_DIR_INPUT_GRID) // "/" // TRIM(SFC_FILES_INPUT_GRID(TILE_NUM))
 PRINT*,'WILL READ ',TRIM(FIELD), ' FROM: ', TRIM(TILEFILE)

 ERROR=NF90_OPEN(TRIM(TILEFILE),NF90_NOWRITE,NCID)
 CALL NETCDF_ERR(ERROR, 'OPENING: '//TRIM(TILEFILE) )

 ERROR=NF90_INQ_VARID(NCID, FIELD, ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING FIELD ID' )

 IF (PRESENT(SFCDATA_3D)) THEN
   ERROR=NF90_GET_VAR(NCID, ID_VAR, SFCDATA_3D)
   CALL NETCDF_ERR(ERROR, 'READING FIELD' )
 ELSE
   ERROR=NF90_GET_VAR(NCID, ID_VAR, SFCDATA)
   CALL NETCDF_ERR(ERROR, 'READING FIELD' )
 ENDIF

 ERROR = NF90_CLOSE(NCID)

 END SUBROUTINE READ_FV3_GRID_DATA_NETCDF

 end module input_data
