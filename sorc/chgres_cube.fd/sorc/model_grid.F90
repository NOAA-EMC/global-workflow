 module model_grid

!--------------------------------------------------------------------------
! Module model_grid
!
! Abstract: Specify input and target model grids
!
! Public Subroutines:
! -------------------
! define_target_grid               Setup the esmf grid object for the
!                                  target grid.
! define_input_grid                Setup the esmf grid object for the
!                                  input grid.
! cleanup_input_target_grid_data   Deallocate all esmf grid objects.
!
! Public variables:
! -----------------
! i/j_input                        i/j dimension of each cube of the
!                                  input grid.
! ip1/jp1_input                    i/j dimension plus 1 of input grid.
! i/j_target                       i/j dimension of each cube or of
!                                  a nest, target grid.
! ip1/jp1_target                   i/j dimension plus 1 of input grid.
! input_grid                       input grid esmf grid object
! landmask_target_grid             land mask target grid - '1' land;
!                                  '0' non-land
! latitude_input_grid              latitude of grid center, input grid
! latitude_target_grid             latitude of grid center, target grid
! latitude_s_input_grid            latitude of 'south' edge of grid
!                                  box, input grid
! latitude_s_target_grid           latitude of 'south' edge of grid
!                                  box, target grid
! latitude_w_input_grid            latitude of 'west' edge of grid
!                                  box, input grid
! latitude_w_target_grid           latitude of 'west' edge of grid
!                                  box, target grid
! longitude_input_grid             longitude of grid center, input grid
! longitude_target_grid            longitude of grid center, target grid
! longitude_s_input_grid           longitude of 'south' edge of grid
!                                  box, input grid
! longitude_s_target_grid          longitude of 'south' edge of grid
!                                  box, target grid
! longitude_w_input_grid           longitude of 'west' edge of grid
!                                  box, input grid
! longitude_w_target_grid          longitude of 'west' edge of grid
!                                  box, target grid
! lsoil_target                     Number of soil layers, target grid.
! num_tiles_input_grid             Number of tiles, input grid
! num_tiles_target_grid            Number of tiles, target grid
! seamask_target_grid              sea mask target grid - '1' non-land;
!                                  '0' land
! target_grid                      target grid esmf grid object.
! terrain_target_grid              terrain height target grid
! tiles_target_grid                Tile names of target grid.
!
!--------------------------------------------------------------------------

 use esmf

 implicit none

 private

 character(len=5), allocatable, public  :: tiles_target_grid(:)

 integer, parameter, public             :: lsoil_target = 4 ! # soil layers
 integer, public                        :: i_input, j_input
 integer, public                        :: ip1_input, jp1_input
 integer, public                        :: i_target, j_target
 integer, public                        :: ip1_target, jp1_target
 integer, public                        :: num_tiles_input_grid
 integer, public                        :: num_tiles_target_grid

 type(esmf_grid),  public               :: input_grid
 type(esmf_grid),  public               :: target_grid

 type(esmf_field),  public              :: latitude_input_grid
 type(esmf_field),  public              :: longitude_input_grid
 type(esmf_field),  public              :: latitude_s_input_grid
 type(esmf_field),  public              :: longitude_s_input_grid
 type(esmf_field),  public              :: latitude_w_input_grid
 type(esmf_field),  public              :: longitude_w_input_grid

 type(esmf_field),  public              :: landmask_target_grid
 type(esmf_field),  public              :: latitude_target_grid
 type(esmf_field),  public              :: latitude_s_target_grid
 type(esmf_field),  public              :: latitude_w_target_grid
 type(esmf_field),  public              :: longitude_target_grid
 type(esmf_field),  public              :: longitude_s_target_grid
 type(esmf_field),  public              :: longitude_w_target_grid
 type(esmf_field),  public              :: seamask_target_grid
 type(esmf_field),  public              :: terrain_target_grid

 public :: define_target_grid
 public :: define_input_grid
 public :: cleanup_input_target_grid_data

 contains

!--------------------------------------------------------------------------
! Set up the esmf grid object for the input grid.  If the input
! source is tiled fv3 restart or history data, the grid is created
! by reading the mosaic and grid files.  If the input source is
! fv3 global gaussian nemsio, spectral gfs global gaussian nemsio, or
! spectral gfs global gaussian sigio/sfcio, the grid is setup by  
! computing lat/lons using the sp library.
!--------------------------------------------------------------------------

 subroutine define_input_grid(localpet, npets)

 use program_setup, only       : input_type

 implicit none

 integer, intent(in)          :: localpet, npets

 if (trim(input_type) == "gaussian" .or. &
     trim(input_type) == "gfs_gaussian" .or. &
     trim(input_type) == "gfs_spectral") then
   call define_input_grid_gaussian(localpet, npets)
 else
   call define_input_grid_mosaic(localpet, npets)
 endif

 end subroutine define_input_grid

!--------------------------------------------------------------------------
! Define grid object for input data on global gaussian grids.
! Recognized file formats: 
!
!  - fv3gfs nemsio
!  - spectral gfs nemsio (starting July 19, 2017)
!  - spectral gfs sigio  (prior to July 19, 2017)
!  - spectral gfs sfcio  (prior to July 19, 2017)
!--------------------------------------------------------------------------

 subroutine define_input_grid_gaussian(localpet, npets)

 use nemsio_module

 use program_setup, only       : data_dir_input_grid, &
                                 atm_files_input_grid, &
                                 sfc_files_input_grid, &
                                 input_type, &
                                 convert_atm, convert_sfc

 use sfcio_module
 use sigio_module

 implicit none

 integer, intent(in)              :: localpet, npets

 character(len=250)               :: the_file

 integer                          :: i, j, rc, clb(2), cub(2)
 integer(sfcio_intkind)           :: rc2
 integer(sigio_intkind)           :: rc3

 real(esmf_kind_r8), allocatable  :: latitude(:,:)
 real(esmf_kind_r8), allocatable  :: longitude(:,:)
 real(esmf_kind_r8), pointer      :: lat_src_ptr(:,:)
 real(esmf_kind_r8), pointer      :: lon_src_ptr(:,:)
 real(esmf_kind_r8)               :: deltalon
 real(esmf_kind_r8), allocatable  :: slat(:), wlat(:)

 type(nemsio_gfile)               :: gfile
 type(esmf_polekind_flag)         :: polekindflag(2)
 type(sfcio_head)                 :: sfchead
 type(sigio_head)                 :: sighead

 print*,"- DEFINE INPUT GRID OBJECT FOR GAUSSIAN DATA."

 num_tiles_input_grid = 1

 if (convert_sfc) then
   the_file=trim(data_dir_input_grid) // "/" // trim(sfc_files_input_grid(1))
 elseif (convert_atm) then
   the_file=trim(data_dir_input_grid) // "/" // trim(atm_files_input_grid(1))
 endif

 if (trim(input_type) == "gfs_spectral") then  ! sigio/sfcio format, used by
                                               ! spectral gfs prior to 7/19/2017.

   if (convert_sfc) then   ! sfcio format
     print*,"- OPEN AND READ ", trim(the_file)
     call sfcio_sropen(21, trim(the_file), rc2)
     if (rc2 /= 0) call error_handler("OPENING FILE", rc2)
     call sfcio_srhead(21, sfchead, rc2)
     if (rc2 /= 0) call error_handler("READING FILE", rc2)
     call sfcio_sclose(21, rc2)
     i_input = sfchead%lonb
     j_input = sfchead%latb
   elseif (convert_atm) then ! sigio format
     print*,"- OPEN AND READ ", trim(the_file)
     call sigio_sropen(21, trim(the_file), rc3)
     if (rc3 /= 0) call error_handler("OPENING FILE", rc3)
     call sigio_srhead(21, sighead, rc3)
     if (rc3 /= 0) call error_handler("READING FILE", rc3)
     call sigio_sclose(21, rc3)
     i_input = sighead%lonb
     j_input = sighead%latb
   endif

 else ! nemsio format

   call nemsio_init(iret=rc)

   print*,"- OPEN AND READ ", trim(the_file)
   call nemsio_open(gfile, the_file, "read", iret=rc)
   if (rc /= 0) call error_handler("OPENING FILE", rc)

   call nemsio_getfilehead(gfile, iret=rc, dimx=i_input, dimy=j_input)
   if (rc /= 0) call error_handler("READING FILE", rc)

   call nemsio_close(gfile)
 
 endif

 ip1_input = i_input + 1
 jp1_input = j_input + 1

 polekindflag(1:2) = ESMF_POLEKIND_MONOPOLE

 print*,"- CALL GridCreate1PeriDim FOR INPUT GRID."
 input_grid = ESMF_GridCreate1PeriDim(minIndex=(/1,1/), &
                                    maxIndex=(/i_input,j_input/), &
                                    polekindflag=polekindflag, &
                                    periodicDim=1, &
                                    poleDim=2,  &
                                    coordSys=ESMF_COORDSYS_SPH_DEG, &
                                    regDecomp=(/1,npets/),  &
                                    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
   call error_handler("IN GridCreate1PeriDim", rc)

 print*,"- CALL FieldCreate FOR INPUT GRID LATITUDE."
 latitude_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="input_grid_latitude", rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
   call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT GRID LONGITUDE."
 longitude_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="input_grid_longitude", rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
   call error_handler("IN FieldCreate", rc)

 allocate(longitude(i_input,j_input))
 allocate(latitude(i_input,j_input))

 deltalon = 360.0_esmf_kind_r8 / real(i_input,kind=esmf_kind_r8)
 do i = 1, i_input
   longitude(i,:) = real((i-1),kind=esmf_kind_r8) * deltalon
 enddo

 allocate(slat(j_input))
 allocate(wlat(j_input))
 call splat(4, j_input, slat, wlat)

 do i = 1, j_input
   latitude(:,i) = 90.0_esmf_kind_r8 - (acos(slat(i))* 180.0_esmf_kind_r8 / &
                  (4.0_esmf_kind_r8*atan(1.0_esmf_kind_r8)))
 enddo

 deallocate(slat, wlat)

 print*,"- CALL FieldScatter FOR INPUT GRID LONGITUDE."
 call ESMF_FieldScatter(longitude_input_grid, longitude, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

 print*,"- CALL FieldScatter FOR INPUT GRID LATITUDE."
 call ESMF_FieldScatter(latitude_input_grid, latitude, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

 print*,"- CALL GridAddCoord FOR INPUT GRID."
 call ESMF_GridAddCoord(input_grid, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddCoord", rc)

 print*,"- CALL GridGetCoord FOR INPUT GRID X-COORD."
 nullify(lon_src_ptr)
 call ESMF_GridGetCoord(input_grid, &
                        staggerLoc=ESMF_STAGGERLOC_CENTER, &
                        coordDim=1, &
                        farrayPtr=lon_src_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetCoord", rc)

 print*,"- CALL GridGetCoord FOR INPUT GRID Y-COORD."
 nullify(lat_src_ptr)
 call ESMF_GridGetCoord(input_grid, &
                        staggerLoc=ESMF_STAGGERLOC_CENTER, &
                        coordDim=2, &
                        computationalLBound=clb, &
                        computationalUBound=cub, &
                        farrayPtr=lat_src_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetCoord", rc)

 do j = clb(2), cub(2)
   do i = clb(1), cub(1)
     lon_src_ptr(i,j) = longitude(i,j)
     if (lon_src_ptr(i,j) > 360.0_esmf_kind_r8) lon_src_ptr(i,j) = lon_src_ptr(i,j) - 360.0_esmf_kind_r8
     lat_src_ptr(i,j) = latitude(i,j)
   enddo
 enddo

 print*,"- CALL GridAddCoord FOR INPUT GRID."
 call ESMF_GridAddCoord(input_grid, &
                        staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddCoord", rc)

 print*,"- CALL GridGetCoord FOR INPUT GRID X-COORD."
 nullify(lon_src_ptr)
 call ESMF_GridGetCoord(input_grid, &
                        staggerLoc=ESMF_STAGGERLOC_CORNER, &
                        coordDim=1, &
                        farrayPtr=lon_src_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetCoord", rc)

 print*,"- CALL GridGetCoord FOR INPUT GRID Y-COORD."
 nullify(lat_src_ptr)
 call ESMF_GridGetCoord(input_grid, &
                        staggerLoc=ESMF_STAGGERLOC_CORNER, &
                        coordDim=2, &
                        computationalLBound=clb, &
                        computationalUBound=cub, &
                        farrayPtr=lat_src_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetCoord", rc)

 print*,'bounds for corners ',localpet,clb(1),cub(1),clb(2),cub(2)

 do j = clb(2), cub(2)
   do i = clb(1), cub(1)
     lon_src_ptr(i,j) = longitude(i,1) - (0.5_esmf_kind_r8*deltalon)
     if (lon_src_ptr(i,j) > 360.0_esmf_kind_r8) lon_src_ptr(i,j) = lon_src_ptr(i,j) - 360.0_esmf_kind_r8
     if (j == 1) then 
       lat_src_ptr(i,j) = 90.0_esmf_kind_r8
       cycle
     endif
     if (j == jp1_input) then
       lat_src_ptr(i,j) = -90.0_esmf_kind_r8
       cycle
     endif
     lat_src_ptr(i,j) = 0.5_esmf_kind_r8 * (latitude(i,j-1)+ latitude(i,j))
   enddo
 enddo

 deallocate(latitude,longitude)

 end subroutine define_input_grid_gaussian

 subroutine define_input_grid_mosaic(localpet, npets)

 use netcdf
 use program_setup, only       : mosaic_file_input_grid,  &
                                 orog_dir_input_grid, &
                                 orog_files_input_grid

 implicit none

 character(len=500)           :: the_file

 integer, intent(in)          :: localpet, npets

 integer                      :: id_tiles, id_dim, tile
 integer                      :: extra, error, ncid
 integer, allocatable         :: decomptile(:,:)

 integer(esmf_kind_i8), allocatable    :: landmask_one_tile(:,:)

 real(esmf_kind_r8), allocatable       :: latitude_one_tile(:,:)
 real(esmf_kind_r8), allocatable       :: latitude_s_one_tile(:,:)
 real(esmf_kind_r8), allocatable       :: latitude_w_one_tile(:,:)
 real(esmf_kind_r8), allocatable       :: longitude_one_tile(:,:)
 real(esmf_kind_r8), allocatable       :: longitude_s_one_tile(:,:)
 real(esmf_kind_r8), allocatable       :: longitude_w_one_tile(:,:)

 print*,'- OPEN INPUT GRID MOSAIC FILE: ',trim(mosaic_file_input_grid)
 error=nf90_open(trim(mosaic_file_input_grid),nf90_nowrite,ncid)
 call netcdf_err(error, 'opening grid mosaic file')

 print*,"- READ NUMBER OF TILES"
 error=nf90_inq_dimid(ncid, 'ntiles', id_tiles)
 call netcdf_err(error, 'reading ntiles id')
 error=nf90_inquire_dimension(ncid,id_tiles,len=num_tiles_input_grid)
 call netcdf_err(error, 'reading ntiles')

 error = nf90_close(ncid)

 print*,'- NUMBER OF TILES, INPUT MODEL GRID IS ', num_tiles_input_grid

 if (mod(npets,num_tiles_input_grid) /= 0) then
   call error_handler("MUST RUN WITH A TASK COUNT THAT IS A MULTIPLE OF 6.", 1)
 endif

!-----------------------------------------------------------------------
! Create ESMF grid object for the model grid.
!-----------------------------------------------------------------------

 extra = npets / num_tiles_input_grid

 allocate(decomptile(2,num_tiles_input_grid))

 do tile = 1, num_tiles_input_grid
   decomptile(:,tile)=(/1,extra/)
 enddo

 print*,"- CALL GridCreateMosaic FOR INPUT MODEL GRID"
 input_grid = ESMF_GridCreateMosaic(filename=trim(mosaic_file_input_grid), &
                                  regDecompPTile=decomptile, &
                                  staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER, &
                                                   ESMF_STAGGERLOC_EDGE1, ESMF_STAGGERLOC_EDGE2/), &
                                  indexflag=ESMF_INDEX_GLOBAL, &
                                  tileFilePath=trim(orog_dir_input_grid), &
                                  rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridCreateMosaic", error)

!-----------------------------------------------------------------------
! Read the mask and lat/lons.
!-----------------------------------------------------------------------

 print*,"- CALL FieldCreate FOR INPUT GRID LATITUDE."
 latitude_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="input_grid_latitude", &
                                   rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR INPUT GRID LONGITUDE."
 longitude_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="input_grid_longitude", &
                                   rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR INPUT GRID LATITUDE_S."
 latitude_s_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_EDGE2, &
                                   name="input_grid_latitude_s", &
                                   rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR INPUT GRID LONGITUDE_S."
 longitude_s_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_EDGE2, &
                                   name="input_grid_longitude_s", &
                                   rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR INPUT GRID LATITUDE_W."
 latitude_w_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_EDGE1, &
                                   name="input_grid_latitude_w", &
                                   rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR INPUT GRID LONGITUDE_W."
 longitude_w_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_EDGE1, &
                                   name="input_grid_longitude_w", &
                                   rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 the_file = trim(orog_dir_input_grid) // trim(orog_files_input_grid(1))

 print*,'- OPEN FIRST INPUT GRID OROGRAPHY FILE: ',trim(the_file)
 error=nf90_open(trim(the_file),nf90_nowrite,ncid)
 call netcdf_err(error, 'opening ororgraphy file')
 print*,"- READ GRID DIMENSIONS"
 error=nf90_inq_dimid(ncid, 'lon', id_dim)
 call netcdf_err(error, 'reading lon id')
 error=nf90_inquire_dimension(ncid,id_dim,len=i_input)
 call netcdf_err(error, 'reading lon')
 error=nf90_inq_dimid(ncid, 'lat', id_dim)
 call netcdf_err(error, 'reading lat id')
 error=nf90_inquire_dimension(ncid,id_dim,len=j_input)
 call netcdf_err(error, 'reading lat')
 error = nf90_close(ncid)

 print*,"- I/J DIMENSIONS OF THE INPUT GRID TILES ", i_input, j_input

 ip1_input = i_input + 1
 jp1_input = j_input + 1

 if (localpet == 0) then
   allocate(longitude_one_tile(i_input,j_input))
   allocate(longitude_s_one_tile(i_input,jp1_input))
   allocate(longitude_w_one_tile(ip1_input,j_input))
   allocate(latitude_one_tile(i_input,j_input))
   allocate(latitude_s_one_tile(i_input,jp1_input))
   allocate(latitude_w_one_tile(ip1_input,j_input))
   allocate(landmask_one_tile(i_input,j_input))
 else
   allocate(longitude_one_tile(0,0))
   allocate(longitude_s_one_tile(0,0))
   allocate(longitude_w_one_tile(0,0))
   allocate(latitude_one_tile(0,0))
   allocate(latitude_s_one_tile(0,0))
   allocate(latitude_w_one_tile(0,0))
   allocate(landmask_one_tile(0,0))
 endif

 do tile = 1, num_tiles_input_grid
   if (localpet == 0) then
     call get_model_latlons(mosaic_file_input_grid, orog_dir_input_grid, num_tiles_input_grid, tile, &
                            i_input, j_input, ip1_input, jp1_input, latitude_one_tile, &
                            latitude_s_one_tile, latitude_w_one_tile, longitude_one_tile, &
                            longitude_s_one_tile, longitude_w_one_tile)
   endif
   print*,"- CALL FieldScatter FOR INPUT GRID LATITUDE. TILE IS: ", tile
   call ESMF_FieldScatter(latitude_input_grid, latitude_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR INPUT GRID LONGITUDE. TILE IS: ", tile
   call ESMF_FieldScatter(longitude_input_grid, longitude_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR INPUT GRID LATITUDE_S. TILE IS: ", tile
   call ESMF_FieldScatter(latitude_s_input_grid, latitude_s_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR INPUT GRID LONGITUDE_S. TILE IS: ", tile
   call ESMF_FieldScatter(longitude_s_input_grid, longitude_s_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR INPUT GRID LATITUDE_W. TILE IS: ", tile
   call ESMF_FieldScatter(latitude_w_input_grid, latitude_w_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR INPUT GRID LONGITUDE_W. TILE IS: ", tile
   call ESMF_FieldScatter(longitude_w_input_grid, longitude_w_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", error)
 enddo

 deallocate(longitude_one_tile)
 deallocate(longitude_s_one_tile)
 deallocate(longitude_w_one_tile)
 deallocate(latitude_one_tile)
 deallocate(latitude_s_one_tile)
 deallocate(latitude_w_one_tile)
 deallocate(landmask_one_tile)

 end subroutine define_input_grid_mosaic

 subroutine define_target_grid(localpet, npets)

 use netcdf
 use program_setup, only       : mosaic_file_target_grid, &
                                 orog_dir_target_grid,    &
                                 orog_files_target_grid

 implicit none

 integer, intent(in)                   :: localpet, npets

 character(len=500)                    :: the_file

 integer                               :: error, ncid, extra
 integer                               :: id_tiles
 integer                               :: id_dim, id_grid_tiles
 integer                               :: tile
 integer, allocatable                  :: decomptile(:,:)
 integer(esmf_kind_i8), allocatable    :: landmask_one_tile(:,:)
 integer(esmf_kind_i8), allocatable    :: seamask_one_tile(:,:)
 
 real(esmf_kind_r8), allocatable       :: latitude_one_tile(:,:)
 real(esmf_kind_r8), allocatable       :: latitude_s_one_tile(:,:)
 real(esmf_kind_r8), allocatable       :: latitude_w_one_tile(:,:)
 real(esmf_kind_r8), allocatable       :: longitude_one_tile(:,:)
 real(esmf_kind_r8), allocatable       :: longitude_s_one_tile(:,:)
 real(esmf_kind_r8), allocatable       :: longitude_w_one_tile(:,:)
 real(esmf_kind_r8), allocatable       :: terrain_one_tile(:,:)

 print*,'- OPEN TARGET GRID MOSAIC FILE: ',trim(mosaic_file_target_grid)
 error=nf90_open(trim(mosaic_file_target_grid),nf90_nowrite,ncid)
 call netcdf_err(error, 'opening grid mosaic file')

 print*,"- READ NUMBER OF TILES"
 error=nf90_inq_dimid(ncid, 'ntiles', id_tiles)
 call netcdf_err(error, 'reading ntile id')
 error=nf90_inquire_dimension(ncid,id_tiles,len=num_tiles_target_grid)
 call netcdf_err(error, 'reading ntiles')
 error=nf90_inq_varid(ncid, 'gridtiles', id_grid_tiles)
 call netcdf_err(error, 'reading gridtiles id')
 allocate(tiles_target_grid(num_tiles_target_grid))
 tiles_target_grid="NULL"
 print*,"- READ TILE NAMES"
 error=nf90_get_var(ncid, id_grid_tiles, tiles_target_grid)
 call netcdf_err(error, 'reading gridtiles')

 error = nf90_close(ncid)

 print*,'- NUMBER OF TILES, TARGET MODEL GRID IS ', num_tiles_target_grid

 if (mod(npets,num_tiles_target_grid) /= 0) then
   call error_handler("MUST RUN WITH TASK COUNT THAT IS A MULTIPLE OF # OF TILES.", 1)
 endif

!-----------------------------------------------------------------------
! Get the model grid specs and land mask from the orography files.
!-----------------------------------------------------------------------

 the_file = trim(orog_dir_target_grid) // trim(orog_files_target_grid(1))

 print*,'- OPEN FIRST TARGET GRID OROGRAPHY FILE: ',trim(the_file)
 error=nf90_open(trim(the_file),nf90_nowrite,ncid)
 call netcdf_err(error, 'opening orography file')
 print*,"- READ GRID DIMENSIONS"
 error=nf90_inq_dimid(ncid, 'lon', id_dim)
 call netcdf_err(error, 'reading lon id')
 error=nf90_inquire_dimension(ncid,id_dim,len=i_target)
 call netcdf_err(error, 'reading lon')
 error=nf90_inq_dimid(ncid, 'lat', id_dim)
 call netcdf_err(error, 'reading lat id')
 error=nf90_inquire_dimension(ncid,id_dim,len=j_target)
 call netcdf_err(error, 'reading lat')
 error = nf90_close(ncid)

 print*,"- I/J DIMENSIONS OF THE TARGET GRID TILES ", i_target, j_target

 ip1_target = i_target + 1
 jp1_target = j_target + 1

!-----------------------------------------------------------------------
! Create ESMF grid object for the model grid.
!-----------------------------------------------------------------------

 extra = npets / num_tiles_target_grid

 allocate(decomptile(2,num_tiles_target_grid))

 do tile = 1, num_tiles_target_grid
   decomptile(:,tile)=(/1,extra/)
 enddo

 print*,"- CALL GridCreateMosaic FOR TARGET GRID"
 target_grid = ESMF_GridCreateMosaic(filename=trim(mosaic_file_target_grid), &
                                  regDecompPTile=decomptile, &
                                  staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER, &
                                                   ESMF_STAGGERLOC_EDGE1, ESMF_STAGGERLOC_EDGE2/), &
                                  indexflag=ESMF_INDEX_GLOBAL, &
                                  tileFilePath=trim(orog_dir_target_grid), rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridCreateMosaic", error)

!-----------------------------------------------------------------------
! Set target model landmask (1 - land, 0 - not land) and 
! seamask (1 - non-land, 0 -land).  Read lat/lon on target grid.
!-----------------------------------------------------------------------

 print*,"- CALL FieldCreate FOR TARGET GRID LANDMASK."
 landmask_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_I8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="target_grid_landmask", rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR TARGET GRID SEAMASK."
 seamask_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_I8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="target_grid_seamask", rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR TARGET GRID LATITUDE."
 latitude_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="target_grid_latitude", rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR TARGET GRID LATITUDE_S."
 latitude_s_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_EDGE2, &
                                   name="target_grid_latitude_s", rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR TARGET GRID LATITUDE_W."
 latitude_w_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_EDGE1, &
                                   name="target_grid_latitude_w", &
                                   rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR TARGET GRID LONGITUDE."
 longitude_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="target_grid_longitude", &
                                   rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR TARGET GRID LONGITUDE_S."
 longitude_s_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_EDGE2, &
                                   name="target_grid_longitude_s", &
                                   rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR TARGET GRID LONGITUDE_W."
 longitude_w_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_EDGE1, &
                                   name="target_grid_longitude_w", &
                                   rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 print*,"- CALL FieldCreate FOR TARGET GRID TERRAIN."
 terrain_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="target_grid_terrain", &
                                   rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", error)

 if (localpet == 0) then
   allocate(landmask_one_tile(i_target,j_target))
   allocate(seamask_one_tile(i_target,j_target))
   allocate(latitude_one_tile(i_target,j_target))
   allocate(latitude_s_one_tile(i_target,jp1_target))
   allocate(latitude_w_one_tile(ip1_target,j_target))
   allocate(longitude_one_tile(i_target,j_target))
   allocate(longitude_s_one_tile(i_target,jp1_target))
   allocate(longitude_w_one_tile(ip1_target,j_target))
   allocate(terrain_one_tile(i_target,j_target))
 else
   allocate(landmask_one_tile(0,0))
   allocate(seamask_one_tile(0,0))
   allocate(longitude_one_tile(0,0))
   allocate(longitude_s_one_tile(0,0))
   allocate(longitude_w_one_tile(0,0))
   allocate(latitude_one_tile(0,0))
   allocate(latitude_s_one_tile(0,0))
   allocate(latitude_w_one_tile(0,0))
   allocate(terrain_one_tile(0,0))
 endif

 do tile = 1, num_tiles_target_grid
   if (localpet == 0) then
     the_file = trim(orog_dir_target_grid) // trim(orog_files_target_grid(tile))
     call get_model_mask_terrain(trim(the_file), i_target, j_target, landmask_one_tile, &
                                 terrain_one_tile)
     seamask_one_tile = 0
     where(landmask_one_tile == 0) seamask_one_tile = 1
     call get_model_latlons(mosaic_file_target_grid, orog_dir_target_grid, num_tiles_target_grid, tile, &
                            i_target, j_target, ip1_target, jp1_target, latitude_one_tile, &
                            latitude_s_one_tile, latitude_w_one_tile, longitude_one_tile, &
                            longitude_s_one_tile, longitude_w_one_tile)
   endif
   print*,"- CALL FieldScatter FOR TARGET GRID LANDMASK. TILE IS: ", tile
   call ESMF_FieldScatter(landmask_target_grid, landmask_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR TARGET GRID SEAMASK. TILE IS: ", tile
   call ESMF_FieldScatter(seamask_target_grid, seamask_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR TARGET GRID LONGITUDE. TILE IS: ", tile
   call ESMF_FieldScatter(longitude_target_grid, longitude_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR TARGET GRID LONGITUDE_S. TILE IS: ", tile
   call ESMF_FieldScatter(longitude_s_target_grid, longitude_s_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR TARGET GRID LONGITUDE_W. TILE IS: ", tile
   call ESMF_FieldScatter(longitude_w_target_grid, longitude_w_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR TARGET GRID LATITUDE. TILE IS: ", tile
   call ESMF_FieldScatter(latitude_target_grid, latitude_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR TARGET GRID LATITUDE_S. TILE IS: ", tile
   call ESMF_FieldScatter(latitude_s_target_grid, latitude_s_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR TARGET GRID LATITUDE_W. TILE IS: ", tile
   call ESMF_FieldScatter(latitude_w_target_grid, latitude_w_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
   print*,"- CALL FieldScatter FOR TARGET GRID TERRAIN. TILE IS: ", tile
   call ESMF_FieldScatter(terrain_target_grid, terrain_one_tile, rootpet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldScatter", error)
 enddo

 deallocate(landmask_one_tile)
 deallocate(seamask_one_tile)
 deallocate(longitude_one_tile)
 deallocate(longitude_s_one_tile)
 deallocate(longitude_w_one_tile)
 deallocate(latitude_one_tile)
 deallocate(latitude_s_one_tile)
 deallocate(latitude_w_one_tile)
 deallocate(terrain_one_tile)

 end subroutine define_target_grid

!-----------------------------------------------------------------------
! Read model lat/lons for a single tile from the "grid" file.
!-----------------------------------------------------------------------

 subroutine get_model_latlons(mosaic_file, orog_dir, num_tiles, tile, &
                              i_tile, j_tile, ip1_tile, jp1_tile,  &
                              latitude, latitude_s, latitude_w, &
                              longitude, longitude_s, longitude_w)

 use netcdf

 implicit none

 character(len=*), intent(in)      :: mosaic_file, orog_dir

 integer, intent(in)               :: num_tiles, tile
 integer, intent(in)               :: i_tile, j_tile
 integer, intent(in)               :: ip1_tile, jp1_tile

 real(esmf_kind_r8), intent(out)   :: latitude(i_tile, j_tile)
 real(esmf_kind_r8), intent(out)   :: latitude_s(i_tile, jp1_tile)
 real(esmf_kind_r8), intent(out)   :: latitude_w(ip1_tile, j_tile)
 real(esmf_kind_r8), intent(out)   :: longitude(i_tile, j_tile)
 real(esmf_kind_r8), intent(out)   :: longitude_s(i_tile, jp1_tile)
 real(esmf_kind_r8), intent(out)   :: longitude_w(ip1_tile, j_tile)

 character(len=25)                 :: grid_files(num_tiles)
 character(len=255)                :: grid_file

 integer                           :: error, id_var, ncid
 integer                           :: id_dim, nxp, nyp, i, j, ii, jj

 real(esmf_kind_r8), allocatable   :: tmpvar(:,:)

 print*,"- READ MODEL GRID FILE"

 print*,'- OPEN MOSAIC FILE: ', trim(mosaic_file)
 error=nf90_open(trim(mosaic_file), nf90_nowrite, ncid)
 call netcdf_err(error, 'opening mosaic file')

 print*,"- READ GRID FILE NAMES"
 error=nf90_inq_varid(ncid, 'gridfiles', id_var)
 call netcdf_err(error, 'reading gridfiles id')
 error=nf90_get_var(ncid, id_var, grid_files)
 call netcdf_err(error, 'reading gridfiles')

 error = nf90_close(ncid)

 grid_file = trim(orog_dir) // trim(grid_files(tile))

 print*,'- OPEN GRID FILE: ', trim(grid_file)
 error=nf90_open(trim(grid_file), nf90_nowrite, ncid)
 call netcdf_err(error, 'opening grid file')

 print*,'- READ NXP ID'
 error=nf90_inq_dimid(ncid, 'nxp', id_dim)
 call netcdf_err(error, 'reading nxp id')

 print*,'- READ NXP'
 error=nf90_inquire_dimension(ncid,id_dim,len=nxp)
 call netcdf_err(error, 'reading nxp')

 print*,'- READ NYP ID'
 error=nf90_inq_dimid(ncid, 'nyp', id_dim)
 call netcdf_err(error, 'reading nyp id')

 print*,'- READ NYP'
 error=nf90_inquire_dimension(ncid,id_dim,len=nyp)
 call netcdf_err(error, 'reading nyp')

 if ((nxp/2 /= i_tile) .or. (nyp/2 /= j_tile)) then
   call error_handler("DIMENSION MISMATCH IN GRID FILE.", 1)
 endif

 allocate(tmpvar(nxp,nyp))

 print*,'- READ LONGITUDE ID'
 error=nf90_inq_varid(ncid, 'x', id_var)
 call netcdf_err(error, 'reading longitude id')

 print*,'- READ LONGITUDE'
 error=nf90_get_var(ncid, id_var, tmpvar)
 call netcdf_err(error, 'reading longitude')

 do j = 1, j_tile
 do i = 1, i_tile
   ii = 2*i
   jj = 2*j
   longitude(i,j) = tmpvar(ii,jj)
 enddo
 enddo

 do j = 1, jp1_tile
 do i = 1, i_tile
   ii = 2*i
   jj = (2*j) - 1
   longitude_s(i,j) = tmpvar(ii,jj)
 enddo
 enddo

 do j = 1, j_tile
 do i = 1, ip1_tile
   ii = (2*i) - 1
   jj = 2*j
   longitude_w(i,j) = tmpvar(ii,jj)
 enddo
 enddo

 print*,'- READ LATITUDE ID'
 error=nf90_inq_varid(ncid, 'y', id_var)
 call netcdf_err(error, 'reading latitude id')

 print*,'- READ LATIITUDE'
 error=nf90_get_var(ncid, id_var, tmpvar)
 call netcdf_err(error, 'reading latitude')

 do j = 1, j_tile
 do i = 1, i_tile
   ii = 2*i
   jj = 2*j
   latitude(i,j) = tmpvar(ii,jj)
 enddo
 enddo

 do j = 1, jp1_tile
 do i = 1, i_tile
   ii = 2*i
   jj = (2*j) - 1
   latitude_s(i,j) = tmpvar(ii,jj)
 enddo
 enddo

 do j = 1, j_tile
 do i = 1, ip1_tile
   ii = (2*i) - 1
   jj = 2*j
   latitude_w(i,j) = tmpvar(ii,jj)
 enddo
 enddo

 deallocate(tmpvar)

 error = nf90_close(ncid)

 end subroutine get_model_latlons

!-----------------------------------------------------------------------
! Read the model land mask and terrain for a single tile.
!-----------------------------------------------------------------------

 subroutine get_model_mask_terrain(orog_file, idim, jdim, mask, terrain)

 use netcdf

 implicit none

 character(len=*), intent(in)       :: orog_file

 integer, intent(in)                :: idim, jdim
 integer(esmf_kind_i8), intent(out) :: mask(idim,jdim)

 real(esmf_kind_i8), intent(out)    :: terrain(idim,jdim)

 integer                            :: error, lat, lon
 integer                            :: ncid, id_dim, id_var

 real(kind=4), allocatable          :: dummy(:,:)

 print*,"- READ MODEL LAND MASK FILE"

 print*,'- OPEN LAND MASK FILE: ', orog_file
 error=nf90_open(orog_file,nf90_nowrite,ncid)
 call netcdf_err(error, 'opening land mask file')

 print*,"- READ I-DIMENSION"
 error=nf90_inq_dimid(ncid, 'lon', id_dim)
 call netcdf_err(error, 'reading idim id')
 error=nf90_inquire_dimension(ncid,id_dim,len=lon)
 call netcdf_err(error, 'reading idim')

 print*,"- READ J-DIMENSION"
 error=nf90_inq_dimid(ncid, 'lat', id_dim)
 call netcdf_err(error, 'reading jdim id')
 error=nf90_inquire_dimension(ncid,id_dim,len=lat)
 call netcdf_err(error, 'reading jdim')

 print*,"- I/J DIMENSIONS: ", lon, lat

 if ((lon /= idim) .or. (lat /= jdim)) then
   call error_handler("MISMATCH IN DIMENSIONS.", 1)
 endif

 allocate(dummy(idim,jdim))

 print*,"- READ LAND MASK"
 error=nf90_inq_varid(ncid, 'slmsk', id_var)
 call netcdf_err(error, 'reading slmsk id')
 error=nf90_get_var(ncid, id_var, dummy)
 call netcdf_err(error, 'reading slmsk')
 mask = nint(dummy)

 print*,"- READ RAW OROGRAPHY."
 error=nf90_inq_varid(ncid, 'orog_raw', id_var)
 call netcdf_err(error, 'reading orog_raw id')
 error=nf90_get_var(ncid, id_var, dummy)
 call netcdf_err(error, 'reading orog_raw')
 terrain = dummy

 error = nf90_close(ncid)

 deallocate (dummy)

 end subroutine get_model_mask_terrain

 subroutine cleanup_input_target_grid_data

 implicit none

 integer                                :: rc

 print*,"- DESTROY MODEL DATA."

 if (ESMF_FieldIsCreated(latitude_s_input_grid)) then
   call ESMF_FieldDestroy(latitude_s_input_grid, rc=rc)
 endif
 if (ESMF_FieldIsCreated(latitude_w_input_grid)) then
   call ESMF_FieldDestroy(latitude_w_input_grid, rc=rc)
 endif
 if (ESMF_FieldIsCreated(longitude_s_input_grid)) then
   call ESMF_FieldDestroy(longitude_s_input_grid, rc=rc)
 endif
 if (ESMF_FieldIsCreated(longitude_w_input_grid)) then
   call ESMF_FieldDestroy(longitude_w_input_grid, rc=rc)
 endif
 call ESMF_FieldDestroy(landmask_target_grid, rc=rc)
 call ESMF_FieldDestroy(latitude_target_grid, rc=rc)
 call ESMF_FieldDestroy(latitude_s_target_grid, rc=rc)
 call ESMF_FieldDestroy(latitude_w_target_grid, rc=rc)
 call ESMF_FieldDestroy(longitude_target_grid, rc=rc)
 call ESMF_FieldDestroy(longitude_s_target_grid, rc=rc)
 call ESMF_FieldDestroy(longitude_w_target_grid, rc=rc)
 call ESMF_FieldDestroy(seamask_target_grid, rc=rc)
 call ESMF_FieldDestroy(terrain_target_grid, rc=rc)
 call ESMF_GridDestroy(input_grid, rc=rc)
 call ESMF_GridDestroy(target_grid, rc=rc)

 end subroutine cleanup_input_target_grid_data

 end module model_grid
