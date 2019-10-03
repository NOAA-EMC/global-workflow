 module model_grid

 use esmf

 implicit none

 private

 integer, public                        :: i_target, j_target

 type(esmf_grid),  public               :: input_grid
 type(esmf_grid),  public               :: target_grid

 type(esmf_field),  public              :: latitude_target_grid
 type(esmf_field),  public              :: longitude_target_grid

 public :: define_input_grid
 public :: define_target_grid

 contains

 subroutine define_input_grid(npets)

 implicit none

 integer, intent(in)  :: npets

 character(len=200) :: mosaic_file_input_grid, orog_dir_input_grid
 character(len=200) :: sfc_files_input_grid(6), data_dir_input_grid
 character(len=200) :: orog_files_input_grid(6)

 integer :: error, extra, num_tiles_input_grid, tile

 integer, allocatable         :: decomptile(:,:)

 namelist /config/ data_dir_input_grid, sfc_files_input_grid, &
                   orog_dir_input_grid, orog_files_input_grid

!-----------------------------------------------------------------------
! Create ESMF grid object for the model grid.
!-----------------------------------------------------------------------

 num_tiles_input_grid = 6

 extra = npets / num_tiles_input_grid

 allocate(decomptile(2,num_tiles_input_grid))

 do tile = 1, num_tiles_input_grid
   decomptile(:,tile)=(/1,extra/)
 enddo

 open(41, file="./fort.41", iostat=error)
 if (error /= 0) call error_handler("OPENING SETUP NAMELIST.", error)
 read(41, nml=config, iostat=error)
 if (error /= 0) call error_handler("READING SETUP NAMELIST.", error)
 close (41)

 mosaic_file_input_grid=trim(orog_dir_input_grid) // "/C768_mosaic.nc"

 print*,"- CALL GridCreateMosaic FOR INPUT GRID"
 input_grid = ESMF_GridCreateMosaic(filename=trim(mosaic_file_input_grid), &
                                  regDecompPTile=decomptile, &
                                  staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER, &
                                                   ESMF_STAGGERLOC_EDGE1, ESMF_STAGGERLOC_EDGE2/), &
                                  indexflag=ESMF_INDEX_GLOBAL, &
                                  tileFilePath=trim(orog_dir_input_grid), rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridCreateMosaic", error)

 end subroutine define_input_grid

 subroutine define_target_grid(npets)

 implicit none

 integer, intent(in) :: npets

 integer :: i, j, rc, clb(2), cub(2)
 integer :: ip1_target, jp1_target

 real(esmf_kind_r8), allocatable  :: latitude(:,:)
 real(esmf_kind_r8), allocatable  :: longitude(:,:)
 real(esmf_kind_r8)               :: deltalon
 real(esmf_kind_r8), allocatable  :: slat(:), wlat(:)
 real(esmf_kind_r8), pointer      :: lat_src_ptr(:,:)
 real(esmf_kind_r8), pointer      :: lon_src_ptr(:,:)

 type(esmf_polekind_flag)         :: polekindflag(2)

 i_target = 3072
 j_target = 1536

 ip1_target = i_target + 1
 jp1_target = j_target + 1

 polekindflag(1:2) = ESMF_POLEKIND_MONOPOLE

 print*,"- CALL GridCreate1PeriDim FOR TARGET GAUSSIAN GRID."
 target_grid = ESMF_GridCreate1PeriDim(minIndex=(/1,1/), &
                                    maxIndex=(/i_target,j_target/), &
                                    polekindflag=polekindflag, &
                                    periodicDim=1, &
                                    poleDim=2,  &
                                    coordSys=ESMF_COORDSYS_SPH_DEG, &
                                    regDecomp=(/1,npets/),  &
                                    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
   call error_handler("IN GridCreate1PeriDim", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID LATITUDE."
 latitude_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="target_grid_latitude", rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
   call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID LONGITUDE."
 longitude_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="target_grid_longitude", rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
   call error_handler("IN FieldCreate", rc)

 allocate(longitude(i_target,j_target))
 allocate(latitude(i_target,j_target))

 deltalon = 360.0_esmf_kind_r8 / real(i_target,kind=esmf_kind_r8)
 do i = 1, i_target
   longitude(i,:) = real((i-1),kind=esmf_kind_r8) * deltalon
 enddo

 allocate(slat(j_target))
 allocate(wlat(j_target))
 call splat(4, j_target, slat, wlat)

 do i = 1, j_target
   latitude(:,i) = 90.0_esmf_kind_r8 - (acos(slat(i))* 180.0_esmf_kind_r8 / &
                  (4.0_esmf_kind_r8*atan(1.0_esmf_kind_r8)))
 enddo

 deallocate(slat, wlat)

 print*,"- CALL FieldScatter FOR TARGET GRID LONGITUDE."
 call ESMF_FieldScatter(longitude_target_grid, longitude, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

 print*,"- CALL FieldScatter FOR TARGET GRID LATITUDE."
 call ESMF_FieldScatter(latitude_target_grid, latitude, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

 print*,"- CALL GridAddCoord FOR TARGET GRID."
 call ESMF_GridAddCoord(target_grid, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddCoord", rc)

 print*,"- CALL GridGetCoord FOR TARGET GRID X-COORD."
 nullify(lon_src_ptr)
 call ESMF_GridGetCoord(target_grid, &
                        staggerLoc=ESMF_STAGGERLOC_CENTER, &
                        coordDim=1, &
                        farrayPtr=lon_src_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetCoord", rc)

 print*,"- CALL GridGetCoord FOR TARGET GRID Y-COORD."
 nullify(lat_src_ptr)
 call ESMF_GridGetCoord(target_grid, &
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
     if (i==1 .and.j==1) print*,'lat/lon point 11 ',latitude(i,j), longitude(i,j)
     if (i==i_target .and.j==j_target) print*,'lat/lon point last point ',latitude(i,j), longitude(i,j)
   enddo
 enddo


 print*,"- CALL GridAddCoord FOR TARGET GRID."
 call ESMF_GridAddCoord(target_grid, &
                        staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddCoord", rc)

 print*,"- CALL GridGetCoord FOR TARGET GRID X-COORD."
 nullify(lon_src_ptr)
 call ESMF_GridGetCoord(target_grid, &
                        staggerLoc=ESMF_STAGGERLOC_CORNER, &
                        coordDim=1, &
                        farrayPtr=lon_src_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetCoord", rc)

 print*,"- CALL GridGetCoord FOR TARGET GRID Y-COORD."
 nullify(lat_src_ptr)
 call ESMF_GridGetCoord(target_grid, &
                        staggerLoc=ESMF_STAGGERLOC_CORNER, &
                        coordDim=2, &
                        computationalLBound=clb, &
                        computationalUBound=cub, &
                        farrayPtr=lat_src_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetCoord", rc)

 do j = clb(2), cub(2)
   do i = clb(1), cub(1)
     lon_src_ptr(i,j) = longitude(i,1) - (0.5_esmf_kind_r8*deltalon)
     if (lon_src_ptr(i,j) > 360.0_esmf_kind_r8) lon_src_ptr(i,j) = lon_src_ptr(i,j) - 360.0_esmf_kind_r8
     if (j == 1) then
       lat_src_ptr(i,j) = 90.0_esmf_kind_r8
       cycle
     endif
     if (j == jp1_target) then
       lat_src_ptr(i,j) = -90.0_esmf_kind_r8
       cycle
     endif
     lat_src_ptr(i,j) = 0.5_esmf_kind_r8 * (latitude(i,j-1)+ latitude(i,j))
   enddo
 enddo

 print*,'lat/lon corner',maxval(lat_src_ptr),minval(lat_src_ptr),maxval(lon_src_ptr),minval(lon_src_ptr)

 deallocate(latitude,longitude)


 end subroutine define_target_grid
 




 end module model_grid
