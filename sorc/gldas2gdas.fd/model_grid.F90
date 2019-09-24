 module model_grid

 use esmf
 use setup

 implicit none

 private

 integer, public                        :: i_gldas, j_gldas
 integer, public                        :: i_gdas, j_gdas
 integer, parameter, public             :: lsoil =4

 type(esmf_grid),  public               :: gldas_grid
 type(esmf_grid),  public               :: gdas_grid

 type(esmf_field),  public              :: latitude_gldas_grid
 type(esmf_field),  public              :: longitude_gldas_grid

 public :: define_gldas_grid
 public :: define_gdas_grid

 contains

 subroutine define_gdas_grid(npets)

 implicit none

 integer, intent(in)  :: npets

 character(len=300) :: mosaic_file_gdas_grid

 integer :: error, extra, num_tiles_gdas_grid, tile

 integer, allocatable         :: decomptile(:,:)

!-----------------------------------------------------------------------
! Create ESMF grid object for the model grid.
!-----------------------------------------------------------------------

 i_gdas = 768
 j_gdas = 768

 num_tiles_gdas_grid = 6

 extra = npets / num_tiles_gdas_grid

 allocate(decomptile(2,num_tiles_gdas_grid))

 do tile = 1, num_tiles_gdas_grid
   decomptile(:,tile)=(/1,extra/)
 enddo

 mosaic_file_gdas_grid=trim(orog_dir_gdas_grid) // "/C768_mosaic.nc"

 print*,"- CALL GridCreateMosaic FOR GDAS GRID"
 gdas_grid = ESMF_GridCreateMosaic(filename=trim(mosaic_file_gdas_grid), &
                                  regDecompPTile=decomptile, &
                                  staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER, &
                                                   ESMF_STAGGERLOC_EDGE1, ESMF_STAGGERLOC_EDGE2/), &
                                  indexflag=ESMF_INDEX_GLOBAL, &
                                  tileFilePath=trim(orog_dir_gdas_grid), rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridCreateMosaic", error)

 end subroutine define_gdas_grid

 subroutine define_gldas_grid(npets)

 implicit none

 integer, intent(in) :: npets

 integer :: i, j, rc, clb(2), cub(2)
 integer :: ip1_gldas, jp1_gldas

 real(esmf_kind_r8), allocatable  :: latitude(:,:)
 real(esmf_kind_r8), allocatable  :: longitude(:,:)
 real(esmf_kind_r8)               :: deltalon
 real(esmf_kind_r8), allocatable  :: slat(:), wlat(:)
 real(esmf_kind_r8), pointer      :: lat_src_ptr(:,:)
 real(esmf_kind_r8), pointer      :: lon_src_ptr(:,:)

 type(esmf_polekind_flag)         :: polekindflag(2)

 i_gldas = 3072
 j_gldas = 1536

 ip1_gldas = i_gldas + 1
 jp1_gldas = j_gldas + 1

 polekindflag(1:2) = ESMF_POLEKIND_MONOPOLE

 print*,"- CALL GridCreate1PeriDim FOR gldas GAUSSIAN GRID."
 gldas_grid = ESMF_GridCreate1PeriDim(minIndex=(/1,1/), &
                                    maxIndex=(/i_gldas,j_gldas/), &
                                    polekindflag=polekindflag, &
                                    periodicDim=1, &
                                    poleDim=2,  &
                                    coordSys=ESMF_COORDSYS_SPH_DEG, &
                                    regDecomp=(/1,npets/),  &
                                    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
   call error_handler("IN GridCreate1PeriDim", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID LATITUDE."
 latitude_gldas_grid = ESMF_FieldCreate(gldas_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="gldas_grid_latitude", rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
   call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET GRID LONGITUDE."
 longitude_gldas_grid = ESMF_FieldCreate(gldas_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   name="gldas_grid_longitude", rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
   call error_handler("IN FieldCreate", rc)

 allocate(longitude(i_gldas,j_gldas))
 allocate(latitude(i_gldas,j_gldas))

 deltalon = 360.0_esmf_kind_r8 / real(i_gldas,kind=esmf_kind_r8)
 do i = 1, i_gldas
   longitude(i,:) = real((i-1),kind=esmf_kind_r8) * deltalon
 enddo

 allocate(slat(j_gldas))
 allocate(wlat(j_gldas))
 call splat(4, j_gldas, slat, wlat)

 do i = 1, j_gldas
    latitude(:,i) = 90.0_esmf_kind_r8 - (acos(slat(i))* 180.0_esmf_kind_r8 / &
                   (4.0_esmf_kind_r8*atan(1.0_esmf_kind_r8)))
!  latitude(:,j_gldas-i+1) = 90.0_esmf_kind_r8 - (acos(slat(i))* 180.0_esmf_kind_r8 / &
!                 (4.0_esmf_kind_r8*atan(1.0_esmf_kind_r8)))
 enddo

 deallocate(slat, wlat)

 print*,"- CALL FieldScatter FOR gldas GRID LONGITUDE."
 call ESMF_FieldScatter(longitude_gldas_grid, longitude, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

 print*,"- CALL FieldScatter FOR gldas GRID LATITUDE."
 call ESMF_FieldScatter(latitude_gldas_grid, latitude, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

 print*,"- CALL GridAddCoord FOR gldas GRID."
 call ESMF_GridAddCoord(gldas_grid, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddCoord", rc)

 print*,"- CALL GridGetCoord FOR gldas GRID X-COORD."
 nullify(lon_src_ptr)
 call ESMF_GridGetCoord(gldas_grid, &
                        staggerLoc=ESMF_STAGGERLOC_CENTER, &
                        coordDim=1, &
                        farrayPtr=lon_src_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetCoord", rc)

 print*,"- CALL GridGetCoord FOR gldas GRID Y-COORD."
 nullify(lat_src_ptr)
 call ESMF_GridGetCoord(gldas_grid, &
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
     if (i==i_gldas .and.j==j_gldas) print*,'lat/lon point last point ',latitude(i,j), longitude(i,j)
   enddo
 enddo


 print*,"- CALL GridAddCoord FOR gldas GRID."
 call ESMF_GridAddCoord(gldas_grid, &
                        staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddCoord", rc)

 print*,"- CALL GridGetCoord FOR gldas GRID X-COORD."
 nullify(lon_src_ptr)
 call ESMF_GridGetCoord(gldas_grid, &
                        staggerLoc=ESMF_STAGGERLOC_CORNER, &
                        coordDim=1, &
                        farrayPtr=lon_src_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetCoord", rc)

 print*,"- CALL GridGetCoord FOR gldas GRID Y-COORD."
 nullify(lat_src_ptr)
 call ESMF_GridGetCoord(gldas_grid, &
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
!      lat_src_ptr(i,j) = -90.0_esmf_kind_r8
       lat_src_ptr(i,j) = 90.0_esmf_kind_r8
       cycle
     endif
     if (j == jp1_gldas) then
       lat_src_ptr(i,j) = -90.0_esmf_kind_r8
!      lat_src_ptr(i,j) =  90.0_esmf_kind_r8
       cycle
     endif
     lat_src_ptr(i,j) = 0.5_esmf_kind_r8 * (latitude(i,j-1)+ latitude(i,j))
   enddo
 enddo

 print*,'lat/lon corner',maxval(lat_src_ptr),minval(lat_src_ptr),maxval(lon_src_ptr),minval(lon_src_ptr)

 deallocate(latitude,longitude)


 end subroutine define_gldas_grid
 




 end module model_grid
