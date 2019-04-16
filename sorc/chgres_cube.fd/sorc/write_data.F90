!--------------------------------------------------------------------------
! Module: write_data
!
! Abstract: Write out target grid data into appropriate files for
!    the forecast model.
!
! Main Subroutines:
! -------------------
! write_fv3_atm_header_netcdf      Writes atmospheric header file,
!                                  netcdf format.
! write_fv3_atm_bndy_data_netcdf   Writes atmospheric fields along the
!                                  lateral boundary.  For regional grids.
!                                  netcdf format.
! write_fv3_atm_data_netcdf        Writes atmospheric data into a 
!                                  'coldstart' file (netcdf)
! write_fv3_sfc_data_netcdf        Writes surface and nst data into a 
!                                  'coldstart' file (netcdf)
!--------------------------------------------------------------------------

 subroutine write_fv3_atm_header_netcdf(localpet)

 use esmf

 use netcdf

 use atmosphere, only : nvcoord_target, &
                        vcoord_target,  &
                        levp1_target

 use program_setup, only : num_tracers

 implicit none

 integer, intent(in) :: localpet

 character(len=13)   :: outfile

 integer             :: fsize=65536, initial = 0
 integer             :: header_buffer_val = 16384
 integer             :: error, ncid, dim_nvcoord
 integer             :: dim_levp1, id_ntrac, id_vcoord

 real(kind=esmf_kind_r8), allocatable :: tmp(:,:)

 if (localpet /= 0) return

 outfile="./gfs_ctrl.nc"

 print*,"- WRITE ATMOSPHERIC HEADER FILE: ", trim(outfile)

 error = nf90_create(outfile, IOR(NF90_NETCDF4,NF90_CLASSIC_MODEL), &
                     ncid, initialsize=initial, chunksize=fsize)
 call netcdf_err(error, 'CREATING FILE='//trim(outfile) )

 error = nf90_def_dim(ncid, 'nvcoord', nvcoord_target, dim_nvcoord)
 call netcdf_err(error, 'define dimension nvcoord for file='//trim(outfile) )

 error = nf90_def_dim(ncid, 'levsp', levp1_target, dim_levp1)
 call netcdf_err(error, 'define dimension levsp for file='//trim(outfile) )

 error = nf90_def_var(ncid, 'ntrac', nf90_int, id_ntrac)
 call netcdf_err(error, 'define var ntrac for file='//trim(outfile) )

 error = nf90_def_var(ncid, 'vcoord', nf90_double, (/dim_levp1, dim_nvcoord/), id_vcoord)
 call netcdf_err(error, 'define var vcoord for file='//trim(outfile) )

 error = nf90_enddef(ncid, header_buffer_val,4,0,4)
 call netcdf_err(error, 'end meta define for file='//trim(outfile) )

 error = nf90_put_var( ncid, id_ntrac, num_tracers)
 call netcdf_err(error, 'write var ntrac for file='//trim(outfile) )

 allocate(tmp(levp1_target, nvcoord_target))
 tmp(1:levp1_target,:) = vcoord_target(levp1_target:1:-1,:)

 error = nf90_put_var( ncid, id_vcoord, tmp)
 call netcdf_err(error, 'write var vcoord for file='//trim(outfile) )

 deallocate(tmp)

 error = nf90_close(ncid)

 end subroutine write_fv3_atm_header_netcdf

 subroutine write_fv3_atm_bndy_data_netcdf(localpet)

!---------------------------------------------------------------------------
!
! Output data along the four halo boundaries.  The naming convention
! assumes point (1,1) is the lower left corner of the grid:
!
!          --------------- TOP ---------------
!          |                                 |
!          |                                 |
!     LEFT |                                 | RIGHT
!          |                                 |
!          |PT(1,1)                          |
!          ------------- BOTTOM --------------
!
!---------------------------------------------------------------------------

 use esmf
 use netcdf

 use atmosphere, only            : lev_target, levp1_target, &
                                   dzdt_target_grid, &
                                   ps_target_grid, &
                                   tracers_target_grid, &
                                   u_s_target_grid, &
                                   v_s_target_grid, &
                                   u_w_target_grid, &
                                   v_w_target_grid, &
                                   zh_target_grid

 use model_grid, only            : i_target, ip1_target, j_target, jp1_target

 use program_setup, only         : halo_bndy, halo_blend, &
                                   input_type, tracers, num_tracers

 implicit none

 integer, intent(in)            :: localpet

 character(len=50)              :: name

 integer                        :: fsize=65536, initial = 0
 integer                        :: header_buffer_val = 16384
 integer                        :: ncid, error, tile, i, n
 integer                        :: dim_lon, dim_lat
 integer                        :: dim_lonp, dim_halo
 integer                        :: dim_halop, dim_latm
 integer                        :: dim_lev, dim_levp1
 integer                        :: j_target2, halo, halo_p1
 integer                        :: id_i_bottom, id_j_bottom
 integer                        :: id_i_top, id_j_top
 integer                        :: id_i_right, id_j_right
 integer                        :: id_i_left, id_j_left
 integer                        :: id_ps_bottom, id_ps_top
 integer                        :: id_ps_right, id_ps_left
 integer                        :: id_w_bottom, id_w_top
 integer                        :: id_w_right, id_w_left
 integer                        :: id_zh_bottom, id_zh_top
 integer                        :: id_zh_right, id_zh_left
 integer, allocatable           :: id_tracer_bottom(:), id_tracer_top(:)
 integer, allocatable           :: id_tracer_right(:), id_tracer_left(:)
 integer                        :: id_i_w_bottom, id_j_w_bottom
 integer                        :: id_i_w_top, id_j_w_top
 integer                        :: id_j_w_right, id_i_w_left
 integer                        :: id_j_w_left, id_i_w_right
 integer                        :: id_u_w_bottom, id_u_w_top
 integer                        :: id_u_w_right, id_u_w_left
 integer                        :: id_v_w_bottom, id_v_w_top
 integer                        :: id_v_w_right, id_v_w_left
 integer                        :: id_i_s_bottom, id_j_s_bottom
 integer                        :: id_i_s_top, id_j_s_top
 integer                        :: id_i_s_right, id_j_s_right
 integer                        :: id_i_s_left, id_j_s_left
 integer                        :: id_u_s_bottom, id_u_s_top
 integer                        :: id_u_s_right, id_u_s_left
 integer                        :: id_v_s_bottom, id_v_s_top
 integer                        :: id_v_s_right, id_v_s_left
 integer                        :: i_start_top, i_end_top
 integer                        :: j_start_top, j_end_top
 integer                        :: i_start_bottom, i_end_bottom
 integer                        :: j_start_bottom, j_end_bottom
 integer                        :: i_start_left, i_end_left
 integer                        :: j_start_left, j_end_left
 integer                        :: i_start_right, i_end_right
 integer                        :: j_start_right, j_end_right
 integer(kind=4), allocatable   :: idum(:)

 real(kind=4), allocatable        :: dum2d_top(:,:), dum2d_bottom(:,:)
 real(kind=4), allocatable        :: dum2d_left(:,:), dum2d_right(:,:)
 real(kind=4), allocatable        :: dum3d_top(:,:,:), dum3d_bottom(:,:,:)
 real(kind=4), allocatable        :: dum3d_left(:,:,:), dum3d_right(:,:,:)
 real(esmf_kind_r8), allocatable  :: data_one_tile(:,:)
 real(esmf_kind_r8), allocatable  :: data_one_tile_3d(:,:,:)

 print*,"- OUTPUT LATERAL BOUNDARY DATA."

 halo = halo_bndy + halo_blend
 halo_p1 = halo + 1

 allocate(id_tracer_bottom(num_tracers))
 allocate(id_tracer_top(num_tracers))
 allocate(id_tracer_left(num_tracers))
 allocate(id_tracer_right(num_tracers))

 if (localpet == 0) then

!--- open the file
   error = nf90_create("./gfs.bndy.nc", IOR(NF90_NETCDF4,NF90_CLASSIC_MODEL), &
                     ncid, initialsize=initial, chunksize=fsize)
   call netcdf_err(error, 'CREATING BNDY FILE' )

   error = nf90_def_dim(ncid, 'lon', i_target, dim_lon)
   call netcdf_err(error, 'defining lon dimension')

   j_target2 = j_target - (2*halo)
   error = nf90_def_dim(ncid, 'lat', j_target2, dim_lat)
   call netcdf_err(error, 'DEFINING LAT DIMENSION')

   error = nf90_def_dim(ncid, 'lonp', ip1_target, dim_lonp)
   call netcdf_err(error, 'DEFINING LONP DIMENSION')

   j_target2 = jp1_target - (2*halo_p1)
   error = nf90_def_dim(ncid, 'latm', j_target2, dim_latm)
   call netcdf_err(error, 'DEFINING LATM DIMENSION')

   error = nf90_def_dim(ncid, 'halo', halo, dim_halo)
   call netcdf_err(error, 'DEFINING HALO DIMENSION')

   error = nf90_def_dim(ncid, 'halop', halo_p1, dim_halop)
   call netcdf_err(error, 'DEFINING HALOP DIMENSION')

   error = nf90_def_dim(ncid, 'lev', lev_target, dim_lev)
   call netcdf_err(error, 'DEFINING LEV DIMENSION')

   error = nf90_def_dim(ncid, 'levp', levp1_target, dim_levp1)
   call netcdf_err(error, 'DEFINING LEVP DIMENSION')

   error = nf90_def_var(ncid, 'i_bottom', NF90_INT, &
                             (/dim_lon/), id_i_bottom)
   call netcdf_err(error, 'DEFINING I_BOTTOM')

   error = nf90_def_var(ncid, 'j_bottom', NF90_INT, &
                             (/dim_halo/), id_j_bottom)
   call netcdf_err(error, 'DEFINING J_BOTTOM')

   error = nf90_def_var(ncid, 'i_top', NF90_INT, &
                             (/dim_lon/), id_i_top)
   call netcdf_err(error, 'DEFINING I_TOP')

   error = nf90_def_var(ncid, 'j_top', NF90_INT, &
                             (/dim_halo/), id_j_top)
   call netcdf_err(error, 'DEFINING J_TOP')

   error = nf90_def_var(ncid, 'i_right', NF90_INT, &
                             (/dim_halo/), id_i_right)
   call netcdf_err(error, 'DEFINING I_RIGHT')

   error = nf90_def_var(ncid, 'j_right', NF90_INT, &
                             (/dim_lat/), id_j_right)
   call netcdf_err(error, 'DEFINING J_RIGHT')

   error = nf90_def_var(ncid, 'i_left', NF90_INT, &
                             (/dim_halo/), id_i_left)
   call netcdf_err(error, 'DEFINING I_LEFT')

   error = nf90_def_var(ncid, 'j_left', NF90_INT, &
                             (/dim_lat/), id_j_left)
   call netcdf_err(error, 'DEFINING J_LEFT')

   error = nf90_def_var(ncid, 'ps_bottom', NF90_FLOAT, &
                             (/dim_lon, dim_halo/), id_ps_bottom)
   call netcdf_err(error, 'DEFINING PS_BOTTOM')

   error = nf90_def_var(ncid, 'ps_top', NF90_FLOAT, &
                             (/dim_lon, dim_halo/), id_ps_top)
   call netcdf_err(error, 'DEFINING PS_TOP')

   error = nf90_def_var(ncid, 'ps_right', NF90_FLOAT, &
                             (/dim_halo, dim_lat/), id_ps_right)
   call netcdf_err(error, 'DEFINING PS_RIGHT')

   error = nf90_def_var(ncid, 'ps_left', NF90_FLOAT, &
                             (/dim_halo, dim_lat/), id_ps_left)
   call netcdf_err(error, 'DEFINING PS_LEFT')

   error = nf90_def_var(ncid, 'w_bottom', NF90_FLOAT, &
                             (/dim_lon, dim_halo, dim_lev/), id_w_bottom)
   call netcdf_err(error, 'DEFINING W_BOTTOM')

   error = nf90_def_var(ncid, 'w_top', NF90_FLOAT, &
                             (/dim_lon, dim_halo, dim_lev/), id_w_top)
   call netcdf_err(error, 'DEFINING W_TOP')

   error = nf90_def_var(ncid, 'w_right', NF90_FLOAT, &
                             (/dim_halo, dim_lat, dim_lev/), id_w_right)
   call netcdf_err(error, 'DEFINING W_RIGHT')

   error = nf90_def_var(ncid, 'w_left', NF90_FLOAT, &
                             (/dim_halo, dim_lat, dim_lev/), id_w_left)
   call netcdf_err(error, 'DEFINING W_LEFT')

   error = nf90_def_var(ncid, 'zh_bottom', NF90_FLOAT, &
                             (/dim_lon, dim_halo, dim_levp1/), id_zh_bottom)
   call netcdf_err(error, 'DEFINING ZH_BOTTOM')

   error = nf90_def_var(ncid, 'zh_top', NF90_FLOAT, &
                             (/dim_lon, dim_halo, dim_levp1/), id_zh_top)
   call netcdf_err(error, 'DEFINING ZH_TOP')

   error = nf90_def_var(ncid, 'zh_right', NF90_FLOAT, &
                             (/dim_halo, dim_lat, dim_levp1/), id_zh_right)
   call netcdf_err(error, 'DEFINING ZH_RIGHT')

   error = nf90_def_var(ncid, 'zh_left', NF90_FLOAT, &
                             (/dim_halo, dim_lat, dim_levp1/), id_zh_left)
   call netcdf_err(error, 'DEFINING ZH_LEFT')

   do n = 1, num_tracers

     name = trim(tracers(n)) // "_bottom"
     error = nf90_def_var(ncid, name, NF90_FLOAT, &
                             (/dim_lon, dim_halo, dim_lev/), id_tracer_bottom(n))
     call netcdf_err(error, 'DEFINING TRACER_BOTTOM')

     name = trim(tracers(n)) // "_top"
     error = nf90_def_var(ncid, name, NF90_FLOAT, &
                             (/dim_lon, dim_halo, dim_lev/), id_tracer_top(n))
     call netcdf_err(error, 'DEFINING TRACER_TOP')

     name = trim(tracers(n)) // "_right"
     error = nf90_def_var(ncid, name, NF90_FLOAT, &
                             (/dim_halo, dim_lat, dim_lev/), id_tracer_right(n))
     call netcdf_err(error, 'DEFINING TRACER_RIGHT')

     name = trim(tracers(n)) // "_left"
     error = nf90_def_var(ncid, name, NF90_FLOAT, &
                             (/dim_halo, dim_lat, dim_lev/), id_tracer_left(n))
     call netcdf_err(error, 'DEFINING TRACER_LEFT')

   enddo

   error = nf90_def_var(ncid, 'i_w_bottom', NF90_INT, &
                             (/dim_lonp/), id_i_w_bottom)
   call netcdf_err(error, 'DEFINING I_W_BOTTOM')

   error = nf90_def_var(ncid, 'j_w_bottom', NF90_INT, &
                             (/dim_halo/), id_j_w_bottom)
   call netcdf_err(error, 'DEFINING J_W_BOTTOM')

   error = nf90_def_var(ncid, 'i_w_top', NF90_INT, &
                             (/dim_lonp/), id_i_w_top)
   call netcdf_err(error, 'DEFINING I_W_TOP')

   error = nf90_def_var(ncid, 'j_w_top', NF90_INT, &
                             (/dim_halo/), id_j_w_top)
   call netcdf_err(error, 'DEFINING J_W_TOP')

   error = nf90_def_var(ncid, 'i_w_right', NF90_INT, &
                             (/dim_halop/), id_i_w_right)
   call netcdf_err(error, 'DEFINING I_W_RIGHT')

   error = nf90_def_var(ncid, 'j_w_right', NF90_INT, &
                             (/dim_lat/), id_j_w_right)
   call netcdf_err(error, 'DEFINING J_W_RIGHT')

   error = nf90_def_var(ncid, 'i_w_left', NF90_INT, &
                             (/dim_halop/), id_i_w_left)
   call netcdf_err(error, 'DEFINING I_W_LEFT')

   error = nf90_def_var(ncid, 'j_w_left', NF90_INT, &
                             (/dim_lat/), id_j_w_left)
   call netcdf_err(error, 'DEFINING J_W_LEFT')

   error = nf90_def_var(ncid, 'u_w_bottom', NF90_FLOAT, &
                             (/dim_lonp, dim_halo, dim_lev/), id_u_w_bottom)
   call netcdf_err(error, 'DEFINING U_W_BOTTOM')

   error = nf90_def_var(ncid, 'u_w_top', NF90_FLOAT, &
                             (/dim_lonp, dim_halo, dim_lev/), id_u_w_top)
   call netcdf_err(error, 'DEFINING U_W_TOP')

   error = nf90_def_var(ncid, 'u_w_right', NF90_FLOAT, &
                             (/dim_halop, dim_lat, dim_lev/), id_u_w_right)
   call netcdf_err(error, 'DEFINING U_W_RIGHT')

   error = nf90_def_var(ncid, 'u_w_left', NF90_FLOAT, &
                             (/dim_halop, dim_lat, dim_lev/), id_u_w_left)
   call netcdf_err(error, 'DEFINING U_W_LEFT')

   error = nf90_def_var(ncid, 'v_w_bottom', NF90_FLOAT, &
                             (/dim_lonp, dim_halo, dim_lev/), id_v_w_bottom)
   call netcdf_err(error, 'DEFINING V_W_BOTTOM')

   error = nf90_def_var(ncid, 'v_w_top', NF90_FLOAT, &
                             (/dim_lonp, dim_halo, dim_lev/), id_v_w_top)
   call netcdf_err(error, 'DEFINING V_W_TOP')

   error = nf90_def_var(ncid, 'v_w_right', NF90_FLOAT, &
                             (/dim_halop, dim_lat, dim_lev/), id_v_w_right)
   call netcdf_err(error, 'DEFINING V_W_RIGHT')

   error = nf90_def_var(ncid, 'v_w_left', NF90_FLOAT, &
                             (/dim_halop, dim_lat, dim_lev/), id_v_w_left)
   call netcdf_err(error, 'DEFINING V_W_LEFT')

   error = nf90_def_var(ncid, 'i_s_bottom', NF90_INT, &
                             (/dim_lon/), id_i_s_bottom)
   call netcdf_err(error, 'DEFINING I_S_BOTTOM')

   error = nf90_def_var(ncid, 'j_s_bottom', NF90_INT, &
                             (/dim_halop/), id_j_s_bottom)
   call netcdf_err(error, 'DEFINING J_S_BOTTOM')

   error = nf90_def_var(ncid, 'i_s_top', NF90_INT, &
                             (/dim_lon/), id_i_s_top)
   call netcdf_err(error, 'DEFINING I_S_TOP')

   error = nf90_def_var(ncid, 'j_s_top', NF90_INT, &
                             (/dim_halop/), id_j_s_top)
   call netcdf_err(error, 'DEFINING J_S_TOP')

   error = nf90_def_var(ncid, 'i_s_right', NF90_INT, &
                             (/dim_halo/), id_i_s_right)
   call netcdf_err(error, 'DEFINING I_S_RIGHT')

   error = nf90_def_var(ncid, 'j_s_right', NF90_INT, &
                             (/dim_latm/), id_j_s_right)
   call netcdf_err(error, 'DEFINING J_S_RIGHT')

   error = nf90_def_var(ncid, 'i_s_left', NF90_INT, &
                             (/dim_halo/), id_i_s_left)
   call netcdf_err(error, 'DEFINING I_S_LEFT')

   error = nf90_def_var(ncid, 'j_s_left', NF90_INT, &
                             (/dim_latm/), id_j_s_left)
   call netcdf_err(error, 'DEFINING J_S_LEFT')

   error = nf90_def_var(ncid, 'u_s_bottom', NF90_FLOAT, &
                             (/dim_lon, dim_halop, dim_lev/), id_u_s_bottom)
   call netcdf_err(error, 'DEFINING U_S_BOTTOM')

   error = nf90_def_var(ncid, 'u_s_top', NF90_FLOAT, &
                             (/dim_lon, dim_halop, dim_lev/), id_u_s_top)
   call netcdf_err(error, 'DEFINING U_S_TOP')

   error = nf90_def_var(ncid, 'u_s_right', NF90_FLOAT, &
                             (/dim_halo, dim_latm, dim_lev/), id_u_s_right)
   call netcdf_err(error, 'DEFINING U_S_RIGHT')

   error = nf90_def_var(ncid, 'u_s_left', NF90_FLOAT, &
                             (/dim_halo, dim_latm, dim_lev/), id_u_s_left)
   call netcdf_err(error, 'DEFINING U_S_LEFT')

   error = nf90_def_var(ncid, 'v_s_bottom', NF90_FLOAT, &
                             (/dim_lon, dim_halop, dim_lev/), id_v_s_bottom)
   call netcdf_err(error, 'DEFINING V_S_BOTTOM')

   error = nf90_def_var(ncid, 'v_s_top', NF90_FLOAT, &
                             (/dim_lon, dim_halop, dim_lev/), id_v_s_top)
   call netcdf_err(error, 'DEFINING V_S_TOP')

   error = nf90_def_var(ncid, 'v_s_right', NF90_FLOAT, &
                             (/dim_halo, dim_latm, dim_lev/), id_v_s_right)
   call netcdf_err(error, 'DEFINING V_S_RIGHT')

   error = nf90_def_var(ncid, 'v_s_left', NF90_FLOAT, &
                             (/dim_halo, dim_latm, dim_lev/), id_v_s_left)
   call netcdf_err(error, 'DEFINING V_S_LEFT')

!--- define global attributes
   if (trim(input_type) == "gaussian") then
     error = nf90_put_att(ncid, nf90_global, 'source', 'FV3GFS GAUSSIAN NEMSIO FILE')
   elseif (trim(input_type) == "gfs_gaussian") then
     error = nf90_put_att(ncid, nf90_global, 'source', 'SPECTRAL GFS GAUSSIAN NEMSIO FILE')
   elseif (trim(input_type) == "gfs_spectral") then
     error = nf90_put_att(ncid, nf90_global, 'source', 'SPECTRAL GFS SIGIO FILE')
   elseif (trim(input_type) == "history") then
     error = nf90_put_att(ncid, nf90_global, 'source', 'FV3GFS TILED HISTORY FILE')
   elseif (trim(input_type) == "restart") then
     error = nf90_put_att(ncid, nf90_global, 'source', 'FV3GFS TILED RESTART FILE')
   endif

   error = nf90_enddef(ncid, header_buffer_val,4,0,4)
   call netcdf_err(error, 'DEFINING END OF HEADER')

 endif

! Set up bounds.  Indices are with respect to the whole grid -
! including halo.

 i_start_top = 1
 i_end_top   = i_target
 j_start_top = j_target - halo + 1
 j_end_top   = j_target

 i_start_bottom = 1
 i_end_bottom   = i_target
 j_start_bottom = 1
 j_end_bottom   = halo

 i_start_left = 1
 i_end_left   = halo
 j_start_left = halo + 1
 j_end_left   = j_target - halo

 i_start_right = i_target - halo + 1
 i_end_right   = i_target
 j_start_right = halo + 1
 j_end_right   = j_target - halo

 if (localpet == 0) then

! Indices are with respect to the computational grid -
! without lateral halo but including blending halo.

   allocate(idum(i_start_top:i_end_top))
   do i = i_start_top, i_end_top
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_i_top, idum)
   call netcdf_err(error, "WRITING I_TOP")
   deallocate(idum)
   allocate(idum(i_start_bottom:i_end_bottom))
   do i = i_start_bottom, i_end_bottom
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_i_bottom, idum)
   call netcdf_err(error, "WRITING I_BOTTOM")
   deallocate(idum)
   allocate(idum(i_start_left:i_end_left))
   do i = i_start_left, i_end_left
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_i_left, idum)
   call netcdf_err(error, "WRITING I_LEFT")
   deallocate(idum)
   allocate(idum(i_start_right:i_end_right))
   do i = i_start_right, i_end_right
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_i_right, idum)
   call netcdf_err(error, "WRITING I_RIGHT")
   deallocate(idum)
   allocate(idum(j_start_top:j_end_top))
   do i = j_start_top, j_end_top
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_j_top, idum)
   call netcdf_err(error, "WRITING J_TOP")
   deallocate(idum)
   allocate(idum(j_start_bottom:j_end_bottom))
   do i = j_start_bottom, j_end_bottom
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_j_bottom, idum)
   call netcdf_err(error, "WRITING J_BOTTOM")
   deallocate(idum)
   allocate(idum(j_start_left:j_end_left))
   do i = j_start_left, j_end_left
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_j_left, idum)
   call netcdf_err(error, "WRITING J_LEFT")
   deallocate(idum)
   allocate(idum(j_start_right:j_end_right))
   do i = j_start_right, j_end_right
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_j_right, idum)
   call netcdf_err(error, "WRITING J_RIGHT")
   deallocate(idum)
 endif

!  surface pressure

 if (localpet == 0) then
   allocate(data_one_tile(i_target,j_target))
   allocate(dum2d_top(i_target,halo))
   allocate(dum2d_bottom(i_target,halo))
   allocate(dum2d_left(halo, j_target-2*halo))
   allocate(dum2d_right(halo, j_target-2*halo))
 else
   allocate(data_one_tile(0,0))
   allocate(dum2d_top(0,0))
   allocate(dum2d_bottom(0,0))
   allocate(dum2d_left(0,0))
   allocate(dum2d_right(0,0))
 endif

 tile = 1

 print*,"- CALL FieldGather FOR TARGET GRID SURFACE PRESSURE"
 call ESMF_FieldGather(ps_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dum2d_top(:,:) = data_one_tile(i_start_top:i_end_top, j_start_top:j_end_top)
   error = nf90_put_var( ncid, id_ps_top, dum2d_top)
   call netcdf_err(error, 'WRITING PS TOP' )
   dum2d_bottom(:,:) = data_one_tile(i_start_bottom:i_end_bottom, j_start_bottom:j_end_bottom)
   error = nf90_put_var( ncid, id_ps_bottom, dum2d_bottom)
   call netcdf_err(error, 'WRITING PS BOTTOM' )
   dum2d_left(:,:) = data_one_tile(i_start_left:i_end_left, j_start_left:j_end_left)
   error = nf90_put_var( ncid, id_ps_left, dum2d_left)
   call netcdf_err(error, 'WRITING PS LEFT' )
   dum2d_right(:,:) = data_one_tile(i_start_right:i_end_right, j_start_right:j_end_right)
   error = nf90_put_var( ncid, id_ps_right, dum2d_right)
   call netcdf_err(error, 'WRITING PS RIGHT' )
 endif

 deallocate(dum2d_top, dum2d_bottom, dum2d_left, dum2d_right, data_one_tile)

!  height

 if (localpet == 0) then
   allocate(data_one_tile_3d(i_target,j_target,levp1_target))
   allocate(dum3d_top(i_target,halo,levp1_target))
   allocate(dum3d_bottom(i_target,halo,levp1_target))
   allocate(dum3d_left(halo, (j_target-2*halo), levp1_target))
   allocate(dum3d_right(halo, (j_target-2*halo), levp1_target))
 else
   allocate(data_one_tile_3d(0,0,0))
   allocate(dum3d_top(0,0,0))
   allocate(dum3d_bottom(0,0,0))
   allocate(dum3d_left(0,0,0))
   allocate(dum3d_right(0,0,0))
 endif

 print*,"- CALL FieldGather FOR TARGET GRID HEIGHT FOR TILE: ", tile
 call ESMF_FieldGather(zh_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dum3d_top(:,:,:) = data_one_tile_3d(i_start_top:i_end_top,j_start_top:j_end_top,:)
   dum3d_top(:,:,1:levp1_target) = dum3d_top(:,:,levp1_target:1:-1) 
   error = nf90_put_var( ncid, id_zh_top, dum3d_top)
   call netcdf_err(error, 'WRITING ZH TOP' )
   dum3d_bottom(:,:,:) = data_one_tile_3d(i_start_bottom:i_end_bottom,j_start_bottom:j_end_bottom,:)
   dum3d_bottom(:,:,1:levp1_target) = dum3d_bottom(:,:,levp1_target:1:-1) 
   error = nf90_put_var( ncid, id_zh_bottom, dum3d_bottom)
   call netcdf_err(error, 'WRITING ZH BOTTOM' )
   dum3d_left(:,:,:) = data_one_tile_3d(i_start_left:i_end_left,j_start_left:j_end_left,:)
   dum3d_left(:,:,1:levp1_target) = dum3d_left(:,:,levp1_target:1:-1) 
   error = nf90_put_var( ncid, id_zh_left, dum3d_left)
   call netcdf_err(error, 'WRITING ZH LEFT' )
   dum3d_right(:,:,:) = data_one_tile_3d(i_start_right:i_end_right,j_start_right:j_end_right,:)
   dum3d_right(:,:,1:levp1_target) = dum3d_right(:,:,levp1_target:1:-1) 
   error = nf90_put_var( ncid, id_zh_right, dum3d_right)
   call netcdf_err(error, 'WRITING ZH RIGHT' )
 endif

 deallocate(dum3d_top, dum3d_bottom, dum3d_left, dum3d_right, data_one_tile_3d)
 
!  Tracers

 if (localpet == 0) then
   allocate(data_one_tile_3d(i_target,j_target,lev_target))
   allocate(dum3d_top(i_target,halo,lev_target))
   allocate(dum3d_bottom(i_target,halo,lev_target))
   allocate(dum3d_left(halo, (j_target-2*halo), lev_target))
   allocate(dum3d_right(halo, (j_target-2*halo), lev_target))
 else
   allocate(data_one_tile_3d(0,0,0))
   allocate(dum3d_top(0,0,0))
   allocate(dum3d_bottom(0,0,0))
   allocate(dum3d_left(0,0,0))
   allocate(dum3d_right(0,0,0))
 endif

 do n = 1, num_tracers

   print*,"- CALL FieldGather FOR TARGET GRID TRACER FOR TILE: ", trim(tracers(n)), tile
   call ESMF_FieldGather(tracers_target_grid(n), data_one_tile_3d, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum3d_top(:,:,:) = data_one_tile_3d(i_start_top:i_end_top,j_start_top:j_end_top,:)
     dum3d_top(:,:,1:lev_target) = dum3d_top(:,:,lev_target:1:-1) 
     error = nf90_put_var( ncid, id_tracer_top(n), dum3d_top)
     call netcdf_err(error, 'WRITING TRACER TOP' )
     dum3d_bottom(:,:,:) = data_one_tile_3d(i_start_bottom:i_end_bottom,j_start_bottom:j_end_bottom,:)
     dum3d_bottom(:,:,1:lev_target) = dum3d_bottom(:,:,lev_target:1:-1) 
     error = nf90_put_var( ncid, id_tracer_bottom(n), dum3d_bottom)
     call netcdf_err(error, 'WRITING TRACER BOTTOM' )
     dum3d_left(:,:,:) = data_one_tile_3d(i_start_left:i_end_left,j_start_left:j_end_left,:)
     dum3d_left(:,:,1:lev_target) = dum3d_left(:,:,lev_target:1:-1) 
     error = nf90_put_var( ncid, id_tracer_left(n), dum3d_left)
     call netcdf_err(error, 'WRITING TRACER LEFT' )
     dum3d_right(:,:,:) = data_one_tile_3d(i_start_right:i_end_right,j_start_right:j_end_right,:)
     dum3d_right(:,:,1:lev_target) = dum3d_right(:,:,lev_target:1:-1) 
     error = nf90_put_var( ncid, id_tracer_right(n), dum3d_right)
     call netcdf_err(error, 'WRITING TRACER RIGHT' )
   endif

 enddo

! Vertical velocity

 print*,"- CALL FieldGather FOR TARGET GRID W FOR TILE: ", tile
 call ESMF_FieldGather(dzdt_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dum3d_top(:,:,:) = data_one_tile_3d(i_start_top:i_end_top,j_start_top:j_end_top,:)
   dum3d_top(:,:,1:lev_target) = dum3d_top(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_w_top, dum3d_top)
   call netcdf_err(error, 'WRITING W TOP' )
   dum3d_bottom(:,:,:) = data_one_tile_3d(i_start_bottom:i_end_bottom,j_start_bottom:j_end_bottom,:)
   dum3d_bottom(:,:,1:lev_target) = dum3d_bottom(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_w_bottom, dum3d_bottom)
   call netcdf_err(error, 'WRITING W BOTTOM' )
   dum3d_left(:,:,:) = data_one_tile_3d(i_start_left:i_end_left,j_start_left:j_end_left,:)
   dum3d_left(:,:,1:lev_target) = dum3d_left(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_w_left, dum3d_left)
   call netcdf_err(error, 'WRITING W LEFT' )
   dum3d_right(:,:,:) = data_one_tile_3d(i_start_right:i_end_right,j_start_right:j_end_right,:)
   dum3d_right(:,:,1:lev_target) = dum3d_right(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_w_right, dum3d_right)
   call netcdf_err(error, 'WRITING W RIGHT' )
 endif

 deallocate(dum3d_top, dum3d_bottom, dum3d_left, dum3d_right, data_one_tile_3d)

! Set up bounds for staggered 'S' winds

 i_start_top = 1
 i_end_top   = i_target
 j_start_top = jp1_target - halo_p1 + 1
 j_end_top   = jp1_target

 i_start_bottom = 1
 i_end_bottom   = i_target
 j_start_bottom = 1
 j_end_bottom   = halo_p1

 i_start_left = 1
 i_end_left   = halo
 j_start_left = halo_p1 + 1
 j_end_left   = jp1_target - halo_p1

 i_start_right = i_target - halo + 1
 i_end_right   = i_target
 j_start_right = halo_p1 + 1
 j_end_right   = jp1_target - halo_p1

 if (localpet == 0) then
   allocate(idum(i_start_top:i_end_top))
   do i = i_start_top, i_end_top
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_i_s_top, idum)
   call netcdf_err(error, "WRITING I_S_TOP")
   deallocate(idum)
   allocate(idum(i_start_bottom:i_end_bottom))
   do i = i_start_bottom, i_end_bottom
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_i_s_bottom, idum)
   call netcdf_err(error, "WRITING I_S_BOTTOM")
   deallocate(idum)
   allocate(idum(i_start_left:i_end_left))
   do i = i_start_left, i_end_left
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_i_s_left, idum)
   call netcdf_err(error, "WRITING I_S_LEFT")
   deallocate(idum)
   allocate(idum(i_start_right:i_end_right))
   do i = i_start_right, i_end_right
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_i_s_right, idum)
   call netcdf_err(error, "WRITING I_S_RIGHT")
   deallocate(idum)
   allocate(idum(j_start_top:j_end_top))
   do i = j_start_top, j_end_top
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_j_s_top, idum)
   call netcdf_err(error, "WRITING J_S_TOP")
   deallocate(idum)
   allocate(idum(j_start_bottom:j_end_bottom))
   do i = j_start_bottom, j_end_bottom
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_j_s_bottom, idum)
   call netcdf_err(error, "WRITING J_S_BOTTOM")
   deallocate(idum)
   allocate(idum(j_start_left:j_end_left))
   do i = j_start_left, j_end_left
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_j_s_left, idum)
   call netcdf_err(error, "WRITING J_S_LEFT")
   deallocate(idum)
   allocate(idum(j_start_right:j_end_right))
   do i = j_start_right, j_end_right
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_j_s_right, idum)
   call netcdf_err(error, "WRITING J_S_RIGHT")
   deallocate(idum)
 endif

! U-WINDS 'S'

 if (localpet == 0) then
   allocate(data_one_tile_3d(i_target,jp1_target,lev_target))
   allocate(dum3d_top(i_target,halo_p1,lev_target))
   allocate(dum3d_bottom(i_target,halo_p1,lev_target))
   allocate(dum3d_left(halo, (j_end_left-j_start_left+1), lev_target))
   allocate(dum3d_right(halo, (j_end_right-j_start_right+1), lev_target))
 else
   allocate(data_one_tile_3d(0,0,0))
   allocate(dum3d_top(0,0,0))
   allocate(dum3d_bottom(0,0,0))
   allocate(dum3d_left(0,0,0))
   allocate(dum3d_right(0,0,0))
 endif

 print*,"- CALL FieldGather FOR TARGET GRID U_S FOR TILE: ", tile
 call ESMF_FieldGather(u_s_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dum3d_top(:,:,:) = data_one_tile_3d(i_start_top:i_end_top,j_start_top:j_end_top,:)
   dum3d_top(:,:,1:lev_target) = dum3d_top(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_u_s_top, dum3d_top)
   call netcdf_err(error, 'WRITING U_S TOP' )
   dum3d_bottom(:,:,:) = data_one_tile_3d(i_start_bottom:i_end_bottom,j_start_bottom:j_end_bottom,:)
   dum3d_bottom(:,:,1:lev_target) = dum3d_bottom(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_u_s_bottom, dum3d_bottom)
   call netcdf_err(error, 'WRITING U_S BOTTOM' )
   dum3d_left(:,:,:) = data_one_tile_3d(i_start_left:i_end_left,j_start_left:j_end_left,:)
   dum3d_left(:,:,1:lev_target) = dum3d_left(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_u_s_left, dum3d_left)
   call netcdf_err(error, 'WRITING U_S LEFT' )
   dum3d_right(:,:,:) = data_one_tile_3d(i_start_right:i_end_right,j_start_right:j_end_right,:)
   dum3d_right(:,:,1:lev_target) = dum3d_right(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_u_s_right, dum3d_right)
   call netcdf_err(error, 'WRITING U_S RIGHT' )
 endif

! V-WINDS 'S'

 print*,"- CALL FieldGather FOR TARGET GRID V_S FOR TILE: ", tile
 call ESMF_FieldGather(v_s_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dum3d_top(:,:,:) = data_one_tile_3d(i_start_top:i_end_top,j_start_top:j_end_top,:)
   dum3d_top(:,:,1:lev_target) = dum3d_top(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_v_s_top, dum3d_top)
   call netcdf_err(error, 'WRITING V_S TOP' )
   dum3d_bottom(:,:,:) = data_one_tile_3d(i_start_bottom:i_end_bottom,j_start_bottom:j_end_bottom,:)
   dum3d_bottom(:,:,1:lev_target) = dum3d_bottom(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_v_s_bottom, dum3d_bottom)
   call netcdf_err(error, 'WRITING V_S BOTTOM' )
   dum3d_left(:,:,:) = data_one_tile_3d(i_start_left:i_end_left,j_start_left:j_end_left,:)
   dum3d_left(:,:,1:lev_target) = dum3d_left(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_v_s_left, dum3d_left)
   call netcdf_err(error, 'WRITING V_S LEFT' )
   dum3d_right(:,:,:) = data_one_tile_3d(i_start_right:i_end_right,j_start_right:j_end_right,:)
   dum3d_right(:,:,1:lev_target) = dum3d_right(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_v_s_right, dum3d_right)
   call netcdf_err(error, 'WRITING V_S RIGHT' )
 endif

 deallocate(dum3d_top, dum3d_bottom, dum3d_left, dum3d_right, data_one_tile_3d)

! Set up bounds for staggered 'W' winds

 i_start_top = 1
 i_end_top   = ip1_target
 j_start_top = j_target - halo + 1
 j_end_top   = j_target

 i_start_bottom = 1
 i_end_bottom   = ip1_target
 j_start_bottom = 1
 j_end_bottom   = halo

 i_start_left = 1
 i_end_left   = halo_p1
 j_start_left = halo_p1
 j_end_left   = j_target - halo

 i_start_right = ip1_target - halo_p1 + 1
 i_end_right   = ip1_target
 j_start_right = halo_p1
 j_end_right   = j_target - halo

 if (localpet == 0) then
   allocate(idum(i_start_top:i_end_top))
   do i = i_start_top, i_end_top
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_i_w_top, idum)
   call netcdf_err(error, "WRITING I_W_TOP")
   deallocate(idum)
   allocate(idum(i_start_bottom:i_end_bottom))
   do i = i_start_bottom, i_end_bottom
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_i_w_bottom, idum)
   call netcdf_err(error, "WRITING I_W_BOTTOM")
   deallocate(idum)
   allocate(idum(i_start_left:i_end_left))
   do i = i_start_left, i_end_left
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_i_w_left, idum)
   call netcdf_err(error, "WRITING I_W_LEFT")
   deallocate(idum)
   allocate(idum(i_start_right:i_end_right))
   do i = i_start_right, i_end_right
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_i_w_right, idum)
   call netcdf_err(error, "WRITING I_W_RIGHT")
   deallocate(idum)
   allocate(idum(j_start_top:j_end_top))
   do i = j_start_top, j_end_top
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_j_w_top, idum)
   call netcdf_err(error, "WRITING J_W_TOP")
   deallocate(idum)
   allocate(idum(j_start_bottom:j_end_bottom))
   do i = j_start_bottom, j_end_bottom
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_j_w_bottom, idum)
   call netcdf_err(error, "WRITING J_W_BOTTOM")
   deallocate(idum)
   allocate(idum(j_start_left:j_end_left))
   do i = j_start_left, j_end_left
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_j_w_left, idum)
   call netcdf_err(error, "WRITING J_W_LEFT")
   deallocate(idum)
   allocate(idum(j_start_right:j_end_right))
   do i = j_start_right, j_end_right
     idum(i) = i - halo_bndy
   enddo
   error = nf90_put_var(ncid, id_j_w_right, idum)
   call netcdf_err(error, "WRITING J_W_RIGHT")
   deallocate(idum)
 endif

! U-WINDS 'W'

 if (localpet == 0) then
   allocate(data_one_tile_3d(ip1_target,j_target,lev_target))
   allocate(dum3d_top(ip1_target,halo,lev_target))
   allocate(dum3d_bottom(ip1_target,halo,lev_target))
   allocate(dum3d_left(halo_p1, (j_end_left-j_start_left+1), lev_target))
   allocate(dum3d_right(halo_p1, (j_end_right-j_start_right+1), lev_target))
 else
   allocate(data_one_tile_3d(0,0,0))
   allocate(dum3d_top(0,0,0))
   allocate(dum3d_bottom(0,0,0))
   allocate(dum3d_left(0,0,0))
   allocate(dum3d_right(0,0,0))
 endif

 print*,"- CALL FieldGather FOR TARGET GRID U_W FOR TILE: ", tile
 call ESMF_FieldGather(u_w_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dum3d_top(:,:,:) = data_one_tile_3d(i_start_top:i_end_top,j_start_top:j_end_top,:)
   dum3d_top(:,:,1:lev_target) = dum3d_top(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_u_w_top, dum3d_top)
   call netcdf_err(error, 'WRITING U_W TOP' )
   dum3d_bottom(:,:,:) = data_one_tile_3d(i_start_bottom:i_end_bottom,j_start_bottom:j_end_bottom,:)
   dum3d_bottom(:,:,1:lev_target) = dum3d_bottom(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_u_w_bottom, dum3d_bottom)
   call netcdf_err(error, 'WRITING U_W BOTTOM' )
   dum3d_left(:,:,:) = data_one_tile_3d(i_start_left:i_end_left,j_start_left:j_end_left,:)
   dum3d_left(:,:,1:lev_target) = dum3d_left(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_u_w_left, dum3d_left)
   call netcdf_err(error, 'WRITING U_W LEFT' )
   dum3d_right(:,:,:) = data_one_tile_3d(i_start_right:i_end_right,j_start_right:j_end_right,:)
   dum3d_right(:,:,1:lev_target) = dum3d_right(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_u_w_right, dum3d_right)
   call netcdf_err(error, 'WRITING U_W RIGHT' )
 endif

! V-WINDS 'W'

 print*,"- CALL FieldGather FOR TARGET GRID V_W FOR TILE: ", tile
 call ESMF_FieldGather(v_w_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=error)
 if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

 if (localpet == 0) then
   dum3d_top(:,:,:) = data_one_tile_3d(i_start_top:i_end_top,j_start_top:j_end_top,:)
   dum3d_top(:,:,1:lev_target) = dum3d_top(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_v_w_top, dum3d_top)
   call netcdf_err(error, 'WRITING V_W TOP' )
   dum3d_bottom(:,:,:) = data_one_tile_3d(i_start_bottom:i_end_bottom,j_start_bottom:j_end_bottom,:)
   dum3d_bottom(:,:,1:lev_target) = dum3d_bottom(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_v_w_bottom, dum3d_bottom)
   call netcdf_err(error, 'WRITING V_W BOTTOM' )
   dum3d_left(:,:,:) = data_one_tile_3d(i_start_left:i_end_left,j_start_left:j_end_left,:)
   dum3d_left(:,:,1:lev_target) = dum3d_left(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_v_w_left, dum3d_left)
   call netcdf_err(error, 'WRITING V_W LEFT' )
   dum3d_right(:,:,:) = data_one_tile_3d(i_start_right:i_end_right,j_start_right:j_end_right,:)
   dum3d_right(:,:,1:lev_target) = dum3d_right(:,:,lev_target:1:-1) 
   error = nf90_put_var( ncid, id_v_w_right, dum3d_right)
   call netcdf_err(error, 'WRITING V_W RIGHT' )
 endif

 deallocate(dum3d_top, dum3d_bottom, dum3d_left, dum3d_right, data_one_tile_3d)
 deallocate(id_tracer_bottom, id_tracer_top, id_tracer_left, id_tracer_right)

 if (localpet == 0) error = nf90_close(ncid)

 end subroutine write_fv3_atm_bndy_data_netcdf

!---------------------------------------------------------------------------
! Write atmospheric coldstart files.
!
! Routine write tiled files in parallel.  Tile 1 is written by
! localpet 0; tile 2 by localpet 1, etc.  The number of pets
! must be equal to or greater than the number of tiled files.
!---------------------------------------------------------------------------

 subroutine write_fv3_atm_data_netcdf(localpet)

 use esmf
 use netcdf

 use program_setup, only           : halo=>halo_bndy, &
                                     input_type, tracers, num_tracers

 use atmosphere, only              : lev_target, &
                                     levp1_target, &
                                     ps_target_grid, &
                                     zh_target_grid, &
                                     dzdt_target_grid, &
                                     tracers_target_grid, &
                                     temp_target_grid, &
                                     delp_target_grid, &
                                     u_s_target_grid,   &
                                     v_s_target_grid,   &
                                     u_w_target_grid,   &
                                     v_w_target_grid

 use model_grid, only              : num_tiles_target_grid, &
                                     i_target, j_target, &
                                     ip1_target, jp1_target, &
                                     longitude_target_grid, &
                                     latitude_target_grid

 implicit none

 integer, intent(in)              :: localpet

 character(len=128)               :: outfile

 integer                          :: error, ncid, tile, n
 integer                          :: fsize=65536, initial = 0
 integer                          :: header_buffer_val = 16384
 integer                          :: dim_lon, dim_lat
 integer                          :: dim_lonp, dim_latp
 integer                          :: dim_lev, dim_levp1, dim_ntracer
 integer, allocatable             :: id_tracers(:)
 integer                          :: id_lon, id_lat, id_ps
 integer                          :: id_w, id_zh, id_u_w
 integer                          :: id_v_w, id_u_s, id_v_s
 integer                          :: id_t, id_delp
 integer                          :: i_start, i_end, j_start, j_end
 integer                          :: i_target_out, j_target_out
 integer                          :: ip1_target_out, jp1_target_out
 integer                          :: ip1_end, jp1_end

 real(esmf_kind_r8), allocatable  :: data_one_tile(:,:)
 real(esmf_kind_r8), allocatable  :: data_one_tile_3d(:,:,:)
 real(kind=4), allocatable        :: dum2d(:,:)
 real(kind=4), allocatable        :: dum3d(:,:,:)

! Remove any halo region.

 i_target_out = i_target-(2*halo)
 j_target_out = j_target-(2*halo)
 
 i_start = halo + 1
 j_start = halo + 1
 i_end   = i_target - halo
 j_end   = j_target - halo

 ip1_target_out = i_target_out + 1
 jp1_target_out = j_target_out + 1

 ip1_end = i_end + 1
 jp1_end = j_end + 1

 if (localpet < num_tiles_target_grid) then
   allocate(data_one_tile(i_target,j_target))
   allocate(dum2d(i_target_out,j_target_out))
 else
   allocate(data_one_tile(0,0))
   allocate(dum2d(0,0))
 endif

 allocate(id_tracers(num_tracers))

 HEADER : if (localpet < num_tiles_target_grid) then

   tile = localpet + 1
   WRITE(OUTFILE, '(A, I1, A)'), 'out.atm.tile', tile, '.nc'

!--- open the file
   error = nf90_create(outfile, IOR(NF90_NETCDF4,NF90_CLASSIC_MODEL), &
                       ncid, initialsize=initial, chunksize=fsize)
   call netcdf_err(error, 'CREATING FILE='//trim(outfile) )

!--- define dimension
   error = nf90_def_dim(ncid, 'lon', i_target_out, dim_lon)
   call netcdf_err(error, 'DEFINING LON DIMENSION' )
   error = nf90_def_dim(ncid, 'lat', j_target_out, dim_lat)
   call netcdf_err(error, 'DEFINING LAT DIMENSION' )
   error = nf90_def_dim(ncid, 'lonp', ip1_target_out, dim_lonp)
   call netcdf_err(error, 'DEFINING LONP DIMENSION' )
   error = nf90_def_dim(ncid, 'latp', jp1_target_out, dim_latp)
   call netcdf_err(error, 'DEFINING LATP DIMENSION' )
   error = nf90_def_dim(ncid, 'lev', lev_target, dim_lev)
   call netcdf_err(error, 'DEFINING LEV DIMENSION' )
   error = nf90_def_dim(ncid, 'levp', levp1_target, dim_levp1)
   call netcdf_err(error, 'DEFINING LEVP DIMENSION' )
   error = nf90_def_dim(ncid, 'ntracer', num_tracers, dim_ntracer)
   call netcdf_err(error, 'DEFINING NTRACER DIMENSION' )

!--- define global attributes
   if (trim(input_type) == "gaussian") then
     error = nf90_put_att(ncid, nf90_global, 'source', 'FV3GFS GAUSSIAN NEMSIO FILE')
   elseif (trim(input_type) == "gfs_gaussian") then
     error = nf90_put_att(ncid, nf90_global, 'source', 'SPECTRAL GFS GAUSSIAN NEMSIO FILE')
   elseif (trim(input_type) == "gfs_spectral") then
     error = nf90_put_att(ncid, nf90_global, 'source', 'SPECTRAL GFS SIGIO FILE')
   elseif (trim(input_type) == "history") then
     error = nf90_put_att(ncid, nf90_global, 'source', 'FV3GFS TILED HISTORY FILE')
   elseif (trim(input_type) == "restart") then
     error = nf90_put_att(ncid, nf90_global, 'source', 'FV3GFS TILED RESTART FILE')
   endif

!--- define field
   error = nf90_def_var(ncid, 'lon', NF90_FLOAT, (/dim_lon/), id_lon)
   call netcdf_err(error, 'DEFINING LON FIELD' )
   error = nf90_put_att(ncid, id_lon, "cartesian_axis", "X")
   call netcdf_err(error, 'WRITING LON FIELD' )
   error = nf90_def_var(ncid, 'lat', NF90_FLOAT, (/dim_lat/), id_lat)
   call netcdf_err(error, 'DEFINING LAT FIELD' )
   error = nf90_put_att(ncid, id_lat, "cartesian_axis", "Y")
   call netcdf_err(error, 'WRITING LAT FIELD' )
   error = nf90_def_var(ncid, 'ps', NF90_FLOAT, (/dim_lon,dim_lat/), id_ps)
   call netcdf_err(error, 'WRITING PS' )
   error = nf90_def_var(ncid, 'w', NF90_FLOAT, (/dim_lon,dim_lat,dim_lev/), id_w)
   call netcdf_err(error, 'WRITING W' )
   error = nf90_def_var(ncid, 'zh', NF90_FLOAT, (/dim_lon,dim_lat,dim_levp1/), id_zh)
   call netcdf_err(error, 'WRITING ZH' )
   error = nf90_def_var(ncid, 't', NF90_FLOAT, (/dim_lon,dim_lat,dim_lev/), id_t)
   call netcdf_err(error, 'WRITING T' )
   error = nf90_def_var(ncid, 'delp', NF90_FLOAT, (/dim_lon,dim_lat,dim_lev/), id_delp)
   call netcdf_err(error, 'WRITING DELP' )
   do n = 1, num_tracers
     error = nf90_def_var(ncid, tracers(n), NF90_FLOAT, (/dim_lon,dim_lat,dim_lev/), id_tracers(n))
     call netcdf_err(error, 'WRITING TRACERS' )
   enddo
   error = nf90_def_var(ncid, 'u_w', NF90_FLOAT, (/dim_lonp,dim_lat,dim_lev/), id_u_w)
   call netcdf_err(error, 'WRITING U_W' )
   error = nf90_def_var(ncid, 'v_w', NF90_FLOAT, (/dim_lonp,dim_lat,dim_lev/), id_v_w)
   call netcdf_err(error, 'WRITING V_W' )
   error = nf90_def_var(ncid, 'u_s', NF90_FLOAT, (/dim_lon,dim_latp,dim_lev/), id_u_s)
   call netcdf_err(error, 'WRITING U_S' )
   error = nf90_def_var(ncid, 'v_s', NF90_FLOAT, (/dim_lon,dim_latp,dim_lev/), id_v_s)
   call netcdf_err(error, 'WRITING V_S' )

   error = nf90_enddef(ncid, header_buffer_val,4,0,4)
   call netcdf_err(error, 'DEFINING HEADER' )

 endif HEADER

!  longitude

 do tile = 1, num_tiles_target_grid
   print*,"- CALL FieldGather FOR TARGET GRID LONGITUDE FOR TILE: ", tile
   call ESMF_FieldGather(longitude_target_grid, data_one_tile, rootPet=tile-1, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)
 enddo

 if (localpet < num_tiles_target_grid) then
   dum2d(:,:) = data_one_tile(i_start:i_end, j_start:j_end)
   error = nf90_put_var( ncid, id_lon, dum2d(:,1))
   call netcdf_err(error, 'WRITING LONGITUDE RECORD' )
 endif

!  latitude

 do tile = 1, num_tiles_target_grid
   print*,"- CALL FieldGather FOR TARGET GRID LATITUDE FOR TILE: ", tile
   call ESMF_FieldGather(latitude_target_grid, data_one_tile, rootPet=tile-1, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)
 enddo

 if (localpet < num_tiles_target_grid) then
   dum2d(:,:) = data_one_tile(i_start:i_end, j_start:j_end)
   error = nf90_put_var( ncid, id_lat, dum2d(1,:))
   call netcdf_err(error, 'WRITING LATITUDE RECORD' )
 endif

!  surface pressure

 do tile = 1, num_tiles_target_grid
   print*,"- CALL FieldGather FOR TARGET GRID SURFACE PRESSURE FOR TILE: ", tile
   call ESMF_FieldGather(ps_target_grid, data_one_tile, rootPet=tile-1, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)
 enddo

 if (localpet < num_tiles_target_grid) then
   dum2d(:,:) = data_one_tile(i_start:i_end, j_start:j_end)
   error = nf90_put_var( ncid, id_ps, dum2d)
   call netcdf_err(error, 'WRITING SURFACE PRESSURE RECORD' )
 endif

 deallocate(dum2d, data_one_tile)

!  height

 if (localpet < num_tiles_target_grid) then
   allocate(dum3d(i_target_out,j_target_out,levp1_target))
   allocate(data_one_tile_3d(i_target,j_target,levp1_target))
 else
   allocate(dum3d(0,0,0))
   allocate(data_one_tile_3d(0,0,0))
 endif

 do tile = 1, num_tiles_target_grid
   print*,"- CALL FieldGather FOR TARGET GRID HEIGHT FOR TILE: ", tile
   call ESMF_FieldGather(zh_target_grid, data_one_tile_3d, rootPet=tile-1, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)
 enddo

 if (localpet < num_tiles_target_grid) then
   dum3d(:,:,:) = data_one_tile_3d(i_start:i_end,j_start:j_end,:)
   dum3d(:,:,1:levp1_target) = dum3d(:,:,levp1_target:1:-1)
   error = nf90_put_var( ncid, id_zh, dum3d)
   call netcdf_err(error, 'WRITING HEIGHT RECORD' )
 endif

 deallocate(dum3d, data_one_tile_3d)

!  vertical velocity

 if (localpet < num_tiles_target_grid) then
   allocate(dum3d(i_target_out,j_target_out,lev_target))
   allocate(data_one_tile_3d(i_target,j_target,lev_target))
 else
   allocate(dum3d(0,0,0))
   allocate(data_one_tile_3d(0,0,0))
 endif

 do tile = 1, num_tiles_target_grid
   print*,"- CALL FieldGather FOR TARGET GRID VERTICAL VELOCITY FOR TILE: ", tile
   call ESMF_FieldGather(dzdt_target_grid, data_one_tile_3d, rootPet=tile-1, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)
 enddo

 if (localpet < num_tiles_target_grid) then
   dum3d(:,:,:) = data_one_tile_3d(i_start:i_end,j_start:j_end,:)
   dum3d(:,:,1:lev_target) = dum3d(:,:,lev_target:1:-1)
   error = nf90_put_var( ncid, id_w, dum3d)
   call netcdf_err(error, 'WRITING VERTICAL VELOCITY RECORD' )
 endif

!  delp

 do tile = 1, num_tiles_target_grid
   print*,"- CALL FieldGather FOR TARGET GRID DELP FOR TILE: ", tile
   call ESMF_FieldGather(delp_target_grid, data_one_tile_3d, rootPet=tile-1, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)
 enddo

 if (localpet < num_tiles_target_grid) then
   dum3d(:,:,:) = data_one_tile_3d(i_start:i_end,j_start:j_end,:)
   dum3d(:,:,1:lev_target) = dum3d(:,:,lev_target:1:-1)
   error = nf90_put_var( ncid, id_delp, dum3d)
   call netcdf_err(error, 'WRITING DELP RECORD' )
 endif

!  temperature

 do tile = 1, num_tiles_target_grid
   print*,"- CALL FieldGather FOR TARGET GRID TEMPERATURE FOR TILE: ", tile
   call ESMF_FieldGather(temp_target_grid, data_one_tile_3d, rootPet=tile-1, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)
 enddo

 if (localpet < num_tiles_target_grid) then
   dum3d(:,:,:) = data_one_tile_3d(i_start:i_end,j_start:j_end,:)
   dum3d(:,:,1:lev_target) = dum3d(:,:,lev_target:1:-1)
   error = nf90_put_var( ncid, id_t, dum3d)
   call netcdf_err(error, 'WRITING TEMPERTAURE RECORD' )
 endif

!  tracers

 do n = 1, num_tracers

   do tile = 1, num_tiles_target_grid
     print*,"- CALL FieldGather FOR TARGET GRID TRACER ", trim(tracers(n)), " TILE: ", tile
     call ESMF_FieldGather(tracers_target_grid(n), data_one_tile_3d, rootPet=tile-1, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)
   enddo

   if (localpet < num_tiles_target_grid) then
     dum3d(:,:,:) = data_one_tile_3d(i_start:i_end,j_start:j_end,:)
     dum3d(:,:,1:lev_target) = dum3d(:,:,lev_target:1:-1)
     error = nf90_put_var( ncid, id_tracers(n), dum3d)
     call netcdf_err(error, 'WRITING TRACER RECORD' )
   endif

 enddo

 deallocate(dum3d, data_one_tile_3d)

!  uwinds s

 if (localpet < num_tiles_target_grid) then
   allocate(dum3d(i_target_out,jp1_target_out,lev_target))
   allocate(data_one_tile_3d(i_target,jp1_target,lev_target))
 else
   allocate(dum3d(0,0,0))
   allocate(data_one_tile_3d(0,0,0))
 endif

 do tile = 1, num_tiles_target_grid
   print*,"- CALL FieldGather FOR TARGET GRID U_S FOR TILE: ", tile
   call ESMF_FieldGather(u_s_target_grid, data_one_tile_3d, rootPet=tile-1, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)
 enddo

 if (localpet < num_tiles_target_grid) then
   dum3d(:,:,:) = data_one_tile_3d(i_start:i_end,j_start:jp1_end,:)
   dum3d(:,:,1:lev_target) = dum3d(:,:,lev_target:1:-1)
   error = nf90_put_var( ncid, id_u_s, dum3d)
   call netcdf_err(error, 'WRITING U_S RECORD' )
 endif

!  vwinds s

 do tile = 1, num_tiles_target_grid
   print*,"- CALL FieldGather FOR TARGET GRID V_S FOR TILE: ", tile
   call ESMF_FieldGather(v_s_target_grid, data_one_tile_3d, rootPet=tile-1, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)
 enddo

 if (localpet < num_tiles_target_grid) then
   dum3d(:,:,:) = data_one_tile_3d(i_start:i_end,j_start:jp1_end,:)
   dum3d(:,:,1:lev_target) = dum3d(:,:,lev_target:1:-1)
   error = nf90_put_var( ncid, id_v_s, dum3d)
   call netcdf_err(error, 'WRITING V_S RECORD' )
 endif

 deallocate(dum3d, data_one_tile_3d)

!  uwinds w

 if (localpet < num_tiles_target_grid) then
   allocate(dum3d(ip1_target_out,j_target_out,lev_target))
   allocate(data_one_tile_3d(ip1_target,j_target,lev_target))
 else
   allocate(dum3d(0,0,0))
   allocate(data_one_tile_3d(0,0,0))
 endif

 do tile = 1, num_tiles_target_grid
   print*,"- CALL FieldGather FOR TARGET GRID U_W FOR TILE: ", tile
   call ESMF_FieldGather(u_w_target_grid, data_one_tile_3d, rootPet=tile-1, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)
 enddo

 if (localpet < num_tiles_target_grid) then
   dum3d(:,:,:) = data_one_tile_3d(i_start:ip1_end,j_start:j_end,:)
   dum3d(:,:,1:lev_target) = dum3d(:,:,lev_target:1:-1)
   error = nf90_put_var( ncid, id_u_w, dum3d)
   call netcdf_err(error, 'WRITING U_W RECORD' )
 endif

!  vwinds w

 do tile = 1, num_tiles_target_grid
   print*,"- CALL FieldGather FOR TARGET GRID V_W FOR TILE: ", tile
   call ESMF_FieldGather(v_w_target_grid, data_one_tile_3d, rootPet=tile-1, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)
 enddo

 if (localpet < num_tiles_target_grid) then
   dum3d(:,:,:) = data_one_tile_3d(i_start:ip1_end,j_start:j_end,:)
   dum3d(:,:,1:lev_target) = dum3d(:,:,lev_target:1:-1)
   error = nf90_put_var( ncid, id_v_w, dum3d)
   call netcdf_err(error, 'WRITING V_W RECORD' )
 endif

 deallocate(dum3d, data_one_tile_3d, id_tracers)

!-------------------------------------------------------------------------------
! close file
!-------------------------------------------------------------------------------

 if (localpet < num_tiles_target_grid) error = nf90_close(ncid)

 end subroutine write_fv3_atm_data_netcdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

 subroutine write_fv3_sfc_data_netcdf(localpet)

 use esmf
 use netcdf

 use model_grid, only            : num_tiles_target_grid, &
                                   landmask_target_grid, &
                                   i_target, j_target, lsoil_target

 use program_setup, only         : convert_nst, halo=>halo_bndy

 use surface, only               : canopy_mc_target_grid,  &
                                   f10m_target_grid, &
                                   ffmm_target_grid, &
                                   q2m_target_grid,   &
                                   seaice_depth_target_grid, &
                                   seaice_fract_target_grid, &
                                   seaice_skin_temp_target_grid, &
                                   skin_temp_target_grid, &
                                   soil_temp_target_grid, &
                                   soilm_liq_target_grid, &
                                   soilm_tot_target_grid, &
                                   srflag_target_grid, &
                                   snow_liq_equiv_target_grid, &
                                   snow_depth_target_grid, &
                                   t2m_target_grid,   &
                                   tprcp_target_grid, &
                                   ustar_target_grid, &
                                   z0_target_grid, &
                                   c_d_target_grid, &
                                   c_0_target_grid, &
                                   d_conv_target_grid, &
                                   dt_cool_target_grid, &
                                   ifd_target_grid, &
                                   qrain_target_grid, &
                                   tref_target_grid, &
                                   w_d_target_grid, &
                                   w_0_target_grid, &
                                   xs_target_grid, &
                                   xt_target_grid, &
                                   xu_target_grid, &
                                   xv_target_grid, &
                                   xz_target_grid, &
                                   xtts_target_grid, &
                                   xzts_target_grid, &
                                   z_c_target_grid, &
                                   zm_target_grid

 use static_data, only           : alvsf_target_grid,   &
                                   alnsf_target_grid,   &
                                   alvwf_target_grid,   &
                                   alnwf_target_grid,   &
                                   facsf_target_grid, &
                                   facwf_target_grid, &
                                   max_veg_greenness_target_grid, &
                                   min_veg_greenness_target_grid, &
                                   mxsno_albedo_target_grid, &
                                   slope_type_target_grid, &
                                   soil_type_target_grid,  &
                                   substrate_temp_target_grid,  &
                                   veg_greenness_target_grid, &
                                   veg_type_target_grid

 implicit none

 integer, intent(in)            :: localpet
 character(len=128)             :: outfile

 integer                        :: fsize=65536, initial = 0
 integer                        :: header_buffer_val = 16384
 integer                        :: dim_x, dim_y, dim_lsoil, dim_time
 integer                        :: error, i, ncid, tile
 integer                        :: id_x, id_y, id_lsoil
 integer                        :: id_slmsk, id_time
 integer                        :: id_tsea, id_sheleg, id_tg3
 integer                        :: id_zorl, id_alvsf, id_alvwf
 integer                        :: id_alnsf, id_alnwf, id_vfrac
 integer                        :: id_canopy, id_f10m, id_t2m
 integer                        :: id_q2m, id_vtype, id_stype
 integer                        :: id_facsf, id_facwf, id_uustar
 integer                        :: id_ffmm, id_ffhh, id_hice
 integer                        :: id_fice, id_tisfc, id_tprcp
 integer                        :: id_srflag, id_snwdph, id_shdmin
 integer                        :: id_shdmax, id_slope, id_snoalb
 integer                        :: id_stc, id_smc, id_slc
 integer                        :: id_tref, id_z_c, id_c_0
 integer                        :: id_c_d, id_w_0, id_w_d
 integer                        :: id_xt, id_xs, id_xu, id_xv
 integer                        :: id_xz, id_zm, id_xtts, id_xzts
 integer                        :: id_d_conv, id_ifd, id_dt_cool
 integer                        :: id_qrain
 integer                        :: i_target_out, j_target_out
 integer                        :: istart, iend, jstart, jend

 integer(esmf_kind_i8), allocatable :: idata_one_tile(:,:)

 real(kind=4), allocatable       :: lsoil_data(:), x_data(:), y_data(:)
 real(kind=8), allocatable       :: dum2d(:,:), dum3d(:,:,:)
 real(kind=4)                    :: times
 real(esmf_kind_r8), allocatable :: data_one_tile(:,:)
 real(esmf_kind_r8), allocatable :: data_one_tile_3d(:,:,:)

! Remove any halo region.

 i_target_out = i_target-(2*halo)
 j_target_out = j_target-(2*halo)
 
 istart = halo + 1
 jstart = halo + 1
 iend   = i_target - halo
 jend   = j_target - halo

 allocate(lsoil_data(lsoil_target))
 do i = 1, lsoil_target
   lsoil_data(i) = float(i)
 enddo

 allocate(x_data(i_target_out))
 do i = 1, i_target_out
   x_data(i) = float(i)
 enddo

 allocate(y_data(j_target_out))
 do i = 1, j_target_out
   y_data(i) = float(i)
 enddo

 if (convert_nst) then
   print*,'- WRITE FV3 SURFACE AND NST DATA TO NETCDF FILE'
 else
   print*,'- WRITE FV3 SURFACE DATA TO NETCDF FILE'
 endif

 if (localpet == 0) then
   allocate(data_one_tile(i_target,j_target))
   allocate(data_one_tile_3d(i_target,j_target,lsoil_target))
   allocate(idata_one_tile(i_target,j_target))
   allocate(dum2d(i_target_out,j_target_out))
   allocate(dum3d(i_target_out,j_target_out,lsoil_target))
 else
   allocate(data_one_tile(0,0))
   allocate(data_one_tile_3d(0,0,0))
   allocate(idata_one_tile(0,0))
   allocate(dum2d(0,0))
   allocate(dum3d(0,0,0))
 endif

 TILE_LOOP : do tile = 1, num_tiles_target_grid

   LOCAL_PET : if (localpet == 0) then

     WRITE(OUTFILE, '(A, I1, A)'), 'out.sfc.tile', tile, '.nc'

!--- open the file
     error = nf90_create(outfile, IOR(NF90_NETCDF4,NF90_CLASSIC_MODEL), &
                         ncid, initialsize=initial, chunksize=fsize)
     call netcdf_err(error, 'CREATING FILE='//trim(outfile) )

!--- define dimensions
     error = nf90_def_dim(ncid, 'xaxis_1', i_target_out, dim_x)
     call netcdf_err(error, 'DEFINING XAXIS DIMENSION' )
     error = nf90_def_dim(ncid, 'yaxis_1', j_target_out, dim_y)
     call netcdf_err(error, 'DEFINING YAXIS DIMENSION' )
     error = nf90_def_dim(ncid, 'zaxis_1', lsoil_target, dim_lsoil)
     call netcdf_err(error, 'DEFINING ZAXIS DIMENSION' )
     error = nf90_def_dim(ncid, 'Time', 1, dim_time)
     call netcdf_err(error, 'DEFINING TIME DIMENSION' )

 !--- define fields
     error = nf90_def_var(ncid, 'xaxis_1', NF90_FLOAT, (/dim_x/), id_x)
     call netcdf_err(error, 'DEFINING XAXIS_1 FIELD' )
     error = nf90_put_att(ncid, id_x, "long_name", "xaxis_1")
     call netcdf_err(error, 'DEFINING XAXIS_1 LONG NAME' )
     error = nf90_put_att(ncid, id_x, "units", "none")
     call netcdf_err(error, 'DEFINING XAXIS_1 UNITS' )
     error = nf90_put_att(ncid, id_x, "cartesian_axis", "X")
     call netcdf_err(error, 'WRITING XAXIS_1 FIELD' )

     error = nf90_def_var(ncid, 'yaxis_1', NF90_FLOAT, (/dim_y/), id_y)
     call netcdf_err(error, 'DEFINING YAXIS_1 FIELD' )
     error = nf90_put_att(ncid, id_y, "long_name", "yaxis_1")
     call netcdf_err(error, 'DEFINING YAXIS_1 LONG NAME' )
     error = nf90_put_att(ncid, id_y, "units", "none")
     call netcdf_err(error, 'DEFINING YAXIS_1 UNITS' )
     error = nf90_put_att(ncid, id_y, "cartesian_axis", "Y")
     call netcdf_err(error, 'WRITING YAXIS_1 FIELD' )

     error = nf90_def_var(ncid, 'zaxis_1', NF90_FLOAT, (/dim_lsoil/), id_lsoil)
     call netcdf_err(error, 'DEFINING ZAXIS_1 FIELD' )
     error = nf90_put_att(ncid, id_lsoil, "long_name", "zaxis_1")
     call netcdf_err(error, 'DEFINING ZAXIS_1 LONG NAME' )
     error = nf90_put_att(ncid, id_lsoil, "units", "none")
     call netcdf_err(error, 'DEFINING ZAXIS_1 UNITS' )
     error = nf90_put_att(ncid, id_lsoil, "cartesian_axis", "Z")
     call netcdf_err(error, 'WRITING ZAXIS_1 FIELD' )

     error = nf90_def_var(ncid, 'Time', NF90_FLOAT, dim_time, id_time)
     call netcdf_err(error, 'DEFINING TIME FIELD' )
     error = nf90_put_att(ncid, id_time, "long_name", "Time")
     call netcdf_err(error, 'DEFINING TIME LONG NAME' )
     error = nf90_put_att(ncid, id_time, "units", "time level")
     call netcdf_err(error, 'DEFINING TIME UNITS' )
     error = nf90_put_att(ncid, id_time, "cartesian_axis", "T")
     call netcdf_err(error, 'WRITING TIME FIELD' )

     error = nf90_def_var(ncid, 'slmsk', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_slmsk)
     call netcdf_err(error, 'DEFINING SLMSK' )
     error = nf90_put_att(ncid, id_slmsk, "long_name", "slmsk")
     call netcdf_err(error, 'DEFINING SLMSK LONG NAME' )
     error = nf90_put_att(ncid, id_slmsk, "units", "none")
     call netcdf_err(error, 'DEFINING SLMSK UNITS' )

     error = nf90_def_var(ncid, 'tsea', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_tsea)
     call netcdf_err(error, 'DEFINING TSEA' )
     error = nf90_put_att(ncid, id_tsea, "long_name", "tsea")
     call netcdf_err(error, 'DEFINING TSEA LONG NAME' )
     error = nf90_put_att(ncid, id_tsea, "units", "none")
     call netcdf_err(error, 'DEFINING TSEA UNITS' )

     error = nf90_def_var(ncid, 'sheleg', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_sheleg)
     call netcdf_err(error, 'DEFINING SHELEG' )
     error = nf90_put_att(ncid, id_sheleg, "long_name", "sheleg")
     call netcdf_err(error, 'DEFINING SHELEG LONG NAME' )
     error = nf90_put_att(ncid, id_sheleg, "units", "none")
     call netcdf_err(error, 'DEFINING SHELEG UNITS' )

     error = nf90_def_var(ncid, 'tg3', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_tg3)
     call netcdf_err(error, 'DEFINING TG3' )
     error = nf90_put_att(ncid, id_tg3, "long_name", "tg3")
     call netcdf_err(error, 'DEFINING TG3 LONG NAME' )
     error = nf90_put_att(ncid, id_tg3, "units", "none")
     call netcdf_err(error, 'DEFINING TG3 UNITS' )

     error = nf90_def_var(ncid, 'zorl', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_zorl)
     call netcdf_err(error, 'DEFINING ZORL' )
     error = nf90_put_att(ncid, id_zorl, "long_name", "zorl")
     call netcdf_err(error, 'DEFINING ZORL LONG NAME' )
     error = nf90_put_att(ncid, id_zorl, "units", "none")
     call netcdf_err(error, 'DEFINING ZORL UNITS' )

     error = nf90_def_var(ncid, 'alvsf', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_alvsf)
     call netcdf_err(error, 'DEFINING ALVSF' )
     error = nf90_put_att(ncid, id_alvsf, "long_name", "alvsf")
     call netcdf_err(error, 'DEFINING ALVSF LONG NAME' )
     error = nf90_put_att(ncid, id_alvsf, "units", "none")
     call netcdf_err(error, 'DEFINING ALVSF UNITS' )

     error = nf90_def_var(ncid, 'alvwf', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_alvwf)
     call netcdf_err(error, 'DEFINING ALVWF' )
     error = nf90_put_att(ncid, id_alvwf, "long_name", "alvwf")
     call netcdf_err(error, 'DEFINING ALVWF LONG NAME' )
     error = nf90_put_att(ncid, id_alvwf, "units", "none")
     call netcdf_err(error, 'DEFINING ALVWF UNITS' )

     error = nf90_def_var(ncid, 'alnsf', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_alnsf)
     call netcdf_err(error, 'DEFINING ALNSF' )
     error = nf90_put_att(ncid, id_alnsf, "long_name", "alnsf")
     call netcdf_err(error, 'DEFINING ALNSF LONG NAME' )
     error = nf90_put_att(ncid, id_alnsf, "units", "none")
     call netcdf_err(error, 'DEFINING ALNSF UNITS' )

     error = nf90_def_var(ncid, 'alnwf', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_alnwf)
     call netcdf_err(error, 'DEFINING ALNWF' )
     error = nf90_put_att(ncid, id_alnwf, "long_name", "alnwf")
     call netcdf_err(error, 'DEFINING ALNWF LONG NAME' )
     error = nf90_put_att(ncid, id_alnwf, "units", "none")
     call netcdf_err(error, 'DEFINING ALNWF UNITS' )

     error = nf90_def_var(ncid, 'facsf', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_facsf)
     call netcdf_err(error, 'DEFINING FACSF' )
     error = nf90_put_att(ncid, id_facsf, "long_name", "facsf")
     call netcdf_err(error, 'DEFINING FACSF LONG NAME' )
     error = nf90_put_att(ncid, id_facsf, "units", "none")
     call netcdf_err(error, 'DEFINING FACSF UNITS' )

     error = nf90_def_var(ncid, 'facwf', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_facwf)
     call netcdf_err(error, 'DEFINING FACWF' )
     error = nf90_put_att(ncid, id_facwf, "long_name", "facwf")
     call netcdf_err(error, 'DEFINING FACWF LONG NAME' )
     error = nf90_put_att(ncid, id_facwf, "units", "none")
     call netcdf_err(error, 'DEFINING FACWF UNITS' )

     error = nf90_def_var(ncid, 'vfrac', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_vfrac)
     call netcdf_err(error, 'DEFINING VFRAC' )
     error = nf90_put_att(ncid, id_vfrac, "long_name", "vfrac")
     call netcdf_err(error, 'DEFINING VFRAC LONG NAME' )
     error = nf90_put_att(ncid, id_vfrac, "units", "none")
     call netcdf_err(error, 'DEFINING VFRAC UNITS' )

     error = nf90_def_var(ncid, 'canopy', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_canopy)
     call netcdf_err(error, 'DEFINING CANOPY' )
     error = nf90_put_att(ncid, id_canopy, "long_name", "canopy")
     call netcdf_err(error, 'DEFINING CANOPY LONG NAME' )
     error = nf90_put_att(ncid, id_canopy, "units", "none")
     call netcdf_err(error, 'DEFINING CANOPY UNITS' )

     error = nf90_def_var(ncid, 'f10m', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_f10m)
     call netcdf_err(error, 'DEFINING F10M' )
     error = nf90_put_att(ncid, id_f10m, "long_name", "f10m")
     call netcdf_err(error, 'DEFINING F10M LONG NAME' )
     error = nf90_put_att(ncid, id_f10m, "units", "none")
     call netcdf_err(error, 'DEFINING F10M UNITS' )

     error = nf90_def_var(ncid, 't2m', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_t2m)
     call netcdf_err(error, 'DEFINING T2M' )
     error = nf90_put_att(ncid, id_t2m, "long_name", "t2m")
     call netcdf_err(error, 'DEFINING T2M LONG NAME' )
     error = nf90_put_att(ncid, id_t2m, "units", "none")
     call netcdf_err(error, 'DEFINING T2M UNITS' )

     error = nf90_def_var(ncid, 'q2m', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_q2m)
     call netcdf_err(error, 'DEFINING Q2M' )
     error = nf90_put_att(ncid, id_q2m, "long_name", "q2m")
     call netcdf_err(error, 'DEFINING Q2M LONG NAME' )
     error = nf90_put_att(ncid, id_q2m, "units", "none")
     call netcdf_err(error, 'DEFINING Q2M UNITS' )

     error = nf90_def_var(ncid, 'vtype', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_vtype)
     call netcdf_err(error, 'DEFINING VTYPE' )
     error = nf90_put_att(ncid, id_vtype, "long_name", "vtype")
     call netcdf_err(error, 'DEFINING VTYPE LONG NAME' )
     error = nf90_put_att(ncid, id_vtype, "units", "none")
     call netcdf_err(error, 'DEFINING VTYPE UNITS' )

     error = nf90_def_var(ncid, 'stype', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_stype)
     call netcdf_err(error, 'DEFINING STYPE' )
     error = nf90_put_att(ncid, id_stype, "long_name", "stype")
     call netcdf_err(error, 'DEFINING STYPE LONG NAME' )
     error = nf90_put_att(ncid, id_stype, "units", "none")
     call netcdf_err(error, 'DEFINING STYPE UNITS' )

     error = nf90_def_var(ncid, 'uustar', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_uustar)
     call netcdf_err(error, 'DEFINING UUSTAR' )
     error = nf90_put_att(ncid, id_uustar, "long_name", "uustar")
     call netcdf_err(error, 'DEFINING UUSTAR LONG NAME' )
     error = nf90_put_att(ncid, id_uustar, "units", "none")
     call netcdf_err(error, 'DEFINING UUSTAR UNITS' )

     error = nf90_def_var(ncid, 'ffmm', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_ffmm)
     call netcdf_err(error, 'DEFINING FFMM' )
     error = nf90_put_att(ncid, id_ffmm, "long_name", "ffmm")
     call netcdf_err(error, 'DEFINING FFMM LONG NAME' )
     error = nf90_put_att(ncid, id_ffmm, "units", "none")
     call netcdf_err(error, 'DEFINING FFMM UNITS' )

     error = nf90_def_var(ncid, 'ffhh', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_ffhh)
     call netcdf_err(error, 'DEFINING FFHH' )
     error = nf90_put_att(ncid, id_ffhh, "long_name", "ffhh")
     call netcdf_err(error, 'DEFINING FFHH LONG NAME' )
     error = nf90_put_att(ncid, id_ffhh, "units", "none")
     call netcdf_err(error, 'DEFINING FFHH UNITS' )

     error = nf90_def_var(ncid, 'hice', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_hice)
     call netcdf_err(error, 'DEFINING HICE' )
     error = nf90_put_att(ncid, id_hice, "long_name", "hice")
     call netcdf_err(error, 'DEFINING HICE LONG NAME' )
     error = nf90_put_att(ncid, id_hice, "units", "none")
     call netcdf_err(error, 'DEFINING HICE UNITS' )

     error = nf90_def_var(ncid, 'fice', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_fice)
     call netcdf_err(error, 'DEFINING FICE' )
     error = nf90_put_att(ncid, id_fice, "long_name", "fice")
     call netcdf_err(error, 'DEFINING FICE LONG NAME' )
     error = nf90_put_att(ncid, id_fice, "units", "none")
     call netcdf_err(error, 'DEFINING FICE UNITS' )

     error = nf90_def_var(ncid, 'tisfc', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_tisfc)
     call netcdf_err(error, 'DEFINING TISFC' )
     error = nf90_put_att(ncid, id_tisfc, "long_name", "tisfc")
     call netcdf_err(error, 'DEFINING TISFC LONG NAME' )
     error = nf90_put_att(ncid, id_tisfc, "units", "none")
     call netcdf_err(error, 'DEFINING TISFC UNITS' )

     error = nf90_def_var(ncid, 'tprcp', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_tprcp)
     call netcdf_err(error, 'DEFINING TPRCP' )
     error = nf90_put_att(ncid, id_tprcp, "long_name", "tprcp")
     call netcdf_err(error, 'DEFINING TPRCP LONG NAME' )
     error = nf90_put_att(ncid, id_tprcp, "units", "none")
     call netcdf_err(error, 'DEFINING TPRCP UNITS' )

     error = nf90_def_var(ncid, 'srflag', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_srflag)
     call netcdf_err(error, 'DEFINING SRFLAG' )
     error = nf90_put_att(ncid, id_srflag, "long_name", "srflag")
     call netcdf_err(error, 'DEFINING SRFLAG LONG NAME' )
     error = nf90_put_att(ncid, id_srflag, "units", "none")
     call netcdf_err(error, 'DEFINING SRFLAG UNITS' )

     error = nf90_def_var(ncid, 'snwdph', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_snwdph)
     call netcdf_err(error, 'DEFINING SNWDPH' )
     error = nf90_put_att(ncid, id_snwdph, "long_name", "snwdph")
     call netcdf_err(error, 'DEFINING SNWDPH LONG NAME' )
     error = nf90_put_att(ncid, id_snwdph, "units", "none")
     call netcdf_err(error, 'DEFINING SNWDPH UNITS' )

     error = nf90_def_var(ncid, 'shdmin', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_shdmin)
     call netcdf_err(error, 'DEFINING SHDMIN' )
     error = nf90_put_att(ncid, id_shdmin, "long_name", "shdmin")
     call netcdf_err(error, 'DEFINING SHDMIN LONG NAME' )
     error = nf90_put_att(ncid, id_shdmin, "units", "none")
     call netcdf_err(error, 'DEFINING SHDMIN UNITS' )

     error = nf90_def_var(ncid, 'shdmax', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_shdmax)
     call netcdf_err(error, 'DEFINING SHDMAX' )
     error = nf90_put_att(ncid, id_shdmax, "long_name", "shdmax")
     call netcdf_err(error, 'DEFINING SHDMAX LONG NAME' )
     error = nf90_put_att(ncid, id_shdmax, "units", "none")
     call netcdf_err(error, 'DEFINING SHDMAX UNITS' )

     error = nf90_def_var(ncid, 'slope', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_slope)
     call netcdf_err(error, 'DEFINING SLOPE' )
     error = nf90_put_att(ncid, id_slope, "long_name", "slope")
     call netcdf_err(error, 'DEFINING SLOPE LONG NAME' )
     error = nf90_put_att(ncid, id_slope, "units", "none")
     call netcdf_err(error, 'DEFINING SLOPE UNITS' )

     error = nf90_def_var(ncid, 'snoalb', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_snoalb)
     call netcdf_err(error, 'DEFINING SNOALB' )
     error = nf90_put_att(ncid, id_snoalb, "long_name", "snoalb")
     call netcdf_err(error, 'DEFINING SNOALB LONG NAME' )
     error = nf90_put_att(ncid, id_snoalb, "units", "none")
     call netcdf_err(error, 'DEFINING SNOALB UNITS' )

     error = nf90_def_var(ncid, 'stc', NF90_DOUBLE, (/dim_x,dim_y,dim_lsoil,dim_time/), id_stc)
     call netcdf_err(error, 'DEFINING STC' )
     error = nf90_put_att(ncid, id_stc, "long_name", "stc")
     call netcdf_err(error, 'DEFINING STC LONG NAME' )
     error = nf90_put_att(ncid, id_stc, "units", "none")
     call netcdf_err(error, 'DEFINING STC UNITS' )

     error = nf90_def_var(ncid, 'smc', NF90_DOUBLE, (/dim_x,dim_y,dim_lsoil,dim_time/), id_smc)
     call netcdf_err(error, 'DEFINING SMC' )
     error = nf90_put_att(ncid, id_smc, "long_name", "smc")
     call netcdf_err(error, 'DEFINING SMC LONG NAME' )
     error = nf90_put_att(ncid, id_smc, "units", "none")
     call netcdf_err(error, 'DEFINING SMC UNITS' )

     error = nf90_def_var(ncid, 'slc', NF90_DOUBLE, (/dim_x,dim_y,dim_lsoil,dim_time/), id_slc)
     call netcdf_err(error, 'DEFINING SLC' )
     error = nf90_put_att(ncid, id_slc, "long_name", "slc")
     call netcdf_err(error, 'DEFINING SLC LONG NAME' )
     error = nf90_put_att(ncid, id_slc, "units", "none")
     call netcdf_err(error, 'DEFINING SLC UNITS' )

     if (convert_nst) then

       error = nf90_def_var(ncid, 'tref', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_tref)
       call netcdf_err(error, 'DEFINING TREF' )
       error = nf90_put_att(ncid, id_tref, "long_name", "tref")
       call netcdf_err(error, 'DEFINING TREF LONG NAME' )
       error = nf90_put_att(ncid, id_tref, "units", "none")
       call netcdf_err(error, 'DEFINING TREF UNITS' )

       error = nf90_def_var(ncid, 'z_c', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_z_c)
       call netcdf_err(error, 'DEFINING Z_C' )
       error = nf90_put_att(ncid, id_z_c, "long_name", "z_c")
       call netcdf_err(error, 'DEFINING Z_C LONG NAME' )
       error = nf90_put_att(ncid, id_z_c, "units", "none")
       call netcdf_err(error, 'DEFINING Z_C UNITS' )

       error = nf90_def_var(ncid, 'c_0', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_c_0)
       call netcdf_err(error, 'DEFINING C_0' )
       error = nf90_put_att(ncid, id_c_0, "long_name", "c_0")
       call netcdf_err(error, 'DEFINING C_0 LONG NAME' )
       error = nf90_put_att(ncid, id_c_0, "units", "none")
       call netcdf_err(error, 'DEFINING C_0 UNITS' )

       error = nf90_def_var(ncid, 'c_d', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_c_d)
       call netcdf_err(error, 'DEFINING C_D' )
       error = nf90_put_att(ncid, id_c_d, "long_name", "c_d")
       call netcdf_err(error, 'DEFINING C_D LONG NAME' )
       error = nf90_put_att(ncid, id_c_d, "units", "none")
       call netcdf_err(error, 'DEFINING C_D UNITS' )

       error = nf90_def_var(ncid, 'w_0', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_w_0)
       call netcdf_err(error, 'DEFINING W_0' )
       error = nf90_put_att(ncid, id_w_0, "long_name", "w_0")
       call netcdf_err(error, 'DEFINING W_0 LONG NAME' )
       error = nf90_put_att(ncid, id_w_0, "units", "none")
       call netcdf_err(error, 'DEFINING W_0 UNITS' )

       error = nf90_def_var(ncid, 'w_d', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_w_d)
       call netcdf_err(error, 'DEFINING W_D' )
       error = nf90_put_att(ncid, id_w_d, "long_name", "w_d")
       call netcdf_err(error, 'DEFINING W_D LONG NAME' )
       error = nf90_put_att(ncid, id_w_d, "units", "none")
       call netcdf_err(error, 'DEFINING W_D UNITS' )

       error = nf90_def_var(ncid, 'xt', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_xt)
       call netcdf_err(error, 'DEFINING XT' )
       error = nf90_put_att(ncid, id_xt, "long_name", "xt")
       call netcdf_err(error, 'DEFINING XT LONG NAME' )
       error = nf90_put_att(ncid, id_xt, "units", "none")
       call netcdf_err(error, 'DEFINING XT UNITS' )

       error = nf90_def_var(ncid, 'xs', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_xs)
       call netcdf_err(error, 'DEFINING XS' )
       error = nf90_put_att(ncid, id_xs, "long_name", "xs")
       call netcdf_err(error, 'DEFINING XS LONG NAME' )
       error = nf90_put_att(ncid, id_xs, "units", "none")
       call netcdf_err(error, 'DEFINING XS UNITS' )

       error = nf90_def_var(ncid, 'xu', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_xu)
       call netcdf_err(error, 'DEFINING XU' )
       error = nf90_put_att(ncid, id_xu, "long_name", "xu")
       call netcdf_err(error, 'DEFINING XU LONG NAME' )
       error = nf90_put_att(ncid, id_xu, "units", "none")
       call netcdf_err(error, 'DEFINING XU UNITS' )

       error = nf90_def_var(ncid, 'xv', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_xv)
       call netcdf_err(error, 'DEFINING XV' )
       error = nf90_put_att(ncid, id_xv, "long_name", "xv")
       call netcdf_err(error, 'DEFINING XV LONG NAME' )
       error = nf90_put_att(ncid, id_xv, "units", "none")
       call netcdf_err(error, 'DEFINING XV UNITS' )

       error = nf90_def_var(ncid, 'xz', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_xz)
       call netcdf_err(error, 'DEFINING XZ' )
       error = nf90_put_att(ncid, id_xz, "long_name", "xz")
       call netcdf_err(error, 'DEFINING XZ LONG NAME' )
       error = nf90_put_att(ncid, id_xz, "units", "none")
       call netcdf_err(error, 'DEFINING XZ UNITS' )

       error = nf90_def_var(ncid, 'zm', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_zm)
       call netcdf_err(error, 'DEFINING ZM' )
       error = nf90_put_att(ncid, id_zm, "long_name", "zm")
       call netcdf_err(error, 'DEFINING ZM LONG NAME' )
       error = nf90_put_att(ncid, id_zm, "units", "none")
       call netcdf_err(error, 'DEFINING ZM UNITS' )

       error = nf90_def_var(ncid, 'xtts', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_xtts)
       call netcdf_err(error, 'DEFINING XTTS' )
       error = nf90_put_att(ncid, id_xtts, "long_name", "xtts")
       call netcdf_err(error, 'DEFINING XTTS LONG NAME' )
       error = nf90_put_att(ncid, id_xtts, "units", "none")
       call netcdf_err(error, 'DEFINING XTTS UNITS' )

       error = nf90_def_var(ncid, 'xzts', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_xzts)
       call netcdf_err(error, 'DEFINING XZTS' )
       error = nf90_put_att(ncid, id_xzts, "long_name", "xzts")
       call netcdf_err(error, 'DEFINING XZTS LONG NAME' )
       error = nf90_put_att(ncid, id_xzts, "units", "none")
       call netcdf_err(error, 'DEFINING XZTS UNITS' )

       error = nf90_def_var(ncid, 'd_conv', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_d_conv)
       call netcdf_err(error, 'DEFINING D_CONV' )
       error = nf90_put_att(ncid, id_d_conv, "long_name", "d_conv")
       call netcdf_err(error, 'DEFINING D_CONV LONG NAME' )
       error = nf90_put_att(ncid, id_d_conv, "units", "none")
       call netcdf_err(error, 'DEFINING D_CONV UNITS' )

       error = nf90_def_var(ncid, 'ifd', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_ifd)
       call netcdf_err(error, 'DEFINING IFD' )
       error = nf90_put_att(ncid, id_ifd, "long_name", "ifd")
       call netcdf_err(error, 'DEFINING IFD LONG NAME' )
       error = nf90_put_att(ncid, id_ifd, "units", "none")
       call netcdf_err(error, 'DEFINING IFD UNITS' )

       error = nf90_def_var(ncid, 'dt_cool', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_dt_cool)
       call netcdf_err(error, 'DEFINING DT_COOL' )
       error = nf90_put_att(ncid, id_dt_cool, "long_name", "dt_cool")
       call netcdf_err(error, 'DEFINING DT_COOL LONG NAME' )
       error = nf90_put_att(ncid, id_dt_cool, "units", "none")
       call netcdf_err(error, 'DEFINING DT_COOL UNITS' )

       error = nf90_def_var(ncid, 'qrain', NF90_DOUBLE, (/dim_x,dim_y,dim_time/), id_qrain)
       call netcdf_err(error, 'DEFINING QRAIN' )
       error = nf90_put_att(ncid, id_qrain, "long_name", "qrain")
       call netcdf_err(error, 'DEFINING QRAIN LONG NAME' )
       error = nf90_put_att(ncid, id_qrain, "units", "none")
       call netcdf_err(error, 'DEFINING QRAIN UNITS' )

     endif  ! nsst records

     error = nf90_enddef(ncid, header_buffer_val,4,0,4)
     call netcdf_err(error, 'DEFINING HEADER' )

   endif LOCAL_PET ! is localpet 0?

   if (localpet == 0) then
     error = nf90_put_var( ncid, id_lsoil, lsoil_data)
     call netcdf_err(error, 'WRITING ZAXIS RECORD' )
     error = nf90_put_var( ncid, id_x, x_data)
     call netcdf_err(error, 'WRITING XAXIS RECORD' )
     error = nf90_put_var( ncid, id_y, y_data)
     call netcdf_err(error, 'WRITING YAXIS RECORD' )
     times = 1.0
     error = nf90_put_var( ncid, id_time, times)
     call netcdf_err(error, 'WRITING TIME RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID SNOW LIQ EQUIV FOR TILE: ", tile
   call ESMF_FieldGather(snow_liq_equiv_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_sheleg, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING SNOW LIQ EQUIV RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID SNOW DEPTH FOR TILE: ", tile
   call ESMF_FieldGather(snow_depth_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_snwdph, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING SNWDPH RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID SLOPE TYPE FOR TILE: ", tile
   call ESMF_FieldGather(slope_type_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_slope, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING SLOPE RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID Z0 FOR TILE: ", tile
   call ESMF_FieldGather(z0_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_zorl, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING Z0 RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID MAX SNOW ALBEDO FOR TILE: ", tile
   call ESMF_FieldGather(mxsno_albedo_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_snoalb, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING MAX SNOW ALBEDO RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID SOIL TYPE FOR TILE: ", tile
   call ESMF_FieldGather(soil_type_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_stype, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING SOIL TYPE RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID VEGETATION TYPE FOR TILE: ", tile
   call ESMF_FieldGather(veg_type_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_vtype, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING VEGETATION TYPE RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID VEGETATION GREENNESS FOR TILE: ", tile
   call ESMF_FieldGather(veg_greenness_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_vfrac, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING VEGETATION GREENNESS RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID SUBSTRATE TEMPERATURE FOR TILE: ", tile
   call ESMF_FieldGather(substrate_temp_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_tg3, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING SUBSTRATE TEMPERATURE RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID FACSF FOR TILE: ", tile
   call ESMF_FieldGather(facsf_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_facsf, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING FACSF RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID FACWF FOR TILE: ", tile
   call ESMF_FieldGather(facwf_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_facwf, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING FACWF RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID ALNSF FOR TILE: ", tile
   call ESMF_FieldGather(alnsf_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_alnsf, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING ALNSF RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID ALNWF FOR TILE: ", tile
   call ESMF_FieldGather(alnwf_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_alnwf, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING ALNWF RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID ALVSF FOR TILE: ", tile
   call ESMF_FieldGather(alvsf_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_alvsf, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING ALVSF RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID ALVWF FOR TILE: ", tile
   call ESMF_FieldGather(alvwf_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_alvwf, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING ALVWF RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID MAX VEGETATION GREENNESS FOR TILE: ", tile
   call ESMF_FieldGather(max_veg_greenness_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_shdmax, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING MAX VEGETATION GREENNESS RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID MIN VEGETATION GREENNESS FOR TILE: ", tile
   call ESMF_FieldGather(min_veg_greenness_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_shdmin, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING MIN VEGETATION GREENNESS RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID T2M FOR TILE: ", tile
   call ESMF_FieldGather(t2m_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_t2m, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING T2M RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID Q2M FOR TILE: ", tile
   call ESMF_FieldGather(q2m_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_q2m, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING Q2M RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID TPRCP FOR TILE: ", tile
   call ESMF_FieldGather(tprcp_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_tprcp, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING TPRCP RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID F10M FOR TILE: ", tile
   call ESMF_FieldGather(f10m_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_f10m, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING F10M RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID FFMM FOR TILE: ", tile
   call ESMF_FieldGather(ffmm_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_ffmm, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING FFMM RECORD' )
     dum2d = 0.0
     error = nf90_put_var( ncid, id_ffhh, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING FFHH RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID USTAR FOR TILE: ", tile
   call ESMF_FieldGather(ustar_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_uustar, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING USTAR RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID SRFLAG FOR TILE: ", tile
   call ESMF_FieldGather(srflag_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_srflag, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING SRFLAG RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID SEA ICE FRACTION FOR TILE: ", tile
   call ESMF_FieldGather(seaice_fract_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_fice, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING FICE RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID SEA ICE DEPTH FOR TILE: ", tile
   call ESMF_FieldGather(seaice_depth_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_hice, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING HICE RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID SEA ICE SKIN TEMP FOR TILE: ", tile
   call ESMF_FieldGather(seaice_skin_temp_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_tisfc, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING TISFC RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID SKIN TEMP FOR TILE: ", tile
   call ESMF_FieldGather(skin_temp_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_tsea, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING TSEA RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID LANDMASK FOR TILE: ", tile
   call ESMF_FieldGather(landmask_target_grid, idata_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = float(idata_one_tile(istart:iend, jstart:jend))
     error = nf90_put_var( ncid, id_slmsk, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING LANDMASK RECORD' )
   endif

   print*,"- CALL FieldGather FOR TARGET GRID CANOPY MOISTURE CONTENT FOR TILE: ", tile
   call ESMF_FieldGather(canopy_mc_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
     error = nf90_put_var( ncid, id_canopy, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
     call netcdf_err(error, 'WRITING CANOPY MC RECORD' )
   endif

! soil temperature 

   print*,"- CALL FieldGather FOR TARGET GRID SOIL TEMPERATURE FOR TILE: ", tile
   call ESMF_FieldGather(soil_temp_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum3d(:,:,:) = data_one_tile_3d(istart:iend, jstart:jend,:)
     error = nf90_put_var( ncid, id_stc, dum3d, start=(/1,1,1,1/), count=(/i_target_out,j_target_out,lsoil_target,1/))
     call netcdf_err(error, 'WRITING SOIL TEMP RECORD' )
   endif

! soil moisture (total)

   print*,"- CALL FieldGather FOR TARGET GRID TOTAL SOIL MOISTURE FOR TILE: ", tile
   call ESMF_FieldGather(soilm_tot_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum3d(:,:,:) = data_one_tile_3d(istart:iend, jstart:jend,:)
     error = nf90_put_var( ncid, id_smc, dum3d, start=(/1,1,1,1/), count=(/i_target_out,j_target_out,lsoil_target,1/))
     call netcdf_err(error, 'WRITING TOTAL SOIL MOISTURE RECORD' )
   endif

! soil moisture (liquid)

   print*,"- CALL FieldGather FOR TARGET GRID LIQUID SOIL MOISTURE FOR TILE: ", tile
   call ESMF_FieldGather(soilm_liq_target_grid, data_one_tile_3d, rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
      call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     dum3d(:,:,:) = data_one_tile_3d(istart:iend, jstart:jend,:)
     error = nf90_put_var( ncid, id_slc, dum3d, start=(/1,1,1,1/), count=(/i_target_out,j_target_out,lsoil_target,1/))
     call netcdf_err(error, 'WRITING LIQUID SOIL MOISTURE RECORD' )
   endif

   if (convert_nst) then

     print*,"- CALL FieldGather FOR TARGET C_D FOR TILE: ", tile
     call ESMF_FieldGather(c_d_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_c_d, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING C_D RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET C_0 FOR TILE: ", tile
     call ESMF_FieldGather(c_0_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_c_0, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING C_0 RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET D_CONV FOR TILE: ", tile
     call ESMF_FieldGather(d_conv_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_d_conv, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING D_CONV RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET DT_COOL FOR TILE: ", tile
     call ESMF_FieldGather(dt_cool_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_dt_cool, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING DT_COOL RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET IFD FOR TILE: ", tile
     call ESMF_FieldGather(ifd_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_ifd, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING IFD RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET QRAIN FOR TILE: ", tile
     call ESMF_FieldGather(qrain_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_qrain, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING QRAIN RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET TREF FOR TILE: ", tile
     call ESMF_FieldGather(tref_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_tref, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING TREF RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET W_D FOR TILE: ", tile
     call ESMF_FieldGather(w_d_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_w_d, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING W_D RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET W_0 FOR TILE: ", tile
     call ESMF_FieldGather(w_0_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_w_0, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING W_0 RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET XS FOR TILE: ", tile
     call ESMF_FieldGather(xs_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_xs, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING XS RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET XT FOR TILE: ", tile
     call ESMF_FieldGather(xt_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_xt, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING XT RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET XU FOR TILE: ", tile
     call ESMF_FieldGather(xu_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_xu, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING XU RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET XV FOR TILE: ", tile
     call ESMF_FieldGather(xv_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_xv, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING XV RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET XZ FOR TILE: ", tile
     call ESMF_FieldGather(xz_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_xz, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING XZ RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET XTTS FOR TILE: ", tile
     call ESMF_FieldGather(xtts_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_xtts, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING XTTS RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET XZTS FOR TILE: ", tile
     call ESMF_FieldGather(xzts_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_xzts, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING XZTS RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET Z_C FOR TILE: ", tile
     call ESMF_FieldGather(z_c_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_z_c, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING Z_C RECORD' )
     endif

     print*,"- CALL FieldGather FOR TARGET ZM FOR TILE: ", tile
     call ESMF_FieldGather(zm_target_grid, data_one_tile, rootPet=0, tile=tile, rc=error)
     if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
        call error_handler("IN FieldGather", error)

     if (localpet == 0) then
       dum2d(:,:) = data_one_tile(istart:iend, jstart:jend)
       error = nf90_put_var( ncid, id_zm, dum2d, start=(/1,1,1/), count=(/i_target_out,j_target_out,1/))
       call netcdf_err(error, 'WRITING ZM RECORD' )
     endif

   endif ! convert nst

!-------------------------------------------------------------------------------
! close file
!-------------------------------------------------------------------------------

   error = nf90_close(ncid)

 enddo TILE_LOOP

 deallocate(lsoil_data, x_data, y_data)
 deallocate(data_one_tile, data_one_tile_3d, idata_one_tile, dum2d, dum3d)

 return

 end subroutine write_fv3_sfc_data_netcdf
