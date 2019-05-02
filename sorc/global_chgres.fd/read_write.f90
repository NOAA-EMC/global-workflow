 SUBROUTINE WRITE_FV3_ATMS_HEADER_NETCDF(LEVS_P1, NTRACM, NVCOORD, VCOORD)

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: LEVS_P1
 INTEGER, INTENT(IN) :: NTRACM
 INTEGER, INTENT(IN) :: NVCOORD

 REAL, INTENT(IN)    :: VCOORD(LEVS_P1, NVCOORD)

 CHARACTER(LEN=13)   :: OUTFILE

 INTEGER             :: ERROR, NCID
 INTEGER             :: DIM_NVCOORD, DIM_LEVSP
 INTEGER             :: ID_NTRAC, ID_VCOORD
 INTEGER             :: FSIZE=65536, INITAL = 0
 INTEGER             :: HEADER_BUFFER_VAL = 16384

 REAL(KIND=8)        :: TMP(LEVS_P1,NVCOORD)

 include "netcdf.inc"

 OUTFILE = "./gfs_ctrl.nc"

 ERROR = NF__CREATE(OUTFILE, IOR(NF_NETCDF4,NF_CLASSIC_MODEL), INITAL, FSIZE, NCID)
 CALL NETCDF_ERROR(ERROR, 'Creating file '//TRIM(OUTFILE) )

 ERROR = NF_DEF_DIM(NCID, 'nvcoord', NVCOORD, DIM_NVCOORD)
 CALL NETCDF_ERROR(ERROR, 'define dimension nvcoord for file='//TRIM(OUTFILE) )

 ERROR = NF_DEF_DIM(NCID, 'levsp', LEVS_P1, DIM_LEVSP)
 CALL NETCDF_ERROR(ERROR, 'define dimension levsp for file='//TRIM(OUTFILE) )

 ERROR = NF_DEF_VAR(NCID, 'ntrac', NF_INT, 0, (/0/), ID_NTRAC)
 CALL NETCDF_ERROR(ERROR, 'define var ntrac for file='//TRIM(OUTFILE) )

 ERROR = NF_DEF_VAR(NCID, 'vcoord', NF_DOUBLE, 2, (/DIM_LEVSP, DIM_NVCOORD/), ID_VCOORD)
 CALL NETCDF_ERROR(ERROR, 'define var vcoord for file='//TRIM(OUTFILE) )   

 ERROR = NF__ENDDEF(NCID, HEADER_BUFFER_VAL,4,0,4)
 CALL NETCDF_ERROR(ERROR, 'end meta define for file='//TRIM(OUTFILE) )

 ERROR = NF_PUT_VAR_INT( NCID, ID_NTRAC, NTRACM)
 CALL NETCDF_ERROR(ERROR, 'write var ntrac for file='//TRIM(OUTFILE) )

 TMP(1:LEVS_P1,:) = VCOORD(LEVS_P1:1:-1,:)
 ERROR = NF_PUT_VAR_DOUBLE( NCID, ID_VCOORD, TMP)
 CALL NETCDF_ERROR(ERROR, 'write var vcoord for file='//TRIM(OUTFILE) )

 ERROR = NF_CLOSE(NCID)

 END SUBROUTINE WRITE_FV3_ATMS_HEADER_NETCDF

 subroutine netcdf_error( err, string )
 implicit none
 integer, intent(in) :: err
 character(len=*), intent(in) :: string
 character(len=256) :: errmsg
 include "netcdf.inc"

 if( err.EQ.NF_NOERR )return
 errmsg = NF_STRERROR(err)
 print*,''
 print*,'FATAL ERROR: ', trim(string), ': ', trim(errmsg)
 print*,'STOP.'
 call errexit(999)

 return
 end subroutine netcdf_error

 subroutine write_fv3_sfc_data_netcdf(lonb, latb, lsoil, sfcoutput, f10m, &
                           t2m, q2m, uustar, ffmm, ffhh, tprcp, srflag, tile, &
                           num_nsst_fields, nsst_output)

 use surface_chgres, only        : sfc1d

 implicit none

 integer, intent(in)            :: latb, lonb, lsoil, tile
 integer, intent(in)            :: num_nsst_fields
 character(len=128)             :: outfile

 integer                        :: fsize=65536, inital = 0
 integer                        :: header_buffer_val = 16384
 integer                        :: dim_lon, dim_lat, dim_lsoil
 integer                        :: error, ncid, i
 integer                        :: id_lon, id_lat, id_lsoil
 integer                        :: id_geolon, id_geolat, id_slmsk
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
 
 logical                        :: write_nsst

 real, intent(in)               :: f10m(lonb,latb)
 real, intent(in)               :: q2m(lonb,latb)
 real, intent(in)               :: t2m(lonb,latb)
 real, intent(in)               :: uustar(lonb,latb)
 real, intent(in)               :: ffmm(lonb,latb)
 real, intent(in)               :: ffhh(lonb,latb)
 real, intent(in)               :: tprcp(lonb,latb)
 real, intent(in)               :: srflag(lonb,latb)
 real, intent(in), optional     :: nsst_output(lonb*latb,num_nsst_fields)
 real(kind=4)                   :: lsoil_data(lsoil)
 real(kind=4), allocatable      :: dum2d(:,:), dum3d(:,:,:)

 type(sfc1d)                    :: sfcoutput

 include "netcdf.inc"

 write_nsst = .false.
 if (present(nsst_output)) write_nsst = .true.

 if (write_nsst) then
   print*,'- WRITE FV3 SURFACE AND NSST DATA TO NETCDF FILE'
 else
   print*,'- WRITE FV3 SURFACE DATA TO NETCDF FILE'
 endif

 if (tile < 10) then
   write(outfile, '(A, I1, A)'), 'out.sfc.tile', tile, '.nc'
 else
   write(outfile, '(A, I2, A)'), 'out.sfc.tile', tile, '.nc'
 endif

!--- open the file
 error = NF__CREATE(outfile, IOR(NF_NETCDF4,NF_CLASSIC_MODEL), inital, fsize, ncid)
 call netcdf_error(error, 'CREATING FILE='//trim(outfile) )

!--- define dimension
 error = nf_def_dim(ncid, 'lon', lonb, dim_lon)
 call netcdf_error(error, 'DEFINING LON DIMENSION' )
 error = nf_def_dim(ncid, 'lat', latb, dim_lat)
 call netcdf_error(error, 'DEFINING LAT DIMENSION' )
 error = nf_def_dim(ncid, 'lsoil', lsoil, dim_lsoil)
 call netcdf_error(error, 'DEFINING LSOIL DIMENSION' )

 !--- define field
 error = nf_def_var(ncid, 'lon', NF_FLOAT, 1, (/dim_lon/), id_lon)
 call netcdf_error(error, 'DEFINING LON FIELD' )
 error = nf_put_att_text(ncid, id_lon, "cartesian_axis", 1, "X")
 call netcdf_error(error, 'WRITING LON FIELD' )
 error = nf_def_var(ncid, 'lat', NF_FLOAT, 1, (/dim_lat/), id_lat)
 call netcdf_error(error, 'DEFINING LAT FIELD' )
 error = nf_put_att_text(ncid, id_lat, "cartesian_axis", 1, "Y")
 call netcdf_error(error, 'WRITING LAT FIELD' )
 error = nf_def_var(ncid, 'lsoil', NF_FLOAT, 1, (/dim_lsoil/), id_lsoil)
 call netcdf_error(error, 'DEFINING LSOIL FIELD' )
 error = nf_put_att_text(ncid, id_lsoil, "cartesian_axis", 1, "Z")
 call netcdf_error(error, 'WRITING LSOIL FIELD' )
 error = nf_def_var(ncid, 'geolon', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_geolon)
 call netcdf_error(error, 'DEFINING GEOLON' )
 error = nf_def_var(ncid, 'geolat', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_geolat)
 call netcdf_error(error, 'DEFINING GEOLAT' )
 error = nf_def_var(ncid, 'slmsk', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_slmsk)
 call netcdf_error(error, 'DEFINING SLMSK' )
 error = nf_def_var(ncid, 'tsea', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_tsea)
 call netcdf_error(error, 'DEFINING TSEA' )
 error = nf_def_var(ncid, 'sheleg', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_sheleg)
 call netcdf_error(error, 'DEFINING SHELEG' )
 error = nf_def_var(ncid, 'tg3', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_tg3)
 call netcdf_error(error, 'DEFINING TG3' )
 error = nf_def_var(ncid, 'zorl', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_zorl)
 call netcdf_error(error, 'DEFINING ZORL' )
 error = nf_def_var(ncid, 'alvsf', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_alvsf)
 call netcdf_error(error, 'DEFINING ALVSF' )
 error = nf_def_var(ncid, 'alvwf', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_alvwf)
 call netcdf_error(error, 'DEFINING ALVWF' )
 error = nf_def_var(ncid, 'alnsf', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_alnsf)
 call netcdf_error(error, 'DEFINING ALNSF' )
 error = nf_def_var(ncid, 'alnwf', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_alnwf)
 call netcdf_error(error, 'DEFINING ALNWF' )
 error = nf_def_var(ncid, 'vfrac', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_vfrac)
 call netcdf_error(error, 'DEFINING VFRAC' )
 error = nf_def_var(ncid, 'canopy', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_canopy)
 call netcdf_error(error, 'DEFINING CANOPY' )
 error = nf_def_var(ncid, 'f10m', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_f10m)
 call netcdf_error(error, 'DEFINING F10M' )
 error = nf_def_var(ncid, 't2m', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_t2m)
 call netcdf_error(error, 'DEFINING T2M' )
 error = nf_def_var(ncid, 'q2m', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_q2m)
 call netcdf_error(error, 'DEFINING Q2M' )
 error = nf_def_var(ncid, 'vtype', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_vtype)
 call netcdf_error(error, 'DEFINING VTYPE' )
 error = nf_def_var(ncid, 'stype', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_stype)
 call netcdf_error(error, 'DEFINING STYPE' )
 error = nf_def_var(ncid, 'facsf', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_facsf)
 call netcdf_error(error, 'DEFINING FACSF' )
 error = nf_def_var(ncid, 'facwf', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_facwf)
 call netcdf_error(error, 'DEFINING FACWF' )
 error = nf_def_var(ncid, 'uustar', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_uustar)
 call netcdf_error(error, 'DEFINING UUSTAR' )
 error = nf_def_var(ncid, 'ffmm', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_ffmm)
 call netcdf_error(error, 'DEFINING FFMM' )
 error = nf_def_var(ncid, 'ffhh', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_ffhh)
 call netcdf_error(error, 'DEFINING FFHH' )
 error = nf_def_var(ncid, 'hice', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_hice)
 call netcdf_error(error, 'DEFINING HICE' )
 error = nf_def_var(ncid, 'fice', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_fice)
 call netcdf_error(error, 'DEFINING FICE' )
 error = nf_def_var(ncid, 'tisfc', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_tisfc)
 call netcdf_error(error, 'DEFINING TISFC' )
 error = nf_def_var(ncid, 'tprcp', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_tprcp)
 call netcdf_error(error, 'DEFINING TPRCP' )
 error = nf_def_var(ncid, 'srflag', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_srflag)
 call netcdf_error(error, 'DEFINING SRFLAG' )
 error = nf_def_var(ncid, 'snwdph', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_snwdph)
 call netcdf_error(error, 'DEFINING SNWDPH' )
 error = nf_def_var(ncid, 'shdmin', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_shdmin)
 call netcdf_error(error, 'DEFINING SHDMIN' )
 error = nf_def_var(ncid, 'shdmax', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_shdmax)
 call netcdf_error(error, 'DEFINING SHDMAX' )
 error = nf_def_var(ncid, 'slope', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_slope)
 call netcdf_error(error, 'DEFINING SLOPE' )
 error = nf_def_var(ncid, 'snoalb', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_snoalb)
 call netcdf_error(error, 'DEFINING SNOALB' )
 error = nf_def_var(ncid, 'stc', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_lsoil/), id_stc)
 call netcdf_error(error, 'DEFINING STC' )
 error = nf_def_var(ncid, 'smc', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_lsoil/), id_smc)
 call netcdf_error(error, 'DEFINING SMC' )
 error = nf_def_var(ncid, 'slc', NF_FLOAT, 3, (/dim_lon,dim_lat,dim_lsoil/), id_slc)
 call netcdf_error(error, 'DEFINING SLC' )
 if (write_nsst) then
   error = nf_def_var(ncid, 'tref', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_tref)
   call netcdf_error(error, 'DEFINING TREF' )
   error = nf_def_var(ncid, 'z_c', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_z_c)
   call netcdf_error(error, 'DEFINING Z_C' )
   error = nf_def_var(ncid, 'c_0', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_c_0)
   call netcdf_error(error, 'DEFINING C_0' )
   error = nf_def_var(ncid, 'c_d', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_c_d)
   call netcdf_error(error, 'DEFINING C_D' )
   error = nf_def_var(ncid, 'w_0', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_w_0)
   call netcdf_error(error, 'DEFINING W_0' )
   error = nf_def_var(ncid, 'w_d', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_w_d)
   call netcdf_error(error, 'DEFINING W_D' )
   error = nf_def_var(ncid, 'xt', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_xt)
   call netcdf_error(error, 'DEFINING XT' )
   error = nf_def_var(ncid, 'xs', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_xs)
   call netcdf_error(error, 'DEFINING XS' )
   error = nf_def_var(ncid, 'xu', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_xu)
   call netcdf_error(error, 'DEFINING XU' )
   error = nf_def_var(ncid, 'xv', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_xv)
   call netcdf_error(error, 'DEFINING XV' )
   error = nf_def_var(ncid, 'xz', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_xz)
   call netcdf_error(error, 'DEFINING XZ' )
   error = nf_def_var(ncid, 'zm', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_zm)
   call netcdf_error(error, 'DEFINING ZM' )
   error = nf_def_var(ncid, 'xtts', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_xtts)
   call netcdf_error(error, 'DEFINING XTTS' )
   error = nf_def_var(ncid, 'xzts', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_xzts)
   call netcdf_error(error, 'DEFINING XZTS' )
   error = nf_def_var(ncid, 'd_conv', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_d_conv)
   call netcdf_error(error, 'DEFINING D_CONV' )
   error = nf_def_var(ncid, 'ifd', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_ifd)
   call netcdf_error(error, 'DEFINING IFD' )
   error = nf_def_var(ncid, 'dt_cool', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_dt_cool)
   call netcdf_error(error, 'DEFINING DT_COOL' )
   error = nf_def_var(ncid, 'qrain', NF_FLOAT, 2, (/dim_lon,dim_lat/), id_qrain)
   call netcdf_error(error, 'DEFINING QRAIN' )
 endif

 error = nf__enddef(ncid, header_buffer_val,4,0,4)
 call netcdf_error(error, 'DEFINING HEADER' )

 allocate(dum2d(lonb,latb))

 dum2d = reshape(sfcoutput%lons, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_lon, dum2d(:,1))
 call netcdf_error(error, 'WRITING LON HEADER RECORD' )

 dum2d = reshape(sfcoutput%lats, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_lat, dum2d(1,:))
 call netcdf_error(error, 'WRITING LAT HEADER RECORD' )

 do i = 1, lsoil
   lsoil_data(i) = float(i)
 enddo
 error = nf_put_var_real( ncid, id_lsoil, lsoil_data)
 call netcdf_error(error, 'WRITING LSOIL HEADER' )

 dum2d = reshape(sfcoutput%lons, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_geolon, dum2d)
 call netcdf_error(error, 'WRITING GEOLON RECORD' )

 dum2d = reshape(sfcoutput%lats, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_geolat, dum2d)
 call netcdf_error(error, 'WRITING GEOLAT RECORD' )

 dum2d = reshape(sfcoutput%lsmask, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_slmsk, dum2d)
 call netcdf_error(error, 'WRITING SLMSK RECORD' )

 dum2d = reshape(sfcoutput%skin_temp, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_tsea, dum2d)
 call netcdf_error(error, 'WRITING TSEA RECORD' )

 dum2d = reshape(sfcoutput%snow_liq_equiv, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_sheleg, dum2d)
 call netcdf_error(error, 'WRITING SHELEG RECORD' )

 dum2d = reshape(sfcoutput%substrate_temp, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_tg3, dum2d)
 call netcdf_error(error, 'WRITING TG3 RECORD' )

 dum2d = reshape(sfcoutput%z0, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_zorl, dum2d)
 call netcdf_error(error, 'WRITING ZORL RECORD' )

 dum2d = reshape(sfcoutput%alvsf, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_alvsf, dum2d)
 call netcdf_error(error, 'WRITING ALVSF RECORD' )

 dum2d = reshape(sfcoutput%alvwf, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_alvwf, dum2d)
 call netcdf_error(error, 'WRITING ALVWF RECORD' )

 dum2d = reshape(sfcoutput%alnsf, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_alnsf, dum2d)
 call netcdf_error(error, 'WRITING ALNSF RECORD' )

 dum2d = reshape(sfcoutput%alnwf, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_alnwf, dum2d)
 call netcdf_error(error, 'WRITING ALNWF RECORD' )

 dum2d = reshape(sfcoutput%greenfrc, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_vfrac, dum2d)
 call netcdf_error(error, 'WRITING VFRAC RECORD' )

 dum2d = reshape(sfcoutput%canopy_mc, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_canopy, dum2d)
 call netcdf_error(error, 'WRITING CANOPY RECORD' )

 dum2d = f10m
 error = nf_put_var_real( ncid, id_f10m, dum2d)
 call netcdf_error(error, 'WRITING F10M RECORD' )

 dum2d = t2m
 error = nf_put_var_real( ncid, id_t2m, dum2d)
 call netcdf_error(error, 'WRITING T2M RECORD' )

 dum2d = q2m
 error = nf_put_var_real( ncid, id_q2m, dum2d)
 call netcdf_error(error, 'WRITING Q2M RECORD' )

 dum2d = reshape(float(sfcoutput%veg_type), (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_vtype, dum2d)
 call netcdf_error(error, 'WRITING VTYPE RECORD' )

 dum2d = reshape(float(sfcoutput%soil_type), (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_stype, dum2d)
 call netcdf_error(error, 'WRITING STYPE RECORD' )

 dum2d = reshape(sfcoutput%facsf, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_facsf, dum2d)
 call netcdf_error(error, 'WRITING FACSF RECORD' )

 dum2d = reshape(sfcoutput%facwf, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_facwf, dum2d)
 call netcdf_error(error, 'WRITING FACWF RECORD' )

 dum2d = uustar
 error = nf_put_var_real( ncid, id_uustar, dum2d)
 call netcdf_error(error, 'WRITING UUSTAR RECORD' )

 dum2d = ffmm
 error = nf_put_var_real( ncid, id_ffmm, dum2d)
 call netcdf_error(error, 'WRITING FFMM RECORD' )

 dum2d = ffhh
 error = nf_put_var_real( ncid, id_ffhh, dum2d)
 call netcdf_error(error, 'WRITING FFHH RECORD' )

 dum2d = reshape(sfcoutput%sea_ice_depth, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_hice, dum2d)
 call netcdf_error(error, 'WRITING HICE RECORD' )

 dum2d = reshape(sfcoutput%sea_ice_fract, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_fice, dum2d)
 call netcdf_error(error, 'WRITING FICE RECORD' )

 dum2d = reshape(sfcoutput%sea_ice_temp, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_tisfc, dum2d)
 call netcdf_error(error, 'WRITING TISFC RECORD' )

 dum2d = tprcp
 error = nf_put_var_real( ncid, id_tprcp, dum2d)
 call netcdf_error(error, 'WRITING TPRCP RECORD' )

 dum2d = srflag
 error = nf_put_var_real( ncid, id_srflag, dum2d)
 call netcdf_error(error, 'WRITING SRFLAG RECORD' )

 dum2d = reshape(sfcoutput%snow_depth, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_snwdph, dum2d)
 call netcdf_error(error, 'WRITING SNWDPH RECORD' )

 dum2d = reshape(sfcoutput%greenfrc_min, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_shdmin, dum2d)
 call netcdf_error(error, 'WRITING SHDMIN RECORD' )

 dum2d = reshape(sfcoutput%greenfrc_max, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_shdmax, dum2d)
 call netcdf_error(error, 'WRITING SHDMAX RECORD' )

 dum2d = reshape(float(sfcoutput%slope_type), (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_slope, dum2d)
 call netcdf_error(error, 'WRITING SLOPE RECORD' )

 dum2d = reshape(sfcoutput%mxsnow_alb, (/lonb,latb/) )
 error = nf_put_var_real( ncid, id_snoalb, dum2d)
 call netcdf_error(error, 'WRITING SNOALB RECORD' )

 deallocate (dum2d)

 allocate(dum3d(lonb,latb,lsoil))

 dum3d = reshape(sfcoutput%soil_temp, (/lonb,latb,lsoil/) )
 error = nf_put_var_real( ncid, id_stc, dum3d)
 call netcdf_error(error, 'WRITING STC RECORD' )

 dum3d = reshape(sfcoutput%soilm_tot, (/lonb,latb,lsoil/) )
 error = nf_put_var_real( ncid, id_smc, dum3d)
 call netcdf_error(error, 'WRITING SMC RECORD' )

 dum3d = reshape(sfcoutput%soilm_liq, (/lonb,latb,lsoil/) )
 error = nf_put_var_real( ncid, id_slc, dum3d)
 call netcdf_error(error, 'WRITING SLC RECORD' )

 deallocate (dum3d)

 if (write_nsst) then

   allocate(dum2d(lonb,latb))

   dum2d = reshape(nsst_output(:,17), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_tref, dum2d)
   call netcdf_error(error, 'WRITING TREF RECORD' )

   dum2d = reshape(nsst_output(:,10), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_z_c, dum2d)
   call netcdf_error(error, 'WRITING Z_C RECORD' )

   dum2d = reshape(nsst_output(:,11), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_c_0, dum2d)
   call netcdf_error(error, 'WRITING C_0 RECORD' )

   dum2d = reshape(nsst_output(:,12), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_c_d, dum2d)
   call netcdf_error(error, 'WRITING C_D RECORD' )

   dum2d = reshape(nsst_output(:,13), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_w_0, dum2d)
   call netcdf_error(error, 'WRITING W_0 RECORD' )

   dum2d = reshape(nsst_output(:,14), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_w_d, dum2d)
   call netcdf_error(error, 'WRITING W_D RECORD' )

   dum2d = reshape(nsst_output(:,1), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_xt, dum2d)
   call netcdf_error(error, 'WRITING XT RECORD' )

   dum2d = reshape(nsst_output(:,2), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_xs, dum2d)
   call netcdf_error(error, 'WRITING XS RECORD' )

   dum2d = reshape(nsst_output(:,3), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_xu, dum2d)
   call netcdf_error(error, 'WRITING XU RECORD' )

   dum2d = reshape(nsst_output(:,4), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_xv, dum2d)
   call netcdf_error(error, 'WRITING XV RECORD' )

   dum2d = reshape(nsst_output(:,5), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_xz, dum2d)
   call netcdf_error(error, 'WRITING XZ RECORD' )

   dum2d = reshape(nsst_output(:,6), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_zm, dum2d)
   call netcdf_error(error, 'WRITING ZM RECORD' )

   dum2d = reshape(nsst_output(:,7), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_xtts, dum2d)
   call netcdf_error(error, 'WRITING XTTS RECORD' )

   dum2d = reshape(nsst_output(:,8), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_xzts, dum2d)
   call netcdf_error(error, 'WRITING XZTS RECORD' )

   dum2d = reshape(nsst_output(:,15), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_d_conv, dum2d)
   call netcdf_error(error, 'WRITING D_CONV RECORD' )

   dum2d = reshape(nsst_output(:,16), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_ifd, dum2d)
   call netcdf_error(error, 'WRITING IFD RECORD' )

   dum2d = reshape(nsst_output(:,9), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_dt_cool, dum2d)
   call netcdf_error(error, 'WRITING DT_COOL RECORD' )

   dum2d = reshape(nsst_output(:,18), (/lonb,latb/) )
   error = nf_put_var_real(ncid, id_qrain, dum2d)
   call netcdf_error(error, 'WRITING QRAIN RECORD' )

   deallocate(dum2d)

 endif

 error = nf_close(ncid)

 end subroutine write_fv3_sfc_data_netcdf

 SUBROUTINE READ_FV3_LATLON_NETCDF(TILE_NUM, IMO, JMO, GEOLON, GEOLAT)

 use netcdf

 IMPLICIT NONE

 include "netcdf.inc"

 INTEGER, INTENT(IN)     :: TILE_NUM, IMO, JMO

 REAL, INTENT(OUT)       :: GEOLON(IMO,JMO), GEOLAT(IMO,JMO)

 CHARACTER(LEN=256)      :: TILEFILE

 INTEGER                 :: ERROR, ID_DIM, NCID, NX, NY
 INTEGER                 :: ID_VAR

 REAL, ALLOCATABLE       :: TMPVAR(:,:)

 WRITE(TILEFILE, "(A,I1)") "chgres.fv3.grd.t", TILE_NUM

 ERROR=NF90_OPEN(TRIM(TILEFILE),NF_NOWRITE,NCID)
 CALL NETCDF_ERROR(ERROR, 'OPENING FILE: '//TRIM(TILEFILE) )

 ERROR=NF90_INQ_DIMID(NCID, 'nx', ID_DIM)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING NX ID' )

 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=NX)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING NX' )

 ERROR=NF90_INQ_DIMID(NCID, 'ny', ID_DIM)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING NY ID' )

 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=NY)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING NY' )

 IF ((NX/2) /= IMO .OR. (NY/2) /= JMO) THEN
   PRINT*,'FATAL ERROR: DIMENSIONS IN GRID FILE WRONG.'
   CALL ERREXIT(160)
 ENDIF

 ALLOCATE(TMPVAR(NX,NY))

 ERROR=NF90_INQ_VARID(NCID, 'x', ID_VAR) 
 CALL NETCDF_ERROR(ERROR, 'ERROR READING X ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, TMPVAR)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING X RECORD' )

 GEOLON(1:IMO,1:JMO)     = TMPVAR(2:NX:2,2:NY:2)

 ERROR=NF90_INQ_VARID(NCID, 'y', ID_VAR) 
 CALL NETCDF_ERROR(ERROR, 'ERROR READING Y ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, TMPVAR)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING Y RECORD' )

 GEOLAT(1:IMO,1:JMO)     = TMPVAR(2:NX:2,2:NY:2)

 DEALLOCATE(TMPVAR)

 ERROR = NF_CLOSE(NCID)

 END SUBROUTINE READ_FV3_LATLON_NETCDF

 SUBROUTINE READ_FV3_GRID_DIMS_NETCDF(TILE_NUM,IMO,JMO)
   
 use netcdf

 IMPLICIT NONE

 include "netcdf.inc"

 INTEGER, INTENT(IN)   :: TILE_NUM
 INTEGER, INTENT(OUT)  :: IMO, JMO

 CHARACTER(LEN=256)    :: TILEFILE

 INTEGER               :: ERROR, NCID, ID_DIM
  
 IF (TILE_NUM < 10) THEN
   WRITE(TILEFILE, "(A,I1)") "chgres.fv3.orog.t", TILE_NUM
 ELSE
   WRITE(TILEFILE, "(A,I2)") "chgres.fv3.orog.t", TILE_NUM
 ENDIF

 PRINT*,'WILL READ GRID DIMENSIONS FROM: ', TRIM(TILEFILE)

 ERROR=NF90_OPEN(TRIM(TILEFILE),NF_NOWRITE,NCID)
 CALL NETCDF_ERROR(ERROR, 'OPENING: '//TRIM(TILEFILE) )

 ERROR=NF90_INQ_DIMID(NCID, 'lon', ID_DIM)
 CALL NETCDF_ERROR(ERROR, 'READING LON ID' )
 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=IMO)
 CALL NETCDF_ERROR(ERROR, 'READING LON VALUE' )

 PRINT*,'I-DIRECTION GRID DIM: ',IMO

 ERROR=NF90_INQ_DIMID(NCID, 'lat', ID_DIM)
 CALL NETCDF_ERROR(ERROR, 'READING LAT ID' )
 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=JMO)
 CALL NETCDF_ERROR(ERROR, 'READING LAT VALUE' )

 PRINT*,'J-DIRECTION GRID DIM: ',JMO

 ERROR = NF_CLOSE(NCID)

 END SUBROUTINE READ_FV3_GRID_DIMS_NETCDF

 SUBROUTINE READ_FV3_GRID_DATA_NETCDF(FIELD,TILE_NUM,IMO,JMO,SFCDATA)
   
 use netcdf

 IMPLICIT NONE

 include "netcdf.inc"

 CHARACTER(LEN=*)      :: FIELD

 INTEGER, INTENT(IN)   :: IMO, JMO, TILE_NUM

 REAL, INTENT(OUT)     :: SFCDATA(IMO,JMO)

 CHARACTER(LEN=256)    :: TILEFILE

 INTEGER               :: ERROR, NCID, LAT, LON, ID_DIM
 INTEGER               :: ID_VAR
  
 IF (TILE_NUM < 10) THEN
   WRITE(TILEFILE, "(A,I1)") "chgres.fv3.orog.t", TILE_NUM
 ELSE
   WRITE(TILEFILE, "(A,I2)") "chgres.fv3.orog.t", TILE_NUM
 ENDIF

 PRINT*,'WILL READ ',TRIM(FIELD), ' FROM: ', TRIM(TILEFILE)

 ERROR=NF90_OPEN(TRIM(TILEFILE),NF_NOWRITE,NCID)
 CALL NETCDF_ERROR(ERROR, 'OPENING: '//TRIM(TILEFILE) )

 ERROR=NF90_INQ_DIMID(NCID, 'lon', ID_DIM)
 CALL NETCDF_ERROR(ERROR, 'READING LON ID' )
 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=LON)
 CALL NETCDF_ERROR(ERROR, 'READING LON VALUE' )

 PRINT*,'LON IS ',LON
 IF(LON/=IMO) THEN
   PRINT*,'FATAL ERROR: I-DIMENSIONS DO NOT MATCH ',LON,IMO
   CALL ERREXIT(101)
 ENDIF

 ERROR=NF90_INQ_DIMID(NCID, 'lat', ID_DIM)
 CALL NETCDF_ERROR(ERROR, 'READING LAT ID' )
 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=LAT)
 CALL NETCDF_ERROR(ERROR, 'READING LAT VALUE' )

 PRINT*,'LAT IS ',LAT
 IF(LAT/=JMO) THEN
   PRINT*,'FATAL ERROR: J-DIMENSIONS DO NOT MATCH ',LAT,JMO
   CALL ERREXIT(102)
 ENDIF

 ERROR=NF90_INQ_VARID(NCID, FIELD, ID_VAR) 
 CALL NETCDF_ERROR(ERROR, 'READING FIELD ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, SFCDATA)
 CALL NETCDF_ERROR(ERROR, 'READING FIELD' )

 ERROR = NF_CLOSE(NCID)

 END SUBROUTINE READ_FV3_GRID_DATA_NETCDF

 SUBROUTINE WRITE_FV3_ATMS_BNDY_NETCDF(ZS,PS,T,W,U,V,Q,VCOORD,LONB,LATB,&
                                  LEVSO,NTRACM,NVCOORD,HALO)

!---------------------------------------------------------------------------
!
! Output data along the four halo boundaries.  The naming convention is
! based on point (1,1) being in the lower left corner of the grid:
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

 use netcdf

 IMPLICIT NONE

 include "netcdf.inc"

 INTEGER,  INTENT(IN)  :: LONB, LATB, LEVSO, NTRACM
 INTEGER,  INTENT(IN)  :: NVCOORD, HALO

 REAL, INTENT(IN)      :: PS(LONB,LATB), ZS(LONB,LATB)
 REAL, INTENT(IN)      :: T(LONB,LATB,LEVSO), W(LONB,LATB,LEVSO)
 REAL, INTENT(IN)      :: U(LONB,LATB,LEVSO), V(LONB,LATB,LEVSO)
 REAL, INTENT(IN)      :: Q(LONB,LATB,LEVSO,NTRACM)
 REAL, INTENT(IN)      :: VCOORD(LEVSO+1,NVCOORD)

 CHARACTER(LEN=256)    :: OUTFILE, TILEFILE

 INTEGER               :: I, II, J, JJ, IHALO, JHALO, K
 INTEGER               :: HALO_P1, IM, JM, JM2, ID_VAR
 INTEGER               :: ID_I_BOTTOM, ID_J_BOTTOM
 INTEGER               :: ID_I_TOP, ID_J_TOP
 INTEGER               :: ID_I_RIGHT, ID_J_RIGHT
 INTEGER               :: ID_I_LEFT, ID_J_LEFT
 INTEGER               :: ID_I_W_BOTTOM, ID_J_W_BOTTOM
 INTEGER               :: ID_I_W_TOP, ID_J_W_TOP
 INTEGER               :: ID_I_W_RIGHT, ID_J_W_RIGHT
 INTEGER               :: ID_I_W_LEFT, ID_J_W_LEFT
 INTEGER               :: ID_I_S_BOTTOM, ID_J_S_BOTTOM
 INTEGER               :: ID_I_S_TOP, ID_J_S_TOP
 INTEGER               :: ID_I_S_RIGHT, ID_J_S_RIGHT
 INTEGER               :: ID_I_S_LEFT, ID_J_S_LEFT
 INTEGER               :: ID_PS_TOP, ID_PS_BOTTOM
 INTEGER               :: ID_PS_RIGHT, ID_PS_LEFT
 INTEGER               :: ID_SPHUM_TOP, ID_SPHUM_BOTTOM
 INTEGER               :: ID_SPHUM_RIGHT, ID_SPHUM_LEFT
 INTEGER               :: ID_CLWMR_TOP, ID_CLWMR_BOTTOM
 INTEGER               :: ID_CLWMR_RIGHT, ID_CLWMR_LEFT
 INTEGER               :: ID_O3MR_TOP, ID_O3MR_BOTTOM
 INTEGER               :: ID_O3MR_RIGHT, ID_O3MR_LEFT
 INTEGER               :: ID_W_TOP, ID_W_BOTTOM
 INTEGER               :: ID_W_RIGHT, ID_W_LEFT
 INTEGER               :: ID_ZH_TOP, ID_ZH_BOTTOM
 INTEGER               :: ID_ZH_RIGHT, ID_ZH_LEFT
 INTEGER               :: ID_U_S_TOP, ID_U_S_BOTTOM
 INTEGER               :: ID_U_S_RIGHT, ID_U_S_LEFT
 INTEGER               :: ID_U_W_TOP, ID_U_W_BOTTOM
 INTEGER               :: ID_U_W_RIGHT, ID_U_W_LEFT
 INTEGER               :: ID_V_S_TOP, ID_V_S_BOTTOM
 INTEGER               :: ID_V_S_RIGHT, ID_V_S_LEFT
 INTEGER               :: ID_V_W_TOP, ID_V_W_BOTTOM
 INTEGER               :: ID_V_W_RIGHT, ID_V_W_LEFT
 INTEGER               :: ERROR, ID_DIM, NX, NY, NCID, NCID2
 INTEGER               :: DIM_HALO, DIM_HALOP, DIM_LON, DIM_LAT
 INTEGER               :: DIM_LATM, DIM_LONP
 INTEGER               :: DIM_LEV, DIM_LEVP
 INTEGER               :: ISTART, IEND, JSTART, JEND
 INTEGER               :: LEVSO_P1
 INTEGER               :: INITAL=0, FSIZE=65536
 INTEGER               :: HEADER_BUFFER_VAL = 16384
 INTEGER, ALLOCATABLE  :: IDUM(:)

 REAL, ALLOCATABLE     :: GEOLAT(:,:), GEOLON(:,:)
 REAL, ALLOCATABLE     :: GEOLAT_HALO(:,:), GEOLON_HALO(:,:)
 REAL, ALLOCATABLE     :: HALO_2D(:,:), HALO_3D(:,:,:), HALO_3D2(:,:,:)
 REAL, ALLOCATABLE     :: AK(:), BK(:), ZH(:,:,:)

 REAL(KIND=4), ALLOCATABLE  :: HALO_2D_4BYTE(:,:)
 REAL(KIND=4), ALLOCATABLE  :: HALO_3D_4BYTE(:,:,:)

 print*,''
 print*,'- COMPUTE AND OUTPUT LATERAL BOUNDARY DATA.'

 HALO_P1 = HALO + 1

 LEVSO_P1 = LEVSO + 1

 ALLOCATE(AK(LEVSO_P1))
 ALLOCATE(BK(LEVSO_P1))
 ALLOCATE(ZH(LONB,LATB,LEVSO_P1))

 AK = VCOORD(:,1)
 BK = VCOORD(:,2)

 CALL COMPUTE_ZH(LONB,LATB,LEVSO,AK,BK,PS,ZS,T,Q,ZH)
    
 DEALLOCATE(AK, BK)

!----------------------------------------------------------------------------------
! Read FV3 grid file.  This routine only works for a regional domain
! and assumes that domain is tile number 7.
!----------------------------------------------------------------------------------

 TILEFILE="chgres.fv3.grd.t7"

 PRINT*, "READ FV3 GRID INFO FROM: "//TRIM(TILEFILE)

 ERROR=NF90_OPEN(TRIM(TILEFILE),NF_NOWRITE,NCID)
 CALL NETCDF_ERROR(ERROR, 'OPENING FILE: '//TRIM(TILEFILE) )

 ERROR=NF90_INQ_DIMID(NCID, 'nx', ID_DIM)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING NX ID' )

 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=NX)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING NX' )

 ERROR=NF90_INQ_DIMID(NCID, 'ny', ID_DIM)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING NY ID' )

 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=NY)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING NY' )

 IF (MOD(NX,2) /= 0) THEN
   PRINT*,'FATAL ERROR: NX IS NOT EVEN'
   CALL ERREXIT(130)
 ENDIF

 IF (MOD(NY,2) /= 0) THEN
   PRINT*,'FATAL ERROR: NY IS NOT EVEN'
   CALL ERREXIT(131)
 ENDIF

 IM = NX/2
 JM = NY/2

 ALLOCATE(GEOLON(NX+1,NY+1))
 ALLOCATE(GEOLAT(NX+1,NY+1))

 ERROR=NF90_INQ_VARID(NCID, 'x', ID_VAR) 
 CALL NETCDF_ERROR(ERROR, 'ERROR READING X ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, GEOLON)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING X RECORD' )

 ERROR=NF90_INQ_VARID(NCID, 'y', ID_VAR) 
 CALL NETCDF_ERROR(ERROR, 'ERROR READING Y ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, GEOLAT)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING Y RECORD' )

 ERROR = NF_CLOSE(NCID2)

!----------------------------------------------------------------------------------
! Create output file header.
!----------------------------------------------------------------------------------

 WRITE(OUTFILE, '(A, I1, A)'), 'gfs_bndy.tile', 7, '.nc'
 ERROR = NF__CREATE(OUTFILE, IOR(NF_NETCDF4,NF_CLASSIC_MODEL),INITAL, FSIZE, NCID2)
 CALL NETCDF_ERROR(ERROR, 'CREATING FILE: '//TRIM(OUTFILE) )

 ERROR = NF90_DEF_DIM(NCID2, 'lon', IM, DIM_LON)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LON DIMENSION')

 JM2 = JM - (2*HALO)
 ERROR = NF90_DEF_DIM(NCID2, 'lat', JM2, DIM_LAT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LAT DIMENSION')

 ERROR = NF90_DEF_DIM(NCID2, 'lonp', (IM+1), DIM_LONP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LONP DIMENSION')

 JM2 = (JM + 1) - (2*HALO_P1)
 ERROR = NF90_DEF_DIM(NCID2, 'latm', JM2, DIM_LATM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LATM DIMENSION')

 ERROR = NF90_DEF_DIM(NCID2, 'halo', HALO, DIM_HALO)
 CALL NETCDF_ERROR(ERROR, 'DEFINING HALO DIMENSION')

 ERROR = NF90_DEF_DIM(NCID2, 'halop', HALO_P1, DIM_HALOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING HALOP DIMENSION')

 ERROR = NF90_DEF_DIM(NCID2, 'lev', LEVSO, DIM_LEV)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LEV DIMENSION')

 ERROR = NF90_DEF_DIM(NCID2, 'levp', LEVSO_P1, DIM_LEVP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LEVP DIMENSION')

 ERROR = NF90_DEF_VAR(NCID2, 'i_bottom', NF90_INT, &
                             (/DIM_LON/), ID_I_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING I_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'j_bottom', NF90_INT, &
                             (/DIM_HALO/), ID_J_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING J_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'i_top', NF90_INT, &
                             (/DIM_LON/), ID_I_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING I_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'j_top', NF90_INT, &
                             (/DIM_HALO/), ID_J_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING J_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'i_right', NF90_INT, &
                             (/DIM_HALO/), ID_I_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING I_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'j_right', NF90_INT, &
                             (/DIM_LAT/), ID_J_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING J_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'i_left', NF90_INT, &
                             (/DIM_HALO/), ID_I_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING I_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'j_left', NF90_INT, &
                             (/DIM_LAT/), ID_J_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING J_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'ps_bottom', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALO/), ID_PS_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING PS_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'ps_top', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALO/), ID_PS_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING PS_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'ps_right', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LAT/), ID_PS_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING PS_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'ps_left', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LAT/), ID_PS_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING PS_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'w_bottom', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALO, DIM_LEV/), ID_W_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING W_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'w_top', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALO, DIM_LEV/), ID_W_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING W_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'w_right', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LAT, DIM_LEV/), ID_W_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING W_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'w_left', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LAT, DIM_LEV/), ID_W_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING W_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'zh_bottom', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALO, DIM_LEVP/), ID_ZH_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING ZH_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'zh_top', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALO, DIM_LEVP/), ID_ZH_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING ZH_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'zh_right', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LAT, DIM_LEVP/), ID_ZH_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING ZH_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'zh_left', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LAT, DIM_LEVP/), ID_ZH_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING ZH_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'sphum_bottom', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALO, DIM_LEV/), ID_SPHUM_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING SPHUM_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'sphum_top', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALO, DIM_LEV/), ID_SPHUM_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING SPHUM_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'sphum_right', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LAT, DIM_LEV/), ID_SPHUM_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING SPHUM_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'sphum_left', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LAT, DIM_LEV/), ID_SPHUM_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING SPHUM_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'o3mr_bottom', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALO, DIM_LEV/), ID_O3MR_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING O3MR_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'o3mr_top', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALO, DIM_LEV/), ID_O3MR_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING O3MR_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'o3mr_right', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LAT, DIM_LEV/), ID_O3MR_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING O3MR_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'o3mr_left', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LAT, DIM_LEV/), ID_O3MR_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING O3MR_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'liq_wat_bottom', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALO, DIM_LEV/), ID_CLWMR_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LIQ_WAT_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'liq_wat_top', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALO, DIM_LEV/), ID_CLWMR_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LIQ_WAT_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'liq_wat_right', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LAT, DIM_LEV/), ID_CLWMR_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LIQ_WAT_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'liq_wat_left', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LAT, DIM_LEV/), ID_CLWMR_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LIQ_WAT_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'i_w_bottom', NF90_INT, &
                             (/DIM_LONP/), ID_I_W_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING I_W_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'j_w_bottom', NF90_INT, &
                             (/DIM_HALO/), ID_J_W_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING J_W_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'i_w_top', NF90_INT, &
                             (/DIM_LONP/), ID_I_W_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING I_W_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'j_w_top', NF90_INT, &
                             (/DIM_HALO/), ID_J_W_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING J_W_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'i_w_right', NF90_INT, &
                             (/DIM_HALOP/), ID_I_W_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING I_W_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'j_w_right', NF90_INT, &
                             (/DIM_LAT/), ID_J_W_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING J_W_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'i_w_left', NF90_INT, &
                             (/DIM_HALOP/), ID_I_W_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING I_W_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'j_w_left', NF90_INT, &
                             (/DIM_LAT/), ID_J_W_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING J_W_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'u_w_bottom', NF90_FLOAT, &
                             (/DIM_LONP, DIM_HALO, DIM_LEV/), ID_U_W_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING U_W_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'u_w_top', NF90_FLOAT, &
                             (/DIM_LONP, DIM_HALO, DIM_LEV/), ID_U_W_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING U_W_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'u_w_right', NF90_FLOAT, &
                             (/DIM_HALOP, DIM_LAT, DIM_LEV/), ID_U_W_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING U_W_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'u_w_left', NF90_FLOAT, &
                             (/DIM_HALOP, DIM_LAT, DIM_LEV/), ID_U_W_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING U_W_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'v_w_bottom', NF90_FLOAT, &
                             (/DIM_LONP, DIM_HALO, DIM_LEV/), ID_V_W_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING V_W_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'v_w_top', NF90_FLOAT, &
                             (/DIM_LONP, DIM_HALO, DIM_LEV/), ID_V_W_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING V_W_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'v_w_right', NF90_FLOAT, &
                             (/DIM_HALOP, DIM_LAT, DIM_LEV/), ID_V_W_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING V_W_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'v_w_left', NF90_FLOAT, &
                             (/DIM_HALOP, DIM_LAT, DIM_LEV/), ID_V_W_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING V_W_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'i_s_bottom', NF90_INT, &
                             (/DIM_LON/), ID_I_S_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING I_S_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'j_s_bottom', NF90_INT, &
                             (/DIM_HALOP/), ID_J_S_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING J_S_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'i_s_top', NF90_INT, &
                             (/DIM_LON/), ID_I_S_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING I_S_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'j_s_top', NF90_INT, &
                             (/DIM_HALOP/), ID_J_S_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING J_S_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'i_s_right', NF90_INT, &
                             (/DIM_HALO/), ID_I_S_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING I_S_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'j_s_right', NF90_INT, &
                             (/DIM_LATM/), ID_J_S_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING J_S_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'i_s_left', NF90_INT, &
                             (/DIM_HALO/), ID_I_S_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING I_S_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'j_s_left', NF90_INT, &
                             (/DIM_LATM/), ID_J_S_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING J_S_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'u_s_bottom', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALOP, DIM_LEV/), ID_U_S_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING U_S_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'u_s_top', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALOP, DIM_LEV/), ID_U_S_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING U_S_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'u_s_right', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LATM, DIM_LEV/), ID_U_S_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING U_S_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'u_s_left', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LATM, DIM_LEV/), ID_U_S_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING U_S_LEFT')

 ERROR = NF90_DEF_VAR(NCID2, 'v_s_bottom', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALOP, DIM_LEV/), ID_V_S_BOTTOM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING V_S_BOTTOM')

 ERROR = NF90_DEF_VAR(NCID2, 'v_s_top', NF90_FLOAT, &
                             (/DIM_LON, DIM_HALOP, DIM_LEV/), ID_V_S_TOP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING V_S_TOP')

 ERROR = NF90_DEF_VAR(NCID2, 'v_s_right', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LATM, DIM_LEV/), ID_V_S_RIGHT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING V_S_RIGHT')

 ERROR = NF90_DEF_VAR(NCID2, 'v_s_left', NF90_FLOAT, &
                             (/DIM_HALO, DIM_LATM, DIM_LEV/), ID_V_S_LEFT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING V_S_LEFT')

 ERROR = NF__ENDDEF(NCID2, HEADER_BUFFER_VAL, 4, 0, 4)
 CALL NETCDF_ERROR(ERROR, 'DEFINING END OF HEADER')

!----------------------------------------------------------------------------------
! "Bottom" boundary.
!----------------------------------------------------------------------------------

 ISTART = 1
 IEND   = IM
 JSTART = 1
 JEND   = HALO

 IHALO = IEND - ISTART + 1
 JHALO = JEND - JSTART + 1

 ALLOCATE(IDUM(ISTART:IEND))
 DO I = ISTART, IEND
   IDUM(I) = I
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_I_BOTTOM, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING I_BOTTOM')
 DEALLOCATE(IDUM)

 ALLOCATE(IDUM(JSTART:JEND))
 DO J = JSTART, JEND
   IDUM(J) = J
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_J_BOTTOM, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING J_BOTTOM')
 DEALLOCATE(IDUM)

 ALLOCATE(GEOLAT_HALO(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_HALO(ISTART:IEND,JSTART:JEND))

 DO J = JSTART, JEND
   DO I = ISTART, IEND
     II = 2*I
     JJ = 2*J
     GEOLON_HALO(I,J) = GEOLON(II,JJ)
     GEOLAT_HALO(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO
 
 ALLOCATE(HALO_2D(ISTART:IEND,JSTART:JEND))
 ALLOCATE(HALO_2D_4BYTE(ISTART:IEND,JSTART:JEND))

 CALL GL2ANY(0, 1, PS, LONB, LATB, HALO_2D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 HALO_2D_4BYTE = REAL(HALO_2D,4)

 ERROR = NF90_PUT_VAR(NCID2, ID_PS_BOTTOM, HALO_2D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING PS_BOTTOM')

 DEALLOCATE(HALO_2D, HALO_2D_4BYTE)

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANY(0, LEVSO, Q(:,:,:,1), LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_SPHUM_BOTTOM, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING SPHUM_BOTTOM')

 CALL GL2ANY(0, LEVSO, Q(:,:,:,2), LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_O3MR_BOTTOM, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING O3MR_BOTTOM')

 CALL GL2ANY(0, LEVSO, Q(:,:,:,3), LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_CLWMR_BOTTOM, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING LIQ_WAT_BOTTOM')

 CALL GL2ANY(0, LEVSO, W, LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_W_BOTTOM, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING W_BOTTOM')

 DEALLOCATE(HALO_3D, HALO_3D_4BYTE)

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO_P1))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO_P1))

 CALL GL2ANY(0, LEVSO_P1, ZH, LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO_P1
   HALO_3D_4BYTE(:,:,LEVSO_P1-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_ZH_BOTTOM, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING ZH_BOTTOM')

 DEALLOCATE(HALO_3D, HALO_3D_4BYTE)
 DEALLOCATE(GEOLAT_HALO, GEOLON_HALO)

 ISTART = 1
 IEND   = IM+1
 JSTART = 1
 JEND   = HALO

 IHALO = IEND - ISTART + 1
 JHALO = JEND - JSTART + 1

 ALLOCATE(IDUM(ISTART:IEND))
 DO I = ISTART, IEND
   IDUM(I) = I
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_I_W_BOTTOM, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING I_W_BOTTOM')
 DEALLOCATE(IDUM)

 ALLOCATE(IDUM(JSTART:JEND))
 DO J = JSTART, JEND
   IDUM(J) = J
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_J_W_BOTTOM, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING J_W_BOTTOM')
 DEALLOCATE(IDUM)

 ALLOCATE(GEOLAT_HALO(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_HALO(ISTART:IEND,JSTART:JEND))

 DO J = JSTART, JEND
   DO I = ISTART, IEND
     II = (2*I)-1
     JJ = 2*J
     GEOLON_HALO(I,J) = GEOLON(II,JJ)
     GEOLAT_HALO(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D2(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANYV(0, LEVSO, U, V, LONB, LATB, HALO_3D, HALO_3D2, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_U_W_BOTTOM, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING U_W_BOTTOM')

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D2(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_V_W_BOTTOM, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING V_W_BOTTOM')

 DEALLOCATE(HALO_3D, HALO_3D2, HALO_3D_4BYTE)
 DEALLOCATE(GEOLAT_HALO, GEOLON_HALO)

 ISTART = 1
 IEND   = IM
 JSTART = 1
 JEND   = HALO + 1

 IHALO = IEND - ISTART + 1
 JHALO = JEND - JSTART + 1

 ALLOCATE(IDUM(ISTART:IEND))
 DO I = ISTART, IEND
   IDUM(I) = I
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_I_S_BOTTOM, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING I_S_BOTTOM')
 DEALLOCATE(IDUM)

 ALLOCATE(IDUM(JSTART:JEND))
 DO J = JSTART, JEND
   IDUM(J) = J
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_J_S_BOTTOM, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING J_S_BOTTOM')
 DEALLOCATE(IDUM)

 ALLOCATE(GEOLAT_HALO(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_HALO(ISTART:IEND,JSTART:JEND))

 DO J = JSTART, JEND
   DO I = ISTART, IEND
     II = 2*I
     JJ = (2*J) - 1
     GEOLON_HALO(I,J) = GEOLON(II,JJ)
     GEOLAT_HALO(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D2(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANYV(0, LEVSO, U, V, LONB, LATB, HALO_3D, HALO_3D2, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_U_S_BOTTOM, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING U_S_BOTTOM')

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D2(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_V_S_BOTTOM, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING V_S_BOTTOM')

 DEALLOCATE(HALO_3D, HALO_3D2, HALO_3D_4BYTE)
 DEALLOCATE(GEOLAT_HALO, GEOLON_HALO)

!----------------------------------------------------------------------------------
! "Top" boundary.
!----------------------------------------------------------------------------------

 ISTART = 1
 IEND   = IM
 JSTART = JM - HALO + 1
 JEND   = JM

 IHALO = IEND - ISTART + 1
 JHALO = JEND - JSTART + 1

 ALLOCATE(IDUM(ISTART:IEND))
 DO I = ISTART, IEND
   IDUM(I) = I
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_I_TOP, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING I_TOP')
 DEALLOCATE(IDUM)

 ALLOCATE(IDUM(JSTART:JEND))
 DO J = JSTART, JEND
   IDUM(J) = J
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_J_TOP, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING J_TOP')
 DEALLOCATE(IDUM)

 ALLOCATE(GEOLAT_HALO(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_HALO(ISTART:IEND,JSTART:JEND))

 DO J = JSTART, JEND
   DO I = ISTART, IEND
     II = 2*I
     JJ = 2*J
     GEOLON_HALO(I,J) = GEOLON(II,JJ)
     GEOLAT_HALO(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO

 ALLOCATE(HALO_2D(ISTART:IEND,JSTART:JEND))
 ALLOCATE(HALO_2D_4BYTE(ISTART:IEND,JSTART:JEND))

 CALL GL2ANY(0, 1, PS, LONB, LATB, HALO_2D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 HALO_2D_4BYTE = REAL(HALO_2D,4)

 ERROR = NF90_PUT_VAR(NCID2, ID_PS_TOP, HALO_2D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING PS_TOP')

 DEALLOCATE(HALO_2D, HALO_2D_4BYTE)

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANY(0, LEVSO, Q(:,:,:,1), LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_SPHUM_TOP, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING SPHUM_TOP')

 CALL GL2ANY(0, LEVSO, Q(:,:,:,2), LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_O3MR_TOP, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING O3MR_TOP')

 CALL GL2ANY(0, LEVSO, Q(:,:,:,3), LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_CLWMR_TOP, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING CLWMR_TOP')

 CALL GL2ANY(0, LEVSO, W, LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_W_TOP, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING W_TOP')

 DEALLOCATE(HALO_3D, HALO_3D_4BYTE)

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO_P1))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO_P1))

 CALL GL2ANY(0, LEVSO_P1, ZH, LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO_P1
   HALO_3D_4BYTE(:,:,LEVSO_P1-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_ZH_TOP, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING ZH_TOP')

 DEALLOCATE(HALO_3D, HALO_3D_4BYTE)
 DEALLOCATE(GEOLAT_HALO, GEOLON_HALO)

 ISTART = 1
 IEND   = IM+1
 JSTART = JM - HALO + 1
 JEND   = JM

 IHALO = IEND - ISTART + 1
 JHALO = JEND - JSTART + 1

 ALLOCATE(IDUM(ISTART:IEND))
 DO I = ISTART, IEND
   IDUM(I) = I
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_I_W_TOP, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING I_W_TOP')
 DEALLOCATE(IDUM)

 ALLOCATE(IDUM(JSTART:JEND))
 DO J = JSTART, JEND
   IDUM(J) = J
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_J_W_TOP, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING J_W_TOP')
 DEALLOCATE(IDUM)

 ALLOCATE(GEOLAT_HALO(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_HALO(ISTART:IEND,JSTART:JEND))

 DO J = JSTART, JEND
   DO I = ISTART, IEND
     II = (2*I)-1
     JJ = 2*J
     GEOLON_HALO(I,J) = GEOLON(II,JJ)
     GEOLAT_HALO(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D2(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANYV(0, LEVSO, U, V, LONB, LATB, HALO_3D, HALO_3D2, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_U_W_TOP, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING U_W_TOP')

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D2(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_V_W_TOP, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING V_W_TOP')

 DEALLOCATE(HALO_3D, HALO_3D2, HALO_3D_4BYTE)
 DEALLOCATE(GEOLAT_HALO, GEOLON_HALO)

 ISTART = 1
 IEND   = IM
 JSTART = JM - HALO + 1
 JEND   = JM + 1

 IHALO = IEND - ISTART + 1
 JHALO = JEND - JSTART + 1

 ALLOCATE(IDUM(ISTART:IEND))
 DO I = ISTART, IEND
   IDUM(I) = I
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_I_S_TOP, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING I_S_TOP')
 DEALLOCATE(IDUM)

 ALLOCATE(IDUM(JSTART:JEND))
 DO J = JSTART, JEND
   IDUM(J) = J
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_J_S_TOP, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING J_S_TOP')

 DEALLOCATE(IDUM)

 ALLOCATE(GEOLAT_HALO(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_HALO(ISTART:IEND,JSTART:JEND))

 DO J = JSTART, JEND
   DO I = ISTART, IEND
     II = 2*I
     JJ = (2*J) - 1
     GEOLON_HALO(I,J) = GEOLON(II,JJ)
     GEOLAT_HALO(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D2(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANYV(0, LEVSO, U, V, LONB, LATB, HALO_3D, HALO_3D2, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_U_S_TOP, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING U_S_TOP')

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D2(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_V_S_TOP, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING V_S_TOP')

 DEALLOCATE(HALO_3D, HALO_3D2, HALO_3D_4BYTE)
 DEALLOCATE(GEOLAT_HALO, GEOLON_HALO)

!----------------------------------------------------------------------------------
! "Left" boundary.
!----------------------------------------------------------------------------------

 ISTART = 1
 IEND   = HALO
 JSTART = HALO + 1
 JEND   = JM - HALO

 IHALO = IEND - ISTART + 1
 JHALO = JEND - JSTART + 1

 ALLOCATE(IDUM(ISTART:IEND))
 DO I = ISTART, IEND
   IDUM(I) = I
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_I_LEFT, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING I_LEFT')
 DEALLOCATE(IDUM)

 ALLOCATE(IDUM(JSTART:JEND))
 DO J = JSTART, JEND
   IDUM(J) = J
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_J_LEFT, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING J_LEFT')
 DEALLOCATE(IDUM)

 ALLOCATE(GEOLAT_HALO(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_HALO(ISTART:IEND,JSTART:JEND))

 DO J = JSTART, JEND
   DO I = ISTART, IEND
     II = 2*I
     JJ = 2*J
     GEOLON_HALO(I,J) = GEOLON(II,JJ)
     GEOLAT_HALO(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO

 ALLOCATE(HALO_2D(ISTART:IEND,JSTART:JEND))
 ALLOCATE(HALO_2D_4BYTE(ISTART:IEND,JSTART:JEND))

 CALL GL2ANY(0, 1, PS, LONB, LATB, HALO_2D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 HALO_2D_4BYTE = REAL(HALO_2D,4)

 ERROR = NF90_PUT_VAR(NCID2, ID_PS_LEFT, HALO_2D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING PS_LEFT')

 DEALLOCATE(HALO_2D, HALO_2D_4BYTE)

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANY(0, LEVSO, Q(:,:,:,1), LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_SPHUM_LEFT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING SPHUM_LEFT')

 CALL GL2ANY(0, LEVSO, Q(:,:,:,2), LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_O3MR_LEFT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING O3MR_LEFT')

 CALL GL2ANY(0, LEVSO, Q(:,:,:,3), LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_CLWMR_LEFT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING CLWMR_LEFT')

 CALL GL2ANY(0, LEVSO, W, LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_W_LEFT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING W_LEFT')

 DEALLOCATE(HALO_3D, HALO_3D_4BYTE)

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO_P1))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO_P1))

 CALL GL2ANY(0, LEVSO_P1, ZH, LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO_P1
   HALO_3D_4BYTE(:,:,LEVSO_P1-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_ZH_LEFT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING ZH_LEFT')

 DEALLOCATE(HALO_3D, HALO_3D_4BYTE)
 DEALLOCATE(GEOLAT_HALO, GEOLON_HALO)

 ISTART = 1
 IEND   = HALO + 1
 JSTART = HALO + 1
 JEND   = JM - HALO

 IHALO = IEND - ISTART + 1
 JHALO = JEND - JSTART + 1

 ALLOCATE(IDUM(ISTART:IEND))
 DO I = ISTART, IEND
   IDUM(I) = I
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_I_W_LEFT, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING I_W_LEFT')
 DEALLOCATE(IDUM)

 ALLOCATE(IDUM(JSTART:JEND))
 DO J = JSTART, JEND
   IDUM(J) = J
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_J_W_LEFT, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING J_W_LEFT')
 DEALLOCATE(IDUM)

 ALLOCATE(GEOLAT_HALO(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_HALO(ISTART:IEND,JSTART:JEND))

 DO J = JSTART, JEND
   DO I = ISTART, IEND
     II = (2*I)-1
     JJ = 2*J
     GEOLON_HALO(I,J) = GEOLON(II,JJ)
     GEOLAT_HALO(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D2(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANYV(0, LEVSO, U, V, LONB, LATB, HALO_3D, HALO_3D2, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_U_W_LEFT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING U_W_LEFT')

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D2(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_V_W_LEFT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING V_W_LEFT')

 DEALLOCATE(HALO_3D, HALO_3D2, HALO_3D_4BYTE)
 DEALLOCATE(GEOLAT_HALO, GEOLON_HALO)

 ISTART = 1
 IEND   = HALO
 JSTART = HALO_P1 + 1
 JEND   = JM + 1 - HALO_P1

 IHALO = IEND - ISTART + 1
 JHALO = JEND - JSTART + 1

 ALLOCATE(IDUM(ISTART:IEND))
 DO I = ISTART, IEND
   IDUM(I) = I
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_I_S_LEFT, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING I_S_LEFT')
 DEALLOCATE(IDUM)

 ALLOCATE(IDUM(JSTART:JEND))
 DO J = JSTART, JEND
   IDUM(J) = J
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_J_S_LEFT, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING J_S_LEFT')
 DEALLOCATE(IDUM)

 ALLOCATE(GEOLAT_HALO(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_HALO(ISTART:IEND,JSTART:JEND))

 DO J = JSTART, JEND
   DO I = ISTART, IEND
     II = 2*I
     JJ = (2*J) - 1
     GEOLON_HALO(I,J) = GEOLON(II,JJ)
     GEOLAT_HALO(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D2(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANYV(0, LEVSO, U, V, LONB, LATB, HALO_3D, HALO_3D2, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_U_S_LEFT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING U_S_LEFT')

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D2(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_V_S_LEFT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING V_S_LEFT')

 DEALLOCATE(HALO_3D, HALO_3D2, HALO_3D_4BYTE)
 DEALLOCATE(GEOLAT_HALO, GEOLON_HALO)

!----------------------------------------------------------------------------------
! "Right" boundary.
!----------------------------------------------------------------------------------

 ISTART = IM - HALO + 1
 IEND   = IM
 JSTART = HALO + 1
 JEND   = JM - HALO

 IHALO = IEND - ISTART + 1
 JHALO = JEND - JSTART + 1

 ALLOCATE(IDUM(ISTART:IEND))
 DO I = ISTART, IEND
   IDUM(I) = I
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_I_RIGHT, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING I_RIGHT')
 DEALLOCATE(IDUM)

 ALLOCATE(IDUM(JSTART:JEND))
 DO J = JSTART, JEND
   IDUM(J) = J
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_J_RIGHT, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING J_RIGHT')
 DEALLOCATE(IDUM)

 ALLOCATE(GEOLAT_HALO(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_HALO(ISTART:IEND,JSTART:JEND))

 DO J = JSTART, JEND
   DO I = ISTART, IEND
     II = 2*I
     JJ = 2*J
     GEOLON_HALO(I,J) = GEOLON(II,JJ)
     GEOLAT_HALO(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO

 ALLOCATE(HALO_2D(ISTART:IEND,JSTART:JEND))
 ALLOCATE(HALO_2D_4BYTE(ISTART:IEND,JSTART:JEND))

 CALL GL2ANY(0, 1, PS, LONB, LATB, HALO_2D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 HALO_2D_4BYTE = REAL(HALO_2D,4)

 ERROR = NF90_PUT_VAR(NCID2, ID_PS_RIGHT, HALO_2D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING PS_RIGHT')

 DEALLOCATE(HALO_2D, HALO_2D_4BYTE)

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANY(0, LEVSO, Q(:,:,:,1), LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_SPHUM_RIGHT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING SPHUM_RIGHT')

 CALL GL2ANY(0, LEVSO, Q(:,:,:,2), LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_O3MR_RIGHT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING O3MR_RIGHT')

 CALL GL2ANY(0, LEVSO, Q(:,:,:,3), LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_CLWMR_RIGHT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING CLWMR_RIGHT')

 CALL GL2ANY(0, LEVSO, W, LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_W_RIGHT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING W_RIGHT')

 DEALLOCATE(HALO_3D, HALO_3D_4BYTE)

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO_P1))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO_P1))

 CALL GL2ANY(0, LEVSO_P1, ZH, LONB, LATB, HALO_3D, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)
 DO K = 1, LEVSO_P1
   HALO_3D_4BYTE(:,:,LEVSO_P1-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_ZH_RIGHT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING ZH_RIGHT')

 DEALLOCATE(HALO_3D, HALO_3D_4BYTE)
 DEALLOCATE(GEOLAT_HALO, GEOLON_HALO)

 ISTART = IM - HALO + 1
 IEND   = IM + 1
 JSTART = HALO + 1
 JEND   = JM - HALO

 IHALO = IEND - ISTART + 1
 JHALO = JEND - JSTART + 1

 ALLOCATE(IDUM(ISTART:IEND))
 DO I = ISTART, IEND
   IDUM(I) = I
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_I_W_RIGHT, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING I_W_RIGHT')
 DEALLOCATE(IDUM)

 ALLOCATE(IDUM(JSTART:JEND))
 DO J = JSTART, JEND
   IDUM(J) = J
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_J_W_RIGHT, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING J_W_RIGHT')
 DEALLOCATE(IDUM)

 ALLOCATE(GEOLAT_HALO(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_HALO(ISTART:IEND,JSTART:JEND))

 DO J = JSTART, JEND
   DO I = ISTART, IEND
     II = (2*I)-1
     JJ = 2*J
     GEOLON_HALO(I,J) = GEOLON(II,JJ)
     GEOLAT_HALO(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D2(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANYV(0, LEVSO, U, V, LONB, LATB, HALO_3D, HALO_3D2, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_U_W_RIGHT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING U_W_RIGHT')

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D2(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_V_W_RIGHT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING V_W_RIGHT')

 DEALLOCATE(HALO_3D, HALO_3D2, HALO_3D_4BYTE)
 DEALLOCATE(GEOLAT_HALO, GEOLON_HALO)

 ISTART = IM - HALO + 1
 IEND   = IM
 JSTART = HALO_P1 + 1
 JEND   = JM + 1 - HALO_P1

 IHALO = IEND - ISTART + 1
 JHALO = JEND - JSTART + 1

 ALLOCATE(IDUM(ISTART:IEND))
 DO I = ISTART, IEND
   IDUM(I) = I
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_I_S_RIGHT, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING I_S_RIGHT')
 DEALLOCATE(IDUM)

 ALLOCATE(IDUM(JSTART:JEND))
 DO J = JSTART, JEND
   IDUM(J) = J
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_J_S_RIGHT, IDUM)
 CALL NETCDF_ERROR(ERROR, 'WRITING J_S_RIGHT')
 DEALLOCATE(IDUM)

 ALLOCATE(GEOLAT_HALO(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_HALO(ISTART:IEND,JSTART:JEND))

 DO J = JSTART, JEND
   DO I = ISTART, IEND
     II = 2*I
     JJ = (2*J) - 1
     GEOLON_HALO(I,J) = GEOLON(II,JJ)
     GEOLAT_HALO(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO

 ALLOCATE(HALO_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(HALO_3D2(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANYV(0, LEVSO, U, V, LONB, LATB, HALO_3D, HALO_3D2, IHALO, JHALO, GEOLON_HALO, GEOLAT_HALO)

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_U_S_RIGHT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING U_S_RIGHT')

 DO K = 1, LEVSO
   HALO_3D_4BYTE(:,:,LEVSO-K+1) = REAL(HALO_3D2(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_V_S_RIGHT, HALO_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING V_S_RIGHT')

 DEALLOCATE(HALO_3D, HALO_3D2, HALO_3D_4BYTE)
 DEALLOCATE(GEOLAT_HALO, GEOLON_HALO)

!----------------------------------------------------------------------------------
! Cleanup and close file.
!----------------------------------------------------------------------------------

 DEALLOCATE(GEOLAT)
 DEALLOCATE(GEOLON)

 ERROR = NF_CLOSE(NCID2)

 END SUBROUTINE WRITE_FV3_ATMS_BNDY_NETCDF

 SUBROUTINE WRITE_FV3_ATMS_NETCDF(ZS,PS,T,W,U,V,Q,VCOORD,LONB,LATB,&
                                  LEVSO,NTRACM,NVCOORD,NTILES,HALO)

 use netcdf

 IMPLICIT NONE

 include "netcdf.inc"

 INTEGER,  INTENT(IN)  :: NTILES, LONB, LATB, LEVSO, NTRACM
 INTEGER,  INTENT(IN)  :: NVCOORD, HALO

 REAL, INTENT(IN)      :: PS(LONB,LATB), ZS(LONB,LATB)
 REAL, INTENT(IN)      :: T(LONB,LATB,LEVSO), W(LONB,LATB,LEVSO)
 REAL, INTENT(IN)      :: U(LONB,LATB,LEVSO), V(LONB,LATB,LEVSO)
 REAL, INTENT(IN)      :: Q(LONB,LATB,LEVSO,NTRACM)
 REAL, INTENT(IN)      :: VCOORD(LEVSO+1,NVCOORD)

 CHARACTER(LEN=256)    :: TILEFILE, OUTFILE

 INTEGER               :: ID_DIM, ID_VAR, IM, JM
 INTEGER               :: ERROR, N, NCID, NCID2, NX, NY
 INTEGER               :: INITAL=0, FSIZE=65536
 INTEGER               :: HEADER_BUFFER_VAL = 16384
 INTEGER               :: DIM_LON, DIM_LAT, DIM_LONP, DIM_LATP
 INTEGER               :: DIM_LEV, DIM_LEVP, DIM_TRACER
 INTEGER               :: ID_LON, ID_LAT, ID_PS
 INTEGER               :: ID_W, ID_ZH, ID_SPHUM, ID_O3MR
 INTEGER               :: ID_CLWMR, ID_U_W, ID_V_W
 INTEGER               :: ID_U_S, ID_V_S, K, LEVSO_P1
 INTEGER               :: I, J, II, JJ
 INTEGER               :: ISTART, IEND, JSTART, JEND, IM_OUT, JM_OUT
 INTEGER               :: START_TILE, END_TILE

 REAL, ALLOCATABLE     :: CUBE_2D(:,:), CUBE_3D(:,:,:), CUBE_3D2(:,:,:)
 REAL, ALLOCATABLE     :: AK(:), BK(:), ZH(:,:,:)
 REAL, ALLOCATABLE     :: GEOLAT(:,:), GEOLAT_W(:,:), GEOLAT_S(:,:)
 REAL, ALLOCATABLE     :: GEOLON(:,:), GEOLON_W(:,:)
 REAL, ALLOCATABLE     :: GEOLON_S(:,:), TMPVAR(:,:)

 REAL(KIND=4), ALLOCATABLE  :: CUBE_2D_4BYTE(:,:)
 REAL(KIND=4), ALLOCATABLE  :: CUBE_3D_4BYTE(:,:,:)

 LEVSO_P1 = LEVSO + 1

 CALL WRITE_FV3_ATMS_HEADER_NETCDF(LEVSO_P1, NTRACM, NVCOORD, VCOORD)

 ALLOCATE(AK(LEVSO_P1))
 ALLOCATE(BK(LEVSO_P1))
 ALLOCATE(ZH(LONB,LATB,(LEVSO_P1)))

 AK = VCOORD(:,1)
 BK = VCOORD(:,2)

 CALL COMPUTE_ZH(LONB,LATB,LEVSO,AK,BK,PS,ZS,T,Q,ZH)
    
 DEALLOCATE(AK, BK)

 PRINT*,''

 IF (HALO == 0) THEN  ! NOT A REGIONAL GRID
   START_TILE = 1
   END_TILE   = NTILES
 ELSE                 ! A REGIONAL GRID.  ASSUME IT IS TILE 7.
   START_TILE = 7
   END_TILE   = 7
 ENDIF

 TILE_LOOP : DO N = START_TILE, END_TILE

 PRINT*,'WRITE FV3 ATMOSPHERIC DATA FOR TILE ',N

 IF (N < 10) THEN
   WRITE(TILEFILE, "(A,I1)") "chgres.fv3.grd.t", N
 ELSE
   WRITE(TILEFILE, "(A,I2)") "chgres.fv3.grd.t", N
 ENDIF

 ERROR=NF90_OPEN(TRIM(TILEFILE),NF_NOWRITE,NCID)
 CALL NETCDF_ERROR(ERROR, 'OPENING FILE: '//TRIM(TILEFILE) )

 ERROR=NF90_INQ_DIMID(NCID, 'nx', ID_DIM)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING NX ID' )

 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=NX)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING NX' )

 ERROR=NF90_INQ_DIMID(NCID, 'ny', ID_DIM)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING NY ID' )

 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=NY)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING NY' )

 IF (MOD(NX,2) /= 0) THEN
   PRINT*,'FATAL ERROR: NX IS NOT EVEN'
   CALL ERREXIT(103)
 ENDIF

 IF (MOD(NY,2) /= 0) THEN
   PRINT*,'FATAL ERROR: NY IS NOT EVEN'
   CALL ERREXIT(104)
 ENDIF

 IM = NX/2
 JM = NY/2

 IF (HALO > 0) THEN
   ISTART     = 1 + HALO
   IEND       = IM - HALO
   JSTART     = 1+ HALO
   JEND       = JM - HALO
   PRINT*,''
   PRINT*,"WILL NOT PROCESS HALO REGION."
   PRINT*,"HALO IS ", HALO, " ROWS/COLUMNS"
   PRINT*,"WILL PROCESS I= ", ISTART, " TO ", IEND
   PRINT*,"WILL PROCESS J= ", JSTART, " TO ", JEND
   PRINT*,''
 ELSE
   ISTART = 1
   IEND   = IM
   JSTART = 1
   JEND   = JM
 ENDIF

 IM_OUT = IEND - ISTART + 1
 JM_OUT = JEND - JSTART +1

 PRINT*, "READ FV3 GRID INFO FROM: "//TRIM(TILEFILE)

 ALLOCATE(TMPVAR(NX+1,NY+1))
 ALLOCATE(GEOLON(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLON_W(ISTART:IEND+1,JSTART:JEND))
 ALLOCATE(GEOLON_S(ISTART:IEND,JSTART:JEND+1))

 ERROR=NF90_INQ_VARID(NCID, 'x', ID_VAR) 
 CALL NETCDF_ERROR(ERROR, 'ERROR READING X ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, TMPVAR)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING X RECORD' )

 DO J = JSTART, JEND
 DO I = ISTART, IEND
   II = 2*I
   JJ = 2*J
   GEOLON(I,J) = TMPVAR(II,JJ)
 ENDDO
 ENDDO

 DO J = JSTART, JEND
 DO I = ISTART, IEND+1
   II = (2*I) - 1
   JJ = 2*J
   GEOLON_W(I,J) = TMPVAR(II,JJ)
 ENDDO
 ENDDO

 DO J = JSTART, JEND+1
 DO I = ISTART, IEND
   II = 2*I
   JJ = (2*J) - 1
   GEOLON_S(I,J) = TMPVAR(II,JJ)
 ENDDO
 ENDDO

 ERROR=NF90_INQ_VARID(NCID, 'y', ID_VAR) 
 CALL NETCDF_ERROR(ERROR, 'ERROR READING Y ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, TMPVAR)
 CALL NETCDF_ERROR(ERROR, 'ERROR READING Y RECORD' )

 ERROR = NF_CLOSE(NCID)

 ALLOCATE(GEOLAT(ISTART:IEND,JSTART:JEND))
 ALLOCATE(GEOLAT_W(ISTART:IEND+1,JSTART:JEND))
 ALLOCATE(GEOLAT_S(ISTART:IEND,JSTART:JEND+1))

 DO J = JSTART, JEND
 DO I = ISTART, IEND
   II = 2*I
   JJ = 2*J
   GEOLAT(I,J) = TMPVAR(II,JJ)
 ENDDO
 ENDDO

 DO J = JSTART, JEND
 DO I = ISTART, IEND+1
   II = (2*I) - 1
   JJ = 2*J
   GEOLAT_W(I,J) = TMPVAR(II,JJ)
 ENDDO
 ENDDO

 DO J = JSTART, JEND+1
 DO I = ISTART, IEND
   II = 2*I
   JJ = (2*J) - 1
   GEOLAT_S(I,J) = TMPVAR(II,JJ)
 ENDDO
 ENDDO
 
 DEALLOCATE(TMPVAR)

 IF (N < 10) THEN
   WRITE(OUTFILE, "(A,I1,A)") 'gfs_data.tile', N, '.nc' 
 ELSE
   WRITE(OUTFILE, "(A,I2,A)") 'gfs_data.tile', N, '.nc'
 ENDIF

 ERROR = NF__CREATE(OUTFILE, IOR(NF_NETCDF4,NF_CLASSIC_MODEL),INITAL, FSIZE, NCID2)
 CALL NETCDF_ERROR(ERROR, 'CREATING FILE: '//TRIM(OUTFILE) )

 ERROR = NF90_DEF_DIM(NCID2, 'lon', IM_OUT, DIM_LON)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LON DIMENSION')

 ERROR = NF90_DEF_DIM(NCID2, 'lat', JM_OUT, DIM_LAT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LAT DIMENSION')

 ERROR = NF90_DEF_DIM(NCID2, 'lonp', (IM_OUT+1), DIM_LONP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LONP DIMENSION')

 ERROR = NF90_DEF_DIM(NCID2, 'latp', (JM_OUT+1), DIM_LATP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LATP DIMENSION')

 ERROR = NF90_DEF_DIM(NCID2, 'lev', LEVSO, DIM_LEV)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LEV DIMENSION')

 ERROR = NF90_DEF_DIM(NCID2, 'levp', LEVSO_P1, DIM_LEVP)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LEVP DIMENSION')

 ERROR = NF90_DEF_DIM(NCID2, 'ntracer', NTRACM, DIM_TRACER)
 CALL NETCDF_ERROR(ERROR, 'DEFINING NTRACER DIMENSION')

 ERROR = NF90_DEF_VAR(NCID2, 'lon', NF90_FLOAT, DIM_LON, ID_LON)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LON VARIABLE')

 ERROR = NF_PUT_ATT_TEXT(NCID2, ID_LON, "cartesian_axis", 1, "X")
 CALL NETCDF_ERROR(ERROR, 'DEFINING X-AXIS')

 ERROR = NF90_DEF_VAR(NCID2, 'lat', NF90_FLOAT, DIM_LAT, ID_LAT)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LAT VARIABLE')

 ERROR = NF_PUT_ATT_TEXT(NCID2, ID_LAT, "cartesian_axis", 1, "Y")
 CALL NETCDF_ERROR(ERROR, 'DEFINING Y-AXIS')

 ERROR = NF90_DEF_VAR(NCID2, 'ps', NF90_FLOAT, &
                             (/DIM_LON, DIM_LAT/), ID_PS)
 CALL NETCDF_ERROR(ERROR, 'DEFINING PS')
 
 ERROR = NF90_DEF_VAR(NCID2, 'w', NF90_FLOAT,  &
                             (/DIM_LON, DIM_LAT, DIM_LEV/), ID_W)
 CALL NETCDF_ERROR(ERROR, 'DEFINING W')

 ERROR = NF90_DEF_VAR(NCID2, 'zh', NF90_FLOAT,  &
                             (/DIM_LON, DIM_LAT, DIM_LEVP/), ID_ZH)
 CALL NETCDF_ERROR(ERROR, 'DEFINING ZH')

 ERROR = NF90_DEF_VAR(NCID2, 'sphum', NF90_FLOAT, &
                             (/DIM_LON, DIM_LAT, DIM_LEV/), ID_SPHUM)
 CALL NETCDF_ERROR(ERROR, 'DEFINING SPHUM')

 ERROR = NF90_DEF_VAR(NCID2, 'o3mr', NF90_FLOAT, &
                             (/DIM_LON, DIM_LAT, DIM_LEV/), ID_O3MR)
 CALL NETCDF_ERROR(ERROR, 'DEFINING O3MR')

 ERROR = NF90_DEF_VAR(NCID2, 'liq_wat', NF90_FLOAT, &
                             (/DIM_LON, DIM_LAT, DIM_LEV/), ID_CLWMR)
 CALL NETCDF_ERROR(ERROR, 'DEFINING LIQ_WAT')

 ERROR = NF90_DEF_VAR(NCID2, 'u_w', NF90_FLOAT, &
                             (/DIM_LONP, DIM_LAT, DIM_LEV/), ID_U_W)
 CALL NETCDF_ERROR(ERROR, 'DEFINING U_W')

 ERROR = NF90_DEF_VAR(NCID2, 'v_w', NF90_FLOAT, &
                             (/DIM_LONP, DIM_LAT, DIM_LEV/), ID_V_W)
 CALL NETCDF_ERROR(ERROR, 'DEFINING V_W')

 ERROR = NF90_DEF_VAR(NCID2, 'u_s', NF90_FLOAT,  &
                             (/DIM_LON, DIM_LATP, DIM_LEV/), ID_U_S)
 CALL NETCDF_ERROR(ERROR, 'DEFINING U_S')

 ERROR = NF90_DEF_VAR(NCID2, 'v_s', NF90_FLOAT,  &
                             (/DIM_LON, DIM_LATP, DIM_LEV/), ID_V_S)
 CALL NETCDF_ERROR(ERROR, 'DEFINING V_S')

 ERROR = NF__ENDDEF(NCID2, HEADER_BUFFER_VAL, 4, 0, 4)
 CALL NETCDF_ERROR(ERROR, 'DEFINING END OF HEADER')

!------------------------------------------------------------------
! Write out data.  fv3 convention: lowest model level is levso.
! top model layer is 1.  this is opposite the gfs convention.
!------------------------------------------------------------------

 ALLOCATE(CUBE_2D(ISTART:IEND,JSTART:JEND), CUBE_2D_4BYTE(ISTART:IEND,JSTART:JEND))

 CUBE_2D_4BYTE = REAL(GEOLON,4)
 ERROR = NF90_PUT_VAR(NCID2, ID_LON, CUBE_2D_4BYTE(:,JSTART))
 CALL NETCDF_ERROR(ERROR, 'WRITING LON')

 CUBE_2D_4BYTE = REAL(GEOLAT,4)
 ERROR = NF90_PUT_VAR(NCID2, ID_LAT, CUBE_2D_4BYTE(ISTART,:))
 CALL NETCDF_ERROR(ERROR, 'WRITING LAT')

 CALL GL2ANY(0,1,PS,LONB,LATB,CUBE_2D,IM_OUT,JM_OUT,GEOLON, GEOLAT)
 CUBE_2D_4BYTE = REAL(CUBE_2D,4)

 ERROR = NF90_PUT_VAR(NCID2, ID_PS, CUBE_2D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING PS')
    
 DEALLOCATE(CUBE_2D_4BYTE, CUBE_2D)

 ALLOCATE(CUBE_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(CUBE_3D(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANY(0,LEVSO,W,LONB,LATB,CUBE_3D,IM_OUT,JM_OUT,GEOLON, GEOLAT)
 DO K = 1, LEVSO
   CUBE_3D_4BYTE(:,:,LEVSO-K+1) = REAL(CUBE_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_W, CUBE_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING W')

 DEALLOCATE(CUBE_3D_4BYTE, CUBE_3D)

 ALLOCATE(CUBE_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO_P1))
 ALLOCATE(CUBE_3D(ISTART:IEND,JSTART:JEND,LEVSO_P1))

 CALL GL2ANY(0,LEVSO_P1,ZH,LONB,LATB,CUBE_3D,IM_OUT,JM_OUT,GEOLON, GEOLAT)
 DO K = 1, LEVSO_P1
   CUBE_3D_4BYTE(:,:,LEVSO_P1-K+1) = REAL(CUBE_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_ZH, CUBE_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING ZH')

 DEALLOCATE(CUBE_3D, CUBE_3D_4BYTE)

 ALLOCATE(CUBE_3D_4BYTE(ISTART:IEND,JSTART:JEND,LEVSO))
 ALLOCATE(CUBE_3D(ISTART:IEND,JSTART:JEND,LEVSO))

 CALL GL2ANY(0,LEVSO,Q(:,:,:,1),LONB,LATB,CUBE_3D,IM_OUT,JM_OUT,GEOLON, GEOLAT)
 DO K = 1, LEVSO
   CUBE_3D_4BYTE(:,:,LEVSO-K+1) = REAL(CUBE_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_SPHUM, CUBE_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING SPHUM')

 CALL GL2ANY(0,LEVSO,Q(:,:,:,2),LONB,LATB,CUBE_3D,IM_OUT,JM_OUT,GEOLON, GEOLAT)
 DO K = 1, LEVSO
   CUBE_3D_4BYTE(:,:,LEVSO-K+1) = REAL(CUBE_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_O3MR, CUBE_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING O3MR')

 CALL GL2ANY(0,LEVSO,Q(:,:,:,3),LONB,LATB,CUBE_3D,IM_OUT,JM_OUT,GEOLON, GEOLAT)
 DO K = 1, LEVSO
   CUBE_3D_4BYTE(:,:,LEVSO-K+1) = REAL(CUBE_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_CLWMR, CUBE_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING CLWMR')

 DEALLOCATE (CUBE_3D, CUBE_3D_4BYTE)

 ALLOCATE(CUBE_3D_4BYTE(ISTART:IEND+1,JSTART:JEND,LEVSO))
 ALLOCATE(CUBE_3D(ISTART:IEND+1,JSTART:JEND,LEVSO))
 ALLOCATE(CUBE_3D2(ISTART:IEND+1,JSTART:JEND,LEVSO))

 CALL GL2ANYV(0,LEVSO,U,V,LONB,LATB,CUBE_3D,CUBE_3D2,(IM_OUT+1),JM_OUT,GEOLON_W, GEOLAT_W)

 DO K = 1, LEVSO
   CUBE_3D_4BYTE(:,:,LEVSO-K+1) = REAL(CUBE_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_U_W, CUBE_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING U_W')

 DO K = 1, LEVSO
   CUBE_3D_4BYTE(:,:,LEVSO-K+1) = REAL(CUBE_3D2(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_V_W, CUBE_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING V_W')

 DEALLOCATE (CUBE_3D, CUBE_3D2, CUBE_3D_4BYTE)

 ALLOCATE(CUBE_3D_4BYTE(ISTART:IEND,JSTART:JEND+1,LEVSO))
 ALLOCATE(CUBE_3D(ISTART:IEND,JSTART:JEND+1,LEVSO))
 ALLOCATE(CUBE_3D2(ISTART:IEND,JSTART:JEND+1,LEVSO))

 CALL GL2ANYV(0,LEVSO,U,V,LONB,LATB,CUBE_3D,CUBE_3D2,IM_OUT,(JM_OUT+1),GEOLON_S, GEOLAT_S)

 DO K = 1, LEVSO
   CUBE_3D_4BYTE(:,:,LEVSO-K+1) = REAL(CUBE_3D(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_U_S, CUBE_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING U_S')

 DO K = 1, LEVSO
   CUBE_3D_4BYTE(:,:,LEVSO-K+1) = REAL(CUBE_3D2(:,:,K),4)
 ENDDO

 ERROR = NF90_PUT_VAR(NCID2, ID_V_S, CUBE_3D_4BYTE)
 CALL NETCDF_ERROR(ERROR, 'WRITING V_S')

 ERROR = NF_CLOSE(NCID2)

 DEALLOCATE(CUBE_3D, CUBE_3D2, CUBE_3D_4BYTE)
 DEALLOCATE(GEOLON, GEOLON_W, GEOLON_S)
 DEALLOCATE(GEOLAT, GEOLAT_W, GEOLAT_S)

 ENDDO TILE_LOOP

 DEALLOCATE(ZH)

 END SUBROUTINE WRITE_FV3_ATMS_NETCDF

 SUBROUTINE READ_GFS_NSST_DATA_NSTIO(IMI,JMI,NUM_NSST_FIELDS,     &
                                     NSST_INPUT, MASK_INPUT, &
                                     NSST_YEAR, NSST_MON,    &
                                     NSST_DAY, NSST_HOUR,    &
                                     NSST_FHOUR)

 USE NSTIO_MODULE

 IMPLICIT NONE

 INTEGER, INTENT(IN)  :: IMI, JMI, NUM_NSST_FIELDS
 INTEGER, INTENT(OUT) :: NSST_YEAR, NSST_MON
 INTEGER, INTENT(OUT) :: NSST_DAY, NSST_HOUR

 REAL, INTENT(OUT)   :: NSST_FHOUR
 REAL, INTENT(OUT)   :: MASK_INPUT(IMI,JMI)
 REAL, INTENT(OUT)   :: NSST_INPUT(IMI,JMI,NUM_NSST_FIELDS)

 INTEGER(NSTIO_INTKIND) :: NSSTI, IRET

 TYPE(NSTIO_HEAD)        :: NSST_IN_HEAD
 TYPE(NSTIO_DATA)        :: NSST_IN_DATA

 PRINT*,'- READ NSST FILE chgres.inp.nst.'
!  OPEN NSST FILES
 NSSTI=31
 CALL NSTIO_SROPEN(NSSTI,'chgres.inp.nst',IRET)
 IF(IRET/=0)THEN
   PRINT*,'FATAL ERROR OPENING chgres.inp.nst ', IRET
   CALL ERREXIT(105)
 ENDIF
 CALL NSTIO_SRHEAD(NSSTI,NSST_IN_HEAD,IRET)
 IF(IRET/=0)THEN
   PRINT*,'FATAL ERROR READING chgres.inp.nst ', IRET
   CALL ERREXIT(106)
 ENDIF
 CALL NSTIO_ALDATA(NSST_IN_HEAD,NSST_IN_DATA,IRET)
 CALL NSTIO_SRDATA(NSSTI,NSST_IN_HEAD,NSST_IN_DATA,IRET)
 IF(IRET/=0)THEN
   PRINT*,'FATAL ERROR READING chgres.inp.nst ', IRET
   CALL ERREXIT(107)
 ENDIF
 NSST_YEAR=NSST_IN_HEAD%IDATE(4)
 NSST_MON=NSST_IN_HEAD%IDATE(2)
 NSST_DAY=NSST_IN_HEAD%IDATE(3)
 NSST_HOUR=NSST_IN_HEAD%IDATE(1)
 NSST_FHOUR=NSST_IN_HEAD%FHOUR
 NSST_INPUT(:,:,1)=NSST_IN_DATA%XT
 NSST_INPUT(:,:,2)=NSST_IN_DATA%XS
 NSST_INPUT(:,:,3)=NSST_IN_DATA%XU
 NSST_INPUT(:,:,4)=NSST_IN_DATA%XV
 NSST_INPUT(:,:,5)=NSST_IN_DATA%XZ
 NSST_INPUT(:,:,6)=NSST_IN_DATA%ZM
 NSST_INPUT(:,:,7)=NSST_IN_DATA%XTTS
 NSST_INPUT(:,:,8)=NSST_IN_DATA%XZTS
 NSST_INPUT(:,:,9)=NSST_IN_DATA%DT_COOL
 NSST_INPUT(:,:,10)=NSST_IN_DATA%Z_C
 NSST_INPUT(:,:,11)=NSST_IN_DATA%C_0
 NSST_INPUT(:,:,12)=NSST_IN_DATA%C_D
 NSST_INPUT(:,:,13)=NSST_IN_DATA%W_0
 NSST_INPUT(:,:,14)=NSST_IN_DATA%W_D
 NSST_INPUT(:,:,15)=NSST_IN_DATA%D_CONV
 NSST_INPUT(:,:,16)=NSST_IN_DATA%IFD
 NSST_INPUT(:,:,17)=NSST_IN_DATA%TREF
 NSST_INPUT(:,:,18)=NSST_IN_DATA%QRAIN
 MASK_INPUT=NSST_IN_DATA%SLMSK
 CALL NSTIO_AXDATA(NSST_IN_DATA,IRET)
 CALL NSTIO_SRCLOSE(NSSTI,IRET)

 END SUBROUTINE READ_GFS_NSST_DATA_NSTIO

 SUBROUTINE READ_GFS_NSST_DATA_NEMSIO (MASK_INPUT,NSST_INPUT,IMI,JMI, &
                 NUM_NSST_FIELDS,NSST_YEAR,NSST_MON,NSST_DAY,    & 
                 NSST_HOUR,NSST_FHOUR)

!-----------------------------------------------------------------------
! Subroutine: read nsst data from nemsio file
!
! Author: George Gayno/EMC
!
! Abstract: Reads an nsst file in nemsio format.  Places data
!           in the "nsst_input" array as expected by routine
!           nsst_chgres.
!
! Input files: 
!    "chgres.inp.nst" - input nsst nemsio file
!
! Output files:  none
!
! History:
!   2016-04-05   Gayno - Initial version
!
! Condition codes:  all non-zero codes are fatal
!    16 - bad initialization of nemsio environment
!    17 - bad open of nst file "chgres.inp.nst"
!    18 - bad read of "chgres.inp.nst" header
!    19 - the program assumes that the resolution of the
!         nst grid matches the input surface grid.  if
!         they are not the same, stop procoessing.
!    20 - the nst file does not have the 19 required records.
!    21 - bad read of an nst file record.
!-----------------------------------------------------------------------

 use nemsio_module

 implicit none

 character(len=3)        :: levtyp
 character(len=8)        :: recname(19)

 integer, intent(in)     :: imi, jmi, num_nsst_fields
 integer, intent(out)    :: nsst_year, nsst_mon
 integer, intent(out)    :: nsst_day, nsst_hour

 real,    intent(out)    :: mask_input(imi,jmi)
 real,    intent(out)    :: nsst_input(imi,jmi,num_nsst_fields)
 real,    intent(out)    :: nsst_fhour

 integer(nemsio_intkind) :: iret, nrec, dimx, dimy, lev, nframe
 integer(nemsio_intkind) :: idate(7), nfhour

 integer                 :: j

 real(nemsio_realkind),allocatable :: dummy(:)

 type(nemsio_gfile)      :: gfile

 data recname   /"land    ", "xt      ", "xs      ", &
                 "xu      ", "xv      ", "xz      ", &
                 "zm      ", "xtts    ", "xzts    ", &
                 "dtcool  ", "zc      ", "c0      ", &
                 "cd      ", "w0      ", "wd      ", &
                 "dconv   ", "ifd     ", "tref    ", &
                 "qrain   " /

 print*,"- READ INPUT NSST DATA IN NEMSIO FORMAT"

 call nemsio_init(iret=iret)
 if (iret /= 0) then
   print*,"- FATAL ERROR: bad nemsio initialization."
   print*,"- IRET IS ", iret
   call errexit(108)
 endif

 call nemsio_open(gfile, "chgres.inp.nst", "read", iret=iret)
 if (iret /= 0) then
   print*,"- FATAL ERROR: bad open of chgres.inp.nst."
   print*,"- IRET IS ", iret
   call errexit(109)
 endif

 print*,"- READ FILE HEADER"
 call nemsio_getfilehead(gfile,iret=iret,nrec=nrec,dimx=dimx, &
           dimy=dimy,idate=idate,nfhour=nfhour)
 if (iret /= 0) then
   print*,"- FATAL ERROR: bad read of chgres.inp.nst header."
   print*,"- IRET IS ", iret
   call errexit(110)
 endif

 if (dimx /= imi .or. dimy /= jmi) then
   print*,"- FATAL ERROR: nst and sfc file resolution"
   print*,"- must be the same."
   call errexit(111)
 endif

 if (nrec /= 19) then
   print*,"- FATAL ERROR: nst file has wrong number of records."
   call errexit(112)
 endif

 nsst_year=idate(1)
 nsst_mon=idate(2)
 nsst_day=idate(3)
 nsst_hour=idate(4)
 nsst_fhour=float(nfhour)

 levtyp='sfc'
 lev=1
 nframe=0

 allocate(dummy(imi*jmi))

!-----------------------------------------------------------------------
! Read land mask.  Note: older file versions use 'slmsk'
! as the header id.
!-----------------------------------------------------------------------

 call nemsio_readrecv(gfile,recname(1),levtyp,lev, &
           dummy,nframe,iret)
 if (iret /= 0) then
   call nemsio_readrecv(gfile,"slmsk",levtyp,lev, &
             dummy,nframe,iret)
   if (iret /= 0) then
     print*,"- FATAL ERROR: bad read of landmask record."
     print*,"- IRET IS ", iret
     call errexit(113)
   endif
 endif
 mask_input = reshape (dummy, (/imi,jmi/))

 print*,"- READ DATA RECORDS"
 do j = 2, nrec
   call nemsio_readrecv(gfile,recname(j),levtyp,lev, &
             dummy,nframe,iret)
   if (iret /= 0) then
     print*,"- FATAL ERROR: bad read of chgres.inp.nst."
     print*,"- IRET IS ", iret
     call errexit(114)
   endif
   nsst_input(:,:,j-1) = reshape (dummy, (/imi,jmi/))
 enddo

 deallocate(dummy)

 call nemsio_close(gfile,iret=iret)

 call nemsio_finalize()

 END SUBROUTINE READ_GFS_NSST_DATA_NEMSIO

 SUBROUTINE READ_GFS_SFC_HEADER_NEMSIO (IMI,JMI,IVSI,LSOILI, &
                 FCSTHOUR,IDATE4O,KGDS_INPUT)

 USE NEMSIO_MODULE

 IMPLICIT NONE

 INTEGER, INTENT(OUT)     :: IMI,JMI,IVSI,LSOILI,IDATE4O(4)
 INTEGER, INTENT(OUT)     :: KGDS_INPUT(200)

 REAL, INTENT(OUT)        :: FCSTHOUR

 CHARACTER(LEN=8)         :: FILETYPE

 INTEGER(NEMSIO_INTKIND)  :: DIMX, DIMY, IRET, VERSION
 INTEGER(NEMSIO_INTKIND)  :: NSOIL, IDATE(7), NFHOUR

 TYPE(NEMSIO_GFILE)       :: GFILEISFC

 CALL NEMSIO_OPEN(GFILEISFC,'chgres.inp.sfc','read',IRET=IRET)
 IF (IRET /= 0) THEN
   PRINT*,"FATAL ERROR OPENING chgres.inp.sfc"
   PRINT*,"IRET IS: ",IRET
   CALL ERREXIT(119)
 ENDIF

 CALL NEMSIO_GETFILEHEAD(GFILEISFC,GTYPE=FILETYPE,IRET=IRET, &
           VERSION=VERSION, DIMX=DIMX, DIMY=DIMY, NSOIL=NSOIL, &
           IDATE=IDATE, NFHOUR=NFHOUR)
 IF (IRET /= 0) THEN
   PRINT*,"FATAL ERROR READING chgres.inp.sfc FILE HEADER."
   PRINT*,"IRET IS: ",IRET
   CALL ERREXIT(120)
 ENDIF

! check bad status

 CALL NEMSIO_CLOSE(GFILEISFC,IRET=IRET)

 IMI        = DIMX
 JMI        = DIMY
 LSOILI     = NSOIL
 IVSI       = VERSION
 FCSTHOUR   = FLOAT(NFHOUR)
 IDATE4O(1) = IDATE(4)  ! HOUR
 IDATE4O(2) = IDATE(2)  ! MONTH
 IDATE4O(3) = IDATE(3)  ! DAY
 IDATE4O(4) = IDATE(1)  ! YEAR
     
 KGDS_INPUT = 0
 KGDS_INPUT(1) = 4          ! OCT 6 - TYPE OF GRID (GAUSSIAN)
 KGDS_INPUT(2) = IMI        ! OCT 7-8 - # PTS ON LATITUDE CIRCLE
 KGDS_INPUT(3) = JMI        ! OCT 9-10 - # PTS ON LONGITUDE CIRCLE
 KGDS_INPUT(4) = 90000      ! OCT 11-13 - LAT OF ORIGIN
 KGDS_INPUT(5) = 0          ! OCT 14-16 - LON OF ORIGIN
 KGDS_INPUT(6) = 128        ! OCT 17 - RESOLUTION FLAG
 KGDS_INPUT(7) = -90000     ! OCT 18-20 - LAT OF EXTREME POINT
 KGDS_INPUT(8) = NINT(-360000./IMI)  ! OCT 21-23 - LON OF EXTREME POINT
 KGDS_INPUT(9)  = NINT((360.0 / FLOAT(IMI))*1000.0)
                            ! OCT 24-25 - LONGITUDE DIRECTION INCR.
 KGDS_INPUT(10) = JMI /2    ! OCT 26-27 - NUMBER OF CIRCLES POLE TO EQUATOR
 KGDS_INPUT(12) = 255       ! OCT 29 - RESERVED
 KGDS_INPUT(20) = 255       ! OCT 5  - NOT USED, SET TO 255

 END SUBROUTINE READ_GFS_SFC_HEADER_NEMSIO

 SUBROUTINE READ_GFS_SFC_HEADER_SFCIO (NSFCI,IMI,JMI,IVSI,LSOILI, &
                 FCSTHOUR,IDATE4O,KGDS_INPUT)

 USE SFCIO_MODULE

 INTEGER, INTENT(IN)  :: NSFCI
 INTEGER, INTENT(OUT) :: IMI,JMI,IVSI,LSOILI,IDATE4O(4)
 INTEGER, INTENT(OUT) :: KGDS_INPUT(200)
 INTEGER              :: IRET

 REAL, INTENT(OUT)    :: FCSTHOUR

 TYPE(SFCIO_HEAD)     :: SFCHEADI

 CALL SFCIO_SROPEN(NSFCI,'chgres.inp.sfc',IRET)
 IF (IRET /= 0) THEN
   PRINT*,"FATAL ERROR OPENING chgres.inp.sfc"
   PRINT*,"IRET IS: ", IRET
   CALL ERREXIT(121)
 ENDIF

 CALL SFCIO_SRHEAD(NSFCI,SFCHEADI,IRET)
 IF (IRET /= 0) THEN
   PRINT*,"FATAL ERROR READING chgres.inp.sfc HEADER"
   PRINT*,"IRET IS: ", IRET
   CALL ERREXIT(122)
 ENDIF

 CALL SFCIO_SCLOSE(NSFCI,IRET)

 IMI = SFCHEADI%LONB
 JMI = SFCHEADI%LATB
 IVSI = SFCHEADI%IVS
 LSOILI = SFCHEADI%LSOIL
 FCSTHOUR = SFCHEADI%FHOUR
 IDATE4O = SFCHEADI%IDATE

 KGDS_INPUT = 0
 KGDS_INPUT(1) = 4          ! OCT 6 - TYPE OF GRID (GAUSSIAN)
 KGDS_INPUT(2) = IMI        ! OCT 7-8 - # PTS ON LATITUDE CIRCLE
 KGDS_INPUT(3) = JMI        ! OCT 9-10 - # PTS ON LONGITUDE CIRCLE
 KGDS_INPUT(4) = 90000      ! OCT 11-13 - LAT OF ORIGIN
 KGDS_INPUT(5) = 0          ! OCT 14-16 - LON OF ORIGIN
 KGDS_INPUT(6) = 128        ! OCT 17 - RESOLUTION FLAG
 KGDS_INPUT(7) = -90000     ! OCT 18-20 - LAT OF EXTREME POINT
 KGDS_INPUT(8) = NINT(-360000./IMI)  ! OCT 21-23 - LON OF EXTREME POINT
 KGDS_INPUT(9)  = NINT((360.0 / FLOAT(IMI))*1000.0)
                            ! OCT 24-25 - LONGITUDE DIRECTION INCR.
 KGDS_INPUT(10) = JMI /2    ! OCT 26-27 - NUMBER OF CIRCLES POLE TO EQUATOR
 KGDS_INPUT(12) = 255       ! OCT 29 - RESERVED
 KGDS_INPUT(20) = 255       ! OCT 5  - NOT USED, SET TO 255

 END SUBROUTINE READ_GFS_SFC_HEADER_SFCIO

 SUBROUTINE READ_GFS_SFC_DATA_NEMSIO (IMI, JMI, LSOILI, IVSI, SFCINPUT, &
                                 F10MI, T2MI, Q2MI,  &
                                 UUSTARI, FFMMI, FFHHI, SRFLAGI, &
                                 TPRCPI)

 USE NEMSIO_MODULE
 USE NEMSIO_GFS
 USE SURFACE_CHGRES

 IMPLICIT NONE

 INTEGER, INTENT(IN)  :: IMI, JMI, LSOILI, IVSI

 REAL, INTENT(OUT)    :: F10MI(IMI,JMI), T2MI(IMI,JMI)
 REAL, INTENT(OUT)    :: Q2MI(IMI,JMI), UUSTARI(IMI,JMI)
 REAL, INTENT(OUT)    :: FFMMI(IMI,JMI), FFHHI(IMI,JMI)
 REAL, INTENT(OUT)    :: SRFLAGI(IMI,JMI), TPRCPI(IMI,JMI)

 INTEGER(NEMSIO_INTKIND)  :: IRET
 INTEGER                  :: I, J, L

 TYPE(SFC2D)          :: SFCINPUT
 TYPE(NEMSIO_GFILE)   :: GFILEISFC
 TYPE(NEMSIO_DBTA)    :: GFSDATAI

 CALL NEMSIO_OPEN(GFILEISFC,'chgres.inp.sfc','read',IRET=IRET)
 IF(IRET /= 0)THEN
   PRINT*,"FATAL ERROR OPENING chgres.inp.sfc"
   PRINT*,"IRET IS ", IRET
   CALL ERREXIT(144)
 ENDIF

 CALL NEMSIO_GFS_ALSFC(IMI, JMI, LSOILI, GFSDATAI)

 CALL NEMSIO_GFS_RDSFC(GFILEISFC,GFSDATAI,IRET)
 IF(IRET /= 0)THEN
   PRINT*,"FATAL ERROR READING DATA FROM chgres.inp.sfc"
   PRINT*,"IRET IS ", IRET
   CALL ERREXIT(145)
 ENDIF

 CALL NEMSIO_CLOSE(GFILEISFC, IRET=IRET)

!$OMP PARALLEL DO PRIVATE(I,J)
 DO J = 1, JMI
   DO I = 1, IMI

     SFCINPUT%ALNSF(I,J) = GFSDATAI%ALNSF(I,J)
     SFCINPUT%ALNWF(I,J) = GFSDATAI%ALNWF(I,J)
     SFCINPUT%ALVSF(I,J) = GFSDATAI%ALVSF(I,J)
     SFCINPUT%ALVWF(I,J) = GFSDATAI%ALVWF(I,J)
     SFCINPUT%CANOPY_MC(I,J) = GFSDATAI%CANOPY(I,J)
     SFCINPUT%GREENFRC(I,J) = GFSDATAI%VFRAC(I,J)
     SFCINPUT%FACSF(I,J) = GFSDATAI%FACSF(I,J)
     SFCINPUT%FACWF(I,J) = GFSDATAI%FACWF(I,J)
     SFCINPUT%SKIN_TEMP(I,J) = GFSDATAI%TSEA(I,J)
     SFCINPUT%LSMASK(I,J) = GFSDATAI%SLMSK(I,J)
     SFCINPUT%SEA_ICE_FLAG(I,J) = 0
     IF(NINT(SFCINPUT%LSMASK(I,J))==2) THEN
       SFCINPUT%LSMASK(I,J)=0.
       SFCINPUT%SEA_ICE_FLAG(I,J) = 1
      ENDIF
     SFCINPUT%Z0(I,J) = GFSDATAI%ZORL(I,J)
     SFCINPUT%OROG(I,J)         = GFSDATAI%OROG(I,J)
     SFCINPUT%VEG_TYPE(I,J)     = NINT(GFSDATAI%VTYPE(I,J))
     SFCINPUT%SOIL_TYPE(I,J)    = NINT(GFSDATAI%STYPE(I,J))
     SFCINPUT%SNOW_LIQ_EQUIV(I,J) = GFSDATAI%SHELEG(I,J)

   ENDDO
 ENDDO
!$OMP END PARALLEL DO
 
 DO L = 1, LSOILI
!$OMP PARALLEL DO PRIVATE(I,J)
   DO J = 1, JMI
     DO I = 1, IMI
       SFCINPUT%SOILM_TOT(I,J,L) = GFSDATAI%SMC(I,J,L)
       SFCINPUT%SOIL_TEMP(I,J,L) = GFSDATAI%STC(I,J,L)
     ENDDO
   ENDDO
!$OMP END PARALLEL DO
 ENDDO

 SRFLAGI = 0.0
 TPRCPI  = 0.0

 IF (IVSI >= 200501) THEN
!$OMP PARALLEL DO PRIVATE(I,J)
   DO J = 1, JMI
     DO I = 1, IMI
       SFCINPUT%SEA_ICE_FRACT(I,J) = GFSDATAI%FICE(I,J)
       SFCINPUT%SEA_ICE_DEPTH(I,J) = GFSDATAI%HICE(I,J)
       SFCINPUT%MXSNOW_ALB(I,J)    = GFSDATAI%SNOALB(I,J)
       SFCINPUT%SNOW_DEPTH(I,J)    = GFSDATAI%SNWDPH(I,J)
       SFCINPUT%SLOPE_TYPE(I,J)    = NINT(GFSDATAI%SLOPE(I,J))
       SFCINPUT%GREENFRC_MAX(I,J)  = GFSDATAI%SHDMAX(I,J)
       SFCINPUT%GREENFRC_MIN(I,J)  = GFSDATAI%SHDMIN(I,J)
       SRFLAGI(I,J)                = GFSDATAI%SRFLAG(I,J)
       TPRCPI(I,J)                 = GFSDATAI%TPRCP(I,J)
     ENDDO
   ENDDO
!$OMP END PARALLEL DO

   DO L=1,LSOILI
!$OMP PARALLEL DO PRIVATE(I,J)
     DO J = 1, JMI
       DO I = 1, IMI
         SFCINPUT%SOILM_LIQ(I,J,L) = GFSDATAI%SLC(I,J,L)
       ENDDO
     ENDDO
   ENDDO

 END IF  ! IVS >= 200501

!$OMP PARALLEL DO PRIVATE(I,J)
 DO J = 1, JMI
   DO I = 1, IMI
     F10MI(I,J) = GFSDATAI%F10M(I,J)
     T2MI(I,J) = GFSDATAI%T2M(I,J)
     Q2MI(I,J) = GFSDATAI%Q2M(I,J)
     UUSTARI(I,J) = GFSDATAI%UUSTAR(I,J)
     FFMMI(I,J) = GFSDATAI%FFMM(I,J)
     FFHHI(I,J) = GFSDATAI%FFHH(I,J)
   ENDDO
 ENDDO
!$OMP END PARALLEL DO

 END SUBROUTINE READ_GFS_SFC_DATA_NEMSIO

 SUBROUTINE READ_GFS_SFC_DATA_SFCIO (NSFCI, IMI, JMI, SFCINPUT,  &
                              F10MI, T2MI, Q2MI, &
                              UUSTARI, FFMMI, FFHHI, SRFLAGI, &
                              TPRCPI)

 USE SFCIO_MODULE
 USE SURFACE_CHGRES

 IMPLICIT NONE

 INTEGER, INTENT(IN)  :: NSFCI, IMI, JMI
 INTEGER              :: I,J,L, IRET

 REAL, INTENT(OUT)    :: F10MI(IMI,JMI), T2MI(IMI,JMI)
 REAL, INTENT(OUT)    :: Q2MI(IMI,JMI), UUSTARI(IMI,JMI)
 REAL, INTENT(OUT)    :: FFMMI(IMI,JMI), FFHHI(IMI,JMI)
 REAL, INTENT(OUT)    :: SRFLAGI(IMI,JMI), TPRCPI(IMI,JMI)

 TYPE(SFC2D)          :: SFCINPUT
 TYPE(SFCIO_HEAD)     :: SFCHEADI
 TYPE(SFCIO_DBTA)     :: SFCDATAI

 CALL SFCIO_SROPEN(NSFCI,'chgres.inp.sfc',IRET)
 IF(IRET /=0) THEN
   PRINT*,"FATAL ERROR OPENING chgres.inp.sfc"
   PRINT*,"IRET IS ", IRET
   CALL ERREXIT(155)
 ENDIF

 CALL SFCIO_SRHEAD(NSFCI,SFCHEADI,IRET)
 IF(IRET /=0) THEN
   PRINT*,"FATAL ERROR READING chgres.inp.sfc HEADER"
   PRINT*,"IRET IS ", IRET
   CALL ERREXIT(156)
 ENDIF

 CALL SFCIO_ALDBTA(SFCHEADI,SFCDATAI,IRET)
 IF(IRET.NE.0) THEN
   PRINT*,"FATAL ERROR ALLOCATING SFC DATA STRUCTURE"
   PRINT*,"IRET IS ", IRET
   CALL ERREXIT(158)
 ENDIF

 CALL SFCIO_SRDBTA(NSFCI,SFCHEADI,SFCDATAI,IRET)
 IF(IRET /=0) THEN
   PRINT*,"FATAL ERROR READING chgres.inp.sfc DATA"
   PRINT*,"IRET IS ", IRET
   CALL ERREXIT(157)
 ENDIF

 CALL SFCIO_SCLOSE(NSFCI,IRET)

!$OMP PARALLEL DO PRIVATE(I,J)

 DO J = 1, SFCHEADI%LATB
 DO I = 1, SFCHEADI%LONB

   SFCINPUT%ALNSF(I,J) = SFCDATAI%ALNSF(I,J)
   SFCINPUT%ALNWF(I,J) = SFCDATAI%ALNWF(I,J)
   SFCINPUT%ALVSF(I,J) = SFCDATAI%ALVSF(I,J)
   SFCINPUT%ALVWF(I,J) = SFCDATAI%ALVWF(I,J)
   SFCINPUT%CANOPY_MC(I,J) = SFCDATAI%CANOPY(I,J)
   SFCINPUT%GREENFRC(I,J) = SFCDATAI%VFRAC(I,J)
   SFCINPUT%FACSF(I,J) = SFCDATAI%FACSF(I,J)
   SFCINPUT%FACWF(I,J) = SFCDATAI%FACWF(I,J)
   SFCINPUT%SKIN_TEMP(I,J) = SFCDATAI%TSEA(I,J)
   SFCINPUT%LSMASK(I,J) = SFCDATAI%SLMSK(I,J)
   SFCINPUT%SEA_ICE_FLAG(I,J) = 0
   IF(NINT(SFCINPUT%LSMASK(I,J))==2) THEN
     SFCINPUT%LSMASK(I,J)=0.
     SFCINPUT%SEA_ICE_FLAG(I,J) = 1
   ENDIF
   SFCINPUT%Z0(I,J) = SFCDATAI%ZORL(I,J)
   SFCINPUT%OROG(I,J)         = SFCDATAI%OROG(I,J)
   SFCINPUT%VEG_TYPE(I,J)     = NINT(SFCDATAI%VTYPE(I,J))
   SFCINPUT%SOIL_TYPE(I,J)    = NINT(SFCDATAI%STYPE(I,J))
   SFCINPUT%SNOW_LIQ_EQUIV(I,J) = SFCDATAI%SHELEG(I,J)

 ENDDO
 ENDDO

!$OMP END PARALLEL DO

 DO L = 1, SFCHEADI%LSOIL
!$OMP PARALLEL DO PRIVATE(I,J)
   DO J = 1, SFCHEADI%LATB
     DO I = 1, SFCHEADI%LONB
       SFCINPUT%SOILM_TOT(I,J,L) = SFCDATAI%SMC(I,J,L)
       SFCINPUT%SOIL_TEMP(I,J,L) = SFCDATAI%STC(I,J,L)
     ENDDO
   ENDDO
!$OMP END PARALLEL DO
 ENDDO

 SRFLAGI = 0.0
 TPRCPI  = 0.0

 IF (SFCHEADI%IVS >= 200501) THEN
!$OMP PARALLEL DO PRIVATE(I,J)
   DO J = 1, SFCHEADI%LATB
     DO I = 1, SFCHEADI%LONB
       SFCINPUT%SEA_ICE_FRACT(I,J) = SFCDATAI%FICE(I,J)
       SFCINPUT%SEA_ICE_DEPTH(I,J) = SFCDATAI%HICE(I,J)
       SFCINPUT%MXSNOW_ALB(I,J)    = SFCDATAI%SNOALB(I,J)
       SFCINPUT%SNOW_DEPTH(I,J)    = SFCDATAI%SNWDPH(I,J)
       SFCINPUT%SLOPE_TYPE(I,J)    = NINT(SFCDATAI%SLOPE(I,J))
       SFCINPUT%GREENFRC_MAX(I,J)  = SFCDATAI%SHDMAX(I,J)
       SFCINPUT%GREENFRC_MIN(I,J)  = SFCDATAI%SHDMIN(I,J)
       SRFLAGI(I,J)                = SFCDATAI%SRFLAG(I,J)
       TPRCPI(I,J)                 = SFCDATAI%TPRCP(I,J)
     ENDDO
   ENDDO
!$OMP END PARALLEL DO

   DO L=1,SFCHEADI%LSOIL
!$OMP PARALLEL DO PRIVATE(I,J)
     DO J = 1, SFCHEADI%LATB
       DO I = 1, SFCHEADI%LONB
         SFCINPUT%SOILM_LIQ(I,J,L) = SFCDATAI%SLC(I,J,L)
       ENDDO
     ENDDO
   ENDDO

 END IF  ! IVS >= 200501

!$OMP PARALLEL DO PRIVATE(I,J)
 DO J = 1, SFCHEADI%LATB
   DO I = 1, SFCHEADI%LONB
     F10MI(I,J) = SFCDATAI%F10M(I,J)
     T2MI(I,J) = SFCDATAI%T2M(I,J)
     Q2MI(I,J) = SFCDATAI%Q2M(I,J)
     UUSTARI(I,J) = SFCDATAI%UUSTAR(I,J)
     FFMMI(I,J) = SFCDATAI%FFMM(I,J)
     FFHHI(I,J) = SFCDATAI%FFHH(I,J)
   ENDDO
 ENDDO
!$OMP END PARALLEL DO

 CALL SFCIO_AXDBTA(SFCDATAI,IRET)

 END SUBROUTINE READ_GFS_SFC_DATA_SFCIO
