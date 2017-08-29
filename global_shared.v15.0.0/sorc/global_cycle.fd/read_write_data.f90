 MODULE READ_WRITE_DATA

 PRIVATE

 PUBLIC :: READ_SFC_NETCDF
 PUBLIC :: READ_LAT_LON_OROG
 PUBLIC :: WRITE_DATA

 CONTAINS

 subroutine write_data(slifcs,tsffcs,snofcs,tg3fcs,zorfcs, &
                       albfcs,alffcs,vegfcs,cnpfcs,f10m, &
                       t2m,q2m,vetfcs,sotfcs,ustar,fmm,fhh, &
                       sicfcs,sihfcs,sitfcs,tprcp,srflag, &
                       swdfcs,vmnfcs,vmxfcs,slpfcs, &
                       absfcs,slcfcs,smcfcs,stcfcs,&
                       idim,jdim,lensfc,lsoil,fnbgso)

! Note: the model restart files contain an additional snow field -
! snow cover (snocvr).  That field is required for bit identical
! reproducability.  If that record does not exist, the model
! will compute it as an initialization step.  Because this
! program will update snow, it let the model update snow
! cover by not outputting this field.

 use netcdf

 implicit none

 character(len=*), intent(in) :: fnbgso

 integer, intent(in)         :: idim, jdim, lensfc, lsoil

 real, intent(in)            :: slifcs(lensfc), tsffcs(lensfc)
 real, intent(in)            :: snofcs(lensfc), tg3fcs(lensfc)
 real, intent(in)            :: vegfcs(lensfc), cnpfcs(lensfc)
 real, intent(in)            :: zorfcs(lensfc), albfcs(lensfc,4)
 real, intent(in)            :: f10m(lensfc), alffcs(lensfc,2)
 real, intent(in)            :: t2m(lensfc), q2m(lensfc)
 real, intent(in)            :: vetfcs(lensfc), sotfcs(lensfc)
 real, intent(in)            :: ustar(lensfc), fmm(lensfc)
 real, intent(in)            :: fhh(lensfc), sicfcs(lensfc)
 real, intent(in)            :: sihfcs(lensfc), sitfcs(lensfc)
 real, intent(in)            :: tprcp(lensfc), srflag(lensfc)
 real, intent(in)            :: swdfcs(lensfc), vmnfcs(lensfc)
 real, intent(in)            :: vmxfcs(lensfc), slpfcs(lensfc)
 real, intent(in)            :: absfcs(lensfc), slcfcs(lensfc,lsoil)
 real, intent(in)            :: smcfcs(lensfc,lsoil), stcfcs(lensfc,lsoil)

 logical :: do_nsst

 integer                     :: fsize=65536, inital=0
 integer                     :: header_buffer_val = 16384
 integer                     :: dims_3d(3), dims_strt(3), dims_end(3)
 integer                     :: dims_4d(4), dims4_strt(4), dims4_end(4)
 integer                     :: error, i, ncid
 integer                     :: dim_x, dim_y, dim_lsoil, dim_time
 integer                     :: id_x, id_y, id_lsoil, id_time
 integer                     :: id_slmsk, id_tsea, id_sheleg
 integer                     :: id_alnwf, id_alvwf, id_alnsf, id_alvsf
 integer                     :: id_tg3, id_zorl, id_facsf, id_facwf
 integer                     :: id_vfrac, id_canopy, id_f10m, id_t2m
 integer                     :: id_q2m, id_stype, id_vtype, id_uustar
 integer                     :: id_ffmm, id_ffhh, id_fice, id_hice
 integer                     :: id_tisfc, id_tprcp, id_srflag
 integer                     :: id_snwdph, id_shdmin, id_shdmax
 integer                     :: id_slope, id_snoalb, id_qrain
 integer                     :: id_dt_cool, id_ifd, id_d_conv
 integer                     :: id_xzts, id_xtts, id_zm, id_xz
 integer                     :: id_xv, id_xu, id_xs, id_xt
 integer                     :: id_w_d, id_w_0, id_c_d
 integer                     :: id_c_0, id_z_c, id_tref
 integer                     :: id_stc, id_smc, id_slc

 real(kind=4)                :: times
 real(kind=4), allocatable   :: lsoil_data(:), x_data(:), y_data(:)
 real(kind=8), allocatable   :: dum2d(:,:), dum3d(:,:,:)

 include "netcdf.inc"

 do_nsst = .true.

!--- open the file
 error = NF__CREATE(fnbgso, IOR(NF_NETCDF4,NF_CLASSIC_MODEL), inital, fsize, ncid)
 call netcdf_err(error, 'CREATING FILE='//trim(fnbgso) )

 print*,'top of write ',idim,jdim,lsoil
 print*,'stcfcs ',maxval(stcfcs),minval(stcfcs)

!--- define dimensions
 error = nf_def_dim(ncid, 'xaxis_1', idim, dim_x)
 call netcdf_err(error, 'DEFINING XAXIS DIMENSION' )
 error = nf_def_dim(ncid, 'yaxis_1', jdim, dim_y)
 call netcdf_err(error, 'DEFINING YAXIS DIMENSION' )
 error = nf_def_dim(ncid, 'zaxis_1', lsoil, dim_lsoil)
 call netcdf_err(error, 'DEFINING ZAXIS DIMENSION' )
 error = nf_def_dim(ncid, 'Time', 1, dim_time)
 call netcdf_err(error, 'DEFINING TIME DIMENSION' )

 !--- define fields
 error = nf_def_var(ncid, 'xaxis_1', NF_FLOAT, 1, dim_x, id_x)
 call netcdf_err(error, 'DEFINING XAXIS_1 FIELD' )
 error = nf_put_att_text(ncid, id_x, "long_name", 7, "xaxis_1")
 call netcdf_err(error, 'DEFINING XAXIS_1 LONG NAME' )
 error = nf_put_att_text(ncid, id_x, "units", 4, "none")
 call netcdf_err(error, 'DEFINING XAXIS_1 UNITS' )
 error = nf_put_att_text(ncid, id_x, "cartesian_axis", 1, "X")
 call netcdf_err(error, 'WRITING XAXIS_1 FIELD' )

 error = nf_def_var(ncid, 'yaxis_1', NF_FLOAT, 1, dim_y, id_y)
 call netcdf_err(error, 'DEFINING YAXIS_1 FIELD' )
 error = nf_put_att_text(ncid, id_y, "long_name", 7, "yaxis_1")
 call netcdf_err(error, 'DEFINING YAXIS_1 LONG NAME' )
 error = nf_put_att_text(ncid, id_y, "units", 4, "none")
 call netcdf_err(error, 'DEFINING YAXIS_1 UNITS' )
 error = nf_put_att_text(ncid, id_y, "cartesian_axis", 1, "Y")
 call netcdf_err(error, 'WRITING YAXIS_1 FIELD' )

 error = nf_def_var(ncid, 'zaxis_1', NF_FLOAT, 1, dim_lsoil, id_lsoil)
 call netcdf_err(error, 'DEFINING ZAXIS_1 FIELD' )
 error = nf_put_att_text(ncid, id_lsoil, "long_name", 7, "zaxis_1")
 call netcdf_err(error, 'DEFINING ZAXIS_1 LONG NAME' )
 error = nf_put_att_text(ncid, id_lsoil, "units", 4, "none")
 call netcdf_err(error, 'DEFINING ZAXIS_1 UNITS' )
 error = nf_put_att_text(ncid, id_lsoil, "cartesian_axis", 1, "Z")
 call netcdf_err(error, 'WRITING ZAXIS_1 FIELD' )

 error = nf_def_var(ncid, 'Time', NF_FLOAT, 1, dim_time, id_time)
 call netcdf_err(error, 'DEFINING TIME FIELD' )
 error = nf_put_att_text(ncid, id_time, "long_name", 4, "Time")
 call netcdf_err(error, 'DEFINING TIME LONG NAME' )
 error = nf_put_att_text(ncid, id_time, "units", 10, "time level")
 call netcdf_err(error, 'DEFINING TIME UNITS' )
 error = nf_put_att_text(ncid, id_time, "cartesian_axis", 1, "T")
 call netcdf_err(error, 'WRITING TIME FIELD' )

 dims_3d(1) = dim_x
 dims_3d(2) = dim_y
 dims_3d(3) = dim_time

 error = nf_def_var(ncid, 'slmsk', NF_DOUBLE, 3, dims_3d, id_slmsk)
 call netcdf_err(error, 'DEFINING SLMSK' )
 error = nf_put_att_text(ncid, id_slmsk, "long_name", 5, "slmsk")
 call netcdf_err(error, 'DEFINING SLMSK LONG NAME' )
 error = nf_put_att_text(ncid, id_slmsk, "units", 4, "none")
 call netcdf_err(error, 'DEFINING SLMSK UNITS' )

 error = nf_def_var(ncid, 'tsea', NF_DOUBLE, 3, dims_3d, id_tsea)
 call netcdf_err(error, 'DEFINING TSEA' )
 error = nf_put_att_text(ncid, id_tsea, "long_name", 4, "tsea")
 call netcdf_err(error, 'DEFINING TSEA LONG NAME' )
 error = nf_put_att_text(ncid, id_tsea, "units", 4, "none")
 call netcdf_err(error, 'DEFINING TSEA UNITS' )

 error = nf_def_var(ncid, 'sheleg', NF_DOUBLE, 3, dims_3d, id_sheleg)
 call netcdf_err(error, 'DEFINING SHELEG' )
 error = nf_put_att_text(ncid, id_sheleg, "long_name", 6, "sheleg")
 call netcdf_err(error, 'DEFINING SHELEG LONG NAME' )
 error = nf_put_att_text(ncid, id_sheleg, "units", 4, "none")
 call netcdf_err(error, 'DEFINING SHELEG UNITS' )

 error = nf_def_var(ncid, 'tg3', NF_DOUBLE, 3, dims_3d, id_tg3)
 call netcdf_err(error, 'DEFINING TG3' )
 error = nf_put_att_text(ncid, id_tg3, "long_name", 3, "tg3")
 call netcdf_err(error, 'DEFINING TG3 LONG NAME' )
 error = nf_put_att_text(ncid, id_tg3, "units", 4, "none")
 call netcdf_err(error, 'DEFINING TG3 UNITS' )

 error = nf_def_var(ncid, 'zorl', NF_DOUBLE, 3, dims_3d, id_zorl)
 call netcdf_err(error, 'DEFINING ZORL' )
 error = nf_put_att_text(ncid, id_zorl, "long_name", 4, "zorl")
 call netcdf_err(error, 'DEFINING ZORL LONG NAME' )
 error = nf_put_att_text(ncid, id_zorl, "units", 4, "none")
 call netcdf_err(error, 'DEFINING ZORL UNITS' )

 error = nf_def_var(ncid, 'alvsf', NF_DOUBLE, 3, dims_3d, id_alvsf)
 call netcdf_err(error, 'DEFINING ALVSF' )
 error = nf_put_att_text(ncid, id_alvsf, "long_name", 5, "alvsf")
 call netcdf_err(error, 'DEFINING ALVSF LONG NAME' )
 error = nf_put_att_text(ncid, id_alvsf, "units", 4, "none")
 call netcdf_err(error, 'DEFINING ALVSF UNITS' )

 error = nf_def_var(ncid, 'alvwf', NF_DOUBLE, 3, dims_3d, id_alvwf)
 call netcdf_err(error, 'DEFINING ALVWF' )
 error = nf_put_att_text(ncid, id_alvwf, "long_name", 5, "alvwf")
 call netcdf_err(error, 'DEFINING ALVWF LONG NAME' )
 error = nf_put_att_text(ncid, id_alvwf, "units", 4, "none")
 call netcdf_err(error, 'DEFINING ALVWF UNITS' )

 error = nf_def_var(ncid, 'alnsf', NF_DOUBLE, 3, dims_3d, id_alnsf)
 call netcdf_err(error, 'DEFINING ALNSF' )
 error = nf_put_att_text(ncid, id_alnsf, "long_name", 5, "alnsf")
 call netcdf_err(error, 'DEFINING ALNSF LONG NAME' )
 error = nf_put_att_text(ncid, id_alnsf, "units", 4, "none")
 call netcdf_err(error, 'DEFINING ALNSF UNITS' )

 error = nf_def_var(ncid, 'alnwf', NF_DOUBLE, 3, dims_3d, id_alnwf)
 call netcdf_err(error, 'DEFINING ALNWF' )
 error = nf_put_att_text(ncid, id_alnwf, "long_name", 5, "alnwf")
 call netcdf_err(error, 'DEFINING ALNWF LONG NAME' )
 error = nf_put_att_text(ncid, id_alnwf, "units", 4, "none")
 call netcdf_err(error, 'DEFINING ALNWF UNITS' )

 error = nf_def_var(ncid, 'facsf', NF_DOUBLE, 3, dims_3d, id_facsf)
 call netcdf_err(error, 'DEFINING FACSF' )
 error = nf_put_att_text(ncid, id_facsf, "long_name", 5, "facsf")
 call netcdf_err(error, 'DEFINING FACSF LONG NAME' )
 error = nf_put_att_text(ncid, id_facsf, "units", 4, "none")
 call netcdf_err(error, 'DEFINING FACSF UNITS' )

 error = nf_def_var(ncid, 'facwf', NF_DOUBLE, 3, dims_3d, id_facwf)
 call netcdf_err(error, 'DEFINING FACWF' )
 error = nf_put_att_text(ncid, id_facwf, "long_name", 5, "facwf")
 call netcdf_err(error, 'DEFINING FACWF LONG NAME' )
 error = nf_put_att_text(ncid, id_facwf, "units", 4, "none")
 call netcdf_err(error, 'DEFINING FACWF UNITS' )

 error = nf_def_var(ncid, 'vfrac', NF_DOUBLE, 3, dims_3d, id_vfrac)
 call netcdf_err(error, 'DEFINING VFRAC' )
 error = nf_put_att_text(ncid, id_vfrac, "long_name", 5, "vfrac")
 call netcdf_err(error, 'DEFINING FACWF LONG NAME' )
 error = nf_put_att_text(ncid, id_vfrac, "units", 4, "none")
 call netcdf_err(error, 'DEFINING VFRAC UNITS' )

 error = nf_def_var(ncid, 'canopy', NF_DOUBLE, 3, dims_3d, id_canopy)
 call netcdf_err(error, 'DEFINING CANOPY' )
 error = nf_put_att_text(ncid, id_canopy, "long_name", 6, "canopy")
 call netcdf_err(error, 'DEFINING CANOPY LONG NAME' )
 error = nf_put_att_text(ncid, id_canopy, "units", 4, "none")
 call netcdf_err(error, 'DEFINING CANOPY UNITS' )

 error = nf_def_var(ncid, 'f10m', NF_DOUBLE, 3, dims_3d, id_f10m)
 call netcdf_err(error, 'DEFINING F10M' )
 error = nf_put_att_text(ncid, id_f10m, "long_name", 4, "f10m")
 call netcdf_err(error, 'DEFINING F10M LONG NAME' )
 error = nf_put_att_text(ncid, id_f10m, "units", 4, "none")
 call netcdf_err(error, 'DEFINING F10M UNITS' )

 error = nf_def_var(ncid, 't2m', NF_DOUBLE, 3, dims_3d, id_t2m)
 call netcdf_err(error, 'DEFINING T2M' )
 error = nf_put_att_text(ncid, id_t2m, "long_name", 3, "t2m")
 call netcdf_err(error, 'DEFINING T2M LONG NAME' )
 error = nf_put_att_text(ncid, id_t2m, "units", 4, "none")
 call netcdf_err(error, 'DEFINING T2M UNITS' )

 error = nf_def_var(ncid, 'q2m', NF_DOUBLE, 3, dims_3d, id_q2m)
 call netcdf_err(error, 'DEFINING Q2M' )
 error = nf_put_att_text(ncid, id_q2m, "long_name", 3, "q2m")
 call netcdf_err(error, 'DEFINING Q2M LONG NAME' )
 error = nf_put_att_text(ncid, id_q2m, "units", 4, "none")
 call netcdf_err(error, 'DEFINING Q2M UNITS' )

 error = nf_def_var(ncid, 'vtype', NF_DOUBLE, 3, dims_3d, id_vtype)
 call netcdf_err(error, 'DEFINING VTYPE' )
 error = nf_put_att_text(ncid, id_vtype, "long_name", 5, "vtype")
 call netcdf_err(error, 'DEFINING VTYPE LONG NAME' )
 error = nf_put_att_text(ncid, id_vtype, "units", 4, "none")
 call netcdf_err(error, 'DEFINING VTYPE UNITS' )

 error = nf_def_var(ncid, 'stype', NF_DOUBLE, 3, dims_3d, id_stype)
 call netcdf_err(error, 'DEFINING STYPE' )
 error = nf_put_att_text(ncid, id_stype, "long_name", 5, "stype")
 call netcdf_err(error, 'DEFINING STYPE LONG NAME' )
 error = nf_put_att_text(ncid, id_stype, "units", 4, "none")
 call netcdf_err(error, 'DEFINING STYPE UNITS' )

 error = nf_def_var(ncid, 'uustar', NF_DOUBLE, 3, dims_3d, id_uustar)
 call netcdf_err(error, 'DEFINING UUSTAR' )
 error = nf_put_att_text(ncid, id_uustar, "long_name", 6, "uustar")
 call netcdf_err(error, 'DEFINING UUSTAR LONG NAME' )
 error = nf_put_att_text(ncid, id_uustar, "units", 4, "none")
 call netcdf_err(error, 'DEFINING UUSTAR UNITS' )

 error = nf_def_var(ncid, 'ffmm', NF_DOUBLE, 3, dims_3d, id_ffmm)
 call netcdf_err(error, 'DEFINING FFMM' )
 error = nf_put_att_text(ncid, id_ffmm, "long_name", 4, "ffmm")
 call netcdf_err(error, 'DEFINING FFMM LONG NAME' )
 error = nf_put_att_text(ncid, id_ffmm, "units", 4, "none")
 call netcdf_err(error, 'DEFINING FFMM UNITS' )

 error = nf_def_var(ncid, 'ffhh', NF_DOUBLE, 3, dims_3d, id_ffhh)
 call netcdf_err(error, 'DEFINING FFHH' )
 error = nf_put_att_text(ncid, id_ffhh, "long_name", 4, "ffhh")
 call netcdf_err(error, 'DEFINING FFHH LONG NAME' )
 error = nf_put_att_text(ncid, id_ffhh, "units", 4, "none")
 call netcdf_err(error, 'DEFINING FFHH UNITS' )

 error = nf_def_var(ncid, 'hice', NF_DOUBLE, 3, dims_3d, id_hice)
 call netcdf_err(error, 'DEFINING HICE' )
 error = nf_put_att_text(ncid, id_hice, "long_name", 4, "hice")
 call netcdf_err(error, 'DEFINING HICE LONG NAME' )
 error = nf_put_att_text(ncid, id_hice, "units", 4, "none")
 call netcdf_err(error, 'DEFINING HICE UNITS' )

 error = nf_def_var(ncid, 'fice', NF_DOUBLE, 3, dims_3d, id_fice)
 call netcdf_err(error, 'DEFINING FICE' )
 error = nf_put_att_text(ncid, id_fice, "long_name", 4, "fice")
 call netcdf_err(error, 'DEFINING FICE LONG NAME' )
 error = nf_put_att_text(ncid, id_fice, "units", 4, "none")
 call netcdf_err(error, 'DEFINING FICE UNITS' )

 error = nf_def_var(ncid, 'tisfc', NF_DOUBLE, 3, dims_3d, id_tisfc)
 call netcdf_err(error, 'DEFINING TISFC' )
 error = nf_put_att_text(ncid, id_tisfc, "long_name", 5, "tisfc")
 call netcdf_err(error, 'DEFINING TISFC LONG NAME' )
 error = nf_put_att_text(ncid, id_tisfc, "units", 4, "none")
 call netcdf_err(error, 'DEFINING TISFC UNITS' )

 error = nf_def_var(ncid, 'tprcp', NF_DOUBLE, 3, dims_3d, id_tprcp)
 call netcdf_err(error, 'DEFINING TPRCP' )
 error = nf_put_att_text(ncid, id_tprcp, "long_name", 5, "tprcp")
 call netcdf_err(error, 'DEFINING TPRCP LONG NAME' )
 error = nf_put_att_text(ncid, id_tprcp, "units", 4, "none")
 call netcdf_err(error, 'DEFINING TPRCP UNITS' )

 error = nf_def_var(ncid, 'srflag', NF_DOUBLE, 3, dims_3d, id_srflag)
 call netcdf_err(error, 'DEFINING SRFLAG' )
 error = nf_put_att_text(ncid, id_srflag, "long_name", 6, "srflag")
 call netcdf_err(error, 'DEFINING SRFLAG LONG NAME' )
 error = nf_put_att_text(ncid, id_srflag, "units", 4, "none")
 call netcdf_err(error, 'DEFINING SRFLAG UNITS' )

 error = nf_def_var(ncid, 'snwdph', NF_DOUBLE, 3, dims_3d, id_snwdph)
 call netcdf_err(error, 'DEFINING SNWDPH' )
 error = nf_put_att_text(ncid, id_snwdph, "long_name", 6, "snwdph")
 call netcdf_err(error, 'DEFINING SNWDPH LONG NAME' )
 error = nf_put_att_text(ncid, id_snwdph, "units", 4, "none")
 call netcdf_err(error, 'DEFINING SNWDPH UNITS' )

 error = nf_def_var(ncid, 'shdmin', NF_DOUBLE, 3, dims_3d, id_shdmin)
 call netcdf_err(error, 'DEFINING SHDMIN' )
 error = nf_put_att_text(ncid, id_shdmin, "long_name", 6, "shdmin")
 call netcdf_err(error, 'DEFINING SHDMIN LONG NAME' )
 error = nf_put_att_text(ncid, id_shdmin, "units", 4, "none")
 call netcdf_err(error, 'DEFINING SHDMIN UNITS' )

 error = nf_def_var(ncid, 'shdmax', NF_DOUBLE, 3, dims_3d, id_shdmax)
 call netcdf_err(error, 'DEFINING SHDMAX' )
 error = nf_put_att_text(ncid, id_shdmax, "long_name", 6, "shdmax")
 call netcdf_err(error, 'DEFINING SHDMAX LONG NAME' )
 error = nf_put_att_text(ncid, id_shdmax, "units", 4, "none")
 call netcdf_err(error, 'DEFINING SHDMAX UNITS' )

 error = nf_def_var(ncid, 'slope', NF_DOUBLE, 3, dims_3d, id_slope)
 call netcdf_err(error, 'DEFINING SLOPE' )
 error = nf_put_att_text(ncid, id_slope, "long_name", 5, "slope")
 call netcdf_err(error, 'DEFINING SLOPE LONG NAME' )
 error = nf_put_att_text(ncid, id_slope, "units", 4, "none")
 call netcdf_err(error, 'DEFINING SLOPE UNITS' )

 error = nf_def_var(ncid, 'snoalb', NF_DOUBLE, 3, dims_3d, id_snoalb)
 call netcdf_err(error, 'DEFINING SNOALB' )
 error = nf_put_att_text(ncid, id_snoalb, "long_name", 6, "snoalb")
 call netcdf_err(error, 'DEFINING SNOALB LONG NAME' )
 error = nf_put_att_text(ncid, id_snoalb, "units", 4, "none")
 call netcdf_err(error, 'DEFINING SNOALB UNITS' )

 NSST_HEADER : if (do_nsst) then

   error = nf_def_var(ncid, 'tref', NF_DOUBLE, 3, dims_3d, id_tref)
   call netcdf_err(error, 'DEFINING TREF' )
   error = nf_put_att_text(ncid, id_tref, "long_name", 4, "tref")
   call netcdf_err(error, 'DEFINING TREF LONG NAME' )
   error = nf_put_att_text(ncid, id_tref, "units", 4, "none")
   call netcdf_err(error, 'DEFINING TREF UNITS' )

   error = nf_def_var(ncid, 'z_c', NF_DOUBLE, 3, dims_3d, id_z_c)
   call netcdf_err(error, 'DEFINING Z_C' )
   error = nf_put_att_text(ncid, id_z_c, "long_name", 3, "z_c")
   call netcdf_err(error, 'DEFINING Z_C LONG NAME' )
   error = nf_put_att_text(ncid, id_z_c, "units", 4, "none")
   call netcdf_err(error, 'DEFINING Z_C UNITS' )

   error = nf_def_var(ncid, 'c_0', NF_DOUBLE, 3, dims_3d, id_c_0)
   call netcdf_err(error, 'DEFINING C_0' )
   error = nf_put_att_text(ncid, id_c_0, "long_name", 3, "c_0")
   call netcdf_err(error, 'DEFINING C_0 LONG NAME' )
   error = nf_put_att_text(ncid, id_c_0, "units", 4, "none")
   call netcdf_err(error, 'DEFINING C_0 UNITS' )

   error = nf_def_var(ncid, 'c_d', NF_DOUBLE, 3, dims_3d, id_c_d)
   call netcdf_err(error, 'DEFINING C_D' )
   error = nf_put_att_text(ncid, id_c_d, "long_name", 3, "c_d")
   call netcdf_err(error, 'DEFINING C_D LONG NAME' )
   error = nf_put_att_text(ncid, id_c_d, "units", 4, "none")
   call netcdf_err(error, 'DEFINING C_D UNITS' )

   error = nf_def_var(ncid, 'w_0', NF_DOUBLE, 3, dims_3d, id_w_0)
   call netcdf_err(error, 'DEFINING W_0' )
   error = nf_put_att_text(ncid, id_w_0, "long_name", 3, "w_0")
   call netcdf_err(error, 'DEFINING W_0 LONG NAME' )
   error = nf_put_att_text(ncid, id_w_0, "units", 4, "none")
   call netcdf_err(error, 'DEFINING W_0 UNITS' )

   error = nf_def_var(ncid, 'w_d', NF_DOUBLE, 3, dims_3d, id_w_d)
   call netcdf_err(error, 'DEFINING W_D' )
   error = nf_put_att_text(ncid, id_w_d, "long_name", 3, "w_d")
   call netcdf_err(error, 'DEFINING W_D LONG NAME' )
   error = nf_put_att_text(ncid, id_w_d, "units", 4, "none")
   call netcdf_err(error, 'DEFINING W_D UNITS' )

   error = nf_def_var(ncid, 'xt', NF_DOUBLE, 3, dims_3d, id_xt)
   call netcdf_err(error, 'DEFINING XT' )
   error = nf_put_att_text(ncid, id_xt, "long_name", 2, "xt")
   call netcdf_err(error, 'DEFINING XT LONG NAME' )
   error = nf_put_att_text(ncid, id_xt, "units", 4, "none")
   call netcdf_err(error, 'DEFINING XT UNITS' )

   error = nf_def_var(ncid, 'xs', NF_DOUBLE, 3, dims_3d, id_xs)
   call netcdf_err(error, 'DEFINING XS' )
   error = nf_put_att_text(ncid, id_xs, "long_name", 2, "xs")
   call netcdf_err(error, 'DEFINING XS LONG NAME' )
   error = nf_put_att_text(ncid, id_xs, "units", 4, "none")
   call netcdf_err(error, 'DEFINING XS UNITS' )

   error = nf_def_var(ncid, 'xu', NF_DOUBLE, 3, dims_3d, id_xu)
   call netcdf_err(error, 'DEFINING XU' )
   error = nf_put_att_text(ncid, id_xu, "long_name", 2, "xu")
   call netcdf_err(error, 'DEFINING XU LONG NAME' )
   error = nf_put_att_text(ncid, id_xu, "units", 4, "none")
   call netcdf_err(error, 'DEFINING XU UNITS' )

   error = nf_def_var(ncid, 'xv', NF_DOUBLE, 3, dims_3d, id_xv)
   call netcdf_err(error, 'DEFINING XV' )
   error = nf_put_att_text(ncid, id_xv, "long_name", 2, "xv")
   call netcdf_err(error, 'DEFINING XV LONG NAME' )
   error = nf_put_att_text(ncid, id_xv, "units", 4, "none")
   call netcdf_err(error, 'DEFINING XV UNITS' )

   error = nf_def_var(ncid, 'xz', NF_DOUBLE, 3, dims_3d, id_xz)
   call netcdf_err(error, 'DEFINING XZ' )
   error = nf_put_att_text(ncid, id_xz, "long_name", 2, "xz")
   call netcdf_err(error, 'DEFINING XZ LONG NAME' )
   error = nf_put_att_text(ncid, id_xz, "units", 4, "none")
   call netcdf_err(error, 'DEFINING XZ UNITS' )

   error = nf_def_var(ncid, 'zm', NF_DOUBLE, 3, dims_3d, id_zm)
   call netcdf_err(error, 'DEFINING ZM' )
   error = nf_put_att_text(ncid, id_zm, "long_name", 2, "zm")
   call netcdf_err(error, 'DEFINING ZM LONG NAME' )
   error = nf_put_att_text(ncid, id_zm, "units", 4, "none")
   call netcdf_err(error, 'DEFINING ZM UNITS' )

   error = nf_def_var(ncid, 'xtts', NF_DOUBLE, 3, dims_3d, id_xtts)
   call netcdf_err(error, 'DEFINING XTTS' )
   error = nf_put_att_text(ncid, id_xtts, "long_name", 4, "xtts")
   call netcdf_err(error, 'DEFINING XTTS LONG NAME' )
   error = nf_put_att_text(ncid, id_xtts, "units", 4, "none")
   call netcdf_err(error, 'DEFINING XTTS UNITS' )

   error = nf_def_var(ncid, 'xzts', NF_DOUBLE, 3, dims_3d, id_xzts)
   call netcdf_err(error, 'DEFINING XZTS' )
   error = nf_put_att_text(ncid, id_xzts, "long_name", 4, "xzts")
   call netcdf_err(error, 'DEFINING XZTS LONG NAME' )
   error = nf_put_att_text(ncid, id_xzts, "units", 4, "none")
   call netcdf_err(error, 'DEFINING XZTS UNITS' )

   error = nf_def_var(ncid, 'd_conv', NF_DOUBLE, 3, dims_3d, id_d_conv)
   call netcdf_err(error, 'DEFINING D_CONV' )
   error = nf_put_att_text(ncid, id_d_conv, "long_name", 6, "d_conv")
   call netcdf_err(error, 'DEFINING D_CONV LONG NAME' )
   error = nf_put_att_text(ncid, id_d_conv, "units", 4, "none")
   call netcdf_err(error, 'DEFINING D_CONV UNITS' )

   error = nf_def_var(ncid, 'ifd', NF_DOUBLE, 3, dims_3d, id_ifd)
   call netcdf_err(error, 'DEFINING IFD' )
   error = nf_put_att_text(ncid, id_ifd, "long_name", 3, "ifd")
   call netcdf_err(error, 'DEFINING IFD LONG NAME' )
   error = nf_put_att_text(ncid, id_ifd, "units", 4, "none")
   call netcdf_err(error, 'DEFINING IFD UNITS' )

   error = nf_def_var(ncid, 'dt_cool', NF_DOUBLE, 3, dims_3d, id_dt_cool)
   call netcdf_err(error, 'DEFINING DT_COOL' )
   error = nf_put_att_text(ncid, id_dt_cool, "long_name", 7, "dt_cool")
   call netcdf_err(error, 'DEFINING DT_COOL LONG NAME' )
   error = nf_put_att_text(ncid, id_dt_cool, "units", 4, "none")
   call netcdf_err(error, 'DEFINING DT_COOL UNITS' )

   error = nf_def_var(ncid, 'qrain', NF_DOUBLE, 3, dims_3d, id_qrain)
   call netcdf_err(error, 'DEFINING QRAIN' )
   error = nf_put_att_text(ncid, id_qrain, "long_name", 5, "qrain")
   call netcdf_err(error, 'DEFINING QRAIN LONG NAME' )
   error = nf_put_att_text(ncid, id_qrain, "units", 4, "none")
   call netcdf_err(error, 'DEFINING QRAIN UNITS' )

 endif NSST_HEADER

 dims_4d(1) = dim_x
 dims_4d(2) = dim_y
 dims_4d(3) = dim_lsoil
 dims_4d(4) = dim_time

 error = nf_def_var(ncid, 'stc', NF_DOUBLE, 4, dims_4d, id_stc)
 call netcdf_err(error, 'DEFINING STC' )
 error = nf_put_att_text(ncid, id_stc, "long_name", 3, "stc")
 call netcdf_err(error, 'DEFINING STC LONG NAME' )
 error = nf_put_att_text(ncid, id_stc, "units", 4, "none")
 call netcdf_err(error, 'DEFINING STC UNITS' )

 error = nf_def_var(ncid, 'smc', NF_DOUBLE, 4, dims_4d, id_smc)
 call netcdf_err(error, 'DEFINING SMC' )
 error = nf_put_att_text(ncid, id_smc, "long_name", 3, "smc")
 call netcdf_err(error, 'DEFINING SMC LONG NAME' )
 error = nf_put_att_text(ncid, id_smc, "units", 4, "none")
 call netcdf_err(error, 'DEFINING SMC UNITS' )

 error = nf_def_var(ncid, 'slc', NF_DOUBLE, 4, dims_4d, id_slc)
 call netcdf_err(error, 'DEFINING SLC' )
 error = nf_put_att_text(ncid, id_slc, "long_name", 3, "slc")
 call netcdf_err(error, 'DEFINING SLC LONG NAME' )
 error = nf_put_att_text(ncid, id_slc, "units", 4, "none")
 call netcdf_err(error, 'DEFINING SLC UNITS' )

 error = nf__enddef(ncid, header_buffer_val,4,0,4)
 call netcdf_err(error, 'DEFINING HEADER' )

!---------------------------------------------------------------------------------
! Write data
!---------------------------------------------------------------------------------

     allocate(lsoil_data(lsoil))
     do i = 1, lsoil
       lsoil_data(i) = float(i)
     enddo

     allocate(x_data(idim))
     do i = 1, idim
       x_data(i) = float(i)
     enddo

     allocate(y_data(jdim))
     do i = 1, jdim
       y_data(i) = float(i)
     enddo

     error = nf_put_var_real( ncid, id_lsoil, lsoil_data)
     call netcdf_err(error, 'WRITING ZAXIS RECORD' )
     error = nf_put_var_real( ncid, id_x, x_data)
     call netcdf_err(error, 'WRITING XAXIS RECORD' )
     error = nf_put_var_real( ncid, id_y, y_data)
     call netcdf_err(error, 'WRITING YAXIS RECORD' )
     times = 1.0
     error = nf_put_var_real( ncid, id_time, times)
     call netcdf_err(error, 'WRITING TIME RECORD' )

     deallocate(lsoil_data, x_data, y_data)

     dims_strt(1:3) = 1
     dims_end(1) = idim
     dims_end(2) = jdim
     dims_end(3) = 1

     allocate(dum2d(idim,jdim))

     dum2d = reshape(slifcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_slmsk, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING LANDMASK RECORD' )

     dum2d = reshape(tsffcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_tsea, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING TSEA RECORD' )

     dum2d = reshape(snofcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_sheleg, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING SHELEG RECORD' )

     dum2d = reshape(tg3fcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_tg3, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING TG3 RECORD' )

     dum2d = reshape(zorfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_zorl, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING ZORL RECORD' )

     dum2d = reshape(albfcs(:,1), (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_alvsf, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING ALVSF RECORD' )

     dum2d = reshape(albfcs(:,2), (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_alvwf, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING ALVWF RECORD' )

     dum2d = reshape(albfcs(:,3), (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_alnsf, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING ALNSF RECORD' )

     dum2d = reshape(albfcs(:,4), (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_alnwf, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING ALNWF RECORD' )

     dum2d = reshape(alffcs(:,1), (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_facsf, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING FACSF RECORD' )

     dum2d = reshape(alffcs(:,2), (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_facwf, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING FACWF RECORD' )

     dum2d = reshape(vegfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_vfrac, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING VFRAC RECORD' )

     dum2d = reshape(cnpfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_canopy, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING CANOPY RECORD' )

     dum2d = reshape(f10m, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_f10m, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING F10M RECORD' )

     dum2d = reshape(t2m, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_t2m, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING T2M RECORD' )

     dum2d = reshape(q2m, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_q2m, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING Q2M RECORD' )

     dum2d = reshape(vetfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_vtype, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING VTYPE RECORD' )

     dum2d = reshape(sotfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_stype, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING STYPE RECORD' )

     dum2d = reshape(ustar, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_uustar, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING UUSTAR RECORD' )

     dum2d = reshape(fmm, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_ffmm, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING FFMM RECORD' )

     dum2d = reshape(fhh, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_ffhh, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING FFHH RECORD' )

     dum2d = reshape(sihfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_hice, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING HICE RECORD' )

     dum2d = reshape(sicfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_fice, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING FICE RECORD' )

     dum2d = reshape(sitfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_tisfc, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING TISFC RECORD' )

     dum2d = reshape(tprcp, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_tprcp, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING TPRCP RECORD' )

     dum2d = reshape(srflag, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_srflag, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING SRFLAG RECORD' )

     dum2d = reshape(swdfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_snwdph, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING SNWDPH RECORD' )

     dum2d = reshape(vmnfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_shdmin, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING SHDMIN RECORD' )

     dum2d = reshape(vmxfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_shdmax, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING SHDMAX RECORD' )

     dum2d = reshape(slpfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_slope, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING SLOPE RECORD' )

     dum2d = reshape(absfcs, (/idim,jdim/))
     error = nf_put_vara_double( ncid, id_snoalb, dims_strt, dims_end, dum2d)
     call netcdf_err(error, 'WRITING SNOALB RECORD' )

     deallocate(dum2d)

     dims4_strt(1:4) = 1
     dims4_end(1) = idim
     dims4_end(2) = jdim
     dims4_end(3) = lsoil
     dims4_end(4) = 1

     allocate(dum3d(idim,jdim,lsoil))

     dum3d = reshape(slcfcs, (/idim,jdim,lsoil/))
     error = nf_put_vara_double( ncid, id_slc, dims4_strt, dims4_end, dum3d)
     call netcdf_err(error, 'WRITING SLC RECORD' )

     dum3d = reshape(smcfcs, (/idim,jdim,lsoil/))
     error = nf_put_vara_double( ncid, id_smc, dims4_strt, dims4_end, dum3d)
     call netcdf_err(error, 'WRITING SMC RECORD' )

     dum3d = reshape(stcfcs, (/idim,jdim,lsoil/))
     error = nf_put_vara_double( ncid, id_stc, dims4_strt, dims4_end, dum3d)
     call netcdf_err(error, 'WRITING STC RECORD' )

     deallocate(dum3d)

 error = nf_close(ncid)

 end subroutine write_data

 SUBROUTINE READ_LAT_LON_OROG(RLA,RLO,OROG,OROG_UF,FNOROG,FNGRID,&
                              IDIM,JDIM,IJDIM)

 use netcdf

 IMPLICIT NONE

 include "netcdf.inc"

 CHARACTER(LEN=*), INTENT(IN) :: FNOROG, FNGRID

 INTEGER, INTENT(IN)    :: IDIM, JDIM, IJDIM

 REAL, INTENT(OUT)      :: RLA(IJDIM),RLO(IJDIM)
 REAL, INTENT(OUT)      :: OROG(IJDIM),OROG_UF(IJDIM)

 INTEGER                :: ERROR, NCID, NCID_OROG
 INTEGER                :: I, II, J, JJ
 INTEGER                :: ID_DIM, ID_VAR, NX, NY

 REAL, ALLOCATABLE      :: DUMMY(:,:), GEOLAT(:,:), GEOLON(:,:)
 REAL(KIND=4), ALLOCATABLE :: DUMMY4(:,:)

 PRINT*, "READ FV3 GRID INFO FROM: "//TRIM(FNGRID)

 ERROR=NF90_OPEN(TRIM(FNGRID),NF_NOWRITE,NCID)
 CALL NETCDF_ERR(ERROR, 'OPENING FILE: '//TRIM(FNGRID) )

 ERROR=NF90_INQ_DIMID(NCID, 'nx', ID_DIM)
 CALL NETCDF_ERR(ERROR, 'ERROR READING NX ID' )

 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=NX)
 CALL NETCDF_ERR(ERROR, 'ERROR READING NX' )

 ERROR=NF90_INQ_DIMID(NCID, 'ny', ID_DIM)
 CALL NETCDF_ERR(ERROR, 'ERROR READING NY ID' )

 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=NY)
 CALL NETCDF_ERR(ERROR, 'ERROR READING NY' )

 IF ((NX/2) /= IDIM .OR. (NY/2) /= JDIM) THEN
   PRINT*,'FATAL ERROR: DIMENSIONS IN FILE: ',(NX/2),(NY/2)
   PRINT*,'DO NOT MATCH GRID DIMENSIONS: ',IDIM,JDIM
   CALL ERREXIT(130)
 ENDIF

 ALLOCATE(GEOLON(NX+1,NY+1))
 ALLOCATE(GEOLAT(NX+1,NY+1))

 ERROR=NF90_INQ_VARID(NCID, 'x', ID_VAR)
 CALL NETCDF_ERR(ERROR, 'ERROR READING X ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, GEOLON)
 CALL NETCDF_ERR(ERROR, 'ERROR READING X RECORD' )

 ERROR=NF90_INQ_VARID(NCID, 'y', ID_VAR)
 CALL NETCDF_ERR(ERROR, 'ERROR READING Y ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, GEOLAT)
 CALL NETCDF_ERR(ERROR, 'ERROR READING Y RECORD' )

 ERROR = NF_CLOSE(NCID)

 ALLOCATE(DUMMY(IDIM,JDIM))

 DO J = 1, JDIM
   DO I = 1, IDIM
     II = 2*I
     JJ = 2*J
     DUMMY(I,J) = GEOLON(II,JJ)
   ENDDO
 ENDDO

 RLO = RESHAPE(DUMMY, (/IJDIM/))

 DEALLOCATE(GEOLON)

 DO J = 1, JDIM
   DO I = 1, IDIM
     II = 2*I
     JJ = 2*J
     DUMMY(I,J) = GEOLAT(II,JJ)
   ENDDO
 ENDDO

 RLA = RESHAPE(DUMMY, (/IJDIM/))

 DEALLOCATE(GEOLAT, DUMMY)

 PRINT*, "READ FV3 OROG INFO FROM: "//TRIM(FNOROG)

 ERROR=NF90_OPEN(TRIM(FNOROG),NF_NOWRITE,NCID_OROG)
 CALL NETCDF_ERR(ERROR, 'OPENING FILE: '//TRIM(FNOROG) )

 ALLOCATE(DUMMY4(IDIM,JDIM))

 ERROR=NF90_INQ_VARID(NCID_OROG, 'orog_raw', ID_VAR)
 CALL NETCDF_ERR(ERROR, 'ERROR READING orog_raw ID' )
 ERROR=NF90_GET_VAR(NCID_OROG, ID_VAR, DUMMY4)
 CALL NETCDF_ERR(ERROR, 'ERROR READING orog_raw RECORD' )
 OROG_UF = RESHAPE(DUMMY4, (/IJDIM/))
 print*,'orog raw ',maxval(dummy4)

 ERROR=NF90_INQ_VARID(NCID_OROG, 'orog_filt', ID_VAR)
 CALL NETCDF_ERR(ERROR, 'ERROR READING orog_filt ID' )
 ERROR=NF90_GET_VAR(NCID_OROG, ID_VAR, DUMMY4)
 CALL NETCDF_ERR(ERROR, 'ERROR READING orog_filt RECORD' )
 OROG = RESHAPE(DUMMY4, (/IJDIM/))
 print*,'orog filter',maxval(dummy4)

 DEALLOCATE(DUMMY4)

 ERROR = NF_CLOSE(NCID_OROG)

 END SUBROUTINE READ_LAT_LON_OROG

 SUBROUTINE NETCDF_ERR( ERR, STRING )

 IMPLICIT NONE

 INTEGER, INTENT(IN) :: ERR
 CHARACTER(LEN=*), INTENT(IN) :: STRING
 CHARACTER(LEN=256) :: ERRMSG
 include "netcdf.inc"

 IF( ERR.EQ.NF_NOERR )RETURN
 ERRMSG = NF_STRERROR(ERR)
 PRINT*,''
 PRINT*,'FATAL ERROR: ', TRIM(STRING), ': ', TRIM(ERRMSG)
 print*,'STOP.'
 CALL ERREXIT(999)

 RETURN
 END SUBROUTINE NETCDF_ERR

 SUBROUTINE READ_SFC_NETCDF(TSFFCS,SMCFCS,SNOFCS,STCFCS, &
                            TG3FCS,ZORFCS, &
                            CVFCS,CVBFCS,CVTFCS,ALBFCS, &
                            SLIFCS,VEGFCS,CNPFCS,F10M, &
                            VETFCS,SOTFCS,ALFFCS, &
                            USTAR,FMM,FHH, &
                            SIHFCS,SICFCS,SITFCS, &
                            TPRCP,SRFLAG,SWDFCS,  &
                            VMNFCS,VMXFCS,SLCFCS, &
                            SLPFCS,ABSFCS,T2M,Q2M,SLMASK, &
                            ZSOIL,LSOIL,LENSFC,FNBGSI)

 use netcdf

 IMPLICIT NONE

 include "netcdf.inc"

 CHARACTER(LEN=*), INTENT(IN) :: FNBGSI

 INTEGER, INTENT(IN)       :: LSOIL, LENSFC

 REAL, INTENT(OUT)         :: CVFCS(LENSFC), CVBFCS(LENSFC)
 REAL, INTENT(OUT)         :: CVTFCS(LENSFC), ALBFCS(LENSFC,4)
 REAL, INTENT(OUT)         :: SLIFCS(LENSFC), CNPFCS(LENSFC)
 REAL, INTENT(OUT)         :: VEGFCS(LENSFC), F10M(LENSFC)
 REAL, INTENT(OUT)         :: VETFCS(LENSFC), SOTFCS(LENSFC)
 REAL, INTENT(OUT)         :: TSFFCS(LENSFC), SNOFCS(LENSFC)
 REAL, INTENT(OUT)         :: TG3FCS(LENSFC), ZORFCS(LENSFC)
 REAL, INTENT(OUT)         :: ALFFCS(LENSFC,2), USTAR(LENSFC)
 REAL, INTENT(OUT)         :: FMM(LENSFC), FHH(LENSFC)
 REAL, INTENT(OUT)         :: SIHFCS(LENSFC), SICFCS(LENSFC)
 REAL, INTENT(OUT)         :: SITFCS(LENSFC), TPRCP(LENSFC)
 REAL, INTENT(OUT)         :: SRFLAG(LENSFC), SWDFCS(LENSFC)
 REAL, INTENT(OUT)         :: VMNFCS(LENSFC), VMXFCS(LENSFC)
 REAL, INTENT(OUT)         :: SLPFCS(LENSFC), ABSFCS(LENSFC)
 REAL, INTENT(OUT)         :: T2M(LENSFC), Q2M(LENSFC), SLMASK(LENSFC)
 REAL, INTENT(OUT)         :: SLCFCS(LENSFC,LSOIL)
 REAL, INTENT(OUT)         :: SMCFCS(LENSFC,LSOIL)
 REAL, INTENT(OUT)         :: STCFCS(LENSFC,LSOIL)
 REAL(KIND=4), INTENT(OUT) :: ZSOIL(LSOIL)

 INTEGER                   :: ERROR, NCID
 INTEGER                   :: IDIM, JDIM, ID_DIM
 INTEGER                   :: ID_VAR

 REAL(KIND=8), ALLOCATABLE :: DUMMY(:,:), DUMMY3D(:,:,:)

 PRINT*, "READ INPUT SFC DATA FROM: "//TRIM(FNBGSI)

 ERROR=NF90_OPEN(TRIM(FNBGSI),NF_NOWRITE,NCID)
 CALL NETCDF_ERR(ERROR, 'OPENING FILE: '//TRIM(FNBGSI) )

 ERROR=NF90_INQ_DIMID(NCID, 'xaxis_1', ID_DIM)
 CALL NETCDF_ERR(ERROR, 'READING xaxis_1' )
 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=IDIM)
 CALL NETCDF_ERR(ERROR, 'READING xaxis_1' )

 ERROR=NF90_INQ_DIMID(NCID, 'yaxis_1', ID_DIM)
 CALL NETCDF_ERR(ERROR, 'READING yaxis_1' )
 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=JDIM)
 CALL NETCDF_ERR(ERROR, 'READING yaxis_1' )

 IF ((IDIM*JDIM) /= LENSFC) THEN
   PRINT*,'FATAL ERROR: DIMENSIONS WRONG.'
   CALL ERREXIT(88)
 ENDIF

 ALLOCATE(DUMMY(IDIM,JDIM))

 ERROR=NF90_INQ_VARID(NCID, "tsea", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING tsea ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING tsea' )
 TSFFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'tsffcs ',maxval(tsffcs),minval(tsffcs)

 ERROR=NF90_INQ_VARID(NCID, "sheleg", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING sheleg ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING sheleg' )
 SNOFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'snofcs ',maxval(snofcs),minval(snofcs)

 ERROR=NF90_INQ_VARID(NCID, "tg3", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING tg3 ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING tg3' )
 TG3FCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'tg3fcs ',maxval(tg3fcs),minval(tg3fcs)

 ERROR=NF90_INQ_VARID(NCID, "zorl", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING zorl ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING zorl' )
 ZORFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'zorfcs ',maxval(zorfcs),minval(zorfcs)

 ERROR=NF90_INQ_VARID(NCID, "alvsf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING alvsf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING alvsf' )
 ALBFCS(:,1) = RESHAPE(DUMMY, (/LENSFC/))
 print*,'alvsf ',maxval(albfcs(:,1)),minval(albfcs(:,1))

 ERROR=NF90_INQ_VARID(NCID, "alvwf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING alvwf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING alvwf' )
 ALBFCS(:,2) = RESHAPE(DUMMY, (/LENSFC/))
 print*,'alvwf ',maxval(albfcs(:,2)),minval(albfcs(:,2))

 ERROR=NF90_INQ_VARID(NCID, "alnsf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING alnsf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING alnsf' )
 ALBFCS(:,3) = RESHAPE(DUMMY, (/LENSFC/))
 print*,'alnsf ',maxval(albfcs(:,3)),minval(albfcs(:,3))

 ERROR=NF90_INQ_VARID(NCID, "alnwf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING alnwf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING alnwf' )
 ALBFCS(:,4) = RESHAPE(DUMMY, (/LENSFC/))
 print*,'alnwf ',maxval(albfcs(:,4)),minval(albfcs(:,4))
  
 ERROR=NF90_INQ_VARID(NCID, "slmsk", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING slmsk ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING slmsk' )
 SLIFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'slifcs ',maxval(slifcs),minval(slifcs)
 SLMASK = SLIFCS
 WHERE (SLMASK > 1.5) SLMASK=0.0  ! remove sea ice
 print*,'slmask ',maxval(slmask),minval(slmask)
  
 ERROR=NF90_INQ_VARID(NCID, "canopy", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING canopy ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING canopy' )
 CNPFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'canopy ',maxval(cnpfcs),minval(cnpfcs)
  
 ERROR=NF90_INQ_VARID(NCID, "vfrac", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING vfrac ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING vfrac' )
 VEGFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'vfrac ',maxval(vegfcs),minval(vegfcs)
  
 ERROR=NF90_INQ_VARID(NCID, "f10m", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING f10m ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING f10m' )
 F10M = RESHAPE(DUMMY, (/LENSFC/))
 print*,'f10m ',maxval(f10m),minval(f10m)
  
 ERROR=NF90_INQ_VARID(NCID, "vtype", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING vtype ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING vtype' )
 VETFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'vtype ',maxval(vetfcs),minval(vetfcs)
  
 ERROR=NF90_INQ_VARID(NCID, "stype", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING stype ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING stype' )
 SOTFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'stype ',maxval(sotfcs),minval(sotfcs)
  
 ERROR=NF90_INQ_VARID(NCID, "facsf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING facsf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING facsf' )
 ALFFCS(:,1) = RESHAPE(DUMMY, (/LENSFC/))
 print*,'facsf ',maxval(alffcs(:,1)),minval(alffcs(:,1))
  
 ERROR=NF90_INQ_VARID(NCID, "facwf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING facwf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING facwf' )
 ALFFCS(:,2) = RESHAPE(DUMMY, (/LENSFC/))
 print*,'facwf ',maxval(alffcs(:,2)),minval(alffcs(:,2))
  
 ERROR=NF90_INQ_VARID(NCID, "uustar", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING uustar ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING uustar' )
 USTAR = RESHAPE(DUMMY, (/LENSFC/))
 print*,'uustar ',maxval(ustar),minval(ustar)
  
 ERROR=NF90_INQ_VARID(NCID, "ffmm", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING ffmm ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING ffmm' )
 FMM = RESHAPE(DUMMY, (/LENSFC/))
 print*,'ffmm ',maxval(fmm),minval(fmm)
  
 ERROR=NF90_INQ_VARID(NCID, "ffhh", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING ffhh ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING ffhh' )
 FHH = RESHAPE(DUMMY, (/LENSFC/))
 print*,'ffhh ',maxval(fhh),minval(fhh)
  
 ERROR=NF90_INQ_VARID(NCID, "hice", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING hice ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING hice' )
 SIHFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'hice ',maxval(sihfcs),minval(sihfcs)
  
 ERROR=NF90_INQ_VARID(NCID, "fice", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING fice ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING fice' )
 SICFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'fice ',maxval(sicfcs),minval(sicfcs)
  
 ERROR=NF90_INQ_VARID(NCID, "tisfc", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING tisfc ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING tisfc' )
 SITFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'tisfc ',maxval(sitfcs),minval(sitfcs)
  
 ERROR=NF90_INQ_VARID(NCID, "tprcp", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING tprcp ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING tprcp' )
 TPRCP = RESHAPE(DUMMY, (/LENSFC/))
 print*,'tprcp ',maxval(tprcp),minval(tprcp)
  
 ERROR=NF90_INQ_VARID(NCID, "srflag", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING srflag ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING srflag' )
 SRFLAG = RESHAPE(DUMMY, (/LENSFC/))
 print*,'srflag ',maxval(srflag),minval(srflag)
  
 ERROR=NF90_INQ_VARID(NCID, "snwdph", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING snwdph ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING snwdph' )
 SWDFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'snwdph ',maxval(swdfcs),minval(swdfcs)
  
 ERROR=NF90_INQ_VARID(NCID, "shdmin", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING shdmin ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING shdmin' )
 VMNFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'shdmin ',maxval(vmnfcs),minval(vmnfcs)
  
 ERROR=NF90_INQ_VARID(NCID, "shdmax", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING shdmax ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING shdmax' )
 VMXFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'shdmax ',maxval(vmxfcs),minval(vmxfcs)
  
 ERROR=NF90_INQ_VARID(NCID, "slope", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING slope ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING slope' )
 SLPFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'slope ',maxval(slpfcs),minval(slpfcs)
  
 ERROR=NF90_INQ_VARID(NCID, "snoalb", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING snoalb ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING snoalb' )
 ABSFCS = RESHAPE(DUMMY, (/LENSFC/))
 print*,'snoalb ',maxval(absfcs),minval(absfcs)
  
 ERROR=NF90_INQ_VARID(NCID, "t2m", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING t2m ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING t2m' )
 T2M = RESHAPE(DUMMY, (/LENSFC/))
 print*,'t2m ',maxval(t2m),minval(t2m)
  
 ERROR=NF90_INQ_VARID(NCID, "q2m", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING q2m ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING q2m' )
 Q2M = RESHAPE(DUMMY, (/LENSFC/))
 print*,'q2m ',maxval(q2m),minval(q2m)
  
 DEALLOCATE(DUMMY)

 ALLOCATE(DUMMY3D(IDIM,JDIM,LSOIL))

 ERROR=NF90_INQ_VARID(NCID, "smc", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING smc ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy3d)
 CALL NETCDF_ERR(ERROR, 'READING smc' )
 SMCFCS = RESHAPE(DUMMY3D, (/LENSFC,LSOIL/))
 print*,'smcfcs ',maxval(smcfcs),minval(smcfcs)

 ERROR=NF90_INQ_VARID(NCID, "slc", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING slc ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy3d)
 CALL NETCDF_ERR(ERROR, 'READING slc' )
 SLCFCS = RESHAPE(DUMMY3D, (/LENSFC,LSOIL/))
 print*,'slcfcs ',maxval(slcfcs),minval(slcfcs)

 ERROR=NF90_INQ_VARID(NCID, "stc", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING stc ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy3d)
 CALL NETCDF_ERR(ERROR, 'READING stc' )
 STCFCS = RESHAPE(DUMMY3D, (/LENSFC,LSOIL/))
 print*,'stcfcs ',maxval(stcfcs),minval(stcfcs)

 DEALLOCATE(DUMMY3D)

! cloud fields not in warm restart files.  set to zero?

 CVFCS = 0.0
 CVTFCS = 0.0
 CVBFCS = 0.0

! soil layer thicknesses not in warm restart files.  hardwire
! for now.

 ZSOIL(1) = -0.1
 ZSOIL(2) = -0.4
 ZSOIL(3) = -1.0
 ZSOIL(4) = -2.0

 ERROR = NF_CLOSE(NCID)

 END SUBROUTINE READ_SFC_NETCDF
 
 END MODULE READ_WRITE_DATA
