 MODULE READ_WRITE_DATA

 USE NETCDF

 PRIVATE

! DATA STRUCTURE TO HOLD ALL NSST RECORDS.

 TYPE, PUBLIC :: NSST_DATA
   REAL, ALLOCATABLE :: C_0(:)
   REAL, ALLOCATABLE :: C_D(:)
   REAL, ALLOCATABLE :: D_CONV(:)
   REAL, ALLOCATABLE :: DT_COOL(:)
   REAL, ALLOCATABLE :: IFD(:)
   REAL, ALLOCATABLE :: QRAIN(:)
   REAL, ALLOCATABLE :: TREF(:)
   REAL, ALLOCATABLE :: TFINC(:)
   REAL, ALLOCATABLE :: W_0(:)
   REAL, ALLOCATABLE :: W_D(:)
   REAL, ALLOCATABLE :: XS(:)
   REAL, ALLOCATABLE :: XT(:)
   REAL, ALLOCATABLE :: XTTS(:)
   REAL, ALLOCATABLE :: XU(:)
   REAL, ALLOCATABLE :: XV(:)
   REAL, ALLOCATABLE :: XZ(:)
   REAL, ALLOCATABLE :: XZTS(:)
   REAL, ALLOCATABLE :: Z_C(:)
   REAL, ALLOCATABLE :: ZM(:)
 END TYPE NSST_DATA

 INTEGER, PUBLIC              :: IDIM_GAUS, JDIM_GAUS

 INTEGER, ALLOCATABLE, PUBLIC :: SLMSK_GAUS(:,:)

 REAL, ALLOCATABLE, PUBLIC    :: DTREF_GAUS(:,:)

 PUBLIC :: READ_DATA
 PUBLIC :: READ_GSI_DATA
 PUBLIC :: READ_LAT_LON_OROG
 PUBLIC :: WRITE_DATA

 CONTAINS

 subroutine write_data(slifcs,tsffcs,snofcs,tg3fcs,zorfcs, &
                       albfcs,alffcs,vegfcs,cnpfcs,f10m, &
                       t2m,q2m,vetfcs,sotfcs,ustar,fmm,fhh, &
                       sicfcs,sihfcs,sitfcs,tprcp,srflag, &
                       swdfcs,vmnfcs,vmxfcs,slpfcs, &
                       absfcs,slcfcs,smcfcs,stcfcs,&
                       idim,jdim,lensfc,lsoil,do_nsst,nsst)

!------------------------------------------------------------------
! Write out all surface records - and nsst records if selected -
! to a netcdf file.  
! 
! Note: the model restart files contain an additional snow field -
! snow cover (snocvr).  That field is required for bit identical
! reproducability.  If that record does not exist, the model
! will compute it as an initialization step.  Because this
! program does not contain the snow cover algorithm, it will
! let the model compute it.
!------------------------------------------------------------------

 implicit none

 integer, intent(in)         :: idim, jdim, lensfc, lsoil

 logical, intent(in)         :: do_nsst

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

 type(nsst_data)             :: nsst

 character(len=3)            :: rankch
 character(len=50)           :: fnbgso

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
 integer                     :: id_w_d, id_w_0, id_c_d, id_tfinc
 integer                     :: id_c_0, id_z_c, id_tref
 integer                     :: id_stc, id_smc, id_slc
 integer                     :: myrank

 real(kind=4)                :: times
 real(kind=4), allocatable   :: lsoil_data(:), x_data(:), y_data(:)
 real(kind=8), allocatable   :: dum2d(:,:), dum3d(:,:,:)

 include "mpif.h"

 call mpi_comm_rank(mpi_comm_world, myrank, error)

 write(rankch, '(i3.3)') (myrank+1)

 fnbgso = "./fnbgso." // rankch

 print*
 print*,"WRITE OUTPUT SFC DATA TO: ",trim(fnbgso)

!--- open the file
 error = NF90_CREATE(fnbgso, IOR(NF90_NETCDF4,NF90_CLASSIC_MODEL), ncid, initialsize=inital, chunksize=fsize)
 call netcdf_err(error, 'CREATING FILE='//trim(fnbgso) )

!--- define dimensions
 error = nf90_def_dim(ncid, 'xaxis_1', idim, dim_x)
 call netcdf_err(error, 'DEFINING XAXIS DIMENSION' )
 error = nf90_def_dim(ncid, 'yaxis_1', jdim, dim_y)
 call netcdf_err(error, 'DEFINING YAXIS DIMENSION' )
 error = nf90_def_dim(ncid, 'zaxis_1', lsoil, dim_lsoil)
 call netcdf_err(error, 'DEFINING ZAXIS DIMENSION' )
 error = nf90_def_dim(ncid, 'Time', 1, dim_time)
 call netcdf_err(error, 'DEFINING TIME DIMENSION' )

 !--- define fields
 error = nf90_def_var(ncid, 'xaxis_1', NF90_FLOAT, dim_x, id_x)
 call netcdf_err(error, 'DEFINING XAXIS_1 FIELD' )
 error = nf90_put_att(ncid, id_x, "long_name", "xaxis_1")
 call netcdf_err(error, 'DEFINING XAXIS_1 LONG NAME' )
 error = nf90_put_att(ncid, id_x, "units", "none")
 call netcdf_err(error, 'DEFINING XAXIS_1 UNITS' )
 error = nf90_put_att(ncid, id_x, "cartesian_axis", "X")
 call netcdf_err(error, 'WRITING XAXIS_1 FIELD' )

 error = nf90_def_var(ncid, 'yaxis_1', NF90_FLOAT, dim_y, id_y)
 call netcdf_err(error, 'DEFINING YAXIS_1 FIELD' )
 error = nf90_put_att(ncid, id_y, "long_name", "yaxis_1")
 call netcdf_err(error, 'DEFINING YAXIS_1 LONG NAME' )
 error = nf90_put_att(ncid, id_y, "units", "none")
 call netcdf_err(error, 'DEFINING YAXIS_1 UNITS' )
 error = nf90_put_att(ncid, id_y, "cartesian_axis", "Y")
 call netcdf_err(error, 'WRITING YAXIS_1 FIELD' )

 error = nf90_def_var(ncid, 'zaxis_1', NF90_FLOAT, dim_lsoil, id_lsoil)
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

 dims_3d(1) = dim_x
 dims_3d(2) = dim_y
 dims_3d(3) = dim_time

 error = nf90_def_var(ncid, 'slmsk', NF90_DOUBLE, dims_3d, id_slmsk)
 call netcdf_err(error, 'DEFINING SLMSK' )
 error = nf90_put_att(ncid, id_slmsk, "long_name", "slmsk")
 call netcdf_err(error, 'DEFINING SLMSK LONG NAME' )
 error = nf90_put_att(ncid, id_slmsk, "units", "none")
 call netcdf_err(error, 'DEFINING SLMSK UNITS' )

 error = nf90_def_var(ncid, 'tsea', NF90_DOUBLE, dims_3d, id_tsea)
 call netcdf_err(error, 'DEFINING TSEA' )
 error = nf90_put_att(ncid, id_tsea, "long_name", "tsea")
 call netcdf_err(error, 'DEFINING TSEA LONG NAME' )
 error = nf90_put_att(ncid, id_tsea, "units", "none")
 call netcdf_err(error, 'DEFINING TSEA UNITS' )

 error = nf90_def_var(ncid, 'sheleg', NF90_DOUBLE, dims_3d, id_sheleg)
 call netcdf_err(error, 'DEFINING SHELEG' )
 error = nf90_put_att(ncid, id_sheleg, "long_name", "sheleg")
 call netcdf_err(error, 'DEFINING SHELEG LONG NAME' )
 error = nf90_put_att(ncid, id_sheleg, "units", "none")
 call netcdf_err(error, 'DEFINING SHELEG UNITS' )

 error = nf90_def_var(ncid, 'tg3', NF90_DOUBLE, dims_3d, id_tg3)
 call netcdf_err(error, 'DEFINING TG3' )
 error = nf90_put_att(ncid, id_tg3, "long_name", "tg3")
 call netcdf_err(error, 'DEFINING TG3 LONG NAME' )
 error = nf90_put_att(ncid, id_tg3, "units", "none")
 call netcdf_err(error, 'DEFINING TG3 UNITS' )

 error = nf90_def_var(ncid, 'zorl', NF90_DOUBLE, dims_3d, id_zorl)
 call netcdf_err(error, 'DEFINING ZORL' )
 error = nf90_put_att(ncid, id_zorl, "long_name", "zorl")
 call netcdf_err(error, 'DEFINING ZORL LONG NAME' )
 error = nf90_put_att(ncid, id_zorl, "units", "none")
 call netcdf_err(error, 'DEFINING ZORL UNITS' )

 error = nf90_def_var(ncid, 'alvsf', NF90_DOUBLE, dims_3d, id_alvsf)
 call netcdf_err(error, 'DEFINING ALVSF' )
 error = nf90_put_att(ncid, id_alvsf, "long_name", "alvsf")
 call netcdf_err(error, 'DEFINING ALVSF LONG NAME' )
 error = nf90_put_att(ncid, id_alvsf, "units", "none")
 call netcdf_err(error, 'DEFINING ALVSF UNITS' )

 error = nf90_def_var(ncid, 'alvwf', NF90_DOUBLE, dims_3d, id_alvwf)
 call netcdf_err(error, 'DEFINING ALVWF' )
 error = nf90_put_att(ncid, id_alvwf, "long_name", "alvwf")
 call netcdf_err(error, 'DEFINING ALVWF LONG NAME' )
 error = nf90_put_att(ncid, id_alvwf, "units", "none")
 call netcdf_err(error, 'DEFINING ALVWF UNITS' )

 error = nf90_def_var(ncid, 'alnsf', NF90_DOUBLE, dims_3d, id_alnsf)
 call netcdf_err(error, 'DEFINING ALNSF' )
 error = nf90_put_att(ncid, id_alnsf, "long_name", "alnsf")
 call netcdf_err(error, 'DEFINING ALNSF LONG NAME' )
 error = nf90_put_att(ncid, id_alnsf, "units", "none")
 call netcdf_err(error, 'DEFINING ALNSF UNITS' )

 error = nf90_def_var(ncid, 'alnwf', NF90_DOUBLE, dims_3d, id_alnwf)
 call netcdf_err(error, 'DEFINING ALNWF' )
 error = nf90_put_att(ncid, id_alnwf, "long_name", "alnwf")
 call netcdf_err(error, 'DEFINING ALNWF LONG NAME' )
 error = nf90_put_att(ncid, id_alnwf, "units", "none")
 call netcdf_err(error, 'DEFINING ALNWF UNITS' )

 error = nf90_def_var(ncid, 'facsf', NF90_DOUBLE, dims_3d, id_facsf)
 call netcdf_err(error, 'DEFINING FACSF' )
 error = nf90_put_att(ncid, id_facsf, "long_name", "facsf")
 call netcdf_err(error, 'DEFINING FACSF LONG NAME' )
 error = nf90_put_att(ncid, id_facsf, "units", "none")
 call netcdf_err(error, 'DEFINING FACSF UNITS' )

 error = nf90_def_var(ncid, 'facwf', NF90_DOUBLE, dims_3d, id_facwf)
 call netcdf_err(error, 'DEFINING FACWF' )
 error = nf90_put_att(ncid, id_facwf, "long_name", "facwf")
 call netcdf_err(error, 'DEFINING FACWF LONG NAME' )
 error = nf90_put_att(ncid, id_facwf, "units", "none")
 call netcdf_err(error, 'DEFINING FACWF UNITS' )

 error = nf90_def_var(ncid, 'vfrac', NF90_DOUBLE, dims_3d, id_vfrac)
 call netcdf_err(error, 'DEFINING VFRAC' )
 error = nf90_put_att(ncid, id_vfrac, "long_name", "vfrac")
 call netcdf_err(error, 'DEFINING FACWF LONG NAME' )
 error = nf90_put_att(ncid, id_vfrac, "units", "none")
 call netcdf_err(error, 'DEFINING VFRAC UNITS' )

 error = nf90_def_var(ncid, 'canopy', NF90_DOUBLE, dims_3d, id_canopy)
 call netcdf_err(error, 'DEFINING CANOPY' )
 error = nf90_put_att(ncid, id_canopy, "long_name", "canopy")
 call netcdf_err(error, 'DEFINING CANOPY LONG NAME' )
 error = nf90_put_att(ncid, id_canopy, "units", "none")
 call netcdf_err(error, 'DEFINING CANOPY UNITS' )

 error = nf90_def_var(ncid, 'f10m', NF90_DOUBLE, dims_3d, id_f10m)
 call netcdf_err(error, 'DEFINING F10M' )
 error = nf90_put_att(ncid, id_f10m, "long_name", "f10m")
 call netcdf_err(error, 'DEFINING F10M LONG NAME' )
 error = nf90_put_att(ncid, id_f10m, "units", "none")
 call netcdf_err(error, 'DEFINING F10M UNITS' )

 error = nf90_def_var(ncid, 't2m', NF90_DOUBLE, dims_3d, id_t2m)
 call netcdf_err(error, 'DEFINING T2M' )
 error = nf90_put_att(ncid, id_t2m, "long_name", "t2m")
 call netcdf_err(error, 'DEFINING T2M LONG NAME' )
 error = nf90_put_att(ncid, id_t2m, "units", "none")
 call netcdf_err(error, 'DEFINING T2M UNITS' )

 error = nf90_def_var(ncid, 'q2m', NF90_DOUBLE, dims_3d, id_q2m)
 call netcdf_err(error, 'DEFINING Q2M' )
 error = nf90_put_att(ncid, id_q2m, "long_name", "q2m")
 call netcdf_err(error, 'DEFINING Q2M LONG NAME' )
 error = nf90_put_att(ncid, id_q2m, "units", "none")
 call netcdf_err(error, 'DEFINING Q2M UNITS' )

 error = nf90_def_var(ncid, 'vtype', NF90_DOUBLE, dims_3d, id_vtype)
 call netcdf_err(error, 'DEFINING VTYPE' )
 error = nf90_put_att(ncid, id_vtype, "long_name", "vtype")
 call netcdf_err(error, 'DEFINING VTYPE LONG NAME' )
 error = nf90_put_att(ncid, id_vtype, "units", "none")
 call netcdf_err(error, 'DEFINING VTYPE UNITS' )

 error = nf90_def_var(ncid, 'stype', NF90_DOUBLE, dims_3d, id_stype)
 call netcdf_err(error, 'DEFINING STYPE' )
 error = nf90_put_att(ncid, id_stype, "long_name", "stype")
 call netcdf_err(error, 'DEFINING STYPE LONG NAME' )
 error = nf90_put_att(ncid, id_stype, "units", "none")
 call netcdf_err(error, 'DEFINING STYPE UNITS' )

 error = nf90_def_var(ncid, 'uustar', NF90_DOUBLE, dims_3d, id_uustar)
 call netcdf_err(error, 'DEFINING UUSTAR' )
 error = nf90_put_att(ncid, id_uustar, "long_name", "uustar")
 call netcdf_err(error, 'DEFINING UUSTAR LONG NAME' )
 error = nf90_put_att(ncid, id_uustar, "units", "none")
 call netcdf_err(error, 'DEFINING UUSTAR UNITS' )

 error = nf90_def_var(ncid, 'ffmm', NF90_DOUBLE, dims_3d, id_ffmm)
 call netcdf_err(error, 'DEFINING FFMM' )
 error = nf90_put_att(ncid, id_ffmm, "long_name", "ffmm")
 call netcdf_err(error, 'DEFINING FFMM LONG NAME' )
 error = nf90_put_att(ncid, id_ffmm, "units", "none")
 call netcdf_err(error, 'DEFINING FFMM UNITS' )

 error = nf90_def_var(ncid, 'ffhh', NF90_DOUBLE, dims_3d, id_ffhh)
 call netcdf_err(error, 'DEFINING FFHH' )
 error = nf90_put_att(ncid, id_ffhh, "long_name", "ffhh")
 call netcdf_err(error, 'DEFINING FFHH LONG NAME' )
 error = nf90_put_att(ncid, id_ffhh, "units", "none")
 call netcdf_err(error, 'DEFINING FFHH UNITS' )

 error = nf90_def_var(ncid, 'hice', NF90_DOUBLE, dims_3d, id_hice)
 call netcdf_err(error, 'DEFINING HICE' )
 error = nf90_put_att(ncid, id_hice, "long_name", "hice")
 call netcdf_err(error, 'DEFINING HICE LONG NAME' )
 error = nf90_put_att(ncid, id_hice, "units", "none")
 call netcdf_err(error, 'DEFINING HICE UNITS' )

 error = nf90_def_var(ncid, 'fice', NF90_DOUBLE, dims_3d, id_fice)
 call netcdf_err(error, 'DEFINING FICE' )
 error = nf90_put_att(ncid, id_fice, "long_name", "fice")
 call netcdf_err(error, 'DEFINING FICE LONG NAME' )
 error = nf90_put_att(ncid, id_fice, "units", "none")
 call netcdf_err(error, 'DEFINING FICE UNITS' )

 error = nf90_def_var(ncid, 'tisfc', NF90_DOUBLE, dims_3d, id_tisfc)
 call netcdf_err(error, 'DEFINING TISFC' )
 error = nf90_put_att(ncid, id_tisfc, "long_name", "tisfc")
 call netcdf_err(error, 'DEFINING TISFC LONG NAME' )
 error = nf90_put_att(ncid, id_tisfc, "units", "none")
 call netcdf_err(error, 'DEFINING TISFC UNITS' )

 error = nf90_def_var(ncid, 'tprcp', NF90_DOUBLE, dims_3d, id_tprcp)
 call netcdf_err(error, 'DEFINING TPRCP' )
 error = nf90_put_att(ncid, id_tprcp, "long_name", "tprcp")
 call netcdf_err(error, 'DEFINING TPRCP LONG NAME' )
 error = nf90_put_att(ncid, id_tprcp, "units", "none")
 call netcdf_err(error, 'DEFINING TPRCP UNITS' )

 error = nf90_def_var(ncid, 'srflag', NF90_DOUBLE, dims_3d, id_srflag)
 call netcdf_err(error, 'DEFINING SRFLAG' )
 error = nf90_put_att(ncid, id_srflag, "long_name", "srflag")
 call netcdf_err(error, 'DEFINING SRFLAG LONG NAME' )
 error = nf90_put_att(ncid, id_srflag, "units", "none")
 call netcdf_err(error, 'DEFINING SRFLAG UNITS' )

 error = nf90_def_var(ncid, 'snwdph', NF90_DOUBLE, dims_3d, id_snwdph)
 call netcdf_err(error, 'DEFINING SNWDPH' )
 error = nf90_put_att(ncid, id_snwdph, "long_name", "snwdph")
 call netcdf_err(error, 'DEFINING SNWDPH LONG NAME' )
 error = nf90_put_att(ncid, id_snwdph, "units", "none")
 call netcdf_err(error, 'DEFINING SNWDPH UNITS' )

 error = nf90_def_var(ncid, 'shdmin', NF90_DOUBLE, dims_3d, id_shdmin)
 call netcdf_err(error, 'DEFINING SHDMIN' )
 error = nf90_put_att(ncid, id_shdmin, "long_name", "shdmin")
 call netcdf_err(error, 'DEFINING SHDMIN LONG NAME' )
 error = nf90_put_att(ncid, id_shdmin, "units", "none")
 call netcdf_err(error, 'DEFINING SHDMIN UNITS' )

 error = nf90_def_var(ncid, 'shdmax', NF90_DOUBLE, dims_3d, id_shdmax)
 call netcdf_err(error, 'DEFINING SHDMAX' )
 error = nf90_put_att(ncid, id_shdmax, "long_name", "shdmax")
 call netcdf_err(error, 'DEFINING SHDMAX LONG NAME' )
 error = nf90_put_att(ncid, id_shdmax, "units", "none")
 call netcdf_err(error, 'DEFINING SHDMAX UNITS' )

 error = nf90_def_var(ncid, 'slope', NF90_DOUBLE, dims_3d, id_slope)
 call netcdf_err(error, 'DEFINING SLOPE' )
 error = nf90_put_att(ncid, id_slope, "long_name", "slope")
 call netcdf_err(error, 'DEFINING SLOPE LONG NAME' )
 error = nf90_put_att(ncid, id_slope, "units", "none")
 call netcdf_err(error, 'DEFINING SLOPE UNITS' )

 error = nf90_def_var(ncid, 'snoalb', NF90_DOUBLE, dims_3d, id_snoalb)
 call netcdf_err(error, 'DEFINING SNOALB' )
 error = nf90_put_att(ncid, id_snoalb, "long_name", "snoalb")
 call netcdf_err(error, 'DEFINING SNOALB LONG NAME' )
 error = nf90_put_att(ncid, id_snoalb, "units", "none")
 call netcdf_err(error, 'DEFINING SNOALB UNITS' )

 NSST_HEADER : if (do_nsst) then

   print*
   print*,"WRITE NSST RECORDS."

   error = nf90_def_var(ncid, 'tref', NF90_DOUBLE, dims_3d, id_tref)
   call netcdf_err(error, 'DEFINING TREF' )
   error = nf90_put_att(ncid, id_tref, "long_name", "tref")
   call netcdf_err(error, 'DEFINING TREF LONG NAME' )
   error = nf90_put_att(ncid, id_tref, "units", "none")
   call netcdf_err(error, 'DEFINING TREF UNITS' )

   error = nf90_def_var(ncid, 'z_c', NF90_DOUBLE, dims_3d, id_z_c)
   call netcdf_err(error, 'DEFINING Z_C' )
   error = nf90_put_att(ncid, id_z_c, "long_name", "z_c")
   call netcdf_err(error, 'DEFINING Z_C LONG NAME' )
   error = nf90_put_att(ncid, id_z_c, "units", "none")
   call netcdf_err(error, 'DEFINING Z_C UNITS' )

   error = nf90_def_var(ncid, 'c_0', NF90_DOUBLE, dims_3d, id_c_0)
   call netcdf_err(error, 'DEFINING C_0' )
   error = nf90_put_att(ncid, id_c_0, "long_name", "c_0")
   call netcdf_err(error, 'DEFINING C_0 LONG NAME' )
   error = nf90_put_att(ncid, id_c_0, "units", "none")
   call netcdf_err(error, 'DEFINING C_0 UNITS' )

   error = nf90_def_var(ncid, 'c_d', NF90_DOUBLE, dims_3d, id_c_d)
   call netcdf_err(error, 'DEFINING C_D' )
   error = nf90_put_att(ncid, id_c_d, "long_name", "c_d")
   call netcdf_err(error, 'DEFINING C_D LONG NAME' )
   error = nf90_put_att(ncid, id_c_d, "units", "none")
   call netcdf_err(error, 'DEFINING C_D UNITS' )

   error = nf90_def_var(ncid, 'w_0', NF90_DOUBLE, dims_3d, id_w_0)
   call netcdf_err(error, 'DEFINING W_0' )
   error = nf90_put_att(ncid, id_w_0, "long_name", "w_0")
   call netcdf_err(error, 'DEFINING W_0 LONG NAME' )
   error = nf90_put_att(ncid, id_w_0, "units", "none")
   call netcdf_err(error, 'DEFINING W_0 UNITS' )

   error = nf90_def_var(ncid, 'w_d', NF90_DOUBLE, dims_3d, id_w_d)
   call netcdf_err(error, 'DEFINING W_D' )
   error = nf90_put_att(ncid, id_w_d, "long_name", "w_d")
   call netcdf_err(error, 'DEFINING W_D LONG NAME' )
   error = nf90_put_att(ncid, id_w_d, "units", "none")
   call netcdf_err(error, 'DEFINING W_D UNITS' )

   error = nf90_def_var(ncid, 'xt', NF90_DOUBLE, dims_3d, id_xt)
   call netcdf_err(error, 'DEFINING XT' )
   error = nf90_put_att(ncid, id_xt, "long_name", "xt")
   call netcdf_err(error, 'DEFINING XT LONG NAME' )
   error = nf90_put_att(ncid, id_xt, "units", "none")
   call netcdf_err(error, 'DEFINING XT UNITS' )

   error = nf90_def_var(ncid, 'xs', NF90_DOUBLE, dims_3d, id_xs)
   call netcdf_err(error, 'DEFINING XS' )
   error = nf90_put_att(ncid, id_xs, "long_name", "xs")
   call netcdf_err(error, 'DEFINING XS LONG NAME' )
   error = nf90_put_att(ncid, id_xs, "units", "none")
   call netcdf_err(error, 'DEFINING XS UNITS' )

   error = nf90_def_var(ncid, 'xu', NF90_DOUBLE, dims_3d, id_xu)
   call netcdf_err(error, 'DEFINING XU' )
   error = nf90_put_att(ncid, id_xu, "long_name", "xu")
   call netcdf_err(error, 'DEFINING XU LONG NAME' )
   error = nf90_put_att(ncid, id_xu, "units", "none")
   call netcdf_err(error, 'DEFINING XU UNITS' )

   error = nf90_def_var(ncid, 'xv', NF90_DOUBLE, dims_3d, id_xv)
   call netcdf_err(error, 'DEFINING XV' )
   error = nf90_put_att(ncid, id_xv, "long_name", "xv")
   call netcdf_err(error, 'DEFINING XV LONG NAME' )
   error = nf90_put_att(ncid, id_xv, "units", "none")
   call netcdf_err(error, 'DEFINING XV UNITS' )

   error = nf90_def_var(ncid, 'xz', NF90_DOUBLE, dims_3d, id_xz)
   call netcdf_err(error, 'DEFINING XZ' )
   error = nf90_put_att(ncid, id_xz, "long_name", "xz")
   call netcdf_err(error, 'DEFINING XZ LONG NAME' )
   error = nf90_put_att(ncid, id_xz, "units", "none")
   call netcdf_err(error, 'DEFINING XZ UNITS' )

   error = nf90_def_var(ncid, 'zm', NF90_DOUBLE, dims_3d, id_zm)
   call netcdf_err(error, 'DEFINING ZM' )
   error = nf90_put_att(ncid, id_zm, "long_name", "zm")
   call netcdf_err(error, 'DEFINING ZM LONG NAME' )
   error = nf90_put_att(ncid, id_zm, "units", "none")
   call netcdf_err(error, 'DEFINING ZM UNITS' )

   error = nf90_def_var(ncid, 'xtts', NF90_DOUBLE, dims_3d, id_xtts)
   call netcdf_err(error, 'DEFINING XTTS' )
   error = nf90_put_att(ncid, id_xtts, "long_name", "xtts")
   call netcdf_err(error, 'DEFINING XTTS LONG NAME' )
   error = nf90_put_att(ncid, id_xtts, "units", "none")
   call netcdf_err(error, 'DEFINING XTTS UNITS' )

   error = nf90_def_var(ncid, 'xzts', NF90_DOUBLE, dims_3d, id_xzts)
   call netcdf_err(error, 'DEFINING XZTS' )
   error = nf90_put_att(ncid, id_xzts, "long_name", "xzts")
   call netcdf_err(error, 'DEFINING XZTS LONG NAME' )
   error = nf90_put_att(ncid, id_xzts, "units", "none")
   call netcdf_err(error, 'DEFINING XZTS UNITS' )

   error = nf90_def_var(ncid, 'd_conv', NF90_DOUBLE, dims_3d, id_d_conv)
   call netcdf_err(error, 'DEFINING D_CONV' )
   error = nf90_put_att(ncid, id_d_conv, "long_name", "d_conv")
   call netcdf_err(error, 'DEFINING D_CONV LONG NAME' )
   error = nf90_put_att(ncid, id_d_conv, "units", "none")
   call netcdf_err(error, 'DEFINING D_CONV UNITS' )

   error = nf90_def_var(ncid, 'ifd', NF90_DOUBLE, dims_3d, id_ifd)
   call netcdf_err(error, 'DEFINING IFD' )
   error = nf90_put_att(ncid, id_ifd, "long_name", "ifd")
   call netcdf_err(error, 'DEFINING IFD LONG NAME' )
   error = nf90_put_att(ncid, id_ifd, "units", "none")
   call netcdf_err(error, 'DEFINING IFD UNITS' )

   error = nf90_def_var(ncid, 'dt_cool', NF90_DOUBLE, dims_3d, id_dt_cool)
   call netcdf_err(error, 'DEFINING DT_COOL' )
   error = nf90_put_att(ncid, id_dt_cool, "long_name", "dt_cool")
   call netcdf_err(error, 'DEFINING DT_COOL LONG NAME' )
   error = nf90_put_att(ncid, id_dt_cool, "units", "none")
   call netcdf_err(error, 'DEFINING DT_COOL UNITS' )

   error = nf90_def_var(ncid, 'qrain', NF90_DOUBLE, dims_3d, id_qrain)
   call netcdf_err(error, 'DEFINING QRAIN' )
   error = nf90_put_att(ncid, id_qrain, "long_name", "qrain")
   call netcdf_err(error, 'DEFINING QRAIN LONG NAME' )
   error = nf90_put_att(ncid, id_qrain, "units", "none")
   call netcdf_err(error, 'DEFINING QRAIN UNITS' )

   error = nf90_def_var(ncid, 'tfinc', NF90_DOUBLE, dims_3d, id_tfinc)
   call netcdf_err(error, 'DEFINING TFINC' )
   error = nf90_put_att(ncid, id_tfinc, "long_name", "tfinc")
   call netcdf_err(error, 'DEFINING TFINC LONG NAME' )
   error = nf90_put_att(ncid, id_tfinc, "units", "none")
   call netcdf_err(error, 'DEFINING TFINC UNITS' )

 endif NSST_HEADER

 dims_4d(1) = dim_x
 dims_4d(2) = dim_y
 dims_4d(3) = dim_lsoil
 dims_4d(4) = dim_time

 error = nf90_def_var(ncid, 'stc', NF90_DOUBLE, dims_4d, id_stc)
 call netcdf_err(error, 'DEFINING STC' )
 error = nf90_put_att(ncid, id_stc, "long_name", "stc")
 call netcdf_err(error, 'DEFINING STC LONG NAME' )
 error = nf90_put_att(ncid, id_stc, "units", "none")
 call netcdf_err(error, 'DEFINING STC UNITS' )

 error = nf90_def_var(ncid, 'smc', NF90_DOUBLE, dims_4d, id_smc)
 call netcdf_err(error, 'DEFINING SMC' )
 error = nf90_put_att(ncid, id_smc, "long_name", "smc")
 call netcdf_err(error, 'DEFINING SMC LONG NAME' )
 error = nf90_put_att(ncid, id_smc, "units", "none")
 call netcdf_err(error, 'DEFINING SMC UNITS' )

 error = nf90_def_var(ncid, 'slc', NF90_DOUBLE, dims_4d, id_slc)
 call netcdf_err(error, 'DEFINING SLC' )
 error = nf90_put_att(ncid, id_slc, "long_name", "slc")
 call netcdf_err(error, 'DEFINING SLC LONG NAME' )
 error = nf90_put_att(ncid, id_slc, "units", "none")
 call netcdf_err(error, 'DEFINING SLC UNITS' )

 error = nf90_enddef(ncid, header_buffer_val,4,0,4)
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

 error = nf90_put_var( ncid, id_lsoil, lsoil_data)
 call netcdf_err(error, 'WRITING ZAXIS RECORD' )
 error = nf90_put_var( ncid, id_x, x_data)
 call netcdf_err(error, 'WRITING XAXIS RECORD' )
 error = nf90_put_var( ncid, id_y, y_data)
 call netcdf_err(error, 'WRITING YAXIS RECORD' )
 times = 1.0
 error = nf90_put_var( ncid, id_time, times)
 call netcdf_err(error, 'WRITING TIME RECORD' )

 deallocate(lsoil_data, x_data, y_data)

 dims_strt(1:3) = 1
 dims_end(1) = idim
 dims_end(2) = jdim
 dims_end(3) = 1

 allocate(dum2d(idim,jdim))

 dum2d = reshape(slifcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_slmsk, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING LANDMASK RECORD' )

 dum2d = reshape(tsffcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_tsea, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING TSEA RECORD' )

 dum2d = reshape(snofcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_sheleg, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING SHELEG RECORD' )

 dum2d = reshape(tg3fcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_tg3, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING TG3 RECORD' )

 dum2d = reshape(zorfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_zorl, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING ZORL RECORD' )

 dum2d = reshape(albfcs(:,1), (/idim,jdim/))
 error = nf90_put_var( ncid, id_alvsf, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING ALVSF RECORD' )

 dum2d = reshape(albfcs(:,2), (/idim,jdim/))
 error = nf90_put_var( ncid, id_alvwf, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING ALVWF RECORD' )

 dum2d = reshape(albfcs(:,3), (/idim,jdim/))
 error = nf90_put_var( ncid, id_alnsf, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING ALNSF RECORD' )

 dum2d = reshape(albfcs(:,4), (/idim,jdim/))
 error = nf90_put_var( ncid, id_alnwf, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING ALNWF RECORD' )

 dum2d = reshape(alffcs(:,1), (/idim,jdim/))
 error = nf90_put_var( ncid, id_facsf, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING FACSF RECORD' )

 dum2d = reshape(alffcs(:,2), (/idim,jdim/))
 error = nf90_put_var( ncid, id_facwf, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING FACWF RECORD' )

 dum2d = reshape(vegfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_vfrac, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING VFRAC RECORD' )

 dum2d = reshape(cnpfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_canopy, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING CANOPY RECORD' )

 dum2d = reshape(f10m, (/idim,jdim/))
 error = nf90_put_var( ncid, id_f10m, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING F10M RECORD' )

 dum2d = reshape(t2m, (/idim,jdim/))
 error = nf90_put_var( ncid, id_t2m, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING T2M RECORD' )

 dum2d = reshape(q2m, (/idim,jdim/))
 error = nf90_put_var( ncid, id_q2m, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING Q2M RECORD' )

 dum2d = reshape(vetfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_vtype, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING VTYPE RECORD' )

 dum2d = reshape(sotfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_stype, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING STYPE RECORD' )

 dum2d = reshape(ustar, (/idim,jdim/))
 error = nf90_put_var( ncid, id_uustar, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING UUSTAR RECORD' )

 dum2d = reshape(fmm, (/idim,jdim/))
 error = nf90_put_var( ncid, id_ffmm, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING FFMM RECORD' )

 dum2d = reshape(fhh, (/idim,jdim/))
 error = nf90_put_var( ncid, id_ffhh, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING FFHH RECORD' )

 dum2d = reshape(sihfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_hice, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING HICE RECORD' )

 dum2d = reshape(sicfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_fice, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING FICE RECORD' )

 dum2d = reshape(sitfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_tisfc, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING TISFC RECORD' )

 dum2d = reshape(tprcp, (/idim,jdim/))
 error = nf90_put_var( ncid, id_tprcp, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING TPRCP RECORD' )

 dum2d = reshape(srflag, (/idim,jdim/))
 error = nf90_put_var( ncid, id_srflag, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING SRFLAG RECORD' )

 dum2d = reshape(swdfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_snwdph, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING SNWDPH RECORD' )

 dum2d = reshape(vmnfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_shdmin, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING SHDMIN RECORD' )

 dum2d = reshape(vmxfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_shdmax, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING SHDMAX RECORD' )

 dum2d = reshape(slpfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_slope, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING SLOPE RECORD' )

 dum2d = reshape(absfcs, (/idim,jdim/))
 error = nf90_put_var( ncid, id_snoalb, dum2d, dims_strt, dims_end)
 call netcdf_err(error, 'WRITING SNOALB RECORD' )

 NSST_WRITE : if (do_nsst) then

   dum2d = reshape(nsst%tref, (/idim,jdim/))
   error = nf90_put_var( ncid, id_tref, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING TREF RECORD' )

   dum2d = reshape(nsst%z_c, (/idim,jdim/))
   error = nf90_put_var( ncid, id_z_c, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING Z_C RECORD' )

   dum2d = reshape(nsst%c_0, (/idim,jdim/))
   error = nf90_put_var( ncid, id_c_0, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING C_0 RECORD' )

   dum2d = reshape(nsst%c_d, (/idim,jdim/))
   error = nf90_put_var( ncid, id_c_d, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING C_D RECORD' )

   dum2d = reshape(nsst%w_0, (/idim,jdim/))
   error = nf90_put_var( ncid, id_w_0, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING W_0 RECORD' )

   dum2d = reshape(nsst%w_d, (/idim,jdim/))
   error = nf90_put_var( ncid, id_w_d, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING W_D RECORD' )

   dum2d = reshape(nsst%xt, (/idim,jdim/))
   error = nf90_put_var( ncid, id_xt, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING XT RECORD' )

   dum2d = reshape(nsst%xs, (/idim,jdim/))
   error = nf90_put_var( ncid, id_xs, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING XS RECORD' )

   dum2d = reshape(nsst%xu, (/idim,jdim/))
   error = nf90_put_var( ncid, id_xu, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING XU RECORD' )

   dum2d = reshape(nsst%xv, (/idim,jdim/))
   error = nf90_put_var( ncid, id_xv, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING XV RECORD' )

   dum2d = reshape(nsst%xz, (/idim,jdim/))
   error = nf90_put_var( ncid, id_xz, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING XZ RECORD' )

   dum2d = reshape(nsst%zm, (/idim,jdim/))
   error = nf90_put_var( ncid, id_zm, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING ZM RECORD' )

   dum2d = reshape(nsst%zm, (/idim,jdim/))
   error = nf90_put_var( ncid, id_zm, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING ZM RECORD' )

   dum2d = reshape(nsst%xtts, (/idim,jdim/))
   error = nf90_put_var( ncid, id_xtts, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING XTTS RECORD' )

   dum2d = reshape(nsst%xzts, (/idim,jdim/))
   error = nf90_put_var( ncid, id_xzts, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING XZTS RECORD' )

   dum2d = reshape(nsst%d_conv, (/idim,jdim/))
   error = nf90_put_var( ncid, id_d_conv, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING D_CONV RECORD' )

   dum2d = reshape(nsst%ifd, (/idim,jdim/))
   error = nf90_put_var( ncid, id_ifd, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING IFD RECORD' )

   dum2d = reshape(nsst%dt_cool, (/idim,jdim/))
   error = nf90_put_var( ncid, id_dt_cool, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING DT_COOL RECORD' )

   dum2d = reshape(nsst%qrain, (/idim,jdim/))
   error = nf90_put_var( ncid, id_qrain, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING QRAIN RECORD' )

   dum2d = reshape(nsst%tfinc, (/idim,jdim/))
   error = nf90_put_var( ncid, id_tfinc, dum2d, dims_strt, dims_end)
   call netcdf_err(error, 'WRITING TFINC RECORD' )

 endif NSST_WRITE

 deallocate(dum2d)

 dims4_strt(1:4) = 1
 dims4_end(1) = idim
 dims4_end(2) = jdim
 dims4_end(3) = lsoil
 dims4_end(4) = 1

 allocate(dum3d(idim,jdim,lsoil))

 dum3d = reshape(slcfcs, (/idim,jdim,lsoil/))
 error = nf90_put_var( ncid, id_slc, dum3d, dims4_strt, dims4_end)
 call netcdf_err(error, 'WRITING SLC RECORD' )

 dum3d = reshape(smcfcs, (/idim,jdim,lsoil/))
 error = nf90_put_var( ncid, id_smc, dum3d, dims4_strt, dims4_end)
 call netcdf_err(error, 'WRITING SMC RECORD' )

 dum3d = reshape(stcfcs, (/idim,jdim,lsoil/))
 error = nf90_put_var( ncid, id_stc, dum3d, dims4_strt, dims4_end)
 call netcdf_err(error, 'WRITING STC RECORD' )

 deallocate(dum3d)

 error = nf90_close(ncid)

 end subroutine write_data

 SUBROUTINE READ_LAT_LON_OROG(RLA,RLO,OROG,OROG_UF,&
                              TILE_NUM,IDIM,JDIM,IJDIM)

!--------------------------------------------------------------
! READ LATITUDE, LONGITUDE, FILTERED OROGRAPHY, AND
! UNFILTERED OROGRAPHY FOR THE CUBED-SPHERE TILE FROM
! THE "GRID" FILE.
!--------------------------------------------------------------

 IMPLICIT NONE

 include "mpif.h"

 INTEGER, INTENT(IN)    :: IDIM, JDIM, IJDIM

 CHARACTER(LEN=5), INTENT(OUT) :: TILE_NUM

 REAL, INTENT(OUT)      :: RLA(IJDIM),RLO(IJDIM)
 REAL, INTENT(OUT)      :: OROG(IJDIM),OROG_UF(IJDIM)

 CHARACTER(LEN=50)      :: FNOROG, FNGRID
 CHARACTER(LEN=3)       :: RANKCH

 INTEGER                :: ERROR, NCID, NCID_OROG
 INTEGER                :: I, II, J, JJ, MYRANK
 INTEGER                :: ID_DIM, ID_VAR, NX, NY

 REAL, ALLOCATABLE         :: DUMMY(:,:), GEOLAT(:,:), GEOLON(:,:)
 REAL(KIND=4), ALLOCATABLE :: DUMMY4(:,:)

 CALL MPI_COMM_RANK(MPI_COMM_WORLD, MYRANK, ERROR)

 WRITE(RANKCH, '(I3.3)') (MYRANK+1)

 FNGRID = "./fngrid." // RANKCH

 PRINT*
 PRINT*, "READ FV3 GRID INFO FROM: "//TRIM(FNGRID)

 ERROR=NF90_OPEN(TRIM(FNGRID),NF90_NOWRITE,NCID)
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
   CALL MPI_ABORT(MPI_COMM_WORLD, 130)
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

 ERROR=NF90_INQ_VARID(NCID, 'tile', ID_VAR)
 CALL NETCDF_ERR(ERROR, 'ERROR READING TILE ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, TILE_NUM)
 CALL NETCDF_ERR(ERROR, 'ERROR READING TILE RECORD' )

 ERROR = NF90_CLOSE(NCID)

 FNOROG = "./fnorog." // RANKCH

 PRINT*
 PRINT*, "READ FV3 OROG INFO FROM: "//TRIM(FNOROG)

 ERROR=NF90_OPEN(TRIM(FNOROG),NF90_NOWRITE,NCID_OROG)
 CALL NETCDF_ERR(ERROR, 'OPENING FILE: '//TRIM(FNOROG) )

 ALLOCATE(DUMMY4(IDIM,JDIM))

 ERROR=NF90_INQ_VARID(NCID_OROG, 'orog_raw', ID_VAR)
 CALL NETCDF_ERR(ERROR, 'ERROR READING orog_raw ID' )
 ERROR=NF90_GET_VAR(NCID_OROG, ID_VAR, DUMMY4)
 CALL NETCDF_ERR(ERROR, 'ERROR READING orog_raw RECORD' )
 OROG_UF = RESHAPE(DUMMY4, (/IJDIM/))

 ERROR=NF90_INQ_VARID(NCID_OROG, 'orog_filt', ID_VAR)
 CALL NETCDF_ERR(ERROR, 'ERROR READING orog_filt ID' )
 ERROR=NF90_GET_VAR(NCID_OROG, ID_VAR, DUMMY4)
 CALL NETCDF_ERR(ERROR, 'ERROR READING orog_filt RECORD' )
 OROG = RESHAPE(DUMMY4, (/IJDIM/))

 DEALLOCATE(DUMMY4)

 ERROR = NF90_CLOSE(NCID_OROG)

 END SUBROUTINE READ_LAT_LON_OROG

 SUBROUTINE NETCDF_ERR( ERR, STRING )

!--------------------------------------------------------------
! IF AT NETCDF CALL RETURNS AN ERROR, PRINT OUT A MESSAGE
! AND STOP PROCESSING.
!--------------------------------------------------------------

 IMPLICIT NONE

 include 'mpif.h'

 INTEGER, INTENT(IN) :: ERR
 CHARACTER(LEN=*), INTENT(IN) :: STRING
 CHARACTER(LEN=80) :: ERRMSG

 IF( ERR == NF90_NOERR )RETURN
 ERRMSG = NF90_STRERROR(ERR)
 PRINT*,''
 PRINT*,'FATAL ERROR: ', TRIM(STRING), ': ', TRIM(ERRMSG)
 PRINT*,'STOP.'
 CALL MPI_ABORT(MPI_COMM_WORLD, 999)

 RETURN
 END SUBROUTINE NETCDF_ERR

 SUBROUTINE READ_GSI_DATA(GSI_FILE)

!-----------------------------------------------------------------
! READ FILE FROM THE GSI CONTAINING THE FOUNDATION TEMPERATURE
! INCREMENTS AND MASK.  DATA IS IN NETCDF AND ON A GAUSSIAN GRID. 
! THE GRID CONTAINS TWO EXTRA ROWS FOR EACH POLE.  THE 
! INTERPOLATION FROM GAUSSIAN TO NATIVE GRID ASSUMES NO POLE
! POINTS, SO THESE ARE REMOVED.  
!-----------------------------------------------------------------

 IMPLICIT NONE

 CHARACTER(LEN=*), INTENT(IN)     :: GSI_FILE

 INTEGER                          :: ERROR, ID_DIM, NCID
 INTEGER                          :: ID_VAR, J

 INTEGER(KIND=1), ALLOCATABLE     :: IDUMMY(:,:)

 REAL(KIND=8), ALLOCATABLE        :: DUMMY(:,:)

 PRINT*
 PRINT*, "READ INPUT GSI DATA FROM: "//TRIM(GSI_FILE)

 ERROR=NF90_OPEN(TRIM(GSI_FILE),NF90_NOWRITE,NCID)
 CALL NETCDF_ERR(ERROR, 'OPENING FILE: '//TRIM(GSI_FILE) )

 ERROR=NF90_INQ_DIMID(NCID, 'latitude', ID_DIM)
 CALL NETCDF_ERR(ERROR, 'READING latitude' )
 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=JDIM_GAUS)
 CALL NETCDF_ERR(ERROR, 'READING latitude' )
 JDIM_GAUS = JDIM_GAUS - 2  ! WILL IGNORE POLE POINTS
 
 ERROR=NF90_INQ_DIMID(NCID, 'longitude', ID_DIM)
 CALL NETCDF_ERR(ERROR, 'READING longitude' )
 ERROR=NF90_INQUIRE_DIMENSION(NCID,ID_DIM,LEN=IDIM_GAUS)
 CALL NETCDF_ERR(ERROR, 'READING longitude' )

 ALLOCATE(DUMMY(IDIM_GAUS,JDIM_GAUS+2))
 ALLOCATE(DTREF_GAUS(IDIM_GAUS,JDIM_GAUS))

 ERROR=NF90_INQ_VARID(NCID, "dtf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING dtf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, DUMMY)
 CALL NETCDF_ERR(ERROR, 'READING dtf' )

 ALLOCATE(IDUMMY(IDIM_GAUS,JDIM_GAUS+2))
 ALLOCATE(SLMSK_GAUS(IDIM_GAUS,JDIM_GAUS))

 ERROR=NF90_INQ_VARID(NCID, "msk", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING msk ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, IDUMMY)
 CALL NETCDF_ERR(ERROR, 'READING msk' )

! REMOVE POLE POINTS.

 DO J = 1, JDIM_GAUS
   SLMSK_GAUS(:,J) = IDUMMY(:,J+1)
   DTREF_GAUS(:,J) = DUMMY(:,J+1)
 ENDDO

 DEALLOCATE(DUMMY)
 DEALLOCATE(IDUMMY)

 ERROR = NF90_CLOSE(NCID)

 END SUBROUTINE READ_GSI_DATA

 SUBROUTINE READ_DATA(TSFFCS,SMCFCS,SNOFCS,STCFCS, &
                      TG3FCS,ZORFCS, &
                      CVFCS,CVBFCS,CVTFCS,ALBFCS, &
                      SLIFCS,VEGFCS,CNPFCS,F10M, &
                      VETFCS,SOTFCS,ALFFCS, &
                      USTAR,FMM,FHH, &
                      SIHFCS,SICFCS,SITFCS, &
                      TPRCP,SRFLAG,SWDFCS,  &
                      VMNFCS,VMXFCS,SLCFCS, &
                      SLPFCS,ABSFCS,T2M,Q2M,SLMASK, &
                      ZSOIL,LSOIL,LENSFC,DO_NSST,NSST)

!-----------------------------------------------------------------
! READ THE FIRST GUESS SURFACE RECORDS AND NSST RECORDS (IF
! SELECTED) FOR A SINGLE CUBED-SPHERE TILE.
!-----------------------------------------------------------------

 IMPLICIT NONE

 include "mpif.h"

 INTEGER, INTENT(IN)       :: LSOIL, LENSFC

 LOGICAL, INTENT(IN)       :: DO_NSST

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

 TYPE(NSST_DATA)           :: NSST

 CHARACTER(LEN=50)         :: FNBGSI
 CHARACTER(LEN=3)          :: RANKCH

 INTEGER                   :: ERROR, NCID, MYRANK
 INTEGER                   :: IDIM, JDIM, ID_DIM
 INTEGER                   :: ID_VAR

 REAL(KIND=8), ALLOCATABLE :: DUMMY(:,:), DUMMY3D(:,:,:)

 CALL MPI_COMM_RANK(MPI_COMM_WORLD, MYRANK, ERROR)

 WRITE(RANKCH, '(I3.3)') (MYRANK+1)

 FNBGSI = "./fnbgsi." // RANKCH

 PRINT*
 PRINT*, "READ INPUT SFC DATA FROM: "//TRIM(FNBGSI)

 ERROR=NF90_OPEN(TRIM(FNBGSI),NF90_NOWRITE,NCID)
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
   CALL MPI_ABORT(MPI_COMM_WORLD, 88)
 ENDIF

 ALLOCATE(DUMMY(IDIM,JDIM))

 ERROR=NF90_INQ_VARID(NCID, "tsea", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING tsea ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING tsea' )
 TSFFCS = RESHAPE(DUMMY, (/LENSFC/))

 ERROR=NF90_INQ_VARID(NCID, "sheleg", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING sheleg ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING sheleg' )
 SNOFCS = RESHAPE(DUMMY, (/LENSFC/))

 ERROR=NF90_INQ_VARID(NCID, "tg3", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING tg3 ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING tg3' )
 TG3FCS = RESHAPE(DUMMY, (/LENSFC/))

 ERROR=NF90_INQ_VARID(NCID, "zorl", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING zorl ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING zorl' )
 ZORFCS = RESHAPE(DUMMY, (/LENSFC/))

 ERROR=NF90_INQ_VARID(NCID, "alvsf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING alvsf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING alvsf' )
 ALBFCS(:,1) = RESHAPE(DUMMY, (/LENSFC/))

 ERROR=NF90_INQ_VARID(NCID, "alvwf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING alvwf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING alvwf' )
 ALBFCS(:,2) = RESHAPE(DUMMY, (/LENSFC/))

 ERROR=NF90_INQ_VARID(NCID, "alnsf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING alnsf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING alnsf' )
 ALBFCS(:,3) = RESHAPE(DUMMY, (/LENSFC/))

 ERROR=NF90_INQ_VARID(NCID, "alnwf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING alnwf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING alnwf' )
 ALBFCS(:,4) = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "slmsk", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING slmsk ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING slmsk' )
 SLIFCS = RESHAPE(DUMMY, (/LENSFC/))
 SLMASK = SLIFCS
 WHERE (SLMASK > 1.5) SLMASK=0.0  ! remove sea ice
  
 ERROR=NF90_INQ_VARID(NCID, "canopy", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING canopy ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING canopy' )
 CNPFCS = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "vfrac", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING vfrac ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING vfrac' )
 VEGFCS = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "f10m", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING f10m ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING f10m' )
 F10M = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "vtype", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING vtype ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING vtype' )
 VETFCS = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "stype", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING stype ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING stype' )
 SOTFCS = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "facsf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING facsf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING facsf' )
 ALFFCS(:,1) = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "facwf", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING facwf ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING facwf' )
 ALFFCS(:,2) = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "uustar", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING uustar ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING uustar' )
 USTAR = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "ffmm", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING ffmm ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING ffmm' )
 FMM = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "ffhh", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING ffhh ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING ffhh' )
 FHH = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "hice", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING hice ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING hice' )
 SIHFCS = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "fice", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING fice ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING fice' )
 SICFCS = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "tisfc", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING tisfc ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING tisfc' )
 SITFCS = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "tprcp", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING tprcp ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING tprcp' )
 TPRCP = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "srflag", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING srflag ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING srflag' )
 SRFLAG = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "snwdph", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING snwdph ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING snwdph' )
 SWDFCS = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "shdmin", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING shdmin ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING shdmin' )
 VMNFCS = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "shdmax", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING shdmax ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING shdmax' )
 VMXFCS = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "slope", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING slope ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING slope' )
 SLPFCS = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "snoalb", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING snoalb ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING snoalb' )
 ABSFCS = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "t2m", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING t2m ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING t2m' )
 T2M = RESHAPE(DUMMY, (/LENSFC/))
  
 ERROR=NF90_INQ_VARID(NCID, "q2m", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING q2m ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
 CALL NETCDF_ERR(ERROR, 'READING q2m' )
 Q2M = RESHAPE(DUMMY, (/LENSFC/))
  
 NSST_READ : IF(DO_NSST) THEN

   PRINT*
   PRINT*,"WILL READ NSST RECORDS."

   ERROR=NF90_INQ_VARID(NCID, "c_0", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING c_0 ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING c_0' )
   NSST%C_0 = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "c_d", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING c_d ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING c_d' )
   NSST%C_D = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "d_conv", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING d_conv ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING d_conv' )
   NSST%D_CONV = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "dt_cool", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING dt_cool ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING dt_cool' )
   NSST%DT_COOL = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "ifd", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING ifd ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING ifd' )
   NSST%IFD = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "qrain", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING qrain ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING qrain' )
   NSST%QRAIN = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "tref", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING tref ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING tref' )
   NSST%TREF = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "w_0", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING w_0 ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING w_0' )
   NSST%W_0 = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "w_d", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING w_d ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING w_d' )
   NSST%W_D = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "xs", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING xs ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING xs' )
   NSST%XS = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "xt", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING xt ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING xt' )
   NSST%XT = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "xtts", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING xtts ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING xtts' )
   NSST%XTTS = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "xu", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING xu ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING xu' )
   NSST%XU = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "xv", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING xv ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING xv' )
   NSST%XV = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "xz", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING xz ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING xz' )
   NSST%XZ = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "xzts", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING xzts ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING xzts' )
   NSST%XZTS = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "z_c", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING z_c ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING z_c' )
   NSST%Z_C = RESHAPE(DUMMY, (/LENSFC/))

   ERROR=NF90_INQ_VARID(NCID, "zm", ID_VAR)
   CALL NETCDF_ERR(ERROR, 'READING zm ID' )
   ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy)
   CALL NETCDF_ERR(ERROR, 'READING zm' )
   NSST%ZM = RESHAPE(DUMMY, (/LENSFC/))

 END IF NSST_READ

 DEALLOCATE(DUMMY)

 ALLOCATE(DUMMY3D(IDIM,JDIM,LSOIL))

 ERROR=NF90_INQ_VARID(NCID, "smc", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING smc ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy3d)
 CALL NETCDF_ERR(ERROR, 'READING smc' )
 SMCFCS = RESHAPE(DUMMY3D, (/LENSFC,LSOIL/))

 ERROR=NF90_INQ_VARID(NCID, "slc", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING slc ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy3d)
 CALL NETCDF_ERR(ERROR, 'READING slc' )
 SLCFCS = RESHAPE(DUMMY3D, (/LENSFC,LSOIL/))

 ERROR=NF90_INQ_VARID(NCID, "stc", ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING stc ID' )
 ERROR=NF90_GET_VAR(NCID, ID_VAR, dummy3d)
 CALL NETCDF_ERR(ERROR, 'READING stc' )
 STCFCS = RESHAPE(DUMMY3D, (/LENSFC,LSOIL/))

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

 ERROR = NF90_CLOSE(NCID)

 END SUBROUTINE READ_DATA
 
 END MODULE READ_WRITE_DATA
