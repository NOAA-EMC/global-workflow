module convert_netcdf_mod
use abstract_convert_netcdf_mod 
  type, extends(abstract_convert_netcdf_class) :: convert_netcdf_class
  contains
    procedure, pass(this) :: convert_netcdf_mass => convert_netcdf_mass_wrf
    procedure, pass(this) :: convert_netcdf_nmm  => convert_netcdf_nmm_wrf
    procedure, pass(this) :: update_netcdf_mass  => update_netcdf_mass_wrf
    procedure, pass(this) :: update_netcdf_nmm   => update_netcdf_nmm_wrf
  end type convert_netcdf_class
contains  
  subroutine convert_netcdf_mass_wrf(this)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    convert_netcdf_mass   read wrf mass netcdf restart
  !   prgmmr: parrish          org: np22                date: 2003-09-05
  !
  ! abstract: using wrf library routines, read a wrf mass core netcdf
  !             format restart file.  write the result to temporary netcdf
  !             file expected by read_wrf_mass_guess.
  !
  ! program history log:
  !   2004-09-10  parrish
  !   2004-11-05  treadon - add return code 74 for error stop
  !   2004-12-15  treadon - remove get_lun, read guess from file "wrf_inout"
  !   2005-07-06  parrish - add read of pint byte address
  !   2005-12-09  middlecoff - initialize character variable staggering
  !   2006-09-15  treadon - use nhr_assimilation to build local guess filename
  !   2010-03-29  Hu  - add code to read 5 cloud/hydrometeor variables for cloud analysis
  !   2010-03-29  Hu  - bug fix: replace XICE with SEAICE 
  !   2010-10-20  hclin   - added 15 wrfchem/gocart fields for aod
  !   2011-11-14  tong - add loop to read upto 7 wrf mass netcdf restart file and
  !                          write to temporary netcdf files (extend FGAT capability for
  !                          wrf mass netcdf format)
  !   2012-11-26  Hu  - add code to read surface variables for GSD soil nudging
  !   2014-03-12  hu     - add code to read ges_q2 (2m Q), 
  !                               Qnr(rain number concentration), 
  !                               and nsoil (number of soil levels)
  !   2015-01-13  ladwig - add code for Qni and Qnc (cloud ice and water number
  !                                concentration)
  !   2017-03-23  Hu  - add code to read hybrid vertical coodinate in WRF MASS
  !                         core
  !
  !   2016-02-14 Johnson, Y. Wang, X. Wang  - add code to read vertical velocity  (W) and
  !                                           Reflectivity (REFL_10CM) for radar
  !                                           DA, POC: xuguang.wang@ou.edu
  !   2020-09-13 CAPS(C. Liu, L. Chen, and H. Li) 
  !                            - add code for hydrometer variables needed for direct reflectivity DA
  !                            - add CV transform option on hydrometer variables for direct reflectivity DA
  !
  !   input argument list:
  !
  !   output argument list:
  !
  !     NOTES:  this is beginning of allowing direct connection of gsi to wrf files
  !             without seperate external interface.  it is very inefficient, and
  !             later versions will be made to reduce the total i/o involved.
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: r_single,i_kind
    use constants, only: h300
    use gsi_4dvar, only: nhr_assimilation
    use rapidrefresh_cldsurf_mod, only: l_hydrometeor_bkio,l_gsd_soilTQ_nudge
    use rapidrefresh_cldsurf_mod, only: i_use_2mt4b,i_use_2mq4b
    use gsi_metguess_mod, only: gsi_metguess_get
    use chemmod, only: laeroana_gocart, ppmv_conv,wrf_pm2_5
    use gsi_chemguess_mod, only: gsi_chemguess_get
    use gridmod, only: wrf_mass_hybridcord
    use netcdf_mod, only: nc_check

    use wrf_vars_mod, only : w_exist, dbz_exist
    use constants, only: zero
    use obsmod, only   : if_model_dbz
    use gsi_io, only: verbose
    use directDA_radaruse_mod, only: l_use_dbz_directDA
  
    implicit none
  
  ! Declare local parameters
    class(convert_netcdf_class), intent(inout) :: this
    real(r_single),parameter:: one_single = 1.0_r_single
    real(r_single),parameter:: r45 = 45.0_r_single
  
    character(len=120) :: flnm1
    character(len=19)  :: DateStr1
    character(len=6)   :: filename
    integer(i_kind)            :: dh1
    
    integer(i_kind) :: iunit
    
    integer(i_kind) :: i,j,k
    integer(i_kind) :: ndim1
    integer(i_kind) :: WrfType
    integer(i_kind), dimension(4)  :: start_index, end_index
    character (len= 4) :: staggering=' N/A'
    character (len= 3) :: ordering
    
    character (len=80), dimension(3)  ::  dimnames
    character (len=80) :: SysDepInfo
   
    integer(i_kind) :: n_actual_clouds, ierr, Status, Status_next_time, n
    integer(i_kind) :: iv, n_gocart_var, ier
    
  ! binary stuff
  
  ! rmse stuff
    
    character (len=31) :: rmse_var
    integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
    integer(i_kind) nlon_regional,nlat_regional,nsig_regional,nsig_soil_regional
    real(r_single) pt_regional
    real(r_single) rdx,rdy
    real(r_single),allocatable::field3(:,:,:),field2(:,:),field1(:),field2b(:,:),field2c(:,:)
    real(r_single),allocatable::field3u(:,:,:),field3v(:,:,:),field1a(:)

    real(r_single),allocatable::field3w(:,:,:)

    integer(i_kind),allocatable::ifield2(:,:)
    real(r_single) rad2deg_single
    integer(i_kind) wrf_real
    data iunit / 15 /
    logical print_verbose
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
    
    print_verbose=.false.
    if(verbose) print_verbose=.true.
    wrf_real=104
    end_index=0
    start_index=0
    
  ! Inquire about cloud guess fields
    call gsi_metguess_get('clouds::3d',n_actual_clouds,ierr)
  
  ! transfer code from diffwrf for converting netcdf wrf nmm restart file
  ! to temporary binary format
  
    call ext_ncd_ioinit(sysdepinfo,status)
    call set_wrf_debug_level ( 5 )
    
    n_loop: do n=1,9 ! loop over forecast hours in assim interval
  
       if(n==nhr_assimilation)then
          flnm1 = 'wrf_inout'
       else
          write(flnm1,'("wrf_inou",i1.1)')n
       endif
  
       call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
       if(n==nhr_assimilation)then
          if ( Status /= 0 )then
             write(6,*)'CONVERT_NETCDF_MASS:  problem with flnm1 = ',&
                  trim(flnm1),', Status = ', Status
             call stop2(74)
          endif
       else
          if ( Status /= 0 )then
             write(6,*)'CONVERT_NETCDF_MASS:  problem with flnm1 = ',&
                  trim(flnm1),', Status = ', Status
             cycle n_loop
          endif
       endif
  
    
       write(filename,'("sigf",i2.2)') n
       open(iunit,file=filename,form='unformatted')
  
       if(print_verbose)write(6,*)' convert ',trim(flnm1),' to ', trim(filename)
  
  !-------------  get date info
  
       call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
       read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
       write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond
  
  !    write(6,*)' dh1  = ',dh1         !DEDE
  
  !-------------  get grid info
       rmse_var='SMOIS'
  
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )                !DEDE
  
       if(print_verbose)then
          write(6,*)' dh1  = ',dh1         !DEDE
          write(6,*)'rmse_var = ',trim(rmse_var)
          write(6,*)'ndim1 = ',ndim1
          write(6,*)'ordering = ',trim(ordering)
          write(6,*)'staggering = ',trim(staggering)
          write(6,*)'start_index = ',start_index
          write(6,*)'end_index = ',end_index
          write(6,*)'WrfType = ',WrfType
          write(6,*)'ierr  = ',ierr   !DEDE
       end if
  
       nlon_regional=end_index(1)
       nlat_regional=end_index(2)
       nsig_soil_regional=end_index(3)
  
       rmse_var='T'
  
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )                !DEDE
  
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1 = ',ndim1,' dh1 = ',dh1
          write(6,*)' WrfType = ',WrfType,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
  
       nlon_regional=end_index(1)
       nlat_regional=end_index(2)
       nsig_regional=end_index(3)
       if(print_verbose)write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
       allocate(field2(nlon_regional,nlat_regional),field3(nlon_regional,nlat_regional,nsig_regional+1))
       allocate(field3u(nlon_regional+1,nlat_regional,nsig_regional))
       allocate(field3v(nlon_regional,nlat_regional+1,nsig_regional))
       allocate(field2b(nlon_regional,nlat_regional),field2c(nlon_regional,nlat_regional))
       allocate(ifield2(nlon_regional,nlat_regional))
       allocate(field1(max(nlon_regional,nlat_regional,nsig_regional)))
       allocate(field1a(max(nlon_regional,nlat_regional,nsig_regional)))
       if(w_exist) then
         allocate(field3w(nlon_regional,nlat_regional,nsig_regional+1))
       end if
    
       rmse_var='P_TOP'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            pt_regional,WRF_REAL,0,0,0,ordering,          &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' p_top=',pt_regional
    
       write(iunit) iyear,imonth,iday,ihour,iminute,isecond, &
            nlon_regional,nlat_regional,nsig_regional,pt_regional,nsig_soil_regional
       
       if(wrf_mass_hybridcord) then
          rmse_var='C3H'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index,                   & !dom
            start_index,end_index,                   & !mem
            start_index,end_index,                   & !pat
            ierr                                 )
          if(print_verbose)then
             do k=1,nsig_regional
                write(6,*)' k,c3h(k)=',k,field1(k)
             end do
          end if
          rmse_var='C4H'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1a,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index,                   & !dom
            start_index,end_index,                   & !mem
            start_index,end_index,                   & !pat
            ierr                                 )
          if(print_verbose)then
             do k=1,nsig_regional
                write(6,*)' k,c4h(k)=',k,field1a(k)
             end do
          end if
          write(iunit)field1(1:nsig_regional),field1a(1:nsig_regional)  ! c3h,c4h
       
          rmse_var='C3F'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index,                   & !dom
            start_index,end_index,                   & !mem
            start_index,end_index,                   & !pat
            ierr                                 )
          if(print_verbose)then
             do k=1,nsig_regional+1
                write(6,*)' k,c3f(k)=',k,field1(k)
             end do
          end if
          rmse_var='C4F'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1a,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index,                   & !dom
            start_index,end_index,                   & !mem
            start_index,end_index,                   & !pat
            ierr                                 )
          if(print_verbose)then
             do k=1,nsig_regional+1
                write(6,*)' k,c4f(k)=',k,field1a(k)
             end do
          end if
          write(iunit)field1(1:nsig_regional+1),field1a(1:nsig_regional+1)  ! c3f,c4f
       else
          rmse_var='ZNU'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field1,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,                    &
               start_index,end_index,                   & !dom
               start_index,end_index,                   & !mem
               start_index,end_index,                   & !pat
               ierr                                 )
          if(print_verbose)then
             do k=1,nsig_regional
                write(6,*)' k,znu(k)=',k,field1(k)
             end do
          end if
          field1a=0.0_r_single
          write(iunit)field1(1:nsig_regional),field1a(1:nsig_regional)  ! ZNU
    
          rmse_var='ZNW'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index,                   & !dom
            start_index,end_index,                   & !mem
            start_index,end_index,                   & !pat
            ierr                                 )
          if(print_verbose)then
             do k=1,nsig_regional+1
                write(6,*)' k,znw(k)=',k,field1(k)
             end do
          end if
          field1a=0.0_r_single
          write(iunit)field1(1:nsig_regional+1),field1a(1:nsig_regional+1)  ! ZNW
       endif
     
       rmse_var='RDX'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            rdx,WRF_REAL,0,0,0,ordering,          &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' 1/rdx=',one_single/rdx
     
       rmse_var='RDY'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            rdy,WRF_REAL,0,0,0,ordering,          &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' 1/rdy=',one_single/rdy
     
       rmse_var='MAPFAC_M'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)then
          write(6,*)' max,min mapfac_m=',maxval(field2),minval(field2)
          write(6,*)' max,min MAPFAC_M(:,1)=',maxval(field2(:,1)),minval(field2(:,1))
          write(6,*)' max,min MAPFAC_M(1,:)=',maxval(field2(1,:)),minval(field2(1,:))
          write(6,*)' mapfac_m(1,1),mapfac_m(nlon,1)=',field2(1,1),field2(nlon_regional,1)
          write(6,*)' mapfac_m(1,nlat),mapfac_m(nlon,nlat)=', &
            field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
       end if
       field2b=one_single/(field2*rdx)  !DX_MC
       field2c=one_single/(field2*rdy)  !DY_MC
     
       rad2deg_single=r45/atan(one_single)
       rmse_var='XLAT'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)then
          write(6,*)' max,min XLAT(:,1)=',maxval(field2(:,1)),minval(field2(:,1))
          write(6,*)' max,min XLAT(1,:)=',maxval(field2(1,:)),minval(field2(1,:))
          write(6,*)' xlat(1,1),xlat(nlon,1)=',field2(1,1),field2(nlon_regional,1)
          write(6,*)' xlat(1,nlat),xlat(nlon,nlat)=', &
            field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
       end if
       field2=field2/rad2deg_single
       write(iunit)field2,field2b   !XLAT,DX_MC
     
       rmse_var='XLONG'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)then
          write(6,*)' max,min XLONG(:,1)=',maxval(field2(:,1)),minval(field2(:,1))
          write(6,*)' max,min XLONG(1,:)=',maxval(field2(1,:)),minval(field2(1,:))
          write(6,*)' xlong(1,1),xlong(nlon,1)=',field2(1,1),field2(nlon_regional,1)
          write(6,*)' xlong(1,nlat),xlong(nlon,nlat)=', &
            field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
       end if
       field2=field2/rad2deg_single
       write(iunit)field2,field2c   !XLONG,DY_MC
     
       rmse_var='MUB'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' max,min MUB=',maxval(field2),minval(field2)
     
       rmse_var='MU'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2b,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose) &
          write(6,*)' max,min MU=',maxval(field2b),minval(field2b)
       field2=field2b+field2+pt_regional
       if(print_verbose) &
         write(6,*)' max,min psfc0=',maxval(field2),minval(field2)
       write(iunit)field2   ! psfc0
     
       rmse_var='PHB'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       k=1
       if(print_verbose) &
          write(6,*)' k,max,min,mid PHB=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
       write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! PHB (zsfc*g)
     
       rmse_var='T'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       field3=field3+h300
       do k=1,nsig_regional
          if(print_verbose) &
             write(6,*)' k,max,min,mid T=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
          write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! POT TEMP (sensible??)
       end do
     
       rmse_var='QVAPOR'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       do k=1,nsig_regional
          if(print_verbose) &
             write(6,*)' k,max,min,mid Q=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
          write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q
       end do
     
       rmse_var='U'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3u,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       do k=1,nsig_regional
          if(print_verbose) &
             write(6,*)' k,max,min,mid U=',k,maxval(field3u(:,:,k)),minval(field3u(:,:,k)), &
               field3u(nlon_regional/2,nlat_regional/2,k)
          write(iunit)((field3u(i,j,k),i=1,nlon_regional+1),j=1,nlat_regional)   ! U
       end do
     
       rmse_var='V'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3v,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       do k=1,nsig_regional
          if(print_verbose) &
             write(6,*)' k,max,min,mid V=',k,maxval(field3v(:,:,k)),minval(field3v(:,:,k)), &
               field3v(nlon_regional/2,nlat_regional/2,k)
          write(iunit)((field3v(i,j,k),i=1,nlon_regional),j=1,nlat_regional+1)   ! V
       end do

       if(w_exist) then
          rmse_var='W'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
            write(6,*)' rmse_var=',trim(rmse_var)
            write(6,*)' ordering=',ordering
            write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
            write(6,*)' ndim1=',ndim1
            write(6,*)' staggering=',staggering
            write(6,*)' start_index=',start_index
            write(6,*)' end_index=',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3w,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index,               & !dom
             start_index,end_index,               & !mem
             start_index,end_index,               & !pat
             ierr                                 )
          do k=1,nsig_regional+1
            if(print_verbose)then
              write(6,*)' k,max,min,mid W=',k,maxval(field3w(:,:,k)),minval(field3w(:,:,k)), &
                  field3w(nlon_regional/2,nlat_regional/2,k)
            end if
            write(iunit)((field3w(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! w
          end do
       end if

     
!      rmse_var='LANDMASK'
       rmse_var='XLAND'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)then
          write(6,*)' max,min sm=',maxval(field2),minval(field2)
          write(6,*)' landmask(1,1),landmask(nlon,1)=',field2(1,1),field2(nlon_regional,1)
          write(6,*)' landmask(1,nlat),landmask(nlon,nlat)=', &
            field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
       end if
       DO j=1,nlat_regional
       DO i=1,nlon_regional
          if(abs(field2(i,j)-2.0)<0.01) field2(i,j)=0.0  !for XLAND 2=water 1=land
       ENDDO
       ENDDO
       write(iunit)field2   !LANDMASK   (1=land, 0=water)
     
       rmse_var='SEAICE'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' max,min SEAICE=',maxval(field2),minval(field2)
       write(iunit)field2   !  SEAICE
     
       rmse_var='SST'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' max,min SST=',maxval(field2),minval(field2)
       write(iunit)field2   !SST
       
       rmse_var='IVGTYP'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            ifield2,WrfType,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' max,min IVGTYP=',maxval(ifield2),minval(ifield2)
       write(iunit)ifield2   !IVGTYP
     
       rmse_var='ISLTYP'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            ifield2,WrfType,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' max,min ISLTYP=',maxval(ifield2),minval(ifield2)
       write(iunit)ifield2   !ISLTYP
     
       rmse_var='VEGFRA'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' max,min VEGFRA=',maxval(field2),minval(field2)
       write(iunit)field2   !VEGFRA
       
       rmse_var='SNOW'    !
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' max,min SNOW=',maxval(field2),minval(field2)
       write(iunit)field2   !SNOW
     
       rmse_var='U10'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' max,min U10=',maxval(field2),minval(field2)
       write(iunit)field2   !U10
     
       rmse_var='V10'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' max,min V10=',maxval(field2),minval(field2)
       write(iunit)field2   !V10
     
       rmse_var='SMOIS'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       k=1
       if(l_gsd_soilTQ_nudge) then
          do k=1,nsig_soil_regional
             if(print_verbose) &
               write(6,*)' k,max,min,mid SMOIS=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                  field3(nlon_regional/2,nlat_regional/2,k)
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! SMOIS
          enddo
       else
          if(print_verbose) &
             write(6,*)' k,max,min,mid SMOIS=',k,maxval(field3(:,:,1)),minval(field3(:,:,1)), &
               field3(nlon_regional/2,nlat_regional/2,1)
          write(iunit)((field3(i,j,1),i=1,nlon_regional),j=1,nlat_regional)   ! SMOIS
       endif

       rmse_var='TSLB'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       k=1
       if(l_gsd_soilTQ_nudge) then
          do k=1,nsig_soil_regional
             if(print_verbose) &
                write(6,*)' k,max,min,mid TSLB=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                  field3(nlon_regional/2,nlat_regional/2,k)
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! TSLB
          enddo
       else
          if(print_verbose) &
             write(6,*)' k,max,min,mid TSLB=',k,maxval(field3(:,:,1)),minval(field3(:,:,1)), &
               field3(nlon_regional/2,nlat_regional/2,1)
          write(iunit)((field3(i,j,1),i=1,nlon_regional),j=1,nlat_regional)   ! TSLB
       endif
     
       rmse_var='TSK'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
          write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
          write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
          write(6,*)' start_index = ',start_index,' end_index = ',end_index
       end if
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index,               & !dom
            start_index,end_index,               & !mem
            start_index,end_index,               & !pat
            ierr                                 )
       if(print_verbose)write(6,*)' max,min TSK=',maxval(field2),minval(field2)
       write(iunit)field2   !TSK


       if(i_use_2mt4b >0 .or. i_use_2mq4b >0 ) then
          rmse_var='Q2'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index,               & !dom
               start_index,end_index,               & !mem
               start_index,end_index,               & !pat
               ierr                                 )
          if(print_verbose)write(6,*)' max,min Q2=',maxval(field2),minval(field2)
          write(iunit)field2   !Q2
       endif
   
       if(l_gsd_soilTQ_nudge) then
          rmse_var='SOILT1'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var=',trim(rmse_var)
             write(6,*)' ordering=',ordering
             write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
             write(6,*)' ndim1=',ndim1
             write(6,*)' staggering=',staggering
             write(6,*)' start_index=',start_index
             write(6,*)' end_index=',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index,               & !dom
               start_index,end_index,               & !mem
               start_index,end_index,               & !pat
               ierr                                 )
          if(print_verbose)write(6,*)' max,min SOILT1=',maxval(field2),minval(field2)
          write(iunit)field2   !SOILT1
       endif

       if(i_use_2mt4b >0) then
          rmse_var='TH2'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var=',trim(rmse_var)
             write(6,*)' ordering=',ordering
             write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
             write(6,*)' ndim1=',ndim1
             write(6,*)' staggering=',staggering
             write(6,*)' start_index=',start_index
             write(6,*)' end_index=',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index,               & !dom
               start_index,end_index,               & !mem
               start_index,end_index,               & !pat
               ierr                                 )
          if(print_verbose)write(6,*)' max,min TH2=',maxval(field2),minval(field2)
          write(iunit)field2   !TH2
       endif

       if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
          rmse_var='QCLOUD'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index,               & !dom
               start_index,end_index,               & !mem
               start_index,end_index,               & !pat
               ierr                                 )
          do k=1,nsig_regional
             if(print_verbose)write(6,*)' k,max,min,mid Qc=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &         
                      field3(nlon_regional/2,nlat_regional/2,k)
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Qc
          end do
    
          rmse_var='QRAIN'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index,               & !dom
               start_index,end_index,               & !mem
               start_index,end_index,               & !pat
               ierr                                 )
          do k=1,nsig_regional
             if(print_verbose)write(6,*)' k,max,min,mid Qr=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &         
                      field3(nlon_regional/2,nlat_regional/2,k)
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Qr
          end do
   
          rmse_var='QSNOW'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index,               & !dom
               start_index,end_index,               & !mem
               start_index,end_index,               & !pat
               ierr                                 )
          do k=1,nsig_regional
             if(print_verbose)write(6,*)' k,max,min,mid Qs=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                        field3(nlon_regional/2,nlat_regional/2,k)
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Qs
          end do
   
          rmse_var='QICE'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index,               & !dom
               start_index,end_index,               & !mem
               start_index,end_index,               & !pat
               ierr                                 )
          do k=1,nsig_regional
             if(print_verbose)write(6,*)' k,max,min,mid Qi=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                        field3(nlon_regional/2,nlat_regional/2,k)
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Qi
          end do
    
          rmse_var='QGRAUP'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index,               & !dom
               start_index,end_index,               & !mem
               start_index,end_index,               & !pat
               ierr                                 )
          do k=1,nsig_regional
             if(print_verbose) &
                write(6,*)' k,max,min,mid Qg=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                        field3(nlon_regional/2,nlat_regional/2,k)
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Qg
          end do
   
          rmse_var='QNRAIN'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering,    & 
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index,               & !dom
               start_index,end_index,               & !mem
               start_index,end_index,               & !pat
               ierr                                 )
          do k=1,nsig_regional
             if(print_verbose) &
                write(6,*)' k,max,min,mid Qnr=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                      field3(nlon_regional/2,nlat_regional/2,k)
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional) ! Qnr    
          end do

          if ( .not. l_use_dbz_directDA) then ! direct reflectivity DA does not use QNRAIN and QNICE
             rmse_var='QNICE'
             call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering,    & 
                  start_index,end_index, WrfType, ierr    )
             if(print_verbose)then
                write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
                write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr
                write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
                write(6,*)' start_index = ',start_index,' end_index = ',end_index
             end if
             call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
                  field3,WRF_REAL,0,0,0,ordering,           &
                  staggering, dimnames ,               &
                  start_index,end_index,               & !dom
                  start_index,end_index,               & !mem
                  start_index,end_index,               & !pat
                  ierr                                 )
             do k=1,nsig_regional
                if(print_verbose) &
                   write(6,*)' k,max,min,mid Qni=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                         field3(nlon_regional/2,nlat_regional/2,k)
                write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional) ! Qni    
             end do

             rmse_var='QNCLOUD'
             call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering,    & 
                  start_index,end_index, WrfType, ierr    )
             if(print_verbose)then
                write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
                write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr
                write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
                write(6,*)' start_index = ',start_index,' end_index = ',end_index
             end if
             call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
                  field3,WRF_REAL,0,0,0,ordering,           &
                  staggering, dimnames ,               &
                  start_index,end_index,               & !dom
                  start_index,end_index,               & !mem
                  start_index,end_index,               & !pat
                  ierr                                 )
             do k=1,nsig_regional
                if(print_verbose) &
                   write(6,*)' k,max,min,mid Qnc=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                         field3(nlon_regional/2,nlat_regional/2,k)
                write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional) ! Qnc
             end do
          end if

          if( dbz_exist .and. if_model_dbz ) then
             rmse_var='REFL_10CM'
             call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering,&
                  start_index,end_index, WrfType, ierr    )
             if(print_verbose)then
               write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
               write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr !DEDE
               write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
               write(6,*)' start_index = ',start_index,' end_index = ',end_index
             end if
             call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
                  field3,WRF_REAL,0,0,0,ordering,           &
                  staggering, dimnames ,               &
                  start_index,end_index,               & !dom
                  start_index,end_index,               & !mem
                  start_index,end_index,               & !pat
                  ierr                                 )
             do k=1,nsig_regional
! notes: the negative reflectivity is valid value in real observation.
               if(print_verbose)then
                  write(6,*)' k,max,min,mid Dbz=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                              field3(nlon_regional/2,nlat_regional/2,k)
               end if
               write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional) !dBZ
             end do
          end if

          rmse_var='RAD_TTEN_DFI'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
             write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr   !DEDE
             write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
             write(6,*)' start_index = ',start_index,' end_index = ',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index,               & !dom
               start_index,end_index,               & !mem
               start_index,end_index,               & !pat
               ierr                                 )
          do k=1,nsig_regional
             if(print_verbose) &
               write(6,*)' k,max,min,mid TTEN=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                      field3(nlon_regional/2,nlat_regional/2,k)
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! TTEN
          end do
   
       endif   ! l_hydrometeor_bkio

       if(laeroana_gocart) then
          call gsi_chemguess_get('aerosols::3d', n_gocart_var, ier)
          do iv = 1, n_gocart_var
             select case ( iv )
                case ( 1 )
                   rmse_var='sulf'
                case ( 2 )
                   rmse_var='BC1'
                case ( 3 )
                   rmse_var='BC2'
                case ( 4 )
                   rmse_var='OC1'
                case ( 5 )
                   rmse_var='OC2'
                case ( 6 )
                   rmse_var='DUST_1'
                case ( 7 )
                   rmse_var='DUST_2'
                case ( 8 )
                   rmse_var='DUST_3'
                case ( 9 )
                  rmse_var='DUST_4'
                case ( 10 )
                   rmse_var='DUST_5'
                case ( 11 )
                   rmse_var='SEAS_1'
                case ( 12 )
                   rmse_var='SEAS_2'
                case ( 13 )
                   rmse_var='SEAS_3'
                case ( 14 )
                   rmse_var='SEAS_4'
                case ( 15 )
                   rmse_var='P25'
             end select

             call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
                  start_index,end_index, WrfType, ierr    )
             if(print_verbose)then
                write(6,*)' rmse_var=',trim(rmse_var)
                write(6,*)' ordering=',ordering
                write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
                write(6,*)' ndim1=',ndim1
                write(6,*)' staggering=',staggering
                write(6,*)' start_index=',start_index
                write(6,*)' end_index=',end_index
             end if
             call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index,               & !dom
               start_index,end_index,               & !mem
               start_index,end_index,               & !pat
               ierr                                 )
             if ( trim(rmse_var) == 'sulf' ) then
                field3 = field3*ppmv_conv   ! ppmv to ug/kg
             end if
             do k=1,nsig_regional
                if(print_verbose) &
                write(6,*)' k,max,min,mid var=',rmse_var,k,     &
                       maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                      field3(nlon_regional/2,nlat_regional/2,k)
                write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)
             end do
          end do ! n_gocart_var loop
       endif ! laeroana_gocart
   
       if (wrf_pm2_5) then

          rmse_var='PM2_5_DRY'
          CALL ext_ncd_get_var_info (dh1,TRIM(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index, WrfType, ierr    )
          if(print_verbose)then
             write(6,*)' rmse_var=',TRIM(rmse_var)
             write(6,*)' ordering=',ordering
             write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
             write(6,*)' ndim1=',ndim1
             write(6,*)' staggering=',staggering
             write(6,*)' start_index=',start_index
             write(6,*)' end_index=',end_index
          end if
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index,               & !dom
               start_index,end_index,               & !mem
               start_index,end_index,               & !pat
               ierr                                 )

          do k=1,nsig_regional
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)
          end do

       endif


       deallocate(field1,field1a,field2,field2b,field2c,ifield2,field3,field3u,field3v)
       if(w_exist) deallocate(field3w)
       close(iunit)
       call ext_ncd_ioclose(dh1, Status)
   
    enddo n_loop
   
  end subroutine convert_netcdf_mass_wrf
  
  subroutine convert_netcdf_nmm_wrf(this,update_pint,ctph0,stph0,tlm0,guess)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    convert_netcdf_nmm    read wrf nmm netcdf restart
  !   prgmmr: parrish          org: np22                date: 2003-09-05
  !
  ! abstract: using wrf library routines, read a wrf nmm netcdf
  !             format restart file.  write the result to temporary netcdf
  !             file expected by read_wrf_nmm_guess.
  !
  ! program history log:
  !   2004-09-10  parrish
  !   2004-11-05  treadon - add return code 74 for error stop
  !   2004-12-15  treadon - remove get_lun, read guess from file "wrf_inout"
  !   2005-07-06  parrish - add read of pint byte address
  !   2005-10-17  parrish - add ctph0,stph0,tlm0
  !   2005-12-09  middlecoff - initialize character variable staggering
  !   2006-09-15  treadon - use nhr_assimilation to build local guess filename
  !   2011-11-14  tong - add loop to read upto 7 wrf nmm netcdf restart file and
  !                          write to temporary netcdf files (extend FGAT capability for
  !                          wrf nmm netcdf format)
  !   2012-01-13  zhu     - add cloud hydrometeors
  !   2012-10-11  eliu    - modify to add the use of use_gfs_stratosphere
  !   2013-02-15  parrish - change dimension of eta1_new,eta2_new from nsig_max to nsig_max+1.
  !   2014-10-28  tong    - modify to read wrf nmm ensemble forecast
  !
  !   input argument list:
  !     update_pint:   false on input
  !     guess:         true read guess otherwise read wrf nmm ensemble forecast  
  !
  !   output argument list:
  !     update_pint:   true on output if field pint (non-hydrostatic pressure in nmm model)
  !                     is available, in which case pint gets updated by analysis increment of pd,
  !                      the nmm hydrostatic pressure thickness variable.
  !     ctph0,stph0:   cos and sin thp0, earth lat of center of nmm grid (0 deg lat in rotated nmm coordinate)
  !                      (used by calctends routines)
  !     tlm0
  !
  !     NOTES:  this is beginning of allowing direct connection of gsi to wrf files
  !             without seperate external interface.  it is very inefficient, and
  !             later versions will be made to reduce the total i/o involved.
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
  !  create internal binary file from netcdf format wrf restart file
  
    use kinds, only: r_single,i_kind,r_kind
    use gsi_4dvar, only: nhr_assimilation
    use constants, only: half,rad2deg,zero
    use gsi_metguess_mod, only: gsi_metguess_get
    use gfs_stratosphere, only: mix_gfs_nmmb_vcoords,use_gfs_stratosphere,nsig_max,nsig_save
    use gridmod, only: half_grid,filled_grid,fill_nmm_grid2a3,half_nmm_grid2a
    use hybrid_ensemble_parameters, only: n_ens,merge_two_grid_ensperts
    use gsi_io, only: verbose
  
  ! use wrf_data
    implicit none
  ! include 'wrf_status_codes.h'
  ! include 'netcdf.inc'
  
    class(convert_netcdf_class), intent(inout) :: this
    logical     ,intent(in   ) :: guess
    logical     ,intent(inout) :: update_pint
    real(r_kind),intent(  out) :: ctph0,stph0,tlm0
  
    character(len=120) :: flnm1
    character(len=19)  :: DateStr1
    character(len=24)  :: filename
    integer(i_kind)            :: dh1
    
    integer(i_kind) :: iunit
    
    integer(i_kind) :: i,j,k
    integer(i_kind) :: ndim1
    integer(i_kind) :: WrfType
    integer(i_kind), dimension(4)  :: start_index, end_index1
    character (len= 4) :: staggering=' N/A'
    character (len= 3) :: ordering
  
    character (len=80), dimension(3)  ::  dimnames
    character (len=80) :: SysDepInfo
  
    integer(i_kind) :: n_actual_clouds, ierr, Status, Status_next_time, n
    integer(i_kind) :: nlp
  
  ! binary stuff
  
  ! rmse stuff
  
    character (len=31) :: rmse_var
    
    integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
    integer(i_kind) nlon_regional,nlat_regional,nsig_regional,nlon,nlat
    integer(i_kind) nsig_regional_new,nsig_read 
    real(r_single) pt_regional,pdtop_regional,dy_nmm
    real(r_single) dlmd_regional,dphd_regional
    real(r_single),allocatable::aeta1(:),deta1(:),eta1(:)               
    real(r_single),allocatable::aeta2(:),deta2(:),eta2(:)                
    real(r_single),allocatable::aeta1_new(:),deta1_new(:),eta1_new(:)    
    real(r_single),allocatable::aeta2_new(:),deta2_new(:),eta2_new(:)    
    real(r_single),allocatable::field3(:,:,:),field2(:,:),field1(:),field2b(:,:)
    integer(i_kind),allocatable::ifield2(:,:)
    real(r_single),allocatable:: glat(:,:),glon(:,:)
    real(r_kind),allocatable:: glat8(:,:),glon8(:,:)
    real(r_kind),allocatable::glat_an(:,:),glon_an(:,:)
    real(r_kind),allocatable::gxtemp(:,:),gytemp(:,:)
    real(r_kind),allocatable::gxtemp_an(:,:),gytemp_an(:,:)
    real(r_kind),allocatable::region_lat(:,:),region_lon(:,:)
    integer(i_kind) :: i0,j0
    integer(i_kind) wrf_real
    logical :: print_verbose
    data iunit / 15 /
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
    wrf_real=104
    end_index1=0
    print_verbose=.false.
    if(verbose)print_verbose=.true.
  
  ! Inquire about cloud guess fields
    call gsi_metguess_get('clouds::3d',n_actual_clouds,ierr)
  
  ! transfer code from diffwrf for converting netcdf wrf nmm restart file
  ! to temporary binary format
  
    call ext_ncd_ioinit(sysdepinfo,status)
    call set_wrf_debug_level ( 1 )
  
    if(guess) then
       nlp=9
    else
       if(.not. merge_two_grid_ensperts) then
          nlp=n_ens
       else
          nlp=2*n_ens
       endif
    endif
    
  
    n_loop: do n=1,nlp ! loop over forecast hours in assim interval
  
       if(guess)then
          if(n==nhr_assimilation)then
             flnm1 = 'wrf_inout'
          else
             write(flnm1,'("wrf_inou",i1.1)')n
          endif
          write(filename,'("sigf",i2.2)') n
       else
          if(.not. merge_two_grid_ensperts)then
             write(flnm1,'("wrf_en",i3.3)')n
             write(filename,'("sigf06_ens_mem",i3.3)')n
          else
             if(n <= n_ens)then
                write(flnm1,'("d01_en",i3.3)')n
                write(filename,'("sigf06_ens_mem",i3.3)')n
             else
                write(flnm1,'("d02_en",i3.3)')n-n_ens
                write(filename,'("sigf06_d02_ens_mem",i3.3)')n-n_ens
             endif
          endif
       end if
     
       call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
       if(guess)then
          if(n==nhr_assimilation)then
             if ( Status /= 0 )then
                write(6,*)'CONVERT_NETCDF_NMM:  problem with flnm1 = ',&
                     trim(flnm1),', Status = ', Status
                call stop2(74)
             endif
          else
             if ( Status /= 0 )then
                write(6,*)'CONVERT_NETCDF_NMM:  problem with flnm1 = ',&
                     trim(flnm1),', Status = ', Status
                cycle
             endif
          end if
       else
          if ( Status /= 0 )then
             write(6,*)'CONVERT_NETCDF_NMM:  problem with wrfens = ',&
                  trim(flnm1),', Status = ', Status
             call stop2(74)
          endif
       endif
    
       open(iunit,file=filename,form='unformatted')
       if(guess)write(6,*)'CONVERT_NETCDF_NMM: output file = ', filename  
  
  !-------------  get date info
  
       call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
       read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
       if(guess .and. print_verbose)write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond
       
  !-------------  get grid info
       rmse_var='T'
       call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       nlon_regional=end_index1(1)
       nlat_regional=end_index1(2)
       nsig_regional=end_index1(3)
  !    these will hold original vertical structure for regional
       allocate(deta1(nsig_regional),aeta1(nsig_regional),eta1(nsig_regional+1))
       allocate(deta2(nsig_regional),aeta2(nsig_regional),eta2(nsig_regional+1))
       write(6,*)'CONVERT_NETCDF_NMM: nlon_regional,nlat_regional,nsig_regional,nsig_max=', &
                  nlon_regional,nlat_regional,nsig_regional,nsig_max
       allocate(field2(nlon_regional,nlat_regional),field3(nlon_regional,nlat_regional,nsig_regional+1))   
       allocate(field2b(nlon_regional,nlat_regional),ifield2(nlon_regional,nlat_regional))
       allocate(field1(max(nlon_regional,nlat_regional,nsig_regional+1)))       
  
       rmse_var='SMC' ! soil moisture volume fraction
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
  
  !    Read in east-west angular distance (degrees) H-to-V points 
       rmse_var='DLMD'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
  
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            dlmd_regional,WRF_REAL,0,0,0,ordering,          &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
       if(guess .and. print_verbose)write(6,*)' dlmd=',dlmd_regional
       
       rmse_var='DPHD' ! north-south angular distance (degrees) H-to-V points                      
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            dphd_regional,WRF_REAL,0,0,0,ordering,          &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
       if(guess .and. print_verbose)write(6,*)' dphd=',dphd_regional
    
       rmse_var='PT' ! pressure (Pa) at top of domain
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            pt_regional,WRF_REAL,0,0,0,ordering,          &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
       if(guess .and. print_verbose)write(6,*)' pt=',pt_regional
  
       rmse_var='PDTOP' ! mass (pa) at model top in pressure domain
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            pdtop_regional,WRF_REAL,0,0,0,ordering,       &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
       if(guess .and. print_verbose)write(6,*)' pdtop=',pdtop_regional
       rmse_var='DETA1' ! delta sigma in pressure domain
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index1,                   & !dom
            start_index,end_index1,                   & !mem
            start_index,end_index1,                   & !pat
            ierr                                 )
       do k=1,nsig_regional
          deta1(k)=field1(k)    
       end do
       if(guess .and. print_verbose .and. n==nhr_assimilation)then
          write(6,*)'CONVERT_NETCDF_NMM:' 
          do k=1,nsig_regional
             write(6,*)' k,deta1(k)=',k,field1(k)
          end do
       end if
  !    write(iunit)field1(1:nsig_regional)  ! DETA1    
       
  !    Read in midlayer sigma value in pressure domain  
       rmse_var='AETA1'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index1,                   & !dom
            start_index,end_index1,                   & !mem
            start_index,end_index1,                   & !pat
            ierr                                 )
       do k=1,nsig_regional
          aeta1(k)=field1(k)
       end do
       if(guess .and. print_verbose .and. n==nhr_assimilation)then
          write(6,*)'CONVERT_NETCDF_NMM:' 
          do k=1,nsig_regional
             write(6,*)' k,aeta1(k)=',k,field1(k)
          end do
       end if
  !    write(iunit)field1(1:nsig_regional)  ! AETA1
    
       rmse_var='ETA1' ! interface sigma value in pressure domain 
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index1,                   & !dom
            start_index,end_index1,                   & !mem
            start_index,end_index1,                   & !pat
            ierr                                 )
       do k=1,nsig_regional+1
         eta1(k)=field1(k)  
       end do
       if(guess .and. print_verbose .and. n==nhr_assimilation) then
          write(6,*)'CONVERT_NETCDF_NMM:' 
          do k=1,nsig_regional+1
             write(6,*)' k, eta1(k)=',k,field1(k)
          end do
       end if
  !    write(iunit)field1(1:nsig_regional+1)  !  ETA1
    
       rmse_var='DETA2' ! delta sigma in pressure domain 
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index1,                   & !dom
            start_index,end_index1,                   & !mem
            start_index,end_index1,                   & !pat
            ierr                                 )
       do k=1,nsig_regional
          deta2(k)=field1(k) 
       end do
       if(guess .and. print_verbose .and. n==nhr_assimilation) then
          write(6,*)'CONVERT_NETCDF_NMM:' 
          do k=1,nsig_regional
             write(6,*)' k,deta2(k)=',k,field1(k)
          end do
       end if
  !    write(iunit)field1(1:nsig_regional)  ! DETA2 
       
       rmse_var='AETA2' ! midlayer sigma value in pressure domain 
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index1,                   & !dom
            start_index,end_index1,                   & !mem
            start_index,end_index1,                   & !pat
            ierr                                 )
       do k=1,nsig_regional
          aeta2(k)=field1(k) 
       end do
       if(guess .and. print_verbose .and. n==nhr_assimilation)then
          write(6,*)'CONVERT_NETCDF_NMM:' 
          do k=1,nsig_regional
             write(6,*)' k,aeta2(k)=',k,field1(k)
          end do
       end if
  !    write(iunit)field1(1:nsig_regional)  ! AETA2 
       
       rmse_var='ETA2' ! interface sigma value in pressure domain 
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index1,                   & !dom
            start_index,end_index1,                   & !mem
            start_index,end_index1,                   & !pat
            ierr                                 )
       do k=1,nsig_regional+1
          eta2(k)=field1(k) 
       end do
       if(guess .and. print_verbose .and. n==nhr_assimilation)then
          write(6,*)'CONVERT_NETCDF_NMM:' 
          do k=1,nsig_regional+1
             write(6,*)' k,eta2(k)=',k,field1(k)
          end do
       end if
  !    write(iunit)field1(1:nsig_regional+1)  ! ETA2
  !    Create new vertical coordinate structure if use_gfs_stratosphere is true 
  !    Blend and extend vertical coordinate with GFS 
  !    This will create updated values for: deta1, aeat1, eta1 
  !                                         deta2, aeat2, eta2
  !                                         nsig_regional 
       nsig_read=nsig_regional  ! hold the original nsig_regional
       if(use_gfs_stratosphere) then  ! get new vertical coordinate           
          allocate(deta1_new(nsig_max),aeta1_new(nsig_max),eta1_new(nsig_max+1))
          allocate(deta2_new(nsig_max),aeta2_new(nsig_max),eta2_new(nsig_max+1))
          call mix_gfs_nmmb_vcoords(deta1,aeta1,eta1,deta2,aeta2,eta2, &
                                    pdtop_regional,pt_regional,nsig_regional, &
                                    deta1_new,aeta1_new,eta1_new,deta2_new,aeta2_new,eta2_new,nsig_regional_new)
          nsig_read=nsig_save
          write(6,*)' in convert_netcdf_nmm, compute new vertical coordinate which is merged with gfs'                       
          write(6,*)' previous nsig_regional=',nsig_regional
          nsig_regional=nsig_regional_new    ! new nsig
          write(6,*)'      new nsig_regional=',nsig_regional
          write(6,*)'              nsig_read=',nsig_read  
          deallocate(deta1,aeta1,eta1)
          deallocate(deta2,aeta2,eta2)
          allocate(deta1(nsig_regional),aeta1(nsig_regional),eta1(nsig_regional+1))
          allocate(deta2(nsig_regional),aeta2(nsig_regional),eta2(nsig_regional+1))
          do k=1,nsig_regional
             deta1(k)=deta1_new(k)
             aeta1(k)=aeta1_new(k)
             deta2(k)=deta2_new(k)
             aeta2(k)=aeta2_new(k)
          end do
          do k=1,nsig_regional+1
             eta1(k)=eta1_new(k)
             eta2(k)=eta2_new(k)
          end do
          deallocate(deta1_new,aeta1_new,eta1_new)
          deallocate(deta2_new,aeta2_new,eta2_new)
       end if ! use_gfs_stratosphere
  !    write out header and new vertical coordinate structure
       if(guess)then
          write(iunit) iyear,imonth,iday,ihour,iminute,isecond, &
               nlon_regional,nlat_regional,nsig_regional, &
               dlmd_regional,dphd_regional,pt_regional,pdtop_regional
       else
          write(iunit) nlon_regional,nlat_regional,nsig_regional, &
               dlmd_regional,dphd_regional,pt_regional,pdtop_regional
       end if      
       if(guess)write(iunit)deta1(1:nsig_regional)    ! DETA1
       write(iunit)aeta1(1:nsig_regional)    ! AETA1
       if(guess)write(iunit) eta1(1:nsig_regional+1)  !  ETA1
       if(guess)write(iunit)deta2(1:nsig_regional)    ! DETA2
       write(iunit)aeta2(1:nsig_regional)    ! AETA2
       if(guess)write(iunit) eta2(1:nsig_regional+1)  !  ETA2
       deallocate(deta1,aeta1,eta1,deta2,aeta2,eta2)
       rmse_var='GLAT' ! geographic latitude (radians)
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
       if(guess .and. print_verbose)then
          write(6,*)' max,min GLAT=',rad2deg*maxval(field2),rad2deg*minval(field2)
          write(6,*)' glat(1,1),glat(nlon,1)=',rad2deg*field2(1,1),rad2deg*field2(nlon_regional,1)
          write(6,*)' glat(1,nlat),glat(nlon,nlat)=', &
            rad2deg*field2(1,nlat_regional),rad2deg*field2(nlon_regional,nlat_regional)
          write(6,*)' my guess at tph0d = ',rad2deg*field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)
       end if
       ctph0=cos(real(field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2),r_kind))
       stph0=sin(real(field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2),r_kind))
       
       if(guess)then
          rmse_var='DX_NMM' ! east-west distance (m) H-to-V points 
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2b,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(print_verbose)then
             write(6,*)' max,min DX_NMM=',maxval(field2b),minval(field2b)
             write(6,*)' dx_nmm(1,1),dx_nmm(nlon,1)=',field2b(1,1),field2b(nlon_regional,1)
             write(6,*)' dx_nmm(1,nlat),dx_nmm(nlon,nlat)=', &
               field2b(1,nlat_regional),field2b(nlon_regional,nlat_regional)
          end if
       end if
       if(guess)then
          write(iunit)field2,field2b   !GLAT,DX_NMM
       else
          allocate(glat(nlon_regional,nlat_regional))
          glat=field2
       end if
       
       rmse_var='GLON' ! geographic longitude (radians)
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
       if(guess .and. print_verbose)then
          write(6,*)' max,min GLON=',rad2deg*maxval(field2),rad2deg*minval(field2)
          write(6,*)' glon(1,1),glon(nlon,1)=',rad2deg*field2(1,1),rad2deg*field2(nlon_regional,1)
          write(6,*)' glon(1,nlat),glon(nlon,nlat)=', &
            rad2deg*field2(1,nlat_regional),rad2deg*field2(nlon_regional,nlat_regional)
          write(6,*)' my guess at tlm0d = ',rad2deg*field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)
       end if
       tlm0=half*real((field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)+ &
                field2(2+(nlon_regional-1)/2,1+(nlat_regional-1)/2)),r_kind)
  
       if(guess)then
          rmse_var='DY_NMM' ! north-south distance (m) H-to-V points
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               dy_nmm,WRF_REAL,0,0,0,ordering,       &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(print_verbose)write(6,*)' dy_nmm=',dy_nmm
          field2b=dy_nmm
       end if
       if(guess)then
          write(iunit)field2,field2b   !GLON,DY_NMM
       else
          allocate(glon(nlon_regional,nlat_regional))
          glon=field2        !GLON
       end if
  
       if(.not. guess)then
          if(filled_grid) then
             nlon=2*nlon_regional-1
             nlat=nlat_regional
          end if
          if(half_grid) then
             nlon=nlon_regional
             nlat=1+nlat_regional/2
          end if
  
          allocate(glat_an(nlon,nlat),glon_an(nlon,nlat))
          allocate(region_lat(nlat,nlon),region_lon(nlat,nlon))
          if(half_grid) then
             call half_nmm_grid2a(glon,nlon_regional,nlat_regional,glon_an,1)
             call half_nmm_grid2a(glat,nlon_regional,nlat_regional,glat_an,1)
          end if
  
          if(filled_grid) then
             allocate(gxtemp(nlon_regional,nlat_regional))
             allocate(gytemp(nlon_regional,nlat_regional))
             allocate(glon8(nlon_regional,nlat_regional))
             allocate(glat8(nlon_regional,nlat_regional))
             glon8=glon
             glat8=glat
             i0=nlon_regional/2
             j0=nlat_regional/2
             call ll2rpolar(glat8,glon8,nlon_regional*nlat_regional, &
                            gxtemp,gytemp,glat8(i0,j0),glon8(i0,j0),zero)
             allocate(gxtemp_an(nlon,nlat))
             allocate(gytemp_an(nlon,nlat))
             call fill_nmm_grid2a3(gxtemp,nlon_regional,nlat_regional,gxtemp_an)
             call fill_nmm_grid2a3(gytemp,nlon_regional,nlat_regional,gytemp_an)
             call rpolar2ll(gxtemp_an,gytemp_an,nlon*nlat, &
                            glat_an,glon_an,glat8(i0,j0),glon8(i0,j0),zero)
             deallocate(gxtemp,gytemp,gxtemp_an,gytemp_an,glon8,glat8)
          end if
  
          do k=1,nlon
             do i=1,nlat
                region_lat(i,k)=glat_an(k,i)
                region_lon(i,k)=glon_an(k,i)
             end do
          end do
          write(iunit)region_lat
          write(iunit)region_lon
          deallocate(glat,glon,glat_an,glon_an,region_lat,region_lon)
       end if
  
       rmse_var='PD' ! mass (Pa) at grid point (I,J) in sigma domain 
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
       if(guess .and. print_verbose)write(6,*)' max,min pd=',maxval(field2),minval(field2)
       write(iunit)field2   !PD
       
       if(guess)then
          rmse_var='FIS' ! surface geopotential (m2s-2)  
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
               ierr                                 )
          if(print_verbose)write(6,*)' max,min FIS=',maxval(field2),minval(field2)
          write(iunit)field2   ! FIS
     
          update_pint=.false. ! model layer interface pressure (Pa)
          rmse_var='PINT'
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(ierr==0) then
            update_pint=.true.
            if(print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
            call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
                 field3,WRF_REAL,0,0,0,ordering,           &
                 staggering, dimnames ,               &
                 start_index,end_index1,               & !dom
                 start_index,end_index1,               & !mem
                 start_index,end_index1,               & !pat
                 ierr                                 )
            do k=1,nsig_read+1           
               if(print_verbose)write(6,*)' k,max,min,mid PINT=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                    field3(nlon_regional/2,nlat_regional/2,k)
               write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! PINT
            end do
            if(print_verbose)then
               do k=1,nsig_read+1
                  write(6,'(i6,a6,3(2x,f15.6))') &
                        k,'PINT',field3(1,1,k), &
                              field3(nlon_regional/2,nlat_regional/2,k), &
                              field3(nlon_regional,nlat_regional,k)
               enddo
            end if
          end if
       end if
       rmse_var='T' ! sensible temperature (K)  
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' ierr,rmse_var=',ierr,trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
       do k=1,nsig_read     
          if(guess .and. print_verbose)write(6,*)' k,max,min,mid T=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
          write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! T
       end do
       if(guess)then
          do k=1,nsig_read
              if(print_verbose)write(6,'(i6,a6,3(2x,f15.6))') &
                    k,'T',field3(1,1,k), &
                          field3(nlon_regional/2,nlat_regional/2,k), &
                          field3(nlon_regional,nlat_regional,k)
          enddo
       end if
       rmse_var='Q'    ! Read in specific humidity (Kg/Kg)
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
       if(guess .and. print_verbose)then
          do k=1,nsig_read
             write(6,*)' k,max,min,mid Q=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
          end do
       end if
       do k=1,nsig_read       
          write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q
       end do
       if(guess .and. print_verbose)then
          do k=1,nsig_read
              write(6,'(i6,a6,3(2x,f15.12))') &
                     k,'Q',field3(1,1,k), &
                           field3(nlon_regional/2,nlat_regional/2,k), &
                           field3(nlon_regional,nlat_regional,k)
          enddo
       end if
       rmse_var='U' ! U component of wind (m/s)
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
       do k=1,nsig_read     
          write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! U
       end do
       if(guess .and. print_verbose)then
          do k=1,nsig_read     
             write(6,*)' k,max,min,mid U=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
          end do
          do k=1,nsig_read 
              write(6,'(i6,a6,3(2x,f15.6))') &
                    k,'U',field3(1,1,k), &
                          field3(nlon_regional/2,nlat_regional/2,k), &
                          field3(nlon_regional,nlat_regional,k)
          enddo
       end if
       rmse_var='V' ! V component of wind (m/s)
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
       do k=1,nsig_read      
          write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! V
       end do
       if(guess .and. print_verbose)then
          do k=1,nsig_read      
             write(6,*)' k,max,min,mid V=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
          end do
          do k=1,nsig_read
              write(6,'(i6,a6,3(2x,f15.6))') &
                    k,'V',field3(1,1,k), &
                          field3(nlon_regional/2,nlat_regional/2,k), &
                          field3(nlon_regional,nlat_regional,k)
          enddo
       end if
  
       if(guess)then
          rmse_var='SM' ! land-sea mask (sea=1 land=0) 
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(print_verbose)then
             write(6,*)' max,min sm=',maxval(field2),minval(field2)
             write(6,*)' sm(1,1),sm(nlon,1)=',field2(1,1),field2(nlon_regional,1)
             write(6,*)' sm(1,nlat),sm(nlon,nlat)=', &
                  field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
          end if
          write(iunit)field2   !SM
          
          rmse_var='SICE' ! sea ice mask (1=sea ice 0=no sea ice) 
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(print_verbose)write(6,*)' max,min SICE=',maxval(field2),minval(field2)
          write(iunit)field2   !SICE
          
          rmse_var='SST' ! sea surface temperature (K)
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(guess .and. print_verbose)write(6,*)' max,min SST=',maxval(field2),minval(field2)
          write(iunit)field2   !SST
          
          rmse_var='IVGTYP' ! vegetation type
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               ifield2,WrfType,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(guess .and. print_verbose)write(6,*)' max,min IVGTYP=',maxval(ifield2),minval(ifield2)
          write(iunit)ifield2   !IVGTYP
   
          rmse_var='ISLTYP' ! soil type 
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               ifield2,WrfType,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(guess .and. print_verbose)write(6,*)' max,min ISLTYP=',maxval(ifield2),minval(ifield2)
          write(iunit)ifield2   !ISLTYP
          
          rmse_var='VEGFRC' ! vegetation fraction 
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(guess .and. print_verbose)write(6,*)' max,min VEGFRC=',maxval(field2),minval(field2)
          write(iunit)field2   !VEGFRC
       
          rmse_var='SNO' ! liquid water equivalent of snow on ground (kg/m2) 
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(guess .and. print_verbose)write(6,*)' max,min SNO=',maxval(field2),minval(field2)
          write(iunit)field2   !SNO
          
          rmse_var='U10' ! U at 10 meter (m/s)
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(guess .and. print_verbose)write(6,*)' max,min U10=',maxval(field2),minval(field2)
          write(iunit)field2   !U10
          
          rmse_var='V10' ! V at 10 meter (m/s) 
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(guess .and. print_verbose)write(6,*)' max,min V10=',maxval(field2),minval(field2)
          write(iunit)field2   !V10
          
          rmse_var='SMC' ! soil moisture volume fraction 
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          k=1
          if(guess .and. print_verbose)write(6,*)' k,max,min,mid SMC=',k,maxval(field3(:,:,1)),minval(field3(:,:,1)), &
               field3(nlon_regional/2,nlat_regional/2,1)
          write(iunit)((field3(i,j,1),i=1,nlon_regional),j=1,nlat_regional)   ! SMC
          
          rmse_var='STC' ! soil temperature 
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          k=1
          if(guess .and. print_verbose)write(6,*)' k,max,min,mid STC=',k,maxval(field3(:,:,1)),minval(field3(:,:,1)), &
               field3(nlon_regional/2,nlat_regional/2,1)
          write(iunit)((field3(i,j,1),i=1,nlon_regional),j=1,nlat_regional)   ! STC
          
          rmse_var='TSK' ! skin temperature (K)
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' rmse_var=',trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field2,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(guess .and. print_verbose)write(6,*)' max,min TSK=',maxval(field2),minval(field2)
          write(iunit)field2   !TSK
       end if
     
       if (n_actual_clouds>0) then! Read in cloud related fields
          rmse_var='CWM' ! cloud water mixing ratio    
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' ierr,rmse_var=',ierr,trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(guess .and. print_verbose)then
             do k=1,nsig_read
                write(6,*)' k,max,min,mid CWM=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                  field3(nlon_regional/2,nlat_regional/2,k)
             end do
          end if
          do k=1,nsig_read
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! CWM
          end do
  
          rmse_var='F_ICE' ! fraction of ice cloud in grid box
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' ierr,rmse_var=',ierr,trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          do k=1,nsig_read
             if(guess .and. print_verbose)write(6,*)' k,max,min,mid F_ICE=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                  field3(nlon_regional/2,nlat_regional/2,k)
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! F_ICE
          end do
  
          rmse_var='F_RAIN' ! fraction of rain in grid box 
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' ierr,rmse_var=',ierr,trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(print_verbose .and. guess)then
             do k=1,nsig_read
                write(6,*)' k,max,min,mid F_RAIN=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                  field3(nlon_regional/2,nlat_regional/2,k)
             end do
          end if
          do k=1,nsig_read
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! F_RAIN
          end do
  
          rmse_var='F_RIMEF' ! ? 
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(guess .and. print_verbose)write(6,*)' ierr,rmse_var=',ierr,trim(rmse_var)
          call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
          if(guess .and. print_verbose)then
             do k=1,nsig_read
                write(6,*)' k,max,min,mid F_RIMEF=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                     field3(nlon_regional/2,nlat_regional/2,k)
             end do
          end if
          do k=1,nsig_read
             write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! F_RIMEF
          end do
       end if ! end of n_actual_clouds>0
       
       deallocate(field1,field2,field2b,ifield2,field3)
       close(iunit)
       call ext_ncd_ioclose(dh1, Status)
  
    enddo n_loop
  
  end subroutine convert_netcdf_nmm_wrf
  
  subroutine update_netcdf_mass_wrf(this)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    update_netcdf_mass  create netcdf format wrf restart file from internal binary file.
  !   prgmmr:
  !
  ! abstract: create netcdf format wrf restart file from internal binary file
  !
  ! program history log:
  !   2004-11-05  treadon - add return code 75 for error stop
  !   2004-12-15  treadon - remove get_lun, read guess from file "wrf_inout"
  !   2005-12-09  middlecoff - initialize character variable staggering and removed staggering1,staggering2
  !   2006-04-06  middlecoff - added read of SM and SICE to match the writes in wrwrfmass.F90  
  !                            and read in the rest of the fields to match the writes in wrwrfmass.F90  
  !   2006-06-09  liu - bug fix: replace SM and SICE with SMOIS and XICE
  !   2008-03-29  Hu  - bug fix: replace XICE with SEAICE and 
  !                              comment out update for SMOIS (the actually 
  !                              variable is Landmask there).
  !   2009-08-14  lueken - update documentation
  !   2010-03-29  Hu  - add code to update 5 cloud/hydrometeor variables for cloud analysis
  !   2010-10-20  hclin   - added 15 wrfchem/gocart fields for aod 
  !   2012-01-09  Hu  - add code to update START_TIME to analysis time
  !   2012-04-13  Whitaker - clip positive definite quantities to tiny_single
  !   2014-03-12  hu     - add code to read ges_q2 (2m Q), 
  !                               Qnr(rain number concentration), 
  !                               and nsoil (number of soil levels)
  !   2015-01-13  ladwig - add code for Qni and Qnc (cloud ice and water number
  !                               concentration)
  !   2020-09-13 CAPS(C. Liu, L. Chen, and H. Li) 
  !                            - add code for hydrometer variables needed for direct reflectivity DA
  !                            - add CV transform option on hydrometer variables for direct reflectivity DA
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:
  !
  !$$$ end documentation block
  
    use netcdf, only: nf90_open,nf90_close,nf90_put_att
    use netcdf, only: nf90_write,nf90_global
    use kinds, only: r_single,i_kind,r_kind
    use constants, only: h300,tiny_single
    use rapidrefresh_cldsurf_mod, only: l_hydrometeor_bkio,l_gsd_soilTQ_nudge
    use rapidrefresh_cldsurf_mod, only: i_gsdcldanal_type
    use gsi_metguess_mod, only: gsi_metguess_get,GSI_MetGuess_Bundle
    use rapidrefresh_cldsurf_mod, only: i_use_2mt4b,i_use_2mq4b
    use gsi_bundlemod, only: GSI_BundleGetPointer
    use guess_grids, only: ntguessig
    use chemmod, only: laeroana_gocart, ppmv_conv,wrf_pm2_5
    use gsi_chemguess_mod, only: gsi_chemguess_get
    use netcdf_mod, only: nc_check
    use wrf_vars_mod, only : w_exist, dbz_exist
    use obsmod, only   : if_model_dbz
    use gsi_io, only: verbose
    use directDA_radaruse_mod, only: l_use_dbz_directDA
  
    implicit none
    class(convert_netcdf_class), intent(inout) :: this
  
    include 'netcdf.inc'
  
  ! Declare local parameters
  
    character(len=120) :: flnm1,flnm2
    character(len=19)  :: DateStr1
    integer(i_kind)            :: dh1,iw3jdn
  
    integer(i_kind) :: iunit
    integer(i_kind) :: iv, n_gocart_var, ier
  
    integer(i_kind) :: i,j,k
    integer(i_kind) :: ndim1
    integer(i_kind) :: WrfType
    integer(i_kind), dimension(4)  :: start_index, end_index1
    character (len= 4) :: staggering=' N/A'
    character (len= 3) :: ordering
  
    character (len=80), dimension(3)  ::  dimnames
    character (len=80) :: SysDepInfo
    character(len=24),parameter :: myname_ = 'update_netcdf_mass'
  
  
    integer(i_kind) :: it, n_actual_clouds, ierr, istatus, Status, Status_next_time
    real(r_kind), pointer :: ges_qc(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qi(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qr(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qs(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qg(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qnr(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qni(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qnc(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dbz(:,:,:)=>NULL()
  ! binary stuff
    logical print_verbose
  
  ! rmse stuff
  
    character (len=31) :: rmse_var
  
    integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
    integer(i_kind) nlon_regional,nlat_regional,nsig_regional,nsig_soil_regional
    real(r_single) pt_regional
    real(r_single),allocatable::field3(:,:,:),field2(:,:),field1(:),field2b(:,:)
    real(r_single),allocatable::field3u(:,:,:),field3v(:,:,:),field3w(:,:,:)
    integer(i_kind),allocatable::ifield2(:,:)
    integer(i_kind) wrf_real
    data iunit / 15 /
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate


    print_verbose=.false.
    if(verbose) print_verbose=.true.
  
  ! binary stuff

    wrf_real=104
    end_index1=0
  
  ! Inquire about guess fields
    call gsi_metguess_get('clouds::3d',n_actual_clouds,ierr)
    if (n_actual_clouds>0) then
  !    get pointer to relevant instance of cloud-related backgroud
       it=ntguessig
       ierr=0
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql', ges_qc, istatus );ierr=ierr+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi', ges_qi, istatus );ierr=ierr+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr', ges_qr, istatus );ierr=ierr+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs', ges_qs, istatus );ierr=ierr+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg', ges_qg, istatus );ierr=ierr+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr,istatus );ierr=ierr+istatus
       if ( .not. l_use_dbz_directDA) then   ! direct reflectivity DA does not update these two variables for now
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qni',ges_qni,istatus );ierr=ierr+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnc',ges_qnc,istatus );ierr=ierr+istatus
       end if
       if(dbz_exist) call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it),'dbz',ges_dbz,istatus );ierr=ierr+istatus
       if (ierr/=0) n_actual_clouds=0
    end if
  
  ! transfer code from diffwrf for converting netcdf wrf nmm restart file
  ! to temporary binary format
    if( i_gsdcldanal_type==6 .or. i_gsdcldanal_type==3 .or. i_gsdcldanal_type==7) call ext_ncd_ioinit(sysdepinfo,status)
  !
  !           update mass core netcdf file with analysis variables from 3dvar
  !
    flnm1='wrf_inout'
    call ext_ncd_open_for_update( trim(flnm1), 0, 0, "", dh1, Status)
    if ( Status /= 0 )then
       write(6,*)'UPDATE_NETCDF_MASS:  problem with flnm1 = ',&
            trim(flnm1),', Status = ', Status
       call stop2(75)
    endif
          
    
    close(51)
    flnm2='siganl'
    open(iunit,file=flnm2,form='unformatted')
  
       
  !-------------  get date info
  
    call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
    read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
    write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond
  
  !-------------  get grid info
    rmse_var='SMOIS'
    call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
                                 start_index,end_index1, WrfType, ierr    )
    nlon_regional=end_index1(1)
    nlat_regional=end_index1(2)
    nsig_soil_regional=end_index1(3)
  
    rmse_var='T'
    call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
                                 start_index,end_index1, WrfType, ierr    )
    nlon_regional=end_index1(1)
    nlat_regional=end_index1(2)
    nsig_regional=end_index1(3)
    if(print_verbose)write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
    allocate(field2(nlon_regional,nlat_regional),field3(nlon_regional,nlat_regional,nsig_regional))
    allocate(field3u(nlon_regional+1,nlat_regional,nsig_regional))
    allocate(field3v(nlon_regional,nlat_regional+1,nsig_regional))
    allocate(field3w(nlon_regional,nlat_regional,nsig_regional+1))
    allocate(field2b(nlon_regional,nlat_regional))
    allocate(ifield2(nlon_regional,nlat_regional))
    allocate(field1(max(nlon_regional,nlat_regional,nsig_regional)))
    
    rmse_var='P_TOP'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
         pt_regional,WRF_REAL,0,0,0,ordering,          &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    if(print_verbose)write(6,*)' p_top=',pt_regional
    read(iunit) ! iyear,imonth,iday,ihour,iminute,isecond, &
  !        nlon_regional,nlat_regional,nsig_regional,pt_regional
    
    read(iunit) ! field1(1:nsig_regional)  ! AETA1  (ZNU)
    
    read(iunit) ! field1(1:nsig_regional+1)  !  ETA1 (ZNW)
    
    read(iunit) ! field2   !XLAT,DX_MC
    
    read(iunit) ! field2   !XLONG,DY_MC
    
    rmse_var='MUB'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
         field2,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    if(print_verbose)write(6,*)' max,min MUB=',maxval(field2),minval(field2)
    
    read(iunit)   field2b   !psfc
    if(print_verbose)write(6,*)' max,min psfc=',maxval(field2b),minval(field2b)
    field2b=field2b-field2-pt_regional
    if(print_verbose)write(6,*)' max,min MU=',maxval(field2b),minval(field2b)
    rmse_var='MU'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field2b,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    
    read(iunit) ! field2   ! PHB (FIS)
    
    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! T
       if(print_verbose) &
          write(6,*)' k,max,min,mid T=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    field3=field3-h300
    rmse_var='T'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    
    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q
       if(print_verbose) &
          write(6,*)' k,max,min,mid Q=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='QVAPOR'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    where (field3 < tiny_single) field3 = tiny_single
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    
    do k=1,nsig_regional
       read(iunit)((field3u(i,j,k),i=1,nlon_regional+1),j=1,nlat_regional)   ! U
       if(print_verbose) &
          write(6,*)' k,max,min,mid U=',k,maxval(field3u(:,:,k)),minval(field3u(:,:,k)), &
            field3u(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='U'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3u,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    
    do k=1,nsig_regional
       read(iunit)((field3v(i,j,k),i=1,nlon_regional),j=1,nlat_regional+1)   ! V
       if(print_verbose) &
          write(6,*)' k,max,min,mid V=',k,maxval(field3v(:,:,k)),minval(field3v(:,:,k)), &
            field3v(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='V'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3v,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

    if(w_exist) then
    do k=1,nsig_regional+1
       read(iunit)((field3w(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! V
       write(6,*)' k,max,min,mid W=',k,maxval(field3w(:,:,k)),minval(field3w(:,:,k)), &
            field3w(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='W'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
      write(6,*)' rmse_var=',trim(rmse_var)
      write(6,*)' ordering=',ordering
      write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
      write(6,*)' ndim1=',ndim1
      write(6,*)' staggering=',staggering
      write(6,*)' start_index=',start_index
      write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3w,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    end if
    
    read(iunit)   field2   !  LANDMASK
    if(print_verbose)write(6,*)'max,min LANDMASK=',maxval(field2),minval(field2)
  
    read(iunit)   field2   ! SEAICE
    if(print_verbose)write(6,*)'max,min SEAICE=',maxval(field2),minval(field2)
    rmse_var='SEAICE'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
      write(6,*)' rmse_var=',trim(rmse_var)
      write(6,*)' ordering=',ordering
      write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
      write(6,*)' ndim1=',ndim1
      write(6,*)' staggering=',staggering
      write(6,*)' start_index=',start_index
      write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field2,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
  
    read(iunit)   field2   !SST
    if(print_verbose)write(6,*)' max,min SST=',maxval(field2),minval(field2)
    rmse_var='SST'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field2,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    
  ! Read in the rest of the fields
    if(l_gsd_soilTQ_nudge) then
       do k=4,9
          read(iunit) field2 !Rest of the fields
          if(print_verbose) &
             write(6,*)'read max,min REST',k,maxval(field2),minval(field2)
       end do
  
       do k=1,nsig_soil_regional
          read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! smois
          if(print_verbose) &
             write(6,*)' k,max,min,mid SMOIS=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
       end do
       rmse_var='SMOIS'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var=',trim(rmse_var)
          write(6,*)' ordering=',ordering
          write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
          write(6,*)' ndim1=',ndim1
          write(6,*)' staggering=',staggering
          write(6,*)' start_index=',start_index
          write(6,*)' end_index1=',end_index1
       end if
       call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  
       do k=1,nsig_soil_regional
          read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! tslb
          if(print_verbose) &
             write(6,*)' k,max,min,mid TSLB=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
       end do
       rmse_var='TSLB'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var=',trim(rmse_var)
          write(6,*)' ordering=',ordering
          write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
          write(6,*)' ndim1=',ndim1
          write(6,*)' staggering=',staggering
          write(6,*)' start_index=',start_index
          write(6,*)' end_index1=',end_index1
       end if
       call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
    else
       do k=4,11   ! corrected according to Ming Hu's finding
  
          read(iunit) field2 !Rest of the fields
          if(print_verbose) &
             write(6,*)'read max,min REST',k,maxval(field2),minval(field2)
       end do
    endif
  
    read(iunit)   field2   !TSK
    if(print_verbose)write(6,*)' max,min TSK=',maxval(field2),minval(field2)
    rmse_var='TSK'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field2,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
  
    if(i_use_2mt4b >0 .or. i_use_2mq4b >0 ) then
       read(iunit)   field2   !Q2
       if(print_verbose)write(6,*)' max,min Q2=',maxval(field2),minval(field2)
       rmse_var='Q2'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var=',trim(rmse_var)
          write(6,*)' ordering=',ordering
          write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
          write(6,*)' ndim1=',ndim1
          write(6,*)' staggering=',staggering
          write(6,*)' start_index=',start_index
          write(6,*)' end_index1=',end_index1
       end if
       call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
    endif
  
    if(l_gsd_soilTQ_nudge) then
       read(iunit)   field2   !SOILT1
       if(print_verbose) &
          write(6,*)' max,min SOILT1 d=',maxval(field2),minval(field2)
       rmse_var='SOILT1'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var=',trim(rmse_var)
          write(6,*)' ordering=',ordering
          write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
          write(6,*)' ndim1=',ndim1
          write(6,*)' staggering=',staggering
          write(6,*)' start_index=',start_index
          write(6,*)' end_index1=',end_index1
       end if
       call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
    endif
  
    if(i_use_2mt4b >0) then
       read(iunit)   field2   !TH2
       if(print_verbose)write(6,*)' max,min TH2 d=',maxval(field2),minval(field2)
       rmse_var='TH2'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var=',trim(rmse_var)
          write(6,*)' ordering=',ordering
          write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
          write(6,*)' ndim1=',ndim1
          write(6,*)' staggering=',staggering
          write(6,*)' start_index=',start_index
          write(6,*)' end_index1=',end_index1
       end if
       call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
            field2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
    endif
 
    if (l_hydrometeor_bkio .and. n_actual_clouds>0) then
      do k=1,nsig_regional
         read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qc
         if(print_verbose) &
            write(6,*)' k,max,min,mid Qc=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
              field3(nlon_regional/2,nlat_regional/2,k)
      end do
      rmse_var='QCLOUD'
      call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
           start_index,end_index1, WrfType, ierr    )
      if(print_verbose)then
         write(6,*)' rmse_var=',trim(rmse_var)
         write(6,*)' ordering=',ordering
         write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
         write(6,*)' ndim1=',ndim1
         write(6,*)' staggering=',staggering
         write(6,*)' start_index=',start_index
         write(6,*)' end_index1=',end_index1
      end if
      where (field3 < tiny_single) field3 = tiny_single
      call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
           field3,WRF_REAL,0,0,0,ordering,           &
           staggering, dimnames ,               &
           start_index,end_index1,               & !dom
           start_index,end_index1,               & !mem
           start_index,end_index1,               & !pat
           ierr                                 )
  
      do k=1,nsig_regional
         read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qr
         if(print_verbose) &
            write(6,*)' k,max,min,mid Qr=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
              field3(nlon_regional/2,nlat_regional/2,k)
      end do
      rmse_var='QRAIN'
      call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
           start_index,end_index1, WrfType, ierr    )
      if(print_verbose)then
         write(6,*)' rmse_var=',trim(rmse_var)
         write(6,*)' ordering=',ordering
         write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
         write(6,*)' ndim1=',ndim1
         write(6,*)' staggering=',staggering
         write(6,*)' start_index=',start_index
         write(6,*)' end_index1=',end_index1
      end if
      where (field3 < tiny_single) field3 = tiny_single
      call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
           field3,WRF_REAL,0,0,0,ordering,           &
           staggering, dimnames ,               &
           start_index,end_index1,               & !dom
           start_index,end_index1,               & !mem
           start_index,end_index1,               & !pat
           ierr                                 )
  
      do k=1,nsig_regional
         read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qs
         if(print_verbose) &
            write(6,*)' k,max,min,mid Qs=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
              field3(nlon_regional/2,nlat_regional/2,k)
      end do
      rmse_var='QSNOW'
      call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
           start_index,end_index1, WrfType, ierr    )
      if(print_verbose)then
         write(6,*)' rmse_var=',trim(rmse_var)
         write(6,*)' ordering=',ordering
         write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
         write(6,*)' ndim1=',ndim1
         write(6,*)' staggering=',staggering
         write(6,*)' start_index=',start_index
         write(6,*)' end_index1=',end_index1
      end if
      where (field3 < tiny_single) field3 = tiny_single
      call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
           field3,WRF_REAL,0,0,0,ordering,           &
           staggering, dimnames ,               &
           start_index,end_index1,               & !dom
           start_index,end_index1,               & !mem
           start_index,end_index1,               & !pat
           ierr                                 )
  
      do k=1,nsig_regional
         read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qi
         if(print_verbose) &
            write(6,*)' k,max,min,mid Qi=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
              field3(nlon_regional/2,nlat_regional/2,k)
      end do
      rmse_var='QICE'
      call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
           start_index,end_index1, WrfType, ierr    )
      if(print_verbose)then
         write(6,*)' rmse_var=',trim(rmse_var)
         write(6,*)' ordering=',ordering
         write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
         write(6,*)' ndim1=',ndim1
         write(6,*)' staggering=',staggering
         write(6,*)' start_index=',start_index
         write(6,*)' end_index1=',end_index1
      end if
      where (field3 < tiny_single) field3 = tiny_single
      call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
           field3,WRF_REAL,0,0,0,ordering,           &
           staggering, dimnames ,               &
           start_index,end_index1,               & !dom
           start_index,end_index1,               & !mem
           start_index,end_index1,               & !pat
           ierr                                 )
  
      do k=1,nsig_regional
         read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qg
         if(print_verbose) &
            write(6,*)' k,max,min,mid Qg=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
              field3(nlon_regional/2,nlat_regional/2,k)
      end do
      rmse_var='QGRAUP'
      call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
           start_index,end_index1, WrfType, ierr    )
      if(print_verbose)then
         write(6,*)' rmse_var=',trim(rmse_var)
         write(6,*)' ordering=',ordering
         write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
         write(6,*)' ndim1=',ndim1
         write(6,*)' staggering=',staggering
         write(6,*)' start_index=',start_index
         write(6,*)' end_index1=',end_index1
      end if
      where (field3 < tiny_single) field3 = tiny_single
      call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
           field3,WRF_REAL,0,0,0,ordering,           &
           staggering, dimnames ,               &
           start_index,end_index1,               & !dom
           start_index,end_index1,               & !mem
           start_index,end_index1,               & !pat
           ierr                                 )
  
      do k=1,nsig_regional
         read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional) !  Qnr
         if(print_verbose) &
            write(6,*)' k,max,min,mid Qnr=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
              field3(nlon_regional/2,nlat_regional/2,k)
      end do
      rmse_var='QNRAIN'
      call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
           start_index,end_index1, WrfType, ierr    )
      if(print_verbose)then
         write(6,*)' rmse_var=',trim(rmse_var)
         write(6,*)' ordering=',ordering
         write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
         write(6,*)' ndim1=',ndim1
         write(6,*)' staggering=',staggering
         write(6,*)' start_index=',start_index
         write(6,*)' end_index1=',end_index1
      end if
      where (field3 < tiny_single) field3 = tiny_single
      call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
           field3,WRF_REAL,0,0,0,ordering,           &
           staggering, dimnames ,               &
           start_index,end_index1,               & !dom
           start_index,end_index1,               & !mem
           start_index,end_index1,               & !pat
           ierr                                 )

      if ( .not. l_use_dbz_directDA ) then ! direct reflectivity DA does not update qni and qns

         do k=1,nsig_regional
            read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional) !  Qni
            if(print_verbose) &
               write(6,*)' k,max,min,mid Qni=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                 field3(nlon_regional/2,nlat_regional/2,k)
         end do
         rmse_var='QNICE'
         call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
              start_index,end_index1, WrfType, ierr    )
         if(print_verbose)then
            write(6,*)' rmse_var=',trim(rmse_var)
            write(6,*)' ordering=',ordering
            write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
            write(6,*)' ndim1=',ndim1
            write(6,*)' staggering=',staggering
            write(6,*)' start_index=',start_index
            write(6,*)' end_index1=',end_index1
         end if
         where (field3 < tiny_single) field3 = tiny_single
         call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
              field3,WRF_REAL,0,0,0,ordering,           &
              staggering, dimnames ,               &
              start_index,end_index1,               & !dom
              start_index,end_index1,               & !mem
              start_index,end_index1,               & !pat
              ierr                                 )
  
         do k=1,nsig_regional
            read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional) !  Qnc
            if(print_verbose) &
               write(6,*)' k,max,min,mid Qnc=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                  field3(nlon_regional/2,nlat_regional/2,k)
         end do
         rmse_var='QNCLOUD'
         call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
              start_index,end_index1, WrfType, ierr    )
         if(print_verbose)then
            write(6,*)' rmse_var=',trim(rmse_var)
            write(6,*)' ordering=',ordering
            write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
            write(6,*)' ndim1=',ndim1
            write(6,*)' staggering=',staggering
            write(6,*)' start_index=',start_index
            write(6,*)' end_index1=',end_index1
         end if
         where (field3 < tiny_single) field3 = tiny_single
         call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
              field3,WRF_REAL,0,0,0,ordering,           &
              staggering, dimnames ,               &
              start_index,end_index1,               & !dom
              start_index,end_index1,               & !mem
              start_index,end_index1,               & !pat
              ierr                                 )
       end if
    end if ! l_hydrometeor_bkio
    
    if(dbz_exist .and. if_model_dbz)then
      do k=1,nsig_regional
         read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! V
         if(print_verbose) write(6,*)' k,max,min,mid dbz=',k,maxval(field3v(:,:,k)),minval(field3v(:,:,k)), &
              field3v(nlon_regional/2,nlat_regional/2,k)
      end do
      rmse_var='REFL_10CM'
      call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
           start_index,end_index1, WrfType, ierr    )
      if(print_verbose)then
        write(6,*)' rmse_var=',trim(rmse_var)
        write(6,*)' ordering=',ordering
        write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
        write(6,*)' ndim1=',ndim1
        write(6,*)' staggering=',staggering
        write(6,*)' start_index=',start_index
        write(6,*)' end_index1=',end_index1
      end if
      call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
           field3,WRF_REAL,0,0,0,ordering,           &
           staggering, dimnames ,               &
           start_index,end_index1,               & !dom
           start_index,end_index1,               & !mem
           start_index,end_index1,               & !pat
           ierr                                 )
    end if
 
    if( l_hydrometeor_bkio )then
      do k=1,nsig_regional
         read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! TTEN 
         if(print_verbose) &
            write(6,*)' k,max,min,mid TTEN=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
              field3(nlon_regional/2,nlat_regional/2,k)
      end do
      rmse_var='RAD_TTEN_DFI'
      call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
           start_index,end_index1, WrfType, ierr    )
      if(print_verbose) then
         write(6,*)' rmse_var=',trim(rmse_var)
         write(6,*)' ordering=',ordering
         write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
         write(6,*)' ndim1=',ndim1
         write(6,*)' staggering=',staggering
         write(6,*)' start_index=',start_index
         write(6,*)' end_index1=',end_index1
      end if
      call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
           field3,WRF_REAL,0,0,0,ordering,           &
           staggering, dimnames ,               &
           start_index,end_index1,               & !dom
           start_index,end_index1,               & !mem
           start_index,end_index1,               & !pat
           ierr                                 )
  
    end if     ! l_hydrometeor_bkio
  
    if(laeroana_gocart) then
       call gsi_chemguess_get('aerosols::3d', n_gocart_var, ier)
       do iv = 1, n_gocart_var
          select case ( iv )
             case ( 1 )
                rmse_var='sulf'
             case ( 2 )
                rmse_var='BC1'
             case ( 3 )
                rmse_var='BC2'
             case ( 4 )
                rmse_var='OC1'
             case ( 5 )
                rmse_var='OC2'
             case ( 6 )
                rmse_var='DUST_1'
             case ( 7 )
                rmse_var='DUST_2'
             case ( 8 )
                rmse_var='DUST_3'
             case ( 9 )
                rmse_var='DUST_4'
             case ( 10 )
                rmse_var='DUST_5'
             case ( 11 )
                rmse_var='SEAS_1'
             case ( 12 )
               rmse_var='SEAS_2'
             case ( 13 )
                rmse_var='SEAS_3'
             case ( 14 )
                rmse_var='SEAS_4'
             case ( 15 )
                rmse_var='P25'
          end select
          do k=1,nsig_regional
             read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)
             if(print_verbose) &
                write(6,*)' k,max,min,mid var=',rmse_var,k,            &
                       maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                      field3(nlon_regional/2,nlat_regional/2,k)
          end do
          call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
               start_index,end_index1, WrfType, ierr    )
          if(print_verbose) then
             write(6,*)' rmse_var=',trim(rmse_var)
             write(6,*)' ordering=',ordering
             write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
             write(6,*)' ndim1=',ndim1
             write(6,*)' staggering=',staggering
             write(6,*)' start_index=',start_index
             write(6,*)' end_index=',end_index1
          end if
          if ( trim(rmse_var) == 'sulf' ) then
             field3 = field3/ppmv_conv    ! ug/kg to ppmv
          end if
          call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
               field3,WRF_REAL,0,0,0,ordering,           &
               staggering, dimnames ,               &
               start_index,end_index1,               & !dom
               start_index,end_index1,               & !mem
               start_index,end_index1,               & !pat
               ierr                                 )
       end do ! n_gocart_var loop
    endif ! laeroana_gocart
  
    if (wrf_pm2_5) then
  
       rmse_var='PM2_5_DRY'
  
       do k=1,nsig_regional
          READ(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)
          if(print_verbose) &
             write(6,*)' k,max,min,mid var=',rmse_var,k,            &
               maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
       end do
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var=',trim(rmse_var)
          write(6,*)' ordering=',ordering
          write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
          write(6,*)' ndim1=',ndim1
          write(6,*)' staggering=',staggering
          write(6,*)' start_index=',start_index
          write(6,*)' end_index=',end_index1
       end if
       call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
    endif
  
    deallocate(field1,field2,field2b,ifield2,field3,field3u,field3v,field3w)
    call ext_ncd_ioclose(dh1, Status)
    close(iunit)
    !
    !  reopen, update global attributes.
    !
    call nc_check( nf90_open(trim(adjustl(flnm1)),nf90_write,dh1),&
      myname_,'open: '//trim(flnm1) )
  call nc_check( nf90_put_att(dh1,nf90_global,'START_DATE',trim(DateStr1)),&
      myname_,'put_att:  START_DATE '//trim(flnm1) )
  call nc_check( nf90_put_att(dh1,nf90_global,'SIMULATION_START_DATE',trim(DateStr1)),&
      myname_,'put_att:  SIMULATION_START_DATE '//trim(flnm1) )
  call nc_check( nf90_put_att(dh1,nf90_global,'GMT',real(ihour,r_single)),&
      myname_,'put_att: GMT '//trim(flnm1) )
  call nc_check( nf90_put_att(dh1,nf90_global,'JULYR',iyear),&
      myname_,'put_att: JULYR'//trim(flnm1) )
  call nc_check( nf90_put_att(dh1,nf90_global,'JULDAY',iw3jdn(iyear,imonth,iday)-iw3jdn(iyear,1,1)+1),&
      myname_,'put_att: JULDAY'//trim(flnm1) )
  call nc_check( nf90_close(dh1),&
      myname_,'close: '//trim(flnm1) )
    
  end subroutine update_netcdf_mass_wrf
  
  subroutine update_netcdf_nmm_wrf(this)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    update_netcdf_nmm   create netcdf format wrf restart from internal binary file.
  !   pgrmmr:
  !
  ! abstract: create netcdf format wrf restart from internal binary file.
  !
  ! program history log:
  !   2004-11-05  treadon - add return code 75 for error stop
  !   2004-12-15  treadon - remove get_lun, read guess from file "wrf_inout"
  !   2005-12-09  middlecoff - read in pint if update_pint=.true.
  !   2005-12-09  middlecoff - initialize character variable staggering and removed staggering1,staggering2
  !   2009-08-14  lueken - update documentation
  !   2012-01-14  zhu    - add cloud hydrometeors
  !   2012-04-20  Whitaker - update NSTART_HOUR variable in HWRF restart file.
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:
  !
  !$$$ end documentation block
  
    use netcdf, only: nf90_open,nf90_close,nf90_put_att
    use netcdf, only: nf90_write,nf90_global
    use kinds, only: r_single,i_kind,r_kind
    use constants, only: tiny_single
    use wrf_params_mod, only: update_pint
    use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use mpeu_util, only: die,getindex
    use control_vectors, only: cvars3d
    use guess_grids, only: ntguessig
    use netcdf_mod, only: nc_check
    use gsi_io, only: verbose
  ! use wrf_data
    implicit none
    include 'netcdf.inc'
    class(convert_netcdf_class), intent(inout) :: this
! include 'wrf_status_codes.h'
  
    character(len=120) :: flnm1,flnm2
    character(len=19)  :: DateStr1
    integer(i_kind)            :: dh1
  
    integer(i_kind) :: iunit
  
    integer(i_kind) :: i,j,k
    integer(i_kind) :: ndim1
    integer(i_kind) :: WrfType
    integer(i_kind), dimension(4)  :: start_index, end_index1
    character (len= 4) :: staggering=' N/A'
    character (len= 3) :: ordering
  
    character (len=80), dimension(3)  ::  dimnames
    character(len=24),parameter :: myname_ = 'update_netcdf_nmm'
    
    integer(i_kind) :: it, n_actual_clouds, ier, iret, ierr, Status, Status_next_time
    integer(i_kind) icw4crtm,iqtotal
    real(r_kind),pointer,dimension(:,:,:):: ges_ql=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qi=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qr=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qs=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qg=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qh=>NULL()
  
  ! binary stuff
  
  ! rmse stuff
  
    character (len=31) :: rmse_var
    integer(i_kind) :: nallo
  
    integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond,iw3jdn
    integer(i_kind) nlon_regional,nlat_regional,nsig_regional,istatus
  ! real(r_single) pt_regional
    real(r_single),allocatable::field3(:,:,:),field2(:,:),field1(:)
    integer(i_kind),allocatable::ifield2(:,:)
    integer(i_kind),allocatable::ifield1(:)
    integer(i_kind) wrf_real
    data iunit / 15 /
    logical print_verbose
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate

    print_verbose=.false.
    if(verbose)print_verbose=.true.
    wrf_real=104
    start_index=0
    end_index1=0
  
  ! Inquire about cloud guess fields
    call gsi_metguess_get('clouds::3d',n_actual_clouds,istatus)
    if (n_actual_clouds>0) then
       it=ntguessig
  !    Get pointer to cloud water mixing ratio
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier=iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qs',ges_qs,iret); ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qg',ges_qg,iret); ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qh',ges_qh,iret); ier=ier+iret
  
  !    Determine whether or not cloud-condensate is the control variable
       icw4crtm=getindex(cvars3d,'cw')
  
  !    Determine whether total moisture (water vapor+total cloud condensate) is the control variable
       iqtotal=getindex(cvars3d,'qt')
  
       if (ier/=0 .or. (icw4crtm<=0 .and. iqtotal<=0)) n_actual_clouds=0
    end if
  
  ! transfer code from diffwrf for converting netcdf wrf nmm restart file
  ! to temporary binary format
  
  !
  !           update nmm netcdf file with analysis variables from 3dvar
  !
    flnm1='wrf_inout'
    call ext_ncd_open_for_update( trim(flnm1), 0, 0, "", dh1, Status)
    if ( Status /= 0 )then
       write(6,*)'UPDATE_NETCDF_NMM:  problem with flnm1 = ',&
            trim(flnm1),', Status = ', Status
       call stop2(75)
    endif
    
       
    close(51)
    flnm2='siganl'
    open(iunit,file=flnm2,form='unformatted')
  
  !-------------  get date info
  
    call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
    read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
    write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond
  
  !-------------  get grid info
    rmse_var='T'
    call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &    
                                 start_index,end_index1, WrfType, ierr    )      
  
    nlon_regional=end_index1(1)
    nlat_regional=end_index1(2)
    nsig_regional=end_index1(3)
    nallo = nsig_regional
    if(update_pint) nallo = nallo+1   ! add contribution of PINT
  ! write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
    write(6,*)' update_netcdf_nmm: nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional               
    allocate(field2(nlon_regional,nlat_regional))
    allocate(field3(nlon_regional,nlat_regional,nallo))
    allocate(ifield2(nlon_regional,nlat_regional))
    allocate(field1(max(nlon_regional,nlat_regional,nsig_regional)))
    allocate(ifield1(max(nlon_regional,nlat_regional,nsig_regional)))
    
    read(iunit) ! iyear,imonth,iday,ihour,iminute,isecond, &
         !        nlon_regional,nlat_regional,nsig_regional, &
         !        dlmd_regional,dphd_regional,pt_regional,pdtop_regional
    read(iunit) ! field1(1:nsig_regional)  ! DETA1
  
    read(iunit) ! field1(1:nsig_regional)  ! AETA1
  
    read(iunit) ! field1(1:nsig_regional+1)  !  ETA1
  
    read(iunit) ! field1(1:nsig_regional)  ! DETA2
  
    read(iunit) ! field1(1:nsig_regional)  ! AETA2
  
    read(iunit) ! field1(1:nsig_regional+1)  ! ETA2
  
    read(iunit) ! field2   !GLAT,DX_NMM
  
    read(iunit) ! field2   !GLON,DY_NMM
  
    read(iunit)   field2   !PD 
    if(print_verbose)write(6,*)' max,min pd=',maxval(field2),minval(field2)
    rmse_var='PD'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field2,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    
    read(iunit) ! field2   ! FIS
    
    if(update_pint) then
       do k=1,nsig_regional+1
          read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! PINT
          if(print_verbose) &
             write(6,*)' k,max,min,mid PINT=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                      field3(nlon_regional/2,nlat_regional/2,k)
       end do
       rmse_var='PINT'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
                                  start_index,end_index1, WrfType, ierr    )
       if(print_verbose) then
          write(6,*)' rmse_var=',trim(rmse_var)
          write(6,*)' ordering=',ordering
          write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
          write(6,*)' ndim1=',ndim1
          write(6,*)' staggering=',staggering
          write(6,*)' start_index=',start_index
          write(6,*)' end_index1=',end_index1
       end if
       call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),          &
                                field3,WRF_REAL,0,0,0,ordering,       &
                                staggering, dimnames ,                &
                                start_index,end_index1,               & !dom
                                start_index,end_index1,               & !mem
                                start_index,end_index1,               & !pat
                                ierr                                  )
    endif
  
    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! T
       if(print_verbose) &
          write(6,*)' k,max,min,mid T=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='T'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose) then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    
    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q
       if(print_verbose) &
          write(6,*)' k,max,min,mid Q=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='Q'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    where (field3 < tiny_single) field3 = tiny_single
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    
    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! U
       if(print_verbose) &
          write(6,*)' k,max,min,mid U=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='U'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose)then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    
    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! V
       if(print_verbose) &
          write(6,*)' k,max,min,mid V=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='V'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    if(print_verbose) then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
    
    do k=1,12
       read(iunit) field2
       if(print_verbose)write(6,*)'read max,min REST',k,maxval(field2),minval(field2)
    end do
  
    if (n_actual_clouds>0) then
       do k=1,nsig_regional
          read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! CWM
          if(print_verbose) &
             write(6,*)' k,max,min,mid CWM=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
       end do
       rmse_var='CWM'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var=',trim(rmse_var)
          write(6,*)' ordering=',ordering
          write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
          write(6,*)' ndim1=',ndim1
          write(6,*)' staggering=',staggering
          write(6,*)' start_index=',start_index
          write(6,*)' end_index1=',end_index1
       end if
       where (field3 < tiny_single) field3 = tiny_single
       call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  
       do k=1,nsig_regional
          read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! F_ICE
          if(print_verbose) &
             write(6,*)' k,max,min,mid F_ICE=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
       end do
       rmse_var='F_ICE'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var=',trim(rmse_var)
          write(6,*)' ordering=',ordering
          write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
          write(6,*)' ndim1=',ndim1
          write(6,*)' staggering=',staggering
          write(6,*)' start_index=',start_index
          write(6,*)' end_index1=',end_index1
       end if
       where (field3 < tiny_single) field3 = tiny_single
       call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  
       do k=1,nsig_regional
          read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! F_RAIN
          if(print_verbose) &
             write(6,*)' k,max,min,mid F_RAIN=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
       end do
       rmse_var='F_RAIN'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(print_verbose) then
          write(6,*)' rmse_var=',trim(rmse_var)
          write(6,*)' ordering=',ordering
          write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
          write(6,*)' ndim1=',ndim1
          write(6,*)' staggering=',staggering
          write(6,*)' start_index=',start_index
          write(6,*)' end_index1=',end_index1
       end if
       where (field3 < tiny_single) field3 = tiny_single
       call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  
       do k=1,nsig_regional
          read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! F_RIMEF
          if(print_verbose) &
             write(6,*)' k,max,min,mid F_RIMEF=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
               field3(nlon_regional/2,nlat_regional/2,k)
       end do
       rmse_var='F_RIMEF'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       if(print_verbose)then
          write(6,*)' rmse_var=',trim(rmse_var)
          write(6,*)' ordering=',ordering
          write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
          write(6,*)' ndim1=',ndim1
          write(6,*)' staggering=',staggering
          write(6,*)' start_index=',start_index
          write(6,*)' end_index1=',end_index1
       end if
       where (field3 < tiny_single) field3 = tiny_single
       call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
            field3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
    end if
  
    close(iunit)
  
    rmse_var='NSTART_HOUR'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
    if(print_verbose) then
       write(6,*)' rmse_var=',trim(rmse_var)
       write(6,*)' ordering=',ordering
       write(6,*)' WrfType=',WrfType
       write(6,*)' ndim1=',ndim1
       write(6,*)' staggering=',staggering
       write(6,*)' start_index=',start_index
       write(6,*)' end_index1=',end_index1
    end if
  ifield1(1) = ihour
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
            ifield1,WrfType,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
    if (ierr .ne. 0) then
     write(6,*) '**error updating NSTART_HOUR (only needed for HWRF restart file)**'
    endif
    deallocate(field1,field2,ifield1,ifield2,field3)
    call ext_ncd_ioclose(dh1, Status)
    !
    !  reopen, update global attributes.
    !
    call nc_check( nf90_open(trim(adjustl(flnm1)),nf90_write,dh1),&
      myname_,'open: '//trim(adjustl(flnm1)) )
  call nc_check( nf90_put_att(dh1,nf90_global,'START_DATE',trim(DateStr1)),&
      myname_,'put_att:  START_DATE '//trim(adjustl(flnm1)) )
  call nc_check( nf90_put_att(dh1,nf90_global,'SIMULATION_START_DATE',trim(DateStr1)),&
      myname_,'put_att:  SIMULATION_START_DATE '//trim(adjustl(flnm1)) )
  call nc_check( nf90_put_att(dh1,nf90_global,'GMT',real(ihour,r_single)),&
      myname_,'put_att: GMT '//trim(adjustl(flnm1)) )
  call nc_check( nf90_put_att(dh1,nf90_global,'JULYR',iyear),&
      myname_,'put_att: JULYR'//trim(adjustl(flnm1)) )
  call nc_check( nf90_put_att(dh1,nf90_global,'JULDAY',iw3jdn(iyear,imonth,iday)-iw3jdn(iyear,1,1)+1),&
      myname_,'put_att: JULDAY'//trim(adjustl(flnm1)) )
  call nc_check( nf90_close(dh1),&
      myname_,'close: '//trim(adjustl(flnm1)) )
    
  end subroutine update_netcdf_nmm_wrf
end module convert_netcdf_mod
