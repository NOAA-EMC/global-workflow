module fv3_module


  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------
  use netcdf
  use constants
  use kinds
  use nemsio_module   

  type nemsio_meta
     character(nemsio_charkind),   dimension(:),       allocatable :: recname	     
     character(nemsio_charkind),   dimension(:),       allocatable :: reclevtyp      
     character(16),                dimension(:),       allocatable :: variname	     
     character(16),                dimension(:),       allocatable :: varrname
     character(16),                dimension(:),       allocatable :: varr8name      
     character(16),                dimension(:),       allocatable :: aryiname	     
     character(16),                dimension(:),       allocatable :: aryr8name      
     character(nemsio_charkind8)                                   :: gdatatype      
     character(nemsio_charkind8)                                   :: modelname      
     real(nemsio_realkind)                                         :: rlon_min	     
     real(nemsio_realkind)                                         :: rlon_max	     
     real(nemsio_realkind)                                         :: rlat_min	     
     real(nemsio_realkind)                                         :: rlat_max	     
     real(nemsio_realkind),        dimension(:),       allocatable :: lon     
     real(nemsio_realkind),        dimension(:),       allocatable :: lat
     real(nemsio_realkind),        dimension(:),       allocatable :: varrval
     integer(nemsio_intkind),      dimension(:,:),     allocatable :: aryival	     
     integer(nemsio_intkind),      dimension(:),       allocatable :: reclev	     
     integer(nemsio_intkind),      dimension(:),       allocatable :: varival	     
     integer(nemsio_intkind),      dimension(:),       allocatable :: aryilen	     
     integer(nemsio_intkind),      dimension(:),       allocatable :: aryr8len	     
     integer(nemsio_intkind)                                       :: idate(7)	     
     integer(nemsio_intkind)                                       :: version	     
     integer(nemsio_intkind)                                       :: nreo_vc	     
     integer(nemsio_intkind)                                       :: nrec	     
     integer(nemsio_intkind)                                       :: nmeta	     
     integer(nemsio_intkind)                                       :: nmetavari      
     integer(nemsio_intkind)                                       :: nmetaaryi      
     integer(nemsio_intkind)                                       :: nmetavarr
     integer(nemsio_intkind)                                       :: nfhour	     
     integer(nemsio_intkind)                                       :: nfminute	     
     integer(nemsio_intkind)                                       :: nfsecondn      
     integer(nemsio_intkind)                                       :: nfsecondd      
     integer(nemsio_intkind)                                       :: dimx     
     integer(nemsio_intkind)                                       :: dimy     
     integer(nemsio_intkind)                                       :: dimz     
     integer(nemsio_intkind)                                       :: nframe     
     integer(nemsio_intkind)                                       :: nsoil     
     integer(nemsio_intkind)                                       :: ntrac    
     integer(nemsio_intkind)                                       :: ncldt     
     integer(nemsio_intkind)                                       :: idvc     
     integer(nemsio_intkind)                                       :: idsl     
     integer(nemsio_intkind)                                       :: idvm     
     integer(nemsio_intkind)                                       :: idrt     
     integer(nemsio_intkind)                                       :: fhour     

  end type nemsio_meta ! type nemsio_meta
  contains
!-----------------------------------------------------------------------
  subroutine fv3_netcdf_read_2d(ncid2d,ifhr,meta_nemsio,varname,data2d)
    
    implicit none
    type(nemsio_meta)               :: meta_nemsio
    integer                         :: ncid2d
    integer                         :: ifhr,varid,stat
    real                            :: data2d(meta_nemsio%dimx,meta_nemsio%dimy)
    character(nemsio_charkind)      :: varname

  ! loop through 2d data
    stat = nf90_inq_varid(ncid2d,trim(varname),varid)
    !print*,stat,varid,trim(varname)
    stat = nf90_get_var(ncid2d,varid,data2d,start=(/1,1,ifhr/),count=(/meta_nemsio%dimx,meta_nemsio%dimy,1/))
    IF (stat .NE. 0 ) THEN
       print*,'error reading ',varname
       STOP
    ENDIF

end subroutine    fv3_netcdf_read_2d
!-----------------------------------------------------------------------

  subroutine fv3_netcdf_read_3d(ncid3d,ifhr,meta_nemsio,varname,k,data2d)
    
    implicit none
    
    type(nemsio_meta)               :: meta_nemsio
    integer                         :: ncid3d
    integer                         :: k
    integer                         :: ifhr,varid,stat
    character(nemsio_charkind)      :: varname
    !real                            :: data3d(meta_nemsio%dimx,meta_nemsio%dimy,meta_nemsio%dimz)
    real                            :: data2d(meta_nemsio%dimx,meta_nemsio%dimy)


    stat = nf90_inq_varid(ncid3d,trim(varname),varid)
    !print*,stat,varname,varid
    !stat = nf90_get_var(ncid3d,varid,data3d,start=(/1,1,1,ifhr/),count=(/meta_nemsio%dimx,meta_nemsio%dimy,meta_nemsio%dimz,1/))
    stat = nf90_get_var(ncid3d,varid,data2d,start=(/1,1,k,ifhr/),count=(/meta_nemsio%dimx,meta_nemsio%dimy,1,1/))
    
    IF (stat .NE. 0 ) THEN
       print*,'error reading ',varname
       STOP
    ENDIF
   
end subroutine    fv3_netcdf_read_3d
!-----------------------------------------------------------------------

  subroutine define_nemsio_meta(meta_nemsio,nlons,nlats,nlevs,nvar2d,nvar3d,lons,lats)
    implicit none
    type(nemsio_meta)               :: meta_nemsio
    integer                         :: nlons,nlats,nlevs,i,j,k,nvar2d,nvar3d
    integer*8                       :: ct
    real                         :: lons(nlons),lats(nlats)
! local

    meta_nemsio%idate(1:6) = 0
    meta_nemsio%idate(7)   = 1
    meta_nemsio%modelname  = 'GFS'
    meta_nemsio%version    = 198410
    meta_nemsio%nrec       = nvar2d + nlevs*nvar3d 
    meta_nemsio%nmeta      = 8
    meta_nemsio%nmetavari  = 3
    meta_nemsio%nmetavarr  = 1
    meta_nemsio%nmetaaryi  = 1
    meta_nemsio%dimx       = nlons
    meta_nemsio%dimy       = nlats
    meta_nemsio%dimz       = nlevs
    meta_nemsio%rlon_min   = minval(lons)
    meta_nemsio%rlon_max   = maxval(lons)
    meta_nemsio%rlat_min   = minval(lats)
    meta_nemsio%rlat_max   = maxval(lats)
    meta_nemsio%nsoil      = 4
    meta_nemsio%nframe     = 0
    meta_nemsio%nfminute   = 0
    meta_nemsio%nfsecondn  = 0
    meta_nemsio%nfsecondd  = 1
    meta_nemsio%ntrac      = 3
    meta_nemsio%idrt       = 0   
    meta_nemsio%ncldt      = 3
    meta_nemsio%idvc       = 2


   allocate(meta_nemsio%recname(meta_nemsio%nrec))
   allocate(meta_nemsio%reclevtyp(meta_nemsio%nrec))
   allocate(meta_nemsio%reclev(meta_nemsio%nrec))
   allocate(meta_nemsio%variname(meta_nemsio%nmetavari))
   allocate(meta_nemsio%varival(meta_nemsio%nmetavari))
   allocate(meta_nemsio%aryiname(meta_nemsio%nmetavari))
   allocate(meta_nemsio%aryilen(meta_nemsio%nmetavari))
   allocate(meta_nemsio%varrname(meta_nemsio%nmetavarr))
   allocate(meta_nemsio%varrval(meta_nemsio%nmetavarr))
   allocate(meta_nemsio%lon(nlons*nlats))
   allocate(meta_nemsio%lat(nlons*nlats))

   meta_nemsio%varrname(1)='zhour'
   meta_nemsio%variname(1)='cu_physics'
   meta_nemsio%varival(1)=4
   meta_nemsio%variname(2)='mp_physics'
   meta_nemsio%varival(2)=1000 
   meta_nemsio%variname(3)='IVEGSRC'
   meta_nemsio%varival(3)=2
   ct=1
   DO j=1,nlats
      DO i=1,nlons
         meta_nemsio%lon(ct)      = lons(i)
         meta_nemsio%lat(ct)      = lats(j)
	 ct=ct+1
      ENDDO
   ENDDO
   
   meta_nemsio%aryilen(1)    = nlats/2
   meta_nemsio%aryiname(1)   = 'lpl'
   meta_nemsio%reclev(:)=1
   meta_nemsio%recname(1)    = 'albdo_ave'
   meta_nemsio%reclevtyp(1)  = 'sfc'
   meta_nemsio%recname(2)    = 'cprat_ave'
   meta_nemsio%reclevtyp(2)  = 'sfc'
   meta_nemsio%recname(3)    = 'prate_ave'
   meta_nemsio%reclevtyp(3)  = 'sfc'
   meta_nemsio%recname(4)    = 'dlwrf_ave'
   meta_nemsio%reclevtyp(4)  = 'sfc'
   meta_nemsio%recname(5)    = 'ulwrf_ave'
   meta_nemsio%reclevtyp(5)  = 'sfc'
   meta_nemsio%recname(6)    = 'dswrf_ave'
   meta_nemsio%reclevtyp(6)  = 'sfc'
   meta_nemsio%recname(7)    = 'uswrf_ave'
   meta_nemsio%reclevtyp(7)  = 'sfc'
   meta_nemsio%recname(8)    = 'dswrf_ave'
   meta_nemsio%reclevtyp(8)  = 'nom. top'
   meta_nemsio%recname(9)    = 'uswrf_ave'
   meta_nemsio%reclevtyp(9)  = 'nom. top'
   meta_nemsio%recname(10)   = 'ulwrf_ave'
   meta_nemsio%reclevtyp(10) = 'nom. top'
   meta_nemsio%recname(11)   = 'gflux_ave'
   meta_nemsio%reclevtyp(11) = 'sfc'
   meta_nemsio%recname(12)   = 'hgt'
   meta_nemsio%reclevtyp(12) = 'sfc'
   meta_nemsio%recname(13)   = 'hpbl'
   meta_nemsio%reclevtyp(13) = 'sfc'
   meta_nemsio%recname(14)   = 'icec'
   meta_nemsio%reclevtyp(14) = 'sfc'
   meta_nemsio%recname(15)   = 'land'
   meta_nemsio%reclevtyp(15) = 'sfc'
   meta_nemsio%recname(16)   = 'lhtfl_ave'
   meta_nemsio%reclevtyp(16) = 'sfc'
   meta_nemsio%recname(17)   = 'shtfl_ave'
   meta_nemsio%reclevtyp(17) = 'sfc'
   meta_nemsio%recname(18)   = 'pres'
   meta_nemsio%reclevtyp(18) = 'sfc'
   meta_nemsio%recname(19)   = 'pwat'
   meta_nemsio%reclevtyp(19) = 'atmos col'
   meta_nemsio%recname(20)   = 'soilm'
   meta_nemsio%reclevtyp(20) = '0-200 cm down'
   meta_nemsio%recname(21)   = 'soilw'
   meta_nemsio%reclevtyp(21) = '0-10 cm down'
   meta_nemsio%recname(22)   = 'soilw'
   meta_nemsio%reclevtyp(22) = '10-40 cm down'
   meta_nemsio%recname(23)   = 'soilw'
   meta_nemsio%reclevtyp(23) = '40-100 cm down'
   meta_nemsio%recname(24)   = 'soilw'
   meta_nemsio%reclevtyp(24) = '100-200 cm down'
   meta_nemsio%recname(25)   = 'spfh'
   meta_nemsio%reclevtyp(25) = '2 m above gnd'
   meta_nemsio%recname(26)   = 'tmp'
   meta_nemsio%reclevtyp(26) = '0-10 cm down'
   meta_nemsio%recname(27)   = 'tmp'
   meta_nemsio%reclevtyp(27) = '10-40 cm down'
   meta_nemsio%recname(28)   = 'tmp'
   meta_nemsio%reclevtyp(28) = '40-100 cm down'
   meta_nemsio%recname(29)   = 'tmp'
   meta_nemsio%reclevtyp(29) = '100-200 cm down'
   meta_nemsio%recname(30)   = 'tmp'
   meta_nemsio%reclevtyp(30) = '2 m above gnd'
   meta_nemsio%recname(31)   = 'tmp'
   meta_nemsio%reclevtyp(31) = 'sfc'
   meta_nemsio%recname(32)   = 'ugwd'
   meta_nemsio%reclevtyp(32) = 'sfc'
   meta_nemsio%recname(33)   = 'vgwd'
   meta_nemsio%reclevtyp(33) = 'sfc'
   meta_nemsio%recname(34)   = 'uflx_ave'
   meta_nemsio%reclevtyp(34) = 'sfc'
   meta_nemsio%recname(35)   = 'vflx_ave'
   meta_nemsio%reclevtyp(35) = 'sfc'
   meta_nemsio%recname(36)   = 'ugrd'
   meta_nemsio%reclevtyp(36) = '10 m above gnd'
   meta_nemsio%recname(37)   = 'vgrd'
   meta_nemsio%reclevtyp(37) = '10 m above gnd'
   meta_nemsio%recname(38)   = 'weasd'
   meta_nemsio%reclevtyp(38) = 'sfc'
   meta_nemsio%recname(39)   = 'snod'
   meta_nemsio%reclevtyp(39) = 'sfc'
   meta_nemsio%recname(40)   = 'zorl'
   meta_nemsio%reclevtyp(40) = 'sfc'
   meta_nemsio%recname(41)   = 'vfrac'
   meta_nemsio%reclevtyp(41) = 'sfc'
   meta_nemsio%recname(42)   = 'f10m'
   meta_nemsio%reclevtyp(42) = 'sfc'
   meta_nemsio%recname(43)   = 'vtype'
   meta_nemsio%reclevtyp(43) = 'sfc'
   meta_nemsio%recname(44)   = 'stype'
   meta_nemsio%reclevtyp(44) = 'sfc'
   meta_nemsio%recname(45)   = 'tcdc_ave'
   meta_nemsio%reclevtyp(45) = 'atmos col'
   meta_nemsio%recname(46)   = 'tcdc_ave'
   meta_nemsio%reclevtyp(46) = 'high cld lay'
   meta_nemsio%recname(47)   = 'tcdc_ave'
   meta_nemsio%reclevtyp(47) = 'mid cld lay'
   meta_nemsio%recname(48)   = 'tcdc_ave'
   meta_nemsio%reclevtyp(48) = 'low cld lay'
!  loop through 3d variables	
   DO k = 1, nlevs
      meta_nemsio%recname(k+nvar2d)	       = 'ugrd'
      meta_nemsio%reclevtyp(k+nvar2d)         =  'mid layer'
      meta_nemsio%reclev(k+nvar2d)	       =  k
      meta_nemsio%recname(k+nvar2d+nlevs)     =  'vgrd'
      meta_nemsio%reclevtyp(k+nvar2d+nlevs)   =  'mid layer'
      meta_nemsio%reclev(k+nvar2d+nlevs)      =  k
      meta_nemsio%recname(k+nvar2d+nlevs*2)   =  'tmp'
      meta_nemsio%reclevtyp(k+nvar2d+nlevs*2) =  'mid layer'
      meta_nemsio%reclev(k+nvar2d+nlevs*2)    =  k
      meta_nemsio%recname(k+nvar2d+nlevs*3)   =  'spfh'
      meta_nemsio%reclevtyp(k+nvar2d+nlevs*3) =  'mid layer'
      meta_nemsio%reclev(k+nvar2d+nlevs*3)    =  k
      meta_nemsio%recname(k+nvar2d+nlevs*4)   =  'o3mr'
      meta_nemsio%reclevtyp(k+nvar2d+nlevs*4) =  'mid layer'
      meta_nemsio%reclev(k+nvar2d+nlevs*4)    =  k
      meta_nemsio%recname(k+nvar2d+nlevs*5)   =  'pres'
      meta_nemsio%reclevtyp(k+nvar2d+nlevs*5) =  'mid layer'
      meta_nemsio%reclev(k+nvar2d+nlevs*5)    =  k
      meta_nemsio%recname(k+nvar2d+nlevs*6)   =  'clwmr'
      meta_nemsio%reclevtyp(k+nvar2d+nlevs*6) =  'mid layer'
      meta_nemsio%reclev(k+nvar2d+nlevs*6)    =  k
      meta_nemsio%recname(k+nvar2d+nlevs*7)   =  'dpres'
      meta_nemsio%reclevtyp(k+nvar2d+nlevs*7) =  'mid layer'
      meta_nemsio%reclev(k+nvar2d+nlevs*7)    =  k
      if (nvar3d == 9) then
       meta_nemsio%recname(k+nvar2d+nlevs*8)   =  'vvel'
       meta_nemsio%reclevtyp(k+nvar2d+nlevs*8) =  'mid layer'
       meta_nemsio%reclev(k+nvar2d+nlevs*8)    =  k
      endif
   ENDDO

  end subroutine define_nemsio_meta

  subroutine nems_write_init(datapath,filename_base,meta_nemsio,gfile)
 
   
    implicit none

    type(nemsio_meta)                                                :: meta_nemsio
    character(len=200)                                               :: datapath
    character(len=100)                                               :: filename_base
    character(len=400)                                               :: filename
    type(nemsio_gfile)                                               :: gfile
    integer                                                          :: nemsio_iret
    integer                                                          :: i, j, k

    write(filename,500) trim(datapath)//'/'//trim(filename_base)
500 format(a,i3.3)
    print*,trim(filename)
    call nemsio_init(iret=nemsio_iret)
    print*,'iret=',nemsio_iret
    !gfile%gtype           = 'NEMSIO'
    meta_nemsio%gdatatype = 'bin4'
    call nemsio_open(gfile,trim(filename),'write',                                  &
         & iret=nemsio_iret,                                                        &
         & modelname=trim(meta_nemsio%modelname),                                   &
         & version=meta_nemsio%version,gdatatype=meta_nemsio%gdatatype,             &
         & dimx=meta_nemsio%dimx,dimy=meta_nemsio%dimy,                             &
         & dimz=meta_nemsio%dimz,rlon_min=meta_nemsio%rlon_min,                     &
         & rlon_max=meta_nemsio%rlon_max,rlat_min=meta_nemsio%rlat_min,             &
         & rlat_max=meta_nemsio%rlat_max,                                           &
         & lon=meta_nemsio%lon,lat=meta_nemsio%lat,                                 &
         & idate=meta_nemsio%idate,nrec=meta_nemsio%nrec,                           &
         & nframe=meta_nemsio%nframe,idrt=meta_nemsio%idrt,ncldt=                   &
         & meta_nemsio%ncldt,idvc=meta_nemsio%idvc,                                 &
         & nfhour=meta_nemsio%nfhour,nfminute=meta_nemsio%nfminute,                 &
         & nfsecondn=meta_nemsio%nfsecondn,nmeta=meta_nemsio%nmeta,                 &
         & nfsecondd=meta_nemsio%nfsecondd,extrameta=.true.,                        &
         & nmetaaryi=meta_nemsio%nmetaaryi,recname=meta_nemsio%recname,             &
         & nmetavari=meta_nemsio%nmetavari,variname=meta_nemsio%variname,           &
         & varival=meta_nemsio%varival,varrval=meta_nemsio%varrval,                 &
         & nmetavarr=meta_nemsio%nmetavarr,varrname=meta_nemsio%varrname,           &
         & reclevtyp=meta_nemsio%reclevtyp,                                         &
         & reclev=meta_nemsio%reclev,aryiname=meta_nemsio%aryiname,                 &
         & aryilen=meta_nemsio%aryilen)
    print*,'iret=',nemsio_iret
   end subroutine nems_write_init
 
  
!------------------------------------------------------  
  subroutine nems_write(gfile,recname,reclevtyp,level,dimx,data2d,iret)
 
  implicit none
    type(nemsio_gfile)         :: gfile
    integer                    :: iret,level,dimx
    real                       :: data2d(dimx)
    character(nemsio_charkind) :: recname, reclevtyp

     call nemsio_writerecv(gfile,recname,levtyp=reclevtyp,lev=level,data=data2d,iret=iret)
     if (iret.NE.0) then
         print*,'error writing',recname,level,iret
         STOP
     ENDIF

  end subroutine nems_write  
  
  
end module fv3_module
