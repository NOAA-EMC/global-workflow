!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                                                                              !
!  This program reads ocean and ice data on lat/lon grid                       !
!  It contains 4 subroutines: diag, read_ice, read_ocn                         !
!                                                                              !
!  Xingren Wu   (Xingren.Wu@noaa.gov)                                          !
!     May 18, 2007                                                             !
!  Xingren Wu                                                                  !
!     Nov 16, 2007 - modified                                                  !
!         fix kpds(13) to kpds(15) for handle different forecast time unit     !
!  Xingren Wu                                                                  !
!     Dec 4, 2007 - modified                                                   ! 
!        add extra fields to match production output                           !
!  Xingren Wu                                                                  !
!     Sep 7, 2016 - modified                                                   !
!        add cice                                                              !
!  Xingren Wu                                                                  !
!     Feb 10, 2017 - modified                                                  ! 
!        writing grib2                                                         !
!  Christopher Melhauser                                                       !
!     Sep 8, 2017 - overhauled code for latlon --> grb2 only                   !
!  Xingren Wu                                                                  !
!     Oct 23, 2017 - Bug/fix                                                   !
!  Suranjana Saha                                                              !
!     Nov 8, 2017 - Added new variables in the MOM6 NetCDF file to grib2       !
!                                                                              !
!  Xingren Wu                                                                  !
!     Nov 22, 2017 - Fixed the following                                       !
!            1. Ice concentration                                              !
!            2. Ice thickness                                                  !
!            3. Snow depth                                                     !
!            4. Surface Temperature over Water and Ice                         !
!            5. u-component of ice drift                                       !
!            6. v-component of ice drift                                       !
!     Mar 30, 2018 - Bug fix                                                   !
!                                                                              !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!  Notes:                                                                      !
!  1. uflx, vflx, lhtfl and shtfl are opposite in sign to data in the flx file !
!  2. nlwrs=net lw radiation at sfc i.e dlwrfavesfc-ulwrfavesfc flx file       !
!  3. nswrs=net sw radiation at sfc i.e. dswrfavesfc-uswrfavesfc in flx file   !
!  4. sfc_hflux (THFLX) = Total net heat, i.e.                                 !
!     dswrfavesfc-uswrfavesfc+dlwrfavesfc-ulwrfavesfc-lhtflsfc-shtflsfc in flx !
!  5. evp=evaporation i.e -lhtflsfc*0.03456 in flx file                        !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! Note that the input NetCDF and output grib2 files have the following:        !
! z=1 is topmost level; z=40 is bottom most level                              !
! even though grads control file puts z=1 at the bottom and z=40 at the top    !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

module regdiag_mod
use wgrib2api

      implicit none

contains
 
      subroutine read_ocn(im,jm,km,lon,lat, &
            t,s,u,v,eta,sfcflx,pme,mld,taux,tauy,ocean_file,  &
            sss,ssu,ssv,speed,sensible,latent,sw,lw,lprec,evap)
      include "netcdf.inc"
      integer im,jm,km,i,j,k
      integer status, ncid 
      integer :: id_lat,id_lon, &
                 id_temp,id_salt,id_u,id_v,id_wt,                       &             
                 id_eta_t,id_sfc_hflux,id_pme,id_mld,id_tau_x,id_tau_y, &
                 id_sss,id_ssu,id_ssv,id_speed,                         &
                 id_sensible,id_latent,id_sw,id_lw,id_lprec,id_evap

      real, dimension(im,jm,km) :: t,s,u,v
      real, dimension(im,jm)    :: eta,sfcflx,pme,mld,taux,tauy,LwLatSens
      real, dimension(im,jm)    :: sss,ssu,ssv,speed,sensible,latent,sw,lw
      real, dimension(im,jm)    :: lprec,evap
      real, dimension(im)       :: lon
      real, dimension(jm)       :: lat
      real, dimension(km,jm,im) :: tmp3D
      real, dimension(jm,im)    :: tmp2D
      character(len=300) :: ocean_file

      real, parameter :: c2k=273.15
      real :: undef

      undef=-1.0E+34
      status = nf_open(ocean_file, NF_NOWRITE, ncid)
      if(status.ne.NF_NOERR) stop 'cannot open ocean_file'

      status = nf_inq_varid(ncid, 'temp'    , id_temp)
      status = nf_inq_varid(ncid, 'so'      , id_salt)
      status = nf_inq_varid(ncid, 'uo'      , id_u   )
      status = nf_inq_varid(ncid, 'vo'      , id_v   )
      status = nf_inq_varid(ncid, 'SSH'     , id_eta_t)
      status = nf_inq_varid(ncid, 'LwLatSens',id_sfc_hflux)
      status = nf_inq_varid(ncid, 'Heat_PmE', id_pme)
      status = nf_inq_varid(ncid, 'mld'     , id_mld)
      status = nf_inq_varid(ncid, 'taux'    , id_tau_x)
      status = nf_inq_varid(ncid, 'tauy'    , id_tau_y)
      status = nf_inq_varid(ncid, 'lon'     , id_lon)
      status = nf_inq_varid(ncid, 'lat'     , id_lat)
      status = nf_inq_varid(ncid, 'SSS'     , id_sss)
      status = nf_inq_varid(ncid, 'SSU'     , id_ssu)
      status = nf_inq_varid(ncid, 'SSV'     , id_ssv)
      status = nf_inq_varid(ncid, 'speed'   , id_speed)
      status = nf_inq_varid(ncid, 'sensible', id_sensible)
      status = nf_inq_varid(ncid, 'latent'  , id_latent)
      status = nf_inq_varid(ncid, 'SW'      , id_sw)
      status = nf_inq_varid(ncid, 'LW'      , id_lw)
      status = nf_inq_varid(ncid, 'lprec'   , id_lprec)
      status = nf_inq_varid(ncid, 'evap'    , id_evap)

! read variable
      status = nf_get_var_real(ncid, id_lon,             lon)
      status = nf_get_var_real(ncid, id_lat,             lat)
      status = nf_get_var_real(ncid, id_temp,              t)     !temp
      status = nf_get_var_real(ncid, id_salt,              s)     !salt
      status = nf_get_var_real(ncid, id_u,                 u)     !u
      status = nf_get_var_real(ncid, id_v,                 v)     !v
      status = nf_get_var_real(ncid, id_eta_t,           eta)     !eta_t
      status = nf_get_var_real(ncid, id_sfc_hflux, LwLatSens)     !sfc_hflux
      status = nf_get_var_real(ncid, id_pme,             pme)     !pme
      status = nf_get_var_real(ncid, id_mld,             mld)     !mld
      status = nf_get_var_real(ncid, id_tau_x,          taux)     !tau_x
      status = nf_get_var_real(ncid, id_tau_y,          tauy)     !tau_y
      status = nf_get_var_real(ncid, id_sss,             sss)     !sss
      status = nf_get_var_real(ncid, id_ssu,             ssu)     !ssu
      status = nf_get_var_real(ncid, id_ssv,             ssv)     !ssv
      status = nf_get_var_real(ncid, id_speed,         speed)     !speed
      status = nf_get_var_real(ncid, id_sensible,   sensible)     !sensible
      status = nf_get_var_real(ncid, id_latent,       latent)     !latent
      status = nf_get_var_real(ncid, id_sw,               sw)     !sw
      status = nf_get_var_real(ncid, id_lw,               lw)     !lw
      status = nf_get_var_real(ncid, id_lprec,           lprec)   !lprec
      status = nf_get_var_real(ncid, id_evap,             evap)   !evap

!     status = nf_get_var_real(ncid, id_fprec,           fprec)   !fprec
!     status = nf_get_var_real(ncid, id_lrunoff,       lrunoff)   !lrunoff
!     status = nf_get_var_real(ncid, id_frunoff,       frunoff)   !frunoff

      sfcflx=SW+LwLatSens

! convert all temps in Celcius that are read in to Kelvin below..

      t=t+c2k

      return

      end subroutine read_ocn

      subroutine flip_ijk(im,jm,km,datain,dataout)
      implicit none
      integer :: im,jm,km,i,j,k
      real, intent(out), dimension(im,jm,km) :: dataout
      real, intent(in),  dimension(km,jm,im) :: datain
      do k=1,km
      do j=1,jm
      do i=1,im
        dataout(i,j,k)=datain(k,j,i)
      enddo
      enddo
      enddo 

      return
      end subroutine flip_ijk

      subroutine flip_ij(im,jm,datain,dataout)
      implicit none
      integer :: im,jm,i,j
      real, intent(out), dimension(im,jm) :: dataout
      real, intent(in),  dimension(jm,im) :: datain

      do j=1,jm
      do i=1,im
        dataout(i,j)=datain(j,i)
      enddo
      enddo

      return
      end subroutine flip_ij

      subroutine set_undef(im,jm,spv,undef,data)
      implicit none
      integer :: im,jm,i,j
      real    :: spv,undef
      real, intent(inout), dimension(im,jm) :: data

      do j=1,jm
      do i=1,im
        if (data(i,j) .GE. spv) data(i,j)=undef
      enddo
      enddo

      return
      end subroutine set_undef

      subroutine read_ice(im,jm,hi_h,hs_h,aice_h,ts_h,uvel_h,vvel_h,icefile)
      
      include "netcdf.inc"

      character(len=300) :: icefile
      integer :: status,ncid
      integer :: i,j
      integer :: im,jm
      integer :: id_hi_h,id_hs_h,id_tsfc_h,id_aice_h,id_sst_h,id_uvel_h,id_vvel_h

      real :: spv_ci,undef
      real, dimension(im,jm) :: hi_h,hs_h,Tsfc_h,aice_h,sst_h,ts_h,uvel_h,vvel_h,fi,fw

      spv_ci=1.0e+30
      undef=-1.0E+34
      print *, 'icefile:', icefile
      status = nf_open(icefile, NF_NOWRITE, ncid)
      print *, 'status:', status
      if(status.ne.NF_NOERR) stop 'cannot open ice_file'   

      status = nf_inq_varid(ncid, 'hi_h    ', id_hi_h)
      status = nf_inq_varid(ncid, 'hs_h    ', id_hs_h)
      status = nf_inq_varid(ncid, 'Tsfc_h  ', id_tsfc_h)
      status = nf_inq_varid(ncid, 'aice_h  ', id_aice_h)
      status = nf_inq_varid(ncid, 'sst_h   ', id_sst_h)
      status = nf_inq_varid(ncid, 'uvel_h  ', id_uvel_h)
      status = nf_inq_varid(ncid, 'vvel_h  ', id_vvel_h)

      status = nf_get_var_real(ncid, id_hi_h     ,hi_h)
      status = nf_get_var_real(ncid, id_hs_h     ,hs_h)
      status = nf_get_var_real(ncid, id_tsfc_h   ,Tsfc_h)
      status = nf_get_var_real(ncid, id_aice_h   ,aice_h)
      status = nf_get_var_real(ncid, id_sst_h    ,sst_h)
      status = nf_get_var_real(ncid, id_uvel_h   ,uvel_h)
      status = nf_get_var_real(ncid, id_vvel_h   ,vvel_h)

      do j=1,jm
      do i=1,im
         if (aice_h(i,j) < spv_ci) then
           if (aice_h(i,j) > 1.0) then
              print *,'Warning: aice_h>1:',aice_h(i,j)
              aice_h(i,j)=1.0
           endif
           fi(i,j)=aice_h(i,j)
         else
           fi(i,j)=0.0
         endif
         fw=1.0-fi(i,j)
         ts_h(i,j)=fw(i,j)*sst_h(i,j)+fi(i,j)*Tsfc_h(i,j)
      enddo
      enddo


      return
      end subroutine read_ice

      subroutine diag(im,jm,km,icefile,ocnfile,                  &
      isyr,ismth,isday,ishr,iyr,imth,iday,ihr,mfh,mfhout,        &
      flonw,flone,dlon,flatn,flats,dlat,imo,jmo,                 &
      outfile,mfcstcpl,igenocnp)

      integer nvar,ko,ki,kk,ndtc,mfcstcpl,igenocnp
!      integer mkmoc,nreg
      real    hfc

      parameter(nvar=37,ko=40,ki=5)
      parameter(ndtc=7)
!
      character*300 cicefile,icefile,ocnfile,outfile
!      character*120 mocfile
      character*120 template_file,template_inv,metadata,template_var
      character*10  datecode
      character*4   level4
      character*8   hr8
      character*80  levelm(nvar)
      character*80  levelcode,ftime
      character*5   varcode(nvar)

      integer im,jm,km
      integer imo,jmo
      integer iyr,imth,iday,ihr,mfh,mfhout
      integer isyr,ismth,isday,ishr
      integer hrdif
      integer isdate(8),iedate(8)
      real    dlat,dlon,flats,flatn,flonw,flone
!     real    tripolat,dtripolat
!     integer jtripolat
      real    factor,undef,spv_ci,spv_pme,spv_tau,val
      integer i,j,k,ierr,ilev,ind,iret,nv,ndata,nr,nundef,kreg
      integer nx,ny
      real    datedif(5)
!
      real*8, dimension(im,jm)     ::  hi,hs,ts,t1,t2,fi,alb,ui,vi,sst,saltf
!
      real, dimension(im,jm)     ::  hi_ci,hs_ci,ts_ci,fi_ci,ui_ci,vi_ci
!
      real, dimension(im,jm,km)  ::  t,s,u,v,w,vv
!      real, dimension(im,jm,km)  ::  dckt,dcks,vfc
      real, dimension(im,jm)     ::  eta,sfcflx,pme,mld,taux,tauy,uice,vice
      real, dimension(im,jm)     ::  sss,ssu,ssv,speed,sensible,latent,sw,lw
      real, dimension(im,jm)     ::  lprec,evap
      real, dimension(im,jm)     ::  varice
      real, dimension(im)        ::  lon
      real, dimension(jm)        ::  lat
!
      real, dimension(imo,jmo)   ::  grid,grdtmp,varsfc
      real, dimension(imo,jmo,km)  :: varocn,tocn,socn

      real zt(ko)
      real zw(ko)
      real dtc(ndtc)
      real grid2(imo*jmo)
!
      real,    dimension(nvar)  :: fac
      integer, dimension(nvar)  :: kpds5,kpds6,kpds7,kpds22
      integer, dimension(ko)    :: levs

      integer, parameter           :: kpds_dim=200
      integer, dimension(kpds_dim)  :: KPDS,KGDS,JPDS,JGDS
      logical*1 lbms(imo,jmo)
      logical :: climate = .false.
!     new wgrib2api requires this
      integer, parameter ::  regex=1
!
      data dtc/2.5,5.,10.,15.,20.,25.,28./
!
! NV  kpds5=Variable (Parameter Table)
! http://www.nco.ncep.noaa.gov/pmb/docs/on388/table2.html#TABLE128
!
! 1.  13=Potential temperature (2)
! 2.  88=Salinity (2)
! 3.  49=u-component of current (2)
! 4.  50=v-component of current (2)
! 5.  40=Geometric Vertical velocity (2)
! 6.  124=Momentum flux, u component (2)
! 7.  125=Momentum flux, v component (2)
! 8.  198=Sea Surface Height Relative to Geoid (129)
! 9.  91=Ice concentration (2)
! 10. 92=Ice thickness (2)
! 11. 66=Snow depth (2)
! 12. 11=Surface Temperature over Water and Ice(2)
! 13. 95=u-component of ice drift (2)
! 14. 96=v-component of ice drift (2)
! 15. 188=Evaporation - Precipitation (2)
! 16. 202=Total downward heat flux at surface (downward is positive) (129)
! 17. 195=Geometric Depth Below Sea Surface (129)
! 18. 195=Geometric Depth Below Sea Surface (129)
! 19. 197=Ocean Heat Content (129)
! 20. 194=Tropical Cyclone Heat Potential (129)
! 21. 195=Geometric Depth Below Sea Surface for the 2.5C isotherm (129)
! 22. 195=Geometric Depth Below Sea Surface for the 5C   isotherm (129)
! 23. 195=Geometric Depth Below Sea Surface for the 10C  isotherm (129)
! 24. 195=Geometric Depth Below Sea Surface for the 15C  isotherm (129)
! 25. 195=Geometric Depth Below Sea Surface for the 20C  isotherm (129)
! 26. 195=Geometric Depth Below Sea Surface for the 25C  isotherm (129)
! 27. 195=Geometric Depth Below Sea Surface for the 28C  isotherm (129)
! 28. 88=Sea Surface Salinity (2)
! 29. 49=Sea Surface u-current (2)
! 30. 50=Sea Surface v-current (2)
! 31. 32=Sea Surface speed (2)
! 32. 122=Sensible Heat (2)
! 33. 121=Latent Heat (2)
! 34. 111=Net surface Downward Short Wave flux (2)
! 35. 112=Net surface Downward Long Wave flux (2)
! 36. 59=Precipitation (2)
! 37. 57=Evaporation (2)
!
      data kpds5/   13, 88, 49, 50, 40,124,125,198, 91, 92, &
                    66, 11, 95, 96,188,202,195,195,197,194, &
                   195,195,195,195,195,195,195, 88, 49, 50, &
                    32,122,121,111,112, 59, 57/

      data kpds6/  160,160,160,160,160,  1,  1,  1,  1,  1, &
                     1,  1,  1,  1,  1,  1,237,238,236,239, &
                   235,235,235,235,235,235,235,  1,  1,  1, &
                     1,  1,  1,  1,  1,  1,  1/

      data kpds7/    0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                     0,  0,  0,  0,  0,  0,  0,  0, 30,260, &
                    25, 50,100,150,200,250,280,  0,  0,  0, &
                     0,  0,  0,  0,  0,  0,  0/
!-------------------------------------------------------------------
!  Xingren: check why fac=273.15 for Ice temperature
!           check why fac=-8.64e6 for evap minus precip 
!-------------------------------------------------------------------
      data fac/    0.0, 0.001,1.0,1.0,    1.0,    1.0,    1.0,1.0,1.0,1.0, &
                   1.0,273.15,1.0,1.0,-8.64e3,    1.0,    1.0,1.0,1.0,1.0, &
                   1.0,   1.0,1.0,1.0,    1.0,    1.0,    1.0,1.0,1.0,1.0, &
                   1.0,   1.0,1.0,1.0,    1.0,    1.0,    1.0/

      data kpds22/   2,  5,  3,  3,  9,  3,  3,  3,  3,  2, &
                     3,  2,  3,  3,  3,  3,  0,  0, -5, -4, &
                     0,  0,  0,  0,  0,  0,  0,  5,  3,  3, &
                     0,  0,  0,  0,  0,  3,  3/
!
      data levs/  5,  15,  25,  35,  45,  55,  65,  75, &
                 85,  95, 105, 115, 125, 135, 145, 155, &
                165, 175, 185, 195, 205, 215, 225, 238, &
                262, 303, 366, 459, 584, 747, 949,1193, &
               1479,1807,2174,2579,3016,3483,3972,4478/
!
      data spv_tau/-1.0E+5/
      data spv_ci/1.0E+29/
      data spv_pme/-1.0E+10/
      data undef/-1.0E+34/
!
      data zt/      5.,  15.,  25.,  35.,  45.,  55.,  65.,  75., &
                   85.,  95., 105., 115., 125., 135., 145., 155., &
                  165., 175., 185., 195., 205., 215., 225., 238., &
                  262., 303., 366., 459., 584., 747., 949.,1193., &
                 1479.,1807.,2174.,2579.,3016.,3483.,3972.,4478./
!
      data zw/     10.,  20.,  30.,  40.,  50.,  60.,  70.,  80., &
                   90., 100., 110., 120., 130., 140., 150., 160., &
                  170., 180., 190., 200., 210., 220., 232., 250., &
                  283., 335., 413., 522., 666., 848.,1072.,1337., &
                 1643.,1991.,2377.,2798.,3250.,3728.,4225.,4737./
!
      data levelm/' m below sea level', ' m below sea level',     &
                  ' m below sea level', ' m below sea level',     &
                  ' m below sea level',                           &
                  'surface', 'surface', 'surface', 'surface',     &
                  'surface', 'surface', 'surface', 'surface',     &
                  'surface', 'surface', 'surface',                &
                  'bottom of ocean mixed layer',                  &
                  'bottom of ocean isothermal layer',             &
                  '0-300 m ocean layer',                          &
            'layer ocean surface and 26C ocean isothermal level', &
                  '2.5C ocean isotherm',                          &
                  '5C ocean isotherm',                            &
                  '10C ocean isotherm',                           &
                  '15C ocean isotherm',                           &
                  '20C ocean isotherm',                           &
                  '25C ocean isotherm',                           &
                  '28C ocean isotherm',                           &
                  'surface',                                      &
                  'surface',                                      &
                  'surface',                                      &
                  'surface',                                      &
                  'surface',                                      &
                  'surface',                                      &
                  'surface',                                      &
                  'surface',                                      &
                  'surface',                                      &
                  'surface'/

      data varcode/'POT',   'SALTY', 'UOGRD', 'VOGRD', 'DZDT',    &
                   'UFLX',  'VFLX',  'SSHG',  'ICEC',  'ICETK',   &
                   'SNOD',  'TMP',   'UICE',  'VICE',  'EMNP',    &
                   'THFLX', 'DBSS',  'DBSS',  'OHC',   'TCHP',    &
                   'DBSS',  'DBSS',  'DBSS',  'DBSS',  'DBSS',    &
                   'DBSS',  'DBSS',                               &
                   'SALTY', 'UOGRD', 'VOGRD','SPEED','SHTFL',     &
                   'LHTFL', 'NSWRS', 'NLWRS','PRATE','EVP'/
!
        template_file = 'iceocnpost.g2'
        template_inv = '@mem:0'

!       find grid size to make sure
        iret = grb2_mk_inv(template_file, template_inv)
        if (iret.ne.0) stop 1

!       search using variable and regular date YYYYMMDDHH
        template_var = '^1:'
! Based on Wesley's suggestion
!       iret = grb2_inq(template_file,template_inv,template_var,nx=nx,ny=ny)
        iret = grb2_inq(template_file,template_inv,template_var,nx=nx,ny=ny,regex=regex)
        if (iret.ne.1) then
           if (iret.eq.0) write(*,*) 'could not find message'
           if (iret.gt.1) write(*,*) 'found multiple messages ', iret
           stop 2
        endif

!
        if (mfcstcpl.eq.1) climate = .true.
!
        print *,'im = ',im
        print *,'jm = ',jm
        print *,'km = ',km

        print *,'imo = ',imo
        print *,'jmo = ',jmo

        print *,'IGEN_OCNP = ',igenocnp
        print *,'mfcstcpl = ',mfcstcpl
        print *,'climate = ',climate

        isdate=0
        isdate(1)=isyr
        isdate(2)=ismth
        isdate(3)=isday
        isdate(5)=ishr
        print *,'isdate ',isdate

        iedate=0
        iedate(1)=iyr
        iedate(2)=imth
        iedate(3)=iday
        iedate(5)=ihr

        call w3difdat(iedate,isdate,2,datedif)
        hrdif=datedif(2)
        write(hr8,'(i8)') hrdif
!       print *,'datedif',datedif
       print *,'hrdif',hrdif

        write(datecode,'(i4.4,i2.2,i2.2,i2.2)') isyr, ismth, isday, ishr
        print*, 'datecode=',datecode

        call read_ocn(im,jm,km,lon,lat, &
            t,s,u,v,eta,sfcflx,pme,mld,taux,tauy,ocnfile, &
            sss,ssu,ssv,speed,sensible,latent,sw,lw,lprec,evap)
        print *,'after call read_ocn'
        call read_ice(im,jm,hi_ci,hs_ci,fi_ci,ts_ci,ui_ci,vi_ci,icefile)
        print *,'after call read_ice'
        uice(:,:)=ui_ci(:,:)
        vice(:,:)=vi_ci(:,:)

       do k=1,80
          ftime(k:k)=' '
       enddo
!      ftime='6 hour fcst'

       kpds(1)=7
       if (igenocnp.GT.0) then
          kpds(2)=igenocnp
       else
          kpds(2)=98
       endif
       kpds(3)=10
       kpds(4)=192
       kpds(8)=mod(isyr-1,100)+1
       kpds(9)=ismth
       kpds(10)=isday
       kpds(11)=ishr
       kpds(12)=0
       if (mfhout == 1) then
          kpds(13)=1
       else if (mfhout == 3) then
          kpds(13)=10
       else if (mfhout == 6) then
          kpds(13)=11
       else if (mfhout == 12) then
          kpds(13)=12
       else if (mfhout == 24) then
          kpds(13)=2
       else
         print *,'invalid mhout, must be one of (1 3 6 12 24).'
         stop
       endif
       if (climate) then
         kpds(13)=1
         kpds(14)=mfh
         print *,'kpds(14)=',kpds(14)
         print *,'mfhout=',mfhout
         kpds(15)=0
         kpds(16)=10
         ftime=hr8 // ' hour fcst'
         print *, 'ftime:', ftime
       else
         if (mfh > 1530) then
           kpds(13)=1
           kpds(14)=mfh
           kpds(15)=0
           kpds(16)=10
         else
           kpds(14)=mfh/mfhout-1
           kpds(15)=kpds(14)+1
           kpds(16)=3
         endif
       endif
       kpds(17)=0
       kpds(18)=1
       kpds(19)=2
       kpds(20)=0
       kpds(21)=((iyr-1)/100)+1
       kpds(23)=4
       kpds(24)=0
       kpds(25)=32

       print*,'kpds:',kpds(1:25)
!
       kgds(1)=0
       kgds(2)=imo
       kgds(3)=jmo
       kgds(4)=nint(flatn*1000.)
       kgds(5)=nint(flonw*1000.)
       kgds(6)=128
       kgds(7)=nint(flats*1000.)
       kgds(8)=nint(flone*1000.)
       kgds(9)=nint(dlon*1000.)
       kgds(10)=nint(dlat*1000.)
       kgds(11)=0
       kgds(12)=0
       kgds(13)=0
       kgds(14)=0
       kgds(15)=0
       kgds(16)=0
       kgds(17)=0
       kgds(18)=0
       kgds(19)=0
       kgds(20)=255
       kgds(21)=0
       kgds(22)=0
!
       print*,'kgds:',kgds(1:22)
!
       ndata=imo*jmo
!
       ind=0
       do nv=1,5
       factor=fac(nv)
       kpds(22)=kpds22(nv)
       print *,nv,' factor ',factor,' kpds22 ',kpds(22)

!temp/potdsl... 
      if (nv.eq.1) then
       varocn=t      
       do k=1,km
       do j=1,jmo
       do i=1,imo
          if (varocn(i,j,k).LE.undef) then
             tocn(i,j,k)=undef
          else
             tocn(i,j,k)=varocn(i,j,k)
          endif
       enddo
       enddo
       enddo
      endif

!salinity
      if (nv.eq.2) then
       varocn=s
       do k=1,km
       do j=1,jmo
       do i=1,imo
          socn(i,j,k)=varocn(i,j,k)
       enddo
       enddo
       enddo
      endif
!u-current
      if (nv.eq.3) then
       varocn=u
      endif
!v-current
      if (nv.eq.4) then
       varocn=v
      endif
!-------------------------------------------------------------------
!  Xingren: 
!     w vertical velocity (not present in raw NetCDF file)
!     so how can you compute the vertical velocity with the program below?
!-------------------------------------------------------------------
      if (nv.eq.5) then
        varocn=w
        do k=1,km-1
           kk=km-k+1
           do j=1,jmo
           do i=1,imo
             if (varocn(i,j,k).LE.undef) then
                varocn(i,j,k)=varocn(i,j,k+1)
             else
                varocn(i,j,k)=(varocn(i,j,k+1)*(zw(kk)-zt(kk))  &
                       +varocn(i,j,k)*(zt(kk)-zw(kk-1)))/(zw(kk)-zw(kk-1))
             endif
           enddo
           enddo
        enddo
     continue 
     endif

      do k=1,km
       ind=ind+1
       ilev=levs(k)

!   flip N-S
       do j=1,jmo
        grdtmp(:,j)=varocn(:,jmo-j+1,k)
       enddo

!   make bit-map....
       do j=1,jmo
       do i=1,imo
          val=grdtmp(i,j)
          if (val.eq.undef) then
             lbms(i,j) = .false.
             grid2(i+(j-1)*nx)=9.999E+20
          else
             if (nv.eq.1) then
                grid(i,j)=val+factor
             else
                grid(i,j)=val*factor
             endif
             lbms(i,j) = .true.
             grid2(i+(j-1)*nx)=grid(i,j)
          endif
       enddo
       enddo

       print *,' record written ',ind,nv,k,grid(92,125),lbms(92,125)
       kpds(5)=kpds5(nv)
       kpds(6)=kpds6(nv)
       kpds(7)=ilev

       write(level4,'(i4)') ilev
       levelcode=level4 // levelm(nv)
       print *, 'levelcode= ', levelcode
       print *, 'trim(ftime):', trim(ftime)
       metadata='d=' // datecode // ':' // trim(varcode(nv)) // ':' // trim(levelcode) // ':' // trim(ftime) // ':'
       iret = grb2_wrt(outfile,template_file,1,data1=grid2,meta=metadata)
       write(*,*) iret
!
!.. end level-loop
      enddo
!.. end variable-loop
      enddo

!... now read and grib 5 surface records...
!
       do nv=6,nvar
       levelcode=levelm(nv)
       print *, 'levelcode= ', levelcode
       print *, 'process data for nv=',nv
       factor=fac(nv)
       kpds(22)=kpds22(nv)
       print *,nv,' factor ',factor,' kpds22 ',kpds(22)
       ind=ind+1
       kpds(5)=kpds5(nv)
       kpds(6)=kpds6(nv)
       kpds(7)=kpds7(nv)
       if (nv .EQ. 8 .or. (nv.GE.16 .AND. nv.LE.27)) then
          kpds(19)=129
       else if (nv .EQ. 15) then
          kpds(19)=128
       else
          kpds(19)=2
       endif
! taux
      if (nv.eq.6) then
       varsfc=taux
       do j=1,jmo
       do i=1,imo
          if (varsfc(i,j) .LE. spv_tau) varsfc(i,j)=undef
       enddo
       enddo
      endif
! tauy
      if (nv.eq.7) then
       varsfc=tauy
       do j=1,jmo
       do i=1,imo
          if (varsfc(i,j) .LE. spv_tau) varsfc(i,j)=undef
       enddo
       enddo
      endif
! eta
      if (nv.eq.8) then
       varsfc=eta
      endif
! fi
      if (nv.eq.9) then
       varsfc=fi_ci
       call set_undef(im,jm,spv_ci,undef,varsfc)
      endif
! hi
      if (nv.eq.10) then
       varsfc=hi_ci
       call set_undef(im,jm,spv_ci,undef,varsfc)
      endif
! hs
      if (nv.eq.11) then
       varsfc=hs_ci
       call set_undef(im,jm,spv_ci,undef,varsfc)
      endif
! ts
      if (nv.eq.12) then
       varsfc=ts_ci
       call set_undef(im,jm,spv_ci,undef,varsfc)
      endif
! uice
      if (nv.eq.13) then
       varsfc=uice
       call set_undef(im,jm,spv_ci,undef,varsfc)
      endif
! vice
      if (nv.eq.14) then
       varsfc=vice
       call set_undef(im,jm,spv_ci,undef,varsfc)
      endif
! pme
      if (nv.eq.15) then
       varsfc=lprec-evap !pme
       do j=1,jm
       do i=1,im
!          if (varsfc(i,j) .LE. spv_pme) varsfc(i,j)=undef
          if (lprec(i,j) .LE. spv_pme) varsfc(i,j)=undef
       enddo
       enddo
      endif
! sfc_flx
      if (nv.eq.16) then
       varsfc=sfcflx
       do j=1,jm
       do i=1,im
          if (varsfc(i,j) .LE. undef) varsfc(i,j)=undef
       enddo
       enddo
      endif
! mld
      if (nv.eq.17) then
!      call mixed_layer(imo,jmo,km,tocn,socn,zt,varsfc,undef)
       varsfc=mld
       do j=1,jm
       do i=1,im
        if (varsfc(i,j) .LE. undef) varsfc(i,j)=undef
       enddo
       enddo
      endif
! sfc isothm layer depth
      if (nv.eq.18) then
       call sfc_isothm_layer(imo,jmo,km,tocn,zt,varsfc,undef)
      endif
! ocean heat content
      if (nv.eq.19) then
       call ocean_heat(imo,jmo,km,tocn,socn,zw,zt,varsfc,undef)
      endif
! tropical cyc heat potential
      if (nv.eq.20) then
       call tchp26(imo,jmo,km,tocn,socn,zw,zt,varsfc,undef)
      endif
! depth of 7 different isotherms...
      if (nv.ge.21 .AND. nv.le.27) then
       i=nv-20
       call isothm_layer(imo,jmo,km,dtc(i),tocn,zt,varsfc,undef)
      endif
! sea surface salinity
      if (nv.eq.28) then
       varsfc=sss
       do j=1,jm
       do i=1,im
        if (varsfc(i,j) .LE. undef) varsfc(i,j)=undef
       enddo
       enddo
      endif
! sea surface u-current
      if (nv.eq.29) then
       varsfc=ssu
       do j=1,jm
       do i=1,im
        if (varsfc(i,j) .LE. undef) varsfc(i,j)=undef
       enddo
       enddo
      endif
! sea surface v-current
      if (nv.eq.30) then
       varsfc=ssv
       do j=1,jm
       do i=1,im
        if (varsfc(i,j) .LE. undef) varsfc(i,j)=undef
       enddo
       enddo
      endif
! sea surface speed
      if (nv.eq.31) then
       varsfc=speed
       do j=1,jm
       do i=1,im
        if (varsfc(i,j) .LE. undef) varsfc(i,j)=undef
       enddo
       enddo
      endif
! sensible heat
      if (nv.eq.32) then
       varsfc=sensible
       do j=1,jm
       do i=1,im
        if (varsfc(i,j) .LE. undef) varsfc(i,j)=undef
       enddo
       enddo
      endif
! latent heat
      if (nv.eq.33) then
       varsfc=latent
       do j=1,jm
       do i=1,im
        if (varsfc(i,j) .LE. undef) varsfc(i,j)=undef
       enddo
       enddo
      endif
! net downward shortwave radiation at the surface
      if (nv.eq.34) then
       varsfc=sw
       do j=1,jm
       do i=1,im
        if (varsfc(i,j) .LE. undef) varsfc(i,j)=undef
       enddo
       enddo
      endif
! net downward longwave radiation at the surface
      if (nv.eq.35) then
       varsfc=lw
       do j=1,jm
       do i=1,im
        if (varsfc(i,j) .LE. undef) varsfc(i,j)=undef
       enddo
       enddo
      endif
! precipitation rate
      if (nv.eq.36) then
       varsfc=lprec
       do j=1,jm
       do i=1,im
        if (varsfc(i,j) .LE. undef) varsfc(i,j)=undef
       enddo
       enddo
      endif
! evaporation
      if (nv.eq.37) then
       varsfc=evap
       do j=1,jm
       do i=1,im
        if (varsfc(i,j) .LE. undef) varsfc(i,j)=undef
       enddo
       enddo
      endif

       nundef=0
       do j=1,jmo
          grdtmp(:,j)=varsfc(:,jmo-j+1)
       enddo
       do j=1,jmo
       do i=1,imo
          val=grdtmp(i,j)
          if (val.eq.undef) then
             lbms(i,j) = .false.
             nundef=nundef+1
             grid2(i+(j-1)*nx)=9.999E+20
          else
             if (nv.EQ.12) then
                grid(i,j)=val+factor
             else
                grid(i,j)=val*factor
             endif
             lbms(i,j) = .true.
             grid2(i+(j-1)*nx)=grid(i,j)
          endif
       enddo
       enddo
       if(nundef.eq.imo*jmo) then
        print *,' record not written because of all undef ', &
        ind,kpds(5),kpds(6),kpds(7)
       else
        print *,' record written ',ind,kpds(5),kpds(6),kpds(7), &
        grid(92,125),lbms(92,125),factor
       endif
       print *,'nv= ', nv
       metadata='d=' // datecode // ':' // trim(varcode(nv)) // ':' // trim(levelcode) // ':' // trim(ftime) // ':'
       iret = grb2_wrt(outfile,template_file,1,data1=grid2,meta=metadata)
       write(*,*) iret

       enddo

      return
      end subroutine diag

      subroutine isothm_layer(im,jm,km,dtc,temp,zlev,zisothm,undef)

      real, parameter :: c2k=273.15
      integer inumc,im,jm,km
      integer i,j,k
      real  dtc
      real, dimension(km) :: tz,zlev
      real, dimension(im,jm) :: zisothm
      real, dimension(im,jm,km) :: temp
      real  a,b,tc,undef

      tc=dtc+c2k

      do j=1,jm
      do i=1,im

         zisothm(i,j)=undef
         if (temp(i,j,1) .GE. tc) then
            do k=1,km
               tz(k)=temp(i,j,k)
            enddo
            do k=2,km
               if (tz(k) .LT. -3.0) go to 111
               if (tz(k) .LT. tc) then
                  a = (tz(k)-tc) / (tz(k)-tz(k-1))
                  b = (tc-tz(k-1)) / (tz(k)-tz(k-1))
                  zisothm(i,j)=a*zlev(k-1)+b*zlev(k)
                  go to 111
               endif
            enddo
         endif
 111     continue
      enddo
      enddo

      return
      end subroutine isothm_layer

      subroutine mixed_layer(im,jm,km,temp,salt,zlev,mld,undef)

      real, parameter :: disot=0.8, c2k=273.15
      integer im,jm,km
      integer i,j,k,kmsk,kbm,kbp,krf
      real, dimension(km) :: zlev,plev,sa,ta,th,rho
      real, dimension(im,jm) :: mld
      real, dimension(im,jm,km) :: temp,salt
      real  a,b,deltarho,dr,rb,undef

      do k=1,km
         plev(k) = press(zlev(k),980.0)
      enddo

      do j=1,jm
      do i=1,im
         kmsk = 0
         do k=1,km
            if (temp(i,j,k).GT.0.0 .AND. salt(i,j,k).GT.0.0) then
               ta(k) = temp(i,j,k)-c2k
               sa(k) = salt(i,j,k)
               kmsk = k
            endif
         enddo

         if (kmsk.EQ.0 .OR. ta(1).LT.-3.0) then
            mld(i,j)=undef
         else
            deltarho = (density(0.0,ta(1)-disot,sa(1)) &
                      - density(0.0,ta(1),sa(1)))
            do k=1,kmsk
               th(k) = theta(plev(k),ta(k),sa(k),0.0)
               rho(k) = density(0.0,th(k),sa(k)) - 1000.0
            enddo
            krf = 1
            kbm = 0
            kbp = 0
            do k = krf,kmsk
               if ((rho(k)-rho(krf)) .GE. deltarho) then
                   kbp = k
                    exit
               endif
            enddo
            if (kbp .LE. 1) then
               mld(i,j) = undef
            else
               kbm = kbp - 1
               rb = rho(krf) + deltarho
               dr = rho(kbp) - rho(kbm)
               a = (rho(kbp) - rb) / dr
               b = (rb - rho(kbm)) / dr
               mld(i,j) = zlev(kbm)*a + zlev(kbp)*b
            endif
         endif
      enddo
      enddo

      end subroutine mixed_layer

      subroutine sfc_isothm_layer(im,jm,km,temp,zlev,sitd,undef)

      real, parameter :: disot=0.8
      integer im,jm,km
      integer i,j,k
      real, dimension(im,jm) :: sitd
      real, dimension(im,jm,km) :: temp
      real, dimension(km) :: zlev,tz
      real  a,b,tc,undef

      do j=1,jm
      do i=1,im

         sitd(i,j)=undef

         if (temp(i,j,1).GE.0.0) then
            tc=temp(i,j,1)-disot
            do k=1,km
               tz(k)=temp(i,j,k)
            enddo
            do k=2,km
               if (tz(k).LT.0.0) go to 112
               if (tz(k).LT.tc) then
                  a = (tz(k)-tc) / (tz(k)-tz(k-1))
                  b = (tc-tz(k-1)) / (tz(k)-tz(k-1))
                  sitd(i,j) = a*zlev(k-1) + b*zlev(k)
                  go to 112
               endif
            enddo
         endif

 112     continue

      enddo
      enddo

      return
      end subroutine sfc_isothm_layer

      subroutine ocean_heat(im,jm,km,temp,salt,zblev,zlev,ocnhc,undef)

      integer, parameter :: kmh=26
      real, parameter :: c2k=273.15
      integer im,jm,km
      integer i,j,k
      real, dimension(km) :: zblev,zlev,plev
      real, dimension(im,jm) :: ocnhc
      real, dimension(im,jm,km) :: salt,temp
      real  dptlyr,rk,sk,tk,undef
      real  pk,rhm,rhp,tempk,zk

      k=kmh
      zk=0.5*(300.0+zblev(k-1))
      pk=press(zk,980.0)
      rhm = (zk-zlev(k-1))/(zlev(k)-zlev(k-1))
      rhp = (zlev(k)-zk)/(zlev(k)-zlev(k-1))

      do k=1,km
         plev(k) = press(zlev(k),980.0)
      enddo

      do j=1,jm
      do i=1,im

         ocnhc(i,j)=undef

         if (temp(i,j,kmh).GT.0.0 .AND. salt(i,j,kmh).GT.0.0) then
            ocnhc(i,j)=0.
            do k=1,kmh-1
               tk=temp(i,j,k)-c2k
!              print *,'tk= ',tk
               sk=salt(i,j,k)
               rk=density(plev(k),tk,sk)
               if (k .eq. 1) then
                  dptlyr=zblev(k)
               else
                  dptlyr=zblev(k)-zblev(k-1)
               endif
               ocnhc(i,j)=ocnhc(i,j) + rk*temp(i,j,k)*dptlyr
            enddo
            k=kmh
            tempk=rhp*temp(i,j,k-1) + rhm*temp(i,j,k)
            tk=tempk - c2k
            sk=rhp*salt(i,j,k-1) + rhm*salt(i,j,k)
            rk=density(pk,tk,sk)
            dptlyr=300.0-zblev(k-1)
            ocnhc(i,j)=ocnhc(i,j)+rk*tempk*dptlyr
            ocnhc(i,j)=ocnhc(i,j)*3996.
         endif
      enddo
      enddo

      return
      end subroutine ocean_heat

      subroutine tchp26(im,jm,km,temp,salt,zblev,zlev,ocnhcp,undef)

      real, parameter :: c2k=273.15, t26=26.0
      integer im,jm,km
      integer i,j,k,k26
      real, dimension(km) :: zblev,zlev,plev,tz
      real, dimension(im,jm) :: ocnhcp,z26isothm
      real, dimension(im,jm,km) :: salt,temp
      real  dptlyr,rk,sk,tk,undef
      real  rhm,rhp,pk,zk
      real  a,b,skk,skm,tc,z26
      logical*1 lbms(im,jm)

      tc=c2k+t26

      do k=1,km
         plev(k) = press(zlev(k),980.0)
      enddo

      do j=1,jm
      do i=1,im

         z26isothm(i,j)=undef
         lbms(i,j)=.false.
         if (temp(i,j,1) .GE. tc) then
            do k=1,km
               tz(k)=temp(i,j,k)
            enddo
            k = 1
            do while (tz(k).GE.tc)
               k26 = k
               if (k.EQ.km) exit
               k = k + 1
            enddo
            k = k26
            if (tz(k) .GT. tc) then
               if (k.LT.km .AND. tz(k+1).GT.0.0) then
                  k = k + 1
                  a = (tz(k)-tc) / (tz(k)-tz(k-1))
                  b = (tc-tz(k-1)) / (tz(k)-tz(k-1))
                  z26isothm(i,j) = a*zlev(k-1) + b*zlev(k)
                  lbms(i,j)=.true.
               else if (k.GE.2 .AND. tz(k).LT.tz(k-1)) then
                  a = (tz(k)-tc) / (tz(k)-tz(k-1))
                  b = (tc-tz(k-1)) / (tz(k)-tz(k-1))
                  z26 = a*zlev(k-1) + b*zlev(k)
                  if (z26.LE.zblev(k)) then
                     z26isothm(i,j) = z26
                     lbms(i,j)=.true.
                  endif
              endif
            else if (tz(k).EQ.tc) then
               z26isothm(i,j) = zlev(k)
               lbms(i,j)=.true.
            endif
         endif

      enddo
      enddo

!
!---------- get ocean heat potential relative to 26C (TCHP) ------------
!
      do j=1,jm
      do i=1,im

         if (temp(i,j,1) .GT. 0.0) then
            ocnhcp(i,j)=0.0
         else
            ocnhcp(i,j)=undef
            cycle
         endif

         if (lbms(i,j)) then   ! we have water above 26c

            z26 = z26isothm(i,j)
!
!  case where Z26 is within the topmost layer
!
            if (z26 .LE. zblev(1)) then
               tk=temp(i,j,1)-c2k
               if (salt(i,j,1) .GT. 0.0) then
                   sk=salt(i,j,1)
               else
                   sk=35.   ! fake salinity
               endif
               rk=density(plev(1),tk,sk)
               dptlyr=z26
               ocnhcp(i,j) = rk*(tk-t26)*dptlyr*3996.
!
!  case where z26 is below the top layer and above the bottom
!
            else
               k26 = 1
               do k=2,km
                  if (z26.GT.zblev(k-1) .AND.  z26.LE.zblev(k)) k26=k
               enddo

               ocnhcp(i,j)=0.0
               do k=1,k26-1
                  tk=temp(i,j,k)-c2k
                  if (salt(i,j,K) .GT. 0.0) then
                     sk=salt(i,j,k)
                  else
                     sk=35.   ! fake salinity
                  endif
                  rk=density(plev(k),tk,sk)
                  if (k .EQ. 1) then
                     dptlyr=zblev(1)
                  else
                     dptlyr=zblev(k)-zblev(k-1)
                  endif
                  ocnhcp(i,j)=ocnhcp(i,j)+rk*(tk-26.0)*dptlyr
               enddo
               k=k26
               zk=0.5*(z26+zblev(k-1))
               pk=press(zk,980.0)
               rhm = (zk-zlev(k-1))/(zlev(k)-zlev(k-1))
               rhp = (zlev(k)-zk)/(zlev(k)-zlev(k-1))
               tk=rhp*temp(i,j,k-1) + rhm*temp(i,j,k) - c2k
               if (salt(i,j,k-1) .GT. 0.0) then
                  skm=salt(i,j,k-1)
               else
                  skm=35.   ! fake salinity
               endif
               if (salt(i,j,k) .GT. 0.0) then
                  skk=salt(i,j,k)
               else
                  skk=35.   ! fake salinity
               endif
               sk=(rhp*skm + rhm*skk)
               rk=density(pk,tk,sk)
               dptlyr=z26-zblev(k-1)
               ocnhcp(i,j)=ocnhcp(i,j)+rk*(tk-26.0)*dptlyr
               ocnhcp(i,j)=ocnhcp(i,j)*3996.
            endif
!
!  case where temperature is above 26C down to the bottom
!
         else if ((temp(i,j,1)-c2k) .GT. t26) then
            ocnhcp(i,j)=0.0
            do k=1,km
               if (temp(i,j,k) .GT. undef) then
                  tk=temp(i,j,k)-c2k
                  if (salt(i,j,k) .GT. 0.0) then
                     sk=salt(i,j,k)
                  else
                     sk=35.   ! fake salinity
                  endif
                  rk=density(plev(k),tk,sk)
                  if (k .EQ. 1) then
                     dptlyr=zblev(1)
                  else
                     dptlyr=zblev(k)-zblev(k-1)
                  endif
                  ocnhcp(i,j)=ocnhcp(i,j)+rk*(tk-26.0)*dptlyr
               endif
            enddo
            ocnhcp(i,j)=ocnhcp(i,j)*3996.
         endif

      enddo
      enddo

      return
      end subroutine tchp26
 
      function press(z, g)

!   copy from cfs_ocean_time.f and modified
!   depth (z) in meters and grav acc'l (g) in cm/sec**2

      integer, parameter :: itr=20
      integer i
      real p, a0, z, g, press
      real(kind=8) :: e, ae, es
!
      p = z*(1.0076+z*(2.3487e-6-z*1.2887e-11))
      e = zeta(p,g)-z
      ae = abs(e)
      es = ae*2.
      do i = 1,itr
        a0 = 0.972643+p*(1.32696e-5-p*(6.228e-12+p*1.885e-16))
        a0 = a0/(1.0+1.83e-5*p)
        p = p-((g+1.113e-4*p)/a0)*e*0.001
        es = ae
        e = zeta(p,g)-z
        ae = abs(e)
        if (ae .le. 0.01) exit
      enddo
!
      press = p
!
      end function press

      function zeta(p, glat)
!
!   copy from cfs_ocean_time.f and modified

      real p, glat, z, zeta

      z = ((-3.434e-12*p+1.113e-7)*p+0.712953)*p+14190.7*log(1.0+1.83e-5*p)
      z = (z/(glat+1.113e-4*p))*1000.

      zeta = z
!
      end function zeta

      function density(prs, tmp, sal)
!
!   copy from cfs_ocean_time.f and modified
!     Density is in units of kg/m**3  (1 g/cm**3 = 1000 kg/m**3)

      real density, prs, tmp, sal
      real p, t, s, kstp, k0, kw, d0, dw
!
      s = sal
      t = tmp
      p = prs/10.00
!
      kw = 19652.21+(148.4206-(2.327105-(1.360477e-2-5.155288e-5*t)*t)*t)*t
!
      k0 = kw+s*(54.6746-(0.603459-(1.09987e-2-6.1670e-5*t)*t)*t)   &
             +sqrt(s*s*s)*(7.944e-2+(1.6483e-2-5.3009e-4*t)*t)
!
      kstp = k0+p*((3.239908+(1.43713e-3+(1.16092e-4-5.77905e-7*t)*t)*t) &
             +s*(2.2838e-3-(1.0981e-5+1.6078e-6*t)*t)                    &
             +sqrt(s*s*s)*1.91075e-4                                     &
             +p*((8.50935e-5-(6.12293e-6-5.2787e-8*t)*t)                 &
             -s*(9.9348e-7-(2.0816e-8+9.1697e-10*t)*t))) 
!
      dw = 999.842594+(6.793952e-2-(9.095290e-3-(1.001685e-4  &
             -(1.120083e-6-6.536332e-9*t)*t)*t)*t)*t
!
      d0 = dw+s*(0.824493-(4.0899e-3-(7.6438e-5-(8.2467e-7        &
             -5.3875e-9*t)*t)*t)*t)                               &
             -sqrt(s*s*s)*(5.72466e-3-(1.0227e-4-1.6546e-6*t)*t)  &
             +s*s*4.8314e-4
!
      density = d0/(1.0-p/kstp)

      end function density

      function theta(p, t, s, pref)

      real(kind=8), parameter :: sqrt2 = 0.7071067811865475
      real theta, p,t, s, pref
      real del_p, del_t1, del_t2, del_t3, del_t4, tp, th

      del_p = pref-p
      del_t1 = del_p*atg(p,t,s)
      tp = t+0.5*del_t1
      del_t2 = del_p*atg((p+0.5*del_p),tp,s)
      tp = t+(-0.5+sqrt2)*del_t1+(1.0-sqrt2)*del_t2
      del_t3 = del_p*atg((p+0.5*del_p),tp,s)
      tp = t-sqrt2*del_t2+(1.0+sqrt2)*del_t3
      del_t4 = del_p*atg(pref,tp,s)
      th = (t+(del_t1+(1.0-sqrt2)*del_t2*2.0 &
         + (1.0+sqrt2)*del_t3*2.0+del_t4)/6.0)
      theta = th

      end function theta

      function atg(p, t, s)

      real atg, p, t, s, ds, a

      ds = s-35.0
      a = (((-2.1687e-16*t+1.8676e-14)*t-4.6206e-13)*p           &
             +((2.7759e-12*t-1.1351e-10)*ds+((-5.4481e-14*t      & 
             +8.733e-12)*t-6.7795e-10)*t+1.8741e-8))*p           &
             +(-4.2393e-8*t+1.8932e-6)*ds                        &
             +((6.6228e-10*t-6.836e-8)*t+8.5258e-6)*t+3.5803e-5

      atg = a

      end function atg


      end module regdiag_mod
