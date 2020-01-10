      subroutine meteorg(npoint,rlat,rlon,istat,cstat,elevstn,
     &             nf,nfile,fnsig,jdate,idate,
     &       levso,levs,im,jm,kdim,
     &       landwater,nend1,nint1,nint3,iidum,jjdum,fformat)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    meteorg                           
!   PRGMMR: HUALU PAN        ORG: W/NMC23    DATE: 1999-07-21
!
! ABSTRACT: Creates BUFR meteogram files for the AVN and MRF.
!
! PROGRAM HISTORY LOG:
!   1999-07-21  HUALU PAN                            
!   2007-02-02  FANGLIN YANG  EXPAND FOR HYBRID COORDINATES USING SIGIO 
!   2009-07-24  FANGLIN YANG  CHANGE OUTPUT PRESSURE TO INTEGER-LAYER 
!                             PRESSURE (line 290)
!                             CORRECT THE TEMPERATURE ADJUSTMENT (line 238)
!   2014-03-27  DANA CARLIS   UNIFY CODE WITH GFS FORECAST MODEL PRECIP
!                             TYPE CALCULATION
!   2016-09-27  HUIYA CHUANG  MODIFY TO READ GFS NEMS OUTPUT ON GRID SPACE
!   2017-02-27  GUANG PING LOU CHANGE OUTPUT PRECIPITATION TO HOURLY AMOUNT
!                              TO 120 HOURS AND 3 HOURLY TO 180 HOURS.
!   2018-02-01  GUANG PING LOU INGEST FV3GFS NEMSIO ACCUMULATED PRECIPITATION 
!                              AND RECALCULATE HOURLY AND 3 HOURLY OUTPUT DEPENDING
!                               ON LOGICAL VALUE OF precip_accu. 
!   2018-02-08  GUANG PING LOU ADDED READING IN AND USING DZDT AS VERTICAL VELOCITY
!   2018-02-16  GUANG PING LOU ADDED READING IN AND USING MODEL DELP AND DELZ
!   2018-02-21  GUANG PING LOU THIS VERSION IS BACKWARD COMPATIBLE TO GFS MODEL
!   2018-03-27  GUANG PING LOU CHANGE STATION ELEVATION CORRECTION LAPSE RATE FROM 0.01 TO 0.0065
!   2018-03-28  GUANG PING LOU GENERALIZE TIME INTERVAL 
!   2019-07-08  GUANG PING LOU ADDED STATION CHARACTER IDS
!   2019-10-08  GUANG PING LOU MODIFY TO READ IN NetCDF FILES. REMOVE NEMSIO
!                              RELATED CALLS AND CLEAN UP THE CODE.
!
! USAGE:    CALL PROGRAM meteorg
!   INPUT:
!     npoint           - number of points
!     rlat(npint)      - latitude
!     rlon(npoint)      - longtitude
!     istat(npoint)    - station id
!     elevstn(npoint)  - station elevation (m)
!     nf               - forecast cycle         
!     fnsig            - sigma file name        
!     idate(4)         - date                   
!     levso            - output vertical layers
!     levs             - input vertical layers
!     kdim             - sfc file dimension         
!
!   OUTPUT:    
!     nfile            - output data file channel       
!     jdate            - date YYYYMMDDHH       
!
! ATTRIBUTES:
!   LANGUAGE: 
!   MACHINE:  IBM SP
!
!$$$
      use netcdf
      use nemsio_module
      use sigio_module 
      use physcons
      use mersenne_twister
      use funcphys
      implicit none 
      include 'mpif.h'
      type(nemsio_gfile) :: gfile
      type(nemsio_gfile) :: ffile
      type(nemsio_gfile) :: ffile2
      integer :: nfile,npoint,levso,levs,kdim
      integer :: nfile1
      integer :: i,j,im,jm,kk,idum,jdum,idvc,idsl
!     idsl              Integer(sigio_intkind) semi-lagrangian id
!     idvc              Integer(sigio_intkind) vertical coordinate id
!                       (=1 for sigma, =2 for ec-hybrid, =3 for ncep hybrid)
      integer,parameter :: nvcoord=2 
      integer :: idate(4),nij,nflx,np,k,l,nf,nfhour,np1
      integer :: idate_nems(7)
      integer :: iret,jdate,leveta,lm,lp1
      integer :: ie,iw,jn,js
      character*150 :: fnsig,fngrib
      real*8 :: data(6*levso+25)
      real*8 :: rstat1
      character*8 :: cstat1
      character*4 :: cstat(npoint)
      real :: fhour,pp,ppn,qs,qsn,esn,es,psfc,ppi,dtemp,iwx,nd
      real :: t,q,u,v,td,tlcl,plcl,qw,tw,xlat,xlon,iossil
      real :: dx,dy
      integer,dimension(npoint):: landwater
      integer,dimension(im,jm):: lwmask
      real,dimension(im,jm)::  apcp, cpcp
      real,dimension(npoint,2+levso*3):: grids,gridsi
      real,dimension(npoint) :: rlat,rlon,pmsl,ps,psn,elevstn
      real,dimension(im*jm) :: dum1d,dum1d2
      real,dimension(im,jm) :: gdlat, hgt, gdlon
      real,dimension(im,jm,15) :: dum2d
      real,dimension(im,jm,levs) :: t3d, q3d, uh, vh,omega3d
      real,dimension(im,jm,levs) :: delp,delz,dummy3d
      real,dimension(im,jm,levs+1) :: pint, zint
      real,dimension(npoint,levso) :: gridu,gridv,omega,qnew,zp
      real,dimension(npoint):: gradx, grady
      real,dimension(npoint,levs) :: griddiv,gridui,gridvi,omegai
      real,dimension(npoint,levso) :: p1,p2,p3,pd1,pd2,pd3,tt,ttnew
      real,dimension(npoint,levso) :: z1
      real,dimension(npoint,levso+1) :: pi3
      real :: zp2(2)
      real,dimension(kdim,npoint) :: sfc
      real,dimension(1,levso+1) :: prsi,phii
      real,dimension(1,levso) ::  gt0,gq0,prsl,phy_f3d
      real :: PREC,TSKIN,SR,randomno(1,2)
      real :: DOMR,DOMZR,DOMIP,DOMS
      real :: vcoord(levs+1,nvcoord),vdummy(levs+1)
      real :: vcoordnems(levs+1,3,2)
      real :: rdum
      integer :: n3dfercld,iseedl
      integer :: istat(npoint)
      logical :: trace
      logical, parameter :: debugprint=.true.
!!      logical, parameter :: debugprint=.false.
      character             lprecip_accu*3
      real, parameter :: ERAD=6.371E6
      real, parameter :: DTR=3.1415926/180.
      real :: ap
      integer :: nf1, fint
      integer :: nend1, nint1, nint3
      character*150 :: fngrib2
      integer recn_dpres,recn_delz,recn_dzdt
      integer :: jrec
      equivalence (cstat1,rstat1)
      integer iidum(npoint),jjdum(npoint)
      integer  :: error, ncid, ncid2, id_var,dimid
      character(len=100) :: long_name
      character(len=6) :: fformat
      integer,dimension(8) :: clocking
      character(10)  :: date
      character(12) :: time
      character(7)  :: zone
      character(3)  :: Zreverse
      character(20)  :: VarName,LayName

      nij = 12
      nflx = 6 * levso
          recn_dpres = 0
          recn_delz = 0
          recn_dzdt = 0
          jrec = 0
        lprecip_accu='yes'

       idvc=2
       idsl=1
!read in NetCDF file header info
       print*,"fformat= ", fformat
       print*,'meteorg.f, idum,jdum= '
        do np = 1, npoint
          print*,  iidum(np), jjdum(np)
         enddo

       if(fformat .eq. "netcdf") then
          error=nf90_open(trim(fnsig),nf90_nowrite,ncid)
          error=nf90_get_att(ncid,nf90_global,"ak",vdummy)
           do k = 1, levs+1
            vcoord(k,1)=vdummy(levs-k+1)
           enddo
          error=nf90_get_att(ncid,nf90_global,"bk",vdummy)
           do k = 1, levs+1
            vcoord(k,2)=vdummy(levs-k+1)
           enddo
          error=nf90_inq_varid(ncid, "time", id_var)
          error=nf90_get_var(ncid, id_var, nfhour)
          print*, "nfhour:",nfhour
          error=nf90_get_att(ncid,id_var,"units",long_name)
          print*,'time units',' -- ',trim(long_name)
        read(long_name(13:16),"(i4)")idate(4)
        read(long_name(18:19),"(i2)")idate(2)
        read(long_name(21:22),"(i2)")idate(3)
        read(long_name(24:25),"(i2)")idate(1)
        fhour=float(nfhour)
        print*,'date= ', idate
        jdate = idate(4)*1000000 + idate(2)*10000+
     &        idate(3)*100 + idate(1)
          print *, 'jdate = ', jdate
          error=nf90_inq_varid(ncid, "lon", id_var)
          error=nf90_get_var(ncid, id_var, gdlon)
          error=nf90_inq_varid(ncid, "lat", id_var)
          error=nf90_get_var(ncid, id_var, gdlat)
!!end read NetCDF hearder info, read nemsio below if necessary
       else 

      call nemsio_open(gfile,trim(fnsig),'read',iret=iret)
      call nemsio_getfilehead(gfile,iret=iret
     +     ,idate=idate_nems(1:7),nfhour=nfhour
     +     ,idvc=idvc,idsl=idsl,lat=dum1d,lon=dum1d2
     +     ,vcoord=vcoordnems)

      do k=1,levs+1
        vcoord(k,1)=vcoordnems(k,1,1)
        vcoord(k,2)=vcoordnems(k,2,1)
      end do
      idate(1)=idate_nems(4)
      idate(2)=idate_nems(2)
      idate(3)=idate_nems(3)
      idate(4)=idate_nems(1)
      fhour=float(nfhour)
      print *, ' processing forecast hour ', fhour
      print *, ' idate =', idate
      jdate = idate(4)*1000000 + idate(2)*10000+
     &        idate(3)*100 + idate(1)
      print *, 'jdate = ', jdate
      print *, 'Total number of stations = ', npoint
      ap = 0.0
      do j=1,jm
        do i=1,im
          gdlat(i,j)=dum1d((j-1)*im+i)
          gdlon(i,j)=dum1d2((j-1)*im+i)
        end do
      end do
   
      endif !end read in nemsio hearder 

      if(debugprint) then
      do k=1,levs+1 
      print*,'vcoord(k,1)= ', k, vcoord(k,1)
      end do
      do k=1,levs+1 
      print*,'vcoord(k,2)= ', k, vcoord(k,2)
      end do
      print*,'sample lat= ',gdlat(im/5,jm/4)
     +       ,gdlat(im/5,jm/3),gdlat(im/5,jm/2)
      print*,'sample lon= ',gdlon(im/5,jm/4)
     +       ,gdlon(im/5,jm/3),gdlon(im/5,jm/2)
      endif

      call date_and_time(date,time,zone,clocking)
      print *,'1date, time, zone',date, time, zone
! topography
      if (fformat == 'netcdf') then
      VarName='hgtsfc'
      Zreverse='yes'
      call read_netcdf(ncid,im,jm,1,VarName,hgt,Zreverse,error)
        if (error /= 0) print*,'surface hgt not found'
       else 
      VarName='hgt'
      LayName='sfc'
       call  read_nemsio(gfile,im,jm,1,VarName,LayName,hgt,error)
        if (error /= 0) print*,'surface hgt not found'
      endif
        if(debugprint)print*,'sample sfc h= ',hgt(im/5,jm/4)
     +    ,hgt(im/5,jm/3),hgt(im/5,jm/2)

! surface pressure (Pa)
       if (fformat == 'netcdf') then
      VarName='pressfc'
      Zreverse='yes'
       call read_netcdf(ncid,im,jm,1,VarName,pint(:,:,1),
     &       Zreverse,error)
        if (error /= 0) print*,'surface pressure not found'
       else 
      VarName='pres'
      LayName='sfc'
        call  read_nemsio(gfile,im,jm,1,VarName,
     &                    LayName,pint(:,:,1),error)
        if (error /= 0) print*,'surface pressure not found'
       endif
        if(debugprint)print*,'sample sfc P= ',pint(im/2,jm/4,1),
     +          pint(im/2,jm/3,1),pint(im/2,jm/2,1)

! temperature using NetCDF
      if (fformat == 'netcdf') then
      VarName='tmp'
      Zreverse='yes'
       call read_netcdf(ncid,im,jm,levs,VarName,t3d,Zreverse,error)
        if (error /= 0) print*,'temp not found'
       else 
      VarName='tmp'
      LayName='mid layer'
        call  read_nemsio(gfile,im,jm,levs,VarName,LayName,t3d,error)
        if (error /= 0) print*,'temp not found'
       endif
         if(debugprint) then
          print*,'sample T at lev=1 to levs '
            do k = 1, levs
             print*,k, t3d(im/2,jm/3,k)
            enddo
          endif
! specific humidity
      if (fformat == 'netcdf') then
      VarName='spfh'
      Zreverse='yes'
       call read_netcdf(ncid,im,jm,levs,VarName,q3d,Zreverse,error)
        if (error /= 0) print*,'spfh not found'
       else 
      VarName='spfh'
      LayName='mid layer'
        call  read_nemsio(gfile,im,jm,levs,VarName,LayName,q3d,error)
        if (error /= 0) print*,'spfh not found'
       endif
        if(debugprint) then
          print*,'sample Q at lev=1 to levs '
            do k = 1, levs
             print*,k, q3d(im/2,jm/3,k)
            enddo
          endif
! U wind
      if (fformat == 'netcdf') then
      VarName='ugrd'
      Zreverse='yes'
       call read_netcdf(ncid,im,jm,levs,VarName,uh,Zreverse,error)
        if (error /= 0) print*,'ugrd not found'
       else 
      VarName='ugrd'
      LayName='mid layer'
        call  read_nemsio(gfile,im,jm,levs,VarName,LayName,uh,error)
        if (error /= 0) print*,'ugrd not found'
       endif
          if(debugprint) then
          print*,'sample U at lev=1 to levs '
            do k = 1, levs
             print*,k, uh(im/2,jm/3,k)
            enddo
          endif
! V wind
      if (fformat == 'netcdf') then
      VarName='vgrd'
      Zreverse='yes'
       call read_netcdf(ncid,im,jm,levs,VarName,vh,Zreverse,error)
        if (error /= 0) print*,'vgrd not found'
       else 
      VarName='vgrd'
      LayName='mid layer'
        call  read_nemsio(gfile,im,jm,levs,VarName,LayName,vh,error)
        if (error /= 0) print*,'vgrd not found'
       endif
          if(debugprint) then
          print*,'sample V at lev=1 to levs '
            do k = 1, levs
             print*,k, vh(im/2,jm/3,k)
            enddo
          endif
! dzdt !added by Guang Ping Lou for FV3GFS
      if (fformat == 'netcdf') then
      VarName='dzdt'
      Zreverse='yes'
       call read_netcdf(ncid,im,jm,levs,VarName,omega3d,Zreverse,error)
        if (error /= 0) print*,'dzdt not found'
       else 
      VarName='dzdt'
      LayName='mid layer'
        call  read_nemsio(gfile,im,jm,levs,VarName,LayName,
     &           omega3d,error)
        if (error /= 0) print*,'dzdt not found'
       endif
          if(debugprint) then
          print*,'sample dzdt at lev=1 to levs '
            do k = 1, levs
             print*,k, omega3d(im/2,jm/3,k)
            enddo
          endif
! dpres !added by Guang Ping Lou for FV3GFS (interface pressure delta)
      if (fformat == 'netcdf') then
      VarName='dpres'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,levs,VarName,delp,Zreverse,error)
        if (error /= 0) print*,'dpres not found'
       else 
      VarName='dpres'
      LayName='mid layer'
        call  read_nemsio(gfile,im,jm,levs,VarName,LayName,
     &               delp,error)
        if (error /= 0) print*,'dpres not found'
       endif
          if(debugprint) then
          print*,'sample delp at lev=1 to levs '
            do k = 1, levs
             print*,k, delp(im/2,jm/3,k)
            enddo
          endif
! delz !added by Guang Ping Lou for FV3GFS ("height thickness" with unit "meters" bottom up)
      if (fformat == 'netcdf') then
      VarName='delz'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,levs,VarName,delz,Zreverse,error)
        if (error /= 0) print*,'delz not found'
       else 
      VarName='delz'
      LayName='mid layer'
        call read_nemsio(gfile,im,jm,levs,VarName,LayName,delz,error)
        if (error /= 0) print*,'delz not found'
       endif
          if(debugprint) then
          print*,'sample delz at lev=1 to levs '
            do k = 1, levs
             print*,k, delz(im/2,jm/3,k)
            enddo
          endif

! compute interface pressure
      if(recn_dpres == -9999) then
      do k=2,levs+1
        do j=1,jm
          do i=1,im
            pint(i,j,k)=vcoord(k,1)
     +           +vcoord(k,2)*pint(i,j,1) 
          end do
        end do
       end do
       else
! compute pint using dpres from top down if DZDT is used
      if (fformat == 'netcdf') then
        do j=1,jm
          do i=1,im
            pint(i,j,levs+1) = delp(i,j,1)
          end do
        end do
      do k=levs,2,-1
         kk=levs-k+2
        do j=1,jm
          do i=1,im
            pint(i,j,k) = pint(i,j,k+1) + delp(i,j,kk)
          end do
        end do
       end do
        else
         do k=2,levs+1
          do j=1,jm
           do i=1,im
            pint(i,j,k) = pint(i,j,k-1) - delp(i,j,k-1)
            end do
           end do
          end do
         endif
          if(debugprint) then
          print*,'sample interface pressure pint at lev =1 to levs '
            do k = 1, levs+1
             print*,k, pint(im/2,jm/3,k),pint(im/3,jm/8,k)
            enddo
          endif
       endif
! compute interface height (meter)
      if(recn_delz == -9999) then
       print*, 'using calculated height'
       else
! compute zint using delz from bot up if DZDT is used
      if (fformat == 'netcdf') then
        do j=1,jm
          do i=1,im
            zint(i,j,1) = 0.0
          end do
        end do
      do k=2,levs+1
         kk=levs-k+1
        do j=1,jm
          do i=1,im
            zint(i,j,k) = zint(i,j,k-1) - delz(i,j,kk)
          end do
        end do
       end do
        else
       do k=2,levs+1
        do j=1,jm
         do i=1,im
          zint(i,j,k) = zint(i,j,k-1) + delz(i,j,k-1)
          end do
         end do
        end do
        endif
          if(debugprint) then
          print*,'sample interface height zint at lev =1 to levs '
            do k = 1, levs+1
             print*,k, zint(im/2,jm/3,k),zint(im/3,jm/8,k)
            enddo
          endif
       endif

! close up this NetCDF file
      error=nf90_close(ncid)

! Now open up NetCDF surface files
       if ( nf .le. nend1 ) then
      nf1 = nf - nint1
      else
      nf1 = nf - nint3
       endif
       if ( nf == 0 ) nf1=0
      if(nf==0) then
        fngrib='flxf00'
      elseif(nf.lt.10) then
        fngrib='flxf0'
        write(fngrib(6:6),'(i1)') nf
      elseif(nf.lt.100) then
        fngrib='flxf'
        write(fngrib(5:6),'(i2)') nf
      else
        fngrib='flxf'
        write(fngrib(5:7),'(i3)') nf
      endif
      if(nf1==0) then
        fngrib2='flxf00'
      elseif(nf1.lt.10) then
        fngrib2='flxf0'
        write(fngrib2(6:6),'(i1)') nf1
      elseif(nf1.lt.100) then
        fngrib2='flxf'
        write(fngrib2(5:6),'(i2)') nf1
      else
        fngrib2='flxf'
        write(fngrib2(5:7),'(i3)') nf1
      endif
      if (fformat == 'netcdf') then
          error=nf90_open(trim(fngrib),nf90_nowrite,ncid)
!open T-nint below
          error=nf90_open(trim(fngrib2),nf90_nowrite,ncid2)
       if(error /= 0)print*,'file not open',trim(fngrib), trim(fngrib2)
       else 
      call nemsio_open(ffile,trim(fngrib),'read',iret=error)
      call nemsio_open(ffile2,trim(fngrib2),'read',iret=error)
       if(error /= 0)print*,'file not open',trim(fngrib), trim(fngrib2)
       endif
! land water mask
      if (fformat == 'netcdf') then
      VarName='land'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,lwmask,Zreverse,error)
        if (error /= 0) print*,'lwmask not found'
       else 
      VarName='land'
      LayName='sfc'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,lwmask,error)
        if (error /= 0) print*,'lwmask not found'
       endif
        if(debugprint)
     +   print*,'sample land mask= ',lwmask(im/2,jm/4),
     +          lwmask(im/2,jm/3)

! surface T
      if (fformat == 'netcdf') then
      VarName='tmpsfc'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,dum2d(:,:,1),
     &       Zreverse,error)
        if (error /= 0) print*,'tmpsfc not found'
       else 
      VarName='tmp'
      LayName='sfc'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     &        dum2d(:,:,1),error)
        if (error /= 0) print*,'tmpsfc not found'
       endif
        if(debugprint)
     +   print*,'sample sfc T= ',dum2d(im/2,jm/4,1),dum2d(im/2,jm/3,1),
     +          dum2d(im/2,jm/2,1)
! 2m T
      if (fformat == 'netcdf') then
      VarName='tmp2m'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,dum2d(:,:,2),
     &              Zreverse,error)
        if (error /= 0) print*,'tmp2m not found'
       else 
      VarName='tmp'
      LayName='2 m above gnd'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             dum2d(:,:,2),error)
        if (error /= 0) print*,'tmp2m not found'
       endif
        if(debugprint)
     +   print*,'sample 2m T= ',dum2d(im/2,jm/4,2),dum2d(im/2,jm/3,2),
     +          dum2d(im/2,jm/2,2)

! 2m Q
      if (fformat == 'netcdf') then
      VarName='spfh2m'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,dum2d(:,:,3),
     &           Zreverse,error)
        if (error /= 0) print*,'spfh2m not found'
       else 
      VarName='spfh'
      LayName='2 m above gnd'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             dum2d(:,:,3),error)
        if (error /= 0) print*,'spfh2m not found'
       endif
        if(debugprint)
     +   print*,'sample 2m Q= ',dum2d(im/2,jm/4,3),dum2d(im/2,jm/3,3),
     +          dum2d(im/2,jm/2,3)

! U10
      if (fformat == 'netcdf') then
      VarName='ugrd10m'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,dum2d(:,:,4),
     &              Zreverse,error)
        if (error /= 0) print*,'ugrd10m not found'
       else 
      VarName='ugrd'
      LayName='10 m above gnd'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             dum2d(:,:,4),error)
        if (error /= 0) print*,'ugrd10m not found'
       endif

! V10
      if (fformat == 'netcdf') then
      VarName='vgrd10m'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,dum2d(:,:,5),
     &             Zreverse,error)
        if (error /= 0) print*,'vgrd10m not found'
       else 
      VarName='vgrd'
      LayName='10 m above gnd'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             dum2d(:,:,5),error)
        if (error /= 0) print*,'vgrd10m not found'
       endif

! soil T
      if (fformat == 'netcdf') then
      VarName='soilt1'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,dum2d(:,:,6),
     &              Zreverse,error)
        if (error /= 0) print*,'soilt1 not found'
       else 
      VarName='tmp'
      LayName='0-10 cm down'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             dum2d(:,:,6),error)
        if (error /= 0) print*,'soil T not found'
       endif
        if(debugprint)
     +   print*,'sample soil T= ',dum2d(im/2,jm/4,6),dum2d(im/2,jm/3,6),
     +          dum2d(im/2,jm/2,6)

! snow depth
      if (fformat == 'netcdf') then
      VarName='snod'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,dum2d(:,:,7),
     &          Zreverse,error)
        if (error /= 0) print*,'snod not found'
       else 
      VarName='snod'
      LayName='sfc'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             dum2d(:,:,7),error)
        if (error /= 0) print*,'snod not found'
       endif

! evaporation
!instantaneous surface latent heat net flux
      if (fformat == 'netcdf') then
      VarName='lhtfl'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,dum2d(:,:,8),
     &            Zreverse,error)
        if (error /= 0) print*,'lhtfl not found'
       else 
      VarName='lhtfl'
      LayName='sfc'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             dum2d(:,:,8),error)
        if (error /= 0) print*,'lhtfl not found'
       endif
        if(debugprint)
     +   print*,'evaporation latent heat net flux= ',
     +          dum2d(im/2,jm/4,8),dum2d(im/2,jm/3,8)
        if(debugprint)
     +   print*,'evaporation latent heat net flux stn 000692)= ',
     +          dum2d(2239,441,8)

! total precip
       if ( nf .le. nend1 ) then
      fint = nint1
      else
      fint = nint3
       endif
! for accumulated precipitation:
      if (fformat == 'netcdf') then
      VarName='prate_ave'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,apcp,Zreverse,error) !current hour
       call read_netcdf(ncid2,im,jm,1,VarName,cpcp,Zreverse,error) !earlier hour
        if (error /= 0) print*,'prate_ave not found'
       else 
      VarName='prate_ave'
      LayName='sfc'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             apcp,error)
        call  read_nemsio(ffile2,im,jm,1,VarName,LayName,
     +             cpcp,error)
        if (error /= 0) print*,'prate_ave2 not found'
       endif
        if(debugprint)
     & print*,'sample fhour ,3= ', fhour,
     & '1sample precip rate= ',apcp(im/2,jm/3),cpcp(im/2,jm/3)
          ap=fhour-fint
        do j=1,jm
          do i=1,im
           dum2d(i,j,9) =(apcp(i,j)*fhour-cpcp(i,j)*ap)*3600.0
          end do
        end do

        if(debugprint)
     & print*,'sample fhour ,5= ', fhour,
     & 'sample total precip= ',dum2d(im/2,jm/4,9),
     +         dum2d(im/2,jm/3,9),dum2d(im/2,jm/2,9)

! convective precip
      if (fformat == 'netcdf') then
      VarName='cprat_ave'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,apcp,Zreverse,error) !current hour
       call read_netcdf(ncid2,im,jm,1,VarName,cpcp,Zreverse,error) !earlier hour
        if (error /= 0) print*,'cprat_ave not found'
       else 
      VarName='cprat_ave'
      LayName='sfc'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             apcp,error)
        call  read_nemsio(ffile2,im,jm,1,VarName,LayName,
     +             cpcp,error)
        if (error /= 0) print*,'cprat_ave2 not found'
       endif
          ap=fhour-fint
        do j=1,jm
          do i=1,im
           dum2d(i,j,10)=(apcp(i,j)*fhour-cpcp(i,j)*ap)*3600.0
     &                       
          end do
        end do

! water equi
      if (fformat == 'netcdf') then
      VarName='weasd'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,dum2d(:,:,11),
     &                Zreverse,error)
        if (error /= 0) print*,'weasd not found'
       else 
      VarName='weasd'
      LayName='sfc'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             dum2d(:,:,11),error)
        if (error /= 0) print*,'weasd not found'
       endif

! low cloud fraction
      if (fformat == 'netcdf') then
      VarName='tcdc_avelcl'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,
     &               dum2d(:,:,12),Zreverse,error)
        if (error /= 0) print*,'tcdc_avelcl not found'
       else 
      VarName='tcdc_ave'
      LayName='low cld lay'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             dum2d(:,:,12),error)
        if (error /= 0) print*,'low cld lay not found'
       endif

! mid cloud fraction
      if (fformat == 'netcdf') then
      VarName='tcdc_avemcl'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,
     &               dum2d(:,:,13),Zreverse,error)
        if (error /= 0) print*,'tcdc_avemcl not found'
       else 
      VarName='tcdc_ave'
      LayName='mid cld lay'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             dum2d(:,:,13),error)
        if (error /= 0) print*,'mid cld lay not found'
       endif

! high cloud fraction
      if (fformat == 'netcdf') then
      VarName='tcdc_avehcl'
      Zreverse='no'
       call read_netcdf(ncid,im,jm,1,VarName,
     &               dum2d(:,:,14),Zreverse,error)
        if (error /= 0) print*,'tcdc_avehcl not found'
       else 
      VarName='tcdc_ave'
      LayName='high cld lay'
        call  read_nemsio(ffile,im,jm,1,VarName,LayName,
     +             dum2d(:,:,14),error)
        if (error /= 0) print*,'high cld lay not found'
       endif

        if(debugprint)
     +   print*,'sample high cloud frac= ',dum2d(im/2,jm/4,14),
     +         dum2d(im/2,jm/3,14),dum2d(im/2,jm/2,14)

      if (fformat == 'netcdf') then
        error=nf90_close(ncid)
        error=nf90_close(ncid2)
      else
        call nemsio_close(ffile,iret=error)
        call nemsio_close(ffile2,iret=error)
      endif
      call date_and_time(date,time,zone,clocking)
!      print *,'10reading surface data end= ', clocking
      print *,'10date, time, zone',date, time, zone
!
!  get the nearest neighbor i,j from the table
!
        np1=0
!     
      do np=1, npoint
! use read in predetermined i,j
      if (np1==0) then
        idum=iidum(np)
        jdum=jjdum(np)

      else
!  find nearest neighbor 
        rdum=rlon(np)
        if(rdum<0.)rdum=rdum+360.

        do j=1,jm-1
         do i=1,im-1
          if((rdum>=gdlon(i,j) .and. rdum<=gdlon(i+1,j)) .and.
     +    (rlat(np)<=gdlat(i,j).and.rlat(np)>=gdlat(i,j+1)) ) then
              if(landwater(np) == 2)then
               idum=i
               jdum=j
            exit
              else if(landwater(np) == lwmask(i,j))then
               idum=i
               jdum=j      !1
            exit
              else if(landwater(np) == lwmask(i+1,j))then
               idum=i+1
               jdum=j      ! 2 
            exit
              else if(landwater(np) == lwmask(i-1,j))then
               idum=i-1
               jdum=j      ! 3
            exit
              else if(landwater(np) == lwmask(i,j+1))then
               idum=i
               jdum=j+1    ! 4
            exit
              else if(landwater(np) == lwmask(i,j-1))then
               idum=i
               jdum=j-1    ! 5
            exit
              else if(landwater(np) == lwmask(i+1,j-1))then
               idum=i+1
               jdum=j-1    ! 6
            exit
              else if(landwater(np) == lwmask(i+1,j+1))then
               idum=i+1
               jdum=j+1    ! 7
            exit
              else if(landwater(np) == lwmask(i-1,j+1))then
               idum=i-1
               jdum=j+1    ! 8
            exit
              else if(landwater(np) == lwmask(i-1,j-1))then
               idum=i-1
               jdum=j-1    ! 9
            exit
              else if(landwater(np) == lwmask(i,j+2))then
               idum=i
               jdum=j+2    !   10
            exit
              else if(landwater(np) == lwmask(i+2,j))then
               idum=i+2
               jdum=j      !11
            exit
              else if(landwater(np) == lwmask(i,j-2))then
               idum=i
               jdum=j-2     !  12
            exit
              else if(landwater(np) == lwmask(i-2,j))then
               idum=i-2
               jdum=j       !13
            exit
              else if(landwater(np) == lwmask(i-2,j+1))then
               idum=i-2
               jdum=j+1      ! 14
            exit
              else if(landwater(np) == lwmask(i-1,j+2))then
               idum=i-1
               jdum=j+2      !15
            exit
              else if(landwater(np) == lwmask(i+1,j+2))then
               idum=i+1
               jdum=j+2      !16
            exit
              else if(landwater(np) == lwmask(i+2,j+1))then
               idum=i+2
               jdum=j+1      !17
            exit
              else if(landwater(np) == lwmask(i+2,j-1))then
               idum=i+2
               jdum=j-1      !18
            exit
              else if(landwater(np) == lwmask(i+1,j-2))then
               idum=i+1
               jdum=j-2       !19
            exit
              else if(landwater(np) == lwmask(i-1,j-2))then
               idum=i-1
               jdum=j-2       !20
            exit
              else if(landwater(np) == lwmask(i-2,j-1))then
               idum=i-2
               jdum=j-1       !21
            exit
              else if(landwater(np) == lwmask(i-2,j-2))then
               idum=i-2
               jdum=j-2       !22
            exit
              else if(landwater(np) == lwmask(i+2,j-2))then
               idum=i+2
               jdum=j-2       !23
            exit
              else if(landwater(np) == lwmask(i+2,j+2))then
               idum=i+2
               jdum=j+2       !24
            exit
              else if(landwater(np) == lwmask(i-2,j+2))then
               idum=i-2
               jdum=j+2       !25
            exit
              else if(landwater(np) == lwmask(i+3,j))then
               idum=i+3
               jdum=j         !26
            exit
              else if(landwater(np) == lwmask(i-3,j))then
               idum=i-3
               jdum=j         !27
            exit
              else if(landwater(np) == lwmask(i,j+3))then
               idum=i
               jdum=j+3         !28
            exit
              else if(landwater(np) == lwmask(i,j-3))then
               idum=i
               jdum=j-3         !29
            exit
             else
CC             print*,'no matching land sea mask np,landwater,i,j,mask= '
CC             print*,  np,landwater(np),i,j,lwmask(i,j)
CC             print*, ' So it takes i,j '
               idum=i
               jdum=j
             exit
             end if
          end if
         end do
        end do

        idum=max0(min0(idum,im),1)
        jdum=max0(min0(jdum,jm),1)
        endif !! read in i,j ends here
        if (fhour==0.0) then
        if(debugprint) then
         write(nij,98) np,idum,jdum,rlat(np),rlon(np)
   98     FORMAT (3I6, 2F9.2)
          if(elevstn(np)==-999.) elevstn(np)=hgt(idum,jdum)
         write(9,99) np,rlat(np),rlon(np),elevstn(np),hgt(idum,jdum)
   99     FORMAT (I6, 4F9.2)
         if(np==1 .or.np==100)print*,'nearest neighbor for station ',np
     +  ,idum,jdum,rlon(np),rlat(np),lwmask(i,j),landwater(np)
        endif
        endif

        gridsi(np,1)=hgt(idum,jdum)
        gridsi(np,2)=pint(idum,jdum,1)
        ie=idum+1
        iw=idum-1
        jn=jdum-1
        js=jdum+1
        dx=(gdlon(ie,jdum)-gdlon(iw,jdum))*dtr*erad*
     +      cos(gdlat(idum,jdum)*dtr)
        dy=(gdlat(idum,jn)-gdlat(idum,js))*erad*dtr
        gradx(np)=(log(pint(ie,jdum,1))
     +     -log(pint(iw,jdum,1)))/dx
        grady(np)=(log(pint(idum,jn,1))
     +      -log(pint(idum,js,1)))/dy
        if(debugprint) then
        if(np==1.or.np==100)print*,'gradx,grady= ',
     +   gradx(np),grady(np)
        endif
        
        sfc(5,np)=dum2d(idum,jdum,1)
        sfc(6,np)=dum2d(idum,jdum,6)
        sfc(17,np)=dum2d(idum,jdum,8)
        sfc(12,np)=dum2d(idum,jdum,9)
        sfc(11,np)=dum2d(idum,jdum,10)
        sfc(10,np)=dum2d(idum,jdum,11)
        sfc(27,np)=dum2d(idum,jdum,12)
        sfc(26,np)=dum2d(idum,jdum,13)
        sfc(25,np)=dum2d(idum,jdum,14)
        sfc(34,np)=dum2d(idum,jdum,4)
        sfc(35,np)=dum2d(idum,jdum,5)
        sfc(30,np)=dum2d(idum,jdum,2)
        sfc(31,np)=dum2d(idum,jdum,3)

CC There may be cases where convective precip is greater than total precip
CC due to rounding and interpolation errors, correct it here -G.P. Lou:
        if(sfc(11,np) .gt. sfc(12,np)) sfc(11,np)=sfc(12,np) 

        do k=1,levs
          gridsi(np,k+2)=t3d(idum,jdum,k) 
          gridsi(np,k+2+levs)=q3d(idum,jdum,k)
          gridsi(np,k+2+2*levs)=omega3d(idum,jdum,k)
          gridui(np,k)=uh(idum,jdum,k)
          gridvi(np,k)=vh(idum,jdum,k)
          p1(np,k)=0.5*(pint(idum,jdum,k)+pint(idum,jdum,k+1))
          z1(np,k)=0.5*(zint(idum,jdum,k)+zint(idum,jdum,k+1))
 
          griddiv(np,k)=(uh(ie,jdum,k)-uh(iw,jdum,k))/dx+
     +       (vh(idum,jn,k)*cos(gdlat(idum,jn)*dtr)-
     +       vh(idum,js,k)*cos(gdlat(idum,js)*dtr))/dy/
     +       cos(gdlat(idum,jdum)*dtr)
        end do
      end do 
      
      print*,'finish finding nearest neighbor for each station'

        do np = 1, npoint
!        !ps in kPa
          ps(np) = gridsi(np,2)/1000.  !! surface pressure
        enddo

!
!   compute omega(Pa/s) and interface layer pressure (Pa) 
!
      if(recn_dzdt == -9999) then !!calculated omega
      do np=1,npoint
       call modstuff(levs,idvc,idsl,
     &       nvcoord,vcoord,ps(np)*1000,
     &       gradx(np),grady(np),griddiv(np,1:levs),
     &       gridui(np,1:levs),gridvi(np,1:levs),
     &       pd1(np,1:levs),pd1(np,1:levs),omegai(np,1:levs))
      enddo
!
!  put omega (pa/s) in the tracer to prepare for interpolation
!
        print*, 'using calculated omega '
        do k = 1, levs
          do np = 1, npoint
            gridsi(np,2+levs*2+k) = omegai(np,k)
          enddo
        enddo
       else
        print*, 'using model dzdt m/s'
          if(debugprint) then
        do k = 1, levs
          print*,'sample gridsi(dzdt) at lev ',k,' = ',
     +     gridsi(10,2+levs*2+k)
        enddo
       endif
       endif

!        -----------------
! levs=levso so the following section will not be
! excuted so comment out sigma sction for now 
!         sigheado=sighead
!        -----------------
        print*, 'levs,levso= ', levs, levso
        if(levs.ne.levso) then
          do np = 1, npoint
            grids(np,1) = gridsi(np,1)
            grids(np,2) = gridsi(np,2)
          enddo
          call vintg(npoint,npoint,levs,levso,2,
     &      p1,gridui,gridvi,gridsi(1,3),gridsi(1,3+levs),
     &      p2,gridu, gridv, grids (1,3),grids (1,3+levso))
          do k = 1, levso
            do np = 1, npoint
              omega(np,k) = grids(np,2+levso*2+k) 
            enddo
          enddo
        else
          do k = 1, levs
            do np = 1, npoint
              p2(np,k)    = p1(np,k)
              gridu(np,k) = gridui(np,k)
              gridv(np,k) = gridvi(np,k)
              omega(np,k) = omegai(np,k) 
            enddo
          enddo
! Put topo(1),surf press(2),vir temp(3:66),and specifi hum(67:130) in grids
! for each station
          do k = 1, 2*levs+2
            do np = 1, npoint
              grids(np,k) = gridsi(np,k)
            enddo
          enddo
        endif  !END OF IF STATMENT LEVS .NE. LEVSO
      if(recn_dzdt == 0 ) then !!DZDT
          do k = 1, levs
            do np = 1, npoint
              omega(np,k) = gridsi(np,2+levs*2+k) 
            enddo
          enddo
                 if(debugprint)
     +     print*,'sample (omega) dzdt ', (omega(3,k),k=1,levs)
      endif
!
!  move surface pressure to the station surface from the model surface
!
        do np = 1, npoint
!
!  when the station elevation information in the table says missing,
!    use the model elevation
!
!          print *, "elevstn = ", elevstn(np)
          if(elevstn(np)==-999.) elevstn(np) = grids(np,1)
          psn(np) = ps(np) * exp(-con_g*(elevstn(np)-grids(np,1)) /
     &              (con_rd * grids(np,3)))
          call sigio_modpr(1,1,levso,nvcoord,idvc,
     &         idsl,vcoord,iret,
     &         ps=psn(np)*1000,pd=pd3(np,1:levso),pm=p3(np,1:levso))
          grids(np,2) = log(psn(np))
          if(np==1)print*,'station H,grud H,psn,ps,new pm',
     &     elevstn(np),grids(np,1),psn(np),ps(np),p3(np,1:levso)
        enddo
!
!  move t to new levels conserving theta
!  move q to new levels conserving RH
!
        do k = 1, levso
          do np = 1, npoint
            pp  = p2(np,k)            
            ppn = p3(np,k)          
            tt(np,k) = grids(np,k+2)    
            ttnew(np,k) = tt(np,k) * (ppn/pp)**(con_rocp)
!            if(np==1)print*,'k,pp,ppn,tt,ttnew= ',k,pp,ppn,
!     +      tt(np,k),ttnew(np,k)
            call svp(qsn,esn,ppn,ttnew(np,k))
            call svp(qs,es,pp,tt(np,k))
            qnew(np,k) = grids(np,k+levso+2) * qsn / qs
          enddo
        enddo
!
!  move the new clocking into the old
!
        do np = 1, npoint
          ps(np) = psn(np)
        enddo
        do k = 1, levso
          do np = 1, npoint
            grids(np,k+2) = ttnew(np,k)
            grids(np,k+levso+2) = qnew(np,k)
          enddo
        enddo
        print*,'finish adjusting to station terrain'
!
!  get sea-level pressure (Pa) and layer geopotential height
!
        do np=1,npoint
          call gslp(levso,elevstn(np),ps(np)*1000,
     &      p3(np,1:levso),ttnew(np,1:levso),qnew(np,1:levso),
     &      pmsl(np),zp(np,1:levso),zp2(1:2))
        enddo
      if(recn_delz == -9999) then 
        print*, 'using calculated height '
       else
        print*, 'using model height m'
        do  k = 1, levso
          do np=1, npoint
           zp(np,k) =  z1(np,k)
          enddo
        enddo
       endif
       print*,'finish computing MSLP'
       print*,'finish computing zp ', (zp(3,k),k=1,levso)
       print*,'finish computing zp2(1-2) ', zp2(1),zp2(2)
!
!  prepare buffer data
!
        do np = 1, npoint
          pi3(np,1)=psn(np)*1000
          do k=1,levso
            pi3(np,k+1)=pi3(np,k)-pd3(np,k)    !layer pressure (Pa)
          enddo
!!     ==ivalence (cstat1,rstat1)
          cstat1=cstat(np)
          data(1) = ifix(fhour+.2) * 3600    ! FORECAST TIME (SEC)
          data(2) = istat(np)                ! STATION NUMBER
          data(3) = rstat1                   ! STATION CHARACTER ID
          data(4) = rlat(np)                 ! LATITUDE  (DEG N)
          data(5) = rlon(np)                 ! LONGITUDE (DEG E)
          data(6) = elevstn(np)              ! STATION ELEVATION (M)
          psfc = 10. * psn(np)               ! convert to MB
          leveta = 1
          do k = 1, levso
!
!  look for the layer above 500 mb for precip type computation
!
            if(pi3(np,k).ge.50000.) leveta = k
            ppi = pi3(np,k)                 
            t = grids(np,k+2)
            q = max(1.e-8,grids(np,2+k+levso))
            u = gridu(np,k)
            v = gridv(np,k)
!           data((k-1)*6+7) = pi3(np,k)                ! PRESSURE (PA) at interface layer
            data((k-1)*6+7) = p3(np,k)                 ! PRESSURE (PA) at integer layer
            data((k-1)*6+8) = t                        ! TEMPERATURE (K)
            data((k-1)*6+9) = u                        ! U WIND (M/S)
            data((k-1)*6+10) = v                        ! V WIND (M/S)
            data((k-1)*6+11) = q                       ! HUMIDITY (KG/KG)
            data((k-1)*6+12) = omega(np,k)*100.        ! Omega (pa/sec) !changed to dzdt(cm/s) if available
          enddo
!
!  process surface flux file fields
!
          data(8+nflx) = psfc * 100.                   ! SURFACE PRESSURE (PA)
          data(7+nflx) = pmsl(np)                           
!!          dtemp = .0065 * (grids(np,1) - elevstn(np))
!!          dtemp = .0100 * (grids(np,1) - elevstn(np))
!!          sfc(37,np) = data(6+nflx) * .01
          sfc(37,np) = data(7+nflx) * .01
          sfc(39,np) = zp2(2)   !500 hPa height       
!
!  do height correction if there is no snow or if the temp is less than 0
! G.P.LOU:
! It was decided that no corrctions were needed due to higher model
! resolution.
!
!          if(sfc(10,np)==0.) then
!            sfc(30,np) = sfc(30,np) + dtemp
!            sfc(5,np) = sfc(5,np) + dtemp
!          endif
!          if(sfc(10,np).gt.0..and.sfc(5,np).lt.273.16) then
!            sfc(5,np) = sfc(5,np) + dtemp
!            if(sfc(5,np).gt.273.16) then
!              dtemp = sfc(5,np) - 273.16
!              sfc(5,np) = 273.16
!            endif
!            sfc(30,np) = sfc(30,np) + dtemp
!          endif
        if(debugprint)
     +   print*,'evaporation (stn 000692)= ',sfc(17,np)
          data(9+nflx) = sfc(5,np)                       ! tsfc (K)
          data(10+nflx) = sfc(6,np)                       ! 10cm soil temp (K)
          data(11+nflx) = sfc(17,np)                     ! evaporation (w/m**2)
          data(12+nflx) = sfc(12,np)                     ! total precip (m)
          data(13+nflx) = sfc(11,np)                     ! convective precip (m)
          data(14+nflx) = sfc(10,np)                     ! water equi. snow (m)
          data(15+nflx) = sfc(27,np)                     ! low cloud (%)
          data(16+nflx) = sfc(26,np)                     ! mid cloud
          data(17+nflx) = sfc(25,np)                     ! high cloud
          data(18+nflx) = sfc(34,np)                     ! U10 (m/s)
          data(19+nflx) = sfc(35,np)                     ! V10 (m/s)
          data(20+nflx) = sfc(30,np)                     ! T2 (K)
          data(21+nflx) = sfc(31,np)                     ! Q2 (K)

          data(22+nflx) = 0.
          data(23+nflx) = 0.
          data(24+nflx) = 0.
          data(25+nflx) = 0.
          iwx = 0
          nd = 0
          trace = .false.
          DOMS=0.
          DOMR=0.
          DOMIP=0.
          DOMZR=0.
          if(np==1.or.np==2) nd = 1
          if(np==1.or.np==2) trace = .true.

          if(sfc(12,np).gt.0.) then !check for precip then calc precip type
          do k = 1, leveta+1
            pp = p3(np,k)              
            ppi = pi3(np,k)               
            t = grids(np,k+2)
            q = max(0.,grids(np,2+k+levso))
            u = gridu(np,k)
            v = gridv(np,k)
            if(q.gt.1.e-6.and.pp.ge.20000.) then
              call tdew(td,t,q,pp)
              call lcl(tlcl,plcl,t,pp,q)
              call mstadb(qw,tw,pp,q,tlcl,plcl)
            else
              td = t - 30.
              tw = t - 30.
            endif
!  Calpreciptype input variables
            gt0(1,k)= t
            gq0(1,k) = q
            prsl(1,k) = pp
            prsi(1,k)=ppi
            phii(1,k)=zp(np,k)     !height in meters 
          enddo
!       Use GFS routine calpreciptype.f to calculate precip type
            xlat=rlat(np)
            xlon=rlon(np)
            lm=leveta
            lp1=leveta+1
            PREC=data(12+nflx)
            n3dfercld=1  !if =3 then use Ferriers Explicit Precip Type
            TSKIN=1.     !used in Ferriers Explicit Precip Scheme
            SR=1.        !used in Ferriers Explicit Precip Scheme
            iseedl=jdate
            call random_setseed(iseedl)
            call random_number(randomno)
            call calpreciptype(1,1,1,1,lm,lp1,randomno,xlat,xlon, !input
     &      gt0,gq0,prsl,prsi,PREC,phii,n3dfercld,TSKIN,SR,phy_f3d, !input
     &      DOMR,DOMZR,DOMIP,DOMS)  ! Output vars
          endif
          data(nflx + 22) = DOMS
          data(nflx + 23) = DOMIP
          data(nflx + 24) = DOMZR
          data(nflx + 25) = DOMR
          if(np==1.or.np==100) then
            print *, ' surface fields for hour', nf, 'np =', np
            print *, (data(l+nflx),l=1,25)
            print *, ' temperature sounding'
            print 6101, (data((k-1)*6+8),k=1,levso)
            print *, ' omega sounding'
            print *, (data((k-1)*6+12),k=1,levso)
            print *, ' divergence sounding'
            print *, (griddiv(np,k),k=1,levs)
          endif
C        print *, 'in meteorg nfile1= ', nfile1
          write(nfile) data
        enddo  !End loop over stations np
      call date_and_time(date,time,zone,clocking)
!      print *,'13reading write data end= ', clocking
      print *,'13date, time, zone',date, time, zone
        print *, 'in meteorg nf,nfile,nfhour= ',  nf,nfile,nfhour
        print *, 'Finished writing bufr data file'
 6101   format(2x,6f12.3)
 6102   format(2x,6f12.5)
 6103   format(2x,6f12.5)
!
      close(unit=nfile)
      return
 910  print *, ' error reading surface flux file'
      end

!-----------------------------------------------------------------------
