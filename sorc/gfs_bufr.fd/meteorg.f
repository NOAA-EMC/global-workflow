      subroutine meteorg(npoint,rlat,rlon,istat,elevstn,
     &             nf,nfile,fnsig,jdate,idate,
     &       iromb,maxwv,kwskip,levso,levs,im,jm,kdim,
     &       landwater,nend1,nint1,nint3)

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
!     iromb            - for triangular truncation iromb==0
!     maxwv            - wave number
!     kwskip           - spectral array
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
      use nemsio_module
      use sigio_module 
      use physcons
      use mersenne_twister
      use funcphys
      implicit none 
      include 'mpif.h'
!      type(sigio_head):: sighead,sigheado
!      type(sigio_data):: sigdata
      type(nemsio_gfile) :: gfile
      type(nemsio_gfile) :: ffile
      type(nemsio_gfile) :: ffile2
      integer :: kwskip,iromb,maxwv,nfile,npoint,levso,levs,kdim
      integer :: nfile1
      integer :: i,j,im,jm,jj,idum,jdum,idvc,idsl
      integer,parameter :: nvcoord=2 
      real :: scalar(kwskip,2+levs*2)
      real :: vector(kwskip,levs*2)
      integer :: idate(4),nsig,nsfc,nflx,np,k,l,nf,nfhour
      integer :: idate_nems(7)
      integer :: iret,iret1,jdate,nsil,leveta,lm,lp1
      integer :: ie,iw,jn,js
      character*150 :: fnsig,fngrib
      real*8 :: data(6*levso+24)
      real :: fhour,pp,ppn,qs,qsn,esn,es,psfc,ppi,dtemp,iwx,nd
      real :: t,q,u,v,td,tlcl,plcl,qw,tw,xlat,xlon,iossil,dlon
      real :: dx,dy,zhour,zhour2
      integer,dimension(npoint):: landwater
      integer,dimension(im,jm):: lwmask
      real,dimension(im,jm)::  apcp, cpcp
      real,dimension(npoint,2+levso*3):: grids,gridsi
      real,dimension(npoint) :: rlat,rlon,pmsl,ps,psn,elevstn
      real,dimension(im*jm) :: dum1d,dum1d2
      real,dimension(im,jm) :: gdlat, hgt, gdlon
      real,dimension(im,jm,15) :: dum2d
      real,dimension(im,jm,levs) :: t3d, q3d, uh, vh,omega3d
      real,dimension(im,jm,levs) :: delp,delz
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
      real :: vcoord(levs+1,nvcoord),vcoordnems(levs+1,3,2)
      real :: rdum
      integer :: n3dfercld,iseedl,time
      integer :: istat(npoint)
      logical :: trace
      logical, parameter :: debugprint=.true.
      character             lprecip_accu*3
      real, parameter :: ERAD=6.371E6
      real, parameter :: DTR=3.1415926/180.
      real :: ap
      integer :: nf1, fint
      integer :: nend1, nint1, nint3
      character*150 :: fngrib2
      character(len=20)  :: VarNameP, VarNameCP
      integer recn_dpres,recn_delz,recn_dzdt
      integer :: jrec

      nsig = 11
      nsfc = 12
      nflx = 6 * levso
          recn_dpres = 0
          recn_delz = 0
          recn_dzdt = 0
          jrec = 0
        lprecip_accu='yes'

!      call sigio_sropen(nsig,trim(fnsig),iret)
!      call sigio_srhead(nsig,sighead,iret1)
!      if(iret.ne.0 .or. iret1.ne.0) then
!        print*,"fail to open sigma file" 
!        stop
!      endif
!      fhour=sighead%fhour
!      idate=sighead%idate

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
      if(debugprint)print*,'sample lat= ',gdlat(im/5,jm/4)
     +       ,gdlat(im/5,jm/3),gdlat(im/5,jm/2)
      if(debugprint)print*,'sample lon= ',gdlon(im/5,jm/4)
     +       ,gdlon(im/5,jm/3),gdlon(im/5,jm/2)

! read nemsio data
! topography
      call nemsio_readrecvw34(gfile,'hgt','sfc',1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'surface hgt not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            hgt(i,j) = dum1d(jj+i)
          end do
        end do
        if(debugprint)print*,'sample sfc h= ',hgt(im/5,jm/4)
     +    ,hgt(im/5,jm/3),hgt(im/5,jm/2)
      end if 

! surface pressure (Pa)
!      scalar(:,2)=sigdata%ps

      call nemsio_readrecvw34(gfile,'pres','sfc',1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'surface pres not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            pint(i,j,1) = dum1d(jj+i)
          end do
        end do
        if(debugprint)print*,'sample sfc P= ',pint(im/2,jm/4,1),
     +          pint(im/2,jm/3,1),pint(im/2,jm/2,1)
      end if

! temperature using nemsio
      do k =1, levs
        call nemsio_readrecvw34(gfile,'tmp'
     +       ,'mid layer',k,data=dum1d,iret=iret)
        if (iret /= 0) then
          print*,'T not found at ',k
        else
          do j=1,jm
            jj= (j-1)*im
            do i=1,im
              t3d(i,j,k) = dum1d(jj+i)
            end do
          end do
          if(debugprint)
     +     print*,'sample T at lev ',k,' = ',t3d(im/2,jm/4,k),
     +          t3d(im/2,jm/3,k),t3d(im/2,jm/2,k)
        end if
! specific humidity
        call nemsio_readrecvw34(gfile,'spfh'
     +       ,'mid layer',k,data=dum1d,iret=iret)
        if (iret /= 0) then
          print*,'Q not found at ',k
        else
          do j=1,jm
            jj= (j-1)*im
            do i=1,im
              q3d(i,j,k) = dum1d(jj+i)
            end do
          end do
          if(debugprint)
     +     print*,'sample Q at lev ',k,' = ',q3d(im/2,jm/4,k),
     +          q3d(im/2,jm/3,k),q3d(im/2,jm/2,k)
        end if        
! U wind
        call nemsio_readrecvw34(gfile,'ugrd'
     +       ,'mid layer',k,data=dum1d,iret=iret)
        if (iret /= 0) then
          print*,'U not found at ',k
        else
          do j=1,jm
            jj= (j-1)*im
            do i=1,im
              uh(i,j,k) = dum1d(jj+i)
            end do
          end do
          if(debugprint)
     +     print*,'sample U at lev ',k,' = ',uh(im/2,jm/4,k),
     +          uh(im/2,jm/3,k),uh(im/2,jm/2,k)
        end if
! V wind
        call nemsio_readrecvw34(gfile,'vgrd'
     +       ,'mid layer',k,data=dum1d,iret=iret)
        if (iret /= 0) then
          print*,'V not found at ',k
        else
          do j=1,jm
            jj= (j-1)*im
            do i=1,im
              vh(i,j,k) = dum1d(jj+i)
            end do
          end do
          if(debugprint)
     +     print*,'sample V at lev ',k,' = ',vh(im/2,jm/4,k),
     +          vh(im/2,jm/3,k),vh(im/2,jm/2,k)
        end if
! dzdt !added by Guang Ping Lou for FV3GFS
        call nemsio_readrecvw34(gfile,'dzdt'
     +       ,'mid layer',k,data=dum1d,iret=iret)
        if (iret /= 0) then
          recn_dzdt = -9999
          print*,'dzdt not found at ',k
        else
          do j=1,jm
            jj= (j-1)*im
            do i=1,im
              omega3d(i,j,k) = dum1d(jj+i) * 100.0 !convert from m/s to cm/s
            end do
          end do
          if(debugprint)
     +     print*,'sample dzdt at lev ',k,' = ',omega3d(im/2,jm/4,k),
     +          omega3d(im/2,jm/3,k),omega3d(im/2,jm/2,k)
        end if
! dpres !added by Guang Ping Lou for FV3GFS (interface pressure delta)
        call nemsio_readrecvw34(gfile,'dpres'
     +       ,'mid layer',k,data=dum1d,iret=iret)
        if (iret /= 0) then
          recn_dpres = -9999
          print*,'dpres not found at ',k
        else
          do j=1,jm
            jj= (j-1)*im
            do i=1,im
              delp(i,j,k) = dum1d(jj+i)
            end do
          end do
          if(debugprint)
     +     print*,'sample dpres at lev ',k,' = ',delp(im/2,jm/4,k),
     +          delp(im/2,jm/3,k),delp(im/2,jm/2,k)
        end if
! delz !added by Guang Ping Lou for FV3GFS ("height thickness" with unit "meters" bottom up)
        call nemsio_readrecvw34(gfile,'delz'
     +       ,'mid layer',k,data=dum1d,iret=iret)
        if (iret /= 0) then
          recn_delz = -9999
          print*,'delz not found at ',k
        else
          do j=1,jm
            jj= (j-1)*im
            do i=1,im
              delz(i,j,k) = dum1d(jj+i)
            end do
          end do
          if(debugprint)
     +     print*,'sample delz at lev ',k,' = ',delz(im/2,jm/4,k),
     +          delz(im/2,jm/3,k),delz(im/2,jm/2,k)
        end if
      end do ! vertical loop k

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
! compute pint using dpres from bot up if DZDT is used
      do k=2,levs+1
        do j=1,jm
          do i=1,im
            pint(i,j,k) = pint(i,j,k-1) - delp(i,j,k-1)
          end do
        end do
          if(debugprint)
     +     print*,'sample interface pressure pint at lev ',k,' = ',
     +     pint(im/2,jm/4,k),
     +          pint(im/2,jm/3,k),pint(im/2,jm/2,k)
       end do
       endif
! compute interface height (meter)
      if(recn_delz == -9999) then
       print*, 'using calculated height'
       else
! compute pint using dpres from bot up if DZDT is used
        do j=1,jm
          do i=1,im
            zint(i,j,1) = 0.0
          end do
        end do
      do k=2,levs+1
        do j=1,jm
          do i=1,im
            zint(i,j,k) = zint(i,j,k-1) + delz(i,j,k-1)
          end do
        end do
          if(debugprint)
     +     print*,'sample interface height pint at lev ',k,' = ',
     +     zint(im/2,jm/4,k),
     +          zint(im/2,jm/3,k),zint(im/2,jm/2,k)
       end do
       endif

! close up this nems file
      call nemsio_close(gfile,iret=iret)

! read surface data
!      read(nsfc,err=910) sfc
! open nemsio flux file
       if ( nf .le. nend1 ) then
      nf1 = nf - nint1
      else
      nf1 = nf - nint3
       endif
       if ( nf .eq. 0 ) nf1=0
      if(nf.eq.0) then
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
      if(nf1.eq.0) then
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
      call nemsio_open(ffile,trim(fngrib),'read',iret=iret)
      call nemsio_open(ffile2,trim(fngrib2),'read',iret=iret)
       print*, 'open file1,2= ', trim(fngrib),'  ', trim(fngrib2)
      if ( iret /= 0 ) then
        print*,"fail to open nems flux file";stop
      endif
! get hour when buket was last emptied
        if(lprecip_accu == 'no') then
      call nemsio_getheadvar(ffile,'zhour',zhour,iret=iret)
      call nemsio_getheadvar(ffile2,'zhour',zhour2,iret=iret)
      if(debugprint)print*,'sample zhour,zhour2= ',zhour,zhour2
        else
         zhour=0.0
         zhour2=0.0
      if(debugprint)print*,'No bucket zhour,zhour2= ',zhour,zhour2
       endif
! land water mask
      call nemsio_readrecvw34(ffile,'land','sfc',1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'land mask not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            lwmask(i,j) = dum1d(jj+i)
          end do
        end do
        if(debugprint)
     +   print*,'sample land mask= ',lwmask(im/2,jm/4),
     +          lwmask(im/2,jm/3)
      end if

! surface T
      call nemsio_readrecvw34(ffile,'tmp','sfc',1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'surface T not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,1) = dum1d(jj+i)
          end do
        end do
        if(debugprint)
     +   print*,'sample sfc T= ',dum2d(im/2,jm/4,1),dum2d(im/2,jm/3,1),
     +          dum2d(im/2,jm/2,1)
      end if
! 2m T
      call nemsio_readrecvw34(ffile,'tmp','2 m above gnd',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'surface T not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,2) = dum1d(jj+i)
          end do
        end do
      end if

! 2m Q
      call nemsio_readrecvw34(ffile,'spfh','2 m above gnd',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'surface T not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,3) = dum1d(jj+i)
          end do
        end do
        if(debugprint)
     +   print*,'sample 2m Q= ',dum2d(im/2,jm/4,3),dum2d(im/2,jm/3,3),
     +          dum2d(im/2,jm/2,3)
      end if

! U10
      call nemsio_readrecvw34(ffile,'ugrd','10 m above gnd',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'10 m U not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,4) = dum1d(jj+i)
          end do
        end do
      end if

! V10
      call nemsio_readrecvw34(ffile,'vgrd','10 m above gnd',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'10 m V not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,5) = dum1d(jj+i)
          end do
        end do
      end if

! soil T
      call nemsio_readrecvw34(ffile,'tmp','0-10 cm down',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'soil T not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,6) = dum1d(jj+i)
          end do
        end do
        if(debugprint)
     +   print*,'sample soil T= ',dum2d(im/2,jm/4,6),dum2d(im/2,jm/3,6),
     +          dum2d(im/2,jm/2,6)
      end if
! snow depth
      call nemsio_readrecvw34(ffile,'snod','sfc',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'snow depth not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,7) = dum1d(jj+i)
          end do
        end do
      end if

! evaporation
      call nemsio_readrecvw34(ffile,'lhtfl','sfc',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'latent heat flux not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,8) = dum1d(jj+i)
          end do
        end do
      end if

! total precip
       if ( nf .le. nend1 ) then
      fint = nint1
      else
      fint = nint3
       endif
! for accumulated precipitation:
! read meta data to see if precip has zero bucket
!!      call nemsio_getheadvar(ffile,trim(VarName),lprecip_accu,iret)
      call nemsio_searchrecv(ffile,jrec,'prate_ave','sfc',1,iret=iret)
      if (jrec > 0) then
        lprecip_accu='yes'
        VarNameP='prate_ave'
        VarNameCP='cprat_ave'
        if(debugprint)print*,trim(VarNameP),
     &  " Continous precipitation rate found (no bucket)",
     &  "iret= ", iret
         else 
        lprecip_accu='no'
        VarNameP='prateb_ave'
        VarNameCP='cpratb_ave'
        if(debugprint)print*,trim(VarNameP),
     &  " Continous precipitation rate NOT found (with bucket)",
     &  "iret= ", iret
      end if

        if(lprecip_accu == 'yes') then
        if(debugprint) print*, 'continuous precipitation'
      call nemsio_readrecvw34(ffile2,trim(VarNameP),'sfc',
     & 1,data=dum1d2,iret=iret)
      if (iret /= 0) then
        print*,'total precip not found'
      else
        if(debugprint)
     & print*,'sample fhour zhour zhour2,3= ', fhour, zhour, zhour2,
     & '1sample precip rate= ',dum1d(im/2+(jm/4-1)*im),
     +         dum1d(im/2+(jm/3-1)*im),dum1d(im/2+(jm/2-1)*im)
       end if
      call nemsio_readrecvw34(ffile,trim(VarNameP),'sfc',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'prate_ave not found'
      else
        if(debugprint)
     & print*,'sample fhour zhour zhour2,4= ', fhour, zhour, zhour2,
     & '2sample precip rate= ',dum1d(im/2+(jm/4-1)*im),
     +         dum1d(im/2+(jm/3-1)*im),dum1d(im/2+(jm/2-1)*im)
          ap=fhour-fint
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
           dum2d(i,j,9) =(dum1d(jj+i)*fhour-dum1d2(jj+i)*ap)
     &                        * 3600.0
          end do
        end do
      end if

        if(debugprint)
     & print*,'sample fhour zhour zhour2,5= ', fhour, zhour, zhour2,
     & 'sample total precip= ',dum2d(im/2,jm/4,9),
     +         dum2d(im/2,jm/3,9),dum2d(im/2,jm/2,9)

! convective precip
      call nemsio_readrecvw34(ffile2,trim(VarNameCP),'sfc',
     & 1,data=dum1d2,iret=iret)
      if (iret /= 0) then
        print*,'convective precip not found= ', trim(VarNameCP)
      end if
      call nemsio_readrecvw34(ffile,trim(VarNameCP),'sfc',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'cprat_ave not found= ', trim(VarNameCP)
      else
          ap=fhour-fint
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,10)=(dum1d(jj+i)*fhour-dum1d2(jj+i)*ap)
     &                        * 3600.0
          end do
        end do
      end if

!for bucketed precipitation:
      else     !if precip_accu = 'no'
        if(debugprint) print*, 'bucketed precipitation'
        if(debugprint)print*,trim(VarNameP), lprecip_accu
      if ( mod(nf1,6) .eq. 0) then
         do j=1,jm
          do i=1,im
            apcp(i,j) = 0.0
          end do
         end do
       print*, 'mod(nf1,6)= ', nf1
      print*,'sample fhour zhour zhour2,6= ', fhour, zhour, zhour2
      else
      call nemsio_readrecvw34(ffile2,trim(VarNameP),'sfc',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'total precip not found'
      else
        if(debugprint)
     & print*,'sample fhour zhour zhour2,7= ', fhour, zhour, zhour2,
     & '1sample precip rate= ',dum1d(im/2+(jm/4-1)*im),
     +         dum1d(im/2+(jm/3-1)*im),dum1d(im/2+(jm/2-1)*im)
          ap=3600.*(fhour-zhour2-fint)
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            apcp(i,j) = dum1d(jj+i)*ap
          end do
         end do
       end if
       end if
      call nemsio_readrecvw34(ffile,trim(VarNameP),'sfc',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'prate_ave not found'
      else
        if(debugprint)
     & print*,'sample fhour zhour zhour2,8= ', fhour, zhour, zhour2,
     & '2sample precip rate= ',dum1d(im/2+(jm/4-1)*im),
     +         dum1d(im/2+(jm/3-1)*im),dum1d(im/2+(jm/2-1)*im)
            ap = 3600.*(fhour-zhour)
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,9) = dum1d(jj+i)*ap - apcp(i,j)
          end do
        end do
      end if
        if(debugprint)
     & print*,'sample fhour zhour zhour2,9= ', fhour, zhour, zhour2,
     & 'sample total precip= ',dum2d(im/2,jm/4,9),
     +         dum2d(im/2,jm/3,9),dum2d(im/2,jm/2,9)

! convective precip
      if ( mod(nf1,6) .eq. 0) then
         do j=1,jm
          do i=1,im
            cpcp(i,j) = 0.0
          end do
         end do
       print*, 'cpcp mod(nf1,6)= ', nf1
      else
      call nemsio_readrecvw34(ffile2,trim(VarNameCP),'sfc',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'convective precip not found'
      else
          ap=3600.*(fhour-zhour2-fint)
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            cpcp(i,j) = dum1d(jj+i)*ap
          end do
        end do
      end if
      end if     !precip_accu
      call nemsio_readrecvw34(ffile,trim(VarNameCP),'sfc',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'cprat_ave not found'
      else
            ap = 3600.*(fhour-zhour)
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,10) = dum1d(jj+i)*ap - cpcp(i,j)
          end do
        end do
      end if
      end if

! water equi
      call nemsio_readrecvw34(ffile,'weasd','sfc',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'water equivqlent not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,11) = dum1d(jj+i)
          end do
        end do
      end if

! low cloud fraction
      call nemsio_readrecvw34(ffile,'tcdc_ave','low cld lay',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'latent heat flux not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,12) = dum1d(jj+i)
          end do
        end do
      end if

! mid cloud fraction
      call nemsio_readrecvw34(ffile,'tcdc_ave','mid cld lay',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'latent heat flux not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,13) = dum1d(jj+i)
          end do
        end do
      end if

! high cloud fraction
      call nemsio_readrecvw34(ffile,'tcdc_ave','high cld lay',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'latent heat flux not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,14) = dum1d(jj+i)
          end do
        end do
        if(debugprint)
     +   print*,'sample high cloud frac= ',dum2d(im/2,jm/4,14),
     +         dum2d(im/2,jm/3,14),dum2d(im/2,jm/2,14)
      end if

      call nemsio_close(ffile,iret=iret)
      call nemsio_close(ffile2,iret=iret)

!
!  find nearest neighbor 
!     
      dlon=360./float(im)
      do np=1, npoint
        rdum=rlon(np)
        if(rdum<0.)rdum=rdum+360.
!        idum=nint(rdum/dlon)+1  ! assume evenly spaced longitude

        do j=1,jm-1
         do i=1,im-1
          if((rdum>=gdlon(i,j) .and. rdum<=gdlon(i+1,j)) .and.
     +    (rlat(np)<=gdlat(i,j).and.rlat(np)>=gdlat(i,j+1)) ) then
              if(landwater(np) .eq. 2)then
               idum=i
               jdum=j
            exit
              else if(landwater(np) .eq. lwmask(i,j))then
               idum=i
               jdum=j      !1
            exit
              else if(landwater(np) .eq. lwmask(i+1,j))then
               idum=i+1
               jdum=j      ! 2 
            exit
              else if(landwater(np) .eq. lwmask(i-1,j))then
               idum=i-1
               jdum=j      ! 3
            exit
              else if(landwater(np) .eq. lwmask(i,j+1))then
               idum=i
               jdum=j+1    ! 4
            exit
              else if(landwater(np) .eq. lwmask(i,j-1))then
               idum=i
               jdum=j-1    ! 5
            exit
              else if(landwater(np) .eq. lwmask(i+1,j-1))then
               idum=i+1
               jdum=j-1    ! 6
            exit
              else if(landwater(np) .eq. lwmask(i+1,j+1))then
               idum=i+1
               jdum=j+1    ! 7
            exit
              else if(landwater(np) .eq. lwmask(i-1,j+1))then
               idum=i-1
               jdum=j+1    ! 8
            exit
              else if(landwater(np) .eq. lwmask(i-1,j-1))then
               idum=i-1
               jdum=j-1    ! 9
            exit
              else if(landwater(np) .eq. lwmask(i,j+2))then
               idum=i
               jdum=j+2    !   10
            exit
              else if(landwater(np) .eq. lwmask(i+2,j))then
               idum=i+2
               jdum=j      !11
            exit
              else if(landwater(np) .eq. lwmask(i,j-2))then
               idum=i
               jdum=j-2     !  12
            exit
              else if(landwater(np) .eq. lwmask(i-2,j))then
               idum=i-2
               jdum=j       !13
            exit
              else if(landwater(np) .eq. lwmask(i-2,j+1))then
               idum=i-2
               jdum=j+1      ! 14
            exit
              else if(landwater(np) .eq. lwmask(i-1,j+2))then
               idum=i-1
               jdum=j+2      !15
            exit
              else if(landwater(np) .eq. lwmask(i+1,j+2))then
               idum=i+1
               jdum=j+2      !16
            exit
              else if(landwater(np) .eq. lwmask(i+2,j+1))then
               idum=i+2
               jdum=j+1      !17
            exit
              else if(landwater(np) .eq. lwmask(i+2,j-1))then
               idum=i+2
               jdum=j-1      !18
            exit
              else if(landwater(np) .eq. lwmask(i+1,j-2))then
               idum=i+1
               jdum=j-2       !19
            exit
              else if(landwater(np) .eq. lwmask(i-1,j-2))then
               idum=i-1
               jdum=j-2       !20
            exit
              else if(landwater(np) .eq. lwmask(i-2,j-1))then
               idum=i-2
               jdum=j-1       !21
            exit
              else if(landwater(np) .eq. lwmask(i-2,j-2))then
               idum=i-2
               jdum=j-2       !22
            exit
              else if(landwater(np) .eq. lwmask(i+2,j-2))then
               idum=i+2
               jdum=j-2       !23
            exit
              else if(landwater(np) .eq. lwmask(i+2,j+2))then
               idum=i+2
               jdum=j+2       !24
            exit
              else if(landwater(np) .eq. lwmask(i-2,j+2))then
               idum=i-2
               jdum=j+2       !25
            exit
              else if(landwater(np) .eq. lwmask(i+3,j))then
               idum=i+3
               jdum=j         !26
            exit
              else if(landwater(np) .eq. lwmask(i-3,j))then
               idum=i-3
               jdum=j         !27
            exit
              else if(landwater(np) .eq. lwmask(i,j+3))then
               idum=i
               jdum=j+3         !28
            exit
              else if(landwater(np) .eq. lwmask(i,j-3))then
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

         if(np==1 .or.np==100)print*,'nearest neighbor for station ',np
     +  ,idum,jdum,rlon(np),rlat(np),lwmask(i,j),landwater(np)

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
        if(np==1.or.np==100)print*,'gradx,grady= ',
     +   gradx(np),grady(np)
        
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
      if(recn_dzdt == -9999) then !!calculated omega
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
!        print *, ' omegai ='
!        print 6102, (omegai(1,k),k=1,levs)

!        -----------------
! levs=levso so the following section will not be
! excuted so comment out sigma sction for now 
!         sigheado=sighead
!        -----------------
        print*, 'levs,levso= ', levs, levso
        if(levs.ne.levso) then
          nsil = 13
          rewind nsil
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
          if(elevstn(np).eq.-999.) elevstn(np) = grids(np,1)
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
            if(np==1)print*,'k,pp,ppn,tt,ttnew= ',k,pp,ppn,
     +      tt(np,k),ttnew(np,k)
            call svp(qsn,esn,ppn,ttnew(np,k))
            call svp(qs,es,pp,tt(np,k))
            qnew(np,k) = grids(np,k+levso+2) * qsn / qs
          enddo
        enddo
!
!  move the new values into the old
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
          data(1) = ifix(fhour+.2) * 3600    ! FORECAST TIME (SEC)
          data(2) = istat(np)                ! STATION NUMBER
          data(3) = rlat(np)                 ! LATITUDE  (DEG N)
          data(4) = rlon(np)                 ! LONGITUDE (DEG E)
!         data(5) = grids(np,1)              ! STATION ELEVATION (M)
          data(5) = elevstn(np)              ! STATION ELEVATION (M)
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
!           data((k-1)*6+6) = pi3(np,k)                ! PRESSURE (PA) at interface layer
            data((k-1)*6+6) = p3(np,k)                 ! PRESSURE (PA) at integer layer
            data((k-1)*6+7) = t                        ! TEMPERATURE (K)
            data((k-1)*6+8) = u                        ! U WIND (M/S)
            data((k-1)*6+9) = v                        ! V WIND (M/S)
            data((k-1)*6+10) = q                       ! HUMIDITY (KG/KG)
            data((k-1)*6+11) = omega(np,k)             ! Omega (pa/sec) !changed to dzdt(cm/s) if available
          enddo
!
!  process surface flux file fields
!
          data(7+nflx) = psfc * 100.                   ! SURFACE PRESSURE (PA)
          data(6+nflx) = pmsl(np)                           
!!          dtemp = .0065 * (grids(np,1) - elevstn(np))
          dtemp = .0100 * (grids(np,1) - elevstn(np))
          sfc(37,np) = data(6+nflx) * .01
          sfc(39,np) = zp2(2)   !500 hPa height       
!
!  do height correction if there is no snow or if the temp is less than 0
!
          if(sfc(10,np).eq.0.) then
            sfc(30,np) = sfc(30,np) + dtemp
            sfc(5,np) = sfc(5,np) + dtemp
          endif
          if(sfc(10,np).gt.0..and.sfc(5,np).lt.273.16) then
            sfc(5,np) = sfc(5,np) + dtemp
            if(sfc(5,np).gt.273.16) then
              dtemp = sfc(5,np) - 273.16
              sfc(5,np) = 273.16
            endif
            sfc(30,np) = sfc(30,np) + dtemp
          endif
          data(8+nflx) = sfc(5,np)                       ! tsfc (K)
          data(9+nflx) = sfc(6,np)                       ! 10cm soil temp (K)
          data(10+nflx) = sfc(17,np)                     ! evaporation (w/m**2)
          data(11+nflx) = sfc(12,np)                     ! total precip (m)
          data(12+nflx) = sfc(11,np)                     ! convective precip (m)
          data(13+nflx) = sfc(10,np)                     ! water equi. snow (m)
          data(14+nflx) = sfc(27,np)                     ! low cloud (%)
          data(15+nflx) = sfc(26,np)                     ! mid cloud
          data(16+nflx) = sfc(25,np)                     ! high cloud
          data(17+nflx) = sfc(34,np)                     ! U10 (m/s)
          data(18+nflx) = sfc(35,np)                     ! V10 (m/s)
          data(19+nflx) = sfc(30,np)                     ! T2 (K)
          data(20+nflx) = sfc(31,np)                     ! Q2 (K)

          data(21+nflx) = 0.
          data(22+nflx) = 0.
          data(23+nflx) = 0.
          data(24+nflx) = 0.
          iwx = 0
          nd = 0
          trace = .false.
          DOMS=0.
          DOMR=0.
          DOMIP=0.
          DOMZR=0.
          if(np.eq.1.or.np.eq.2) nd = 1
          if(np.eq.1.or.np.eq.2) trace = .true.

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
            PREC=data(11+nflx)
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
          data(nflx + 21) = DOMS
          data(nflx + 22) = DOMIP
          data(nflx + 23) = DOMZR
          data(nflx + 24) = DOMR
          if(np.eq.1.or.np.eq.100) then
            print *, ' surface fields for hour', nf, 'np =', np
            print *, (data(l+nflx),l=1,24)
            print *, ' temperature sounding'
            print 6101, (data((k-1)*6+7),k=1,levso)
            print *, ' omega sounding'
            print *, (data((k-1)*6+11),k=1,levso)
            print *, ' divergence sounding'
            print *, (griddiv(np,k),k=1,levs)
          endif
C        print *, 'in meteorg nfile1= ', nfile1
          write(nfile) data
        enddo  !End loop over stations np
        print *, 'in meteorg nf,nfile,nfhour= ',  nf,nfile,nfhour
        print *, 'Finished writing bufr data file'
 6101   format(2x,6f12.3)
 6102   format(2x,6f12.5)
 6103   format(2x,6f12.5)
!
!  write out surface binary file for grads
!
!       time=0.
!       nlev=1
!       nflag=1
!       write(gstid,'(i8)') istat(np)
!write(125,'(i4,x,i2,x,i2,x,i2,3x,a8,3x,f5.2,x,f7.2,x,4f5.2)')
!     &jdat(1),jdat(2),jdat(3),jdat(5),gstid,rlat(np),rlon(np),
!     &DOMR,DOMS,DOMIP,DOMZR

!      call sigio_axdata(sigdata,iret)
!      call sigio_sclose(nsig,iret)
!      call nemsio_close(gfile,iret=iret)
      close(unit=nfile)
      return
 910  print *, ' error reading surface flux file'
      end

!-----------------------------------------------------------------------
