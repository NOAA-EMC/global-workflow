      subroutine meteorg(npoint,rlat,rlon,istat,elevstn,
     &             nf,nfile,fnsig,jdate,idate,
     &             iromb,maxwv,kwskip,levso,levs,im,jm,kdim)

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
      integer :: kwskip,iromb,maxwv,nfile,npoint,levso,levs,kdim
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
      real :: dx,dy
      real,dimension(npoint,2+levso*3):: grids,gridsi
      real,dimension(npoint) :: rlat,rlon,pmsl,ps,psn,elevstn
      real,dimension(im*jm) :: dum1d,dum1d2
      real,dimension(im,jm) :: gdlat, hgt, gdlon
      real,dimension(im,jm,15) :: dum2d
      real,dimension(im,jm,levs) :: t3d, q3d, uh, vh,omega3d
      real,dimension(im,jm,levs+1) :: pint
      real,dimension(npoint,levso) :: gridu,gridv,omega,qnew,zp
      real,dimension(npoint):: gradx, grady
      real,dimension(npoint,levs) :: griddiv,gridui,gridvi,omegai
      real,dimension(npoint,levso) :: p1,p2,p3,pd1,pd2,pd3,tt,ttnew
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
      real, parameter :: ERAD=6.371E6
      real, parameter :: DTR=3.1415926/180.

      nsig = 11
      nsfc = 12
      nflx = 6 * levso

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

! read sigio data
!      call sigio_aldata(sighead,sigdata,iret)
!      call sigio_srdata(nsig,sighead,sigdata,iret)
! topography (m)
!      scalar(:,1)=sigdata%hs
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

! virtual temperature (k)
!      do k = 1, levs
!        scalar(:,k+2)=sigdata%T(:,k)
!      enddo
! divergence and vorticity
!      do k = 1, levs
!        vector(:,k)=sigdata%d(:,k)
!        vector(:,k+levs)=sigdata%z(:,k)
!      enddo
! specific humidity
!      do k = 1, levs
!        scalar(:,k+2+levs)=sigdata%q(:,k,1)
!      enddo

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
      end do ! vertical loop
! compute interface pressure
      do k=2,levs+1
        do j=1,jm
          do i=1,im
            pint(i,j,k)=vcoord(k,1)
     +           +vcoord(k,2)*pint(i,j,1) 
          end do
        end do
      end do  

! close up this nems file
      call nemsio_close(gfile,iret=iret)

! Chuang: modify to read new nemsio flux file directly instead
!         hence will turn off the run of gfs_flux in ush script
!
! read surface data
!      read(nsfc,err=910) sfc
! open nemsio flux file
      fngrib='flxf00'
      if(nf.lt.10) then
        fngrib='flxf0'
        write(fngrib(6:6),'(i1)') nf
      elseif(nf.lt.100) then
        fngrib='flxf'
        write(fngrib(5:6),'(i2)') nf
      else
        fngrib='flxf'
        write(fngrib(5:7),'(i3)') nf
      endif
      call nemsio_open(ffile,trim(fngrib),'read',iret=iret)
      if ( iret /= 0 ) then
        print*,"fail to open nems flux file";stop
      endif
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
      call nemsio_readrecvw34(ffile,'prate_ave','sfc',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'total precip not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,9) = dum1d(jj+i)
          end do
        end do
        if(debugprint)
     +   print*,'sample total precip= ',dum2d(im/2,jm/4,9),
     +         dum2d(im/2,jm/3,9),dum2d(im/2,jm/2,9)
      end if

! convective precip
      call nemsio_readrecvw34(ffile,'cprat_ave','sfc',
     & 1,data=dum1d,iret=iret)
      if (iret /= 0) then
        print*,'latent heat flux not found'
      else
        do j=1,jm
          jj= (j-1)*im
          do i=1,im
            dum2d(i,j,10) = dum1d(jj+i)
          end do
        end do
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
          if((rdum>=gdlon(i,j) .and. rdum<gdlon(i+1,j)) .and.
     +    (rlat(np)<=gdlat(i,j).and.rlat(np)>gdlat(i,j+1)))then
            idum=i
            jdum=j
            exit
          end if 
         end do
        end do

        idum=max0(min0(idum,im),1)
        jdum=max0(min0(jdum,jm),1)
        if(np==1 .or.np==100)print*,'nearest neighbor for station'
     +  ,idum,jdum,rlon(np),rlat(np)

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

        do k=1,levs
          gridsi(np,k+2)=t3d(idum,jdum,k) 
          gridsi(np,k+2+levs)=q3d(idum,jdum,k)
          gridsi(np,k+2+2*levs)=omega3d(idum,jdum,k)
          gridui(np,k)=uh(idum,jdum,k)
          gridvi(np,k)=vh(idum,jdum,k)
          p1(np,k)=0.5*(pint(idum,jdum,k)+pint(idum,jdum,k+1))
 
          griddiv(np,k)=(uh(ie,jdum,k)-uh(iw,jdum,k))/dx+
     +       (vh(idum,jn,k)*cos(gdlat(idum,jn)*dtr)-
     +       vh(idum,js,k)*cos(gdlat(idum,js)*dtr))/dy/
     +       cos(gdlat(idum,jdum)*dtr)
!          griddiv(np,k)=(uh(ie,jdum,k)-uh(iw,jdum,k))/dx+
!     +        (vh(idum,jn,k)-vh(idum,js,k))/dy 
          if(np==1.or.np==100)print*,
     +    'np,k,idum,jdum,uhe,uhw,vhn,vhs,dx,dy,gdlat'
     +    ,np,k,idum,jdum,uh(ie,jdum,k),uh(iw,jdum,k),vh(idum,jn,k)
     +    ,vh(idum,js,k),dx,dy,gdlat(idum,jdum),
     +    cos(gdlat(idum,jdum)*dtr),griddiv(np,k) 
        end do
      end do 
      
      print*,'finish finding nearest neighbor for each station'

!        call sptgpt(iromb,maxwv,2*levs+2,npoint,
!     &       kwskip,npoint,1,1,rlat,rlon,scalar,gridsi)
!        call sptgptsd(iromb,maxwv,2,npoint,
!     &       kwskip,npoint,1,1,rlat,rlon,scalar,gridsi,gradx,grady)
!        call sptgptv(iromb,maxwv,levs,npoint,
!     &       kwskip,npoint,1,1,rlat,rlon,vector,vector(1,levs+1),
!     &       gridui,gridvi)
!        call sptgpt(iromb,maxwv,levs,npoint,
!     &       kwskip,npoint,1,1,rlat,rlon,vector,griddiv)
        do np = 1, npoint
!        !ps in kPa
          !ps(np) = exp(gridsi(np,2))
          ps(np) = gridsi(np,2)/1000.
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
       if(np==1.or.mod(np,20)==0)print*,'griddiv after ca modstu'
     &  ,griddiv(np,1:levs)
       if(np==1.or.mod(np,20)==0)print*,'omegai after ca modstu'
     &  ,omegai(np,1:levs)
      enddo
!
!  put omega (pa/s) in the tracer to prepare for interpolation
!
        do k = 1, levs
          do np = 1, npoint
            gridsi(np,2+levs*2+k) = omegai(np,k)
          enddo
        enddo
!        print *, ' omegai ='
!        print 6102, (omegai(1,k),k=1,levs)

!        -----------------
! levs=levso so the following section will not be
! excuted so comment out sigma sction for now 
!         sigheado=sighead
!        -----------------
        if(levs.ne.levso) then
          nsil = 13
          rewind nsil
!          call newsig(nsil,sigheado%idvc,levso,
!     &           sigheado%nvcoord,sigheado%vcoord,iossil)
!          if(iossil.ne.0) print*, "fail to read new levels"

!  obtain new interface-layer pressure for new levso
          do np = 1, npoint
!            call sigio_modpr(1,1,levso,sigheado%nvcoord,sigheado%idvc,
!     &           sigheado%idsl,sigheado%vcoord,iret,
!     &           ps=ps(np)*1000,pd=pd2(np,1:levso),pm=p2(np,1:levso))
          enddo
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
!  convert virtual temperature to temperature 
!  Chuang Oct. 2016: removing conversion since nemsio outputs T instead
!                    of Tv
!        do k = 1, levso
!          do np = 1, npoint
!            tt(np,k) = grids(np,k+2) / (1. + .608 *
!     &                 grids(np,k+levso+2))
!          enddo
!        enddo
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
       print*,'finish computing MSLP'
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
            data((k-1)*6+11) = omega(np,k)             ! Omega (pa/sec)
          enddo
!
!  process surface flux file fields
!
          data(7+nflx) = psfc * 100.                   ! SURFACE PRESSURE (PA)
          data(6+nflx) = pmsl(np)                           
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
          write(nfile) data
        enddo  !End loop over stations np
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
