!-----------------------------------------------------------------------
      subroutine gribit(gribi,lbm,idrt,im,jm,mxbit,colat1,
     &                  ilpds,iptv,icen,igen,ibms,ipu,itl,il1,il2,
     &                  iyr,imo,idy,ihr,iftu,ip1,ip2,itr,
     &                  ina,inm,icen2,ids,iens,
     &                  xlat1,xlon1,delx,dely,orient,proj,
     &                  grib,lgrib,ierr)
!$$$  subprogram documentation block
!
! subprogram:    gribit      create grib message
!   prgmmr: iredell          org: w/nmc23    date: 92-10-31
!
! abstract: create a grib message from a full field.
!   at present, only global latlon grids and gaussian grids
!   and regional polar projections are allowed.
!
! program history log:
!   92-10-31  iredell
!   94-05-04  juang (for gsm and rsm use)
!   97-09-17  iredell  made y2k compliant
!
! usage:    call gribit(f,lbm,idrt,im,jm,mxbit,colat1,
!    &                  ilpds,iptv,icen,igen,ibms,ipu,itl,il1,il2,
!    &                  iyr,imo,idy,ihr,iftu,ip1,ip2,itr,
!    &                  ina,inm,icen2,ids,iens,
!    &                  xlat1,xlon1,delx,dely,orient,proj,
!    &                  grib,lgrib,ierr)
!   input argument list:
!     f        - real (im*jm) field data to pack into grib message
!     lbm      - logical (im*jm) bitmap to use if ibms=1
!     idrt     - integer data representation type
!                (0 for latlon or 4 for gaussian or 5 for polar)
!     im       - integer longitudinal dimension
!     jm       - integer latitudinal dimension
!     mxbit    - integer maximum number of bits to use (0 for no limit)
!     colat1   - real first colatitude of grid if idrt=4 (radians)
!     ilpds    - integer length of the pds (usually 28)
!     iptv     - integer parameter table version (usually 1)
!     icen     - integer forecast center (usually 7)
!     igen     - integer model generating code
!     ibms     - integer bitmap flag (0 for no bitmap)
!     ipu      - integer parameter and unit indicator
!     itl      - integer type of level indicator
!     il1      - integer first level value (0 for single level)
!     il2      - integer second level value
!     iyr      - integer 4-digit year
!     imo      - integer month
!     idy      - integer day
!     ihr      - integer hour
!     iftu     - integer forecast time unit (1 for hour)
!     ip1      - integer first time period
!     ip2      - integer second time period (0 for single period)
!     itr      - integer time range indicator (10 for single period)
!     ina      - integer number included in average
!     inm      - integer number missing from average
!     icen2    - integer forecast subcenter
!                (usually 0 but 1 for reanal or 2 for ensemble)
!     ids      - integer decimal scaling
!     iens     - integer (5) ensemble extended pds values
!                (application,type,identification,product,smoothing)
!                (used only if icen2=2 and ilpds>=45)
!     xlat1    - real first point of regional latitude (radians)
!     xlon1    - real first point of regional longitude (radians)
!     delx     - real dx on 60n for regional (m)
!     dely     - real dy on 60n for regional (m)
!     proj     - real polar projection flag 0 for north 1 for south
!     orient   - real orientation of regional domain
!
!   output argument list:
!     grib     - character (lgrib) grib message
!     lgrib    - integer length of grib message
!                (no more than 100+ilpds+im*jm*(mxbit+1)/8)
!     ierr     - integer error code (0 for success)
!
! subprograms called:
!   getbit     - compute number of bits and round data appropriately
!   w3fi68     - make general pds
!   pdsens     - make ensemble pds
!   w3fi72     - engrib data into a grib1 message
!
! attributes:
!   language: cray fortran
!
!$$$
      use machine
      use namelist_def, only : climate
      implicit none
!!
      integer itr,ip2,inm,ina,ihr,idy,ip1,iftu,ids,icen2,ilpds,icen,
     &        iptv,im,idrt,mxbit,jm,il2,il1,imo,iyr,ibms,igen,itl,ipu,
     &        icy,iyc,nbit,nbm,jp2,jp1,ipx,jtr,kclust,nfo,kmembr,
     &        kprob,ilast,iresfl,igrid,iscan,i,ierr,nf,igds11,igds09,
     &        igds10,loni,lon1,lati,igds12,jftu,lat1,lgrib
      real (kind=kind_io8) pi,xprob,fmax,fmin
      real (kind=kind_io8) proj,xlon1,xlat1,colat1,
     &                     orient,dely,delx
      integer iens(5)
      real (kind=kind_io4) gribi(im*jm)
!     real (kind=kind_io8) f(im*jm)
      logical(1) lbm(im*jm)
      character grib(*)
      integer ibm(im*jm*ibms+1-ibms),ipds(100),igds(100),ibds(100)
      real (kind=kind_io8) fr(im*jm)
      character pds(ilpds)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  determine grid parameters
!     do i=1,im*jm
!     f(i)=gribi(i)
!     enddo
!sela  write(0,*) 'gribit top'
      pi=acos(-1.)
      nf=im*jm
      if(idrt.eq.0) then
        if(im.eq.144.and.jm.eq.73) then
          igrid=2
        elseif(im.eq.360.and.jm.eq.181) then
          igrid=3
        else
          igrid=255
        endif
        iresfl=128
        iscan=0
        lat1=nint(90.e3)
        lon1=0
        lati=nint(180.e3/(jm-1))
        loni=nint(360.e3/im)
        igds09=-lat1
        igds10=-loni
        igds11=lati
        igds12=loni
      elseif(idrt.eq.4) then
        if(im.eq.192.and.jm.eq.94) then
          igrid=98
        elseif(im.eq.384.and.jm.eq.190) then
          igrid=126
        else
          igrid=255
        endif
        iresfl=128
        iscan=0
        lat1=nint(90.e3-180.e3/pi*colat1)
        lon1=0
        lati=jm/2
        loni=nint(360.e3/im)
        igds09=-lat1
        igds10=-loni
        igds11=lati
        igds12=loni
      elseif(idrt.eq.5) then    ! polar projection
        igrid=255
        iresfl=0
        iscan=2
        lat1=180.e3/pi*xlat1
        lon1=180.e3/pi*xlon1
        igds09=orient*1.e3
        igds10=delx
        igds11=dely
        igds12=proj
      else
        ierr=40
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  reset forecast hour units in case of overflow
      jftu=iftu
      jp1=ip1
      jp2=ip2
      jtr=itr
      ipx=max(ip1,ip2)
      if (climate) then
        jp1 = ipx
        jp2 = 0
        jtr = 10
      else
!cc   if(itr.ge.2.and.itr.le.5.and.ipx.ge.256) then
!cc     jp1=ip2
!cc     jp2=0
!cc     jtr=10
!cc   endif
        if(iftu.eq.1) then
          if((itr.ge.2.and.itr.le.5.and.ipx.ge.256).or.ipx.ge.65536)
     &                                                         then
            if(mod(ip1,24)+mod(ip2,24).eq.0.and.ipx.lt.24*256) then
              jftu=2
              jp1=ip1/24
              jp2=ip2/24
            elseif(mod(ip1,12)+mod(ip2,12).eq.0.and.ipx.lt.12*256)
     &                                                         then
              jftu=12
              jp1=ip1/12
              jp2=ip2/12
            elseif(mod(ip1,6)+mod(ip2,6).eq.0.and.ipx.lt.6*256) then
              jftu=11
              jp1=ip1/6
              jp2=ip2/6
            elseif(mod(ip1,3)+mod(ip2,3).eq.0.and.ipx.lt.3*256) then
              jftu=10
              jp1=ip1/3
              jp2=ip2/3
            else
              jp1=ipx
              jp2=0
              jtr=10
            endif
          endif
        endif
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  fix year and century
      iyc=mod(iyr-1,100)+1
      icy=(iyr-1)/100+1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  fill pds parameters
      ipds(01)=ilpds    ! length of pds
      ipds(02)=iptv     ! parameter table version id
      ipds(03)=icen     ! center id
      ipds(04)=igen     ! generating model id
      ipds(05)=igrid    ! grid id
      ipds(06)=1        ! gds flag
      ipds(07)=ibms     ! bms flag
      ipds(08)=ipu      ! parameter unit id
      ipds(09)=itl      ! type of level id
      ipds(10)=il1      ! level 1 or 0
      ipds(11)=il2      ! level 2
      ipds(12)=iyc      ! year
      ipds(13)=imo      ! month
      ipds(14)=idy      ! day
      ipds(15)=ihr      ! hour
      ipds(16)=0        ! minute
      ipds(17)=jftu     ! forecast time unit id
      ipds(18)=jp1      ! time period 1
      ipds(19)=jp2      ! time period 2 or 0
      ipds(20)=jtr      ! time range indicator
      ipds(21)=ina      ! number in average
      ipds(22)=inm      ! number missing
      ipds(23)=icy      ! century
      ipds(24)=icen2    ! forecast subcenter
      ipds(25)=ids      ! decimal scaling
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  fill gds and bds parameters
      igds(01)=0        ! number of vertical coords
      igds(02)=255      ! vertical coord flag
      igds(03)=idrt     ! data representation type
      igds(04)=im       ! east-west points
      igds(05)=jm       ! north-south points
      igds(06)=lat1     ! latitude of origin
      igds(07)=lon1     ! longitude of origin
      igds(08)=iresfl   ! resolution flag
      igds(09)=igds09   ! latitude of end or orientation
      igds(10)=igds10   ! longitude of end or dx in meter on 60n
      igds(11)=igds11   ! lat increment or gaussian lats or dy in meter
      igds(12)=igds12   ! longitude increment or projection
      igds(13)=iscan    ! scanning mode flags
      igds(14:18)=0     ! not used
      ibds(1:9)=0       ! bds flags
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  fill bitmap and count valid data.  reset bitmap flag if all valid.
      nbm=nf
      if(ibms.ne.0) then
        nbm=0
        do i=1,nf
          if(lbm(i)) then
            ibm(i)=1
            nbm=nbm+1
          else
            ibm(i)=0
          endif
        enddo
        if(nbm.eq.nf) ipds(7)=0
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  round data and determine number of bits
!sela  write(0,*) 'gribit getbit'
      if(nbm.eq.0) then
        do i=1,nf
          fr(i)=0.
        enddo
        nbit=0
      else
        call getbitl(ipds(7),0,ids,nf,ibm,gribi,fr,fmin,fmax,nbit)
!       write(0,'("getbit:",4i4,4x,2i4,4x,2g16.6)')
!    &   ipu,itl,il1,il2,ids,nbit,fmin,fmax
!sela  write(66,'(g20.10)') fmin
        if(mxbit.gt.0) nbit=min(nbit,mxbit)
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  create product definition section
!sela  write(0,*) 'gribit w3fi68'
      call w3fi68(ipds,pds)
      if(icen2.eq.2.and.ilpds.ge.45) then
        ilast=45
        call pdsens(iens,kprob,xprob,kclust,kmembr,ilast,pds)
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  create grib message
!sela  write(0,*) 'gribit w3fi72'
      call w3fi72(0,fr,0,nbit,1,ipds,pds,
     &            1,255,igds,0,0,ibm,nf,ibds,
     &            nfo,grib,lgrib,ierr)
!sela  write(0,*) 'gribit end'
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
      subroutine getbitl(ibm,ibs,ids,len,mg,g,ground,gmin,gmax,nbit)
!$$$  subprogram documentation block
!
! subprogram:    getbit      compute number of bits and round field.
!   prgmmr: iredell          org: w/nmc23    date: 92-10-31
!
! abstract: the number of bits required to pack a given field
!   for particular binary and decimal scalings is computed.
!   the field is rounded off to the decimal scaling for packing.
!   the minimum and maximum rounded field values are also returned.
!   grib bitmap masking for valid data is optionally used.
!
! program history log:
!   96-09-16  iredell
!
! usage:    call gtbits(ibm,ibs,ids,len,mg,g,gmin,gmax,nbit)
!   input argument list:
!     ibm      - integer bitmap flag (=0 for no bitmap)
!     ibs      - integer binary scaling
!                (e.g. ibs=3 to round field to nearest eighth value)
!     ids      - integer decimal scaling
!                (e.g. ids=3 to round field to nearest milli-value)
!                (note that ids and ibs can both be nonzero,
!                 e.g. ids=1 and ibs=1 rounds to the nearest twentieth)
!     len      - integer length of the field and bitmap
!     mg       - integer (len) bitmap if ibm=1 (0 to skip, 1 to keep)
!     g        - real (len) field
!
!   output argument list:
!     ground   - real (len) field rounded to decimal and binary scaling
!                (set to zero where bitmap is 0 if ibm=1)
!     gmin     - real minimum valid rounded field value
!     gmax     - real maximum valid rounded field value
!     nbit     - integer number of bits to pack
!
! attributes:
!   language: cray fortran
!
!$$$
      use machine, only : kind_io4
!
      implicit none

      integer len,ibm,ibs,ids,nbit
      integer,              dimension(len) :: mg
      real    gmax,gmin
      real (kind=kind_io4), dimension(len) :: g
      real,                 dimension(len) :: ground
!
      real    s,si
      integer i,i1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  round field and determine extremes where bitmap is on
      s  = 2.**ibs*10.**ids
      si = 1.0 / s
      if(ibm == 0) then
        ground(1) = nint(g(1)*s)*si
        gmax = ground(1)
        gmin = ground(1)
        do i=2,len
          ground(i) = nint(g(i)*s)*si
          gmax = max(gmax,ground(i))
          gmin = min(gmin,ground(i))
        enddo
      else
        i1=1
        dowhile(i1 <= len .and. mg(i1) == 0)
          i1 = i1+1
        enddo
        if(i1 <= len) then
          do i=1,i1-1
            ground(i) = 0.
          enddo
          ground(i1) = nint(g(i1)*s)*si
          gmax = ground(i1)
          gmin = ground(i1)
          do i=i1+1,len
            if(mg(i) /= 0) then
              ground(i) = nint(g(i)*s)*si
              gmax = max(gmax,ground(i))
              gmin = min(gmin,ground(i))
            else
              ground(i) = 0.
            endif
          enddo
        else
          do i=1,len
            ground(i) = 0.
          enddo
          gmax = 0.
          gmin = 0.
        endif
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute number of bits
      nbit = log((gmax-gmin)*s+0.9)/log(2.)+1.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
