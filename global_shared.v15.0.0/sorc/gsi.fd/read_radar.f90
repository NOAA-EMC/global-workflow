!  SUBSET=NC006001 -- level 3 superobs
!  SUBSET=NC006002 -- level 2.5 superobs
!  SUBSET=NC006070 -- RADIAL WIND FROM P3 RADAR
subroutine read_radar(nread,ndata,nodata,infile,lunout,obstype,twind,sis,hgtl_full,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_radar                    read radar radial winds
!   prgmmr: yang             org: np23                date: 1998-05-15
!
! abstract:  This routine reads radar radial wind files.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   1998-05-15  yang, weiyu
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-16  treadon - update documentation
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2005-06-10  devenyi/treadon - correct subset declaration
!   2005-08-02  derber - modify to use convinfo file
!   2005-09-08  derber - modify to use input group time window
!   2005-10-11  treadon - change convinfo read to free format
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-10-26  treadon - add routine tag to convinfo printout
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-02-08  derber  - modify to use new convinfo module
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-21  parrish - modify to use level 2, 2.5, and/or 3 radar wind 
!                         superobs, with qc based on vad wind data.
!   2006-05-23  parrish - interpolate model elevation to vad wind site
!   2006-07-28  derber  - use r1000 from constants
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-04-17  safford - rm unused vars and uses
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2009-06-08  parrish - remove erroneous call to cosd, sind
!   2009-05-08  tong    - add reading NOAA P3 tail Dopple  radar data
!   2010-09-08  parrish - remove subroutine check_rotate_wind.  This was a debug routine introduced when
!                           the reference wind rotation angle was stored as an angle, beta_ref.  This field
!                           had a discontinuity at the date line (180E), which resulted in erroneous wind
!                           rotation angles for a small number of winds whose rotation angle was interpolated
!                           from beta_ref values across the discontinuity.  This was fixed by replacing the
!                           beta_ref field with cos_beta_ref, sin_beta_ref.
!   2011-03-28 s.liu  -   add subtype to radial wind observation and limit the use
!                           of level2.5 and level3 data in Conus domain for NMM and NMMB
!   2011-08-01  lueken  - remove deter_zsfc_model (placed in deter_sfc_mod) and fix indentation
!   2012-01-11 m.Hu  -   add subtype to radial wind observation and limit the use
!                           of level2.5 and level3 data in Conus domain for ARW
!   2012-06-26 y.li/x.wang add TDR fore/aft sweep separation for thinning,xuguang.wang@ou.edu
!   2012-04-28  s.liu  -  use new VAD wind
!   2012-11-12  s.liu  -  add new VAD wind flag
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2013-05-07  tong   -  add reading tdr superobs data 
!   2013-05-22  tong   -  Modified the criteria of seperating fore and aft sweeps for TDR NOAA/FRENCH antenna
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!
!
!   input argument list:
!     infile   - file from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     hgtl_full- 3d geopotential height on full domain grid
!
!   output argument list:
!     nread    - number of doppler lidar wind observations read
!     ndata    - number of doppler lidar wind profiles retained for further processing
!     nodata   - number of doppler lidar wind observations retained for further processing
!     sis      - satellite/instrument/sensor indicator
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
  use kinds, only: r_kind,r_single,r_double,i_kind,i_byte
  use constants, only: zero,zero_single,half,one,two,three,deg2rad,rearth,rad2deg, &
      one_tenth,r10,r1000,r60inv,r100,r400,grav_equator, &
      eccentricity,somigliana,grav_ratio,grav, &
      semi_major_axis,flattening,two
  use qcmod, only: erradar_inflate,vadfile,newvad
  use obsmod, only: iadate,l_foreaft_thin
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,time_4dvar,thin4d
  use gridmod, only: regional,nlat,nlon,tll2xy,rlats,rlons,rotate_wind_ll2xy,nsig
  use gridmod, only: wrf_nmm_regional,nems_nmmb_regional,cmaq_regional,wrf_mass_regional
  use convinfo, only: nconvtype,ctwind, &
      ncmiter,ncgroup,ncnumgrp,icuse,ictype,ioctype,ithin_conv,rmesh_conv,pmesh_conv
  use guess_grids, only: hrdifsig,geop_hgtl,nfldsig,ges_prslavg
  use convthin, only: make3grids,map3grids,del3grids,use_all
  use deter_sfc_mod, only: deter_sfc2,deter_zsfc_model
  use mpimod, only: npe
  implicit none 
  
! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile
  character(len=20),intent(in  ) :: sis
  real(r_kind)    ,intent(in   ) :: twind
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  real(r_kind),dimension(nlat,nlon,nsig),intent(in):: hgtl_full

! Declare local parameters
  integer(i_kind),parameter:: maxlevs=1500
  integer(i_kind),parameter:: maxdat=22
  integer(i_kind),parameter:: maxvad=500
! integer(i_kind),parameter:: maxvadbins=20
  integer(i_kind),parameter:: maxvadbins=15
  real(r_kind),parameter:: r4_r_kind = 4.0_r_kind

  real(r_kind),parameter:: dzvad=304.8_r_kind  !  vad reports are every 1000 ft = 304.8 meters
  real(r_kind),parameter:: r3_5 = 3.5_r_kind
  real(r_kind),parameter:: r6 = 6.0_r_kind
  real(r_kind),parameter:: r8 = 8.0_r_kind
  real(r_kind),parameter:: r90 = 90.0_r_kind
  real(r_kind),parameter:: r200 = 200.0_r_kind
  real(r_kind),parameter:: r150 = 150.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: r50000 = 50000.0_r_kind
  real(r_kind),parameter:: r60 = 60.0_r_kind
  real(r_kind),parameter:: r75 = 75.0_r_kind
  real(r_kind),parameter:: r92 = 92.6e03_r_kind
  real(r_kind),parameter:: r89_5  = 89.5_r_kind
  real(r_kind),parameter:: r2 = 2.0_r_kind
  real(r_kind),parameter:: r71 = 71.0_r_kind
  real(r_kind),parameter:: four_thirds = 4.0_r_kind / 3.0_r_kind

! Declare local variables
  logical good,outside,good0,lexist1,lexist2
  
  character(10) date
  character(80) hdrstr(2),datstr(2)
  character(8) subset,subset_check(3)
  character(30) outmessage
  character(255) filename
  
  integer(i_kind) lnbufr,i,j,k,maxobs,icntpnt,iiout,n,istop
  integer(i_kind) nmrecs,ibadazm,ibadtilt,ibadrange,ibadwnd,ibaddist,ibadheight,ibadvad,kthin
  integer(i_kind) iyr,imo,idy,ihr,imn,isc,ithin
  integer(i_kind) ibadstaheight,ibaderror,notgood,idate,iheightbelowsta,ibadfit
  integer(i_kind) notgood0
  integer(i_kind) novadmatch,ioutofvadrange
  integer(i_kind) iy,im,idd,ihh,iret,levs,mincy,minobs,kx0,kxadd,kx
  integer(i_kind) nreal,nchanl,ilat,ilon,ikx
  integer(i_kind),dimension(5):: idate5
  integer(i_kind) ivad,ivadz,nvad,idomsfc
  
  real(r_kind) timeb,rmesh,usage,ff10,sfcr,skint,t4dv,t4dvo,toff
  real(r_kind) eradkm,dlat_earth,dlon_earth
  real(r_kind) dlat,dlon,staheight,tiltangle,clon,slon,clat,slat
  real(r_kind) timeo,clonh,slonh,clath,slath,cdist,dist
  real(r_kind) rwnd,azm,height,error,wqm
  real(r_kind) azm_earth,cosazm_earth,sinazm_earth,cosazm,sinazm
  real(r_kind):: zsges
  
  real(r_kind),dimension(maxdat):: cdata
  real(r_kind),allocatable,dimension(:,:):: cdata_all
  
  real(r_double) rstation_id
  real(r_double),dimension(12):: hdr
  character(8) cstaid
  character(4) this_staid
  equivalence (this_staid,cstaid)
  equivalence (cstaid,rstation_id)
  real(r_double),dimension(7,maxlevs):: radar_obs
  real(r_double),dimension(4,maxlevs):: vad_obs
  real(r_double),dimension(2,maxlevs):: fcst_obs
  
  character(8) vadid(maxvad)
  real(r_kind) vadlat(maxvad),vadlon(maxvad),vadqm(maxvad,maxvadbins)
  real(r_kind) vadu(maxvad,maxvadbins),vadv(maxvad,maxvadbins)
  real(r_kind) vadcount(maxvad,maxvadbins)
  real(r_kind),dimension(maxvad,maxvadbins)::vadfit2,vadcount2,vadwgt2
  real(r_kind),dimension(maxvad,maxvadbins)::vadfit2_5,vadcount2_5,vadwgt2_5
  real(r_kind),dimension(maxvad,maxvadbins)::vadfit3,vadcount3,vadwgt3
  real(r_kind) zob,vadqmmin,vadqmmax
  integer(i_kind) level2(maxvad),level2_5(maxvad),level3(maxvad),level3_tossed_by_2_5(maxvad)
  integer(i_kind) loop,numcut
  integer(i_kind) numhits(0:maxvad)
  real(r_kind) timemax,timemin,errmax,errmin
  real(r_kind) dlatmax,dlonmax,dlatmin,dlonmin
  real(r_kind) xscale,xscalei
  integer(i_kind) max_rrr,nboxmax
  integer(i_kind) irrr,iaaa,iaaamax,iaaamin
  integer(i_byte),allocatable::nobs_box(:,:,:,:)
  real(r_kind) dlonvad,dlatvad,vadlon_earth,vadlat_earth
  real(r_kind) this_stalat,this_stalon,this_stahgt,thistime,thislat,thislon
  real(r_kind) azm0,elev0,range0,rotang
  real(r_kind) thishgt,thisvr,corrected_azimuth,thiserr,corrected_tilt
  integer(i_kind) nsuper2_in,nsuper2_kept
  integer(i_kind) nsuper2_5_in,nsuper2_5_kept
  integer(i_kind) nsuper3_in,nsuper3_kept
  real(r_kind) errzmax
  real(r_kind) thisfit,thisvadspd,thisfit2,uob,vob,thiswgt
! real(r_kind) dist2min,dist2max
! real(r_kind) dist2_5min,dist2_5max
  real(r_kind) vad_leash

! following variables are use for tdr rw data
  real(r_double),dimension(4,maxlevs):: tdr_obs
  integer(i_kind) :: ii,jjj,nmissing,nirrr,noutside,ntimeout,nsubzero,iimax
  integer(i_kind) ntdrvr_in,ntdrvr_kept,ntdrvr_thin1,ntdrvr_thin2
  integer(i_kind) ntdrvr_thin2_foreswp,ntdrvr_thin2_aftswp
  integer(i_kind) maxout,maxdata
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1
  integer(i_kind),allocatable,dimension(:):: isort

  real(r_single) elevmax,elevmin
  real(r_single) thisrange,thisazimuth,thistilt
  real(r_single), dimension(maxlevs) :: dopbin, z, elev, elat8, elon8, glob_azimuth8

  real(r_kind) rlon0,this_stalatr,thistiltr
  real(r_kind) clat0,slat0
  real(r_single) a43,aactual,selev0,celev0,erad

  real(r_kind) sin2,termg,termr,termrg,zobs
  real(r_kind) xmesh,pmesh
  real(r_kind),dimension(nsig):: zges,hges
  real(r_kind) dx,dy,dx1,dy1,w00,w10,w01,w11
  logical luse
  integer(i_kind) ntmp,iout
  integer(i_kind):: zflag
  integer(i_kind) nlevz         ! vertical level for thinning
  real(r_kind) crit1,timedif
  real(r_kind),allocatable,dimension(:):: zl_thin
  real(r_kind),parameter:: r16000 = 16000.0_r_kind
  real(r_kind) diffuu,diffvv

! following variables are for fore/aft separation
  real(r_kind) tdrele1,tdrele2,tdrele3
  integer(i_kind) nswp,firstbeam,nforeswp,naftswp,nfore,naft,nswptype,irec
  logical foreswp,aftswp
  
  data lnbufr/10/
  data hdrstr(1) / 'CLAT CLON SELV ANEL YEAR MNTH DAYS HOUR MINU MGPT' /
  data hdrstr(2) / 'PTID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON HSMSL ANAZ ANEL' /
  data datstr(1) / 'STDM SUPLAT SUPLON HEIT RWND RWAZ RSTD' /
  data datstr(2) / 'DIST HREF DMVR DVSW' /

  data ithin / -9 /
  data rmesh / -99.999_r_kind /
  
!***********************************************************************************

! Check to see if radar wind files exist.  If none exist, exit this routine.
  inquire(file='radar_supobs_from_level2',exist=lexist1)
  inquire(file=trim(infile),exist=lexist2)
  if (.not.lexist1 .and. .not.lexist2) goto 900

  eradkm=rearth*0.001_r_kind
  maxobs=2e6
  nreal=maxdat
  nchanl=0
  ilon=2
  ilat=3
  iaaamax=-huge(iaaamax)
  iaaamin=huge(iaaamin)
  dlatmax=-huge(dlatmax)
  dlonmax=-huge(dlonmax)
  dlatmin=huge(dlatmin)
  dlonmin=huge(dlonmin)

  allocate(cdata_all(maxdat,maxobs),isort(maxobs))

  isort = 0
  cdata_all=zero

  if (trim(infile) == 'tldplrbufr' .or. trim(infile) == 'tldplrso') goto 65

! Initialize variables
! vad_leash=.1_r_kind
  vad_leash=.3_r_kind
 !xscale=5000._r_kind
 !xscale=10000._r_kind
  xscale=20000._r_kind
  write(6,*)'READ_RADAR:  set vad_leash,xscale=',vad_leash,xscale
  write(6,*)'READ_RADAR:  set maxvadbins,maxbadbins*dzvad=',maxvadbins,&
     maxvadbins*dzvad
  xscalei=one/xscale
  max_rrr=nint(100000.0_r_kind*xscalei)
  nboxmax=1

  kx0=22500

  nmrecs=0
  irec=0

  errzmax=zero
  nvad=0
  vadlon=zero
  vadlat=zero
  vadqm=-99999_r_kind
  vadu=zero
  vadv=zero
  vadcount=zero
  vadqmmax=-huge(vadqmmax)
  vadqmmin=huge(vadqmmin)

! First read in all vad winds so can use vad wind quality marks to decide 
! which radar data to keep
! Open, then read bufr data

  open(lnbufr,file=vadfile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)

11 call readsb(lnbufr,iret)
  if(iret/=0) then
     call readmg(lnbufr,subset,idate,iret)
     if(iret/=0) go to 21
     go to 11
  end if
  call ufbint(lnbufr,hdr,7,1,levs,'SID XOB YOB DHR TYP SAID TSB')
  kx=nint(hdr(5))
  if(kx /= 224) go to 11       !  for now just hardwire vad wind type
  if(kx==224 .and. .not.newvad) then
    if(hdr(7)==2) then
        newvad=.true.
        go to 21
     end if
  end if 
! End of bufr read loop
  go to 11

! Normal exit
21 continue
  call closbf(lnbufr)

!  enddo msg_report

  open(lnbufr,file=vadfile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  call readmg(lnbufr,subset,idate,iret)
  if(iret/=0) go to 20

! Time offset
  call time_4dvar(idate,toff)

  write(date,'( i10)') idate
  read (date,'(i4,3i2)') iy,im,idd,ihh 
  write(6,*)'READ_RADAR:  first read vad winds--use vad quality marks to qc 2.5/3 radar winds'

! Big loop over vadwnd bufr file
10 call readsb(lnbufr,iret)
  if(iret/=0) then
     call readmg(lnbufr,subset,idate,iret)
     if(iret/=0) go to 20
     go to 10
  end if
  nmrecs = nmrecs+1

! Read header.  Extract station infomration
  call ufbint(lnbufr,hdr,7,1,levs,'SID XOB YOB DHR TYP SAID TSB')
  kx=nint(hdr(5))
  if(kx /= 224) go to 10       !  for now just hardwire vad wind type

! write(6,*)'new vad::',newvad, hdr(7)
  if(.not.newvad .and. hdr(7)==2) go to 10
  if(newvad .and. hdr(7)/=2) go to 10
                               !  and don't worry about subtypes
! Is vadwnd in convinfo file
  ikx=0
  do i=1,nconvtype
     if(kx == ictype(i)) then
        ikx=i
        exit
     end if
  end do
  if(ikx == 0) go to 10

! Time check
  t4dv=toff+hdr(4)
  if (l4dvar.or.l4densvar) then
     if (t4dv<zero .OR. t4dv>winlen) go to 10 ! outside time window
  else
     timeb=hdr(4)
     if(abs(timeb) > ctwind(ikx) .or. abs(timeb) > half) go to 10 ! outside time window 
  endif

! Create table of vad lat-lons and quality marks in 500m increments
! for cross-referencing bird qc against radar winds
  rstation_id=hdr(1)      !station id
  dlon_earth=hdr(2)       !station lat (degrees)
  dlat_earth=hdr(3)       !station lon (degrees)

  if (dlon_earth>=r360) dlon_earth=dlon_earth-r360
  if (dlon_earth<zero ) dlon_earth=dlon_earth+r360
  dlat_earth = dlat_earth * deg2rad
  dlon_earth = dlon_earth * deg2rad
  ivad=0
  if(nvad>0) then
     do i=1,nvad
        if(modulo(rad2deg*abs(dlon_earth-vadlon(i)),r360)<one_tenth.and. &
           rad2deg*abs(dlat_earth-vadlat(i))<one_tenth) then
           ivad=i
           exit
        end if
     end do
  end if
  if(ivad==0) then
     nvad=nvad+1
     if(nvad>maxvad) then
        write(6,*)'READ_RADAR:  ***ERROR*** MORE THAN ',maxvad,' RADARS:  PROGRAM STOPS'
        call stop2(84)
     end if
     ivad=nvad
     vadlon(ivad)=dlon_earth
     vadlat(ivad)=dlat_earth
     vadid(ivad)=cstaid
  end if

! Update vadqm table
  call ufbint(lnbufr,vad_obs,4,maxlevs,levs,'ZOB WQM UOB VOB ')
  call ufbint(lnbufr,fcst_obs,2,maxlevs,levs,'UFC VFC ')
  if(levs>maxlevs) then
     write(6,*)'READ_RADAR:  ***ERROR*** need to increase read_radar bufr size since ',&
        ' number of levs=',levs,' > maxlevs=',maxlevs
     call stop2(84)
  endif

  do k=1,levs
     wqm=vad_obs(2,k)
     zob=vad_obs(1,k)
     uob=vad_obs(3,k)
     vob=vad_obs(4,k)
     if(newvad) then
     diffuu=uob-fcst_obs(1,k) 
     diffvv=vob-fcst_obs(2,k) 
     if(sqrt(diffuu**2+diffvv**2)>10.0) cycle
     if(abs(diffvv)>8.0) cycle
     if(abs(diffvv)>5.0.and.zob<5000.0) cycle
     if(zob>7000.0) cycle
     end if
     ivadz=nint(zob/dzvad)
     if(ivadz<1.or.ivadz>maxvadbins) cycle
     errzmax=max(abs(zob-ivadz*dzvad),errzmax)
     vadqm(ivad,ivadz)=max(vadqm(ivad,ivadz),wqm)
     vadqmmax=max(vadqmmax,wqm)
     vadqmmin=min(vadqmmin,wqm)
     vadu(ivad,ivadz)=vadu(ivad,ivadz)+uob
     vadv(ivad,ivadz)=vadv(ivad,ivadz)+vob
     vadcount(ivad,ivadz)=vadcount(ivad,ivadz)+one
  end do
     

! End of bufr read loop
  go to 10

! Normal exit
20 continue
  call closbf(lnbufr)


! Print vadwnd table
  if(nvad>0) then
     do ivad=1,nvad
        do ivadz=1,maxvadbins
           vadu(ivad,ivadz)=vadu(ivad,ivadz)/max(one,vadcount(ivad,ivadz))
           vadv(ivad,ivadz)=vadv(ivad,ivadz)/max(one,vadcount(ivad,ivadz))
        end do
        write(6,'(" n,lat,lon,qm=",i3,2f8.2,2x,25i3)') &
           ivad,vadlat(ivad)*rad2deg,vadlon(ivad)*rad2deg,(max(-9,nint(vadqm(ivad,k))),k=1,maxvadbins)
     end do
  end if
  write(6,*)' errzmax=',errzmax
  
!  Allocate thinning grids around each radar
!  space needed is nvad*max_rrr*max_rrr*8*max_zzz
!
!      max_rrr=20
!      maxvadbins=20
!      nvad=150
!      space=150*20*20*8*20 = 64000*150=9600000  peanuts
  
  allocate(nobs_box(max_rrr,8*max_rrr,maxvadbins,nvad))
  nobs_box=0

! Set level2_5 to 0.  Then loop over routine twice, first looking for
! level 2.5 data, and setting level2_5=count of 2.5 data for any 2.5 data
! available that passes the vad tests.  The second pass puts in level 3
! data where it is available and no level 2.5 data was saved/available 
! (level2_5=0)

  vadfit2=zero
  vadfit2_5=zero
  vadfit3=zero
  vadwgt2=zero
  vadwgt2_5=zero
  vadwgt3=zero
  vadcount2=zero
  vadcount2_5=zero
  vadcount3=zero
  level2=0
  level2_5=0
  level3=0
  level3_tossed_by_2_5=0
  subset_check(1)='NC006002'
  subset_check(2)='NC006001'

! First process any level 2 superobs.
! Initialize variables.
  ikx=0
  do i=1,nconvtype
     if(trim(ioctype(i)) == trim(obstype))ikx = i
  end do
  
  timemax=-huge(timemax)
  timemin=huge(timemin)
  errmax=-huge(errmax)
  errmin=huge(errmin)
  loop=0

  numhits=0
  ibadazm=0
  ibadwnd=0
  ibaddist=0
  ibadheight=0
  ibadstaheight=0
  iheightbelowsta=0
  ibaderror=0
  ibadvad=0
  ibadfit=0
  ioutofvadrange=0
  kthin=0
  novadmatch=0
  notgood=0
  notgood0=0
  nsuper2_in=0
  nsuper2_kept=0

  if(loop==0) outmessage='level 2 superobs:'

! Open sequential file containing superobs
  open(lnbufr,file='radar_supobs_from_level2',form='unformatted')
  rewind lnbufr

 ! dist2max=-huge(dist2max)
 ! dist2min=huge(dist2min)

! Loop to read superobs data file
  do
     read(lnbufr,iostat=iret)this_staid,this_stalat,this_stalon,this_stahgt, &
        thistime,thislat,thislon,thishgt,thisvr,corrected_azimuth,thiserr,corrected_tilt
     if(iret/=0) exit
     nsuper2_in=nsuper2_in+1

     dlat_earth=this_stalat    !station lat (degrees)
     dlon_earth=this_stalon    !station lon (degrees)
     if (dlon_earth>=r360) dlon_earth=dlon_earth-r360
     if (dlon_earth<zero ) dlon_earth=dlon_earth+r360
     dlat_earth = dlat_earth * deg2rad
     dlon_earth = dlon_earth * deg2rad
     
     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if (outside) cycle
        dlatmax=max(dlat,dlatmax)
        dlonmax=max(dlon,dlonmax)
        dlatmin=min(dlat,dlatmin)
        dlonmin=min(dlon,dlonmin)
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif
     
     clon=cos(dlon_earth)
     slon=sin(dlon_earth)
     clat=cos(dlat_earth)
     slat=sin(dlat_earth)
     staheight=this_stahgt    !station elevation
     tiltangle=corrected_tilt*deg2rad

!    Find vad wind match
     ivad=0
     do k=1,nvad
        cdist=sin(vadlat(k))*slat+cos(vadlat(k))*clat* &
             (sin(vadlon(k))*slon+cos(vadlon(k))*clon)
        cdist=max(-one,min(cdist,one))
        dist=rad2deg*acos(cdist)
        
        if(dist < 0.2_r_kind) then
           ivad=k
           exit
        end if
     end do
     numhits(ivad)=numhits(ivad)+1
     if(ivad==0) then
        novadmatch=novadmatch+1
        cycle
     end if
     
     vadlon_earth=vadlon(ivad)
     vadlat_earth=vadlat(ivad)
     if(regional)then
        call tll2xy(vadlon_earth,vadlat_earth,dlonvad,dlatvad,outside)
        if (outside) cycle
        dlatmax=max(dlatvad,dlatmax)
        dlonmax=max(dlonvad,dlonmax)
        dlatmin=min(dlatvad,dlatmin)
        dlonmin=min(dlonvad,dlonmin)
     else
        dlatvad = vadlat_earth
        dlonvad = vadlon_earth
        call grdcrd1(dlatvad,rlats,nlat,1)
        call grdcrd1(dlonvad,rlons,nlon,1)
     endif

!    Get model terrain at VAD wind location
     call deter_zsfc_model(dlatvad,dlonvad,zsges)

     t4dvo=toff+thistime
     timemax=max(timemax,t4dvo)
     timemin=min(timemin,t4dvo)

!    Exclude data if it does not fall within time window
     if (l4dvar.or.l4densvar) then
        if (t4dvo<zero .OR. t4dvo>winlen) cycle
     else
        timeo=thistime
        if(abs(timeo)>half ) cycle
     endif

!    Get observation (lon,lat).  Compute distance from radar.
     dlat_earth=thislat
     dlon_earth=thislon
     if(dlon_earth>=r360) dlon_earth=dlon_earth-r360
     if(dlon_earth<zero ) dlon_earth=dlon_earth+r360
     
     dlat_earth = dlat_earth*deg2rad
     dlon_earth = dlon_earth*deg2rad
     if(regional) then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if (outside) cycle
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif
     
     clonh=cos(dlon_earth)
     slonh=sin(dlon_earth)
     clath=cos(dlat_earth)
     slath=sin(dlat_earth)
     cdist=slat*slath+clat*clath*(slon*slonh+clon*clonh)
     cdist=max(-one,min(cdist,one))
     dist=eradkm*acos(cdist)
     irrr=nint(dist*1000*xscalei)
     if(irrr<=0 .or. irrr>max_rrr) cycle

!    Extract radial wind data
     height= thishgt
     rwnd  = thisvr
     azm_earth = corrected_azimuth
     if(regional) then
        cosazm_earth=cos(azm_earth*deg2rad)
        sinazm_earth=sin(azm_earth*deg2rad)
        call rotate_wind_ll2xy(cosazm_earth,sinazm_earth,cosazm,sinazm,dlon_earth,dlon,dlat)
        azm=atan2(sinazm,cosazm)*rad2deg
     else
        azm=azm_earth
     end if
     iaaa=azm/(r360/(r8*irrr))
     iaaa=mod(iaaa,8*irrr)
     if(iaaa<0) iaaa=iaaa+8*irrr
     iaaa=iaaa+1
     iaaamax=max(iaaamax,iaaa)
     iaaamin=min(iaaamin,iaaa)
          
     error = erradar_inflate*thiserr
     errmax=max(error,errmax)
     if(thiserr>zero) errmin=min(error,errmin)
     
!    Perform limited qc based on azimuth angle, radial wind
!    speed, distance from radar site, elevation of radar,
!    height of observation, observation error, and goodness of fit to vad wind

     good0=.true.
     if(abs(azm)>r400) then
        ibadazm=ibadazm+1; good0=.false.
     end if
     if(abs(rwnd)>r200) then
        ibadwnd=ibadwnd+1; good0=.false.
     end if
     if(dist>r400) then
        ibaddist=ibaddist+1; good0=.false.
     end if
     if(staheight<-r1000.or.staheight>r50000) then
        ibadstaheight=ibadstaheight+1; good0=.false.
     end if
     if(height<-r1000.or.height>r50000) then
        ibadheight=ibadheight+1; good0=.false.
     end if
     if(height<staheight) then
        iheightbelowsta=iheightbelowsta+1 ; good0=.false.
     end if
     if(thiserr>r6 .or. thiserr<=zero) then
        ibaderror=ibaderror+1; good0=.false.
     end if
     good=.true.
     if(.not.good0) then
        notgood0=notgood0+1
        cycle
     else

!       Check fit to vad wind and vad wind quality mark
        ivadz=nint(thishgt/dzvad)
        if(ivadz>maxvadbins.or.ivadz<1) then
           ioutofvadrange=ioutofvadrange+1
           cycle
        end if
        thiswgt=one/max(r4_r_kind,thiserr**2)
        thisfit2=(vadu(ivad,ivadz)*cos(azm_earth*deg2rad)+vadv(ivad,ivadz)*sin(azm_earth*deg2rad)-thisvr)**2
        thisfit=sqrt(thisfit2)
        thisvadspd=sqrt(vadu(ivad,ivadz)**2+vadv(ivad,ivadz)**2)
        vadfit2(ivad,ivadz)=vadfit2(ivad,ivadz)+thiswgt*thisfit2
        vadcount2(ivad,ivadz)=vadcount2(ivad,ivadz)+one
        vadwgt2(ivad,ivadz)=vadwgt2(ivad,ivadz)+thiswgt
        if(thisfit/max(one,thisvadspd)>vad_leash) then
           ibadfit=ibadfit+1; good=.false.
        end if
        if(nobs_box(irrr,iaaa,ivadz,ivad)>nboxmax) then
           kthin=kthin+1
           good=.false.
        end if
        if(vadqm(ivad,ivadz) > r3_5  .or.  vadqm(ivad,ivadz) < -one) then
           ibadvad=ibadvad+1 ; good=.false.
        end if
     end if
     
!    If data is good, load into output array
     if(good) then
        nsuper2_kept=nsuper2_kept+1
        level2(ivad)=level2(ivad)+1
        nobs_box(irrr,iaaa,ivadz,ivad)=nobs_box(irrr,iaaa,ivadz,ivad)+1
        ndata    =min(ndata+1,maxobs)
        nodata   =min(nodata+1,maxobs)  !number of obs not used (no meaning here)
        usage = zero
        if(icuse(ikx) < 0)usage=r100
        if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
           if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
        end if

        call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,skint,ff10,sfcr)

        cdata(1) = error             ! wind obs error (m/s)
        cdata(2) = dlon              ! grid relative longitude
        cdata(3) = dlat              ! grid relative latitude
        cdata(4) = height            ! obs absolute height (m)
        cdata(5) = rwnd              ! wind obs (m/s)
        cdata(6) = azm*deg2rad       ! azimuth angle (radians)
        cdata(7) = t4dv              ! obs time (hour)
        cdata(8) = ikx               ! type               
        cdata(9) = tiltangle         ! tilt angle (radians)
        cdata(10)= staheight         ! station elevation (m)
        cdata(11)= rstation_id       ! station id
        cdata(12)= usage             ! usage parameter
        cdata(13)= idomsfc           ! dominate surface type
        cdata(14)= skint             ! skin temperature
        cdata(15)= ff10              ! 10 meter wind factor
        cdata(16)= sfcr              ! surface roughness
        cdata(17)=dlon_earth*rad2deg ! earth relative longitude (degrees)
        cdata(18)=dlat_earth*rad2deg ! earth relative latitude (degrees)
        cdata(19)=dist               ! range from radar in km (used to estimate beam spread)
        cdata(20)=zsges              ! model elevation at radar site
        cdata(21)=thiserr
        cdata(22)=two

!       if(vadid(ivad)=='0303LWX') then
!          dist2max=max(dist2max,dist)
!          dist2min=min(dist2min,dist)
!       end if

        do i=1,maxdat
           cdata_all(i,ndata)=cdata(i)
        end do
        
     else
        notgood = notgood + 1
     end if
     
  end do

  close(lnbufr)	! A simple unformatted fortran file should not be mixed with a bufr I/O
  write(6,*)'READ_RADAR:  ',trim(outmessage),' reached eof on 2/2.5/3 superob radar file'

  write(6,*)'READ_RADAR: nsuper2_in,nsuper2_kept=',nsuper2_in,nsuper2_kept
  write(6,*)'READ_RADAR: # no vad match   =',novadmatch
  write(6,*)'READ_RADAR: # out of vadrange=',ioutofvadrange
  write(6,*)'READ_RADAR: # bad azimuths=',ibadazm
  write(6,*)'READ_RADAR: # bad winds   =',ibadwnd
  write(6,*)'READ_RADAR: # bad dists   =',ibaddist
  write(6,*)'READ_RADAR: # bad stahgts =',ibadstaheight
  write(6,*)'READ_RADAR: # bad obshgts =',ibadheight
  write(6,*)'READ_RADAR: # bad errors  =',ibaderror
  write(6,*)'READ_RADAR: # bad vadwnd  =',ibadvad
  write(6,*)'READ_RADAR: # bad fit     =',ibadfit 
  write(6,*)'READ_RADAR: # num thinned =',kthin
  write(6,*)'READ_RADAR: # notgood0    =',notgood0
  write(6,*)'READ_RADAR: # notgood     =',notgood
  write(6,*)'READ_RADAR: # hgt belowsta=',iheightbelowsta
  write(6,*)'READ_RADAR: timemin,max   =',timemin,timemax
  write(6,*)'READ_RADAR: errmin,max    =',errmin,errmax
  write(6,*)'READ_RADAR: dlatmin,max,dlonmin,max=',dlatmin,dlatmax,dlonmin,dlonmax
  write(6,*)'READ_RADAR: iaaamin,max,8*max_rrr  =',iaaamin,iaaamax,8*max_rrr


!  Next process level 2.5 and 3 superobs

!  Bigger loop over first level 2.5 data, and then level3 data

  timemax=-huge(timemax)
  timemin=huge(timemin)
  errmax=-huge(errmax)
  errmin=huge(errmin)
  nsuper2_5_in=0
  nsuper3_in=0
  nsuper2_5_kept=0
  nsuper3_kept=0
  do loop=1,2

     numhits=0
     ibadazm=0
     ibadwnd=0
     ibaddist=0
     ibadheight=0
     ibadstaheight=0
     iheightbelowsta=0
     ibaderror=0
     ibadvad=0
     ibadfit=0
     ioutofvadrange=0
     kthin=0
     novadmatch=0
     notgood=0
     notgood0=0
!    dist2_5max=-huge(dist2_5max)
!    dist2_5min=huge(dist2_5min)

     if(loop==1)     outmessage='level 2.5 superobs:'
     if(loop==2)     outmessage='level 3 superobs:'

!    Open, then read bufr data
     open(lnbufr,file=trim(infile),form='unformatted')

     call openbf(lnbufr,'IN',lnbufr)
     call datelen(10)
     call readmg(lnbufr,subset,idate,iret)
     if(iret/=0) then
        call closbf(lnbufr)
        go to 1000
     end if

     idate5(1) = iy    ! year
     idate5(2) = im    ! month
     idate5(3) = idd   ! day
     idate5(4) = ihh   ! hour
     idate5(5) = 0     ! minute
     call w3fs21(idate5,mincy)


     nmrecs=0
!    Big loop over bufr file

50   call readsb(lnbufr,iret)
60   continue
     if(iret/=0) then
        call readmg(lnbufr,subset,idate,iret)
        if(iret/=0) go to 1000
        go to 50
     end if
     if(subset/=subset_check(loop)) then
        iret=99
        go to 60
     end if
     nmrecs = nmrecs+1
     

!    Read header.  Extract station infomration
     call ufbint(lnbufr,hdr,10,1,levs,hdrstr(1))

 !   rstation_id=hdr(1)        !station id
     write(cstaid,'(2i4)')idint(hdr(1)),idint(hdr(2))
     if(cstaid(1:1)==' ')cstaid(1:1)='S'
     dlat_earth=hdr(1)         !station lat (degrees)
     dlon_earth=hdr(2)         !station lon (degrees)
     if (dlon_earth>=r360) dlon_earth=dlon_earth-r360
     if (dlon_earth<zero ) dlon_earth=dlon_earth+r360

     if (wrf_nmm_regional.or.nems_nmmb_regional.or.cmaq_regional.or.wrf_mass_regional) then
        if(loop==1) then 
           if(dlon_earth>230.0_r_kind .and.  &
              dlat_earth <54.0_r_kind)then
              go to 50 
           end if
        end if
     end if
     dlat_earth = dlat_earth * deg2rad
     dlon_earth = dlon_earth * deg2rad
     
     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if (outside) go to 50
        dlatmax=max(dlat,dlatmax)
        dlonmax=max(dlon,dlonmax)
        dlatmin=min(dlat,dlatmin)
        dlonmin=min(dlon,dlonmin)
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif
     
     clon=cos(dlon_earth)
     slon=sin(dlon_earth)
     clat=cos(dlat_earth)
     slat=sin(dlat_earth)
     staheight=hdr(3)    !station elevation
     tiltangle=hdr(4)*deg2rad

!    Find vad wind match
     ivad=0
     do k=1,nvad
        cdist=sin(vadlat(k))*slat+cos(vadlat(k))*clat* &
             (sin(vadlon(k))*slon+cos(vadlon(k))*clon)
        cdist=max(-one,min(cdist,one))
        dist=rad2deg*acos(cdist)
        
        if(dist < 0.2_r_kind) then
           ivad=k
           exit
        end if
     end do
     numhits(ivad)=numhits(ivad)+1
     if(ivad==0) then
        novadmatch=novadmatch+1
        go to 50
     end if
     
     vadlon_earth=vadlon(ivad)
     vadlat_earth=vadlat(ivad)
     if(regional)then
        call tll2xy(vadlon_earth,vadlat_earth,dlonvad,dlatvad,outside)
        if (outside) go to 50
        dlatmax=max(dlatvad,dlatmax)
        dlonmax=max(dlonvad,dlonmax)
        dlatmin=min(dlatvad,dlatmin)
        dlonmin=min(dlonvad,dlonmin)
     else
        dlatvad = vadlat_earth
        dlonvad = vadlon_earth
        call grdcrd1(dlatvad,rlats,nlat,1)
        call grdcrd1(dlonvad,rlons,nlon,1)
     endif

!    Get model terrain at VAD wind location
     call deter_zsfc_model(dlatvad,dlonvad,zsges)

     iyr = hdr(5)
     imo = hdr(6)
     idy = hdr(7)
     ihr = hdr(8)
     imn = hdr(9)

     idate5(1) = iyr
     idate5(2) = imo
     idate5(3) = idy
     idate5(4) = ihr
     idate5(5) = imn
     ikx=0
     do i=1,nconvtype
        if(trim(ioctype(i)) == trim(obstype))ikx = i
     end do
     if(ikx==0) go to 50
     call w3fs21(idate5,minobs)
     t4dv=real(minobs-iwinbgn,r_kind)*r60inv
     if (l4dvar.or.l4densvar) then
        if (t4dv<zero .OR. t4dv>winlen) goto 50
     else
        timeb = real(minobs-mincy,r_kind)*r60inv
!       if (abs(timeb)>twind .or. abs(timeb) > ctwind(ikx)) then
        if (abs(timeb)>half .or. abs(timeb) > ctwind(ikx)) then 
!          write(6,*)'READ_RADAR:  time outside window ',timeb,' skip this obs'
           goto 50
        endif
     endif

!    Go through the data levels
     call ufbint(lnbufr,radar_obs,7,maxlevs,levs,datstr(1))
     if(levs>maxlevs) then
        write(6,*)'READ_RADAR:  ***ERROR*** increase read_radar bufr size since ',&
           'number of levs=',levs,' > maxlevs=',maxlevs
        call stop2(84)
     endif

     numcut=0
     do k=1,levs
        if(loop==1)     nsuper2_5_in=nsuper2_5_in+1
        if(loop==2)     nsuper3_in=nsuper3_in+1
        nread=nread+1
        t4dvo=real(minobs+radar_obs(1,k)-iwinbgn,r_kind)*r60inv
        timemax=max(timemax,t4dvo)
        timemin=min(timemin,t4dvo)
        if(loop==2 .and. ivad> 0 .and. level2_5(ivad)/=0) then
           level3_tossed_by_2_5(ivad)=level3_tossed_by_2_5(ivad)+1
           numcut=numcut+1
           cycle
        end if

!       Exclude data if it does not fall within time window
        if (l4dvar.or.l4densvar) then
           if (t4dvo<zero .OR. t4dvo>winlen) cycle
           timeo=t4dv
        else
           timeo=(real(minobs-mincy,r_kind)+real(radar_obs(1,k),r_kind))*r60inv
           if(abs(timeo)>twind .or. abs(timeo) > ctwind(ikx)) then
!             write(6,*)'READ_RADAR:  time outside window ',timeo,&
!                ' skip obs ',nread,' at lev=',k
              cycle
           end if
        end if

!       Get observation (lon,lat).  Compute distance from radar.
        if(radar_obs(3,k)>=r360) radar_obs(3,k)=radar_obs(3,k)-r360
        if(radar_obs(3,k)<zero ) radar_obs(3,k)=radar_obs(3,k)+r360

        dlat_earth = radar_obs(2,k)*deg2rad
        dlon_earth = radar_obs(3,k)*deg2rad
        if(regional) then
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
           if (outside) cycle
        else
           dlat = dlat_earth
           dlon = dlon_earth
           call grdcrd1(dlat,rlats,nlat,1)
           call grdcrd1(dlon,rlons,nlon,1)
        endif
        
        clonh=cos(dlon_earth)
        slonh=sin(dlon_earth)
        clath=cos(dlat_earth)
        slath=sin(dlat_earth)
        cdist=slat*slath+clat*clath*(slon*slonh+clon*clonh)
        cdist=max(-one,min(cdist,one))
        dist=eradkm*acos(cdist)
        irrr=nint(dist*1000*xscalei)
        if(irrr<=0 .or. irrr>max_rrr) cycle

!       Set observation "type" to be function of distance from radar
        kxadd=nint(dist*one_tenth)
        kx=kx0+kxadd

!       Extract radial wind data
        height= radar_obs(4,k)
        rwnd  = radar_obs(5,k)
        azm_earth   = r90-radar_obs(6,k)
        if(regional) then
           cosazm_earth=cos(azm_earth*deg2rad)
           sinazm_earth=sin(azm_earth*deg2rad)
           call rotate_wind_ll2xy(cosazm_earth,sinazm_earth,cosazm,sinazm,dlon_earth,dlon,dlat)
           azm=atan2(sinazm,cosazm)*rad2deg
        else
           azm=azm_earth
        end if
        iaaa=azm/(r360/(r8*irrr))
        iaaa=mod(iaaa,8*irrr)
        if(iaaa<0) iaaa=iaaa+8*irrr
        iaaa=iaaa+1
        iaaamax=max(iaaamax,iaaa)
        iaaamin=min(iaaamin,iaaa)
        
        error = erradar_inflate*radar_obs(7,k)

!    Increase error for lev2.5 and lev3
        if (wrf_nmm_regional.or.nems_nmmb_regional.or.cmaq_regional.or.wrf_mass_regional) then
           if(dlon_earth*rad2deg>230.0_r_kind .and.  &
              dlat_earth*rad2deg <54.0_r_kind)then
              error = error+r10
           end if
        end if
        errmax=max(error,errmax)
        if(radar_obs(7,k)>zero) errmin=min(error,errmin)
        
!       Perform limited qc based on azimuth angle, radial wind
!       speed, distance from radar site, elevation of radar,
!       height of observation, observation error.

        good0=.true.
        if(abs(azm)>r400) then
           ibadazm=ibadazm+1; good0=.false.
        end if
        if(abs(rwnd)>r200) then
           ibadwnd=ibadwnd+1; good0=.false.
        end if
        if(dist>r400) then
           ibaddist=ibaddist+1; good0=.false.
        end if
        if(staheight<-r1000 .or. staheight>r50000) then
           ibadstaheight=ibadstaheight+1; good0=.false.
        end if
        if(height<-r1000 .or. height>r50000) then
           ibadheight=ibadheight+1; good0=.false.
        end if
        if(height<staheight) then
           iheightbelowsta=iheightbelowsta+1 ; good0=.false.
        end if
        if(radar_obs(7,k)>r6 .or. radar_obs(7,k)<=zero) then
           ibaderror=ibaderror+1; good0=.false.
        end if
        good=.true.
        if(.not.good0) then
           notgood0=notgood0+1
           cycle
        else

!          Check against vad wind quality mark
           ivadz=nint(height/dzvad)
           if(ivadz>maxvadbins.or.ivadz<1) then
              ioutofvadrange=ioutofvadrange+1
              cycle
           end if
           thiserr = radar_obs(7,k)
           thiswgt=one/max(r4_r_kind,thiserr**2)
           thisfit2=(vadu(ivad,ivadz)*cos(azm_earth*deg2rad)+vadv(ivad,ivadz)*sin(azm_earth*deg2rad)-rwnd)**2
           thisfit=sqrt(thisfit2)
           thisvadspd=sqrt(vadu(ivad,ivadz)**2+vadv(ivad,ivadz)**2)
           if(loop==1) then
              vadfit2_5(ivad,ivadz)=vadfit2_5(ivad,ivadz)+thiswgt*thisfit2
              vadcount2_5(ivad,ivadz)=vadcount2_5(ivad,ivadz)+one
              vadwgt2_5(ivad,ivadz)=vadwgt2_5(ivad,ivadz)+thiswgt
           else
              vadfit3(ivad,ivadz)=vadfit3(ivad,ivadz)+thiswgt*thisfit2
              vadcount3(ivad,ivadz)=vadcount3(ivad,ivadz)+one
              vadwgt3(ivad,ivadz)=vadwgt3(ivad,ivadz)+thiswgt
           end if
           if(thisfit/max(one,thisvadspd)>vad_leash) then
              ibadfit=ibadfit+1; good=.false.
           end if
           if(nobs_box(irrr,iaaa,ivadz,ivad)>nboxmax) then
              kthin=kthin+1
              good=.false.
           end if
           if(vadqm(ivad,ivadz)>r3_5 .or. vadqm(ivad,ivadz)<-one) then
              ibadvad=ibadvad+1 ; good=.false.
           end if
        end if

!       If data is good, load into output array
        if(good) then
           if(loop==1.and.ivad>0) then
              nsuper2_5_kept=nsuper2_5_kept+1
              level2_5(ivad)=level2_5(ivad)+1
           end if
           if(loop==2.and.ivad>0) then
              nsuper3_kept=nsuper3_kept+1
              level3(ivad)=level3(ivad)+1
           end if
           nobs_box(irrr,iaaa,ivadz,ivad)=nobs_box(irrr,iaaa,ivadz,ivad)+1
           ndata  = min(ndata+1,maxobs)
           nodata = min(nodata+1,maxobs)  !number of obs not used (no meaning here)
           usage  = zero
           if(icuse(ikx) < 0)usage=r100
           if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
              if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
           end if
           
           call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,skint,ff10,sfcr)
           
           cdata(1) = error             ! wind obs error (m/s)
           cdata(2) = dlon              ! grid relative longitude
           cdata(3) = dlat              ! grid relative latitude
           cdata(4) = height            ! obs absolute height (m)
           cdata(5) = rwnd              ! wind obs (m/s)
           cdata(6) = azm*deg2rad       ! azimuth angle (radians)
           cdata(7) = t4dvo             ! obs time (hour)
           cdata(8) = ikx               ! type               
           cdata(9) = tiltangle         ! tilt angle (radians)
           cdata(10)= staheight         ! station elevation (m)
           cdata(11)= rstation_id       ! station id
           cdata(12)= usage             ! usage parameter
           cdata(13)= idomsfc           ! dominate surface type
           cdata(14)= skint             ! skin temperature
           cdata(15)= ff10              ! 10 meter wind factor
           cdata(16)= sfcr              ! surface roughness
           cdata(17)=dlon_earth*rad2deg ! earth relative longitude (degrees)
           cdata(18)=dlat_earth*rad2deg ! earth relative latitude (degrees)
           cdata(19)=dist               ! range from radar in km (used to estimate beam spread)
           cdata(20)=zsges              ! model elevation at radar site
           cdata(21)=radar_obs(7,k)     ! original error from bufr file
           if(loop==1) then
              cdata(22)=2.5_r_kind
           else
              cdata(22)=three
           end if

           do i=1,maxdat
              cdata_all(i,ndata)=cdata(i)
           end do
           
        else
           notgood = notgood + 1
        end if
        
!    End of k loop over levs
     end do

!    End of bufr read loop
     go to 50

!    Normal exit
1000 continue
     call closbf(lnbufr)


!    Close unit to bufr file
     write(6,*)'READ_RADAR:  ',trim(outmessage),' reached eof on 2.5/3 superob radar file.'

     if(loop==1)     write(6,*)'READ_RADAR:  nsuper2_5_in,nsuper2_5_kept=',nsuper2_5_in,nsuper2_5_kept
     if(loop==2)     write(6,*)'READ_RADAR:  nsuper3_in,nsuper3_kept=',nsuper3_in,nsuper3_kept
     write(6,*)'READ_RADAR: # no vad match   =',novadmatch
     write(6,*)'READ_RADAR: # out of vadrange=',ioutofvadrange
     write(6,*)'READ_RADAR: # bad azimuths=',ibadazm
     write(6,*)'READ_RADAR: # bad winds   =',ibadwnd
     write(6,*)'READ_RADAR: # bad dists   =',ibaddist
     write(6,*)'READ_RADAR: # bad stahgts =',ibadstaheight
     write(6,*)'READ_RADAR: # bad obshgts =',ibadheight
     write(6,*)'READ_RADAR: # bad errors  =',ibaderror
     write(6,*)'READ_RADAR: # bad vadwnd  =',ibadvad
     write(6,*)'READ_RADAR: # bad fit     =',ibadfit 
     write(6,*)'READ_RADAR: # num thinned =',kthin
     write(6,*)'READ_RADAR: # notgood0    =',notgood0
     write(6,*)'READ_RADAR: # notgood     =',notgood
     write(6,*)'READ_RADAR: # hgt belowsta=',iheightbelowsta
     write(6,*)'READ_RADAR: timemin,max   =',timemin,timemax
     write(6,*)'READ_RADAR: errmin,max    =',errmin,errmax
     write(6,*)'READ_RADAR: dlatmin,max,dlonmin,max=',dlatmin,dlatmax,dlonmin,dlonmax
     write(6,*)'READ_RADAR: iaaamin,max,8*max_rrr  =',iaaamin,iaaamax,8*max_rrr

  end do       !   end bigger loop over first level 2.5, then level 3 radar data


! Write out vad statistics
  do ivad=1,nvad
     write(6,'(" fit of 2, 2.5, 3 data to vad station, lat, lon = ",a8,2f14.2)') &
        vadid(ivad),vadlat(ivad)*rad2deg,vadlon(ivad)*rad2deg
     do ivadz=1,maxvadbins
        if(vadcount2(ivad,ivadz)>half) then
           vadfit2(ivad,ivadz)=sqrt(vadfit2(ivad,ivadz)/vadwgt2(ivad,ivadz))
        else
           vadfit2(ivad,ivadz)=zero
        end if
        if(vadcount2_5(ivad,ivadz)>half) then
           vadfit2_5(ivad,ivadz)=sqrt(vadfit2_5(ivad,ivadz)/vadwgt2_5(ivad,ivadz))
        else
           vadfit2_5(ivad,ivadz)=zero
        end if
        if(vadcount3(ivad,ivadz)>half) then
           vadfit3(ivad,ivadz)=sqrt(vadfit3(ivad,ivadz)/vadwgt3(ivad,ivadz))
        else
           vadfit3(ivad,ivadz)=zero
        end if
        write(6,'(" h,f2,f2.5,f3=",i7,f10.2,"/",i5,f10.2,"/",i5,f10.2,"/",i5)')nint(ivadz*dzvad),&
           vadfit2(ivad,ivadz),nint(vadcount2(ivad,ivadz)),&
           vadfit2_5(ivad,ivadz),nint(vadcount2_5(ivad,ivadz)),&
           vadfit3(ivad,ivadz),nint(vadcount3(ivad,ivadz))
     end do
  end do

  deallocate(nobs_box)

65 continue



  erad = rearth
  thiserr=5.0_r_kind

  timemax=-huge(timemax)
  timemin=huge(timemin)
  errmax=-huge(errmax)
  errmin=huge(errmin)
  elevmax=-huge(elevmax)
  elevmin=huge(elevmin)

  loop=3

  nirrr=0
  noutside=0
  ntimeout=0
  nsubzero=0
  ibadazm=0
  ibadwnd=0
  ibaddist=0
  ibadtilt=0
  ibadrange=0
  ibadheight=0
  ibadstaheight=0
  notgood=0
  notgood0=0
  nread=0
  ntdrvr_in=0
  ntdrvr_kept=0
  ntdrvr_thin1=0
  ntdrvr_thin2=0
  ntdrvr_thin2_foreswp=0
  ntdrvr_thin2_aftswp=0
  maxout=0
  maxdata=0
  nmissing=0
  subset_check(3)='NC006070'
  icntpnt=0
  nswp=0
  nforeswp=0
  naftswp=0
  nfore=0
  naft=0

  xscale=100._r_kind
  xscalei=one/xscale
  max_rrr=nint(100000.0_r_kind*xscalei)
  jjj=0
  iimax=0

  if(loop == 3) outmessage='tail Doppler radar obs:'

  use_all = .true.
  do i=1,nconvtype
     if(trim(ioctype(i)) == trim(obstype) .and. ictype(i) < 999 .and. icuse(i) > 0)then
        ithin=ithin_conv(i)
        print *,'mtong i, ithin_conv(i)=', i, ithin_conv(i)
        if(ithin > 0)then
           rmesh=rmesh_conv(i)
           pmesh=pmesh_conv(i)
           use_all = .false.
           if(pmesh > zero) then ! Here pmesh is height in meters
              zflag=1
              nlevz=r16000/pmesh
           else
              zflag=0
              nlevz=nsig
           endif
           xmesh=rmesh
           call make3grids(xmesh,nlevz)
           allocate(zl_thin(nlevz))
           if (zflag==1) then
              do k=1,nlevz
                 zl_thin(k)=(k-1)*pmesh
              enddo
           endif
           write(6,*)'READ_RADAR: obstype,ictype,rmesh,zflag,nlevz,pmesh=',&
              trim(ioctype(i)),ictype(i),rmesh,zflag,nlevz,pmesh
           exit
        end if
     end if
  end do

  if(trim(infile) == 'tldplrso') goto 75

  nswptype=0
  nmrecs=0
  irec=0
  if(l_foreaft_thin)then
! read the first 500 records to deterine which criterion
! should be used to seperate fore/aft sweep
    open(lnbufr,file=trim(infile),form='unformatted')
    call openbf(lnbufr,'IN',lnbufr)
    call datelen(10)
    call readmg(lnbufr,subset,idate,iret)
    if(iret/=0) then
       write(6,*)'READ_RADAR: problem reading tail Doppler radar bufr file tldplrbufr'
       call closbf(lnbufr)
       go to 1100
    end if

!   Big loop over bufr file

700   call readsb(lnbufr,iret)
800   continue
    if(iret/=0) then
       call readmg(lnbufr,subset,idate,iret)
       if(iret/=0) go to 85
       go to 700
    end if
    if(subset/=subset_check(loop)) then
       iret=99
       go to 800
    end if
    nmrecs = nmrecs+1

!   Read header.  Extract elevation angle
    call ufbint(lnbufr,hdr,12,1,levs,hdrstr(2))
    thistilt=hdr(12)

    if(nmrecs == 1)then
      tdrele1 = hdr(12)
      tdrele2 = hdr(12)
    end if

    tdrele1 = tdrele2
    tdrele2 = hdr(12)
    if(abs(tdrele2-tdrele1)>r100) then
       print *,'tdrele2,tdrele1=',tdrele2,tdrele1
       nswptype=1
       go to 85
    end if

    if(nmrecs <= 500)then
       go to 700
    else
       go to 85
    end if

85 continue
    call closbf(lnbufr)
    close(lnbufr)
  end if

  print *,'nmrecs, nswptype=', nmrecs, nswptype

! Open, then read bufr data
  open(lnbufr,file=trim(infile),form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  call readmg(lnbufr,subset,idate,iret)
  if(iret/=0) then
     write(6,*)'READ_RADAR: problem reading tail Doppler radar bufr file tldplrbufr'
     call closbf(lnbufr)
     go to 1100
  end if

! Time offset
  call time_4dvar(idate,toff)

  write(date,'( i10)') idate
  read (date,'(i4,3i2)') iy,im,idd,ihh
  write(6,*)'READ_RADAR: bufr file date is ',iy,im,idd,ihh

  idate5(1) = iy    ! year
  idate5(2) = im    ! month
  idate5(3) = idd   ! day
  idate5(4) = ihh   ! hour
  idate5(5) = 0     ! minute
  call w3fs21(idate5,mincy)

  nmrecs=0
!    Big loop over bufr file

  if(l_foreaft_thin) then 
     firstbeam = 0
     foreswp = .true.
     aftswp = .false.
     nforeswp=1
     naftswp=0
     nswp=1
  else
     foreswp = .false.
     aftswp = .false.
  endif

70   call readsb(lnbufr,iret)
80   continue
  if(iret/=0) then
     call readmg(lnbufr,subset,idate,iret)
     if(iret/=0) go to 1100
     go to 70
  end if
  if(subset/=subset_check(loop)) then
     iret=99
     go to 80
  end if
  nmrecs = nmrecs+1
  irec = irec+1

!    Read header.  Extract station infomration
  call ufbint(lnbufr,hdr,12,1,levs,hdrstr(2))

! rstation_id=hdr(1)
  if(hdr(1) == zero)then
     cstaid='NOAA    '
  else if(hdr(1) == one)then
     cstaid='FRENCH  '
  else if(hdr(1)== two)then
     cstaid='G-IV    '
  else if(hdr(1)== three)then
     cstaid='AOC     '
  else
     cstaid='UNKNOWN '
  endif

  kx=990+nint(hdr(1))

  if(nmrecs==1)print *,'Antenna ID:', hdr(1),cstaid
  
  iyr = hdr(2)
  imo = hdr(3)
  idy = hdr(4)
  ihr = hdr(5)
  imn = hdr(6)
  isc = hdr(7)

  idate5(1) = iyr
  idate5(2) = imo
  idate5(3) = idy
  idate5(4) = ihr
  idate5(5) = imn
  ikx=0
  do i=1,nconvtype
     if(trim(ioctype(i)) == trim(obstype) .and. kx == ictype(i))ikx = i
  end do
  if(ikx == 0) go to 70
  call w3fs21(idate5,minobs)

  t4dv=real(minobs-iwinbgn,r_kind)*r60inv
  if (l4dvar.or.l4densvar) then
     if (t4dv<zero .OR. t4dv>winlen) then
        ntimeout=ntimeout+1
        goto 70
     end if
     timeo=t4dv
  else
     timeo = real(minobs-mincy,r_kind)*r60inv
     if (abs(timeo) > twind .or. abs(timeo) > ctwind(ikx)) then
        ntimeout=ntimeout+1
        goto 70
     end if
  endif

  timemax=max(timemax,timeo)
  timemin=min(timemin,timeo)

  this_stalat=hdr(8)
  this_stalon=hdr(9)

  rlon0=deg2rad*this_stalon
  this_stalatr=this_stalat*deg2rad
  clat0=cos(this_stalatr) ; slat0=sin(this_stalatr)
  this_stahgt=hdr(10)
  thisazimuth=hdr(11)
  thistilt=hdr(12)
  elevmax=max(elevmax,thistilt)
  elevmin=min(elevmin,thistilt)

!  define fore/aft sweeps for thinning (pseduo dual Doppler)

  if(l_foreaft_thin)then
     if (firstbeam == 0) then
        tdrele1 = hdr(12)
        tdrele2 = hdr(12)
        if(nswptype == 0)then
           tdrele3 = hdr(12)
        end if 
        firstbeam = 1
     endif

     if(nswptype == 0)then
        tdrele1 = tdrele2
        tdrele2 = tdrele3
        tdrele3 = hdr(12)

        if(firstbeam > 0 .and. tdrele2>=tdrele1 .and. tdrele2>=tdrele3 .and. tdrele2 > r60 &
           .and. irec > r150)then  
           if(foreswp) then
              foreswp = .false.
              aftswp = .true.
              naftswp = naftswp+1
              irec=0
           else
              aftswp = .false.
              foreswp = .true.
              nforeswp = nforeswp+1
              irec=0
           endif
   
           nswp = nswp+1
        endif

     else if(nswptype == 1)then
        tdrele1 = tdrele2
        tdrele2 = hdr(12)

        if(abs(tdrele2-tdrele1)>r100) then
           if(foreswp) then
              foreswp = .false.
              aftswp = .true.
              naftswp = naftswp+1
              irec=0
           else
              aftswp = .false.
              foreswp = .true.
              nforeswp = nforeswp+1
              irec=0
           endif

           nswp = nswp+1
        endif
     else
        foreswp = .false.
        aftswp = .false.
     end if
  else
     foreswp = .false.
     aftswp = .false.
  endif

  if(abs(thistilt)>r75)then
     ibadtilt=ibadtilt+1; goto 70
  endif

  staheight=this_stahgt
  if(staheight<-r1000.or.staheight>r50000) then
     ibadstaheight=ibadstaheight+1; goto 70
  end if

!    Go through the data levels
  call ufbint(lnbufr,tdr_obs,4,maxlevs,levs,datstr(2))
  if(levs>maxlevs) then
     write(6,*)'READ_RADAR:  ***ERROR*** increase read_radar bufr size since ',&
        'number of levs=',levs,' > maxlevs=',maxlevs
     call stop2(84)
  endif
! use local coordinate centered on this_stalat,this_stalon. note that global and local
! azimuth angle are the same at the origin (this_stalat,this_stalon)
! and azimuth angle is fixed in local coordinate along entire radial line.
! we convert back to global azimuth angle at each point along line
! at end of computation.  that way we avoid worrying about where poles are.

  aactual=erad+this_stahgt
  thistiltr=thistilt*deg2rad
  selev0=sin(thistiltr) ; celev0=cos(thistiltr)
  a43=four_thirds*aactual
  ii=0
  do k=1,levs
     nread=nread+1
!    Select data every 3 km along each beam
     if(MOD(INT(tdr_obs(1,k)-tdr_obs(1,1)),3000) < 100)then
        if(tdr_obs(3,k) >= 800.) nmissing=nmissing+1     !xx
        if(tdr_obs(3,k) < 800.) then
           ii=ii+1
           dopbin(ii)=tdr_obs(3,k)
           thisrange=tdr_obs(1,k)

           call getvrlocalinfo(thisrange,thisazimuth,this_stahgt,aactual,a43,selev0,celev0, &
                          rlon0,clat0,slat0,r8,r89_5,nsubzero,ii,z(ii),elev(ii),elat8(ii), &
                          elon8(ii),glob_azimuth8(ii))
        end if
     else
        ntdrvr_thin1=ntdrvr_thin1+1
     endif
  end do

! Further process tail Doppler radar Vr data
  iimax=max(iimax,ii)  

  if( ii > 0 )then
     dlat_earth=this_stalat    !station lat (degrees)
     dlon_earth=this_stalon    !station lon (degrees)
     if (dlon_earth>=r360) dlon_earth=dlon_earth-r360
     if (dlon_earth<zero ) dlon_earth=dlon_earth+r360
     dlat_earth = dlat_earth * deg2rad
     dlon_earth = dlon_earth * deg2rad

     clon=cos(dlon_earth)
     slon=sin(dlon_earth)
     clat=cos(dlat_earth)
     slat=sin(dlat_earth)

     do i=1,ii
        ntdrvr_in=ntdrvr_in+1
        tiltangle=elev(i)*deg2rad

!     Get observation (lon,lat).  Compute distance from radar.
        dlat_earth=elat8(i)
        dlon_earth=elon8(i)
        if(dlon_earth>=r360) dlon_earth=dlon_earth-r360
        if(dlon_earth<zero ) dlon_earth=dlon_earth+r360
        dlat_earth = dlat_earth*deg2rad
        dlon_earth = dlon_earth*deg2rad

        if(regional) then
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
           dlatmax=max(dlat,dlatmax)
           dlonmax=max(dlon,dlonmax)
           dlatmin=min(dlat,dlatmin)
           dlonmin=min(dlon,dlonmin)
           if (outside) then
              noutside=noutside+1
              cycle
           endif
        else
           dlat = dlat_earth
           dlon = dlon_earth
           call grdcrd1(dlat,rlats,nlat,1)
           call grdcrd1(dlon,rlons,nlon,1)
        endif
        clonh=cos(dlon_earth)
        slonh=sin(dlon_earth)
        clath=cos(dlat_earth)
        slath=sin(dlat_earth)
        cdist=slat*slath+clat*clath*(slon*slonh+clon*clonh)
        cdist=max(-one,min(cdist,one))
        dist=eradkm*acos(cdist)
        irrr=nint(dist*1000*xscalei)
        if(irrr<=0 .or. irrr>max_rrr)then
           nirrr=nirrr+1
           cycle
        endif

!     Extract radial wind data
        height= z(i)
        rwnd  = dopbin(i)
        azm_earth = glob_azimuth8(i)
        if(regional) then
           cosazm_earth=cos(azm_earth*deg2rad)
           sinazm_earth=sin(azm_earth*deg2rad)
           call rotate_wind_ll2xy(cosazm_earth,sinazm_earth,cosazm,sinazm,dlon_earth,dlon,dlat)
           azm=atan2(sinazm,cosazm)*rad2deg
        else
           azm=azm_earth
        end if
        iaaa=azm/(r360/(r8*irrr))
        iaaa=mod(iaaa,8*irrr)
        if(iaaa<0) iaaa=iaaa+8*irrr
        iaaa=iaaa+1
        iaaamax=max(iaaamax,iaaa)
        iaaamin=min(iaaamin,iaaa)
        error = erradar_inflate*thiserr
        errmax=max(error,errmax)
        if(thiserr>zero) errmin=min(error,errmin)

!     Perform limited qc based on azimuth angle, elevation angle, radial wind
!     speed, range, distance from radar site

        good0=.true.
        if(abs(azm)>r400) then
           ibadazm=ibadazm+1; good0=.false.
        end if
        if(abs(rwnd) > r71 .or. abs(rwnd) < r2 ) then
           ibadwnd=ibadwnd+1; good0=.false.
        end if
        if(thisrange>r92) then
           ibadrange=ibadrange+1; good0=.false.
        end if
        if(dist>r400) then
           ibaddist=ibaddist+1; good0=.false.
        end if
        if(height<-r1000.or.height>r50000) then
           ibadheight=ibadheight+1; good0=.false.
        end if
        good=.true.
        if(.not.good0) then
           notgood0=notgood0+1
           cycle
        end if
!     if data is good, load into output array

        if(good) then
           ntdrvr_kept=ntdrvr_kept+1
!####################       Data thinning       ###################

           icntpnt=icntpnt+1

           if(ithin > 0)then
              if(zflag == 0)then
                 klon1= int(dlon);  klat1= int(dlat)
                 dx   = dlon-klon1; dy   = dlat-klat1
                 dx1  = one-dx;     dy1  = one-dy
                 w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

                 klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
                 if (klon1==0) klon1=nlon
                 klatp1=min(nlat,klat1+1); klonp1=klon1+1
                 if (klonp1==nlon+1) klonp1=1
                 do kk=1,nsig
                    hges(kk)=w00*hgtl_full(klat1 ,klon1 ,kk) +  &
                             w10*hgtl_full(klatp1,klon1 ,kk) + &
                             w01*hgtl_full(klat1 ,klonp1,kk) + &
                             w11*hgtl_full(klatp1,klonp1,kk)
                 end do
                 sin2  = sin(dlat_earth)*sin(dlat_earth)
                 termg = grav_equator * &
                    ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
                 termr = semi_major_axis /(one + flattening + grav_ratio -  &
                    two*flattening*sin2)
                 termrg = (termg/grav)*termr
                 do k=1,nsig
                    zges(k) = (termr*hges(k)) / (termrg-hges(k))
                    zl_thin(k)=zges(k)
                 end do
              endif

              zobs = height

              ntmp=ndata  ! counting moved to map3gridS
              if (thin4d) then
                 timedif = zero
              else
                 timedif=abs(t4dv-toff)
              endif
              crit1 = timedif/r6+half

              call map3grids(1,zflag,zl_thin,nlevz,dlat_earth,dlon_earth,&
                 zobs,crit1,ndata,iout,icntpnt,iiout,luse,foreswp,aftswp)
              maxout=max(maxout,iout)
              maxdata=max(maxdata,ndata)

              if (.not. luse) then
                 if (foreswp) then
                    ntdrvr_thin2_foreswp=ntdrvr_thin2_foreswp+1  
                 else if (aftswp) then
                    ntdrvr_thin2_aftswp=ntdrvr_thin2_aftswp+1
                 end if
                 ntdrvr_thin2=ntdrvr_thin2+1
                 cycle
              endif
              if(iiout > 0) isort(iiout)=0
              if (ndata > ntmp) then
                 nodata=nodata+1
              endif
              isort(icntpnt)=iout

           else
              ndata =ndata+1
              nodata=nodata+1
              iout=ndata
              isort(icntpnt)=iout
           endif

           if(ndata > maxobs) then
              write(6,*)'READ_PREPBUFR:  ***WARNING*** ndata > maxobs for ',obstype
              ndata = maxobs
           end if

!       Set usage variable
           usage = zero

           if(icuse(ikx) < 0)usage=r100
           if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
              if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
           end if

           call deter_zsfc_model(dlat,dlon,zsges)

! Get information from surface file necessary for conventional data here
           call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,skint,ff10,sfcr)


           cdata(1) = error             ! wind obs error (m/s)
           cdata(2) = dlon              ! grid relative longitude
           cdata(3) = dlat              ! grid relative latitude
           cdata(4) = height            ! obs absolute height (m)
           cdata(5) = rwnd              ! wind obs (m/s)
           cdata(6) = azm*deg2rad       ! azimuth angle (radians)
           cdata(7) = t4dv              ! obs time (hour)
           cdata(8) = ikx               ! type
           cdata(9) = tiltangle         ! tilt angle (radians)
           cdata(10)= staheight         ! station elevation (m)
           cdata(11)= rstation_id       ! station id
           cdata(12)= usage             ! usage parameter
           cdata(13)= idomsfc           ! dominate surface type
           cdata(14)= skint             ! skin temperature
           cdata(15)= ff10              ! 10 meter wind factor
           cdata(16)= sfcr              ! surface roughness
           cdata(17)=dlon_earth*rad2deg ! earth relative longitude (degrees)
           cdata(18)=dlat_earth*rad2deg ! earth relative latitude (degrees)
           cdata(19)=dist               ! range from radar in km (used to estimate beam spread)
           cdata(20)=zsges              ! model elevation at radar site
           cdata(21)=thiserr
           cdata(22)=hdr(1)+three+one   ! tail Doppler radar
           do j=1,maxdat
              cdata_all(j,iout)=cdata(j)
           end do
           if(foreswp)nfore=nfore+1
           if(aftswp)naft=naft+1
           jjj=jjj+1
        else
           notgood = notgood + 1
        end if  ! if(good)

     end do

  endif ! if(ii .gt. 0)

! End of bufr read loop

69 continue

  go to 70

! Normal exit
1100 continue
  call closbf(lnbufr)


! Close unit to bufr file
  close(lnbufr)

  go to 1200

75 continue

! Loop to read TDR superobs data

  ikx=0
  do i=1,nconvtype
     if(trim(ioctype(i)) == trim(obstype))ikx = i
  end do
  if(ikx == 0) go to 900

  call w3fs21(iadate,mincy) ! analysis time in minutes

  open(lnbufr,file=trim(infile),form='formatted',err=300)
  rewind (lnbufr)
  do n=1,10
     istop=0
     read(lnbufr,'(a)',err=200,end=1200)filename
     print *,'filename=', trim(filename)
     open(25,file=trim(filename),form='formatted',access='sequential')
     do while (istop.eq.0)
        ii=1
        READ(25,'(I4,4I2,8F10.3)',iostat=istop) iyr,imo,idy,ihr,imn,this_stalat, &
        this_stalon,this_stahgt,azm0,elev0,range0,thisvr,rotang

        nread=nread+1
     
        idate5(1) = iyr
        idate5(2) = imo
        idate5(3) = idy
        idate5(4) = ihr
        idate5(5) = imn
        call w3fs21(idate5,minobs)
   
        t4dv=real(minobs-iwinbgn,r_kind)*r60inv
        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) goto 90
           timeo=t4dv
        else
           timeo = real(minobs-mincy,r_kind)*r60inv
           if (abs(timeo)>twind) goto 90
        endif

        timemax=max(timemax,timeo)
        timemin=min(timemin,timeo)
   
        rlon0=deg2rad*this_stalon
        this_stalatr=this_stalat*deg2rad
        clat0=cos(this_stalatr) ; slat0=sin(this_stalatr)
        thistilt=elev0
        elevmax=max(elevmax,thistilt)
        elevmin=min(elevmin,thistilt)
        thisazimuth=azm0
        thisrange=range0*r1000
        if(abs(thistilt)>r75)then
           ibadtilt=ibadtilt+1; goto 90
        endif
   
        staheight=this_stahgt
        if(staheight<-r1000.or.staheight>r50000) then
           ibadstaheight=ibadstaheight+1; goto 90
        end if

        aactual=erad+this_stahgt
        thistiltr=thistilt*deg2rad
        selev0=sin(thistiltr) ; celev0=cos(thistiltr)
        a43=four_thirds*aactual

         
        call getvrlocalinfo(thisrange,thisazimuth,this_stahgt,aactual,a43,selev0,celev0, &
                       rlon0,clat0,slat0,r8,r89_5,nsubzero,ii,z(ii),elev(ii),elat8(ii), &
                       elon8(ii),glob_azimuth8(ii))


        dlat_earth=this_stalat    !station lat (degrees)
        dlon_earth=this_stalon    !station lon (degrees)
        if (dlon_earth>=r360) dlon_earth=dlon_earth-r360
        if (dlon_earth<zero ) dlon_earth=dlon_earth+r360
        dlat_earth = dlat_earth * deg2rad
        dlon_earth = dlon_earth * deg2rad

        clon=cos(dlon_earth)
        slon=sin(dlon_earth)
        clat=cos(dlat_earth)
        slat=sin(dlat_earth)


        ntdrvr_in=ntdrvr_in+1
        tiltangle=elev(ii)*deg2rad

!     Get observation (lon,lat).  Compute distance from radar.
        dlat_earth=elat8(ii)
        dlon_earth=elon8(ii)
        if(dlon_earth>=r360) dlon_earth=dlon_earth-r360
        if(dlon_earth<zero ) dlon_earth=dlon_earth+r360
        dlat_earth = dlat_earth*deg2rad
        dlon_earth = dlon_earth*deg2rad

        if(regional) then
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
           dlatmax=max(dlat,dlatmax)
           dlonmax=max(dlon,dlonmax)
           dlatmin=min(dlat,dlatmin)
           dlonmin=min(dlon,dlonmin)
           if (outside) then
              noutside=noutside+1
              cycle
           endif
        else
           dlat = dlat_earth
           dlon = dlon_earth
           call grdcrd1(dlat,rlats,nlat,1)
           call grdcrd1(dlon,rlons,nlon,1)
        endif
        clonh=cos(dlon_earth)
        slonh=sin(dlon_earth)
        clath=cos(dlat_earth)
        slath=sin(dlat_earth)
        cdist=slat*slath+clat*clath*(slon*slonh+clon*clonh)
        cdist=max(-one,min(cdist,one))
        dist=eradkm*acos(cdist)
        irrr=nint(dist*1000*xscalei)
        if(irrr<=0 .or. irrr>max_rrr)then
           nirrr=nirrr+1
           cycle
        endif

!     Extract radial wind data
        height= z(ii)
        rwnd  = thisvr
        azm_earth = glob_azimuth8(ii)
        if(regional) then
           cosazm_earth=cos(azm_earth*deg2rad)
           sinazm_earth=sin(azm_earth*deg2rad)
           call rotate_wind_ll2xy(cosazm_earth,sinazm_earth,cosazm,sinazm,dlon_earth,dlon,dlat)
           azm=atan2(sinazm,cosazm)*rad2deg
        else
           azm=azm_earth
        end if
        iaaa=azm/(r360/(r8*irrr))
        iaaa=mod(iaaa,8*irrr)
        if(iaaa<0) iaaa=iaaa+8*irrr
        iaaa=iaaa+1
        iaaamax=max(iaaamax,iaaa)
        iaaamin=min(iaaamin,iaaa)
        error = erradar_inflate*thiserr
        errmax=max(error,errmax)
        if(thiserr>zero) errmin=min(error,errmin)

!     Perform limited qc based on azimuth angle, elevation angle, radial wind
!     speed, range, distance from radar site

        good0=.true.
        if(abs(azm)>r400) then
           ibadazm=ibadazm+1; good0=.false.
        end if
        if(abs(rwnd) > r71) then
           ibadwnd=ibadwnd+1; good0=.false.
        end if
        if(thisrange>r92) then
           ibadrange=ibadrange+1; good0=.false.
        end if
        if(dist>r400) then
           ibaddist=ibaddist+1; good0=.false.
        end if
        if(height<-r1000.or.height>r50000) then
           ibadheight=ibadheight+1; good0=.false.
        end if
        good=.true.
        if(.not.good0) then
           notgood0=notgood0+1
           cycle
        end if
!     if data is good, load into output array

        if(good) then
           ntdrvr_kept=ntdrvr_kept+1
!####################       Data thinning       ###################

           icntpnt=icntpnt+1

           if(ithin > 0)then
              if(zflag == 0)then
                 klon1= int(dlon);  klat1= int(dlat)
                 dx   = dlon-klon1; dy   = dlat-klat1
                 dx1  = one-dx;     dy1  = one-dy
                 w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

                 klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
                 if (klon1==0) klon1=nlon
                 klatp1=min(nlat,klat1+1); klonp1=klon1+1
                 if (klonp1==nlon+1) klonp1=1
                 do kk=1,nsig
                    hges(kk)=w00*hgtl_full(klat1 ,klon1 ,kk) +  &
                             w10*hgtl_full(klatp1,klon1 ,kk) + &
                             w01*hgtl_full(klat1 ,klonp1,kk) + &
                             w11*hgtl_full(klatp1,klonp1,kk)
                 end do
                 sin2  = sin(dlat_earth)*sin(dlat_earth)
                 termg = grav_equator * &
                    ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
                 termr = semi_major_axis /(one + flattening + grav_ratio -  &
                    two*flattening*sin2)
                 termrg = (termg/grav)*termr
                 do k=1,nsig
                    zges(k) = (termr*hges(k)) / (termrg-hges(k))
                    zl_thin(k)=zges(k)
                 end do
              endif

              zobs = height

              ntmp=ndata  ! counting moved to map3gridS
              if (thin4d) then
                 timedif = zero
              else
                 timedif=abs(t4dv-toff)
              endif
              crit1 = timedif/r6+half

              call map3grids(1,zflag,zl_thin,nlevz,dlat_earth,dlon_earth,&
                 zobs,crit1,ndata,iout,icntpnt,iiout,luse,.false.,.false.)
              maxout=max(maxout,iout)
              maxdata=max(maxdata,ndata)

              if (.not. luse) then
                 ntdrvr_thin2=ntdrvr_thin2+1
                 cycle
              endif
              if(iiout > 0) isort(iiout)=0
              if (ndata > ntmp) then
                 nodata=nodata+1
              endif
              isort(icntpnt)=iout

           else
              ndata =ndata+1
              nodata=nodata+1
              iout=ndata
              isort(icntpnt)=iout
           endif

           if(ndata > maxobs) then
              write(6,*)'READ_PREPBUFR:  ***WARNING*** ndata > maxobs for ',obstype
              ndata = maxobs
           end if

!       Set usage variable
           usage = zero

           if(icuse(ikx) < 0)usage=r100
           if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
              if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
           end if

           call deter_zsfc_model(dlat,dlon,zsges)

! Get information from surface file necessary for conventional data here
           call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,skint,ff10,sfcr)


           cdata(1) = error             ! wind obs error (m/s)
           cdata(2) = dlon              ! grid relative longitude
           cdata(3) = dlat              ! grid relative latitude
           cdata(4) = height            ! obs absolute height (m)
           cdata(5) = rwnd              ! wind obs (m/s)
           cdata(6) = azm*deg2rad       ! azimuth angle (radians)
           cdata(7) = t4dv              ! obs time (hour)
           cdata(8) = ikx               ! type
           cdata(9) = tiltangle         ! tilt angle (radians)
           cdata(10)= staheight         ! station elevation (m)
           cdata(11)= rstation_id       ! station id
           cdata(12)= usage             ! usage parameter
           cdata(13)= idomsfc           ! dominate surface type
           cdata(14)= skint             ! skin temperature
           cdata(15)= ff10              ! 10 meter wind factor
           cdata(16)= sfcr              ! surface roughness
           cdata(17)=dlon_earth*rad2deg ! earth relative longitude (degrees)
           cdata(18)=dlat_earth*rad2deg ! earth relative latitude (degrees)
           cdata(19)=dist               ! range from radar in km (used to estimate beam spread)
           cdata(20)=zsges              ! model elevation at radar site
           cdata(21)=thiserr
           cdata(22)=three+two          ! tail Doppler radar
           do j=1,maxdat
              cdata_all(j,iout)=cdata(j)
           end do
           jjj=jjj+1
        else
           notgood = notgood + 1
        end if  ! if(good)
        
90 continue
     end do ! end of loop, reading records of data
     close(25)

  end do ! end of loop, reading TDR so data files

1200 continue
  close(lnbufr)

  if (.not. use_all) then
     deallocate(zl_thin) 
     call del3grids
  endif

  write(6,*)'READ_RADAR: # records(beams) read in nmrecs=', nmrecs
  write(6,*)'READ_RADAR: # records out of time window =', ntimeout
  write(6,*)'READ_RADAR: # records with bad tilt=',ibadtilt
  write(6,*)'READ_RADAR: # records with bad station height =',ibadstaheight
  write(6,*)'READ_RADAR: # data read in nread=', nread 
  write(6,*)'READ_RADAR: # data with missing value nmissing=', nmissing
  write(6,*)'READ_RADAR: # data likely to be below sealevel nsubzero=', nsubzero
  write(6,*)'READ_RADAR: # data removed by thinning along the beam ntdrvr_thin1=', ntdrvr_thin1 
  write(6,*)'READ_RADAR: # data retained after thinning along the beam ntdrvr_in=', ntdrvr_in
  write(6,*)'READ_RADAR: # out of domain =', noutside
  write(6,*)'READ_RADAR: # out of range =', nirrr
  write(6,*)'READ_RADAR: # bad azimuths =',ibadazm
  write(6,*)'READ_RADAR: # bad winds (<2m/s or >71m/s) =',ibadwnd
  write(6,*)'READ_RADAR: # bad ranges   =',ibadrange
  write(6,*)'READ_RADAR: # bad distance from radar =',ibaddist
  write(6,*)'READ_RADAR: # bad obs height =',ibadheight
  write(6,*)'READ_RADAR: # bad data =',notgood0
  write(6,*)'READ_RADAR: # data retained after QC ntdrvr_kept=', ntdrvr_kept
  write(6,*)'READ_RADAR: # data removed by thinning mesh ntdrvr_thin2=', ntdrvr_thin2
  if(l_foreaft_thin)then
    write(6,*)'READ_RADAR: nforeswp,naftswp,nswp=',nforeswp,naftswp,nswp
    write(6,*)'READ_RADAR: ntdrvr_thin2_foreswp,ntdrvr_thin2_aftswp=',ntdrvr_thin2_foreswp,ntdrvr_thin2_aftswp
    write(6,*)'READ_RADAR: data retained for further processing nfore,naft=',nfore,naft
  end if
  write(6,*)'READ_RADAR: data retained for further processing =', jjj
  write(6,*)'READ_RADAR: timemin,max   =',timemin,timemax
  write(6,*)'READ_RADAR: elevmin,max   =',elevmin,elevmax
  write(6,*)'READ_RADAR: dlatmin,max,dlonmin,max=',dlatmin,dlatmax,dlonmin,dlonmax
  write(6,*)'READ_RADAR: iaaamin,max,8*max_rrr  =',iaaamin,iaaamax,8*max_rrr
  write(6,*)'READ_RADAR: iimax =',iimax

! Write observation to scratch file
  call count_obs(ndata,maxdat,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,ndata)
  deallocate(cdata_all)
  
900 continue

  return

300 write(6,*) 'read_radar open TDR SO file list failed '
   call stop2(555)
200 write(6,*) 'read_radar read TDR SO data failed '
   call stop2(555)
end subroutine read_radar

subroutine getvrlocalinfo(thisrange,thisazimuth,this_stahgt,aactual,a43,selev0,celev0, &
                          rlon0,clat0,slat0,r8,r89_5,nsubzero,ii,z,elev,elat8,elon8, &
                          glob_azimuth8)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getvrlocalinfo  following subroutine radar_bufr_read_all       
!   prgmmr: tong             org: np23                date: 2013-03-28
!
! abstract:  This routine calcuate radial wind elevation, elevation angle, 
!            earth lat lon and  and azimuth angle at observation location 
!
! program history log:
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block

  use kinds, only: r_kind,r_single,i_kind
  use constants, only: one,half,two,deg2rad,rad2deg,zero_single,rearth

  implicit none

  real(r_single) ,intent(in   ) :: thisrange,thisazimuth,a43,aactual,selev0,celev0
  real(r_kind)   ,intent(in   ) :: this_stahgt,rlon0,clat0,slat0,r8,r89_5
  integer(i_kind),intent(inout) :: nsubzero
  integer(i_kind),intent(inout) :: ii
  real(r_single) ,intent(out  ) :: elev,z,elat8,elon8,glob_azimuth8

! local variables
  real(r_single) b,c,epsh,h,ha,celev,selev,gamma  
  real(r_single) rad_per_meter
  real(r_kind) thisazimuthr,rlonloc,rlatloc,rlonglob,rlatglob,thislat,thislon
  real(r_kind) clat1,caz0,saz0,cdlon,sdlon,caz1,saz1

  rad_per_meter= one/rearth

! use 4/3rds rule to get elevation of radar beam
! (if local temperature available, then vertical position can be
! estimated with greater accuracy)
  b=thisrange*(thisrange+two*aactual*selev0)
  c=sqrt(aactual*aactual+b)
  ha=b/(aactual+c)
  epsh=(thisrange*thisrange-ha*ha)/(r8*aactual)
  h=ha-epsh
  z=this_stahgt+h
  if(z < zero_single)then ! don't use observation if it is likely to be below sealevel
     nsubzero=nsubzero+1
     ii=ii-1
  else

! Get elevation angle at obs location
     celev=celev0
     selev=selev0
     if(thisrange>=one) then
        celev=a43*celev0/(a43+h)
        selev=(thisrange*thisrange+h*h+two*a43*h)/(two*thisrange*(a43+h))
     end if
     elev=rad2deg*atan2(selev,celev)
     gamma=half*thisrange*(celev0+celev)

! Get earth lat lon at obs location
     thisazimuthr=thisazimuth*deg2rad
     rlonloc=rad_per_meter*gamma*cos(thisazimuthr)
     rlatloc=rad_per_meter*gamma*sin(thisazimuthr)
     call invtllv(rlonloc,rlatloc,rlon0,clat0,slat0,rlonglob,rlatglob)
     thislat=rlatglob*rad2deg
     thislon=rlonglob*rad2deg
! Keep away from poles
     if(abs(thislat)>r89_5)then
        ii=ii-1
     else
        elat8=thislat
        elon8=thislon
! Get corrected azimuth
        clat1=cos(rlatglob)
        caz0=cos(thisazimuthr)
        saz0=sin(thisazimuthr)
        cdlon=cos(rlonglob-rlon0)
        sdlon=sin(rlonglob-rlon0)
        caz1=clat0*caz0/clat1
        saz1=saz0*cdlon-caz0*sdlon*slat0
        glob_azimuth8=atan2(saz1,caz1)*rad2deg
     end if
  end if
 
  return
end subroutine getvrlocalinfo

