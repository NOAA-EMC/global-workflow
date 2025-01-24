!  SUBSET=NC006001 -- level 3 superobs
!  SUBSET=NC006002 -- level 2.5 superobs
!  SUBSET=NC006070 -- RADIAL WIND FROM P3 RADAR
!  SUBSET=NC0062XX -- RADIAL WIND FROM Level 2 radar obs on grid-tilt(column-tilt)
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
!   2015-10-01  guo     - consolidate use of ob location (in deg)
!   2016-09-xx  CAPS(G. Zhao) -  add capability reading CAPS column-tilt radar radial wind data
!   2016-12-21  lippi/carley - add logic to run l2rw loop (==0) or run loop for l3rw and l2_5rw (==1,2) 
!                              to help fix a multiple data read bug (when l2rwbufr and radarbufr were both 
!                              listed in the OBS_INPUT table) and for added flexibility for experimental setups.
!   2018-02-15  wu      - add code for fv3_regional option
!   2020-05-04  wu   - no rotate_wind for fv3_regional
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
  use obsmod, only: iadate,ianldate,l_foreaft_thin,reduce_diag
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,time_4dvar,thin4d
  use gridmod, only: regional,nlat,nlon,tll2xy,rlats,rlons,rotate_wind_ll2xy,nsig,&
      fv3_regional
  use gridmod, only: wrf_nmm_regional,nems_nmmb_regional,cmaq_regional,wrf_mass_regional
  use gridmod, only: fv3_regional
  use convinfo, only: nconvtype,ctwind, &
      ncmiter,ncgroup,ncnumgrp,icuse,ictype,ioctype,ithin_conv,rmesh_conv,pmesh_conv,pmot_conv
  use convthin, only: make3grids,map3grids_m,del3grids,use_all
  use mpimod, only: npe
  use gsi_io, only: verbose
  use mpimod, only: mype
  use directDA_radaruse_mod, only: oe_rw, lvldbg, refl_lowbnd_rw
  use directDA_radaruse_mod, only: l_correct_azmu, l_correct_tilt, i_correct_tilt, &
                               l_azm_east1st, l_plt_diag_rw
  use directDA_radaruse_mod, only: l_use_rw_columntilt
  use deter_sfc_mod, only: deter_sfc2,deter_zsfc_model

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
  logical lexist3
  
  character(10) date
  character(80) hdrstr(2),datstr(2)
  character(8) subset,subset_check(3)
  character(30) outmessage
  character(255) filename
  
  integer(i_kind) lnbufr,i,j,k,maxobs,n,istop
  integer(i_kind) nmrecs,ibadazm,ibadtilt,ibadrange,ibadwnd,ibaddist,ibadheight,ibadvad,kthin
  integer(i_kind) iyr,imo,idy,ihr,imn,isc,ithin
  integer(i_kind) ibadstaheight,ibaderror,notgood,idate,iheightbelowsta,ibadfit
  integer(i_kind) notgood0
  integer(i_kind) novadmatch,ioutofvadrange
  integer(i_kind) iy,im,idd,ihh,iret,levs,mincy,minobs,kx0,kxadd,kx
  integer(i_kind) nreal,nchanl,ilat,ilon,ikx
  integer(i_kind),dimension(5):: idate5
  integer(i_kind) ivad,ivadz,nvad,idomsfc

  integer(i_kind) idbz_bad, idbz_clr, idbz_cld, ibadwnd_cld
  integer(i_kind) ioutside_rdr, ioutside_obs, ioutside_tim, iazmoutrange
  integer(i_kind) icld_goodwnd, icld_badwnd, idbz_bad2
  integer(i_kind) itiltoutrange, iohgtoutrange, iorngoutrange
  integer(i_kind) itiltoutrange2
  integer(i_kind) nmsg,ntb
  integer(i_kind) ireadmg, ireadsb
  integer(i_kind)                :: igoodwnd
  integer(i_kind), dimension(30) :: igoodwnd_dbz
  
  real(r_kind) timeb,rmesh,usage,ff10,sfcr,skint,t4dv,t4dvo,toff
  real(r_kind) eradkm,dlat_earth,dlon_earth
  real(r_kind) dlat_earth_deg,dlon_earth_deg
  real(r_kind) dlat,dlon,staheight,tiltangle,clon,slon,clat,slat
  real(r_kind) timeo,clonh,slonh,clath,slath,cdist,dist
  real(r_kind) rwnd,azm,height,error,wqm
  real(r_kind) azm_earth,cosazm_earth,sinazm_earth,cosazm,sinazm
  real(r_kind):: zsges

  real(r_kind) dhdr,dsdr,rdbz,range1
  real(r_kind) sfcrng0, elvang0

  real(r_double),dimension(12):: hdr2
  real(r_double),dimension(12):: hdr3

! ====================================================================!
!       variables used for L2RWBUFR data (column-tilt RW)
!
! --- BUFR Key word variables for l2rwbufr_colmtilt by CAPS f88d2arps
  character(80):: hdrstr2(3), obstr
  real(r_kind) :: thiserr_rw
  real(r_double),dimension(5,maxlevs):: radar_obs_columntilt   ! to read in column-tilt radar obs in l2rwbufr

! L2 radar data number statistics
  character(4),     dimension(1000) :: radartable_id
  integer(i_kind),  dimension(1000) :: radartable_inside
  integer(i_kind),  dimension(1000) :: radartable_ncolm
  integer(i_kind),  dimension(1000) :: radartable_ncolm_ins
  integer(i_kind),  dimension(1000) :: radartable_ncolm_out
  integer(i_kind),  dimension(1000) :: radartable_ncolm_outtimw
  integer(i_kind),  dimension(1000) :: radartable_ndata_use
  integer(i_kind)   :: icnt_radar_read
  integer(i_kind)   :: icnt_radar_inside
  integer(i_kind)   :: icnt_radar_outside
  integer(i_kind)   :: irdr, irdr_ptr, icnt
  integer(i_kind)   :: ntot_col, ntot_col_o, ntot_col_i, ntot_dat_u, ntot_col_ot
  integer(i_kind)   :: irpt

! filename for level 2 radar obs data on column-tilt (produced by 88d2arps)
  character(255) :: fname_l2rwbufr_columntilt
! ====================================================================!

  real(r_kind) thisazm, azm_east
  integer(i_kind) ncolumntiltl2_in,ncolumntiltl2_kept

  
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
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1

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
  integer(i_kind) iout
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

  logical, allocatable,dimension(:)     :: rusage,rthin
  logical save_all
! integer(i_kind)  numthin,numqc,numrem
  integer(i_kind) nxdata,pmot,numall
  
  data lnbufr/10/
  data hdrstr(1) / 'CLAT CLON SELV ANEL YEAR MNTH DAYS HOUR MINU MGPT' /
  data hdrstr(2) / 'PTID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON HSMSL ANAZ ANEL' /
  data datstr(1) / 'STDM SUPLAT SUPLON HEIT RWND RWAZ RSTD' /
  data datstr(2) / 'DIST HREF DMVR DVSW' /

! used for BUFR Radial Wind on column-tilt
  data hdrstr2(1) /'SSTN CLAT CLON HSMSL HSALG ANAZ XOB YOB'/
  data hdrstr2(2) /'YEAR MNTH DAYS HOUR MINU SECO'/
  data hdrstr2(3) /'SCID HNQV VOCP VOID'/
  data obstr      /'ANEL HEIT DISTL2RW DMVR HREF'/

  data ithin / -9 /
  data rmesh / -99.999_r_kind /
  logical print_verbose 
!***********************************************************************************
  print_verbose=.false.
  if(verbose)print_verbose=.true.

! Check to see if radar wind files exist.  If none exist, exit this routine.
  inquire(file='radar_supobs_from_level2',exist=lexist1)
  inquire(file=trim(infile),exist=lexist2)

  lexist3=.false.
  if ( l_use_rw_columntilt) then
! add level 2 bufr format RW data on column-tilt
     fname_l2rwbufr_columntilt='l2rwbufr_cltl'
     inquire(file=trim(fname_l2rwbufr_columntilt),exist=lexist3)
     if ( .not. lexist3 )  then
         write(6,'(1x,A70,A20,A10,I6)') &
             ' level-2 column-tilt radar wind file is not found and to skip : ', &
             trim(fname_l2rwbufr_columntilt), ' on pe:',mype
     else
         write(6,'(1x,A70,A20,A10,I6)') &
             ' level-2 column-tilt radar wind file is found and to read : ', &
             trim(fname_l2rwbufr_columntilt), ' on pe:',mype
     end if
  end if

  if (.not.lexist1 .and. .not.lexist2 .and. .not.lexist3) return

  eradkm=rearth*0.001_r_kind
  maxobs=4e6
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

  if(ianldate > 2016092000)then
     hdrstr(2)='PTID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON FLVLST ANAZ ANEL'
  end if

  allocate(cdata_all(maxdat,maxobs),rusage(maxobs),rthin(maxobs))

  rusage=.true.
  rthin=.false.

  if (trim(infile) /= 'tldplrbufr' .and. trim(infile) /= 'tldplrso') then

!    Initialize variables
!    vad_leash=.1_r_kind
     vad_leash=.3_r_kind
 !   xscale=5000._r_kind
 !   xscale=10000._r_kind
     xscale=20000._r_kind
     if(print_verbose)then
        write(6,*)'READ_RADAR:  set vad_leash,xscale=',vad_leash,xscale
        write(6,*)'READ_RADAR:  set maxvadbins,maxbadbins*dzvad=',maxvadbins,&
           maxvadbins*dzvad
     end if
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

!    First read in all vad winds so can use vad wind quality marks to decide 
!    which radar data to keep
!    Open, then read bufr data

     open(lnbufr,file=vadfile,form='unformatted')
     call openbf(lnbufr,'IN',lnbufr)
     call datelen(10)

     loop0: do
        call readsb(lnbufr,iret)
         if(iret/=0) then
            call readmg(lnbufr,subset,idate,iret)
            if(iret/=0) exit loop0
            cycle loop0
         end if
         call ufbint(lnbufr,hdr,7,1,levs,'SID XOB YOB DHR TYP SAID TSB')
         kx=nint(hdr(5))
         if(kx /= 224)cycle loop0     !  for now just hardwire vad wind type
         if(kx==224 .and. .not.newvad) then
           if(hdr(7)==2) then
               newvad=.true.
               exit loop0
            end if
         end if 
!        End of bufr read loop
     end do loop0

     call closbf(lnbufr)
     close(lnbufr)

!     enddo msg_report

     open(lnbufr,file=vadfile,form='unformatted')
     call openbf(lnbufr,'IN',lnbufr)
     call datelen(10)
     call readmg(lnbufr,subset,idate,iret)
     if(iret==0) then

!       Time offset
        call time_4dvar(idate,toff)

        write(date,'( i10)') idate
        read (date,'(i4,3i2)') iy,im,idd,ihh 
        if(print_verbose) &
           write(6,*)'READ_RADAR:  first read vad winds--use vad quality marks to qc 2.5/3 radar winds'

!       Big loop over vadwnd bufr file
        loop1: do
           call readsb(lnbufr,iret)
           if(iret/=0) then
              call readmg(lnbufr,subset,idate,iret)
              if(iret/=0) exit loop1
              cycle loop1
           end if
           nmrecs = nmrecs+1

!          Read header.  Extract station infomration
           call ufbint(lnbufr,hdr,7,1,levs,'SID XOB YOB DHR TYP SAID TSB')
           kx=nint(hdr(5))
           if(kx /= 224) cycle loop1       !  for now just hardwire vad wind type
   
!          write(6,*)'new vad::',newvad, hdr(7)
           if(.not.newvad .and. hdr(7)==2) cycle loop1
           if(newvad .and. hdr(7)/=2) cycle loop1
                                  !  and don't worry about subtypes
!          Is vadwnd in convinfo file
           ikx=0
           do i=1,nconvtype
              if(kx == ictype(i)) then
                 ikx=i
                 exit
              end if
           end do
           if(ikx == 0) cycle loop1

!          Time check
           t4dv=toff+hdr(4)
           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .OR. t4dv>winlen) cycle loop1 ! outside time window
           else
              timeb=hdr(4)
              if(abs(timeb) > ctwind(ikx) .or. abs(timeb) > half) cycle loop1 ! outside time window 
           endif

!          Create table of vad lat-lons and quality marks in 500m increments
!          for cross-referencing bird qc against radar winds
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

!          Update vadqm table
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
     

!       End of bufr read loop
        end do loop1

!    Normal exit
     end if
     call closbf(lnbufr)
     close(lnbufr)


!    Print vadwnd table
     if(nvad>0) then
        do ivad=1,nvad
           do ivadz=1,maxvadbins
              vadu(ivad,ivadz)=vadu(ivad,ivadz)/max(one,vadcount(ivad,ivadz))
              vadv(ivad,ivadz)=vadv(ivad,ivadz)/max(one,vadcount(ivad,ivadz))
           end do
           if(print_verbose) &
              write(6,'(" n,lat,lon,qm=",i3,2f8.2,2x,25i3)') &
              ivad,vadlat(ivad)*rad2deg,vadlon(ivad)*rad2deg,(max(-9,nint(vadqm(ivad,k))),k=1,maxvadbins)
        end do
     end if
     if(print_verbose)write(6,*)' errzmax=',errzmax
  
!     Allocate thinning grids around each radar
!     space needed is nvad*max_rrr*max_rrr*8*max_zzz
!
!         max_rrr=20
!         maxvadbins=20
!         nvad=150
!         space=150*20*20*8*20 = 64000*150=9600000  peanuts
  
     allocate(nobs_box(max_rrr,8*max_rrr,maxvadbins,nvad))
     nobs_box=0

!    Set level2_5 to 0.  Then loop over routine twice, first looking for
!    level 2.5 data, and setting level2_5=count of 2.5 data for any 2.5 data
!    available that passes the vad tests.  The second pass puts in level 3
!    data where it is available and no level 2.5 data was saved/available 
!    (level2_5=0)

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
   
!    First process any level 2 superobs.
!    Initialize variables.
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

!     LEVEL_TWO_READ: if(loop==0 .and. sis=='l2rw') then
     if(loop==0) outmessage='level 2 superobs:'

!    Open sequential file containing superobs
     open(lnbufr,file='radar_supobs_from_level2',form='unformatted')
     rewind lnbufr
     pmot=0
     if(ikx /= 0)then
        pmot=pmot_conv(ikx)
        if(reduce_diag .and. pmot < 2)pmot=pmot+2
        save_all = .false.
        if(pmot /= 2 .and. pmot /= 0)save_all=.true.
     end if

 !    dist2max=-huge(dist2max)
 !    dist2min=huge(dist2min)

!    Loop to read superobs data file
     superobs:do
        if(ikx == 0) exit superobs
        read(lnbufr,iostat=iret)this_staid,this_stalat,this_stalon,this_stahgt, &
           thistime,thislat,thislon,thishgt,thisvr,corrected_azimuth,thiserr,corrected_tilt
        if(iret/=0) exit
        nsuper2_in=nsuper2_in+1

        dlat_earth=this_stalat    !station lat (degrees)
        dlon_earth=this_stalon    !station lon (degrees)
        if (dlon_earth>=r360) dlon_earth=dlon_earth-r360
        if (dlon_earth<zero ) dlon_earth=dlon_earth+r360
        dlat_earth_deg = dlat_earth
        dlon_earth_deg = dlon_earth
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

!       Find vad wind match
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

!       Get model terrain at VAD wind location
        call deter_zsfc_model(dlatvad,dlonvad,zsges)

        t4dvo=toff+thistime
        timemax=max(timemax,t4dvo)
        timemin=min(timemin,t4dvo)

!       Exclude data if it does not fall within time window
        if (l4dvar.or.l4densvar) then
           if (t4dvo<zero .OR. t4dvo>winlen) cycle
        else
           timeo=thistime
           if(abs(timeo)>half ) cycle
        endif

!       Get observation (lon,lat).  Compute distance from radar.
        dlat_earth=thislat
        dlon_earth=thislon
        if(dlon_earth>=r360) dlon_earth=dlon_earth-r360
        if(dlon_earth<zero ) dlon_earth=dlon_earth+r360
     
        dlat_earth_deg = dlat_earth
        dlon_earth_deg = dlon_earth
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

!       Extract radial wind data
        height= thishgt
        rwnd  = thisvr
        azm_earth = corrected_azimuth
        if(regional .and. .not. fv3_regional) then
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
     
!       Perform limited qc based on azimuth angle, radial wind
!       speed, distance from radar site, elevation of radar,
!       height of observation, observation error, and goodness of fit to vad wind

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

!          Check fit to vad wind and vad wind quality mark
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

!       If data is good, load into output array
        if(good) then
           nsuper2_kept=nsuper2_kept+1
           level2(ivad)=level2(ivad)+1
           nobs_box(irrr,iaaa,ivadz,ivad)=nobs_box(irrr,iaaa,ivadz,ivad)+1
           ndata    =min(ndata+1,maxobs)
           nodata   =min(nodata+1,maxobs)  
           usage = zero
           if(icuse(ikx) < 0)usage=r100
           if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
              if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
           end if
           if(usage >= 100._r_kind) rusage(ndata)=.false.
  
           call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,skint,ff10,sfcr)
   
           LEVEL_TWO_READ: if(loop==0 .and. sis=='l2rw') then      
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
              cdata(17)=dlon_earth_deg     ! earth relative longitude (degrees)
              cdata(18)=dlat_earth_deg     ! earth relative latitude (degrees)
              cdata(19)=dist               ! range from radar in km (used to estimate beam spread)
              cdata(20)=zsges              ! model elevation at radar site
              cdata(21)=thiserr
              cdata(22)=two

!             if(vadid(ivad)=='0303LWX') then
!                dist2max=max(dist2max,dist)
!                dist2min=min(dist2min,dist)
!             end if

              do i=1,maxdat
                 cdata_all(i,ndata)=cdata(i)
              end do
           END IF LEVEL_TWO_READ
        
        else
           notgood = notgood + 1
        end if

     end do superobs

     close(lnbufr)     ! A simple unformatted fortran file should not be mixed with a bufr I/O
     nread=nsuper2_kept

     LEVEL_TWO_READ_2: if(loop==0 .and. sis=='l2rw') then      
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
     END IF LEVEL_TWO_READ_2

     LEVEL_THREE_READ: if(sis=='l3rw' .or. sis=='rw') then
!     Next process level 2.5 and 3 superobs

!     Bigger loop over first level 2.5 data, and then level3 data

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
!          dist2_5max=-huge(dist2_5max)
!          dist2_5min=huge(dist2_5min)
   
           if(loop==1)     outmessage='level 2.5 superobs:'
           if(loop==2)     outmessage='level 3 superobs:'

           idate5(1) = iy    ! year
           idate5(2) = im    ! month
           idate5(3) = idd   ! day
           idate5(4) = ihh   ! hour
           idate5(5) = 0     ! minute
           call w3fs21(idate5,mincy)
      
           nmrecs=0
   
!          Open, then read bufr data
           open(lnbufr,file=trim(infile),form='unformatted')
   
           call openbf(lnbufr,'IN',lnbufr)
           call datelen(10)
           call readmg(lnbufr,subset,idate,iret)
           if(iret==0) then
   
!          Big loop over bufr file
   
             loop2: do
                call readsb(lnbufr,iret)
                if(iret/=0) then
                   call readmg(lnbufr,subset,idate,iret)
                   if(iret/=0) exit loop2
                   cycle loop2
                end if
                if(subset/=subset_check(loop)) then
                   call readmg(lnbufr,subset,idate,iret)
                   if(iret/=0) exit loop2
                   cycle loop2
                end if
                nmrecs = nmrecs+1
        
     
!               Read header.  Extract station infomration
                call ufbint(lnbufr,hdr,10,1,levs,hdrstr(1))
   
    !           rstation_id=hdr(1)        !station id
                write(cstaid,'(2i4)')idint(hdr(1)),idint(hdr(2))
                if(cstaid(1:1)==' ')cstaid(1:1)='S'
                dlat_earth=hdr(1)         !station lat (degrees)
                dlon_earth=hdr(2)         !station lon (degrees)
                if (dlon_earth>=r360) dlon_earth=dlon_earth-r360
                if (dlon_earth<zero ) dlon_earth=dlon_earth+r360
   
                if (wrf_nmm_regional.or.nems_nmmb_regional.or.cmaq_regional.or.wrf_mass_regional&
                     .or. fv3_regional ) then
                   if(loop==1) then 
                      if(dlon_earth>230.0_r_kind .and.  &
                         dlat_earth <54.0_r_kind)then
                         cycle loop2
                      end if
                   end if
                end if
                dlat_earth = dlat_earth * deg2rad
                dlon_earth = dlon_earth * deg2rad
             
                if(regional)then
                   call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
                   if (outside) cycle loop2
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
   
!               Find vad wind match
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
                   cycle loop2
                end if
          
                vadlon_earth=vadlon(ivad)
                vadlat_earth=vadlat(ivad)
                if(regional)then
                   call tll2xy(vadlon_earth,vadlat_earth,dlonvad,dlatvad,outside)
                   if (outside) cycle loop2
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
     
!               Get model terrain at VAD wind location
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
                if(ikx==0) cycle loop2
                pmot=pmot_conv(ikx)
                if(reduce_diag .and. pmot < 2)pmot=pmot+2
                save_all = .false.
                if(pmot /= 2 .and. pmot /= 0)save_all=.true.
                call w3fs21(idate5,minobs)
                t4dv=real(minobs-iwinbgn,r_kind)*r60inv
                if (l4dvar.or.l4densvar) then
                   if (t4dv<zero .OR. t4dv>winlen) cycle loop2
                else
                   timeb = real(minobs-mincy,r_kind)*r60inv
!                  if (abs(timeb)>twind .or. abs(timeb) > ctwind(ikx)) then
                   if (abs(timeb)>half .or. abs(timeb) > ctwind(ikx)) then 
!                     write(6,*)'READ_RADAR:  time outside window ',timeb,' skip this obs'
                      cycle loop2
                   endif
                endif
   
!               Go through the data levels
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
     
!                  Exclude data if it does not fall within time window
                   if (l4dvar.or.l4densvar) then
                      if (t4dvo<zero .OR. t4dvo>winlen) cycle
                      timeo=t4dv
                   else
                      timeo=(real(minobs-mincy,r_kind)+real(radar_obs(1,k),r_kind))*r60inv
                      if(abs(timeo)>twind .or. abs(timeo) > ctwind(ikx)) then
!                        write(6,*)'READ_RADAR:  time outside window ',timeo,&
!                           ' skip obs ',nread,' at lev=',k
                         cycle
                      end if
                   end if
   
!                  Get observation (lon,lat).  Compute distance from radar.
                   if(radar_obs(3,k)>=r360) radar_obs(3,k)=radar_obs(3,k)-r360
                   if(radar_obs(3,k)<zero ) radar_obs(3,k)=radar_obs(3,k)+r360
   
                   dlat_earth_deg = radar_obs(2,k)
                   dlon_earth_deg = radar_obs(3,k)
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
   
!                  Set observation "type" to be function of distance from radar
                   kxadd=nint(dist*one_tenth)
                   kx=kx0+kxadd
     
!                  Extract radial wind data
                   height= radar_obs(4,k)
                   rwnd  = radar_obs(5,k)
                   azm_earth   = r90-radar_obs(6,k)
                   if(regional .and. .not. fv3_regional) then
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
   
!               Increase error for lev2.5 and lev3
                   if (wrf_nmm_regional.or.nems_nmmb_regional.or.cmaq_regional.or.wrf_mass_regional &
                        .or. fv3_regional ) then
                      if(dlon_earth*rad2deg>230.0_r_kind .and.  &
                         dlat_earth*rad2deg <54.0_r_kind)then
                         error = error+r10
                      end if
                   end if
                   errmax=max(error,errmax)
                   if(radar_obs(7,k)>zero) errmin=min(error,errmin)
                
!                  Perform limited qc based on azimuth angle, radial wind
!                  speed, distance from radar site, elevation of radar,
!                  height of observation, observation error.
   
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
     
!                   Check against vad wind quality mark
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
   
!                  If data is good, load into output array
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
                      nodata = min(nodata+1,maxobs)  
                      usage  = zero
                      if(icuse(ikx) < 0)usage=r100
                      if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
                         if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
                      end if
                      if(usage >= 100._r_kind) rusage(ndata)=.false.
                
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
                      cdata(17)=dlon_earth_deg     ! earth relative longitude (degrees)
                      cdata(18)=dlat_earth_deg     ! earth relative latitude (degrees)
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
             
!               End of k loop over levs
                end do
   
!            End of bufr read loop
             end do loop2
           end if
     
!          Normal exit

!          Close unit to bufr file
           call closbf(lnbufr)
           close(lnbufr)
   
   
           write(6,*)'READ_RADAR:  ',trim(outmessage),' reached eof on 2.5/3 superob radar file.'
   
           if(loop==1)write(6,*)'READ_RADAR:  nsuper2_5_in,nsuper2_5_kept=',nsuper2_5_in,nsuper2_5_kept
           if(loop==2)write(6,*)'READ_RADAR:  nsuper3_in,nsuper3_kept=',nsuper3_in,nsuper3_kept
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
     END IF LEVEL_THREE_READ


!!
     if ( l_use_rw_columntilt) then
!=========================================================================================!
! ------------------------------------------------------- !
!  Next process level 2 radar obs (rw+dbz) on column-tilt !
!           produced by  88d2arps of CAPS                 !
!  Note:                                                  !
!        It is assumed that these data have been          !
!        processed with good quality control,             !
!        so no VAD match is required for these data.      !
!        Horizontally they are on model grid,             !
!        vertically on radar scan tilt,                   !
!        so the thinning is different to level 2/2.5/3    !
!        superobs radar data. (thinning is not ready for  !
!        this version.                                    !
! ------------------------------------------------------- !
! =========================================================================================!

!  Bigger loop over first level 2.5 data, and then level3 data

        timemax=-huge(timemax)
        timemin=huge(timemin)
        errmax=-huge(errmax)
        errmin=huge(errmin)
        ncolumntiltl2_in=0
        ncolumntiltl2_kept=0
        loop=-1

        thiserr_rw=oe_rw          ! user-defined obs error for radial wind in namelist
        write(6,*)'READ_RADAR: L2RW radar radial wind obs error (user-definedin namelist)=', &
                  thiserr_rw,'(m/s).'

        numhits=0
        ibadazm=0
        ibadwnd=0
        ibadwnd_cld=0
        igoodwnd=0
        igoodwnd_dbz(:)=0
        ibaddist=0
        ibadheight=0
        ibadstaheight=0
        iheightbelowsta=0
        ibaderror=0
        ibadvad=0
        ibadfit=0
        ioutofvadrange=0
        idbz_bad=0
        idbz_bad2=0
        idbz_clr=0
        idbz_cld=0
        ioutside_rdr=0
        ioutside_tim=0
        ioutside_obs=0
        iazmoutrange=0
        itiltoutrange=0
        itiltoutrange2=0
        iohgtoutrange=0
        iorngoutrange=0
        icld_goodwnd=0
        icld_badwnd=0
        kthin=0
        novadmatch=0
        notgood=0
        notgood0=0
        radartable_id(:)=''
        radartable_inside(:)=0             !  1: radar is inside
                                           ! -1: outside
                                           !  0: initial value
        radartable_ncolm(:)=0
        radartable_ncolm_ins(:)=0
        radartable_ncolm_out(:)=0
        radartable_ncolm_outtimw(:)=0
        radartable_ndata_use(:)=0
        icnt_radar_read=0
        icnt_radar_inside=0
        icnt_radar_outside=0
   
        if (l_azm_east1st) then
            write(6,*)'READ_RADAR: change azimuth to east as 0 before correct it. :',l_azm_east1st
        else
            write(6,*)'READ_RADAR: change azimuth to east as 0 after  correct it. :',l_azm_east1st
        end if
        if (i_correct_tilt ==1) then
            write(6,*)'READ_RADAR: use GSI  code to correct tilt angle. :',i_correct_tilt
        else
            write(6,*)'READ_RADAR: use ARPS code to correct tilt angle. :',i_correct_tilt
        end if
   
        if(loop==-1)     outmessage='level 2 radar obs (column-tilt):'
   
!       Open, then read bufr data
        open(lnbufr,file=trim(fname_l2rwbufr_columntilt),form='unformatted')
   
        call openbf(lnbufr,'IN',lnbufr)
        call datelen(10)
   
        nmsg=0
        ntb=0
        nmrecs=0
        msg_report: do while (ireadmg(lnbufr,subset,idate) == 0)
            nmsg=nmsg+1
            if(subset(1:6)/='NC0062') then
                write(6,*)'mype=',mype,' subset check does not pass for L2 columntilt RW file. subset=',subset
                cycle msg_report
            end if
            if (lvldbg > 1) &
                write(6,'(1x,A6,I4,A30,A10,1x,A12,I10,1x,A10,I8)')'(mype:',mype,') READ_RADAR: subset:', &
                     subset,' data_date:',idate,' nmsg:',nmsg
   
!           Big loop over bufr file
   
            sb_report: do while (ireadsb(lnbufr) == 0)
                nmrecs = nmrecs+1
!               Read header.  Extract station infomration
                hdr(:)=zero
                call ufbint(lnbufr,hdr,8,1,levs,hdrstr2(1))
!               Read header.  Extract time infomration
                hdr2(:)=zero
                call ufbint(lnbufr,hdr2,6,1,levs,hdrstr2(2))
!               Read header.  Extract other infomration (not used by GSI currently)
                hdr3(:)=zero
                call ufbint(lnbufr,hdr3,4,1,levs,hdrstr2(3))
   
                rstation_id=hdr(1)      !station id   SSTN
                if(abs(hdr(2))>r90 .or. abs(hdr(3))>r360) cycle sb_report
                dlat_earth=hdr(2)       !station lon (degrees) CLAT
                dlon_earth=hdr(3)       !station lat (degrees) CLON
                if (dlon_earth==r360) dlon_earth=dlon_earth-r360
                if (dlon_earth<zero ) dlon_earth=dlon_earth+r360
                this_stalon=dlon_earth  ! station earth longitude (Deg)
                this_stalat=dlat_earth  ! station earth latitude  (Deg)
   
   
!               deg to rad (tll2xy needs rad, not deg)
                dlat_earth = dlat_earth * deg2rad
                dlon_earth = dlon_earth * deg2rad
   
!               check up radar_table
                irdr_ptr = -1
                if ( icnt_radar_read == 0 ) then                             ! first radar in table
                    icnt_radar_read = 1
                    radartable_id(icnt_radar_read) = this_staid
                    irdr_ptr=icnt_radar_read
                else
                    irdr_ptr=-1
                    loop_rdrtbl: do irdr=1,icnt_radar_read
                        if ( this_staid == radartable_id(irdr) ) then      ! found same radar in table
                            irdr_ptr=irdr
                            exit loop_rdrtbl
                        end if
                    end do loop_rdrtbl
                    if (irdr_ptr <= 0) then                        !  found new radar and add it into table
                        icnt_radar_read = icnt_radar_read + 1
                        radartable_id(icnt_radar_read) = this_staid
                        irdr_ptr=icnt_radar_read
                    end if
                end if
   
!               check up whether radar station is inside the domain
                radartable_ncolm(irdr_ptr) = radartable_ncolm(irdr_ptr) + 1
                if(regional)then
                    outside=.false.
                    call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
                    if (outside) then
                        if ( radartable_inside(irdr_ptr) > 0 ) then
                            write(6,*)'*** WARNING --> READ_RADAR: (columntilt) radar:',this_staid, &
                                      ' is found both inside and outside. Wrong! <-- WARNING ***'
                            call stop2(999)
                        end if
                        radartable_inside(irdr_ptr)    = -1
                        radartable_ncolm_out(irdr_ptr) = radartable_ncolm_out(irdr_ptr) + 1
                        ioutside_obs=ioutside_obs+1
                        cycle sb_report
                    else
                        if ( radartable_inside(irdr_ptr) < 0 ) then
                            write(6,*)'*** WARNING --> READ_RADAR: (columntilt) radar:',this_staid, &
                                      ' is found both inside and outside. Wrong! <-- WARNING ***'
                            call stop2(999)
                        end if
                        radartable_inside(irdr_ptr)    =  1
                    end if
                    dlatmax=max(dlat,dlatmax)
                    dlonmax=max(dlon,dlonmax)
                    dlatmin=min(dlat,dlatmin)
                    dlonmin=min(dlon,dlonmin)
                else
                    dlat = dlat_earth
                    dlon = dlon_earth
                    call grdcrd1(dlat,rlats,nlat,1)
                    call grdcrd1(dlon,rlons,nlon,1)
   
                    radartable_inside(irdr_ptr) =  1                     ! global: radar is always inside
   
                endif
   
                clon=cos(dlon_earth)
                slon=sin(dlon_earth)
                clat=cos(dlat_earth)
                slat=sin(dlat_earth)
   
                staheight=hdr(4)             ! station elevation  HSMSL 
   
!   ---------------------------------------------------------------------------------!
!               Skipping finding vad wind match for column-tilt radar wind data
!   ---------------------------------------------------------------------------------!
   
!               Get model terrain at radar station location: zsges
                call deter_zsfc_model(dlat,dlon,zsges)
   
                iyr = hdr2(1)
                imo = hdr2(2)
                idy = hdr2(3)
                ihr = hdr2(4)
                imn = hdr2(5)
                isc = hdr2(6)
   
                idate5(1) = iyr
                idate5(2) = imo
                idate5(3) = idy
                idate5(4) = ihr
                idate5(5) = imn
                ikx=0
                do i=1,nconvtype
                    if(trim(ioctype(i)) == trim(obstype))ikx = i
                end do
                if(ikx==0) cycle sb_report
                pmot=pmot_conv(ikx)
                if(reduce_diag .and. pmot < 2)pmot=pmot+2
                save_all = .false.
                if(pmot /= 2 .and. pmot /= 0)save_all=.true.
   
!               time window check     
                call w3fs21(idate5,minobs)
                call w3fs21(idate5,mincy)
                t4dv=real(minobs-iwinbgn,r_kind)*r60inv
                if (l4dvar.or.l4densvar) then
                    if (t4dv<zero .OR. t4dv>winlen) cycle sb_report
                else
                    timeb = real(minobs-mincy,r_kind)*r60inv
                    if (abs(timeb)>half .or. abs(timeb) > ctwind(ikx)) then
                        ioutside_tim=ioutside_tim+1
                        radartable_ncolm_outtimw(irdr_ptr) = radartable_ncolm_outtimw(irdr_ptr) + 1
                        cycle sb_report
                    endif
                endif
   
!             Get observation (lon,lat).  Compute distance from radar.
!                 adjusting XOB (longitude)  --> hdr(7)
!                           YOB (lattitude)  --> hdr(8)
                if(abs(hdr(8))>r90 .or. abs(hdr(7))>r360) cycle sb_report
                thislon=hdr(7)            ! obs longitude (Deg)
                thislat=hdr(8)            ! obs latitude  (Deg)
                if(thislon==r360) thislon=thislon-r360
                if(thislon<zero ) thislon=thislon+r360
                dlon_earth = thislon*deg2rad
                dlat_earth = thislat*deg2rad
!   
!             Get grid indices for lon/lat
!               check up whether this radar obs (on this column grid) is inside
                if(regional) then
                    outside=.false.
                    call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
                    if (outside) then
                        radartable_ncolm_out(irdr_ptr) = radartable_ncolm_out(irdr_ptr) + 1
                        ioutside_obs=ioutside_obs+1
                        cycle sb_report
                    else
                        radartable_ncolm_ins(irdr_ptr) = radartable_ncolm_ins(irdr_ptr) + 1
                    end if
                else
                    dlat = dlat_earth
                    dlon = dlon_earth
                    call grdcrd1(dlat,rlats,nlat,1)
                    call grdcrd1(dlon,rlons,nlon,1)
                    radartable_ncolm_ins(irdr_ptr) = radartable_ncolm_ins(irdr_ptr) + 1
                endif
   
                clonh=cos(dlon_earth)
                slonh=sin(dlon_earth)
                clath=cos(dlat_earth)
                slath=sin(dlat_earth)
                cdist=slat*slath+clat*clath*(slon*slonh+clon*clonh)
                cdist=max(-one,min(cdist,one))
!             dist -- surface great circle range in km
                dist=eradkm*acos(cdist)
   
!             Set observation "type" to be function of distance from radar
                kxadd=nint(dist*one_tenth)
                kx=kx0+kxadd
   
!             Extract azimuth for each column of radial wind data
   
   
!             0. convert azimuth from pointing to north defined as 0
!                                  to pointing to east as 0 (GSI rule)
!                                  this step needs to be done before next step of
!                                  correction.
!              For accuracy, keep away from poles, rather than properly deal with polar singularity
                if(abs(thislat)>r89_5) cycle sb_report
   
!              Checking the azimuth angle and counting the bad data
                if (hdr(6) < 0.0_r_kind .or. hdr(6) > 360.0_r_kind) then
                    iazmoutrange=iazmoutrange+1
                    write(6,'(1x,A100,F10.3,1x,I8)')'(mype:',mype, &
                        ') READ_RADAR: azimuth in BUFR is not in 0~360 (skip) -- azm_hdr(6)  cnt:', &
                        hdr(6),iazmoutrange
                    cycle sb_report
                end if
   
!               azm_east is only used for cdata(13) when diagnose and plotting
                azm_east = r90 - hdr(6)  ! original azimuth (degree, but east as 0)
                if ( azm_east < zero ) azm_east = azm_east + r360
                if ( abs(azm_east - r360) <= 1.0E-3_r_kind ) azm_east = zero
                if ( azm_east > r360 ) azm_east = azm_east - r360
   
!             Adjustment of Azimuth.
!             Including:
!               1) convert azimuth from north as zero (radar-based) to east as zero (model-based)
!               2) correcting azimuth due to the differences between the earth-lat/lon
!                  of radar station and radar observation
!               3) rotation of azimuth from earth-lat/lon model grid to WRF Curvilinear grid
   
                if ( l_azm_east1st ) then
   
!                 1. First, convert azimuth from pointing to north defined as 0 to pointing to east as 0 (GSI rule)
!                  (1) hdr(6)  -- radar azimuth, north as 0 (degree)
!                  (2) thisazm -- model wind direction azimuth with east as 0 (degree)
                    thisazm     = r90-hdr(6)  ! wind direction azimuth (deg)
!                  (3) change the domain of thisazm back to [0,360].
                    if ( thisazm < zero ) thisazm = thisazm + r360
                    if ( abs(thisazm - r360) <= 1.0E-3_r_kind ) thisazm = zero
                    if ( thisazm > r360 ) thisazm = thisazm - r360
   
!                 2. then azimuth is corrected due to lat/lon differences between radar stn and radar obs.
!                    see subroutine radar_bufr_read_all (in read_l2bufr_mod.f90) for "corrected_azimuth".
!                    azm_earth is the corrected azimuth (degree)
                    if ( l_correct_azmu ) then
                        call get_azimuth_corrected(this_stalon,this_stalat,    &
                            thislon,    thislat,        &
                            thisazm,    azm_earth)
!                    Note: the corrected_azimuth is in [-pi,pi] (due to domain of Function ATAN2).
!                          Convert it back to [0,2*pi], i.e. [0, 360]
                        if ( azm_earth < zero) azm_earth = azm_earth + r360
                        if ( abs(azm_earth - r360) <= 1.0E-3_r_kind ) azm_earth = zero
                        if ( azm_earth > r360 ) azm_earth = azm_earth - r360
                    else
                        azm_earth = thisazm        ! if do not correct it.
                    end if
   
                else
   
!                 1. First, azimuth is corrected due to lat/lon differences between radar stn and radar obs.
                    thisazm = hdr(6)                    ! ANAZ  (degree and zero to north)
!                    see subroutine radar_bufr_read_all (in read_l2bufr_mod.f90) for "corrected_azimuth".
!                    azm_earth is the corrected azimuth (degree)
                    if ( l_correct_azmu ) then
                        call get_azimuth_corrected(this_stalon,this_stalat,    &
                            thislon,    thislat,        &
                            thisazm,    azm_earth)
!                    Note: the corrected_azimuth is in [-pi,pi] (due to domain of Function ATAN2).
!                          Convert it back to [0,2*pi], i.e. [0, 360]
                        if ( azm_earth < zero) azm_earth = azm_earth + r360
                        if ( abs(azm_earth - r360) <= 1.0E-3_r_kind ) azm_earth = zero
                        if ( azm_earth > r360 ) azm_earth = azm_earth - r360
                    else
                        azm_earth = thisazm        ! if do not correct it.
                    end if
   
!                 2. Then convert azimuth from pointing to north defined as 0 to pointing to east as 0 (GSI rule)
                    azm_earth   = r90-azm_earth           ! east as zero, (degree)
!                  (1) change the domain of azm_earth back to [0,360].
                    if ( azm_earth < zero ) azm_earth = azm_earth + r360
                    if ( abs(azm_earth - r360) <= 1.0E-3_r_kind ) azm_earth = zero
                    if ( azm_earth > r360 ) azm_earth = azm_earth - r360
   
                end if
   
!             3. rotation adjustment due to lat/lon earth coordinate rotating to X/Y
!                rotated coordinate. So rotaing azimuth_eaarth to azimuth_xy
!                Note:
!                     (1) for this calculation, azimuth must be changed to the angle
!                     with 0 to the east, not to the north.  (r90-azm_earth)
!                     (2) after rotation, azm (the rotated azimuth) is in unit of
!                     degree. So when stored in cdata(6), it is converted to
!                     radians.
                if(regional .and. .not. fv3_regional) then
                    cosazm_earth=cos(azm_earth*deg2rad)
                    sinazm_earth=sin(azm_earth*deg2rad)
                    call rotate_wind_ll2xy(cosazm_earth,sinazm_earth,cosazm,sinazm,dlon_earth,dlon,dlat)
                    azm=atan2(sinazm,cosazm)*rad2deg
!                 Note: ATAN2 is used to find the rotated_azimuth, the domain of
!                       ATAN2 is [-pi,pi], or say [-180, 180]. For consistency,
!                       convert it back to [0,2*pi],i.e., [0, 360].
                    if (azm < zero) azm = azm + r360
                    if ( abs(azm - r360) <= 1.0E-3_r_kind ) azm = zero
                    if ( azm > r360 ) azm = azm - r360
                else
                    azm=azm_earth
                end if
   
!             Go through the data levels
                call ufbint(lnbufr,radar_obs_columntilt,5,maxlevs,levs,obstr)
                if(levs>maxlevs) then
                    write(6,*)'READ_RADAR:  ***ERROR*** increase read_radar bufr size since ',&
                        'number of levs=',levs,' > maxlevs=',maxlevs
                    call stop2(84)
                endif
   
                numcut=0      ! l3 data toosed by l2.5 data
   
                loop_lvl : do k=1,levs
                    ncolumntiltl2_in=ncolumntiltl2_in+1
                    nread=nread+1
                    ntb=ntb+1
                    t4dvo=real(minobs-iwinbgn,r_kind)*r60inv
                    timemax=max(timemax,t4dvo)
                    timemin=min(timemin,t4dvo)
   
                    kxadd=nint(dist*one_tenth)
                    kx=kx0+kxadd
   
!                 Extract radial wind data
                    thistilt = radar_obs_columntilt(1,k)              ! radar scan/tilt angle (degree)
                    height   = radar_obs_columntilt(2,k)              ! obs beam height (meter)
                    range1   = radar_obs_columntilt(3,k)              ! obs beam range (meter)
                    rwnd     = radar_obs_columntilt(4,k)              ! radial wind (m/s)
                    rdbz     = radar_obs_columntilt(5,k)              ! reflectivity (dBZ)
   
                    sfcrng0  = dist*r1000                       ! surface range in unit of meters
!                 check on the values from BUFR data
!                 Should check dbz and rwnd also (first?)
                    if (thistilt < 0.0_r_kind .OR. thistilt > 90.0_r_kind) then
                        itiltoutrange=itiltoutrange+1
                        if ( rdbz >= 0.0_r_kind .AND. rdbz < 100.0_r_kind ) then
                            write(9997,'(1x,A80,F10.3,1x,I8,2F20.3)') &
                                'READ_RADAR: tilt in BUFR is not in 0~90 (skip) -- tilt  cnt:', &
                                thistilt,itiltoutrange,rdbz,rwnd
                        end if
!                        cycle loop_lvl
                    end if
                    if (height < -1.0_r_kind .OR. height > 100000.0_r_kind ) then
                        iohgtoutrange=iohgtoutrange+1
                        if ( rdbz >= 0.0_r_kind .AND. rdbz < 100.0_r_kind ) then
                            write(9997,'(1x,A80,F20.3,1x,I8,2F20.3)') &
                                'READ_RADAR: obs hgt in BUFR is not in 0~100km (skip) -- obshgt  cnt:', &
                                height,iohgtoutrange,rdbz,rwnd
                        end if
!                        cycle loop_lvl
                    end if
                    if (range1 <  0.0_r_kind .OR. range1 > 510000.0_r_kind ) then
                        iorngoutrange=iorngoutrange+1
                        if ( rdbz >= 0.0_r_kind .AND. rdbz < 100.0_r_kind ) then
                            write(9997,'(1x,A80,F20.3,1x,I8,2F20.3)') &
                                'READ_RADAR: obs range in BUFR is not in 0~510km (skip) -- obsrange  cnt:', &
                                range1,iorngoutrange,rdbz,rwnd
                        end if
!                        cycle loop_lvl
                    end if
   
!                 Get corrected tilt(elevation) angle
                    if ( l_correct_tilt ) then
                        if ( i_correct_tilt == 1 ) then
!                 Considering the curvature effect of beam and earth surface
!                 Note: two schemes
!                   1. algorithm used in radar_bufr_read_all (read_l2bufr_mod.f90) for superobs
   
                            call get_rdr_obshgttilt(thistilt,range1,staheight, &
                                thishgt,corrected_tilt)
                            write(9991,'(1x,A4,A4,A9,F7.3,A7,F12.1,A9,F7.1,A12,F7.1,A12,F7.1,A11,F7.3,A8,F12.1)') &
                                'ID:',this_staid,' tilt(o):',thistilt,' range:',range1,    &
                                ' sta_hgt:',staheight,' obs_hgt(c):',thishgt,' obs_hgt(o):',height, &
                                ' tilt(crc):',corrected_tilt,' sfcrng:',sfcrng0
                        else
   
!                   2. scheme used in arps pacakge (arpsenkf: dhdr and dsdr)
!                      note: 
!                           using beamelv and dhdrange in adas/radarlib3d.f90. Need
!                           obs height (beam height) and surface range (from radar
!                           to obs location on surface). Based on 88d2arps data,
!                           when rdbz/rwnd are missing values, the obs height is
!                           also missing values (-999.0), then it is not necessary
!                           to correct the tilt, since this obs will be rejected.
   
                            call beamelv(height,sfcrng0,elvang0,range0)
                            call dhdrange(elvang0,range0,dhdr)
                            dsdr=sqrt(1._r_kind-dhdr*dhdr)
                            corrected_tilt=atan2(dhdr,dsdr)*rad2deg
                        end if
!                     Warning Note: tiltangle which is stored in cdata(9), must be in unit of radians.
                        tiltangle=corrected_tilt
                    else
                        tiltangle=thistilt
                    end if
                    if (tiltangle < 0.0_r_kind .OR. tiltangle > 90.0_r_kind) then
                        itiltoutrange2=itiltoutrange2+1
                        write(9997,'(1x,A100,2F10.3,1x,I8,4F20.3)') &
                            'READ_RADAR: crc-tilt is not in 0~90deg. -- tilt (org crc) cnt dbz rw height range:', &
                            thistilt, tiltangle, itiltoutrange2, rdbz, rwnd, height, range1
!                        cycle lvl_loop
                    end if
   
!                 obs error inflation with erradar_inflate (not really used for L2RWBUFR data)
                    error = erradar_inflate*oe_rw
   
!                 Increase error for lev2.5 and lev3
!                  (for specific domain: but commented off and not used for L2RWBUFR data)
                    errmax=max(error,errmax)
                    if(oe_rw >        zero) errmin=min(error,errmin)
   
!                 Perform limited qc based on azimuth angle, radial wind
!                 speed, distance from radar site, elevation of radar,
!                 height of observation, observation error.
   
                    good0=.true.
                    if(abs(azm)>r400) then
                        ibadazm=ibadazm+1; good0=.false.
                    end if
                    if(abs(rwnd)>r200) then
                        ibadwnd=ibadwnd+1; good0=.false.
                        if (rdbz >= 5.0_r_kind .and. rdbz < 120._r_kind) then
                            ibadwnd_cld=ibadwnd_cld + 1
                            write(9998,'(1x,A60,2F20.2,A8,2F10.3,2F10.2,I10)') &
                                'bad wind with cloud --- rw dbz rdr_id azm scntlt xob yob cnt:',          &
                                rwnd,rdbz,this_staid,azm,thistilt,dlon,dlat,ibadwnd_cld
                        end if
                    else
                        igoodwnd=igoodwnd+1
                        if (rdbz<-20.0_r_kind ) then
                            igoodwnd_dbz(1) = igoodwnd_dbz(1)+1
                        else if (rdbz>=-20.0_r_kind .AND. rdbz < -0.5_r_kind ) then
                            igoodwnd_dbz(2) = igoodwnd_dbz(2)+1
                        else if (rdbz>=-0.5_r_kind .AND. rdbz <  0.5_r_kind ) then
                            igoodwnd_dbz(3) = igoodwnd_dbz(3)+1
                        else if (rdbz>= 0.5_r_kind .AND. rdbz <  5.0_r_kind ) then
                            igoodwnd_dbz(4) = igoodwnd_dbz(4)+1
                        else if (rdbz>= 5.0_r_kind .AND. rdbz <  100.0_r_kind ) then
                            igoodwnd_dbz(5) = igoodwnd_dbz(5)+1
                        else
                            igoodwnd_dbz(6) = igoodwnd_dbz(6)+1
                        end if
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
   
                    if(oe_rw>r6 .or. oe_rw<=zero) then
                        ibaderror=ibaderror+1; good0=.false.
                    end if
   
                    if (rdbz < -20.0_r_kind ) then
                        idbz_bad=idbz_bad+1; good0=.false.
                    else if (rdbz >= -20.0_r_kind .AND. rdbz < refl_lowbnd_rw) then
                        idbz_clr=idbz_clr+1; good0=.false.
                    else if (rdbz >= refl_lowbnd_rw .and. rdbz < 100.0_r_kind) then
                        idbz_cld=idbz_cld+1
                        if (abs(rwnd)>r200) then
                            icld_badwnd=icld_badwnd+1
                            good0=.false.
                        else
                            icld_goodwnd=icld_goodwnd+1
                        end if
                    else
                        idbz_bad2=idbz_bad2+1; good0=.false.
                    end if
   
!                 good and notgood are used for further QC check with VAD match.
!                 note:   No further QC check with VAD match for L2RWBUFR data
!                         So good is actually same as good0
                    good=.true.
                    if(.not.good0) then
                        notgood0=notgood0+1
                        good=.false.
                        cycle loop_lvl
                    end if
   
!                 If data is good, load into output array
                    if(good) then
   
!                     no vad and obs box counting for L2RWBUFR
!                     (they are used for NCEP L2/2.5/3 radar obs, see code for those data)
   
                        radartable_ndata_use(irdr_ptr) = radartable_ndata_use(irdr_ptr) + 1
                        ncolumntiltl2_kept=ncolumntiltl2_kept+1
                        if ( ndata+1 > maxobs ) then
                            do irpt=1,5
                                write(6,'(1x,A128)')' *** *** WARNING --> READ_RADAR: ndata is more than maxobs(=2,000,000) &
                                                     --> increase maxobs!!! <-- WARNING *** ***'
                            end do
                            write(6,'(1x,A128)')' *** *** WARNING --> INCREASE maxobs in READ_RADAR beyond 2,000,000, &
                                                  re-compile GSI, re-run !!! <-- WARNING*** ***'
                        end if
                        ndata  = min(ndata+1,maxobs)
                        nodata = min(nodata+1,maxobs)  
                        usage  = zero
                        if(icuse(ikx) < 0)usage=r100
                        if(usage >= 100._r_kind) rusage(ndata)=.false.
   
                        call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,skint,ff10,sfcr)
   
                        cdata(1) = error             ! wind obs error (m/s)
                        cdata(2) = dlon              ! grid relative longitude
                        cdata(3) = dlat              ! grid relative latitude
                        cdata(4) = height            ! obs absolute height (m)
                        cdata(5) = rwnd              ! wind obs (m/s)
                        cdata(6) = azm*deg2rad       ! azimuth angle (radians)
                        cdata(7) = t4dvo             ! obs time (hour)
                        cdata(8) = ikx               ! type               
                        cdata(9) = tiltangle*deg2rad ! tilt angle (radians)
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
                        cdata(21)=thiserr_rw         ! original error (same as user-defined in namelist)
                        cdata(22)=two                ! sub-type, rank of radar date (level 2/2.5/3. or higher for airborne)
!                       put original azimuth, tilt and dbz into cdata for plotting diag file.
                        if (l_plt_diag_rw) then
                            cdata(13)= azm_east      ! original azimuth (degree, but east as 0)
                            cdata(14)= thistilt      ! orignal scan tilt (degree)
                            cdata(16)= rdbz          ! reflectivity at same point with rwnd
                        end if
   
                        do i=1,maxdat
                            cdata_all(i,ndata)=cdata(i)
                        end do
   
                    else
                        notgood = notgood + 1
                    end if
   
!                 End of k loop over levs
                end do loop_lvl
   
!             End of bufr read loop
            end do sb_report
   
        end do msg_report
   
!       Close unit to bufr file
        call closbf(lnbufr)
   
        write(6,*)'READ_RADAR:  ',trim(outmessage),' reached eof on level 2 radar file (grid-tilt; column-tilt).'
   
        if(loop==-1)    write(6,*)'READ_RADAR: ncolumntiltl2_in,ncolumntiltl2_kept=',ncolumntiltl2_in,ncolumntiltl2_kept
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
        write(6,*)'READ_RADAR: # hgt belowsta=',iheightbelowsta
        write(6,*)'READ_RADAR: # bad dbz(<-20.0) =',idbz_bad
        write(6,*)'READ_RADAR: # bad dbz(>100.0) =',idbz_bad2
        write(6,*)'READ_RADAR: # clearsky dbz (-20.0~',refl_lowbnd_rw,') =',idbz_clr
        write(6,*)'READ_RADAR: # cloud dbz (',refl_lowbnd_rw,'~100.0) =',idbz_cld
        write(6,*)'READ_RADAR: # bad winds (cloud) =',ibadwnd_cld
        write(6,*)'READ_RADAR: # cloud (bad winds) =',icld_badwnd
        write(6,*)'READ_RADAR: # cloud (goodwinds) =',icld_goodwnd
        write(6,*)'READ_RADAR: # azim out of range =',iazmoutrange
        write(6,*)'READ_RADAR: # tilt(org) out of range =',itiltoutrange
        write(6,*)'READ_RADAR: # tilt(crc) out of range =',itiltoutrange2
        write(6,*)'READ_RADAR: # obshgt out  range =',iohgtoutrange
        write(6,*)'READ_RADAR: # obsrng out  range =',iorngoutrange
        write(6,*)'READ_RADAR: # notgood0    =',notgood0
        write(6,*)'READ_RADAR: # notgood(VAD)=',notgood
        write(6,*)'READ_RADAR: timemin,max   =',timemin,timemax
        write(6,*)'READ_RADAR: errmin,max    =',errmin,errmax
        write(6,*)'READ_RADAR: dlatmin,max,dlonmin,max=',dlatmin,dlatmax,dlonmin,dlonmax
        write(6,*)'READ_RADAR: nmsg,nmrecs,ntb=',nmsg,nmrecs,ntb
        write(6,*)'READ_RADAR: # good winds (abs(rwnd) <= 200m/s) =',igoodwnd
        write(6,*)'READ_RADAR: # dbz distribution in good winds #'
        write(6,'(1x,A60,F12.5,1x,F12.5)')'READ_RADAR:radar wind--> obs error(adjust) thiserr_rw(original)= ',error, thiserr_rw
        write(6,*)'----------------------------------------------------------------'
        write(6,'(1x,6A15)')'<-20.0dbz','-20.0 ~ -0.5','-0.5 ~ 0.5','0.5 ~ 5.0','5.0 ~ 100.0','>100.0dbz'
        write(6,'(1x,6I15)')igoodwnd_dbz(1:6)
        write(6,*)'----------------------------------------------------------------'
   
!       sum up radar table information
        ntot_col   = 0; ntot_col_o = 0; ntot_col_i = 0; ntot_dat_u = 0;
        ntot_col_ot= 0; icnt_radar_inside=0; icnt_radar_outside=0;
        open(2018,file='radarlist_rw_used.txt',form='formatted')
        write(2018,'(1x,7(1x,A15))')'       RADAR_ID', '   radar_inside', ' total_column',       &
                                    '  column_inside', ' column_outside', ' OutTWindow',       &
                                    '      ndata_use' 
        write(6,*)'----------------------------------------------------------------'
        write(6,   '(1x,7(1x,A15))')'       RADAR_ID', '   radar_inside', ' total_column',       &
                                    '  column_inside', ' column_outside', 'colm_OutTWindow',       &
                                    '      ndata_use'
        do icnt=1, icnt_radar_read
            if ( radartable_inside(icnt) > 0 ) then
                icnt_radar_inside  = icnt_radar_inside  + 1
            else
                icnt_radar_outside = icnt_radar_outside + 1
            end if
            ntot_col   = ntot_col   + radartable_ncolm(icnt)
            ntot_col_o = ntot_col_o + radartable_ncolm_out(icnt)
            ntot_col_i = ntot_col_i + radartable_ncolm_ins(icnt)
            ntot_dat_u = ntot_dat_u + radartable_ndata_use(icnt)
            ntot_col_ot= ntot_col_ot+ radartable_ncolm_outtimw(icnt)
            write(2018,'(1x,1x,A15,1x,I15,5(1x,I15))')radartable_id(icnt),radartable_inside(icnt), &
                 radartable_ncolm(icnt),radartable_ncolm_ins(icnt),radartable_ncolm_out(icnt), &
                 radartable_ncolm_outtimw(icnt),radartable_ndata_use(icnt)
            write(6,'(1x,1x,A15,1x,I15,5(1x,I15))')radartable_id(icnt),radartable_inside(icnt), &
                 radartable_ncolm(icnt),radartable_ncolm_ins(icnt),radartable_ncolm_out(icnt), &
                 radartable_ncolm_outtimw(icnt),radartable_ndata_use(icnt)
        end do
        write(2018,*)'----------------------------------------------------------------'
        write(6,*)'----------------------------------------------------------------'
        write(2018,'(1x,1x,A15,1x,I6,3x,I6,5(1x,I15))')'     Total ',icnt_radar_inside,         &
             icnt_radar_outside,ntot_col,ntot_col_i,ntot_col_o,ntot_col_ot,ntot_dat_u
        write(6,   '(1x,1x,A15,1x,I6,3x,I6,5(1x,I15))')'     Total ',icnt_radar_inside,         &
             icnt_radar_outside,ntot_col,ntot_col_i,ntot_col_o,ntot_col_ot,ntot_dat_u
        write(2018,*)'----------------------------------------------------------------'
        close(2018)
        write(6,*)'----------------------------------------------------------------'
   
! end of read columntilt radar level 2 data
!====================================================================================!
     end if

!    Write out vad statistics
     do ivad=1,nvad
        if(print_verbose)write(6,'(" fit of 2, 2.5, 3 data to vad station, lat, lon = ",a8,2f14.2)') &
           vadid(ivad),vadlat(ivad)*rad2deg,vadlon(ivad)*rad2deg
        do ivadz=1,maxvadbins
           if(vadcount2(ivad,ivadz) > half .and. vadcount2_5(ivad,ivadz) > half &
                 .and. vadcount(ivad,ivadz) > half)then
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
             if(print_verbose)write(6,'(" h,f2,f2.5,f3=",i7,f10.2,"/",i5,f10.2,"/",i5,f10.2,"/",i5)')nint(ivadz*dzvad),&
              vadfit2(ivad,ivadz),nint(vadcount2(ivad,ivadz)),&
              vadfit2_5(ivad,ivadz),nint(vadcount2_5(ivad,ivadz)),&
              vadfit3(ivad,ivadz),nint(vadcount3(ivad,ivadz))
           end if
        end do
     end do

     deallocate(nobs_box)

  end if

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
  ntdrvr_in=0
  ntdrvr_kept=0
  ntdrvr_thin1=0
  ntdrvr_thin2=0
  ntdrvr_thin2_foreswp=0
  ntdrvr_thin2_aftswp=0
  nmissing=0
  subset_check(3)='NC006070'
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

  if(trim(infile) == 'tldplrso') then

!    Loop to read TDR superobs data

     ikx=0
     do i=1,nconvtype
        if(trim(ioctype(i)) == trim(obstype))ikx = i
     end do
     if(ikx == 0) return
     pmot=pmot_conv(ikx)
     if(reduce_diag .and. pmot < 2)pmot=pmot+2
     save_all = .false.
     if(pmot /= 2 .and. pmot /= 0)save_all=.true.

     call w3fs21(iadate,mincy) ! analysis time in minutes

     open(lnbufr,file=trim(infile),form='formatted',err=300)
     rewind (lnbufr)
     do n=1,10
        istop=0
        read(lnbufr,'(a)',err=200,end=1200)filename
        print *,'filename=', trim(filename)
        open(25,file=trim(filename),form='formatted',access='sequential')
        loop3: do while (istop.eq.0)
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
              if (t4dv<zero .OR. t4dv>winlen) cycle loop3
              timeo=t4dv
           else
              timeo = real(minobs-mincy,r_kind)*r60inv
              if (abs(timeo)>twind) cycle loop3
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
              ibadtilt=ibadtilt+1; cycle loop3
           endif
   
           staheight=this_stahgt
           if(staheight<-r1000.or.staheight>r50000) then
              ibadstaheight=ibadstaheight+1; cycle loop3
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

!        Get observation (lon,lat).  Compute distance from radar.
           dlat_earth=elat8(ii)
           dlon_earth=elon8(ii)
           if(dlon_earth>=r360) dlon_earth=dlon_earth-r360
           if(dlon_earth<zero ) dlon_earth=dlon_earth+r360
           dlat_earth_deg = dlat_earth
           dlon_earth_deg = dlon_earth
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

!        Extract radial wind data
           height= z(ii)
           rwnd  = thisvr
           azm_earth = glob_azimuth8(ii)
           if(regional .and. .not. fv3_regional) then
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

!        Perform limited qc based on azimuth angle, elevation angle, radial wind
!        speed, range, distance from radar site

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
!        if data is good, load into output array

           if(good) then
              ntdrvr_kept=ntdrvr_kept+1
!####################       Data thinning       ###################

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

                 if (thin4d) then
                    timedif = zero
                 else
                    timedif=abs(t4dv-toff)
                 endif
                 crit1 = timedif/r6+half

                 call map3grids_m(1,save_all,zflag,zl_thin,nlevz, &
                    dlat_earth,dlon_earth,zobs,crit1,ndata,&
                    luse,maxobs,rthin,.false.,.false.)

                 if (.not. luse) then
                    ntdrvr_thin2=ntdrvr_thin2+1
                    cycle
                 endif

              else
                 ndata =ndata+1
              endif
              iout=ndata

              if(ndata > maxobs) then
                 write(6,*)'READ_PREPBUFR:  ***WARNING*** ndata > maxobs for ',obstype
                 ndata = maxobs
              end if

!          Set usage variable
              usage = zero

              if(icuse(ikx) < 0)usage=r100
              if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
                 if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
              end if
              if(usage >= 100._r_kind) rusage(ndata)=.false.
   
              call deter_zsfc_model(dlat,dlon,zsges)
 
!    Get information from surface file necessary for conventional data here
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
              cdata(17)=dlon_earth_deg     ! earth relative longitude (degrees)
              cdata(18)=dlat_earth_deg     ! earth relative latitude (degrees)
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
        
        end do loop3! end of loop, reading records of data
        close(25)

     end do ! end of loop, reading TDR so data files
     close(lnbufr)

  else if (trim(infile) == 'tldplrbufr' ) then

     nswptype=0
     nmrecs=0
     irec=0

!    Open data file
     open(lnbufr,file=trim(infile),form='unformatted')
     call openbf(lnbufr,'IN',lnbufr)
     call datelen(10)
     call readmg(lnbufr,subset,idate,iret)
     if(iret==0) then

!       Time offset
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

        if(l_foreaft_thin)then

!    Read the first 500 records to deterine which criterion
!    should be used to seperate fore/aft sweep

!         Big loop over bufr file

          loop5: do 
             call readsb(lnbufr,iret)
             if(iret/=0) then
                call readmg(lnbufr,subset,idate,iret)
                if(iret/=0) exit loop5
                cycle loop5
             end if
             if(subset/=subset_check(loop)) then
                call readmg(lnbufr,subset,idate,iret)
                if(iret/=0) exit loop5
                cycle loop5
             end if
             nmrecs = nmrecs+1

!            Read header.  Extract elevation angle
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
                exit loop5
             end if

             if(nmrecs <= 500)then
                cycle loop5
             else
                exit loop5
             end if

          end do loop5

          firstbeam = 0
          foreswp = .true.
          aftswp = .false.
          nforeswp=1
          naftswp=0
          nswp=1

          call closbf(lnbufr)
          close(lnbufr)

          open(lnbufr,file=trim(infile),form='unformatted')
          call openbf(lnbufr,'IN',lnbufr)
          call datelen(10)
          call readmg(lnbufr,subset,idate,iret)

        else
           foreswp = .false.
           aftswp = .false.
        end if

        print *,'nmrecs, nswptype=', nmrecs, nswptype

        nmrecs=0

!          Big loop over bufr file

        loop4: do 
           call readsb(lnbufr,iret)
           if(iret/=0) then
              call readmg(lnbufr,subset,idate,iret)
              if(iret/=0) exit loop4
              cycle loop4
           end if
           if(subset/=subset_check(loop)) then
              call readmg(lnbufr,subset,idate,iret)
              if(iret/=0) exit loop4
              cycle loop4
           end if
           nmrecs = nmrecs+1
           irec = irec+1

!             Read header.  Extract station infomration
           call ufbint(lnbufr,hdr,12,1,levs,hdrstr(2))

!          rstation_id=hdr(1)
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
           if(ikx == 0) cycle loop4
           pmot=pmot_conv(ikx)
           if(reduce_diag .and. pmot < 2)pmot=pmot+2
           save_all=.false.
           if(pmot /= 2 .and. pmot /= 0) save_all=.true.
           call w3fs21(idate5,minobs)

           t4dv=real(minobs-iwinbgn,r_kind)*r60inv
           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .OR. t4dv>winlen) then
                 ntimeout=ntimeout+1
                 cycle loop4
              end if
              timeo=t4dv
           else
              timeo = real(minobs-mincy,r_kind)*r60inv
              if (abs(timeo) > twind .or. abs(timeo) > ctwind(ikx)) then
                 ntimeout=ntimeout+1
                 cycle loop4
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

!           define fore/aft sweeps for thinning (pseduo dual Doppler)

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
              ibadtilt=ibadtilt+1; cycle loop4
           endif

           staheight=this_stahgt
           if(staheight<-r1000.or.staheight>r50000) then
              ibadstaheight=ibadstaheight+1; cycle loop4
           end if

!             Go through the data levels
           call ufbint(lnbufr,tdr_obs,4,maxlevs,levs,datstr(2))
           if(levs>maxlevs) then
              write(6,*)'READ_RADAR:  ***ERROR*** increase read_radar bufr size since ',&
                 'number of levs=',levs,' > maxlevs=',maxlevs
              call stop2(84)
           endif
!          use local coordinate centered on this_stalat,this_stalon. note that global and local
!          azimuth angle are the same at the origin (this_stalat,this_stalon)
!          and azimuth angle is fixed in local coordinate along entire radial line.
!          we convert back to global azimuth angle at each point along line
!          at end of computation.  that way we avoid worrying about where poles are.

           aactual=erad+this_stahgt
           thistiltr=thistilt*deg2rad
           selev0=sin(thistiltr) ; celev0=cos(thistiltr)
           a43=four_thirds*aactual
           ii=0
           do k=1,levs
              nread=nread+1
!             Select data every 3 km along each beam
              if(MOD(INT(tdr_obs(1,k)-tdr_obs(1,1)),3000) < 100)then
                 if(tdr_obs(3,k) >= 800.) then
                    nmissing=nmissing+1     !xx
                 else
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

!          Further process tail Doppler radar Vr data
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

!              Get observation (lon,lat).  Compute distance from radar.
                 dlat_earth=elat8(i)
                 dlon_earth=elon8(i)
                 if(dlon_earth>=r360) dlon_earth=dlon_earth-r360
                 if(dlon_earth<zero ) dlon_earth=dlon_earth+r360
                 dlat_earth_deg = dlat_earth
                 dlon_earth_deg = dlon_earth
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

!              Extract radial wind data
                 height= z(i)
                 rwnd  = dopbin(i)
                 azm_earth = glob_azimuth8(i)
                 if(regional .and. .not. fv3_regional) then
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

!              Perform limited qc based on azimuth angle, elevation angle, radial wind
!              speed, range, distance from radar site

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
                    good=.false.
                    cycle
                 end if
!           if data is good, load into output array

                 if(good) then
                    ntdrvr_kept=ntdrvr_kept+1
!####################       Data thinning       ###################

                    if(ndata>maxobs) exit

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

                       if (thin4d) then
                          timedif = zero
                       else
                          timedif=abs(t4dv-toff)
                       endif
                       crit1 = timedif/r6+half

                       call map3grids_m(1,save_all,zflag,zl_thin,nlevz, &
                          dlat_earth,dlon_earth,zobs,crit1,ndata,&
                          luse,maxobs,rthin,foreswp,aftswp)
   
                       if (.not. luse) then
                          if (foreswp) then
                             ntdrvr_thin2_foreswp=ntdrvr_thin2_foreswp+1  
                          else if (aftswp) then
                             ntdrvr_thin2_aftswp=ntdrvr_thin2_aftswp+1
                          end if
                          ntdrvr_thin2=ntdrvr_thin2+1
                          cycle
                       endif
   
                    else
                       ndata =ndata+1
                    endif
                    iout=ndata

                    if(ndata > maxobs) then
                       write(6,*)'READ_PREPBUFR:  ***WARNING*** ndata > maxobs for ',obstype
                       ndata = maxobs
                    end if

!                Set usage variable
                    usage = zero

                    if(icuse(ikx) < 0)usage=r100
                    if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
                       if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
                    end if
                    if(usage >= 100._r_kind) rusage(ndata)=.false.
                    call deter_zsfc_model(dlat,dlon,zsges)

!       Get information from surface file necessary for conventional data here
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
                    cdata(17)= dlon_earth_deg    ! earth relative longitude (degrees)
                    cdata(18)= dlat_earth_deg    ! earth relative latitude (degrees)
                    cdata(19)= dist              ! range from radar in km (used to estimate beam spread)
                    cdata(20)= zsges             ! model elevation at radar site
                    cdata(21)= thiserr
                    cdata(22)= hdr(1)+three+one  ! tail Doppler radar
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

!   End of bufr read loop
        end do loop4

! Normal exit
     else
        write(6,*)'READ_RADAR: problem reading tail Doppler radar bufr file tldplrbufr'
     end if
     call closbf(lnbufr)


  end if

1200 continue
  close(lnbufr)

  if (.not. use_all) then
     deallocate(zl_thin) 
     call del3grids
  endif


  nxdata=ndata
  ndata=0
  if(nxdata > 0)then
!    numthin=0
!    numqc=0
!    numrem=0
!    do i=1,nxdata
!      if(.not. rusage(i))then
!         numqc=numqc+1
!      else if(rthin(i))then
!         numthin=numthin+1
!      else
!         numrem=numrem+1
!      end if
!    end do
!    write(6,*) ' radar1 ',trim(ioctype(ikx)),ikx,numall,&
!           numrem,numqc,numthin,pmot

!     If flag to not save thinned data is set - compress data
     do i=1,nxdata
       

!      pmot=0 - all obs - thin obs
!      pmot=1 - all obs
!      pmot=2 - use obs
!      pmot=3 - use obs + thin obs
       if((pmot == 0 .and. .not. rthin(i)) .or. &
          (pmot == 1) .or. &
          (pmot == 2 .and. (rusage(i) .and. .not. rthin(i)))  .or. &
          (pmot == 3 .and. rusage(i))) then

          if(rthin(i))cdata_all(12,i)=101._r_kind
          ndata=ndata+1
          do k=1,maxdat
             cdata_all(k,ndata)=cdata_all(k,i)
          end do
       end if
    end do
  end if
  nodata=nodata+ndata
  deallocate(rusage,rthin)

  write(6,*)'READ_RADAR: # records saved in radar1 = ', ndata
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
  use read_l2bufr_mod, only: invtllv

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

subroutine read_radar_l2rw_novadqc(ndata,nodata,lunout,obstype,sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_radar_l2rw_novadqc  read radar L2 radial winds no VAD QC
!   prgmmr: yang             org: np23                date: 1998-05-15
!
! abstract:  This routine reads radar radial wind files.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!
! program history log:
!   2015-10-19  lippi   - Modified from read_radar to only process level 2 radial
!                         wind obs. and skip vad wind checks.
!
!   input argument list:
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!
!   output argument list:
!     ndata    - number of doppler lidar wind profiles retained for further
!     processing
!     nodata   - number of doppler lidar wind observations retained for further
!     processing
!     sis      - satellite/instrument/sensor indicator
!     nobs     - array of observations on each subdomain for each processor!


  use kinds, only: r_kind,r_single,r_double,i_kind,i_byte
  use constants, only: zero,half,one,two,deg2rad,rearth,rad2deg,r1000,r100,r400
  use qcmod, only: erradar_inflate
  use oneobmod, only: oneobtest,learthrel_rw
  use gsi_4dvar, only: l4dvar,l4densvar,winlen,time_4dvar
  use gridmod, only: regional,nlat,nlon,tll2xy,rlats,rlons,rotate_wind_ll2xy,&
           fv3_regional
  use convinfo, only: nconvtype,ncmiter,ncgroup,ncnumgrp,icuse,ioctype,pmot_conv
  use deter_sfc_mod, only: deter_sfc2,deter_zsfc_model
  use mpimod, only: npe
  use obsmod, only: reduce_diag,time_offset 

  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: obstype!,infile
  character(len=20),intent(in  ) :: sis
!  real(r_kind)    ,intent(in   ) :: twind
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: ndata,nodata!,nread
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs


! Declare local parameters
  integer(i_kind),parameter:: maxlevs=1500
  integer(i_kind),parameter:: maxdat=22
  real(r_kind),parameter:: r4_r_kind = 4.0_r_kind


  real(r_kind),parameter:: r6 = 6.0_r_kind
  real(r_kind),parameter:: r8 = 8.0_r_kind
  real(r_kind),parameter:: r90 = 90.0_r_kind
  real(r_kind),parameter:: r200 = 200.0_r_kind
  real(r_kind),parameter:: r150 = 150.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r50000 = 50000.0_r_kind
  real(r_kind),parameter:: r89_5  = 89.5_r_kind
  real(r_kind),parameter:: four_thirds = 4.0_r_kind / 3.0_r_kind

! Declare local variables
  logical good,outside,good0

  character(30) outmessage
 
  integer(i_kind) lnbufr,i,k,maxobs
  integer(i_kind) nmrecs,ibadazm,ibadwnd,ibaddist,ibadheight,kthin
  integer(i_kind) ibadstaheight,ibaderror,notgood,iheightbelowsta,ibadfit
  integer(i_kind) notgood0
  integer(i_kind) iret,kx0
  integer(i_kind) nreal,nchanl,ilat,ilon,ikx
  integer(i_kind) idomsfc
  real(r_kind) usage,ff10,sfcr,skint,t4dvo
  real(r_kind) eradkm,dlat_earth,dlon_earth
  real(r_kind) dlat,dlon,staheight,tiltangle,clon,slon,clat,slat
  real(r_kind) timeo,clonh,slonh,clath,slath,cdist,dist
  real(r_kind) rwnd,azm,height,error
  real(r_kind) azm_earth,cosazm_earth,sinazm_earth,cosazm,sinazm
  real(r_kind):: zsges

  real(r_kind),dimension(maxdat):: cdata
  real(r_kind),allocatable,dimension(:,:):: cdata_all

  real(r_double) rstation_id
  character(8) cstaid
  character(4) this_staid
  equivalence (this_staid,cstaid)
  equivalence (cstaid,rstation_id)


  integer(i_kind) loop
  real(r_kind) timemax,timemin,errmax,errmin
  real(r_kind) dlatmax,dlonmax,dlatmin,dlonmin
  real(r_kind) xscale,xscalei
  integer(i_kind) max_rrr,nboxmax
  integer(i_kind) irrr,iaaa,iaaamax,iaaamin
  real(r_kind) this_stalat,this_stalon,this_stahgt,thistime,thislat,thislon
  real(r_kind) thishgt,thisvr,corrected_azimuth,thiserr,corrected_tilt
  integer(i_kind) nsuper2_in,nsuper2_kept
  real(r_kind) errzmax
  logical, allocatable,dimension(:)     :: rusage
  integer(i_kind)  numqc,numrem
  integer(i_kind) nxdata,pmot,numall
  logical save_all


! following variables are for fore/aft separation
  integer(i_kind) irec

  data lnbufr/10/

!***********************************************************************************

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

  allocate(cdata_all(maxdat,maxobs),rusage(maxobs))

  rusage=.true.
! Initialize variables
  xscale=1000._r_kind
  xscalei=one/xscale
  max_rrr=nint(200000.0_r_kind*xscalei)
  nboxmax=1

  kx0=22500

  nmrecs=0
  irec=0

  errzmax=zero

! First process any level 2 superobs.
! Initialize variables.
  ikx=0
  do i=1,nconvtype
     if(trim(ioctype(i)) == trim(obstype))ikx = i
  end do
  if(ikx == 0) return
  pmot=pmot_conv(ikx)
  if(reduce_diag .and. pmot < 2)pmot=pmot+2
  save_all=.false.
  if(pmot /= 2 .and. pmot /= 0) save_all=.true.

  timemax=-huge(timemax)
  timemin=huge(timemin)
  errmax=-huge(errmax)
  errmin=huge(errmin)
  loop=0

  ibadazm=0
  ibadwnd=0
  ibaddist=0
  ibadheight=0
  ibadstaheight=0
  iheightbelowsta=0
  iheightbelowsta=0
  ibaderror=0
  ibadfit=0
  kthin=0
  notgood=0
  notgood0=0
  nsuper2_in=0
  nsuper2_kept=0

  if(loop==0) outmessage='level 2 superobs:'

! Open sequential file containing superobs
  open(lnbufr,file='radar_supobs_from_level2',form='unformatted')
  rewind lnbufr

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

     t4dvo=thistime+time_offset
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
     if(.not. oneobtest) then
        irrr=nint(dist*1000*xscalei)
        if(irrr<=0 .or. irrr>max_rrr) cycle
     end if 
!    Extract radial wind data
     height= thishgt
     rwnd  = thisvr
     azm_earth = corrected_azimuth

     if(regional .and. .not. fv3_regional) then
        if(oneobtest .and. learthrel_rw) then ! for non rotated winds!!!
           cosazm=cos(azm_earth*deg2rad)
           sinazm=sin(azm_earth*deg2rad)
           azm=atan2(sinazm,cosazm)*rad2deg
        else
           cosazm_earth=cos(azm_earth*deg2rad)
           sinazm_earth=sin(azm_earth*deg2rad)
           call rotate_wind_ll2xy(cosazm_earth,sinazm_earth,cosazm,sinazm,dlon_earth,dlon,dlat)
           azm=atan2(sinazm,cosazm)*rad2deg
        end if 

     else
        azm=azm_earth
     end if

     if(.not. oneobtest) then
        iaaa=azm/(r360/(r8*irrr))
        iaaa=mod(iaaa,8*irrr)
        if(iaaa<0) iaaa=iaaa+8*irrr
        iaaa=iaaa+1
        iaaamax=max(iaaamax,iaaa)
        iaaamin=min(iaaamin,iaaa)
     end if 

     error = erradar_inflate*thiserr
     errmax=max(error,errmax)

     if(thiserr>zero) errmin=min(error,errmin)
!    Perform limited qc based on azimuth angle, radial wind
!    speed, distance from radar site, elevation of radar,
!    height of observation, and observation error
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
        good=.false.
        notgood0=notgood0+1
        cycle
     end if

!    If data is good, load into output array
     if(good) then
        nsuper2_kept=nsuper2_kept+1
        ndata    =min(ndata+1,maxobs)
        nodata   =min(nodata+1,maxobs)  
        usage = zero
        if(icuse(ikx) < 0)usage=r100
        if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
           if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
        end if
        if(usage >= 100._r_kind)rusage(ndata)=.true.

        call deter_sfc2(dlat_earth,dlon_earth,t4dvo,idomsfc,skint,ff10,sfcr)
        call deter_zsfc_model(dlat,dlon,zsges)

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
        cdata(19)=dist               ! range from radar in km (used to estimatebeam spread)
        cdata(20)=zsges              ! model elevation at radar site
        cdata(21)=thiserr
        cdata(22)=two

        do i=1,maxdat
           cdata_all(i,ndata)=cdata(i)
        end do

     else
        notgood = notgood + 1
     end if

  end do
  nxdata=ndata
  ndata=0
  if(nxdata > 0)then
!    numqc=0
!    numrem=0
!    do i=1,nxdata
!      if(.not. rusage(i))then
!         numqc=numqc+1
!      else
!         numrem=numrem+1
!      end if
!    end do
!    write(6,*) ' radar3 ',numall,numrem,numqc
!     If flag to not save thinned data is set - compress data
     if(pmot /= 1)then
       do i=1,nxdata

!         pmot=0 - all obs - thin obs
!         pmot=1 - all obs
!         pmot=2 - use obs
!         pmot=3 - use obs + thin obs
          if( pmot == 0  .or. &
             (pmot == 2 .and. rusage(i))  .or. &
             (pmot == 3 .and. rusage(i))) then

             ndata=ndata+1
             if(i > ndata)then
                do k=1,maxdat
                   cdata_all(k,ndata)=cdata_all(k,i)
                end do
             end if
          end if
       end do
     end if
  end if
  nodata=nodata+ndata


  close(lnbufr) ! A simple unformatted fortran file should not be mixed with bufr I/O

  write(6,*)'READ_RADAR_L2RW_NOVADQC:  ',trim(outmessage),' reached eof on 2 superob radar file'
  write(6,*)'READ_RADAR_L2RW_NOVADQC: nsuper2_in,nsuper2_kept=',nsuper2_in,nsuper2_kept
  write(6,*)'READ_RADAR_L2RW_NOVADQC: # bad azimuths=',ibadazm
  write(6,*)'READ_RADAR_L2RW_NOVADQC: # bad winds   =',ibadwnd
  write(6,*)'READ_RADAR_L2RW_NOVADQC: # bad dists   =',ibaddist
  write(6,*)'READ_RADAR_L2RW_NOVADQC: # bad stahgts =',ibadstaheight
  write(6,*)'READ_RADAR_L2RW_NOVADQC: # bad obshgts =',ibadheight
  write(6,*)'READ_RADAR_L2RW_NOVADQC: # bad errors  =',ibaderror
  write(6,*)'READ_RADAR_L2RW_NOVADQC: # bad fit     =',ibadfit
  write(6,*)'READ_RADAR_L2RW_NOVADQC: # num thinned =',kthin
  write(6,*)'READ_RADAR_L2RW_NOVADQC: # notgood0    =',notgood0
  write(6,*)'READ_RADAR_L2RW_NOVADQC: # notgood     =',notgood
  write(6,*)'READ_RADAR_L2RW_NOVADQC: # hgt belowsta=',iheightbelowsta
  write(6,*)'READ_RADAR_L2RW_NOVADQC: timemin,max   =',timemin,timemax
  write(6,*)'READ_RADAR_L2RW_NOVADQC: errmin,max    =',errmin,errmax
  write(6,*)'READ_RADAR_L2RW_NOVADQC: dlatmin,max,dlonmin,max=',dlatmin,dlatmax,dlonmin,dlonmax
  write(6,*)'READ_RADAR_L2RW_NOVADQC: iaaamin,max,8*max_rrr=',iaaamin,iaaamax,8*max_rrr

! Write observation to scratch file
  call count_obs(ndata,maxdat,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,ndata)
  deallocate(cdata_all,rusage)

  return

end subroutine read_radar_l2rw_novadqc

!!!!!!!!!!!!!!! Added for l2rw thinning !!!!!!!!!!!!!!!
subroutine read_radar_l2rw(ndata,nodata,lunout,obstype,sis,nobs,hgtl_full)
  use kinds, only: r_kind,r_single,r_double,i_kind,i_byte
  use constants, only: zero,half,one,two,deg2rad,rearth,rad2deg,r1000,r100,r400
  use qcmod, only: erradar_inflate
  use oneobmod, only: oneobtest,learthrel_rw
  use gsi_4dvar, only: l4dvar,l4densvar,winlen,time_4dvar
  use gridmod, only: regional,nlat,nlon,tll2xy,rlats,rlons,rotate_wind_ll2xy,nsig 
  use obsmod, only: doradaroneob,oneobradid,time_offset,reduce_diag 
  use mpeu_util, only: gettablesize,gettable 
  use convinfo, only: nconvtype,icuse,ioctype
  use deter_sfc_mod, only: deter_sfc2,deter_zsfc_model
  use mpimod, only: npe
  use read_l2bufr_mod, only: radar_sites,radar_rmesh,radar_zmesh,elev_angle_max,del_time,range_max,radar_pmot  
  use constants, only: eccentricity,somigliana,grav_ratio,grav,semi_major_axis,flattening,grav_equator 
  use obsmod,only: radar_no_thinning,iadate 
  use deter_sfc_mod, only: deter_sfc2,deter_zsfc_model
  use convthin, only: make3grids,map3grids_m 

  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: obstype
  character(len=20),intent(in  ) :: sis
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: ndata,nodata
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  real(r_kind),dimension(nlat,nlon,nsig),intent(in):: hgtl_full 

! Declare local parameters
  integer(i_kind),parameter:: maxlevs=1500
  integer(i_kind),parameter:: maxdat=22
  real(r_kind),parameter:: r4_r_kind = 4.0_r_kind


  real(r_kind),parameter:: r6 = 6.0_r_kind
  real(r_kind),parameter:: r8 = 8.0_r_kind
  real(r_kind),parameter:: r90 = 90.0_r_kind
  real(r_kind),parameter:: r200 = 200.0_r_kind
  real(r_kind),parameter:: r150 = 150.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r50000 = 50000.0_r_kind
  real(r_kind),parameter:: r89_5  = 89.5_r_kind
  real(r_kind),parameter:: four_thirds = 4.0_r_kind / 3.0_r_kind
  integer(i_kind),parameter:: n_gates_max=4000  
  real(r_double),parameter:: r1e5_double = 1.0e5_r_double 
  real(r_kind),parameter:: rinv60 = 1.0_r_kind/60.0_r_kind 
  logical good,outside,good0

  character(30) outmessage
  integer(i_kind) lnbufr,i,k,maxobs
  integer(i_kind) nmrecs,ibadazm,ibadwnd,ibaddist,ibadheight,kthin
  integer(i_kind) ibadstaheight,ibaderror,notgood,iheightbelowsta,ibadfit
  integer(i_kind) notgood0
  integer(i_kind) iret,kx0
  integer(i_kind) nreal,nchanl,ilat,ilon,ikx
  integer(i_kind) idomsfc
  real(r_kind) usage,ff10,sfcr,skint,t4dvo
  real(r_kind) eradkm,dlat_earth,dlon_earth
  real(r_kind) dlat,dlon,staheight,tiltangle,clon,slon,clat,slat
  real(r_kind) timeo,clonh,slonh,clath,slath,cdist,dist
  real(r_kind) rwnd,azm,height,error
  real(r_kind) azm_earth,cosazm_earth,sinazm_earth,cosazm,sinazm
  real(r_kind):: zsges

  real(r_kind),dimension(maxdat):: cdata
  real(r_kind),allocatable,dimension(:,:):: cdata_all

  real(r_double) rstation_id
  character(8) cstaid
  character(4) this_staid
  equivalence (this_staid,cstaid)
  equivalence (cstaid,rstation_id)


  integer(i_kind) loop
  real(r_kind) timemax,timemin,errmax,errmin
  real(r_kind) dlatmax,dlonmax,dlatmin,dlonmin
  real(r_kind) xscale,xscalei
  integer(i_kind) max_rrr,nboxmax
  integer(i_kind) irrr,iaaa,iaaamax,iaaamin
  real(r_kind) this_stalat,this_stalon,this_stahgt,thistime,thislat,thislon
  real(r_kind) thishgt,thisvr,corrected_azimuth,thiserr,corrected_tilt
  integer(i_kind) nsuper2_in,nsuper2_kept
  real(r_kind) errzmax
  character(len=*),parameter:: tbname='SUPEROB_RADAR::' 
  integer(i_kind) ntot,radar_true,radar_count,inbufr,lundx,idups,idate,n_gates,levs 
  integer(i_kind) idate5(5) 
  integer(i_kind) nminref,nminthis,nrange_max 
  integer(i_kind) nobs_in,nradials_in,nradials_fail_angmax,nradials_fail_time,nradials_fail_elb,ireadmg,ireadsb

  integer(i_kind) nobs_badvr,nobs_badsr,j 
  real(r_kind) rlon0,clat0,slat0,this_stalatr,thisrange,thisazimuth,thistilt,thisvr2 
  real(r_kind) rad_per_meter,erad,ddiffmin,distfact 
  character(len=256),allocatable,dimension(:):: rtable 
  character(4),allocatable,dimension(:):: rsite 
  integer(i_kind),allocatable,dimension(:):: ruse
  character(8) chdr2,subset 
  real(r_double) rdisttest(n_gates_max),hdr(3),hdr2(12),rwnd0(3,n_gates_max) 
  character(4) stn_id 
  equivalence (chdr2,hdr2(1)) 
  real(r_kind) stn_lat,stn_lon,stn_hgt,stn_az,stn_el,t,range,vrmax,vrmin,aactual,a43,b,c,selev0,celev0,thistiltr,epsh,h,ha,rlonloc,rlatloc

  real(r_kind) celev,selev,gamma,thisazimuthr,rlonglob,rlatglob,clat1,caz0,saz0,cdlon,sdlon,caz1,saz1 
  real(r_kind):: relm,srlm,crlm,sph,cph,cc,anum,denom 
  real(r_kind) :: rmesh,xmesh,zmesh,dx,dy,dx1,dy1,w00,w01,w10,w11 
  real(r_kind), allocatable, dimension(:) :: zl_thin 
  integer(i_kind) :: ithin,zflag,nlevz,klon1,klat1,kk,klatp1,klonp1 
  real(r_kind),dimension(nsig):: hges,zges 
  real(r_kind) sin2,termg,termr,termrg,zobs 
  integer(i_kind) iout,ntdrvr_thin2 
  real(r_kind) crit1,timedif 
  logical :: luse 
  integer(i_kind) iyref,imref,idref,ihref,nout
  logical, allocatable,dimension(:)     :: rusage,rthin
  logical save_all
! integer(i_kind)  numthin,numqc,numrem
  integer(i_kind) nxdata,pmot,numall

! following variables are for fore/aft separation
  integer(i_kind) irec

  data lnbufr/10/
  if (radar_sites) then
     open(666,file=trim('gsiparm.anl'),form='formatted')
     call gettablesize(tbname,666,ntot,radar_count)
     allocate(rtable(radar_count),rsite(radar_count),ruse(radar_count))
     call gettable(tbname,666,ntot,radar_count,rtable)
     do i=1,radar_count
       read(rtable(i),*) rsite(i),ruse(i)
       write(*,'(A14,X,A4,X,I3)'),"Radar Sites: ",rsite(i),ruse(i)
     end do
  end if
  rad_per_meter= one/rearth
  erad = rearth

  eradkm=rearth*0.001_r_kind
  maxobs=2e7
  nreal=maxdat
  nchanl=0
  ilon=2
  ilat=3
  ikx=0
  do j=1,nconvtype
     if(trim(ioctype(j)) == trim(obstype))ikx = j
  end do
  iaaamax=-huge(iaaamax)
  iaaamin=huge(iaaamin)
  dlatmax=-huge(dlatmax)
  dlonmax=-huge(dlonmax)
  dlatmin=huge(dlatmin)
  dlonmin=huge(dlonmin)
  allocate(cdata_all(maxdat,maxobs),rusage(maxobs),rthin(maxobs))

  rthin=.false.
  rusage=.true.
  xscale=1000._r_kind
  xscalei=one/xscale
  max_rrr=nint(1000000.0_r_kind*xscalei) 
  nboxmax=1
  kx0=22500
  nmrecs=0
  irec=0
  errzmax=zero
  
  timemax=-huge(timemax)
  timemin=huge(timemin)
  errmax=-huge(errmax)
  errmin=huge(errmin)
  loop=0

  ibadazm=0
  ibadwnd=0
  ibaddist=0
  ibadheight=0
  ibadstaheight=0
  iheightbelowsta=0
  iheightbelowsta=0
  ibaderror=0
  ibadfit=0
  kthin=0
  notgood=0
  notgood0=0
  nsuper2_in=0
  nsuper2_kept=0
  ntdrvr_thin2=0
  nout=0
  if(loop==0) outmessage='level 2 superobs:'
  rmesh=radar_rmesh
  zmesh=radar_zmesh
  nlevz=nint(16000._r_kind/zmesh)
  xmesh=rmesh
  pmot=radar_pmot
  if(reduce_diag .and. pmot < 2)pmot=pmot+2
  save_all=.false.
  if(pmot /= 2 .and. pmot /= 0) save_all=.true.
  call make3grids(xmesh,nlevz)
  allocate(zl_thin(nlevz))
  zflag=1
  if (zflag == 1) then
    do k=1,nlevz
      zl_thin(k)=k*zmesh
    enddo
  endif
  inbufr=10
  open(inbufr,file="l2rwbufr",form='unformatted')
  rewind inbufr
  lundx=inbufr
  call openbf(inbufr,'IN',lundx)
  call datelen(10)
  iyref=iadate(1)
  imref=iadate(2)
  idref=iadate(3)
  ihref=iadate(4)
  idate5(1)=iyref
  idate5(2)=imref
  idate5(3)=idref
  idate5(4)=ihref
  idate5(5)=0          ! minutes
  call w3fs21(idate5,nminref)
  idups=0
  nobs_in=0
  nradials_in=0
  nradials_fail_angmax=0
  nradials_fail_time=0
  nradials_fail_elb=0
  ddiffmin=huge(ddiffmin)
  do while(ireadmg(inbufr,subset,idate)>=0)
    do while (ireadsb(inbufr)==0)
      call ufbint(inbufr,rdisttest,1,n_gates_max,n_gates,'DIST125M')
      if(n_gates>1) then
        do i=1,n_gates-1
          if(nint(abs(rdisttest(i+1)-rdisttest(i)))==0) then
            idups=idups+1
          else
            ddiffmin=min(abs(rdisttest(i+1)-rdisttest(i)),ddiffmin)
          end if
        end do
      end if
      distfact=zero
      if(nint(ddiffmin)==1)     distfact=250._r_kind
      if(nint(ddiffmin)==2)     distfact=125._r_kind
      if(distfact==zero) then
        write(6,*)'RADAR_BUFR_READ_ALL:  problem with level 2 bufr file, gate distance scale factor undetermined, going with 125'
        distfact=125._r_kind
      end if
      call ufbint(inbufr,hdr2,12,1,levs,'SSTN CLAT CLON HSMSL HSALG ANEL YEAR MNTH DAYS HOUR MINU SECO')
      if(hdr2(6)>elev_angle_max) then
         nradials_fail_angmax=nradials_fail_angmax+1
         cycle
      end if
      idate5(1)=nint(hdr2(7)) ; idate5(2)=nint(hdr2(8)) ; idate5(3)=nint(hdr2(9))
      idate5(4)=nint(hdr2(10)) ; idate5(5)=nint(hdr2(11))
      call w3fs21(idate5,nminthis)
      t=(real(nminthis-nminref,r_kind)+real(nint(hdr2(12)),r_kind)*rinv60)*rinv60
      timemax=max(t,timemax)
      timemin=min(t,timemin)
      if(abs(t)>del_time) then
         nradials_fail_time=nradials_fail_time+1
         cycle
      end if
      nobs_in=nobs_in+n_gates
      stn_id=chdr2
      radar_true=0 
      if (radar_sites) then 
         do i=1,radar_count 
            if (trim(stn_id) .eq. trim(rsite(i)) .and. ruse(i) .eq. 1 ) radar_true=1 
         end do 
         if (radar_true .eq. 0) cycle 
      end if 
      stn_lat=hdr2(2)
      stn_lon=hdr2(3)
      stn_hgt=hdr2(4)+hdr2(5)
      call ufbint(inbufr,hdr,3,1,levs,'ANAZ ANEL QCRW')
      nradials_in=nradials_in+1
      stn_az=r90-hdr(1)
      stn_el=hdr(2)
      call ufbint(inbufr,rwnd0,3,n_gates_max,n_gates,'DIST125M DMVR DVSW')
      do i=1,n_gates
        range=distfact*rwnd0(1,i)
        if(range>range_max) then
           nrange_max=nrange_max+1
           cycle
        end if
        if(rwnd0(2,i)>r1e5_double) then
           nobs_badvr=nobs_badvr+1
           cycle
        end if
        if(rwnd0(3,i)>r1e5_double) then
           nobs_badsr=nobs_badsr+1
           cycle
        end if
        this_stalat=stn_lat
        if(abs(this_stalat)>r89_5) cycle
        this_stalon=stn_lon
        rlon0=deg2rad*this_stalon
        this_stalatr=this_stalat*deg2rad
        clat0=cos(this_stalatr) ; slat0=sin(this_stalatr)
        this_staid=stn_id
        this_stahgt=stn_hgt
        thisrange=  range
        thisazimuth=stn_az
        thistilt=stn_el
        thisvr=rwnd0(2,i)
        vrmax=max(vrmax,thisvr)
        vrmin=min(vrmin,thisvr)
        thisvr2=rwnd0(2,i)**2
        thiserr=5.0_r_kind 
        errmax=max(errmax,thiserr)
        errmin=min(errmin,thiserr)
        thistime=t
        aactual=erad+this_stahgt
        a43=four_thirds*aactual
        thistiltr=thistilt*deg2rad
        selev0=sin(thistiltr)
        celev0=cos(thistiltr)
        b=thisrange*(thisrange+two*aactual*selev0)
        c=sqrt(aactual*aactual+b)
        ha=b/(aactual+c)
        epsh=(thisrange*thisrange-ha*ha)/(r8*aactual)
        h=ha-epsh
        thishgt=this_stahgt+h
        celev=celev0
        selev=selev0
        if(thisrange>=one) then
           celev=a43*celev0/(a43+h)
           selev=(thisrange*thisrange+h*h+two*a43*h)/(two*thisrange*(a43+h))
        end if
        corrected_tilt=atan2(selev,celev)*rad2deg
        gamma=half*thisrange*(celev0+celev)
!       Get earth lat lon of superob
        thisazimuthr=thisazimuth*deg2rad
        rlonloc=rad_per_meter*gamma*cos(thisazimuthr)
        rlatloc=rad_per_meter*gamma*sin(thisazimuthr)
        RELM=rlonloc
        SRLM=SIN(RELM)
        CRLM=COS(RELM)
        SPH=SIN(rlatloc)
        CPH=COS(rlatloc)
        CC=CPH*CRLM
        ANUM=CPH*SRLM
        DENOM=clat0*CC-slat0*SPH
        rlonglob=rlon0+ATAN2(ANUM,DENOM)
        rlatglob=ASIN(clat0*SPH+slat0*CC)
        thislat=rlatglob*rad2deg
        thislon=rlonglob*rad2deg
        if(abs(thislat)>r89_5) cycle
        clat1=cos(rlatglob)
        caz0=cos(thisazimuthr)
        saz0=sin(thisazimuthr)
        cdlon=cos(rlonglob-rlon0)
        sdlon=sin(rlonglob-rlon0)
        caz1=clat0*caz0/clat1
        saz1=saz0*cdlon-caz0*sdlon*slat0
        corrected_azimuth=atan2(saz1,caz1)*rad2deg

        if (doradaroneob .and. (oneobradid /= this_staid)) cycle 
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
        t4dvo=time_offset+thistime
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
          if (outside) then
            cycle
          end if
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
        if(.not. oneobtest) then
          irrr=nint(dist*1000._r_kind*xscalei)
          if(irrr<=0 .or. irrr>max_rrr) cycle
        end if
!    Extract radial wind data
        height= thishgt
        rwnd  = thisvr
        azm_earth = corrected_azimuth
        if(regional) then
          if(oneobtest .and. learthrel_rw) then ! for non rotated winds!!!
            cosazm=cos(azm_earth*deg2rad)
            sinazm=sin(azm_earth*deg2rad)
            azm=atan2(sinazm,cosazm)*rad2deg
          else
            cosazm_earth=cos(azm_earth*deg2rad)
            sinazm_earth=sin(azm_earth*deg2rad)
            call rotate_wind_ll2xy(cosazm_earth,sinazm_earth,cosazm,sinazm,dlon_earth,dlon,dlat)
            azm=atan2(sinazm,cosazm)*rad2deg
          end if
        else
          azm=azm_earth
        end if
!####################       Data thinning       ###################
        if(ndata>maxobs) exit
        ithin=1 !number of obs to keep per grid box
        if(radar_no_thinning) then
          ithin=-1
        endif
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
            sin2  = sin(thislat)*sin(thislat)
            termg = grav_equator * &
               ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
            termr = semi_major_axis /(one + flattening + grav_ratio -  &
               two*flattening*sin2)
            termrg = (termg/grav)*termr
            do kk=1,nsig
               zges(kk) = (termr*hges(kk)) / (termrg-hges(kk))
               zl_thin(kk)=zges(kk)
            end do
          endif
          zobs = height
          if (l4dvar) then
            timedif = zero
          else
            timedif=abs(t4dvo-time_offset)
          endif
          crit1 = timedif/r6+half
          call map3grids_m(1,save_all,zflag,zl_thin,nlevz, &
             dlat_earth,dlon_earth,zobs,crit1,ndata,&
             luse,maxobs,rthin,.false.,.false.)
          if (.not. luse) then
             ntdrvr_thin2=ntdrvr_thin2+1
             cycle
          endif
        else
          ndata =ndata+1
        endif
        iout=ndata
!####################       Data thinning       ###################
        if(.not. oneobtest) then
          iaaa=azm/(r360/(r8*irrr))
          iaaa=mod(iaaa,8*irrr)
          if(iaaa<0) iaaa=iaaa+8*irrr
          iaaa=iaaa+1
          iaaamax=max(iaaamax,iaaa)
          iaaamin=min(iaaamin,iaaa)
        end if
        error = erradar_inflate*thiserr
        errmax=max(error,errmax)
        if(thiserr>zero) errmin=min(error,errmin)
!    Perform limited qc based on azimuth angle, radial wind
!    speed, distance from radar site, elevation of radar,
!    height of observation, and observation error
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
        end if

!    If data is good, load into output array
        if(good) then
          
          usage = zero
          if(icuse(ikx) < 0)then
            rusage(ndata)=.false.
            usage=r100
          end if

!    Get information from surface file necessary for conventional data here
          call deter_zsfc_model(dlat,dlon,zsges)
          call deter_sfc2(dlat_earth,dlon_earth,t4dvo,idomsfc,skint,ff10,sfcr)

          nsuper2_kept=nsuper2_kept+1
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
          cdata(19)=dist               ! range from radar in km (used to estimatebeam spread)
          cdata(20)=zsges              ! model elevation at radar site
          cdata(21)=thiserr
          cdata(22)=two
          do j=1,maxdat
            cdata_all(j,iout)=cdata(j) 
          end do
        else
          notgood = notgood + 1
        end if
      end do
    end do
  end do
  close(lnbufr) ! A simple unformatted fortran file should not be mixed with bufr I/O
  nxdata=ndata
  ndata=0
  if(nxdata > 0)then
!    numthin=0
!    numqc=0
!    numrem=0
!    do i=1,nxdata
!      if(.not. rusage(i))then
!         numqc=numqc+1
!      else if(rthin(i))then
!         numthin=numthin+1
!      else
!         numrem=numrem+1
!      end if
!    end do
!    write(6,*) ' radar2 ',numall,numrem,numqc,numthin
!   If thinned data set quality mark to 14
     if (ithin == 1 ) then
        do i=1,nxdata
          if(rthin(i))cdata_all(12,i)=101._r_kind
       end do
     end if

!     If flag to not save thinned data is set - compress data
     if(pmot /= 1)then
       do i=1,nxdata

!         pmot=0 - all obs - thin obs
!         pmot=1 - all obs
!         pmot=2 - use obs
!         pmot=3 - use obs + thin obs
          if((pmot == 0 .and. .not. rthin(i)) .or. &
             (pmot == 2 .and. (rusage(i) .and. .not. rthin(i)))  .or. &
             (pmot == 3 .and. rusage(i))) then

             ndata=ndata+1
             if(i > ndata)then
                do k=1,maxdat
                   cdata_all(k,ndata)=cdata_all(k,i)
                end do
             end if
          end if
        end do
     end if
  end if
  nodata=nodata+ndata
  write(6,*)'READ_RADAR_L2RW:  ',trim(outmessage),' reached eof on 2 superob radar file'
  write(6,*)'READ_RADAR_L2RW: nsuper2_in,nsuper2_kept=',nsuper2_in,nsuper2_kept
  write(6,*)'READ_RADAR_L2RW: # bad winds =',ibadwnd,nobs_badvr,nobs_badsr
  write(6,*)'READ_RADAR_L2RW: # num thinned =',kthin,ntdrvr_thin2
  write(6,*)'READ_RADAR_L2RW: timemin,max   =',timemin,timemax
  write(6,*)'READ_RADAR_L2RW: errmin,max    =',errmin,errmax
  write(6,*)'READ_RADAR_L2RW: dlatmin,max,dlonmin,max=',dlatmin,dlatmax,dlonmin,dlonmax

! Write observation to scratch file
  deallocate(rusage,rthin)
  call count_obs(ndata,maxdat,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,ndata)
  deallocate(cdata_all)
  if (radar_sites) deallocate(rtable,rsite,ruse)  
  deallocate(zl_thin)
  return

end subroutine read_radar_l2rw
!!!!!!!!!!!!!!! End added for l2rw thinning !!!!!!!!!!!!!!!

subroutine get_azimuth_corrected(this_stalon,this_stalat,    &
                                 thislon,    thislat,        &
                                 thisazm,    corrected_azm)
!=======================================================================!
! Author: G. Zhao

! Follow the alogrithm in subroutine radar_bufr_read_all (in
! read_l2bufr_mod.f90) to get corrected azimuth
!
! Note: all input and output are in unit of degree, not radian.                            
!=======================================================================!

  use kinds, only: r_kind,r_single,i_kind
  use constants, only: one,half,two,deg2rad,rad2deg,zero_single

  implicit none

  real(r_kind) ,  intent(in   ) :: this_stalon,this_stalat,thislon,thislat,thisazm
  real(r_kind) ,  intent(out  ) :: corrected_azm

! local variables
  real(r_kind) rlon0,clat0,slat0,rlonglob,rlatglob,clat1,caz0,saz0,cdlon,sdlon,caz1,saz1
  real(r_kind) this_stalatr,thisazimuthr

  rlon0=this_stalon*deg2rad
  this_stalatr=this_stalat*deg2rad
  rlonglob=thislon*deg2rad
  rlatglob=thislat*deg2rad
  thisazimuthr=thisazm*deg2rad

! Get corrected azimuth

  clat0=cos(this_stalatr)
  slat0=sin(this_stalatr)
  clat1=cos(rlatglob)
  caz0=cos(thisazimuthr)
  saz0=sin(thisazimuthr)
  cdlon=cos(rlonglob-rlon0)
  sdlon=sin(rlonglob-rlon0)
  caz1=clat0*caz0/clat1
  saz1=saz0*cdlon-caz0*sdlon*slat0

  corrected_azm=atan2(saz1,caz1)*rad2deg

  return

end subroutine get_azimuth_corrected
!
!
!
subroutine get_rdr_obshgttilt(thistilt,thisrange,this_stahgt,        &
                              thishgt,corrected_tilt)
!-------------------------------------------------------------------!
! following the algorithm used in read_l2bufr_mod.f90 to calculate the
! observation height (radar beam height).
! 
! 2017-10-19    CAPS(G. Zhao)
! Input:
!       thistilt        scan tilt of radar beam (elevation angle at radar station,
!                       degree)
!       thisrange       radar beam range at observation point (meter)
!       this_stahgt     height of radar station (meter)
!
! Output:
!       thishgt         radar beam height at observation point (meter)
!       corrected_tilt  elevation angle of radar beam at observation point
!                       (degree, sometimes called corrected tilt angle)
!                       Due to curvature of radar beam and surface of earth
!-------------------------------------------------------------------!
!
  use kinds, only: r_kind,r_single,i_kind
  use constants, only: zero,half,one,two,rearth,deg2rad,rad2deg,zero_quad,one_quad

  implicit none

  real(r_kind)   ,intent(in   ) :: thistilt, thisrange, this_stahgt
  real(r_kind)   ,intent(  out) :: thishgt
  real(r_kind)   ,intent(  out) :: corrected_tilt

! local parameters
  real(r_kind),parameter:: four_thirds = 4.0_r_kind / 3.0_r_kind
  real(r_kind),parameter:: r8          = 8.0_r_kind
  real(r_kind),parameter:: r89_5       = 89.5_r_kind
  real(r_kind),parameter:: r90         = 90.0_r_kind
  real(r_kind),parameter:: missval     = -999.0_r_kind

! local variables
  real(r_kind) a43,aactual,b,c,selev0,celev0,epsh,erad,h,ha
  real(r_kind) rad_per_meter
  real(r_kind) thistiltr
  real(r_kind) celev,selev


  rad_per_meter= one/rearth
  erad = rearth

! use 4/3rds rule to get elevation of radar beam
! (if local temperature available, then vertical position can be
! estimated with greater accuracy)

  aactual=erad+this_stahgt
  a43=four_thirds*aactual
  thistiltr=thistilt*deg2rad
  selev0=sin(thistiltr)
  celev0=cos(thistiltr)
  b=thisrange*(thisrange+two*aactual*selev0)
  c=sqrt(aactual*aactual+b)
  ha=b/(aactual+c)
  epsh=(thisrange*thisrange-ha*ha)/(r8*aactual)
  h=ha-epsh
  thishgt=this_stahgt+h

  if(thishgt < zero)then ! don't use observation if it is likely to be below sealevel
      thishgt=missval
  end if

! Get corrected tilt angle (i.e., elevation angle at obs location)
  celev=celev0
  selev=selev0
  if(thisrange>=one) then
      celev=a43*celev0/(a43+h)
      selev=(thisrange*thisrange+h*h+two*a43*h)/(two*thisrange*(a43+h))
  end if
  corrected_tilt=atan2(selev,celev)*rad2deg

  return

end subroutine get_rdr_obshgttilt

!===============================================================================!
!      subroutines from CAPS ARPS package
!      adas/radarlib3d.f90
!      including: 
!        subroutine beamelv dhdrange
!===============================================================================!
!
subroutine beamelv(height,sfcrng,elvang,range)
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Calculate the elevation angle (elvang) and the along
!  ray-path distance (range) of a radar beam
!  crossing through the given height and along-ground
!  distance.
!
!  This method assumes dn/dh is constant such that the
!  beam curves with a radius of 4/3 of the earth's radius.
!  This is dervied from Eq. 2.28 of Doviak and Zrnic',
!  Doppler Radar and Weather Observations, 1st Ed.
!
!-----------------------------------------------------------------------
!
!  AUTHOR: Keith Brewster
!  10/10/95
!
!  MODIFICATION HISTORY:
!
!-----------------------------------------------------------------------
!
!  INPUT:
!    height   Height (meters) of beam above ground.
!    sfcrng   Distance (meters) of point along ground from radar.
!
!  OUTPUT
!    elvang   Elevation angle (degrees) of radar beam
!    range    Distance (meters) along radar beam from radar
!
!-----------------------------------------------------------------------
!

!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_kind,r_single,i_kind,r_double

  implicit none
  real(r_kind), intent(in   ) :: height
  real(r_kind), intent(in   ) :: sfcrng
  real(r_kind), intent(  out) :: elvang
  real(r_kind), intent(  out) :: range
!
  real(r_double) :: eradius,frthrde,rad2deg
  parameter (eradius=6371000._r_double,                                          &
             frthrde=(4._r_double*eradius/3._r_double),                          &
             rad2deg=(180._r_double/3.14592654_r_double))
!
  real(r_double):: elvrad,hgtdb,rngdb,drange
!
  if (sfcrng > 0._r_kind) then

    hgtdb=frthrde+dble(height)
    rngdb=dble(sfcrng)/frthrde

    elvrad = atan((hgtdb*cos(rngdb) - frthrde)/(hgtdb * sin(rngdb)))
    drange = (hgtdb*sin(rngdb))/cos(elvrad)
    elvang=rad2deg*elvrad
    range=drange

  else

    elvang=90._r_kind
    range=height

  end if
  return
end subroutine beamelv

!

subroutine dhdrange(elvang,range,dhdr)
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Calculate the local change in height of the radar
!  beam with respect to a change in range.  Due to
!  curvature of the beam and the earth's surface this is
!  generally different what would be calculated from the
!  elevation angle measured at the radar.  This derivative
!  is needed for finding 3-d velocities from radial winds
!  and accounting for terminal velocity of precipitation.
!
!  This formulation, consistent with subroutine beamhgt,
!  assumes a 4/3 earth radius beam curvature.  This formula
!  is obtained by differentiating Eq 2.28 of Doviak and
!  Zrnic', Doppler Radar and Weather Observations, 1st Ed.
!
!-----------------------------------------------------------------------
!
!  AUTHOR: Keith Brewster
!  06/22/95
!
!  MODIFICATION HISTORY:
!
!
!-----------------------------------------------------------------------
!
!  INPUT:
!
!    elvang   Elevation angle (degrees) of radar beam
!    range    Distance (meters) along radar beam from radar
!
!  OUTPUT:
!    dhdr     Change in height per change in range (non-dimensional)
!
!
!-----------------------------------------------------------------------
!

!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_kind,r_single,i_kind,r_double

  implicit none
  real(r_kind), intent(in   ) :: range
  real(r_kind), intent(in   ) :: elvang
  real(r_kind), intent(  out) :: dhdr
!
  real(r_double) :: eradius,frthrde,eighthre,fthsq,deg2rad
  parameter (eradius=6371000._r_double,                                 &
             frthrde=(4._r_double*eradius/3._r_double),                 &
             eighthre=(8._r_double*eradius/3._r_double),                &
             fthsq=(frthrde*frthrde),                                   &
             deg2rad=(3.14592654_r_double/180._r_double))
!
  real(r_double) :: sinelv,dhdrdb,drange
!
  drange=dble(range)
  sinelv=sin(deg2rad*dble(elvang))
  dhdrdb = (drange+frthrde*sinelv)/                                     &
         sqrt(drange*drange + fthsq + eighthre*drange*sinelv)
  dhdr = dhdrdb
!
 return 
end subroutine dhdrange
