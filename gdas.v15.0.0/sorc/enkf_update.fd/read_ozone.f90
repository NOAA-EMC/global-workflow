subroutine read_ozone(nread,ndata,nodata,jsatid,infile,gstime,lunout, &
           obstype,twind,sis,ithin,rmesh,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_ozone                    read ozone data
!   prgmmr: yang             org: np23                date: 1998-05-15
!
! abstract:  This routine reads SBUV/2 ozone observations.  Both layer
!            and total column values are read in.  The routine has
!            the ability to read both IEEE and BUFR format SBUV/2
!            ozone data files. OMI and GOME data is optionally thinned
!            to a specific resolution using simple quality control checks.
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
!   2004-09-17  todling - fixed intent of jsatid
!   2004-12-02  todling - compilation in OSF1 forces big_endian for bufr files;
!                         need to force little_endian for ieee files
!   2004-12-22  kokron  - change cpp tokens to add support for ifort compiler
!                         efc does not have a convert option so it should use
!                         the other 'open'
!   2005-03-14  treadon - define numeric constants to r_kind precision
!   2005-05-12  wu - add OMI total ozone 
!   2005-06-27  guo     - bug fix: hour read from header was incorrect
!   2005-09-08  derber - modify to use input group time window
!   2005-09-19  treadon - add check on NOAA-17 sbuv data (toss bad data)
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-12-23  treadon - bound longitude to be less than 360.0
!   2006-01-26  treadon - remove ieee sbuv option
!   2006-02-03  derber  - modify for new obs control and obs count
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2007-07-10  zhou    - modify to read version 8 SBUV/2 BUFR data(keep 
!                         option to read version 6 data), also add 
!                         total ozone and ozone profile quality control.
!   2007-09-11  h.liu - add kidsat for nimbus-7, n09, n11, n14
!   2007-10-16  zhou    - organize ozone flag control for all satellites
!   2008-04-16  h.liu   - thin OMI and read in GOME data
!   2008-05-27  safford - rm unused vars and uses
!   2008-05-30  treadon - accept  version8 poq=7 obs for further processing
!   2008-06-01  treadon - adjust logic to correctly handle zero length BUFR files
!   2008-06-03  treadon - add use_poq7 flag
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2009-01-20  sienkiewicz - merge in changes for MLS ozone
!   2009-04-21  derber  - add ithin to call to makegrids
!   2009-3-05   h.liu   - read in OMI bufr, QC GOME2 and OMI
!   2009-7-02   h.liu   - toss the OMI data with AFBO=3 (c-pair correction) and clean up codes
!   2010-05-26  treadon - add timedif=zero for l4dvar (used in thinning)
!   2010-06-02  sienkiewicz - care for closing bufr other than for o3lev
!   2011-07-04  todling  - fixes to run either single or double precision
!   2011-08-01  lueken  - replaced F90 with f90 (no machine logic)
!   2012-10-12  h.liu  - read in MLS v2 Near Real Time (NRT) and v2.2 standard bufr data
!   2013-01-17  h.liu  - read in MLS v3 Near Real Time (NRT) 
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2013-02-05  guo     - STOP in dec2bin() was replaced with die() to signal an _abort_.
!   2014-02-03  guo	- removed unused "o3lev" handling, which can (and should) be
!                         implemented again in module m_extOzone, if ever needed.
!   2015-02-23  Rancic/Thomas - add thin4d to time window logical
!
!   input argument list:
!     obstype  - observation type to process
!     jsatid   - satellite id to read
!     infile   - unit from which to read ozone data
!     gstime   - analysis time in minutes from reference date
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!
!   output argument list:
!     nread    - number of sbuv/omi ozone observations read
!     ndata    - number of sbuv/omi ozone profiles retained for further processing
!     nodata   - number of sbuv/omi ozone observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! remarks:
!   NCEP stopped producing IEEE format sbuv ozone files in April 2004.  
!   Hence, the IEEE portion of this routine no future application.  It 
!   is retained in the GSI package for use with retrospective runs.  The
!   IEEE portion of this routine may be removed from the GSI at a later date.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: makegrids,map2tgrid,destroygrids, &
      finalcheck,itxmax
  use gridmod, only: nlat,nlon,regional,tll2xy,rlats,rlons
  use constants, only: deg2rad,zero,rad2deg,one_tenth,r60inv,two
  use obsmod, only: iadate,nloz_v6,nloz_v8
  use convinfo, only: nconvtype, &
      icuse,ictype,ioctype
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,thin4d
  use qcmod, only: use_poq7
  use ozinfo, only: jpch_oz,nusis_oz,iuse_oz
  use mpimod, only: npe
  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile,jsatid
  character(len=20),intent(in  ) :: sis
  integer(i_kind) ,intent(in   ) :: lunout,ithin
  integer(i_kind) ,intent(inout) :: nread
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  integer(i_kind) ,intent(inout) :: ndata,nodata
  real(r_kind)    ,intent(in   ) :: gstime,twind,rmesh

! Declare local parameters
  real(r_kind),parameter:: r6   = 6.0_r_kind
  real(r_kind),parameter:: r76  = 76.0_r_kind
  real(r_kind),parameter:: r84  = 84.0_r_kind

  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: rmiss = -9999.9_r_kind
  real(r_kind),parameter:: badoz = 10000.0_r_kind

! Declare local variables
  logical outside,version6,version8,iuse
  
  character(2) version
  character(8) subset,subset6,subset8
  character(49) ozstr,ozostr
  character(63) lozstr
  character(51) ozgstr
  character(27) ozgstr2
  character(42) ozostr2
  character(64) mlstr
  character(14) mlstrl

  integer(i_kind) maxobs,nozdat,nloz
  integer(i_kind) idate,jdate,ksatid,kk,iy,iret,im,ihh,idd,lunin
  integer(i_kind) nmind,i
  integer(i_kind) nmrecs,k,ilat,ilon,nreal,nchanl
! integer(i_kind) ithin,kidsat
  integer(i_kind) kidsat
  integer(i_kind) idate5(5)
  integer(i_kind) JULIAN,IDAYYR,IDAYWK
  integer(i_kind) ikx
  integer(i_kind) decimal,binary(14),binary_mls(18)


  integer(i_kind) itx,itt,ipoq7

  real(r_kind) tdiff,sstime,dlon,dlat,t4dv,timedif,crit1,dist1
  real(r_kind) slons0,slats0,rsat,solzen,solzenp,dlat_earth,dlon_earth
  real(r_kind),allocatable,dimension(:):: poz

! maximum number of observations set to 
  real(r_kind),allocatable,dimension(:,:):: ozout
  real(r_double) toq,poq
  real(r_double),dimension(nloz_v6):: ozone_v6
  real(r_double),dimension(29,nloz_v8):: ozone_v8
  real(r_double),dimension(10):: hdroz
  real(r_double),dimension(10):: hdrozg
  real(r_double),dimension(5):: hdrozg2
  real(r_double),dimension(10):: hdrozo
  real(r_double),dimension(8) :: hdrozo2
  real(r_double),dimension(13):: hdrmls
  real(r_double),allocatable,dimension(:,:) :: hdrmlsl
  real(r_kind),allocatable,dimension(:):: mlspres,mlsoz,mlsozpc,usage1
  integer(i_kind),allocatable,dimension(:):: ipos

  real(r_double) totoz,hdrmls13
  integer(i_kind) :: k0
  logical :: first

! MLS data version: mlsv=22 is version 2.2 standard data; 
!                   mlsv=20 is v2 near-real-time data
!                   mlsv=30 is v3 near-real-time data
  integer(i_kind) :: mlsv

  data lozstr &
       / 'OSP12 OSP11 OSP10 OSP9 OSP8 OSP7 OSP6 OSP5 OSP4 OSP3 OSP2 OSP1 ' /
  data ozgstr &
       / 'SAID CLAT CLON YEAR DOYR HOUR MINU SECO SOZA SOLAZI' /
  data ozgstr2 &
       / 'CLDMNT SNOC ACIDX STKO FOVN' /
  data ozostr &
       / 'SAID CLAT CLON YEAR MNTH DAYS HOUR MINU SECO SOZA' /
! since 2009020412, the omi bufr contains fovn
  data ozostr2 &
       / 'CLDMNT ACIDX STKO VZAN TOQC TOQF FOVN AFBO' /

  data mlstr &
       / 'SAID CLAT CLON YEAR MNTH DAYS HOUR MINU SECO SOZA CONV MLST PCCF' /
  data mlstrl &
       / 'PRLC OZMX OZMP' /

  data lunin / 10 /
  data subset6 / 'NC008010' /
  data subset8 / 'NC008011' /

!**************************************************************************
! Set constants.  Initialize variables
  rsat=999._r_kind
  maxobs=1e6
  ilon=3
  ilat=4
  ipoq7=0
  if (use_poq7) ipoq7=7


! Separately process sbuv or omi ozone
  
  if (obstype == 'sbuv2' ) then

     nreal=9
     open(lunin,file=trim(infile),form='unformatted')
     nmrecs=0
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     call readmg(lunin,subset,idate,iret)

     version6 = .false.
     version8 = .false.
     if (subset == subset6) then
        version6 = .true.
        nloz     = nloz_v6
        version  = 'v6'
     elseif (subset == subset8) then
        version8 = .true. 
        nloz     = nloz_v8
        version  = 'v8'
     else
        write(6,*)'READ_OZONE:  *** WARNING: unknown sbuv version type, subset=',subset
        write(6,*)' infile=',trim(infile), ', lunin=',lunin, ', obstype=',obstype,', jsatid=',jsatid
        write(6,*)' SKIP PROCESSING OF THIS SBUV FILE'
        goto 170
     endif

!    Set dependent variables and allocate arrays
     nchanl=nloz+1
     nozdat=nreal+nchanl
     allocate (ozout(nozdat,maxobs))
     allocate (  poz(nloz+1))


!    Set BUFR string based on sbuv version
     if (version6) then
        ozstr='SAID CLAT CLON YEAR MNTH DAYS HOUR MINU OSZA OPSZ'
     else if (version8) then
        ozstr='SAID CLAT CLON YEAR MNTH DAYS HOUR MINU SECO SOZA'
     endif

     if(iret/=0) goto 160
     
110  continue
     call readsb(lunin,iret)
     if (iret/=0) then
        call readmg(lunin,subset,jdate,iret)
        if (iret/=0) goto 150
        goto 110
     endif
     
!    extract header information
!    BUFR code values for satellite identifiers are listed in
!    Dennis Keyser's website,
!    http://www.emc.ncep.noaa.gov/mmb/papers/keyser/Satellite_Historical.txt

     call ufbint(lunin,hdroz,10,1,iret,ozstr)
     rsat = hdroz(1); ksatid=rsat
     if(jsatid == 'nim07') kidsat = 767
     if(jsatid == 'n09') kidsat = 201
     if(jsatid == 'n11') kidsat = 203
     if(jsatid == 'n14') kidsat = 205
     if(jsatid == 'n16') kidsat = 207
     if(jsatid == 'n17') kidsat = 208
     if(jsatid == 'n18') kidsat = 209
     if(jsatid == 'n19') kidsat = 223

     if (ksatid /= kidsat) go to 110

     nmrecs=nmrecs+nloz+1
    
!    Convert observation location to radians
     slats0= hdroz(2)
     slons0= hdroz(3)
     if(abs(slats0)>90._r_kind .or. abs(slons0)>r360) go to 110  
     if(slons0< zero) slons0=slons0+r360
     if(slons0==r360) slons0=zero
     dlat_earth = slats0 * deg2rad
     dlon_earth = slons0 * deg2rad

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if(outside) go to 110
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif
     
!    Special check for NOAA-17 version 6
!    Before July 2007 NOAA-17 SBUV/2 has a stray light problem which produces
!    erroneous ozone profile retrievals for a limited portion
!    of its measurements. The contaminated signals only occur
!    in the Southern Hemisphere and only for Solar Zenith
!    Angles (SZA) greater than 76 Degrees.

     if (version6) then
        solzen = hdroz(9)    ! solar zenith angle
        solzenp= hdroz(10)   ! profile solar zenith angle
        if (ksatid==208 .and. dlat_earth<zero .and. solzenp > r76) goto 110
     else if(version8)then
        solzen = hdroz(10)    ! solar zenith angle
     endif

!    Convert observation time to relative time
     idate5(1) = hdroz(4)  !year
     idate5(2) = hdroz(5)  !month
     idate5(3) = hdroz(6)  !day
     idate5(4) = hdroz(7)  !hour
     idate5(5) = hdroz(8)  !minute
     call w3fs21(idate5,nmind)
     t4dv=real((nmind-iwinbgn),r_kind)*r60inv
     sstime=real(nmind,r_kind)
     tdiff=(sstime-gstime)*r60inv
     if (l4dvar.or.l4densvar) then
        if(t4dv<zero .OR. t4dv>winlen) goto 110
     else
        if(abs(tdiff) > twind) goto 110
     end if
     
!    Extract layer ozone values and compute profile total ozone
     if (version8) then
        call ufbseq(lunin,ozone_v8,29,21,iret,'OZOPQLSQ')
        totoz=zero
        do k=1,nloz
           kk=nloz-k+1
           poz(k) = ozone_v8(6,kk)
           totoz=totoz+ozone_v8(6,k)
        end do
        poz(nloz+1) = totoz
     endif
 
     if (version6) then
        call ufbint(lunin,ozone_v6,nloz,1,iret,lozstr)
        do k=1,nloz
           kk=nloz-k+1
           poz(k) = ozone_v6(kk)
        end do

!       extract total ozone
        call ufbint(lunin,totoz,1,1,iret,'OTSP')
        poz(nloz+1) = totoz
     endif


!    Extract and apply version 8 total and profile ozone quaility information
!    Toss observations for which the total ozone error code is neither 0 nor 2
!    Toss observations for which the profile ozone error code is neither 0 nor 1
!    NOTES:  
!      1) Profile ozone error code 0 identifies good data; 1 identifies good
!         data with a solar zenith angle > 84 degrees;  7 identifies profile
!         for which stray light correction applied
!      2) Total ozone error code 0 indentifies good data; 2 identifies good 
!         data with a solar zenith angle > 84 degrees.
!      3) We do not use the version 6 error flags.   Thus, initialize toq and
!         poq to 0 (use the data)

     toq=0._r_double
     poq=0._r_double
     if (version8) then
        call ufbint(lunin,toq,1,1,iret,'SBUVTOQ')
        call ufbint(lunin,poq,1,1,iret,'SBUVPOQ')
        if (toq/=0 .and. toq/=2) goto 110
        if (poq/=0 .and. poq/=1 .and. poq/=ipoq7) goto 110
     endif

!    Check ozone layer values.  If any layer value is bad, toss entire profile
     do k=1,nloz
        if (poz(k)>badoz) goto 110
     end do
     
!    Write ozone record to output file
     ndata=min(ndata+1,maxobs)
     nodata=nodata+nloz+1
     ozout(1,ndata)=rsat
     ozout(2,ndata)=t4dv
     ozout(3,ndata)=dlon               ! grid relative longitude
     ozout(4,ndata)=dlat               ! grid relative latitude
     ozout(5,ndata)=dlon_earth*rad2deg ! earth relative longitude (degrees)
     ozout(6,ndata)=dlat_earth*rad2deg ! earth relative latitude (degrees)
     ozout(7,ndata)=toq                ! total ozone error flag
     ozout(8,ndata)=poq                ! profile ozone error flag
     ozout(9,ndata)=solzen             ! solar zenith angle
     do k=1,nloz+1
        ozout(k+9,ndata)=poz(k)
     end do

!    Loop back to read next profile
     goto 110

!    End of bufr ozone block

! Process GOME-2 data

  else if ( obstype == 'gome') then

!    Make thinning grids
     call makegrids(rmesh,ithin)

     open(lunin,file=trim(infile),form='unformatted')
     nmrecs=0
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     call readmg(lunin,subset,idate,iret)

     if (subset == 'NC008012') then
        write(6,*)'READ_OZONE:  GOME-2 data type, subset=',subset
     else
        write(6,*)'READ_OZONE:  *** WARNING: unknown ozone data type, subset=',subset
        write(6,*)' infile=',trim(infile), ', lunin=',lunin, ', obstype=',obstype,', jsatid=',jsatid
        goto 170
     endif

!    Set dependent variables and allocate arrays
     nreal=14
     nloz=0
     nchanl=1
     nozdat=nreal+nchanl
     allocate (ozout(nozdat,itxmax))
     do k=1,itxmax
        do i=1,nozdat
           ozout(i,k)=rmiss
        end do
     end do

     iy=0
     idd=0
     ihh=0
     if(iret/=0) goto 160

120  continue
     call readsb(lunin,iret)
     if (iret/=0) then
        call readmg(lunin,subset,jdate,iret)
        if (iret/=0) goto 150
        goto 120
     endif
     
!    extract header information
     call ufbint(lunin,hdrozg,10,1,iret,ozgstr)
     call ufbint(lunin,hdrozg2,5,1,iret,ozgstr2)
     rsat = hdrozg(1); ksatid=rsat

     if(jsatid == 'metop-a')kidsat = 4
     if(jsatid == 'metop-b')kidsat = 3
     if(jsatid == 'metop-c')kidsat = 5

     if (ksatid /= kidsat) go to 120

!    NESDIS does not put a flag for high SZA gome-2 data (SZA > 84 degree)
     if ( hdrozg(9) > r84 ) go to 120

     nmrecs=nmrecs+nloz+1
    
!    Convert observation location to radians
     slats0= hdrozg(2)
     slons0= hdrozg(3)
     if(abs(slats0)>90._r_kind .or. abs(slons0)>r360) go to 120  
     if(slons0< zero) slons0=slons0+r360
     if(slons0==r360) slons0=zero
     dlat_earth = slats0 * deg2rad
     dlon_earth = slons0 * deg2rad

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if(outside) go to 120
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif
     
!    Convert observation time to relative time
     idate5(1) = hdrozg(4)  !year
     IDAYYR = hdrozg(5)     ! Day of year
     JULIAN = -31739 + 1461 * (idate5(1) + 4799) /4 &
              -3 * ((idate5(1) + 4899) / 100) / 4 + IDAYYR
     call w3fs26(JULIAN,idate5(1),idate5(2),idate5(3),IDAYWK,IDAYYR)
!    idate5(2) month
!    idate5(3) day
     idate5(4) = hdrozg(6)  !hour
     idate5(5) = hdrozg(7)  !minute
     call w3fs21(idate5,nmind)
     t4dv=real((nmind-iwinbgn),r_kind)*r60inv
     sstime=real(nmind,r_kind)
     tdiff=(sstime-gstime)*r60inv
     if (l4dvar.or.l4densvar) then
        if(t4dv<zero .OR. t4dv>winlen) goto 120
     else
        if(abs(tdiff) > twind) goto 120
     end if

!    extract total ozone
     call ufbint(lunin,totoz,1,1,iret,'OZON')

     if (totoz > badoz ) goto 120

!    only accept flag 0 (good) data
     toq=0._r_double
     call ufbint(lunin,toq,1,1,iret,'GOMEEF')
     if (toq/=0) goto 120

!    only accept scan positions from 2 to 25
     if( hdrozg2(5) < two .or. hdrozg2(5) > 25.0_r_kind ) goto 120

!    thin GOME data
!    GOME data has bias when the satellite looks to the east. Consider QC out this data.

     if (thin4d) then
        timedif = zero 
     else 
        timedif = r6*abs(tdiff)        ! range:  0 to 18
     endif 
     crit1 = 0.01_r_kind+timedif
     call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
     if(.not. iuse) goto 120

     call finalcheck(dist1,crit1,itx,iuse)
     if(.not. iuse) goto 120

     ndata=ndata+1
     nodata=ndata

     ozout(1,itx)=rsat
     ozout(2,itx)=t4dv
     ozout(3,itx)=dlon               ! grid relative longitude
     ozout(4,itx)=dlat               ! grid relative latitude
     ozout(5,itx)=dlon_earth*rad2deg ! earth relative longitude (degrees)
     ozout(6,itx)=dlat_earth*rad2deg ! earth relative latitude (degrees)
     ozout(7,itx)=toq                ! total ozone error flag
     ozout(8,itx)=hdrozg(9)          ! solar zenith angle
     ozout(9,itx)=hdrozg(10)         ! solar azimuth angle
     ozout(10,itx)=hdrozg2(1)        ! CLOUD AMOUNT IN SEGMENT
     ozout(11,itx)=hdrozg2(2)        ! SNOW COVER
     ozout(12,itx)=hdrozg2(3)        ! AEROSOL CONTAMINATION INDEX
     ozout(13,itx)=hdrozg2(4)        ! ASCENDING/DESCENDING ORBIT QUALIFIER
     ozout(14,itx)=hdrozg2(5)        ! scan position (fovn)
     ozout(15,itx)=totoz
       
     goto 120

!    End of GOME bufr block


! Process OMI data
  else if ( obstype == 'omi') then

!    Make thinning grids
     call makegrids(rmesh,ithin)

     nmrecs=0
     open(lunin,file=trim(infile),form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     call readmg(lunin,subset,idate,iret)
     if (subset == 'NC008013') then
        write(6,*)'READ_OZONE:  OMI data type, subset=',subset
     else
        write(6,*)'READ_OZONE:  *** WARNING: unknown ozone data type, subset=',subset
        write(6,*)' infile=',trim(infile), ', lunin=',lunin, ', obstype=',obstype,', jsatid=',jsatid
        goto 170
     endif

!    Set dependent variables and allocate arraysn
     nreal=14
     nloz=0
     nchanl=1
     nozdat=nreal+nchanl
     allocate (ozout(nozdat,itxmax))
     do k=1,itxmax
        do i=1,nozdat
           ozout(i,k)=rmiss
        end do
     end do

     iy=0
     im=0
     idd=0
     ihh=0
     if(iret/=0) goto 160

130  continue
     call readsb(lunin,iret)
     if (iret/=0) then
        call readmg(lunin,subset,jdate,iret)
        if (iret/=0) goto 150
        goto 130
     endif

!    extract header information
     call ufbint(lunin,hdrozo,10,1,iret,ozostr)
     call ufbint(lunin,hdrozo2,8,1,iret,ozostr2)
     rsat = hdrozo(1); ksatid=rsat

     if(jsatid == 'aura')kidsat = 785
     if (ksatid /= kidsat) go to 130


     nmrecs=nmrecs+nloz+1

!    Convert observation location to radians
     slats0= hdrozo(2)
     slons0= hdrozo(3)
     if(abs(slats0)>90._r_kind .or. abs(slons0)>r360) go to 130  
     if(slons0< zero) slons0=slons0+r360
     if(slons0==r360) slons0=zero
     dlat_earth = slats0 * deg2rad
     dlon_earth = slons0 * deg2rad

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if(outside) go to 130
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif

! convert observation time to relative time
     idate5(1) = hdrozo(4)  !year
     idate5(2) = hdrozo(5)  !month
     idate5(3) = hdrozo(6)  !day
     idate5(4) = hdrozo(7)  !hour
     idate5(5) = hdrozo(8)  !minute
     call w3fs21(idate5,nmind)

     t4dv=real((nmind-iwinbgn),r_kind)*r60inv
     sstime=real(nmind,r_kind)
     tdiff=(sstime-gstime)*r60inv
     if (l4dvar.or.l4densvar) then
        if (t4dv<zero .OR. t4dv>winlen) go to 130
     else
        if(abs(tdiff) > twind) go to 130
     end if

!    extract total ozone
     call ufbint(lunin,totoz,1,1,iret,'OZON')
     if (totoz > badoz ) goto 130

!    Bit 10 in TOQF represents row anomaly. 
     decimal=int(hdrozo2(6))
     call dec2bin(decimal,binary,14)
     if (binary(10) == 1 ) then
        goto 130
     endif

!    only accept flag 0 1, flag 2 is high SZA data which is not used for now
     toq=hdrozo2(5)
     if (toq/=0 .and. toq/=1) goto 130

!    remove the bad scan position data: fovn beyond 25
     if (hdrozo2(7) >=25.0_r_double) goto 130

!    remove the data in which the C-pair algorithm ((331 and 360 nm) is used. 
     if (hdrozo2(8) == 3_r_double .or. hdrozo2(8) == 13_r_double) goto 130


!    thin OMI data

     if (thin4d) then
        timedif = zero 
     else 
        timedif = r6*abs(tdiff)        ! range:  0 to 18
     endif 
     crit1 = 0.01_r_kind+timedif
     call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
     if(.not. iuse)go to 130
 
     call finalcheck(dist1,crit1,itx,iuse)
     if(.not. iuse)go to 130
   
     ndata=ndata+1
     nodata=ndata



     ozout(1,itx)=rsat
     ozout(2,itx)=t4dv
     ozout(3,itx)=dlon               ! grid relative longitude
     ozout(4,itx)=dlat               ! grid relative latitude
     ozout(5,itx)=dlon_earth*rad2deg ! earth relative longitude (degrees)
     ozout(6,itx)=dlat_earth*rad2deg ! earth relative latitude (degrees)
     ozout(7,itx)=hdrozo2(5)         ! total ozone quality code
     ozout(8,itx)=hdrozo(10)         ! solar zenith angle
     ozout(9,itx)=binary(10)         ! row anomaly flag
     ozout(10,itx)=hdrozo2(1)        !  cloud amount
     ozout(11,itx)=hdrozo2(4)        !  vzan
     ozout(12,itx)=hdrozo2(2)        !  aerosol index
     ozout(13,itx)=hdrozo2(3)        !  ascending/descending
     ozout(14,itx)=hdrozo2(7)        !  scan position
     ozout(15,itx)=totoz

!    End of loop over observations
     go to 130

! End of OMI block

! Process MLS bufr data
  else if ( index(obstype,'mls')/=0 ) then

     nmrecs=0

     open(lunin,file=trim(infile),form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     call readmg(lunin,subset,idate,iret)
     if (subset == 'NC008015') then
        write(6,*)'READ_OZONE:  MLS data type, subset=',subset
     else
        write(6,*)'READ_OZONE:  *** WARNING: unknown ozone data type, subset=',subset
        write(6,*)' infile=',trim(infile), ', lunin=',lunin, ', obstype=',obstype,', jsatid=',jsatid
        goto 170
     endif

     if(iret/=0) goto 160

180  continue
     call readsb(lunin,iret)
     if (iret/=0) then
        call readmg(lunin,subset,jdate,iret)
        if (iret/=0) goto 150
        goto 180
     endif

!    Get # of vertical pressure levels nloz and MLS NRT data version which depends on nloz
     allocate(hdrmlsl(3,100))
     call ufbrep(lunin,hdrmlsl,3,100,iret,mlstrl)
     nloz=iret
!    for NRT data, mlsv=20 or 30 depending on the nloz
     mlsv=-999
     if(nloz==37) then
        if(index(sis,'mls22')/=0 ) then       !mls v2.2 data
           mlsv=22
        else if(index(sis,'mls20')/=0 ) then  !mls v2 nrt data
           mlsv=20
        end if
     else if (nloz==55) then                !mls v3 nrt data
        if (index(sis,'mls30')/=0 ) then
           mlsv=30
        endif
     else
        write(6,*) 'invalid vertical level number: ', nloz
        write(6,*) '******STOP*******: error reading MLS vertical levels in read_ozone.f90'
        call stop2(338)
     end if
     deallocate(hdrmlsl)

     write(6,*) 'READ_OZONE: MLS data version=',mlsv
     write(6,*) 'READ_OZONE: MLS vertical level number=',nloz

     if (mlsv<0) then
        write(6,*) 'inconsistent MLS versions.  bufr nloz=',nloz,' obsinput sis= ',trim(sis)
        write(6,*) '******STOP*******: error bufr and specified MLS versions'
        call stop2(338)
     end if

!    Allocate arrays
     allocate(hdrmlsl(3,nloz))
     allocate (mlspres(nloz))
     allocate (mlsoz(nloz))
     allocate (mlsozpc(nloz))
     allocate(ipos(nloz))
     allocate (usage1(nloz))
 
!    Set dependent variables and allocate arrays
     nreal=12
     nchanl=1
     nozdat=nreal+nchanl
     allocate (ozout(nozdat,maxobs))

     do k=1,maxobs
        do i=1,nozdat
           ozout(i,k)=rmiss
        end do
     end do

     ikx=0
     k0=0
     ipos=999
     first=.false.
     do k=1,jpch_oz
        if( (.not. first) .and. index(nusis_oz(k),sis)/=0 ) then
           k0=k
           first=.true.
        end if
        if(first .and. index(nusis_oz(k),sis)/=0 ) then
           ikx=ikx+1
           ipos(ikx)=k0+ikx-1
        end if
     end do

!    Reopen unit to bufr file
     call closbf(lunin)
     open(lunin,file=trim(infile),form='unformatted')
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     call readmg(lunin,subset,idate,iret)

140  continue
     call readsb(lunin,iret)
     if (iret/=0) then
        call readmg(lunin,subset,jdate,iret)
        if (iret/=0) goto 150
        goto 140
     endif

     do k=1,nloz
        if (iuse_oz(ipos(k)) < 0) then
           usage1(k) = 100._r_kind
        else
           usage1(k) = zero
        endif
     end do

!    extract header information
     call ufbint(lunin,hdrmls,13,1,iret,mlstr)
     rsat = hdrmls(1); ksatid=rsat

     if(jsatid == 'aura')kidsat = 785
     if (ksatid /= kidsat) go to 140

     nmrecs=nmrecs+nloz

!    Convert observation location to radians
     slats0= hdrmls(2)
     slons0= hdrmls(3)
     if(abs(slats0)>90._r_kind .or. abs(slons0)>r360) go to 140  
     if(slons0< zero) slons0=slons0+r360
     if(slons0==r360) slons0=zero
     dlat_earth = slats0 * deg2rad
     dlon_earth = slons0 * deg2rad

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if(outside) go to 140
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat,rlats,nlat,1)
        call grdcrd1(dlon,rlons,nlon,1)
     endif

! convert observation time to relative time
     idate5(1) = hdrmls(4)  !year
     idate5(2) = hdrmls(5)  !month
     idate5(3) = hdrmls(6)  !day
     idate5(4) = hdrmls(7)  !hour
     idate5(5) = hdrmls(8)  !minute
     call w3fs21(idate5,nmind)

     t4dv=real((nmind-iwinbgn),r_kind)*r60inv
     sstime=real(nmind,r_kind)
     tdiff=(sstime-gstime)*r60inv
     if (l4dvar.or.l4densvar) then
        if (t4dv<zero .OR. t4dv>winlen) go to 140
     else
        if(abs(tdiff) > twind) go to 140
     end if

!    v2.2 data screening, only accept:
!    Pressure range(PRLC):       215-0.02mb (lev5-27)
!    Precision(OZMP):            positive OZMP;    
!    Status flag(MLST):          only use even number
!    Quality(PCCF):              use >1.2 for data at 215-100mb & low latitude, 
!                                use >0.4 for data elsewhere
!    Convergence(CONV):          use <1.8

!    v2 NRT data screening, only accept:
!    Pressure range(PRLC):       68-0.2mb (lev8-23)
!    Precision(OZMP):            positive OZMP;    
!    Status flag(MLST):          only use even number
!    Quality(PCCF):              do NOT use <1.2 or >3.0

!    v3 NRT data screening, only accept:
!    Pressure range(PRLC):       261-0.1mb (lev8-43)
!    Precision(OZMP):            positive OZMP;
!    Status flag(MLST):          only use even number
!    Quality(PCCF):              only use if >0.4 
!    Convergence(CONV):          use <1.2

!    status: Bit 1 in MLST represents data should not be used
!    Note: in BUFR bits are defined from left to right as: 123456789...
!    whereas in HDF5 (and the nasa document) bits are defined from right to left as: ...876543210
     decimal=int(hdrmls(12))
     call dec2bin(decimal,binary_mls,18)
     if (binary_mls(1) == 1 ) goto 140

!    v2.2 data, remove data when convergence>1.8 
!    v3 NRT data,remove data when convergence>1.2 
     if(mlsv==22) then
        if(hdrmls(11) >= 1.8_r_kind) go to 140
     else if(mlsv==30) then
        if(hdrmls(11) >= 1.2_r_kind) go to 140
     end if

!    extract pressure, ozone mixing ratio and precision
     call ufbrep(lunin,hdrmlsl,3,nloz,iret,mlstrl)

     do k=1,nloz
        mlspres(k)=log(hdrmlsl(1,k)*0.001_r_kind)    ! mls pressure in Pa, coverted to log(cb)
        mlsoz(k)=hdrmlsl(2,k)                     ! ozone mixing ratio in ppmv
        mlsozpc(k)=hdrmlsl(3,k)                   ! ozone mixing ratio precision in ppmv
!       there is possibility that mlsoz in bufr is 0 or negative or larger than 100 which are not reasonable values.
        if(mlsoz(k)<1.0e-8_r_kind .or. mlsoz(k)>100.0_r_kind ) then 
          usage1(k)=1000._r_kind
!         for v2.2 data, if this unreasonable value happens between 215mb (lev5) and 0.02mb (lev27), throw the whole profile
!         for v2 NRT data, if this unreasonable value happens between 68mb (lev8) and 0.2mb (lev23), throw the whole profile
!         for v3 NRT data, if this unreasonable value happens between 261mb (lev8) and 0.1mb (lev43), throw the whole profile
          if(mlsv==22 .and. (k<=27 .and. k>=5)) go to 140
          if(mlsv==20 .and. (k<=23 .and. k>=8)) go to 140
          if(mlsv==30 .and. (k<=43 .and. k>=8)) go to 140
        end if
     end do
        
     do k=1,nloz
!       pressure range
        if(mlsv==22) then
          if(hdrmlsl(1,k)>21700._r_kind .or. hdrmlsl(1,k)<1._r_kind) usage1(k)=1000._r_kind
        else if(mlsv==20) then
          if(hdrmlsl(1,k)>6900._r_kind .or. hdrmlsl(1,k)<10._r_kind) usage1(k)=1000._r_kind
        else if(mlsv==30) then
          if(hdrmlsl(1,k)>26500._r_kind .or. hdrmlsl(1,k)<10._r_kind) usage1(k)=1000._r_kind
        end if
!       only positive precision accepted
        if(hdrmlsl(3,k)<=0._r_kind) usage1(k)=1000._r_kind
     end do

!   status screening
     hdrmls13=hdrmls(13)*0.1_r_kind
     if(mlsv==22) then
        if (abs(slats0)<30._r_kind) then
           do k=1,nloz
              if(hdrmlsl(1,k)>10100._r_kind .and. hdrmlsl(1,k)<21700._r_kind) then
                 if(hdrmls13 <= 1.2_r_kind) usage1(k)=1000._r_kind
              else
                 if(hdrmls13 <= 0.4_r_kind) usage1(k)=1000._r_kind
              endif
           end do
        else
           if(hdrmls13 <= 0.4_r_kind) then
              do k=1,nloz
                 usage1(k)=1000._r_kind
              end do
           end if
        end if
     else if(mlsv==20) then
        if(hdrmls13 <= 1.2_r_kind .or. hdrmls13 >= 3.0_r_kind) then
           do k=1,nloz
              usage1(k)=1000._r_kind
           end do
        end if
     else if(mlsv==30) then
        if(hdrmls13 <= 0.4_r_kind) then
           do k=1,nloz
              usage1(k)=1000._r_kind
           end do
        end if
     end if

     do k=1,nloz

        ndata=min(ndata+1,maxobs)
        nodata=ndata
!       if(ndata >= nloz) goto 140

        ozout(1,ndata)=rsat
        ozout(2,ndata)=t4dv
        ozout(3,ndata)=dlon               ! grid relative longitude
        ozout(4,ndata)=dlat               ! grid relative latitude
        ozout(5,ndata)=dlon_earth*rad2deg ! earth relative longitude (degrees)
        ozout(6,ndata)=dlat_earth*rad2deg ! earth relative latitude (degrees)
        ozout(7,ndata)=hdrmls(10)         ! solar zenith angle

        ozout(8,ndata)=usage1(k)          ! 
        ozout(9,ndata)=mlspres(k)         ! mls pressure in log(cb)
        ozout(10,ndata)=mlsozpc(k)        ! ozone mixing ratio precision in ppmv
        ozout(11,ndata)=float(ipos(k))    ! pointer of obs level index in ozinfo.txt
        ozout(12,ndata)=nloz              ! # of mls vertical levels
        ozout(nreal+1,ndata)=mlsoz(k)     ! ozone mixing ratio in ppmv
     end do

     go to 140

!    End of MLS bufr loop

  endif

! Jump here when eof detected
150 continue

! If gome or omi data, compress ozout array to thinned data
  if (obstype=='omi' .or. obstype=='gome') then
     kk=0
     do k=1,itxmax
        if (ozout(1,k)>zero) then
           kk=kk+1
           do i=1,nozdat
              ozout(i,kk)=ozout(i,k)
           end do
        endif
     end do
     ndata=kk
     nodata=ndata
  endif

! Write header record and data to output file for further processing
  call count_obs(ndata,nozdat,ilat,ilon,ozout,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((ozout(k,i),k=1,nozdat),i=1,ndata)
  nread=nmrecs

! Deallocate local arrays
160 continue
  deallocate(ozout)
  if (obstype == 'sbuv2') deallocate(poz)
  if (index(obstype,'mls')/=0) then
     deallocate(hdrmlsl)
     deallocate(mlspres)
     deallocate(mlsoz)
     deallocate(mlsozpc)
     deallocate(ipos)
     deallocate(usage1)
  end if

! Close unit to input data file
170 continue
  call closbf(lunin)
  close(lunin)

! Deallocate satthin arrays
  if (obstype == 'omi' .or. obstype == 'gome')call destroygrids

  return
  
end subroutine read_ozone






SUBROUTINE dec2bin(dec,bin,ndim)
 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dec2bin                  convert decimal number to binary 
!   prgmmr: unknown             org: np23                date: 2010-04-06
!
! abstract:  This routine convert a decimal number to binary
!
! program history log:
!   2010-04-06  hliu

!   input argument list:
!     dec  - observation type to process
!
!   output argument list:
!     bin    - number of sbuv/omi ozone observations read
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!

    use kinds, only: i_kind
    use mpeu_util, only: die, perr

    implicit none
 
! Declare passed variables
    integer(i_kind) ,intent(inout) :: dec
    integer(i_kind) ,intent(in)    :: ndim
    integer(i_kind) ,intent(out)   :: bin(ndim)

! Declare local variables
    integer(i_kind):: bindec, i

!   Check to determine decimal # is within bounds
    i = ndim
    IF ((dec - 2**i) >= 0) THEN
       write(6,*) 'Decimal Number too Large. Must be < 2^(',ndim-1,')'
       call die('dec2bin')
    END IF

!   Determine the scalar for each of the decimal positions
    DO WHILE (i >= 1)
       bindec = 2**(i-1)
       IF ((dec - bindec) >= 0) THEN
          bin(i) = 1
          dec = dec - bindec
       ELSE
          bin(i) = 0
       END IF
       i = i - 1
    END DO

    RETURN
END subroutine dec2bin
