!-----------------------------------------------------------------------
      PROGRAM CHGRES
!C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: GLOBAL_CHGRES
!   PRGMMR: IREDELL          ORG: NP23        DATE: 1999-09-10
!
! ABSTRACT: THIS PROGRAM CHANGES THE RESOLUTION OF THE SIGMA, SURFACE
!   AND NSST RESTART FILES FROM THE GLOBAL SPECTRAL MODEL.  THE INPUT FILES
!   SHOULD HAVE HEADER RECORDS IDENTIFYING THEIR RESPECTIVE RESOLUTIONS.
!   THE OUTPUT FILES RESOLUTION ARE SPECIFIED IN THE NAMELIST NAMCHG.
!   EITHER THE INPUT SIGMA OR SURFACE FILE MAY BE MISSING, IN WHICH
!   CASE NO COUNTERPART FILE IS CREATED WITH THE NEW RESOLUTION.
!
!   THE PROCEDURE FOR CHANGING THE SIGMA FILE RESOLUTION IS THUS.
!   A NEW OROGRAPHY IS OPTIONALLY READ IN.  IF IT IS MISSING,
!   THE NEW OROGRAPHY WILL BE THE TRANSFORM OF THE OLD OROGRAPHY.
!   A NEW SIGMA STRUCTURE IS ALSO READ IN.  THIS FILE IS ONLY
!   OPTIONAL IF THE NUMBER OF LEVELS ARE THE SAME, IN WHICH CASE
!   THE NEW SIGMA STRUCTURE DEFAULTS TO THE OLD SIGMA STRUCTURE.
!   THEN, THE INPUT SPECTRAL FIELDS ARE READ IN AND TRANSFORMED
!   TO THE NEW GAUSSIAN GRID.  A NEW SURFACE PRESSURE IS CALCULATED
!   HYDROSTATICALLY BASED ON THE NEW OROGRAPHY.  THEN THE UPPER-AIR
!   FIELDS ARE VERTICALLY INTERPOLATED TO THE INFERRED NEW PRESSURES.
!   THE VERTICAL INTERPOLATION IS GENERALLY CUBIC LAGRANGIAN IN LOG
!   PRESSURE WITH A MONOTONIC CONDITION THAT A NEW VALUE CANNOT EXCEED
!   THE RANGE OF ITS IMMEDIATE OLD NEIGHBORS.  INTERPOLATION IS LINEAR
!   BETWEEN THE TWO OUTER INTERVALS OF THE OLD DOMAIN.  FIELDS ARE HELD
!   CONSTANT OUTSIDE THE OLD DOMAIN, EXCEPT FOR TEMPERATURE AND HUMIDITY
!   BELOW THE OLD DOMAIN, WHERE THE TEMPERATURE LAPSE RATE IS HELD
!   FIXED AT -6.5 K/KM AND THE RELATIVE HUMIDITY IS ALSO HELD FIXED.
!   FINALLY, ALL FIELDS ARE TRANSFORMED TO THE NEW SPECTRAL SPACE
!   AND WRITTEN OUT.  NOTE THAT ALL TRACERS ARE INTERPOLATED UNLESS
!   REQUESTED OTHERWISE.  ALTERNATIVELY, IF NO TRANSFORMS ARE NEEDED,
!   THEN NO NEW OROGRAPHY OR SIGMA STRUCTURE IS READ IN AND THE
!   SPECTRAL COEFFICIENTS ARE DIRECTLY PADDED OR TRUNCATED.
!   FURTHERMORE, IF OZONE IS REQUESTED IN THE OUTPUT FILE
!   BUT IS NOT IN THE INPUT FILE, THEN OZONE IS GENERATED
!   FROM CLIMATOLOGY AND, OPTIONALLY, A TOTAL OZONE GRIB FIELD.
!   THE LAST RECORD PRECIPITATION IS ALSO INTERPOLATED IF REQUESTED.
!
!   THE PROCEDURE FOR CHANGING THE SURFACE FILE RESOLUTION IS THUS:
!   NEAREST NEIGHBOR INTERPOLATION IS PERFORMED SO THAT LAND/NONLAND
!   POINTS ON THE INPUT GRID ARE MAPPED TO LAND/NONLAND POINTS
!   ON THE TARGET GRID.  IF THE INPUT FILE CONTAINS LANDICE
!   AND THE OUTPUT GRID IS TO HAVE LANDICE, THEN NONLAND IS
!   MAPPED TO NONLAND, LANDICE IS MAPPED TO LANDICE, ICE FREE
!   LAND IS MAPPED TO ICE FREE LAND. OPTIONALLY, THE CLIMO FIELDS
!   SUCH AS ALBEDO, ROUGHNESS, ETC, MAY DETERMINED ON THE OUTPUT 
!   GRID FROM SFCCYCLE (WHICH IS CALLED FROM THE SURFACE
!   CHGRES MODULE).  THE LATTER IS RECOMMENDED WHEN CONVERTING
!   FROM A LOW TO HIGH RESOLUTION GRID.  A NEW LAND-SEA MASK IS
!   OPTIONALLY READ IN. IF IT IS MISSING, THE NEW LAND-SEA MASK IS
!   INTERPOLATED FROM THE OLD MASK.  SKIN AND SOIL TEMPERATURE OVER
!   LAND ARE ADJUSTED FOR DIFFERENCES BETWEEN THE INPUT AND OUTPUT
!   OROGRAPHY. LIQ SOIL MOISTURE IS CALCULATED ACCORDING TO THE
!   ADJUSTED TEMP. OUTPUT OROGRAPHY MAY BE READ IN FROM FILE OR INTERPOLATED
!   FROM INPUT OROGRAPHY.  NOTE: OLDER VERSIONS OF THE SURFACE
!   RESTART FILE (BEFORE IVS 200501) DO NOT HAVE OROGRAPHY RECORDS.
!   IN CASES WHERE THE INPUT SURFACE FILE IS PRE 200501,
!   THE PROGRAM WILL GET THE OROGRAPHY FROM THE SIGMA FILE.  
!   THEREFORE, YOU MUST SET THE OPTIONS TO CONVERT A SIGMA FILE
!   AS WELL AS A SURFACE FILE.  WHEN CHANGING A PRE 200501 FILE,
!   THE PROGRAM WILL INTERPOLATE ONLY THOSE LAND FIELDS NEEDED 
!   TO RUN THE OLD OSU LAND MODEL AND OLD SEA ICE PHYSICS.
!   WHEN CHANGING A 200501 FILE, THE PROGRAM WILL INTERPOLATE/CALC
!   THOSE ADDITIONAL FIELDS NEEDED BY THE NOAH LSM (MAX SNOW ALB,
!   LIQ. SOIL MOIST, SNOW DEPTH, PRECIP, PRECIP TYPE, SLOPE TYPE,
!   MAX/MIN GREENNESS) AND THE NEW SEA ICE MODEL (ICE DEPTH AND
!   FRACTION).  WHEN CHANGING A PRE 200501 FILE TO A 200501 FILE,
!   THE PROGRAM WILL AUTOMATICALLY INITIALIZE THE ABOVE 
!   MENTIONED FIELDS USING EITHER GUESS VALUES OR VALUES
!   CALCULATED FROM SFCCYCLE.  THE PROGRAM WILL ALSO CONVERT FROM TWO 
!   TO FOUR SOIL LAYERS AND VICE VERSA.  THE PROGRAM WILL RUN
!   ON THE FULL OR REDUCED GRID DEPENDING ON THE LONSPERLAT
!   RECORD OF THE INPUT FILE OR WHETHER THE USER SPECIFIES
!   AN EXTERNAL LONSPERLAT FILE.  THE PROGRAM WILL INITIALIZE
!   ALL LAND STATES FOR THE LANDICE PHYSICS IF DESIRED.  THE PROGRAM
!   WILL SCALE TOTAL SOIL MOISTURE FOR ANY DIFFERENCES IN SOIL
!   TYPE BETWEEN THE INPUT AND OUTPUT GRIDS.  CONTACT G. GAYNO
!   WITH QUESTIONS.
!
!   THE PROCEDURE FOR CONVERTING AN NSST FILE IS THUS:
!   NEAREST NEIGHBOR INTERPOLATION IS USED TO MAP TO THE OUTPUT
!   GRID AS SOME NSST FIELDS ARE NOT CONTINUOUS.  NSST FIELDS
!   ARE ONLY REQUIRED AT OPEN WATER POINTS.  FOR CONSISTENCY,
!   THESE OPEN WATER POINTS ARE DETERMINED BY THE LAND-SEA
!   MASK FROM THE SURFACE RESTART FILE.  THEREFORE, WHEN 
!   CONVERTING AN NSST FILE, A SURFACE RESTART FILE MUST ALSO
!   BE CONVERTED.  
!  
! PROGRAM HISTORY LOG:
!   98-04-03  IREDELL
!   2007       Juang, Moorthi, Gayno, F Yang, Doris Pan
!   2008       Moorthi S
!   2011-08-05 Gayno G   : Added capability to work with nsst files
!   2011       J. Wang   : change header for sigio to nemsio file
!   2013       Sarah Lu  : revise NEWSIG (copied from global_chgres.fd)
!   2014       Moorthi S : merged trunk version with ngac version (nemsio)
!   2015       Moorthi S : added option to use binary orography
!   2015       Iredell M : added option to write nemsio on reduced grid      
!   2015       Fanglin Y : added the logical rdgrid
!   2015-10-06 Moorthi S : added option to read past ics from Tom Hamill for GEFS      
!   2015-12-06 Moorthi S : added option to read nemsio file without p, dp, vel (nopdpvv)
!   2016-04-22 Moorthi S : merge with Jim Ables's modifications adding latitudinal
!                          decomposition to enable changing resolution from a nemsio
!                          file to a nemsio file.  Upgrade the code to work for 
!                          vertical resolution change and fix some additional bugs.
!                          added nopdpvv option not to output p, dp and vvel
!   2016-04-27 Moorthi S : fixed a bug for nopdpvv=.false.
!   2016-06-01 Gayno G   : option to process nsst files in nemsio format
!
! NAMELISTS:
!   NAMCHG:
!     JCAP       INTEGER NEW SPECTRAL RESOLUTION (DEFAULT: NO CHANGE)
!     LEVS       INTEGER NEW NUMBER OF LEVELS (DEFAULT: NO CHANGE)
!     NTRAC      INTEGER NEW NUMBER OF TRACERS (DEFAULT: NO CHANGE)
!     LONB       INTEGER NEW NUMBER OF PHYSICS LONGITUDES
!                (DEFAULT: INFERRED)
!     LATB       INTEGER NEW NUMBER OF PHYSICS LATITUDES
!                (DEFAULT: INFERRED)
!     LONBI      INTEGER OLD NUMBER OF PHYSICS LONGITUDES
!                (DEFAULT: INFERRED FROM INPUT SURFACE OR SIGMA FILE)
!     LATBI      INTEGER OLD NUMBER OF PHYSICS LATITUDES
!                (DEFAULT: INFERRED FROM INPUT SURFACE OR SIGMA FILE)
!     IDVC       INTEGER NEW VERTICAL COORDINATE ID
!                (1 FOR SIGMA, 2 FOR HYBRID, 3 GENERALL HYBRID) (DEFAULT: NO CHANGE)
!     IDVM       INTEGER NEW VERTICAL MASS VARIABLE ID
!                (1 FOR LN(PS) AND 2 FOR PS (KPA)) (DEFAULT: NO CHANGE)
!     IDSL       INTEGER NEW TYPE OF SIGMA STRUCTURE
!                (1 FOR PHILLIPS OR 2 FOR MEAN) (DEFAULT: NO CHANGE)
!     IGEN       INTEGER NEW GENERATING CODE (DEFAULT: NO CHANGE)
!     MGG        INTEGER NUMBER OF PRECIP FIELDS TO COPY (DEFAULT: 0)
!     MQUICK     INTEGER FLAG TO SKIP TRANSFORMS (DEFAULT: 0)
!     IDVT       INTEGER NEW TRACER VARIABLE ID (DEFAULT: NO CHANGE)
!     NCLDT      INTEGER NEW NUMBER OF CLOUDS (DEFAULT: NO CHANGE)
!     LATCH      INTEGER NUMBER OF LATITUDES TO PROCESS AT ONE TIME
!                (DEFAULT: 8)
!     LSOIL      INTEGER NEW NUMBER OF SOIL LAYERS (DEFAULT: NO CHANGE)
!     IVSSFC     INTEGER NEW VERSION NUMBER SFC FILE (DEFAULT: NO CHANGE)
!     IVSSIG     INTEGER NEW VERSION NUMBER SIGMA FIILE (DEFAULT: NO CHANGE)
!     NVCOORD    INTEGER NEW NUMBER OF VERTICAL COORDINATES
!                (DEFAULT: NO CHANGE)
!     IDRT       NEMSIO OUTPUT OPTION: 4--GAUSSIAN (default); 0-- LATLON
!     OUTTYP     INTEGER NUMBER OF OUTPUT TYPE: 
!                1 NEMSIO GRID; 2 SIGIO SPECTRAL (default); 0 BOTH SIGIO & NEMSIO
!     CHGQ0      SET NEGATIVE VALUES OF TRACERS TO ZERO in CHGRES, 0 FALSE; 1 TRUE.
!
! INPUT FILES:
!   UNIT   11    chgres.inp.sig        OLD SIGMA FILE (IN EITHER SIGIO OR NEMSIO FORMAT)
!   UNIT   12    chgres.inp.orogb      NEW GRIB OROGRAPHY
!   UNIT   13    chgres.inp.siglevel   NEW VERTICAL STRUCTURE
!   UNIT   14    chgres.inp.o3clim     OZONE CLIMATOLOGY
!   UNIT   15    chgres.inp.o3tgb      NEW GRIB TOTAL OZONE
!   UNIT   16    chgres.inp.orogb_uf   NEW GRIB OROGRAPHY (unfiltered)
!   UNIT   21    chgres.inp.sfc        OLD SURFACE FILE
!   UNIT   22    chgres.inp.slmgb      NEW GRIB LAND-SEA MASK
!   UNIT   23    chgres.inp.lonsperlat NEW NUMBER OF LONS PER LAT
!   UNIT   31    chgres.inp.nst        NSST FILE FOR INPUT GRID (NSTIO OR NEMSIO FORMAT)
!
! OUTPUT FILES:
!   UNIT   51    chgres.out.sig        NEW HISTROY FILE IN SPECTRAL COEFFICIENTS (SIGIO)
!   UNIT         chgres.out.grd        NEW HISTROY FILE IN GAUSSIAN GRID (NEMSIO)
!   UNIT   61    chgres.out.sfc        NEW SURFACE FILE
!   UNIT         chgres.out.sfn        NEW SURFACE FILE (NEMSIO)
!   UNIT   32    chgres.out.nst        NSST FILE FOR OUTPUT GRID (NSTIO FORMAT)
!   UNIT         chgres.out.nsn        NSST FILE FOR OUTPUT GRID (NEMSIO FORMAT)
!
! SUBPROGRAMS CALLED:
!   NCPUS        GET ENVIRONMENT NUMBER OF PARALLEL PROCESSES
!   RDSGH        READ A SIGMA FILE HEADER
!   CHGRES1      CHANGE RESOLUTION
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!
! REMARKS:
!  VALID VALUES OF IDVT
!  ALL UNITS ARE SPECIFIC IN KG/KG
!  IDVT  NTRAC 
!     0      2    VAPOR,OZONE,CLOUD (DEFAULT OPERATIONAL)
!     1      1    VAPOR,OZONE
!     2      1    VAPOR,CLOUD
!    21      2    VAPOR,OZONE,CLOUD
!    12      2    VAPOR,CLOUD,OZONE
!   100     20    SET 1: VAPOR,OZONE,CLOUD, AND INITIAL VALUES OF
!                 CLAT*CLON,CLAT*SLON,SLAT,
!                 V*SLON-U*SLAT*CLON,-V*CLON-U*SLAT*SLON,U*CLAT
!                 ONE,K,SIGMA,PS,PRES,TEMP,ENTROPY,MOIST ENTROPY
!                 VAPOR,OZONE,CLOUD
!   200       5   IDEA tracer set with O and O2 in addition q, O3, clw
!
! NUMBER OF TRACERS FOR NEMSIO FILE:
!-- data holder q(:,:,ntraco_q)
!--    ntraco_q=NTRAC_MET+NTRACO_AER
!-- nemsio data header recname/reclevtyp/reclev,  NTRAC=NTRAC_MET+NTRAC_AER
!--    NTRAC_MET: meteorology tracerss, could be: spfh,ozn,cld
!--    NTRAC_AER: aerosol tracer
!--
!C$$$
      USE SIGIO_MODULE
      USE NSTIO_MODULE
      USE NEMSIO_MODULE
      USE NEMSIO_GFS
      USE SFCIO_MODULE
      USE FUNCPHYS
      USE SURFACE_CHGRES
      USE READ_WRITE_UTILS, ONLY   : INTERPRED,
     &                               UNINTERPRED
      IMPLICIT NONE
      INTEGER:: JCAP=0,LEVS=0,NTRAC=0,LONB=0,LATB=0,LONBI=0,LATBI=0,
     &          IDVC=0,IDVM=0,IDSL=0,IGEN=0,MGG=0,MQUICK=0,IDVT=0,
     &          NCLDT=0,LATCH=8,LSOIL=0,IVSSFC=0,IVSSIG=0,NVCOORD=0,
     &          IDRT=4,OUTTYP=2,IALB=0,CHGQ0=0,ISOT=0,IVEGSRC=0
      CHARACTER*4 :: GRDFMT='grib'
!
      real, parameter :: pifac=180/ACOS(-1.0)
!
      REAL RI(0:20),CPI(0:20)
      LOGICAL use_ufo, nst_anl, grb_oro, rdgrid, nopdpvv
      NAMELIST/NAMCHG/ JCAP,LEVS,NTRAC,LONB,LATB,LONBI,LATBI,
     &                 IDVC,IDVM,IDSL,IGEN,MGG,MQUICK,IDVT,NCLDT,LATCH,
     &                 LSOIL,IVSSFC,IVSSIG,NVCOORD,OUTTYP,IDRT,RI,CPI,
     &                 IALB,CHGQ0,use_ufo,nst_anl,GRDFMT,grb_oro,rdgrid
     &,                nopdpvv,ISOT,IVEGSRC
!
      INTEGER NSIGI,NORO,NSIL,NO3C,NO3T,NSIGO,noro_uf
     &,       IRET,IOSORO,IOSSIL,ITER,IRET0,IRET1,IOSORO_uf
     &,       NCI,NCO,IMI,JMI,IMO,JMO,IJX,NTRACM,J1,J2,JL,IJL,J,JN,JS,N
     &,       NTRACO, II, ijmo, LSI,LSO, latch2,KF,K, IOLPL,LATG2,IOSLM
     &,       NSFCI,NSLM,NLPL,NSFCO,NSSTI,INPTYP, kall
     &,       sfcpress_id_i, thermodyn_id_i
     &,       sfcpress_id_o, thermodyn_id_o, VVEL_PRECISION
     &,       NREC, LEVSI, LEVSO, JREC, FIELDSIZE,NREC_SFC,i,l

      INTEGER                 :: NSST_YEAR, NSST_MON
      INTEGER                 :: NSST_DAY, NSST_HOUR
      INTEGER, dimension(200) :: KGDS_INPUT, KGDS_OUTPUT
     &,                          JPDS,JGDS,KPDS,KGDS
      REAL, ALLOCATABLE       :: MASK_OUTPUT(:),  DUMMY(:), DUMMY2(:,:)
     &,                          RLATS_OUTPUT(:), RLONS_OUTPUT(:)
     &,                          NSST_INPUT(:,:,:),MASK_INPUT(:,:)
     &,                          NSST_OUTPUT_THIN(:,:)
      integer, parameter      :: num_nsst_fields=18
      TYPE(SFC2D) SFCINPUT
      TYPE(SFC1D) SFCOUTPUT
      TYPE(NSTIO_HEAD)        :: NSST_IN_HEAD
      TYPE(NSTIO_DATA)        :: NSST_IN_DATA
      INTEGER, ALLOCATABLE    :: KMSK(:,:)
      INTEGER IDAT(8),JDAT(8)
      REAL RINC(5), NSST_FHOUR
      LOGICAL*1,ALLOCATABLE   :: BITMAP(:)
      LOGICAL, PARAMETER      :: MERGE=.FALSE.
      LOGICAL                 :: DO_NSST
      REAL,ALLOCATABLE        :: SLAT(:),WLAT(:),CLAT(:),RLAT(:)
      REAL,ALLOCATABLE        :: HSI(:),OROGI(:,:),OROGO(:,:),
     &                           orogo_uf(:,:), orogo_uf2(:,:)
      REAL,ALLOCATABLE        :: ZSI(:,:),    PSI(:,:), PI(:,:,:),
     &                           TI(:,:,:),   UI(:,:,:),VI(:,:,:),
     &                           QI(:,:,:,:), WI(:,:,:), TIV(:,:,:),
     &                           XCP(:,:,:),VIRT(:,:,:),SUMQ(:,:,:)
      REAL,ALLOCATABLE        :: ZSO(:,:),  PSO(:,:), PO(:,:,:)
     &,                          TO(:,:,:), UO(:,:,:),VO(:,:,:)
     &,                          QO(:,:,:,:), WO(:,:,:), TPO(:,:)
     &,                          DPDTO(:,:),DTDPO(:,:,:),DPO(:,:,:)
!    &,                   PKI(:,:),TKI(:,:),UKI(:,:),VKI(:,:),QKI(:,:,:)
!    &,                   PKO(:,:),TKO(:,:),UKO(:,:),VKO(:,:),QKO(:,:,:)
      TYPE(SIGIO_HEAD)        :: SIGHEADI,SIGHEADO
      TYPE(SIGIO_DBTA)        :: SIGDATAI,SIGDATAO
      TYPE(SFCIO_HEAD)        :: SFCHEADI,SFCHEADO
      TYPE(SFCIO_DBTA)        :: SFCDATAI,SFCDATAO
      REAL,ALLOCATABLE        :: SLMSKI(:,:),SLMSKO(:,:)
      real ttot,tmin,tmax,fcsthour,timeomega,timestart,timeend
      real (kind=8) timef            
      integer                 :: iolpl3,nlpl3
      integer,allocatable     :: lpl3(:)
!cggg
      real, allocatable       :: ak(:), bk(:), ck(:), vcoord(:,:),
     &                           vcoordi(:,:), vcoord_new(:,:)
      real(nemsio_realkind), allocatable :: tmp(:)

! Define variables for NEMSIO
      TYPE(NEMSIO_GFILE) :: GFILEI,GFILEISFC
      TYPE(NEMSIO_GFILE) :: GFILEO,GFILEOSFC
      TYPE(NEMSIO_HEAD)  :: GFSHEADI, GFSHEADO
      TYPE(NEMSIO_HEADV) :: GFSHEADVI, GFSHEADVO
      TYPE(NEMSIO_DBTA)  :: GFSDATAI
      TYPE(NEMSIO_DBTA)  :: GFSDATAO
      CHARACTER(8)       :: FILETYPE,modelname
!
! Define local vars:
      INTEGER JCAPO,LONBO,LATBO,IDVCO,IDVMO,IDSLO,IGENO,IDVTO,NCLDTO
     &,       NVCOORDO,IVSSIGO,IDATE4O(4),ICEN2O,ITRUNO,IORDERO,IENSO(2)
     &,       IREALFO,IDUSRO,IXGRO,IDRUNO,IDPPO,LSOILO,IVSO,tlmeta
     &,       I_OZN,I_CLD
      CHARACTER(16),allocatable :: TRAC_NAME(:)
      INTEGER,ALLOCATABLE       :: LPLO(:), LPL_OUTPUT(:)
      REAL(4),ALLOCATABLE       :: ZSOILO(:), oro4(:,:)
      REAL(4) FHOURO,PDRYINIO
      LOGICAL OUTSIG, vert_interp_only

      REAL(8) mysum
      real                      :: rlat1, rlat2
      
! .....................................................................
! .....................................................................
      rlat1   = 90000.0
      rlat2   =-90000.0
      ri      = 0.0
      cpi     = 0.0
      use_ufo = .false.
      nst_anl = .false.
      grb_oro = .true.
      rdgrid  = .false.
      nopdpvv = .false.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ NAMELIST
      call instrument(30,kall,ttot,tmin,tmax)
      CALL W3TAGB('GLOBAL_CHGRES',1999,0253,0056,'NP23')
      CALL GFUNCPHYS
      READ(*,NAMCHG)

      write(6,namchg)
!
      latch2 = latch + latch
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OPEN  FILES
      NSIGI    = 11
      NORO     = 12
      NSIL     = 13
      NO3C     = 14
      NO3T     = 15
      noro_uf  = 16
      nlpl3    = 17
      NSIGO    = 51
      INPTYP   = 0

      call nemsio_init(iret)

      CALL SIGIO_SROPEN(NSIGI,'chgres.inp.sig',IRET)
      CALL SIGIO_SRHEAD(NSIGI,SIGHEADI,IRET1)
      IF(IRET == 0 .AND. IRET1 == 0) THEN
        INPTYP = 2
      ELSE
        CALL NEMSIO_OPEN(GFILEI,'chgres.inp.sig','read',IRET=IRET)
        CALL NEMSIO_GETFILEHEAD(GFILEI,GTYPE=FILETYPE,
     &       MODELNAME=MODELNAME)
        print *,'open chgres.inp.sig, iret=',iret, 'gtype=',filetype
        IF (trim(FILETYPE) == 'NEMSIO' .AND. trim(MODELNAME) == 'GFS'
     &    .and. IRET == 0) INPTYP = 1
      ENDIF

      IF(INPTYP /= 0) THEN
        if (grb_oro) then
          CALL BAOPENR(NORO,'chgres.inp.orogb',IRET)
        else
          open(noro, file='chgres.inp.orogb', form='unformatted'
     &,                        status='old', iostat=iret)
        endif
        IF(IRET /= 0) NORO = 0
        OPEN(NSIL,FILE='chgres.inp.siglevel',
     &       FORM='FORMATTED',STATUS='OLD',IOSTAT=IRET)
        IF(IRET /= 0) NSIL = 0
        OPEN(NO3C,FILE='chgres.inp.o3clim',
     &       FORM='FORMATTED',STATUS='OLD',IOSTAT=IRET)
        IF(IRET /= 0) NO3C = 0
        CALL BAOPENR(NO3T,'chgres.inp.o3tgb',IRET)
        IF(IRET /= 0) NO3T = 0
        CALL SIGIO_SWOPEN(NSIGO,'chgres.out.sig',IRET)
      ELSE
       PRINT*,'--- FAIL TO OPEN chgres.inp.sig ---'
       PRINT*,'--- PROCEED TO CHANGE chgres.inp.sfc ---'
       NSIGO = 0
      ENDIF

      call instrument(1,kall,ttot,tmin,tmax)

! ---------------------------------------------------------------------
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!  CHANGE RESOLUTION OF INPUT SIGIO SIGMA FILE
!  OUTPUT HISTORY FILE IN SIGIO SIGMA OR GFSIO/NEMSIO GRID FORMAT, OR BOTH

      IF(INPTYP == 2) THEN
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
! ---------------------------------------------------------------------

        PRINT*, 'CHGRES INPUT:  SPECTRAL SIGIO SIGMA FILE '
        IF(OUTTYP == 1 )PRINT*, 'CHGRES OUTPUT: NEMSIO GRID FILE'
        IF(OUTTYP == 2 )PRINT*, 'CHGRES OUTPUT: SIGIO SIGMA FILE'
        IF(OUTTYP == 0 )PRINT*, 
     &       'CHGRES OUTPUT: BOTH SIGIO SIGMA and NEMSIO GRID FILES'

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OPEN SIGIO SIGMA FILE HEADER
!!      CALL SIGIO_SRHEAD(NSIGI,SIGHEADI,IRET)

        IF(LONBI > 0 .AND. LATBI > 0) THEN
          SIGHEADI%LONB = LONBI
          SIGHEADI%LATB = LATBI
        ENDIF
        LEVSI = SIGHEADI%LEVS
        IMI   = SIGHEADI%LONB
        JMI   = SIGHEADI%LATB

        if (cpi(0) == 0.0) then
          if (mod(SIGHEADI%IDVM/10,10) == 3) then
            do n=1,sigheadi%ntrac+1
              cpi(n-1) = SIGHEADI%cpi(n)
              ri(n-1)  = SIGHEADI%ri(n)
            enddo
          endif
        endif

        JCAPO = SIGHEADI%JCAP
        IF(JCAP > 0) JCAPO = JCAP
        LEVSO = SIGHEADI%LEVS
        IF(LEVS > 0) LEVSO = LEVS
        LONBO = SIGHEADI%LONB
        LATBO = SIGHEADI%LATB
        IF(LONB > 0 .AND. LATB > 0) THEN
          LONBO = LONB
          LATBO = LATB
        ENDIF
        NTRACO = SIGHEADI%NTRAC
        IF(NTRAC > 0) NTRACO = NTRAC
        IF (IDVT == 200) THEN
          NTRACO = MAX(SIGHEADI%NTRAC+2, NTRACO)
          IF (NTRAC > 0 .AND. NTRACO > NTRAC) THEN
            print *,' Incompatible values specified for NTRAC & IDVT'
            stop 1111
          ENDIF
        ENDIF
        ALLOCATE(TRAC_NAME(NTRACO))
        TRAC_NAME(1) = 'spfh'
        if (ntraco == 2) then
          IF(IDVT  == 1) THEN
            TRAC_NAME(2) = 'o3mr'
            I_OZN = 2 ; I_CLD = 0
          ELSEIF(IDVT == 2) THEN
            TRAC_NAME(2) = 'clwmr'
            I_OZN = 0 ; I_CLD = 2
          ENDIF
        elseif (ntraco == 3) then
          IF(IDVT == 0) THEN
            TRAC_NAME(2) = 'o3mr'
            TRAC_NAME(3) = 'clwmr'
            I_OZN = 2 ; I_CLD = 0
          ELSEIF(IDVT == 21) THEN
            TRAC_NAME(2) = 'o3mr' ;  TRAC_NAME(3) = 'clwmr'
            I_OZN = 2 ; I_CLD = 3
          ELSEIF(IDVT == 12) THEN
            TRAC_NAME(2) = 'clwmr' ; TRAC_NAME(3) = 'o3mr'
            I_OZN = 3 ; I_CLD = 2
          endif
        elseif (ntraco == 4) then
          TRAC_NAME(4) = 'tke'
          IF(IDVT == 0) THEN
            TRAC_NAME(2) = 'o3mr'
            TRAC_NAME(3) = 'clwmr'
            I_OZN = 2 ; I_CLD = 0
          ELSEIF(IDVT == 21) THEN
            TRAC_NAME(2) = 'o3mr' ;  TRAC_NAME(3) = 'clwmr'
            I_OZN = 2 ; I_CLD = 3
          ELSEIF(IDVT == 12) THEN
            TRAC_NAME(2) = 'clwmr' ; TRAC_NAME(3) = 'o3mr'
            I_OZN = 3 ; I_CLD = 2
          endif
        ELSEIF(IDVT == 100) THEN
          TRAC_NAME(2) = 'clwmr'   ; TRAC_NAME(3) = 'o3mr'
          I_OZN = 2 ; I_CLD = 3
        ELSEIF(IDVT == 200) THEN                             ! for WAM
          TRAC_NAME(2) = 'clwmr'   ; TRAC_NAME(3) = 'o3mr'
          TRAC_NAME(4) = 'o'       ; TRAC_NAME(5) = 'o2'
          I_OZN = 2 ; I_CLD = 3
        ENDIF

        IDVCO    = SIGHEADI%IDVC
        IDVMO    = SIGHEADI%IDVM
        IDSLO    = SIGHEADI%IDSL
        IGENO    = SIGHEADI%IGEN
        IDVTO    = SIGHEADI%IDVT
        NCLDTO   = SIGHEADI%NCLDT
        IVSSIGO  = SIGHEADI%IVS
        NVCOORDO = SIGHEADI%NVCOORD

        if (NVCOORDO >= 3 ) then
          if (sigheadi%vcoord(1,NVCOORDO) < 0.0) then
              sigheadi%vcoord(:,NVCOORDO) = 0.0
              sigheadi%nvcoord = 2
          endif
        endif

        IF(IDVC   >  0) IDVCO    = IDVC
        IF(IDVM   >= 0) IDVMO    = IDVM
        IF(IDSL   >= 0) IDSLO    = IDSL
        IF(IGEN   >  0) IGENO    = IGEN
        IF(IDVT   >  0) IDVTO    = IDVT
        IF(NCLDT  >  0) NCLDTO   = NCLDT
        IF(IVSSIG >  0) IVSSIGO  = IVSSIG
        IF(IDVC == 1)   NVCOORDO = 1
        IF(NVCOORD > 0) NVCOORDO = NVCOORD

        FHOURO = SIGHEADI%FHOUR
        IDATE4O(1:4) = SIGHEADI%IDATE(1:4)

        ITRUNO   = SIGHEADI%ITRUN
        IORDERO  = SIGHEADI%IORDER
        IREALFO  = SIGHEADI%IREALF
        ICEN2O   = SIGHEADI%ICEN2
        IDPPO    = SIGHEADI%IDPP
        IDRUNO   = SIGHEADI%IDRUN
        IDUSRO   = SIGHEADI%IDUSR
        IXGRO    = SIGHEADI%IXGR
        IENSO    = SIGHEADI%IENS
!
        PDRYINIO = SIGHEADI%PDRYINI
        if (jcapo /= sigheadi%jcap) PDRYINIO = 0
        IF(MQUICK == 1) THEN
          IF(LEVSO /= SIGHEADI%LEVS) CALL ERREXIT(24)
          IF(JCAPO < SIGHEADI%JCAP)  CALL ERREXIT(24)
          IF(OUTTYP /= 2)            CALL ERREXIT(24)
        ENDIF

        PRINT '("CHANGE SIGMA FILE RESOLUTION FROM ",
     &   I4," WAVES, ",I4," LEVELS, AND ",I4," TRACERS")',
     &   SIGHEADI%JCAP,SIGHEADI%LEVS,SIGHEADI%NTRAC
        PRINT '("                                  ",
     &   " IVS=",I6," IDVC=",I2," NVCOORD=",I2,"  IDVM=",I2)',
     &   SIGHEADI%IVS,SIGHEADI%IDVC,SIGHEADI%NVCOORD,SIGHEADI%IDVM
        PRINT '("                               TO ",
     &   I4," WAVES, ",I4," LEVELS, AND ",I4," TRACERS.")',
     &   JCAPO,LEVSO,NTRACO
        PRINT '("                                  ",
     &   " IVS=",I6," IDVC=",I2," NVCOORD=",I2,"  IDVM=",I2)',
     &   IVSSIGO,IDVCO,NVCOORDO,IDVMO

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  read in 2d lonsperlat
        allocate(lpl3(latbo))
        lpl3  = lonbo                      ! full grid as default
        latg2 = 0
        if(rdgrid) then
          open(nlpl3,file='chgres.inp.lpl3',
     &         form='formatted',status='old',iostat=iolpl3)
          if(iolpl3 == 0) then
            read(nlpl3,*,iostat=iolpl3)
     &           latg2,lpl3(1:min(latg2,(latbo+1)/2))
            print *,'lpl3 read: expected ',(latbo+1)/2,', read ',latg2
            if(iolpl3 == 0 .and. latg2 == (latbo+1)/2) then
              do j=1,latbo/2
                lpl3(latbo+1-j) = lpl3(j)
              enddo
              print '(" lpl3 read in")'
            endif
          endif
        endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ OLD SIGMA FILE
        CALL SIGIO_ALDBTA(SIGHEADI,SIGDATAI,IRET)
        IF(IRET /= 0) THEN
          PRINT '("  ERROR ALLOCATING ")'
          CALL ERREXIT(4)
        ENDIF
        CALL SIGIO_SRDBTA(NSIGI,SIGHEADI,SIGDATAI,IRET)
        IF(IRET /= 0) THEN
          print *,' ERROR READING file nsigi=',nsigi
          CALL ERREXIT(4)
        ENDIF
        NCI    = SIZE(SIGDATAI%T,1)
        NCO    = (JCAPO+1)*(JCAPO+2)
        IMO    = LONBO
        JMO    = LATBO
        IJX    = IMO*LATCH2
        NTRACM = MIN(SIGHEADI%NTRAC,NTRACO)
!       NTRACO = NTRACM
        IF (IDVT == 200) NTRACO = MIN(SIGHEADI%NTRAC,NTRACO) + 2
!if more tracers
!jw        IF (IDVT > 200 ) NTRACO = SIGHEADO%NTRAC
!
        IF(OUTTYP == 0 .or. OUTTYP == 2) THEN
!---------------------------------
!-- prepare head and data for sigdatao
!
!         ----------------
           SIGHEADO = SIGHEADI
!         ----------------

          SIGHEADO%JCAP    = JCAPO
          SIGHEADO%LATB    = LATBO
          SIGHEADO%LONB    = LONBO
          SIGHEADO%LEVS    = LEVSO
          SIGHEADO%NTRAC   = NTRACO
          SIGHEADO%IDVC    = IDVCO
          SIGHEADO%IDVM    = IDVMO
          SIGHEADO%IDSL    = IDSLO
          SIGHEADO%IGEN    = IGENO
          SIGHEADO%IDVT    = IDVTO
          SIGHEADO%NCLDT   = NCLDTO
          SIGHEADO%IVS     = IVSSIGO
          SIGHEADO%NVCOORD = NVCOORDO

          CALL SIGIO_ALHEAD(SIGHEADO,IRET)
          CALL SIGIO_ALDBTA(SIGHEADO,SIGDATAO,IRET)

          SIGHEADO%LONF = SIGHEADO%LONB
          SIGHEADO%LATF = SIGHEADO%LATB
          SIGHEADO%LONR = SIGHEADO%LONB
          SIGHEADO%LATR = SIGHEADO%LATB

!  INITIALIZE TEMPORARY OUTPUT DATA

          SIGDATAO%HS = 0.0
          SIGDATAO%PS = 0.0
          SIGDATAO%T  = 0.0
          SIGDATAO%D  = 0.0
          SIGDATAO%Z  = 0.0
          SIGDATAO%Q  = 0.0
        ENDIF

        IF(OUTTYP == 0 .or. OUTTYP == 1) THEN
!---------------------------------
!-- prepare head and data for sigdatao
!
          GFSHEADO%JCAP  = JCAPO
          GFSHEADO%DIMY  = LATBO
          GFSHEADO%DIMX  = LONBO
          GFSHEADO%DIMZ  = LEVSO
          GFSHEADO%NTRAC = NTRACO
          GFSHEADO%IDVC  = IDVCO
          GFSHEADO%IDVM  = IDVMO
          GFSHEADO%IDSL  = IDSLO
          GFSHEADO%IGEN  = IGENO
          GFSHEADO%IDVT  = IDVTO
          GFSHEADO%NCLDT = NCLDTO
          GFSHEADO%IDRT  = IDRT
          GFSHEADO%LONF  = LONBO
          GFSHEADO%LATF  = LATBO
          GFSHEADO%LONR  = LONBO
          GFSHEADO%LATR  = LATBO
!
          ALLOCATE(GFSHEADVO%VCOORD(LEVSO+1,3,2))
!
          call nemsio_gfs_algrd(IMO,JMO,LEVSO,NTRACO,GFSDATAO,nopdpvv)
 
!
        ENDIF
!!!!!!!!!Do We need below here?!!!!!!!!!!!!!!!!!!!!!
!       if (sigheado%jcap /= sigheadi%jcap) SIGHEADO%PDRYINI = 0
!       print *,' pdryinia=',SIGHEADO%PDRYINI
!       IF(MQUICK.EQ.1) THEN
!         IF(SIGHEADO%LEVS.NE.SIGHEADI%LEVS) CALL ERREXIT(24)
!         IF(SIGHEADO%JCAP.LT.SIGHEADI%JCAP) CALL ERREXIT(24)
!       ENDIF
!       CALL SIGIO_ALHEAD(SIGHEADO,IRET)
!       if (mod(sigheado%idvm/10,10) == 3) then
!         sigheado%cpi(1:sigheado%ntrac+1) = cpi(0:sigheado%ntrac)
!         sigheado%ri(1:sigheado%ntrac+1)  = ri(0:sigheado%ntrac)
!       endif

!       if (mod(sigheado%idvm/10,10) == 3)
!    &   print *,' cpi=',sigheado%cpi(1:sigheado%ntrac+1)
!    &,' ri=',sigheado%ri(1:sigheado%ntrac+1)

!       PRINT '("CHANGE SIGMA FILE RESOLUTION FROM ",
!    &   I4," WAVES, ",I4," LEVELS, AND ",I4," TRACERS")',
!    &   SIGHEADI%JCAP,SIGHEADI%LEVS,SIGHEADI%NTRAC
!       PRINT '("                                  ",
!    &   " IVS=",I6," IDVC=",I2," NVCOORD=",I2,"  IDVM=",I2)',
!    &   SIGHEADI%IVS,SIGHEADI%IDVC,SIGHEADI%NVCOORD,SIGHEADI%IDVM
!       PRINT '("                               TO ",
!    &   I4," WAVES, ",I4," LEVELS, AND ",I4," TRACERS.")',
!    &   SIGHEADO%JCAP,SIGHEADO%LEVS,SIGHEADO%NTRAC
!       PRINT '("                                  ",
!    &   " IVS=",I6," IDVC=",I2," NVCOORD=",I2,"  IDVM=",I2)',
!    &   SIGHEADO%IVS,SIGHEADO%IDVC,SIGHEADO%NVCOORD,SIGHEADO%IDVM

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ OLD SIGMA FILE
!       CALL SIGIO_ALDBTA(SIGHEADI,SIGDATAI,IRET)
!       IF(IRET.NE.0) THEN
!         PRINT '("  ERROR ALLOCATING ")'
!         CALL ERREXIT(4)
!       ENDIF
!       CALL SIGIO_ALDBTA(SIGHEADO,SIGDATAO,IRET)
!       IF(IRET.NE.0) THEN
!         PRINT '("  ERROR ALLOCATING ")'
!         CALL ERREXIT(4)
!       ENDIF
!       CALL SIGIO_SRDBTA(NSIGI,SIGHEADI,SIGDATAI,IRET)
!       IF(IRET.NE.0) THEN
!         PRINT '("  ERROR READING ")'
!         CALL ERREXIT(4)
!       ENDIF
!       NCI    = SIZE(SIGDATAI%T,1)
!       NCO    = SIZE(SIGDATAO%T,1)
!       LEVSO  = SIGHEADO%LEVS
!       IMO    = SIGHEADO%LONB
!       JMO    = SIGHEADO%LATB
!       IJX    = IMO*LATCH2
!       NTRACM = MIN(SIGHEADI%NTRAC,SIGHEADO%NTRAC)
!       NTRACO = NTRACM
!       IF (IDVT == 200) NTRACO = MIN(SIGHEADI%NTRAC,SIGHEADO%NTRAC) + 2

!!!!!!!!!Do We need above here?!!!!!!!!!!!!!!!!!!!!!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!       print *,' before mquick if MQUICK=',MQUICK
        mquick_if: IF(MQUICK == 0) THEN

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ALLOCATE DATA FOR GFSIO OUTPUT
!         IF(OUTTYP.EQ.1 .OR. OUTTYP.EQ.0) THEN
!           ALLOCATE(GFSDATAO%ZS(IMO,JMO))
!           ALLOCATE(GFSDATAO%PS(IMO,JMO))
!           ALLOCATE(GFSDATAO%P(IMO,JMO,LEVSO))
!           ALLOCATE(GFSDATAO%DP(IMO,JMO,LEVSO))
!           ALLOCATE(GFSDATAO%T(IMO,JMO,LEVSO))
!           ALLOCATE(GFSDATAO%U(IMO,JMO,LEVSO))
!           ALLOCATE(GFSDATAO%V(IMO,JMO,LEVSO))
!           ALLOCATE(GFSDATAO%Q(IMO,JMO,LEVSO,NTRACO))
!           ALLOCATE(GFSDATAO%W(IMO,JMO,LEVSO))
!         ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  USE TRANSFORMS TO CHANGE RESOLUTION
          if (idrt == 4) then
          PRINT '("  USING A GAUSSIAN TRANSFORM GRID OF ",I4," X ",I4)',
     &     IMO,JMO
          elseif (idrt == 0) then
          PRINT '("  USING A LAT/LON TRANSFORM GRID OF ",I4," X ",I4)',
     &     IMO,JMO
          endif

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ALLOCATE TEMPORARY SIGIO DATA
          ALLOCATE(VCOORD_NEW(LEVSO+1,NVCOORDO))
!
          ALLOCATE(SLAT(JMO), WLAT(JMO), CLAT(JMO), RLAT(JMO))
          ALLOCATE(HSI(NCI))
          ALLOCATE(OROGI(IMO,JMO),  OROGO(IMO,JMO))
          ALLOCATE(ZSI(IMO,LATCH2), PSI(IMO,LATCH2))
          ALLOCATE(PI(IMO,LATCH2,LEVSI))
          ALLOCATE(TI(IMO,LATCH2,LEVSI))
!         ALLOCATE(TIV(IMO,LATCH2,LEVSI))
          ALLOCATE(UI(IMO,LATCH2,LEVSI))
          ALLOCATE(VI(IMO,LATCH2,LEVSI))
          ALLOCATE(QI(IMO,LATCH2,LEVSI,NTRACM))
          if (thermodyn_id_i == 3) then
            ALLOCATE(SUMQ(IMO,LATCH2,LEVSI))
            ALLOCATE(XCP (IMO,LATCH2,LEVSI))
          else
            ALLOCATE(VIRT(IMO,LATCH2,LEVSI))
          endif
!
          ALLOCATE(WI(IMO,LATCH2,LEVSI))
!
          ALLOCATE(ZSO(IMO,LATCH2), PSO(IMO,LATCH2))
          ALLOCATE(PO(IMO,LATCH2,LEVSO))
          ALLOCATE(TO(IMO,LATCH2,LEVSO))
          ALLOCATE(UO(IMO,LATCH2,LEVSO))
          ALLOCATE(VO(IMO,LATCH2,LEVSO))
          ALLOCATE(WO(IMO,LATCH2,LEVSO))
          ALLOCATE(QO(IMO,LATCH2,LEVSO,NTRACO))
          ALLOCATE(TPO(IMO,LEVSO))
          ALLOCATE(DPO(IMO,LATCH2,LEVSO))
!         ALLOCATE(DPDTO(IMO,LEVSO))
          ALLOCATE(DTDPO(IMO,LATCH2,LEVSO))
!
          call instrument(2,kall,ttot,tmin,tmax)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW LATITUDES
          CALL SPLAT(IDRT,JMO,SLAT,WLAT)
!$omp parallel do private(j)
          do j=1,jmo
            CLAT(j) = SQRT(1-SLAT(j)*SLAT(j))
            RLAT(j) = pifac * ASIN(SLAT(j))
          enddo
          IF(OUTTYP == 1 .or. OUTTYP == 0) THEN
            ALLOCATE(GFSHEADVO%LAT(IMO*JMO), GFSHEADVO%LON(IMO*JMO))
            IF(JMO /= SIGHEADI%LATB) THEN
              DO J=1,JMO
                DO I=1,IMO
                  GFSHEADVO%LAT(I+(J-1)*IMO) = RLAT(J)
                ENDDO
              ENDDO
            ENDIF
            IF(IMO /= SIGHEADI%LONB) THEN
              Do J=1,JMO
                Do I=1,IMO
                  GFSHEADVO%LON(I+(J-1)*IMO) = 360./imo*(i-1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW OROGRAPHY
          IF(NORO /= 0) THEN
            if (grb_oro) then
              JPDS    = -1
              JPDS(5) = 8
              ALLOCATE(BITMAP(IMO*JMO))
              CALL GETGB(NORO,0,IMO*JMO,0,JPDS,JGDS,
     &                   KF,K,KPDS,KGDS,BITMAP,OROGO,IOSORO)
              DEALLOCATE(BITMAP)
              IF(IOSORO == 0 .AND. (KGDS(1) /= IDRT .OR.
     &        KGDS(2) /= IMO .OR. KGDS(3) /= JMO)) IOSORO = 100
            else
              allocate (oro4(imo,jmo))
              read(noro) oro4
              orogo = oro4
              deallocate(oro4)
              iosoro = 0
            endif
          ELSE
            IOSORO = 9
          ENDIF
          IF(IOSORO == 0) THEN
            PRINT '("  NEW OROGRAPHY READ IN")'
            if (grb_oro) then
              if (kgds(4) == -90000 .and. kgds(5) == -180000) then
                print *,' reversing the lat/lon for orography'
                call REVERS(imo, jmo, OROGO)
              endif
            endif
!           SIGHEADO%PDRYINI = 0
            PDRYINIO = 0
          ELSE
            PRINT '("  NEW OROGRAPHY TRUNCATED FROM OLD")'
          ENDIF
!
          call instrument(3,kall,ttot,tmin,tmax)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW SIGMA LEVELS
          IF(NSIL /= 0) THEN
!cggg
            allocate(vcoord(LEVSO+1,NVCOORDO))
            CALL NEWSIG(NSIL,IDVCO,LEVSO,
     &                  NVCOORDO,VCOORD,IOSSIL)
            print *,'after read newsig,iossil=',iossil,
     &      'NVCOORDO=',NVCOORDO,'vcoord=',
     &      vcoord(1:5,1:NVCOORDO)
            VCOORD_NEW(1:LEVSO+1,1:NVCOORDO) =
     &           vcoord(1:LEVSO+1,1:NVCOORDO)
!           allocate(vcoord(SIGHEADO%LEVS+1,SIGHEADO%NVCOORD))
!           vcoord = SIGHEADO%VCOORD
!           CALL NEWSIG(NSIL,SIGHEADO%IDVC,SIGHEADO%LEVS,
!    &                  SIGHEADO%NVCOORD,VCOORD,IOSSIL)
!           SIGHEADO%VCOORD = vcoord

            deallocate(vcoord)
!cggg     &                  SIGHEADO%NVCOORD,SIGHEADO%VCOORD,IOSSIL)
            IF(IOSSIL == 0) THEN
              PRINT '("  NEW MODEL LEVELS READ IN")'
            ENDIF
          ELSEIF(IDVCO    == SIGHEADI%IDVC .AND.
     &           LEVSO    == SIGHEADI%LEVS .AND.
     &           NVCOORDO == SIGHEADI%NVCOORD) THEN
            VCOORD_NEW = SIGHEADI%VCOORD
            IOSSIL = 0
            PRINT '("  NEW MODEL LEVELS COPIED FROM OLD")'
!         ELSEIF(SIGHEADO%IDVC.EQ.SIGHEADI%IDVC.AND.
!    &           SIGHEADO%LEVS.EQ.SIGHEADI%LEVS.AND.
!    &           SIGHEADO%NVCOORD.EQ.SIGHEADI%NVCOORD) THEN
!           SIGHEADO%VCOORD=SIGHEADI%VCOORD
!           IOSSIL=0
!           PRINT '("  NEW MODEL LEVELS COPIED FROM OLD")'
          ELSE
            IOSSIL = 42
          ENDIF
          IF(IOSSIL /= 0) THEN
            PRINT '("  ERROR DEFINING SIGMA VALUES ",I4)',IOSSIL
            CALL ERREXIT(8)
          ENDIF
          call instrument(4,kall,ttot,tmin,tmax)
!
! ------------------------------------------------------------------
!  GET PS and T DATA TYPE FOR THE FILE
          sfcpress_id_i  = mod(SIGHEADI%IDVM,10)
          thermodyn_id_i = mod(SIGHEADI%IDVM/10,10)
          sfcpress_id_o  = mod(IDVMO,10)
          thermodyn_id_o = mod(IDVMO/10,10)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  INITIALIZE TEMPORARY OUTPUT DATA
!         SIGDATAO%HS=0
!         SIGDATAO%PS=0
!         SIGDATAO%T=0
!         SIGDATAO%D=0
!         SIGDATAO%Z=0
!         SIGDATAO%Q=0

          HSI = SIGDATAI%HS

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  LOOP OVER LATITUDE
          timeomega = 0.
          mysum     = 0.

          DO J1=1,(JMO+1)/2,LATCH
            J2  = MIN(J1+LATCH-1,(JMO+1)/2)
!           write(0,*)' for j1=',j1,' j2=',j2,' latch=',latch
            JL  = 2*(J2-J1+1)
            IJL = IMO*JL
            CALL TRSSC(SIGHEADI%JCAP,NCI,SIGHEADI%LEVS,NTRACM,
     &                 SIGHEADI%IDVM,IDRT,IMO,JMO,IJX,J1,J2,1,
     &                 lpl3(j1:j2),
     &                 SIGDATAI%HS,SIGDATAI%PS,SIGDATAI%T,
     &                 SIGDATAI%D,SIGDATAI%Z,SIGDATAI%Q,
     &                 ZSI,PSI,TI,UI,VI,QI
     &                 )
!           call hhmaxmin(ti,IMO,2*LATCH,1,JL,SIGHEADI%LEVS,' ti  ' )
!           call hhmaxmin(ui,IMO,2*LATCH,1,JL,SIGHEADI%LEVS,' ui  ' )
!           call hhmaxmin(vi,IMO,2*LATCH,1,JL,SIGHEADI%LEVS,' vi  ' )
!
            if (thermodyn_id_i == 3) then
              xcp(:,1:JL,:)  = 0.0
              sumq(:,1:JL,:) = 0.0
              do n=1,NTRACM
                if( cpi(n) .ne. 0.0 .and. ri(n) .ne. 0.0) then
                 xcp(:,1:JL,:)  = xcp(:,1:JL,:)  + cpi(n)*qi(:,1:JL,:,n)
                 sumq(:,1:JL,:) = sumq(:,1:JL,:) + qi(:,1:JL,:,n)
                endif
              enddo
              xcp(:,1:JL,:)  = (1.-sumq(:,1:JL,:))*cpi(0)+xcp(:,1:JL,:)
!
!             call hhmaxmin(xcp,IMO,2*LATCH,JL,SIGHEADI%LEVS,' xcp ' )
            else
              virt(:,1:JL,:) = (1.+(461.50/287.05-1)*QI(:,1:JL,:,1))
!             call hhmaxmin(virt,IMO,2*LATCH,JL,SIGHEADI%LEVS,' virt  ')
            endif
!
!           TIV(:,1:JL,:) = TI(:,1:JL,:)
!    &                    * (1.+(461.50/287.05-1)*QI(:,1:JL,:,1))  ! virtual temperature
            call instrument(5,kall,ttot,tmin,tmax)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CONVERT TO SURFACE PRESSURE AND TEMPERATURE
!
!            call hhmaxmin(psi,IMO,2*LATCH,1,JL,1,' psi before case')
            SELECT CASE(sfcpress_id_i)
             CASE(0,1)
              PSI(:,1:JL) = 1.E3*EXP(PSI(:,1:JL))
             CASE(2)
              PSI(:,1:JL) = 1.E3*PSI(:,1:JL)
             CASE DEFAULT
              PRINT *,' DEFAULT SELECTED: PSI is P in pascal '
            END SELECT
!           call hhmaxmin(psi,IMO,2*LATCH,1,JL,1,' psi in chgres ' )
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  USE NEW OROGRAPHY OR USE TRUNCATED OROGRAPHY
            IF(IOSORO == 0) THEN
              DO J=J1,J2
                JN=J
                JS=JMO+1-J
                do i=1,imo
                  ZSO(i,2*(J-J1)+1) = OROGO(i,JN)
                  ZSO(i,2*(J-J1)+2) = OROGO(i,JS)
                  OROGI(i,JN)       = ZSI(i,2*(J-J1)+1)
                  OROGI(i,JS)       = ZSI(i,2*(J-J1)+2)
                enddo
              ENDDO
            ELSE
              do i=1,imo
                ZSO(i,1:JL) = ZSI(i,1:JL)
              enddo
              DO J=J1,J2
                JN=J
                JS=JMO+1-J
                do i=1,imo
                  OROGI(i,JN) = ZSI(i,2*(J-J1)+1)
                  OROGI(i,JS) = ZSI(i,2*(J-J1)+2)
                  OROGO(i,JN) = ZSO(i,2*(J-J1)+1)
                  OROGO(i,JS) = ZSO(i,2*(J-J1)+2)
                enddo
              ENDDO
            ENDIF
         call instrument(6,kall,ttot,tmin,tmax)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NEW PRESSURE AND NEW SURFACE PRESSURE
!
            if (thermodyn_id_i == 3) then
               TI(:,1:JL,:) = TI(:,1:JL,:)/cpi(0)   ! enthalpy (CpT/Cpd)
            endif
!
!compute omega
            timestart=timef()
!cggg
            allocate(vcoord(SIGHEADI%LEVS+1,SIGHEADI%NVCOORD))
            vcoord = SIGHEADI%VCOORD
            call getomega(SIGHEADI%JCAP,NCI,SIGHEADI%LEVS,
     &        SIGHEADI%IDVC,SIGHEADI%IDVM,IDRT,SIGHEADI%IDSL,
     &        SIGHEADI%NVCOORD,VCOORD,
     &        IMO,JMO,IJL,IJX,J1,J2,1,SIGDATAI%D,SIGDATAI%PS,
     &        PSI,TI,UI,VI,WI)
             timeend=timef()
             timeomega=timeomega+timeend-timestart
!
!     print *,' before SIGIO_MODPRD'
            CALL SIGIO_MODPRD
     &       (IJL,IJX,SIGHEADI%LEVS,SIGHEADI%NVCOORD,
     &        SIGHEADI%IDVC,SIGHEADI%IDSL,VCOORD,IRET,
     &        PS=PSI,T=TI,PM=PI)
!cggg
            deallocate (vcoord)
!
            select case( thermodyn_id_i )
             case(0,1)
              TI(:,1:JL,:) = TI(:,1:JL,:)/virt(:,1:JL,:)         ! to t
             case(2)
!             print *,' thermodyn_id_o=',thermodyn_id_o,' dry t '
             case(3)
              TI(:,1:JL,:) = TI(:,1:JL,:)/xcp(:,1:JL,:)*cpi(0)   ! to t
             case default
!             print *,' unknown input thermodyn id =',thermodyn_id_i
            end select
!
            CALL NEWPS(IJL,ZSI,PSI,IJX,SIGHEADI%LEVS,
     &                 PI,TI,QI,ZSO,PSO)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  VERTICALLY INTERPOLATE UPPER-AIR FIELDS
! -- Mark Iredell's approach 
!           TPO=250.               !virtual temperature
!           DO ITER=1,100
!             CALL SIGIO_MODPR
!    &         (IJL,IJX,SIGHEADO%LEVS,SIGHEADO%NVCOORD,
!    &          SIGHEADO%IDVC,SIGHEADO%IDSL,SIGHEADO%VCOORD,IRET,
!    &          PS=PSO,T=TPO,PM=PO,DPMDT=DPDTO,PD=DPO)
!        call instrument(7,kall,ttot,tmin,tmax)
!             CALL VINTG(IJL,IJX,SIGHEADI%LEVS,SIGHEADO%LEVS,NTRACM,
!    &                   PI,UI,VI,TIV,QI,PO,UO,VO,TO,QO,DTDPO)
!             TPO=(TPO-TO)*(DPDTO*DTDPO)/(DPDTO*DTDPO-1)
!             IF(MAXVAL(ABS(TPO/TO)).GT.1.E-6) THEN
!               TO=TO+TPO
!               TPO=TO
!             ELSE
!               EXIT
!             ENDIF
!        call instrument(8,kall,ttot,tmin,tmax)
!           ENDDO
!           print *,' maxima iteration is ',iter
!           TO=TO/(1.+(461.50/287.05-1)*QO(:,:,:,1))  ! virtual to real
! -- end of Mark Iredell's approach 
!
!
!  VERTICALLY INTERPOLATE UPPER-AIR FIELDS
! -- Henry Juang's approach
         allocate (vcoord(LEVSO+1,NVCOORDO))
         vcoord = VCOORD_NEW
         CALL NEWPR1(IJL,IJX,LEVSO,SIGHEADI%LEVS,
     &               IDVCO,IDVMO,IDSLO,
     &               NVCOORDO, vcoord,
     &               RI, CPI, NTRACM,
     &               PI,TI,QI,PSO,PO,DPO)
         deallocate (vcoord)
         call instrument(7,kall,ttot,tmin,tmax)
         CALL VINTG(IJL,IJX,SIGHEADI%LEVS,LEVSO,NTRACM,
     &                 PI,UI,VI,TI,QI,WI,PO,UO,VO,TO,QO,DTDPO,WO)

!           call hhmaxmin(uo,IMO,2*LATCH,1,JL,SIGHEADI%LEVS,' ui 2a ' )
!           call hhmaxmin(vo,IMO,2*LATCH,1,JL,SIGHEADI%LEVS,' vi 2a ' )
!
c idea add init condition for temp tracer4-5 ( o o2)
          IF (IDVT == 200) then
            CALL  VINTG_IDEA(IMO,LATCH,LEVSO,NTRACO,
     &        PO,RLAT,JMO,J1,J2,SIGHEADI%IDATE,UO,VO,TO,QO)
          ENDIF
!           print*,'www'
!           print*,' qo=',QO(1,1,1:SIGHEADO%LEVS,4)
!
          if( IDVCO  == 3 ) then
!cggg
            allocate(ak(LEVSO+1), bk(LEVSO+1), ck(LEVSO+1))
            do k=1,LEVSO+1
              ak(k) = VCOORD_NEW(k,1)
              bk(k) = VCOORD_NEW(k,2)
              ck(k) = VCOORD_NEW(k,3)
            enddo
            call checkdp(IJL,IJX,LEVSO,ak,bk,ck,PSO,TO,QO)

!             call checkdp(IJL,IJX,SIGHEADO%LEVS,
!    &                 SIGHEADO%VCOORD(1:(SIGHEADO%LEVS+1),1),
!    &                 SIGHEADO%VCOORD(1:(SIGHEADO%LEVS+1),2),
!    &                 SIGHEADO%VCOORD(1:(SIGHEADO%LEVS+1),3),
!    &                 PSO,TO,QO)
!cggg
            deallocate (ak, bk, ck)
          endif
          call instrument(8,kall,ttot,tmin,tmax)
! -- end of Henry Juang's approach

!----force tracers to be positvie
          if (CHGQ0 == 1) QO = max(QO, 0.0)
!----force tracers to be positvie

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  PASS DATA TO NEMSIO
            IF(OUTTYP == 1 .OR. OUTTYP == 0) THEN
!
!  ALLOCATE DATA FOR NEMSIO OUTPUT
!
             DO J=J1,J2
               JN = J
               JS = JMO+1-J
               DO I=1,IMO
                 GFSDATAO%ZS(I,JN) = ZSO(I,2*(J-J1)+1)
                 GFSDATAO%ZS(I,JS) = ZSO(I,2*(J-J1)+2)
                 GFSDATAO%PS(I,JN) = PSO(I,2*(J-J1)+1)
                 GFSDATAO%PS(I,JS) = PSO(I,2*(J-J1)+2)
               ENDDO
               if (nopdpvv) then
                 DO K=1,LEVSO
                   DO I=1,IMO
                     GFSDATAO%T(I,JN,K)  = TO(I,2*(J-J1)+1,K)
                     GFSDATAO%T(I,JS,K)  = TO(I,2*(J-J1)+2,K)
                     GFSDATAO%U(I,JN,K)  = UO(I,2*(J-J1)+1,K)
                     GFSDATAO%U(I,JS,K)  = UO(I,2*(J-J1)+2,K)
                     GFSDATAO%V(I,JN,K)  = VO(I,2*(J-J1)+1,K)
                     GFSDATAO%V(I,JS,K)  = VO(I,2*(J-J1)+2,K)
                   ENDDO
                 ENDDO
               else
                 DO K=1,LEVSO
                   DO I=1,IMO
                     GFSDATAO%DP(I,JN,K) = DPO(I,2*(J-J1)+1,K)
                     GFSDATAO%DP(I,JS,K) = DPO(I,2*(J-J1)+2,K)
                     GFSDATAO%P(I,JN,K)  = PO(I,2*(J-J1)+1,K)
                     GFSDATAO%P(I,JS,K)  = PO(I,2*(J-J1)+2,K)
                     GFSDATAO%T(I,JN,K)  = TO(I,2*(J-J1)+1,K)
                     GFSDATAO%T(I,JS,K)  = TO(I,2*(J-J1)+2,K)
                     GFSDATAO%U(I,JN,K)  = UO(I,2*(J-J1)+1,K)
                     GFSDATAO%U(I,JS,K)  = UO(I,2*(J-J1)+2,K)
                     GFSDATAO%V(I,JN,K)  = VO(I,2*(J-J1)+1,K)
                     GFSDATAO%V(I,JS,K)  = VO(I,2*(J-J1)+2,K)
                     GFSDATAO%W(I,JN,K)  = WO(I,2*(J-J1)+1,K)
                     GFSDATAO%W(I,JS,K)  = WO(I,2*(J-J1)+2,K)
                   ENDDO
                 ENDDO
               endif
!idea?
               DO N=1,NTRACM
                 DO K=1,LEVSO
                   DO I=1,IMO
                     GFSDATAO%Q(I,JN,K,N) = QO(I,2*(J-J1)+1,K,N)
                     GFSDATAO%Q(I,JS,K,N) = QO(I,2*(J-J1)+2,K,N)
                   ENDDO
                 ENDDO
               ENDDO
             ENDDO
            ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!   TRANSFORM BACK TO THE NEW SPECTRAL SPACE FOR SIGIO SIGMA OUTPUT 
            IF(OUTTYP == 2 .OR. OUTTYP == 0) THEN
!             print*,' transform back to the new spectral space '
              CALL TRBSC(JCAPO,NCO,LEVSO,NTRACO,
     &                   IDVMO,IDRT,IMO,JMO,IJX,J1,J2,1,cpi,
     &                   ZSO,PSO,TO,UO,VO,QO,
     &                   SIGDATAO%HS,SIGDATAO%PS,SIGDATAO%T,
     &                   SIGDATAO%D,SIGDATAO%Z,SIGDATAO%Q)
            ENDIF
         call instrument(9,kall,ttot,tmin,tmax)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ENDDO
! END OF LOOP OVER LATITUDE
          print *,'omega time=',timeomega
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  DEALLOCATE TEMPORARY DATA
          DEALLOCATE(ZSI, PSI, PI, TI, UI, VI, WI, QI)
!         DEALLOCATE(TIV)
          DEALLOCATE(ZSO, PSO, PO, TO, UO, VO, WO, QO)
          DEALLOCATE(TPO, DTDPO, DPO)
!         DEALLOCATE(DPDTO)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  USE PADDING OR TRUNCATION TO CHANGE RESOLUTION

        ELSE
          CALL PADSSC(SIGHEADI%JCAP,NCI,SIGHEADO%JCAP,NCO,
     &                SIGHEADO%LEVS,NTRACO,
     &                SIGDATAI%HS,SIGDATAI%PS,SIGDATAI%T,
     &                SIGDATAI%D,SIGDATAI%Z,SIGDATAI%Q,
     &                SIGDATAO%HS,SIGDATAO%PS,SIGDATAO%T,
     &                SIGDATAO%D,SIGDATAO%Z,SIGDATAO%Q)
        ENDIF  mquick_if

!     print *,' after mquick_if'
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GENERATE NEW OZONE FROM CLIMATOLOGY IF NECESSARY
        IF(SIGHEADI%NTRAC == 1 .AND. NTRACO > 1  .AND.
     &     IDVCO == 0 .AND. MOD(IDVTO/1,10) <= 1 .AND. NO3C /= 0) THEN
          IDAT    = 0
          IDAT(1) = SIGHEADO%IDATE(4)
          IDAT(2) = SIGHEADO%IDATE(2)
          IDAT(3) = SIGHEADO%IDATE(3)
          IDAT(5) = SIGHEADO%IDATE(1)
          RINC    = 0
          RINC(2) = SIGHEADO%FHOUR
          CALL W3MOVDAT(RINC,IDAT,JDAT)
          SIGHEADO%VCOORD=VCOORD_NEW(:,:)
          CALL SPECO3(NO3C,NO3T,SIGHEADO%JCAP,NCO,SIGHEADO%LEVS,
     &                SIGHEADO%LONB,SIGHEADO%LATB,1,JDAT,
     &                SIGHEADO%SL,SIGHEADO%VCOORD(:,1),
     &                SIGDATAO%PS,SIGDATAO%Q(1,1,2))
          PRINT '("  OZONE GENERATED FROM CLIMATOLOGY")'
        ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GENERATE SPECIAL SETS OF TRACERS
        IF(SIGHEADO%IDVT > 0 .AND. MOD(SIGHEADO%IDVT,100) == 0) THEN
          CALL SPECSETS(SIGHEADO,SIGDATAO,IDRT)
        ENDIF

!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  delete data
!
        CALL SIGIO_AXDBTA(SIGDATAI,IRET)

!  WRITE THE NEW SIGMA FILE
        IF(OUTTYP == 2 .OR. OUTTYP == 0) THEN
!         ----------------
!
          SIGHEADO%LONF    = SIGHEADO%LONB
          SIGHEADO%LATF    = SIGHEADO%LATB
          SIGHEADO%LONR    = SIGHEADO%LONB
          SIGHEADO%LATR    = SIGHEADO%LATB
          SIGHEADO%PDRYINI = PDRYINIO
          SIGHEADO%VCOORD  = VCOORD_NEW(:,:)
!
          if (mod(sigheado%idvm/10,10) == 3) then
           sigheado%cpi(1:sigheado%ntrac+1) = cpi(0:sigheado%ntrac)
           sigheado%ri(1:sigheado%ntrac+1)  = ri(0:sigheado%ntrac)
         endif

!      print *,' sigheado%vcoord=',sigheado%vcoord(1:5,1:nvcoordo),
!     &  'nvcoordo=',nvcoordo
!      print *,' sigheado%pdryini2=',sigheado%pdryini
          CALL SIGIO_SWHEAD(NSIGO,SIGHEADO,IRET)
          CALL SIGIO_SWDBTA(NSIGO,SIGHEADO,SIGDATAO,IRET)
          CALL SIGIO_SCLOSE(NSIGO,IRET)
          CALL SIGIO_AXDBTA(SIGDATAO,IRET)
        ENDIF
        call instrument(10,kall,ttot,tmin,tmax)
!
        CALL SIGIO_SCLOSE(NSIGI,IRET)

! ---------------------------------------------------------------------
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!  CHANGE RESOLUTION OF INPUT NEMSIO GRID FILE
!  OUTPUT HISTORY FILE IN SIGIO SIGMA OR NEMSIO GRID FORMAT, OR BOTH

      ELSEIF(INPTYP == 1) THEN
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
! ---------------------------------------------------------------------

        PRINT*, 'CHGRES INPUT:  NEMSIO GRID FILE '
        IF(OUTTYP == 1 )PRINT*, 'CHGRES OUTPUT: NEMSIO GRID FILE'
        IF(OUTTYP == 2 )PRINT*, 'CHGRES OUTPUT: SIGIO SIGMA FILE'
        IF(OUTTYP == 0 )PRINT*,
     &       'CHGRES OUTPUT: BOTH SIGIO SIGMA and NEMSIO GRID FILES'
        IF(OUTTYP /= 1 .and. MQUICK == 1) CALL ERREXIT(24)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OPEN (READ) NEMSIO GRID FILE HEADERS
        CALL NEMSIO_GETFILEHEAD(GFILEI
     &,                         IDATE=GFSHEADI%IDATE
     &,                         NFHOUR=GFSHEADI%NFHOUR
     &,                         NFMINUTE=GFSHEADI%NFMINUTE
     &,                         NFSECONDN=GFSHEADI%NFSECONDN
     &,                         NFSECONDD=GFSHEADI%NFSECONDD
     &,                         VERSION=GFSHEADI%VERSION
     &,                         NREC=GFSHEADI%NREC
     &,                         DIMX=GFSHEADI%DIMX
     &,                         DIMY=GFSHEADI%DIMY
     &,                         DIMZ=GFSHEADI%DIMZ
     &,                         JCAP=GFSHEADI%JCAP
     &,                         NTRAC=GFSHEADI%NTRAC
     &,                         NCLDT=GFSHEADI%NCLDT
     &,                         NSOIL=GFSHEADI%NSOIL
     &,                         IDSL=GFSHEADI%IDSL
     &,                         IDVC=GFSHEADI%IDVC
     &,                         IDVM=GFSHEADI%IDVM
     &,                         IDRT=GFSHEADI%IDRT
     &,                         extrameta=GFSHEADI%extrameta
     &,                         nmetavari=GFSHEADI%nmetavari
     &,                         nmetavarr=GFSHEADI%nmetavarr
     &,                         nmetavarr8=GFSHEADI%nmetavarr8
     &,                         nmetavarl=GFSHEADI%nmetavarl
     &,                         nmetavarc=GFSHEADI%nmetavarc
     &,                         nmetaaryi=GFSHEADI%nmetaaryi
     &,                         nmetaaryr=GFSHEADI%nmetaaryr
     &,                         nmetaaryr8=GFSHEADI%nmetaaryr8
     &,                         IRET=IRET0)
!
        CALL NEMSIO_GETHEADVAR(GFILEI,'FHOUR', GFSHEADI%FHOUR,IRET=IRET)
        IF(IRET/=0) GFSHEADI%FHOUR = real(GFSHEADI%NFHOUR,8)
     &    +real(GFSHEADI%NFMINUTE,8)/60.
     &    +real(GFSHEADI%NFSECONDN,8)/(3600.*GFSHEADI%NFSECONDD)
!
!       CALL NEMSIO_GETHEADVAR(GFILEI,'LATB', GFSHEADI%LATB,IRET=IRET)
!       CALL NEMSIO_GETHEADVAR(GFILEI,'LONB', GFSHEADI%LONB,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'dimy', GFSHEADI%LATB,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'dimx', GFSHEADI%LONB,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'LEVS', GFSHEADI%LEVS,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'ITRUN', GFSHEADI%ITRUN,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IORDER', GFSHEADI%IORDER,
     &                         IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IREALF', GFSHEADI%IREALF,
     &                         IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IGEN', GFSHEADI%IGEN,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'LATF', GFSHEADI%LATF,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'LONF', GFSHEADI%LONF,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'LATR', GFSHEADI%LATR,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'LONR', GFSHEADI%LONR,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'ICEN2', GFSHEADI%ICEN2,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IENS', GFSHEADI%IENS,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IDPP', GFSHEADI%IDPP,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IDVT', GFSHEADI%IDVT,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IDRUN', GFSHEADI%IDRUN,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IDUSR', GFSHEADI%IDUSR,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'PDRYINI', GFSHEADI%PDRYINI,
     &                         IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IXGR', GFSHEADI%IXGR,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IVS', GFSHEADI%IVSSIG,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'NVCOORD', GFSHEADI%NVCOORD,
     &                         IRET=IRET)
        print *,'input from nemsio,idate=',GFSHEADI%idate,'version=',
     &   GFSHEADI%version,'nrec=',GFSHEADI%nrec,'jcap=',GFSHEADI%jcap,
     &   'ntrac=',GFSHEADI%ntrac,'NCLDT=',GFSHEADI%NCLDT,
     &   'idsl=',GFSHEADI%idsl,'idvc=',GFSHEADI%idvc,'idvm=',
     &   GFSHEADI%idvm,'idrt=',GFSHEADI%idrt,'fhour=',GFSHEADI%fhour,
     &   'latb=',GFSHEADI%latb,'lonb=',GFSHEADI%lonb,'levs=',
     &   GFSHEADI%levs,'itrun=',GFSHEADI%itrun,'iorder=',
     &   GFSHEADI%iorder,'irealf=',GFSHEADI%irealf,'igen=',
     &   GFSHEADI%igen,'latf=',GFSHEADI%latf,'lonf=',GFSHEADI%lonf,
     &   'latr=',GFSHEADI%latr,'lonr=',GFSHEADI%lonr,
     &   'icen2=',GFSHEADI%icen2,'iens=',GFSHEADI%iens,'idpp=',
     &   GFSHEADI%idpp,'idvt=',GFSHEADI%idvt,'idrun=',GFSHEADI%idrun,
     &   'pdryini=',GFSHEADI%pdryini,'ivs=',gfsheado%ivssig,
     &    'ixgr=',GFSHEADI%ixgr,'nvcoord=',GFSHEADI%nvcoord

        LEVSI = GFSHEADI%DIMZ
        IMI   = GFSHEADI%DIMX
        JMI   = GFSHEADI%DIMY
        ALLOCATE(GFSHEADVI%RECNAME(GFSHEADI%NREC))
        ALLOCATE(GFSHEADVI%RECLEVTYP(GFSHEADI%NREC))
        ALLOCATE(GFSHEADVI%RECLEV(GFSHEADI%NREC))
        ALLOCATE(GFSHEADVI%VCOORD(LEVSI+1,3,2))
        ALLOCATE(GFSHEADVI%LAT(IMI*JMI))
        ALLOCATE(GFSHEADVI%LON(IMI*JMI))
        ALLOCATE(GFSHEADVI%CPI(GFSHEADI%NTRAC+1))
        ALLOCATE(GFSHEADVI%RI(GFSHEADI%NTRAC+1))
        if(GFSHEADI%nmetavari>0)  then
          ALLOCATE(GFSHEADVI%variname(GFSHEADI%nmetavari))
          ALLOCATE(GFSHEADVI%varival(GFSHEADI%nmetavari))
          CALL NEMSIO_GETFILEHEAD(GFILEI
     &,                           variname=GFSHEADVI%variname
     &,                           varival=GFSHEADVI%varival,IRET=IRET1)
        endif
        if(GFSHEADI%nmetavarr>0)  then
          ALLOCATE(GFSHEADVI%varrname(GFSHEADI%nmetavarr))
          ALLOCATE(GFSHEADVI%varrval(GFSHEADI%nmetavarr))
          CALL NEMSIO_GETFILEHEAD(GFILEI
     &,                           varrname=GFSHEADVI%varrname
     &,                           varrval=GFSHEADVI%varrval,IRET=IRET1)
        endif
        if(GFSHEADI%nmetavarl>0)  then
          ALLOCATE(GFSHEADVI%varlname(GFSHEADI%nmetavarl))
          ALLOCATE(GFSHEADVI%varlval(GFSHEADI%nmetavarl))
          CALL NEMSIO_GETFILEHEAD(GFILEI
     &,                           varlname=GFSHEADVI%varlname
     &,                           varlval=GFSHEADVI%varlval,IRET=IRET1)
        endif
        if(GFSHEADI%nmetavarc>0)  then
          ALLOCATE(GFSHEADVI%varcname(GFSHEADI%nmetavarc))
          ALLOCATE(GFSHEADVI%varcval(GFSHEADI%nmetavarc))
          CALL NEMSIO_GETFILEHEAD(GFILEI
     &,                           varcname=GFSHEADVI%varcname
     &,                           varcval=GFSHEADVI%varcval,IRET=IRET1)
        endif
        if(GFSHEADI%nmetavarr8>0)  then
          ALLOCATE(GFSHEADVI%varr8name(GFSHEADI%nmetavarr8))
          ALLOCATE(GFSHEADVI%varr8val(GFSHEADI%nmetavarr8))
          CALL NEMSIO_GETFILEHEAD(GFILEI
     &,                           varr8name=GFSHEADVI%varr8name
     &,                           varr8val=GFSHEADVI%varr8val
     &,                           IRET=IRET1)
        endif
        if(GFSHEADI%nmetaaryi>0)  then
          ALLOCATE(GFSHEADVI%aryiname(GFSHEADI%nmetaaryi))
          ALLOCATE(GFSHEADVI%aryilen(GFSHEADI%nmetaaryi))
          CALL NEMSIO_GETFILEHEAD(GFILEI
     &,                           aryiname=GFSHEADVI%aryiname
     &,                           aryilen=GFSHEADVI%aryilen,IRET=IRET1)
        endif
        if(GFSHEADI%nmetaaryr>0)  then
          ALLOCATE(GFSHEADVI%aryrname(GFSHEADI%nmetaaryr))
          ALLOCATE(GFSHEADVI%aryrlen(GFSHEADI%nmetaaryr))
          CALL NEMSIO_GETFILEHEAD(GFILEI
     &,                           aryrname=GFSHEADVI%aryrname
     &,                           aryrlen=GFSHEADVI%aryrlen,IRET=IRET1)
        endif
        if(GFSHEADI%nmetaaryr8>0)  then
          ALLOCATE(GFSHEADVI%aryr8name(GFSHEADI%nmetaaryr8))
          ALLOCATE(GFSHEADVI%aryr8len(GFSHEADI%nmetaaryr8))
          CALL NEMSIO_GETFILEHEAD(GFILEI
     &,                           aryr8name=GFSHEADVI%aryr8name
     &,                           aryr8len=GFSHEADVI%aryr8len
     &,                           IRET=IRET1)
        endif

        CALL NEMSIO_GETFILEHEAD(GFILEI
     &,                         RECNAME=GFSHEADVI%RECNAME
     &,                         RECLEVTYP=GFSHEADVI%RECLEVTYP
     &,                         RECLEV=GFSHEADVI%RECLEV
     &,                         VCOORD=GFSHEADVI%VCOORD
     &,                         LAT=GFSHEADVI%LAT
     &,                         LON=GFSHEADVI%LON
     &,                         CPI=GFSHEADVI%CPI
     &,                         RI=GFSHEADVI%RI
     &,                         IRET=IRET1)
        if(GFSHEADI%nmetaaryi > 0) then
          ALLOCATE(GFSHEADVI%aryival(maxval(GFSHEADVI%aryilen),
     &             GFSHEADI%nmetaaryi))
          CALL NEMSIO_GETFILEHEAD(GFILEI,aryival=GFSHEADVI%aryival)
        else
          ALLOCATE(GFSHEADVI%aryival(1,1))
        endif
        if(GFSHEADI%nmetaaryr > 0) then
          ALLOCATE(GFSHEADVI%aryrval(maxval(GFSHEADVI%aryrlen),
     &             GFSHEADI%nmetaaryr))
          CALL NEMSIO_GETFILEHEAD(GFILEI,aryrval=GFSHEADVI%aryrval)
        else
          ALLOCATE(GFSHEADVI%aryrval(1,1))
        endif
        if(GFSHEADI%nmetaaryr8 > 0) then
          ALLOCATE(GFSHEADVI%aryr8val(maxval(GFSHEADVI%aryr8len),
     &     GFSHEADI%nmetaaryr8))
          CALL NEMSIO_GETFILEHEAD(GFILEI,aryr8val=GFSHEADVI%aryr8val)
        else
          ALLOCATE(GFSHEADVI%aryr8val(1,1))
        endif
!jw     IF(IRET.NE.0 .OR. IRET1.NE.0) THEN
        IF(GFSHEADI%NVCOORD == -9999) THEN
           GFSHEADI%NVCOORD = 3
           if(maxval(GFSHEADVI%VCOORD(:,3,1)) == 0. .and.
     &        minval(GFSHEADVI%VCOORD(:,3,1)) == 0. ) then
            GFSHEADI%NVCOORD = 2
!jw for hyb: when no idsl is set
            if(GFSHEADI%IDSL == -9999)GFSHEADI%IDSL = 1
            if(maxval(GFSHEADVI%VCOORD(:,2,1)) == 0. .and.
     &         minval(GFSHEADVI%VCOORD(:,2,1)) ==0.) then
            GFSHEADI%NVCOORD = 1
            endif
          endif
        ENDIF
        IF(IRET0.NE.0 ) THEN
          PRINT*, 'ERROR READNG NEMSIO FILE HEADER. EXIT'
          CALL ERREXIT(24)
        ENDIF

!       print *,
!    & 'nmetavari=',GFSHEADI%nmetavari,'nmetavarr=',GFSHEADI%nmetavarr,
!    & 'nmetaaryi=',GFSHEADI%nmetaaryi,'nmetaaryr=',GFSHEADI%nmetaaryr,
!    & 'aryilen=',GFSHEADVI%aryilen,'GFSHEADVI%aryiname=',
!    &   GFSHEADVI%aryiname,'nvcoord=',GFSHEADI%NVCOORD

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ INPUT NEMSIO DATA ARRAY
        ALLOCATE(GFSDATAI%ZS(IMI,JMI))
        ALLOCATE(GFSDATAI%PS(IMI,JMI))
        ALLOCATE(GFSDATAI%T(IMI,JMI,LEVSI))
        ALLOCATE(GFSDATAI%U(IMI,JMI,LEVSI))
        ALLOCATE(GFSDATAI%V(IMI,JMI,LEVSI))
        ALLOCATE(GFSDATAI%Q(IMI,JMI,LEVSI,GFSHEADI%NTRAC))
        if (.not. nopdpvv) then
          ALLOCATE(GFSDATAI%P(IMI,JMI,LEVSI))
          ALLOCATE(GFSDATAI%DP(IMI,JMI,LEVSI))
          ALLOCATE(GFSDATAI%W(IMI,JMI,LEVSI))
        endif
        allocate(tmp(size(GFSDATAI%ZS,1)*size(GFSDATAI%ZS,2)) )
!
!       CALL NEMSIO_GFS_RDGRD(GFILEI,GFSDATAI,nopdpvv,iret=iret)
        CALL NEMSIO_GFS_RDGRD(GFILEI,GFSDATAI,iret=iret)
        do k=1,levsi
         print *,'aft nemsio_gfs_rdgrd,k=',k,'tmp=',
     &     maxval(GFSDATAI%t(:,:,k)),
     &     minval(GFSDATAI%t(:,:,k)),'q=',maxval(GFSDATAI%q(:,:,k,1)),
     &     minval(GFSDATAI%q(:,:,k,1))
        enddo
!
        NCI = SIZE(GFSDATAI%T,1)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  UPDATE NEMSIO OUTPUT FILE HEADERS USING NAME LIST VARIABLES IF NECESSARY

        !--------------------
         GFSHEADO = GFSHEADI
        !--------------------

        IF(JCAP > 0) GFSHEADO%JCAP = JCAP
        GFSHEADO%LEVS = GFSHEADI%DIMZ
        IF(LEVS > 0 .or. levso > 0) GFSHEADO%LEVS = LEVS
        GFSHEADO%LONB = GFSHEADI%DIMX
        GFSHEADO%LATB = GFSHEADI%DIMY
        IF(LONB > 0 .AND. LATB > 0) THEN
          GFSHEADO%LONB = LONB
          GFSHEADO%LATB = LATB
        ENDIF
        NTRACM = GFSHEADO%NTRAC
        NTRACO = GFSHEADO%NTRAC
        IF(NTRAC > 0) GFSHEADO%NTRAC = NTRAC
!       print *,'GFSHEADO%NTRAC=',GFSHEADO%NTRAC
        IF(IDVT > 0 ) THEN
          IF(IDVT == 200)  THEN
            NTRACO = MIN(GFSHEADI%NTRAC,GFSHEADO%NTRAC) + 2
          ELSE IF(IDVT > 200 ) THEN
            NTRACO = GFSHEADO%NTRAC
          ENDIF
          GFSHEADO%NTRAC = NTRACO
        ENDIF
        IF(NTRACO /= GFSHEADI%NTRAC) THEN
          print *,'WARNING: user is outputing different tracers from',
     &   ' input file, please define the tracers name in nemsio header!'
        ENDIF

        IF(IDVC  >  0) GFSHEADO%IDVC = IDVC
        IF(IDVM  >  0) GFSHEADO%IDVM = IDVM
        IF(IDSL  >  0) GFSHEADO%IDSL = IDSL
        IF(IGEN  >  0) GFSHEADO%IGEN = IGEN
        IF(IDVT  >  0) GFSHEADO%IDVT = IDVT
        IF(NCLDT >  0) GFSHEADO%NCLDT = NCLDT
        IF(NCLDT == 0) NCLDT = GFSHEADO%NCLDT
        IF(NVCOORD > 0) GFSHEADO%NVCOORD = NVCOORD
        IF(IVSSIG > 0) GFSHEADO%IVSSIG = IVSSIG
        IF(GFSHEADO%IVSSIG == 0 .or. GFSHEADO%IVSSIG == -9999)
     &           GFSHEADO%IVSSIG = 200509
        IF(IDRT == 0) GFSHEADO%IDRT = IDRT
        GFSHEADO%LONF = GFSHEADO%LONB
        GFSHEADO%LATF = GFSHEADO%LATB
        GFSHEADO%LONR = GFSHEADO%LONB
        GFSHEADO%LATR = GFSHEADO%LATB
        GFSHEADO%DIMX = GFSHEADO%LONB
        GFSHEADO%DIMY = GFSHEADO%LATB
        GFSHEADO%DIMZ = GFSHEADO%LEVS
        LEVSO = GFSHEADO%LEVS
        IMO   = GFSHEADO%LONB
        JMO   = GFSHEADO%LATB
        LONBO = GFSHEADO%LONB
        LATBO = GFSHEADO%LATB
        JCAPO = GFSHEADO%JCAP
        NCO   = (JCAPO+1)*(JCAPO+2)

!     print *,' levs=',levs,' levso=',levso,' GFSHEADO%LEVS='
!    &,GFSHEADO%LEVS,' GFSHEADO%dimz=',GFSHEADO%dimz

!       if (imo == GFSHEADI%LONB .and. jmo == GFSHEADI%LATB) then
!         vert_interp_only = .true.
!         write(0,*)' imo',imo,' jmo=',jmo
!    &,      ' only vertical interpolation done'
!       endif
        thermodyn_id_i = mod( GFSHEADI%IDVM/10,10)
        sfcpress_id_o  = mod( GFSHEADO%IDVM,10)
        thermodyn_id_o = mod( GFSHEADO%IDVM/10,10)
!
        IF(MQUICK == 1) THEN
          IF(LATBO.NE.GFSHEADI%LATB) CALL ERREXIT(24)
          IF(LONBO.NE.GFSHEADI%LONB) CALL ERREXIT(24)
          IF(LEVSO.NE.GFSHEADI%LEVS) CALL ERREXIT(24)
          IF(JCAPO.LT.GFSHEADI%JCAP) CALL ERREXIT(24)
        ENDIF
!
        call instrument(2,kall,ttot,tmin,tmax)

        PRINT '("CHANGE NEMSIO FILE RESOLUTION FROM ",
     &   I4," lonb,",I4," latb,",I4," LEVELS, AND ",I4," TRACERS")',
     &   GFSHEADI%LONB,GFSHEADI%LATB,GFSHEADI%LEVS,GFSHEADI%NTRAC
        PRINT '("                                  ",
     &   " IVS=",I6," IDVC=",I2," NVCOORD=",I2,"  IDVM=",I2)',
     &   GFSHEADI%VERSION,GFSHEADI%IDVC,GFSHEADI%NVCOORD,GFSHEADI%IDVM
        PRINT '("                               TO ",
     &   I4," lonb, ",I4," latb, ",I4," LEVELS, AND ",I4," TRACERS.")',
     &   GFSHEADO%LONB,GFSHEADO%LATB,GFSHEADO%LEVS,GFSHEADO%NTRAC
        PRINT '("                                  ",
     &   " IVS=",I6," IDVC=",I2," NVCOORD=",I2,"  IDVM=",I2)',
     &   GFSHEADO%VERSION,GFSHEADO%IDVC,GFSHEADO%NVCOORD,GFSHEADO%IDVM

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  UPDATE NEMSIO OUTPUT FILE SECOND HEADER
! idea ?
        if (nopdpvv) then
          NREC = 2 + GFSHEADO%LEVS*(3+NTRACO)      !zs,ps,t,u,v,q(ntracer)
        else
          NREC = 2 + GFSHEADO%LEVS*(6+NTRACO)      !zs,ps,p,dp,t,u,v,q(ntracer),vvel
        endif

        GFSHEADO%NREC = NREC
        ALLOCATE(GFSHEADVO%VCOORD(LEVSO+1,3,2))
        ALLOCATE(GFSHEADVO%LAT(IMO*JMO))
        ALLOCATE(GFSHEADVO%LON(IMO*JMO))
        ALLOCATE(GFSHEADVO%CPI(NTRACO+1))
        ALLOCATE(GFSHEADVO%RI(NTRACO+1))
        IF(IMO*JMO == SIZE(GFSHEADVI%LON)) THEN
          GFSHEADVO%LAT = GFSHEADVI%LAT
          GFSHEADVO%LON = GFSHEADVI%LON
        ENDIF
        ALLOCATE(GFSHEADVO%RECNAME(NREC))
        ALLOCATE(GFSHEADVO%RECLEVTYP(NREC))
        ALLOCATE(GFSHEADVO%RECLEV(NREC))
        write(0,*)' NREC=',NREC,' GFSHEADI%NREC=',GFSHEADI%NREC
     &,' GFSHEADO%LEVS=',GFSHEADO%LEVS
        if (GFSHEADO%LEVS == GFSHEADI%LEVS .and. .not. nopdpvv) then
          IF(NREC == GFSHEADI%NREC) THEN
            GFSHEADVO%RECNAME   = GFSHEADVI%RECNAME
            GFSHEADVO%RECLEVTYP = GFSHEADVI%RECLEVTYP
            GFSHEADVO%RECLEV    = GFSHEADVI%RECLEV
!if only add vvel
          ELSEIF(NREC == GFSHEADI%NREC+GFSHEADO%LEVS) THEN
            GFSHEADVO%RECNAME(1:GFSHEADI%NREC)=
     &         GFSHEADVI%RECNAME(1:GFSHEADI%NREC)
            GFSHEADVO%RECNAME(GFSHEADI%NREC+1:NREC)='vvel'
            GFSHEADVO%RECLEVTYP(1:GFSHEADI%NREC)=
     &         GFSHEADVI%RECLEVTYP(1:GFSHEADI%NREC)
            GFSHEADVO%RECLEVTYP(GFSHEADI%NREC+1:NREC)='mid layer'
            GFSHEADVO%RECLEV(1:GFSHEADI%NREC)=
     &         GFSHEADVI%RECLEV(1:GFSHEADI%NREC)
              DO K=1,GFSHEADO%LEVS
                GFSHEADVO%RECLEV(GFSHEADI%NREC+k) = K
              ENDDo
          ELSE
              print *,'WRONG: EXTRA tracers to output, but no field'
     &,               'name specified!!!'
              CALL ERREXIT(26)
          endif
        else
!set recname, reclevtyp,reclev
!         ALLOCATE(GFSHEADVO%RECNAME(NREC))
!         ALLOCATE(GFSHEADVO%RECLEVTYP(NREC))
!         ALLOCATE(GFSHEADVO%RECLEV(NREC))

          GFSHEADVO%RECNAME(1) = 'hgt'
          GFSHEADVO%RECNAME(2) = 'pres'
          if (nopdpvv) then
            do k=1,levso
              GFSHEADVO%RECNAME(2        +k) = 'ugrd'
              GFSHEADVO%RECNAME(2+  LEVSO+k) = 'vgrd'
              GFSHEADVO%RECNAME(2+2*LEVSO+k) = 'tmp'
            enddo
          jrec = 2 + 3*LEVSO
          else
            do k=1,levso
              GFSHEADVO%RECNAME(2        +k) = 'dpres'
              GFSHEADVO%RECNAME(2+  LEVSO+k) = 'pres'
              GFSHEADVO%RECNAME(2+2*levso+k) = 'ugrd'
              GFSHEADVO%RECNAME(2+3*LEVSO+k) = 'vgrd'
              GFSHEADVO%RECNAME(2+4*LEVSO+k) = 'tmp'
            enddo
            jrec = 2 + 5*LEVSO
          endif
!
!set tracer name from list
!
          ALLOCATE(TRAC_NAME(NTRACO))
          TRAC_NAME(1) = 'spfh'
          if (ntraco == 2) then
            IF(IDVT  == 1) THEN
              TRAC_NAME(2) = 'o3mr'
              I_OZN = 2 ; I_CLD = 0
            ELSEIF(IDVT == 2) THEN
              TRAC_NAME(2) = 'clwmr'
              I_OZN = 0 ; I_CLD = 2
            ENDIF
          elseif (ntraco == 3) then
            IF(IDVT == 0) THEN
              TRAC_NAME(2) = 'o3mr'
              TRAC_NAME(3) = 'clwmr'
              I_OZN = 2 ; I_CLD = 0
            ELSEIF(IDVT == 21) THEN
              TRAC_NAME(2) = 'o3mr' ;  TRAC_NAME(3) = 'clwmr'
              I_OZN = 2 ; I_CLD = 3
            ELSEIF(IDVT == 12) THEN
              TRAC_NAME(2) = 'clwmr' ; TRAC_NAME(3) = 'o3mr'
              I_OZN = 3 ; I_CLD = 2
            endif
          elseif (ntraco == 4) then
            TRAC_NAME(4) = 'tke'
            IF(IDVT == 0) THEN
              TRAC_NAME(2) = 'o3mr'
              TRAC_NAME(3) = 'clwmr'
              I_OZN = 2 ; I_CLD = 0
            ELSEIF(IDVT == 21) THEN
              TRAC_NAME(2) = 'o3mr' ;  TRAC_NAME(3) = 'clwmr'
              I_OZN = 2 ; I_CLD = 3
            ELSEIF(IDVT == 12) THEN
              TRAC_NAME(2) = 'clwmr' ; TRAC_NAME(3) = 'o3mr'
              I_OZN = 3 ; I_CLD = 2
            endif
          ELSEIF(IDVT == 100) THEN
            TRAC_NAME(2) = 'clwmr'   ; TRAC_NAME(3) = 'o3mr'
            I_OZN = 2 ; I_CLD = 3
          ELSEIF(IDVT == 200) THEN                             ! for WAM
            TRAC_NAME(2) = 'clwmr'   ; TRAC_NAME(3) = 'o3mr'
            TRAC_NAME(4) = 'o'       ; TRAC_NAME(5) = 'o2'
            I_OZN = 2 ; I_CLD = 3
          ENDIF
          DO N=1,NTRACO
            DO K=1,LEVSO
              GFSHEADVO%RECNAME(jrec+K+(N-1)*LEVSO) = TRAC_NAME(N)
            ENDDO
          ENDDo
          if (.not. nopdpvv) then
            jrec = jrec + NTRACO*LEVSO
            GFSHEADVO%RECNAME((jrec+1):(jrec+LEVSO))='vvel'
          endif
!
          GFSHEADVO%RECLEVTYP(1:2)    = 'sfc'
          GFSHEADVO%RECLEVTYP(3:NREC) = 'mid layer'
          GFSHEADVO%RECLEV(1:2)       = 1
          if (nopdpvv) then
            DO K=1,LEVSO
              GFSHEADVO%RECLEV(2        +K) = K
              GFSHEADVO%RECLEV(2+  LEVSO+K) = K
              GFSHEADVO%RECLEV(2+2*LEVSO+K) = K
            ENDDO
            jrec = 2 + 3*LEVSO
          else
            DO K=1,LEVSO
              GFSHEADVO%RECLEV(2        +K) = K
              GFSHEADVO%RECLEV(2+  LEVSO+K) = K
              GFSHEADVO%RECLEV(2+2*LEVSO+K) = K
              GFSHEADVO%RECLEV(2+3*LEVSO+K) = K
              GFSHEADVO%RECLEV(2+4*LEVSO+K) = K
              GFSHEADVO%RECLEV(2+(5+NTRACO)*LEVSO+K)=K
            ENDDO
            jrec = 2 + 5*LEVSO
          endif
          DO N=1,NTRACO
            DO K=1,LEVSO
              GFSHEADVO%RECLEV(jrec+(n-1)*LEVSO+K) = K
            ENDDO
          ENDDO
        ENDIF
!
!        print *,
!    & 'nmetavari=',GFSHEADO%nmetavari,'nmetavarr=',GFSHEADO%nmetavarr,
!    & 'nmetaaryi=',GFSHEADO%nmetaaryi,'nmetaaryr=',GFSHEADO%nmetaaryr,
!    & 'nrec=',nrec,'gonrec=',GFSHEADO%NREC

        if(GFSHEADO%nmetavari > 0) then
          ALLOCATE(GFSHEADVO%variname(GFSHEADO%nmetavari))
          ALLOCATE(GFSHEADVO%varival(GFSHEADO%nmetavari))
          GFSHEADVO%variname = GFSHEADVI%VARINAME
          GFSHEADVO%varival  = GFSHEADVI%VARIVAL
        else
          ALLOCATE(GFSHEADVO%variname(1))
          ALLOCATE(GFSHEADVO%varival(1))
        endif
        if(GFSHEADO%nmetavarr > 0) then
          ALLOCATE(GFSHEADVO%varrname(GFSHEADO%nmetavarr))
          ALLOCATE(GFSHEADVO%varrval(GFSHEADO%nmetavarr))
          GFSHEADVO%varrname = GFSHEADVI%VARRNAME
          GFSHEADVO%varrval  = GFSHEADVI%VARRVAL
        else
         ALLOCATE(GFSHEADVO%varrname(1))
         ALLOCATE(GFSHEADVO%varrval(1))
        endif
        if(GFSHEADO%nmetavarl > 0) then
          ALLOCATE(GFSHEADVO%varlname(GFSHEADO%nmetavarl))
          ALLOCATE(GFSHEADVO%varlval(GFSHEADO%nmetavarl))
          GFSHEADVO%varlname = GFSHEADVI%VARLNAME
          GFSHEADVO%varlval  = GFSHEADVI%VARLVAL
        else
          ALLOCATE(GFSHEADVO%varlname(1))
          ALLOCATE(GFSHEADVO%varlval(1))
        endif
        if(GFSHEADO%nmetavarc > 0) then
          ALLOCATE(GFSHEADVO%varcname(GFSHEADO%nmetavarc))
          ALLOCATE(GFSHEADVO%varcval(GFSHEADO%nmetavarc))
          GFSHEADVO%varcname = GFSHEADVI%VARCNAME
          GFSHEADVO%varcval  = GFSHEADVI%VARCVAL
        else
          ALLOCATE(GFSHEADVO%varcname(1))
          ALLOCATE(GFSHEADVO%varcval(1))
        endif
        if(GFSHEADO%nmetavarr8 > 0) then
          ALLOCATE(GFSHEADVO%varr8name(GFSHEADO%nmetavarr8))
          ALLOCATE(GFSHEADVO%varr8val(GFSHEADO%nmetavarr8))
          GFSHEADVO%varr8name = GFSHEADVI%VARR8NAME
          GFSHEADVO%varr8val  = GFSHEADVI%VARR8VAL
        else
          ALLOCATE(GFSHEADVO%varr8name(1))
          ALLOCATE(GFSHEADVO%varr8val(1))
        endif
        if(GFSHEADO%nmetaaryi > 0) then
          ALLOCATE(GFSHEADVO%aryiname(GFSHEADO%nmetaaryi))
          ALLOCATE(GFSHEADVO%aryilen(GFSHEADO%nmetaaryi))
          GFSHEADVO%aryiname = GFSHEADVI%ARYINAME
          GFSHEADVO%aryilen  = GFSHEADVI%ARYILEN
        else
          ALLOCATE(GFSHEADVO%aryiname(1))
          ALLOCATE(GFSHEADVO%aryilen(1))
        endif
        if(GFSHEADO%nmetaaryr > 0) then
          ALLOCATE(GFSHEADVO%aryrname(GFSHEADO%nmetaaryr))
          ALLOCATE(GFSHEADVO%aryrlen(GFSHEADO%nmetaaryr))
          GFSHEADVO%aryrname = GFSHEADVI%ARYRNAME
          GFSHEADVO%aryrlen  = GFSHEADVI%ARYRLEN
        else
          ALLOCATE(GFSHEADVO%aryrname(1))
          ALLOCATE(GFSHEADVO%aryrlen(1))
        ENDIF
        if(GFSHEADO%nmetaaryr8 > 0) then
          ALLOCATE(GFSHEADVO%aryr8name(GFSHEADO%nmetaaryr8))
          ALLOCATE(GFSHEADVO%aryr8len(GFSHEADO%nmetaaryr8))
          GFSHEADVO%aryr8name = GFSHEADVI%ARYR8NAME
          GFSHEADVO%aryr8len  = GFSHEADVI%ARYR8LEN
        else
          ALLOCATE(GFSHEADVO%aryr8name(1))
          ALLOCATE(GFSHEADVO%aryr8len(1))
        ENDIF
!
!-- adjust integer variables
!
        do i=1,GFSHEADO%nmetavari
          if(trim(GFSHEADVO%variname(i))  == 'LONB'.or.
     &       trim(GFSHEADVO%variname(i))  == 'lonb')
     &      GFSHEADVO%varival(i) = GFSHEADO%LONB
          if(trim(GFSHEADVO%variname(i)) == 'LATB'.or.
     &       trim(GFSHEADVO%variname(i)) == 'latb')
     &      GFSHEADVO%varival(i) = GFSHEADO%LATB
          if(trim(GFSHEADVO%variname(i)) == 'LATF'.or.
     &       trim(GFSHEADVO%variname(i)) == 'latf')
     &      GFSHEADVO%varival(i) = GFSHEADO%LATB
          if(trim(GFSHEADVO%variname(i)) == 'LATG'.or.
     &       trim(GFSHEADVO%variname(i)) == 'latg')
     &      GFSHEADVO%varival(i) = GFSHEADO%LATB
          if(trim(GFSHEADVO%variname(i)) == 'LONF'.or.
     &       trim(GFSHEADVO%variname(i)) == 'lonf')
     &      GFSHEADVO%varival(i) = GFSHEADO%LONB
          if(trim(GFSHEADVO%variname(i)) == 'LATR'.or.
     &       trim(GFSHEADVO%variname(i)) == 'latr')
     &      GFSHEADVO%varival(i) = GFSHEADO%LATB
          if(trim(GFSHEADVO%variname(i)) == 'LONR'.or.
     &       trim(GFSHEADVO%variname(i)) == 'lonr')
     &      GFSHEADVO%varival(i) = GFSHEADO%LONB
          if(trim(GFSHEADVO%variname(i)) == 'IDVC'.or.
     &       trim(GFSHEADVO%variname(i)) == 'idvc')
     &      GFSHEADVO%varival(i) = GFSHEADO%IDVC
          if(trim(GFSHEADVO%variname(i)) == 'IDVM'.or.
     &       trim(GFSHEADVO%variname(i)) == 'idvm')
     &      GFSHEADVO%varival(i) = GFSHEADO%IDVM
          if(trim(GFSHEADVO%variname(i)) == 'IDSL'.or.
     &       trim(GFSHEADVO%variname(i)) == 'idsl')
     &      GFSHEADVO%varival(i) = GFSHEADO%IDSL
          if(trim(GFSHEADVO%variname(i)) == 'NCLDT'.or.
     &       trim(GFSHEADVO%variname(i)) == 'ncldt')
     &      GFSHEADVO%varival(i) = GFSHEADO%NCLDT
          if(trim(GFSHEADVO%variname(i)) == 'NVCOORD'.or.
     &       trim(GFSHEADVO%variname(i)) == 'nvcoord')
     &      GFSHEADVO%varival(i) = GFSHEADO%NVCOORD
        enddo
!
        if(GFSHEADO%nmetaaryi > 0) then
            if(.not.allocated(GFSHEADVO%aryival) )
     &        ALLOCATE(GFSHEADVO%aryival(maxval(GFSHEADVO%aryilen),
     &                 GFSHEADO%nmetaaryi))
            GFSHEADVO%aryival = GFSHEADVI%aryival
        else
           if(.not.allocated(GFSHEADVO%aryival))
     &         ALLOCATE(GFSHEADVO%aryival(1,1))
        endif
        if(GFSHEADO%nmetaaryr > 0) then
            ALLOCATE(GFSHEADVO%aryrval(maxval(GFSHEADVO%aryrlen),
     &               GFSHEADO%nmetaaryr))
            GFSHEADVO%aryrval = GFSHEADVI%aryrval
        else
            ALLOCATE(GFSHEADVO%aryrval(1,1))
        endif
        if(GFSHEADO%nmetaaryr8 > 0) then
            ALLOCATE(GFSHEADVO%aryr8val(maxval(GFSHEADVO%aryr8len),
     &               GFSHEADO%nmetaaryr8))
            GFSHEADVO%aryr8val = GFSHEADVI%aryr8val
        else
            ALLOCATE(GFSHEADVO%aryr8val(1,1))
        endif
        if(  GFSHEADO%LEVS == GFSHEADI%LEVS )then
           GFSHEADVO%VCOORD = GFSHEADVI%VCOORD
        else
           print *,'output VCOORD from input'
        endif
        if ( GFSHEADO%NTRAC == GFSHEADI%NTRAC) then
           GFSHEADVO%CPI = GFSHEADVI%CPI
           GFSHEADVO%RI  = GFSHEADVI%RI
        else
           print *,'WARNING: You have different Tracers from input,',
     &    ' make sure to provide CPI & RI, for generalized coordinate'
        endif
        cpi(0:ntraco) = GFSHEADVO%CPI(1:ntraco+1)
        ri(0:ntraco)  = GFSHEADVO%RI(1:ntraco+1)

!
        GFSHEADVO%VARINAME(1:GFSHEADO%NMETAVARI)=(/'LATB        '
     &    ,'LONB        ','LEVS        ','ITRUN       ','IORDER      '
     &    ,'IREALF      ','IGEN        ','LATG        ','LONF        '
     &    ,'LATR        ','LONR        ','ICEN2       ','IDPP        '
     &    ,'IDVT        ','IDRUN       ','IDUSR       ','IXGR        '
     &    ,'NVCOORD     ','SFCPRESS_ID ','THERMODYN_ID','IVS         '/)
         GFSHEADVO%VARIVAL(1:GFSHEADO%NMETAVARI)=(/JMO,
     &        IMO,LEVSO,GFSHEADo%ITRUN,GFSHEADo%IORDER,
     &        GFSHEADo%IREALF,GFSHEADo%IGEN,GFSHEADO%LATF,GFSHEADO%LONF,
     &        GFSHEADO%LATR,GFSHEADO%LONR,GFSHEADo%ICEN2,GFSHEADo%IDPP,
     &        GFSHEADo%IDVT,GFSHEADo%IDRUN,GFSHEADo%IDUSR,GFSHEADo%IXGR,
     &        GFSHEADo%NVCOORD,SFCPRESS_ID_O,
     &        THERMODYN_ID_O,GFSHEADo%IVSSIG /)
!        print *,' after GFSHEADVO%VARIVAL LEVSO=',levso
!
!       print *,'after headvo, vcoord=',GFSHEADVO%VCOORD(1:5,1,1)
!     &  ,GFSHEADVO%VCOORD(1:5,2,1),'IMO=',IMO,'JMO=',JMO,'LEVSO=',LEVSO
!     &,'NTRAC=',GFSHEADO%NTRAC,NTRACO,'NTRACM=',NTRACM,'LEVSI=',LEVSI,
!     & 'cpi=',GFSHEADVO%CPI(1:ntraco+1),'ri=',GFSHEADVO%RI(1:ntraco+1)
!     &  ,'in cpi=',GFSHEADVI%CPI(1:ntraco+1),'ri=',
!     &    GFSHEADVI%RI(1:ntraco+1)

!     print *,' bef alllevs=',levs,' levso=',levso,' GFSHEADO%LEVS='
!    &,GFSHEADO%LEVS,' GFSHEADO%dimz=',GFSHEADO%dimz

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ALLOCATE NEMSIO OUTPUT DATA

!       ALLOCATE(GFSDATAO%ZS(IMO,JMO))
!       ALLOCATE(GFSDATAO%PS(IMO,JMO))
!       ALLOCATE(GFSDATAO%T(IMO,JMO,LEVSO))
!       ALLOCATE(GFSDATAO%U(IMO,JMO,LEVSO))
!       ALLOCATE(GFSDATAO%V(IMO,JMO,LEVSO))
!       ALLOCATE(GFSDATAO%Q(IMO,JMO,LEVSO,GFSHEADO%NTRAC))
!       if (.not. nopdpvv) then
!         ALLOCATE(GFSDATAO%P(IMO,JMO,LEVSO))
!         ALLOCATE(GFSDATAO%DP(IMO,JMO,LEVSO))
!         ALLOCATE(GFSDATAO%W(IMO,JMO,LEVSO))
!       endif

!
        mquicknems: IF (MQUICK == 0) THEN
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ALLOCATE TEMPORARY DATA
          ALLOCATE(SLAT(JMO), WLAT(JMO), CLAT(JMO), RLAT(JMO))
          ALLOCATE(OROGO(IMO,JMO))

!         ALLOCATE(OROGI(IMO,JMO), OROGO(IMO,JMO), ZSI(IMO,JMO)
!    &,            PSI(IMO,JMO),   PSO(IMO,JMO),   ZSO(IMO,JMO))
!         ALLOCATE(PI(IMO,JMO,LEVSI),  TI(IMO,JMO,LEVSI)
!    &,            TIV(IMO,JMO,LEVSI), UI(IMO,JMO,LEVSI)
!    &,            VI(IMO,JMO,LEVSI),  WI(IMO,JMO,LEVSI))
!         ALLOCATE(QI(IMO,JMO,LEVSI,NTRACM))
!         ALLOCATE(PO(IMO,JMO,LEVSO), TO(IMO,JMO,LEVSO)
!         ALLOCATE(TO(IMO,LATCH2,LEVSO)
!    &,            UO(IMO,JMO,LEVSO), VO(IMO,JMO,LEVSO))
!    &,            WO(IMO,JMO,LEVSO))
!         ALLOCATE(QO(IMO,JMO,LEVSO,NTRACO))
!         ALLOCATE(TPO(IMO,LEVSO))
!         ALLOCATE(DTDPO(IMO,2*LATCH,LEVSO))
!         ALLOCATE(DPO(IMO,JMO,LEVSO))

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW LATITUDES
          CALL SPLAT(IDRT,JMO,SLAT,WLAT)
          do j=1,jmo
            CLAT(j) = SQRT(1-SLAT(j)*SLAT(j))
            RLAT(j) = pifac * ASIN(SLAT(j))
          enddo
       
          IF(JMO /= GFSHEADI%LATB) THEN
!$omp parallel do private(i,j)
            Do J=1,JMO
              DO I=1,IMO
                GFSHEADVO%LAT(I+(J-1)*IMO) = RLAT(J)
              ENDDO
            ENDDO
          ENDIF
          IF(IMO /= GFSHEADI%LONB) THEN
!$omp parallel do private(i,j)
            Do J=1,JMO
              DO I=1,IMO
                GFSHEADVO%LON(I+(J-1)*IMO) = 360. / imo*(i-1)
              ENDDO
            ENDDO
          ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW OROGRAPHY
          IF(NORO /= 0) THEN
            if (grb_oro) then
              JPDS    = -1
              JPDS(5) = 8
              ALLOCATE(BITMAP(IMO*JMO))
              CALL GETGB(NORO,0,IMO*JMO,0,JPDS,JGDS,
     &                   KF,K,KPDS,KGDS,BITMAP,OROGO,IOSORO)
              DEALLOCATE(BITMAP)
              IF(IOSORO == 0    .AND. (KGDS(1) /= IDRT .OR.
     &           KGDS(2) /= IMO .OR.   KGDS(3) /= JMO)) IOSORO = 100
            else
              allocate (oro4(imo,jmo))
              read(noro) oro4
              orogo = oro4
              deallocate(oro4)
              iosoro = 0
            endif
          ELSE
            IOSORO = 9
          ENDIF
          IF(IOSORO == 0) THEN
            PRINT '("  NEW OROGRAPHY READ IN")'
            if (grb_oro) then
              if (kgds(4) == -90000 .and. kgds(5) == -180000) then
                print *,' reversing the lat/lon for orography'
                call REVERS(imo, jmo, OROGO)
              endif
            endif
          ELSE
            PRINT '("  NEW OROGRAPHY INTERPOLATED FROM OLD")'
          ENDIF
          call instrument(3,kall,ttot,tmin,tmax)
!         print *,'after new orography'
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW SIGMA LEVELS
          IF(NSIL /= 0) THEN
!cggg
            allocate(vcoord(GFSHEADO%LEVS+1,GFSHEADO%NVCOORD))
            CALL NEWSIG(NSIL,GFSHEADO%IDVC,GFSHEADO%LEVS,
     &                  GFSHEADO%NVCOORD,VCOORD,IOSSIL)
            GFSHEADVO%VCOORD(1:GFSHEADO%LEVS+1,1:GFSHEADO%NVCOORD,1) =
     &        vcoord(1:GFSHEADO%LEVS+1,1:GFSHEADO%NVCOORD)
            deallocate(vcoord)
            IF(IOSSIL == 0) THEN
              PRINT '("  NEW MODEL LEVELS READ IN")'
            ENDIF
          ELSEIF(GFSHEADO%IDVC    == GFSHEADI%IDVC .AND.
     &           GFSHEADO%LEVS    == GFSHEADI%LEVS .AND.
     &           GFSHEADO%NVCOORD == GFSHEADI%NVCOORD) THEN
            GFSHEADVO%VCOORD = GFSHEADVI%VCOORD
            IOSSIL = 0
            PRINT '("  NEW MODEL LEVELS COPIED FROM OLD")'
          ELSE
            IOSSIL=42
          ENDIF
          IF(IOSSIL.NE.0) THEN
            PRINT '("  ERROR DEFINING SIGMA VALUES ",I4)',IOSSIL
            CALL ERREXIT(8)
          ENDIF
          call instrument(4,kall,ttot,tmin,tmax)
!
!
!       print *,'bf gl2gl,zs=',maxval(GFSDATAI%ZS),minval(GFSDATAI%ZS),
!     &  'size(zs)=',size(GFSDATAI%ZS,1),size(GFSDATAI%ZS,2),
!     &  'size(zsi)=',size(ZSI,1),size(ZSI,2),
!     &   'imi=',imi,'jm1=',jmi,'imo=',imo,'jmo=',jmo,'idrt=',
!     &    GFSHEADI%IDRT,GFSHEADO%IDRT
!
        ALLOCATE(ZSI(IMO,latch2),      PSI(IMO,latch2))
        ALLOCATE(TI(IMO,latch2,LEVSI), TIV(IMO,latch2,LEVSI)
     &,          UI(IMO,latch2,LEVSI), VI(IMO,latch2,LEVSI)
     &,          PI(IMO,latch2,LEVSI), WI(IMO,latch2,LEVSI)
     &,          QI(IMO,latch2,LEVSI,NTRACM))

        ALLOCATE(GFSDATAO%ZS(IMO,JMO))
        ALLOCATE(GFSDATAO%PS(IMO,JMO))
        ALLOCATE(GFSDATAO%T(IMO,JMO,LEVSO))
        ALLOCATE(GFSDATAO%U(IMO,JMO,LEVSO))
        ALLOCATE(GFSDATAO%V(IMO,JMO,LEVSO))
        ALLOCATE(GFSDATAO%Q(IMO,JMO,LEVSO,GFSHEADO%NTRAC))

       if (.not. nopdpvv) then
          ALLOCATE(GFSDATAO%P(IMO,JMO,LEVSO))
          ALLOCATE(GFSDATAO%DP(IMO,JMO,LEVSO))
          ALLOCATE(GFSDATAO%W(IMO,JMO,LEVSO))
        else
          ALLOCATE(PO(IMO,latch2,LEVSO))
          ALLOCATE(DPO(IMO,latch2,LEVSO))
          ALLOCATE(WO(IMO,latch2,LEVSO))
        endif

        ALLOCATE(DTDPO(IMO,latch2,LEVSO))
!
        call instrument(5,kall,ttot,tmin,tmax)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NEW SURFACE PRESSURE
!--------
! start lat loop
!
!       print *,'bf allocate,input levs=',GFSHEADI%LEVS,'nvcoord=',
!    &    GFSHEADI%NVCOORD,allocated(vcoord)
!    &,' output levels =',GFSHEADO%levs

       allocate(vcoordi(GFSHEADI%LEVS+1,GFSHEADI%NVCOORD))
       vcoordi(1:GFSHEADI%LEVS+1,1:GFSHEADI%NVCOORD) =
     &      GFSHEADVI%VCOORD(1:GFSHEADI%LEVS+1,1:GFSHEADI%NVCOORD,1)
       allocate(vcoord(GFSHEADO%LEVS+1,GFSHEADO%NVCOORD))
            vcoord(1:GFSHEADO%LEVS+1,1:GFSHEADO%NVCOORD) =
     &        GFSHEADVO%VCOORD(1:GFSHEADO%LEVS+1,1:GFSHEADO%NVCOORD,1)
!
       write(0,*)' vcoordi(1) =',vcoordi(1:GFSHEADI%LEVS+1,1)
       write(0,*)' vcoordi(2) =',vcoordi(1:GFSHEADI%LEVS+1,2)

      latloop:   DO J1=1,JMO,LATCH2
!
          J2  = MIN(J1+LATCH2-1, JMO)
          JL  = J2-J1+1
          IJL = IMO*JL
          IJX = IMO*JL
!
!  HORIZONTAL INTERPOLATION
!
!
          CALL GL2GL(2,1,GFSDATAI%ZS,IMI,JMI,ZSI,IMO,JL,
     &              GFSHEADI%IDRT, GFSHEADO%IDRT,rlat(j1),rlat(j2),
     &              JMO)
          CALL GL2GL(2,1,GFSDATAI%PS,IMI,JMI,PSI,IMO,JL,
     &              GFSHEADI%IDRT, GFSHEADO%IDRT,rlat(j1),rlat(j2),
     &              JMO)
          if (.not. nopdpvv) then
            CALL GL2GL(2,LEVSI,GFSDATAI%P,IMI,JMI,PI,IMO,JL,
     &                GFSHEADI%IDRT, GFSHEADO%IDRT,rlat(j1),rlat(j2),
     &                JMO)
          else
            do l=1,levsi
              do j=1,jl
                do i=1,imo
                  pi(i,j,l) = vcoordi(l,1) + vcoordi(l,2)*psi(i,j)
                enddo
              enddo
!           write(0,*)' j=',j,' pi=',pi(1,1,l),'l =',l,' psi=',psi(1,1)
            enddo
          endif
          CALL GL2GL(2,LEVSI,GFSDATAI%T,IMI,JMI,TI,IMO,JL,
     &              GFSHEADI%IDRT, GFSHEADO%IDRT,rlat(j1),rlat(j2),
     &              JMO)
          CALL GL2GL(2,LEVSI,GFSDATAI%U,IMI,JMI,UI,IMO,JL,
     &              GFSHEADI%IDRT, GFSHEADO%IDRT,rlat(j1),rlat(j2),
     &              JMO)
          CALL GL2GL(2,LEVSI,GFSDATAI%V,IMI,JMI,VI,IMO,JL,
     &              GFSHEADI%IDRT, GFSHEADO%IDRT,rlat(j1),rlat(j2),
     &              JMO)
          DO N=1,NTRACM
            CALL GL2GL(2,LEVSI,GFSDATAI%Q(:,:,:,N),IMI,JMI,
     &                QI(:,:,:,N),
     &                IMO,JL,GFSHEADI%IDRT, GFSHEADO%IDRT,rlat(j1),
     &                rlat(j2),JMO)
          ENDDO
          if ( NREC == GFSHEADI%NREC .and. .not. nopdpvv ) then
            CALL GL2GL(2,LEVSI,GFSDATAI%W,IMI,JMI,WI,IMO,JL,
     &                GFSHEADI%IDRT, GFSHEADO%IDRT,rlat(j1),rlat(j2),
     &                JMO)
          else
            WI = 0.
          endif
          TIV = TI*(1.+(461.50/287.05-1)*QI(:,:,:,1))  ! virtual temperature


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  USE NEW OROGRAPHY OR USE TRUNCATED OROGRAPHY

          IF(IOSORO == 0) THEN
            GFSDATAO%ZS(:,j1:j2) = OROGO(:,j1:j2)
          ELSE
            GFSDATAO%ZS(:,j1:j2) = ZSI
            OROGO(:,j1:j2) = GFSDATAO%ZS(:,j1:j2)
          ENDIF

          CALL SIGIO_MODPRD
     &       (IJL,IJX,GFSHEADI%LEVS,GFSHEADI%NVCOORD,
     &        GFSHEADI%IDVC,GFSHEADI%IDSL,VCOORDI,IRET,
     &        PS=PSI,T=TIV,
     &        PM=PI)

!cggg
          TI = TIV / (1.+(461.50/287.05-1)*QI(:,:,:,1))  ! virtual temperature
!
          if(any(TI == 0)) then
             print *,'tmp=0,j1=',j1,'j2=',j2,'tmp=',maxval(TI),
     &       minval(TI)
          endif
          CALL NEWPS(IJL,ZSI,PSI,IJX,GFSHEADI%LEVS,
     &               PI,TI,QI(:,:,:,:),
     &               GFSDATAO%ZS(:,J1:J2),GFSDATAO%PS(:,J1:J2))

!-----------------------------------------------------------------------
!  VERTICALLY INTERPOLATE UPPER-AIR FIELDS
! -- Henry Juang's approach
!cggg
          if (nopdpvv) then
            CALL NEWPR1(IJL,IJX,GFSHEADO%LEVS,GFSHEADI%LEVS,
     &                  GFSHEADO%IDVC,GFSHEADO%IDVM,GFSHEADO%IDSL,
     &                  GFSHEADO%NVCOORD,VCOORD,
     &                  RI, CPI, NTRACM,
     &                  PI,TI,QI,
     &                  GFSDATAO%PS(:,J1:J2), PO, DPO)
!cggg
            call instrument(9,kall,ttot,tmin,tmax)

            CALL VINTG(IJL,IJX,GFSHEADI%LEVS,GFSHEADO%LEVS,NTRACM,
     &                 PI,UI,VI,
     &                 TI,QI,WI,
     &                 PO,                   GFSDATAO%U(:,J1:J2,:),
     &                 GFSDATAO%V(:,J1:J2,:),GFSDATAO%T(:,J1:J2,:),
     &                 GFSDATAO%Q(:,J1:J2,:,:), DTDPO, WO)
          else
            CALL NEWPR1(IJL,IJX,GFSHEADO%LEVS,GFSHEADI%LEVS,
     &                  GFSHEADO%IDVC,GFSHEADO%IDVM,GFSHEADO%IDSL,
     &                  GFSHEADO%NVCOORD,VCOORD,
!cggg&                  GFSHEADO%NVCOORD,GFSHEADVO%VCOORD,
     &                  RI, CPI, NTRACM,
!    &                  GFSHEADVO%RI,GFSHEADVO%CPI,NTRACM,
     &                  PI,TI,QI,
     &                  GFSDATAO%PS(:,J1:J2),GFSDATAO%P(:,J1:J2,:),
     &                  GFSDATAO%DP(:,J1:J2,:))
!cggg
            call instrument(9,kall,ttot,tmin,tmax)

            CALL VINTG(IJL,IJX,GFSHEADI%LEVS,GFSHEADO%LEVS,NTRACM,
     &                 PI,UI,VI,
     &                 TI,QI,WI,
     &                 GFSDATAO%P(:,J1:J2,:),GFSDATAO%U(:,J1:J2,:),
     &                 GFSDATAO%V(:,J1:J2,:),GFSDATAO%T(:,J1:J2,:),
     &                 GFSDATAO%Q(:,J1:J2,:,:),DTDPO,
     &                 GFSDATAO%W(:,J1:J2,:))

          endif
!
!--------------------------------------------------------------------------
          if( GFSHEADO%IDVC  == 3 ) then
!cggg
             allocate(ak(GFSHEADO%LEVS+1), bk(GFSHEADO%LEVS+1)
     &,               ck(GFSHEADO%LEVS+1))

             ak = GFSHEADVO%VCOORD(1:(GFSHEADO%LEVS+1),1,1)
             bk = GFSHEADVO%VCOORD(1:(GFSHEADO%LEVS+1),2,1)
             ck = GFSHEADVO%VCOORD(1:(GFSHEADO%LEVS+1),3,1)

             call checkdp(IJL,IJX,GFSHEADO%LEVS,ak,bk,ck,
     &                    GFSDATAO%PS(:,J1:J2),GFSDATAO%T(:,J1:J2,:),
     &                    GFSDATAO%Q(:,J1:J2,:,:))
             deallocate(ak,bk,ck)
          endif
          call instrument(10,kall,ttot,tmin,tmax)
! -- end of Henry Juang's approach


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  PASS DATA TO NEMSIO
!         IF(OUTTYP == 1 .OR. OUTTYP == 0) THEN
!!$omp parallel do private(i,j)
!jaa          DO J=J1,J2
!jaa            DO I=1,IMO
!jaa              GFSDATAO%ZS(I,J) = ZSO(I,J)
!jaa              GFSDATAO%PS(I,J) = PSO(I,J)
!jaa            ENDDO
!jaa          ENDDO
!!$omp parallel do private(i,j,k,n)
!jaa          DO K=1,GFSHEADO%LEVS
!jaa            DO J=J1,J2
!jaa              DO I=1,IMO
!jaa                GFSDATAO%DP(I,J,K) = DPO(I,J,K)
!jaa                GFSDATAO%P(I,J,K)  = PO(I,J,K)
!jaa                GFSDATAO%T(I,J,K)  = TO(I,J,K)
!jaa                GFSDATAO%U(I,J,K)  = UO(I,J,K)
!jaa                GFSDATAO%V(I,J,K)  = VO(I,J,K)
!jaa                DO N=1,NTRACO
!jaa                  GFSDATAO%Q(I,J,K,N) = QO(I,J,K,N)
!jaa                ENDDO
!jaa                GFSDATAO%W(I,J,K)  = WO(I,J,K)
!jaa              ENDDO
!jaa            ENDDO
!jaa          ENDDO
!           ENDIF
!
            call instrument(11,kall,ttot,tmin,tmax)
!
!---- end lat loop
          enddo latloop

          deallocate(vcoord, vcoordi)

!     print *,' aft lat loop alllevs=',levs,' levso=',levso,
!    &' GFSHEADO%LEVS='
!    &,GFSHEADO%LEVS,' GFSHEADO%dimz=',GFSHEADO%dimz
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  DEALLOCATE TEMPORARY DATA
          DEALLOCATE(GFSDATAI%ZS, GFSDATAI%PS, GFSDATAI%T,
     &               GFSDATAI%U,  GFSDATAI%V,  GFSDATAI%Q)
          if (allocated(GFSDATAI%P))  deallocate(GFSDATAI%P)
          if (allocated(GFSDATAI%DP)) deallocate(GFSDATAI%DP)
          if (allocated(GFSDATAI%W))  deallocate(GFSDATAI%W)
          DEALLOCATE(ZSI, PSI, PI, TI, TIV, UI, VI, WI, QI, DTDPO)
          if (allocated(po))  deallocate(po)
          if (allocated(dpo)) deallocate(dpo)
          if (allocated(wo))  deallocate(wo)

          call instrument(12,kall,ttot,tmin,tmax)
!
        ELSE                          ! mquick /= 0
          ALLOCATE(GFSDATAO%ZS(IMO,JMO))
          ALLOCATE(GFSDATAO%PS(IMO,JMO))
          ALLOCATE(GFSDATAO%T(IMO,JMO,LEVSO))
          ALLOCATE(GFSDATAO%U(IMO,JMO,LEVSO))
          ALLOCATE(GFSDATAO%V(IMO,JMO,LEVSO))
          ALLOCATE(GFSDATAO%Q(IMO,JMO,LEVSO,GFSHEADO%NTRAC))

          GFSDATAO%ZS = GFSDATAI%ZS
          GFSDATAO%PS = GFSDATAI%PS
          GFSDATAO%u  = GFSDATAI%U
          GFSDATAO%V  = GFSDATAI%V
          GFSDATAO%T  = GFSDATAI%T
          GFSDATAO%Q  = GFSDATAI%Q

          if (.not. nopdpvv) then
            ALLOCATE(GFSDATAO%P(IMO,JMO,LEVSO))
            ALLOCATE(GFSDATAO%DP(IMO,JMO,LEVSO))
            ALLOCATE(GFSDATAO%W(IMO,JMO,LEVSO))
            GFSDATAO%DP = GFSDATAI%DP
            GFSDATAO%P  = GFSDATAI%P
            if(NREC == GFSHEADI%NREC) THEN
              GFSDATAO%W = GFSDATAI%W
            else
              GFSDATAO%W = 0.
            endif
          endif

!----------------------------------------------------
          DEALLOCATE(GFSDATAI%ZS, GFSDATAI%PS, GFSDATAI%T,
     &               GFSDATAI%U,  GFSDATAI%V,  GFSDATAI%Q)
          if (allocated(GFSDATAI%P))  deallocate(GFSDATAI%P)
          if (allocated(GFSDATAI%DP)) deallocate(GFSDATAI%DP)
          if (allocated(GFSDATAI%W))  deallocate(GFSDATAI%W)
!
        ENDIF mquicknems
!
        call nemsio_gfs_axheadv(GFSHEADVI)
!
        CALL NEMSIO_CLOSE(GFILEI,IRET=IRET)
!
! ---------------------------------------------------------------------
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ENDIF     !!!END OF INPTYP OPTIONS
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!     print*, ' after inptyp options,outtyp= ',outtyp,'MQUICK=',MQUICK
!    &, ' before  outsig gfsheado%levs=',gfsheado%levs
!    &, ' before  outsig gfsheado%dimz=',gfsheado%dimz
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! WRITE OUT NEMSIO  FILE
!
      OUTSIG = NSIGO > 0 .and. (OUTTYP <= 2 .OR. OUTTYP >= 0) .AND.
     &       (INPTYP == 2 .and. MQUICK == 0 .or. INPTYP == 1)
      IF(OUTSIG) THEN
!
!*** from nemsio -> sigio output
!
        IF(INPTYP == 1 .AND. (OUTTYP == 0 .OR. OUTTYP == 2)) THEN
!
           SIGHEADO%JCAP  = GFSHEADO%JCAP
           SIGHEADO%LATB  = GFSHEADO%LATB
           SIGHEADO%LONB  = GFSHEADO%LONB
           SIGHEADO%LATF  = GFSHEADO%LATB
           SIGHEADO%LONF  = GFSHEADO%LONB
           SIGHEADO%LATR  = GFSHEADO%LATB
           SIGHEADO%LONR  = GFSHEADO%LONB
           SIGHEADO%LEVS  = GFSHEADO%LEVS
           SIGHEADO%NTRAC = GFSHEADO%NTRAC
           SIGHEADO%IDVC  = GFSHEADO%IDVC
           SIGHEADO%IDVM  = GFSHEADO%IDVM
           SIGHEADO%IDSL  = GFSHEADO%IDSL
           SIGHEADO%IGEN  = GFSHEADO%IGEN
           SIGHEADO%IDVT  = GFSHEADO%IDVT
           SIGHEADO%NCLDT = GFSHEADO%NCLDT
           SIGHEADO%IVS   = GFSHEADO%IVSSIG
           SIGHEADO%NVCOORD   = GFSHEADO%NVCOORD
           SIGHEADO%IDATE(1:4) = GFSHEADO%IDATE(1:4)
           SIGHEADO%IDATE(1) = GFSHEADO%IDATE(4)
           SIGHEADO%IDATE(4) = GFSHEADO%IDATE(1)
           SIGHEADO%fhour = GFSHEADO%fhour
           SIGHEADO%itrun = GFSHEADO%itrun
           if(SIGHEADO%itrun==-9999) SIGHEADO%itrun=1
           SIGHEADO%iorder = GFSHEADO%iorder
           if(SIGHEADO%iorder==-9999) SIGHEADO%iorder=2
           SIGHEADO%irealf = GFSHEADO%irealf
           if(SIGHEADO%irealf==-9999) SIGHEADO%irealf=1
           SIGHEADO%igen = GFSHEADO%igen
           if(SIGHEADO%igen==-9999) SIGHEADO%igen=82
           SIGHEADO%icen2 = GFSHEADO%icen2
           if(SIGHEADO%icen2==-9999) SIGHEADO%icen2=0
           SIGHEADO%iens = GFSHEADO%iens
           SIGHEADO%idpp = GFSHEADO%idpp
           if(SIGHEADO%idpp==-9999) SIGHEADO%idpp=0
           SIGHEADO%idrun = GFSHEADO%idrun
           if(SIGHEADO%idrun==-9999) SIGHEADO%idrun=0
           SIGHEADO%idusr = GFSHEADO%idusr
           if(SIGHEADO%idusr==-9999) SIGHEADO%idusr=0
           SIGHEADO%pdryini = GFSHEADO%PDRYINI
           SIGHEADO%ixgr = GFSHEADO%ixgr
           if(SIGHEADO%ixgr==-9999) SIGHEADO%ixgr=0
           print *,'pdryini=',SIGHEADO%pdryini,'iens=',SIGHEADO%iens,
     &    'idusr=',SIGHEADO%idusr,GFSHEADO%idusr,'iens=',SIGHEADO%iens,
     &     'ixgr=',SIGHEADO%ixgr
!
           CALL SIGIO_ALHEAD(SIGHEADO,IRET)
           SIGHEADO%VCOORD(1:SIGHEADO%LEVS+1,1:SIGHEADO%NVCOORD) =
     &       GFSHEADVO%VCOORD(1:SIGHEADO%LEVS+1,1:SIGHEADO%NVCOORD,1)
!           print *,'vcoordo=',GFSHEADVO%VCOORD(1:5,1:2,1),'sigheado=',
!     &      SIGHEADO%VCOORD(1:5,1:2),'nvcoord=',SIGHEADO%NVCOORD,
!     &      'levs=',SIGHEADO%LEVS
           CALL SIGIO_ALDBTA(SIGHEADO,SIGDATAO,IRET)
!  INITIALIZE TEMPORARY OUTPUT DATA
           SIGDATAO%HS = 0
           SIGDATAO%PS = 0
           SIGDATAO%T  = 0
           SIGDATAO%D  = 0
           SIGDATAO%Z  = 0
           SIGDATAO%Q  = 0
!
!   REORDER THE FIELDS FOR SPECTRAL TRANSFORM
           CALL REORD(IMO,JMO,LEVSO,NTRACO,
     &                GFSDATAO%ZS,GFSDATAO%PS,GFSDATAO%T,
     &                GFSDATAO%U,GFSDATAO%V,GFSDATAO%Q )
!
!   TRANSFORM BACK TO THE NEW SPECTRAL SPACE FOR SIGIO SIGMA OUTPUT
           IJX = IMO*JMO
!          print*,' transform back to the new spectral space,t='
!     &   ,gfsdatao%t(1,1:20,2),'q=',gfsdatao%q(1,1:10,2,1),
!     &    'IMO=',IMO,'JMO=',JMO
!     &   ,'IJX=',IJX,'cpi=',cpi,'idvm=',GFSHEADO%IDVM,'ntraco=',
!     &   ntraco
           CALL TRBSC(JCAPO,NCO,LEVSO,NTRACO,GFSHEADO%IDVM,
     &                IDRT,IMO,JMO,IJX,1,(JMO+1)/2,1,cpi,
     &                GFSDATAO%ZS,GFSDATAO%PS,GFSDATAO%T,
     &                GFSDATAO%U,GFSDATAO%V,GFSDATAO%Q,
     &                SIGDATAO%HS,SIGDATAO%PS,SIGDATAO%T,
     &                SIGDATAO%D,SIGDATAO%Z,SIGDATAO%Q)
!
!   WRITE OUT SIGMA FILE
           CALL SIGIO_SWHEAD(NSIGO,SIGHEADO,IRET)
           CALL SIGIO_SWDBTA(NSIGO,SIGHEADO,SIGDATAO,IRET)
           CALL SIGIO_SCLOSE(NSIGO,IRET)
           CALL SIGIO_AXDBTA(SIGDATAO,IRET)
!
        ENDIF
!
!*** for nemsio output from either nemsio or sigio
!
        IF(OUTTYP == 0 .OR. OUTTYP == 1) THEN

          FIELDSIZE = IMO*JMO
! --- fill in NEMSIO header data  ---
          IF (INPTYP == 2) THEN
! idea ?
            if (nopdpvv) then
              NREC = 2 + LEVSO*(3+NTRACO)     !zs,ps,t,u,v,q(ntracer)
            else
              NREC = 2 + LEVSO*(6+NTRACO)     !zs,ps,p,dp,t,u,v,q(ntracer)
            endif
            GFSHEADO%NREC = NREC

!            print *,'nrec=',nrec,'imo=',imo,'jmo=',jmo,
!    &        'fieldsize=',FIELDSIZE,'levso=',levso
!
            ALLOCATE(GFSHEADVO%CPI(NTRACO+1))
            ALLOCATE(GFSHEADVO%RI(NTRACO+1))
!
!** set up meta data
! set up meta data variables
             GFSHEADO%NFHOUR     = INT(FHOURO)
             GFSHEADO%NFMINUTE   = INT((FHOURO-GFSHEADO%NFHOUR)*60)
             GFSHEADO%NFSECONDN  = INT((FHOURO-GFSHEADO%NFHOUR-
     &          GFSHEADO%NFMINUTE/60.)*3600*100)
             GFSHEADO%NFSECONDD  = 100
             GFSHEADO%IDATE(1:6) = 0
             GFSHEADO%IDATE(1:4) = IDATE4O(1:4)
             GFSHEADO%IDATE(1)   = IDATE4O(4)
             GFSHEADO%IDATE(4)   = IDATE4O(1)
             GFSHEADO%IDATE(7)   = 100
             print *,'GFSHEADO%IDATE=',GFSHEADO%IDATE(1:4)
!meta needed by GSI
!other meta variables
             GFSHEADO%EXTRAMETA = .true.
!
             GFSHEADO%NMETAVARI = 20
            ALLOCATE(GFSHEADVO%VARINAME(GFSHEADO%NMETAVARI))
            ALLOCATE(GFSHEADVO%VARIVAL(GFSHEADO%NMETAVARI))
             GFSHEADVO%VARINAME(1:GFSHEADO%NMETAVARI)=(/'LATB        '
     &    ,'LONB        ','LEVS        ','ITRUN       ','IORDER      '
     &    ,'IREALF      ','IGEN        ','LATG        ','LONF        '
     &    ,'LATR        ','LONR        ','ICEN2       ','IDPP        '
     &    ,'IDVT        ','IDRUN       ','IDUSR       ','IXGR        '
     &    ,'NVCOORD     ','SFCPRESS_ID ','THERMODYN_ID','IVS         '/)
             GFSHEADVO%VARIVAL(1:GFSHEADO%NMETAVARI)=(/LATBO,
     &        LONBO,LEVSO,ITRUNO,IORDERO,
     &        IREALFO,IGENO,GFSHEADO%LATF,GFSHEADO%LONF,
     &        GFSHEADO%LATR,GFSHEADO%LONR,ICEN2O,IDPPO,
     &        IDVTO,IDRUNO,IDUSRO,IXGRO,
     &        NVCOORDO,SFCPRESS_ID_O,THERMODYN_ID_O,IVSSIGO /)
!            print *,' after GFSHEADVO%VARIVAL LEVSO=',levso
!
             GFSHEADO%NMETAVARR = 2
             ALLOCATE(GFSHEADVO%VARRNAME(GFSHEADO%NMETAVARR))
             ALLOCATE(GFSHEADVO%VARRVAL(GFSHEADO%NMETAVARR))
             GFSHEADVO%VARRNAME(1:GFSHEADO%NMETAVARR)=(/'PDRYINI',
     &                                                 'fhour  '/)
             GFSHEADVO%VARRVAL(1:GFSHEADO%NMETAVARR)=(/PDRYINIO,fhouro/)
!
             GFSHEADO%NMETAARYI = 1
             ALLOCATE(GFSHEADVO%ARYINAME(GFSHEADO%NMETAARYI))
             ALLOCATE(GFSHEADVO%ARYILEN(GFSHEADO%NMETAARYI))
             GFSHEADVO%ARYINAME(1:GFSHEADO%NMETAARYI)=(/'IENS'/)
             GFSHEADVO%ARYILEN(1:GFSHEADO%NMETAARYI)=2

             ALLOCATE(GFSHEADVO%ARYIVAL(maxval(GFSHEADVO%ARYILEN(1:
     &                GFSHEADO%NMETAARYI)),GFSHEADO%NMETAARYI))
             GFSHEADVO%ARYIVAL(1:GFSHEADVO%ARYILEN(1),1)=IENSO(1:2)
!
! set up meta data arrays
            GFSHEADVO%VCOORD = 0.
            GFSHEADVO%VCOORD(1:LEVSO+1,1:NVCOORDO,1) =
     &            VCOORD_NEW(1:LEVSO+1,1:NVCOORDO)
!
            if (mod(idvmO/10,10) == 3) then
              GFSHEADVO%CPI(1:NTRACO+1) = CPI(0:NTRACO)
              GFSHEADVO%RI(1:NTRACO+1)  = RI(0:NTRACO)
            else
              GFSHEADVO%CPI = -9999.
              GFSHEADVO%RI  = -9999.
            endif
!
! --- end NEMSIO header data  ---
          ENDIF    !input type 2

!set recname, reclevtyp,reclev
          IF (INPTYP == 2) THEN
            ALLOCATE(GFSHEADVO%RECNAME(NREC))
            ALLOCATE(GFSHEADVO%RECLEVTYP(NREC))
            ALLOCATE(GFSHEADVO%RECLEV(NREC))

            GFSHEADVO%RECNAME(1) = 'hgt'
            GFSHEADVO%RECNAME(2) = 'pres'
            if (nopdpvv) then
              do k=1,levso
                GFSHEADVO%RECNAME(2        +k) = 'ugrd'
                GFSHEADVO%RECNAME(2+  LEVSO+k) = 'vgrd'
                GFSHEADVO%RECNAME(2+2*LEVSO+k) = 'tmp'
              enddo
            jrec = 2 + 3*LEVSO
            else
              do k=1,levso
                GFSHEADVO%RECNAME(2        +k) = 'dpres'
                GFSHEADVO%RECNAME(2+  LEVSO+k) = 'pres'
                GFSHEADVO%RECNAME(2+2*LEVSO+k) = 'ugrd'
                GFSHEADVO%RECNAME(2+3*LEVSO+k) = 'vgrd'
                GFSHEADVO%RECNAME(2+4*LEVSO+k) = 'tmp'
              enddo
              jrec = 2 + 5*LEVSO
            endif
!set tracer name from list
            DO N=1,NTRACO
              DO K=1,LEVSO
               GFSHEADVO%RECNAME(jrec+K+(N-1)*LEVSO) = TRAC_NAME(N)
              ENDDO
            ENDDo
            if (.not. nopdpvv) then
              jrec = jrec + NTRACO*LEVSO
              GFSHEADVO%RECNAME((jrec+1):(jrec+LEVSO))='vvel'
            endif
!
            GFSHEADVO%RECLEVTYP(1:2)    = 'sfc'
            GFSHEADVO%RECLEVTYP(3:NREC) = 'mid layer'
            GFSHEADVO%RECLEV(1:2)       = 1
            if (nopdpvv) then
              DO K=1,LEVSO
                GFSHEADVO%RECLEV(2        +K) = K
                GFSHEADVO%RECLEV(2+  LEVSO+K) = K
                GFSHEADVO%RECLEV(2+2*LEVSO+K) = K
              ENDDO
              jrec = 2 + 3*LEVSO
            else
              DO K=1,LEVSO
                GFSHEADVO%RECLEV(2        +K) = K
                GFSHEADVO%RECLEV(2+  LEVSO+K) = K
                GFSHEADVO%RECLEV(2+2*LEVSO+K) = K
                GFSHEADVO%RECLEV(2+3*LEVSO+K) = K
                GFSHEADVO%RECLEV(2+4*LEVSO+K) = K
                GFSHEADVO%RECLEV(2+(5+NTRACO)*LEVSO+K) = K
              ENDDO
              jrec = 2 + 5*LEVSO
            endif
            DO N=1,NTRACO
              DO K=1,LEVSO
                GFSHEADVO%RECLEV(jrec+(n-1)*LEVSO+K) = K
              ENDDO
            ENDDO
          ENDIF   !set recname, reclevtyp,reclev
!

       ! --- end NEMSIO header data  ---

!         print*, ' write out nemsio chgres.out.grd,grdfmt=',grdfmt
!    &,' gfsheado%levs=',gfsheado%levs,' gfsheado%dimz=',gfsheado%dimz
!    &,' outtyp=',outtyp

          CALL NEMSIO_OPEN(GFILEO,TRIM('chgres.out.grd'),'write'
     &,                    MODELNAME="GFS"
     &,                    GDATATYPE=GRDFMT
     &,                    NFHOUR=GFSHEADO%NFHOUR
     &,                    NFMINUTE=GFSHEADO%NFMINUTE
     &,                    NFSECONDN=GFSHEADO%NFSECONDN
     &,                    NFSECONDD=GFSHEADO%NFSECONDD
     &,                    IDATE=GFSHEADO%IDATE
     &,                    NREC=GFSHEADO%NREC
     &,                    DIMX=GFSHEADO%DIMX
     &,                    DIMY=GFSHEADO%DIMY
     &,                    DIMZ=GFSHEADO%DIMZ
     &,                    JCAP=GFSHEADO%JCAP
     &,                    NTRAC=GFSHEADO%NTRAC
     &,                    IDSL=GFSHEADO%IDSL
     &,                    IDVC=GFSHEADO%IDVC
     &,                    IDVM=GFSHEADO%IDVM
     &,                    NCLDT=GFSHEADO%NCLDT
     &,                    IDRT=GFSHEADO%IDRT
     &,                    RECNAME=GFSHEADVO%RECNAME
     &,                    RECLEVTYP=GFSHEADVO%RECLEVTYP
     &,                    RECLEV=GFSHEADVO%RECLEV
     &,                    VCOORD=GFSHEADVO%VCOORD
     &,                    LON=GFSHEADVO%LON
     &,                    LAT=GFSHEADVO%LAT
     &,                    CPI=GFSHEADVO%CPI
     &,                    RI=GFSHEADVO%RI
     &,                    EXTRAMETA=GFSHEADO%EXTRAMETA
     &,                    NMETAVARI=GFSHEADO%NMETAVARI
     &,                    NMETAVARR=GFSHEADO%NMETAVARR
     &,                    NMETAVARR8=GFSHEADO%NMETAVARR8
     &,                    NMETAVARL=GFSHEADO%NMETAVARL
     &,                    NMETAVARC=GFSHEADO%NMETAVARC
     &,                    NMETAARYI=GFSHEADO%NMETAARYI
     &,                    NMETAARYR=GFSHEADO%NMETAARYR
     &,                    NMETAARYR8=GFSHEADO%NMETAARYR8
     &,                    VARINAME=GFSHEADVO%VARINAME
     &,                    VARIVAL=GFSHEADVO%VARIVAL
     &,                    VARRNAME=GFSHEADVO%VARRNAME
     &,                    VARRVAL=GFSHEADVO%VARRVAL
     &,                    VARLNAME=GFSHEADVO%VARLNAME
     &,                    VARLVAL=GFSHEADVO%VARLVAL
     &,                    VARCNAME=GFSHEADVO%VARCNAME
     &,                    VARCVAL=GFSHEADVO%VARCVAL
     &,                    VARR8NAME=GFSHEADVO%VARR8NAME
     &,                    VARR8VAL=GFSHEADVO%VARR8VAL
     &,                    ARYINAME=GFSHEADVO%ARYINAME
     &,                    ARYILEN=GFSHEADVO%ARYILEN
     &,                    ARYIVAL=GFSHEADVO%ARYIVAL
     &,                    ARYRNAME=GFSHEADVO%ARYRNAME
     &,                    ARYRLEN=GFSHEADVO%ARYRLEN
     &,                    ARYRVAL=GFSHEADVO%ARYRVAL
     &,                    ARYR8NAME=GFSHEADVO%ARYR8NAME
     &,                    ARYR8LEN=GFSHEADVO%ARYR8LEN
     &,                    ARYR8VAL=GFSHEADVO%ARYR8VAL
     &,                    IRET=IRET)
          IF(IRET.NE.0) THEN
            PRINT*, ' ERROR AT NEMSIO_OPEN chgres.out.grd,iret=',iret
            CALL ERREXIT(4)
          ENDIF

!
!         call nemsio_getfilehead(gfileo,tlmeta=tlmeta,iret=iret)
!         print *,' output nemsio grd file, tlmeta=',tlmeta
!         print *,'after gfsout,nemsio_open,iret=',iret,'im=',
!    &    GFSHEADO%DIMX,GFSHEADO%DIMY,GFSHEADO%DIMZ,'JCAP=',
!    &    GFSHEADO%JCAP,'idsl=',GFSHEADO%IDSL,'IDRT=',GFSHEADO%IDRT,
!    &   'IDVM=',GFSHEADO%IDVM,'NCLDT=',GFSHEADO%NCLDT,'NDVC=',
!    &    GFSHEADO%IDVC
!
!         JREC = 1    ! why this is needed?
!
!nemsio data
          print *,'wrtgrd, gfsdatao=',maxval(gfsdatao%t(:,:,1)),
     &    minval(gfsdatao%t(:,:,1)),'ugrd=',maxval(gfsdatao%u(:,:,1)),
     &    minval(gfsdatao%u(:,:,1)),'vgrd=',maxval(gfsdatao%v(:,:,1)),
     &    minval(gfsdatao%v(:,:,1)),
!    &    minval(gfsdatao%v(:,:,1)),'w=',maxval(gfsdatao%w(:,:,1)),
!    &    minval(gfsdatao%w(:,:,1)),'spfh=',
     &    'spfh=',
     &    maxval(gfsdatao%Q(:,:,1,1)),minval(gfsdatao%Q(:,:,1,1)),'cw=',
     &    maxval(gfsdatao%Q(:,:,1,3)),minval(gfsdatao%Q(:,:,1,3))

!         call nemsio_gfs_wrtgrd(GFILEO,GFSDATAO,nopdpvv,iret=iret)
          call nemsio_gfs_wrtgrd(GFILEO,GFSDATAO,iret=iret)
          call nemsio_gfs_axgrd(GFSDATAO)
          call nemsio_gfs_axheadv(GFSHEADVO)
          call nemsio_close(GFILEO,IRET=IRET)

          write(0,*)'after nemsio_close gfileo,iret=',iret
!
        ENDIF          ! end OUTTYP

      ENDIF            ! end if outsig

! ---------------------------------------------------------------------
!  CLOSE FILES
      IF(INPTYP /= 0) THEN
        IF(NORO > 0) then
          if (grb_oro) then
           CALL BACLOSE(NORO,IRET)
          else
            close(noro)
          endif
        endif
        IF(NSIL > 0)  CLOSE(NSIL)
        IF(NO3C > 0)  CLOSE(NO3C)
        IF(NO3T > 0)  CALL BACLOSE(NO3T,IRET)
      ENDIF

!  END OF CHANGE RESOLUTION FOR HISTROY FILE
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------

!    resolutioon change for sfc file starts here
!    -------------------------------------------

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OPEN SURFACE FILES
      NSFCI = 21
      NSLM  = 22
      NLPL  = 23
      NSFCO = 61
      CALL SFCIO_SROPEN(NSFCI,'chgres.inp.sfc',IRET)
      CALL SFCIO_SRHEAD(NSFCI,SFCHEADI,IRET1)
      IF(IRET == 0 .and. IRET1 == 0) THEN
        INPTYP = 2
      ELSE
        CALL NEMSIO_OPEN(GFILEISFC,'chgres.inp.sfc','read',IRET=IRET)
        CALL NEMSIO_GETFILEHEAD(GFILEISFC,GTYPE=FILETYPE)
        print *,'open chgres.inp.sfc,iret=',iret, 'gtype=',filetype
        IF (trim(FILETYPE) == 'NEMSIO' .AND. IRET == 0) THEN
          INPTYP = 1
          print *,'open nemsio file'
        ELSE
          INPTYP = 0
          NSFCO  = 0
        ENDIF
      ENDIF

      IF(INPTYP /=0 ) then
        CALL BAOPENR(NSLM,'chgres.inp.slmgb',IRET)
        IF(IRET.NE.0) NSLM=0
        print *,' iret=',iret,' nslm=',nslm
        OPEN(NLPL,FILE='chgres.inp.lonsperlat',
     &       FORM='FORMATTED',STATUS='OLD',IOSTAT=IRET)
        IF(IRET.NE.0) NLPL=0
      ENDIF
!
      call instrument(15,kall,ttot,tmin,tmax)
!
      INQUIRE (FILE="./chgres.inp.nst", EXIST=DO_NSST)
      IF (DO_NSST .AND. NSFCO == 0) THEN
        print*,'fatal error: when converting an nsst restart file,'
        print*,'you must also convert a surface restart file.'
        stop 333
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OPEN SURFACE HEADER
      inptypif: IF(INPTYP==2) THEN
        CALL SFCIO_SRHEAD(NSFCI,SFCHEADI,IRET)

        srheadif: IF(IRET == 0) THEN
!
          LONBO   = SFCHEADI%LONB
          LATBO   = SFCHEADI%LATB
          IVSO    = SFCHEADI%IVS
          LSOILO  = SFCHEADI%LSOIL
          IREALFO = SFCHEADI%IREALF
          IDATE4O = SFCHEADI%IDATE
          FHOURO  = SFCHEADI%FHOUR
          print *,'for sfc file,fhouro=',SFCHEADI%FHOUR
          IF(LONB > 0) LONBO = LONB
          IF(LATB > 0) LATBO = LATB
          IF(IVSSFC > 0)IVSO = IVSSFC

          IF(LSOIL > 0) THEN
            IF(LSOIL == 2 .OR. LSOIL == 4) THEN
              LSOILO = LSOIL
            ELSE
              PRINT '("  NUMBER OF SOIL LAYERS MUST BE 2 OR 4. ")'
              CALL ERREXIT(9)
            ENDIF
          ENDIF
!adjust
          IF (IVSO < 200501) THEN
            LSOILO = 2 ! MUST USE 2 LAYERS.
          ELSE
            allocate(ZSOILO(LSOILO))
            IF (LSOILO == 2) THEN
              ZSOILO =(/-.1, -2.0/)
            ELSE
              ZSOILO =(/-.1, -.4, -1.0, -2.0/)
            END IF
          END IF
!
          SFCHEADO       = SFCHEADI
!
          SFCHEADO%LONB  = LONBO
          SFCHEADO%LATB  = LATBO
          SFCHEADO%IVS   = IVSO
          SFCHEADO%LSOIL = LSOILO
          if (latb > 0 .and. idrt == 0) then
            call sfcio_alhead(sfcheado,latb=latb,iret=iret)
          else
            call sfcio_alhead(sfcheado,iret)
          endif
!ggg start
          if (allocated(sfcheado%zsoil)) deallocate (sfcheado%zsoil)
          ALLOCATE(SFCHEADO%ZSOIL(SFCHEADO%LSOIL))
          SFCHEADO%ZSOIL = ZSOILO
!ggg end
        ELSE
          NSFCO = 0
        ENDIF srheadif

        call instrument(16,kall,ttot,tmin,tmax)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CHANGE RESOLUTION OF SURFACE FILE

        CONVERT_SFC_FILE : IF (NSFCO > 0) THEN
          PRINT '("CHANGE SURFACE FILE RESOLUTION",
     &   " FROM ",I4," X ",I4," X ",I4,"   VERSION",I8)',
     &     SFCHEADI%LONB,SFCHEADI%LATB,SFCHEADI%LSOIL,SFCHEADI%IVS
          PRINT '("                              ",
     &   "   TO ",I4," X ",I4," X ",I4,"   VERSION",I8)',
     &     SFCHEADO%LONB,SFCHEADO%LATB,SFCHEADO%LSOIL,SFCHEADO%IVS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ OLD SURFACE FILE
          CALL SFCIO_ALDBTA(SFCHEADI,SFCDATAI,IRET)
          IF(IRET.NE.0) THEN
            PRINT '("  ERROR ALLOCATING ")'
            CALL ERREXIT(4)
          ENDIF
          CALL SFCIO_ALDBTA(SFCHEADO,SFCDATAO,IRET)
          IF(IRET.NE.0) THEN
            PRINT '("  ERROR ALLOCATING ")'
            CALL ERREXIT(4)
          ENDIF
          CALL SFCIO_SRDBTA(NSFCI,SFCHEADI,SFCDATAI,IRET)
          IF(IRET.NE.0) THEN
            PRINT '("  ERROR READING ")'
            CALL ERREXIT(4)
          ENDIF
          IMI = SFCHEADI%LONB
          JMI = SFCHEADI%LATB
          IMO = SFCHEADO%LONB
          JMO = SFCHEADO%LATB
          call instrument(17,kall,ttot,tmin,tmax)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET OLD AND NEW LAND-SEA MASK.  REMOVE SEA ICE WHICH IS HANDLED 
!  SEPARATELY.
          ALLOCATE(SLMSKI(IMI,JMI))
          SLMSKI=SFCDATAI%SLMSK
          WHERE(NINT(SLMSKI).EQ.2) SLMSKI=0.
          ALLOCATE(SLMSKO(IMO,JMO))
          IF(NSLM > 0) THEN
            JPDS=-1
            JPDS(5)=81
            ALLOCATE(BITMAP(IMO*JMO))
            CALL GETGB(NSLM,0,IMO*JMO,0,JPDS,JGDS,
     &                 KF,K,KPDS,KGDS,BITMAP,SLMSKO,IOSLM)
            DEALLOCATE(BITMAP)
            IF(IOSLM == 0 .AND. (KGDS(1) /= idrt .OR.
     &       KGDS(2) /= IMO .OR. KGDS(3) /= JMO)) IOSLM = 100
          ELSE 
            IOSLM = 100
          ENDIF
          IF(IOSLM == 0) THEN 
            PRINT '("  NEW LAND-SEA MASK READ IN")'
            if (kgds(4) == -90000 .and. kgds(5) == -180000) then
              print *,' reversing the lat/lon for land-sea mask'
              call REVERS(imo, jmo, SLMSKO)
            endif
          ELSE
            CALL GL2GL(2,1,SLMSKI,IMI,JMI,SLMSKO,IMO,JMO,4,IDRT,
     &                    rlat1,rlat2,JMO)
            PRINT '("  NEW LAND-SEA MASK INTERPOLATED FROM OLD")'
          ENDIF
          SLMSKO = NINT(SLMSKO)
          call instrument(18,kall,ttot,tmin,tmax)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW LONSPERLAT
          ALLOCATE(LPLO((LATBO+1)/2))
          IF(NLPL > 0) THEN
            READ(NLPL,*,IOSTAT=IOLPL) LATG2,LPLO
            IF(LATG2 /= (LATBO+1)/2) IOLPL = 99
            print *,'NLPL=',NLPL,'LATG2=',LATG2,'LPLO=',maxval(LPLO)
          ELSE
            IOLPL = 100
          ENDIF
          IF(IOLPL == 0) THEN
            PRINT '("  NEW LONSPERLAT READ IN")'
          ELSE
            IF(LONBO.EQ.SFCHEADI%LONB.AND.
     &        LATBO.EQ.SFCHEADI%LATB.AND.IDRT.EQ.4) THEN
              LPLO=SFCHEADI%LPL
              PRINT '("  NEW LONSPERLAT COPIED FROM OLD")'
            ELSE
              LPLO=LONBO
              PRINT '("  NEW LONSPERLAT ASSUMED TO BE UNIFORM")'
            ENDIF
          ENDIF
          SFCHEADO%LPL=LPLO
          call instrument(19,kall,ttot,tmin,tmax)
          IF(ALL(SFCDATAI%OROG.LE.0)) THEN
            IF (ALLOCATED(HSI)) THEN
              CALL SPTEZ(0,SIGHEADI%JCAP,IDRT,SFCHEADI%LONB,
     &                   SFCHEADI%LATB,HSI,SFCDATAI%OROG,+1)
            ELSEIF(SFCHEADI%LONB==size(GFSDATAI%ZS,1) .and.
     &        SFCHEADI%LATB==size(GFSDATAI%ZS,2) ) THEN
              SFCDATAI%OROG=GFSDATAI%ZS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OLD SURFACE FILES DO NOT HAVE OROG.  NEED TO GET THIS FROM A SIGMA FILE. 
            ELSE
              PRINT '("  NEED OROG ON INPUT GRID FROM SIGMA FILE")'
              CALL ERREXIT(10)
            END IF  
          ENDIF
          IF(ALLOCATED(OROGO)) THEN
            SFCDATAO%OROG = OROGO
          ELSE
            IF(NORO /= 0) THEN
              if (grb_oro) then
                CALL BAOPENR(NORO,'chgres.inp.orogb',IRET)
                IF (IRET == 0) THEN  ! orog file exists.
                  JPDS    = -1
                  JPDS(5) = 8
                  ALLOCATE(BITMAP(IMO*JMO))
                  CALL GETGB(NORO,0,IMO*JMO,0,JPDS,JGDS,
     &                       KF,K,KPDS,KGDS,BITMAP,SFCDATAO%OROG,Iret)
                  DEALLOCATE(BITMAP)
                  PRINT '("  READ grib OROGRAPHY ON OUTPUT GRID")'
                endif
                CALL BACLOSE(NORO,IRET)
              else
                open(noro, file='chgres.inp.orogb', form='unformatted'
     &,                          status='old', iostat=iret)
                if (iret == 0) then
                  allocate (oro4(imo,jmo))
                  read(noro) oro4
                  SFCDATAO%orog = oro4
                  deallocate(oro4)
                endif
                PRINT '("  READ binary OROGRAPHY ON OUTPUT GRID")'
                close(noro)
              endif
              IF (IRET /= 0) THEN  ! bad read. abort here?
               PRINT '("  BAD READ OF OUTPUT GRID OROGRAPHY GRID FILE")'
               PRINT '(" INTERPOLATE OUTPUT OROGRAPHY FROM INPUT GRID")'
               CALL GL2GL(2,1,SFCDATAI%OROG,IMI,JMI,SFCDATAO%OROG,
     &,                      IMO,JMO,4,IDRT,rlat1,rlat2,JMO)
              END IF
            ELSE
             PRINT '("  INTERPOLATE OUTPUT OROGRAPHY FROM INPUT GRID")'
             CALL GL2GL(2,1,SFCDATAI%OROG,IMI,JMI,SFCDATAO%OROG,IMO,
     &                     JMO,4,IDRT,rlat1,rlat2,JMO)
            END IF
          ENDIF
!
!     Open and read unfiltered orography
!

          if (use_ufo .and. noro_uf > 0) then
            ALLOCATE(OROGo_uf(IMO,JMO))
            if (grb_oro) then
              CALL BAOPENR(NORO_uf,'chgres.inp.orogb_uf',IRET)
            else
             open(noro_uf,file='chgres.inp.orogb_uf', form='unformatted'
     &,                          status='old', iostat=iret)
            endif
            IF (IRET /= 0) NORO_uf = 0
!
            if (noro_uf > 0) then
              if (grb_oro) then
                JPDS    = -1
                JPDS(5) = 8
                ALLOCATE(BITMAP(IMO*JMO))
                CALL GETGB(NORO_uf,0,IMO*JMO,0,JPDS,JGDS,
     &                     KF,K,KPDS,KGDS,BITMAP,OROGO_uf,IOSORO_uf)
                DEALLOCATE(BITMAP)
                IF(IOSORO_uf == 0 .AND. (KGDS(1) /= IDRT .OR.
     &          KGDS(2) /= IMO .OR. KGDS(3) /= JMO)) IOSORO_uf = 100
               if (kgds(4) == -90000 .and. kgds(5) == -180000) then
                 print *,' reversing the lat/lon for orography'
                 call REVERS(imo, jmo, OROGO_uf)
                endif
                CALL BACLOSE(NORO_uf,IRET)
              else
                allocate (oro4(imo,jmo))
                read(noro_uf) oro4
                orogo_uf = oro4
                deallocate(oro4)
                iosoro_uf = 0
                close(noro_uf)
              endif
              IF(IOSORO_uf == 0) THEN
                PRINT '("  NEW unfiltered OROGRAPHY READ IN")'
              else
                use_ufo = .false.
              endif
            ELSE
              use_ufo = .false.
            ENDIF
          endif
          if (.not. use_ufo) then
            if (.not. allocated (OROGo_uf)) ALLOCATE(OROGo_uf(IMO,JMO))
            orogo_uf = 0.0
            PRINT '("  unfiltered  OROGRAPHY set to zero")'
          endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  INTERPOLATE SOME SURFACE FIELDS THE OLD WAY.  THESE ARE FIELDS
!  THAT ARE EITHER DIAGNOSTIC OR DO NOT REQUIRE SPECIAL HANDLING
!  BY THE NEW SURFACE CHGRES LOGIC.   

          CALL GL2GL(2,1,SFCDATAI%CV,IMI,JMI,SFCDATAO%CV,IMO,JMO
     &,                 4,IDRT,rlat1,rlat2,JMO)
          CALL GL2GL(2,1,SFCDATAI%CVB,IMI,JMI,SFCDATAO%CVB,IMO,JMO
     &,                 4,IDRT,rlat1,rlat2,JMO)
          CALL GL2GL(2,1,SFCDATAI%CVT,IMI,JMI,SFCDATAO%CVT,IMO,JMO
     &,                 4,IDRT,rlat1,rlat2,JMO)
          CALL GL2GL(2,1,SFCDATAI%F10M,IMI,JMI,SFCDATAO%F10M,IMO,JMO
     &,                 4,IDRT,rlat1,rlat2,JMO)
          CALL GL2GL(2,1,SFCDATAI%T2M,IMI,JMI,SFCDATAO%T2M,IMO,JMO
     &,                 4,IDRT,rlat1,rlat2,JMO)
          CALL GL2GL(2,1,SFCDATAI%Q2M,IMI,JMI,SFCDATAO%Q2M,IMO,JMO
     &,                 4,IDRT,rlat1,rlat2,JMO)
          CALL GL2GL(2,1,SFCDATAI%UUSTAR,IMI,JMI,SFCDATAO%UUSTAR,
     &                  IMO,JMO,4,IDRT,rlat1,rlat2,JMO)
          CALL GL2GL(2,1,SFCDATAI%FFMM,IMI,JMI,SFCDATAO%FFMM,IMO,JMO
     &,                 4,IDRT,rlat1,rlat2,JMO)
          CALL GL2GL(2,1,SFCDATAI%FFHH,IMI,JMI,SFCDATAO%FFHH,IMO,JMO
     &,                 4,IDRT,rlat1,rlat2,JMO)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  PRIOR TO IVS 200501, THESE FIELDS WERE UNDEFINED.  IF OUTPUTTING AN
!  200501 FILE, NEED TO INITIALIZE THESE TO A VALUE.  
!         IF(SFCHEADI%IVS.NE.200501.AND.SFCHEADO%IVS.EQ.200501)THEN
          IF(SFCHEADI%IVS.LT.200501.AND.SFCHEADO%IVS.GE.200501)THEN
            SFCDATAO%TPRCP  = 0. ! SET PRECIP TO ZERO.
            SFCDATAO%SRFLAG = 0. ! SET PRECIP TYPE FLAG TO ZERO (LIQUID).
          ELSE
           CALL GL2GL(2,1,SFCDATAI%TPRCP,IMI,JMI,SFCDATAO%TPRCP,IMO,
     &                   JMO,4,IDRT,rlat1,rlat2,JMO)
           CALL GL2GL(2,1,SFCDATAI%SRFLAG,IMI,JMI,SFCDATAO%SRFLAG,
     &                   IMO,JMO,4,IDRT,rlat1,rlat2,JMO)
          END IF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  PLACE SURFACE DATA INTO DATA STRUCTURE EXPECTED BY SURFACE
!  CHGRES CODE.  FIRST, DO INPUT DATA. 

          ALLOCATE (
     &        SFCINPUT%ALNSF(IMI,JMI),    SFCINPUT%ALNWF(IMI,JMI)
     &,       SFCINPUT%ALVSF(IMI,JMI),    SFCINPUT%ALVWF(IMI,JMI)
     &,       SFCINPUT%CANOPY_MC(IMI,JMI),SFCINPUT%GREENFRC(IMI,JMI)
     &,       SFCINPUT%FACSF(IMI,JMI),    SFCINPUT%FACWF(IMI,JMI)
     &,       SFCINPUT%SKIN_TEMP(IMI,JMI),SFCINPUT%LSMASK(IMI,JMI)
     &,       SFCINPUT%SEA_ICE_FLAG(IMI,JMI)
     &,       SFCINPUT%SNOW_LIQ_EQUIV(IMI,JMI)
     &,       SFCINPUT%Z0(IMI,JMI),       SFCINPUT%OROG(IMI,JMI)
     &,       SFCINPUT%VEG_TYPE(IMI,JMI), SFCINPUT%SOIL_TYPE(IMI,JMI)
     &,       SFCINPUT%SOILM_TOT(IMI,JMI,SFCHEADI%LSOIL)
     &,       SFCINPUT%SOIL_TEMP(IMI,JMI,SFCHEADI%LSOIL)
     *              )

!$omp parallel do private(i,j)
          do j=1,jmi
            do i=1,imi
              SFCINPUT%ALNSF(i,j)        = SFCDATAI%ALNSF(i,j)
              SFCINPUT%ALNWF(i,j)        = SFCDATAI%ALNWF(i,j)
              SFCINPUT%ALVSF(i,j)        = SFCDATAI%ALVSF(i,j)
              SFCINPUT%ALVWF(i,j)        = SFCDATAI%ALVWF(i,j)
              SFCINPUT%CANOPY_MC(i,j)    = SFCDATAI%CANOPY(i,j)
              SFCINPUT%GREENFRC(i,j)     = SFCDATAI%VFRAC(i,j)
              SFCINPUT%FACSF(i,j)        = SFCDATAI%FACSF(i,j)
              SFCINPUT%FACWF(i,j)        = SFCDATAI%FACWF(i,j)
              SFCINPUT%SKIN_TEMP(i,j)    = SFCDATAI%TSEA(i,j)
              SFCINPUT%LSMASK(i,j)       = SLMSKI(i,j)
              SFCINPUT%SEA_ICE_FLAG(i,j) = 0
              SFCINPUT%Z0(i,j)           = SFCDATAI%ZORL(i,j)
              SFCINPUT%OROG(i,j)         = SFCDATAI%OROG(i,j)
              SFCINPUT%VEG_TYPE(i,j)     = NINT(SFCDATAI%VTYPE(i,j))
              SFCINPUT%SOIL_TYPE(i,j)    = NINT(SFCDATAI%STYPE(i,j))

              SFCINPUT%SNOW_LIQ_EQUIV(i,j) = SFCDATAI%SHELEG(i,j)
              if (SFCDATAI%SLMSK(i,j) > 1.99)
     &          SFCINPUT%SEA_ICE_FLAG(i,j) = 1
            enddo
          enddo
          do l=1,SFCHEADI%LSOIL
!$omp parallel do private(i,j)
            do j=1,jmi
              do i=1,imi
                SFCINPUT%SOILM_TOT(i,j,l) = SFCDATAI%SMC(i,j,l)
                SFCINPUT%SOIL_TEMP(i,j,l) = SFCDATAI%STC(i,j,l)
              enddo
            enddo
          enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  THE 200501 VERSION OF THE SURFACE FILE HAS ADDITIONAL FIELDS FOR
!  USE BY NOAH LSM AND NEW SEA ICE PHYSICS, WHILE OLDER VERSIONS
!  DO NOT.  IF THESE VARIABLES ARE NOT ALLOCATED, THE SURFACE CHGRES
!  CODE WILL NOT INTERPOLATE THEM.

          IF (SFCHEADI%IVS >= 200501) then
            ALLOCATE (SFCINPUT%SEA_ICE_FRACT(IMI,JMI)
     &,               SFCINPUT%SEA_ICE_DEPTH(IMI,JMI)
     &,               SFCINPUT%MXSNOW_ALB(IMI,JMI)
     &,               SFCINPUT%SNOW_DEPTH(IMI,JMI)
     &,               SFCINPUT%SLOPE_TYPE(IMI,JMI)
     &,               SFCINPUT%GREENFRC_MAX(IMI,JMI)
     &,               SFCINPUT%GREENFRC_MIN(IMI,JMI)
     &,               SFCINPUT%SOILM_LIQ(IMI,JMI,SFCHEADI%LSOIL))

!$omp parallel do private(i,j)
            do j=1,jmi
              do i=1,imi
                SFCINPUT%SEA_ICE_FRACT(i,j) = SFCDATAI%FICE(i,j)
                SFCINPUT%SEA_ICE_DEPTH(i,j) = SFCDATAI%HICE(i,j)
                SFCINPUT%MXSNOW_ALB(i,j)    = SFCDATAI%SNOALB(i,j)
                SFCINPUT%SNOW_DEPTH(i,j)    = SFCDATAI%SNWDPH(i,j)
                SFCINPUT%SLOPE_TYPE(i,j)    = NINT(SFCDATAI%SLOPE(i,j))
                SFCINPUT%GREENFRC_MAX(i,j)  = SFCDATAI%SHDMAX(i,j)
                SFCINPUT%GREENFRC_MIN(i,j)  = SFCDATAI%SHDMIN(i,j)
              enddo
            enddo
            do l=1,sfcheadi%lsoil
!$omp parallel do private(i,j)
              do j=1,jmi
                do i=1,imi
                  SFCINPUT%SOILM_LIQ(i,j,l)     = SFCDATAI%SLC(i,j,l)
                enddo
              enddo
            enddo
          END IF
          CALL SFCIO_AXDBTA(SFCDATAI,IRET)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  NOW ALLOCATE OUTPUT DATA STRUCTURE.  SURFACE CHGRES LOGIC NEEDS
!  THE LAND MASK AND OROGRAPHY ON OUTPUT GRID.  USE THE LONSPERLAT
!  INFO TO RUN EITHER THE REDUCED OR FULL GRID.
          IJMO = SUM(LPLO)*2              ! NUMBER OF GRID POINTS
                                          ! ACCOUNTING FOR REDUCED GRID. 
          if (idrt == 0) ijmo = imo * jmo
          ALLOCATE(SFCOUTPUT%OROG(IJMO))
          ALLOCATE (KMSK(IMO,JMO))
          ALLOCATE(OROGO_uf2(IMO,JMO))
          KMSK = 0
          CALL INTERPRED(1,KMSK,SFCDATAO%OROG,SFCOUTPUT%OROG,
     &                   IMO,JMO,IJMO,LPLO)

          CALL INTERPRED(1,KMSK,OROGO_uf,OROGO_uf2,
     &                   IMO,JMO,IJMO,LPLO)

          ALLOCATE(SFCOUTPUT%LSMASK(IJMO))
          CALL INTERPRED(1,KMSK,SLMSKO,SFCOUTPUT%LSMASK,
     &                   IMO,JMO,IJMO,LPLO)

          ALLOCATE(SFCOUTPUT%LATS(IJMO))
          ALLOCATE(SFCOUTPUT%LONS(IJMO))
          CALL LATLONS(JMO, IJMO, LPLO,
     &                 SFCOUTPUT%LATS, SFCOUTPUT%LONS, IDRT) 
          ALLOCATE(SFCOUTPUT%ALNSF(IJMO))
          ALLOCATE(SFCOUTPUT%ALNWF(IJMO))
          ALLOCATE(SFCOUTPUT%ALVSF(IJMO))
          ALLOCATE(SFCOUTPUT%ALVWF(IJMO))
          ALLOCATE(SFCOUTPUT%CANOPY_MC(IJMO))
          ALLOCATE(SFCOUTPUT%FACSF(IJMO))
          ALLOCATE(SFCOUTPUT%FACWF(IJMO))
          ALLOCATE(SFCOUTPUT%GREENFRC(IJMO))
          ALLOCATE(SFCOUTPUT%SUBSTRATE_TEMP(IJMO))
          ALLOCATE(SFCOUTPUT%SKIN_TEMP(IJMO))
          ALLOCATE(SFCOUTPUT%SNOW_LIQ_EQUIV(IJMO))
          ALLOCATE(SFCOUTPUT%Z0(IJMO))
          ALLOCATE(SFCOUTPUT%SOILM_TOT(IJMO,LSOILO))
          ALLOCATE(SFCOUTPUT%SOIL_TEMP(IJMO,LSOILO))
          ALLOCATE(SFCOUTPUT%VEG_TYPE(IJMO))
          ALLOCATE(SFCOUTPUT%SOIL_TYPE(IJMO))
          ALLOCATE(SFCOUTPUT%SEA_ICE_FLAG(IJMO))
          IF (SFCHEADO%IVS >= 200501) THEN
            ALLOCATE(SFCOUTPUT%SLOPE_TYPE(IJMO))
            ALLOCATE(SFCOUTPUT%SEA_ICE_FRACT(IJMO))
            ALLOCATE(SFCOUTPUT%SEA_ICE_DEPTH(IJMO))
            ALLOCATE(SFCOUTPUT%SOILM_LIQ(IJMO,LSOILO))
            ALLOCATE(SFCOUTPUT%SNOW_DEPTH(IJMO))
            ALLOCATE(SFCOUTPUT%MXSNOW_ALB(IJMO))
            ALLOCATE(SFCOUTPUT%GREENFRC_MAX(IJMO))
            ALLOCATE(SFCOUTPUT%GREENFRC_MIN(IJMO))
          END IF
          IF (SFCHEADO%IVS >= 200509) then
            ALLOCATE (SFCOUTPUT%SEA_ICE_TEMP(IJMO))
          END IF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  INFO DESCRIBING BOTH GRIDS.
          KGDS_INPUT = 0
          KGDS_INPUT(1) = 4          ! OCT 6 - TYPE OF GRID (GAUSSIAN)
          KGDS_INPUT(2) = IMI        ! OCT 7-8 - # PTS ON LATITUDE CIRCLE
          KGDS_INPUT(3) = JMI        ! OCT 9-10 - # PTS ON LONGITUDE CIRCLE
          KGDS_INPUT(4) = 90000      ! OCT 11-13 - LAT OF ORIGIN
          KGDS_INPUT(5) = 0          ! OCT 14-16 - LON OF ORIGIN
          KGDS_INPUT(6) = 128        ! OCT 17 - RESOLUTION FLAG
          KGDS_INPUT(7) = -90000     ! OCT 18-20 - LAT OF EXTREME POINT
          KGDS_INPUT(8) = NINT(-360000./IMI)  ! OCT 21-23 - LON OF EXTREME POINT
          KGDS_INPUT(9)  = NINT((360.0 / FLOAT(IMI))*1000.0)
                                   ! OCT 24-25 - LONGITUDE DIRECTION INCR.
          KGDS_INPUT(10) = JMI /2    ! OCT 26-27 - NUMBER OF CIRCLES POLE TO EQUATOR
          KGDS_INPUT(12) = 255       ! OCT 29 - RESERVED
          KGDS_INPUT(20) = 255       ! OCT 5  - NOT USED, SET TO 255
  
          KGDS_OUTPUT = 0
          KGDS_OUTPUT(1) = IDRT       ! OCT 6 - TYPE OF GRID (GAUSSIAN)
          KGDS_OUTPUT(2) = IMO        ! OCT 7-8 - # PTS ON LATITUDE CIRCLE
          KGDS_OUTPUT(3) = JMO        ! OCT 9-10 - # PTS ON LONGITUDE CIRCLE
          KGDS_OUTPUT(4) = 90000      ! OCT 11-13 - LAT OF ORIGIN
          KGDS_OUTPUT(5) = 0          ! OCT 14-16 - LON OF ORIGIN
          KGDS_OUTPUT(6) = 128        ! OCT 17 - RESOLUTION FLAG
          KGDS_OUTPUT(7) = -90000     ! OCT 18-20 - LAT OF EXTREME POINT
          KGDS_OUTPUT(8) = NINT(-360000./IMO)  ! OCT 21-23 - LON OF EXTREME POINT
          KGDS_OUTPUT(9)  = NINT((360.0 / FLOAT(IMO))*1000.0)
                                  ! OCT 24-25 - LONGITUDE DIRECTION INCR.
          KGDS_OUTPUT(10) = JMO /2    ! OCT 26-27 - NUMBER OF CIRCLES POLE TO EQUATOR
          KGDS_OUTPUT(12) = 255       ! OCT 29 - RESERVED
          KGDS_OUTPUT(20) = 255       ! OCT 5  - NOT USED, SET TO 255
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CALL SURFACE CHGRES DRIVER.
          FCSTHOUR = SFCHEADI%FHOUR
          CALL SURFACE_CHGRES_DRIVER(IMO,JMO,IJMO,LSOILO,LPLO,
     &                               KGDS_OUTPUT,SFCOUTPUT,IMI,JMI,
     &                               orogo_uf2,use_ufo,nst_anl,
     &                               SFCHEADI%LSOIL,
     &                               SFCHEADI%IDATE(1),
     &                               SFCHEADI%IDATE(2),
     &                               SFCHEADI%IDATE(3),
     &                               SFCHEADI%IDATE(4), FCSTHOUR,
     &                               KGDS_INPUT, SFCINPUT, IALB,
     &                               ISOT, IVEGSRC, MERGE, IRET)
          IF (IRET  /= 0) THEN
            PRINT '("  ERROR IN SURFACE CHGRES DRIVER ")'
            CALL ERREXIT(34)
          END IF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GO FROM REDUCED, 1-D ARRAYS TO 2-D FOR OUTPUT.
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%LSMASK,SLMSKO,
     &                     IMO,JMO,IJMO,LPLO)
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%OROG,SFCDATAO%OROG,
     &                     IMO,JMO,IJMO,LPLO)
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%ALNSF,SFCDATAO%ALNSF,
     &                   IMO,JMO,IJMO,LPLO)
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%ALNWF,SFCDATAO%ALNWF,
     &                     IMO,JMO,IJMO,LPLO)
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%ALVSF,SFCDATAO%ALVSF,
     &                     IMO,JMO,IJMO,LPLO)
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%ALVWF,SFCDATAO%ALVWF,
     &                     IMO,JMO,IJMO,LPLO)
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%GREENFRC,SFCDATAO%VFRAC,
     &                     IMO,JMO,IJMO,LPLO)
          IF (ALLOCATED(SFCOUTPUT%GREENFRC_MAX)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%GREENFRC_MAX,SFCDATAO%SHDMAX
     &,                    IMO,JMO,IJMO,LPLO)
          END IF
          IF (ALLOCATED(SFCOUTPUT%GREENFRC_MIN)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%GREENFRC_MIN,SFCDATAO%SHDMIN
     &,                    IMO,JMO,IJMO,LPLO)
          END IF
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%Z0,SFCDATAO%ZORL,
     &                     IMO,JMO,IJMO,LPLO)
!         call hhmaxmin(SFCOUTPUT%SUBSTRATE_TEMP,IMO,jmo,jmo,1,' TG3  ' )
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SUBSTRATE_TEMP,SFCDATAO%TG3,
     &                     IMO,JMO,IJMO,LPLO)
          IF (ALLOCATED (SFCOUTPUT%MXSNOW_ALB)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%MXSNOW_ALB,SFCDATAO%SNOALB,
     &                     IMO,JMO,IJMO,LPLO)
          END IF
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%CANOPY_MC,SFCDATAO%CANOPY,
     &                     IMO,JMO,IJMO,LPLO)
          IF (ALLOCATED(SFCOUTPUT%SEA_ICE_FRACT)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SEA_ICE_FRACT,SFCDATAO%FICE,
     &                     IMO,JMO,IJMO,LPLO)
          END IF
          IF (ALLOCATED(SFCOUTPUT%SEA_ICE_DEPTH)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SEA_ICE_DEPTH,SFCDATAO%HICE,
     &                     IMO,JMO,IJMO,LPLO)
          END IF
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%FACSF,SFCDATAO%FACSF,
     &                     IMO,JMO,IJMO,LPLO)
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%FACWF,SFCDATAO%FACWF,
     &                   IMO,JMO,IJMO,LPLO)
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SKIN_TEMP,SFCDATAO%TSEA,
     &                     IMO,JMO,IJMO,LPLO)
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SNOW_LIQ_EQUIV,
     &                     SFCDATAO%SHELEG,IMO,JMO,IJMO,LPLO)
          IF (ALLOCATED (SFCOUTPUT%SNOW_DEPTH)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SNOW_DEPTH,SFCDATAO%SNWDPH,
     &                     IMO,JMO,IJMO,LPLO)
          END IF
          DO K=1, SFCHEADO%LSOIL
            CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SOILM_TOT(:,K),
     &                       SFCDATAO%SMC(:,:,K),
     &                       IMO,JMO,IJMO,LPLO)
            IF (ALLOCATED(SFCOUTPUT%SOILM_LIQ)) THEN
              CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SOILM_LIQ(:,K),  
     &                         SFCDATAO%SLC(:,:,K),
     &                         IMO,JMO,IJMO,LPLO)
            END IF
            CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SOIL_TEMP(:,K), 
     &                       SFCDATAO%STC(:,:,K),
     &                       IMO,JMO,IJMO,LPLO)
          ENDDO
!
          IF (ALLOCATED(SFCOUTPUT%SEA_ICE_TEMP)) THEN
            CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SEA_ICE_TEMP,
     &                       SFCDATAO%TISFC,IMO,JMO,IJMO,LPLO)
          END IF
!
          ALLOCATE(DUMMY(IJMO))
          IF (ALLOCATED(SFCOUTPUT%SLOPE_TYPE)) THEN
            DUMMY=FLOAT(SFCOUTPUT%SLOPE_TYPE)
            CALL UNINTERPRED(1,KMSK,DUMMY,SFCDATAO%SLOPE,
     &                       IMO,JMO,IJMO,LPLO)
          END IF
          DUMMY=FLOAT(SFCOUTPUT%SOIL_TYPE)
          CALL UNINTERPRED(1,KMSK,DUMMY,SFCDATAO%STYPE,
     &                     IMO,JMO,IJMO,LPLO)
          DUMMY=FLOAT(SFCOUTPUT%VEG_TYPE)
          CALL UNINTERPRED(1,KMSK,DUMMY,SFCDATAO%VTYPE,
     &                     IMO,JMO,IJMO,LPLO)
          DUMMY=FLOAT(SFCOUTPUT%SEA_ICE_FLAG)
          ALLOCATE(DUMMY2(IMO,JMO))
          CALL UNINTERPRED(1,KMSK,DUMMY,DUMMY2,
     &                     IMO,JMO,IJMO,LPLO)
          WHERE (NINT(DUMMY2)==1)SLMSKO=2.0
          SFCDATAO%SLMSK=SLMSKO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  FREE UP MEMORY.
          DEALLOCATE (DUMMY2)
          DEALLOCATE (DUMMY)
          DEALLOCATE (KMSK)
          IF (DO_NSST) THEN
            ALLOCATE(RLATS_OUTPUT(IJMO))
            RLATS_OUTPUT=SFCOUTPUT%LATS
            ALLOCATE(RLONS_OUTPUT(IJMO))
            RLONS_OUTPUT=SFCOUTPUT%LONS
            ALLOCATE(MASK_OUTPUT(IJMO))
            MASK_OUTPUT=SFCOUTPUT%LSMASK
            WHERE(SFCOUTPUT%SEA_ICE_FLAG==1) MASK_OUTPUT=2
            ALLOCATE(NSST_OUTPUT_THIN(IJMO,NUM_NSST_FIELDS))
          ENDIF
          CALL SURFACE_CHGRES_AX2D(SFCINPUT)
          CALL SURFACE_CHGRES_AX1D(SFCOUTPUT)
          call instrument(20,kall,ttot,tmin,tmax)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  WRITE THE NEW SURFACE FILE
          IF(OUTTYP == 2.or.OUTTYP==0) THEN
            DEALLOCATE(SLMSKI)
            DEALLOCATE(SLMSKO)
            PRINT*,"- WRITE SURFACE DATA TO SFCIO FILE."
            CALL SFCIO_SWOPEN(NSFCO,'chgres.out.sfc',IRET)
            IF(IRET.GE.0) THEN
              CALL SFCIO_SWHEAD(NSFCO,SFCHEADO,IRET)
              CALL SFCIO_SWDBTA(NSFCO,SFCHEADO,SFCDATAO,IRET)
!             CALL SFCIO_AXDBTA(SFCDATAO,IRET)
              CALL SFCIO_SCLOSE(NSFCI,IRET)
              IF(NSLM.GT.0) CALL BACLOSE(NSLM,IRET)
              IF(NLPL.GT.0) CLOSE(NLPL)
              CALL SFCIO_SCLOSE(NSFCO,IRET)
            ENDIF
            call instrument(21,kall,ttot,tmin,tmax)
          ENDIF
          CONVERT_NSST_FILE : IF (DO_NSST) THEN
            print*,'- CONVERT NSST FILE chgres.inp.nst.'
C  OPEN NSST FILES
            NSSTI=31
            CALL NSTIO_SROPEN(NSSTI,'chgres.inp.nst',IRET)
            IF(IRET/=0)THEN
              print*,'fatal error opening chgres.inp.nst ', iret
              stop 334
            ENDIF
            CALL NSTIO_SRHEAD(NSSTI,NSST_IN_HEAD,IRET)
            IF(IRET/=0)THEN
              print*,'fatal error reading chgres.inp.nst ', iret
                stop 335
            ENDIF
            CALL NSTIO_ALDATA(NSST_IN_HEAD,NSST_IN_DATA,IRET)
            CALL NSTIO_SRDATA(NSSTI,NSST_IN_HEAD,NSST_IN_DATA,IRET)
            IF(IRET/=0)THEN
              print*,'fatal error reading chgres.inp.nst ', iret
              stop 336
            ENDIF
            ALLOCATE(NSST_INPUT(IMI,JMI,NUM_NSST_FIELDS))
            NSST_INPUT(:,:,1)=NSST_IN_DATA%XT
            NSST_INPUT(:,:,2)=NSST_IN_DATA%XS
            NSST_INPUT(:,:,3)=NSST_IN_DATA%XU
            NSST_INPUT(:,:,4)=NSST_IN_DATA%XV
            NSST_INPUT(:,:,5)=NSST_IN_DATA%XZ
            NSST_INPUT(:,:,6)=NSST_IN_DATA%ZM
            NSST_INPUT(:,:,7)=NSST_IN_DATA%XTTS
            NSST_INPUT(:,:,8)=NSST_IN_DATA%XZTS
            NSST_INPUT(:,:,9)=NSST_IN_DATA%DT_COOL
            NSST_INPUT(:,:,10)=NSST_IN_DATA%Z_C
            NSST_INPUT(:,:,11)=NSST_IN_DATA%C_0
            NSST_INPUT(:,:,12)=NSST_IN_DATA%C_D
            NSST_INPUT(:,:,13)=NSST_IN_DATA%W_0
            NSST_INPUT(:,:,14)=NSST_IN_DATA%W_D
            NSST_INPUT(:,:,15)=NSST_IN_DATA%D_CONV
            NSST_INPUT(:,:,16)=NSST_IN_DATA%IFD
            NSST_INPUT(:,:,17)=NSST_IN_DATA%TREF
            NSST_INPUT(:,:,18)=NSST_IN_DATA%QRAIN
            ALLOCATE(MASK_INPUT(IMI,JMI))
            MASK_INPUT=NSST_IN_DATA%SLMSK
            CALL NSTIO_AXDATA(NSST_IN_DATA,IRET)
            CALL NSTIO_SRCLOSE(NSSTI,IRET)
            PRINT*,"- CHANGE NSST FILE RESOLUTION FROM ",IMI, " X ",JMI
            PRINT*,"                                TO ",IMO, " X ",JMO
            CALL NSST_CHGRES(IMI, JMI, MASK_OUTPUT,
     &                       IJMO, KGDS_INPUT, NSST_INPUT, MASK_INPUT,
     &                       NSST_OUTPUT_THIN, NUM_NSST_FIELDS,
     &                       KGDS_OUTPUT, 
     &                       RLATS_OUTPUT, RLONS_OUTPUT)
            DEALLOCATE(RLATS_OUTPUT, RLONS_OUTPUT,NSST_INPUT,MASK_INPUT)
            NSST_YEAR=NSST_IN_HEAD%IDATE(4)
            NSST_MON=NSST_IN_HEAD%IDATE(2)
            NSST_DAY=NSST_IN_HEAD%IDATE(3)
            NSST_HOUR=NSST_IN_HEAD%IDATE(1)
            NSST_FHOUR=NSST_IN_HEAD%FHOUR
            IF (OUTTYP==2.OR.OUTTYP==0)THEN
              CALL WRITE_NSST_TO_NSTIO(NSST_YEAR,NSST_MON,NSST_DAY,
     &                          NSST_HOUR,NSST_FHOUR,LPLO,IMO,JMO,IJMO,
     &                          MASK_OUTPUT,NSST_OUTPUT_THIN,
     &                          NUM_NSST_FIELDS)
            ENDIF
            IF (OUTTYP==1.OR.OUTTYP==0)THEN
              CALL WRITE_NSST_TO_NEMSIO(IMO,JMO,IJMO,NSST_YEAR,NSST_MON,
     &             NSST_DAY,NSST_HOUR,NSST_FHOUR,MASK_OUTPUT,
     &             NSST_OUTPUT_THIN,NUM_NSST_FIELDS,LPLO)
            ENDIF
            DEALLOCATE(NSST_OUTPUT_THIN)
            DEALLOCATE(MASK_OUTPUT)
          ENDIF CONVERT_NSST_FILE
        ENDIF CONVERT_SFC_FILE
!
!-- elseif nsfcoif read in from nemsio file
!************************************************************************

      ELSEIF (INPTYP == 1) THEN
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET SURFACE HEADER
!--getlpl and zsoil
        CALL NEMSIO_GETFILEHEAD(GFILEISFC
     &,          IDATE=GFSHEADI%IDATE
     &,          NFHOUR=GFSHEADI%NFHOUR
     &,          NFMINUTE=GFSHEADI%NFMINUTE
     &,          NFSECONDN=GFSHEADI%NFSECONDN
     &,          NFSECONDD=GFSHEADI%NFSECONDD
     &,          VERSION=GFSHEADI%VERSION
     &,          NREC=GFSHEADI%NREC
     &,          DIMX=GFSHEADI%LONB
     &,          DIMY=GFSHEADI%LATB
     &,          DIMZ=GFSHEADI%DIMZ
     &,          NSOIL=GFSHEADI%NSOIL
     &,          extrameta=GFSHEADI%extrameta
     &,          nmetavari=GFSHEADI%nmetavari
     &,          nmetavarr=GFSHEADI%nmetavarr
     &,          nmetaaryi=GFSHEADI%nmetaaryi
     &,          nmetaaryr=GFSHEADI%nmetaaryr
     &,          IRET=IRET)

        CALL NEMSIO_GETHEADVAR(GFILEISFC,'irealf',GFSHEADI%irealf,IRET)
        ALLOCATE(GFSHEADVI%LPL((GFSHEADI%LATB+1)/2) )
        ALLOCATE(GFSHEADVI%ZSOIL(GFSHEADI%NSOIL))
        CALL NEMSIO_GETHEADVAR(GFILEISFC,'lpl',GFSHEADVI%LPL,IRET)
        CALL NEMSIO_GETHEADVAR(GFILEISFC,'zsoil',GFSHEADVI%ZSOIL,IRET)
        CALL NEMSIO_GETHEADVAR(GFILEISFC,'IVSSFC',GFSHEADI%IVS,IRET)
        CALL NEMSIO_GETHEADVAR(GFILEISFC,'fhour',GFSHEADI%fhour,IRET)
        IF(IRET/=0) GFSHEADI%FHOUR=real(GFSHEADI%NFHOUR,8)
     &      +real(GFSHEADI%NFMINUTE,8)/60.
     &      +real(GFSHEADI%NFSECONDN,8)/(3600.*GFSHEADI%NFSECONDD)
!
        ALLOCATE(GFSHEADVI%RECNAME(GFSHEADI%NREC))
        ALLOCATE(GFSHEADVI%RECLEVTYP(GFSHEADI%NREC))
        ALLOCATE(GFSHEADVI%RECLEV(GFSHEADI%NREC))
        ALLOCATE(GFSHEADVI%variname(GFSHEADI%nmetavari))
        ALLOCATE(GFSHEADVI%varrname(GFSHEADI%nmetavarr))
        ALLOCATE(GFSHEADVI%varival(GFSHEADI%nmetavari))
        ALLOCATE(GFSHEADVI%varrval(GFSHEADI%nmetavarr))
        ALLOCATE(GFSHEADVI%aryiname(GFSHEADI%nmetaaryi))
        ALLOCATE(GFSHEADVI%aryrname(GFSHEADI%nmetaaryr))
        ALLOCATE(GFSHEADVI%aryilen(GFSHEADI%nmetaaryi))
        ALLOCATE(GFSHEADVI%aryrlen(GFSHEADI%nmetaaryr))
!
        CALL NEMSIO_GETFILEHEAD(GFILEISFC
     &,      RECNAME=GFSHEADVI%RECNAME
     &,      RECLEVTYP=GFSHEADVI%RECLEVTYP
     &,      RECLEV=GFSHEADVI%RECLEV
     &,      variname=GFSHEADVI%variname
     &,      varrname=GFSHEADVI%varrname
     &,      varival=GFSHEADVI%varival
     &,      varrval=GFSHEADVI%varrval
     &,      aryiname=GFSHEADVI%aryiname
     &,      aryrname=GFSHEADVI%aryrname
     &,      aryilen=GFSHEADVI%aryilen
     &,      aryrlen=GFSHEADVI%aryrlen
     &,      IRET=IRET1)
        if(GFSHEADI%nmetaaryi>0) then
          ALLOCATE(GFSHEADVI%aryival(maxval(GFSHEADVI%aryilen),
     &    GFSHEADI%nmetaaryi))
          CALL NEMSIO_GETFILEHEAD(GFILEISFC,aryival=GFSHEADVI%aryival,
     &     iret=iret)
        endif
        if(GFSHEADI%nmetaaryr>0) then
          ALLOCATE(GFSHEADVI%aryrval(maxval(GFSHEADVI%aryrlen),
     &     GFSHEADI%nmetaaryr))
          CALL NEMSIO_GETFILEHEAD(GFILEISFC,aryrval=GFSHEADVI%aryrval,
     &         iret=iret)
        endif
!** for nemsio sfc output
        GFSHEADO=GFSHEADI
!
        IF(LONB.GT.0) GFSHEADO%LONB=LONB
        IF(LATB.GT.0) GFSHEADO%LATB=LATB
        GFSHEADO%LONR=GFSHEADO%LONB
        GFSHEADO%LATR=GFSHEADO%LATB
        IF(IVSSFC.GT.0)GFSHEADO%IVS=IVSSFC
        GFSHEADO%DIMX=GFSHEADO%LONB
        GFSHEADO%DIMY=GFSHEADO%LATB
        GFSHEADO%DIMZ=1
!
        IF(GFSHEADO%NREC==GFSHEADI%NREC ) THEN
          if(allocated(GFSHEADVO%RECNAME))deallocate(GFSHEADVO%RECNAME)
          if(allocated(GFSHEADVO%RECLEVTYP))
     &           deallocate(GFSHEADVO%RECLEVTYP)
          if(allocated(GFSHEADVO%RECLEV))deallocate(GFSHEADVO%RECLEV)
          ALLOCATE(GFSHEADVO%RECNAME(GFSHEADI%NREC))
          ALLOCATE(GFSHEADVO%RECLEVTYP(GFSHEADI%NREC))
          ALLOCATE(GFSHEADVO%RECLEV(GFSHEADI%NREC))
          GFSHEADVO%RECNAME=GFSHEADVI%RECNAME
          GFSHEADVO%RECLEVTYP=GFSHEADVI%RECLEVTYP
          GFSHEADVO%RECLEV=GFSHEADVI%RECLEV
        ENDif
        GFSHEADO%extrameta=GFSHEADI%extrameta
        GFSHEADO%nmetavari=GFSHEADI%nmetavari
        GFSHEADO%nmetavarr=GFSHEADI%nmetavarr
        GFSHEADO%nmetavarr8=GFSHEADI%nmetavarr8
        GFSHEADO%nmetaaryi=GFSHEADI%nmetaaryi
        GFSHEADO%nmetaaryr=GFSHEADI%nmetaaryr
        GFSHEADO%nmetaaryr8=GFSHEADI%nmetaaryr8
!
        if(GFSHEADO%nmetavari>0) then
          if(ALLOCATED(GFSHEADVO%variname))
     &                                   deallocate(GFSHEADVO%variname)
          if(ALLOCATED(GFSHEADVO%varival))deallocate(GFSHEADVO%varival)
          ALLOCATE(GFSHEADVO%variname(GFSHEADO%nmetavari))
          ALLOCATE(GFSHEADVO%varival(GFSHEADO%nmetavari))
          GFSHEADVO%variname=GFSHEADVI%variname
          GFSHEADVO%varival=GFSHEADVI%varival
        endif
        if(GFSHEADO%nmetavarr>0) then
          if(ALLOCATED(GFSHEADVO%varrname))
     &                                   deallocate(GFSHEADVO%varrname)
          if(ALLOCATED(GFSHEADVO%varrval))deallocate(GFSHEADVO%varrval)
          ALLOCATE(GFSHEADVO%varrname(GFSHEADO%nmetavarr))
          ALLOCATE(GFSHEADVO%varrval(GFSHEADO%nmetavarr))
          GFSHEADVO%varrname=GFSHEADVI%varrname
          GFSHEADVO%varrval=GFSHEADVI%varrval
        endif
        if(GFSHEADO%nmetavarr8>0) then
          if(ALLOCATED(GFSHEADVO%varr8name))
     &                                 deallocate(GFSHEADVO%varr8name)
          if(ALLOCATED(GFSHEADVO%varr8val))
     &                                 deallocate(GFSHEADVO%varr8val)
          ALLOCATE(GFSHEADVO%varr8name(GFSHEADO%nmetavarr8))
          ALLOCATE(GFSHEADVO%varr8val(GFSHEADO%nmetavarr8))
          GFSHEADVO%varr8name=GFSHEADVI%varr8name
          GFSHEADVO%varr8val=GFSHEADVI%varr8val
        endif
        if(GFSHEADO%nmetaaryi>0) then
          if(ALLOCATED(GFSHEADVO%aryiname))
     &                                  deallocate(GFSHEADVO%aryiname)
          if(ALLOCATED(GFSHEADVO%aryilen))deallocate(GFSHEADVO%aryilen)
          if(ALLOCATED(GFSHEADVO%aryival))deallocate(GFSHEADVO%aryival)
          ALLOCATE(GFSHEADVO%aryiname(GFSHEADO%nmetaaryi))
          ALLOCATE(GFSHEADVO%aryilen(GFSHEADO%nmetaaryi))
          GFSHEADVO%aryiname=GFSHEADVI%aryiname
          GFSHEADVO%aryilen=GFSHEADVI%aryilen
          ALLOCATE(GFSHEADVO%aryival(maxval(GFSHEADVO%aryilen),
     &      GFSHEADO%nmetaaryi))
          GFSHEADVO%aryival=GFSHEADVI%aryival
        endif
        if(GFSHEADO%nmetaaryr>0) then
          if(ALLOCATED(GFSHEADVO%aryrname))
     &                                   deallocate(GFSHEADVO%aryrname)
          if(ALLOCATED(GFSHEADVO%aryrlen))deallocate(GFSHEADVO%aryrlen)
          if(ALLOCATED(GFSHEADVO%aryrval))deallocate(GFSHEADVO%aryrval)
          ALLOCATE(GFSHEADVO%aryrname(GFSHEADO%nmetaaryr))
          ALLOCATE(GFSHEADVO%aryrlen(GFSHEADO%nmetaaryr))
          GFSHEADVO%aryrname=GFSHEADVI%aryrname
          GFSHEADVO%aryrlen=GFSHEADVI%aryrlen
          ALLOCATE(GFSHEADVO%aryrval(maxval(GFSHEADVO%aryrlen),
     &      GFSHEADO%nmetaaryr))
          GFSHEADVO%aryrval=GFSHEADVI%aryrval
        endif
        if(GFSHEADO%nmetaaryr8>0) then
          if(ALLOCATED(GFSHEADVO%aryr8name))
     &      deallocate(GFSHEADVO%aryr8name)
          if(ALLOCATED(GFSHEADVO%aryr8len))
     &                                   deallocate(GFSHEADVO%aryr8len)
          if(ALLOCATED(GFSHEADVO%aryr8val))
     &                                   deallocate(GFSHEADVO%aryr8val)
          ALLOCATE(GFSHEADVO%aryr8name(GFSHEADO%nmetaaryr8))
          ALLOCATE(GFSHEADVO%aryr8len(GFSHEADO%nmetaaryr8))
          GFSHEADVO%aryr8name=GFSHEADVI%aryr8name
          GFSHEADVO%aryr8len=GFSHEADVI%aryr8len
          ALLOCATE(GFSHEADVO%aryr8val(maxval(GFSHEADVO%aryr8len),
     &      GFSHEADO%nmetaaryr8))
          GFSHEADVO%aryr8val=GFSHEADVI%aryr8val
        endif
!
        IMI = GFSHEADI%LONB
        JMI = GFSHEADI%LATB
        IMO = GFSHEADO%LONB
        JMO = GFSHEADO%LATB
!
        if(allocated(GFSHEADVO%LPL) ) deallocate(GFSHEADVO%LPL)
        if (latb .gt. 0 .and. idrt == 0) then
          ALLOCATE(GFSHEADVO%LPL((LATB+1)/2) )
        else
          ALLOCATE(GFSHEADVO%LPL((GFSHEADO%LATB+1)/2) )
        endif
        if(GFSHEADO%LATB==GFSHEADI%LATB) GFSHEADVO%LPL=GFSHEADVI%LPL
!
        ALLOCATE(GFSHEADVO%ZSOIL(GFSHEADO%NSOIL))
        GFSHEADVO%ZSOIL=GFSHEADVI%ZSOIL
!
        IF(LSOIL.GT.0) THEN
          IF(LSOIL.EQ.2.OR.LSOIL.EQ.4)THEN
             GFSHEADO%NSOIL=LSOIL
!ggg start
             if (allocated(GFSHEADVO%ZSOIL))
     &           deallocate (GFSHEADVO%ZSOIL)
             ALLOCATE(GFSHEADVO%ZSOIL(GFSHEADO%NSOIL))
!ggg end
          ELSE
              PRINT '("  NUMBER OF SOIL LAYERS MUST BE 2 OR 4. ")'
              CALL ERREXIT(9)
          ENDIF
        ENDIF
        IF (GFSHEADO%IVS < 200501) THEN
           GFSHEADO%NSOIL=2 ! MUST USE 2 LAYERS.
        ELSE
          IF (GFSHEADO%NSOIL == 2) THEN
            GFSHEADVO%ZSOIL =(/-.1, -2.0/)
          ELSE
            GFSHEADVO%ZSOIL =(/-.1, -.4, -1.0, -2.0/)
          END IF
        END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CHANGE RESOLUTION OF SURFACE FILE
        PRINT '("CHANGE NEMSIO FILE SURFACE RESOLUTION",
     &   " FROM ",I4," X ",I4," X ",I4,"   VERSION",I8)',
     &   GFSHEADI%LONB,GFSHEADI%LATB,GFSHEADI%NSOIL,GFSHEADI%IVS
        PRINT '("                              ",
     &   "   TO ",I4," X ",I4," X ",I4,"   VERSION",I8)',
     &   GFSHEADO%LONB,GFSHEADO%LATB,GFSHEADO%NSOIL,GFSHEADO%IVS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ OLD SURFACE FILE
        call nemsio_gfs_alsfc(IMI,JMI,GFSHEADI%NSOIL,GFSDATAI)
        call nemsio_gfs_alsfc(IMO,JMO,GFSHEADO%NSOIL,GFSDATAO)
!
!read input sfc data
        call nemsio_gfs_rdsfc(GFILEISFC,GFSDATAI,iret)
        call instrument(17,kall,ttot,tmin,tmax)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET OLD AND NEW LAND-SEA MASK.  REMOVE SEA ICE WHICH IS HANDLED
!  SEPARATELY.
        ALLOCATE(SLMSKI(IMI,JMI))
        SLMSKI=GFSDATAI%SLMSK
        WHERE(NINT(SLMSKI).EQ.2) SLMSKI=0.
        ALLOCATE(SLMSKO(IMO,JMO))
        IF(NSLM.GT.0) THEN
          JPDS=-1
          JPDS(5)=81
          ALLOCATE(BITMAP(IMO*JMO))
          CALL GETGB(NSLM,0,IMO*JMO,0,JPDS,JGDS,
     &               KF,K,KPDS,KGDS,BITMAP,SLMSKO,IOSLM)
          IF(IOSLM .EQ. 0 .AND. (KGDS(1) .NE. idrt .OR.
     &       KGDS(2) .NE. IMO .OR. KGDS(3) .NE. JMO)) IOSLM = 100
          DEALLOCATE(BITMAP)
        ELSE
          IOSLM=100
        ENDIF
        IF(IOSLM.EQ.0) THEN
          PRINT '("  NEW LAND-SEA MASK READ IN")'
          if (kgds(4) == -90000 .and. kgds(5) == -180000) then
            print *,' reversing the lat/lon for land-sea mask'
            call REVERS(imo, jmo, SLMSKO)
          endif
        ELSE
          CALL GL2GL(2,1,SLMSKI,IMI,JMI,SLMSKO,IMO,JMO,4,IDRT,
     &                  rlat1,rlat2,JMO)
          PRINT '("  NEW LAND-SEA MASK INTERPOLATED FROM OLD")'
        ENDIF
        SLMSKO=NINT(SLMSKO)
        call instrument(18,kall,ttot,tmin,tmax)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW LONSPERLAT
        IF(NLPL.GT.0) THEN
          READ(NLPL,*,IOSTAT=IOLPL) LATG2,GFSHEADVO%LPL
          IF(LATG2.NE.GFSHEADO%LATB/2) IOLPL=99
        ELSE
          IOLPL=100
        ENDIF
        IF(IOLPL.EQ.0) THEN
          PRINT '("  NEW LONSPERLAT READ IN")'
        ELSE
          IF(GFSHEADO%LONB.EQ.GFSHEADI%LONB.AND.
     &      GFSHEADO%LATB.EQ.GFSHEADI%LATB.AND.IDRT.EQ.4) THEN
            GFSHEADVO%LPL=GFSHEADVI%LPL
            PRINT '("  NEW LONSPERLAT COPIED FROM OLD")'
          ELSE
            GFSHEADVO%LPL=GFSHEADO%LONB
            PRINT '("  NEW LONSPERLAT ASSUMED TO BE UNIFORM")'
          ENDIF
        ENDIF
        call instrument(19,kall,ttot,tmin,tmax)
        IF(ALL(GFSDATAI%OROG <= 0)) THEN
          IF(ALLOCATED(HSI)) THEN
            CALL SPTEZ(0,SIGHEADI%JCAP,IDRT,SFCHEADI%LONB,SFCHEADI%LATB,
     &                 HSI,SFCDATAI%OROG,+1)
          ELSEIF(SFCHEADI%LONB == size(GFSDATAI%ZS,1) .and.
     &           SFCHEADI%LATB == size(GFSDATAI%ZS,2) ) THEN
            SFCDATAI%OROG = GFSDATAI%ZS
!jw         CALL SPTEZ(0,GFSHEADI%JCAP,IDRT,GFSHEADI%LONB,GFSHEADI%LATB,
!jw     &              HSI,GFSDATAI%OROG,+1)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OLD SURFACE FILES DO NOT HAVE OROG.  NEED TO GET THIS FROM  A SIGMA FILE.
          ELSE
            PRINT '("  NEED OROG ON INPUT GRID FROM SIGMA FILE")'
            CALL ERREXIT(10)
          END IF
        ENDIF
        IF(ALLOCATED(OROGO)) THEN
          GFSDATAO%OROG = OROGO
        ELSE
          if (grb_oro) then
            CALL BAOPENR(NORO,'chgres.inp.orogb',IRET)
            if (iret == 0) then
              JPDS    = -1
              JPDS(5) = 8
              ALLOCATE(BITMAP(IMO*JMO))
              CALL GETGB(NORO,0,IMO*JMO,0,JPDS,JGDS,
     &                   KF,K,KPDS,KGDS,BITMAP,GFSDATAO%OROG,Iret)
              DEALLOCATE(BITMAP)
              IF (IRET == 0) THEN  ! orog file exists.
                PRINT '("  READ grib OROGRAPHY ON OUTPUT GRID")'
              endif
              CALL BACLOSE(NORO,IRET)
            else
              open(noro, file='chgres.inp.orogb', form='unformatted'
     &,                        status='old', iostat=iret)
              allocate (oro4(imo,jmo))
              read(noro) oro4
              GFSDATAO%orog = oro4
              deallocate(oro4)
              iret = 0
              PRINT '("  READ binary OROGRAPHY ON OUTPUT GRID")'
              CLOSE(NORO)
            endif
          endif
          IF (IRET /= 0) THEN  ! bad read. abort here?
            PRINT '("  BAD READ OF OUTPUT GRID OROGRAPHY GRID FILE")'
            PRINT '("  INTERPOLATE OUTPUT OROGRAPHY FROM INPUT GRID")'
            CALL GL2GL(2,1,GFSDATAI%OROG,IMI,JMI,
     &                 GFSDATAO%OROG,IMO,JMO,4,IDRT,rlat1,rlat2,JMO)
          ELSE
            PRINT '("  INTERPOLATE OUTPUT OROGRAPHY FROM INPUT GRID")'
            CALL GL2GL(2,1,GFSDATAI%OROG,IMI,JMI,
     &                 GFSDATAO%OROG,IMO,JMO,4,IDRT,rlat1,rlat2,JMO)
          END IF
        ENDIF
!
!     Open and read unfiltered orography
!

        if (use_ufo .and. noro_uf > 0) then
         if (.not. allocated (OROGo_uf)) ALLOCATE(OROGo_uf(IMO,JMO))
          if (grb_oro) then
            CALL BAOPENR(NORO_uf,'chgres.inp.orogb_uf',IRET)
          else
           open(noro_uf,file='chgres.inp.orogb_uf', form='unformatted'
     &,                        status='old', iostat=iret)
          endif
          IF (IRET /= 0) NORO_uf = 0
!
          if (noro_uf > 0) then
            if (grb_oro) then
              JPDS    = -1
              JPDS(5) = 8
              ALLOCATE(BITMAP(IMO*JMO))
              CALL GETGB(NORO_uf,0,IMO*JMO,0,JPDS,JGDS,
     &                   KF,K,KPDS,KGDS,BITMAP,OROGO_uf,IOSORO_uf)
              DEALLOCATE(BITMAP)
              IF(IOSORO_uf == 0 .AND. (KGDS(1) /= IDRT .OR.
     &            KGDS(2) /= IMO .OR. KGDS(3) /= JMO)) IOSORO_uf = 100
              if (kgds(4) == -90000 .and. kgds(5) == -180000) then
                 print *,' reversing the lat/lon for orography'
                 call REVERS(imo, jmo, OROGO_uf)
              endif
              CALL BACLOSE(NORO_uf,IRET)
            else
              allocate (oro4(imo,jmo))
              read(noro_uf) oro4
              orogo_uf = oro4
              deallocate(oro4)
              iosoro_uf = 0
              close(noro_uf)
            endif
            IF(IOSORO_uf == 0) THEN
              PRINT '("  NEW unfiltered OROGRAPHY READ IN")'
            else
              use_ufo = .false.
            endif
          ELSE
            use_ufo = .false.
          ENDIF
        endif
        if (.not. use_ufo) then
          if (.not. allocated (OROGo_uf)) ALLOCATE(OROGo_uf(IMO,JMO))
          orogo_uf = 0.0
          PRINT '("  unfiltered  OROGRAPHY set to zero")'
        endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  INTERPOLATE SOME SURFACE FIELDS THE OLD WAY.  THESE ARE FIELDS
!  THAT ARE EITHER DIAGNOSTIC OR DO NOT REQUIRE SPECIAL HANDLING
!  BY THE NEW SURFACE CHGRES LOGIC.
!jw, from Moorthi nemsio not output cv,cvb,cvt
!       CALL GL2GL(2,1,GFSDATAI%CV,IMI,JMI,GFSDATAO%CV,IMO,JMO
!    &,                                                         4,IDRT)
!       CALL GL2GL(2,1,GFSDATAI%CVB,IMI,JMI,GFSDATAO%CVB,IMO,JMO
!    &,                                                         4,IDRT)
!       CALL GL2GL(2,1,GFSDATAI%CVT,IMI,JMI,GFSDATAO%CVT,IMO,JMO
!    &,                                                         4,IDRT)
        CALL GL2GL(2,1,GFSDATAI%F10M,IMI,JMI,GFSDATAO%F10M,IMO,JMO
     &,               4,IDRT,rlat1,rlat2,JMO)
        CALL GL2GL(2,1,GFSDATAI%T2M,IMI,JMI,GFSDATAO%T2M,IMO,JMO
     &,               4,IDRT,rlat1,rlat2,JMO)
        CALL GL2GL(2,1,GFSDATAI%Q2M,IMI,JMI,GFSDATAO%Q2M,IMO,JMO
     &,               4,IDRT,rlat1,rlat2,JMO)
        CALL GL2GL(2,1,GFSDATAI%UUSTAR,IMI,JMI,GFSDATAO%UUSTAR,IMO
     &,               JMO,4,IDRT,rlat1,rlat2,JMO)
        CALL GL2GL(2,1,GFSDATAI%FFMM,IMI,JMI,GFSDATAO%FFMM,IMO,JMO
     &,               4,IDRT,rlat1,rlat2,JMO)
        CALL GL2GL(2,1,GFSDATAI%FFHH,IMI,JMI,GFSDATAO%FFHH,IMO,JMO
     &,               4,IDRT,rlat1,rlat2,JMO)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  PRIOR TO IVS 200501, THESE FIELDS WERE UNDEFINED.  IF OUTPUTTING AN
!  200501 FILE, NEED TO INITIALIZE THESE TO A VALUE.
!       IF(SFCHEADI%IVS.NE.200501.AND.SFCHEADO%IVS.EQ.200501)THEN
        IF(GFSHEADI%IVS.LT.200501.AND.GFSHEADO%IVS.GE.200501)THEN
          GFSDATAO%TPRCP  = 0. ! SET PRECIP TO ZERO.
          GFSDATAO%SRFLAG = 0. ! SET PRECIP TYPE FLAG TO ZERO (LIQUID).
        ELSE
         CALL GL2GL(2,1,GFSDATAI%TPRCP,IMI,JMI,GFSDATAO%TPRCP,IMO,JMO
     &,                4,IDRT,rlat1,rlat2,JMO)
         CALL GL2GL(2,1,GFSDATAI%SRFLAG,IMI,JMI,GFSDATAO%SRFLAG,IMO
     &,                JMO,4,IDRT,rlat1,rlat2,JMO)
        END IF
!       print *,'datao srflag,tprcp=',maxval(GFSDATAO%TPRCP),
!     &     minval(GFSDATAO%TPRCP)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  PLACE SURFACE DATA INTO DATA STRUCTURE EXPECTED BY SURFACE
!  CHGRES CODE.  FIRST, DO INPUT DATA.
        ALLOCATE (SFCINPUT%ALNSF(IMI,JMI))
        ALLOCATE (SFCINPUT%ALNWF(IMI,JMI))
        ALLOCATE (SFCINPUT%ALVSF(IMI,JMI))
        ALLOCATE (SFCINPUT%ALVWF(IMI,JMI))
        ALLOCATE (SFCINPUT%CANOPY_MC(IMI,JMI))
        ALLOCATE (SFCINPUT%GREENFRC(IMI,JMI))
        ALLOCATE (SFCINPUT%FACSF(IMI,JMI))
        ALLOCATE (SFCINPUT%FACWF(IMI,JMI))
        ALLOCATE (SFCINPUT%SKIN_TEMP(IMI,JMI))
        ALLOCATE (SFCINPUT%LSMASK(IMI,JMI))
        ALLOCATE (SFCINPUT%SEA_ICE_FLAG(IMI,JMI))
        ALLOCATE (SFCINPUT%SNOW_LIQ_EQUIV(IMI,JMI))
        ALLOCATE (SFCINPUT%Z0(IMI,JMI))
        ALLOCATE (SFCINPUT%OROG(IMI,JMI))
        ALLOCATE (SFCINPUT%VEG_TYPE(IMI,JMI))
        ALLOCATE (SFCINPUT%SOIL_TYPE(IMI,JMI))
        ALLOCATE (SFCINPUT%SOILM_TOT(IMI,JMI,GFSHEADI%NSOIL))
        ALLOCATE (SFCINPUT%SOIL_TEMP(IMI,JMI,GFSHEADI%NSOIL))
!
!$omp parallel do private(i,j)
        do j=1,jmi
          do i=1,imi
            SFCINPUT%ALNSF(i,j)          = GFSDATAI%ALNSF(i,j)
            SFCINPUT%ALNWF(i,j)          = GFSDATAI%ALNWF(i,j)
            SFCINPUT%ALVSF(i,j)          = GFSDATAI%ALVSF(i,j)
            SFCINPUT%ALVWF(i,j)          = GFSDATAI%ALVWF(i,j)
            SFCINPUT%CANOPY_MC(i,j)      = GFSDATAI%CANOPY(i,j)
            SFCINPUT%GREENFRC(i,j)       = GFSDATAI%VFRAC(i,j)
            SFCINPUT%FACSF(i,j)          = GFSDATAI%FACSF(i,j)
            SFCINPUT%FACWF(i,j)          = GFSDATAI%FACWF(i,j)
            SFCINPUT%SKIN_TEMP(i,j)      = GFSDATAI%TSEA(i,j)
            SFCINPUT%LSMASK(i,j)         = SLMSKI(i,j)
            SFCINPUT%SEA_ICE_FLAG(i,j)   = 0
            if (GFSDATAI%SLMSK(i,j) > 1.99)
     &        SFCINPUT%SEA_ICE_FLAG(i,j) = 1
            SFCINPUT%SNOW_LIQ_EQUIV(i,j) = GFSDATAI%SHELEG(i,j)
            SFCINPUT%Z0(i,j)             = GFSDATAI%ZORL(i,j)
            SFCINPUT%OROG(i,j)           = GFSDATAI%OROG(i,j)
            SFCINPUT%VEG_TYPE(i,j)       = NINT(GFSDATAI%VTYPE(i,j))
            SFCINPUT%SOIL_TYPE(i,j)      = NINT(GFSDATAI%STYPE(i,j))
          enddo
        enddo
        DO K=1, GFSHEADI%NSOIL
! $omp parallel do private(i,j)
          do j=1,jmi
            do i=1,imi
              SFCINPUT%SOILM_TOT(i,j,k)  = GFSDATAI%SMC(i,j,k)
              SFCINPUT%SOIL_TEMP(i,j,k)  = GFSDATAI%STC(i,j,k)
            enddo
          enddo
        enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  THE 200501 VERSION OF THE SURFACE FILE HAS ADDITIONAL FIELDS FOR
!  USE BY NOAH LSM AND NEW SEA ICE PHYSICS, WHILE OLDER VERSIONS
!  DO NOT.  IF THESE VARIABLES ARE NOT ALLOCATED, THE SURFACE CHGRES
!  CODE WILL NOT INTERPOLATE THEM.
        IF (GFSHEADI%IVS >= 200501) then
          ALLOCATE (SFCINPUT%SEA_ICE_FRACT(IMI,JMI))
          ALLOCATE (SFCINPUT%SEA_ICE_DEPTH(IMI,JMI))
          ALLOCATE (SFCINPUT%MXSNOW_ALB(IMI,JMI))
          ALLOCATE (SFCINPUT%SNOW_DEPTH(IMI,JMI))
          ALLOCATE (SFCINPUT%SLOPE_TYPE(IMI,JMI))
          ALLOCATE (SFCINPUT%GREENFRC_MAX(IMI,JMI))
          ALLOCATE (SFCINPUT%GREENFRC_MIN(IMI,JMI))
          ALLOCATE (SFCINPUT%SOILM_LIQ(IMI,JMI,GFSHEADI%NSOIL))
!
!$omp parallel do private(i,j)
          do j=1,jmi
            do i=1,imi
              SFCINPUT%SEA_ICE_FRACT(i,j) = GFSDATAI%FICE(i,j)
              SFCINPUT%SEA_ICE_DEPTH(i,j) = GFSDATAI%HICE(i,j)
              SFCINPUT%MXSNOW_ALB(i,j)    = GFSDATAI%SNOALB(i,j)
              SFCINPUT%SNOW_DEPTH(i,j)    = GFSDATAI%SNWDPH(i,j)
              SFCINPUT%SLOPE_TYPE(i,j)    = NINT(GFSDATAI%SLOPE(i,j))
              SFCINPUT%GREENFRC_MAX(i,j)  = GFSDATAI%SHDMAX(i,j)
              SFCINPUT%GREENFRC_MIN(i,j)  = GFSDATAI%SHDMIN(i,j)
            enddo
          enddo
          DO K=1, GFSHEADI%NSOIL
!$omp parallel do private(i,j)
            do j=1,jmi
              do i=1,imi
                SFCINPUT%SOILM_LIQ(i,j,k) = GFSDATAI%SLC(i,j,k)
              enddo
            enddo
          enddo
        END IF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  NOW ALLOCATE OUTPUT DATA STRUCTURE.  SURFACE CHGRES LOGIC NEEDS
!  THE LAND MASK AND OROGRAPHY ON OUTPUT GRID.  USE THE LONSPERLAT
!  INFO TO RUN EITHER THE REDUCED OR FULL GRID.
        IJMO = SUM(GFSHEADVO%LPL)*2   ! NUMBER OF GRID POINTS
                                      ! ACCOUNTING FOR REDUCED GRID.
        if (idrt == 0) ijmo = imo * jmo
        ALLOCATE (SFCOUTPUT%OROG(IJMO))
        ALLOCATE (KMSK(IMO,JMO))
        ALLOCATE (OROGO_uf2(IMO,JMO))
        KMSK = 0
        CALL INTERPRED(1,KMSK,GFSDATAO%OROG,SFCOUTPUT%OROG,
     &                 IMO,JMO,IJMO,GFSHEADVO%LPL)
        CALL INTERPRED(1,KMSK,OROGO_uf,OROGO_uf2,
     &                 IMO,JMO,IJMO,GFSHEADVO%LPL)

        ALLOCATE(SFCOUTPUT%LSMASK(IJMO))
        CALL INTERPRED(1,KMSK,SLMSKO,SFCOUTPUT%LSMASK,
     &                 IMO,JMO,IJMO,GFSHEADVO%LPL)
 
        ALLOCATE(SFCOUTPUT%LATS(IJMO))
        ALLOCATE(SFCOUTPUT%LONS(IJMO))
        CALL LATLONS(JMO, IJMO, GFSHEADVO%LPL,
     &               SFCOUTPUT%LATS, SFCOUTPUT%LONS, IDRT)
        ALLOCATE(SFCOUTPUT%ALNSF(IJMO))
        ALLOCATE(SFCOUTPUT%ALNWF(IJMO))
        ALLOCATE(SFCOUTPUT%ALVSF(IJMO))
        ALLOCATE(SFCOUTPUT%ALVWF(IJMO))
        ALLOCATE(SFCOUTPUT%CANOPY_MC(IJMO))
        ALLOCATE(SFCOUTPUT%FACSF(IJMO))
        ALLOCATE(SFCOUTPUT%FACWF(IJMO))
        ALLOCATE(SFCOUTPUT%GREENFRC(IJMO))
        ALLOCATE(SFCOUTPUT%SUBSTRATE_TEMP(IJMO))
        ALLOCATE(SFCOUTPUT%SKIN_TEMP(IJMO))
        ALLOCATE(SFCOUTPUT%SNOW_LIQ_EQUIV(IJMO))
        ALLOCATE(SFCOUTPUT%Z0(IJMO))
        ALLOCATE(SFCOUTPUT%SOILM_TOT(IJMO,GFSHEADO%NSOIL))
        ALLOCATE(SFCOUTPUT%SOIL_TEMP(IJMO,GFSHEADO%NSOIL))
        ALLOCATE(SFCOUTPUT%VEG_TYPE(IJMO))
        ALLOCATE(SFCOUTPUT%SOIL_TYPE(IJMO))
        ALLOCATE(SFCOUTPUT%SEA_ICE_FLAG(IJMO))
        IF (GFSHEADO%IVS >= 200501) THEN
          ALLOCATE(SFCOUTPUT%SLOPE_TYPE(IJMO))
          ALLOCATE(SFCOUTPUT%SEA_ICE_FRACT(IJMO))
          ALLOCATE(SFCOUTPUT%SEA_ICE_DEPTH(IJMO))
          ALLOCATE(SFCOUTPUT%SOILM_LIQ(IJMO,GFSHEADO%NSOIL))
          ALLOCATE(SFCOUTPUT%SNOW_DEPTH(IJMO))
          ALLOCATE(SFCOUTPUT%MXSNOW_ALB(IJMO))
          ALLOCATE(SFCOUTPUT%GREENFRC_MAX(IJMO))
          ALLOCATE(SFCOUTPUT%GREENFRC_MIN(IJMO))
        END IF
        IF (GFSHEADO%IVS >= 200509) then
          ALLOCATE (SFCOUTPUT%SEA_ICE_TEMP(IJMO))
        END IF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  INFO DESCRIBING BOTH GRIDS.
        KGDS_INPUT = 0
        KGDS_INPUT(1) = 4          ! OCT 6 - TYPE OF GRID (GAUSSIAN)
        KGDS_INPUT(2) = IMI        ! OCT 7-8 - # PTS ON LATITUDE CIRCLE
        KGDS_INPUT(3) = JMI        ! OCT 9-10 - # PTS ON LONGITUDE CIRCLE
        KGDS_INPUT(4) = 90000      ! OCT 11-13 - LAT OF ORIGIN
        KGDS_INPUT(5) = 0          ! OCT 14-16 - LON OF ORIGIN
        KGDS_INPUT(6) = 128        ! OCT 17 - RESOLUTION FLAG
        KGDS_INPUT(7) = -90000     ! OCT 18-20 - LAT OF EXTREME POINT
        KGDS_INPUT(8) = NINT(-360000./IMI)  ! OCT 21-23 - LON OF EXTREME POINT
        KGDS_INPUT(9)  = NINT((360.0 / FLOAT(IMI))*1000.0)
                                 ! OCT 24-25 - LONGITUDE DIRECTION INCR.
        KGDS_INPUT(10) = JMI /2    ! OCT 26-27 - NUMBER OF CIRCLES POLE TO EQUATOR
        KGDS_INPUT(12) = 255       ! OCT 29 - RESERVED
        KGDS_INPUT(20) = 255       ! OCT 5  - NOT USED, SET TO 255

        KGDS_OUTPUT = 0
        KGDS_OUTPUT(1) = IDRT       ! OCT 6 - TYPE OF GRID (GAUSSIAN)
        KGDS_OUTPUT(2) = IMO        ! OCT 7-8 - # PTS ON LATITUDE CIRCLE
        KGDS_OUTPUT(3) = JMO        ! OCT 9-10 - # PTS ON LONGITUDE CIRCLE
        KGDS_OUTPUT(4) = 90000      ! OCT 11-13 - LAT OF ORIGIN
        KGDS_OUTPUT(5) = 0          ! OCT 14-16 - LON OF ORIGIN
        KGDS_OUTPUT(6) = 128        ! OCT 17 - RESOLUTION FLAG
        KGDS_OUTPUT(7) = -90000     ! OCT 18-20 - LAT OF EXTREME POINT
        KGDS_OUTPUT(8) = NINT(-360000./IMO)  ! OCT 21-23 - LON OF EXTREME POINT
        KGDS_OUTPUT(9)  = NINT((360.0 / FLOAT(IMO))*1000.0)
                                    ! OCT 24-25 - LONGITUDE DIRECTION INCR
        KGDS_OUTPUT(10) = JMO /2    ! OCT 26-27 - NUMBER OF CIRCLES POLE TO EQUATOR
        KGDS_OUTPUT(12) = 255       ! OCT 29 - RESERVED
        KGDS_OUTPUT(20) = 255       ! OCT 5  - NOT USED, SET TO 255
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CALL SURFACE CHGRES DRIVER.
        FCSTHOUR = GFSHEADI%FHOUR
        CALL SURFACE_CHGRES_DRIVER(IMO,JMO,IJMO,GFSHEADO%NSOIL,
     &                             GFSHEADVO%LPL,
     &                             KGDS_OUTPUT,SFCOUTPUT,IMI,JMI,
     &                             orogo_uf2,use_ufo,nst_anl,
     &                             GFSHEADI%NSOIL,
     &                             GFSHEADI%IDATE(4),
     &                             GFSHEADI%IDATE(2), GFSHEADI%IDATE(3),
     &                             GFSHEADI%IDATE(1), FCSTHOUR,
     &                             KGDS_INPUT, SFCINPUT, IALB,
     &                             ISOT, IVEGSRC, MERGE, IRET)
        IF (IRET .NE. 0) THEN
          PRINT '("  ERROR IN SURFACE CHGRES DRIVER ")'
          CALL ERREXIT(34)
        END IF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GO FROM REDUCED, 1-D ARRAYS TO 2-D FOR OUTPUT.
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%LSMASK,SLMSKO,
     &                  IMO,JMO,IJMO,GFSHEADVO%LPL)
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%OROG,GFSDATAO%OROG,
     &                   IMO,JMO,IJMO,GFSHEADVO%LPL)
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%ALNSF,GFSDATAO%ALNSF,
     &                   IMO,JMO,IJMO,GFSHEADVO%LPL)
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%ALNWF,GFSDATAO%ALNWF,
     &                   IMO,JMO,IJMO,GFSHEADVO%LPL)
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%ALVSF,GFSDATAO%ALVSF,
     &                   IMO,JMO,IJMO,GFSHEADVO%LPL)
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%ALVWF,GFSDATAO%ALVWF,
     &                   IMO,JMO,IJMO,GFSHEADVO%LPL)
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%GREENFRC,GFSDATAO%VFRAC,
     &                   IMO,JMO,IJMO,GFSHEADVO%LPL)
        IF (ALLOCATED(SFCOUTPUT%GREENFRC_MAX)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%GREENFRC_MAX,GFSDATAO%SHDMAX
     &,                    IMO,JMO,IJMO,GFSHEADVO%LPL)
        END IF
        IF (ALLOCATED(SFCOUTPUT%GREENFRC_MIN)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%GREENFRC_MIN,GFSDATAO%SHDMIN
     &,                    IMO,JMO,IJMO,GFSHEADVO%LPL)
        END IF
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%Z0,GFSDATAO%ZORL,
     &                   IMO,JMO,IJMO,GFSHEADVO%LPL)
!       call hhmaxmin(SFCOUTPUT%SUBSTRATE_TEMP,IMO,jmo,jmo,1,' TG3  ' )
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SUBSTRATE_TEMP,GFSDATAO%TG3,
     &                   IMO,JMO,IJMO,GFSHEADVO%LPL)
        IF (ALLOCATED (SFCOUTPUT%MXSNOW_ALB)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%MXSNOW_ALB,GFSDATAO%SNOALB,
     &                     IMO,JMO,IJMO,GFSHEADVO%LPL)
        END IF
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%CANOPY_MC,GFSDATAO%CANOPY,
     &                   IMO,JMO,IJMO,GFSHEADVO%LPL)
        IF (ALLOCATED(SFCOUTPUT%SEA_ICE_FRACT)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SEA_ICE_FRACT,GFSDATAO%FICE,
     &                     IMO,JMO,IJMO,GFSHEADVO%LPL)
        END IF
        IF (ALLOCATED(SFCOUTPUT%SEA_ICE_DEPTH)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SEA_ICE_DEPTH,GFSDATAO%HICE,
     &                     IMO,JMO,IJMO,GFSHEADVO%LPL)
        END IF
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%FACSF,GFSDATAO%FACSF,
     &                   IMO,JMO,IJMO,GFSHEADVO%LPL)
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%FACWF,GFSDATAO%FACWF,
     &                   IMO,JMO,IJMO,GFSHEADVO%LPL)
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SKIN_TEMP,GFSDATAO%TSEA,
     &                   IMO,JMO,IJMO,GFSHEADVO%LPL)
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SNOW_LIQ_EQUIV,GFSDATAO%SHELEG
     &,                   IMO,JMO,IJMO,GFSHEADVO%LPL)
        IF (ALLOCATED (SFCOUTPUT%SNOW_DEPTH)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SNOW_DEPTH,GFSDATAO%SNWDPH,
     &                     IMO,JMO,IJMO,GFSHEADVO%LPL)
        END IF
        DO K=1, GFSHEADO%NSOIL
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SOILM_TOT(:,K),
     &                      GFSDATAO%SMC(:,:,K),
     &                      IMO,JMO,IJMO,GFSHEADVO%LPL)
          IF (ALLOCATED(SFCOUTPUT%SOILM_LIQ)) THEN
            CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SOILM_LIQ(:,K),
     &                       GFSDATAO%SLC(:,:,K),
     &                       IMO,JMO,IJMO,GFSHEADVO%LPL)
         END IF
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SOIL_TEMP(:,K),
     &                     GFSDATAO%STC(:,:,K),
     &                     IMO,JMO,IJMO,GFSHEADVO%LPL)
        ENDDO
!
        IF (ALLOCATED(SFCOUTPUT%SEA_ICE_TEMP)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SEA_ICE_TEMP,GFSDATAO%TISFC,
     &                    IMO,JMO,IJMO,GFSHEADVO%LPL)
        END IF
!
        ALLOCATE(DUMMY(IJMO))
        IF (ALLOCATED(SFCOUTPUT%SLOPE_TYPE)) THEN
          DUMMY=FLOAT(SFCOUTPUT%SLOPE_TYPE)
          CALL UNINTERPRED(1,KMSK,DUMMY,GFSDATAO%SLOPE,
     &                     IMO,JMO,IJMO,GFSHEADVO%LPL)
        END IF
        DUMMY=FLOAT(SFCOUTPUT%SOIL_TYPE)
        CALL UNINTERPRED(1,KMSK,DUMMY,GFSDATAO%STYPE,
     &                  IMO,JMO,IJMO,GFSHEADVO%LPL)
        DUMMY=FLOAT(SFCOUTPUT%VEG_TYPE)
        CALL UNINTERPRED(1,KMSK,DUMMY,GFSDATAO%VTYPE,
     &                  IMO,JMO,IJMO,GFSHEADVO%LPL)
        DUMMY=FLOAT(SFCOUTPUT%SEA_ICE_FLAG)
        ALLOCATE(DUMMY2(IMO,JMO))
        CALL UNINTERPRED(1,KMSK,DUMMY,DUMMY2,
     &                  IMO,JMO,IJMO,GFSHEADVO%LPL)
        WHERE (NINT(DUMMY2)==1)SLMSKO=2.0
        GFSDATAO%SLMSK=SLMSKO

        DEALLOCATE (DUMMY2, DUMMY, KMSK)

        call nemsio_gfs_axsfc(GFSDATAI)
!
        call nemsio_close(GFILEISFC)
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! output sfcio file
        IF( NSFCO>0 .and. (OUTTYP == 0.or.OUTTYP == 2)) THEN
          PRINT*,"- WRITE SURFACE DATA TO SFCIO FILE."
          CALL SFCIO_SWOPEN(NSFCO,'chgres.out.sfc',IRET)
          IF(IRET >= 0) THEN
!set sfcio head
            SFCHEADO%fhour=GFSHEADO%fhour
            SFCHEADO%IDATE(1:4) = GFSHEADO%IDATE(1:4)
            SFCHEADO%IDATE(1)   = GFSHEADO%IDATE(4)
            SFCHEADO%IDATE(4)   = GFSHEADO%IDATE(1)
            SFCHEADO%LATB       = GFSHEADO%DIMY
            SFCHEADO%LONB       = GFSHEADO%DIMX
            SFCHEADO%IVS        = 200509
            if(IVSSFC > 0) SFCHEADO%IVS = IVSSFC
            SFCHEADO%LSOIL      = GFSHEADO%NSOIL
            call sfcio_alhead(sfcheado,iret)
!  GET NEW LONSPERLAT
            SFCHEADO%LPL    = GFSHEADVO%LPL
!ggg start
            SFCHEADO%ZSOIL  = GFSHEADVO%ZSOIL
!
!-- set data back:
            CALL SFCIO_ALDBTA(SFCHEADO,SFCDATAO,IRET)
            SFCDATAO%tsea   = gfsdatao%tsea
            SFCDATAO%smc    = gfsdatao%smc
            SFCDATAO%sheleg = gfsdatao%sheleg
            SFCDATAO%stc    = gfsdatao%stc
            SFCDATAO%tg3    = gfsdatao%tg3
            SFCDATAO%zorl   = gfsdatao%zorl
            SFCDATAO%cv     = -9999.
            SFCDATAO%cvb    = -9999.
            SFCDATAO%cvt    = -9999.
            SFCDATAO%alvsf  = gfsdatao%alvsf
            SFCDATAO%alvwf  = gfsdatao%alvwf
            SFCDATAO%alnsf  = gfsdatao%alnsf
            SFCDATAO%alnwf  = gfsdatao%alnwf
            SFCDATAO%slmsk  = gfsdatao%slmsk
            SFCDATAO%vfrac  = gfsdatao%vfrac
            SFCDATAO%canopy = gfsdatao%canopy
            SFCDATAO%f10m   = gfsdatao%f10m
            SFCDATAO%t2m    = gfsdatao%t2m
            SFCDATAO%q2m    = gfsdatao%q2m
            SFCDATAO%vtype  = gfsdatao%vtype
            SFCDATAO%stype  = gfsdatao%stype
            SFCDATAO%facsf  = gfsdatao%facsf
            SFCDATAO%facwf  = gfsdatao%facwf
            SFCDATAO%uustar = gfsdatao%uustar
            SFCDATAO%ffmm   = gfsdatao%ffmm
            SFCDATAO%ffhh   = gfsdatao%ffhh
            SFCDATAO%hice   = gfsdatao%hice
            SFCDATAO%fice   = gfsdatao%fice
            SFCDATAO%tisfc  = gfsdatao%tisfc
            SFCDATAO%tprcp  = gfsdatao%tprcp
            SFCDATAO%srflag = gfsdatao%srflag
            SFCDATAO%snwdph = gfsdatao%snwdph
            SFCDATAO%slc    = gfsdatao%slc
            SFCDATAO%shdmin = gfsdatao%shdmin
            SFCDATAO%shdmax = gfsdatao%shdmax
            SFCDATAO%slope  = gfsdatao%slope
            SFCDATAO%snoalb = gfsdatao%snoalb
            SFCDATAO%orog   = gfsdatao%orog
!
            CALL SFCIO_SWHEAD(NSFCO,SFCHEADO,IRET)
            CALL SFCIO_SWDBTA(NSFCO,SFCHEADO,SFCDATAO,IRET)
            IF(NSLM > 0) CALL BACLOSE(NSLM,IRET)
            IF(NLPL > 0) CLOSE(NLPL)
            CALL SFCIO_SCLOSE(NSFCO,IRET)
          ENDIF
        ENDIF  ! write sfc file to sfcio format

        IF (DO_NSST) THEN
          ALLOCATE(LPL_OUTPUT((JMO+1)/2))
          LPL_OUTPUT=GFSHEADVO%LPL
          ALLOCATE(RLATS_OUTPUT(IJMO))
          RLATS_OUTPUT=SFCOUTPUT%LATS
          ALLOCATE(RLONS_OUTPUT(IJMO))
          RLONS_OUTPUT=SFCOUTPUT%LONS
          ALLOCATE(MASK_OUTPUT(IJMO))
          MASK_OUTPUT=SFCOUTPUT%LSMASK
          WHERE(SFCOUTPUT%SEA_ICE_FLAG==1) MASK_OUTPUT=2
          ALLOCATE(NSST_OUTPUT_THIN(IJMO,NUM_NSST_FIELDS))
          ALLOCATE(NSST_INPUT(IMI,JMI,NUM_NSST_FIELDS))
          ALLOCATE(MASK_INPUT(IMI,JMI))
          CALL READ_NSST_FROM_NEMSIO(MASK_INPUT,NSST_INPUT,IMI,JMI,
     &           NUM_NSST_FIELDS,NSST_YEAR,NSST_MON,NSST_DAY,
     &           NSST_HOUR,NSST_FHOUR)
          PRINT*,"- CHANGE NSST FILE RESOLUTION FROM ",IMI, " X ",JMI
          PRINT*,"                                TO ",IMO, " X ",JMO
          CALL NSST_CHGRES(IMI,JMI,MASK_OUTPUT,
     &                     IJMO,KGDS_INPUT, NSST_INPUT, MASK_INPUT,
     &                     NSST_OUTPUT_THIN, NUM_NSST_FIELDS,
     &                     KGDS_OUTPUT,
     &                     RLATS_OUTPUT,RLONS_OUTPUT)
          DEALLOCATE(RLATS_OUTPUT,RLONS_OUTPUT)
          DEALLOCATE(NSST_INPUT,MASK_INPUT)
          IF (OUTTYP==2.OR.OUTTYP==0)THEN
            CALL WRITE_NSST_TO_NSTIO(NSST_YEAR,NSST_MON,NSST_DAY,
     &                        NSST_HOUR,NSST_FHOUR,LPL_OUTPUT,IMO,JMO,
     &                        IJMO,MASK_OUTPUT,NSST_OUTPUT_THIN,
     &                        NUM_NSST_FIELDS)
          ENDIF
          IF (OUTTYP==1.OR.OUTTYP==0)THEN
            CALL WRITE_NSST_TO_NEMSIO(IMO,JMO,IJMO,NSST_YEAR,NSST_MON,
     &           NSST_DAY,NSST_HOUR,NSST_FHOUR,MASK_OUTPUT,
     &           NSST_OUTPUT_THIN,NUM_NSST_FIELDS,LPL_OUTPUT)
          ENDIF
          DEALLOCATE(NSST_OUTPUT_THIN, MASK_OUTPUT)
          DEALLOCATE(LPL_OUTPUT)
        ENDIF  ! process nsst file
!
      ENDIF  inptypif  ! end of inptyp sfc if
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Prepare for nemsio output
      nsfcoif2: IF(NSFCO>0) THEN
       outtypif: IF (OUTTYP == 0 .or. OUTTYP==1) THEN
        inptypsfc: IF(INPTYP==2) THEN
          IMO = LONBO
          JMO = LATBO
          GFSHEADO%DIMX = LONBO
          GFSHEADO%DIMY = LATBO
          GFSHEADO%DIMZ = 1
          GFSHEADO%NFHOUR    = INT(FHOURO)
          GFSHEADO%NFMINUTE  = INT((FHOURO-GFSHEADO%NFHOUR)*60)
          GFSHEADO%NFSECONDN = INT((FHOURO-GFSHEADO%NFHOUR-
     &        GFSHEADO%NFMINUTE/60.)*3600.*100.)
          GFSHEADO%NFSECONDD  = 100
          GFSHEADO%IDATE(1:6) = 0
          GFSHEADO%IDATE(1:4) = IDATE4O(1:4)
          GFSHEADO%IDATE(1)   = IDATE4O(4)
          GFSHEADO%IDATE(4)   = IDATE4O(1)
          GFSHEADO%IDATE(7)   = 100
          GFSHEADO%NSOIL      = LSOILO
!         print *,'GFSHEADO%IDATE=',GFSHEADO%IDATE(1:4),'nsoil=',
!    &       GFSHEADO%NSOIL,'allocated(zsoil)=',
!    &       allocated(GFSHEADVO%ZSOIL),'ivssfc=',ivssfc
!
          ALLOCATE(GFSHEADVO%ZSOIL(GFSHEADO%NSOIL))
          IF(IVSSFC > 0) THEN
            GFSHEADO%IVS=IVSSFC
            IF(LSOIL > 0) THEN
              IF(LSOIL == 2 .OR. LSOIL == 4)THEN
                GFSHEADO%NSOIL = LSOIL
              ELSE
                PRINT '("  NUMBER OF SOIL LAYERS MUST BE 2 OR 4. ")'
                CALL ERREXIT(9)
              ENDIF
            ENDIF
            IF (GFSHEADO%IVS < 200501) THEN
              GFSHEADO%NSOIL = 2 ! MUST USE 2 LAYERS.
            END IF
          ENDIF
!
!-- set data back:
          call nemsio_gfs_alsfc(IMO,JMO,LSOILO,GFSDATAO)

          do l=1,lsoilo
!$omp parallel do private(i,j)
            do j=1,jmo
              do i=1,imo
                GFSDATAO%stc(i,j,l) = sfcdatao%stc(i,j,l)
                GFSDATAO%smc(i,j,l) = sfcdatao%smc(i,j,l)
                GFSDATAO%slc(i,j,l) = sfcdatao%slc(i,j,l)
              enddo
            enddo
          enddo
!$omp parallel do private(i,j)
          do j=1,jmo
            do i=1,imo
              GFSDATAO%tsea(i,j)   = sfcdatao%tsea(i,j)
              GFSDATAO%sheleg(i,j) = sfcdatao%sheleg(i,j)
              GFSDATAO%tg3(i,j)    = sfcdatao%tg3(i,j)
              GFSDATAO%zorl(i,j)   = sfcdatao%zorl(i,j)
              GFSDATAO%alvsf(i,j)  = sfcdatao%alvsf(i,j)
              GFSDATAO%alvwf(i,j)  = sfcdatao%alvwf(i,j)
              GFSDATAO%alnsf(i,j)  = sfcdatao%alnsf(i,j)
              GFSDATAO%alnwf(i,j)  = sfcdatao%alnwf(i,j)
              GFSDATAO%slmsk(i,j)  = sfcdatao%slmsk(i,j)
              GFSDATAO%vfrac(i,j)  = sfcdatao%vfrac(i,j)
              GFSDATAO%canopy(i,j) = sfcdatao%canopy(i,j)
              GFSDATAO%f10m(i,j)   = sfcdatao%f10m(i,j)
              GFSDATAO%t2m(i,j)    = sfcdatao%t2m(i,j)
              GFSDATAO%q2m(i,j)    = sfcdatao%q2m(i,j)
              GFSDATAO%vtype(i,j)  = sfcdatao%vtype(i,j)
              GFSDATAO%stype(i,j)  = sfcdatao%stype(i,j)
              GFSDATAO%facsf(i,j)  = sfcdatao%facsf(i,j)
              GFSDATAO%facwf(i,j)  = sfcdatao%facwf(i,j)
              GFSDATAO%uustar(i,j) = sfcdatao%uustar(i,j)
              GFSDATAO%ffmm(i,j)   = sfcdatao%ffmm(i,j)
              GFSDATAO%ffhh(i,j)   = sfcdatao%ffhh(i,j)
              GFSDATAO%hice(i,j)   = sfcdatao%hice(i,j)
              GFSDATAO%fice(i,j)   = sfcdatao%fice(i,j)
              GFSDATAO%tisfc(i,j)  = sfcdatao%tisfc(i,j)
              GFSDATAO%tprcp(i,j)  = sfcdatao%tprcp(i,j)
              GFSDATAO%srflag(i,j) = sfcdatao%srflag(i,j)
              GFSDATAO%snwdph(i,j) = sfcdatao%snwdph(i,j)
              GFSDATAO%shdmin(i,j) = sfcdatao%shdmin(i,j)
              GFSDATAO%shdmax(i,j) = sfcdatao%shdmax(i,j)
              GFSDATAO%slope(i,j)  = sfcdatao%slope(i,j)
              GFSDATAO%snoalb(i,j) = sfcdatao%snoalb(i,j)
              GFSDATAO%orog(i,j)   = sfcdatao%orog(i,j)
            enddo
          enddo
!
!other meta variables
        GFSHEADO%EXTRAMETA = .true.
!
        GFSHEADO%NMETAVARI = 5
        ALLOCATE(GFSHEADVO%VARINAME(GFSHEADO%NMETAVARI))
        ALLOCATE(GFSHEADVO%VARIVAL(GFSHEADO%NMETAVARI))
        GFSHEADVO%VARINAME(1:GFSHEADO%NMETAVARI) = (/'LATB   '
     &    ,'LONB   ','IREALF ','LSOIL  ','IVSSFC ' /)
        GFSHEADVO%VARIVAL(1:GFSHEADO%NMETAVARI)  = (/LATBO,
     &    LONBO,IREALFO,LSOILO,IVSO /)
!
        GFSHEADO%NMETAVARR = 1
        ALLOCATE(GFSHEADVO%VARRNAME(GFSHEADO%NMETAVARR))
        ALLOCATE(GFSHEADVO%VARRVAL(GFSHEADO%NMETAVARR))
        GFSHEADVO%VARRNAME(1:GFSHEADO%NMETAVARR) = (/'FHOUR  '/)
        GFSHEADVO%VARRVAL(1:GFSHEADO%NMETAVARR)  = (/FHOURO/)
!
        GFSHEADO%NMETAARYI = 1
        ALLOCATE(GFSHEADVO%ARYINAME(GFSHEADO%NMETAARYI))
        ALLOCATE(GFSHEADVO%ARYILEN(GFSHEADO%NMETAARYI))
        GFSHEADVO%ARYINAME(1:GFSHEADO%NMETAARYI) = (/'lpl '/)
        GFSHEADVO%ARYILEN(1:GFSHEADO%NMETAARYI)  =
     &       (/size(LPLO)/)
        ALLOCATE(GFSHEADVO%ARYIVAL(maxval(GFSHEADVO%ARYILEN(1:
     &       GFSHEADO%NMETAARYI)),GFSHEADO%NMETAARYI))
        GFSHEADVO%ARYIVAL(1:GFSHEADVO%ARYILEN(1),1)=LPLO(1:
     &       size(LPLO))
!
        GFSHEADO%NMETAARYR = 1
        ALLOCATE(GFSHEADVO%ARYRNAME(GFSHEADO%NMETAARYR))
        ALLOCATE(GFSHEADVO%ARYRLEN(GFSHEADO%NMETAARYR))
        GFSHEADVO%ARYRNAME(1:GFSHEADO%NMETAARYR) = (/'zsoil'/)
        GFSHEADVO%ARYRLEN(1:GFSHEADO%NMETAARYR)  = size(ZSOILO)

        ALLOCATE(GFSHEADVO%ARYRVAL(maxval(GFSHEADVO%ARYRLEN(1:
     &       GFSHEADO%NMETAARYR)),GFSHEADO%NMETAARYR))
        GFSHEADVO%ARYRVAL(1:GFSHEADVO%ARYRLEN(1),1) = ZSOILO(1:
     &       LSOILO)
!
        NREC_SFC = 32 + 3*GFSHEADO%NSOIL
        GFSHEADO%NREC = NREC_SFC
        ALLOCATE(GFSHEADVO%RECNAME(NREC_SFC))
        ALLOCATE(GFSHEADVO%RECLEVTYP(NREC_SFC))
        ALLOCATE(GFSHEADVO%RECLEV(NREC_SFC))
!record name
        GFSHEADVO%RECNAME(1)  = 'tmp'
        GFSHEADVO%RECNAME(2)  = 'weasd'
        GFSHEADVO%RECNAME(3)  = 'tg3'
        GFSHEADVO%RECNAME(4)  = 'sfcr'
!       GFSHEADVO%RECNAME(5)  = 'tcdc'
!       GFSHEADVO%RECNAME(6)  = 'pres'
!       GFSHEADVO%RECNAME(7)  = 'pres'
        GFSHEADVO%RECNAME(5)  = 'alvsf'
        GFSHEADVO%RECNAME(6)  = 'alvwf'
        GFSHEADVO%RECNAME(7)  = 'alnsf'
        GFSHEADVO%RECNAME(8)  = 'alnwf'
        GFSHEADVO%RECNAME(9)  = 'land'
        GFSHEADVO%RECNAME(10) = 'veg'
        GFSHEADVO%RECNAME(11) = 'cnwat'
        GFSHEADVO%RECNAME(12) = 'f10m'
        GFSHEADVO%RECNAME(13) = 'tmp'
        GFSHEADVO%RECNAME(14) = 'spfh'
        GFSHEADVO%RECNAME(15) = 'vtype'
        GFSHEADVO%RECNAME(16) = 'sotyp'
        GFSHEADVO%RECNAME(17) = 'facsf'
        GFSHEADVO%RECNAME(18) = 'facwf'
        GFSHEADVO%RECNAME(19) = 'fricv'
        GFSHEADVO%RECNAME(20) = 'ffmm'
        GFSHEADVO%RECNAME(21) = 'ffhh'
        GFSHEADVO%RECNAME(22) = 'icetk'
        GFSHEADVO%RECNAME(23) = 'icec'
        GFSHEADVO%RECNAME(24) = 'tisfc'
        GFSHEADVO%RECNAME(25) = 'tprcp'
        GFSHEADVO%RECNAME(26) = 'crain'
        GFSHEADVO%RECNAME(27) = 'snod'
        GFSHEADVO%RECNAME(28) = 'shdmin'
        GFSHEADVO%RECNAME(29) = 'shdmax'
        GFSHEADVO%RECNAME(30) = 'sltyp'
        GFSHEADVO%RECNAME(31) = 'salbd'
        GFSHEADVO%RECNAME(32) = 'orog'
        GFSHEADVO%RECNAME(33:32+GFSHEADO%NSOIL) = 'smc'
        GFSHEADVO%RECNAME(GFSHEADO%NSOIL+33:32+2*GFSHEADO%NSOIL) =
     &      'stc'
        GFSHEADVO%RECNAME(2*GFSHEADO%NSOIL+33:32+3*GFSHEADO%NSOIL) =
     &      'slc'
!
        GFSHEADVO%RECLEVTYP(1:32) = 'sfc'
!       GFSHEADVO%RECLEVTYP(5)    = 'convect-cld lay'
!       GFSHEADVO%RECLEVTYP(6)    = 'convect-cld bot'
!       GFSHEADVO%RECLEVTYP(7)    = 'convect-cld top'
        GFSHEADVO%RECLEVTYP(12)   = '10 m above gnd'
        GFSHEADVO%RECLEVTYP(13)   = '2 m above gnd'
        GFSHEADVO%RECLEVTYP(14)   = '2 m above gnd'
        GFSHEADVO%RECLEVTYP(33:NREC_SFC) = 'soil layer'
!
        GFSHEADVO%RECLEV(1:32) = 1
        DO K=1,GFSHEADO%NSOIL
          GFSHEADVO%RECLEV(K+32) = K
          GFSHEADVO%RECLEV(GFSHEADO%NSOIL+K+32) = K
          GFSHEADVO%RECLEV(2*GFSHEADO%NSOIL+K+32)=K
        ENDDO
!
      else if(INPTYP ==1) then

       if(GFSHEADO%nmetavari>0) then
         do i=1,GFSHEADO%nmetavari
           if(GFSHEADVO%variname(i)=='LATB'.or.
     &        GFSHEADVO%variname(i)=='latb')
     &       GFSHEADVO%varival(i)=GFSHEADO%LATB
           if(GFSHEADVO%variname(i)=='LONB'.or.
     &       GFSHEADVO%variname(i)=='lonb')
     &       GFSHEADVO%varival(i)=GFSHEADO%LONB
           if(GFSHEADVO%variname(i)=='LATR'.or.
     &        GFSHEADVO%variname(i)=='latr')
     &       GFSHEADVO%varival(i)=GFSHEADO%LATR
           if(GFSHEADVO%variname(i)=='LONR'.or.
     &       GFSHEADVO%variname(i)=='lonr')
     &       GFSHEADVO%varival(i)=GFSHEADO%LONR
           if(GFSHEADVO%variname(i)=='IREALF'.or.
     &       GFSHEADVO%variname(i)=='irealf')
     &       GFSHEADVO%varival(i)=GFSHEADO%IREALF
           if(GFSHEADVO%variname(i)=='LSOIL'.or.
     &        GFSHEADVO%variname(i)=='lsoil')
     &       GFSHEADVO%varival(i)=GFSHEADO%NSOIL
           if(GFSHEADVO%variname(i)=='IVSSFC'.or.
     &        GFSHEADVO%variname(i)=='ivssfc')
     &       GFSHEADVO%varival(i)=GFSHEADO%IVS
           if(GFSHEADVO%variname(i)=='IDRT'.or.
     &        GFSHEADVO%variname(i)=='idrt')
     &       GFSHEADVO%varival(i)=GFSHEADO%idrt
         enddo
       endif

       IF (GFSHEADO%nmetaaryi>0) THEN

         DO I=1,GFSHEADO%nmetaaryi

           IF( (GFSHEADVO%aryiname(i)=='lpl' .OR. 
     &          GFSHEADVO%aryiname(i)=='LPL') .AND.
     &          GFSHEADVO%aryilen(i).NE.
     &         ((GFSHEADO%LATB+1)/2) ) THEN

             GFSHEADVO%aryilen(i)=(GFSHEADO%LATB+1)/2
             IF(ALLOCATED(GFSHEADVO%aryival))
     &          DEALLOCATE(GFSHEADVO%aryival)
             ALLOCATE(GFSHEADVO%aryival(maxval(GFSHEADVO%aryilen),
     &          GFSHEADO%nmetaaryi))

           ENDIF

         ENDDO

         DO I=1,GFSHEADO%nmetaaryi

           IF( GFSHEADVO%aryiname(i)=='lpl' .OR.
     &         GFSHEADVO%aryiname(i)=='LPL' ) THEN
                
             GFSHEADVO%aryival(1:(GFSHEADO%LATB+1)/2,I)=
     &         GFSHEADVO%LPL(1:(GFSHEADO%LATB+1)/2)

           ELSE

             GFSHEADVO%aryival(1:GFSHEADVO%aryilen(i),i)=
     &        GFSHEADVI%aryival(1:GFSHEADVO%aryilen(i),i)

           ENDIF

         ENDDO

!         print *,'aryiname=',GFSHEADVO%aryiname,GFSHEADVO%aryilen
!     &  ,'aryival=',GFSHEADVO%aryival

       ENDIF

       if(GFSHEADO%nmetaaryr>0) then
         DO i=1,GFSHEADO%nmetaaryr
           if(GFSHEADVO%aryrname(i)=='zsoil') then
             GFSHEADVO%aryrval(1:GFSHEADO%NSOIL,i)=
     &    GFSHEADVO%ZSOIL(1:GFSHEADO%NSOIL)
           endif
         ENDDO
       endif

       call nemsio_gfs_axheadv(GFSHEADVI)
!
       ENDIF  inptypsfc ! end of inptyp 2
!--------------------------------------------------------------------
!** write out nemsio sfc file
!
      PRINT*,"- WRITE SURFACE DATA TO NEMSIO FILE."
      CALL NEMSIO_OPEN(GFILEOSFC,TRIM('chgres.out.sfn'),'write'
     &,        MODELNAME="GFS"
     &,        GDATATYPE="bin4"
     &,        NFHOUR=GFSHEADO%NFHOUR
     &,        NFMINUTE=GFSHEADO%NFMINUTE
     &,        NFSECONDN=GFSHEADO%NFSECONDN
     &,        NFSECONDD=GFSHEADO%NFSECONDD
     &,        IDATE=GFSHEADO%IDATE
     &,        NREC=GFSHEADO%NREC
     &,        DIMX=GFSHEADO%DIMX
     &,        DIMY=GFSHEADO%DIMY
     &,        DIMZ=GFSHEADO%DIMZ
     &,        NSOIL=GFSHEADO%NSOIL
     &,        NMETA=5
     &,        RECNAME=GFSHEADVO%RECNAME
     &,        RECLEVTYP=GFSHEADVO%RECLEVTYP
     &,        RECLEV=GFSHEADVO%RECLEV
     &,        EXTRAMETA=GFSHEADO%EXTRAMETA
     &,        NMETAVARI=GFSHEADO%NMETAVARI
     &,        NMETAVARR=GFSHEADO%NMETAVARR
     &,        NMETAARYI=GFSHEADO%NMETAARYI
     &,        NMETAARYR=GFSHEADO%NMETAARYR
     &,        VARINAME=GFSHEADVO%VARINAME
     &,        VARIVAL=GFSHEADVO%VARIVAL
     &,        VARRNAME=GFSHEADVO%VARRNAME
     &,        VARRVAL=GFSHEADVO%VARRVAL
     &,        ARYINAME=GFSHEADVO%ARYINAME
     &,        ARYILEN=GFSHEADVO%ARYILEN
     &,        ARYIVAL=GFSHEADVO%ARYIVAL
     &,        ARYRNAME=GFSHEADVO%ARYRNAME
     &,        ARYRLEN=GFSHEADVO%ARYRLEN
     &,        ARYRVAL=GFSHEADVO%ARYRVAL
     &,        IRET=IRET)
        IF(IRET.NE.0) THEN
          PRINT*, ' ERROR AT NEMSIO_OPEN chgres.out.sfn '
          CALL ERREXIT(4)
        ENDIF

        print *,'after nemsio_open sfc file,iret=',iret
!
!sfc data
        call nemsio_gfs_wrtsfc(GFILEOSFC,GFSDATAO,iret)
        call nemsio_close(GFILEOSFC,iret=iret)
        call nemsio_gfs_axheadv(GFSHEADVO)
        call nemsio_gfs_axsfc(GFSDATAO)

      ENDIF outtypif   ! end outtyp
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  FREE UP MEMORY.
      CALL SURFACE_CHGRES_AX2D(SFCINPUT)
      CALL SURFACE_CHGRES_AX1D(SFCOUTPUT)
      call instrument(20,kall,ttot,tmin,tmax)
!
      ENDIF nsfcoif2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL SFCIO_AXDBTA(SFCDATAO,IRET)
!
      call nemsio_finalize()
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      IF(NSIGO==0 .or.NSFCO==0) THEN
        print *,'no sig/grid or sfc file'
!jw        CALL ERREXIT(5)
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL W3TAGE('GLOBAL_CHGRES')
       do k=1,30
       call instrument(-k,kall,ttot,tmin,tmax)
       print '(2i6,3f12.3)',k,kall,ttot,tmin,tmax
       enddo
      END
!-----------------------------------------------------------------------
      SUBROUTINE READ_NSST_FROM_NEMSIO(MASK_INPUT,NSST_INPUT,IMI,JMI,
     &           NUM_NSST_FIELDS,NSST_YEAR,NSST_MON,NSST_DAY,
     &           NSST_HOUR,NSST_FHOUR)

!-----------------------------------------------------------------------
! Subroutine: read nsst data from nemsio file
!
! Author: George Gayno/EMC
!
! Abstract: Reads an nsst file in nemsio format.  Places data
!           in the "nsst_input" array as expected by routine
!           nsst_chgres.
!
! Input files: 
!    "chgres.inp.nst" - input nsst nemsio file
!
! Output files:  none
!
! History:
!   2016-04-05   Gayno - Initial version
!
! Condition codes:  all non-zero codes are fatal
!    16 - bad initialization of nemsio environment
!    17 - bad open of nst file "chgres.inp.nst"
!    18 - bad read of "chgres.inp.nst" header
!    19 - the program assumes that the resolution of the
!         nst grid matches the input surface grid.  if
!         they are not the same, stop procoessing.
!    20 - the nst file does not have the 19 required records.
!    21 - bad read of an nst file record.
!-----------------------------------------------------------------------

      use nemsio_module

      implicit none

      character(len=3)        :: levtyp
      character(len=8)        :: recname(19)

      integer, intent(in)     :: imi, jmi, num_nsst_fields
      integer, intent(out)    :: nsst_year, nsst_mon
      integer, intent(out)    :: nsst_day, nsst_hour

      real,    intent(out)    :: mask_input(imi,jmi)
      real,    intent(out)    :: nsst_input(imi,jmi,num_nsst_fields)
      real,    intent(out)    :: nsst_fhour

      integer(nemsio_intkind) :: iret, nrec, dimx, dimy, lev, nframe
      integer(nemsio_intkind) :: idate(7), nfhour

      integer                 :: i, j

      real(nemsio_realkind),allocatable :: dummy(:)

      type(nemsio_gfile)      :: gfile

      data recname   /"land    ", "xt      ", "xs      ",
     &           "xu      ", "xv      ", "xz      ",
     &           "zm      ", "xtts    ", "xzts    ",
     &           "dtcool  ", "zc      ", "c0      ",
     &           "cd      ", "w0      ", "wd      ",
     &           "dconv   ", "ifd     ", "tref    ",
     &           "qrain   " /

      print*,"- READ INPUT NSST DATA IN NEMSIO FORMAT"

      call nemsio_init(iret=iret)
      if (iret /= 0) then
        print*,"- FATAL ERROR: bad nemsio initialization."
        print*,"- IRET IS ", iret
        call errexit(16)
      endif

      call nemsio_open(gfile, "chgres.inp.nst", "read", iret=iret)
      if (iret /= 0) then
        print*,"- FATAL ERROR: bad open of chgres.inp.nst."
        print*,"- IRET IS ", iret
        call errexit(17)
      endif

      print*,"- READ FILE HEADER"
      call nemsio_getfilehead(gfile,iret=iret,nrec=nrec,dimx=dimx,
     &     dimy=dimy,idate=idate,nfhour=nfhour)
      if (iret /= 0) then
        print*,"- FATAL ERROR: bad read of chgres.inp.nst header."
        print*,"- IRET IS ", iret
        call errexit(18)
      endif

      if (dimx /= imi .or. dimy /= jmi) then
        print*,"- FATAL ERROR: nst and sfc file resolution"
        print*,"- must be the same."
        call errexit(19)
      endif

      if (nrec /= 19) then
        print*,"- FATAL ERROR: nst file has wrong number of records."
        call errexit(20)
      endif

      nsst_year=idate(1)
      nsst_mon=idate(2)
      nsst_day=idate(3)
      nsst_hour=idate(4)
      nsst_fhour=float(nfhour)

      levtyp='sfc'
      lev=1
      nframe=0

      allocate(dummy(imi*jmi))

      print*,"- READ LANDMASK RECORD"
      call nemsio_readrecv(gfile,recname(1),levtyp,lev,
     &     dummy,nframe,iret)

      if (iret /= 0) then
        print*,"- FATAL ERROR: bad read of chgres.inp.nst."
        print*,"- IRET IS ", iret
        call errexit(21)
      endif

      mask_input = reshape (dummy, (/imi,jmi/))

      print*,"- READ REMAINING DATA RECORDS"
      do j = 2, nrec
        call nemsio_readrecv(gfile,recname(j),levtyp,lev,
     &       dummy,nframe,iret)
        if (iret /= 0) then
          print*,"- FATAL ERROR: bad read of chgres.inp.nst."
          print*,"- IRET IS ", iret
          call errexit(21)
        endif
        nsst_input(:,:,j-1) = reshape (dummy, (/imi,jmi/))
      enddo

      deallocate(dummy)

      call nemsio_close(gfile,iret=iret)

      call nemsio_finalize()

      END SUBROUTINE READ_NSST_FROM_NEMSIO
!-----------------------------------------------------------------------
      SUBROUTINE WRITE_NSST_TO_NSTIO(NSST_YEAR,NSST_MON,NSST_DAY,
     &                        NSST_HOUR,NSST_FHOUR,LPLO,IMO,JMO,IJMO,
     &                        MASK_OUTPUT,NSST_OUTPUT_THIN,
     &                        NUM_NSST_FIELDS)

!-----------------------------------------------------------------------
! Subroutine: write nsst data to an nstio formatted file.
!
! Author: George Gayno/EMC
!
! Abstract: Converts nsst data from the reduced to the full
!           gaussian grid.  Then, writes the converted data
!           to an nstio file.
!
! Input files: none
!
! Output files:  
!     "chgres.out.nst"  - output nstio file
!
! History:
!   2016-04-05   Gayno - Initial version
!
! Condition codes:  all non-zero codes are fatal
!   51 - error allocating output data structure.
!   52 - error writing nstio file "chgres.out.nst".
!-----------------------------------------------------------------------

      USE NSTIO_MODULE
      USE READ_WRITE_UTILS, ONLY : UNINTERPRED

      IMPLICIT NONE

      INTEGER, INTENT(IN)  :: IMO,JMO,IJMO
      INTEGER, INTENT(IN)  :: NSST_YEAR, NSST_MON
      INTEGER, INTENT(IN)  :: NSST_DAY, NSST_HOUR
      INTEGER, INTENT(IN)  :: NUM_NSST_FIELDS
      INTEGER, INTENT(IN)  :: LPLO((JMO+1)/2)

      REAL, INTENT(IN)     :: MASK_OUTPUT(IJMO), NSST_FHOUR
      REAL, INTENT(IN)     :: NSST_OUTPUT_THIN(IJMO,NUM_NSST_FIELDS)

      INTEGER              :: IRET, NSSTO
      INTEGER, ALLOCATABLE :: KMSK(:,:)

      REAL, ALLOCATABLE    :: DUMMY2(:,:)

      TYPE(NSTIO_HEAD)     :: NSST_OUT_HEAD
      TYPE(NSTIO_DATA)     :: NSST_OUT_DATA

      print*,"- WRITE NSST DATA IN NSTIO FORMAT"

      ALLOCATE(KMSK(IMO,JMO))
      KMSK=0

      NSSTO=32
      NSST_OUT_HEAD%CLABNST="GFS NST"
      NSST_OUT_HEAD%FHOUR=NSST_FHOUR
      NSST_OUT_HEAD%IDATE(1)=NSST_HOUR
      NSST_OUT_HEAD%IDATE(2)=NSST_MON
      NSST_OUT_HEAD%IDATE(3)=NSST_DAY
      NSST_OUT_HEAD%IDATE(4)=NSST_YEAR
      NSST_OUT_HEAD%LSEA=0 ! number of layers. not used yet.
      NSST_OUT_HEAD%IVO=200907
      NSST_OUT_HEAD%IREALF=1  ! four-byte floats
      NSST_OUT_HEAD%LONB=IMO
      NSST_OUT_HEAD%LATB=JMO
      CALL NSTIO_ALHEAD(NSST_OUT_HEAD,IRET)
      NSST_OUT_HEAD%LPL=LPLO
      CALL NSTIO_ALDATA(NSST_OUT_HEAD,NSST_OUT_DATA,IRET)
      IF(IRET/=0)THEN
        PRINT*,'FATAL ERROR: ALLOCATING NSST OUTPUT DATA STRUCTURE'
        CALL ERREXIT(51)
      ENDIF
      ALLOCATE(DUMMY2(IMO,JMO))
      CALL UNINTERPRED(1,KMSK,MASK_OUTPUT,DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%SLMSK=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,1),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%XT=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,2),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%XS=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,3),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%XU=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,4),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%XV=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,5),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%XZ=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,6),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%ZM=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,7),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%XTTS=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,8),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%XZTS=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,9),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%DT_COOL=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,10),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%Z_C=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,11),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%C_0=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,12),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%C_D=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,13),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%W_0=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,14),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%W_D=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,15),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%D_CONV=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,16),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%IFD=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,17),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%TREF=DUMMY2
      CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,18),DUMMY2,
     &                 IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
      NSST_OUT_DATA%QRAIN=DUMMY2
      print*,"- WRITE NSST DATA TO chgres.out.nst"
      CALL NSTIO_SWOHDC(NSSTO,'chgres.out.nst',
     &                  NSST_OUT_HEAD,NSST_OUT_DATA,IRET)
      IF(IRET/=0)THEN
        print*,'FATAL ERROR WRITING chgres.out.nst'
        print*,'IRET IS ',iret
        CALL ERREXIT(52)
      ENDIF
      CALL NSTIO_AXDATA(NSST_OUT_DATA,IRET)
      DEALLOCATE(DUMMY2, KMSK)
      END SUBROUTINE WRITE_NSST_TO_NSTIO
!-----------------------------------------------------------------------
      SUBROUTINE WRITE_NSST_TO_NEMSIO(IMO,JMO,IJMO,YEAR,MON,DAY,HOUR,
     &           FHOUR, MASK_OUTPUT, NSST_OUTPUT_THIN, 
     &           NUM_NSST_FIELDS, LPL)

!-----------------------------------------------------------------------
! Subroutine: write nsst data to a nemsio formatted file.
!
! Author: George Gayno/EMC
!
! Abstract: Converts nsst data from the reduced to the full
!           gaussian grid.  Then, writes the converted data
!           to a nemsio file.
!
! Input files: none
!
! Output files:  
!     "chgres.out.nsn"  - output nemsio file
!
! History:
!   2016-06-05   Gayno - Initial version
!
! Condition codes:  all non-zero codes are fatal
!     26 - bad nemsio initialization
!     27 - bad open of nemsio output file
!     28 - bad write of nemsio output file
!-----------------------------------------------------------------------

      use nemsio_module
      use read_write_utils, only : uninterpred

      implicit none

      integer, intent(in)   :: imo, jmo, ijmo, year, mon, day, hour
      integer, intent(in)   :: num_nsst_fields
      integer, intent(in)   :: lpl((jmo+1)/2)

      real,    intent(in)   :: fhour, mask_output(ijmo)
      real,    intent(in)   :: nsst_output_thin(ijmo,num_nsst_fields)

      integer(nemsio_intkind), parameter     :: nrec=19
      integer(nemsio_intkind), parameter     :: nmetavari=1

      type(nemsio_gfile)        :: gfile

      character(len=7)          :: gdatatype
      character(len=3)          :: modelname
      character(len=8)          :: recname(nrec)
      character(len=3)          :: reclevtyp(nrec)
      character(len=6)          :: variname(nmetavari)

      integer(nemsio_intkind)   :: dimx, dimy, dimz, iret, j
      integer(nemsio_intkind)   :: idate(7), idrt, nsoil, ntrac
      integer(nemsio_intkind)   :: nmeta, nfhour, reclev(nrec)
      integer(nemsio_intkind)   :: varival(nmetavari)
      integer                   :: fieldsize
      integer, allocatable      :: kmsk(:,:)

      logical(nemsio_logickind) :: extrameta

      real(nemsio_realkind), allocatable :: dummy(:)
      real, allocatable                  :: dummy2(:,:)

      data recname   /"c0      ", "cd      ", "dconv   ",
     &           "dtcool  ", "ifd     ", "land    ",
     &           "qrain   ", "tref    ", "w0      ",
     &           "wd      ", "xs      ", "xt      ",
     &           "xtts    ", "xu      ", "xv      ",
     &           "xz      ", "xzts    ", "zc      ",
     &           "zm      " /

      data reclevtyp /"sfc", "sfc", "sfc", "sfc", "sfc", "sfc",
     &           "sfc", "sfc", "sfc", "sfc", "sfc", "sfc",
     &           "sfc", "sfc", "sfc", "sfc", "sfc", "sfc",
     &           "sfc" /

      data reclev   /1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     &               1, 1, 1, 1, 1, 1 /

      print*,"- WRITE NSST DATA IN NEMSIO FORMAT"

!------------------------------------------------------------------------
! First meta data record.
!------------------------------------------------------------------------

      gdatatype="bin4_be"
      modelname="GFS"
      nmeta=5  ! number meta data records.

!------------------------------------------------------------------------
! Second meta data record.  The 3rd, 4th and 5th meta data records
! are "recname", "reclevtyp" and "reclev".
!------------------------------------------------------------------------

      dimx      = imo
      dimy      = jmo
      dimz      = 1
      idrt      = 4
      idate(1)  = year
      idate(2)  = mon
      idate(3)  = day
      idate(4)  = hour
      idate(5)  = 0
      idate(6)  = 0    ! seconds numerator
      idate(7)  = 100  ! seconds denominator
      nfhour    = nint(fhour)  ! assumes whole hours
      nsoil     = 0
      ntrac     = 0
      extrameta = .true.

!------------------------------------------------------------------------
! User-defined meta data.
!------------------------------------------------------------------------

      variname(1) = "ivsnst"
      varival(1)  = 200907

!------------------------------------------------------------------------
! Write out data to nemsio file.
!------------------------------------------------------------------------

      call nemsio_init(iret=iret)
      if (iret /= 0) then
        print*,"- FATAL ERROR: bad nemsio initialization."
        print*,"- IRET IS ", iret
        call errexit(26)
      endif

      call nemsio_open(gfile, "chgres.out.nsn", "write", iret=iret,
     &  modelname=modelname,
     &  dimx=dimx, dimy=dimy, dimz=dimz, nsoil=nsoil, ntrac=ntrac,
     &  nrec=nrec, nmeta=nmeta, recname=recname, reclevtyp=reclevtyp,
     &  reclev=reclev, nfhour=nfhour,
     &  nmetavari=nmetavari, extrameta=extrameta, variname=variname,
     &  varival=varival,
     &  idrt=idrt, idate=idate, gdatatype=gdatatype)

      if (iret /= 0) then
        print*,"FATAL ERROR: bad open of file chgres.out.nsn"
        print*,"- IRET IS ", iret
        call errexit(27)
      endif

      allocate(kmsk(imo,jmo))
      kmsk = 0

      fieldsize = imo*jmo
      allocate (dummy(fieldsize))   ! full gaussian grid 1-d
      allocate (dummy2(imo,jmo))  ! full gaussian grid

      j = 1  ! c0
      call uninterpred(1,kmsk,nsst_output_thin(:,11),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 2  ! cd
      call uninterpred(1,kmsk,nsst_output_thin(:,12),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 3  ! dconv
      call uninterpred(1,kmsk,nsst_output_thin(:,15),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 4  ! dtcool
      call uninterpred(1,kmsk,nsst_output_thin(:,9),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 5  ! ifd
      call uninterpred(1,kmsk,nsst_output_thin(:,16),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 6  ! land
      call uninterpred(1,kmsk,mask_output,dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 7  ! qrain
      call uninterpred(1,kmsk,nsst_output_thin(:,18),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 8  ! tref
      call uninterpred(1,kmsk,nsst_output_thin(:,17),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 9  ! w0
      call uninterpred(1,kmsk,nsst_output_thin(:,13),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 10 ! wd
      call uninterpred(1,kmsk,nsst_output_thin(:,14),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 11 ! xs
      call uninterpred(1,kmsk,nsst_output_thin(:,2),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 12 ! xt
      call uninterpred(1,kmsk,nsst_output_thin(:,1),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 13 ! xtts
      call uninterpred(1,kmsk,nsst_output_thin(:,7),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 14 ! xu
      call uninterpred(1,kmsk,nsst_output_thin(:,3),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 15 ! xv
      call uninterpred(1,kmsk,nsst_output_thin(:,4),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 16 ! xz
      call uninterpred(1,kmsk,nsst_output_thin(:,5),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 17 ! xzts
      call uninterpred(1,kmsk,nsst_output_thin(:,8),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 18 ! zc
      call uninterpred(1,kmsk,nsst_output_thin(:,10),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      j = 19 ! zm
      call uninterpred(1,kmsk,nsst_output_thin(:,6),dummy2,
     &                 imo,jmo,ijmo,lpl)
      dummy = reshape(dummy2, (/fieldsize/))
      call nemsio_writerec(gfile,j,dummy,iret=iret)
      if (iret /= 0) goto 900

      deallocate (dummy, dummy2, kmsk)

      call nemsio_close(gfile,iret=iret)
      call nemsio_finalize()

      return

 900  continue

      print*,"- FATAL ERROR: bad write of chgres.out.nsn record ",j
      print*,"- IRET IS ", iret
      call errexit(28)

      END SUBROUTINE WRITE_NSST_TO_NEMSIO
!-----------------------------------------------------------------------
      SUBROUTINE NEWSIG(NSIL,IDVC,LEVS,NVCOORD,VCOORD,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: NEWSIG         GET NEW SIGMA STRUCTURE
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 98-04-03
!
! ABSTRACT: READ IN INTERFACE SIGMA VALUES (OR USE OLD VALUES)
!   AND COMPUTE FULL SIGMA VALUES.
!
! PROGRAM HISTORY LOG:
!   98-04-03  IREDELL
!
! USAGE:    CALL NEWSIG(NSIL,IDVC,LEVS,NVCOORD,VCOORD,IRET)
!   INPUT ARGUMENTS:
!     NSIL         INTEGER UNIT NUMBER OF NEW SIGMA INTERFACE VALUES
!     IDVC         INTEGER VERTICAL COORDINATE ID
!     LEVS         INTEGER NEW NUMBER OF LEVELS
!     NVCOORD      INTEGER NEW NUMBER OF VERTICAL COORDINATES
!   OUTPUT ARGUMENTS:
!     VCOORD       REAL (LEVS+1,NVCOORD) NEW VERTICAL COORDINATES
!     IRET         INTEGER RETURN CODE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!
!C$$$
      implicit none
!
      integer :: NSIL,IDVC,LEVS,NVCOORD,IRET
      REAL    :: VCOORD(LEVS+1,NVCOORD)
!
      integer :: IDVCI,LEVSI,NVCOORDI,k,n
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ VERTICAL COORDINATES
      READ(NSIL,*,IOSTAT=IRET) IDVCI,LEVSI,NVCOORDI
      write(0,*)' IDVCI=',IDVCI,' LEVSI=',LEVSI,' NVCOORDI=',NVCOORDI
      IF(IRET == 0) THEN
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        if (nvcoordi == 0) then  ! Added by Moorthi for gaea
          nvcoordi = nvcoord
          backspace nsil
          if (idvci > 5) then
            levsi    = idvci
            idvci    = 0
            idvc     = idvci
            nvcoordi = 1
            nvcoord  = nvcoordi
            
            backspace nsil
            READ(NSIL,*,IOSTAT=IRET) (VCOORD(K,1),K=2,LEVS)
            VCOORD(1,1)      = 1.
            VCOORD(LEVS+1,1) = 0.
          else
            READ(NSIL,*,IOSTAT=IRET)
     &                  ((VCOORD(K,N),N=1,NVCOORD),K=1,LEVS+1)
          endif
        elseif (nvcoordi <= 3) then
          READ(NSIL,*,IOSTAT=IRET)
     &                  ((VCOORD(K,N),N=1,NVCOORD),K=1,LEVS+1)
        else
          write(0,*)'nvcoordi=',nvcoordi,' not available-abort chgres'
          stop 5555
        endif
        IF(IRET    .NE. 0) RETURN
        IF(IDVCI   .NE. IDVC.OR.LEVSI .NE. LEVS) IRET = 28
        IF(NVCOORDI.NE. NVCOORD)                 IRET = 28
        IF(IRET .NE. 0) RETURN
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ INTERFACE HYBRID VALUES
      ELSE
        REWIND NSIL
        READ(NSIL,*,IOSTAT=IRET) IDVCI
        REWIND NSIL
        IF(IRET == 0 .AND. (IDVCI == 2 .OR. IDVCI == 3)) THEN
          READ(NSIL,*,IOSTAT=IRET) IDVCI, LEVSI
          READ(NSIL,*,IOSTAT=IRET) (VCOORD(K,1),VCOORD(K,2),K=1,LEVS+1)
          IF(IRET.NE.0) RETURN
          IF(IDVCI.NE.IDVC.OR.LEVSI.NE.LEVS) IRET = 28
          IF(IRET.NE.0) RETURN
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ INTERFACE SIGMA VALUES
        ELSE
          VCOORD(1,1)      = 1.
          VCOORD(LEVS+1,1) = 0.
          READ(NSIL,*,IOSTAT=IRET) LEVSI
          READ(NSIL,*,IOSTAT=IRET) (VCOORD(K,1),K=2,LEVS)
          IF(IRET.NE.0) RETURN
          IF(LEVSI.NE.LEVS) IRET = 28
          IF(IRET.NE.0) RETURN
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENDIF
      IRET=0
      END
!-----------------------------------------------------------------------
      SUBROUTINE TRSSC(JCAP,NC,KM,NTRAC,IDVM,
     &                 IDRT,LONB,LATB,IJN,J1,J2,JC,LONSPERLAT,
     &                 SZS,SPS,ST,SD,SZ,SQ,ZS,PS,T,U,V,Q
     &                 )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    TRSSC       TRANSFORM SIGMA SPECTRAL FIELDS TO GRID
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: TRANSFORMS SIGMA SPECTRAL FIELDS TO GRID AND CONVERTS
C   LOG SURFACE PRESSURE TO SURFACE PRESSURE AND VIRTUAL TEMPERATURE
C   TO TEMPERATURE.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL TRSSC(JCAP,NC,KM,NTRAC,IDVM,
C    &                 IDRT,LONB,LATB,IJN,J1,J2,JC,LONSPERLAT,
C    &                 SZS,SPS,ST,SD,SZ,SQ,ZS,PS,T,U,V,Q)
C   INPUT ARGUMENT LIST:
C     JCAP         INTEGER SPECTRAL TRUNCATION
C     NC           INTEGER FIRST DIMENSION (NC>=(JCAP+1)*(JCAP+2))
C     KM           INTEGER NUMBER OF LEVELS
C     NTRAC        INTEGER NUMBER OF TRACERS
C     IDVM         INTEGER MASS VARIABLE ID
C     IDRT         INTEGER DATA REPRESENTATION TYPE
C     LONB         INTEGER NUMBER OF LONGITUDES
C     LATB         INTEGER NUMBER OF LATITUDES
C     IJN          INTEGER HORIZONTAL DIMENSION
C     J1           INTEGER FIRST LATITUDE
C     J2           INTEGER LAST LATITUDE
C     JC           INTEGER NUMBER OF CPUS
C     LONSPERLAT   INTEGER (J1:J2) NUMBER OF LONGITUDES PER LATITUDE
C     SZS          REAL (NC) OROGRAPHY
C     SPS          REAL (NC) LOG SURFACE PRESSURE
C     ST           REAL (NC,LEVS) VIRTUAL TEMPERATURE
C     SD           REAL (NC,LEVS) DIVERGENCE
C     SZ           REAL (NC,LEVS) VORTICITY
C     SQ           REAL (NC,LEVS*NTRAC) TRACERS
C   OUTPUT ARGUMENT LIST:
C     ZS           REAL (IJN) OROGRAPHY
C     PS           REAL (IJN) SURFACE PRESSURE
C     T            REAL (IJN,KM) TEMPERATURE
C     U            REAL (IJN,KM) ZONAL WIND
C     V            REAL (IJN,KM) MERIDIONAL WIND
C     Q            REAL (IJN,KM*NTRAC) TRACERS
C
C SUBPROGRAMS CALLED:
C   SPTRAN       PERFORM A SCALAR SPHERICAL TRANSFORM
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      INTEGER LONSPERLAT(J1:J2)
      REAL SZS(NC),SPS(NC),ST(NC,KM),SD(NC,KM),SZ(NC,KM),SQ(NC,KM*NTRAC)
      REAL ZS(IJN),PS(IJN),T(IJN,KM),U(IJN,KM),V(IJN,KM),Q(IJN,KM*NTRAC)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SPECTRAL TRANSFORMS
      LONB2=LONB*2
      IJ=LONB2*(J2-J1+1)
      IN=1
      IS=1+LONB
      CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,1,1,1,LONB2,LONB2,NC,IJN,
     &            J1,J2,JC,SZS,ZS(IN),ZS(IS),1)
      CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,1,1,1,LONB2,LONB2,NC,IJN,
     &            J1,J2,JC,SPS,PS(IN),PS(IS),1)
      CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,KM,1,1,LONB2,LONB2,NC,IJN,
     &            J1,J2,JC,ST,T(IN,1),T(IS,1),1)
      CALL SPTRANV(0,JCAP,IDRT,LONB,LATB,KM,1,1,LONB2,LONB2,NC,IJN,
     &             J1,J2,JC,SD,SZ,U(IN,1),U(IS,1),V(IN,1),V(IS,1),1)
      CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,KM*NTRAC,1,1,LONB2,LONB2,NC,IJN,
     &            J1,J2,JC,SQ,Q(IN,1),Q(IS,1),1)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM TO REDUCED GRID INSTEAD
      DO J=J1,J2
        JN=LONB2*(J-J1)+IN
        JS=LONB2*(J-J1)+IS
        CALL SPTRRJ(LONB,LONSPERLAT(J),ZS(JN),ZS(JN),1)
        CALL SPTRRJ(LONB,LONSPERLAT(J),ZS(JS),ZS(JS),1)
        CALL SPTRRJ(LONB,LONSPERLAT(J),PS(JN),PS(JN),1)
        CALL SPTRRJ(LONB,LONSPERLAT(J),PS(JS),PS(JS),1)
        DO K=1,KM
          CALL SPTRRJ(LONB,LONSPERLAT(J),T(JN,K),T(JN,K),1)
          CALL SPTRRJ(LONB,LONSPERLAT(J),T(JS,K),T(JS,K),1)
          CALL SPTRRJ(LONB,LONSPERLAT(J),U(JN,K),U(JN,K),1)
          CALL SPTRRJ(LONB,LONSPERLAT(J),U(JS,K),U(JS,K),1)
          CALL SPTRRJ(LONB,LONSPERLAT(J),V(JN,K),V(JN,K),1)
          CALL SPTRRJ(LONB,LONSPERLAT(J),V(JS,K),V(JS,K),1)
        ENDDO
        DO K=1,KM*NTRAC
          CALL SPTRRJ(LONB,LONSPERLAT(J),Q(JN,K),Q(JN,K),1)
          CALL SPTRRJ(LONB,LONSPERLAT(J),Q(JS,K),Q(JS,K),1)
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CONVERT TO SURFACE PRESSURE AND TEMPERATURE
!     SELECT CASE(MOD(IDVM,10))
!     CASE(0,1)
!       DO I=1,IJ
!         PS(I)=1.E3*EXP(PS(I))
!       ENDDO
!     CASE(2)
!       DO I=1,IJ
!         PS(I)=1.E3*PS(I)
!       ENDDO
!     CASE DEFAULT
!       DO I=1,IJ
!         PS(I)=1.E3*EXP(PS(I))
!       ENDDO
!     END SELECT
!     SELECT CASE(MOD(IDVM/10,10))
!     CASE(0,1)
!       DO K=1,KM
!         DO I=1,IJ
!           T(I,K)=T(I,K)/(1.+(461.50/287.05-1)*Q(I,K))
!     if (t(i,k) .lt. 10) print *,' t=',t(i,k),' i=',i
!         ENDDO
!     print *,' T=',t(ij,k),' q=',Q(IJ,K),' k=',k
!       ENDDO
!     CASE DEFAULT
!       DO K=1,KM
!         DO I=1,IJ
!           T(I,K)=T(I,K)/(1.+(461.50/287.05-1)*Q(I,K))
!         ENDDO
!       ENDDO
!     END SELECT
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE TRBSC(JCAP,NC,KM,NTRAC,IDVM,
     &                 IDRT,LONB,LATB,IJN,J1,J2,JC,cpi,
     &                 ZS,PS,T,U,V,Q,SZS,SPS,ST,SD,SZ,SQ)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    TRBSC       TRANSFORM SIGMA GRID FIELDS TO SPECTRAL
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: CONVERTS GRIDDED SURFACE PRESSURE TO LOG SURFACE PRESSURE
C   AND TEMPERATURE TO VIRTUAL TEMPERATURE AND TRANSFORMS ALL SIGMA
C   FIELDS TO SPECTRAL SPACE.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL TRBSC(JCAP,NC,KM,NTRAC,IDVM,
C    &                 IDRT,LONB,LATB,IJN,J1,J2,JC,
C    &                 ZS,PS,T,U,V,Q,SZS,SPS,ST,SD,SZ,SQ)
C   INPUT ARGUMENT LIST:
C     JCAP         INTEGER SPECTRAL TRUNCATION
C     NC           INTEGER FIRST DIMENSION (NC>=(JCAP+1)*(JCAP+2))
C     KM           INTEGER NUMBER OF LEVELS
C     NTRAC        INTEGER NUMBER OF TRACERS
C     IDVM         INTEGER MASS VARIABLE ID
C     IDRT         INTEGER DATA REPRESENTATION TYPE
C     LONB         INTEGER NUMBER OF LONGITUDES
C     LATB         INTEGER NUMBER OF LATITUDES
C     IJN          INTEGER HORIZONTAL DIMENSION
C     J1           INTEGER FIRST LATITUDE
C     J2           INTEGER LAST LATITUDE
C     JC           INTEGER NUMBER OF CPUS
C     ZS           REAL (IJN) OROGRAPHY
C     PS           REAL (IJN) SURFACE PRESSURE
C     T            REAL (IJN,KM) TEMPERATURE
C     U            REAL (IJN,KM) ZONAL WIND
C     V            REAL (IJN,KM) MERIDIONAL WIND
C     Q            REAL (IJN,KM*NTRAC) TRACERS
C   OUTPUT ARGUMENT LIST:
C     SZS          REAL (NC) OROGRAPHY
C     SPS          REAL (NC) LOG SURFACE PRESSURE
C     ST           REAL (NC,LEVS) VIRTUAL TEMPERATURE
C     SD           REAL (NC,LEVS) DIVERGENCE
C     SZ           REAL (NC,LEVS) VORTICITY
C     SQ           REAL (NC,LEVS*NTRAC) TRACERS
C
C SUBPROGRAMS CALLED:
C   SPTRAN       PERFORM A SCALAR SPHERICAL TRANSFORM
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      real cpi(0:ntrac)
      REAL ZS(IJN),PS(IJN),T(IJN,KM),U(IJN,KM),V(IJN,KM),Q(IJN,KM*NTRAC)
      REAL SZS(NC),SPS(NC),ST(NC,KM),SD(NC,KM),SZ(NC,KM),SQ(NC,KM*NTRAC)
      real xcp, sumq
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CONVERT FROM SURFACE PRESSURE AND TEMPERATURE
      LONB2=LONB*2
      IJ=LONB2*(J2-J1+1)
      IN=1
      IS=1+LONB
      SELECT CASE(MOD(IDVM,10))
      CASE(0,1)
        DO I=1,IJ
          PS(I)=LOG(PS(I)/1.E3)
        ENDDO
      CASE(2)
        DO I=1,IJ
          PS(I)=PS(I)/1.E3
        ENDDO
      CASE DEFAULT
        PRINT *,' DEFAULT SELECTED: PSO AS IT IS '
!       DO I=1,IJ
!         PS(I)=LOG(PS(I)/1.E3)
!       ENDDO
      END SELECT
!     print *,' TRBSC PS=',ps(1:10)
      SELECT CASE(MOD(IDVM/10,10))
      CASE(0,1)
        DO K=1,KM
          DO I=1,IJ
            T(I,K)=T(I,K)*(1.+(461.50/287.05-1)*Q(I,K))
          ENDDO
        ENDDO
      CASE(2)
        print *,' TO is dry temperature'
      CASE(3)
        DO K=1,KM
          DO I=1,IJ
            xcp = 0.0
            sumq = 0.0
            do n=1,NTRAC
              if( cpi(n).ne.0.0 ) then
                xcp  = xcp  + cpi(n)*q(i,(n-1)*km+k)
                sumq = sumq + q(i,(n-1)*km+k)
              endif
            enddo
            xcp    = (1.-sumq)*cpi(0) + xcp
            T(I,K) = T(I,K) * xcp
          ENDDO
        ENDDO
      CASE DEFAULT
        DO K=1,KM
          DO I=1,IJ
            T(I,K)=T(I,K)*(1.+(461.50/287.05-1)*Q(I,K))
          ENDDO
        ENDDO
      END SELECT
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SPECTRAL TRANSFORMS
      CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,1,1,1,LONB2,LONB2,NC,IJN,
     &            J1,J2,JC,SZS,ZS(IN),ZS(IS),-1)
      CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,1,1,1,LONB2,LONB2,NC,IJN,
     &            J1,J2,JC,SPS,PS(IN),PS(IS),-1)
      CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,KM,1,1,LONB2,LONB2,NC,IJN,
     &            J1,J2,JC,ST,T(IN,1),T(IS,1),-1)
      CALL SPTRANV(0,JCAP,IDRT,LONB,LATB,KM,1,1,LONB2,LONB2,NC,IJN,
     &             J1,J2,JC,SD,SZ,U(IN,1),U(IS,1),V(IN,1),V(IS,1),-1)
      CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,KM*NTRAC,1,1,LONB2,LONB2,NC,IJN,
     &            J1,J2,JC,SQ,Q(IN,1),Q(IS,1),-1)
      END
C-----------------------------------------------------------------------
      SUBROUTINE NEWPS(IM,ZS,PS,IMX,KM,P,T,Q,ZSNEW,PSNEW)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    NEWPS       COMPUTE NEW SURFACE PRESSURE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTES A NEW SURFACE PRESSURE GIVEN A NEW OROGRAPHY.
C   THE NEW PRESSURE IS COMPUTED ASSUMING A HYDROSTATIC BALANCE
C   AND A CONSTANT TEMPERATURE LAPSE RATE.  BELOW GROUND, THE
C   LAPSE RATE IS ASSUMED TO BE -6.5 K/KM.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL NEWPS(IM,ZS,PS,IMX,KM,P,T,Q,ZSNEW,PSNEW)
C   INPUT ARGUMENT LIST:
C     IM           INTEGER NUMBER OF POINTS TO COMPUTE
C     ZS           REAL (IM) OLD OROGRAPHY (M)
C     PS           REAL (IM) OLD SURFACE PRESSURE (PA)
C     IMX          INTEGER FIRST DIMENSION
C     KM           INTEGER NUMBER OF LEVELS
C     P            REAL (IMX,KM) PRESSURES (PA)
C     T            REAL (IMX,KM) TEMPERATURES (K)
C     Q            REAL (IMX,KM) SPECIFIC HUMIDITIES (KG/KG)
C     ZSNEW        REAL (IM) NEW OROGRAPHY (M)
C   OUTPUT ARGUMENT LIST:
C     PSNEW        REAL (IM) NEW SURFACE PRESSURE (PA)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      REAL ZS(IM),PS(IM),P(IMX,KM),T(IMX,KM),Q(IMX,KM)
      REAL ZSNEW(IM),PSNEW(IM)
      PARAMETER(BETA=-6.5E-3,EPSILON=1.E-9)
      PARAMETER(G=9.80665,RD=287.05,RV=461.50)
      PARAMETER(GOR=G/RD,FV=RV/RD-1.)
      REAL ZU(IM)
      FTV(AT,AQ)=AT*(1+FV*AQ)
      FGAM(APU,ATVU,APD,ATVD)=-GOR*LOG(ATVD/ATVU)/LOG(APD/APU)
      FZ0(AP,ATV,AZD,APD)=AZD+ATV/GOR*LOG(APD/AP)
      FZ1(AP,ATV,AZD,APD,AGAM)=AZD-ATV/AGAM*((APD/AP)**(-AGAM/GOR)-1)
      FP0(AZ,AZU,APU,ATVU)=APU*EXP(-GOR/ATVU*(AZ-AZU))
      FP1(AZ,AZU,APU,ATVU,AGAM)=APU*(1+AGAM/ATVU*(AZ-AZU))**(-GOR/AGAM)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE SURFACE PRESSURE BELOW THE ORIGINAL GROUND
      LS=0
      K=1
      GAMMA=BETA
      DO I=1,IM
!       if (zsnew(i) == zs(i)) then
!         psnew(i) = ps(i)
!       else
          PU=P(I,K)
          TVU=FTV(T(I,K),Q(I,K))
          ZU(I)=FZ1(PU,TVU,ZS(I),PS(I),GAMMA)
          IF(ZSNEW(I).LE.ZU(I)) THEN
            PU=P(I,K)
            TVU=FTV(T(I,K),Q(I,K))
            IF(ABS(GAMMA).GT.EPSILON) THEN
              PSNEW(I)=FP1(ZSNEW(I),ZU(I),PU,TVU,GAMMA)
            ELSE
              PSNEW(I)=FP0(ZSNEW(I),ZU(I),PU,TVU)
            ENDIF
          ELSE
            PSNEW(I)=0
            LS=LS+1
          ENDIF
!       endif
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE SURFACE PRESSURE ABOVE THE ORIGINAL GROUND
      DO K=2,KM
        IF(LS.GT.0) THEN
          DO I=1,IM
            IF(PSNEW(I).EQ.0) THEN
              PU=P(I,K)
              TVU=FTV(T(I,K),Q(I,K))
              PD=P(I,K-1)
              TVD=FTV(T(I,K-1),Q(I,K-1))
              GAMMA=FGAM(PU,TVU,PD,TVD)
              IF(ABS(GAMMA).GT.EPSILON) THEN
                ZU(I)=FZ1(PU,TVU,ZU(I),PD,GAMMA)
              ELSE
                ZU(I)=FZ0(PU,TVU,ZU(I),PD)
              ENDIF
              IF(ZSNEW(I).LE.ZU(I)) THEN
                IF(ABS(GAMMA).GT.EPSILON) THEN
                  PSNEW(I)=FP1(ZSNEW(I),ZU(I),PU,TVU,GAMMA)
                ELSE
                  PSNEW(I)=FP0(ZSNEW(I),ZU(I),PU,TVU)
                ENDIF
                LS=LS-1
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE SURFACE PRESSURE OVER THE TOP
      IF(LS.GT.0) THEN
        K=KM
        GAMMA=0
        DO I=1,IM
          IF(PSNEW(I).EQ.0) THEN
            PU=P(I,K)
            TVU=FTV(T(I,K),Q(I,K))
            PSNEW(I)=FP0(ZSNEW(I),ZU(I),PU,TVU)
          ENDIF
        ENDDO
      ENDIF
      END
C-----------------------------------------------------------------------
      SUBROUTINE VINTG(IM,IX,KM1,KM2,NT,P1,U1,V1,T1,Q1,W1,P2,
     &                 U2,V2,T2,Q2,DTDP2,W2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    VINTG       VERTICALLY INTERPOLATE UPPER-AIR FIELDS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: VERTICALLY INTERPOLATE UPPER-AIR FIELDS.
C   WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS ARE INTERPOLATED.
C   THE INTERPOLATION IS CUBIC LAGRANGIAN IN LOG PRESSURE
C   WITH A MONOTONIC CONSTRAINT IN THE CENTER OF THE DOMAIN.
C   IN THE OUTER INTERVALS IT IS LINEAR IN LOG PRESSURE.
C   OUTSIDE THE DOMAIN, FIELDS ARE GENERALLY HELD CONSTANT,
C   EXCEPT FOR TEMPERATURE AND HUMIDITY BELOW THE INPUT DOMAIN,
C   WHERE THE TEMPERATURE LAPSE RATE IS HELD FIXED AT -6.5 K/KM AND
C   THE RELATIVE HUMIDITY IS HELD CONSTANT.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL VINTG(IM,IX,KM1,KM2,NT,P1,U1,V1,T1,Q1,P2,
C    &                 U2,V2,T2,Q2)
C   INPUT ARGUMENT LIST:
C     IM           INTEGER NUMBER OF POINTS TO COMPUTE
C     IX           INTEGER FIRST DIMENSION
C     KM1          INTEGER NUMBER OF INPUT LEVELS
C     KM2          INTEGER NUMBER OF OUTPUT LEVELS
C     NT           INTEGER NUMBER OF TRACERS
C     P1           REAL (IX,KM1) INPUT PRESSURES
C                  ORDERED FROM BOTTOM TO TOP OF ATMOSPHERE
C     U1           REAL (IX,KM1) INPUT ZONAL WIND
C     V1           REAL (IX,KM1) INPUT MERIDIONAL WIND
C     T1           REAL (IX,KM1) INPUT TEMPERATURE (K)
C     Q1           REAL (IX,KM1,NT) INPUT TRACERS (HUMIDITY FIRST)
C     P2           REAL (IX,KM2) OUTPUT PRESSURES
C   OUTPUT ARGUMENT LIST:
C     U2           REAL (IX,KM2) OUTPUT ZONAL WIND
C     V2           REAL (IX,KM2) OUTPUT MERIDIONAL WIND
C     T2           REAL (IX,KM2) OUTPUT TEMPERATURE (K)
C     Q2           REAL (IX,KM2,NT) OUTPUT TRACERS (HUMIDITY FIRST)
C     DTDP2        REAL (IX,KM2) OUTPUT DTDP
C
C SUBPROGRAMS CALLED:
C   TERP3        CUBICALLY INTERPOLATE IN ONE DIMENSION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      REAL P1(IX,KM1),U1(IX,KM1),V1(IX,KM1),T1(IX,KM1),Q1(IX,KM1,NT)
     &     ,W1(IX,KM1)
      REAL P2(IX,KM2),U2(IX,KM2),V2(IX,KM2),T2(IX,KM2),Q2(IX,KM2,NT)
     &     ,W2(IX,KM2)
      REAL,optional :: DTDP2(IX,KM2)
      PARAMETER(DLTDZ=-6.5E-3*287.05/9.80665)
      PARAMETER(DLPVDRT=-2.5E6/461.50)

      REAL,allocatable :: Z1(:,:),Z2(:,:)
      REAL,allocatable :: C1(:,:,:),C2(:,:,:),J2(:,:,:)
      real (kind=8) timef,mbytes,print_memory
      integer             :: ijaa
!
      allocate (Z1(IM+1,KM1),Z2(IM+1,KM2))
      allocate (C1(IM+1,KM1,4+NT),C2(IM+1,KM2,4+NT),J2(IM+1,KM2,4+NT))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE LOG PRESSURE INTERPOLATING COORDINATE
!  AND COPY INPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS
!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(K,I)
      DO K=1,KM1
        DO I=1,IM
          Z1(I,K)   = -LOG(P1(I,K))
          C1(I,K,1) =  U1(I,K)
          C1(I,K,2) =  V1(I,K)
          C1(I,K,3) =  W1(I,K)
          C1(I,K,4) =  T1(I,K)
          C1(I,K,5) =  Q1(I,K,1)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
      DO N=2,NT
        DO K=1,KM1
          DO I=1,IM
            C1(I,K,4+N) = Q1(I,K,N)
          ENDDO
        ENDDO
      ENDDO
!      print *,' p2=',p2(1,:)
!      print *,' im=',im,' km2=',km2,' ix=',ix,'nt=',nt
!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(K,I)
      DO K=1,KM2
        DO I=1,IM
          Z2(I,K) = -LOG(P2(I,K))
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PERFORM LAGRANGIAN ONE-DIMENSIONAL INTERPOLATION
C  THAT IS 4TH-ORDER IN INTERIOR, 2ND-ORDER IN OUTSIDE INTERVALS
C  AND 1ST-ORDER FOR EXTRAPOLATION.
      CALL TERP3(IM,1,1,1,1,4+NT,(IM+1)*KM1,(IM+1)*KM2,
     &           KM1,IM+1,IM+1,Z1,C1,KM2,IM+1,IM+1,Z2,C2,J2)
!      print *,' c2=',maxval(c2(1,:,:)),minval(c2(1,:,:))
!     print *,' j2:=',j2(1,1,:)
!     print *,' j2:=',j2(im,km2,:)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COPY OUTPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS
C  EXCEPT BELOW THE INPUT DOMAIN, LET TEMPERATURE INCREASE WITH A FIXED
C  LAPSE RATE AND LET THE RELATIVE HUMIDITY REMAIN CONSTANT.
      DO K=1,KM2
        DO I=1,IM
          U2(I,K)=C2(I,K,1)
          V2(I,K)=C2(I,K,2)
          W2(I,K)=C2(I,K,3)
          DZ=Z2(I,K)-Z1(I,1)
          IF(DZ.GE.0) THEN
            T2(I,K)=C2(I,K,4)
            Q2(I,K,1)=C2(I,K,5)
!jaa        DTDP2(I,K)=-J2(I,K,4)/P2(I,K)
          ELSE
            T2(I,K)=T1(I,1)*EXP(DLTDZ*DZ)
            Q2(I,K,1)=Q1(I,1,1)*EXP(DLPVDRT*(1/T2(I,K)-1/T1(I,1))-DZ)
!jaa        DTDP2(I,K)=-T2(I,K)*DLTDZ/P2(I,K)
          ENDIF
        ENDDO
      ENDDO
      DO N=2,NT
        DO K=1,KM2
          DO I=1,IM
            Q2(I,K,N)=C2(I,K,4+N)
          ENDDO
        ENDDO
      ENDDO
      deallocate (Z1,Z2,C1,C2,J2)
      END
C-----------------------------------------------------------------------
      SUBROUTINE TERP3(IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2,
     &                 KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2,KXQ2,Z2,Q2,J2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    TERP3       CUBICALLY INTERPOLATE IN ONE DIMENSION
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 98-05-01
C
C ABSTRACT: INTERPOLATE FIELD(S) IN ONE DIMENSION ALONG THE COLUMN(S).
C   THE INTERPOLATION IS CUBIC LAGRANGIAN WITH A MONOTONIC CONSTRAINT
C   IN THE CENTER OF THE DOMAIN.  IN THE OUTER INTERVALS IT IS LINEAR.
C   OUTSIDE THE DOMAIN, FIELDS ARE HELD CONSTANT.
C
C PROGRAM HISTORY LOG:
C   98-05-01  MARK IREDELL
C 1999-01-04  IREDELL  USE ESSL SEARCH
C
C USAGE:    CALL TERP3(IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2,
C    &                 KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2,KXQ2,Z2,Q2,J2)
C   INPUT ARGUMENT LIST:
C     IM           INTEGER NUMBER OF COLUMNS
C     IXZ1         INTEGER COLUMN SKIP NUMBER FOR Z1
C     IXQ1         INTEGER COLUMN SKIP NUMBER FOR Q1
C     IXZ2         INTEGER COLUMN SKIP NUMBER FOR Z2
C     IXQ2         INTEGER COLUMN SKIP NUMBER FOR Q2
C     NM           INTEGER NUMBER OF FIELDS PER COLUMN
C     NXQ1         INTEGER FIELD SKIP NUMBER FOR Q1
C     NXQ2         INTEGER FIELD SKIP NUMBER FOR Q2
C     KM1          INTEGER NUMBER OF INPUT POINTS
C     KXZ1         INTEGER POINT SKIP NUMBER FOR Z1
C     KXQ1         INTEGER POINT SKIP NUMBER FOR Q1
C     Z1           REAL (1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
C                  INPUT COORDINATE VALUES IN WHICH TO INTERPOLATE
C                  (Z1 MUST BE STRICTLY MONOTONIC IN EITHER DIRECTION)
C     Q1           REAL (1+(IM-1)*IXQ1+(KM1-1)*KXQ1+(NM-1)*NXQ1)
C                  INPUT FIELDS TO INTERPOLATE
C     KM2          INTEGER NUMBER OF OUTPUT POINTS
C     KXZ2         INTEGER POINT SKIP NUMBER FOR Z2
C     KXQ2         INTEGER POINT SKIP NUMBER FOR Q2
C     Z2           REAL (1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
C                  OUTPUT COORDINATE VALUES TO WHICH TO INTERPOLATE
C                  (Z2 NEED NOT BE MONOTONIC)
C     
C   OUTPUT ARGUMENT LIST:
C     Q2           REAL (1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)
C                  OUTPUT INTERPOLATED FIELDS
C     J2           REAL (1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)
C                  OUTPUT INTERPOLATED FIELDS CHANGE WRT Z2
C
C SUBPROGRAMS CALLED:
C   RSEARCH      SEARCH FOR A SURROUNDING REAL INTERVAL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      IMPLICIT NONE
      INTEGER IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2
      INTEGER KM1,KXZ1,KXQ1,KM2,KXZ2,KXQ2
      INTEGER I,K1,K2,N
      REAL Z1(1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
      REAL Q1(1+(IM-1)*IXQ1+(KM1-1)*KXQ1+(NM-1)*NXQ1)
      REAL Z2(1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
      REAL Q2(1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)
      REAL J2(1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)
      REAL FFA(IM),FFB(IM),FFC(IM),FFD(IM)
      REAL GGA(IM),GGB(IM),GGC(IM),GGD(IM)
      INTEGER K1S(IM,KM2)
      REAL Z1A,Z1B,Z1C,Z1D,Q1A,Q1B,Q1C,Q1D,Z2S,Q2S,J2S
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FIND THE SURROUNDING INPUT INTERVAL FOR EACH OUTPUT POINT.
      CALL RSEARCH(IM,KM1,IXZ1,KXZ1,Z1,KM2,IXZ2,KXZ2,Z2,1,IM,K1S)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GENERALLY INTERPOLATE CUBICALLY WITH MONOTONIC CONSTRAINT
C  FROM TWO NEAREST INPUT POINTS ON EITHER SIDE OF THE OUTPUT POINT,
C  BUT WITHIN THE TWO EDGE INTERVALS INTERPOLATE LINEARLY.
C  KEEP THE OUTPUT FIELDS CONSTANT OUTSIDE THE INPUT DOMAIN.

!$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(IM,IXZ1,IXQ1,IXZ2)
!$OMP+ SHARED(IXQ2,NM,NXQ1,NXQ2,KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2)
!$OMP+ SHARED(KXQ2,Z2,Q2,J2,K1S)

      DO K2=1,KM2
        DO I=1,IM
          K1=K1S(I,K2)
          IF(K1.EQ.1.OR.K1.EQ.KM1-1) THEN
            Z2S=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2)
            Z1A=Z1(1+(I-1)*IXZ1+(K1-1)*KXZ1)
            Z1B=Z1(1+(I-1)*IXZ1+(K1+0)*KXZ1)
            FFA(I)=(Z2S-Z1B)/(Z1A-Z1B)
            FFB(I)=(Z2S-Z1A)/(Z1B-Z1A)
            GGA(I)=1/(Z1A-Z1B)
            GGB(I)=1/(Z1B-Z1A)
          ELSEIF(K1.GT.1.AND.K1.LT.KM1-1) THEN
            Z2S=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2)
            Z1A=Z1(1+(I-1)*IXZ1+(K1-2)*KXZ1)
            Z1B=Z1(1+(I-1)*IXZ1+(K1-1)*KXZ1)
            Z1C=Z1(1+(I-1)*IXZ1+(K1+0)*KXZ1)
            Z1D=Z1(1+(I-1)*IXZ1+(K1+1)*KXZ1)
            FFA(I)=(Z2S-Z1B)/(Z1A-Z1B)*
     &             (Z2S-Z1C)/(Z1A-Z1C)*
     &             (Z2S-Z1D)/(Z1A-Z1D)
            FFB(I)=(Z2S-Z1A)/(Z1B-Z1A)*
     &             (Z2S-Z1C)/(Z1B-Z1C)*
     &             (Z2S-Z1D)/(Z1B-Z1D)
            FFC(I)=(Z2S-Z1A)/(Z1C-Z1A)*
     &             (Z2S-Z1B)/(Z1C-Z1B)*
     &             (Z2S-Z1D)/(Z1C-Z1D)
            FFD(I)=(Z2S-Z1A)/(Z1D-Z1A)*
     &             (Z2S-Z1B)/(Z1D-Z1B)*
     &             (Z2S-Z1C)/(Z1D-Z1C)
            GGA(I)=        1/(Z1A-Z1B)*
     &             (Z2S-Z1C)/(Z1A-Z1C)*
     &             (Z2S-Z1D)/(Z1A-Z1D)+
     &             (Z2S-Z1B)/(Z1A-Z1B)*
     &                     1/(Z1A-Z1C)*
     &             (Z2S-Z1D)/(Z1A-Z1D)+
     &             (Z2S-Z1B)/(Z1A-Z1B)*
     &             (Z2S-Z1C)/(Z1A-Z1C)*
     &                     1/(Z1A-Z1D)
            GGB(I)=        1/(Z1B-Z1A)*
     &             (Z2S-Z1C)/(Z1B-Z1C)*
     &             (Z2S-Z1D)/(Z1B-Z1D)+
     &             (Z2S-Z1A)/(Z1B-Z1A)*
     &                     1/(Z1B-Z1C)*
     &             (Z2S-Z1D)/(Z1B-Z1D)+
     &             (Z2S-Z1A)/(Z1B-Z1A)*
     &             (Z2S-Z1C)/(Z1B-Z1C)*
     &                     1/(Z1B-Z1D)
            GGC(I)=        1/(Z1C-Z1A)*
     &             (Z2S-Z1B)/(Z1C-Z1B)*
     &             (Z2S-Z1D)/(Z1C-Z1D)+
     &             (Z2S-Z1A)/(Z1C-Z1A)*
     &                     1/(Z1C-Z1B)*
     &             (Z2S-Z1D)/(Z1C-Z1D)+
     &             (Z2S-Z1A)/(Z1C-Z1A)*
     &             (Z2S-Z1B)/(Z1C-Z1B)*
     &                     1/(Z1C-Z1D)
            GGD(I)=        1/(Z1D-Z1A)*
     &             (Z2S-Z1B)/(Z1D-Z1B)*
     &             (Z2S-Z1C)/(Z1D-Z1C)+
     &             (Z2S-Z1A)/(Z1D-Z1A)*
     &                     1/(Z1D-Z1B)*
     &             (Z2S-Z1C)/(Z1D-Z1C)+
     &             (Z2S-Z1A)/(Z1D-Z1A)*
     &             (Z2S-Z1B)/(Z1D-Z1B)*
     &                     1/(Z1D-Z1C)
          ENDIF
        ENDDO
C  INTERPOLATE.
        DO N=1,NM
          DO I=1,IM
            K1=K1S(I,K2)
            IF(K1.EQ.0) THEN
              Q2S=Q1(1+(I-1)*IXQ1+(N-1)*NXQ1)
              J2S=0
            ELSEIF(K1.EQ.KM1) THEN
              Q2S=Q1(1+(I-1)*IXQ1+(KM1-1)*KXQ1+(N-1)*NXQ1)
              J2S=0
            ELSEIF(K1.EQ.1.OR.K1.EQ.KM1-1) THEN
              Q1A=Q1(1+(I-1)*IXQ1+(K1-1)*KXQ1+(N-1)*NXQ1)
              Q1B=Q1(1+(I-1)*IXQ1+(K1+0)*KXQ1+(N-1)*NXQ1)
              Q2S=FFA(I)*Q1A+FFB(I)*Q1B
              J2S=GGA(I)*Q1A+GGB(I)*Q1B
            ELSE
              Q1A=Q1(1+(I-1)*IXQ1+(K1-2)*KXQ1+(N-1)*NXQ1)
              Q1B=Q1(1+(I-1)*IXQ1+(K1-1)*KXQ1+(N-1)*NXQ1)
              Q1C=Q1(1+(I-1)*IXQ1+(K1+0)*KXQ1+(N-1)*NXQ1)
              Q1D=Q1(1+(I-1)*IXQ1+(K1+1)*KXQ1+(N-1)*NXQ1)
              Q2S=FFA(I)*Q1A+FFB(I)*Q1B+FFC(I)*Q1C+FFD(I)*Q1D
              J2S=GGA(I)*Q1A+GGB(I)*Q1B+GGC(I)*Q1C+GGD(I)*Q1D
              IF(Q2S.LT.MIN(Q1B,Q1C)) THEN
                Q2S=MIN(Q1B,Q1C)
                J2S=0
              ELSEIF(Q2S.GT.MAX(Q1B,Q1C)) THEN
                Q2S=MAX(Q1B,Q1C)
                J2S=0
              ENDIF
            ENDIF
            Q2(1+(I-1)*IXQ2+(K2-1)*KXQ2+(N-1)*NXQ2)=Q2S
            J2(1+(I-1)*IXQ2+(K2-1)*KXQ2+(N-1)*NXQ2)=J2S
          ENDDO
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE RSEARCH(IM,KM1,IXZ1,KXZ1,Z1,KM2,IXZ2,KXZ2,Z2,IXL2,KXL2,
     &                   L2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RSEARCH     SEARCH FOR A SURROUNDING REAL INTERVAL
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 98-05-01
C
C ABSTRACT: THIS SUBPROGRAM SEARCHES MONOTONIC SEQUENCES OF REAL NUMBERS
C   FOR INTERVALS THAT SURROUND A GIVEN SEARCH SET OF REAL NUMBERS.
C   THE SEQUENCES MAY BE MONOTONIC IN EITHER DIRECTION; THE REAL NUMBERS
C   MAY BE SINGLE OR DOUBLE PRECISION; THE INPUT SEQUENCES AND SETS
C   AND THE OUTPUT LOCATIONS MAY BE ARBITRARILY DIMENSIONED.
C
C PROGRAM HISTORY LOG:
C 1999-01-05  MARK IREDELL
C
C USAGE:    CALL RSEARCH(IM,KM1,IXZ1,KXZ1,Z1,KM2,IXZ2,KXZ2,Z2,IXL2,KXL2,
C    &                   L2)
C   INPUT ARGUMENT LIST:
C     IM           INTEGER NUMBER OF SEQUENCES TO SEARCH
C     KM1          INTEGER NUMBER OF POINTS IN EACH SEQUENCE
C     IXZ1         INTEGER SEQUENCE SKIP NUMBER FOR Z1
C     KXZ1         INTEGER POINT SKIP NUMBER FOR Z1
C     Z1           REAL (1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
C                  SEQUENCE VALUES TO SEARCH
C                  (Z1 MUST BE MONOTONIC IN EITHER DIRECTION)
C     KM2          INTEGER NUMBER OF POINTS TO SEARCH FOR
C                  IN EACH RESPECTIVE SEQUENCE
C     IXZ2         INTEGER SEQUENCE SKIP NUMBER FOR Z2
C     KXZ2         INTEGER POINT SKIP NUMBER FOR Z2
C     Z2           REAL (1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
C                  SET OF VALUES TO SEARCH FOR
C                  (Z2 NEED NOT BE MONOTONIC)
C     IXL2         INTEGER SEQUENCE SKIP NUMBER FOR L2
C     KXL2         INTEGER POINT SKIP NUMBER FOR L2
C     
C   OUTPUT ARGUMENT LIST:
C     L2           INTEGER (1+(IM-1)*IXL2+(KM2-1)*KXL2)
C                  INTERVAL LOCATIONS HAVING VALUES FROM 0 TO KM1
C                  (Z2 WILL BE BETWEEN Z1(L2) AND Z1(L2+1))
C
C SUBPROGRAMS CALLED:
C   SBSRCH       ESSL BINARY SEARCH
C   DBSRCH       ESSL BINARY SEARCH
C
C REMARKS:
C   IF THE ARRAY Z1 IS DIMENSIONED (IM,KM1), THEN THE SKIP NUMBERS ARE
C   IXZ1=1 AND KXZ1=IM; IF IT IS DIMENSIONED (KM1,IM), THEN THE SKIP
C   NUMBERS ARE IXZ1=KM1 AND KXZ1=1; IF IT IS DIMENSIONED (IM,JM,KM1),
C   THEN THE SKIP NUMBERS ARE IXZ1=1 AND KXZ1=IM*JM; ETCETERA.
C   SIMILAR EXAMPLES APPLY TO THE SKIP NUMBERS FOR Z2 AND L2.
C
C   RETURNED VALUES OF 0 OR KM1 INDICATE THAT THE GIVEN SEARCH VALUE
C   IS OUTSIDE THE RANGE OF THE SEQUENCE.
C
C   IF A SEARCH VALUE IS IDENTICAL TO ONE OF THE SEQUENCE VALUES
C   THEN THE LOCATION RETURNED POINTS TO THE IDENTICAL VALUE.
C   IF THE SEQUENCE IS NOT STRICTLY MONOTONIC AND A SEARCH VALUE IS
C   IDENTICAL TO MORE THAN ONE OF THE SEQUENCE VALUES, THEN THE
C   LOCATION RETURNED MAY POINT TO ANY OF THE IDENTICAL VALUES.
C
C   TO BE EXACT, FOR EACH I FROM 1 TO IM AND FOR EACH K FROM 1 TO KM2,
C   Z=Z2(1+(I-1)*IXZ2+(K-1)*KXZ2) IS THE SEARCH VALUE AND
C   L=L2(1+(I-1)*IXL2+(K-1)*KXL2) IS THE LOCATION RETURNED.
C   IF L=0, THEN Z IS LESS THAN THE START POINT Z1(1+(I-1)*IXZ1)
C   FOR ASCENDING SEQUENCES (OR GREATER THAN FOR DESCENDING SEQUENCES).
C   IF L=KM1, THEN Z IS GREATER THAN OR EQUAL TO THE END POINT
C   Z1(1+(I-1)*IXZ1+(KM1-1)*KXZ1) FOR ASCENDING SEQUENCES
C   (OR LESS THAN OR EQUAL TO FOR DESCENDING SEQUENCES).
C   OTHERWISE Z IS BETWEEN THE VALUES Z1(1+(I-1)*IXZ1+(L-1)*KXZ1) AND
C   Z1(1+(I-1)*IXZ1+(L-0)*KXZ1) AND MAY EQUAL THE FORMER.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
!     IMPLICIT NONE
!     INTEGER,INTENT(IN):: IM,KM1,IXZ1,KXZ1,KM2,IXZ2,KXZ2,IXL2,KXL2
!     REAL,INTENT(IN):: Z1(1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
!     REAL,INTENT(IN):: Z2(1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
!     INTEGER,INTENT(OUT):: L2(1+(IM-1)*IXL2+(KM2-1)*KXL2)
!     INTEGER(4) INCX,N,INCY,M,INDX(KM2),RC(KM2),IOPT
!     INTEGER I,K2
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FIND THE SURROUNDING INPUT INTERVAL FOR EACH OUTPUT POINT.
!     DO I=1,IM
!       IF(Z1(1+(I-1)*IXZ1).LE.Z1(1+(I-1)*IXZ1+(KM1-1)*KXZ1)) THEN
C  INPUT COORDINATE IS MONOTONICALLY ASCENDING.
!         INCX=KXZ2
!         N=KM2
!         INCY=KXZ1
!         M=KM1
!         IOPT=1
!         IF(DIGITS(1.).LT.DIGITS(1._8)) THEN
!           CALL SBSRCH(Z2(1+(I-1)*IXZ2),INCX,N,
!    &                  Z1(1+(I-1)*IXZ1),INCY,M,INDX,RC,IOPT)
!         ELSE
!           CALL DBSRCH(Z2(1+(I-1)*IXZ2),INCX,N,
!    &                  Z1(1+(I-1)*IXZ1),INCY,M,INDX,RC,IOPT)
!         ENDIF
!         DO K2=1,KM2
!           L2(1+(I-1)*IXL2+(K2-1)*KXL2)=INDX(K2)-RC(K2)
!         ENDDO
!       ELSE
!  INPUT COORDINATE IS MONOTONICALLY DESCENDING.
!         INCX=KXZ2
!         N=KM2
!         INCY=-KXZ1
!         M=KM1
!         IOPT=0
!         IF(DIGITS(1.).LT.DIGITS(1._8)) THEN
!           CALL SBSRCH(Z2(1+(I-1)*IXZ2),INCX,N,
!    &                  Z1(1+(I-1)*IXZ1),INCY,M,INDX,RC,IOPT)
!         ELSE
!           CALL DBSRCH(Z2(1+(I-1)*IXZ2),INCX,N,
!    &                  Z1(1+(I-1)*IXZ1),INCY,M,INDX,RC,IOPT)
!         ENDIF
!         DO K2=1,KM2
!           L2(1+(I-1)*IXL2+(K2-1)*KXL2)=KM1+1-INDX(K2)
!         ENDDO
!       ENDIF
!     ENDDO
!
      IMPLICIT NONE
      INTEGER,INTENT(IN):: IM,KM1,IXZ1,KXZ1,KM2,IXZ2,KXZ2,IXL2,KXL2
      REAL,INTENT(IN):: Z1(1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
      REAL,INTENT(IN):: Z2(1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
      INTEGER,INTENT(OUT):: L2(1+(IM-1)*IXL2+(KM2-1)*KXL2)
      INTEGER I,K2,L,L0,LI
      REAL Z
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  FIND THE SURROUNDING INPUT INTERVAL FOR EACH OUTPUT POINT.
      DO I=1,IM
        IF(Z1(1+(I-1)*IXZ1).LE.Z1(1+(I-1)*IXZ1+(KM1-1)*KXZ1)) THEN
!C  INPUT COORDINATE IS MONOTONICALLY ASCENDING.
          DO K2=1,KM2
            Z=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2)
            L=0
            DO
              IF(Z.LT.Z1(1+(I-1)*IXZ1+L*KXZ1)) EXIT
              L=L+1
              IF(L.EQ.KM1) EXIT
            ENDDO
            L2(1+(I-1)*IXL2+(K2-1)*KXL2)=L
          ENDDO
        ELSE
!C  INPUT COORDINATE IS MONOTONICALLY DESCENDING.
          DO K2=1,KM2
            Z=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2)
            L=0
            DO
              IF(Z.GT.Z1(1+(I-1)*IXZ1+L*KXZ1)) EXIT
              L=L+1
              IF(L.EQ.KM1) EXIT
            ENDDO
            L2(1+(I-1)*IXL2+(K2-1)*KXL2)=L
          ENDDO
        ENDIF
      ENDDO

      END SUBROUTINE
C-----------------------------------------------------------------------
      SUBROUTINE PADSSC(JCAPI,NCI,JCAP,NC,LEVS,NTRAC,
     &                  SZSI,SPSI,STI,SDI,SZI,SQI,
     &                  SZS,SPS,ST,SD,SZ,SQ)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    PADSSC      PAD OR TRUNCATE SPECTRAL FIELDS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: PAD OR TRUNCATE SPECTRAL FIELDS.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL PADSSC(JCAPI,NCI,JCAP,NC,LEVS,NTRAC,
C    &                  SZSI,SPSI,STI,SDI,SZI,SQI,
C    &                  SZS,SPS,ST,SD,SZ,SQ)
C   INPUT ARGUMENT LIST:
C     JCAPI        INTEGER INPUT SPECTRAL TRUNCATION
C     NCI          INTEGER INPUT NUMBER OF SPECTRAL COEFFICIENTS
C     JCAP         INTEGER OUTPUT SPECTRAL TRUNCATION
C     NC           INTEGER OUTPUT NUMBER OF SPECTRAL COEFFICIENTS
C     LEVS         INTEGER NUMBER OF LEVELS
C     NTRAC        INTEGER NUMBER OF TRACERS
C     SZSI         REAL (NCI) INPUT OROGRAPHY
C     SPSI         REAL (NCI) INPUT LOG SURFACE PRESSURE
C     STI          REAL (NCI,LEVS) INPUT VIRTUAL TEMPERATURE
C     SDI          REAL (NCI,LEVS) INPUT DIVERGENCE
C     SZI          REAL (NCI,LEVS) INPUT VORTICITY
C     SQI          REAL (NCI,LEVS*NTRAC) INPUT TRACERS
C   OUTPUT ARGUMENT LIST:
C     SZS          REAL (NC) OUTPUT OROGRAPHY
C     SPS          REAL (NC) OUTPUT LOG SURFACE PRESSURE
C     ST           REAL (NC,LEVS) OUTPUT VIRTUAL TEMPERATURE
C     SD           REAL (NC,LEVS) OUTPUT DIVERGENCE
C     SZ           REAL (NC,LEVS) OUTPUT VORTICITY
C     SQ           REAL (NC,LEVS*NTRAC) OUTPUT TRACERS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      REAL SZSI(NCI),SPSI(NCI),
     &     STI(NCI,LEVS),SDI(NCI,LEVS),SZI(NCI,LEVS),SQI(NCI,LEVS*NTRAC)
      REAL SZS(NC),SPS(NC),
     &     ST(NC,LEVS),SD(NC,LEVS),SZ(NC,LEVS),SQ(NC,LEVS*NTRAC)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL SPPAD(0,JCAPI,SZSI,0,JCAP,SZS)
      CALL SPPAD(0,JCAPI,SPSI,0,JCAP,SPS)
CMIC$ DO ALL PRIVATE(K) SHARED(LEVS,JCAPI,JCAP,STI,SDI,SZI,ST,SD,SZ)
      DO K=1,LEVS
        CALL SPPAD(0,JCAPI,STI(1,K),0,JCAP,ST(1,K))
        CALL SPPAD(0,JCAPI,SDI(1,K),0,JCAP,SD(1,K))
        CALL SPPAD(0,JCAPI,SZI(1,K),0,JCAP,SZ(1,K))
      ENDDO
CMIC$ DO ALL PRIVATE(K) SHARED(LEVS,NTRAC,JCAPI,JCAP,SQI,SQ)
      DO K=1,LEVS*NTRAC
        CALL SPPAD(0,JCAPI,SQI(1,K),0,JCAP,SQ(1,K))
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE SPPAD(I1,M1,Q1,I2,M2,Q2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPPAD       PAD OR TRUNCATE A SPECTRAL FIELD
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: PAD OR TRUNCATE A SPECTRAL FIELD
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPPAD(I1,M1,Q1,I2,M2,Q2)
C
C   INPUT ARGUMENT LIST:
C     I1       - INTEGER INPUT SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M1       - INTEGER INPUT SPECTRAL TRUNCATION
C     Q1       - REAL ((M+1)*((I+1)*M+2)) INPUT FIELD
C     I2       - INTEGER OUTPUT SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M2       - INTEGER OUTPUT SPECTRAL TRUNCATION
C
C   OUTPUT ARGUMENT LIST:
C     Q2       - REAL ((M+1)*((I+1)*M+2)) OUTPUT FIELD
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      REAL Q1((M1+1)*((I1+1)*M1+2))
      REAL Q2((M2+1)*((I2+1)*M2+2))
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      DO L=0,M2
        DO N=L,I2*L+M2
          KS2=L*(2*M2+(I2-1)*(L-1))+2*N
          IF(L.LE.M1.AND.N.LE.I1*L+M1) THEN
            KS1=L*(2*M1+(I1-1)*(L-1))+2*N
            Q2(KS2+1)=Q1(KS1+1)
            Q2(KS2+2)=Q1(KS1+2)
          ELSE
            Q2(KS2+1)=0
            Q2(KS2+2)=0
          ENDIF
        ENDDO
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE GL2GL(IP,KM,G1,IM1,JM1,G2,IM2,JM2,IDRTI,IDRTO,rlat1,
     &                    rlat2,JMO)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GL2GL       INTERPOLATE GAUSSIAN GRID TO GAUSSIAN GRID
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: LINEARLY INTERPOLATES GAUSSIAN GRID TO GAUSSIAN GRID.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL GL2GL(IP,KM,G1,IM1,JM1,G2,IM2,JM2)
C   INPUT ARGUMENT LIST:
C     IP           INTEGER INTERPOLATION TYPE
C     KM           INTEGER NUMBER OF LEVELS
C     G1           REAL (IM1,JM1,KM) INPUT GAUSSIAN FIELD
C     IM1          INTEGER NUMBER OF INPUT LONGITUDES
C     JM1          INTEGER NUMBER OF INPUT LATITUDES
C     IM2          INTEGER NUMBER OF OUTPUT LONGITUDES
C     JM2          INTEGER NUMBER OF OUTPUT LATITUDES
C   OUTPUT ARGUMENT LIST:
C     G2           REAL (IM2,JM2,KM) OUTPUT GAUSSIAN FIELD
C

C SUBPROGRAMS CALLED:
C   IPOLATES     IREDELL'S POLATE FOR SCALAR FIELDS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      implicit none
      INTEGER, INTENT(IN) :: IM1,JM1,IM2,JM2,KM,IP,JMO
      REAL G1(IM1,JM1,KM),G2(IM2,JM2,KM)
      LOGICAL*1 L1(IM1,JM1,KM),L2(IM2,JM2,KM)
      REAL    RLAT(IM2,JM2),RLON(IM2,JM2)
      INTEGER IB1(KM),IB2(KM),NO,IRET
      INTEGER KGDS1(200),KGDS2(200)
      INTEGER IDRTI, IDRTO
!     DATA KGDS1/4,0,0,90000,0,0,-90000,193*0/
!     DATA KGDS2/4,0,0,90000,0,0,-90000,193*0/
      INTEGER IPOPT(20)
      real rlat1,rlat2
!     DATA IPOPT/20*0/
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      KGDS1=0
      KGDS1(1) = 4 ; KGDS1(4) = 90000 ; KGDS1(7) = -90000
      KGDS2    = 0
      KGDS2(1) = 4
      IPOPT    = 0
      if(rlat1 == 90000. .and. rlat2 ==-90000.) then
       KGDS2(4) = 90000
       KGDS2(7) = -90000
      else
       kgds2(4) = nint(1000*rlat1)
       kgds2(7) = nint(1000*rlat2)
      endif
      KGDS1(1) = IDRTI
      KGDS2(1) = IDRTO
      IF(IM1 /= IM2 .OR. JM1 /= JM2) THEN
        IB1       = 0
        KGDS1(2)  = IM1
        KGDS1(3)  = JM1
        KGDS1(8)  = NINT(-360000./IM1)
        KGDS1(10) = JM1/2
        KGDS2(2)  = IM2
        KGDS2(3)  = JM2
        KGDS2(8)  = NINT(-360000./IM2)
!jaa    KGDS2(10) = JM2/2
        KGDS2(10) = JMO/2
        CALL IPOLATES(IP,IPOPT,KGDS1,KGDS2,IM1*JM1,IM2*JM2,KM,IB1,L1,G1,
     &                NO,RLAT,RLON,IB2,L2,G2,IRET)
      ELSE
        G2 = G1
      ENDIF
      END
C-----------------------------------------------------------------------
      SUBROUTINE SPECO3(NO3C,NO3T,JCAP,NC,LEVS,LONB,LATB,JC,IDAT,SL,SI,
     &                  SPS,SO3)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPECO3      GENERATE SPECTRAL OZONE FROM CLIMATOLOGY
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 98-05-01
C
C ABSTRACT: GENERATE SPECIFIC OZONE ON SIGMA LEVELS IN SPECTRAL SPACE
C   INTERPOLATED FROM CLIMATOLOGY AND POSSIBLY MODIFIED BY A SEPARATE
C   TOTAL OZONE FIELD.  THE CLIMATOLOGY READ COMES FROM GSFC.
C   IF AN TOTAL OZONE FIELD IS PROVIDED, THE CLIMATOLOGY PROFILES ARE
C   MULTIPLIED BY A SUITABLE FACTOR TO FIT THE TOTAL OZONE FIELD.
C
C PROGRAM HISTORY LOG:
C   98-05-01  MARK IREDELL
C
C USAGE:    CALL SPECO3(NO3C,NO3T,JCAP,NC,LEVS,LONB,LATB,JC,IDAT,SL,SI,
C    &                  SPS,SO3)
C   INPUT ARGUMENT LIST:
C     NO3C         INTEGER OZONE CLIMATOLOGY UNIT
C     NO3T         INTEGER GRIB TOTAL OZONE UNIT
C     JCAP         INTEGER SPECTRAL TRUNCATION
C     NC           INTEGER SPECTRAL FIRST DIMENSION
C     LEVS         INTEGER NUMBER OF SIGMA LEVELS
C     LONB         INTEGER NUMBER OF LONGITUDES IN GAUSSIAN GRID
C     LATB         INTEGER NUMBER OF LATITUDES IN GAUSSIAN GRID
C     JC           INTEGER NUMBER OF CPUS
C     IDAT         INTEGER (8) NCEP ABSOLUTE DATE AND TIME
C     SL           REAL (LEVS) FULL SIGMA VALUES
C     SI           REAL (LEVS+1) INTERFACE SIGMA VALUES
C     SPS          REAL (NC) SPECTRAL LOG PRESSURE
C   OUTPUT ARGUMENT LIST:
C     SO3          REAL (NC,LEVS) SPECTRAL OZONE (KG/KG)
C
C SUBPROGRAMS CALLED:
C   SPTRAN       PERFORM A SCALAR SPHERICAL TRANSFORM
C   GETO3C       GET AND INTERPOLATE OZONE CLIMATOLOGY
C   GETGB        FINDS AND UNPACKS A GRIB MESSAGE
C   IPOLATES     IREDELL'S POLATE FOR SCALAR FIELDS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      implicit none
      INTEGER NO3C,NO3T,JCAP,NC,LEVS,LONB,LATB,JC
      INTEGER IDAT(8)
      REAL SL(LEVS),SI(LEVS+1),SPS(NC),SO3(NC,LEVS)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200),KGDSGG(200)
      REAL PS(LONB*LATB)
      REAL PG(LONB*LATB+1,LEVS),O3G(LONB*LATB+1,LEVS)
      INTEGER,PARAMETER :: MO3T=720*361
      LOGICAL*1 LB(MO3T)
      REAL FB(MO3T),O3T(LONB*LATB)
      REAL WORK1(LONB*LATB),WORK2(LONB*LATB)
      LOGICAL*1 L2(LONB*LATB)
      INTEGER K,KF,NG,NX,IN,IS,IO3T,IRET,I,NO,IB2(1)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DEFINE GAUSSIAN TRANSFORM GRID
      KGDSGG=0
      KGDSGG(1)=4
      KGDSGG(2)=LONB
      KGDSGG(3)=LATB
      KGDSGG(4)=90000
      KGDSGG(7)=-90000
      KGDSGG(8)=NINT(-360000./LONB)
      KGDSGG(10)=LATB/2
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE SIGMA LEVEL PRESSURES
      NG=LONB*LATB
      NX=LONB*LATB+1
      IN=1
      IS=1+LONB*(LATB-1)
      CALL SPTRAN(0,JCAP,4,LONB,LATB,1,0,0,0,0,0,0,0,0,JC,SPS,
     &            PS(IN),PS(IS),1)
      PS=10*EXP(PS)
      DO K=1,LEVS
        PG(:NG,K)=PS(:NG)*SL(K)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET AND INTERPOLATE OZONE CLIMATOLOGY
      CALL GETO3C(NO3C,IDAT,KGDSGG,NX,LEVS,PG,O3G)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET GRIB TOTAL OZONE
      JPDS=-1
      JPDS(5)=10
      IF(NO3T.NE.0) THEN
        CALL GETGB(NO3T,0,MO3T,0,JPDS,JGDS,KF,K,KPDS,KGDS,LB,FB,IO3T)
      ELSE
        IO3T=9
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  MODIFY OZONE CLIMATOLOGY TO FIT TOTAL OZONE IF IT WAS FOUND
      IF(IO3T.EQ.0) THEN
        CALL IPOLATES(0,0,KGDS,KGDSGG,MO3T,NG,1,0,LB,FB,
     &                NO,WORK1,WORK2,IB2,L2,O3T,IRET)
        DO I=1,NG
          WORK1(I)=0
        ENDDO
        DO K=1,LEVS
          DO I=1,NG
            WORK1(I)=WORK1(I)+(SI(K)-SI(K+1))*O3G(I,K)
          ENDDO
        ENDDO
        DO I=1,NG
          WORK2(I)=O3T(I)/(WORK1(I)*PS(I)*(100/9.80665/2.14E-5))
        ENDDO
        DO K=1,LEVS
          DO I=1,NG
            O3G(I,K)=O3G(I,K)*WORK2(I)
          ENDDO
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM TO SPECTRAL SPACE
      CALL SPTRAN(0,JCAP,4,LONB,LATB,LEVS,0,0,0,0,NC,NX,0,0,JC,SO3,
     &            O3G(IN,1),O3G(IS,1),-1)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE GETO3C(LU,IDAT,KGDS,MG,KG,PG,O3G)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GETO3C      GET AND INTERPOLATE OZONE CLIMATOLOGY
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 98-05-01
C
C ABSTRACT: READ THE GSFC OZONE CLIMATOLOGY AND INTERPOLATE IT
C   IN TIME AND SPACE TO A SPECIFIC TIME OF YEAR AND
C   TO A SPECIFIC THREE-DIMENSIONAL GRID, RESPECTIVELY.
C
C PROGRAM HISTORY LOG:
C   98-05-01  MARK IREDELL
C
C USAGE:    CALL GETO3C(LU,IDAT,KGDS,MG,KG,PG,O3G)
C   INPUT ARGUMENT LIST:
C     LU           INTEGER OZONE CLIMATOLOGY UNIT
C     IDAT         INTEGER (8) NCEP ABSOLUTE DATE AND TIME
C     KGDS         INTEGER (200) GRID DESCRIPTION PARAMETERS
C     MG           INTEGER HORIZONTAL DIMENSION OF GRID FIELDS
C     KG           INTEGER NUMBER OF VERTICAL LEVELS
C     PG           REAL (MG,KG) PRESSURES TO WHICH TO INTERPOLATE (MB)
C   OUTPUT ARGUMENT LIST:
C     O3G          REAL (MG,KG) CLIMATOLOGICAL OZONE (KG/KG)
C
C SUBPROGRAMS CALLED:
C   W3DIFDAT     PERFORM A SIMPLE SCALAR SPHERICAL TRANSFORM
C   GETO3C       GET AND INTERPOLATE OZONE CLIMATOLOGY
C   GETGB        FINDS AND UNPACKS A GRIB MESSAGE
C   IPOLATES     IREDELL'S POLATE FOR SCALAR FIELDS
C   TERP3        CUBICALLY INTERPOLATE IN ONE DIMENSION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      IMPLICIT NONE
!
      integer, PARAMETER :: JMC=18,KMC=17
      INTEGER LU,MG,KG
      INTEGER IDAT(8)
      INTEGER KGDS(200)
      REAL PG(MG,KG),O3G(MG,KG)
      REAL PRES(KMC),O3Y(JMC,KMC,12),O3M(JMC,KMC),O3W(MG,KMC)
      REAL Z1(MG,KMC),Z2(MG,KG)
      INTEGER KGDS2(200)
      INTEGER LATS(JMC)
      INTEGER JDAT(8)
      REAL D1,D2
      INTEGER N,M2,IRET,NO,M1,KM,K,J,M,MON
      REAL RINC(5)
      REAL O3M2(2,JMC,KMC)
      REAL, allocatable :: J2(:)
      REAL RLAT(MG),RLON(MG)
      INTEGER IBI(KMC),IBO(KMC)
      LOGICAL*1 LI,LO(MG,KMC)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ THE CLIMATOLOGY
      KM=KMC
      IF(LU.LE.0) RETURN
      REWIND LU
      READ(LU,*) PRES
      DO M=1,12
        READ(LU,*) (MON,LATS(J),(O3Y(J,K,MON),K=1,KMC),J=1,JMC)
      ENDDO
C  CONVERT FROM PARTS PER MILLION TO SPECIFIC OZONE
      O3Y=O3Y*1.655E-6
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INTERPOLATE CLIMATOLOGY IN TIME
      M1=IDAT(2)
C  DETERMINE TIME INTERVAL TO THE 16TH OF THE MONTH
      JDAT=0
      JDAT(1)=IDAT(1)
      JDAT(2)=IDAT(2)
      JDAT(3)=16
      CALL W3DIFDAT(JDAT,IDAT,1,RINC)
C  DATE IN FIRST HALF OF THE MONTH; INTERPOLATE WITH THE PREVIOUS MONTH
      IF(RINC(1).GE.0.) THEN
        D1=RINC(1)
        IF(M1.GT.1) THEN
          M2=M1-1
          JDAT(1)=IDAT(1)
        ELSE
          M2=12
          JDAT(1)=IDAT(1)-1
        ENDIF
        JDAT(2)=M2
        CALL W3DIFDAT(JDAT,IDAT,1,RINC)
        D2=-RINC(1)
C  DATE IN SECOND HALF OF THE MONTH; INTERPOLATE WITH THE NEXT MONTH
      ELSE
        D1=-RINC(1)
        IF(M1.LT.12) THEN
          M2=M1+1
          JDAT(1)=IDAT(1)
        ELSE
          M2=1
          JDAT(1)=IDAT(1)+1
        ENDIF
        JDAT(2)=M2
        CALL W3DIFDAT(JDAT,IDAT,1,RINC)
        D2=RINC(1)
      ENDIF
C  INTERPOLATE
      O3M=(D2*O3Y(:,:,M1)+D1*O3Y(:,:,M2))/(D1+D2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INTERPOLATE CLIMATOLOGY TO A GRID
      KGDS2=0
C  DEFINE THE GRID OF THE INPUT CLIMATOLOGY
      KGDS2(1)=0
      KGDS2(2)=2
      KGDS2(3)=JMC
      KGDS2(4)=LATS(1)*1000
      KGDS2(5)=0
      KGDS2(7)=LATS(JMC)*1000
      KGDS2(8)=180000
      KGDS2(11)=64
      KGDS2(20)=255
      O3M2(1,:,:)=O3M
      O3M2(2,:,:)=O3M
      IBI=0
C  INTERPOLATE HORIZONTALLY TO REQUESTED GRID
      CALL IPOLATES(0,0,KGDS2,KGDS,2*JMC,MG,KMC,IBI,LI,O3M2,
     &              NO,RLAT,RLON,IBO,LO,O3W,IRET)
C  INTERPOLATE VERTICALLY TO REQUESTED PRESSURES
!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(K,N)
      DO K=1,KMC
        DO N=1,NO
          Z1(N,K)=-LOG(PRES(K))
        ENDDO
      ENDDO
!$OMP END PARALLEL DO

!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(K,N)

      DO K=1,KG
        DO N=1,NO
          Z2(N,K)=-LOG(PG(N,K))
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
      if (.not. allocated(j2)) allocate (j2(NO+KG*MG))
      CALL TERP3(NO,1,1,1,1,1,MG*KMC,MG*KG,
     &           KMC,MG,MG,Z1,O3W,KG,MG,MG,Z2,O3G,J2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------
!$$$  Subprogram documentation block
!
! Subprogram: specsets       Create special tracer sets
!   Prgmmr: Iredell          Org: W/NP23      Date: 2004-09-24
!
! Abstract: This subprogram optionally augments the tracers
!   in the global model initial conditions if special tracer set
!   are requested, based on the value of the tracer variable ID.
!
! Program history log:
!   2004-09-24   Iredell
!
! Modules used:
!   sigio_module   global model sigma file types and I/O
!
! Usage:    call specsets(h,d,idrt)
!   Input arguments:
!     h            type(sigio_head) sigma file header
!       ak
!       bk
!       idsl 
!       idvc 
!       idvt
!       jcap 
!       latb 
!       levs 
!       lonb 
!       ntrac
!       si
!     d            type(sigio_data) sigma file data
!       hs
!       ps
!       t
!       d
!       z
!       q
!
!   Output arguments:
!     d            type(sigio_data) sigma file data
!       q
!
! Attributes:
!   Language: Fortran90
!
! Remarks:
!   Pertinent values of h%idvt and h%ntrac
!   idvt  ntrac 
!    100     20    set 1: vapor,ozone,cloud, and initial values of
!                  clat*clon,clat*slon,slat,
!                  v*slon-u*slat*clon,-v*clon-u*slat*slon,u*clat
!                  one,k,sigma,ps,pres,temp,entropy,moist entropy
!                  vapor,ozone,cloud
!
!C$$$
      subroutine specsets(h,d,idrt)
        use sigio_module
        implicit none
        type(sigio_head),intent(in):: h
!cggg        type(sigio_data),intent(inout):: d
        type(sigio_dbta),intent(inout):: d
        integer, intent(in)   :: idrt
        real,dimension(h%latb):: slat,wlat
        real,dimension(h%lonb,h%latb):: hs,ps
        real,dimension(h%lonb,h%latb,h%levs):: pm,pd
        real,dimension(h%lonb,h%latb,h%levs):: t,u,v
        real,dimension(h%lonb,h%latb,h%levs,h%ntrac):: q
        real clat,rlon
!cggg
        real, allocatable :: vcoord(:,:)

        integer i,j,k,iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        call splat(idrt,h%latb,slat,wlat)
        call sptez(0,h%jcap,idrt,h%lonb,h%latb,d%hs,hs,+1)
        call sptez(0,h%jcap,idrt,h%lonb,h%latb,d%ps,ps,+1)
        call sptezm(0,h%jcap,idrt,h%lonb,h%latb,h%levs,d%t,t,+1)
        call sptezmv(0,h%jcap,idrt,h%lonb,h%latb,h%levs,d%d,d%z,u,v,+1)
        call sptezm(0,h%jcap,idrt,h%lonb,h%latb,h%levs*3,d%q,q,+1)
        ps=1.e3*exp(ps)
        t=t/(1.+(461.50/287.05-1)*q(:,:,:,1))
!cggg
        allocate(vcoord(h%levs+1,h%nvcoord))
        vcoord = h%vcoord
        call sigio_modprd(h%lonb*h%latb,h%lonb*h%latb,h%levs,h%nvcoord,
!cggg     &                    h%idvc,h%idsl,h%vcoord,iret,
     &                    h%idvc,h%idsl,vcoord,iret,
     &                    ps=ps,t=t,pm=pm,pd=pd)
       deallocate (vcoord)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        if(h%idvt==100.and.h%ntrac==20) then
!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(i,j,clat,rlon,k)
          do j=1,h%latb
            clat=sqrt(1-slat(j)**2)
            do i=1,h%lonb
              rlon=2*acos(-1.)*(i-1)/h%lonb
              q(i,j,:,4)=clat*cos(rlon)
              q(i,j,:,5)=clat*sin(rlon)
              q(i,j,:,6)=slat(j)
              q(i,j,:,7)=v(i,j,:)*sin(rlon)-u(i,j,:)*slat(j)*cos(rlon)
              q(i,j,:,8)=-v(i,j,:)*cos(rlon)-u(i,j,:)*slat(j)*sin(rlon)
              q(i,j,:,9)=u(i,j,:)*clat
              q(i,j,:,10)=1
              q(i,j,:,11)=(/(k,k=1,h%levs)/)
              q(i,j,:,12)=pm(i,j,:)/ps(i,j)
              q(i,j,:,13)=ps(i,j)
              q(i,j,:,14)=pm(i,j,:)
              q(i,j,:,15)=t(i,j,:)
              call dothe(1,1,h%levs,pm(i,j,:),t(i,j,:),q(i,j,:,1),      &
     &                   q(i,j,:,16),q(i,j,:,17))
              q(i,j,:,16)=1004.6*log(q(i,j,:,16)/273.15)
              q(i,j,:,17)=1004.6*log(q(i,j,:,17)/273.15)
              q(i,j,:,18)=q(i,j,:,1)
              q(i,j,:,19)=q(i,j,:,2)
              q(i,j,:,20)=q(i,j,:,3)
            enddo
          enddo
!$OMP END PARALLEL DO
          call sptezm(0,h%jcap,idrt,h%lonb,h%latb,h%levs*(h%ntrac-3),      &
     &                d%q(1,1,4),q(1,1,1,4),-1)
        endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end subroutine
!-----------------------------------------------------------------------
      subroutine dothe(im,ix,km,p,t,q,th,the)
      use physcons
      use funcphys
      implicit none
      integer,intent(in):: im,ix,km
      real,intent(in):: p(ix,km),t(ix,km),q(ix,km)
      real,intent(out):: th(ix,km),the(ix,km)
      integer i,k
      real(krealfp) pr,tr,qr
      real(krealfp) qminr,elr,pvr,tdpdr,tlclr,pklclr
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  potential temperature
      do k=1,km
        do i=1,im
          pr=p(i,k)
          tr=t(i,k)
          th(i,k)=tr/fpkapx(pr)
        enddo
      enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  equivalent potential temperature
      qminr=1.e-6
      do k=1,km
        do i=1,im
          pr=p(i,k)
          tr=t(i,k)
          qr=q(i,k)
          if(qr.lt.qminr) then
            elr=con_hvap+con_dldt*(tr-con_ttp)
            elr=elr*exp(-con_dldt/con_cp*(qminr-qr))
            tr=(elr-con_hvap)/con_dldt+con_ttp
            qr=qminr
          endif
          pvr=pr*qr/(con_eps-con_epsm1*qr)
          tdpdr=tr-ftdpx(pvr)
          tlclr=ftlclx(tr,tdpdr)
          pklclr=fpkapx(pr)*tlclr/tr
          the(i,k)=fthex(tlclr,pklclr)
        enddo
      enddo
      end subroutine
!-----------------------------------------------------------------------
!$$$  Subprogram documentation block
!
! Subprogram: latlons       calculate lat/lons at each grid point
!   Prgmmr: Gayno          Org: W/NP23      Date: 2006-04-13
!
! Abstract:
!
! Program history log:
!   2006-04-13   Iredell
!
! Modules used:
!
! Usage:    call latlons()
!   Input arguments:
!   Output arguments:
!
! Attributes:
!   Language: Fortran90
!
! Remarks:
!
!C$$$
      subroutine latlons(latb, ijmdl, lonsperlat, lats, lons, idrt)
      implicit none

      integer, intent(in) :: latb, ijmdl, lonsperlat((latb+1)/2), idrt
      integer             :: n, ii, jj, j, i
      real, allocatable   :: a(:), w(:), gaul(:)
      real                :: radi, dx
      real, intent(out)   :: lats(ijmdl),lons(ijmdl)

      allocate(gaul(latb))
      allocate(a(latb))
      allocate(w(latb))
      call splat(idrt, latb, a, w)
      radi = 180.0 / (4.*atan(1.))
      do n=1,latb
        gaul(n) = acos(a(n)) * radi
      enddo
      deallocate (a,w)
      ii = 0
      do j=1,latb
      jj = j
      if (j .gt. latb/2) jj = latb - j + 1
      dx = 360. / float(lonsperlat(jj))
      do i=1,lonsperlat(jj)
        ii = ii + 1
        lats(ii) = 90. - gaul(j)
        lons(ii) = float(i-1)*dx
        if(lons(ii).gt.180.) lons(ii) = lons(ii) - 360.
      enddo
      enddo
      deallocate (gaul)
      return
      end subroutine latlons

      subroutine hhmaxmin(a,im,jm,j1,j2,km,ch)
      implicit none
      integer im, jm, j1,j2, km
      character*(*) ch
      real  a(im,jm,km)
      integer i, j, k
      real fmin, fmax
      do k=1,km
        fmin=1.0e10
        fmax=-1.0e10
        do j=j1,j2
          do i=1,im
            fmax=max(fmax,a(i,j,k))
            fmin=min(fmin,a(i,j,k))
          enddo
        enddo
!       print *,' max=',fmax,' min=',fmin,' at k=',k,' for ',ch
      enddo
      return
      end
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE NEWPR1(IM,IX,KM,KMP,IDVC,IDVM,IDSL,NVCOORD,VCOORD,
     &                  RI, CPI, NTRACM,
     &                  PP,TP,QP,PS,PM,DP)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    NEWPR1      COMPUTE MODEL PRESSURES
C   PRGMMR: JUANG          ORG: W/NMC23     DATE: 2005-04-11
C   PRGMMR: Fanglin Yang   ORG: W/NMC23     DATE: 2006-11-28
!   PRGMMR: S. Moorthi     ORG: NCEP/EMC    DATE: 2006-12-12
!   PRGMMR: S. Moorthi     ORG: NCEP/EMC    DATE: 2007-01-02
C
C ABSTRACT: COMPUTE MODEL PRESSURES.
C
C PROGRAM HISTORY LOG:
C 2005-04-11  HANN_MING HENRY JUANG    hybrid sigma, sigma-p, and sigma-theta
C
C USAGE:    CALL NEWPR1(IM,IX,KM,KMP,IDVC,IDSL,NVCOORD,VCOORD,PP,TP,QP,PS,PI,PM,DP)
C   INPUT ARGUMENT LIST:
C     IM           INTEGER NUMBER OF POINTS TO COMPUTE
C     IX           INTEGER FIRST DIMENSION
C     KM           INTEGER NUMBER OF LEVELS
C     KMP          INTEGER NUMBER OF OLD LEVELS
C     IDVC         INTEGER VERTICAL COORDINATE ID
C                  (1 FOR SIGMA AND 2 FOR HYBRID)
C     IDSL         INTEGER TYPE OF SIGMA STRUCTURE
C                  (1 FOR PHILLIPS OR 2 FOR MEAN)
C     NVCOORD      INTEGER NUMBER OF VERTICAL COORDINATES
C     VCOORD       REAL (KM+1,NVCOORD) VERTICAL COORDINATE VALUES
C                  FOR IDVC=1, NVCOORD=1: SIGMA INTERFACE
C                  FOR IDVC=2, NVCOORD=2: HYBRID INTERFACE A AND B
C                  FOR IDVC=3, NVCOORD=3: JUANG GENERAL HYBRID INTERFACE, WHICH INCLUDES
C                     AK  REAL (KM+1) HYBRID INTERFACE A 
C                     BK  REAL (KM+1) HYBRID INTERFACE B
C                     CK  REAL (KM+1) HYBRID INTERFACE C
C     PP           REAL (IX,KM) OLD PRESSURE
C     TP           REAL (IX,KM) OLD TEMPERATURE
C     QP           REAL (IX,KM) OLD SPECIFIC HUMIDITY
C     PS           REAL (IX) SURFACE PRESSURE (PA)
C   OUTPUT ARGUMENT LIST:
C     PM           REAL (IX,KM) MID-LAYER PRESSURE (PA)
C     DP           REAL (IX,KM) LAYER DELTA PRESSURE (PA)        
C   TEMPORARY
C     PI           REAL (IX,KM+1) INTERFACE PRESSURE (PA)
C     SI           REAL (KM+1) SIGMA INTERFACE VALUES (IDVC=1)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      implicit none
      real, PARAMETER :: RD=287.05,  RV=461.50,    CP=1004.6
     &,                  ROCP=RD/CP, ROCP1=ROCP+1, ROCPR=1/ROCP
     &,                  FV=RV/RD-1.
      integer im, ix, km, kmp, idvc, idvm, idsl, nvcoord, ntracm
      real ri(0:ntracm), cpi(0:ntracm)
      REAL SI(KM+1),AK(KM+1),BK(KM+1),CK(KM+1),CK2(KM)
      REAL VCOORD(KM+1,NVCOORD)
      REAL PS(IX),PI(IX,KM+1),PM(IX,KM)
      REAL DP(IX,KM)
      REAL PP(IX,KMP),TP(IX,KMP),QP(IX,KMP,ntracm),THETAP(KMP)
      REAL PO(KMP),TO(KMP),QO(KMP,ntracm)
      REAL PN(KM ),TN(KM ),QN(KM,ntracm ), AKBKPS(KM)
      REAL TOV(KM),TRK,PIO(KM+1),PISAVE(IX,KM+1)
!
      real xcp, sumq, xcp2, sumq2, temu, temd, converg, dpmin
     &,    dpminall, tvu, tvd, tem, tem1, cp0i, qnk
      integer sfcpress_id, thermodyn_id, i, k, n, nit
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      sfcpress_id  = mod(IDVM,1)
      thermodyn_id = mod(IDVM/10,10)
      IF(IDVC.EQ.3) THEN  ! hmhj for s-t
        DO K=1,KM
          AK(K)  = VCOORD(K,1)
          BK(K)  = VCOORD(K,2)
          CK(K)  = VCOORD(K,3)
          TOV(K) = 300.0
        ENDDO
        PI(1:IM,1)    = PS(1:IM)
        PI(1:IM,KM+1) = 0.0
!
! first guess : assume KMP=KM
!
        if (thermodyn_id <= 1) then
!!$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(KM,kmp,IM)
!!$OMP+ SHARED(qn,qp,TOV,PI,AK,BK,PS,CK)
!$omp  parallel do shared(km,kmp,im,qp,tp,tov,pi,ak,bk,ck)
!$omp1 private(i,k,tem,qnk,trk)
          DO K=2,KM
            tem = float(k-1) / float(kmp-1)
            DO I=1,IM
              qnk     = qp(i,1,1)  + (qp(i,kmp,1)-qp(i,1,1))*TEM
              TRK     = (TP(I,K)*(1.0+FV*QNK)/TOV(K)) ** ROCPR
              PI(I,K) = AK(K) + BK(K)*PS(I) + CK(K)*TRK	! p at interfaces
            ENDDO
          ENDDO
!!$OMP END PARALLEL DO
        elseif (thermodyn_id == 3) then
          cp0i = 1.0 / cpi(0)
          DO K=2,KM
            tem = float(k-1) / float(kmp-1)
            DO I=1,IM
              xcp  = 0.0
              sumq = 0.0
              do n=1,NTRACM
                qn(k,n) = qp(i,1,n)  + (qp(i,kmp,n)-qp(i,1,n))*TEM
                if( cpi(n).ne.0.0 ) then
                  xcp  = xcp  + cpi(n)*qn(k,n)
                  sumq = sumq + qn(k,n)
                endif
              enddo
              xcp  = (1.-sumq) + xcp * cp0i
              tem1 = tp(i,1) + (tp(i,kmp)-tp(i,1))*tem
              trk  = (tem1*xcp/tov(k)) ** ROCPR
              PI(I,K) = AK(K) + BK(K)*PS(I) + CK(K)*TRK	! p at interfaces
            enddo
          enddo
        endif

        DPMINALL=1000.0
!$omp parallel do
!$omp1 shared(im,km,kmp,ntracm,thermodyn_id,pp,tp,qp,cpi,cp0i)
!$omp1 shared(ak,bk,ck,pi)
!$omp1 private(i,k,nit,converg,dpmin,tvu,tvd,trk)
!$omp1 private(pio,po,to,qo,pn,tn,qn,akbkps)
!$omp1 private(xcp,xcp2,sumq,sumq2,temu,temd)
!
        DO I=1,IM
          DO K=1,KMP
            PO(K)   = PP(I,K)
            TO(K)   = TP(I,K)
            QO(K,:) = QP(I,K,:)
          ENDDO
          do k=2,km
            akbkps(k) = ak(k) + bk(k)*ps(i)
          enddo
! iteration
         DO Nit=1,400	! default number of iterations
           CONVERG = 0.0
           DPMIN   = 1000.0
           DO K=1,KM+1
             PIO(K) = PI(I,K)
           ENDDO
           DO K=1,KM
             PN(K) = 0.5*(PIO(K)+PIO(K+1))
           ENDDO
! do interpolation by the intrinsic method to get TN and QN
           if (thermodyn_id <= 1) then
             CALL VINTTQ(KMP,KM,PO,TO,QO(1,1),PN,TN,QN(1,1))
             DO K=2,KM
               TVU = TN(K  )*(1.0+FV*QN(K,1))
               TVD = TN(K-1)*(1.0+FV*QN(K-1,1))
               TRK = ((TVD+TVU)/(TOV(K-1)+TOV(K))) ** ROCPR
               PI(I,K) = AKBKPS(K) + CK(K)*TRK
               CONVERG = MAX(CONVERG,ABS(PI(I,K)-PIO(K))
     &                                 /(PI(I,K)+PIO(K)))
               PI(I,K) = 0.5*(PI(I,K)+PIO(K))	! make it converged faster
               DPMIN   = MIN(DPMIN,PI(I,K-1)-PI(I,K))
             ENDDO
           elseif (thermodyn_id == 3) then
             CALL VINTTR(1,1,KMP,KM,NTRACM,PO,TO,QO,PN,TN,QN)
             DO K=2,KM
               xcp   = 0.0
               xcp2  = 0.0
               sumq  = 0.0
               sumq2 = 0.0
               do n=1,NTRACM
                 if( cpi(n).ne.0.0 ) then
                   xcp   = xcp   + cpi(n)*qn(k,n)
                   sumq  = sumq  + qn(k,n)
                   xcp2  = xcp2  + cpi(n)*qn(k-1,n)
                   sumq2 = sumq2 + qn(k-1,n)
                 endif
               enddo
               temu    = (1.-sumq)  + xcp*cp0i
               temd    = (1.-sumq2) + xcp2*cp0i
               trk     = ((tn(k)*temu + tn(k-1)*temd)
     &                 /  (TOV(K)     + TOV(K-1))) ** ROCPR
               PI(I,K) = AKBKPS(K) + CK(K)*TRK
               CONVERG = MAX(CONVERG,ABS(PI(I,K)-PIO(K))
     &                                 /(PI(I,K)+PIO(K)))
               PI(I,K) = 0.5*(PI(I,K)+PIO(K))      ! make it converged faster
               DPMIN   = MIN(DPMIN,PI(I,K-1)-PI(I,K))
             ENDDO
!     if (i .eq. 1) print *,' converg=',converg,' nit=',nit
           endif
           IF( CONVERG.LE.1.E-6 ) GOTO 100
         ENDDO
 100     CONTINUE
!        PRINT *,'I=',I,' CONVERGED AT',Nit,' ITERATIONS',' DPMIN='
!    &,          DPMIN
         DPMINALL = MIN(DPMINALL,DPMIN)
        ENDDO
!!$OMP END PARALLEL DO
!       PRINT *,' ---- THE MINIMUM DP FOR A GROUP IS ',DPMINALL
      ELSE IF(IDVC.EQ.2) THEN
        DO K=1,KM+1
          AK(K)      = VCOORD(K,1)
          BK(K)      = VCOORD(K,2)
          PI(1:IM,K) = AK(K) + BK(K)*PS(1:IM)
        ENDDO
      ELSE
        DO K=1,KM+1
          SI(K)      = VCOORD(K,1)
          PI(1:IM,K) = SI(K)*PS(1:IM)
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(IDSL.EQ.2) THEN
        DO K=1,KM
          PM(1:IM,K) = (PI(1:IM,K)+PI(1:IM,K+1))/2
        ENDDO
      ELSE
!$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(KM,PM,IM,PI)
        DO K=1,KM
          PM(1:IM,K) = ((PI(1:IM,K)**ROCP1-PI(1:IM,K+1)**ROCP1)/
     &                 (ROCP1*(PI(1:IM,K)-PI(1:IM,K+1))))**ROCPR
        ENDDO
!$OMP END PARALLEL DO
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      DO K=1,KM
        DO I=1,IM
          DP(I,K) = PI(I,K) - PI(I,K+1)
        ENDDO
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE CHECKDP(IM,IX,KM,AK,BK,CK,PS,TP,QP)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CHECKDP       COMPUTE MODEL PRESSURES
C   PRGMMR: JUANG          ORG: W/NMC23     DATE: 2005-04-11
C
C ABSTRACT: CHECK THICKNESS FOR SIGMA-THETA COORDINATE
C
C PROGRAM HISTORY LOG:
C 2005-04-11  HANN_MING HENRY JUANG    hybrid sigma, sigma-p, and sigma-theta
C
C   INPUT ARGUMENT LIST:
C     IM           INTEGER NUMBER OF POINTS TO COMPUTE
C     IX           INTEGER FIRST DIMENSION
C     KM           INTEGER NUMBER OF LEVELS
C     AK           REAL (KM+1) HYBRID INTERFACE A
C     BK           REAL (KM+1) HYBRID INTERFACE B
C     CK           REAL (KM+1) HYBRID INTERFACE C
C     TP           REAL (IX,KM) OLD TEMPERATURE
C     QP           REAL (IX,KM) OLD SPECIFIC HUMIDITY
C     PS           REAL (IX) SURFACE PRESSURE (PA)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      IMPLICIT NONE
      INTEGER IM,IX,KM
      REAL,PARAMETER  :: RD=287.05,RV=461.50,CP=1004.6
      REAL,PARAMETER  :: ROCP=RD/CP,ROCP1=ROCP+1,ROCPR=1./ROCP,
     &                   FV=RV/RD-1.
      REAL AK(KM+1),BK(KM+1),CK(KM+1),PS(IX)
      REAL TP(IX,KM),QP(IX,KM),PI(IM,KM+1)
      REAL TOV(KM),TRK,TVU,TVD
      INTEGER K,I,KMIN
      REAL dpmin,FTV,dltdz,dlpvdrt,AT,AQ
!
      FTV(AT,AQ)=AT*(1+FV*AQ)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        DO K=1,KM
          TOV(K) = 300.0
        ENDDO
        PI(1:IM,1)=PS(1:IM)
        PI(1:IM,KM+1)=0.0
!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(K,I,TVU,TVD,TRK)
        DO K=2,KM
          DO I=1,IM
            TVU=FTV(TP(I,K  ),QP(I,K  ))
            TVD=FTV(TP(I,K-1),QP(I,K-1))
            TRK = (TVD+TVU)/(TOV(K-1)+TOV(K))
            TRK = TRK ** ROCPR
            PI(I,K)=AK(K)+BK(K)*PS(I)+CK(K)*TRK
          ENDDO
        ENDDO
!$OMP END PARALLEL DO

        DO I=1,IM
          DPMIN=1000.
          DO K=1,KM
            IF( PI(I,K)-PI(I,K+1) .LT. DPMIN ) THEN
              KMIN=K
              DPMIN=PI(I,K)-PI(I,K+1)
            ENDIF
          ENDDO
          IF( DPMIN.LT.0.0 )PRINT *,' I KMIN DPMIN ',I,KMIN,DPMIN
        ENDDO
      RETURN
      END

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      SUBROUTINE VINTTQ(KM1,KM2,P1,T1,Q1,P2,T2,Q2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    VINTTQ   VERTICALLY INTERPOLATE UPPER-AIR T AND Q
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: VERTICALLY INTERPOLATE UPPER-AIR FIELDS.
C   WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS ARE INTERPOLATED.
C   THE INTERPOLATION IS CUBIC LAGRANGIAN IN LOG PRESSURE
C   WITH A MONOTONIC CONSTRAINT IN THE CENTER OF THE DOMAIN.
C   IN THE OUTER INTERVALS IT IS LINEAR IN LOG PRESSURE.
C   OUTSIDE THE DOMAIN, FIELDS ARE GENERALLY HELD CONSTANT,
C   EXCEPT FOR TEMPERATURE AND HUMIDITY BELOW THE INPUT DOMAIN,
C   WHERE THE TEMPERATURE LAPSE RATE IS HELD FIXED AT -6.5 K/KM AND
C   THE RELATIVE HUMIDITY IS HELD CONSTANT.
C
C PROGRAM HISTORY LOG:
C   1991-10-31  MARK IREDELL
C   2005-08-31  Henry JUANG MODIFIED IT TO DO T AND Q FROM VINTG
C
C USAGE:    CALL VINTTQ(IM,IX,KM1,KM2,P1,T1,Q1,P2,T2,Q2)
C   INPUT ARGUMENT LIST:
C     KM1          INTEGER NUMBER OF INPUT LEVELS
C     KM2          INTEGER NUMBER OF OUTPUT LEVELS
C     P1           REAL (IX,KM1) INPUT PRESSURES
C                  ORDERED FROM BOTTOM TO TOP OF ATMOSPHERE
C     T1           REAL (IX,KM1) INPUT TEMPERATURE (K)
C     Q1           REAL (IX,KM1,NT) INPUT TRACERS (HUMIDITY FIRST)
C     P2           REAL (IX,KM2) OUTPUT PRESSURES
C   OUTPUT ARGUMENT LIST:
C     T2           REAL (IX,KM2) OUTPUT TEMPERATURE (K)
C     Q2           REAL (IX,KM2,NT) OUTPUT TRACERS (HUMIDITY FIRST)
C
C SUBPROGRAMS CALLED:
C   TERP3_HJ     CUBICALLY INTERPOLATE IN ONE DIMENSION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      IMPLICIT NONE
      INTEGER KM1,KM2
      REAL P1(KM1),T1(KM1),Q1(KM1)
      REAL P2(KM2),T2(KM2),Q2(KM2)
      REAL,PARAMETER :: DLTDZ=-6.5E-3*287.05/9.80665
      REAL,PARAMETER :: DLPVDRT=-2.5E6/461.50
      REAL Z1(2,KM1),Z2(2,KM2)
      REAL C1(2,KM1,2),C2(2,KM2,2)
      INTEGER K
      REAL DZ,Z2S,Z1A,Z1B,Q1C,Q1D,Q2S,Z1C,Z1D
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE LOG PRESSURE INTERPOLATING COORDINATE
C  AND COPY INPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS
!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(K)
      DO K=1,KM1
        Z1(1,K)=-LOG(P1(K))
        C1(1,K,1)=T1(K)
        C1(1,K,2)=Q1(K)
      ENDDO
!$OMP END PARALLEL DO
!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(K)
      DO K=1,KM2
        Z2(1,K)=-LOG(P2(K))
      ENDDO
!$OMP END PARALLEL DO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PERFORM LAGRANGIAN ONE-DIMENSIONAL INTERPOLATION
C  THAT IS 4TH-ORDER IN INTERIOR, 2ND-ORDER IN OUTSIDE INTERVALS
C  AND 1ST-ORDER FOR EXTRAPOLATION.
      CALL TERP3_HJ(1,1,1,1,1,2,2*KM1,2*KM2,
     &           KM1,2,2,Z1,C1,KM2,2,2,Z2,C2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COPY OUTPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS
C  EXCEPT BELOW THE INPUT DOMAIN, LET TEMPERATURE INCREASE WITH A FIXED
C  LAPSE RATE AND LET THE RELATIVE HUMIDITY REMAIN CONSTANT.
      DO K=1,KM2
        DZ=Z2(1,K)-Z1(1,1)
        IF(DZ.GE.0) THEN
          T2(K)=C2(1,K,1)
          Q2(K)=C2(1,K,2)
        ELSE
          T2(K)=T1(1)*EXP(DLTDZ*DZ)
          Q2(K)=Q1(1)*EXP(DLPVDRT*(1/T2(K)-1/T1(1))-DZ)
        ENDIF
      ENDDO
      END
C-----------------------------------------------------------------------
      SUBROUTINE TERP3_HJ(IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2,
     &                 KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2,KXQ2,Z2,Q2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    TERP3_HJ    CUBICALLY INTERPOLATE IN ONE DIMENSION
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 98-05-01
C
C ABSTRACT: INTERPOLATE FIELD(S) IN ONE DIMENSION ALONG THE COLUMN(S).
C   THE INTERPOLATION IS CUBIC LAGRANGIAN WITH A MONOTONIC CONSTRAINT
C   IN THE CENTER OF THE DOMAIN.  IN THE OUTER INTERVALS IT IS LINEAR.
C   OUTSIDE THE DOMAIN, FIELDS ARE HELD CONSTANT.
C
C PROGRAM HISTORY LOG:
C   98-05-01  MARK IREDELL
C 1999-01-04  IREDELL  USE ESSL SEARCH
C 2006-11-10  SIMPLIFIED VERSION OF TERP3
C
C USAGE:    CALL TERP3_HJ(IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2,
C    &                 KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2,KXQ2,Z2,Q2)
C   INPUT ARGUMENT LIST:
C     IM           INTEGER NUMBER OF COLUMNS
C     IXZ1         INTEGER COLUMN SKIP NUMBER FOR Z1
C     IXQ1         INTEGER COLUMN SKIP NUMBER FOR Q1
C     IXZ2         INTEGER COLUMN SKIP NUMBER FOR Z2
C     IXQ2         INTEGER COLUMN SKIP NUMBER FOR Q2
C     NM           INTEGER NUMBER OF FIELDS PER COLUMN
C     NXQ1         INTEGER FIELD SKIP NUMBER FOR Q1
C     NXQ2         INTEGER FIELD SKIP NUMBER FOR Q2
C     KM1          INTEGER NUMBER OF INPUT POINTS
C     KXZ1         INTEGER POINT SKIP NUMBER FOR Z1
C     KXQ1         INTEGER POINT SKIP NUMBER FOR Q1
C     Z1           REAL (1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
C                  INPUT COORDINATE VALUES IN WHICH TO INTERPOLATE
C                  (Z1 MUST BE STRICTLY MONOTONIC IN EITHER DIRECTION)
C     Q1           REAL (1+(IM-1)*IXQ1+(KM1-1)*KXQ1+(NM-1)*NXQ1)
C                  INPUT FIELDS TO INTERPOLATE
C     KM2          INTEGER NUMBER OF OUTPUT POINTS
C     KXZ2         INTEGER POINT SKIP NUMBER FOR Z2
C     KXQ2         INTEGER POINT SKIP NUMBER FOR Q2
C     Z2           REAL (1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
C                  OUTPUT COORDINATE VALUES TO WHICH TO INTERPOLATE
C                  (Z2 NEED NOT BE MONOTONIC)
C     
C   OUTPUT ARGUMENT LIST:
C     Q2           REAL (1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)
C                  OUTPUT INTERPOLATED FIELDS
C
C SUBPROGRAMS CALLED:
C   RSEARCH      SEARCH FOR A SURROUNDING REAL INTERVAL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      IMPLICIT NONE
      INTEGER IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2,KM1,KXZ1,KXQ1,
     &        KM2,KXZ2,KXQ2
      REAL Z1(1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
      REAL Q1(1+(IM-1)*IXQ1+(KM1-1)*KXQ1+(NM-1)*NXQ1)
      REAL Z2(1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
      REAL Q2(1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)
      REAL FFA(IM),FFB(IM),FFC(IM),FFD(IM)
      INTEGER K1S(IM,KM2)
      REAL Q2S,Q1A,Q1B,Q1C,Q1D,Z2S,Z1A,Z1B,Z1C,Z1D
      INTEGER I,K1,K2,N
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FIND THE SURROUNDING INPUT INTERVAL FOR EACH OUTPUT POINT.
      CALL RSEARCH(IM,KM1,IXZ1,KXZ1,Z1,KM2,IXZ2,KXZ2,Z2,1,IM,K1S)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GENERALLY INTERPOLATE CUBICALLY WITH MONOTONIC CONSTRAINT
C  FROM TWO NEAREST INPUT POINTS ON EITHER SIDE OF THE OUTPUT POINT,
C  BUT WITHIN THE TWO EDGE INTERVALS INTERPOLATE LINEARLY.
C  KEEP THE OUTPUT FIELDS CONSTANT OUTSIDE THE INPUT DOMAIN.
      DO K2=1,KM2
        DO I=1,IM
          K1=K1S(I,K2)
          IF(K1.EQ.1.OR.K1.EQ.KM1-1) THEN
            Z2S=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2)
            Z1A=Z1(1+(I-1)*IXZ1+(K1-1)*KXZ1)
            Z1B=Z1(1+(I-1)*IXZ1+(K1+0)*KXZ1)
            FFA(I)=(Z2S-Z1B)/(Z1A-Z1B)
            FFB(I)=(Z2S-Z1A)/(Z1B-Z1A)
          ELSEIF(K1.GT.1.AND.K1.LT.KM1-1) THEN
            Z2S=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2)
            Z1A=Z1(1+(I-1)*IXZ1+(K1-2)*KXZ1)
            Z1B=Z1(1+(I-1)*IXZ1+(K1-1)*KXZ1)
            Z1C=Z1(1+(I-1)*IXZ1+(K1+0)*KXZ1)
            Z1D=Z1(1+(I-1)*IXZ1+(K1+1)*KXZ1)
            FFA(I)=(Z2S-Z1B)/(Z1A-Z1B)*
     &             (Z2S-Z1C)/(Z1A-Z1C)*
     &             (Z2S-Z1D)/(Z1A-Z1D)
            FFB(I)=(Z2S-Z1A)/(Z1B-Z1A)*
     &             (Z2S-Z1C)/(Z1B-Z1C)*
     &             (Z2S-Z1D)/(Z1B-Z1D)
            FFC(I)=(Z2S-Z1A)/(Z1C-Z1A)*
     &             (Z2S-Z1B)/(Z1C-Z1B)*
     &             (Z2S-Z1D)/(Z1C-Z1D)
            FFD(I)=(Z2S-Z1A)/(Z1D-Z1A)*
     &             (Z2S-Z1B)/(Z1D-Z1B)*
     &             (Z2S-Z1C)/(Z1D-Z1C)
          ENDIF
        ENDDO
C  INTERPOLATE.
        DO N=1,NM
          DO I=1,IM
            K1=K1S(I,K2)
            IF(K1.EQ.0) THEN
              Q2S=Q1(1+(I-1)*IXQ1+(N-1)*NXQ1)
            ELSEIF(K1.EQ.KM1) THEN
              Q2S=Q1(1+(I-1)*IXQ1+(KM1-1)*KXQ1+(N-1)*NXQ1)
            ELSEIF(K1.EQ.1.OR.K1.EQ.KM1-1) THEN
              Q1A=Q1(1+(I-1)*IXQ1+(K1-1)*KXQ1+(N-1)*NXQ1)
              Q1B=Q1(1+(I-1)*IXQ1+(K1+0)*KXQ1+(N-1)*NXQ1)
              Q2S=FFA(I)*Q1A+FFB(I)*Q1B
            ELSE
              Q1A=Q1(1+(I-1)*IXQ1+(K1-2)*KXQ1+(N-1)*NXQ1)
              Q1B=Q1(1+(I-1)*IXQ1+(K1-1)*KXQ1+(N-1)*NXQ1)
              Q1C=Q1(1+(I-1)*IXQ1+(K1+0)*KXQ1+(N-1)*NXQ1)
              Q1D=Q1(1+(I-1)*IXQ1+(K1+1)*KXQ1+(N-1)*NXQ1)
              Q2S=MIN(MAX(
     &            FFA(I)*Q1A+FFB(I)*Q1B+FFC(I)*Q1C+FFD(I)*Q1D,
     &            MIN(Q1B,Q1C)),MAX(Q1B,Q1C))
            ENDIF
            Q2(1+(I-1)*IXQ2+(K2-1)*KXQ2+(N-1)*NXQ2)=Q2S
          ENDDO
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE VINTTR(IM,IX,KM1,KM2,NT,P1,T1,Q1,P2,T2,Q2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    VINTG       VERTICALLY INTERPOLATE UPPER-AIR FIELDS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!   PRGMMR: S. MOORTHI       ORG: NCEP/EMC    DATE: 2006-12-12
C
C ABSTRACT: VERTICALLY INTERPOLATE UPPER-AIR FIELDS.
C   WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS ARE INTERPOLATED.
C   THE INTERPOLATION IS CUBIC LAGRANGIAN IN LOG PRESSURE
C   WITH A MONOTONIC CONSTRAINT IN THE CENTER OF THE DOMAIN.
C   IN THE OUTER INTERVALS IT IS LINEAR IN LOG PRESSURE.
C   OUTSIDE THE DOMAIN, FIELDS ARE GENERALLY HELD CONSTANT,
C   EXCEPT FOR TEMPERATURE AND HUMIDITY BELOW THE INPUT DOMAIN,
C   WHERE THE TEMPERATURE LAPSE RATE IS HELD FIXED AT -6.5 K/KM AND
C   THE RELATIVE HUMIDITY IS HELD CONSTANT.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL VINTTR(IM,IX,KM1,KM2,NT,P1,T1,Q1,P2,T2,Q2)
C   INPUT ARGUMENT LIST:
C     IM           INTEGER NUMBER OF POINTS TO COMPUTE
C     IX           INTEGER FIRST DIMENSION
C     KM1          INTEGER NUMBER OF INPUT LEVELS
C     KM2          INTEGER NUMBER OF OUTPUT LEVELS
C     NT           INTEGER NUMBER OF TRACERS
C     P1           REAL (IX,KM1) INPUT PRESSURES
C                  ORDERED FROM BOTTOM TO TOP OF ATMOSPHERE
C     T1           REAL (IX,KM1) INPUT TEMPERATURE (K)
C     Q1           REAL (IX,KM1,NT) INPUT TRACERS (HUMIDITY FIRST)
C     P2           REAL (IX,KM2) OUTPUT PRESSURES
C   OUTPUT ARGUMENT LIST:
C     T2           REAL (IX,KM2) OUTPUT TEMPERATURE (K)
C     Q2           REAL (IX,KM2,NT) OUTPUT TRACERS (HUMIDITY FIRST)
C
C SUBPROGRAMS CALLED:
C   TERP3        CUBICALLY INTERPOLATE IN ONE DIMENSION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      IMPLICIT NONE
      INTEGER IM,IX,KM1,KM2,NT
      REAL P1(IX,KM1),T1(IX,KM1),Q1(IX,KM1,NT)
      REAL P2(IX,KM2),T2(IX,KM2),Q2(IX,KM2,NT)
      REAL,PARAMETER :: DLTDZ=-6.5E-3*287.05/9.80665
      REAL,PARAMETER :: DLPVDRT=-2.5E6/461.50
      REAL Z1(IM+1,KM1),Z2(IM+1,KM2)
      REAL C1(IM+1,KM1,1+NT),C2(IM+1,KM2,1+NT),J2(IM+1,KM2,1+NT)
      INTEGER I,N,K
      REAL DZ
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE LOG PRESSURE INTERPOLATING COORDINATE
C  AND COPY INPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS

!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(K,I)
      DO K=1,KM1
        DO I=1,IM
          Z1(I,K)=-LOG(P1(I,K))
          C1(I,K,1)=T1(I,K)
          C1(I,K,2)=Q1(I,K,1)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
      DO N=2,NT
        DO K=1,KM1
          DO I=1,IM
            C1(I,K,1+N)=Q1(I,K,N)
          ENDDO
        ENDDO
      ENDDO
!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(K,I)
      DO K=1,KM2
        DO I=1,IM
          Z2(I,K)=-LOG(P2(I,K))
        ENDDO
      ENDDO
!$OMP END PARALLEL DO

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PERFORM LAGRANGIAN ONE-DIMENSIONAL INTERPOLATION
C  THAT IS 4TH-ORDER IN INTERIOR, 2ND-ORDER IN OUTSIDE INTERVALS
C  AND 1ST-ORDER FOR EXTRAPOLATION.
      CALL TERP3(IM,1,1,1,1,1+NT,(IM+1)*KM1,(IM+1)*KM2,
     &           KM1,IM+1,IM+1,Z1,C1,KM2,IM+1,IM+1,Z2,C2,J2)
!     print *,' c2=',c2(1,:,:)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COPY OUTPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS
C  EXCEPT BELOW THE INPUT DOMAIN, LET TEMPERATURE INCREASE WITH A FIXED
C  LAPSE RATE AND LET THE RELATIVE HUMIDITY REMAIN CONSTANT.
      DO K=1,KM2
!       print *,' p2=',p2(1,k),' k=',k
!       print *,' J2=',j2(1,k,3),' k=',k
        DO I=1,IM
          DZ=Z2(I,K)-Z1(I,1)
          IF(DZ.GE.0) THEN
            T2(I,K)=C2(I,K,1)
            Q2(I,K,1)=C2(I,K,2)
          ELSE
            T2(I,K)=T1(I,1)*EXP(DLTDZ*DZ)
            Q2(I,K,1)=Q1(I,1,1)*EXP(DLPVDRT*(1/T2(I,K)-1/T1(I,1))-DZ)
          ENDIF
        ENDDO
      ENDDO
      DO N=2,NT
        DO K=1,KM2
          DO I=1,IM
            Q2(I,K,N)=C2(I,K,1+N)
          ENDDO
        ENDDO
      ENDDO
      END

!----------------------------------------------------------------------------------
      SUBROUTINE REVERS(IM, JM, F)
!
      implicit none
      integer im, jm
      REAL F(IM,JM), WRK(IM,JM)
!
      real    tem
      integer i, imb2, j, jr, ir
!
!     reverse east-west and north-south
!
      imb2 = im / 2
      do j=1,jm
        do i=1,im
          WRK(i,j) = F(i,j)
        enddo
      enddo
      do j=1,jm
         jr = jm - j + 1
         do i=1,im
            ir = i + imb2
            if (ir .gt. im) ir = ir - im
            f(i,j) = WRK(ir,jr)
         enddo
      enddo
!
!    This is for the south pole point
!
      tem = 0.0
      do i=1,im
        tem= tem + F(I,jm)
      enddo
      tem = tem / im
      do i=1,im
         F(I,jm) = tem
      enddo
!
      return
      end

!----------------------------------------------------------------------------------
       subroutine getomega(jcap,nc,km,idvc,idvm,idrt,idsl,nvcoord,
     &      vcoord,
     &      lonb,latb,ijl,ijn,j1,j2,jc,sd,sps,psi,ti,ui,vi,wi)
!!!!!
!cggg
       use sigio_module, only : sigio_modprd
       implicit none
!
       integer,intent(in):: jcap,nc,km,idvc,idvm,idrt,idsl,nvcoord
       integer,intent(in):: lonb,latb,ijl,j1,j2,jc,ijn
       real,intent(in):: vcoord(km+1,nvcoord)
       real,intent(in):: sd(nc,km),sps(nc)
       real,intent(in):: psi(ijn),ti(ijn,km),ui(ijn,km),vi(ijn,km)
       real,intent(out):: wi(ijn,km)
       real :: pd(ijn,km),pi(ijn,km+1),pm(ijn,km)
       real :: os
       real dpmdps(ijn,km),dpddps(ijn,km),dpidps(ijn,km+1),vgradp,psmean
       real di(ijn,km),psx(ijn),psy(ijn)
       integer k,i,ij,lonb2,in,is,iret
!----1. spectral transform
      lonb2=lonb*2
      ij=lonb2*(j2-j1+1)
      in=1
      is=1+lonb
      call sptrand(0,jcap,idrt,lonb,latb,1,1,1,lonb2,lonb2,nc,ijn,
     &     j1,j2,jc,sps,psmean,
     &     psx(in),psx(is),psy(in),psy(is),1)
      SELECT CASE(MOD(IDVM,10))
      CASE(0,1)
          continue
      CASE(2)
!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(i)
          do i=1,ijn
           psx(i)=psx(i)/(psi(i)*1.0E-3)
           psy(i)=psy(i)/(psi(i)*1.0E-3)
          enddo
!$OMP END PARALLEL DO
      CASE DEFAULT
!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(i)
          do i=1,ijn
           psx(i)=psx(i)/psi(i)
           psy(i)=psy(i)/psi(i)
          enddo
!$OMP END PARALLEL DO
      END SELECT

!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(K)
      do K=1,km
        call sptran(0,jcap,idrt,lonb,latb,1,1,1,lonb2,lonb2,nc,ijn,
     &     j1,j2,jc,sd(1,k),di(in,k),di(is,k),1)
      enddo
!$OMP END PARALLEL DO
!      call hhmaxmin(psx,ijn,1,1,1,1,'in getomega psx')
!      call hhmaxmin(psy,ijn,1,1,1,1,'in getomega psy')
!      call hhmaxmin(ui,ijn,1,1,1,km,'in getomega ui')
!      call hhmaxmin(vi,ijn,1,1,1,km,'in getomega vi')
!      call hhmaxmin(ti,ijn,1,1,1,km,'in getomega ti')
!      call hhmaxmin(di,ijn,1,1,1,km,'in getomega di')
!      print *,'idvc=',idvc,'idsl=',idsl,'nvcoord=',nvcoord
!----2. pm,pd,dpmdps,dpddps
!       do k=1,km
!        u(k)=ui(i,k)
!        v(k)=vi(i,k)
!        t(k)=ti(i,k)
!        d(k)=di(i,k)
!       enddo
!       call sigio_modpr(1,1,km,nvcoord,idvc,idsl,vcoord,iret,
!     &      ps=psi(i),t=ti,pm=pm,pd=pd,dpmdps=dpmdps,dpddps=dpddps)

       call sigio_modprd(ijl,ijn,km,nvcoord,idvc,idsl,vcoord,iret,
     &             ps=psi,t=ti,pm=pm,pd=pd,dpmdps=dpmdps,dpddps=dpddps)
!       print *,'after sigio_modpr'

!----3.omeda from modstuff
!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(i)
      do i=1,ijl
       pi(i,1)=psi(i)
       dpidps(i,1)=1.
       do k=1,km
         pi(i,k+1)=pi(i,k)-pd(i,k)
         dpidps(i,k+1)=dpidps(i,k)-dpddps(i,k)
       enddo
       os=0.
       do k=km,1,-1
        vgradp=ui(i,k)*psx(i)+vi(i,k)*psy(i)
        os=os-vgradp*psi(i)*(dpmdps(i,k)-dpidps(i,k+1))-                 &
     &     di(i,k)*(pm(i,k)-pi(i,k+1))
        wi(i,k)=vgradp*psi(i)*dpmdps(i,k)+os
        os=os-vgradp*psi(i)*(dpidps(i,k)-dpmdps(i,k))-                   &
     &     di(i,k)*(pi(i,k)-pm(i,k))
       enddo
!
      enddo
!$OMP END PARALLEL DO
!      call hhmaxmin(wi,ijn,1,1,1,km,'in getomega wi')
!---
       return
       end subroutine 
!--------------------------------------------------------------------------
       subroutine REORD(IMO,JMO,LEVSO,NTRACO,
     &                  ZSO,PSO,TO,UO,VO,QO )
!--------------------------------------------
!
!  programmer: J Wang   reorder grid fields to prepare for spetral transform
!
       implicit none
!
       integer, intent(in) :: imo,jmo,levso,ntraco
       real,dimension(imo,jmo),intent(inout)         ::  zso,pso
       real,dimension(imo,jmo,levso),intent(inout)   ::  to,uo,vo
       real,dimension(imo,jmo,levso,ntraco),intent(inout)   ::  QO
!local vars
!
       integer i,j,k,n
       real,allocatable :: tmp(:,:)
!
       call ordns(imo,jmo,zso)
       call ordns(imo,jmo,pso)
!
       do k=1,levso
         call ordns(imo,jmo,to(:,:,k))
         call ordns(imo,jmo,uo(:,:,k))
         call ordns(imo,jmo,vo(:,:,k))
       enddo
!
       do n=1,ntraco
         do k=1,levso
           call ordns(imo,jmo,qo(:,:,k,n))
         enddo
       enddo
!
       return
      end subroutine reord
!
      subroutine ordns(im,jm,data2d)
!
        implicit none
!
        integer, intent(in) :: im,jm
        real,dimension(im,jm),intent(inout) :: data2d
!
        integer i,j ,jn,js,jj
        real,allocatable :: tmp(:,:)
!
        allocate(tmp(im,jm))
        do j=1,(jm+1)/2
          JN=J
          JS=JM+1-J
          jj = 2*(J-1)+1
          do i=1,im
            TMP(i,jj)   = data2d(i,JN)
            TMP(i,jj+1) = data2d(i,JS)
          enddo
        enddo
        do j=1,jm
          do i=1,im
            data2d(i,j) = tmp(i,j)
          enddo
        enddo
        deallocate(tmp)
!
        return
      end subroutine ordns
      subroutine sptrrj(imax,lonsperlat,grid,gred,idir)
        implicit none
        integer,intent(in):: imax,lonsperlat,idir
        real,intent(inout):: grid(imax),gred(imax)
        real four(imax+2),gour(lonsperlat)
        integer i,iour
        real rred

        rred = lonsperlat/real(imax)
        four = 0
!! take transformed to full 'grid' and make it like transformed to reduced and interpolated 'gred'
        if(idir > 0) then
          call spfft1(imax,imax/2+1,imax,1,four,grid,-idir)
          call spfft1(lonsperlat,imax/2+1,imax,1,four,gour,idir)
          do i=1,imax
            iour = nint((i-1)*rred)+1
            if(iour == lonsperlat+1) iour  =1
            gred(i) = gour(iour)
          enddo
!! take transformed to reduced and interpolated 'gred' and make it like transformed to full 'grid'
        elseif(idir < 0) then
          do iour=1,lonsperlat
            i = nint((iour-1)/rred)+1
            if(i == imax+1) i = 1
            gour(iour) = gred(i)
          enddo
          call spfft1(lonsperlat,imax/2+1,imax,1,four,gour,idir)
          call spfft1(imax,imax/2+1,imax,1,four,grid,-idir)
        endif
      end subroutine
