!-----------------------------------------------------------------------
      PROGRAM CHGRES
!C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: GLOBAL_CHGRES
!   PRGMMR: IREDELL          ORG: NP23        DATE: 1999-09-10
!
! ABSTRACT: THIS PROGRAM INTERPOLATES THE SIGMA, SURFACE AND NSST RESTART
!   FILES FROM THE GLOBAL SPECTRAL MODEL TO THE FV3 CUBED SPHERE GRID.  
!
!   THE PROCEDURE FOR CHANGING THE SIGMA FILE RESOLUTION IS THUS:
!   WHEN THE SIGMA FILE IS SIGIO FORMAT (SPECTRAL COEFFICIENTS)
!   THE DATA IS FIRST CONVERTED FROM SPECTRAL TO GRID POINT 
!   SPACE (A GLOBAL GAUSSIAN OR REGULAR LAT/LON GRID).  THE DIMENSIONS
!   OF THAT GLOBAL GRID ARE USER-SELECTABLE.   BY DEFAULT, THE 
!   DIMENSIONS ARE DETERMINED FROM THE JCAP VALUE OF THE INPUT
!   DATA.  NO CONVERSION IS REQUIRED WHEN THE WHEN THE SIGMA FILE
!   IS IN NEMSIO FORMAT AS THE DATA ARE ALREADY IN GRID POINT SPACE.
!   IF THE USER SELECTS A NEW NUMBER OF VERTICAL LEVELS OR A 
!   DIFFERENT TYPE OF LEVEL (I.E., HYBRID OR SIGMA), THEN A VERTICAL
!   INTERPOLATION IS PERFOMRED.  THE DATA ARE THEN INTERPOLATED
!   FROM THE GLOBAL GRID TO THE SIX TILES OF THE FV3 CUBED SPHERE
!   GRID.
!
!   THE SURFACE FILE CONVERSION IS DONE FOR A SINGLE FV3 TILE ONLY.
!   SO FOR A GLOBAL SIX TILE FV3 GRID, THIS PROGRAM MUST BE CALLED
!   SIX TIMES.  THE PROCEDURE FOR INTERPOLATING SURFACE DATA IS:
!   NEAREST NEIGHBOR INTERPOLATION IS PERFORMED SO THAT LAND/NONLAND
!   POINTS ON THE INPUT GRID ARE MAPPED TO LAND/NONLAND POINTS
!   ON THE TARGET GRID.  IF THE INPUT FILE CONTAINS LANDICE
!   AND THE OUTPUT GRID IS TO HAVE LANDICE, THEN NONLAND IS
!   MAPPED TO NONLAND, LANDICE IS MAPPED TO LANDICE, ICE FREE
!   LAND IS MAPPED TO ICE FREE LAND. OPTIONALLY, THE CLIMO FIELDS
!   SUCH AS ALBEDO, ROUGHNESS, ETC, MAY DETERMINED ON THE OUTPUT
!   GRID FROM SFCCYCLE (WHICH IS CALLED FROM THE SURFACE
!   CHGRES MODULE).  THE LATTER IS RECOMMENDED WHEN CONVERTING
!   FROM A LOW TO HIGH RESOLUTION GRID.  THE LAND-SEA MASK ON THE
!   TARGET FV3 TILE IS READ FROM FILE. SKIN AND SOIL TEMPERATURE OVER
!   LAND ARE ADJUSTED FOR DIFFERENCES BETWEEN THE INPUT AND OUTPUT
!   OROGRAPHY. LIQ SOIL MOISTURE IS CALCULATED ACCORDING TO THE
!   ADJUSTED TEMP. OROGRAPHY ON THE TARGET FV3 TILE IS READ FROM
!   THE SAME FILE AS THE FV3 LAND-SEA MASK.  THE PROGRAM WILL ALSO
!   CONVERT FROM TWO TO FOUR SOIL LAYERS AND VICE VERSA. IT WILL
!   WILL INITIALIZE ALL LAND STATES FOR THE LANDICE PHYSICS IF
!   DESIRED.  THE PROGRAM WILL SCALE TOTAL SOIL MOISTURE FOR ANY
!   DIFFERENCES IN SOIL TYPE BETWEEN THE INPUT AND OUTPUT GRIDS.
!
!   THE PROCEDURE FOR CONVERTING AN NSST FILE IS THUS:
!   NEAREST NEIGHBOR INTERPOLATION IS USED TO MAP TO THE OUTPUT
!   GRID AS SOME NSST FIELDS ARE NOT CONTINUOUS.  NSST FIELDS
!   ARE ONLY REQUIRED AT OPEN WATER POINTS.  FOR CONSISTENCY,
!   THESE OPEN WATER POINTS ARE DETERMINED BY THE FV3 LAND-SEA
!   MASK OUTPUT FROM THE SURFACE FILE CONVERSION STEP (WHICH
!   INCLUDES SEA ICE).  THEREFORE, WHEN CONVERTING AN NSST FILE,
!   A SURFACE RESTART FILE MUST ALSO BE CONVERTED. AS FOR THE
!   SURFACE, THE NSST CONVERSION IS DONE FOR A SINGLE FV3 TILE
!   ONLY.
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
!   2017-01-27 Gayno/GFDL : extensive modifications to interpolate gfs data 
!                           to fv3 cubed sphere grid.
!   2017-04-12 Gayno      : Write nsst records to the surface netcdf restart
!                           file.  Previously, the nsst records were written
!
! NAMELISTS:
!   NAMCHG:
!     LEVS       INTEGER NEW NUMBER OF ATMOSPHERIC LEVELS (DEFAULT: NO CHANGE
!                FROM INPUT GFS DATA)
!     NTRAC      INTEGER NEW NUMBER OF TRACERS (DEFAULT: NO CHANGE FROM INPUT
!                GFS DATA)
!     LONB       INTEGER NUMBER OF LONGITUDES - GFS GAUSSIAN INPUT GRID.  ONLY
!                USED WHEN INPUT GFS DATA IS SFCIO FORMAT. (DEFAULT: FROM
!                INPUT GFS SFCIO DATA HEADER)
!     LATB       INTEGER NUMBER OF LATITUDES - GFS GAUSSIAN INPUT GRID.  ONLY
!                USED WHEN INPUT GFS DATA IS SFCIO FORMAT. (DEFAULT: FROM
!                INPUT GFS SFCIO DATA HEADER)
!     IDVC       INTEGER NEW VERTICAL COORDINATE ID
!                (1 FOR SIGMA, 2 FOR HYBRID, 3 GENERAL HYBRID) (DEFAULT: SAME AS
!                INPUT GFS DATA)
!     IDVM       INTEGER NEW VERTICAL MASS VARIABLE ID
!                (1 FOR LN(PS) AND 2 FOR PS (KPA)) (DEFAULT: SAME AS INPUT GFS
!                DATA).
!     IDSL       INTEGER NEW TYPE OF SIGMA STRUCTURE
!                (1 FOR PHILLIPS OR 2 FOR MEAN) (DEFAULT: SAME AS INPUT GFS DATA)
!     MQUICK     INTEGER FLAG TO SKIP VERTICAL INTERPOLATION.  USEFUL WHEN THE
!                NUMBER AND TYPE OF VERTICAL LEVELS IS NOT CHANGING.  ONLY
!                ACTIVE WHEN INPUT GFS DATA IS NEMSIO FORMAT.  (DEFAULT: 0 -
!                DONT SKIP.  TO SKIP, SET TO 1)
!     IDVT       INTEGER NEW TRACER VARIABLE ID (DEFAULT: SAME AS INPUT GFS DATA)
!     LATCH      INTEGER NUMBER OF GAUSSIAN LATITUDES TO PROCESS AT ONE TIME
!                (DEFAULT: 8)
!     LSOIL      INTEGER NEW NUMBER OF SOIL LAYERS (DEFAULT: NO CHANGE FROM INPUT
!                GFS DATA).
!     IVSSFC     INTEGER NEW VERSION NUMBER SFC FILE (DEFAULT: NO CHANGE FROM INPUT
!                GFS DATA).
!     NVCOORD    INTEGER NEW NUMBER OF VERTICAL COORDINATES
!                (DEFAULT: NO CHANGE FROM INPUT GFS DATA)
!     IDRT       SPECTRAL TO INTERMEDIATE GRID OPTION.  WHEN INPUT GFS SIGMA FILE IS
!                SIGIO FORMAT (SPECTRAL COEFFICIENTS), CHGRES FIRST CONVERTS TO A
!                TO A GLOBAL GRID. THAT GLOBAL GRID IS IN TURN INTERPOLATED TO THE
!                FV3 GRID TILES.  THE IDRT OPTION CONTROLS WHAT GLOBAL GRID IS
!                CHOSEN.  DEFAULT IS '4' - GLOBAL GAUSSIAN GRID.  CAN ALSO CHOOSE
!                '0' - GLOBAL REGULAR LAT/LON GRID.
!     OUTTYP     INTEGER NUMBER OF OUTPUT FILE TYPE.  NOT USED YET.  CURRENTLY,
!                THE FV3 SIGMA AND SURFACE/NSST FILES ARE NETCDF FORMAT ONLY.
!     CHGQ0      SET NEGATIVE VALUES OF TRACERS TO ZERO: 0 FALSE; 1 TRUE.
!     REGIONAL   FLAG FOR PROCESSING STAND-ALONE NEST.  WHEN '1', REMOVE HALO
!                FROM GRIDS AND CREATE AN ATMOSPHERIC BOUNDARY FILE.  WHEN '2',
!                CREATE BOUNDARY FILE ONLY.  WHEN '0', PROCESS NORMALLY AS
!                FOR A GLOBAL GRID.  DEFAULT IS '0'.
!     HALO       WHEN RUNNING A STAND-ALONE NEST, THIS SPECIFIES THE NUMBER OF
!                ROWS/COLS FOR THE HALO.
!
! INPUT FILES:
!   UNIT   11    chgres.inp.sig          GFS SIGMA FILE (IN EITHER SIGIO OR NEMSIO FORMAT)
!   UNIT   13    chgres.inp.siglevel     NEW VERTICAL STRUCTURE
!   UNIT   21    chgres.inp.sfc          GFS SURFACE FILE (SFCIO OR NEMSIO FORMAT)
!   UNIT   17    chgres.inp.lpl3         WHEN GFS SIGMA FILE IS SIGIO (SPECTRAL), CHGRES
!                                        FIRST CONVERTS TO A GLOBAL GAUSSIAN OR LAT/LON
!                                        GRID. THIS GRID CAN BE REDUCED (# GRID POINTS
!                                        DECREASES TOWARDS THE POLE).  THIS FILE DEFINES
!                                        THE REDUCED GRID.
!   UNIT   31    chgres.inp.nst          NSST FILE FOR INPUT GRID (NSTIO OR NEMSIO FORMAT)
!   UNIT   ??    chgres.fv3.orog.t[1-6]  FV3 OROGRAPHY AND LAND MASK.  ONE FILE FOR EACH
!                                        SIX GLOBAL TILES (NETCDF FORMAT)
!   UNIT   ??    chgres.fv3.grd.t[1-6]   FV3 GRID SPECS FILE. ONE FILE FOR EACH SIX
!                                        GLOBAL TILES (NETCDF FORMAT)
! OUTPUT FILES:
!   UNIT   ??    gfs_ctrl.nc             FV3 ATMOSPHERIC HEADER FILE (NETCDF FORMAT)
!   UNIT   ??    out.sfc.tile[1-6].nc    FV3 SURFACE/NSST DATA FILE (NETCDF FORMAT)  ONE
!                                        FILE FOR EACH SIX GLOBAL TILES.
!   UNIT   ??    gfs_data.tile[1-6].nc   FVN ATMOSPHERIC FILE (NETCDF FORMAT).  ONE FILE
!                                        FOR EACH SIX GLOBAL TILES.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
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
      USE NEMSIO_MODULE
      USE NEMSIO_GFS
      USE SFCIO_MODULE
      USE FUNCPHYS
      USE SURFACE_CHGRES

      IMPLICIT NONE
      INTEGER:: LEVS=0,NTRAC=0,LONB=0,LATB=0,                      &
                IDVC=0,IDVM=0,IDSL=0,MQUICK=0,IDVT=0,        &
                LATCH=8,LSOIL=0,IVSSFC=0,NVCOORD=0,                &
                IDRT=4,OUTTYP=999,IALB=0,CHGQ0=0,ISOT=0,IVEGSRC=0, &
                NTILES=6,TILE_NUM=1,REGIONAL=0,HALO=0
!
      REAL, PARAMETER :: PIFAC=180/ACOS(-1.0)
!
      REAL RI(0:20),CPI(0:20)

      LOGICAL USE_UFO, NST_ANL, RDGRID, NOPDPVV

      NAMELIST/NAMCHG/ LEVS,NTRAC,LONB,LATB,                       &
                       IDVC,IDVM,IDSL,MQUICK,IDVT,LATCH,       &
                       LSOIL,IVSSFC,NVCOORD,OUTTYP,IDRT,RI,CPI,    &
                       IALB,CHGQ0,USE_UFO,NST_ANL,RDGRID,      &
                       NOPDPVV,ISOT,IVEGSRC,NTILES,TILE_NUM,   &
                       REGIONAL, HALO
!
      INTEGER NSIGI,NSIL,NSIGO,                    & 
              IRET,IOSSIL,IRET0,IRET1,             &
              NCI,IMI,JMI,IMO,JMO,IJX,NTRACM,      &
              J1,J2,JL,IJL,J,JN,JS,N,              &
              NTRACO, IJMO, LATCH2, K, LATG2,      &
              NSFCI,NSFCO,INPTYP,                  &
              SFCPRESS_ID_I, THERMODYN_ID_I,       &
              SFCPRESS_ID_O, THERMODYN_ID_O,       &
              NREC, LEVSI, LEVSO, I, L

      INTEGER                 :: IMO_WITH_HALO, JMO_WITH_HALO
      INTEGER                 :: NSST_YEAR, NSST_MON
      INTEGER                 :: NSST_DAY, NSST_HOUR
      INTEGER, DIMENSION(200) :: KGDS_INPUT, KGDS_OUTPUT
      REAL, ALLOCATABLE       :: MASK_OUTPUT(:),  RLATS_OUTPUT(:),    &
                                 RLONS_OUTPUT(:), NSST_INPUT(:,:,:),  &
                                 MASK_INPUT(:,:), NSST_OUTPUT(:,:)
      INTEGER, PARAMETER      :: NUM_NSST_FIELDS=18
      TYPE(SFC2D)             :: SFCINPUT
      TYPE(SFC1D)             :: SFCOUTPUT
      LOGICAL, PARAMETER      :: MERGE=.FALSE.
      LOGICAL                 :: DO_NSST
      REAL,ALLOCATABLE        :: SLAT(:),WLAT(:),CLAT(:),RLAT(:)
      REAL,ALLOCATABLE        :: OROGO(:,:), OROGO_UF(:,:)
      REAL,ALLOCATABLE        :: ZSI(:,:),    PSI(:,:), PI(:,:,:),    &
                                 TI(:,:,:),   UI(:,:,:),VI(:,:,:),    &
                                 QI(:,:,:,:), WI(:,:,:), TIV(:,:,:),  & 
                                 XCP(:,:,:),VIRT(:,:,:),SUMQ(:,:,:)
      REAL,ALLOCATABLE        :: ZSO(:,:),  PSO(:,:), PO(:,:,:),      &
                                 TO(:,:,:), UO(:,:,:),VO(:,:,:),      &
                                 QO(:,:,:,:), WO(:,:,:), TPO(:,:),    &
                                 DTDPO(:,:,:),DPO(:,:,:)
      REAL,ALLOCATABLE        :: F10MI(:,:),T2MI(:,:),Q2MI(:,:)
      REAL,ALLOCATABLE        :: UUSTARI(:,:),FFMMI(:,:),FFHHI(:,:)
      REAL,ALLOCATABLE        :: TPRCPI(:,:),SRFLAGI(:,:)
      REAL,ALLOCATABLE        :: F10MO(:,:),T2MO(:,:),Q2MO(:,:)
      REAL,ALLOCATABLE        :: UUSTARO(:,:),FFMMO(:,:),FFHHO(:,:)
      REAL,ALLOCATABLE        :: TPRCPO(:,:),SRFLAGO(:,:)
      TYPE(SIGIO_HEAD)        :: SIGHEADI,SIGHEADO
      TYPE(SIGIO_DBTA)        :: SIGDATAI
      TYPE(SFCIO_HEAD)        :: SFCHEADI
      REAL,ALLOCATABLE        :: SLMSKO(:,:)
      REAL,ALLOCATABLE        :: GEOLAT(:,:), GEOLON(:,:), TMPVAR(:,:)
      REAL,ALLOCATABLE        :: TMPLAT(:,:), TMPLON(:,:)
      REAL                    :: FCSTHOUR, NSST_FHOUR
      INTEGER                 :: IOLPL3,NLPL3,FILESZ
      INTEGER,ALLOCATABLE     :: LPL3(:)
      REAL, ALLOCATABLE       :: AK(:), BK(:), CK(:), VCOORD(:,:),   &
                                 VCOORDI(:,:), VCOORDO(:,:)

! Define variables for NEMSIO
      TYPE(NEMSIO_GFILE) :: GFILEI,GFILEISFC
      TYPE(NEMSIO_HEAD)  :: GFSHEADI
      TYPE(NEMSIO_HEADV) :: GFSHEADVI
      TYPE(NEMSIO_DBTA)  :: GFSDATAI
      TYPE(NEMSIO_DBTA)  :: GFSDATAO
      CHARACTER(8)       :: FILETYPE
      CHARACTER(LEN=16)  :: FILETYPE2
!
! Define local vars:
      INTEGER LONBO,LATBO,IDVCO,IDVMO,IDSLO,IDVTO,      &
              NVCOORDO,IDATE4O(4),LSOILO,IVSO,          &
              I_OZN,I_CLD, LSOILI, IVSI
      CHARACTER(16),allocatable :: TRAC_NAME(:)
      REAL(4) FHOURO

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  EXECUTION BEGINS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      RI      = 0.0
      CPI     = 0.0
      USE_UFO = .FALSE.
      NST_ANL = .FALSE.
      RDGRID  = .FALSE.
      NOPDPVV = .FALSE.

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ NAMELIST
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      CALL W3TAGB('GLOBAL_CHGRES',1999,0253,0056,'NP23')
      CALL GFUNCPHYS
      READ(*,NAMCHG)
      WRITE(6,NAMCHG)
 
      LATCH2 = LATCH + LATCH

      IF (REGIONAL == 1) THEN
        PRINT*,"WILL CREATE GRIDS WITHOUT HALO."
        PRINT*,"WILL CREATE ATMOSPHERIC BOUNDARY FILE."
      ELSEIF (REGIONAL == 2) THEN
        PRINT*,"WILL CREATE ATMOSPHERIC BOUNDARY FILE ONLY."
      ELSE
        HALO = 0
      ENDIF

      IF (REGIONAL > 0) THEN
        IF (HALO == 0) THEN
          PRINT *,'FATAL ERROR: MUST SPECIFIY NON-ZERO HALO.'
          CALL ERREXIT(51)
        ELSE
          PRINT*,"USER SPECIFIED HALO IS: ", HALO, " ROWS/COLS."
        ENDIF
      ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OPEN INPUT ATMOSPHERIC FILE.  DETERMINE FILE TYPE.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      NSIGI    = 11
      NSIL     = 13
      NLPL3    = 17
      NSIGO    = 51
      INPTYP   = 0

      CALL NEMSIO_INIT(IRET)

      OPEN (NSIGI, FILE='chgres.inp.sig', ACCESS='DIRECT', RECL=16, IOSTAT=IRET)
      READ (NSIGI, REC=1, IOSTAT=IRET1) FILETYPE2
      CLOSE (NSIGI)

      IF(IRET == 0 .AND. IRET1 == 0) THEN
        IRET = INDEX(FILETYPE2, "NEMSIO")
        IF (IRET /= 0) THEN
          INPTYP = 1
          PRINT*,'INPUT ATMOS FILE chgres.inp.sig IS NEMSIO FORMAT'
        ELSE
          CALL SIGIO_SROPEN(NSIGI,'chgres.inp.sig',IRET)
          CALL SIGIO_SRHEAD(NSIGI,SIGHEADI,IRET1)
          IF(IRET == 0 .AND. IRET1 == 0) THEN
            INPTYP = 2
            PRINT*,'INPUT ATMOS FILE chgres.inp.sig IS SIGIO FORMAT'
          ENDIF
        ENDIF
      ENDIF

      IF(INPTYP /= 0) THEN
        OPEN(NSIL,FILE='chgres.inp.siglevel',       &
             FORM='FORMATTED',STATUS='OLD',IOSTAT=IRET)
        IF(IRET /= 0) NSIL = 0
      ELSE
        PRINT*,'--- FAILED TO OPEN ATMOS FILE chgres.inp.sig ---'
        PRINT*,'--- PROCEED TO CHANGE SFC FILE chgres.inp.sfc ---'
        NSIGO = 0
      ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CHANGE RESOLUTION OF INPUT ATMOS SIGIO FILE.
!  OUTPUT ATMOS FILE ON FV3 CUBED-SPHERE GRID WILL BE NETCDF FORMAT.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      IF(INPTYP == 2) THEN

        PRINT*, 'CHGRES INPUT:  GFS SPECTRAL SIGIO SIGMA FILE '
        PRINT*, 'CHGRES OUTPUT: FV3 NETCDF FILE'

        IF (CPI(0) == 0.0) THEN
          IF (MOD(SIGHEADI%IDVM/10,10) == 3) THEN
            DO N=1,SIGHEADI%NTRAC+1
              CPI(N-1) = SIGHEADI%CPI(N)
              RI(N-1)  = SIGHEADI%RI(N)
            ENDDO
          ENDIF
        ENDIF

        LEVSI = SIGHEADI%LEVS
        LEVSO = LEVSI
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
            PRINT *,'FATAL ERROR: Incompatible values specified for NTRAC & IDVT'
            CALL ERREXIT(13)
          ENDIF
        ENDIF

        ALLOCATE(TRAC_NAME(NTRACO))

        CALL GET_TRACERS(IDVT, NTRACO, I_CLD, I_OZN, TRAC_NAME)

        IDVCO    = SIGHEADI%IDVC
        IDVMO    = SIGHEADI%IDVM
        IDSLO    = SIGHEADI%IDSL
        IDVTO    = SIGHEADI%IDVT
        NVCOORDO = SIGHEADI%NVCOORD

        IF (NVCOORDO >= 3 ) THEN
          IF (SIGHEADI%VCOORD(1,NVCOORDO) < 0.0) THEN
              SIGHEADI%VCOORD(:,NVCOORDO) = 0.0
              SIGHEADI%NVCOORD = 2
          ENDIF
        ENDIF

        IF(IDVC   >  0) IDVCO    = IDVC
        IF(IDVM   >= 0) IDVMO    = IDVM
        IF(IDSL   >= 0) IDSLO    = IDSL
        IF(IDVT   >  0) IDVTO    = IDVT
        IF(IDVC == 1)   NVCOORDO = 1
        IF(NVCOORD > 0) NVCOORDO = NVCOORD

        FHOURO = SIGHEADI%FHOUR
        IDATE4O(1:4) = SIGHEADI%IDATE(1:4)
 
        IF(MQUICK == 1) THEN
          PRINT*,''
          PRINT*,' MQUICK OPTION NOT AVAILABLE WHEN INPUT DATA IS'
          PRINT*,' SPECTRAL GFS AND OUTPUT IS FV3 CUBE GRID.'
          PRINT*,' IGNORING MQUICK SETTING.'
        ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW SIGMA LEVELS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        IF(NSIL /= 0) THEN
          ALLOCATE(VCOORDO(LEVSO+1,NVCOORDO))
          CALL NEWSIG(NSIL,IDVCO,LEVSO,NVCOORDO,VCOORDO,IOSSIL)
          IF(IOSSIL == 0) THEN
            PRINT*,"NEW MODEL LEVELS READ IN"
          ENDIF
        ELSEIF(IDVCO    == SIGHEADI%IDVC .AND.        &
               LEVSO    == SIGHEADI%LEVS .AND.        &
               NVCOORDO == SIGHEADI%NVCOORD) THEN
          ALLOCATE(VCOORDO(LEVSO+1,NVCOORDO))
          VCOORDO = SIGHEADI%VCOORD
          IOSSIL = 0
          PRINT*,"NEW MODEL LEVELS COPIED FROM OLD"
        ELSE
          IOSSIL = 42
        ENDIF
        IF(IOSSIL /= 0) THEN
          PRINT*,''
          PRINT*,"FATAL ERROR DEFINING SIGMA VALUES."
          PRINT*,"IOSSIL IS: ",IOSSIL
          CALL ERREXIT(8)
        ENDIF
 
        PRINT*,''
        PRINT*,"INPUT SIGMA FILE SPECS:"
        PRINT*," WAVES:   ",SIGHEADI%JCAP
        PRINT*," LEVELS:  ",SIGHEADI%LEVS
        PRINT*," NTRAC:   ",SIGHEADI%NTRAC
        PRINT*," IVS:     ",SIGHEADI%IVS
        PRINT*," IDVC:    ",SIGHEADI%IDVC
        PRINT*," NVCOORD: ",SIGHEADI%NVCOORD
        PRINT*," IDVM:    ",SIGHEADI%IDVM

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ IN 2D LONSPERLAT
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        ALLOCATE(LPL3(LATBO))
        LPL3  = LONBO                      ! full grid as default
        LATG2 = 0
        IF(RDGRID) THEN
          PRINT*,''
          PRINT*,"READ LPL FROM UNIT ",NLPL3
          OPEN(NLPL3,FILE='chgres.inp.lpl3',   &
               FORM='FORMATTED',STATUS='OLD',IOSTAT=IOLPL3)
          IF(IOLPL3 == 0) THEN
            READ(NLPL3,*,IOSTAT=IOLPL3) LATG2,LPL3(1:MIN(LATG2,(LATBO+1)/2))
            PRINT *,'LPL3 READ: EXPECTED ',(LATBO+1)/2,', READ ',LATG2
            IF(IOLPL3 == 0 .AND. LATG2 == (LATBO+1)/2) THEN
              DO J=1,LATBO/2
                LPL3(LATBO+1-J) = LPL3(J)
              ENDDO
              PRINT*,"LPL3 READ IN"
            ENDIF
          ELSE
            PRINT*,"WARNING: BAD READ OF LONSPERLAT FILE.  USE FULL GRID."
          ENDIF
        ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ OLD SIGMA FILE
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        CALL SIGIO_ALDBTA(SIGHEADI,SIGDATAI,IRET)
        IF(IRET /= 0) THEN
          PRINT*,'FATAL ERROR ALLOCATING SIGDATAI. IRET: ',IRET
          CALL ERREXIT(4)
        ENDIF
        CALL SIGIO_SRDBTA(NSIGI,SIGHEADI,SIGDATAI,IRET)
        IF(IRET /= 0) THEN
          PRINT *,'FATAL ERROR READING FILE NSIGI=',NSIGI
          CALL ERREXIT(94)
        ENDIF

        NCI    = SIZE(SIGDATAI%T,1)
        IJX    = LONBO*LATCH2
        NTRACM = MIN(SIGHEADI%NTRAC,NTRACO)
        IF (IDVT == 200) NTRACO = MIN(SIGHEADI%NTRAC,NTRACO) + 2

        CALL NEMSIO_GFS_ALGRD(LONBO,LATBO,LEVSO,NTRACO,GFSDATAO,NOPDPVV)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  USE TRANSFORMS TO CHANGE RESOLUTION
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        PRINT*,''
        IF (IDRT == 4) THEN
          PRINT*,"TRANSFORM TO A GAUSSIAN GRID OF:"
        ELSEIF (IDRT == 0) THEN
          PRINT*,"TRANSFORM TO A LAT/ON GRID OF:"
        ENDIF
        PRINT*," I/J DIMS:   ",LONBO,LATBO
        PRINT*," LEVELS:  ",LEVSO
        PRINT*," NTRAC:   ",NTRACO
        PRINT*," IDVC:    ",IDVCO
        PRINT*," NVCOORD: ",NVCOORDO
        PRINT*," IDVM:    ",IDVMO

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ALLOCATE TEMPORARY SIGIO DATA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        ALLOCATE(SLAT(LATBO), WLAT(LATBO), CLAT(LATBO), RLAT(LATBO))
        ALLOCATE(ZSI(LONBO,LATCH2), PSI(LONBO,LATCH2))
        ALLOCATE(PI(LONBO,LATCH2,LEVSI))
        ALLOCATE(TI(LONBO,LATCH2,LEVSI))
        ALLOCATE(UI(LONBO,LATCH2,LEVSI))
        ALLOCATE(VI(LONBO,LATCH2,LEVSI))
        ALLOCATE(QI(LONBO,LATCH2,LEVSI,NTRACM))
        IF (THERMODYN_ID_I == 3) THEN
          ALLOCATE(SUMQ(LONBO,LATCH2,LEVSI))
          ALLOCATE(XCP (LONBO,LATCH2,LEVSI))
        ELSE
          ALLOCATE(VIRT(LONBO,LATCH2,LEVSI))
        ENDIF
        ALLOCATE(WI(LONBO,LATCH2,LEVSI))
        ALLOCATE(ZSO(LONBO,LATCH2), PSO(LONBO,LATCH2))
        ALLOCATE(PO(LONBO,LATCH2,LEVSO))
        ALLOCATE(TO(LONBO,LATCH2,LEVSO))
        ALLOCATE(UO(LONBO,LATCH2,LEVSO))
        ALLOCATE(VO(LONBO,LATCH2,LEVSO))
        ALLOCATE(WO(LONBO,LATCH2,LEVSO))
        ALLOCATE(QO(LONBO,LATCH2,LEVSO,NTRACO))
        ALLOCATE(TPO(LONBO,LEVSO))
        ALLOCATE(DPO(LONBO,LATCH2,LEVSO))
        ALLOCATE(DTDPO(LONBO,LATCH2,LEVSO))

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW LATITUDES
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        CALL SPLAT(IDRT,LATBO,SLAT,WLAT)
!$omp parallel do private(j)
        DO J=1,LATBO
          CLAT(J) = SQRT(1-SLAT(J)*SLAT(J))
          RLAT(J) = PIFAC * ASIN(SLAT(J))
        ENDDO
        DEALLOCATE(SLAT, CLAT, WLAT)
!
! ------------------------------------------------------------------
!  GET PS and T DATA TYPE FOR THE FILE

        SFCPRESS_ID_I  = MOD(SIGHEADI%IDVM,10)
        THERMODYN_ID_I = MOD(SIGHEADI%IDVM/10,10)
        SFCPRESS_ID_O  = MOD(IDVMO,10)
        THERMODYN_ID_O = MOD(IDVMO/10,10)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  LOOP OVER LATITUDE
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        LATLOOP1 : DO J1=1,(LATBO+1)/2,LATCH

          J2  = MIN(J1+LATCH-1,(LATBO+1)/2)
          JL  = 2*(J2-J1+1)
          IJL = LONBO*JL
          CALL TRSSC(SIGHEADI%JCAP,NCI,SIGHEADI%LEVS,NTRACM,         &
                       SIGHEADI%IDVM,IDRT,LONBO,LATBO,IJX,J1,J2,1,   &
                       LPL3(J1:J2),                                  & 
                       SIGDATAI%HS,SIGDATAI%PS,SIGDATAI%T,           &
                       SIGDATAI%D,SIGDATAI%Z,SIGDATAI%Q,             &
                       ZSI,PSI,TI,UI,VI,QI)
 
          IF (THERMODYN_ID_I == 3) THEN
            XCP(:,1:JL,:)  = 0.0
            SUMQ(:,1:JL,:) = 0.0
            DO N=1,NTRACM
              IF( CPI(N) .NE. 0.0 .AND. RI(N) .NE. 0.0) THEN
               XCP(:,1:JL,:)  = XCP(:,1:JL,:)  + CPI(N)*QI(:,1:JL,:,N)
               SUMQ(:,1:JL,:) = SUMQ(:,1:JL,:) + QI(:,1:JL,:,N)
              ENDIF
            ENDDO
            XCP(:,1:JL,:)  = (1.-SUMQ(:,1:JL,:))*CPI(0)+XCP(:,1:JL,:)
          ELSE
            VIRT(:,1:JL,:) = (1.+(461.50/287.05-1)*QI(:,1:JL,:,1))
          ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CONVERT TO SURFACE PRESSURE AND TEMPERATURE
!
          SELECT CASE(SFCPRESS_ID_I)
           CASE(0,1)
            PSI(:,1:JL) = 1.E3*EXP(PSI(:,1:JL))
           CASE(2)
            PSI(:,1:JL) = 1.E3*PSI(:,1:JL)
           CASE DEFAULT
            PRINT *,' DEFAULT SELECTED: PSI IS P IN PASCAL '
          END SELECT
 
          DO I=1,LONBO  ! not using external gaussian terrain
            ZSO(I,1:JL) = ZSI(I,1:JL)
          ENDDO

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NEW PRESSURE AND NEW SURFACE PRESSURE AND OMEGA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

          IF (THERMODYN_ID_I == 3) THEN
             TI(:,1:JL,:) = TI(:,1:JL,:)/CPI(0)   ! enthalpy (CpT/Cpd)
          ENDIF

          ALLOCATE(VCOORD(SIGHEADI%LEVS+1,SIGHEADI%NVCOORD))
          VCOORD = SIGHEADI%VCOORD

          CALL GETOMEGA(SIGHEADI%JCAP,NCI,SIGHEADI%LEVS,           &
                        SIGHEADI%IDVC,SIGHEADI%IDVM,IDRT,          &
                        SIGHEADI%IDSL,SIGHEADI%NVCOORD,VCOORD,     &
                        LONBO,LATBO,IJL,IJX,J1,J2,1,SIGDATAI%D,    &
                        SIGDATAI%PS,PSI,TI,UI,VI,WI)

          CALL SIGIO_MODPRD(IJL,IJX,SIGHEADI%LEVS,SIGHEADI%NVCOORD,  &
                            SIGHEADI%IDVC,SIGHEADI%IDSL,VCOORD,IRET, &
                            PS=PSI,T=TI,PM=PI)

          DEALLOCATE (VCOORD)
!
          SELECT CASE( THERMODYN_ID_I )
           CASE(0,1)
              TI(:,1:JL,:) = TI(:,1:JL,:)/VIRT(:,1:JL,:)         ! to t
           CASE(2)
           CASE(3)
              TI(:,1:JL,:) = TI(:,1:JL,:)/XCP(:,1:JL,:)*CPI(0)   ! to t
           CASE DEFAULT
          END SELECT
 
          PSO = PSI
!
!  VERTICALLY INTERPOLATE UPPER-AIR FIELDS - HENRY JUANG'S APPROACH
!
          CALL NEWPR1(IJL,IJX,LEVSO,SIGHEADI%LEVS,IDVCO,IDVMO,IDSLO, &
                      NVCOORDO, VCOORDO,RI, CPI, NTRACM,             & 
                      PI,TI,QI,PSO,PO,DPO)

          CALL VINTG(IJL,IJX,SIGHEADI%LEVS,LEVSO,NTRACM,  &
                     PI,UI,VI,TI,QI,WI,PO,UO,VO,TO,QO,DTDPO,WO)

! idea add init condition for temp tracer4-5 ( o o2)
          IF (IDVT == 200) then
            CALL VINTG_IDEA(LONBO,LATCH,LEVSO,NTRACO,PO,RLAT,  &
                            LATBO,J1,J2,SIGHEADI%IDATE,UO,VO,TO,QO)
          ENDIF
!
          IF( IDVCO  == 3 ) THEN
            ALLOCATE(AK(LEVSO+1), BK(LEVSO+1), CK(LEVSO+1))
            DO K=1,LEVSO+1
              AK(K) = VCOORDO(K,1)
              BK(K) = VCOORDO(K,2)
              CK(K) = VCOORDO(K,3)
            ENDDO
            CALL CHECKDP(IJL,IJX,LEVSO,AK,BK,CK,PSO,TO,QO)
            DEALLOCATE (AK, BK, CK)
          ENDIF

!----force tracers to be positvie
          IF (CHGQ0 == 1) QO = MAX(QO, 0.0)

          DO J=J1,J2
            JN = J
            JS = LATBO+1-J
            DO I=1,LONBO
              GFSDATAO%ZS(I,JN) = ZSO(I,2*(J-J1)+1)
              GFSDATAO%ZS(I,JS) = ZSO(I,2*(J-J1)+2)
              GFSDATAO%PS(I,JN) = PSO(I,2*(J-J1)+1)
              GFSDATAO%PS(I,JS) = PSO(I,2*(J-J1)+2)
            ENDDO
            IF (NOPDPVV) THEN
              DO K=1,LEVSO
                DO I=1,LONBO
                  GFSDATAO%T(I,JN,K)  = TO(I,2*(J-J1)+1,K)
                  GFSDATAO%T(I,JS,K)  = TO(I,2*(J-J1)+2,K)
                  GFSDATAO%U(I,JN,K)  = UO(I,2*(J-J1)+1,K)
                  GFSDATAO%U(I,JS,K)  = UO(I,2*(J-J1)+2,K)
                  GFSDATAO%V(I,JN,K)  = VO(I,2*(J-J1)+1,K)
                  GFSDATAO%V(I,JS,K)  = VO(I,2*(J-J1)+2,K)
                ENDDO
              ENDDO
            ELSE
              DO K=1,LEVSO
                DO I=1,LONBO
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
            ENDIF
 
            DO N=1,NTRACM
              DO K=1,LEVSO
                DO I=1,LONBO
                  GFSDATAO%Q(I,JN,K,N) = QO(I,2*(J-J1)+1,K,N)
                  GFSDATAO%Q(I,JS,K,N) = QO(I,2*(J-J1)+2,K,N)
                ENDDO
              ENDDO
            ENDDO
          ENDDO

        ENDDO LATLOOP1

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  DEALLOCATE TEMPORARY DATA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        DEALLOCATE(ZSI, PSI, PI, TI, UI, VI, WI, QI)
        DEALLOCATE(ZSO, PSO, PO, TO, UO, VO, WO, QO)
        DEALLOCATE(TPO, DTDPO, DPO)
        DEALLOCATE(LPL3, RLAT)
        IF (ALLOCATED(SUMQ)) DEALLOCATE(SUMQ)
        IF (ALLOCATED(XCP))  DEALLOCATE(XCP)
        IF (ALLOCATED(VIRT)) DEALLOCATE(VIRT)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GENERATE SPECIAL SETS OF TRACERS
        IF(SIGHEADO%IDVT > 0 .AND. MOD(SIGHEADO%IDVT,100) == 0) THEN
          PRINT*,'FATAL ERROR: USE OF SPECIAL TRACER SETS NOT YET IMPLEMENTED'
          PRINT*,'FOR FV3. STOP.'
          CALL ERREXIT(11)
!fv3  The specsets routine passes back tracers in spectral space.  Routine
!fv3  should be modified to pass back tracers on gaussian or fv3 grid. 
!fv3      CALL SPECSETS(SIGHEADO,SIGDATAO,IDRT)
        ENDIF
 
        CALL SIGIO_AXDBTA(SIGDATAI,IRET)
 
        CALL SIGIO_SCLOSE(NSIGI,IRET)

        IF (REGIONAL < 2) THEN
          CALL WRITE_FV3_ATMS_NETCDF(GFSDATAO%ZS,GFSDATAO%PS,GFSDATAO%T,GFSDATAO%W,   &
                 GFSDATAO%U,GFSDATAO%V,GFSDATAO%Q,VCOORDO,        &
                 LONBO,LATBO,LEVSO,NTRACM,NVCOORDO,NTILES,HALO)  
        ENDIF

        IF (REGIONAL >= 1) THEN
          CALL WRITE_FV3_ATMS_BNDY_NETCDF(GFSDATAO%ZS,GFSDATAO%PS,GFSDATAO%T,   &
                   GFSDATAO%W,GFSDATAO%U,GFSDATAO%V,GFSDATAO%Q,VCOORDO,         &
                   LONBO,LATBO,LEVSO,NTRACM,NVCOORDO,HALO)
        ENDIF

        CALL NEMSIO_GFS_AXGRD(GFSDATAO)

        DEALLOCATE (VCOORDO)
        DEALLOCATE (TRAC_NAME)

! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!  CHANGE RESOLUTION OF INPUT NEMSIO GRID FILE
!  OUTPUT ATMOS FILE ON FV3 CUBED-SPHERE GRID WILL BE NETCDF FORMAT.
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      ELSEIF(INPTYP == 1) THEN

        PRINT*, 'CHGRES INPUT:  GFS GAUSSIAN NEMSIO GRID FILE '
        PRINT*, 'CHGRES OUTPUT: FV3 NETCDF FILE'

        CALL NEMSIO_OPEN(GFILEI,'chgres.inp.sig','read',IRET=IRET)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OPEN (READ) NEMSIO GRID FILE HEADERS
        CALL NEMSIO_GETFILEHEAD(GFILEI, IDATE=GFSHEADI%IDATE,       &
                                NFHOUR=GFSHEADI%NFHOUR,             &
                                NFMINUTE=GFSHEADI%NFMINUTE,         &
                                NFSECONDN=GFSHEADI%NFSECONDN,       &
                                NFSECONDD=GFSHEADI%NFSECONDD,       &
                                VERSION=GFSHEADI%VERSION,           &
                                NREC=GFSHEADI%NREC,                 &
                                DIMX=GFSHEADI%DIMX,                 &
                                DIMY=GFSHEADI%DIMY,                 &
                                DIMZ=GFSHEADI%DIMZ,                 &
                                JCAP=GFSHEADI%JCAP,                 &
                                NTRAC=GFSHEADI%NTRAC,               &
                                NCLDT=GFSHEADI%NCLDT,               &
                                NSOIL=GFSHEADI%NSOIL,               &
                                IDSL=GFSHEADI%IDSL,                 &
                                IDVC=GFSHEADI%IDVC,                 &
                                IDVM=GFSHEADI%IDVM,                 &
                                IDRT=GFSHEADI%IDRT, IRET=IRET0)
 
        CALL NEMSIO_GETHEADVAR(GFILEI,'FHOUR', GFSHEADI%FHOUR,IRET=IRET)

        IF(IRET/=0) GFSHEADI%FHOUR = REAL(GFSHEADI%NFHOUR,8) +     &
           REAL(GFSHEADI%NFMINUTE,8)/60. +                         &
           REAL(GFSHEADI%NFSECONDN,8)/(3600.*GFSHEADI%NFSECONDD)
 
        CALL NEMSIO_GETHEADVAR(GFILEI,'DIMY', GFSHEADI%LATB,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'DIMX', GFSHEADI%LONB,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'LEVS', GFSHEADI%LEVS,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'ITRUN', GFSHEADI%ITRUN,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IORDER', GFSHEADI%IORDER,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IREALF', GFSHEADI%IREALF, IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'ICEN2', GFSHEADI%ICEN2,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IDVT', GFSHEADI%IDVT,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'PDRYINI', GFSHEADI%PDRYINI,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IVS', GFSHEADI%IVSSIG,IRET=IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'NVCOORD', GFSHEADI%NVCOORD,IRET=IRET)

        PRINT*,''
        PRINT*,"INPUT NEMSIO FILE SPECS:"
        WRITE(6,155) GFSHEADI%IDATE
 155    FORMAT("  DATE:      ",7(I6))
        PRINT*," FHOUR:     ",GFSHEADI%FHOUR
        PRINT*," VERSION:   ",GFSHEADI%VERSION
        PRINT*," NREC:      ",GFSHEADI%NREC
        PRINT*," JCAP:      ",GFSHEADI%JCAP
        PRINT*," NTRAC:     ",GFSHEADI%NTRAC
        PRINT*," NCLDT:     ",GFSHEADI%NCLDT
        PRINT*," IDSL:      ",GFSHEADI%IDSL
        PRINT*," IDVC:      ",GFSHEADI%IDVC
        PRINT*," IDVM:      ",GFSHEADI%IDVM
        PRINT*," IDRT:      ",GFSHEADI%IDRT
        PRINT*," LONB:      ",GFSHEADI%LONB
        PRINT*," LATB:      ",GFSHEADI%LATB
        PRINT*," LEVS:      ",GFSHEADI%LEVS
        PRINT*," ITRUN:     ",GFSHEADI%ITRUN
        PRINT*," IORDER:    ",GFSHEADI%IORDER
        PRINT*," IREALF:    ",GFSHEADI%IREALF
        PRINT*," ICEN2:     ",GFSHEADI%ICEN2
        PRINT*," IDVT:      ",GFSHEADI%IDVT
        PRINT*," PDRYINI:   ",GFSHEADI%PDRYINI
        PRINT*," IVSSIG:    ",GFSHEADI%IVSSIG
        PRINT*," NVCOORD:   ",GFSHEADI%NVCOORD
        
        LEVSI = GFSHEADI%DIMZ
        LONB   = GFSHEADI%DIMX
        LATB   = GFSHEADI%DIMY

        ALLOCATE(GFSHEADVI%VCOORD(LEVSI+1,3,2))
        ALLOCATE(GFSHEADVI%CPI(GFSHEADI%NTRAC+1))
        ALLOCATE(GFSHEADVI%RI(GFSHEADI%NTRAC+1))

        CALL NEMSIO_GETFILEHEAD(GFILEI, VCOORD=GFSHEADVI%VCOORD,    &
                                CPI=GFSHEADVI%CPI, RI=GFSHEADVI%RI, &
                                IRET=IRET1)

        IF(IRET1.NE.0 ) THEN
          PRINT*, 'FATAL ERROR READNG NEMSIO FILE HEADER. IRET: ', IRET1
          CALL ERREXIT(25)
        ENDIF

        IF (GFSHEADI%NVCOORD == -9999) THEN
          GFSHEADI%NVCOORD = 3
          IF (MAXVAL(GFSHEADVI%VCOORD(:,3,1)) == 0. .AND.   &
              MINVAL(GFSHEADVI%VCOORD(:,3,1)) == 0. ) THEN
            GFSHEADI%NVCOORD = 2
!       jw for hyb: when no idsl is set
            IF (GFSHEADI%IDSL == -9999)GFSHEADI%IDSL = 1
            IF (MAXVAL(GFSHEADVI%VCOORD(:,2,1)) == 0. .AND. &
                MINVAL(GFSHEADVI%VCOORD(:,2,1)) ==0.) THEN
              GFSHEADI%NVCOORD = 1
            ENDIF 
          ENDIF 
        ENDIF

        ALLOCATE(VCOORDI(LEVSI+1,GFSHEADI%NVCOORD))
        VCOORDI(:,:) = GFSHEADVI%VCOORD(:,:,1)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ INPUT NEMSIO DATA ARRAY

        ALLOCATE(GFSDATAI%ZS(LONB,LATB))
        ALLOCATE(GFSDATAI%PS(LONB,LATB))
        ALLOCATE(GFSDATAI%T(LONB,LATB,LEVSI))
        ALLOCATE(GFSDATAI%U(LONB,LATB,LEVSI))
        ALLOCATE(GFSDATAI%V(LONB,LATB,LEVSI))
        ALLOCATE(GFSDATAI%Q(LONB,LATB,LEVSI,GFSHEADI%NTRAC))
        IF (.NOT. NOPDPVV) THEN
          ALLOCATE(GFSDATAI%P(LONB,LATB,LEVSI))
          ALLOCATE(GFSDATAI%DP(LONB,LATB,LEVSI))
          ALLOCATE(GFSDATAI%W(LONB,LATB,LEVSI))
        ENDIF 

        CALL NEMSIO_GFS_RDGRD(GFILEI,GFSDATAI,IRET=IRET)

        CALL NEMSIO_CLOSE(GFILEI,IRET=IRET)

        IF (LEVS > 0) THEN
          LEVSO = LEVS
        ELSE
          LEVSO = LEVSI
        ENDIF

        NTRACO = GFSHEADI%NTRAC

        IF(IDVT > 0)THEN
          IDVTO = IDVT
        ELSE
          IDVTO = GFSHEADI%IDVT
        ENDIF

        IF (NTRACO == 3 .AND. IDVTO == 21) THEN
          PRINT*,'INPUT FILE TRACERS: SPFH, O3MR, CLWMR'
        ELSE
          PRINT*,'- FATAL ERROR: CHGRES ASSUMES NTRACO=3 AND IDVT=21'
          PRINT*,'- INPUT FILE VALUES ARE ',NTRACO,IDVT
          PRINT*,'- STOP.'
          CALL ERREXIT(27)
        ENDIF

        IF(IDVC  >  0) THEN
          IDVCO = IDVC
        ELSE
          IDVCO = GFSHEADI%IDVC
        ENDIF

        IF(IDVM  >  0) THEN
          IDVMO = IDVM
        ELSE
          IDVMO = GFSHEADI%IDVM
        ENDIF

        IF(IDSL  >  0) THEN
          IDSLO = IDSL
        ELSE
          IDSLO = GFSHEADI%IDSL
        ENDIF

        IF(NVCOORD > 0) THEN
          NVCOORDO = NVCOORD
        ELSE
          NVCOORDO = GFSHEADI%NVCOORD
        ENDIF

        IF(MQUICK == 1) THEN
          IF(LEVSO.NE.LEVSI)    CALL ERREXIT(28)
        ENDIF
 
!  GET NEW SIGMA LEVELS

        IF(NSIL /= 0) THEN
          ALLOCATE(VCOORDO(LEVSO+1,NVCOORDO))
          CALL NEWSIG(NSIL, IDVCO, LEVSO, NVCOORDO, VCOORDO, IOSSIL)
          IF(IOSSIL == 0) THEN
            PRINT '("  NEW MODEL LEVELS READ IN")'
          ENDIF
        ELSEIF(IDVCO == GFSHEADI%IDVC .AND.  &
               LEVSO == LEVSI .AND.          &
               NVCOORDO == GFSHEADI%NVCOORD) THEN
          ALLOCATE(VCOORDO(LEVSO+1,NVCOORDO))
          VCOORDO(:,:) = GFSHEADVI%VCOORD(:,1:NVCOORDO,1)
          IOSSIL = 0
          PRINT '("  NEW MODEL LEVELS COPIED FROM OLD")'
        ELSE
          IOSSIL=42
        ENDIF

        IF(IOSSIL.NE.0) THEN
          PRINT*,"FATAL ERROR DEFINING SIGMA VALUES.  IOSSIL: ", IOSSIL
          CALL ERREXIT(81)
        ENDIF

        PRINT*,''
        PRINT*,"TRANSFORM TO A CUBED-SPHERE GRID OF:"
        PRINT*," LEVELS:  ",LEVSO
        PRINT*," NTRAC:   ",NTRACO
        PRINT*," IDVC:    ",IDVCO
        PRINT*," NVCOORD: ",NVCOORDO
  
        IF (NOPDPVV) THEN
          NREC = 2 + LEVSO*(3+NTRACO)      !zs,ps,t,u,v,q(ntracer)
        ELSE
          NREC = 2 + LEVSO*(6+NTRACO)      !zs,ps,p,dp,t,u,v,q(ntracer),vvel
        ENDIF

        IF (NTRACO == GFSHEADI%NTRAC) THEN
          CPI(0:NTRACO) = GFSHEADVI%CPI(1:NTRACO+1)
          RI(0:NTRACO)  = GFSHEADVI%RI(1:NTRACO+1)
        ELSE
          PRINT *,'FATAL ERROR: You have different Tracers from input,',  &
          ' make sure to provide CPI & RI, for generalized coordinate.'
          PRINT*,' Stop program.'
          CALL ERREXIT(24)
        ENDIF

        MQUICKNEMS : IF (MQUICK == 0) THEN
 
          ALLOCATE(ZSI(LONB,LATCH2), PSI(LONB,LATCH2))
          ALLOCATE(TI(LONB,LATCH2,LEVSI), TIV(LONB,LATCH2,LEVSI),   &
                   UI(LONB,LATCH2,LEVSI), VI(LONB,LATCH2,LEVSI),    &
                   PI(LONB,LATCH2,LEVSI), WI(LONB,LATCH2,LEVSI),    &
                   QI(LONB,LATCH2,LEVSI,NTRACO))

          ALLOCATE(GFSDATAO%ZS(LONB,LATB))
          ALLOCATE(GFSDATAO%PS(LONB,LATB))
          ALLOCATE(GFSDATAO%T(LONB,LATB,LEVSO))
          ALLOCATE(GFSDATAO%U(LONB,LATB,LEVSO))
          ALLOCATE(GFSDATAO%V(LONB,LATB,LEVSO))
          ALLOCATE(GFSDATAO%Q(LONB,LATB,LEVSO,NTRACO))

          IF (.NOT. NOPDPVV) THEN
            ALLOCATE(GFSDATAO%P(LONB,LATB,LEVSO))
            ALLOCATE(GFSDATAO%DP(LONB,LATB,LEVSO))
            ALLOCATE(GFSDATAO%W(LONB,LATB,LEVSO))
          ELSE
            ALLOCATE(PO(LONB,LATCH2,LEVSO))
            ALLOCATE(DPO(LONB,LATCH2,LEVSO))
            ALLOCATE(WO(LONB,LATCH2,LEVSO))
          ENDIF

          ALLOCATE(DTDPO(LONB,LATCH2,LEVSO))

          LATLOOP : DO J1=1,LATB,LATCH2
 
            J2  = MIN(J1+LATCH2-1, LATB)
            JL  = J2-J1+1
            IJL = LONB*JL
            IJX = LONB*JL
 
            ZSI(:,:) = GFSDATAI%ZS(:,J1:J2)
            PSI(:,:) = GFSDATAI%PS(:,J1:J2)

            IF (.NOT. NOPDPVV) THEN
              PI(:,:,:) = GFSDATAI%P(:,J1:J2,:)
            ELSE
              DO L = 1, LEVSI
                DO J = 1, JL
                  DO I = 1, LONB
                    PI(I,J,L) = VCOORDI(L,1) + VCOORDI(L,2)*PSI(i,j)
                  ENDDO
                ENDDO
              ENDDO
            ENDIF

            TI(:,:,:) = GFSDATAI%T(:,J1:J2,:)
            UI(:,:,:) = GFSDATAI%U(:,J1:J2,:)
            VI(:,:,:) = GFSDATAI%V(:,J1:J2,:)

            DO N=1,NTRACO
              QI(:,:,:,N) = GFSDATAI%Q(:,J1:J2,:,N)
            ENDDO

            IF ( NREC == GFSHEADI%NREC .AND. .NOT. NOPDPVV ) THEN
              WI(:,:,:) = GFSDATAI%W(:,J1:J2,:)
            ELSE
              WI = 0.
            ENDIF

            TIV = TI*(1.+(461.50/287.05-1)*QI(:,:,:,1))  ! virtual temperature

            GFSDATAO%ZS(:,J1:J2) = ZSI

            CALL SIGIO_MODPRD(IJL,IJX,LEVSI,GFSHEADI%NVCOORD,   &
                              GFSHEADI%IDVC,GFSHEADI%IDSL,VCOORDI,IRET,  &
                              PS=PSI,T=TIV,PM=PI)

            TI = TIV / (1.+(461.50/287.05-1)*QI(:,:,:,1))  ! virtual temperature
 
            IF(ANY(TI == 0)) THEN
              PRINT *,'WARNING: tmp=0,j1=',j1,'j2=',j2,'tmp=',MAXVAL(TI),MINVAL(TI)
            ENDIF

            GFSDATAO%PS(:,J1:J2) = PSI

!-----------------------------------------------------------------------
!  VERTICALLY INTERPOLATE UPPER-AIR FIELDS
! -- Henry Juang's approach

            IF (NOPDPVV) THEN
              CALL NEWPR1(IJL, IJX, LEVSO, LEVSI, IDVCO, IDVMO, IDSLO, &
                          NVCOORDO, VCOORDO, RI, CPI, NTRACO,          &
                          PI, TI, QI, GFSDATAO%PS(:,J1:J2), PO, DPO)

              CALL VINTG(IJL,IJX,LEVSI,LEVSO,NTRACO,PI,UI,VI,TI,QI,WI, &
                         PO, GFSDATAO%U(:,J1:J2,:),  GFSDATAO%V(:,J1:J2,:), &
                         GFSDATAO%T(:,J1:J2,:), GFSDATAO%Q(:,J1:J2,:,:), DTDPO, WO)

            ELSE
              CALL NEWPR1(IJL, IJX, LEVSO, LEVSI, IDVCO, IDVMO, IDSLO,     &
                          NVCOORDO, VCOORDO, RI, CPI, NTRACO, PI, TI, QI,  &
                          GFSDATAO%PS(:,J1:J2), GFSDATAO%P(:,J1:J2,:),     &
                          GFSDATAO%DP(:,J1:J2,:))

              CALL VINTG(IJL,IJX,LEVSI,LEVSO,NTRACO,PI,UI,VI,TI,QI,WI,     &
                         GFSDATAO%P(:,J1:J2,:),GFSDATAO%U(:,J1:J2,:),      &
                         GFSDATAO%V(:,J1:J2,:),GFSDATAO%T(:,J1:J2,:),      &
                         GFSDATAO%Q(:,J1:J2,:,:),DTDPO,                    &
                         GFSDATAO%W(:,J1:J2,:))
            ENDIF
 
            IF (IDVCO  == 3) THEN
              ALLOCATE(AK(LEVSO+1), BK(LEVSO+1), CK(LEVSO+1))

              AK = VCOORDO(1:(LEVSO+1),1)
              BK = VCOORDO(1:(LEVSO+1),2)
              CK = VCOORDO(1:(LEVSO+1),3)

              CALL CHECKDP(IJL,IJX,LEVSO,AK,BK,CK,GFSDATAO%PS(:,J1:J2),    &
                           GFSDATAO%T(:,J1:J2,:),GFSDATAO%Q(:,J1:J2,:,:))

              DEALLOCATE(AK,BK,CK)
            ENDIF

          ENDDO LATLOOP

          DEALLOCATE(ZSI, PSI, PI, TI, TIV, UI, VI, WI, QI, DTDPO)

          IF (ALLOCATED(PO))  DEALLOCATE(PO)
          IF (ALLOCATED(DPO)) DEALLOCATE(DPO)
          IF (ALLOCATED(WO))  DEALLOCATE(WO)
 
        ELSE    ! MQUICK /= 0

          ALLOCATE(GFSDATAO%ZS(LONB,LATB))
          ALLOCATE(GFSDATAO%PS(LONB,LATB))
          ALLOCATE(GFSDATAO%T(LONB,LATB,LEVSO))
          ALLOCATE(GFSDATAO%U(LONB,LATB,LEVSO))
          ALLOCATE(GFSDATAO%V(LONB,LATB,LEVSO))
          ALLOCATE(GFSDATAO%Q(LONB,LATB,LEVSO,NTRACO))

          GFSDATAO%ZS = GFSDATAI%ZS
          GFSDATAO%PS = GFSDATAI%PS
          GFSDATAO%U  = GFSDATAI%U
          GFSDATAO%V  = GFSDATAI%V
          GFSDATAO%T  = GFSDATAI%T
          GFSDATAO%Q  = GFSDATAI%Q

          IF (.NOT. NOPDPVV) THEN
            ALLOCATE(GFSDATAO%W(LONB,LATB,LEVSO))
            ALLOCATE(GFSDATAO%P(LONB,LATB,LEVSO))
            ALLOCATE(GFSDATAO%DP(LONB,LATB,LEVSO))
            GFSDATAO%DP = GFSDATAI%DP
            GFSDATAO%P  = GFSDATAI%P
            IF(NREC == GFSHEADI%NREC) THEN
              GFSDATAO%W = GFSDATAI%W
            ELSE
              GFSDATAO%W = 0.
            ENDIF
          ENDIF
 
        ENDIF MQUICKNEMS

        DEALLOCATE(VCOORDI)
 
        CALL NEMSIO_GFS_AXHEADV(GFSHEADVI)

        CALL NEMSIO_GFS_AXGRD(GFSDATAI)

        IF (.NOT. ALLOCATED(GFSDATAO%W)) THEN
          ALLOCATE(GFSDATAO%W(LONB,LATB,LEVSO))
          GFSDATAO%W = 0.0
        END IF

        IF (REGIONAL < 2) THEN
          CALL WRITE_FV3_ATMS_NETCDF(GFSDATAO%ZS,GFSDATAO%PS,GFSDATAO%T,GFSDATAO%W,  &
                 GFSDATAO%U,GFSDATAO%V,GFSDATAO%Q,VCOORDO,         &
                 LONB,LATB,LEVSO,NTRACO,NVCOORDO,NTILES,HALO)
        ENDIF

        IF (REGIONAL >=1) THEN
          CALL WRITE_FV3_ATMS_BNDY_NETCDF(GFSDATAO%ZS,GFSDATAO%PS,GFSDATAO%T,   &
                   GFSDATAO%W,GFSDATAO%U,GFSDATAO%V,GFSDATAO%Q,VCOORDO,         &
                   LONB,LATB,LEVSO,NTRACO,NVCOORDO,HALO)
        ENDIF

        DEALLOCATE(VCOORDO)

        CALL NEMSIO_GFS_AXGRD(GFSDATAO)
    
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ENDIF     !!!END OF INPTYP OPTIONS FOR ATMOS FILE
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

! ---------------------------------------------------------------------
!  CLOSE FILES
      IF(INPTYP /= 0) THEN
        IF(NSIL > 0)  CLOSE(NSIL)
      ENDIF

! ---------------------------------------------------------------------
!  END OF CHANGE RESOLUTION FOR ATMOSPHERIC FIELDS.
! ---------------------------------------------------------------------

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OPEN SURFACE FILES
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      NSFCI = 21
      NSFCO = 61
      CALL SFCIO_SROPEN(NSFCI,'chgres.inp.sfc',IRET)
      CALL SFCIO_SRHEAD(NSFCI,SFCHEADI,IRET1)
      IF(IRET == 0 .AND. IRET1 == 0) THEN
        INPTYP = 2
        CALL SFCIO_SCLOSE(NSFCI, IRET)
      ELSE
        CALL NEMSIO_OPEN(GFILEISFC,'chgres.inp.sfc','read',IRET=IRET)
        CALL NEMSIO_GETFILEHEAD(GFILEISFC,GTYPE=FILETYPE,IRET=IRET)
        PRINT *,'OPEN chgres.inp.sfc,iret=',IRET, 'gtype=',FILETYPE
        IF (TRIM(FILETYPE) == 'NEMSIO' .AND. IRET == 0) THEN
          INPTYP = 1
          CALL NEMSIO_CLOSE(GFILEISFC, IRET=IRET)
        ELSE
          INPTYP = 0
          NSFCO  = 0
        ENDIF
      ENDIF

      IF (NSFCO == 0) GOTO 80

      FILESZ=0
      DO_NSST=.FALSE.
      INQUIRE (FILE="./chgres.inp.nst", SIZE=FILESZ)
      IF (FILESZ > 0) DO_NSST=.TRUE.
      IF (DO_NSST .AND. NSFCO == 0) THEN
        PRINT*,'FATAL ERROR: WHEN CONVERTING AN NSST RESTART FILE,'
        PRINT*,'YOU MUST ALSO CONVERT A SURFACE RESTART FILE.'
        CALL ERREXIT(33)
      ENDIF

      IF(INPTYP==2) THEN

        CALL READ_GFS_SFC_HEADER_SFCIO (NSFCI,IMI,JMI,IVSI,LSOILI, &
                              FCSTHOUR,IDATE4O,KGDS_INPUT)

      ELSEIF(INPTYP==1) THEN

        CALL READ_GFS_SFC_HEADER_NEMSIO (IMI,JMI,IVSI,LSOILI,  &
                              FCSTHOUR,IDATE4O,KGDS_INPUT)

      ENDIF

      ALLOCATE (SFCINPUT%ALNSF(IMI,JMI),    SFCINPUT%ALNWF(IMI,JMI),    &
                SFCINPUT%ALVSF(IMI,JMI),    SFCINPUT%ALVWF(IMI,JMI),    &
                SFCINPUT%CANOPY_MC(IMI,JMI),SFCINPUT%GREENFRC(IMI,JMI), &
                SFCINPUT%FACSF(IMI,JMI),    SFCINPUT%FACWF(IMI,JMI),    &
                SFCINPUT%SKIN_TEMP(IMI,JMI),SFCINPUT%LSMASK(IMI,JMI),   &
                SFCINPUT%SEA_ICE_FLAG(IMI,JMI),                         &
                SFCINPUT%SNOW_LIQ_EQUIV(IMI,JMI),                       &
                SFCINPUT%Z0(IMI,JMI),       SFCINPUT%OROG(IMI,JMI),     &
                SFCINPUT%VEG_TYPE(IMI,JMI), SFCINPUT%SOIL_TYPE(IMI,JMI),&
                SFCINPUT%SOILM_TOT(IMI,JMI,LSOILI),                     &
                SFCINPUT%SOIL_TEMP(IMI,JMI,LSOILI) )

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  THE 200501 VERSION OF THE SURFACE FILE HAS ADDITIONAL FIELDS FOR
!  USE BY NOAH LSM AND NEW SEA ICE PHYSICS, WHILE OLDER VERSIONS
!  DO NOT.  IF THESE VARIABLES ARE NOT ALLOCATED, THE SURFACE CHGRES
!  CODE WILL NOT INTERPOLATE THEM.

      IF (IVSI >= 200501) THEN
        ALLOCATE (SFCINPUT%SEA_ICE_FRACT(IMI,JMI),   &
                  SFCINPUT%SEA_ICE_DEPTH(IMI,JMI),   &
                  SFCINPUT%MXSNOW_ALB(IMI,JMI),      &
                  SFCINPUT%SNOW_DEPTH(IMI,JMI),      &
                  SFCINPUT%SLOPE_TYPE(IMI,JMI),      &
                  SFCINPUT%GREENFRC_MAX(IMI,JMI),    &
                  SFCINPUT%GREENFRC_MIN(IMI,JMI),    &
                  SFCINPUT%SOILM_LIQ(IMI,JMI,LSOILI) )
      ENDIF

      ALLOCATE (F10MI(IMI,JMI), T2MI(IMI,JMI), Q2MI(IMI,JMI),      &
                UUSTARI(IMI,JMI), FFMMI(IMI,JMI), FFHHI(IMI,JMI),  & 
                SRFLAGI(IMI,JMI), TPRCPI(IMI,JMI) )

      IF(INPTYP==2) THEN
   
        CALL READ_GFS_SFC_DATA_SFCIO (NSFCI, IMI, JMI, SFCINPUT,       &
                            F10MI, T2MI, Q2MI, UUSTARI, FFMMI, FFHHI,  &
                            SRFLAGI, TPRCPI)

      ELSE

        CALL READ_GFS_SFC_DATA_NEMSIO (IMI, JMI, LSOILI, IVSI, SFCINPUT,  &
     &                       F10MI, T2MI, Q2MI, UUSTARI, FFMMI, FFHHI,    &
     &                       SRFLAGI, TPRCPI)

      ENDIF
      
      IVSO = 200509
      IF(IVSSFC>0) IVSO = IVSSFC

      LSOILO = LSOILI
      IF(LSOIL > 0) LSOILO = LSOIL
      IF(IVSO < 200501) LSOILO = 2
      IF(LSOILO /= 2 .AND. LSOILO /= 4) THEN
        PRINT*,"FATAL ERROR: NUMBER OF SOIL LAYERS MUST BE 2 OR 4."
        CALL ERREXIT(9)
      ENDIF

      CALL READ_FV3_GRID_DIMS_NETCDF(TILE_NUM,IMO,JMO)

      IMO_WITH_HALO = IMO
      JMO_WITH_HALO = JMO

      IF (HALO > 0) THEN
        IMO = IMO - (2*HALO)
        JMO = JMO - (2*HALO)
        PRINT*,"WILL REMOVE HALO."
        PRINT*,"FULL GRID DIMENSIONS: ", IMO_WITH_HALO, JMO_WITH_HALO
        PRINT*,"NO HALO DIMENSIONS  : ", IMO, JMO
      ENDIF

      IJMO = IMO * JMO

      PRINT '(" CHANGE SURFACE FILE RESOLUTION",           &
         " FROM ",I4," X ",I4," X ",I4,"   VERSION",I8)', &
           IMI,JMI,LSOILI,IVSI
      PRINT '("                              ",           &
         "   TO ",I4," X ",I4," X ",I4,"   VERSION",I8)', &
           IMO,JMO,LSOILO,IVSO

      ALLOCATE(TMPVAR(IMO_WITH_HALO,JMO_WITH_HALO))
      ALLOCATE(TMPLAT(IMO_WITH_HALO,JMO_WITH_HALO))
      ALLOCATE(TMPLON(IMO_WITH_HALO,JMO_WITH_HALO))

      ALLOCATE(GEOLAT(IMO,JMO))
      ALLOCATE(GEOLON(IMO,JMO))
      CALL READ_FV3_LATLON_NETCDF(TILE_NUM,IMO_WITH_HALO,JMO_WITH_HALO,TMPLON,TMPLAT)
      DO J = 1, JMO
      DO I = 1, IMO
        GEOLAT(I,J) = TMPLAT(I+HALO,J+HALO)
        GEOLON(I,J) = TMPLON(I+HALO,J+HALO)
      ENDDO
      ENDDO

      DEALLOCATE(TMPLAT, TMPLON)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  INTERPOLATE SOME SURFACE FIELDS THE OLD WAY.  THESE ARE FIELDS
!  THAT ARE EITHER DIAGNOSTIC OR DO NOT REQUIRE SPECIAL HANDLING
!  BY THE NEW SURFACE CHGRES LOGIC.   

      ALLOCATE (F10MO(IMO,JMO), T2MO(IMO,JMO), Q2MO(IMO,JMO),       &
                UUSTARO(IMO,JMO), FFMMO(IMO,JMO), FFHHO(IMO,JMO),   &
                TPRCPO(IMO,JMO), SRFLAGO(IMO,JMO) )

      CALL GL2ANY(0,1,F10MI,IMI,JMI,F10MO,IMO,JMO,GEOLON,GEOLAT)
      CALL GL2ANY(0,1,T2MI,IMI,JMI,T2MO,IMO,JMO,GEOLON,GEOLAT)
      CALL GL2ANY(0,1,Q2MI,IMI,JMI,Q2MO,IMO,JMO,GEOLON,GEOLAT)
      CALL GL2ANY(0,1,UUSTARI,IMI,JMI,UUSTARO,IMO,JMO,GEOLON,GEOLAT)
      CALL GL2ANY(0,1,FFMMI,IMI,JMI,FFMMO,IMO,JMO,GEOLON,GEOLAT)
      CALL GL2ANY(0,1,FFHHI,IMI,JMI,FFHHO,IMO,JMO,GEOLON,GEOLAT)
      CALL GL2ANY(0,1,TPRCPI,IMI,JMI,TPRCPO,IMO,JMO,GEOLON,GEOLAT)
      CALL GL2ANY(2,1,SRFLAGI,IMI,JMI,SRFLAGO,IMO,JMO,GEOLON,GEOLAT)

      DEALLOCATE (F10MI, T2MI, Q2MI, UUSTARI, FFMMI, FFHHI)
      DEALLOCATE (TPRCPI, SRFLAGI)

      ALLOCATE(SLMSKO(IMO,JMO))
      CALL READ_FV3_GRID_DATA_NETCDF('slmsk',TILE_NUM,IMO_WITH_HALO,JMO_WITH_HALO,TMPVAR)
      DO J = 1, JMO
      DO I = 1, IMO
        SLMSKO(I,J) = TMPVAR(I+HALO,J+HALO)
      ENDDO
      ENDDO
      ALLOCATE(SFCOUTPUT%LSMASK(IJMO))
      SFCOUTPUT%LSMASK = RESHAPE(SLMSKO, (/IJMO/))
      DEALLOCATE(SLMSKO)

      ALLOCATE(OROGO(IMO,JMO))
      CALL READ_FV3_GRID_DATA_NETCDF('orog_filt',TILE_NUM,IMO_WITH_HALO,JMO_WITH_HALO,TMPVAR)
      DO J = 1, JMO
      DO I = 1, IMO
        OROGO(I,J) = TMPVAR(I+HALO,J+HALO)
      ENDDO
      ENDDO
      ALLOCATE(SFCOUTPUT%OROG(IJMO))
      SFCOUTPUT%OROG = RESHAPE(OROGO, (/IJMO/))
      DEALLOCATE(OROGO)
 
      ALLOCATE(OROGO_UF(IMO,JMO))
      CALL READ_FV3_GRID_DATA_NETCDF('orog_raw',TILE_NUM,IMO_WITH_HALO,JMO_WITH_HALO,TMPVAR)
      DO J = 1, JMO
      DO I = 1, IMO
        OROGO_UF(I,J) = TMPVAR(I+HALO,J+HALO)
      ENDDO
      ENDDO

      DEALLOCATE(TMPVAR)

      ALLOCATE(SFCOUTPUT%LATS(IJMO))
      SFCOUTPUT%LATS = RESHAPE(GEOLAT, (/IJMO/))
      DEALLOCATE(GEOLAT)
      ALLOCATE(SFCOUTPUT%LONS(IJMO))
      SFCOUTPUT%LONS = RESHAPE(GEOLON, (/IJMO/))
      DEALLOCATE(GEOLON)

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
      IF (IVSO >= 200501) THEN
        ALLOCATE(SFCOUTPUT%SLOPE_TYPE(IJMO))
        ALLOCATE(SFCOUTPUT%SEA_ICE_FRACT(IJMO))
        ALLOCATE(SFCOUTPUT%SEA_ICE_DEPTH(IJMO))
        ALLOCATE(SFCOUTPUT%SOILM_LIQ(IJMO,LSOILO))
        ALLOCATE(SFCOUTPUT%SNOW_DEPTH(IJMO))
        ALLOCATE(SFCOUTPUT%MXSNOW_ALB(IJMO))
        ALLOCATE(SFCOUTPUT%GREENFRC_MAX(IJMO))
        ALLOCATE(SFCOUTPUT%GREENFRC_MIN(IJMO))
      END IF
      IF (IVSO >= 200509) then
        ALLOCATE (SFCOUTPUT%SEA_ICE_TEMP(IJMO))
      END IF

! the fv3 does not have a grib 1 gds.  so, there is no way to set the
! kgds array for the output grid.  ipolates only uses kgds for the 
! output grid when doing the budget interpolation.  so, for now,
! only bilinear and neighbor interpolation will be used.

      KGDS_OUTPUT = 0

      CALL SURFACE_CHGRES_DRIVER(IMO,JMO,IJMO,LSOILO,              &
                                 KGDS_OUTPUT,SFCOUTPUT,IMI,JMI,    &
                                 OROGO_UF,USE_UFO,NST_ANL,         &
                                 LSOILI, IDATE4O(1), IDATE4O(2),   &
                                 IDATE4O(3), IDATE4O(4), FCSTHOUR, &           
                                 KGDS_INPUT, SFCINPUT, IALB,       &
                                 ISOT, IVEGSRC, TILE_NUM, MERGE, IRET)

      IF (IRET /= 0) THEN
        PRINT*, "FATAL ERROR IN SURFACE CHGRES DRIVER. IRET: ", IRET
        CALL ERREXIT(34)
      END IF

      CALL SURFACE_CHGRES_AX2D(SFCINPUT)

      DEALLOCATE(OROGO_UF)

      IF (DO_NSST) THEN

        ALLOCATE(RLATS_OUTPUT(IJMO))
        RLATS_OUTPUT=SFCOUTPUT%LATS
        ALLOCATE(RLONS_OUTPUT(IJMO))
        RLONS_OUTPUT=SFCOUTPUT%LONS
        ALLOCATE(MASK_OUTPUT(IJMO))
        MASK_OUTPUT=SFCOUTPUT%LSMASK
        WHERE(SFCOUTPUT%SEA_ICE_FLAG==1) MASK_OUTPUT=2

        ALLOCATE(NSST_OUTPUT(IJMO,NUM_NSST_FIELDS))
        ALLOCATE(NSST_INPUT(IMI,JMI,NUM_NSST_FIELDS))
        ALLOCATE(MASK_INPUT(IMI,JMI))

        IF (INPTYP == 1) THEN
          CALL READ_GFS_NSST_DATA_NEMSIO (MASK_INPUT,NSST_INPUT,IMI,JMI, &
                   NUM_NSST_FIELDS,NSST_YEAR,NSST_MON,NSST_DAY,  &
                   NSST_HOUR,NSST_FHOUR)
        ELSEIF (INPTYP == 2) THEN
          CALL READ_GFS_NSST_DATA_NSTIO (IMI,JMI,NUM_NSST_FIELDS,       &
                                    NSST_INPUT, MASK_INPUT,NSST_YEAR,   &
                                    NSST_MON,NSST_DAY,NSST_HOUR,        &
                                    NSST_FHOUR)
        ENDIF

        PRINT*,"- CHANGE NSST FILE RESOLUTION FROM ",IMI, " X ",JMI
        PRINT*,"                                TO ",IMO, " X ",JMO

        CALL NSST_CHGRES(IMI, JMI, MASK_OUTPUT, SFCOUTPUT%SKIN_TEMP, &
                         IMO, IJMO, KGDS_INPUT, NSST_INPUT, MASK_INPUT, &
                         NSST_OUTPUT, NUM_NSST_FIELDS, &
                         KGDS_OUTPUT, RLATS_OUTPUT, RLONS_OUTPUT)

        DEALLOCATE(RLATS_OUTPUT,RLONS_OUTPUT)
        DEALLOCATE(NSST_INPUT,MASK_INPUT,MASK_OUTPUT)

        CALL WRITE_FV3_SFC_DATA_NETCDF(IMO,JMO,LSOILO,SFCOUTPUT,F10MO, &
                           T2MO,Q2MO,UUSTARO,FFMMO,FFHHO,TPRCPO, &
                           SRFLAGO,TILE_NUM,NUM_NSST_FIELDS,NSST_OUTPUT)

        DEALLOCATE(NSST_OUTPUT)

      ELSE   ! output surface data only.

        CALL WRITE_FV3_SFC_DATA_NETCDF(IMO,JMO,LSOILO,SFCOUTPUT,F10MO, &
                           T2MO,Q2MO,UUSTARO,FFMMO,FFHHO,TPRCPO, &
                           SRFLAGO,TILE_NUM,NUM_NSST_FIELDS)


      ENDIF  ! process nsst file

      DEALLOCATE(F10MO, T2MO, Q2MO, UUSTARO, FFMMO, FFHHO)
      DEALLOCATE(TPRCPO, SRFLAGO)
      CALL SURFACE_CHGRES_AX1D(SFCOUTPUT)

 80   CONTINUE

      IF(NSIGO==0 .AND. NSFCO==0) THEN
        PRINT *,'- NO INPUT ATMOS OR SURFACE FILE SPECIFIED'
      ENDIF

      CALL W3TAGE('GLOBAL_CHGRES')
      END PROGRAM CHGRES
