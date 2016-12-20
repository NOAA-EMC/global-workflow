C-----------------------------------------------------------------------
      PROGRAM CHGRES
CC$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: GLOBAL_CHGRES
C   PRGMMR: IREDELL          ORG: NP23        DATE: 1999-09-10
C
C ABSTRACT: THIS PROGRAM CHANGES THE RESOLUTION OF THE SIGMA, SURFACE
C   AND NSST RESTART FILES FROM THE GLOBAL SPECTRAL MODEL.  THE INPUT FILES
C   SHOULD HAVE HEADER RECORDS IDENTIFYING THEIR RESPECTIVE RESOLUTIONS.
C   THE OUTPUT FILES RESOLUTION ARE SPECIFIED IN THE NAMELIST NAMCHG.
C   EITHER THE INPUT SIGMA OR SURFACE FILE MAY BE MISSING, IN WHICH
C   CASE NO COUNTERPART FILE IS CREATED WITH THE NEW RESOLUTION.
C
C   THE PROCEDURE FOR CHANGING THE SIGMA FILE RESOLUTION IS THUS.
C   A NEW OROGRAPHY IS OPTIONALLY READ IN.  IF IT IS MISSING,
C   THE NEW OROGRAPHY WILL BE THE TRANSFORM OF THE OLD OROGRAPHY.
C   A NEW SIGMA STRUCTURE IS ALSO READ IN.  THIS FILE IS ONLY
C   OPTIONAL IF THE NUMBER OF LEVELS ARE THE SAME, IN WHICH CASE
C   THE NEW SIGMA STRUCTURE DEFAULTS TO THE OLD SIGMA STRUCTURE.
C   THEN, THE INPUT SPECTRAL FIELDS ARE READ IN AND TRANSFORMED
C   TO THE NEW GAUSSIAN GRID.  A NEW SURFACE PRESSURE IS CALCULATED
C   HYDROSTATICALLY BASED ON THE NEW OROGRAPHY.  THEN THE UPPER-AIR
C   FIELDS ARE VERTICALLY INTERPOLATED TO THE INFERRED NEW PRESSURES.
C   THE VERTICAL INTERPOLATION IS GENERALLY CUBIC LAGRANGIAN IN LOG
C   PRESSURE WITH A MONOTONIC CONDITION THAT A NEW VALUE CANNOT EXCEED
C   THE RANGE OF ITS IMMEDIATE OLD NEIGHBORS.  INTERPOLATION IS LINEAR
C   BETWEEN THE TWO OUTER INTERVALS OF THE OLD DOMAIN.  FIELDS ARE HELD
C   CONSTANT OUTSIDE THE OLD DOMAIN, EXCEPT FOR TEMPERATURE AND HUMIDITY
C   BELOW THE OLD DOMAIN, WHERE THE TEMPERATURE LAPSE RATE IS HELD
C   FIXED AT -6.5 K/KM AND THE RELATIVE HUMIDITY IS ALSO HELD FIXED.
C   FINALLY, ALL FIELDS ARE TRANSFORMED TO THE NEW SPECTRAL SPACE
C   AND WRITTEN OUT.  NOTE THAT ALL TRACERS ARE INTERPOLATED UNLESS
C   REQUESTED OTHERWISE.  ALTERNATIVELY, IF NO TRANSFORMS ARE NEEDED,
C   THEN NO NEW OROGRAPHY OR SIGMA STRUCTURE IS READ IN AND THE
C   SPECTRAL COEFFICIENTS ARE DIRECTLY PADDED OR TRUNCATED.
C   FURTHERMORE, IF OZONE IS REQUESTED IN THE OUTPUT FILE
C   BUT IS NOT IN THE INPUT FILE, THEN OZONE IS GENERATED
C   FROM CLIMATOLOGY AND, OPTIONALLY, A TOTAL OZONE GRIB FIELD.
C   THE LAST RECORD PRECIPITATION IS ALSO INTERPOLATED IF REQUESTED.
C
C   THE PROCEDURE FOR CHANGING THE SURFACE FILE RESOLUTION IS THUS:
C   NEAREST NEIGHBOR INTERPOLATION IS PERFORMED SO THAT LAND/NONLAND
C   POINTS ON THE INPUT GRID ARE MAPPED TO LAND/NONLAND POINTS
C   ON THE TARGET GRID.  IF THE INPUT FILE CONTAINS LANDICE
C   AND THE OUTPUT GRID IS TO HAVE LANDICE, THEN NONLAND IS
C   MAPPED TO NONLAND, LANDICE IS MAPPED TO LANDICE, ICE FREE
C   LAND IS MAPPED TO ICE FREE LAND. OPTIONALLY, THE CLIMO FIELDS
C   SUCH AS ALBEDO, ROUGHNESS, ETC, MAY DETERMINED ON THE OUTPUT 
C   GRID FROM SFCCYCLE (WHICH IS CALLED FROM THE SURFACE
C   CHGRES MODULE).  THE LATTER IS RECOMMENDED WHEN CONVERTING
C   FROM A LOW TO HIGH RESOLUTION GRID.  A NEW LAND-SEA MASK IS
C   OPTIONALLY READ IN. IF IT IS MISSING, THE NEW LAND-SEA MASK IS
C   INTERPOLATED FROM THE OLD MASK.  SKIN AND SOIL TEMPERATURE OVER
C   LAND ARE ADJUSTED FOR DIFFERENCES BETWEEN THE INPUT AND OUTPUT
C   OROGRAPHY. LIQ SOIL MOISTURE IS CALCULATED ACCORDING TO THE
C   ADJUSTED TEMP. OUTPUT OROGRAPHY MAY BE READ IN FROM FILE OR INTERPOLATED
C   FROM INPUT OROGRAPHY.  NOTE: OLDER VERSIONS OF THE SURFACE
C   RESTART FILE (BEFORE IVS 200501) DO NOT HAVE OROGRAPHY RECORDS.
C   IN CASES WHERE THE INPUT SURFACE FILE IS PRE 200501,
C   THE PROGRAM WILL GET THE OROGRAPHY FROM THE SIGMA FILE.  
C   THEREFORE, YOU MUST SET THE OPTIONS TO CONVERT A SIGMA FILE
C   AS WELL AS A SURFACE FILE.  WHEN CHANGING A PRE 200501 FILE,
C   THE PROGRAM WILL INTERPOLATE ONLY THOSE LAND FIELDS NEEDED 
C   TO RUN THE OLD OSU LAND MODEL AND OLD SEA ICE PHYSICS.
C   WHEN CHANGING A 200501 FILE, THE PROGRAM WILL INTERPOLATE/CALC
C   THOSE ADDITIONAL FIELDS NEEDED BY THE NOAH LSM (MAX SNOW ALB,
C   LIQ. SOIL MOIST, SNOW DEPTH, PRECIP, PRECIP TYPE, SLOPE TYPE,
C   MAX/MIN GREENNESS) AND THE NEW SEA ICE MODEL (ICE DEPTH AND
C   FRACTION).  WHEN CHANGING A PRE 200501 FILE TO A 200501 FILE,
C   THE PROGRAM WILL AUTOMATICALLY INITIALIZE THE ABOVE 
C   MENTIONED FIELDS USING EITHER GUESS VALUES OR VALUES
C   CALCULATED FROM SFCCYCLE.  THE PROGRAM WILL ALSO CONVERT FROM TWO 
C   TO FOUR SOIL LAYERS AND VICE VERSA.  THE PROGRAM WILL RUN
C   ON THE FULL OR REDUCED GRID DEPENDING ON THE LONSPERLAT
C   RECORD OF THE INPUT FILE OR WHETHER THE USER SPECIFIES
C   AN EXTERNAL LONSPERLAT FILE.  THE PROGRAM WILL INITIALIZE
C   ALL LAND STATES FOR THE LANDICE PHYSICS IF DESIRED.  THE PROGRAM
C   WILL SCALE TOTAL SOIL MOISTURE FOR ANY DIFFERENCES IN SOIL
C   TYPE BETWEEN THE INPUT AND OUTPUT GRIDS.  CONTACT G. GAYNO
C   WITH QUESTIONS.
C
C PROGRAM HISTORY LOG:
C   98-04-03  IREDELL
!   2007      Juang, Moorthi, Gayno, F Yang, D Pan
C   2008      Moorthi
C   2011      Added capability to work with nsst files
C NAMELISTS:
C   NAMCHG:
C     JCAP       INTEGER NEW SPECTRAL RESOLUTION (DEFAULT: NO CHANGE)
C     LEVS       INTEGER NEW NUMBER OF LEVELS (DEFAULT: NO CHANGE)
C     NTRAC      INTEGER NEW NUMBER OF TRACERS (DEFAULT: NO CHANGE)
C     LONB       INTEGER NEW NUMBER OF PHYSICS LONGITUDES
C                (DEFAULT: INFERRED)
C     LATB       INTEGER NEW NUMBER OF PHYSICS LATITUDES
C                (DEFAULT: INFERRED)
C     LONBI      INTEGER OLD NUMBER OF PHYSICS LONGITUDES
C                (DEFAULT: INFERRED FROM INPUT SURFACE OR SIGMA FILE)
C     LATBI      INTEGER OLD NUMBER OF PHYSICS LATITUDES
C                (DEFAULT: INFERRED FROM INPUT SURFACE OR SIGMA FILE)
C     IDVC       INTEGER NEW VERTICAL COORDINATE ID
C                (1 FOR SIGMA, 2 FOR HYBRID, 3 GENERALL HYBRID) (DEFAULT: NO CHANGE)
C     IDVM       INTEGER NEW VERTICAL MASS VARIABLE ID
C                (1 FOR LN(PS) AND 2 FOR PS (KPA)) (DEFAULT: NO CHANGE)
C     IDSL       INTEGER NEW TYPE OF SIGMA STRUCTURE
C                (1 FOR PHILLIPS OR 2 FOR MEAN) (DEFAULT: NO CHANGE)
C     IGEN       INTEGER NEW GENERATING CODE (DEFAULT: NO CHANGE)
C     MGG        INTEGER NUMBER OF PRECIP FIELDS TO COPY (DEFAULT: 0)
C     MQUICK     INTEGER FLAG TO SKIP TRANSFORMS (DEFAULT: 0)
C     IDVT       INTEGER NEW TRACER VARIABLE ID (DEFAULT: NO CHANGE)
C     NCLDT      INTEGER NEW NUMBER OF CLOUDS (DEFAULT: NO CHANGE)
C     LATCH      INTEGER NUMBER OF LATITUDES TO PROCESS AT ONE TIME
C                (DEFAULT: 8)
C     LSOIL      INTEGER NEW NUMBER OF SOIL LAYERS (DEFAULT: NO CHANGE)
C     IVSSFC     INTEGER NEW VERSION NUMBER SFC FILE (DEFAULT: NO CHANGE)
C     IVSSIG     INTEGER NEW VERSION NUMBER SIGMA FIILE (DEFAULT: NO CHANGE)
C     NVCOORD    INTEGER NEW NUMBER OF VERTICAL COORDINATES
C                (DEFAULT: NO CHANGE)
C     IDRT       GFSIO OUTPUT OPTION: 4--GAUSSIAN (default); 0-- LATLON
C     OUTTYP     INTEGER NUMBER OF OUTPUT TYPE: 
C                1 GFSIO GRID; 2 SIGIO SPECTRAL (default); 0 BOTH SIGIO & GFSIO
C     CHGQ0      SET NEGATIVE VALUES OF TRACERS TO ZERO in CHGRES, 0 FALSE; 1 TRUE.
C
C INPUT FILES:
C   UNIT   11    chgres.inp.sig        OLD SIGMA FILE (IN EITHER SIGIO OR GFSIO FORMAT)
C   UNIT   12    chgres.inp.orogb      NEW GRIB OROGRAPHY
C   UNIT   13    chgres.inp.siglevel   NEW VERTICAL STRUCTURE
C   UNIT   14    chgres.inp.o3clim     OZONE CLIMATOLOGY
C   UNIT   15    chgres.inp.o3tgb      NEW GRIB TOTAL OZONE
C   UNIT   16    chgres.inp.orogb_uf   NEW GRIB OROGRAPHY (unfiltered)
C   UNIT   21    chgres.inp.sfc        OLD SURFACE FILE
C   UNIT   22    chgres.inp.slmgb      NEW GRIB LAND-SEA MASK
C   UNIT   23    chgres.inp.lonsperlat NEW NUMBER OF LONS PER LAT
C
C OUTPUT FILES:
C   UNIT   51    chgres.out.sig        NEW HISTROY FILE IN SPECTRAL COEFFICIENTS (SIGIO)
C   UNIT         chgres.out.grd        NEW HISTROY FILE IN GAUSSIAN GRID (GFSIO)
C   UNIT   61    chgres.out.sfc        NEW SURFACE FILE
C
C SUBPROGRAMS CALLED:
C   NCPUS        GET ENVIRONMENT NUMBER OF PARALLEL PROCESSES
C   RDSGH        READ A SIGMA FILE HEADER
C   CHGRES1      CHANGE RESOLUTION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C REMARKS:
C  VALID VALUES OF IDVT
C  ALL UNITS ARE SPECIFIC IN KG/KG
C  IDVT  NTRAC 
C     0      2    VAPOR,OZONE,CLOUD (DEFAULT OPERATIONAL)
C     1      1    VAPOR,OZONE
C     2      1    VAPOR,CLOUD
C    21      2    VAPOR,OZONE,CLOUD
C    12      2    VAPOR,CLOUD,OZONE
C   100     20    SET 1: VAPOR,OZONE,CLOUD, AND INITIAL VALUES OF
C                 CLAT*CLON,CLAT*SLON,SLAT,
C                 V*SLON-U*SLAT*CLON,-V*CLON-U*SLAT*SLON,U*CLAT
C                 ONE,K,SIGMA,PS,PRES,TEMP,ENTROPY,MOIST ENTROPY
C                 VAPOR,OZONE,CLOUD
!   200       5   IDEA tracer set with O and O2 in addition q, O3, clw
C
CC$$$
      USE SIGIO_MODULE
      USE NSTIO_MODULE
      USE GFSIO_MODULE
      USE GFSIO_RST
      USE SFCIO_MODULE
      USE FUNCPHYS
      USE SURFACE_CHGRES
      USE READ_WRITE_UTILS, ONLY   : INTERPRED,
     &                               UNINTERPRED
      IMPLICIT NONE

      include "netcdf.inc"

      INTEGER:: JCAP=0,LEVS=0,NTRAC=0,LONB=0,LATB=0,LONBI=0,LATBI=0,
     &          IDVC=0,IDVM=0,IDSL=0,IGEN=0,MGG=0,MQUICK=0,IDVT=0,
     &          NCLDT=0,LATCH=8,LSOIL=0,IVSSFC=0,IVSSIG=0,NVCOORD=0,
     &          IDRT=4,OUTTYP=2,IALB=0,CHGQ0=0
      REAL RI(0:20),CPI(0:20)
      logical            :: make_sfc=.true.
      INTEGER            :: ntiles = 6
      character(len=256) :: OUTGRID = ""
      character(len=256) :: OUTOROG = ""
      LOGICAL use_ufo
      NAMELIST/NAMCHG/ JCAP,LEVS,NTRAC,LONB,LATB,LONBI,LATBI,
     &                 IDVC,IDVM,IDSL,IGEN,MGG,MQUICK,IDVT,NCLDT,LATCH,
     &                 LSOIL,IVSSFC,IVSSIG,NVCOORD,OUTTYP,IDRT,RI,CPI,
     &                 IALB,CHGQ0,use_ufo,make_sfc,ntiles,OUTGRID,
     &                 OUTOROG
      INTEGER NSIGI,NORO,NSIL,NO3C,NO3T,NSIGO,noro_uf
      INTEGER IRET,IOSORO,IOSSIL,ITER,IRET1,IOSORO_uf
      integer ipi,ipj,ipk, ij
      INTEGER NCI,NCO,IMI,JMI,IMO,JMO,IJX,NTRACM,J1,J2,JL,IJL,J,JN,JS,N
     &,       NTRACO, II
      INTEGER :: IJMO, KGDS_INPUT(200), KGDS_OUTPUT(200)
      integer :: IJMI
      real, allocatable :: lons_input(:), lats_input(:)
      REAL, ALLOCATABLE :: MASK_OUTPUT(:) ,
     &        DUMMY(:), DUMMY2(:,:)
      real,                 allocatable :: tmpvar(:,:)
      real,             allocatable :: geolon_in(:,:), geolat_in(:,:)
      real,          allocatable :: geolon_e(:,:), geolat_e(:,:)
      real,          allocatable :: geolon_n(:,:), geolat_n(:,:)
      real(sfcio_realkind), allocatable :: geolon(:,:), geolat(:,:)
      
      real(sfcio_realkind), allocatable :: orog_raw(:,:)
      REAL, ALLOCATABLE :: RLATS_OUTPUT(:), RLONS_OUTPUT(:)
      real, allocatable :: nsst_output_thin(:,:)
      integer, parameter :: num_nsst_fields=18
      TYPE(SFC2D) SFCINPUT
      TYPE(SFC1D) SFCOUTPUT
      TYPE(NSTIO_HEAD):: NSST_IN_HEAD, NSST_OUT_HEAD
      TYPE(NSTIO_DATA):: NSST_IN_DATA, NSST_OUT_DATA
      INTEGER :: VVEL_PRECISION 
      INTEGER, ALLOCATABLE :: KMSK(:,:)
      INTEGER LSI,LSO
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200),KF,K
      INTEGER IDAT(8),JDAT(8)
      REAL RINC(5)
      LOGICAL*1,ALLOCATABLE:: BITMAP(:)
      LOGICAL, PARAMETER    :: MERGE=.FALSE.
      LOGICAL :: DO_NSST
      REAL,ALLOCATABLE:: SLAT(:),WLAT(:),CLAT(:),RLAT(:)
      REAL,ALLOCATABLE:: HSI(:),OROGI(:,:),OROGO(:,:), orogo_uf(:,:)
     &,                  orogo_uf2(:,:)
      REAL,ALLOCATABLE:: ZSI(:,:),PSI(:,:),PI(:,:,:),
     &                   TI(:,:,:),UI(:,:,:),VI(:,:,:),QI(:,:,:,:),
     &                   WI(:,:,:),TIV(:,:,:),
     &                   XCP(:,:,:),VIRT(:,:,:),SUMQ(:,:,:)
      REAL,ALLOCATABLE:: ZSO(:,:),PSO(:,:),PO(:,:,:),
     &                   TO(:,:,:),UO(:,:,:),VO(:,:,:),QO(:,:,:,:),
     &                   WO(:,:,:)
      REAL,ALLOCATABLE:: TPO(:,:),DPDTO(:,:),DTDPO(:,:)
!     REAL,ALLOCATABLE:: PKI(:,:),TKI(:,:),UKI(:,:),VKI(:,:),QKI(:,:,:)
!     REAL,ALLOCATABLE:: PKO(:,:),TKO(:,:),UKO(:,:),VKO(:,:),QKO(:,:,:)
      REAL,ALLOCATABLE:: DPO(:,:,:)
      TYPE(SIGIO_HEAD):: SIGHEADI,SIGHEADO
      TYPE(SIGIO_DBTA):: SIGDATAI,SIGDATAO
      INTEGER NSFCI,NSLM,NLPL,NSFCO,NSSTI,NSSTO,INPTYP
      INTEGER IOLPL,LATG2,IOSLM
      integer sfcpress_id_i, thermodyn_id_i
      integer sfcpress_id_o, thermodyn_id_o
      TYPE(SFCIO_HEAD):: SFCHEADI,SFCHEADO
      TYPE(SFCIO_DBTA):: SFCDATAI,SFCDATAO
      REAL,ALLOCATABLE:: SLMSKI(:,:),SLMSKO(:,:), SLMSKO2(:,:)

      integer,      allocatable :: GRID_LPL(:)
      REAL, allocatable :: GRIDLONS(:),GRIDLATS(:)
      REAL(KIND=4), allocatable :: GRID_ZS(:,:),GRID_PS(:,:)
      REAL(KIND=4), allocatable :: GRID_P(:,:,:),GRID_DP(:,:,:)
      REAL(KIND=4), allocatable :: GRID_T(:,:,:),GRID_U(:,:,:)
      REAL(KIND=4), allocatable :: GRID_V(:,:,:),GRID_W(:,:,:)
      REAL(KIND=4), allocatable :: GRID_Q(:,:,:,:), GRID_ZH(:,:,:)
      REAL(KIND=4), allocatable :: GRID_RH(:,:,:)
      REAL(KIND=4), allocatable :: GRID_U_E(:,:,:),GRID_V_E(:,:,:)
      REAL(KIND=4), allocatable :: GRID_U_N(:,:,:),GRID_V_N(:,:,:)
      real(kind=4), allocatable :: zh_in_r4(:,:,:)
      real(kind=4), allocatable :: rh_in_r4(:,:,:)
      REAL,         allocatable :: tmp_in(:,:), tmp_out(:,:)
      real,         allocatable :: zh_in(:,:,:), ZS_in(:,:), ps_in(:,:)
      real,         allocatable :: rh_in(:,:,:), P_in(:,:,:)
      real,         allocatable :: SPHUM_in(:,:,:), T_in(:,:,:)
      character(len=256) :: tilefile, out_file
      integer :: l
       integer kall
       real ttot,tmin,tmax,fcsthour
       real (kind=8) timef            
       real timeomega,timestart,timeend
!cggg
       real, allocatable :: ak(:), bk(:), ck(:), vcoord(:,:)
       real(gfsiokind), allocatable :: tmp(:)
       real, allocatable :: sl_r8(:), ak_r8(:)
! Define variables for GFSIO
      TYPE(GFSIO_GFILE) :: GFILEI             
      TYPE(GFSIO_GFILE) :: GFILEO             
      TYPE(GFSIO_HEAD)  :: GFSHEADI, GFSHEADO
      TYPE(GFSIO_HEADV) :: GFSHEADVI, GFSHEADVO
      TYPE(GFSIO_DATA)  :: GFSDATAI, GFSDATAO
      INTEGER :: NREC, LEVSI, LEVSO, JREC, I, idim_gfsio, jdim_gfsio
      CHARACTER(8) :: FILETYPE                 
      integer      :: fsize=65536
      integer      :: error, ncid, id_dim, id_var, nx, ny
      logical      :: fexist     

      integer :: LONG, LATG  ! grid size from grid file
      character(len=128) :: outfile
      integer            :: dim_lon, dim_lat, id_slmsk
      integer            :: header_buffer_val = 16384
      integer            :: inital = 0
      logical            :: outorog_exist, read_grid
      integer            :: ncid_orog
      integer            :: i3,j3
      logical            :: opened
! .....................................................................
! .....................................................................
      ri  = 0.0
      cpi = 0.0
      use_ufo = .false.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ NAMELIST
      call instrument(30,kall,ttot,tmin,tmax)
      CALL W3TAGB('GLOBAL_CHGRES',1999,0253,0056,'NP23')
      CALL GFUNCPHYS
      READ(*,NAMCHG)
!      READ(22, NAMCHG)
!      OUTGRID ="C48_grid"
!      OUTGRID ="/archive/z1l/GFS_data/C48/grid/C48_grid"
       
      write(6,namchg)

! --- read the grid resolution if the OUTGRID exists.
      if( trim(OUTGRID) .NE. "none" .and. trim(OUTOROG) .NE. "" ) then
         inquire(file=trim(OUTGRID), exist=fexist)
         if(.not. fexist) then
            print*, "file "//trim(OUTGRID)//" does not exist"
            CALL ERREXIT(4)
         endif
         do ncid = 103, 512
           inquire( ncid,OPENED=opened )
           if( .NOT.opened )exit
         end do

         print*, "outgrid=", trim(outgrid)
         error=NF__OPEN(trim(OUTGRID),NF_NOWRITE,fsize,ncid)
         call netcdf_err(error, 'Open file '//trim(OUTGRID) )
         error=nf_inq_dimid(ncid, 'nx', id_dim)
         call netcdf_err(error, 'inquire dimension nx from file '//
     &                   trim(OUTGRID) )
         error=nf_inq_dimlen(ncid,id_dim,nx)
         call netcdf_err(error, 'inquire dimension nx length '//
     &       'from file '//trim(OUTGRID) )
         
         error=nf_inq_dimid(ncid, 'ny', id_dim)
         call netcdf_err(error, 'inquire dimension ny from file '//
     &                   trim(OUTGRID) )
         error=nf_inq_dimlen(ncid,id_dim,ny)
         call netcdf_err(error, 'inquire dimension ny length '//
     &       'from file '//trim(OUTGRID) )
         print*, "nx = ", nx
         if(LONB .ne. nx/2) then
            print*, "LONB=",LONB, " /= grid file nx/2=",nx/2
            print*, "Set IM = ", nx/2
            LONB = nx/2
         endif
         if(LATB .ne. ny/2) then
            print*, "LATB=",LATB, " /= grid file ny/2=",ny/2
            print*, "Set LTAB = ", ny/2
            LATB = ny/2
         endif
         error=nf_close(ncid)
         call netcdf_err(error, 'close file '//trim(OUTGRID) )
         
      endif         
      
      
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OPEN  FILES
      NSIGI    = 11
      NORO     = 12
      NSIL     = 13
      NO3C     = 14
      NO3T     = 15
      noro_uf  = 16
      NSIGO    = 51
      INPTYP   = 0

      call gfsio_init(iret)

      CALL SIGIO_SROPEN(NSIGI,'chgres.inp.sig',IRET)
      CALL SIGIO_SRHEAD(NSIGI,SIGHEADI,IRET1)
      IF(IRET.EQ.0 .AND. IRET1.EQ.0) THEN
        INPTYP=2
      ELSE
        CALL GFSIO_OPEN(GFILEI,'chgres.inp.sig','read',IRET=IRET)
        CALL GFSIO_GETFILEHEAD(GFILEI,GTYPE=FILETYPE)
        print *,'iret=',iret, 'gtype=',filetype
        IF (FILETYPE.EQ.'GFSIOATM' .AND. IRET.EQ.0) INPTYP=1
      ENDIF

      IF(INPTYP.NE.0) THEN
        CALL BAOPENR(NORO,'chgres.inp.orogb',IRET)
        IF(IRET.NE.0) NORO=0
        OPEN(NSIL,FILE='chgres.inp.siglevel',
     &       FORM='FORMATTED',STATUS='OLD',IOSTAT=IRET)
        IF(IRET.NE.0) NSIL=0
        OPEN(NO3C,FILE='chgres.inp.o3clim',
     &       FORM='FORMATTED',STATUS='OLD',IOSTAT=IRET)
        IF(IRET.NE.0) NO3C=0
        CALL BAOPENR(NO3T,'chgres.inp.o3tgb',IRET)
        IF(IRET.NE.0) NO3T=0
        CALL SIGIO_SWOPEN(NSIGO,'chgres.out.sig',IRET)
      ELSE
       PRINT*,'--- FAIL TO OPEN chgres.inp.sig ---'
       PRINT*,'--- PROCEED TO CHANGE chgres.inp.sfc ---'
      ENDIF
         call instrument(1,kall,ttot,tmin,tmax)
      
! ---------------------------------------------------------------------
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!  CHANGE RESOLUTION OF INPUT SIGIO SIGMA FILE
!  OUTPUT HISTORY FILE IN SIGIO SIGMA OR GFSIO GRID FORMAT, OR BOTH

!z1l: We only need to change INPTYP = 2
      if( .not. make_sfc ) THEN   
      IF(INPTYP.EQ.2) THEN
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
! ---------------------------------------------------------------------

        PRINT*, 'CHGRES INPUT:  SPECTRAL SIGIO SIGMA FILE '
        IF(OUTTYP .EQ. 1 )PRINT*, 'CHGRES OUTPUT: GFSIO GRID FILE'
        IF(OUTTYP .EQ. 2 )PRINT*, 'CHGRES OUTPUT: SIGIO SIGMA FILE'
        IF(OUTTYP .EQ. 0 )PRINT*, 
     &       'CHGRES OUTPUT: BOTH SIGIO SIGMA and GFSIO GRID FILES'

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OPEN SIGIO SIGMA FILE HEADER
!!      CALL SIGIO_SRHEAD(NSIGI,SIGHEADI,IRET)
          IF(LONBI.GT.0.AND.LATBI.GT.0) THEN
            SIGHEADI%LONB=LONBI
            SIGHEADI%LATB=LATBI
          ENDIF
          LEVSI=SIGHEADI%LEVS
          IMI=SIGHEADI%LONB
          JMI=SIGHEADI%LATB

          if (cpi(0) == 0.0) then
            if (mod(SIGHEADI%IDVM/10,10) == 3) then
              do n=1,sigheadi%ntrac+1
                cpi(n-1) = SIGHEADI%cpi(n)
                ri(n-1)  = SIGHEADI%ri(n)
              enddo
            endif
          endif

!         ----------------
          SIGHEADO=SIGHEADI
!         ----------------
          IF(JCAP.GT.0) SIGHEADO%JCAP=JCAP
          IF(LEVS.GT.0) SIGHEADO%LEVS=LEVS
          IF(LONB.GT.0.AND.LATB.GT.0) THEN
            SIGHEADO%LONB=LONB
            SIGHEADO%LATB=LATB
          ENDIF
          IF(NTRAC.GT.0) SIGHEADO%NTRAC=NTRAC
          IF (IDVT == 200) THEN
            SIGHEADO%NTRAC = MAX(SIGHEADI%NTRAC+2, SIGHEADO%NTRAC)
            IF (NTRAC > 0 .AND. SIGHEADO%NTRAC > NTRAC) THEN
              print *,' Incompatible values specified for NTRAC & IDVT'
              stop 1111
            ENDIF
          ENDIF
          IF(IDVC >  0)  SIGHEADO%IDVC  = IDVC
          IF(IDVM >= 0)  SIGHEADO%IDVM  = IDVM
          IF(IDSL >= 0)  SIGHEADO%IDSL  = IDSL
          IF(IGEN.GT.0)  SIGHEADO%IGEN  = IGEN
          IF(IDVT.GT.0)  SIGHEADO%IDVT  = IDVT
          IF(NCLDT.GT.0) SIGHEADO%NCLDT = NCLDT
          IF(NCLDT.EQ.0) NCLDT          = SIGHEADI%NCLDT
          IF(IVSSIG.GT.0) SIGHEADO%IVS  = IVSSIG
          IF(IDVC == 1)   SIGHEADO%NVCOORD = 1
          IF(NVCOORD.GT.0) SIGHEADO%NVCOORD=NVCOORD
          SIGHEADO%LONF=SIGHEADO%LONB
          SIGHEADO%LATF=SIGHEADO%LATB
          SIGHEADO%LONR=SIGHEADO%LONB
          SIGHEADO%LATR=SIGHEADO%LATB
          if (sigheado%jcap /= sigheadi%jcap) SIGHEADO%PDRYINI = 0
      print *,' pdryinia=',SIGHEADO%PDRYINI
          IF(MQUICK.EQ.1) THEN
            IF(SIGHEADO%LEVS.NE.SIGHEADI%LEVS) CALL ERREXIT(24)
            IF(SIGHEADO%JCAP.LT.SIGHEADI%JCAP) CALL ERREXIT(24)
          ENDIF
          CALL SIGIO_ALHEAD(SIGHEADO,IRET)
         if (mod(sigheado%idvm/10,10) == 3) then
           sigheado%cpi(1:sigheado%ntrac+1) = cpi(0:sigheado%ntrac)
           sigheado%ri(1:sigheado%ntrac+1)  = ri(0:sigheado%ntrac)
         endif

         if (mod(sigheado%idvm/10,10) == 3)
     &   print *,' cpi=',sigheado%cpi(1:sigheado%ntrac+1)
     &,' ri=',sigheado%ri(1:sigheado%ntrac+1)

        PRINT '("CHANGE SIGMA FILE RESOLUTION FROM ",
     &   I4," WAVES, ",I4," LEVELS, AND ",I4," TRACERS")',
     &   SIGHEADI%JCAP,SIGHEADI%LEVS,SIGHEADI%NTRAC
        PRINT '("                                  ",
     &   " IVS=",I6," IDVC=",I2," NVCOORD=",I2,"  IDVM=",I2)',
     &   SIGHEADI%IVS,SIGHEADI%IDVC,SIGHEADI%NVCOORD,SIGHEADI%IDVM
        PRINT '("                               TO ",
     &   I4," WAVES, ",I4," LEVELS, AND ",I4," TRACERS.")',
     &   SIGHEADO%JCAP,SIGHEADO%LEVS,SIGHEADO%NTRAC
        PRINT '("                                  ",
     &   " IVS=",I6," IDVC=",I2," NVCOORD=",I2,"  IDVM=",I2)',
     &   SIGHEADO%IVS,SIGHEADO%IDVC,SIGHEADO%NVCOORD,SIGHEADO%IDVM

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ OLD SIGMA FILE
        CALL SIGIO_ALDBTA(SIGHEADI,SIGDATAI,IRET)
        IF(IRET.NE.0) THEN
          PRINT '("  ERROR ALLOCATING ")'
          CALL ERREXIT(4)
        ENDIF
        CALL SIGIO_ALDBTA(SIGHEADO,SIGDATAO,IRET)
        IF(IRET.NE.0) THEN
          PRINT '("  ERROR ALLOCATING ")'
          CALL ERREXIT(4)
        ENDIF
        CALL SIGIO_SRDBTA(NSIGI,SIGHEADI,SIGDATAI,IRET)
        IF(IRET.NE.0) THEN
          PRINT '("  ERROR READING ")'
          CALL ERREXIT(4)
        ENDIF
        NCI    = SIZE(SIGDATAI%T,1)
        NCO    = SIZE(SIGDATAO%T,1)
        LEVSO  = SIGHEADO%LEVS
        IMO    = SIGHEADO%LONB
        JMO    = SIGHEADO%LATB
        IJX    = IMO*2*LATCH
        NTRACM = MIN(SIGHEADI%NTRAC,SIGHEADO%NTRAC)
        NTRACO = NTRACM
        IF (IDVT == 200) NTRACO = MIN(SIGHEADI%NTRAC,SIGHEADO%NTRAC) + 2

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        IF(MQUICK.EQ.0) THEN

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ALLOCATE DATA FOR GFSIO OUTPUT
          IF(OUTTYP.EQ.1 .OR. OUTTYP.EQ.0) THEN
            ALLOCATE(GFSDATAO%ZS(IMO,JMO))
            ALLOCATE(GFSDATAO%PS(IMO,JMO))
            ALLOCATE(GFSDATAO%P(IMO,JMO,LEVSO))
            ALLOCATE(GFSDATAO%DP(IMO,JMO,LEVSO))
            ALLOCATE(GFSDATAO%T(IMO,JMO,LEVSO))
            ALLOCATE(GFSDATAO%U(IMO,JMO,LEVSO))
            ALLOCATE(GFSDATAO%V(IMO,JMO,LEVSO))
            ALLOCATE(GFSDATAO%Q(IMO,JMO,LEVSO,NTRACO))
            ALLOCATE(GFSDATAO%W(IMO,JMO,LEVSO))
          ENDIF

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
          ALLOCATE(SLAT(JMO))
          ALLOCATE(WLAT(JMO))
          ALLOCATE(CLAT(JMO))
          ALLOCATE(RLAT(JMO))
          ALLOCATE(HSI(NCI))
          ALLOCATE(OROGI(IMO,JMO))
          ALLOCATE(OROGO(IMO,JMO))
          ALLOCATE(ZSI(IMO,2*LATCH))
          ALLOCATE(PSI(IMO,2*LATCH))
          ALLOCATE(PI(IMO,2*LATCH,LEVSI))
          ALLOCATE(TI(IMO,2*LATCH,LEVSI))
!         ALLOCATE(TIV(IMO,2*LATCH,LEVSI))
          ALLOCATE(UI(IMO,2*LATCH,LEVSI))
          ALLOCATE(VI(IMO,2*LATCH,LEVSI))
          ALLOCATE(QI(IMO,2*LATCH,LEVSI,NTRACM))
          ALLOCATE(SUMQ(IMO,2*LATCH,LEVSI))
          ALLOCATE(XCP (IMO,2*LATCH,LEVSI))
          ALLOCATE(VIRT(IMO,2*LATCH,LEVSI))
!
          ALLOCATE(WI(IMO,2*LATCH,LEVSI))
!
          ALLOCATE(ZSO(IMO,2*LATCH))
          ALLOCATE(PSO(IMO,2*LATCH))
          ALLOCATE(PO(IMO,2*LATCH,LEVSO))
          ALLOCATE(TO(IMO,2*LATCH,LEVSO))
          ALLOCATE(UO(IMO,2*LATCH,LEVSO))
          ALLOCATE(VO(IMO,2*LATCH,LEVSO))
          ALLOCATE(WO(IMO,2*LATCH,LEVSO))
          ALLOCATE(QO(IMO,2*LATCH,LEVSO,NTRACO))
          ALLOCATE(TPO(IMO,LEVSO))
          ALLOCATE(DPO(IMO,2*LATCH,LEVSO))
!         ALLOCATE(DPDTO(IMO,LEVSO))
          ALLOCATE(DTDPO(IMO*2*LATCH,LEVSO))
!
!         ALLOCATE(PKI(IMO,LEVSI))
!         ALLOCATE(TKI(IMO,LEVSI))
!         ALLOCATE(UKI(IMO,LEVSI))
!         ALLOCATE(VKI(IMO,LEVSI))
!         ALLOCATE(QKI(IMO,LEVSI,NTRACM))
!
!         ALLOCATE(DPO(IMO,2*LATCH,LEVSO))
!         ALLOCATE(PKO(IMO,LEVSO))
!         ALLOCATE(TKO(IMO,LEVSO))
!         ALLOCATE(UKO(IMO,LEVSO))
!         ALLOCATE(VKO(IMO,LEVSO))
!         ALLOCATE(QKO(IMO,LEVSO,NTRACO))
       call instrument(2,kall,ttot,tmin,tmax)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW LATITUDES
          CALL SPLAT(IDRT,JMO,SLAT,WLAT)
          CLAT=SQRT(1-SLAT**2)
          RLAT=180/ACOS(-1.)*ASIN(SLAT)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW OROGRAPHY
          JPDS    = -1
          JPDS(5) = 8
          IF(NORO.NE.0) THEN
            ALLOCATE(BITMAP(IMO*JMO))
            CALL GETGB(NORO,0,IMO*JMO,0,JPDS,JGDS,
     &                 KF,K,KPDS,KGDS,BITMAP,OROGO,IOSORO)
            DEALLOCATE(BITMAP)
          ELSE
            IOSORO=9
          ENDIF
          IF(IOSORO.EQ.0.AND.(KGDS(1).NE.IDRT.OR.
     &     KGDS(2).NE.IMO.OR.KGDS(3).NE.JMO)) IOSORO=100
          IF(IOSORO.EQ.0) THEN
            PRINT '("  NEW OROGRAPHY READ IN")'
            if (kgds(4) == -90000 .and. kgds(5) == -180000) then
              print *,' reversing the lat/lon for orography'
              call REVERS(imo, jmo, OROGO)
            endif
            SIGHEADO%PDRYINI = 0
          ELSE
            PRINT '("  NEW OROGRAPHY TRUNCATED FROM OLD")'
          ENDIF
!
       call instrument(3,kall,ttot,tmin,tmax)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW SIGMA LEVELS
          IF(NSIL.NE.0) THEN
!cggg
            allocate(vcoord(SIGHEADO%LEVS+1,SIGHEADO%NVCOORD))
            vcoord = SIGHEADO%VCOORD
            CALL NEWSIG(NSIL,SIGHEADO%IDVC,SIGHEADO%LEVS,
     &                  SIGHEADO%NVCOORD,VCOORD,IOSSIL)
            SIGHEADO%VCOORD = vcoord
            deallocate(vcoord)
!cggg     &                  SIGHEADO%NVCOORD,SIGHEADO%VCOORD,IOSSIL)
            IF(IOSSIL.EQ.0) THEN
              PRINT '("  NEW MODEL LEVELS READ IN")'
            ENDIF
          ELSEIF(SIGHEADO%IDVC.EQ.SIGHEADI%IDVC.AND.
     &           SIGHEADO%LEVS.EQ.SIGHEADI%LEVS.AND.
     &           SIGHEADO%NVCOORD.EQ.SIGHEADI%NVCOORD) THEN
            SIGHEADO%VCOORD=SIGHEADI%VCOORD
            IOSSIL=0
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
C ------------------------------------------------------------------
C  GET PS and T DATA TYPE FOR THE FILE
          sfcpress_id_i=mod(SIGHEADI%IDVM,10)
          thermodyn_id_i=mod(SIGHEADI%IDVM/10,10)
          sfcpress_id_o=mod(SIGHEADO%IDVM,10)
          thermodyn_id_o=mod(SIGHEADO%IDVM/10,10)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  INITIALIZE TEMPORARY OUTPUT DATA
          SIGDATAO%HS=0
          SIGDATAO%PS=0
          SIGDATAO%T=0
          SIGDATAO%D=0
          SIGDATAO%Z=0
          SIGDATAO%Q=0
          HSI=SIGDATAI%HS

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  LOOP OVER LATITUDE
          timeomega=0.
          DO J1=1,(JMO+1)/2,LATCH
            J2=MIN(J1+LATCH-1,(JMO+1)/2)
            JL=2*(J2-J1+1)
            IJL=IMO*JL
            CALL TRSSC(SIGHEADI%JCAP,NCI,SIGHEADI%LEVS,NTRACM,
     &                 SIGHEADI%IDVM,IDRT,IMO,JMO,IJX,J1,J2,1,
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
            IF(IOSORO.EQ.0) THEN
              DO J=J1,J2
                JN=J
                JS=JMO+1-J
                ZSO(:,2*(J-J1)+1) = OROGO(:,JN)
                ZSO(:,2*(J-J1)+2) = OROGO(:,JS)
                OROGI(:,JN)       = ZSI(:,2*(J-J1)+1)
                OROGI(:,JS)       = ZSI(:,2*(J-J1)+2)
              ENDDO
            ELSE
              ZSO(:,:JL) = ZSI(:,:JL)
              DO J=J1,J2
                JN=J
                JS=JMO+1-J
                OROGI(:,JN) = ZSI(:,2*(J-J1)+1)
                OROGI(:,JS) = ZSI(:,2*(J-J1)+2)
                OROGO(:,JN) = ZSO(:,2*(J-J1)+1)
                OROGO(:,JS) = ZSO(:,2*(J-J1)+2)
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
!cggg     &        SIGHEADI%NVCOORD,SIGHEADI%VCOORD,
     &        SIGHEADI%NVCOORD,VCOORD,
     &        IMO,JMO,IJL,IJX,J1,J2,1,SIGDATAI%D,SIGDATAI%PS,
     &        PSI,TI,UI,VI,WI)
             timeend=timef()
             timeomega=timeomega+timeend-timestart
!
            CALL SIGIO_MODPRD
     &       (IJL,IJX,SIGHEADI%LEVS,SIGHEADI%NVCOORD,
!cggg     &        SIGHEADI%IDVC,SIGHEADI%IDSL,SIGHEADI%VCOORD,IRET,
     &        SIGHEADI%IDVC,SIGHEADI%IDSL,VCOORD,IRET,
     &        PS=PSI,T=TI,PM=PI)
!cggg
            deallocate (vcoord)
!
            select case( thermodyn_id_i )
             case(0,1)
              TI(:,1:JL,:) = TI(:,1:JL,:)/virt(:,1:JL,:)         ! to t
             case(2)
              print *,' thermodyn_id_o=',thermodyn_id_o,' dry t '
             case(3)
              TI(:,1:JL,:) = TI(:,1:JL,:)/xcp(:,1:JL,:)*cpi(0)   ! to t
             case default
              print *,' unknow input thermodyn id =',thermodyn_id_i
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
!cggg
         allocate (vcoord(SIGHEADO%LEVS+1,SIGHEADO%NVCOORD))
         vcoord = SIGHEADO%VCOORD
            CALL NEWPR1(IJL,IJX,SIGHEADO%LEVS,SIGHEADI%LEVS,
     &                 SIGHEADO%IDVC,SIGHEADO%IDVM,SIGHEADO%IDSL,
     &                 SIGHEADO%NVCOORD, vcoord,
     &                 RI, CPI, NTRACM,
     &                 PI,TI,QI,PSO,PO,DPO)
         deallocate (vcoord)
!cggg            CALL NEWPR1(IJL,IJX,SIGHEADO%LEVS,SIGHEADI%LEVS,
!cggg     &                 SIGHEADO%IDVC,SIGHEADO%IDVM,SIGHEADO%IDSL,
!cggg     &                 SIGHEADO%NVCOORD,SIGHEADO%VCOORD,
!cggg     &                 RI, CPI, NTRACM,
!cggg     &                 PI,TI,QI,PSO,PO,DPO)
         call instrument(7,kall,ttot,tmin,tmax)
         CALL VINTG(IJL,IJX,SIGHEADI%LEVS,SIGHEADO%LEVS,NTRACM,
     &                 PI,UI,VI,TI,QI,WI,PO,UO,VO,TO,QO,DTDPO,WO)
!           call hhmaxmin(uo,IMO,2*LATCH,1,JL,SIGHEADI%LEVS,' ui 2a ' )
!           call hhmaxmin(vo,IMO,2*LATCH,1,JL,SIGHEADI%LEVS,' vi 2a ' )
!
c idea add init condition for temp tracer4-5 ( o o2)
            IF (IDVT == 200) then
              CALL  VINTG_IDEA(IMO,LATCH,SIGHEADO%LEVS,NTRACO,
     &          PO,RLAT,JMO,J1,J2,SIGHEADI%IDATE,UO,VO,TO,QO)
            ENDIF
!           print*,'www'
!           print'(12f5.3)',QO(1,1,1:SIGHEADO%LEVS,4)
!
            if( SIGHEADO%IDVC .eq. 3 ) then
!cggg
          allocate(ak(SIGHEADO%LEVS+1))
          ak = SIGHEADO%VCOORD(1:(SIGHEADO%LEVS+1),1)
          allocate(bk(SIGHEADO%LEVS+1))
          bk = SIGHEADO%VCOORD(1:(SIGHEADO%LEVS+1),2)
          allocate(ck(SIGHEADO%LEVS+1))
          ck = SIGHEADO%VCOORD(1:(SIGHEADO%LEVS+1),3)
          call checkdp(IJL,IJX,SIGHEADO%LEVS,ak,bk,ck,
     &                 PSO,TO,QO)
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
!  PASS DATA TO GFSIO
!           print*,' pass data on gaussian grid to gfsio array '      
            IF(OUTTYP.EQ.1 .OR. OUTTYP.EQ.0) THEN
             DO J=J1,J2
               JN=J
               JS=JMO+1-J
               DO I=1,IMO
                 GFSDATAO%ZS(I,JN) = ZSO(I,2*(J-J1)+1)
                 GFSDATAO%ZS(I,JS) = ZSO(I,2*(J-J1)+2)
                 GFSDATAO%PS(I,JN) = PSO(I,2*(J-J1)+1)
                 GFSDATAO%PS(I,JS) = PSO(I,2*(J-J1)+2)
               ENDDO
               DO K=1,SIGHEADO%LEVS
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
!idea?
               DO N=1,NTRACM
                 DO K=1,SIGHEADO%LEVS
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
            IF(OUTTYP.EQ.2 .OR. OUTTYP.EQ.0) THEN
!             print*,' transform back to the new spectral space '
              CALL TRBSC(SIGHEADO%JCAP,NCO,SIGHEADO%LEVS,NTRACO,
     &                 SIGHEADO%IDVM,IDRT,IMO,JMO,IJX,J1,J2,1,cpi,
     &                 ZSO,PSO,TO,UO,VO,QO,
     &                 SIGDATAO%HS,SIGDATAO%PS,SIGDATAO%T,
     &                 SIGDATAO%D,SIGDATAO%Z,SIGDATAO%Q)
            ENDIF
         call instrument(9,kall,ttot,tmin,tmax)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          ENDDO
! END OF LOOP OVER LATITUDE
          print *,'omega time=',timeomega
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  DEALLOCATE TEMPORARY DATA
          DEALLOCATE(ZSI)
          DEALLOCATE(PSI)
          DEALLOCATE(PI)
          DEALLOCATE(TI)
!         DEALLOCATE(TIV)
          DEALLOCATE(UI)
          DEALLOCATE(VI)
          DEALLOCATE(WI)
          DEALLOCATE(QI)
          DEALLOCATE(ZSO)
          DEALLOCATE(PSO)
          DEALLOCATE(PO)
          DEALLOCATE(TO)
          DEALLOCATE(UO)
          DEALLOCATE(VO)
          DEALLOCATE(QO)
          DEALLOCATE(WO)
          DEALLOCATE(TPO)
!         DEALLOCATE(DPDTO)
          DEALLOCATE(DTDPO)
          DEALLOCATE(DPO)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  USE PADDING OR TRUNCATION TO CHANGE RESOLUTION

        ELSE
          CALL PADSSC(SIGHEADI%JCAP,NCI,SIGHEADO%JCAP,NCO,
     &                SIGHEADO%LEVS,NTRACO,
     &                SIGDATAI%HS,SIGDATAI%PS,SIGDATAI%T,
     &                SIGDATAI%D,SIGDATAI%Z,SIGDATAI%Q,
     &                SIGDATAO%HS,SIGDATAO%PS,SIGDATAO%T,
     &                SIGDATAO%D,SIGDATAO%Z,SIGDATAO%Q)
        ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GENERATE NEW OZONE FROM CLIMATOLOGY IF NECESSARY
        IF(SIGHEADI%NTRAC.EQ.1.AND.SIGHEADO%NTRAC.GT.1.AND.
     &     SIGHEADO%IDVC.EQ.0.AND.MOD(SIGHEADO%IDVT/1,10).LE.1.AND.
     &     NO3C.NE.0) THEN
          IDAT=0
          IDAT(1)=SIGHEADO%IDATE(4)
          IDAT(2)=SIGHEADO%IDATE(2)
          IDAT(3)=SIGHEADO%IDATE(3)
          IDAT(5)=SIGHEADO%IDATE(1)
          RINC=0
          RINC(2)=SIGHEADO%FHOUR
          allocate(sl_r8(size(SIGHEADO%SL)))
          allocate(ak_r8(size(SIGHEADO%SL)))
          sl_r8 = SIGHEADO%SL
          ak_r8 = SIGHEADO%VCOORD(:,1)
          CALL W3MOVDAT(RINC,IDAT,JDAT)
          CALL SPECO3(NO3C,NO3T,SIGHEADO%JCAP,NCO,SIGHEADO%LEVS,
     &                SIGHEADO%LONB,SIGHEADO%LATB,1,JDAT,
     &                sl_r8,ak_r8,
     &                SIGDATAO%PS,SIGDATAO%Q(1,1,2))
          deallocate(sl_r8, ak_r8)
          PRINT '("  OZONE GENERATED FROM CLIMATOLOGY")'
        ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GENERATE SPECIAL SETS OF TRACERS
        IF(SIGHEADO%IDVT.GT.0.AND.MOD(SIGHEADO%IDVT,100).EQ.0) THEN
          CALL SPECSETS(SIGHEADO,SIGDATAO,IDRT)
        ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  WRITE NEW GFSIO FILE
        IF(OUTTYP.EQ.1 .OR. OUTTYP.EQ.0 .AND. MQUICK.EQ.0) THEN
!         print*, ' prepare gfsio header '  

       ! --- fill in GFSIO header data  ---
! idea ?
          NREC=2+SIGHEADO%LEVS*(6+NTRACM)     !zs,ps,p,dp,t,u,v,q(ntracer)
          LEVSO=SIGHEADO%LEVS
          ALLOCATE(GFSHEADVO%VCOORD(LEVSO+1,SIGHEADO%NVCOORD))
          ALLOCATE(GFSHEADVO%RECNAME(NREC))
          ALLOCATE(GFSHEADVO%RECLEVTYP(NREC))
          ALLOCATE(GFSHEADVO%RECLEV(NREC))
          ALLOCATE(GFSHEADVO%GLAT1D(JMO))
          ALLOCATE(GFSHEADVO%GLON1D(IMO))
          ALLOCATE(GFSHEADVO%CPI(NTRACM+1))
          ALLOCATE(GFSHEADVO%RI(NTRACM+1))

          GFSHEADVO%VCOORD   =SIGHEADO%VCOORD
          GFSHEADVO%RECNAME(1)='hgt'
          GFSHEADVO%RECNAME(2)='pres'
          GFSHEADVO%RECNAME(3:(2+LEVSO))='pres'
          GFSHEADVO%RECNAME((3+LEVSO):(2+2*LEVSO))='dpres'
          GFSHEADVO%RECNAME((3+2*LEVSO):(2+3*LEVSO))='tmp'
          GFSHEADVO%RECNAME((3+3*LEVSO):(2+4*LEVSO))='ugrd'
          GFSHEADVO%RECNAME((3+4*LEVSO):(2+5*LEVSO))='vgrd'
          GFSHEADVO%RECNAME((3+5*LEVSO):(2+6*LEVSO))='spfh'
          GFSHEADVO%RECNAME((3+6*LEVSO):(2+7*LEVSO))='o3mr'
          GFSHEADVO%RECNAME((3+7*LEVSO):(2+8*LEVSO))='clwmr'
          GFSHEADVO%RECNAME((3+8*LEVSO):(2+9*LEVSO))='vvel'
          GFSHEADVO%RECLEVTYP(1:2)='sfc'
          GFSHEADVO%RECLEVTYP(3:NREC)='layer'
          GFSHEADVO%RECLEV(1:2)=1      
          DO K=1,LEVSO
            GFSHEADVO%RECLEV(2+K)=K
            GFSHEADVO%RECLEV(2+LEVSO+K)=K
            GFSHEADVO%RECLEV(2+2*LEVSO+K)=K
            GFSHEADVO%RECLEV(2+3*LEVSO+K)=K
            GFSHEADVO%RECLEV(2+4*LEVSO+K)=K
            GFSHEADVO%RECLEV(2+5*LEVSO+K)=K
            GFSHEADVO%RECLEV(2+6*LEVSO+K)=K
            GFSHEADVO%RECLEV(2+7*LEVSO+K)=K
            GFSHEADVO%RECLEV(2+8*LEVSO+K)=K
          ENDDO
          if (mod(sigheado%idvm/10,10) == 3) then
            GFSHEADVO%CPI=SIGHEADO%CPI
            GFSHEADVO%RI=SIGHEADO%RI
          else
            GFSHEADVO%CPI=0.
            GFSHEADVO%RI=0.
          endif
         
          GFSHEADO%VERSION  =SIGHEADO%IVS
          GFSHEADO%FHOUR    =SIGHEADO%FHOUR
          GFSHEADO%IDATE    =SIGHEADO%IDATE   
          GFSHEADO%NREC     =NREC     
          GFSHEADO%LATB     =SIGHEADO%LATB
          GFSHEADO%LONB     =SIGHEADO%LONB
          GFSHEADO%LEVS     =SIGHEADO%LEVS
          GFSHEADO%JCAP     =SIGHEADO%JCAP
          GFSHEADO%ITRUN    =SIGHEADO%ITRUN
          GFSHEADO%IORDER   =SIGHEADO%IORDER
          GFSHEADO%IREALF   =SIGHEADO%IREALF
          GFSHEADO%IGEN     =SIGHEADO%IGEN  
          GFSHEADO%LATF     =SIGHEADO%LATF  
          GFSHEADO%LONF     =SIGHEADO%LONF  
          GFSHEADO%LATR     =SIGHEADO%LATR  
          GFSHEADO%LONR     =SIGHEADO%LONR  
          GFSHEADO%NTRAC    =SIGHEADO%NTRAC 
          GFSHEADO%ICEN2    =SIGHEADO%ICEN2
          GFSHEADO%IENS     =SIGHEADO%IENS   
          GFSHEADO%IDPP     =SIGHEADO%IDPP
          GFSHEADO%IDSL     =SIGHEADO%IDSL
          GFSHEADO%IDVC     =SIGHEADO%IDVC
          GFSHEADO%IDVM     =SIGHEADO%IDVM
          GFSHEADO%IDVT     =SIGHEADO%IDVT
          GFSHEADO%IDRUN    =SIGHEADO%IDRUN
          GFSHEADO%IDUSR    =SIGHEADO%IDUSR
          GFSHEADO%PDRYINI  =SIGHEADO%PDRYINI
          GFSHEADO%NCLDT    =SIGHEADO%NCLDT  
          GFSHEADO%IXGR     =SIGHEADO%IXGR   
          GFSHEADO%NVCOORD  =SIGHEADO%NVCOORD
          GFSHEADO%IDRT     =IDRT               
       ! --- end GFSIO header data  ---
          
!         print*, ' write out gfsio chgres.out.grd'
          CALL GFSIO_OPEN(GFILEO,TRIM('chgres.out.grd'),'write'
     &,        VERSION=GFSHEADO%VERSION  
     &,        FHOUR=GFSHEADO%FHOUR
     &,        IDATE=GFSHEADO%IDATE
     &,        NREC=GFSHEADO%NREC     
     &,        LATB=GFSHEADO%LATB    
     &,        LONB=GFSHEADO%LONB   
     &,        LEVS=GFSHEADO%LEVS  
     &,        JCAP=GFSHEADO%JCAP 
     &,        ITRUN=GFSHEADO%ITRUN    
     &,        IORDER=GFSHEADO%IORDER  
     &,        IREALF=GFSHEADO%IREALF 
     &,        IGEN=GFSHEADO%IGEN  
     &,        LATF=GFSHEADO%LATF 
     &,        LONF=GFSHEADO%LONF     
     &,        LATR=GFSHEADO%LATR    
     &,        LONR=GFSHEADO%LONR   
     &,        NTRAC=GFSHEADO%NTRAC    
     &,        ICEN2=GFSHEADO%ICEN2  
     &,        IENS=GFSHEADO%IENS
     &,        IDPP=GFSHEADO%IDPP     
     &,        IDSL=GFSHEADO%IDSL    
     &,        IDVC=GFSHEADO%IDVC   
     &,        IDVM=GFSHEADO%IDVM  
     &,        IDVT=GFSHEADO%IDVT 
     &,        IDRUN=GFSHEADO%IDRUN    
     &,        IDUSR=GFSHEADO%IDUSR  
     &,        PDRYINI=GFSHEADO%PDRYINI 
     &,        NCLDT=GFSHEADO%NCLDT  
     &,        IXGR=GFSHEADO%IXGR   
     &,        NVCOORD=GFSHEADO%NVCOORD 
     &,        IDRT=GFSHEADO%IDRT 
     &,        RECNAME=GFSHEADVO%RECNAME
     &,        RECLEVTYP=GFSHEADVO%RECLEVTYP
     &,        RECLEV=GFSHEADVO%RECLEV  
     &,        VCOORD=GFSHEADVO%VCOORD 
     &,        CPI=GFSHEADVO%CPI 
     &,        RI=GFSHEADVO%RI 
     &,        IRET=IRET)
          IF(IRET.NE.0) THEN
            PRINT*, ' ERROR AT GFSIO_OPEN chgres.out.grd '
            CALL ERREXIT(4)
          ENDIF

         print*, "size of GFSHEADVO%VCOORD", size(GFSHEADVO%VCOORD,1), 
     &          size(GFSHEADVO%VCOORD,2)
         print*, "size of GFSHEADVO%CPI", size(GFSHEADVO%CPI(:)), 
     &          size(GFSHEADVO%RI(:))
         CALL write_gfs_head("gfs_ctrl.nc",GFSHEADO%VERSION
     &,        GFSHEADO%FHOUR,GFSHEADO%IDATE
     &,        GFSHEADO%NREC,GFSHEADO%LATB
     &,        GFSHEADO%LONB,GFSHEADO%LEVS
     &,        GFSHEADO%JCAP,GFSHEADO%ITRUN
     &,        GFSHEADO%IORDER,GFSHEADO%IREALF
     &,        GFSHEADO%IGEN,GFSHEADO%LATF
     &,        GFSHEADO%LONF,GFSHEADO%LATR
     &,        GFSHEADO%LONR,GFSHEADO%NTRAC
     &,        GFSHEADO%ICEN2,GFSHEADO%IENS
     &,        GFSHEADO%IDPP,GFSHEADO%IDSL
     &,        GFSHEADO%IDVC,GFSHEADO%IDVM
     &,        GFSHEADO%IDVT,GFSHEADO%IDRUN
     &,        GFSHEADO%IDUSR,GFSHEADO%PDRYINI
     &,        GFSHEADO%NCLDT,GFSHEADO%IXGR
     &,        GFSHEADO%NVCOORD,GFSHEADO%IDRT
     &,        GFSHEADVO%RECNAME,GFSHEADVO%RECLEVTYP
     &,        GFSHEADVO%RECLEV,GFSHEADVO%VCOORD,NTRACM
     &,        GFSHEADVO%CPI,GFSHEADVO%RI )

        allocate(geolon(IMO,JMO), geolat(IMO,JMO))
        geolon=0; geolat=0

            allocate(ZH_in(IMO,JMO,LEVSO+1))
            allocate(ZH_in_r4(IMO,JMO,LEVSO+1))
            allocate(RH_in(IMO,JMO,LEVSO))
            allocate(RH_in_r4(IMO,JMO,LEVSO))
            allocate(P_in(IMO,JMO,LEVSO))
            allocate(ZS_in(IMO,JMO), PS_in(IMO,JMO) )
            allocate(SPHUM_in(IMO,JMO,LEVSO))
            allocate(T_in(IMO,JMO,LEVSO))
            zs_in = GFSDATAO%ZS
            ps_in = GFSDATAO%PS
            SPHUM_in = GFSDATAO%Q(:,:,LEVSO:1:-1,1)
            T_in = GFSDATAO%T(:,:,LEVSO:1:-1)
            P_in = GFSDATAO%P(:,:,LEVSO:1:-1)
            if(allocated(ak)) deallocate(ak)
            if(allocated(bk)) deallocate(bk)
            allocate(ak(LEVSO+1))
            allocate(bk(LEVSO+1))
            ak = GFSHEADVO%VCOORD(LEVSO+1:1:-1,1)
            bk = GFSHEADVO%VCOORD(LEVSO+1:1:-1,2)
            ak(1) = max(1.e-9, ak(1))
            ! compute zh, rh on Gaussian grid.
            call compute_zh_rh(IMO, JMO, LEVSO, ak, bk,
     &                      ps_in, zs_in, t_in, p_in, sphum_in, 
     &                      zh_in, rh_in )        
            deallocate(ps_in, zs_in, sphum_in, t_in, p_in)
            zh_in_r4(:,:,:) = zh_in(:,:,LEVSO+1:1:-1)
            rh_in_r4(:,:,:) = rh_in(:,:,LEVSO:1:-1)
            out_file = "gfs_data.nc"
            call write_gfs_data(IMO, JMO, LEVSO, NTRACM
     &,        GFSDATAO%ZS, GFSDATAO%PS, GFSDATAO%P, GFSDATAO%DP
     &,        GFSDATAO%T, GFSDATAO%U, GFSDATAO%V, GFSDATAO%W,zh_in_r4
     &,        rh_in_r4, GFSDATAO%Q, .false., geolon
     &,        geolat,"gfs_data.nc")

!!      deallocate(geolon, geolat)
        deallocate(geolon, geolat,zh_in_r4,rh_in_r4)
!---- if ntiles == 6, remap onto the cubic sphere grid.
        if(OUTGRID .NE. '') then
          !--- loop through each tile to read grid and remapping.
          do n = 1, ntiles
            if(ntiles>1) then
              write(tilefile, "(a,i1,a)") trim(OUTGRID)//".tile",n,".nc"
            else
              tilefile = trim(OUTGRID)
            endif
            inquire(file=trim(tilefile), exist=fexist)
            if(.not. fexist) then
              print*, "file "//trim(tilefile)//" does not exist"
              CALL ERREXIT(4)
            endif
            error=NF__OPEN(trim(tilefile),NF_NOWRITE,fsize,ncid)
            call netcdf_err(error, 'Open file '//trim(tilefile) )
            error=nf_inq_dimid(ncid, 'nx', id_dim)
            call netcdf_err(error, 'inquire dimension nx from file '//
     &                   trim(tilefile) )
            error=nf_inq_dimlen(ncid,id_dim,nx)
            call netcdf_err(error, 'inquire dimension nx length '//
     &           'from file '//trim(tilefile) )
            error=nf_inq_dimid(ncid, 'ny', id_dim)
            call netcdf_err(error, 'inquire dimension ny from file '//
     &                     trim(tilefile) )
            error=nf_inq_dimlen(ncid,id_dim,ny)
            call netcdf_err(error, 'inquire dimension ny length '//
     &           'from file '//trim(tilefile) )
            if( mod(nx,2) .NE. 0) then
              print*, "nx = ", nx, " is not a even number"
              CALL ERREXIT(4)
            endif
            if( mod(ny,2) .NE. 0) then
              print*, "ny = ", ny, " is not a even number"
              CALL ERREXIT(4)
            endif

            LONG = nx/2
            LATG = ny/2

            print*, "Read the grid from file "//trim(tilefile)

            allocate(geolon(LONG,LATG), geolat(LONG,LATG))
            allocate(geolon_e(LONG+1,LATG), geolat_e(LONG+1,LATG))
            allocate(geolon_n(LONG,LATG+1), geolat_n(LONG,LATG+1))
            allocate(tmpvar(nx+1,ny+1))

            error=nf_inq_varid(ncid, 'x', id_var)
            call netcdf_err(error, 'inquire varid of x from file '
     &                      //trim(tilefile) )
            error=nf_get_var_double(ncid, id_var, tmpvar)
            call netcdf_err(error, 'inquire data of x from file '
     &                     //trim(tilefile) )
            geolon(1:LONG,1:LATG) = tmpvar(2:nx:2,2:ny:2)
            geolon_e(1:LONG+1,1:LATG) = tmpvar(1:nx+1:2,2:ny:2)
            geolon_n(1:LONG,1:LATG+1) = tmpvar(2:nx:2,1:ny+1:2)
            error=nf_inq_varid(ncid, 'y', id_var)
            call netcdf_err(error, 'inquire varid of y from file '
     &                   //trim(tilefile) )
            error=nf_get_var_double(ncid, id_var, tmpvar)
            call netcdf_err(error, 'inquire data of y from file '
     &                   //trim(tilefile) )
            geolat(1:LONG,1:LATG) = tmpvar(2:nx:2,2:ny:2)
            geolat_e(1:LONG+1,1:LATG) = tmpvar(1:nx+1:2,2:ny:2)
            geolat_n(1:LONG,1:LATG+1) = tmpvar(2:nx:2,1:ny+1:2)
            
            deallocate(tmpvar)

            allocate(GRID_LPL(LATG/2))
            GRID_LPL(:) = LONG
            allocate(tmpvar(LONG, LATG))
            allocate(GRIDLONS(LONG*LATG))
            allocate(GRIDLATS(LONG*LATG))
            tmpvar = geolon
            CALL INTERPRED(1,KMSK,tmpvar,GRIDLONS,
     &               LONG,LATG,LONG*LATG,GRID_LPL)
            tmpvar = geolat
            CALL INTERPRED(1,KMSK,tmpvar,GRIDLATS,
     &               LONG,LATG,LONG*LATG,GRID_LPL)
            deallocate(tmpvar, GRID_LPL)

            allocate(GRID_ZS(LONG,LATG), GRID_PS(LONG,LATG))
            allocate(GRID_P(LONG,LATG,LEVSO), GRID_DP(LONG,LATG,LEVSO))
            allocate(GRID_T(LONG,LATG,LEVSO), GRID_U(LONG,LATG,LEVSO))
            allocate(GRID_V(LONG,LATG,LEVSO), GRID_W(LONG,LATG,LEVSO))
            allocate(GRID_Q(LONG,LATG,LEVSO,NTRACM))
            allocate(GRID_ZH(LONG,LATG,LEVSO+1))
            allocate(GRID_RH(LONG,LATG,LEVSO))
            allocate(tmp_in(IMO,JMO), tmp_out(LONG,LATG))
            allocate(GRID_U_E(LONG+1,LATG,LEVSO))
            allocate(GRID_V_E(LONG+1,LATG,LEVSO))
            allocate(GRID_U_N(LONG,LATG+1,LEVSO))
            allocate(GRID_V_N(LONG,LATG+1,LEVSO))
            
              tmp_in = GFSDATAO%ZS
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,LONG,LATG,
     &             4, GRIDLONS, GRIDLATS)
              GRID_ZS = tmp_out
!            endif
            tmp_in = GFSDATAO%PS
            call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,LONG,LATG,
     &           4, GRIDLONS, GRIDLATS)
            GRID_PS = tmp_out

            do k = 1, LEVSO
              tmp_in(:,:) = GFSDATAO%P(:,:,k)
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &         LONG,LATG,4, GRIDLONS, GRIDLATS)
              GRID_P(:,:,k) = tmp_out(:,:)
              tmp_in(:,:) = GFSDATAO%DP(:,:,k)
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &         LONG,LATG,4, GRIDLONS, GRIDLATS)
              GRID_DP(:,:,k) = tmp_out(:,:)
              tmp_in(:,:) = GFSDATAO%T(:,:,k)
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &         LONG,LATG,4, GRIDLONS, GRIDLATS)
              GRID_T(:,:,k) = tmp_out(:,:)
              tmp_in(:,:) = GFSDATAO%U(:,:,k)
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &         LONG,LATG,4, GRIDLONS, GRIDLATS)
              GRID_U(:,:,k) = tmp_out(:,:)
              tmp_in(:,:) = GFSDATAO%V(:,:,k)
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &         LONG,LATG,4, GRIDLONS, GRIDLATS)
              GRID_V(:,:,k) = tmp_out(:,:)
              tmp_in(:,:) = GFSDATAO%W(:,:,k)
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &         LONG,LATG,4, GRIDLONS, GRIDLATS)
              GRID_W(:,:,k) = tmp_out(:,:)
              tmp_in(:,:) = rh_in(:,:,LEVSO+1-k)
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &         LONG,LATG,4, GRIDLONS, GRIDLATS)
              GRID_RH(:,:,k) = tmp_out(:,:)
            enddo
            do k = 1, LEVSO+1
              tmp_in(:,:) = zh_in(:,:,LEVSO+2-k)
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &         LONG,LATG,4, GRIDLONS, GRIDLATS)
              GRID_ZH(:,:,k) = tmp_out(:,:)
            enddo   
            do l=1,NTRACM 
              do k=1,LEVSO
                tmp_in(:,:) = GFSDATAO%Q(:,:,k,l)
                call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &            LONG,LATG,4, GRIDLONS, GRIDLATS)
                GRID_Q(:,:,k,l) = tmp_out(:,:)

!                call GL2ANY(0,1,GFSDATAO%Q(:,:,k,l),IMO,JMO, 
!     &            GRID_Q(:,:,k,l),LONG,LATG,4, GRIDLONS, GRIDLATS)
              enddo 
            enddo

            !--- remap wind onto face
            deallocate(GRIDLONS,GRIDLATS,tmp_out)
            allocate(GRIDLONS((LONG+1)*LATG))
            allocate(GRIDLATS((LONG+1)*LATG))
            allocate(tmp_out(LONG+1,LATG))
            k = 0
            do j3 = 1, LATG; do i3 = 1, LONG+1
               k = k+1
               GRIDLONS(k) = geolon_e(i3,j3)
               GRIDLATS(k) = geolat_e(i3,j3)
            enddo ; enddo
        
            do k = 1, LEVSO
              tmp_in(:,:) = GFSDATAO%U(:,:,k)
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &         LONG+1,LATG,4, GRIDLONS, GRIDLATS)
              GRID_U_E(:,:,k) = tmp_out(:,:)
              tmp_in(:,:) = GFSDATAO%V(:,:,k)
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &         LONG+1,LATG,4, GRIDLONS, GRIDLATS)
              GRID_V_E(:,:,k) = tmp_out(:,:)            
            enddo
            deallocate(GRIDLONS,GRIDLATS,tmp_out)
            allocate(GRIDLONS(LONG*(LATG+1)))
            allocate(GRIDLATS(LONG*(LATG+1)))
            allocate(tmp_out(LONG,LATG+1))
            k = 0
            do j3 = 1, LATG+1; do i3 = 1, LONG
               k = k+1
               GRIDLONS(k) = geolon_n(i3,j3)
               GRIDLATS(k) = geolat_n(i3,j3)
            enddo ; enddo

            do k = 1, LEVSO
              tmp_in(:,:) = GFSDATAO%U(:,:,k)
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &         LONG,LATG+1,4, GRIDLONS, GRIDLATS)
              GRID_U_N(:,:,k) = tmp_out(:,:)
              tmp_in(:,:) = GFSDATAO%V(:,:,k)
              call GL2ANY(0,1,tmp_in,IMO,JMO,tmp_out,
     &         LONG,LATG+1,4, GRIDLONS, GRIDLATS)
              GRID_V_N(:,:,k) = tmp_out(:,:)            
            enddo
            
            write(out_file, '(a, i1, a)'), 'gfs_data.tile', n, '.nc'
            CALL write_gfs_data2(LONG, LATG, LEVSO, NTRACM
     &,        GRID_ZS, GRID_PS, GRID_P, GRID_DP
     &,        GRID_T, GRID_U, GRID_V, GRID_W, GRID_ZH, GRID_RH
     &,        GRID_Q, .true., geolon,geolat,out_file,
     &         GRID_U_E,GRID_V_E,GRID_U_N,GRID_V_N      )

            deallocate(GRID_ZS,GRID_PS,GRID_P,GRID_DP,GRID_T)
            deallocate(GRID_U,GRID_V,GRID_W,GRID_Q,GRID_ZH,GRID_RH)
            deallocate(GRIDLONS,GRIDLATS,tmp_in,tmp_out)
            deallocate(geolon,geolat,geolon_e,geolat_e)
            deallocate(geolon_n,geolat_n)
            deallocate(GRID_U_E,GRID_V_E,GRID_U_N,GRID_V_N)
          enddo
          
          

        else if(ntiles .NE. 1) then
          print*, "ERROR: ntiles should be 1 or 6"
             
        endif   

        if(.false.) then 
        
          JREC=1

          idim_gfsio=size(GFSDATAO%ZS,1)
          jdim_gfsio=size(GFSDATAO%ZS,2)
!
          allocate(tmp(idim_gfsio*jdim_gfsio))
!
          do j=1,jdim_gfsio
            ii = (j-1)*idim_gfsio
            do i=1,idim_gfsio
              tmp(i+ii) = GFSDATAO%ZS(i,j)
            enddo
          enddo
          CALL GFSIO_WRITEREC(GFILEO,JREC,
     &                        tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
          IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC ZS '
!
          JREC=JREC+1
          do j=1,jdim_gfsio
            ii = (j-1)*idim_gfsio
            do i=1,idim_gfsio
              tmp(i+ii) = GFSDATAO%PS(i,j)
            enddo
          enddo
          CALL GFSIO_WRITEREC(GFILEO,JREC,
     &                        tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
          IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC PS '
          DO K=1,LEVSO
            JREC=JREC+1

            do j=1,jdim_gfsio
              ii = (j-1)*idim_gfsio
              do i=1,idim_gfsio
                tmp(i+ii) = GFSDATAO%P(i,j,k)
              enddo
            enddo
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &                          tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC P '
          ENDDO
          DO K=1,LEVSO
          JREC=JREC+1
!     if (k == 1) print*,' DP=', GFSDATAO%DP(1,:,1)
            do j=1,jdim_gfsio
              ii = (j-1)*idim_gfsio
              do i=1,idim_gfsio
                tmp(i+ii) = GFSDATAO%DP(i,j,k)
              enddo
            enddo
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &                          tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC DP '
          ENDDO
          DO K=1,LEVSO
            JREC=JREC+1
            do j=1,jdim_gfsio
              ii = (j-1)*idim_gfsio
              do i=1,idim_gfsio
                tmp(i+ii) = GFSDATAO%T(i,j,k)
              enddo
            enddo
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &                          tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC T '
          ENDDO
          DO K=1,LEVSO
            JREC=JREC+1
            do j=1,jdim_gfsio
              ii = (j-1)*idim_gfsio
              do i=1,idim_gfsio
                tmp(i+ii) = GFSDATAO%U(i,j,k)
              enddo
            enddo
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &                          tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC U '
          ENDDO
          DO K=1,LEVSO
            JREC=JREC+1
            do j=1,jdim_gfsio
              ii = (j-1)*idim_gfsio
              do i=1,idim_gfsio
                tmp(i+ii) = GFSDATAO%V(i,j,k)
              enddo
            enddo
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &                          tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC V '
          ENDDO
!!        DO N=1,GFSHEADO%NTRAC
          DO N=1,NTRACM
            DO K=1,LEVSO
              JREC=JREC+1
              do j=1,jdim_gfsio
                ii = (j-1)*idim_gfsio
                do i=1,idim_gfsio
                  tmp(i+ii) = GFSDATAO%Q(i,j,k,n)
                enddo
              enddo
              CALL GFSIO_WRITEREC(GFILEO,JREC,
     &                            tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
              IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC Q '
            ENDDO
          ENDDO
          DO K=1,LEVSO
            JREC=JREC+1
            do j=1,jdim_gfsio
              ii = (j-1)*idim_gfsio
              do i=1,idim_gfsio
                tmp(i+ii) = GFSDATAO%W(i,j,k)
              enddo
            enddo
            VVEL_PRECISION=6
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &                          tmp,IRET=IRET,IDRT=GFSHEADO%IDRT,
     &                          PRECISION=VVEL_PRECISION)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC W '
          ENDDO
          print*,' JREC=', JREC, ' NREC=', NREC
         endif
          DEALLOCATE(GFSHEADVO%VCOORD)
          DEALLOCATE(GFSHEADVO%RECNAME)
          DEALLOCATE(GFSHEADVO%RECLEVTYP)
          DEALLOCATE(GFSHEADVO%RECLEV)
          DEALLOCATE(GFSHEADVO%GLAT1D)
          DEALLOCATE(GFSHEADVO%GLON1D)
          DEALLOCATE(GFSHEADVO%CPI)
          DEALLOCATE(GFSHEADVO%RI)

          DEALLOCATE(GFSDATAO%ZS)
          DEALLOCATE(GFSDATAO%PS)
          DEALLOCATE(GFSDATAO%P)
          DEALLOCATE(GFSDATAO%DP)
          DEALLOCATE(GFSDATAO%T)
          DEALLOCATE(GFSDATAO%U)
          DEALLOCATE(GFSDATAO%V)
          DEALLOCATE(GFSDATAO%Q)
          DEALLOCATE(GFSDATAO%W)
        ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  WRITE THE NEW SIGMA FILE
        IF(OUTTYP.EQ.2 .OR. OUTTYP.EQ.0) THEN
          CALL SIGIO_AXDBTA(SIGDATAI,IRET)
!     print *,' sigheado%vcoord=',sigheado%vcoord
      print *,' sigheado%pdryini2=',sigheado%pdryini
          CALL SIGIO_SWHEAD(NSIGO,SIGHEADO,IRET)
          CALL SIGIO_SWDBTA(NSIGO,SIGHEADO,SIGDATAO,IRET)
          CALL SIGIO_AXDBTA(SIGDATAO,IRET)
        ENDIF
       call instrument(10,kall,ttot,tmin,tmax)

! ---------------------------------------------------------------------
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!  CHANGE RESOLUTION OF INPUT GFSIO GRID FILE
!  OUTPUT HISTORY FILE IN SIGIO SIGMA OR GFSIO GRID FORMAT, OR BOTH

      ELSEIF(INPTYP.EQ.1) THEN
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
! ---------------------------------------------------------------------

        PRINT*, 'CHGRES INPUT:  GFSIO GRID FILE '
        IF(OUTTYP .EQ. 1 )PRINT*, 'CHGRES OUTPUT: GFSIO GRID FILE'
        IF(OUTTYP .EQ. 2 )PRINT*, 'CHGRES OUTPUT: SIGIO SIGMA FILE'
        IF(OUTTYP .EQ. 0 )PRINT*,
     &       'CHGRES OUTPUT: BOTH SIGIO SIGMA and GFSIO GRID FILES'
        IF(MQUICK.EQ.1) CALL ERREXIT(24)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  OPEN (READ) GFSIO GRID FILE HEADERS
        CALL GFSIO_GETFILEHEAD(GFILEI
     &,        VERSION=GFSHEADI%VERSION  
     &,        FHOUR=GFSHEADI%FHOUR
     &,        IDATE=GFSHEADI%IDATE
     &,        NREC=GFSHEADI%NREC     
     &,        LATB=GFSHEADI%LATB    
     &,        LONB=GFSHEADI%LONB   
     &,        LEVS=GFSHEADI%LEVS  
     &,        JCAP=GFSHEADI%JCAP 
     &,        ITRUN=GFSHEADI%ITRUN    
     &,        IORDER=GFSHEADI%IORDER  
     &,        IREALF=GFSHEADI%IREALF 
     &,        IGEN=GFSHEADI%IGEN  
     &,        LATF=GFSHEADI%LATF 
     &,        LONF=GFSHEADI%LONF     
     &,        LATR=GFSHEADI%LATR    
     &,        LONR=GFSHEADI%LONR   
     &,        NTRAC=GFSHEADI%NTRAC    
     &,        ICEN2=GFSHEADI%ICEN2  
     &,        IENS=GFSHEADI%IENS
     &,        IDPP=GFSHEADI%IDPP     
     &,        IDSL=GFSHEADI%IDSL    
     &,        IDVC=GFSHEADI%IDVC   
     &,        IDVM=GFSHEADI%IDVM  
     &,        IDVT=GFSHEADI%IDVT 
     &,        IDRUN=GFSHEADI%IDRUN    
     &,        IDUSR=GFSHEADI%IDUSR  
     &,        PDRYINI=GFSHEADI%PDRYINI 
     &,        NCLDT=GFSHEADI%NCLDT  
     &,        IXGR=GFSHEADI%IXGR   
     &,        NVCOORD=GFSHEADI%NVCOORD 
     &,        IDRT=GFSHEADI%IDRT 
     &,        IRET=IRET)

        LEVSI=GFSHEADI%LEVS
        IMI=GFSHEADI%LONB
        JMI=GFSHEADI%LATB
        ALLOCATE(GFSHEADVI%RECNAME(GFSHEADI%NREC))
        ALLOCATE(GFSHEADVI%RECLEVTYP(GFSHEADI%NREC))
        ALLOCATE(GFSHEADVI%RECLEV(GFSHEADI%NREC))
        ALLOCATE(GFSHEADVI%VCOORD(LEVSI+1,GFSHEADI%NVCOORD))
        ALLOCATE(GFSHEADVI%GLAT1D(GFSHEADI%LATB))
        ALLOCATE(GFSHEADVI%GLON1D(GFSHEADI%LONB))
        ALLOCATE(GFSHEADVI%CPI(GFSHEADI%NTRAC+1))
        ALLOCATE(GFSHEADVI%RI(GFSHEADI%NTRAC+1))

        CALL GFSIO_GETFILEHEAD(GFILEI
     &,        RECNAME=GFSHEADVI%RECNAME
     &,        RECLEVTYP=GFSHEADVI%RECLEVTYP
     &,        RECLEV=GFSHEADVI%RECLEV  
     &,        VCOORD=GFSHEADVI%VCOORD 
     &,        GLAT1D=GFSHEADVI%GLAT1D
     &,        GLON1D=GFSHEADVI%GLON1D
     &,        CPI=GFSHEADVI%CPI
     &,        RI=GFSHEADVI%RI
     &,        IRET=IRET1)


        IF(IRET.NE.0 .OR. IRET1.NE.0) THEN
          PRINT*, 'ERROR READNG GFSIO FILE HEADER. EXIT'
          CALL ERREXIT(24)
        ENDIF

        PRINT '("CHANGE GFSIO FILE RESOLUTION FROM ",
     &   I4," WAVES, ",I4," LEVELS, AND ",I4," TRACERS")',
     &   GFSHEADI%JCAP,GFSHEADI%LEVS,GFSHEADI%NTRAC
        PRINT '("                                  ",
     &   " IVS=",I6," IDVC=",I2," NVCOORD=",I2,"  IDVM=",I2)',
     &   GFSHEADI%VERSION,GFSHEADI%IDVC,GFSHEADI%NVCOORD,GFSHEADI%IDVM
        PRINT '("                               TO ",
     &   I4," WAVES, ",I4," LEVELS, AND ",I4," TRACERS.")',
     &   GFSHEADO%JCAP,GFSHEADO%LEVS,GFSHEADO%NTRAC
        PRINT '("                                  ",
     &   " IVS=",I6," IDVC=",I2," NVCOORD=",I2,"  IDVM=",I2)',
     &   GFSHEADO%VERSION,GFSHEADO%IDVC,GFSHEADO%NVCOORD,GFSHEADO%IDVM

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ INPUT GFSIO DATA ARRAY
        ALLOCATE(GFSDATAI%ZS(IMI,JMI))
        ALLOCATE(GFSDATAI%PS(IMI,JMI))
        ALLOCATE(GFSDATAI%P(IMI,JMI,LEVSI))
        ALLOCATE(GFSDATAI%DP(IMI,JMI,LEVSI))
        ALLOCATE(GFSDATAI%T(IMI,JMI,LEVSI))
        ALLOCATE(GFSDATAI%U(IMI,JMI,LEVSI))
        ALLOCATE(GFSDATAI%V(IMI,JMI,LEVSI))
        ALLOCATE(GFSDATAI%Q(IMI,JMI,LEVSI,GFSHEADI%NTRAC))
        ALLOCATE(GFSDATAI%W(IMI,JMI,LEVSI))

          allocate(tmp(size(GFSDATAO%ZS,1)*size(GFSDATAO%ZS,2)) )
          do j=1,size(GFSDATAO%ZS,2)
          do i=1,size(GFSDATAO%ZS,1)
            tmp(i+(j-1)*size(GFSDATAO%ZS,1))=GFSDATAO%ZS(i,j)
          enddo
          enddo
        JREC=1
          CALL GFSIO_READREC(GFILEI,JREC,
     &         tmp,IRET=IRET)
          IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_READREC ZS '
          do j=1,size(GFSDATAO%PS,2)
          do i=1,size(GFSDATAO%PS,1)
            tmp(i+(j-1)*size(GFSDATAO%PS,1))=GFSDATAO%PS(i,j)
          enddo
          enddo
        JREC=JREC+1
          CALL GFSIO_READREC(GFILEI,JREC,
     &         tmp,IRET=IRET)
          IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_READREC PS '
        DO K=1,LEVSI
          do j=1,size(GFSDATAO%P,2)
          do i=1,size(GFSDATAO%P,1)
            tmp(i+(j-1)*size(GFSDATAO%P,1))=GFSDATAO%P(i,j,K)
          enddo
          enddo
        JREC=JREC+1
          CALL GFSIO_READREC(GFILEI,JREC,
     &         tmp,IRET=IRET)
          IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_READREC P '
        ENDDO
        DO K=1,LEVSI
          do j=1,size(GFSDATAO%DP,2)
          do i=1,size(GFSDATAO%DP,1)
            tmp(i+(j-1)*size(GFSDATAO%DP,1))=GFSDATAO%DP(i,j,K)
          enddo
          enddo
        JREC=JREC+1
          CALL GFSIO_READREC(GFILEI,JREC,
     &         tmp,IRET=IRET)
          IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_READREC DP '
        ENDDO
        DO K=1,LEVSI
          do j=1,size(GFSDATAO%T,2)
          do i=1,size(GFSDATAO%T,1)
            tmp(i+(j-1)*size(GFSDATAO%T,1))=GFSDATAO%T(i,j,K)
          enddo
          enddo
        JREC=JREC+1
          CALL GFSIO_READREC(GFILEI,JREC,
     &         tmp,IRET=IRET)
          IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_READREC T '
        ENDDO
        DO K=1,LEVSI
          do j=1,size(GFSDATAO%U,2)
          do i=1,size(GFSDATAO%U,1)
            tmp(i+(j-1)*size(GFSDATAO%U,1))=GFSDATAO%U(i,j,K)
          enddo
          enddo
        JREC=JREC+1
          CALL GFSIO_READREC(GFILEI,JREC,
     &         tmp,IRET=IRET)
          IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_READREC U '
        ENDDO
        DO K=1,LEVSI
          do j=1,size(GFSDATAO%V,2)
          do i=1,size(GFSDATAO%V,1)
            tmp(i+(j-1)*size(GFSDATAO%V,1))=GFSDATAO%V(i,j,K)
          enddo
          enddo

        JREC=JREC+1
          CALL GFSIO_READREC(GFILEI,JREC,
     &         tmp,IRET=IRET)
          IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_READREC V '
        ENDDO
        DO N=1,GFSHEADI%NTRAC
        DO K=1,LEVSI
          do j=1,size(GFSDATAO%Q,2)
          do i=1,size(GFSDATAO%Q,1)
            tmp(i+(j-1)*size(GFSDATAO%Q,1))=GFSDATAO%Q(i,j,K,N)
          enddo
          enddo
        JREC=JREC+1
          CALL GFSIO_READREC(GFILEI,JREC,
     &         tmp,IRET=IRET)
          IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_READREC Q '
        ENDDO
        ENDDO
        IF ( JREC .LT. GFSHEADI%NREC ) then
           DO K=1,LEVSI
          do j=1,size(GFSDATAO%W,2)
          do i=1,size(GFSDATAO%W,1)
            tmp(i+(j-1)*size(GFSDATAO%W,1))=GFSDATAO%W(i,j,K)
          enddo
          enddo

           JREC=JREC+1
             CALL GFSIO_READREC(GFILEI,JREC,
     &          tmp,IRET=IRET)
             IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_READREC W '
           ENDDO
         ELSE
!----   currently, computing vvel from gfsio is not available!
        print *,'currently, computing vvel from gfsio is not available!'
         ENDIF

        NCI=SIZE(GFSDATAI%T,1)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  UPDATE GFSIO OUTPUT FILE HEADERS USING NAME LIST VARIABLES IF NECESSARY

        !--------------------
         GFSHEADO = GFSHEADI
        !--------------------

        IF(JCAP.GT.0) GFSHEADO%JCAP=JCAP
        IF(LEVS.GT.0) GFSHEADO%LEVS=LEVS
        IF(LONB.GT.0.AND.LATB.GT.0) THEN
          GFSHEADO%LONB=LONB
          GFSHEADO%LATB=LATB
        ENDIF
        IF(NTRAC.GT.0) GFSHEADO%NTRAC=NTRAC
        IF(IDVC.GT.0)  GFSHEADO%IDVC=IDVC
        IF(IDVM.GT.0)  GFSHEADO%IDVM=IDVM
        IF(IDSL.GT.0)  GFSHEADO%IDSL=IDSL
        IF(IGEN.GT.0)  GFSHEADO%IGEN=IGEN
        IF(IDVT.GT.0)  GFSHEADO%IDVT=IDVT
        IF(NCLDT.GT.0) GFSHEADO%NCLDT=NCLDT
        IF(NCLDT.EQ.0) NCLDT=GFSHEADO%NCLDT
        IF(NVCOORD.GT.0) GFSHEADO%NVCOORD=NVCOORD
        IF(IDRT.EQ.0) GFSHEADO%IDRT=IDRT
        GFSHEADO%LONF=GFSHEADO%LONB
        GFSHEADO%LATF=GFSHEADO%LATB
        GFSHEADO%LONR=GFSHEADO%LONB
        GFSHEADO%LATR=GFSHEADO%LATB
        LEVSO=GFSHEADO%LEVS
        IMO=GFSHEADO%LONB
        JMO=GFSHEADO%LATB
        NTRACM=MIN(GFSHEADI%NTRAC,GFSHEADO%NTRAC)
        NTRACO=NTRACM
       call instrument(2,kall,ttot,tmin,tmax)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  UPDATE GFSIO OUTPUT FILE SECOND HEADER
! idea ?
        NREC=2+GFSHEADO%LEVS*(6+NTRACM)     !zs,ps,p,dp,t,u,v,q(ntracer)
        ALLOCATE(GFSHEADVO%VCOORD(LEVSO+1,GFSHEADO%NVCOORD))
        ALLOCATE(GFSHEADVO%RECNAME(NREC))
        ALLOCATE(GFSHEADVO%RECLEVTYP(NREC))
        ALLOCATE(GFSHEADVO%RECLEV(NREC))
        ALLOCATE(GFSHEADVO%GLAT1D(JMO))
        ALLOCATE(GFSHEADVO%GLON1D(IMO))
        ALLOCATE(GFSHEADVO%CPI(NTRACM+1))
        ALLOCATE(GFSHEADVO%RI(NTRACM+1))

        IF(GFSHEADO%IDVC.EQ.GFSHEADI%IDVC.AND.
     &      GFSHEADO%LEVS.EQ.GFSHEADI%LEVS.AND.
     &      GFSHEADO%NVCOORD.EQ.GFSHEADI%NVCOORD.AND.
     &      NREC.EQ.GFSHEADI%NREC) THEN
          GFSHEADVO=GFSHEADVI
        ELSE
          GFSHEADVO%RECNAME(1)='hgt'
          GFSHEADVO%RECNAME(2)='pres'
          GFSHEADVO%RECNAME(3:(2+LEVSO))='pres'
          GFSHEADVO%RECNAME((3+LEVSO):(2+2*LEVSO))='dpres'
          GFSHEADVO%RECNAME((3+2*LEVSO):(2+3*LEVSO))='tmp'
          GFSHEADVO%RECNAME((3+3*LEVSO):(2+4*LEVSO))='ugrd'
          GFSHEADVO%RECNAME((3+4*LEVSO):(2+5*LEVSO))='vgrd'
          GFSHEADVO%RECNAME((3+5*LEVSO):(2+6*LEVSO))='spfh'
          GFSHEADVO%RECNAME((3+6*LEVSO):(2+7*LEVSO))='o3mr'
          GFSHEADVO%RECNAME((3+7*LEVSO):(2+8*LEVSO))='clwmr'
          if ( NREC.eq.GFSHEADI%NREC ) then
          GFSHEADVO%RECNAME((3+8*LEVSO):(2+9*LEVSO))='vvel'
          endif
          GFSHEADVO%RECLEVTYP(1:2)='sfc'
          GFSHEADVO%RECLEVTYP(3:NREC)='layer'
          GFSHEADVO%RECLEV(1:2)=1      
          DO K=1,LEVSO
            GFSHEADVO%RECLEV(2+K)=K
            GFSHEADVO%RECLEV(2+LEVSO+K)=K
            GFSHEADVO%RECLEV(2+2*LEVSO+K)=K
            GFSHEADVO%RECLEV(2+3*LEVSO+K)=K
            GFSHEADVO%RECLEV(2+4*LEVSO+K)=K
            GFSHEADVO%RECLEV(2+5*LEVSO+K)=K
            GFSHEADVO%RECLEV(2+6*LEVSO+K)=K
            GFSHEADVO%RECLEV(2+7*LEVSO+K)=K
            if ( NREC.eq.GFSHEADI%NREC ) then
              GFSHEADVO%RECLEV(2+8*LEVSO+K)=K
            endif 
          ENDDO
          if(  GFSHEADO%LEVS.EQ.GFSHEADI%LEVS.AND.
     &     GFSHEADO%NVCOORD.EQ.GFSHEADI%NVCOORD ) then
             GFSHEADVO%VCOORD=GFSHEADVI%VCOORD
          else
             print *,'You use different VCOORD from input,', 
     &      ' please provide GFSHEADVO%VCOORD!'
          endif
          if ( GFSHEADO%NTRAC.EQ.GFSHEADI%NTRAC) then
            GFSHEADVO%CPI=GFSHEADVI%CPI
            GFSHEADVO%RI=GFSHEADVI%RI
          else
             print *,'WARNING: You have different Tracers from input,',
     &       ' please provide GFSHEADVO%VCOORD!'
          endif
        ENDIF
        cpi(0:ntraco)=GFSHEADVO%CPI(1:ntraco+1)
        ri(0:ntraco)=GFSHEADVO%RI(1:ntraco+1)
!       print *,'after headvo, vcoord=',GFSHEADVO%VCOORD(1:5,1)
!     &  ,  GFSHEADVO%VCOORD(1:5,2)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ALLOCATE GFSIO OUTPUT DATA
        ALLOCATE(GFSDATAO%ZS(IMO,JMO))
        ALLOCATE(GFSDATAO%PS(IMO,JMO))
        ALLOCATE(GFSDATAO%P(IMO,JMO,LEVSO))
        ALLOCATE(GFSDATAO%DP(IMO,JMO,LEVSO))
        ALLOCATE(GFSDATAO%T(IMO,JMO,LEVSO))
        ALLOCATE(GFSDATAO%U(IMO,JMO,LEVSO))
        ALLOCATE(GFSDATAO%V(IMO,JMO,LEVSO))
        ALLOCATE(GFSDATAO%Q(IMO,JMO,LEVSO,GFSHEADO%NTRAC))
        ALLOCATE(GFSDATAO%W(IMO,JMO,LEVSO))

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ALLOCATE TEMPORARY DATA
          ALLOCATE(SLAT(JMO))
          ALLOCATE(WLAT(JMO))
          ALLOCATE(CLAT(JMO))
          ALLOCATE(RLAT(JMO))
          ALLOCATE(HSI(NCI))
          ALLOCATE(OROGI(IMO,JMO))
          ALLOCATE(OROGO(IMO,JMO))
          ALLOCATE(ZSI(IMO,JMO))
          ALLOCATE(PSI(IMO,JMO))
          ALLOCATE(PI(IMO,JMO,LEVSI))
          ALLOCATE(TI(IMO,JMO,LEVSI))
          ALLOCATE(TIV(IMO,JMO,LEVSI))
          ALLOCATE(UI(IMO,JMO,LEVSI))
          ALLOCATE(VI(IMO,JMO,LEVSI))
          ALLOCATE(WI(IMO,JMO,LEVSI))
          ALLOCATE(QI(IMO,JMO,LEVSI,NTRACM))
          ALLOCATE(ZSO(IMO,JMO))
          ALLOCATE(PSO(IMO,JMO))
          ALLOCATE(PO(IMO,JMO,LEVSO))
          ALLOCATE(TO(IMO,JMO,LEVSO))
          ALLOCATE(UO(IMO,JMO,LEVSO))
          ALLOCATE(VO(IMO,JMO,LEVSO))
          ALLOCATE(WO(IMO,JMO,LEVSO))
          ALLOCATE(QO(IMO,JMO,LEVSO,NTRACO))
          ALLOCATE(TPO(IMO,LEVSO))
!         ALLOCATE(DPDTO(IMO,LEVSO))
          ALLOCATE(DTDPO(IMO*JMO,LEVSO))
          ALLOCATE(DPO(IMO,JMO,LEVSO))

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW LATITUDES
          CALL SPLAT(IDRT,JMO,SLAT,WLAT)
          CLAT=SQRT(1-SLAT**2)
          RLAT=180/ACOS(-1.)*ASIN(SLAT)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW OROGRAPHY
          JPDS=-1
          JPDS(5)=8
          IF(NORO.NE.0) THEN
            ALLOCATE(BITMAP(IMO*JMO))
            CALL GETGB(NORO,0,IMO*JMO,0,JPDS,JGDS,
     &                 KF,K,KPDS,KGDS,BITMAP,OROGO,IOSORO)
            DEALLOCATE(BITMAP)
          ELSE
            IOSORO=9
          ENDIF
          IF(IOSORO.EQ.0.AND.(KGDS(1).NE.IDRT.OR.
     &     KGDS(2).NE.IMO.OR.KGDS(3).NE.JMO)) IOSORO=100
          IF(IOSORO.EQ.0) THEN
            PRINT '("  NEW OROGRAPHY READ IN")'
            if (kgds(4) == -90000 .and. kgds(5) == -180000) then
              print *,' reversing the lat/lon for orography'
              call REVERS(imo, jmo, OROGO)
            endif
          ELSE
            PRINT '("  NEW OROGRAPHY INTERPOLATED FROM OLD")'
          ENDIF
       call instrument(3,kall,ttot,tmin,tmax)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET NEW SIGMA LEVELS
          IF(NSIL.NE.0) THEN
!cggg
            allocate(vcoord(GFSHEADO%LEVS+1,GFSHEADO%NVCOORD))
            vcoord = GFSHEADVO%VCOORD
            CALL NEWSIG(NSIL,GFSHEADO%IDVC,GFSHEADO%LEVS,
     &                  GFSHEADO%NVCOORD,VCOORD,IOSSIL)
            GFSHEADVO%VCOORD = vcoord
            deallocate(vcoord)
!cggg     &                  GFSHEADO%NVCOORD,GFSHEADVO%VCOORD,IOSSIL)
            IF(IOSSIL.EQ.0) THEN
              PRINT '("  NEW MODEL LEVELS READ IN")'
            ENDIF
          ELSEIF(GFSHEADO%IDVC.EQ.GFSHEADI%IDVC.AND.
     &           GFSHEADO%LEVS.EQ.GFSHEADI%LEVS.AND.
     &           GFSHEADO%NVCOORD.EQ.GFSHEADI%NVCOORD) THEN
            GFSHEADVO%VCOORD=GFSHEADVI%VCOORD
            IOSSIL=0
            PRINT '("  NEW MODEL LEVELS COPIED FROM OLD")'
          ELSE
            IOSSIL=42
          ENDIF
          IF(IOSSIL.NE.0) THEN
            PRINT '("  ERROR DEFINING SIGMA VALUES ",I4)',IOSSIL
            CALL ERREXIT(8)
          ENDIF
       call instrument(4,kall,ttot,tmin,tmax)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  HORIZONTAL INTERPOLATION
          DO J=1,JMI
          DO I=1,IMI
           HSI(I*J)=GFSDATAI%ZS(I,J)
          ENDDO
          ENDDO
          CALL GL2GL(2,1,GFSDATAI%ZS,IMI,JMI,ZSI,IMO,JMO,
     &               GFSHEADI%IDRT, GFSHEADO%IDRT)
          CALL GL2GL(2,1,GFSDATAI%PS,IMI,JMI,PSI,IMO,JMO,
     &               GFSHEADI%IDRT, GFSHEADO%IDRT)
          CALL GL2GL(2,LEVSI,GFSDATAI%P,IMI,JMI,PI,IMO,JMO,
     &               GFSHEADI%IDRT, GFSHEADO%IDRT)
          CALL GL2GL(2,LEVSI,GFSDATAI%T,IMI,JMI,TI,IMO,JMO,
     &               GFSHEADI%IDRT, GFSHEADO%IDRT)
          CALL GL2GL(2,LEVSI,GFSDATAI%U,IMI,JMI,UI,IMO,JMO,
     &               GFSHEADI%IDRT, GFSHEADO%IDRT)
          CALL GL2GL(2,LEVSI,GFSDATAI%V,IMI,JMI,VI,IMO,JMO,
     &               GFSHEADI%IDRT, GFSHEADO%IDRT)
          DO N=1,NTRACM
          CALL GL2GL(2,LEVSI,GFSDATAI%Q(:,:,:,N),IMI,JMI,QI(:,:,:,N),
     &               IMO,JMO,GFSHEADI%IDRT, GFSHEADO%IDRT)
          ENDDO
          if ( NREC.eq.GFSHEADI%NREC ) then
            CALL GL2GL(2,LEVSI,GFSDATAI%W,IMI,JMI,WI,IMO,JMO,
     &               GFSHEADI%IDRT, GFSHEADO%IDRT)
          else
             WI=0.
          endif
          TIV=TI*(1.+(461.50/287.05-1)*QI(:,:,:,1))  ! virtual temperature

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  USE NEW OROGRAPHY OR USE TRUNCATED OROGRAPHY

          IF(IOSORO.EQ.0) THEN
            ZSO=OROGO
            OROGI=ZSI
          ELSE
            ZSO=ZSI             
            OROGI=ZSI             
            OROGO=ZSO             
          ENDIF
         call instrument(5,kall,ttot,tmin,tmax)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NEW SURFACE PRESSURE
          IJX=IMO*JMO
          IJL=IMO*JMO
!cggg
          allocate(vcoord(GFSHEADI%LEVS+1,GFSHEADI%NVCOORD))
          vcoord = GFSHEADVI%VCOORD
          CALL SIGIO_MODPRD
     &       (IJL,IJX,GFSHEADI%LEVS,GFSHEADI%NVCOORD,
!cggg     &        GFSHEADI%IDVC,GFSHEADI%IDSL,GFSHEADVI%VCOORD,IRET,
     &        GFSHEADI%IDVC,GFSHEADI%IDSL,VCOORD,IRET,
     &        PS=PSI,T=TIV,PM=PI)
!cggg
           deallocate (vcoord)
            CALL NEWPS(IJL,ZSI,PSI,IJX,GFSHEADI%LEVS,
     &                 PI,TI,QI,ZSO,PSO)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  VERTICALLY INTERPOLATE UPPER-AIR FIELDS
! -- Mark Iredell's approach 
!           TPO=250.               !virtual temperature
!           DO ITER=1,100
!             CALL SIGIO_MODPR
!    &         (IJL,IJX,GFSHEADO%LEVS,GFSHEADO%NVCOORD,
!    &          GFSHEADO%IDVC,GFSHEADO%IDSL,GFSHEADO%VCOORD,IRET,
!    &          PS=PSO,T=TPO,PM=PO,DPMDT=DPDTO,PD=DPO)
!        call instrument(7,kall,ttot,tmin,tmax)
!             CALL VINTG(IJL,IJX,GFSHEADI%LEVS,GFSHEADO%LEVS,NTRACM,
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
!cggg
            allocate(vcoord(GFSHEADO%LEVS+1,GFSHEADO%NVCOORD))
            vcoord = GFSHEADVO%VCOORD
            CALL NEWPR1(IJL,IJX,GFSHEADO%LEVS,GFSHEADI%LEVS,
     &                 GFSHEADO%IDVC,GFSHEADO%IDVM,GFSHEADO%IDSL,
     &                 GFSHEADO%NVCOORD,VCOORD,
!cggg     &                 GFSHEADO%NVCOORD,GFSHEADVO%VCOORD,
     &                 RI, CPI, NTRACM,
!    &                 GFSHEADVO%RI,GFSHEADVO%CPI,NTRACM,
     &                 PI,TI,QI,PSO,PO,DPO)
!cggg
        deallocate(vcoord)
         call instrument(9,kall,ttot,tmin,tmax)
            CALL VINTG(IJL,IJX,GFSHEADI%LEVS,GFSHEADO%LEVS,NTRACM,
     &                 PI,UI,VI,TI,QI,WI,PO,UO,VO,TO,QO,DTDPO,WO)
!
! idea add init condition for temp tracer4-5 ( o o2)
!           IF (IDVT == 200) then
!             CALL  VINTG_IDEA(IMO,LATCH,SIGHEADO%LEVS,NTRACO,
!    &          PO,RLAT,JMO,J1,J2,SIGHEADI%IDATE,UO,VO,TO,QO)
!           ENDIF
!
            if( GFSHEADO%IDVC .eq. 3 ) then
!cggg
          allocate(ak(GFSHEADO%LEVS+1))
          ak = GFSHEADVO%VCOORD(1:(GFSHEADO%LEVS+1),1)
          allocate(bk(GFSHEADO%LEVS+1))
          bk = GFSHEADVO%VCOORD(1:(GFSHEADO%LEVS+1),2)
          allocate(ck(GFSHEADO%LEVS+1))
          ck = GFSHEADVO%VCOORD(1:(GFSHEADO%LEVS+1),3)
              call checkdp(IJL,IJX,GFSHEADO%LEVS,ak,bk,ck,
     &                 PSO,TO,QO)
!cggg              call checkdp(IJL,IJX,GFSHEADO%LEVS,
!cggg     &                 GFSHEADVO%VCOORD(1:(GFSHEADO%LEVS+1),1),
!cggg     &                 GFSHEADVO%VCOORD(1:(GFSHEADO%LEVS+1),2),
!cggg     &                 GFSHEADVO%VCOORD(1:(GFSHEADO%LEVS+1),3),
!cggg     &                 PSO,TO,QO)
              deallocate(ak,bk,ck)
            endif
         call instrument(10,kall,ttot,tmin,tmax)
! -- end of Henry Juang's approach

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  PASS DATA TO GFSIO
            IF(OUTTYP.EQ.1 .OR. OUTTYP.EQ.0) THEN
              GFSDATAO%ZS = ZSO
              GFSDATAO%PS = PSO
              GFSDATAO%DP = DPO
              GFSDATAO%P  = PO
              GFSDATAO%T  = TO
              GFSDATAO%U  = UO
              GFSDATAO%V  = VO
              DO N=1,NTRACM
                GFSDATAO%Q(:,:,:,N) = QO(:,:,:,N)
              ENDDO
              GFSDATAO%W  = WO
            ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!   TRANSFORM BACK TO THE NEW SPECTRAL SPACE FOR SIGIO SIGMA OUTPUT 
            IF(OUTTYP.EQ.2 .OR. OUTTYP.EQ.0) THEN
              print*,' gfsio transform back to spectral space '
              print*,' is yet to be completed '
!             CALL SIGIO_ALDATA(SIGHEADO,SIGDATAO,IRET)
!             CALL TRBSC(GFSHEADO%JCAP,NCO,GFSHEADO%LEVS,NTRACM,
!    &                 GFSHEADO%IDVM,IDRT,IMO,JMO,IJX,1,JMO,1,
!    &                 ZSO,PSO,TO,UO,VO,QO,
!    &                 SIGDATAO%HS,SIGDATAO%PS,SIGDATAO%T,
!    &                 SIGDATAO%D,SIGDATAO%Z,SIGDATAO%Q)
            ENDIF
         call instrument(11,kall,ttot,tmin,tmax)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  DEALLOCATE TEMPORARY DATA
          DEALLOCATE(ZSI)
          DEALLOCATE(PSI)
          DEALLOCATE(PI)
          DEALLOCATE(TI)
          DEALLOCATE(TIV)
          DEALLOCATE(UI)
          DEALLOCATE(VI)
          DEALLOCATE(WI)
          DEALLOCATE(QI)
          DEALLOCATE(ZSO)
          DEALLOCATE(PSO)
          DEALLOCATE(PO)
          DEALLOCATE(TO)
          DEALLOCATE(UO)
          DEALLOCATE(VO)
          DEALLOCATE(QO)
          DEALLOCATE(WO)
          DEALLOCATE(TPO)
!         DEALLOCATE(DPDTO)
          DEALLOCATE(DTDPO)
          DEALLOCATE(DPO)
       call instrument(12,kall,ttot,tmin,tmax)


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GENERATE NEW OZONE FROM CLIMATOLOGY IF NECESSARY
        IF(GFSHEADI%NTRAC.EQ.1.AND.GFSHEADO%NTRAC.GT.1.AND.
     &     GFSHEADO%IDVC.EQ.0.AND.MOD(GFSHEADO%IDVT/1,10).LE.1.AND.
     &     NO3C.NE.0) THEN
          print*," SPECO3 not working yet for gfsio, exit"
          call errexit(24)
          IDAT=0
          IDAT(1)=GFSHEADO%IDATE(4)
          IDAT(2)=GFSHEADO%IDATE(2)
          IDAT(3)=GFSHEADO%IDATE(3)
          IDAT(5)=GFSHEADO%IDATE(1)
          RINC=0
          RINC(2)=GFSHEADO%FHOUR
!         CALL W3MOVDAT(RINC,IDAT,JDAT)
!         CALL SPECO3(NO3C,NO3T,SIGHEADO%JCAP,NCO,SIGHEADO%LEVS,
!    &                SIGHEADO%LONB,SIGHEADO%LATB,1,JDAT,
!    &                SIGHEADO%SL,SIGHEADO%VCOORD(:,1),
!    &                SIGDATAO%PS,SIGDATAO%Q(1,1,2))
!         PRINT '("  OZONE GENERATED FROM CLIMATOLOGY")'
        ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GENERATE SPECIAL SETS OF TRACERS
        IF(GFSHEADO%IDVT.GT.0.AND.MOD(GFSHEADO%IDVT,100).EQ.0) THEN
!!        CALL SPECSETS(GFSHEADO,GFSDATAO,IDRT)
          print*," SPECSETS not working yet for gfsio, exit"
          call errexit(24)
        ENDIF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! WRITE OUT GFSIO  FILE

        IF(OUTTYP.EQ.1 .OR. OUTTYP.EQ.0) THEN
          print*, ' write out gfsio chgres.out.grd'
          CALL GFSIO_OPEN(GFILEO,TRIM('chgres.out.grd'),'write'
     &,        VERSION=GFSHEADO%VERSION  
     &,        FHOUR=GFSHEADO%FHOUR
     &,        IDATE=GFSHEADO%IDATE
     &,        NREC=GFSHEADO%NREC     
     &,        LATB=GFSHEADO%LATB    
     &,        LONB=GFSHEADO%LONB   
     &,        LEVS=GFSHEADO%LEVS  
     &,        JCAP=GFSHEADO%JCAP 
     &,        ITRUN=GFSHEADO%ITRUN    
     &,        IORDER=GFSHEADO%IORDER  
     &,        IREALF=GFSHEADO%IREALF 
     &,        IGEN=GFSHEADO%IGEN  
     &,        LATF=GFSHEADO%LATF 
     &,        LONF=GFSHEADO%LONF     
     &,        LATR=GFSHEADO%LATR    
     &,        LONR=GFSHEADO%LONR   
     &,        NTRAC=GFSHEADO%NTRAC    
     &,        ICEN2=GFSHEADO%ICEN2  
     &,        IENS=GFSHEADO%IENS
     &,        IDPP=GFSHEADO%IDPP     
     &,        IDSL=GFSHEADO%IDSL    
     &,        IDVC=GFSHEADO%IDVC   
     &,        IDVM=GFSHEADO%IDVM  
     &,        IDVT=GFSHEADO%IDVT 
     &,        IDRUN=GFSHEADO%IDRUN    
     &,        IDUSR=GFSHEADO%IDUSR  
     &,        PDRYINI=GFSHEADO%PDRYINI 
     &,        NCLDT=GFSHEADO%NCLDT  
     &,        IXGR=GFSHEADO%IXGR   
     &,        NVCOORD=GFSHEADO%NVCOORD 
     &,        IDRT=GFSHEADO%IDRT 
     &,        RECNAME=GFSHEADVO%RECNAME
     &,        RECLEVTYP=GFSHEADVO%RECLEVTYP
     &,        RECLEV=GFSHEADVO%RECLEV  
     &,        VCOORD=GFSHEADVO%VCOORD 
     &,        CPI=GFSHEADVO%CPI 
     &,        RI=GFSHEADVO%RI 
     &,        IRET=IRET)
          IF(IRET.NE.0) THEN
            PRINT*, ' ERROR AT GFSIO_OPEN chgres.out.grd '
            CALL ERREXIT(4)
          ENDIF
          print*, ' gfsio chgres.out.grd open'

         
          allocate(tmp(size(GFSDATAO%ZS,1)*size(GFSDATAO%ZS,2)) )
          do j=1,size(GFSDATAO%ZS,2)
          do i=1,size(GFSDATAO%ZS,1)
            tmp(i+(j-1)*size(GFSDATAO%ZS,1))=GFSDATAO%ZS(i,j)
          enddo
          enddo
          JREC=1
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &           tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC ZS '
          do j=1,size(GFSDATAO%PS,2)
          do i=1,size(GFSDATAO%PS,1)
            tmp(i+(j-1)*size(GFSDATAO%PS,1))=GFSDATAO%PS(i,j)
          enddo
          enddo
          JREC=JREC+1
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &           tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC PS '
          DO K=1,LEVSO
          do j=1,size(GFSDATAO%P,2)
          do i=1,size(GFSDATAO%P,1)
            tmp(i+(j-1)*size(GFSDATAO%P,1))=GFSDATAO%P(i,j,K)
          enddo
          enddo
          JREC=JREC+1
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &           tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC P '
          ENDDO
          DO K=1,LEVSO
          do j=1,size(GFSDATAO%DP,2)
          do i=1,size(GFSDATAO%DP,1)
            tmp(i+(j-1)*size(GFSDATAO%DP,1))=GFSDATAO%DP(i,j,K)
          enddo
          enddo
          JREC=JREC+1
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &           tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC DP '
          ENDDO
          DO K=1,LEVSO
          do j=1,size(GFSDATAO%T,2)
          do i=1,size(GFSDATAO%T,1)
            tmp(i+(j-1)*size(GFSDATAO%T,1))=GFSDATAO%T(i,j,K)
          enddo
          enddo
          JREC=JREC+1
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &           tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC T '
          ENDDO
          DO K=1,LEVSO
          do j=1,size(GFSDATAO%U,2)
          do i=1,size(GFSDATAO%U,1)
            tmp(i+(j-1)*size(GFSDATAO%U,1))=GFSDATAO%U(i,j,K)
          enddo
          enddo
          JREC=JREC+1
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &           tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC U '
          ENDDO
          DO K=1,LEVSO
          do j=1,size(GFSDATAO%V,2)
          do i=1,size(GFSDATAO%V,1)
            tmp(i+(j-1)*size(GFSDATAO%V,1))=GFSDATAO%V(i,j,K)
          enddo
          enddo
          JREC=JREC+1
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &           tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC V '
          ENDDO
          DO N=1,GFSHEADO%NTRAC
          DO K=1,LEVSO
          do j=1,size(GFSDATAO%Q,2)
          do i=1,size(GFSDATAO%Q,1)
            tmp(i+(j-1)*size(GFSDATAO%Q,1))=GFSDATAO%Q(i,j,K,N)
          enddo
          enddo
          JREC=JREC+1
            CALL GFSIO_WRITEREC(GFILEO,JREC,
     &           tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
            IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC Q '
          ENDDO
          ENDDO
          if ( NREC.eq.GFSHEADO%NREC ) then
           DO K=1,LEVSO
          do j=1,size(GFSDATAO%W,2)
          do i=1,size(GFSDATAO%W,1)
            tmp(i+(j-1)*size(GFSDATAO%W,1))=GFSDATAO%W(i,j,K)
          enddo
          enddo
            JREC=JREC+1
              CALL GFSIO_WRITEREC(GFILEO,JREC,
     &           tmp,IRET=IRET,IDRT=GFSHEADO%IDRT)
              IF(IRET.NE.0) PRINT*, ' ERROR GFSIO_WRITEREC W '
           ENDDO
          endif 
          deallocate(tmp)

            print*,' JREC=', JREC, ' NREC=', NREC
        ENDIF

        DEALLOCATE(GFSHEADVI%VCOORD)
        DEALLOCATE(GFSHEADVI%RECNAME)
        DEALLOCATE(GFSHEADVI%RECLEVTYP)
        DEALLOCATE(GFSHEADVI%RECLEV)
        DEALLOCATE(GFSHEADVI%GLAT1D)
        DEALLOCATE(GFSHEADVI%GLON1D)
        DEALLOCATE(GFSHEADVI%CPI)
        DEALLOCATE(GFSHEADVI%RI)
        DEALLOCATE(GFSDATAI%ZS)
        DEALLOCATE(GFSDATAI%PS)
        DEALLOCATE(GFSDATAI%P)
        DEALLOCATE(GFSDATAI%DP)
        DEALLOCATE(GFSDATAI%T)
        DEALLOCATE(GFSDATAI%U)
        DEALLOCATE(GFSDATAI%V)
        DEALLOCATE(GFSDATAI%Q)
        DEALLOCATE(GFSDATAI%W)
        DEALLOCATE(GFSHEADVO%VCOORD)
        DEALLOCATE(GFSHEADVO%RECNAME)
        DEALLOCATE(GFSHEADVO%RECLEVTYP)
        DEALLOCATE(GFSHEADVO%RECLEV)
        DEALLOCATE(GFSHEADVO%GLAT1D)
        DEALLOCATE(GFSHEADVO%GLON1D)
        DEALLOCATE(GFSHEADVO%CPI)
        DEALLOCATE(GFSHEADVO%RI)
        DEALLOCATE(GFSDATAO%ZS)
        DEALLOCATE(GFSDATAO%PS)
        DEALLOCATE(GFSDATAO%P)
        DEALLOCATE(GFSDATAO%DP)
        DEALLOCATE(GFSDATAO%T)
        DEALLOCATE(GFSDATAO%U)
        DEALLOCATE(GFSDATAO%V)
        DEALLOCATE(GFSDATAO%Q)
        DEALLOCATE(GFSDATAO%W)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  WRITE THE NEW SIGMA FILE
        IF(OUTTYP.EQ.2 .OR. OUTTYP.EQ.0) THEN
          print*," from gfsio to sigio is yet to be finished" 
!         CALL SIGIO_SWHEAD(NSIGO,SIGHEADO,IRET)
!         CALL SIGIO_SWDATA(NSIGO,SIGHEADO,SIGDATAO,IRET)
!         CALL SIGIO_AXDATA(SIGDATAO,IRET)
        ENDIF
       call instrument(13,kall,ttot,tmin,tmax)

! ---------------------------------------------------------------------
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ENDIF     !!!END OF INPTYP OPTIONS
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
! ---------------------------------------------------------------------
      ENDIF  ! END OF make_sfc option
!  CLOSE FILES
      IF(INPTYP.EQ.1)  CALL GFSIO_CLOSE(GFILEI,IRET=IRET)
      IF(INPTYP.EQ.2)  CALL SIGIO_SCLOSE(NSIGI,IRET)
      IF(INPTYP.NE.0) THEN
        CALL GFSIO_CLOSE(GFILEO,IRET=IRET)
        CALL SIGIO_SCLOSE(NSIGO,IRET)
        IF(NORO.GT.0)  CALL BACLOSE(NORO,IRET)
        IF(NSIL.GT.0)  CLOSE(NSIL)
        IF(NO3C.GT.0)  CLOSE(NO3C)
        IF(NO3T.GT.0)  CALL BACLOSE(NO3T,IRET)
      ENDIF

!  END OF CHANGE RESOLUTION FOR HISTROY FILE
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------






C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  OPEN SURFACE FILES
      NSFCI=21
      NSLM=22
      NLPL=23
      NSFCO=61
      if( make_sfc) then
      
      CALL SFCIO_SROPEN(NSFCI,'chgres.inp.sfc',IRET)
      IF(IRET.EQ.0) THEN
        CALL BAOPENR(NSLM,'chgres.inp.slmgb',IRET)
        IF(IRET.NE.0) NSLM=0
      print *,' iret=',iret,' nslm=',nslm
        OPEN(NLPL,FILE='chgres.inp.lonsperlat',
     &       FORM='FORMATTED',STATUS='OLD',IOSTAT=IRET)
        IF(IRET.NE.0) NLPL=0
        CALL SFCIO_SWOPEN(NSFCO,'chgres.out.sfc',IRET)
        IF(IRET.NE.0) NSFCO=0
      ELSE
        NSFCO=0
      ENDIF
       call instrument(15,kall,ttot,tmin,tmax)
      INQUIRE (FILE="./chgres.inp.nst", EXIST=DO_NSST)
      IF (DO_NSST .AND. NSFCO == 0) THEN
        print*,'error: when converting an nsst restart file,'
        print*,'you must also convert a surface restart file.'
        stop 333
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  OPEN SURFACE HEADER
      IF(NSFCO.GT.0) THEN
        CALL SFCIO_SRHEAD(NSFCI,SFCHEADI,IRET)
        IF(IRET.EQ.0) THEN
          SFCHEADO=SFCHEADI
          IF(LONB.GT.0) SFCHEADO%LONB=LONB
          IF(LATB.GT.0) SFCHEADO%LATB=LATB
          IF(IVSSFC.GT.0) THEN
            SFCHEADO%IVS=IVSSFC
!           IF (SFCHEADO%IVS /= 200501 .AND.
!    &          SFCHEADI%IVS == 200501) THEN
            IF (SFCHEADO%IVS < 200501 .AND.
     &          SFCHEADI%IVS >=  200501) THEN
              SFCHEADO%CLABSFC="  "
            END IF
          END IF
          if (latb .gt. 0 .and. idrt == 0) then
            call sfcio_alhead(sfcheado,latb=latb,iret=iret)
          else
            call sfcio_alhead(sfcheado,iret)
          endif
          IF(LSOIL.GT.0) THEN
            IF(LSOIL.EQ.2.OR.LSOIL.EQ.4)THEN
              SFCHEADO%LSOIL=LSOIL
              if (allocated(sfcheado%zsoil)) deallocate (sfcheado%zsoil)
              ALLOCATE(SFCHEADO%ZSOIL(SFCHEADO%LSOIL))
            ELSE
              PRINT '("  NUMBER OF SOIL LAYERS MUST BE 2 OR 4. ")'
              CALL ERREXIT(9)
            ENDIF
          ENDIF
          IF (SFCHEADO%IVS < 200501) THEN
            SFCHEADO%LSOIL=2 ! MUST USE 2 LAYERS.
          ELSE
            IF (SFCHEADO%LSOIL == 2) THEN
              SFCHEADO%ZSOIL =(/-.1, -2.0/)
            ELSE
              SFCHEADO%ZSOIL =(/-.1, -.4, -1.0, -2.0/)
            END IF
          END IF
!         ALLOCATE(SFCHEADO%LPL(SFCHEADO%LATB/2))
        ELSE
          NSFCO=0
        ENDIF
      ENDIF
       call instrument(16,kall,ttot,tmin,tmax)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CHANGE RESOLUTION OF SURFACE FILE
      CONVERT_SFC_FILE : IF(NSFCO.GT.0) THEN
        PRINT '("CHANGE SURFACE FILE RESOLUTION",
     &   " FROM ",I4," X ",I4," X ",I4,"   VERSION",I8)',
     &   SFCHEADI%LONB,SFCHEADI%LATB,SFCHEADI%LSOIL,SFCHEADI%IVS
        PRINT '("                              ",
     &   "   TO ",I4," X ",I4," X ",I4,"   VERSION",I8)',
     &   SFCHEADO%LONB,SFCHEADO%LATB,SFCHEADO%LSOIL,SFCHEADO%IVS
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ OLD SURFACE FILE
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
        IMI=SFCHEADI%LONB
        JMI=SFCHEADI%LATB
        IMO=SFCHEADO%LONB
        JMO=SFCHEADO%LATB

        allocate(geolon_in(IMI, JMI))
        allocate(geolat_in(IMI, JMI))
        IJMI = sum(SFCHEADI%LPL)*2
        print*, "IJMI = ", IJMI
        allocate(LONS_input(IJMI))
        allocate(LATS_input(IJMI))
        CALL LATLONS(JMI, IJMI, SFCHEADI%LPL,
     &             LATS_input, LONS_input, IDRT)
        CALL UNINTERPRED(1,KMSK,LONS_input,geolon_in,
     &                 IMI,JMI,IJMI,SFCHEADI%LPL)
        CALL UNINTERPRED(1,KMSK,LATS_input,geolat_in,
     &                 IMI,JMI,IJMI,SFCHEADI%LPL)
        deallocate(lons_input,lats_input)
        
        call instrument(17,kall,ttot,tmin,tmax)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET NEW LONSPERLAT
        IF(NLPL.GT.0) THEN
          READ(NLPL,*,IOSTAT=IOLPL) LATG2,SFCHEADO%LPL
          IF(LATG2.NE.SFCHEADO%LATB/2) IOLPL=99
        ELSE
          IOLPL=100
        ENDIF
        IF(IOLPL.EQ.0) THEN
          PRINT '("  NEW LONSPERLAT READ IN")'
        ELSE
          IF(SFCHEADO%LONB.EQ.SFCHEADI%LONB.AND.
     &      SFCHEADO%LATB.EQ.SFCHEADI%LATB.AND.IDRT.EQ.4) THEN
            SFCHEADO%LPL=SFCHEADI%LPL
            PRINT '("  NEW LONSPERLAT COPIED FROM OLD")'
          ELSE
            SFCHEADO%LPL=SFCHEADO%LONB
            PRINT '("  NEW LONSPERLAT ASSUMED TO BE UNIFORM")'
          ENDIF
        ENDIF
       call instrument(19,kall,ttot,tmin,tmax)

!     get grid information
      IJMO = SUM(SFCHEADO%LPL)*2      ! NUMBER OF GRID POINTS
                                      ! ACCOUNTING FOR REDUCED GRID. 
      if (idrt == 0) ijmo = imo * jmo
      allocate(geolon(LONB,LATB), geolat(LONB,LATB))
      ALLOCATE(SFCOUTPUT%LATS(IJMO))
      ALLOCATE(SFCOUTPUT%LONS(IJMO))

      IF(trim(OUTGRID) .NE. "") THEN
         inquire(file=trim(OUTGRID), exist=fexist)
         if(.not. fexist) then
            print*, "file "//trim(OUTGRID)//" does not exist"
            CALL ERREXIT(4)
         endif
         error=NF__OPEN(trim(OUTGRID),NF_NOWRITE,fsize,ncid)
         call netcdf_err(error, 'Open file '//trim(OUTGRID) )
         error=nf_inq_dimid(ncid, 'nx', id_dim)
         call netcdf_err(error, 'inquire dimension nx from file '//
     &                   trim(OUTGRID) )
         error=nf_inq_dimlen(ncid,id_dim,nx)
         call netcdf_err(error, 'inquire dimension nx length '//
     &       'from file '//trim(OUTGRID) )
         error=nf_inq_dimid(ncid, 'ny', id_dim)
         call netcdf_err(error, 'inquire dimension ny from file '//
     &                   trim(OUTGRID) )
         error=nf_inq_dimlen(ncid,id_dim,ny)
         call netcdf_err(error, 'inquire dimension ny length '//
     &       'from file '//trim(OUTGRID) )
!        LONB should equal nx/2 and LATB should equal ny/2
         if(LONB .ne. nx/2) then
            print*, "LONB=",LONB, " /= grid file lon=",nx/2
            CALL ERREXIT(4)
         endif
         if(LATB .ne. ny/2) then
            print*, "LATB=",LATB, " /= grid file lat=",ny/2
            CALL ERREXIT(4)
         endif

         print*, "Read the grid from file "//trim(OUTGRID)

         allocate(tmpvar(nx+1,ny+1))

         error=nf_inq_varid(ncid, 'x', id_var)
         call netcdf_err(error, 'inquire varid of x from file '
     &                   //trim(OUTGRID) )
         error=nf_get_var_double(ncid, id_var, tmpvar)
         call netcdf_err(error, 'inquire data of x from file '
     &                   //trim(OUTGRID) )
         geolon(1:LONB,1:LATB) = tmpvar(2:nx:2,2:ny:2)
         error=nf_inq_varid(ncid, 'y', id_var)
         call netcdf_err(error, 'inquire varid of y from file '
     &                   //trim(OUTGRID) )
         error=nf_get_var_double(ncid, id_var, tmpvar)
         call netcdf_err(error, 'inquire data of y from file '
     &                   //trim(OUTGRID) )
         geolat(1:LONB,1:LATB) = tmpvar(2:nx:2,2:ny:2)
         deallocate(tmpvar)

         allocate(tmpvar(IMO, JMO))
         tmpvar = geolon
         CALL INTERPRED(1,KMSK,tmpvar,SFCOUTPUT%LONS,
     &               IMO,JMO,IJMO,SFCHEADO%LPL)
         tmpvar = geolat
         CALL INTERPRED(1,KMSK,tmpvar,SFCOUTPUT%LATS,
     &               IMO,JMO,IJMO,SFCHEADO%LPL)

         deallocate(tmpvar)
      ELSE
        allocate(tmpvar(IMO, JMO))
        CALL LATLONS(JMO, IJMO, SFCHEADO%LPL,
     &             SFCOUTPUT%LATS, SFCOUTPUT%LONS, IDRT)

        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%LONS,tmpvar,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
        geolon = tmpvar
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%LATS,tmpvar,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
        geolat = tmpvar
        deallocate(tmpvar)
      ENDIF

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET OLD AND NEW LAND-SEA MASK.  REMOVE SEA ICE WHICH IS HANDLED 
C  SEPARATELY.
       ALLOCATE(SLMSKI(IMI,JMI))
       SLMSKI=SFCDATAI%SLMSK
       WHERE(NINT(SLMSKI).EQ.2) SLMSKI=0.
       ALLOCATE(SLMSKO(IMO,JMO))
       ALLOCATE(SLMSKO2(IMO,JMO))
       ALLOCATE(orog_raw(IMO,JMO))
       !-- read land-sea mask from OUTORIG if it exists
       outorog_exist = .false.
       IF(trim(OUTOROG) .NE. "") THEN
         inquire(file=trim(OUTOROG), exist=fexist)
         if(.not. fexist) then
            print*, "file "//trim(OUTOROG)//" does not exist"
            CALL ERREXIT(4)
         endif
         outorog_exist = .true.
         error=NF__OPEN(trim(OUTOROG),NF_NOWRITE,fsize,ncid_orog)
         call netcdf_err(error, 'Open file '//trim(OUTOROG) )
         error=nf_inq_dimid(ncid_orog, 'lon', id_dim)
         call netcdf_err(error, 'inquire dimension lon from file '//
     &                   trim(OUTOROG) )
         error=nf_inq_dimlen(ncid_orog,id_dim,nx)
         call netcdf_err(error, 'inquire dimension lon length '//
     &       'from file '//trim(OUTOROG) )
         error=nf_inq_dimid(ncid_orog, 'lat', id_dim)
         call netcdf_err(error, 'inquire dimension lat from file '//
     &                   trim(OUTOROG) )
         error=nf_inq_dimlen(ncid_orog,id_dim,ny)
         call netcdf_err(error, 'inquire dimension lat length '//
     &       'from file '//trim(OUTOROG) )
!        LONB should equal nx and LATB should equal ny
         if(LONB .ne. nx) then
            print*, "LONB=",LONB, " /= grid file lon=",nx
            CALL ERREXIT(4)
         endif
         if(LATB .ne. ny) then
            print*, "LATB=",LATB, " /= grid file lat=",ny
            CALL ERREXIT(4)
         endif
         print*, "Read the land-sea mask from file "//trim(OUTOROG)
         error=nf_inq_varid(ncid_orog, 'slmsk', id_var)
         call netcdf_err(error, 'inquire varid of slmsk from file '
     &                   //trim(OUTOROG) )
         error=nf_get_var_double(ncid_orog, id_var, SLMSKO)
         call netcdf_err(error, 'inquire data of slmsk from file '
     &                   //trim(OUTOROG) )
                 
       ELSE
         IF(NSLM.GT.0) THEN
           JPDS=-1
           JPDS(5)=81
           ALLOCATE(BITMAP(IMO*JMO))
           CALL GETGB(NSLM,0,IMO*JMO,0,JPDS,JGDS,
     &               KF,K,KPDS,KGDS,BITMAP,SLMSKO,IOSLM)
           DEALLOCATE(BITMAP)
           IF(IOSLM .EQ. 0 .AND. (KGDS(1) .NE. idrt .OR.
     &      KGDS(2) .NE. IMO .OR. KGDS(3) .NE. JMO)) IOSLM=100
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
!          CALL GL2GL(2,1,SLMSKI,IMI,JMI,SLMSKO,IMO,JMO,4,IDRT)
           CALL GL2ANY(2,1,SLMSKI,IMI,JMI,SLMSKO,IMO,JMO,4,
     &         SFCOUTPUT%LONS,SFCOUTPUT%LATS)
           PRINT '("  NEW LAND-SEA MASK INTERPOLATED FROM OLD")'
         ENDIF
       ENDIF
        SLMSKO=NINT(SLMSKO)

       call instrument(18,kall,ttot,tmin,tmax)


       IF(ALL(SFCDATAI%OROG.LE.0)) THEN
         IF (ALLOCATED(OROGI)) THEN
           CALL SPTEZ(0,SIGHEADI%JCAP,IDRT,SFCHEADI%LONB,SFCHEADI%LATB,
     &                HSI,SFCDATAI%OROG,+1)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  OLD SURFACE FILES DO NOT HAVE OROG.  NEED TO GET THIS FROM
C  A SIGMA FILE. 
         ELSE
           PRINT '("  NEED OROG ON INPUT GRID FROM SIGMA FILE")'
           CALL ERREXIT(10)
         END IF  
       ENDIF

       if( outorog_exist ) THEN
         print*, "Read the orogophy from file "//trim(OUTOROG)
         error=nf_inq_varid(ncid_orog, 'orog_filt', id_var)
         call netcdf_err(error, 'inquire varid of orog_filt from file '
     &                   //trim(OUTOROG) )
         error=nf_get_var_double(ncid_orog, id_var, SFCDATAO%OROG)
         call netcdf_err(error, 'inquire data of orog_filt from file '
     &                   //trim(OUTOROG) )

         error=nf_inq_varid(ncid_orog, 'orog_raw', id_var)
         call netcdf_err(error, 'inquire varid of orog_raw from file '
     &                   //trim(OUTOROG) )
         error=nf_get_var_real(ncid_orog, id_var, orog_raw)
         call netcdf_err(error, 'inquire data of orog_raw from file '
     &                   //trim(OUTOROG) )

         error = nf_close(ncid_orog)
         call netcdf_err(error, 'close file='//trim(outorog) )
         
       ELSE
         IF(ALLOCATED(OROGO)) THEN
           SFCDATAO%OROG=OROGO
         ELSE
           CALL BAOPENR(NORO,'chgres.inp.orogb',IRET)
           IF (IRET == 0) THEN  ! orog file exists.
             JPDS=-1
             JPDS(5)=8
             PRINT '("  READ OROGRAPHY ON OUTPUT GRID")'
             ALLOCATE(BITMAP(IMO*JMO))
             CALL GETGB(NORO,0,IMO*JMO,0,JPDS,JGDS,
     &                  KF,K,KPDS,KGDS,BITMAP,SFCDATAO%OROG,IRET)
             DEALLOCATE(BITMAP)
             IF (IRET /= 0) THEN  ! bad read. abort here?
               PRINT '("  BAD READ OF OUTPUT GRID OROGRAPHY GRID FILE")'
              PRINT '("  INTERPOLATE OUTPUT OROGRAPHY FROM INPUT GRID")'
!            CALL GL2GL(2,1,SFCDATAI%OROG,IMI,JMI,SFCDATAO%OROG,IMO,JMO
!     &,                                                         4,IDRT)
             CALL GL2ANY(2,1,SFCDATAI%OROG,IMI,JMI,SFCDATAO%OROG,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
             END IF
             CALL BACLOSE(NORO,IRET)
           ELSE
             PRINT '("  INTERPOLATE OUTPUT OROGRAPHY FROM INPUT GRID")'
!             CALL GL2GL(2,1,SFCDATAI%OROG,IMI,JMI,SFCDATAO%OROG,IMO,JMO
!     &,                                                         4,IDRT)
             CALL GL2ANY(2,1,SFCDATAI%OROG,IMI,JMI,SFCDATAO%OROG,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
           END IF
         ENDIF
         orog_raw = SFCDATAO%OROG
       ENDIF
!     
!     Open and read unfiltered orography
!
       if (use_ufo) then
        ALLOCATE(OROGo_uf(IMO,JMO))
        if( outorog_exist ) THEN
         orogo_uf = orog_raw
        else
           
         CALL BAOPENR(NORO_uf,'chgres.inp.orogb_uf',IRET)
         IF(IRET.NE.0) NORO_uf=0
!
         JPDS    = -1
         JPDS(5) = 8
         if (noro_uf > 0) then
           ALLOCATE(BITMAP(IMO*JMO))
           CALL GETGB(NORO_uf,0,IMO*JMO,0,JPDS,JGDS,
     &              KF,K,KPDS,KGDS,BITMAP,OROGO_uf,IOSORO_uf)
           DEALLOCATE(BITMAP)
         endif
         IF(IOSORO_uf == 0 .AND. (KGDS(1) /= IDRT .OR.
     &     KGDS(2) /= IMO .OR. KGDS(3) /= JMO)) IOSORO_uf = 100
         IF(IOSORO_uf == 0) THEN
           PRINT '("  NEW unfiltered OROGRAPHY READ IN")'
           if (kgds(4) == -90000 .and. kgds(5) == -180000) then
             print *,' reversing the lat/lon for orography'
             call REVERS(imo, jmo, OROGO_uf)
           endif
           orog_raw = orogo_uf
         ELSE
           use_ufo = .false.
         ENDIF
        endif
       endif
       if (.not. use_ufo) then
         if (.not. allocated (OROGo_uf)) ALLOCATE(OROGo_uf(IMO,JMO))
         orogo_uf = 0.0
         PRINT '("  unfiltered  OROGRAPHY set to zero")'
       endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INTERPOLATE SOME SURFACE FIELDS THE OLD WAY.  THESE ARE FIELDS
C  THAT ARE EITHER DIAGNOSTIC OR DO NOT REQUIRE SPECIAL HANDLING
C  BY THE NEW SURFACE CHGRES LOGIC.   
!        CALL GL2GL(2,1,SFCDATAI%CV,IMI,JMI,SFCDATAO%CV,IMO,JMO
!     &,                                                         4,IDRT)
!        CALL GL2GL(2,1,SFCDATAI%CVB,IMI,JMI,SFCDATAO%CVB,IMO,JMO
!     &,                                                         4,IDRT)
!        CALL GL2GL(2,1,SFCDATAI%CVT,IMI,JMI,SFCDATAO%CVT,IMO,JMO
!     &,                                                         4,IDRT)
!        CALL GL2GL(2,1,SFCDATAI%F10M,IMI,JMI,SFCDATAO%F10M,IMO,JMO
!     &,                                                         4,IDRT)
!        CALL GL2GL(2,1,SFCDATAI%T2M,IMI,JMI,SFCDATAO%T2M,IMO,JMO
!     &,                                                         4,IDRT)
!        CALL GL2GL(2,1,SFCDATAI%Q2M,IMI,JMI,SFCDATAO%Q2M,IMO,JMO
!     &,                                                         4,IDRT)
!        CALL GL2GL(2,1,SFCDATAI%UUSTAR,IMI,JMI,SFCDATAO%UUSTAR,IMO,JMO
!     &,                                                         4,IDRT)
!        CALL GL2GL(2,1,SFCDATAI%FFMM,IMI,JMI,SFCDATAO%FFMM,IMO,JMO
!     &,                                                         4,IDRT)
!        CALL GL2GL(2,1,SFCDATAI%FFHH,IMI,JMI,SFCDATAO%FFHH,IMO,JMO
!     &,                                                         4,IDRT)

        CALL GL2ANY(2,1,SFCDATAI%CV,IMI,JMI,SFCDATAO%CV,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
        CALL GL2ANY(2,1,SFCDATAI%CVB,IMI,JMI,SFCDATAO%CVB,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
        CALL GL2ANY(2,1,SFCDATAI%CVT,IMI,JMI,SFCDATAO%CVT,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
        CALL GL2ANY(2,1,SFCDATAI%F10M,IMI,JMI,SFCDATAO%F10M,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
        CALL GL2ANY(2,1,SFCDATAI%T2M,IMI,JMI,SFCDATAO%T2M,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
        CALL GL2ANY(2,1,SFCDATAI%Q2M,IMI,JMI,SFCDATAO%Q2M,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
        CALL GL2ANY(2,1,SFCDATAI%UUSTAR,IMI,JMI,SFCDATAO%UUSTAR,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
        CALL GL2ANY(2,1,SFCDATAI%FFMM,IMI,JMI,SFCDATAO%FFMM,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
        CALL GL2ANY(2,1,SFCDATAI%FFHH,IMI,JMI,SFCDATAO%FFHH,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PRIOR TO IVS 200501, THESE FIELDS WERE UNDEFINED.  IF OUTPUTTING AN
C  200501 FILE, NEED TO INITIALIZE THESE TO A VALUE.  
!       IF(SFCHEADI%IVS.NE.200501.AND.SFCHEADO%IVS.EQ.200501)THEN
        IF(SFCHEADI%IVS.LT.200501.AND.SFCHEADO%IVS.GE.200501)THEN
          SFCDATAO%TPRCP  = 0. ! SET PRECIP TO ZERO.
          SFCDATAO%SRFLAG = 0. ! SET PRECIP TYPE FLAG TO ZERO (LIQUID).
        ELSE
!         CALL GL2GL(2,1,SFCDATAI%TPRCP,IMI,JMI,SFCDATAO%TPRCP,IMO,JMO
!     &,                                                         4,IDRT)
!         CALL GL2GL(2,1,SFCDATAI%SRFLAG,IMI,JMI,SFCDATAO%SRFLAG,IMO,JMO
!     &,                                                         4,IDRT)

         CALL GL2ANY(2,1,SFCDATAI%TPRCP,IMI,JMI,SFCDATAO%TPRCP,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
         CALL GL2ANY(2,1,SFCDATAI%SRFLAG,IMI,JMI,SFCDATAO%SRFLAG,IMO,JMO
     &,                                 4,SFCOUTPUT%LONS,SFCOUTPUT%LATS)
        END IF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PLACE SURFACE DATA INTO DATA STRUCTURE EXPECTED BY SURFACE
C  CHGRES CODE.  FIRST, DO INPUT DATA. 
      ALLOCATE (SFCINPUT%ALNSF(IMI,JMI))
      SFCINPUT%ALNSF=SFCDATAI%ALNSF
      ALLOCATE (SFCINPUT%ALNWF(IMI,JMI))
      SFCINPUT%ALNWF=SFCDATAI%ALNWF
      ALLOCATE (SFCINPUT%ALVSF(IMI,JMI))
      SFCINPUT%ALVSF=SFCDATAI%ALVSF
      ALLOCATE (SFCINPUT%ALVWF(IMI,JMI))
      SFCINPUT%ALVWF=SFCDATAI%ALVWF
      ALLOCATE (SFCINPUT%CANOPY_MC(IMI,JMI))
      SFCINPUT%CANOPY_MC=SFCDATAI%CANOPY
      ALLOCATE (SFCINPUT%GREENFRC(IMI,JMI))
      SFCINPUT%GREENFRC=SFCDATAI%VFRAC
      ALLOCATE (SFCINPUT%FACSF(IMI,JMI))
      SFCINPUT%FACSF=SFCDATAI%FACSF
      ALLOCATE (SFCINPUT%FACWF(IMI,JMI))
      SFCINPUT%FACWF=SFCDATAI%FACWF
      ALLOCATE (SFCINPUT%SKIN_TEMP(IMI,JMI))
      SFCINPUT%SKIN_TEMP=SFCDATAI%TSEA
      ALLOCATE (SFCINPUT%LSMASK(IMI,JMI))
      SFCINPUT%LSMASK=SLMSKI
      ALLOCATE (SFCINPUT%SEA_ICE_FLAG(IMI,JMI))
      SFCINPUT%SEA_ICE_FLAG = 0
      WHERE (SFCDATAI%SLMSK > 1.99) SFCINPUT%SEA_ICE_FLAG = 1
      ALLOCATE (SFCINPUT%SNOW_LIQ_EQUIV(IMI,JMI))
      SFCINPUT%SNOW_LIQ_EQUIV=SFCDATAI%SHELEG
      ALLOCATE (SFCINPUT%Z0(IMI,JMI))
      SFCINPUT%Z0=SFCDATAI%ZORL
      ALLOCATE (SFCINPUT%OROG(IMI,JMI))
      SFCINPUT%OROG=SFCDATAI%OROG
      ALLOCATE (SFCINPUT%VEG_TYPE(IMI,JMI))
      SFCINPUT%VEG_TYPE = NINT(SFCDATAI%VTYPE)
      ALLOCATE (SFCINPUT%SOIL_TYPE(IMI,JMI))
      SFCINPUT%SOIL_TYPE = NINT(SFCDATAI%STYPE)
      ALLOCATE (SFCINPUT%SOILM_TOT(IMI,JMI,SFCHEADI%LSOIL))
      SFCINPUT%SOILM_TOT = SFCDATAI%SMC
      ALLOCATE (SFCINPUT%SOIL_TEMP(IMI,JMI,SFCHEADI%LSOIL))
      SFCINPUT%SOIL_TEMP = SFCDATAI%STC
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  THE 200501 VERSION OF THE SURFACE FILE HAS ADDITIONAL FIELDS FOR
C  USE BY NOAH LSM AND NEW SEA ICE PHYSICS, WHILE OLDER VERSIONS
C  DO NOT.  IF THESE VARIABLES ARE NOT ALLOCATED, THE SURFACE CHGRES
C  CODE WILL NOT INTERPOLATE THEM.
      IF (SFCHEADI%IVS >= 200501) then
        ALLOCATE (SFCINPUT%SEA_ICE_FRACT(IMI,JMI))
        SFCINPUT%SEA_ICE_FRACT=SFCDATAI%FICE
        ALLOCATE (SFCINPUT%SEA_ICE_DEPTH(IMI,JMI))
        SFCINPUT%SEA_ICE_DEPTH=SFCDATAI%HICE
        ALLOCATE (SFCINPUT%MXSNOW_ALB(IMI,JMI))
        SFCINPUT%MXSNOW_ALB=SFCDATAI%SNOALB
        ALLOCATE (SFCINPUT%SNOW_DEPTH(IMI,JMI))
        SFCINPUT%SNOW_DEPTH=SFCDATAI%SNWDPH
        ALLOCATE (SFCINPUT%SLOPE_TYPE(IMI,JMI))
        SFCINPUT%SLOPE_TYPE = NINT(SFCDATAI%SLOPE)
        ALLOCATE (SFCINPUT%GREENFRC_MAX(IMI,JMI))
        SFCINPUT%GREENFRC_MAX=SFCDATAI%SHDMAX
        ALLOCATE (SFCINPUT%GREENFRC_MIN(IMI,JMI))
        SFCINPUT%GREENFRC_MIN=SFCDATAI%SHDMIN
        ALLOCATE (SFCINPUT%SOILM_LIQ(IMI,JMI,SFCHEADI%LSOIL))
        SFCINPUT%SOILM_LIQ = SFCDATAI%SLC
      END IF
      CALL SFCIO_AXDBTA(SFCDATAI,IRET)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  NOW ALLOCATE OUTPUT DATA STRUCTURE.  SURFACE CHGRES LOGIC NEEDS
C  THE LAND MASK AND OROGRAPHY ON OUTPUT GRID.  USE THE LONSPERLAT
C  INFO TO RUN EITHER THE REDUCED OR FULL GRID.
      IJMO = SUM(SFCHEADO%LPL)*2      ! NUMBER OF GRID POINTS
                                      ! ACCOUNTING FOR REDUCED GRID. 
      if (idrt == 0) ijmo = imo * jmo
      ALLOCATE(SFCOUTPUT%OROG(IJMO))
      ALLOCATE (KMSK(IMO,JMO))
      ALLOCATE(OROGO_uf2(IMO,JMO))
      KMSK = 0
      CALL INTERPRED(1,KMSK,SFCDATAO%OROG,SFCOUTPUT%OROG,
     &               IMO,JMO,IJMO,SFCHEADO%LPL)

      CALL INTERPRED(1,KMSK,OROGO_uf,OROGO_uf2,
     &               IMO,JMO,IJMO,SFCHEADO%LPL)

      ALLOCATE(SFCOUTPUT%LSMASK(IJMO))
      CALL INTERPRED(1,KMSK,SLMSKO,SFCOUTPUT%LSMASK,
     &               IMO,JMO,IJMO,SFCHEADO%LPL)

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
      ALLOCATE(SFCOUTPUT%SOILM_TOT(IJMO,SFCHEADO%LSOIL))
      ALLOCATE(SFCOUTPUT%SOIL_TEMP(IJMO,SFCHEADO%LSOIL))
      ALLOCATE(SFCOUTPUT%VEG_TYPE(IJMO))
      ALLOCATE(SFCOUTPUT%SOIL_TYPE(IJMO))
      ALLOCATE(SFCOUTPUT%SEA_ICE_FLAG(IJMO))
      IF (SFCHEADO%IVS >= 200501) THEN
        ALLOCATE(SFCOUTPUT%SLOPE_TYPE(IJMO))
        ALLOCATE(SFCOUTPUT%SEA_ICE_FRACT(IJMO))
        ALLOCATE(SFCOUTPUT%SEA_ICE_DEPTH(IJMO))
        ALLOCATE(SFCOUTPUT%SOILM_LIQ(IJMO,SFCHEADO%LSOIL))
        ALLOCATE(SFCOUTPUT%SNOW_DEPTH(IJMO))
        ALLOCATE(SFCOUTPUT%MXSNOW_ALB(IJMO))
        ALLOCATE(SFCOUTPUT%GREENFRC_MAX(IJMO))
        ALLOCATE(SFCOUTPUT%GREENFRC_MIN(IJMO))
      END IF
      IF (SFCHEADO%IVS >= 200509) then
        ALLOCATE (SFCOUTPUT%SEA_ICE_TEMP(IJMO))
      END IF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INFO DESCRIBING BOTH GRIDS.
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

      KGDS_OUTPUT = -1
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
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALL SURFACE CHGRES DRIVER.
      FCSTHOUR = SFCHEADI%FHOUR
      CALL SURFACE_CHGRES_DRIVER(IMO,JMO,IJMO,SFCHEADO%LSOIL,
     &                           SFCHEADO%LPL,
     &                           KGDS_OUTPUT,SFCOUTPUT,IMI,JMI,
     &                           orogo_uf2,use_ufo,
     &                           SFCHEADI%LSOIL,
     &                           SFCHEADI%IDATE(1),
     &                           SFCHEADI%IDATE(2), SFCHEADI%IDATE(3),
     &                           SFCHEADI%IDATE(4), FCSTHOUR,
     &                           KGDS_INPUT, SFCINPUT, IALB,
     &                           MERGE, IRET, geolon_in, geolat_in)
      IF (IRET .NE. 0) THEN
        PRINT '("  ERROR IN SURFACE CHGRES DRIVER ")'
        CALL ERREXIT(34)
      END IF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GO FROM REDUCED, 1-D ARRAYS TO 2-D FOR OUTPUT.
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%LSMASK,SLMSKO,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%OROG,SFCDATAO%OROG,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%ALNSF,SFCDATAO%ALNSF,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%ALNWF,SFCDATAO%ALNWF,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%ALVSF,SFCDATAO%ALVSF,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%ALVWF,SFCDATAO%ALVWF,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%GREENFRC,SFCDATAO%VFRAC,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      IF (ALLOCATED(SFCOUTPUT%GREENFRC_MAX)) THEN
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%GREENFRC_MAX,SFCDATAO%SHDMAX,
     &                   IMO,JMO,IJMO,SFCHEADO%LPL)
      END IF
      IF (ALLOCATED(SFCOUTPUT%GREENFRC_MIN)) THEN
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%GREENFRC_MIN,SFCDATAO%SHDMIN,
     &                   IMO,JMO,IJMO,SFCHEADO%LPL)
      END IF
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%Z0,SFCDATAO%ZORL,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
!      call hhmaxmin(SFCOUTPUT%SUBSTRATE_TEMP,IMO,jmo,jmo,1,' TG3  ' )
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SUBSTRATE_TEMP,SFCDATAO%TG3,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      IF (ALLOCATED (SFCOUTPUT%MXSNOW_ALB)) THEN
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%MXSNOW_ALB,SFCDATAO%SNOALB,
     &                   IMO,JMO,IJMO,SFCHEADO%LPL)
      END IF
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%CANOPY_MC,SFCDATAO%CANOPY,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      IF (ALLOCATED(SFCOUTPUT%SEA_ICE_FRACT)) THEN
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SEA_ICE_FRACT,SFCDATAO%FICE,
     &                   IMO,JMO,IJMO,SFCHEADO%LPL)
      END IF
      IF (ALLOCATED(SFCOUTPUT%SEA_ICE_DEPTH)) THEN
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SEA_ICE_DEPTH,SFCDATAO%HICE,
     &                   IMO,JMO,IJMO,SFCHEADO%LPL)
      END IF
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%FACSF,SFCDATAO%FACSF,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%FACWF,SFCDATAO%FACWF,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SKIN_TEMP,SFCDATAO%TSEA,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SNOW_LIQ_EQUIV,SFCDATAO%SHELEG,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      IF (ALLOCATED (SFCOUTPUT%SNOW_DEPTH)) THEN
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SNOW_DEPTH,SFCDATAO%SNWDPH,
     &                   IMO,JMO,IJMO,SFCHEADO%LPL)
      END IF
      DO K=1, SFCHEADO%LSOIL
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SOILM_TOT(:,K),
     &                   SFCDATAO%SMC(:,:,K),
     &                   IMO,JMO,IJMO,SFCHEADO%LPL)
        IF (ALLOCATED(SFCOUTPUT%SOILM_LIQ)) THEN
          CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SOILM_LIQ(:,K),  
     &                     SFCDATAO%SLC(:,:,K),
     &                     IMO,JMO,IJMO,SFCHEADO%LPL)
        END IF
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SOIL_TEMP(:,K), 
     &                   SFCDATAO%STC(:,:,K),
     &                   IMO,JMO,IJMO,SFCHEADO%LPL)
      ENDDO
!
      IF (ALLOCATED(SFCOUTPUT%SEA_ICE_TEMP)) THEN
        CALL UNINTERPRED(1,KMSK,SFCOUTPUT%SEA_ICE_TEMP,SFCDATAO%TISFC,
     &                   IMO,JMO,IJMO,SFCHEADO%LPL)
      END IF
!
      ALLOCATE(DUMMY(IJMO))
      IF (ALLOCATED(SFCOUTPUT%SLOPE_TYPE)) THEN
        DUMMY=FLOAT(SFCOUTPUT%SLOPE_TYPE)
        CALL UNINTERPRED(1,KMSK,DUMMY,SFCDATAO%SLOPE,
     &                   IMO,JMO,IJMO,SFCHEADO%LPL)
      END IF
      DUMMY=FLOAT(SFCOUTPUT%SOIL_TYPE)
      CALL UNINTERPRED(1,KMSK,DUMMY,SFCDATAO%STYPE,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      DUMMY=FLOAT(SFCOUTPUT%VEG_TYPE)
      CALL UNINTERPRED(1,KMSK,DUMMY,SFCDATAO%VTYPE,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      DUMMY=FLOAT(SFCOUTPUT%SEA_ICE_FLAG)
      ALLOCATE(DUMMY2(IMO,JMO))
      CALL UNINTERPRED(1,KMSK,DUMMY,DUMMY2,
     &                 IMO,JMO,IJMO,SFCHEADO%LPL)
      WHERE (NINT(DUMMY2)==1)SLMSKO=2.0
      SFCDATAO%SLMSK=SLMSKO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FREE UP MEMORY.
      DEALLOCATE (DUMMY2)
      DEALLOCATE (DUMMY)
      DEALLOCATE (KMSK)
      IF (DO_NSST) THEN
        ALLOCATE(KMSK(IMO,JMO))
        KMSK=0
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
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  WRITE THE NEW SURFACE FILE
        DEALLOCATE(SLMSKI)
        DEALLOCATE(SLMSKO)
        CALL SFCIO_SWHEAD(NSFCO,SFCHEADO,IRET)
        call write_sfc_head('sfc_ctrl.nc',SFCHEADO%ivs,SFCHEADO%lsoil
     &,     SFCHEADO%fhour,SFCHEADO%idate,SFCHEADO%lonb,SFCHEADO%latb
     &,      SFCHEADO%irealf )
        CALL SFCIO_SWDBTA(NSFCO,SFCHEADO,SFCDATAO,IRET,geolon,geolat
     &,                   orog_raw)
        CALL SFCIO_AXDBTA(SFCDATAO,IRET)
        CALL SFCIO_SCLOSE(NSFCI,IRET)
        IF(NSLM.GT.0) CALL BACLOSE(NSLM,IRET)
        IF(NLPL.GT.0) CLOSE(NLPL)
        CALL SFCIO_SCLOSE(NSFCO,IRET)
        call instrument(21,kall,ttot,tmin,tmax)
        CONVERT_NSST_FILE : IF (DO_NSST) THEN
          print*,'- CONVERT NSST FILE chgres.inp.nst.'
C  OPEN NSST FILES
          NSSTI=31
          NSSTO=32
          CALL NSTIO_SROPEN(NSSTI,'chgres.inp.nst',IRET)
          IF(IRET/=0)THEN
            print*,'error opening chgres.inp.nst ', iret
            stop 334
          ENDIF
          CALL NSTIO_SRHEAD(NSSTI,NSST_IN_HEAD,IRET)
          IF(IRET/=0)THEN
            print*,'error reading chgres.inp.nst ', iret
            stop 335
          ENDIF
          CALL NSTIO_ALDATA(NSST_IN_HEAD,NSST_IN_DATA,IRET)
          CALL NSTIO_SRDATA(NSSTI,NSST_IN_HEAD,NSST_IN_DATA,IRET)
          IF(IRET/=0)THEN
            print*,'error reading chgres.inp.nst ', iret
            stop 336
          ENDIF
          CALL NSST_CHGRES(IMI,JMI,MASK_OUTPUT,
     &                     IJMO,KGDS_INPUT, NSST_IN_DATA, 
     &                     NSST_OUTPUT_THIN, NUM_NSST_FIELDS,
     &                     KGDS_OUTPUT, 
     &                     RLATS_OUTPUT,RLONS_OUTPUT)
          DEALLOCATE(RLATS_OUTPUT, RLONS_OUTPUT)
          CALL NSTIO_AXDATA(NSST_IN_DATA,IRET)
          CALL NSTIO_SRCLOSE(NSSTI,IRET)
          NSST_OUT_HEAD%CLABNST=NSST_IN_HEAD%CLABNST
          NSST_OUT_HEAD%FHOUR=NSST_IN_HEAD%FHOUR
          NSST_OUT_HEAD%IDATE=NSST_IN_HEAD%IDATE
          NSST_OUT_HEAD%LSEA=NSST_IN_HEAD%LSEA
          NSST_OUT_HEAD%IVO=NSST_IN_HEAD%IVO
          NSST_OUT_HEAD%IREALF=NSST_IN_HEAD%IREALF
          NSST_OUT_HEAD%LONB=SFCHEADO%LONB
          NSST_OUT_HEAD%LATB=SFCHEADO%LATB
          CALL NSTIO_ALHEAD(NSST_OUT_HEAD,IRET)
          NSST_OUT_HEAD%LPL=SFCHEADO%LPL
          CALL NSTIO_ALDATA(NSST_OUT_HEAD,NSST_OUT_DATA,IRET)
          IF(IRET/=0)THEN
            print*,'error allocating nsst output data structure'
            stop 337
          ENDIF
          ALLOCATE(DUMMY2(IMO,JMO))
          CALL UNINTERPRED(1,KMSK,MASK_OUTPUT,DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          DEALLOCATE(MASK_OUTPUT)
          NSST_OUT_DATA%SLMSK=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,1),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%XT=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,2),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%XS=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,3),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%XU=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,4),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%XV=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,5),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%XZ=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,6),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%ZM=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,7),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%XTTS=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,8),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%XZTS=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,9),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%DT_COOL=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,10),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%Z_C=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,11),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%C_0=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,12),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%C_D=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,13),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%W_0=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,14),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%W_D=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,15),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%D_CONV=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,16),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%IFD=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,17),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%TREF=DUMMY2
          CALL UNINTERPRED(1,KMSK,NSST_OUTPUT_THIN(:,18),DUMMY2,
     &                     IMO,JMO,IJMO,NSST_OUT_HEAD%LPL)
          NSST_OUT_DATA%QRAIN=DUMMY2
          DEALLOCATE(NSST_OUTPUT_THIN)
          CALL NSTIO_SWOHDC(NSSTO,'chgres.out.nst',
     &                      NSST_OUT_HEAD,NSST_OUT_DATA,IRET)
          IF(IRET/=0)THEN
            print*,'error writing chgres.out.nst ',iret
            stop 339
          ENDIF
          CALL NSTIO_AXDATA(NSST_OUT_DATA,IRET)
          DEALLOCATE(KMSK)
        ENDIF CONVERT_NSST_FILE
      ENDIF CONVERT_SFC_FILE
      
      ENDIF  ! end of make_sfc 
!
      call gfsio_finalize()
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL W3TAGE('GLOBAL_CHGRES')
       do k=1,30
       call instrument(-k,kall,ttot,tmin,tmax)
       print '(2i6,3f12.3)',k,kall,ttot,tmin,tmax
       enddo
      END
C-----------------------------------------------------------------------
      SUBROUTINE NEWSIG(NSIL,IDVC,LEVS,NVCOORD,VCOORD,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: NEWSIG         GET NEW SIGMA STRUCTURE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 98-04-03
C
C ABSTRACT: READ IN INTERFACE SIGMA VALUES (OR USE OLD VALUES)
C   AND COMPUTE FULL SIGMA VALUES.
C
C PROGRAM HISTORY LOG:
C   98-04-03  IREDELL
C
C USAGE:    CALL NEWSIG(NSIL,IDVC,LEVS,NVCOORD,VCOORD,IRET)
C   INPUT ARGUMENTS:
C     NSIL         INTEGER UNIT NUMBER OF NEW SIGMA INTERFACE VALUES
C     IDVC         INTEGER VERTICAL COORDINATE ID
C     LEVS         INTEGER NEW NUMBER OF LEVELS
C     NVCOORD      INTEGER NEW NUMBER OF VERTICAL COORDINATES
C   OUTPUT ARGUMENTS:
C     VCOORD       REAL (LEVS+1,NVCOORD) NEW VERTICAL COORDINATES
C     IRET         INTEGER RETURN CODE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
CC$$$
      REAL VCOORD(LEVS+1,NVCOORD)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ VERTICAL COORDINATES
      READ(NSIL,*,IOSTAT=IRET) IDVCI,LEVSI,NVCOORDI
      write(0,*)' IDVCI=',IDVCI,' LEVSI=',LEVSI,' NVCOORDI=',NVCOORDI
      IF(IRET == 0) THEN
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ INTERFACE HYBRID VALUES
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
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ INTERFACE SIGMA VALUES
        ELSE
          VCOORD(1,1)      = 1.
          VCOORD(LEVS+1,1) = 0.
          READ(NSIL,*,IOSTAT=IRET) LEVSI
          READ(NSIL,*,IOSTAT=IRET) (VCOORD(K,1),K=2,LEVS)
          IF(IRET.NE.0) RETURN
          IF(LEVSI.NE.LEVS) IRET = 28
          IF(IRET.NE.0) RETURN
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENDIF
      IRET=0
      END
C-----------------------------------------------------------------------
      SUBROUTINE TRSSC(JCAP,NC,KM,NTRAC,IDVM,
     &                 IDRT,LONB,LATB,IJN,J1,J2,JC,
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
C    &                 IDRT,LONB,LATB,IJN,J1,J2,JC,
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
      REAL DTDP2(IX,KM2)
      PARAMETER(DLTDZ=-6.5E-3*287.05/9.80665)
      PARAMETER(DLPVDRT=-2.5E6/461.50)
      REAL Z1(IM+1,KM1),Z2(IM+1,KM2)
      REAL C1(IM+1,KM1,4+NT),C2(IM+1,KM2,4+NT),J2(IM+1,KM2,4+NT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE LOG PRESSURE INTERPOLATING COORDINATE
C  AND COPY INPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS
!$OMP PARALLEL DO DEFAULT(SHARED)
!$OMP+ PRIVATE(K,I)
      DO K=1,KM1
        DO I=1,IM
          Z1(I,K)=-LOG(P1(I,K))
          C1(I,K,1)=U1(I,K)
          C1(I,K,2)=V1(I,K)
          C1(I,K,3)=W1(I,K)
          C1(I,K,4)=T1(I,K)
          C1(I,K,5)=Q1(I,K,1)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
      DO N=2,NT
        DO K=1,KM1
          DO I=1,IM
            C1(I,K,4+N)=Q1(I,K,N)
          ENDDO
        ENDDO
      ENDDO
!      print *,' p2=',p2(1,:)
!      print *,' im=',im,' km2=',km2,' ix=',ix,'nt=',nt
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
      CALL TERP3(IM,1,1,1,1,4+NT,(IM+1)*KM1,(IM+1)*KM2,
     &           KM1,IM+1,IM+1,Z1,C1,KM2,IM+1,IM+1,Z2,C2,J2)
!     print *,' c2=',c2(1,:,:)
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
            DTDP2(I,K)=-J2(I,K,4)/P2(I,K)
          ELSE
            T2(I,K)=T1(I,1)*EXP(DLTDZ*DZ)
            Q2(I,K,1)=Q1(I,1,1)*EXP(DLPVDRT*(1/T2(I,K)-1/T1(I,1))-DZ)
            DTDP2(I,K)=-T2(I,K)*DLTDZ/P2(I,K)
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
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE GL2GL(IP,KM,G1,IM1,JM1,G2,IM2,JM2,IDRTI,IDRTO)
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
      REAL G1(IM1,JM1,KM),G2(IM2,JM2,KM)
      LOGICAL*1 L1(IM1,JM1,KM),L2(IM2,JM2,KM)
      REAL RLAT(IM2,JM2),RLON(IM2,JM2)
      INTEGER IB1(KM),IB2(KM)
      INTEGER KGDS1(200),KGDS2(200)
      INTEGER IDRTI, IDRTO
      DATA KGDS1/4,0,0,90000,0,0,-90000,193*0/
      DATA KGDS2/4,0,0,90000,0,0,-90000,193*0/
      INTEGER IPOPT(20)
      DATA IPOPT/20*0/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      KGDS1(1) = IDRTI
      KGDS2(1) = IDRTO
      IF(IM1.NE.IM2.OR.JM1.NE.JM2) THEN
        IB1=0
        KGDS1(2)=IM1
        KGDS1(3)=JM1
        KGDS1(8)=NINT(-360000./IM1)
        KGDS1(10)=JM1/2
        KGDS2(2)=IM2
        KGDS2(3)=JM2
        KGDS2(8)=NINT(-360000./IM2)
        KGDS2(10)=JM2/2
        CALL IPOLATES(IP,IPOPT,KGDS1,KGDS2,IM1*JM1,IM2*JM2,KM,IB1,L1,G1,
     &                NO,RLAT,RLON,IB2,L2,G2,IRET)
      ELSE
        G2=G1
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE GL2ANY(IP,KM,G1,IM1,JM1,G2,IM2,JM2,IDRTI,RLON,RLAT)
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
      REAL G1(IM1,JM1,KM),G2(IM2,JM2,KM)
      LOGICAL*1 L1(IM1,JM1,KM),L2(IM2,JM2,KM)
      REAL, intent(in) :: RLAT(IM2,JM2),RLON(IM2,JM2)
      INTEGER IB1(KM),IB2(KM)
      INTEGER KGDS1(200),KGDS2(200)
      INTEGER IDRTI, IDRTO
      DATA KGDS1/4,0,0,90000,0,0,-90000,193*0/
      DATA KGDS2/4,0,0,90000,0,0,-90000,193*0/
      INTEGER IPOPT(20)
      DATA IPOPT/20*0/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      KGDS1(1) = IDRTI
      KGDS2(1) = -1
      NO = IM2*JM2
      IF(IM1.NE.IM2.OR.JM1.NE.JM2) THEN
        IB1=0
        KGDS1(2)=IM1
        KGDS1(3)=JM1
        KGDS1(8)=NINT(-360000./IM1)
        KGDS1(10)=JM1/2
        KGDS2(2)=IM2
        KGDS2(3)=JM2
        KGDS2(8)=NINT(-360000./IM2)
        KGDS2(10)=JM2/2
        CALL IPOLATES(IP,IPOPT,KGDS1,KGDS2,IM1*JM1,IM2*JM2,KM,IB1,L1,G1,
     &                NO,RLAT,RLON,IB2,L2,G2,IRET)
      ELSE
        G2=G1
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
      INTEGER IDAT(8)
      REAL SL(LEVS),SI(LEVS+1),SPS(NC),SO3(NC,LEVS)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200),KGDSGG(200)
      REAL PS(LONB*LATB)
      REAL PG(LONB*LATB+1,LEVS),O3G(LONB*LATB+1,LEVS)
      PARAMETER(MO3T=720*361)
      LOGICAL*1 LB(MO3T)
      REAL FB(MO3T),O3T(LONB*LATB)
      REAL WORK1(LONB*LATB),WORK2(LONB*LATB)
      LOGICAL*1 L2(LONB*LATB)
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
      PARAMETER(JMC=18,KMC=17)
      INTEGER IDAT(8)
      INTEGER KGDS(200)
      REAL PG(MG,KG),O3G(MG,KG)
      REAL PRES(KMC),O3Y(JMC,KMC,12),O3M(JMC,KMC),O3W(MG,KMC)
      REAL Z1(MG,KMC),Z2(MG,KG)
      INTEGER KGDS2(200)
      INTEGER LATS(JMC)
      INTEGER JDAT(8)
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
        print *,' max=',fmax,' min=',fmin,' at k=',k,' for ',ch
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
!C$OMP PARALLEL DO DEFAULT(PRIVATE)
!C$OMP+ SHARED(im,km,kmp,ntracm,thermodyn_id,pp,tp,qp,cpi,cp0i)
!C$OMP+ SHARED(ak,bk,ck,pi)
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
!$OMP END PARALLEL DO
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
      PARAMETER(RD=287.05,RV=461.50,CP=1004.6)
      PARAMETER(ROCP=RD/CP,ROCP1=ROCP+1,ROCPR=1/ROCP,FV=RV/RD-1.)
      REAL AK(KM+1),BK(KM+1),CK(KM+1),PS(IX)
      REAL TP(IX,KM),QP(IX,KM),PI(IM,KM+1)
      REAL TOV(KM),TRK,TVU,TVD
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
      REAL P1(KM1),T1(KM1),Q1(KM1)
      REAL P2(KM2),T2(KM2),Q2(KM2)
      PARAMETER(DLTDZ=-6.5E-3*287.05/9.80665)
      PARAMETER(DLPVDRT=-2.5E6/461.50)
      REAL Z1(2,KM1),Z2(2,KM2)
      REAL C1(2,KM1,2),C2(2,KM2,2)
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
      REAL Z1(1+(IM-1)*IXZ1+(KM1-1)*KXZ1)
      REAL Q1(1+(IM-1)*IXQ1+(KM1-1)*KXQ1+(NM-1)*NXQ1)
      REAL Z2(1+(IM-1)*IXZ2+(KM2-1)*KXZ2)
      REAL Q2(1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)
      REAL FFA(IM),FFB(IM),FFC(IM),FFD(IM)
      INTEGER K1S(IM,KM2)
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
      REAL P1(IX,KM1),T1(IX,KM1),Q1(IX,KM1,NT)
      REAL P2(IX,KM2),T2(IX,KM2),Q2(IX,KM2,NT)
      PARAMETER(DLTDZ=-6.5E-3*287.05/9.80665)
      PARAMETER(DLPVDRT=-2.5E6/461.50)
      REAL Z1(IM+1,KM1),Z2(IM+1,KM2)
      REAL C1(IM+1,KM1,1+NT),C2(IM+1,KM2,1+NT),J2(IM+1,KM2,1+NT)
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


       subroutine compute_zh_rh(im, jm, levp, ak, bk,
     & ps, zs, t, p, sphum, zh, rh )
       implicit none
       integer, intent(in):: levp, im,jm
       real,    intent(in), dimension(levp+1):: ak, bk
       real,    intent(in), dimension(im,jm):: ps, zs
       real,    intent(in), dimension(im,jm,levp):: t, p
       real,    intent(in), dimension(im,jm,levp):: sphum
       real,    intent(out), dimension(im,jm,levp+1):: zh
       real,    intent(out), dimension(im,jm,levp):: rh
       ! Local:
       real, dimension(im,levp+1):: pe0, pn0
       integer i,j,k
       real, parameter :: GRAV   = 9.80665
       real, parameter :: RDGAS  = 287.05
       real, parameter :: RVGAS = 461.50
       real, parameter :: e0 = 610.71
       real, parameter :: hlv = 2.501e6
       real, parameter :: tfreeze = 273.15
       real  :: zvir 
       real:: grd

       grd = grav/rdgas
       zvir = rvgas/rdgas - 1.   
       do j = 1, jm
         do i=1, im
           pe0(i,1) = ak(1)
           pn0(i,1) = log(pe0(i,1))
         enddo

         do k=2,levp+1
            do i=1,im
              pe0(i,k) = ak(k) + bk(k)*ps(i,j)
              pn0(i,k) = log(pe0(i,k))
            enddo
         enddo

         zh(1:im,j,levp+1) = zs(1:im,j)
         do k = levp, 1, -1
           do i = 1, im
             zh(i,j,k) = zh(i,j,k+1)+t(i,j,k)*(1.+zvir*sphum(i,j,k))*
     &        (pn0(i,k+1)-pn0(i,k))/grd
             rh(i,j,k) = 100.0*sphum(i,j,k)/(e0*rdgas/(rvgas*p(i,j,k))*
     &        exp(hlv/rvgas*(t(i,j,k)-tfreeze)/(t(i,j,k)*tfreeze)))
           enddo
         enddo

       enddo

       end subroutine compute_zh_rh

       

