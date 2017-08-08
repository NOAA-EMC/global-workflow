       PROGRAM SFC_DRV

!----------------------------------------------------------------------
!
!  Stand alone surface cycle driver for the cubed-sphere grid.
!
!  2005-02-03:  Iredell   for global_analysis
!  2014-11-30:  xuli      add nst_anl
!  2015-05-26:  Hang Lei  Added NEMSIO read/write function in the code
!  2017-08-08:  Gayno     Modify to work on cubed-sphere grid
!
!  LUGB         Unit number used in the subprogram
!  IDIM,JDIM    i/j dimension of a cubed-sphere tile.
!  IY,IM,ID,IH  Year, month, day, and hour of initial state.
!  FH           Forecast hour
!  IALB         Use modis albedo when '1'. Use brigleb when '0'.
!  ISOT         Use statsgo soil type when '1'. Use zobler when '0'.
!  IVEGSRC      Use igbp veg type when '1'.  Use sib when '2'.
!  SIG1T        Sigma level 1 temperature for dead start.
!               If not dead start, no need for dimension but set to
!               zero as in the example below.
!----------------------------------------------------------------------

      IMPLICIT NONE
!
      INTEGER :: IDIM, JDIM, LSOIL, LUGB, IY, IM, ID, IH, IALB
      INTEGER :: ISOT, IVEGSRC
      REAL    :: FH, DELTSFC
      LOGICAL :: USE_UFO, NST_ANL
!
      NAMELIST/NAMCYC/ IDIM,JDIM,LSOIL,LUGB,IY,IM,ID,IH,FH
     &,                DELTSFC,IALB,USE_UFO,NST_ANL
     &,                ISOT,IVEGSRC
!
      DATA IDIM,JDIM,LSOIL/96,96,4/
      DATA IY,IM,ID,IH,FH/1997,8,2,0,0./
      DATA LUGB/51/, DELTSFC/0.0/, IALB/1/
      DATA ISOT/1/, IVEGSRC/2/
!
      PRINT*,"STARTING CYCLE PROGRAM."

      USE_UFO = .FALSE.
      NST_ANL = .FALSE.

      PRINT*,"READ NAMCYC NAMELIST."

      READ(5,NAMCYC)
      WRITE(6,NAMCYC)
!
      CALL MAINSFC(IDIM,JDIM,LSOIL,LUGB,IY,IM,ID,IH,FH,DELTSFC,IALB,
     &             USE_UFO,NST_ANL,ISOT,IVEGSRC)
!
      PRINT*,'CYCLE PROGRAM COMPLETED NORMALLY.'

      STOP

      END PROGRAM SFC_DRV
!
      SUBROUTINE MAINSFC(IDIM,JDIM,LSOIL,LUGB,IY,IM,ID,IH,FH,DELTSFC
     &,                  ialb,use_ufo,nst_anl,isot,ivegsrc)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: IDIM,JDIM,LSOIL,LUGB,IY,IM,ID,IH
      INTEGER, INTENT(IN) :: IALB,ISOT,IVEGSRC

      LOGICAL, INTENT(IN) :: USE_UFO, NST_ANL

      REAL,    INTENT(IN) :: FH, DELTSFC

      INTEGER  LENSFC
!
      REAL TSFFCS(IDIM*JDIM), SNOFCS(IDIM*JDIM),
     1     ZORFCS(IDIM*JDIM), ALBFCS(IDIM*JDIM,4), AISFCS(IDIM*JDIM),
     2     TG3FCS(IDIM*JDIM), ALFFCS(IDIM*JDIM,2),
     3     CVFCS (IDIM*JDIM), CVBFCS(IDIM*JDIM),   CVTFCS(IDIM*JDIM),
     4     CNPFCS(IDIM*JDIM),
     5     SMCFCS(IDIM*JDIM,LSOIL), STCFCS(IDIM*JDIM,LSOIL),
     6     SLIFCS(IDIM*JDIM),       VEGFCS(IDIM*JDIM),
     7     SLMASK(IDIM*JDIM),       OROG(IDIM*JDIM),
     &     vetfcs(idim*jdim),       sotfcs(idim*jdim)
     &,    SIHFCS(IDIM*JDIM),       SICFCS(IDIM*JDIM)
     &,    SITFCS(IDIM*JDIM)
     &,    T2M(IDIM*JDIM),          Q2M(IDIM*JDIM)
     &,    SLCFCS(IDIM*JDIM,LSOIL), SWDFCS(IDIM*JDIM)
     &,    VMNFCS(IDIM*JDIM),       VMXFCS(IDIM*JDIM)
     &,    SLPFCS(IDIM*JDIM),       ABSFCS(IDIM*JDIM)
     &,    orog_uf(idim*jdim)

      REAL F10M  (IDIM*JDIM), SIG1T(IDIM*JDIM)
     &,    TPRCP(IDIM*JDIM),  SRFLAG(IDIM*JDIM)
!
      SIG1T = 0.0            ! Not a dead start!
!
      PRINT*,"LUGB,IDIM,JDIM,LSOIL,DELTSFC,IY,IM,ID,IH,FH: ",
     &        LUGB,IDIM,JDIM,LSOIL,DELTSFC,IY,IM,ID,IH,FH

      CALL SFCDRV(LUGB,IDIM,JDIM,LSOIL,SIG1T,DELTSFC,LENSFC,
     1            IY,IM,ID,IH,FH,IALB,
     2            SLMASK,  OROG,SIHFCS,SICFCS,SITFCS,
     *            TSFFCS,SNOFCS,ZORFCS,ALBFCS,TG3FCS,
     4            CNPFCS,SMCFCS,STCFCS,SLIFCS,AISFCS,F10M,
     *            VEGFCS,VETFCS,SOTFCS,ALFFCS,
     5            CVFCS,CVBFCS,CVTFCS,
     +            TPRCP,SRFLAG,SWDFCS,SLCFCS,
     +            VMNFCS,VMXFCS,SLPFCS,ABSFCS,T2M,Q2M,OROG_UF,
     &            USE_UFO,NST_ANL,ISOT,IVEGSRC)
!
      RETURN
      END SUBROUTINE MAINSFC

      SUBROUTINE SFCDRV(LUGB,IDIM,JDIM,LSOIL,SIG1T,DELTSFC,LENSFC
     *,                 IY,IM,ID,IH,FH,IALB
     *,                 SLMASK,OROG,  SIHFCS,SICFCS,SITFCS
     *,                 TSFFCS,SNOFCS,ZORFCS,ALBFCS,TG3FCS
     *,                 CNPFCS,SMCFCS,STCFCS,SLIFCS,AISFCS,F10M
     *,                 VEGFCS,VETFCS,SOTFCS,ALFFCS
     *,                 CVFCS,CVBFCS,CVTFCS
     +,                 TPRCP,SRFLAG,SWDFCS,SLCFCS
     +,                 VMNFCS,VMXFCS,SLPFCS,ABSFCS,T2M,Q2M,orog_uf,
     &                  use_ufo,nst_anl,isot,ivegsrc)
!
      use read_write_data
      use machine, kind_io => kind_io8

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IDIM, JDIM, LSOIL
      INTEGER, INTENT(IN) :: LUGB, IY, IM, ID, IH

      REAL, INTENT(IN)    :: FH, DELTSFC, SIG1T(IDIM*JDIM)

      INTEGER             :: IALB,ISOT,IVEGSRC
      INTEGER, PARAMETEr  :: NLUNIT=35, ME=0

      LOGICAL             :: USE_UFO, NST_ANL
!
!  THIS IS A DRIVER FOR VERSION II SURFACE PROGRAM.
!
!  This program runs in two different modes:
!
!  1.  Analysis mode (FH=0.)
!
!      This program merges climatology, analysis and forecast guess to create
!      new surface fields (BGES).  If analysis file is given, the program 
!      uses it if date of the analysis matches with IY,IM,ID,IH (see Note
!      below).
!
!  2.  Forecast mode (FH.GT.0.)
!    
!      This program interpolates climatology to the date corresponding to the 
!      forecast hour.  If surface analysis file is given, for the corresponding
!      dates, the program will use it.  This is forcing-by-observation experiment.
!
!   NOTE:
!
!      If the date of the analysis does not match given IY,IM,ID,IH, (and FH),
!      the program searches an old analysis by going back 6 hours, then 12 hours,
!      then one day upto NREPMX days (parameter statement in the SUBROTINE FIXRD. 
!      Now defined as 8).  This allows the user to provide non-daily analysis to 
!      be used.  If matching field is not found, the forecast guess will be used.
!
!      Use of a combined earlier surface analyses and current analysis is 
!      NOT allowed (as was done in the old version for snow analysis in which
!      old snow analysis is used in combination with initial guess), except
!      for sea surface temperature.  For sst anolmaly interpolation, you need to
!      set LANOM=.TRUE. and must provide sst analysis at initial time.  
!
!      If you want to do complex merging of past and present surface field analysis,
!      YOU NEED TO CREATE a separate file that contains DAILY SURFACE FIELD.
!
!      For a dead start, do not supply FNBGSI or set FNBGSI='        ' 
!
!  LUGB is the unit number used in this subprogram
!  IDIM,JDIM ... grid dimension in x and y direction, respectively of a tile
!                of the cubed-sphere grid.
!  LSOIL .. Number of soil layers 
!  IY,IM,ID,IH .. Year, month, day, and hour of initial state.
!  FH .. Forecast hour
!  SIG1T .. Sigma level 1 temperature for dead start.  Should be on Gaussian
!           grid.  If not dead start, no need for dimension but set to zero
!           as in the example below.
!
!  Variable naming conventions:
!
!     OROG .. Orography
!     ALB  .. Albedo
!     WET  .. Soil wetness as defined for bucket model
!     SNO  .. Snow DEPTH
!     ZOR  .. Surface roughness length
!     VET  .. Vegetation type
!     PLR  .. Plant evaporation resistance
!     TSF  .. Surface skin temperature.  Sea surface temp. over ocean.
!     TG3  .. Deep soil temperature (at 500cm)
!     STC  .. Soil temperature (LSOIL layrs)
!     SMC  .. Soil moisture (LSOIL layrs)
!     SCV  .. Snow cover (not snow depth)
!     AIS  .. Sea ice mask (0 or 1)
!     ACN  .. Sea ice concentration (fraction)
!     GLA  .. Glacier (permanent snow) mask (0 or 1)
!     MXI  .. Maximum sea ice extent (0 or 1)
!     MSK  .. Land ocean mask (0=ocean 1=land)
!     CNP  .. Canopy water content
!     CV   .. Convective cloud cover
!     CVB  .. Convective cloud base
!     CVT  .. Convective cloud top
!     SLI  .. LAND/SEA/SEA-ICE mask. (1/0/2 respectively)
!     cover .. Vegetation cover
!     SOI  .. Soil type
!     SIH  .. Sea ice thickness
!     SIC  .. Sea ice concentration
!     SWD  .. Actual snow depth
!     SLC  .. Liquid soil moisture (LSOIL layers)
!     VMN  .. Vegetation cover minimum
!     VMX  .. Vegetation cover maximum
!     SLP  .. Slope type
!     ABS  .. Maximum snow albedo
!     T2M  .. 2m Temperature
!     Q2M  .. 2m Specific Humidity
!     TICE .. Ice Temperature
!     OROG_uf  .. Orography unfiltered
!
!  COEEFICIENTS OF BLENDING FORECAST AND INTERPOLATED CLIM
!  (OR ANALYZED) FIELDS OVER SEA OR LAND(L) (NOT FOR CLOUDS)
!  1.0 = USE OF FORECAST
!  0.0 = REPLACE WITH INTERPOLATED ANALYSIS
!
!    These values are set for analysis mode.
!
!   Variables                  Land                 Sea
!   ---------------------------------------------------------
!   Surface temperature        Forecast             Analysis
!   Albedo                     Analysis             Analysis
!   Sea-ice                    Analysis             Analysis
!   Snow                       Analysis             Forecast (over sea ice)
!   Roughness                  Analysis             Forecast
!   Plant resistance           Analysis             Analysis
!   Soil wetness (layer)       Weighted average     Analysis
!   Soil temperature           Forecast             Analysis
!   Canopy waver content       Forecast             Forecast
!   Convective cloud cover     Forecast             Forecast
!   Convective cloud bottm     Forecast             Forecast
!   Convective cloud top       Forecast             Forecast
!   Vegetation cover           Analysis             Analysis
!   vegetation type            Analysis             Analysis
!   soil type                  Analysis             Analysis
!
!  MASK OROGRAPHY AND VARIANCE ON GAUSSIAN GRID
!
      CHARACTER*500 FNOROG, FNGRID
!
      REAL SLMASK(IDIM*JDIM), OROG(IDIM*JDIM),  orog_uf(idim*jdim)
!
!  PREDICTED SURFACE FIELDS (Last characters 'FCS' indicates FORECAST)
!
      REAL TSFFCS(IDIM*JDIM), SNOFCS(IDIM*JDIM),
     1     ZORFCS(IDIM*JDIM), ALBFCS(IDIM*JDIM,4),AISFCS(IDIM*JDIM),
     2     TG3FCS(IDIM*JDIM), 
     3     CVFCS (IDIM*JDIM), CVBFCS(IDIM*JDIM),  CVTFCS(IDIM*JDIM),
     4     CNPFCS(IDIM*JDIM),
     5     SMCFCS(IDIM*JDIM,LSOIL), STCFCS(IDIM*JDIM,LSOIL),
     6     SLIFCS(IDIM*JDIM),   VEGFCS(IDIM*JDIM),
     &     vetfcs(idim*jdim),   sotfcs(idim*jdim),
     &     alffcs(idim*jdim,2)
      REAL SIHFCS(IDIM*JDIM),   SICFCS(IDIM*JDIM)
      REAL SITFCS(IDIM*JDIM)
      REAL T2M(IDIM*JDIM),      Q2M(IDIM*JDIM)
     &,    SWDFCS(IDIM*JDIM),   SLCFCS(IDIM*JDIM,LSOIL)
     &,    VMNFCS(IDIM*JDIM),   VMXFCS(IDIM*JDIM)
     &,    SLPFCS(IDIM*JDIM),   ABSFCS(IDIM*JDIM)
!
      REAL ALBFC(IDIM*JDIM*4),     SMCFC(IDIM*JDIM*LSOIL)
     &,    STCFC(IDIM*JDIM*LSOIL), ALFFC(IDIM*JDIM*2)
     &,    SLCFC(IDIM*JDIM*LSOIL)
!
      real ustar(idim*jdim),fmm(idim*jdim),fhh(idim*jdim)
     +,   TPRCP(IDIM*JDIM),    SRFLAG(IDIM*JDIM)
      real(kind=4) zsoil(lsoil)
!
! Ratio of sigma level 1 wind and 10m wind (diagnozed by model and not touched
! in this program).
!
      REAL :: F10M(IDIM*JDIM)
!
!  Input and output SURFACE FIELDS (BGES) file names
!
      CHARACTER*500 FNBGSI,FNBGSO
!
!     OUTPUT FILE  ... PRIMARY SURFACE FILE FOR RADIATION AND FORECAST
!
!       REC.  1    LABEL
!       REC.  2    DATE RECORD
!       REC.  3    TSF
!       REC.  4    SOILM(TWO LAYERS)
!       REC.  5    SNOW
!       REC.  6    SOILT(TWO LAYERS)
!       REC.  7    TG3
!       REC.  8    ZOR
!       REC.  9    CV
!       REC. 10    CVB
!       REC. 11    CVT
!       REC. 12    ALBEDO (four types)
!       REC. 13    SLIMSK
!       REC. 14    vegetation cover
!       REC. 14    PLANTR
!       REC. 15    F10M
!       REC. 16    CANOPY WATER CONTENT (CNPANL)
!       REC. 17    vegetation type
!       REC. 18    soil type
!       REC. 19    zeneith angle dependent vegetation fraction (two types)
!       REC. 20    UUSTAR
!       REC. 21    FFMM
!       REC. 22    FFHH
!Cwu add SIH & SIC
!       REC. 23    SIH(one category only)
!       REC. 24    SIC
!Clu [+8L] add PRCP, FLAG, SWD, SLC, VMN, VMX, SLP, ABS
!       REC. 25    TPRCP
!       REC. 26    SRFLAG
!       REC. 27    SWD
!       REC. 28    SLC (4 LAYERS)
!       REC. 29    VMN
!       REC. 30    VMX
!       REC. 31    SLP
!       REC. 32    ABS
!
!  LAT/LON PHYSICS GRID FOR MONITORING
!
      REAL :: RLA(IDIM*JDIM),RLO(IDIM*JDIM)
 
      INTEGER :: IFP, LENSFC, I, II, K

      NAMELIST/NAMSFCD/FNOROG,FNGRID,FNBGSI,FNBGSO
!
!  Defaults file names
!
      DATA FNOROG/'        '/
      DATA FNGRID/'        '/
      DATA FNBGSI/'        '/
      DATA FNBGSO/'        '/
!
      DATA IFP/0/
!
      SAVE IFP, FNGRID, FNOROG, FNBGSI, FNBGSO
!
      IF(IFP.EQ.0) THEN
        IFP = 1
        READ (5,NAMSFCD)
        WRITE(6,NAMSFCD)
      ENDIF

      PRINT*,'in sfcdrv,idim=',idim,'jdim=',jdim,'FH=',FH

      LENSFC = IDIM * JDIM

      CALL READ_LAT_LON_OROG(RLA,RLO,OROG,OROG_UF,FNOROG,FNGRID,
     &                       IDIM,JDIM,LENSFC)

      CALL READ_SFC_NETCDF(TSFFCS,SMCFCS,SNOFCS,STCFCS,TG3FCS,ZORFCS,
     &                     CVFCS,CVBFCS,CVTFCS,ALBFCS,SLIFCS,
     &                     VEGFCS,CNPFCS,F10M,VETFCS,SOTFCS,
     &                     ALFFCS,USTAR,FMM,FHH,
     &                     SIHFCS,SICFCS,SITFCS,
     &                     TPRCP,SRFLAG,SWDFCS,
     &                     VMNFCS,VMXFCS,SLCFCS,
     &                     SLPFCS,ABSFCS,T2M,Q2M,SLMASK,
     &                     ZSOIL,LSOIL,LENSFC,FNBGSI)

      IF (USE_UFO) THEN
        PRINT*,'USE UNFILTERED OROGRAPHY.'
      ELSE
        OROG_UF = 0.0
      ENDIF
 
      DO I=1,LENSFC
        AISFCS(I) = 0.
        IF(NINT(SLIFCS(I)).EQ.2) AISFCS(I) = 1.
      ENDDO

      DO K=1,LSOIL
        II = (K-1)*LENSFC
        DO I=1,LENSFC
          SMCFC(II+I) = SMCFCS(I,K)
          STCFC(II+I) = STCFCS(I,K)
          SLCFC(II+I) = SLCFCS(I,K)
        ENDDO
      ENDDO

      DO K=1,4
        II = (K-1)*LENSFC
        DO I=1,LENSFC
          ALBFC(II+I) = ALBFCS(I,K)
        ENDDO
      ENDDO

      DO K=1,2
        II = (K-1)*LENSFC
        DO I=1,LENSFC
          ALFFC(II+I) = ALFFCS(I,K)
        ENDDO
      ENDDO
 
        CALL SFCCYCLE(LUGB,LENSFC,LSOIL,SIG1T,DELTSFC
     &,               IY,IM,ID,IH,FH,RLA,RLO
     &,               SLMASK,OROG, OROG_UF, USE_UFO, NST_ANL
     &,               SIHFCS,SICFCS,SITFCS
     &,               SWDFCS,SLCFC
     &,               VMNFCS,VMXFCS,SLPFCS,ABSFCS
     &,               TSFFCS,SNOFCS,ZORFCS,ALBFC,TG3FCS
     &,               CNPFCS,SMCFC,STCFC,SLIFCS,AISFCS,F10M
     &,               VEGFCS,VETFCS,SOTFCS,ALFFC
     &,               CVFCS,CVBFCS,CVTFCS,ME,NLUNIT,IALB,ISOT,IVEGSRC)

      DO K=1,LSOIL
        II = (K-1)*LENSFC
        DO I=1,LENSFC
          SMCFCS(I,K) = SMCFC(II+I)
          STCFCS(I,K) = STCFC(II+I)
          SLCFCS(I,K) = SLCFC(II+I)
        ENDDO
      ENDDO

      DO K=1,4
        II = (K-1)*LENSFC
        DO I=1,LENSFC
          ALBFCS(I,K) = ALBFC(II+I)
        ENDDO
      ENDDO

      DO K=1,2
        II = (K-1)*LENSFC
        DO I=1,LENSFC
          ALFFCS(I,K) = ALFFC(II+I)
        ENDDO
      ENDDO
 
      CALL WRITE_DATA(SLIFCS,TSFFCS,SNOFCS,TG3FCS,ZORFCS,
     &                ALBFCS,ALFFCS,VEGFCS,CNPFCS,F10M,
     &                T2M,Q2M,VETFCS,SOTFCS,USTAR,FMM,FHH,
     &                SICFCS,SIHFCS,SITFCS,
     &                TPRCP,SRFLAG,SWDFCS,
     &                VMNFCS,VMXFCS,SLPFCS,ABSFCS,
     &                SLCFCS,SMCFCS,STCFCS,
     &                IDIM,JDIM,LENSFC,LSOIL)

      RETURN

      END SUBROUTINE SFCDRV

      SUBROUTINE QCMASK(SLMASK,SLLND,SLSEA,IDIM,JDIM,RLA,RLO)
!
CFPP$ NOCONCUR R
      DIMENSION SLMASK(IDIM,JDIM),RLA(IDIM,JDIM),RLO(IDIM,JDIM)
!
      WRITE(6,*) ' QCMASK'
!
!  CHECK LAND-SEA MASK
!
      DO J=1,JDIM
      DO I=1,IDIM
        IF(SLMASK(I,J).NE.SLLND.AND.SLMASK(I,J).NE.SLSEA) THEN
          PRINT *,'*** LAND-SEA MASK NOT ',SLLND,' OR ',SLSEA,
     1          ' AT LAT=',RLA(I,J),' LON=',RLO(I,J),' IT IS ',
     2          SLMASK(I,J)
          CALL ABORT
        ENDIF
      ENDDO
      ENDDO
!
!  Remove isolated sea point
!
      DO J=1,JDIM
        DO I=1,IDIM
          IP = I + 1
          IM = I - 1
          JP = J + 1
          JM = J - 1
          IF(JP.GT.JDIM) JP = JDIM - 1
          IF(JM.LT.1)    JM = 2
          IF(IP.GT.IDIM) IP = 1
          IF(IM.LT.1   ) IM = IDIM
          IF(SLMASK(I,J).EQ.0.) THEN
            IF(SLMASK(IP,JP).EQ.1..AND.SLMASK(I ,JP).EQ.1..AND.
     1         SLMASK(IM,JP).EQ.1..AND.
     2         SLMASK(IP,J ).EQ.1..AND.SLMASK(IM,J ).EQ.1..AND.
     3         SLMASK(IP,JM).EQ.1..AND.SLMASK(I ,JM).EQ.1..AND.
     4         SLMASK(IM,JM).EQ.1) THEN
              SLMASK(I,J)=1.
              WRITE(6,*) ' Isolated open sea point modified to land',
     1                   ' at LAT=',RLA(I,J),' LON=',RLO(I,J)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!  Remove isolated land point
!
      DO J=1,JDIM
        DO I=1,IDIM
          IP = I + 1
          IM = I - 1
          JP = J + 1
          JM = J - 1
          IF(JP.GT.JDIM) JP = JDIM - 1
          IF(JM.LT.1)    JM = 2
          IF(IP.GT.IDIM) IP = 1
          IF(IM.LT.1)    IM = IDIM
          IF(SLMASK(I,J).EQ.1.) THEN
            IF(SLMASK(IP,JP).EQ.0..AND.SLMASK(I ,JP).EQ.0..AND.
     1         SLMASK(IM,JP).EQ.0..AND.
     2         SLMASK(IP,J ).EQ.0..AND.SLMASK(IM,J ).EQ.0..AND.
     3         SLMASK(IP,JM).EQ.0..AND.SLMASK(I ,JM).EQ.0..AND.
     4         SLMASK(IM,JM).EQ.0) THEN
              SLMASK(I,J)=0.
              WRITE(6,*) ' Isolated land point modified to sea',
     1                   ' at LAT=',RLA(I,J),' LON=',RLO(I,J)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE QCMASK
