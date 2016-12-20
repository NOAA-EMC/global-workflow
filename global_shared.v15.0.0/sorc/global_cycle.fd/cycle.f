       PROGRAM SFC_DRV
!
!  Stand alone  surface cycle driver for reduced grid
!
!  2005-02-03:  Iredell   for global_analysis
!  2014-11-30:  xuli      add nst_anl
!  2015-05-26:  Hang Lei  Added NEMSIO read/write function in the code
!
!  LUGB is the unit number used in the subprogram
!  IDIM,JDIM ... Maximum Longitudinal and Latitudinal grid dimensions
!  IY,IM,ID,IH .. Year, month, day, and hour of initial state.
!  FH .. Forecast hour
!
!  SIG1T .. Sigma level 1 temperature for dead start.
!           If not dead start, no need for dimension but set to zero
!           as in the example below.
!
      Implicit none
!
      integer idim, jdim, lsoil, lugb, iy, im, id, ih, lensfc, ialb
      integer outtyp, isot, ivegsrc
      real    fh,   deltsfc
      logical use_ufo
      logical nst_anl
!
      NAMELIST/NAMCYC/ IDIM,JDIM,LSOIL,LUGB,IY,IM,ID,IH,FH
     &,                DELTSFC,ialb,use_ufo,nst_anl
     &,                OUTTYP,isot,ivegsrc
!
Cwu  change LSOIL: LSOIL=4
!     DATA IDIM,JDIM,LSOIL/192,94,2/
      DATA IDIM,JDIM,LSOIL/192,94,4/
      DATA IY,IM,ID,IH,FH/1997,8,2,0,0./
      DATA LUGB/51/, DELTSFC/0.0/, ialb/1/
      DATA ISOT/0/, IVEGSRC/2/
      DATA OUTTYP/1/                       ! default: nemsio

!
      use_ufo = .false.
      nst_anl = .false.
      READ(5,NAMCYC)
      WRITE(6,NAMCYC)
!
      CALL MAINSFC(IDIM,JDIM,LSOIL,LUGB,IY,IM,ID,IH,FH,DELTSFC,ialb,
     &             use_ufo,OUTTYP,isot,ivegsrc)
!
      STOP
      END
!
      SUBROUTINE MAINSFC(IDIM,JDIM,LSOIL,LUGB,IY,IM,ID,IH,FH,DELTSFC
     &,                              ialb, use_ufo,OUTTYP,isot,ivegsrc)
!
      implicit none
!
      integer idim, jdim, lsoil, lugb, iy, im, id, ih, lensfc, ialb
      integer OUTTYP, isot, ivegsrc
      real    fh,   deltsfc
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
!Cwu  add SIHFCS & SICFCS
     &,    SIHFCS(IDIM*JDIM),       SICFCS(IDIM*JDIM)
     &,    SITFCS(IDIM*JDIM)
     &,    T2M(IDIM*JDIM),          Q2M(IDIM*JDIM)
!Clu add SLCFCS,SWDFCS,VMNFCS,VMXFCS,SLPFCS,ABSFCS 
     &,    SLCFCS(IDIM*JDIM,LSOIL), SWDFCS(IDIM*JDIM)
     &,    VMNFCS(IDIM*JDIM),       VMXFCS(IDIM*JDIM)
     &,    SLPFCS(IDIM*JDIM),       ABSFCS(IDIM*JDIM)
     &,    orog_uf(idim*jdim)
      logical use_ufo

      REAL F10M  (IDIM*JDIM), sig1t(idim*jdim)
!Clu add TPRCP & SRFLAG
     &,    TPRCP(IDIM*JDIM),        SRFLAG(IDIM*JDIM)
!
      sig1t = 0.0                 ! Not a dead start!
!
      print *, LUGB,IDIM,JDIM,LSOIL,DELTSFC,
     1            IY,IM,ID,IH,FH
      CALL SFCDRV(LUGB,IDIM,JDIM,LSOIL,SIG1T,DELTSFC,LENSFC,
     1            IY,IM,ID,IH,FH,ialb,OUTTYP,
!Cwu  add SIHFCS & SICFCS
!    2            SLMASK, OROG,
     2            SLMASK,  OROG,SIHFCS,SICFCS,SITFCS,
     *            TSFFCS,SNOFCS,ZORFCS,ALBFCS,TG3FCS,
     4            CNPFCS,SMCFCS,STCFCS,SLIFCS,AISFCS,F10M,
     *            VEGFCS,VETFCS,SOTFCS,ALFFCS,
     5            CVFCS,CVBFCS,CVTFCS,
!Clu add TPRCP,SRFLAG,SLCFCS,SWDFCS,VMNFCS,VMXFCS,SLPFCS,ABSFCS
     +            TPRCP,SRFLAG,SWDFCS,SLCFCS,
     +            VMNFCS,VMXFCS,SLPFCS,ABSFCS,T2M,Q2M,orog_uf,
     &            use_ufo,isot,ivegsrc)
!
      return
      END
      SUBROUTINE SFCDRV(LUGB,IDIM,JDIM,LSOIL,SIG1T,DELTSFC,LENSFC
     *,                 IY,IM,ID,IH,FH, ialb,OUTTYP
!Cwu  add SIHFCS & SICFCS
!    *,                 SLMASK,OROG
     *,                 SLMASK,OROG,  SIHFCS,SICFCS,SITFCS
     *,                 TSFFCS,SNOFCS,ZORFCS,ALBFCS,TG3FCS
     *,                 CNPFCS,SMCFCS,STCFCS,SLIFCS,AISFCS,F10M
     *,                 VEGFCS,VETFCS,SOTFCS,ALFFCS
     *,                 CVFCS,CVBFCS,CVTFCS
!Clu add TPRCP,SRFLAG,SLCFCS,SWDFCS,VMNFCS,VMXFCS,SLPFCS,ABSFCS
     +,                 TPRCP,SRFLAG,SWDFCS,SLCFCS
     +,                 VMNFCS,VMXFCS,SLPFCS,ABSFCS,T2M,Q2M,orog_uf,
     &                  use_ufo,isot,ivegsrc)
!
      use machine, kind_io => kind_io8
      use NEMSIO_MODULE
!CFPP$ NOCONCUR R
      LOGICAL GAUS,   DEADS, QCMSK, ZNLST, MONCLM, MONANL,
     1        MONFCS, MONMER, MONDIF, GRBORO, GRBMSK,
     2        OROIBM, MSKIBM, BGIIBM, BGOIBM, use_ufo
      integer ialb,OUTTYP,isot,ivegsrc
      integer INPTYP
      type(nemsio_gfile) GFILEI
      integer, parameter :: nlunit=35, me=0
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
!  IDIM,JDIM ... Gaussian grid dimension in x and y direction, respectovely
!  LSOIL .. Number of soil layers (2 as of April, 1994)
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
!Cwu  add SIH & SIC
!     SIH  .. Sea ice thickness
!     SIC  .. Sea ice concentration
!Clu  add SWD,SLC,VMN,VMX,SLP,ABS
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
!  Definition of Land/Sea mask. SLLND for land and SLSEA for sea.
!  Definition of Sea/ice mask. AICICE for ice, AICSEA for sea.
!
      PARAMETER(SLLND =1.0,SLSEA =0.0)
      PARAMETER(AICICE=1.0,AICSEA=0.0)
!
!  Max/Min of fields for check and replace.
!
!
      PARAMETER(OROLMX=8000.,OROLMN=-1000.,OROOMX=3000.,OROOMN=-1000.,
     1          OROSMX=8000.,OROSMN=-1000.,OROIMX=3000.,OROIMN=-1000.,
     2          OROJMX=3000.,OROJMN=-1000.)
!
!  Criteria used for monitoring
!
      PARAMETER(EPSTSF=0.01,EPSALB=0.001,EPSSNO=0.01,
     1          EPSWET=0.01,EPSZOR=0.0000001,EPSPLR=1.,EPSORO=0.,
     2          EPSSMC=0.0001,EPSSCV=0.,EPTSFC=0.01,EPSTG3=0.01,
     3          EPSAIS=0.,EPSACN=0.01,EPSVEG=0.01,
     &          epsvet=.01,epssot=.01,epsalf=.001)
!
!  Quality control of analysis snow and sea ice
!
!   QCTSFS .. Surface temperature above which no snow allowed
!   QCSNOS .. Snow depth above which snow must exist
!   QCTSFI .. SST above which sea-ice is not allowed
!
      PARAMETER(QCTSFS=283.16,QCSNOS=100.,QCTSFI=275.16)
!
!  Parameters to obtain snow depth from snow cover and temperature
!
!     PARAMETER(SNWMIN=25.,SNWMAX=100.)
      PARAMETER(SNWMIN=5.0,SNWMAX=100.)
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
!  Critical percentage value for aborting bad points when LGCHEK=.TRUE.
!
      LOGICAL LGCHEK
      DATA LGCHEK/.TRUE./
      DATA CRITP1,CRITP2,CRITP3/80.,80.,25./
!
      PARAMETER (KPDORO=8,KPDMSK=81)
!
!  MASK OROGRAPHY AND VARIANCE ON GAUSSIAN GRID
!
      CHARACTER*500 FNOROG,FNMASK,fnorog_uf
!
!     integer kind_io
!     parameter (kind_io=4)
      integer lonsperlat(jdim/2)
!
      real SLMASK(IDIM*JDIM), OROG(IDIM*JDIM),  orog_uf(idim*jdim)
!    &,                       orogdif(idim*jdim)
      real (kind=kind_io)  orogf(idim*jdim),    slmaskf(idim*jdim)
     &,                    orogf_uf(idim*jdim)
      integer kmsk(idim*jdim),idate(4), idateo(4)
!
!  PREDICTED SURFACE FIELDS (Last characters 'FCS' indicates FORECAST)
!
      REAL TSFFCS(IDIM*JDIM), WETFCS(IDIM*JDIM),  SNOFCS(IDIM*JDIM),
     1     ZORFCS(IDIM*JDIM), ALBFCS(IDIM*JDIM,4),AISFCS(IDIM*JDIM),
     2     TG3FCS(IDIM*JDIM), ACNFCS(IDIM*JDIM),
     3     CVFCS (IDIM*JDIM), CVBFCS(IDIM*JDIM),  CVTFCS(IDIM*JDIM),
     4     CNPFCS(IDIM*JDIM),
     5     SMCFCS(IDIM*JDIM,LSOIL), STCFCS(IDIM*JDIM,LSOIL),
     6     SLIFCS(IDIM*JDIM),   VEGFCS(IDIM*JDIM),
     &     vetfcs(idim*jdim),   sotfcs(idim*jdim),
     &     alffcs(idim*jdim,2)
!Cwu  add SIHFCS & SICFCS
      REAL SIHFCS(IDIM*JDIM),   SICFCS(IDIM*JDIM)
      REAL SITFCS(IDIM*JDIM)
      REAL T2M(IDIM*JDIM),      Q2M(IDIM*JDIM)
!Clu  add SWDFCS,SLCFCS,VMNFCS,VMXFCS,SLPFCS,ABSFCS
     &,    SWDFCS(IDIM*JDIM),   SLCFCS(IDIM*JDIM,LSOIL)
     &,    VMNFCS(IDIM*JDIM),   VMXFCS(IDIM*JDIM)
     &,    SLPFCS(IDIM*JDIM),   ABSFCS(IDIM*JDIM)
!
      REAL ALBFC(IDIM*JDIM*4),     SMCFC(IDIM*JDIM*LSOIL)
     &,    STCFC(IDIM*JDIM*LSOIL), ALFFC(IDIM*JDIM*2)
!Clu  add SLCFC
     &,    SLCFC(IDIM*JDIM*LSOIL)
      REAL SLICLM(IDIM*JDIM), SNOCLM(IDIM*JDIM)
!
      real ustar(idim*jdim),fmm(idim*jdim),fhh(idim*jdim)
!Clu  add tprcp & srflag
     +,   TPRCP(IDIM*JDIM),    SRFLAG(IDIM*JDIM)
      real (4) zsoil(lsoil)
      real(kind=4) buff1(idim*jdim)
      integer ivssfc
!
! Ratio of sigma level 1 wind and 10m wind (diagnozed by model and not touched
! in this program).
!
      DIMENSION F10M  (IDIM*JDIM)
!
!  Input and output SURFACE FIELDS (BGES) file names
!
      CHARACTER*500 FNBGSI,FNBGSO
!
!  Sigma level 1 temperature for dead start
!
      DIMENSION SIG1T(IDIM*JDIM), WRK(IDIM,JDIM)
!
      CHARACTER*32 LABEL
!
!  = 1 ==> FORECAST IS USED
!  = 0 ==> ANALYSIS (OR CLIMATOLOGY) IS USED
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
!
!  LAT/LON of GAUSSIAN GRID FOR MONITORING
!
      DIMENSION GAUL(JDIM),RLA(IDIM*JDIM),RLO(IDIM*JDIM)
!
!  Debug only
!   LDEBUG=.TRUE. creates BGES files for climatology and analysis
!
      LOGICAL LDEBUG
!
!
      NAMELIST/NAMSFCD/FNOROG,FNMASK,fnorog_uf,
     B                 FNBGSI,FNBGSO,
     C                 LDEBUG,
!
     J                 GAUS,   BLNOUT, BLTOUT, DEADS, QCMSK, ZNLST,
!    K                 IGRDBG,
     L                 GRBORO, GRBMSK
!    L                 GRBORO, GRBMSK, OROIBM, MSKIBM, BGIIBM, BGOIBM
!
      DATA GAUS/.TRUE./, BLNOUT/0.0/, BLTOUT/90.0/, DEADS/.FALSE./
     1,    QCMSK/.FALSE./, ZNLST/.FALSE./, IGRDBG/-1/
     3,    GRBORO/.TRUE./
     4,    GRBMSK/.TRUE./
!    4,    GRBMSK/.TRUE./, OROIBM/.TRUE./,MSKIBM/.TRUE./
!    4,    BGIIBM/.TRUE./, BGOIBM/.TRUE./
!
!  Defaults file names
!
      DATA FNOROG/'        '/
      DATA FNMASK/'        '/
      DATA FNOROG_uf/'        '/
!
      DATA FNBGSI/'        '/
      DATA FNBGSO/'        '/
!
      DATA LDEBUG/.FALSE./
!
!  ZONAL DIAGNOSTICS ARRAYS
!
      PARAMETER(NRCZNL=21)
      LOGICAL LSMSK(IDIM,2,6)
      DIMENSION ZNLSL(6,6,NRCZNL),WEIS(6,6)
      logical lprnt
!
      DATA IFP/0/
!
      SAVE IFP,    FNOROG, FNMASK, FNOROG_UF, FNBGSI, FNBGSO,
     C     LDEBUG, GAUS,   BLNOUT, BLTOUT, DEADS, QCMSK,
     K     IGRDBG, GRBORO, GRBMSK, OROIBM, MSKIBM, BGIIBM, BGOIBM
!
      IF(IFP.EQ.0) THEN
        IFP = 1
        READ (5,NAMSFCD)
        WRITE(6,NAMSFCD)
      ENDIF
      if (.not. use_ufo) then
        print *,' FNOROG_UF=',FNOROG_UF, ' but use_ufo=',use_ufo
        print *,' Resetting FNOROG_UF to blank'
        FNOROG_uf = '        '
        print *,' FNOROG_UF=',FNOROG_UF, ' use_ufo=',use_ufo
      endif
!
      print *,'in sfcdrv,idim=',idim,'jdim=',jdim,'FH=',FH
      IJDIM = IDIM * JDIM
!
!  IF THE DATASET NAMES ARE PROVIDED, READ IN LAND/SEA MASK AND
!  OROGRAPHY ON GIVEN GRID.  NOTE THAT FOR GUASSIAN GRIDS THE 
!  OROGRAPHY IS COMPUTED FROM SPHERICAL COEFF WITH ANGULATIONS OVER OCEAN
!
      WRITE(6,*) '=============='
      WRITE(6,*) '   MASK/OROG'
      WRITE(6,*) '=============='
!
      PERCRIT=CRITP1
!
!     real unfiltered orography
!
!
!  This version reads non-grib orography and land sea mask on model grids
!  and unfiltered orography
!
      print *,' calling mskrd idim,jdim,ijdim=',idim,jdim,ijdim
      CALL MSKRD(LUGB,IDIM,JDIM,IJDIM,IY,IM,ID,IH,FH,
     1           FNOROG,FNMASK,fnorog_uf,KPDORO,KPDMSK,
     2           OROGF, SLMASKF, orogf_uf, GRBORO, GRBMSK)
      
!
!  Quality control of MASK
!
!     IF (QCMSK) CALL QCMASK(SLMASKF,SLLND,SLSEA,IDIM,JDIM,RLA,RLO)
!
!  Quality control of Orography
!
      DO I=1,IJDIM
        SLICLM(I) = 1.
        SNOCLM(I) = 0.
      ENDDO
!     CALL QCMXMN('Orog    ',OROGF,SLICLM,SNOCLM,
!    1            OROLMX,OROLMN,OROOMX,OROOMN,OROIMX,OROIMN,
!    2            OROJMX,OROJMN,OROSMX,OROSMN,EPSORO,
!    3            RLA,RLO,IJDIM,0,PERCRIT,LGCHEK,WRK)
!
!
        CALL nemsio_init(iret)

        IF(FNBGSI(1:8).NE.'        ') THEN
          CALL FIXIO_R(TSFFCS,SMCFCS,SNOFCS,STCFCS,TG3FCS,ZORFCS,
     &                  CVFCS,CVBFCS,CVTFCS,ALBFCS(1,1),ALBFCS(1,2),
     &                  ALBFCS(1,3),ALBFCS(1,4),SLIFCS,VEGFCS,CNPFCS,
     &                  F10M,VETFCS,SOTFCS,ALFFCS(1,1),ALFFCS(1,2),
     &                  USTAR,FMM,FHH,SIHFCS,SICFCS,SITFCS,
     +                  TPRCP, SRFLAG,SWDFCS,SLCFCS,
     +                  VMNFCS,VMXFCS,SLPFCS,ABSFCS,T2M,Q2M,
     +                  OROG,zsoil,ivssfc,
     &                  LUGB,IDIM,JDIM,LSOIL,lonsperlat,idate,FNBGSI,
     &                  INPTYP,GFILEI)           
!add INPTYP,GFILEI

        ENDIF
        kmsk = nint(slmaskf)
        buff1 = orogf
        CALL interpred(1,kmsk,buff1,orog,idim,jdim,lonsperlat)
        buff1 = slmaskf
        CALL interpred(1,kmsk,buff1,slmask,idim,jdim,lonsperlat)
        if (use_ufo) then
          buff1 = orogf_uf
          CALL interpred(1,kmsk,buff1,orog_uf,idim,jdim,lonsperlat)
        else
          orog_uf = 0.0
        endif
!
      IF (GAUS) THEN
!
!     COMPUTE GAUSSIAN LATITUDE FOR MONITORING
!
        CALL GAULAT(GAUL,JDIM)
!
        II = 0
        DO J=1,JDIM
          JJ = J
          if (j .gt. jdim/2) jj = jdim - j + 1
          DX = 360. / FLOAT(lonsperlat(jj))
          DO I=1,lonsperlat(jj)
            II = II + 1
            RLA(II) = 90. - GAUL(J)
            RLO(II) = FLOAT(I-1)*DX
            IF(RLO(II).GT.180.) RLO(II) = RLO(II) - 360.
          ENDDO
        ENDDO
        LENSFC = II

        print *,' TOTAL Number of Points = ', LENSFC
!       do i=1,lensfc
!         print *,' I=',I,' RLO=',RLO(KK+I-1),' RLA=',RLA(KK+I-1)
!       enddo
      ELSE
!       COMPUTE REGULAR LATITUDE FOR MONITORING
        DX = 360. / FLOAT(IDIM)
        DY = 180.0 / FLOAT(JDIM-1)
        IF (BLTOUT .GT. 0.0) DY = - DY
        DO J=1,JDIM
          GAUL(J) = BLTOUT + (J-1) * DY
        ENDDO
        DO J=1,JDIM
          DO I=1,IDIM
            RLA((J-1)*IDIM+I) = GAUL(J)
            RLO((J-1)*IDIM+I) = BLNOUT + FLOAT(I-1)*DX
          ENDDO
        ENDDO
        LENSFC = IDIM*JDIM
      ENDIF
!     do i=1,idim*jdim
!         orogdif(i) = orog(i) - orog_uf(i)
!     enddo
!
      DO I=1,LENSFC
        WETFCS(I) = 0.
        AISFCS(I) = 0.
        IF(SLIFCS(I).EQ.2) AISFCS(I) = 1.
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
!
        CALL SFCCYCLE(LUGB,LENSFC,LSOIL,SIG1T,DELTSFC
     &,               IY,IM,ID,IH,FH
     &,               RLA, RLO
     &,               SLMASK,OROG, orog_uf, use_ufo, nst_anl
     &,               SIHFCS,SICFCS, SITFCS
     &,               SWDFCS,SLCFC
     &,               VMNFCS,VMXFCS,SLPFCS,ABSFCS
     &,               TSFFCS,SNOFCS,ZORFCS,ALBFC,TG3FCS
     &,               CNPFCS,SMCFC,STCFC,SLIFCS,AISFCS
     &,               F10M
     &,               VEGFCS,VETFCS,SOTFCS,ALFFC
     &,               CVFCS,CVBFCS,CVTFCS,me,nlunit,IALB,isot,ivegsrc)
!
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
!
!
!     IF (ZNLST) THEN
!       CALL ZNLSFC(SLIFCS,IDIM,JDIM,IJDIM,LSOIL,NRCZNL,
!    1              TSFFCS,WETFCS,SNOFCS,STCFCS,TG3FCS,
!    2              ZORFCS,CVFCS,CVBFCS,CVTFCS,
!    3              ALBFCS,AISFCS,SMCFCS,CNPFCS,VEGFCS,
!    &              vetfcs,sotfcs,alffcs,
!    4              ZNLSL,NZL1,NZL2,LSMSK,WEIS,GAUL,GAUS)
!       CALL ZNLODY(FH,ZNLSL,NZL1,NZL2,LSMSK,WEIS,
!    1              IDIM,JDIM,IDIMT,NRCZNL)
!     ENDIF
!
!
      print *,' fnbgso=',fnbgso
      print *,' idate=',idate,' im=',im,' jdim=',jdim,' lsoil=',lsoil
      IF(FNBGSO(1:8).NE.'        ') THEN
          idateo(1) = ih
          idateo(2) = im
          idateo(3) = id
          idateo(4) = iy
          CALL FIXIO_W(TSFFCS,SMCFCS,SNOFCS,STCFCS,TG3FCS,ZORFCS,
     &                 CVFCS,CVBFCS,CVTFCS,ALBFCS(1,1),ALBFCS(1,2),
     &                 ALBFCS(1,3),ALBFCS(1,4),SLIFCS,VEGFCS,CNPFCS,
     &                 F10M,VETFCS,SOTFCS,ALFFCS(1,1),ALFFCS(1,2),
     &                 USTAR,FMM,FHH,SIHFCS,SICFCS,SITFCS,
     +                 TPRCP,SRFLAG,SWDFCS,SLCFCS,
     +                 VMNFCS,VMXFCS,SLPFCS,ABSFCS,T2M,Q2M,
     +                 OROG,zsoil,ivssfc,
     &                 LUGB,IDIM,JDIM,LSOIL,
     &                 lonsperlat,fh,idateo,FNBGSO,OUTTYP,
     &                 INPTYP,GFILEI)
      ENDIF
!
      CALL nemsio_finalize()
!
      RETURN
      END SUBROUTINE SFCDRV
      SUBROUTINE ZNLSFC(SLIMSK,IDIM,JDIM,IJDIM,LSOIL,NRCZNL,
     &                  TSFANL,WETANL,SNOANL,STCANL,TG3ANL,
     &                  ZORANL,CVANL,CVBANL,CVTANL,
     &                  ALBANL,AISANL,SMCANL,CNPANL,VEGANL,
     &                  vetanl,sotanl,alfanl,
     &                  ZNLSL,NZL1,NZL2,LSMSK,WEIS,GAUL, GAUS)
!
      DIMENSION TSFANL(IDIM,JDIM),WETANL(IDIM,JDIM),SNOANL(IDIM,JDIM),
     &          STCANL(IDIM,JDIM,LSOIL),TG3ANL(IDIM,JDIM),
     &          ZORANL(IDIM,JDIM),
     &          CVANL (IDIM,JDIM),CVBANL(IDIM,JDIM),CVTANL(IDIM,JDIM),
     &          ALBANL(IDIM,JDIM,4),AISANL(IDIM,JDIM),
     &          SMCANL(IDIM,JDIM,LSOIL),CNPANL(IDIM,JDIM),
     &          VEGANL(IDIM,JDIM),vetanl(idim,jdim),sotanl(idim,jdim),
     &          alfanl(idim,jdim,2)
!
CFPP$ NOCONCUR R
      LOGICAL LSMSK(IDIM,2,6), GAUS
      DIMENSION ZNLSL(6,6,NRCZNL),WEIS(6,6)
!
      DIMENSION GAUL(JDIM), WGT(JDIM)
      DIMENSION WORK1(IDIM,2), WORK2(IDIM,2)
      DIMENSION SLIMSK(IDIM,JDIM)
!
      IDIMT=IDIM*2
!     JDIMHF=JDIM/2
!
      IF (GAUS) THEN
        JDIMHF = JDIM/2
        DO J=1,JDIM
          WGT(J)=COS((90.-GAUL(J))*0.01745329)
        ENDDO
      ELSE
        JDIMHF = JDIM/2 + 1
        DO J=1,JDIM
          WGT(J) = COS(GAUL(J)*0.01745329)
        ENDDO
        WGT(JDIMHF) = WGT(JDIMHF) * 0.5
      ENDIF
!
!     LAT LOOP
!
      DO LAT = 1,JDIMHF
        LATCO = JDIM + 1 - LAT
        IF (LAT .EQ. JDIMHF .AND. (.NOT. GAUS)) LATCO = LAT
!
!   ZONAL AVERAGE MONITORING
!
        DO I = 1, IDIM
          WORK1(I,1) = SLIMSK(I,LAT  )
          WORK1(I,2) = SLIMSK(I,LATCO)
          WORK2(I,1) = SNOANL(I,LAT )
          WORK2(I,2) = SNOANL(I,LATCO)
        ENDDO
        CALL ZNLWGT(WORK1,WORK2,WGT(LAT),LAT,JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
!
        DO I = 1, IDIM
          WORK1(I,1) = TSFANL(I,LAT  )
          WORK1(I,2) = TSFANL(I,LATCO)
        ENDDO
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 1, JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        DO I = 1, IDIM
          WORK1(I,1) = SMCANL(I,LAT,1)
          WORK1(I,2) = SMCANL(I,LATCO,1)
        ENDDO
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 2, JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        DO I = 1, IDIM
          WORK1(I,1) = SMCANL(I,LAT,LSOIL)
          WORK1(I,2) = SMCANL(I,LATCO,LSOIL)
        ENDDO
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 3, JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        DO I = 1, IDIM
          WORK1(I,1) = SNOANL(I,LAT       )
          WORK1(I,2) = SNOANL(I,LATCO)
        ENDDO
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 4, JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        DO I = 1, IDIM
          WORK1(I,1) = STCANL(I,LAT       ,1)
          WORK1(I,2) = STCANL(I,LATCO,1)
        ENDDO
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 5, JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        DO I = 1, IDIM
          WORK1(I,1) = STCANL(I,LAT       ,lsoil)
          WORK1(I,2) = STCANL(I,LATCO,lsoil)
        ENDDO
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 6, JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        DO I = 1, IDIM
          WORK1(I,1) = ZORANL(I,LAT       )
          WORK1(I,2) = ZORANL(I,LATCO)
        ENDDO
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 7, JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        IF (GAUS) THEN
          DO I = 1, IDIM
            WORK1(I,1) =   CVANL(I,LAT  )
            WORK1(I,2) =   CVANL(I,LATCO)
          ENDDO
          CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 8, JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
          DO I = 1, IDIM
            WORK1(I,1) = CVBANL(I,LAT  )
            WORK1(I,2) = CVBANL(I,LATCO)
          ENDDO
          CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 9, JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
          DO I = 1, IDIM
            WORK1(I,1) = CVTANL(I,LAT  )
            WORK1(I,2) = CVTANL(I,LATCO)
          ENDDO
          CALL ZNLAVS(WORK1  ,LAT,WGT(LAT),10,JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        ENDIF
        DO KK = 1, 4
         DO I = 1, IDIM
          WORK1(I,1) = ALBANL(I,LAT  ,kk)
          WORK1(I,2) = ALBANL(I,LATCO,kk)
         ENDDO
         num = 10 + kk
         CALL ZNLAVS(WORK1  ,LAT,WGT(LAT),num,JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        ENDDO
        DO I = 1, IDIM
          WORK1(I,1) = CNPANL(I,LAT  )
          WORK1(I,2) = CNPANL(I,LATCO)
        ENDDO
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT),15,JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        DO I = 1, IDIM
          WORK1(I,1) = VEGANL(I,LAT  )
          WORK1(I,2) = VEGANL(I,LATCO)
        enddo
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT),16,JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        DO I = 1, IDIM
          WORK1(I,1) = VEtANL(I,LAT  )
          WORK1(I,2) = VEtANL(I,LATCO)
        enddo
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT),17,JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        DO I = 1, IDIM
          WORK1(I,1) = sotANL(I,LAT  )
          WORK1(I,2) = sotANL(I,LATCO)
        enddo
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT),18,JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        DO I = 1, IDIM
          WORK1(I,1) = alfANL(I,LAT  ,1)
          WORK1(I,2) = alfANL(I,LATCO,1)
        enddo
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT),19,JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
        DO I = 1, IDIM
          WORK1(I,1) = alfANL(I,LAT  ,2)
          WORK1(I,2) = alfANL(I,LATCO,2)
        enddo
        CALL ZNLAVS(WORK1  ,LAT,WGT(LAT),20,JDIMHF
     1,           ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
!
      ENDDO
!
      RETURN
      END
      SUBROUTINE ZNLWGT(SLMSK,SHELEG,WGT,LAT,JDIMHF
     1,               ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
!
CFPP$ NOCONCUR R
      LOGICAL LSMSK(IDIMT,6)
      DIMENSION ZNLSL(6,6,NRCZNL),WEIS(6,6)
!
      DIMENSION SLMSK(1),SHELEG(1)
!
!     JDIMHF=JDIM/2
!
!  ZONAL AVERAGE WEIGHT
!
      NZL1=JDIM/6+1
      NZL1P=NZL1+1
      NZL2=JDIM/3+1
      NZL2P=NZL2+1
!
      N=IDIM+1
!
      IF(LAT.EQ.1) THEN
        DO J = 1, 6
          DO I = 1, 6
            WEIS(I,J) = 0.0
          ENDDO
        ENDDO
      ENDIF
!
!  BARE/SNOW SEA/LAND/ICE AVERAGES
!
      DO I = 1, IDIMT
        LSMSK(I,2) = (SLMSK (I).EQ.1.).AND. (SHELEG(I).LE.1.E-3)
        LSMSK(I,3) = (SLMSK (I).EQ.1.).AND. (SHELEG(I).GT.1.E-3)
        LSMSK(I,4) = (SLMSK (I).EQ.2.).AND. (SHELEG(I).LE.1.E-3)
        LSMSK(I,5) = (SLMSK (I).EQ.2.).AND. (SHELEG(I).GT.1.E-3)
        LSMSK(I,6) = SLMSK (I).EQ.0.0
      ENDDO
!
      IF(LAT.GE.1.AND.LAT.LE.NZL1) THEN
!
!  NORTHERN AND SOUTHERN POLAR REGION
!
        WEIS(2,1) = WEIS(2,1) +         FLOAT(IDIM)*WGT
        WEIS(6,1) = WEIS(6,1) +         FLOAT(IDIM)*WGT
        DO L=2,6
          ISUM = 0
          JSUM = 0
          DO I = 1, IDIM
            IF(LSMSK(I,L))      ISUM = ISUM + 1
            IF(LSMSK(I+IDIM,L)) JSUM = JSUM + 1
          ENDDO
          WEIS(2,L) = WEIS(2,L) + ISUM * WGT
          WEIS(6,L) = WEIS(6,L) + JSUM * WGT
        ENDDO
      ENDIF
!
!  NORTHERN AND SOUTHERN MIDDLE LATITUDES
!
      IF(LAT.GE.NZL1P.AND.LAT.LE.NZL2) THEN
        WEIS(3,1) = WEIS(3,1) +         FLOAT(IDIM)*WGT
        WEIS(5,1) = WEIS(5,1) +         FLOAT(IDIM)*WGT
        DO L=2,6
          ISUM = 0
          JSUM = 0
          DO I = 1, IDIM
            IF(LSMSK(I,L))      ISUM = ISUM + 1
            IF(LSMSK(I+IDIM,L)) JSUM = JSUM + 1
          ENDDO
          WEIS(3,L) = WEIS(3,L) + ISUM * WGT
          WEIS(5,L) = WEIS(5,L) + JSUM * WGT
        ENDDO
      ENDIF
!
      IF(LAT.GE.NZL2P) THEN
        WEIS(4,1) = WEIS(4,1) +         FLOAT(IDIMT)*WGT
        DO L=2,6
          ISUM = 0
          DO I = 1, IDIMT
            IF(LSMSK(I,L)) ISUM = ISUM + 1
          ENDDO
          WEIS(4,L) = WEIS(4,L) + ISUM * WGT
        ENDDO
      ENDIF
!
      IF(LAT.EQ.JDIMHF) THEN
        ZNLSL(1,1,NRCZNL) = 0.
        DO J=2,6
          ZNLSL(1,1,NRCZNL)=ZNLSL(1,1,NRCZNL)+WEIS(J,1)
        ENDDO
!*** NORMALIZE LATITUDE BAND WEIGHTS WITH GLOBAL SUM
        DO J=2,6
          ZNLSL(J,1,NRCZNL)=WEIS(J,1)/ZNLSL(1,1,NRCZNL)
        ENDDO
!
        DO J=2,6
          IF(WEIS(J,1) .NE. 0.0) THEN
            DO L=2,6
              ZNLSL(J,L,NRCZNL)=WEIS(J,L)/WEIS(J,1)
            ENDDO
          ELSE
            ZNLSL(J,L,NRCZNL)=999.0
          END IF
        ENDDO
        ZNLSL(1,1,NRCZNL)=1.0
!*** COMPUTE GLOBAL COVERAGES BY LATITUDE BAND WEIGHTING
        DO L = 2,6
          ZNLSL(1,L,NRCZNL) = 0.0
          DO J = 2,6
            ZNLSL(1,L,NRCZNL) = ZNLSL(1,L,NRCZNL)
     1                   + ZNLSL(J,L,NRCZNL)*ZNLSL(J,1,NRCZNL)
          ENDDO
        ENDDO
        DO L = 1,6
          DO J = 1,6
            ZNLSL(J,L,NRCZNL) = ZNLSL(J,L,NRCZNL)*100.0
          ENDDO
        ENDDO
        DO L=1,6
          WEIS(1,L)=0.0
          DO J=2,6
            WEIS(1,L)=WEIS(1,L)+WEIS(J,L)
          ENDDO
          DO J=1,6
            IF(WEIS(J,L).NE.0.0) WEIS(J,L)=1.0/WEIS(J,L)
          ENDDO
        ENDDO
      ENDIF
!
      RETURN
      END
      SUBROUTINE ZNLAVS(F,LAT,WGT,IND,JDIMHF
     1,               ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
!
CFPP$ NOCONCUR R
!
      LOGICAL LSMSK(IDIMT,6)
      DIMENSION ZNLSL(6,6,NRCZNL),WEIS(6,6)
!
      DIMENSION F(1)
!
      IF(IND.GT.NRCZNL) RETURN
!
!     JDIMHF=JDIM/2
!
      N=IDIM+1
!
      IF(LAT.EQ.1) THEN
        DO J = 1, 6
          DO I = 1, 6
            ZNLSL(I,J,IND) = 0.0
          ENDDO
        ENDDO
      ENDIF
!
      IF(LAT.LE.NZL1) THEN
        DO I = 1, IDIM
          ZNLSL(2,1,IND) = ZNLSL(2,1,IND) + F(I)      * WGT
          ZNLSL(6,1,IND) = ZNLSL(6,1,IND) + F(I+IDIM) * WGT
        ENDDO
        DO L=2,6
          DO I = 1, IDIM
            IF(LSMSK(I,L)) THEN
              ZNLSL(2,L,IND) = ZNLSL(2,L,IND) + F(I) * WGT
            ENDIF
            IF(LSMSK(I+IDIM,L)) THEN
              ZNLSL(6,L,IND) = ZNLSL(6,L,IND) + F(I+IDIM) * WGT
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
      IF(LAT.GT.NZL1.AND.LAT.LE.NZL2) THEN
        DO I = 1, IDIM
          ZNLSL(3,1,IND) = ZNLSL(3,1,IND) + F(I)      * WGT
          ZNLSL(5,1,IND) = ZNLSL(5,1,IND) + F(I+IDIM) * WGT
        ENDDO
        DO L=2,6
          DO I = 1, IDIM
            IF(LSMSK(I,L)) THEN
              ZNLSL(3,L,IND) = ZNLSL(3,L,IND) + F(I) * WGT
            ENDIF
            IF(LSMSK(I+IDIM,L)) THEN
              ZNLSL(5,L,IND) = ZNLSL(5,L,IND) + F(I+IDIM) * WGT
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
      IF(LAT.GT.NZL2) THEN
        DO I = 1, IDIMT
          ZNLSL(4,1,IND) = ZNLSL(4,1,IND) + F(I)      * WGT
        ENDDO
        DO L=2,6
          DO I = 1, IDIMT
            IF(LSMSK(I,L)) THEN
              ZNLSL(4,L,IND) = ZNLSL(4,L,IND) + F(I) * WGT
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
      IF(LAT.EQ.JDIMHF) THEN
        DO L=1,6
          ZNLSL(1,L,IND)=0.0
          DO J=2,6
            ZNLSL(1,L,IND) = ZNLSL(1,L,IND) + ZNLSL(J,L,IND)
          ENDDO
          DO J=1,6
            ZNLSL(J,L,IND) = ZNLSL(J,L,IND) * WEIS(J,L)
          ENDDO
        ENDDO
      ENDIF
!
      RETURN
      END
      SUBROUTINE ZNLODY(FH,
     1                ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
!
      LOGICAL LSMSK(IDIMT,6)
      DIMENSION ZNLSL(6,6,NRCZNL),WEIS(6,6)
!
!  THIS ROUTINE PRINTS OUT ZONAL AVERAGES
!
CFPP$ NOCONCUR R
!
      CHARACTER*21 IFMT0
      CHARACTER*31 IFMT1
      CHARACTER*37 IFMT2
      CHARACTER*7  NLAT(6)
!
      CHARACTER*8  LTTLSL(100)
      CHARACTER*8  LBLNK
      DIMENSION  IPWRSL(30)
!
      DATA LBLNK/'        '/
!
      DATA NLAT/'90N-90S','90N-60N','60N-30N','30N-30S','30S-60S',
     1          '60S-90S'/
!
      LTTLSL( 1) = 'TSFC    '
      LTTLSL( 2) = 'SOILM1  '
      LTTLSL( 3) = 'SOILM2  '
      LTTLSL( 4) = 'SNOW    '
      LTTLSL( 5) = 'TG1     '
      LTTLSL( 6) = 'TG2     '
      LTTLSL( 7) = 'ZORL    '
      LTTLSL( 8) = 'CV      '
      LTTLSL( 9) = 'CVB     '
      LTTLSL(10) = 'CVT     '
!     LTTLSL(11) = 'ALB     '
!     LTTLSL(12) = 'PLANTR  '
!     LTTLSL(13) = 'CNPWC   '
!     LTTLSL(14) = 'SLIMSK  '
!     DATA IPWRSL/0,0,0,1,0,0,0,0,0,0,-2,0,0,0,16*0/
      LTTLSL(11) = 'ALVSF   '
      LTTLSL(12) = 'ALVWF   '
      LTTLSL(13) = 'ALNSF   '
      LTTLSL(14) = 'ALNWF   '
      LTTLSL(15) = 'CNPWC   '
      LTTLSL(16) = 'VEGCOVR '
      LTTLSL(17) = 'VEGTYPE '
      LTTLSL(18) = 'SOILTYPE'
      LTTLSL(19) = 'FACSF   '
      LTTLSL(20) = 'FACWF   '
      LTTLSL(21) = 'SLIMSK  '
      DATA IPWRSL/0,0,0,1,0,0,0,0,0,0,4*-2,0,0,0,0,0,0,0,9*0/
!

!
!     JDIMHF=JDIM/2
!
      PRINT *,'@@@@ START OF ZONAL DIAGNOSTIC PRINT @@@'
!
!  SINGLE LEVEL FIELD
!
      K1 = 1
      K2 = 6
      KXXX = K2 - K1 + 1
      DO ITM=1,NRCZNL
        IF(LTTLSL(ITM).NE.LBLNK) THEN
          WRITE(6,100) LTTLSL(ITM),IPWRSL(ITM),FH
          WRITE(6,110)
          KYYY=-IPWRSL(ITM)
          WRITE(IFMT0,130) KYYY,KXXX
          WRITE(6,IFMT0)(NLAT(J),(ZNLSL(J,K,ITM),K=K1,K2),J=1,6)
        ENDIF
      ENDDO
!
      PRINT *,'@@@@ END OF ZONAL DIAGNOSTIC PRINT @@@'
!
  100 FORMAT(1X,A8,'(10**',I3,') FH=',F7.1)
  110 FORMAT(2X,' LAT ',7X,'MEAN',6X,'LND',3X,'SN-LND',6X,'ICE',
     1                  3X,'SN-ICE',6X,'SEA')
  130 FORMAT('(1X,A7,1X,',I2,'P',I2,'F9.2)')
      RETURN
      ENd
      SUBROUTINE MSKRD(LUGB,IDIM,JDIM,IJDIM,IY,IM,ID,IH,FH,
     1                 FNOROG,FNMASK,FNOROG_UF,KPDORO,KPDMSK,
     2                 OROG, SLMASK, orog_uf, GRBO, GRBM)
!
      use machine, kind_io => kind_io8
CFPP$ NOCONCUR R
!
      integer, parameter :: me=0
!     integer kind_io, me
!     parameter (kind_io=4, me=0)
      CHARACTER*500 FNOROG,FNMASK,FNOROG_UF
      real (kind=kind_io)  orog(ijdim),  slmask(ijdim), orog_uf(ijdim)
     &,                    blnm,         bltm
!
      LOGICAL LCLIM, GRBO, GRBM, gausm
!
      LCLIM=.TRUE.
!
!
      IF(FNMASK(1:8).NE.'        ') THEN    ! Read Land/sea mask
        IF (GRBM) THEN
          CALL FIXRDG(LUGB,IDIM,JDIM,FNMASK,
     &                KPDMSK,SLMASK,GAUSM,BLNM,BLTM,me)
        ELSE
          OPEN(UNIT=LUGB,FILE=FNMASK,STATUS='OLD',FORM='UNFORMATTED',
     1         ERR=900)
          WRITE(6,*) ' FILE ',trim(FNMASK),' opened. Unit=',LUGB
          READ(LUGB) SLMASK
  900     CONTINUE
          WRITE(6,*) ' ERROR IN OPENING FILE ',trim(FNMASK)
          PRINT *,'ERROR IN OPENING FILE ',trim(FNMASK)
          CALL ABORT
        ENDIF
        DO I=1,IJDIM
          SLMASK(I)=NINT(SLMASK(I))
        ENDDO
      ENDIF
!
      IF(FNOROG(1:8).NE.'        ') THEN    ! Read Orography
        IF (GRBO) THEN
          CALL FIXRDG(LUGB,IDIM,JDIM,FNOROG,
     &                KPDORO,OROG,GAUSM,BLNM,BLTM,me)
        ELSE
         OPEN(UNIT=LUGB,FILE=FNOROG,STATUS='OLD',FORM='UNFORMATTED',
     1         ERR=910)
         WRITE(6,*) ' FILE ',trim(FNOROG),' opened. Unit=',LUGB
         READ(LUGB) OROG
  910    CONTINUE
         WRITE(6,*) ' ERROR IN OPENING FILE ',trim(FNOROG)
         PRINT *,'ERROR IN OPENING FILE ',trim(FNOROG)
         CALL ABORT
        ENDIF
      ENDIF
      IF(FNOROG_UF(1:8).NE.'        ') THEN    ! Read Orography unfiltered
        IF (GRBO) THEN
          CALL FIXRDG(LUGB,IDIM,JDIM,FNOROG_UF,
     &                KPDORO,OROG_UF,GAUSM,BLNM,BLTM,me)
        ELSE
         OPEN(UNIT=LUGB,FILE=FNOROG_UF,STATUS='OLD',FORM='UNFORMATTED',
     1         ERR=920)
         WRITE(6,*) ' FILE ',trim(FNOROG),' opened. Unit=',LUGB
         READ(LUGB) OROG_UF
  920    CONTINUE
         WRITE(6,*) ' ERROR IN OPENING FILE ',trim(FNOROG_UF)
         PRINT *,'ERROR IN OPENING FILE ',trim(FNOROG_UF)
         CALL ABORT
        ENDIF
      ENDIF
!
      RETURN
      END
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
      END
      SUBROUTINE FIXIO_R(TSEA,SMC,SHELEG,STC,TG3,ZORL,CV,CVB,
     &           CVT,ALVSF,ALVWF,ALNSF,ALNWF,SLMSK,VFRAC,CANOPY,F10M,
!Cwu  add HICE & FICE
!    &           VTYPE,STYPE,FACSF,FACWF,UUSTAR,FFMM,FFHH,
     &           VTYPE,STYPE,FACSF,FACWF,UUSTAR,FFMM,FFHH,HICE ,FICE,
     &           TICE,
!Clu add TPRCP,SRFLAG,SNWDPH,SLC,SHDMIN,SHDMAX,SLOPE,SNOALB
     +           TPRCP, SRFLAG, SNWDPH, SLC,
     +           SHDMIN, SHDMAX, SLOPE, SNOALB, T2M, Q2M,   
     +           OROG,zsoil,ivssfc,
     &           nread,lonl,latd,lsoil,lonsperlat,idate,FNBGSI,
     &           INPTYP,GFILEI)
c
c***********************************************************************
c
      use sfcio_module
      USE NEMSIO_MODULE
      USE NEMSIO_GFS
      implicit none
      integer kind_io
      parameter (kind_io=4)
c
      integer lonl,latd,lsoil,lonsperlat(latd/2),ivssfc

      real SMC(LONL,LATD,LSOIL),STC(LONL,LATD,LSOIL),
     &     TSEA  (LONL,LATD),SHELEG(LONL,LATD),TG3   (LONL,LATD),
     &     ZORL  (LONL,LATD),CV    (LONL,LATD),CVB   (LONL,LATD),
     &     CVT   (LONL,LATD),ALVSF (LONL,LATD),ALVWF (LONL,LATD),
     &     ALNSF (LONL,LATD),ALNWF (LONL,LATD),SLMSK (LONL,LATD),
     &     VFRAC (LONL,LATD),CANOPY(LONL,LATD),F10M  (LONL,LATD),
     &     VTYPE (LONL,LATD),STYPE (LONL,LATD),FACSF (LONL,LATD),
     &     FACWF (LONL,LATD),UUSTAR(LONL,LATD),FFMM  (LONL,LATD),
     &     FFHH  (LONL,LATD)
!Cwu  add HICE & FICE
     &,    HICE(LONL,LATD),FICE(LONL,LATD),TICE(LONL,LATD)
!Clu  add TPRCP,SRFLAG,SNWDPH,SLC,SHDMIN,SHDMAX,SLOPE,SNOALB
     &,    TPRCP(LONL,LATD), SRFLAG(LONL,LATD),
     &     SNWDPH(LONL,LATD),SLC(LONL,LATD,LSOIL),
     &     SHDMIN(LONL,LATD),SHDMAX(LONL,LATD),
     &     SLOPE (LONL,LATD),SNOALB(LONL,LATD),
     &     T2M (LONL,LATD),Q2M(LONL,LATD),
     &     OROG  (LONL,LATD)

      real(kind=kind_io) buff1(LONL,LATD),buff2(LONL,LATD,LSOIL),
     &     buff4(LONL,LATD,4),xhour
      integer nread,i,j,k,ij,idate(4),lonsfc,latsfc
      integer kmsk(lonl,latd),kmskcv(lonl,latd)
      CHARACTER*8 labfix(4)
      CHARACTER*500 FNBGSI
      type(sfcio_head) head
      type(sfcio_data) data
      integer iret
      real (4) zsoil(lsoil)
!
!! Define variables for NEMSIO
      TYPE(NEMSIO_GFILE) :: GFILEI
      TYPE(NEMSIO_HEAD)  :: GHEADI
      TYPE(NEMSIO_HEADV) :: GHEADVI
      TYPE(NEMSIO_DATA)  :: GDATAI
      INTEGER :: IRET1, IMI, JMI
      CHARACTER(8) :: FILETYPE
!!
      INTEGER NSFCO,INPTYP


!! Determine SFCI file format (sfcio or nemsio)

      CALL SFCIO_SROPEN(nread,fnbgsi,IRET)
      CALL SFCIO_SRHEAD(nread,head,IRET1)
      IF(IRET==0 .and. IRET1==0) THEN
        INPTYP=2
      ELSE
        CALL NEMSIO_OPEN(GFILEI,fnbgsi,'read',IRET=IRET)
        CALL NEMSIO_GETFILEHEAD(GFILEI,GTYPE=FILETYPE)
        IF (trim(FILETYPE).EQ.'NEMSIO' .AND. IRET.EQ.0) THEN
          INPTYP=1
          print *,'NEMSIO: SFCI is a nemsio file'
        ELSE
          INPTYP=0
          NSFCO=0
          print *,'NEMSIO: SFCI file format unknown'
        ENDIF
      ENDIF

      IF ( INPTYP .EQ. 1 ) THEN                       ! NEMSIO

C  GET SURFACE HEADER
        CALL NEMSIO_GETFILEHEAD(GFILEI
     &,        IDATE=GHEADI%IDATE
     &,        NFHOUR=GHEADI%NFHOUR
     &,        NFMINUTE=GHEADI%NFMINUTE
     &,        NFSECONDN=GHEADI%NFSECONDN
     &,        NFSECONDD=GHEADI%NFSECONDD
     &,        VERSION=GHEADI%VERSION
     &,        NREC=GHEADI%NREC
     &,        DIMX=GHEADI%LONB
     &,        DIMY=GHEADI%LATB
     &,        DIMZ=GHEADI%DIMZ
     &,        NSOIL=GHEADI%NSOIL
!     &,        extrameta=GHEADI%extrameta
!     &,        nmetavari=GHEADI%nmetavari
!     &,        nmetavarr=GHEADI%nmetavarr
!     &,        nmetaaryi=GHEADI%nmetaaryi
!     &,        nmetaaryr=GHEADI%nmetaaryr
     &,        IRET=IRET)
        if (GHEADI%LONB.ne.lonl.or.GHEADI%LATB.ne.latd
     &      .or. GHEADI%NSOIL .ne. lsoil) then
         WRITE(6,*) ' ERROR IN SURFACE FILE DIMENSIONS ',
     &           GHEADI%LONB,GHEADI%LATB,GHEADI%NSOIL
         CALL ABORT
        endif

        CALL NEMSIO_GETHEADVAR(GFILEI,'irealf',GHEADI%irealf,IRET)
        ALLOCATE(GHEADVI%LPL((GHEADI%LATB+1)/2) )
        ALLOCATE(GHEADVI%ZSOIL(GHEADI%NSOIL))
        CALL NEMSIO_GETHEADVAR(GFILEI,'lpl',GHEADVI%LPL,IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'zsoil',GHEADVI%ZSOIL,IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'IVSSFC',GHEADI%IVS,IRET)
        CALL NEMSIO_GETHEADVAR(GFILEI,'fhour',GHEADI%fhour,IRET)
        if(iret/=0) then
           gheadi%fhour=real(GHEADI%NFHOUR,4)+real(GHEADI%NFMINUTE,4)
     &      /60.+real(GHEADI%NFSECONDN,4)/real(GHEADI%NFSECONDD,4)/3600.
        endif
!

        idate      = GHEADI%IDATE(1:4)
!HL        idate(1)   = GHEADI%IDATE(4)
!        idate(4)   = GHEADI%IDATE(1)
        lonsperlat = GHEADVI%LPL
!        lonsperlat(:) = 384
        zsoil      = GHEADVI%ZSOIL
        ivssfc     = GHEADI%IVS
!
        PRINT 99,nread,GHEADI%fhour,idate,
     &         GHEADI%LONB,GHEADI%LATB,ivssfc
!
!        if(GHEADI%nmetavari>0) then
!          ALLOCATE(GHEADVI%variname(GHEADVI%nmetavari),
!     &     GHEADI%varival(GHEADI%nmetavari) )
!          CALL NEMSIO_GETFILEHEAD(GFILEI,variname=variname,
!     &         varival=varival, iret=iret)
!        endif
!        if(GHEADI%nmetavarr>0) then
!          ALLOCATE(GHEADVr%varrname(GHEADVI%nmetavarr),
!     &     GHEADI%varrval(GHEADI%nmetavarr) )
!          CALL NEMSIO_GETFILEHEAD(GFILEI,varrname=varrname,
!     &         varrval=varrval, iret=iret)
!        endif
!
!        if(GHEADI%nmetaaryi>0) then
!          ALLOCATE(GHEADVI%aryiname(GHEADVI%nmetaaryi),
!     &     GHEADI%aryilen(GHEADI%nmetaaryi) )
!          CALL NEMSIO_GETFILEHEAD(GFILEI,aryiname=aryiname,
!     &         aryilen=aryilen, iret=iret)
!          ALLOCATE(GHEADVI%aryival(maxval(GHEADVI%aryilen),
!     &     GHEADI%nmetaaryi))
!          CALL NEMSIO_GETFILEHEAD(GFILEI,
!     &         aryival=GHEADVI%aryival,iret=iret)
!       print *,'af getaryiname=',GHEADVI%aryival,,iret1=',iret1
!        endif
!        if(GHEADI%nmetaaryr>0) then
!          ALLOCATE(GHEADVI%aryrname(GHEADVI%nmetaaryr),
!     &     GHEADI%aryrlen(GHEADI%nmetaaryr))
!          CALL NEMSIO_GETFILEHEAD(GFILEI,aryrname=GHEADI%aryrname,
!     &         aryrlen=GHEADVI%aryrlen, iret=iret)
!          ALLOCATE(GHEADVI%aryrval(maxval(GHEADVI%aryrlen),
!     &     GHEADI%nmetaaryr))
!          CALL NEMSIO_GETFILEHEAD(GFILEI,aryrval=GHEADVI%aryrval,
!     &         iret=iret)
!        endif
!
        IMI=GHEADI%LONB
        JMI=GHEADI%LATB
!
        call nemsio_gfs_alsfc(IMI,JMI,GHEADI%NSOIL,GDATAI)
        call nemsio_gfs_rdsfc(GFILEI,GDATAI,iret)
!
        kmsk=nint(GDATAI%SLMSK(:,:))
C
        CALL interpred(1,kmsk,GDATAI%tsea,TSEA,lonl,latd,lonsperlat)

        DO K=1,LSOIL
          CALL interpred(1,kmsk,GDATAI%smc(1,1,k),smc(1,1,k)
     &,                  lonl,latd,lonsperlat)
        ENDDO

        CALL interpred(1,kmsk,GDATAI%sheleg,SHELEG,lonl,latd,lonsperlat)

        DO K = 1, LSOIL
          CALL interpred(1,kmsk,GDATAI%stc(1,1,k),stc(1,1,k)
     &,                lonl,latd,lonsperlat)
        ENDDO
        CALL interpred(1,kmsk,GDATAI%tg3,TG3,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%zorl,ZORL,lonl,latd,lonsperlat)
!
        CALL interpred(1,kmsk,GDATAI%alvsf,ALVSF,lonl,latd,lonsperlat)
        CALL interpred(1,kmsk,GDATAI%alvwf,ALVWF,lonl,latd,lonsperlat)
        CALL interpred(1,kmsk,GDATAI%alnsf,ALNSF,lonl,latd,lonsperlat)
        CALL interpred(1,kmsk,GDATAI%alnwf,ALNWF,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%slmsk,SLMSK,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%vfrac,VFRAC,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%canopy,CANOPY,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%f10m,F10M,lonl,latd,lonsperlat)
        CALL interpred(1,kmsk,GDATAI%vtype,VTYPE,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%stype,STYPE,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%facsf,FACSF,lonl,latd,lonsperlat)
        CALL interpred(1,kmsk,GDATAI%facwf,FACWF,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%uustar,UUSTAR,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%ffmm,FFMM,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%ffhh,FFHH,lonl,latd,lonsperlat)

        if (GHEADI%ivs >= 200501) then
        CALL interpred(1,kmsk,GDATAI%hice,HICE,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%fice,FICE,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%tprcp,TPRCP,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%srflag,SRFLAG,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%snwdph,SNWDPH,lonl,latd,lonsperlat)

         DO K = 1, LSOIL
            CALL interpred(1,kmsk,GDATAI%slc(1,1,k),slc(1,1,k)
     &,                  lonl,latd,lonsperlat)
         ENDDO

        CALL interpred(1,kmsk,GDATAI%shdmin,SHDMIN,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%shdmax,SHDMAX,lonl,latd,lonsperlat)

         CALL interpred(1,kmsk,GDATAI%slope,SLOPE,lonl,latd,lonsperlat)

        CALL interpred(1,kmsk,GDATAI%snoalb,SNOALB,lonl,latd,lonsperlat)

         CALL interpred(1,kmsk,GDATAI%orog,OROG,lonl,latd,lonsperlat)

         if (GHEADI%ivs >= 200509) then
           CALL interpred(1,kmsk,GDATAI%t2m,T2M,lonl,latd,lonsperlat)

           CALL interpred(1,kmsk,GDATAI%q2m,Q2M,lonl,latd,lonsperlat)

           CALL interpred(1,kmsk,GDATAI%TISFC,TICE,lonl,latd,lonsperlat)
          endif
        endif

      ELSEIF ( INPTYP .EQ. 2 ) THEN                            ! SFCIO

! nemsio

      call sfcio_srohdc(nread,fnbgsi,head,data,iret)


      print *,' IRET after sfcio_srohdc'
      if (iret .ne. 0) then
        WRITE(6,*) ' ERROR IN OPENING FILE ',trim(FNBGSI),' iret=',iret
        CALL ABORT
      endif
      print *,' FNBGSI=',trim(FNBGSI)
      if (head%lonb.ne.lonl.or.head%latb.ne.latd.or.
     &   head%lsoil .ne. lsoil) then
        WRITE(6,*) ' ERROR IN SURFACE FILE DIMENSIONS ',
     &           head%lonb,head%latb,head%lsoil
        CALL ABORT
      endif

      idate      = head%idate
      lonsperlat = head%lpl
      zsoil      = head%zsoil
      ivssfc     = head%ivs
!
      PRINT 99,nread,head%fhour,head%IDATE,
     &         head%lonb,head%latb,head%ivs
99    FORMAT(' in fixio nread=',i3,2x,'HOUR=',f6.2,3x,'IDATE=',
     &4(1X,I4),4x,'lonsfc,latsfc,ivssfc=',3i8)
!
      kmsk=nint(data%slmsk(:,:))
!      print *,'kmsk=',maxval(kmsk),minval(kmsk)

      CALL interpred(1,kmsk,data%tsea,TSEA,lonl,latd,lonsperlat)

      DO K=1,LSOIL
        CALL interpred(1,kmsk,data%smc(1,1,k),smc(1,1,k)
     &,                lonl,latd,lonsperlat)
      ENDDO

      CALL interpred(1,kmsk,data%sheleg,SHELEG,lonl,latd,lonsperlat)

      DO K = 1, LSOIL
        CALL interpred(1,kmsk,data%stc(1,1,k),stc(1,1,k)
     &,                lonl,latd,lonsperlat)
      ENDDO
      CALL interpred(1,kmsk,data%tg3,TG3,lonl,latd,lonsperlat)

      CALL interpred(1,kmsk,data%zorl,ZORL,lonl,latd,lonsperlat)

      CALL interpred(1,kmsk,data%alvsf,ALVSF,lonl,latd,lonsperlat)
      CALL interpred(1,kmsk,data%alvwf,ALVWF,lonl,latd,lonsperlat)
      CALL interpred(1,kmsk,data%alnsf,ALNSF,lonl,latd,lonsperlat)
      CALL interpred(1,kmsk,data%alnwf,ALNWF,lonl,latd,lonsperlat)

      CALL interpred(1,kmsk,data%slmsk,SLMSK,lonl,latd,lonsperlat)

      CALL interpred(1,kmsk,data%vfrac,VFRAC,lonl,latd,lonsperlat)

      CALL interpred(1,kmsk,data%canopy,CANOPY,lonl,latd,lonsperlat)

      CALL interpred(1,kmsk,data%f10m,F10M,lonl,latd,lonsperlat)
      CALL interpred(1,kmsk,data%vtype,VTYPE,lonl,latd,lonsperlat)

      CALL interpred(1,kmsk,data%stype,STYPE,lonl,latd,lonsperlat)

      CALL interpred(1,kmsk,data%facsf,FACSF,lonl,latd,lonsperlat)
      CALL interpred(1,kmsk,data%facwf,FACWF,lonl,latd,lonsperlat)

      CALL interpred(1,kmsk,data%uustar,UUSTAR,lonl,latd,lonsperlat)

      CALL interpred(1,kmsk,data%ffmm,FFMM,lonl,latd,lonsperlat)

      CALL interpred(1,kmsk,data%ffhh,FFHH,lonl,latd,lonsperlat)

      if (head%ivs >= 200501) then
!Cwu add HICE and FICE 
        CALL interpred(1,kmsk,data%hice,HICE,lonl,latd,lonsperlat)
!       print *, 'CYCLE: read record 23 hice'

        CALL interpred(1,kmsk,data%fice,FICE,lonl,latd,lonsperlat)
!       print *, 'CYCLE: read record 24 fice'

!Clu add TPRCP,SRFLAG,SNWDPH,SLC,SHDMIN,SHDMAX,SLOPE,SNOALB
        CALL interpred(1,kmsk,data%tprcp,TPRCP,lonl,latd,lonsperlat)
!       print *, 'CYCLE: read record 25 tprcp'

        CALL interpred(1,kmsk,data%srflag,SRFLAG,lonl,latd,lonsperlat)
!       print *, 'CYCLE: read record 26 srflag'

        CALL interpred(1,kmsk,data% snwdph,SNWDPH,lonl,latd,lonsperlat)
!       print *, 'CYCLE: read record 27 snwddh'

        DO K = 1, LSOIL
          CALL interpred(1,kmsk,data%slc(1,1,k),slc(1,1,k)
     &,                  lonl,latd,lonsperlat)
!         print *, 'CYCLE: read record 28 slc k=', k
        ENDDO

        CALL interpred(1,kmsk,data%shdmin,SHDMIN,lonl,latd,lonsperlat)
!       print *, 'CYCLE: read record 29 shdmin'

        CALL interpred(1,kmsk,data%shdmax,SHDMAX,lonl,latd,lonsperlat)
!       print *, 'CYCLE: read record 30 shdmax'

        CALL interpred(1,kmsk,data%slope,SLOPE,lonl,latd,lonsperlat)
!       print *, 'CYCLE: read record 31 slope'

        CALL interpred(1,kmsk,data%snoalb,SNOALB,lonl,latd,lonsperlat)
!       print *, 'CYCLE: read record 32 snoalb'

        CALL interpred(1,kmsk,data%orog,OROG,lonl,latd,lonsperlat)

        if (head%ivs >= 200509) then
          CALL interpred(1,kmsk,data%T2M,T2M,lonl,latd,lonsperlat)

          CALL interpred(1,kmsk,data%Q2M,Q2M,lonl,latd,lonsperlat)

          CALL interpred(1,kmsk,data%TISFC,TICE,lonl,latd,lonsperlat)
        endif
      endif
!
      ENDIF

      RETURN
      END

c
c***********************************************************************
c
      SUBROUTINE FIXIO_W(TSEA,SMC,SHELEG,STC,TG3,ZORL,CV,CVB,
     &           CVT,ALVSF,ALVWF,ALNSF,ALNWF,SLMSK,VFRAC,CANOPY,F10M,
!Cwu  add HICE & FICE
!    &           VTYPE,STYPE,FACSF,FACWF,UUSTAR,FFMM,FFHH,
     &           VTYPE,STYPE,FACSF,FACWF,UUSTAR,FFMM,FFHH,HICE, FICE,
     &           TICE,
!Clu add TPRCP,SRFLAG,SNWDPH,SLC,SHDMIN,SHDMAX,SLOPE,SNOALB
     +           TPRCP, SRFLAG, SNWDPH, SLC,
     +           SHDMIN, SHDMAX, SLOPE, SNOALB, T2M, Q2M,
     +           OROG,zsoil,ivssfc,
     &           nw,lonl,latd,lsoil,lonsperlat,xhour,idate,FNBGSO,
     &           OUTTYP,
!HL add INPTYP and GFILEI
     &           INPTYP,GFILEI)

c
c***********************************************************************
c
      use sfcio_module
      USE NEMSIO_MODULE
      USE NEMSIO_GFS
      implicit none
      integer kind_io
      parameter (kind_io=4)
c
      integer OUTTYP,INPTYP
      TYPE(NEMSIO_GFILE) :: GFILEI

      integer lonl,latd,lsoil,lonsperlat(latd/2),ivssfc
      real SMC(LONL,LATD,LSOIL),STC(LONL,LATD,LSOIL),
!Cwu  add HICE & FICE
     &     HICE(LONL,LATD),FICE(LONL,LATD),TICE(LONL,LATD),
     &     TSEA  (LONL,LATD),SHELEG(LONL,LATD),TG3   (LONL,LATD),
     &     ZORL  (LONL,LATD),CV    (LONL,LATD),CVB   (LONL,LATD),
     &     CVT   (LONL,LATD),ALVSF (LONL,LATD),ALVWF (LONL,LATD),
     &     ALNSF (LONL,LATD),ALNWF (LONL,LATD),SLMSK (LONL,LATD),
     &     VFRAC (LONL,LATD),CANOPY(LONL,LATD),F10M  (LONL,LATD),
     &     VTYPE (LONL,LATD),STYPE (LONL,LATD),FACSF (LONL,LATD),
     &     FACWF (LONL,LATD),UUSTAR(LONL,LATD),FFMM  (LONL,LATD),
     &     FFHH  (LONL,LATD),xhour
!Clu  add TPRCP,SRFLAG,SNWDPH,SLC,SHDMIN,SHDMAX,SLOPE,SNOALB
     +,    TPRCP(LONL,LATD),  SRFLAG(LONL,LATD),
     +     SNWDPH(LONL,LATD), SLC(LONL,LATD,LSOIL),
     +     SHDMIN(LONL,LATD), SHDMAX(LONL,LATD),
     +     SLOPE(LONL,LATD),  SNOALB(LONL,LATD),
     &     T2M (LONL,LATD),Q2M(LONL,LATD),
     +     OROG (LONL,LATD)

      real(kind=kind_io) buff1(LONL,LATD),buff2(LONL,LATD,LSOIL),
     &     buff4(LONL,LATD,4),yhour
      integer nw,i,j,k,idate(4)
      CHARACTER*8 labfix(4)
      integer kmsk(lonl,latd),kmskcv(lonl,latd)
      CHARACTER*500 FNBGSO
      type(sfcio_head) head
      type(sfcio_data) data
      integer iret
      real (4) zsoil(lsoil)

!! Define variables for NEMSIO
      TYPE(NEMSIO_GFILE) :: GFILEO
      TYPE(NEMSIO_HEAD)  :: GHEADO
      TYPE(NEMSIO_HEADV) :: GHEADVO
      TYPE(NEMSIO_DATA)  :: GDATAO
      INTEGER            :: IMO,JMO,NREC_SFC
      real(kind=kind_io) tmp(lonl,latd)
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      ifinput1: IF(INPTYP==2) then
!sfcio input
        head%clabsfc=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//
     &               CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
        head%fhour=xhour
        head%idate=idate
        head%latb=latd
        head%lonb=lonl
        head%ivs=ivssfc
        head%lsoil=lsoil
        call sfcio_alhead(head,iret)
        head%lpl     = lonsperlat(1:latd/2)
        head%zsoil   = zsoil
!
        PRINT 99,nw,head%fhour,head%IDATE,
     &           head%lonb,head%latb,head%ivs
99      FORMAT(' in fixio nw=',i3,2x,'HOUR=',f6.2,3x,'IDATE=',
     &  4(1X,I4),4x,'lonsfc,latsfc,ivssfc=',3i8)
!
!        call sfcio_aldata(head,data,iret)
        if(OUTTYP==2) then
          call sfcio_aldata(head,data,iret)
        elseif(OUTTYP==1)then
          call nemsio_gfs_alsfc(lonl,latd,LSOIL,GDATAO)
        endif
!
      ELSEIF(INPTYP==1) THEN
!nemsio input
        gheado%dimx=lonl
        gheado%dimy=latd
        gheado%nsoil=lsoil
        call  nemsio_gfs_alsfc(lonl,latd,lsoil,gdatao)
!
      ENDIF ifinput1

      kmsk=nint(SLMSK)

!      CALL uninterpred(1,kmsk,TSEA,data%tsea,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,TSEA,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%tsea=tmp
      elseif(outtyp==1) then
        gdatao%tsea=tmp
      endif

      DO k=1,LSOIL
        CALL uninterpred(1,kmsk,smc(1,1,k),tmp
     &,                  lonl,latd,lonsperlat)
        if(outtyp==2) then
          data%smc(:,:,k)=tmp
        elseif(outtyp==1) then
          gdatao%smc(:,:,k)=tmp
        endif
      ENDDO

      CALL uninterpred(1,kmsk,SHELEG,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%sheleg=tmp
      elseif(outtyp==1) then
        gdatao%sheleg=tmp
      endif

      DO k=1,LSOIL
        CALL uninterpred(1,kmsk,stc(1,1,k),tmp
     &,                  lonl,latd,lonsperlat)
        if(outtyp==2) then
          data%stc(:,:,k)=tmp
        elseif(outtyp==1) then
          gdatao%stc(:,:,k)=tmp
        endif
      ENDDO

!      CALL uninterpred(1,kmsk,TG3,data%tg3,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,TG3,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%tg3=tmp
      elseif(outtyp==1) then
        gdatao%tg3=tmp
      endif

!      CALL uninterpred(1,kmsk,ZORL,data%zorl,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,ZORL,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%zorl=tmp
      elseif(outtyp==1) then
        gdatao%zorl=tmp
      endif

!      CALL uninterpred(1,kmsk,ALVSF,data%alvsf,lonl,latd,lonsperlat)
!      CALL uninterpred(1,kmsk,ALVWF,data%alvwf,lonl,latd,lonsperlat)
!      CALL uninterpred(1,kmsk,ALNSF,data%alnsf,lonl,latd,lonsperlat)
!      CALL uninterpred(1,kmsk,ALNWF,data%alnwf,lonl,latd,lonsperlat)
! add CV, CVT, CVB
      CALL uninterpred(1,kmsk,CV,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%cv=tmp
      endif
!
      CALL uninterpred(1,kmsk,CVT,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%cvt=tmp
      endif
!
      CALL uninterpred(1,kmsk,CVB,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%cvb=tmp
      endif
!
      CALL uninterpred(1,kmsk,ALVSF,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%alvsf=tmp
      elseif(outtyp==1) then
        gdatao%alvsf=tmp
      endif

      CALL uninterpred(1,kmsk,ALVWF,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%alvwf=tmp
      elseif(outtyp==1) then
        gdatao%alvwf=tmp
      endif
      CALL uninterpred(1,kmsk,ALNSF,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%alnsf=tmp
      elseif(outtyp==1) then
        gdatao%alnsf=tmp
      endif
      CALL uninterpred(1,kmsk,ALNWF,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%alnwf=tmp
      elseif(outtyp==1) then
        gdatao%alnwf=tmp
      endif

      CALL uninterpred(1,kmsk,SLMSK,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%slmsk=tmp
      elseif(outtyp==1) then
        gdatao%slmsk=tmp
      endif

!      CALL uninterpred(1,kmsk,SLMSK,data%slmsk,lonl,latd,lonsperlat)

!      CALL uninterpred(1,kmsk,VFRAC,data%vfrac,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,VFRAC,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%vfrac=tmp
      elseif(outtyp==1) then
        gdatao%vfrac=tmp
      endif

!      CALL uninterpred(1,kmsk,CANOPY,data%canopy,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,CANOPY,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%canopy=tmp
      elseif(outtyp==1) then
        gdatao%canopy=tmp
      endif

!      CALL uninterpred(1,kmsk,F10M,data%f10m,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,F10M,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%f10m=tmp
      elseif(outtyp==1) then
        gdatao%f10m=tmp
      endif

!      CALL uninterpred(1,kmsk,VTYPE,data%vtype,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,VTYPE,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%vtype=tmp
      elseif(outtyp==1) then
        gdatao%vtype=tmp
      endif

!      CALL uninterpred(1,kmsk,STYPE,data%stype,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,STYPE,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%stype=tmp
      elseif(outtyp==1) then
        gdatao%stype=tmp
      endif

!      CALL uninterpred(1,kmsk,FACSF,data%facsf,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,FACSF,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%facsf=tmp
      elseif(outtyp==1) then
        gdatao%facsf=tmp
      endif

!      CALL uninterpred(1,kmsk,FACWF,data%facwf,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,FACWF,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%facwf=tmp
      elseif(outtyp==1) then
        gdatao%facwf=tmp
      endif

!      CALL uninterpred(1,kmsk,UUSTAR,data%uustar,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,UUSTAR,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%uustar=tmp
      elseif(outtyp==1) then
        gdatao%uustar=tmp
      endif

!      CALL uninterpred(1,kmsk,FFMM,data%ffmm,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,FFMM,tmp,lonl,latd,lonsperlat)
      if(outtyp==2) then
        data%ffmm=tmp
      elseif(outtyp==1) then
        gdatao%ffmm=tmp
      endif

!      CALL uninterpred(1,kmsk,FFHH,buff1,lonl,latd,lonsperlat)
      CALL uninterpred(1,kmsk,FFHH,buff1,lonl,latd,lonsperlat)
      if(outtyp==1) then
!????        gdatao%ffhh=buff1
        gdatao%ffhh=0
      endif

      if (ivssfc >= 200501) then
!Cwu   add HICE & FICE

!        CALL uninterpred(1,kmsk,HICE,data%hice,lonl,latd,lonsperlat)

!        CALL uninterpred(1,kmsk,FICE,data%fice,lonl,latd,lonsperlat)
        CALL uninterpred(1,kmsk,HICE,tmp,lonl,latd,lonsperlat)
        if(outtyp==2) then
          data%hice=tmp
        elseif(outtyp==1) then
          gdatao%hice=tmp
        endif

        CALL uninterpred(1,kmsk,FICE,tmp,lonl,latd,lonsperlat)
        if(outtyp==2) then
          data%fice=tmp
        elseif(outtyp==1) then
          gdatao%fice=tmp
        endif


!Clu  add tprcp,srflag,snwdph,slc,shdmin,shdmax,slope,snoalb
!        CALL uninterpred(1,kmsk,TPRCP,data%tprcp,lonl,latd,lonsperlat)
!        CALL uninterpred(1,kmsk,SRFLAG,data%srflag,lonl,latd,lonsperlat)
!        CALL uninterpred(1,kmsk,SNWDPH,data%snwdph,lonl,latd,lonsperlat)

        CALL uninterpred(1,kmsk,TPRCP,tmp,lonl,latd,lonsperlat)
        if(outtyp==2) then
          data%tprcp=tmp
        elseif(outtyp==1) then
          gdatao%tprcp=tmp
        endif

        CALL uninterpred(1,kmsk,SRFLAG,tmp,lonl,latd,lonsperlat)
        if(outtyp==2) then
          data%srflag=tmp
        elseif(outtyp==1) then
          gdatao%srflag=tmp
        endif

        CALL uninterpred(1,kmsk,SNWDPH,tmp,lonl,latd,lonsperlat)
        if(outtyp==2) then
          data%snwdph=tmp
        elseif(outtyp==1) then
          gdatao%snwdph=tmp
        endif


        DO k=1,LSOIL
          CALL uninterpred(1,kmsk,SLC(1,1,k),tmp
     &,                    lonl,latd,lonsperlat)
          if(outtyp==2) then
            data%slc(:,:,k)=tmp
          elseif(outtyp==1) then
            gdatao%slc(:,:,k)=tmp
          endif

        ENDDO

!        CALL uninterpred(1,kmsk,SHDMIN,data%shdmin,lonl,latd,lonsperlat)
!        CALL uninterpred(1,kmsk,SHDMAX,data%shdmax,lonl,latd,lonsperlat)
!        CALL uninterpred(1,kmsk,SLOPE,data%slope,lonl,latd,lonsperlat)
!       CALL uninterpred(1,kmsk,SNOALB,data%snoalb,lonl,latd,lonsperlat)
!        CALL uninterpred(1,kmsk,OROG,data%orog,lonl,latd,lonsperlat)

        CALL uninterpred(1,kmsk,SHDMIN,tmp,lonl,latd,lonsperlat)
        if(outtyp==2) then
          data%shdmin=tmp
        elseif(outtyp==1) then
          gdatao%shdmin=tmp
        endif

        CALL uninterpred(1,kmsk,SHDMAX,tmp,lonl,latd,lonsperlat)
        if(outtyp==2) then
          data%shdmax=tmp
        elseif(outtyp==1) then
          gdatao%shdmax=tmp
        endif

        CALL uninterpred(1,kmsk,SLOPE,tmp,lonl,latd,lonsperlat)
        if(outtyp==2) then
          data%slope=tmp
        elseif(outtyp==1) then
          gdatao%slope=tmp
        endif

        CALL uninterpred(1,kmsk,SNOALB,tmp,lonl,latd,lonsperlat)
        if(outtyp==2) then
          data%snoalb=tmp
        elseif(outtyp==1) then
          gdatao%snoalb=tmp
        endif

        CALL uninterpred(1,kmsk,OROG,tmp,lonl,latd,lonsperlat)
        if(outtyp==2) then
          data%orog=tmp
        elseif(outtyp==1) then
          gdatao%orog=tmp
        endif

!        if (head%ivs >= 200509) then
!          CALL uninterpred(1,kmsk,T2M,data%T2M,lonl,latd,lonsperlat)
!
!          CALL uninterpred(1,kmsk,Q2M,data%Q2M,lonl,latd,lonsperlat)
!
!          CALL uninterpred(1,kmsk,TICE,data%TISFC,lonl,latd,lonsperlat)
        if (ivssfc >= 200509) then
          CALL uninterpred(1,kmsk,T2M,tmp,lonl,latd,lonsperlat)
          if(outtyp==2) then
            data%t2m=tmp
          elseif(outtyp==1) then
            gdatao%t2m=tmp
          endif

          CALL uninterpred(1,kmsk,Q2M,tmp,lonl,latd,lonsperlat)
          if(outtyp==2) then
            data%q2m=tmp
          elseif(outtyp==1) then
            gdatao%q2m=tmp
          endif

          CALL uninterpred(1,kmsk,TICE,tmp,lonl,latd,lonsperlat)
          if(outtyp==2) then
            data%tisfc=tmp
          elseif(outtyp==1) then
            gdatao%tisfc=tmp
          endif
        endif
      endif


      ifinput2: IF(INPTYP==2) then
!
!** sfcio input to nemsio output
!
      IF (OUTTYP .EQ. 2)  THEN

        print *, 'NEMSIO: write out sfcio file'

        call sfcio_swohdc(nw,fnbgso,head,data,iret)
        if(iret.ne.0) then
          WRITE(6,*) ' ERROR IN OPENING FILE ',trim(FNBGSO)
          CALL ABORT
        endif

      ELSEIF ( OUTTYP .EQ. 1 ) THEN

        print *, 'NEMSIO: write out nemsio file,inptyp=',inptyp
! prepare nemsio sfc output
! header
        IMO=lonl
        JMO=latd
        GHEADO%DIMX=IMO
        GHEADO%DIMY=JMO
        GHEADO%NFHOUR=INT(xhour)
        GHEADO%NFMINUTE=INT((xhour-GHEADO%NFHOUR)*60)
        GHEADO%NFSECONDN=INT((xhour-GHEADO%NFHOUR-
     &        GHEADO%NFMINUTE/60.)*3600*100)
        GHEADO%NFSECONDD=100
        GHEADO%IDATE=0
        GHEADO%IDATE(1:4)=IDATE(1:4)
        GHEADO%IDATE(1)=IDATE(4)
        GHEADO%IDATE(4)=IDATE(1)
        GHEADO%NSOIL=LSOIL
!
        ALLOCATE(GHEADVO%ZSOIL(GHEADO%NSOIL))
        IF(IVSSFC.GT.0) THEN
            GHEADO%IVS=IVSSFC
            IF(LSOIL.GT.0) THEN
              IF(LSOIL.EQ.2.OR.LSOIL.EQ.4)THEN
                GHEADO%NSOIL=LSOIL
              ELSE
                PRINT '("  NUMBER OF SOIL LAYERS MUST BE 2 OR 4. ")'
                CALL ERREXIT(9)
              ENDIF
            ENDIF
            IF (GHEADO%IVS < 200501) THEN
              GHEADO%NSOIL=2 ! MUST USE 2 LAYERS.
            END IF
        ENDIF

        PRINT 99,nw,GHEADO%NFHOUR,GHEADO%IDATE(1:4),
     &           GHEADO%DIMX,GHEADO%DIMY,GHEADO%IVS


        GHEADO%irealf=head%irealf

!other meta variables
        GHEADO%EXTRAMETA=.true.
!
        GHEADO%NMETAVARI=5
        ALLOCATE(GHEADVO%VARINAME(GHEADO%NMETAVARI))
        ALLOCATE(GHEADVO%VARIVAL(GHEADO%NMETAVARI))
        GHEADVO%VARINAME(1:GHEADO%NMETAVARI)=(/'LATB   '
     &    ,'LONB   ','IREALF ','LSOIL  ','IVSSFC ' /)
        GHEADVO%VARIVAL(1:GHEADO%NMETAVARI)=(/latd,
     &    lonl,GHEADO%irealf,lsoil,IVSSFC /)
!
        GHEADO%NMETAVARR=1
        ALLOCATE(GHEADVO%VARRNAME(GHEADO%NMETAVARR))
        ALLOCATE(GHEADVO%VARRVAL(GHEADO%NMETAVARR))
        GHEADVO%VARRNAME(1:GHEADO%NMETAVARR)=(/'FHOUR  '/)
        GHEADVO%VARRVAL(1:GHEADO%NMETAVARR)=(/XHOUR/)
!
        GHEADO%NMETAARYI=1
        ALLOCATE(GHEADVO%ARYINAME(GHEADO%NMETAARYI))
        ALLOCATE(GHEADVO%ARYILEN(GHEADO%NMETAARYI))
        GHEADVO%ARYINAME(1:GHEADO%NMETAARYI)=(/'lpl '/)
        GHEADVO%ARYILEN(1:GHEADO%NMETAARYI)=
     &       (/size(lonsperlat)/)
        ALLOCATE(GHEADVO%ARYIVAL(maxval(GHEADVO%ARYILEN(1:
     &       GHEADO%NMETAARYI)),GHEADO%NMETAARYI))
        GHEADVO%ARYIVAL(1:GHEADVO%ARYILEN(1),1)=
     &   lonsperlat(1:size(lonsperlat))
!
        GHEADO%NMETAARYR=1
        ALLOCATE(GHEADVO%ARYRNAME(GHEADO%NMETAARYR))
        ALLOCATE(GHEADVO%ARYRLEN(GHEADO%NMETAARYR))
        GHEADVO%ARYRNAME(1:GHEADO%NMETAARYR)=(/'zsoil'/)
        GHEADVO%ARYRLEN(1:GHEADO%NMETAARYR)=size(ZSOIL)

        ALLOCATE(GHEADVO%ARYRVAL(maxval(GHEADVO%ARYRLEN(1:
     &       GHEADO%NMETAARYR)),GHEADO%NMETAARYR))
        GHEADVO%ARYRVAL(1:GHEADVO%ARYRLEN(1),1)=ZSOIL(1:
     &       LSOIL)

!
        NREC_SFC=32+3*GHEADO%NSOIL
        GHEADO%NREC=NREC_SFC
        ALLOCATE(GHEADVO%RECNAME(NREC_SFC))
        ALLOCATE(GHEADVO%RECLEVTYP(NREC_SFC))
        ALLOCATE(GHEADVO%RECLEV(NREC_SFC))
!record name
        GHEADVO%RECNAME(1)='tmp'
        GHEADVO%RECNAME(2)='weasd'
        GHEADVO%RECNAME(3)='tg3'
        GHEADVO%RECNAME(4)='sfcr'
!        GHEADVO%RECNAME(5)='tcdc'
!        GHEADVO%RECNAME(6)='pres'
!        GHEADVO%RECNAME(7)='pres'
        GHEADVO%RECNAME(5)='alvsf'
        GHEADVO%RECNAME(6)='alvwf'
        GHEADVO%RECNAME(7)='alnsf'
        GHEADVO%RECNAME(8)='alnwf'
        GHEADVO%RECNAME(9)='land'
        GHEADVO%RECNAME(10)='veg'
        GHEADVO%RECNAME(11)='cnwat'
        GHEADVO%RECNAME(12)='f10m'
        GHEADVO%RECNAME(13)='tmp'
        GHEADVO%RECNAME(14)='spfh'
        GHEADVO%RECNAME(15)='vtype'
        GHEADVO%RECNAME(16)='sotyp'
        GHEADVO%RECNAME(17)='facsf'
        GHEADVO%RECNAME(18)='facwf'
        GHEADVO%RECNAME(19)='fricv'
        GHEADVO%RECNAME(20)='ffmm'
        GHEADVO%RECNAME(21)='ffhh'
        GHEADVO%RECNAME(22)='icetk'
        GHEADVO%RECNAME(23)='icec'
        GHEADVO%RECNAME(24)='tisfc'
        GHEADVO%RECNAME(25)='tprcp'
        GHEADVO%RECNAME(26)='crain'
        GHEADVO%RECNAME(27)='snod'
        GHEADVO%RECNAME(28)='shdmin'
        GHEADVO%RECNAME(29)='shdmax'
        GHEADVO%RECNAME(30)='sltyp'
        GHEADVO%RECNAME(31)='salbd'
        GHEADVO%RECNAME(32)='orog'
        GHEADVO%RECNAME(33:32+GHEADO%NSOIL)='smc'
        GHEADVO%RECNAME(GHEADO%NSOIL+33:32+2*GHEADO%NSOIL)='stc'
        GHEADVO%RECNAME(2*GHEADO%NSOIL+33:32+3*GHEADO%NSOIL)=
     &      'slc'
!
        GHEADVO%RECLEVTYP(1:32)='sfc'
        GHEADVO%RECLEVTYP(12)='10 m above gnd'
        GHEADVO%RECLEVTYP(13)='2 m above gnd'
        GHEADVO%RECLEVTYP(14)='2 m above gnd'
        GHEADVO%RECLEVTYP(33:NREC_SFC)='soil layer'

        GHEADVO%RECLEV(1:32)=1
        DO K=1,GHEADO%NSOIL
          GHEADVO%RECLEV(K+32)=K
          GHEADVO%RECLEV(GHEADO%NSOIL+K+32)=K
          GHEADVO%RECLEV(2*GHEADO%NSOIL+K+32)=K
        ENDDO
!
!** write out nemsio sfc file
!
!      print *,'output nemsio,nrec=',GHEADO%NREC,
!     & 'size(recname)=',size(GHEADVO%RECNAME)
!         print*, '2249'
!HL         print*,GHEADO%IDATE, GHEADO%NFHOUR
      CALL NEMSIO_OPEN(GFILEO,TRIM(fnbgso),'write'
     &,        MODELNAME="GFS"
     &,        GDATATYPE="bin4"
     &,        NFHOUR=GHEADO%NFHOUR
     &,        NFMINUTE=GHEADO%NFMINUTE
     &,        NFSECONDN=GHEADO%NFSECONDN
     &,        NFSECONDD=GHEADO%NFSECONDD
     &,        IDATE=GHEADO%IDATE
     &,        NREC=GHEADO%NREC
     &,        DIMX=GHEADO%DIMX
     &,        DIMY=GHEADO%DIMY
     &,        DIMZ=GHEADO%DIMZ
     &,        NSOIL=GHEADO%NSOIL
     &,        NMETA=5
     &,        RECNAME=GHEADVO%RECNAME
     &,        RECLEVTYP=GHEADVO%RECLEVTYP
     &,        RECLEV=GHEADVO%RECLEV
     &,        EXTRAMETA=GHEADO%EXTRAMETA
     &,        NMETAVARI=GHEADO%NMETAVARI
     &,        NMETAVARR=GHEADO%NMETAVARR
     &,        NMETAARYI=GHEADO%NMETAARYI
     &,        NMETAARYR=GHEADO%NMETAARYR
     &,        VARINAME=GHEADVO%VARINAME
     &,        VARIVAL=GHEADVO%VARIVAL
     &,        VARRNAME=GHEADVO%VARRNAME
     &,        VARRVAL=GHEADVO%VARRVAL
     &,        ARYINAME=GHEADVO%ARYINAME
     &,        ARYILEN=GHEADVO%ARYILEN
     &,        ARYIVAL=GHEADVO%ARYIVAL
     &,        ARYRNAME=GHEADVO%ARYRNAME
     &,        ARYRLEN=GHEADVO%ARYRLEN
     &,        ARYRVAL=GHEADVO%ARYRVAL
     &,        IRET=IRET)
        IF(IRET.NE.0) THEN
          PRINT*, ' ERROR AT NEMSIO_OPEN ', trim(fnbgso)
          CALL ERREXIT(4)
        ENDIF
!
       ENDIF    !end output if
!
      ELSEIF(INPTYP==1) then
!nemsio input
        IMO=lonl
        JMO=latd
        GHEADO%DIMX=IMO
        GHEADO%DIMY=JMO
        GHEADO%NFHOUR=INT(xhour)
        GHEADO%NFMINUTE=INT((xhour-GHEADO%NFHOUR)*60)
        GHEADO%NFSECONDN=INT((xhour-GHEADO%NFHOUR-
     &        GHEADO%NFMINUTE/60.)*3600*100)
        GHEADO%NFSECONDD=100
        GHEADO%IDATE=0
        GHEADO%IDATE(1:4)=IDATE(1:4)
        GHEADO%IDATE(1)=IDATE(4)
        GHEADO%IDATE(4)=IDATE(1)+INT(xhour)
        GHEADO%NSOIL=LSOIL
!
        GFILEO=GFILEI
!
!** nemsio sfc to nemsio sfc

        print*, '2295',IDATE
        print*, '2296',GHEADO%IDATE, GHEADO%NFHOUR
        CALL NEMSIO_OPEN(GFILEO,TRIM(fnbgso),'write'
     &,        IDATE=GHEADO%IDATE,NFHOUR=GHEADO%NFHOUR
     &,        NFMINUTE=GHEADO%NFMINUTE
     &,        NFSECONDN=GHEADO%NFSECONDN
     &,        NFSECONDD=GHEADO%NFSECONDD
     &,        NSOIL=GHEADO%NSOIL,iret=iret)
        if(iret/=0)print *,'af nemsio write open,iret=',iret
!
!** end inpif

      ENDIF ifinput2
!
      IF (OUTTYP==1) then
       call nemsio_gfs_wrtsfc(GFILEO,GDATAO,iret)
       if(iret/=0) print *,'af nemsio write wrt,iret=',iret
       call nemsio_close(GFILEO,iret=iret)
       if(iret/=0) print *,'after nemsio_close,iret=',iret
!
       if(INPTYP==1) then
         call nemsio_close(GFILEI,iret=iret)
       endif
       call nemsio_gfs_axsfc(GDATAO)
       call nemsio_gfs_axheadv(GHEADVO)

!      call sfcio_swohdc(nw,fnbgso,head,data,iret)
!      if(iret.ne.0) then
!        WRITE(6,*) ' ERROR IN OPENING FILE ',trim(FNBGSO)
!        CALL ABORT
!      endif
      ENDIF
      RETURN
      END

      subroutine interpred(iord,kmsk,f,fi,lonl,latd,lonsperlat)
        implicit none
        integer kind_io
        parameter (kind_io=4)
        integer,intent(in):: iord
        integer,intent(in):: lonl
        integer,intent(in):: latd
        integer,intent(in):: lonsperlat(latd/2)
        integer,intent(in):: kmsk(lonl,latd)
        real(kind=kind_io),intent(in):: f(lonl,latd)
        real,intent(out):: fi(lonl*latd)
!       real(kind=kind_io),intent(out):: fi(lonl*latd)
        real (4) ftem(lonl)
        integer j,lons,jj,latd2,ii,i
        latd2 = latd / 2
        ii    = 1
        do j=1,latd
          jj = j
          if (j .gt. latd2) jj = latd - j + 1
          lons=lonsperlat(jj)
          if(lons.ne.lonl) then
            call intlon(iord,1,1,lonl,lons,kmsk(1,j),f(1,j),ftem)
            fi(ii:ii+lons-1) = ftem(1:lons)
          else
            do i=1,lonl
              fi(ii+i-1)  = f(i,j)
            enddo
          endif
          ii = ii + lons
        enddo
      end subroutine

      subroutine uninterpred(iord,kmsk,fi,f,lonl,latd,lonsperlat)
        implicit none
        integer kind_io
        parameter (kind_io=4)
        integer,intent(in):: iord
        integer,intent(in):: lonl
        integer,intent(in):: latd
        integer,intent(in):: lonsperlat(latd/2)
        integer,intent(in):: kmsk(lonl*latd)
        real,intent(in):: fi(lonl*latd)
!       real(kind=kind_io),intent(in):: fi(lonl*latd)
        real(kind=kind_io),intent(out):: f(lonl,latd)
        real (4) ftem(lonl)
        integer j,lons,jj,latd2,ii,i
        latd2 = latd / 2
        ii    = 1
        do j=1,latd
          jj = j
          if (j .gt. latd2) jj = latd - j + 1
          lons=lonsperlat(jj)
          if(lons.ne.lonl) then
            ftem(1:lons) = fi(ii:ii+lons-1)
            call intlon(iord,1,1,lons,lonl,kmsk(ii),ftem,f(1,j))
          else
            do i=1,lonl
              f(i,j)  = fi(ii+i-1)
            enddo
          endif
          ii = ii + lons
        enddo
      end subroutine
      subroutine intlon(iord,imon,imsk,m1,m2,k1,f1,f2)
        implicit none
        integer,intent(in):: iord,imon,imsk,m1,m2
        integer,intent(in):: k1(m1)
        real(4),intent(in):: f1(m1)
        real(4),intent(out):: f2(m2)
        integer i2,in,il,ir
        real r,x1
        r=real(m1)/real(m2)
        do i2=1,m2
          x1=(i2-1)*r
          il=int(x1)+1
          ir=mod(il,m1)+1
          if(iord.eq.2.and.(imsk.eq.0.or.k1(il).eq.k1(ir))) then
            f2(i2)=f1(il)*(il-x1)+f1(ir)*(x1-il+1)
          else
            in=mod(nint(x1),m1)+1
            f2(i2)=f1(in)
          endif
        enddo
      end subroutine
