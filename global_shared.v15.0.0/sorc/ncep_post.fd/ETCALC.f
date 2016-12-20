      SUBROUTINE ETCALC(ETA,ETP,ESD,VEGFAC,ISOIL,SMC,CMC,        &
     &                  EC,EDIR,ETRANS,ESNOW,SMCDRY,SMCMAX)
! ----------------------------------------------------------------------
! PROGRAM HISTORY LOG:
!   03-01-17  M EK AND H CHUANG - LIFTED IT FROM MODEL FOR POST
! ----------------------------------------------------------------------
! DETERMINE INDIVIDUAL COMPONENTS OF SURFACE EVAPORATION
! INPUT:
!   ETA    = TOTAL SURFACE EVAPORATION (W/m2)
!   ETP    = POTENTIAL EVAPORATION (W/m2)
!   ESD    = WATER EQUIVALENT SNOW DEPTH (m)
!   VEGFAC = GREEN VEGETATION FRACTION (fraction ...or percent?)
!   ISOIL  = SOIL TYPE (1-19)
!   SMC    = UPPER SOIL LAYER (0-10 CM) SOIL MOISTURE (VOLUMETRIC)
!   CMC    = CANOPY WATER CONTENT (m)
! OUTPUT:
!   EC     = EVAPORATION OF CANOPY WATER (W/m2)
!   EDIR   = DIRECT SOIL EVAPORATION (W/m2)
!   ETRANS = TRANSPIRATION (W/m2)
!   ESNOW  = SNOW SUBLIMATION (W/m2)
! ----------------------------------------------------------------------
      implicit none
!
      integer,parameter :: nosoiltype=19
!jw
      integer,intent(in) :: ISOIL
      real,intent(in) :: ETA,ETP,ESD,VEGFAC,SMC
      real,intent(inout) :: CMC
      real,intent(out) ::  EC,EDIR,ETRANS,ESNOW
      real SMCDRY,SMCMAX,SRATIO,CMCMAX,CFACTR
      real fx,fxexp,spatio
      REAL SMDRY(nosoiltype),SMMAX(nosoiltype)

      DATA CFACTR,CMCMAX /0.5,0.5E-3/
! ----------------------------------------------------------------------
! SOIL TYPES   ZOBLER (1986), COSBY ET AL (1984)
!   1   SAND
!   2   LOAMY SAND
!   3   SANDY LOAM
!   4   SILT LOAM
!   5   SILT
!   6   LOAM
!   7   SANDY CLAY LOAM
!   8   SILTY CLAY LOAM
!   9   CLAY LOAM
!  10   SANDY CLAY
!  11   SILTY CLAY
!  12   CLAY
!  13   ORGANIC MATERIAL
!  14   WATER
!  15   BEDROCK
!  16   OTHER(land-ice)
!  17   PLAYA
!  18   LAVA
!  19   WHITE SAND
! ----------------------------------------------------------------------
      DATA SMDRY /0.023, 0.028, 0.047, 0.084, 0.084, 0.066,             &
     &           0.069, 0.120, 0.103, 0.100, 0.126, 0.135,              &
     &           0.069, 0.000, 0.012, 0.028, 0.135, 0.012,              &
     &           0.023/
!
      DATA SMMAX /0.395, 0.421, 0.434, 0.476, 0.476, 0.439,             &
     &           0.404, 0.464, 0.465, 0.406, 0.468, 0.457,              &
     &           0.464, 0.000, 0.200, 0.421, 0.457, 0.200,              &
     &           0.395/
!
      DATA FXEXP /2.0/

! ----------------------------------------------------------------------
! INITIALIZE EVAPORATION COMPONENTS
! ----------------------------------------------------------------------
      EC     = 0.0
      EDIR   = 0.0
      ETRANS = 0.0
      ESNOW  = 0.0

! ----------------------------------------------------------------------
! SET SMCDRY AND SMCMAX VALUES
! ----------------------------------------------------------------------
      SMCDRY = SMDRY(ISOIL)
      SMCMAX = SMMAX(ISOIL)

! ----------------------------------------------------------------------
! DETERMINE INDIVIDUAL COMPONENTS OF EVAPORATION
! NO SURFACE EVAPORATION COMPONENTS IF POTENTIAL (ETP)<0
! IF SNOW ON THE GROUND (ESD>0), ALL EVAPORATION IS SNOW SUBLIMATION,
! ELSE IT IT A SUM OF CANOPY EVAP, DIRECT SOIL EVAP AND TRANSPIRATION
! ----------------------------------------------------------------------
      IF (ETP .GT. 0.) THEN      
        IF (ESD .GT. 0.) THEN
          ESNOW = ETA
        ELSE

! ----------------------------------------------------------------------
! CANOPY EVAPORATION
! ----------------------------------------------------------------------
          IF (CMC .GT. 0) THEN
            IF (CMC .GT. CMCMAX) CMC = CMCMAX
            EC = VEGFAC*((CMC/CMCMAX)**CFACTR)*ETP
          ENDIF

! ----------------------------------------------------------------------
! DIRECT SOIL EVAPORATION A FUNCTION OF RELATIVE SOIL MOISTURE
! AVAILABILITY, LINEAR WHEN FXEXP=1.
! ----------------------------------------------------------------------
          SRATIO = (SMC-SMCDRY)/(SMCMAX-SMCDRY)
          IF (SRATIO .GT. 0.) THEN
            FX = SRATIO**FXEXP
            FX = MAX(0.,MIN(FX,1.))
          ELSE
            FX = 0.
          ENDIF
          EDIR = FX*(1.0-VEGFAC)*ETP

! ----------------------------------------------------------------------
! CALCULATE TRANSPIRATION AS A RESIDUAL OF THE TOTAL MINUS EDIR AND EC
! ----------------------------------------------------------------------
          ETRANS = ETA - EDIR - EC
        ENDIF
        IF (ETRANS .LT. 0.) ETRANS = 0.

      ENDIF

      RETURN
      END
