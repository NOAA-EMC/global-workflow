      SUBROUTINE CANRES(SOLAR,SFCTMP,Q2,SFCPRS,SMC,  &
     &                  GC,RC,IVEG,ISOIL,            &
     &                  RSMIN,NROOTS,SMCWLT,SMCREF,  &
     &                  RCS,RCQ,RCT,RCSOIL,SLDPTH)
!
! ######################################################################
!                        SUBROUTINE CANRES
!                        -----------------
!       THIS ROUTINE CALCULATES THE CANOPY RESISTANCE WHICH DEPENDS ON
!       INCOMING SOLAR RADIATION, AIR TEMPERATURE, ATMOSPHERIC WATER
!       VAPOR PRESSURE DEFICIT AT THE LOWEST MODEL LEVEL, AND SOIL
!       MOISTURE (PREFERABLY UNFROZEN SOIL MOISTURE RATHER THAN TOTAL)
! ----------------------------------------------------------------------
!        SOURCE:  JARVIS (1976), JACQUEMIN AND NOILHAN (1990 BLM)
! ----------------------------------------------------------------------
! PROGRAM HISTORY LOG:
!   03-01-17  M EK AND H CHUANG - LIFTED IT FROM MODEL FOR POST 
! ----------------------------------------------------------------------
! INPUT:  SOLAR: INCOMING SOLAR RADIATION
! 	  CH:	  SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE
! 	  SFCTMP: AIR TEMPERATURE AT 1ST LEVEL ABOVE GROUND
! 	  Q2:	  AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
! 	  Q2SAT:  SATURATION AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
! 	  SFCPRS: SURFACE PRESSURE
! 	  SMC:    VOLUMETRIC SOIL MOISTURE 
! 	  ZSOIL:  SOIL DEPTH (NEGATIVE SIGN, AS IT IS BELOW GROUND)
! 	  NSOIL:  NO. OF SOIL LAYERS
! 	  IROOT:  NO. OF SOIL LAYERS IN ROOT ZONE (1.LE.NROOT.LE.NSOIL)
! 	  XLAI:   LEAF AREA INDEX
! 	  SMCWLT: WILTING POINT
! 	  SMCREF: REFERENCE SOIL MOISTURE
! 		  (WHERE SOIL WATER DEFICIT STRESS SETS IN)
!
! RSMIN, RSMAX, TOPT, RGL, HS: CANOPY STRESS PARAMETERS SET IN
! SUBROUTINE REDPRM
!
!  (SEE EQNS 12-14 AND TABLE 2 OF SEC. 3.1.2 OF 
!       CHEN ET AL., 1996, JGR, VOL 101(D3), 7251-7268)               
!
!        OUTPUT:  PC: PLANT COEFFICIENT
!                 RC: CANOPY RESISTANCE
!                 GC: CANOPY CONDUCTANCE
! ----------------------------------------------------------------------
! ######################################################################

     use params_mod, only: xlai, pq0, a2, a3, a4
     use ctlblk_mod, only: novegtype, nsoil, ivegsrc
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!      integer,parameter :: nosoiltype=19,novegtype=27
      integer,parameter :: nosoiltype=19
      INTEGER N,K
      INTEGER, allocatable:: IROOT(:)
      INTEGER,intent(in) :: IVEG,ISOIL
      INTEGER,intent(out) :: NROOTS

      real,intent(in) :: SOLAR,SFCTMP,Q2,SFCPRS
      real,dimension(NSOIL),intent(in) :: SMC,SLDPTH
      real,intent(out) :: RCT,RCS,RCQ,RCSOIL,GC,RC,SMCWLT,SMCREF,  &
       RSMIN

      REAL CH
      REAL ZSOIL(NSOIL), PART(NSOIL)
      REAL, allocatable :: RSMN(:),RGL(:),HS(:)
      REAL SMWLT(nosoiltype),SMREF(nosoiltype)     &
     & ,Q2SAT
      REAL TOPT,RSMAX,FF
      REAL P,QS,GX,TAIR4,ST1,SLVCP,RR,DELTA,TBLO,            &
           RCMIN,RCMAX

      DATA RSMAX /5000./
      DATA TOPT /298.0/

!      DATA IROOT /1,3,3,3,3,3,3,3,3,3,4,4,4,4,4,0,2,2,1,3,         &
!     &            3,3,2,1,1,1,1/

!  SSIB VEGETATION TYPES (DORMAN AND SELLERS, 1989; JAM)
!
!   1   Urban and Built-Up Land
!   2   Dryland Cropland and Pasture
!   3   Irrigated Cropland and Pasture
!   4   Mixed Dryland/Irrigated Cropland and Pasture
!   5   Cropland/Grassland Mosaic
!   6   Cropland/Woodland Mosaic
!   7   Grassland
!   8   Shrubland
!   9   Mixed Shrubland/Grassland
!  10   Savanna
!  11   Deciduous Broadleaf Forest
!  12   Deciduous Needleleaf Forest
!  13   Evergreen Broadleaf Forest
!  14   Evergreen Needleleaf Forest
!  15   Mixed Forest
!  16   Water Bodies
!  17   Herbaceous Wetland
!  18   Wooded Wetland
!  19   Barren or Sparsely Vegetated
!  20   Herbaceous Tundra
!  21   Wooded Tundra
!  22   Mixed Tundra
!  23   Bare Ground Tundra
!  24   Snow or Ice
!  25   Playa
!  26   Lava
!  27   White Sand
!
!      DATA RSMN /200.0,  70.0,  70.0,  70.0,  70.0,  70.0,      &  
!     &            70.0, 300.0, 170.0,  70.0, 100.0, 150.0,      &
! MEK MAY 2007
! increase evergreen forest and mixed forest
!     &           150.0, 125.0, 125.0, 100.0,  40.0, 100.0,
!     &           150.0, 250.0, 150.0, 100.0,  40.0, 100.0,      &
!     &           300.0, 150.0, 150.0, 150.0, 200.0, 200.0,      &
!     &            40.0, 100.0, 300.0/
!     
!      DATA RGL /100.0, 100.0, 100.0, 100.0, 100.0,  65.0,       &
!     &          100.0, 100.0, 100.0,  65.0,  30.0,  30.0,       &
!     &           30.0,  30.0,  30.0,  30.0, 100.0,  30.0,       &
!     &          100.0, 100.0, 100.0, 100.0, 100.0, 100.0,       &
!     &          100.0, 100.0, 100.0/
!     
!      DATA HS /42.00, 36.25, 36.25, 36.25, 36.25, 44.14,        &
!     &         36.35, 42.00, 39.18, 54.53, 54.53, 47.35,        &
!     &         41.69, 47.35, 51.93, 51.75, 60.00, 51.93,        &
!     &         42.00, 42.00, 42.00, 42.00, 42.00, 42.00,        &
!     &         36.25, 42.00, 42.00/
!
! SOIL TYPES   ZOBLER (1986)      COSBY ET AL (1984) (quartz cont.(1))
! ----  -------
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

!      DATA SMREF /0.196, 0.248, 0.282, 0.332, 0.332, 0.301,
!     &           0.293, 0.368, 0.361, 0.320, 0.388, 0.389,
!     &           0.319, 0.000, 0.116, 0.248, 0.389, 0.116,
!     &           0.196/
! MEK MAY 2007
      DATA SMREF /0.236, 0.283, 0.312, 0.360, 0.360, 0.329,        &
     &            0.315, 0.387, 0.382, 0.338, 0.404, 0.403,        &
     &            0.348, 0.000, 0.133, 0.283, 0.403, 0.133,        &
     &            0.236/
!     
      DATA SMWLT /0.023, 0.028, 0.047, 0.084, 0.084, 0.066,       &
     &           0.069, 0.120, 0.103, 0.100, 0.126, 0.135,        &
     &           0.069, 0.000, 0.012, 0.028, 0.135, 0.012,        &
     &           0.023/



! ----------------------------------------------------------------------
! INITIALIZE CANOPY CONDUCTANCE TERMS
! ----------------------------------------------------------------------
! allocate data for IROOT RSMN RGL HS
      allocate(IROOT(novegtype))
      allocate(RSMN(novegtype))
      allocate(RGL(novegtype))
      allocate(HS(novegtype))
      if(ivegsrc==1)then  !IGBP veg type
       IROOT=(/4,4,4,4,4,3,3,3,3,3,3,3,1,3,2,3,0,3,3,2/)
       RSMN=(/300.0, 300.0, 300.0, 175.0, 175.0, 225.0, &
             225.0,  70.0,  70.0,  70.0,  40.0,  70.0, &
             400.0,  70.0, 200.0, 400.0, 100.0, 225.0, &
             150.0, 200.0/)
       RGL=(/30.0,  30.0,  30.0,  30.0,  30.0, 100.0,  &
     &          100.0,  65.0,  65.0, 100.0, 100.0, 100.0, & 
     &          100.0, 100.0, 100.0, 100.0,  30.0, 100.0, &
     &          100.0, 100.0/)
       HS=(/47.35, 41.69, 47.35, 54.53, 51.93, 42.00, &
     &         42.00, 42.00, 42.00, 36.35, 60.00, 36.25, &
     &         42.00, 36.25, 42.00, 42.00, 51.75, 42.00, &
     &         42.00, 42.00/)
      else ! asuume the other type is USGS veg type
       IROOT=(/1,3,3,3,3,3,3,3,3,3,4,4,4,4,4,0,2,2,1,3, &
              3,3,2,1/)
       RSMN=(/200.0,  70.0,  70.0,  70.0,  70.0,  70.0, &
     &            70.0, 300.0, 170.0,  70.0, 100.0, 150.0, &
! MEK MAY 2007
! increase evergreen forest and mixed forest
!     &           150.0, 125.0, 125.0, 100.0,  40.0, 100.0,
     &           150.0, 250.0, 150.0, 100.0,  40.0, 100.0,  &
     &           300.0, 150.0, 150.0, 150.0, 200.0, 200.0/) 
       RGL=(/100.0, 100.0, 100.0, 100.0, 100.0,  65.0,  &
     &          100.0, 100.0, 100.0,  65.0,  30.0,  30.0, &
     &           30.0,  30.0,  30.0,  30.0, 100.0,  30.0, &
     &          100.0, 100.0, 100.0, 100.0, 100.0, 100.0/) 
       HS=(/42.00, 36.25, 36.25, 36.25, 36.25, 44.14, &
     &         36.35, 42.00, 39.18, 54.53, 54.53, 47.35, &
     &         41.69, 47.35, 51.93, 51.75, 60.00, 51.93, &
     &         42.00, 42.00, 42.00, 42.00, 42.00, 42.00/) 
      end if

      RCS = 0.0
      RCT = 0.0
      RCQ = 0.0
      RCSOIL = 0.0
      RC = 0.0

!      ZSOIL(1)=-0.1
!      ZSOIL(2)=-0.4
!      ZSOIL(3)=-1.0
!      ZSOIL(4)=-2.0
      
      DO N=1,NSOIL
       IF(N.EQ.1)THEN
        ZSOIL(N)=-1.0*SLDPTH(N)
       ELSE
        ZSOIL(N)=ZSOIL(N-1)-SLDPTH(N)
       END IF
      END DO
! ----------------------------------------------------------------------
! SET SMCWLT, SMCREF, RSMIN, NROOTS VALUES
! ----------------------------------------------------------------------
      SMCWLT = SMWLT(ISOIL)
      SMCREF = SMREF(ISOIL)
      RSMIN = RSMN(IVEG)
      NROOTS = IROOT(IVEG)

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO INCOMING SOLAR RADIATION
! ----------------------------------------------------------------------

      FF = 0.55*2.0*SOLAR/(RGL(IVEG)*XLAI)
      RCS = (FF + RSMIN/RSMAX) / (1.0 + FF)

      RCS = MAX(RCS,0.0001)
      RCS = MIN(RCS,1.0)

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO AIR TEMPERATURE AT FIRST MODEL LEVEL ABOVE GROUND
! ----------------------------------------------------------------------

      RCT = 1.0 - 0.0016*((TOPT-SFCTMP)**2.0)

      RCT = MAX(RCT,0.0001)
      RCT = MIN(RCT,1.0)

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO VAPOR PRESSURE DEFICIT AT FIRST MODEL LEVEL.
! ----------------------------------------------------------------------

!      P = SFCPRS
! Insert QSAT computation used in ETA2P
      TBLO=SFCTMP
      Q2SAT=PQ0/SFCPRS*EXP(A2*(TBLO-A3)/(TBLO-A4)) 
      QS = Q2SAT
! RCQ EXPRESSION FROM SSIB 
      RCQ = 1.0/(1.0+HS(IVEG)*(QS-Q2))

!      RCQ = MAX(RCQ,0.01)
      RCQ = MAX(RCQ,0.0001)
      RCQ = MIN(RCQ,1.0)

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO SOIL MOISTURE AVAILABILITY.
! DETERMINE CONTRIBUTION FROM EACH SOIL LAYER, THEN ADD THEM UP.
! ----------------------------------------------------------------------

      GX = (SMC(1)-SMCWLT)/(SMCREF-SMCWLT)
      IF (GX .GT. 1.) GX = 1.
      IF (GX .LT. 0.) GX = 0.

!####   USING SOIL DEPTH AS WEIGHTING FACTOR
      PART(1) = (ZSOIL(1)/ZSOIL(NROOTS)) * GX

!#### USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
!C      PART(1) = RTDIS(1) * GX
      
      IF (NROOTS .GT. 1) THEN
       DO K = 2, NROOTS
        GX = (SMC(K)-SMCWLT)/(SMCREF-SMCWLT)
        IF (GX .GT. 1.) GX = 1.
        IF (GX .LT. 0.) GX = 0.
!####   USING SOIL DEPTH AS WEIGHTING FACTOR        
        PART(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOTS)) * GX

!#### USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
!C         PART(K) = RTDIS(K) * GX 
               
       END DO
      ENDIF
      DO K = 1, NROOTS
        RCSOIL = RCSOIL+PART(K)
      END DO

      RCSOIL = MAX(RCSOIL,0.0001)
      RCSOIL = MIN(RCSOIL,1.0)

! ----------------------------------------------------------------------
!         DETERMINE CANOPY RESISTANCE DUE TO ALL FACTORS.
!         CONVERT CANOPY RESISTANCE (RC) TO PLANT COEFFICIENT (PC).
! ----------------------------------------------------------------------

!C/98/01/05/........RC = RCMIN/(RCS*RCT*RCQ*RCSOIL)
!      RC = RCMIN(IVEG)/(XLAI*RCS*RCT*RCQ*RCSOIL)

      RCMIN = RSMIN/XLAI
      RCMAX = RSMAX/XLAI
      RC = RCMIN/(RCS*RCT*RCQ*RCSOIL)

      RC = MAX(RCMIN,MIN(RC,RCMAX))

      GC = 1./RC
      
      RETURN
      END
