      SUBROUTINE NGMSLP
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    NGMSLP      NMC SEA LEVEL PRESSURE REDUCTION
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-02-02       
!     
! ABSTRACT:
!     
!     THIS ROUTINE COMPUTES SEA LEVEL PRESSURE USING THE
!     HYDROSTATIC EQUATION WITH THE SHUELL CORRECTION.  THE
!     FOLLOWING IS BASED ON DOCUMENTATION IN SUBROUTINE 
!     OUTHYDRO OF THE NGM:
!     
!     THE FUNDAMENTAL HYDROSTATIC EQUATION IS
!        D(HEIGHT)
!        ---------  =  TAU = VIRTUAL TEMPERATURE * (RGAS/GRAVITY)
!        D (Z)
!      WHERE
!        Z = MINUS LOG OF PRESSURE (-LN(P)).
!
!     SEA-LEVEL PRESSURE IS COMPUTED FROM THE FORMULA
!        PRESS(MSL) = PRESS(GROUND) * EXP( F)
!     WHERE
!        F        = HEIGHT OF GROUND / MEAN TAU
!        MEAN TAU = ( TAU(GRND) + TAU(SL) ) / 2
!     
!     IN THE NGM TAU(GRND) AND TAU(SL) ARE FIRST SET USING A 
!     6.5DEG/KM LAPSE RATE FROM LOWEST MDL LEVEL.  THIS IS MODIFIED
!     BY A CORRECTION BASED ON THE CRITICAL TAU OF THE SHUELL
!     CORRECTION:
!                  TAUCR=(RGASD/GRAVITY) * 290.66
!   
!     1) WHERE ONLY TAU(SL) EXCEEDS TAUCR, CHANGE TAU(SL) TO TAUCR.
!
!     2) WHERE BOTH TAU(SL) AND TAU(GRND) EXCEED TAUCR,
!        CHANGE TAU(SL) TO TAUCR-CONST*(TAU(GRND)-TAUCR  )**2
!        WHERE CONST = .005 (GRAVITY/RGASD)
!   
!     THE AVERAGE OF TAU(SL) AND TAU(GRND) IS THEN USED TOGETHER
!     WITH THE GROUND HEIGHT AND PRESSURE TO DERIVE THE PRESSURE
!     AT SEA LEVEL. 
!     
!     HEIGHT OF THE 1000MB SURFACE IS COMPUTED FROM THE MSL PRESSURE
!     FIELD USING THE FORMULA:
!     
!       P(MSL) - P(1000MB) = MEAN DENSITY * GRAVITY * HGT(1000MBS)
!     
!     WHERE P(MSL) IS THE SEA LEVEL PRESSURE FIELD WE HAVE JUST
!     COMPUTED.
!     
!
!     MEB 6/13/02: THIS CODE HAS BEEN SIMPLIFIED CONSIDERABLY FROM
!     THE ONE USED IN ETAPOST.  HORIZONTAL SMOOTHING HAS BEEN
!     REMOVED AND THE FIRST MODEL LEVEL IS USED RATHER
!     THAN THE MEAN OF THE VIRTUAL TEMPERATURES IN
!     THE LOWEST 30MB ABOVE GROUND TO COMPUTE TAU(GRND).
!     
!   .     
!     
! PROGRAM HISTORY LOG:
!   93-02-02  RUSS TREADON
!   98-06-08  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION
!   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   01-11-02  H CHUANG - MODIFIED LINE 234 FOR COMPUTATION OF 
!                         SIGMA/HYBRID SLP
!   01-12-18  H CHUANG - INCLUDED SMOOTHING ALONG BOUNDARIES TO BE
!                         CONSISTENT WITH MESINGER SLP
!   02-06-13  MIKE BALDWIN - WRF VERSION
!   06-12-18  H CHUANG - BUG FIX TO CORRECT TAU AT SFC
!     
! USAGE:    CALL NGMSLP
!   INPUT ARGUMENT LIST:
!     NONE     
!
!   OUTPUT ARGUMENT LIST: 
!     NONE
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - CTLBLK
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
      use vrbls3d,    only: zint, pint, t, q, zmid
      use vrbls2d,    only: slp, fis, z1000
      use masks,      only: lmh
      use params_mod, only: rd, gi, g, h1, d608, gamma, d50, p1000
      use ctlblk_mod, only: jsta, jend, im, jm
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!     
      real,PARAMETER :: ZSL=0.0
      real,PARAMETER :: TAUCR=RD*GI*290.66,CONST=0.005*G/RD
      real,PARAMETER :: GORD=G/RD,DP=60.E2
!     
!     DECLARE VARIABLES
!
      integer I,J,LLMH
      real ZSFC,PSFC,TVRT,TAU,TVRSFC,TAUSFC,TVRSL,TAUSL,TAUAVG,    &
           ALPAVG,PAVG,RHOAVG,RRHOG
!     
!**********************************************************************
!     START NGMSLP HERE.
!     
!     LOOP OVER HORIZONTAL GRID.
!
!!$omp  parallel do
!!$omp& private(llmh,pavg,psfc,qavg,rhoavg,rrhog,
!!$omp&         tau,tauavg,tausfc,tausl,tavg,tvrbar,tvrsfc,tvrsl,
!!$omp&         tvrt,tvrtal,zbar,zl,zsfc)
       DO J=JSTA,JEND
       DO I=1,IM
         LLMH = NINT(LMH(I,J))
         ZSFC = ZINT(I,J,LLMH+1)
         PSFC = PINT(I,J,LLMH+1)
         SLP(I,J) = PSFC
!
!        COMPUTE LAYER TAU (VIRTUAL TEMP*RD/G).
         TVRT = T(I,J,LLMH)*(H1+D608*Q(I,J,LLMH))
         TAU  = TVRT*RD*GI
!     
!        COMPUTE TAU AT THE GROUND (Z=ZSFC) AND SEA LEVEL (Z=0)
!        ASSUMING A CONSTANT LAPSE RATE OF GAMMA=6.5DEG/KM.
!         TVRSFC = TVRT + (ZSFC- ZSL)*GAMMA
         TVRSFC = TVRT + (ZMID(I,J,LLMH) - ZSFC)*GAMMA ! Chuang
         TAUSFC = TVRSFC*RD*GI
!         TVRSL  = TVRT + (ZSFC- ZSL)*GAMMA
         TVRSL  = TVRT + (ZMID(I,J,LLMH) - ZSL)*GAMMA
         TAUSL  = TVRSL*RD*GI
!     
!        IF NEED BE APPLY SHEULL CORRECTION.
         IF ((TAUSL.GT.TAUCR).AND.(TAUSFC.LE.TAUCR)) THEN
            TAUSL=TAUCR
         ELSEIF ((TAUSL.GT.TAUCR).AND.(TAUSFC.GT.TAUCR)) THEN
            TAUSL = TAUCR-CONST*(TAUSFC-TAUCR)**2
         ENDIF
!     
!        COMPUTE MEAN TAU.
         TAUAVG = D50*(TAUSL+TAUSFC)
!     
!        COMPUTE SEA LEVEL PRESSURE.
         IF (ABS(FIS(I,J)).GT.1.0)SLP(I,J) = PSFC*EXP(ZSFC/TAUAVG)
!     
!        COMPUTE 1000MB HEIGHTS.
         ALPAVG   = D50*(ALOG(PSFC)+ALOG(SLP(I,J)))
         PAVG     = EXP(ALPAVG)
         RHOAVG   = PAVG*GI/TAUAVG
         RRHOG    = H1/(RHOAVG*G)
         Z1000(I,J) = (SLP(I,J)-P1000)*RRHOG
!     
!     MOVE TO NEXT HORIZONTAL GRIDPOINT.
      ENDDO
      ENDDO
!     
!     
!     END OF ROUTINE.
!     

      RETURN
      END
