      SUBROUTINE CALPW(PW,IDECID)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALPW       COMPUTES 
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-24       
!     
! ABSTRACT:  
!     THIS ROUTINE COMPUTES PRECIPITABLE WATER IN A COLUMN
!     EXTENDING FROM THE FIRST ATMOSPHERIC ETA LAYER TO THE
!     MODEL TOP.  THE DEFINITION USED IS
!                                 TOP
!            PRECIPITABLE WATER = SUM (Q+CLDW) DP*HTM/G
!                                 BOT
!     WHERE,
!        BOT IS THE FIRST ETA LAYER,
!        TOP IS THE MODEL TOP,
!        Q IS THE SPECIFIC HUMIDITY (KG/KG) IN THE LAYER
!        CLDW IS THE CLOUD WATER (KG/KG) IN THE LAYER
!        DP (Pa) IS THE LAYER THICKNESS.
!        HTM IS THE HEIGHT MASK AT THAT LAYER (=0 IF BELOW GROUND)
!        G IS THE GRAVITATIONAL CONSTANT
!     
! PROGRAM HISTORY LOG:
!   92-12-24  RUSS TREADON
!   96-03-04  MIKE BALDWIN - ADD CLOUD WATER AND SPEED UP CODE
!   98-06-15  T BLACK      - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION                 
!   02-06-19  MIKE BALDWIN - WRF VERSION 
!   04-12-30  H CHUANG      - UPDATE TO CALCULATE TOTAL COLUMN FOR OTHER
!                                     HYDROMETEORS                
!   11-12-14  SARAH LU     - UPDATE TO CALCULATE AEROSOL OPTICAL DEPTH
!     
! USAGE:    CALL CALPW(PW)
!   INPUT ARGUMENT LIST:
!     PW       - ARRAY OF PRECIPITABLE WATER.
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
!       COMMON   - LOOPS
!                  MASKS
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
      use vrbls3d,    only: q, qqw, qqi, qqr, qqs, cwm, qqg, t, rswtt,    &
                            train, tcucn, mcvg, pmid, o3, ext, pint, rlwtt
      use masks,      only: htm
      use params_mod, only: tfrz, gi
      use ctlblk_mod, only: lm, jsta, jend, im
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
!     
!     SET DENSITY OF WATER AT 1 ATMOSPHERE PRESSURE, 0C.
!     UNITS ARE KG/M**3.
      real,PARAMETER :: RHOWAT=1.E3
      real,parameter:: con_rd      =2.8705e+2 ! gas constant air    (J/kg/K)
      real,parameter:: con_rv      =4.6150e+2 ! gas constant H2O
      real,parameter:: con_eps     =con_rd/con_rv
      real,parameter:: con_epsm1   =con_rd/con_rv-1
!     
!     DECLARE VARIABLES.
!     
      integer,intent(in)  ::  IDECID
      real,dimension(IM,jsta:jend),intent(inout) :: PW
      INTEGER LLMH,I,J,L
      REAL ALPM,DZ,PM,PWSUM,RHOAIR,DP,ES
      real,external :: FPVSNEW
      REAL QDUM(IM,jsta:jend), PWS(IM,jsta:jend),QS(IM,jsta:jend)
!
!***************************************************************
!     START CALPW HERE.
!
!     INITIALIZE PW TO 0.    
!     
!$omp  parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          PW(i,j)  = 0.
          PWS(i,j) = 0.
        ENDDO
      ENDDO
!     
!     OUTER LOOP OVER VERTICAL DIMENSION.
!     INNER LOOP OVER HORIZONTAL GRID.
!     
!!$omp  parallel do private(i,j,l,es,dp)
      DO L = 1,LM
        IF (IDECID <= 1) THEN
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = Q(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID == 2) THEN
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = QQW(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID == 3) THEN
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = QQI(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID == 4) THEN
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = QQR(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID == 5) THEN
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = QQS(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID == 6) THEN
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = CWM(I,J,L)
            ENDDO
          ENDDO
! SRD
        ELSE IF (IDECID == 16) THEN
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = QQG(I,J,L)
            ENDDO
          ENDDO
! SRD
        ELSE IF (IDECID == 7) THEN
!-- Total supercooled liquid
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              IF (T(I,J,L) .GE. TFRZ) THEN
                Qdum(I,J) = 0.
              ELSE
                Qdum(I,J) = QQW(I,J,L) + QQR(I,J,L)
              ENDIF
            ENDDO
          ENDDO
        ELSE IF (IDECID == 8) THEN
!-- Total melting ice
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              IF (T(I,J,L) <= TFRZ) THEN
                Qdum(I,J) = 0.
              ELSE
                Qdum(I,J) = QQI(I,J,L) + QQS(I,J,L)
              ENDIF
            ENDDO
          ENDDO
        ELSE IF (IDECID == 9) THEN
! SHORT WAVE T TENDENCY
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = RSWTT(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID == 10) THEN
! LONG WAVE T TENDENCY
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = RLWTT(I,J,L)
            ENDDO
          ENDDO  
        ELSE IF (IDECID == 11) THEN
! LATENT HEATING FROM GRID SCALE RAIN/EVAP
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = TRAIN(I,J,L)
            ENDDO
          ENDDO  
        ELSE IF (IDECID == 12) THEN
! LATENT HEATING FROM CONVECTION
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = TCUCN(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID == 13) THEN
! MOISTURE CONVERGENCE
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = MCVG(I,J,L)
            ENDDO
          ENDDO
! RH
        ELSE IF (IDECID == 14) THEN
!$omp  parallel do private(i,j,es)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = Q(I,J,L)
              ES        = min(FPVSNEW(T(I,J,L)),PMID(I,J,L))
              QS(I,J)   = CON_EPS*ES/(PMID(I,J,L)+CON_EPSM1*ES)
            ENDDO
          END DO
! OZONE
        ELSE IF (IDECID == 15) THEN
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = O3(I,J,L)
            ENDDO
          END DO

! AEROSOL EXTINCTION (GOCART)
        ELSE IF (IDECID == 17) THEN
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J) = EXT(I,J,L)
            ENDDO
          END DO

        ENDIF
!
!$omp  parallel do private(i,j,dp)
        DO J=JSTA,JEND
          DO I=1,IM
            DP      = PINT(I,J,L+1) - PINT(I,J,L)
            PW(I,J) = PW(I,J) + Qdum(I,J)*DP*GI*HTM(I,J,L)
            IF (IDECID == 17) THEN
             PW(I,J) = PW(I,J) + Qdum(I,J)*MAX(DP,0.)*GI*HTM(I,J,L)
            ENDIF
            IF (IDECID == 14) PWS(I,J) = PWS(I,J) + QS(I,J)*DP*GI*HTM(I,J,L)
          ENDDO
        ENDDO
      ENDDO                 ! l loop

      
      IF (IDECID == 14)THEN
!$omp  parallel do private(i,j,dp)
        DO J=JSTA,JEND
          DO I=1,IM
            PW(I,J) = max(0.,PW(I,J)/PWS(I,J)*100.) 
          ENDDO
        ENDDO
      END IF
!  convert ozone from kg/m2 to dobson units, which give the depth of the
!  ozone layer in 1e-5 m if brought to natural temperature and pressure.    

      IF (IDECID == 15) then
!$omp  parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            PW(I,J) = PW(I,J) / 2.14e-5
          ENDDO
        ENDDO
      endif
!
!     END OF ROUTINE.
!     
      RETURN
      END
