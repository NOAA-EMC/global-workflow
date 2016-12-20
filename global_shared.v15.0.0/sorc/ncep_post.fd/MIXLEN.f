      SUBROUTINE MIXLEN(EL0,EL)
!
!     CALCULATES LAYER-AVERAGED BLACKADAR'S MIXING LENGTH, AND PBL TOP
!     AS CPBLT*(ASYMPTOTIC EL); AND THEN EL, ACCOUNT TAKEN OF STABILITY,
!     PBL TOP AND VERTICAL GRID DISTANCE RESTRICTIONS (SEE BELOW)
!
!     SET FROM EXISTING CODES BY L. LOBOCKI, JUNE 5, 1992
!       MODIFIED BY FEDOR MESINGER, OCTOBER 13, NOVEMBER 19
!       MODIFIED BY JIM TUCCILLO FOR MPI IMPLEMENTATION
!   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-06-19  MIKE BALDWIN - WRF VERSION
!
!     INPUT:
!     ------
!
!     ZINT (IM,jsta_2l:jend_2u,LP1) - ETA INTERFACES HEIGHT FIELD
!     T    (IM,jsta_2l:jend_2u,LM)  - TEMPERATURE
!     PMID (IM,jsta_2l:jend_2u,LM)  - PRESSURE IN LAYERS
!     Q2   (IM,jsta_2l:jend_2u,LM)  - TURBULENCE KINETIC ENERGY * 2
!     HGT  (IM,jsta_2l:jend_2u)     - SURFACE ELEVATION ARRAY
!     HTM  (IM,jsta_2l:jend_2u,LM)  - HEIGHT TOPOGRAPHY MASK ARRAY
!     EL0  (IM,JM)     - ARRAY OF ASYMPTOTIC VALUES FOR MIXING LENGTH
!
!     OUTPUT:
!     -------
!
!     EL   (IM,jsta_2l:jend_2u,LM) - FIELD OF RESULTING MASTER LENGTH SCALES
!
!
!     SCRATCH AREAS:
!     --------------
!
!     VKRMZ(IM,JM)
!
!     RELEVANT CONSTANTS:
!     -------------------
!
!     VON KARMAN CONSTANT:
      use vrbls3d, only: zint, pmid, t, q2
      use masks, only: lmh, htm
      use params_mod, only: EPSQ2, CAPA
      use ctlblk_mod, only: jsta, jend, jsta_m, jend_m, im, jm, jsta_2l, jend_2u,&
              lm, lm1

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!
      real,PARAMETER :: VKRM=0.4
!     CONSTANTS NEEDED FOR THE EL(BL,ST,ZI) SCHEME:
      real,PARAMETER :: FRG=4.*9.8,DRDRFF=0.54,CPBLT=10.,     &
        CSH=0.23*0.5, EPSN2=1.E-7
!
!     ------------------------------------------------------------------
!
      real,intent(in) :: EL0(IM,JM)
      real,intent(out) ::  EL(IM,jsta_2l:jend_2u,LM)
      real HGT(IM,JM),APE(IM,JM,2)
!
      integer I,J,L
      real ZL,VKRMZ,ENSQ,Q2KL,ELST,ZIAG,ELVGD

!***********************************************************************
!
!$omp  parallel do
      DO L=1,LM
        DO J=JSTA,JEND
        DO I=1,IM
          EL(I,J,L)=0.
        ENDDO
        ENDDO
      ENDDO
        DO J=JSTA,JEND
        DO I=1,IM
          HGT(I,J)=ZINT(I,J,NINT(LMH(I,J))+1)
        ENDDO
        ENDDO
!
!---THE AVERAGE EL SCHEME---------------------------(FM, AUGUST 19 MEMO)
!   FIRST GET EL IN THE LAYERS
!
!$omp  parallel do private(i,j,l,vkrmz,zl)
      DO L=1,LM
        DO J=JSTA,JEND
          DO I=1,IM
            ZL        = 0.5*(ZINT(I,J,L)+ZINT(I,J,L+1))
            VKRMZ     = (ZL-HGT(I,J))*VKRM
            EL(I,J,L) = EL0(I,J)*VKRMZ/(EL0(I,J)+VKRMZ)
          ENDDO
        ENDDO
      ENDDO
!***
!***  GET NOW THE INTERFACE EL BY TWO-POINT AVERAGING OF LAYER VALUES
!***
      DO L=1,LM1
!$omp  parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            EL(I,J,L) = 0.5*(EL(I,J,L)+EL(I,J,L+1))*HTM(I,J,L+1)
          ENDDO
        ENDDO
      ENDDO
!
!$omp  parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          EL(I,J,LM) = 0.0
        ENDDO
      ENDDO
!---STABILITY, PBL TOP, AND VERTICAL GRID DISTANCE RESTRICTIONS:--------
!   COMPUTE EL STABLE AND
!   * USE THE SMALLER OF EL BLACKADAR, EL STABLE IF WITHIN PBL;
!   * USE THE SMALLEST OF EL STABLE, ELVGD, AND VKRMZ IF ABOVE PBL
!       (ASSUME PBL TOP IS AT CPBLT*EL0(K));
!$omp  parallel do private(i,j)
      DO J=JSTA_M,JEND_M
        DO I=1,IM
          APE(I,J,1) = (1.E5/PMID(I,J,1))**CAPA
        ENDDO
      ENDDO
!
      DO L=1,LM1
!$omp  parallel do private(i,j,elst,elvgd,ensq,q2kl,ziag)
        DO J=JSTA_M,JEND_M
          DO I=2,IM-1
            APE(I,J,2) = (1.E5/PMID(I,J,L+1))**CAPA
            ENSQ = HTM(I,J,L+1)*                                     &
                   FRG*(T(I,J,L)*APE(I,J,1)-T(I,J,L+1)*APE(I,J,2))/  &
                  ((T(I,J,L)*APE(I,J,1)+T(I,J,L+1)*APE(I,J,2))*     &
                   (ZINT(I,J,L)-ZINT(I,J,L+2))+EPSN2)
            ENSQ = AMAX1(ENSQ,EPSN2)
            Q2KL = AMAX1(EPSQ2,Q2(I,J,L))
            ELST = DRDRFF*SQRT(Q2KL/ENSQ)
!WAS        ELST = DRDRFF*SQRT(Q2(I,J,L)/ENSQ)
            ZIAG = ZINT(I,J,L+1)-HGT(I,J)
!
            IF(ZIAG < CPBLT*EL0(I,J))THEN
              EL(I,J,L) = AMIN1(EL(I,J,L),ELST)
            ELSE
              ELVGD     = CSH*(ZINT(I,J,L)-ZINT(I,J,L+2))
              EL(I,J,L) = AMIN1(ELST,ELVGD,VKRM*ZIAG)
            ENDIF
            APE(I,J,1) = APE(I,J,2)
          ENDDO
        ENDDO
      ENDDO
!
      RETURN
      END

