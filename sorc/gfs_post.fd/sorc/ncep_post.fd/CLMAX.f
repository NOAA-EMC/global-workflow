      SUBROUTINE CLMAX(EL0,SQZ,SQ,RQ2L,RQ2H)
!
!     CALCULATES THE FREE-ATMOSPHERE ASYMPTOTE OF THE TURBULENCE LENGTH
!     SCALE (L-INF IN THE BLACKADAR's FORMULA) FROM THE DISTRIBUTION
!     OF THE TURBULENT ENERGY (see MY82)
!
!     EXTRACTED FROM EXISTING CODE BY L. LOBOCKI, JULY 28, 1992
!   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-06-19  MIKE BALDWIN - WRF VERSION
!
!     INPUT:
!     ------
!
!     PINT (IM,jsta_2l:jend_2u,LP1)  - PRESSURE ON INTERFACES 
!     HTM  (IM,jsta_2l:jend_2u,LM)  - HEIGHT TOPOGRAPHY MASK ARRAY
!     Q2   (IM,jsta_2l:jend_2u,LM)  - TWICE THE TURBULENT ENERGY FIELD
!     ZINT (IM,jsta_2l:jend_2u,LP1) - ETA INTERFACES HEIGHT FIELD
!     SM   (IM,jsta_2l:jend_2u)     - SEA MASK
!     HGT  (IM,jsta_2l:jend_2u)     - SURFACE ELEVATION ARRAY
!     LMH  (IM,jsta_2l:jend_2u)     - TOPOGRAPHY INDEXES ARRAY
!
!     OUTPUT:
!     -------
!
!     EL0 (IM,JM)      - ARRAY OF RESULTING ASYMPTOTIC MIXING LENGTHS
!
!
!     SCRATCH AREAS:
!     --------------
!
!     SQZ(IM,JM),SQ(IM,JM),RQ2L(IM,JM),RQ2H(IM,JM)
!
!
!     RELEVANT CONSTANTS:
!     -------------------
!
!     PROPORTIONALITY CONSTANT BETWEEN ASYMPTOTIC MIXING LENGTH AND THE
!     S.D. OF Q DISTRIBUTION, FOR LAND AND SEA AREAS, CORRESPONDINGLY:

      use vrbls3d, only: zint, q2, pint
!      use vrbls2d, only:
      use masks, only: lmh, sm
      use params_mod, only: EPSQ2
      use ctlblk_mod, only: jsta, jend, lm, im
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      real,PARAMETER :: ALPHAL=0.2, ALPHAS=0.2
!
!     ASYMPTOTIC MIXING LENGTH LIMITATIONS:
      real,PARAMETER :: EL0M=300.0, ELMIN=11.0
!
!     MINIMAL VALUE OF TURBULENT ENERGY:
!      real,PARAMETER :: EPSQ2=0.2
!
!     ------------------------------------------------------------------
!
      real,dimension(IM,jsta:jend),intent(inout) ::  SQZ,SQ,RQ2L,RQ2H,EL0
      real,dimension(IM,jsta:jend)               ::   HGT
!jw
      integer I,J,L
      real dp, RQ2m
!     ------------------------------------------------------------------
!
!
!$omp  parallel do
      DO J=JSTA,JEND
        DO I=1,IM
          SQZ(I,J)  = 0.0
          SQ(I,J)   = 0.0
          RQ2H(I,J) = 0.0
          HGT(I,J)  = ZINT(I,J,NINT(LMH(I,J)))
        ENDDO
      ENDDO
!
      DO L=1,LM
        DO J=JSTA,JEND
          DO I=1,IM
            IF(Q2(I,J,L) <= EPSQ2) THEN
              RQ2L(I,J) = 0.0
            ELSE
              RQ2L(I,J) = SQRT(Q2(I,J,L))
            ENDIF
!
!         -----------------------------------------------------------------
!         THIS PART OF THE CODE IS LEFT FOR TESTING OTHER PARAMETERIZATION
!         SCHEMES 
!
!         IF (L.GE.LMH(I,J)) GOTO 215
!         RQ2L(I,J)=SQRT(Q2(I,J,L))
!         IF(Q2(I,J,L).LT.0.0)THEN
!           write(3,*)'NEGATIVE Q2 AT (I,J,L)=(',I,',',J,',',L,'): ',
!                     Q2(I,J,L)
!           STOP
!         ENDIF
!         -----------------------------------------------------------------
!
            DP = PINT(I,J,L+1) - PINT(I,J,L)
!***
!***      SUM OF Q2 AT BOTH LOWER & UPPER SURFACES:
!***
            RQ2M = RQ2H(I,J) + RQ2L(I,J)
!***
!***      INTEGRAL OF Q*Z OVER DP
!***
            SQZ(I,J) = ((ZINT(I,J,L)+ZINT(I,J,L+1))*0.5-HGT(I,J))*RQ2M*DP    &
     &               +  SQZ(I,J)
!***
!***      INTEGRAL OF Q OVER DP:
!***
            SQ(I,J)   = RQ2M*DP + SQ(I,J)
            RQ2H(I,J) = RQ2L(I,J)
          ENDDO
        ENDDO
!215    CONTINUE
      ENDDO
!***
!***    CLIPPING & APPLYING DIFFERENT VALUES OF THE PROPORTIONALITY 
!***    CONSTANT ALPHA FOR THE LAND AND SEA AREA:
!***
!$omp  parallel do
      DO J=JSTA,JEND
        DO I=1,IM
          EL0(I,J)= MAX(MIN(                                           &
     &       ((SM(I,J)*ALPHAS+(1.0-SM(I,J))*ALPHAL)*SQZ(I,J)           &
     &       /(SQ(I,J)+EPSQ2)),EL0M),ELMIN)
        ENDDO
      ENDDO
!
      RETURN
      END

