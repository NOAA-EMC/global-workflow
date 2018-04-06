      SUBROUTINE CALCAPE(ITYPE,DPBND,P1D,T1D,Q1D,L1D,CAPE,    &  
                         CINS,PPARC,ZEQL,THUND)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALCAPE     COMPUTES CAPE AND CINS
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-02-10      
!     
! ABSTRACT:  
!     
!     THIS ROUTINE COMPUTES CAPE AND CINS GIVEN TEMPERATURE,
!     PRESSURE, AND SPECIFIC HUMIDTY.  IN "STORM AND CLOUD 
!     DYNAMICS" (1989, ACADEMIC PRESS) COTTON AND ANTHES DEFINE
!     CAPE (EQUATION 9.16, P501) AS
!
!                  EL
!         CAPE =  SUM G * LN(THETAP/THETAA) DZ 
!                 LCL
!     
!     WHERE,
!      EL    = EQUILIBRIUM LEVEL,
!     LCL    = LIFTING CONDENSTATION LEVEL,
!       G    = GRAVITATIONAL ACCELERATION,
!     THETAP = LIFTED PARCEL POTENTIAL TEMPERATURE,
!     THETAA = AMBIENT POTENTIAL TEMPERATURE.
!     
!     NOTE THAT THE INTEGRAND LN(THETAP/THETAA) APPROXIMATELY
!     EQUALS (THETAP-THETAA)/THETAA.  THIS RATIO IS OFTEN USED
!     IN THE DEFINITION OF CAPE/CINS.
!     
!     TWO TYPES OF CAPE/CINS CAN BE COMPUTED BY THIS ROUTINE.  THE
!     SUMMATION PROCESS IS THE SAME FOR BOTH CASES.  WHAT DIFFERS
!     IS THE DEFINITION OF THE PARCEL TO LIFT.  FOR ITYPE=1 THE
!     PARCEL WITH THE WARMEST THETA-E IN A DPBND PASCAL LAYER ABOVE
!     THE MODEL SURFACE IS LIFTED.  THE ARRAYS P1D, T1D, AND Q1D
!     ARE NOT USED.  FOR ITYPE=2 THE ARRAYS P1D, T1D, AND Q1D
!     DEFINE THE PARCEL TO LIFT IN EACH COLUMN.  BOTH TYPES OF
!     CAPE/CINS MAY BE COMPUTED IN A SINGLE EXECUTION OF THE POST
!     PROCESSOR.
!     
!     THIS ALGORITHM PROCEEDS AS FOLLOWS.
!     FOR EACH COLUMN, 
!        (1)  INITIALIZE RUNNING CAPE AND CINS SUM TO 0.0
!        (2)  COMPUTE TEMPERATURE AND PRESSURE AT THE LCL USING
!             LOOK UP TABLE (PTBL).  USE EITHER PARCEL THAT GIVES
!             MAX THETAE IN LOWEST DPBND ABOVE GROUND (ITYPE=1)
!             OR GIVEN PARCEL FROM T1D,Q1D,...(ITYPE=2).
!        (3)  COMPUTE THE TEMP OF A PARCEL LIFTED FROM THE LCL.
!             WE KNOW THAT THE PARCEL'S
!             EQUIVALENT POTENTIAL TEMPERATURE (THESP) REMAINS
!             CONSTANT THROUGH THIS PROCESS.  WE CAN
!             COMPUTE TPAR USING THIS KNOWLEDGE USING LOOK
!             UP TABLE (SUBROUTINE TTBLEX).
!        (4)  FIND THE EQUILIBRIUM LEVEL.  THIS IS DEFINED AS THE
!             HIGHEST POSITIVELY BUOYANT LAYER.
!             (IF THERE IS NO POSITIVELY BUOYANT LAYER, CAPE/CINS
!              WILL BE ZERO)
!        (5)  COMPUTE CAPE/CINS.  
!             (A) COMPUTE THETAP.  WE KNOW TPAR AND P.
!             (B) COMPUTE THETAA.  WE KNOW T AND P.  
!        (6)  ADD G*(THETAP-THETAA)*DZ TO THE RUNNING CAPE OR CINS SUM.
!             (A) IF THETAP > THETAA, ADD TO THE CAPE SUM.
!             (B) IF THETAP < THETAA, ADD TO THE CINS SUM.
!        (7)  ARE WE AT EQUILIBRIUM LEVEL? 
!             (A) IF YES, STOP THE SUMMATION.
!             (B) IF NO, CONTIUNUE THE SUMMATION.
!        (8)  ENFORCE LIMITS ON CAPE AND CINS (I.E. NO NEGATIVE CAPE)
!     
! PROGRAM HISTORY LOG:
!   93-02-10  RUSS TREADON
!   93-06-19  RUSS TREADON - GENERALIZED ROUTINE TO ALLOW FOR 
!                            TYPE 2 CAPE/CINS CALCULATIONS.     
!   94-09-23  MIKE BALDWIN - MODIFIED TO USE LOOK UP TABLES
!                            INSTEAD OF COMPLICATED EQUATIONS.
!   94-10-13  MIKE BALDWIN - MODIFIED TO CONTINUE CAPE/CINS CALC
!                            UP TO AT HIGHEST BUOYANT LAYER.
!   98-06-12  T BLACK      - CONVERSION FROM 1-D TO 2-D
!   98-08-18  T BLACK      - COMPUTE APE INTERNALLY
!   00-01-04  JIM TUCCILLO - MPI VERSION              
!   02-01-15  MIKE BALDWIN - WRF VERSION
!   03-08-24  G MANIKIN    - ADDED LEVEL OF PARCEL BEING LIFTED
!                            AS OUTPUT FROM THE ROUTINE AND ADDED
!                            THE DEPTH OVER WHICH ONE SEARCHES FOR
!                            THE MOST UNSTABLE PARCEL AS INPUT
!   10-09-09  G MANIKIN    - CHANGED COMPUTATION TO USE VIRTUAL TEMP
!                          - ADDED EQ LVL HGHT AND THUNDER PARAMETER    
!   15-xx-xx  S MOORTHI    - optimization and threading
!
! USAGE:    CALL CALCAPE(ITYPE,DPBND,P1D,T1D,Q1D,L1D,CAPE,
!                                CINS,PPARC)
!   INPUT ARGUMENT LIST:
!     ITYPE    - INTEGER FLAG SPECIFYING HOW PARCEL TO LIFT IS
!                IDENTIFIED.  SEE COMMENTS ABOVE.
!     DPBND    - DEPTH OVER WHICH ONE SEARCHES FOR MOST UNSTABLE PARCEL
!     P1D      - ARRAY OF PRESSURE OF PARCELS TO LIFT.
!     T1D      - ARRAY OF TEMPERATURE OF PARCELS TO LIFT.
!     Q1D      - ARRAY OF SPECIFIC HUMIDITY OF PARCELS TO LIFT.
!     L1D      - ARRAY OF MODEL LEVEL OF PARCELS TO LIFT.
!
!   OUTPUT ARGUMENT LIST: 
!     CAPE     - CONVECTIVE AVAILABLE POTENTIAL ENERGY (J/KG)
!     CINS     - CONVECTIVE INHIBITION (J/KG)
!     PPARC    - PRESSURE LEVEL OF PARCEL LIFTED WHEN ONE SEARCHES
!                  OVER A PARTICULAR DEPTH TO COMPUTE CAPE/CIN
!     
!   OUTPUT FILES:
!     STDOUT  - RUN TIME STANDARD OUT.
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       BOUND    - BOUND (CLIP) DATA BETWEEN UPPER AND LOWER LIMTS.
!       TTBLEX   - LOOKUP TABLE ROUTINE TO GET T FROM THETAE AND P
!
!     LIBRARY:
!       COMMON   - 
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90
!     MACHINE : CRAY C-90
!$$$  
!
      use vrbls3d,    only: pmid, t, q, zint
      use masks,      only: lmh
      use params_mod, only: d00, h1m12, h99999, h10e5, capa, elocp, eps,  &
                            oneps, g
      use lookup_mod, only: thl, rdth, jtb, qs0, sqs, rdq, itb, ptbl,     &
                            plq, ttbl, pl, rdp, the0, sthe, rdthe, ttblq, &
                            itbq, jtbq, rdpq, the0q, stheq, rdtheq
      use ctlblk_mod, only: jsta_2l, jend_2u, lm, jsta, jend, im, me

!     
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
!     INCLUDE/SET PARAMETERS.  CONSTANTS ARE FROM BOLTON (MWR, 1980).
      real,PARAMETER :: ISMTHP=2,ISMTHT=2,ISMTHQ=2
!     
!     DECLARE VARIABLES.
!
      integer,intent(in) :: ITYPE
      real,intent(in)    :: DPBND
      integer, dimension(IM,Jsta:jend),intent(in)    :: L1D
      real,    dimension(IM,Jsta:jend),intent(in)    :: P1D,T1D
      real,    dimension(IM,jsta:jend),intent(inout) :: Q1D,CAPE,CINS,PPARC,ZEQL
!     
      integer, dimension(im,jsta:jend) :: IEQL, IPTB, ITHTB, PARCEL, KLRES, KHRES, LCL, IDX
!     
      real,    dimension(im,jsta:jend) :: THESP, PSP, CAPE20, QQ, PP, THUND  
      REAL, ALLOCATABLE :: TPAR(:,:,:)

      LOGICAL THUNDER(IM,jsta:jend), NEEDTHUN 
      real PSFCK,PKL,TBTK,QBTK,APEBTK,TTHBTK,TTHK,APESPK,TPSPK,        &
           BQS00K,SQS00K,BQS10K,SQS10K,BQK,SQK,TQK,PRESK,GDZKL,THETAP, &
           THETAA,P00K,P10K,P01K,P11K,TTHESK,ESATP,QSATP,TVP,TV
      real,external :: fpvsnew
      integer I,J,L,KNUML,KNUMH,LBEG,LEND,IQ, KB,ITTBK

!     integer I,J,L,KNUML,KNUMH,LBEG,LEND,IQ,IT,LMHK, KB,ITTBK
!     
!**************************************************************
!     START CALCAPE HERE.
!     
      ALLOCATE(TPAR(IM,JSTA_2L:JEND_2U,LM))
!
!     COMPUTE CAPE/CINS
!
!        WHICH IS: THE SUM FROM THE LCL TO THE EQ LEVEL OF
!             G * (LN(THETAP) - LN(THETAA)) * DZ
!
!             (POSITIVE AREA FOR CAPE, NEGATIVE FOR CINS)
!
!        WHERE:
!             THETAP IS THE PARCEL THETA
!             THETAA IS THE AMBIENT THETA
!             DZ IS THE THICKNESS OF THE LAYER
!
!         USING LCL AS LEVEL DIRECTLY BELOW SATURATION POINT
!         AND EQ LEVEL IS THE HIGHEST POSITIVELY BUOYANT LEVEL.
!  
!         IEQL = EQ LEVEL
!         P_thetaemax - real  pressure of theta-e max parcel (Pa)
!
!     INITIALIZE CAPE AND CINS ARRAYS
! 
!$omp  parallel do
      DO J=JSTA,JEND
        DO I=1,IM
          CAPE(I,J)    = D00
          CAPE20(I,J)  = D00
          CINS(I,J)    = D00
          LCL(I,J)     = 0
          THESP(I,J)   = D00
          IEQL(I,J)    = LM
          PARCEL(I,J)  = LM
          PSP(I,J)     = D00
          PPARC(I,J)   = D00
          THUNDER(I,J) = .TRUE.
        ENDDO
      ENDDO
!
!$omp  parallel do
      DO L=1,LM
        DO J=JSTA,JEND
          DO I=1,IM
            TPAR(I,J,L) = D00
          ENDDO
        ENDDO
      ENDDO
!     
!     TYPE 2 CAPE/CINS:
!     NOTE THAT FOR TYPE 1 CAPE/CINS ARRAYS P1D, T1D, Q1D 
!     ARE DUMMY ARRAYS.
!     
      IF (ITYPE == 2) THEN
!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            Q1D(I,J) = MIN(MAX(H1M12,Q1D(I,J)),H99999)
          ENDDO
        ENDDO
      ENDIF
!-------FOR ITYPE=1--FIND MAXIMUM THETA E LAYER IN LOWEST DPBND ABOVE GROUND-------
!-------FOR ITYPE=2--FIND THETA E LAYER OF GIVEN T1D, Q1D, P1D---------------------
!--------------TRIAL MAXIMUM BUOYANCY LEVEL VARIABLES-------------------

      DO KB=1,LM
!hc     IF (ITYPE.EQ.2.AND.KB.GT.1) cycle
        IF (ITYPE == 1 .OR. (ITYPE == 2 .AND. KB == 1)) THEN

!$omp  parallel do private(i,j,apebtk,apespk,bqk,bqs00k,bqs10k,iq,ittbk,    &
!$omp &         p00k,p01k,p10k,p11k,pkl,psfck,qbtk,sqk,sqs00k,              &
!$omp &         sqs10k,tbtk,tpspk,tqk,tthbtk,tthesk,tthk)
          DO J=JSTA,JEND
            DO I=1,IM
              PSFCK  = PMID(I,J,NINT(LMH(I,J)))
              PKL    = PMID(I,J,KB)

!hc           IF (ITYPE.EQ.1.AND.(PKL.LT.PSFCK-DPBND.OR.PKL.GT.PSFCK)) cycle
              IF (ITYPE ==2 .OR.                                                &
                 (ITYPE == 1 .AND. (PKL >= PSFCK-DPBND .AND. PKL <= PSFCK)))THEN
                IF (ITYPE == 1) THEN
                  TBTK   = T(I,J,KB)
                  QBTK   = max(0.0, Q(I,J,KB))
                  APEBTK = (H10E5/PKL)**CAPA
                ELSE
                  PKL    = P1D(I,J)
                  TBTK   = T1D(I,J)
                  QBTK   = max(0.0, Q1D(I,J))
                  APEBTK = (H10E5/PKL)**CAPA
                ENDIF

!----------Breogan Gomez - 2009-02-06
! To prevent QBTK to be less than 0 which leads to a unrealistic value of PRESK
!  and a floating invalid.

!               if(QBTK < 0) QBTK = 0

!--------------SCALING POTENTIAL TEMPERATURE & TABLE INDEX--------------
                TTHBTK  =  TBTK*APEBTK
                TTHK    = (TTHBTK-THL)*RDTH
                QQ(I,J) = TTHK - AINT(TTHK)
                ITTBK   = INT(TTHK) + 1
!--------------KEEPING INDICES WITHIN THE TABLE-------------------------
                IF(ITTBK < 1)   THEN
                  ITTBK   = 1
                  QQ(I,J) = D00
                ENDIF
                IF(ITTBK >= JTB) THEN
                  ITTBK   = JTB-1
                  QQ(I,J) = D00
                ENDIF
!--------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY---------------
                BQS00K = QS0(ITTBK)
                SQS00K = SQS(ITTBK)
                BQS10K = QS0(ITTBK+1)
                SQS10K = SQS(ITTBK+1)
!--------------SCALING SPEC. HUMIDITY & TABLE INDEX---------------------
                BQK     = (BQS10K-BQS00K)*QQ(I,J) + BQS00K
                SQK     = (SQS10K-SQS00K)*QQ(I,J) + SQS00K
                TQK     = (QBTK-BQK)/SQK*RDQ
                PP(I,J) = TQK-AINT(TQK)
                IQ      = INT(TQK)+1
!--------------KEEPING INDICES WITHIN THE TABLE-------------------------
                IF(IQ < 1)    THEN
                  IQ      = 1
                  PP(I,J) = D00
                ENDIF
                IF(IQ >= ITB)  THEN
                  IQ      = ITB-1
                  PP(I,J) = D00
                ENDIF
!--------------SATURATION PRESSURE AT FOUR SURROUNDING TABLE PTS.-------
                P00K = PTBL(IQ  ,ITTBK  )
                P10K = PTBL(IQ+1,ITTBK  )
                P01K = PTBL(IQ  ,ITTBK+1)
                P11K = PTBL(IQ+1,ITTBK+1)
!--------------SATURATION POINT VARIABLES AT THE BOTTOM-----------------
                TPSPK = P00K + (P10K-P00K)*PP(I,J) + (P01K-P00K)*QQ(I,J)  &
                             + (P00K-P10K-P01K+P11K)*PP(I,J)*QQ(I,J)

!!from WPP::tgs          APESPK=(H10E5/TPSPK)**CAPA
                if (TPSPK > 1.0e-3) then
                  APESPK = (max(0.,H10E5/ TPSPK))**CAPA
                else
                  APESPK = 0.0
                endif

                TTHESK = TTHBTK * EXP(ELOCP*QBTK*APESPK/TTHBTK)
!--------------CHECK FOR MAXIMUM THETA E--------------------------------
                IF(TTHESK > THESP(I,J)) THEN
                  PSP  (I,J)  = TPSPK
                  THESP(I,J)  = TTHESK
                  PARCEL(I,J) = KB
                ENDIF
              END IF 
            ENDDO  ! I  loop
          ENDDO    ! J  loop
        END IF
      ENDDO        ! KB loop

!----FIND THE PRESSURE OF THE PARCEL THAT WAS LIFTED
!$omp  parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            PPARC(I,J) = PMID(I,J,PARCEL(I,J))
          ENDDO
        ENDDO
!
!-----CHOOSE LAYER DIRECTLY BELOW PSP AS LCL AND------------------------
!-----ENSURE THAT THE LCL IS ABOVE GROUND.------------------------------
!-------(IN SOME RARE CASES FOR ITYPE=2, IT IS NOT)---------------------
      DO L=1,LM
!$omp  parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            IF (PMID(I,J,L) < PSP(I,J))    LCL(I,J) = L+1
          ENDDO
        ENDDO
      ENDDO
!$omp  parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          IF (LCL(I,J) > NINT(LMH(I,J))) LCL(I,J) = NINT(LMH(I,J))
          IF (ITYPE  > 2) THEN
            IF (T(I,J,LCL(I,J)) < 263.15) THEN
              THUNDER(I,J) = .FALSE.
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!---------FIND TEMP OF PARCEL LIFTED ALONG MOIST ADIABAT (TPAR)---------
!-----------------------------------------------------------------------

      DO L=LM,1,-1
!--------------SCALING PRESSURE & TT TABLE INDEX------------------------
        KNUML = 0
        KNUMH = 0
        DO J=JSTA,JEND
          DO I=1,IM
            KLRES(I,J) = 0
            KHRES(I,J) = 0
            IF(L <= LCL(I,J)) THEN
              IF(PMID(I,J,L) < PLQ)THEN
                KNUML = KNUML + 1
                KLRES(I,J) = 1
              ELSE
                KNUMH = KNUMH + 1
                KHRES(I,J) = 1
              ENDIF
            ENDIF
          ENDDO
        ENDDO
!***
!***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE<PLQ
!**
        IF(KNUML > 0) THEN
          CALL TTBLEX(TPAR(1,JSTA_2L,L),TTBL,ITB,JTB,KLRES             &
                    , PMID(1,JSTA_2L,L),PL,QQ,PP,RDP,THE0,STHE         &
                    , RDTHE,THESP,IPTB,ITHTB)
        ENDIF
!***
!***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE>PLQ
!**
        IF(KNUMH > 0) THEN
          CALL TTBLEX(TPAR(1,JSTA_2L,L),TTBLQ,ITBQ,JTBQ,KHRES          &
                    , PMID(1,JSTA_2L,L),PLQ,QQ,PP,RDPQ                 &
                     ,THE0Q,STHEQ,RDTHEQ,THESP,IPTB,ITHTB)
        ENDIF

!------------SEARCH FOR EQ LEVEL----------------------------------------
!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            IF(KHRES(I,J) > 0) THEN
              IF(TPAR(I,J,L) > T(I,J,L)) IEQL(I,J) = L
            ENDIF
          ENDDO
        ENDDO
!
!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            IF(KLRES(I,J) > 0) THEN
              IF(TPAR(I,J,L) > T(I,J,L)) IEQL(I,J) = L
            ENDIF
          ENDDO
        ENDDO
!-----------------------------------------------------------------------
      ENDDO                  ! end of do l=lm,1,-1 loop
!------------COMPUTE CAPE AND CINS--------------------------------------
      LBEG = 1000
      LEND = 0
      DO J=JSTA,JEND
        DO I=1,IM
          LBEG = MIN(IEQL(I,J),LBEG)
          LEND = MAX(LCL(I,J),LEND)
        ENDDO
      ENDDO
!
!$omp parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          IF(T(I,J,IEQL(I,J)) > 255.65) THEN
            THUNDER(I,J) = .FALSE.
          ENDIF
        ENDDO
      ENDDO
!
      DO L=LBEG,LEND

!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            IDX(I,J) = 0
            IF(L >= IEQL(I,J).AND.L <= LCL(I,J)) THEN
              IDX(I,J) = 1
            ENDIF
          ENDDO
        ENDDO
!
!$omp  parallel do private(i,j,gdzkl,presk,thetaa,thetap,esatp,qsatp,tvp,tv)
        DO J=JSTA,JEND
          DO I=1,IM
            IF(IDX(I,J) > 0) THEN
              PRESK  = PMID(I,J,L)
              GDZKL   = (ZINT(I,J,L)-ZINT(I,J,L+1)) * G
              ESATP  = min(FPVSNEW(TPAR(I,J,L)),PRESK)
              QSATP  = EPS*ESATP/(PRESK-ESATP*ONEPS)
              TVP    = TPAR(I,J,L)*(1+0.608*QSATP)
              THETAP = TVP*(H10E5/PRESK)**CAPA
              TV     = T(I,J,L)*(1+0.608*Q(I,J,L)) 
              THETAA = TV*(H10E5/PRESK)**CAPA
              IF(THETAP < THETAA) THEN
                CINS(I,J) = CINS(I,J) + (LOG(THETAP)-LOG(THETAA))*GDZKL
              ELSEIF(THETAP > THETAA) THEN
                CAPE(I,J) = CAPE(I,J) + (LOG(THETAP)-LOG(THETAA))*GDZKL
                IF (THUNDER(I,J) .AND. T(I,J,L)  < 273.15                 &
                                 .AND. T(I,J,L)  > 253.15) THEN
                 CAPE20(I,J) = CAPE20(I,J) + (LOG(THETAP)-LOG(THETAA))*GDZKL
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
!    
!     ENFORCE LOWER LIMIT OF 0.0 ON CAPE AND UPPER
!     LIMIT OF 0.0 ON CINS.
!
!$omp  parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          CAPE(I,J) = MAX(D00,CAPE(I,J))
          CINS(I,J) = MIN(CINS(I,J),D00)
! add equillibrium height
          ZEQL(I,J) = ZINT(I,J,IEQL(I,J))
          IF (CAPE20(I,J) < 75.) THEN
            THUNDER(I,J) = .FALSE.
          ENDIF
          IF (THUNDER(I,J)) THEN
            THUND(I,J) = 1.0
          ELSE
            THUND(I,J) = 0.0
          ENDIF
        ENDDO
      ENDDO
!     
      DEALLOCATE(TPAR)
!     
      RETURN
      END
