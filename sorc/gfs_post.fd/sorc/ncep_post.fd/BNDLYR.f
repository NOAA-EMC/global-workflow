      SUBROUTINE BNDLYR(PBND,TBND,QBND,RHBND,UBND,VBND,       &
                        WBND,OMGBND,PWTBND,QCNVBND,LVLBND)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    BNDLYR      COMPUTES CONSTANT MASS MEAN FIELDS
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-01-29
!     
! ABSTRACT:  THIS ROUTINE COMPUTES CONSTANT MASS (BOUNDARY LAYER)
!   FIELDS.  THE FIELDS ARE A MEAN OVER LAYERS PARAMETER DPBND
!   (PASCALS) THICK.  THERE ARE NBND CONSTANT MASS LAYERS, EACH
!   DPBND THICK STARTING FROM THE SURFACE UP.  COMPUTED BOUNDARY 
!   LAYER FIELDS ARE PRESSURE, TEMPERATURE, SPECIFIC HUMIDITY,
!   RELATIVE HUMIDITY, U AND V WINDS, VERTICAL VELOCITY,
!   AND PRECIPITABLE WATER.  GIVEN THESE FUNDAMENTAL VARIABLES
!   OTHER FIELDS MAY BE COMPUTED.
!
!   ***WARNING*** IF YOU CHANGE PARAMETER NBND IN THIS ROUTINE 
!                 DON'T FOREGET TO CHANGE IT ALSO IN THE CALLING
!                 SUBPROGRAM, MISCLN.
!   .     
!     
! PROGRAM HISTORY LOG:
!   93-01-29  RUSS TREADON
!   93-05-07  RUSS TREADON - ADDED DOC BLOCK AND MORE COMMENTS.
!   93-06-19  RUSS TREADON - ADDED LVLBND TO PARAMETER LIST.
!   96-03-07  MIKE BALDWIN - CHANGE PWTR CALC TO INCLUDE CLD WTR
!                            SPEED UP CODE
!   98-06-16  T BLACK      - CONVERSION FROM 1-D TO 2-D
!   98-08-18  MIKE BALDWIN - CHANGE QSBND TO RHBND IN CALL,
!                            COMPUTE RH OVER ICE
!   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
!   00-01-04  JIM TUCCILLO - MPI VERSION 
!   02-01-15  MIKE BALDWIN - WRF VERSION
!     
!     USAGE:    CALL BNDLYR(PBND,TBND,QBND,RHBND,UBND,VBND,
!                            WBND,OMGBND,PWTBND,QCNVBND)
!           
!   INPUT ARGUMENT LIST:
!     NONE     
!
!   OUTPUT ARGUMENT LIST: 
!     PBND     - LAYER MEAN PRESSURE IN NBND BOUNDARY LAYERS (NBL).
!     TBND     - LAYER MEAN TEMPERATURE IN NBL.
!     QBND     - LAYER MEAN SPECIFIC HUMIDITY IN NBL.
!     RHBND    - LAYER MEAN RELATIVE HUM. (QBND/QSBND) IN  NBL.
!     UBND     - LAYER MEAN U WIND COMPONENT IN NBL.
!     VBND     - LAYER MEAN V WIND COMPONENT IN NBL.
!     WBND     - LAYER MEAN W WIND COMPONENT IN NBL.
!     OMGBND   - LAYER MEAN VERTICAL VELOCITY IN NBL.
!     PWTBND   - LAYER PRECIPITABLE WATER IN NBL.
!     LVLBND   - ETA LAYER AT MIDPOINT OF NBL.
!     QCNVBND  - LAYER MOISTURE CONVERGENCE IN NBL.
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!
!     LIBRARY:
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90
!     MACHINE : CRAY C-90
!$$$  
!     
!
      use vrbls3d,    only: pint, q, uh, vh, pmid, t, omga, wh, cwm
      use masks,      only: lmh
      use params_mod, only: d00, gi, pq0, a2, a3, a4
      use ctlblk_mod, only: jsta_2l, jend_2u, lm, jsta, jend, modelname,      &
                            jsta_m, jend_m, im, nbnd
      use physcons,   only: con_rd, con_rv, con_eps, con_epsm1
      use gridspec_mod, only: gridtype
!
      implicit none
!
!     DECLARE VARIABLES.
!
      real,external :: FPVSNEW
      real,PARAMETER :: DPBND=30.E2
      integer, dimension(IM,jsta:jend,NBND),intent(inout) :: LVLBND
      real,    dimension(IM,jsta:jend,NBND),intent(inout) :: PBND,TBND,  &
               QBND,RHBND,UBND,VBND,WBND,OMGBND,PWTBND,QCNVBND

      REAL Q1D(IM,JSTA_2L:JEND_2U),V1D(IM,JSTA_2L:JEND_2U),              &
           U1D(IM,JSTA_2L:JEND_2U),QCNV1D(IM,JSTA_2L:JEND_2U)
!
      REAL, ALLOCATABLE :: PBINT(:,:,:),QSBND(:,:,:)
      REAL, ALLOCATABLE :: PSUM(:,:,:), QCNVG(:,:,:)
      REAL, ALLOCATABLE :: PVSUM(:,:,:),NSUM(:,:,:)
!
      integer I,J,L,IE,IW,LL,LV,LBND
      real DP,QSAT,PV1,PV2,PMV,RPSUM,RPVSUM,PMIN,PM,DELP,PMINV,DELPV
      real es
!
!*****************************************************************************
!     START BNDLYR HERE
!
      ALLOCATE (PBINT(IM,JSTA_2L:JEND_2U,NBND+1))
      ALLOCATE (QSBND(IM,JSTA_2L:JEND_2U,NBND))
      ALLOCATE (PSUM(IM,JSTA_2L:JEND_2U,NBND))
      ALLOCATE (QCNVG(IM,JSTA_2L:JEND_2U,LM))
      ALLOCATE (PVSUM(IM,JSTA_2L:JEND_2U,NBND))
      ALLOCATE (NSUM(IM,JSTA_2L:JEND_2U,NBND))
!
!     LOOP OVER HORIZONTAL GRID.  AT EACH MASS POINT COMPUTE
!     PRESSURE AT THE INTERFACE OF EACH BOUNDARY LAYER.
!     
!$omp  parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          PBINT(I,J,1) = PINT(I,J,NINT(LMH(I,J))+1)
        ENDDO
      ENDDO
!
      DO LBND=2,NBND+1
!$omp  parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            PBINT(I,J,LBND) = PBINT(I,J,LBND-1) - DPBND
          ENDDO
        ENDDO
      ENDDO

!          COMPUTE MOISTURE CONVERGENCE FOR EVERY LEVEL
      DO L=1,LM
!$omp  parallel do private(i,j)
        DO J=JSTA_2L,JEND_2U
          DO I=1,IM
            Q1D(I,J) = Q(I,J,L)
            U1D(I,J) = UH(I,J,L)
            V1D(I,J) = VH(I,J,L)
           ENDDO
        ENDDO
        CALL CALMCVG(Q1D,U1D,V1D,QCNV1D)
!$omp  parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            QCNVG(I,J,L)=QCNV1D(I,J)
          ENDDO
        ENDDO
      ENDDO

!     
!     LOOP OVER HORIZONTAL.  AT EACH MASS POINT COMPUTE 
!     MASS WEIGHTED LAYER MEAN P, T, Q, U, V, OMEGA, 
!     WAND PRECIPITABLE WATER IN EACH BOUNDARY LAYER FROM THE SURFACE UP.
!     
!!$omp+ private(dp,pm,qsat)
!!$omp  parallel do private(i,j,lbnd,l,ie,iw,dp,pm,qsat,pv1,pv2,pmv)
      DO LBND=1,NBND
!$omp  parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            PBND(I,J,LBND)   = D00
            TBND(I,J,LBND)   = D00
            QBND(I,J,LBND)   = D00
            QSBND(I,J,LBND)  = D00
            RHBND(I,J,LBND)  = D00
            UBND(I,J,LBND)   = D00
            VBND(I,J,LBND)   = D00
            WBND(I,J,LBND)   = D00
            OMGBND(I,J,LBND) = D00
            LVLBND(I,J,LBND) = 0
            NSUM(I,J,LBND)   = 0
            PSUM(I,J,LBND)   = D00
            PVSUM(I,J,LBND)  = D00
            PWTBND(I,J,LBND) = D00
            QCNVBND(I,J,LBND)= D00
          ENDDO
        ENDDO
!
!$omp  parallel do private(i,j,l,dp,pm,es,qsat)
        DO L=1,LM
          DO J=JSTA,JEND
            DO I=1,IM
!
              PM = PMID(I,J,L)
              IF((PBINT(I,J,LBND)   >= PM).AND.              & 
                 (PBINT(I,J,LBND+1) <= PM)) THEN
                DP = PINT(I,J,L+1) - PINT(I,J,L)
                PSUM(I,J,LBND)   = PSUM(I,J,LBND)   + DP
                NSUM(I,J,LBND)   = NSUM(I,J,LBND)   + 1
                LVLBND(I,J,LBND) = LVLBND(I,J,LBND) + L
                TBND(I,J,LBND)   = TBND(I,J,LBND)   + T(I,J,L)*DP
                QBND(I,J,LBND)   = QBND(I,J,LBND)   + Q(I,J,L)*DP
                OMGBND(I,J,LBND) = OMGBND(I,J,LBND) + OMGA(I,J,L)*DP
                IF(gridtype == 'A')THEN
                  UBND(I,J,LBND)  = UBND(I,J,LBND) + UH(I,J,L)*DP
                  VBND(I,J,LBND)  = VBND(I,J,LBND) + VH(I,J,L)*DP
                END IF
                WBND(I,J,LBND)    = WBND(I,J,LBND) + WH(I,J,L)*DP
                QCNVBND(I,J,LBND) = QCNVBND(I,J,LBND) + QCNVG(I,J,L)*DP
                PWTBND(I,J,LBND)  = PWTBND(I,J,LBND)           &
                                  + ( Q(I,J,L)+CWM(I,J,L))*DP*GI
                IF(MODELNAME == 'GFS')THEN
                  ES   = min(FPVSNEW(T(I,J,L)),PM)
                  QSAT = CON_EPS*ES/(PM+CON_EPSM1*ES)
                ELSE
                  QSAT = PQ0/PM*EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
                END IF 
                QSBND(I,J,LBND) = QSBND(I,J,LBND) + QSAT*DP
              ENDIF
            ENDDO
          ENDDO
        ENDDO


        IF(gridtype=='E')THEN
          CALL EXCH(PINT(1:IM,JSTA_2L:JEND_2U,1))
          DO L=1,LM
            CALL EXCH(PINT(1:IM,JSTA_2L:JEND_2U,L+1))
!$omp  parallel do private(i,j,ie,iw,dp,pv1,pv2,pmv)
            DO J=JSTA_M,JEND_M
              DO I=2,IM-1
                IE = I+MOD(J,2)
                IW = I+MOD(J,2)-1
                PV1 = 0.25*(PINT(IW,J,L)    + PINT(IE,J,L)    &
                           +PINT(I,J+1,L)   + PINT(I,J-1,L))
                PV2 = 0.25*(PINT(IW,J,L+1)  + PINT(IE,J,L+1)  &
                           +PINT(I,J+1,L+1) + PINT(I,J-1,L+1))
                DP  = PV2-PV1
                PMV = 0.5*(PV1+PV2)
                IF((PBINT(IW,J,LBND).GE.PMV).AND.        &
                   (PBINT(IW,J,LBND+1).LE.PMV)) THEN
                  PVSUM(I,J,LBND) = PVSUM(I,J,LBND) + DP
                  UBND(I,J,LBND)  = UBND(I,J,LBND)  + DP* UH(I,J,L)
                  VBND(I,J,LBND)  = VBND(I,J,LBND)  + DP*VH(I,J,L)
                ENDIF
!
              ENDDO
            ENDDO
          ENDDO
        ELSE IF (gridtype=='B')THEN
          CALL EXCH(PINT(1:IM,JSTA_2L:JEND_2U,1))
          DO L=1,LM
            CALL EXCH(PINT(1:IM,JSTA_2L:JEND_2U,L+1))
!$omp  parallel do private(i,j,ie,iw,dp,pv1,pv2,pmv)
            DO J=JSTA_M,JEND_M
              DO I=2,IM-1
                IE = I+1
                IW = I
                PV1 = 0.25*(PINT(IW,J,L)     + PINT(IE,J,L)      &
                           +PINT(IW,J+1,L)   + PINT(IE,J+1,L))
                PV2 = 0.25*(PINT(IW,J,L+1)   + PINT(IE,J,L+1)    &
                           +PINT(IW,J+1,L+1) + PINT(IE,J+1,L+1))
                DP  = PV2-PV1
                PMV = 0.5*(PV1+PV2)
                IF((PBINT(IW,J,LBND).GE.PMV).AND.        &
                   (PBINT(IW,J,LBND+1).LE.PMV)) THEN
                  PVSUM(I,J,LBND) = PVSUM(I,J,LBND)+DP
                  UBND(I,J,LBND)  = UBND(I,J,LBND)+UH(I,J,L)*DP
                  VBND(I,J,LBND)  = VBND(I,J,LBND)+VH(I,J,L)*DP
                ENDIF
!
              ENDDO
            ENDDO
          ENDDO
        END IF
       
      ENDDO            ! end of lbnd loop
!
!!$omp+ private(rpsum)
!$omp  parallel do private(i,j,lbnd,rpsum,rpvsum)
      DO LBND=1,NBND
        DO J=JSTA,JEND
          DO I=1,IM
            IF(PSUM(I,J,LBND).NE.0.)THEN
              RPSUM           = 1./PSUM(I,J,LBND)
              LVLBND(I,J,LBND)= LVLBND(I,J,LBND)/NSUM(I,J,LBND)
              PBND(I,J,LBND)  = (PBINT(I,J,LBND)+PBINT(I,J,LBND+1))*0.5
              TBND(I,J,LBND)  = TBND(I,J,LBND)*RPSUM
              QBND(I,J,LBND)  = QBND(I,J,LBND)*RPSUM
              QSBND(I,J,LBND) = QSBND(I,J,LBND)*RPSUM
              OMGBND(I,J,LBND)= OMGBND(I,J,LBND)*RPSUM
              IF(gridtype=='A')THEN
               UBND(I,J,LBND)  = UBND(I,J,LBND)*RPSUM
               VBND(I,J,LBND)  = VBND(I,J,LBND)*RPSUM
              END IF 
              WBND(I,J,LBND)     = WBND(I,J,LBND)*RPSUM
              QCNVBND(I,J,LBND)  = QCNVBND(I,J,LBND)*RPSUM
            ENDIF
          ENDDO
        ENDDO

        IF(gridtype=='E' .or. gridtype=='B')THEN
          DO J=JSTA_M,JEND_M
            DO I=2,IM-1
              IF(PVSUM(I,J,LBND).NE.0.)THEN
                RPVSUM         = 1./PVSUM(I,J,LBND)
                UBND(I,J,LBND) = UBND(I,J,LBND)*RPVSUM
                VBND(I,J,LBND) = VBND(I,J,LBND)*RPVSUM
              ENDIF
            ENDDO
          ENDDO
        END IF 
      ENDDO
!
!  IF NO ETA MID LAYER PRESSURES FELL WITHIN A BND LYR,
!   FIND THE CLOSEST LAYER TO THE BND LYR AND ASSIGN THE VALUES THERE
!
!!$omp+ private(delp,dp,l,pm,pmin,qsat)
!$omp  parallel do private(i,j,lbnd,l,ll,ie,iw,pminv,delp,dp,pm,pmin,es,qsat,pmv,delpv)
      DO LBND=1,NBND
        DO J=JSTA,JEND
          DO I=1,IM
            IF(PSUM(I,J,LBND).EQ.0.)THEN
              L    = LM
              PMIN = 9999999.
              PBND(I,J,LBND) = (PBINT(I,J,LBND)+PBINT(I,J,LBND+1))*0.5
!
              DO LL=1,LM
                PM   = PMID(I,J,LL)
                DELP = ABS(PM-PBND(I,J,LBND))
                IF(DELP.LT.PMIN)THEN
                  PMIN = DELP
                  L    = LL
                ENDIF
              ENDDO
!
              DP = PINT(I,J,L+1)-PINT(I,J,L)
              PM = PMID(I,J,L)
              LVLBND(I,J,LBND) = L
              TBND(I,J,LBND)   = T(I,J,L)
              QBND(I,J,LBND)   = Q(I,J,L)
              IF(gridtype == 'A')THEN
                UBND(I,J,LBND) = UH(I,J,L)
                VBND(I,J,LBND) = VH(I,J,L)
              END IF 
              WBND(I,J,LBND)    = WH(I,J,L)
              QCNVBND(I,J,LBND) = QCNVG(I,J,L)
              IF(MODELNAME == 'GFS')THEN
                ES   = FPVSNEW(T(I,J,L))
                ES   = MIN(ES,PM)
                QSAT = CON_EPS*ES/(PM+CON_EPSM1*ES)
              ELSE
                QSAT=PQ0/PM*EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
              END IF  
              QSBND(I,J,LBND)  = QSAT
              OMGBND(I,J,LBND) = OMGA(I,J,L)
              PWTBND(I,J,LBND) = (Q(I,J,L)+CWM(I,J,L))*DP*GI
            ENDIF
!
!   RH, BOUNDS CHECK
!
            RHBND(I,J,LBND) = QBND(I,J,LBND)/QSBND(I,J,LBND)
            IF (RHBND(I,J,LBND).GT.1.0) THEN
              RHBND(I,J,LBND) = 1.0
              QBND(I,J,LBND)  = RHBND(I,J,LBND)*QSBND(I,J,LBND)
            ENDIF
            IF (RHBND(I,J,LBND).LT.0.01) THEN
              RHBND(I,J,LBND) = 0.01
              QBND(I,J,LBND)  = RHBND(I,J,LBND)*QSBND(I,J,LBND)
            ENDIF
          ENDDO
        ENDDO
!
        IF(gridtype == 'E')THEN
          DO J=JSTA_M,JEND_M
            DO I=2,IM-1
              IF(PVSUM(I,J,LBND).EQ.0.)THEN
              LV = LM
              PMINV = 9999999.
              IE = I+MOD(J,2)
              IW = I+MOD(J,2)-1
! 
!           PINT HALOS UPDATED ALREADY
!
              DO LL=1,LM
                PMV = 0.125*(PINT(IW,J,LL)    + PINT(IE,J,LL)   +      &
                             PINT(I,J+1,LL)   + PINT(I,J-1,LL)  +      &
                             PINT(IW,J,LL+1)  + PINT(IE,J,LL+1) +      &
                             PINT(I,J+1,LL+1) + PINT(I,J-1,LL+1))
                DELPV = ABS(PMV-PBND(I,J,LBND))
                IF(DELPV.LT.PMINV)THEN
                  PMINV = DELPV
                  LV    = LL
                ENDIF
              ENDDO
!
              UBND(I,J,LBND) = UH(I,J,LV)
              VBND(I,J,LBND) = VH(I,J,LV)
            ENDIF
           ENDDO
         ENDDO     
!        END IF

        ELSE IF(gridtype=='B')THEN
          DO J=JSTA_M,JEND_M
            DO I=2,IM-1
              IF(PVSUM(I,J,LBND).EQ.0.)THEN
                LV=LM
                PMINV=9999999.
                IE=I+1
                IW=I
!
!           PINT HALOS UPDATED ALREADY
!
                DO LL=1,LM
                  PMV=0.125*(PINT(IW,J,LL)+PINT(IE,J,LL)+      &
                        PINT(IW,J+1,LL)+PINT(IE,J+1,LL)+       &
                        PINT(IW,J,LL+1)+PINT(IE,J,LL+1)+       &
                        PINT(IW,J+1,LL+1)+PINT(IE,J+1,LL+1))
                  DELPV=ABS(PMV-PBND(I,J,LBND))
                  IF(DELPV.LT.PMINV)THEN
                    PMINV=DELPV
                    LV=LL
                  ENDIF
                ENDDO

                UBND(I,J,LBND) = UH(I,J,LV)
                VBND(I,J,LBND) = VH(I,J,LV)
              ENDIF
            ENDDO
          ENDDO     
        END IF 
      ENDDO                ! end of lbnd loop
!
      DEALLOCATE (PBINT, QSBND, PSUM, PVSUM, QCNVG, NSUM)
!
!     END OF ROUTINE
!     
      RETURN
      END
!
