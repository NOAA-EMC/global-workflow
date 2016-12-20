      SUBROUTINE CALMCVG(Q1D,U1D,V1D,QCNVG)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALMCVG     COMPUTES MOISTURE CONVERGENCE
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-01-22       
!     
! ABSTRACT:
!     GIVEN SPECIFIC HUMIDITY, Q, AND THE U-V WIND COMPONENTS
!     THIS ROUTINE EVALUATES THE VECTOR OPERATION, 
!                      DEL DOT (Q*VEC)
!     WHERE,
!        DEL IS THE VECTOR GRADIENT OPERATOR,
!        DOT IS THE STANDARD DOT PRODUCT OPERATOR, AND
!        VEC IS THE VECTOR WIND.
!     MINUS ONE TIMES THE RESULTING SCALAR FIELD IS THE 
!     MOISTURE CONVERGENCE WHICH IS RETURNED BY THIS ROUTINE.
!   .     
!     
! PROGRAM HISTORY LOG:
!   93-01-22  RUSS TREADON
!   98-06-08  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION              
!   02-04-23  MIKE BALDWIN - WRF C-GRID VERSION     
!   05-07-07  BINBIN ZHOU - ADD RSM A GRID
!   06-04-25  H CHUANG - BUG FIXES TO CORECTLY COMPUTE MC AT BOUNDARIES 
!   
! USAGE:    CALL CALMCVG(Q1D,U1D,V1D,QCNVG)
!   INPUT ARGUMENT LIST:
!     Q1D      - SPECIFIC HUMIDITY AT P-POINTS (KG/KG)
!     U1D      - U WIND COMPONENT (M/S) AT P-POINTS
!     V1D      - V WIND COMPONENT (M/S) AT P-POINTS
!
!   OUTPUT ARGUMENT LIST: 
!     QCNVG    - MOISTURE CONVERGENCE (1/S) AT P-POINTS
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - MASKS
!                  DYNAM
!                  OPTIONS
!                  INDX
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90
!     MACHINE : CRAY C-90
!$$$  
!
!     
!     
      use masks,        only: dx, dy, hbm2
      use params_mod,   only: d00, d25
      use ctlblk_mod,   only: jsta_2l, jend_2u, spval, jsta_m, jend_m,       &
                              jsta_m2, jend_m2, im, jm
      use gridspec_mod, only: gridtype
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
!     DECLARE VARIABLES.
!     
      REAL,dimension(IM,jsta_2l:jend_2u),intent(in)    ::  Q1D, U1D, V1D
      REAL,dimension(IM,jsta_2l:jend_2u),intent(inout) ::  QCNVG

      REAL R2DY, R2DX
      REAL, dimension(im,jsta_2l:jend_2u) ::  UWND, VWND, QV
      INTEGER IHE(JM),IHW(JM),IVE(JM),IVW(JM)
      integer I,J,ISTA,IEND
      real QVDY,QUDX
!     
!***************************************************************************
!     START CALMCVG HERE.
!
!     
!     INITIALIZE MOISTURE CONVERGENCE ARRAY.  LOAD TEMPORARY WIND ARRAYS.
!     
!$omp  parallel do private(i,j)
      DO J=JSTA_2L,JEND_2U
        DO I=1,IM
          QCNVG(I,J) = 0.
          UWND(I,J)  = U1D(I,J)
          VWND(I,J)  = V1D(I,J)
          IF (UWND(I,J) == SPVAL) UWND(I,J) = D00
          IF (VWND(I,J) == SPVAL) VWND(I,J) = D00
        ENDDO
      ENDDO
      
      CALL EXCH_F(Q1D)
      CALL EXCH_F(VWND)
!
      IF(gridtype == 'A')THEN
!$omp  parallel do private(i,j,qudx,qvdy,r2dx,r2dy)
       DO J=JSTA_M,JEND_M
         DO I=2,IM-1
           IF(VWND(I,J+1).LT.SPVAL.AND.VWND(I,J-1).LT.SPVAL.AND.          &
              UWND(I+1,J).LT.SPVAL.AND.UWND(I-1,J).LT.SPVAL) THEN
             R2DX   = 1./(2.*DX(I,J))   !MEB DX?
             R2DY   = 1./(2.*DY(I,J))   !MEB DY?  
             QUDX   = (Q1D(I+1,J)*UWND(I+1,J)-Q1D(I-1,J)*UWND(I-1,J))*R2DX
             QVDY   = (Q1D(I,J+1)*VWND(I,J+1)-Q1D(I,J-1)*VWND(I,J-1))*R2DY
             QCNVG(I,J) = -(QUDX + QVDY)
           ENDIF
         ENDDO
       ENDDO
      ELSE IF(gridtype == 'E')THEN

       DO J=JSTA_M,JEND_M
         IHE(J) = MOD(J+1,2)
         IHW(J) = IHE(J)-1
         IVE(J) = MOD(J,2)
         IVW(J) = IVE(J)-1 
       END DO
     
!$omp  parallel do private(i,j,ista,iend)
       DO J=JSTA_M,JEND_M
         ISTA = 1+MOD(J+1,2)
         IEND = IM-MOD(J,2)
         DO I=ISTA,IEND
           QV(I,J) = D25*(Q1D(I,J-1)+Q1D(I+IVW(J),J)                   &
                         +Q1D(I+IVE(J),J)+Q1D(I,J+1))
         END DO
       END DO

       CALL EXCH_F(QV)
!      CALL EXCH_F(VWND)

!
!$omp  parallel do private(i,j,iend,qudx,qvdy,r2dx,r2dy)
       DO J=JSTA_M2,JEND_M2
         IEND = IM-1-MOD(J,2)
         DO I=2,IEND
           R2DX   = 1./(2.*DX(I,J))
           R2DY   = 1./(2.*DY(I,J))
           QUDX   = (QV(I+IHE(J),J)*UWND(I+IHE(J),J)                   &
                    -QV(I+IHW(J),J)*UWND(I+IHW(J),J))*R2DX
           QVDY   = (QV(I,J+1)*VWND(I,J+1)-QV(I,J-1)*VWND(I,J-1))*R2DY

           QCNVG(I,J) = -(QUDX + QVDY) * HBM2(I,J)
         ENDDO
       ENDDO
      ELSE IF(gridtype=='B')THEN
     
       CALL EXCH_F(UWND)
!
!$omp  parallel do private(i,j,iend,qudx,qvdy,r2dx,r2dy)
       DO J=JSTA_M,JEND_M
        DO I=2,IM-1
          R2DX   = 1./DX(I,J)
          R2DY   = 1./DY(I,J)
          QUDX=(0.5*(UWND(I,J)+UWND(I,J-1))*0.5*(Q1D(I,J)+Q1D(I+1,J))        &
               -0.5*(UWND(I-1,J)+UWND(I-1,J-1))*0.5*(Q1D(I,J)+Q1D(I-1,J)))*R2DX
          QVDY=(0.5*(VWND(I,J)+VWND(I-1,J))*0.5*(Q1D(I,J)+Q1D(I,J+1))        &
               -0.5*(VWND(I,J-1)+VWND(I-1,J-1))*0.5*(Q1D(I,J)+Q1D(I,J-1)))*R2DY
  
          QCNVG(I,J) = -(QUDX + QVDY)
!	  print*,'mcvg=',i,j,r2dx,r2dy,QCNVG(I,J)
        ENDDO
       ENDDO
      ENDIF
!meb not sure about the indexing for the c-grid
!
!     END OF ROUTINE.
!     
      RETURN
      END

