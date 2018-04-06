      SUBROUTINE CALDRG(DRAGCO)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALDRG      COMPUTE DRAG COEFFICIENT
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-09-01
!     
! ABSTRACT:  THIS ROUTINE COMPUTES A SURFACE LAYER DRAG
!   COEFFICIENT USING EQUATION (7.4.1A) IN "AN INTRODUCTION
!   TO BOUNDARY LAYER METEOROLOGY" BY STULL (1988, KLUWER
!   ACADEMIC PUBLISHERS).
!   .     
!     
! PROGRAM HISTORY LOG:
!   93-09-01  RUSS TREADON
!   98-06-15  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION           
!   02-01-15  MIKE BALDWIN - WRF VERSION
!   05-02-22 H CHUANG - ADD WRF NMM COMPONENTS 
!     
! USAGE:    CALL CALDRG(DRAGCO)
!   INPUT ARGUMENT LIST:
!     NONE     
!
!   OUTPUT ARGUMENT LIST: 
!     DRAGCO   - SURFACE LAYER DRAG COEFFICIENT
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - LOOPS
!                  SRFDSP
!                  PVRBLS
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90
!     MACHINE : CRAY C-90
!$$$  
!     
!
      use vrbls3d, only: uh, vh
      use vrbls2d, only: uz0, vz0, ustar
      use masks, only: lmh
      use params_mod, only: d00, d50, d25
      use ctlblk_mod, only: jsta, jend, jsta_m, jend_m, modelname, spval, im, jm,  &
                            jsta_2l, jend_2u
      use gridspec_mod, only: gridtype
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!     INCLUDE/SET PARAMETERS.
!     
!     DECLARE VARIABLES.
      REAL,intent(inout) ::  DRAGCO(IM,jsta_2l:jend_2u)
      INTEGER IHE(JM),IHW(JM)
      integer I,J,LHMK,IE,IW,LMHK
      real UBAR,VBAR,WSPDSQ,USTRSQ,SUMU,SUMV,ULMH,VLMH,UZ0H,VZ0H
!     
!********************************************************************
!     START CALDRG HERE.
!     
!     INITIALIZE DRAG COEFFICIENT ARRAY TO ZERO.
!     
!$omp parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          DRAGCO(I,J) = D00
        ENDDO
      ENDDO
!

      IF(gridtype=='A')THEN 
       DO J=JSTA,JEND
       DO I=1,IM
!     
        LMHK=NINT(LMH(I,J))
!     
!        COMPUTE A MEAN MASS POINT WIND SPEED BETWEEN THE
!        FIRST ATMOSPHERIC ETA LAYER AND Z0.  ACCORDING TO
!        NETCDF OUTPUT, UZ0 AND VZ0 ARE AT MASS POINTS. (MEB 6/11/02)
!
        UBAR=D50*(UH(I,J,LMHK)+UZ0(I,J))
        VBAR=D50*(VH(I,J,LMHK)+VZ0(I,J))
        WSPDSQ=UBAR*UBAR+VBAR*VBAR
!     
!        COMPUTE A DRAG COEFFICIENT.
!
        USTRSQ=USTAR(I,J)*USTAR(I,J)
        IF(WSPDSQ .GT. 1.0E-6)DRAGCO(I,J)=USTRSQ/WSPDSQ
!
       ENDDO
       ENDDO
      ELSE IF(gridtype=='E')THEN
      
       DO J=JSTA_M,JEND_M
        IHE(J)=MOD(J+1,2)
        IHW(J)=IHE(J)-1
       ENDDO
       
       DO J=JSTA_M,JEND_M
       DO I=2,IM-1
!
!        COMPUTE A MEAN MASS POINT WIND IN THE
!        FIRST ATMOSPHERIC ETA LAYER.
!
        LMHK=NINT(LMH(I,J))
        IE=I+IHE(J)
        IW=I+IHW(J)
        SUMU=UH(IE,J,LMHK)+UH(IW,J,LMHK)+UH(I,J-1,LMHK)          &
          +UH(I,J+1,LMHK)
        SUMV=VH(IE,J,LMHK)+VH(IW,J,LMHK)+VH(I,J-1,LMHK)          &
          +VH(I,J+1,LMHK)
        ULMH=D25*SUMU
        VLMH=D25*SUMV
!
!        COMPUTE A MEAN MASS POINT WIND AT HEIGHT Z0.
!
        UZ0H=D25*(UZ0(IE,J)+UZ0(IW,J)+UZ0(I,J-1)+UZ0(I,J+1))
        VZ0H=D25*(VZ0(IE,J)+VZ0(IW,J)+VZ0(I,J-1)+VZ0(I,J+1))
!
!        COMPUTE A MEAN MASS POINT WIND SPEED BETWEEN THE
!        FIRST ATMOSPHERIC ETA LAYER AND Z0.
!
        UBAR=D50*(ULMH+UZ0H)
        VBAR=D50*(VLMH+VZ0H)
        WSPDSQ=UBAR*UBAR+VBAR*VBAR
!jjt  WSPDSQ=MIN(WSPDSQ,0.1)
!
!        COMPUTE A DRAG COEFFICIENT.
!
        USTRSQ=USTAR(I,J)*USTAR(I,J)
        IF(WSPDSQ .GT. 1.0E-6)DRAGCO(I,J)=USTRSQ/WSPDSQ
!
       END DO
       END DO
      ELSE IF(gridtype=='B')THEN 
       DO J=JSTA_M,JEND_M
       DO I=2,IM-1
!
!        COMPUTE A MEAN MASS POINT WIND IN THE
!        FIRST ATMOSPHERIC ETA LAYER.
!
        LMHK=NINT(LMH(I,J))
        IE=I
        IW=I-1
        SUMU=UH(IE,J,LMHK)+UH(IW,J,LMHK)+UH(I,J-1,LMHK)          &
          +UH(IW,J-1,LMHK)
        SUMV=VH(IE,J,LMHK)+VH(IW,J,LMHK)+VH(I,J-1,LMHK)          &
          +VH(IW,J-1,LMHK)
        ULMH=D25*SUMU
        VLMH=D25*SUMV
!
!        COMPUTE A MEAN MASS POINT WIND AT HEIGHT Z0.
!
! NEMS-NMMB is now putting uz0 and vz0 on mass points to save double interpolation time in
! the model
        IF(MODELNAME == 'NMM')THEN
          UZ0H=UZ0(I,J)
          VZ0H=VZ0(I,J)
        ELSE   
          UZ0H=D25*(UZ0(IE,J)+UZ0(IW,J)+UZ0(I,J-1)+UZ0(IW,J-1))
          VZ0H=D25*(VZ0(IE,J)+VZ0(IW,J)+VZ0(I,J-1)+VZ0(IW,J-1))
        END IF  
!
!        COMPUTE A MEAN MASS POINT WIND SPEED BETWEEN THE
!        FIRST ATMOSPHERIC ETA LAYER AND Z0.
!
        UBAR=D50*(ULMH+UZ0H)
        VBAR=D50*(VLMH+VZ0H)
        WSPDSQ=UBAR*UBAR+VBAR*VBAR
!jjt  WSPDSQ=MIN(WSPDSQ,0.1)
!
!        COMPUTE A DRAG COEFFICIENT.
!
        USTRSQ=USTAR(I,J)*USTAR(I,J)
        IF(WSPDSQ .GT. 1.0E-6)DRAGCO(I,J)=USTRSQ/WSPDSQ
!
       END DO
       END DO
      ELSE 
      
!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            DRAGCO(I,J) = SPVAL
          ENDDO
        ENDDO

      END IF 
!     
!     END OF ROUTINE.
!     
      RETURN
      END
