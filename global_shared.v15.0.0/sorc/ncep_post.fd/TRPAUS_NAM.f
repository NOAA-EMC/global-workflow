      SUBROUTINE TRPAUS(PTROP,TTROP,ZTROP,UTROP,VTROP,SHTROP)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    TRPAUS      COMPUTE TROPOPAUSE DATA.
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22       
!     
! ABSTRACT:  
!     THIS ROUTINE COMPUTES TROPOPAUSE DATA.  AT EACH MASS
!     POINT A SURFACE UP SEARCH IS MADE FOR THE FIRST 
!     OCCURRENCE OF A THREE LAYER MEAN LAPSE RATE LESS THAN
!     OR EQUAL TO A CRITICAL LAPSE RATE.  THIS CRITCAL LAPSE
!     RATE IS 2DEG/KM.  THIS IS IN ACCORD WITH THE WMO
!     DEFINITION OF A TROPOPAUSE.  A MAXIMUM TROPOPAUSE
!     PRESSURE OF 500MB IS ENFORCED.  ONC THE TROPOPAUSE
!     IS LOCATED IN A COLUMN, PRESSURE, TEMPERATURE, U
!     AND V WINDS, AND VERTICAL WIND SHEAR ARE COMPUTED.
!   .     
!     
! PROGRAM HISTORY LOG:
!   92-12-22  RUSS TREADON
!   97-03-06  GEOFF MANIKIN - CHANGED CRITERIA FOR DETERMINING
!                            THE TROPOPAUSE AND ADDED HEIGHT
!   98-06-15  T BLACK       - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO  - MPI VERSION
!   02-04-23  MIKE BALDWIN  - WRF VERSION
!     
! USAGE:    CALL TRPAUS(PTROP,TTROP,ZTROP,UTROP,VTROP,SHTROP)
!   INPUT ARGUMENT LIST:
!     NONE     
!
!   OUTPUT ARGUMENT LIST: 
!     PTROP    - TROPOPAUSE PRESSURE.
!     TTROP    - TROPOPAUSE TEMPERATURE.
!     ZTROP    - TROPOPAUSE HEIGHT
!     UTROP    - TROPOPAUSE U WIND COMPONENT.
!     VTROP    - TROPOPAUSE V WIND COMPONENT.
!     SHTROP   - VERTICAL WIND SHEAR AT TROPOPAUSE.
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
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
!     
!     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE PARAMETERS.
!
       use vrbls3d, only:
       use masks
       use params_mod
       use ctlblk_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!
!     PARAMTER CRTLAP SPECIFIES THE CRITICAL LAPSE RATE
!     (IN K/M) IDENTIFYING THE TROPOPAUSE.  WE START 
!     LOOKING FOR THE TROPOPAUSE ABOVE PRESSURE LEVEL
!     PSTART (IN PASALS).
      PARAMETER (CRTLAP=0.002E0, PSTART=5.0E4)
!     
!     DECLARE VARIABLES.
!     
      REAL PTROP(IM,JM),TTROP(IM,JM),ZTROP(IM,JM),UTROP(IM,JM)
      REAL VTROP(IM,JM),SHTROP(IM,JM)
      REAL TLAPSE(LM),DZ2(LM),DELT2(LM),TLAPSE2(LM)
!
      integer I,J
      real PM,DELT,DZ,RSQDIF
!     
!*****************************************************************************
!     START TRPAUS HERE.
!     
!     LOOP OVER THE HORIZONTAL GRID.
!    
      DO J=JSTA,JEND
      DO I=1,IM
         PTROP(I,J)  = SPVAL
         TTROP(I,J)  = SPVAL
         ZTROP(I,J)  = SPVAL
         UTROP(I,J)  = SPVAL
         VTROP(I,J)  = SPVAL
         SHTROP(I,J) = SPVAL
      ENDDO
      ENDDO
!
!$omp  parallel do
!$omp& private(delt,delt2,dz,dz2,ie,iw,l,llmh,pm,rsqdif,
!$omp&         tlapse,tlapse2,u0,u0l,uh,uh0,ul,
!$omp&         v0,v0l,vh,vh0)
      DO 20 J=JSTA,JEND
      DO 20 I=1,IM
!     
!        COMPUTE THE TEMPERATURE LAPSE RATE (-DT/DZ) BETWEEN ETA 
!        LAYERS MOVING UP FROM THE GROUND.  THE FIRST ETA LAYER
!        ABOVE PRESSURE "PSTART" IN WHICH THE LAPSE RATE IS LESS
!        THAN THE CRITCAL LAPSE RATE IS LABELED THE TROPOPAUSE.
!
        LLMH=NINT(LMH(I,J))
!
        DO 10 L=LLMH-1,2,-1
        PM     = PINT(I,J,L)
        DELT   = T(I,J,L-1)-T(I,J,L)
        DZ     = D50*(ZINT(I,J,L-1)-ZINT(I,J,L+1))
        TLAPSE(L) = -DELT/DZ
!
        IF ((TLAPSE(L).LT.CRTLAP).AND.(PM.LT.PSTART)) THEN 
          IF (L .EQ. 2 .AND. TLAPSE(L) .LT. CRTLAP) GOTO15
          DZ2(L+1) = 0.
!
          DO 17 LL=L,3,-1
          DZ2(LL) = 0.
          DELT2(LL) = 0.
          TLAPSE2(LL) = 0.
          DZ2(LL) = (2./3.)*(ZINT(I,J,LL-2)-ZINT(I,J,L+1))      
          IF ((DZ2(LL) .GT. 2000.) .AND.                       &
              (DZ2(LL+1) .GT. 2000.)) GO TO 15
          DELT2(LL) = T(I,J,LL-2)-T(I,J,L)
          TLAPSE2(LL) = -DELT2(LL)/DZ2(LL)
!
          IF (TLAPSE2(LL) .GT. CRTLAP) THEN
            GOTO 10
          ENDIF
!
   17     CONTINUE 
        ELSE
          GOTO 10 
        ENDIF 
!
   15   PTROP(I,J)  = D50*(PINT(I,J,L)+PINT(I,J,L+1))
        TTROP(I,J)  = T(I,J,L)
        ZTROP(I,J)= 0.5*(ZINT(I,J,L)+ZINT(I,J,L+1))
!
        UTROP (I,J) = UH(I,J,L)
        VTROP (I,J) = VH(I,J,L)
        DZ        = ZINT(I,J,L)-ZINT(I,J,L+1)
        RSQDIF    = SQRT(((UH(I,J,L-1)-UH(I,J,L+1))*0.5)**2     &
     &                  +((VH(I,J,L-1)-VH(I,J,L+1))*0.5)**2)
        SHTROP(I,J) = RSQDIF/DZ
        GOTO 20
   10   CONTINUE

!X         WRITE(88,*)'REACHED TOP FOR K,P,TLAPSE:  ',K,PM,TLAPSE

        DZ       = D50*(ZINT(I,J,2)-ZINT(I,J,3))
        PTROP(I,J) = D50*(PINT(I,J,2)+PINT(I,J,3))
        TTROP(I,J) = T(I,J,2)
        ZTROP(I,J)= D50*(ZINT(I,J,2)+ZINT(I,J,3))
        UTROP (I,J) = UH(I,J,2)
        VTROP (I,J) = VH(I,J,2)
        RSQDIF    = SQRT(((UH(I,J,1)-UH(I,J,3))*0.5)**2        &
     &                  +((VH(I,J,1)-VH(I,J,3))*0.5)**2)
        SHTROP(I,J) = RSQDIF/DZ

!X        WRITE(82,1010)I,J,L,PTROP(I,J)*D01,TTROP(I,J),
!X     X       UTROP(I,J),VTROP(I,J),SHTROP(I,J)
!     
   20 CONTINUE

!     
!     END OF ROUTINE.
!     
      RETURN
      END
