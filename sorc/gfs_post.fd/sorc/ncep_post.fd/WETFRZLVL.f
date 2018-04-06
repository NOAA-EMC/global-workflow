      SUBROUTINE WETFRZLVL(TWET,ZWET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    WETFRZLVL      COMPUTES LEVEL OF 0 WET BULB	 
! PRGRMMR: MANIKIN         ORG: W/NP2      DATE: 03-11-14     
!     
! ABSTRACT:
!     THIS ROUTINE COMPUTES THE LOWEST HEIGHT WITH A WET BULB
!     TEMPERATURE OF FREEZING FOR EACH MASS POINT ON THE ETA GRID.  
!     THE COMPUTED WET BULB ZERO HEIGHT IS THE MEAN SEA LEVEL
!     HEIGHT.  AT EACH MASS POINT WE MOVE UP FROM THE SURFACE TO 
!     FIND THE FIRST ETA LAYER WHERE THE TW IS LESS THAN
!     273.16K.  VERTICAL INTERPOLATION IN TEMPERATURE TO THE FREEZING
!     TEMPERATURE GIVES THE FREEZING LEVEL HEIGHT.  PRESSURE AND 
!     SPECIFIC HUMIDITY ARE INTERPOLATED TO THIS LEVEL AND ALONG WITH
!     THE TEMPERATURE PROVIDE THE FREEZING LEVEL RELATIVE HUMIDITY.
!     IF THE SURFACE (SKIN) TEMPERATURE IS BELOW FREEZING, THE ROUTINE
!     USES SURFACE BASED FIELDS TO COMPUTE THE RELATIVE HUMIDITY.
!     
! PROGRAM HISTORY LOG:
!   03-11-14 GEOFF MANIKIN - NEW PROGRAM 
!   04-12-06  G MANIKIN    - CORRECTED COMPUTATION OF SFC TEMPERATURE
!   05-03-11  H CHUANG     - WRF VERSION
!     
! USAGE:   CALL WETFRZLVL(TWET,ZWET) 
!   INPUT ARGUMENT LIST:
!     TWET     - WET BULB TEMPERATURES 
!
!   OUTPUT ARGUMENT LIST: 
!     ZWET     - ABOVE GROUND LEVEL HEIGHT OF LEVEL WITH 0 WET BULB.
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       REL_HUM
!     LIBRARY:
!       COMMON   - 
!                  LOOPS
!                  PVRBLS
!                  MASKS
!                  MAPOT
!                  POSTVAR
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
!     
      use vrbls3d, only: pint, zint, t
      use vrbls2d, only:  fis, thz0, ths
      use masks, only: lmh, sm
      use params_mod, only: gi, p1000, capa, tfrz, d0065, d50
      use ctlblk_mod, only: jsta, jend, im, jsta_2l, jend_2u, lm
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!     DECLARE VARIABLES.
!     
      REAL,intent(in) :: TWET(IM,JSTA_2L:JEND_2U,LM)
      REAL,intent(out) :: ZWET(IM,jsta:jend)
!     
      integer I,J,LLMH,L
      real HTSFC,THSFC,PSFC,TSFC,DELZ,DELT,ZL,ZU
!*********************************************************************
!     START FRZLVL.
!
!     LOOP OVER HORIZONTAL GRID.
!     
!!$omp  parallel do
!!$omp& private(delt,delz,htsfc,l,llmh
!!$omp&         tsfc,zl,zu)
      DO 20 J=JSTA,JEND
      DO 20 I=1,IM
         HTSFC     = FIS(I,J)*GI
         LLMH      = NINT(LMH(I,J))
         ZWET(I,J) = HTSFC
!     
!        CHECK IF FREEZING LEVEL IS AT THE GROUND.
!        IF YES, ESTIMATE UNDERGROUND FREEZING LEVEL USING 6.5C/KM LAPSE RATE
!        AND ASSUME RH TO BE EQUAL TO RH AT SFC
!     
         THSFC = (SM(I,J)*THZ0(I,J)+(1.-SM(I,J))*THS(I,J))
         PSFC  = PINT(I,J,LLMH+1)
         TSFC  = THSFC*(PSFC/P1000)**CAPA

         IF (TSFC.LE.TFRZ) THEN
!            ZWET(I,J) = HTSFC
            ZWET(I,J) = HTSFC+(TSFC-TFRZ)/D0065
            GOTO 20
         ENDIF
!     
!        OTHERWISE, LOCATE THE FREEZING LEVEL ALOFT.
!
         DO 10 L = LLMH,1,-1
            IF (TWET(I,J,L).LE.TFRZ) THEN
               IF (L.LT.LLMH-1) THEN
                  DELZ = D50*(ZINT(I,J,L)-ZINT(I,J,L+2))
                  ZL   = D50*(ZINT(I,J,L+1)+ZINT(I,J,L+2))
                  DELT = TWET(I,J,L)-TWET(I,J,L+1)
                  ZWET(I,J) = ZL + (TFRZ-TWET(I,J,L+1))/DELT*DELZ
               ELSE
                  ZU      = D50*(ZINT(I,J,L)+ZINT(I,J,L+1))
                  ZL      = HTSFC
                  DELZ    = ZU-ZL
                  TSFC    = SM(I,J)*THZ0(I,J)+(1.-SM(I,J))*THS(I,J)    &
                   *(PINT(I,J,NINT(LMH(I,J))+1)/P1000)**CAPA
                  DELT    = T(I,J,L)-TSFC
		  IF(DELT .NE. 0.)THEN  
                   ZWET(I,J) = ZL + (TFRZ-TSFC)/DELT*DELZ
		  ELSE
		   ZWET(I,J) = HTSFC+(TSFC-TWET(I,J,L))/D0065
		  END IF  
                  IF (ZWET(I,J) .GT. ZU) THEN
                    ZWET(I,J)=ZU
                  ENDIF
                   IF ((-1*ZWET(I,J)) .GT. ZU) THEN
                    ZWET(I,J)=ZU
                  endif
               ENDIF
               GOTO 20
            ENDIF
 10      CONTINUE
 20   CONTINUE
!     
!     END OF ROUTINE.
!     
      RETURN
      END
