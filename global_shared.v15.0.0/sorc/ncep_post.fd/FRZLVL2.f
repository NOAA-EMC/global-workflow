      SUBROUTINE FRZLVL2(ZFRZ,RHFRZ,PFRZL)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    FRZLVL      COMPUTES FRZING LVL Z AND RH
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22       
!     
! ABSTRACT:
!     THIS ROUTINE COMPUTES THE FREEZING LEVEL HEIGHT AND RELATIVE
!     HUMIDITY AT THIS LEVEL FOR EACH MASS POINT ON THE ETA GRID.
!     THE COMPUTED FREEZING LEVEL HEIGHT IS THE MEAN SEA LEVEL
!     HEIGHT.  AT EACH MASS POINT WE MOVE UP FROM THE SURFACE TO 
!     FIND THE LAST ETA LAYER WHERE THE TEMPERATURE IS LESS THAN
!     273.16K AND THE TEMP IN THE LAYER BELOW IS ABOVE 273.16K.
!     VERTICAL INTERPOLATION IN TEMPERATURE TO THE FREEZING
!     TEMPERATURE GIVES THE FREEZING LEVEL HEIGHT.  PRESSURE AND 
!     SPECIFIC HUMIDITY ARE INTERPOLATED TO THIS LEVEL AND ALONG WITH
!     THE TEMPERATURE PROVIDE THE FREEZING LEVEL RELATIVE HUMIDITY.
!     IF THE ENTIRE ATMOSPHERE IS BELOW FREEZING, THE ROUTINE
!     USES SURFACE BASED FIELDS TO COMPUTE THE RELATIVE HUMIDITY.
!     
!     NOTE THAT IN POSTING FREEZING LEVEL DATA THE LFM LOOK-ALIKE FILE
!     (IE, GRID 26), WE PACK 273.15K AS THE FREEZING TEMPERATURE.  ALL
!     OTHER OUTPUT GRIDS USE 273.16K
!   .     
!     
! PROGRAM HISTORY LOG:
!   92-12-22  RUSS TREADON
!   93-06-10  RUSS TREADON - CORRECTED FREEZING LEVEL HEIGHTS TO BE
!                            WITH REPSECT TO MEAN SEA LEVEL, NOT  
!                            ABOVE GROUND LEVEL.
!   95-03-10  MIKE BALDWIN - GET HIGHEST FREEZING LEVEL.
!   98-06-15  T BLACK      - CONVERSION FROM 1-D TO 2-D
!   98-08-17  MIKE BALDWIN - COMPUTE RH OVER ICE IF NECESSARY
!   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
!   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
!   00-01-04  JIM TUCCILLO - MPI VERSION
!   01-10-25  H CHUANG     - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-01-15  MIKE BALDWIN - WRF VERSION
!   10-08-27  T. Smirnova  - added PFRZL to the output
!
! USAGE:    CALL FRZLVL(ZFRZ,RHFRZ)
!   INPUT ARGUMENT LIST:
!     NONE     
!
!   OUTPUT ARGUMENT LIST: 
!     ZFRZ     - ABOVE GROUND LEVEL FREEZING HEIGHT.
!     RHFRZ    - RELATIVE HUMIDITY AT FREEZING LEVEL.
!     PFRZL    - PRESSURE AT FREEZING LEVEL.
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - 
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
      use vrbls3d, only: pint, t, zmid, pmid, q, zint, alpint
      use vrbls2d, only: fis, tshltr, pshltr, qz0, qs, qshltr
      use masks, only: lmh, sm
      use params_mod, only: gi, d00, capa, d0065, tfrz, pq0, a2, a3, a4, d50
      use ctlblk_mod, only: jsta, jend, spval, lm, modelname, im
      use physcons, only: con_rd, con_rv, con_eps, con_epsm1

      implicit none

      real,external::FPVSNEW
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!      implicit none
!     
!     DECLARE VARIABLES.
!
      REAL,PARAMETER::PUCAP=300.0E2
      REAL,dimension(im,jsta:jend),intent(out) ::  RHFRZ, ZFRZ, PFRZL
!jw
      integer I,J,L,LICE,LLMH
      real HTSFC,PSFC,QSFC,RHSFC,QW,QSAT,DELZ,DELT,DELQ,DELALP,DELZP,  &
           ZL,ZU,DZABV,QFRZ,ALPL,ALPH,ALPFRZ,PFRZ,QSFRZ,RHZ,DZFR, &
           TSFC,es
!     
!*********************************************************************
!     START FRZLVL.
!
!     LOOP OVER HORIZONTAL GRID.
!     

      DO 20 J=JSTA,JEND
      DO 20 I=1,IM
         HTSFC    = FIS(I,J)*GI
         LLMH     = NINT(LMH(I,J))
         RHFRZ(I,J) = D00
         ZFRZ(I,J)  = HTSFC
         PSFC     = PINT(I,J,LLMH)
         PFRZL(I,J) = PSFC
!     
!        FIND THE HIGHEST LAYER WHERE THE TEMPERATURE
!        CHANGES FROM ABOVE TO BELOW FREEZING.
!     
!         TSFC = (SM(I,J)*THZ0(I,J)+(1.-SM(I,J))*THS(I,J))    &
!         	 *(PINT(I,J,NINT(LMH(I,J))+1)/P1000)**CAPA	 
         IF(TSHLTR(I,J)/=SPVAL .AND. PSHLTR(I,J)/=SPVAL)THEN
          TSFC=TSHLTR(I,J)*(PSHLTR(I,J)*1.E-5)**CAPA
         ELSE
! GFS analysis does not have flux file to retrieve TSFC from
          TSFC=T(I,J,LM)+D0065*(ZMID(I,J,LM)-HTSFC-2.0)
         END IF
         LICE=LLMH
! Per AWC's request, put a 300 mb cap for highest freezing level so that it
! does not go into stratosphere
         DO L = LLMH-1,1,-1
            IF (PMID(I,J,L).GE.PUCAP .AND. &
	    (T(I,J,L).LE.TFRZ.AND.T(I,J,L+1).GT.TFRZ))LICE=L
         ENDDO
!     
!        CHECK IF FREEZING LEVEL IS AT THE GROUND.
!     
         IF (LICE.EQ.LLMH.AND.TSFC.LE.TFRZ) THEN
            ZFRZ(I,J) = HTSFC+2.0+(TSFC-TFRZ)/D0065
            QSFC    = SM(I,J)*QZ0(I,J)+(1.-SM(I,J))*QS(I,J)
            IF(QSHLTR(I,J)/=SPVAL)THEN
             PSFC=PSHLTR(I,J)
             QSFC=QSHLTR(I,J)
            ELSE
             QSFC=Q(I,J,LM)
             PSFC=PMID(I,J,LM)
            END IF
            PFRZL(I,J) = PSFC
!
            IF(MODELNAME == 'GFS' .OR. MODELNAME == 'RAPR')THEN
             ES=FPVSNEW(TSFC)
             ES=MIN(ES,PSFC)
             QSAT=CON_EPS*ES/(PSFC+CON_EPSM1*ES)
            ELSE
             QSAT=PQ0/PSFC  &
               *EXP(A2*(TSFC-A3)/(TSFC-A4))
            END IF
!
            RHSFC   = QSFC/QSAT
            RHSFC   = AMAX1(0.01,RHSFC)
            RHSFC   = AMIN1(RHSFC,1.0)
            RHFRZ(I,J)= RHSFC
!     
!        OTHERWISE, LOCATE THE FREEZING LEVEL ALOFT.
!
         ELSE IF (LICE.LT.LLMH) THEN
                  L=LICE
                  DELZ = D50*(ZINT(I,J,L)-ZINT(I,J,L+2))
                  ZL   = D50*(ZINT(I,J,L+1)+ZINT(I,J,L+2))
                  DELT = T(I,J,L)-T(I,J,L+1)
                  ZFRZ(I,J) = ZL+(TFRZ-T(I,J,L+1))/DELT*DELZ
!     
                  DZABV = ZFRZ(I,J)-ZL
                  DELQ  = Q(I,J,L)-Q(I,J,L+1)
                  QFRZ  = Q(I,J,L+1) + DELQ/DELZ*DZABV
                  QFRZ  = AMAX1(0.0,QFRZ)
!     
                  ALPL   = ALPINT(I,J,L+2)
                  ALPH   = ALPINT(I,J,L)
                  DELALP = ALPH - ALPL
                  DELZP  = ZINT(I,J,L)-ZINT(I,J,L+2)
                  DZFR   = ZFRZ(I,J) - ZINT(I,J,L+2)
                  ALPFRZ = ALPL + DELALP/DELZP*DZFR
                  PFRZ   = EXP(ALPFRZ)
                  PFRZL(I,J) = PFRZ
                  IF(MODELNAME == 'GFS'.OR.MODELNAME == 'RAPR')THEN
                    ES=FPVSNEW(TFRZ)
                    ES=MIN(ES,PFRZ)
                    QSFRZ=CON_EPS*ES/(PFRZ+CON_EPSM1*ES)
                  ELSE
                    QSFRZ=PQ0/PFRZ  & 
                     *EXP(A2*(TFRZ-A3)/(TFRZ-A4))
                  END IF
!                  QSFRZ  = PQ0/PFRZ
!     
                  RHZ      = QFRZ/QSFRZ
                  RHZ      = AMAX1(0.01,RHZ)
                  RHZ      = AMIN1(RHZ,1.0)
                  RHFRZ(I,J) = RHZ
!     
         ELSE
                  L=LICE
                  ZU      = ZMID(I,J,L)
                  ZL      = HTSFC+2.0
                  DELZ    = ZU-ZL
                  IF(TSHLTR(I,J)/=SPVAL .AND. PSHLTR(I,J)/=SPVAL)THEN
                   TSFC=TSHLTR(I,J)*(PSHLTR(I,J)*1.E-5)**CAPA
                  ELSE
! GFS analysis does not have flux file to retrieve TSFC from
                   TSFC=T(I,J,LM)+D0065*(ZMID(I,J,LM)-HTSFC-2.0)
                  END IF
                  DELT    = T(I,J,L)-TSFC
                  ZFRZ(I,J) = ZL + (TFRZ-TSFC)/DELT*DELZ
!     
                  DZABV   = ZFRZ(I,J)-ZL
! GFS does not output QS
                  IF(QSHLTR(I,J)/=SPVAL)THEN
                   QSFC=QSHLTR(I,J)
                  ELSE
                   QSFC=Q(I,J,LM)
                  END IF
                  DELQ    = Q(I,J,L)-QSFC
                  QFRZ    = QSFC + DELQ/DELZ*DZABV
                  QFRZ    = AMAX1(0.0,QFRZ)
!     
                  ALPH    = ALPINT(I,J,L)
                  ALPL    = ALOG(PSFC)
                  DELALP  = ALPH-ALPL
                  ALPFRZ  = ALPL + DELALP/DELZ*DZABV
                  PFRZ    = EXP(ALPFRZ)
                  PFRZL(I,J) = PFRZ
                  IF(MODELNAME == 'GFS'.OR.MODELNAME == 'RAPR')THEN
                    ES=FPVSNEW(TFRZ)
                    ES=MIN(ES,PFRZ)
                    QSFRZ=CON_EPS*ES/(PFRZ+CON_EPSM1*ES)
                  ELSE
                    QSFRZ=PQ0/PFRZ  &
                     *EXP(A2*(TFRZ-A3)/(TFRZ-A4))
                  END IF
!
                  RHZ     = QFRZ/QSFRZ
                  RHZ     = AMAX1(0.01,RHZ)
                  RHZ     = AMIN1(RHZ,1.0)
                  RHFRZ(I,J)= RHZ
         ENDIF
!     
!              BOUND FREEZING LEVEL RH.  FREEZING LEVEL HEIGHT IS
!              MEASURED WITH RESPECT TO MEAN SEA LEVEL.
!
               RHFRZ(I,J) = AMAX1(0.01,RHFRZ(I,J))
               RHFRZ(I,J) = AMIN1(RHFRZ(I,J),1.00)
               ZFRZ(I,J)  = AMAX1(0.0,ZFRZ(I,J))
 20   CONTINUE
!     
!     END OF ROUTINE.
!     
      RETURN
      END
