C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    OUTPLT      OUTPUT THE SURFACE PLOT FILE 
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 97-01-10
C
C ABSTRACT: OUTPLT BUILDS THE OUTPUT PLOT FILE AND WRITES IT  
C
C PROGRAM HISTORY LOG:
C   97-01-10  LARRY SAGER
C 2015-01-27  BOI VUONG    Added If condition check to fix
C                          the array OAFOS has value 0 
C
C USAGE:    CALL OUTPLT  (IAFS, IREC, OHEDR, OTABL, KSTN, ITABL)           
C   INPUT ARGUMENT LIST:
C     IAFS     - OUTPUT UNIT NUMBER                 
C     IREC     - SPECIFIES THE NUMBER OF THE RECORD TO WRITE IN THE
C                NHPLOT FILE
C     OHEDR    - PLOTFILE HEADING ARRAY
C     OTABL    - DATA ARRAY
C     KSTN     - NUMBER OF REPORTS TO OUTPUT
C     ITABL    - THIN/NOT THINNED INFORMATION
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
      SUBROUTINE OUTPLT(IAFS, IREC, OHEDR, OTABL, ISTT, KSTN, ITABL)
C 
C     THIS ROUTINE OUTPUTS THE AFOS PLOTFILE
C
      CHARACTER*8   CWORK 
C
      INTEGER       ITABL(3,20000)
      INTEGER       ISTT(20000)
C
      CHARACTER*1   OAFOS(1280)     
      CHARACTER*1280 AFOS
      CHARACTER*1   OTABL(20000,150)
      CHARACTER*1   OHEDR(133)
      CHARACTER*1   OEOR
      CHARACTER*1   OEOB
      CHARACTER*1   OWORK(8)
C     
      EQUIVALENCE   (CWORK,OWORK)
      EQUIVALENCE   (IWORK,OWORK)
      EQUIVALENCE   (AFOS,OAFOS(1))
C 
      DATA          OAFOS  /1280*' '/
      DATA          IPOIN  /133/
      DATA          IEND   /Z'00454E44204F4620'/
      DATA          IENE   /Z'444154410D0AC500'/
      DATA          IENF   /Z'003205CE31830000'/
      DATA          IEOA   /Z'0D0A000000000000'/
C
C     OUTPUT THE AFOS SURFACE PLOT FILE.        
C
      DO K = 1,133
         OAFOS(K) = OHEDR(K)
      END DO
C
      IWORK = IEOA
      call byteswap(IWORK,8,1)
      ICNT = 0
C
      DO 10 K = 1,KSTN
C 
C        LOOP THROUGH THE STATIONS.  START BY SEEING
C        IF THIS STATION IS MARKED FOR THINNING
C
         IF (ITABL(3,K) .LT. 0) THEN   
C
C           THIS STATION IS MARKED FOR THINNING.  
C           DROP THIS STATION IF IT IS A LAND STATION.
C           IF IT IS A MARINE STATION, MOVE IT TO
C           THE MARGIN OF THE CHART AND SAVE INTO
C           THE PLOTFILE.
C           
	    IF(ISTT(K) .LE. 2) GOTO 10
            ICNT = ICNT +1
C
C           CHANGE THE PSOWDT WORD TO 7 TO INDICATE 
C             MARGIN PLOTTING
C
             ICOM = 0
            DO J = 1,150
               IF(OTABL(K,J) .EQ. ',') THEN
                  ICOM = ICOM + 1
                  IF (ICOM .EQ. 2) THEN
                     OTABL(K,J+1) = '7'
                     GOTO 8
                  END IF
               END IF
            END DO
	 END IF
  8	 ISWT = 0
	 J = 0
	 DO WHILE (ISWT .EQ. 0)
	       J = J + 1
	       IPOIN = IPOIN + 1
	       IF ( IPOIN .GT. 1280) THEN
C
C                 OUTPUT THE AFOS BLOCK
C
                  IREC = IREC + 1
		  WRITE ( IAFS, REC=IREC ) AFOS

		  IPOIN = 1
		  DO I = 1,1280
		     OAFOS(I) = ' '
		  END DO
	       END IF
C
C              LOAD THE OUTPUT FILE
C 
	       OAFOS(IPOIN) = OTABL(K,J)
	       IF (OTABL(K,J) .EQ. OWORK(2)) ISWT = 1
	       IF (J .GE. 100) THEN
c       	  OAFOS(IPOIN-2) = ';'
c	          OAFOS(IPOIN-1) = OWORK(2)

                  IF (IPOIN .GT. 2) OAFOS(IPOIN-2) = ';'
                  IF (IPOIN .GT. 1) OAFOS(IPOIN-1) = OWORK(2)

		  OAFOS(IPOIN) = OWORK(1)
		  ISWT = 1
	       END IF
	 END DO
 10   CONTINUE
C 
C     ADD THE END LABEL
C
      IWORK = IEND
      call byteswap(iwork,8,1)
      DO J = 1,8 
	 IPOIN = IPOIN + 1
	 IF(IPOIN .GT. 1280) THEN
C
C            OUTPUT THE AFOS BLOCK
C
             IREC = IREC + 1
             WRITE ( IAFS, REC=IREC ) AFOS

	     IPOIN = 1
	     DO I = 1,1280
		OAFOS(I) = ' '
	     END DO
	 END IF
	 OAFOS(IPOIN) = OWORK(J)
      END DO
      IWORK = IENE
      call byteswap(iwork,8,1)
      DO J = 1,8 
	 IPOIN = IPOIN + 1
	 IF(IPOIN .GT. 1280) THEN
C
C            OUTPUT THE AFOS BLOCK
C
             IREC = IREC + 1
             WRITE ( IAFS, REC=IREC ) AFOS
	     IPOIN = 1
	     DO I = 1,1280
		OAFOS(I) = ' '
	     END DO
	 END IF
	 OAFOS(IPOIN) = OWORK(J)
      END DO
      IWORK = IENF
      call byteswap(iwork,8,1)
      DO J = 1,7 
	 IPOIN = IPOIN + 1
	 IF(IPOIN .GT. 1280) THEN
C
C            OUTPUT THE AFOS BLOCK
C
             IREC = IREC + 1
             WRITE ( IAFS, REC=IREC ) AFOS
	     IPOIN = 1
	     DO I = 1,1280
		OAFOS(I) = ' '
	     END DO
	 END IF
	 OAFOS(IPOIN) = OWORK(J)
      END DO
      IF ( IPOIN .NE. 1 ) THEN
          IREC = IREC + 1
          WRITE ( IAFS, REC=IREC ) AFOS
      END IF
C
      RETURN
      END
