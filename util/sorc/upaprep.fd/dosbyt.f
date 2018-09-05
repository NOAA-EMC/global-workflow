
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    DOSBYT      PACK AND WRITE A BLOCK    
C   PRGMMR: LARRY SAGER      ORG: W/NMC41    DATE: 96-08-27
C
C ABSTRACT: DOSBYT PACKS A REPORT INTO THE OUTPUT BLOCK      
C   USING SBYTES.  WHEN THE BLOCK IS FULL, IT OUTPUTS IT.  
C
C PROGRAM HISTORY LOG:
C   96-08-27  LARRY SAGER
C
C USAGE:    CALL DOSBYT(IARR,KRET,IUNO,COUT,KNEXT,IREPTS,IENT)
C   INPUT ARGUMENT LIST:
C     IARR     - REPORT                                             
C     KRET     - 12HR OLD MANDATORY LEVEL DATA
C     IUNO     - OUTPUT UNIT NUMBER          
C     COUT     - OUTPUT BLOCK
C     KNEXT    - POINTER TO NEXT LOCATION
C     IENT     - SWITCH  IENT=1  NORMAL
C                        IENT=2  END; OUTPUT LAST BLOCK
C                .      .    .                                       .
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     IREPTS   - NUMBER OF REPORTS             
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY4
C
C$$$
C
      SUBROUTINE DOSBYT(IARR, KRET, IUNO, COUT, KNEXT, IREPTS, IENT)
C
      CHARACTER*8   COUT (512)
      CHARACTER*8   CEND, CSTR, CBLK, CENDFL  
      INTEGER NEXT5

C
      INTEGER*8     IARR (300)
C
C      DATA CEND    /'END_REPO'/
C      DATA CSTR    /'STR_REPO'/
      DATA CEND    /'OPER_DNE'/
      DATA CSTR    /'OPER_RTS'/
      DATA CBLK    /'        '/
C      DATA CENDFL  /'ENDOFILE'/
      DATA CENDFL  /'ELIFODNE'/
      DATA KLIM    /512/
      DATA SHFTL   /4096/

C
C     THIS SUBROUTINE USES SBYTES TO FORM THE OUTPUT.
C     THE BLOCK IS WRITTEN WHEN FULL.
C  
C     IF IENT = 1   -   LOAD REPORT INTO BLOCK
C        IENT = 2   -   END CONDITION. WRITE LAST BLOCK.
C
      IF (IENT .EQ. 1) THEN
         IREPTS = IREPTS + 1 
         INEXT = KNEXT + KRET/2
         COUT(KNEXT) = CSTR
         IARR(3) = (INEXT*2 + 1)*SHFTL
         KNEXT = KNEXT + 1
C        PRINT 100,IARR(11),IARR(12),INEXT
 100     FORMAT(' STATION ',a8,a8,' INEXT IS ',i8)
         IF (INEXT .GE. KLIM) THEN
            INEXT = INEXT - KLIM
            IARR(3) = (INEXT*2 + 1)*SHFTL
            IST = 2*(KLIM - KNEXT + 1)
            CALL SBYTESCCS(COUT(KNEXT), IARR, 0, 32, 0, IST)
            NEXT5 = KNEXT + 5
            call byteswap(COUT(NEXT5), 8, 1)
            if (COUT(2)(1:6) .EQ. "UPAUPA" ) THEN
                call byteswap(COUT(1), 8, 39)
                call byteswap(COUT(5), 8, 1) 
            end if 
            if (COUT(2)(1:6) .EQ. "AIRCFT" ) THEN
                call byteswap(COUT(1), 8, 17)
                call byteswap(COUT(5), 8, 1)
            end if
            if (COUT(2)(1:6) .EQ. "SATWND" ) THEN
                call byteswap(COUT(1), 8, 17)
                call byteswap(COUT(5), 8, 1)
            end if
            call byteswap(COUT(1), 8, 512)
            WRITE(IUNO) COUT
            if (NEXT5 . gt.  KLIM) THEN
                NEXT5 = MOD( NEXT5, KLIM)
            end if
            KRET = KRET - IST 
            DO KK = 1,KLIM
               COUT(KK) = CBLK
            END DO
            KNEXT = 1
            IF (KRET .NE. 0) THEN
               CALL SBYTESCCS(COUT, IARR(IST+1), 0, 32, 0, KRET)
               call byteswap(COUT(NEXT5), 8, 1)
               KNEXT = KRET/2 + 1
            END IF
            COUT(KNEXT) = CEND
            KNEXT = KNEXT + 1
          ELSE 
            CALL SBYTESCCS(COUT(KNEXT), IARR, 0, 32, 0, KRET)
            NEXT5 = KNEXT + 5
            call byteswap(COUT(NEXT5), 8, 1)
            KNEXT = KNEXT + KRET/2
            IF (KNEXT .GT. KLIM) THEN
                call byteswap(COUT(1), 8, 512)
  		WRITE(IUNO) COUT
		KNEXT = 1
		DO KK = 1,KLIM
		   COUT(KK) = CBLK
		END DO
	    END IF
            COUT(KNEXT) = CEND
            KNEXT = KNEXT + 1
            IF (KNEXT .GT. KLIM) THEN
               call byteswap(COUT(1), 8, 512)
               WRITE(IUNO) COUT
               KNEXT = 1
               DO KK = 1,KLIM
                  COUT(KK) = CBLK
               END DO
            END IF
          END IF
       ELSE
C
C         OUTPUT THE LAST BLOCK OF DATA
C
          IF (KNEXT.NE.1) THEN
             COUT(KNEXT) = CENDFL
            do i = 1, 512
                if (COUT(i) .EQ. CSTR) then
C                   call byteswap(COUT(i+6), 8, 1)
                end if
            end do
             call byteswap(COUT(1), 8, 512)
             WRITE (IUNO) COUT
          ELSE
C
C            OUTPUT AN END-OF-FILE RECORD, FOR THE CASE
C            WHEN THERE IS NO DATA IN THE LAST BLOCK EXCEPT
C            THE END OF FILE INDICATOR
C
             COUT(1) = CENDFL
             DO  K=1,KLIM
                COUT(K) = CBLK
             END DO
             call byteswap(COUT(1), 8, 512)
             WRITE(IUNO) COUT
          END IF
       END IF
       RETURN
C
       END
