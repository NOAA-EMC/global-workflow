      SUBROUTINE STRPTITL(CHWORK,PACKED)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    COMIFID      MAKE STRIP TITLES AND PUT THEM TO MAP
C   PRGMMR: KRISHNA KUMAR         ORG: W/NP12     DATE: 1999-08-01
C
C ABSTRACT: MAKE STRIP TITLES AND CALL PUTLAB TO PUT THEM TO TAPE 55.
C
C PROGRAM HISTORY LOG:
C   96-07-01  LUKE LIN
C   96-11-07  LUKE LIN  ADD BASE TIME FOR PFAX1 FAX CHARTS.
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM RS/6000.
C
C USAGE:    CALL STRPTITL
C   INPUT ARGUMENT LIST:
C     CHWORK   - CHARACTER*12  WORD ARRAY THAT CONTAINS THREE
C              - ID WORDS AFTER GETING A PACKED DATA FIELD.
C              - CHWORK(1:4) HAS IDWORD(1).
C              - CHWORK(5:8) HAS IDWORD(7).
C              - CHWORK(9:12) HAS IDWORD(8).
C              - IN ON 84 FORMAT.
C     PACKED   - LOGICAL FLAG SET = .TRUE. ITIMES(1) IS A PACKED ID
C              - WORD THEN THE TAU IS IN THE 4TH BYTE. IF = .FALSE.
C              - THEN THE TAU IS IN ALL OF ITIMES(1).
C
C   OUTPUT ARGUMENT LIST:
C
C   OUTPUT FILES:
C     FT06F001 - MESSAGE FOR STRPTITL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM
C
C$$$
C
C
C
      COMMON /LABG/ GULPXX(2),GULPXY(2),LVFLG,NUMG,GLAB
     1
      INTEGER       GULPXX,GULPXY,LVFLG,NUMG
      CHARACTER*24  GLAB
C
C ..........   FOR FAX CUT
C
      COMMON /FAXICK/ ISCHED(8,50),INXISCHED
      COMMON /FLFSRP/ INXLFSTRP,LSNUMBYT(20),LSINTEXT(20),LSTITPX(20),
     1                LSTITPY(20),LSDCKPX(20),LSDCKPY(20),
     2                LSDCKOPT(20),LSFONT(20)
      CHARACTER*120   LSINTEXT
      REAL            LSFONT
      COMMON /FRTSRP/ INXRTSTRP,RSNUMBYT(20),RSINTEXT(20),RSTITPX(20),
     1                RSTITPY(20),RSFONT(20),RSFCSTHR(20)
      CHARACTER*120   RSINTEXT
      INTEGER         RSNUMBYT,RSTITPX,RSTITPY,RSFCSTHR
      REAL            RSFONT
C
      COMMON/PACKRA/JTIME(2),IVLDHR,IVLDAY,IVLDMO,IVLDYR,
     1              IHOUR,IDAY,MONTH,IYR
C
C
      INTEGER         IPRPUT(2),ITAG
      REAL            PUTANG
      INTEGER         NUMT
C
      CHARACTER*12 CHWORK
C
      CHARACTER*28 VALID
      CHARACTER*28 BASE 
      CHARACTER*4  MONS (12)
      CHARACTER*1  IZ
      CHARACTER*8 CHTEXT
      INTEGER     KTIME
      LOGICAL     PACKED
      CHARACTER*4  IHDAYW
C
      DATA         VALID     /'VALID                       '/
      DATA         MONS      /'JAN ','FEB ','MAR ','APR ','MAY ',
     1                        'JUN ','JUL ','AUG ','SEP ','OCT ',
     2                        'NOV ','DEC '/
      DATA         IZ        /'Z'/

C
C------------------------------------------------------------------
C
C
C     PRINT *,' STRPTITL STARTING!'
C     PRINT *,' INXLFSTRP=', INXLFSTRP
C     PRINT *,' INXRTSTRP=', INXRTSTRP
C
      IF (INXLFSTRP .GT. 0) THEN
C        PRINT *,' INXLFSTRP=', INXLFSTRP
C
C        .... MAKE VALID TIME ....
         IF(PACKED) THEN
            ITAU  = mova2i(CHWORK(4:4))
         ELSE
#if defined (_LITTLE_ENDIAN)
            CALL SWAP_BYTES_ON_COPY (ISCRATCH, CHWORK, 8_8, 1_8)
            CALL GBYTE (ISCRATCH, KTIME, 0, 32)
#else
            CALL GBYTE(CHWORK,KTIME,0,32)
#endif
            ITAU = KTIME
         ENDIF
         IYEAR   = mova2i(CHWORK(5:5))
         MONTH   = mova2i(CHWORK(6:6))
         IDAY    = mova2i(CHWORK(7:7))
         IHOUR   = mova2i(CHWORK(8:8))
         IGEN    = mova2i(CHWORK(10:10))
         JTIME(1)= ITAU
         ICORYR  = 1900
         IF(IYEAR.LT.96)ICORYR  = 2000
         IYR = IYEAR +  ICORYR
C
          WRITE(6,FMT='('' STRPTITL: CALLING UPDATR WITH: TAU ='',I4,
     1  '' HOUR='',I3,'' DAY='',I3,'' MONTH='',I3,'' YEAR='',I5)')
     2   JTIME(1),IHOUR,IDAY,MONTH,IYR
C
         CALL UPDATR
C
C        CONSTRUCT VALID TIME ARRAY IE. VALID 00Z THU 16 MAR 1989
C
         CALL DAYOWK(IVLDAY,IVLDMO,IVLDYR,IDAYWK,IHDAYW)
C
         CALL BIN2EB(IVLDHR,CHTEXT,2,'A999')
C
         VALID(7:8) = CHTEXT(1:2)
         VALID(9:9) = IZ
         VALID(11:13) = IHDAYW(1:4)
         CALL BIN2EB(IVLDAY,CHTEXT,2,'A999')
         VALID(15:16) = CHTEXT(1:2)
         VALID(18:21) = MONS(IVLDMO)(1:4)
         CALL BIN2EB(IVLDYR,CHTEXT,4,'A999')
         VALID(22:25) = CHTEXT(1:4)
            WRITE(6,FMT='('' VALID='',A)') VALID(1:28)
C
         PUTANG = 0.0
         IPRPUT(1) = 0
         IPRPUT(2) = 0
         ITAG = 0
         DO I = 1, INXLFSTRP
            IF (LSTITPX(I).GT.0 .AND. LSTITPY(I).GT.0) THEN
C              .... PUT LEFT STRIP TITLE
               NUMT = LSNUMBYT(I)
               IBEGIN = NUMT + 1
               ISTOP = IBEGIN + 25 - 1
               LSINTEXT(I)(IBEGIN:ISTOP) = VALID(1:25)
C              ... ADD VALID TIME INTO LEFT STRIP TITLE
               NUMT = NUMT + 25
               PUTHGT = LSFONT(I)
               IX = LSTITPX(I)
               IY = LSTITPY(I)
C              PRINT *,' IX=',IX,' IY=',IY,' NUMT=',NUMT
               IF ((GULPXX(2).EQ.-49 .AND. GULPXY(2).EQ.-49) .OR.
     $         (GULPXX(2).EQ.-48 .AND. GULPXY(2).EQ.-48)) THEN
                   LSINTEXT(I)(NUMT+1:NUMT+10)=' BASED ON '
                   NUMT = NUMT+11
                   CALL BIN2EB(IHOUR,CHTEXT,2,'A999')
                   BASE(1:2)= CHTEXT(1:2)
                   BASE(3:3) = IZ
                   BASE(4:4) = '/'
                   CALL BIN2EB(IDAY,CHTEXT,2,'A999')
                   BASE(5:6)= CHTEXT(1:2)
                   BASE(7:7)= '.'
                   LSINTEXT(I)(NUMT:NUMT+6)=BASE(1:7)
                   NUMT = NUMT+7
               ENDIF
               PRINT *,' LEFT STRIP TITLE=',LSINTEXT(I)
C
               CALL PUTLAB(IX,IY,PUTHGT,LSINTEXT(I),PUTANG,NUMT,
     1                     IPRPUT,ITAG)
            ENDIF
C
            IF (LSDCKPX(I).GT.0 .AND. LSDCKPY(I).GT.0) THEN
C              ..... PUT DUCK IF ANY
               IX = LSDCKPX(I)
               IY = LSDCKPY(I)
               KANG = 0
               KFLAG = LSDCKOPT(I)
               CALL DUCK(IX,IY,KANG,KFLAG)
            ENDIF
         ENDDO
      ENDIF
C
      IF (INXRTSTRP .GT. 0) THEN
C        PRINT *,' INXRTSTRP=', INXRTSTRP
C        .... MAKE VALID TIME ....
         IF(PACKED) THEN
            ITAU  = mova2i(CHWORK(4:4))
         ELSE
            CALL GBYTE(CHWORK,KTIME,0,32)
            ITAU = KTIME
         ENDIF
C
         PUTANG = 0.0
         IPRPUT(1) = 0
         IPRPUT(2) = 0
         ITAG = 0
         DO I = 1, INXRTSTRP
C
            IF (RSFCSTHR(I) .NE. ITAU) THEN
               ITAU = RSFCSTHR(I)
C              ... REMAKE VALID TIME
               IYEAR   = mova2i(CHWORK(5:5))
               MONTH   = mova2i(CHWORK(6:6))
               IDAY    = mova2i(CHWORK(7:7))
               IHOUR   = mova2i(CHWORK(8:8))
               IGEN    = mova2i(CHWORK(10:10))
               JTIME(1)= ITAU
               ICORYR  = 1900
               IF(IYEAR.LT.96)ICORYR  = 2000
               IYR = IYEAR +  ICORYR
C
          WRITE(6,FMT='('' GULFTL: CALLING UPDATR WITH: TAU ='',I4,
     1  '' HOUR='',I3,'' DAY='',I3,'' MONTH='',I3,'' YEAR='',I5)')
     2   JTIME(1),IHOUR,IDAY,MONTH,IYR
C
               CALL UPDATR
C
C              CONSTRUCT VALID TIME ARRAY IE. VALID 00Z THU 16 MAR 1989
C
               CALL DAYOWK(IVLDAY,IVLDMO,IVLDYR,IDAYWK,IHDAYW)
C
               CALL BIN2EB(IVLDHR,CHTEXT,2,'A999')
C
               VALID(7:8) = CHTEXT(1:2)
               VALID(9:9) = IZ
               VALID(11:13) = IHDAYW(1:4)
               CALL BIN2EB(IVLDAY,CHTEXT,2,'A999')
               VALID(15:16) = CHTEXT(1:2)
               VALID(18:21) = MONS(IVLDMO)(1:4)
               CALL BIN2EB(IVLDYR,CHTEXT,4,'A999')
               VALID(22:25) = CHTEXT(1:4)
                  WRITE(6,FMT='('' VALID='',A)') VALID(1:28)
C
            ENDIF
            IF (RSTITPX(I).GT.0 .AND. RSTITPY(I).GT.0) THEN
C              .... PUT RIGHT STRIP TITLE
               NUMT = RSNUMBYT(I)
               IBEGIN = NUMT + 1
               ISTOP = IBEGIN + 25 - 1
               RSINTEXT(I)(IBEGIN:ISTOP) = VALID(1:25)
C              ... ADD VALID TIME INTO LEFT STRIP TITLE
               NUMT = NUMT + 25
               PUTHGT = RSFONT(I)
               IX = RSTITPX(I)
               IY = RSTITPY(I)
C              PRINT *,' IX=',IX,' IY=',IY,' NUMT=',NUMT
               IF ((GULPXX(2).EQ.-49 .AND. GULPXY(2).EQ.-49) .OR.
     $         (GULPXX(2).EQ.-48 .AND. GULPXY(2).EQ.-48)) THEN
                   LSINTEXT(I)(NUMT+1:NUMT+10)=' BASED ON '
                   NUMT = NUMT+11
                   CALL BIN2EB(IHOUR,CHTEXT,2,'A999')
                   BASE(1:2)= CHTEXT(1:2)
                   BASE(3:3) = IZ
                   BASE(4:4) = '/'
                   CALL BIN2EB(IDAY,CHTEXT,2,'A999')
                   BASE(5:6)= CHTEXT(1:2)
                   BASE(7:7)= '.'
                   LSINTEXT(I)(NUMT:NUMT+6)=BASE(1:7)
                   NUMT = NUMT+7
               ENDIF
               PRINT *,' RIGHT STRIP TITLE=',RSINTEXT(I)
               CALL PUTLAB(IX,IY,PUTHGT,RSINTEXT(I),PUTANG,NUMT,
     1                     IPRPUT,ITAG)
            ENDIF
         ENDDO
      ENDIF
C
      RETURN
      END
