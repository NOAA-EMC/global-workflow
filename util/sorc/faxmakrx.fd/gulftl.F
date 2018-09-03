      SUBROUTINE GULFTL(CHWORK,CHTYPE,PACKED,AFOS,LGULF)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GULFTL      FORMS TEXT OF GULF TITLE AND OTHER TITLES
C   PRGMMR: KRISHNA KUMAR      ORG: W/NP12     DATE: 1999-08-01
C
C ABSTRACT:  FORMS THE TEXT OF THE GULF TITLE FOR VARIAN/FAX MAPS AND
C   THE BASE AND VALID TIME TEXT FOR OTHER MAPS. ALL TEXT IS RETURNED
C   IN LGULF ARRAY. FOR FURTHER DETAILS SEE OUTOUT ARGS. BELOW.
C
C PROGRAM HISTORY LOG:
C   89-02-08  ORIGINAL AUTHOR HENRICHSEN
C   95-01-04  LUKE LIN      CONVERT IT CFT-77.
C   95-01-18  LUKE LIN      GET ITAU FROM SUBROUTINE RD1GRIB
C   96-06-03  HENRICHSEN    REMOVED THE 95-01-18 MODIFICATIONS AND 
C                           CORRECTED AN ERROR IN CALL TO "GBYTES" IT 
C                           SHOULD HAVE BEEN A CALL TO "GBYTE".
C   96-06-18  HENRICHSEN    ADDED NAM48 TO MODTAB 
C   96-08-21  HENRICHSEN    ADDED CHANGED GENERATING MODEL # 77 FROM JMET
C                           TO SA126 WHICH SIGNIFIES 126 WAVE TRIANGULAR,
C                           28 LAYER SPECTRAL MODEL FROM "AVIATION" RUN.
C   97-03-11  LIN           MODIFY FOR NEGATIVE TAU TEST AND GFS D+/D- MAPS.
C   97-03-31  LIN           CHANGE GFSBC TO GFS FOR IGEN .EQ. 76.
C   97-04-18  LIN           FIX ECMWF D+3/D+8 MAP TITLES. 
C   98-02-11  LILLY         ADDED NAM32 TO MODTAB
C   98-06-03  FACEY         REPLACE CALL TO W3FS15 WITH CALL TO W3MOVDAT
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM
C                           RS/6000. 
C
C USAGE:    CALL GULFTL(CHWORK,CHTYPE,PACKED,AFOS,LGULF)
C   INPUT ARGUMENT LIST:
C     CHWORK   - CHARACTER*12  WORD ARRAY THAT CONTAINS THREE
C              - ID WORDS AFTER GETING A PACKED DATA FIELD.
C              - CHWORK(1:4) HAS IDWORD(1).
C              - CHWORK(5:8) HAS IDWORD(7).
C              - CHWORK(9:12) HAS IDWORD(8).
C              - IN ON 84 FORMAT.
C     CHTYPE   - CHARACTER*4 WORD THAT CONTAINS THE TYPE OF FIELD IN
C              - HOLLERTH IE 'HGT ', 'TEMP', 'PRS '
C     PACKED   - LOGICAL FLAG SET = .TRUE. ITIMES(1) IS A PACKED ID
C              - WORD THEN THE TAU IS IN THE 4TH BYTE. IF = .FALSE.
C              - THEN THE TAU IS IN ALL OF ITIMES(1).
C     AFOS     - LOGICAL FLAG SET = .TRUE. IF AFOS PRODUCT THEN
C              - 16 EXTRA BYTES ARE ADDED AT THE END OF LGULF ARRAY
C              - CONTAINING: MM/DD/HHZ   YYYY  WHICH ARE THE
C              - BASE TIME OF THE AFOS GRAPHIC.
C
C   OUTPUT ARGUMENT LIST:
C     LGULF    -  CHARACTER*1 77 WORD ARRAY CONTAINING GULF TITLE.
C              -  BYTES 1 THRU 16 CONTAIN THE GULF TITLE, IE
C              -  HGT12 00Z05OPTIN     .
C              -  BYTES 17 THRU 36 CONTAIN THE BASE TIME TITLE, IE
C              -  12Z MON 03 JUN 1996  .
C              -  BYTES 37 THRU 61 CONTAIN THE VALID TIME TITLE, IE
C              -  VALID 00Z WED 05 JUN 1996  .
C              -  BYTES 62 THRU 77 CONTAIN MM/DD/HHZ  YYYY USED
C              -  WITH AFOS PRODUCTS, IE
C              -  06/03/12Z  1996  .
C
C   OUTPUT FILES:
C     FT06F001 - STANDARD PRINT FILE.
C
C REMARKS: IF PACKED = .FALSE. THEN TAU IS IN ALL OF ITIMES(1).
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
      COMMON/PACKRA/JTIME(2),IVLDHR,IVLDAY,IVLDMO,IVLDYR,
     1              IHOUR,IDAY,MONTH,IYR
C
C
      COMMON /MEANCON/ MEANFG,LKMEAN,MEANTYP
C
      CHARACTER*4 MEANTYP
C
      INTEGER    MEANFG
      LOGICAL    LKMEAN
C
      CHARACTER*77 LGULF
      CHARACTER*50 BLANKS
      CHARACTER*28 VALID
      CHARACTER*20 FROM
      CHARACTER*16 ATIME
      CHARACTER*12 CHWORK
      CHARACTER*8 CHTEXT
      CHARACTER*5  MISSG
      CHARACTER*5  MODTAB(47)
      CHARACTER*4  CHTYPE
      CHARACTER*4  IHDAYW
      CHARACTER*4  MONS (12)
      CHARACTER*1  IZ
      CHARACTER*4  LTIME,LDATE
C
C
      INTEGER     IYEAR, MONTH, IDAY 
      INTEGER     JDAT(8), IDAT(8) 
      REAL        RINC(5)
      INTEGER     KTIME
      INTEGER     MAXMOD
      INTEGER     MODNUM(47)
C
      INTEGER     IDT(8)
      INTEGER     MSKFF
      INTEGER     MSKTIME
      DATA        MSKTIME /Z'FFFFFFFF00000000'/
      INTEGER     TIMEFG
      DATA        TIMEFG /Z'FFFF0000'/
C
      LOGICAL     AFOS
      LOGICAL     PACKED
C
      DATA        BLANKS
     1 /'                                                  '/
C       '12345678901234567890123456789012345678901234567890'
      DATA         VALID     /'VALID                       '/
      DATA         FROM      /'                    '/
C                             '12345678901234567890'
      DATA         ATIME     /'  /  /  Z       '/
C                             '1234567890123456'
      DATA         MISSG     /'GM   '/
      DATA         MODTAB
     1            /'BATRO','NOWAV','TRAJ ',
     2             'SCM  ','HOUGH','SNOW ','NGM  ',
     3             'GOI  ','NOW  ','SMG2C','SMG3C','TOVS ','LFM  ',
     4             'NMSFC','NHSFC','ECMWF','FNOC ','AFGWC','CRGN ',
     5             'WRGN ','ERGN ','SMG4 ','ROI  ','SMG4I','UKMET',
     6             'SCLR ','HCN  ','SMT80','QLM  ','SBLND','ISENA',
     7             'SFVSB','GMEXW','GAKWV','GFS  ','SA126','SM126',
     8             'BCKUP','SMT62','SSI  ','NAM80','NAM32','NAM30',
     9             'RUC80','ENSMB','PWAV ','NAM  '/
      DATA         MONS      /'JAN ','FEB ','MAR ','APR ','MAY ',
     1                        'JUN ','JUL ','AUG ','SEP ','OCT ',
     2                        'NOV ','DEC '/
      DATA         IZ        /'Z'/
      DATA        MAXMOD     /47/
      DATA        MODNUM     /01,10,18,19,22,25,39,43,44,45,46,49,53,
     1                        55,56,57,58,59,60,61,62,63,64,65,66,67,
     2                        68,69,70,71,72,73,74,75,76,77,78,79,80,
     3                        81,83,84,85,86,87,88,89/
C
         print*,'In Gulftitle    *******'
         write(06,1111)chwork 
 1111    format('chwork= ',Z24)
         print*,'packed   ',packed
C
         LGULF(1:50)  = BLANKS(1:50)
         LGULF(51:77) = BLANKS(1:27)
            IF(PACKED) THEN
               ITAU  = mova2i(CHWORK(4:4))  !kumar
            ELSE
#if defined (_LITTLE_ENDIAN)
      CALL SWAP_BYTES_ON_COPY (ISCRATCH, CHWORK, 8_8, 1_8)
               CALL GBYTE(ISCRATCH,KTIME,0,32)
#else
               CALL GBYTE(CHWORK,KTIME,0,32)
#endif
               ITAU = KTIME
C      WRITE (0,*) 'FPDebug: ITAU =', ITAU
            ENDIF
C      WRITE (0,*) 'FPDebug: TIMEFG =', TIMEFG
         IF (ITAU .GE. TIMEFG) THEN
C            ... TEST FOR NEGATIVE NUMBER
             ITAU = IOR(ITAU,MSKTIME)
         ENDIF
C
         print*,'In Gulftitl  meanfg  ',meanfg
         IF (MEANFG.EQ.9) THEN
            LTIME(1:4) = CHWORK(5:8)
            IDAT=0
            JDAT=0
            RINC=0
            RINC(2)=36
            ICENT=1900
            IDAT(1)=mova2i(LTIME(1:1))
            IF(IDAT(1) .LT. 96) ICENT=2000
            IDAT(1)=IDAT(1)+ICENT
            IDAT(2)=mova2i(LTIME(2:2))
            IDAT(3)=mova2i(LTIME(3:3))
            IDAT(5)=mova2i(LTIME(4:4))
            write(06,1112)ltime
 1112       format('ltime  =  ',Z4)
            CALL W3MOVDAT(RINC,IDAT,JDAT)
            IYEAR   = JDAT(1)
            MONTH   = JDAT(2)
            IDAY    = JDAT(3)
            IHOUR   = JDAT(5)
         ELSE
            IYEAR   = mova2i(CHWORK(5:5))
            ICENT = 1900
            IF (IYEAR .LT. 96) ICENT=2000
            IYEAR = ICENT + IYEAR
            MONTH   = mova2i(CHWORK(6:6))
            IDAY    = mova2i(CHWORK(7:7))
            IHOUR   = mova2i(CHWORK(8:8))
         ENDIF
         
         IGEN    = mova2i(CHWORK(10:10))
C
         IF (IGEN .EQ. 74) IGEN = 66
C         ..... FOR UKMET
         IF (LKMEAN .AND. MEANFG.NE.9) THEN
            REWIND 49
            READ(49,FMT='(6X,I4,2I2)') IYEAR, MONTH, IDAY 
C            print *,' idt=',(idt(i),i=1,8)
            IHOUR = 0
            IF (MEANFG.EQ.2.OR.MEANFG.EQ.4) IHOUR = 12
            IGEN    = 76
            IF (MEANFG.EQ.3.OR.MEANFG.EQ.4) IGEN = 71 
         ENDIF
         IF (LKMEAN .AND. MEANFG.EQ.9) IGEN = 76

         PRINT *, ' IGEN=', IGEN
         JTIME(1)= ITAU
         PRINT *, 'ICENT=', ICENT
         IYR = IYEAR 
C
          WRITE(6,FMT='('' GULFTL: CALLING UPDATR WITH: TAU ='',I4,
     1  '' HOUR='',I3,'' DAY='',I3,'' MONTH='',I3,'' YEAR='',I5)')
     2   JTIME(1),IHOUR,IDAY,MONTH,IYR
C
        CALL UPDATR
C
C       MOVE CHTYPE INTO LGULF
C
        LGULF(1:3) = CHTYPE(1:3)
C      WRITE (0,*) 'FPDebug: LGULF(1:3) =', LGULF(1:3)
C
        NUM = 2
        IF(ITAU.GE.100) NUM = 3
C
        CALL BIN2EB(ITAU,CHTEXT,NUM,'A999')
C
C       MOVE TAU INTO LGULF
C
        IEND = 4 + (NUM - 1)
        LGULF(4:IEND) = CHTEXT(1:NUM)
C      WRITE (0,*) 'FPDebug: LGULF(4:IEND) =', LGULF(4:IEND)
C
        CALL BIN2EB(IVLDHR,CHTEXT,2,'A999')
C
        LGULF(7:8) = CHTEXT(1:2)
C      WRITE (0,*) 'FPDebug: LGULF(7:8) =', LGULF(7:8)
C
C       MOVE VALID HR INTO LGULF
C
        LGULF(9:9) = IZ
        CALL BIN2EB(IVLDAY,CHTEXT,2,'A999')
C      WRITE (0,*) 'FPDebug: LGULF(9:9) =', LGULF(9:9)
C
C       MOVE VALID DAY INTO LGULF
C
        LGULF(10:11) = CHTEXT(1:2)
C
            DO  40 J = 1,MAXMOD
C
                     IF(MODNUM(J).EQ.IGEN) THEN
C
C                      LOAD GENERATING MODEL NAME INTO LGULF
C
                       LGULF(12:16) = MODTAB(J)(1:5)
C      WRITE (0,*) 'FPDebug: LGULF(12:16) =', LGULF(12:16)
                        GO TO 70
                      ELSE
                      ENDIF
  40        CONTINUE
C
C      CLOUDN'T FIND GENNERATING MODEL IN MODTAB SO CONVERT MODEL
C      NUMBER TO ASCII AND LOAD INTO MISSG
C
        IF (IGEN.EQ.82) THEN
           MISSG(1:3) = 'SSI'
C          .... THIS QUICK FIX FOR FINAL 82
        ENDIF
C
        CALL BIN2EB(IGEN,CHTEXT,2,'A999')
        MISSG(4:5) = CHTEXT(1:2)
C
        LGULF(12:16) = MISSG(1:5)
C
C
C
  70    CONTINUE
C
            WRITE(6,FMT='('' LGULF='',A)') LGULF(1:16)
C
C      NOW BUILD THE BASE TITLE LINE IE. 12Z MON 03 JUN 1996
C
         CALL DAYOWK(IDAY,MONTH,IYR,IDAYWK,IHDAYW)
C
C
        CALL BIN2EB(IHOUR,CHTEXT,2,'A999')
C
C
C       MOVE BASE HR INTO FROM
C
        FROM(1:2) = CHTEXT(1:2)
        FROM(3:3) = IZ
        FROM(5:8) = IHDAYW(1:4)
        CALL BIN2EB(IDAY,CHTEXT,2,'A999')
        FROM(9:10) = CHTEXT(1:2)
        FROM(12:15) = MONS(MONTH)(1:4)
        CALL BIN2EB(IYR,CHTEXT,4,'A999')
        FROM(16:19) = CHTEXT(1:4)
C
            WRITE(6,FMT='('' FROM='',A)') FROM(1:20)
        LGULF(17:36) = FROM(1:20)
C
C       CONSTRUCT VALID TIME ARRAY IE. VALID 00Z THU 16 MAR 1989
C
         CALL DAYOWK(IVLDAY,IVLDMO,IVLDYR,IDAYWK,IHDAYW)
        VALID(7:09) = LGULF(7:09)
        VALID(11:13) = IHDAYW(1:4)
        CALL BIN2EB(IVLDAY,CHTEXT,2,'A999')
        VALID(15:16) = CHTEXT(1:2)
        VALID(18:21) = MONS(IVLDMO)(1:4)
        CALL BIN2EB(IVLDYR,CHTEXT,4,'A999')
        VALID(22:25) = CHTEXT(1:4)
            WRITE(6,FMT='('' VALID='',A)') VALID(1:28)
        LGULF(37:64) = VALID(1:28)
        IF(AFOS) THEN
C
C       CONSTRUCT AFOS BASE TIME ARRAY IE. 03/15/00Z   1989
C
        CALL BIN2EB(MONTH,CHTEXT,2,'A999')
        ATIME(1:2) = CHTEXT(1:2)
        ATIME(4:5) = LGULF(25:26)
        ATIME(7:9) = LGULF(17:19)
        ATIME(13:16) = LGULF(32:35)
        LGULF(62:77) = ATIME(1:16)
            WRITE(6,FMT='('' ATIME FOR AFOS='',A)') LGULF(62:77)
        ELSE
        ENDIF
      RETURN
      END
