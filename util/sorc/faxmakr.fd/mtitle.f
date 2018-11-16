      SUBROUTINE MTITLE(LABEL)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MTITLE      PUT TITLES IN THE DESIGN MAP FILE.
C   PRGMMR: KRISHNA KUMAR         ORG: W/NP12      DATE: 1999-08-01
C
C ABSTRACT: STORES THE MAP TITLES IN THE DESIGN FILE.
C
C PROGRAM HISTORY LOG:
C   94-09-28  ORIGINAL AUTHOR  HENRICHSEN
C   94-11-28  HENRICHSEN    ADD NEW ARG TO DGNTXT
C   94-12-30  LUKE LIN      CONVERT IT CFT-77
C   96-04-29  LUKE LIN      MODIFY FOR FAXMAKR.
C   96-06-07  HENRICHSEN    MODIFY PUT ON A USER TITLE AS NEEDED. AND
C                           FIX GULF LABEL.
C   96-06-21  HENRICHSEN    MODIFY PUT AN ADDITIONAL LINE ON THE
C                           GULF LABEL IF LLEVEL FLAG IS .TRUE.
C                           FIX GULF LABEL.  ADDED LOGIC TO USE TITLE
C                           HEIGHT "THEIGHT" THAT WAS PASSED IN COMMON/
C                           LLABFX/.
C   96-06-27  HENRICHSEN    MODIFY TO FIX SPACING INBETWEEN THE GULF LABEL
C                           TEXT.
C   96-10-10  LUKE LIN      FIX GULF LABELS FOR GFS AND CGRID
C   96-11-18  LUKE LIN      ADD THE TROPOPAUSE GULF TITLE.
C   97-06-18  LUKE LIN      MODIFY FOR TYPE 5 NAM GULF TITLE.
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM RS/6000
C
C USAGE:    CALL MTITLE
C   INPUT ARGUMENT LIST:
C   LABEL      - 12 WORDS LABEL ON INPUT.  WORDS 1-5 SHOULD CONTAIN
C                LABEL ID OF DESIRED FIELD FLD.
C
C   FROM COMMON/LLABFX/
C     GULPXX   - INTEGER X GRID COORDINATE OF GULF TITLE
C     GULPXY   - INTEGER Y GRID COORDINATE OF GULF TITLE
C     TITPXX   - INTEGER X GRID COORDINATE OF MAP  TITLE
C     TITPXY   - INTEGER Y GRID COORDINATE OF MAP  TITLE
C     NUMG     - INTEGER WORD THAT CONTAINS THE NUMBER OF BYTES IN
C              - GLAB ARRAY.
C     NUMT     - INTEGER WORD THAT CONTAINS THE NUMBER OF BYTES IN
C              - TITLE ARRAY.
C     GLAB     - CHARACTER*24 ARRAY CONTAINS THE GULF TITLE.
C     TITLE    - CHARACTER*152 ARRAY CONTAINS THE TITLE.
C     JFID  - CHARACTER*48  ARRAY CONTAINS THE JFID TITLE IN
C     LLEVEL   - LOGICAL FLAG THAT SIGNALS IF THE HEIGHT OF SURFACE
C              - IS TO BE ADDED AS AN EXTRA LINE ON THE GULF TITLE.
C
C   FROM COMMON/POLE/
C     XPOL     - REAL*4 X LOCATION OF POLE
C     YPOL     - REAL*4 Y LOCATION OF POLE
C     GDTYPE   - INTEGER*4 GRID TYPE FLAG.
C
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C
ckumar
      COMMON /DUCKNOAA/ DUCKX,DUCKY,DUCKFG
ckumar
      INTEGER       DUCKX,DUCKY,DUCKFG
C
      COMMON /ILCON/ MAP(15)
      INTEGER       MAP
C
      CHARACTER*8 BGNAME
      EQUIVALENCE (BGNAME,MAP(1))
C
      COMMON /LABG/ GULPXX(2),GULPXY(2),LVFLG,NUMG,GLAB
C
      INTEGER       GULPXX,GULPXY,LVFLG,NUMG
      CHARACTER*24  GLAB
C
      COMMON /LLABFX/ TITPXX,TITPXY,HGTFLG,THEIGHT,NUMT,JBYT,
     1              TITLE,JFID,EXVALID              
C
      INTEGER       TITPXX,TITPXY,HGTFLG,NUMT,JBYT
      REAL          THEIGHT
      CHARACTER*152 TITLE
      CHARACTER*48  JFID
      CHARACTER*32  EXVALID
C
      COMMON /EXTIT/ ETITFONT,ETITPXX,ETITPXY,NOEXT,EXTRAT,FGEXTRAT
      REAL    ETITFONT
      INTEGER ETITPXX,ETITPXY,NOEXT
      CHARACTER*80    EXTRAT
      LOGICAL         FGEXTRAT
C
C
      LOGICAL       LLEVEL 
C
      COMMON /MUTCON/ KEYIDX,UA1V,UA2V,UA3V,UM1V,UM2V,UM3V,LINEVU,
     1               LINEP,IGRIDP,T1

C
      COMMON  /POLE/ XPOL,YPOL,GDTYPE
C     ...THE POLE POSITION IN GRID(65,65) IS AT GRID(33,33).
C     ...THE POLE POSITION IN GRID(53,45) IS AT GRID(27,49).
C

      COMMON /UUNAME/ USRBYT,LUNAM,USRNAM
C
      INTEGER       USRBYT
      LOGICAL       LUNAM
      CHARACTER*96  USRNAM
C
      LOGICAL       LBIG          
C
      CHARACTER*10  GULFLAB
C
      REAL          PUTHGT,PUTANG,THGT,GHGT,UHGT
      INTEGER       IPRPUT(2)
C
      INTEGER        GDTYPE
      INTEGER        NUMV
C
      INTEGER         LABEL(12)
      INTEGER     MSK2
      DATA        MSK2      /Z'00000000000000FF'/
      DATA        GULFLAB   /'          '/
C
      LBIG = .FALSE.
C
C          CHECK TO SEE IF GRID TYPE IS 26, IF SO SET LBIG = .TRUE.
C          TO ADJUST HEIGHT AND I/J FOR PART OF GULF TITLE.
C
         IF (IGRIDP.EQ.26)LBIG = .TRUE.
         IF (IGRIDP.EQ.05)LBIG = .TRUE.
C
C       CHECK TO SEE IF LLEVEL SHOULD BE ON
C
         IF(LVFLG.GT.0)THEN
           LLEVEL = .TRUE.
           WRITE(6,FMT='('' MTITLE: PUTING AN EXTRA LINE IN THE GULF'',
     1   '' TITLE. THE GULF TITLE WILL BE :'',A)')GLAB(1:21)
         ELSE
           WRITE(6,FMT='('' MTITLE: LVFLG='',I2,'' THE GULF'',
     1   '' TITLE. THE GULF TITLE WILL BE :'',A)')LVFLG,GLAB(1:17)
           LLEVEL = .FALSE.
         ENDIF       
C
         PUTHGT = 11.0
         PUTANG = 0.0
         IPRPUT(1) = 0
         IPRPUT(2) = 2
         ITAG = 0
C
C        PUT TITLE ON MAP
C
      IF (TITPXX.GT.0 .AND. TITPXY.GT.0) THEN
        IF(HGTFLG.NE.0)THEN
         THGT = THEIGHT
        ELSE
         THGT = PUTHGT
        ENDIF
            IX = TITPXX - MAP(13)
            IY = TITPXY - MAP(14)
      CALL PUTLAB(IX,IY,THGT,TITLE,PUTANG,NUMT,IPRPUT,ITAG)
      ENDIF
C
C          PUT USR'S TITLE FROM PARM.
C
      IF(LUNAM)THEN
            IX = TITPXX - MAP(13)
            IY = TITPXY + 45 - MAP(14)
            NUM = USRBYT
            UHGT = PUTHGT
C
        WRITE(6,FMT='('' MTITLE: PUTING AN EXTRA TITLE ON THE MAP.'',
     1   '' THE TITLE HAS '',I2,'' BYTES AND IS: '',A)')USRBYT,
     2   USRNAM(1:USRBYT)
        CALL PUTLAB(IX,IY,UHGT,USRNAM,PUTANG,NUM,IPRPUT,ITAG)
      ENDIF
C
C     .... PUT EXTRA TITLE FROM OPTION CARDS
      IF (FGEXTRAT) THEN
        IF(HGTFLG.NE.0)THEN
         THGT = THEIGHT
        ELSE
         THGT = PUTHGT
        ENDIF
            IX = ETITPXX - MAP(13)
            IY = ETITPXY - MAP(14)
            THGT = ETITFONT
        WRITE(6,FMT='('' ETITLE: PUTING AN EXTRA TITLE ON THE MAP.'',
     1   '' THE TITLE HAS '',I2,'' BYTES AND IS: '',A)')NOEXT, 
     2   EXTRAT(1:NOEXT) 
        CALL PUTLAB(IX,IY,THGT,EXTRAT,PUTANG,NOEXT,IPRPUT,ITAG)
      ENDIF
C
C          PUT DUCK LABEL ON THE MAP
C
      IF (DUCKX.GT.0 .AND.DUCKY.GT.0) THEN
           IX = DUCKX - MAP(13)
           IY = DUCKY - MAP(14)
           KANG = 0
           KFLAG = DUCKFG
           CALL DUCK(IX,IY,KANG,KFLAG)
      ENDIF
C
C          PUT GULF LABEL ON MAP
C
      IF (GULPXX(1) .GT.0 .AND. GULPXY(1) .GT.0) THEN
       IF (IGRIDP.EQ.101 .AND. BGNAME(1:6).EQ.'NH4006') THEN
C      ....  FOR NGM C-GRID ON NH4006
          GHGT = 19.0
            IX = GULPXX(1) - MAP(13)
            IY = GULPXY(1) - MAP(14) +20
            GULFLAB(1:6) = GLAB(1:6)
            NCHAR = 6
            IF (GLAB(1:3) .EQ. 'QPV') THEN
C              ... FOR QPVV NGM GRID-C ...
               GULFLAB(1:5)='QPVV/'
               GULFLAB(6:7)=GLAB(4:5)
               GULFLAB(8:8)='H'
               NCHAR = 8
            ENDIF
          CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
C           ...SUCH AS QPV12....
            IX = GULPXX(1) - MAP(13)
            IY = GULPXY(1) - MAP(14) +10
            GULFLAB(1:5) = GLAB(7:11)
            NCHAR = 5
          CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
            IX = GULPXX(1) - MAP(13) - 5
            IY = GULPXY(1) - MAP(14)
            GULFLAB(1:5) = GLAB(13:17)
            NCHAR = 5
          CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)        
            IX = GULPXX(1) - MAP(13) + 15
            IY = GULPXY(1) - MAP(14)
            GULFLAB(1:7) = ' GRID-C'
            NCHAR = 7
          CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
            IX = GULPXX(1) - MAP(13) - 5
            IY = GULPXY(1) - MAP(14) - 10
            GULFLAB(1:10) = '12HR ACCUM'
            NCHAR =10 
          CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
       ELSE
C         FOR OTHER REGULAR MAP'S GULF TITLES
C
C          CHECK TO SEE IF LBIG IS ON, IF SO ADJUST HEIGHT AND I/J
C
         IX = GULPXX(1) - MAP(13)
         IY = GULPXY(1) - MAP(14) +30
C
         IF (LBIG) THEN
C           ... TYPE 26 OR 05 SUCH AS NGM, NAM ...
            GHGT = 11.0
            IX = IX - 15
            GULFLAB(1:6) = GLAB(1:6)
            NCHAR = 6
            CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
         ELSE IF (IGRIDP.EQ.27 .OR. IGRIDP.EQ.28) THEN
C           ... TYPE 27 SUCH AS GFS
            GHGT = 1.0
            IF (GLAB(1:2) .EQ. 'RH') THEN
               GULFLAB(1:6) = GLAB(1:6)
            ELSE IF (GLAB(1:3) .EQ. 'VOR') THEN
               GULFLAB(1:6) = GLAB(1:6)
            ELSE IF (GLAB(1:2) .EQ. 'LI') THEN
               GULFLAB(1:6) = GLAB(1:6)
               GULFLAB(3:3) = ' '
            ELSE IF (GLAB(1:2) .EQ. 'TP') THEN
               GULFLAB(1:6) = GLAB(1:6)
               GULFLAB(3:3) = ' '
            ELSE IF (GLAB(1:3) .EQ. 'THK') THEN
               GULFLAB(1:6) = GLAB(1:6)
            ELSE IF (KEYIDX.EQ.41) THEN
               GULFLAB(1:3)='TP '
            ELSE
               GULFLAB(1:2) = GLAB(18:19)
               GULFLAB(3:5) = GLAB(3:5)
               GULFLAB(3:3) = ' '
            ENDIF
            NCHAR = 5
C
            IFCSTHR = IAND(LABEL(1),MSK2)
            PRINT *,' IFCSTHR =',IFCSTHR
            IF (IFCSTHR .GE. 100) THEN
               GULFLAB(3:6) = GLAB(3:6)
               GULFLAB(3:3) = ' '
               NCHAR = 6
            ENDIF
            PRINT *,' GFS-LABEL1=', GULFLAB,' --=',GLAB(1:6)
            PRINT *,' GLAB(16:24)=',GLAB(16:24)
            CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
         ELSE IF (IGRIDP.EQ.101) THEN
C           ... TYPE 101 SUCH AS NGM C-GRID
            GHGT = 1.0
            GULFLAB(1:6) = GLAB(1:6)
C           PRINT *,' NGM C-GRID: GLAB=',GLAB(1:6)
            NCHAR = 6
            IX = GULPXX(1) - MAP(13)
            IY = GULPXY(1) - MAP(14) +30
            IF (GLAB(1:3) .EQ. 'QPV') THEN
C              ... FOR QPVV NGM GRID-C ...
               GULFLAB(1:5)='QPVV/'
               GULFLAB(6:7)=GLAB(4:5)
               GULFLAB(8:8)='H'
C              PRINT *,' NGM C-GRID: GULFLAB=',GULFLAB(1:8)
               NCHAR = 8
            ENDIF
            CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
         ENDIF
C
C          CHECK TO SEE IF LLEVEL IS ON, IF SO ADD A HEIGHT LABEL 
C          LINE TO THE GULF LABEL
C
         IF (LLEVEL .AND. IGRIDP.EQ.26) THEN
               IX = GULPXX(1) - MAP(13)
               IY = GULPXY(1) - MAP(14) +50
               IX = IX - 15
               GHGT = 11.0
               GULFLAB(1:5) = GLAB(18:22)
               NCHAR = 4
            CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
         ENDIF
C
         IF (LLEVEL .AND. IGRIDP.EQ.05) THEN
               IX = GULPXX(1) - MAP(13)
               IY = GULPXY(1) - MAP(14) +50
               IX = IX - 15
               GHGT = 11.0
               GULFLAB(1:5) = GLAB(18:22)
               NCHAR = 4
            CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
         ENDIF
C
            IX = GULPXX(1) - MAP(13)
            IY = GULPXY(1) - MAP(14) +15
            GHGT = 1.0
            GULFLAB(1:5) = GLAB(7:11)
            NCHAR = 5
C           PRINT *,' GFS-LABEL3=', GULFLAB
            CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
        IF (IGRIDP .NE. 101) THEN
            IX = GULPXX(1) - MAP(13)
            IY = GULPXY(1) - MAP(14)
            GULFLAB(1:5) = GLAB(13:17)
            NCHAR = 5
            CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
C           PRINT *,' GFS-LABEL4=', GULFLAB
C
        ELSE
C           ... FOR NGM C-GRID
            IX = GULPXX(1) - MAP(13) -10
            IY = GULPXY(1) - MAP(14)
            GULFLAB(1:5) = GLAB(13:17)
            NCHAR = 5
            CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)        
            IX = GULPXX(1) - MAP(13) + 30
            IY = GULPXY(1) - MAP(14)
            GULFLAB(1:6) = 'GRID-C'
            NCHAR = 6
            CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
            IX = GULPXX(1) - MAP(13) - 10
            IY = GULPXY(1) - MAP(14) - 15
            GULFLAB(1:10) = '12HR ACCUM'
            NCHAR =10 
            CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
        ENDIF
C
       ENDIF
      ENDIF 
C
C
C          CHECK TO SEE IF A 2ND GULF LABEL IS TO BE PUT ON THE MAP.
C
      IF (GULPXX(2) .GT.0 .AND. GULPXY(2) .GT.0) THEN
            IX = GULPXX(2) - MAP(13)
            IY = GULPXY(2) - MAP(14) +30
            GULFLAB(1:6) = GLAB(1:6)
            NCHAR = 6
C
C          CHECK TO SEE IF LBIG IS ON, IF SO ADJUST HEIGHT AND I/J
C
         IF (IGRIDP.EQ.101 .AND. BGNAME(1:6).EQ.'NH4006') GOTO 700
C
         IF (LBIG) THEN
            GHGT = 11.0
            IX = IX - 15
         ELSE
            GHGT = 1.0
         ENDIF
C
         IF (GLAB(1:3).EQ.'QPV' .AND. IGRIDP.EQ.101) THEN
C              ... FOR QPVV NGM GRID-C ...
               GULFLAB(1:5)='QPVV/'
               GULFLAB(6:7)=GLAB(4:5)
               GULFLAB(8:8)='H'
               NCHAR = 8
         ENDIF

         CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
C
C          CHECK TO SEE IF LLEVEL IS ON, IF SO ADD A HEIGHT LABEL 
C          LINE TO THE GULF LABEL
C
         IF (LLEVEL) THEN
            IX = GULPXX(2) - MAP(13)
            IY = GULPXY(2) - MAP(14) +50
            IX = IX - 15
            GHGT = 11.0
            GULFLAB(1:5) = GLAB(18:22)
            NCHAR = 4
            CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
         ENDIF
            IX = GULPXX(2) - MAP(13)
            IY = GULPXY(2) - MAP(14) +15
            GHGT = 1.0
            GULFLAB(1:5) = GLAB(7:11)
            NCHAR = 5
        CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
C
  700   CONTINUE
C
        GHGT = 1.0
        IF (IGRIDP .NE. 101) THEN
            IX = GULPXX(2) - MAP(13)
            IY = GULPXY(2) - MAP(14)
            GULFLAB(1:5) = GLAB(13:17)
            NCHAR = 5
        CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)        
C
        ELSE
C           ... FOR NGM C-GRID
            IX = GULPXX(2) - MAP(13) -10
            IY = GULPXY(2) - MAP(14)
            GULFLAB(1:5) = GLAB(13:17)
            NCHAR = 5
        CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)        
            IX = GULPXX(2) - MAP(13) + 30
            IY = GULPXY(2) - MAP(14)
            GULFLAB(1:6) = 'GRID-C'
            NCHAR = 6
        CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
            IX = GULPXX(2) - MAP(13) - 10
            IY = GULPXY(2) - MAP(14) - 15
            GULFLAB(1:10) = '12HR ACCUM'
            NCHAR =10 
        CALL PUTLAB(IX,IY,GHGT,GULFLAB,PUTANG,NCHAR,IPRPUT,ITAG)
        ENDIF
      ENDIF 
C
      RETURN
      END
