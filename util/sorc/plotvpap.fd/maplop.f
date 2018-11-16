      SUBROUTINE MAPLOP(NOLVSR,ILVLTS,WATPAP,ITOUT,LDZDTQ,LDZNEW,IOPTN,
     1                  DHMBS,KRUN,IDUMPT,IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MAPLOP      EXECUTIVE FOR PLOTTING OBSERVATIONS
C   PRGMMR: LIN              ORG: W/NP12   DATE: 97-03-03
C
C ABSTRACT: LOOPS THROUGH THE MB LEVELS (NOLVSR) AND CALLS VARIOUS
C   SUB. TO GET AND PROCESS THE UPPER AIR DATA AS PLOTTED LABELS FOR
C   OTHER GRAPHICS PROGRAMS.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR UNKNOWN
C   89-04-28  HENRICHSEN CLEAN UP AD DOCUMENT, ADD COMMON IAFOS.
C   90-10-31  HENRICHSEN ADD NEW WORD TO COMMON IAFOS, NUMAFS.
C   93-05-07  LILLY CONVERT SUB. TO FORTRAN 77
C   97-01-31  SHIMOMURA - CONVERT TO CRAY
C   97-02-27  SHIMOMURA - MODS TO READ AIRCFT OBS
C   97-03-03  LIN       - MODS TO DEAL WITH MID CYCLE MAPS
C
C USAGE:    CALL MAPLOP  (NOLVSR,ILVLTS,WATPAP,ITOUT,LDZDTQ,LDZNEW,
C          1            IOPTN,DHMBS,KRUN,IDUMPT,IERR)
C   INPUT ARGUMENT LIST:
C     NOLVSR   - INTEGER   WORD NUMBER OF UPPER AIR LEVELS TO PROCESS.
C     ILVLTS   - INTEGER   10 WORD ARRAY CONTAIN LIST OF LEVELS TO GET
C     WATPAP   - REAL*8 NAME OF FILE TO READ BY SUB READPA.
C     ITOUT    - INTEGER FLAG WITH RANGE FROM 1 THRU 13 , USED
C              - TO DETERMINE TYPE OF MAP BACKGROUND DATA IS TO BE
C              - DISPLAYED ON. ITOUT IS A FUNCTION OF KRUN AND IS
C              - SET IN SUB KOPTN.
C     LDZDTQ   - LOGICAL   FLAG = .TRUE. FOR MIDCYC, SET IN SUB KOPTN.
C              - = .FALSE. IF NOT MIDCYC.
C     LDZNEW   - LOGICAL   FLAG = .TRUE. FOR MIDCYC, SET IN SUB KOPTN.
C              - =  .FALSE. IF NOT MIDCYC.
C     IOPTN    - INTEGER FLAG READ IN FROM 1ST DATA CARD.
C     DHMBS    - REAL*8 29 WORD ARRAY OF NAMES OF MB LEVELS IN
C              - HOLLERTH IE. 'SURFACE ','1000MB  ','850MB   ', ECT
C     KRUN     - INTEGER RUN OPTION FLAG READ IN FROM 1ST DATA CARD.
C     IDUMPT   - INTEGER   2 WORD ARRAY CONTAINING THE DATA DUMP TIME
C              - IN HOLLERTH IE. '10+4','0   '.

C     COMMON   - /IAFOS / NUMAFS,NAMPIL(10),LVERSN,AFOS,SEND,CARD,MARG
C              - LVERSN 36 BYTE ARRAY CONTAINING THE AFOS VERSION
C              - STRIP TITLE.
C              - AFOS A LOGICAL   FLAG SET TO .TRUE. OR .FALSE.
C
C   OUTPUT ARGUMENT LIST:
C     IERR     - ERROR RETURN.
C     NUMAFS   - COUNTER RETURNED IN COMMON/IAFOS/ INDICATES THE NUMBER
C              - AFOS PLOTFILE MAPS MADE.
C
C   OUTPUT FILES:
C     FT06F001 - PRINT FILE.
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C
      COMMON / DATE / NYR,NMO,NDA,NHR,CDUMP
      CHARACTER*8     CDUMP
      COMMON /IAFOS / NUMAFS,NAMPIL(10),LVERSN,AFOS,SEND,CARD,MARG
      COMMON /TIMES / NANJI(12)
      COMMON /KPLOT / LABEL(2,1024),LABIX,NOBUF,IDRA(50)

      COMMON /MIDCYC/ MIDCYC
      LOGICAL     MIDCYC

C     ----------------------------------------------------------------
C     ----------------------------------------------------------------
      INTEGER    MAXOBS
      PARAMETER (MAXOBS=20000)
      INTEGER    NDATASIZ
      PARAMETER (NDATASIZ=MAXOBS+1)
      INTEGER    LMTWRDPOB
      PARAMETER (LMTWRDPOB=10)
      INTEGER    LMTHFWPOB
      PARAMETER (LMTHFWPOB=2*LMTWRDPOB)		!... =(20)

      COMMON  /OBSLVLDB/NOBSDB,LVLIX,IOBS2PK
      INTEGER           IOBS2PK(LMTWRDPOB,NDATASIZ)


      
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      COMMON /      / ISCRDATA
      INTEGER         ISCRDATA(3,NDATASIZ)
      COMMON  /JSPACE/IDREC,NDATA
      INTEGER         IDREC(6)
      INTEGER         NDATA(3,NDATASIZ)

       INTEGER     NDIXMAX		!... NO OF J-ROWS IN ANY GRID
       PARAMETER  (NDIXMAX=118)	
       INTEGER     IDIXAD(NDIXMAX)
       INTEGER     IDIXCO(NDIXMAX)

       INTEGER     NDIX

C      . . . . . . . . . . . . . . . . . . . .
C                            ... SORTEM ARGS ...
       INTEGER   ISKEY
       DATA      ISKEY   / 1 /
       INTEGER   KEYML
       DATA      KEYML   / 6 /
       INTEGER   KEYMH
       DATA      KEYMH   / 41 /
       INTEGER   IRET_SOR
C      . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . .
       integer     mskJIJ
       data        mskJIJ    / X'000001FFFFFFFFE0' /
       INTEGER     MSKJJJ
       DATA        MSKJJJ     / X'000001FF00000000' /
       INTEGER     NRITSHF
       DATA        NRITSHF    / 32 /
C      . . . . . . . . . . . . . . . . . . . .

C     ----------------------------------------------------------------
C     ----------------------------------------------------------------
      CHARACTER*8  NEWPAP
      DATA         NEWPAP         /'NEWPAP  '/
      CHARACTER*8  WATPAP
C
      CHARACTER*8  DHMBS(29)
C
      INTEGER    KSTDHT(21)
      DATA       KSTDHT
     1             /113,1457,3011,557,718,916,1036,1178,1362,1621,
     2              1849,2063,2389,2648,3105,3347,3578,3944,4245,
     3              4766,5472/


      INTEGER    ILVLTS(10)
      INTEGER    IDUMPT(2)
C
      INTEGER    IFLG
      DATA       IFLG        /0/
      INTEGER    INIT
      DATA       INIT        /Z'3F012B00'/
      INTEGER    LIDOBS
      DATA       LIDOBS      /200/
      INTEGER    MAXDFE
      DATA       MAXDFE      /255/
      INTEGER    MSKJJ
      DATA       MSKJJ       /Z'1FF'/
      INTEGER    NDIM1
      DATA       NDIM1       /4001/
C
      LOGICAL    W3AI24
      LOGICAL    AFOS
      LOGICAL    CARD
      LOGICAL    LDZDTQ
      LOGICAL    LDZNEW
      CHARACTER*1  LVERSN(40)
      LOGICAL    MARG
      LOGICAL    LMARGIN
      LOGICAL    SEND
C
      INTEGER     KROT_PRI(2),ICMD,IPT,JPT,NCHAR
      REAL        HEIGHT,ANGLE
      CHARACTER*4 CTEXT
      CHARACTER*8 HEDER(50)
      CHARACTER*8 HEDERAIR(50)
      integer     LUNUPA
      LOGICAL     LWNDONLY
      CHARACTER*2 CHTEXT
      LOGICAL     LAIRCFTQQ
      LOGICAL     LSATWNDQQ
      LOGICAL     LADDONQQ
      INTEGER     IRET_AIR
C
C---------------------------------------------------------------------------
C       SET NUMAFS COUNTER TO ZERO
C
         NUMAFS = 0
C
         IER = 0
         IERRA = 0

         LUNUPA = 41
         LUNSAT = 43
         LUNACF = 42
         LUNTOS = 44
         LAIRCFTQQ = .TRUE. 
         LSATWNDQQ = .TRUE. 
         LADDONQQ  = .TRUE. 
C
C        IF ( KRUN.EQ.8 .OR. KRUN.EQ.9) THEN
C            PRINT *, ' THIS 1/2 DOT, ONLY PLOT UPAUPA.'
C            LAIRCFTQQ = .FALSE.
C            LSATWNDQQ = .FALSE.
C            LADDONQQ  = .FALSE.
C        ENDIF
C
      DO 1000 IMAP=1,NOLVSR
         ILVLT = ILVLTS(IMAP)
         LVLDES = ILVLT - 1

         IF ( .NOT. MIDCYC) THEN

           call readupa(LUNUPA,LVLDES,KRUN,ITOUT,HEDER,IRET_RED)

           PRINT *, ' HEADER=',HEDER(3),' ',HEDER(4),' ',HEDER(5)
           IF(IRET_RED .NE. 0) THEN
              PRINT 100,IRET_RED
  100         FORMAT(1H ,'MAPLOP::READUPA: RETURN-CODE = ', I4)
              IERR = 13
              GO TO 1000
           ENDIF
           PRINT *, ' HEADER=',HEDER(3),' ',HEDER(4),' ',HEDER(5)
           CHTEXT= HEDER(3)(1:2)
           CALL ASC2INT(2,CHTEXT,NYR,IERR)
           CHTEXT= HEDER(3)(3:4)
           CALL ASC2INT(2,CHTEXT,NMO,IERR)
           CHTEXT= HEDER(3)(5:6)
           CALL ASC2INT(2,CHTEXT,NDA,IERR)
            
           CHTEXT= HEDER(3)(7:8) 
           CALL ASC2INT(2,CHTEXT,NHR,IERR)
           CDUMP(1:4)=HEDER(4)(3:6)
           PRINT *,' NYR=',NYR,' NMO=',NMO,' NDA=',NDA,' NHR=',NHR
           PRINT *,' DUMP TIME IS ',CDUMP
         ENDIF
C
         IF(LAIRCFTQQ) THEN
           CALL READAIR(LUNACF,LADDONQQ,LVLDES,KRUN,ITOUT,HEDERAIR,
     1                  IRET_AIR)
           IF(IRET_AIR .NE. 0) THEN
             WRITE(6,FMT='(1H ,''maplop::readair: return code='',I5,
     1                         '', NOBSDB='',I6)')
     A               IRET_AIR,NOBSDB
           ENDIF
         PRINT *, ' HEADER=',HEDERAIR(3),' ',HEDER(4),' ',HEDERAIR(5)
           CHTEXT= HEDERAIR(3)(1:2)
           CALL ASC2INT(2,CHTEXT,NYR,IERR)
           CHTEXT= HEDERAIR(3)(3:4)
           CALL ASC2INT(2,CHTEXT,NMO,IERR)
           CHTEXT= HEDERAIR(3)(5:6)
           CALL ASC2INT(2,CHTEXT,NDA,IERR)
            
           CHTEXT= HEDERAIR(3)(7:8)
           CALL ASC2INT(2,CHTEXT,NHR,IERR)
           PRINT *,' NYR=',NYR,' NMO=',NMO,' NDA=',NDA,' NHR=',NHR
         ENDIF
C
         IF(LSATWNDQQ) THEN
           PRINT *, ' BEFOR SATWND'
           LADDONQQ  = .TRUE.
           CALL READAIR(LUNSAT,LADDONQQ,LVLDES,KRUN,ITOUT,HEDERAIR,
     1                  IRET_AIR)
           IF(IRET_AIR .NE. 0) THEN
             WRITE(6,FMT='(1H ,''maplop::readsat: return code='',I5,
     1                         '', NOBSDB='',I6)')
     A               IRET_AIR,NOBSDB
           ENDIF
         ENDIF
C
C??      CALL READPA(WATPAP,LOCT,IDTBL,IDREC,NDATA,NDIM1,ITCT,LVLDES,
C??  1            ITOUT,IERRA)
C     ...THE CALL TO READPA WAS PUT BEFORE INITIALIZATION OF LABEL FILE
C     ...   TO GET DATE-TIME INFO INTO IDTBL AND GET GOOD READ OF PAP
         IF( (IOPTN .NE. 1) .OR. (.NOT. LDZNEW) ) GO TO 300
C??      IF(IMAP .EQ. 1) CALL W3FK40(NEWPAP,LCT1,MAXDFE)
C??      CALL RDSIRS(NEWPAP,LCT1,IDTB1,NDATA,NDIM1,ITCT,LVLDES,ITOUT,IERRA)
C??      PRINT 210,IERRA
  210    FORMAT(1H ,20X,'RETURNED FROM RDSIRS WITH IERRA= ',I4)
  300    CONTINUE

C        ...GET 50 WORD IDRA INITIALIZED FOR THIS MAP...
         CALL INIDRA(IMAP,ILVLT,DHMBS,ITOUT,KRUN)
         IMO = NMO
         IDA = NDA
         IYR = NYR
         IHR = NHR
          CALL PUTLAB(1,1,1.0,ITEMP,0.0,1,0,-1)
C         ...WHICH ARE DUMMY ARGS EXCP FOR THE -1 WHICH TELLS
C         ...   PUTLAB TO PRINT THE ID RECORD ONTO TAPE55
          DO  310  J = 1,1024
             LABEL(1,J) = 0
             LABEL(2,J) = 0
  310     CONTINUE
C
C         ... INITIALIZE LABEL ARRAY ...
C
          LABIX = 0
          NOBUF = 0
C
          IPT = 1
          JPT = 0
          HEIGHT = 1.0
          ANGLE = 0.0
          KROT_PRI(1) = 0
          KROT_PRI(2) = 0
          CTEXT(1:1) = '?'
          CTEXT(2:2) = CHAR(1)
          CTEXT(3:3) = '$'
          CTEXT(4:4) = CHAR(0)
          NCHAR = 2
          ICMD = -2

          CALL PUTLAB(IPT,JPT,HEIGHT,CTEXT,ANGLE,NCHAR,KROT_PRI,
     X                ICMD)
C
C         ...WHICH INITITALIZES TO REGULAR CHAR SET...
          IF(ITOUT .EQ. 6) CALL TITLEN(100,100,IERR0)
C         ...PLACE TITLE ON SIRS PLOTTED CHART...
          IF(ITOUT .NE. 7) GO TO 320
C     ...PLACE A TITLE ON THE MERCATOR CHARTS...
           NANJI(5) = IDUMPT(1)
           NANJI(6) = IDUMPT(2)
           IONE=850
           ITWO=2200
           CALL TITLEO(IONE,ITWO,DHMBS(ILVLT),IERRR)
  320      CONTINUE
C
C          ============================================================
C          ============================================================

           DO  J = 1,NDATASIZ
             DO  I = 1,3
               NDATA(I,J) = 0
             ENDDO
           ENDDO

           lvldes = lvlix - 1

           call mksorwrd(lvldes,itout,itct,iret_mks)

           write(6,815)ITCT,iret_mks
  815      format(1h ,'tesrduupa::mksorwrd: ITCT=',I8,
     1                '; return-code=',I4)
           do  JJ = 1,ITCT
             iacc = iand(ndata(1,JJ),mskJIJ)
             IF(IACC .EQ. 0) THEN
               WRITE(6,817)jj
  817          FORMAT(1H ,'tesrduupa::mksorwrd: results terminated at ',
     1                   I8,'th word with zero value')
               go to 818
             endif
           enddo
  818      continue

           call presort(KRUN)   

           do  JJ = 1,ITCT
             iacc = iand(ndata(1,JJ),mskJIJ)
             IF(IACC .EQ. 0) THEN
               WRITE(6,819)jj
  819          FORMAT(1H ,'tesrduupa::presort: results terminated at ',
     1                    I8,'th word with zero value')
               go to 820
             endif
           enddo
  820      continue


           call sortem(NDATA,3,NDATASIZ,ISCRDATA,ISKEY,KEYML,KEYMH,
     1                IRET_SOR)
           WRITE(6,825)iret_sor
  825      FORMAT(1H ,'tesrduupa::sortem: return-code=',I3)
           IF(IRET_SOR .NE. 0) THEN
             IERR = 11
             GO TO 1000
           ENDIF

           NDIX = 52
           IF(KRUN .EQ. 11) NDIX = 118
           IF(KRUN .EQ. 15) NDIX = 82
           IF(KRUN .EQ. 22) NDIX = 42
           IF(KRUN .EQ. 23) NDIX = 42

           print *,' before dixie'
           call dixie(NDATA,3,NDATASIZ,ISKEY,MSKJJJ,NRITSHF,
     1                IDIXAD,IDIXCO,NDIX,IRET_DIX)
           WRITE(6,835) IRET_DIX
  835      FORMAT(1H ,'tesrduupa::dixie: RETURN-CODE=',I5)

          call thin_upa(NDATA,3,NDATASIZ,ISKEY,IDIXAD,IDIXCO,NDIX,ITOUT)

C          ============================================================
C          =============================================================
           IF( .NOT. LDZDTQ) GO TO 430
C**        CALL TITLEJ(1230,170) PLACE THE DATA CUT TIME OVER THE YUCATAN.
           CALL TITLEJ(130,50)
C          ...PLACE HGT CHANGE LEGEND
C          IF (KRUN.EQ.8 .OR. KRUN.EQ.9) THEN
C              CALL TITLHC(110,50,ILVLT)
C          ENDIF
           NPASS = 1
C          ...FIRST TIME THRU WORK WITH NEWPAP
       IF((IMAP .GE. 2).OR.(.NOT. W3AI24(WATPAP,NEWPAP,8))) GO TO 410
C ...          DO 400 I=1,256
C ...            LCT1(I) = LOCT(I)
C ...  400     CONTINUE
C
          IFLG = 2
  410    CONTINUE
  430    CONTINUE
C
      IF( (ITOUT .NE. 2) .OR. (.NOT.AFOS) ) GO TO 500
C
C     I WILL BYPASS THE AFOS PLOTFILE FORMATION UNLESS THIS IS A
C     TWO-DOT RUN AND THE APPROPRIATE PARM SWITCH IS ON.
C
      IF( (LVLDES .LT. 2) .OR. (LVLDES .GT. 8) ) GO TO 500
      IF(LVLDES .EQ. 5) GO TO 500
C     ...OTHERWISE PREPARE AFOS PLOTFILE HEADERS...
      NHR = IHR
      NDA = IDA
      NMO = IMO
      NYR = IYR
      PRINT *,' NHR=', NHR
C
C???  CALL AFPLTF(LVLDES,IDUMPT,KADDZ,ITOUT,IER)
C
C     CHECK RETURN CODE FROM AFOSPH.
C
C
C     IER CAN HAVE 3 VALUES: = 0 EVERY THING OK
C                            = 1 COULD NOT FIND AFOS PIL NUMBER SO
C                              SUB B4PLOT WAS NOT CALLED IN AFPLTF.
C                            = 2 SUB AFORMT DID NOT WRITE AFOS
C                            = PLOTFILE.
C
C?       WRITE(6,451) IER,LVLDES,ITOUT
  451 FORMAT(' RETURN SUB AFPLTF WITH IER=',I2,' FOR LEVEL ',I4,
     X   ' ITOUT=', I2)
C??   IF(IER .EQ. 1) GO TO 500
C?    PRINT *, ' ICHK BEFORE 500'
C?    IF(IER.EQ.2)WRITE(6,454) LVLDES
  454 FORMAT(' ERROR RETURN FROM AFORMT.  PLOTFILE NOT WRITTEN FOR',
     X   ' LEVEL ', I2)
C??   GO TO 600
  500 CONTINUE
      IF (MARG) THEN
         LMARGIN = .TRUE.
         PRINT *, ' LMARGIN IS TRUE.'
      ELSE
         LMARGIN = .FALSE.
         PRINT *, ' LMARGIN IS FALSE.'
      ENDIF
      IDELTA = 0
      LWNDONLY = .FALSE.
      CALL B4PLOTX(LWNDONLY,LVLDES,IDELTA,ITOUT,LMARGIN,AFOS,IERROR)
  600 CONTINUE
C
      CALL IDTITL
C     ...WHICH PRINTS SOME IDENTS ON THE MAP AS A TEST OF PRINTING
      IF(ITOUT .EQ. 5) CALL TITLES
C     ...WHICH LABELS SOME OF THE GEOGRAPHY IN THE SOUTHERN HEMISPHERE..
      IF(ITOUT .LE. 2) CALL LGND1D
      IF(ITOUT .EQ. 7) CALL LGND1D
      IF((ITOUT .EQ. 12) .OR. (ITOUT .EQ. 13)) CALL LGND1D
C
C     WHICH WRITES A SPECIAL LEGEND ON 1- AND 2-DOT MAPS AS WELL AS
C     ON TROPIC PLOTS ON MERC AND GOES.
C
C     FOR CHECKOUT AND TESTING, PRINT THIS SPECIAL LEGEND
C     REGARDLESS OF THE OPTION.
C
      CALL PUTLAB(1,1,1.0,ITEMP,0.0,1,0,-7)
C     ...WHICH FINISHED OFF THIS LOGICAL FILE ON TAPE55
      LCKPT = 1088
      WRITE(6,888) LCKPT
  888 FORMAT('   ARRIVED AT CHECKPOINT = ', I4)
      PRINT 889
  889 FORMAT(1H1)
 1000 CONTINUE
      RETURN
      END
