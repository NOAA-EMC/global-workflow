      SUBROUTINE readair(LUNAIR,LADDONQQ,LVLDES,KRUN,ITOUT,HEDER,
     1                   IRET_RDA)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    readair     INITIALIZE OBS DATABASE & READ ACFT FILE
C   PRGMMR: SHIMOMURA        ORG: W/NP12     DATE: 97-02-18
C
C ABSTRACT: INITIALIZE OBS DATABASE AND READ ACFT FILE AND STORE INTO
C   THE DATABASE IN COMMON.  AN ADAPTATION OF READPA.  CALLED FROM
C   MAPLOP IN PLOTPAP.
C
C PROGRAM HISTORY LOG:
C   97-02-18  ORIGINAL AUTHOR -- DAVID SHIMOMURA
C                          ADAPTING READUPA() LOGIC TO READING ACFT FILE
C
C USAGE:    CALL readair(LUNAIR,LADDONQQ,LVLDES,KRUN,ITOUT,HEDER,
C                        IRET_RDA)
C
C   INPUT ARGUMENT LIST:
C     LUNAIR   - UNIT NUMBER OF INPUT ACFT FILE TO BE READ
C     LADDONQQ - LOGICAL SWITCH
C              = .T.  IF YOU WISH TO ADD ONTO AN ALREADY INITIALIZED
C                           LOCAL OBSERVATIONS DATABASE;
C              = .F.  IF YOU WANT ME TO INITIALIZE THE LOCAL DATABASE
C                           BEFORE PUTTING ANY ACFT OBS IN THERE;
C
C     LVLDES   - LEVEL DESIRED BY INDEX NUMBER
C     KRUN
C     ITOUT    - INTEGER FLAG WITH RANGE FROM 1 THRU 13 , USED
C              - TO DETERMINE TYPE OF MAP BACKGROUND DATA IS TO BE
C              - DISPLAYED ON. ITOUT IS A FUNCTION OF KRUN AND IS
C              - SET IN SUB KOPTN.
C
C   OUTPUT ARGUMENT LIST:
C     HEDER    - 50 WORD HEADER READ FROM LUNAIR WILL BE STORED HERE
C     IRET_RDA    - TO MARK ERROR RETURNS
C     common /OBSLVLDB/
C     IOBS2PK    - BIG BIN FOR RESULTING OBS (10,NDATASIZ)
C     NDATASIZ    - J DIMENSION OF IOBS2PK (NO. OF OBS IT CAN HOLD+1)
C     NOBSDB     - NO. OF OBS PUT INTO IOBS2PK FROM LUNAIR FILE

C     COMMON   - / DATE /NYR,NMON,NDAY,NHR
C     COMMON   - / DAT1 /IHR1,IDA1,IMO1,IYR1
C     COMMON   - /WLONG0/ WLONG0
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C
C     If this aircraft obs reader is used after readupa has put raobs
C     into the local data base, then do I want to add onto an existing
C     database???   Then can I assume that the database has already been
C     initialized???   But is there any plotted chart of aircraft only?
C
C
C
C     This is a test program to use the subroutine rd_airc          
C     to return one AIRCRAFT station at a time from the 
C     rd_airc data file
C
C     Usage:
C     CALL readair(LUNAIR,LVLDES,KRUN,ITOUT,HEDER,IRET_RDA)
C            RESULTING DATA ARE PUT INTO COMMON
C    
C     CALLS rd_airc TO ACTUALLY READ THE DATA FILE:
C            CALL rd_airc(LUNAIR,LINITQ, LVLIX, HEDER, LONELVL, IRET )
C
C     where the input parameters are,
C
C            LUNAIR   -   Input file number
C            LINITQ   -   LOGICAL SWITCH 
C                      = .TRUE. ;  TO COMMAND rd_airc TO REWIND
C                            THE INPUT FILE AND READ THE FILE HEADER
C                            BEFORE READING THE OBSERVATIONS;
C                            SUBR rd_airc WILL RESET THAT SWITCH TO =.F.
C                            SO THAT ON SUBSEQUENT CALLS TO GET THE
C                            NEXT REPORTS, IT WILL NOT INITIALIZE.
C                            
C            LVLIX    -   MILLIBAR LEVEL BY INDEX NUMBER
C    
C     and the output parameters are,
C          
C            HEDER  -   The rd_airc file header record.
C            LONELVL(18) - ONE LEVEL'S DATA IN BINARY
C
C            IRET   -   Return code:        0 = report returned
C                                          -1 = end of file
C
       INTEGER      MAXWRDLVL
       PARAMETER   (MAXWRDLVL=24)
       INTEGER      NWRDLVLPAK
       PARAMETER   (NWRDLVLPAK=MAXWRDLVL/2)

       INTEGER      MAXWRDHDR
       PARAMETER   (MAXWRDHDR=50)
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
      COMMON   / DATE /NYR,NMON,NDAY,NHR
      COMMON   / DAT1 /IHR1,IDA1,IMO1,IYR1
      COMMON   /WLONG0/ WLONG0
C

C     -----------------------------------------------------------------
C     Usage:
C     CALL readair(LUNAIR,LADDONQQ,LVLDES,KRUN,ITOUT,HEDER,IRET_RDA)
       integer      LUNAIR
       LOGICAL      LADDONQQ
       integer      lvldes
       INTEGER      KRUN
       INTEGER      ITOUT
       CHARACTER*8  HEDER(50)
       integer      IRET_RDA
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


       INTEGER      IMAX
       INTEGER      JMAX
C
      DATA     ABGMXI         /109.0/
      DATA     ABGMXJ         / 81.0/
      DATA     AIMAX          /47.0/
      DATA     AIMIN          /1.0/
      DATA     AJMAX          /51.0/
      DATA     AJMIN          /1.0/

       REAL         FLD(16000)		!... BIG ENOUGH FOR ANY?

       logical      LINITQ
      INTEGER       LVLIX
      INTEGER       LONELVL(MAXWRDLVL)
      REAL         FLONELVL(MAXWRDLVL)

      INTEGER       IONELVLPAK(NWRDLVLPAK)



      CHARACTER*8   CHGTS(21)
C
      DATA  CHGTS  /'1000    ',' 925    ',' 850    ',' 700    ',
     1              ' 500    ',' 400    ',' 300    ',' 250    ',
     1              ' 200    ',' 150    ',' 100    ','  70    ',
     3              '  50    ','  30    ','  20    ','  10    ',
     4              '   7    ','   5    ','   3    ','   2    ',
     5              '   1    ' /
C
       INTEGER    KEIL
       INTEGER    KLLGO2
       LOGICAL    LINTERPQQ
       REAL       HGTM
       LOGICAL    LINBOUNDQQ
       REAL       ALAT
       REAL       ALONG

       real       obtime

      integer      namstn
      character*8  cnamstn
      equivalence (namstn,cnamstn)

C      ... ARGS FOR HALF-PACKING VIA SBYTES ...
       INTEGER     NOFFSET
       DATA        NOFFSET   /  0 /
       INTEGER     KBITPGRP
       DATA        KBITPGRP  / 32 /
       INTEGER     KPADBITS
       DATA        KPADBITS  /  0 /
       INTEGER     NGRPS2DO  
C      . . . . . . . . . . . . . . . . . . . .
       integer     irec
       integer     nofflvl    
       integer     noutsider

       real        paltm
       REAL        ALTMLOW, ALTMHIGH
       INTEGER     IRET_ALT

        LOGICAL  LNEWPA
        LOGICAL  LGRANP
        LOGICAL  LDEWDO
        LOGICAL  LCHKPRNTQ
        LOGICAL  LDOACFTQQ
       

       SAVE
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       IRET_RDA = 0
C ...       NOBSDB = 0
       LCHKPRNTQ = .FALSE.
       LDOACFTQQ = .TRUE.

       NGRPS2DO = MAXWRDLVL

           NDIX = 52
           IF(KRUN .EQ. 11) NDIX = 118
           IF(KRUN .EQ. 15) NDIX = 82
           IF(KRUN .EQ. 20) NDIX = 144
           IF(KRUN .EQ. 22) NDIX = 42
           IF(KRUN .EQ. 23) NDIX = 42
           AJMAX = FLOAT(NDIX)
C       --------------------------------------------------------------
C       ... to determine KEIL from ITOUT,  -- (SHOULD BE A FUNCTION)
      LNEWPA = .TRUE.
      LGRANP = .FALSE.
      
      IF(ITOUT .LT. 1) GO TO 702
      IF(ITOUT .GT. 14) GO TO 702
      GO TO 709
  702 CONTINUE
      PRINT 704, ITOUT
  704 FORMAT(1H , 'ERROR RETURN FROM READPA. GIVEN OUT-OF-RANGE ITOUT =
     1HEX', Z8, 2X, 'UNABLE TO READ ANY OBSERVATIONS' )
      IRET_RDA = 1
      RETURN

  709 CONTINUE
      NPTYPS = 3
      LDEWDO = .FALSE.
C             1   2   3   4   5   6   7   8   9  10  11  12  13
C ... GO TO (710,720,730,740,750,760,770,780,702,790,795,798,798),ITOUT

      if(itout .eq. 1) then
        KEIL = 1
C       ...FOR LFM ...
        IF(LVLDES .GT. 8) GO TO 799
        IF(.NOT. LNEWPA) GO TO 799
        NPTYPS = 4
        LDEWDO = .TRUE.
        GO TO 799

      else if(itout .eq. 2) then
        KEIL = 1
C       ...FOR LFM ....
        IF(LVLDES .GT.8) GO TO 799
        IF(.NOT. LNEWPA) GO TO 799
        NPTYPS = 4
        LDEWDO = .TRUE.
        GO TO 799

      else if(itout .eq. 3) then
C       ...IQSY ON N HEMI ...
        KEIL = 2
        GO TO 799

      else if(itout .eq. 4) then
        KEIL = 15
        AIMAX = 55.0
        AJMAX = 42.0
        GO TO 799

      else if(itout .eq. 5) then
        KEIL = 3
        GO TO 799

      else if(itout .eq. 6) then
        KEIL = 2
C       ...FOR SIRS PLOTTED CHART ...
        GO TO 799

      else if(itout .eq. 7) then
C       ...INTIIALIZE FOR TROPIC PLOT ON MERCATOR
        KEIL = 6
        GO TO 799

      else if(itout .eq. 8) then		!... big NHemi
C       ...INITIALIZE FOR LARGE 1/20M BACKGROUND.
        KEIL = 7
        aimax = 109
        ajmax = 81
        GO TO 799

      else if(itout .eq. 9) then
        go to 702		!... ERROR ... no ITOUT==9 ever

      else if(itout .eq. 10) then
C       ...INITIALIZE FOR GENERALIZED PLOT ON NH 1/40M BACKGROUND.
        KEIL = 2
        GO TO 799

      else if(itout .eq. 11) then
C       ...INITIALIZE FOR NH 1/60M BACKGROUND...
        AIMAX = 65.0
        AJMAX = 65.0
        KEIL = 14
        GO TO 799

      else if((itout .eq. 12) .or. (itout .eq. 13)) then
C       ...GOES-TYPE DISPLAY.
        KEIL = 0
        GO TO 799

      else if(itout .eq. 14) then		!... big srn hemi
        aimax = 109
        ajmax = 144
        keil = 16
        go to 799
   
      else
        go to 702
      endif
C
  799 CONTINUE

C     ... boundary tester needs KLLGO2,

      CALL SETKEIL(KRUN,ITOUTLCL,KLLGO2)
      
      IF(KRUN .EQ. 5) THEN
         AIMAX = 55.0
         AJMAX = 42.0
      ENDIF

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C 
      LVL = LVLDES
C     ...BETTER CHECK FOR LVL W/I VALID RANGE...
      IF((LVL .GT. 0) .AND. (LVL .LT. 22)) GO TO 810
      PRINT 7992,LVL
7992  FORMAT(1H ,'ERROR RETURN FROM READPA.  OUT-OF-RANGE LVL = HEX',Z8,
     X2X,'UNABLE TO SET ID FOR LATER READ OF LUNAIR')
C
  810 CONTINUE

C     .. ZERO THE BIG BIN
C     ...INITIALIZE COUNTER FOR BIG BIN OBS COUNT
C
C     *     *     *     *     *     *     *     *     *     *     *

C       --------------------------------------------------------------
C      ... DEFINE FOR BOUNDARY-TESTER ...
       DO  I = 1,16000
         FLD(I) = 0.0
       ENDDO

C ...       KLLGO2 = 3
C ...       KRUN = 2
C ...       KEIL = 1		!... LFM GRID
       LINTERPQQ = .FALSE.
C      . . . . . . . . . . . . . . . . . .
       LVLIX = LVLDES + 1
C ...         LVLIX = 3			!... GETS THE 850-MB DATA
C ...         LVLIX = 5   		!... GETS THE 500-MB DATA
       call setaltit(LVLDES,KRUN,ALTMLOW,ALTMHIGH,IRET_ALT)
       if(iret_alt .NE. 0) then
         write(6,125)KRUN,LVLDES
  125    format(1h ,'readair::setaltit: Failed to set altitude bounds ',
     1              ' for KRUN=',I3,' LVLDES=',I3)
         LDOACFTQQ = .FALSE.
       ELSE
         WRITE(6,127)ALTMLOW,ALTMHIGH
  127    FORMAT(1H ,'readair::setaltit: ALTMLOW=',F9.1,'  ALTMHIGH=',
     1               F9.1)
       ENDIF

       IF(.NOT. LDOACFTQQ) THEN
         IRET_RDA = 13
         GO TO 999
       ENDIF

C      READ REPORT FROM THE AIRCFT FILE
C
       PRINT *,' readair: ready to read ACFT via rd_airc()'
       IRET = 0
C ....       LUNAIR = 41
       LINITQ = .TRUE.		!... rd_airc WILL RESET TO .F.
       IREC = 0
       nofflvl = 0
       noutsider = 0
       IX = 0

       IF(LADDONQQ) THEN
         write(6,175)NOBSDB,(iobs2pk(I,NOBSDB),I=1,LMTWRDPOB)
  175    format(1h ,'readair: will add ACFT obs onto a partially ',
     1              'filled database . . .',
     2         /1h ,'The dump of the last obs (',I6,') in database:',
     3         /1h ,(4Z17.16))

       else
C        ... TO INITIALIZE THE DATABASE TO ALL ZEROS,
         WRITE(6,176)
  176    FORMAT(1H ,'readair: will zero the IOBS2PK database')

         do  j = 1,NDATASIZ
           do  i = 1,LMTWRDPOB
             IOBS2PK(I,J) = 0
           ENDDO
         ENDDO
         NOBSDB = 0
       ENDIF

       write(6,195)CHGTS(LVLIX)(1:6),LVLIX,KRUN,ITOUT,KEIL,AIMAX,AJMAX,
     1             KLLGO2,ALTMLOW,ALTMHIGH
  195  format(1h ,'readair: ready to start reading ACFT for ',A6,
     1            'MBS with LVLIX=',I4,
     2       /1H ,7X,'KRUN=',I5,'; ITOUT=',I5,'; KEIL=',I5,
     3            '; AIMAX,AJMAX= (',F9.3,',',F9.3,')',
     4       /1H ,7X,'KLLGO2=',I4,';  ALTMLOW=',F9.1,'  ALTMHIGH=',
     1               F9.1)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

  200  continue 	!... do until logical end of file ...
C
C         READ AN AIRCRAFT REPORT FROM THE rd_airc DATA SET
C
          CALL rd_airc(LUNAIR,LINITQ, HEDER, LONELVL, IRET)
          if(iret .ne. 0) then
            if(iret .EQ. -1) then	!... normal LEOF found
              go to 800
            else
              IRET_RDA = iret
              go to 999
            endif
          endif


             DO  IOB = 1,MAXWRDLVL
               IF((IOB .EQ. 11) .OR. (IOB .EQ. 12)) THEN
                 FLONELVL(IOB) = 0.0
               ELSE
                 FLONELVL(IOB) = FLOAT(LONELVL(IOB)) / 4096.0
               ENDIF
             ENDDO

C            ... is it within altitude bounds ? 
             paltm = float(lonelvl(14+1)) / 4096.0
             if((paltm .LT. altmlow) .OR.
     1          (paltm .GT. altmhigh)) then
               nofflvl = nofflvl + 1

C              ... this seems to be tossing all A/C obs with this test
C              ... because the press-alt was MISSING ...
               IF(MOD(nofflvl,100) .EQ. 0) THEN
C                ... PRINT A tossed REPORT EVERY ONCE IN A WHILE ...
C
          
                 if(LCHKPRNTQ) THEN
                   write(6,1161)nofflvl,paltm,altmlow,altmhigh
 1161              format(1h ,'readair: following is off-level-tossed ',
     1                        'aircft obs number=',I6,
     2                   /1h ,7X,'where paltm=',F9.1,'; altmlow=',F9.1,
     3                        'altmhigh=',F9.1)
                   WRITE(6,116)(LONELVL(I),I=1,MAXWRDLVL)
C ...  116              FORMAT((4Z17.16))
                 ENDIF

                 NAMSTN = IONELVLPAK(6)
                 IF(NAMSTN .EQ. 0) THEN
                   CNAMSTN(1:8) = ' '
                 ENDIF

                 obtime = FLONELVL(4)

                 IF(LCHKPRNTQ) THEN
                   write(6,117) cnamstn(1:8),obtime,FLONELVL(1),
     1                          FLONELVL(2), FLONELVL(7)
C ...  117         format(1h ,A8,2x,F7.2,'Z   LAT=',F7.2,' LONG=',F8.2,
C ...     2                       ' ELEV=',F6.0)
               
                   write(6,118)CHGTS(LVLIX),(FLONELVL(I),I=15,21)
C ...  118              format(2X,A6,3X,F7.0,4X,F5.1,
C ...     1                  4X,F5.1,2X,F6.0,4X,F5.0,4X,F6.0,2X,F8.0)
                 ENDIF
               ENDIF
               go to 200	!... acft not in lvl bounds; skip it
             endif

C 

C            ... next perform a bndry test on this lat/lon ...
             ALAT  = FLONELVL(1)
             ALONG = FLONELVL(2)
             IMAX = NINT(AIMAX)
             JMAX = NINT(AJMAX)

             call wibound(KRUN,KEIL,KLLGO2,LINTERPQQ,ALAT,ALONG,
     1                     FLD,IMAX,JMAX,HGTM,LINBOUNDQQ)

C            ... IF OUT-OF-AREA-BOUNDS AIRCRAFT, THEN 
C            ...    DO NOT DATABASE IT ...
             IF(.NOT. LINBOUNDQQ) THEN
               noutsider = noutsider + 1
               GO TO 200		!... skip this acrft obs
             ENDIF

C            ... otherwise, we got a keeper AIRCRAFT report, so 
C            ...     database it ...
             IREC = IREC + 1

            CALL SBYTESCCS(IONELVLPAK,LONELVL,NOFFSET,KBITPGRP,KPADBITS,
     1                   NGRPS2DO)

             IF((LONELVL(14+8) .NE. 0) .AND. (LINBOUNDQQ)) THEN
C               ... STASH INTO DATABASE ...
                IF(NOBSDB .LT. MAXOBS) THEN
                   NOBSDB = NOBSDB + 1
C                  ... TO SKIP THE 7TH WORD; STASH 1:6; 8:11
                   IDEST = 0
                   DO LW = 1,11
                     IF(LW .NE. 7) THEN
                       IDEST = IDEST + 1
                       IOBS2PK(IDEST,NOBSDB) = IONELVLPAK(LW)
                     ENDIF
                   ENDDO
                ELSE
                  WRITE(6,FMT='(1H ,''readair: FAILED ON INADEQUATE'',
     1                          '' SPACE ALLOCATED FOR DATABASE OF '',
     2                          ''OBS BY LVL'',
     2                  /1H ,7X,''NUMBER OF OBS LIMITED TO ='',I8)')
     A                  MAXOBS

                  IRET_RDA = 1
                  GO TO 999
                ENDIF
             ENDIF
                

             IF(MOD(IREC,10) .EQ. 0) THEN
C              ... PRINT A REPORT EVERY ONCE IN A WHILE ...
C
          
               if(LCHKPRNTQ) THEN
                 WRITE(6,116)(LONELVL(I),I=1,MAXWRDLVL)
  116            FORMAT((4Z17.16))
               ENDIF

               NAMSTN = IONELVLPAK(6)
               IF(NAMSTN .EQ. 0) THEN
                 CNAMSTN(1:8) = ' '
               ENDIF

               obtime = FLONELVL(4)

               IF(LCHKPRNTQ) THEN
                 write(6,117) cnamstn(1:8),obtime,FLONELVL(1),
     1                        FLONELVL(2), FLONELVL(7)
  117            format(1h ,A8,2x,F7.2,'Z   LAT=',F7.2,' LONG=',F8.2,
     2                     ' ELEV=',F6.0)
               
                 WRITE(6,1172)(IONELVLPAK(I),I=1,NWRDLVLPAK)
 1172            FORMAT((4Z17.16))

                 write(6,118)CHGTS(LVLIX),(FLONELVL(I),I=15,21)
  118            format(2X,A6,3X,F7.0,4X,F5.1,
     1                4X,F5.1,2X,F6.0,4X,F5.0,4X,F6.0,2X,F8.0)
                 write(6,119)LINBOUNDQQ
  119            FORMAT(1H ,'wibound: LINBOUNDQQ= .',L1,'.',
     1                 /1H   )
               ENDIF
             ENDIF

       go to 200		!... enddo by report

  800  continue
       WRITE(6,FMT='(1H ,''readair::rd_airc completed with '',
     1                   '' tossed off-lvl acft reports='',I8,
     2              /1h ,7X,''tossed out-of-area-bounds acft reports='',
     3                        I8,
     4              /1h ,7X,''Remaining aircraft report count='',I8,
     5              /1H ,7X,''STASHED-INTO-DATABASE COUNT ='',I8)')
     A         nofflvl,noutsider,IREC,NOBSDB


  999  continue
       RETURN
       END
