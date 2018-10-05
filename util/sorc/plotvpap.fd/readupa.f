      SUBROUTINE READUPA(LUNUPA,LVLDES,KRUN,ITOUT,HEDER,IRET_RED)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    READUPA     INITIALIZE OBS DATABASE AND READ UPA FILE
C   PRGMMR: SHIMOMURA        ORG: W/NP12     DATE: 97-02-04
C
C ABSTRACT: INITIALIZE OBS DATABASE AND READ UPA FILE AND STORE INTO
C   THE DATABASE IN COMMON.  AN ADAPTATION OF READPA.  CALLED FROM
C   MAPLOP IN PLOTPAP.
C
C PROGRAM HISTORY LOG:
C   97-01-31  ORIGINAL AUTHOR -- DAVID SHIMOMURA
C   97-02-04  SHIMOMURA -- ADDED CALL SETKEIL TO SET KLLGO2
C   97-02-13  SHIMOMURA -- ADDED MORE LEVELS FOR STRATOSPHERIC LEVELS
C
C USAGE:    CALL READUPA(LUNUPA,LVLDES,KRUN,ITOUT,HEDER,IRET_RED)
C
C   INPUT ARGUMENT LIST:
C     LUNUPA   - UNIT NUMBER OF INPUT UPAUPA FILE TO BE READ
C     LVLDES   - LEVEL DESIRED BY INDEX NUMBER
C     KRUN
C     ITOUT    - INTEGER FLAG WITH RANGE FROM 1 THRU 13 , USED
C              - TO DETERMINE TYPE OF MAP BACKGROUND DATA IS TO BE
C              - DISPLAYED ON. ITOUT IS A FUNCTION OF KRUN AND IS
C              - SET IN SUB KOPTN.
C
C   OUTPUT ARGUMENT LIST:
C     HEDER    - 50 WORD HEADER READ FROM LUNUPA WILL BE STORED HERE
C     IRET_RED    - TO MARK ERROR RETURNS
C     common /OBSLVLDB/
C     IOBS2PK    - BIG BIN FOR RESULTING OBS (10,NDATASIZ)
C     NDATASIZ    - J DIMENSION OF IOBS2PK (NO. OF OBS IT CAN HOLD+1)
C     NOBSDB     - NO. OF OBS PUT INTO IOBS2PK FROM LUNUPA FILE

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
C                                                 22-JAN-1997/dss
C     ... copied tesrdupa into tesrduupa

C                                                  6-JAN-1996/DSS
C     ... COPIED LARRY'S MAIN.F INTO tesrdupa
C
C     This is a test program to use the subroutine rd_uupa          
C     to return one upper air station at a time from the 
C     rd_uupa data file
C
C     Usage:
C     CALL READUPA(LUNUPA,LVLDES,KRUN,ITOUT,HEDER,IRET_RED)
C            RESULTING DATA ARE PUT INTO COMMON
C    
C     CALLS RD_UUPA TO ACTUALLY READ THE DATA FILE:
C            CALL rd_uupa(LUNUPA,LINITQ, LVLIX, HEDER, LONELVL, IRET )
C
C     where the input parameters are,
C
C            LUNUPA   -   Input file number
C            LINITQ   -   LOGICAL SWITCH 
C                      = .TRUE. ;  TO COMMAND RD_UUPA TO REWIND
C                            THE INPUT FILE AND READ THE FILE HEADER
C                            BEFORE READING THE OBSERVATIONS;
C                            SUBR RD_UUPA WILL RESET THAT SWITCH TO =.F.
C                            SO THAT ON SUBSEQUENT CALLS TO GET THE
C                            NEXT REPORTS, IT WILL NOT INITIALIZE.
C                            
C            LVLIX    -   MILLIBAR LEVEL BY INDEX NUMBER
C    
C     and the output parameters are,
C          
C            HEDER  -   The rd_uupa file header record.
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
C     CALL READUPA(LUNUPA,LVLDES,KRUN,ITOUT,HEDER,IRET_RED)
       integer      LUNUPA
       integer      lvldes
       INTEGER      KRUN
       INTEGER      ITOUT
       CHARACTER*8  HEDER(50)
       integer      IRET_RED
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

       REAL         FLD(6000)		!... BIG ENOUGH FOR ANY?

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
        LOGICAL  LNEWPA
        LOGICAL  LGRANP
        LOGICAL  LDEWDO
       

       SAVE
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       IRET_RED = 0
       NOBSDB = 0
       


       NGRPS2DO = MAXWRDLVL

           NDIX = 52
           IF(KRUN .EQ. 11) NDIX = 118
           IF(KRUN .EQ. 15) NDIX = 82
           IF(KRUN .EQ. 22) NDIX = 42
           IF(KRUN .EQ. 23) NDIX = 42
           AJMAX = FLOAT(NDIX)
C       --------------------------------------------------------------
C       ... to determine KEIL from ITOUT,  -- (SHOULD BE A FUNCTION)
      LNEWPA = .TRUE.
      LGRANP = .FALSE.
      
      IF(ITOUT .LT. 1) GO TO 702
      IF(ITOUT .GT. 13) GO TO 702
      GO TO 709
  702 CONTINUE
      PRINT 704, ITOUT
  704 FORMAT(1H , 'ERROR RETURN FROM READPA. GIVEN OUT-OF-RANGE ITOUT =
     1HEX', Z8, 2X, 'UNABLE TO READ ANY OBSERVATIONS' )
      IRET_RED = 1
      RETURN

  709 CONTINUE
      NPTYPS = 3
      LDEWDO = .FALSE.
      GO TO (710,720,730,740,750,760,770,780,702,790,795,798,798),ITOUT
  710 CONTINUE
      KEIL = 1
C     ...FOR LFM ...
      IF(LVLDES .GT. 8) GO TO 799
      IF(.NOT. LNEWPA) GO TO 799
      NPTYPS = 4
      LDEWDO = .TRUE.
      GO TO 799

  720 CONTINUE
      KEIL = 1
C     ...FOR LFM ....
      IF(LVLDES .GT.8) GO TO 799
      IF(.NOT. LNEWPA) GO TO 799
      NPTYPS = 4
      LDEWDO = .TRUE.
      GO TO 799

  730 CONTINUE
C     ...IQSY ON N HEMI ...
      KEIL = 2
      GO TO 799

  740 CONTINUE
      KEIL = 15
      AIMAX = 55.0
      AJMAX = 42.0
      GO TO 799

  750 CONTINUE
      KEIL = 3
      GO TO 799

  760 CONTINUE
      KEIL = 2
C     ...FOR SIRS PLOTTED CHART ...
      GO TO 799

  770 CONTINUE
C     ...INTIIALIZE FOR TROPIC PLOT ON MERCATOR
      KEIL = 6
      GO TO 799

  780 CONTINUE
C     ...INITIALIZE FOR LARGE 1/20M BACKGROUND.
      KEIL = 7
      GO TO 799

  790 CONTINUE
C     ...INITIALIZE FOR GENERALIZED PLOT ON NH 1/40M BACKGROUND.
      KEIL = 2
      GO TO 799

  795 CONTINUE
C     ...INITIALIZE FOR NH 1/60M BACKGROUND...
      AIMAX = 65.0
      AJMAX = 65.0
      KEIL = 14
      GO TO 799

  798 CONTINUE
C     ...GOES-TYPE DISPLAY.
      KEIL = 0
      GO TO 799
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
     X2X,'UNABLE TO SET ID FOR LATER READ OF LUNUPA')
C
  810 CONTINUE

C     .. ZERO THE BIG BIN
C     ...INITIALIZE COUNTER FOR BIG BIN OBS COUNT
C
C     *     *     *     *     *     *     *     *     *     *     *

C       --------------------------------------------------------------
C      ... DEFINE FOR BOUNDARY-TESTER ...
       DO  I = 1,6000
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

C      READ A REPORT FROM THE rd_uupa FILE
C
       PRINT *,' readupa: ready to read UPA via rd_uupa()'
       IRET = 0
C ....       LUNUPA = 41
       LINITQ = .TRUE.		!... RD_UUPA WILL RESET TO .F.
       IREC = 0
       IX = 0

C      ... TO INITIALIZE THE DATABASE TO ALL ZEROS,
       do  j = 1,NDATASIZ
         do  i = 1,LMTWRDPOB
           IOBS2PK(I,J) = 0
         ENDDO
       ENDDO
       NOBSDB = 0

       write(6,195)CHGTS(LVLIX)(1:6),LVLIX,KRUN,ITOUT,KEIL,AIMAX,AJMAX,
     1             KLLGO2
  195  format(1h ,'readupa: ready to start reading UPA for ',A6,
     1            'MBS with LVLIX=',I4,
     2       /1H ,7X,'KRUN=',I5,'; ITOUT=',I5,'; KEIL=',I5,
     3            '; AIMAX,AJMAX= (',F9.3,',',F9.3,')',
     4       /1H ,7X,'KLLGO2=',I4)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

  200  continue 	!... do until logical end of file ...
C
C         READ AN UPPER AIR REPORT FROM THE rd_uupa DATA SET
C
          CALL rd_uupa(LUNUPA,LINITQ,LVLIX, HEDER, LONELVL, IRET)
          if(iret .ne. 0) then
            if(iret .EQ. -1) then	!... normal LEOF found
              go to 800
            else
              IRET_RED = iret
              go to 999
            endif
          endif

C         ... otherwise, got a report, so process it ...
             IREC = IREC + 1
C 
             DO  IOB = 1,MAXWRDLVL
               IF((IOB .EQ. 11) .OR. (IOB .EQ. 12)) THEN
                 FLONELVL(IOB) = 0.0
               ELSE
                 FLONELVL(IOB) = FLOAT(LONELVL(IOB)) / 4096.0
               ENDIF
             ENDDO

C            ... next perform a bndry test on this lat/lon ...
             ALAT  = FLONELVL(1)
             ALONG = FLONELVL(2)
             IMAX = NINT(AIMAX)
             JMAX = NINT(AJMAX)

             call wibound(KRUN,KEIL,KLLGO2,LINTERPQQ,ALAT,ALONG,
     1                     FLD,IMAX,JMAX,HGTM,LINBOUNDQQ)

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
                  WRITE(6,FMT='(1H ,''TESRDUUPA: FAILED ON INADEQUATE'',
     1                          '' SPACE ALLOCATED FOR DATABASE OF '',
     2                          ''OBS BY LVL'',
     2                  /1H ,7X,''NUMBER OF OBS LIMITED TO ='',I8)')
     A                  MAXOBS

                  IRET_RED = 1
                  GO TO 999
                ENDIF
             ENDIF
                

             IF(MOD(IREC,10) .EQ. 0) THEN
C              ... PRINT A REPORT EVERY ONCE IN A WHILE ...
C
          

C ...               WRITE(6,116)(LONELVL(I),I=1,MAXWRDLVL)
C ...  116          FORMAT((4Z17.16))

               NAMSTN = IONELVLPAK(6)
               IF(NAMSTN .EQ. 0) THEN
                 CNAMSTN(1:8) = ' '
               ENDIF
C              write(6,117) cnamstn(1:8),FLONELVL(1),FLONELVL(2),
C    1                      FLONELVL(7)
  117          format(1h ,A8,' LAT ',F7.2,' LONG ',F8.2,' ELEV ',F6.0)
               
              WRITE(6,1172)(IONELVLPAK(I),I=1,NWRDLVLPAK)
 1172          FORMAT((4Z17.16))

C              write(6,118)CHGTS(LVLIX),(FLONELVL(I),I=15,21)
  118          format(2X,A6,3X,F7.0,4X,F5.1,
     1              4X,F5.1,2X,F6.0,4X,F5.0,4X,F6.0,2X,F8.0)
C              write(6,119)LINBOUNDQQ
  119          FORMAT(1H ,'wibound: LINBOUNDQQ= .',L1,'.',
     1               /1H   )
             ENDIF

       go to 200		!... enddo by report

  800  continue
       WRITE(6,FMT='(1H ,'' rd_uupa test completed with '',I8,
     1                   '' reports;'',
     2             /1H ,7X,''STASHED-INTO-DATABASE COUNT ='',I8)')
     A         IREC,NOBSDB




  999  continue
       RETURN
       END
