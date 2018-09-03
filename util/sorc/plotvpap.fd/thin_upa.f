      SUBROUTINE THIN_UPA(NDATA,ITM,JTM,IJTHK,IDIXAD,IDIXCO,NDIX,ITOUT)
C                                                  13-JAN-1997/DSS
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    THIN_UPA    THIN ADP DATA
C   PRGMMR: SHIMOMURA        ORG: W/NP12     DATE: 1997-01-13
C
C ABSTRACT: THINNING THE ADP DATA FIELD
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR  UNKNOWN
C   89-05-01  STEVE LILLY  ADD DOCUMENTATION BLOCK
C   93-05-03  LILLY CONVERT SUBROUTINE TO FORTRAN 77
C   97-01-13  SHIMOMURA:  COPIED THIK (MY TEMP CRAY VERSION 25-JUL-1996)
C                            AND CHANGED NAME FROM THIK TO THIN_UPA;
C                            CHGD INTERNALLY USED I/J TO JJJjjjIIIiii
C
C
C USAGE:    CALL THIN_UPA(NDATA,ITM,JTM,IJTHK,IDIXAD,IDIXCO,NDIX,ITOUT)
C
C   INPUT ARGUMENT LIST:
C     NDATA(ITM,JTM) - THE ADP DATA ARRAY TO BE THINNED
C                          WHERE (NDATA(I,J),I=1,ITM) CONTAINS ONE
C                          STATION'S OBSERVATIONAL DATA;
C                      THIS ARRAY MUST ALREADY BE IN SORTED ORDER;
C        GIVEN: IN NDATA(1,J) ...   
C              //NOVRLP/TYP/TOSSED/JG//IG/IF/JF/PRIOR//
C              // 16     6    1     9// 9  9  9   5  //
C              ... (ZONES FOR NOVRLP, TOSSED ARE USED LOCALLY)   
C        GIVEN: IN NDATA(2,J) ...
C              //                             /OFFTIME//
C              //                                 4   //

C     IJTHK    - I-SUBSCRIPT WITHIN ITM-WORDS WHEREIN THE DATA FOR 
C                    LOCATION-OF-STATION (IN UNITS OF GRID I/J)
C                    CAN BE FOUND
C
C     IDIXAD   - I = BOTTOM, J TOP POINTS TO THE FIRST OBSERVATION
C              - IN THE I-TH, J-STRIP     (made by DIXIE)
C     IDIXCO   - THE TOTAL NUMBER OF OBSERVATION
C                                         (made by DIXIE)
C     NDIX     - DIMENSION ARRAY IE; IDIXCO(NDIX)
C                                         (made by DIXIE)
C
C     ITOUT    - INTEGER FLAG WITH RANGE FROM 1 THRU 13 , USED
C              - TO DETERMINE TYPE OF MAP BACKGROUND DATA IS TO BE
C              - DISPLAYED ON. ITOUT IS A FUNCTION OF KRUN AND IS
C              - SET IN SUB KOPTN.

C     COMMON   - /WLONG0/ WLONG0
C                   AN ARGUMENT NEEDED ONLY FOR CALL GOESLL(WLONG0,...)
C                      IN SUB-SUBROUTINE PRTOSSED()
C
C   OUTPUT ARGUMENT LIST:
C     NDATA(ITM,JTM) - THE SAME ADP DATA ARRAY  WITH FLAGS CHANGED
C                        TO INDICATE THOSE WHICH HAVE BEEN THINNED
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  NAS
C
C$$$
C
      integer*8   NDATA(ITM,JTM)
      INTEGER     IDIXAD(NDIX)
      INTEGER     IDIXCO(NDIX)
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ... caution:  following work arrays dimensioned for ITM=3; so
C     ...           if your data base is bigger, then change sizes ...
      integer*8   NDATX(3)
      integer*8   NDATY(3)

C ...       I*2   NDPEA(12) ...
C ...       I*2   NDPEB(12)
C
C ...      EQUIVALENCE(NDATX(1),NDPEA(1),int4a(1))
C ...      EQUIVALENCE(NDATY(1),NDPEB(1),int4b(1))
C    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

C
      DATA   CLOSE           /10.0/
C     ...WHERE CLOSE IS IN DOTS  OF INTERVAL BETWN STNS FOR TOO CLOSE...
C
      DATA   DOTSGI          /37.5/
C
      DATA   JOFT            /2/
C     ...THE LONGWORD LOCATION (I*8 NDATX(JOFT) OR I*8 NDATY(JOFT)) 
C     ...    WHOSE LOW-ORDER 4-BITS (SEE MSKOFT) CONTAINS THE OFF-TIME
C     ...    PARAMETER IN THE DATA ARRAY...
C
C
      INTEGER   KTOSSBIT
      DATA      KTOSSBIT     / 41 /	!... IBSET(NDATY(IJTH),KTOSSBIT)


      DATA   MSKII           /Z'FF800000'/
      DATA   MSKJI           /Z'000001FF'/
      DATA   MSKOFT          /Z'0000000F'/
      DATA   MSKPJI          /Z'FFFFFFE0'/
      DATA   MSKPR           /Z'0000001F'/
      DATA   MSKR14          /Z'00003FFF'/

      INTEGER  MSKTYP
      DATA     MSKTYP          /X'0000FC0000000000'/
C ...      DATA   MSKTYP       /Z'0000FC00'/

      DATA   NOTJD           /Z'FFFFFE00'/
      DATA   NOTPR           /Z'FFFFFFE0'/
      INTEGER   NOTMSKI2HI
      DATA      NOTMSKI2HI        / X'0000FFFF' /

      INTEGER   NOTMSK_IGIFJF	     !... NOT(9+9+9 B5)
      DATA      NOTMSK_IGIFJF     / X'FFFFFFFF0000001F' /
      INTEGER   MSK_JF			     !... 9 B5
      DATA      MSK_JF            / X'0000000000003FE0' /

      INTEGER   MSK_IGIF          !...1234567812345678...
      DATA      MSK_IGIF          / X'00000000FFFFC000' /


      INTEGER   MSKLOCN
      DATA      MSKLOCN       / X'000001FFFFFFFFE0'/ 	!... JI + PJI

      INTEGER   MSK16B
      DATA      MSK16B        / X'000000000000FFFF' /
      INTEGER   MSK32B
      DATA      MSK32B        / X'00000000FFFFFFFF' /
      INTEGER   MSK48B
      DATA      MSK48B        / X'0000FFFFFFFFFFFF' /
C
      DATA   SCALE            / 1.953124E-3 /
C     ...WHICH IS RECIPROCAL OF 512 TO SHIFT BINARY PT 9 BITS...

      INTEGER   IACC
      INTEGER   LONGWRD
      INTEGER   IGIF_MOVD
      INTEGER   JG_MOVD

      integer   nduptossd
      integer   ntottossd
      integer   jptr_enddata
      INTEGER   LCKPT

      INTEGER   LOCN_A
      INTEGER   LOCN_B

      CHARACTER*5  CMD
      LOGICAL      LADVANCE
      LOGICAL      LTOSS1
      LOGICAL      LOPNL
C
       SAVE
      
C     . . . . . .   S T A R T   . . . . . . . . . . . . . . . . . . .
C
      ntottossd = 0
      LOPNL = .TRUE.
C     ...IF YOU WANT TO PRINT EVERY TOSSED STN, SET LOPNL TO FALSE...
      LTOSS1 = .TRUE.
      IKDOT = 34
      JKDOT = 20
C... WHICH INITIALIZES BOUNDS OF PLOTTING MODEL TO IQSY SIZE....

   69 CONTINUE

C ...      GO TO (71,71,73,74,75,73,77,78,70,73,79,82,82),ITOUT
C ...         ...  1  2  3  4  5  6  7  8  9 10 11 12 13 ...

C     . . . . . . . . . . . . .  CASE ITOUT . . . . . . . . . . . . .
      IF((ITOUT .EQ. 1) .OR. 
     1   (ITOUT .EQ. 2)) THEN
C ...                    71   CONTINUE
        KEIL = 1
        IKDOT = 50
        JKDOT = 24
        LTOSS1 = .FALSE.

      ELSE IF((ITOUT .EQ. 3) .OR. 
     1        (ITOUT .EQ. 6) .OR. 
     2        (ITOUT .EQ. 10)) THEN
C ...                          73   CONTINUE
        KEIL = 2

      ELSE IF(ITOUT .EQ. 4) THEN 
C ...                       74   CONTINUE
        KEIL = 15
        IKDOT = 50
        JKDOT = 24
        LTOSS1 = .FALSE.

      ELSE IF(ITOUT .EQ. 5) THEN
C ...                       75   CONTINUE
        KEIL = 3

      ELSE IF(ITOUT .EQ. 7) THEN
C ...                       77   CONTINUE
        KEIL = 6
        IKDOT = 20
        JKDOT = 34

      ELSE IF(ITOUT .EQ. 8) THEN
C ...                       78   CONTINUE
        KEIL = 7
        IKDOT = 50
        JKDOT = 24

      ELSE IF(ITOUT .EQ. 9) THEN
        KEIL = 4

      ELSE IF(ITOUT .EQ. 10) THEN
        keil = 2

      ELSE IF(ITOUT .EQ. 11) THEN
C ...                        79   CONTINUE
        KEIL = 14

      ELSE IF((ITOUT .EQ. 12) .OR.
     1        (ITOUT .EQ. 13)) THEN
C ...                          82   CONTINUE
        DOTSGI = 50.0
        KEIL = 0

      ELSE
C       ... THIS VALUE OF ITOUT IS UNKNOWN ...
C       ...      IF(ITOUT .LT. 1) GOTO 66
C       ...      IF(ITOUT .GT.13) GOTO 66

C       WRITE(6,68) ITOUT
   68   FORMAT(1H ,/1H , 'THIN_UPA: ERRONEOUS ITOUT WAS GIVEN.',
     1        /1H ,7X,'GIVEN OUT-OF-RANGE ITOUT = HEX ',Z16.16,
     2        /1H ,7X,'... GOING ON... WITH DEFAULT KEIL=4 ')

C       ...WHICH WILL SET FOR PEATMOS 2-PANEL FOR UNKNOWN ITOUT
C ...                          70   CONTINUE
        KEIL = 4
      ENDIF

      GO TO 90
C
   90 CONTINUE
      IJTH = IJTHK
C
C     ******************************************************************
C
C                           PART ONE
C
C     REFORMAT I AND J.
C
C     ******************************************************************
C
      DO 137 JA=1,JTM
C       ...FETCH ITM WORDS OF ONE DATA POINT...
        jptr_enddata = JA
        DO 132 I=1,ITM
          IF(NDATA(IJTH,JA) .EQ. 0) then
            write(6,131)IJTH,jptr_enddata,KEIL
  131       format(1H ,'thin_upa: PART-1 located terminating zero ',
     1                 'word at NDATA(',I4,',',I6,')',
     2            /1H ,7X,'KEIL=',I4)
            GO TO 139			!... JUMP OUT OF DO
          endif
          NDATX(I) = NDATA(I,JA)
  132   CONTINUE

          
C       ...REFORMAT THE I/J WORD AND ZERO OVERLAP COUNT...

C       . . . . . . . . . . . . . . . . .

C       ... GIVEN:       // ... /JG//IG/IF/JF/PRIOR//
C                        // 23    9// 9  9  9  5   //BITS
C       ... REARRANGED:  // ... /JG//JF/IG/IF/PRIOR//
C                        // 23    9// 9  9  9  5   //BITS

        LONGWRD   = NDATX(IJTH)
        IACC      = IAND(LONGWRD,NOTMSK_IGIFJF)
C       ... WHICH ERASED IGRID+IFRACT+KFRACT AND PRESERVED THE REST

        IGIF_MOVD = ISHFT(IAND(LONGWRD,MSK_IGIF),-9)
        IACC      = IOR(IACC,IGIF_MOVD)

        JF_MOVD   = ISHFT(IAND(LONGWRD,MSK_JF),+18)
C ...        JF_MOVD   = ISHFT(IAND(LONGWORD,MSK_JF),+18) ...
        IACC      = IOR(IACC,JF_MOVD)
        
        IACC      = IBCLR(IACC,KTOSSBIT)	!... INIT TO NOT-TOSSED

C ...   ...  NDPEA(1) = 0    !... zero hi-order 16bits for ovrlap count
        NDATX(IJTH) = IAND(IACC,MSK48B)
C       . . . . . . . . . . . . . . . . .
        NDATA(IJTH,JA) = NDATX(IJTH)
C
C     I/J PORTION OF THE DOUBLE WORD IS NOW JG||JF||IG||IF WHERE THERE
C     ARE 9 BITS FOR INTEGER AND 9 BITS FOR FRACTION.
C
  137 CONTINUE
C     ...        ... WHERE 137 CONTINUE IS ENDDO ON  JA=1,JTM
C
  139 CONTINUE
C
C------------------------------------------------
      lckpt = 139
C     ... I am looking for where the extraneous zero word is being put
      DO  JA=1,JTM
        jofzero = JA
        IF(NDATA(IJTH,JA) .EQ. 0) then
            write(6,FMT='(1H ,''thin_upa: at LCKPT='',I5,
     1                  '', found terminating zero word at NDATA('',I4,
     2                  '','',I6,'')'')')
     A              LCKPT,IJTH,jofzero
            GO TO 1393			!... JUMP OUT OF DO
        ENDIF
      ENDDO
 1393 CONTINUE
C------------------------------------------------
C     ******************************************************************
C
C                         PART TWO
C
C     TOSS DUPLICATE REPORTS (SAME I/J AND SAME PRIORITY).
C
C     ******************************************************************
C
      nduptossd = 0
      DO 177 JA=1,JTM
C       ...FETCH ITM WORDS OF STATION A...
        DO  I=1,ITM
          NDATX(I) = NDATA(I,JA)
        enddo
C
        JB = JA + 1
        IF(NDATA(IJTH,JA) .EQ. 0) GO TO 180 	!... ended toss-duplic

C       ...TEST WHETHER STATION A IS ALREADY TOSSED...


        IF(BTEST(NDATX(IJTH),KTOSSBIT)) THEN
           GO TO 177			!... MARKED AS "TOSSED", SO SKIP
        ENDIF

  144   CONTINUE
C       ... FETCH  PACKED I/J OF POINT A...
        LOCN_A = IAND(NDATX(1),MSKLOCN)

        IF(JB .GE. JTM) GO TO 177

C       ...FETCH ITM WORDS OF STN B...
        DO 146 I=1,ITM
          IF(NDATA(IJTH,JB) .EQ. 0) GO TO 177
          NDATY(I) = NDATA(I,JB)
  146   CONTINUE

C       ...GET PACKED I,J OF POINT B...
        LOCN_B = IAND(NDATY(1),MSKLOCN)

        IF(LOCN_A .NE. LOCN_B) THEN
          GO TO 177		!... NOT AT SAME LOCN, SO GO NEXT PT-A
        ENDIF
C       ... OTHERWISE, COMES HERE IF DUPLICATE I/J.  ELIMINATE ONE...

        IPRIB = IAND(NDATY(IJTH),MSKPR)

        IF(IPRIA - IPRIB) 170,156,175
  156   CONTINUE
C         ...COMES HERE IF SAME I/J AND SAME PRIORITY...
          IOFFTA = IAND(NDATX(JOFT),MSKOFT)

          IOFFTB = IAND(NDATY(JOFT),MSKOFT)

        IF(IOFFTA .GT. IOFFTB) GO TO 175

  170   CONTINUE
C       ... COMES HERE IF(IPRIA .LT. IPRIB) ... . . . . . . . . .
C       ...TOSS POINT B -- I.E., SET THE KTOSSBIT ...


        NDATY(IJTH) = IBSET(NDATY(IJTH),KTOSSBIT)
        NDATA(IJTH,JB) = NDATY(IJTH)

        nduptossd = nduptossd + 1
        ntottossd = ntottossd + 1

        JSUB = JB
        JCNT = 1
        CALL PRTOSSED(ITM,NDATY,NDATA,KEIL,JSUB,JCNT,JTM,IDIXAD,
     1              IDIXCO,ITOUT,LOPNL)
  171   CONTINUE
        JB = JB + 1
        GO TO 144

  175 CONTINUE
C       ... COMES HERE IF(IPRIA .GT. IPRIB) ...
C       ... TOSS POINT A -- I.E., SET THE KTOSSBIT ...


        NDATX(IJTH) = IBSET(NDATX(IJTH),KTOSSBIT)
        NDATA(IJTH,JA) = NDATX(IJTH)

        nduptossd = nduptossd + 1
        ntottossd = ntottossd + 1

        JCNT = 1
        JSUB = JA
        CALL PRTOSSED(ITM,NDATY,NDATA,KEIL,JSUB,JCNT,JTM,IDIXAD,
     1              IDIXCO,ITOUT,LOPNL)

  176   CONTINUE
  177 CONTINUE
C     ...       . . . . . WHERE 177 IS ENDDO ON JA=1,JTM  . . . . . .
  180 CONTINUE
C
C     DUPLICATES HAVE BEEN TOSSED.
C
      WRITE(6,620)nduptossd,ntottossd
  620 FORMAT(1H ,'THIN_UPA: AT END OF STEP 1 -- TOSSED ',I5,
     1               ' DUPLICATES',
     2      /1H ,7X,'TOTAL OF TOSSED = ',I5)

C------------------------------------------------
      lckpt = 180
C     ... I am looking for where the extraneous zero word is being put
C ...      DO  JA=1,JTM
C ...        jofzero = JA
C ...        IF(NDATA(IJTH,JA) .EQ. 0) then
C ...            write(6,FMT='(1H ,''thin_upa: at LCKPT='',I5,
C ...1                  '', found terminating zero word at NDATA('',I4,
C ...2                  '','',I6,'')'')')
C ...A              LCKPT,IJTH,jofzero
C ...       GO TO 1803			!... JUMP OUT OF DO
C ...   ENDIF
C ... ENDDO
C ... 1803 CONTINUE
C------------------------------------------------

C
C     COUNT OVERLAPS AND TOSS ANY LOW QUALITY STATIONS OVERLAPPING A
C     HIGHER QUALITY STATION.
C
C     ...INITIALIZE CONSTANTS...
      delgrdi = FLOAT(IKDOT) / DOTSGI
      delgrdj = FLOAT(JKDOT) / DOTSGI
      CLOSE2 = CLOSE / DOTSGI
      CLOSE2 = CLOSE2 * CLOSE2
C     ...FOR TEST ON DISTANCE-SQUARED FOR NEARLY SAME LOCATION...
      WRITE(6,635)  CLOSE2
  635 FORMAT(1X,5X, 'CLOSE2 ... DISTANCE LIMIT = ', E11.4)
      MAXLAP = 0
C     ...FOR THE MAX OVERLAP FOUND FOR ANY POINT...
      DO 280 JA=1,JTM
        NOVRLP = 0
C       ...FETCH ITM WORDS OF POINT A...
        DO 210 I=1,ITM
          IF(NDATA(IJTH,JA) .EQ. 0) GO TO 290
          NDATX(I) = NDATA(I,JA)
  210   CONTINUE
C
        IPRIA = IAND(NDATX(IJTH),MSKPR)
     
        IF(BTEST(NDATX(IJTH),KTOSSBIT)) THEN
          GO TO 280		!... THIS PT_A IS MARKED "TOSSED"; SKIP
        ENDIF


        NDATX(IJTH) = IAND(NDATX(IJTH),MSK48B)		!... NOVLP=0

        NDATA(IJTH,JA) = NDATX(IJTH)

        CMD(1:5) = 'COUNT'
        LADVANCE = .FALSE.

        CALL OVRLPUPA(CMD,delgrdi,delgrdj,NDATA,ITM,JTM,IJTH,
     1            JA,NDATX,NDATY,IPRIA,
     2            IDIXAD,IDIXCO,NDIX,CLOSE2,LOPNL,
     3            ITOUT,KEIL,
     4            NOVRLP,ntottossd,LADVANCE)

        IF(LADVANCE) GOTO 280
C ...        IF(CMD=='DECRE') GOTO 366		!... WARNING fr compiler
C ...        CMD IS NOT CHANGED W/I OVERLAPS(); SO 
C ...           CMD WILL BE ='COUNT' HERE
C
C       ...RETURN FROM SECTION 500 WHEN CMD == 'COUNT' ...
  266   CONTINUE
C       ...STORE THE OVERLAP COUNT FOR POINT A...


        NDATX(IJTH) = IAND(NDATX(IJTH),MSK48B)		!... NOVLP=0
        MQ = ISHFT(IAND(NOVRLP,MSK16B),48)
        NDATX(IJTH) = IOR(MQ,NDATX(IJTH))  	!... NDPEA(1)=NOVRLP

        NDATA(IJTH,JA) = NDATX(IJTH)
        IF(NOVRLP .GT. MAXLAP) MAXLAP = NOVRLP
C       ...I HAVE FINISHED ONE POINT A.  GO GET ANOTHER...
  280 CONTINUE
C     ... WHERE 280 IS ENDDO ON  JA=1,JTM  . . . . . . . . . . . . . .
C
  290 CONTINUE
      WRITE(6,600) MAXLAP,NTOTTOSSD
  600 FORMAT(1H ,'thin_upa:AT END OF PART 3  OF THINNER.  ',
     1           'MAX OVERLAP COUNT=',I5,
     2      /1H ,7X,'TOTAL-TOSSED COUNT=',I5,/1H  )
C
C------------------------------------------------
      lckpt = 290
C     ... I am looking for where the extraneous zero word is being put
C ...      DO  JA=1,JTM
C ...        jofzero = JA
C ...        IF(NDATA(IJTH,JA) .EQ. 0) then
C ...            write(6,FMT='(1H ,''thin_upa: at LCKPT='',I5,
C ...1                  '', found terminating zero word at NDATA('',I4,
C ...2                  '','',I6,'')'')')
C ...A              LCKPT,IJTH,jofzero
C ...       GO TO 2903			!... JUMP OUT OF DO
C ...   ENDIF
C ... ENDDO
C ... 2903 CONTINUE
C------------------------------------------------

C     CALL PDUMP(NDATA(1,1),NDATA(6,600),0)

C     ... TO THIN OVERLAPPING STATIONS...
      KOVCRI = MAXLAP - 1
      IF(KOVCRI .GE. 0) GO TO 310
C     ...AT THIS POINT KOVCRI IS INITIALIZED...
      KOVCRI = 0
  310 CONTINUE

C     ... LOGIC ERROR WITHIN FOLLOWING LOOP IN WHICH A ZERO WORD IS
C     ...   BEING INTRODUCED IN NDATA(1,X)

      NTOSS = 0
  311 CONTINUE			!... top of loop on KOVCRI * * * * * * *
C     ...INITIALIZE THE SKIP COUNT...
      KIPS = 1
  315 CONTINUE
      NTOSD = 0
      J1 = KIPS
      DO 380 JA=J1,JTM,KIPS
C       ...FETCH ITM WORDS OF POINT A...
        DO 317 I=1,ITM
          IF(NDATA(IJTH,JA) .EQ. 0) GO TO 385
          NDATX(I) = NDATA(I,JA)
  317   CONTINUE

C       ...DO NOT THIN LAND STATIONS OF TYPE ONE...

        NTYPE = IAND(NDATX(IJTH),MSKTYP)
        NTYPE = ISHFT(NTYPE,-42)

        IF(NTYPE .NE. 1) GO TO 320
        IF(.NOT. LTOSS1) GO TO 380
  320   CONTINUE
        IPRIA = IAND(NDATX(IJTH),MSKPR)

        IF(IPRIA) 322,380,322
  322   CONTINUE
C ...   ...   NOVRLP = NDPEA(1)
        NOVRLP = IAND(ISHFT(NDATX(IJTH),-48),MSK16B)
C       ... CAN NOVRLP EVER BE NEGATIV-VALUED ????

        IF(NOVRLP .EQ. 0) GO TO 380
        IF(NOVRLP .LE. KOVCRI) GO TO 380
C
C       COMES THIS WAY IF POINT A HAS MORE OVERLAPS THAN KOVCRI.
C
C       TOSS POINT A BY setting ktossbit; AND zero ITS OVERLAP COUNT.


        NDATX(IJTH) = IAND(NDATX(IJTH),MSK48B) 		!... NOVERLAPS=0
        NDATX(IJTH) = IBSET(NDATX(IJTH),KTOSSBIT)	!... TOSSED
        ntottossd = ntottossd + 1
        NDATA(IJTH,JA) = NDATX(IJTH)

        JSUB = JA
        JCNT = 1
        CALL PRTOSSED(ITM,NDATY,NDATA,KEIL,JSUB,JCNT,JTM,IDIXAD,
     1              IDIXCO,ITOUT,LOPNL)
  328   CONTINUE
        NTOSD = NTOSD + 1
C
C       SET CMD TO PERFORM REDUCE-OVERLAP-COUNT OF ALL STATIONS B WHICH
C         OVERLAP STATION A.
C
        CMD(1:5) = 'DECRE'
        LADVANCE = .FALSE.

        CALL OVRLPUPA(CMD,delgrdi,delgrdj,NDATA,ITM,JTM,IJTH,
     1            JA,NDATX,NDATY,IPRIA,
     2            IDIXAD,IDIXCO,NDIX,CLOSE2,LOPNL,
     3            ITOUT,KEIL,
     4            NOVRLP,ntottossd,LADVANCE)


        IF(LADVANCE) GOTO 380
C ...        IF(LADVANCE) GOTO 280 	!... WARNING fr compiler
C ...   ...  IF(CMD== 'COUNT') GOTO 266 	!... WARNING fr compiler
C       ...  CMD IS NOT CHANGED W/I OVRLPUPA(); SO CMD=='DECRE' HERE.
C
C       ... SECTION 500 RETURNS TO 366 WHEN CMD = 'DECRE' AND 
C       ...    AFTER DOING ALL POINTS B.
C
  366   CONTINUE
  380 CONTINUE
C     ... WHERE 380 IS ENDDO  ON JA=J1,JTM,KIPS . . . . . . . . . . .

C
  385 CONTINUE
      WRITE(6,665) KOVCRI,KIPS,NTOSD,ntottossd
  665 FORMAT(1h ,'thin_upa:THIN CYCLE WITH OVERLAP CRITERION=', I3,
     1      /1h ,7X,'SKIP CONSTANT=',I3,'; NO. OF OBS TOSSED=', I4,
     2      /1h ,7X,'total-tossed count=',I5,/1H  )
C------------------------------------------------
      lckpt = 385
C     ... I am looking for where the extraneous zero word is being put
C ...      DO  JA=1,JTM
C ...        jofzero = JA
C ...        IF(NDATA(IJTH,JA) .EQ. 0) then
C ...            write(6,FMT='(1H ,''thin_upa: at LCKPT='',I5,
C ...1                  '', found terminating zero word at NDATA('',I4,
C ...2                  '','',I6,'')'')')
C ...A              LCKPT,IJTH,jofzero
C ...       GO TO 3853			!... JUMP OUT OF DO
C ...   ENDIF
C ... ENDDO
C ... 3853 CONTINUE
C------------------------------------------------

      NTOSS = NTOSS + NTOSD
      KIPS = KIPS - 1
      IF(KIPS .GT. 0) GO TO 315
      IF(KOVCRI .LE. 0) GO TO 400
      KOVCRI = KOVCRI - 1
      GO TO 311
  400 CONTINUE
      WRITE(6,611) NTOSS,NTOTTOSSD
  611 FORMAT(1H ,'thin_upa:COUNT OF OBS TOSSED IN PART(4) =',I5,
     1      /1h ,7X,'AT END OF thin_upa, TOTAL-TOSSED COUNT=',I5,/1H  )
C
C------------------------------------------------
      lckpt = 400
C     ... I am looking for where the extraneous zero word is being put
      DO  JA=1,JTM
        jofzero = JA
        IF(NDATA(IJTH,JA) .EQ. 0) then
            write(6,FMT='(1H ,''thin_upa: at LCKPT='',I5,
     1                  '', found terminating zero word at NDATA('',I4,
     2                  '','',I6,'')'')')
     A              LCKPT,IJTH,jofzero
            GO TO 4003			!... JUMP OUT OF DO
        ENDIF
      ENDDO
 4003 CONTINUE
C------------------------------------------------
      RETURN
      END

      SUBROUTINE OVRLPUPA(CMD,delgrdi,delgrdj,NDATA,ITM,JTM,IJTH,
     1            JA,NDATX,NDATY,IPRIA,
     2            IDIXAD,IDIXCO,NDIX,CLOSE2,LOPNL,
     3            ITOUT,KEIL,
     4            NOVRLP,ntottossd,LADVANCE)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    OVRLPUPA    THIN ADP DATA
C   PRGMMR: LILLY            ORG: W/NMC412   DATE: 93-05-02
C
C ABSTRACT: THINNING THE ADP DATA FIELD ...
C   ONLY FOR ONE GIVEN STN-A, SCANS THE OBSERVATIONS ARRAY TO FIND ALL
C   NEARBY STN-B'S; PERFORMS THE COUNTING-OVERLAPS/SCRUB-TOO-CLOSE
C   FUNCTION OR THE DECREMENTING-OVERLAP-COUNTS-STN-B'S FUNCTION 
C   AS DETERMINED BY THE GIVEN SWITCH VALUE IN "CMD".  THAT FUNCTION-
C   CONTROLLING SWITCH IS READ-ONLY;  NOT RESET WITHIN THIS SUBR.
C   Called from thin_upa (loop 280) in 'COUNT' mode;
C   Called from thin_upa (loop 380) in 'DECRE' mode.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR  UNKNOWN
C   89-05-01  STEVE LILLY  ADD DOCUMENTATION BLOCK
C   93-05-02  LILLY CONVERT SUBROUTINE TO FORTRAN 77
C   96-07-25  SHIMOMURA - CONVERT TO CRAY FORTRAN
C
C USAGE:    CALL OVRLPUPA(CMD,delgrdi,delgrdj,NDATA,ITM,JTM,IJTH,
C     1            JA,NDATX,NDATY,IPRIA,
C     2            IDIXAD,IDIXCO,NDIX,CLOSE2,LOPNL,
C     3            ITOUT,KEIL,
C     4            NOVRLP,ntottossd,LADVANCE)
C
C   INPUT ARGUMENT LIST:
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C
C USAGE:    CALL OVRLPUPA(CMD,delgrdi,delgrdj,NDATA,ITM,JTM,IJTH,
C     1            JA,NDATX,NDATY,IPRIA,
C     2            IDIXAD,IDIXCO,NDIX,CLOSE2,LOPNL,
C     3            ITOUT,KEIL,
C     4            NOVRLP,ntottossd,LADVANCE)
      CHARACTER*5  CMD
      REAL         DELGRDI,DELGRDJ
      integer*8    NDATA(ITM,JTM)
      INTEGER      IJTH

      INTEGER      JA
      integer*8    NDATX(3)
      integer*8    NDATY(3)
      INTEGER      IPRIA
C
      INTEGER      IDIXAD(NDIX)
      INTEGER      IDIXCO(NDIX)
      REAL         CLOSE2
      LOGICAL      LOPNL

      INTEGER      ITOUT
      INTEGER      KEIL

      INTEGER      NOVRLP
      INTEGER      NTOTTOSSD
      LOGICAL      LADVANCE

C
C
      DATA   MSKPR           /Z'0000001F'/
      DATA   MSKR18          /Z'0003FFFF'/
      DATA   NOTPR           /Z'FFFFFFE0'/

      INTEGER  NOTMSKI2HI
      DATA     NOTMSKI2HI    / X'0000FFFF' /

      INTEGER  MSK16B
      DATA     MSK16B        / X'0000FFFF' /
      INTEGER  MSK32B
      DATA     MSK32B        / X'FFFFFFFF' /
      integer  msk48b
      data     msk48b        / X'0000FFFFFFFFFFFF' /
C
      DATA   SCALE           /1.953124E-3/
C     ...WHICH IS RECIPROCAL OF 512 TO SHIFT BINARY PT 9 BITS...

      INTEGER   KTOSSBIT
      DATA      KTOSSBIT     / 41 /	!... IBSET(NDATY(IJTH),KTOSSBIT)

       INTEGER     III_IFR_A,JJJ_JFR_A
       INTEGER     III_IFR_B,JJJ_JFR_B

       INTEGER     NJROWS
       INTEGER     JB
       INTEGER     JBPTR

       INTEGER      IACC
       CHARACTER*8  CACC
       EQUIVALENCE (IACC,CACC)
       
       INTEGER      MQ
       CHARACTER*8  CMQ
       EQUIVALENCE (MQ,CMQ)

       SAVE
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C      . . . .   S T A R T    S U B 5 0 0    . . . . . . . . . . . .
C
       NJROWS = NDIX - 1

C      ... GET X,Y LOCATION OF STN-A ...
       III_IFR_A = IAND((ISHFT(NDATX(1), -5)),MSKR18)
       JJJ_JFR_A = IAND((ISHFT(NDATX(1),-23)),MSKR18)        

       STNI = FLOAT(III_IFR_A) * SCALE
       STNJ = FLOAT(JJJ_JFR_A) * SCALE

      AIRT = STNI + delgrdi
      AILEF = STNI - delgrdi
      AJHI = STNJ + delgrdj
      AJLO = STNJ - delgrdj
      JLOS = AJLO
      JHIS = AJHI

      DO 560 JSTRIP=JLOS,JHIS
        IF(JSTRIP .LT. 1) GO TO 560
        IF(JSTRIP .GE. NJROWS) GO TO 564
        NSTRIP = IDIXCO(JSTRIP)
C
        IF(NSTRIP .EQ. 0) GO TO 560
        JB = IDIXAD(JSTRIP)

        DO 555 JBR=1,NSTRIP
          JBPTR = JB + JBR - 1
          IF(JBPTR .EQ. JA) GO TO 555

C         ...FETCH ITM WORDS OF POINT B...
          DO 519 I=1,ITM
            IF(NDATA(IJTH,JBPTR) .EQ. 0) GO TO 564
            NDATY(I) = NDATA(I,JBPTR)
  519     CONTINUE

          IF(BTEST(NDATY(IJTH),KTOSSBIT)) THEN
            GO TO 555		!... THIS B WAS ALREADY TOSSED; SKIP
          ENDIF

          IPRIB = IAND(NDATY(IJTH),MSKPR)

C ...          IF(IPRIB .EQ. 0) GO TO 550	!... JB++; NEXT B-STN
C ...          ... WHICH WAS OLD WAY TO TEST FOR "TOSSED"

C         ... GET X,Y LOCATION OF STN-B ...
          III_IFR_B = IAND((ISHFT(NDATY(1), -5)),MSKR18)
          JJJ_JFR_B = IAND((ISHFT(NDATY(1),-23)),MSKR18)        

          BSTNI = FLOAT(III_IFR_B) * SCALE
          BSTNJ = FLOAT(JJJ_JFR_B) * SCALE

C         ...BOX TEST...  WE ARE LOOKING FOR ALL B-STN'S WITHIN THE BOX

          IF(AILEF .GT. BSTNI) GO TO 555  	!... JB++; NEXT STN
          IF(AIRT  .LT. BSTNI) GO TO 560	!... JUMP TO NEXT STRIP
          IF(AJLO  .GT. BSTNJ) GO TO 555	!... JB++; NEXT STN
          IF(AJHI  .LT. BSTNJ) GO TO 555	!... B-STN IS ABOVE BOX?
C         ... OTHERWISE, WITHIN BOX ... SO CONTINUE ...
C
C         ... CMD='COUNT'; ON FIRST CALL; FROM COUNTING-OVERLAPS SECTION
C         ...    ='DECRE'; ON 2ND CALL; TO DECREMENT OVERLAP-COUNTS
C                                     OF STN-B'S NEAR THIS TOSSED STN-A
C
          IF(CMD .EQ. 'DECRE') GO TO 570   	!... JUMP TO DECR B-OVR

C         . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C         ... PERFORMS THIS PARA ONLY IF CMD='COUNT';
C                  WHICH WAS CALL OVRLPUPA OUT OF COUNTING-OVERLAPS SECT

C         ...THE PART (3) LOGIC IS IN HERE AT 522 SECTION...
C ...     ...     IF(IPRIA - IPRIB) 533,538,534 ...

          IF(IPRIA .EQ. IPRIB) THEN
C           ...EQUAL QUALITY STNS -- TEST FOR TOO CLOSE...
            DI = BSTNI - STNI
            DJ = BSTNJ - STNJ
            DISTSQ = (DI * DI) + (DJ * DJ)
            IF(DISTSQ .LE. CLOSE2) THEN
              IACC = NDATX(2)
              MQ   = NDATY(2)
              WRITE(6,630) DISTSQ, NDATX(1),CACC(1:6),NDATX(2),
     1                             NDATY(1),CMQ(1:6),NDATY(2)
  630         FORMAT(1H ,'THIN_UPA::OVRLPUPA: SCRUBBED PT B FOR TOO ',
     1                   'CLOSE; DISTSQ = ', E11.4,
     2              /1h ,' A= ', Z16,1X,A6,1X,Z4
     3                   '; B= ',Z16,1X,A6,1X,Z4)
              GO TO 533
C             ...WHICH SCRUBS PT B FOR TOO-CLOSE STNS...

            ENDIF
C           ... OTHERWISE, NOT TOO-CLOSE, SO ...
            NOVRLP = NOVRLP + 1
            GO TO 555   			!... JB++;

          ELSE IF(IPRIA .LT. IPRIB) THEN
            GO TO 533		!... TO SCRUB PT-B

          ELSE
C           ... IPRIA  .GT. IPRIB, SO
C           ... TOSS POINT_A, ADVANCING TO ANOTHER POINT A...


            NDATX(IJTH) = IBSET(NDATX(IJTH),KTOSSBIT)	!... TOSSED A
            NDATA(IJTH,JA) = NDATX(IJTH)
            NTOTTOSSD = NTOTTOSSD + 1

            XI = STNI
            XJ = STNJ
            JSUB = JA
            JCNT = 2
            CALL PRTOSSED(ITM,NDATY,NDATA,KEIL,JSUB,JCNT,JTM,IDIXAD,
     1                  IDIXCO,ITOUT,LOPNL)

            LADVANCE = .TRUE.
            GO TO 564 		!... TO EXIT OUT OF OVRLPUPA
          ENDIF
C          . . . . . . . 

  533     CONTINUE
C         ...SCRUB THIS B, THEN GO GET ANOTHER...
       

          NDATY(IJTH) = IBSET(NDATY(IJTH),KTOSSBIT)	!... TOSSED B
          NDATA(IJTH,JBPTR) = NDATY(IJTH)
          NTOTTOSSD = NTOTTOSSD + 1

          XI = BSTNI
          XJ = BSTNJ
          JSUB = JBPTR
          JCNT = 2
          CALL PRTOSSED(ITM,NDATY,NDATA,KEIL,JSUB,JCNT,JTM,IDIXAD,
     1                IDIXCO,ITOUT,LOPNL)
  536     CONTINUE
          GO TO 555 			!... JB++;

C       
C         ...    END OF CMD='COUNT'  PARAGRAPH ...
C         . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  570     CONTINUE
C         ... COMES HERE ONLY ON CMD= 'DECRE'  RUN ...
C         ...    FOR DECREMENTING-OVERLAPS-SPECIFIC PARAGRAPH
C
C         ... POINT B IS WITHIN THE BOX ABOUT A...
C

          NLAPB = IAND(ISHFT(NDATY(IJTH),-48),MSK16B)
C         ... CAN NLAPB EVER BE NEGATIVE-VALUED ????

          IF(NLAPB .EQ. 0) GO TO 555

C ...     ...  NDPEB(1) = NDPEB(1) - 1
          NLAPB = NLAPB - 1

 
          NDATY(IJTH) = IAND(NDATY(IJTH),MSK48B)    !... NDPEB(1)=0
          MQ = ISHFT(IAND(NLAPB,MSK16B),48)
          NDATY(IJTH) = IOR(MQ,NDATY(IJTH))  	    !... NDPEB(1)=NLAPB

          NDATA(IJTH,JBPTR) = NDATY(IJTH)
C         ...WHICH OVERSTORED THE OVERLAP COUNT DECREMENTED OF POINT B
          GO TO 555
C
C         ...    END OF DECREMENTING-OVRLAP-COUNT SPECIFIC PARA ...
C         . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C
  555   CONTINUE
C       ...     WHERE 555 IS ENDDO ON JBR=1,NSTRIP . . . . . . . . .
C
C       ... WHEN IT FALLS THROUGH, ALL B POINTS IN THIS STRIP HAVE BEEN
C       ...    EXAMINED.
C
  560 CONTINUE
C     ...   WHERE 560 IS ENDDO ON JSTRIP=JLOS,JHIS . . . . . . . . . 
C
C     ... WHEN IT FALLS THROUGH 560, ALL STRIPS WITHIN RANGE OF POINT A
C     ...    ARE FINISHED.
C
  564 CONTINUE
C
      RETURN
      END

      SUBROUTINE PRTOSSED(ITM,NDATY,NDATA,KEIL,JSUB,JCNT,JTM,IDIXAD,
     1            IDIXCO,ITOUT,LOPNL)
C                                              23-JUL-1996/DSS
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PRTOSSED      THIN ADP DATA
C   PRGMMR: LILLY            ORG: W/NMC412   DATE: 93-05-07
C
C ABSTRACT: THINNING THE ADP DATA FIELD
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR  UNKNOWN
C   89-05-01  STEVE LILLY  ADD DOCUMENTATION BLOCK
C   93-05-07  LILLY CONVERT SUBROUTINE TO FORTRAN 77
C   96-07-23  SHIMOMURA -- CONVERTING FROM IBM FORTRAN TO CRAY FORTRAN
C
C USAGE:    CALL PRTOSSED(ITM,NDATY,NDATA,KEIL,JSUB,JCNT,JTM,IDIXAD,
C                 IDIXCO,ITOUT,LOPNL)
C
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C

C     -----------------------------------------------------------------
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


      COMMON /WLONG0/ WLONG0
C
      integer*8   NDATA(ITM,JTM)
      integer*8   NDATY(3)
C
C ...      INTEGER      int4b(6)
C

      INTEGER      IACC
      CHARACTER*8  CACC
      EQUIVALENCE (IACC,CACC)

      LOGICAL     LOPNL
C
      DATA   DISSAT          /6.619/
      DATA   DOTSGI          /37.5/
      DATA   GSCALE          /26.7858/
C
C
      DATA      MSKR18                 /Z'0003FFFF'/

      INTEGER   MSK32B
      DATA      MSK32B                / X'FFFFFFFF' /
      INTEGER   NEGSIGNEXT
      DATA      NEGSIGNEXT    / X'FFFFFFFF00000000' /
C
      DATA   SCALE           /1.953124E-3/
C     ...WHICH IS RECIPROCAL OF 512 TO SHIFT BINARY PT 9 BITS...
C
      DATA   XSHIFT          /10.0/
      DATA   YSHIFT          /10.0/
C
      INTEGER   INDADDR
      INTEGER   LATLON
      INTEGER   LAT,LON
      REAL      ALAT,ALONG

       INTEGER     III_IFR_B,JJJ_JFR_B
       LOGICAL  LDONOTHING

       SAVE  


       LDONOTHING = .TRUE.
       IF(LDONOTHING) THEN
C        ... TRYING TO ISOLATE FAULT BY ELIMINATING POSSIBILITIES
         RETURN

       ENDIF

C     ...FETCH ITM WORDS OF POINT B...
C
      DO 701 I=1,ITM
        NDATY(I) = NDATA(I,JSUB)
  701 CONTINUE

      indaddr = iand(ndaty(3),msk32b)
      latlon  = IOBS2PK(1,INDADDR)
      LON = IAND(LATLON,MSK32B)
      LAT = IAND(ISHFT(LATLON,-32),MSK32B)
      IF(BTEST(LAT,31)) THEN
         LAT = IOR(LAT,NEGSIGNEXT)
      ENDIF
      ALAT = FLOAT(LAT)/4096.0
      IF(BTEST(LON,31)) THEN
         LON = IOR(LON,NEGSIGNEXT)
      ENDIF
      ALONG = FLOAT(LON)/4096.0
C
C     ... (NO BREAKOUT INTO 32-BIT GROUPS NECESSARY HERE) ...
C      . . . . . . . . . . . . . . . .
C         ... GET X,Y LOCATION OF STN-B ...
          III_IFR_B = IAND((ISHFT(NDATY(1), -5)),MSKR18)
          JJJ_JFR_B = IAND((ISHFT(NDATY(1),-23)),MSKR18)        

          XI = FLOAT(III_IFR_B) * SCALE
          XJ = FLOAT(JJJ_JFR_B) * SCALE

C      . . . . . . . . . . . . . . . . 
  702 CONTINUE
      IF(ITOUT .EQ. 7) THEN
C       ...             ITOUT = 7;  WHICH IS TROPIC ON MERC...
        GO TO 730
      ENDIF

      IF((ITOUT .NE. 12) .AND. (ITOUT .NE. 13)) THEN

        GO TO 730
      ENDIF
C     ... OTHERWISE, ITOUT=12; OR ITOUT=13;   GOES MAP ...
        GO TO 730


  730 CONTINUE

C     ... MOVE ITM WORDS INTO NDATY FOR PRINTING...
      DO  I=1,ITM
        NDATY(I) = NDATA(I,JSUB)
      ENDDO
C
      
      IF(.NOT. LOPNL) THEN
        IACC = NDATY(2)
       
        WRITE(6,704) CACC(1:6),ALAT,ALONG,(NDATY(I),I=1,2)
  704   FORMAT(1X ,'TOSSED ', A6, 3X, F6.2, 3X, F7.2,
     1        2Z17.16)

      ENDIF
C
      RETURN
      END
