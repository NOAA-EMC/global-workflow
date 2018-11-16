       INTEGER FUNCTION ijbsirch(NAME,TABLE,IDIMEN,MXTAB)
C                                                  6-Aug-1996/dss
C      ... copied ~/cntr/ijbsirch.for into ~/plop/ijbsirch.f
C      ...   in order to adapt to searching for a matching name
C      ...   in an I*8 table(IDIMEN,J) in which the match-sought
C      ...   is in TABLE(1,J);  NAME in HOLLERITH, left-justified
C      ...   with blank fill;
C      ...   TABLE(1,J) must be in sorted order.
C                                                  1-JUL-1988/DSS
C      ... TO LOOK FOR MATCHING NAME IN TABLE WHICH MUST BE IN
C      ..  ascii sorted ORDER.
C      ... TYPE integer
C      ... RESULT IS THE VALUE OF THE POINTER TO THE MATCHING ENTRY 
C      ... IN THE GIVEN TABLE.  IF NO MATCH, THEN ijbsirch VALUE = 0 
C      ... IF ERROR-RETURN, THEN ijbsirch VALUE IS SET TO NEGATIVE
C
       integer    NAME
       integer    TABLE(IDIMEN,MXTAB)
C
C
       integer      klanks
       data         klanks     / X'2020202020202020' /
       integer      kbinzero
       data         kbinzero   / 0 /

       LOGICAL    LASTLP
       LOGICAL    LOFFRT
       LOGICAL    LOFFLW
C
C
       ijbsirch = 0  		!... init result to "not-found"
       MDIS = 0
       IF(MXTAB .LE. 0) GO TO 920
       IF(IDIMEN .LE. 0) GO TO 930

       if(name .eq. klanks) go to 900
       if(name .eq. kbinzero) go to 910
C
C       IF(MXTAB .GE. 10) GO TO 300
C      ... WHICH WILL DO BINARY SEARCH ONLY IF TABLE IS .GE. 10 ITEMS
C      ... OTHERWISE, DO SEQUENTIAL COMPARE ... 
       DO  ITA = 1,MXTAB
         IF(NAME .EQ. TABLE(1,ITA)) THEN
           MDIS = ITA
           GO TO 400  		!... found match at TABLE(1,MDIS)
         ENDIF
       END DO
       GO TO 900   		!... failed to find a match 
C
C
  300  CONTINUE
C      ... COMES HERE TO DO BINARY SEARCH ON TABLE ...
       IBEGIN = 1
       MIDPT = (MXTAB/2) + 1
       INCR = MIDPT
       II = 0
       LASTLP = .FALSE.
       LOFFRT = .FALSE.
       LOFFLW = .FALSE.
  330  CONTINUE
       II = II + 1
       IF(INCR .LE. 1) LASTLP = .TRUE.
C      ... OTHERWISE, WE ARE NOT DOWN TO SINGLE STEP OF FINAL COMPARE 
       INCR = (INCR+1) / 2
       IF(LOFFRT) GO TO 333
       IF(LOFFLW) GO TO 344
       MDIS = IBEGIN + MIDPT - 1
       IF(NAME .LT. TABLE(1,MDIS)) THEN
         GO TO 333
       ELSE IF(NAME .EQ. TABLE(1,MDIS)) THEN
C        ... FOUND EXACT MATCH, SO JUMP TO "FOUND"
         GO TO 400
       ELSE
C        ... WAS .GT.  ...
         GO TO 344
       ENDIF
  333  CONTINUE
       LOFFRT = .FALSE.
C      ... GO TO LOWER HALF AND BISECT IT ...
       IF(LASTLP) GO TO 388
       MIDPT = MIDPT - INCR
       IF(MIDPT .GT. 0) GO TO 330
C      ... OTHERWISE, SEARCH FELL BELOW LOWER END OF TABLE ...
C      ... SET SWITCH LOFFLW AND LET IT JUMP BACK INTO TABLE ...
       LOFFLW = .TRUE.
       GO TO 330
C
  344  CONTINUE
       LOFFLW = .FALSE.
C      ... GO TO UPPER HALF AND BISECT ...
       IF (LASTLP) GO TO 388
       MIDPT = MIDPT + INCR
       IF(MIDPT .LE. MXTAB) GO TO 330
C      ... OTHERWISE, SEARCH FELL BEYOND END OF TABLE ...
C      ... SET SWITCH LOFFRT AND LET IT JUMP BACK INTO TABLE
       LOFFRT = .TRUE.
       GO TO 330
C
  388  CONTINUE
C      ... COMES TO 388 IF NO MATCH FOUND IN TABLE ...
       GO TO 900

C
  400  CONTINUE
C      ... COMES HERE IF MATCH WAS FOUND AT MDIS ...
       ijbsirch = MDIS
       GO TO 999
C
  900  CONTINUE
       ijbsirch = 0
       GO TO 999

  910  continue
C      ... comes here if name = binary zero ...
       ijbsirch = -1
       go to 999

  920  CONTINUE
C      ... COMES HERE IF BAD MXTAB ARG (J-DIMENSION OF TABLE) WAS GIVEN
       ijbsirch = -2
       GO TO 999
  930  CONTINUE
C      ... COMES HERE IF BAD IDIMEN ARG (I-DIMENSION OF TABLE) WAS GIVEN
       ijbsirch = -3
       GO TO 999
C
  999  CONTINUE
       RETURN
       END 
