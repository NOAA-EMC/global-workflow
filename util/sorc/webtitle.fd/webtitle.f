C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: WEBTITLE 
C   PRGMMR: SAGER            ORG: NP12        DATE: 2003-10-02
C
C ABSTRACT:  READS A FILE CONTAINING THE CURRENT DATE AND THE FORECAST
C   HOUR AND WRITES A FILE CONTAINING A TITLE CONTAINING A REFORMATED
C   DATE.  THIS FILE IS USED TO CREATE A NEW FORMATED TITLE FOR THE
C   NCEP MODEL GRAPHICS WEBSITE                    
C
C PROGRAM HISTORY LOG:
C
C   03-10-02  L. SAGER    ORIGINAL VERSION
C   01-30-13  B. MABE     Updated for WCOSS system. Remove Equiv and
C                         char to integer implied casts 
C USAGE:
C   INPUT FILES:
C     FT05     - CURRENT DATE AND FORECAST HOUR
C
C   OUTPUT FILES:
C     FT55     - UPDATED TITLE CONTAINING REFORMATTED
C                DATE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    -
C     LIBRARY:   - W3AI15 W3FS15 W3DOXDAT
C       COMMON   -
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM
C
C$$$
C
      INTEGER       idat(8) 
      CHARACTER*4   cout(10)
      CHARACTER*3   days(7)  
      CHARACTER*14  block
      CHARACTER*40  line1    
      CHARACTER*40  line2    
      CHARACTER*4   tb1(2)
      CHARACTER*2   tb2(3)
      BYTE       bsmdate(4)
      BYTE       retdate(4)

      DATA          idat   /8*0/
      DATA          days   /'SUN','MON','TUE','WED','THU','FRI','SAT'/

      DATA          line1  /'09/01/2003 12UTC  24HR FCST VALID TUE 09'/      

      DATA          line2  /'/02/2003 12UTC  NCEP/NWS/NOAA'/      

      CALL W3TAGB('WEBTITLE',2001,0275,0076,'NP12')
C
C     Start by reading in the date/time
C
      READ(5,102) block
 102  FORMAT(a14)
      READ(block,100) tb1(1), tb1(2), tb2(1), tb2(2), tb2(3), tb2(4)
 100  FORMAT(2a4,4a2)
      
      read(tb1(1),*) jtau
      read(tb1(2),*) iyear
      iwork = iyear - 2000
      bsmdate(1)=iwork 
      read(tb2(1),*) bsmdate(2)
      read(tb2(2),*) bsmdate(3)
      read(tb2(3),*) bsmdate(4)
           
C USAGE:    CALL W3FS15 (IDATE, JTAU, NDATE)
C   INPUT ARGUMENT LIST:
C     IDATE    - PACKED BINARY DATE/TIME AS FOLLOWS:
C                BYTE 1  IS YEAR OF CENTURY  00-99
C                BYTE 2  IS MONTH            01-12
C                BYTE 3  IS DAY OF MONTH     01-31
C                BYTE 4  IS HOUR             00-23
C                SUBROUTINE TAKES ADVANTAGE OF FORTRAN ADDRESS
C                PASSING, IDATE AND NDATE MAY BE
C                A CHARACTER*1 ARRAY OF FOUR, THE LEFT 32
C                BITS OF 64 BIT INTEGER WORD. AN OFFICE NOTE 85
C                LABEL CAN BE STORED IN
C                4 INTEGER WORDS.
C                IF INTEGER THE 2ND WORD IS USED. OUTPUT
C                IS STORED IN LEFT 32 BITS. FOR A OFFICE NOTE 84
C                LABEL THE 7TH WORD IS IN THE 4TH CRAY 64 BIT
C                INTEGER, THE LEFT 32 BITS.
C     JTAU     - INTEGER  NUMBER OF HOURS TO UPDATE (IF POSITIVE)
C                OR BACKDATE (IF NEGATIVE)
C
C   OUTPUT ARGUMENT LIST:
C     NDATE    - NEW DATE/TIME WORD RETURNED IN THE
C                SAME FORMAT AS 'IDATE'. 'NDATE' AND 'IDATE' MAY
C                BE THE SAME VARIABLE.

      CALL w3fs15(bsmdate,jtau,retdate) 
C
C...  w3doxdat returns the day of the week
C
C  INPUT VARIABLES:
C     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
C                (YEAR, MONTH, DAY, TIME ZONE,
C                 HOUR, MINUTE, SECOND, MILLISECOND)
C
C   OUTPUT VARIABLES:
C     JDOW       INTEGER DAY OF WEEK (1-7, WHERE 1 IS SUNDAY)
C     JDOY       INTEGER DAY OF YEAR (1-366, WHERE 1 IS JANUARY 1)
C     JDAY       INTEGER JULIAN DAY (DAY NUMBER FROM JAN. 1,4713 B.C.)
C
      idat(1) = iyear
      idat(2) = retdate(2)
      idat(3) = retdate(3)
      idat(5) = retdate(4)

      CALL w3doxdat(idat,jdow,jdoy,jday)
       
C
C     Convert the valid date back to character
C

      CALL w3ai15(idat,cout,10,2,' ')


      line1(1:2) = block(9:10)
      line1(4:5) = block(11:12)
      line1(9:10) = block(7:8) 
      line1(12:13) = block(13:14)
      line1(18:20) = block(2:4)
      line1(35:37) = days(jdow)
      line1(39:40) = cout(2)(1:2)

      line2(2:3) = cout(3)(1:2)
      line2(7:8) = cout(1)(1:2)
      line2(10:11) = cout(5)(1:2)

   

      write(55,105) line1,line2
 105  FORMAT(2a40)

      CALL W3TAGE('WEBTITLE')
      STOP
      END
