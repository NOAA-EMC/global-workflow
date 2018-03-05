      PROGRAM GRIB2GRIB
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: GRIB2GRIB
C   PRGMMR: VUONG            ORG: NP11        DATE: 2001-05-18
C
C ABSTRACT:  FINDS AND EXTRACTS GRIB RECORDS FROM A GRIB FILE MADE
C   BY GRIBAWP1 FOR FAMILY OF SERVICES.
C   CONTROL CARDS CAN BE USED TO READ AND EXTRACT ALL OR
C   SELECTED GRIB RECORDS FROM A GRIB FILE. TO EXTRACT ONLY A SELECTED
C   NUMBER OF GRIB RECORDS FROM A GRIB FILE, A CONTROL CARD WITH A PDS
C   CAN BE USED. IF THE PDS IN THE CONTROL CARD IS FOUND IN THE INPUT
C   GRIB FILE, THE ENTIRE GRIB MESSAGE 'GRIB' TO '7777' IS WRITTEN
C   TO THE DESIGNATED OUTPUT FILE. CONTROL CARDS CAN ALSO BE USED TO
C   EXTRACT GRIB RECORDS ACCORDING TO THEIR GRID TYPE. NOTE: THIS
C   PROGRAM WAS DERIVED FROM THE 'unpkgrb1.f' CODE BUT DOES NOT
C   UNPACK ANY DATA.
C
C PROGRAM HISTORY LOG:
C   96-05-21  SOUTHALL
C   98-05-07  JOHNSON
C   98-07-01  VUONG      CONVERTED TO FORTRAN 90 AND Y2K COMPLIANT
C                        REMOVED CALLS TO W3LOG
C   99-05-19  VUONG      CONVERTED TO IBM RS/6000 SP
C   01-05-18  VUONG      INCREASING THE SIZE OF WORK ARRAY TO HANDLE THE
C                        LARGER GRIB FILE.
C 2012-08-09  VUONG      MODIFIED TO USE MODULE IFPORT
C
C USAGE:
C   INPUT FILES:
C      5         - INPUT CONTROL CARDS (3 TYPES)
C                  FORMAT (I1,I3,A44)
C                  FORMAT (A44)
C                  FORMAT (6(2X,4Z2),2X,I1)
C     11         - INTERNAL UNIT NUMBER OF AWIP GRIB INPUT FILE
C
C   OUTPUT FILES:
C     6        -   PRINT OUTPUT (STANDARD OUTPUT - FORTRAN)
C                  PRINTS ON SCREEN OF PC/WORKSTATION.
C     51       -   1 WRITE FOR EACH GRIB RECORD
C                  ENTIRE GRIB MESSAGE, 'GRIB' TO '7777'
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:     NONE
C
C     LIBRARY:
C       W3LIB    - IW3PDS, XMOVEX, W3TAGB
C
C   EXIT STATES:
C     COND = 0    SUCCESSFUL RUN
C          = 10   ERROR ON OPEN OF CONTROL CARD FILE
C          = 20   ERROR ON OPEN OF INPUT GRIB FILE
C          = 30   ERROR ON OPEN OF OUTPUT PACKED GRIB FILE
C          = 40   ERROR, INPUT GRIB FILE IS TOO BIG, CHANGE PROGRAM
C          = 60   ERROR READING GRIB FILE
C          = 99   I/O ERROR USING STAT FUNCTION
C          = 100  PROGRAMMER FORGOT CONTROL CARDS
C          = 110  NO CARD WITH FFFFFFFF SHOWING END OF PDS CARDS
C
C  WARNING:  On a CRAY you may have to use assign cards.  If your 
C            job prints the file name and file byte size O.K. but
C            quits with an OPEN INPUT FILE ERROR, use these assign
C            cards (at the prompt).
C 
C            $ assign -R
C            $ assign -a input-file-name -s sbin input-file-name
C            
C   EXAMPLE:  CONTROL CARDS IN 'grib2grib.dat' FILE
C
C  Example 1. To extract selected records (w/out unpacking them) from
C             file xtrn.eta24 and xtrn.mrf144 using cards read in with
C             the PDS of the record. After the last card with a PDS,
C             the next card must have FFFFFFFF starting in column 3.
C             Get the 500 mb HGT and 500 mb TMP (1st 2 PDS's) and the
C             850 mb RH and the MSL (2nd 2 PDS's) and write the packed
C             data into eta.packed.dat and mso.packed.dat respectively.
C             The PDS's of these fields and other can be obtained
C             by getting an inventory of the input grib files xtrn.eta24
C             and xtrn.mso30.  Column 63 is set to 2 so that only 11
C             of the 1st 12 bytes are used to find records. For more
C             info. concerning the number in Col. 63 refer to the
C             subroutine "iw3pds.f"
C
C  col. 1
C       0000xtrn.eta24
C       eta.packed.dat
C         00001C02  0759D380  076401F4  00000000  00000000  00000000  2
C         00001C02  0759D380  0B6401F4  00000000  00000000  00000000  2
C         FFFFFFFF  00000000  00000000  00000000  00000000  00000000  0
C       0000xtrn.mso30
C       mso.packed.dat
C         00001C02  0755D4C0  34640352  00000000  00000000  00000000  2
C         00001C02  074EC980  02660000  00000000  00000000  00000000  2
C         FFFFFFFF  00000000  00000000  00000000  00000000  00000000  0
C
C  Example 2. To extract only the grib messages from xtrn.mrf144 and
C             write them (without unpacking) into mrf.packed.dat.
C
C  col. 1
C       1000xtrn.mrf144
C       mrf.packed.dat
C
C  Example 3. To extract all grib records in the file xtrn.mrf144 by
C             grid type 201 and 203 placing them into mrf201.dat and
C             mrf203.dat respectively.
C  col. 1
C       2201xtrn.mrf144
C       mrf201.dat
C       2203xtrn.mrf144
C       mrf203.dat
C
C ATTRIBUTES:
C   LANGUAGE: F90 FORTRAN
C
C$$$
C
      USE IFPORT
      PARAMETER          (MXSIZE=300000)
      PARAMETER          (MSIZE=MXSIZE*3)
      PARAMETER          (IBUFSIZE=35000000)
C
C      IBUF is set bigger than the largest GRIB file you may read.
C
      CHARACTER * 1      IBUF(IBUFSIZE)
      CHARACTER * 28     V1(900)
      CHARACTER * 2      HEXPDS(24)
      CHARACTER * 44     IFILE5
      CHARACTER * 44     NFILE
      CHARACTER * 1      MSGA(MSIZE)
      CHARACTER * 44     OFILE
      CHARACTER * 1      IPDS(24)
C
      INTEGER            JPDS(4)
      INTEGER            RCBYTE(900)
      INTEGER            ISTART(900)
      INTEGER            IUNPK(900)
      INTEGER*4          JSTAT(12)
      INTEGER            TYPEGRID
C
      LOGICAL            IW3PDS
C
C
      SAVE
C
      DATA  IFILE5/'grib2grib.dat'/
      DATA  JJJ   /0/
C
C
C***********************************************************************
C
C
      CALL W3TAGB('GRIB2GRIB',2001,0138,0064,'NP11') 
      KKK = 0
C
C          OPEN CONTROL CARD FILE
C
      OPEN (UNIT=5,FILE=IFILE5,STATUS='OLD',
     & FORM='FORMATTED',IOSTAT=MERR)
      IF (MERR.NE.0) THEN
         PRINT *,' ERROR OPENING CONTROL CARD FILE = ',IFILE5
         CALL ERREXIT (10)
      ENDIF
C
C      PARAMETERS ON CONTROL CARD TYPE 1
C
C      IALL  = 0    WRITE ONLY SELECTED RECORDS USING CARDS WITH PDS
C      IALL  = 1    WRITE ALL RECORDS IN FILE
C      IALL  = 2    WRITE ONLY ONE GRID TYPE
C
   10 CONTINUE
      READ (5,'(I1,I3,A44)',END=100) IALL, TYPEGRID, NFILE
      PRINT 15, IALL, TYPEGRID
   15 FORMAT (' IALL TYPEGRID = ',I1,I4)
      WRITE(*,'(A,A10)')' INPUT AWIPS FILE = ',NFILE
      KKK = KKK + 1
C
C        READ OUTPUT FILE CARD WITH FILE NAME
C
      JJJ = JJJ + 1
      READ (5,'(A44)',END=100) OFILE
      WRITE(6,'(A,A10)')' OUTPUT FILE FOR PACKED GRIB MESSAGES = ',OFILE
c
      IF (IALL.EQ.2.AND.TYPEGRID.EQ.0) THEN
         PRINT *,'ERROR, IALL = 2, AND TYPEGRID = 0'
         GO TO 10
      END IF
C
C        CALL FUNCTION STAT TO FIND NUMBER OF BYTES IN FILE
C        WORD 8 OF ARRAY JSTAT HAS THE NUMBER OF BYTES IN FILE
C        IF YOUR COMPUTER DOES NOT HAVE THE C FUNCTION STAT
C        COMMENT OUT THE NEXT 7 LINES
C
C        STRING VALUES IN THE C LANGUAGE ARE TERMINATED WITH
C        NULL CHARACTER CHAR(0)
C
      NFILE = TRIM(NFILE)//CHAR(0)

      IF (STAT(NFILE,JSTAT).NE.0) THEN
         PRINT *,'ERROR IN FUNCTION STAT GETTING FILE STATS'
         CALL ERREXIT (99)
      ELSE
         KBYTES = JSTAT(8)
         WRITE(6,'(A,I0)')' NUMBER OF BYTES IN GRIB FILE   = ',KBYTES
      END IF
C
C        TEST TO SEE IF INPUT GRIB FILE IS TOO BIG
C
      IF (IBUFSIZE.LT.KBYTES) THEN
         PRINT *,'GRIB INPUT FILE IS TO BIG FOR GRIB2GRIB PROGRAM'
         PRINT *,'CHANGE PROGRAM SO PARAMETER IBUFSIZE IS LARGER'
         PRINT *,'THAN THE NUMBER OF BYTES IN THE FILE'
         CALL ERREXIT (40)
      END IF
C
C       OPEN INPUT GRIB FILE - ERRORS?  read WARNING above
C
      OPEN (UNIT=11,FILE=NFILE,STATUS='OLD',ACCESS='DIRECT',
     & FORM='UNFORMATTED',IOSTAT=MERR,RECL=KBYTES)
      IF (MERR.NE.0) THEN
         PRINT *,'OPEN INPUT FILE ERROR ON FILE = ', NFILE
         PRINT *,'ERROR = ',MERR
         CALL ERREXIT (20)
      END IF
C
C
C       OPEN OUTPUT FILE FOR PACKED GRIB DATA IN FLOATING POINT
C
c     OPEN (UNIT=51,FILE=OFILE,STATUS='unknown',
c    & FORM='UNFORMATTED',ACCESS='SEQUENTIAL',IOSTAT=MERR)

      CALL BAOPENW(51,OFILE,IRET)
      IF (IRET.NE.0) THEN
         PRINT *,'OPEN OUTPUT FILE ERROR ON FILE = ', OFILE
         PRINT *,'ERROR = ',IRET
         CALL ERREXIT(30)
      END IF
C
C        Read the entire GRIB file into array IBUF
C        On SUN and SGI the stat function works, when RECL=1
C        REC is a pointer at what byte in the file to start
C        reading at. The SUN and SGI compilers compile this as
C        one big read, other compilers, such as the INTERGRAPH,
C        give a compile error, or run time error.
C        If so, comment out this line, uncomment the code that
C        that applies to your computer. This works, but is very
C        slow because you are doing one read for each byte in
C        file.  Replace this if your computer has a better way of
C        doing it. This should also work if RECL=KBYTES.
C
      READ (11,REC=1,ERR=60) (IBUF(I),I=1,KBYTES)
C
C      Use this if you have no stat function or system function
C      This is very slow.
C
C      I = 1
C 1    CONTINUE
C        READ (11,REC=I,IOSTAT=IERR) IBUF(I)
C        IF (IERR.EQ.0) THEN
C          I = I + 1
C          GO TO 1
C        ELSE
C          KBYTES = I - 1
C          PRINT *,'NUMBER OF BYTES IN GRIB FILE   = ',KBYTES
C        END IF
C
C      Use this if you have stat or system function and RECL=1 but
C      the read does not work. This is very slow.
C
C      DO I = 1,KBYTES
C        READ (11,REC=I,ERR=60) IBUF(I)
C      END DO
C
C  -------------------  FIND  all 'GRIB' in file, save address
C  -------------------  PDS, length of grib message.
C
      JSTART = 1
      NN = 0
      DO 101 I = JSTART, KBYTES
        IF (MOVA2I(IBUF(I  )).NE.71) GO TO 101
        IF (MOVA2I(IBUF(I+1)).NE.82) GO TO 101
        IF (MOVA2I(IBUF(I+2)).NE.73) GO TO 101
        IF (MOVA2I(IBUF(I+3)).NE.66) GO TO 101
        IF (MOVA2I(IBUF(I+7)).NE.1)  GO TO 101
        NN = NN + 1
           DO J = I,I+27
             V1(NN)(J-I+1:J-I+1) = IBUF(J+8)
           END DO

        RCBYTE(NN) = MOVA2I(IBUF(I+4)) * 65536 + MOVA2I(IBUF(I+5)) *
     &  256 + MOVA2I(IBUF(I+6))
        JSTART     = I + RCBYTE(NN)
        ISTART(nn) = I
C       PRINT *,'NN,I,JSTART,RCBYTE(NN) = ',NN,I,JSTART,RCBYTE(NN)
  101 CONTINUE
C
      WRITE(6,'(A34,I0)') 'NUMBER OF GRIB RECORDS IN FILE = ',NN
C
C
      NUMREC = NN
C
C
C      WRITE ALL GRIB RECORDS OR JUST THOSE OF A CERTAIN GRID TYPE
C
      IF (IALL.EQ.1.OR.IALL.EQ.2) THEN
         NREC = 0
         IF (IALL.EQ.1) PRINT *,'ALL GRIB RECORDS IN THE FILE'
         IF (IALL.EQ.2) THEN
            PRINT *,'IALL = 2, OUTPUT ONLY RECORDS OF GRID TYPE ',
     &      TYPEGRID
            PRINT *,' '
         END IF
C
C     PROCESS ALL GRIB RECORDS, IF IALL=2, SAVE ONLY CERTAIN GRID TYPES
C
         DO 30 K =1,NUMREC
           MBYTES = RCBYTE(K)
           KSTART = ISTART(K)
           IF (IALL.EQ.2) THEN
              IGRID = MOVA2I(IBUF(KSTART+14))
C             PRINT *,'IGRID = ',IGRID
              IF (TYPEGRID.NE.IGRID) GO TO 30
           END IF
C
C          MOVE GRIB RECORD SO IT IS ON WORD BOUNDARY
C
           CALL XMOVEX(MSGA,IBUF(KSTART),MBYTES)
           NGRID = IGRID
C
C
           IF (IALL.EQ.2) THEN
              PRINT 29, K, NGRID,OFILE
   29         FORMAT (3X,'GRIB RECORD #',I4,' IS OF GRID TYPE: ',
     &        I4,', WRITING ENTIRE GRIB MESSAGE ','TO ',A44)
           ENDIF
c          WRITE (51) (IBUF(M),M=KSTART,KSTART+MBYTES)
           CALL WRYTE(51, MBYTES+1, IBUF(KSTART))
           NREC = NREC + 1
C
C
   30    CONTINUE
      ENDIF
C
C      READ ONLY SELECTED RECORDS BY CONTROL CARD, CONTROL CARD
C      HAS PDS, INDEX RECORD IS SEARCHED FOR MATCH OF PDS, WITH
C      OR WITHOUT USING THE DATE IN THE PDS.  KEY = 0 IF YOU DO
C      NOT USE DATE IN PRODUCT DEFINITION SECTION (PDS).
C
      IF (IALL.EQ.0) THEN
         DO I = 1,NUMREC
           IUNPK(I) = 0
         END DO
         N      = 0
         ICARDS = 0
C
   32    CONTINUE
         READ (5,'(6(2X,4A2),2X,I1)',END=110) HEXPDS, KEY
         DO J = 1,24
           CALL HEXCHAR(HEXPDS(J),IPDS(J),IER)
           IF (IER.EQ.1) PRINT *,'HEXCHAR ERROR '
         END DO
C
C     TEST FOR  FFFFFFFF, END OF (PDS) CARDS.
C
         IF (MOVA2I(IPDS(1)).EQ.255) GO TO 36
         N = N + 1
         PRINT 33, HEXPDS, KEY
   33    FORMAT (' PDS = ',6(2X,4A2),2X,I1)
         DO 35 K = 1,NUMREC
           IF (IW3PDS(V1(K),IPDS,KEY)) THEN
              ICARDS        = ICARDS + 1
              IUNPK(ICARDS) = K
C             print *,'IUNPK(',icards,') = ',k
              GO TO 32
           ENDIF
   35    CONTINUE
         PRINT 37, HEXPDS, KEY
   37    FORMAT (' CAN NOT FIND PDS = ',6(2X,4A2),2X,I1)
         IF (N.LE.NUMREC) GO TO 32
         IF (N.GT.NUMREC) THEN
            PRINT *,' POSSIBLE ERROR, YOU HAVE MORE PDS CARDS'
            PRINT *,' THEN RECORDS IN FILE. YOU MAY WANT A '
            PRINT *,' DUPLICATE RECORD, I WILL CONTINUE.'
            GO TO 32
         END IF
   36    CONTINUE
         PRINT *,' '
      ENDIF
C
C       WRITE ONLY SELECTED PACKED GRIB RECORDS TO OUTPUT FILE
C
      IF (IALL.EQ.0) THEN
         IF (ICARDS.EQ.0) GO TO 50
         NREC = 0
         DO 40 K = 1,ICARDS
           MBYTES = RCBYTE(IUNPK(K))
           KSTART = ISTART(IUNPK(K))
C
C          MOVE GRIB RECORD SO IT IS ON WORD BOUNDARY
C
           CALL XMOVEX(MSGA,IBUF(KSTART),MBYTES)
C
           PRINT 39, K, IUNPK(K) ,OFILE
   39      FORMAT ('PDS FOR CONTROL CARD ',I2,' MATCHES THAT OF GRIB ',
     &     'RECORD # ',I3,', WRITE ENTIRE GRIB MESSAGE TO: ',A44)
c          WRITE (51) (IBUF(M),M=KSTART,KSTART+MBYTES)
           CALL WRYTE(51, MBYTES+1, IBUF(KSTART))
           NREC = NREC + 1
C
   40    CONTINUE
      ENDIF
C
   50 CONTINUE
      PRINT *,' '
      IF (NREC.NE.0) THEN
         WRITE(6,'(A7,I0,A22,A)')' WROTE ',NREC,
     &                ' GRIB RECORDS IN FILE ',OFILE(1:30)
      ENDIF
      PRINT *,' '
c
      CLOSE (UNIT=11)
c     END FILE 51
      CALL BACLOSE(51,IRET)
c     CLOSE (UNIT=51)
      GO TO 10
C
   60 CONTINUE
      PRINT *,'READ ERROR READING GRIB FILE, ERROR = ',JERR
      CALL ERREXIT (60)
C
  100 CONTINUE
      CALL W3TAGE('GRIB2GRIB') 
      IF (KKK.EQ.0) THEN
         PRINT *,'EOF READING 1ST CONTROL CARD, FILE = ',IFILE5
         CALL ERREXIT (100)
      ENDIF
      STOP
C
  110 CONTINUE
      PRINT *,' EOF READING CONTROL CARDS WITH PDS '
      CALL ERREXIT (110)
      END
