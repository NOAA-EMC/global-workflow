#if !defined (_BIG_ENDIAN) && !defined (_LITTLE_ENDIAN)
#error 'Neither _BIG_ENDIAN nor _LITTLE_ENDIAN is defined.'
#endif
       SUBROUTINE REBLKFX4(LUNIX6T,LUNIPK6,NUMRECFAX,
     1                     IISCHED,C1IFID,MYOPTNBITS,IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    REBLKFX4    CONVERT RLE NMC EXT 6-BIT TO PACKED FORM
C   PRGMMR: KRISHNA KUMAR      ORG: W/NP12     DATE: 1999-10-26
C
C ABSTRACT: CONVERT A FILE CONTAINING ONE RUN-LENGTH ENCODED (RLE)
C    RASTER PRODUCT IN NMC EXTENDED 6-BIT CODE INTO THE NMC PACKED
C    6-BIT FAX CODE, AND ADD ALL THE HEADERS AND TRAILERS REQUIRED
C    IN THE FINAL NMC 6-BIT PACKED FAX PRODUCT FORMAT.
C
C PROGRAM HISTORY LOG:
C   85-12-02  ORIGINAL AUTHOR(S)'S NAME: DAVID SHIMOMURA
C               -- TO REBLOCK FAX PRODUCT FROM GIVEN 512-BYTE 
C                  RECORDS INTO 1440-BYTE BLOCKS.
C
C   86-03-06  SHIMOMURA -- MODIFIED TO WRITE COMMENTS TO UNIT=46 
C                  FOR RUNNING UNDER FXD.COM
C
C   86-03-07  SHIMOMURA -- COPIED FROM VAX VERSION:'REBLKFAX.FOR'
C                  TO MAKE A VERSION WHICH CALLS faxwrs3 TO INSERT
C                  DOUBLE-DLE'S AND OTHER RJE COMMS REQUIREMENTS
C                  FOR MOVING FROM VAX TO IBM MAINFRAME USING RJE LINE,
C                  AND BYPASS THE INTERMEDIATE FILE OF PURE NMC STD FAX.
C
C   86-04-29  SHIMOMURA -- modified to discard trailing blank lines 
C                  before the end-of-map.
C
C   89-12-26  SHIMOMURA -- COPIED VAX VERSION OF [6,300]REBLKFX.FOR
C                  TO MAKE A CHECKOUT VERSION, STRIPPING OUT THE RJE 
C                  TRANSMISSION MODS IN AN ATTEMPT TO MAKE PURE NMC 
C                  6-BIT PACKED 1440 BYTE RECORDS;
C
C   91-08-08  ART WICK - Modified to run on Intergraph Unix workstation.
C
C   93-05-26  SHIMOMURA -- Modified for new af77 compiler, which is a
C                  change from previous version of Green Hills compiler.
C
C   95-04-25  SHIMOMURA -- THE OPNL INTERGRAPH VERSION IS "REBLKFX2", 
C                  SO I COPIED INTO "REBLKFX3" TO MAKE MODS WITHOUT 
C                  AFFECTING OPNS.  THESE MODS ARE FOR GENERATING MAP
C                  BACKGROUNDS WHICH HAVE DIFFERENCES IN THE "IFID"-
C                  HEADER.
C 
C   96-04-30  SHIMOMURA -- REPROGRAM FROM INTERGRAPH TO CRAY
C   96-05-13  SHIMOMURA -- RENAMED TO REBLKFX4 BECAUSE I AM CHANGING
C                  THE INPUT RECORD SIZE FROM 512-BYTES TO 1920-BYTES
C   96-11-11  SHIMOMURA -- CORRECTING FOR ISCHED OF MULTI-PANELS
C
C 1999-07-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM RS/6000
C 1999-07-20  HENRICHSEN MODIFY TO USE DIRECT ACCESS I/O ON FAX OUTPUT
C             FILE FOR RUNNINGON THE IBM SP.
C 1999-10-26  KRISHNA KUMAR COMMENTED A STATEMENT (NBLOCKOUT=NBLOCKOUT+1) 
C             WHICH WAS INCREMENTED TWICE (TYPO ERROR) WHICH CAUSED HAVOC ON 
C             MANY FAX GRAPHICS PROGRAMS WITH SUBTITLES & SUBSETS  
C
C USAGE:    CALL REBLKFX4(LUNIX6T, LUNIPK6,NUMRECFAX,
C     1                     IISCHED,C1IFID,MYOPTNBITS,IERR)
C
C   INPUT ARGUMENT LIST:
C          NUMRECFAX RECORD NUMBER IN OUTPUT FAX FILE TO BEGIN WRITTING.
C     I*8  IISCHED(8,60) - CNTR,S  FAX SCHED CONTROLS;
C     C*1  C1IFID(48) - CNTR,S FAX HEADER ID;
C     I*8  MYOPTNBITS - FOR OPTIONS
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     I*8  IERR - RETURN CODE
C               = 0; NORMAL RETURN
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C     I*8  LUNIX6T  - DSRN OF INPUT FILE CONTAINING ONE PRODUCT
C                     IN  NMC EXTENDED 6BIT CODE;
C
C   OUTPUT FILES:
C     I*8  LUNIPK6  - DSRN OF OUTPUT FILE IN NMC 6BIT PACKED RLE;
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C                 CALLS piksor()   TO SORT ISCHED ITEMS
C                 CALLS PAK8TO6() TO CONCATENATE 6-BIT CODE.
C                 CALLS PADIFID() TO PAD THE IFID.
C                 calls sbytesccs()  to quarter pack the iisched data
C
C ATTRIBUTES:
C   LANGUAGE: IBM FORTRAN 90
C   MACHINE:  IBM
C
C$$$
C
C                                                  22-MAY-1996/DSS
C      ...
C
C      . . . . . .  D E F I N E   C O N S T A N T S   . . . . . . . . .
       INTEGER       NBYTPWRD
       PARAMETER    (NBYTPWRD=8)  		!... CRAY 8-BYTE INT
C ...       PARAMETER    (NBYTPWRD=4)  		!... INTERGRAPH I*4 WRD

       INTEGER       INRECL   		!... INPUT RECL IN BYTES
       PARAMETER    (INRECL=1920)

       INTEGER       INRECLINT      		!... 64 I*8 WRDS =1920/8
       PARAMETER    (INRECLINT=INRECL/NBYTPWRD)

       INTEGER       INRECLINT2      		!...128 I*8 WRDS =2*64
       PARAMETER    (INRECLINT2=2*INRECLINT)

       INTEGER       INRECLINTP1      		!... 65 I*8 WRDS =1+64
       PARAMETER    (INRECLINTP1=1+INRECLINT)


       INTEGER       NMCSTDRECL   		!... = 1440 BYTES
       PARAMETER    (NMCSTDRECL=1440)

       INTEGER       MAXEXTBYT
       PARAMETER    (MAXEXTBYT=1920)
C      ... WHERE 1920 BYTES IS CAPACITY OF C1SPRED ARRAY
C      ...    WHICH IS EQUIV TO 1440-BYTES IN COMPRESSED 6-BITS, BUT
C      ...    EXTENDED TO 8-BITS PER 6-BIT ITEM.

       INTEGER       MAXEXTINT    		!...240 I*8 WRDS =1920/8
       PARAMETER    (MAXEXTINT=MAXEXTBYT/NBYTPWRD)
C
C
       INTEGER       MXSCHED
       PARAMETER    (MXSCHED=60)
C      ... MAX NO. OF SUBSETS/INSETS DEFINED IN GIVEN IISCHED ARRAY.
C      ... BEDIENT USES 59
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       COMMON  /XSCHEDS/NTOTSCHED,NSCHED_CUT,NSCHED_TITL,JOFSCH,
     1                  NUMPANEL,JSCHED2D,JLINSRTED
       INTEGER       NTOTSCHED 
       INTEGER       NSCHED_CUT
       INTEGER       NSCHED_TITL
       INTEGER       JOFSCH
       INTEGER       NUMPANEL
       INTEGER       JSCHED2D(8,MXSCHED)
       INTEGER       JLINSRTED(2,MXSCHED)

C      ...   SUBSET DEFS IN JSCHED2D WHOSE POINTERS REMAIN TO
C      ...   BE DEFINED = F(STARTING SCAN LINE)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C                    CALL SEQUENCE
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C
       INTEGER    LUNIX6T   		!...Arg1: dsrn of input.x6t
C
C      ... WHERE LUNIX6T IS DSRN OF LOGICAL UNIT WHERE INPUT FILE
C      ...   OF FAX.X6T  RASTER DATA FORMATTED 6-BIT CODE IN 8-BIT 
C      ...   BYTE IS TO BE READ IN FROM
C
       INTEGER    LUNIPK6  		!...Arg2: dsrn of output.pk6
C      ... WHERE LUNIPK6 IS DSRN OF LOGICAL UNIT WHERE NMCSTDRECL-BYTE
C      ...   BLOCKED RECORD WILL BE OUTPUT.
C
       INTEGER          IISCHED(8,MXSCHED)	!...Arg3: isched()
C      ... WHERE IISCHED IS ACCEPTED AS ARG IN CALL SEQ
C      ...   AND IS IN THE SAME FORMAT AS IN CALL TO CNTR
C
       CHARACTER*1      C1IFID(48)  	!...Arg4: IFID(48BYTES)
C
       INTEGER          MYOPTNBITS    	!...Arg5: option controls
C      ... WHERE MYOPTNBITS ARE MY OWN OPTION BITS;
C      ...    DO NOT CONFUSE THESE WITH THE MAP(2) OPTION BITS
C
       INTEGER          IERR   		!...ARG6
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C
       integer   nsizdes
       data      nsizdes    / 180 /  	!... 1440/8bytperword= 180 wrds
       integer      JSCHEDS(180)        !... 1440/8bytesperword=180 wrds
       CHARACTER*1  C1JSCHED(1440)
       EQUIVALENCE  (C1JSCHED(1),JSCHEDS(1))
       
       integer      noffset
       integer      nbitpgrp
       integer      nbitskip
       integer      ngrps2do

C
C ...    ... i was thinking to explicitly save what I need to
C ...    ...   preserve from one panel to next panel;
C ...    ...   but I cannot equivalence arrays in common  
       INTEGER          INTSPRED(MAXEXTINT)
       CHARACTER*1      C1SPRED(MAXEXTBYT)    		!... (1920)
       EQUIVALENCE (INTSPRED(1),C1SPRED(1))
C      ... WHERE C1SPRED IS BIG ENOUGH FOR ONE OUTPUT FAX
C      ...   RECORD SPREAD OUT 6 BITS DATA PER 8-BIT BYTE.
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       INTEGER       JOUTBF(180)    		!... I*4 WAS (360)
       CHARACTER*1   C1OUTBF(NMCSTDRECL)
       EQUIVALENCE  (C1OUTBF(1),JOUTBF(1))
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . .  Input is 1920-byte rec of .X6T data . . . . . . .

C
       INTEGER       inbufA(INRECLINT)
C
C
       CHARACTER*1   CINBUF(INRECL)
       EQUIVALENCE  (inbufA(1),CINBUF(1))
C
       INTEGER       KHEADER(8)       			!... 8*8 = 64
C      ... WHERE REBLKFX4 WILL COPY padded IFID HEADER INTO FROM INPUT
C
       CHARACTER*1   LHEADER(64)
       EQUIVALENCE  (KHEADER(1),LHEADER(1))
C
C

       INTEGER       IACC
       integer       iword1

       INTEGER       MSKLHS
       DATA          MSKLHS         / X'FFFFFFFF00000000' /
       INTEGER       MSKRHS
       DATA          MSKRHS         / X'00000000FFFFFFFF' /

       INTEGER       MSKHI3BYT   
       DATA          MSKHI3BYT      / X'FFFFFF0000000000' /
       INTEGER       MAPSTARTFLAG
       DATA          MAPSTARTFLAG   / X'FFFFFF0000000000' /
C
C      . . . . . . . . . . . .   logical switches    . . . . . . . . .
C      . . . . . . . . . . . .   derived from MYOPTNBITS  . . . . . . .

       LOGICAL    LENTIREQQ   		!... ALL-IN-1 OR IN PANELS
       LOGICAL    LADD_ONQQ             !... AT ENTRY TO ADD-ONTO EXISTG
       LOGICAL    LEAVE_OPENQQ    	!... AT EXIT TO LEAVE PROD OPEN
       LOGICAL    LSKIPFFQQ    		!... AT ENTRY NOT TEST FOR FF's 
       LOGICAL    LRASAT65QQ   		!... RASTER STARTS @(65) OR (49)
       LOGICAL    LID_CALSQQ
       LOGICAL    LID_PADDQQ
       LOGICAL    LIDIN_CDCQQ
       LOGICAL    LIDIN_EBCQQ
       LOGICAL    LIDIN_ASCQQ
       LOGICAL    LIDOUT_CDCQQ
       LOGICAL    LIDOUT_EBCQQ
       LOGICAL    LIDOUT_ASCQQ
       logical    LSCHED_EXTDQQ
 
C      . . . . . . . . . . . .   logical switches    . . . . . . . . .
C
       LOGICAL    LSTARTING_PROD
C
C
       LOGICAL       LSTRIPTITLQQ
       LOGICAL       LEOMAP
       LOGICAL       LEOFIL
       LOGICAL       LINEMT
       LOGICAL       MANYBLA
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
       CHARACTER*1     CONEBYT

       CHARACTER*1     NULL
C
       CHARACTER*1     KENDMAP
C ...       DATA          KENDMAP / '33'X /
       CHARACTER*1     KENDLIN
C ...       DATA          KENDLIN / '30'X /
C
C      ... FOR COMPARING INPUT RECORD-HEADERS IN EXTENDED 6-BIT FRMT:
       integer         KSTART_IFID
       data            KSTART_IFID   / X'3F3F3F3F00000000' /
C      ...                   WHICH IS START OF MAP W/ IFID BLOCK HEADER
       integer         KSTART_TITL
       data            KSTART_TITL   / X'3F3F3F3E00000000' /
C      ...                   WHICH IS START STRIP-TITLES BLOCK HEADER
       integer         KSTART_SCHED
       data            KSTART_SCHED  / X'3F3F3F3D00000000' /
C      ...                   WHICH IS THE START ISCHEDS BLOCK HEADER
       integer         KEND_ALLMAPS
       data            KEND_ALLMAPS  / X'3F3F3F3C00000000' /
C      ...                   WHICH IS THE END-OF-ALL-MAPS BLOCK HEADER

       INTEGER       KENDALLPKD
       DATA          KENDALLPKD    / X'FFFFFC0000000000' /
C
       integer       K2MANY
       DATA          K2MANY / 50 /
C      ... where K2MANY is limiting no. of blank scan lines to
C      ...   permit at end of map before holding up and looking
C      ...   ahead to see if it is indeed the end.
C
       INTEGER     ISPR
C      ... WHERE ISPR COUNTS BYTES PUT INTO C1SPRED
       INTEGER     ISPSAV
       INTEGER     NRECIN
C      ... WHERE NRECIN COUNTS THE 1920-BYTE PHYSICAL RECORDS INPUT
       INTEGER     NSCANLN
       INTEGER     NBLOCKOUT
C      ... WHERE NBLOCKOUT COUNTS BLOCKS THIS SUBROUTINE HAS
C      ...   FILLED UP 
       LOGICAL     LSTARTEDQQ    	!... HAS OUTPUT MAP STARTED ??
C      ...  FOR START OF MAP FOUND FLAG
       INTEGER     NBLSAV
C
        CHARACTER*1  C1IFIDCDC(48)


       SAVE
C
C     *     *     *     *     *     *     *     *     *     *    
C
C      . . .   S T A R T   . . . 
C      ...   INITIALIZATION   ...
       IERR = 0
C      ...           DEFINE BYTE CONSTANTS ...

       NULL = CHAR(0)

       KENDMAP = CHAR(51)  		!... = X'33' = END-OF-MAP
       KENDLIN = CHAR(48)   		!... = X'30' = END-OF-A-LINE
       
       write(6,105)MYOPTNBITS
  105  format(1h ,'REBLKFX4: started version dated 20-Jul-1999;',
     1            '  MYOPTNBITS=hex ',Z16,/)

       IF((LUNIX6T .LE. 0) .OR.
     1    (LUNIX6T .GT. 99)) THEN
         WRITE(6,115)LUNIX6T
  115    FORMAT(1H ,'REBLKFX4:FAILED ON BAD-VALUED INPUT DSRN ',
     1              'LUNIX6T =',I7)
         IERR = 1
         GO TO 999
       ENDIF

       IF((LUNIPK6 .LE. 0) .OR.
     1    (LUNIPK6 .GT. 99)) THEN
         WRITE(6,117)LUNIPK6
  117    FORMAT(1H ,'REBLKFX4:FAILED ON BAD-VALUED OUTPUT DSRN ',
     1              'LUNIPK6 =',I7)
         IERR = 2
         GO TO 999
       ENDIF

       REWIND  LUNIX6T

C      ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...

       IF(BTEST(MYOPTNBITS,0)) THEN
C         ... THERE IS A BIT AT THE (0) POSITION ...
          LENTIREQQ = .FALSE.   	!... SO MUST DO SEVERAL PANELS
          IF(BTEST(MYOPTNBITS,1)) THEN
C           ... THERE IS A BIT AT THE (1) POSITION ...
            LADD_ONQQ = .TRUE.    	!... ADD-ONTO EXISTING PRODUCT
          ELSE
C           ... THERE IS A ZERO AT THE (1) POSITION ...
            LADD_ONQQ = .FALSE.   	!... SO INITIALIZE FOR NEW PROD
          ENDIF
          IF(BTEST(MYOPTNBITS,2)) THEN
C           ... THERE IS A BIT AT THE (2) POSITION ...
            LEAVE_OPENQQ = .TRUE.  	!... AT END LEAVE WITH PROD OPEN
          ELSE
C           ... THERE IS A ZERO AT THE (2) POSITION ...
            LEAVE_OPENQQ = .FALSE. 	!... CLOSE AT END OF THIS PANEL
          ENDIF             
       ELSE
C         ... THERE IS A ZERO AT THE (0) POSITION ...
          LENTIREQQ = .TRUE.      	!... COMPLETE ENTITY IN ONE
          LADD_ONQQ = .FALSE.      	!...   SO INITIALIZE AT START
          LEAVE_OPENQQ = .FALSE.   	!...   AND CLOSE AT END
       ENDIF

       IF(BTEST(MYOPTNBITS,3)) THEN
C        ... THERE IS A BIT AT THE (3) POSITION ...
         LSKIPFFQQ = .FALSE.   		!... BIT3=1; TEST FOR FFFFFF
       ELSE
         LSKIPFFQQ = .TRUE.  		!... BIT3=0; DO NOT TEST FF
       ENDIF
      
       IF(BTEST(MYOPTNBITS,4)) THEN
C        ... THERE IS A BIT AT THE (4) POSITION ...
         LRASAT65QQ = .FALSE.   	!...BIT4=1; RASTER STARTS @(49)
       ELSE
         LRASAT65QQ = .TRUE.   		!...BIT4=0; RASTER STARTS @(65)
       ENDIF

       IF(BTEST(MYOPTNBITS,5)) THEN
C        ... THERE IS A BIT AT THE (5) POSITION ...
         LID_CALSQQ = .FALSE.   !...BIT5=1; FETCH IFID FROM RECORD(1)
       ELSE
         LID_CALSQQ = .TRUE.   	!...BIT5=0; FETCH IFID FROM CALL SEQ
       ENDIF

       IF(BTEST(MYOPTNBITS,6)) THEN
C        ... THERE IS A BIT AT THE (6) POSITION ...
         LID_PADDQQ = .FALSE.   !...BIT6=1; IFID IS NOT PADDED
       ELSE
         LID_PADDQQ = .TRUE.   	!...BIT6=0; IFID IS PADDED
       ENDIF


       LIDIN_CDCQQ = .FALSE.
       LIDIN_EBCQQ = .FALSE.
       LIDIN_ASCQQ = .FALSE.
       IF(BTEST(MYOPTNBITS,7)) THEN
C        ... THERE IS A BIT AT THE (7) POSITION ...
         LIDIN_CDCQQ = .FALSE.   !...BIT7=1; INPUT IFID IS .NOT. CDC
         IF(BTEST(MYOPTNBITS,8)) THEN
C          ... THERE IS A BIT AT THE (8) POSITION & 1 AT (7)
           LIDIN_EBCQQ = .TRUE.   !...BIT8=1; INPUT IFID IS EBCDIC
         ELSE
C          ... THERE IS A ZERO AT THE (8) POSITION & 1 AT (7)
           LIDIN_ASCQQ = .TRUE.
         ENDIF 
  
       ELSE
C        ... THERE IS A ZERO AT THE (7) POSITION ...
         LIDIN_CDCQQ = .TRUE.    !...BIT7=0; INPUT IFID IS CDC DISP CODE
       ENDIF


       LIDOUT_CDCQQ = .FALSE.
       LIDOUT_EBCQQ = .FALSE.
       LIDOUT_ASCQQ = .FALSE.
       IF(BTEST(MYOPTNBITS,9)) THEN
C        ... THERE IS A BIT AT THE (9) POSITION ...
         LIDOUT_CDCQQ = .FALSE.   !...BIT9=1; OUTPUT IFID IS .NOT. CDC
         IF(BTEST(MYOPTNBITS,10)) THEN
C          ... THERE IS A BIT AT THE (10) POSITION & 1 AT (9)
           LIDOUT_EBCQQ = .TRUE.   !...BIT10=1; OUTPUT IFID IS EBCDIC
         ELSE
C          ... THERE IS A ZERO AT THE (10) POSITION & 1 AT (9)
           LIDOUT_ASCQQ = .TRUE.
         ENDIF 
  
       ELSE
C        ... THERE IS A ZERO AT THE (9) POSITION ...
         LIDOUT_CDCQQ = .TRUE.    !...BIT9=0; OUTPUT IFID IS CDC DISP
       ENDIF


       IF(BTEST(MYOPTNBITS,16)) THEN
C        ... THERE IS A BIT AT THE (16) POSITION ...
         LSCHED_EXTDQQ = .FALSE.   	!...BIT16=1; isched data is 
C                                            packed concatenated I*2
       ELSE
         LSCHED_EXTDQQ = .TRUE.   	!...BIT17=0; isched data is 
C                                            I*2 extended into I*8 word
       ENDIF
       
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... watch out for multi-panel calls in which I must add onto
C      ...   existing partial product instead of initializing ... 
       LEOMAP = .FALSE.
       LEOFIL = .FALSE.
       LSTARTEDQQ = .FALSE.
C      ... FOR START OF MAP FOUND FLAG
       LSTRIPTITLQQ = .FALSE.
C      ... WHEN IN THE INNER LOOP PROCESSING EACH BYTE OF THE 6-BIT
C      ...    RLE ENCODED RASTER GRAPHCS, WHEN IN THE MAIN BODY OF 
C      ...    THE MAP, LSTRIPTITLQQ == .F.; OTHERWISE, WHEN 
C      ...    PROCESSING THE STRIP-TITLES, THEN LSTRIPTITLQQ == .T.
       NRECIN = 0
C      ... WHERE NRECIN COUNTS THE 1920-BYTE PHYSICAL RECORDS INPUT
       LSTARTING_PROD = .FALSE.
       IF(.NOT. LADD_ONQQ) THEN   	!... initialize
C        ... THIS IS EITHER AN ENTIRE PRODUCT IN ONE; OR 
C        ... THIS IS THE STARTING PANEL OF A MULTI-PANEL PRODUCT
         LSTARTING_PROD = .TRUE.
         NBLOCKOUT = 0
C        ... WHERE NBLOCKOUT COUNTS BLOCKS THIS SUBROUTINE HAS
C        ...   FILLED UP 
         NSCANLN = 0
C
         ISPR = 0
C          ... WHERE ISPR COUNTS BYTES PUT INTO C1SPRED

         JOFSCH = 2
C        ... WHERE POINTER INITIALIZED TO FIRST SCHED DATA IN JSCHED2D 
C        ...   IS NOT =1 SINCE THAT'S WHERE THE FFFFFD WORD IS.

         NUMPANEL = 0

       ELSE
C        ... THIS IS AN ADD-ON PANEL OF A MULTI-PANEL PRODUCT ...
         NUMPANEL = NUMPANEL + 1
         WRITE(6,FMT='(1H ,''reblkfx4: INITIALIZING FOR SECONDARY '',
     1                     ''PANEL-'',I2,'' OF A MULTI-PANEL PRODUCT'',
     2              /1H ,7X,''TRANSLATED-SCHED-COUNTER, JOFSCH='',I8)')
     A           NUMPANEL,JOFSCH
       ENDIF
C
C      ===============================================================
       IF(LSTARTING_PROD) THEN
C        ...  the ISCHED sorting and moving went into subr makfffd()

         call makfffd(iisched,LSCHED_EXTDQQ)

       ENDIF

C      ===============================================================
C
C
  200  CONTINUE
       LCKPT = 200
       NRECIN = NRECIN + 1
	i = 1
	do i = 1,INRECLINT
	    inbufA(i) = 0
	enddo
C
C      ... here comes first read of extended 6-bit file;
C      ...   expecting the start-of-map with IFIds ...

       READ(LUNIX6T,IOSTAT=IOERR,ERR=9405,END=900) inbufA
#if defined(_LITTLE_ENDIAN)
       CALL SWAP_BYTES_IN_PLACE (inbufA, 8_8, INRECLINT)
#endif
       NRECIN = NRECIN + 1
	go to 9407
C
 9405	continue
	write (6,9406) ioerr,NRECIN
 9406	format(1h ,'REBLKFX4: reading LUNIX6T got IOSTAT error = ',
     1              i4,
     2        /1H ,'          AFTER READING NRECIN=',I4,'  RECORDS')
	go to 980
 9407	continue
C
C
C ...      WRITE(6,205) (inbufA(I),I=1,INRECLINT)
  205  FORMAT(1H ,8Z9.8)
C
       IF(.NOT. LSKIPFFQQ) THEN
C        ... SO TEST FOR START-OF-MAP FLAGS HERE ...
C        ... THE FIRST RECORD MUST START WITH X'3F3F3F3F....'
         iword1 = IAND(inbufA(1),MSKLHS)

         IF(iword1 .NE. KSTART_IFID) THEN
C          ... ERROR: THIS FILE IS UNUSUAL.  FIRST RECORD IS NOT MARKED
C          ...   WITH THE START-OF-MAP FLAG ...
           GO TO 970
         ENDIF
       ENDIF
C
C      ... COMES HERE AFTER READING THE FIRST RECORD OF PASS(I) ...
C      ...    AND IF FIRST REC READ HAD STARTOFMAP FLAGS  ...
C      ...    OR ELSE, IF TOLD TO SKIP THE LOOKING FOR STARTOFMAP FLAGS
C      ...              AND ASSUME THIS IS THE FIRST RECORD OF PASS(I)
       IFR = 0
       LCKPT = 222

       IF(LSTARTING_PROD) THEN

         DO  I = 1,MAXEXTINT
           INTSPRED(I) = 0
         ENDDO
         ISPR = 64    		!... TO ALLOW SPACE FOR IFID

         IF(.NOT. LID_CALSQQ) THEN
C          ... TRY FOR IFID FROM RECORD 1 ... BUT ONLY IF STARTING PASS
           IF(LID_PADDQQ) THEN
C            ...   EXTRACT THE 64-BYTE BYTE PADDED IFID HEADER ...
             DO  IK = 1,8
               KHEADER(IK) = inbufA(IK)
             ENDDO
C
             WRITE(6,225) (KHEADER(I),I=1,8)
  225  FORMAT(1H ,'KHEADER (padded) FROM INPUT FILE=',
     1       /1H , 6X,4Z17.16,
     2       /1H , 6X,4Z17.16 )
C
           ELSE
C            ... IFID FROM RECORD 1 IS NOT PADDED ...
C            ...   EXTRACT THE 48-BYTE BYTE PADDED IFID HEADER ...
             DO  IK = 1,6
               KHEADER(IK) = inbufA(IK)
             ENDDO
C
             WRITE(6,2252) (KHEADER(I),I=1,6)
 2252  FORMAT(1H ,'KHEADER (NOT-padded) FROM INPUT FILE=',
     1       /1H , 6X,4Z17.16,
     2       /1H , 6X,2Z17.16 )
C            ... THEN I MUST PAD THIS 48-BYTE IFID ...
C ...       CALL MAKIFID(C1IFID)  ... removed

           ENDIF
C
         ELSE
C          ... COMES HERE IF I SHOULD OBTAIN IFID FROM CALL SEQ ARG
C ...      is the given ifid in CDC display code????
           IF(LIDIN_ASCQQ) THEN
C            ... TO CONVERT GIVEN IFID FROM ASCII TO CDC DISPLAY CODE,
             NCHIFID = 48
             CALL ASC2CDC(NCHIFID,C1IFID,C1IFIDCDC,IRET_A2C)

             CALL PADIFID(C1IFIDCDC,LHEADER)
           ELSE           

             CALL PADIFID(C1IFID,LHEADER)
           ENDIF
C
         ENDIF
#if defined (_LITTLE_ENDIAN)
         CALL SWAP_BYTES_IN_PLACE (KHEADER, 8_8, 8_8)
#endif
         KHEADER(1) = IAND(KHEADER(1),MSKRHS)
         KHEADER(1) =  IOR(KHEADER(1),KSTART_IFID)
         DO  I = 1,8
           INTSPRED(I) = KHEADER(I)
         ENDDO
         write(6,228)(INTSPRED(L),L=1,8)       	!... 8*8=64
  228  format(1h ,'REBLKFX4: IFID in extended form in C1SPRED=',
     1       /1H , 6X,4Z17.16,
     2       /1H , 6X,4Z17.16 )
#if defined (_LITTLE_ENDIAN)
         CALL SWAP_BYTES_IN_PLACE (INTSPRED, 8_8, 8_8)
#endif
C
       ENDIF

       LINEMT = .TRUE.
       MANYBLA = .FALSE.
       NROWBLA = 0
       NRUNBLA = 0
C
       IF(LRASAT65QQ) THEN
         IFR = 64   			!... used to be 48
       ELSE
         IFR = 48
       ENDIF

C      ... Since John Simmons' data has some leading blank
C      ... scanlines before the Fax map, skip those before
C      ... doing anything else.
       NBEFORE = 0
  230  CONTINUE
C      ...   we already have data in the work buffer CINBUF 
       LCKPT = 230

       M1 = IFR + 1
       DO  244  ICC = M1,INRECL
         CONEBYT = CINBUF(ICC)
         IF(CONEBYT .EQ. KENDLIN) THEN
           NBEFORE = NBEFORE + 1
           GO TO 244
         ENDIF
         IF(CONEBYT .EQ. KENDMAP) GO TO 930
C        ... which was empty map ...
C        ... OTHERWISE, HERE AT (ICC) IS VERY FIRST NON-BLANK
C        ...   SCANLINE ...
         IFR = ICC - 1
C        ... but, can I have one starting EOLN???
         IF(NBEFORE .GT. 0) THEN
           NBEFORE = NBEFORE - 1
           IFR = IFR - 1
         ENDIF
         WRITE(6,241) NBEFORE
  241    FORMAT(1H ,'REBLKFX4:DISCARDED LEADING BLANK SCANS N=',I5)
         GO TO 310
C        ... WHICH IS NORMAL WAY OUT OF THIS DISCARDING LOOP
  244  CONTINUE
C      ... ENTIRE BUFFER WAS BLANK SCANLINES ...
	do i = 1,INRECLINT
	    inbufA(i) = 0
	enddo
C
        READ(LUNIX6T,IOSTAT=IOERR,ERR=246,END=940) inbufA
#if defined(_LITTLE_ENDIAN)
        CALL SWAP_BYTES_IN_PLACE (inbufA, 8_8, INRECLINT)
#endif
	go to 248
C
  246	continue
	write (6,247) ioerr,NRECIN
  247	format(1h ,'REBLKFX4: at 246 read LUNIX6T got IOSTAT error= ',
     1              i4,
     2        /1H ,'          AFTER READING NRECIN=',I4,'  RECORDS')
	go to 980

  248	continue

       NRECIN = NRECIN + 1

       IFR = 0
       GO TO 230
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . .   outermost DO on reading input record at a time  . . .
C      . . . .         where input record is 1920-byte  .X6T data
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  300  CONTINUE
       LCKPT = 300
C
  310  CONTINUE
C      ... PROCESS JOHN SIMMONS' DATA HERE
C      ... LOOK FOR END-OF-SCANLINE FOR RESETING IISCHED VRBLS
C      ... LOOK FOR END-OF-MAP
C      ... MOVE BYTES FROM CINBUF INTO C1SPRED
       NBLSAV = NBLOCKOUT
       ISPSAV = ISPR + 1
C      ... TO SAVE BYTE- AND BLOCK- POINTER TO START OF THE 
C      ...   SCAN LINE TO COME.
C      
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . .   Mid-level  DO  to move each byte, one byte at a time,
C      . . . .       from input buffer, CINBUF, to work buffer, C1SPRED;
C      . . . .       examining each byte for special flags
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       M1 = IFR + 1
       DO 377   IC = M1,INRECL
         CONEBYT = CINBUF(IC)
         IF(MANYBLA) GO TO 340 
         IF(CONEBYT .EQ. KENDLIN) THEN
           GO TO 315
         ELSE IF(CONEBYT .EQ. KENDMAP) THEN
           LEOMAP = .TRUE.
           GO TO 477
         ENDIF
         LINEMT = .FALSE.
         GO TO 320
C
  315    CONTINUE
C        ... COMES HERE ON END_OF_SCANLINE ...
C        ...   HERE TEST FOR SUCCESSIVE BLANK SCANLINES WAS ADDED
         IF(LINEMT) THEN
           NRUNBLA = NRUNBLA + 1
           IF(NRUNBLA .LE. K2MANY) GO TO 317
C          ... OTHERWISE, THIS RUN OF BLANKS EXCEEDS K2MANY LINES
C          ...   SO I WILL NOT OUTPUT ANY MORE UNTIL IT VERIFIES
C          ...   AS THE END OF MAP OR NOT
           MANYBLA = .TRUE.
           NROWBLA = 1
           GO TO 377         	!... jump to end of DO; to get next byte

         ELSE
C          ... THIS END-OF-LINE FOR A NON-BLANK SCANLINE
           NRUNBLA = 0
           LINEMT = .TRUE.
         ENDIF
C
  317  CONTINUE
C
         IF(LSTRIPTITLQQ) THEN
           IF((JOFSCH-1) .GT. NTOTSCHED) THEN
             GO TO 319
C          ... WHICH TESTED FOR END-OF-SCHED ENTRIES ...
           ELSE
C            ... OTHERWISE, A SCHED ENTRY STILL REMAINS UNADDRESSED

             CALL REBLK700(NSCANLN,NBLSAV,ISPSAV,
     1                       NBLOCKOUT,ISPR)
           ENDIF
         ELSE
C          ... .NOT. STRIP-TITLE, SO WITHIN MAIN PART OF MAP ...
           IF((JOFSCH-1) .GT. NSCHED_CUT) THEN
             GO TO 319
C          ... WHICH TESTED FOR END-OF-SCHED ENTRIES ...
           ELSE
C            ... OTHERWISE, A SCHED ENTRY STILL REMAINS UNADDRESSED

             CALL REBLK700(NSCANLN,NBLSAV,ISPSAV,
     1                       NBLOCKOUT,ISPR)
           ENDIF

         ENDIF

  319    CONTINUE
C
         NSCANLN = NSCANLN + 1
  320    CONTINUE
C        ... COMES HERE TO STASH ONE BYTE INTO C1SPRED
         IF((ISPR+1) .GT. MAXEXTBYT) THEN
           LCKPT = 322

           CALL REBLK600(LUNIPK6,NUMRECFAX,INTSPRED,C1SPRED,ISPR,
     1                   C1OUTBF,JOUTBF,NBLOCKOUT,LSTARTEDQQ,IRET600)
           IF(IRET600 .NE. 0) GO TO 980

         ENDIF
C
         ISPR = ISPR + 1
         C1SPRED(ISPR) = CONEBYT
         GO TO 377
C
  340    CONTINUE
C        ... COMES HERE IF MANYBLA IS .TRUE. MEANING WE HAVE
C        ...   HIT A BATCH OF BLANK SCAN LINES, WHICH MIGHT BE
C        ...   CLOSE TO THE END OF MAP.
         IF(CONEBYT .EQ. KENDLIN) THEN
           NROWBLA = NROWBLA + 1
           GO TO 377
         ENDIF
C
         IF(CONEBYT .EQ. KENDMAP) GO TO 477
C        ... OTHERWISE, SHUCKS!  IS NON-BLANK LINE AFTER SO MANY
C        ... BLANKS.  I WILL HAVE TO OUTPUT THOSE BLANKS WHICH I
C        ... WAS MERELY COUNTING WITHOUT WRITING.
         DO  366  IBL = 1,NROWBLA
           IF(LSTRIPTITLQQ) THEN
             IF((JOFSCH-1) .GT. NTOTSCHED) THEN
               GO TO 344
C              ... WHICH TESTED FOR END-OF-SCHED ENTRIES ...
             ELSE
C              ... OTHERWISE, A SCHED ENTRY STILL REMAINS UNADDRESSED

               CALL REBLK700(NSCANLN,NBLSAV,ISPSAV,
     1                         NBLOCKOUT,ISPR)
             ENDIF
           ELSE
C            ... .NOT. STRIP-TITLE, SO WITHIN MAIN PART OF MAP ...
             IF((JOFSCH-1) .GT. NSCHED_CUT) THEN
               GO TO 344
C              ... WHICH TESTED FOR END-OF-SCHED ENTRIES ...
             ELSE
C              ... OTHERWISE, A SCHED ENTRY STILL REMAINS UNADDRESSED

               CALL REBLK700(NSCANLN,NBLSAV,ISPSAV,
     1                         NBLOCKOUT,ISPR)
             ENDIF

           ENDIF


  344      CONTINUE
C
           NSCANLN = NSCANLN + 1
           IF((ISPR+1) .GT. MAXEXTBYT) THEN
             LCKPT = 347

             CALL REBLK600(LUNIPK6,NUMRECFAX,INTSPRED,C1SPRED,ISPR,
     1                     C1OUTBF,JOUTBF,NBLOCKOUT,LSTARTEDQQ,IRET600)
             IF(IRET600 .NE. 0) GO TO 980

           ENDIF
           ISPR = ISPR + 1
           C1SPRED(ISPR) = KENDLIN
  366    continue
C        ...    which is ENDDO on IBL on blank rows ...  . . . 
C        . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C
         MANYBLA = .FALSE.
C        ... THEN RETURN TO NORMAL COURSE TO TRANSFER THE 
C        ...   CURRENT NON-BLANK CONEBYT INTO C1SPRED
         GO TO 320
C
  377  CONTINUE
C      . . . .   enddo on ic byte at a time thru one input record  . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C      ... WHEN IT FALLS OUT OF BOTTOM OF DO LOOP,
C      ...   I HAVE EXHAUSTED 1920-BYTE GIVEN-DATA ARRAY
       IF(LEOFIL) GO TO 477
C
C      ... WHEN CINBUF EMPTIES,
C      ...   AND IF NOT E-O-F YET, THEN 
C      ...     FETCH ANOTHER INPUT RECORD
C      ------------------------------------------------
C
       READ(LUNIX6T,IOSTAT=IOERR,ERR=380,END=384) inbufA
#if defined(_LITTLE_ENDIAN)
       CALL SWAP_BYTES_IN_PLACE (inbufA, 8_8, INRECLINT)
#endif

C      ... ON GOOD RECORD READ,
       NRECIN = NRECIN + 1
C      ... REINITIALIZE POINTERS FOR FRESH INPUT
       IFR = 0
       GO TO 300
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... OTHERWISE, BAD READ, SO ...
  380	continue
C       ... COMES TO 380 ON PARITY ERR ON READ JUST ABOVE ...
	write (6,382) ioerr,NRECIN
  382	format(1h ,'REBLKFX4: AT 382 read LUNIX6T got IOSTAT error=',
     1              i4,
     2        /1H ,'          AFTER READING NRECIN=',I4,'  RECORDS')
	go to 980

  384	continue
C       ... COMES TO 384 ON E-O-F ON READ JUST ABOVE ...
	write (6,386) NRECIN
  386	format(1h ,'REBLKFX4: AT 386 read LUNIX6T, hit EOF ',
     2        /1H ,'          AFTER READING NRECIN=',I4,'  RECORDS')
        LEOFIL = .TRUE.
        GO TO 477
C
C
  477  CONTINUE
C      ... COMES HERE OUT OF LOOP FOR NORMAL END OF PRODUCT
C      ... WATCH OUT FOR ENDOFMAP-BYTE RECORD FOLLOWED BY STRIP TITLES;
C      ... MUST DIFFERENTIATE BETWEEN ENDOFMAP AND ENDOFFILE
       LEOMAP = .TRUE.
       IF((ISPR+1) .GT. MAXEXTBYT) THEN
         LCKPT = 478

         CALL REBLK600(LUNIPK6,NUMRECFAX,INTSPRED,C1SPRED,ISPR,
     1                 C1OUTBF,JOUTBF,NBLOCKOUT,LSTARTEDQQ,IRET600)
         IF(IRET600 .NE. 0) GO TO 980

       ENDIF
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       IF(LEAVE_OPENQQ) THEN
         WRITE(6,478)NBLOCKOUT,ISPR,NSCANLN
  478    FORMAT(1H ,'REBLKFX4: NOT-FINAL PANEL COMPLETED WITH ',
     1              'NBLOCKOUT=',I5,'; ISPR=',I6,
     2         /1H ,'          NSCANLN=',I6,
     3              ';  LEAVING WITH INCOMPLETE PRODUCT')
         GO TO 999   		!... JUMP TO EXIT . . . . . . . 
       ENDIF
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
       ISPR = ISPR + 1
       C1SPRED(ISPR) = KENDMAP
C
C      ... OUTPUT THIS PARTIALLY FULL LAST DATA RECORD
C
       LCKPT = 488
C
       CALL REBLK600(LUNIPK6,NUMRECFAX,INTSPRED,C1SPRED,ISPR,
     1               C1OUTBF,JOUTBF,NBLOCKOUT,LSTARTEDQQ,IRET600)
       IF(IRET600 .NE. 0) GO TO 980
C
       WRITE(6,479) NRECIN,NSCANLN,NBLOCKOUT
  479  FORMAT(1H ,'REBLKFX4:NORMAL ENDING OF FAX  PRODUCT',
     1       /1H ,'WAS FOUND IN LRECORD NO.',I4,
     2       /1H ,'AT END TOTAL SCAN_LINES=',I6,'   NBLOCKOUTS=',I4)
C
       IF(MANYBLA) THEN
         WRITE(6,485) NROWBLA
  485    FORMAT(1H ,'REBLKFX4:DISCARDED TRAILING BLANK SCANS N=',I6)
       ENDIF

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... must process the strip titles, just like preceding graphic
C      ... data, before going on to FFFFFD-headed record
       IF(.NOT. LSTRIPTITLQQ) THEN

         IF(LEOFIL) GO TO 490
C
C        ...   AND IF NOT E-O-F YET, THEN 
C        ...     FETCH ANOTHER INPUT RECORD
C        ...   EXPECT THE STRIP TITLES HERE
C        ...   BUT ONLY IF THIS IS AN ENTIRE PRODUCT FILE
C        ...      OF IF THIS IS THE LAST PANEL OF A MULTI-PANEL PROD
C
         IF(.NOT. LEAVE_OPENQQ) THEN
           READ(LUNIX6T,IOSTAT=IOERR,ERR=4852,END=4855) inbufA
#if defined(_LITTLE_ENDIAN)
           CALL SWAP_BYTES_IN_PLACE (inbufA, 8_8, INRECLINT)
#endif
           GO TO 488   		!... was good read ...
         ENDIF
C        . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C        ... OTHERWISE, BAD READ, SO ...
 4852	 continue
C        ... COMES TO 4852 ON PARITY ERR ON READ JUST ABOVE ...
	 write (6,4854) ioerr,NRECIN
 4854	 format(1h ,'REBLKFX4: AT 4854 read LUNIX6T got IOSTAT error=',
     1              i4,
     2        /1H ,'          AFTER READING NRECIN=',I4,'  RECORDS')
	 go to 980

 4855	 continue
C        ... COMES TO 4855 ON E-O-F ON READ JUST ABOVE ...
	 write (6,486) NRECIN
  486	 format(1h ,'REBLKFX4: AT 486 read LUNIX6T, hit EOF ',
     2         /1H ,'          AFTER READING NRECIN=',I4,'  RECORDS')
         LEOFIL = .TRUE.
         GO TO 490
C        . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

  488    CONTINUE
C        ... ON GOOD RECORD READ,
         NRECIN = NRECIN + 1
C        ... IS IT THE STRIP-TITLES WHICH I AM EXPECTING???
         call byteswap(inbufA(1), 8, 1)
         IWORD1 = IAND(inbufA(1),MSKLHS)
         call byteswap(inbufA(1), 8, 1)
         IF(IWORD1 .EQ. KSTART_TITL) THEN
C          ... YES!  HERE IS THE START OF THE STRIP TITLES ...
           LSTRIPTITLQQ = .TRUE.
           NSCANLN = 7400  	!... SO THAT CURRENT LN WILL MATCH SCHED
C          ... REINITIALIZE POINTERS FOR FRESH INPUT TITLES
           IFR = 4 		!... ptr to source
           DO  I = 1,MAXEXTINT
             INTSPRED(I) = 0
           ENDDO
           intspred(1) = kstart_titl   		!... block header titles
           call byteswap(intspred(1), 8, 1)
           ispr = 4   		!... ptr to destination
           GO TO 300   		!... jump way back to do striptitl

         ELSE IF(IWORD1 .EQ. KSTART_SCHED) THEN
           GO TO 490
         ELSE IF(IWORD1 .EQ. KEND_ALLMAPS) THEN
           GO TO 490
         ENDIF
         GO TO 490

       ENDIF
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C      ... OUTPUT THE FFFFFD-HEADED JSCHEDS ARRAY HERE ...
C      ...   IF INTERGRAPH VERSION, SWAP THE BYTES FIRST
C      ...   ELSE IF CRAY VERSION, COMPRESS THE JSCHED2D ARRAY
C      ...     BY QUARTER-PACKING THE ARRAY
  490  CONTINUE
       do  i = 1,nsizdes
         JSCHEDS(i) = 0
       enddo
       noffset = 0
       nbitpgrp = 16
       nbitskip = 0
       ngrps2do = 8*(ntotsched + 1)
       call sbytesccs(JSCHEDS,jsched2d,noffset,nbitpgrp,nbitskip,
     1             ngrps2do)
C
       
       NBLOCKOUT = NBLOCKOUT + 1
C
          NUMRECFAX  = NUMRECFAX + 1
#if defined(_LITTLE_ENDIAN)
           CALL SWAP_BYTES_IN_PLACE (JSCHEDS, 8_8, 180_8)
#endif
           WRITE(LUNIPK6, REC=NUMRECFAX,ERR=920 )C1JSCHED
           
C       write(LUNIPK6,ERR=920)JSCHEDS
C
C      ... WHICH OUTPUT THE FFFFFD-ARRAY 
C
C      ... OUTPUT THE FFFFFC-HEADED END_OF_ALL_MAPS ARRAY HERE ...
       DO  I = 1,180
         JOUTBF(I) = 0
       ENDDO
C      JOUTBF(1) = KENDALLPKD     	!... END-OF-ALL-MAPS IN FILE
       LCKPT = 495
          NUMRECFAX  = NUMRECFAX + 1
#if defined(_LITTLE_ENDIAN)
           CALL SWAP_BYTES_IN_PLACE (JOUTBF, 8_8, 180_8)
#endif
           WRITE(LUNIPK6, REC=NUMRECFAX,ERR=920 )C1OUTBF
           
C      write(LUNIPK6,ERR=980)JOUTBF

C
       GO TO 999
C
C     *     *     *     *     *     *     *     *     *
C
C     *     *     *     *     *     *     *     *     *
C
  900  CONTINUE
C      ... COMES HERE IF END-OF-FILE  ENCOUNTERED ON INPUT FILE
       WRITE(6,905) LUNIX6T,LCKPT,NRECIN
  905  FORMAT(1H ,'REBLKFX4:HIT EOF ON FAX  INPUT FILE DSRN=',I3,
     1       /1H ,'   AFTER CHECKPOINT=', I5,
     2            '  ON LRECORD NO.', I4)
       GO TO 980
C
C
  920  CONTINUE
C      ... COMES HERE IF WRITE PARITY ERROR ON WRITING TO 
C      ...   UNIT = LUNIPK6
C
       WRITE(6,925) LUNIPK6,NBLOCKOUT
  925  FORMAT(1H ,'*** ERROR WHILE TRYING TO WRITE TO UNIT=',
     1        I3,/1H ,5X,'KBLOCK = ', I3)
C
       GO TO 980
C
  930  CONTINUE
C      ... COMES HERE IF GIVEN AN ALL-BLANK MAP
       WRITE(6,935) NBEFORE
  935  FORMAT(1H ,'REBLKFX4: GIVEN AN EMPTY MAP; NO FAX MAP.',
     1       /1H ,'         BLANK SCAN_LINE COUNT =', I5)
       IERR= 3
       GO TO 999
C
  940  CONTINUE
C      ... COMES HERE IF GIVEN EMPTY MAP, BADLY FORMATTED
       WRITE(6,945) NBEFORE
  945  FORMAT(1H ,'REBLKFX4: GIVEN AN EMPTY MAP; NO FAX MAP.',
     1       /1H ,'  BAD FORMAT.  HIT PHYSICAL EOF. NBEFORE=',I5)
       IERR = 4
       GO TO 999
C
  966  CONTINUE
C      ... COMES HERE IF SCANNED THRU FINAL REC W/O FINDING
C      ...   THE '33'X END...
       WRITE(6,968) LUNIX6T,LCKPT,NRECIN                      
  968  FORMAT(1H ,'REBLKFX4:ERRONEOUS FORMAT IN FAXS FILE ON DSRN=',
     A             I3,
     1       /1H ,'   NO HEX33 END-MARK FOUND IN FINAL REC NO.',
     2            I4)
       GO TO 980
C
  970  CONTINUE
C      ... COMES HERE IF NO VALID 1ST FULLWORD ON FIRST RECORD
       WRITE(6,975)inbufA(1)
  975  FORMAT(1H ,'REBLKFX4:** ERROR STOP.  INVALID X6T FILE HEADER.',
     1            ' WORD(1)= X', Z17.16)
C
       GO TO 980
C
  980  CONTINUE
C
       IERR = 1
       GO TO 999
  999  CONTINUE
       RETURN
       END
       SUBROUTINE REBLK600(LUNIPK6,NUMRECFAX,INTSPRED,C1SPRED,ISPR,
     1                     C1OUTBF,JOUTBF,NBLOCKOUT,LSTARTEDQQ,IRET600)
C      ... GIVEN: ONE FULL (USUALLY FULL) 1920-BYTE BIN
C      ...           (FULL, EXCEPT WHEN FLUSHING LAST DATA RECORD)
C      ... TASK:  COMPRESS INTO 1440-BYTES, THEN OUTPUT 
C      ... WHERE INTSPRED MUST EQUIVALENCED TO C1SPRED IN CALLER,S 
C      ... WHERE JOUTBF   MUST EQUIVALENCED TO C1OUTBF IN CALLER,S
 
       INTEGER       NBYTPWRD
       PARAMETER    (NBYTPWRD=8)  		!... CRAY 8-BYTE INT

       INTEGER       NMCSTDRECL   		!... = 1440 BYTES
       PARAMETER    (NMCSTDRECL=1440)

       INTEGER       MAXEXTBYT
       PARAMETER    (MAXEXTBYT=1920)
       INTEGER       MAXEXTINT    		!...240 I*8 WRDS =1920/8
       PARAMETER    (MAXEXTINT=MAXEXTBYT/NBYTPWRD)

C      ... WHERE 1920 BYTES IS CAPACITY OF C1SPRED ARRAY
C      ...    WHICH IS EQUIV TO 1440-BYTES IN COMPRESSED 6-BITS
C      ...    EXTENDED TO 8-BITS PER 6-BIT ITEM.

       INTEGER       LUNIPK6

       INTEGER       INTSPRED(MAXEXTINT)
       CHARACTER*1   C1SPRED(MAXEXTBYT)
C      ... where  EQUIV (INTSPRED,C1SPRED) ... MUST BE IN CALLER,S

       INTEGER       JOUTBF(180)    		!... I*4 WAS (360)
       CHARACTER*1   C1OUTBF(NMCSTDRECL)
C      ... WHERE JOUTBF MUST EQUIVALENCED TO C1OUTBF IN CALLER,S
 

       INTEGER      NBLOCKOUT
       LOGICAL      LSTARTEDQQ
       INTEGER      IRET600

       CHARACTER*1    NULL

       SAVE 

C     *     *     *     *     *     *     *     *     *

       IRET600 = 0
       NULL = CHAR(0)
C
C
  600  CONTINUE
C
       IF(ISPR .LE. 0) GO TO 666
C      ... IF NOTHING WAS MOVED INTO C1SPRED, DON'T OUTPUT ANY
C ...      WRITE(6,611) LCKPT
  611  FORMAT(1H ,'REBLKFX4 IS WRITING BLOCK AT CHECKPT=', I4)
C
       IF(ISPR .LT. MAXEXTBYT) THEN
C        ... ZERO REMAINDER OF BUFFER IF PARTIALLY FULL
         MM1 = ISPR + 1
         DO  I = MM1,MAXEXTBYT
           C1SPRED(I) = NULL
         ENDDO
       ENDIF
C
C      ... TO COMPRESS THE 6-BITS-IN-8-BITS INTO CONCATENATED
C      ...   6-BITS,
#if defined(_LITTLE_ENDIAN)
       CALL SWAP_BYTES_IN_PLACE (INTSPRED, 8_8, MAXEXTINT)
#endif
       CALL PAK8TO6(INTSPRED,JOUTBF)
C
C
       NBLOCKOUT = NBLOCKOUT + 1
C ...      IF(NBLOCKOUT .LE. 1) THEN
C        ... DUMP THE FIRST C1OUTBF TO SEE IF IT STARTS W/ FFFFFF
C ...        WRITE(6,624)(JOUTBF(I),I=1,360)
C 624    FORMAT(1H ,'REBLKFX4: FIRST OUTPUT BUFFER CONTAINS ...',
C    1         /1H ,(8Z9.8))
C ...      ENDIF
C
C      WHERE EACH FAX RECORD IS 1440 BYTES

          NUMRECFAX  = NUMRECFAX + 1
#if defined(_LITTLE_ENDIAN)
           CALL SWAP_BYTES_IN_PLACE (JOUTBF, 8_8, 180_8)
#endif
           WRITE(LUNIPK6, REC=NUMRECFAX,ERR=920 )C1OUTBF
           
C       WRITE(LUNIPK6,ERR=920)JOUTBF

C
  666  CONTINUE
C
       do  i = 1,MAXEXTINT
         intspred(i) = 0
       enddo
       ISPR = 0
       GO TO 999
C
C     *     *     *     *     *     *     *     *     *
C
  920  CONTINUE
C      ... COMES HERE IF WRITE PARITY ERROR ON WRITING TO 
C      ...   UNIT = LUNIPK6
C
       WRITE(6,925) LUNIPK6,NBLOCKOUT
  925  FORMAT(1H ,'REBLK600: ERROR WHILE TRYING TO WRITE TO UNIT=',
     1        I3,/1H ,5X,'KBLOCK = ', I3)
C
       GO TO 980
C
  980  CONTINUE
C
       IRET600 = 1
       GO TO 999

  999  CONTINUE
       RETURN
       END

       SUBROUTINE REBLK700(NSCANLN,NBLSAV,ISPSAV,
     1                     NBLOCKOUT,ISPR)
C      ... CHANGED FROM ASSIGNED GOTO 700 TO SUBR ...   30-APR1996/DSS
C     *     *     *     *     *     *     *     *     *
C
       INTEGER       MXSCHED
       PARAMETER    (MXSCHED=60)
C      ... MAX NO. OF SUBSETS/INSETS DEFINED IN GIVEN IISCHED ARRAY.
C      ... BEDIENT USES 59
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       COMMON  /XSCHEDS/NTOTSCHED,NSCHED_CUT,NSCHED_TITL,JOFSCH,
     1                  NUMPANEL,JSCHED2D,JLINSRTED
       INTEGER       NTOTSCHED 
       INTEGER       NSCHED_CUT
       INTEGER       NSCHED_TITL
       INTEGER       JOFSCH
       INTEGER       NUMPANEL
       INTEGER       JSCHED2D(8,MXSCHED)
       INTEGER       JLINSRTED(2,MXSCHED)

C      ...   SUBSET DEFS IN JSCHED2D WHOSE POINTERS REMAIN TO
C      ...   BE DEFINED = F(STARTING SCAN LINE)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       INTEGER       MAXEXTBYT
       PARAMETER    (MAXEXTBYT=1920)
C      ... WHERE 1920 BYTES IS CAPACITY OF C1SPRED ARRAY
C      ...    WHICH IS EQUIV TO 1440-BYTES IN COMPRESSED 6-BITS
C      ...    EXTENDED TO 8-BITS PER 6-BIT ITEM.

       INTEGER   NSCANLN,NBLSAV,ISPSAV,NBLOCKOUT,ISPR

         SAVE

  700    CONTINUE
C        ... COMES HERE ON END_OF_SCANLINE ...
         JWHICH = JOFSCH - 1
         IF(JWHICH .GT. NTOTSCHED) THEN
           GO TO 719
C          ... WHICH TESTED FOR END-OF-SCHED ENTRIES ...
         ENDIF

C        ... WHAT ABOUT THE STRIP-TITLE VALUES UP THERE ABOVE 7400???
C        ... HOW ARE YOU EVER GOING TO MATCH AGAINS THOSE ???
C        ... BY RESETTING NSCANLN=7400 IN MAIN WHEN GOING BACK THRU
C        ... THE STRIPTITLE RLE BLOCK


C        ... OTHERWISE, A SCHED ENTRY STILL REMAINS UNADDRESSED
         IF(NSCANLN .LT. JLINSRTED(1,JWHICH)) GO TO 717
C        ... OTHERWISE, CURRENT SCANLINE IS A MATCH OF THIS
C        ...   SCHED ENTRY'S STARTING SCANLINE ...
C          ... SO PUT POINTERS IN PLACE OF STARTING SCAN LINE.
           JPTR = JLINSRTED(2,JWHICH)
           JSCHED2D(2,JPTR) = NBLSAV
           IF(ISPSAV .GT. 0) THEN
             JSCHED2D(3,JPTR) = ISPSAV - 1
           ELSE
C            ... FAIL-SAFE TO PREVENT NEGATIVE-VALUED BYTE-POINTER ...
             JSCHED2D(3,JPTR) = 0
           ENDIF
C          ... WHERE BYTE POINTER DECREMENTED BECAUSE BEDIENT
C          ...   WANTS RELATIVE BYTE POINTER.
           JOFSCH = JOFSCH + 1
           GO TO 700
C          ... WHICH GOES BACK TO SEE IF ANOTHER SCHED ENTRY
C          ...   EXISTS WITH SAME STARTING SCANLINE.
C
  717    CONTINUE
C        ... COMES HERE ONLY IF SOME SCHED ENTRY REMAINS
C        ...   UNADDRESSED, AND IF THE CURRENT LINE DID NOT
C        ...   MATCH.
C        ... SAVE POINTER TO THE START OF NEXT SCAN LINE.
           NBLSAV = NBLOCKOUT
           ISPSAV = ISPR + 2
           IF(ISPSAV .GT. MAXEXTBYT) THEN
             ISPSAV = 1
             NBLSAV = NBLSAV + 1
           ENDIF
C
  719    CONTINUE
       RETURN
       END
       
       SUBROUTINE MAKFFFD(IISCHED,LSCHED_EXTDQQ)
C      ...  GIVEN: (1.)IISCHED ARRAY DEFINING EACH SUBSET AND REAL-INSET
C      ...                     AND STRIP-TITLE-INSET IN THIS PRODUCT
C      ...         (2.)LSCHED_EXTDQQ -- LOGICAL SWITCH TO TELL ME
C                              WHICH OF TWO DATA FORMATS IS ISCHED IN:
C                         == .T.; EACH 16-BIT "WORD" IS IN THE LOW-ORDER
C                                 16-BITS OF EACH 64-BIT INTEGER;
C                         == .F.; THE 16-BIT "WORDS" ARE CONCATENATED
C      ...  RESULTS:  INTO WORK ARRAYS AND COUNTERS IN /XSCHEDS/
C
C      ...  WHAT DOES THIS DO?
C      ...    (A.) ZEROS THE JSCHED2D WORKSPACE FOR THE FFFFFD-HEADED 
C      ...            ISCHED TRAILER-RECORD ARRAY;
C      ...    (B.) COPIES THE GIVEN IISCHED DATA INTO JSCHED2D
C      ...            WITH DATA STILL IN THE EXTENDED FORMAT;
C      ...            WHY EVEN COPY IT THEN???  BECAUSE I WILL CHANGE
C      ...            CONTENTS, AND I DO NOT WANT TO CHANGE THE GIVEN;
C      ...    (C.) CREATES AND SORTS A SORTKEY ARRAY:
C      ...            CONTAINING THE STARTING SCANLINE J-VALUE
C      ...            AND A POINTER INTO THE JSCHED2D-ARRAY
C                    (DOES NOT REARRANGE THE DATA WITHIN JSCHED2D.)
C      ...          
C      ...    
C      ...  THAT IS ALL IT DOES.   IT IS NOT NOT NOT READY TO GO.
C      ...  THE STARTING SCANLINE NUMBER HAS NOT BEEN CONVERTED TO
C      ...     A POINTER BY BLOCK NUMBER AND BYTE NUMBER WITHIN BLOCK.
C      ...  SINCE THE CRAY DOES NOT HAVE INTEGER*2, THE RESULTING
C      ...     JSCHED2D ARRAY IS AN EXTENDED VERSION IN WHICH EACH
C      ...     VALUE IS IN THE LOW-ORDER 16-BITS OF EACH 64-BIT WORD;
C      ...     SO IT STILL NEEDS A CALL SBYTESCCS() TO PACK THE I*2 DATA

C      ...  WHY DO THIS?
C      ...     SOMEWHERE ELSE IS THE LOGIC FOR CONVERTING THE SCANLINE
C      ...     NUMBER INTO A POINTER BY BLOCK NUMBER AND BYTE NUMBER.
C      ...     WHEN IT DOES THAT, IT WILL BE EASIER TO DO THAT IF THE
C      ...     SCANLINE NUMBERS ARE SORTED. 
C      ================================================================
C      ...     SHOULD I SORT THE POINTERS ONLY AND LEAVE THE GIVEN
C      ...     ITEM SEQUNCE INTACT??? (Yes!)
  
C      ...     DOES BEDIENT CHANGE THE ARRANGEMENT? (No!)
C      ...     SEE EXAMPLE SOMEWHERE!!!!  (See "isched.doc")
C      ...
C      ...     I looked at an FT01 file and the isched 8-iiword sets
C      ...     are not sorted; they are in their original given sequence
C      ...     so I should sort only the pointer array.  So the logic
C      ...     in this subroutine must be changed.    (1-May-1996/dss)
C
C      ================================================================
C      ...     I SPLIT THE ISCHED MILLING ARND INTO A SUBR.
C      ...     THE ISCHED DATA IS ESSENTIALLY 16-BIT I*2 WORD ORIENTED
C      ...     HOW CAN I DO THIS IN THE I*8 WORD OF CRAY?
C      ...     THE FFFFFD-HEADED TRAILER RECORD IS NOT 6-BIT PACKED;
C      ...     IT IS I*2 JSCHEDS(720) = 720 I*2 INTEGER VALUES
C      ...     IN SETS OF 8 I*2 INTEGERS PER LOGICAL MAP SUBSET;
C      ...     IF, ON THE CRAY, WE WORK IN A SPACE OF I*8 JSCHEDS(720)
C      ...     IN WHICH THE DATA IS IN THE LOW-ORDER 16-BITS OF EVERY
C      ...     LONGWORD, THEN ONLY AT THE VERY END, JUST BEFORE OUTPUT
C      ...     I COULD QUARTER-PACK IT.
C
C      ... INITIALIZE FFFFFD-HEADED RECORD HERE
C      ...   SORT SUBSET_DEFINITIONS BY THEIR STARTING SCANLINE;
C      ...   COUNT THE NUMBER OF SUBSETS;
C      ...   INITIALIZE FLAGS.


C
       INTEGER       MXSCHED
       PARAMETER    (MXSCHED=60)
C      ... MAX NO. OF SUBSETS/INSETS DEFINED IN GIVEN IISCHED ARRAY.
C      ... BEDIENT USES 59

       INTEGER       LIMITSETS
       PARAMETER    (LIMITSETS=MXSCHED-2)
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       COMMON  /XSCHEDS/NTOTSCHED,NSCHED_CUT,NSCHED_TITL,JOFSCH,
     1                  NUMPANEL,JSCHED2D,JLINSRTED
       INTEGER       NTOTSCHED 
       INTEGER       NSCHED_CUT
       INTEGER       NSCHED_TITL
       INTEGER       JOFSCH
       INTEGER       NUMPANEL
       INTEGER       JSCHED2D(8,MXSCHED)
       INTEGER       JLINSRTED(2,MXSCHED)

C      ...   SUBSET DEFS IN JSCHED2D WHOSE POINTERS REMAIN TO
C      ...   BE DEFINED = F(STARTING SCAN LINE)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       INTEGER        IISCHED(8,MXSCHED)
C      ... WHERE IISCHED IS GIVEN AS ARG1
C      ...   AND IS IN THE SAME FORMAT AS IN CALL TO CNTR
       LOGICAL        LSCHED_EXTDQQ   		!... ARG2


       INTEGER       ISORKEY(MXSCHED)
C
       INTEGER       KIISCHED(8)
C      ... WHERE KIISCHED IS A CANNED SUBSET DEF. IN CASE
C      ...   THE GIVEN IISCHED IS BAD

       DATA     KIISCHED / 150, 0,  0,1200, X'8000',X'D800', 0,0 /
C      ... WHERE DUMMY KIISCHED USES SUBSET NO. 150 (MY CHECKOUT)

       INTEGER       I8HOLD(2)
       CHARACTER*1   C1HOLD(16)
       EQUIVALENCE  (I8HOLD(1),C1HOLD(1))

       INTEGER       IPAIR(2)
       INTEGER       IIWRDEXT(8)
       INTEGER       MSKLHS
       DATA          MSKLHS   / X'FFFFFFFF00000000' /             
       INTEGER       MSKRHS
       DATA          MSKRHS   / X'00000000FFFFFFFF' /

       INTEGER       JLINE_VAL
C
       SAVE
C
C      . . .   S T A R T   . . . . . . . . . . . . . . . . . . . . . .
C
       write(6,151)LSCHED_EXTDQQ
  151  format(1h ,'reblkfx4::makfffd: entered with LSCHED_EXTDQQ = .',
     1            L1,'.')

       NTOTSCHED = 0

       DO  J = 1,MXSCHED  			!... MXSCHED=60
         ISORKEY(J) = 0
       ENDDO

       DO  J = 1,MXSCHED
         JLINSRTED(1,MXSCHED) = 0
         JLINSRTED(2,MXSCHED) = 0
       ENDDO

C
C ...       INTEGER       JSCHED2D(8,90)
       DO  J = 1,MXSCHED  			!... MXSCHED=60
         DO  I = 1,8
           JSCHED2D(I,J) = 0
         ENDDO
       ENDDO
C
       JSCHED2D(1,1) = X'FFFF'
       JSCHED2D(2,1) = X'FD00'
C
       NTOTSCHED = 0
       IF(LSCHED_EXTDQQ)THEN
C        ... M2 IS SET TO ENSURE THERE WILL BE A ZERO-VALUE WITHIN
C        ...         MXSCHED-SIZED ARRAYS BEYOND LAST NON-ZERO ITEM
         M2 = MXSCHED - 2     
         DO  JJ = 1,M2
           IF((IISCHED(1,JJ) .EQ. 0) .AND.
     1        (IISCHED(2,JJ) .EQ. 0)) THEN
             GO TO 154
           ENDIF
           JSTARTLINE = IISCHED(2,JJ)
           IF(JSTARTLINE .GE. 8200) THEN
C            ... BEYOND FAX STRIP TITLES INTO AFOS ...
C            ... AND I AM NOT DOING AFOS, SO JUMP OUT
             GO TO 154
           ENDIF

C          ... OTHERWISE, THIS ITEM IS NOT THE END OF ISCHED YET
           NTOTSCHED = NTOTSCHED + 1

           DO  II = 1,8
             JSCHED2D(II,JJ+1) = IISCHED(II,JJ)
           ENDDO

           IACC = JSTARTLINE  	   	!... J-VAL OF STARTING SCANLN
           IACC = ISHFT(IACC,32)    	!... SORT KEY IN HI-ORDER HAFWD
           JPTR = JJ + 1                !... PTR INTO JSCHED2D ARRAY
           ISORKEY(JJ) = IOR(IACC,JPTR)	!... /SORTKEY/PTR/
         ENDDO
         GO TO 154

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
       ELSE
C        ... COMES HERE IF LSCHED_EXTDQQ == .F. 
C        ... DATA WITHIN IISCHED(8,J) IS REALLY CONCATENATED I*2 WORDS
         NTOTSCHED = 0
         DO  JJ = 1,15     	!... 60/4SETS PER 8I*8 = 15
           DO  II = 1,8,2      	!... 2 I*8 INTS == 8 I*2 WORDS
C            ... FETCH NEXT TWO I*8 WORDS OUT OF IISCHED() ARRAY ...
             I8HOLD(1) = IISCHED(II,JJ)
             I8HOLD(2) = IISCHED(II+1,JJ)
             IACC = IAND(I8HOLD(1),MSKLHS)
             IF(IACC .EQ. 0) THEN
C              ... THIS IS THE LAST DATA ITEM TO TRANSFER ...
               GO TO 154
             ENDIF

             IF((NTOTSCHED+1) .GT. LIMITSETS) THEN
               GO TO 154
             ENDIF

             DO  IDE = 1,8
               IBY1 = 2*(IDE-1) + 1
               IBY2 = IBY1 + 1
               IPAIR(1) = MOVA2I(C1HOLD(IBY1))
               IPAIR(1) = ISHFT(IPAIR(1),8)
               IPAIR(2) = MOVA2I(C1HOLD(IBY2))
               IIWRDEXT(IDE) = IOR(IPAIR(1),IPAIR(2))
             ENDDO
C            ... NOW NON-ZERO 8-WORD SET IN (IIWRDEXT(I),I=1,8)
C            ...   JUST LIKE A SET FROM EXTENDED IISCHED ...
             JSTARTLINE = IIWRDEXT(2)
             IF(JSTARTLINE .GE. 8200) THEN
C              ... BEYOND FAX STRIP TITLES INTO AFOS ...
C              ... AND I AM NOT DOING AFOS, SO JUMP OUT
               GO TO 154
             ENDIF

C            ... OTHERWISE, THIS ITEM IS NOT THE END OF ISCHED YET
             NTOTSCHED = NTOTSCHED + 1

             DO  II2 = 1,8
               JSCHED2D(II2,NTOTSCHED+1) = IIWRDEXT(II2)
             ENDDO

             IACC = JSTARTLINE     	!... J-VAL OF STARTING SCANLN
             IACC = ISHFT(IACC,32)    	!... SORT KEY IN HI-ORDER HAFWD
             JPTR = NTOTSCHED + 1       !... PTR INTO JSCHED2D ARRAY
             ISORKEY(NTOTSCHED) = IOR(IACC,JPTR)	
C                                       !... /SORTKEY/PTR/
           ENDDO
         ENDDO
         GO TO 154

       ENDIF
C
  154  CONTINUE
C      ... ALL SUBSET DEFINITIONS HAVE BEEN COPIED INTO JSCHED2D
C      ... AND NTOTSCHED CONTAINS THE COUNT OF SUBSETS/INSETS.

       WRITE(6,157) NTOTSCHED
  157  FORMAT(1H ,'REBLKFX4:IN GIVEN IISCHED, NO. OF _SETS=',I3)
C
       IF(NTOTSCHED .LE. 0) THEN
C        ... BAD IISCHED WAS GIVEN; BUT LET'S TRY TO CONTINUE
C        ... WITH A CANNED IISCHED FOUND IN KIISCHED
         WRITE(6,159)
  159    FORMAT(1H ,'REBLKFX4:-W- BAD IISCHED.  PROCEEDING ANYWAY.')
         DO  I = 1,8
           JSCHED2D(I,2) = KIISCHED(I)
         ENDDO
C
         JSCHED2D(1,3) = 0
         JSCHED2D(2,3) = 0
C
         JLINSRTED(1,1) = KIISCHED(2)  	!... JLINE VAL
         JLINSRTED(2,1) = 2 		!... PTR
C
         NTOTSCHED = 1
         NSCHED_CUT = 1
         GO TO 177
       ENDIF
C
C      ... IF ONLY ONE GOOD ITEM IN IISCHEDS HAS BEEN MOVED, THEN 
C      ...     BYPASS THE SORT
       IF(NTOTSCHED .LE. 1) THEN
         JSCHED2D(1,3) = 0
         JSCHED2D(2,3) = 0
C
         JLINSRTED(1,1) = JSCHED2D(2,2) 	!... JLINE VAL
         JLINSRTED(2,1) = 2 			!... PTR
C
         NTOTSCHED = 1
         NSCHED_CUT = 1
         GO TO 177
       ENDIF

C      ... OTHERWISE,TO SORT THE SUBSET DEFS BY STARTING SCAN LINE,

       CALL PIKSOR(ISORKEY,NTOTSCHED)

       WRITE(6,165) NTOTSCHED 
  165  FORMAT(1H ,'REBLKFX4::MAKFFFD: DID PIKSOR. NTOTSCHED=',I4)
C
       NSCHED_CUT  = 0
       NSCHED_TITL = 0
       DO  IS = 1,NTOTSCHED
         IACC = ISORKEY(IS)
         JPTR = IAND(IACC,MSKRHS)
         JLINE_VAL = ISHFT(IACC,-32)   	
         JLINSRTED(1,IS) = JLINE_VAL   	
         JLINSRTED(2,IS) = JPTR 		!... PTR
         IF(JLINE_VAL .LT. 7400) THEN
           NSCHED_CUT = NSCHED_CUT + 1
         ELSE IF(JLINE_VAL .LT. 8200) THEN
           NSCHED_TITL = NSCHED_TITL + 1
         ENDIF
       ENDDO
C
  177  CONTINUE
       WRITE(6,178)NTOTSCHED,NSCHED_CUT,NSCHED_TITL
  178  FORMAT(1H ,'REBLKFX4::MAKFFFD: EXITING WITH NTOTSCHED=',I4,
     1       /1H ,'        NSCHED_CUT=',I4,';  NSCHED_TITL=',I4)
       IF(NTOTSCHED .GT. 0) THEN
         WRITE(6,1781)
 1781    FORMAT(1H ,'  LINUM JOFSHD -- FROM JLINSRTED(1,J),(2,J):')
         WRITE(6,1782)(JLINSRTED(1,J),JLINSRTED(2,J),J=1,NTOTSCHED)
 1782    FORMAT((2I6))
         WRITE(6,1783)
 1783    FORMAT(1H ,' ..... ..... . . . END OF JLINSRTED . . .')
       ENDIF
     
       RETURN
       END
