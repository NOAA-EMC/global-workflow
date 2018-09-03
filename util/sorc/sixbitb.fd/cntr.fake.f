       SUBROUTINE CNTR(IRET_CNT, IMAGE, IMAGSIZ_WRDS, 
     1                 MAP, LABEL, ICHK, IFID, SCHED, INDEX, 
     1                 NFLDS,
     2                 FLD1, DASH1, OFSET1, SHAD1,
     3                 FLD2, DASH2, OFSET2, SHAD2,
     4                 FLD3, DASH3, OFSET3, SHAD3,
     5                 FLD4, DASH4, OFSET4, SHAD4)
C                                                  30-OCT-1996/LLIN
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    CNTR        EXECUTIVE FOR BEDIENT'S GRAPHICS PACKAGE 
C   PRGMMR: KRISHNA KUMAR       ORG: W/NP12       DATE:1999-09-08
C
C ABSTRACT: SUBR CNTR() IS THE EXECUTIVE FOR THE BEDIENT GRAPHICS
C   PACKAGE WHICH STANDS AT THE GATEWAY BETWEEN THE GRAPHICS 
C   APPLICATIONS PROGRAM (WHICH PREPARES GRIDPOINT DATA FIELDS AND 
C   THEIR DISPLAY SPECIFICATIONS, AND TEXT AND SYMBOL DATA INTO 
C   "LABEL-ARRAY" FORMAT); AND BEDIENT'S GRAPHIC PACKAGE WHICH INCLUDES 
C   THE FUNCTIONS WHICH FETCHES THE RASTER MAP-BACKGROUND AND 
C   INITIALIZES THE IMAGE WORKSPACE WITH THE MAP-BACKGROUND; CONTOURS 
C   THE GIVEN GRIDPOINT DATA FIELDS AND LAYS THE RESULTING CONTOURS INTO
C   THE IMAGE BIT-PLANE; ACCEPTS THE "LABEL-ARRAY" FORMATTED TEXT- AND 
C   SYMBOL-DATA AND PLOTS THOSE INTO THE IMAGE BIT-PLANE; ENCODES THE 
C   IMAGE IN NMC 6-BIT PACKED RLE RASTER CODE; FORMATS THE RESULTING 
C   PRODUCT WITH THE STRUCTURE: (A)THE GIVEN HEADER ("IFID"); (B) THE  
C   MAIN SECTION OF RASTER GRAPHICS; (C) THE RASTER-FORMATTED FAX STRIP-
C   TITLES; (D) THE FAX-CUT WINDOWING SPECIFICATIONS ("SCHED"); AND
C   (E) THE PRODUCT TERMINATOR.
C
C   THIS VERSION OF cntr ACCEPTS THE CALL SEQUENCE ARGUMENTS (WITH SOME
C   CHANGES); AND REFORMATS THOSE ARGUMENTS IN ORDER 
C   TO ADAPT TO CALLING OUR OWN UNDERLYING REPLACEMENT FUNCTIONS.
C
C PROGRAM HISTORY LOG:
C   ??-??-??  ORIGINAL AUTHOR - H. ART BEDIENT
C   ??-??-??  GLORIA DENT
C   ??-??-??  LUKE LIN
C   93-03-30  LUKE LIN: MODIFY THE 4-CHAR INTEGER TO ENHANCE CNTR
C   93-05-11  LILLY INSERTS TEMPLATE.
C   96-02-28  SHIMOMURA: CONVERT FROM IBM FORTRAN TO CRAY FORTRAN
C   96-03-04  SHIMOMURA: MAKING A NEW VERSION TO ADAPT THE OLD CALL
C                        SEQUENCE TO A REPLACED SET OF UNDERLYING
C                        FUNCTIONS.
C   96-06-18  SHIMOMURA: ADDING STRIP-TITLE AND ISCHED LOGIC
C   96-09-23  LUKE LIN: MODIFY THE SHADE OPTIONS.
C   96-10-30  LUKE LIN: INCREASE THE PRIORITY LOOPING FROM 3 TO 5 FOR WINDBARB.
C   97-02-03  LUKE LIN: LOAD IFID PROPERLY.
C 1999-07-01  KRISHNA KUMAR MODIFY TO RUN ON IBM RS/6000
C 1999-08-20  HENRICHSEN MODIFY TO USE USE DIRECT ACCESS I/O ON
C                        FAX FILE FOR IBM SP. USE LOCAL DIRECTORY FOR
C                        MAP BACKGROUND, HENCE EACH SCRIPT MUST COPY
C                        THE APPROPRIATE MAP BACKGROUND FILES FROM  
C                        PRODUCTION DIRECTORY 
C                        /nwprod/util/fix/graph_gphbg 
C                        OR IN CHECK OUT USER MAY USE OWN LOCAL 
C                        BACKGROUND DIRECTORY SUCH AS 
C                        /nfsuser/g02/wx12ph/util/fix/graph_gphbg
C USAGE:    CALL CNTR(IRET_CNT,MAP,LABEL,ICHK,IFID,SCHED,INDEX,NFLDS,
C    1                FLD1,DASH1,OFSET1,SHAD1,
C    2                FLD2,DASH2,OFSET2,SHAD2,
C    3                FLD3,DASH3,OFSET3,SHAD3,
C    4                FLD4,DASH4,OFSET4,SHAD4)
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C     (0.) IRET_CNT - INTEGER  RETURN_CODE
C
C     (1.) MAP -   INTEGER     MAP(15) IS A LIST CONTAINING:
C          MAP(1): BACKGROUND NAME IN 6 CHARACTERS, FOLLOWED BY
C                     TWO BYTES OF FILL -- NULL IS PREFERRED
C 
C          MAP(2): FLAGS IN TWO LOW-ORDER BYTES
C                  X'0001' = HASH THE BGND (TURN OFF EVERY OTHER PIXEL);
C                  X'0002' = DOUBLE THE SCALE OF THIS BGND;
C                  X'0004' = 2-PANEL ... TO PLACE A DUPLICATE OF A CUT 
C                              OUT OF THE GIVEN MAP-BACKGROUND BESIDE
C                              THE FIRST; 
C                  X'0008' = LEAVE PRODUCT OPEN AT END OF THIS CALL;
C                  X'0010' = ADD THIS PRODUCT ONTO AN OPEN PRODUCT FILE;
C                  X'0020' = CYCLIC PRODUCT; 
C                             (SOME SIDEWAYS MERCATOR PRODUCTS CAN
C                              EXTEND BEYOND LAST SCANLINE BY CYCLING TO
C                              CONTINUE WITH THE FIRST SCANLINE. IF THE
C                              DESIRED PRODUCT AND ITS MERCATOR GRID
C                              DOES NOT END AT THE SAME MERIDIAN AS THE
C                              MAP BGND, THEN THIS OPTION WILL PERMIT
C                              THE GEOGRAPHY TO EXTEND CYCLICALLY.)
C                  X'0040' = AFOS OUTPUT REQUIRED.
C
C          FOLLOWED BY THE MAP REGISTRATION CONSTANTS:
C          MAP(3), (4), (5),  (6):   (FOR THE ENTIRE PRODUCT),
C              I,   J, WIDTH,LENGTH
C          MAP(7), (8), (9),  (10):  (FOR THE MAP BACKGROUND),
C              I,   J, WIDTH,LENGTH
C          MAP(11),(12): FOR CONTOUR-POSITION FINE-ADJUSTMENT
C              DI,  DJ 
C          MAP(13),(14): FOR TEXT- & SYMBOL-PLOT POSITION FINE-ADJUST
C              DI,  DJ 
C          MAP(15) = T1 
C              T1 GRID-LENGTH IN FAX UNITS AS A REAL NUMBER
C
C             ALL ORIGINS ARE MEASURED FROM THE BACKGROUND MAP
C             IF ANY, AT A POINT 1800 UNITS TO THE LEFT OF THE FAX EDGE
C             IN SCAN LINE UNITS.
C             FINAL FAX START WILL BE AT RIGHT EDGE OF PRODUCT
C             CONTOUR-ADJUST IS NORMALLY MEASURED FROM THE LOWER
C             LEFT CORNER OF THE BACKGROUND. A LINE WITH HLL=P
C             WILL HAVE ZERO ADJUST.
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     (2.) LABEL - INTEGER   LABEL(2048)
C          LABEL IS AN ARRAY CONTAINING 2 WORDS PER ENTRY OF THE FORM:
C                   VFD  15/J, 1/FLAG, 3/PRIORITY, 13/I
C                   VFD  32/4HLITERAL
C
C                THE LITERAL STRING TERMINATES BY '$' IF LESS THAN 4;
C                THE LABEL-ARRAY IS TERMINATED BY A PAIR OF ZERO WORDS.
C                IF(J .GE. 7400) THEN
C                   THIS LABEL-ITEM IS PUT IN THE CHART-LABEL RECORD.

C                IF(IOR(LABEL(1),LABEL(2)) .EQ. 0) THEN
C                   THERE IS NO LABEL-ARRAY TEXT-DATA TO PROCESS;

C                IF(LABEL(1) .EQ. -1) THEN
C                   LABEL-ARRAY DATA IS OUT ON FILE 55;

C                ELSE 
C                   LABEL-ARRAY IS FOUND IN LABELLED COMMON 
C                      AND DOES NOT EXCEED 1024 ITEMS.
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     (3.) ICHK -  INTEGER   ICHK(3)
C          ICHK(1) IS FOR THE RETURN CODE 
C                    (SEE UNDER OUTPUT ARGUMENT LIST) 
C
C          ICHK(2) = WORK-SPACE ALLOCATED IN LABELLED COMMON /ISPACE/
C                 THE LENGTH MUST BE AT LEAST 6*2048 IF MERGE IS USED
C                 THE LENGTH MUST BE AT LEAST 
C                           5219 + (8+NFLDS*5)*T + NFLDS*2*Q*T
C                           WHERE Q=INTF(T1/4.) + 1
C                                 T=(2*P)+1-ODD)

C          ICHK(3) = 0; FOR A NORMAL MAP
C                  = 1; FOR THE LAST MAP IN THE PRODUCTS FILE; SO CLOSE
C                         THE OUTPUT FILE WITH AN END-OF-ALL-MAPS FLAG.

C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     (4.) IFID -   INTEGER*8   IFID(6)  ... = 48-bytes
C          IFID IS THE FRONT ID FOR THIS OUTPUT MAP PRODUCT
C          IT CONTAINS 48 BYTES. THE FIRST THREE ARE REPLACED BY A
C          FLAG. THE BYTE(4) = 'F' --  FOR FAX TRANMISSION;
C                            = 'V' --  FOR LOCAL "VARIAN" PLOTTER ONLY;
C                            = 'B' --  FOR BOTH OF THE ABOVE;
C          THE REMAINDER IS DATE TIME AND PRODUCT TITLE INFORMATION
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     (5.) SCHED - INTEGER     SCHED(8,60)
C          SCHED CONTROLS THE SUBSEQUENT FAX WINDOWING AND COMPOSITING
C             BY OSO AFTER WE GENERATE THE PRODUCT AND SEND IT TO OSO.
C          THE LIMITING NO. OF SCHEDULE ENTRIES PERMITTED = 59 ENTRIES
C          SCHED IS AN ARRAY CONTAINING 8 I*2 WORDS PER ENTRY 
C                DEFINING FAX CUTS
C            II1,II2,II3,II4, L5A,L5B,L6A,L6B, II7,II8
C            I2  I2  2X  I2   I1  I1  I1  I1   I2  I2

C          WHERE  
C            II1 IS MAP SUBSET/INSET NUMBER (IN BINARY);

C            II2 IS STARTING SCAN-LINE NUMBER (IN BINARY) 
C                 (WHICH IS THE Y-ORIGIN OF THIS CUT);
C            II3 IS RESERVED
C                 (II2 VALUE IS CONVERTED, BY CNTR, 
C                        INTO RECORD NO. IN WORD II2
C                           AND BYTE NO. IN WORD II3 )

C            II4 IS THE COUNT OF SCANLINES IN THIS CUT
C                 (WHICH IS THE Y-DIMENSION OF THIS CUT);

C            L5A CONTAINS 8 FLAG BITS
C            L5B IS INDENT-VALUE(IN BYTES)
C                 (WHICH IS THE X-ORIGIN OF THIS CUT);
C            L6A IS COMPONENT WIDTH(IN BYTES)
C                 (WHICH IS THE X-DIMENSION OF THIS CUT);

C            ...SPECS FOR THE INSET TO FOLLOW THIS COMPONENT 
C            ...  WITHIN THE COMPOSITE, (WHICH COMPONENT IS NOT 
C            ...  NECESSARILY THE INSET DEFINITION WHICH COMES NEXT 
C            ...  IN THIS ARRAY).
C            L6B IS INDENT-VALUE(IN BYTES) TO X-POSITION 
C                 (WITHIN THE COMPOSITE) THE FOLLOWING INSET

C            II7 IS SCANLINE TO Y-POSITION (WITHIN THE COMPOSITE)
C                 THE FOLLOWING INSET
C            II8 IS INSET NUMBER OF THE FOLLOWING INSET TO BE 
C                 COMPOSITED 
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     (6.) INDEX - INTEGER   INDEX(2,3)
C          INDEX SPECIFIES THE DIMENSIONS OF THE GRID TO BE CONTOURED
C          THE FORM OF INDEX IS:
C             E0= GRID COUNT
C             E1= P = MAX VALUE OF HALF LINE LENGTH
C                 P IS THE SYMMETRY GRID LINE
C             E2=0 IF ODD 
C               =1 IF EVEN;
C             E3=RECTANGULAR FLAG
C             E4,E5= THE LINE ORIGIN,THE HALF LINE LENGTH
C                (ONLY ONE E4,E5 ENTRY IS REQUIRED IF RECTANGULAR)
C             ETC

C             IF(E3.EQ.0 .OR. E3.EQ.1) THERE MUST BE A COMPLETE TABLE
C             IF E3 IS 1 THE POLYGON IS DRAWN TO THE BNDRY
C             IF E3 IS NEG THE RECT WILL BE DRAWN THE BNDRY
C             IF ODD IS 0 ONE EXTRA ROW IS DONE
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     (7.) NFLDS - INTEGER   NFLDS
C          NFLDS IS THE NUMBER OF GRID FIELDS TO CONTOUR
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C          FOUR WORD ENTRY PER GRIDPOINT DATA SET TO BE CONTOURED ...
C          FLD, DASH, OFFSET, SHADE
C             FLD IS THE LOC OF THE GRID ARRAY
C             DASH IS THE DASH MASK /DASH WEIGHT IN HALF WORDS
C             OFFSET IS WEIGHT/ OFFSET IN HALF WORDS
C                  WEIGHT IS 0 OR 2 FOR LINE WEIGHT
C                  IF WEIGHT IS NEGATIVE,NO NEGATIVE VALUED CONTOURS
C                  OFFSET MOVES THIS CONTOUR - SCAN UNITS TO RIGHT
C                       IF THE OFFSET IS LESS THAN 100 AND
C                       GREATER THAN ZERO, IT IS CONSIDERED TO BE THE
C                       MAXIMUM CONTOUR TO BE ALLOWED
C             SHADE IS THE SHADE MASK /AND THE SHADE MATCH IN HALF WDS
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     (8.)  FLD1  - REAL      FLD1(*)  --- 1ST GRIDPOINT FIELD
C     (9.)  DASH1  - INTEGER   DASH1(2)  ---     DASHING SPECS
C     (10.) OFSET1 - INTEGER   OFSET1(2) ---     LINE WEIGHT/ADJ X-POSIT
C     (11.) SHAD1  - INTEGER   SHAD1(20)  ---     SHADING SPECS
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     (8.)  FLD2  - REAL      FLD2(*)  --- 2ND GRIDPOINT FIELD
C     (9.)  DASH2  - INTEGER   DASH2(2)  ---     DASHING SPECS
C     (20.) OFSET2 - INTEGER   OFSET2(2) ---     LINE WEIGHT/ADJ X-POSIT
C     (22.) SHAD2  - INTEGER   SHAD2(20)  ---     SHADING SPECS
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     (8.)  FLD3  - REAL      FLD3(*)  --- 3RD GRIDPOINT FIELD
C     (9.)  DASH3  - INTEGER   DASH3(2)  ---     DASHING SPECS
C     (30.) OFSET3 - INTEGER   OFSET3(2) ---     LINE WEIGHT/ADJ X-POSIT
C     (33.) SHAD3  - INTEGER   SHAD3(20)  ---     SHADING SPECS
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     (8.)  FLD4  - REAL      FLD4(*)  --- 4TH GRIDPOINT FIELD
C     (9.)  DASH4  - INTEGER   DASH4(2)  ---     DASHING SPECS
C     (40.) OFSET4 - INTEGER   OFSET4(2) ---     LINE WEIGHT/ADJ X-POSIT
C     (44.) SHAD4  - INTEGER   SHAD4(20)  ---     SHADING SPECS
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C   OUTPUT ARGUMENT LIST:
C     (3.) ICHK -  INTEGER   ICHK(3)
C          ICHK(1) IS FOR THE RETURN CODE: 
C                  = -1 NORMAL
C                  =  0 OUTPUT TROUBLE
C                  =  1 BACKGROUND TROUBLE
C
C                 (BUT THIS RETURN CODE IS QUEER IN THAT IN ORDER TO
C                  ACTIVATE THIS RETURN CODE FUNCTION, THE USER MUST
C                  SET THIS: 
C                      ICHK(1) = A NON-ZERO VALUE;) 
C
C          ICHK(2) = SPACE ALLOCATED (SEE INPUT ARGUMENT LIST)
C
C          ICHK(3) = END-PRODUCT SPEC (SEE INPUT ARGUMENT LIST)
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C   INPUT FILES:
C     FT02F001 -  LIBRARY OF MAP-BACKGROUNDS IN RASTER-FORMAT
C                    DIRECT-ACCESS FILE FORMAT
C  
C     FT55F001 -  "LABEL-ARRAY' FORMAT, EACH RECORD CONTAINS 1024 LABEL-
C                    ARRAY ITEMS OR 8192 BYTES PER RECORD.
C   WORK FILES:
C     FT60F001 -  TEMPORARY WORK FILES FOR SORT/MERGING FT55F001 DATA
C     FT61F001 -     "
C     FT62F001 -     "
C     FT63F001 -     "
C
C   OUTPUT FILES: 
C     FT0XF001 - NMC 6-BIT PACKED RLE RASTER FORMATTED MAPS DESTINED
C                  FOR FAX OR VARIAN.
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C REMARKS:
C     SUBROUTINES CALLED by old CNTR:
C        ISORT(), MERGE(),
C        CONST(),
C        INPUT(), OUTP(),
C        CNTRI()
C        ENDMAP()
C     . . . . . . . . . . . 
C     SUBROUTINES CALLED by new CNTR:
C        ISORT2D(), MERGES(),
C        CONSTA(), GETBGND, PRTITLE
C     . . . . . . . . . . . 
C          DATA SET CNTR       AT LEVEL 004 AS OF 05/11/93
C     ...NO ASYNC I/O CLEANED ON MAY 27, 1986 (DKM)...
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM
C
C$$$
C
C   *******************************************************
C
C
C     THIS SUBROUTINE CONVERTS THE CALL SEQUENCE CONSTANTS
C     INTERNAL CONSTANTS AND INITIALIZES THE FILES,THEN CNTRI()
C     IS CALLED TO ALLOW FORTRAN TO COMPUTE THE ADDRESS CONSTANTS
C     SORT AND MERGE THE LABEL LIST IF REQUIRED
C  *** N O T E :  TO MAKE A EXE LOAD MODULE CNTR MUST COMPILE WITH
C  *** N O T E :  CNTR7AS FROM 'NMC.PROD.V77GRAPH.SOURCE'



       INTEGER    LMAX
       PARAMETER (LMAX=1024)
       
       INTEGER    LMAX2
       PARAMETER (LMAX2 = 2*LMAX)   	!... = 2048

       INTEGER    LUNBGD
       PARAMETER (LUNBGD=11)  		!... INPUT UNIT FOR MAP-BGND

       INTEGER    LUNRAS
       PARAMETER (LUNRAS=71)

       INTEGER    LUNRASST
       PARAMETER (LUNRASST=72) 	!... CHECKOUT PURE-RASTER STRIPTITL UNIT

       INTEGER    LUNEXT6B
       PARAMETER (LUNEXT6B=80)

       INTEGER    LUNIPK6
ckumar       PARAMETER (LUNIPK6=81)

C   *******************************************************
C ...    CALL CNTR(IRET_CNT, IMAGE, IMAGSIZ_WRDS,
C ...  X MAP,LABEL,ICHK,IFID,SCHED,INDEX,NFLDS,
C ...  X FLD1,DASH1,OFSET1,SHAD1,
C ...  X FLD2,DASH2,OFSET2,SHAD2,
C ...  X FLD3,DASH3,OFSET3,SHAD3,
C ...  X FLD4,DASH4,OFSET4,SHAD4)
C   *******************************************************
C
      INTEGER     IRET_CNT
      INTEGER     IMAGE(IMAGSIZ_WRDS)
      INTEGER     MAP(15)
      INTEGER     LABEL(LMAX2)
      INTEGER     ICHK(3)
      INTEGER     IFID(6)      		!... I*8 ifid(6) == 48bytes
      INTEGER     SCHED(8,60)   	!... i*2  in low-order of i*8
      INTEGER     INDEX(2,3)
      INTEGER     NFLDS
      REAL        FLD1(*)
      INTEGER     DASH1(2), OFSET1(2), SHAD1(20)
      REAL        FLD2(*)
      INTEGER     DASH2(2), OFSET2(2), SHAD2(20)
      REAL        FLD3(*)
      INTEGER     DASH3(2), OFSET3(2), SHAD3(20)
      REAL        FLD4(*)
      INTEGER     DASH4(2), OFSET4(2), SHAD4(20)

C   *******************************************************

C
      COMMON /DCESS/ IV,IS
      COMMON /IFS/   IAFOS(2),NNAFST


      COMMON/IOUTFAX/IFAXOUT,NRECFAX
      
      INTEGER    IFAXOUT
      INTEGER    NRECFAX
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      COMMON /ISPACE/IBUFF(LMAX2),IFIRST,L1728,JFLAG(2),JTABLE(3200),
     1               IVTABL(1200),IVSCAN(30),
     X               TWO,CIN(6),MPIN(480),
     X               MPWK(131,10),CADJ(14),CNS(9),ITBL(2,5),ID(4,4),
     X               PHASE

      INTEGER      TWO
      INTEGER      CIN
      INTEGER      CADJ

      INTEGER      BGND(3,80)
      EQUIVALENCE (MPIN(1),BGND(1,1))
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

      COMMON /OBUFF/MOUT(7),IOUT(360),NOUT(360),ISCHED(2,80),ISTOP
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      COMMON  /ALT_LBL/ LBL_INCOREQ,LBL_EMPTYQ,LABEL_PKD(LMAX)
      LOGICAL   LBL_INCOREQ
      LOGICAL   LBL_EMPTYQ
      INTEGER   LABEL_PKD
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C      ...   FOR PLOTTED STRIP-TITLES IN IMAGE_STR(I,J)  ...
       INTEGER      MAXIWORD_STR
       PARAMETER   (MAXIWORD_STR=27)  		!... 27 I*8 = 1728 PELS

       INTEGER      MAXJSLINE_STR
       PARAMETER   (MAXJSLINE_STR=800)  	!... LIMIT STRIP TITLES

       COMMON   /STITLES/ IMAGE_STR
       INTEGER           IMAGE_STR(MAXIWORD_STR,MAXJSLINE_STR)

       COMMON   /STITLPLT/NRECSTART_STR, NITMPLTED_STR, 
     1                    MXJVAL_STR, MXJLABITM_STR
       INTEGER           NRECSTART_STR          !... PTR IN LABEL FILE
       INTEGER           NITMPLTED_STR  	!... COUNT STRITM PLTED
       INTEGER           MXJVAL_STR		!... MAXJ STR PLTED
       INTEGER           MXJLABITM_STR   	!... AND THAT LABL ITEM

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . .   Statisitcs about LABEL array via LOOK_LAB()   . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
       integer      lmtmxmn
       parameter   (lmtmxmn=8)

       INTEGER      LMTPRIOR
       PARAMETER   (LMTPRIOR=8)

       COMMON      /STAT_LAB/NITEM_TOT,NITEM_TITLE,NPRIOR_LAB,
     1                       MAXIJ_LAB,MINIJ_LAB

       INTEGER      NITEM_TOT     !... TOTAL COUNT OF LABEL-ARRAY ITEMS  
       INTEGER      NITEM_TITLE
       INTEGER      NPRIOR_LAB(LMTPRIOR,2)
       integer      MAXIJ_LAB(LMTMXMN,2)
       integer      MINIJ_LAB(LMTMXMN,2)
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . used by bufow512() and bf512_wr()  . . . . .

       COMMON /ARBFOX6B/ LUNX6B,LUX6BOPNQ,NBUFX6B,IPTR_X6BF
       INTEGER       LUNX6B
       LOGICAL       LUX6BOPNQ
       INTEGER       NBUFX6B
       INTEGER       IPTR_X6BF
C      . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... INTEGER       IWINDOW(30)
C      ... IWINDOW(1) = (J_BG_SKP)              !... NSKIP BG AT START
C      ... IWINDOW(2) = (J_BG_ORG) 		!... J0 BG IN IMAGE WORK 
C      ... IWINDOW(4) = (J_FR_MAX)  = 1876      !... ENTIRE IMAGE WORK-J
C      ... IWINDOW(5) = (IPXL_BG_SKP)        !... DISCARD PXL @ LN START
C      ... IWINDOW(6) = IOPTNBITS AS GIVEN IN MAP(2)
C      ... IWINDOW(7) = (IPXL_BG_ORG)         	!... I0 BG IN IMAGE WORK
C      ... IWINDOW(15) = (IWRD_FR_MAX) = 27     !... ENTIRE IMAGE WORK-I
C      ... IWINDOW(16) = (IPXL_FR_MAX) = 1728   !... MAX PXL IN RESULT
C      ... IWINDOW(17) = MAPBGNAME = nh2500x
C      ... IWINDOW(18) = (J_BG_SPA)          !... SPACE FOR BG W/I FRAME
C      ... IWINDOW(19) = (IPXL_BG_SPA)       !... SPACE FOR BG W/I FRAME
C      ... IWINDOW(20) = (IPXL_BG_CUT)       !... PXLS TO USE FROM BG LN
C      ... IWINDOW(21) = 0 FLAG FOR MAIN MAP; OR =1 FOR TITLE WINDOW
       
       INTEGER    J_FR_MAX
       PARAMETER (J_FR_MAX=4)
       INTEGER    IWRD_FR_MAX
       PARAMETER (IWRD_FR_MAX=15)
       INTEGER    IPXL_FR_MAX
       PARAMETER (IPXL_FR_MAX=16)

       INTEGER    J_BG_SPA
       PARAMETER (J_BG_SPA=18)
       INTEGER    IPXL_BG_SPA
       PARAMETER (IPXL_BG_SPA=19)

       INTEGER    J_BG_SKP
       PARAMETER (J_BG_SKP=1)
       INTEGER    IPXL_BG_SKP
       PARAMETER (IPXL_BG_SKP=5)
       INTEGER    IPXL_BG_CUT
       PARAMETER (IPXL_BG_CUT=20)

       INTEGER    J_BG_ORG
       PARAMETER (J_BG_ORG=2)
       INTEGER    IPXL_BG_ORG
       PARAMETER (IPXL_BG_ORG=7)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       
C

      INTEGER      DFL
      DATA         DFL       / X'FFFFFF00' /

      INTEGER      EFL   		!... END-OF-ALL MAPS FLAG
      DATA         EFL       / X'FFFFFC00' /
C
      INTEGER      ISTP
      DATA         ISTP      / X'C5D5C440' /   	!... "END " IN BGND DIR
C  
      INTEGER      MSK
ckumar      DATA         MSK       /       X'FF' /
      DATA         MSK       / X'00000000000000FF' /

      INTEGER      MSK1
      DATA         MSK1      / X'FFFF0000' /
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      INTEGER      IRET_MRG
      INTEGER      JTM
      INTEGER      NLABITMSOR
      INTEGER      IRET_SR2

      INTEGER      IL_STR(15)
      INTEGER      IWINDOW_STR(30) 	!... map const: STRIP TITLES
      INTEGER      IWINDOW(30)

      INTEGER      maxiword
      INTEGER      maxjsline

      LOGICAL      LEBCDIC
      LOGICAL      LCHKSORTQQ
      LOGICAL      LANYTITLESQQ

      integer      lncount_str
      INTEGER      IMGTYP
      INTEGER      IMAGSIZ_STR
      INTEGER      NPXLOUT_STR
      INTEGER      NROWSHOW_STR

C     ... OPTN BITS TO MATCH THOSE GIVEN IN MAP(2) ...
      INTEGER    K0008X
      DATA       K0008X   / X'0008' /  	!... LEAVE OUTP OPEN AT EXIT
      INTEGER    K0010X    		
      DATA       K0010X   / X'0010' /  	!... ADDING ONTO LEFT-OPEN OUTP

      INTEGER      MYOPTNBITS

      INTEGER      I8IFID(6)
      CHARACTER*1  C1IFID(48)
      EQUIVALENCE (I8IFID(1),C1IFID(1))
C
      CHARACTER*40 CPATHBGND

      CHARACTER*1  NULL

      INTEGER      ITAPE

      SAVE
C
C     . . . . . .   S T A R T   . . . . . . . . . . . . . . . . . . . .
C
      IRET_CNT = 0
      NULL = CHAR(0)
      LUNX6B = LUNEXT6B    	!... DSRN INTO COMMON /ARBFOX6B/
      WRITE(6,FMT='(1H ,''CNTR:ENTERED SUBR CNTR '',
     1                  ''VERSION 3.2  DATED 20-AUG-1999'',
     2             /1H ,7X,''WITH OPTNS IN MAP(2)=HEX'',Z17.16)')
     A        MAP(2)


      MYOPTNBITS = 0

      MYOPTNBITS = IBSET(MYOPTNBITS,7)	!... GIVEN IFID IS IN ASCII

      IF(IAND(MAP(2),K0010X) .NE. 0) THEN
C       ... IF ADDING-ON TO PREVIOUSLY LEFT-OPEN PRODUCT FILE,
        MYOPTNBITS = IBSET(MYOPTNBITS,0) 	!... MULTI-PANEL
        MYOPTNBITS = IBSET(MYOPTNBITS,1)	!... ADDING ON TO EXISTG
      ENDIF
      IF(IAND(MAP(2),K0008X) .NE. 0) THEN
C       ... IF LEAVING THE OUTPUT FILE OPEN AT EXIT,
        MYOPTNBITS = IBSET(MYOPTNBITS,0) 	!... MULTI-PANEL
        MYOPTNBITS = IBSET(MYOPTNBITS,2)	!... LEAVE OPEN AT END
      ENDIF


      LCKPT = 101
      PRINT  888, LCKPT
  888 FORMAT(1H , 'CNTR:ARRIVED AT CHECKPOINT = ', I4)
C
      IF(ICHK(1) .NE. 0) THEN
        ICHK(1) = -1    	!... INITIALIZE RET CODE TO NORMAL
      ENDIF

C     =================================================================
C     . . .   S T E P (1.)   SORT THE LABEL-ARRAY DATA 
C                                WHETHER IN-CORE OR OUT ON FILE FT55
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


      LBL_INCOREQ = .TRUE.
      LBL_EMPTYQ  = .FALSE.

      DO  J = 1,LMAX
        LABEL_PKD(J) = 0
      ENDDO

      IF(IOR(LABEL(1),LABEL(2)) .EQ. 0) THEN
C       ... EMPTY LABEL-ARRAY, SO SKIP TEXT/SYMBOL PROCESSING,
        LBL_EMPTYQ = .TRUE.
        GO TO 12
      ELSE IF(LABEL(1) .NE. -1) THEN
C       ... USE IN-CORE LABEL-ARRAY ...
        LBL_INCOREQ = .TRUE.
        GO TO 10
      ENDIF
C     ... OTHERWISE, LABEL(1) = -1; SO LABEL-DATA IS OUT THERE ON FT55,
C
C     ***          ***                   ***
C     ... THE LABEL LIST IS ON A SEQUENTIAL FILE ...
        LBL_INCOREQ = .FALSE.

C       ... TO SORT/MERGE THE EXTERNAL FILE-55 LABEL-ARRAY FILE,

        CALL MERGES(ITAPE,IRET_MRG)

        IF(IRET_MRG .NE. 0) THEN
C         ... IN OLD VERSION, THAT USED TO STOP WITHIN MERGE
          WRITE(6,FMT='(1H ,''CNTR::MERGES: SERIOUS ERROR ... '',
     1                      ''RETURNED WITH CODE='', I5)')
     A            IRET_MRG

          LBL_EMPTYQ = .TRUE.
          REWIND ITAPE
          GO TO 12
        ELSE
          WRITE(6,FMT='(1H ,''CNTR: normal return from merges '',
     1                      ''with sorted LABEL-data on ITAPE='',I4)')
     A            ITAPE
        ENDIF
      GO TO 12

 10   CONTINUE
C     ... TO SORT THE IN-CORE LABEL-ARRAY,
C     ...    (BUT THE IN-CORE LABEL ARRAY ON CRAY IS UNCOMPRESSED)
    
      JTM = LMAX

C     ... TO SORT, IN PLACE, THE 2-D LABEL ARRAY ...

      CALL ISORT2D(LABEL,JTM,NLABITMSOR,IRET_SR2)
      
      IF(IRET_SR2 .EQ. -1) THEN
C       ... THIS SHOULD BE THE USUAL CASE OF ZERO-TERMINATOR FOUND
C       ...    IN THIS IN-CORE LABEL ARRAY,
        WRITE(6,FMT='(1H ,''CNTR::ISORT2D: SORTED THE IN-CORE LABEL '',
     1                    ''ARRAY AND FOUND THE ZERO-TERMINATOR '',
     2               /1H ,''     AFTER NITEMS_SORTED='',I6,
     3                    '';  ISORT2D RETURN-CODE='',I4)')
     A          NLABITMSOR,IRET_SR2

      ELSE IF(IRET_SR2 .EQ. 0) THEN
C       ... UNEXPECTED RETURN TO HAVE SORTED AN IN-CORE LABEL ARRAY
C       ...      WITHOUT FINDING A ZERO-TERMINATOR.
        WRITE(6,FMT='(1H ,''CNTR::ISORT2D: WARNING! IN-CORE LABEL-'',
     1                    ''ARRAY DOES NOT HAVE A ZERO-TERMINATOR.''
     3               /1H ,''      NITEMS_SORTED='',I6,
     4                    ''; ISORT2D RETURN-CODE='',I4)')
     A           NLABITMSOR,IRET_SR2

      ELSE IF(IRET_SR2 .EQ. 1) THEN
C       ... SERIOUS ERROR IN SORT2D ...
        WRITE(6,FMT='(1H ,''CNTR::ISORT2D: FAILED TO SORT IN-CORE '',
     1                    ''LABEL ARRAY. BAD JTM SIZE='',I6,
     2               /1H ,''  NITEMS_SORTED='',I6,
     3                    ''; RETURN CODE='',I4)')
     A          JTM,NLABITMSOR,IRET_SR2

        LBL_EMPTYQ = .TRUE.

      ELSE IF(IRET_SR2 .EQ. 5) THEN
C       ... EMPTY LABEL ARRAY ...
        WRITE(6,FMT='(1H ,''CNTR::ISORT2D: GIVEN EMPTY IN-CORE '',
     1                    ''LABEL ARRAY.  RETURN CODE='',I4)')
     A          IRET_SR2

        LBL_EMPTYQ = .TRUE.

      ELSE

        WRITE(6,FMT='(1H ,''CNTR::ISORT2D:ERROR.   UNKNOWN RETURN-'',
     1                    ''CODE='',I4,
     2               /1H ,''     NITEMS_SORTED='',I6)')
     A          IRET_SR2,NLABITMSOR

        LBL_EMPTYQ = .TRUE.
      ENDIF

C     ... FOR THE CASE OF THE IN-CORE LABEL ARRAY,  COMPRESS THE 
C     ...   SORTED LABEL ARRAY

      IF(LBL_EMPTYQ) THEN
        GO TO 12
      ENDIF

      IF(NLABITMSOR .LE. 0) THEN
        LBL_EMPTYQ = .TRUE.
        GO TO 12
      ENDIF

C     ... OTHERWISE, HALF-PACK LABEL ARRAY WITH DESTINATION= LABEL_PKD
      CALL HAFPAKRA(LABEL,LMAX2,LABEL_PKD,LMAX,NWD_PKD,IRET_HAF)

      IF(IRET_HAF .NE. 0) THEN
        WRITE(6,FMT='(1H ,''CNTR::HAFPAKRA: FAILED WITH ERROR-CODE='',
     1                      I3,
     1               /1H ,''     BAD ARRAY SIZE.  SRCSIZ='',I6,
     2                    '' DESTSIZ='',I6)')
     A          LMAX2,LMAX
        IRET_CNT = -1
        GO TO 999    		!... ERROR RETURN
      ENDIF

      IF(NLABITMSOR .GE. LMAX) THEN
C       ... TO FORCE A ZERO-TERMINATOR INTO AN EXACTLY FULL LABEL ARRAY
        LABEL_PKD(LMAX) = 0
        NLABITMSOR = LMAX - 1
      ENDIF
C     ... NOW NLABITMSOR IS WITHIN RANGE, ERASE REMAINDER OF PACKED ARRA
      M1 = NLABITMSOR + 1
      DO  J = M1,LMAX
        LABEL_PKD(J) = 0
      ENDDO
         
      GO TO 12
 12   continue
C     ... TO GATHER STATISTICS ABOUT THE LABEL-ARRAY ITEMS ...
       NITEM_TOT = 0
       NITEM_TITLE = 0
       LANYTITLESQQ = .FALSE.
       do  j = 1,2
         do  i = 1,LMTPRIOR
           NPRIOR_LAB(I,J) = 0
         ENDDO
       ENDDO
       do  j = 1,2
         do  i = 1,LMTMXMN
           MAXIJ_LAB(I,J) = 0
         ENDDO
       ENDDO
       do  j = 1,2
         do  i = 1,LMTMXMN
           MINIJ_LAB(I,J) = 0
         ENDDO
       ENDDO

      if(.NOT. LBL_EMPTYQ) THEN
        LCHKSORTQQ = .TRUE.

        call look_lab(ITAPE, LCHKSORTQQ, IRET_look)
C         ... which works for case of LBL_INCOREQ also,
C         ... by querying COMMON /ALT_LBL/ ...
 
        WRITE(6,FMT='(1H ,''CNTR::look_lab: RETURNED WITH TOTAL '',
     1                    '' SORTED ITEM COUNT='',I8,
     2               /1h ,7X,''look_lab() RETURN-CODE ='',I4)')
     A          NITEM_TOT,IRET_look

        IF(IRET_LOOK .NE. 0) THEN
          WRITE(6,FMT='(1H ,''CNTR:... WARNING ...:LOOK_LAB FAILED '',
     1                      ''WITH RETURN CODE='',I3)')
     A            IRET_LOOK
          GO TO 122   	!... TRY TO LET IT CONTINUE ANYWAY
        ELSE
C         ... OTHERWISE, NORMAL return from  LOOK_LAB ...
          IF(NITEM_TITLE .LE. 0) THEN
            LANYTITLESQQ = .FALSE.
          ELSE
            LANYTITLESQQ = .TRUE.
          ENDIF
        ENDIF
      ENDIF
      GO TO 122
C     =================================================================
C     . . .   S T E P (2.)   COMPUTE SCALE AND ADJUSTMENT CONSTANTS ...
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

 122  CONTINUE
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . .   CONSTANTS FOR THE STRIP-TITLE WINDOW  . . . . . . 
       IL_STR(1) = 0  		!... NO MAP BGND FOR STRIP-TITLE REGION
       IL_STR(2) = 0

       IL_STR(3) = 72   	!... 1800-(72)=1728pels =27 longwords
       IL_STR(4) = 0
       IL_STR(5) = 1728
       IL_STR(6) = MAXJSLINE_STR 		!... = 800 scanline max
 
       IL_STR(7) = 72
       IL_STR(8) = 0
       IL_STR(9) = 1728
       IL_STR(10) = MAXJSLINE_STR

       IL_STR(11) = 0
       IL_STR(12) = 0
       IL_STR(13) = 0
       IL_STR(14) = -7399		!... delta-j to position titles
       IL_STR(15) = 0

       call consta(IL_STR,IWINDOW_STR)

       IWINDOW_STR(21) = 1   		!... flags this as strip-titles
       
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

      CALL CONSTA(MAP,IWINDOW)

      maxiword = iwindow(15)
      maxjsline = iwindow(J_FR_MAX)

      IF((MAXIWORD .LE. 0) .OR. (MAXJSLINE .LE. 0)) THEN
        WRITE(6,FMT='(1H ,''CNTR::CONSTA: DIMENSIONS OF IMAGE= ('',
     1              I4,'','',I6,'') ... WHICH ARE WRONG ! ! !'',
     2             /1H ,'' GO CHECK THE MAP REGISTRATION CONSTANTS!'',
     3             /1H ,'' * * * *   E R R O R   S T O P   * * * *'')')
     A        MAXIWORD,MAXJSLINE
        IRET_CNT = -2
        GO TO 999
      ENDIF

      IMGSIZNEED = MAXIWORD * MAXJSLINE
C     ... ALLOCATED IS IMAGSIZ_WRDS
      WRITE(6,FMT='(1H ,''CNTR::CONSTA: DIMENSIONS OF IMAGE= ('',
     1           I4,'','',I6,'')'',
     2      /1H ,''         WHICH IS'',I8,'' WORDS OF SPACE REQUIRED'', 
     3           '' FOR THIS PRODUCT'',
     4      /1H ,''  ALLOCATED SIZE='',I8,'' WORDS'')')
     A        MAXIWORD,MAXJSLINE,IMGSIZNEED,IMAGSIZ_WRDS
C      
C
      IF(IMGSIZNEED .GT. IMAGSIZ_WRDS) THEN
        WRITE(6,125)
  125   FORMAT(1H ,'CNTR:  ALLOCATED SPACE FOR IMAGE IS INADEQUATE',
     1        /1H ,' * * * * *   E R R O R   S T O P   * * * * * *')
        IRET_CNT = -3
        GO TO 999
      ENDIF
C
       DO  I = 1,IMGSIZNEED
         IMAGE(I) = 0
       ENDDO

C      ...   THE FOLLOWING STRIP-TITLE INITIALIZATION NEEDS TO HAVE
C      ...     SOME IF TESTS FOR FIRST PANEL OF MULTI-CALL-CNTR PRODUCT
       DO  J = 1,MAXJSLINE_STR
         DO  I = 1,MAXIWORD_STR
           IMAGE_STR(I,J) = 0
         ENDDO
       ENDDO

       NRECSTART_STR = 0          !... PTR IN LABEL FILE
       NITMPLTED_STR = 0
       MXJVAL_STR = 0
       MXJLABITM_STR = 0
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C
C     =================================================================
C     . . .   S T E P (3.) ... FETCH MAP-BACKGROUND IF REQUESTED ...
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     ***          ***                   ***
C     ... FROM OTHER MAP() CONSTANTS, DETERMINE THE DESIRED PRODUCT
C     ...   SIZE, AND INITIALIZE THE IMAGE BITPLANE


      IF(MAP(1) .EQ. 0) THEN
C       ... NO MAP-BACKGROUND HAS BEEN REQUESTED ...
        GO TO 13

      ENDIF
C

C     ... GET MAP BGND NAME FROM MAP(1); get bgnd; lay it into image

       
      cpathbgnd(1:40) = ' '
c
ckumar Path of map background is set to current local directory
c      on IBM SP 
      CPATHBGND(1:) = './' //NULL
      print*,'In CNTR - map background dir', CPATHBGND(1:)
      call getbgnd(LUNBGD,CPATHBGND,IWINDOW,IMAGE,IRET_BG)


      GO TO 13

C     =================================================================
C     . . .   S T E P (4.) ... MOVE CONTOUR-FIELD POINTERS 
C                        AND CONTOUR DISPLAY SPECS FROM CALL SEQUENCE
C                        INTO ID(4,4)  (WHICH IS IN /ISPACE/)
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

 13   CONTINUE
C
C     ***          ***                   ***
      IF(NFLDS .LE. 0) GO TO 15
C       ... OTHERWISE, WE HAVE SOME FLDS ...
C     ... DO FLD1 ...
        I=1
        ID(1,I) = LOC(FLD1)
        ID(2,I) = DASH1(1)
        ID(3,I) = OFSET1(1)
        ID(4,I) = SHAD1(1)
        IF(NFLDS .EQ. 1) THEN
          GO TO 15
        ENDIF
C     ... OTHERWISE, DO FLD2 ...
        I=2
        ID(1,I)=LOC(FLD2)
        ID(2,I) = DASH2(1)
        ID(3,I) = OFSET2(1)
        ID(4,I) = SHAD2(1)
        IF(NFLDS .EQ. 2) THEN
          GO TO 15
        ENDIF
C       ... OTHERWISE, DO FLD3 ...
        I=3
        ID(1,I)=LOC(FLD3)
        ID(2,I) = DASH3(1)
        ID(3,I) = OFSET3(1)
        ID(4,I) = SHAD3(1)
        IF(NFLDS .EQ. 3) THEN
          GO TO 15
        ENDIF
C       ... OTHERWISE, DO FLD4 ...
        I=4
        ID(1,I)=LOC(FLD4)
        ID(2,I) = DASH4(1)
        ID(3,I) = OFSET4(1)
        ID(4,I) = SHAD4(1)
        IF(NFLDS .EQ. 4) GO TO 15
C       ... OTHERWISE, BAD VALUE IN NFLDS,
        PRINT 1003
 1003   FORMAT(1H ,'CNTR:FAILED. NFLDS VALUE TOO BIG IN CALL SEQUENCE')
        PRINT *,' NO OF FLD=',NFLDS
        STOP 30

C     =================================================================
C     . . .   S T E P (5.1) 
C     ...         PREPARE MAINLINE GENERATOR FUNCTION FOR THIS MAP
C     ...         TEST FOR ADEQUATE SPACE ALLOCATED FOR CONTOURING NFLDS
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

 15   CONTINUE
C
C     ***          ***                   ***
C ...      IQ = CADJ(14)
C ...      N = 35
C ...      ISTOP = ICHK(2)
C ...      ILNTH = (8 + NFLDS*5)*IP + NFLDS*(2*IQ*IP + N)
C ...      L1728 = CADJ(10)
C ...      IFIRST = 0
C ...      N = ILNTH + (LOC(PHASE)-LOC(IBUFF))/4
C ...      PRINT 1004,N
C ... 1004 FORMAT(1H ,'CNTR: WORDS REQUIRED IN ISPACE =',I6)
C ...      IF(ICHK(2) .LT. N) THEN
C ...        STOP 30
C ...      ENDIF

      GO TO 16

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     . . .   S T E P (5.2) 
C     ...         PREPARE MAINLINE GENERATOR FUNCTION FOR THIS MAP
C     ...         PREPARE OUTPUT BUFFER FOR FIRST RECORD
C     ...                 AND POINTERS FOR OUTPUT
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
 16   CONTINUE
C
C     ***          ***                   ***
      IF(IAND(MAP(2),16) .NE. 0) THEN
C       ... IF ADDING-ON TO PREVIOUSLY LEFT-OPEN PRODUCT FILE,

        GO TO 30
      ENDIF
C     ... OTHERWISE, INITIALIZE FOR NEW OUTPUT,
      DO  L=1,360   		!... i*4 iout(360) == 1440-byte buffer
        IOUT(L)=0
      ENDDO

      IOUT(1) = DFL   		!... START-OF-PRODUCT FLAG

      IF(IFID(1) .EQ. 0) GO TO 19
        IOUT(1)=IOR(IAND(MSK,IFID(1)),IOUT(1))
        LCKPT = 2137
        PRINT  888, LCKPT
ckumar        DO  L=2,12
        DO L=2,6
          IOUT(L)=IFID(L)
        ENDDO

 19   CONTINUE
      MOUT(3) = 64
      MOUT(4) = 4
      MOUT(5) = 1439
      MOUT(6) = CADJ(12)
      MOUT(7) = 0

      GO TO 30

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C     . . .   S T E P (5.3)  ... CALL CNTRI() ...
C     ...         PASS CONTROL TO  MAINLINE GENERATOR FUNCTION
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
 30   CONTINUE
C
C     ***          ***                   ***
      ML = IABS(INDEX(1,1)) + 2
      LCKPT = 2147
      PRINT  888, LCKPT

      PRINT *,'SHAD1=',SHAD1(1),' ',SHAD1(2),' ',SHAD1(3),' ',SHAD1(4)
      PRINT *,'SHAD2=',SHAD2(1),' ',SHAD2(2),' ',SHAD2(3),' ',SHAD2(4)
           CALL CNTRI(IRET_CNT, IMAGE, IMAGSIZ_WRDS, IWINDOW,
     1                 MAP, LABEL, INDEX,NFLDS,
     2                 FLD1, DASH1, OFSET1, SHAD1,
     3                 FLD2, DASH2, OFSET2, SHAD2,
     4                 FLD3, DASH3, OFSET3, SHAD3,
     5                 FLD4, DASH4, OFSET4, SHAD4)
C                .      .    .                                       .

C ...      CALL CNTRI(PHASE,IP,IQ,ILNTH,ML,INDEX(1,1),NFLDS,SCHED,
C ...     1           LABEL,ITAPE)

C ...      IF(TWO .EQ. 6) THEN
C       ... WAS ERROR RETURN FROM CNTRI, SO ...
C ...        GO TO 50
C ...      ENDIF

C ...      IF(ICHK(3) .GT. 0) THEN
C       ... THIS IS THE LAST MAP, SO MARK AS END-OF-ALL-MAPS,

C ...        CALL ENDMAP(IOUT(1))

C ...      ENDIF

      LCKPT = 2149
      PRINT   888, LCKPT
C
C ...      IF(IAND(CADJ(6),8) .NE. 0) THEN
C        ... IS THIS A LEAVE-IT-OPEN CASE ???????
C ...         RETURN
C ...      ENDIF

C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ...   To plot the LABEL-array data to the IMAGE plane ...
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       LEBCDIC   = .FALSE.
C        ... SCAN THRU THE ITAPE 3 TIMES TO GET PRIORITIES SEPARATELY

       DO  IPRIO = 1,5
         LOOPRIOR = IPRIO - 1
         call prtitle(itape,LOOPRIOR,iwindow,LEBCDIC,
     1                IMAGE,MAXIWORD,MAXJSLINE, iret_plt)

         WRITE(6,435)IRET_PLT
  435    FORMAT(1H ,'CNTR::prtitle: RETN-CODE=',I6)

         IF(LANYTITLESQQ) THEN
           NRECSTART_STR = minij_lab(7,2)

           ICOUNTSTRIP = NPRIOR_LAB(IPRIO,2)
           IF(ICOUNTSTRIP .GT. 0) THEN
             WRITE(6,FMT='(1H ,''CNTR::PRTITLE: FOR LOOPRIOR='',I3,
     1                  '';  COUNT OF STRIP-TITLE ITEMS='',I5,
     2             /1H ,'' SO WE MUST CALL PRTITLE(FOR STRIP TITLES)'',
     3             /1h ,'' WHICH BEGIN IN RECORD NUMBER='',I5)')
     A            LOOPRIOR,ICOUNTSTRIP,NRECSTART_STR

             call prtitle(itape,LOOPRIOR,iwindow_STR,LEBCDIC,
     1                  IMAGE_STR,MAXIWORD_STR,MAXJSLINE_STR,iret_plt)

             WRITE(6,435)IRET_PLT

           ELSE
             WRITE(6,FMT='(1H ,''CNTR::PRTITLE: FOR LOOPRIOR='',I3,
     1                         '';  NO STRIP-TITLES TO PROCESS'')')
     A               LOOPRIOR
           ENDIF
         ENDIF
       ENDDO
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... To output the bitplane for checkout ...
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C      REWIND LUNRAS

C      ... checkout only to output pure-raster image files ... 

C      WRITE(LUNRAS,ERR=900) (IMAGE(I),I=1,IMGSIZNEED)

C      ... WAS ANY STRIP TITLES  WRITTEN TO IMAGE_STR ??
C      IF(NITMPLTED_STR .GT. 0) THEN
C        WRITE(6,525)NITMPLTED_STR,MXJVAL_STR,MXJLABITM_STR
C 525    FORMAT(1H ,'CNTR: STRIP-TITLE PLOTTED ITEM COUNT=',I7,
C    1         /1H ,7X,'MAX PLOTTED SCANLINE J-VALUE=',I7,
C    2         /1H ,7X,'FROM THE LABEL-ARRAY ITEM = HEX',Z17.16)

C        REWIND LUNRASST
C        do jln = 1,MAXJSLINE_STR

C          WRITE(LUNRASST,ERR=907) (IMAGE_STR(I,JLN),I=1,MAXIWORD_STR)

C        ENDDO

C      ELSE
C        WRITE(6,535)LUNRASST
C 535    FORMAT(1H ,'PLOT55AV: EMPTY STRIP-TITLES IMAGE; SO DID ',
C    1              'NOT OUTPUT ON UNIT=',I4)
C      ENDIF 

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... to convert image to Extended 6-bit format and output it ...

       imgtyp = 0    		!... main body of map
       CALL img2x6t(imgtyp,IMAGE,IMAGSIZ_WRDS, maxiword, MAXJSLINE,
     1              IWINDOW(IPXL_FR_MAX), MAXJSLINE, iret_ras2)

       if(iret_ras2 .NE. 0) then
         write(6,FMT='(1h ,''cntr::img2x6t:(main) Failed with '',
     1                     ''retn code='',I5)')
     A           iret_ras2

         IRET_CNT = 4
         go to 999
       endif
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      ... WAS ANY STRIP TITLES  WRITTEN TO IMAGE_STR ??
       IF(NITMPLTED_STR .GT. 0) THEN

         IMAGSIZ_STR = MAXIWORD_STR * MAXJSLINE_STR
         NPXLOUT_STR = 1728
         NROWSHOW_STR = MAXJSLINE_STR 	!... = 800
         LNCOUNT_STR = 800     		!... might be reduced by dataj

         imgtyp = 1    		!... passing the strip-title image
         CALL img2x6t(imgtyp,IMAGE_STR,IMAGSIZ_STR, maxiword_STR, 
     1                lncount_STR,
     2                npxlout_STR, nrowshow_STR, iret_ras2)

         if(iret_ras2 .NE. 0) then
           write(6,FMT='(1h ,''cntr::img2x6t:(strp-titles) Failed '',
     1                       ''with retn code='',I5)')
     A             iret_ras2
           IRET_CNT = 5
           go to 999
         endif
       else
         write(6,FMT='(1h ,''cntr: skipped img2x6t() for converting '',
     1                     ''strip-title image into X6B code'',
     2                /1h ,7X,''because no title data was plotted'')')
       endif
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C      ================================================================
       DO  I = 1,6
         I8IFID(I) = IFID(I)
       ENDDO

                LUNIPK6 = IFAXOUT
                NUMRECFAX  = NRECFAX
                

        WRITE(6,FMT='(1H ,''CNTR: CALLING REBLKFX4: LUNIPK6='',I3,
     1                '' NUMRECFAX='',I4)')LUNIPK6,NUMRECFAX 
     
       CALL REBLKFX4(LUNEXT6B,LUNIPK6,NUMRECFAX,
     1                       SCHED,C1IFID,MYOPTNBITS,iret_reb)
     
C PETER
C   I MUST SUBTRACT 1 FROM NUMRECFAX TO POSITION TO PROPER RECORD.
C PETER
             NRECFAX = NUMRECFAX - 1
                
        WRITE(6,FMT='(1H ,''CNTR: REBLKFX4: RETURN CODE='',I6,
     1    '' NRECFAX='',I4,'' NUMRECFAX='',I4)')
     2     iret_reb,NRECFAX,NUMRECFAX
 
C      ================================================================

       GO TO 999


C     ... ERROR IN OUTPUT FORMAT; RETRY
 50   CONTINUE
C ...      DO 51 I = 2,360
C ...        IOUT(I)=0
C ... 51   CONTINUE

C ...      IOUT(1) = EFL

C ...      CALL OUTP(IOUT(1),MOUT(1))

C ...      ICHK(1) = 0
C ...      PRINT 1012
C ... 1012 FORMAT(1H ,'CNTR:ERROR ... OUTPUT SYNCH ERROR.  TRY AGAIN')
C ...      GO TO 999


C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  900  CONTINUE
       WRITE(6,905)LUNRAS
  905  FORMAT(1H ,'CNTR: FAILED on write ERR on checkout raster ',
     1            'output UNIT=',I3)
       iret_cnt = 6
       go to 999

  907  CONTINUE
       WRITE(6,905)LUNRASST
       iret_cnt = 6
       go to 999

  910  CONTINUE
       WRITE(6,915)LUNBGD
  915  FORMAT(1H ,'CNTR: FAILED on read ERR on map background ',
     1            'HEADER record on UNIT=',I3)
       iret_cnt = 7
       go to 999

  920  CONTINUE
       WRITE(6,925)LUNBGD
  925  FORMAT(1H ,'CNTR: FAILED on read EOF on map background ',
     1            'HEADER record on UNIT=',I3)
       iret_cnt = 8
       go to 999

  930  CONTINUE
       WRITE(6,935)LUNBGD
  935  FORMAT(1H ,'CNTR: FAILED on read ERR on map background ',
     1            'data record on UNIT=',I3)
       iret_cnt = 9
       go to 999


  999  CONTINUE
       RETURN
       END
