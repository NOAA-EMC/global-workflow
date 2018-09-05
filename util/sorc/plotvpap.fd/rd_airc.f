      SUBROUTINE rd_airc (LUNAIR,LINITQ,cfil_hdr,LONELVL, IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    rd_airc     READ AN aircraft AND GET ONE LVL DATA
C   PRGMMR: SHIMOMURA        ORG: W/NP12     DATE: 97-02-18
C
C ABSTRACT: THIS rd_airc SUBROUTINE IS CALLED FROM WITHIN THE PLOTPAP 
C   PROGRAM TO READ AN aircraft OBSERVATION IN ORDER TO FETCH A SINGLE
C   aircraft's OBSERVED QUANTITIES AT ITS REPORTING LEVEL.
C   THE INPUT aircraft OBSERVATIONAL DATA FILE IS IN AN INTERMEDIATE 
C   FORMAT WHICH HAS BEEN CREATED BY LARRY SAGER FOR PLOTPAP INPUT.
C
C PROGRAM HISTORY LOG:
C   97-01-06  ORIGINAL AUTHOR: DAVID SHIMOMURA
C   97-02-13  SHIMOMURA   CHANGED MAX NO. OF LEVELS ALLOWED TO =21 LVLS
C   97-02-18  SHIMOMURA - adapting rd_uupa to the task of reading
C                         aircraft observation;
C
C USAGE: CALL rd_airc (LUNAIR,LINITQ,cfil_hdr,LONELVL, IRET)
C   INPUT ARGUMENT LIST:
C     LUNAIR - INT      UNIT NUMBER OF INPUT UPA FILE.
C
C     LINITQ - LOGICAL  SWITCH TO INDICATE THAT THIS IS FIRST CALL
C                       TO READ THIS FILE; SO I WILL INITIALIZE
C                       COUNTERS AND FETCH THE FILE HEADER BEFORE
C                       PROCEEDING INTO THE OBSERVATIONAL DATA FETCHING;
C                       I WILL RESET THIS SWITCH TO .FALSE. SO THAT
C                       I WILL NOT COME THROUGH THE INITIALIZATION
C                       SECTION AGAIN.
C
C
C
C   OUTPUT ARGUMENT LIST:
C     cfil_hdr(MAXWRDHDR) - C*8 CFIL_HDR(50)       WHERE MAXWRDHDR=50;  
C                           DESTINATION FOR UPA FILE HEADER
C        
C     LONELVL(MAXWRDLVL)  - INT LONELVL(24)        WHERE MAXWRDLVL=24;
C                           DESTINATION FOR OBSERVATIONAL DATA
C                           FOR ONE LEVEL;
C
C     IRET - INTEGER RETURN CODE
C
C   INPUT FILES:
C     UPAUPA FILE OF OBSERVATIONS WHOSE UNIT NUMBER IS LUNAIR
C
C   OUTPUT FILES:
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C REMARKS:
C   THE PROGRAM LOGIC WAS DERIVED FROM LARRY SAGER'S LISTER.
C 
C   SEE:   R. ALLARD (1973): "W3AI02 - ADP REPORT UNPACKER", 
C          especially APPENDIX A and APPENDIX C 
C          FOR THE SEQUENCE AND CONTENT OF DATA ITEMS IN THE INPUT FILE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C
C     ... patched to permit higher levels ...          13-Feb-1997/dss
C
C                                                       3-Feb-1997/dss
C
C     To read one upper-air station's report, and extract one millibar 
C     level's observational data
C     ...                   
C     ... program logic:
C     ... The ability to scan from one report to the next report depends 
C     ... on the pointer to the end-of-the-current-report; which pointer
C     ... is found in "word"(3) of the preamble of every report;
C     ... where "word" is the 32-bit word of legacy code.
C         . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C         KNEXT = subscript into the 512 longword input buffer
C                    C8INPBUF(KNEXT); 
C                    where KNEXT is whereami right now;
C                 Initial value is obtained from Cfil_hdr(5);
C                    which is expected to point to a longword
C                    containing "STR_REPO" ... for start-of-report; or
C                    else       "ENDOFFIL" ... for logical end-of-file;
C                        which are the only two acceptable values here.
C
C                 When the "STR_REPO" has been found at C8INPBUF(KNEXT),
C                    then we increment KNEXT to look at the following
C                    longword.  This very first longword after the 
C                    "STR_REPO" contains the lat/lon.
C 
C         . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C         K4ENDREP -  The second longword after the "STR_REPO" contains 
C                     the Observation Time in its low-order 32-bit word,
C                     and contains K4ENDREP in its hi-order 32-bit word.
C
C                 The value in this K4ENDREP is the pointer to the 
C                 32-bit word within a 32-bit I*4 word data input buffer  
C                 which points to the end of this current report; 
C                         I*4 BIN(K4ENDREP)   == "END_"
C                         I*4 BIN(K4ENDREP+1) == "REPO"
C
C                 A pointer to the 64-bit longword within C8INPBUF()
C                 which points to the end of this current report
C                 could be derived from K4ENDREP:
C                         K8ENDREP = (K4ENDREP + 1) / 2
C                         C8INPBUF(K8ENDREP) == "END_REPO"
C
C
C         . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C         Every call to this Subr rd_airc() will try to return one
C         aircraft observation.  How does it save the pointers from one
C         call to the next?
C              it seems to save KEND  in arr(299);
C                           and KNEXT in arr(300);
C              and initializes from arr(299),(300) at entry;
C              This assumes that the caller leaves arr() unchanged
C              between calls;  This logic WAS changed. 
C         . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
      INTEGER     LASTCH		!... INTEGER FUNCTION LASTCH
      EXTERNAL    LASTCH

      INTEGER KPTR_IB			!... WHERE IS POINTER (IB) ???
      DATA    KPTR_IB    / 14 /		!...    IN LONELVL(16)
C                                       !... BUT I MOVED IT TO (14)

      INTEGER     MAXWRDLVL
      PARAMETER  (MAXWRDLVL=24)

      integer     MAXWRDHDR
      PARAMETER  (MAXWRDHDR=50)

      COMMON  /RDUPABFS/ K8ENDREP, KNEXT, NBUFUPA,
     1                   LPREVEND, LPREVNXT, NBUFPREV,JUPABUF,I4UPABIN
      INTEGER           LPREVEND
      INTEGER           LPREVNXT
      INTEGER           NBUFPREV
      
      INTEGER           K8ENDREP
      INTEGER           KNEXT
      INTEGER           NBUFUPA

      INTEGER          JUPABUF(512), JUPABUF2(512)
      CHARACTER*8      C8INPBUF (512)	!... 512 longwords=4096 byte/blk
      EQUIVALENCE     (JUPABUF2(1),C8INPBUF(1)(1:1))

      INTEGER          I4UPABIN(1024)
      
C     ... TO UNPACK THE 64-BIT WORDS INTO 32-BIT WORDS,
C ... CALL GBYTES(JUPABUF,I4UPABIN,NOFFSET,NBITSGRP,NPADBITS,NGRPS2DO)
      INTEGER       NOFFSET
      DATA          NOFFSET       /  0 /
      INTEGER       NBITSGRP
      DATA          NBITSGRP      / 32 /
      INTEGER       NPADBITS
      DATA          NPADBITS      /  0 /
      INTEGER       NGRPS2DO
      DATA          NGRPS2DO      / 1024 /
     
C ...      REAL*8        ARR(*)
C ...      KEND  = ARR(299)			!... IFIX() ????
C ...      KNEXT = ARR(300)			!... IFIX() ????

      integer       LUNAIR
      LOGICAL       LINITQ
      CHARACTER*8   cfil_hdr(MAXWRDHDR)
      INTEGER       LONELVL(MAXWRDLVL)
      INTEGER       IRET

      CHARACTER*8   CFIL

      CHARACTER*8   CENDFILE
      DATA          CENDFILE   /'ENDOFILE'/
C
C
      INTEGER       MSKLHS
      DATA          MSKLHS         / Z'FFFFFFFF00000000' /
      INTEGER       NEGSIGNEXT
      DATA          NEGSIGNEXT     / X'FFFFFFFF00000000' /

      DATA  KLIM   /1024/
      DATA  IOLDU  /0/
C
C     ... FOLLOWING BITS ARE IN SAME POSITIONS AS IN OLD NBITS ...
      integer   LDZBIT			!... OLD HAD ONLY 4 BITS
      DATA      LDZBIT     / 4 /	!...    this is a new bit
      INTEGER   LWBIT
      DATA      LWBIT      / 3 /
      INTEGER   LZBIT
      DATA      LZBIT      / 2 /
      INTEGER   LTBIT
      DATA      LTBIT      / 1 /
      INTEGER   LTDBIT
      DATA      LTDBIT     / 0 /
      INTEGER   IIIIOLD
C
      INTEGER   ISOBSERVED
      INTEGER   LBIT
      INTEGER   MISSING
      DATA      MISSING       / X'7FFFF000' /
      INTEGER   MSKOBSBITS
      DATA      MSKOBSBITS    / X'0003F000' /		!... 6 OBS QUAN

      integer       name(2)
      character*8   cname(2)
      equivalence  (name(1),cname(1))

      character*8   cnamstn
      CHARACTER*8   CNAMPREV

      INTEGER       K4ENDREP

      integer       isrc,kkstart,indexlvl,IB,millibarix,ntotlvls
      
      INTEGER       ICOUNTAC
      INTEGER       MSKRHS
      DATA          MSKRHS  / X'00000000FFFFFFFF' /
      INTEGER       IALT
      REAL          PRES

      INTEGER       LCKPT

      SAVE
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      IRET = 0

      LCKPT = 100

C ...      K8ENDREP  = ARR(299)
C ...      KNEXT = ARR(300)
C ...      ... THOSE HAVE BEEN SAVED IN COMMON /RDUPABFS/

      DO  I = 1,MAXWRDLVL
        LONELVL(I) = 0
      ENDDO
C
C     . . . . . . . . .   FILE HEADER     . . . . . . . . . . . . . . .
C
      IF(LINITQ) THEN
         LINITQ = .FALSE.	!... RESET SO IT WILL NOT COME AGAIN

C ...      IF (LUNAIR. NE. IOLDU) THEN
C ...         IOLDU = LUNAIR
C
C        ... FIRST READ FOR THIS UPAUPA FILE. READ IN THE
C        ...    HEADER RECORD AND INITIALIZE
C
C        . . . . . . . . . . . . . . . . .
         LCKPT = 111
         ICOUNTAC = 0
         NBUFUPA = 0
         CNAMPREV(1:8) = ' '

         REWIND  LUNAIR
         READ  (LUNAIR,ERR=910,END=920) JUPABUF
         JUPABUF2 = JUPABUF
         call byteswap(JUPABUF2(1), 8, 512)
         NBUFUPA = NBUFUPA + 1

        CALL GBYTES(JUPABUF,I4UPABIN,NOFFSET,NBITSGRP,NPADBITS,NGRPS2DO)
         do ihw = 1,NGRPS2DO
           IF(BTEST(I4UPABIN(IHW),31)) THEN	!... HI-ORDER BIT
              I4UPABIN(IHW) = IOR(NEGSIGNEXT,I4UPABIN(IHW))
           ENDIF
         ENDDO
C        . . . . . . . . . . . . . . . . .
C
C        ... STORE THE HEADER RECORD INTO THE cfil_hdr ARRAY
C
         DO I = 1,MAXWRDHDR
           CFIL_HDR(I)(1:) = ' '
         ENDDO

         DO K = 1,MAXWRDHDR
            KSAVE = K      
            cfil_hdr(K) = C8INPBUF(K)
            IF (cfil_hdr(K) .EQ. 'ENDBLOCK') GO TO 130
         END DO

  130    CONTINUE
         WRITE(6, 135) (cfil_hdr(KK),KK=1,KSAVE)
  135    FORMAT(1H ,'rd_airc: Header read is ...',/,8(8a8,/))

         KNEXT = JUPABUF(5)
      END IF		!... ON PROCESSING THE FILE HEADER
C
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C     ...  CHECK FOR THE END OF FILE
C
  140 CONTINUE
      IF(C8INPBUF(KNEXT).EQ.CENDFILE) THEN
          IRET = -1
          write(6,145)
  145     format(1H ,'rd_airc: Found  NORMAL logical End-of-File')
          GO TO 999			!... EXIT
      END IF

      IF(C8INPBUF(KNEXT).NE.'STR_REPO') THEN
          write(6,165) KNEXT
  165     format(1H ,'rd_airc: Failed to find  "STR_REPO" AT ',
     1               'C8INPBUF(',I8,')')
          PRINT *,' JOB STOPPING'
          IRET = -1
          GO TO 999			!... ERROR EXIT
      END IF

C     ... OTHERWISE, FOUND "STR_REPO" AT C8INPBUF(KNEXT) ...
      KNEXT = KNEXT + 1		
C               !... KNEXT AT 1ST LONGWORD BEYOND THE "STR_REPO" 

C     =================================================================
          LCKPT = 200
C ...          J = 1
C
C         ... TO MOVE ONE LEVEL'S DATA  OF ONE AIRCRAFT REPORT
C
C         ... TO MOVE PREAMBLE OF FIRST 16 WORDS IN SEQUENCE ...
          KKSTART = 2*KNEXT - 1
C         ... where KKSTART points to first "hafword" of preamble
C         ...                    where hafword is unpacked 32-bit words
          JJSRC = KKSTART - 1
          do  i = 1,16
            jjsrc = JJSRC + 1
            IF(JJSRC .GT. KLIM) THEN
C              ... REPLENISH THE I4UPABIN() ...
               LCKPT = 210
               CALL fillubin(LUNAIR,LCKPT,IRET_FIL)
               JUPABUF2 = JUPABUF
               call byteswap(JUPABUF2(1), 8, 512)

               IF(IRET_FIL .NE. 0) THEN
                  IRET = IRET_FIL
                  GO TO 999
               ENDIF

               JJSRC = 1
               
               KKSTART = KKSTART - KLIM
C              ... because I refreshed the buffer during the preamble
C              ...   I need a decremented-by-1024 value of KKSTART, 
C              ...   below the crease,
C              ...   to compute (ISRC) for the millibar lvl data
C              ...   in the following paragraph

            ENDIF

            lonelvl(i) = I4UPABIN(JJSRC)

C           ------------------------------------------------------

            IF(I .EQ. 3) THEN		!... WORD(3) IS END-REC PTR
C             ... UPDATE THE END-RECORD POINTER ...
              K4ENDREP = I4UPABIN(JJSRC)/4096	!... I*4 WORD(3)
              IF (K4ENDREP .GT. KLIM) THEN	!... K : 1024

                WRITE(6,225)K4ENDREP,KLIM
  225           FORMAT(1H ,'rd_airc:BRIDGED END-REPORT POINTER ',
     1                     '(K4ENDREP)= (',I8,')',
     2                /1H ,7X,'LIMITING BUFFER SIZE=',I6)

C               ... failed here on last report within record no. 3
C               ...   in which K4ENDREP = 1025
C               ... Try to go on with end-ptr set for start of next rec
C               ...   without refilling buffer yet,

                K4ENDREP = MOD(K4ENDREP,KLIM)

C ...                write(6,235)
C ...  235           format(1h 'rd_airc: JOB STOPPING')
C ...                IRET = -1 
C ...                GO TO 999		!... ERROR EXIT
              END IF
              K8ENDREP = (K4ENDREP+1)/2
            ENDIF
          enddo
C         ... which completed the moving of 16-word preamble of acft
C         ... but, since this is not RAOB, I do not need the raob
C         ... pointers in (13) and (14); so if I overwrite those
C         ... with category 6 pointers in (15) and (16), then the
C         ... results should be same structure
          LONELVL(13) = LONELVL(15)
          LONELVL(14) = LONELVL(16)

C         ------------------------------------------------------------
          ICOUNTAC = ICOUNTAC + 1

          IB   = ishft(LONELVL(KPTR_IB),-12)

          IDEST = KPTR_IB
          ISRC =  KKSTART -1 + IB - 1
C            ... WHERE THE POINTER: IB IS THE RELATIVE I*4 WORD
C                                   RELATIVE TO ZERO AT (KKSTART-1)
C                             ISRC             IDEST
C         ...I4UPABIN(KKSTART-1+IB)    LONELVL(14+1)  PRESS-ALT(METERS)
C                    (KKSTART-1+IB+1)         (14+2)  TEMPERATURE
C                    (KKSTART-1+IB+2)         (14+3)  DEWPOINT
C                    (KKSTART-1+IB+3)         (14+4)  WIND DIR
C                    (KKSTART-1+IB+4)         (14+5)  WIND SPEED
C                    (KKSTART-1+IB+5)         (14+6)  MARKERS
C                    (KKSTART-1+IB+6)         (14+7)  PRESSURE (MBS)
C                                           (14+8) <== LBITS + IIII BITS

            ISOBSERVED = 0
            LBIT = 11
            DO ITEMOBS = 1,7   	!... GET 7 OBS QUANTITIES FOR LVL
              LBIT = LBIT + 1
              ISRC = ISRC + 1
              IF(ISRC .GT. KLIM) THEN
                LCKPT = 250
                CALL fillubin(LUNAIR,LCKPT,IRET_FIL)
                JUPABUF2 = JUPABUF
                call byteswap(JUPABUF2(1), 8, 512)

                IF(IRET_FIL .NE. 0) THEN
                  IRET = IRET_FIL
                  GO TO 999
                ENDIF

                ISRC = ISRC - KLIM
              ENDIF

              LONELVL(IDEST+ITEMOBS) = I4UPABIN(ISRC)
C             ... is that isrc yieldin Operand range error?
C             ... TO SET A BIT TO INDICATE THE PRESENCE OF OBSERVED QUAN
              IF(I4UPABIN(ISRC) .NE. MISSING) THEN
                 ISOBSERVED = IBSET(ISOBSERVED,LBIT)
              ENDIF
            ENDDO

            ISOBSERVED = IAND(ISOBSERVED,MSKOBSBITS)
C           ... add to this bit word what the old user expected ...
            iiiiold = 0
            if(btest(isobserved,12)) then		!... ZZZZ
              IIIIOLD = ibset(iiiiold,LZBIT)
            endif
            if(btest(isobserved,13)) then		!... TT
              IIIIOLD = ibset(iiiiold,LTBIT)
            endif
            if(btest(isobserved,14)) then		!... TD
              IIIIOLD = ibset(iiiiold,LTDBIT)
            endif
            if((btest(isobserved,15)) .and.		!... dd
     1         (btest(isobserved,16))) then		!... fff
              IIIIOLD = ibset(iiiiold,LWBIT)
            endif

C                ... since I do not plot a DZ for acft obs,
C ...            if(btest(isobserved,17)) then		!... DZ
C ...              IIIIOLD = ibset(iiiiold,LDZBIT)
C ...            endif
            
            LONELVL(IDEST+8) = ior(ISOBSERVED,iiiiold)
       

C         ... how can i move KNEXT to point to end of rec?
          IF(LPREVEND .GT. K8ENDREP) THEN
C           ... here is the case where this report spanned the end-of-
C           ... input-buffer;
   
C           ... Has the record number been upped already?
            IF(NBUFPREV .EQ. NBUFUPA) THEN
C             ... NO, the record number has not been upped; so read 
              LCKPT = 420
              CALL fillubin(LUNAIR,LCKPT,IRET_FIL)
              JUPABUF2 = JUPABUF
              call byteswap(JUPABUF2(1), 8, 512)

              IF(IRET_FIL .NE. 0) THEN
                  IRET = IRET_FIL
                  GO TO 999
              ENDIF
            endif
          ENDIF


          if(C8INPBUF(K8ENDREP) .NE. 'END_REPO') THEN
C           ... failed here after reading the 2nd buffer full ...
            WRITE(6,475)K8ENDREP,jupabuf(K8ENDREP),NBUFUPA
  475       FORMAT(1H ,'rd_airc: FAILED.  I am lost in data file',
     1            /1h ,7X,'Expected end-report in C8INPBUF(',I6,
     2                ') = hex',Z16,
     3            /1h ,7X,'COUNT OF RECORDS READ=',I6)
            IRET = 3
            GO TO 999
          else
            name(1) = lonelvl(11)
            name(2) = lonelvl(12)
            cnamstn(1:4) = cname(1)(5:8)
            cnamstn(5:8) = cname(2)(5:8)

C ...            IF(CNAMSTN(1:8) .NE. CNAMPREV(1:8)) THEN
C ...              WRITE(6,478)cnamstn(1:6), K8ENDREP,NBUFUPA
C ...  478         FORMAT(1H ,'rd_airc: '
C ...     1                'Found end-of-record ',A6,' in C8INPBUF(',I6,
C ...     2                  ') in rec no =',I6)
C ...            ENDIF
            CNAMPREV(1:8) = CNAMSTN(1:8)
          ENDIF

          KNEXT = K8ENDREP + 1
C         -----------------------------------------------------------
          IF(KNEXT .GT. 512) THEN
            KNEXT = MOD(KNEXT,512)
          ENDIF
          IF(LPREVNXT .GT. KNEXT) THEN
C           ... here is the case where this report spanned the end-of-
C           ... input-buffer;   
C           ... Has the record number been upped already?
            IF(NBUFPREV .EQ. NBUFUPA) THEN
C             ... NO, the record number has not been upped; so read
 
              LCKPT = 480
              CALL fillubin(LUNAIR,LCKPT,IRET_FIL)
              JUPABUF2 = JUPABUF
              call byteswap(JUPABUF2(1), 8, 512)

              IF(IRET_FIL .NE. 0) THEN
                  IRET = IRET_FIL
                  GO TO 999
              ENDIF
            endif
          ENDIF

C         -----------------------------------------------------------
C         . . .   S A V E   . . . . . . . . . . . . . . . . . . . .
          LPREVNXT = KNEXT		!... PTR TO "STR_REPO" OF NEXT
          LPREVEND = knext -1		!... PTR TO "END_REPO" OF THIS
          NBUFPREV = NBUFUPA		!... COUNT OF BUFFER IN OF NEXT
          GO TO 999

C         ... and where is K8ENDREP supposed to be left at?

C         ============================================================

  910  CONTINUE
       WRITE(6,915)LCKPT,LUNAIR,NBUFUPA
  915  FORMAT(1H ,'rd_airc:AT LCKPT=',I5,'FAILED ON PARITY ERROR WHILE ',
     1            'READING FROM UNIT=',I3,
     2       /1H ,'        COUNT OF INPUT BLOCKS READ BEFORE ERR =',I8)
       IRET = 1
       GO TO 999

  920  CONTINUE
       WRITE(6,925)LCKPT,LUNAIR,NBUFUPA
  925  FORMAT(1H ,'rd_airc:AT LCKPT=',I5,'FAILED ON PHYSICAL ',
     1            'END-OF-FILE WHILE READING FROM UNIT=',I3,
     2       /1H ,'        COUNT OF INPUT BLOCKS READ BEFORE END =',I8)
       IRET = 2
       GO TO 999

  999  CONTINUE
       RETURN
       END
