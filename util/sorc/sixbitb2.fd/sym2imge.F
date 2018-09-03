#if !defined (_BIG_ENDIAN) && !defined (_LITTLE_ENDIAN)
#error 'Neither _BIG_ENDIAN nor _LITTLE_ENDIAN is defined.'
#endif

       SUBROUTINE SYM2IMGE(IPL,JPL,IROTRA,NEWPXLHGT,NEWPXLWID,LERASE,
     1                     IMAGE,MAXIWORD,MAXJSLINE,IRET_S2I)
C                                                24-OCT-1996/DSS
C      ... CHANGED NAME FROM sym2imag() to sym2imge() BECAUSE I ADDED
C      ...   A RETURN CODE TO THE CALL-SEQUENCE.
C      ... 
C      ... THIS IS LOOKING AT THE IMAGE WITH ORIGIN IN THE UPPER-LEFT
C      ... AND WITH LOWEST VALUED SCANLINE AT THE TOP OF PAGE
C      ... WITH PIXELS SWEEPING LEFT-TO-RIGHT STARTING AT THE FAX-EDGE
C      ... AT THE LEFT EDGE;
C
C      ... THE GIVEN POINT(IPL,JPL) IS THE PIXEL AT WHICH TO POSITION 
C      ... THE UPPER-LEFT CORNER OF THE SYMBOL (WHICH IS A SYMBOL'S 
C      ... ORIGIN -- WHICH IS THE LOWER-RIGHT CORNER OF THE LETTER
C      ... IN THE REGULAR CHAR SET SINCE THE CHARAC DEFINITIONS ARE
C      ... BIT-MAPPED UPSIDE-DOWN);
C 
C      ... THIS PERFORMS NO ADJUSTMENTS TO THE POSITION, SO YOU MUST
C      ... DO ALL ADJUSTMENTS BEFORE YOU CALL ME.
C
C                                                19-MAR-1996/DSS
C      ... MODIFIED TO ADD IMAGE TO CALL SEQ INSTEAD OF DEFINING
C      ...    AS COMMON BLOCK
C                      TO BE ABLE TO WORK WITH VARIOUS DIMENSIONS,
C                      MAYBE THE IMAGE SHOULD BE ONE-DIMENSIONED
C                      AND I SHOULD COMPUTE WHICH WORD BY KNOWING
C                      MAXIWORD,MAXJSLINE
C                      BUT FOR SIMPLE DEMO, KEEP IT SIMPLE
C
C                                                 01-JUL-1999/VKK
C      ... KRISHNA KUMAR MODIFIED THIS CODE FOR IBM RS/6000
C          COMPL & MASK ARE CRAY SPECIFIC FUNCTIONS - THE IBM 
C          EQUIVALENT FUNCTIONS HAVE BEEN ADDED IN THE GRAPHICS
C          LIBRARY IN /nwprod/gphlib90/gphlib.source
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      
        INTEGER          IMAGE(MAXIWORD,MAXJSLINE)
C       . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
        integer    kwrdszbyts
c GWV  1/20/99 made compl an i*8 function
        integer (kind=8)  compl
        parameter (kwrdszbyts=8)   		!... CRAY long int
        integer    kwrdszbits
        parameter (kwrdszbits=8*kwrdszbyts)    	!... CRAY 64-bit wrd

       INTEGER   IPL,JPL
       INTEGER   IROTRA(NEWPXLHGT)
       INTEGER   NEWPXLWID
       LOGICAL   LERASE    			!... <= .T. erase under 
       INTEGER   IRET_S2I

       LOGICAL   LNEED2
       INTEGER   IBGWORK(2)
       INTEGER   MSKLEFT
       INTEGER   MSKRIGHT
       INTEGER   NOTMSKLEFT
       INTEGER   NOTMSKRIGHT
CKUMAR
       LOGICAL PRINTCAP1,PRINTCAP2
       DATA PRINTCAP1 /.TRUE./
       DATA PRINTCAP2 /.TRUE./
CKUMAR


       SAVE

       IRET_S2I = 0

       IPLM1 = IPL - 1
       ITHWORD = IPLM1 / kwrdszbits   		!... [0,NWORDS]
       ITHWORD = ITHWORD + 1
       ITHBITINWORD = MOD(IPLM1,kwrdszbits)   	!... BIT [0:63]
       IF(ITHWORD .LT. 1) then
          IRET_S2I = 1
          go to 999
       ELSE IF(ITHWORD .GT. MAXIWORD) THEN
          IRET_S2I = 2
          GO TO 999
C         ... FOR KEEP IT SIMPLE STAGE, JUMP OUT IF OUT OF BOUNDS ...
       ENDIF

       LNEED2 = .FALSE.
       IF((ITHBITINWORD + NEWPXLWID) .GT. kwrdszbits) THEN
C        ... WE WILL NEED ADJACENT WORD ON THE SAME SCANLINE ...
         LNEED2 = .TRUE.
         IF((ITHWORD + 1) .GT. MAXIWORD) THEN
           IRET_S2I = 3
           GO TO 999
C          ... FOR KEEP IT SIMPLE STAGE, JUMP OUT IF AT EDGE ...
         ENDIF
       ENDIF
       JCURR = JPL - 1
       DO LINSYM = 1,NEWPXLHGT
         JCURR = JCURR + 1
         IBGWORK(1) = 0
         IBGWORK(2) = 0
         IF((JCURR .GT. 0) .AND.
     1      (JCURR .LE. MAXJSLINE)) THEN
           IBGWORK(1) = IMAGE(ITHWORD,JCURR)
           IF(LNEED2) THEN
             IBGWORK(2) = IMAGE(ITHWORD+1,JCURR)
           ENDIF
#if defined(_LITTLE_ENDIAN)
c           CALL SWAP_BYTES_IN_PLACE (IBGWORK, 8_8, 2_8)
#endif
           LINEDEF = IROTRA(LINSYM)
           IRIGHT = 0
           MSKSYM = MASK(NEWPXLWID)
           LINEDEF = IAND(LINEDEF,MSKSYM)        
           ILEFT = ISHFT(LINEDEF,-ITHBITINWORD)
           MSKLEFT = ISHFT(MSKSYM,-ITHBITINWORD)
           NBITOFF = (ITHBITINWORD + NEWPXLWID) - kwrdszbits
           IF(NBITOFF .GT. 0) THEN
             LEFSHIF = NEWPXLWID - NBITOFF
             IF((LEFSHIF .GT. 0) .AND.
     1          (LEFSHIF .LT. NEWPXLWID)) THEN
               IRIGHT = ISHFT(LINEDEF,LEFSHIF)
               MSKRIGHT = ISHFT(MSKSYM,LEFSHIF)
             ELSE
               IRIGHT = 0
               MSKRIGHT = 0
             ENDIF
           ENDIF

           IF(LERASE) THEN
             NOTMSKLEFT  = COMPL(MSKLEFT)
             NOTMSKRIGHT = COMPL(MSKRIGHT)
c                NOTMSKRIGHT = not(MSKRIGHT)
c                NOTMSKLEFT  = not(MSKLEFT )
             IBGWORK(1) = IAND(NOTMSKLEFT,IBGWORK(1))
             IBGWORK(2) = IAND(NOTMSKRIGHT,IBGWORK(2))
           ENDIF

           IBGWORK(1) = IOR(ILEFT,IBGWORK(1))
           IBGWORK(2) = IOR(IRIGHT,IBGWORK(2))
#if defined(_LITTLE_ENDIAN)
c           CALL SWAP_BYTES_IN_PLACE (IBGWORK, 8_8, 2_8)
#endif
	   write(92,'(2A,8(1x,I5),Z16.16)')
     +  'in sym2imge: ipl, jpl, newpxlhgt, newpxlwid, maxiword, ',
     +  'maxjsline, ithword, jcurr, ibgwork(1) = ', ipl, jpl,
     +  newpxlhgt, newpxlwid, maxiword, maxjsline, ithword, jcurr,
     +  ibgwork(1)
           IMAGE(ITHWORD,JCURR) = IBGWORK(1)
           IF(NBITOFF .GT. 0) THEN
	   write(92,'(2A,8(1x,I5),Z16.16)')
     +  'in sym2imge: ipl, jpl, newpxlhgt, newpxlwid, maxiword, ',
     +  'maxjsline, ithword+1, jcurr, ibgwork(2) = ', ipl, jpl,
     +  newpxlhgt, newpxlwid, maxiword, maxjsline, ithword+1, jcurr,
     +  ibgwork(2)
             IMAGE(ITHWORD+1,JCURR) = IBGWORK(2)
           ENDIF
         ENDIF
       ENDDO
CKUMAR CHECK THIS
CKUMAR
C IMAGE(MAXIWORD,MAXJSLINE)
c              IF (PRINTCAP1) THEN
c              write(92,*)'In SYM2IMGE image '
c              write(92,'(5(1X,Z16.16))')
c     1        ((IMAGE(i,j),j=1,MAXJSLINE),i=1,MAXIWORD)
c              PRINTCAP1 = .FALSE.
c              ENDIF
  999  CONTINUE
       RETURN
       END
C
