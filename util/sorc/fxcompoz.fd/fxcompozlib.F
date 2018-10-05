       subroutine ALLARGV(narg_lmt,carg,narg_got,iret_args)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ALLARGV     GET ALL COMMAND-LINE ARGUMENTS
C   PRGMMR: HENRICHSEN       ORG: W/NP12    DATE: 1999-11-05 
C
C ABSTRACT: TO GET ALL COMMAND-LINE ARGUMENTS; RETURNING THEM
C   AS NULL-TERMINATED STRINGS IN A CHARACTER*112 ARRAY.
C
C PROGRAM HISTORY LOG:
C   95-05-09  DAVID SHIMOMURA
C   95-05-09  SHIMOMURA: MADE ALLARGU() ... AN INTERGRAPH VERSION 
C                        FROM CRAY VERSION;
C   95-05-23  SHIMOMURA: MADE ALLARGV() ... AN INTERGRAPH VERSION
C                        WITH LONGER LINES TO BE ABLE TO HANDLE
C                        THE PARM= ARG WITH ITS 100 CHAR EN-QUOTED TEXT
C   95-07-10  SHIMOMURA: MODIFYING ALLARGV() FOR THE CRAY
C 1999-11-05  HENRICHSEN: MODIFYING ALLARGV() FOR THE IBM, MADE THE
C                         FUNCTION iargc and nargsinline integer(4)
C
C    
C
C USAGE:    CALL ALLARGV(NARG_LMT, CARG, NARG_GOT, IRET_ARGS)
C                            1       2       3       4         
C   INPUT ARGUMENT LIST:
C     (1.) NARG_LMT - LIMITING NO. OF ARGS EXPECTED; 
C                     THE DIMENSION OF ARG2 
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C
C     (2.) CARG()   - CHARACTER*112  CARG(NARG_LMT)
C                     HERE ARE THE DESIRED ARGUMENTS
C                     AS NULL-TERMINATED CHAR STRINGS.
C                
C     (3.) NARG_GOT - COUNT OF ARGUMENTS WHICH WERE GOTTEN.
C
C     (4.) IRET_ARGS - RETURN CODE
C                    = 0;  NORMAL RETURN
C                    = -1; ABNORMAL EXIT.  TOO MANY COMMAND-LINE ARGS 
C                           FOR THE DIMENSION OF CARG ARRAY
C                          (NARG_LMT IS INADEQUATE)
C
C                    = POSITIVE NON-ZERO VALUE IS A WARNING: 
C                           ONE OR MORE OF THE GIVEN ARGS WAS TOO LONG, 
C                           SO I HAVE TRUNCATED THE TOO-LONG ONE(S);
C                           THE NTH BIT SET IS THE NTH ARGUMENT 
C                           WHICH WAS TOO LONG.
C
C                    THE RETURN CODE=-1 (TOO MANY ARGS) HAS PRIORITY 
C                       OVER THE POSITIVE NON-ZERO (ARG IS TOO LONG).
C
C   OUTPUT FILES:
C     FT06F001 - SOME CHECKOUT PRINTOUT
C
C REMARKS: 
C     CAUTION: THE USER MUST DEFINE THE CARG ARRAY AS 
C                 CHARACTER*112  CARG()
C
C     WARNING: THE SIZE OF EACH COMMAND-LINE ARGUMENT IS LIMITED TO 
C              A MAX OF 111-CHARACTERS; SO THAT ONE BYTE ADDED FOR THE
C              TERMINATING NULL CHARACTER WOULD FILL THE CHARACTER*112
C              CARG() ITEM.
C
C     WHEN THE USER WANTS THE CHAR COUNT OF ONE OF THE RETURNED
C        CARG() CHARACTER STRINGS SHE COULD INDEX ON THE NULL TERMINATOR
C              NCHARS = INDEX(CARG(IAR),NULLCHR) - 1
C
C     TO EMULATE THE IBM PARM FIELD, THE USER SHOULD KEY_IN ON THE
C        COMMAND LINE:
C              PARM='IN BETWEEN THE QUOTES IS THE PARMS'
C        WHAT IS RETURNED FROM ALLARGV() FROM THE PARM= ENTRY IS
C        ONE ARG WHICH STARTS WITH "PARM=" AND INCLUDES EVERYTHING
C        WHICH WAS WITHIN THE SINGLE-QUOTE SIGNS, BUT THE QUOTE SIGNS
C        THEMSELVES WILL DISAPPEAR; AND A NULL-TERMINATOR WILL BE ADDED.
C        THE STARTING "PARM=" IS A KEY WORD FOR THE PARMS, AND SHOULD 
C        NOT BE USED TO START ANY OTHER ARGUMENT.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C
       integer    kbytpwrd
       parameter (kbytpwrd=8)          ! ... FOR THE CRAY
       integer    maxnbyt
       parameter (maxnbyt=112)
       integer    maxnwrds
       parameter (maxnwrds=maxnbyt/kbytpwrd)
       integer    lmt_txt
       parameter (lmt_txt=maxnbyt-1)

C      ... call seq. args ...
       integer       narg_lmt
       character*112 carg(narg_lmt)
       integer       narg_got
       integer       iret_args

C
C      ... FUNCTIONS ...
       external  lastch
       integer   lastch
       external  notrail
       integer   notrail
C      -------------------------------------------------------------
       integer        jwork(maxnwrds)
       character*112  cwork
       equivalence   (jwork,cwork)
       
C      THESE MUST BE INTEGER(4) FOR FUNCTION CARG AND SUB GETARG.
       
       integer(4)     nargsinline 
       integer(4)     narguse
       integer(4)     iargc
       integer(4)     iar
C      -------------------------------------------------------------

       integer        nchars

       LOGICAL        LPARMQQ
       logical        LERROR
       character*1    BKSLSH       
       character*1    KLF
       character*1    NULLCHR
       character*1    lonech
       

C      . . . . . . . .   S T A R T   . . . . . . . . . . . . . . . .


       NULLCHR = char(0)
       KLF     = char(10)
       BKSLSH  = CHAR(92)
C
       LERROR = .FALSE.
       LPARMQQ = .FALSE.

       iret_args = 0
       narg_got = 0
       do i = 1,narg_lmt
         carg(i)(1:) = NULLCHR
       enddo
C                                                 15-Nov-1994/dss
C      ... to demo command-line argument fetching  3-Oct-1994/dss
C
C
       nargsinline = iargc()

       write(6,115) nargsinline
  115  format(1h ,'ALLARGV: count of args found in command line =', I3)

       if(nargsinline .gt. 0) then
         narguse = nargsinline
         if(narguse .gt. narg_lmt) then
           narguse = narg_lmt
           write(6,117)narguse,nargsinline
  117      format(1h ,'ALLARGV: WARNING... too many command-line args',
     A                '  *  *  *  *  *  *  *  *  *',
     1            /,1h ,'         Proceeding with only',I3,
     2                ' args out of',I3)
           LERROR = .true.
         endif

         do  iar = 1,narguse
           LPARMQQ = .FALSE.

           cwork(1:) = ' '

           call getarg(iar,cwork)

           narg_got = narg_got + 1
           nchars = lastch(cwork)

           if(nchars .le. 0) then
             write(6,125)iar
  125        format(1h ,'ALLARGV:getarg() returned an empty arg for',
     A                  ' no.',I3 )
           else
C            ... SOME TEXT EXISTS IN THIS ARG ...
C            ...   DOES IT START WITH "PARM=" ???
             if((cwork(1:5) .EQ. 'PARM=') .OR.
     1          (cwork(1:5) .EQ. 'parm=') ) then
               LPARMQQ = .TRUE.
C              ... this arg is special case of PARM=
C              ... which can include blanks, so cannot lastch() it ...
               nchars = notrail(cwork)
             endif
C ...             iwdss = ((nchars-1)/kbytpwrd) + 1
C            ... where iwdss points to last word so I could hex dump
C            ...    that last word, to see if NULL is there     
C            ... There was no NULL; only blank fill.      
             IF(LPARMQQ) THEN
C              ... FILTER OUT ANY BACKSLASH or LINE_FEED ...
               ioutc = 0
               do  inc = 1,nchars
                 if(ioutc .LT. lmt_txt) then
                   lonech = cwork(inc:inc)
                   if((lonech .EQ. BKSLSH) .OR.
     1                (lonech .EQ. KLF)) then
                   else
                     ioutc = ioutc + 1
                     carg(iar)(ioutc:ioutc) = lonech
                   endif
                 else
C                  ... comes here if ioutc .GE. lmt_txt, 
C                  ... so I cannot increment ioutc for this inc char
C                  ... so truncate the string at (1:ioutc)
C                  ... should a warning be return-coded???
                   itba = iar - 1
                   iret_args = ibset(iret_args,itba)
                   go to 155
                 endif
               enddo 
  155          continue
               nchars = ioutc
               np1 = nchars+1
               carg(iar)(np1:np1) = NULLCHR

             else
C              ... this is .not. a PARM field,
               if(nchars .GT. lmt_txt) then
C                ... truncate any string longer than 111 to 111
                 nchars = lmt_txt
C                ... should a warning be return-coded???
                 itba = iar - 1
                 iret_args = ibset(iret_args,itba)
               endif

               carg(iar) = cwork(1:nchars)//NULLCHR
             endif

           endif
         enddo
       endif

       IF(LERROR) THEN
         iret_args = -1
C        ... WHICH OVERRIDES THE TOO-LONG ARG INDICATOR WITH
C        ...   TOO MANY ARGS INDICATOR, WHICH IS MORE SERIOUS ERROR
       ENDIF

       return
       end
       integer function atoi(string)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C FUNCTION:      ATOI        convert ascii string to integer
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 95-07-10
C
C ABSTRACT: to convert ascii string to integer ...
C
C PROGRAM HISTORY LOG:
c   93-12-02  original author unknown
C   95-07-10  SHIMOMURA: MODIFYING ALLARGV() FOR THE CRAY
c   98-08-11  caruso     removed CDIR$ INTEGER=64 from top of this code.
c                        upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C
C USAGE:    iret = atoi(string)
c
C   INPUT ARGUMENT LIST:
c     string    -  character string
c
c   output argument list
c     iret      -  integer representation of character 'string'
c
c   remarks:
C     CAUTION: FAILS WITH SYSTEM-F-INTOVF, integer overflow
C              IF YOU FEED IT A TOO-LARGE-FOR 32-BIT-INTEGER NUMBER
C     CAUTION: THE CALLING PROGRAM SHOULD DECLARE THIS FUNCTION
C              WITH STATEMENT:  integer    ATOI
C     Caution: 2-Dec-1993 -- this is failing, so I added 
C                            D-line comments to debug
C              Deleted the include of global.def
C              Put isdigit logic in-line, replacing function
C     Caution: Expects a NULL-terminated string.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$
C
       character*(*)  string
C
       logical        LISDIGIT
C      ... I HAVE PUT LISDIGIT IN LINE INSTEAD OF FUNCTION ...
       integer        i
       integer        il
       integer        msign
       integer        lengivn
       integer        kchzero
       character*1    chone
       LOGICAL        CHECKOUT
       DATA           CHECKOUT   / .FALSE. /
       character*1    nullchr

       save
C
       nullchr = char(0)
C
       atoi = 0
       locnull = 0
       limitscan = 0
       lengivn = len(string)
       if(lengivn .le. 0) go to 999

       IF(CHECKOUT) THEN
         write(6,105)lengivn
  105    format(' atoi: entered with len(string)=',I5)
       ENDIF

       limitscan = lengivn
       locnull = index(string(1:lengivn),nullchr)
       if(locnull .gt. 0 .and. locnull .le. lengivn) then
         limitscan = locnull - 1
       endif
       if(limitscan .le. 0) then
         atoi = 0
         IF(CHECKOUT) THEN
           write(6,125)limitscan,locnull
  125      format(' atoi: jumping out at 125, since limitscan=',I5,
     *            '; locnull=',I5)
         ENDIF
         go to 999
       endif

       IF(CHECKOUT) THEN
         WRITE(6,205)limitscan
  205    FORMAT(' atoi: ready to scan with limitscan=',I5)
       ENDIF
       i = 1
C      ... to reposition pointer beyond leading blanks,
       do  ll = 1,LIMITSCAN
         if(string(i:i) .ne. ' ') go to 222
C        ... otherwise, i-th char is leading blank, so incr
         i = i + 1
       enddo

       if(i .gt. LIMITSCAN) go to 999
C        ... which tested for all blanks in given string
  222  continue
C      ... otherwise, i is pointing at first non-blank char w/i string,
       msign = 1
       if(string(i:i) .eq. '+' .or. string(i:i) .eq. '-') then
          if(string(i:i) .eq. '-') msign = -1
          i = i + 1
       endif
       if(i .gt. LIMITSCAN) go to 999
C      ... which tested for sign-only in given string.
C
       kchzero = mova2i('0')
       do il = i,LIMITSCAN
         chone = string(il:il)

c***what's LLE and LGE?????
         LISDIGIT = (LGE(CHONE,'0') .AND. LLE(CHONE,'9'))
         if(.not. LISDIGIT) THEN
           IF(CHECKOUT) THEN
             WRITE(6,315)il
  315        FORMAT(' atoi: jumping out on non-digit at il=',I5)
           ENDIF
           go to 333
C          ... which terminates scan on any non-digit character
         ELSE
           atoi = 10*atoi + (mova2i(chone) - kchzero)
         ENDIF
       enddo
  333  continue
       atoi = msign * atoi
       IF(CHECKOUT) THEN
         WRITE(6,345)atoi
  345    FORMAT(' atoi: came thru 333 w/ atoi =',I9)
       ENDIF

  999  continue
       return
       end
c**********************************************************************
       SUBROUTINE dcdlnx6b(lchkprntq,linecurr,nchs,LDUBL,nskpdib,npels,
     1                     c1pure,kbytpur, iret_pel)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    dcdlnx6b    data unpacker
C   PRGMMR: HENRICHSEN       ORG: W/NMC41    DATE:  1999-11-03 
C
C ABSTRACT:  decode 6 bit logical input data.
C
C PROGRAM HISTORY LOG:
C   87-05-22  shimomura  COPIED FROM DCD6 IN ORDER TO ADAPT IT TO READ JOHN SIMMONS' 
c                        OUTPUT FILE INSTEAD OF BEDIENT'S MAP FILE.
C                        A PRIMITIVE VERSION OF BEDIENT'S DCD4
C                        TO DECODE 6-BIT PACKED RASTER FORMAT MAP BGND.
C                        ONE SCANLINE PER CALL IS UNPACKED AND 
C                        THE RESULT IS PUT INTO LMAPWK.
C   95-08-16  shimomura  copied ~/vt240/dcdext6b.for into ~/cra/bg/count_pel.f
C                        to strip it down to simply count the pixels in one
C                        given complete scanline, which is encoded in NMC
C                        extended 6-bit code.
C                        Called from within CRAY package -- Main: look_x6b.f --
C                        in which I wanted to determine the bounds of the map bgnd
C                        so that I could set dimensions for the Type-9.
C   95-08-21  shimomura  copied count_pel.f into ~/car/bg/dcdlnx6b.f
C                        because I needed to build it up to a decoder
C                        of one scanline from x6b to pure_raster.
C                        Called from scn_x6bA() after it has extracted one 
c                        scanline (in x6b code) from the buffered data.
C   97-02-20  shimomura  copied ~/cra/bg/dcdlnx6b.f into ~/ncod/chk/dcdlnx6b.f 
C                        because when used in fxcompos package it failed when
C                        called with LDUBL= .T.;  it yielded a blank map cut.
C                        so I copied it into here so I could patch it without
C                        affecting its use in bg;
c   98-08-11  caruso     removed CDIR$ INTEGER=64 from top of this code.
c                        upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C   1999-11-03           HENRICHSEN MDIFY TO RUN ON IBM SP.
C
C USAGE:  CALL dcdlnx6b(lchkprntq,linecurr,nchs,LDUBL,nskpdib,npels,
C                       c1pure,kbytpur, iret_pel)
c
C   INPUT ARGUMENT LIST:
C          lchkprntq  - logical to get checkout print
C          linecurr   - character*1 array containing one complete scanline 
c                       in X6B
c          nchs       - array size of linecurr
C          LDUBL      - logical.  to double every pixel on scanline
C          nskpdib    - integer. count of dibytes (4-pixel groups) to skip
C                          off of the FAX edge of the scanline.    
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C          npels      - total number of pels in this scanline
C          c1pure     - char*1 destination array allocated for the 
C                                 decoded-into-pure-uncompressed-pels
c          kbytpur    - array size of c1pure
C          iret_pel   - return code
C
C   OUTPUT FILES:
C     FT06F001 - SOME CHECKOUT PRINTOUT
C
C REMARKS: 
C      ... PROCEDURE USED TO UNPACK THE DATA:
C      ... THE GIVEN DATA IS ENCODED IN 6-BIT LOGICAL GROUPS
C      ... RIGHT-JUSTIFIED IN 8-BIT BYTES (NOT CONCATENATED).
C      ... THE 6 BITS ARE FORMATTED: 'TTIIII' (IN BINARY)
C      ...   WHERE TT = TYPE OF DATA,
C      ...         IIII = DATA
C      ...   TT=00 FOR RUN OF ZEROS FOR WHICH IIII IS COUNT OF 
C      ...         DIBYTES OF ZEROS.
C      ...   TT=01 FOR RUN OF ONES  FOR WHICH IIII IS COUNT OF
C      ...         DIBYTES OF ONES.
C      ...            A SEQUENCE OF RUN COUNTS OF THE SAME TYPE
C      ...         IS INTERPRETED AS:
C      ...   TOTAL_COUNT= IIII(1ST) + 16*III(2ND) + 256*IIII(3RD)
C      ...   TT=10 FOR "AS_IS" FOR WHICH IIII IS THE 4 BITS OF
C                  RASTER DATA AS_IS.
C      ...   TT=11 FOR COMMAND OR SPECIAL SIGNAL
C                  FOR WHICH IIII=0000 MARKS END_OF_SCANLINE;
C                        AND IIII=0011 MARKS END_OF_THE_MAP.
C      ... AS EACH 6-BITS IS EXTRACTED, IT IS INTERPRETED AND
C      ... THE RESULTING UNPACKED SCANLINE DEVELOPS IN LMAPWK
C      ... BUT IN AN INTERMEDIATE FORM IN WHICH EACH 8-BIT BYTE
C      ... CONTAINS ONLY 4BITS OF DATA IN ITS LOW-ORDER HALF BYTE.
C      ... HOWEVER, IF THE DOUBLE_SCALE OPTION HAD BEEN REQUESTED,
C      ... THEN EACH 4-BITS OF DATA IS TRANSLATED INTO 8-BITS
C      ... IN WHICH EACH ORIGINAL BIT IS REPLACED BY TWO BITS.
C
C      ... WHEN ALL THE DATA FOR ONE SCANLINE HAS BEEN DECODED,
C      ... IF NOT DOUBLING SCALE, THEN THE INTERMEDIATE
C      ... FORM IS COMPRESSED TO ELIMINATE THE HIGH-ORDER 4BITS
C      ... OF EVERY BYTE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C
       logical     lchkprntq
       character*1 linecurr(nchs)       !... one complete X6B scanline
       LOGICAL     LDUBL
       INTEGER     NSKPDIB
       integer     npels     !... total number of pels in this scanline
       character*1 c1pure(kbytpur) 	!... space for one pure scanln
       integer     iret_pel    		!...  return code
C

       integer    nbytspwrd
C ...       parameter (nbytspwrd=4)   	!... for Intergraph
       parameter (nbytspwrd=8)   	!... for CRAY

       integer    lmtnpels
       parameter (lmtnpels=4192)
       integer    lmtngrps
       parameter (lmtngrps=lmtnpels/4)  !... = 1048 di-bytes
       integer    iwrksz		!... =  262 words(on Intergraph)
       parameter (iwrksz= lmtngrps/nbytspwrd)
C
       integer      intmapwrk(iwrksz)
       character*1  c1mapwrk(lmtngrps)
       equivalence (intmapwrk,c1mapwrk)

C ...  call pk4_8bit(c1mapwrk,IOTBY, nskpdib, c1pure,kbytpur, 
C ...                nbytres, iret_pk4)
       integer    nbytres
       integer    iret_pk4
       
       integer    keom
       data       keom     / X'33' /    	!... end-of-map
       integer    keos
       data       keos     / X'30' /     	!... end-of-scanline

       INTEGER    MSK6BT
       DATA       MSK6BT       / X'3F' /
       integer    MSK4BT
       DATA       MSK4BT       / X'0F' /
C
       INTEGER    LTTBLI(16)
       DATA       LTTBLI       / X'00', X'03', X'0C', X'0F',
     1                           X'30', X'33', X'3C', X'3F',
     2                           X'C0', X'C3', X'CC', X'CF',
     3                           X'F0', X'F3', X'FC', X'FF' /
       INTEGER    ITTPRE
       INTEGER    ITT
       INTEGER    IIII
       INTEGER    I6BIT
       INTEGER    IOTBY       		!... SUBSCRIPT PURE-RASTER LINE

       integer    ntotpels
       integer    jonegrp
       character*1     NULL
       character*1     cblack


       integer    nwhiterun
       integer    nblackrun
       INTEGER    NRUNDIBY
       logical    whitey
       LOGICAL    RUNNING
       integer    lckpt
      
       SAVE
C     ==============================================================
       iret_pel = 0
       NULL = char(0)
       lckpt = 100
       npels = 0
C      ... test given nchs for within bounds,
       ngrps = nchs
       if(ngrps .le. 0) then
C        ... given zero data bytes, so count=0 ...
         lckpt = 101
         go to 999
       else if(ngrps .GT. lmtngrps) then
C        ... warning: scanline is too long for me;
C        ...    truncate! and proceed ...
         iret_pel = 1
         ngrps = lmtngrps
       endif

C      ... test the codes ...
       ntotpels = 0
C      ... to zero out the pure-raster work array ...
       do  i = 1,iwrksz
         intmapwrk(i) = 0
       enddo

       if(LDUBL) then
         cblack = char(255)     	!... = X'FF'
       else
         cblack = char(15)    		!... = X'0F'
       endif

       LSHFRC = 0
       NRUNDIBY = 0
       RUNNING = .FALSE.
       IOTBY = 0
       ITT = 03
C      ... WHERE ITT IS INITIALIZED TO "COMMAND" TYPE
C      ... WHICH WILL BE MOVED INTO ITTPRE (THE PRECEDING ITT)
C      ... WHEN THE CURRENT ITT IS OBTAINED.

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       do  ig = 1,ngrps
         jonegrp = mova2i(linecurr(ig))
         if((jonegrp .GT. keom) .OR.
     1      (jonegrp .LT. 0))    then
C           ... bad code here ... file is not good.  ERROR STOP
            iret_pel = -1
            lckpt = 211
            go to 999
         else if(jonegrp .EQ. keom) then
C           ... end-of-map flag here; don't call me again
            iret_pel = -2
            lckpt = 222
            go to 800
         else if(jonegrp .EQ. keos) then
C           ... end-of-scanline flag here;
            lckpt = 233 
            go to 800
         else if(jonegrp .GT. keos) then
C           ... by this case, there should be no other 30+ codes ...
C           ... bad code here ... file is not good.  ERROR STOP
            iret_pel = -1
            lckpt = 244
            go to 999
         else
C           ... otherwise, the TT-codes are [ 00, 01, or 02] ...
C
            lckpt = 311
            ITTPRE = ITT
C           ... WHICH SAVES THE TYPE OF THE PRECEDING ITEM ...
            I6BIT = IAND(jonegrp,MSK6BT)

c***replace shiftr (I think) with ishift???
            ITT   = ISHFT(I6BIT,-4)
C           ... WHICH ISOLATES THE TT BITS OF TTIIII ...
            IIII  = IAND(jonegrp,MSK4BT)

C           . . . . . . . . . . . . 
            IF(ITT .EQ. 00) THEN
              lckpt = 322
C             ... COMES HERE IF TT = '00'BINARY
C             ...    WHICH IS COUNT OF RUN OF ZERO DIBYTES
              IF(ITTPRE .NE. 00) LSHFRC = 0
C             ... OTHERWISE, THE ITEM IMMEDIATELY PRECEDING THIS ONE
C             ...   WAS ALSO A COUNT OF ZEROS ...
C
              NRUNDIBY = IIII
              IF(LSHFRC .GT. 8) LSHFRC = 0
C             ... WHICH LIMITS RUN COUNTS

c***replace shiftl (I think) with ishft???
              NRUNDIBY = ISHFT(NRUNDIBY,LSHFRC)
              LSHFRC = LSHFRC + 4
              IOTBY = IOTBY + NRUNDIBY
C             ... FOR WHITES, SIMPLY MOVE PTR ALONG OUPUT BUFFER ...
              ntotpels = ntotpels + 4*nrundiby
C             . . . . . . GO TO 117   . . . . . . 
C
            ELSE IF(ITT .EQ. 01) THEN
              lckpt = 333
C             ... COMES HERE IF TT = '01'BINARY,
C             ...   WHICH IS COUNT OF RUN_OF_BLACK DIBYTES
              IF(ITTPRE .NE. 01) LSHFRC = 0
C             ... OTHERWISE, 
C             ...     THE PRECEDING ITEM WAS ALSO A COUNT OF BLACKS
C
              NRUNDIBY = IIII
              IF(LSHFRC .GT. 8) LSHFRC = 0
C             ... WHICH LIMITS RUN COUNTS ...


c***replace shiftl (I think) with ishft???
              NRUNDIBY = ISHFT(NRUNDIBY,LSHFRC)
              LSHFRC = LSHFRC + 4
              IF(IIII .LE. 0) THEN
                lckpt = 344
                GO TO 118
              ENDIF
C
              IF((IOTBY+NRUNDIBY) .GT. lmtngrps) THEN
                lckpt = 355
C               ... THIS RUN OF BLACKS WILL OVERFLO WORK AREA
C               ... SO TRUNCATE THE COUNT OF BLACKS TO FIT
                NRUNDIBY = lmtngrps - IOTBY
                IF(NRUNDIBY .LE. 0) GO TO 800
C               ... OTHERWISE, COUNT OF BLACKS EXISTS,
              ENDIF
C
              lckpt = 366
C             ... TO PUT COUNT=NRUNDIBY OF BLACKS TO WORK ARRAY,
              do  i = 1,nrundiby
                iotby = iotby + 1
                c1mapwrk(iotby) = cblack
              enddo
              ntotpels = ntotpels + nrundiby
C             . . . . .  GO TO 117  . . . . . . . 
            ELSE IF(ITT .EQ. 02) THEN
              lckpt = 411
C             ... COMES HERE FOR "AS_IS" DIBYTE IN IIII
              IF(LDUBL) THEN
                IIII = LTTBLI(IIII+1)
C               ... WHICH DOUBLES THE 4-BIT PATTERN FOR DOUBLING SCALE
              ENDIF
C             ... NOW, PUT THE CURRENT AS_IS DIBYTE ...
              IOTBY = IOTBY + 1
              c1mapwrk(IOTBY) = CHAR(IIII)

              LSHFRC = 0
C             ... ANY "AS_IS" WILL TERMINATE THE INCREMENTING OF 
C             ... THE FACTOR USED BY A PRECEDING RUN_COUNT SEQ.
              NTOTPELS = NTOTPELS + 4
C             . . . . .   GO TO 117   . . . . . . . 
            ENDIF
C            
C           ... 117  CONTINUE . . . . . . . . . . . 

C           ... CHECK IOTBY FOR END OF OUTPUT BUFFER ...
            IF(IOTBY .GE. lmtngrps) then
              lckpt = 433
              GO TO 800
C             ... WHICH JUMPS TO CLEANUP WHEN FINISHED ONE OUTPUT LINE
C             ...      DUE TO OUTPUT BUFFER FILLED UP
            endif
C           ... OTHERWISE, 
C           ...    IOTBY HAS NOT REACHED END OF THE OUTPUT BUFFER

  118       CONTINUE
            lckpt = 444
         endif
       enddo

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  800  continue
C      ... comes here to finish off ...
       if(lchkprntq) then
         write(6,805)lckpt
  805    format(' dcdlnx6b: arrived at 800 from lckpt=',I5)
       endif
       npels = ntotpels
       if(kbytpur .LE. 0) then
C        ... ERROR. Invalid value in kbytpur given in call dcdlnx6b
         iret_pel = 2
         lckpt = 811
         go to 999
       else
         if(LDUBL) then
           call mv8_8bit(c1mapwrk,IOTBY,nskpdib, c1pure,kbytpur,
     *                   nbytres, iret_mv8)

           if(lchkprntq) then
             write(6,815)iret_mv8, nbytres
  815        format(' dcdlnx6b::mv8_8bit: return code=',I4,
     *              '; nbytres=',I5)
           endif
         else

           call pk4_8bit(c1mapwrk,IOTBY,nskpdib, c1pure,kbytpur,
     *                   nbytres, iret_pk4)

           if(lchkprntq) then
             write(6,825)iret_pk4, nbytres
  825        format(' dcdlnx6b::pk4_8bit: return code=',I4,
     *              '; nbytres=',I5)
           endif
         endif

C        ... assume that scn_x6bA() has initialized c1pure to zeros,
C        ... so comment following out and check timing.
C        ... to fill the rest of scanline beyond nbytres with zeros,       
C ...         nbyfill = kbytpur - nbytres
C ...         if(nbyfill .GT. 0) then
C ...           do ib = nbytres+1,kbytpur
C ...             c1pure(ib) = NULL
C ...           enddo
C ...         endif

       endif

       go to 999
  999  continue
       return
       end       
c***********************************************************************
       subroutine egtnxtwd(line,lpstart,lpcurr,delims,ltr,word,ierror)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    EGTNXTWD    get next word
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 95-07-20
C
C ABSTRACT:  to get word from given line starting at line(lp:lp).
C
C PROGRAM HISTORY LOG:
C   90-03-23  shimomura  To get one word out of the given line, starting
C                        at line(lp:lp) ...
C                        (after eliminating leading blanks out of the 
c                        word.)
C   91-07-02  shimomura  copied char function getwordx to make it a 
c                        subroutine
c   91-12-05  shimomura  copied subr getwordy.for in order to make a 
c                        version that does not stop for a blank terminator.  
c                        You can activate the blank by including it in 
c                        delims.
c   95-06-19  shimomura  copied ~/tools/getwordnb.f (which was last compiled 
c                        on Dec 2 1993) in order to adapt to called from 
c                        prs_keqv() to NOT increment "lp", but return with 
c                        "lpcurr" pointing to last location interrogated;
C                        and to prepare it for CRAY
C   95-07-20  shimomura  Yet another version of gtnxtwrd() because I needed 
c                        to skip over trailing blanks until I found either 
c                        the non-blank terminator, or the last blank
C                        terminator before the start of the next word.
c   98-08-11  caruso     removed CDIR$ INTEGER=64 from top of this code.
c                        upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C
C USAGE:  CALL EGTNXTWD(line,lpstart,lpcurr,delims,ltr,word,ierror)
c
C   INPUT ARGUMENT LIST:
C     LINE     - given character string (NULL-terminated)
C                from which the next word is to be extracted.
C     lpstart  - integer. Line-Pointer from which to scan for word
C                User should initialize to =1, to start scan
C     DELIMS   - NULL-terminated char string of delimiters
C                which are in addition to the default
C                NEWLINE and NULL terminators.
c
c   output argument list
c     lpcurr   - integer.
c     ltr      - char*1
c     word     - char string
c     ierr     - return code
c
c   remarks:
C     egtnxtwd ... to get word from line at lp and increment lp
C       A version of getword that tests for additional delimiters
C       which are presented in the NULL-terminated string 
C       named "delims"; the basic version tests for NULL and
C       NEWLINE delimiters. 
C       Added a test for a special end-of-line config of ",-"
C       just before the NULL which terminates the line
C       which we are using as a continuation signal.
C     CAUTION: Usually you would want to first call with lp=1;
C                do not call with lp=0.
C     CAUTION: Given line must be NULL terminated to stop the scan.
C     CAUTION: You must allocate one more byte for your word than
C                the text you expect, since I will insert a
C                NULL terminator.
C
C ...       include       '/usr2/shimomur/tools/global.def'
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$
C
       logical        lcheckout
       parameter     (lcheckout=.FALSE.)

       character*(*)  line
       integer        lpstart
       integer        lpcurr
       character*(*)  delims
       character*1    ltr
       character*(*)  word
       integer        ierror
C
       integer        MXLNLP
       integer        MAXDELIM
       integer        wp
       integer        wp1
       integer        lenword

       integer        LBYLTR
       integer        llone

       character*1    lonech

       integer        lwherebl
       logical        found_term
       logical        found_nonblank
       character*1    NULL
       character*1    NEWLINE
       character*1    BLANK
C
       save
C
       ierror = 0
       NULL    = char(0)
       NEWLINE = char(10)
       BLANK   = char(32)
C
       lpcurr = lpstart
       if(lpcurr .le. 0) lpcurr = 1
C      ... which resets lp to first char position if we forgot to
C      ...   initialize it.
C
       ltr = NULL
       lenword = len(word)
       if(lenword .le. 0) go to 900
C      ... otherwise, results string "word" has a length ...
C      ... initalize results to NULL ...
       word = NULL
C
C      . . . .  To set maxdelim for limit of indexing thru delims . . . . 
       maxdelim = 0
       lendelims = len(delims)
       if(lendelims .le. 0) go to 910
C      ... otherwise, arg delims was defined as a char string ...
C      ... is it NULL terminated?
       lwhere0 = index(delims,NULL)
       if(lwhere0 .eq. 1) then
C        ... NULL terminator is in first char position, so nothing in
         maxdelim = 0
       
       else if(lwhere0 .le. 0) then
C        ... delims does not have a NULL terminator ...
C        ... so, try to go with length of delims ...
         maxdelim = lendelims
       else
C        ... the normal delimiters string, NULL terminated at lwhere0
         maxdelim = lwhere0 - 1
       endif
C
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . .   To set MXLNLP for limiting line scan   . . . . . . . .
C
       MXLNLP = 0
       lenline = len(line)
       if(lenline .le. 0) go to 920
C          ... line was not defined as a character string ...
C      ... otherwise, line has a length.  Is it NULL terminated?
       limlp = index(line(1:lenline),NULL)
       if(limlp .eq. 1) then
C        ... that means the NULL terminator is in first char position
C        ...   of the given line, so I can't do anything with this.
         go to 930
       else if(limlp .le. 0) then
C        ... there was no NULL terminator in the given line ...
C        ... try to substitute a limit as a function of string length
         MXLNLP = lenline
       else
C        ... the normal line string, NULL terminated at limlp
         MXLNLP = limlp - 1
       endif
C      ... leaves here with MXLNLP pointing to the last character
C      ...   to be examined in the given line.
       if(lpcurr .gt. MXLNLP) go to 940
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
       limcont = limlp - 2
C
       if(lcheckout) then
         WRITE(6,115)lpcurr,lenword
  115    FORMAT(' egtnxtwd:CALLED W/LP=',I5,'  WORD DIMENSIONED',I5)
       endif

       do while(line(lpcurr:lpcurr) .eq. BLANK)
C         ... to skip leading blanks
         lpcurr = lpcurr + 1
       enddo

       if(line(lpcurr:lpcurr) .eq. NEWLINE) then
         word = NEWLINE//NULL
         ltr = NEWLINE
         if(lcheckout) then
           WRITE(6,165)lpcurr
  165      FORMAT(' egtnxtwd:Before finding word, hit NEWLINE ',
     *            'at LP=', I4)
         endif
         go to 800
       endif
C      ... otherwise, we can begin scanning for next WORD ...
       wp = 1
C ...         do while(wp .ne. lenword .and. lpcurr .le. limlp)
       FOUND_TERM = .FALSE.
       DO WHILE (.NOT. FOUND_TERM) 
           IF(lpcurr .GT. MXLNLP) then
C            ... we have reached the end-of-given-line ...
C            ... simulate hitting a NULL terminator ...
             if(lcheckout) then
               write(6,225)lpcurr
  225          format(' egtnxtwd: reached end-of-line at LP=',I4,
     *                '; end like NULL')
             endif
 
             found_term = .true.
             ltr = NULL
             go to 244
           endif
C
           lonech = line(lpcurr:lpcurr)
C
           if(lonech .eq. NULL) then
C            ... THIS SHOULD NEVER COME THIS WAY, SINCE MXLNLP IS
C            ...    BEFORE THE NULL TERMINATOR
             found_term = .true.
             ltr = NULL
C ...           else if(lonech .eq. BLANK) then
C            ... this is usual way out on BLANK terminator found ...
C ...             found_term = .true.
C ...             ltr = BLANK
           else if(lonech .eq. NEWLINE) then
             found_term = .true.
             ltr = NEWLINE
           else if(lpcurr .eq. limcont
     *                   .and.
     *             lpcurr+1 .le. len(line)) then
             if (line(lpcurr:lpcurr+1) .eq. ',-') then
               found_term = .true.
               ltr = NEWLINE
             end if
           else
C            ... test lonech for match against delims ...
             if(maxdelim .gt. 0) then
               do  iter = 1,maxdelim
                 if(lonech .eq. delims(iter:iter)) then 
                   found_term = .true.
                   ltr = delims(iter:iter)
                   go to 244
C                  ... which jumped out of do upon delimiter_found
                 endif
               enddo
             endif
C            ... this lonech does not match any of delims ...
           endif
C
           if(.not. found_term) then
C            ... this lonech is not a delimiter, so stash it in word ...
C            ...   (unless results word space has been filled up, but
C            ...   we will throw away characters until 
C            ...   terminator is found in the line)
             IF(WP .LT. LENWORD) then
               WORD(WP:WP) = lonech
               wp = wp + 1
             endif
           endif
C            ... 
  244      continue
C          ... even for case of found_term, we will increment lp 
C          ...   so that lp is pointing at beyond the terminator
           lpcurr = lpcurr + 1
       enddo
C      ... comes here only after delimiter has been found ...
       if(lcheckout) then
         LBYLTR = MOVA2I(LTR)
         LPM1 = lpcurr - 1
         WRITE(6,246)LBYLTR,LPM1
  246    FORMAT(' egtnxtwd:found delimiter =HEX ',Z2.2,' AT LP=',I3)
       
         llone = mova2i(lonech)
         WRITE(6,255)lpcurr,wp,llone
  255    FORMAT(' egtnxtwd:fell thru enddo with lp=',I4,
     *          ' wp=',I4,' lonech=hex',Z2.2)
       endif

  300  continue
       word(wp:wp) = NULL
C      ... WHICH INSERTED A NULL TERMINATOR IN RESULTING WORD ...
       if(lcheckout) then
         IF(WP.GT.1) THEN
           WP1= WP - 1
           LBYLTR = MOVA2I(LTR)
           WRITE(6,345)WORD(1:WP1),LBYLTR
  345      FORMAT(' egtnxtwd:WORD="',A,'" ltr=hex ',Z2.2)
         ENDIF
       endif

       IF(LTR .NE. BLANK) GO TO 800
C        ... WHICH IS ALL FINISHED EXIT ...

C      ... OTHERWISE, THE TERMINATOR WAS BLANK, SO MOVE POINTER 
C      ...   TO A HIGHER-PRIORITY TERMINATOR, OR
C      ...   TO THE LAST OF A STRING OF BLANKS BEFORE THE START OF
C      ...   THE NEXT WORD
C      ================================================================
       FOUND_NONBLANK = .FALSE.
       DO WHILE (.NOT. FOUND_NONBLANK) 
           IF(lpcurr .GT. MXLNLP) then
C            ... we have reached the end-of-given-line ...
C            ... simulate hitting a NULL terminator ...
             if(lcheckout) then
               write(6,425)lpcurr
  425          format(1h ,'egtnxtwd+: reached end-of-line at LP=',I4,
     1                    '; end like NULL')
             endif
 
             FOUND_NONBLANK = .true.
             ltr = NULL
             go to 444
           endif
C
C          . . . . . . . . . . . . . . . . . .
C
           lonech = line(lpcurr:lpcurr)
C
           if(lonech .eq. NULL) then
C            ... THIS SHOULD NEVER COME THIS WAY, SINCE MXLNLP IS
C            ...    BEFORE THE NULL TERMINATOR
             FOUND_NONBLANK = .true.
             ltr = NULL

           else if(lonech .eq. NEWLINE) then
             FOUND_NONBLANK = .true.
             ltr = NEWLINE

           else if(lpcurr .eq. limcont .and.
     1             line(lpcurr:lpcurr+1) .eq. ',-') then
             FOUND_NONBLANK = .true.
             ltr = NEWLINE

           else
             if(lonech .eq. BLANK) then
C              ... this is part of trailing blanks which I want to 
C              ...   skip over
               go to 444
             else
C              ... this lonech is non-blank, so this lonech will 
C              ...    terminate scan one way or another.
               FOUND_NONBLANK = .true.
C              ... test this non-blank lonech for match against delims
               if(maxdelim .gt. 0) then
                 do  iter = 1,maxdelim
                   if(lonech .eq. delims(iter:iter)) then 
                     ltr = delims(iter:iter)
                     go to 444
C                    ... which jumped out upon non-blank-delimiter
C                    ...   found
                   endif
                 enddo
               endif
C              ... this non-blank lonech does not match any of delims
C              ... So this lonech is start of next word, 
C              ... jump out without incrementing lpcurr
               ltr = BLANK
               go to 445
             endif
           endif
C
  444      continue
C          ... even for case of FOUND_NONBLANK terminator, we will 
C          ... increment lpcurr; so that lpcurr is pointing at beyond 
C          ... the terminator
           lpcurr = lpcurr + 1
       enddo

  445  continue
C      ... comes here with lpcurr pointing beyond terminator ...
C      ... and ltr containing the terminator-character
       if(lcheckout) then
         LBYLTR = MOVA2I(LTR)
         LPM1 = lpcurr - 1
         WRITE(6,446)LBYLTR,LPM1
  446    FORMAT(' egtnxtwd+:found delimiter =HEX ',Z2.2,' AT LP=',
     *               I3)
       
         llone = mova2i(lonech)
         WRITE(6,455)lpcurr,wp,llone
  455    FORMAT(' egtnxtwd+:fell thru enddo with lpcurr=',I4,
     *          ' wp=',I4,' lonech=hex',Z2.2)
       endif
       go to 800
C      ================================================================

  800  CONTINUE
       go to 999
  900  continue
       ierror = 1
       go to 999
  910  continue
       ierror = 2
       go to 999
  920  continue
       ierror = 3
       go to 999
  930  continue
       ierror = 4
       go to 999
  940  continue
C      ... called with LP pointing beyond the last character data in LINE
       ierror = -1
       go to 999
C
  999  continue
       return
       end
c************************************************************************
       subroutine ekeyvals(maxnkeys,keyword,foundkey,numvalkey,iret_key)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C subprogram:    ekeyvals    fetch and parse PARM
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 95-06-20
C
C ABSTRACT: fetchs the PARM from the command_line; and
C           parses the PARM to seek matches for the given keywords
C           and returns the corresponding numeric value which follows
C           the equal sign of each keyword.
C
C PROGRAM HISTORY LOG:
C   95-06-20  SHIMOMURA
c   98-08-11  caruso     removed CDIR$ INTEGER=64 from top of this code.
c                        upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C
C USAGE:  call ekeyvals(maxnkeys,keyword,foundkey,numvalkey,iret_key)
c
C   INPUT ARGUMENT LIST:
c     maxnkeys  - integer max number of keys
c     keyword   - char*5 'ssno ' or 'dumy '
c
c   output argument list
c     foundkey  - logical.  true if keyword was found
c     numvalkey - integer.  subscript number(?)
c     iret_key  - return code
c
c   remarks:
C      ... A limited-use parser of PARM
C      ...    for use by the vxdump program
C
C      ... to fetch the PARM from the command_line; and
C      ... to parse the PARM to seek matches for the given keywords
C      ... and return the corresponding numeric value which follows
C      ... the equal sign of each keyword.
C      ... some grammar rules for text found in PARM:
C      ... (1.) keyword must be 5 characters or less;
C      ... (2.) keyword must be followed by an equal sign;
C      ... (3.) value is limited to a max of 5 digits;
C      ... (4.) each set of [keyword // '=' // digits] is terminated
C      ...         by a comma;
C
C      ... CALLS ON SUBROUTINES:  W3AS00(), egtnxtwd();
C                ON FUNCTIONS:    lowrcase(), LASTCH(), ATOI()
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$
C      . . . . . .  ekeyvals()  . . . . . . . . . . . . . . . . . . .
       integer        maxnkeys
       character*5    keyword(maxnkeys)
       logical        foundkey(maxnkeys)
       integer        numvalkey(maxnkeys)
       integer        iret_key
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       logical        checkout
       parameter     (checkout=.FALSE.)

C      . . . . . .   FUNCTIONS   . . . . . . . . . . . . . . . . . .

       external       lowrcase
       character*6    lowrcase

       external       lastch
       integer        lastch

       external       atoi
       integer        atoi
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       integer        lmtnkeys
       parameter     (lmtnkeys=12)

       integer        maxnwrd
       parameter     (maxnwrd=2*lmtnkeys)

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       integer        iptr_val(lmtnkeys)
       integer        nchkey(lmtnkeys)
       integer        nch_key

       character*6    cword(maxnwrd)
       character*1    cterm(maxnwrd)


C ...       call w3as00(nchparm,cparm,iret_as00) . . . . . .
       integer        nchparm
       character*101  cparm
       integer        iret_as00
C      . . . . . . . . . . . . . . . . . . . . . . . . . . .

       integer        lpstart
       integer        lpcurr
       character*5    delims
       character*1    ltr
C
       character*6    compar
       character*1    NULL
C
C      . . . . .   S T A R T   . . . . . . . . . . . . . . . . . . .
C   
       NULL = char(0)
       iret_key = 0
       if(maxnkeys .GT. lmtnkeys) then
         write(6,105)maxnkeys,lmtnkeys
  105    format(' ekeyvals: Failed on too-large maxnkeys=',I9
     *         /,'         when compiled limit in here =',I9)
         iret_key = 1
         go to 999
       endif


C      ... fetch cline via w3as00()
C
       call w3as00(nchparm,cparm,iret_as00)
       if(iret_as00 .ne. 0) then
         write(6,145)iret_as00
  145    format(' ekeyvals::w3as00: FAILED with code=',I5)
         iret_key = 2
         go to 999
       endif
C
C      ... then, break the parm apart into cword() and each
C      ...       corresponding terminator in cterm()
C      
       nwrdfound = 0
       delims(1:) = ';,= '//NULL
       lpstart = 1
       do  iwrd = 1,maxnwrd
          call egtnxtwd(cparm(1:nchparm),lpstart,lpcurr,delims,
     *                  ltr,cword(iwrd),ierror)
          if(ierror .ne. 0) then
            write(6,235)ierror
  235       format(' ekeyvals::egtnxtwd() yielded return code=',I5)
            go to 300
          endif
          nwrdfound = nwrdfound + 1
          cterm(nwrdfound) = ltr
          lpstart = lpcurr
       enddo

  300  continue
       write(6,315)nwrdfound
  315  format(' ekeyvals: At 300 w/ nwrdfound=',I3)
       if(nwrdfound .GT. 0) then
         do  iw = 1,nwrdfound
           ltermin = mova2i(cterm(iw))
           write(6,325)iw,cword(iw)(1:lastch(cword(iw))),ltermin
  325      format(1x,I3,'. "',A,'";  terminator= X',Z2.2)
         enddo
       endif
C    
C
C      ... now let's try to use the data, which is why I wanted this
C      ...    parsing capability
C      ... scan for like "head"
C
       lckpt = 400
       if(checkout) then
         write(6,405)LCKPT
       endif
  405  format(' ekeyvals: LCKPT=',I5)

       do  i = 1,maxnkeys
         iptr_val(i) = 0
         numvalkey(i) = 0
         foundkey(i) = .false.
         nchkey(i) = lastch(keyword(i))
       enddo

       lckpt = 410
       if(checkout) then
         write(6,405)LCKPT
       endif

       do  iw = 1,nwrdfound
         if(cterm(iw)(1:1) .eq. '=') then
           compar(1:6) = ' '
           nch_word = lastch(cword(iw))
           if(nch_word .GT. 0) then
             compar(1:) = lowrcase(cword(iw))
             do  ik = 1,maxnkeys
               nch_key = nchkey(ik)
               if(nch_word .GE. nch_key) then
C                ... we will compare only if comparand has enough ltrs
C                ... at least as many letters as the keyword ...
                 if(compar(1:nch_key) .EQ. keyword(ik)(1:nch_key)) then
C                  ... found the keyword(ik) at cword(iw) ...
                   if(iw .LT. nwrdfound) then
                     iptr_val(ik) = iw + 1
                   endif
                 endif
               endif
             enddo
           endif
         endif
       enddo
C
       lckpt = 420
       if(checkout) then
         write(6,405)LCKPT
       endif
C
       do  ik = 1,maxnkeys
         nch_key = nchkey(ik)
         if(iptr_val(ik) .NE. 0) then
           iofval = iptr_val(ik)
           nchval = lastch(cword(iofval))
           numvalkey(ik) = atoi(cword(iofval))
           foundkey(ik) = .TRUE.

           write(6,425)keyword(ik)(1:nch_key),cword(iofval)(1:nchval),
     *                 numvalkey(ik)
  425      format(' ekeyvals: "',A,'" = "',A,'" =',I9)
         else
C          ... failed to find a match for this keyword ...
           foundkey(ik) = .FALSE.
           if(checkout) then
             write(6,435)keyword(ik)(1:nch_key)
  435        format(' ekeyvals: did not find, in cparm, the ', 
     *              'keyword= "',A,'"')
           endif
         endif
       enddo
       
  999  continue
       return
       end  
c*******************************************************************
       SUBROUTINE FINDLINK(ISSNO, npartfound, iplotarg, IRET_FND)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C subprogram:    findlink       
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 97-07-08
C
C ABSTRACT: finds the isched linked components of the fax entity
c           which starts with subset no. = issno and returns the
c           descriptions of the components (up to 4) including
c           where to locate the start of the component in the
c           input file(s).
C
C PROGRAM HISTORY LOG:
c   96-07-03  shimomura  original author 
C   97-07-08  SHIMOMURA  iplotarg(14,JPAR) = JPRODNO
C                        (I think the 14th word is not used.)
c   98-08-11  caruso     upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C
C USAGE:  call FINDLINK(ISSNO, npartfound, iplotarg, IRET_FND)
c
C   INPUT ARGUMENT LIST:
c     issno      - integer subset no.
c
c   output argument list
c     npartfound - integer number of components found
c     iplotarg   - integer array.  holds plotting info.
c     iret_fnd   - return code
c
c   remarks:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$

       INTEGER    MAXNFILS
       PARAMETER (MAXNFILS=4)

       INTEGER    MAXNPRODS
       PARAMETER (MAXNPRODS=20)   	!... MAX PRODS PER FILE

       INTEGER    LMTPRODTOT
       PARAMETER (LMTPRODTOT=MAXNFILS*MAXNPRODS)   	!... 80 = 4*20

       INTEGER    MAXITMDBA
       PARAMETER (MAXITMDBA=10)

       COMMON  /PRODDESC/numprods,IFID(6,LMTPRODTOT),
     1                            ISCHED(180,LMTPRODTOT),
     2                            IPROD_DBA(MAXITMDBA,LMTPRODTOT)

C      ==============================================================
       INTEGER    MXIIPITEM   	!... EACH SCHED ITEM HAS 8 I*2 WORDS
       PARAMETER (MXIIPITEM=8)

       INTEGER    MXSCH       	!... MAX NO. SCHED ITEMS PERMITTED/BLOCK
       PARAMETER (MXSCH=61)

       INTEGER      MAXPLTARG   	!... I-DIMEN OF IPLOTARG(I,4)
       PARAMETER   (MAXPLTARG=20)

       INTEGER      IIMAXPART
       PARAMETER   (IIMAXPART=10)

       INTEGER      MAXNPART
       PARAMETER   (MAXNPART=4)

       integer      isch4part(IIMAXPART,MAXNPART)

C      --------------------------------------------
       INTEGER      ISSNO
       integer      npartfound
       integer      iplotarg(MAXPLTARG,MAXNPART)
       INTEGER      IRET_FND
C      --------------------------------------------

       INTEGER      ITHIS_SSNO
       INTEGER      NEXT_SSNO
       INTEGER      ITEMWRD(2)

       INTEGER      NOFFSET
       DATA         NOFFSET    /  0 /
       INTEGER      KBITPGRP
       DATA         KBITPGRP   / 16 /
       INTEGER      KPADBITS
       DATA         KPADBITS   /  0 /
       INTEGER      NGRPS2DO
       DATA         NGRPS2DO   /  8 /
       INTEGER      NEGSIGNI2
       DATA         NEGSIGNI2    / X'FFFFFFFFFFFF8000' /
       INTEGER      NEGSIGNBYT
       DATA         NEGSIGNBYT   / X'FFFFFFFFFFFFFF80' /
       INTEGER      MSKLOW16
       DATA         MSKLOW16             / X'0000FFFF' /
       INTEGER      MSKLOWBYT
       DATA         MSKLOWBYT                / X'00FF' /

       integer      issno_seek
       INTEGER      ITEM_COUNT
       INTEGER      IIARRAY(8)
       INTEGER       J0VAL_DST
       INTEGER       INDENT_DST
       INTEGER       INDENT_SRC
       INTEGER       IWIDTH
       INTEGER       IHGT
       logical       lsubsetq
       LOGICAL       LOOPAGAIN
       LOGICAL       LDOUBLEQQ
       LOGICAL       LFRAMEQQ

       CHARACTER*7   CXXXSET

       SAVE

       IRET_FND = 0
       LOOPAGAIN = .FALSE.
       npartfound = 0
       do  j = 1,MAXNPART    		!... 4 part max of a FAX entity
         do  ii = 1,IIMAXPART
           isch4part(ii,j) = 0
         enddo
       enddo

       DO  J = 1,MAXNPART 		!... 4 PARTS IS MAX
         DO  I = 1,MAXPLTARG
           IPLOTARG(I,J) = 0
         ENDDO
       ENDDO

       write(6,105)issno
  105  format(' findlink: was called to look in sched(s) for SSNO=',
     *             I6)
       issno_seek = issno

       write(6,107)
  107  format('   SSNO/                     FLAG/  WIDTHS/      ',
     *        '   NEXT',
     *      /,'  INSNO   BLOK   BYTE  LINES INDTHS INDNXT  J0NXT',
     *        '  INSNO')

       LOOPAGAIN = .FALSE.
  222  CONTINUE   		!... LOOPS BACK TO HERE TO SCAN AGAIN
       DO 377 JPR = 1,NUMPRODS
C        ... SCAN ISCHED(1,JPR) LOOKING FOR SUBSET NO. == ISSNO

         ITEM_COUNT = 0
         DO  NSCH = 2,MXSCH
           ITEMPTR = 2*(NSCH-1) + 1
           ITEMWRD(1) = ISCHED(ITEMPTR,JPR)
           ITEMWRD(2) = ISCHED(ITEMPTR+1,JPR)
           IF(IOR(ITEMWRD(1),ITEMWRD(2)) .EQ. 0) THEN
C             ... END OF SCHED OF THIS JPR BLOCK ...
              WRITE(6,225)ITEM_COUNT,JPR
  225         FORMAT(' FINDLINK: FOUND TERMINATING ZERO AFTER ',
     1               'LOOKING AT ITEM-COUNT=',I4,
     2             /,'    FROM PRODUCT NO.',I3)
              GO TO 377  	!... LOOP TO NEXT PRODUCT
           ENDIF
C          ... otherwise, this sched item is non-zero, so ...
           item_count = item_count + 1

           call gbytes(itemwrd,iiarray,NOFFSET,KBITPGRP,KPADBITS,
     1                 NGRPS2DO)

           IF(BTEST(IIARRAY(7),15)) THEN    	!... IF(HI-ORDER BITON)
             IIARRAY(7) = IOR(IIARRAY(7),NEGSIGNI2)
           ENDIF
          
           WRITE(6,235)(IIARRAY(I),I=1,8)
  235      FORMAT(1x,4I7,' X:',Z4.4,' X:',Z4.4,2I7)

           ITHIS_SSNO = iiarray(1)
C          ... (the following match test failed to find a match, ...
C          ...    until I SAVEd everything ...)
           IF(ITHIS_SSNO .EQ. ISSNO_seek) THEN
C            ... I HAVE FOUND THE SCHED ITEM FOR ISSNO.
             npartfound = npartfound + 1

C            ... SO save THIS found component of the FAX entity
             do  ii = 1,8
               isch4part(ii,npartfound) = iiarray(ii)
             enddo
             isch4part( 9,npartfound) = NSCH
             isch4part(10,npartfound) = JPR

             NEXT_SSNO = iiarray(8)
             write(6,245)NSCH,JPR
  245        format(' findlink: found matching SSNO at NSCH=',I4,
     *              ';  in Product No.=',I3)
             if(NEXT_SSNO .LE. 0) then
               write(6,247)
  247          format('     which has no further link')
               go to 400  	!... ONLY NORMAL EXIT OUT OF LOOP 377

             else
               write(6,249)NEXT_SSNO
  249          format('     which links to NEXT_SSNO=',I6)
               issno_seek = next_ssno
               LOOPAGAIN = .FALSE.
             ENDIF
           ENDIF
         ENDDO
  377  CONTINUE
C      ... WHICH IS ENDDO BY JPR PRODUCT ...
C      ... WHICH MEANS: FAILED TO FIND A MATCH FOR ISSNO_SEEK
       IF(LOOPAGAIN) THEN
C        ... ERROR STOP ... CANNOT FIND ONE OF THE COMPONENTS ...
         WRITE(6,379)ISSNO,ISSNO_SEEK
  379    FORMAT(' FINDLINK: FAILED TO FIND ONE COMPONENT OF ',
     1          'THIS FAX ENTITY ...',
     2         /,7X,'          WHICH STARTED WITH SSNO.=',I6,
     3         /,7X,'FAILED TO FIND SUBSET OR INSET NO.=',I6)
         IRET_FND = 1
         GO TO 999
       ELSE
C        ... FIRST HIT END OF LOOP, SO GO BACK TO THE BEGINNING 
C        ...    AND SEARCH THRU ENTIRE SET ...
         LOOPAGAIN = .TRUE.
         GO TO 222
       ENDIF
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
 
  400  continue
C      ... summarize the components of the FAX entity ...
       write(6,405)ISSNO,npartfound
  405  format(' findlink: For the FAX composite chart which begins',
     1        ' with Subset No.',I6,
     2       /,5X,'the number of components found=',I3)

       do jpar = 1,npartfound
         JPRODNO = isch4part(10,jpar)

         if(BTEST(isch4part(5,jpar),15)) then
           lsubsetq = .TRUE.
           cxxxset(1:7) = 'SUBSET '
         else
           lsubsetq = .FALSE.
           cxxxset(1:7) = ' INSET '
         endif

         LDOUBLEQQ = .FALSE.
         LFRAMEQQ = .FALSE.
         if(lsubsetq)then
           LDOUBLEQQ = BTEST(ISCH4PART(5,JPAR),9)
         else
C          ... this is an inset; .not. subset;
           LDOUBLEQQ = BTEST(ISCH4PART(5,JPAR),10)
           LFRAMEQQ = BTEST(ISCH4PART(5,JPAR),14)
         endif

         IF(jpar .LE. 1) THEN
           J0VAL_DST   = 0
           INDENT_DST  = 0
         ELSE
           J0VAL_DST   = ISCH4PART(7,JPAR-1)
           INDENT_DST  = IAND(ISCH4PART(6,JPAR-1),MSKLOWBYT)
           INDENT_DST  = 8*INDENT_DST
         ENDIF
         IWIDTH = IAND(ISHFT(ISCH4PART(6,JPAR),-8),MSKLOWBYT)
         IWIDTH = 8*IWIDTH
         IHGT = ISCH4PART(4,JPAR)

         INDENT_SRC  = IAND(ISCH4PART(5,JPAR),MSKLOWBYT)
         IF(BTEST(INDENT_SRC,7)) THEN
           INDENT_SRC = IOR(INDENT_SRC,NEGSIGNBYT)
         ENDIF
         INDENT_SRC = 8*INDENT_SRC

         write(6,425)CXXXSET(1:7),isch4part(1,jpar),J0VAL_DST,
     1               INDENT_DST,IWIDTH,IHGT,IPROD_DBA(1,JPRODNO),
     2               IPROD_DBA(3,JPRODNO),ISCH4PART(2,JPAR),
     3               ISCH4PART(3,JPAR)
  425    FORMAT(/1x,A7,'NO.',I6,'; TO BE PLOTTED AT J=',I6,
     1              '; INDENT_DST=',I6,
     2         /,7X,'WIDTH,HGT= (',I6,',',I6,')',
     3         /,7X,'TO BE FETCHED FROM U:',I2,
     4                 '   PROD-START REC=',I6,
     5         /,7X,'FROM WHICH POINT NREC-SKIP=',I6,
     6                 ';  NBYT-SKIP=',I6)
C      ... IPLOTARG(1,) = 1; IF SUBSET
C                       = 0; IF  INSET
C                  (2,) = 1288; the SUBSET OR INSET NUMBER
         if(lsubsetq) then
           iplotarg(1,JPAR) = 1
         else
           iplotarg(1,JPAR) = 0
         endif

         iplotarg(2,JPAR) = isch4part(1,JPAR)  		!... ssno
         iplotarg(3,JPAR) = J0VAL_DST
         iplotarg(4,JPAR) = INDENT_DST
         iplotarg(5,JPAR) = IWIDTH
         iplotarg(6,JPAR) = IHGT
         iplotarg(7,JPAR) = IPROD_DBA(1,JPRODNO) 	!... U:UNIT
         iplotarg(8,JPAR) = IPROD_DBA(3,JPRODNO)  	!... FFFFFF-REC
         iplotarg(9,JPAR) = ISCH4PART(2,JPAR)   	!... REC-SKIP
         iplotarg(10,JPAR) = ISCH4PART(3,JPAR)  	!... BYT-SKIP

         iplotarg(11,JPAR) = INDENT_SRC

         iplotarg(12,JPAR) = 0
         IF(LFRAMEQQ) THEN
           iplotarg(12,JPAR) = 1
         ENDIF

         iplotarg(13,JPAR) = 0
         IF(LDOUBLEQQ) THEN
           iplotarg(13,JPAR) = 1
         ENDIF
         
         iplotarg(14,JPAR) = JPRODNO		!... 8-Jul-1997/dss
       enddo

  999  continue
       RETURN
       END
c************************************************************************
       LOGICAL FUNCTION ISALPHA(C)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C FUNCTION:      isalpha     see if character is alpha or numeric
C   PRGMMR: CARUSO           ORG: W/NP12      DATE: 98-08-11
C
C ABSTRACT: .TRUE. IF THE ONE CHARACTER IS ANY LETTER OR DIGIT ...
C
C PROGRAM HISTORY LOG:
c   yy-mm-dd  original author unknown
c   98-08-11  caruso     upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C
C USAGE:    if(isalpha(c)) then...
c
C   INPUT ARGUMENT LIST:
c     c    -  character*1
c
c   output argument list:
c     true if character is a letter or number,
c     false if not.
c
c   remarks:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$
       CHARACTER*1 C
       save
       ISALPHA = ((LGE(C,'0') .AND. LLE(C,'9'))
     1       .OR. (LGE(C,'a') .AND. LLE(C,'z'))
     2       .OR. (LGE(C,'A') .AND. LLE(C,'Z')))
       RETURN
       END
c************************************************************************
       CHARACTER*(*) FUNCTION LOWRCASE(STRING)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C FUNCTION:      lowrcase        convert char string to lowercase
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 95-07-10
C
C ABSTRACT: CONVERT A CHARACTER STRING OR TO LOWRCASE 
c           EXCEPT WHERE ENCLOSED IN QUOTES; terminate scan on first
c           BLANK or NULLCHR or on LEN(STRING) 
c           or on LEN(LOWRCASE); whichever is encountered first
C
C PROGRAM HISTORY LOG:
c   yy-mm-dd  original author unknown
c   98-08-11  caruso     removed CDIR$ INTEGER=64 from top of this code.
c                        upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C
C USAGE:  cret = lowrcase(string)
c
C   INPUT ARGUMENT LIST:
c     string  -  char string to be converted to all lowercase
c
c   output argument list:
c     cret    -  char string in lowercase.
c
c   remarks:
c
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$

       character*(*) string
       external      lastch
       INTEGER       lastch      		!...declares function

       integer       i
       INTEGER       LIMITIN
       INTEGER       LIMITOUT
       CHARACTER*1   NULLCHR
       CHARACTER*1   QUOTE

       save

       NULLCHR = CHAR(0)
       QUOTE = CHAR(39)

       i = 1
       LIMITOUT = LEN(LOWRCASE)
       LIMITIN = lastch(STRING)
       IF(LIMITIN .GT. LIMITOUT) THEN
         LIMITIN = LIMITOUT
       ENDIF
       IF(LIMITIN .LE. 0) GO TO 555
       do while (i .le. LIMITIN)
          if (string(i:i) .eq. QUOTE) then
             LOWRCASE(i:i) = QUOTE
             i = i + 1
             do while (string(i:i) .ne. QUOTE .and. i .le. LIMITIN)
                LOWRCASE(i:i) = string(i:i)
                i = i + 1
             enddo
             LOWRCASE(i:i) = string(i:i)
             if(string(i:i) .ne. NULLCHR) i = i + 1
          else
             if(llt(string(i:i),'A') .or. lgt(string(i:i),'Z')) then
                LOWRCASE(i:i) = string(i:i)
             else         ! this is a upper-case letter, so lower it ...
                LOWRCASE(i:i) = char(mova2i('a') - mova2i('A') +
     1                            mova2i(string(i:i)))
             endif
             i = i + 1
          endif
       enddo
  555  CONTINUE
       IF(I .LE. LIMITOUT) THEN
         LOWRCASE(i:i) = NULLCHR
       ENDIF
       return
       end
c****************************************************************
       subroutine mkpurhdr(c8mapnam,NWDOUT,NPELS,ched,kdimhed,
     1                     nwdhed,iret_hdr)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C subprogram:    mkpurhdr    make header for pure raster file
C   PRGMMR: caruso           ORG: W/NMC41    DATE: 98-08-11
C
C ABSTRACT:  composes an initial header for the pure-raster file
C            within the x6b_pur() package;
C            the header follows the format used by ty9_pur()
C
C PROGRAM HISTORY LOG:
C   95-08-28  SHIMOMURA
c   98-08-11  caruso     upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.  removed call to s/r gmtdatim,
c                        which wasn't y2k-compliant, and replaced it with
c                        call to w3utcdat.
C
C USAGE:  call mkpurhdr(c8mapnam,NWDOUT,NPELS,ched,kdimhed,
c                          nwdhed,iret_hdr)
c
C   INPUT ARGUMENT LIST:
c     c8mapnam   - char*8 map name
c     nwdout     - integer number of words to output
c     npels      - integer number of pixels to output
c
c   output argument list
c     ched       - char*8 header array
c     kdimhed    - integer number of array elements in ched
c     nwdhed     - integer number of words in header
c     iret_hdr   - return code
c
c   remarks:
C     This only composes; does not output the header;
C     Only initializes; still lacks a few items which
C     some other module fills in at completion of product.
C     Calls int function lastch() and w3utcdat (system date/time
c     in GMT).
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$
      external     lastch
      integer      lastch

      character*8  c8mapnam
      INTEGER      NWDOUT
      INTEGER      NPELS
      CHARACTER*8  CHED(kdimhed)
      integer      iret_hdr

      integer      idat(8)
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
C       ... FOLG FOR BIN2CH() ARGS:
      INTEGER       KCHBIN
      DATA          KCHBIN  / 8 /

      CHARACTER*4   CPLMI
      DATA          CPLMI / 'R999' /

      INTEGER       IACC
      CHARACTER*8   CACC
      EQUIVALENCE  (IACC,CACC)

      integer        maxwrdhed
      data           maxwrdhed  / 8 /

      save

        iret_hdr = 0
        nwdhed = 0
        if(kdimhed .LT. maxwrdhed) then
C         ... error:  she didn't allocate enough space in destination
          write(6,905)kdimhed,maxwrdhed
  905     format(' mkpurhdr: Failed due to insufficient space ',
     1           ' for resulting header in ched()',
     2          /,7X,'kdimhed=',I3,';  but needed ',I3)

          iret_hdr = -1
          go to 999

        endif
C       ... otherwise, CHED() has enough array to hold the items
        lenched = len(ched(1))
        if(lenched .NE. 8) then
C          ... error.  Destination must be set for char strings of 8
C          ...           characters per item.
           write(6,145)
  145      format(' mkpurhdr: Failed on improperly defined ',
     1            'destination array for header;',
     2           /,7X,'I expected CHARACTER*8 CHED(8)')
           iret_hdr = -2
           go to 999
         endif

          CHED(1)(1:8) = 'PURERAST'
          CHED(2)(1:8) = '00000000'
          CHED(3)(1:8) = '00000000'
C ...          CHED(4)(1:8) = 'NH2007  '
          ched(4)(1:8) = ' '
          nch = lastch(c8mapnam(1:8))
          if(nch .LE. 0) then
            ched(4)(1:8) = 'UNKNOWN '
            iret_hdr = 1
          ELSE
            IF(NCH .GT. 8) THEN
              NCH = 8
              iret_hdr = 2
            ENDIF

            ched(4)(1:nch) = c8mapnam(1:nch)
          ENDIF        
 
          CHED(5)(1:8) = '00000000'
        
          CALL BIN2CH(NWDOUT,CACC(1:8),KCHBIN,CPLMI(1:4))
          CHED(6)(1:8) = CACC(1:8)

C ...          NPELS = NBITSPWD*NWDOUT
          CALL BIN2CH(NPELS,CACC(1:8),KCHBIN,CPLMI(1:4))
          CHED(7)(1:8) = CACC(1:8)

          call w3utcdat(idat)
          iyr2d = mod(idat(1),100)
          imon  = idat(2)
          iday  = idat(3) 
          if(iyr2d.lt.1) then
            ched(8)(1:2) = '00'
          elseif(iyr2d.lt.10) then
            ched(8)(1:1) = '0'
            write(ched(8)(2:2),fmt='(i1)') iyr2d         
          else
            write(ched(8)(1:2),fmt='(i2)') iyr2d 
          endif
          if(imon.lt.10) then
            ched(8)(3:3) = '0'
            write(ched(8)(4:4),fmt='(i1)') imon         
          else       
            write(ched(8)(3:4),fmt='(i2)') imon         
          endif
          if(iday.lt.10) then
            ched(8)(5:5) = '0'
            write(ched(8)(6:6),fmt='(i1)') iday         
          else       
            write(ched(8)(5:6),fmt='(i2)') iday         
          endif
          ched(8)(7:8) = '  '    

c          CALL GMTDATIM(CYYMMDD,CZZZZ)
c          CHED(8)(1:8) = CYYMMDD(1:6)//'  '
           
          nwdhed = 8
          DO  JJ = 1,NWDHED
            WRITE(6,374)JJ,ched(JJ)(1:8)
  374       FORMAT(' mkpurhdr:CHED(',I3,') = "',A,'"')
          ENDDO

  999     continue
          return
          end
c********************************************************************
       subroutine mv8_8bit (c1sorc,nbytin, nskpdib, c1dest,kbytout,
     1                     nbytres,iret_mv8)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C subprogram:    mv8_8bit    move 8 bits into byte
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 95-07-10
C
C ABSTRACT:  To move bit into bytes, after considering array sizes
C            and skip value; 
C
C PROGRAM HISTORY LOG:
c   95-08-21  shimomura
c   97-02-20  shimomura  Copied from ~/cra/bg/mv8_8bit.f 
C                        into ~/ncod/chk/mv8_8bit.f because when 
c                        referenced by dcdlnx6b() within fxcompos
C                        under the double-the-scale option, this 
c                        returned with nothing.
C                        So I needed to add comments.
c   98-08-11  caruso     removed CDIR$ INTEGER=64 from top of this code.
c                        upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C
C USAGE:  call mv8_8bit (c1sorc,nbytin, nskpdib, c1dest,kbytout,
c                          nbytres,iret_mv8)
c
C   INPUT ARGUMENT LIST:
C     c1sorc   -  char*1 array of size nbytin. 8-bits data per byte
c     nbytin   -  integer size of array c1sorc
C     nskpdib  -  number of c1sorc bytes (each containing
C                 a doubled-dibyte of data) to skip before stashing any;
c
c   output argument list
C     c1dest   -  char*1 array of size nbytres. 8 bits of data per byte
c     kbytout  -  integer size of array c1dest
c     nbytres  -  integer.  same as kbytout!
C     iret_mv8 -  return code
C                 =  0;  normal exit.
C                 =  1;  Fatal Error,  Recompile! 
C                    You gave me no space in destination 
C                    array.
C                 = -1;  Warning: the skip constant moved ptr 
C                    beyond any existing data in line
C                 = -2;  Warning: the space in destination 
C                    array is inadequate for the given 
C                    data.  Truncate and go, anyway.
c
c   remarks:
C      ... This will work with either INTEGER length.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$
C
       character*1   c1sorc(nbytin)
       integer       nskpdib
       character*1   c1dest(kbytout)
       integer       nbytres
       integer       iret_mv8
C      . . . . . . . . . . . . . . . . . .
 
       integer       M1
       integer       M2
       integer       ndibstodo
       integer       idemax

       SAVE
C      . . . . .   S T A R T   . . . . . . . . . . . . . . . . . . .

       iret_mv8 = 0
       nbytres = 0
       ide = 0

       M1 = 1
       if(nskpdib .GT. 0) then
         M1 = nskpdib + 1
       endif

       M2 = nbytin
       if(M2 .LE. 0) then
         nbytres = 0
         go to 999
       endif
       if(M1 .GT. M2) then
         iret_mv8 = -1
C        ... WARNING:the skip constant moved ptr beyond any existing 
C        ...         data in line
         nbytres = 0
         go to 999
       endif
C
C      ... otherwise, (M1 .LE. M2), so go on ...
C      ... 
       ndibstodo = M2 - M1 + 1
       idemax = ndibstodo
 
C      ... Did she allocate enough space for me in C1DEST(kbytout) ???
       if(kbytout .LE. 0) then
         iret_mv8 = 1
C        ... Fatal Error, you gave me no space in destination array.
         nbytres = 0
         go to 999
       else if(kbytout .LT. idemax) then
         iret_mv8 = -2
C        ... Warning, the space for destination array is inadequate
C        ...          for the given data.  Truncate and go, anyway.
         ndibstodo = kbytout
       endif
C      ... 
       M2 = M1 + ndibstodo - 1
       idemax = ndibstodo 

       ide = 0
       IF(M2 .GT. 0) THEN
         do ic = M1,M2
           ide = ide + 1
           if(ide .GT. idemax) go to 366
           c1dest(ide) = c1sorc(ic)
         enddo
       ENDIF
  366  continue
       
       nbytres = ide
C
  999  continue
       return
       end
c************************************************************************
       subroutine pk4_8bit (c1sorc,nbytin, nskpdib, c1dest,kbytout,
     1                     nbytres,iret_pk4)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C subprogram:    pk4_8bit    half-pack into bytes
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 95-07-10
C
C ABSTRACT:  To half-pack into bytes 
C
C PROGRAM HISTORY LOG:
C   95-08-21  SHIMOMURA
c   98-08-11  caruso     upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C
C USAGE:  call pk4_8bit (c1sorc,nbytin, nskpdib, c1dest,kbytout,
c                        nbytres,iret_pk4)
c
C   INPUT ARGUMENT LIST:
C     c1sorc   -  char*1 array of size nbytin. low order 4-bits per byte
c     nbytin   -  integer size of array c1sorc
C     nskpdib  -  number of c1sorc bytes (each containing
C                 a dibyte of data) to skip before stashing any;
c
c   output argument list
C     c1dest   -  char*1 array of size nbytres. 4+4 bits per byte
c     kbytout  -  integer size of array c1dest
c     nbytres  -  integer.  same as kbytout!
C     iret_pk4 -  return code
C                 =  0;  normal exit.
C                 =  1;  Fatal Error,  Recompile! 
C                    You gave me no space in destination 
C                    array.
C                 = -1;  Warning: the skip constant moved ptr 
C                    beyond any existing data in line
C                 = -2;  Warning: the space in destination 
C                    array is inadequate for the given 
C                    data.  Truncate and go, anyway.
c
c   remarks:
C     This will work with either INTEGER length.
C     This is slow and safe version.  I could make it go faster
C     if I could assume given integer arrays; given zero filled;
C     no bounds checking; etc.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$
C
       character*1   c1sorc(nbytin)
       integer       nskpdib
       character*1   c1dest(kbytout)
       integer       nbytres
       integer       iret_pk4
C      . . . . . . . . . . . . . . . . . .
 
       integer       M1
       integer       M2
       integer       ndibstodo
       integer       idemax
       integer       iacc
       integer       mq
       integer       msk4bit
       data          msk4bit    / X'0F' /
       LOGICAL       L1MORE

C      . . . . .   S T A R T   . . . . . . . . . . . . . . . . . . .

       iret_pk4 = 0
       nbytres = 0
       ide = 0

       M1 = 1
       if(nskpdib .GT. 0) then
         M1 = nskpdib + 1
       endif

       M2 = nbytin
       if(M2 .LE. 0) then
         nbytres = 0
         go to 999
       endif
       if(M1 .GT. M2) then
         iret_pk4 = -1
C        ... WARNING:the skip constant moved ptr beyond any existing 
C        ...         data in line
         nbytres = 0
         go to 999
       endif
C
C      ... otherwise, (M1 .LE. M2), so go on ...
C      ... 
       ndibstodo = M2 - M1 + 1
       idemax = ndibstodo / 2
       if(mod(ndibstodo,2) .NE. 0) then
C         ... is odd-number of given bytes ...
          idemax = idemax + 1
C         ... where idemax is required space in c1dest ...
       endif
C      ... Did she allocate enough space for me in C1DEST(kbytout) ???
       if(kbytout .LE. 0) then
         iret_pk4 = 1
C        ... Fatal Error, you gave me no space in destination array.
         nbytres = 0
         go to 999
       else if(kbytout .LT. idemax) then
         iret_pk4 = -2
C        ... Warning, the space for destination array is inadequate
C        ...          for the given data.  Truncate and go, anyway.
         ndibstodo = 2 * kbytout
       endif
C      ... 
       L1MORE = .FALSE.
       M2 = M1 + ndibstodo - 1
       idemax = ndibstodo / 2
       if(mod(ndibstodo,2) .NE. 0) then
C         ... is odd-number of given bytes ...
          idemax = idemax + 1
C         ... where idemax is required space in c1dest ...
C         ... set to do one-less byte in big loop; to do the pairs first
          L1MORE = .TRUE.
          M2 = M2 - 1
       endif

       ide = 0
       IF(M2 .GT. 0) THEN
         do ic = M1,M2,2
           iacc = mova2i(c1sorc(ic))
           iacc = ishft(iand(iacc,msk4bit),4)
           mq  = mova2i(c1sorc(ic+1)) 
           iacc = ior(iacc,iand(mq,msk4bit))
           ide = ide + 1
           c1dest(ide) = char(iacc)
         enddo
       ENDIF
       
       IF(L1MORE) THEN
         IC = M2 + 1
           iacc = mova2i(c1sorc(ic))
           iacc = ishft(iand(iacc,msk4bit),4)
           ide = ide + 1
           c1dest(ide) = char(iacc)
       ENDIF
       nbytres = ide
C
  999  continue
       return
       end
c**********************************************************************
       SUBROUTINE PLOTPART(JPART,npartfound,IPLOTARG,IRET_PLP)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C subprogram:    plotpart    plot part of the fax chart
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 95-07-10
C
C ABSTRACT:  CALLED TO PLOT ONE COMPONENT OF THE COMPOSITE FAX CHART
C            BY FETCHING FROM THE 6-BIT PACKED INPUT FILE
C            DECODING; CLIPPING; PUTTING PURE-RASTERS INTO
C            BITPLANE WORKSPACE.
C
C PROGRAM HISTORY LOG:
C   96-08-19  SHIMOMURA
c   98-08-11  caruso     removed CDIR$ INTEGER=64 from top of this code.
c                        upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C
C USAGE:  call PLOTPART(JPART,npartfound,IPLOTARG,IRET_PLP)
c
C   INPUT ARGUMENT LIST:
c     jpart      - integer number of component to be plotted
c     npartfound - integer total number of components
c     iplotarg   - integer array of plotting info. 
c
c   output argument list
c     iret_plp   - return code
c
c   remarks:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$
c
       external  rd1byt99
       integer   rd1byt99
       external  reposi99
       integer   reposi99
       external  rewind99
       integer   rewind99
       external  wheren99
       integer   wheren99

       integer      kbytpwrd
       parameter   (kbytpwrd=8)     	!... CRAY version
       integer   krecsizbyt
       parameter   (krecsizbyt=1440)
       integer      krecsizwrd
       parameter   (krecsizwrd=krecsizbyt/kbytpwrd)
       integer   nbytsred
C      --------------------------------------------
C      . . .   CALL SEQ FOR SUBR PLOTPART() . . . .
       INTEGER      MAXPLTARG   	!... I-DIMEN OF IPLOTARG(I,4)
       PARAMETER   (MAXPLTARG=20)

       INTEGER      MAXNPART
       PARAMETER   (MAXNPART=4)

       INTEGER      JPART
       integer      npartfound
       integer      iplotarg(MAXPLTARG,MAXNPART)
       INTEGER      IRET_PLP

C      ... IPLOTARG(1,) = 1; IF SUBSET
C                       = 0; IF  INSET
C                  (2,) = 1288; the SUBSET OR INSET NUMBER
C      --------------------------------------------

       integer        intarray(krecsizwrd)
       character*1    c1array(krecsizbyt)
       equivalence   (intarray,c1array)

       integer   iret_rew
       integer   iret_pos
       integer   iret_rds
       integer   inrelrecno
       integer   numrec
       integer   nbufpart
       integer   icountEOL
       integer   nbyskip
       integer      lintot
       LOGICAL      LCHKPRT
       LOGICAL      LFIRSTQ

       INTEGER      ISSNO
       INTEGER      J0VAL_DST
       INTEGER      INDENT_DST
       INTEGER      IWIDTH
       INTEGER      IHGT
       integer      LUNINP
       INTEGER      NREC_STARTOMAP
       INTEGER      NREC_SKIP
       INTEGER      NBYT_SKIP
       INTEGER      INDENT_SRC
       LOGICAL      LSUBSETQ
       LOGICAL      LFRAMEQQ
       LOGICAL      LDOUBLEQQ
       CHARACTER*7   CXXXSET

       integer   byte_offset
       integer   from_indx
       data         from_indx     / 0 /   	!... from begin of file

       SAVE

       IRET_PLP = 0
       IF(JPART .LE. 0 .OR. JPART .GT. MAXNPART) THEN
         WRITE(6,FMT='('' plotpart: FAILED ON BAD-VALUED JPART='',
     *               I8)') JPART
         IRET_PLP = 1
         GO TO 999
       ENDIF

       IF(JPART .GT. NPARTFOUND) THEN
         WRITE(6,FMT='('' plotpart: FAILED ON BAD-VALUED JPART='',
     *      I8,/,7X,''or BAD-VALUED TOTAL PARTS FOUND='',I8)')
     *           JPART,NPARTFOUND
         IRET_PLP = 2
         GO TO 999
       ENDIF

         IF(iplotarg(1,JPART) .EQ. 0) THEN
           LSUBSETQ = .FALSE.  		!... IS INSET; NOT SUBSET
           cxxxset(1:7) = ' INSET '
         ELSE
           LSUBSETQ = .TRUE.
           cxxxset(1:7) = 'SUBSET '
         endif

         ISSNO      = iplotarg(2,JPART)
         J0VAL_DST  = iplotarg(3,JPART)
         INDENT_DST = iplotarg(4,JPART)
         IWIDTH     = iplotarg(5,JPART)
         IHGT       = iplotarg(6,JPART)
         LUNINP     = iplotarg(7,JPART)
         NREC_STARTOMAP = iplotarg(8,JPART)
         NREC_SKIP  = iplotarg(9,JPART)
         NBYT_SKIP  = iplotarg(10,JPART)

         INDENT_SRC = iplotarg(11,JPART)
         LFRAMEQQ = .FALSE.
         IF (iplotarg(12,JPART) .NE. 0) THEN
            LFRAMEQQ = .TRUE.
         ENDIF
         LDOUBLEQQ = .FALSE.
         IF(iplotarg(13,JPART) .NE. 0) THEN
            LDOUBLEQQ = .TRUE.
         ENDIF
         WRITE(6,415)JPART
  415    FORMAT(/,'plotpart: . . .  P A R T -- ',I2,'  . . .')
         write(6,425)CXXXSET(1:7),ISSNO,J0VAL_DST,
     1               INDENT_DST,IWIDTH,IHGT,LUNINP,
     2               NREC_STARTOMAP,NREC_SKIP,
     3               NBYT_SKIP

  425    FORMAT(1x,A7,'NO.',I6,'; TO BE PLOTTED AT J=',I6,
     1              '; INDENT_DST=',I6,
     2         /,7X,'WIDTH,HGT= (',I6,',',I6,')',
     3         /,7X,'TO BE FETCHED FROM U:',I2,
     4                 '   PROD-START REC=',I6,
     5         /,7X,'FROM WHICH POINT NREC-SKIP=',I6,
     6                 ';  NBYT-SKIP=',I6)

         write(6,427)INDENT_SRC,LFRAMEQQ,LDOUBLEQQ
  427    format(7X,'INDENT_SRC=',I6,'; LFRAMEQQ= .',L1,'.; ',
     1             'LDOUBLEQQ= .',L1,'.')

         iret_rew = rewind99(LUNINP)
         if(iret_rew .NE. 0) then
           write(6,FMT='('' plotpart::rewind99: FAILED to '',
     *       ''rewind with code='',I4,/,7X,''LUNINP='',I3)')
     *       iret_rew,LUNINP
           iret_plp = 4
           go to 999
         endif

         inrelrecno = nrec_startomap - 1
         if(inrelrecno .LT. 0) then
           inrelrecno = 0
         endif
         inrelrecno = inrelrecno + nrec_skip

         byte_offset = krecsizbyt * inrelrecno

         iret_pos = reposi99(LUNINP,byte_offset,from_indx)
         if(iret_pos .NE. 0) then
           write(6,FMT='('' plotpart::reposi99: FAILED to '',
     *                   ''reposition with code='',I4,
     *                  /,7X,''LUNINP='',I3)')
     *             iret_pos,LUNINP
           iret_plp = 5
           go to 999
         endif
         nbufpart = 0
         lintot = 0
         LFIRSTQ = .TRUE.
         LCHKPRT = .TRUE.
  500    continue
         if(lintot .GE. IHGT) then
C          ... I have satisfied the IHGT no. of scanlines for this part
           go to 555
         endif

         iret_rds = rd1byt99(LUNINP,krecsizbyt,c1array,nbytsred)
#if defined(_LITTLE_ENDIAN)
         CALL SWAP_BYTES_IN_PLACE (intarray, 8_8, nbytsred / 8_8)
#endif
         if(iret_rds .NE. 0) then
           write(6,FMT='('' plotpart::rdsbyt99: FAILED to read '',
     *                   ''with code='',I4,
     *                  /,7X,''LUNINP='',I3)')
     *             iret_rds,LUNINP
           iret_plp = 5
           go to 999
         endif
         nbufpart = nbufpart + 1
C        ... does rdsbyt99 automatically update pointer by recsiz?
C        ... next, how to unpack .pak6 to .x6t ???
         if (nbufpart .EQ. 1) then
           nbyskip = nbyt_skip
         else
           nbyskip = 0
         endif
         numrec = inrelrecno + nbufpart

         CALL scan1440(INTARRAY,numrec,nbyskip,LFIRSTQ,LCHKPRT,
     1                 icountEOL,iret_scn)

         lintot = lintot + icountEOL
         if(iret_scn .LT. 0) then
            write(6,525)iret_scn
  525       format(' plotpart::scan1440: terminated reading ',
     1             'with code=',I4)
            go to 555
         endif
C        ... pull one scanline out byte-by-byte until end-of-line
         
C        ...   if bin is exhausted, then I must read another bin.pak6
         LCHKPRT = .FALSE.
         GO TO 500

  555    continue
C        ... comes here to clean up after reading scanlines loop
         write(6,557)nbufpart,lintot,IHGT
  557    format(' plotpart: completed reading this part with ',
     1          'input buffer count=',I6,
     2         /,7X,'which contained total line count=',I6,
     3         /,7X,'          the desired line count=',I6)
         go to 999

  999  CONTINUE
       return
       end
c*********************************************************************
       subroutine rec_dump(mrecno,idatarec,nwrdprec)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    REC_DUMP    RECORD-DUMPER IN HEX AND AS CHARACTERS
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 95-05-31
C
C ABSTRACT: TO DUMP THE GIVEN RECORD IN HEXADECIMAL AND ALSO INTERPRETED 
C   AS ASCII CHARACTERS; WITH THE BYTE-LOCATION (IN HEX) OF THE STARTING
C   WORD OF EACH PRINTED LINE OF THE DUMP.
C
C PROGRAM HISTORY LOG:
C   95-05-31  DAVID SHIMOMURA
c   98-08-11  caruso     removed CDIR$ INTEGER=64 from top of this code.
c                        upgrading to fortran 90 and making y2k-compliant.
C
C USAGE:    CALL REC_DUMP(MRECNO, IDATAREC, NWRDPREC)
C   INPUT ARGUMENT LIST:
C     MRECNO    - INT RECORD NUMBER - 
C                     THE FIRST REC IS NO. 1 (NOT ZERO);
C                     YOU MUST INCREMENT IT WHEN APPLICABLE.
C     IDATAREC  - INT DATA RECORD
C     NWRDPREC  - INT NUMBER OF WORDS PER RECORD
C
C   OUTPUT FILES:
C     FT06F001 - PRINTOUT
C                THE ONLY OUTPUT IS THE DUMP TO THE PRINT FILE
C
C REMARKS: 
C     THIS IS LIMITED TO A DATA RECORD OF FULLWORDS
C       WHERE FULLWORD ON THE CRAY IS 8-BYTES;
C
C     THE HEX DUMP OF EACH FULLWORD IS PRINTED IN TWO PARTS
C       WITH AN INTERVENING BLANK IN THE MIDDLE OF THE FULLWORD.
C     
C     CALLS ON XASC2ASC() IN ORDER TO INTERPRET BYTES AS ASCII
C       CHARACTERS.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN 90
C   MACHINE:  CRAY 
C
C$$$
C                                                       30-May-1995/dss
C      ... A version of fwd_dump for the CRAY; which is a hi-endian 
C      ... machine, so I don't need to reverse the word sequence
C      ... of the printed HEX-dumped words; but I still like the VAX-
C      ... style of HEX and character printed on the same line.
C
C
       integer      nwrdpline
       parameter   (nwrdpline=2)
C ...     ... where nwrdpline is no. of data full_words (CRAY)
C ...          per print line
       integer      niiwrdpline
       parameter   (niiwrdpline=2*nwrdpline)
C      ... where niiwrdpline is no. of data half_words per print line
C
       integer      kbytpwrd
       parameter   (kbytpwrd=8)
C
       integer      mrecno
       integer      idatarec(nwrdprec)

       integer      iprwd(nwrdpline)

       integer       iprchwd(nwrdpline)
       character*16  chprtln
       equivalence  (iprchwd,chprtln)

       character*16   chexwd(nwrdpline)
       character*8   chexwdhaf(niiwrdpline)
       equivalence  (chexwd,chexwdhaf)
       
       integer      nwords
       integer      nlines
       integer      nzeroln
       integer      iwrdaddr
       integer      ibytaddr
       integer      m1,m2
       integer      iorsum
       integer      jpw
       integer      nbyskp
       integer      irecno
       integer      iacc
       integer      nwrds_remain
       logical      lpart_ln
       logical      last_line

       save
C      . . . . . . .   s t a r t   . . . . . . . . . . . . . . . . .
C
       nwords = nwrdprec
       if(nwords .le. 0) then
C        ... FATAL ERROR WHEN NUMBER OF WORDS PER RECORD IS .LE. 0
         write(6,125)MRECNO
         WRITE(6,115)NWRDPREC
  115    format(/,' FWD_DUMP: ERROR.  BAD VALUE WAS GIVEN FOR',
     1            ' NUMBER_OF_WORDS_PER_RECORD=',I9,
     2         /,'========================================',
     3           '=======================================')
         GO TO 999
       endif

       irecno = mrecno
       if(irecno .le. 0) then
         irecno = 1
       endif
       write(6,125)MRECNO
  125  format(/,' * * * * * *  RECORD NO. =',I9,
     1          ' * * * * * * * * * * * * * * * * * * *')  
       nlines = nwords / nwrdpline

       lpart_ln = .false.
       nwrds_remain = mod(nwords,nwrdpline)
       if(nwrds_remain .NE. 0) then
         nlines = nlines + 1
         lpart_ln = .true.
       endif

       nzeroln = 0
       nbytpline = nwrdpline * kbytpwrd
       ibytaddr = (irecno-1) * nwords * kbytpwrd
       ibytaddr = ibytaddr - nbytpline

       last_line = .false.
       do  jprln = 1,nlines
         if(jprln .eq. nlines) then
           last_line = .true.
         endif
       
         ibytaddr = ibytaddr + nbytpline
         iwrdaddr = nwrdpline*(jprln-1)
         m1 = iwrdaddr + 1
         m2 = m1 + nwrdpline - 1

         if(last_line) then
C          ... initialize line buffers for last line, in case partial
           chprtln(1:) = ' '
           do  i = 1,nwrdpline
             iprwd(i) = 0
           enddo
           if(m2 .GT. nwrdprec) then
C            ... here is case of last partial line, 
C            ..    do not let do reach beyond end of record ...
             m2 = nwrdprec
           endif
         endif

         iorsum = 0
         ic = 0
         do  ip = m1,m2
           ic = ic + 1
           iacc = idatarec(ip)
           iprchwd(ic) = iacc
           iorsum = ior(iorsum,iacc)
           iprwd(ic) = iacc
         enddo
         
         call xasc2asc(chprtln,chprtln,nchmoved,iret_asc)

         if(iorsum .eq. 0) then
C          ... this print line is all zero, so
           nzeroln = nzeroln + 1
           if(nzeroln .eq. 1) then
C            ... on the first occurrence of a bundle of zero lines, pr
           m2ii = niiwrdpline
C          ... but what if this first zero line was actually the
C          ... last and partial line ???
           if(last_line) then
             if(lpart_ln) then
               m2ii = 2*nwrds_remain
             endif
           endif

           do  i = 1,m2ii
             chexwdhaf(i)(1:8) = '00000000'
           enddo
           write(6,261) ibytaddr,(chexwdhaf(i),i=1,niiwrdpline)
  261      format(1X,Z8.8,':',4(1X,A8),' : ')
           endif
         else
C          ... here is a not all-zeros line,
C          ... if this non-zero line was preceded by more than
C          ... one zero line, then I should print the count of
C          ... zero lines that it skipped
           if(nzeroln .gt. 1) then
             nbyskp = nwrdpline*kbytpwrd*(nzeroln - 1)
             write(6,262)nbyskp
  262        format(1h ,'(',Z8.8,')*  is Count (in HEX) of zero bytes',
     1                  ' not printed.')
           endif
           nzeroln = 0
C        
C          ... then print the current non-zero line
           do  iwrd = 1,nwrdpline
             write(chexwd(iwrd),2632) iprwd(iwrd)
 2632        format(Z16.16)
           enddo

C          ... but what if this was last_line and it was a partial??
C          ... I would rather print blanks for the positions on the
C          ... line which are not in the record bounds
           if(last_line) then
             if(lpart_ln) then
               m1i = nwrds_remain + 1
               if(m1i .LE. nwrdpline) then
                 do  iwrd = m1i,nwrdpline
                   chexwd(iwrd)(1:) = ' '
                 enddo
               endif
             endif
           endif

           write(6,263) ibytaddr,(chexwdhaf(i),i=1,niiwrdpline),
     1                   chprtln(1:16)
  263      format(1X,Z8.8,':',4(1X,A8),' : "',A,'"')
         endif
       enddo
C      ... what if running zero lines at end of a record?
       if(nzeroln .GT. 1) then
             nbyskp = nwrdpline*kbytpwrd*(nzeroln - 1)

C            ... but what if last line of zeros was a partial line?
             if(lpart_ln) then
               nbytxs = (nwrdpline - nwrds_remain)*kbytpwrd
               nbyskp = nbyskp - nbytxs
             endif
C            ... 
             write(6,262)nbyskp
       endif
       nzeroln = 0
  999  continue
       return
       end
c**********************************************************************
      SUBROUTINE scan1440(inbuf,numrec,nbyt_skip,LFIRSTQ,LCHKPRT,
     *                     icountEOL,jretn)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C subprogram:    scan1440
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 95-07-10
C
C ABSTRACT: 
C
C PROGRAM HISTORY LOG:
C   94-12-06  shimomura  copied ~/ig2/vt240/rdsx6b_512.for 
C             in order to adapt to the CRAY environment
C   96-08-19  shimomura  copied ~/ig2/cra/bg/rd6b1440.f in order to
C                        adapt to a limited application within
C                        checkout program fxcompos
C                        subr plotpart()
C                        in which a 6-bit packed raster input buffer 
c                        has just been read into; and what is really 
c                        desired is one scanline at a time to be 
c                        extracted from that input buffer;
C                        But just for flow, simply count the scanlines
C                        In this application, we do not read input from 
c                        within this;  but maybe it should?
C                        This expects to be called only for good raster 
c                        data buffer; not for ISCHED record; not for 
c                        END-of-all maps
c   98-08-11  caruso     upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C
C USAGE:  call scan1440(inbuf,numrec,nbyt_skip,LFIRSTQ,LCHKPRT,
c                          icountEOL,jretn)
c
C   INPUT ARGUMENT LIST:
c     inbuf     - integer input buffer array
c     numrec    - integer number of records
c     nbyt_skip - integer number of bytes to skip
c     lfirstq   - logical.  if true, initialize array longlinwrk to zero
c     lchkprt   - logical.  if true, checkout print is on.
c
c   output argument list
c     icounteol - integer number of scanlines
C     jretn     - return code = 0   for usual interior record
C                             = +1  for start-of-map flag encountered
C                             = -1  X'FFFFFD'-headed ISCHED record 
c                                   was found
C                             = -2  X'FFFFFC'-headed end-of-all-maps 
c                                   was found
c
c   remarks:
C      ... User will set LFIRSTQ=.TRUE. on the first call of a PART
C            I will use that flag to initialize output X6B scanline
C      -----------------------------------------------------------------                     
C      ... Subroutines called:
C            GBYTES() -- a Russ Jones' W3LIB routine to unpack
C      -----------------------------------------------------------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$
C
       INTEGER         kbytin
       parameter      (kbytin = 1440)
       INTEGER         kwdinp
       parameter      (kwdinp = kbytin / 8)

       integer         kgrpin
       parameter      (kgrpin = kbytin*8/6)
C        ...   where  (kgrpin = kbytin*8/6) = 1920 groups per rec.
C
       integer         lmtnpels
       parameter      (lmtnpels=4224)  		!... used to be 4192
       integer         lmtndibyt
       parameter      (lmtndibyt=lmtnpels/4)  	!... =1056 di-bytes
C
       integer         lmtpurbyt
       parameter      (lmtpurbyt=lmtnpels/8)    !... =528 bytes
       integer         lmtpurwrd
       parameter      (lmtpurwrd=lmtpurbyt/8)  	!... =66 longwords
           
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       INTEGER         INBUF(kwdinp)
       integer         numrec
       integer         nbyt_skip
       LOGICAL         LFIRSTQ
       LOGICAL         LCHKPRT
       integer         icountEOL
       INTEGER         jjmapin(KGRPIN)
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
       integer         noffset
       integer         ngrps2do
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       integer         longlinwrk(lmtndibyt)
       INTEGER         IDIBYT
       character*1     linex6b(lmtndibyt)

       integer         i8pure(lmtpurwrd)
       character*1     c1pure(lmtpurbyt)    	!...dcdArg6
       equivalence    (i8pure(1),c1pure(1))
C
       integer         nchs   		!...dcdArg2
       logical         ldubl   		!...dcdArg3
       integer         nskpdib    	!...dcdArg4
       integer         npels		!...dcdArg5
       integer         kbytpur		!...dcdArg7
       integer         iret_pel  	!...dcdArg8

       INTEGER         jretn
C
       integer         ifid(6)
       character*48    cifid
       integer         mskrhs
       data            mskrhs      / X'00000000FFFFFFFF' /
       INTEGER         KDOTSISP
       DATA            KDOTSISP    / X'2F2F2F2F00000000' /

       integer         lendmap
       data            lendmap     / X'33' /
       integer         lendline
       data            lendline    / X'30' /
C
       integer         lonecode
C
       integer  LCKPT
       integer  nrectemp
C
       integer  kbitpgrp
       data     kbitpgrp   / 6 /

       integer  kpadbits
       data     kpadbits   / 0 /
C      ... which are args to CGYTES() ...
       integer    kbitpx6b
       data       kbitpx6b     / 8 /
       integer    noffsetx6b
       data       noffsetx6b   / 0 /
C      ... which are args to SBYTESCCS ...
C
       integer  msk6fs
       integer  kstart
       integer  kisched
       integer  kendall

       data     msk6fs    / X'FFFFFF0000000000' /
       data     kstart    / X'FFFFFF0000000000' /
       data     kisched   / X'FFFFFD0000000000' /
       data     kendall   / X'FFFFFC0000000000' /
C
       character*4    cmd_pur

       SAVE

C      ... I am changing return codes from what it used to be ...
       jretn = 0
       icountEOL = 0
       ngrps2do = 0
       LCURRGRP = 0

       IF(LFIRSTQ) THEN
         idibyt = 0
         do  i = 1,lmtndibyt
           longlinwrk(i) = 0
         enddo
         LFIRSTQ = .FALSE.
       ENDIF

       if(nbyt_skip .LT. 0 .or. nbyt_skip .GT. kgrpin) then
         write(6,105)NBYT_SKIP
  105    format(' scan1440: FAILED on bad value for NBYT_SKIP=',I9)
         jretn = -3
         go to 999
       endif
C
  200  continue


       if(iand(ieor(inbuf(1),kstart),msk6fs) .eq. 0) then
C        ... found a start of map ...

         write(6,215)numrec
  215    format(' scan1440: found start-of-map at Rec. No. =',I7)
C        ... try to decode the IFID zone in the first rec of map ...
         do  i = 1,6
           ifid(i) = inbuf(i)
         enddo
         ifid(1) = iand(ifid(1),mskrhs)
         ifid(1) =  ior(ifid(1),KDOTSISP)
C        ... which overwrote the FFFFFF00 in the lhs, with dots
C        ... convert ifid from CDC display code into ASCII;

         CIFID(1:) = ' '

         write(6,217)cifid(1:48)
  217    format('         IFID="',A,'"')
       
C        ... On first record, of map
C        ...    spread it out, just like an interior record
         noffset = 0      
         ngrps2do = kgrpin       
C        ...   where  (kgrpin = kbytin*8/6) = 1920 groups per rec.
C
         Call GBYTES(inbuf,jjmapin,noffset,kbitpgrp,kpadbits,ngrps2do)
C
C        ... from here I will return w/ (jjmapin(i),i=1,ngrps2do)
C        ... containing the extended 6-bit code from one record read
         JRETN = 1
         go to 300

       else if(iand(ieor(inbuf(1),kisched),msk6fs) .eq. 0) then
C        ... found isched rec at end of map ... 
C        ...   In this application, I should not be here;
         write(6,225)numrec
  225    format(' scan1440: found ISCHED endmap at Rec. No. =',I7)

C        ... but user must not expect raster data returned
         JRETN = -1
         go to 999

       else if(iand(ieor(inbuf(1),kendall),msk6fs) .eq. 0) then
C        ... found end of all maps ...
         write(6,235)numrec
  235    format(' scan1440: found end-of-all-maps at Rec. No. =',I7)
         jretn = -2
         go to 999


       else
C        ... here is interior raster-data rec. with no header ...
C        ... On interior records,
         noffset = 0      
         ngrps2do = kgrpin       
C        ...   where  (kgrpin = kbytin*8/6) = 1920 groups per rec.
C
         Call GBYTES(inbuf,jjmapin,noffset,kbitpgrp,kpadbits,ngrps2do)
C
C        ... from here I will return w/ (jjmapin(i),i=1,ngrps2do)
C        ... containing the extended 6-bit code from one record read
         go to 300

       endif
       go to 999

  300  continue
C      ... count the End-of-scanlines in interval 
C      ...     jjmapin(nbyt_skip+1,kgrpin)
       icountEOL = 0
       m1 = nbyt_skip + 1
       m2 = m1 + 15
       if(m2 .GT. kgrpin) then
         m2 = kgrpin
       endif
       IF(LCHKPRT) THEN
         write(6,304)m1,kgrpin
  304    format(' scan1440: scan from jjmapin(',I6,') to (',I6,')')
         write(6,305)(jjmapin(i),i=m1,m2)
  305    format(16Z4.2)
       ENDIF
       do  iby = m1,kgrpin
         lonecode = jjmapin(iby)
         if(lonecode .EQ. lendmap) then

           write(6,315)iby,ngrps2do
  315      format(' scan1440: Found end-of-a-map flag at byte(',I5,
     1                ') of',I5)
           write(6,335)icountEOL,numrec
           JRETN = -5
           go to 999
         else if(lonecode .EQ. lendline) then
           if(idibyt .LT. lmtndibyt) then
             idibyt = idibyt + 1
             longlinwrk(idibyt) = lonecode
             icountEOL = icountEOL + 1
           endif
C
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C          . . . . .  when it hits end-of-scanline, then process line
           cmd_pur(1:4) = 'conv'
           call vx6b_pur(cmd_pur,longlinwrk,idibyt,iret_pur)
C          . . . . . . . . . . . . . . . . . . . . . . . . . . . .

           idibyt = 0
         
         else
C          ... NOT A CONTROL DI-BYTE; ASSUME RLE CODE DATA,
           if(idibyt .LT. lmtndibyt) then
             idibyt = idibyt + 1
             longlinwrk(idibyt) = lonecode
           endif
         endif
       enddo
  333  continue
       IF(LCHKPRT) THEN
         write(6,335)icountEOL,numrec
  335    format(' scan1440: end-of-scanlines count=',I6,
     1              '; in rec. no.',I6)
       ENDIF       
       
       go to 999
C      ... which returns to user
C      ... who will call me again when she wants another input record
C      ...   unpacked


C      ----------------------------------------------------------------
C
  999  continue
       RETURN
       END
c**********************************************************************
       subroutine strpfnam(lwr_or_upr,c1name,nc,cnamtxt,nchres,iretn)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C subprogram:    strpfnam    strip filename    
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 95-07-10
C
C ABSTRACT:  filename stripper. used to extract the innermost name, 
c            without the extent name and without the pathname.
C
C PROGRAM HISTORY LOG:
c   95-08-29  shimomura
c   98-08-11  caruso     upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C
C USAGE:  call strpfnam(lwr_or_upr,c1name,nc,cnamtxt,nchres,iretn)
c
C   INPUT ARGUMENT LIST:
C     lwr_or_upr -  integer =0  for no change in Case;
C                          =1  change to Lower Case;
C                          =2  change to Upper Case )
c     c1name     -  char*1 name array
c     nc         -  integer array size of c1name
c
c   output argument list
c     cnamtxt    -  char array with filename
c     nchres     -  integer number of chars in resultant string
c     iretn      -  return code 
c                      = 0  for normal return
C                      = 1  if text string for results is undefined
C                      = 2  if nc dimension of given string is <=0
C                      = 3  if given text is all blanks
C                      = 4  if given text is bad. NULL before any good.
C                              did you put anything into c1name?
C                      = 5  if I am lost.  impossible.
C                      = 6  if given text has no good portion in it.
C                              did you end with a special character?
c
c   remarks:
C      ... copied ~/tools/stripfinam.f into ~/cra/bg/strpfnam.f
C      ...    in order to make a CRAY version.
C
C      ... called from SIF cmd84A to strip the given filename of
C      ...   superfluous info and extract only the unqualified name.
C      Hypotheses:  SIF cmd84A wants the unqualified filename
C          with no extent; no directory names; no leading nor
C          trailing blanks; limited to 12 alphanumeric characters
C          of which only the first 9 characters will actually be
C          used.  Note: only alphanumerics are allowed.  No underbars.
C         (Underbar permitted in CRAY version of 950829)
C          Also, this subroutine will set the alphanumerics to upper-
C          case.
C         (Case change is an option in CRAY version of 950829.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$
C
C
C      ... calls on logical function isalpha(c)
       external       isalpha
       logical        isalpha
C      ... for logical function isalpha ...

C
       integer        lwr_or_upr    	!...=1 lwr; =2 upr; else unchgd
       character*1    c1name(nc)
       character*(*)  cnamtxt
       integer        iretn
C
       logical        lfoundext
       character*1    chone
C
       character*1    underbar
       data           underbar   / '_' /

       character*1    NULL

       save
C
       NULL = char(0)

       iretn = 0
       nchres = 0
       limres = len(cnamtxt)
       if(limres .le. 0) then
         iretn = 1
         go to 999
       endif
C
       cnamtxt = NULL
       limgivn = nc
       if(limgivn .le. 0) then
         iretn = 2
         go to 999
       endif
C
C      ... scan given string for any leading blanks ...

       m1 = 1
       do  i = 1,limgivn
         chone = c1name(i)
         if(chone .eq. NULL) go to 900
         if(chone .ne. ' ') go to 200

C          ... which jumps out of this loop on very first non-blank char
C          ... otherwise, this ith char is a leading blank,

         m1 = i + 1
       enddo

C      ... if it falls thru this do, then given string was all blanks

       iretn = 3
       go to 999

C      ... otherwise, first non-blank char at c1name(m1)

  200  continue

C      .. when it comes here, m1 points to first non-blank char in c1name
C      ...  so scan for other end of string that starts at c1name(m1)

       m2 = m1
       do  i = m1,limgivn
         chone = c1name(i)
         if(chone .eq. NULL .or. chone .eq. ' ') go to 300

C          ... normal end of string if NULL or blank terminated, 
C          ...    m2 pointing at last good char before the terminator
C          ... otherwise, this ith char is non-blank non-NULL

         m2 = i
       enddo

C      ... if it falls thru this do, then given string fill out to 
C      ...   limgivn with non-blank characters

  300  continue

C      ... examine the string from c1name(m1) to (m2) for good stuff

       ncha = m2 - m1 + 1
       if(ncha .le. 0) go to 910

C        ... logic error.  cannot be
C
C      ... look for extent_name behind the last period

       lfoundext = .false.
       locstart = m2
       locterm = m2 + 1
       do  ir = m2,m1,-1
         chone = c1name(ir)
         if((isalpha(chone)) .OR.
     1      (chone .EQ. underbar)) then

C          ... good one ...

           locstart = ir

         else

C          ... here is the non-alphanumeric I was afraid I'd find

           if (.not. lfoundext) then
             if (chone .eq. '.') then
               lfoundext = .true.
               locterm = ir

C              ... where locterm points at the '.' before the extent

             else

C              ... the special char is not a '.', so terminate scan

               locstart = ir + 1
               go to 400
             endif
           else

C            ... comes here on a special char after I had already
C            ...   reset for extent name, so terminate scan

             locstart = ir + 1
             go to 400
           endif
         endif
       enddo
  400  continue

C      ... comes here with good stuff bounded by locstart:(locterm-1)

       locstop = locterm - 1
       ngood = locterm - locstart
       if(ngood .le. 0) go to 920

C      ... can i pass all the good stuff to results string?
C      ...    n2do  is  lesser of limres and ngood and 12

       n2do = min(ngood,limres,12)
       do  i = 1,n2do
         ll = locstart + i - 1
         chone = c1name(ll)
         if(lwr_or_upr .EQ. 1) then

C           ... lower_case it ...

           if(llt(chone,'A') .or. lgt(chone,'Z')) then
             cnamtxt(i:i) = chone
           else

C            ... chone is an Upper_case ltr, so lower it,

             cnamtxt(i:i) = char(mova2i('a') - mova2i('A') + 
     1                           mova2i(chone))
           endif
         else if(lwr_or_upr .EQ. 2) then

C           ... upper_case it ...

           if(lge(chone,'a') .and. lle(chone,'z')) then
             cnamtxt(i:i) = char(mova2i('A') - mova2i('a') + 
     1                           mova2i(chone))
           else
             cnamtxt(i:i) = chone
           endif
         else

C           ... leave _case unchanged ...

             cnamtxt(i:i) = chone
         endif 
       enddo
       nchres = n2do
       if(n2do .lt. limres) then
         cnamtxt(n2do+1:n2do+1) = NULL

C        ... which puts a NULL terminator if there is space ...

       endif
       go to  999
C
  900  continue
       iretn = 4
       go to 999
  910  continue
       iretn = 5
       go to 999
  920  continue
       iretn = 6
       go to 999
C
  999  continue
       return
       end
c**********************************************************************
       subroutine vx6b_pur(cmd_pur,longlinwrk,ndimen,iret_pur)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C subprogram:    vx6b_pur    decode nmc 6-git rle code
C   PRGMMR: SHIMOMURA        ORG: W/NMC41    DATE: 95-07-10
C
C ABSTRACT:  decoder of one very-extended NMC 6-bit RLE code
C            called from scan1440() which is within Program fxcompos 
C
C PROGRAM HISTORY LOG:
c   96-08-21  shimomura
c   98-08-11  caruso     removed CDIR$ INTEGER=64 from top of this code.
c                        upgrading to fortran 90 and making y2k-compliant.
c                        adding docblock.
C
C USAGE:  call vx6b_pur(cmd_pur,longlinwrk,ndimen,iret_pur)
c
C   INPUT ARGUMENT LIST:
c     cmd_pur    - char.  if = 'init', initialize.  if = 'conv', decode
c     longlinwrk - integer array to be decoded
c     ndimen     - integer size of longlinwrk
c
c   output argument list
c     iret_pur   - return code
c
c   remarks:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
c
c$$$
C
       integer         nbytpwrd
       parameter      (nbytpwrd=8)

       integer         lmtnpels
       parameter      (lmtnpels=4224)  		!... used to be 4192
       integer         lmtndibyt
       parameter      (lmtndibyt=lmtnpels/4)  	!... =1056 di-bytes
C
       integer         lmtpurbyt
       parameter      (lmtpurbyt=lmtnpels/8)    !... =528 bytes
       integer         lmtpurwrd
       parameter      (lmtpurwrd=lmtpurbyt/8)  	!... =66 longwords
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       character*4     cmd_pur
       integer         longlinwrk(ndimen)
       integer         iret_pur
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

C      ==============================================================
       INTEGER    IMAXWIDWRD
       PARAMETER (IMAXWIDWRD=27)  	!...27*64 = 1728 PELS WIDE
       INTEGER    JMAXHGTLNS
       PARAMETER (JMAXHGTLNS=7500)

       COMMON    /SPAC4IMG/ NPELS_WID, NLNS_NEED, JLNADDCON, JLN_CURR,
     1                      IMAGE(IMAXWIDWRD,JMAXHGTLNS)

C      ==============================================================
       INTEGER      IMAXWIDBYT
       PARAMETER   (IMAXWIDBYT=8*IMAXWIDWRD) 	!... 8*27 = 216 BYT
       integer      i8purfax(IMAXWIDWRD)	!...  27 wrd = 1728 pels
       character*1  c1purfax(IMAXWIDBYT)	!... 216 byt = 1728 pels
       equivalence (i8purfax(1),c1purfax(1))

       character*1     linex6b(lmtndibyt)

       integer         i8pure(lmtpurwrd)
       character*1     c1pure(lmtpurbyt)    	!...dcdArg6
       equivalence    (i8pure(1),c1pure(1))
C
       integer    keos
       data       keos         / X'30' /

       integer    kframe1
       data       kframe1      / X'F000000000000000' /

       integer    kpadbits
       data       kpadbits     / 0 /
       integer    kbitpx6b
       data       kbitpx6b     / 8 /
       integer    noffsetx6b
       data       noffsetx6b   / 0 /
C      ... which are args to SBYTESCCS ...
C
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . . . . .   Args in call to dcdlnx6b()  . . . . . . . . . . .
C      ...       character*1     linex6b(lmtndibyt)    !... Arg1
       integer         nchs   	       !...dcdArg2 given data byte count
C                                               in given linex6b() array
       logical         ldubl   		!...dcdArg3
       integer         nskpdib    	!...dcdArg4 skip off of FAX edge
       integer         npeltot		!...dcdArg5 result
C                                      !...dcdArg6 is C1PURE for results
       integer         kbytpur		!...dcdArg7 = 216 = given 
C                                              dimension of c*1 C1PURE()
       integer         iret_pel  	!...dcdArg8 resulting err_flag
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       LOGICAL         LFRAMEQQ
       integer         linum_part
       integer         indent_src
       integer         iwidth_src
       integer         jhgt_src
       integer         itoss_src

       integer         npxlperbyt
       integer         npxldo
       integer         npxldoing
       integer         nwdout
       integer         nritshfbyt
       integer         ibytdes
       integer         nbyt2mov
       logical         checkout
       logical         lchkprntq
       character*1     cxff

       SAVE

       iret_pur = 0
       cxff = char(255)  		!... = X'FF'

       checkout = .TRUE.
       if(cmd_pur(1:4) .EQ. 'init') then
C        ... in this initialization call for this part,
C        ...    longlinwrk contins the iplotarg for this part

       
         linum_part = 0
         j0val_dst  = longlinwrk(3)
         indent_dst = longlinwrk(4)
         jhgt_src   = longlinwrk(6)
         indent_src = longlinwrk(11)
         iwidth_src = longlinwrk(5)

         itoss_src = 1728 - indent_src - iwidth_src
         if(itoss_src .LT. 0) then
           itoss_src = 0
         endif
C        ... where itoss_src is no. of pels to discard from the fax edge
C        ...    of the complete scanline, to get to the portion for keep
         nskpdib = itoss_src / 4  		!... in 4-bit groups

         ldubl = .false.
         if(longlinwrk(13) .NE. 0) then
           ldubl = .TRUE.
         endif

C        . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C        ... the LDUBL option is resulting in blank map;
C        ... so let us disable the doubling, just to see if it will flow

C ...         ldubl = .false.
C
C        ... that mod was effective in producing the cut, undoubled,
C        ...   which was better than the blank subset it produces 
C        ...   without that patch;
C        ... But to let me debug the faulty ldubl, comment that patch
C        . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C
         if(LDUBL) then
           npxlperbyt = 4    	!... only 4 original pels, expanded,/byt 
         else
           npxlperbyt = 8
         endif

         npxldo = iwidth_src

         kbytpur = npxldo/npxlperbyt
C          ... where npxlperbyt == 8; for usual case
C                               == 4; for LDUBLing case
C          ...       npxlperbyt is number of original pxls which go into
C          ...       a byte of fnl pure
C          ... where kbytpur is the number of bytes of pure raster
C          ... to be generated from the source width cut

         if(mod(npxldo,npxlperbyt) .NE. 0) then
            kbytpur = kbytpur + 1
         endif

         npxldoing = npxldo
         if(kbytpur .GT. lmtpurbyt) then
C          ...  character*1     c1pure(lmtpurbyt)    	!...dcdArg6
C          ... kbytpur is too big for dimension of C1pure(); so reset
           kbytpur = lmtpurbyt
           npxldoing = npxlperbyt * kbytpur
         endif

         nwdout = kbytpur / nbytpwrd
         if(mod(kbytpur,nbytpwrd) .NE. 0) then
           nwdout = nwdout + 1
         endif
 
         write(6,FMT='(/,'' vx6b_pur: at INIT, pure-raster '',
     1                 ''scanline-length in bytes in KBYTPUR='',I5,
     2                /,''          which will contain pixel '',
     3                    ''count in NPXLDOING='',I6,
     4                /,''          output longwords per scanline='',
     5                     I4)')
     A           kbytpur,npxldoing,nwdout


         LFRAMEQQ = .FALSE.
         if(longlinwrk(12) .NE. 0) then
           LFRAMEQQ = .TRUE.
         endif

         iwidsrcpxl = iwidth_src
         if(LDUBL) then
           iwidsrcpxl = 2*iwidsrcpxl
         endif
         nritshfbyt = (1728 - indent_dst - iwidsrcpxl)/8
         if(nritshfbyt .LT. 0) then
           nritshfbyt = 0
         endif

         ibytdes = nritshfbyt + 1
C        ... where c1purfax(ibytdes) is pointing to the image byte
C        ...   from which to start storing the one resulting pure line
C        ...   where ibytdes=1 is the FAX edge byte.
C
         nbyt2mov = kbytpur
         if((ibytdes - 1 + nbyt2mov) .GT. IMAXWIDBYT) then
           nbyt2mov = IMAXWIDBYT + 1 - ibytdes
         endif
         write(6,175)nbyt2mov,ibytdes,kbytpur,nritshfbyt,indent_dst,
     1               iwidth_src
  175    format(' vx6b_pur: at end of "init",  nbyt2mov=',I5,
     1              ';  ibytdes=',I5,
     2         /,7X,'kbytpur=',I5,'; nritshfbyt=',I5,
     3                 'indent_dest=',I5,'; iwidth_src=',I5)

         go to 999
C   
       endif

C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
C      ... otherwise, case of decode the given one scanline
C      ...    from very-extended 6-bit into pure uncompressed;
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
       linum_part = linum_part + 1
       if(linum_part .GT. jhgt_src) then
C         ... accept no more data ... we have satisfied this part
          iret_pur = 1
          go to 999
       endif

       if(mod((linum_part-1),100) .EQ. 0) then
         lchkprntq = .true.
       else
         lchkprntq = .false.
       endif

       do  i = 1,lmtpurwrd
         i8pure(i) = 0
       enddo

       if(ndimen .LE. 0) then
C        ... no data given; assume blank scanline
         go to 333
       else if(longlinwrk(1) .EQ. keos) then
C        ... blank scanline
         go to 333
       endif
C
C                      1        2         3         4    
#if defined(_LITTLE_ENDIAN)
      CALL SWAP_BYTES_IN_PLACE (linex6b, 8_8, ndimen)
#endif
       call SBYTESCCS(linex6b,longlinwrk,noffsetx6b,kbitpx6b,
     1             kpadbits,ndimen)
#if defined(_LITTLE_ENDIAN)
      CALL SWAP_BYTES_IN_PLACE (linex6b, 8_8, ndimen)
      CALL SWAP_BYTES_IN_PLACE (longlinwrk, 8_8, ndimen)
#endif
C                      5       6
       nchs = ndimen
C      ... now one entire scanline in x6b format is in 
C      ...    C*1 (linex6b(i),i=1,nchs)  the way I am accustomed to
C
       call dcdlnx6b(lchkprntq,linex6b,nchs,ldubl,nskpdib,npeltot,
     1               c1pure,kbytpur,iret_pel)

       if(lchkprntq) then
         write(6,315)iret_pel,linum_part,NCHS,LDUBL,nskpdib,npeltot,
     1               KBYTPUR
  315    format(' vx6b_pur::dcdlnx6b: iret_pel=',I5,
     1              ';  on part,s scanline no.=',I6,
     2         /,7X,'NCHS=',I6,'; LDUBL= .',L1,'. nskpdib=',I5,
     3              '; npeltot=',I6,'; KBYTPUR=',I6)
         iorsum = 0
         do  i = 1,27
           iorsum = ior(iorsum,i8pure(i))
         enddo 
         if(iorsum .eq. 0) then
           write(6,317)
  317      format(7X,' . . . Resulting line is all zero  . . . . .')
         endif
       ENDIF

       if(iret_pel .NE. 0) then
         if(iret_pel .LT. 0) then
C           ... serious error; so stop
            write(6,FMT='('' vx6b_pur::dcdlnx6b: scanline no.'',I6,
     1                 '';  FATAL ERROR code='',I4)')
     A              linum_part,iret_pel

            iret_pur = -9 
            go to 999
         else
C           ... warning from dcdlnx6b,
            if(checkout) then
              write(6,FMT='('' vx6b_pur::dcdlnx6b: scanline no.'',I6,
     1                          '';  WARNING code='',I4)')
     A                linum_part,iret_pel

            endif
         endif
       endif

  333  continue
C      ...   add FRAME here into c1pure ...
       if(LFRAMEQQ) THEN
         if((linum_part .LE. 4) .OR.
     1      (linum_part .GT. (jhgt_src - 4))) then
           do  ic = 1,kbytpur
             c1pure(ic) = cxff
           enddo
         else
C          ... this framed line is not near bottom or top of part
           i8pure(1) = ior(i8pure(1),kframe1)
           c1pure(kbytpur) = cxff
         endif
       endif
C      ... other top and bottom sides coming later ...

C      ... how to position a real inset on half of the image line?

       do  i = 1,IMAXWIDWRD
         i8purfax(i) = 0
       enddo

       if(lchkprntq) then
         write(6,535)nbyt2mov,ibytdes
  535    format(' vx6b_pur: moving ',I5,'-bytes from c1pure(1) to ',
     1          'c1purfax(',I5,')')
       endif

       if((ibytdes - 1 + nbyt2mov) .GT. IMAXWIDBYT) then
          nbyt2mov = IMAXWIDBYT + 1 - ibytdes
       endif
       if (ibytdes .LT. IMAXWIDBYT -1) then
          call xmovex(c1purfax(ibytdes),c1pure(1),nbyt2mov)
       endif 

C      ... only for checkout of the bad doubling case to force full line
C ...       call xmovex(c1purfax(1),c1pure(1),216)
   
C ...       ... how to put c1purfax into the image bitplane?
       if(LDUBL) then
         linumout = 2*linum_part - 1
         jln_curr = j0val_dst + linumout + jlnaddcon - 1
         do  iw = 1,imaxwidwrd
           image(iw,jln_curr) = ior(image(iw,jln_curr),i8purfax(iw))
         enddo

C        ... repeat that image line on the following line
C ...         jln_curr = j0val_dst + (2*linum_part) + jlnaddcon - 1
         jln_curr = jln_curr + 1
         do  iw = 1,imaxwidwrd
           image(iw,jln_curr) = ior(image(iw,jln_curr),i8purfax(iw))
         enddo

       else
C        ... for the usual un-doubled case,
         jln_curr = j0val_dst + linum_part + jlnaddcon - 1
         do  iw = 1,imaxwidwrd
           image(iw,jln_curr) = ior(image(iw,jln_curr),i8purfax(iw))
         enddo

       endif
C
  999  continue
       return
       end
