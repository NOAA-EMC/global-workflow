       subroutine ALLARGV(narg_lmt,carg,narg_got,iret_args)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    ALLARGV     GET ALL COMMAND-LINE ARGUMENTS
C   PRGMMR: HENRICHSEN       ORG: W/NP12    DATE: 1999-11-04 
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
C 1999-11-04  HENRICHSEN: MODIFYING ALLARGV() FOR THE IBM, MADE THE
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