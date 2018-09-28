       subroutine gtnxtwrd(line,lpstart,lpcurr,delims,ltr,word,ierror)
C      ... copied ~/tools/getwordnb.f           19-Jun-1995/dss
C      ...     (which was last compiled on Dec 2 1993)
C      ...    in order to adapt to called from prs_keqv()
C      ...    to NOT increment "lp", but return with "lpcurr"
C      ...    pointing to last location interrogated;
C      ...    and to prepare it for CRAY
C
C      ...  
C      ... copied subr getwordy.for              5-Dec-1991/dss
C      ... in order to make a version that does not stop for a 
C      ... blank terminator.  You can activate the blank by
C      ... including it in delims.
C      
C                                                2-Jul-1991/dss
C      ... copied char function getwordx to make it a subroutine
C                                                23-mar-1990/dss
C      ... To get one word out of the given line, starting at
C      ...     line(lp:lp) ...
C      ...    (after eliminating leading blanks out of the word.)
C      ... GIVEN:
C      ... (1) C*(*) LINE ... given character string (NULL-terminated)
C      ...              from which the next word is to be extracted.
C      ... (2) I*4   lpstart ... Line-Pointer from which to scan for word
C      ...              User should initialize to =1, to start scan
C      ... (3) C*(*) DELIMS ... NULL-terminated string of delimiters
C                           which are in addition to the default
C                           NEWLINE and NULL terminators.
C      ... (4) 
C      ... gtnxtwrd ... to get word from line at lp and increment lp
C      ...   A version of getword that tests for additional delimiters
C      ...   which are presented in the NULL-terminated string 
C      ...   named "delims"; the basic version tests for NULL and
C      ...   NEWLINE delimiters. 
C      ...   Added a test for a special end-of-line config of ",-"
C      ...   just before the NULL which terminates the line
C      ...   which we are using as a continuation signal.
C      ... CAUTION: Usually you would want to first call with lp=1;
C      ...            do not call with lp=0.
C      ... CAUTION: Given line must be NULL terminated to stop the scan.
C      ... CAUTION: You must allocate one more byte for your word than
C                     the text you expect, since I will insert a
C                     NULL terminator.
C
C      -----------------------------------------------------------
C      ... ierror =1; destination word is not defined as a char
C                         string;
C                 =2; delimiters DELIMS was not defined as a
C                         character string;
C                 =3; source line was not defined as a character
C                         string;
C                 =4; source line was empty ... first char in line
C                         was a NULL char;
C                 = -1;  Terminated on line-pointer LPCURR beyond 
C                        end of line;
C       ---------------------------------------------------------
C ...       include       '/usr2/shimomur/tools/global.def'
C

       logical        lcheckout
       parameter     (lcheckout=.FALSE.)
C      parameter     (lcheckout=.TRUE.)

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

       logical        found_term
       character*1    NULL
       character*1    NEWLINE
       character*1    BLANK
C
C
       ierror = 0
       NULL    = char(0)
       NEWLINE = char(10)
       BLANK   = char(32)
C
       lpcurr = lpstart
       if(lpcurr .le. 0) lpcurr = 1
C      ... which resets lp to first char position if she forgot to
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
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C      . . .   To set MXLNLP for limiting line scan   . . . . . . . .
C
C      PRINT *,' LINE=',LINE
       MXLNLP = 0
       lenline = len(line)
C      PRINT *, 'LENLINE=',LENLINE
       if(lenline .le. 0) go to 920
C          ... line was not defined as a character string ...
C      ... otherwise, line has a length.  Is it NULL terminated?
       limlp = index(line(1:lenline),NULL)
C      PRINT *,'LIMLP=',LIMLP
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
C      PRINT *,'MXLNLP=',MXLNLP
C      ... leaves here with MXLNLP pointing to the last character
C      ...   to be examined in the given line.
C      PRINT *,'LPCURR=',LPCURR
       if(lpcurr .gt. MXLNLP) go to 940
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
       limcont = limlp - 2
C
       if(lcheckout) then
         WRITE(6,115)lpcurr,lenword
  115    FORMAT(1H ,'gtnxtwrd:CALLED W/LP=',I5,'  WORD DIMENSIONED',I5)
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
  165      FORMAT(1H ,'gtnxtwrd:Before finding word, hit NEWLINE ',
     1                'at LP=', I4)
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
  225          format(1h ,'gtnxtwrd: reached end-of-line at LP=',I4,
     1                    '; end like NULL')
             endif
 
             found_term = .true.
             ltr = NULL
C            PRINT *,' FOUND_TERM IS TRUE.'
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
           else if(lpcurr .eq. limcont .and.
     1             line(lpcurr:lpcurr+1) .eq. ',-') then
             found_term = .true.
             ltr = NEWLINE
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
C          PRINT *,' 244, LPCURR=',LPCURR,' =',line(lpcurr:lpcurr)
           IF (line(lpcurr:lpcurr).EQ. '=') THEN
               lpcurr = lpcurr - 1
           ENDIF
       enddo
C      ... comes here only after delimiter has been found ...
C      PRINT *,' OUT OFF WHILE-DO'
       if(lcheckout) then
         LBYLTR = MOVA2I(LTR)
         LPM1 = lpcurr - 1
         WRITE(6,246)LBYLTR,LPM1
  246    FORMAT(1H ,'gtnxtwrd:found delimiter =HEX ',Z2.2,' AT LP=',I3)
       
         llone = mova2i(lonech)
         WRITE(6,255)lpcurr,wp,llone
  255    FORMAT(1H ,'gtnxtwrd:fell thru enddo with lp=',I4,
     1              ' wp=',I4,' lonech=hex',Z2.2)
       endif

  300  continue
       word(wp:wp) = NULL
C      ... WHICH INSERTED A NULL TERMINATOR IN RESULTING WORD ...
       if(lcheckout) then
         IF(WP.GT.1) THEN
           WP1= WP - 1
           LBYLTR = MOVA2I(LTR)
           WRITE(6,345)WORD(1:WP1),LBYLTR
  345      FORMAT(1H ,'gtnxtwrd:WORD="',A,'" ltr=hex ',Z2.2)
         ENDIF
       endif

C     PRINT *,' CHECK-END'
C      ... WHEN IT'S FINISHED THE LINE POINTER IS POINTING AT THE
C      ... DELIMITER THAT PAUSED THIS SCAN; SO IF I WANT IT TO POINT
C      ... BEYOND THE TERMINATOR ON NEXT CALL, THEN I HAVE TO INCR
C ...       IF(LTR .EQ. NULL .OR. LTR .EQ. NEWLINE)GO TO 800
C        ... WHICH WILL NOT LET IT MOVE ALONG IF WE ARE AT END OF LINE
C        ... OTHERWISE,
C ...       lpcurr = lpcurr + 1
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
