       subroutine movch(nchars,c1sorc,iofsrc,c1dest,iofdst)
C                                                    18-Jul-1996/dss
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MOVCH       TO MOVE CHARS FROM C*1 ARRAY TO C*1 ARRAY
C   PRGMMR: SHIMOMURA        ORG: W/NP12     DATE: 96-07-18
C
C ABSTRACT: TO MOVE CHARACTERS FROM CHARACTER*1 SOURCE ARRAY TO
C   CHARACTER*1 DESTINATION ARRAY.
C   To move NCHARS-bytes; start fetching from c1sorc(iofsrc) 
C      ...               and start storing at c1dest(iofdst).
C
C PROGRAM HISTORY LOG:
C   96-07-18  ORIGINAL AUTHOR: DAVID SHIMOMURA
C
C USAGE:    CALL movch(nchars,c1sorc,iofsrc,c1dest,iofdst)
C   INPUT ARGUMENT LIST:
C     NCHARS   - COUNT OF CHARACTERS TO MOVE
C     C1SORC   - SOURCE ARRAY:      CHARACTER*1 C1SORC(*)
C     IOFSRC   - SUBSCRIPT WITHIN SOURCE TO START THE FETCHING FROM;
C                         C1SORC(IOFSRC)
C     IOFDST   - SUBSCRIPT WITHIN DESTINATION TO START STORING INTO;
C                         C1DEST(IOFDST) 
C
C   OUTPUT ARGUMENT LIST:
C     C1DEST   - DESTINATION ARRAY: CHARACTER*1 C1DEST(*)
C
C
C REMARKS:
C     CAUTION:  You must give me valid values of array subscripts.
C      ...    If you point beyond destination array, I will try to 
C      ...    to write out of bounds, since I do not know your
C      ...    array sizes.
C
C      ... the logic used herein: this is an envelope to call XMOVEX();
C      ...    which is in W3LIB.
C      ... This will permit me to keep the same call sequence
C      ...    as in my IBM movch() routine.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$

       integer      nchars
       character*1  c1sorc(*)
       integer      iofsrc
       character*1  c1dest(*)
       integer      iofdst

       call xmovex(c1dest(iofdst),c1sorc(iofsrc),nchars)

       return
       end
