        SUBROUTINE BFRHDR ( luntbl, cseqn, prfflg, clist, np, iret )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PROGRAM NAME (up to 20 characters)
C   PRGMMR: YOUR NAME        ORG: W/NMCXX    DATE: YY-MM-DD
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  PLEASE PROVIDE A BRIEF DESCRIPTION OF
C   WHAT THE SUBPROGRAM DOES.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL PROGRAMMER'S NAME HERE
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C   YY-MM-DD  MODIFIER2   DESCRIPTION OF CHANGE
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$
C***********************************************************************
C* BFRHDR
C*
C* This subroutine reads a Jack Woollen BUFR encoding table file to
C* get the string of parameters to be written.  This subroutine is
C* given the sequence nmemonic and returns the list associated with it.
C* This list is a character string and is used as the last input to
C* UFBINT.
C*
C*
C* BFRHDR ( LUNTBL, CSEQN, PRFFLG, CLIST, NP, IRET )
C*
C* Input parameters:
C*      LUNTBL          INTEGER         Unit number of BUFR Table file
C*      CSEQN           CHAR*           Sequence mnemonic
C*      PRFFLG          LOGICAL         Flag for profile parms
C*                                        = .true. for multi-level parms
C*
C* Output parameters:
C*      CLIST           CHAR*           String of parm names
C*      NP              INTEGER         Number of parm names in string
C*      IRET            INTEGER         Return code
C*                                        0 = normal return
C*                                       -1 = Improper table file
C*                                       -2 = Sequence not found
C**
C* Log:
C* K. Brill/NMC         05/94
C***********************************************************************
C*
        CHARACTER*(*)           cseqn, clist
        LOGICAL                 prfflg
C*
        LOGICAL                 found
        CHARACTER*80            sbuf
C
C*      Set starting column number of parameter list in the table.
C
        DATA                    istart / 14 /
C-----------------------------------------------------------------------
        iret = 0
C
C*      Count the number of lines to end of file (used to reposition
C*      pointer to original line at the end).
C
        found = .true.
        lcnt = 1
        DO WHILE ( found )
            READ ( luntbl, 1000, IOSTAT=ios ) sbuf
1000        FORMAT (A)
            IF ( ios .ne. 0 ) THEN
                found = .false.
            ELSE
                lcnt = lcnt + 1
            END IF
        END DO
C
C*      Read from the file for positioning.
C
        REWIND luntbl
        found = .false.
        DO WHILE ( .not. found )
            READ (luntbl, 1000, IOSTAT=ios ) sbuf
            IF ( ios .ne. 0 ) THEN
                iret = -1
                RETURN
            END IF
            iq1 = INDEX ( sbuf, '| REFERENCE' )
            iq2 = INDEX ( sbuf, '| UNITS' )
            iq = iq1 * iq2
            IF ( iq .ne. 0 ) found = .true.
        END DO
C
C*      Get length of sequence mnemonic string.
C
        lc = LEN ( cseqn )
        DO WHILE ( cseqn ( lc:lc ) .eq. ' ' )
            lc = lc-1
        END DO
C
C*      Start searching backward for the sequence mnemonic.
C
        found = .false.
        lenc=0
        DO WHILE ( .not. found )
            BACKSPACE luntbl
            READ ( luntbl, 1000, IOSTAT=ios ) sbuf
            IF ( ios .ne. 0 .or. sbuf (1:2) .eq. '.-' ) THEN
                iret = -2
                RETURN
            END IF
            BACKSPACE luntbl
            iq = INDEX ( sbuf ( 1:14 ), cseqn ( 1:lc ) )
            IF ( iq .ne. 0 ) THEN
                found = .true.
C
C*              Find the last character of last parameter.
C
                i = 79
                DO WHILE ( sbuf ( i:i ) .eq. ' ' )
                    i = i - 1
                END DO
                clist = ' '
                clist = sbuf ( istart:i )
C
C*              Count the number of entries in CLIST.
C
                lenc = i - istart + 1
                nspcs = 0
                np = 0
                DO j = 1, lenc
                    IF ( clist ( j:j ) .eq. ' ' ) nspcs = nspcs + 1
                END DO
                np = nspcs + 1
C
C*              Handle profile sequence.
C
                IF ( prfflg ) THEN
C                    sbuf = cseqn ( 1:lc ) // '^ ' // clist ( 1:lenc )
                    sbuf = clist ( 1:lenc )
                    clist = sbuf
                END IF
            END IF
        END DO
C
C*      Reposition file to original record.
C
        found = .true.
        DO WHILE ( found )
            READ ( luntbl, 1000, IOSTAT=ios ) sbuf
            IF ( ios .ne. 0 ) found = .false.
        END DO
        DO k = 1, lcnt
            BACKSPACE luntbl
        END DO
C*
        RETURN
        END
