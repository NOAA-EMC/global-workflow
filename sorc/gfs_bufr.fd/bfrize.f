        SUBROUTINE BFRIZE ( luntbl, lunbfr, sbset, iyr, imn, idy, ihr,
     +                      seqnam, seqflg, nseq, lvlwise, data, nlvl,
     +                      clist, npp, wrkd, iret )
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
C* BFRIZE
C*
C* This subroutine calls Jack Woollen's BUFR encoding routines to
C* write a BUFR message to an output file.  SBSET is the Mnemonic
C* for the TABLE A entry associated with this message.  It appears
C* in the table referenced by LUNTBL.  If LUNTBL = 0, the output
C* BUFR file is closed.
C*
C* The data in the array DATA are ordered according to the individual
C* elements of the Sequences given in SEQNAM.  The contents of SEQNAM
C* and SEQFLG and, consequently of DATA, are determined by the BUFR
C* table file referenced by LUNTBL.  Each entry in SEQNAM has a list of
C* parameters associated with it in the table.  This list is read from
C* the table and the number of parameters is determined.  This
C* information is stored in CLIST and NPP for future calls to BFRIZE.
C* If the parameters associated with the entry in SEQNAM exist on NLVL
C* levels, the corresponding array element of SEQFLG must be .true.;
C* otherwise, it is .false.
C*
C* Profile data in array DATA may be stored such that contiguous
C* elements are values of different parameters on the same level
C* (parameter-wise storage) or the same parameter on different levels
C* (level-wise storage).  If LVLWISE=.false. parameter-wise storage
C* is assumed; otherwise, LVLWISE=.true. and level-wise storage is
C* assumed.
C*
C* The example below shows the contents of SEQNAM, SEQFLG, and DATA
C* for a case when NLVL=3, LVLWISE=.true., and the table file has the
C* following entries for the Mnemonic Sequences:
C*
C* MNEMONIC | SEQUENCE
C*
C* MODELOUT | HDR {PROF} SFC
C* HDR      | RLAT RLON
C* PROF     | PRES TMPK
C* SFC      | PMSL PRCP
C*
C* SEQNAM and SEQFLG have the following assigned entries:
C*
C*      INDEX   SEQNAM  SEQFLG
C*        1      HDR    .false.
C*        2      PROF   .true.
C*        3      SFC    .false.
C*
C* DATA must contain the following values in this order:
C*
C*      DATA (1) = rlat         DATA (6)  = tmpk (1)
C*      DATA (2) = rlon         DATA (7)  = tmpk (2)
C*      DATA (3) = pres (1)     DATA (8)  = tmpk (3)
C*      DATA (4) = pres (2)     DATA (9)  = pmsl
C*      DATA (5) = pres (3)     DATA (10) = prcp
C*
C* The lower-case names above signify numerical values of the
C* parameters.  The values of multiple level parameters are stored
C* contiguously.
C*
C* To add a new output parameter, update the table file by adding the
C* Mnemonic for the parameter to an existing Sequence or by adding
C* a new Sequence.  If a new Sequence has been added, SEQNAM and
C* SEQFLG must be updated accordingly.  In any case, the new output
C* parameter value must be placed in the correct position within the
C* array DATA.
C*
C* CLIST contains the lists of parameter names for each element of
C* SEQNAM.  If CLIST (1) is blank, BFRHDR is called with SEQNAM and
C* SEQFLG as inputs to load the names of the parameters into CLIST;
C* otherwise, the names in CLIST are used.  For every element of
C* SEQNAM there is a corresponding element of CLIST.  For each element
C* of CLIST, there is a corresponding element of NPP giving the number
C* of parameter names in the list.
C*
C* DATA (i) = 10.E+10 is the missing value.
C*
C* WRKD is a scratch array and should be dimensioned the same size as
C* data.  WRKD is not used if LVLWISE=.false.
C*
C* BFRIZE ( LUNTBL, LUNBFR, SBSET, IYR, IMN, IDY, IHR,
C*          SEQNAM, SEQFLG, NSEQ, LVLWISE, DATA, NLVL, CLIST, NPP,
C*          WRKD, IRET )
C*
C* Input parameters:
C*      LUNTBL          INTEGER         Unit number of BUFR Table file
C*      LUNBFR          INTEGER         Unit number of BUFR data file
C*      SBSET           CHAR*           BUFR subset name
C*      IYR             INTEGER         4-digit year
C*      IMN             INTEGER         2-digit month
C*      IDY             INTEGER         2-digit day
C*      IHR             INTEGER         2-digit cycle hour
C*      SEQNAM (NSEQ)   CHAR*           Mnemonic Sequence names
C*      SEQFLG (NSEQ)   LOGICAL         Multi-level flag
C*      NSEQ            INTEGER         Number of Sequence names & flags
C*      LVLWISE         LOGICAL         Level-wise profile data flag
C*      DATA   (*)      REAL            Data array
C*      NLVL            INTEGER         Number of levels
C*
C* Input and Output parameters:
C*      CLIST  (NSEQ)   CHAR*           Parameter name lists
C*      NPP    (NSEQ)   INTEGER         Number of parameter names
C*
C* Output parameters:
C*      WRKD   (*)      REAL            Array of reordered profile data
C*      IRET            INTEGER         Return code
C*                                        0 = normal return
C**
C* Log:
C* K. Brill/NMC         05/94
C* K. Brill/NMC         06/94   Added LVLWISE, CLIST, NPP, WRKD
C   98-08-28  ROZWODOSKI  MADE CHANGES FOR Y2K COMPLIANCE.
C***********************************************************************
        REAL*8          data (*)
        INTEGER         npp (*), nlvl (*)
        CHARACTER*(*)   seqnam (*), sbset
        LOGICAL         seqflg (*), lvlwise
        CHARACTER*(*)   clist (*)
        REAL*8          wrkd (*)
C-----------------------------------------------------------------------
        iret = 0
c        print*,'Bufriz.f is creating bufr file'

C
C*      Close BUFR file if LUNTBL = 0.
C
        IF ( luntbl .eq. 0 ) THEN
            CALL CLOSBF ( lunbfr )
            RETURN
        END IF
C
C*      Check the status of the output BUFR file.
C
        CALL STATUS ( lunbfr, lun, iopn, imm )
        IF ( iopn .eq. 0 ) THEN
            CALL SETBLOCK(1)
            CALL OPENBF ( lunbfr, 'OUT', luntbl )
            CALL DATELEN ( 10 )
        END IF
C
C*      Open a new message.
C
        idate = iyr * 1000000 + imn * 10000 + idy * 100 + ihr
c        print *, 'Bufriz idate = ', idate
        CALL OPENMB ( lunbfr, sbset, idate )
C
C*      Create the parameter name lists if CLIST (1) is blank.
C
c        print *, 'clist (1) = ', clist(1)
c        print *, 'npp (1) = ', npp(1)
c        print *, 'seqnam (1) = ', seqnam(1)
c        print *, 'seqflg (1) = ', seqflg(1)
c        print *, 'nseq = ', nseq
        IF ( clist (1) .eq. ' ' ) THEN
            DO is = 1, nseq
                CALL BFRHDR ( luntbl, seqnam (is), seqflg (is),
     +                        clist (is), npp (is), iret )
                IF ( iret .ne. 0 ) RETURN
            END DO
        END IF
C
C*      Load the sequences.
C
        idpntr = 1
        indxlv = 0
        DO is = 1, nseq
            np = npp (is)
            IF ( seqflg (is) ) THEN
                indxlv = indxlv + 1
                IF ( lvlwise ) THEN
C
C*                  This is level-wise multi-level data.
C
                    istrt = idpntr
                    indx = 0
                    DO k = 1, nlvl (indxlv)
                        DO ip = 1, np
                            indx = indx + 1
                            wrkd ( indx ) =
     +                           data ( istrt + (ip-1) * nlvl (indxlv) )
                        END DO
                        istrt = istrt + 1
                    END DO
                    CALL UFBINT ( lunbfr, wrkd, np, nlvl (indxlv), 
     +                            irtrn, clist (is) )
                ELSE
C
C*                  This is parameter-wise multi-level data.
C
                    CALL UFBINT ( lunbfr, data (idpntr), np, 
     +                            nlvl (indxlv), irtrn, clist (is) )
                END IF
                idpntr = idpntr + np * nlvl (indxlv)
            ELSE
C
C*              This is single-level data.
C
                CALL UFBINT ( lunbfr, data (idpntr),
     +                        np, 1, irtrn, clist (is) )
                idpntr = idpntr + np
            END IF
        END DO
        CALL WRITSB ( lunbfr )
C*
        RETURN
        END
