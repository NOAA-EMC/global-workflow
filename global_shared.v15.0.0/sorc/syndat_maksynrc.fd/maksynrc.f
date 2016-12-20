C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: SYNDAT_MAKSYNRC   MAKE SYNDAT RECORD FROM HUMAN INPUT
C   PRGMMR: STOKES                ORG: NP23        DATE: 2013-03-15
C
C ABSTRACT: QUERIES HUMAN INPUT FOR INFORMATION TO CONSTRUCT TROPICAL
C           CYCLONE SYNTHETIC DATA RECORD AND WRITES RECORD TO FORTRAN
C           UNIT 51
C
C PROGRAM HISTORY LOG:
C 1997-06-26  S. J. LORD ---- ORIGINAL AUTHOR
C 1998-11-23  D. A. KEYSER -- FORTRAN 90 AND Y2K COMPLIANT
C 1998-12-30  D. A. KEYSER -- MODIFIED TO OUTPUT RECORDS CONTAINING A
C               4-DIGIT YEAR
C 2000-03-03  D. A. KEYSER -- CONVERTED TO RUN ON IBM-SP MACHINE
C 2013-03-15  D. C. STOKES -- Modified some stdout writes to display
C                             cleanly as part of WCOSS transition.
C
C USAGE:
C   INPUT FILES:
C    UNIT 05 - INPUT FILE FOR HUMAN (KEYBOARD ENTRY)
C
C   OUTPUT FILES:
C    UNIT 06 - STANDARD OUTPUT PRINT
C    UNIT 51 - SYNTHETIC DATA RECORD  (ONE PER RUN)
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:  - BEGINE ENDE MAKVIT NSEW
C     LIBRARY:
C      W3LIB:  - W3TAGB W3TAGE
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE  FORTRAN 90
C   MACHINE:  IBM-SP, IBM-iDataPlex
C
C$$$
      program SYNDAT_MAKSYNRC
      logical fstflg
      character rsmc*4,stmnam*9,stmid*3
      data iuntvi/51/,fstflg/.false./

        CALL W3TAGB('SYNDAT_MAKSYNRC',2013,0074,0000,'NP23   ')

      write(6,*) "Welcome to the Synthetic Data Record Maker"
      write(6,*) "+++  FORTRAN 90 / Y2K VERSION +++"
      write(6,*) "+++       03 March 2000       +++"
      write(6,*) "Please follow all directions carefully, paying"
      write(6,*) "careful attention to the Units as units"
      write(6,*) "conversions are hardwired"

      call begine
      write(6,*) 'Enter Storm Name (UPPER CASE)'
      read(5,1) stmnam
    1 format(a)
      write(6,2) stmnam
    2 format('  Storm name is:',a9)
      call ende

      call begine
      write(6,*) 'Enter Storm Identifier (e.g. 03P)'
      read(5,11) stmid
   11 format(a)
      write(6,12) stmid
   12 format('  Storm Identifier is:',a3)
      call ende

      call begine
      write(6,*) 'Enter Organization ID (e.g. NHC, JTWC)'
      read(5,11) rsmc
      write(6,13) rsmc
   13 format('  Organization Identifier is:',a4)
      call ende

      call begine
      write(6,*) 'Enter date (yyyymmdd)'
      read(5,*) idate
      write(6,*) 'Date is: ',idate
      call ende

      call begine
      write(6,*) 'Enter hour (hh)'
      read(5,*) ihour
      iutc=ihour*100
      write(6,*) 'Hour  is: ',ihour
      call ende

      call begine
      write(6,*) 'Enter storm latitude (negative for south)'
      read(5,*) stmlat
      write(6,'(x,a,f5.1)') 'Storm latitude is: ',stmlat
      call ende

      call begine
      write(6,*) 'Enter storm longitude (DEG EAST)'
      read(5,*) stmlon
      write(6,'(x,a,f5.1)') 'Storm longitude is: ',stmlon
      call ende

      call begine
      write(6,*) 'Enter storm direction (DEG FROM NORTH)'
      read(5,*) stmdir
      write(6,'(x,a,f4.0)') 'Storm direction is: ',stmdir
      call ende

      call begine
      write(6,*) 'Enter storm speed (KNOTS)'
      read(5,*) stmspd
      write(6,'(x,a,f6.2)') 'Storm speed is: ',stmspd
      stmspd=stmspd/1.94
      call ende

      call begine
      write(6,*) 'Enter storm central pressure (MB)'
      read(5,*) pcen
      write(6,'(x,a,f5.0)') 'Storm central pressure is: ',pcen
      call ende

      call begine
      write(6,*) 'Enter storm environmental pressure (MB)'
      read(5,*) penv
      write(6,'(x,a,f5.0)') 'Storm environmental pressure is: ',penv
      call ende

      call begine
      write(6,*) 'Enter estimated maximum wind (KNOTS)'
      read(5,*) vmax
      write(6,'(x,a,f4.0)') 'Estimated maximum wind (KNOTS) is: ',vmax
      vmax=vmax/1.94
      call ende

      call begine
      write(6,*) 'Enter estimated radius of outermost closed ',
     1'isobar (ROCI), i.e. size of the storm circulation (KM)'
      read(5,*) rmax
      write(6,'(x,a,f5.0)') 'Estimated ROCI (KM) is: ',rmax
      call ende

      call begine
      write(6,*) 'Enter estimated radius of maximum wind (KM)'
      read(5,*) rmw
      write(6,'(x,a,f5.0)') 
     1   'Estimated radius of maximum wind (KM) is: ',rmw
      call ende

      call begine
      call nsew
      write(6,*) 'Enter estimated radius of 15 m/s (35 knot) winds (KM)'
      write(6,*)
     1 'in each each of the following quadrants (e.g. 290 222 200 180)'
      write(6,*) 'Note: numbers must be separated by blanks'
      write(6,*) 'Note: numbers must be in the order NE SE SW NW and be'
     1 ,' separated by blanks'
      write(6,*) 'Note: enter all negative numbers to denote no ',
     1'estimate'
      read(5,*)  r15ne,r15se,r15sw,r15nw
      write(6,'(x,a,4f8.0)') 
     1   'Estimated radius of 15 m/s (35 knot) winds is: ',
     2           r15ne,r15se,r15sw,r15nw
      call ende

      call begine
      call nsew
      write(6,*) 'Enter estimated radius of 26 m/s (55 knot) winds (KM)'
      write(6,*)
     1 'in each each of the following quadrants (e.g. 50 50 50 50)'
      write(6,*) 'Note: numbers must be separated by blanks'
      write(6,*) 'Note: numbers must be in the order NE SE SW NW and be'
     1'separated by blanks'
      write(6,*) 'Note: enter all negative numbers to denote no ',
     1'estimate'
      read(5,*)  r26ne,r26se,r26sw,r26nw
      write(6,'(x,a,4f8.0)') 
     1   'Estimated radius of 26 m/s (35 knot) winds is: ',
     2           r26ne,r26se,r26sw,r26nw
      call ende

      call begine
      write(6,*) 'Enter estimated top of cyclonic circulation (mb)'
      read(5,*) ptop
      write(6,'(x,a,f7.1)') 
     1   'Estimated top of cyclonic circulation (mb) is: ',ptop
      call ende

      call begine
      write(6,*) 'Enter estimated latitude at maximum forecast time '
      write(6,*) '(negative for south)'
      write(6,*) 'Note: enter -99.0 to denote no estimate'
      read(5,*) fclat
      write(6,'(x,a,f5.1)') 
     1   'Estimated latitude at maximum forecast time is: ', fclat
      call ende

      call begine
      write(6,*) 'Enter estimated longitude at maximum forecast time '
      write(6,*) '(DEG EAST)'
      write(6,*) 'Note: enter a negative number to denote no estimate'
      read(5,*) fclon
      write(6,'(x,a,f5.1)') 
     1   'Estimated longitude at maximum forecast time is: ', fclon
      call ende

      call begine
      write(6,*) 'Enter maximum forecast time (hours, e.g. 72)'
      write(6,*) 'Note: enter a negative number to denote no estimate'
      read(5,*) fcstp
      write(6,'(x,a,f4.0)') 'Maximum forecast time is: ',fcstp
      call ende

      CALL MAKVIT(IUNTVI,IDATE,IUTC,STMLAT,STMLON,STMDIR,STMSPD,
     1            PCEN,PENV,RMAX,VMAX,RMW,R15NE,R15SE,R15SW,
     2            R15NW,PTOP,STMNAM,STMID,RSMC,FSTFLG,r26ne,
     3            r26se,r26sw,r26nw,fcstp,fclat,fclon)

        CALL W3TAGE('SYNDAT_MAKSYNRC')
      stop
      end
      SUBROUTINE BEGINE
      write(6,1)
    1 format(' ')
      write(6,11)
   11 format(' *******************************************************')
      return
      end

      SUBROUTINE ENDE
      write(6,1)
    1 format(' *******************************************************')
      write(6,11)
   11 format(' ')
      return
      end
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MAKVIT      CREATES TROP. CYCLONE VITAL. STAT. DATA
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 1998-12-30
C
C ABSTRACT: CREATES TROPICAL CYCLONE VITAL STATISTICS RECORDS FROM
C   RAW INFORMATION SUCH AS LATITUDE, LONGITUDE, MAX. WINDS ETC.
C
C PROGRAM HISTORY LOG:
C 1991-06-06  S. J. LORD ---- ORIGINAL AUTHOR
C 1998-11-23  D. A. KEYSER -- FORTRAN 90 AND Y2K COMPLIANT
C 1998-12-30  D. A. KEYSER -- MODIFIED TO OUTPUT RECORDS CONTAINING A
C               4-DIGIT YEAR
C
C USAGE:    CALL PGM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
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
C     DDNAME1  - GENERIC NAME & CONTENT
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C     DDNAME2  - GENERIC NAME & CONTENT AS ABOVE
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  CRAY, SGI
C
C$$$
      SUBROUTINE MAKVIT(IUNTVI,IDATE,IUTC,STMLAT,STMLON,STMDIR,STMSPD,
     1                  PCEN,PENV,RMAX,VMAX,RMW,R15NE,R15SE,R15SW,
     2                  R15NW,PTOP,STMNAM,STMID,RSMC,FSTFLG,r26ne,
     3                  r26se,r26sw,r26nw,fcstp,fclat,fclon)
C
      SAVE
C      
      CHARACTER *(*) RSMC,STMNAM,STMID
      LOGICAL FSTFLG
C
      PARAMETER (MAXCHR=129)
      PARAMETER (MAXVIT=22)
      PARAMETER (MAXTPC= 3)
C
      CHARACTER BUFIN*1,RSMCZ*4,STMIDZ*3,STMNMZ*9,FSTFLZ*1,STMDPZ*1,
     1          SHALO*1,MEDIUM*1,
     2          DEEP*1,LATNS*1,LONEW*1,FMTVIT*6,FMTMIS*4,BUFINZ*129,
     3          RELOCZ*1,STMTPC*1,EXE*1,
     7          latnsf,lonewf
C
      DIMENSION IVTVAR(MAXVIT),VITVAR(MAXVIT),VITFAC(MAXVIT),
     1          ISTVAR(MAXVIT),IENVAR(MAXVIT),STMTOP(0:MAXTPC)
C
      DIMENSION BUFIN(MAXCHR),STMTPC(0:MAXTPC),FMTVIT(MAXVIT),
     1          MISSNG(MAXVIT),FMTMIS(MAXVIT)
C
      EQUIVALENCE (BUFIN(1),RSMCZ),(BUFIN(5),RELOCZ),(BUFIN(6),STMIDZ),
     1            (BUFIN(10),STMNMZ),(BUFIN(19),FSTFLZ),
     2            (BUFIN(37),LATNS),(BUFIN(43),LONEW),
     3            (BUFIN(95),STMDPZ),(BUFIN(1),BUFINZ),
     4            (BUFIN(123),LATNSF),(BUFIN(129),LONEWF)
C
      EQUIVALENCE (IVTVAR(1),IDATEZ),(IVTVAR(2),IUTCZ)
C
      EQUIVALENCE (VITVAR( 3),STMLTZ),(VITVAR( 4),STMLNZ),
     1            (VITVAR( 5),STMDRZ),(VITVAR( 6),STMSPZ),
     2            (VITVAR( 7),PCENZ), (VITVAR( 8),PENVZ),
     3            (VITVAR( 9),RMAXZ), (VITVAR(10),VMAXZ),
     4            (VITVAR(11),RMWZ),  (VITVAR(12),R15NEZ),
     5            (VITVAR(13),R15SEZ),(VITVAR(14),R15SWZ),
     6            (VITVAR(15),R15NWZ),(VITVAR(16),R26NEZ),
     7            (VITVAR(17),R26SEZ),(VITVAR(18),R26SWZ),
     8            (VITVAR(19),R26NWZ),(VITVAR(20),FCSTPZ),
     9            (VITVAR(21),FCLATZ),(VITVAR(22),FCLONZ)
C
      EQUIVALENCE (STMTPC(0), EXE),(STMTPC(1),SHALO),(STMTPC(2),MEDIUM),
     1            (STMTPC(3),DEEP)
C
      DATA SHALO/'S'/,MEDIUM/'M'/,DEEP/'D'/,EXE/'X'/,
     2     VITFAC/2*1.0,2*0.1,1.0,0.1,14*1.0,2*0.1/,
     3     FMTVIT/'(I8.8)','(I4.4)','(I3.3)','(I4.4)',2*'(I3.3)',
     4            3*'(I4.4)','(I2.2)','(I3.3)',8*'(I4.4)','(I2.2)',
     5            '(I3.3)','(I4.4)'/,
     6     FMTMIS/'(I8)','(I4)','(I3)','(I4)',2*'(I3)',3*'(I4)',
     7            '(I2)','(I3)',8*'(I4)','(I2)','(I3)','(I4)'/,
     8     MISSNG/-9999999,-999,-99,-999,2*-99,3*-999,-9,-99,8*-999,-9,
     9     -99,-999/,
     O     ISTVAR/20,29,34,39,45,49,53,58,63,68,71,75,80,85,90, 97,102,
     O     107,112,117,120,125/,
     1     IENVAR/27,32,36,42,47,51,56,61,66,69,73,78,83,88,93,100,105,
     1     110,115,118,122,128/,
     3     STMTOP/-99.0,700.,400.,200./
C
      BUFINZ=' '
      RSMCZ=RSMC
cvvvvvy2k

C NOTE: This program OUTPUTS a record containing a 4-digit year - for
C       example:

C NHC  13L MITCH     19981028 1800 164N 0858W 270 010 0957 1008 0371 51 019 0278 0278 0185 0185 D
C 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345 ...
C          1         2         3         4         5         6         7         8         9 ...

C       This program will truncate the integer work containing the
C       date in the form yyyymmdd to the form yymmdd prior to writing
C       it into the output record.
cppppp
      print *, ' '
      print *, ' '
      print *, '==> tcvitals file can now contain a 4-digit year, so ',
     $ 'no conversion is necessary since 4-digit year is input'
      print *, ' '
      print *, ' '
cppppp
caaaaay2k
      IDATEZ=IDATE
      IUTCZ=IUTC
      STMNMZ=STMNAM
      STMIDZ=STMID
      STMLTZ=STMLAT
C
      IF(STMLTZ .GE. 0.0)  THEN
         LATNS='N'
      ELSE
         LATNS='S'
         STMLTZ=ABS(STMLTZ)
      ENDIF
C
      IF(STMLON .GE. 180.)  THEN
         STMLNZ=360.-STMLON
         LONEW='W'
C
      ELSE
         STMLNZ=STMLON
         LONEW='E'
      ENDIF
C
      IF(fclat .GE. 0.0)  THEN
         fclatz=fclat
         latnsf='N'
      ELSE if (fclat .gt. -90.)  then
         latnsf='S'
         fclatz=ABS(fclat)
c
      else
         latnsf='S'
         fclatz=-99.9
      ENDIF
C
      IF(fclon .GE. 180.)  THEN
         fclonz=360.-fclon
         lonewf='W'
C
      ELSE if (fclon .gt. 0.)  then
         fclonz=fclon
         lonewf='E'
c
      else
         fclonz=-999.9
         lonewf='E'
      ENDIF
C
      STMDRZ=STMDIR
      STMSPZ=STMSPD
      PCENZ =PCEN
      PENVZ =PENV
      RMAXZ =RMAX
      VMAXZ =VMAX
      RMWZ  =RMW
      R15NEZ=R15NE
      R15SEZ=R15SE
      R15SWZ=R15SW
      R15NWZ=R15NW
      r26nez=r26ne
      r26sez=r26se
      r26swz=r26sw
      r26nwz=r26nw
      fcstpz=fcstp
C
      FSTFLZ=' '
      IF(FSTFLG)  FSTFLZ=':'
C
      DO IV=1,2
         IF(IVTVAR(IV) .GE. 0)  THEN
            WRITE(BUFINZ(ISTVAR(IV):IENVAR(IV)),FMTVIT(IV))  IVTVAR(IV)
         ELSE
            WRITE(BUFINZ(ISTVAR(IV):IENVAR(IV)),FMTMIS(IV))  MISSNG(IV)
         ENDIF
      ENDDO
C
      DO IV=3,MAXVIT
         IF(VITVAR(IV) .GE. 0)  THEN
            IVTVAR(IV)=NINT(VITVAR(IV)/VITFAC(IV))
            WRITE(BUFINZ(ISTVAR(IV):IENVAR(IV)),FMTVIT(IV))  IVTVAR(IV)
         ELSE
            WRITE(BUFINZ(ISTVAR(IV):IENVAR(IV)),FMTMIS(IV))  MISSNG(IV)
         ENDIF
      ENDDO
C
      DO ITOP=0,MAXTPC
         IF(PTOP .EQ. STMTOP(ITOP))  THEN
            STMDPZ=STMTPC(ITOP)
            GO TO 31
         ENDIF
      ENDDO

   31 CONTINUE
C
      IF(IUNTVI .GT. 0)  THEN
         WRITE(IUNTVI,41) BUFINZ
   41    FORMAT(A)
         WRITE(6,43) BUFINZ
   43    FORMAT(' ...',A,'...')
      ELSE
         WRITE(6,43) BUFINZ
      ENDIF
C
      RETURN
      END

      SUBROUTINE NSEW
      write(6,*)  ' Quadrants'
      write(6,*)  '  NW : NE'
      write(6,*)  '----------- Order of quadrants: NE SE SW NW'
      write(6,*)  '  SW : SE'
      return
      end
