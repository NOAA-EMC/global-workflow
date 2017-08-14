C-----------------------------------------------------------------------
      PROGRAM ANOMGB
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: ANOMGB       COMPUTE ANOMALIES WITH GRIB FILES
C   PRGMMR: IREDELL          ORG: NP23        DATE: 1998-10-22
C
C ABSTRACT: The command anomgb computes anomalies for fields read
C   from a GRIB file and outputs them to another GRIB file.
C   Unless otherwise directed (-x option) the GRIB index files
C   are also used to speed the reading.  The fields are interpolated
C   to an output grid if specified (-g option). The interpolation type
C   defaults to bilinear but may be specified directly (-i option).
C   The copying may be limited to specific fields (-k option).
C   The climatology GRIB file (-C option) must be specified;
C   its index file (-c option) is optional.
C
C PROGRAM HISTORY LOG:
C   97-01-13  IREDELL
C   97-03-18  IREDELL  INCREASED VERBOSITY
C   98-07-07  IREDELL  Y2K COMPATIBILITY
C
C COMMAND LINE OPTIONS:
C   -c climindex   
C      Optional index file for the climatology GRIB file.
C
C   -C climgrib    
C      GRIB file used to get the climatology when difftype=5
C      or 'anomaly'.  At present the climatology must be
C      expressed in monthly averages.
C
C   -g "grid [kgds]"
C      Verification grid identification.  If grid=-1
C      (the default), then the grid is taken from the first
C      GRIB field in GRIB file 1.  If grid=-2,
C      then the grid is taken from the first GRIB field
C      in GRIB file 2.  IF grid=-3, then the grid is taken
C      from the first GRIB field in the climatology.
C      If 0<grid<255, then grid designates an NCEP grid.
C      If grid=255, then the grid must be specified by the
C      full set of kgds parameters determining a GRIB GDS
C      (grid description section) in the W3FI63 format.
C
C   -i "ip [ipopts]"
C      Interpolation options.  The default is bilinear
C      interpolation (ip=0).  Other interpolation options
C      are bicubic (ip=1), neighbor (ip=2), budget (ip=3),
C      and spectral (ip=4).  See the documentation for iplib
C      for further details about interpolation.
C
C   -k "kpds"
C      Full set of kpds parameters determing a GRIB PDS
C      (product definition section) in the W3FI63 format
C      determining the field(s) from GRIB file 1
C      for which statistics are computed.  Note that
C      kpds(5) is the parameter indicator (PDS octet 9).
C      A wildcard is specified by -1 (the defaults).
C      If the -k is not specified, then anomgb will attempt
C      to compare every field in GRIB file 1.
C
C   -x 
C      Turns off the use of index files.  The index records
C      are then extracted from the GRIB file(s), which
C      will increase the time taken by anomgb.
C
C   -X
C      Turns on verbose printout.  This option is incompatible
C      with GRIB output to standard output (designated as '-').
C
C INPUT FILES:
C   UNIT   11    GRIB FILE 1
C   UNIT   13    CLIMATOLOGY GRIB FILE
C   UNIT   31    GRIB INDEX FILE 1
C   UNIT   33    CLIMATOLOGY GRIB INDEX FILE
C
C OUTPUT FILES:
C   UNIT   51    GRIB FILE 2
C
C SUBPROGRAMS CALLED:
C   IARGC
C   GETARG
C   ERRMSG
C   EUSAGE
C   ERREXIT
C   FPARSEI
C   FPARSER
C   BAOPENR
C   AOGB
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      CHARACTER*256 CARG,CG1,CX1,CGC,CXC,CG2
      INTEGER KARG(100)
      INTEGER KGDSI(200),IPOPT(20),JPDS1(200)
      CHARACTER*400 GDS
      DATA IGI/-1/,KGDSI/19*0,255,180*0/
      DATA IP/0/,IPOPT/20*-1/
      DATA JPDS1/200*-1/
      DATA LXX/0/,LX/1/
      DATA CGC/' '/,CXC/' '/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PARSE COMMAND LINE OPTIONS
      NARG=IARGC()
      IARG=1
      LSTOPT=0
      DOWHILE(IARG.LE.NARG.AND.LSTOPT.EQ.0)
        CALL GETARG(IARG,CARG)
        LARG=LEN_TRIM(CARG)
        IARG=IARG+1
        IF(CARG(1:1).NE.'-') THEN
          LSTOPT=1
          IARG=IARG-1
        ELSEIF(LARG.EQ.1) THEN
          CALL ERRMSG('anomgb: invalid option -')
          CALL EUSAGE
          CALL ERREXIT(1)
        ELSE
          L=2
          DOWHILE(L.LE.LARG)
            IF(CARG(L:L).EQ.'-') THEN
              LSTOPT=1
            ELSEIF(CARG(L:L).EQ.'c') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              LCXC=LARG-L
              CXC=CARG(L+1:LARG)
              L=LARG
            ELSEIF(CARG(L:L).EQ.'C') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              LCGC=LARG-L
              CGC=CARG(L+1:LARG)
              L=LARG
            ELSEIF(CARG(L:L).EQ.'g') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              KARG(1)=IGI
              KARG(2:100)=KGDSI(1:99)
              CALL FPARSEI(CARG(L+1:LARG),100,KARG)
              IGI=KARG(1)
              IF(IGI.GT.0.AND.IGI.LT.255) THEN
                CALL MAKGDS(IGI,KGDSI,GDS,LGDS,IRET)
                IF(IRET.NE.0) IGI=0
              ELSEIF(IGI.EQ.255) THEN
                KGDSI(1:99)=KARG(2:100)
              ENDIF
              IF(IGI.LT.-4.OR.IGI.EQ.0.OR.IGI.GT.255) THEN
                CALL ERRMSG('anomgb: invalid output grid '//
     &                      CARG(L+1:LARG))
                CALL EUSAGE
                CALL ERREXIT(1)
              ENDIF
              MI=LENGDS(KGDSI)
              IF(MI.LE.0) THEN
                CALL ERRMSG('anomgb: unsupported output grid '//
     &                      CARG(L+1:LARG))
                CALL EUSAGE
                CALL ERREXIT(1)
              ENDIF
              L=LARG
            ELSEIF(CARG(L:L).EQ.'i') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              KARG(1)=IP
              KARG(2:21)=IPOPT
              CALL FPARSEI(CARG(L+1:LARG),21,KARG)
              IP=KARG(1)
              IPOPT=KARG(2:21)
              L=LARG
            ELSEIF(CARG(L:L).EQ.'k') THEN
              IF(L.EQ.LARG) THEN
                L=0
                CALL GETARG(IARG,CARG)
                LARG=LEN_TRIM(CARG)
                IARG=IARG+1
              ENDIF
              CALL FPARSEI(CARG(L+1:LARG),100,JPDS1)
              IF(JPDS1(5).EQ.0) THEN
                CALL ERRMSG('anomgb: invalid PDS parms '//
     &                      CARG(L+1:LARG))
                CALL EUSAGE
                CALL ERREXIT(1)
              ELSEIF(JPDS1(5).EQ.-1) THEN
                JPDS1(5)=007
              ELSEIF(JPDS1(5).NE.007.AND.JPDS1(5).NE.222) THEN
                CALL ERRMSG('anomgb: only fields 007 or 222 allowed')
                CALL ERREXIT(1)
              ENDIF
              L=LARG
            ELSEIF(CARG(L:L).EQ.'x') THEN
              LX=0
            ELSEIF(CARG(L:L).EQ.'X') THEN
              LXX=1
            ELSE
              CALL ERRMSG('anomgb: invalid option '//CARG(L:L))
              CALL EUSAGE
              CALL ERREXIT(1)
            ENDIF
            L=L+1
          ENDDO
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PARSE COMMAND LINE POSITIONAL ARGUMENTS
      NXARG=LX+2
      IF(NARG-IARG+1.NE.NXARG) THEN
        CALL ERRMSG('anomgb: incorrect number of arguments')
        CALL EUSAGE
        CALL ERREXIT(NXARG)
      ENDIF
      LG1=11
      CALL GETARG(IARG,CG1)
      LCG1=LEN_TRIM(CG1)
      IARG=IARG+1
      CALL BAOPENR(LG1,CG1(1:LCG1),IRETBA)
      IF(IRETBA.NE.0) THEN
        CALL ERRMSG('anomgb:  error accessing file '//CG1(1:LCG1))
        CALL ERREXIT(8)
      ENDIF
      IF(LX.GT.0) THEN
        LX1=31
        CALL GETARG(IARG,CX1)
        LCX1=LEN_TRIM(CX1)
        IARG=IARG+1
        CALL BAOPENR(LX1,CX1(1:LCX1),IRETBA)
        IF(IRETBA.NE.0) THEN
          CALL ERRMSG('anomgb:  error accessing file '//CX1(1:LCX1))
          CALL ERREXIT(8)
        ENDIF
      ELSE
        LX1=0
      ENDIF
      CALL GETARG(IARG,CG2)
      LCG2=LEN_TRIM(CG2)
      IARG=IARG+1
      IF(CG2(1:LCG2).EQ.'-') THEN
        LG2=6
        IF(LXX.GT.0) THEN
          CALL ERRMSG('anomgb:  piping incompatible with the X option')
          CALL ERREXIT(1)
        ENDIF
      ELSE
        LG2=51
        CALL BAOPEN(LG2,CG2(1:LCG2),IRETBA)
        IF(IRETBA.NE.0) THEN
          CALL ERRMSG('anomgb:  error accessing file '//CG2(1:LCG2))
          CALL ERREXIT(8)
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  OPEN CLIMATOLOGY GRIB FILE
      LGC=13
      IF(CGC(1:1).EQ.' ') THEN
        CALL ERRMSG('anomgb: unspecified climatology')
        CALL ERREXIT(1)
      ENDIF
      CALL BAOPENR(LGC,CGC(1:LCGC),IRETBA)
      IF(IRETBA.NE.0) THEN
        CALL ERRMSG('anomgb:  error accessing file '//CGC(1:LCGC))
        CALL ERREXIT(8)
      ENDIF
      IF(CXC(1:1).NE.' ') THEN
        LXC=33
        CALL BAOPENR(LXC,CXC(1:LCXC),IRETBA)
        IF(IRETBA.NE.0) THEN
          CALL ERRMSG('anomgb:  error accessing file '//CXC(1:LCXC))
          CALL ERREXIT(8)
        ENDIF
      ELSE
        LXC=0
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GO
      IF(LXX.GT.0) THEN
        CALL W3TAGB('ANOMGB  ',1998,0295,0046,'NP23   ')
      ENDIF
      CALL AOGB(LG1,LX1,LGC,LXC,LG2,
     &          IGI,KGDSI,IP,IPOPT,JPDS1,LXX)
      IF(LXX.GT.0) THEN
        CALL W3TAGE('ANOMGB  ')
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE EUSAGE
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    EUSAGE      PRINT PROPER USAGE TO STDERR
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: PRINT PROPER USAGE TO STDERR.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL EUSAGE
C
C SUBPROGRAMS CALLED:
C   ERRMSG
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL ERRMSG('Usage: anomgb'//
     & ' -C climgrib [-c climindex]')
      CALL ERRMSG('             '//
     & ' [-g "grid [kgds]"] [-i "ip [ipopts]"]')
      CALL ERRMSG('             '//
     & ' [-k "kpds"] [-X]')
      CALL ERRMSG('       then either:')
      CALL ERRMSG('             '//
     & ' grib1 index1 grib2')
      CALL ERRMSG('            or:')
      CALL ERRMSG('             '//
     & ' -x grib1 grib2')
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE AOGB(LG1,LX1,LGC,LXC,LG2,
     &                IGI,KGDSI,IP,IPOPT,JPDS1,LXX)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    AOGB        COMPUTE GRIB ANOMALIES
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: COMPUTE GRIB ANOMALIES.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL AOGB(LG1,LX1,LGC,LXC,LG2,
C    &                IGI,KGDSI,IP,IPOPT,JPDS1,LXX)
C   INPUT ARGUMENTS:
C     LG1          INTEGER UNIT NUMBER FOR GRIB FILE 1
C     LX1          INTEGER UNIT NUMBER FOR GRIB INDEX FILE 1
C     LGC          INTEGER UNIT NUMBER FOR GRIB FILE CLIM
C     LXC          INTEGER UNIT NUMBER FOR GRIB INDEX FILE CLIM
C     LG2          INTEGER UNIT NUMBER FOR GRIB FILE 2
C     IGI          INTEGER OUTPUT GRID IDENTIFICATION
C     KGDSI        INTEGER (200) OUTPUT GRID PARAMETERS
C     IP           INTEGER INTERPOLATION TYPE
C     IPOPT        INTEGER (20) INTERPOLATION OPTIONS
C     JPDS1        INTEGER (100) KPDS SEARCH OPTIONS
C     LXX          INTEGER FLAG FOR VERBOSE OUTPUT
C
C SUBPROGRAMS CALLED:
C   GETGBMH
C   AOGB1
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      PARAMETER(MBUF=256*1024)
      CHARACTER CBUF1(MBUF),CBUFC(MBUF)
      INTEGER JPDS1(100)
      INTEGER KGDSI(200)
      INTEGER IPOPT(20)
      INTEGER JPDS(200),JGDS(200),KPDS1(200),KGDS1(200)
      INTEGER KPDSC(200),KGDSC(200)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ GRIB HEADERS
      JPDS=-1
      JGDS=-1
      KRC=-1
      KPDS1=0
      KGDS1=0
      CALL GETGBMH(LGC,LXC,KRC,JPDS,JGDS,
     &             MBUF,CBUFC,NLENC,NNUMC,MNUMC,
     &             KC,MC,KRC,KPDSC,KGDSC,IRET)
      IF(LXX.GT.0) THEN
        IF(IRET.EQ.99) THEN
          PRINT *,'anomgb climatology field not found'
        ELSEIF(IRET.NE.0) THEN
          PRINT *,'anomgb climatology field retrieval error code ',IRET
        ENDIF
      ENDIF
      IF(IRET.EQ.0) THEN
        KR1=-1
        KPDS1=0
        KGDS1=0
        CALL GETGBMH(LG1,LX1,KR1,JPDS1,JGDS,
     &               MBUF,CBUF1,NLEN1,NNUM1,MNUM1,
     &               K1,M1,KR1,KPDS1,KGDS1,IRET)
        IF(LXX.GT.0) THEN
          IF(IRET.EQ.99) THEN
            PRINT *,'anomgb field not found'
          ELSEIF(IRET.NE.0) THEN
            PRINT *,'anomgb header retrieval error code ',IRET
          ENDIF
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  LOOP UNTIL DONE
      NO=0
      DOWHILE(IRET.EQ.0)
        IF(IGI.EQ.-1) THEN
          IGI=KPDS1(3)
          KGDSI=KGDS1
          MI=M1
        ELSE
          MI=LENGDS(KGDSI)
        ENDIF
        IF(IGI.GT.0.AND.IGI.LE.255) THEN
          MF=MAX(M1,MC)
          CALL AOGB1(LG1,LX1,M1,CBUF1,NLEN1,NNUM1,MNUM1,
     &               MBUF,MF,MI,
     &               IGI,KGDSI,IP,IPOPT,JPDS1,
     &               LGC,LXC,MC,CBUFC,NLENC,NNUMC,MNUMC,
     &               LG2,LXX,KR1-1,NO,IRET1)
        ENDIF
        KPDS1=0
        KGDS1=0
        CALL GETGBMH(LG1,LX1,KR1,JPDS1,JGDS,
     &               MBUF,CBUF1,NLEN1,NNUM1,MNUM1,
     &               K1,M1,KR1,KPDS1,KGDS1,IRET)
        IF(LXX.GT.0) THEN
          IF(IRET.NE.0.AND.IRET.NE.99) THEN
            PRINT *,'anomgb header retrieval error code ',IRET
          ENDIF
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LXX.GT.0) THEN
        PRINT *,'anomgb wrote ',NO,' total records'
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE AOGB1(LG1,LX1,M1,CBUF1,NLEN1,NNUM1,MNUM1,
     &                 MBUF,MF,MI,
     &                 IGI,KGDSI,IP,IPOPT,JPDS1,
     &                 LGC,LXC,MC,CBUFC,NLENC,NNUMC,MNUMC,
     &                 LG2,LXX,KS1,NO,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    AOGB1       COMPUTE ONE GRIB ANOMALY
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: COMPUTE ONE GRIB ANOMALY.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL AOGB1(LG1,LX1,M1,CBUF1,NLEN1,NNUM1,MNUM1,
C    &                 MBUF,MF,MI,
C    &                 IGI,KGDSI,IP,IPOPT,JPDS1,
C    &                 LGC,LXC,MC,CBUFC,NLENC,NNUMC,MNUMC,
C    &                 LG2,LXX,KS1,NO,IRET)
C   INPUT ARGUMENTS:
C     LG1          INTEGER UNIT NUMBER FOR GRIB FILE 1
C     LX1          INTEGER UNIT NUMBER FOR GRIB INDEX FILE 1
C     M1           INTEGER DIMENSION OF GRIB FIELD 1
C     CBUF1        CHARACTER (MBUF) INDEX BUFFER 1
C     NLEN1        INTEGER RECORD LENGTH OF INDEX BUFFER 1
C     NNUM1        INTEGER NUMBER OF RECORDS IN INDEX BUFFER 1
C     NLEN1        INTEGER LENGTH OF EACH INDEX RECORD 1
C     NNUM1        INTEGER NUMBER OF INDEX RECORDS 1
C     MNUM1        INTEGER NUMBER OF INDEX RECORDS 1 SKIPPED
C     MBUF         INTEGER DIMENSION OF INDEX BUFFERS
C     MF           INTEGER DIMENSION OF FIELD
C     MI           INTEGER DIMENSION OF OUTPUT GRID
C     IGI          INTEGER OUTPUT GRID IDENTIFICATION
C     KGDSI        INTEGER (200) OUTPUT GRID PARAMETERS
C     IP           INTEGER INTERPOLATION TYPE
C     IPOPT        INTEGER (20) INTERPOLATION OPTIONS
C     JPDS1        INTEGER (100) KPDS SEARCH OPTIONS
C     LGC          INTEGER UNIT NUMBER FOR GRIB FILE CLIM
C     LXC          INTEGER UNIT NUMBER FOR GRIB INDEX FILE CLIM
C     MC           INTEGER DIMENSION OF GRIB FIELD CLIM
C     CBUFC        CHARACTER (MBUF) INDEX BUFFER CLIM
C     NLENC        INTEGER RECORD LENGTH OF INDEX BUFFER CLIM
C     NNUMC        INTEGER NUMBER OF RECORDS IN INDEX BUFFER CLIM
C     NLENC        INTEGER LENGTH OF EACH INDEX RECORD CLIM
C     NNUMC        INTEGER NUMBER OF INDEX RECORDS CLIM
C     MNUMC        INTEGER NUMBER OF INDEX RECORDS CLIM SKIPPED
C     LG2          INTEGER UNIT NUMBER FOR GRIB FILE 2
C     LXX          INTEGER FLAG FOR VERBOSE OUTPUT
C     KS1          INTEGER INPUT RECORD COUNTER
C     NO           INTEGER OUTPUT RECORD COUNTER
C   OUTPUT ARGUMENTS:
C     NO           INTEGER OUTPUT RECORD COUNTER
C     IRET         INTEGER RETURN CODE
C
C SUBPROGRAMS CALLED:
C   GETGBM
C   GETCLIM
C   INTGRIB
C   PUTGB
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      CHARACTER CBUF1(MBUF),CBUFC(MBUF)
      INTEGER JPDS1(100)
      INTEGER KGDSI(200)
      INTEGER IPOPT(20)
      INTEGER JGDS(200),KPDS1(200),KGDS1(200)
      INTEGER KPDSC(200),KGDSC(200)
      LOGICAL*1 LR(MF),L1I(MI),LCI(MI)
      REAL FR(MF),F1I(MI),FCI(MI)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET FIELD FROM FILE 1
      JGDS=-1
      KPDS1=0
      KGDS1=0
      CALL GETGBM(LG1,LX1,M1,KS1,JPDS1,JGDS,
     &            MBUF,CBUF1,NLEN1,NNUM1,MNUM1,
     &            K1,KR1,KPDS1,KGDS1,LR,FR,IRET)
      IF(LXX.GT.0) THEN
        IF(IRET.NE.0) THEN
          PRINT *,'anomgb data retrieval error code ',IRET
        ELSE
          PRINT *,'anomgb read field from record ',KR1
          PRINT *,'       ...KPDS(1:16)=',(KPDS1(I),I=1,16)
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INTERPOLATE FIELD 1
      IF(IRET.EQ.0) THEN
        IB1=MOD(KPDS1(4)/64,2)
        CALL INTGRIB(0,IP,IPOPT,KGDS1,K1,IB1,LR,FR,GR,KGDSI,MI,
     &               IB1I,L1I,F1I,G1I,IRET)
        IF(LXX.GT.0) THEN
          IF(IRET.EQ.0) THEN
            PRINT *,'       interpolated to grid ',IGI
          ELSEIF(IRET.GT.0) THEN
            PRINT *,'       interpolation error code ',IRET
          ENDIF
        ENDIF
        IF(IRET.EQ.-1) IRET=0
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET CLIMATOLOGY
      IF(IRET.EQ.0) THEN
        CALL GETCLIM(LGC,LXC,MC,MBUF,CBUFC,NLENC,NNUMC,MNUMC,
     &               KPDS1,KPDSC,KGDSC,KC,LR,FR,IRET)
        IF(LXX.GT.0) THEN
          IF(IRET.EQ.0) THEN
            PRINT *,'       climatology retrieved'
            PRINT *,'       ...KPDS(1:16)=',(KPDSC(I),I=1,16)
          ELSEIF(IRET.EQ.99) THEN
            PRINT *,'       climatology not found for this field'
          ELSE
            PRINT *,'       climatology retrieval error code ',IRET
          ENDIF
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INTERPOLATE CLIMATOLOGY
      IF(IRET.EQ.0) THEN
        IBC=MOD(KPDSC(4)/64,2)
        CALL INTGRIB(0,IP,IPOPT,KGDSC,KC,IBC,LR,FR,GR,KGDSI,MI,
     &               IBCI,LCI,FCI,GCI,IRET)
        IF(LXX.GT.0) THEN
          IF(IRET.EQ.0) THEN
            PRINT *,'       interpolated to grid ',IGI
          ELSEIF(IRET.GT.0) THEN
            PRINT *,'       interpolation error code ',IRET
          ENDIF
        ENDIF
        IF(IRET.EQ.-1) IRET=0
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE ANOMALY AND WRITE OUTPUT FIELD
      IF(IRET.EQ.0) THEN
        DO I=1,MI
          IF(LCI(I).AND.L1I(I)) THEN
            F1I(I)=F1I(I)-FCI(I)
          ELSE
            IB1I=1
            L1I(I)=.FALSE.
          ENDIF
        ENDDO
        KPDS1(3)=IGI
        KPDS1(4)=128+64*IB1I
        IF(KPDS1(5).EQ.007) KPDS1(5)=027
        IF(KPDS1(5).EQ.222) KPDS1(5)=230
        CALL PUTGB(LG2,MI,KPDS1,KGDSI,L1I,F1I,IRET)
        IF(IRET.EQ.0) NO=NO+1
        IF(LXX.GT.0) THEN
          IF(IRET.NE.0) THEN
            PRINT *,'       packing error code ',IRET
          ELSE
            PRINT *,'       wrote field to record ',NO
            PRINT *,'       ...KPDS(1:16)=',(KPDS1(I),I=1,16)
          ENDIF
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE GETCLIM(LG,LX,M,MBUF,CBUF,NLEN,NNUM,MNUM,
     &                   KPDS1,KPDS,KGDS,K,L,F,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GETCLIM     GET CLIMATOLOGY
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: GET CLIMATOLOGICAL FIELD FROM MONTHLY CLIMATOLOGY.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL GETCLIM(LG,LX,M,MBUF,CBUF,NLEN,NNUM,MNUM,
C    &                   KPDS1,KPDS,KGDS,K,L,F,IRET)
C   INPUT ARGUMENTS:
C     LG           INTEGER UNIT OF THE UNBLOCKED GRIB DATA FILE
C     LX           INTEGER UNIT OF THE UNBLOCKED GRIB INDEX FILE
C     M            INTEGER MAXIMUM NUMBER OF DATA POINTS TO UNPACK
C     MBUF         INTEGER LENGTH OF INDEX BUFFER IN BYTES
C     CBUF         CHARACTER*1 (MBUF) INDEX BUFFER
C     NLEN         INTEGER LENGTH OF EACH INDEX RECORD IN BYTES
C     NNUM         INTEGER NUMBER OF INDEX RECORDS
C     MNUM         INTEGER NUMBER OF INDEX RECORDS SKIPPED
C     KPDS1        INTEGER (200) UNPACKED PDS PARAMETERS
C   OUTPUT ARGUMENTS:
C     CBUF         CHARACTER*1 (MBUF) INDEX BUFFER
C     NLEN         INTEGER LENGTH OF EACH INDEX RECORD IN BYTES
C     NNUM         INTEGER NUMBER OF INDEX RECORDS
C     MNUM         INTEGER NUMBER OF INDEX RECORDS SKIPPED
C     KPDS         INTEGER (200) UNPACKED PDS PARAMETERS
C     KGDS         INTEGER (200) UNPACKED GDS PARAMETERS
C     K            INTEGER NUMBER OF DATA POINTS UNPACKED
C     L            LOGICAL*1 (M) UNPACKED BITMAP IF PRESENT
C     F            REAL (M) UNPACKED DATA
C     IRET         INTEGER RETURN CODE
C                  
C SUBPROGRAMS CALLED:
C   W3MOVDAT
C   GETGBM 
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      CHARACTER CBUF(MBUF)
      INTEGER KPDS1(200),KPDS(200),KGDS(200)
      LOGICAL*1 L(M),L2(M)
      REAL F(M),F2(M)
      INTEGER JPDS(200),JGDS(200)
      INTEGER IDAT(8),JDAT(8)
      REAL RINC(5)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C DETERMINE VALID DAY OF YEAR
      IDAT=0
      IDAT(1)=(KPDS1(21)-1)*100+KPDS1(8)
      IDAT(2)=KPDS1(9)
      IDAT(3)=KPDS1(10)
      IDAT(5)=KPDS1(11)
      RINC=0
      IFR=MAX(KPDS1(14),KPDS1(15))
      IF(KPDS1(16).GE.2.AND.KPDS1(16).LE.5) THEN
        IFR=(KPDS1(14)+KPDS1(15)+1)/2
      ELSEIF(KPDS1(16).GE.115.AND.KPDS1(16).LE.116) THEN
        IFR=KPDS1(14)+(KPDS1(15)*(KPDS1(17)-1)+1)/2
      ENDIF
      IF(KPDS1(13).EQ.0) THEN
        RINC(2)=IFR/60.
      ELSEIF(KPDS1(13).EQ.1) THEN
        RINC(2)=IFR
      ELSEIF(KPDS1(13).EQ.2) THEN
        RINC(2)=IFR*24.
      ELSEIF(KPDS1(13).EQ.10) THEN
        RINC(2)=IFR*3.
      ELSEIF(KPDS1(13).EQ.11) THEN
        RINC(2)=IFR*6.
      ELSEIF(KPDS1(13).EQ.12) THEN
        RINC(2)=IFR*12.
      ENDIF
      CALL W3MOVDAT(RINC,IDAT,JDAT)
      IMON=JDAT(2)
      IDAY=JDAT(3)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C GET AND INTERPOLATE CLIMATOLOGY
      JPDS=-1
      JPDS(5)=KPDS1(5)
      JPDS(6)=KPDS1(6)
      JPDS(7)=KPDS1(7)
      JPDS(9)=IMON
      JPDS(16)=51
      JGDS=-1
      KR=0
      KPDS=0
      KGDS=0
      CALL GETGBM(LG,LX,M,KR,JPDS,JGDS,
     &            MBUF,CBUF,NLEN,NNUM,MNUM,
     &            K,KR,KPDS,KGDS,L,F,IRET)
      IF(IRET.EQ.0.AND.IDAY.NE.15) THEN
        KR=0
        JPDS=KPDS
        JGDS=KGDS
        JPDS(9)=MOD(IMON+ISIGN(1,IDAY-15)+11,12)+1
        CALL GETGBM(LG,LX,M,KR,JPDS,JGDS,
     &              MBUF,CBUF,NLEN,NNUM,MNUM,
     &              K,KR,KPDS,KGDS,L2,F2,IRET)
        IF(IRET.EQ.0) THEN
          DO I=1,K
            IF(L(I).AND.L2(I)) F(I)=F(I)+ABS(IDAY-15)/30.*(F2(I)-F(I))
          ENDDO
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE INTGRIB(IV,IP,IPOPT,KGDS1,K1,IB1,L1,F1,G1,KGDS2,K2,
     &                   IB2,L2,F2,G2,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    INTGRIB     INTERPOLATE FIELD
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: INTERPOLATE FIELD.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL INTGRIB(IV,IP,IPOPT,KGDS1,K1,IB1,L1,F1,G1,KGDS2,K2,
C    &                   IB2,L2,F2,G2,IRET)
C   INPUT ARGUMENTS:
C     IV           INTEGER VECTOR FLAG
C     IP           INTEGER INTERPOLATION TYPE
C     IPOPT        INTEGER (20) INTERPOLATION OPTIONS
C     KGDS1        INTEGER (200) INPUT GRID PARAMETERS
C     K1           INTEGER INPUT DIMENSION
C     IB1          INTEGER INPUT BITMAP FLAG
C     L1           LOGICAL*1 (K1) INPUT BITMAP IF IB1=1
C     F1           REAL (K1) INPUT FIELD
C     G1           REAL (K1) INPUT Y-COMPONENT IF IV=1
C     KGDS2        INTEGER (200) OUTPUT GRID PARAMETERS
C     K2           INTEGER OUTPUT DIMENSION
C     IB2          INTEGER OUTPUT BITMAP FLAG
C     L2           LOGICAL*1 (K2) OUTPUT BITMAP
C     F2           REAL (K2) OUTPUT FIELD
C     G2           REAL (K2) OUTPUT Y-COMPONENT IF IV=1
C
C SUBPROGRAMS CALLED:
C   LENGDSF
C   INTGRIB1
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      INTEGER IPOPT(20)
      INTEGER KGDS1(200),KGDS2(200)
      LOGICAL*1 L1(K1),L2(K2)
      REAL F1(K1),F2(K2)
      REAL G1(K1),G2(K2)
      INTEGER KGDS1F(200),KGDS2F(200)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE WHETHER INTERPOLATION IS NECESSARY
      IF(IP.EQ.4) THEN
        INT=1
      ELSE
        INT=0
        DO I=1,200
          INT=MAX(INT,ABS(KGDS1(I)-KGDS2(I)))
        ENDDO
        INT=MIN(INT,1)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COPY FIELD
      IF(INT.EQ.0) THEN
        IB2=IB1
        DO I=1,K1
          L2(I)=L1(I)
          F2(I)=F1(I)
          IF(IV.NE.0) G2(I)=G1(I)
        ENDDO
        IRET=-1
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE REGULARIZED GRIDS AND INTERPOLATE FIELD
      ELSE
        K1F=LENGDSF(KGDS1,KGDS1F)
        IF(K1F.EQ.K1) K1F=1
        K2F=LENGDSF(KGDS2,KGDS2F)
        IF(K2F.EQ.K2) K2F=1
        MRL=MAX(K2,K2F)
        IF(IV.EQ.0) THEN
          MRO=1
        ELSE
          MRO=MRL
        ENDIF
        IF(K1F.GT.0.AND.K2F.GT.0) THEN
          CALL INTGRIB1(K1F,KGDS1F,K2F,KGDS2F,MRL,MRO,
     &                  IV,IP,IPOPT,KGDS1,K1,IB1,L1,F1,G1,KGDS2,K2,
     &                  IB2,L2,F2,G2,IRET)
        ELSE
          IRET=101
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE INTGRIB1(K1F,KGDS1F,K2F,KGDS2F,MRL,MRO,
     &                    IV,IP,IPOPT,KGDS1,K1,IB1,L1,F1,G1,KGDS2,K2,
     &                    IB2,L2,F2,G2,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    INTGRIB1    INTERPOLATE FIELD
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: INTERPOLATE FIELD.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL INTGRIB1(K1F,KGDS1F,K2F,KGDS2F,MRL,MRO,
C    &                    IV,IP,IPOPT,KGDS1,K1,IB1,L1,F1,G1,KGDS2,K2,
C    &                    IB2,L2,F2,G2,IRET)
C   INPUT ARGUMENTS:
C     K1F          INTEGER REGULARIZED INPUT DIMENSION
C     KGDS1F       INTEGER (200) REGULARIZED INPUT GRID PARAMETERS
C     K2F          INTEGER REGULARIZED OUTPUT DIMENSION
C     KGDS2F       INTEGER (200) REGULARIZED OUTPUT GRID PARAMETERS
C     MRL          INTEGER DIMENSION OF RLAT AND RLON
C     MRO          INTEGER DIMENSION OF CROT AND SROT
C     IV           INTEGER VECTOR FLAG
C     IP           INTEGER INTERPOLATION TYPE
C     IPOPT        INTEGER (20) INTERPOLATION OPTIONS
C     KGDS1        INTEGER (200) INPUT GRID PARAMETERS
C     K1           INTEGER INPUT DIMENSION
C     IB1          INTEGER INPUT BITMAP FLAG
C     L1           LOGICAL*1 (K1) INPUT BITMAP IF IB1=1
C     F1           REAL (K1) INPUT FIELD
C     G1           REAL (K1) INPUT Y-COMPONENT IF IV=1
C     KGDS2        INTEGER (200) OUTPUT GRID PARAMETERS
C     K2           INTEGER OUTPUT DIMENSION
C     IB2          INTEGER OUTPUT BITMAP FLAG
C     L2           LOGICAL*1 (K2) OUTPUT BITMAP
C     F2           REAL (K2) OUTPUT FIELD
C     G2           REAL (K2) OUTPUT Y-COMPONENT IF IV=1
C
C SUBPROGRAMS CALLED:
C   IPOLATES
C   IPOLATEV
C   IPXWAFS2
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      INTEGER IPOPT(20)
      INTEGER KGDS1(200),KGDS2(200)
      LOGICAL*1 L1(K1),L2(K2)
      REAL F1(K1),F2(K2),G1(K1),G2(K2)
      INTEGER KGDS1F(200),KGDS2F(200)
      LOGICAL*1 L1F(K1F),L2F(K2F)
      REAL F1F(K1F),F2F(K2F),G1F(K1F),G2F(K2F)
      REAL RLAT(MRL),RLON(MRL),CROT(MRO),SROT(MRO)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  REGLR TO REGLR SCALAR
      IF(K1F.EQ.1.AND.K2F.EQ.1.AND.IV.EQ.0) THEN
        CALL IPOLATES(IP,IPOPT,KGDS1,KGDS2,K1,K2,1,IB1,L1,F1,
     &                KI,RLAT,RLON,IB2,L2,F2,IRET)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  IRREG TO REGLR SCALAR
      ELSEIF(K1F.NE.1.AND.K2F.EQ.1.AND.IV.EQ.0) THEN
        CALL IPXWAFS2(1,K1,K1F,1,
     &                KGDS1,IB1,L1,F1,KGDS1F,IB1F,L1F,F1F,IRET)
        IF(IRET.EQ.0) THEN
          CALL IPOLATES(IP,IPOPT,KGDS1F,KGDS2,K1F,K2,1,IB1F,L1F,F1F,
     &                  KI,RLAT,RLON,IB2,L2,F2,IRET)
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  REGLR TO IRREG SCALAR
      ELSEIF(K1F.EQ.1.AND.K2F.NE.1.AND.IV.EQ.0) THEN
        CALL IPOLATES(IP,IPOPT,KGDS1,KGDS2F,K1,K2F,1,IB1,L1,F1,
     &                KI,RLAT,RLON,IB2F,L2F,F2F,IRET)
        IF(IRET.EQ.0) THEN
          CALL IPXWAFS2(-1,K2,K2F,1,
     &                  KGDS2,IB2,L2,F2,KGDS2F,IB2F,L2F,F2F,IRET)
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  IRREG TO IRREG SCALAR
      ELSEIF(K1F.NE.1.AND.K2F.NE.1.AND.IV.EQ.0) THEN
        CALL IPXWAFS2(1,K1,K1F,1,
     &                KGDS1,IB1,L1,F1,KGDS1F,IB1F,L1F,F1F,IRET)
        IF(IRET.EQ.0) THEN
          CALL IPOLATES(IP,IPOPT,KGDS1F,KGDS2F,K1F,K2F,1,IB1F,L1F,F1F,
     &                  KI,RLAT,RLON,IB2F,L2F,F2F,IRET)
          IF(IRET.EQ.0) THEN
            CALL IPXWAFS2(-1,K2,K2F,1,
     &                    KGDS2,IB2,L2,F2,KGDS2F,IB2F,L2F,F2F,IRET)
          ENDIF
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  REGLR TO REGLR VECTOR
      ELSEIF(K1F.EQ.1.AND.K2F.EQ.1.AND.IV.NE.0) THEN
        CALL IPOLATEV(IP,IPOPT,KGDS1,KGDS2,K1,K2,1,IB1,L1,F1,G1,
     &                KI,RLAT,RLON,CROT,SROT,IB2,L2,F2,G2,IRET)
        IF(IRET.EQ.0.AND.KI.EQ.K2-1) THEN
          F2(K2)=0
          G2(K2)=0
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  IRREG TO REGLR VECTOR
      ELSEIF(K1F.NE.1.AND.K2F.EQ.1.AND.IV.NE.0) THEN
        CALL IPXWAFS2(1,K1,K1F,1,
     &                KGDS1,IB1,L1,F1,KGDS1F,IB1F,L1F,F1F,IRET)
        CALL IPXWAFS2(1,K1,K1F,1,
     &                KGDS1,IB1,L1,G1,KGDS1F,IB1F,L1F,G1F,IRET)
        IF(IRET.EQ.0) THEN
          CALL IPOLATEV(IP,IPOPT,KGDS1F,KGDS2,K1F,K2,1,
     &                  IB1F,L1F,F1F,G1F,
     &                  KI,RLAT,RLON,CROT,SROT,IB2,L2,F2,G2,IRET)
          IF(IRET.EQ.0.AND.KI.EQ.K2-1) THEN
            F2(K2)=0
            G2(K2)=0
          ENDIF
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  REGLR TO IRREG VECTOR
      ELSEIF(K1F.EQ.1.AND.K2F.NE.1.AND.IV.NE.0) THEN
        CALL IPOLATEV(IP,IPOPT,KGDS1,KGDS2F,K1,K2F,1,IB1,L1,F1,G1,
     &                KI,RLAT,RLON,CROT,SROT,IB2F,L2F,F2F,G2F,IRET)
        IF(IRET.EQ.0) THEN
          CALL IPXWAFS2(-1,K2,K2F,1,
     &                  KGDS2,IB2,L2,F2,KGDS2F,IB2F,L2F,F2F,IRET)
          CALL IPXWAFS2(-1,K2,K2F,1,
     &                  KGDS2,IB2,L2,G2,KGDS2F,IB2F,L2F,G2F,IRET)
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  IRREG TO IRREG VECTOR
      ELSEIF(K1F.NE.1.AND.K2F.NE.1.AND.IV.NE.0) THEN
        CALL IPXWAFS2(1,K1,K1F,1,
     &                KGDS1,IB1,L1,F1,KGDS1F,IB1F,L1F,F1F,IRET)
        CALL IPXWAFS2(1,K1,K1F,1,
     &                KGDS1,IB1,L1,G1,KGDS1F,IB1F,L1F,G1F,IRET)
        IF(IRET.EQ.0) THEN
          CALL IPOLATEV(IP,IPOPT,KGDS1F,KGDS2F,K1F,K2F,1,
     &                  IB1F,L1F,F1F,G1F,
     &                  KI,RLAT,RLON,CROT,SROT,IB2F,L2F,F2F,G2F,IRET)
          IF(IRET.EQ.0) THEN
            CALL IPXWAFS2(-1,K2,K2F,1,
     &                    KGDS2,IB2,L2,F2,KGDS2F,IB2F,L2F,F2F,IRET)
            CALL IPXWAFS2(-1,K2,K2F,1,
     &                    KGDS2,IB2,L2,G2,KGDS2F,IB2F,L2F,G2F,IRET)
          ENDIF
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      FUNCTION LENGDSF(KGDS,KGDSF)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    LENGDSF     RETURN THE LENGTH OF A FILLED GRID
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 96-07-19
C
C ABSTRACT: GIVEN A GRID DESCRIPTION SECTION (IN W3FI63 FORMAT),
C   RETURN THE GRID DESCRIPTION SECTION AND SIZE OF ITS REGULARIZED
C   COUNTERPART.  THAT IS, IF THE INPUT GRID IS REGULAR, THEN ITSELF
C   IS RETURNED ALONG WITH ITS GRID SIZE; HOWEVER IF THE INPUT GRID IS
C   ONLY QUASI-REGULAR (SUCH AS THE WAFS GRIDS), THEN ITS FILLED REGULAR
C   VERSION IS RETURNED ALONG WITH ITS FILLED GRID SIZE.
C
C PROGRAM HISTORY LOG:
C   96-07-19  IREDELL
C
C USAGE:    CALL LENGDSF(KGDS,KGDSF)
C   INPUT ARGUMENTS:
C     KGDS         INTEGER (200) GDS PARAMETERS IN W3FI63 FORMAT
C   OUTPUT ARGUMENTS:
C     KGDSF        INTEGER (200) REGULAR GDS PARAMETERS IN W3FI63 FORMAT
C     LENGDSF      INTEGER SIZE OF REGULARIZED GRID
C
C SUBPROGRAMS CALLED:
C   IPXWAFS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      INTEGER KGDS(200),KGDSF(200)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(KGDS(1).EQ.201) THEN
        KGDSF=KGDS
        LENGDSF=KGDS(7)*KGDS(8)-KGDS(8)/2
      ELSEIF(KGDS(1).EQ.202) THEN
        KGDSF=KGDS
        LENGDSF=KGDS(7)*KGDS(8)
      ELSEIF(KGDS(20).NE.255) THEN
        CALL IPXWAFS(1,1,1,0,KGDS,DUM,KGDSF,DUMF,IRET)
        IF(IRET.EQ.0) THEN
          LENGDSF=KGDSF(2)*KGDSF(3)
        ELSE
          LENGDSF=0
        ENDIF
      ELSE
        KGDSF=KGDS
        LENGDSF=KGDS(2)*KGDS(3)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
