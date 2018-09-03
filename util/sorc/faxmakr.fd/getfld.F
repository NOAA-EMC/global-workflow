#if !defined (_BIG_ENDIAN) && !defined (_LITTLE_ENDIAN)
#error 'Neither _BIG_ENDIAN nor _LITTLE_ENDIAN is defined.'
#endif
      SUBROUTINE GETFLD(NUMFLD,LABEL,LUGRB,LUGRBIX,IEXIT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: GETFLD        INVENTORY GRIB EDITION 1 FILE
C   AUTHOR: KRISHNA KUMAR       ORG: WP12       DATE: 1999-08-01
C
C ABSTRACT: READ ONE DEGREE GRIB AND INDEX FILE ACCORDING TO INPUT
C   7-WORD ID.  EXTRACT THE GRIB AND CONVERT IT 65*65 AND SMOOTH IT
C   THEN STORE IT TO BUFFER AND GET A OFFICE 84  12-WORD ID.
C
C HISTORY LOG:
C   94-11-09  LUKE LIN
C   95-10-16  LUKE LIN      MODIFY TO USE IW3GETV
C   96-07-05  LUKE LIN      CHANGE UNPACK GRID BUFF SIZE FROM 163800 TO 16900
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM
C                           RS/6000. 
C
C INPUT ARGUMENTS:
C     LABEL    -  INTEGER*12 6-WORD OFFICE 84 ID
C     NUMFLD   - .EQ. 1  FOR ONE FIELD
C              - .GT. 1 FOR 2 OR MORE FIELDS.SAVE PREVIOUS FIELD
C     LUGRB    -  ONE DEGREE GRIB FILE UNIT
C     LUGRBIX  -  ONE DEGREE GRIB INDEX FILE UNIT
C
C OUTPUT ARGEMENTS:
C    IEXIT    -- RETURN STATUS
C              - =0, NORMAL;
C              - SEE SUBROUTINE IW3GETV.
C     BUFF     - BUFFER(16900) CONTAINS GRID DATA
C     BUFFV    - SAVE OF PREVIOUS BUFFER(16900) CONTAINS GRID DATA
C
C USAGE:
C   INPUT FILES:
C     FT05F001 -   CONTROL CARDS, ONE FOR EACH OUTPUT FILE
C
C   OUTPUT FILES:
C     FT06F001 -   PRINT OUTPUT (STANDARD FORTRAN OUTPUT FILE)
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:     IW3GETV
C
C     LIBRARY:
C       SPECIAL  - NONE
C
C   REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C
C     COMMON /ISPACE/SBFLD1(16900),SBFLD2(16900),IDREC(12),BUFF(16900),
C    &      BUFFV(16900),BUFFU(16900)
C
      COMMON /ISPACE/SBFLD1(16900),SBFLD2(16900),IDREC(12),BUFF(16900),
     &               SAV1ST(16900),SAV2ND(16900)
      REAL   SBFLD1, SBFLD2, BUFF, SAV1ST, SAV2ND
C     ... 12 ID WORDS UNPACKED FROM 84'S DATA FILE; BUFF CONTAINS
C         UNPACKED DATA FOR THAT PARTICULAR FIELD;
C
      INTEGER LABEL(12)
C
C   THE HEADER CONSTANTS
C     COMMON / HDCONS / ITAU,IGRID,IGCODE,ISCALE
      COMMON/HDCONS/ITAU,IDUMMY(3)
C     ..... ITAU COMES FROM SUBR RD1GRID.F

C
      CHARACTER * 132 TITLE
      INTEGER         MAXBUF
      INTEGER         LUGRBIX, LUGRB
      CHARACTER*1     FCSTHR(8)
      INTEGER         LABEL1
      EQUIVALENCE     (LABEL1,FCSTHR(1))
C
C  ... MAXBUF IS THE MAXIMUN BUFFER SIZE...
       DATA MAXBUF /16900/
ckumar
      character*80 gribfile,gribixfile
      character*2 clugrb,clugrbix
#if defined (__INTEL_COMPILER)
      CHARACTER*6     envvar
#else
      CHARACTER * 11  envvar
#endif
ckumar
C
C***********************************************************************
C
      IERR = 0
      IF (NUMFLD .EQ. 2) THEN
C     ....SAVE PREVIOUS BUFFER IN BUFFV
         DO 10 I=1,MAXBUF
            SAV1ST(I) = BUFF(I)
   10    CONTINUE
      ENDIF
c
c*** kumar opening grib & index files here
c
      write(clugrb,fmt='(i2)')lugrb
      write(clugrbix,fmt='(i2)')lugrbix
      print*,'grib & index files unit #s ',clugrb,clugrbix
c
#if defined (__INTEL_COMPILER)
      envvar='FORT  '
      write(envvar(5:6),fmt='(I2)') lugrb
#else
      envvar='XLFUNIT_   '
      write(envvar(9:10),fmt='(I2)') lugrb
#endif
      call getenv(envvar,gribfile)

      call baopen(lugrb,gribfile,iret)
      print*,'iret after baopen grib_file   ',iret,gribfile

#if defined (__INTEL_COMPILER)
      envvar='FORT  '
      write(envvar(5:6),fmt='(I2)') lugrbix
#else
      envvar='XLFUNIT_   '
      write(envvar(9:10),fmt='(I2)') lugrbix
#endif
      call getenv(envvar,gribixfile)

      call baopen(lugrbix,gribixfile,iret)
      print*,'iret after baopen grib_index_file   ',iret,gribixfile
c
c*** kumar
c
C
      CALL IW3GETV(LUGRBIX, LUGRB, LABEL, BUFF, TITLE, IERR)
C
      IF (IERR.GE.96 .AND. IERR.LE.99) THEN
        PRINT *,'*IW3GETV ERROR = ',IERR
        IERR = 1
        IEXIT = 1
        GO TO 999
      END IF
C
ckumar         N = 1
ckumar      DO I=1,100
ckumar           WRITE(6,20)(BUFF(M),M=N, N+7)
ckumar 20        FORMAT(8(1X,F15.6))
ckumar           N=N+8
ckumar      ENDDO
c
C
      print *,title(1:86)
      print *,' '
C
C
      print *, 'GETFLDS: NUMFLD=', NUMFLD
      IF (NUMFLD .EQ. 1) THEN
         DO I=1, 12
            IDREC(I) = LABEL(I)
         ENDDO
         LABEL1 = LABEL(1)
#if defined (_LITTLE_ENDIAN)
         ITAU   = mova2i(FCSTHR(1))
#else    
         ITAU   = mova2i(FCSTHR(8))
#endif
         print *,' itau=', itau
      ENDIF
C
      PRINT *, 'GETFLDS: IDREC '
      WRITE(6,30)(IDREC(M),M=1,12)
  30  FORMAT (6(1X,Z16))
c
c*** kumar closing grib & grib index files here
c
       call baclose(lugrb,iret)
       call baclose(lugrbix,iret)
c
c*** kumar
c
  999 CONTINUE
C
      RETURN
      END
