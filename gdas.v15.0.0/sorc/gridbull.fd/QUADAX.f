C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    QUADAX      Extracts qudrant from grid
C   PRGMMR: BOSTELMAN        ORG: W/NMC421   DATE: 96-05-23
C
C ABSTRACT: Extracts desired quadrant from input field
C
C PROGRAM HISTORY LOG:
C   96-06-12  W.J. BOSTELMAN
C
C USAGE:    CALL  QUADAX (FLDS, FLDB, NQ)
C
C   INPUT ARGUMENT LIST:
C    FLDSA - 1 deg lat-lon grid field
C    NQ    - Area number of quadrant
C
C   OUTPUT ARGUMENT LIST:
C    FLDB  - 5 deg lat-lon quadrant
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  NAS
C
C$$$
C
      SUBROUTINE  QUADAX (FLDA, FLDB, NQ)
C
      PARAMETER     (IPTS  = 360, JPTS = 181)
      PARAMETER     (IJOUT = 285)
C
      INTEGER       ILEFT (4), IRGHT (4)
      INTEGER       JBEGN (3), JEND (3), JINC(3)
C
      REAL          FLDA (IPTS, JPTS), FLDB(IJOUT)
C
      DATA  ILEFT / 271, 181,  91,  1 /
      DATA  IRGHT / 360, 271, 181, 91 /
      DATA  JBEGN / 71,  56, 111 /
      DATA  JEND  /  1, 126, 181 /
      DATA  JINC  / -5,   5,   5 /
      DATA  ISKIP /  5 /
C
      IJ  = 1
      NQJ = (NQ - 1) / 4  + 1
      NQI = MOD (NQ-1, 4) + 1
      DO J = JBEGN (NQJ), JEND (NQJ), JINC (NQJ)
        DO I = ILEFT (NQI), IRGHT (NQI), ISKIP
          FLDB (IJ) = FLDA (I,J)
          IJ = IJ + 1
        END DO
        IF (NQI .EQ. 1) THEN  ! COPY 0E TO 360E
          FLDB (IJ) = FLDA (1, J)
          IJ = IJ + 1
        ENDIF
      END DO
C
      RETURN
      END
