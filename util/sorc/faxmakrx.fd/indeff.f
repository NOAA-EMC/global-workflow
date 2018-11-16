      SUBROUTINE INDEFF(FLDA,IBIG,JBIG,ISML,JSML,ISKP,JSKP,IFAC,XINDEF)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INDEFF      PLACE INDEFINATES IS THE CORNERS OF FIELD
C   PRGMMR: KRISHNA KUMAR         ORG: W/NP12    DATE: 1999-08-01
C
C ABSTRACT: PLACE INDEFINATES IN THE DESIRED CORNERS OF THE FIELD TO
C   ELIMINATE UNIMPORTANT CONTOURS ON THE FOUR CORNERS.
C
C PROGRAM HISTORY LOG:
C   79-07-03  ORIGINAL AUTHOR HENRICHSEN
C   89-11-27  HENRICHSEN    ADD NEW DOCBLOCK
C   92-07-07  HNERICHSEN    CONVERT TO FORTRAN 77
C   94-05-16  LUKE LIN      MODIFY TO FIT THE PROGRAM
C   94-12-22  LUKE LIN      CONVERT IT TO CFT-77.
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM RS/6000.
C
C USAGE:    CALL INDEFF(FLDA,IBIG,JBIG,ISML,JSML,ISKP,JSKP,IFAC,XINDEF)
C   INPUT ARGUMENT LIST:
C     IBIG     - MAX SIZE I OF THE FIELD.
C     JBIG     - MAX SIZE J OF THE FIELD.
C     ISML     - SMALL SIZE I OF THE FIELD.
C     JSML     - SMALL SIZE J OF THE FIELD.
C     ISKP     - SKIP I OF THE FIELD.
C     JSKP     - SKIP J OF THE FIELD.
C     IFAC     - IFAC IS THE NUMBER OF GRID UNITS TO COME IN FROM
C              - EACH CORNER AS END POINTS OF TRIANGLE TO FILL WITH
C              - INDEFFINATES.............
C
C   OUTPUT ARGUMENT LIST:
C     FLDA     - UNPACKED DATA FIELD WITH INDEFINATES
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
      REAL       FLDA(IBIG,JBIG)
      REAL       XINDEF
C
C
C . . . SET LIMITS OF LOOP TO DO LOWER LEFT CORNER ............
C
      PRINT 98, xindef
  98  format(1x,' in indef, xindef=', 2x, z16)
C
      print *, 'xindef=', xindef
      print *,' big/sml=', ibig,jbig,isml,jsml
      print *,' skp/fax=', iskp,jskp,ifac
      JSTART = JSKP + 1
      JEND = JSKP + IFAC + 1
      IEND = ISKP + IFAC + 1
      ISTART =  ISKP + 1
C
          DO 40 J=JSTART,JEND
               DO 30 I=ISTART,IEND
                  FLDA(I,J) = XINDEF
   30          CONTINUE
            IEND = IEND -1
   40     CONTINUE
C . . . DO LOWER RIGHT CORNER . . ..
C
      ISTART =  ISML + ISKP - IFAC
      IEND = ISML + ISKP
          DO 55 J=JSTART,JEND
              DO 50 I=ISTART,IEND
                 FLDA(I,J) = XINDEF
   50         CONTINUE
            ISTART = ISTART + 1
   55     CONTINUE
C
C . . . DO UPPER LEFT CORNER . . . .
C
      ISTART = 1 + ISKP
      IEND = 1 + ISKP
      JEND = JSML + JSKP
      JSTART = JSML - IFAC + JSKP
          DO 75 J=JSTART,JEND
              DO 70 I=ISTART,IEND
                 FLDA(I,J) = XINDEF
   70         CONTINUE
            IEND = IEND + 1
   75     CONTINUE
C
C . . . DO UPPER RIGHT CORNER . . . . . . . . . . . ......
C
      ISTART = ISML + ISKP
      IEND = ISML + ISKP
      JEND = JSML + JSKP
      JSTART = JSML - IFAC + JSKP
           DO 90 J=JSTART,JEND
               DO 80 I=ISTART,IEND
                  FLDA(I,J) = XINDEF
   80          CONTINUE
             ISTART = ISTART - 1
   90      CONTINUE
C
C...... DO BOTTOM PART
C
      ISTART = 1
      IEND = IBIG
      JSTART = 1
      JEND = JSKP
          DO 110 J=JSTART, JEND
             DO 100 I=ISTART, IEND
                 FLDA(I,J) = XINDEF
  100        CONTINUE
  110     CONTINUE
C
C...... DO UPPER PART
C
      ISTART = 1
      IEND = IBIG
      JSTART = JSKP + JSML + 1
      IF (JSTART .LE. JBIG) THEN
         JEND = JBIG
             DO 130 J=JSTART, JEND
                DO 120 I=ISTART, IEND
                    FLDA(I,J) = XINDEF
  120           CONTINUE
  130        CONTINUE
      ENDIF
C
C...... DO LEFT-SIDE PORTION
C
      ISTART = 1
      IEND = ISKP
      JSTART = 1
      JEND = JBIG
          DO 150 I=ISTART, IEND
             DO 140 J=ISTART, JEND
                 FLDA(I,J) = XINDEF
  140        CONTINUE
  150     CONTINUE
C
C...... DO RIGHT-SIDE PORTION
C
      ISTART = ISKP + ISML + 1
      IF (ISTART .LE. IBIG) THEN
          IEND = IBIG
          JSTART = 1
          JEND = JBIG
          DO 170 I=ISTART, IEND
             DO 160 J=JSTART, JEND
                 FLDA(I,J) = XINDEF
  160        CONTINUE
  170     CONTINUE
      ENDIF
      RETURN
      END
