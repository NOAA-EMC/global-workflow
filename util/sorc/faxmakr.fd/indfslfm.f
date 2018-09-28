      SUBROUTINE INDFSLFM(FLDIN,IDIM,JDIM,NDIV,IBIG,JBIG,XINDEF)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INDFSLFM    TO SET UNDEFINED ON POLE AREA FOR TYPE 5 
C   PRGMMR: KRISHNA KUMAR       ORG: W/NP12    DATE: 1999-08-01
C
C ABSTRACT: TO SET UNDEFINED ON POLE AREA FOR TYPE 5 ACCORDING TO INPUT
C           BIT MAP FROM GETGB.
C
C PROGRAM HISTORY LOG:
C   97-07-11  LUKE LIN    
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM
C                           RS/6000. 
C 1999-12-20  KRISHNA KUMAR KBMS VALUE SHOULD BE SET TO LOGICAL*1
C
C USAGE:    CALL INDFSLFM(FLDIN,IDIM,JDIM,NDIV,IBIG,JBIG,XINDEF)
C   INPUT ARGUMENTS:
C     FLDIN    - REAL*4  BIGFLD(IBIG,JBIG)
C     IBIG   - I-DIMENSION OF BIGFLD.
C     JBIG   - J-DIMENSION OF BIGFLD.
C     IDIM   - I-DIMENSION OF THE ORIGINAL  SMLFLD.
C     JDIM   - J-DIMENSION OF THE ORIGINAL  SMLFLD.
C
C   OUTPUT ARGUMENT LIST:
C     FLDIN    - REAL*4  BIGFLD(IBIG,JBIG)
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C     *     *     *     *     *     *     *     *     *     *     *
C
      COMMON /BITM/   KBMS(66000)
      LOGICAL*1         KBMS
C
      COMMON/SEA/ SAN(53,57)
      INTEGER    SAN
C
      INTEGER    ZAN(3021)
      INTEGER    ZSAN(53,57)
      EQUIVALENCE (ZAN(1),ZSAN(1,1))
C
      INTEGER    LAN(209,225)
C
      REAL       FLDIN(IBIG,JBIG)
      REAL       XINDEF
C
C     DATA  LAN 
C    1  /209*0,209*0,209*0,209*0,209*0,209*0,209*0,209*0,209*0,209*0,
C    2   209*0,209*0,209*0,209*0,209*0,209*0,209*0,209*0,209*0,209*0,
C    3   209*0,209*0,209*0,209*0,209*0,209*0,209*0,209*0,209*0,209*0,
C    4   209*0,209*0,209*0,209*0,209*0,209*0,209*0,209*0,209*0,209*0,
C    1   209*1,209*1,
C    2   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    3   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    4   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    5   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    6   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    7   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    8   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    9   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    A   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    1   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    2   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    3   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    4   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    5   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    6   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    7   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    8   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    9   209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,209*1,
C    A   209*1,209*1,209*1/
C
C     DATA  SAN    /
C    X   53*0, 53*0, 53*0, 53*0, 53*0, 53*0, 53*0, 53*0, 53*1, 53*1,
C    X   53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1,
C    X   53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1,
C    X   53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1,
C    X   53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1,
C    X   53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1, 53*1/
C
      PRINT *, ' IN SUBROUTINE - INDFSLFM FOR TYPE 5'
      PRINT *, ' IDIM=', IDIM, '  JDIM=',JDIM
      PRINT *, ' IBIG=', IBIG, '  JBIG=',JBIG
C
      DO I=1, 3021
         IF (KBMS(I))  THEN
             ZAN(I) = 1
         ELSE
             ZAN(I) = 0
         ENDIF
      ENDDO
      print *, "ipgm indslfm, ichk=1"
C     .... LOAD THE BIT MAP
      DO I=1, 53
         DO J=1, 57
            SAN(I,J) = 0 
         ENDDO
      ENDDO
      print *, "ipgm indslfm, ichk=2"
C     ... INITIALIZE THE LABEL GRID
      DO I=1, 53
         DO J=2, 57
            IF (ZSAN(I,J) .EQ. 1) SAN(I,J-1) = 1
         ENDDO
      ENDDO
      print *, "ipgm indslfm, ichk=3"
C     .... SET THE BIT MAP ACCORDINGLY
      DO I=1, IBIG
         DO J=1, JBIG
             LAN(I,J) = 1
         ENDDO
      ENDDO
      print *, "ipgm indslfm, ichk=4"
C     ...... INITIALIZE THE CONTOUR GRID
C
      DO I=1, 53
         DO J=1, 57
            IF (ZSAN(I,J) .EQ.0) THEN
               IIS = 4 * (I -1) -6
               IF (IIS .LT. 1) IIS = 1
               IIE = 4 * I + 6
               IF (IIE .GT. 209) IIE = 209
               IJE = 228 - (4 * (J -1) +1)+5
               IF (IJE .GT. 225) IJE = 225
               IJS = 228 - (4 * J) -5 
               IF (IJS .LT. 1) IJS = 1
C              PRINT *, ' UNDEFINE: I=', I,' J=',J
C       PRINT *, ' IIS=', IIS,' IIE=',IIE, ' IJS=',IJS,' IJE=',IJE
               DO IK= IIS, IIE
                  DO JL=IJS, IJE
                     LAN(IK,JL) = 0
                  ENDDO
               ENDDO
            ENDIF
         ENDDO
      ENDDO
      print *, "ipgm indslfm, ichk=5"
C     .... LOAD THE BIT MAP
      DO I=1, IBIG
         DO J=1, JBIG
            IF( LAN(I,J).EQ.0) FLDIN(I,J) = XINDEF
         ENDDO
      ENDDO
      print *, "ipgm indslfm, ichk=5"
C     .... SET UNDEFINED ON THE INPUT GRID ACCORDING TO BIT MAP
C     DO J=1,225
C     PRINT *,' J=', J
C        WRITE(*,140)(NINT(FLDIN(M,J)),M=1,209)
C     ENDDO
C140  format( 5(1x,30(f1.0, 1x),/), 27(f1.0,1x),/)
C140  format( 5(40(1x,  i1),/), 9( 1x,i1),/)
C
C
C     DO J=1,57 
C     PRINT *,' J=', J
C        WRITE(*,150)(SAN(M,J),M=1,53)
C     ENDDO
 150  format( 3(10(1x,  i1),3x),/, 2(10( 1x,i1),3x),3(1x,i1),/)
C
      RETURN
      END
