      SUBROUTINE WNDPLT(GRIDU,GRIDV,IMAX,JMAX)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    WNDPLT      PLOT WIND DIRECTIONS
C   PRGMMR: KRISHNA KUMAR      ORG: W/NP12   DATE: 1999-08-01
C
C ABSTRACT: PLOT WIND DIRECTIONS ON EVERY GRID POINT BY USING
C   U & V COMPONENTS.
C
C PROGRAM HISTORY LOG:
C   90-11-05  ORIGINAL AUTHOR  LUKE LIN
C   95-01-04  LUKE LIN      CONVERT IT CFT-77.
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM
C                           RS/6000.
C
C USAGE:    CALL WNDPLT(GRIDU,GRIDV,IMAX,JMAX)
C   INPUT ARGUMENT LIST:
C     GRIDU    - U COMPONENTS.
C     GRIDV    - V COMPONENTS.
C     IMAX     - SIZE OF ARRAY GRIDU/V AT I.
C     JMAX     - SIZE OF ARRAY GRIDU/V AT J.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM
C
C$$$
C
      COMMON /ICON/ UA1(50),UA2(50),UA3(50),UM1(50),UM2(50),UM3(50),
     1       KTYPE(50)
      CHARACTER*4 KTYPE
      REAL        UA1,UA2,UA3,UM1,UM2,UM3
      COMMON /MUTCON/ KEYIDX,UA1V,UA2V,UA3V,UM1V,UM2V,UM3V,LINEVU,LINEP,
     X               IGRIDP,T1
C     ...KEY TYPE INDEX, CONSTANT, AND CONTOUR LINE VALUE
      REAL        GRIDU(IMAX,JMAX)
      REAL        GRIDV(IMAX,JMAX)
      REAL        CNVRD
C
      DATA        CNVRD    /.01745329/
C
      IPX = 0
      IPY = 0
      IENTRY = 1
      IDELTA = 0
      VA3V = UA3(40)
      VM3V = UM3(40)
C     .... INITIALIZATION
      DO 500 J=2, JMAX-1
         NSKIP = MOD(J,2)
         YY = (J-1) * T1
         DO 500 I=2, IMAX-1
            IF (IGRIDP .EQ. 26) THEN
                NSKIP = NSKIP + 1
                IF (MOD(NSKIP,2).EQ.0)  GO TO 500
            ENDIF
            XX = (I-1) * T1
            IF (IPX.EQ.0 .OR. IPY.EQ.0) GO TO 500
            XU=GRIDU(I,J)
            YV=GRIDV(I,J)
            IF (YV.EQ.0.) GO TO 500
            IF (XU.GE.0.) THEN
               ALPHA = 270.
            ELSE
               ALPHA = 90.
            ENDIF
C
            RADINS = ATAN2(ABS(YV),ABS(XU))
            WNDIR = RADINS / CNVRD
            ASIGN = XU * YV
            IF (ASIGN .GE. 0.0) THEN
                IWNDIR = NINT(ALPHA - WNDIR)
            ELSE
                IWNDIR = NINT(ALPHA + WNDIR)
            ENDIF
            IF (IGRIDP .EQ. 27) THEN
                IWNDIR = IWNDIR - 25
            ENDIF
            IF  (IWNDIR .GT. 360) THEN
                IWNDIR = IWNDIR - 360
            ELSE IF (IWNDIR .LT. 0) THEN
                IWNDIR = IWNDIR + 360
            ENDIF
            KWNDIR = NINT(FLOAT(IWNDIR)/10.)
C
            SPEED = SQRT(XU*XU+YV*YV)
            SPEED = (SPEED + VA3V) * VM3V
            IF(SPEED.LE.0.0) SPEED = 0.0
            KNOTS = NINT(SPEED)
            IF (KNOTS.EQ.0) KWNDIR=0
C           ...REGARDLESS WIND DIRECTION IF WIND SPEED LESS THAN 1.
C     PRINT *,'  I,J,XU,YV,IWNDIR=',I,J,IPX,IPY,XU,YV,KNOTS,IWNDIR
C
            IENTRY = IENTRY + 1
  500 CONTINUE
      RETURN
      END
