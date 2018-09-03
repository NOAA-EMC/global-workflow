      SUBROUTINE INTERP (FLD,imax,jmax,RESULT,STI,STJ,KQUAD)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    INTERP      INTERPOLATE IN GRID FIELD TO STN I/J
C   PRGMMR: SHIMOMURA        ORG: W/NP12     DATE: 1997-01-07
C
C ABSTRACT: INTERPOLATE WITHIN THE GIVEN GRIDPOINT FIELD TO THE
C   I/J OF THE STATION AND RETURN WITH THE ONE INTERPOLATED VALUE
C   IN RESULT.  THE GIVEN KQUAD ARGUMENT TELLS ME THE CONFIGURATION
C   OF THE GRIDPOINTS SURROUNDING THE STATION I/J; WHETHER IT IS AT
C   THE BOUNDARY; WHETHER IT IS AT A SLANTING BOUNDARY.  FROM THE
C   KQUAD VALUE I WILL PERFORM BI-QUADRATIC INTERPOLATION FOR INTERIOR
C   POINTS; OR BI-LINEAR INTERPOLATION AT FIRST INTERIOR POINT AT EDGE
C   OF GRID WITH A 4-POINT SQUARE AVAILABLE; OR AVERAGE OF 3 POINTS 
C   AT SLANTING BOUNDARY.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL AUTHOR: BEDIENT
C   97-01-07  SHIMOMURA: CONVERT TO RUN ON CRAY
C
C USAGE:    CALL INTERP (FLD,imax,jmax,RESULT,STI,STJ,KQUAD)
C   INPUT ARGUMENT LIST:
C     REAL FLD(IMAX,JMAX) - THE GIVEN 2-DIMENSIONAL GRIDPOINT FIELD
C                           WITHIN WHICH I WILL INTERPOLATE;
C     REAL STI,STJ - THE LOCATION OF THE POINT (IN GRID UNITS) 
C                           TO WHICH I WILL INTERPOLATE;
C     INT  KQUAD - THE CONFIGURATION OF GRIDPOINTS SURROUNDING
C                           THE GIVEN LOCATION 
C     ...         =1: LL slanting edge 3-pts triangle ...
C     ...         =2: LR slanting edge 3-pts triangle ...
C     ...         =3: UR slanting edge 3-pts triangle ...
C     ...         =4: UL slanting edge 3-pts triangle ...
C     ...         =5: 4-pt square for 1st interior square at edge...
C     ...         =6: 16-pt (4pt x 4pt) square  for well inside ...
C
C   OUTPUT ARGUMENT LIST:
C     REAL RESULT - WILL CONTAIN THE INTERPOLATED VALUE AT THE 
C                   LOCATION SPECIFIED BY (STI,STJ)
C
C
C REMARKS: 
C   CAUTION:  ACCEPTABLE VALUES OF KQUAD= [1,2,3,4,5,6]
C             IF YOU GIVE ME A NOT-ACCEPTABLE VALUE, THEN 
C               I WILL DO-NOTHING RETURN; WITH RESULT=0.0
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$

C     ...            
      real       FLD(imax,jmax)
      real       RESULT
      real       STI,STJ
      integer    KQUAD

      integer    I,J
      real       FI,FJ
      real       XDELI,XDELJ
      real       ERAS(4)
      real       DI

      RESULT = 0.0
      I=STI
      J=STJ
      FI=I
      FJ=J
      XDELI=STI-FI
      XDELJ=STJ-FJ

      IF(KQUAD .LE. 0) GO TO 999
      IF(KQUAD .GT. 6) GO TO 999

      GO TO (1,2,3,4,5,6),KQUAD

C     ... case_1: LL slanting edge 3-pts triangle ...
    1 DI=(FLD(I+1,J)+FLD(I+1,J+1)+FLD(I,J+1))/3.
      GO TO 10

C     ... case_2: LR slanting edge 3-pts triangle ...
    2 DI=(FLD(I,J)+FLD(I,J+1)+FLD(I+1,J+1))/3.
      GO TO 10

C     ... case_3: UR slanting edge 3-pts triangle ...
    3 DI=(FLD(I,J)+FLD(I+1,J)+FLD(I,J+1))/3.
      GO TO 10

C     ... case_4: UL slanting edge 3-pts triangle ...
    4 DI=(FLD(I,J)+FLD(I+1,J)+FLD(I+1,J+1))/3.
      GO TO 10

C     ... case_5: 4-pt square for 1st interior square at edge...
    5 ERAS(1)=FLD(I,J)
      ERAS(4)=FLD(I,J+1)
      ERAS(2)=ERAS(1)+(FLD(I+1,J)-ERAS(1))*XDELI
      ERAS(3)=ERAS(4)+(FLD(I+1,J+1)-ERAS(4))*XDELI
      DI=ERAS(2)+(ERAS(3)-ERAS(2))*XDELJ
      GO TO 10

C     ... case_6: 16-pt (4pt x 4pt) square  for well inside ...
    6 XI2TM=XDELI*(XDELI-1.)*.25
      XJ2TM=XDELJ*(XDELJ-1.)*.25
      J1=J-1
      DO 40 K=1,4
        ERAS(K) = FLD(I,J1) + (FLD(I+1,J1)-FLD(I,J1))*XDELI +  
     1           (FLD(I-1,J1)-FLD(I,J1)-FLD(I+1,J1)+FLD(I+2,J1))*XI2TM
        J1=J1+1
   40 continue

      DI = ERAS(2) + (ERAS(3)-ERAS(2))*XDELJ + 
     1               (ERAS(1)-ERAS(2)-ERAS(3)+ERAS(4))*XJ2TM

   10 continue
      RESULT=DI
      GO TO 999

  999 CONTINUE
      RETURN
      END
