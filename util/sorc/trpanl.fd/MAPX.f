      SUBROUTINE MAPX
C                                                                       00359700
C     ... TO COMPUTE IMAGE SCALE FOR MERCATOR MAP IN XM(I)              00359800
C     ...   WHERE IMAGE SCALE IS COS(STDLAT) / COS(LAT)                 00359900
C                                                                       00360000
C     ... AND CORIOLIS PARAMETER IN F(I)                                00360100
C     ...   WHERE CORIOLIS PARAMETER = 2*OMEGA*SIN(LAT)                 00360200
C                                                                       00360300
C     ...   FOR 117*51 MERCATOR GRID ...                                00360400
      COMMON /FIXFLD/ XM(51),F(51)
C                                                                       00360600
      DATA     OMEGA / 7.292116E-05 /
C     ... WHERE OMEGA IS ANGULAR VELOCITY OF THE EARTH (IN RADIANS/SEC) 00360800
      DATA     RADPDG / 0.01745329 /
      DATA     STDLAT / 22.5 /
      DATA     IEQUAT / 26 /
      DATA     IMAX / 51 /
      DATA     JMAXGI / 116 /
C                                                                       00361400
      DEGPGI = 360.0 / FLOAT(JMAXGI)
      COSSTD = COS(STDLAT*RADPDG)
      XM(IEQUAT) = COSSTD
C     ... WHERE 26TH ITEM IS EQUATOR...                                 00361800
C     ...   COS(STDLAT) / COS(0)                                        00361900
      F(IEQUAT) = 0.0
C     ... WHERE THE CORIOLIS PARAMETER IS ZERO AT THE EQUATOR ...       00362100
      M1 = IEQUAT + 1
      DO  10  I = M1,IMAX
      XI = I - IEQUAT
      XI = XI * 2.0 * DEGPGI
      XI = EXP(XI*RADPDG)
      SINLAT = (XI - 1.0) / (XI + 1.0)
      COSLAT = SQRT(1.0 - SINLAT*SINLAT)
      XM(I) = COSSTD / COSLAT
      F(I) = 2.0 * OMEGA * SINLAT
   10 CONTINUE
C     ... TO FILL IN THE SOUTHERN LATITUDES ...                         00363200
      M2 = IEQUAT - 1
      DO  20  I = 1,M2
      K = IMAX+ 1 - I
      F(I) = -F(K)
      XM(I) = XM(K)
   20 CONTINUE
      RETURN
      END
