      BLOCK DATA
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    BLOCKDAT    INITIALIZE THE ARRAYS IN COMMON ICON.
C   PRGMMR: KRISHNA KUMAR         ORG: W/NP12    DATE: 1999-08-01
C
C ABSTRACT: BLOCK DATA IS USED TO INITIALIZE THE CONSTANTS FOR UNPACKING
C   AND SCALING THE GRID POINT DATA FIELDS FOR CONTOURING AND LABELING
C   OF CONTOURS.SCALING THE GRID POINT FIELDS FOR CONTOURING AND
C
C PROGRAM HISTORY LOG:
C   94-09-19  ORIGINAL AUTHOR HENRICHSEN
C   94-09-23  HENRICHSEN   ADDED PRESSURE CHANGE AND HEIGHT CHANGE
C   94-09-29 HENRICHSEN    ADDED # 45 WHICH IS 50% RH.
C   94-10-03 HENRICHSEN    RENAMED LIFTED INDEX 21 & 29 TO
C                          TO BEST LIFTED INDEX "LIB". WHERE
C                          THIS WAS ITEM # 17 IN PEPFAX.
C   94-10-04 HENRICHSEN    ADDED TRAJECTORY TEMPS, VERTICAL DISPLACEMENT,
C                          AND KINDEX.
C   94-12-15 HENRICHSEN    CORRECTED LIFTED INDEX "LIB" TO "LI4" AND
C                          THE PREVIOUS "LI4" TO "LI '.
C   95-02-14 HENRICHSEN    REMOVE ITEM# 13 MAKE VORTICITY FROM U AND V
C                          FIELDS.
C   95-07-14 LUKE LIN      ADD 0-9 VECTORS.
C   96-09-12 LUKE LIN      ADD CGRID PRECIP CONSTANTS.
C   96-11-18 LUKE LIN      OPEN 41 FOR TROPOPAUSE STREAM FUNCTION.
C   97-03-12 LUKE LIN      OPEN 18 FOR BLENDED AVERAGE
C   97-12-04 LUKE LIN      CHANGE 39 FOR SFC GEO REL VORTICITY
C   97-12-10 C. CARUSO     CHANGE 17 FOR SFC GEO WIND
C 1999-08-01 KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM RS/6000
C
C   OUTPUT ARGUMENT LIST:
C     COMMON   - /ICON/ UA1(50),UA2(50),UA3(50),UM1(50),
C              -        UM2(50),UM3(50),KTYPE(50)
C
C REMARKS: THE "KTYPES" 26 AND ABOVE WILL PRODUCE MAPS WITH
C   TWICE AS MANY CONTOURS. FOR EXAMPLE KTYPE(1) WILL PRODUCE A MAP
C   WITH ISOBAR LINES EVERY FOUR MILLIBARS.
C   WHERE AS KTYPE(26) WILL PRODUCE A MAP WITH ISOBAR LINES EVERY
C   TWO MILLIBARS.
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
      COMMON /ICON/ UA1(50),UA2(50),UA3(50),UM1(50),UM2(50),UM3(50),
     1       KTYPE(50)
      CHARACTER*4 KTYPE
      REAL        UA1,UA2,UA3,UM1,UM2,UM3
C
      COMMON /MCON/ IADDB,IADDTAU,NUMTYP,NUMKEY,
     1              MTAU(18),MEAN(18),LMEAN,KDN,LDN
C
      INTEGER     NUMTYP
      INTEGER     MTAU
      CHARACTER*4 MEAN
      LOGICAL    LMEAN
      LOGICAL    KDN
      LOGICAL    LDN
C
      DATA        NUMTYP /13/
      DATA        MTAU/   -36,    12,    84,    96,   108,   120,
     1                    132,   144,   156,   168,   204,   216,
     2                    324,     0,     0,     0,     0,     0/
      DATA        MEAN/'D-3 ','D+0 ','D+3 ','D+3+','D+4 ','D+4+',
     1                 'D+5 ','D+5+','D+6 ','D+6+','D+8 ','D+8+',
     2                 'D+13','    ','    ','    ','    ','    '/
      DATA        KTYPE/'PRS ','HGT ','THK ','HTC ','TMP ','RH  ',
     1                  'QP  ','OZNE','VORT','ISO ','WPT ','VEV ',
     2                  '....','SWND','WVHT','ICEG','WDVT','HGTB',
     3                  'QPVV','LI  ','LI4 ','PSC ','HTC ','MSLP',
     4                  'RH  ','SF  ','BHR ','LI  ','LI4 ','PWTR',
     5                  'PRS ','HGT ','THK ','HTC ','TMP ','RH  ',
     6                  'PCP ','RHPT','VORT','ISOT','....','VEV ',
     7                  'TPRS','TVWS','RH  ','TTM ','TVD ','TKI ',
     8                  'QPVV','VEV '/
C . . . DATA FOR SCALING......
C
C       VALUES FOR KEYIDX FOLLOW:
C        1 = PESSURE, 2 = HEIGHT, 3 = THICKNESS, 4 = HEIGHT CHANGE,
C        4 = HEIGHT CHANGE WITH CENTERS AS BIG "H" AND BIG "L".
C        5 = TEMPERATURE,
C        6 = RELATIVE HUMIDITY NO CENTERS WITH LINES EVERY 20%.
C        7 = PRECIPITATION
C        8 = OZONE, 9 = VORTICITY, 10 = ISOTACHS, 11 = WIND PLOTS,
C       12 = VERT VEL, 13 = MAKE VORT, 14 = SEA SURFACE WIND SPEED.
C       15 = WAVE HEIGHTS,16 = SOLID ICE EDGE,17 = SFC GEOS WIND.
C       18 = BLENDED AVERAGE
C       19 = A PCP FOR BIG C GRID FOR 12 HOUR ACCUMULATION
C       20 = SFC TO 500 LIFTED INDEX,
C       21 = BEST LIFTED INDEX ALSO KNOWN AS THE 4-LAYER LIFTED INDEX.
C       22 = PRESSURE CHANGE
C       23 = HEIGHT CHANGE WITH CENTER AS 2 DIGET NUMBER WITH + OR -
C       24 = MSL PRESSURE ANL(VSLPANL)
C       25 = RELATIVE HUMILITY CENTERS WITH LINES EVERY 20%
C       26 = NGM SURFACE PRESSURE,
C       27 = BOUNDRY LAYER HUMIDITY CENTERS WITH LINES EVERY 10%
C       28 = SFC TO 500 LIFTED INDEX,
C       29 = BEST LIFTED INDEX ALSO KNOWN AS THE 4-LAYER LIFTED INDEX.
C       30 = PRICIP WATER
C       31 = PESSURE,32 = HIEGHT,33 = THICKNESS,34 = HEIGHT CHANGE,
C       35 = TEMPERATURE,
C       36 = RELATIVE HUMIDITY CENTERS WITH LINES EVERY 15%
C       37 = PRECIPITATION
C       38 = RH PLOT
C       39 = SFC GEO REL VORTICITY,
C       40 = ISOTACHS,
C       41 = TROPOPAUSE STREAM FUNCTION
C       42 = VERTICAL VELOCITY,
C       43 = TROPOPAUSE PRESSURE,
C       44 = TROPOPAUSE VWS,
C       45 = RELATIVE HUMIDITY CENTERS + THE 50% LINE
C       46 = TRJECTORY TEMPERATURE/DEPOINT TEMPERATURE.
C       47 = TRJECTORY NET VERTICAL DISPLACEMENT.
C       48 = TRJECTORY K INDEX.
C       49 = A PCP FOR BIG C GRID FOR 6 HOUR ACCUMULATION
C       50 = A VV  FOR BIG C GRID
C
C . . . .ADDITIVE CONSTANTS NEW UNITS TO OLD UNITS.
C
      DATA     UA1/0.0    ,0.0    ,0.0    ,0.0    ,-273.16 ,
     2             0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     3             0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     4             0.0    ,0.0    ,0.0    ,0.0    ,-273.16 ,
     5             0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     6             0.0    ,0.0    ,-273.16,0.0    ,0.0     ,
     7             0.0    ,0.0    ,0.0    ,0.0    ,-273.16 ,
     8             0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     9             0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     A          -273.16   ,0.0    ,0.0    ,0.0    ,0.0     /
C . . . .MULTIPLICATIVE CONSTANTS NEW UNITS TO OLD UNITS.
C
      DATA     UM1/0.01   ,1.0    ,1.0    ,1.0    ,1.0     ,
     2             1.0    ,.03937 ,1.5    ,1.0E5  ,1.94254 ,
     3             1.94254,-10.0  ,1.0    ,1.94254,3.280833,
     4             1.0    ,1.94254,1.0    ,.03937 ,1.0     ,
     5             1.0    ,0.01   ,0.1    , 1.0   ,1.0     ,
     6             0.01   ,1.0    ,1.0     ,1.0   ,0.03937 ,
     7             1.0    ,1.0    ,1.0    ,1.0    ,1.0     ,
     8             1.0    ,39.37  ,1.0    ,1.0E5  ,1.94254 ,
     9           9.434E-6 ,-1.0E3 ,0.01   ,592.084973,1.0  ,
     A             1.0    ,0.01   ,1.0     ,.03937,-1.0E1  /
C . . . .ADDITIVE CONSTANTS FOR CONTOURING.
C
      DATA     UA2/0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     2             6.5    ,.98    ,0.0    ,0.0    ,6.5     ,
     3             0.0    ,0.0    ,0.0    ,-2.5   ,8.0     ,
     4             0.0    ,0.0    ,0.0    ,0.98   ,0.0     ,
     5             0.0    ,0.0    ,0.0    ,0.0    ,6.5     ,
     6             0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     7             0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     8             0.0    ,.94    ,0.0    ,0.0    ,0.0     ,
     9             0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     A             0.0    ,0.0    ,0.0    ,0.96   ,0.0     /
C . . . .MULTIPLICATIVE CONSTANTS FOR CONTOURING.
C
      DATA     UM2/0.25   ,.008333,.016667,.016667,0.2     ,
     2             0.05   ,2.0    ,0.5    ,0.5    ,0.05    ,
     3             1.0    ,0.5    ,0.5    ,0.1    ,0.125   ,
     4             0.5    ,1.0    ,0.01667,2.0    ,0.25    ,
     5             0.25   ,0.25   ,.16667 ,.50    ,0.05    ,
     6             0.25   ,0.1    ,.5     ,0.5    ,8.0     ,
     7             0.50   ,.033334,.033334,.016667,0.4     ,
     8             0.06667,4.0    ,1.0    ,0.5    ,0.10    ,
C    9             0.50   ,1.0    ,.04    ,1.0    ,.02     ,
     9             0.01   ,1.0    ,.02    ,0.5    ,.02     ,
     A             0.20   ,0.05   ,.25    ,4.0    ,.33334  /
C . . . .ADDITIVE CONSTANTS FOR CENTER FINDING/CONTOUR LABELING.
C
      DATA     UA3/0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     2             -6.5   ,-.98   ,0.0    ,0.0    ,-6.5    ,
     3             0.0    ,0.0    ,0.0    ,2.5    ,-8.0    ,
     4             0.0    ,0.0    ,0.0    ,-.98   ,0.0     ,
     5             0.0    ,0.0    ,0.0    ,0.0    ,-6.5    ,
     6             0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     7             0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     8             0.0    ,-.94   ,0.0    ,0.0    ,0.0     ,
     9             0.0    ,0.0    ,0.0    ,0.0    ,0.0     ,
     A             0.0    ,0.0    ,0.0    ,-.96   ,0.0     /
C . . . .MULTIPLICATIVE CONSTANTS FOR CENTER FINDING/CONTOUR LABELING.
C
      DATA     UM3/4.0    ,12.0   ,6.0    ,6.0    ,5.0     ,
     2             20.0   ,50.0   ,20.    ,2.0    ,20.0    ,
     3             1.0    ,2.0    ,1.0    ,10.0   ,8.0     ,
     4             2.0    ,1.0    ,6.0    ,50.0   , 4.0    ,
     5             4.0    ,4.0    ,60.0   ,2.0    ,20.0    ,
     6             4.0    ,10.0   ,2.0    ,2.0    ,12.5    ,
     7             2.0    ,3.0    ,3.0    ,6.0    ,2.5     ,
     8             15.0   ,25.0   ,10.    ,2.0    ,10.0    ,
     9             100.0  ,1.0    ,50.0   ,2.0    ,50.0    ,
     6             5.0    ,20.0   ,4.0    ,25.0   ,3.0     /
C
      END
