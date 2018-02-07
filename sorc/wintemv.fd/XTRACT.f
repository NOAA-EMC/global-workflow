      SUBROUTINE XTRACT
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    XTRACT EXTRACT REQUIRED ON84 FIELDS                            
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 88-03-24                     
C                                                                               
C ABSTRACT: EXTRACT ON84 FIELDS REQUIRED FOR THE PRODUCTION                     
C   OF WINTEM BULLETINS.                                                        
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C    88-03-24  CAVANAUGH                                                         
C  2012-11-15  VUONG       VARIABLE ENVVAR TO CHARACTER*6
C                                                                               
C USAGE:    CALL XTRACT (*)                                                     
C   INPUT ARGUMENT LIST:                                                        
C     *        - RETURN ADDRESS                                                 
C                                                                               
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION                     
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: FORTRAN 90
C                                                                               
C$$$                                                                            
C                                                                               
      COMMON   /BLK1/
C               REAL*4                                                          
     1               ULCLAT,  ULCLON,
C               INTEGER*4                                                       
     2               ISEQ,    KTRAN,   IDDFF,   ITEMP,   ITROP,
     3               MAXHGT,  KEYS,    NCAT,    KHEM,    KCNT,
     4               KEYCNT,  LABEL,   LVL,     ITIME,   LVLNNN,
     6               MTAU,
C               CHARACTER*4                                                     
     7               TROP1,   KWBC1,   MAXW1,   KWMO,
C               INTEGER*2                                                       
     8               JTAU,    MAXTRP,  LONINC,  LATINC,  NLVLS,
     9               NLON,    NLAT,    KTAU,
C               CHARACTER*1                                                     
     2               ACOMM,
     3               DATEND,  KTTY,    SPACE,   MOUT,
     4               A,       B,       LF,      CR,
     5               HEADR1,
     6               ALFAA,   ALFAE,   ALFAF,   ALFAM,
     7               ALFAN,   ALFAP,   ALFAS,   ALFAW,
     8               ALFAZ,   KWMO1
C                                                                               
      save
      CHARACTER*8    LFN
C                                                                               
      REAL         WORK(144),     GN(145,37,2),  ULCLAT(200)
      REAL         ULCLON(200)
C     REAL         CNST(4)/0.,1.,0.,0./                                       
C                                                                               
      INTEGER      ISEQ(200),   KTRAN,   KEYS
      INTEGER      NCAT(200),     KHEM,          LABSV(3,50)
      INTEGER      KCNT(200),     KEYCNT,        LABEL(12)
      INTEGER      IDDFF(145,37,10)
      INTEGER      ITEMP(145,37,9)
      INTEGER      ITROP(145,37), MAXHGT(145,37)
      INTEGER      LVL(9,100),      ITIME(8)
      INTEGER      LVLNNN(9)
      CHARACTER*4    KWMO(5)
C                                                                               
C         BASE OF FIRST ON84ID WORD                                             
C                                                                               
C                         U-COMP    V-COMP    TEMP      HGT                     
c     INTEGER*4 L1(4)/Z03000800,Z03100800,Z01000800,Z00100800/                  
c     INTEGER*4 M1(3)/Z03008300,Z03108300,Z00808300/                            
c     INTEGER*4 M2/Z00000000/,M3(2)/Z0000001B,Z0000001C/                        
c     INTEGER*4 LTRP/Z00808200/,M4(2)/Z0000001D,Z0000001E/                      
c     INTEGER*4 M5/Z0000003D/                                                   
C                                                                               
C         BASE OF SECOND ON84ID WORD                                            
C                                                                               
C                              850MB                                            
c     INTEGER*4     LP2LEV(9)/Z014C0882,                                        
C          700MB     500MB     400MB     300MB                                  
c    1     Z01117082,Z00C35082,Z009C4082,Z00753082,                             
C          250MB     200MB     150MB     100MB                                  
c    2     Z0061A882,Z004E2082,Z003A9882,Z00271082/                             
C                                                                               
C         ENTRY FOR FIFTH WORD OF ON84ID                                        
C                                                                               
c     INTEGER*4     L5(4)/Z3E,Z3E,Z3D,Z3D/                                      
C                                                                               
C                                                                               
      INTEGER     JTAU(5),      MAXTRP(200),   LONINC(200)
      INTEGER     LATINC(200),  NLVLS(200),    NLON(200)
      INTEGER     NLAT(200),    KTAU(200)
      integer     level(10)
C                                                                               
C                                                                               
      CHARACTER*1   DATEND
C                                                                               
C                                                                               
      CHARACTER*1   A(200)
      CHARACTER*1   B(200),       KTTY(69,160),  SPACE
      CHARACTER*1   ACOMM(40),    MOUT(1280)
      CHARACTER*1   KWMO1(18),    LF,      CR
      CHARACTER*1   HEADR1(4,100)
      CHARACTER*1   ALFAA,         ALFAE,        EWINDC
      CHARACTER*1   ALFAF,         ALFAN,          ALFAS
      CHARACTER*1   ALFAW,         ALFAZ
      CHARACTER*1   ALFAM,  ALFAP
      CHARACTER*4   TROP1,   MAXW1,   KWBC1
      character*6   envvar
      character*80 fileb,filei


      data        level/850,700,500,400,300,250,200,150,100,0/
  100 FORMAT (' CHECK FOR INCORRECT LABEL',3(2X,Z4))
  110 FORMAT (' SPECIFIED MAP TYPE NOT AVAILABLE',I4)
    1 FORMAT ('ENTERING EXTRACT')
    3 FORMAT(' UNIQUE ENTRY # ',I4)
    4 FORMAT(' LABEL=',12Z10)
    5 FORMAT(' LEVEL ',I4)
    6 FORMAT(' NORMAL XTRACT EXIT')
      MAPIN=28
      MAPOUT=30
      IF(KHEM.NE.1) GO TO 500
      MAPIN=27
      MAPOUT=29
  500 INTERP=0
C     WRITE (6,1)                                                               
      IPTR=0
C                                                                               
C             CHECK EACH VALID KEY                                              
C                                                                               
      DO 4000 IKEY= 1, KEYCNT
C              10 DIFFERENT PRESSURE LEVELS                                     
C          ADJUST 6, 12 HOUR BRANCHES                                           
      ITAU  = KTAU(IKEY)/6
c
c  unit numbers are :
c                    11, 31 for 6-hour forecast
c                    12, 32 for 12-hour forecast
c                    13, 33 for 18-hour forecast
c                    14, 34 for 24-hour forecast
c
        lgb = itau + 10
        lgi = itau + 30
         envvar='FORT  '
         write(envvar(5:6),fmt='(I2)') lgb
         call getenv(envvar,fileb)
         write(envvar(5:6),fmt='(I2)') lgi
         call getenv(envvar,filei)
         call baopen(lgb,fileb,iret)
         call baopen(lgi,filei,iret)

C  ---------------------------                                                  
C                                                                               
C             GET EACH ACTIVE LEVEL FOR THIS VALID KEY                          
C                                                                               
      DO 3000 JJ = 1 , 9
          IF (LVL(JJ,IKEY).NE.1) GO TO 3000
          DO 2000 KK = 1, 3
              IER=0
              goto (1000,1100,1200) kk
 1000      continue
c  get u field
            kpds5 = 33
            kpds6 = 100
            kpds7 = level(jj)
            call readgrib(gn(1,1,1),kpds5,kpds6,kpds7,lgb,lgi,
     &      khem)
c
              DO 1050 J=1,37
c                 WRITE(6,300)(GN(I,J,1),I=131,145)
  300             FORMAT(15(1X,F5.1))
 1050         CONTINUE
              GO TO 2000
C                                                                               
 1100      continue
c  get v field
            kpds5 = 34
            kpds6 = 100
            kpds7 = level(jj)
            call readgrib(gn(1,1,2),kpds5,kpds6,kpds7,lgb,lgi,
     &      khem)
              DO 1110 J=1,37
c             WRITE(6,300)(GN(I,J,2),I=131,145)
 1110         CONTINUE
C         GENERATE DDFFF FROM U,V COMPONENTS                                    
              DO 1150 J=1,37
                  DO 1125 I=1,145
                      CALL W3FC00 (GN(I,J,1),GN(I,J,2),NDIR,NSPD)
                      DDFFF = NDIR * 1000 + NSPD*1.93
                      IDDFF(I,J,JJ)=DDFFF + .5
 1125             CONTINUE
c                 WRITE (6,310)(IDDFF(I,J,JJ),I=131,145)
 1150         CONTINUE
C            IF(KHEM.EQ.1.AND.JJ.EQ.5)GO TO 1170                                
C            GO TO 1180                                                         
C1170       DO 1172 J=1,37                                                      
C               DO 1171 I=1,145                                                 
C                   GN(I,J,1)=IDDFF(I,J,JJ)                                     
C1171           CONTINUE                                                        
C1172       CONTINUE                                                            
C           CALL GRDPRT(GN(1,1,1),KTBL,CNST,KTITLE,KRECT,KCONTR)                
C1180 CONTINUE                                                                  
      GO TO 2000
 1200      continue
c  get temperature
            kpds5 = 11
            kpds6 = 100
            kpds7 = level(jj)
            call readgrib(gn(1,1,1),kpds5,kpds6,kpds7,lgb,lgi,
     &      khem)
      DO 1220 j=1,37
c       WRITE(6,300)(GN(I,J,1)-273.16,i=131,145)
 1220 CONTINUE
      DO 1300 J=1,37
          DO 1250 I=1,145
              TEMP=GN(I,J,1)-273.16
              ITEMP(I,J,JJ)=TEMP + .5
 1250     CONTINUE
 1300 CONTINUE
      DO 1140 N=1,37
c         WRITE(6,310)(IDDFF(M,N,JJ),M=131,145)
c         WRITE(6,310)(ITEMP(M,N,JJ),M=131,145)
  310 FORMAT(15(1X,I5))
 1140 CONTINUE
 2000     CONTINUE
 3000   CONTINUE
C                                                                               
C         IF MAXW/TROP FLAG IS NOT ON, SKIP THIS BLOCK OF CODE                  
C                                                                               
      IF (MAXTRP(IKEY).NE.1) GO TO 4000
c  get tropopause pressure
            kpds5 = 1
            kpds6 = 7
            kpds7 = 0
            call readgrib(gn(1,1,1),kpds5,kpds6,kpds7,lgb,lgi,
     &      khem)
c  convert from Pa unit to mb
        do j = 1, 37
          do i = 1, 145
            gn(i,j,1) = gn(i,j,1) / 100.
          enddo
        enddo
C      CONVERT PRESSURE TO HEIGHT                                              
 3025 DO 3075 J=1,37
        DO 3050 I=1,145
          CALL W3FA03(GN(I,J,1),GN(I,J,2),TMP,THETA)
          GN(I,J,2)=GN(I,J,2)*.0328
 3050   CONTINUE
c        WRITE (6,320)(GN(I,J,2),I=131,145)
  320 FORMAT (1X,15F5.0)
 3075 CONTINUE
      DO 3072 J=1,37
         DO 3070 I=1,145
           ITROP(I,J)=GN(I,J,2) + .5
           ITROP(I,J)=((ITROP(I,J)+5)/10)*10
 3070 CONTINUE
 3072 CONTINUE
      DO 3085 J=1,37
c       WRITE (6,325)(ITROP(I,J),I=131,145)
  325   FORMAT (15(1X,I6))
 3085 CONTINUE
C                                                                               
C         GET MAX WIND DATA                                                     
C            U-COMP FIRST 145 X 37                                              
C                                                                               
c  get u-component of max wind
            kpds5 = 33
            kpds6 = 6
            kpds7 = 0
            call readgrib(gn(1,1,1),kpds5,kpds6,kpds7,lgb,lgi,
     &      khem)
c  get v-component of max wind
            kpds5 = 34
            kpds6 = 6
            kpds7 = 0
            call readgrib(gn(1,1,2),kpds5,kpds6,kpds7,lgb,lgi,
     &      khem)
C         CALCULATE DDFFF                                                       
      DO 3250 J=1,37
        DO 3225 I=1,145
          CALL W3FC00 (GN(I,J,1),GN(I,J,2),NDIR,NSPD)
          DDFFF=NDIR*1000+NSPD*1.93
          IDDFF(I,J,10)=DDFFF + .5
 3225   CONTINUE
c       WRITE (6,350)(IDDFF(I,J,10),I=131,145)
  350 FORMAT (15(1X,I5))
 3250 CONTINUE
c  get pressure of max wind
            kpds5 = 1
            kpds6 = 6
            kpds7 = 0
            call readgrib(gn(1,1,1),kpds5,kpds6,kpds7,lgb,lgi,
     &      khem)
C         HEIGHT OF MAX WIND                                                    
 3300 DO 3350 J=1,37
        DO 3325 I=1,145
c  convert to mb
          gn(i,j,1) = gn(i,j,1) / 100.
          CALL W3FA03(GN(I,J,1),GN(I,J,2),TMP,THETA)
        MAXHGT(I,J)=GN(I,J,2)*.0328 + .5
        MAXHGT(I,J)=((MAXHGT(I,J)+5)/10)*10
 3325   CONTINUE
 3350 CONTINUE
C                                                                               
 3500 CONTINUE
 4000 CONTINUE
C     WRITE (6,6)                                                               
      RETURN
      END
