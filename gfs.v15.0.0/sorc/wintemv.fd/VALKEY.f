      SUBROUTINE VALKEY
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    VALKEY PERFORM VARIOUS CONSISTENCY CHECKS                      
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 88-03-24                     
C                                                                               
C ABSTRACT: PERFORM VARIOUS CONSISTENCY CHECKS.                                 
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   88-03-24  CAVANAUGH                                                         
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE                                 
C                                                                               
C USAGE:    CALL VALKEY (*)                                                     
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
C                                                                               
C                                                                               
C                                                                               
      save
      REAL         ULCLAT(200)
      REAL         ULCLON(200)
      REAL         XULCLT(500),XULCLN(500)
C                                                                               
      INTEGER      ISEQ(200),   KTRAN,   KEYS
      INTEGER      NCAT(200),     KHEM
      INTEGER      KCNT(200),     KEYCNT,        LABEL(12)
      INTEGER      IDDFF(145,37,10)
      INTEGER      ITEMP(145,37,9)
      INTEGER      ITROP(145,37), MAXHGT(145,37)
      INTEGER      LVL(9,100),      ITIME(8)
      INTEGER      LVLNNN(9)
      INTEGER      MISEQ(500),  MNCAT(500),    MLVL(9,500)
      INTEGER      MLNINC(500), MMXTRP(500),   MLTINC(500)
      INTEGER      MNLVLS(500), MNLON(500),    MNLAT(500)
      INTEGER      MKTAU(500)
C                                                                               
      INTEGER     JTAU(5),      MAXTRP(200),   LONINC(200)
      INTEGER     LATINC(200),  NLVLS(200),    NLON(200)
      INTEGER     NLAT(200),    KTAU(200)
      INTEGER     KYREAD
      data        KYREAD/0/
C                                                                               
      CHARACTER*4   KWMO(5)
C                                                                               
      CHARACTER*1   DATEND
C                                                                               
C                                                                               
      CHARACTER*1   A(200)
      CHARACTER*1   B(200),       KTTY(69,160),  SPACE
      CHARACTER*1   ACOMM(40),    MOUT(1280)
      CHARACTER*1   KWMO1(18),    LF,      CR
      CHARACTER*1   HEADR1(4,100)
      CHARACTER*4   TROP1,   MAXW1,   KWBC1
      CHARACTER*1   MHEDR1(4,500), MA(500),      MB(500)
      CHARACTER*1   ALFAA,         ALFAE
      CHARACTER*1   ALFAF,         ALFAN,          ALFAS
      CHARACTER*1   ALFAW,         ALFAZ
      CHARACTER*1   ALFAM,  ALFAP
  100 FORMAT('        HEADING ERROR  ',A1)
   10 FORMAT (' ENTERING VALKEY')
   15 FORMAT (' EOF READING KEYS, PRESUMABLY THAT IS O.K.')
   17 FORMAT (' ERROR EXIT')
  110 FORMAT('        BULLETIN SEQUENCE NUMBER')
  120 FORMAT('        TAU OUT OF RANGE')
  130 FORMAT('        CATALOG NR OUT OF RANGE')
  140 FORMAT('        LONGITUDE - ULC - OUT OF RANGE')
  150 FORMAT('        INCORRECT E-W INDICATOR')
  160 FORMAT('        LONGITUDE INCREMENT INCORRECT')
  170 FORMAT('        NUMBER OF LONGITUDE STRIPS INCORRECT')
  180 FORMAT('        LATITUDE - ULC - OUT OF RANGE')
  190 FORMAT('        INCORRECT N-S INDICATOR',3(3X,A1),' CARD NO.',
     1              I3)
  200 FORMAT('        LATITUDE INCREMENT INCORRECT')
  210 FORMAT('        NUMBER OF LATITUDE STRIPS INCORRECT')
  220 FORMAT('        NLVLS AND ACTUAL LEVELS DO NOT AGREE')
  230 FORMAT('        BULLETIN CONTAINS MORE THAN 2000 CHARACTERS')
  240 FORMAT (4A1,I2,1X,I2,1X,I5,1X,F6.2,1X,A1,2(1X,I1),1X,F5.2,1X,A1,
     *        1X,I1,2(1X,I2),1X,9I1,1X,I1,16X,I8)
  250 FORMAT(9X,4A1,I2,1X,I2,1X,I5,1X,F6.2,1X,A1,2(1X,I1),1X,F5.2,1X,A1,
     *        1X,I1,2(1X,I2),1X,9I1,1X,I1,16X,I8)
  251 FORMAT (1X,I4)
  260 FORMAT ('  I/O ERROR READING KEYS')
  270 FORMAT (' HEADER AND SEQ NR ',4A1,I2,/,
     *'     TAU ',I2,' CATALOG NR ',I5,/,
     *'     LONGITUDE  ',F6.2,' E-W ',A1,' LONINC ',I6,' NLON ',I6,/,
     *'     LATITUDE   ',F6.2,' N-S ',A1,' LATINC ',I6,' NLAT ',I6,/,
     *'     LEVELS     ',I2,' LEVEL 1-9  ',9(1X,I6),/,
     *'     TROPOPAUSE/MAX WINDS ',I6,'  LSEQ   ',I6)
  280 FORMAT(4(1X,A1))
C     WRITE (6,10)                                                              
      KEYCNT=0
      KPTR=1
      TIMKEY=MTAU*6
      IF(KYREAD.NE.0) GO TO 1000
      KYREAD=1
      mmptr = 0
      DO 600 kk=1,500
        READ(KEYS,240,ERR=3150,END=1000)(MHEDR1(N,kk),N=1,4),
     2        MISEQ(kk),MKTAU(kk),MNCAT(kk),XULCLN(kk),
     3        MB(kk),MLNINC(kk),MNLON(kk),XULCLT(kk),
     4        MA(kk),MLTINC(kk),MNLAT(kk),MNLVLS(kk),
     5        (MLVL(N,kk),N=1,9),MMXTRP(kk),MLSEQ
        mmptr = kk
  600 CONTINUE
C               READ KEY LIST                                                   
 1000 continue
      DO 3000 IK=1,MMPTR
          KERR=0
          DO 1010 IJ=1,4
              HEADR1(IJ,KPTR) = MHEDR1(IJ,IK)
 1010     CONTINUE
          ISEQ(KPTR)    = MISEQ(IK)
          KTAU(KPTR)    = MKTAU(IK)
          NCAT(KPTR)    = MNCAT(IK)
          ULCLON(KPTR)  = XULCLN(IK)
          B(KPTR)       = MB(IK)
          LONINC(KPTR)  = MLNINC(IK)
          NLON(KPTR)    = MNLON(IK)
          ULCLAT(KPTR)  = XULCLT(IK)
          A(KPTR)       = MA(IK)
          LATINC(KPTR)  = MLTINC(IK)
          NLAT(KPTR)    = MNLAT(IK)
          NLVLS(KPTR)   = MNLVLS(IK)
          DO 1011 IJ=1,9
              LVL(IJ,KPTR)= MLVL(IJ,IK)
 1011     CONTINUE
          MAXTRP(KPTR)  = MMXTRP(IK)
c         WRITE(6,270)(HEADR1(N,KPTR),N=1,4),ISEQ(KPTR),KTAU(KPTR),             
c    *       NCAT(KPTR),ULCLON(KPTR),B(KPTR),LONINC(KPTR),NLON(KPTR),           
c    *       ULCLAT(KPTR),A(KPTR),LATINC(KPTR),NLAT(KPTR),NLVLS(KPTR),          
c    *       (LVL(N,KPTR),N=1,9),MAXTRP(KPTR),LSEQ                              
      IF (KTAU(KPTR).EQ.TIMKEY) THEN
C                                                                               
C               CHECK N-S INDICATOR                                             
C                                                                               
C  ----------------------------                                                 
C                                                                               
C  ----------------------------                                                 
          if(a(kptr).ne.alfan) then
            if(a(kptr).ne.alfas) then
              kerr = 2
            endif
          endif
          IF (KHEM.EQ.1.AND.A(KPTR).NE.ALFAN) goto 3000
          IF (KHEM.EQ.2.AND.A(KPTR).NE.ALFAS) goto 3000
C                                                                               
C                       END OF FILE                                             
C                                                                               
C               ERROR WHILE READING LIST OF KEYS                                
C                                                                               
C                                                                               
C               PERFORM RANGE AND OR VALIDITY CHECKS ON KEY                     
C                                                                               
 1200     CONTINUE
C                                                                               
C               CHECK HEADERS FOR ALL ALFAS                                     
C                                                                               
C  -----------------------                                                      
          IF (HEADR1(1,KPTR).NE.ALFAF) THEN
              KERR = 3
              WRITE(6, 100) HEADR1(I,KPTR)
          ELSE
              DO 1210 I = 2 , 4
                  IF (HEADR1(I,KPTR).GT.ALFAZ) THEN
                      KERR = 3
                      WRITE(6, 100) HEADR1(I,KPTR)
                  END IF
 1210         CONTINUE
          END IF
C                                                                               
C               CHECK SEQUENCE NUMBER                                           
C                                                                               
 1250     CONTINUE
          IF(ISEQ(KPTR).LT.1.OR.ISEQ(KPTR).GT.99) THEN
C                            SEQUENCE NUMBER ERROR                              
              KERR = 4
              WRITE(6, 110)
          END IF
C                                                                               
C               CHECK LENGTH OF FORECAST (KTAU(KPTR))                           
C                                                                               
          IF (KTAU(KPTR).LT.6.AND.KTAU(KPTR).GT.48) THEN
C                            FORECAST TIME OUT OF RANGE                         
              KERR = 5
              WRITE(6, 120)
          END IF
C                                                                               
C          CHECK CATALOG NUMBER                                                 
C                                                                               
          IF (NCAT(KPTR).LT.0.AND.NCAT(KPTR).GT.9999) THEN
C                            CATALOG NUMBER OUT OF RANGE                        
              KERR = 6
              WRITE(6, 130)
          END IF
C                                                                               
C               CHECK LONGITUDE OF UPPER LEFT CORNER                            
C                                                                               
          IF (ULCLON(KPTR).LT.0.0.OR.ULCLON(KPTR).GT.180.0) THEN
C                            IMPROPER LONGITUDE VALUE                           
              KERR = 7
              WRITE(6, 140)
          ENDIF
C                                                                               
C               CHECK E-W INDICATOR                                             
C                                                                               
 1450     CONTINUE
          IF (B(KPTR).EQ.ALFAE.OR.B(KPTR).EQ.ALFAW) THEN
              GO TO 1500
          ELSE
C                            NOT E OR W INDICATOR                               
              KERR = 8
              WRITE(6, 150)
          END IF
C                                                                               
C               CHECK LONGITUDE INCREMENT INDICATOR                             
C                                                                               
 1500     CONTINUE
          IF (LONINC(KPTR).LT.1.OR.LONINC(KPTR).GT.3) THEN
C                            LONGITUDE INCREMENT OUT OF RANGE                   
              KERR = 9
              WRITE(6, 160)
          END IF
C                                                                               
C               CHECK RANGE OF LONGITUDE STRIPS                                 
C                                                                               
 1550     CONTINUE
          IF (NLON(KPTR).LT.1.OR.NLON(KPTR).GT.7) THEN
C                            TOO MANY LONGITUDE STRIPS                          
              KERR = 10
              WRITE(6, 170)
          END IF
C                                                                               
C               CHECK LATITUDE OF ULC                                           
C                                                                               
 1600     CONTINUE
          IF (ULCLAT(KPTR).LT.0.0.OR.ULCLAT(KPTR).GT.90.0) THEN
C                            LATITUDE OUT OF RANGE                              
              KERR = 11
              WRITE(6, 180)
          END IF
C                                                                               
C               CHECK LATITUDE INCREMENT INDICATOR                              
C                                                                               
 1700     CONTINUE
          IF (LATINC(KPTR).LT.1.OR.LATINC(KPTR).GT.3) THEN
C                            LATITUDE INCREMENT OUT OF RANGE                    
              KERR = 12
              WRITE(6, 200)
          END IF
C                                                                               
C        SET LATITUDE INCREMENT FROM INPUT TABULAR VALUE TO                     
C        TRUE  MULTIPLIER OF 2.5DEG TO GET DESIRED INCREMENTS                   
C                                                                               
 1725     CONTINUE
          IF (LATINC(KPTR).GT.2) THEN
              LATINC(KPTR)=4
          END IF
C                                                                               
C    CHECK FOR LATITUDE BOUNDARY VIOLATION                                      
C                                                                               
C              TEST IS TO BE SURE THAT SOUTHERN MOST EXTENT OF                  
C               OUTPUT GRID DOES NOT EXTEND SOUTH OF EQUATOR                    
C                                                                               
C            NEED TO GENERATE SOUTHERN HEMISPHERE TEST IN FUTURE                
C                                                                               
C  ------------------------------------                                         
          IF(A(KPTR).NE.ALFAN) THEN
              X=90.0 - ULCLAT(KPTR)
              IF ((X/2.5-(NLAT(KPTR)-1)*LATINC(KPTR)+.001).GE.0) THEN
                  GO TO 1750
              END IF
          END IF
          IF((ULCLAT(KPTR)/2.5-(NLAT(KPTR)-1)*LATINC(KPTR)+.001).GE.0)
     &     THEN
              GOTO 1750
          END IF
          print *, ' a(kptr), alfan =', a(kptr), alfan
          print *, ' ulclat(kptr),nlat(kptr),latinc(kptr) ='
          print *, ulclat(kptr), nlat(kptr), latinc(kptr)
          x = ULCLAT(KPTR)/2.5-(NLAT(KPTR)-1)*LATINC(KPTR)
          print *, ' x =', x
C  ------------------------------------                                         
  205     FORMAT (1X,'LATITUDE BOUNDARY VIOLATION')
 1740     WRITE (6,205)
C                            LATITUDE INCREMENT OUT OF RANGE                    
          KERR = 13
C                                                                               
C               CHECK NUMBER OF LATITUDE STRIPS                                 
C                                                                               
 1750     CONTINUE
          IF (NLAT(KPTR).LT.1.OR.NLAT(KPTR).GT.12) THEN
              KERR = 14
              WRITE(6, 210)
          END IF
C                                                                               
C               SEE IF LVL TOTAL IS EQUAL TO NLVLS                              
C                                                                               
 1800     CONTINUE
          K = 0
          DO 1850 M = 1 , 9
              IF (LVL(M,KPTR).EQ.1) THEN
                  K = K + 1
              END IF
 1850     CONTINUE
C                                                                               
          IF (NLVLS(KPTR).NE.K) THEN
              KERR = 15
              WRITE(6, 220)
          END IF
C                                                                               
C               CHECK BULLETIN CHARACTER LOADING                                
C                                                                               
 1900     CONTINUE
          KHDGS = 45 + (NLON(KPTR) * 9)
          KLVLS = NLVLS(KPTR)
          IF (MAXTRP(KPTR).NE.0) THEN
              KLVLS = KLVLS + 2
          END IF
          KOUNT = KHDGS + (KLVLS * (7 + NLON(KPTR) * 9) * NLAT(KPTR))
     1          + ((NLAT(KPTR)-1) * 7)
          IF (KOUNT.LE.2000) THEN
              GO TO 2000
          END IF
          KERR = 16
          WRITE(6, 230)
C                                                                               
C               IF NO ERROR EXISTS ON THIS KEY,                                 
C                  RETURN W/VALID KEY                                           
C                                                                               
 2000     CONTINUE
          IF (KERR.EQ.0) THEN
              KEYCNT=KEYCNT+1
              KPTR=KPTR+1
          ELSE
C                       SHOW KEY IN ERROR                                       
              PRINT *,'ERROR ERROR ERROR ERROR'
              WRITE(6,250) (HEADR1(N,KPTR),N=1,4),ISEQ(KPTR),KTAU(KPTR),
     *            NCAT(KPTR),ULCLON(KPTR),B(KPTR),
     *            LONINC(KPTR),NLON(KPTR),ULCLAT(KPTR),A(KPTR),
     *            LATINC(KPTR),NLAT(KPTR),NLVLS(KPTR),
     *            (LVL(N,KPTR),N=1,9),MAXTRP(KPTR),LSEQ
          END IF
      endif
C               GET NEXT KEY                                                    
 3000 CONTINUE
      RETURN
 3150 WRITE (6,260)
      WRITE (6,17)
      RETURN  
      END
