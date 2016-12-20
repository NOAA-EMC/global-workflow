C          DATA SET WINTEMV    AT LEVEL 060 AS OF 09/13/91              
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
C                .      .    .                                       .  
C MAIN PROGRAM: WINTEMV
C   PRGMMR: VUONG            ORG: NP11        DATE: 1999-08-31
C                                                                       
C ABSTRACT: THIS IS AN EXTERNALLY DRIVEN PROGRAM DESIGNED TOD           
C   GENERATE WIND FORECAST BULLETINS IN WINTEM FORMAT.                  
C   BULLETIN KEYS ARE GENERATED DESCRIBING THE AREA,                    
C   FORECAST PERIOD, FLIGHT LEVELS, AND SPACING OF                      
C   FORECAST POINTS. THESE KEYS ARE STORED EXTERNAL TO                  
C   THE MAIN PROGRAM.  THIS PROVIDES AN EASY METHOD OF                  
C   ADDING NEW BULLETINS AS REQUIRED, AND THE EXCLUSION                 
C   OF THOSE BULLETINS THAT NO LONGER HAVE A PRACTICAL                  
C   USE.  THIS CAN BE ACCOMPLISHED WITHOUT INTERRUPTING                 
C   THE OPERATION OF THE MAIN PROGRAM.                                  
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   88-03-24  CAVANAUGH                                                 
C   87-08-17  CAVANAUGH   DESCRIPTION OF CHANGE                         
C   88-01-06  FARLEY      REPLACED XDAM I/O WITH VSAM I/O               
C   88-03-18  FARLEY      REMOVED 'V' FROM DDNAMES                      
C   90-11-14  CAVANAUGH   ENTERED CHECK TO PREVENT 0 DEG WIND DIRECTIONS
C   91-08-19  CAVANAUGH   MODIFIED PROGRAM TO GENERATE COMMUNICATIONS   
C                         PREFIX USING W3FI61. MODIFIED UNIT NUMBERS    
C                         TO CONFORM TO I/O UNIT NUMBER CONVENTIONS.    
C   91-09-06  CAVANAUGH   REMOVED ALL REFERENCES TO PARM FIELD,         
C                         RESTORED UNIT NUMBERS FOR THE PRESENT.        
C   98-04-24  VUONG       REPLACED W3FQ02 WITH CALLS TO W3UTCDAT
C   99-08-31  VUONG       CONVERTED TO RUN ON THE IBM RS/6000 SP
C 2012-11-15  VUONG       CHANGED VARIABLE ENVVAR TO CHARACTER*6
C
C USAGE:                                                                
C   INPUT FILES:                                                        
C     FT05F001 - LIST OF BULLETIN KEYS                                  
c     fort.11 ...  6-hr 1x1 pressure grib file
c     fort.12 ... 12-hr 1x1 pressure grib file
c     fort.13 ... 18-hr 1x1 pressure grib file
c     fort.14 ... 24-hr 1x1 pressure grib file
c     fort.31 ...  6-hr 1x1 pressure grib index file
c     fort.32 ... 12-hr 1x1 pressure grib index file
c     fort.33 ... 18-hr 1x1 pressure grib index file
c     fort.34 ... 24-hr 1x1 pressure grib index file
C                                                                       
C   OUTPUT FILES:                                                       
C     FT06F001 - PRINT FILE                                             
C     FT51F001 - BULLETINS FOR TRAN                                     
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C     UNIQUE:    - VALKEY  XTRACT  LABLCK                               
C     LIBRARY:                                                          
C       W3LIB    - IW3VGE  W3AG15  W3AI15  W3FA03  W3FA11               
C                  W3FA13  W3FC00  W3FC07  W3FT08  W3FT09  W3FT10       
C                  W3FT11  W3UTCDAT
C                                                                       
C   EXIT STATES:                                                        
C     COND =   0 - SUCCESSFUL RUN                                       
C          =   1 - NO VALID KEYS                                        
C          =   5 - ERROR ON 1 OR MORE INPUT KEYS, MUST CORRECT          
C          =  10 - TRAN NOT POSTED                                      
C          = 300 - HAVE VALID KEYS, NO BULLETINS GENERATED              
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90 
C                                                                       
C$$$                                                                    
C                                                                       
C                                                                       
C              START OF PROGRAM                                         
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
      CHARACTER*8    WORKA
C                                                                       
      REAL         ULCLAT(200),    ULCLON(200)
C                                                                       
      INTEGER      ISEQ(200),   KTRAN,   KEYS
      INTEGER      NCAT(200),     KHEM
      INTEGER      KCNT(200),     KEYCNT,        LABEL(12)
      INTEGER      IDDFF(145,37,10)
      INTEGER      ITEMP(145,37,9)
      INTEGER      ITROP(145,37), MAXHGT(145,37)
      INTEGER      LVL(9,100), ITIME(8), IHRMIN
      INTEGER      LVLNNN(9)
      INTEGER     JTAU(5),      MAXTRP(200),   LONINC(200)
      INTEGER     LATINC(200),  NLVLS(200),    NLON(200)
      INTEGER     NLAT(200),    KTAU(200)
      INTEGER     KCOM2(20)
      INTEGER     MDAYS(12)
C                                                                       
      CHARACTER*1   DATEND
      CHARACTER*2   KT
      CHARACTER*4   KWMO(5)
      CHARACTER*1   A(200)
      CHARACTER*1   B(200),       KTTY(69,160),  SPACE
      CHARACTER*1   ACOMM(40),    MOUT(1280)
      CHARACTER*1   KWMO1(18),    LF,      CR
      CHARACTER*1   HEADR1(4,100),   TROP1(4),   MAXW1(4),   KWBC1(4)
      CHARACTER*1   ALFAA,        EWINDC,        ETX
      CHARACTER*1   ALFAF,        ALFAE,         ALFAW,      ALFAN
      CHARACTER*1   ALFAZ,        ALFAS
      CHARACTER*1   ALFAM,  ALFAP,  WORKB(8),KETX, lfascii
      CHARACTER*6   WINTEM,BLANKS,AREG,ENVVAR
      character *80 line, fileo
C
      data wintem/'WINTEM'/,blanks/'      '/,ketx/'>'/,etx/'%'/
      data kt/'KT'/
      data mdays/31,28,31,30,31,30,31,31,30,31,30,31/
C
      EQUIVALENCE  (WORKA,WORKB(1))
C
      CALL W3TAGB('WINTEMV',2012,0320,0087,'NP11')                                              
      envvar='FORT  '
      write(envvar(5:6),fmt='(I2)') ktran
      call getenv(envvar,fileo)
      OPEN(KTRAN,FILE=FILEO,ACCESS='DIRECT',RECL=1281)
      IREC=1 
      lfascii = char(10)
C                                                                       
C              LOOP THRU ALL TAU PERIODS                                
C                                                                       
      DO 9000 MTAU = 1, 8
C                                                                       
C                 LOOP THRU BOTH HEMISPHERES IF REQUIRED                
C                                                                       
          DO 8000 KHEM = 1, 2
C              GET ALL VALID KEYS BASED ON FORECAST PERIOD (TAU)        
C              FOR REQUESTED HEMISPHERE                                 
C                                                                       
              KEYCNT=0
              CALL VALKEY
C         IF NO VALID KEYS, OUTPUT MESSAGE                              
C                                                                       
              IF (KEYCNT.EQ.0) THEN
c                 WRITE (6,9051) KEYCNT                                 
 9051             FORMAT (1X,'KEYCNT=',I4,121X)
                  GO TO 8000
              END IF
C     INITIALIZE ALL FIELDS                                             
              DO M=1,10
                  DO J=1,37
                      DO I=1,145
                          IDDFF(I,J,M)=99999
                          ITROP(I,J)=999
                          MAXHGT(I,J)=999
                      END DO
                  END DO
              END DO 
C                                                                       
C              HAVE ALL VALID KEYS (TAU), EXTRACT REQUIRED DATA         
C                                                                       
c             WRITE(6,9511) KEYCNT                                      
 9511         FORMAT (I4,' VALID KEYS',117X)
              CALL XTRACT
C                                                                       
C              BUILD COMPLETE BULLETIN FOR TRANSMISSION                 
C              & GENERATE TTY FORMAT DATA FOR MICROFICHE                
C                                                                       
              BULCNT=0
C                                                                       
C         OBTAIN CURRENT GMT FOR BULLETIN HEADERS                       
C                                                                       
              CALL W3UTCDAT (ITIME)
C                                                                       
C     PROCESS BULLETINS                                                 
C                                                                       
C         FOR EACH BULLETIN                                             
C                                                                       
              DO 4600 IBULL = 1, KEYCNT
C              INITIALIZE LATITUDE START INDEX FOR ULC                  
              IF (KHEM.EQ.1) THEN
C                   NORTHERN HEMISPHERE                                 
                  SIXLAT = ULCLAT(IBULL)/2.5 +1
              ELSE
C                   SOUTHERN HEMISPHERE                                 
                  SIXLAT = 37.0 - ULCLAT(IBULL)/2.5
              END IF
              LATSIX = SIXLAT + .5
C              INITIALIZE LONGITUDE START INDEX FOR ULC                 
              SIXLON = ULCLON(IBULL)/2.5 + 1
              IF(B(IBULL).EQ.ALFAW) THEN
                  SIXLON = 145.0 - ULCLON(IBULL)/2.5
              END IF
              LONSIX = SIXLON + .5
C              INITIALIZE LINE NUMBER FOR ARCHIVE OUTPUT                
              KLIN = 1
C              IF 10 DEGREE SPACING IS DESIRED, ADJUST INCREMENT        
C                 VALUES TO GET 10 DEGREES.                             
              ILNINC = LONINC(IBULL)
              IF (LONINC(IBULL).GT.2) THEN
                  ILNINC = 4
              END IF
C                                                                       
              ILTINC = -LATINC(IBULL)
              IF (LATINC(IBULL).GT.2) THEN
                  ILTINC = - 4
              END IF
              DO J=1,160
                  DO I=1,69
                      KTTY(I,J)=SPACE
                  END DO
              END DO
C                                                                       
C         BUILD BULLETIN  HEADER                                        
C                                                                       
              DO I = 1, 4
                  ktty(i,klin) = headr1(i,ibull)
              END DO
C                                                                       
C                   ENTER SEQUENCE NUMBER                               
C                                                                       
              N1 = 1
              N2 = 2
              CALL W3AI15 (ISEQ(IBULL),line,N1,N2,'-')
              do ii = 1, 2
                ktty(ii+4,klin) = line(ii:ii)
              enddo
              DO I=1, 4
                ktty(i+7,klin) = kwbc1(i)
              END DO
              KWMO1(12)  = SPACE
              ktty(12,klin) = space
C                                                                       
C           DATE/TIME TO KWMO HEADER                                    
C                                                                       
                  N1 = 1
                  N2 = 2
                  CALL W3AI15 (ITIME(3),line,N1,N2,'-')
                  do ii = 1, 2
                    ktty(12+ii,klin) = line(ii:ii)
                  enddo
                  N2 = 4
                  IHRMIN = ITIME(5) * 100 + ITIME(6)
                  CALL W3AI15 (IHRMIN,line,N1,N2,'-')
                  do ii = 1, 4
                    ktty(ii+14,klin) = line(ii:ii)
                  enddo
C                                                                       
C              KWMO HEADER TO ARCHIVE ARRAY                             
C                                                                       
                  KCNT(KLIN) = 18
c                 DO 4150 I = 1, 18                                     
c                     KTTY(I,KLIN) = KWMO1(I)                           
c4150             CONTINUE                                              
                  WRITE (6,100)(KTTY(N,KLIN),N=1,69)
                  KLIN = 2
C                                                                       
C                WINTEM LINE TO OUTPUT                                  
C                                                                       
                  DO I = 1, 6
                      KTTY(I,KLIN) = WINTEM(I:I)
                  END DO
             IF (IHRMIN.LT.1200) THEN
                 KTIME = 0
             ELSE
                 KTIME = 1200
             END IF
             KKEY = ITIME(1) - ((ITIME(1)/4) * 4)
             IF (KKEY.EQ.0) THEN
                 MDAYS(2) = 29
             END IF
             MM = ITIME(2)
             KDD = ITIME(3)
             KTIME = KTIME + KTAU(IBULL) * 100
             GO TO 4170
 4168        KTIME = KTIME - 2400
             KDD = KDD + 1
 4170        CONTINUE
             IF (KTIME.GE.2400) THEN
                 GO TO 4168
             END IF
             GO TO 4174
 4172        KDD = KDD - MDAYS(MM)
             MM = MM + 1
             IF (MM.GT.12) THEN
                 MM = 1
             END IF
 4174        CONTINUE
             IF (KDD.GT.MDAYS(MM)) THEN
                 GO TO 4172
             END IF
             KTIME = KTIME + KDD * 10000
             INPOS = 8
             N1 = 1
             N2 = 6
             CALL W3AI15 (KTIME,line,N1,N2,'-')
             do i = 1, 6
               ktty(i+inpos-1,klin) = line(i:i)
             enddo
             INPOS = INPOS + N2
             INPOS = 15
             KTTY(INPOS,KLIN) = KT(1:1)
             INPOS = 16
             KTTY(INPOS,KLIN) = KT(2:2)
             INPOS = 17
             KCNT(KLIN) = INPOS - 1
             WRITE (6,100)(KTTY(N,KLIN),N=1,69)
             KLIN = KLIN + 1
C                                                                       
C         FOR EACH LATITUDE BAND                                        
C                                                                       
             JLAT=NLAT(IBULL)
             LATEND = LATSIX + (NLAT(IBULL) - 1) * ILTINC
             LONEND = LONSIX + (NLON(IBULL)-1) * ILNINC
             DO 4500 MLAT = 1, JLAT
                 ILAT = LATSIX - ((MLAT-1) * IABS(ILTINC))
C                                                                       
C              LATITUDE INSERT                                          
C                                                                       
                 VALLAT = (10*ULCLAT(IBULL))+(MLAT-1)*(ILTINC*2.5)*10.
                 IF (KHEM.EQ.2) VALLAT = (10*ULCLAT(IBULL))+(MLAT-1)
     *                    * (-ILTINC*2.5) * 10.
                     LATVAL = VALLAT + .5
                     N1 = 1
                     N2 = 3
c                    CALL W3AI15 (LATVAL,KTTY(1,KLIN),N1,N2,'-')        
                     CALL W3AI15 (LATVAL,line,N1,N2,'-')
                     do i = 1, 3
                       ktty(i,klin) = line(i:i)
                     enddo
                     KTTY(4,KLIN) = ALFAN
                     IF (KHEM.EQ.2) KTTY(4,KLIN) = ALFAS
C                                                                       
C              LONGITUDE INSERT, IF ON FIRST LATITUDE LABEL LINE        
C                                                                       
                         INPOS = 5
                         IF (KLIN.GT.3) GO TO 4203
                             VALLON = ULCLON(IBULL) * 10
                             LONVAL = VALLON + .5
                             KLON=NLON(IBULL)
                             EWINDC=B(IBULL)
                             DO 4200 I = 1, KLON
                                 K = 4
                                 IF (I.EQ.1) THEN
                                     K = 2
                                 END IF
C                                                                       
C               INSERT LEAD SPACES                                      
C                                                                       
                                 INPOS = INPOS + K
C                                                                       
C              INSERT LONGITUDE VALUE                                   
C                                                                       
                                 N1 = 1
                                 N2 = 4
            CALL W3AI15 (LONVAL,line,N1,N2,'-')
                       do ii = 1, 4
                         ktty(ii+inpos-1,klin) = line(ii:ii)
                       enddo
                                 INPOS = INPOS + N2
                                 KTTY(INPOS,KLIN) = EWINDC
                                 INPOS = INPOS + 1
                                 IF(LONVAL.GE.1800) THEN
                                     EWINDC=ALFAW
                                 END IF
                                 IF(LONVAL.GT.1800) THEN
                                     LONVAL=LONVAL-(LONVAL-180)
                                 END IF
                                 IF (EWINDC.EQ.ALFAE) THEN
                                     GO TO 4180
                                 END IF
                                 LONVAL = LONVAL - ILNINC * 25
                                 GO TO 4200
 4180                            LONVAL = LONVAL + ILNINC * 25
 4200                        CONTINUE
 4203                        KCNT(KLIN) = INPOS -1
                             WRITE (6,100)(KTTY(N,KLIN),N=1,69)
                             KLIN = KLIN + 1
C                                                                       
C              PROCESS TROPOPAUSE HEIGHTS IF FLAG IS ON                 
C                                                                       
                             IF (MAXTRP(IBULL).NE.1) THEN
                                 GO TO 4250
                             END IF
C                                                                       
C              TROPOPAUSE HEIGHTS                                       
C                                                                       
                             INPOS = 1
                             DO 4205 I = 1, 4
                                 KTTY(INPOS,KLIN) = TROP1(I)
                                 INPOS = INPOS + 1
 4205                        CONTINUE
                             DO 4210 ILONG=LONSIX,LONEND,ILNINC
                                 K = 1
                                 IF (INPOS.GT.5) K = 6
                                 INPOS = INPOS + K
                                 N1 = 1
                                 N2 = 3
c                                CALL W3AI15 (ITROP(ILONG,ILAT),        
c    *                                KTTY(INPOS,KLIN),N1,N2,'-')       
                                 CALL W3AI15 (ITROP(ILONG,ILAT),
     *                                line,N1,N2,'-')
                                 do ii = 1, 3
                                   ktty(ii+inpos-1,klin) =
     &                              line(ii:ii)
                                 enddo
                                 INPOS = INPOS + N2
 4210                        CONTINUE
                             KCNT(KLIN) = INPOS - 1
                             WRITE (6,100)(KTTY(N,KLIN),N=1,69)
                             KLIN = KLIN + 1
C                                                                       
C              MAX WIND INFORMATION                                     
C                                                                       
                             INPOS = 1
                             DO 4220 I = 1, 4
                                 KTTY(INPOS,KLIN) = MAXW1(I)
                                 INPOS = INPOS + 1
 4220                        CONTINUE
                             KLON=NLON(IBULL)
                             DO 4130 ILONG=LONSIX,LONEND,ILNINC
                                 KTTY(INPOS,KLIN) = SPACE
                                 INPOS = INPOS + 1
                                 N2 = 3
             CALL W3AI15 (MAXHGT(ILONG,ILAT),line,N1,N2,'-')
                                 do ii = 1, 3
                                   ktty(ii+inpos-1,klin) =
     &                               line(ii:ii)
                                 enddo
                                 INPOS = INPOS + N2
                                 N2 = 5
             CALL W3AI15 (IDDFF(ILONG,ILAT,10),line,
     *                    N1,N2,'-')
                                 do ii = 1, 5
                                   ktty(ii+inpos-1,klin) =
     &                               line(ii:ii)
                                 enddo
                                 INPOS = INPOS + N2
 4130                        CONTINUE
                             KCNT(KLIN) = INPOS -1
                             WRITE (6,100)(KTTY(N,KLIN),N=1,69)
                             KLIN = KLIN + 1
C                                                                       
C              WINDS BY LEVELS                                          
C                                                                       
 4250                        DO 4400 J = 1, 9
                                  KLVL = 10 - J
                                  INPOS=1
                                  IF (LVL(KLVL,IBULL).NE.1) THEN
                                      GO TO 4400
                                  END IF
C                                                                       
C              ENTER HEIGHT VALUE FOR THIS BULLETIN LINE                
C                                                                       
                                  KTTY(INPOS,KLIN) = ALFAF
                                  INPOS = INPOS  + 1
                                  N1 = 1
                                  N2 = 3
                            CALL W3AI15 (LVLNNN(KLVL),line,
     *                   N1,N2,'-')
                                  do ii = 1, 3
                                    ktty(ii+inpos-1,klin) =
     &                                line(ii:ii)
                                  enddo
                                  INPOS = INPOS + N2
C                                                                       
C         WINDS BY LONGITUDE                                            
C                                                                       
                                  DO 4300 ILONG=LONSIX,LONEND,ILNINC
                                      LONPTR = ILONG
                                      IF (LONPTR.GE.145) THEN
                                          LONPTR = LONPTR - 144
                                      END IF
C                                                                       
C              INSERT LEAD SPACE                                        
C                                                                       
                                      KTTY(INPOS,KLIN) = SPACE
                                      INPOS = INPOS + 1
C                                                                       
C              MOVE WIND TO OUTPUT LOCATION                             
C                                                                       
                                      N1 = 1
                                      N2 = 5
                                      IDF     = IDDFF(ILONG,ILAT,KLVL)
                                     IF (IDF.LT.1000.AND.IDF.GE.0) THEN
                                      IDDFF(ILONG,ILAT,KLVL)= IDF+36000
                                      END IF
              CALL W3AI15 (IDDFF(ILONG,ILAT,KLVL),WORKA,N1,N2,'-')
                                      DO 4275 N=1,N2
                                          KTTY(INPOS,KLIN)=WORKB(N)
                                          INPOS=INPOS+1
 4275                                 CONTINUE
C                                                                       
C              MOVE TEMPERATURE TO OUTPUT LOCATION                      
C                                                                       
                                      KTTY(INPOS,KLIN)= ALFAM
                                   IF(ITEMP(ILONG,ILAT,KLVL).GE.0) THEN
                                          KTTY(INPOS,KLIN)=ALFAP
                                      END IF
                                      INPOS=INPOS+1
                                     KTEMP=IABS(ITEMP(ILONG,ILAT,KLVL))
                                      N2 = 2
                                    CALL W3AI15 (KTEMP,WORKA,N1,N2,'-')
                                      DO 4280 N=1,N2
                                          KTTY(INPOS,KLIN)=WORKB(N)
                                          INPOS=INPOS+1
4280                                  CONTINUE
4300                              CONTINUE
C                                                                       
C               WRITE LINE TO ARCHIVE/TTY OUTPUT                        
C                                                                       
                                  KCNT(KLIN)=INPOS-1
                                  WRITE (6,100)(KTTY(N,KLIN),N=1,69)
  100                             FORMAT (1X,69A1)
                                  KLIN=KLIN+1
C                                                                       
 4400                         CONTINUE
C                                                                       
 4500                     CONTINUE
C                                                                       
C              CALL ROUTINE TO FORMAT TTY MSG FOR TRANSMISSION          
C                 & PASS TO TRAN FILE                                   
C                                                                       
C                                                                       
C  -------------------------------------------------------              
C               COMMUNICATIONS PREFIX                                   
                         ICAT      = NCAT(IBULL)
                         AREG(1:6) = BLANKS(1:6)
                         IBCKUP    = 0
                         IDAYTP    = 3
c                        CALL W3FI61(MOUT,ICAT,AREG,IBCKUP,IDAYTP,IERR) 
                 call w3ai15(icat,line,1,7,'-')
                 do ii = 1, 40
                    mout(ii) = space
                 enddo
                 do ii = 3, 7
                    mout(ii) = line(ii-2:ii-2)
                 enddo
                 write(mout,4510)
 4510            format("'")
                 mout(2) = '1'
C  -------------------------------------------------------              
                          MPTR = 41
C                                                                       
C         DO ALL TTY LINES                                              
C                                                                       
                          DO 4550 II= 1, KLIN
C                                                                       
C           DO ALL CHARACTERS IN TTY LINE                               
C                                                                       
                              JCNT = KCNT(II)
                              DO 4525 J = 1, JCNT
                                  MOUT(MPTR) = KTTY(J,II)
                                  MPTR = MPTR + 1
                                  IF (MPTR.LE.1280) GO TO 4525
c                                 WRITE (KTRAN)(MOUT(N),N=1,1280),
c    &                              lfascii
                      WRITE (KTRAN,REC=IREC)(MOUT(N),N=1,1280),lfascii
                      irec=irec+1
C                                 WRITE (6,5812)(MOUT(N),N=1,1280)      
                                  DO 4522 K = 1, 1280
                                      MOUT(K) = SPACE
 4522                             CONTINUE
                                  MPTR = 1
 4525                         CONTINUE
C                                                                       
C            CRCRLF INSERTION                                           
C                                                                       
                              DO 4540 J = 1,3
                                  IF (J.LE.2) GO TO 4530
                                  MOUT(MPTR) = LF
                                  GO TO 4535
 4530                             MOUT(MPTR) = CR
 4535                             MPTR = MPTR + 1
                                  IF (MPTR.LE.1280) GO TO 4540
c                                 WRITE (KTRAN)(MOUT(N),N=1,1280),
c    &                              lfascii
                      WRITE (KTRAN,REC=IREC)(MOUT(N),N=1,1280),lfascii
                      irec=irec+1

C                                 WRITE (6,5812)(MOUT(N),N=1,1280)      
                                  MPTR = 1
                                  DO 4537 K = 1, 1280
                                      MOUT(K) = SPACE
 4537                             CONTINUE
 4540                         CONTINUE
C                                                                       
 4550                     CONTINUE
C            END OF BULLETIN FOR TRAN, INSERT ETX                       
                          MOUT(MPTR)=ETX
                          MPTR=MPTR + 1
                          IF(MPTR.LE.1280) GO TO 4551
c                         WRITE(KTRAN)(MOUT(N),N=1,1280),
c    &                      lfascii
                      WRITE (KTRAN,REC=IREC)(MOUT(N),N=1,1280),lfascii
                      irec=irec+1

                          MPTR=1
 4551                     CONTINUE
C                                                                       
C             IF BLOCK CONTAINS DATA, WRITE TO TRAN FILE                
C                                                                       
                          IF (MPTR.LE.1) GO TO 4560
c                         WRITE (KTRAN)(MOUT(N),N=1,1280),
c    &                      lfascii
                      WRITE (KTRAN,REC=IREC)(MOUT(N),N=1,1280),lfascii
                      irec=irec+1

 4599                     FORMAT(16(80A1))
C                         WRITE (6,5812)(MOUT(N),N=1,1280)              
                          DO 4545 K=1,1280
                              MOUT(K)=SPACE
 4545                     CONTINUE
 4560                 CONTINUE
                      WRITE (6,110)
  110                 FORMAT(1X,'     ')
                      WRITE (6,110)
                      BULCNT=BULCNT+1
                      MOUT(MPTR)=KETX
 4600             CONTINUE
C              TRANSMISSION SECTION                                     
C                                                                       
C                                                                       
C              ARE THERE ANY BULLETINS READY FOR TRANSMISSION           
C                                                                       
                  IF (BULCNT.GT.0) GO TO 8000
      CALL W3TAGE('WINTEMV') 
                  GO TO 9999
C                                                                       
C5812             FORMAT (16(1X,80A1,/))                                
C                                                                       
C         RETURN FOR NEXT BULLETIN                                      
 8000         CONTINUE
 9000     CONTINUE
C                                                                       
          KRET = 0
C                                                                       
C              SWITCH TO PREVENT UNWANTED TRANSMISSIONS                 
C              COMMENT OUT NEXT LINE TO PERMIT TRANSMISSION OF BULLETINS
C         IF (KRET.EQ.0) GO TO 9800                                     
C                                                                       
C                                                                       
c     REWIND KTRAN                                                      
c     CALL W3AG15 ('FT24F001','TRAN    ',KRET)                          
c     IF (KRET.NE.0) THEN                                               
c         PRINT *,'ERROR RETURN FROM W3AG15 =',KRET                     
c         WRITE(6,910)                                                  
c     END IF                                                            
C                                                                       
 9999 CONTINUE
      CALL W3TAGE('WINTEMV') 
 4999 STOP
  910 FORMAT (' BULLETINS WERE NOT POSTED')
  920 FORMAT (' ',8A1,2X,44A1,2X,8A1,2X,6A1,2X,A1)
      END
