       SUBROUTINE  BULHDR ( ITME, IDAY, NBLK, NXTBLK )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                       
C                                                          
C SUBPROGRAM: BULHDR         FORMATS THE BULLETIN HEADER LINE
C   PRGMMR: BOB HOLLERN      ORG: W/NP12     DATE: 97-04-17
C                                                          
C ABSTRACT: FORMATS THE ADMN74 KWBC BULLETIN HEADER LINE.
C                                                          
C PROGRAM HISTORY LOG:                                     
C
C   97-04-17  BOB HOLLERN, AUTHOR
C                                                          
C USAGE:  CALL BULHDR( ITME, IDAY, NBLK, NXTBLK )
C                                                          
C   INPUT ARGUMENT LIST:                                   
C
C     ITME      -   INTEGER VARIABLE SET TO THE BULLETIN HEADER GMT
C                   HOUR
C
C     IDAY      -   INTEGER VARIABLE SET TO CURRENT DAY OF MONTH
C
C                                                          
C   OUTPUT ARGUMENT LIST:                                   
C
C     NXTBLK    -   INTEGER VARIABLE SET TO LAST LOCATION IN NBLK
C                   WHERE BULLETIN DATA WAS STORED
C
C     NBLK      -   CHARACTER ARRAY TO HOLD THE ADMN74 KWBC BULLETIN DATA
C
C ATTRIBUTES:                                              
C   LANGUAGE: FORTRAN 90                                   
C$$$                                                       
       CHARACTER*2560   NBLK
C
       CHARACTER*12  BULHD
C
       CHARACTER*7   NSOH
C
       CHARACTER*4   IEOL,   HR
C
       CHARACTER*2   DAYMO
C
       DATA  NSOH / '''100000' /
C
       DATA  IEOL / '<<@ ' /                                                  
C
       DATA  BULHD / 'ADMN74 KWBC ' /
C                                                                               
       DATA  N2560 / 2560 /
C
100    FORMAT ( 1X, 'NBLK = ', 26(/1X, 60A1 ) ) 
C
C      SET NXTBLK = 0 TO INITIALIZE NBLK TO BLANKS
C
       NXTBLK = 0
       CALL  W3AI19( NSOH, 7, NBLK, N2560, NXTBLK )
C
       PRINT *, 'BULDHR:  NXTBLK = ', NXTBLK
       WRITE(6,100) (NBLK(I:I),I=1,NXTBLK)
C
C      BULLETIN HEADER STARTS AT LOCATION 41 IN NBLK
C
       NXTBLK = 40
       CALL  W3AI19( BULHD, 12, NBLK, N2560, NXTBLK )
C
       PRINT *, 'BULDHR:  NXTBLK = ', NXTBLK
       WRITE(6,100) (NBLK(I:I),I=1,NXTBLK)
C
C      PUT DATE/TIME IN HEADER
C
       WRITE( UNIT=DAYMO(1:2), FMT='(I2.2)' ) IDAY
C
       CALL  W3AI19( DAYMO, 2, NBLK, N2560, NXTBLK )
C
       WRITE( UNIT=HR(1:4), FMT='(I4.4)' ) ITME
       CALL  W3AI19( HR, 4, NBLK, N2560, NXTBLK )
       CALL  W3AI19( IEOL, 3, NBLK, N2560, NXTBLK )
C
       PRINT *, 'BULDHR:  NXTBLK = ', NXTBLK
       WRITE(6,100) (NBLK(I:I),I=1,NXTBLK)
C
       RETURN                                                                   
       END                                                                      
