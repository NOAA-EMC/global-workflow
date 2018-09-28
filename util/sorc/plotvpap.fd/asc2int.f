       SUBROUTINE ASC2INT(NCHAR,C1ASCII,INTOUT,IERR)                          
C                                                                               
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                            
C                .      .    .                                       .          
C SUBPROGRAM:    ASC2INT     TRANSLATE ASCII INTO  INTEGER
C   PRGMMR: LIN              ORG: W/NP12     DATE: 97-01-30                     
C                                                                               
C ABSTRACT: TO TRANSLATE AN ASCII ARRAY INTO INTEGER
C   (IN A CHARACTER*1 ARRAY).                                                   
C   WHERE THE "EXTENDED" DISPLAY CODE IS INTEGER
C                                                                               
C PROGRAM HISTORY LOG:                                                          
C   97-01-30  ORIGINAL AUTHOR: LUKE LIN
C                                                                               
C USAGE:    CALL ASC2INT(NCHAR,C1ASCII,INTOUT,IERR)                           
C   INPUT ARGUMENT LIST:                                                        
C     C*1 C1ASCII(NCHAR) - SOURCE CHARACTER*1 ARRAY IN ASCII                    
C                                                                               
C   OUTPUT ARGUMENT LIST:                                                       
C     INTOUT :    output integer
C                             IN EXTENDED DISPLAY CODE                          
C     INT IERR - ERRFLAG                                                        
C              = 0;  NORMAL RETURN                                              
C              = 1;  BAD VALUE GIVEN IN NCHAR                                   
C                                                                               
C   OUTPUT FILES:                                                               
C     FT06F001 - INCLUDE IF ANY PRINTOUT                                        
C                IF GIVEN VALUE OF NCHAR IS ZERO, THEN                          
C                      ERROR MESSAGE IS PRINTED                                 
C                                                                               
C REMARKS:                                                                      
C                                                                               
C ATTRIBUTES:                                                                   
C   LANGUAGE: FORTRAN 77                                                        
C   MACHINE:  CRAY                                                              
C                                                                               
C$$$                                                                            
C      ... TO TRANSLATE TEXT FROM ASCII INTO CDC DISPLAY CODE                   
C      ...   BUT IN EXTENDED DISPLAY CODE OF 8-BITS PER CHAR.                   
C                                                                               
       CHARACTER*1   C1ASCII(NCHAR)                                             
       INTEGER       INTOUT
       INTEGER       IERR                                                       
C                                                                               
C                                                                               
C                                                                               
C      . . . .   S T A R T    . . . . . . . . . . . . . . . . . . . .           
C                                                                               
       IERR = 0                                                                 
       INTOUT = 0
C                                                                               
       NCH2DO = NCHAR                                                           
       IF(NCH2DO .LE. 0) THEN                                                   
         WRITE(6,FMT='(1H ,''ASC2INT: ERROR! YOU GAVE ME A'',                   
     1       '' BAD CHAR COUNT. NCHAR='',I8)')NCHAR                             
         IERR = 1                                                               
       ELSE                                                                     
        DO  LA = 1,NCH2DO                                                       
            ITEMP = MOVA2I(C1ASCII(LA)) - MOVA2I('0')
            IF ( ITEMP .LT. 0 .OR. ITEMP .GT. 9) THEN
                 IERR = 1
                WRITE(6,FMT='(1H ,''ASC2INT: ERROR! YOU GAVE ME A'',
     1              '' BAD CHAR-DIGIT. NCHAR='',I8)')LA
                RETURN
            ENDIF

            INTOUT = INTOUT*10 + ITEMP
        ENDDO                                                                   
       ENDIF                                                                    
       RETURN                                                                   
       END                                                                      
