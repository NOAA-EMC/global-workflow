      SUBROUTINE GETOVL(MAXMAP,DGNSED,MAP)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GETOVL      GET OVERALL MANDATORY INFORMATION IN
C   PRGMMR: KRISHNA KUMAR         ORG: W/NP12   DATE: 1999-08-01
C
C ABSTRACT: GET CONSTANTS FROM INPUT CONTROLS.
C
C PROGRAM HISTORY LOG:
C   96-03-01  ORIGINAL AUTHOR  LUKE LIN
C   96-04-30  LUKE LIN      READ IL CONSTANTS IN.
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM
C                           RS/6000. 
C
C USAGE:    CALL GETOVL(MAXMAP,MAP)
C   OUTPUT ARGUMENT LIST:
C     MAXMAP   - NUMBER OF MAPS TO BE MADE IN THIS JOB
C     DGNSED   - UNIT NUMBER FOR DGN SEED FILE
C     MAP      - MAP(15) IL CONSTANTS
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
      COMMON / LNATTR / LNWEIGHT, LNSTYLE, LNCOLOR
      INTEGER      LNWEIGHT
      INTEGER      LNSTYLE
      INTEGER      LNCOLOR
C
      INTEGER       MAXMAP
      INTEGER       DGNSED
      INTEGER       MAP(15)
      CHARACTER*1   COMENT
      CHARACTER*5   CMAXMP
      CHARACTER*5   CILCON1
      CHARACTER*5   CILCON2
      CHARACTER*5   CILCON3
      CHARACTER*5   CILCON4
      CHARACTER*5   COPTION
      CHARACTER*6   BGNAME
      CHARACTER*8   BG2NAME
C
      CHARACTER*80  CARD
C
       integer       iacc
       character*8   cacc
       equivalence  (iacc,cacc)
C
      DATA          CMAXMP    /'TOLM:'/
      DATA          CILCON1   /'ILC1:'/
      DATA          CILCON2   /'ILC2:'/
      DATA          CILCON3   /'ILC3:'/
      DATA          CILCON4   /'ILC4:'/
      DATA          COMENT    /'!'/
      DATA          CACC      /'       '/
C
C
C ------------ STARTS ----------------------------------
C
C
      LNWEIGHT = 0
      LNSTYLE = 0
      LNCOLOR = 1
C
C
  100 CONTINUE
C
C     .... READ ONE INPUT CARD
C
         READ(15,FMT='(A)')CARD(1:80)
         WRITE(6,FMT='('' '',A)')CARD(1:80)
         IF ( CARD(1:1) .EQ. COMENT ) GOTO 100
C        ... IT IS A COMMENT CARD
         COPTION = CARD(3:7)
C
         IF (COPTION .EQ. CMAXMP) THEN
C
C            READ THE TOTAL MAP
C
             READ(CARD,FMT='(19X,I4,11X,I2)')MAXMAP,DGNSED
C
             PRINT *,' TATOL MAPS =',MAXMAP,' DGNSED=',DGNSED
C
         ELSE IF (COPTION .EQ. CILCON1) THEN
             READ(CARD,FMT='(8X,A6,9(1X,I4))')BGNAME,(MAP(NN),NN=2,10)
             CACC(1:6)=BGNAME
             MAP(1) = IACC
             PRINT *, ' BACKGROUND=',CACC
         ELSE IF (COPTION .EQ. CILCON2) THEN
             READ(CARD,FMT='(14X,5(1X,I4))')(MAP(NN),NN=11,15)
             RETURN
C
         ELSE IF (COPTION .EQ. CILCON3) THEN
             READ(CARD,FMT='(8X,A8,1X,I4,2(2(1X,I6),2(1X,I4)))')
     1            BG2NAME,(MAP(NN),NN=2,10)
             CACC=BG2NAME
             MAP(1) = IACC
             PRINT *, ' BACKGROUND=',CACC
         ELSE IF (COPTION .EQ. CILCON4) THEN
             READ(CARD,FMT='(14X,5(1X,I6))')(MAP(NN),NN=11,15)
             RETURN
         ELSE
            PRINT *, ' ***FATAL ERROR: UNRECOGNIZE OPTION:',COPTION
         ENDIF
C
      GO TO 100
C
      RETURN
      END
