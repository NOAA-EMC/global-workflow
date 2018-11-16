      SUBROUTINE AFPLTF(LVLDES,IDUMPT,KADDZ,ITOUT,IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    AFPLTF      FORMS THE AFOS PLOTFILE MAPS.
C   PRGMMR: LILLY            ORG: W/NMC412   DATE: 93-05-04
C
C ABSTRACT: FORMS THE AFOS PLOTFILE FOR THE UPPER AIR 2 DOT RUN.
C
C PROGRAM HISTORY LOG:
C   89-04-28  ORIGINAL AUTHOR HENRICHSEN
C   93-06-04  LILLY CONVERT SUB. TO FORTRAN 77
C
C USAGE:    CALL AFPLTF(LVLDES,IDUMPT,KADDZ,ITOUT,IERR)
C   INPUT ARGUMENT LIST:
C              - LGEND TO PUT ON THE PLOTFILE MAPS.
C     LVLDES   - FLAG DENOTING MB LRVEL DESIRED FOR SUB B4PLOT.
C     IDUMPT   - INTEGER   2 WORD ARRAY CONTAINING THE DUMP TIME
C     KADDZ    - STANDARD HEIGHT TO BE ADDED TO D VALUE FOR SUB
C              - B4PLOT.
C     ITOUT    - OPTION FLAG FOR SUB B4PLOT.
C     COMMON /IAFOS /NUMAFS,NAMPIL(10),LVERSN,AFOS,SEND,CARD,MARG,PNCH
C              - LVERSN 36 BYTE ARRAY THAT CONTAINS THE VERSION NAME.
C              - AFOS A LOGICAL   FLAG SET TO .TRUE. OR .FALSE.
C
C   OUTPUT ARGUMENT LIST:
C     IERR     - ERROR RETURN
C              - = 0 GOOD RETURN.
C              - = 1 IF NO MATCH ON PIL LIST.
C              - = 2 IF AN ERROR RETURNED FROM SUB AFORMT.
C
C   OUTPUT ARGUMENT LIST:
C**   COMMON /ISPACE / LBLOCK,ICNTOT ... (REPLACED)
C     COMMON /IFORM/ LCNTOT,IBLOCK

C     COMMON / DATE / NYR,NMO,NDA,NHR
C     COMMON /TIMES / NANJI(12)
C
C   OUTPUT FILES:
C     FT06F001 - STANDARD PRINT FILE.
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  NAS
C
C$$$
C
      LOGICAL      W3AI24
      EXTERNAL     W3AI24

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C**   COMMON /ISPACE / LBLOCK,LCNTOT  ... (REPLACED) ...

      INTEGER         NWRDBLOKSZ
      PARAMETER      (NWRDBLOKSZ=2048)
      INTEGER         NBYTBLOKSZ
      PARAMETER      (NBYTBLOKSZ=8*NWRDBLOKSZ)  	!...=16384

      COMMON /IFORM/  LCNTOT,IBLOCK
      INTEGER         LCNTOT
      INTEGER         IBLOCK(NWRDBLOKSZ) 	!... I*8(2048)=I*4(4096)
      CHARACTER*1    C1BLOCK(NBYTBLOKSZ)
      EQUIVALENCE    (IBLOCK(1),C1BLOCK(1))
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


      COMMON /IAFOS / NUMAFS,NAMPIL,JVERSN,AFOS,SEND,CARD,MARG,PNCH
      INTEGER      NAMPIL(10)
      INTEGER      JVERSN(5)
      CHARACTER*1  LVERSN(40)
      EQUIVALENCE (JVERSN(1),LVERSN(1))
      LOGICAL    AFOS
      LOGICAL    SEND
      LOGICAL    CARD
      LOGICAL    MARG
      LOGICAL    PNCH

C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

      COMMON / DATE / NYR,NMO,NDA,NHR
      COMMON /TIMES / NANJI(12)
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C
      CHARACTER*1  BLANK
      DATA         BLANK       / ' ' /
      CHARACTER*1  CZERO
      DATA         CZERO        / '0' /
      CHARACTER*1  LEND(3) 
      DATA         LEND        / 'E','N','D' /
C
      INTEGER      JPILNUM
      CHARACTER*8  CPILNUM
      INTEGER    IDUMPT(2)
      INTEGER    ITIME(8)


      INTEGER      JSSN(2,8)
      CHARACTER*8  CSSN(2,8)
      EQUIVALENCE (JSSN(1,1),CSSN(1,1))
C                          ...   00Z    12Z ...
      DATA         CSSN        /'    ','    ',
     2                          '5140','5640',
     3                          '5190','5690',
     4                          '5240','5740',
     5                          '    ','    ',
     6                          '5340','5840',
     7                          '5390','5890',
     8                          '5440','5940' /

      CHARACTER*1  LEADER(56)
      CHARACTER*8  MEADER(7)
      DATA         MEADER    /'NMCPLT  ',	!...(1:8)
     2                        ' 0010200',	!...(9:16)
     3                        '02048153',	!...(17:24)
     4                        '62850142',	!...(25:32)
     5                        '5+0975+1',	!...(33:40)
     6                        '688HHDDM',  	!...(41:48)
     7                        'MYY     ' /   		!...,Z4040400D/

      INTEGER      JHD1(2)
      CHARACTER*1  CHD1(16)
      EQUIVALENCE (JHD1(1),CHD1(1))
      DATA         JHD1        / 0, X'0200000000000000' /
C     ...      CHARACTER*1  CHD1(10)    /8*Z00,Z02,Z0/ ...

C   
C

      INTEGER      KCONS
      CHARACTER*1  CCONS(8)
      EQUIVALENCE (KCONS,CCONS(1))           
      DATA         LCONS          / X'C5800D0A00020000' /
C                                      1 2 3 4 5 6 7 8
      CHARACTER*1 HLEAD(2)   		!... /ZC5,Z80/
      CHARACTER*1 CARRET    		!... /Z0D/
      CHARACTER*1 LINFED     		!... /Z0A/
      CHARACTER*1 NULL			!... /Z00/
      CHARACTER*1 STX                   !... /Z02/
      EQUIVALENCE (CCONS(1),HLEAD(1))
      EQUIVALENCE (CCONS(3),CARRET)
      EQUIVALENCE (CCONS(4),LINFED)
      EQUIVALENCE (CCONS(5),NULL)
      EQUIVALENCE (CCONS(6),STX)

      CHARACTER*1 ICYCLE
      CHARACTER*1 LPIL(3)

C
      SAVE
C
C     . . . .   s t a r t   . . . . . . . . . . . . . . . . . . . . .
C
      ICYCLE = NULL
      MEADER(7)(8:8) = CARRET

      IFT50  = 50
      NPLOTF = 24
C
      IERR = 0
      L = 1
      IF(NHR .NE. 0) L = 2
C     ...OTHERWISE THIS IS A 12Z REQUEST...
C
C     ...  GET PIL FOR AFOS PRODUCT ...
C
      PRINT *,' LVLDES =',LVLDES
      ISUBNM = JSSN(L,LVLDES)

C     ... WHAT HAS GETPIL() ARGS BEEN CONVERTED TO FOR CRAY ???
      CALL gtapil(ISUBNM,IFT50,CHD1,IRTN)

      IERR = IRTN
      IF(IRTN.NE.0)GO TO 500
C
      WRITE(6,FMT='(1H ,''afpltf: AFOS PIL FROM SUB gtapil=X'',
     1        2Z17.16)')
     A        JHD1(1),JHD1(2)

      NUMAFS = NUMAFS + 1
C
C     ...ASCII PIL NUMBER IS IN CHD1 ARRAY
C
      CALL MOVCH(3,CHD1,1,LPIL,1)
C
C     ...ASCII PIL NUMBER IN CHD1 ARRAY MOVED INTO C*1 LPIL
C
      CPILNUM(1:8) = ' '

      CALL MOVCH(3,CHD1,1,CPILNUM,1)
C
      NAMPIL(NUMAFS) = JPILNUM
C
C
C        NOW CLEAR C1BLOCK WITH BINARY ZEROS.
C
            DO  J=1,NWRDBLOKSZ
               IBLOCK(J) = 0
            ENDDO
C
      LCNTOT = 1
          CALL MOVCH(2,HLEAD,1,C1BLOCK,LCNTOT)
      LCNTOT = LCNTOT + 2
C
C     OCTOBER 24, 1980
C     I HAVE JUST BEEN ADVISED BY DAN STAROSTA THAT THIS AFOS OUTPUT
C     SHOULD CARRY A CREATE TIME, NOT THE BASE TIME, IN ITS
C     COMMUNICATIONS HEADER.  I MUST THEREFORE PASS A CLOCK TIME TO
C     FORMAT IN CHD1.
C
      CALL W3UTCDAT(ITIME) 
C
      IRMIN = ITIME(6) 
      IRELHR = ITIME(5)    
      CHD1(4) = CHAR(NMO)
      CHD1(5) = CHAR(NDA)
      CHD1(6) = CHAR(NHR + IRELHR)
      CHD1(7) = CHAR(IRMIN)
      CHD1(8) = CHAR(NYR)
      CHD1(9) = CHAR(2)
C
C     NANJI(1-2) HAVE YYMMDDHH IN HOLLERTH SO I NEED TO REORDER FOR
C     AFOS TO : HHDDMMYY FOR AFOS PLT HEADER.(MEADER(44-51))
C
         KEY = 50
           DO 10 I = 1,7,2
              CALL MOVCH(2,NANJI,I,MEADER,KEY)
              KEY = KEY - 2
  10       CONTINUE
C
C     CHECK TO SEE IF FIRST BYTE OF IDUMP IS A BLANK IF SO CHANGE TO
C     A HOLLERTH '0'.
C
           IF(W3AI24(IDUMPT,BLANK,1)) then
              CALL MOVCH(1,CZERO,1,IDUMPT,1)
           endif
C
C     ... GET DUMP TIME FROM IDUMPT ARRAY HOWEVER.....
C     ... IDUMPT(1-2) HAS HH+MM  IN HOLLERTH SO I NEED TO REMOVE THE
C     ... THE PLUS FOR AFOS PLT HEADER.(MEADER(52-55))
C
        WRITE(6,FMT='(1H ,''afpltf: AFOS DUMP TIME = '', 2A4)')
     A          IDUMPT
         KEY = 52
           DO I = 1,4,3
              CALL MOVCH(2,IDUMPT,I,MEADER,KEY)
              KEY = KEY + 2
           ENDDO
C
C     ... CONVERT MEADER TO ASCII.
C
C??   CALL EB2ASC(56,MEADER(1),LEADER(1),IERR)
C
C     ... PUT ASCII PILL NUMBER IM LEADER (7-9).
C
      CALL MOVCH(3,LPIL,1,LEADER,7)
C
      CALL MOVCH(56,LEADER,1,C1BLOCK,LCNTOT)
C
      LCNTOT = LCNTOT + 56
      C1BLOCK(LCNTOT) = LINFED
      LCNTOT = LCNTOT + 1
C
C     ... WRITE A PLOTFILE LEGEND
C
      CALL APLGND(IDUMPT)

C     ... why is B4PLOT called from within an AFOS subr???
C??   CALL B4PLOT(LVLDES,KADDZ,ITOUT,MARG,AFOS)
C
C     ... PLACE A VERSION TITLE IN THE AFOS PLOTFILE FORMAT.
C     ... LVERSN  -  LOGICAL   36 BYTE ARRAY THAT CONTAINS THE
C                 -  VERSION TITLE FOR THE AFOS PLOT FILE MAP.
C                 -  LVERSN(5)   /'1600,145','0,00000Z',
C                 -               ',WD412/D','KPH/9.D1',
C                 -               ZF25E0D0A/
C
C
      CALL MOVCH(36,LVERSN,1,C1BLOCK,LCNTOT)
C
      LCNTOT = LCNTOT + 36

      CALL MOVCH(3,LEND,1,C1BLOCK,LCNTOT)

      LCNTOT = LCNTOT + 2
C
C     ... DUMP C1BLOCK ARRAY IF PNCH FLAG IS .TRUE.
C
C???  IF(PNCH) CALL PDUMP(C1BLOCK(1),C1BLOCK(LCNTOT),0)
C
C     ...  CHECK FOR PNCH FLAG TO SEE IF PLOT FILE IS TO BE WRITTEN TO
C     ...    FT28 AS WELL AS FT24.
C
      IPUNCH = 0
      IF(PNCH) IPUNCH = 2

      CALL MOVCH(3,LPIL,1,CHD1,1)

C???  CALL AFORMT(CHD1,IPUNCH,KER)

      IERR = KER
  500 RETURN
      END
