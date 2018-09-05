      SUBROUTINE LABELCON(FLD1,IMAX,JMAX,DOTSGI,A,B,KDN,LDN,IRETUR) 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    LABELCON    PUT CONTOUR LABELS.
C   PRGMMR: KRISHNA KUMAR       ORG: W/NP12    DATE: 1999-08-01
C
C ABSTRACT: LABEL CENTERS BY CALLING CENTRE AND LABEL THE CONTOURS
C  BY CALLING CLABEL AND CLOSEC.
C
C PROGRAM HISTORY LOG:
C   94-10-31  ORGIONAL AUTHOR HENRICHSEN
C   94-11-28  HENRICHSEN/   PUT LOGIC TO USE OFF SET LABELS FOR
C             LIN           CONTOR STRIP LABELS AND LABELS ABOVE
C                           CONTOURS.
C   94-12-19  HENRICHSEN    CHANGE JUP TO 2 FOR BOUNDARY LAYER RH.
C   94-12-22  LUKE LIN      CONVERT IT CFT-77.
C   96-02-22  LUKE LIN      ADD THE LOGIC TO TRAJECTORY VERTICAL
C                           DISPLACEMENT AND K INDEX MAPS.
C   96-05-30  LUKE LIN      MODIFY CONTOUR LABEL POSITION FOR NGM
C   96-06-12  LUKE LIN      MODIFY CONTOUR LABEL LOGIC FOR CENTRE, CLOSEC,
C                           AND LABEL ROUTINES.
C   96-09-12  LUKE LIN      MODIFY FOR CGRID PRECIP CHARTS.
C   96-10-03  LUKE LIN      MODIFY FOR TROPOPAUSE PRESSURE.
C   96-11-04  LUKE LIN      MODIFY FOR PFAX1 TEMPERATURE PLOTS.
C   97-02-18  LUKE LIN      ADD KEYIDX 34 FOR HIGHT CHANGE WITHOUT CENTERS.
C   97-03-12  LUKE LIN      MODIFY FOR PROB. THUNDERSTORMS/SEVERE WX CENTERS.
C   97-12-04  LUKE LIN      MODIFY KEYIDX 39 FOR GEO REL VORTICITY.
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM
C                           RS/6000. 
C
C USAGE:    CALL LABELCON(Z,IMAX,JMAX,DOTSGI,A,B,KDN,LDN,IRETUR)
C   INPUT ARGUMENT LIST:
C     FLD1     - A GIVEN GRIDPOINT SCALED DATA FIELD
C     IMAX     - I-DIMENSION OF GRIDPOINT DATA FIELD
C     JMAX     - J-DIMENSION OF GRIDPOINT DATA FIELD
C     DOTSGI   - IS DOTS PER GRID INTERVAL
C              - WHERE EACH DOT IS 1/100TH INCH ON VARIAN
C              - NEGATIVE DOTSGI SIGNALS MERC SRN HEMI OPTION
C     A        - IS ADDITIVE CONSTANT
C     B        - IS MULTIPLICATIVE CONSTANT
C              - WHERE TRUE Z VALUE = (Z + A) * B
C     KDN      - LOGICAL*1 FLAG =.TRUE. IF THIS A KDN FIELD.
C     LDN      - LOGICAL*1 FLAG =.TRUE. IF THIS A DN FIELD.
C
C   OUTPUT ARGUMENT LIST:
C     IRETUR   - RETURN CONDITIONS FROM SUBS CALLED.
C              - = 0, NORMAL
C              - = 1, KEYIDX ERROR 
C
C
C   OUTPUT FILES:
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C
      COMMON /LABG/ GULPXX(2),GULPXY(2),LVFLG,NUMG,GLAB
C
      INTEGER       GULPXX,GULPXY,LVFLG,NUMG
      CHARACTER*24  GLAB
C
      INTEGER  IMAX, JMAX
      REAL     FLD1(IMAX,JMAX)
C
      COMMON   /PUTARG/PUTHGT,PUTANG,IPRPUT(2),ITAPUT
C
C
      COMMON /ADJ1/ICOR,JCOR
      COMMON /ADJ2/ XIDID,YJDID
      COMMON /ADJ3/ IRTCOR,IUPCOR
C
      COMMON /MUTCON/ KEYIDX,UA1V,UA2V,UA3V,UM1V,UM2V,UM3V,LINEVU,LINEP,
     X               IGRIDP,LEVEL
C
C
      COMMON /POLE/ XPOL,YPOL,GDTYPE
C
C     ...THE POLE POSITION IN GRID(65,65) IS AT GRID(33,33).
C     ...THE POLE POSITION IN GRID(53,45) IS AT GRID(27,49).
C     ...THE POLE POSITION IN GRID(53,57) IS AT GRID(27,49).
C
      COMMON /CENTR/ CENTFLAG,CENTFONT,CENTNC,CENTCEN,CENTB1RC,
     1               CENTXID,CENTYID,CENTXLIM,CENTFLO,CENTFHI,
     2               CENTFORM
      LOGICAL        CENTFLAG
      REAL           CENTFONT,CENTXID,CENTYID,CENTXLIM,CENTB1RC
      CHARACTER*4    CENTFORM,CENTFLO,CENTFHI
      INTEGER        CENTNC,CENTCEN
C
      COMMON /CENTV/ CENVFLAG,CENVFONT,CENVNC,CENVJUP,
     1               CENVICR,CENVJCR,CENVB1RC,CENVFORM,CENVIFF
      LOGICAL        CENVFLAG
      REAL           CENVFONT,CENVB1RC
      CHARACTER*4    CENVFORM,CENVIFF
      INTEGER        CENVNC,CENVJUP,CENVICR,CENVJCR
C
      COMMON /FIXLAB/ LABFLAG,LABFONT,LABNC,LABRTC,LABUPC,
     1                LABNLAB,LABIJFIX(7),LABB1RC,LABIFF,LABFORM
      LOGICAL         LABFLAG
      CHARACTER*4     LABFORM,LABIFF
      REAL            LABFONT, LABB1RC
      INTEGER         LABNC,LABRTC,LABUPC,LABNLAB,LABIJFIX
C
C
      CHARACTER*4 IFORM
      CHARACTER*4 IFOR01
      CHARACTER*4 IFOR02
      CHARACTER*4 IFOREL
      CHARACTER*4 IFOR06
C
      CHARACTER*4 FORMC
      CHARACTER*4 FORMT
      CHARACTER*4 FORMV
      REAL        FONTC
      REAL        FONTT
      REAL        FONTV
C
      INTEGER   GDTYPE
      INTEGER   ITABMB(7)
      INTEGER   ITABFL(7)
C
      CHARACTER*8 IFF1(5)
      CHARACTER*8 IFFX(5)
      CHARACTER*8 IFFN(5)
      CHARACTER*8 IFF2(5)
      CHARACTER*8 IFF3(5)
      CHARACTER*8 IFF4(5)
      CHARACTER*8 IFF5(5)
      CHARACTER*8 IFG2(5)
      CHARACTER*8 IFFT(5)
      CHARACTER*8 IFFS(5)
C
      CHARACTER*8 IFLO(5)
      CHARACTER*8 IFHI(5)
      CHARACTER*8 IFFV(5)
      CHARACTER*8 IFFC(5)
C
      INTEGER     CIFF1(5)
      INTEGER     CIFFX(5)
      INTEGER     CIFFN(5)
      INTEGER     CIFF2(5)
      INTEGER     CIFF3(5)
      INTEGER     CIFF4(5)
      INTEGER     CIFF5(5)
      INTEGER     CIFG2(5)
      INTEGER     CIFFT(5)
      INTEGER     CIFFS(5)
C
      INTEGER     IRTCRC,IUPCRC
      INTEGER     ICORV,JCORV
      INTEGER     NCT,NCV,NCC
C    
      EQUIVALENCE (IFF1(1),CIFF1(1))
      EQUIVALENCE (IFFX(1),CIFFX(1))
      EQUIVALENCE (IFFN(1),CIFFN(1))
      EQUIVALENCE (IFF2(1),CIFF2(1))
      EQUIVALENCE (IFF3(1),CIFF3(1))
      EQUIVALENCE (IFF4(1),CIFF4(1))
      EQUIVALENCE (IFF5(1),CIFF5(1))
      EQUIVALENCE (IFG2(1),CIFG2(1))
      EQUIVALENCE (IFFT(1),CIFFT(1))
      EQUIVALENCE (IFFS(1),CIFFS(1))
C
      INTEGER   NLAB
      INTEGER   ICEN
      INTEGER   HJFIX(5)
      INTEGER   IIFIX(3)
      INTEGER   JJFIX(5)
      INTEGER   KJFIX(5)
      INTEGER   KKFIX(3)
      INTEGER   LJFIX(5)
      INTEGER   SLFIX(5)
      INTEGER   MJFIX(5)
      INTEGER   MMFIX(5)
      INTEGER   TMPFIX(5)
      INTEGER   IJFIXC(5)
      INTEGER   IJFIX
      INTEGER   KCFIX(12)
      INTEGER   MXITR
C
      INTEGER   M(2)
C
      LOGICAL   KDN
      LOGICAL   LDN
C
      LOGICAL   FGCENT
      LOGICAL   FGCENV
      LOGICAL   FGLABC
C
      DATA        IFOR01     /'A+-+'/
      DATA        IFOR02     /'A999'/
      DATA        IFOREL     /'A..9'/
      DATA        IFOR06     /'S999'/
C     DATA      IFF1         /4H(A1,,4H1H$),0,0,0/
C     DATA      IFF2         /4H(A2,,4H1H$),0,0,0/
C     DATA      IFF3         /4H(A3,,4H1H$),0,0,0/
C     DATA      IFF4         /4H(A4,,4H1H$),0,0,0/
C     DATA      IFG2         /4H(A3,,4H2HK$,1H),0,0/
      DATA      CIFF1        /8H(A1,1H$),0,0,0,0/
      DATA      CIFFX       /8H(2HX$,A1,1H),0,0,0/
      DATA      CIFFN       /8H(2HN$,A1,1H),0,0,0/
      DATA      CIFF2        /8H(A2,1H$),0,0,0,0/
      DATA      CIFF3        /8H(A3,1H$),0,0,0,0/
      DATA      CIFF4        /8H(A4,1H$),0,0,0,0/
      DATA      CIFF5        /8H(1HF,A3,,8H1H$)    ,0,0,0/
      DATA      CIFG2        /8H(A3,2HK$,8H)       ,0,0,0/
      DATA      CIFFT        /8H(1HT,A2,,8H1H$)    ,0,0,0/
      DATA      CIFFS        /8H(1HS,A2,,8H1H$)    ,0,0,0/
C
      DATA      HJFIX        /-13,-21,-29,-37,-45/
      DATA      IIFIX        /-17,-29,-43/
      DATA      JJFIX        /-8,-16,-24,-32,-40/
      DATA      KJFIX        /-5,-13,-21,-29,-37/
      DATA      SLFIX        /-6,-14,-22,-30,-38/
      DATA      KKFIX        /-27,-32,-37/
      DATA      LJFIX        /-16,-24,-32,-40,-48/
      DATA      MJFIX        /-7,-15,-23,-31,-39/
      DATA      MMFIX        /-10,-18,-26,-34,-42/
C
      DATA      ITABMB/400,350,300,250,200,150,100/
      DATA      ITABFL/240,270,300,340,390,450,530/
      DATA      MXITR /7/
C     DATA      KCFIX/0,-4,-8,-12,-16,-20,-24,-28,-32,-36,-40,-44/
      DATA      KCFIX/-1,-5,-9,-13,-17,-21,-25,-29,-33,-37,-41,-45/
C
C      SKIP KEYIDX 51 AND ABOVE
C
      IF (KEYIDX.GT.51) THEN
         PRINT *, ' INVALID KEYDIX = ', KEYIDX
         IRETUR = 1
      ENDIF
C
C     print *,' in subroutine lbelcon'
      print *,' keyidx=', keyidx
C     print *,' max=', imax, jmax
C
      PUTHGT = 1.0
      PUTANG = 0.0
      FONTC = 1.0
      FONTT = 1.0
      FONTV = 1.0
      IPRPUT(1) = 0
      IPRPUT(2) = 0
      IRETUR = 0
      NLAB = 5
      FGCENT = .FALSE.
      FGCENV = .FALSE.
      FGLABC = .FALSE.
      IRTCOR = 0
      IUPCOR = 0
      ICOR = 0
      JCOR = 0
      ICORV = 0
      JCORV = 0
      IRTCRC = 0
      IUPCRC = 0
      FORMC = 'A999'
      FORMT = 'A999'
      FORMV = 'A999'
C
      A1 = A
      B1 = B
      XIDID = -5.0
      YJDID = -5.0
      S = DOTSGI / 60.0
      M(1) = 0
      M(2) = 3
      NCT = 3
      NCV = 3
      NCC = 3
C
C          CHECK TO SEE WHAT LIMIT VALUE SHOULD BE.
C
      IF (KEYIDX.EQ.5 .OR. KEYIDX.EQ.12 .OR.
     1       KEYIDX.EQ.19 .OR. KEYIDX.EQ.35 .OR.
     2       KEYIDX.EQ.42 .OR. KEYIDX.EQ.46 .OR.
     3       KEYIDX.EQ.47 .OR. KEYIDX.EQ.48 .OR.
     4       KEYIDX.EQ.49) THEN
C             CENTERS MIGHT BE NEGATIVE
              XLIM = -500.0
      ELSE IF (KEYIDX.EQ.20 .OR. KEYIDX.EQ.21 .OR.
     1             KEYIDX.EQ.28 .OR. KEYIDX.EQ.29) THEN
C             THIS IS A LIFTED INDEX FILED
              XLIM = -100.0
      ELSE IF (KEYIDX.EQ.9 .OR. KEYIDX.EQ.10) THEN
C             THIS IS A VORTICITY FIELD.
              XLIM=-1.0
      ELSE IF (KEYIDX.EQ.39) THEN
C             THIS IS A GEO REL. VORTICITY FIELD.
              XLIM=-100.0
      ELSE
              XLIM=1.0
      ENDIF
C
C          CHECK FOR PRESSURE OR HEIGHT IF TRUE SET FLAG TO
C          PUT ON THE BIG "H" & "L" CENTERS.
C
      IF (KEYIDX.EQ.1 .OR.  KEYIDX.EQ.18 .OR. KEYIDX.EQ.2
     1   .OR. KEYIDX.EQ.15 .OR. KEYIDX.EQ.26 .OR. KEYIDX.EQ.24
     2       .OR. KEYIDX.EQ.31 .OR. KEYIDX.EQ.32) THEN
C             ....PUT BIG H AND L
            ICEN = 2
C           print *,' icen =2'
      ELSE
            ICEN = 1
      ENDIF
C
      IF (KEYIDX. EQ. 22)THEN
C
C          THIS IS A PRESSURE OR HEIGHT CHANGE FIELD
C
             NCT  = 3
             XLIM  = -100.0
             ICEN = 1
             FORMT = 'A+-+'
C
C            FIND AND LABEL THE PRESSURE OR HEIGHT CHANGE CENTERS
C
             FGCENT = .TRUE.
             DO I=1,5
                IFLO(I) = IFF3(I)
                IFHI(I) = IFF3(I)
             ENDDO
C
C            CALL CENTRE(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,XLIM,ICEN,
C    1                   IFORM,IFLO,IFHI)
C
C            LABEL THE LINES ABOVE THE CENTERS
C
             JUP=2
             NCV  = 3
             ICORV = -15
             JCORV = -10
             FGCENV = .TRUE.
             DO I=1,5
                IFFV(I) = IFF3(I)
             ENDDO
             FORMV = 'A+-+'
C            CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,
C    1                   IFORM,IFFV)
C
C             PUT PRESSURE OR HEIGHT CHANGE STRIP LABELS .
C
             NCC  = 3
             IRTCRC = -15
             IUPCRC =  -10
             FGLABC = .TRUE.
             DO I=1,5
                IFFC(I) = IFF3(I)
             ENDDO
             FORMC = 'A+-+'
             FGLABC = .TRUE.
             DO K3 = 1,NLAB
                IJFIXC(K3) = KJFIX(K3)
C               CALL CLABEL(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,
C    1                   IFORM,IFFC)
             ENDDO
      ELSE IF (KEYIDX. EQ. 23)THEN
C
C          THIS IS A HEIGHT CHANGE FIELD
C
             PUTHGT=11.0
             NCT  = 4
             XLIM  = -500.0
             ICEN = 1
             FORMT = 'A+-+'
C
C            FIND AND LABEL THE HEIGHT CHANGE CENTERS
C
             FGCENT = .TRUE.
             DO I=1,5
                IFLO(I) = IFF4(I)
                IFHI(I) = IFF4(I)
             ENDDO
C
      ELSE IF (KEYIDX.EQ.1 .OR.  KEYIDX.EQ.31 .OR. KEYIDX.EQ.26)THEN
C
C            THIS IS A PRESSURE FILED.
C
             NCT  = 3
             FORMT = 'A999'
C
C
C            FIND AND LABEL THE PRESSURE CENTERS
C
             FGCENT = .TRUE.
             DO I=1,5
                IFLO(I) = IFF3(I)
                IFHI(I) = IFF3(I)
             ENDDO
C            CALL CENTRE(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,XLIM,ICEN,
C    1                   IFORM,IFLO,IFHI)
C
C
C            LABEL THE ISO BARS ABOVE THE CENTERS
C
C            THIS IS A MSL PRESSURE FIELD
             FORMV = IFOR02
             NCV =2
             ICORV = -10
             JCORV = -10
             JUP=7
C            print *,' call glosec'
             FGCENV = .TRUE.
             DO I=1,5
                IFFV(I) = IFF2(I)
             ENDDO
C            CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,
C    1                   IFORM,IFF2)
C
C         PUT STIP LABELS ON THE VERTICAL DISPLACEMENTS LINES
C
          FORMC = 'A999'
          NCC  = 2
          IRTCRC = -10
          IUPCRC = -10
          DO I=1,5
             IFFC(I) = IFF2(I)
          ENDDO
          FGLABC = .TRUE.
          DO K3 = 1,NLAB
                IJFIXC(K3) = SLFIX(K3)
C               CALL CLABEL(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,IFORM,IFF4)
          ENDDO
C
      ELSE IF (KEYIDX.EQ.2 .OR. KEYIDX.EQ.32 .OR. KEYIDX.EQ.15) THEN
C
C             THIS IS A HEIGHT FIELD.
C
             IF(LDN)THEN
C               print *,' ldn section'
                NCT  = 4
                FORMT = 'I+-+'
                ICEN = 1
                XLIM = -500.0
C
C            FIND AND LABEL THE DN CENTERS
C
               FGCENT = .TRUE.
               DO I=1,5
                  IFLO(I) = IFF3(I)
                  IFHI(I) = IFF3(I)
               ENDDO
C              CALL CENTRE(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,XLIM,ICEN,
C    1                    IFORM,IFF3,IFF3)
             ELSE IF(KDN)THEN
C               print *,' kdn section'
                NCT  = 4
                FORMT = 'I+-9'
                ICEN = 1
                XLIM = -4000.0
                B1   = B1*10.0
C
C            FIND AND LABEL THE "K" DN CENTERS
C
                FGCENT = .TRUE.
                DO I=1,5
                   IFLO(I) = IFF3(I)
                   IFHI(I) = IFF3(I)
                ENDDO
C               CALL CENTRE(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,XLIM,ICEN,
C    1                     IFORM,IFF3,IFF3)
C
C
C            PUT STIP LABELS ON THE "K" DN LINES.
C
               FORMC = 'I+-9'
               IRTCRC = -15
               IUPCRC = -10
               NCC =4
               DO I=1,5
                   IFFC(I) = IFF4(I)
               ENDDO
               FGLABC = .TRUE.
               DO K3 = 1,NLAB
                IJFIXC(K3) = JJFIX(K3)
C               CALL CLABEL(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,IFORM,IFF4)
               ENDDO
            ELSE
C              print *,' elsesection'
               NCT  = 3
               FORMT = IFOR02
               ICEN = 2
               XLIM = -1000.0
C              print *, ' contst=', a1,b1,s
C
C             FIND AND LABEL THE CENTERS
C
                FGCENT = .TRUE.
                DO I=1,5
                   IFLO(I) = IFF3(I)
                   IFHI(I) = IFF3(I)
                ENDDO
C               CALL CENTRE(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,XLIM,ICEN,
C    1                     IFORM,IFF3,IFF3)
C
C
C             PUT STIP LABELS ON THE CONTOURS.
C
              NCC = 3
              FORMC = IFOR02
              IRTCRC = -15
              IUPCRC = -10
              FONTC = 15.0
              DO I=1,5
                 IFFC(I) = IFF3(I)
              ENDDO
              FGLABC = .TRUE.
              DO K3 = 1,NLAB
                   IJFIXC(K3) = MJFIX(K3)
C               CALL CLABEL(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,IFORM,IFF3)
              ENDDO
            ENDIF
      ELSE IF (KEYIDX.EQ.3 .OR. KEYIDX.EQ.33) THEN
C
C       THIS IS A THICKNESS FIELD.
C
          IRTCRC = -15
          IUPCRC = -10
          NCC  = 3
          FORMC = IFOR02
C
C         PUT STIP LABELS ON THE CONTOURS.
C
          FONTC  = 15.0
          DO I=1,5
             IFFC(I) = IFF3(I)
          ENDDO
          FGLABC = .TRUE.
          DO K3 = 1,NLAB
                IJFIXC(K3) = MMFIX(K3)
C               CALL CLABEL(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,IFORM,IFF3)
          ENDDO
C
      ELSE IF (KEYIDX. EQ. 34)THEN
C
C          THIS IS A HEIGHT CHANGE FIELD WITHOUT CENTERS
C
             JUP=2
             NCV  = 3
             ICORV = -15
             JCORV = -10
             FGCENV = .TRUE.
             DO I=1,5
                IFFV(I) = IFF3(I)
             ENDDO
             FORMV = 'A+-+'
C            CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,
C    1                   IFORM,IFFV)
C
C             PUT  HEIGHT CHANGE STRIP LABELS .
C
             NCC  = 3
             IRTCRC = -15
             IUPCRC =  -10
             FGLABC = .TRUE.
             DO I=1,5
                IFFC(I) = IFF3(I)
             ENDDO
             FORMC = 'A+-+'
             FGLABC = .TRUE.
             DO K3 = 1,NLAB
                IJFIXC(K3) = KJFIX(K3)
C               CALL CLABEL(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,
C    1                   IFORM,IFFC)
             ENDDO
C
      ELSE IF (KEYIDX .EQ. 47)THEN
C
C       THIS IS A TRAJECTORY VERTICAL DISPLACEMENT FIELD
C
          NCV  = 4
          ICORV = -20
          JCORV = -10
          FORMV = 'A+-+'
          JUP = 4
C
C         LABEL THE VERTICAL DISPLACEMENT LINES ABOVE THE CENTERS
C
          FGCENV = .TRUE.
          DO I=1,5
             IFFV(I) = IFF4(I)
          ENDDO
C         CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,IFORM,IFF4)
C
C         PUT STIP LABELS ON THE VERTICAL DISPLACEMENTS LINES
C
          FORMC = 'A+-+'
          NCC  = 4
          DO I=1,5
             IFFC(I) = IFF4(I)
          ENDDO
          FGLABC = .TRUE.
          DO K3 = 1,3
                IJFIXC(K3) = IIFIX(K3)
C               CALL CLABEL(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,IFORM,IFF4)
          ENDDO
      ELSE IF (KEYIDX .EQ. 48)THEN
C
C       THIS IS A TRAJECTORY VERTICAL K INDEX  FIELD
C
          NCV  = 3
          ICORV = -15
          JCORV = -10
          FORMV = 'A+-+'
          JUP = 4
C
C         LABEL THE K INDEX LINES ABOVE THE CENTERS
C
          FGCENV = .TRUE.
          DO I=1,5
             IFFV(I) = IFF3(I)
          ENDDO
C         CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,IFORM,IFF3)
C
C
C         PUT STIP LABELS ON THE K INDEX LINES
C
          FORMC = 'A+-+'
          DO I=1,5
             IFFC(I) = IFF3(I)
          ENDDO
          FGLABC = .TRUE.
          NCC  = 3
          DO K3 = 1,3
                IJFIXC(K3) = IIFIX(K3)
C               CALL CLABEL(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,IFORM,IFF3)
          ENDDO
C
      ELSE IF (KEYIDX.EQ.5 .OR. KEYIDX.EQ.35
     1       .OR. KEYIDX.EQ.46) THEN
C
C       THIS IS A TEMPERATURE FIELD.
C
          IF (GULPXX(2).EQ.-49 .AND. GULPXY(2).EQ.-49) THEN
C
C              ... FOR PFAX1 ONLY ....
C
C     OUTPUT DIAMOND CIRCLE TEMPERATURE PLOTS (PFAX ONLY)
C
               IRTCOR=-15
               IUPCOR=-6
               A3=0.0
               B3=1.0
               M(2)=3
               IFORM=IFOR01
               PUTHGT=1.0
               PUTANG = 0.0
               PRINT *,'  ***CALL TEMPLP ****'
               CALL TEMPLP(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,IFORM,IFF3)
               GULPXX(2) = -48
               GULPXY(2) = -48
               RETURN
          ELSE IF (GULPXX(2).EQ.-48 .AND. GULPXY(2).EQ.-48) THEN
C
C              ... FOR PFAX1 ONLY ....
C
C     OUTPUT DIAMOND BOX TEMPERATURE PLOTS (PFAX ONLY)
C
               IRTCOR=-15
               IUPCOR=-6
               A3=0.0
               B3=1.0
               M(2)=3
               IFORM=IFOR01
               PUTHGT=1.0
               PUTANG = 0.0
               PRINT *,'  ***CALL TEMPLN****'
               CALL TEMPLN(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,IFORM,IFF3)
               GULPXX(2) = -49
               GULPXY(2) = -49
               RETURN
         ELSE 
C
C
          NLAB = 5
          IRTCRC = -15
          IUPCRC = -10
          IF (GDTYPE .EQ. 26 .OR. GDTYPE .EQ. 05) THEN
C         .... LFM TYPE GRID ....
           IF (KEYIDX .EQ. 46 ) THEN
            NLAB = 3
               DO I=1, NLAB
                  TMPFIX(I) = IIFIX(I)
               ENDDO
           ELSE
               DO I=1, NLAB
                  TMPFIX(I) = MMFIX(I)
               ENDDO
           ENDIF
          ELSE IF (GDTYPE .EQ. 27 .OR. GDTYPE.EQ.28) THEN
C         .... NH NMC STANDARD GRID ....
               DO I=1, NLAB
                  TMPFIX(I) = LJFIX(I)
               ENDDO
          ENDIF
C
          NCC  = 3
          FORMC = IFOR01
C
C         PUT STIP LABELS ON THE CONTOURS.
C
          DO I=1,5
             IFFC(I) = IFF3(I)
          ENDDO
          FGLABC = .TRUE.
          DO K3 = 1,NLAB
                IJFIXC(K3) = TMPFIX(K3)
C               CALL CLABEL(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,IFORM,IFF3)
          ENDDO
C
          FORMV=IFOR01
          NCV =3
          JUP=3
          ICORV = -15
          JCORV = -10
C
C         LABEL THE CONTOURS ABOVE THE CENTERS
C
          FGCENV = .TRUE.
          DO I=1,5
             IFFV(I) = IFF3(I)
          ENDDO
C         CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,IFORM,IFF3)
        ENDIF
C
      ELSE IF (KEYIDX.EQ.6 .OR. KEYIDX.EQ.25 .OR.
     1          KEYIDX.EQ.27 .OR. KEYIDX.EQ.36 .OR.
     2          KEYIDX.EQ.45 )THEN
C
C          THIS IS A RELATIVE HUMIDITY FIELD.
C
C         PRINT *, ' ****** RELATIVE HUMIDITY**********'
          NCT  = 3
          FONTC  = 2.0
          IF (KEYIDX .EQ. 6 )THEN
C
C         DO NOT LABEL THE CENTERS
C
          ELSE
C
C         FIND AND LABEL THE CENTERS
C
            NCT  = 3
            FORMC=IFOREL
            XIDID = -5.0
            YJDID = -5.0
            FGCENT = .TRUE.
            DO I=1,5
                IFLO(I) = IFF3(I)
                IFHI(I) = IFF3(I)
            ENDDO
C           CALL CENTRE(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,XLIM,ICEN,
C    1                   IFORM,IFF3,IFF3)
          ENDIF
C
          IFORM=IFOR02
          NCT  = 2
          JUP = 4
          IF (KEYIDX .EQ. 27)THEN
C
C         THIS IS TH BOUNDARY RH SO SET JUP TO 2 GRID INTERVALS.
C
            JUP=2
          ENDIF
C
C         LABEL THE CONTOURS ABOVE THE CENTERS
C
            IF (KEYIDX .EQ. 25) THEN
C
C              PUT 1 DIGET NUMBERS ON THE LINES
C
               FORMV=IFOREL
               FONTV  =  2.0
               IFORM=IFOR02
               NCV  = 1
               ICORV = -5
               JCORV = -10
               B10TH = B1/10.0
               FGCENV = .TRUE.
               DO I=1,5
                  IFFV(I) = IFF1(I)
               ENDDO
C              CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B10TH,M,JUP,
C    1                    IFORM,IFF1)
            ELSE IF (KEYIDX .EQ. 36)THEN
C
C              PUT 2 DIGET NUMBERS ON THE LINES
C
               FONTV  =  1.0
               NCV  = 2
               ICORV = -10
               JCORV = -10
               FORMV=IFOREL
               FGCENV = .TRUE.
               DO I=1,5
                  IFFV(I) = IFF2(I)
               ENDDO
C              CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,
C    1                    IFORM,IFF2)
            ELSE
               FONTV  =  1.0
               NCV  = 2
               FGCENV = .TRUE.
               FORMV=IFOREL
               ICORV = -10
               JCORV = -10
               DO I=1,5
                  IFFV(I) = IFF1(I)
               ENDDO
C              CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,
C    1                    IFORM,IFF1)
            ENDIF
C
      ELSE IF (KEYIDX.EQ.7 .OR. KEYIDX.EQ.37 .OR. KEYIDX.EQ.19
     1         .OR. KEYIDX.EQ.49) THEN
C
C             THIS IS A PRECIPITATION  FIELD.
C
C
C         FIND AND LABEL THE CENTERS
C
         IF (KEYIDX.EQ.7 .OR. KEYIDX.EQ.37) THEN
            FONTT  = 2.0
            NCT  = 9
            FORMT = 'A999'
            FGCENT = .TRUE.
            DO I=1,5
               IFLO(I) = IFF3(I)
               IFHI(I) = IFF3(I)
            ENDDO
         ELSE
C           ...SPECIAL FOR CGRID PRCIP CHARTS
            PUTHGT = 2.0
            M(2) = 9
            FORMT = 'A999'
            DO I=1,5
               IFLO(I) = IFF3(I)
               IFHI(I) = IFF3(I)
            ENDDO
            CALL GENTRE(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,XLIM,ICEN,
     1                    FORMT,IFHI,IFLO)
C
        ENDIF
      ELSE IF (KEYIDX.EQ.9 .OR. KEYIDX.EQ.13)THEN
C
C             THIS IS A VORTICITY FIELD.
C
C
C         FIND AND LABEL THE CENTERS
C
          NCT  = 1
          FORMT = IFOR02
          XIDID = -5.0
          YJDID = -5.0
          FGCENT = .TRUE.
          DO I=1,5
             IFLO(I) = IFFN(I)
             IFHI(I) = IFFX(I)
          ENDDO
C         CALL CENTRE(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,XLIM,ICEN,
C    1                   IFORM,IFFN,IFFX)
C
C             THIS IS A VORTICITY FIELD.
C
          JUP=4
          NCV  = 2
          ICORV = -10
          JCORV = -10
          FORMV = 'A999'
C
C            LABEL THE CONTOURS ABOVE THE CENTERS
C
          FGCENV = .TRUE.
          DO I=1,5
             IFFV(I) = IFF2(I)
          ENDDO
C         CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,
C    1                   IFORM,IFF2)
C
C
      ELSE IF(KEYIDX.EQ.20 .OR. KEYIDX.EQ.21 .OR.
     2        KEYIDX.EQ.28 .OR. KEYIDX.EQ.29) THEN
C
C             THIS IS A  LIFTED INDEX FIELD.
C
C         FIND AND LABEL THE CENTERS
C
          NCT  = 1
          FORMT = 'A+-+'
          FGCENT = .TRUE.
          DO I=1,5
             IFLO(I) = IFFN(I)
             IFHI(I) = IFFX(I)
          ENDDO
C         CALL CENTRE(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,XLIM,ICEN,
C    1                   IFORM,IFF3,IFF3)
C         IF (IRETUR .EQ. 5) RETURN
C
C             THIS IS A LIFTED INDEX FIELD, PUT LEADING + OR -
C             IN FRONT OF THE CONTOUR NUMBERS.
C
             JUP=2
             NCV  = 3
             ICORV = -15
             JCORV = -10
             FORMV = 'A+-+'
C
C            LABEL THE CONTOURS ABOVE THE CENTERS
C
             FGCENV = .TRUE.
             DO I=1,5
                IFFV(I) = IFF3(I)
             ENDDO
C            CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,
C    1                   IFORM,IFF3)
C
C         PUT STIP LABELS ON THE CONTOURS.
C
             DO I=1,5
                IFFC(I) = IFF3(I)
             ENDDO
             NCC  = 3
             IRTCRC = -15
             IUPCRC = -10
             FGLABC = .TRUE.
             FORMC = 'A+-+'
             DO K3 = 1,NLAB
                IJFIXC(K3) = MJFIX(K3)
C               CALL CLABEL(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,
C    1                   IFORM,IFF3)
             ENDDO
C
      ELSE IF (KEYIDX.EQ.10 .OR. KEYIDX.EQ.40) THEN
C
C         THIS A WIND/ISOTACHS FIELD
C
          NCV =3
          ICORV = -15
          JCORV = -10
          JUP=2
          FORMV=IFOR06
C
C         LABEL THE CONTOURS ABOVE THE CENTERS
C
C         PRINT *,' ISOTACHS CALL CLOSEC'
          FGCENV = .TRUE.
          DO I=1,5
             IFFV(I) = IFG2(I)
          ENDDO
C         CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,IFORM,IFG2)
C
      ELSE IF (KEYIDX.EQ.12 .OR. KEYIDX.EQ.42) THEN
C
C       THIS IS A VERTICAL VELOCITY FIELD
C
C         PRINT *,' ****VERTICAL VELOCITY*****'
          FONTT  = 11.0
          NCT  = 1
          XIDID = -5.0
          YJDID = -10.0
          FORMT=IFOR01
C
C         FIND AND LABEL THE CENTERS
C
          FGCENT = .TRUE.
          DO I=1,5
             IFLO(I) = IFF1(I)
             IFHI(I) = IFF1(I)
          ENDDO
C         CALL CENTRE(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,XLIM,ICEN,
C    1                   IFORM,IFF1,IFF1)
C         IF (IRETUR .EQ. 5) RETURN
C
C
C         LABEL THE CONTOURS ABOVE THE CENTERS
C
          FONTV  = 1.0
          JUP=2
          NCV  = 2
          ICORV = -10
          JCORV = -10
          FORMV=IFOR01
          FGCENV = .TRUE.
          DO I=1,5
             IFFV(I) = IFF2(I)
          ENDDO
C         CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,IFORM,IFF2)
C
      ELSE IF (KEYIDX.EQ.39)THEN
C
C             THIS IS A GEO REL VORTICITY FIELD.
C
C
C         FIND AND LABEL THE CENTERS
C
          NCT  = 1
          FORMT = IFOR02
          XIDID = -5.0
          YJDID = -5.0
          FGCENT = .TRUE.
          DO I=1,5
             IFLO(I) = IFFN(I)
             IFHI(I) = IFFX(I)
          ENDDO
C
          JUP=4
          NCV  = 3
          ICORV = -15
          JCORV = -15
          FORMV = 'A+-+'
C
C            LABEL THE CONTOURS ABOVE THE CENTERS
C
          FGCENV = .TRUE.
          DO I=1,5
             IFFV(I) = IFF3(I)
          ENDDO
C
C
      ELSE IF (KEYIDX.EQ.43) THEN
C 
C        ... TROPOPAUSE PRESSURE
C
C     FIND INDIRECT TROP. PRESSURE STRIP LABELS
      PRINT *,' CALL LABELP '
C
      IRTCOR=-12
      IUPCOR=-5
      PUTHGT=2.0
      LOX=1
      IFORM=IFOR02
      M=3
      DO K3=2,12
         IJFIX=KCFIX(K3)
         IFMAT = 5
         CALL CLABEP(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,LOX,
     X   ITABMB,ITABFL,MXITR,IFORM,IFF5)
      ENDDO
C
C
C     FIND INDIRECT TROP PRESSURE FROM CENTERS
C
C     ICOR=-10
C     JCOR=-5
C     JUP=2
C     CALL CLOSEX(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,LOX,
C    XITABMB,ITABFL,MXITR,IFORM,IFF5)
      RETURN
C
C
      ENDIF
C
C         ... COME TO HERE TO PUT CENTER LABEL OUT
C
      IF (CENTFLAG) THEN
          PRINT *,' **** CENTER FLAG IS ON.*****'
          PUTHGT = CENTFONT
          M(2) = CENTNC
          ICEN = CENTCEN
          IFORM = CENTFORM
          XLIM = CENTXLIM
          XIDID = CENTXID
          YJDID = CENTYID
          B1 = B * CENTB1RC
C         PRINT *,' CENTFLO=',CENTFLO
C         PRINT *,' CENTFHI=',CENTFHI
          IF (CENTFLO .EQ. 'IFF1') THEN
             DO I=1,5
                IFLO(I) = IFF1(I)
             ENDDO
          ELSE IF (CENTFLO .EQ. 'IFFN') THEN
C            PRINT *,' GET A CENTER LO'
             DO I=1,5
                IFLO(I) = IFFN(I)
             ENDDO
          ELSE IF (CENTFLO .EQ. 'IFF2') THEN
             DO I=1,5
                IFLO(I) = IFF2(I)
             ENDDO
          ELSE IF (CENTFLO .EQ. 'IFF3') THEN
             DO I=1,5
                IFLO(I) = IFF3(I)
             ENDDO
          ELSE IF (CENTFLO .EQ. 'IFF4') THEN
             DO I=1,5
                IFLO(I) = IFF4(I)
             ENDDO
          ELSE IF (CENTFLO .EQ. 'IFFT') THEN
             DO I=1,5
                IFLO(I) = IFFT(I)
             ENDDO
          ELSE IF (CENTFLO .EQ. 'IFFS') THEN
             DO I=1,5
                IFLO(I) = IFFS(I)
             ENDDO
          ENDIF
C
          IF (CENTFHI .EQ. 'IFF1') THEN
             DO I=1,5
                IFHI(I) = IFF1(I)
             ENDDO
          ELSE IF (CENTFHI .EQ. 'IFFX') THEN
C            PRINT *,' GET A CENTER HI'
             DO I=1,5
                IFHI(I) = IFFX(I)
             ENDDO
          ELSE IF (CENTFHI .EQ. 'IFF2') THEN
             DO I=1,5
                IFHI(I) = IFF2(I)
             ENDDO
          ELSE IF (CENTFHI .EQ. 'IFF3') THEN
             DO I=1,5
                IFHI(I) = IFF3(I)
             ENDDO
          ELSE IF (CENTFHI .EQ. 'IFF4') THEN
             DO I=1,5
                IFHI(I) = IFF4(I)
             ENDDO
          ELSE IF (CENTFHI .EQ. 'IFFT') THEN
             DO I=1,5
                IFHI(I) = IFFT(I)
             ENDDO
          ELSE IF (CENTFHI .EQ. 'IFFS') THEN
             DO I=1,5
                IFHI(I) = IFFS(I)
             ENDDO
          ENDIF
          CALL CENTRE(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,XLIM,ICEN,
     1                   IFORM,IFLO,IFHI)
      ELSE IF (FGCENT) THEN
          PUTHGT = FONTT
          IFORM = FORMT
          M(2) = NCT
          CALL CENTRE(FLD1,IMAX,JMAX,DOTSGI,A1,B1,M,XLIM,ICEN,
     1                   IFORM,IFLO,IFHI)
      ENDIF
C
C         ... COME TO HERE TO PUT CENTER ABOVE LABEL OUT
C
      IF (CENVFLAG) THEN
          PRINT *,' **** CENTER ABOVE FLAG IS ON.*****'
          PUTHGT = CENVFONT
          M(2) = CENVNC
          IFORM = CENVFORM
          ICOR   = CENVICR
          JCOR   = CENVJCR
          B1 = B * CENVB1RC
          JUP = CENVJUP
          IFORM = CENVFORM
C
          IF (CENVIFF .EQ. 'IFF1') THEN
             DO I=1,5
                IFFV(I) = IFF1(I)
             ENDDO
          ELSE IF (CENVIFF .EQ. 'IFF2') THEN
             DO I=1,5
                IFFV(I) = IFF2(I)
             ENDDO
          ELSE IF (CENVIFF .EQ. 'IFF3') THEN
             DO I=1,5
                IFFV(I) = IFF3(I)
             ENDDO
          ELSE IF (CENVIFF .EQ. 'IFF4') THEN
             DO I=1,5
                IFFV(I) = IFF4(I)
             ENDDO
          ENDIF
C
          CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,IFORM,IFFV)
      ELSE IF (FGCENV) THEN
          PUTHGT = FONTV
          IFORM = FORMV
          M(2) = NCV
          ICOR   = ICORV  
          JCOR   = JCORV  
          CALL CLOSEC(FLD1,IMAX,JMAX,S,A1,B1,M,JUP,IFORM,IFFV)
      ENDIF
C
C         ... COME TO HERE TO PUT FIX CONTOUR LABEL OUT
C
      IF (LABFLAG) THEN
          PRINT *,' **** LABEL  FLAG IS ON.*****'
          PUTHGT = LABFONT
          M(2) = LABNC
          IFORM = LABFORM
          IRTCOR =  LABRTC
          IUPCOR =  LABUPC
          B1 = B *  LABB1RC
          IFORM =  LABFORM
C
          IF ( LABIFF .EQ. 'IFF1') THEN
             DO I=1,5
                IFFC(I) = IFF1(I)
             ENDDO
          ELSE IF ( LABIFF .EQ. 'IFF2') THEN
             DO I=1,5
                IFFC(I) = IFF2(I)
             ENDDO
          ELSE IF ( LABIFF .EQ. 'IFF3') THEN
             DO I=1,5
                IFFC(I) = IFF3(I)
             ENDDO
          ELSE IF ( LABIFF .EQ. 'IFF4') THEN
             DO I=1,5
                IFFC(I) = IFF4(I)
             ENDDO
          ENDIF
C
          print *,' labnlab=',labnlab
          DO K3 = 1,LABNLAB
             print *,' k3=',k3,'  =',labijfix(k3)
                IJFIX = LABIJFIX(K3)
                CALL CLABEL(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,
     1                     IFORM,IFFC)
          ENDDO
      ELSE IF (FGLABC) THEN
C         PRINT *, ' ****LABEL DEFAULT CONSTANTS****'
          PUTHGT = FONTC
          IFORM = FORMC
          M(2) = NCC
          IRTCOR = IRTCRC
          IUPCOR = IUPCRC
C         PRINT *,' COR=',IRTCOR,' ',IUPCOR
          DO K3 = 1,NLAB
                IJFIX = IJFIXC(K3)
                CALL CLABEL(FLD1,IMAX,JMAX,S,A1,B1,M,IJFIX,
     1                     IFORM,IFFC)
          ENDDO
      ENDIF
C
      IF (KEYIDX.EQ.19 .OR. KEYIDX.EQ.49) THEN
C        ... SPECIAL FOR CGRID PRECIP MAP
         CALL THINLB
      ENDIF
C
      RETURN
      END
