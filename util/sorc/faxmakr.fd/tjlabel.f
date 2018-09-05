      SUBROUTINE TJLABEL(FLDIN,IDIM,JDIM,DOTSGI,A1,B1,KEYIDX)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    TJLABEL     PLACE A SPECIAL LABEL AROUND THE BOUNDARY
C   PRGMMR: KRISHNA KUMAR       ORG: W/NP12    DATE: 1999-08-01
C
C ABSTRACT: PLACE A SPECIAL LABEL AROUND THE BOUNDARY FOR TRAJECTORY 4-PANEL
C           CHART.
C
C PROGRAM HISTORY LOG:
C   97-06-19  LUKE LIN    
C 1999-08-01  KRISHNA KUMAR CONVERTED THIS CODE FROM CRAY TO IBM RS/6000.
C
C USAGE:    CALL TJLABEL(FLDIN,IDIM,JDIM,DOTSGI,A1,B1)
C   INPUT ARGUMENTS:
C     FLDIN    - REAL*4  BIGFLD(IBIG,JBIG)
C     IDIM   - I-DIMENSION OF THE ORIGINAL  SMLFLD.
C     JDIM   - J-DIMENSION OF THE ORIGINAL  SMLFLD.
C
C   OUTPUT ARGUMENT LIST:
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: F90
C   MACHINE:  IBM
C
C$$$
C
C     *     *     *     *     *     *     *     *     *     *     *
C
      COMMON/PUTARG/PUTHGT,PUTANG,IPRIOR(2),ICMPUT
      COMMON /ADJ1/   ICOR,JCOR
      COMMON /ADJ3/ILCOR,JDCOR
C
      INTEGER    KEYIDX
      REAL       FLDIN(IDIM,JDIM)
      REAL       DOTSGI,A1,B1
      REAL       FLDDUP(53,57)
      REAL       XINDEF
      INTEGER IJV(4,150)
      INTEGER ICHSIZ(2)
      INTEGER INBTWN(2)
      INTEGER ITBRES(4,200)
      DATA KDIMRE/200/
C
      INTEGER M(2)
C
      INTEGER LIJV
      INTEGER KSVUNI
      CHARACTER*4 IFORM, IFOR06, IFOR05, IFOR04
      CHARACTER*8 IFG(5)
      CHARACTER*8 IFG2(5)
      CHARACTER*8 IFG3(5)
C
      DATA IFG2   /'(A2,1H$)','        ','        ','        ',
     .             '        '/
      DATA IFG3   /'(A3,1H$)','        ','        ','        ',
     .             '        '/
      DATA IFOR06 /'A999'/
      DATA IFOR05 /'A+- '/
      DATA IFOR04 /'S9-9'/
C
      DATA LIJV  /150/
      DATA KSVUNI/45/
C
C     REWIND KSVUNI
C
      PRINT *,' IN TJLABEL'
C
      DO 2760 J = 1,JDIM 
      DO 2760 I = 1,IDIM 
        FLDDUP(I,J) = FLDIN(I,J)
 2760 CONTINUE
C
C -----------------------------------------------
C     ESTABLISH FLAGS FOR STRIP LABELS
C -----------------------------------------------
C
C
C     DO KJ=1,57
C     PRINT *,' J=', KJ
C        WRITE(*,150)(FLDDUP(KM,KJ),KM=1,53)
C     ENDDO
C150  format( 3(10(1x,f5.1),3x),/, 2(10( 1x,F5.1),3x),3(1x,F5.1),/)
C
C
      DO 1500 IY = 1,150
      DO 1500 IX = 1,4
        IJV(IX,IY) = 0
 1500 CONTINUE
      CALL VBOUND(FLDDUP,IDIM,JDIM,IJV,LIJV)
C
C -----------------------------------------------
C     SETUP CONTOUR STRIP LABELS
C -----------------------------------------------
C
C
C ... STRIP LABELS 2ND FIELD
C
        DO 1560 III = 1,LIJV
          DO 1570 I = 1,4
            IF(IJV(I,III).NE.0) GO TO 1552
 1570     CONTINUE
          GO TO 1581
 1552     CONTINUE
          JUP = IJV(4,III) - 1         ! VBOUND GIVES ONE MORE THAN LENGTH
c         IF(JUP) 1560,1560,1554
          IF(JUP.LE.0) GO TO 1560

          PRINT *,' LIJV=',III,' I=',I,' JUP=',JUP
 1554     CONTINUE
          IXX       = IJV(2,III)
          JYY       = IJV(3,III)
          PRINT *,' IXX=', IXX, ' JYY=',JYY
          PUTHGT    =  1.
          IPRIOR(1) =  0
          IPRIOR(2) =  0
          ICMPUT    =  0
          ICOR = 15
          JCOR = 0
          ILCOR = 15
          JDCOR = 0
          M(1)      =  0
          M(2)      =  3
          IFORM     = IFOR06
C
          IF (KEYIDX.EQ.47) THEN
              IFORM = IFOR04
              PUTHGT = 15.0
          ELSE IF (KEYIDX.EQ.51) THEN
              IFORM = IFOR04
          ENDIF
C
          IF (KEYIDX.EQ.48) THEN
              M(2)=2
              ICOR = 10
              ILCOR = 10
              DO I=1,5
                 IFG(I) = IFG2(I)
              ENDDO
          ELSE
              DO I=1,5
                 IFG(I) = IFG3(I)
              ENDDO
          ENDIF
C
          S = DOTSGI / 60.0
          IF(IJV(1,III).NE.1) THEN                    ! VERTICAL LABELS 
c  this call was using fld previously
            PRINT *,' CALL CLOSET'
            CALL CLOSET(FLDIN,IDIM,JDIM,S,A1,B1,M,JUP,IXX,JYY,
     .                  IFORM,1,IFG)
          ELSE                                      ! HORIZONTAL LABELS
c  this call was using fld previously
            PRINT *,' CALL CLOSEE'
            CALL CLOSEE(FLDIN,IDIM,JDIM,S,A1,B1,M,JUP,IXX,JYY,
     .                  IFORM,1,IFG)
          ENDIF
 1560   CONTINUE
 1581   CONTINUE
C
        JUP = 3
        CALL CLOSEC(FLDIN,IDIM,JDIM,S,A1,B1,M,JUP,IFORM,IFG)
C
      RETURN
      END
