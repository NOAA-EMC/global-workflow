      SUBROUTINE H12(MODE,LPIVOT,L1,M,U,IUE,UP,C,ICE,ICV,NCV)
C***BEGIN PROLOGUE  H12
C***REFER TO  HFTI,LSEI,WNNLS
C
C     SUBROUTINE H12 (MODE,LPIVOT,L1,M,U,IUE,UP,C,ICE,ICV,NCV)
C
C     C.L.Lawson and R.J.Hanson, Jet Propulsion Laboratory, 1973 Jun 12
C     to appear in 'Solving Least Squares Problems', Prentice-Hall, 1974
C
C     Modified at SANDIA LABS, May 1977, to --
C
C     1)  Remove double precision accumulation, and
C     2)  Include usage of the Basic Linear Algebra Package for
C         vectors longer than a particular threshold.
C
C     Construction and/or application of a single
C     Householder transformation..     Q = I + U*(U**T)/B
C
C     MODE    = 1 or 2   to select algorithm  H1  or  H2 .
C     LPIVOT is the index of the pivot element.
C     L1,M   If L1 .LE. M   the transformation will be constructed to
C            zero elements indexed from L1 through M.   If L1 GT. M
C            THE SUBROUTINE DOES AN IDENTITY TRANSFORMATION.
C     U(),IUE,UP    On entry to H1 U() contains the pivot vector.
C                   IUE is the storage increment between elements.
C                                       On exit from H1 U() and UP
C                   contain quantities defining the vector U of the
C                   Householder transformation.   On entry to H2 U()
C                   and UP should contain quantities previously computed
C                   by H1.  These will not be modified by H2.
C     C()    On entry to H1 or H2 C() contains a matrix which will be
C            regarded as a set of vectors to which the Householder
C            transformation is to be applied.  On exit C() contains the
C            set of transformed vectors.
C     ICE    Storage increment between elements of vectors in C().
C     ICV    Storage increment between vectors in C().
C     NCV    Number of vectors in C() to be transformed. If NCV .LE. 0
C            no operations will be done on C().
C***ROUTINES CALLED  SAXPY,SDOT,SSWAP
C***END PROLOGUE  H12
c
      USE setparms
c
      DIMENSION U(IUE,M), C(1)
C***FIRST EXECUTABLE STATEMENT  H12
      ONE=1.
C
      IF (0.GE.LPIVOT.OR.LPIVOT.GE.L1.OR.L1.GT.M) RETURN
      CL=ABS(U(1,LPIVOT))
      IF (MODE.EQ.2) GO TO 60
C                            ****** CONSTRUCT THE TRANSFORMATION. ******
          DO 10 J=L1,M
   10     CL=AMAX1(ABS(U(1,J)),CL)
      IF (CL) 130,130,20
   20 CLINV=ONE/CL
      SM=(U(1,LPIVOT)*CLINV)**2
          DO 30 J=L1,M
   30     SM=SM+(U(1,J)*CLINV)**2
      CL=CL*SQRT(SM)
      IF (U(1,LPIVOT)) 50,50,40
   40 CL=-CL
   50 UP=U(1,LPIVOT)-CL
      U(1,LPIVOT)=CL
      GO TO 70
C            ****** APPLY THE TRANSFORMATION  I+U*(U**T)/B  TO C. ******
C
   60 IF (CL) 130,130,70
   70 IF (NCV.LE.0) RETURN
      B=UP*U(1,LPIVOT)
C                       B  MUST BE NONPOSITIVE HERE.  IF B = 0., RETURN.
C
      IF (B) 80,130,130
   80 B=ONE/B
      MML1P2=M-L1+2
      IF (MML1P2.GT.20) GO TO 140
      I2=1-ICV+ICE*(LPIVOT-1)
      INCR=ICE*(L1-LPIVOT)
          DO 120 J=1,NCV
          I2=I2+ICV
          I3=I2+INCR
          I4=I3
          SM=C(I2)*UP
              DO 90 I=L1,M
              SM=SM+C(I3)*U(1,I)
   90         I3=I3+ICE
          IF (SM) 100,120,100
  100     SM=SM*B
          C(I2)=C(I2)+SM*UP
              DO 110 I=L1,M
              C(I4)=C(I4)+SM*U(1,I)
  110         I4=I4+ICE
  120     CONTINUE
  130 RETURN
  140 CONTINUE
      L1M1=L1-1
      KL1=1+(L1M1-1)*ICE
      KL2=KL1
      KLP=1+(LPIVOT-1)*ICE
      UL1M1=U(1,L1M1)
      U(1,L1M1)=UP
      IF (LPIVOT.EQ.L1M1) GO TO 150
      if (kind(C) == real_single) then
        CALL SSWAP(NCV,C(KL1),ICV,C(KLP),ICV)
      else if (kind(C) == real_double) then
        CALL DSWAP(NCV,C(KL1),ICV,C(KLP),ICV)
      endif

  150 CONTINUE

      if (kind(U) == real_single .and. kind(C) == real_single) then
        do J=1,NCV
          SM=SDOT(MML1P2,U(1,L1M1),IUE,C(KL1),ICE)
          SM=SM*B
          CALL SAXPY (MML1P2,SM,U(1,L1M1),IUE,C(KL1),ICE)
          KL1=KL1+ICV
        enddo
      else if (kind(U) == real_double .and. kind(C) == real_double) then
        do J=1,NCV
          SM=DDOT(MML1P2,U(1,L1M1),IUE,C(KL1),ICE)
          SM=SM*B
          CALL DAXPY (MML1P2,SM,U(1,L1M1),IUE,C(KL1),ICE)
          KL1=KL1+ICV
        enddo
      endif

      U(1,L1M1)=UL1M1
      IF (LPIVOT.EQ.L1M1) RETURN
      KL1=KL2

      if (kind(C) == real_single) then
        CALL SSWAP(NCV,C(KL1),ICV,C(KLP),ICV)
      else if (kind(C) == real_double) then
        CALL DSWAP(NCV,C(KL1),ICV,C(KLP),ICV)
      endif

      RETURN
      END
