      SUBROUTINE WNLSM(W,MDW,MME,MA,N,L,PRGOPT,X,RNORM,MODE,IPIVOT,
     1   ITYPE,WD,H,SCALE,Z,TEMP,D)
C***BEGIN PROLOGUE  WNLSM
C***REFER TO  WNNLS
C
C     This is a companion subprogram to WNNLS( ).
C     The documentation for WNNLS( ) has more complete
C     usage instructions.
C
C     Written by Karen H. Haskell, Sandia Laboratories,
C     with the help of R.J. Hanson, Sandia Laboratories,
C     December 1976 - January 1978.
C     Revised March 4, 1982.
C
C     In addition to the parameters discussed in the prologue to
C     subroutine WNNLS, the following work arrays are used in
C     subroutine WNLSM  (they are passed through the calling
C     sequence from WNNLS for purposes of variable dimensioning).
C     Their contents will in general be of no interest to the user.
C
C         IPIVOT(*)
C            An array of length N.  Upon completion it contains the
C         pivoting information for the cols of W(*,*).
C
C         ITYPE(*)
C            An array of length M which is used to keep track
C         of the classification of the equations.  ITYPE(I)=0
C         denotes equation I as an equality constraint.
C         ITYPE(I)=1 denotes equation I as a least squares
C         equation.
C
C         WD(*)
C            An array of length N.  Upon completion it contains the
C         dual solution vector.
C
C         H(*)
C            An array of length N.  Upon completion it contains the
C         pivot scalars of the Householder transformations performed
C         in the case KRANK.LT.L.
C
C         SCALE(*)
C            An array of length M which is used by the subroutine
C         to store the diagonal matrix of weights.
C         These are used to apply the modified Givens
C         transformations.
C
C         Z(*),TEMP(*)
C            Working arrays of length N.
C
C         D(*)
C            An array of length N that contains the
C         column scaling for the matrix (E).
C                                       (A)
C***ROUTINES CALLED  H12,ISAMAX,SASUM,SAXPY,SCOPY,SNRM2,SROTM,SROTMG,
C                    SSCAL,SSWAP,WNLIT,XERROR
C***END PROLOGUE  WNLSM
C
C     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO
C     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES.
C     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/.
C     (BEGIN CHANGES AT LINE WITH C++ IN COLS. 1-3.)
C     /REAL (12 BLANKS)/DOUBLE PRECISION/,/SASUM/DASUM/,/SROTMG/DROTMG/,
C     /SNRM2/DNRM2/,/ SQRT/ DSQRT/,/SROTM/DROTM/,/AMAX1/DMAX1/,
C     /SCOPY/DCOPY/,/SSCAL/DSCAL/,/SAXPY/DAXPY/,/E0/D0/,/SSWAP/DSWAP/,
C     /ISAMAX/IDAMAX/,/SRELPR/DRELPR/
C
C     SUBROUTINE WNLSM (W,MDW,MME,MA,N,L,PRGOPT,X,RNORM,MODE,
C    1                  IPIVOT,ITYPE,WD,H,SCALE,Z,TEMP,D)
C++
      USE setparms
c
      REAL             W(MDW,1), X(1), WD(1), H(1), SCALE(1), DOPE(4)
      REAL             Z(1), TEMP(1), PRGOPT(1), D(1), SPARAM(5)
      REAL             ALAMDA, ALPHA, ALSQ, AMAX, BNORM, EANORM
      REAL             SRELPR, FAC, ONE, BLOWUP
      REAL             RNORM, SM, T, TAU, TWO, WMAX, ZERO, ZZ, Z2
      REAL             AMAX1, SQRT, SNRM2, SASUM, DNRM2, DASUM

      INTEGER IPIVOT(1), ITYPE(1), IDOPE(8)
      integer(kind = int_single) ISAMAX,IDAMAX
      LOGICAL HITCON, FEASBL, DONE, POS
      DATA ZERO /0.E0/, ONE /1.E0/, TWO /2.E0/, SRELPR /0.E0/
C
C     INITIALIZE-VARIABLES
C***FIRST EXECUTABLE STATEMENT  WNLSM
      ASSIGN 10 TO IGO998
      GO TO 180
C
C     PERFORM INITIAL TRIANGULARIZATION IN THE SUBMATRIX
C     CORRESPONDING TO THE UNCONSTRAINED VARIABLES USING
C     THE PROCEDURE INITIALLY-TRIANGULARIZE.
   10 ASSIGN 20 TO IGO995
      GO TO 280
C
C     PERFORM WNNLS ALGORITHM USING THE FOLLOWING STEPS.
C
C     UNTIL(DONE)
C
C        COMPUTE-SEARCH-DIRECTION-AND-FEASIBLE-POINT
C
C        WHEN (HITCON) ADD-CONSTRAINTS
C
C        ELSE PERFORM-MULTIPLIER-TEST-AND-DROP-A-CONSTRAINT
C
C        FIN
C
C     COMPUTE-FINAL-SOLUTION
C
   20 IF (DONE) GO TO 80
C
      ASSIGN 30 TO IGO991
      GO TO 300
C
C     COMPUTE-SEARCH-DIRECTION-AND-FEASIBLE-POINT
C
   30 IF (.NOT.(HITCON)) GO TO 50
      ASSIGN 40 TO IGO986
      GO TO 370
   40 GO TO 70
C
C     WHEN (HITCON) ADD-CONSTRAINTS
C
   50 ASSIGN 60 TO IGO983
      GO TO 640
   60 CONTINUE
C
C     ELSE PERFORM-MULTIPLIER-TEST-AND-DROP-A-CONSTRAINT
C
   70 GO TO 20
C
   80 ASSIGN 90 TO IGO980
      GO TO 1000
C
C     COMPUTE-FINAL-SOLUTION
C
   90 RETURN
  100 CONTINUE
C
C     TO PROCESS-OPTION-VECTOR
      FAC = 1.E-4
C
C     THE NOMINAL TOLERANCE USED IN THE CODE,
      TAU = SQRT(SRELPR)
C
C     THE NOMINAL BLOW-UP FACTOR USED IN THE CODE.
      BLOWUP = TAU
C
C     THE NOMINAL COLUMN SCALING USED IN THE CODE IS
C     THE IDENTITY SCALING.
      D(1) = ONE
      if (kind(D) == real_single) then
        CALL SCOPY(N, D, 0, D, 1)
      else if (kind(D) == real_double) then
        CALL DCOPY(N, D, 0, D, 1)
      endif
C
C     DEFINE BOUND FOR NUMBER OF OPTIONS TO CHANGE.
      NOPT = 1000
C
C     DEFINE BOUND FOR POSITIVE VALUE OF LINK.
      NLINK = 100000
      NTIMES = 0
      LAST = 1
      LINK = PRGOPT(1)
      IF (.NOT.(LINK.LE.0 .OR. LINK.GT.NLINK)) GO TO 110
      NERR = 3
      IOPT = 1
      CALL XERROR( 'WNNLS( ) THE OPTION VECTOR IS UNDEFINED', 39, NERR,
     1 IOPT)
      MODE = 2
      RETURN
  110 IF (.NOT.(LINK.GT.1)) GO TO 160
      NTIMES = NTIMES + 1
      IF (.NOT.(NTIMES.GT.NOPT)) GO TO 120
      NERR = 3
      IOPT = 1
      CALL XERROR( 'WNNLS( ). THE LINKS IN THE OPTION VECTOR ARE CYCLING
     1.', 53,     NERR, IOPT)
      MODE = 2
      RETURN
  120 KEY = PRGOPT(LAST+1)
      IF (.NOT.(KEY.EQ.6 .AND. PRGOPT(LAST+2).NE.ZERO)) GO TO 140

      if (kind(W) == real_single) then
        do J=1,N
          T = SNRM2(M,W(1,J),1)
          IF (T.NE.ZERO) T = ONE/T
          D(J) = T
        enddo
      else if (kind(W) == real_double) then
        do J=1,N
          T = DNRM2(M,W(1,J),1)
          IF (T.NE.ZERO) T = ONE/T
          D(J) = T
        enddo
      endif

  140 IF (KEY.EQ.7) then
        if (kind(PRGOPT) == real_single) then
          CALL SCOPY(N, PRGOPT(LAST+2), 1, D, 1)
        else if (kind(PRGOPT) == real_double) then
          CALL DCOPY(N, PRGOPT(LAST+2), 1, D, 1)
        endif
      endif
      IF (KEY.EQ.8) TAU = AMAX1(SRELPR,PRGOPT(LAST+2))
      IF (KEY.EQ.9) BLOWUP = AMAX1(SRELPR,PRGOPT(LAST+2))
      NEXT = PRGOPT(LINK)
      IF (.NOT.(NEXT.LE.0 .OR. NEXT.GT.NLINK)) GO TO 150
      NERR = 3
      IOPT = 1
      CALL XERROR( 'WNNLS( ) THE OPTION VECTOR IS UNDEFINED', 39, NERR,
     1 IOPT)
      MODE = 2
      RETURN
  150 LAST = LINK
      LINK = NEXT
      GO TO 110

  160 if (kind(W) == real_single) then
        do J=1,N
          CALL SSCAL(M, D(J), W(1,J), 1)
        enddo
      else if (kind(W) == real_double) then
        do J=1,N
          CALL DSCAL(M, D(J), W(1,J), 1)
        enddo
      endif

      GO TO 1260
  180 CONTINUE
C
C     TO INITIALIZE-VARIABLES
C
C     SRELPR IS THE PRECISION FOR THE PARTICULAR MACHINE
C     BEING USED.  THIS LOGIC AVOIDS RECOMPUTING IT EVERY ENTRY.
      IF (.NOT.(SRELPR.EQ.ZERO)) GO TO 210
c*** changed back by BROSS
c*** changed by RF Boisvert, 19-Feb-92  (fails on HP 9000 Series 300)
cross      srelpr = r1mach(4)
       SRELPR = ONE
  190 IF (ONE+SRELPR.EQ.ONE) GO TO 200
       SRELPR = SRELPR/TWO
       GO TO 190
  200 SRELPR = SRELPR*TWO
cross
  210 M = MA + MME
      ME = MME
      MEP1 = ME + 1
      ASSIGN 220 TO IGO977
      GO TO 100
C
C     PROCESS-OPTION-VECTOR
  220 DONE = .FALSE.
      ITER = 0
      ITMAX = 3*(N-L)
      MODE = 0
      LP1 = L + 1
      NSOLN = L
      NSP1 = NSOLN + 1
      NP1 = N + 1
      NM1 = N - 1
      L1 = MIN0(M,L)
C
C     COMPUTE SCALE FACTOR TO APPLY TO EQUAL. CONSTRAINT EQUAS.

      if (kind(W) == real_single) then
        do J=1,N
          WD(J) = SASUM(M,W(1,J),1)
        enddo
        IMAX   = ISAMAX(N,WD,1)
        EANORM = WD(IMAX)
        BNORM  = SASUM(M,W(1,NP1),1)
      else if (kind(W) == real_double) then
        do J=1,N
          WD(J) = DASUM(M,W(1,J),1)
        enddo
        IMAX   = IDAMAX(N,WD,1)
        EANORM = WD(IMAX)
        BNORM  = DASUM(M,W(1,NP1),1)
      endif
    
      ALAMDA = EANORM/(SRELPR*FAC)
C
C     DEFINE SCALING DIAG MATRIX FOR MOD GIVENS USAGE AND
C     CLASSIFY EQUATION TYPES.
      ALSQ = ALAMDA**2
      DO 260 I=1,M
C
C     WHEN EQU I IS HEAVILY WEIGHTED ITYPE(I)=0, ELSE ITYPE(I)=1.
        IF (.NOT.(I.LE.ME)) GO TO 240
        T = ALSQ
        ITEMP = 0
        GO TO 250
  240   T = ONE
        ITEMP = 1
  250   SCALE(I) = T
        ITYPE(I) = ITEMP
  260 CONTINUE
C
C     SET THE SOLN VECTOR X(*) TO ZERO AND THE COL INTERCHANGE
C     MATRIX TO THE IDENTITY.
      X(1) = ZERO
      if (kind(X) == real_single) then
        CALL SCOPY(N, X, 0, X, 1)
      else if (kind(X) == real_double) then
        CALL DCOPY(N, X, 0, X, 1)
      endif
      DO 270 I=1,N
        IPIVOT(I) = I
  270 CONTINUE
      GO TO 1230
  280 CONTINUE
C
C     TO INITIALLY-TRIANGULARIZE
C
C     SET FIRST L COMPS. OF DUAL VECTOR TO ZERO BECAUSE
C     THESE CORRESPOND TO THE UNCONSTRAINED VARIABLES.
      IF (.NOT.(L.GT.0)) GO TO 290
      WD(1) = ZERO
      if (kind(WD) == real_single) then
        CALL SCOPY(L, WD, 0, WD, 1)
      else if (kind(WD) == real_double) then
        CALL DCOPY(L, WD, 0, WD, 1)
      endif
C
C     THE ARRAYS IDOPE(*) AND DOPE(*) ARE USED TO PASS
C     INFORMATION TO WNLIT().  THIS WAS DONE TO AVOID
C     A LONG CALLING SEQUENCE OR THE USE OF COMMON.
  290 IDOPE(1) = ME
      IDOPE(2) = MEP1
      IDOPE(3) = 0
      IDOPE(4) = 1
      IDOPE(5) = NSOLN
      IDOPE(6) = 0
      IDOPE(7) = 1
      IDOPE(8) = L1
C
      DOPE(1) = ALSQ
      DOPE(2) = EANORM
      DOPE(3) = FAC
      DOPE(4) = TAU
      CALL WNLIT(W, MDW, M, N, L, IPIVOT, ITYPE, H, SCALE, RNORM,
     1 IDOPE, DOPE, DONE)
      ME = IDOPE(1)
      MEP1 = IDOPE(2)
      KRANK = IDOPE(3)
      KRP1 = IDOPE(4)
      NSOLN = IDOPE(5)
      NIV = IDOPE(6)
      NIV1 = IDOPE(7)
      L1 = IDOPE(8)
      GO TO 1240
  300 CONTINUE
C
C     TO COMPUTE-SEARCH-DIRECTION-AND-FEASIBLE-POINT
C
C     SOLVE THE TRIANGULAR SYSTEM OF CURRENTLY NON-ACTIVE
C     VARIABLES AND STORE THE SOLUTION IN Z(*).
C
C     SOLVE-SYSTEM
      ASSIGN 310 TO IGO958
      GO TO 1110
C
C     INCREMENT ITERATION COUNTER AND CHECK AGAINST MAX. NUMBER
C     OF ITERATIONS.
  310 ITER = ITER + 1
      IF (.NOT.(ITER.GT.ITMAX)) GO TO 320
      MODE = 1
      DONE = .TRUE.
C
C     CHECK TO SEE IF ANY CONSTRAINTS HAVE BECOME ACTIVE.
C     IF SO, CALCULATE AN INTERPOLATION FACTOR SO THAT ALL
C     ACTIVE CONSTRAINTS ARE REMOVED FROM THE BASIS.
  320 ALPHA = TWO
      HITCON = .FALSE.
      IF (.NOT.(L.LT.NSOLN)) GO TO 360
      DO 350 J=LP1,NSOLN
        ZZ = Z(J)
        IF (.NOT.(ZZ.LE.ZERO)) GO TO 340
        T = X(J)/(X(J)-ZZ)
        IF (.NOT.(T.LT.ALPHA)) GO TO 330
        ALPHA = T
        JCON = J
  330   HITCON = .TRUE.
  340   CONTINUE
  350 CONTINUE
  360 GO TO 1220
  370 CONTINUE
C
C     TO ADD-CONSTRAINTS
C
C     USE COMPUTED ALPHA TO INTERPOLATE BETWEEN LAST
C     FEASIBLE SOLUTION X(*) AND CURRENT UNCONSTRAINED
C     (AND INFEASIBLE) SOLUTION Z(*).
      IF (.NOT.(LP1.LE.NSOLN)) GO TO 390
      DO 380 J=LP1,NSOLN
        X(J) = X(J) + ALPHA*(Z(J)-X(J))
  380 CONTINUE
  390 FEASBL = .FALSE.
      GO TO 410
  400 IF (FEASBL) GO TO 610
C
C     REMOVE COL JCON AND SHIFT COLS JCON+1 THROUGH N TO THE
C     LEFT. SWAP COL JCON INTO THE N-TH POSITION.  THIS ACHIEVES
C     UPPER HESSENBERG FORM FOR THE NONACTIVE CONSTRAINTS AND
C     LEAVES AN UPPER HESSENBERG MATRIX TO RETRIANGULARIZE.
  410 DO 420 I=1,M
        T = W(I,JCON)
!        if (kind(W) == real_single) then
!          CALL SCOPY(N-JCON, W(I,JCON+1), MDW, W(I,JCON), MDW)
!        else if (kind(W) == real_double) then
!          CALL DCOPY(N-JCON, W(I,JCON+1), MDW, W(I,JCON), MDW)
!        endif
        do j=jcon,n-1
          w(i,j)=w(i,j+1)
        end do
        W(I,N) = T
  420 CONTINUE
C
C     UPDATE PERMUTED INDEX VECTOR TO REFLECT THIS SHIFT AND SWAP.
      ITEMP = IPIVOT(JCON)
      IF (.NOT.(JCON.LT.N)) GO TO 440
      DO 430 I=JCON,NM1
        IPIVOT(I) = IPIVOT(I+1)
  430 CONTINUE
  440 IPIVOT(N) = ITEMP
C
C     SIMILARLY REPERMUTE X(*) VECTOR.
!      if (kind(X) == real_single) then
!        CALL SCOPY(N-JCON, X(JCON+1), 1, X(JCON), 1)
!      else if (kind(X) == real_double) then
!        CALL DCOPY(N-JCON, X(JCON+1), 1, X(JCON), 1)
!      endif
      do j=jcon,n-1
         X(j)=X(J+1)
      end do

      X(N) = ZERO
      NSP1 = NSOLN
      NSOLN = NSOLN - 1
      NIV1 = NIV
      NIV = NIV - 1
C
C     RETRIANGULARIZE UPPER HESSENBERG MATRIX AFTER ADDING CONSTRAINTS.
      J = JCON
      I = KRANK + JCON - L
  450 IF (.NOT.(J.LE.NSOLN)) GO TO 570
      IF (.NOT.(ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.0)) GO TO 470
      ASSIGN 460 TO IGO938
      GO TO 620
C
C     (ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.0) ZERO-IP1-TO-I-IN-COL-J
  460 GO TO 560
  470 IF (.NOT.(ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.1)) GO TO 490
      ASSIGN 480 TO IGO938
      GO TO 620
C
C     (ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.1) ZERO-IP1-TO-I-IN-COL-J
  480 GO TO 560
  490 IF (.NOT.(ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.0)) GO TO 510
      if (kind(W) == real_single) then
        CALL SSWAP(NP1, W(I,1), MDW, W(I+1,1), MDW)
      else if (kind(W) == real_double) then
        CALL DSWAP(NP1, W(I,1), MDW, W(I+1,1), MDW)
      endif
      if (kind(SCALE) == real_single) then
        CALL SSWAP(1, SCALE(I), 1, SCALE(I+1), 1)
      else if (kind(SCALE) == real_double) then
        CALL DSWAP(1, SCALE(I), 1, SCALE(I+1), 1)
      endif
      ITEMP = ITYPE(I+1)
      ITYPE(I+1) = ITYPE(I)
      ITYPE(I) = ITEMP
C
C     SWAPPED ROW WAS FORMERLY A PIVOT ELT., SO IT WILL
C     BE LARGE ENOUGH TO PERFORM ELIM.
      ASSIGN 500 TO IGO938
      GO TO 620
C
C     ZERO-IP1-TO-I-IN-COL-J
  500 GO TO 560
  510 IF (.NOT.(ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.1)) GO TO 550
      T = SCALE(I)*W(I,J)**2/ALSQ
      IF (.NOT.(T.GT.TAU**2*EANORM**2)) GO TO 530
      ASSIGN 520 TO IGO938
      GO TO 620
  520 GO TO 540
  530 if (kind(W) == real_single) then
        CALL SSWAP(NP1, W(I,1), MDW, W(I+1,1), MDW)
      else if (kind(W) == real_double) then
        CALL DSWAP(NP1, W(I,1), MDW, W(I+1,1), MDW)
      endif
      if (kind(SCALE) == real_single) then
        CALL SSWAP(1, SCALE(I), 1, SCALE(I+1), 1)
      else if (kind(SCALE) == real_double) then
        CALL DSWAP(1, SCALE(I), 1, SCALE(I+1), 1)
      endif

      ITEMP = ITYPE(I+1)
      ITYPE(I+1) = ITYPE(I)
      ITYPE(I) = ITEMP
      W(I+1,J) = ZERO
  540 CONTINUE
  550 CONTINUE
  560 I = I + 1
      J = J + 1
      GO TO 450
C
C     SEE IF THE REMAINING COEFFS IN THE SOLN SET ARE FEASIBLE.  THEY
C     SHOULD BE BECAUSE OF THE WAY ALPHA WAS DETERMINED.  IF ANY ARE
C     INFEASIBLE IT IS DUE TO ROUNDOFF ERROR.  ANY THAT ARE NON-
C     POSITIVE WILL BE SET TO ZERO AND REMOVED FROM THE SOLN SET.
  570 IF (.NOT.(LP1.LE.NSOLN)) GO TO 590
      DO 580 JCON=LP1,NSOLN
        IF (X(JCON).LE.ZERO) GO TO 600
  580 CONTINUE
  590 FEASBL = .TRUE.
  600 CONTINUE
      GO TO 400
  610 GO TO 1200
  620 CONTINUE
C
C     TO ZERO-IP1-TO-I-IN-COL-J
      IF (.NOT.(W(I+1,J).NE.ZERO)) GO TO 630
      CALL SROTMG(SCALE(I), SCALE(I+1), W(I,J), W(I+1,J), SPARAM)
      W(I+1,J) = ZERO
      CALL SROTM(NP1-J, W(I,J+1), MDW, W(I+1,J+1), MDW, SPARAM)
  630 GO TO 1290
  640 CONTINUE
C
C     TO PERFORM-MULTIPLIER-TEST-AND-DROP-A-CONSTRAINT
      if (kind(Z) == real_single) then
        CALL SCOPY(NSOLN, Z, 1, X, 1)
      else if (kind(Z) == real_double) then
        CALL DCOPY(NSOLN, Z, 1, X, 1)
      endif

      IF (.NOT.(NSOLN.LT.N)) GO TO 650
      X(NSP1) = ZERO
      if (kind(X) == real_single) then
        CALL SCOPY(N-NSOLN, X(NSP1), 0, X(NSP1), 1)
      else if (kind(X) == real_double) then
        CALL DCOPY(N-NSOLN, X(NSP1), 0, X(NSP1), 1)
      endif

  650 I = NIV1
  660 IF (.NOT.(I.LE.ME)) GO TO 690
C
C     RECLASSIFY LEAST SQUARES EQATIONS AS EQUALITIES AS
C     NECESSARY.
      IF (.NOT.(ITYPE(I).EQ.0)) GO TO 670
      I = I + 1
      GO TO 680
  670 if (kind(W) == real_single) then
        CALL SSWAP(NP1, W(I,1), MDW, W(ME,1), MDW)
      else if (kind(W) == real_double) then
        CALL DSWAP(NP1, W(I,1), MDW, W(ME,1), MDW)
      endif
      if (kind(SCALE) == real_single) then
        CALL SSWAP(1, SCALE(I), 1, SCALE(ME), 1)
      else if (kind(SCALE) == real_double) then
        CALL DSWAP(1, SCALE(I), 1, SCALE(ME), 1)
      endif

      ITEMP = ITYPE(I)
      ITYPE(I) = ITYPE(ME)
      ITYPE(ME) = ITEMP
      MEP1 = ME
      ME = ME - 1
  680 GO TO 660
C
C     FORM INNER PRODUCT VECTOR WD(*) OF DUAL COEFFS.
  690 IF (.NOT.(NSP1.LE.N)) GO TO 730
      DO 720 J=NSP1,N
        SM = ZERO
        IF (.NOT.(NSOLN.LT.M)) GO TO 710
        DO 700 I=NSP1,M
          SM = SM + SCALE(I)*W(I,J)*W(I,NP1)
  700   CONTINUE
  710   WD(J) = SM
  720 CONTINUE
  730 GO TO 750
  740 IF (POS .OR. DONE) GO TO 970
C
C     FIND J SUCH THAT WD(J)=WMAX IS MAXIMUM.  THIS DETERMINES
C     THAT THE INCOMING COL J WILL REDUCE THE RESIDUAL VECTOR
C     AND BE POSITIVE.
  750 WMAX = ZERO
      IWMAX = NSP1
      IF (.NOT.(NSP1.LE.N)) GO TO 780
      DO 770 J=NSP1,N
        IF (.NOT.(WD(J).GT.WMAX)) GO TO 760
        WMAX = WD(J)
        IWMAX = J
  760   CONTINUE
  770 CONTINUE
  780 IF (.NOT.(WMAX.LE.ZERO)) GO TO 790
      DONE = .TRUE.
      GO TO 960
C
C     SET DUAL COEFF TO ZERO FOR INCOMING COL.
  790 WD(IWMAX) = ZERO
C
C     WMAX .GT. ZERO, SO OKAY TO MOVE COL IWMAX TO SOLN SET.
C     PERFORM TRANSFORMATION TO RETRIANGULARIZE, AND TEST
C     FOR NEAR LINEAR DEPENDENCE.
C     SWAP COL IWMAX INTO NSOLN-TH POSITION TO MAINTAIN UPPER
C     HESSENBERG FORM OF ADJACENT COLS, AND ADD NEW COL TO
C     TRIANGULAR DECOMPOSITION.
      NSOLN = NSP1
      NSP1 = NSOLN + 1
      NIV = NIV1
      NIV1 = NIV + 1
      IF (.NOT.(NSOLN.NE.IWMAX)) GO TO 800
      if (kind(W) == real_single) then
        CALL SSWAP(M, W(1,NSOLN), 1, W(1,IWMAX), 1)
      else if (kind(W) == real_double) then
        CALL DSWAP(M, W(1,NSOLN), 1, W(1,IWMAX), 1)
      endif
      WD(IWMAX) = WD(NSOLN)
      WD(NSOLN) = ZERO
      ITEMP = IPIVOT(NSOLN)
      IPIVOT(NSOLN) = IPIVOT(IWMAX)
      IPIVOT(IWMAX) = ITEMP
C
C     REDUCE COL NSOLN SO THAT THE MATRIX OF NONACTIVE
C     CONSTRAINTS VARIABLES IS TRIANGULAR.
  800 J = M
  810 IF (.NOT.(J.GT.NIV)) GO TO 870
      JM1 = J - 1
      JP = JM1
C
C     WHEN OPERATING NEAR THE ME LINE, TEST TO SEE IF THE PIVOT ELT.
C     IS NEAR ZERO.  IF SO, USE THE LARGEST ELT. ABOVE IT AS THE PIVOT.
C     THIS IS TO MAINTAIN THE SHARP INTERFACE BETWEEN WEIGHTED AND
C     NON-WEIGHTED ROWS IN ALL CASES.
      IF (.NOT.(J.EQ.MEP1)) GO TO 850
      IMAX = ME
      AMAX = SCALE(ME)*W(ME,NSOLN)**2
  820 IF (.NOT.(JP.GE.NIV)) GO TO 840
      T = SCALE(JP)*W(JP,NSOLN)**2
      IF (.NOT.(T.GT.AMAX)) GO TO 830
      IMAX = JP
      AMAX = T
  830 JP = JP - 1
      GO TO 820
  840 JP = IMAX
  850 IF (.NOT.(W(J,NSOLN).NE.ZERO)) GO TO 860
      CALL SROTMG(SCALE(JP), SCALE(J), W(JP,NSOLN), W(J,NSOLN), SPARAM)
      W(J,NSOLN) = ZERO
      CALL SROTM(NP1-NSOLN, W(JP,NSP1), MDW, W(J,NSP1), MDW, SPARAM)
  860 J = JM1
      GO TO 810
C
C     SOLVE FOR Z(NSOLN)=PROPOSED NEW VALUE FOR X(NSOLN).
C     TEST IF THIS IS NONPOSITIVE OR TOO LARGE.
C     IF THIS WAS TRUE OR IF THE PIVOT TERM WAS ZERO REJECT
C     THE COL AS DEPENDENT.
  870 IF (.NOT.(W(NIV,NSOLN).NE.ZERO)) GO TO 890
      ISOL = NIV
      ASSIGN 880 TO IGO897
      GO TO 980
C
C     TEST-PROPOSED-NEW-COMPONENT
  880 GO TO 940
  890 IF (.NOT.(NIV.LE.ME .AND. W(MEP1,NSOLN).NE.ZERO)) GO TO 920
C
C     TRY TO ADD ROW MEP1 AS AN ADDITIONAL EQUALITY CONSTRAINT.
C     CHECK SIZE OF PROPOSED NEW SOLN COMPONENT.
C     REJECT IT IF IT IS TOO LARGE.
      ISOL = MEP1
      ASSIGN 900 TO IGO897
      GO TO 980
C
C     TEST-PROPOSED-NEW-COMPONENT
  900 IF (.NOT.(POS)) GO TO 910
C
C     SWAP ROWS MEP1 AND NIV, AND SCALE FACTORS FOR THESE ROWS.

      if (kind(W) == real_single) then
        CALL SSWAP(NP1, W(MEP1,1), MDW, W(NIV,1), MDW)
      else if (kind(W) == real_double) then
        CALL DSWAP(NP1, W(MEP1,1), MDW, W(NIV,1), MDW)
      endif
      if (kind(SCALE) == real_single) then
        CALL SSWAP(1, SCALE(MEP1), 1, SCALE(NIV), 1)
      else if (kind(SCALE) == real_double) then
        CALL DSWAP(1, SCALE(MEP1), 1, SCALE(NIV), 1)
      endif

      ITEMP = ITYPE(MEP1)
      ITYPE(MEP1) = ITYPE(NIV)
      ITYPE(NIV) = ITEMP
      ME = MEP1
      MEP1 = ME + 1
  910 GO TO 930
  920 POS = .FALSE.
  930 CONTINUE
  940 IF (POS) GO TO 950
      NSP1 = NSOLN
      NSOLN = NSOLN - 1
      NIV1 = NIV
      NIV = NIV - 1
  950 CONTINUE
  960 GO TO 740
  970 GO TO 1250
  980 CONTINUE
C
C     TO TEST-PROPOSED-NEW-COMPONENT
      Z2 = W(ISOL,NP1)/W(ISOL,NSOLN)
      Z(NSOLN) = Z2
      POS = Z2.GT.ZERO
      IF (.NOT.(Z2*EANORM.GE.BNORM .AND. POS)) GO TO 990
      POS = .NOT.(BLOWUP*Z2*EANORM.GE.BNORM)
  990 GO TO 1280
 1000 CONTINUE
C     TO COMPUTE-FINAL-SOLUTION
C
C     SOLVE SYSTEM, STORE RESULTS IN X(*).
C
      ASSIGN 1010 TO IGO958
      GO TO 1110
C     SOLVE-SYSTEM
 1010 if (kind(Z) == real_single) then
        CALL SCOPY(NSOLN, Z, 1, X, 1)
      else if (kind(Z) == real_double) then
        CALL DCOPY(NSOLN, Z, 1, X, 1)
      endif
C
C     APPLY HOUSEHOLDER TRANSFORMATIONS TO X(*) IF KRANK.LT.L
      IF (.NOT.(0.LT.KRANK .AND. KRANK.LT.L)) GO TO 1030
      DO 1020 I=1,KRANK
        CALL H12(2, I, KRP1, L, W(I,1), MDW, H(I), X, 1, 1, 1)
 1020 CONTINUE
C
C     FILL IN TRAILING ZEROES FOR CONSTRAINED VARIABLES NOT IN SOLN.
 1030 IF (.NOT.(NSOLN.LT.N)) GO TO 1040
      X(NSP1) = ZERO
      if (kind(X) == real_single) then
        CALL SCOPY(N-NSOLN, X(NSP1), 0, X(NSP1), 1)
      else if (kind(X) == real_double) then
        CALL DCOPY(N-NSOLN, X(NSP1), 0, X(NSP1), 1)
      endif
C
C     REPERMUTE SOLN VECTOR TO NATURAL ORDER.
 1040 DO 1070 I=1,N
        J = I
 1050   IF (IPIVOT(J).EQ.I) GO TO 1060
        J = J + 1
        GO TO 1050
 1060   IPIVOT(J) = IPIVOT(I)
        IPIVOT(I) = J
        if (kind(X) == real_single) then
          CALL SSWAP(1, X(J), 1, X(I), 1)
        else if (kind(X) == real_double) then
          CALL DSWAP(1, X(J), 1, X(I), 1)
        endif
 1070 CONTINUE
C
C     RESCALE THE SOLN USING THE COL SCALING.
      DO 1080 J=1,N
        X(J) = X(J)*D(J)
 1080 CONTINUE
      IF (.NOT.(NSOLN.LT.M)) GO TO 1100
      DO 1090 I=NSP1,M
        T = W(I,NP1)
        IF (I.LE.ME) T = T/ALAMDA
        T = (SCALE(I)*T)*T
        RNORM = RNORM + T
 1090 CONTINUE
 1100 RNORM = SQRT(RNORM)
      GO TO 1210
C
C     TO SOLVE-SYSTEM
C
 1110 CONTINUE
      IF (.NOT.(DONE)) GO TO 1120
      ISOL = 1
      GO TO 1130
 1120 ISOL = LP1
 1130 IF (.NOT.(NSOLN.GE.ISOL)) GO TO 1190
C
C     COPY RT. HAND SIDE INTO TEMP VECTOR TO USE OVERWRITING METHOD.
      if (kind(W) == real_single) then
        CALL SCOPY(NIV, W(1,NP1), 1, TEMP, 1)
      else if (kind(W) == real_double) then
        CALL DCOPY(NIV, W(1,NP1), 1, TEMP, 1)
      endif

      DO 1180 JJ=ISOL,NSOLN
        J = NSOLN - JJ + ISOL
        IF (.NOT.(J.GT.KRANK)) GO TO 1140
        I = NIV - JJ + ISOL
        GO TO 1150
 1140   I = J
 1150   IF (.NOT.(J.GT.KRANK .AND. J.LE.L)) GO TO 1160
        Z(J) = ZERO
        GO TO 1170
 1160   Z(J) = TEMP(I)/W(I,J)

        if (kind(W) == real_single .and. kind(TEMP) == real_single) 
     &  then
          CALL SAXPY(I-1, -Z(J), W(1,J), 1, TEMP, 1)
        else if (kind(W) == real_double .and. kind(TEMP) == real_double)
     &  then
          CALL DAXPY(I-1, -Z(J), W(1,J), 1, TEMP, 1)
        endif

 1170   CONTINUE
 1180 CONTINUE
 1190 GO TO 1270
 1200 GO TO IGO986, (40)
 1210 GO TO IGO980, (90)
 1220 GO TO IGO991, (30)
 1230 GO TO IGO998, (10)
 1240 GO TO IGO995, (20)
 1250 GO TO IGO983, (60)
 1260 GO TO IGO977, (220)
 1270 GO TO IGO958, (310, 1010)
 1280 GO TO IGO897, (880, 900)
 1290 GO TO IGO938, (460, 480, 500, 520)
      END
