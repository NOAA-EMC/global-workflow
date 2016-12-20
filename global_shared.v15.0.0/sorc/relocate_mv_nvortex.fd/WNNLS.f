      SUBROUTINE WNNLS(W,MDW,ME,MA,N,L,PRGOPT,X,RNORM,MODE,IWORK,WORK)
C***BEGIN PROLOGUE  WNNLS
C***DATE WRITTEN   790701   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  K1A2A
C***KEYWORDS  CONSTRAINED LEAST SQUARES,CURVE FITTING,DATA FITTING,
C             EQUALITY CONSTRAINTS,INEQUALITY CONSTRAINTS,
C             NONNEGATIVITY CONSTRAINTS,QUADRATIC PROGRAMMING
C***AUTHOR  HANSON, R. J., (SNLA)
C           HASKELL, K. H., (SNLA)
C***PURPOSE  Solve a linearly constrained least squares problem with
C            equality constraints and nonnegativity constraints on
C            selected variables.
C***DESCRIPTION
C
C     DIMENSION W(MDW,N+1),PRGOPT(*),X(N),IWORK(M+N),WORK(M+5*N)
C
C     Written by Karen H. Haskell, Sandia Laboratories,
C     and R.J. Hanson, Sandia Laboratories.
C
C     Abstract
C
C     This subprogram solves a linearly constrained least squares
C     problem.  Suppose there are given matrices E and A of
C     respective dimensions ME by N and MA by N, and vectors F
C     and B of respective lengths ME and MA.  This subroutine
C     solves the problem
C
C               EX = F, (equations to be exactly satisfied)
C
C               AX = B, (equations to be approximately satisfied,
C                        in the least squares sense)
C
C               subject to components L+1,...,N nonnegative
C
C     Any values ME.GE.0, MA.GE.0 and 0.LE. L .LE.N are permitted.
C
C     The problem is reposed as problem WNNLS
C
C               (WT*E)X = (WT*F)
C               (   A)    (   B), (least squares)
C               subject to components L+1,...,N nonnegative.
C
C     The subprogram chooses the heavy weight (or penalty parameter) WT.
C
C     The parameters for WNNLS are
C
C     INPUT..
C
C     W(*,*),MDW,  The array W(*,*) is double subscripted with first
C     ME,MA,N,L    dimensioning parameter equal to MDW.  For this
C                  discussion let us call M = ME + MA.  Then MDW
C                  must satisfy MDW.GE.M.  The condition MDW.LT.M
C                  is an error.
C
C                  The array W(*,*) contains the matrices and vectors
C
C                       (E  F)
C                       (A  B)
C
C                  in rows and columns 1,...,M and 1,...,N+1
C                  respectively.  Columns 1,...,L correspond to
C                  unconstrained variables X(1),...,X(L).  The
C                  remaining variables are constrained to be
C                  nonnegative. The condition L.LT.0 or L.GT.N is
C                  an error.
C
C     PRGOPT(*)    This real-valued array is the option vector.
C                  If the user is satisfied with the nominal
C                  subprogram features set
C
C                  PRGOPT(1)=1 (or PRGOPT(1)=1.0)
C
C                  Otherwise PRGOPT(*) is a linked list consisting of
C                  groups of data of the following form
C
C                  LINK
C                  KEY
C                  DATA SET
C
C                  The parameters LINK and KEY are each one word.
C                  The DATA SET can be comprised of several words.
C                  The number of items depends on the value of KEY.
C                  The value of LINK points to the first
C                  entry of the next group of data within
C                  PRGOPT(*).  The exception is when there are
C                  no more options to change.  In that
C                  case LINK=1 and the values KEY and DATA SET
C                  are not referenced. The general layout of
C                  PRGOPT(*) is as follows.
C
C               ...PRGOPT(1)=LINK1 (link to first entry of next group)
C               .  PRGOPT(2)=KEY1 (key to the option change)
C               .  PRGOPT(3)=DATA VALUE (data value for this change)
C               .       .
C               .       .
C               .       .
C               ...PRGOPT(LINK1)=LINK2 (link to the first entry of
C               .                       next group)
C               .  PRGOPT(LINK1+1)=KEY2 (key to the option change)
C               .  PRGOPT(LINK1+2)=DATA VALUE
C               ...     .
C               .       .
C               .       .
C               ...PRGOPT(LINK)=1 (no more options to change)
C
C                  Values of LINK that are nonpositive are errors.
C                  A value of LINK.GT.NLINK=100000 is also an error.
C                  This helps prevent using invalid but positive
C                  values of LINK that will probably extend
C                  beyond the program limits of PRGOPT(*).
C                  Unrecognized values of KEY are ignored.  The
C                  order of the options is arbitrary and any number
C                  of options can be changed with the following
C                  restriction.  To prevent cycling in the
C                  processing of the option array a count of the
C                  number of options changed is maintained.
C                  Whenever this count exceeds NOPT=1000 an error
C                  message is printed and the subprogram returns.
C
C                  OPTIONS..
C
C                  KEY=6
C                         Scale the nonzero columns of the
C                  entire data matrix
C                  (E)
C                  (A)
C                  to have length one. The DATA SET for
C                  this option is a single value.  It must
C                  be nonzero if unit length column scaling is
C                  desired.
C
C                  KEY=7
C                         Scale columns of the entire data matrix
C                  (E)
C                  (A)
C                  with a user-provided diagonal matrix.
C                  The DATA SET for this option consists
C                  of the N diagonal scaling factors, one for
C                  each matrix column.
C
C                  KEY=8
C                         Change the rank determination tolerance from
C                  the nominal value of SQRT(SRELPR).  This quantity
C                  can be no smaller than SRELPR, The arithmetic-
C                  storage precision.  The quantity used
C                  here is internally restricted to be at
C                  least SRELPR.  The DATA SET for this option
C                  is the new tolerance.
C
C                  KEY=9
C                         Change the blow-up parameter from the
C                  nominal value of SQRT(SRELPR).  The reciprocal of
C                  this parameter is used in rejecting solution
C                  components as too large when a variable is
C                  first brought into the active set.  Too large
C                  means that the proposed component times the
C                  reciprocal of the parameter is not less than
C                  the ratio of the norms of the right-side
C                  vector and the data matrix.
C                  This parameter can be no smaller than SRELPR,
C                  the arithmetic-storage precision.
C
C                  For example, suppose we want to provide
C                  a diagonal matrix to scale the problem
C                  matrix and change the tolerance used for
C                  determining linear dependence of dropped col
C                  vectors.  For these options the dimensions of
C                  PRGOPT(*) must be at least N+6.  The FORTRAN
C                  statements defining these options would
C                  be as follows.
C
C                  PRGOPT(1)=N+3 (link to entry N+3 in PRGOPT(*))
C                  PRGOPT(2)=7 (user-provided scaling key)
C
C                  CALL SCOPY(N,D,1,PRGOPT(3),1) (copy the N
C                  scaling factors from a user array called D(*)
C                  into PRGOPT(3)-PRGOPT(N+2))
C
C                  PRGOPT(N+3)=N+6 (link to entry N+6 of PRGOPT(*))
C                  PRGOPT(N+4)=8 (linear dependence tolerance key)
C                  PRGOPT(N+5)=... (new value of the tolerance)
C
C                  PRGOPT(N+6)=1 (no more options to change)
C
C
C     IWORK(1),    The amounts of working storage actually allocated
C     IWORK(2)     for the working arrays WORK(*) and IWORK(*),
C                  respectively.  These quantities are compared with
C                  the actual amounts of storage needed for WNNLS( ).
C                  Insufficient storage allocated for either WORK(*)
C                  or IWORK(*) is considered an error.  This feature
C                  was included in WNNLS( ) because miscalculating
C                  the storage formulas for WORK(*) and IWORK(*)
C                  might very well lead to subtle and hard-to-find
C                  execution errors.
C
C                  The length of WORK(*) must be at least
C
C                  LW = ME+MA+5*N
C                  This test will not be made if IWORK(1).LE.0.
C
C                  The length of IWORK(*) must be at least
C
C                  LIW = ME+MA+N
C                  This test will not be made if IWORK(2).LE.0.
C
C     OUTPUT..
C
C     X(*)         An array dimensioned at least N, which will
C                  contain the N components of the solution vector
C                  on output.
C
C     RNORM        The residual norm of the solution.  The value of
C                  RNORM contains the residual vector length of the
C                  equality constraints and least squares equations.
C
C     MODE         The value of MODE indicates the success or failure
C                  of the subprogram.
C
C                  MODE = 0  Subprogram completed successfully.
C
C                       = 1  Max. number of iterations (equal to
C                            3*(N-L)) exceeded. Nearly all problems
C                            should complete in fewer than this
C                            number of iterations. An approximate
C                            solution and its corresponding residual
C                            vector length are in X(*) and RNORM.
C
C                       = 2  Usage error occurred.  The offending
C                            condition is noted with the error
C                            processing subprogram, XERROR( ).
C
C     User-designated
C     Working arrays..
C
C     WORK(*)      A real-valued working array of length at least
C                  M + 5*N.
C
C     IWORK(*)     An integer-valued working array of length at least
C                  M+N.
C***REFERENCES  K.H. HASKELL AND R.J. HANSON, *AN ALGORITHM FOR
C                 LINEAR LEAST SQUARES PROBLEMS WITH EQUALITY AND
C                 NONNEGATIVITY CONSTRAINTS*, SAND77-0552, JUNE 1978.
C               K.H. HASKELL AND R.J. HANSON, *SELECTED ALGORITHMS FOR
C                 THE LINEARLY CONSTRAINED LEAST SQUARES PROBLEM--
C                 A USERS GUIDE*, SAND78-1290, AUGUST 1979.
C               K.H. HASKELL AND R.H. HANSON, *AN ALGORITHM FOR
C                 LINEAR LEAST SQUARES PROBLEMS WITH EQUALITY AND
C                 NONNEGATIVITY CONSTRAINTS*, MATH. PROG. 21 (1981),
C                 PP. 98-118.
C               R.J. HANSON AND K.H. HASKELL, *TWO ALGORITHMS FOR THE
C                 LINEARLY CONSTRAINED LEAST SQUARES PROBLEM*, ACM
C                 TRANS. ON MATH. SOFTWARE, SEPT. 1982.
C***ROUTINES CALLED  WNLSM,XERROR,XERRWV
C***END PROLOGUE  WNNLS
C
C     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO
C     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES.
C     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/.
C     (START AT LINE WITH C++ IN COLS. 1-3.)
C     /REAL (12 BLANKS)/DOUBLE PRECISION/,/, DUMMY/,SNGL(DUMMY)/
C
C     WRITTEN BY KAREN H. HASKELL, SANDIA LABORATORIES,
C     AND R.J. HANSON, SANDIA LABORATORIES.
C     REVISED FEB.25, 1982.
C
C     SUBROUTINES CALLED BY WNNLS( )
C
C++
C     WNLSM         COMPANION SUBROUTINE TO WNNLS( ), WHERE
C                   MOST OF THE COMPUTATION TAKES PLACE.
C
C     XERROR,XERRWV FROM SLATEC ERROR PROCESSING PACKAGE.
C                   THIS IS DOCUMENTED IN SANDIA TECH. REPT.,
C                   SAND78-1189.
C
C     REFERENCES
C
C     1. SOLVING LEAST SQUARES PROBLEMS, BY C.L. LAWSON
C        AND R.J. HANSON.  PRENTICE-HALL, INC. (1974).
C
C     2. BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE, BY
C        C.L. LAWSON, R.J. HANSON, D.R. KINCAID, AND F.T. KROGH.
C        TOMS, V. 5, NO. 3, P. 308.  ALSO AVAILABLE AS
C        SANDIA TECHNICAL REPORT NO. SAND77-0898.
C
C     3. AN ALGORITHM FOR LINEAR LEAST SQUARES WITH EQUALITY
C        AND NONNEGATIVITY CONSTRAINTS, BY K.H. HASKELL AND
C        R.J. HANSON.  AVAILABLE AS SANDIA TECHNICAL REPORT NO.
C        SAND77-0552, AND MATH. PROGRAMMING, VOL. 21, (1981), P. 98-118.
C
C     4. SLATEC COMMON MATH. LIBRARY ERROR HANDLING
C        PACKAGE.  BY R. E. JONES.  AVAILABLE AS SANDIA
C        TECHNICAL REPORT SAND78-1189.
C
      REAL              DUMMY, W(MDW,1), PRGOPT(1), X(1), WORK(1), RNORM
      INTEGER IWORK(*)
C
C
C***FIRST EXECUTABLE STATEMENT  WNNLS
      MODE = 0
       iwork(1)=mdw*6
       iwork(2)=mdw*2
      IF (MA+ME.LE.0 .OR. N.LE.0) RETURN
      IF (.NOT.(IWORK(1).GT.0)) GO TO 20
      LW = ME + MA + 5*N
      IF (.NOT.(IWORK(1).LT.LW)) GO TO 10
      NERR = 2
      IOPT = 1
      write(6,*) 'work array',iwork(1),lw
      CALL XERRWV( 'WNNLS( ), INSUFFICIENT STORAGE ALLOCATED FOR WORK(*)
     1, NEED LW=I1 BELOW', 70, NERR, IOPT, 1, LW, 0, 0, DUMMY, DUMMY)
      MODE = 2
      RETURN
   10 CONTINUE
   20 IF (.NOT.(IWORK(2).GT.0)) GO TO 40
      LIW = ME + MA + N
      IF (.NOT.(IWORK(2).LT.LIW)) GO TO 30
      NERR = 2
      IOPT = 1
      CALL XERRWV( 'WNNLS( ), INSUFFICIENT STORAGE ALLOCATED FOR IWORK(*
     1), NEED LIW=I1 BELOW', 72, NERR, IOPT, 1, LIW, 0, 0, DUMMY, DUMMY)
      MODE = 2
      RETURN
   30 CONTINUE
   40 IF (.NOT.(MDW.LT.ME+MA)) GO TO 50
      NERR = 1
      IOPT = 1
      CALL XERROR( 'WNNLS( ), THE VALUE MDW.LT.ME+MA IS AN ERROR', 44,
     1 NERR, IOPT)
      MODE = 2
      RETURN
   50 IF (0.LE.L .AND. L.LE.N) GO TO 60
      NERR = 2
      IOPT = 1
      CALL XERROR( 'WNNLS( ), L.LE.0.AND.L.LE.N IS REQUIRED', 39, NERR,
     1 IOPT)
      MODE = 2
      RETURN
C
C     THE PURPOSE OF THIS SUBROUTINE IS TO BREAK UP THE ARRAYS
C     WORK(*) AND IWORK(*) INTO SEPARATE WORK ARRAYS
C     REQUIRED BY THE MAIN SUBROUTINE WNLSM( ).
C
   60 L1 = N + 1
      L2 = L1 + N
      L3 = L2 + ME + MA
      L4 = L3 + N
      L5 = L4 + N
C
      CALL WNLSM(W, MDW, ME, MA, N, L, PRGOPT, X, RNORM, MODE, IWORK,
     1 IWORK(L1), WORK(1), WORK(L1), WORK(L2), WORK(L3), WORK(L4),
     2 WORK(L5))
      RETURN
      END
