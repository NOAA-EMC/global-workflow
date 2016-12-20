      SUBROUTINE DEWPOINT( VP, TD)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    DEWPOINT    COMPUTES DEWPOINTS FROM VAPOR PRESSURE
!   PRGMMR: J TUCCILLO       ORG: W/NP2       DATE: 90-05-19
!
! ABSTRACT: COMPUTES THE DEWPOINTS FOR THE N VALUES
!           OF VAPOR PRESSURE IN ARRAY VP.
!   .
!   .       THE FORMULA
!   .
!   .            VP = 0.611 * (X**A) * EXP( (A+B)*(1-X) )
!   .
!   IS USED TO GET DEWPOINT TEMPERATURE T, WHERE
!   .
!   X = T3/T,                   T3=TRIPLE PT TEMPERATURE,
!   VP=VAPOR PRESSURE IN CBS,   0.611=VP AT T3,
!   A=(SPEC. HT. OF WATER-CSUBP OF VAPOR)/GAS CONST OF VAPOR
!   .                        AND
!   B=LATENT HEAT AT T3/(GAS CONST OF VAPOR TIMES T3).
!   .
!   ON THE FIRST CALL, A TABLE  TDP  IS CONSTRUCTED GIVING
!   DEWPOINT AS A FUNCTION OF VAPOR PRESSURE.
!   .
!   VALUES OF VP LESS THAN THE FIRST TABLE ENTRY
!   (RVP1 IN THE CODE) WILL BE GIVEN DEWPOINTS FOR
!   THAT BEGINNING VALUE.  SIMILARLY , VP VALUES THAT
!   EXCEED THE MAXIMUM TABLE VALUE (RVP2 IN THE CODE)
!   WILL BE ASSIGNED DEWPOINTS FOR THAT MAXIMUM VALUE.
!   .
!   THE VALUES 0.02 AND 8.0 FOR RVP1 AND RVP2 YIELD
!   DEWPOINTS OF 233.6K AND 314.7K,RESPECTIVELY.
!   .
!
! PROGRAM HISTORY LOG:
!   90-05-19  J TUCCILLO
!   93-05-12  R TREADON - EXPANDED TABLE SIZE AND RESET
!                         RANGE OF PRESSURES COVERED BY
!                         TABLE.
!   98-06-12  T BLACK   - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION
!
! USAGE:  CALL DEWPOINT( VP, TD)
!   INPUT ARGUMENT LIST:
!     VP       - ARRAY OF N VAPOR PRESSURES(CENTIBARS)
!
!   OUTPUT ARGUMENT LIST:
!     TD       - DEWPOINT IN DEGREES ABSOLUTE
!
!   SUBPROGRAMS CALLED:
!     LIBRARY:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!   MACHINE:  CRAY C-90
!
!$$$
       use ctlblk_mod, only: jsta, jend, im
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!          NT IS THE TABLE SIZE
      integer,PARAMETER :: NT=2000
!...TRANSLATED BY FPP 3.00Z36 11/09/90  14:48:53  
!...SWITCHES: OPTON=I47,OPTOFF=VAE0
      real,intent(out) :: TD(IM,jsta:jend)
      real,intent(in) ::  VP(IM,jsta:jend)
      real TDP(NT)
!jw
      integer NN,I,J,JNT
      real rvp1,rvp2,rt3,rvp3,rlog3,ra,rb,rapb,rtest,rnt,rdvp
      real rgs,rvp,rlvp,rn,rd,rch,rt,w1,w2
      real A,B,DNTM1

!          PREPARE THE TABLE (TDP=DEWPT AS FCN OF VAPOR PRESS).
!          RANGE IN CENTIBARS IS FROM RVP1 THRU RVP2
      rvp1  = 0.0001E0
      rvp2  = 10.E0
!          THE TRIPLE POINT
      RT3   = 273.16E0
!          VAPOR PRESS AT THE TRIPLE POINT
      RVP3  = 0.611E0
      RLOG3 = LOG(RVP3)
!          (SPEC HT OF WATER -CSUBP OF VAPOR)/GAS CONST OF VAPOR.
      RA    = 5.0065E0
!          LATENT HEAT AT T3/(GAS CONST OF VAPOR * TRIPLE PT TEMP).
      RB    = 19.83923E0
      RAPB  = RA + RB
!          CRITERION FOR CONVERGENCE OF NEWTON ITERATION
      RTEST = 1.E-6
!MEB  RTEST=1.E-8  !  PROBABLY WON'T CONVERGE WITH 32-BIT AT THIS CRITERION
!
      RNT   = FLOAT(NT)
!          TABLE INCREMENT IN VAPOR PRESS
      RDVP  = (RVP2-RVP1)/(RNT-1.E0)
!          RGS WILL BE THE GUESSED VALUE OF (T3  /  DEWPOINT)
      RGS   = 1.E0
      RVP   = RVP1-RDVP
!
      DO 20 NN=1,NT
        RVP=RVP+RDVP
        RLVP=LOG(RVP)-RLOG3-RAPB
!     ***** ENTER NEWTON ITERATION LOOP
   10   RN=RA*LOG(RGS)-RAPB*RGS-RLVP
!          THAT WAS VALUE OF FUNCTION
!          NOW GET ITS DERIVATIVE
        RD=(RA/RGS)-RAPB
!          THE DESIRED CHANGE IN THE GUESS
        RCH=RN/RD
        IF( ABS(RCH) .LT. RTEST ) GO TO 15
!          NEED MORE ITERATIONS
        RGS=RGS-RCH
        GO TO 10
!          *****
!          HAVE ACCURATE ENUF VALUE OF RGS=T3/DEWPOINT.
   15   RT=RT3/RGS
        TDP(NN)=RT
!
   20 CONTINUE
!      PRINT 25,RVP1,RVP2,TDP(1),TDP(NT)
!  25  FORMAT(/'0', 'IN SUBROUTINE DEWPOINT, THE DEWPT TABLE ',
!    1             'HAS RVP1=', 1PE13.6, ', RVP2=', 1PE13.6,
!    2             ', TDP(1)=', 1PE13.6, ', AND TDP(NT)=',
!    3             1PE13.6, '.'/)
!           CONSTANTS FOR USING THE TABLE
      A     = 1./RDVP
      B     = 1. - A*RVP1
      DNTM1 = FLOAT(NT) -.01
!
!X      END IF
!
!          *********** ENTER TO USE THE TABLE.  ************
!
!$omp parallel do private(i,j,w1,w2,jnt)
      DO J=JSTA,JEND
        DO I=1,IM
          W1  = MIN(MAX((A*VP(I,J)+B),1.0),DNTM1)
          W2  = AINT(W1)
          JNT = INT(W2)
          TD(I,J) = TDP(JNT) + (W1-W2)*(TDP(JNT+1)-TDP(JNT))
        ENDDO
      ENDDO
!
!
      RETURN
      END
