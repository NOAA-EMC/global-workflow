      SUBROUTINE CALTHTE(P1D,T1D,Q1D,THTE)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALTHTE      COMPUTES THETA-E
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-06-18
!     
! ABSTRACT:  
!     THIS ROUTINE COMPUTES THE EQUIVALENT POTENTIAL TEMPERATURE
!     GIVEN PRESSURE, TEMPERATURE, AND SPECIFIC HUMIDITY.  THE
!     EQUATIONS OF BOLTON (MWR,1980) ARE USED.
!   .     
!     
! PROGRAM HISTORY LOG:
!   93-06-18  RUSS TREADON
!   98-06-16  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION
!     
! USAGE:    CALL CALTHTE(P1D,T1D,Q1D,THTE)
!   INPUT ARGUMENT LIST:
!     P1D      - PRESSURE (PA)
!     T1D      - TEMPERATURE (K)
!     Q1D      - SPECIFIC HUMIDITY (KG/KG)
!
!   OUTPUT ARGUMENT LIST: 
!     THTE     - THETA-E (K)
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       VAPOR    - FUNCTION TO CALCULATE VAPOR PRESSURE.
!     LIBRARY:
!       NONE
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!
!     
      use params_mod, only: d00, eps, oneps, d01, h1m12, p1000, h1
      use ctlblk_mod, only: jsta, jend, im
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      real,PARAMETER :: KG2G=1.E3
      real,PARAMETER :: D35=3.5,D4805=4.805,H2840=2840.,H55=55.
      real,PARAMETER :: D2845=0.2845,D00028=0.00028,D3376=3.376
      real,PARAMETER :: D00254=0.00254,D00081=0.00081,D81=0.81
      real,PARAMETER :: D28=0.28,H2675=2675.
!
!     DECLARE VARIABLES.
!     
      REAL,dimension(IM,jsta:jend),intent(in)    :: P1D,T1D,Q1D
      REAL,dimension(IM,jsta:jend),intent(inout) :: THTE

      integer I,J
      real P,T,Q,EVP,RMX,CKAPA,RKAPA,ARG,DENOM,TLCL,PLCL,FAC,   &
           ETERM,THETAE
!     
!***************************************************************
!     START CALTHTE.
!     
!     ZERO THETA-E ARRAY
!$omp parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          THTE(I,J) = D00
        ENDDO
      ENDDO
!     
!     COMPUTE THETA-E.
!
!      DO J=JSTA_M,JEND_M
!      DO I=2,IM-1
!$omp parallel do private(i,j,p,t,q,evp,rmx,ckapa,rkapa,arg,denom,tlcl,plcl,fac,eterm,thetae)
      DO J=JSTA,JEND
        DO I=1,IM
          P        = P1D(I,J)
          T        = T1D(I,J)
          Q        = Q1D(I,J)
          EVP      = P*Q/(EPS+ONEPS*Q)
          RMX      = EPS*EVP/(P-EVP)
          CKAPA    = D2845*(1.-D28*RMX)
          RKAPA    = 1./CKAPA
          ARG      = max(H1M12, EVP*D01)
          DENOM    = D35*LOG(T) - LOG(EVP*D01) - D4805
          TLCL     = H2840/DENOM + H55
          PLCL     = P*(TLCL/T)**RKAPA
          FAC      = (P1000/P)**CKAPA
          ETERM    = (D3376/TLCL-D00254)*(RMX*KG2G*(H1+D81*RMX))
          THETAE   = T*FAC*EXP(ETERM)
          THTE(I,J)= THETAE
        ENDDO
      ENDDO
!     
!     END OF ROUTINE.
!
      RETURN
      END
