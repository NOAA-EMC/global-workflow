      SUBROUTINE CALSTRM(Z1D,STRM)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALSTRM     COMPUTES GEO STREAMFUNCTION
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22
!     
! ABSTRACT:  
!     THIS ROUTINE COMPUTES THE GEOSTROPHIC STREAMFUNCTION,
!     PSI, FROM THE PASSED GEOPOTENTIAL HEIGHT FIELD, Z.  
!     THE FORMULA USED IS PSI = G*Z/F0, WHERE G IS THE
!     GRAVITATIONAL ACCELERATION CONSTANT AND F0 IS A 
!     CONSTANT CORIOLIS PARAMETER.  F0 IS SET TO BE THE
!     VALUE OF THE CORIOLIS PARAMETER NEAR THE CENTER
!     OF THE MODEL GRID.
!   .     
!     
! PROGRAM HISTORY LOG:
!   92-12-22  RUSS TREADON
!   98-06-08  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-05  JIM TUCCILLO - MPI VERSION
!   02-06-13  MIKE BALDWIN - WRF VERSION
!     
! USAGE:    CALL CALSTRM(Z1D,STRM)
!   INPUT ARGUMENT LIST:
!     Z1D      - GEOPOTENTIAL HEIGHT (M)
!
!   OUTPUT ARGUMENT LIST: 
!     STRM     - GEOSTROPHIC STREAMFUNCTION
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - MAPOT
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
!
!     
!     
!     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
!     
!      use vrbls2d, only:
      use params_mod, only: g
      use ctlblk_mod, only: jsta, jend, im
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      real,PARAMETER :: OMEGA=7.292E-5,TWOMG=2*OMEGA
!
!     DECLARE VARIABLES.
!     
!      LOGICAL FIRST,OLDRD,RESTRT,RUN,SIGMA,STRD
      REAL, dimension(im,jsta:jend), intent(in)    ::  Z1D
      REAL, dimension(im,jsta:jend), intent(inout) ::  STRM
!
      LOGICAL OLDRD,STRD
      integer IMID,I,J
      real f0,gof0
!     
!***************************************************************************
!     START CALSTRM HERE.
!     
!     COMPUTE CORIOLIS PARAMETER AT 40N
!
      IMID=IM/2
      F0   = 1.454441e-4*sin(40.0*0.01745329)
      GOF0 = G/F0
!     
!     COMPUTE GEOSTROPHIC STREAMFUNCTION.
!$omp  parallel do
      DO J=JSTA,JEND
        DO I=1,IM
          STRM(I,J) = GOF0*Z1D(I,J)
        ENDDO
      ENDDO
!     
!     END OF ROUTINE.
      RETURN
      END
