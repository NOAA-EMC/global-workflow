      SUBROUTINE CALGUST(LPBL,ZPBL,GUST)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALGUST      COMPUTE MAX WIND LEVEL 
!   PRGRMMR: MANIKIN        ORG: W/NP2   DATE: 97-03-04       
!     
! ABSTRACT:  
!     THIS ROUTINE COMPUTES SURFACE WIND GUST BY MIXING
!  DOWN MOMENTUM FROM THE LEVEL AT THE HEIGHT OF THE PBL
!     
!     
! PROGRAM HISTORY LOG:
!   03-10-15 GEOFF MANIKIN
!   05-03-09 H CHUANG - WRF VERSION
!   05-07-07 BINBIN ZHOU - ADD RSM   
!   15-03-11 S Moorthi - set sfcwind to spval if u10 and v10 are spvals
!                        for A grid and set gust to just wind
!                        (in GSM with nemsio, it appears u10 & v10 have spval)
!   
! USAGE:    CALL CALGUST(GUST) 
!   INPUT ARGUMENT LIST:
!     NONE     
!
!   OUTPUT ARGUMENT LIST: 
!     GUST    - SPEED OF THE MAXIMUM SFC WIND GUST 
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       H2V     
!
!     LIBRARY:
!       COMMON   - 
!                  LOOPS
!                  OPTIONS
!                  MASKS
!                  INDX
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90
!     MACHINE : CRAY C-90
!$$$  
!     
!     
      use vrbls3d, only: uh, vh, zint, zmid
      use vrbls2d , only: u10h, v10h, u10,v10, fis
      use params_mod, only: d25, gi
      use ctlblk_mod, only: jsta, jend, spval, jsta_m, jend_m, num_procs, mpi_comm_comp, lm,&
              modelname, im, jm, jsta_2l, jend_2u
      use gridspec_mod, only: gridtype

      implicit none

      INCLUDE "mpif.h"
! 
!     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE PARAMETERS.
!
!     DECLARE VARIABLES.
!     
      INTEGER,intent(in) :: LPBL(IM,jsta_2l:jend_2u)
      REAL,intent(in)    :: ZPBL(IM,jsta_2l:jend_2u)
      REAL,intent(inout) :: GUST(IM,jsta_2l:jend_2u)

      integer I,J,IE,IW, L, K, ISTART, ISTOP, JSTART, JSTOP
      integer LMIN,LXXX,IERR
      real ZSFC,DELWIND,USFC,VSFC,SFCWIND,WIND,U0,V0,DZ
!     
!     
!*****************************************************************************
!     START CALMXW HERE.
!     
!     LOOP OVER THE GRID.
!    
!$omp parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          GUST(I,J) = SPVAL 
        ENDDO
      ENDDO
      
      IF(gridtype == 'A') THEN
        ISTART = 1
        ISTOP  = IM
        JSTART = JSTA
        JSTOP  = JEND
      ELSE
        ISTART = 2
        ISTOP  = IM-1
        JSTART = JSTA_M
        JSTOP  = JEND_M
        if ( num_procs > 1 ) then
         !CALL EXCH(U10(1,jsta_2l))
         !CALL EXCH(V10(1,jsta_2l))
         LMIN = max(1, minval(lpbl(1:im,jsta:jend)))
         CALL MPI_ALLREDUCE(LMIN,LXXX,1,MPI_INTEGER,MPI_MIN,MPI_COMM_COMP,IERR)
         DO L=LXXX,LM
          CALL EXCH(UH(1,jsta_2l,L))
          CALL EXCH(VH(1,jsta_2l,L))
         END DO 
        END IF 
      END IF
!
!     ASSUME THAT U AND V HAVE UPDATED HALOS
!
!!$omp  parallel do private(i,j,ie,iw,mxww,u0,v0,wind)
      DO J=JSTART,JSTOP
        DO I=ISTART,ISTOP
           L=LPBL(I,J) 
           IF(gridtype == 'E') THEN
             IE = I + MOD(J+1,2) 
             IW = I + MOD(J+1,2)-1

!        USFC=D25*(U10(I,J-1)+U10(IW,J)+U10(IE,J)+U10(I,J+1)) 
!        VSFC=D25*(V10(I,J-1)+V10(IW,J)+V10(IE,J)+V10(I,J+1))
             USFC = U10H(I,J)
             VSFC = V10H(I,J)
             SFCWIND = SQRT(USFC*USFC + VSFC*VSFC)
             U0   = D25*(UH(I,J-1,L)+UH(IW,J,L)+UH(IE,J,L)+UH(I,J+1,L))
             V0   = D25*(VH(I,J-1,L)+VH(IW,J,L)+VH(IE,J,L)+VH(I,J+1,L))
             WIND = SQRT(U0*U0 + V0*V0)
           ELSE IF(gridtype == 'B') THEN
             IE = I 
             IW = I-1

!        USFC=D25*(U10(I,J-1)+U10(IW,J)+U10(IE,J)+U10(IW,J-1)) 
!        VSFC=D25*(V10(I,J-1)+V10(IW,J)+V10(IE,J)+V10(IW,J-1))
             USFC = U10H(I,J)
             VSFC = V10H(I,J)
             SFCWIND = SQRT(USFC*USFC + VSFC*VSFC)
             U0 = D25*(UH(I,J-1,L)+UH(IW,J,L)+UH(IE,J,L)+UH(IW,J-1,L))
             V0 = D25*(VH(I,J-1,L)+VH(IW,J,L)+VH(IE,J,L)+VH(IW,J-1,L))
             WIND = SQRT(U0*U0 + V0*V0) 
           ELSE IF(gridtype == 'A') THEN
             USFC    = U10(I,J)
             VSFC    = V10(I,J)
             if (usfc < spval .and. vsfc < spval) then
               SFCWIND = SQRT(USFC*USFC + VSFC*VSFC) 
             else
               sfcwind = spval
             endif
             if(MODELNAME == 'RAPR') then
               ZSFC = ZINT(I,J,LM+1)
               L = LPBL(I,J)
! in RUC do 342 k=2,k1-1, where k1 - first level above PBLH
               GUST(I,J) = SFCWIND
               do K=LM-1,L-1,-1
                 U0   = UH(I,J,K)
                 V0   = VH(I,J,K)
                 WIND = SQRT(U0*U0 + V0*V0)
                 DELWIND = WIND - SFCWIND
                 DZ = ZMID(I,J,K)-ZSFC
                 DELWIND  = DELWIND*(1.0-MIN(0.5,DZ/2000.))
                 GUST(I,J) = MAX(GUST(I,J),SFCWIND+DELWIND)
               enddo
             else
               U0   = UH(I,J,L)
               V0   = VH(I,J,L)
               WIND = SQRT(U0*U0 + V0*V0 )
             endif ! endif RAPR

           ELSE
!            print*,'unknown grid type, not computing wind gust'
             return
           END IF

           if(MODELNAME /= 'RAPR')then
             if (sfcwind < spval) then
               DELWIND   = WIND - SFCWIND
               ZSFC      = FIS(I,J)*GI
               DELWIND   = DELWIND*(1.0-MIN(0.5,ZPBL(I,J)/2000.))
               GUST(I,J) = SFCWIND + DELWIND
             else
               GUST(I,J) = wind
             endif
           endif
        enddo
      enddo

!     END OF ROUTINE.
!     
      RETURN
      END
