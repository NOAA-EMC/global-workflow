      SUBROUTINE CALUPDHEL(UPDHEL)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALUPDHEL      COMPUTES UPDRAFT HELICITY
!   PRGRMMR: PYLE            ORG: W/NP2      DATE: 07-10-22       
!     
! ABSTRACT:  
!     THIS ROUTINE COMPUTES THE UPDRAFT HELICITY
!   .     
!     
! PROGRAM HISTORY LOG:
!   07-10-22  M PYLE - based on SPC Algorithm courtesy of David Bright
!   11-01-11  M Pyle - converted to F90 for unified post
!   11-04-05  H Chuang - added B grid option
!     
! USAGE:    CALL CALUPDHEL(UPDHEL)
!
!   INPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT ARGUMENT LIST: 
!     UPDHEL   - UPDRAFT HELICITY (M^2/S^2)
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - CTLBLK
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
!
!      use vrbls2d, only:
      use vrbls3d,      only: wh, uh, vh, zint, zmid
      use masks,        only: lmh, dx, dy
      use params_mod,   only: d00
      use ctlblk_mod,   only: lm, jsta_2l, jend_2u, jsta_m, jend_m,   &
                              global, spval, im, jm
      use gridspec_mod, only: gridtype

      implicit none

!     DECLARE VARIABLES.
!     
!      LOGICAL RUN,FIRST,RESTRT,SIGMA,OLDRD,STRD
      REAL, PARAMETER:: HLOWER=2000., HUPPER=5000.
      REAL ZMIDLOC
      real :: r2dx, r2dy, dz, dcdx, dudy, dvdx
      REAL :: HTSFC(IM,jsta_2l:jend_2u),UPDHEL(IM,jsta_2l:jend_2u)
      integer :: l, j, i 
      INTEGER, dimension(jm) :: IHE,IHW
!        INTEGER DXVAL,DYVAL,CENLAT,CENLON,TRUELAT1,TRUELAT2
!        INTEGER LATSTART,LONSTART,LATLAST,LONLAST 
!     
!***************************************************************************
!     START CALUPDHEL HERE.
!     
      write(6,*) 'min/max WH(:,:,20):: ', minval(WH(:,:,20)), &
                                          maxval(WH(:,:,20))

      DO L=1,LM
        CALL EXCH(UH(1,jsta_2l,L))
      END DO 
      IF (GRIDTYPE == 'B')THEN
        DO L=1,LM
          CALL EXCH(VH(1,jsta_2l,L))
        END DO
      END IF
!$omp parallel do private(i,j)
      DO J=JSTA_2L,JEND_2U
        DO I=1,IM
          UPDHEL(I,J) = D00
        ENDDO
      ENDDO

      DO J=JSTA_2L,JEND_2U
        IHW(J) = -MOD(J,2)
        IHE(J) = IHW(J)+1
      ENDDO

!     Integrate (w * relative vorticity * dz) over the 2 km to
!     5 km AGL depth.

!	initial try without horizontal averaging

!$omp parallel do private(i,j)
      DO J=JSTA_M,JEND_M
        DO I=1,IM
          HTSFC(I,J) = ZINT(I,J,NINT(LMH(I,J))+1)
        ENDDO
      ENDDO

      DO J=JSTA_M,JEND_M
        DO I=2,IM-1

          R2DX   = 1./(2.*DX(I,J))
          R2DY   = 1./(2.*DY(I,J))

          l_loop: DO L=1,LM
            ZMIDLOC = ZMID(I,J,L)
            IF (global) then ! will put in global algorithm later
              UPDHEL(I,J) = spval
              EXIT l_loop
            END IF 

            IF ( (ZMIDLOC - HTSFC(I,J)) .ge. HLOWER  .AND.  &
                 (ZMIDLOC - HTSFC(I,J)) .le. HUPPER ) THEN
              DZ=(ZINT(I,J,L)-ZINT(I,J,L+1))

              IF (WH(I,J,L) .lt. 0) THEN

!          ANY DOWNWARD MOTION IN 2-5 km LAYER KILLS COMPUTATION AND
!          SETS RESULTANT UPDRAFT HELICTY TO ZERO

                UPDHEL(I,J) = 0.
                EXIT l_loop

              ENDIF

              IF(GRIDTYPE == 'A')THEN
                DVDX   = (VH(I+1,J,L)-VH(I-1,J,L))*R2DX
                DUDY   = (UH(I,J+1,L)-UH(I,J-1,L))*R2DY
              ELSE IF (GRIDTYPE == 'E')THEN
                DVDX   = (VH(I+IHE(J),J,L)-VH(I+IHW(J),J,L))*R2DX
                DUDY   = (UH(I,J+1,L)-UH(I,J-1,L))*R2DY
              ELSE IF (GRIDTYPE == 'B')THEN
!! seems like these are 1/dx, 1/dy
                DVDX = (0.5*(VH(I,J,L)+VH(I,J-1,L))-0.5*(VH(I-1,J,L) &
                            +VH(I-1,J-1,L)))*2.*R2DX
                DUDY = (0.5*(UH(I,J,L)+UH(I-1,J,L))-0.5*(UH(I,J-1,L) &
                            +UH(I-1,J-1,L)))*2.*R2DY
              ENDIF

              UPDHEL(I,J)=UPDHEL(I,J)+(DVDX-DUDY)*WH(I,J,L)*DZ

            ENDIF
          ENDDO l_loop
        ENDDO
      ENDDO

!
      print*,'jsta_m, jend_m in calupdhel= ',jsta_m,jend_m
!     
!     END OF ROUTINE.
!     
      RETURN
      END
