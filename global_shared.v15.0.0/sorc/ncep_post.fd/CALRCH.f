      SUBROUTINE CALRCH(EL,RICHNO)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALRCH      COMPUTES GRD RCH NUMBER
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-10-11
!     
! ABSTRACT:  
!   THIS ROUTINE COMPUTES THE GRADIENT RICHARDSON NUMBER
!   AS CODED IN ETA MODEL SUBROUTINE PROFQ2.F.
!   FIX TO AVOID UNREASONABLY SMALL ANEMOMETER LEVEL WINDS.
!   .     
!     
! PROGRAM HISTORY LOG:
!   93-10-11  RUSS TREADON
!   98-06-17  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION
!   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-01-15  MIKE BALDWIN - WRF VERSION
!   05-02-25  H CHUANG - ADD COMPUTATION FOR NMM E GRID
!   05-07-07  BINBIN ZHOU - ADD RSM FOR A GRID  
!   
! USAGE:    CALL CALRCH(EL,RICHNO)
!   INPUT ARGUMENT LIST:
!     EL      - MIXING LENGTH SCALE.
!
!   OUTPUT ARGUMENT LIST: 
!     RICHNO  - GRADIENT RICHARDSON NUMBER.
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - 
!                  CTLBLK
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!
      use vrbls3d,    only: pmid, q, t, uh, vh, zmid, q2
      use masks,      only: vtm
      use params_mod, only: h10e5, capa, d608,h1, epsq2, g, beta
      use ctlblk_mod, only: jsta, jend, spval, lm1, jsta_m, jend_m, im, &
                            jsta_2l, jend_2u, lm
      use gridspec_mod, only: gridtype
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
!     DECLARE VARIABLES.
!     
      REAL,intent(in)    ::  EL(IM,jsta_2l:jend_2u,LM)
      REAL,intent(inout) ::  RICHNO(IM,jsta_2l:jend_2u,LM)
!
      REAL, ALLOCATABLE :: THV(:,:,:)
      integer I,J,L,IW,IE
      real APE,UHKL,ULKL,VHKL,VLKL,WNDSL,WNDSLP,RDZKL,             &
           DTHVKL,DUKL,DVKL,RI,CT,CS
!     real APE,UHKL,ULKL,VHKL,VLKL,WNDSL,WNDSLP,DZKL,RDZKL,Q2KL,QROOT,ELKL,  &
!          ELKLSQ,DTHVKL,DUKL,DVKL,RI,CT,CS
!
!     
!*************************************************************************
!     START CALRCH HERE.
!     
      ALLOCATE ( THV(IM,JSTA_2L:JEND_2U,LM) )
!     INITIALIZE ARRAYS.
!     
!$omp  parallel do
      DO L = 1,LM
        DO J=JSTA,JEND
        DO I=1,IM
          RICHNO(I,J,L)=SPVAL
        ENDDO
        ENDDO
      ENDDO
!
!     COMPUTE VIRTUAL POTENTIAL TEMPERATURE.
!
!$omp  parallel do private(i,j,ape)
      DO L=LM,1,-1
        DO J=JSTA,JEND
          DO I=1,IM
            APE        = (H10E5/PMID(I,J,L))**CAPA
            THV(I,J,L) = (Q(I,J,L)*D608+H1)*T(I,J,L)*APE
          ENDDO
        ENDDO
      ENDDO
!
!     COMPUTE GRADIENT RICHARDSON NUMBER AS CODED IN ETA MODEL
!     SUBROUTINE PROFQ2.F.  OUTER LOOP OVER THE VERTICAL. 
!     INTTER LOOP OVER THE HORIZONTAL.
!
!$omp  parallel do private(i,j,l,ie,iw,cs,ct,dthvkl,dukl,dvkl,             &
!$omp&         rdzkl,ri,uhkl,ulkl,vhkl,vlkl,wndsl,wndslp)
      DO L = 1,LM1
!
        if(GRIDTYPE /= 'A')THEN  
          call exch(VTM(1,jsta_2l,L))
          call exch(UH(1,jsta_2l,L))
          call exch(VH(1,jsta_2l,L))
          call exch(VTM(1,jsta_2l,L+1))
          call exch(UH(1,jsta_2l,L+1))
          call exch(VH(1,jsta_2l,L+1))
        end if  
         
        DO J=JSTA_M,JEND_M
          DO I=2,IM-1
!
            IF(GRIDTYPE == 'A')THEN
              UHKL = UH(I,J,L)
              ULKL = UH(I,J,L+1)
              VHKL = VH(I,J,L)
              VLKL = VH(I,J,L+1)
            ELSE IF(GRIDTYPE == 'E')THEN
              IE = I+MOD(J+1,2) 
              IW = I+MOD(J+1,2)-1
!
!         WE NEED (U,V) WINDS AT A MASS POINT.  FOUR POINT
!         AVERAGE (U,V) WINDS TO MASS POINT.  NORMALIZE FOUR
!         POINT AVERAGE BY THE ACTUAL NUMBER OF (U,V) WINDS
!         USED IN THE AVERAGING.  VTM=1 IF WIND POINT IS
!         ABOVE GROUND.  VTM=0 IF BELOW GROUND.
!
              WNDSL  = VTM(I,J-1,L)+VTM(IW,J,L)+VTM(IE,J,L)+VTM(I,J+1,L)
              WNDSLP = VTM(I,J-1,L+1) + VTM(IW,J,L+1)+                       &
                       VTM(IE,J,L+1)  + VTM(I,J+1,L+1)
              IF(WNDSL == 0. .OR. WNDSLP == 0.) cycle
              UHKL = (UH(I,J-1,L)+UH(IW,J,L)+UH(IE,J,L)+UH(I,J+1,L))/WNDSL
              ULKL = (UH(I,J-1,L+1)+UH(IW,J,L+1)+UH(IE,J,L+1)+             &
                      UH(I,J+1,L+1))/WNDSLP
              VHKL = (VH(I,J-1,L)+VH(IW,J,L)+VH(IE,J,L)+VH(I,J+1,L))/WNDSL
              VLKL = (VH(I,J-1,L+1)+VH(IW,J,L+1)+VH(IE,J,L+1)+             &
                     VH(I,J+1,L+1))/WNDSLP
            ELSE IF(GRIDTYPE == 'B')THEN
              IE = I 
              IW = I-1
              UHKL = (UH(IW,J-1,L)+UH(IW,J,L)+UH(IE,J-1,L)+UH(I,J,L))/4.0
              ULKL = (UH(IW,J-1,L+1)+UH(IW,J,L+1)+UH(IE,J-1,L+1)+             &
                      UH(I,J,L+1))/4.0
              VHKL = (VH(IW,J-1,L)+VH(IW,J,L)+VH(IE,J-1,L)+VH(I,J,L))/4.0
              VLKL = (VH(IW,J-1,L+1)+VH(IW,J,L+1)+VH(IE,J-1,L+1)+             &
                      VH(I,J,L+1))/4.0
            END IF

            RDZKL   = 1.0 / (ZMID(I,J,L)-ZMID(I,J,L+1))

!           Q2KL   = MAX(Q2(I,J,L),0.00001)
!           QROOT  = SQRT(Q2KL)
!           ELKL   = EL(I,J,L)
!           ELKL   = MAX(ELKL,EPSQ2)
!           ELKLSQ = ELKL*ELKL

            DTHVKL = THV(I,J,L)-THV(I,J,L+1)
            DUKL   = (UHKL-ULKL) * RDZKL
            DVKL   = (VHKL-VLKL) * RDZKL
            CS     = DUKL*DUKL + DVKL*DVKL
!     
!         COMPUTE GRADIENT RICHARDSON NUMBER.
!     
            IF(CS <= 1.E-8) THEN
!
!            WIND SHEAR IS VANISHINGLY SMALL - SO SET RICHARDSON
!            NUMBER TO POST PROCESSOR SPECIAL VALUE.
!
              RICHNO(I,J,L) = SPVAL
!
            ELSE
!
!         WIND SHEAR LARGE ENOUGH TO USE RICHARDSON NUMBER.
!
              CT = -1.*G*BETA*DTHVKL*RDZKL
              RI = -CT/CS
              RICHNO(I,J,L) = RI
            ENDIF
!
          ENDDO
        ENDDO
      ENDDO           ! end of l loop
!     
      DEALLOCATE (THV)
!     END OF ROUTINE.
!     
      RETURN
      END

