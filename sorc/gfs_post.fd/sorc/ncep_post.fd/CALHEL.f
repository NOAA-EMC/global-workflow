      SUBROUTINE CALHEL(DEPTH,UST,VST,HELI,USHR1,VSHR1,USHR6,VSHR6)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALHEL       COMPUTES STORM RELATIVE HELICITY
!   PRGRMMR: BALDWIN         ORG: W/NP2      DATE: 94-08-22       
!     
! ABSTRACT:
!     THIS ROUTINE COMPUTES ESTIMATED STORM MOTION AND
!     STORM-RELATIVE ENVIRONMENTAL HELICITY.  
!     (DAVIES-JONES ET AL 1990) THE ALGORITHM PROCEEDS AS 
!     FOLLOWS.
!     
!     THE STORM MOTION COMPUTATION NO LONGER EMPLOYS THE DAVIES AND
!     JOHNS (1993) METHOD WHICH DEFINED STORM MOTION AS 30 DEGREES TO
!     THE RIGHT OF THE 0-6 KM MEAN WIND AT 75% OF THE SPEED FOR MEAN
!     SPEEDS LESS THAN 15 M/S AND 20 DEGREES TO THE RIGHT FOR SPEEDS
!     GREATER THAN 15 M/S.   INSTEAD, WE NOW USE THE DYNAMIC METHOD
!     (BUNKERS ET AL. 1998) WHICH HAS BEEN FOUND TO DO BETTER IN
!     CASES WITH 'NON-CLASSIC' HODOGRAPHS (SUCH AS NORTHWEST-FLOW
!     EVENTS) AND DO AS WELL OR BETTER THAN THE OLD METHOD IN MORE
!     CLASSIC SITUATIONS. 
!     
! PROGRAM HISTORY LOG:
!   94-08-22  MICHAEL BALDWIN
!   97-03-27  MICHAEL BALDWIN - SPEED UP CODE
!   98-06-15  T BLACK         - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO    - MPI VERSION
!   00-01-10  G MANIKIN       - CHANGED TO BUNKERS METHOD
!   02-05-22  G MANIKIN       - NOW ALLOW CHOICE OF COMPUTING
!                               HELICITY OVER TWO DIFFERENT
!                               (0-1 and 0-3 KM) DEPTHS
!   03-03-25  G MANIKIN       - MODIFIED CODE TO COMPUTE MEAN WINDS
!                               USING ARITHMETIC AVERAGES INSTEAD OF
!                               MASS WEIGHTING;  DIFFERENCES ARE MINOR
!                               BUT WANT TO BE CONSISTENT WITH THE
!                               BUNKERS METHOD
!   04-04-16  M PYLE          - MINIMAL MODIFICATIONS, BUT PUT INTO
!                                NMM WRFPOST CODE
!   05=02-25  H CHUANG        - ADD COMPUTATION FOR ARW A GRID
!   05-07-07  BINBIN ZHOU     - ADD RSM FOR A GRID  
!   
! USAGE:    CALHEL(UST,VST,HELI)
!   INPUT ARGUMENT LIST:
!     DPTH      - DEPTH IN METERS OVER WHICH HELICITY SHOULD BE COMPUTED;
!                 ALLOWS ONE TO DISTINGUISH 0-3 KM AND 0-1 KM VALUES
!
!   OUTPUT ARGUMENT LIST: 
!     UST      - ESTIMATED U COMPONENT (M/S) OF STORM MOTION.
!     VST      - ESTIMATED V COMPONENT (M/S) OF STORM MOTION.
!     HELI     - STORM-RELATIVE HELICITY (M**2/S**2)
! CRA
!     USHR1    - U COMPONENT (M/S) OF 0-1 KM SHEAR
!     VSHR1    - V COMPONENT (M/S) OF 0-1 KM SHEAR
!     USHR6    - U COMPONENT (M/S) OF 0-0.5 to 5.5-6.0 KM SHEAR
!     VSHR6    - V COMPONENT (M/S) OF 0-0.5 to 5.5-6.0 KM SHEAR
! CRA

!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!
!     LIBRARY:
!       COMMON   - VRBLS
!                  LOOPS
!                  PHYS 
!                  EXTRA
!                  MASKS
!                  OPTIONS
!                  INDX
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90
!     MACHINE : IBM SP
!$$$  
!
      use vrbls3d,    only: zmid, uh, vh, u, v, zint
      use vrbls2d,    only: fis, u10, v10
      use masks,      only: lmv
      use params_mod, only: g
      use lookup_mod, only: ITB,JTB,ITBQ,JTBQ
      use ctlblk_mod, only: jsta, jend, jsta_m, jend_m, jsta_2l, jend_2u, &
                            lm, im, jm, me
      use gridspec_mod, only: gridtype
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
      real,PARAMETER :: P150=15000.0,P300=30000.0,S15=15.0
      real,PARAMETER :: D3000=3000.0,PI6=0.5235987756,PI9=0.34906585
      real,PARAMETER :: D5500=5500.0,D6000=6000.0,D7000=7000.0
      real,PARAMETER :: D500=500.0
! CRA
      real,PARAMETER :: D1000=1000.0
      real,PARAMETER :: D1500=1500.0
! CRA

!     
!     DECLARE VARIABLES
!     
      real,intent(in)                                  :: DEPTH(2)
      REAL,dimension(IM,jsta_2l:jend_2u),  intent(out) :: UST,VST
      REAL,dimension(IM,jsta_2l:jend_2u,2),intent(out) :: HELI
!
      real, dimension(im,jsta_2l:jend_2u) :: HTSFC, UST6, VST6, UST5, VST5,   &
                                             UST1,  VST1, USHR1, VSHR1,       &
                                             USHR6, VSHR6, U1, V1, U2, V2,    &
                                             HGT1,  HGT2, UMEAN, VMEAN
!     REAL HTSFC(IM,JM)
!
!     REAL UST6(IM,JM),VST6(IM,JM)
!     REAL UST5(IM,JM),VST5(IM,JM)
!     REAL UST1(IM,JM),VST1(IM,JM)
! CRA
!     REAL USHR1(IM,JM),VSHR1(IM,JM),USHR6(IM,JM),VSHR6(IM,JM)
!     REAL U1(IM,JM),V1(IM,JM),U2(IM,JM),V2(IM,JM)
!     REAL HGT1(IM,JM),HGT2(IM,JM),UMEAN(IM,JM),VMEAN(IM,JM)
! CRA

      integer, dimension(im,jsta_2l:jend_2u) :: COUNT6, COUNT5, COUNT1, L1, L2
!     INTEGER COUNT6(IM,JM),COUNT5(IM,JM),COUNT1(IM,JM)
! CRA
!     INTEGER L1(IM,JM),L2(IM,JM)
! CRA

      INTEGER IVE(JM),IVW(JM)
      integer I,J,IW,IE,JS,JN,JVN,JVS,L,N,lv
      integer ISTART,ISTOP,JSTART,JSTOP
      real Z2,DZABV,UMEAN5,VMEAN5,UMEAN1,VMEAN1,UMEAN6,VMEAN6,      &
           DENOM,Z1,Z3,DZ,DZ1,DZ2,DU1,DU2,DV1,DV2
!     
!****************************************************************
!     START CALHEL HERE
!     
!     INITIALIZE ARRAYS.
!     
!$omp  parallel do private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          UST(I,J)    = 0.0
          VST(I,J)    = 0.0
          HELI(I,J,1) = 0.0
          HELI(I,J,2) = 0.0
          UST1(I,J)   = 0.0
          VST1(I,J)   = 0.0
          UST5(I,J)   = 0.0
          VST5(I,J)   = 0.0
          UST6(I,J)   = 0.0
          VST6(I,J)   = 0.0
          COUNT6(I,J) = 0
          COUNT5(I,J) = 0
          COUNT1(I,J) = 0
! CRA
          USHR1(I,J)  = 0.0
          VSHR1(I,J)  = 0.0
          USHR6(I,J)  = 0.0
          VSHR6(I,J)  = 0.0
          U1(I,J)     = 0.0
          U2(I,J)     = 0.0
          V1(I,J)     = 0.0
          V2(I,J)     = 0.0
          UMEAN(I,J)  = 0.0
          VMEAN(I,J)  = 0.0
          HGT1(I,J)   = 0.0
          HGT2(I,J)   = 0.0
          L1(I,J)     = 0
          L2(I,J)     = 0
! CRA

        ENDDO
      ENDDO
      IF(gridtype == 'E')THEN
        JVN =  1
        JVS = -1
        do J=JSTA,JEND
          IVE(J) = MOD(J,2)
          IVW(J) = IVE(J)-1
        enddo
        ISTART = 2
        ISTOP  = IM-1
        JSTART = JSTA_M
        JSTOP  = JEND_M
      ELSE IF(gridtype == 'B')THEN
        JVN = 1
        JVS = 0
        do J=JSTA,JEND
          IVE(J)=1
          IVW(J)=0
        enddo
        ISTART = 2
        ISTOP  = IM-1
        JSTART = JSTA_M
        JSTOP  = JEND_M
      ELSE
        JVN = 0
        JVS = 0
        do J=JSTA,JEND
          IVE(J) = 0
          IVW(J) = 0
        enddo
        ISTART = 1
        ISTOP  = IM
        JSTART = JSTA
        JSTOP  = JEND 
      END IF 
!
!     LOOP OVER HORIZONTAL GRID.
!
!      CALL EXCH(RES(1,jsta_2l)
!      CALL EXCH(PD()

!      DO L = 1,LP1
!        CALL EXCH(ZINT(1,jsta_2l,L))
!      END DO
! 
!!$omp  parallel do private(htsfc,ie,iw)
      IF(gridtype /= 'A') CALL EXCH(FIS(1:IM,JSTA_2L:JEND_2U))
      DO L = 1,LM
        IF(gridtype /= 'A') CALL EXCH(ZMID(1:IM,JSTA_2L:JEND_2U,L)) 
        DO J=JSTART,JSTOP
          DO I=ISTART,ISTOP
            IE = I+IVE(J)
            IW = I+IVW(J)
            JN = J+JVN 
            JS = J+JVS
!mp          PDSLVK=(PD(IW,J)*RES(IW,J)+PD(IE,J)*RES(IE,J)+
!mp     1           PD(I,J+1)*RES(I,J+1)+PD(I,J-1)*RES(I,J-1))*0.25
!mp          PSFCK=AETA(LMV(I,J))*PDSLVK+PT
            IF (gridtype=='B')THEN
              HTSFC(I,J) = (0.25/g)*(FIS(IW,J)+FIS(IE,J)+FIS(I,JN)+FIS(IE,JN))
!     
!     COMPUTE MASS WEIGHTED MEAN WIND IN THE 0-6 KM LAYER, THE
!  0-0.5 KM LAYER, AND THE 5.5-6 KM LAYER 
!
              Z2 = 0.25*(ZMID(IW,J,L)+ZMID(IE,J,L)+ZMID(I,JN,L)+ZMID(IE,JN,L))
            ELSE
              HTSFC(I,J) = (0.25/g)*(FIS(IW,J)+FIS(IE,J)+FIS(I,JN)+FIS(I,JS))
!     
!     COMPUTE MASS WEIGHTED MEAN WIND IN THE 0-6 KM LAYER, THE
!  0-0.5 KM LAYER, AND THE 5.5-6 KM LAYER 
!
              Z2 = 0.25*(ZMID(IW,J,L)+ZMID(IE,J,L)+ZMID(I,JN,L)+ZMID(I,JS,L))
            END IF
            DZABV = Z2-HTSFC(I,J)
  
            lv = NINT(LMV(I,J))
            IF (DZABV <= D6000 .AND. L <= lv) THEN
               UST6(I,J)   = UST6(I,J) + UH(I,J,L) 
               VST6(I,J)   = VST6(I,J) + VH(I,J,L)
               COUNT6(I,J) = COUNT6(I,J) + 1 
            ENDIF

            IF (DZABV < D6000 .AND. DZABV >= D5500 .AND.  L <= lv) THEN
               UST5(I,J)   = UST5(I,J) + UH(I,J,L)
               VST5(I,J)   = VST5(I,J) + VH(I,J,L)
               COUNT5(I,J) = COUNT5(I,J) + 1
            ENDIF         

            IF (DZABV < D500 .AND. L <= lv) THEN
               UST1(I,J)   = UST1(I,J) + UH(I,J,L)
               VST1(I,J)   = VST1(I,J) + VH(I,J,L) 
               COUNT1(I,J) = COUNT1(I,J) + 1
            ENDIF
! CRA
            IF (DZABV >= D1000 .AND. DZABV <= D1500 .AND.  L <= lv) THEN
               U2(I,J)   = U(I,J,L)
               V2(I,J)   = V(I,J,L)
               HGT2(I,J) = DZABV
               L2(I,J)   = L 
            ENDIF
    
            IF (DZABV >= D500 .AND. DZABV < D1000 .AND.               &
                L <= lv .AND. L1(I,J) <= L2(I,J)) THEN
               U1(I,J)   = U(I,J,L)
               V1(I,J)   = V(I,J,L)
               HGT1(I,J) = DZABV
               L1(I,J)   = L 
            ENDIF
! CRA 

          ENDDO
        ENDDO
      ENDDO
!
! CASE WHERE THERE IS NO LEVEL WITH HEIGHT BETWEEN 5500 AND 6000
!
      DO J=JSTART,JSTOP
        DO I=ISTART,ISTOP
          IF (COUNT5(I,J) == 0) THEN
            DO L=LM,1,-1
              IE=I+IVE(J)
              IW=I+IVW(J)
              JN=J+JVN
              JS=J+JVS
              IF (gridtype=='B')THEN
                Z2 = 0.25*(ZMID(IW,J,L)+ZMID(IE,J,L)+ZMID(I,JN,L)+ZMID(IE,JN,L))
              ELSE
                Z2 = 0.25*(ZMID(IW,J,L)+ZMID(IE,J,L)+ZMID(I,JN,L)+ZMID(I,JS,L))
              END IF

              DZABV=Z2-HTSFC(I,J)
              IF (DZABV < D7000 .AND. DZABV >= D6000) THEN 
                 UST5(I,J) = UST5(I,J) + UH(I,J,L)
                 VST5(I,J) = VST5(I,J) + VH(I,J,L)
                 COUNT5(I,J) = 1
                 GOTO 30
              ENDIF
            ENDDO
          ENDIF
30    CONTINUE
        ENDDO
      ENDDO

!
!$omp  parallel do private(i,j,umean6,vmean6,umean5,vmean5,umean1,vmean1,denom)

      DO J=JSTART,JSTOP
        DO I=ISTART,ISTOP
          IF (COUNT6(I,J) > 0 .AND. COUNT1(I,J) > 0 .AND. COUNT5(I,J) > 0) THEN
            UMEAN5 = UST5(I,J) / COUNT5(I,J)
            VMEAN5 = VST5(I,J) / COUNT5(I,J)
            UMEAN1 = UST1(I,J) / COUNT1(I,J)
            VMEAN1 = VST1(I,J) / COUNT1(I,J)
            UMEAN6 = UST6(I,J) / COUNT6(I,J)
            VMEAN6 = VST6(I,J) / COUNT6(I,J)
           
!
!      COMPUTE STORM MOTION VECTOR
!      IT IS DEFINED AS 7.5 M/S TO THE RIGHT OF THE 0-6 KM MEAN
!      WIND CONSTRAINED ALONG A LINE WHICH IS BOTH PERPENDICULAR
!      TO THE 0-6 KM MEAN VERTICAL WIND SHEAR VECTOR AND PASSES
!      THROUGH THE 0-6 KM MEAN WIND.  THE WIND SHEAR VECTOR IS
!      SET AS THE DIFFERENCE BETWEEN THE 5.5-6 KM WIND (THE HEAD
!      OF THE SHEAR VECTOR) AND THE 0-0.5 KM WIND (THE TAIL).
!      THIS IS FOR THE RIGHT-MOVING CASE;  WE IGNORE THE LEFT MOVER.

! CRA
            USHR6(I,J) = UMEAN5 - UMEAN1
            VSHR6(I,J) = VMEAN5 - VMEAN1

            DENOM = USHR6(I,J)*USHR6(I,J)+VSHR6(I,J)*VSHR6(I,J)
            IF (DENOM .NE. 0.0) THEN
              UST(I,J) = UMEAN6 + (7.5*VSHR6(I,J)/SQRT(DENOM))
              VST(I,J) = VMEAN6 - (7.5*USHR6(I,J)/SQRT(DENOM))
            ELSE
              UST(I,J) = 0
              VST(I,J) = 0
            ENDIF
          ELSE
            UST(I,J) = 0.0
            VST(I,J) = 0.0
            USHR6(I,J) = 0.0
            VSHR6(I,J) = 0.0
          ENDIF

          IF(L1(I,J) > 0 .AND. L2(I,J) > 0) THEN
            UMEAN(I,J) = U1(I,J) + (D1000 - HGT1(I,J))*(U2(I,J) -         &
                                    U1(I,J))/(HGT2(I,J) - HGT1(I,J))
            VMEAN(I,J) = V1(I,J) + (D1000 - HGT1(I,J))*(V2(I,J) -         &
                                    V1(I,J))/(HGT2(I,J) - HGT1(I,J))
          ELSE IF(L1(I,J) > 0 .AND. L2(I,J) == 0) THEN
            UMEAN(I,J) = U1(I,J)
            VMEAN(I,J) = V1(I,J)
          ELSE IF(L1(I,J) == 0 .AND. L2(I,J) > 0) THEN
            UMEAN(I,J) = U2(I,J)
            VMEAN(I,J) = U2(I,J)
          ELSE
            UMEAN(I,J) = 0.0
            VMEAN(I,J) = 0.0
          ENDIF

          IF(L1(I,J) > 0 .OR. L2(I,J) > 0) THEN
            USHR1(I,J) = UMEAN(I,J) - U10(I,J)
            VSHR1(I,J) = VMEAN(I,J) - V10(I,J)
          ELSE
            USHR1(I,J) = 0.0
            VSHR1(I,J) = 0.0
          ENDIF
! CRA

!tgs        USHR = UMEAN5 - UMEAN1
!           VSHR = VMEAN5 - VMEAN1

!           UST(I,J) = UMEAN6 + (7.5*VSHR/SQRT(USHR*USHR+VSHR*VSHR))
!           VST(I,J) = VMEAN6 - (7.5*USHR/SQRT(USHR*USHR+VSHR*VSHR))
!         ELSE
!           UST(I,J) = 0.0
!           VST(I,J) = 0.0
!        ENDIF

        ENDDO
      ENDDO
!
!       COMPUTE STORM-RELATIVE HELICITY
!
!!$omp  parallel do private(i,j,n,l,du1,du2,dv1,dv2,dz,dz1,dz2,dzabv,ie,iw,jn,js,z1,z2,z3)
      DO N=1,2 ! for dfferent helicity depth
        DO L = 2,LM-1
          if(GRIDTYPE /= 'A')then
            call exch(ZINT(1,jsta_2l,L))
            call exch(ZINT(1,jsta_2l,L+1))
          end if
          DO J=JSTART,JSTOP
            DO I=ISTART,ISTOP
              IW=I+IVW(J)
              IE=I+IVE(J)
              JN=J+JVN
              JS=J+JVS
              IF (gridtype=='B')THEN
                Z2=0.25*(ZMID(IW,J,L)+ZMID(IE,J,L)+                       &
                         ZMID(I,JN,L)+ZMID(IE,JN,L))                       
              ELSE
                Z2=0.25*(ZMID(IW,J,L)+ZMID(IE,J,L)+                       &
                         ZMID(I,JN,L)+ZMID(I,JS,L))
              END IF
              DZABV=Z2-HTSFC(I,J)
!
              IF(DZABV < DEPTH(N) .AND. L <= NINT(LMV(I,J)))THEN
                IF (gridtype=='B')THEN
                  Z1 = 0.25*(ZMID(IW,J,L+1)+ZMID(IE,J,L+1)+             &
                             ZMID(I,JN,L+1)+ZMID(IE,JN,L+1))
                  Z3 = 0.25*(ZMID(IW,J,L-1)+ZMID(IE,J,L-1)+             &
                             ZMID(I,JN,L-1)+ZMID(IE,JN,L-1))
                  DZ = 0.25*((ZINT(IW,J,L)+ZINT(IE,J,L)+                &
                              ZINT(I,JN,L)+ZINT(IE,JN,L))-              &
                             (ZINT(IW,J,L+1)+ZINT(IE,J,L+1)+            &
                              ZINT(I,JN,L+1)+ZINT(IE,JN,L+1)))
                ELSE
                  Z1 = 0.25*(ZMID(IW,J,L+1)+ZMID(IE,J,L+1)+             &
                             ZMID(I,JN,L+1)+ZMID(I,JS,L+1))
                  Z3 = 0.25*(ZMID(IW,J,L-1)+ZMID(IE,J,L-1)+              &
                            ZMID(I,JN,L-1)+ZMID(I,JS,L-1))
                  DZ = 0.25*((ZINT(IW,J,L)+ZINT(IE,J,L)+                 &
                              ZINT(I,JS,L)+ZINT(I,JN,L))-                &
                             (ZINT(IW,J,L+1)+ZINT(IE,J,L+1)+             &
                              ZINT(I,JS,L+1)+ZINT(I,JN,L+1)))
                END IF      
                DZ1 = Z1-Z2
                DZ2 = Z2-Z3
                DU1 = UH(I,J,L+1)-UH(I,J,L)
                DU2 = UH(I,J,L)-UH(I,J,L-1)
                DV1 = VH(I,J,L+1)-VH(I,J,L)
                DV2 = VH(I,J,L)-VH(I,J,L-1)
                HELI(I,J,N) = ((VH(I,J,L)-VST(I,J))*                      &
                               (DZ2*(DU1/DZ1)+DZ1*(DU2/DZ2))              &
                            -  (UH(I,J,L)-UST(I,J))*                      &
                               (DZ2*(DV1/DZ1)+DZ1*(DV2/DZ2)))             &
                               *DZ/(DZ1+DZ2)+HELI(I,J,N) 

!	    if(i==im/2.and.j==(jsta+jend)/2)print*,'Debug Helicity',depth(N),l,dz1,dz2,du1,  &
!	     du2,dv1,dv2,ust(i,j),vst(i,j)		      
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      END DO  ! end of different helicity depth
!
!     END OF ROUTINE.
!
      RETURN
      END
