
      SUBROUTINE CALLLWS(U,V,H,LLWS)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALLLWS       COMPUTES Low Level Wind Shear (0-2000feet) 
!   PRGRMMR: Binbin Zhou      /NCEP/EMC  DATE: 2005-08-16       
!     
! ABSTRACT:  
!    This program computes the low level wind shear(LLWS) over 0-2000 feet (0-609.5m)
!    layer. But because 10m wind represent sfc wind, 10-619.5 m layer
!    is used. (NOAA/NWS Instruction 10-813, 2004)
!
!    Definition: LLWS(Z1,Z2) is vector difference of wind at z1 and z2
!          where Z1 = 10m   + Surface height
!                Z2 = 619.5 + Surface height
!
!    Algorithm: since Z2 is not defined in the model, so,
!           first thing is searching Z2  to see which layers 
!               it is located(ie between which two pressure levels), 
!           then find the wind vector (U2,V2)at Z2 by interpolating with 
!               the wind vectors of the at pressure levels above and below
!           then compute the vector difference between Z2 and Z1 (ie U10,V10)
!
!
!
!                               
!      ----------------------------------------- K2-1 ---------------------
!                            ^
!                            |
!                            |
!                            |            
!                 ____       |  _____ Z2, U2=interpo[U(K2),U(K2-1)]
!                  ^         |            V2=interpo[V(K2),V(K2-1)]
!                  |         |                       
!      ------------|---------|------------------ K2 ------------------------
!                  |         |
!                  |         |DH=SUM of all layers between K1-1 & K2-1 
!                  |         |                                            .              
!                  |609.5m   |                                            .
!                  |(2000ft) |                                            .
!                  |         v
!      ------------|---------------------------------------------------- LSM-2
!                  |               ^
!                  |               |ZH1   
!                  |               |
!                 o-o 10m       ___v__ Z1,U10,V10                 
!       FIS    ....|.....          ^
!        ^   .            .        |
!      --|-------------------------|------------ K1 -------------------- LSM-1
!        | .                .      |
!        |.                  .     |
!       .|                    ...  |
!      --|-------------------------|------------------------------------- LSM
!      . |                         |
!     ////////////////////////////////////////////////////////////////// Sea Level
!
!
! USAGE:    CALL CALLLWS(U,V,H,LLWS)
!   INPUT ARGUMENT LIST:
!     U     - U wind profile (m/s) (at pressure level)
!     V     - V wind (m/s)         (at pressure level)
!     H     - Height (m)           (at pressure level)
!
!   OUTPUT ARGUMENT LIST: 
!     LLWS  - Low level wind shear (Knots/2000ft) 
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!     LIBRARY:
!       NONE
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90/77
!     MACHINE : BLUE AT NCEP
!$$$  
!
      USE vrbls2d, only: fis, u10, v10
      use params_mod, only: gi
      use ctlblk_mod, only: jsta, jend, im, jm, lsm
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!     DECLARE VARIABLES.
!     
      REAL,DIMENSION(IM,JM,LSM),INTENT(IN)    :: U,V,H
      REAL,DIMENSION(IM,JM),INTENT(INOUT)     :: LLWS
      REAL    :: Z1,Z2,HZ1,DH,U2,V2,W2,RT 
      INTEGER :: K1,K2 
      integer I,J,LP

!***************************************************************
!
!

      DO 100 J=JSTA,JEND
        DO I=1,IM
 
          Z1 = 10.0 + FIS(I,J)*GI                              !Height of 10m levels geographic height (from sea level)
          
          IF(Z1.LT.H(I,J,LSM)) THEN                            !First search location of 10m wind level
            K1 = LSM + 1                                       !to see it is in which pressure levels
          ELSE
            DO LP = LSM,2,-1                                   !If not found, keep searching upward                              
             IF(Z1.GE.H(I,J,LP).AND.Z1.LT.H(I,J,LP-1)) THEN
               K1 = LP 
             END IF
            END DO
          END IF

          HZ1 = H(I,J,K1-1) - Z1                                !Distance between K1-1 and 10m level
 
          DH = 0.0

          IF((HZ1+10).GT.609.6) THEN                            !Then, search 2000ft(609.6m) location
            U2= U10(I,J) + (U(I,J,K1-1)-U10(I,J))*599.6/HZ1     !found it between K1-1 and K1, then linear
            V2= V10(I,J) + (V(I,J,K1-1)-V10(I,J))*599.6/HZ1     !interpolate to get wind at 2000ft U2,V2     
            Z2= FIS(I,J)*GI + 609.6
          ELSE                                                 !otherwise, keep on search upward
            DO LP = K1-1,2,-1
             DH=DH+(H(I,J,LP-1) - H(I,J,LP))
             IF((DH+HZ1+10).gt.609.6) THEN                      !found the 2000ft level 
               Z2=FIS(I,J)*GI+609.6   
               RT=(Z2-H(I,J,LP))/(H(I,J,LP-1)-H(I,J,LP))
               U2=U(I,J,LP)+RT*(U(I,J,LP-1)-U(I,J,LP))
               V2=V(I,J,LP)+RT*(V(I,J,LP-1)-V(I,J,LP))
               K2=LP
               GO TO 610
              END IF
             END DO
            END IF

!computer vector difference
610       LLWS(I,J)=SQRT((U2-U10(I,J))**2+(V2-V10(I,J))**2)/     &
                    609.6 * 1.943*609.6                         !unit: knot/2000ft

 
        ENDDO
 
100   CONTINUE     

      RETURN
      END


      SUBROUTINE CALICING (T1,RH,OMGA, ICING)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALICING       COMPUTES In-Flight Icing
!   PRGRMMR: Binbin Zhou      /NCEP/EMC  DATE: 2005-08-16       
!     
! ABSTRACT:  
!    This program computes the in-flight icing condition
!    with the T-RH-OMGA algorithm provided by S. Silberberg of
!    NCEP/AWC (improved new version)
! 
!    According to S. Silberberg, Icing happens in following 
!    situation:
!       (1) -22C < T < 0C to      
!       (2)  RH > 70 %
!       (3) Ascent air, OMGA < 0 
!       (4) Equivalent Potential Vorticity (EPV) < 0
!       (5) Cloud water if SLD (supercooled large droplet)
!
!    Current version dosn't consider SLD, so cloud water           
!    is not used. EPV computation is not available for current
!    NCEP/EMC models(NAM, WRF, RSM), so EPV is also not
!    used
!
! USAGE:    CALL CALICING(T1,RH,OMGA,ICING)
!   INPUT ARGUMENT LIST:
!     T1     - TEMPERATURE (K)
!     RH     - RELATIVE HUMIDITY  (DECIMAL FORM)
!     OMGA   - Vertical velocity (Pa/sec)
!
!   OUTPUT ARGUMENT LIST: 
!     ICING     - ICING CONDITION (1 or 0)
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!     LIBRARY:
!       NONE
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90/77
!     MACHINE : BLUE AT NCEP
!$$$  
!
      use ctlblk_mod, only: jsta, jend, im
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
!     DECLARE VARIABLES.
!     
      REAL, DIMENSION(IM,jsta:jend), INTENT(IN)    :: T1,RH,OMGA
      REAL, DIMENSION(IM,jsta:jend), INTENT(INOUT) :: ICING 
      integer I,J
!***************************************************************
!
!
      DO J=JSTA,JEND
        DO I=1,IM

         IF(OMGA(I,J) < 0.0 .AND.                       &
            (T1(I,J) <= 273.0 .AND. T1(I,J) >= 251.0)   &
              .AND. RH(I,J) >= 70.0) THEN

           ICING(I,J) = 1.0
         ELSE
           ICING(I,J) = 0.0
         END IF
        ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE CALCAT(U,V,H,U_OLD,V_OLD,H_OLD,CAT)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALCAT       COMPUTES Clear Air Turbulence Index 
!   PRGRMMR: Binbin Zhou      /NCEP/EMC  DATE: 2005-08-16       
!     
! ABSTRACT:  
!    This program computes the Clear Air Turbulence condition
!    which is expressed as Index with Ellrod Algorithm 
!    (Gary P. Ellrod: Wea. and Forecast,1992) and Ri number 
!    suggested by S. Silberberg of AWC. But Ri number is still
!    not classified into 3 level CAT, so current version does
!    not use Ri as suggested by S. Silberberg
!
! PROGRAM HISTORY LOG:
!
!   05-09-19  H CHUANG - MODIFIED TO COMPUTE GRADIENTS FOR BOTH A AND E GRIDS
!
!
!    According to Ellrod, the CAT is classied into 3 levels (index)
!    Light:  CAT = 1     
!    Middle: CAT = 2
!    Severe: CAT = 3
!    No CAT: CAT = 0 
!
! USAGE:    CALL CALCAT(U,V,H,L,CAT)
!   INPUT ARGUMENT LIST:
!     U     - U wind profile (m/s) (at pressure level)
!     V     - V wind (m/s)         (at pressure level)
!     H     - Height (m)           (at pressure level)
!     L     - # of pressure level
!
!   OUTPUT ARGUMENT LIST: 
!     CAT     - CAT Index
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!     LIBRARY:
!       NONE
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90/77
!     MACHINE : BLUE AT NCEP
!$$$  
      use masks, only: dx, dy
      use ctlblk_mod, only: spval, jsta_2l, jend_2u, jsta_m, jend_m, &
              im, jm
      use gridspec_mod, only: gridtype
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none

!     
!     DECLARE VARIABLES.
!     
      REAL,DIMENSION(IM,jsta_2l:jend_2u),INTENT(IN)    :: U,V,H, &
                                                U_OLD,V_OLD,H_OLD
!      INTEGER,INTENT(IN)                      :: L
      REAL,DIMENSION(IM,jsta_2l:jend_2u),INTENT(INOUT) :: CAT

      REAL  DSH, DST, DEF, CVG, VWS, TRBINDX
      INTEGER  IHE(JM),IHW(JM)
      integer I,J
      integer ISTART,ISTOP,JSTART,JSTOP
      real VWS1,VWS2,VWS3,VWS4

!***************************************************************
!
!
      CAT=SPVAL 
      DO J=JSTA_2L,JEND_2U
       IF(GRIDTYPE == 'A')THEN
        IHW(J)=-1
        IHE(J)=1 
	ISTART=2
        ISTOP=IM-1
        JSTART=JSTA_M
        JSTOP=JEND_M
       ELSE IF(GRIDTYPE=='E')THEN
        IHW(J)=-MOD(J,2)
        IHE(J)=IHW(J)+1
	ISTART=2
        ISTOP=IM-1
        JSTART=JSTA_M
        JSTOP=JEND_M
       ELSE IF(GRIDTYPE=='B')THEN
        IHW(J)=-1
        IHE(J)=0 
	ISTART=2
        ISTOP=IM-1
        JSTART=JSTA_M
        JSTOP=JEND_M
       ELSE	
        print*,'no gridtype specified, exit calcat comp'
	return	
       END IF	
      ENDDO

      call exch_f(U)
      call exch_f(V)
      call exch_f(U_OLD)
      call exch_f(V_OLD)
      call exch_f(H)
      call exch_f(H_OLD)

      DO 100 J=JSTART,JSTOP
        DO I=ISTART,ISTOP
!
          IF(GRIDTYPE=='B')THEN
!dsh=dv/dx+du/dy 
           DSH=(0.5*(V(I,J)+V(I,J-1))-0.5*(V(I-1,J)+V(I-1,J-1)))*10000./DX(I,J) &
	      +(0.5*(U(I,J)+U(I-1,J))-0.5*(U(I,J-1)+U(I-1,J-1)))*10000./DY(I,J)
!dst=du/dx-dv/dy
           DST =(0.5*(U(I,J)+U(I,J-1))-0.5*(U(I-1,J)+U(I-1,J-1)))*10000./DX(I,J) &
	      -(0.5*(V(I,J)+V(I-1,J))-0.5*(V(I,J-1)+V(I-1,J-1)))*10000./DY(I,J)
           DEF = SQRT (DSH*DSH + DST*DST)

!cvg=-(du/dx+dv/dy)
           CVG = -((0.5*(U(I,J)+U(I,J-1))-0.5*(U(I-1,J)+U(I-1,J-1)))*10000./DX(I,J) &
                +(0.5*(V(I,J)+V(I-1,J))-0.5*(V(I,J-1)+V(I-1,J-1)))*10000./DY(I,J))	   
          ELSE
!dsh=dv/dx+du/dy           
           DSH = (V(I+IHE(J),J) - V(I+IHW(J),J))*10000./(2*DX(I,J))   &  
              + (U(I,J+1) - U(I,J-1))*10000./(2*DY(I,J))

!dst=du/dx-dv/dy
           DST = (U(I+IHE(J),J) - U(I+IHW(J),J))*10000./(2*DX(I,J))   & 
              - (V(I,J+1) - V(I,J-1))*10000./(2*DY(I,J))

           DEF = SQRT (DSH*DSH + DST*DST)

!cvg=-(du/dx+dv/dy)
           CVG = -( (U(I+IHE(J),J) - U(I+IHW(J),J))*10000./(2*DX(I,J)) &
                  +(V(I,J+1) - V(I,J-1))*10000./(2*DY(I,J)) )
          END IF
	  
          IF(GRIDTYPE == 'A')THEN
!vws=d|U|/dz
           VWS = ( SQRT(U_OLD(I,J)**2+V_OLD(I,J)**2 ) -               &
                  SQRT(U(I,J)**2+V(I,J)**2 )   ) *                    &
                  1000.0/(H_OLD(I,J) - H(I,J))
          else IF(GRIDTYPE == 'E')THEN
!vws=d|U|/dz
	   VWS1 = ( SQRT(U_OLD(I+IHE(J),J)**2+V_OLD(I+IHE(J),J)**2 ) -&
                  SQRT(U(I+IHE(J),J)**2+V(I+IHE(J),J)**2 )   ) 
!vws=d|U|/dz
           VWS2 = ( SQRT(U_OLD(I+IHW(J),J)**2+V_OLD(I+IHW(J),J)**2 ) -&   
                  SQRT(U(I+IHW(J),J)**2+V(I+IHW(J),J)**2 )   ) 
!vws=d|U|/dz
           VWS3 = ( SQRT(U_OLD(I,J-1)**2+V_OLD(I,J-1)**2 ) -          & 
                  SQRT(U(I,J-1)**2+V(I,J-1)**2 )   ) 
!vws=d|U|/dz
           VWS4 = ( SQRT(U_OLD(I,J+1)**2+V_OLD(I,J+1)**2 ) -          & 
                  SQRT(U(I,J+1)**2+V(I,J+1)**2 )   ) 
           VWS=1000.0*(VWS1+VWS2+VWS3+VWS4)/4.0/(H_OLD(I,J) - H(I,J))
	  ELSE IF(GRIDTYPE == 'B')THEN
	   VWS1 = ( SQRT(U_OLD(I+IHE(J),J)**2+V_OLD(I+IHE(J),J)**2 ) -&
                  SQRT(U(I+IHE(J),J)**2+V(I+IHE(J),J)**2 )   ) 
!vws=d|U|/dz
           VWS2 = ( SQRT(U_OLD(I+IHW(J),J)**2+V_OLD(I+IHW(J),J)**2 ) -&   
                  SQRT(U(I+IHW(J),J)**2+V(I+IHW(J),J)**2 )   ) 
!vws=d|U|/dz
           VWS3 = ( SQRT(U_OLD(I,J-1)**2+V_OLD(I,J-1)**2 ) -          & 
                  SQRT(U(I,J-1)**2+V(I,J-1)**2 )   ) 
!vws=d|U|/dz
           VWS4 = ( SQRT(U_OLD(I-1,J-1)**2+V_OLD(I-1,J-1)**2 ) -          & 
                  SQRT(U(I-1,J-1)**2+V(I-1,J-1)**2 )   ) 
           VWS=1000.0*(VWS1+VWS2+VWS3+VWS4)/4.0/(H_OLD(I,J) - H(I,J)) 
	  END IF  
           
          TRBINDX = ABS(VWS)*(DEF + ABS(CVG))
	  
          IF(TRBINDX.LE.4.) THEN
            CAT(I,J) = 0.0
          ELSE IF(TRBINDX.LE.8.) THEN
            CAT(I,J)=1.0
          ELSE IF(TRBINDX.LE.12.) THEN
            CAT(I,J)=2.0
          ELSE
            CAT(I,J)=3.0
          END IF        
 
        ENDDO
 
100   CONTINUE     

      RETURN
      END


      SUBROUTINE CALCEILING (CLDZ,TCLD,CEILING)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALCEILING       COMPUTES Ceiling
!   PRGRMMR: Binbin Zhou      /NCEP/EMC  DATE: 2005-08-18       
!     
! ABSTRACT:  
!    This program computes the ceiling
!    Definition: Ceiling is the cloud base height for cloud fraction > 50%
!    The cloud base is from sea level in the model, while ceiling
!    is from surface. If no ceiling, set ceiling height = 20000 m
!
! USAGE:    CALL CALCEILING (CLDZ,TCLD,CEILING)
!   INPUT ARGUMENT LIST:
!     CLDZ   - CLOUD BASE HEIGHT from sea level(M)
!     TCLD   - TOTAL CLOUD FRACTION (%)
!
!   OUTPUT ARGUMENT LIST: 
!     CEILING - CEILING HEIGHT from surface (m)
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!     LIBRARY:
!       NONE
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90/77
!     MACHINE : BLUE AT NCEP
!$$$  
!

      USE vrbls2d, only: fis
      use params_mod, only: small, gi
      use ctlblk_mod, only: jsta, jend, spval, im
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
!     DECLARE VARIABLES.
!     
      REAL, DIMENSION(IM,jsta:jend), INTENT(IN)    :: CLDZ, TCLD
      REAL, DIMENSION(IM,jsta:jend), INTENT(INOUT) :: CEILING
      integer I,J
!***************************************************************
!
!
      DO J=JSTA,JEND
        DO I=1,IM
          IF(ABS(TCLD(I,J)-SPVAL) <= SMALL) THEN
            CEILING(I,J)=SPVAL
          ELSE IF(TCLD(I,J) >= 50.) THEN
            CEILING(I,J) = CLDZ(I,J)
          ELSE
            CEILING(I,J) = 20000.0
          END IF

          IF(CEILING(I,J) < 0.0) CEILING(I,J)=20000.0

        ENDDO
      ENDDO

      RETURN
      END


      SUBROUTINE CALFLTCND (CEILING,FLTCND)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALFLTCND   COMPUTES Ceiling
!   PRGRMMR: Binbin Zhou      /NCEP/EMC  DATE: 2005-08-18       
!     
! ABSTRACT:  
!    This program computes the flight condition restriction 
!    which is defined as follow (NOAA/NWS/Instruction for TAF, 2004):
!  
!                Ceiling(feet)             Visibility(miles)   FLTCND
!      LIFR        < 200           and/or      < 1               1
!      IFR      >= 500 to <  1000  and/or     >=1 to <  3        2
!      MVFR     >=1000 to <= 3000  and/or     >=3 to <= 5        3
!      VFR         > 3000                       > 5              5
!
!
! USAGE:    CALL CALFLTCND(CEILING,FLTCND)
!   INPUT ARGUMENT LIST:
!     CEILING - CEILING HEIGHT from surface (m)
!     NOTE: VIS - Visibility is passed through COMMON /VISB/
!
!   OUTPUT ARGUMENT LIST: 
!     FLTCND - FLIGHT CONDITION CATERGORY     
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!     LIBRARY:
!       NONE
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90/77
!     MACHINE : BLUE AT NCEP
!$$$  
!
      use vrbls2d, only: vis
      use ctlblk_mod, only: jsta, jend, im
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
!     DECLARE VARIABLES.
!     
      REAL, DIMENSION(IM,jsta:jend), INTENT(IN)    :: CEILING
      REAL, DIMENSION(IM,jsta:jend), INTENT(INOUT) :: FLTCND
      REAL  CEIL,VISI
      integer I,J
!
!***************************************************************
!
!
      DO J=JSTA,JEND
        DO I=1,IM
 
          CEIL = CEILING(I,J) * 3.2808               !from m -> feet
          VISI = VIS(I,J) / 1609.0                   !from m -> miles       

          IF(CEIL.LT.500.0 .OR. VISI.LT.1.0 ) THEN
             FLTCND(I,J) = 1.0

          ELSE IF( (CEIL.GE.500.AND.CEIL.LT.1000.0) .OR.          &
                   (VISI.GE.1.0.AND.VISI.LT.3.0) ) THEN
             FLTCND(I,J) = 2.0

          ELSE IF( (CEIL.GE.1000.AND.CEIL.LE.3000.0) .OR.         &
                   (VISI.GE.3.0.AND.VISI.LE.5.0) ) THEN
             FLTCND(I,J) = 3.0

          ELSE IF( CEIL.GT.3000.0  .OR. VISI.GT.5.0) THEN
             FLTCND(I,J) = 4.0

          END IF

        ENDDO
      ENDDO

      RETURN
      END
