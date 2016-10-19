      SUBROUTINE Rayleigh_damp_mesopause(IM,IX,IY,KM,A,B,C,U1,V1,DT,CP,
     &                         LEVR,PRSL,PRSLRD0)
!
!   ********************************************************************
! ----->  I M P L E M E N T A T I O N    V E R S I O N   <----------
!
!          --- rayleigh friction with total energy conserving ---
!              ----------------     -----------------------
!
!------ friction coefficient is based on deldif ----
!----------------------------------------------------------------------C
!    USE
!        ROUTINE IS CALLED FROM GBPHYS  (AFTER CALL TO GWDPS)
!
!    PURPOSE
!        USING THE GWD PARAMETERIZATIONS OF PS-GLAS AND PH-
!        GFDL TECHNIQUE.  THE TIME TENDENCIES OF U V ARE 
!        ALTERED TO INCLUDE/MIMIC THE EFFECT OF NON-STATIONARY 
!        GRAVITY WAVE DRAG FROM CONVECTION, FRONTGENOSIS,
!        WIND SHEAR ETC.  LOSS OF KINETIC ENERGY FORM GWD DRAG
!        IS CONVERTED INTO INTERNAL ENERGY.   
!
!  INPUT
!        A(IY,KM)  NON-LIN TENDENCY FOR V WIND COMPONENT
!        B(IY,KM)  NON-LIN TENDENCY FOR U WIND COMPONENT
!        C(IY,KM)  NON-LIN TENDENCY FOR TEMPERATURE
!        U1(IX,KM) ZONAL WIND M/SEC  AT T0-DT
!        V1(IX,KM) MERIDIONAL WIND M/SEC AT T0-DT
!        T1(IX,KM) TEMPERATURE DEG K AT T0-DT
!
!        DT  TIME STEP    SECS
!        PSRL(IX,KM)   P AT MIDDLE OF LAYER PASCAL
!        PSRLRD0       P LEVEL AT MIDDLE OF LAYER PASCAL FROM WHICH RAYLEIGH
!                      FRICTION IS APPLIED
!
!  OUTPUT
!        A, B, C AS AUGMENTED BY TENDENCY DUE TO RAYLEIGH FRICTION
!  Revision
!   Jan 2014: Jun Wang  Modified grid point rayleigh damping (henry Juang)
!                       to apply it around mesopause,and use pressure 
!                       instead of p/psfc at model layer
!   ********************************************************************
      USE MACHINE              , ONLY : kind_phys
      implicit none
!
      integer,intent(in)  ::  im, ix, iy, km,levr
      real(kind=kind_phys),intent(in) :: DT, CP, PRSLRD0
      real(kind=kind_phys),intent(in) :: PRSL(IX,KM)
      real(kind=kind_phys),intent(in) :: U1(IX,KM), V1(IX,KM)
      real(kind=kind_phys),intent(inout) :: A(IY,KM), B(IY,KM), C(IY,KM)
!
!--- local vars
      real(kind=kind_phys) RTRD(km)
      real(kind=kind_phys) CONS1, CONS2, HALF
      real(kind=kind_phys) SCLK
      real(kind=kind_phys) DTAUX, DTAUY, WRK1, RTRD0, RTRD1, RFACTRD
      real(kind=kind_phys) ENG0, ENG1
      integer I, K, KSTR, kmesopause
!
!     Some constants
!
      CONS1 = 1.0
      CONS2 = 2.0
      HALF  = CONS1/CONS2
!change prslrd0(2mb) to pascal 
!jw      PRSLRD0 = 200
!-----INITIALIZE SOME ARRAYS
!
      RTRD0=1./(80*86400.)       ! RECIPROCAL OF TIME SCALE PER SCALE HEIGHT, k0
      RTRD1=1./(4*86400.)        ! RECIPROCAL OF TIME SCALE PER SCALE HEIGHT, k1
      Kmesopause=95
!      if (me == 0) then
!         print *, '***IDEA*** Using physical diffusion in all layers'
!         print *,'rtrd1=',RTRD1,'rtrd0=',RTRD0,'Kmesopause=',Kmesopause
!      endif
! pressure in pascal
      KSTR=1
      DO K=1,km
        IF(PRSL(1,K) < PRSLRD0) THEN
          sclk=(K-Kmesopause)/10.
          RTRD(K) = RTRD0 + RTRD1*2.0/cosh(sclk)
        ELSE
          RTRD(K) = 0
        ENDIF
!        print *,'in rayleigh_damp_mesopause, k=',k,
!     &    'RTRD(K)=',RTRD(K),'RTRD1=',RTRD1,2.0/cosh(sclk),
!     &    'prsl(1,k)=',prsl(1,k),'prslrd0=',prslrd0
      ENDDO

      DO K = 1,KM
        DO I = 1,IM
          RFACTRD    = CONS1/(CONS1+DT*RTRD(K))
          DTAUX      = U1(I,k)*(RFACTRD-CONS1)/DT
          DTAUY      = V1(I,k)*(RFACTRD-CONS1)/DT
          ENG0       = HALF*(U1(I,K)*U1(I,K)+V1(I,K)*V1(I,K))
          ENG1       = HALF*((U1(I,K)+DTAUX*DT)*(U1(I,K)+DTAUX*DT)+
     &                       (V1(I,K)+DTAUY*DT)*(V1(I,K)+DTAUY*DT))
          A(I,K)     = A(I,K) + DTAUY
          B(I,K)     = B(I,K) + DTAUX
          C(I,K)     = C(I,K) + (ENG0-ENG1)/CP/DT
        ENDDO
      ENDDO

      RETURN
      END
