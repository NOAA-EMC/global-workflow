      SUBROUTINE Rayleigh_damp(IM,IX,IY,KM,A,B,C,U1,V1,DT,CP,
     &                         LEVR,PRSL,PRSLRD0)
!
!   ********************************************************************
! ----->  I M P L E M E N T A T I O N    V E R S I O N   <----------
!
!          --- rayleigh friction with total energy conservation ---
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
!        prsl(IX,KM)   PRESSURE AT MIDDLE OF LAYER
!
!  OUTPUT
!        A, B, C AS AUGMENTED BY TENDENCY DUE TO RAYLEIGH FRICTION
!   ********************************************************************
      USE MACHINE , ONLY : kind_phys
      implicit none
!
      integer,intent(in)  :: im, ix, iy, km,levr
      real(kind=kind_phys),intent(in)    :: DT, CP, PRSLRD0
      real(kind=kind_phys),intent(in)    :: PRSL(IX,KM)
      real(kind=kind_phys),intent(in)    :: U1(IX,KM), V1(IX,KM)
      real(kind=kind_phys),intent(inout) :: A(IY,KM), B(IY,KM), C(IY,KM)

!--- local variables
      real(kind=kind_phys) rtrd(km)
      real(kind=kind_phys), parameter :: cons1=1.0, cons2=2.0, half=0.5
      real(kind=kind_phys) DTAUX, DTAUY, wrk1, rtrd1, rfactrd
      real(kind=kind_phys) ENG0, ENG1, tem1, tem2, dti, hfbcpdt
      integer i, k
!
! change PRSLRD0(2mb) to pascal
!      PRSLRD0  = 200.
!-----INITIALIZE SOME ARRAYS
!
      RTRD1 = 1./(5*86400) ! RECIPROCAL OF TIME SCALE PER SCALE HEIGHT
                           ! ABOVE BEGINNING SIGMA LEVEL FOR RAYLEIGH DAMPING
!
! pressure in pascal
      DO K=1,km
        IF(PRSL(1,K) < PRSLRD0) THEN
          wrk1 = LOG(PRSLRD0/PRSL(1,K))
          if (k > levr) then
            RTRD(K) = RTRD1 * wrk1 * wrk1
          else
            RTRD(K) = RTRD1 * wrk1
          endif
        ELSE
          RTRD(K) = 0
        ENDIF
      ENDDO

      dti = cons1 / dt
      hfbcpdt = half / (cp*dt)

      DO K = 1,KM
        RFACTRD = CONS1 / (CONS1+DT*RTRD(K)) - cons1
        DO I = 1,IM
          DTAUX  = U1(I,k) * RFACTRD
          DTAUY  = V1(I,k) * RFACTRD
          ENG0   = U1(I,K)*U1(I,K) + V1(I,K)*V1(I,K)
          tem1   = U1(I,K) + DTAUX
          tem2   = V1(I,K) + DTAUY
          ENG1   = tem1*tem1 + tem2*tem2
          A(I,K) = A(I,K) + DTAUY * dti
          B(I,K) = B(I,K) + DTAUX * dti
          C(I,K) = C(I,K) + (ENG0-ENG1) * hfbcpdt
        ENDDO
      ENDDO


      RETURN
      END
