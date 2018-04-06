#include "../ESMFVersionDefine.h"

 SUBROUTINE ENS_Sto_Per_Scheme_Step2(Int_State, USES1, GRIDUSE, &
     Jul_Day, slat1, slat2, rc)

!DHOU, 10/17/2007  Added arguments 7 and 8, i.e. KEMAX(nreg) and nreg for regional rescaling
!DHOU, 10/17/2007  Added argument 6 i.e. RS_GLOBAL for global rescaling factor 
!DHOU  09/11/2007  Added Arguments
!USES1, 0=Zero-out, 1=no-change, 2=replaced with S2, 3=replace S1 and s2 with 0.5(s1+S2).
!           For arrays related to state 1,zem to qm.
!GRIDUSE, 1=S1, 2=S2; Convert the state S1/S2 from spectral to grid and  and back to spec.

! This subroutine is used to compute the second step of the
! stochastic perturbation scheme, in which it carries out the spectral 
! transform computation into the Gaussian grid space arrays, then
! computes the second step of the stochastic perturbation scheme that
! considering the local weighting influences.
!---------------------------------------------------------------------

! !REVISION HISTORY:
!
!  May 2007       Weiyu Yang Initial code for wave-grid conversion for model state .
!  Nov 2007       Dingchen Hou ddopted the code for global/regional rescaling as well 
!                 as conversion, for model state or its perturbation.    
!  Mar 2009       Weiyu Yang Modified for the NEMS model.
!  Sep 2011       Weiyu Yang Modified for using the ESMF 5.2.0r library.
!--------------------------------------------------------

      USE ESMF
 USE ENS_Cpl_InternalState_ESMFMod
 USE machine,  ONLY: kind_evod, kind_phys, kind_rad

 IMPLICIT none

 INCLUDE 'mpif.h'

 TYPE(ENS_Cpl_InternalState),  INTENT(inout) :: Int_State
 INTEGER,                      INTENT(out)   :: rc
 INTEGER                                     :: USES1, GRIDUSE
 INTEGER,                      INTENT(in)    :: Jul_Day
 INTEGER                                     :: ireg, k500
 REAL(KIND = kind_evod)                      :: slat1, slat2

 INTEGER                                     :: i, j, k, l
 INTEGER                                     :: lat
 INTEGER                                     :: lon_lat

 REAL(KIND = kind_evod)                      :: keavg
 REAL(KIND = kind_evod), PARAMETER           :: pi = 3.1415926535897931

 REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: t
 REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: u
 REAL(KIND=KIND_EVOD), DIMENSION(:, :, :), POINTER :: v

 REAL(KIND=KIND_EVOD), DIMENSION(:, :, :, :), POINTER :: tracer

 REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: ps
 REAL(KIND=KIND_EVOD), DIMENSION(:, :), POINTER :: KER

 REAL(KIND = kind_evod)                      :: tm
 REAL(KIND = kind_evod)                      :: um,   vm
 REAL(KIND = kind_evod)                      :: psm

 REAL(KIND = kind_evod)                      :: ts
 REAL(KIND = kind_evod)                      :: us,   vs
 REAL(KIND = kind_evod)                      :: pss

 REAL(KIND = KIND_EVOD), DIMENSION(:), POINTER :: tracerm
 REAL(KIND = KIND_EVOD), DIMENSION(:), POINTER :: tracers

 INTEGER                                     :: rc1
 INTEGER                                     :: rcfinal

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

 IF(.NOT. ASSOCIATED(KER))     ALLOCATE(KER(Int_State%nreg, Int_State%Total_member))
 IF(.NOT. ASSOCIATED(tracerm)) ALLOCATE(tracerm(Int_State%ntrac))
 IF(.NOT. ASSOCIATED(tracers)) ALLOCATE(tracers(Int_State%ntrac))

! ZERO OUT S1 arrays  (model state 1, for t-1)
!---------------------------------------------
 IF(USES1 < 0 .OR. USES1 > 3 ) THEN
     PRINT *, 'INVALID VALUE of USES1=', USES1, 'FORCED STOP, CHECK STEP2'
     STOP
 END IF

 IF(USES1 == 0) THEN
     DO k = 1, Int_State%arraysize_3
         DO j = 1, Int_State%arraysize_2
             DO i = 1, Int_State%arraysize_1
                 Int_State%twm(i, j, k) = 0.0
                 Int_State%uwm(i, j, k) = 0.0
                 Int_State%vwm(i, j, k) = 0.0
             END DO
         END DO
     END DO
     DO l = 1, Int_State%ntrac
         DO k = 1, Int_State%arraysize_3
             DO j = 1, Int_State%arraysize_2
                 DO i = 1, Int_State%arraysize_1
                     Int_State%tracerwm(i, j, k, l) = 0.0
                 END DO
             END DO
         END DO
     END DO
     DO j = 1, Int_State%arraysize_2
         DO i = 1, Int_State%arraysize_1
             Int_State%pswm (i, j) = 0.0
         END DO
     END DO
 END IF

!DHOU 09/06/2007 shift the arrays to process the t-1 time level instead of t level
!---------------------------------------------------------------------------------

 IF(GRIDUSE == 1 ) THEN
     t      => Int_State%twm
     u      => Int_State%uwm
     v      => Int_State%vwm
     tracer => Int_State%tracerwm
     ps     => Int_State%pswm
 ELSE IF(GRIDUSE == 2) THEN
     t      => Int_State%tw
     u      => Int_State%uw
     v      => Int_State%vw
     tracer => Int_State%tracerw
     ps     => Int_State%psw
 ELSE
     PRINT *, 'INVALID VALUE of GRIDUSE=', GRIDUSE, 'FORCED STOP, CHECK STEP2'
     STOP
 ENDIF

! Set up the maximum and minumum initial values for max/min etc.
!---------------------------------------------------------------
 tm      = -1e20
 tracerm = -1e20
 um      = -1e20
 vm      = -1e20
 psm     = -1e20

 ts      =  1e20
 tracers =  1e20
 us      =  1e20
 vs      =  1e20
 pss     =  1e20

 DO j = 1, Int_State%lats_node_a
     lat      = Int_State%global_lats_a(Int_State%ipt_lats_node_a - 1 + j)
     lon_lat  = Int_State%lonsperlat(lat)
     DO k = 1, Int_State%arraysize_3
         DO i = 1, lon_lat
             tm = MAX(tm, t(i, j, k))
             um = MAX(um, u(i, j, k))
             vm = MAX(vm, v(i, j, k))

             ts = MIN(ts, t(i, j, k))
             us = MIN(us, u(i, j, k))
             vs = MIN(vs, v(i, j, k))
         END DO
     END DO

     DO l = 1, Int_State%ntrac
         DO k = 1, Int_State%arraysize_3
             DO i = 1, lon_lat
                 tracerm(l) = MAX(tracerm(l), tracer(i, j, k, l))
                 tracers(l) = MIN(tracers(l), tracer(i, j, k, l))
             END DO
         END DO
     END DO

     DO i = 1, lon_lat
         psm = MAX(psm, ps(i, j))
         pss = MIN(pss, ps(i, j))
     END DO
 END DO
 
 PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum T      = ', tm,   ts  
 PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum q      = ', tracerm(1), tracers(1)
 PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum OZ     = ', tracerm(2), tracers(2)
 PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum CLW    = ', tracerm(3), tracers(3)
 PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum U      = ', um,   us  
 PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum V      = ', vm,   vs  
 PRINT*,'In the Ensemble Coupler, Step2, Maximum and Minumum Ps     = ', psm,  pss 

! Now we get the Gaussian grid fields and can use them for reginal processing.
!-----------------------------------------------------------------------------
 PRINT*, "RS_GLOBAL = ", Int_State%RS_GLOBAL
 IF(ABS(Int_State%RS_GLOBAL) > 0.999 .AND. ABS(Int_State%RS_GLOBAL) < 1.001 .AND. GRIDUSE == 2) THEN

!  Calculating regional rescaling factors fro KINETIC ENERGY at 500hPa.
!----------------------------------------------------------------------
     IF(.NOT. ASSOCIATED(Int_State%KE_work)) &
         ALLOCATE(Int_State%KE_work(Int_State%latmax, Int_State%Total_member))
     Int_State%KE_work = 0.0

!Identify the 500hPa level.
!--------------------------
     IF(Int_State%arraysize_3 == 28) THEN
         k500 = 13 
     ELSE IF(Int_State%arraysize_3 == 64) THEN
         k500 = 25 
     END IF

!Calculate 500hPa Kineitic  Energy KE.
!-------------------------------------
     DO j = 1, Int_State%lats_node_a
         lat      = Int_State%global_lats_a(Int_State%ipt_lats_node_a - 1 + j)
         lon_lat  = Int_State%lonsperlat(lat)
         keavg    = 0.0
         DO i = 1, lon_lat
             keavg = keavg + u(i, j, k500) ** 2 + v(i, j, k500) ** 2 
         END DO
         keavg = 0.5 * keavg / float(lon_lat)
         Int_State%KE_work(lat, Int_State%member_id(Int_State%mm1)) = keavg
     END DO

! Start global broadcast 
! Broadcasting the average KE over this latitude to other tasks
!--------------------------------------------------------------
!    PRINT*,'In the Ensemble Coupler, Start BCST, # of cpus', Int_State%nodes

     DO k = 1, Int_State%nodes    ! each task
         DO j = 1, Int_State%lats_node_global(k)    !each latitude
             CALL ENS_bcst_global &
                 (Int_State%KE_work(Int_State%lats_global(j, k), Int_State%member_id(k)), k - 1, rc1)
         END DO
     END DO

! PRINT*,'In the Ensemble Coupler, Finished BCST, '

! Print out KE_work
!------------------
     IF(Int_State%me == 0) THEN
         PRINT*, 'In the Ensemble Coupler, AFTER BCST, test KE_work'
         DO k = 1, Int_State%latmax
             WRITE(*,'(1x,i4,f6.3,21f8.3)')  &
                 k, Int_State%slat_work(k), (Int_State%KE_work(k, j), j = 1, Int_State%Total_member)
         END DO
     END IF

!Calculating regiobnal re-scaling factors from the KE_work array.
!----------------------------------------------------------------
!    PRINT *, 'DHHHTEST', Int_State%RS_GLOBAL, GRIDUSE, Int_State%KEMAX

     CALL GET_SCALING_FACTORS(Int_State%KE_work, Int_State%latmax, Int_State%Total_member, &
         Int_State%slat_work, Int_State%KEMAX, KER, Int_State%factor1_work,                &
         Int_State%nreg, slat1, slat2)

     IF(Int_State%me == 35) THEN
         PRINT*,'In the Ensemble Coupler, BCST, itest KER', Int_State%Cpl_Run_Calling_Number
         DO k = 1, Int_State%nreg
             WRITE(*,'(1x,A4,i4,15f8.3)') 'KER ', k, (KER(k, j), j = 1, Int_State%Total_member) 
         END DO
         PRINT*,'In the Ensemble Coupler, BCST, itest FAW factor1_work', Int_State%Cpl_Run_Calling_Number
         DO k = 1, Int_State%nreg
             WRITE(*,'(1x,A4,i4,15f8.3)')  'FAW ', k, (Int_State%factor1_work(k, j), j = 1, Int_State%Total_member)
         END DO
     END IF
     DEALLOCATE(Int_State%KE_work)

 ENDIF !! (ABS(Int_State%RS_GLOBAL) > 0.999 .AND. ABS(Int_State%RS_GLOBAL) < 1.001 .AND. GRIDUSE == 2)

!end of the if_block of 500hPa KE base rescaling factor calculation
!------------------------------------------------------------------

! Do the Rescaling for all model variables except ps.
!----------------------------------------------------
 DO j = 1, Int_State%lats_node_a
     lat      = Int_State%global_lats_a(Int_State%ipt_lats_node_a - 1 + j)
     lon_lat  = Int_State%lonsperlat(lat)

! assigning rescaling factor, 3-belt or latitude-dependent
!---------------------------------------------------------
     IF(ABS(Int_State%RS_GLOBAL) > 0.999 .AND. ABS(Int_State%RS_GLOBAL) < 1.001 .AND. GRIDUSE == 2) THEN 
         IF(Int_State%slat_work(lat) >= slat1) THEN
             ireg = 1
         ELSE IF(Int_State%slat_work(lat) >= slat2) THEN
             ireg = 2
         ELSE
             ireg = 3
         END IF
! DHOU 12/10/2007, KE based, 3-belt rescaling
!--------------------------------------------
         Int_State%RS = Int_State%RS_GLOBAL * Int_State%factor1_work(ireg,Int_State%member_id(Int_State%mm1))
     ELSE
! DHOU 12/10/2007, Latitude-Julian_Day based rescaling
!-----------------------------------------------------
!        Int_State%RS = Int_State%RS_GLOBAL * ( 1.0 + 0.2 * ASIN(Int_State%slat_work(lat)) * 2.0/pi &
!                       * COS( (Jul_Day-1)*pi/182.0 ) )                      ! linear function of latitude
         Int_State%RS = Int_State%RS_GLOBAL * ( 1.0 + 0.2 * Int_State%slat_work(lat) * COS( (Jul_Day-1)*pi/182.0 ) )  
                                                                             ! linear function of sin(latitude)
         IF(Int_State%me == 00) THEN
             PRINT *,'RS=', j, lat, Int_State%slat_work(lat), Int_State%RS, Jul_Day
         END IF
     END IF

! Applying the rescaling factor
!------------------------------
     DO k = 1, Int_State%arraysize_3
         DO i = 1, lon_lat
             t(i, j, k) = t(i, j, k) * Int_State%RS
             u(i, j, k) = u(i, j, k) * Int_State%RS
             v(i, j, k) = v(i, j, k) * Int_State%RS
         END DO
     END DO

     DO l = 1, Int_State%ntrac
         DO k = 1, Int_State%arraysize_3
             DO i = 1, lon_lat
                 tracer(i, j, k, l) = tracer(i, j, k, l) * Int_State%RS
             END DO
         END DO
     END DO

     DO i = 1, lon_lat
         ps(i, j) = ps(i, j) * Int_State%RS
     END DO
 END DO

! Replace State S2 with 0.5*(S1+S2) or Replace State S1 with S2
!--------------------------------------------------------------
 IF(USES1 == 3 ) THEN
     DO k = 1, Int_State%arraysize_3
         DO j = 1, Int_State%arraysize_2
             DO i = 1, Int_State%arraysize_1
                 Int_State%tw(i, j, k) = 0.5 * (Int_State%tw(i, j, k) + Int_State%twm(i, j, k)) 
                 Int_State%uw(i, j, k) = 0.5 * (Int_State%uw(i, j, k) + Int_State%uwm(i, j, k))
                 Int_State%vw(i, j, k) = 0.5 * (Int_State%vw(i, j, k) + Int_State%vwm(i, j, k))
             END DO
         END DO
     END DO
     DO l = 1, Int_State%ntrac
         DO k = 1, Int_State%arraysize_3
             DO j = 1, Int_State%arraysize_2
                 DO i = 1, Int_State%arraysize_1
                     Int_State%tracerw(i, j, k, l) = 0.5 *                               &
                         (Int_State%tracerw(i, j, k, l) + Int_State%tracerwm(i, j, k, l)) 
                 END DO
             END DO
         END DO
     END DO
     DO i = 1, Int_State%arraysize_1
         Int_State%psw(i, j) = 0.5 * (Int_State%psw(i, j) + Int_State%pswm(i, j)) 
     END DO
 ELSE IF(USES1 == 2 ) THEN
     DO k = 1, Int_State%arraysize_3
         DO j = 1, Int_State%arraysize_2
             DO i = 1, Int_State%arraysize_1
                 Int_State%twm(i, j, k) = Int_State%tw(i, j, k)
                 Int_State%uwm(i, j, k) = Int_State%uw(i, j, k)
                 Int_State%vwm(i, j, k) = Int_State%vw(i, j, k)
             END DO
         END DO
     END DO
     DO l = 1, Int_State%ntrac
         DO k = 1, Int_State%arraysize_3
             DO j = 1, Int_State%arraysize_2
                 DO i = 1, Int_State%arraysize_1
                     Int_State%tracerwm(i, j, k, l) = Int_State%tracerw(i, j, k, l)
                 END DO
             END DO
         END DO
     END DO
     DO j = 1, Int_State%arraysize_1
         DO i = 1, Int_State%arraysize_1
             Int_State%pswm(i, j) = Int_State%psw(i, j)
         END DO
     END DO
 END IF

! test the final maximum and minumum step2 perturbations.
!--------------------------------------------------------
 tm      = -1e20
 tracerm = -1e20
 um      = -1e20
 vm      = -1e20
 psm     = -1e20

 ts      =  1e20
 tracers =  1e20
 us      =  1e20
 vs      =  1e20
 pss     =  1e20

 DO j = 1, Int_State%lats_node_a
     lat     = Int_State%global_lats_a(Int_State%ipt_lats_node_a - 1 + j)
     lon_lat = Int_State%lonsperlat(lat)
     DO k = 1, Int_State%arraysize_3
         DO i = 1, lon_lat
             tm = MAX(tm, t(i, j, k))
             um = MAX(um, u(i, j, k))
             vm = MAX(vm, v(i, j, k))

             ts = MIN(ts, t(i, j, k))
             us = MIN(us, u(i, j, k))
             vs = MIN(vs, v(i, j, k))
         END DO
     END DO

     DO l = 1, Int_State%ntrac
         DO k = 1, Int_State%arraysize_3
             DO i = 1, lon_lat
                 tracerm(l) = MAX(tracerm(l), tracer(i, j, k, l))
                 tracers(l) = MIN(tracers(l), tracer(i, j, k, l))
             END DO
         END DO
     END DO

     DO i = 1, lon_lat
         psm = MAX(psm, ps(i, j))
         pss = MIN(pss, ps(i, j))
     END DO
 END DO

 PRINT*,'In the Ensemble Coupler, After Step2, Maximum and Minumum T      = ', tm,   ts
 PRINT*,'In the Ensemble Coupler, After Step2, Maximum and Minumum q      = ', tracerm(1), tracers(1)
 PRINT*,'In the Ensemble Coupler, After Step2, Maximum and Minumum OZ     = ', tracerm(2), tracers(2)
 PRINT*,'In the Ensemble Coupler, After Step2, Maximum and Minumum CLW    = ', tracerm(3), tracers(3)
 PRINT*,'In the Ensemble Coupler, After Step2, Maximum and Minumum U      = ', um,   us
 PRINT*,'In the Ensemble Coupler, After Step2, Maximum and Minumum V      = ', vm,   vs
 PRINT*,'In the Ensemble Coupler, After Step2, Maximum and Minumum Ps     = ', psm,  pss

 IF(rcfinal == ESMF_SUCCESS) THEN
     PRINT*, "PASS: ENS_Sto_Per_Scheme_Step2."
 ELSE
     PRINT*, "FAIL: ENS_Sto_Per_Scheme_Step2."
 END IF

 rc = rcfinal

 END SUBROUTINE ENS_Sto_Per_Scheme_Step2





 SUBROUTINE GET_SCALING_FACTORS(KE,nlat,nmember,slat,KEMAX,KER,factor1,nregion,slat1,slat2)
 USE machine,  ONLY: kind_evod
 INTEGER nlat,nmember,nregion
 REAL(KIND = kind_evod)                      :: KE(nlat,nmember)
 REAL(KIND = kind_evod)                      :: factor1(nregion,nmember) 
 REAL(KIND = kind_evod)                      :: slat(nlat),slat1,slat2 
 REAL(KIND = kind_evod)                      :: KEMAX(nregion) 
 REAL(KIND = kind_evod)                      :: KER(nregion,nmember) 
 REAL(KIND = kind_evod)                      :: WEIGHT(3) 
 REAL(KIND = kind_evod)                      :: coslat
 INTEGER i,j,k

 DO k=1,nmember
  DO j=1,nregion
    KER(j,k)=0.0
  ENDDO
 ENDDO
 DO k=1,nmember-1
  DO j=1,nregion
    WEIGHT(j)=0.0
  ENDDO 
  DO j=1,nlat
   coslat=sqrt(1-slat(j)**2)
   IF (slat(j).gt.slat1) then
    KER(1,k)=KER(1,k)+KE(j,k)*coslat
    WEIGHT(1)=WEIGHT(1)+coslat
   ELSEIF (slat(j).gt.slat2) then
    KER(2,k)=KER(2,k)+KE(j,k)*coslat
    WEIGHT(2)=WEIGHT(2)+coslat
   ELSE
    KER(3,k)=KER(3,k)+KE(j,k)*coslat
    WEIGHT(3)=WEIGHT(3)+coslat
   ENDIF
  ENDDO
  DO j=1,nregion
    IF (WEIGHT(j).lt.1.0E-5) THEN
     PRINT *, 'Weight=0 In GET_SCALING_FACTORS, forced to stop! for region/member',j,k
     STOP
    ENDIF
    KER(j,k)=KER(j,k)/WEIGHT(j)
  ENDDO 
  DO j=1,nregion
   if (KER(j,k).gt.KEMAX(j)) THEN
    factor1(j,k)=KEMAX(j)/KER(j,k)
   else
    factor1(j,k)=1.0
   endif
  ENDDO
 ENDDO

 RETURN
 END
