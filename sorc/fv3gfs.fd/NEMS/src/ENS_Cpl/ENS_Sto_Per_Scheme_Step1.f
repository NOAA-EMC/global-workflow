#include "../ESMFVersionDefine.h"

 SUBROUTINE ENS_Sto_Per_Scheme_Step1(cst)

! March    2009 Weiyu Yang Modified for the NEMS model.
! February 2011 Weiyu Yang Updated to use both the ESMF 4.0.0rp2 library,
!                          ESMF 5 library and the the ESMF 3.1.0rp2 library.
! May      2011 Weiyu yang Modified for using the ESMF 5.2.0r_beta_snapshot_07.
! September2011 Weiyu yang Modified for using the ESMF 5.2.0r library.
!---------------------------------------------------------------------------

! This subroutine is used to compute the first step of the 
! stochastic perturbation scheme, in which X_i_dot = T_i + S_i 
! and S_i ~ SUM(W_i,j P_j.
!-------------------------------------------------------------


! In the current code, assume each ensemble member uses the same 
! number of the processors.
!---------------------------------------------------------------
 USE ENS_Cpl_InternalState_ESMFMod

 IMPLICIT none

 TYPE(ENS_Cpl_InternalState),                         INTENT(inout) :: cst

! Working arrays.
!----------------
 INTEGER                                             :: tb
 INTEGER                                             :: i, j, k, l
 REAL(KIND=KIND_EVOD), DIMENSION(cst%ARRAY_ONE_SIZ4) :: pstm, psmtm
 REAL(KIND=KIND_EVOD), DIMENSION(cst%ARRAY_TOT_SIZ4) :: ttm, tmtm, utm, umtm, vtm, vmtm

 REAL(KIND=KIND_EVOD), DIMENSION(cst%ARRAY_TOT_SIZ4, cst%ntrac) :: tracertm, tracermtm

! Sto_Coef(k,j) is the weight of the k'th perturabtion in member j's formulation. 
!--------------------------------------------------------------------------------
 tb = cst%Total_member

 DO i = 1, cst%ARRAY_ONE_SIZ4
     pstm (i) = cst%ps  (i, tb) - cst%ps6  (i, tb)
     psmtm(i) = cst%psm (i, tb) - cst%ps6m (i, tb)
 END DO

 DO i = 1, cst%ARRAY_TOT_SIZ4
     ttm (i) = cst%t (i, tb) - cst%t6 (i, tb)
     tmtm(i) = cst%tm(i, tb) - cst%t6m(i, tb)
     utm (i) = cst%u (i, tb) - cst%u6 (i, tb)
     umtm(i) = cst%um(i, tb) - cst%u6m(i, tb)
     vtm (i) = cst%v (i, tb) - cst%v6 (i, tb)
     vmtm(i) = cst%vm(i, tb) - cst%v6m(i, tb)
 END DO

 DO j = 1, cst%ntrac
     DO i = 1, cst%ARRAY_TOT_SIZ4
         tracertm (i, j) = cst%tracer (i, tb, j) - cst%tracer6 (i, tb, j)
         tracermtm(i, j) = cst%tracerm(i, tb, j) - cst%tracer6m(i, tb, j)
     END DO
 END DO

 DO i = 1, cst%ARRAY_ONE_SIZ4
     cst%ps_step1 (i, tb) = 0.0
     cst%psm_step1(i, tb) = 0.0
 END DO
 DO i = 1, cst%ARRAY_TOT_SIZ4
     cst%t_step1   (i, tb) = 0.0
     cst%u_step1   (i, tb) = 0.0
     cst%v_step1   (i, tb) = 0.0
     cst%tm_step1  (i, tb) = 0.0
     cst%um_step1  (i, tb) = 0.0
     cst%vm_step1  (i, tb) = 0.0
 END DO

 DO j = 1, cst%ntrac
     DO i = 1, cst%ARRAY_TOT_SIZ4
         cst%tracer_step1 (i, tb, j) = 0.0
         cst%tracerm_step1(i, tb, j) = 0.0
     END DO
 END DO

 DO j = 1, tb - 1     !for the forcing of each member j
     DO i = 1, cst%ARRAY_ONE_SIZ4
         cst%ps_step1 (i, j) = 0.0
         cst%psm_step1(i, j) = 0.0
     END DO
     DO k = 1, tb - 1
         DO i = 1, cst%ARRAY_ONE_SIZ4
             cst%ps_step1  (i, j) = cst%ps_step1  (i, j) + cst%Sto_Coef(k, j) *     &
                 (cst%ps (i, k) - cst%ps6 (i, k) - pstm(i))
             cst%psm_step1 (i, j) = cst%psm_step1 (i, j) + cst%Sto_Coef(k, j) *     &
                 (cst%psm(i, k) - cst%ps6m(i, k) - psmtm(i))
         END DO
     END DO

     DO i = 1, cst%ARRAY_TOT_SIZ4
         cst%t_step1   (i, j) = 0.0
         cst%u_step1   (i, j) = 0.0
         cst%v_step1   (i, j) = 0.0
         cst%tm_step1  (i, j) = 0.0
         cst%um_step1  (i, j) = 0.0
         cst%vm_step1  (i, j) = 0.0
     END DO
     DO k = 1, tb - 1
         DO i = 1, cst%ARRAY_TOT_SIZ4
             cst%t_step1   (i, j) = cst%t_step1   (i, j) + cst%Sto_Coef(k, j) *     &
                 (cst%t    (i, k) - cst%t6   (i, k) - ttm(i))
             cst%tm_step1  (i, j) = cst%tm_step1  (i, j) + cst%Sto_Coef(k, j) *     &
                 (cst%tm   (i, k) - cst%t6m  (i, k) - tmtm(i))

             cst%u_step1   (i, j) = cst%u_step1   (i, j) + cst%Sto_Coef(k, j) *     &
                 (cst%u    (i, k) - cst%u6   (i, k) - utm(i))
             cst%um_step1  (i, j) = cst%um_step1  (i, j) + cst%Sto_Coef(k, j) *     &
                 (cst%um   (i, k) - cst%u6m  (i, k) -umtm(i))

             cst%v_step1   (i, j) = cst%v_step1   (i, j) + cst%Sto_Coef(k, j) *     &
                 (cst%v    (i, k) - cst%v6  (i, k) - vtm(i))
             cst%vm_step1  (i, j) = cst%vm_step1  (i, j) + cst%Sto_Coef(k, j) *     &
                 (cst%vm   (i, k) - cst%v6m  (i, k) - vmtm(i))
         END DO
     END DO
 END DO

 DO l = 1, cst%ntrac
     DO j = 1, tb - 1     !for the forcing of each member j
         DO i = 1, cst%ARRAY_TOT_SIZ4
             cst%tracer_step1 (i, j, l) = 0.0
             cst%tracerm_step1(i, j, l) = 0.0
         END DO
     
         DO k = 1, tb - 1
             DO i = 1, cst%ARRAY_TOT_SIZ4
                 cst%tracer_step1 (i, j, l) = cst%tracer_step1 (i, j, l) + cst%Sto_Coef(k, j) *     &
                     (cst%tracer (i, k, l) - cst%tracer6 (i, k, l) - tracertm(i, l))
                 cst%tracerm_step1(i, j, l) = cst%tracerm_step1(i, j, l) + cst%Sto_Coef(k, j) *     &
                     (cst%tracerm(i, k, l) - cst%tracer6m(i, k, l) - tracermtm(i, l))
             END DO
         END DO
     END DO
 END DO

! New feature in B series  DHOU 03-28-2007
! Centralize the perturbations w_step1
! Find the average of the perturbations.
!-----------------------------------------
 IF(cst%CENTER == 1) THEN
     DO i = 1, cst%ARRAY_ONE_SIZ4
         cst%ps_mean(i)  = 0.0
         cst%psm_mean(i) = 0.0
     END DO
     DO j = 1, tb - 1     !for the forcing of each member j
         DO i = 1, cst%ARRAY_ONE_SIZ4
             cst%ps_mean(i)  = cst%ps_mean(i)  + cst%ps_step1 (i, j)
             cst%psm_mean(i) = cst%psm_mean(i) + cst%psm_step1(i, j)
         END DO
     END  DO
     DO i = 1, cst%ARRAY_ONE_SIZ4
         cst%ps_mean (i) = cst%ps_mean (i) / FLOAT(tb - 1)
         cst%psm_mean(i) = cst%psm_mean(i) / FLOAT(tb - 1)
     END DO

! subtract the PS average from the perturbations.
!------------------------------------------------
     DO j = 1, tb - 1  
         DO i = 1, cst%ARRAY_ONE_SIZ4
             cst%ps_step1 (i, j) = cst%ps_step1 (i, j) - cst%ps_mean(i)
             cst%psm_step1(i, j) = cst%psm_step1(i, j) - cst%psm_mean(i)
         END DO
     END  DO

     DO i = 1, cst%ARRAY_TOT_SIZ4
         cst%t_mean (i) = 0.0
         cst%tm_mean(i) = 0.0
         cst%u_mean (i) = 0.0
         cst%um_mean(i) = 0.0
         cst%v_mean (i) = 0.0
         cst%vm_mean(i) = 0.0
     END DO
     DO j = 1, cst%ntrac
         DO i = 1, cst%ARRAY_TOT_SIZ4
             cst%tracer_mean (i, j) = 0.0
             cst%tracerm_mean(i, j) = 0.0
         END DO
     END DO


     DO j = 1, tb - 1     !for the forcing of each member j
         DO i = 1, cst%ARRAY_TOT_SIZ4
             cst%t_mean (i) = cst%t_mean (i) + cst%t_step1 (i, j)
             cst%tm_mean(i) = cst%tm_mean(i) + cst%tm_step1(i, j)
             cst%u_mean (i) = cst%u_mean (i) + cst%u_step1 (i, j)
             cst%um_mean(i) = cst%um_mean(i) + cst%um_step1(i, j)
             cst%v_mean (i) = cst%v_mean (i) + cst%v_step1 (i, j)
             cst%vm_mean(i) = cst%vm_mean(i) + cst%vm_step1(i, j)
         END DO
     END DO

     DO l = 1, cst%ntrac
         DO j = 1, tb - 1 
             DO i = 1, cst%ARRAY_TOT_SIZ4
                 cst%tracer_mean (i, l) = cst%tracer_mean (i, l) + cst%tracer_step1 (i, j, l)
                 cst%tracerm_mean(i, l) = cst%tracerm_mean(i, l) + cst%tracerm_step1(i, j, l)
             END DO
         END DO
     END DO
     
     DO i = 1, cst%ARRAY_TOT_SIZ4
         cst%t_mean (i) = cst%t_mean (i) / FLOAT(tb - 1)
         cst%tm_mean(i) = cst%tm_mean(i) / FLOAT(tb - 1)
         cst%u_mean (i) = cst%u_mean (i) / FLOAT(tb - 1)
         cst%um_mean(i) = cst%um_mean(i) / FLOAT(tb - 1)
         cst%v_mean (i) = cst%v_mean (i) / FLOAT(tb - 1)
         cst%vm_mean(i) = cst%vm_mean(i) / FLOAT(tb - 1)
     END DO

     DO l = 1, cst%ntrac
         DO i = 1, cst%ARRAY_TOT_SIZ4
             cst%tracer_mean (i, l) = cst%tracer_mean (i, l) / FLOAT(tb - 1)
             cst%tracerm_mean(i, l) = cst%tracerm_mean(i, l) / FLOAT(tb - 1)
         END DO
     END DO
     
! subtract the average from the perturbations.
!---------------------------------------------
     DO j = 1, tb - 1  
         DO i = 1, cst%ARRAY_TOT_SIZ4
             cst%t_step1 (i, j) = cst%t_step1 (i, j) - cst%t_mean (i)
             cst%tm_step1(i, j) = cst%tm_step1(i, j) - cst%tm_mean(i)
             cst%u_step1 (i, j) = cst%u_step1 (i, j) - cst%u_mean (i)
             cst%um_step1(i, j) = cst%um_step1(i, j) - cst%um_mean(i)
             cst%v_step1 (i, j) = cst%v_step1 (i, j) - cst%v_mean (i)
             cst%vm_step1(i, j) = cst%vm_step1(i, j) - cst%vm_mean(i)
         END DO
     END  DO

     DO l = 1, cst%ntrac
         DO j = 1, tb - 1 
             DO i = 1, cst%ARRAY_TOT_SIZ4
                 cst%tracer_step1 (i, j, l) = cst%tracer_step1 (i, j, l) - cst%tracer_mean (i, l)
                 cst%tracerm_step1(i, j, l) = cst%tracerm_step1(i, j, l) - cst%tracerm_mean(i, l)
             END DO
         END DO
     END DO
 END IF

 END SUBROUTINE ENS_Sto_Per_Scheme_Step1





 SUBROUTINE ENS_Sto_Per_Scheme_Step1_2(impENS, cst, rc)

! This subroutine is used to compute the first step of the 
! stochastic perturbation scheme, in which X_i_dot = T_i + S_i 
! and S_i ~ SUM(W_i,j P_j.
!-------------------------------------------------------------

! In the current code, assume each ensemble member uses the same 
! number of the processors.
!---------------------------------------------------------------

 USE ESMF
 USE ENS_Cpl_InternalState_ESMFMod
 USE Lib_ESMFStateAddGetMod

 IMPLICIT none

 TYPE(ESMF_State),             INTENT(inout) :: impENS
 TYPE(ENS_Cpl_InternalState),  INTENT(inout) :: cst
 INTEGER,                      INTENT(out)   :: rc

! !WORKING ARRAYS AND LOCAL PARAMETERS.
!--------------------------------------
 TYPE(ESMF_Field)                            :: Field
 TYPE(ESMF_FieldBundle)                      :: Bundle
 INTEGER                                     :: i, j, k, l, i1, j1
 INTEGER                                     :: rc1

 rc1 = ESMF_SUCCESS
 rc  = ESMF_SUCCESS

 CALL GetF90ArrayFromState(impENS, 'pps', cst%work1, 0, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get From impENS to the Cpl Internal State -- PS.")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting From impENS to the Cpl Internal State -- PS, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 DO j = 1, cst%arraysize_2
     DO i = 1, cst%arraysize_1
         cst%psw(i, j) = cst%work1(i, j) + cst%RS_GLOBAL * cst%psw(i, j)
     END DO
 END DO

 CALL GetF90ArrayFromState(impENS, 'psm', cst%work1, 0, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get From impENS to the Cpl Internal State -- PS(-DT).")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting From impENS to the Cpl Internal State -- PS(-DT), rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 DO j = 1, cst%arraysize_2
     DO i = 1, cst%arraysize_1
         cst%pswm(i, j) = cst%work1(i, j) + cst%RS_GLOBAL * cst%pswm(i, j)
     END DO
 END DO

 CALL GetF90ArrayFromState(impENS, 'tt', cst%work2, 0, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get From impENS to the Cpl Internal State -- T.")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting From impENS to the Cpl Internal State -- T, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 DO k = 1, cst%arraysize_3
     DO j = 1, cst%arraysize_2
         DO i = 1, cst%arraysize_1
             cst%tw(i, j, k) = cst%work2(i, j, k) + cst%RS_GLOBAL * cst%tw(i, j, k)
         END DO
     END DO
 END DO

 CALL GetF90ArrayFromState(impENS, 'tm', cst%work2, 0, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get From impENS to the Cpl Internal State -- T(-DT).")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting From impENS to the Cpl Internal State -- T(-DT), rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 DO k = 1, cst%arraysize_3
     DO j = 1, cst%arraysize_2
         DO i = 1, cst%arraysize_1
             cst%twm(i, j, k) = cst%work2(i, j, k) + cst%RS_GLOBAL * cst%twm(i, j, k)
         END DO
     END DO
 END DO

 CALL GetF90ArrayFromState(impENS, 'uu', cst%work2, 0, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get From impENS to the Cpl Internal State -- U.")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting From impENS to the Cpl Internal State -- U, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 DO k = 1, cst%arraysize_3
     DO j = 1, cst%arraysize_2
         DO i = 1, cst%arraysize_1
             cst%uw(i, j, k) = cst%work2(i, j, k) + cst%RS_GLOBAL * cst%uw(i, j, k)
         END DO
     END DO
 END DO

 CALL GetF90ArrayFromState(impENS, 'um', cst%work2, 0, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get From impENS to the Cpl Internal State -- U(-DT).")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting From impENS to the Cpl Internal State -- U(-DT), rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 DO k = 1, cst%arraysize_3
     DO j = 1, cst%arraysize_2
         DO i = 1, cst%arraysize_1
             cst%uwm(i, j, k) = cst%work2(i, j, k) + cst%RS_GLOBAL * cst%uwm(i, j, k)
         END DO
     END DO
 END DO

 CALL GetF90ArrayFromState(impENS, 'vv', cst%work2, 0, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get From impENS to the Cpl Internal State -- V.")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting From impENS to the Cpl Internal State -- V, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 DO k = 1, cst%arraysize_3
     DO j = 1, cst%arraysize_2
         DO i = 1, cst%arraysize_1
             cst%vw(i, j, k) = cst%work2(i, j, k) + cst%RS_GLOBAL * cst%vw(i, j, k)
         END DO
     END DO
 END DO

 CALL GetF90ArrayFromState(impENS, 'vm', cst%work2, 0, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get From impENS to the Cpl Internal State -- V(-DT).")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting From impENS to the Cpl Internal State -- V(-DT), rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 DO k = 1, cst%arraysize_3
     DO j = 1, cst%arraysize_2
         DO i = 1, cst%arraysize_1
             cst%vwm(i, j, k) = cst%work2(i, j, k) + cst%RS_GLOBAL * cst%vwm(i, j, k)
         END DO
     END DO
 END DO

 CALL ESMF_StateGet(impENS, 'tracers', Bundle, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Retrieve Bundle from impENS in ENS Cpl.")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Retrieving Bundle from impENS in ENS Cpl, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 DO j1 = 2, 3
     DO i1 = 1, cst%ntrac
         CALL ESMF_FieldBundleGet(Bundle,                    &
                                  cst%vname(i1, j1),         &
                                  field = Field,             &
                                  rc = rc1)

         IF(ESMF_LogFoundError(rc1, msg="Retrieve Field from Bundle in ENS Cpl.")) THEN
             rc = ESMF_FAILURE
             PRINT*, 'Error Happened When Retrieving Field from Bundle in ENS Cpl, rc = ', rc1
             rc1 = ESMF_SUCCESS
         END IF

         NULLIFY(cst%work2)
         CALL ESMF_FieldGet(Field, farrayPtr = cst%work2, localDE = 0, rc = rc1)
         IF(ESMF_LogFoundError(rc1, msg="Retrieve FArray from Field in ENS Cpl.")) THEN
             rc = ESMF_FAILURE
             PRINT*, 'Error Happened When Retrieving FArray from Field in ENS Cpl, rc = ', rc1
             rc1 = ESMF_SUCCESS
         END IF

         SELECT CASE(j1)
             CASE(2)
                 DO k = 1, cst%arraysize_3
                     DO j = 1, cst%arraysize_2
                         DO i = 1, cst%arraysize_1
                             cst%tracerw (i, j, k, i1) = cst%work2(i, j, k) + cst%RS_GLOBAL * cst%tracerw (i, j, k, i1)
                         END DO
                     END DO
                 END DO
             CASE(3)
                 DO k = 1, cst%arraysize_3
                     DO j = 1, cst%arraysize_2
                         DO i = 1, cst%arraysize_1
                             cst%tracerwm(i, j, k, i1) = cst%work2(i, j, k) + cst%RS_GLOBAL * cst%tracerwm(i, j, k, i1)
                         END DO
                     END DO
                 END DO
         END SELECT

         IF(ESMF_LogFoundError(rc1, msg="Distribute for the Step1 Scheme -- tracer.")) THEN
             rc = ESMF_FAILURE
             PRINT*, 'Error Happened When Distributing for the Step1 Scheme -- tracer, rc = ', rc1
             rc1 = ESMF_SUCCESS
         END IF
     END DO
 END DO

 IF(rc == ESMF_SUCCESS) THEN
     PRINT*, "PASS: ENS_Sto_Per_Scheme_Step1_2"
 ELSE
     PRINT*, "FAIL: ENS_Sto_Per_Scheme_Step1_2"
 END IF

 END SUBROUTINE ENS_Sto_Per_Scheme_Step1_2
