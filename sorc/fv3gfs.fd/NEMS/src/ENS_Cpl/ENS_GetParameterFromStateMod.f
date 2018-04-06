#include "../ESMFVersionDefine.h"

!----------------------------------------------------------------------
! !MODULE: ENS_GetParameterFromStateMod
!        --- Get required parameters from the ENS Coupler ESMF import state
!            for the ensemble coupler to do the spectral transform
!            for the stochastic perturbation scheme, the second step.
!
! !DESCRIPTION: Get all required parameters from the ENS Cpl ESMF import state.
!
! !REVISION HISTORY:
!
!  May      2007     Weiyu Yang Initial code.
!  March    2009     Weiyu Yang Modified for the NEMS model.
!  February 2011     Weiyu Yang Updated to use both the ESMF 4.0.0rp2 library,
!                               ESMF 5 library and the the ESMF 3.1.0rp2 library.
!  May      2011     Weiyu yang Modified for using the ESMF 5.2.0r_beta_snapshot_07.
!  September2011     Weiyu yang Modified for using the ESMF 5.2.0r library.
!
!
! !INTERFACE:
!

 MODULE ENS_GetParameterFromStateMod

 USE ESMF
 USE ENS_Cpl_InternalState_ESMFMod

 IMPLICIT none

 CONTAINS

 SUBROUTINE ENS_GetParameterFromState(State, Int_State, rc)

 TYPE(ESMF_State),                     INTENT(inout) :: State
 TYPE(ENS_Cpl_InternalState), POINTER, INTENT(inout) :: Int_State
 INTEGER, OPTIONAL,                    INTENT(out)   :: rc

 INTEGER                                             :: rc1, rcfinal

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

! One by one get the parameters from the EARTH ESMF export state.
!----------------------------------------------------------------
 CALL ESMF_AttributeGet(State, 'NTRAC', Int_State%ntrac, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get ntrac from the ENS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting ntrac from the ENS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'MPI_R_MPI_R', Int_State%MPI_R_MPI_R, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get MPI_R_MPI_R from the ENS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting MPI_R_MPI_R from the ENS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'JCAP', Int_State%jcap, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get JCAP from the ENS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting JCAP from the ENS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'NODES_COMP', Int_State%nodes_comp, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get NODES_COMP from the ENS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting NODES_COMP from the ENS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'ME_COMP', Int_State%me_comp, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get ME_COMP from the ENS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting ME_COMP from the ENS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'MC_COMP', Int_State%MC_COMP, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get MC_COMP from the ENS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting MC_COMP from the ENS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LATS_NODE_A', Int_State%lats_node_a, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get LATS_NODE_A from the ENS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LATS_NODE_A from the ENS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'IPT_LATS_NODE_A', Int_State%ipt_lats_node_a, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get IPT_LATS_NODE_A from the ENS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting IPT_LATS_NODE_A from the ENS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LONF', Int_State%lonf, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get LONF from the ENS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LONF from the ENS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 CALL ESMF_AttributeGet(State, 'LATG', Int_State%latg, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get LATG from the ENS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LATG from the ENS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 NULLIFY (Int_State%global_lats_a)
 NULLIFY (Int_State%lonsperlat   )
 ALLOCATE(Int_State%global_lats_a(Int_State%latg))
 ALLOCATE(Int_State%lonsperlat   (Int_State%latg))

 CALL ESMF_AttributeGet(State, 'GLOBAL_LATS_A', Int_State%global_lats_a, &
     itemCount = Int_State%latg, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get GLOBAL_LATS_A from the ENS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting GLOBAL_LATS_A from the ENS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF
 CALL ESMF_AttributeGet(State, 'LONSPERLAT', Int_State%lonsperlat, &
     itemCount = Int_State%latg, rc = rc1)

     IF(ESMF_LogFoundError(rc1, msg="Get LONSPERLAT from the ENS Cpl import state.")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting LONSPERLAT from the ENS Cpl import state, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(rcfinal == ESMF_SUCCESS) THEN
     PRINT*, "PASS: ENS_GetParameterFromStateMod.f"
 ELSE
     PRINT*, "FAIL: ENS_GetParameterFromStateMod.f"
 END IF

 IF(PRESENT(rc)) THEN
     rc = rcfinal
 END IF

 END SUBROUTINE ENS_GetParameterFromState

 END MODULE ENS_GetParameterFromStateMod

