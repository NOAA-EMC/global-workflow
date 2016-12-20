!
! !MODULE: GEFS_CplState_ESMFMod --- Run module of the ESMF grided
!                                    component of the GFS ensemble coupler.
!
! !DESCRIPTION: GEFS coupler run module.
!
! !REVISION HISTORY:
!
!  April    2006     Weiyu Yang Initial code.
!  May      2008     Weiyu Yang updated to use the ESMF 3.1.0r library.
!
!
! !INTERFACE:
!
 MODULE GEFS_CplState_ESMFMod
!
!!USES:
!
 USE ESMF_Mod
 USE GEFS_Cpl_InternalState_ESMFMod
 USE Lib_ESMFStateAddGetMod
 use mpi_def, only : mpi_real8

!INCLUDE 'mpif.h'

 IMPLICIT none

 CONTAINS

 SUBROUTINE GEFS_GetTrieoSize(CplGEFS, impGEFS, expGEFS, Cpl_Int_State, rc)

!
! !INPUT VARIABLES AND PARAMETERS:
!---------------------------------
 TYPE(ESMF_CplComp),           INTENT(inout)    :: CplGEFS
 TYPE(ESMF_State),             INTENT(in)       :: impGEFS
 TYPE(ESMF_State),             INTENT(inout)    :: expGEFS
 TYPE(GEFS_Cpl_InternalState), INTENT(inout)    :: Cpl_Int_State

! !OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------
 INTEGER, OPTIONAL,            INTENT(out)      :: rc

!
! !WORKING ARRAYS AND LOCAL PARAMETERS.
!--------------------------------------
 TYPE(ESMF_VM)                                  :: vm
 TYPE(ESMF_Array)                               :: ESMFArray
 TYPE(ESMF_DistGrid)                            :: DistGrid6    ! the ESMF DISTGRID TYPE.
 TYPE(ESMF_Grid)                                :: grid6        ! the ESMF GRID TYPE.
 INTEGER                                        :: i, k1, l1
 INTEGER,              DIMENSION(:),    POINTER :: counts
 INTEGER                                        :: rc1
 INTEGER                                        :: rcfinal

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS
 ALLOCATE(counts(1))

 CALL ESMF_VMGetCurrent(vm, rc = rc1)

 WRITE(Cpl_Int_State%TRIEO_STATE_NAME, 1000)
 WRITE(Cpl_Int_State%TRIEO_STINI_NAME, 2000)
1000 FORMAT('TRIEO_ESMF_STATE')
2000 FORMAT('TRIEO_ESMF_STINI')

 CALL ESMF_StateGet(impGEFS, Cpl_Int_State%TRIEO_STATE_NAME, &
                         ESMFArray, rc = rc1)

 CALL ESMF_ArrayGet(ESMFArray, distgrid = DistGrid6, rc = rc1)

 grid6 = ESMF_GridCreate(name = "Cpl TRIEO Grid", distgrid = DistGrid6, rc = rc1)

 IF(ESMF_LogMsgFoundError(rc1, "Get ESMF Grid6")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Getting the ESMF Grid6, rc = ', rc1
     rc1 = ESMF_SUCCESS
 END IF

 CALL ESMF_ArrayGet(ESMFArray, 0, Cpl_Int_State%trieo_work, rc = rc1)

 counts(1) = SIZE(Cpl_Int_State%trieo_work(:, 1))

 CALL ESMF_VMAllGather  (vm,                             &
                         counts,                         &
                         Cpl_Int_State%TRIEO_ST_SIZE,    &
                         1,                              &
                         rc     = rc1)
 Cpl_Int_State%TRIEO_LS_SIZE = Cpl_Int_State%TRIEO_ST_SIZE - 3

 Cpl_Int_State%TRIE_LS_SIZE        = NINT(Cpl_Int_State%trieo_work(1, 1))
 Cpl_Int_State%TRIO_LS_SIZE        = NINT(Cpl_Int_State%trieo_work(2, 1))
 Cpl_Int_State%TRIEO_VERTICAL_SIZE = NINT(Cpl_Int_State%trieo_work(3, 1))
 NULLIFY(Cpl_Int_State%trieo_work)

 Cpl_Int_State%TRIEO_MAX_SIZE = 0
 DO i = 1, Cpl_Int_State%nodes
     IF( Cpl_Int_State%TRIEO_MAX_SIZE < Cpl_Int_State%TRIEO_LS_SIZE(i)) THEN
         Cpl_Int_State%TRIEO_MAX_SIZE = Cpl_Int_State%TRIEO_LS_SIZE(i)
     END IF
 END DO

 k1 = Cpl_Int_State%TRIEO_MAX_SIZE * Cpl_Int_State%TRIEO_VERTICAL_SIZE * 2

 Cpl_Int_State%TRIEO_LSTOT_SIZ2 = k1 / Cpl_Int_State%nodes
 IF(MOD(k1, Cpl_Int_State%nodes) /= 0)  &
     Cpl_Int_State%TRIEO_LSTOT_SIZ2 = Cpl_Int_State%TRIEO_LSTOT_SIZ2 + 1

 Cpl_Int_State%TRIEO_LSTOT_SIZ3 = Cpl_Int_State%TRIEO_LSTOT_SIZ2 * Cpl_Int_State%nodes

! Assume that each ensemble member uses the same nember of the processors.
!-------------------------------------------------------------------------
 Cpl_Int_State%TRIEO_LSTOT_SIZ4 = Cpl_Int_State%TRIEO_LSTOT_SIZ3 / Cpl_Int_State%Total_member

 DEALLOCATE(counts)

 PRINT*, 'in GEFS_GetTrieoSize, TRIEO_VERTICAL_SIZE=', Cpl_Int_State%TRIEO_VERTICAL_SIZE
 PRINT*, 'in GEFS_GetTrieoSize, TRIEO_MAX_SIZE=',      Cpl_Int_State%TRIEO_MAX_SIZE
 PRINT*, 'in GEFS_GetTrieoSize, TRIE_LS_SIZE=',        Cpl_Int_State%TRIE_LS_SIZE
 PRINT*, 'in GEFS_GetTrieoSize, TRIO_LS_SIZE=',        Cpl_Int_State%TRIO_LS_SIZE
 PRINT*, 'in GEFS_GetTrieoSize, TRIEO_LSTOT_SIZ2=',    Cpl_Int_State%TRIEO_LSTOT_SIZ2
 PRINT*, 'in GEFS_GetTrieoSize, TRIEO_LSTOT_SIZ3=',    Cpl_Int_State%TRIEO_LSTOT_SIZ3
  
 IF(ESMF_LogMsgFoundError(rc1, "In GEFS_GetTrieoSize")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When being in GEFS_GetTrieoSize, rc = ', rc1
     rc1 = ESMF_SUCCESS
 END IF

! Prepare for creating the ESMF export state in the run routine.
!---------------------------------------------------------------
 ALLOCATE(Cpl_Int_State%trieo_ls_write(Cpl_Int_State%TRIEO_ST_SIZE(Cpl_Int_State%mm1), &
                                       Cpl_Int_State%TRIEO_VERTICAL_SIZE*2))

 CALL AddF90ArrayToState(expGEFS, grid6, Cpl_Int_State%TRIEO_STATE_NAME,  &
                                         Cpl_Int_State%trieo_ls_write,    &
                                         rc = rc1)

 IF(ESMF_LogMsgFoundError(rc1, "Create Cpl ESMF State - TRIEO_LS_ST")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Creating ESMF State - TRIEO_LS_ST, rc = ', rc1
 END IF

 IF(PRESENT(rc)) THEN
     rc = rcfinal
 END IF

 END SUBROUTINE GEFS_GetTrieoSize





 SUBROUTINE GEFS_Cpl_ESMFImportState2InternalState(CplGEFS, impGEFS, Cpl_Int_State, rc)

! !INPUT VARIABLES AND PARAMETERS:
!---------------------------------
 TYPE(ESMF_CplComp),           INTENT(inout) :: CplGEFS
 TYPE(ESMF_State),             INTENT(in)    :: impGEFS
 TYPE(GEFS_Cpl_InternalState), INTENT(inout) :: Cpl_Int_State

! !OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------
 INTEGER, OPTIONAL,            INTENT(out)   :: rc

! !WORKING ARRAYS AND LOCAL PARAMETERS.
!--------------------------------------
 TYPE(ESMF_VM)                               :: vm
 INTEGER                                     :: i, j, k, i1, l
 INTEGER                                     :: rc1
 INTEGER                                     :: rcfinal

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

 CALL GetF90ArrayFromState(impGEFS, Cpl_Int_State%TRIEO_STINI_NAME, &
                                    Cpl_Int_State%trieo_work,       &
                                    0,                              &
                                    rc            = rc1)
 k  = 1
 i1 = 0
 DO l = 1, Cpl_Int_State%TRIEO_VERTICAL_SIZE
     DO j = 1, 2
         i1 = i1 + 1
         DO i = 4, Cpl_Int_State%TRIEO_LS_SIZE(Cpl_Int_State%mm1) + 3
             Cpl_Int_State%trieo_ls_max(k) = Cpl_Int_State%trieo_work(i, i1)
             k = k + 1
         END DO
     END DO
 END DO

! Set all remaining array elements to be not NaNQ.
!-------------------------------------------------
 DO j = k, Cpl_Int_State%TRIEO_LSTOT_SIZ3
     Cpl_Int_State%trieo_ls_max(j) = 1.1
 END DO

 CALL mpi_alltoall(Cpl_Int_State%trieo_ls_max, Cpl_Int_State%TRIEO_LSTOT_SIZ2, MPI_REAL8, &
                   Cpl_Int_State%trieo_ls_ini, Cpl_Int_State%TRIEO_LSTOT_SIZ2, MPI_REAL8, &
                   Cpl_Int_State%mpi_comm_cplcomp, rc1)

 IF(ESMF_LogMsgFoundError(rc1, "Get ESMF State - TRIEO_LS_ST_INI")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Getting ESMF State - TRIEO_LS_ST_INI, rc = ', rc1
 END IF

 CALL GetF90ArrayFromState(impGEFS, Cpl_Int_State%TRIEO_STATE_NAME, &
                                    Cpl_Int_State%trieo_work,       &
                                    0,                              &
                                    rc            = rc1)
 k  = 1
 i1 = 0
 DO l = 1, Cpl_Int_State%TRIEO_VERTICAL_SIZE
     DO j = 1, 2
         i1 = i1 + 1
         DO i = 4, Cpl_Int_State%TRIEO_LS_SIZE(Cpl_Int_State%mm1) + 3
             Cpl_Int_State%trieo_ls_max(k) = Cpl_Int_State%trieo_work(i, i1)
             k = k + 1
         END DO
     END DO
 END DO
 NULLIFY(Cpl_Int_State%trieo_work)

! Set all remaining array elements to be not NaNQ.
!-------------------------------------------------
 DO j = k, Cpl_Int_State%TRIEO_LSTOT_SIZ3
     Cpl_Int_State%trieo_ls_max(j) = 1.1
 END DO

 CALL mpi_alltoall(Cpl_Int_State%trieo_ls_max, Cpl_Int_State%TRIEO_LSTOT_SIZ2, MPI_REAL8, &
                   Cpl_Int_State%trieo_ls,     Cpl_Int_State%TRIEO_LSTOT_SIZ2, MPI_REAL8, &
                   Cpl_Int_State%mpi_comm_cplcomp, rc1)

 IF(ESMF_LogMsgFoundError(rc1, "Gete ESMF State - TRIEO_LS_ST")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Getting ESMF State - TRIEO_LS_ST, rc = ', rc1
     rc1 = ESMF_SUCCESS
 END IF

 IF(rcfinal /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: CGEFS_Cpl_ESMFImportState2InternalState."
!ELSE
!    PRINT*, "PASS: GEFS_Cpl_ESMFImportState2InternalState."
 END IF

 IF(PRESENT(rc)) THEN
     rc = rcfinal
 END IF

 END SUBROUTINE GEFS_Cpl_ESMFImportState2InternalState





 SUBROUTINE GEFS_Cpl_InternalState2ESMFExportState(Cpl_Int_State)

! !INPUT VARIABLES AND PARAMETERS:
!---------------------------------
 TYPE(GEFS_Cpl_InternalState), INTENT(inout) :: Cpl_Int_State

! !WORKING ARRAYS AND LOCAL PARAMETERS.
!--------------------------------------
 INTEGER                                     :: i, j, l, i1, l1
 
 l1   = 1
 i1 = 0
 DO l = 1, Cpl_Int_State%TRIEO_VERTICAL_SIZE
     DO j = 1, 2
         i1 = i1 + 1
         Cpl_Int_State%trieo_ls_write(1, i1) = FLOAT(Cpl_Int_State%TRIE_LS_SIZE)
         Cpl_Int_State%trieo_ls_write(2, i1) = FLOAT(Cpl_Int_State%TRIO_LS_SIZE)
         Cpl_Int_State%trieo_ls_write(3, i1) = FLOAT(Cpl_Int_State%TRIEO_VERTICAL_SIZE)

         DO i = 4, Cpl_Int_State%TRIEO_LS_SIZE(Cpl_Int_State%mm1) + 3
             Cpl_Int_State%trieo_ls_write(i, i1) = Cpl_Int_State%trieo_ls_max(l1)
             l1 = l1 + 1
         END DO
     END DO
 END DO

 END SUBROUTINE GEFS_Cpl_InternalState2ESMFExportState

 END MODULE GEFS_CplState_ESMFMod
