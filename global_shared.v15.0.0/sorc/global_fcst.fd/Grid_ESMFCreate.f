! Modified for using the ESMF 3.1.0r new grid interface. 06/06/2008.
! Weiyu Yang.
!-------------------------------------------------------------------

 SUBROUTINE DistGrid_ESMFCreate1(vm, Int_State, DistGrid1, rc)

!
!!USES:
!
 USE ESMF_Mod                 ! The ESMF library.
 USE GFS_InternalState_ESMFMod

 IMPLICIT none

 TYPE(ESMF_VM),           INTENT(inout) :: vm           ! the ESMF virtual machine.
 TYPE(ESMF_DistGrid),     INTENT(inout) :: DistGrid1    ! the ESMF DistGrid
 INTEGER, INTENT(out)                   :: rc
 TYPE(GFS_InternalState), INTENT(inout) :: Int_State

 INTEGER                           :: rc1
 INTEGER,            DIMENSION(2)  :: min, max

 rc  = ESMF_SUCCESS
 rc1 = ESMF_SUCCESS

 min(1) = 1
 min(2) = 1
 max(1) = (Int_State%jcap+1)*(Int_State%jcap+2)
 max(2) = 1

! Create the ESMF DistGrid1 using the 1-D default decomposition.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Create DistGrid1", ESMF_LOG_INFO, rc = rc1)

 DistGrid1 = ESMF_DistGridCreate(minIndex = min, maxIndex = max, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid1")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid1, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(rc /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: DistGrid_ESMFCreate1."
!ELSE
!    PRINT*, "PASS: DistGrid_ESMFCreate1."
 END IF

 END SUBROUTINE DistGrid_ESMFCreate1





 SUBROUTINE Grid_ESMFCreate1(vm, grid1, Int_State, DistGrid1, rc)

!
!!USES:
!
 USE ESMF_Mod                 ! The ESMF library.
 USE GFS_InternalState_ESMFMod

 IMPLICIT none

 TYPE(ESMF_VM),           INTENT(inout) :: vm           ! the ESMF virtual machine.
 TYPE(ESMF_Grid),         INTENT(inout) :: grid1        ! the ESMF GRID TYPE.
 TYPE(ESMF_DistGrid),     INTENT(inout) :: DistGrid1    ! the ESMF DistGrid
 INTEGER, INTENT(out)                   :: rc
 TYPE(GFS_InternalState), INTENT(inout) :: Int_State

 INTEGER                           :: rc1
 INTEGER,            DIMENSION(2)  :: min, max

 rc  = ESMF_SUCCESS
 rc1 = ESMF_SUCCESS

! Create grid.
!-------------
 min(1) = 1
 min(2) = 1
 max(1) = (Int_State%jcap+1)*(Int_State%jcap+2)
 max(2) = 1

! Create the ESMF DistGrid1 using the 1-D default decomposition.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Create DistGrid1", ESMF_LOG_INFO, rc = rc1)

 DistGrid1 = ESMF_DistGridCreate(minIndex = min, maxIndex = max, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid1")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid1, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Create the ESMF grid1 based on the created ESMF DistGrid1 information.
!-----------------------------------------------------------------------
     CALL ESMF_LogWrite("Create Grid1", ESMF_LOG_INFO, rc = rc1)

 grid1 = ESMF_GridCreate(name = "GFS grid1", distgrid = DistGrid1, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create Grid1")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating Grid1, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(rc == ESMF_SUCCESS) THEN
     PRINT*, "PASS: Grid_ESMFCreate1."
 ELSE
     PRINT*, "FAIL: Grid_ESMFCreate1."
 END IF

 END SUBROUTINE Grid_ESMFCreate1





 SUBROUTINE Grid_ESMFCreate2(vm, grid2, Int_State, DistGrid2, rc)

!
!!USES:
!
 USE ESMF_Mod                 ! The ESMF library.
 USE GFS_InternalState_ESMFMod

 IMPLICIT none

 TYPE(ESMF_VM),           INTENT(inout) :: vm           ! the ESMF virtual machine.
 TYPE(ESMF_Grid),         INTENT(inout) :: grid2        ! the ESMF GRID TYPE.
 TYPE(ESMF_DistGrid),     INTENT(inout) :: DistGrid2    ! the ESMF DistGrid
 INTEGER, INTENT(out)                   :: rc
 TYPE(GFS_InternalState), INTENT(inout) :: Int_State

 INTEGER                           :: rc1
 INTEGER,            DIMENSION(2)  :: min, max

 rc  = ESMF_SUCCESS
 rc1 = ESMF_SUCCESS

! Create grid.
!-------------
 min(1) = 1
 min(2) = 1
 max(1) = (Int_State%jcap+1)*(Int_State%jcap+2)
 max(2) = Int_State%levs

! Create the ESMF DistGrid2 using the 1-D default decomposition.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Create DistGrid2", ESMF_LOG_INFO, rc = rc1)

 DistGrid2 = ESMF_DistGridCreate(minIndex = min, maxIndex = max, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid2")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid2, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Create the ESMF grid2 based on the created ESMF DistGrid2 information.
!-----------------------------------------------------------------------
     CALL ESMF_LogWrite("Create Grid2", ESMF_LOG_INFO, rc = rc1)

 grid2 = ESMF_GridCreate(name = "GFS grid2", distgrid = DistGrid2, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create Grid2")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating Grid2, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(rc == ESMF_SUCCESS) THEN
     PRINT*, "PASS: Grid_ESMFCreate2."
 ELSE
     PRINT*, "FAIL: Grid_ESMFCreate2."
 END IF

 END SUBROUTINE Grid_ESMFCreate2





 SUBROUTINE DistGrid_ESMFCreate3(vm, Int_State, DistGrid3, rc)

!
!!USES:
!
 USE ESMF_Mod                 ! The ESMF library.
 USE GFS_InternalState_ESMFMod

 IMPLICIT none

 TYPE(ESMF_VM),           INTENT(inout) :: vm           ! the ESMF virtual machine.
 TYPE(ESMF_DistGrid),     INTENT(inout) :: DistGrid3    ! the ESMF DistGrid
 INTEGER, INTENT(out)                   :: rc
 TYPE(GFS_InternalState), INTENT(inout) :: Int_State

 INTEGER                           :: rc1
 INTEGER,            DIMENSION(2)  :: min, max

 rc  = ESMF_SUCCESS
 rc1 = ESMF_SUCCESS

 min(1) = 1
 min(2) = 1
 max(1) = Int_State%lonr
 max(2) = Int_State%latr

! Create the ESMF DistGrid3 using the 1-D default decomposition.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Create DistGrid3", ESMF_LOG_INFO, rc = rc1)

 DistGrid3 = ESMF_DistGridCreate(minIndex = min, maxIndex = max, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid3")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid3, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(rc /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: DistGrid_ESMFCreate3."
!ELSE
!    PRINT*, "PASS: DistGrid_ESMFCreate3."
 END IF

 END SUBROUTINE DistGrid_ESMFCreate3





 SUBROUTINE Grid_ESMFCreate3(vm, grid3, Int_State, DistGrid3, rc)

!
!!USES:
!
 USE ESMF_Mod                 ! The ESMF library.
 USE GFS_InternalState_ESMFMod

 IMPLICIT none

 TYPE(ESMF_VM),           INTENT(inout) :: vm           ! the ESMF virtual machine.
 TYPE(ESMF_Grid),         INTENT(inout) :: grid3        ! the ESMF GRID TYPE.
 TYPE(ESMF_DistGrid),     INTENT(inout) :: DistGrid3    ! the ESMF DistGrid
 INTEGER, INTENT(out)                   :: rc
 TYPE(GFS_InternalState), INTENT(inout) :: Int_State

 INTEGER                           :: rc1
 INTEGER,            DIMENSION(2)  :: min, max

 rc  = ESMF_SUCCESS
 rc1 = ESMF_SUCCESS

! Create grid.
!-------------
 min(1) = 1
 min(2) = 1
 max(1) = Int_State%lonr
 max(2) = Int_State%latr

! Create the ESMF DistGrid3 using the 1-D default decomposition.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Create DistGrid3", ESMF_LOG_INFO, rc = rc1)

 DistGrid3 = ESMF_DistGridCreate(minIndex = min, maxIndex = max, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid3")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid3, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Create the ESMF grid3 based on the created ESMF DistGrid3 information.
!-----------------------------------------------------------------------
     CALL ESMF_LogWrite("Create Grid3", ESMF_LOG_INFO, rc = rc1)

 grid3 = ESMF_GridCreate(name = "GFS grid3", distgrid = DistGrid3, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create Grid3")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating Grid3, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(rc == ESMF_SUCCESS) THEN
     PRINT*, "PASS: Grid_ESMFCreate3."
 ELSE
     PRINT*, "FAIL: Grid_ESMFCreate3."
 END IF

 END SUBROUTINE Grid_ESMFCreate3





 SUBROUTINE Grid_ESMFCreate4(vm, grid4, Int_State, DistGrid4, rc)

!
!!USES:
!
 USE ESMF_Mod                 ! The ESMF library.
 USE GFS_InternalState_ESMFMod

 IMPLICIT none

 TYPE(ESMF_VM),           INTENT(inout) :: vm           ! the ESMF virtual machine.
 TYPE(ESMF_Grid),         INTENT(inout) :: grid4        ! the ESMF GRID TYPE.
 TYPE(ESMF_DistGrid),     INTENT(inout) :: DistGrid4    ! the ESMF DistGrid
 INTEGER, INTENT(out)                   :: rc
 TYPE(GFS_InternalState), INTENT(inout) :: Int_State

 INTEGER                           :: rc1
 INTEGER,            DIMENSION(2)  :: min, max

 rc  = ESMF_SUCCESS
 rc1 = ESMF_SUCCESS

! Create grid.
!-------------
 min(1)    = 1
 min(2)    = 1
 max(1)    = Int_State%NODES
 max(2)    = 5

! Create the ESMF DistGrid4 using the 1-D default decomposition.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Create DistGrid4", ESMF_LOG_INFO, rc = rc1)

 DistGrid4 = ESMF_DistGridCreate(minIndex = min, maxIndex = max, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid4")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid4, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Create the ESMF grid4 based on the created ESMF DistGrid4 information.
!-----------------------------------------------------------------------
     CALL ESMF_LogWrite("Create Grid4", ESMF_LOG_INFO, rc = rc1)

 grid4 = ESMF_GridCreate(name = "GFS grid4", distgrid = DistGrid4, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create Grid4")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating Grid4, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(rc == ESMF_SUCCESS) THEN
     PRINT*, "PASS: Grid_ESMFCreate4."
 ELSE
     PRINT*, "FAIL: Grid_ESMFCreate4."
 END IF

 END SUBROUTINE Grid_ESMFCreate4





 SUBROUTINE Grid_ESMFCreate5(vm, grid5, Int_State, DistGrid5, rc)

!
!!USES:
!
 USE ESMF_Mod                 ! The ESMF library.
 USE GFS_InternalState_ESMFMod

 IMPLICIT none

 TYPE(ESMF_VM),           INTENT(inout) :: vm           ! the ESMF virtual machine.
 TYPE(ESMF_Grid),         INTENT(inout) :: grid5        ! the ESMF GRID TYPE.
 TYPE(ESMF_DistGrid),     INTENT(inout) :: DistGrid5    ! the ESMF DistGrid
 INTEGER, INTENT(out)                   :: rc
 TYPE(GFS_InternalState), INTENT(inout) :: Int_State

 INTEGER, DIMENSION(:), ALLOCATABLE :: IndexLocalList
 INTEGER                            :: i, j, k
 INTEGER                            :: rc1
 LOGICAL                            :: first

 DATA first/.true./

 SAVE IndexLocalList, first

 rc  = ESMF_SUCCESS
 rc1 = ESMF_SUCCESS

 IF(first) THEN
      ALLOCATE(IndexLocalList(Int_State%trieo_ls_size(Int_State%mm1)))
      first = .false.
 END IF

 k = 1
 DO j = 1, Int_State%NODES
     DO i = 1, Int_State%trieo_ls_size(j)
         IF(Int_State%mm1 == j) IndexLocalList(i) = k
         k = k + 1
     END DO
 END DO

! Create the ESMF DistGrid5.
!---------------------------
     CALL ESMF_LogWrite("Create DistGrid5", ESMF_LOG_INFO, rc = rc1)

 DistGrid5 = ESMF_DistGridCreate(IndexLocalList, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid5")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid5, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Create the ESMF grid5 based on the created ESMF DistGrid5 information.
!-----------------------------------------------------------------------
     CALL ESMF_LogWrite("Create Grid5", ESMF_LOG_INFO, rc = rc1)

 grid5 = ESMF_GridCreate(name = "GFS grid5", distgrid = DistGrid5, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create Grid5")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating Grid5, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(rc /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: Grid_ESMFCreate5."
!ELSE
!    PRINT*, "PASS: Grid_ESMFCreate5."
 END IF

 END SUBROUTINE Grid_ESMFCreate5
