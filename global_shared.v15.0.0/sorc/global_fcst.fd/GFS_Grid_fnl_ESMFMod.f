
! !MODULE: GFS_GridComp_ESMFMod --- ESMF gridded component of the GFS system. 
!
! !DESCRIPTION: GFS gridded component main module.
!
! !REVISION HISTORY:
!
!  November 2004     Weiyu Yang Initial code.
!  May      2005     Weiyu Yang For the updated GFS version.
!                           
!
! !INTERFACE:
!
 MODULE GFS_Grid_fnl_ESMFMod
 
!!USES:
!------

! Define the ESMF internal state and all routines related to run 
! the GFS grid component.
!---------------------------------------------------------------
 USE GFS_Initialize_ESMFMod
 USE GFS_Run_ESMFMod
 USE GFS_Finalize_ESMFMod

 IMPLICIT none

!#include "ESMF_LogMacros.inc"


 PRIVATE   ! By default data is private to this module
!
! !PUBLIC TYPES:
!---------------

 PUBLIC Finalize

!EOP
!-------------------------------------------------------------------------


 CONTAINS



!----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Finalize --- finalizing routine to finish the 
!                        GFS running job.
!
! !DESCRIPTION: This subroutine will finish the GFS computations,
! !             and will release the memory space.
!
! !REVISION HISTORY:
!
!  November 2004     Weiyu Yang Initial code.
!  May      2005     Weiyu Yang For the updated GFS version.
!
! !INTERFACE:

 SUBROUTINE Finalize(gcGFS, impGFS, expGFS, clock, rc)

!
! !INPUT VARIABLES AND PARAMETERS:
!---------------------------------
 TYPE(ESMF_GridComp), INTENT(inout)  :: gcGFS
 TYPE(ESMF_State),    INTENT(inout)  :: impGFS
 TYPE(ESMF_State),    INTENT(inout)  :: expGFS
 TYPE(ESMF_Clock),    INTENT(inout)  :: clock

! !OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------
 INTEGER,    INTENT(out), OPTIONAL   :: rc

! !WORKING ARRAYS AND LOCAL PARAMETERS.
!--------------------------------------
 TYPE(GFS_wrap)                      :: wrap         ! This wrap is a derived type which contains
                                                   ! only a pointer to the internal state.  It is needed
                                                   ! for using different architectures or compliers.
 TYPE(GFS_InternalState), POINTER    :: Int_State    ! the internal state pointer.
 INTEGER                             :: rc1          ! error signal variable.
 INTEGER                             :: rcfinal      ! the final error signal variable.

!EOP
!-------------------------------------------------------------------------

! Initialize the error signal variables.
!---------------------------------------
 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

     CALL ESMF_LogWrite("Get the Internal State in the Finalize Routine", &
                        ESMF_LOG_INFO, rc = rc1)

! Retrieve the ESMF internal state.
!----------------------------------
 CALL ESMF_GridCompGetInternalState(gcGFS, wrap, rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Get the Internal State in the Finalize Routine")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting the Internal State in FInalize Routine, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! pointing the local internal state pointer to the ESMF internal state pointer.
!------------------------------------------------------------------------------
 Int_State => wrap%Int_State

     CALL ESMF_LogWrite("Run the GFS_Finalize", ESMF_LOG_INFO, rc = rc1)

! User code to run the GFS Finalize routine to release the memory space, etc. 
!----------------------------------------------------------------------------
 CALL GFS_Finalize(gcGFS, Int_State, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Run the GFS_Finalize")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Running the GFS_Finalize, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     CALL ESMF_LogWrite("Destroy the ESMF Clock", ESMF_LOG_INFO, rc = rc1)

! Destroy the ESMF clock.
!------------------------
 CALL ESMF_ClockDestroy(clock, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Destroy the ESMF Clock")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Destroying the ESMF Clock, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Print out the final error signal information and put it to rc.
!---------------------------------------------------------------
 IF(rcfinal /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: Finalize."
!ELSE
!    PRINT*, "PASS: Finalize."
 END IF

 IF(PRESENT(rc)) THEN
     rc = rcfinal
 END IF

 END SUBROUTINE Finalize

! End of the GFS ESMF grid_fnl component module.
!-------------------------------------------
 END MODULE GFS_Grid_fnl_ESMFMod
