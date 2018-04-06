#include "../ESMFVersionDefine.h"

!----------------------------------------------------------------------
! !MODULE: ENS_CplComp_ESMFMod
!        --- ESMF coupler gridded component of the EARTH Ensemble 
!            Forecast Operational system. 
!
! !DESCRIPTION: EARTH coupler gridded component main module.
!
! !REVISION HISTORY:
!
!  April    2006     Weiyu Yang Initial code.
!                           
!
!  March 2007         Dingchen Hou added Stochatic Perturbation Combination Coefficient array.
!  January to November 2007      Dingchen and Weiyu Yang
!                     Added Broadcasting procedure and Global variables/arrays
!  November 2007      Dingchen, added minimum documentation, mainly for the arrays added during 2007
!  March    2009      Weiyu yang, modified for the NEMS model.
!  May      2011      Weiyu Yang  modified for using the ESMF 5.2.0r_beta_snapshot_07.

! !INTERFACE:
!
 MODULE ENS_CplComp_ESMFMod
 
!!USES:
!------

! Define the ESMF internal state and all routines related to run 
! the EARTH ensemble coupler grid component.
!---------------------------------------------------------------
!dusan USE ENS_Cpl_ESMFMod
 USE ESMF

 IMPLICIT none

 PRIVATE   ! By default data is private to this module
!
! !PUBLIC TYPES:
!---------------

 PUBLIC ENS_CplCompSetServices

!EOP
!-------------------------------------------------------------------------


 CONTAINS


!----------------------------------------------------------------------
!BOP
!
! !ROUTINE: ENS_CplCompSetServices --- Set services for EARTH Ensemble 
!                                      Coupler Gridded Component.
! 
! !INTERFACE:
!
 SUBROUTINE ENS_CplCompSetServices(CplENS, rc)

! !ARGUMENTS:
!------------

 TYPE(ESMF_CplComp)                 :: CplENS ! gridded component
 INTEGER,             INTENT(out)   :: rc      ! return code
     
! !DESCRIPTION: Set services (register) for the EARTH Ensemble Coupler
!               Grid Component.
!         
!EOP         
!----------------------------------------------------------------------
  
 INTEGER                            :: rc1     = ESMF_SUCCESS
 rc = ESMF_SUCCESS

! REGISTER SERVICES FOR THIS COMPONENT
! ------------------------------------

 CALL ESMF_CplCompSetEntryPoint (CplENS, ESMF_METHOD_INITIALIZE,  Cpl_Initialize &
                                 ,rc    = RC1)

 CALL ESMF_CplCompSetEntryPoint (CplENS, ESMF_METHOD_RUN,   Cpl_Run    &
                                 ,rc    = RC1)

 CALL ESMF_CplCompSetEntryPoint (CplENS, ESMF_METHOD_FINALIZE, Cpl_Finalize   &
                                 ,rc    = RC1)

 END SUBROUTINE ENS_CplCompSetServices





!----------------------------------------------------------------------
!BOP
! !ROUTINE:  Cpl_Initialize --- initialize routine to initialize 
!                               and set up the EARTH ensemble coupler.
!
! !DESCRIPTION: This subroutine initializes the EARTH ensemble coupler
!               before the main running routine.
!
!
! !REVISION HISTORY:
!
!  April    2006     Weiyu Yang Initial code.
!
! !INTERFACE:
!

 SUBROUTINE Cpl_Initialize(CplENS, impENS, expENS, clock, rcfinal)

!
! !INPUT/OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------------

 TYPE(ESMF_CplComp)                    :: CplENS 
 TYPE(ESMF_State)                      :: impENS
 TYPE(ESMF_State)                      :: expENS
 TYPE(ESMF_Clock)                      :: clock

!
! !OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------

 INTEGER,             INTENT(out)       :: rcfinal

! !EOP
!------------------------------------------------------------------------- 

 rcfinal = ESMF_SUCCESS
 
 END SUBROUTINE Cpl_Initialize





!----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Cpl_Run --- Main grid component routine to run the EARTH 
!                       ensemble coupler.
!
! !DESCRIPTION: This subroutine will run the most part computations 
!               of the EARTH ensemble coupler.
!
! !REVISION HISTORY:
!
!  April    2006     Weiyu Yang Initial code.
!
! !INTERFACE:
!

 SUBROUTINE Cpl_Run(CplENS, impENS, expENS, clock, rcfinal)

!
! !INPUT VARIABLES AND PARAMETERS:
!---------------------------------
 TYPE(ESMF_CplComp)                    :: CplENS   
 TYPE(ESMF_State)                      :: impENS 
 
! !OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------
 TYPE(ESMF_Clock)                      :: clock
 TYPE(ESMF_State)                      :: expENS
 INTEGER,            INTENT(out)       :: rcfinal 
!
!EOP
!-------------------------------------------------------------------------

 rcfinal = ESMF_SUCCESS
!
 END SUBROUTINE Cpl_Run





!----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Cpl_Finalize --- finalizing routine to finish the 
!                            EARTH ensemble coupler.
!
! !DESCRIPTION: This subroutine will finish the EARTH ensemble coupler
! !             and will release the memory space.
!
! !REVISION HISTORY:
!
!  April    2006     Weiyu Yang Initial code.
!
! !INTERFACE:

 SUBROUTINE Cpl_Finalize(CplENS, impENS, expENS, clock, rcfinal)

!
! !INPUT VARIABLES AND PARAMETERS:
!---------------------------------
 TYPE(ESMF_CplComp)                 :: CplENS
 TYPE(ESMF_State)                   :: impENS
 TYPE(ESMF_State)                   :: expENS
 TYPE(ESMF_Clock)                   :: clock

! !OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------
 INTEGER,            INTENT(out)    :: rcfinal

 rcfinal = ESMF_SUCCESS

 END SUBROUTINE Cpl_Finalize

 END MODULE ENS_CplComp_ESMFMod
