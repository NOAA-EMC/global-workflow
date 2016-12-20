
! !MODULE: GFS_GridComp_ESMFMod --- ESMF gridded component of the GFS system. 
!
! !DESCRIPTION: GFS gridded component main module.
!
! !REVISION HISTORY:
!
!  November 2004     Weiyu Yang Initial code.
!  May      2005     Weiyu Yang For the updated GFS version.
!  February 2006     Shrinivas Moorthi 
!  February 2008     Weiyu Yang modified for the ESMF 3.1.0 version.
!  September2006     Weiyu Yang For the ensemble run couple version.
!  April    2009     Shrinivas Moorthi merge GEFS and generalized GFS
!  December 2015     C. Redder  Removed intent attribute of the first
!                       argument of the subroutine SetServices to resolve
!                       compiler errors and conform to ANSI standard
!                       fortran and to the interface of a dummy
!                       procedure for a ESMF library routine.
!                           
!
! !INTERFACE:
!
 MODULE GFS_GridComp_ESMFMod
 
!!USES:
!------
 USE ESMF_Mod

! Define the ESMF internal state and all routines related to run 
! the GFS grid component.
!---------------------------------------------------------------
 USE GFS_ErrMsgMod
 USE GFS_Initialize_ESMFMod
 USE GFS_Run_ESMFMod
 USE GFS_Finalize_ESMFMod
 USE mpi_def, ONLY: liope, icolor

 IMPLICIT none

!#include "ESMF_LogMacros.inc"


 PRIVATE   ! By default data is private to this module
!
! !PUBLIC TYPES:
!---------------

 PUBLIC SetServices, ERR_MSG3

!EOP
!-------------------------------------------------------------------------


 CONTAINS


!----------------------------------------------------------------------
!BOP
!
! !ROUTINE: SetServices --- Set services for GFS Gridded Component.
! 
! !INTERFACE:
!
 SUBROUTINE SetServices (gcGFS, rc)
 
! !ARGUMENTS:
!------------

! TYPE(ESMF_GridComp), INTENT(inout) :: gcGFS ! gridded component
 TYPE(ESMF_GridComp)                :: gcGFS ! gridded component
 INTEGER,             INTENT(out)   :: rc    ! return code
     
! !DESCRIPTION: Set services (register) for the GFS Stand Alone
!               Grid Component.
!         
!EOP         
!----------------------------------------------------------------------

 INTEGER                            :: rc1     = ESMF_SUCCESS

! Initializing the error signal variable rc.
!-------------------------------------------
 rc = ESMF_SUCCESS

! REGISTER SERVICES FOR THIS COMPONENT
! ------------------------------------

     CALL ESMF_LogWrite("Set Entry Point for Initialize",                &
                        ESMF_LOG_INFO, rc = rc1)
! Register the Initialize subroutine.  Since it is just one subroutine
! for the Initialize, use ESMF_SINGLEPHASE.  The second argument is
! a pre-defined subroutine type, such as ESMF_SETINIT, ESMF_SETRUN, 
! ESMF_SETFINAL.
!---------------------------------------------------------------------
 CALL ESMF_GridCompSetEntryPoint (gcGFS, ESMF_SETINIT,  Initialize,      &
                                  ESMF_SINGLEPHASE, rc1)

     CALL ERR_MSG3(rc1,'Set Entry Point for Initialize',rc)

     CALL ESMF_LogWrite("Set Entry Point for Run",                       &
                        ESMF_LOG_INFO, rc = rc1)

! Register the Run subroutine.
!-----------------------------
 CALL ESMF_GridCompSetEntryPoint (gcGFS, ESMF_SETRUN,   Run,             &
                                  ESMF_SINGLEPHASE, rc1)

     CALL ERR_MSG3(rc1,'Set Entry Point for Run',rc)

     CALL ESMF_LogWrite("Set Entry Point for Finalize",                  &
                        ESMF_LOG_INFO, rc = rc1)

! Register the Finalize subroutine.
!----------------------------------
 CALL ESMF_GridCompSetEntryPoint (gcGFS, ESMF_SETFINAL, Finalize,        &
                                  ESMF_SINGLEPHASE, rc1)

     CALL ERR_MSG3(rc1,'Set Entry Point for Finalize',rc)

! Check the error signal variable and print out the result.
!----------------------------------------------------------
 IF(rc /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: SetService."
!ELSE
!    PRINT*, "PASS: SetService."
!                           and set up the GFS running job.
 END IF

 END SUBROUTINE SetServices

!----------------------------------------------------------------------
!BOP
! !ROUTINE:  Initialize --- initialize routine to initialize 
!                           and set up the GFS running job.
!
! !DESCRIPTION: This subroutine initializes the GFS running before
!               the main running loop.
!
!
! !REVISION HISTORY:
!
!  November 2004     Weiyu Yang Initial code.
!  May      2005     Weiyu Yang For the updated GFS version.
!  February 2006     Moorthi
!  Octcber  2007     Weiyu Yang modified for the ESMF 3.0.3 version.
!  December 2015     C. Redder  Removed intent attribute of the first
!                       subroutine arguments to resolve compiler errors
!                       and conform to ANSI standard fortran and to the
!                       interface of the dummy procedure for the
!                       routine,  ESMF_GridCompSetEntryPoint (called
!                       above)
!
! !INTERFACE:
!

! This argument list is a standard list for all the Initialize,
! the Run and Finalize routines for an ESMF system.
!--------------------------------------------------------------
 SUBROUTINE Initialize(gcGFS, impGFS, expGFS, clock, rc)

! User code, for computations related to the ESMF interface states.
!------------------------------------------------------------------
 USE GFS_ESMFStateMod
 USE GFS_AddParameterToStateMod
!
! !INPUT/OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------------

! TYPE(ESMF_GridComp), INTENT(inout) :: gcGFS 
! TYPE(ESMF_State),    INTENT(inout) :: impGFS
! TYPE(ESMF_State),    INTENT(inout) :: expGFS
! TYPE(ESMF_Clock),    INTENT(inout) :: clock
 TYPE(ESMF_GridComp) :: gcGFS 
 TYPE(ESMF_State)    :: impGFS
 TYPE(ESMF_State)    :: expGFS
 TYPE(ESMF_Clock)    :: clock

!
! !OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------

 INTEGER, INTENT(out)               :: rc  

! !EOP
!------------------------------------------------------------------------- 
 
! !WORKING ARRAYS AND LOCAL PARAMETERS.  
!--------------------------------------
 TYPE(GFS_wrap)                     :: wrap         ! This wrap is a derived type which contains
                                                    ! only a pointer to the internal state.  It is needed
                                                    ! for using different architectures or compliers.
 TYPE(GFS_InternalState), POINTER   :: Int_State    ! the internal state pointer.
 TYPE(ESMF_VM)                      :: vm_local     ! the ESMF virtual machine.
 TYPE(ESMF_TimeInterval)            :: timeStep     ! the ESMF time step interval.
 TYPE(ESMF_Time)                    :: startTime    ! the ESMF start time.
 TYPE(ESMF_Time)                    :: currTime     ! the ESMF current time.
 TYPE(ESMF_TimeInterval)            :: refTimeInterval 
!TYPE(ESMF_DELayout)                :: myDeLayout   ! the ESMF layout type array.
                                                    ! ESMF time interval, for setting up the begin time.
!TYPE(ESMF_Grid)                    :: grid1        ! the ESMF GRID TYPE ARRAY, for the 
                                                    ! single level spectral arrays.
!TYPE(ESMF_Grid)                    :: grid2        ! the ESMF GRID TYPE ARRAY, for the
                                                    ! multiple levels spectral arrays.
!TYPE(ESMF_Grid)                    :: grid3        ! the ESMF GRID TYPE ARRAY, for the Gaussian grid
                                                    ! surface data arrays.
!TYPE(ESMF_Grid)                    :: grid4        ! the ESMF GRID TYPE ARRAY, for the inputted 
 TYPE(ESMF_Grid)                    :: grid5        ! the ESMF GRID TYPE ARRAY, for the
                                                    ! ensemble coupling spectral arrays.
                                                    ! and time.
 TYPE(ESMF_DistGrid)                :: DistGrid1
 TYPE(ESMF_DistGrid)                :: DistGrid3
!TYPE(ESMF_DistGrid)                :: DistGrid4
 TYPE(ESMF_DistGrid)                :: DistGrid5

 INTEGER(kind=esmf_kind_i4)         :: yy, mm, dd   ! time variables for date
 INTEGER(kind=esmf_kind_i4)         :: hh, mns, sec ! time variables for time
!INTEGER                            :: hhc, mnsc, secc ! time variables for time
 INTEGER                            :: advanceCount4, timeStep_sec
 INTEGER                            :: i, j, n
 INTEGER , DIMENSION(:, :), POINTER :: i2

 INTEGER                            :: rc1 

! Initialize the error signal variables.
!---------------------------------------
 rc1 = ESMF_SUCCESS
 rc  = ESMF_SUCCESS

!These are the standard ESMF internal state lines.
!-------------------------------------------------
     CALL ESMF_LogWrite("Allocate the Internal State",                 &
                        ESMF_LOG_INFO, rc = rc1)

! Allocate the internal state pointer.
!-------------------------------------
 ALLOCATE(Int_State, stat = rc1)

     CALL ERR_MSG3(rc1,' - Allocate the Internal State',rc)

 wrap%Int_State => Int_State

! Attach internal state to the GFS grid component.
!-------------------------------------------------
     CALL ESMF_LogWrite("Set Up the Internal State",                   &
                        ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_GridCompSetInternalState(gcGFS, wrap, rc1)

     CALL ERR_MSG3(rc1,'Set Up the Internal State',rc)

! Use ESMF utilities to get information from the configuration file.
! The function is similar to reading the namelist in the original GFS.
!---------------------------------------------------------------------
     CALL ESMF_LogWrite("Getting Information from the Configure File", &
                        ESMF_LOG_INFO, rc = rc1)
 CALL GFS_GetCf(gcGFS, Int_State,  rc = rc1)

     CALL ERR_MSG3(rc1,'Get Configure File Information',rc)

! Initialize time interval to the parameter from the configure file.
!-------------------------------------------------------------------
     CALL ESMF_LogWrite("Set up Time Step Interval",                   &
                        ESMF_LOG_INFO, rc = rc1)

 timeStep_sec = NINT(Int_State%nam_gfs%DELTIM)

!print *,' deltim=',Int_State%nam_gfs%DELTIM,' timestep_sec=',timestep_sec

 CALL ESMF_TimeIntervalSet(timeStep,                               &
                           s  = timeStep_sec,                      &
                           rc = rc1)

     CALL ERR_MSG3(rc1,'Set up Time Step Interval',rc)

! Get the start time from reading the sigma file.
!----------------------------------------------------------
     CALL ESMF_LogWrite("Getting the Start Time",                      &
                        ESMF_LOG_INFO, rc = rc1)


 CALL StartTimeGet(yy, mm, dd, hh, mns, sec, Int_State%kfhour,         &
                   Int_State%N1,Int_State%N2,Int_State%grib_inp,       &
                   Int_State%nam_gfs%FHROT,  Int_State%nam_gfs%sig_ini,&
                   Int_State%nam_gfs%sig_ini2, rc1)

!CALL StartTimeGet(yy, mm, dd, hh, mns, sec, Int_State%kfhour,     &
!                  Int_State%N1,    Int_State%N2,                  &
!                  Int_State%n1hyb, Int_State%n2hyb,               &
!                  hybrid,FHROT, Int_State%nam_gfs%sig_ini,        &
!                  Int_State%nam_gfs%sig_ini2, rc1)
!Moorthi           Int_State%nam_gfs%hybrid,                       &
!  ,,              Int_State%nam_gfs%FHROT, rc1)

     CALL ERR_MSG3(rc1,'Getting the Start Time',rc)
 
!print *,' yy, mm, dd, hh =', yy,mm,dd,hh
 advanceCount4    = NINT(REAL(Int_State%kfhour) * 3600.0 / Int_State%nam_gfs%DELTIM)
 Int_State%phour  = advanceCount4 * Int_State%nam_gfs%DELTIM / 3600.0
 Int_State%kfhour = NINT(Int_State%phour)

!    print *,' advancecount4=',advancecount4,' kfhour=',Int_State%kfhour
! Initialize the clock with the start time based on the information
! from calling StartTimeGet.
!------------------------------------------
     CALL ESMF_LogWrite("Set up the ESMF Time",                        &
                        ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_TimeSet(startTime, yy = yy, mm = mm,  dd = dd,          &
                              h  = hh, m  = mns, s  = sec, rc = rc1)

     CALL ERR_MSG3(rc1,'Set up the ESMF Time',rc)

     CALL ESMF_LogWrite("Set up the Reference Time Interval",          &
                        ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_TimeIntervalSet(refTimeInterval, h = Int_State%kfhour,      &
                           m = 0, rc = rc1)
! Re-set up the start time based on the kfhour value in the sigma file.
!----------------------------------------------------------------------
!startTime = startTime + refTimeInterval

     CALL ERR_MSG3(rc1,'Set up the Reference Time Interval',rc)
! Set up the ESMF clock which will control the GFS run do loop.
!--------------------------------------------------------------
     CALL ESMF_LogWrite("Create the ESMF Clock", ESMF_LOG_INFO, rc = rc1)

     currTime = startTime + refTimeInterval

     CALL ESMF_ClockSet(clock, name      = 'GFS_RUN_CLOCK', &
                               timeStep  = timeStep,        &
                               startTime = startTime,       &
                               currTime  = currTime,        &
                               rc        = rc1)

     CALL ERR_MSG3(rc1,'Set Up the ESMF Clock',rc)

!
! Get the grid component vm.
! This ESMF_GridCompGet VM can be used at any where you need it.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Get the Local VM", ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_VMGetCurrent(vm_local, rc = rc1)

     CALL ERR_MSG3(rc1,'Get the VM',rc)


! Set up parameters of MPI communications.
! Use ESMF utility to get PE identification and total number of PEs.
!-------------------------------------------------------------------
     CALL ESMF_LogWrite("Get me and NODES from VM", ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_VMGet(vm_local, localPet = Int_State%me,    &
                           petCount = Int_State%NODES, &
                           rc       = rc1)

 Int_State%npe_single_member = Int_State%NODES
 Int_State%mm1               = Int_State%me + 1


     CALL ERR_MSG3(rc1,'Get me and NODES from VM',rc)

! Allocate the local index array i2 to store the local size information of the
! ditributed grid1, grid3, etc..  Information is based per dimension and per De.
!-------------------------------------------------------------------------------
 ALLOCATE(i2(2, Int_State%NODES))

! Initialize the GFS, including set up the internal state
! variables and some local parameter short names, aloocate
! internal state arrays.
!---------------------------------------------------------
     CALL ESMF_LogWrite("Run the GFS_Initialize", ESMF_LOG_INFO, rc = rc1)

 CALL GFS_Initialize(gcGFS, Int_State, clock, rc1)

     CALL ERR_MSG3(rc1,'Run the GFS_Initialize',rc)
!
!

! Create the ESMF grids and distribute the grids into
! the ESMF DELayout (myDeLayout).
!-----------------------------------------------------------------
     CALL ESMF_LogWrite("Creat the ESMF Grid and DELayout.", ESMF_LOG_INFO, rc = rc1)

CALL DistGrid_ESMFCreate1(vm_local, Int_State, distgrid1, rc1)

 CALL DistGrid_ESMFCreate3(vm_local, Int_State, distgrid3, rc1)
 IF(Int_State%ESMF_Sta_List%trieo_export) THEN
     CALL Grid_ESMFCreate5(vm_local, grid5, Int_State, distgrid5, rc1)

     CALL ERR_MSG3(rc1,'Grid_ESMFCreate',rc)


! Associate the grid5 with the ESMF grid component gsGFS
!-------------------------------------------------------
     CALL ESMF_LogWrite("Attach the ESMF Grids to the ESMF Grid Component.", &
                        ESMF_LOG_INFO, rc = rc1)

     CALL ESMF_GridCompSet(gcGFS, grid = grid5, rc = rc1)

     CALL ERR_MSG3(rc1,'ESMF_GridCompSet - set grid5',rc)

! Create a export ESMF state for initializing the coupler.
!---------------------------------------------------------
     n = 11*Int_State%levs+3*Int_State%levh+6

! Set up the ESMF TRIEO STATE name.
!----------------------------------
     CALL ESMF_LogWrite("Set up the ESMF TRIEO STATE name", &
                        ESMF_LOG_INFO, rc = rc1)

     WRITE(Int_State%TRIEO_STATE_NAME, 1000)
     WRITE(Int_State%TRIEO_STINI_NAME, 2000)
1000 FORMAT('TRIEO_ESMF_STATE')
2000 FORMAT('TRIEO_ESMF_STINI')

     CALL ERR_MSG3(rc1, 'Set up the ESMF TRIEO STATE name', rc)

     ALLOCATE(Int_State%write_work8    (Int_State%trieo_ls_size(Int_State%mm1), 2*n))
     ALLOCATE(Int_State%write_work8_ini(Int_State%trieo_ls_size(Int_State%mm1), 2*n))

     Int_State%write_work8 = 0.0
     DO j = 1, 2
         Int_State%write_work8(1, j) = FLOAT(Int_State%TRIE_LS_SIZE(Int_State%mm1))
         Int_State%write_work8(2, j) = FLOAT(Int_State%TRIO_LS_SIZE(Int_State%mm1))
         Int_State%write_work8(3, j) = FLOAT(n)
     END DO
     CALL AddF90ArrayToState(expGFS, grid5, Int_State%TRIEO_STATE_NAME, &
                                            Int_State%write_work8,      &
                                            rc       = rc1)

     Int_State%write_work8_ini = 0.0
     CALL AddF90ArrayToState(expGFS, grid5, Int_State%TRIEO_STINI_NAME, &
                                            Int_State%write_work8_ini,  &
                                            rc       = rc1)

     CALL AddParameterToState(expGFS, Int_State, rc = rc1)
 END IF

! Get the local array size of the grid1, the single level spectral arrays.
!-------------------------------------------------------------------------
 i2 = 0
 CALL ESMF_DistGridGet(DistGrid1, indexCountPDimPDe = i2, rc = rc1)

     CALL ERR_MSG3(rc1,'Grid Get Info - lnt2_s',rc)

! Put the grid1 local array size into the internal state and print it out.
!-------------------------------------------------------------------------
 Int_State%lnt2_s = i2(1, Int_State%me + 1)
 PRINT*, 'Local number of the Grid1', i2(:, Int_State%me + 1)

!-----------------------------------------------------------------
 i2 = 0
 CALL ESMF_DistGridGet(DistGrid3, indexCountPDimPDe = i2, rc = rc1)

! Put the grid3 local array size into the internal state and print it out.
!-------------------------------------------------------------------------
 Int_State%lonr_s = i2(1, Int_State%me + 1)
 Int_State%latr_s = i2(2, Int_State%me + 1)
 PRINT*, 'Local number of the Grid3', i2(:, Int_State%me + 1)

 CALL ESMF_DistGridDestroy(distgrid1, rc = rc1)
 CALL ESMF_DistGridDestroy(distgrid3, rc = rc1)

     CALL ERR_MSG3(rc1,'Grid Get Info - lonr_s, latr_s',rc)

! print out the final error signal variable and put it to rc.
!------------------------------------------------------------
 IF(rc /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: Initialize."
!ELSE
!    PRINT*, "PASS: Initialize."
 END IF

 END SUBROUTINE Initialize







!CALL Grid_ESMFCreate1(vm_local, grid1, grid3, grid4, DistGrid1, &
!    DistGrid3, DistGrid4, Int_State, rc1)

!    CALL ERR_MSG3(rc1,'Grid_ESMFCreate',rc)



! Associate the grid3 with the ESMF grid component gsGFS
! used at the begining of the Run routine when read in
! the surface arrays of the ESMF import state.
!-------------------------------------------------------
!    CALL ESMF_LogWrite("Attach the ESMF Grids to the ESMF Grid Component.", &
!                       ESMF_LOG_INFO, rc = rc1)

!CALL ESMF_GridCompSet(gcGFS, grid = grid3, rc = rc1)

!    CALL ERR_MSG3(rc1,'ESMF_GridCompSet - set grid3',rc)

! Get the local array size of the grid1, the single level spectral arrays.
!-------------------------------------------------------------------------
!i2 = 0
!CALL ESMF_DistGridGet(DistGrid1, indexCountPDimPDe = i2, rc = rc1)

!    CALL ERR_MSG3(rc1, 'Grid Get Info - lnt2_s', rc)

! Put the grid1 local array size into the internal state and print it out.
!-------------------------------------------------------------------------
!Int_State%lnt2_s = i2(1, Int_State%me + 1)
!PRINT*, 'Local number of the Grid1', i2(1, Int_State%me + 1)

! Get the local array size of the grid3, the Gaussian grid arrays.
!-----------------------------------------------------------------
!CALL ESMF_DistGridGet(DistGrid3, indexCountPDimPDe = i2, rc = rc1)

! Put the grid3 local array size into the internal state and print it out.
!-------------------------------------------------------------------------
!Int_State%lonr_s = i2(1, Int_State%me + 1)
!Int_State%latr_s = i2(2, Int_State%me + 1)
!PRINT*, 'Local number of the Grid3', i2(:, Int_State%me + 1)

!    CALL ERR_MSG3(rc1,'Grid Get Info - lonr_s, latr_s',rc)

! Get the size of grid4.  It is just for testing and can be removed.
!-------------------------------------------------------------------
!CALL ESMF_DistGridGet(DistGrid4, indexCountPDimPDe = i2, rc = rc1)
!PRINT*, 'Local number of the Grid4', Int_State%grid4_i2(:, Int_State%me + 1)

!    CALL ERR_MSG3(rc1,'Grid Get Info - grid4',rc)

!CALL ESMF_GridDestroy(grid1, rc = rc1)
!CALL ESMF_GridDestroy(grid3, rc = rc1)
!CALL ESMF_GridDestroy(grid4, rc = rc1)
!DEALLOCATE(i2)

! print out the final error signal variable and put it to rc.
!------------------------------------------------------------
!IF(rc == ESMF_SUCCESS) THEN
!    PRINT*, "PASS: Initialize."
!ELSE
!    PRINT*, "FAIL: Initialize."
!END IF

!END SUBROUTINE Initialize





!----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Run --- Main grid component routine to run the GFS system.
!
! !DESCRIPTION: This subroutine will run the most part computations 
!               of the GFS system.
!
! !REVISION HISTORY:
!
!  November 2004     Weiyu Yang Initial code.
!  May      2005     Weiyu Yang For the updated GFS version.
!  February 2006     Moorthi
!  Octcber  2007     Weiyu Yang modified for the ESMF 3.0.3 version.
!  September2006     Weiyu Yang For the ensemble run couple version.
!  April    2009     Moorthi merged GFS and GEFS versions
!  December 2015     C. Redder  Removed intent attribute of the first
!                       subroutine arguments to resolve compiler errors
!                       and conform to ANSI standard fortran and to the
!                       interface of the dummy procedure for the
!                       routine,  ESMF_GridCompSetEntryPoint (called
!                       above)
!
! !INTERFACE:
!

 SUBROUTINE Run(gcGFS, impGFS, expGFS, clock, rc)

 USE GFS_ESMFStateMod
!
! !INPUT VARIABLES AND PARAMETERS:
!---------------------------------
! TYPE(ESMF_GridComp), INTENT(inout) :: gcGFS   
! TYPE(ESMF_State),    INTENT(in)    :: impGFS 
 TYPE(ESMF_GridComp) :: gcGFS   
 TYPE(ESMF_State)    :: impGFS 
 
! !OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------
! TYPE(ESMF_Clock),    INTENT(inout) :: clock
! TYPE(ESMF_State),    INTENT(inout) :: expGFS
 TYPE(ESMF_Clock)    :: clock
 TYPE(ESMF_State)    :: expGFS
 INTEGER,             INTENT(out)   :: rc   
!
!EOP
!-------------------------------------------------------------------------

!
! !WORKING ARRAYS AND LOCAL PARAMETERS.
!--------------------------------------
 TYPE(ESMF_VM)                     :: vm_local
 TYPE(GFS_wrap)                    :: wrap         ! This wrap is a derived type which contains
                                                   ! only a pointer to the internal state.  It is needed
                                                   ! for using different architectures or compliers.
 TYPE(GFS_InternalState), POINTER  :: Int_State    ! the internal state pointer.
 INTEGER                           :: rc1          ! error signal variable.
 LOGICAL                           :: first = .true.

 SAVE first

! Initialize the error signal variables.
!---------------------------------------
 rc1 = ESMF_SUCCESS
 rc  = ESMF_SUCCESS

!print *,' INSIDE GFS GFS_GridComp'

! Retrieve the ESMF internal state.
!---------------------------------- 
     CALL ESMF_LogWrite("Get the Internal State in the Run Routine", &
                        ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_VMGetCurrent(vm_local, rc = rc1)
 CALL ESMF_GridCompGetInternalState(gcGFS, wrap, rc1)

     CALL ERR_MSG3(rc1,'Get the Internal State in the Run Routine',rc)

! pointing the local internal state pointer to the ESMF internal state pointer.
!------------------------------------------------------------------------------
 Int_State => wrap%Int_State

 IF(Int_State%nam_gfs%Total_member > 1 .AND. .NOT. first .and. Int_State%sps) THEN
     IF(icolor /= 2 .OR. .NOT. liope) THEN
         Int_State%ESMF_Sta_List%trieo_import = .true.
     ELSE
         Int_State%ESMF_Sta_List%trieo_export = .true.
     END IF
 END IF

! Get the ESMF import state and over-write the GFS internal state.
! Update the initial condition arrays in the internal state based on
! the information of the ESMF import state. 
!------------------------------------------------------------------
     CALL ESMF_LogWrite("ESMF Import State to Internal State", &
                        ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_VMBarrier(vm_local,                                    rc = rc1)
! print *,' calling GFS_ESMFImportState2InternalState'
 CALL GFS_ESMFImportState2InternalState(gcGFS, impGFS, Int_State, rc = rc1)
! print *,' after calling GFS_ESMFImportState2InternalState'

     CALL ERR_MSG3(rc1,'ESMF Import State to Internal State',rc)

! Run the GFS.
!--------------------------
     CALL ESMF_LogWrite("Run the GFS_Run", ESMF_LOG_INFO, rc = rc1)

 CALL GFS_Run(clock, Int_State, rc = rc1)

     CALL ERR_MSG3(rc1,'Run the GFS_Run',rc)

! Transfer the GFS export fields in the internal state 
! to the ESMF exprot state which is the public interface
! for other ESMF grid components.
!-------------------------------------------------------
     CALL ESMF_LogWrite("Internal State to ESMF Export State", &
                        ESMF_LOG_INFO, rc = rc1)

!*******************************************************************
!
!  **** This transfer from internal state to export state is
!  **** disabled by Moorthi on 20060622
!

!CALL GFS_InternalState2ESMFExportState(gcGFS, expGFS, Int_State, rc = rc1)

!    CALL ERR_MSG3(rc1,'Internal State to ESMF Export State',rc)

 CALL GFS_InternalState2ESMFExportState(Int_State)
!
!*******************************************************************
!
! Print out the final error signal information and put it to rc.
!---------------------------------------------------------------
 first = .false.

 IF(rc /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: Run."
!ELSE
!    PRINT*, "PASS: Run."
 END IF

 END SUBROUTINE Run


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
!  February 2006     Moorthi
!  Octcber  2007     Weiyu Yang modified for the ESMF 3.0.3 version.
!  September2006     Weiyu Yang For the ensemble run couple version.
!  December 2015     C. Redder  Removed intent attribute of the first
!                       subroutine arguments to resolve compiler errors
!                       and conform to ANSI standard fortran and to the
!                       interface of the dummy procedure for the
!                       routine,  ESMF_GridCompSetEntryPoint (called
!                       above)
!
! !INTERFACE:

 SUBROUTINE Finalize(gcGFS, impGFS, expGFS, clock, rc)

!
! !INPUT VARIABLES AND PARAMETERS:
!---------------------------------
! TYPE(ESMF_GridComp), INTENT(inout)  :: gcGFS
! TYPE(ESMF_State),    INTENT(inout)  :: impGFS
! TYPE(ESMF_State),    INTENT(inout)  :: expGFS
! TYPE(ESMF_Clock),    INTENT(inout)  :: clock
 TYPE(ESMF_GridComp) :: gcGFS
 TYPE(ESMF_State)    :: impGFS
 TYPE(ESMF_State)    :: expGFS
 TYPE(ESMF_Clock)    :: clock

! !OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------
 INTEGER,             INTENT(out)    :: rc

! !WORKING ARRAYS AND LOCAL PARAMETERS.
!--------------------------------------
 TYPE(GFS_wrap)                      :: wrap         ! This wrap is a derived type which contains
                                                   ! only a pointer to the internal state.  It is needed
                                                   ! for using different architectures or compliers.
 TYPE(GFS_InternalState), POINTER    :: Int_State    ! the internal state pointer.
 INTEGER                             :: rc1          ! error signal variable.

!EOP
!-------------------------------------------------------------------------

! Initialize the error signal variables.
!---------------------------------------
 rc1 = ESMF_SUCCESS
 rc  = ESMF_SUCCESS

! Retrieve the ESMF internal state.
!----------------------------------
     CALL ESMF_LogWrite("Get the Internal State in the Finalize Routine", &
                        ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_GridCompGetInternalState(gcGFS, wrap, rc1)

     CALL ERR_MSG3(rc1,'Get the Internal State in the Finalize Routine',rc)

! Point the local internal state pointer to the ESMF internal state pointer.
!------------------------------------------------------------------------------
 Int_State => wrap%Int_State

! Run the GFS Finalize routine to release the memory space, etc. 
!----------------------------------------------------------------------------
     CALL ESMF_LogWrite("Run the GFS_Finalize", ESMF_LOG_INFO, rc = rc1)

 CALL GFS_Finalize(gcGFS, Int_State, rc = rc1)

     CALL ERR_MSG3(rc1,'Run the GFS_Finalize',rc)

!********************************Moorthi***************************
! Destroy the ESMF clock.
!------------------------
!    CALL ESMF_LogWrite("Destroy the ESMF Clock", ESMF_LOG_INFO, rc = rc1)

!CALL ESMF_ClockDestroy(clock, rc = rc1)

!    CALL ERR_MSG3(rc1,'Destroy the ESMF Clock',rc)
!********************************Moorthi***************************

! Print out the final error signal information and put it to rc.
!---------------------------------------------------------------
 IF(rc /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: Finalize."
 ELSE
     PRINT*, "PASS: Finalize."
 END IF

 END SUBROUTINE Finalize

! End of the GFS ESMF grid component module.
!-------------------------------------------
 END MODULE GFS_GridComp_ESMFMod
