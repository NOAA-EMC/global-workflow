
!BOP
!
! !IROUTINE: GFS_ESMF --- Application main program to run the NCEP 
!                         GFS system.
!
! !DESCRIPTION: This program uses the ESMF superstructure to run
!               the NCEP GFS modeling system.
!
! !REVISION HISTORY:
!
!  November 2004     W Yang              Initial code.
!  May      2005     W Yang              For the updated GFS version.
!  September 2005    W Yang              Concurrent version for the ensemble operational forecast.`
!  March     2006    S. Moorthi          New version of the model
!  April     2006    W Yang              GFS Ensemble Coupler (STTP).
!  October   2013    W Yang and D Hou    Fix LIOPE option for GFS Ensemble Coupler (STTP).
!
! !INTERFACE:
 PROGRAM GFS_ESMF

!
!USES:
!
! Use the ESMF library.
!----------------------
! Coupling insertion->
      USE ATM_cc, ONLY: MPI_COMM_Atmos
!<-Coupling insertion
 USE ESMF_Mod

! Use the GFS grid component module, 
! only the set service registration routine is public.
!-----------------------------------------------------
 USE GFS_GridComp_ESMFMod, ONLY: GFS_StandAlone_SetServices => SetServices
 USE GEFS_CplComp_ESMFMod, ONLY: GEFS_CplCompSetServices
 USE mpi_def,              ONLY: liope
 USE GFS_ErrMsgMod

! This is some information required by the ESMF Log error utility.
!-----------------------------------------------------------------
!#include "ESMF_LogMacros.inc"

 IMPLICIT none

!
!!ESMF DERIVED DATA TYPE ARRAYS:
!
 TYPE(ESMF_VM)           :: vm        ! ESMF virtual machine, which contains and
                                      ! manages the computer CPU resource for the 
                                      ! ESMF grid components.
 TYPE(ESMF_GridComp), DIMENSION(:), ALLOCATABLE :: gcGFS
                                      ! ESMF composite gridded component.
 TYPE(ESMF_CplComp)      :: CplGEFS   ! ESMF GFS ensemble coupler gridded component.
 
 TYPE(ESMF_State)        :: impGFS    ! ESMF interface import state.
 TYPE(ESMF_State)        :: expGFS    ! ESMF interface export state.
 TYPE(ESMF_State)        :: impGEFS   ! ESMF Coupler import state.
 TYPE(ESMF_State)        :: expGEFS   ! ESMF Coupler export state.

 TYPE(ESMF_LOG)          :: logGFS    ! ESMF Log Error object.
 TYPE(ESMF_Clock)        :: clock     ! ESMF time management data type clock.
 TYPE(ESMF_Time)         :: startTime
 TYPE(ESMF_TimeInterval) :: timeStep
 TYPE(ESMF_config)       :: Cf        ! ESMF config
 TYPE(ESMF_TimeInterval) :: runDuration

 INTEGER                 :: rc = ESMF_SUCCESS ! running error signal.
 INTEGER                 :: rcfinal           ! final value of the running error signal.
 INTEGER                 :: Total_member      ! number of ensemble members.
 INTEGER                 :: Member_Id
 INTEGER                 :: hh_increase
 INTEGER                 :: hh_start
 INTEGER                 :: hh_final
 INTEGER                 :: Number_start
 INTEGER                 :: Number_final
 INTEGER                 :: i, j, tasks, hh
 INTEGER, Allocatable :: pe_member(:)     ! number of pe per members
 LOGICAL                 :: Ens_sps       ! control of stochastic perturbation scheme (sps)
 LOGICAL                 :: ens_comp_pe   ! the pe flag used in the ensemble coupler.

 CHARACTER(ESMF_MAXSTR)                            :: CplCompName
 CHARACTER(ESMF_MAXSTR)                            :: impGEFSName
 CHARACTER(ESMF_MAXSTR)                            :: expGEFSName
 CHARACTER(ESMF_MAXSTR)                            :: impStateName
 CHARACTER(ESMF_MAXSTR)                            :: expStateName
 CHARACTER(ESMF_MAXSTR), DIMENSION(:), ALLOCATABLE :: GridCompName

 character*20                                      :: PELAB

!
!!BEGIN GFS CODE.
!
!     include 'fexcp.h'
!     call signal(11, xl__trce)
!
! Initialize the final error signal.
!-----------------------------------
 rcfinal = ESMF_SUCCESS

!
!  Initialize the ESMF framework. 
!  ------------------------------
! Coupling insertion->
!        print*,'AM: to call MPI_INIT'

      call MPI_INIT(rc)

!        print*,'AM: back from MPI_INIT'

      call ATM_CMP_START

!        print*,'AM: back from ATM_CMP_START',MPI_COMM_Atmos

 CALL ESMF_Initialize(                                      &
                      defaultCalendar = ESMF_CAL_GREGORIAN, & ! Set up the default calendar.
                      defaultlogtype  = ESMF_LOG_MULTI,     & ! Define multiple log error output file,
                                                              ! each task has its own log error output file.
                      mpiCommunicator = MPI_COMM_Atmos,     &
                      vm = vm,                              &
                      rc              = rc)

!     print*,'AM: back from ESMF_Initialize, rc=',rc
!<-Coupling insertion

!  Open the Log Error file and Set Log parameters.
!-------------------------------------------------

!Question here, if no such lines, no error mesage output to the default log?
!---------------------------------------------------------------------------

! When user does not want to use the ESMF default Log, use it to open a user defined Log.
!-------------------------------------------------------------------------------
 CALL ESMF_LogOpen (logGFS, 'GFS_Error_Log.txt',            &
                    logtype = ESMF_LOG_MULTI, rc = rc)
! Coupling insertion->
!     print*,'AM: back from ESMF_LogOpen'
!<-Coupling insertion

! Set up the user defined Log.
!-----------------------------
 CALL ESMF_LogSet(log         = logGFS,             &
                  verbose     = ESMF_TRUE,          &
                  flush       = ESMF_TRUE,          &
                  rootOnly    = ESMF_FALSE,         &
                  halt        = ESMF_LOG_HALTERROR, & ! It means that the job will be stopped
                                                      ! when error happens.
                  maxElements = 10,                 & ! Maximum number of elements in the log
                                                      ! before printing them to the log file.
                  rc          = rc)
! Coupling insertion->
!     print*,'AM: back from ESMF_LogSet'
!<-Coupling insertion

!
! Load the config_file to the config

 Cf       = ESMF_ConfigCreate(rc = rc)
 CALL ESMF_ConfigLoadFile(Cf, 'gfs_namelist.rc', rc = rc)

! Coupling insertion->
!     print*,'AM: back from ESMF_ConfigLoadFile'
!<-Coupling insertion

! To get the total number of the ensemble members.
! If run stand alone, Total_member = 1.
!-------------------------------------------------
!CALL GetTotalMember(Total_member,Cf,rc)

 rc       = ESMF_SUCCESS
 CALL ESMF_ConfigGetAttribute(Cf, Total_member,                    &
                              label = 'TOTAL_MEMBER:', rc    = rc)
! Coupling insertion->
!     print*,'AM: back from ESMF_ConfigGetAttribute'
!<-Coupling insertion

     CALL ERR_MSG3(rc,'Get the Number of the Total Ensemble Members',rcfinal)

! Aloocate arrays.
!-----------------
 ALLOCATE(gcGFS       (Total_member))
 ALLOCATE(GridCompName(Total_member))
 ALLOCATE(pe_member   (Total_member))

!SET UP CHARACTERS PARAMETERS:
!-----------------------------

! Coupling insertion->
!     call MPI_COMM_SIZE(MPI_COMM_Atmos,tasks,rc)
!<-Coupling insertion
!<-Coupling replacement  CALL ESMF_VMGet(vm, peCount = tasks, rc = rc)

  CALL ESMF_VMGet(vm, peCount = tasks, rc = rc)

 rc       = ESMF_SUCCESS
 DO i=1, Total_member

!                    Set State Names
!                    ---------------
   WRITE(GridCompName(i), '("GFS_STAND_ALONE GRID COMPONENT",I2.2)') i

!                    Get pe_members
!                    ---------------
   write(PELAB,'("PE_MEMBER",I2.2,":")') i
   CALL ESMF_ConfigGetAttribute(Cf, pe_member(i), label = PELAB, rc = rc)

! Coupling insertion->
!     print*,'AM: back from ESMF_ConfigGetAttribute(Cf, pe_member(i)...'
!<-Coupling insertion

   if (pe_member(i) == 0) pe_member(i) = tasks / Total_Member
 ENDDO

 CALL ESMF_ConfigGetAttribute(cf, Ens_sps,             &
                              label = 'ENS_SPS:',    rc = rc)

 CALL ESMF_ConfigGetAttribute(Cf,                      &
                              hh_increase,             &
                              label = 'HH_INCREASE:',  &
                              rc    = rc)

 CALL ESMF_ConfigGetAttribute(Cf,                      &
                              hh_start,                &
                              label = 'HH_START:',     &
                              rc    = rc)

 CALL ESMF_ConfigGetAttribute(Cf,                      &
                              hh_final,                &
                              label = 'HH_FINAL:',     &
                              rc    = rc)

 CALL ESMF_ConfigGetAttribute(Cf,                      &
                              liope,                   &
                              label = 'LIOPE:',        &
                              rc    = rc)

 WRITE(impStateName, '("GFS Import State")')
 WRITE(expStateName, '("GFS Export State")')
 WRITE(CplCompName, '("GFS Coupler Grid Component Name")')
 WRITE(impGEFSName, '("GEFS Import State")')
 WRITE(expGEFSName, '("GEFS Export State")')

!!CREATE THE ESMF GFS GRID COMPONENT AND THE ENSEMBLE COUPLER COMPONENT
!!USING CORRESPONDING LAYOUT:
!----------------------------------------------------------------------
 CALL ESMF_LogWrite("Create the GFS Grid Component and GEFS CPL Component", &
                    ESMF_LOG_INFO, rc = rc)

 CALL GridCompCreate(vm, gcGFS, CplGEFS, GridCompName, CplCompName, &
                         Cf, Total_Member, pe_member, Member_Id,    &
                         Ens_sps, liope, ens_comp_pe, rc)

 CALL ERR_MSG3(rc,'Create Grid Component and Cpl Component',rcfinal)

!REGISTER THE GFS AND COUPLER GRID COMPONENTS:
!---------------------------------------------
 CALL ESMF_LogWrite("Run Set Services of GFS and Coupler Grid Components", &
                    ESMF_LOG_INFO, rc = rc)

! Coupling insertion->
!     print*,'AM: back from ESMF_LogWrite("Run Set Services...'
!<-Coupling insertion

 rc  = ESMF_SUCCESS
 DO i=1, Total_member
  CALL ESMF_GridCompSetServices(gcGFS(i),  GFS_StandAlone_SetServices, rc)
 END DO

 IF (Ens_sps .AND. ens_comp_pe) THEN
     CALL ESMF_CplCompSetServices(CplGEFS, GEFS_CplCompSetServices, rc)
 END IF

! Coupling insertion->
!     print*,'AM: back from GFS_SetServices'
!<-Coupling insertion

 CALL ERR_MSG3(rc,'Grid and Coupler Components Set Services',rcfinal)

!
!!CREATE THE IMPORT AND EXPORT ESMF STATES:
!
 CALL ESMF_LogWrite("Create the Import ESMF State",        &
                     ESMF_LOG_INFO, rc = rc)

! Coupling insertion->
!     print*,'AM: back from ESMF_LogWrite("Create the Import...'
!<-Coupling insertion

 impGFS  = ESMF_StateCreate(statename = impStateName,      &
                            statetype = ESMF_STATE_IMPORT, &
                            rc        = rc)
 IF (Ens_sps .AND. ens_comp_pe) THEN
     impGEFS = ESMF_StateCreate(statename = impGEFSName,       &
                                statetype = ESMF_STATE_IMPORT, &
                                rc        = rc)
 END IF

 CALL ERR_MSG3(rc,'Create the Import ESMF State',rcfinal)


 CALL ESMF_LogWrite("Create the Export ESMF State",        &
                    ESMF_LOG_INFO, rc = rc)
 expGFS  = ESMF_StateCreate(statename = expStateName,      &
                            statetype = ESMF_STATE_EXPORT, &
                            rc        = rc)
 IF (Ens_sps .AND. ens_comp_pe) THEN
     expGEFS = ESMF_StateCreate(statename = expGEFSName,       &
                                statetype = ESMF_STATE_EXPORT, &
                                rc        = rc)
 END IF

 CALL ERR_MSG3(rc,'Create the Export ESMF State',rcfinal)

! Set up the ESMF clock.  At this step just use an arbitrary
! ESMF time and an arbitrary time step to set up it.
!-----------------------------------------------------------
 CALL ESMF_TImeSet(startTime, yy = 2007, rc = rc)

 CALL ERR_MSG3(rc,'Set up the ESMF startTime', rcfinal)

 CALL ESMF_TimeIntervalSet(timeStep, m = 10, rc = rc)

 CALL ERR_MSG3(rc,'Set up the ESMF timeStep', rcfinal)

 clock = ESMF_ClockCreate(name      = 'GFS_RUN_CLOCK', &
                          timeStep  = timeStep,        &
                          startTime = startTime,       &
                          rc        = rc)

 CALL ERR_MSG3(rc,'Create up the ESMF clock', rcfinal)

!
!!INITIALIZE THE GFS GRID COMPONENT:
!
 CALL ESMF_LogWrite("Calling the GFS Initialize",   &
                    ESMF_LOG_INFO, rc = rc)

! Coupling insertion->
!     print*,'AM: to call GFS_Initialize_Ens'
!<-Coupling insertion

 rc  = ESMF_SUCCESS
 DO i=1, Total_member
   IF(Member_Id == i) THEN
     CALL ESMF_GridCompInitialize (gcGFS(i),                       &
                                   importstate = impGFS,           &
                                   exportstate = expGFS,           &
                                   clock       = clock,            &
                                   phase       = ESMF_SINGLEPHASE, &
                                   rc          = rc)
   ENDIF
 ENDDO
!CALL GFS_Initialize_Ens (gcGFS,impGFS,expGFS,clock,Total_member,Member_Id, rc)

! Coupling insertion->
!     print*,'AM: back from GFS_Initialize_Ens'
!<-Coupling insertion

 CALL ERR_MSG3(rc,'Run the GFS Initialize',rcfinal)

!
!!INITIALIZE THE GFS COUPLER GRID COMPONENT:
!
 IF (Ens_sps .AND. ens_comp_pe) THEN
   CALL ESMF_LogWrite("Calling the GFS COUPLER Initialize",    &
                       ESMF_LOG_INFO, rc = rc)
   CALL ESMF_CplCompInitialize(CplGEFS,                        &
                               importstate = expGFS,           &
                               exportstate = impGFS,           &
                               clock       = clock,            &
                               phase       = ESMF_SINGLEPHASE, &
                               rc          = rc)
   CALL ERR_MSG3(rc, 'Run the GFS Coupler Initialize', rcfinal)
 ENDIF

 CALL ESMF_ConfigDestroy(Cf, rc = rc)     ! Destroy the parental config?

! Coupling insertion->
!      print*,'AM: back from ESMF_ConfigDestroy'
!<-Coupling insertion

!
!!RUNNING THE GFS GRID COMPONENT:

 Number_start = hh_start / hh_increase + 1
 Number_final = hh_final / hh_increase
 if(Number_final-Number_start > 0)then
   print *, 'DHOUCoup', Number_start, Number_final, hh_start, hh_final, hh_increase, 'ens_sps=',ens_sps
 end if

 DO j=Number_start, Number_final 
!
   CALL ESMF_LogWrite("Calling the GFS Run", ESMF_LOG_INFO, rc = rc)
   CALL ESMF_VMBarrier(vm, rc = rc)

   rc  = ESMF_SUCCESS
   DO i=1, Total_member
     IF(Member_Id == i) THEN
       CALL ESMF_GridCompRun (gcGFS(i),                       &
                              importstate = impGFS,           &
                              exportstate = expGFS,           &
                              clock       = clock,            &
                              phase       = ESMF_SINGLEPHASE, &
                              rc          = rc)
     ENDIF
   ENDDO

!  CALL GFS_Run_Ens  (gcGFS, impGFS, expGFS, clock, Total_member, Member_Id, rc)

   CALL ERR_MSG3(rc,'Run the GFS RUN',rcfinal)
!
   IF(Ens_sps) THEN
       IF(ens_comp_pe) THEN
           PRINT *, 'DHOU CPL NO j=',j
           CALL ESMF_LogWrite("Calling the GFS Coupler Run",           &
                              ESMF_LOG_INFO, rc = rc)
           CALL ESMF_CplCompRun       (CplGEFS,                        &
                                       importstate = expGFS,           &
                                       exportstate = impGFS,           &
                                       clock       = clock,            &
                                       phase       = ESMF_SINGLEPHASE, &
                                       rc          = rc)

           CALL ERR_MSG3(rc, 'Run the GFS COupler RUN', rcfinal)
       ELSE
! For LIOPE=.true. and the I/O pe.
!---------------------------------
           IF(j < Number_final) THEN
               CALL ESMF_ClockGet(clock, runDuration = runDuration, rc = rc)
               CALL ERR_MSG3(rc, 'Getting runDuration on the I/O pe', rcfinal)

               CALL ESMF_TimeIntervalGet(runDuration, h = hh,       rc = rc)
               CALL ERR_MSG3(rc, 'Getting runDuration on the I/O pe', rcfinal)

! Adjust the clock for the next stage of forecast run.
!-----------------------------------------------------
               hh = hh + hh_increase
               CALL ESMF_TimeIntervalSet(runDuration, h = hh,       rc = rc)
               CALL ERR_MSG3(rc, 'Setting runDuration on the I/O pe', rcfinal)
               CALL ESMF_ClockSet(clock, runDuration = runDuration, rc = rc)
               CALL ERR_MSG3(rc, 'Adjusting clock on the I/O pe', rcfinal)
           END IF
       END IF
       IF(j <= Number_final) THEN
           PRINT*, 'Complete Ensemble GFS Run Cycle = ', j
       END IF
   END IF
 ENDDO
 IF (Ens_sps) THEN       !   RUN GFS_RUN for the last SPS output
   CALL ESMF_LogWrite("Calling the GFS Run", ESMF_LOG_INFO, rc = rc)
   CALL ESMF_VMBarrier(vm, rc = rc)

   DO i=1, Total_member
     IF(Member_Id == i) THEN
       CALL ESMF_GridCompRun (gcGFS(i),                       &
                              importstate = impGFS,           &
                              exportstate = expGFS,           &
                              clock       = clock,            &
                              phase       = ESMF_SINGLEPHASE, &
                              rc          = rc)
     ENDIF
   ENDDO

!  CALL GFS_Run_Ens  (gcGFS, impGFS, expGFS, clock, Total_member, Member_Id, rc)

   CALL ERR_MSG3(rc,'Run the GFS RUN',rcfinal)
 ENDIF

!
!! FINALIZE THE GFS GRID COMPONENT
!
 CALL ESMF_LogWrite("Calling the GFS Finalize_Ens",  &
                    ESMF_LOG_INFO, rc = rc)

 rc   = ESMF_SUCCESS
 DO i=1, Total_member
   IF(Member_Id == i) THEN
     CALL ESMF_GridCompFinalize (gcGFS(i),                       &
                                 importstate = impGFS,           &
                                 exportstate = expGFS,           &
                                 clock       = clock,            &
                                 phase       = ESMF_SINGLEPHASE, &
                                 rc          = rc)
     CALL ESMF_GridCompDestroy(gcGFS(i), rc = rc)
   ENDIF
 ENDDO

 CALL ERR_MSG3(rc, 'Run the GFS Finalize_Ens', rcfinal)

!
!!AFTER RUNNING, FINALIZE THE COUPLER COMPONENT:
!
 IF (Ens_sps .AND. ens_comp_pe) THEN
    CALL ESMF_LogWrite("Calling the GFS Coupler Finalize",      &
                       ESMF_LOG_INFO, rc = rc)
    CALL ESMF_CplCompFinalize  (CplGEFS,                        &
                                importstate = expGFS,           &
                                exportstate = impGFS,           &
                                clock       = clock,            &
                                phase       = ESMF_SINGLEPHASE, &
                                rc          = rc)
    CALL ERR_MSG3(rc, 'Run the GFS Coupler Finalize', rcfinal)
 ENDIF

!  Finalize the ESMF System.
!---------------------------
  CALL ESMF_Finalize()

 IF(rcfinal /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: GFS_Standalone.F90"
!ELSE
!    PRINT*, "PASS: GFS_Standalone.F90"
 END IF
!
!!END THE PROGRAM:
!
 END PROGRAM GFS_ESMF

!
!EOP
!-------------------------------------------------------------------------
