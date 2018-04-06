#include "../ESMFVersionDefine.h"

!
! !MODULE: ENS_Cpl_Run_ESMFMod --- Run module of the ESMF grided
!                                   component of the EARTH ensemble coupler.
!
! !DESCRIPTION: ENS coupler run module.
!
! !REVISION HISTORY:
!
!  April    2006     Weiyu Yang Initial code.
!  March    2009     Weiyu Yang, modified for the NEMS model.
!  May      2011     Weiyu yang, modified for using the ESMF 5.2.0r_beta_snapshot_07.
!
!
! !INTERFACE:
!
 MODULE ENS_Cpl_Run_ESMFMod
!
!!USES:
!
 USE ESMF
 USE ENS_Cpl_InternalState_ESMFMod

 IMPLICIT none

 INCLUDE 'mpif.h'

 CONTAINS

 SUBROUTINE ENS_Cpl_Run(impState, clock, cst, rc)

! This subroutine is used to create the new initial conditions for the 
! next EARTH ensemble forecast run from the last EARTH run inputs and outputs.
! The new created initial condition fields need to be put back. 
!-----------------------------------------------------------------------------

 TYPE(ESMF_State),                     INTENT(inout) :: impState
 TYPE(ESMF_Clock),                     INTENT(inout) :: clock
 TYPE(ENS_Cpl_InternalState), POINTER, INTENT(inout) :: cst
 INTEGER, OPTIONAL,                    INTENT(out)   :: rc



! !WORKING ARRAYS AND LOCAL PARAMETERS.
!--------------------------------------
 TYPE(ESMF_Time)                                     :: currTime
 TYPE(ESMF_TimeInterval)                             :: runDuration
 INTEGER                                             :: hh
 INTEGER                                             :: rc1
 INTEGER                                             :: rcfinal
     
 INTEGER                                             :: i, j, k, l
 INTEGER                                             :: year, month, day, hour
 INTEGER                                             :: Jul_Day
 CHARACTER(ESMF_MAXSTR)                              :: name

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

!  Get the initial time and forecast hours
!-----------------------------------------------------
 CALL ESMF_ClockGet(clock, currTime = currTime, rc = rc1)
 IF(ESMF_LogFoundError(rc1, msg="Get clock in the Cpl")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Getting currTime in the Cpl, rc = ', rc1
     rc1     = ESMF_SUCCESS
 END IF
 CALL ESMF_TimeGet(currTime, yy = year, mm = month, dd = day, h = hour, rc = rc1)

 CALL ESMF_ClockGet(clock, runDuration = runDuration, rc = rc1)
 IF(ESMF_LogFoundError(rc1, msg="Get clock in the Cpl")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Getting runDuration in the Cpl, rc = ', rc1
     rc1     = ESMF_SUCCESS
 END IF
 CALL ESMF_TimeIntervalGet(runDuration, h = hh,       rc = rc1)

!  calculate the stochastic combination coefficient
!-----------------------------------------------------
 IF(cst%Cpl_Run_Calling_Number == cst%Cpl_Run_Calling_Start) THEN
     CALL Cal_Sto_Coef(cst%Total_member, 0, cst%Cpl_Run_Calling_Number, cst%hh_increase, &
         cst%Sto_Coef, year, month, day, hour, hh, 0.005, 10, 0.005, 10)
 ELSE IF(cst%Cpl_Run_Calling_Number > cst%Cpl_Run_Calling_Start) THEN
     CALL Cal_Sto_Coef(cst%Total_member, cst%Cpl_Run_Calling_Number-1, cst%Cpl_Run_Calling_Number,&
         cst%hh_increase, cst%Sto_Coef, year, month, day, hour, hh, 0.005, 10, 0.005, 10)
 ELSE
     PRINT *,'DHOU, ERROR in CPL_RUN/Cal_Sto_Coef, STOPPED'
     STOP
 END IF

! Put computations at here to create the new initial conditions.
!---------------------------------------------------------------
! DHOU, 09/24/2007, add RS_GLOBAL for the global rescaling factor, linear function of lead time
 
!DHOU 06-16-2008 Logistic function to replace linear function
!DHOU 06-30-2008 Use a standard form of logistic function with 4 parameters 
!DHOU 09-22-2008 fine tuning of the parameters
!DHOU for T190 truncated to T126 at 180h
!PARAM(1) = 0.105
!PARAM(2) = 0.03
!PARAM(3) = 0.12
!PARAM(4) = 42.0
!DHOU for T190 truncated to T126 at 384h
 cst%PARAM(1) = 0.100
 cst%PARAM(2) = 0.01
 cst%PARAM(3) = 0.11
 cst%PARAM(4) = 42.0
 cst%RS_GLOBAL = 1.0 -1.0 / (1.0 + EXP (-1.0 * cst%PARAM(3) * (FLOAT(cst%Cpl_Run_Calling_Number) - cst%PARAM(4))))  !Az
 cst%RS_GLOBAL = cst%RS_GLOBAL * (cst%PARAM(1) - cst%PARAM(2)) + cst%PARAM(2)

 cst%RS_GLOBAL = -cst%RS_GLOBAL 

! For Kineitic Energy weighting computation.
!-------------------------------------------
 IF(.NOT. ASSOCIATED(cst%KEMAX)) ALLOCATE(cst%KEMAX(cst%nreg))
 cst%KEMAX(1) = 1001.1375   * 0.7
 cst%KEMAX(2) = 1000.079625 * 1.0
 cst%KEMAX(3) = 1001.1375   * 1.0

! Run stochastic perturbation scheme step1.
!------------------------------------------
 cst%CENTER = 1
 CALL ENS_Sto_Per_Scheme_Step1(cst)

 CALL DistributeBackFromStep1_1(cst%ps_step1,  cst%psw,  cst, rc1)
 CALL DistributeBackFromStep1_1(cst%psm_step1, cst%pswm, cst, rc1)

 name = "T"
 CALL DistributeBackFromStep1(cst%t_step1,    cst%tw,    name, cst, rc1)
 name = "TM"
 CALL DistributeBackFromStep1(cst%tm_step1,   cst%twm,   name, cst, rc1)
 name = "U"
 CALL DistributeBackFromStep1(cst%u_step1,    cst%uw,    name, cst, rc1)
 name = "UM"
 CALL DistributeBackFromStep1(cst%um_step1,   cst%uwm,   name, cst, rc1)
 name = "V"
 CALL DistributeBackFromStep1(cst%v_step1,    cst%vw,    name, cst, rc1)
 name = "VM"
 CALL DistributeBackFromStep1(cst%vm_step1,   cst%vwm,   name, cst, rc1)
 DO i = 1, cst%ntrac
     SELECT CASE(i)
         CASE(1)
             name = "q"
         CASE(2)
             name = "OZ"
         CASE(3)
             name = "CLW"
     END SELECT
     CALL DistributeBackFromStep1(cst%tracer_step1(:, :, i), &
         cst%tracerw(:, :, :, i), name, cst, rc1)
     SELECT CASE(i)
         CASE(1)
             name = "qM"
         CASE(2)
             name = "OZM"
         CASE(3)
             name = "CLWM"
     END SELECT
     CALL DistributeBackFromStep1(cst%tracerm_step1(:, :, i), &
         cst%tracerwm(:, :, :, i), name, cst, rc1)
 END DO

 IF(ESMF_LogFoundError(rc1, msg="Distribute Back For the step2 - CLWM")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Distributing Back For the step2 - CLWM, rc = ', rc1
     rc1     = ESMF_SUCCESS
 END IF

!DHOU, 12/10/2007, Added Jul_day (Jullian Day) as a new argument of Step2.
!-------------------------------------------------------------------------
 CALL Cal_Jul_Day(Jul_Day, year, month, day, hour, hh)

! Run stochastic perturbation scheme step2.
!------------------------------------------
 PRINT*, 'Start calling STEP2_2'

 CALL ENS_Sto_Per_Scheme_Step2(cst, 1, 2, Jul_Day, 0.34, -0.34, rc1)

 PRINT*, 'Finished calling STEP2_2'
 PRINT*, 'Start calling STEP2_1'

 CALL ENS_Sto_Per_Scheme_Step2(cst, 1, 1, Jul_Day, 0.34, -0.34, rc1)

 PRINT*, 'Finished calling STEP2_1'

 IF(ESMF_LogFoundError(rc1, msg="Run Sto_Per_Scheme_Step2")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Running Sto_Per_Scheme_Step2, rc = ', rc1
     rc1     = ESMF_SUCCESS
 END IF

! Run the final stepof the stochastic perturbation scheme.
!---------------------------------------------------------
 CALL ENS_Sto_Per_Scheme_Step1_2(impState, cst, rc1)

 IF(ESMF_LogFoundError(rc1, msg="Run Sto_Per_Scheme_Step1_2")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Running Sto_Per_Scheme_Step1_2, rc = ', rc1
     rc1     = ESMF_SUCCESS
 END IF

 IF(rcfinal == ESMF_SUCCESS) THEN
     PRINT*, "PASS: ENS_Cpl_Run."
 ELSE
     PRINT*, "FAIL: ENS_Cpl_Run."
 END IF

 IF(PRESENT(rc)) THEN
     rc = rcfinal
 END IF

 END SUBROUTINE ENS_Cpl_Run





 SUBROUTINE Cal_Jul_Day(Jul_Day,year,month,day,hour,hh)
 INTEGER Jul_Day,year,month,day,hour,hh,m 
!year,month,day and hour: for the time at which the forecast is valid. 
!hh is the forecast lead time in hours, at NEXT stop of integration.
 INTEGER dd(12)
 DATA dd/31,28,31,30,31,30,31,31,30,31,30,31/
 Jul_Day=day
 DO m=1,month-1
 Jul_Day=Jul_Day+dd(m)
 ENDDO
 IF (Jul_Day .gt. 365) Jul_Day=Jul_Day-365
 PRINT *, 'SS=',Jul_Day,year,month,day,hour,hh
 RETURN
 END SUBROUTINE Cal_Jul_Day

 END MODULE ENS_Cpl_Run_ESMFMod
