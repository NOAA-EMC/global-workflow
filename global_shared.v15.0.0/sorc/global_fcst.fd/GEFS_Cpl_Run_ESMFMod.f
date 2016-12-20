!
! !MODULE: GEFS_Cpl_Run_ESMFMod --- Run module of the ESMF grided
!                                   component of the GFS ensemble coupler.
!
! !DESCRIPTION: GEFS coupler run module.
!
! !REVISION HISTORY:
!
!  April    2006     Weiyu Yang Initial code.
!
!
! !INTERFACE:
!
 MODULE GEFS_Cpl_Run_ESMFMod
!
!!USES:
!
 USE ESMF_Mod
 USE GEFS_Cpl_InternalState_ESMFMod
 use mpi_def, only : mpi_real8

 IMPLICIT none

 CONTAINS

 SUBROUTINE GEFS_Cpl_Run(clock, cst, rc)

! This subroutine is used to create the new initial conditions for the 
! next GFS ensemble forecast run from the last GFS run inputs and outputs.
! The new created initial condition fields need to be put back. 
!-------------------------------------------------------------------------

 TYPE(ESMF_Clock),                      INTENT(inout) :: clock
 TYPE(GEFS_Cpl_InternalState), POINTER, INTENT(inout) :: cst
 INTEGER, OPTIONAL,                     INTENT(out)   :: rc
 REAL(KIND = kind_evod)                               :: rp1
 REAL(KIND=KIND_EVOD)                                 :: KEMAX(3)
 REAL(KIND=KIND_EVOD)                                 :: PARAM(4)

! !WORKING ARRAYS AND LOCAL PARAMETERS.
!--------------------------------------
 TYPE(ESMF_Time)                                      :: currTime
 TYPE(ESMF_TimeInterval)                              :: runDuration
 REAL(KIND=KIND_EVOD), DIMENSION(:),          POINTER :: trieo
 REAL(KIND=KIND_EVOD), DIMENSION(:),          POINTER :: trieo_ini
 REAL(KIND=KIND_EVOD), DIMENSION(:),          POINTER :: trie
 REAL(KIND=KIND_EVOD), DIMENSION(:),          POINTER :: trio
 REAL(KIND=KIND_EVOD), DIMENSION(:),          POINTER :: trie_ini
 REAL(KIND=KIND_EVOD), DIMENSION(:),          POINTER :: trio_ini
 INTEGER                                              :: hh
 INTEGER                                              :: rc1
 INTEGER                                              :: rcfinal
     
 INTEGER                                              :: i, j, k, l
 INTEGER                                              :: year, month, day, hour
 INTEGER                                              :: Jul_Day

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

!  Get the initial time and forecast hours
!-----------------------------------------------------
 CALL ESMF_ClockGet(clock, currTime = currTime, rc = rc1)
 IF(ESMF_LogMsgFoundError(rc1, "Get clock in the Cpl")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Getting currTime in the Cpl, rc = ', rc1
     rc1     = ESMF_SUCCESS
 END IF
 CALL ESMF_TimeGet(currTime, yy=year,mm=month,dd=day,h=hour,rc = rc1)

 CALL ESMF_ClockGet(clock, runDuration = runDuration, rc = rc1)
 IF(ESMF_LogMsgFoundError(rc1, "Get clock in the Cpl")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Getting runDuration in the Cpl, rc = ', rc1
     rc1     = ESMF_SUCCESS
 END IF
 CALL ESMF_TimeIntervalGet(runDuration, h = hh,       rc = rc1)

!  calculate the stochastic combination coefficient
!-----------------------------------------------------
!CALL Cal_Sto_Coef(cst%Total_member,cst%Cpl_Run_Calling_Number,cst%Sto_Coef,year,month,day,hour,hh,0.005,10,0.005,10) 
 print *, 'DHOU, Calling_N and calling_S ', cst%Cpl_Run_Calling_Number, cst%Cpl_Run_Calling_Start 
 if ( cst%Cpl_Run_Calling_Number .eq. cst%Cpl_Run_Calling_Start ) then
CALL Cal_Sto_Coef(cst%Total_member,0,cst%Cpl_Run_Calling_Number,   &
    cst%hh_increase,cst%Sto_Coef,year,month,day,hour,hh,           &
    cst%PARM1(1), cst%PARM1_i(2), cst%PARM1(3), cst%PARM1_i(4)) 
 elseif ( cst%Cpl_Run_Calling_Number .gt. cst%Cpl_Run_Calling_Start ) then
CALL Cal_Sto_Coef(cst%Total_member,cst%Cpl_Run_Calling_Number-1,   &
    cst%Cpl_Run_Calling_Number,cst%hh_increase,cst%Sto_Coef,year,  &
    month,day,hour,hh,                                             &
    cst%PARM1(1), cst%PARM1_i(2), cst%PARM1(3), cst%PARM1_i(4))
 else
print *,'DHOU, ERROR in CPL_RUN/Cal_Sto_Coef,STOPPED'
stop
 endif

! Adjust the clock for the next stage of forecast run.
!-----------------------------------------------------
 if ( cst%Cpl_Run_Calling_Number .lt. cst%Cpl_Run_Calling_Final ) then
 hh = hh + cst%hh_increase
 endif
 CALL ESMF_TimeIntervalSet(runDuration, h = hh,       rc = rc1)
 CALL ESMF_ClockSet(clock, runDuration = runDuration, rc = rc1)

 IF(ESMF_LogMsgFoundError(rc1, "Adjust clock in the Cpl")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Adjusting clock in the Cpl, rc = ', rc1
     rc1     = ESMF_SUCCESS
 END IF

! Print to check the model state variables before adding pert. KE will be printed.
!CALL mpi_alltoall(cst%trieo_ls,  cst%TRIEO_LSTOT_SIZ2, MPI_REAL8, &
!                  cst%trieo_ls_max, cst%TRIEO_LSTOT_SIZ2, MPI_REAL8, &
!                  cst%mpi_comm_cplcomp, rc1)
! PRINT*, 'Start calling STEP2_m for STATE before Pert' 
!CALL GEFS_Sto_Per_Scheme_Step2(cst, 1, 1, 2, 0, 1.0, KEMAX, 3, 0.34, -0.34, rc1)
! PRINT*, 'Finished calling STEP2_m for STATE before Pert' 
!CALL mpi_alltoall(cst%trieo_ls_max,cst%TRIEO_LSTOT_SIZ2, MPI_REAL8,  &
!                  cst%trieo_ls, cst%TRIEO_LSTOT_SIZ2, MPI_REAL8,  &
!                  cst%mpi_comm_cplcomp, rc1)

! Put computations at here to create the new initial conditions.
!---------------------------------------------------------------
! DHOU, 09/24/2007, add rp1 for the global rescaling factor, linear function of lead time
 
!if ( cst%Cpl_Run_Calling_Number .lt. 20 ) then
! rp1=0.1
!elseif ( cst%Cpl_Run_Calling_Number .gt. 63 ) then
! rp1=0.02
!else
!rp1 = (0.1-0.02)*(63-cst%Cpl_Run_Calling_Number)/(63.0 - 20.0) + 0.02
!endif

!DHOU 06-16-2008 Logistic function to replace linear function
!DHOU 06-30-2008 Use a standard form of logistic function with 4 parameters 
!DHOU 09-22-2008 fine tuning of the parameters 
!DHOU for T190 truncated to T126 at 180h
!PARAM(1) = 0.105 
!PARAM(2) = 0.03  
!PARAM(3) = 0.12
!PARAM(4) = 42.0
!DHOU for T190 truncated to T126 at 384h
! PARAM(1) = 0.100 
! PARAM(2) = 0.01  
! PARAM(3) = 0.11
! PARAM(4) = 42.0

 rp1 = 1.0 -1.0 / ( 1.0 + exp ( -1.0 * cst%PARM2(3) * &
     ( float(cst%Cpl_Run_Calling_Number) - cst%PARM2(4) ) ) )  !Az
 rp1 = rp1 * ( cst%PARM2(1) - cst%PARM2(2) ) + cst%PARM2(2)

 rp1=-1.0*rp1    


KEMAX(1)=1001.1375*0.7
KEMAX(2)=1000.079625*1.0
KEMAX(3)=1001.1375*1.0

 CALL Sto_Per_Scheme_Step1(cst%trieo_ls_ini, cst%trieo_ls, cst%trieo_ls_w1, &
                           cst%TRIEO_LSTOT_SIZ4, cst%Total_member,          &
                           cst%Cpl_Run_Calling_Number,                      &
                           cst%Sto_Coef,1)

!!IF (ABS(rp1).gt.0.999.and.ABS(rp1).lt.1.001) THEN

!DHOU, 12/10/2007, Added Jul_day (Jullian Day) as a new argument of Step2.
 CALL Cal_Jul_Day(Jul_Day,year,month,day,hour,hh)

 CALL mpi_alltoall(cst%trieo_ls_w1,  cst%TRIEO_LSTOT_SIZ2, MPI_REAL8, &
                   cst%trieo_ls_max, cst%TRIEO_LSTOT_SIZ2, MPI_REAL8, &
                   cst%mpi_comm_cplcomp, rc1)

  PRINT*, 'Start calling STEP2_2' 
 CALL GEFS_Sto_Per_Scheme_Step2(cst, 1, 1, 2, 0, rp1, Jul_Day, KEMAX, &
      cst%PARM3_i(4), cst%PARM3(2), cst%PARM3(3), rc1)
  PRINT*, 'Finished calling STEP2_2' 
  PRINT*, 'Start calling STEP2_1' 
 CALL GEFS_Sto_Per_Scheme_Step2(cst, 1, 1, 1, 0, rp1, Jul_Day, KEMAX, &
      cst%PARM3_i(4), cst%PARM3(2), cst%PARM3(3), rc1)
  PRINT*, 'Finished calling STEP2_1' 

!DHOU  09/26/2007 Added Argument  6 i(rp1 for global rescaling factor)
! for the GEFS_Sto_Per_Scheme_Step2_m subroutine.
!DHOU  09/11/2007 Added Arguments for the GEFS_Sto_Per_Scheme_Step2_m subroutine.
! Arguments 2,3,4 and 5 are as follows
!ZEROS03, 0=No, 1=YES; Zero out arrays gz, dpdlam, dpdphi, uln, and vln as well as State 3
!S1USE, 0=Zero-out, 1=no-change, 2=replaced with S2; 3=replace s1 and s2 with 0.5(s1+s2);
!      For arrays related to state 1,zem to qm.
!GRIDUSE, 1=S1, 2=S2; Convert the state S1/S2 from spectral to grid and  and back to spec.
!QADJUST, 0=NO, 1=YES; Adjust the q array so that q>=0 everywhere.

 CALL mpi_alltoall(cst%trieo_ls_max,cst%TRIEO_LSTOT_SIZ2, MPI_REAL8,  &
                   cst%trieo_ls_w1, cst%TRIEO_LSTOT_SIZ2, MPI_REAL8,  &
                   cst%mpi_comm_cplcomp, rc1)

!!ENDIF

 CALL Sto_Per_Scheme_Step1_2(cst%trieo_ls, cst%trieo_ls_w1,                 &
                           cst%TRIEO_LSTOT_SIZ4, cst%Total_member,          &
                           cst%Cpl_Run_Calling_Number,1.0)                       
!                          cst%Cpl_Run_Calling_Number,rp1)                       

 CALL mpi_alltoall(cst%trieo_ls,     cst%TRIEO_LSTOT_SIZ2, MPI_REAL8, &
                   cst%trieo_ls_max, cst%TRIEO_LSTOT_SIZ2, MPI_REAL8, &
                   cst%mpi_comm_cplcomp, rc1)

! Print to check the model state variables after adding Pert. KE will be printed.
! PRINT*, 'Start calling STEP2 for STATE after Pert' 
!CALL GEFS_Sto_Per_Scheme_Step2(cst, 1, 1, 2, 0, 1.0, KEMAX, 3, 0.34, -0.34, rc1)
! PRINT*, 'Finished calling STEP2 for STATE after Pert1' 

 IF(ESMF_LogMsgFoundError(rc1, "Run Sto_Per_Scheme_Step2")) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Running Sto_Per_Scheme_Step2, rc = ', rc1
     rc1     = ESMF_SUCCESS
 END IF

 IF(PRESENT(rc)) THEN
     rc = rcfinal
 END IF

 END SUBROUTINE GEFS_Cpl_Run

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

 END MODULE GEFS_Cpl_Run_ESMFMod
