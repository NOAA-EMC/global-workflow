!----------------------------------------------------------------------
! !MODULE: GEFS_CplComp_ESMFMod
!        --- ESMF coupler gridded component of the GFS Ensemble 
!            Forecast Operational system. 
!
! !DESCRIPTION: GFS coupler gridded component main module.
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
!  Octorber 2013      Weiyu Yang, modified for using the LIOPE = .true. option  on GEFS run.
!  December 2015      C. Redder  Removed intent attribute of the first
!                       argument of the subroutine GEFS_CplCompSetServices
!                       to resolve compiler errors and conform to ANSI
!                       standard fortran and to the interface of a dummy
!                       procedure for a ESMF library routine.

! !INTERFACE:
!
 MODULE GEFS_CplComp_ESMFMod
 
!!USES:
!------

! Define the ESMF internal state and all routines related to run 
! the GFS ensemble coupler grid component.
!---------------------------------------------------------------
 USE GEFS_Cpl_ESMFMod

 IMPLICIT none

!#include "ESMF_LogMacros.inc"


 PRIVATE   ! By default data is private to this module
!
! !PUBLIC TYPES:
!---------------

 PUBLIC GEFS_CplCompSetServices

!EOP
!-------------------------------------------------------------------------


 CONTAINS


!----------------------------------------------------------------------
!BOP
!
! !ROUTINE: GEFS_CplCompSetServices --- Set services for GFS Ensemble 
!                                       Coupler Gridded Component.
! 
! !INTERFACE:
!
 SUBROUTINE GEFS_CplCompSetServices(CplGEFS, rc)

! !ARGUMENTS:
!------------

! TYPE(ESMF_CplComp),  INTENT(inout) :: CplGEFS ! gridded component
 TYPE(ESMF_CplComp)                 :: CplGEFS ! gridded component
 INTEGER,             INTENT(out)   :: rc      ! return code
     
! !DESCRIPTION: Set services (register) for the GFS Ensemble Coupler
!               Grid Component.
!         
!EOP         
!----------------------------------------------------------------------

 INTEGER                            :: rc1     = ESMF_SUCCESS
 rc = ESMF_SUCCESS

! REGISTER SERVICES FOR THIS COMPONENT
! ------------------------------------

     CALL ESMF_LogWrite("Set Entry Point for Cpl Initialize",             &
                        ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_CplCompSetEntryPoint (CplGEFS, ESMF_SETINIT,  Cpl_Initialize, &
                                 ESMF_SINGLEPHASE, rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Set Entry Point for Cpl Initialize")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Setting the Entry Point for Cpl Initialize, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     CALL ESMF_LogWrite("Set Entry Point for Cpl Run",                    &
                        ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_CplCompSetEntryPoint (CplGEFS, ESMF_SETRUN,   Cpl_Run,        &
                                 ESMF_SINGLEPHASE, rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Set Entry Point for Cpl Run")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Setting the Entry Point for Cpl Run, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

     CALL ESMF_LogWrite("Set Entry Point for Cpl Finalize",               &
                        ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_CplCompSetEntryPoint (CplGEFS, ESMF_SETFINAL, Cpl_Finalize,   &
                                 ESMF_SINGLEPHASE, rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Set Entry Point for Cpl Finalize")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Setting the Entry Point for Cpl Finalize, rc = ', rc1
     END IF

 IF(rc /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: GEFS_CplCompSetServices."
!ELSE
!    PRINT*, "PASS: GEFS_CplCompSetServices."
 END IF

 END SUBROUTINE GEFS_CplCompSetServices





!----------------------------------------------------------------------
!BOP
! !ROUTINE:  Cpl_Initialize --- initialize routine to initialize 
!                               and set up the GFS ensemble coupler.
!
! !DESCRIPTION: This subroutine initializes the GFS ensemble coupler
!               before the main running routine.
!
!
! !REVISION HISTORY:
!
!  April    2006     Weiyu Yang Initial code.
!  December 2015     C. Redder  Removed intent attribute of the first
!                       subroutine arguments to resolve compiler errors
!                       and conform to ANSI standard fortran and to the
!                       interface of the dummy procedure for the
!                       routine,  ESMF_GridCompSetEntryPoint (called
!                       above)

!
! !INTERFACE:
!

 SUBROUTINE Cpl_Initialize(CplGEFS, impGEFS, expGEFS, clock, rcfinal)

 USE GEFS_GetParameterFromStateMod
 
!
! !INPUT/OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------------

! TYPE(ESMF_CplComp), INTENT(inout)     :: CplGEFS 
! TYPE(ESMF_State),   INTENT(inout)     :: impGEFS
! TYPE(ESMF_State),   INTENT(inout)     :: expGEFS
! TYPE(ESMF_Clock),   INTENT(inout)     :: clock
 TYPE(ESMF_CplComp) :: CplGEFS 
 TYPE(ESMF_State)   :: impGEFS
 TYPE(ESMF_State)   :: expGEFS
 TYPE(ESMF_Clock)   :: clock

!
! !OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------

 INTEGER,             INTENT(out)       :: rcfinal

! !EOP
!------------------------------------------------------------------------- 
 
! !WORKING ARRAYS AND LOCAL PARAMETERS.
!--------------------------------------
 TYPE(ESMF_VM)                          :: vm
 TYPE(GEFS_Cpl_wrap)                    :: wrap
 TYPE(GEFS_Cpl_InternalState), POINTER  :: Cpl_Int_State
 TYPE(ESMF_Config)                      :: Cf
 CHARACTER(ESMF_MAXSTR)                 :: Cf_fname
 CHARACTER(12)                          :: PELAB

 INTEGER                                :: i, j, k, l
 INTEGER                                :: rc1 

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

!These are the standard ESMF internal state lines.
!-------------------------------------------------
     CALL ESMF_LogWrite("Allocate the Cpl Internal State",                &
                    ESMF_LOG_INFO, rc = rc1)

 ALLOCATE(Cpl_Int_State, stat = rc1)

     IF(ESMF_LogMsgFoundAllocError(rc1, " - Allocate the Cpl Internal State")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Allocating the Cpl Internal State, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 wrap%Cpl_Int_State => Cpl_Int_State

! Attach internal state to the GFS Ensemble Coupler grid component.
!------------------------------------------------------------------
     CALL ESMF_LogWrite("Set Up the GFS Ensemble coupler Internal State", &
                        ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_CplCompSetInternalState(CplGEFS, wrap, rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Set Up the  GFS Ensemble coupler Internal State")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Setting Up the  GFS Ensemble coupler Internal State, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Get the coupler grid component vm.
!-----------------------------------
     CALL ESMF_LogWrite("Get the GLobal VM", ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_VMGetCurrent(vm, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Get the VM")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting the VM, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Set up parameters of MPI communications.
! Use ESMF utility to get PE identification and total number of PEs.
!-------------------------------------------------------------------
     CALL ESMF_LogWrite("Get me and NODES from VM", ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_VMGet(vm, localPet = Cpl_Int_State%me,    &
                     petCount = Cpl_Int_State%nodes, &
                     mpiCommunicator = Cpl_Int_State%mpi_comm_cplcomp, &
                     rc       = rc1)
 Cpl_Int_State%mm1 = Cpl_Int_State%me + 1

 Cpl_Int_State%jintmx = 0

     IF(ESMF_LogMsgFoundError(rc1, "Get me and NODES from VM")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting me and NODES from VM, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Set up parameters from the configure file.
!-------------------------------------------
     CALL ESMF_LogWrite("Get parameters from the configure file", &
                         ESMF_LOG_INFO, rc = rc1)

 Cf       = ESMF_ConfigCreate(rc = rc1)
 Cf_fname = 'gfs_namelist.rc'

 CALL ESMF_ConfigLoadFile(Cf, Cf_fname, rc = rc1)

 CALL ESMF_ConfigGetAttribute(Cf,                         &
                              Cpl_Int_State%Total_member, &
                              label = 'TOTAL_MEMBER:',    &
                              rc    = rc1)
 CALL ESMF_ConfigGetAttribute(Cf,                         &
                              Cpl_Int_State%hh_increase,  &
                              label = 'HH_INCREASE:',     &
                              rc    = rc1)
 CALL ESMF_ConfigGetAttribute(Cf,                         &
                              Cpl_Int_State%hh_start,     &
                              label = 'HH_START:',        &
                              rc    = rc1)
 CALL ESMF_ConfigGetAttribute(Cf,                         &
                              Cpl_Int_State%hh_final,     &
                              label = 'HH_FINAL:',        &
                              rc    = rc1)

 ALLOCATE(Cpl_Int_State%TRIEO_LS_SIZE(Cpl_Int_State%nodes))
 ALLOCATE(Cpl_Int_State%TRIEO_ST_SIZE(Cpl_Int_State%nodes))

 CALL ESMF_ConfigGetAttribute(Cf,                         &
                              Cpl_Int_State%PARM1,        &
                              10,                         &
                              label = 'SPS_PARM1:',       &
                              rc    = rc1)
 CALL ESMF_ConfigGetAttribute(Cf,                         &
                              Cpl_Int_State%PARM2,        &
                              10,                         &
                              label = 'SPS_PARM2:',       &
                              rc    = rc1)
 CALL ESMF_ConfigGetAttribute(Cf,                         &
                              Cpl_Int_State%PARM3,        &
                              10,                         &
                              label = 'SPS_PARM3:',       &
                              rc    = rc1)

 DO i = 1, 10
     Cpl_Int_State%PARM1_i(i) = NINT(Cpl_Int_State%PARM1(i))
     Cpl_Int_State%PARM2_i(i) = NINT(Cpl_Int_State%PARM2(i))
     Cpl_Int_State%PARM3_i(i) = NINT(Cpl_Int_State%PARM3(i))
 END DO

 CALL ESMF_ConfigDestroy(Cf, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Get parameters from the configure file")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting parameters from the configure file, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Get the TRIE and TRIO size information from the import state.
!--------------------------------------------------------------
     CALL ESMF_LogWrite("Get size information of trieo from impGEFS", &
                        ESMF_LOG_INFO, rc = rc1)

 CALL GEFS_GetTrieoSize(CplGEFS, impGEFS, expGEFS, Cpl_Int_State, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Get size information of trieo from impGEFS")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When getting size information of trieo from impGEFS, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Allocate trieo arrays.
!-----------------------
 ALLOCATE(Cpl_Int_State%trieo_ls_max(Cpl_Int_State%TRIEO_LSTOT_SIZ3))
 ALLOCATE(Cpl_Int_State%trieo_ls    (Cpl_Int_State%TRIEO_LSTOT_SIZ3))
 ALLOCATE(Cpl_Int_State%trieo_ls_ini(Cpl_Int_State%TRIEO_LSTOT_SIZ3))
 ALLOCATE(Cpl_Int_State%trieo_ls_w1 (Cpl_Int_State%TRIEO_LSTOT_SIZ3))
!ALLOCATE(Cpl_Int_State%trieo_ls_w2 (Cpl_Int_State%TRIEO_LSTOT_SIZ3))
!ALLOCATE(Cpl_Int_State%trieo_ls_w1 (Cpl_Int_State%TRIEO_LSTOT_SIZ3   &
!                                  - Cpl_Int_State%TRIEO_LSTOT_SIZ4)) 
!ALLOCATE(Cpl_Int_State%trieo_ls_w2 (Cpl_Int_State%TRIEO_LSTOT_SIZ3   &
!                                  - Cpl_Int_State%TRIEO_LSTOT_SIZ4)) 

! Get required parameters from the ESMF import state to do the spectral 
! transform processes for the second step of the stochastic perturbation 
! scheme.
!-----------------------------------------------------------------------
 CALL GEFS_GetParameterFromState(impGEFS, Cpl_Int_State, rc = rc1)

! Set up the GFS vertical variable distribution.
!-----------------------------------------------

! In GFS, the vertical variable distribution is:
!-----------------------------------------------
!      parameter(P_gz  = 0*levs+0*levh+1,  !      gze/o(lnte/od,2),
!                P_zem = 0*levs+0*levh+2,  !     zeme/o(lnte/od,2,levs),
!                P_dim = 1*levs+0*levh+2,  !     dime/o(lnte/od,2,levs),
!                P_tem = 2*levs+0*levh+2,  !     teme/o(lnte/od,2,levs),
!                P_rm  = 3*levs+0*levh+2,  !      rme/o(lnte/od,2,levh),
!                P_qm  = 3*levs+1*levh+2,  !      qme/o(lnte/od,2),
!                P_ze  = 3*levs+1*levh+3,  !      zee/o(lnte/od,2,levs),
!                P_di  = 4*levs+1*levh+3,  !      die/o(lnte/od,2,levs),
!                P_te  = 5*levs+1*levh+3,  !      tee/o(lnte/od,2,levs),
!                P_rq  = 6*levs+1*levh+3,  !      rqe/o(lnte/od,2,levh),
!                P_q   = 6*levs+2*levh+3,  !       qe/o(lnte/od,2),
!                P_dlam= 6*levs+2*levh+4,  !  dpdlame/o(lnte/od,2),
!                P_dphi= 6*levs+2*levh+5,  !  dpdphie/o(lnte/od,2),
!                P_uln = 6*levs+2*levh+6,  !     ulne/o(lnte/od,2,levs),
!                P_vln = 7*levs+2*levh+6,  !     vlne/o(lnte/od,2,levs),
!                P_w   = 8*levs+2*levh+6,  !       we/o(lnte/od,2,levs),
!                P_x   = 9*levs+2*levh+6,  !       xe/o(lnte/od,2,levs),
!                P_y   =10*levs+2*levh+6,  !       ye/o(lnte/od,2,levs),
!                P_rt  =11*levs+2*levh+6,  !      rte/o(lnte/od,2,levh),
!                P_zq  =11*levs+3*levh+6)  !      zqe/o(lnte/od,2)

 Cpl_Int_State%P_gz   =  0 * Cpl_Int_State%levs + 0 * Cpl_Int_State%levh + 1
 Cpl_Int_State%P_zem  =  0 * Cpl_Int_State%levs + 0 * Cpl_Int_State%levh + 2
 Cpl_Int_State%P_dim  =  1 * Cpl_Int_State%levs + 0 * Cpl_Int_State%levh + 2
 Cpl_Int_State%P_tem  =  2 * Cpl_Int_State%levs + 0 * Cpl_Int_State%levh + 2
 Cpl_Int_State%P_rm   =  3 * Cpl_Int_State%levs + 0 * Cpl_Int_State%levh + 2
 Cpl_Int_State%P_qm   =  3 * Cpl_Int_State%levs + 1 * Cpl_Int_State%levh + 2
 Cpl_Int_State%P_ze   =  3 * Cpl_Int_State%levs + 1 * Cpl_Int_State%levh + 3
 Cpl_Int_State%P_di   =  4 * Cpl_Int_State%levs + 1 * Cpl_Int_State%levh + 3
 Cpl_Int_State%P_te   =  5 * Cpl_Int_State%levs + 1 * Cpl_Int_State%levh + 3
 Cpl_Int_State%P_rq   =  6 * Cpl_Int_State%levs + 1 * Cpl_Int_State%levh + 3
 Cpl_Int_State%P_q    =  6 * Cpl_Int_State%levs + 2 * Cpl_Int_State%levh + 3
 Cpl_Int_State%P_dlam =  6 * Cpl_Int_State%levs + 2 * Cpl_Int_State%levh + 4
 Cpl_Int_State%P_dphi =  6 * Cpl_Int_State%levs + 2 * Cpl_Int_State%levh + 5
 Cpl_Int_State%P_uln  =  6 * Cpl_Int_State%levs + 2 * Cpl_Int_State%levh + 6
 Cpl_Int_State%P_vln  =  7 * Cpl_Int_State%levs + 2 * Cpl_Int_State%levh + 6
 Cpl_Int_State%P_w    =  8 * Cpl_Int_State%levs + 2 * Cpl_Int_State%levh + 6
 Cpl_Int_State%P_x    =  9 * Cpl_Int_State%levs + 2 * Cpl_Int_State%levh + 6
 Cpl_Int_State%P_y    = 10 * Cpl_Int_State%levs + 2 * Cpl_Int_State%levh + 6
 Cpl_Int_State%P_rt   = 11 * Cpl_Int_State%levs + 2 * Cpl_Int_State%levh + 6
 Cpl_Int_State%P_zq   = 11 * Cpl_Int_State%levs + 3 * Cpl_Int_State%levh + 6

!DHOU, 11/13/2007 assignement of latmax (Number of Gaussian latitudes following 
! setlonsgg subroutine in GFS_Initialize_ESMFMod.f)

!DHOU 09/06/2013 copied the following line from new-gfs
 Cpl_Int_State%latmax = max(Cpl_Int_State%latgd,Cpl_Int_State%latr)
 print *,' Cpl_Int_State%latmax=', Cpl_Int_State%latmax

!if (Cpl_Int_State%jcap .eq. 62) then
! Cpl_Int_State%latmax = 94
!endif
!if (Cpl_Int_State%jcap .eq. 126) then
! Cpl_Int_State%latmax = 190
!endif
!if (Cpl_Int_State%jcap .eq. 170) then
! Cpl_Int_State%latmax = 256
!endif
!if (Cpl_Int_State%jcap .eq. 190) then
! Cpl_Int_State%latmax = 288
!endif
!if (Cpl_Int_State%jcap .eq. 254) then
! Cpl_Int_State%latmax = 384
!endif
!if (Cpl_Int_State%jcap .eq. 382) then
! Cpl_Int_State%latmax = 576
!endif
!if (Cpl_Int_State%jcap .eq. 510) then
! Cpl_Int_State%latmax = 766
!endif
!if (Cpl_Int_State%jcap .eq. 574) then
!#Cpl_Int_State%latmax = 880
! Cpl_Int_State%latmax = 576
!endif
!DHOU, 09/25/2007 set up the Gaussian latitude (sin of it) slat_work and wlat_work  array
 ALLOCATE(Cpl_Int_State%slat_work(Cpl_Int_State%latmax))
 ALLOCATE(Cpl_Int_State%wlat_work(Cpl_Int_State%latmax))
 CALL SPLAT(4,Cpl_Int_State%latmax,Cpl_Int_State%slat_work, Cpl_Int_State%wlat_work)

!DHOU, 11/13/2007 assignement of latnode (Number of Gaussian latitudes per cpu) from other variables 
 Cpl_Int_State%latnode = Cpl_Int_State%latmax / (Cpl_Int_State%nodes / Cpl_Int_State%Total_member) + 1
!Cpl_Int_State%latnode = 32

!DHOU, 09/25/2007
! Set up the global array for the lats_node local variable (lats_node_global, demension 90 for 90cpu)
! this GLOBAL array contains the number of latitudes in each cpu
! The values are 31 or 32 for (T126, 15 members, 90 cpus 190/(90/15)=31.666)

 ALLOCATE(Cpl_Int_State%lats_node_global(Cpl_Int_State%nodes))

!populate the Cpl_Int_State%lats_node_global array by assignment at one cpu and BROADCASTING to all cpus. 

 DO i = 1, Cpl_Int_State%nodes
     IF(Cpl_Int_State%mm1 == i) Cpl_Int_State%lats_node_global(i) = Cpl_Int_State%lats_node
     CALL GEFS_bcst_global_i4(Cpl_Int_State%lats_node_global(i), i - 1, rc1)
 END DO
!    PRINT*, 'INIT_BCST lats_node_global = ', Cpl_Int_State%lats_node_global

! Set up the global lats_global array. (lats_global (32,90) for 90cpu, 15 members) 
! this GLOBAL array contains the index of the latitudes in each latitude of each cpu  
! (in global order, value between 1 and 190 for T126)
! Or the other hand, the lats_node_global(:) array takes value between 1 and 31/32.
!-----------------------------------
 ALLOCATE(Cpl_Int_State%lats_global(Cpl_Int_State%latnode,Cpl_Int_State%nodes))

!populate the Cpl_Int_State%lats_global array by assignment at one cpu and BROADCASTING to all cpus. 

 DO i = 1, Cpl_Int_State%nodes
   DO j = 1, Cpl_Int_State%lats_node_global(i) 
    IF(Cpl_Int_State%mm1 == i)                                      & 
    Cpl_Int_State%lats_global(j,i) = Cpl_Int_State%global_lats(Cpl_Int_State%ipt_lats_node - 1 + j)
    CALL GEFS_bcst_global_i4(Cpl_Int_State%lats_global(j,i), i - 1, rc1)
   END DO
 END DO

!Set up the Cpl_Int_State%factor1_work array and initialize it with values of 1.0.
 Cpl_Int_State%nreg=3
 ALLOCATE(Cpl_Int_State%factor1_work(Cpl_Int_State%nreg,Cpl_Int_State%Total_member))
 DO i=1,Cpl_Int_State%nreg
   DO j=1,Cpl_Int_State%Total_member
        Cpl_Int_State%factor1_work(i,j)=1.0
   ENDDO
 ENDDO

! Set up the member_id global arraycw and populate it by local assignments.
! This is a GLOBAL array,specifying the ens_member for processed by each cpu
!----------------------
 ALLOCATE(Cpl_Int_State%member_id(Cpl_Int_State%nodes))
 i = Cpl_Int_State%nodes / Cpl_Int_State%Total_member
 DO k = 1, Cpl_Int_State%Total_member
     DO j = 1, i
         Cpl_Int_State%member_id((k - 1) * i + j) = k
     END DO
 END DO
!PRINT*, 'INIT_BCST member_id = ', Cpl_Int_State%member_id

! Initialize the number of times of calling the coupler run routine and the combination coefficients.
!-------------------------------------------------------------------
!Cpl_Int_State%Cpl_Run_Calling_Number = 0
 Cpl_Int_State%Cpl_Run_Calling_Number = Cpl_Int_State%hh_start / Cpl_Int_State%hh_increase + 1
 Cpl_Int_State%Cpl_Run_Calling_Start  = Cpl_Int_State%hh_start / Cpl_Int_State%hh_increase + 1
 Cpl_Int_State%Cpl_Run_Calling_Final  = Cpl_Int_State%hh_final / Cpl_Int_State%hh_increase
 ALLOCATE(Cpl_Int_State%Sto_Coef(Cpl_Int_State%Total_member-1,Cpl_Int_State%Total_member-1))
 Cpl_Int_State%Sto_Coef = 0.0

! 09/04/2007, W. Yang and D. Hou
! These arrays willbe used in GEFS_Sto_Per_Scheme_Step2 subroutine and  tested for allocation at each
! application. Nullify them to be consistent with the IF (.NOT.ASSOCIATED) statements in that subroutine

 NULLIFY (Cpl_Int_State%trie_ls)
 NULLIFY (Cpl_Int_State%trio_ls)
 NULLIFY (Cpl_Int_State%four_gr1)
 NULLIFY (Cpl_Int_State%four_gr2)
 NULLIFY (Cpl_Int_State%vor   )
 NULLIFY (Cpl_Int_State%div   )
 NULLIFY (Cpl_Int_State%t     )
 NULLIFY (Cpl_Int_State%q     )
 NULLIFY (Cpl_Int_State%oz    )
 NULLIFY (Cpl_Int_State%clw   )
 NULLIFY (Cpl_Int_State%u     )
 NULLIFY (Cpl_Int_State%v     )
 NULLIFY (Cpl_Int_State%ps    )
 NULLIFY (Cpl_Int_State%dpdlam)
 NULLIFY (Cpl_Int_State%dpdphi)

 IF(rcfinal /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: Cpl_Initialize."
!ELSE
!    PRINT*, "PASS: Cpl_Initialize."
 END IF

 END SUBROUTINE Cpl_Initialize





!----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Cpl_Run --- Main grid component routine to run the GFS 
!                       ensemble coupler.
!
! !DESCRIPTION: This subroutine will run the most part computations 
!               of the GFS ensemble coupler.
!
! !REVISION HISTORY:
!
!  April    2006     Weiyu Yang Initial code.
!  December 2015     C. Redder  Removed intent attribute of the first
!                       subroutine arguments to resolve compiler errors
!                       and conform to ANSI standard fortran and to the
!                       interface of the dummy procedure for the
!                       routine,  ESMF_GridCompSetEntryPoint (called
!                       above)
!
! !INTERFACE:
!

 SUBROUTINE Cpl_Run(CplGEFS, impGEFS, expGEFS, clock, rcfinal)

!
! !INPUT VARIABLES AND PARAMETERS:
!---------------------------------
! TYPE(ESMF_CplComp), INTENT(inout)     :: CplGEFS   
! TYPE(ESMF_State),   INTENT(in)        :: impGEFS 
 TYPE(ESMF_CplComp) :: CplGEFS   
 TYPE(ESMF_State)   :: impGEFS 
 
! !OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------
! TYPE(ESMF_Clock)    INTENT(inout)     :: clock
! TYPE(ESMF_State)    INTENT(inout)     :: expGEFS
 TYPE(ESMF_Clock)   :: clock
 TYPE(ESMF_State)   :: expGEFS
 INTEGER,            INTENT(out)       :: rcfinal 
!
!EOP
!-------------------------------------------------------------------------

!
! !WORKING ARRAYS AND LOCAL PARAMETERS.
!--------------------------------------
 TYPE(ESMF_VM)                         :: vm
 TYPE(GEFS_Cpl_wrap)                   :: wrap
 TYPE(GEFS_Cpl_InternalState), POINTER :: Cpl_Int_State
 INTEGER                               :: rc1

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

! Retrieve the ESMF internal state.
!---------------------------------- 
     CALL ESMF_LogWrite("Get the Internal State in the Cpl Run Routine", &
                        ESMF_LOG_INFO, rc = rc1)
 CALL ESMF_CplCompGetInternalState(CplGEFS, wrap, rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Get the Internal State in the Cpl Run Routine")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting the Internal State in Cpl Run Routine, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 Cpl_Int_State => wrap%Cpl_Int_State

 CALL ESMF_VMGetCurrent(vm, rc = rc1)
 CALL ESMF_VMBarrier  (vm, rc = rc1)

! Update the number of times of calling the coupler run routine.
!---------------------------------------------------------------
!Cpl_Int_State%Cpl_Run_Calling_Number = Cpl_Int_State%Cpl_Run_Calling_Number + 1

! Transfer the GFS export fields to the working arrays in the Cpl internal state.
!--------------------------------------------------------------------------------
     CALL ESMF_LogWrite("ESMF import State to the Cpl Internal State", &
                        ESMF_LOG_INFO, rc = rc1)

 CALL GEFS_Cpl_ESMFImportState2InternalState(CplGEFS, impGEFS, Cpl_Int_State, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "ESMF import State to the Cpl Internal State")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Moving the ESMF import State to the Cpl Internal State, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

!Run the GEFS_Cpl_Run, to create new initial conditions for the next ensemble run.
!---------------------------------------------------------------------------------
     CALL ESMF_LogWrite("Run the GEFS_Cpl_Run", ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_VMBarrier(vm, rc = rc1)
 CALL GEFS_Cpl_Run(clock, Cpl_Int_State, rc = rc1)
 Cpl_Int_State%Cpl_Run_Calling_Number = Cpl_Int_State%Cpl_Run_Calling_Number + 1

!Note that hh=hh+hh_increase is done inside GEFS_Cpl_Run.

     IF(ESMF_LogMsgFoundError(rc1, "Run the GEFS_Cpl_Run")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Running the GEFS_Cpl_Run, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Transfer the new initial conditions to the ESMF Cpl exprot state.
!------------------------------------------------------------------
     CALL ESMF_LogWrite("Cpl Internal State to ESMF Export State", &
                        ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_VMBarrier(vm, rc = rc1)
 CALL GEFS_Cpl_InternalState2ESMFExportState(Cpl_Int_State)

     IF(ESMF_LogMsgFoundError(rc1, "Cpl Internal State to ESMF Export State")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Cpl Setting up the ESMF Export State, rc = ', rc1
     END IF

!End of the grid component run.
!------------------------------
 IF(rcfinal /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: Cpl_Run."
!ELSE
!    PRINT*, "PASS: Cpl_Run."
 END IF

 END SUBROUTINE Cpl_Run





!----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Cpl_Finalize --- finalizing routine to finish the 
!                            GFS ensemble coupler.
!
! !DESCRIPTION: This subroutine will finish the GFS ensemble coupler
! !             and will release the memory space.
!
! !REVISION HISTORY:
!
!  April    2006     Weiyu Yang Initial code.
!  December 2015     C. Redder  Removed intent attribute of the first
!                       subroutine arguments to resolve compiler errors
!                       and conform to ANSI standard fortran and to the
!                       interface of the dummy procedure for the
!                       routine,  ESMF_GridCompSetEntryPoint (called
!                       above)
!
! !INTERFACE:

 SUBROUTINE Cpl_Finalize(CplGEFS, impGEFS, expGEFS, clock, rcfinal)

!
! !INPUT VARIABLES AND PARAMETERS:
!---------------------------------
! TYPE(ESMF_CplComp), INTENT(inout)  :: CplGEFS
! TYPE(ESMF_State),   INTENT(inout)  :: impGEFS
! TYPE(ESMF_State),   INTENT(inout)  :: expGEFS
! TYPE(ESMF_Clock),   INTENT(inout)  :: clock
 TYPE(ESMF_CplComp) :: CplGEFS
 TYPE(ESMF_State)   :: impGEFS
 TYPE(ESMF_State)   :: expGEFS
 TYPE(ESMF_Clock)   :: clock

! !OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------
 INTEGER,            INTENT(out)    :: rcfinal

! !WORKING ARRAYS AND LOCAL PARAMETERS.
!--------------------------------------
 TYPE(GEFS_Cpl_wrap)                   :: wrap
 TYPE(GEFS_Cpl_InternalState), POINTER :: Cpl_Int_State
 INTEGER                               :: rc1

!EOP
!-------------------------------------------------------------------------

 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

! Retrieve the ESMF internal state.
!----------------------------------
     CALL ESMF_LogWrite("Get the Internal State in the Cpl Finalize Routine", &
                        ESMF_LOG_INFO, rc = rc1)

 CALL ESMF_CplCompGetInternalState(CplGEFS, wrap, rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Get the Internal State in the Cpl Finalize Routine")) THEN
         rcfinal = ESMF_FAILURE
         PRINT*, 'Error Happened When Getting the Internal State in Cpl FInalize Routine, rc = ', rc1
     END IF

 Cpl_Int_State => wrap%Cpl_Int_State

 DEALLOCATE(Cpl_Int_State%TRIEO_LS_SIZE,   &
            Cpl_Int_State%TRIEO_ST_SIZE)

! Deallocate trieo arrays.
!-------------------------
! DEALLOCATE(Cpl_Int_State%trieo_ls_max)
! DEALLOCATE(Cpl_Int_State%trieo_ls    )
! DEALLOCATE(Cpl_Int_State%trieo_ls_ini)
! DEALLOCATE(Cpl_Int_State%trieo_ls_w1)
! DEALLOCATE(Cpl_Int_State%trieo_ls_w2)
            
 IF(rcfinal /= ESMF_SUCCESS) THEN
     PRINT*, "FAIL: Cpl_Finalize."
!ELSE
!    PRINT*, "PASS: Cpl_Finalize."
 END IF

 END SUBROUTINE Cpl_Finalize

 END MODULE GEFS_CplComp_ESMFMod
