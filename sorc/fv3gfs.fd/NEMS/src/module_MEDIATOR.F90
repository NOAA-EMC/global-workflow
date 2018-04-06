#include "./ESMFVersionDefine.h"

module module_MEDIATOR

  !-----------------------------------------------------------------------------
  ! NEMS Mediator Component.
  !
  ! The Mediator has multiple Run() phases to support coupling of varied
  ! configurations.  These phases are defined via compsetentry calls.
  ! Each mediator phase has it's check_import timestamp check turned off to
  ! support coupling with arbitrary coupling periods and lags.  The
  ! SetRunClock is forcing the mediator clock to match the driver clock,
  ! again to support arbitrary coupling periods and lags.  That also
  ! allows use of the generic export timestamp method to timestamp
  ! all export fields in the mediator.
  !
  ! Modification Log
  ! * 2015-09-11 Added Land (lnd,l) to Mediator at atm,ice coupling period - DCR
  !              ATM <-> LND: Redist, LND not connected to OCN, ICE
  ! * 2015-10-30 Added Hydro (hyd,h) to Mediator at atm,ice, lnd coupling period - DCR
  !              LND <-> HYD: Regrid, HYD not connected to ATM, OCN, ICE
  ! * 2015-12-16 Added NEMS_GridCopyCoord - DCR
  !              ATM <-> LND: Changed from Redist to Regrid
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Mediator, &
    mediator_routine_SS             => SetServices, &
    mediator_routine_Run            => routine_Run, &
    mediator_label_DataInitialize   => label_DataInitialize, &
    mediator_label_Advance          => label_Advance, &
    mediator_label_CheckImport      => label_CheckImport, &
    mediator_label_TimestampExport  => label_TimestampExport, &
    mediator_label_SetRunClock      => label_SetRunClock
  use module_MEDIATOR_methods
  
  implicit none
  
  private
  
  ! private internal state to keep instance data
  type InternalStateStruct
    integer               :: fastcntr    ! slice counter for writing to NetCDF file
    integer               :: slowcntr    ! slice counter for writing to NetCDF file
    integer               :: accumcntAtm ! accumulator counter
    integer               :: accumcntOcn ! accumulator counter
    integer               :: accumcntIce ! accumulator counter
    integer               :: accumcntLnd ! accumulator counter
    integer               :: accumcntHyd ! accumulator counter
    integer               :: accumcntAtmOcn ! accumulator counter
    type(ESMF_FieldBundle):: FBaccumAtm  ! accumulator of atm export data
    type(ESMF_FieldBundle):: FBaccumOcn  ! accumulator of ocn export data
    type(ESMF_FieldBundle):: FBaccumIce  ! accumulator of ice export data
    type(ESMF_FieldBundle):: FBaccumLnd  ! accumulator of lnd export data
    type(ESMF_FieldBundle):: FBaccumHyd  ! accumulator of lnd export data
    type(ESMF_FieldBundle):: FBaccumAtmOcn  ! accumulator of atm export data
    type(ESMF_FieldBundle):: FBAtm_a     ! Atm export data on atm grid
    type(ESMF_FieldBundle):: FBAtm_o     ! Atm export data mapped to ocn grid
    type(ESMF_FieldBundle):: FBAtm_i     ! Atm export data mapped to ice grid
    type(ESMF_FieldBundle):: FBAtm_l     ! Atm export data mapped to lnd grid
    type(ESMF_FieldBundle):: FBAtm_h     ! Atm export data mapped to hyd grid
    type(ESMF_FieldBundle):: FBOcn_a     ! Ocn export data mapped to atm grid
    type(ESMF_FieldBundle):: FBOcn_o     ! Ocn export data on ocn grid
    type(ESMF_FieldBundle):: FBOcn_i     ! Ocn export data mapped to ice grid
    type(ESMF_FieldBundle):: FBIce_a     ! Ice export data mapped to atm grid
    type(ESMF_FieldBundle):: FBIce_o     ! Ice export data mapped to ocn grid
    type(ESMF_FieldBundle):: FBIce_i     ! Ice export data on ice grid
    type(ESMF_FieldBundle):: FBIce_if    ! Ice export data on ice grid multiplied by ice fraction
    type(ESMF_FieldBundle):: FBLnd_a     ! Lnd export data mapped to atm grid
    type(ESMF_FieldBundle):: FBLnd_l     ! Lnd export on lnd grid
    type(ESMF_FieldBundle):: FBLnd_h     ! Lnd export data mapped to hyd grid
    type(ESMF_FieldBundle):: FBHyd_l     ! Hyd export data mapped to lnd grid
    type(ESMF_FieldBundle):: FBHyd_a     ! Hyd export data mapped to atm grid
    type(ESMF_FieldBundle):: FBHyd_h     ! Hyd export on hyd grid
    type(ESMF_FieldBundle):: FBAtmOcn_o  ! Atm/Ocn flux fields on ocn grid
    type(ESMF_FieldBundle):: FBAtmOcn_a  ! Atm/Ocn flux fields on atm grid
    type(ESMF_FieldBundle):: FBforAtm    ! data storage for atm import
    type(ESMF_FieldBundle):: FBforOcn    ! data storage for ocn import
    type(ESMF_FieldBundle):: FBforIce    ! data storage for ice import
    type(ESMF_FieldBundle):: FBforLnd    ! data storage for lnd import
    type(ESMF_FieldBundle):: FBforHyd    ! data storage for hyd import
    type(ESMF_RouteHandle):: RH_a2o_bilnr  ! atm to ocn bilinear
    type(ESMF_RouteHandle):: RH_o2a_bilnr  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_bilnr  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_bilnr  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_bilnr  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_bilnr  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_bilnr  ! atm to hyd
    type(ESMF_RouteHandle):: RH_h2a_bilnr  ! hyd to atm
    type(ESMF_RouteHandle):: RH_o2i_bilnr  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_bilnr  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_bilnr  ! lnd to hyd
    type(ESMF_RouteHandle):: RH_h2l_bilnr  ! hyd to lnd
    type(ESMF_RouteHandle):: RH_a2o_consf  ! atm to ocn conservative fracarea
    type(ESMF_RouteHandle):: RH_o2a_consf  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_consf  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_consf  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_consf  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_consf  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_consf  ! atm to hyd
    type(ESMF_RouteHandle):: RH_h2a_consf  ! hyd to atm
    type(ESMF_RouteHandle):: RH_o2i_consf  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_consf  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_consf  ! lnd to hyd
    type(ESMF_RouteHandle):: RH_h2l_consf  ! hyd to lnd
    type(ESMF_RouteHandle):: RH_a2o_consd  ! atm to ocn conservative dstarea
    type(ESMF_RouteHandle):: RH_o2a_consd  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_consd  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_consd  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_consd  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_consd  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_consd  ! atm to hyd
    type(ESMF_RouteHandle):: RH_h2a_consd  ! hyd to atm
    type(ESMF_RouteHandle):: RH_o2i_consd  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_consd  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_consd  ! lnd to hyd
    type(ESMF_RouteHandle):: RH_h2l_consd  ! hyd to lnd
    type(ESMF_RouteHandle):: RH_a2o_patch  ! atm to ocn patch
    type(ESMF_RouteHandle):: RH_o2a_patch  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_patch  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_patch  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_patch  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_patch  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_patch  ! atm to hyd
    type(ESMF_RouteHandle):: RH_h2a_patch  ! hyd to atm
    type(ESMF_RouteHandle):: RH_o2i_patch  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_patch  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_patch  ! lnd to hyd
    type(ESMF_RouteHandle):: RH_h2l_patch  ! hyd to lnd
    type(ESMF_RouteHandle):: RH_a2o_fcopy  ! atm to ocn fcopy
    type(ESMF_RouteHandle):: RH_o2a_fcopy  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_fcopy  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_fcopy  ! ice to atm
    type(ESMF_RouteHandle):: RH_a2l_fcopy  ! atm to lnd
    type(ESMF_RouteHandle):: RH_l2a_fcopy  ! lnd to atm
    type(ESMF_RouteHandle):: RH_a2h_fcopy  ! atm to hyd
    type(ESMF_RouteHandle):: RH_h2a_fcopy  ! hyd to atm
    type(ESMF_RouteHandle):: RH_o2i_fcopy  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_fcopy  ! ice to ocn
    type(ESMF_RouteHandle):: RH_l2h_fcopy  ! lnd to hyd
    type(ESMF_RouteHandle):: RH_h2l_fcopy  ! hyd to lnd
    logical               :: a2o_active
    logical               :: o2a_active
    logical               :: a2i_active
    logical               :: i2a_active
    logical               :: a2l_active
    logical               :: l2a_active
    logical               :: a2h_active
    logical               :: h2a_active
    logical               :: o2i_active
    logical               :: i2o_active
    logical               :: l2h_active
    logical               :: h2l_active
!    logical               :: o2l_active ! (o2l connection not implemented)
!    logical               :: l2o_active ! (l2o connection not implemented)
!    logical               :: o2h_active ! (o2h connection not implemented)
!    logical               :: h2o_active ! (h2o connection not implemented)
!    logical               :: i2l_active ! (i2l connection not implemented)
!    logical               :: l2i_active ! (l2i connection not implemented)
!    logical               :: i2h_active ! (i2h connection not implemented)
!    logical               :: h2i_active ! (h2i connection not implemented)

! tcx Xgrid
!    type(ESMF_RouteHandle):: RHa2x       ! atm to xgrid RH
!    type(ESMF_RouteHandle):: RHo2x       ! ocn to xgrid RH
!    type(ESMF_RouteHandle):: RHx2a       ! xgrid to atm RH
!    type(ESMF_RouteHandle):: RHx2o       ! xgrid to ocn RH
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type

  interface fieldBundle_accum ; module procedure &
    fieldBundle_accumFB2FB, &
    fieldBundle_accumFB2ST, &
    fieldBundle_accumST2FB
  end interface

  interface fieldBundle_copy ; module procedure &
    fieldBundle_copyFB2FB, &
    fieldBundle_copyFB2ST, &
    fieldBundle_copyST2FB
  end interface

  interface NUOPCplus_UpdateTimestamp; module procedure &
    NUOPCplus_UpdateTimestampS, &
    NUOPCplus_UpdateTimestampF
  end interface

  ! local variables
  type(ESMF_Grid)    :: gridAtm, gridOcn, gridIce, gridLnd, gridHyd, gridMed
  integer, parameter :: nx_med=400, ny_med=200
  integer            :: dbug_flag = 5
  integer            :: restart_interval = 0
  logical            :: statewrite_flag = .true.      ! diagnostics output, default
  logical            :: rhprint_flag = .false.        ! diagnostics output, default
  logical            :: profile_memory = .true.       ! diagnostics output, default
  logical            :: coldstart = .false.           ! coldstart flag
  logical            :: atmocn_flux_from_atm = .true. ! where is atm/ocn flux computed
  logical            :: generate_landmask = .true.   ! landmask flag
  integer            :: dbrc
  character(len=256) :: msgString
  logical            :: isPresent
  type(ESMF_Time)    :: time_invalidTimeStamp
  type(ESMF_Clock)   :: clock_invalidTimeStamp
  real(ESMF_KIND_R8), parameter :: const_lhvap = 2.501e6_ESMF_KIND_R8  ! latent heat of evaporation ~ J/kg
  integer            :: srcTermProcessing_Value = 0
  logical            :: read_rest_FBaccumAtm = .false.
  logical            :: read_rest_FBaccumOcn = .false.
  logical            :: read_rest_FBaccumIce = .false.
  logical            :: read_rest_FBaccumLnd = .false.
  logical            :: read_rest_FBaccumHyd = .false.
  logical            :: read_rest_FBaccumAtmOcn = .false.
  logical            :: read_rest_FBAtm_a = .false.
  logical            :: read_rest_FBIce_i = .false.
  logical            :: read_rest_FBOcn_o = .false.
  logical            :: read_rest_FBLnd_l = .false.
  logical            :: read_rest_FBHyd_h = .false.
  logical            :: read_rest_FBAtmOcn_o = .false.
!  real(ESMF_KIND_R8), parameter :: spval_init = -9.99999e6_ESMF_KIND_R8  ! spval for initialization
!  real(ESMF_KIND_R8), parameter :: spval = -1.0e36_ESMF_KIND_R8  ! spval
  real(ESMF_KIND_R8), parameter :: spval_init = 0.0_ESMF_KIND_R8  ! spval for initialization
  real(ESMF_KIND_R8), parameter :: spval = 0.0_ESMF_KIND_R8  ! spval
  real(ESMF_KIND_R8), parameter :: czero = 0.0_ESMF_KIND_R8  ! spval
  integer           , parameter :: ispval_mask = -987987     ! spval for RH mask values

  type fld_list_type
    integer :: num = -1
    character(len=64), pointer :: stdname(:)
    character(len=64), pointer :: shortname(:)
    character(len=64), pointer :: transferOffer(:)
    character(len=64), pointer :: mapping(:)
  end type fld_list_type
  type(ESMF_State)     :: NState_AtmImp   ! Atm Import nested state
  type(ESMF_State)     :: NState_OcnImp   ! Ocn Import nested state
  type(ESMF_State)     :: NState_IceImp   ! Ice Import nested state
  type(ESMF_State)     :: NState_LndImp   ! Lnd Import nested state
  type(ESMF_State)     :: NState_HydImp   ! Hyd Import nested state
  type(ESMF_State)     :: NState_AtmExp   ! Atm Export nested state
  type(ESMF_State)     :: NState_OcnExp   ! Ocn Export nested state
  type(ESMF_State)     :: NState_IceExp   ! Ice Export nested state
  type(ESMF_State)     :: NState_LndExp   ! Lnd Export nested state
  type(ESMF_State)     :: NState_HydExp   ! Hyd Export nested state
  type (fld_list_type) :: fldsToAtm
  type (fld_list_type) :: fldsFrAtm
  type (fld_list_type) :: fldsToOcn
  type (fld_list_type) :: fldsFrOcn
  type (fld_list_type) :: fldsToIce
  type (fld_list_type) :: fldsFrIce
  type (fld_list_type) :: fldsToLnd
  type (fld_list_type) :: fldsFrLnd
  type (fld_list_type) :: fldsToHyd
  type (fld_list_type) :: fldsFrHyd
  type (fld_list_type) :: fldsAtmOcn
  real(ESMF_KIND_R8), allocatable :: land_mask(:,:)

  type(ESMF_PoleMethod_Flag), parameter :: polemethod=ESMF_POLEMETHOD_ALLAVG

  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(len=*),parameter :: subname='(module_MEDIATOR:SetServices)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! the NUOPC mediator component will register the generic methods
    call NUOPC_CompDerive(gcomp, mediator_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Provide InitializeP0 to switch from default IPDv00 to IPDv03
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! IPDv03p1: advertise Fields
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeIPDv03p1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    
    ! IPDv03p3: realize connected Fields with transfer action "provide"
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeIPDv03p3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    ! IPDv03p4: optionally modify the decomp/distr of transferred Grid/Mesh
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p4"/), userRoutine=InitializeIPDv03p4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! IPDv03p5: realize all Fields with transfer action "accept"
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p5"/), userRoutine=InitializeIPDv03p5, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! overwrite Finalize
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = slow ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_slow"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_slow", specRoutine=MedPhase_slow, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = fast_before ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_fast_before"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_fast_before", &
      specRoutine=MedPhase_fast_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    
    ! set entry point for Run( phase = fast_after ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_fast_after"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_fast_after", &
      specRoutine=MedPhase_fast_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = accum_fast ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_accum_fast"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_accum_fast", specRoutine=MedPhase_accum_fast, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = atm_ocn_flux ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_atm_ocn_flux"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_atm_ocn_flux", specRoutine=MedPhase_atm_ocn_flux, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = prep_ocn ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_prep_ocn"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_prep_ocn", specRoutine=MedPhase_prep_ocn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = prep_ice ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_prep_ice"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_prep_ice", specRoutine=MedPhase_prep_ice, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = prep_atm ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_prep_atm"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_prep_atm", specRoutine=MedPhase_prep_atm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_TimestampExport, &
      specPhaseLabel="MedPhase_prep_atm", &
      specRoutine=TimestampExport_prep_atm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = prep_lnd ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_prep_lnd"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_prep_lnd", specRoutine=MedPhase_prep_lnd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = prep_hyd ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_prep_hyd"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_prep_hyd", specRoutine=MedPhase_prep_hyd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = write_restart ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_write_restart"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_write_restart", specRoutine=MedPhase_write_restart, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! attach specializing method(s)
    ! -> NUOPC specializes by default --->>> first need to remove the default
    call ESMF_MethodRemove(gcomp, mediator_label_CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_CheckImport, &
      specRoutine=NUOPC_NoOp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! attach specializing method(s)
    ! -> NUOPC specializes by default --->>> first need to remove the default
    call ESMF_MethodRemove(gcomp, mediator_label_SetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_SetRunClock, &
      specRoutine=SetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! AtmOcn Coupling Fields
    call fld_list_add(fldsAtmOcn,"mean_up_lw_flx_ocn"              , "cannot provide","conservefrac")
    call fld_list_add(fldsAtmOcn,"mean_sensi_heat_flx_atm_into_ocn", "cannot provide","conservefrac")
    call fld_list_add(fldsAtmOcn,"mean_laten_heat_flx_atm_into_ocn", "cannot provide","conservefrac")
    call fld_list_add(fldsAtmOcn,"mean_evap_rate_atm_into_ocn"     , "cannot provide","conservefrac")
    call fld_list_add(fldsAtmOcn,"stress_on_air_ocn_zonal"         , "cannot provide","conservefrac")
    call fld_list_add(fldsAtmOcn,"stress_on_air_ocn_merid"         , "cannot provide","conservefrac")
!    call fld_list_add(fldsAtmOcn,"temperature_2m"                  , "cannot provide","bilinear")
!    call fld_list_add(fldsAtmOcn,"humidity_2m"                     , "cannot provide","bilinear")
!    call fld_list_add(fldsAtmOcn,"wind_speed_squared_10m"          , "cannot provide","bilinear")
    call fld_list_add(fldsAtmOcn,"temperature_2m"                  , "cannot provide","conservefrac")
    call fld_list_add(fldsAtmOcn,"humidity_2m"                     , "cannot provide","conservefrac")
    call fld_list_add(fldsAtmOcn,"wind_speed_squared_10m"          , "cannot provide","conservefrac")

    ! Fields to ATM
    call fld_list_add(fldsToAtm,"land_mask"               , "cannot provide")
    call fld_list_add(fldsToAtm,"surface_temperature"     , "will provide")
    call fld_list_add(fldsToAtm,"sea_surface_temperature" , "will provide")
    call fld_list_add(fldsToAtm,"inst_ice_ir_dir_albedo"  , "will provide")
    call fld_list_add(fldsToAtm,"inst_ice_ir_dif_albedo"  , "will provide")
    call fld_list_add(fldsToAtm,"inst_ice_vis_dir_albedo" , "will provide")
    call fld_list_add(fldsToAtm,"inst_ice_vis_dif_albedo" , "will provide")
    call fld_list_add(fldsToAtm,"ice_fraction"            , "will provide")
    call fld_list_add(fldsToAtm,"stress_on_air_ice_zonal" , "will provide")
    call fld_list_add(fldsToAtm,"stress_on_air_ice_merid" , "will provide")
    call fld_list_add(fldsToAtm,"mean_up_lw_flx_ice"      , "will provide")
    call fld_list_add(fldsToAtm,"mean_sensi_heat_flx_atm_into_ice", "will provide")
    call fld_list_add(fldsToAtm,"mean_laten_heat_flx_atm_into_ice", "will provide")
    call fld_list_add(fldsToAtm,"mean_sensi_heat_flx_atm_into_lnd", "will provide")
    call fld_list_add(fldsToAtm,"mean_laten_heat_flx_atm_into_lnd", "will provide")
    call fld_list_add(fldsToAtm,"mean_evap_rate_atm_into_ice"     , "will provide")
    call fld_list_add(fldsToAtm,"mean_zonal_moment_flx"   , "will provide")
    call fld_list_add(fldsToAtm,"mean_merid_moment_flx"   , "will provide")
    call fld_list_add(fldsToAtm,"mean_sensi_heat_flx"     , "will provide")
    call fld_list_add(fldsToAtm,"mean_laten_heat_flx"     , "will provide")
    call fld_list_add(fldsToAtm,"mean_up_lw_flx"          , "will provide")
    call fld_list_add(fldsToAtm,"mean_evap_rate"          , "will provide")
    call fld_list_add(fldsToAtm,"liquid_water_content_of_soil_layer_1", "will provide")
    call fld_list_add(fldsToAtm,"liquid_water_content_of_soil_layer_2", "will provide")
    call fld_list_add(fldsToAtm,"liquid_water_content_of_soil_layer_3", "will provide")
    call fld_list_add(fldsToAtm,"liquid_water_content_of_soil_layer_4", "will provide")
    call fld_list_add(fldsToAtm,"mean_ice_volume"         , "will provide")
    call fld_list_add(fldsToAtm,"mean_snow_volume"        , "will provide")
!    call fld_list_add(fldsFrHyd,"volume_fraction_of_total_water_in_soil", "will provide")
!    call fld_list_add(fldsFrHyd,"surface_snow_thickness"                , "will provide")
!    call fld_list_add(fldsFrHyd,"liquid_water_content_of_surface_snow"  , "will provide")


    ! Fields from ATM
    call fld_list_add(fldsFrAtm,"mean_zonal_moment_flx"   , "cannot provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_merid_moment_flx"   , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_sensi_heat_flx"     , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_laten_heat_flx"     , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_down_lw_flx"        , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_up_lw_flx"          , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_down_sw_flx"        , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_prec_rate"          , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_fprec_rate"         , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_zonal_moment_flx"   , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_merid_moment_flx"   , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_sensi_heat_flx"     , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_laten_heat_flx"     , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_down_lw_flx"        , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_up_lw_flx"          , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_down_sw_flx"        , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_temp_height2m"      , "will provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_spec_humid_height2m", "will provide","bilinear")
#ifdef PATCH_BFB_FIXED
    call fld_list_add(fldsFrAtm,"inst_u_wind_height10m"     , "will provide","patch")
    call fld_list_add(fldsFrAtm,"inst_v_wind_height10m"     , "will provide","patch")
    call fld_list_add(fldsFrAtm,"inst_zonal_wind_height10m" , "will provide","patch")
    call fld_list_add(fldsFrAtm,"inst_merid_wind_height10m" , "will provide","patch")
#else
    call fld_list_add(fldsFrAtm,"inst_u_wind_height10m"     , "will provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_v_wind_height10m"     , "will provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_zonal_wind_height10m" , "will provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_merid_wind_height10m" , "will provide","bilinear")
#endif
    call fld_list_add(fldsFrAtm,"inst_temp_height_surface", "will provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_pres_height_surface", "will provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_surface_height"     , "will provide","bilinear")
    ! new imports from GSM added 04/23/14:
    call fld_list_add(fldsFrAtm,"mean_net_lw_flx"         , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_net_sw_flx"         , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_net_lw_flx"         , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_net_sw_flx"         , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_down_sw_ir_dir_flx" , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_down_sw_ir_dif_flx" , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_down_sw_vis_dir_flx", "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_down_sw_vis_dif_flx", "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_down_sw_ir_dir_flx" , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_down_sw_ir_dif_flx" , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_down_sw_vis_dir_flx", "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_down_sw_vis_dif_flx", "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_net_sw_ir_dir_flx"  , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_net_sw_ir_dif_flx"  , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_net_sw_vis_dir_flx" , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"mean_net_sw_vis_dif_flx" , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_net_sw_ir_dir_flx"  , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_net_sw_ir_dif_flx"  , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_net_sw_vis_dir_flx" , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_net_sw_vis_dif_flx" , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_ir_dir_albedo"      , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_ir_dif_albedo"      , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_vis_dir_albedo"     , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_vis_dif_albedo"     , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_ocn_ir_dir_albedo"  , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_ocn_ir_dif_albedo"  , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_ocn_vis_dir_albedo" , "will provide","conservefrac")
    call fld_list_add(fldsFrAtm,"inst_ocn_vis_dif_albedo" , "will provide","conservefrac")
    ! new imports from GSM added 06/09/15:
    call fld_list_add(fldsFrAtm,"inst_temp_height_lowest"       , "will provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_spec_humid_height_lowest" , "will provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_zonal_wind_height_lowest" , "will provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_merid_wind_height_lowest" , "will provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_pres_height_lowest"       , "will provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_height_lowest"            , "will provide","bilinear")

    ! Fields to OCN
    call fld_list_add(fldsToOcn,"mean_zonal_moment_flx"   , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_merid_moment_flx"   , "will provide")
    call fld_list_add(fldsToOcn,"mean_sensi_heat_flx"     , "will provide")
    call fld_list_add(fldsToOcn,"mean_laten_heat_flx"     , "will provide")
    call fld_list_add(fldsToOcn,"mean_down_lw_flx"        , "will provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_vis_dir_flx", "will provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_vis_dif_flx", "will provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_ir_dir_flx" , "will provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_ir_dif_flx" , "will provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_vis_dir_flx" , "will provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_vis_dif_flx" , "will provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_ir_dir_flx"  , "will provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_ir_dif_flx"  , "will provide")
!   call fld_list_add(fldsToOcn,"mean_salt_flx"           , "will provide")
    call fld_list_add(fldsToOcn,"mean_prec_rate"          , "will provide")
    call fld_list_add(fldsToOcn,"mean_fprec_rate"         , "will provide")
    call fld_list_add(fldsToOcn,"mean_evap_rate"          , "will provide")
!   call fld_list_add(fldsToOcn,"mean_runoff_rate"        , "will provide")
!   call fld_list_add(fldsToOcn,"mean_calving_rate"       , "will provide")
!   call fld_list_add(fldsToOcn,"mean_runoff_flx"         , "will provide")
!   call fld_list_add(fldsToOcn,"mean_calving_flx"        , "will provide")
    call fld_list_add(fldsToOcn,"inst_pres_height_surface", "will provide")
!   call fld_list_add(fldsToOcn,"mass_of_overlying_sea_ice, "will provide")
    call fld_list_add(fldsToOcn,"stress_on_ocn_ice_zonal" , "will provide")
    call fld_list_add(fldsToOcn,"stress_on_ocn_ice_merid" , "will provide")
!    call fld_list_add(fldsToOcn,"stress_on_ocn_ice_idir"  , "will provide")
!    call fld_list_add(fldsToOcn,"stress_on_ocn_ice_jdir"  , "will provide")
    call fld_list_add(fldsToOcn,"mean_sw_pen_to_ocn"      , "will provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_flx"        , "will provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_flx"         , "will provide")
    call fld_list_add(fldsToOcn,"mean_net_lw_flx"         , "will provide")
    call fld_list_add(fldsToOcn,"mean_up_lw_flx"          , "will provide")
    call fld_list_add(fldsToOcn,"inst_temp_height2m"      , "will provide")
    call fld_list_add(fldsToOcn,"inst_spec_humid_height2m", "will provide")
    call fld_list_add(fldsToOcn,"net_heat_flx_to_ocn"     , "will provide")
    call fld_list_add(fldsToOcn,"mean_fresh_water_to_ocean_rate", "will provide")
    call fld_list_add(fldsToOcn,"mean_salt_rate"          , "will provide")
    call fld_list_add(fldsToOcn,"ice_fraction"          , "will provide")
 
    ! Fields from OCN
    call fld_list_add(fldsFrOcn,"ocean_mask"              , "cannot provide","conservedst")
    call fld_list_add(fldsFrOcn,"sea_surface_temperature" , "will provide","copy")
    call fld_list_add(fldsFrOcn,"s_surf"                  , "will provide","copy")
#ifdef PATCH_BFB_FIXED
    call fld_list_add(fldsFrOcn,"ocn_current_zonal"       , "will provide","patch")
    call fld_list_add(fldsFrOcn,"ocn_current_merid"       , "will provide","patch")
#else
    call fld_list_add(fldsFrOcn,"ocn_current_zonal"       , "will provide","copy")
    call fld_list_add(fldsFrOcn,"ocn_current_merid"       , "will provide","copy")
#endif
!    call fld_list_add(fldsFrOcn,"ocn_current_idir"        , "will provide","copy")
!    call fld_list_add(fldsFrOcn,"ocn_current_jdir"        , "will provide","copy")
    call fld_list_add(fldsFrOcn,"sea_lev"                 , "will provide","copy")
    call fld_list_add(fldsFrOcn,"freezing_melting_potential", "will provide","copy")
    call fld_list_add(fldsFrOcn,"upward_sea_ice_basal_available_heat_flux" &
                                                          , "will provide","conservefrac")
    call fld_list_add(fldsFrOcn,"mixed_layer_depth"       , "will provide","copy")
    call fld_list_add(fldsFrOcn,"sea_surface_slope_zonal" , "will provide","copy")
    call fld_list_add(fldsFrOcn,"sea_surface_slope_merid" , "will provide","copy")

    ! Fields to ICE
    call fld_list_add(fldsToIce,"dummyfield"               , "cannot provide")
    call fld_list_add(fldsToIce,"inst_temp_height2m"       , "cannot provide")
    call fld_list_add(fldsToIce,"inst_spec_humid_height2m" , "will provide")
    call fld_list_add(fldsToIce,"inst_zonal_wind_height10m", "will provide")
    call fld_list_add(fldsToIce,"inst_merid_wind_height10m", "will provide")
    call fld_list_add(fldsToIce,"inst_temp_height_surface" , "will provide")
    call fld_list_add(fldsToIce,"inst_surface_height"      , "will provide")
    call fld_list_add(fldsToIce,"inst_pres_height_surface" , "will provide")
    call fld_list_add(fldsToIce,"mean_down_lw_flx"         , "will provide")
    call fld_list_add(fldsToIce,"mean_down_sw_vis_dir_flx" , "will provide")
    call fld_list_add(fldsToIce,"mean_down_sw_vis_dif_flx" , "will provide")
    call fld_list_add(fldsToIce,"mean_down_sw_ir_dir_flx"  , "will provide")
    call fld_list_add(fldsToIce,"mean_down_sw_ir_dif_flx"  , "will provide")
    call fld_list_add(fldsToIce,"mean_prec_rate"           , "will provide")
    call fld_list_add(fldsToIce,"mean_fprec_rate"          , "will provide")
    call fld_list_add(fldsToIce,"sea_surface_temperature"  , "will provide")
    call fld_list_add(fldsToIce,"s_surf"                   , "will provide")
    call fld_list_add(fldsToIce,"sea_lev"                  , "will provide")
    call fld_list_add(fldsToIce,"sea_surface_slope_zonal"  , "will provide")
    call fld_list_add(fldsToIce,"sea_surface_slope_merid"  , "will provide")
    call fld_list_add(fldsToIce,"ocn_current_zonal"        , "will provide")
    call fld_list_add(fldsToIce,"ocn_current_merid"        , "will provide")
!    call fld_list_add(fldsToIce,"ocn_current_idir"         , "will provide")
!    call fld_list_add(fldsToIce,"ocn_current_jdir"         , "will provide")
    call fld_list_add(fldsToIce,"freezing_melting_potential", "will provide")
    call fld_list_add(fldsToIce,"mixed_layer_depth"        , "will provide")
    ! new exports from GSM added 06/09/15:
    call fld_list_add(fldsToIce,"inst_temp_height_lowest"       , "will provide")
    call fld_list_add(fldsToIce,"inst_spec_humid_height_lowest" , "will provide")
    call fld_list_add(fldsToIce,"inst_zonal_wind_height_lowest" , "will provide")
    call fld_list_add(fldsToIce,"inst_merid_wind_height_lowest" , "will provide")
    call fld_list_add(fldsToIce,"inst_pres_height_lowest"       , "will provide")
    call fld_list_add(fldsToIce,"inst_height_lowest"            , "will provide")
    call fld_list_add(fldsToIce,"mean_zonal_moment_flx"         , "will provide")
    call fld_list_add(fldsToIce,"mean_merid_moment_flx"         , "will provide")
    call fld_list_add(fldsToIce,"air_density_height_lowest"     , "will provide")

    ! Fields from ICE
    call fld_list_add(fldsFrIce,"dummyfield"              , "cannot provide","bilinear")
    call fld_list_add(fldsFrIce,"ice_mask"                , "cannot provide","conservedst")
!    call fld_list_add(fldsFrIce,"sea_ice_temperature"     , "will provide","bilinear")
    call fld_list_add(fldsFrIce,"sea_ice_temperature"     , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"inst_ice_ir_dir_albedo"  , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"inst_ice_ir_dif_albedo"  , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"inst_ice_vis_dir_albedo" , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"inst_ice_vis_dif_albedo" , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"ice_fraction"            , "will provide","conservedst")
    call fld_list_add(fldsFrIce,"stress_on_air_ice_zonal" , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"stress_on_air_ice_merid" , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"stress_on_ocn_ice_zonal" , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"stress_on_ocn_ice_merid" , "will provide","conservefrac")
!    call fld_list_add(fldsFrIce,"stress_on_ocn_ice_idir"  , "will provide","copy")
!    call fld_list_add(fldsFrIce,"stress_on_ocn_ice_jdir"  , "will provide","copy")
    call fld_list_add(fldsFrIce,"mean_sw_pen_to_ocn"      , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"mean_net_sw_vis_dir_flx" , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"mean_net_sw_vis_dif_flx" , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"mean_net_sw_ir_dir_flx"  , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"mean_net_sw_ir_dif_flx"  , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"mean_up_lw_flx_ice"      , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"mean_sensi_heat_flx_atm_into_ice", "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"mean_laten_heat_flx_atm_into_ice", "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"mean_evap_rate_atm_into_ice"     , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"net_heat_flx_to_ocn"     , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"mean_fresh_water_to_ocean_rate"  , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"mean_salt_rate"          , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"mean_ice_volume"         , "will provide","conservefrac")
    call fld_list_add(fldsFrIce,"mean_snow_volume"        , "will provide","conservefrac")
 
    ! Required met forcing fields to LND
    call fld_list_add(fldsToLnd,"inst_down_lw_flx"                      , "cannot provide")
    call fld_list_add(fldsToLnd,"inst_down_sw_flx"                      , "cannot provide")
    call fld_list_add(fldsToLnd,"inst_merid_wind_height_lowest"         , "cannot provide")
    call fld_list_add(fldsToLnd,"inst_pres_height_surface"              , "cannot provide")
    call fld_list_add(fldsToLnd,"inst_spec_humid_height_lowest"         , "cannot provide")
    call fld_list_add(fldsToLnd,"inst_temp_height_lowest"               , "cannot provide")
    call fld_list_add(fldsToLnd,"inst_zonal_wind_height_lowest"         , "cannot provide")
    call fld_list_add(fldsToLnd,"mean_prec_rate"                        , "cannot provide")
    ! Feedback from HYD
    call fld_list_add(fldsToLnd,"liquid_water_content_of_soil_layer_1"    , "cannot provide")
    call fld_list_add(fldsToLnd,"liquid_water_content_of_soil_layer_2"    , "cannot provide")
    call fld_list_add(fldsToLnd,"liquid_water_content_of_soil_layer_3"    , "cannot provide")
    call fld_list_add(fldsToLnd,"liquid_water_content_of_soil_layer_4"    , "cannot provide")
!    call fld_list_add(fldsToLnd,"volume_fraction_of_total_water_in_soil", "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"surface_snow_thickness"                , "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"liquid_water_content_of_surface_snow"  , "cannot provide") ! Missing
    ! Other fields to LND
!    call fld_list_add(fldsToLnd,"aerodynamic_roughness_length"          , "cannot provide")
!    call fld_list_add(fldsToLnd,"canopy_moisture_storage"               ,  "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"carbon_dioxide"                        ,  "cannot provide") ! Future
!    call fld_list_add(fldsToLnd,"cosine_zenith_angle"                   , "cannot provide")
!    call fld_list_add(fldsToLnd,"exchange_coefficient_heat"             , "cannot provide")
!    call fld_list_add(fldsToLnd,"exchange_coefficient_heat_height2m"    , "cannot provide")
!    call fld_list_add(fldsToLnd,"exchange_coefficient_moisture_height2m", "cannot provide")
!    call fld_list_add(fldsToLnd,"ice_mask"                              , "cannot provide")
!    call fld_list_add(fldsToLnd,"inst_height_lowest"                    , "cannot provide")
!    call fld_list_add(fldsToLnd,"inst_pres_height_lowest"               , "cannot provide")
!    call fld_list_add(fldsToLnd,"inst_temp_height_surface"              , "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"inst_wind_speed_height_lowest"         , "cannot provide")
!    call fld_list_add(fldsToLnd,"mean_cprec_rate"                       , "cannot provide")
!    call fld_list_add(fldsToLnd,"mean_down_lw_flx"                      , "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"mean_down_sw_flx"                      , "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"mean_fprec_rate"                       , "cannot provide")
!    call fld_list_add(fldsToLnd,"mean_surface_albedo"                   , "cannot provide")
!    call fld_list_add(fldsToLnd,"saturated_mixing_ratio"                , "cannot provide")
!    call fld_list_add(fldsToLnd,"moisture_content_of_soil_layer_1"      , "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"moisture_content_of_soil_layer_2"      , "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"moisture_content_of_soil_layer_3"      , "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"moisture_content_of_soil_layer_4"      , "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"temperature_of_soil_layer_1"           , "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"temperature_of_soil_layer_2"           , "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"temperature_of_soil_layer_3"           , "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"temperature_of_soil_layer_4"           , "cannot provide") ! Missing
!    call fld_list_add(fldsToLnd,"soil_temperature_bottom"               , "cannot provide")
!    call fld_list_add(fldsToLnd,"surface_microwave_emissivity"          , "cannot provide")

    ! Forcing fields to hydrology
    call fld_list_add(fldsFrLnd,"temperature_of_soil_layer_1"             , "cannot provide","conservefrac")
    call fld_list_add(fldsFrLnd,"temperature_of_soil_layer_2"             , "cannot provide","conservefrac")
    call fld_list_add(fldsFrLnd,"temperature_of_soil_layer_3"             , "cannot provide","conservefrac")
    call fld_list_add(fldsFrLnd,"temperature_of_soil_layer_4"             , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"moisture_content_of_soil_layer_1"        , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"moisture_content_of_soil_layer_2"        , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"moisture_content_of_soil_layer_3"        , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"moisture_content_of_soil_layer_4"        , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"liquid_water_content_of_soil_layer_1"  , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"liquid_water_content_of_soil_layer_2"  , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"liquid_water_content_of_soil_layer_3"  , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"liquid_water_content_of_soil_layer_4"  , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"surface_runoff_flux"                   , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"subsurface_runoff_flux"                , "cannot provide","conservefrac")
    ! Feedback to atmosphere
    call fld_list_add(fldsFrLnd,"mean_sensi_heat_flx_atm_into_lnd", "cannot provide","conservefrac")
    call fld_list_add(fldsFrLnd,"mean_laten_heat_flx_atm_into_lnd", "cannot provide","conservefrac")
    ! Other fields from LND
!    call fld_list_add(fldsFrLnd,"aerodynamic_roughness_length"          , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"canopy_moisture_storage"               , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"exchange_coefficient_heat_height2m"    , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"exchange_coefficient_moisture_height2m", "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"ice_mask"                              , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"inst_temp_height_lowest"               , "cannot provide","conservefrac") ! Missing
!    call fld_list_add(fldsFrLnd,"inst_temp_height_surface"              , "cannot provide","conservefrac") ! Missing
!    call fld_list_add(fldsFrLnd,"mean_grnd_sensi_heat_flx"              , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"mean_laten_heat_flx_kinematic"         , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"mean_net_lw_flx"                       , "cannot provide","conservefrac") ! Missing
!    call fld_list_add(fldsFrLnd,"mean_net_sw_flx"                       , "cannot provide","conservefrac") ! Missing
!    call fld_list_add(fldsFrLnd,"mean_surface_albedo"                   , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"mean_surface_skin_temp"                , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"mixing_ratio_surface"                  , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"root_moisture"                         , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"surface_snow_area_fraction"            , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"surface_snow_melt_flux"                , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"liquid_water_content_of_surface_snow"  , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"surface_snow_thickness"                , "cannot provide","conservefrac") ! Missing
!    call fld_list_add(fldsFrLnd,"soil_hydraulic_conductivity_at_saturation", "cannot provide","conservefrac") ! Missing
!    call fld_list_add(fldsFrLnd,"soil_porosity"                         , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"soil_temperature_bottom"               , "cannot provide","conservefrac") ! Missing
!    call fld_list_add(fldsFrLnd,"soil_type"                             , "cannot provide","conservefrac") ! Missing
!    call fld_list_add(fldsFrLnd,"soil_moisture_content"                 , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"subsurface_basin_mask"                 , "cannot provide","conservefrac") ! Missing
!    call fld_list_add(fldsFrLnd,"surface_microwave_emissivity"          , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"vegetation_type"                       , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"volume_fraction_of_frozen_water_in_soil", "cannot provide","conservefrac") ! Missing
!    call fld_list_add(fldsFrLnd,"volume_fraction_of_total_water_in_soil", "cannot provide","conservefrac")
!    call fld_list_add(fldsFrLnd,"volume_fraction_of_total_water_in_soil_at_critical_point", "cannot provide","conservefrac") ! Missing
!    call fld_list_add(fldsFrLnd,"volume_fraction_of_total_water_in_soil_at_field_capacity", "cannot provide","conservefrac") ! Missing
!    call fld_list_add(fldsFrLnd,"volume_fraction_of_total_water_in_soil_at_wilting_point" , "cannot provide","conservefrac") ! Missing

    ! Required LND forcing fields to HYD
    call fld_list_add(fldsToHyd,"temperature_of_soil_layer_1"         , "cannot provide")
    call fld_list_add(fldsToHyd,"temperature_of_soil_layer_2"         , "cannot provide")
    call fld_list_add(fldsToHyd,"temperature_of_soil_layer_3"         , "cannot provide")
    call fld_list_add(fldsToHyd,"temperature_of_soil_layer_4"         , "cannot provide")
!    call fld_list_add(fldsToHyd,"moisture_content_of_soil_layer_1"    , "cannot provide")
!    call fld_list_add(fldsToHyd,"moisture_content_of_soil_layer_2"    , "cannot provide")
!    call fld_list_add(fldsToHyd,"moisture_content_of_soil_layer_3"    , "cannot provide")
!    call fld_list_add(fldsToHyd,"moisture_content_of_soil_layer_4"    , "cannot provide")
!    call fld_list_add(fldsToHyd,"liquid_water_content_of_soil_layer_1", "cannot provide")
!    call fld_list_add(fldsToHyd,"liquid_water_content_of_soil_layer_2", "cannot provide")
!    call fld_list_add(fldsToHyd,"liquid_water_content_of_soil_layer_3", "cannot provide")
!    call fld_list_add(fldsToHyd,"liquid_water_content_of_soil_layer_4", "cannot provide")
!    call fld_list_add(fldsToHyd,"surface_runoff_flux"               , "cannot provide")
!    call fld_list_add(fldsToHyd,"subsurface_runoff_flux"            , "cannot provide")
    ! Met forcing fields to HYD
    call fld_list_add(fldsToHyd,"inst_down_lw_flx"                      , "cannot provide")
    call fld_list_add(fldsToHyd,"inst_down_sw_flx"                      , "cannot provide")
!    call fld_list_add(fldsToHyd,"inst_merid_wind_height_lowest"         , "cannot provide")
!    call fld_list_add(fldsToHyd,"inst_pres_height_surface"              , "cannot provide")
!    call fld_list_add(fldsToHyd,"inst_spec_humid_height_lowest"         , "cannot provide")
!    call fld_list_add(fldsToHyd,"inst_temp_height_lowest"               , "cannot provide")
!    call fld_list_add(fldsToHyd,"inst_zonal_wind_height_lowest"         , "cannot provide")
!    call fld_list_add(fldsToHyd,"mean_prec_rate"                        , "cannot provide")

    ! Fields from HYD to LND and ATM
    call fld_list_add(fldsFrHyd,"liquid_water_content_of_soil_layer_1"        , "cannot provide","conservefrac")
    call fld_list_add(fldsFrHyd,"liquid_water_content_of_soil_layer_2"        , "cannot provide","conservefrac")
    call fld_list_add(fldsFrHyd,"liquid_water_content_of_soil_layer_3"        , "cannot provide","conservefrac")
    call fld_list_add(fldsFrHyd,"liquid_water_content_of_soil_layer_4"        , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrHyd,"volume_fraction_of_total_water_in_soil"      , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrHyd,"surface_snow_thickness"                      , "cannot provide","conservefrac")
!    call fld_list_add(fldsFrHyd,"liquid_water_content_of_surface_snow"        , "cannot provide","conservefrac")
    ! Other fields from HYD
!    call fld_list_add(fldsFrHyd,"water_surface_height_above_reference_datum"  , "cannot provide","conservefrac")

   if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine SetServices
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables    
    character(len=NUOPC_PhaseMapStringLength) :: initPhases(6)
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeP0)'
    character(len=10)                         :: value
    
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, defaultValue="max", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    dbug_flag = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max"/), specialValueList=(/0,255/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    write(msgString,'(A,i6)') trim(subname)//' dbug_flag = ',dbug_flag
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_AttributeGet(gcomp, name="restart_interval", value=value, defaultValue="unset", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    restart_interval = ESMF_UtilString2Int(value, &
      specialStringList=(/"unset"/), specialValueList=(/0/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    write(msgString,'(A,i6)') trim(subname)//' restart_interval = ',restart_interval
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_AttributeGet(gcomp, name="coldstart", value=value, defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    coldstart=(trim(value)=="true")
    write(msgString,'(A,l6)') trim(subname)//' coldstart = ',coldstart
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_AttributeGet(gcomp, name="generate_landmask", value=value, defaultValue="true", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    generate_landmask=(trim(value)=="true")
    write(msgString,'(A,l6)') trim(subname)//' generate_landmask = ',generate_landmask
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_AttributeGet(gcomp, name="DumpFields", value=value, defaultValue="true", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    statewrite_flag=(trim(value)=="true")
    write(msgString,'(A,l6)') trim(subname)//' statewrite_flag = ',statewrite_flag
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    
    call ESMF_AttributeGet(gcomp, name="DumpRHs", value=value, defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    rhprint_flag=(trim(value)=="true")
    write(msgString,'(A,l6)') trim(subname)//' rhprint_flag = ',rhprint_flag
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_AttributeGet(gcomp, name="ProfileMemory", value=value, defaultValue="true", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    profile_memory=(trim(value)/="false")
    write(msgString,'(A,l6)') trim(subname)//' profile_memory = ',profile_memory
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    ! Set clock_invalidTimeStamp
    call ESMF_TimeSet(time_invalidTimeStamp, yy=99999999, mm=1, dd=1, h=0, m=0, s=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    clock_invalidTimeStamp = ESMF_ClockCreate(clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ClockSet(clock_invalidTimeStamp, currTime=time_invalidTimeStamp, &
      stopTime=time_invalidTimeStamp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine InitializeP0

  !-----------------------------------------------------------------------

  subroutine InitializeIPDv03p1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    integer :: n
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p1)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! importable fields:

    ! add a namespace
    call NUOPC_AddNamespace(importState, namespace="ATM", nestedStateName="NestedState-AtmImp", nestedState=NState_AtmImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="OCN", nestedStateName="NestedState-OcnImp", nestedState=NState_OcnImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="ICE", nestedStateName="NestedState-IceImp", nestedState=NState_IceImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="LND", nestedStateName="NestedState-LndImp", nestedState=NState_LndImp, rc=rc)
    call NUOPC_AddNamespace(importState, namespace="HYD", nestedStateName="NestedState-HydImp", nestedState=NState_HydImp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="ATM", nestedStateName="NestedState-AtmExp", nestedState=NState_AtmExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="OCN", nestedStateName="NestedState-OcnExp", nestedState=NState_OcnExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="ICE", nestedStateName="NestedState-IceExp", nestedState=NState_IceExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="LND", nestedStateName="NestedState-LndExp", nestedState=NState_LndExp, rc=rc)
    call NUOPC_AddNamespace(exportState, namespace="HYD", nestedStateName="NestedState-HydExp", nestedState=NState_HydExp, rc=rc)

    do n = 1,fldsFrAtm%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": FrAtm Advertise "// &
          trim(fldsFrAtm%stdname(n))//":"// &
          trim(fldsFrAtm%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_AtmImp, &
        StandardName = trim(fldsFrAtm%stdname(n)), &
        name=fldsFrAtm%shortname(n), &
        TransferOfferGeomObject=fldsFrAtm%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

    do n = 1,fldsFrOcn%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": FrOcn Advertise "// &
          trim(fldsFrOcn%stdname(n))//":"// &
          trim(fldsFrOcn%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_OcnImp, &
        StandardName = fldsFrOcn%stdname(n), &
        name = fldsFrOcn%shortname(n), &
        TransferOfferGeomObject=fldsFrOcn%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

    do n = 1,fldsFrIce%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": FrIce Advertise "// &
          trim(fldsFrIce%stdname(n))//":"// &
          trim(fldsFrIce%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_IceImp, &
        StandardName = fldsFrIce%stdname(n), &
        name = fldsFrIce%shortname(n), &
        TransferOfferGeomObject=fldsFrIce%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

    do n = 1,fldsFrLnd%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": FrLnd Advertise "// &
          trim(fldsFrLnd%stdname(n))//":"// &
          trim(fldsFrLnd%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_LndImp, &
        StandardName = fldsFrLnd%stdname(n), &
        name = fldsFrLnd%shortname(n), &
        TransferOfferGeomObject=fldsFrLnd%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

    do n = 1,fldsFrHyd%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": FrHyd Advertise "// &
          trim(fldsFrHyd%stdname(n))//":"// &
          trim(fldsFrHyd%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_HydImp, &
        StandardName = fldsFrHyd%stdname(n), &
        name = fldsFrHyd%shortname(n), &
        TransferOfferGeomObject=fldsFrHyd%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo
      
    ! exportable fields:

    do n = 1,fldsToAtm%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": ToAtm Advertise "// &
          trim(fldsToAtm%stdname(n))//":"// &
          trim(fldsToAtm%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_AtmExp, &
        StandardName = fldsToAtm%stdname(n), &
        name = fldsToAtm%shortname(n), &
        TransferOfferGeomObject=fldsToAtm%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

    do n = 1,fldsToOcn%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": ToOcn Advertise "// &
          trim(fldsToOcn%stdname(n))//":"// &
          trim(fldsToOcn%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_OcnExp, &
        StandardName = fldsToOcn%stdname(n), &
        name = fldsToOcn%shortname(n), &
        TransferOfferGeomObject=fldsToOcn%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

    do n = 1,fldsToIce%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": ToIce Advertise "// &
          trim(fldsToIce%stdname(n))//":"// &
          trim(fldsToIce%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_IceExp, &
        StandardName = fldsToIce%stdname(n), &
        name = fldsToIce%shortname(n), &
        TransferOfferGeomObject=fldsToIce%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

    do n = 1,fldsToLnd%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": ToLnd Advertise "// &
          trim(fldsToLnd%stdname(n))//":"// &
          trim(fldsToLnd%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_LndExp, &
        StandardName = fldsToLnd%stdname(n), &
        name = fldsToLnd%shortname(n), &
        TransferOfferGeomObject=fldsToLnd%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

    do n = 1,fldsToHyd%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": ToHyd Advertise "// &
          trim(fldsToHyd%stdname(n))//":"// &
          trim(fldsToHyd%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_Advertise(NState_HydExp, &
        StandardName = fldsToHyd%stdname(n), &
        name = fldsToHyd%shortname(n), &
        TransferOfferGeomObject=fldsToHyd%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine InitializeIPDv03p1
  
  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv03p3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    integer                     :: i, j
    real(kind=ESMF_KIND_R8),pointer :: lonPtr(:,:), latPtr(:,:)
    type(InternalState)         :: is_local
    integer                     :: stat
    real(ESMF_KIND_R8)          :: intervalSec
    type(ESMF_TimeInterval)     :: timeStep
    character(ESMF_MAXSTR)      :: transferAction
! tcx XGrid
!    type(ESMF_Field)            :: fieldX, fieldA, fieldO
!    type(ESMF_XGrid)            :: xgrid
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p3)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! Allocate memory for the internal state and set it in the Component.
    allocate(is_local%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of the internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    ! Initialize the internal state members
    is_local%wrap%fastcntr = 1
    is_local%wrap%slowcntr = 1

    gridMed = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/nx_med,ny_med/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -85._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 85._ESMF_KIND_R8/), &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

!    gridLnd = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
!      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_med, ny_med, &
!      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out

!    gridHyd = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
!      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_med, ny_med, &
!      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- Generate RouteHandles
! tcx Xgrid
! what needs to be in the grids to create an XGrid (corners?)
! add error checking code

!    xgrid = ESMF_XGridCreate(sideAGrid=(/gridatm/), sideBGrid=(/gridocn/), rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
!    fieldX = ESMF_FieldCreate(xgrid  , typekind=ESMF_TYPEKIND_R8, rc=rc)
!    fieldA = ESMF_FieldCreate(gridAtm, typekind=ESMF_TYPEKIND_R8, rc=rc)
!    fieldO = ESMF_FieldCreate(gridAtm, typekind=ESMF_TYPEKIND_R8, rc=rc)
!    call ESMF_FieldRegridStore(xgrid, fieldA, fieldX, routehandle=is_local%wrap%RHa2x, rc=rc)
!    call ESMF_FieldRegridStore(xgrid, fieldO, fieldX, routehandle=is_local%wrap%RHo2x, rc=rc)
!    call ESMF_FieldRegridStore(xgrid, fieldX, fieldA, routehandle=is_local%wrap%RHx2a, rc=rc)
!    call ESMF_FieldRegridStore(xgrid, fieldX, fieldO, routehandle=is_local%wrap%RHx2o, rc=rc)
!    call ESMF_FieldDestroy(fieldX, rc=rc)
!    call ESMF_FieldDestroy(fieldA, rc=rc)
!    call ESMF_FieldDestroy(fieldO, rc=rc)
!    call ESMF_XGridDestroy(xgrid, rc=rc)

    !--- Importable fields from atm:

!gjt: import fields from ATM are now marked as "cannot provide" thus accept Grid
!gjt: -> eventually comment out the following lines...
    call realizeConnectedFields(NState_AtmImp, &
      fieldNameList=fldsFrAtm%shortname(1:fldsFrAtm%num), &
      string='AtmImp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- Exportable fields to atm:

    call realizeConnectedFields(NState_AtmExp, &
      fieldNameList=fldsToAtm%shortname(1:fldsToAtm%num), &
      string='AtmExp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- Importable fields from ocn:

    call realizeConnectedFields(NState_OcnImp, &
      fieldNameList=fldsFrOcn%shortname(1:fldsFrOcn%num), &
      string='OcnImp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- Exportable fields to ocn:

    call realizeConnectedFields(NState_OcnExp, &
      fieldNameList=fldsToOcn%shortname(1:fldsToOcn%num), &
      string='OcnExp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- Importable fields from ice:

    call realizeConnectedFields(NState_IceImp, &
      fieldNameList=fldsFrIce%shortname(1:fldsFrIce%num), &
      string='IceImp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- Exportable fields to ice:

    call realizeConnectedFields(NState_IceExp, &
      fieldNameList=fldsToIce%shortname(1:fldsToIce%num), &
      string='IceExp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- Importable fields from lnd:

    call realizeConnectedFields(NState_LndImp, &
      fieldNameList=fldsFrLnd%shortname(1:fldsFrLnd%num), &
      string='LndImp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- Exportable fields to lnd:

    call realizeConnectedFields(NState_LndExp, &
      fieldNameList=fldsToLnd%shortname(1:fldsToLnd%num), &
      string='LndExp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- Importable fields from hyd:

    call realizeConnectedFields(NState_HydImp, &
      fieldNameList=fldsFrHyd%shortname(1:fldsFrHyd%num), &
      string='HydImp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- Exportable fields to hyd:

    call realizeConnectedFields(NState_HydExp, &
      fieldNameList=fldsToHyd%shortname(1:fldsToHyd%num), &
      string='HydExp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Clean Up

!    call ESMF_GridDestroy(gridAtm, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
!    call ESMF_GridDestroy(gridOcn, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
    subroutine realizeConnectedFields(state, fieldNameList, grid, string, rc)
      type(ESMF_State)                :: state
      character(len=*)                :: fieldNameList(:)
      type(ESMF_Grid),optional        :: grid
      character(len=*)                :: string
      integer, intent(out)            :: rc

      integer                         :: n
      type(ESMF_Field)                :: field
      character(len=*),parameter :: subname='(module_MEDIATOR:realizeConnectedFields)'

      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      rc = ESMF_SUCCESS
      
      do n=1, size(fieldNameList)
        if (NUOPC_IsConnected(state, fieldName=fieldNameList(n))) then

          call ESMF_StateGet(state, field=field, itemName=fieldNameList(n), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
            value=transferAction, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          if (trim(transferAction) == "accept") then
            call ESMF_LogWrite(trim(subname)//trim(string)//" field+grid connected "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)

          else   ! provide

#ifdef NUOPC_DOES_SMART_GRID_TRANSFER
            ! realize the connected Field using the internal coupling Field
            if (.not.present(grid)) then
              call ESMF_LogWrite(trim(subname)//trim(string)//": ERROR grid expected", ESMF_LOGMSG_INFO, rc=rc)
              rc = ESMF_FAILURE
              return
            endif
            field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name=fieldNameList(n),rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call NUOPC_Realize(state, field=field, rc=rc)
            call ESMF_LogWrite(trim(subname)//trim(string)//" field connected      "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)

#else
            call NUOPC_SetAttribute(field, name="TransferActionGeomObject", &
              value="accept-internal", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            
#endif
          endif   ! transferAction

        else   ! StateIsFieldConnected

          ! remove a not connected Field from State
          call ESMF_StateRemove(state, (/fieldNameList(n)/), rc=rc)
          call ESMF_LogWrite(trim(subname)//trim(string)//" field NOT connected  "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
      enddo
      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

    end subroutine realizeConnectedFields

  end subroutine InitializeIPDv03p3
  
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv03p4(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
!    type(ESMF_Field)              :: field
!    type(ESMF_Grid)               :: grid
!    integer                       :: localDeCount

!    type(ESMF_DistGrid)           :: distgrid
!    integer                       :: dimCount, tileCount, petCount
!    integer                       :: deCountPTile, extraDEs
!    integer, allocatable          :: minIndexPTile(:,:), maxIndexPTile(:,:)
!    integer, allocatable          :: regDecompPTile(:,:)
!    integer                       :: i, j, n, n1
!    character(ESMF_MAXSTR)        :: transferAction
    
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p4)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    rc = ESMF_SUCCESS

    call realizeConnectedGrid(NState_atmImp, 'AtmImp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call realizeConnectedGrid(NState_atmExp, 'AtmExp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call realizeConnectedGrid(NState_ocnImp, 'OcnImp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call realizeConnectedGrid(NState_ocnExp, 'OcnExp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call realizeConnectedGrid(NState_iceImp, 'IceImp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call realizeConnectedGrid(NState_iceExp, 'IceExp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call realizeConnectedGrid(NState_lndImp, 'LndImp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call realizeConnectedGrid(NState_lndExp, 'LndExp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call realizeConnectedGrid(NState_hydImp, 'HydImp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call realizeConnectedGrid(NState_hydExp, 'HydExp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine realizeConnectedGrid(State,string,rc)

      type(ESMF_State)   , intent(inout) :: State
      character(len=*)   , intent(in)    :: string
      integer            , intent(out)   :: rc
    
      ! local variables
      type(ESMF_Field)              :: field
      type(ESMF_Grid)               :: grid
      integer                       :: localDeCount

      type(ESMF_DistGrid)           :: distgrid
      type(ESMF_DistGridConnection), allocatable :: connectionList(:)
      integer                       :: dimCount, tileCount, petCount
      integer                       :: connectionCount
      integer                       :: deCountPTile, extraDEs
      integer, allocatable          :: minIndexPTile(:,:), maxIndexPTile(:,:)
      integer, allocatable          :: regDecompPTile(:,:)
      integer                       :: i, j, n, n1, fieldCount, nxg
      character(ESMF_MAXSTR),allocatable :: fieldNameList(:)
      character(ESMF_MAXSTR)        :: transferAction
      character(len=*),parameter :: subname='(module_MEDIATOR:realizeConnectedGrid)'
    
      !NOTE: All fo the Fields that set their TransferOfferGeomObject Attribute
      !NOTE: to "cannot provide" should now have the accepted Grid available.
      !NOTE: Go and pull out this Grid for one of a representative Field and 
      !NOTE: modify the decomposition and distribution of the Grid to match the
      !NOTE: Mediator PETs.

      !TODO: quick implementation, do it for each field one by one
      !TODO: commented out below are application to other fields

      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      rc = ESMF_Success

      call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(State, itemNameList=fieldNameList, &
        itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      do n=1, fieldCount
!tcx    do n=1, 1

        call ESMF_StateGet(State, field=field, itemName=fieldNameList(n), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
          value=transferAction, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        if (trim(transferAction) == "accept") then
          if (dbug_flag > 1) then
            call ESMF_LogWrite(trim(subname)//trim(string)//": accept grid for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          endif
          ! while this is still an empty field, it does now hold a Grid with DistGrid
          call ESMF_FieldGet(field, grid=grid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          call Grid_Print(grid,trim(fieldNameList(n))//'_orig',rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          ! access localDeCount to show this is a real Grid
          call ESMF_GridGet(grid, localDeCount=localDeCount, distgrid=distgrid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, file=__FILE__)) return  ! bail out
   
          ! Create a custom DistGrid, based on the minIndex, maxIndex of the 
          ! accepted DistGrid, but with a default regDecomp for the current VM
          ! that leads to 1DE/PET.
    
          ! get dimCount and tileCount
          call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, &
            connectionCount=connectionCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
          allocate(minIndexPTile(dimCount, tileCount), &
                   maxIndexPTile(dimCount, tileCount))
          allocate(connectionList(connectionCount))
    
          ! get minIndex and maxIndex arrays, and connectionList
          call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
            maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
      
          ! construct a default regDecompPTile -> TODO: move this into ESMF as default
          call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          allocate(regDecompPTile(dimCount, tileCount))
          deCountPTile = petCount/tileCount
          extraDEs = max(0, petCount-deCountPTile)
          do i=1, tileCount
            if (i<=extraDEs) then
              regDecompPTile(1, i) = deCountPTile + 1
            else
              regDecompPTile(1, i) = deCountPTile
            endif
            do j=2, dimCount
              regDecompPTile(j, i) = 1
            enddo
          enddo
    
!--- tcraig, hardwire i direction wraparound, temporary
!--- tcraig, now getting info from model distgrid, see above
!          allocate(connectionList(1))
!          nxg = maxIndexPTile(1,1) - minIndexPTile(1,1) + 1
!          write(msgstring,*) trim(subname)//trim(string),': connlist nxg = ',nxg
!          call ESMF_LogWrite(trim(msgstring), ESMF_LOGMSG_INFO, rc=rc)
!          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!            line=__LINE__, file=__FILE__)) return  ! bail out
!          call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1, &
!            tileIndexB=1, positionVector=(/nxg, 0/), rc=rc)
!          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!            line=__LINE__, file=__FILE__)) return  ! bail out

          ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
          ! but with a default regDecompPTile
! tcraig, force connectionlist and gridEdge arguments to fix wraparound
! add XX to turn this off, remove XX to turn this on.  
! need ESMF fixes to implement properly.
!          if (trim(string) == 'XXOcnImp' .or. trim(string) == 'XXOcnExp' .or. &
!              trim(string) == 'XXIceImp' .or. trim(string) == 'XXIceExp') then
          if (trim(string) == 'OcnImp' .or. trim(string) == 'OcnExp' .or. &
              trim(string) == 'AtmImp' .or. trim(string) == 'AtmExp' .or. &
              trim(string) == 'IceImp' .or. trim(string) == 'IceExp' .or. &
              trim(string) == 'XXLndImp' .or. trim(string) == 'XXLndExp' .or. &
              trim(string) == 'XXHydImp' .or. trim(string) == 'XXHydExp' ) then
            distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
              maxIndexPTile=maxIndexPTile, regDecompPTile=regDecompPTile, &
              connectionList=connectionList, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (dbug_flag > 1) then
              call ESMF_LogWrite(trim(subname)//trim(string)//': distgrid with connlist', ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif
            ! Create a new Grid on the new DistGrid and swap it in the Field
            grid = ESMF_GridCreate(distgrid, &
              gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          else
            distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
              maxIndexPTile=maxIndexPTile, regDecompPTile=regDecompPTile, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (dbug_flag > 1) then
              call ESMF_LogWrite(trim(subname)//trim(string)//': distgrid without connlist', ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif
            ! Create a new Grid on the new DistGrid and swap it in the Field
            grid = ESMF_GridCreate(distgrid, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif

          ! local clean-up
          deallocate(connectionList)
          deallocate(minIndexPTile, maxIndexPTile, regDecompPTile)

          ! Swap all the Grids in the State
    
!tcx         do n1=1, fieldCount
          do n1=n,n
            ! access a field in the importState and set the Grid
            call ESMF_StateGet(State, field=field, &
              itemName=fieldNameList(n1), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (dbug_flag > 1) then
              call ESMF_LogWrite(trim(subname)//trim(string)//": attach grid for "//trim(fieldNameList(n1)), ESMF_LOGMSG_INFO, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif
            call Grid_print(grid,trim(fieldNameList(n))//'_new',rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          enddo

        else

          call ESMF_LogWrite(trim(subname)//trim(string)//": provide grid for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)

        endif   ! accept

      enddo   ! nflds

      deallocate(fieldNameList)

      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

    end subroutine realizeConnectedGrid

  end subroutine InitializeIPDv03p4
  
  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv03p5(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Field)            :: field, field1, field2
    type(ESMF_Field)            :: fieldArea
    type(ESMF_Grid)             :: grid
    type(InternalState)         :: is_local
    integer                     :: fieldCount
    real(ESMF_KIND_R8), pointer :: factorList(:)
    character(ESMF_MAXSTR)      :: name
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    integer                     :: i,j
    type(ESMF_Field)            :: fieldAtm, fieldOcn
    type(ESMF_Array)            :: arrayOcn, arrayIce
    type(ESMF_RouteHandle)      :: RH_mapmask  ! unmasked conservative remapping 
    type(ESMF_Grid)             :: gridAtmCoord, gridOcnCoord
    integer(ESMF_KIND_I4), pointer :: dataPtr_arrayOcn(:,:), dataPtr_arrayIce(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fieldOcn(:,:), dataPtr_fieldAtm(:,:)
    logical                     :: isPresentOcn, isPresentIce
    character(len=*),parameter  :: subname='(module_MEDIATOR:InitializeIPDv03p5)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    rc = ESMF_SUCCESS

    !----------------------------------------------------------
    !--- Finish initializing the State Fields
    !----------------------------------------------------------

    call completeFieldInitialization(NState_atmImp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call state_reset(NState_atmImp, value=spval_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call completeFieldInitialization(NState_atmExp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call state_reset(NState_atmExp, value=spval_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call completeFieldInitialization(NState_ocnImp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call state_reset(NState_ocnImp, value=spval_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call completeFieldInitialization(NState_ocnExp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call state_reset(NState_ocnExp, value=spval_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call completeFieldInitialization(NState_iceImp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call state_reset(NState_iceImp, value=spval_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call completeFieldInitialization(NState_iceExp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call state_reset(NState_iceExp, value=spval_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call completeFieldInitialization(NState_lndImp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call state_reset(NState_lndImp, value=spval_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call completeFieldInitialization(NState_lndExp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call state_reset(NState_lndExp, value=spval_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call completeFieldInitialization(NState_hydImp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call state_reset(NState_hydImp, value=spval_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call completeFieldInitialization(NState_hydExp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call state_reset(NState_hydExp, value=spval_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !----------------------------------------------------------
    !--- Set the model grids using first field in each model's import state
    !----------------------------------------------------------

!tcraig old version
!    call ESMF_StateGet(NState_atmImp, field=field, itemName=fldsFrAtm%shortname(1), rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out

!    call ESMF_FieldGet(field, grid=gridAtm, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out

!    call ESMF_StateGet(NState_ocnImp, field=field, itemName=fldsFrOcn%shortname(1), rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out

!    call ESMF_FieldGet(field, grid=gridOcn, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_StateGet(NState_atmImp, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_atmImp, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_StateGet(NState_atmImp, field=field, itemName=fieldNameList(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_FieldGet(field, grid=gridAtm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      deallocate(fieldNameList)
    else
      gridAtm = gridMed
    endif

    call ESMF_StateGet(NState_ocnImp, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_ocnImp, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_StateGet(NState_ocnImp, field=field, itemName=fieldNameList(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_FieldGet(field, grid=gridOcn, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      deallocate(fieldNameList)
    else
      gridOcn = gridMed
    endif

    call ESMF_StateGet(NState_iceImp, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_iceImp, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_StateGet(NState_iceImp, field=field, itemName=fieldNameList(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_FieldGet(field, grid=gridIce, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      deallocate(fieldNameList)
    else
      gridIce = gridMed
    endif

! Land will pick up the grid from the first field exported to Land

    call ESMF_StateGet(NState_lndExp, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_lndExp, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_StateGet(NState_lndExp, field=field, itemName=fieldNameList(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_FieldGet(field, grid=gridLnd, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      call ESMF_GridGetCoord(gridLnd, staggerloc=ESMF_STAGGERLOC_CENTER, &
        isPresent=isPresent,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (.NOT. isPresent) then
        call NEMS_GridCopyCoord(gridcomp=gcomp, gridSrc=gridAtm, gridDst=gridLnd, &
          staggerloc=(/ESMF_STAGGERLOC_CENTER/), invert=(/2/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_LogWrite(trim(subname)// &
          ": Copied gridATM center coordinates to gridLnd", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

      call ESMF_GridGetCoord(gridLnd, staggerloc=ESMF_STAGGERLOC_CORNER, &
        isPresent=isPresent,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      if (.NOT. isPresent) then
        call NEMS_GridCopyCoord(gridcomp=gcomp, gridSrc=gridAtm, gridDst=gridLnd, &
          staggerloc=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), invert=(/2/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_LogWrite(trim(subname)// &
          ": Copied gridATM center and corner coordinates to gridLnd", ESMF_LOGMSG_INFO, rc=dbrc)
        call NEMS_GridCopyItem(gridcomp=gcomp, gridSrc=gridAtm, gridDst=gridLnd, &
          item=(/ESMF_GRIDITEM_AREA/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_LogWrite(trim(subname)// &
          ": Copied gridATM areas to gridLnd", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      deallocate(fieldNameList)
    else
      gridLnd = gridMed
    endif

! Hydro will pick up the grid from the first field exported to Hydro

    call ESMF_StateGet(NState_hydExp, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_hydExp, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_StateGet(NState_hydExp, field=field, itemName=fieldNameList(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_FieldGet(field, grid=gridHyd, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      deallocate(fieldNameList)
    else
      gridHyd = gridMed
    endif

    !----------------------------------------------------------
    !--- Diagnose Grid Info
    !----------------------------------------------------------

    call Grid_Print(gridAtm,'gridAtm',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call Grid_Print(gridOcn,'gridOcn',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call Grid_Print(gridIce,'gridIce',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call Grid_Print(gridLnd,'gridLnd',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call Grid_Print(gridHyd,'gridHyd',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call Grid_Print(gridMed,'gridMed',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

#if 1
    !----------------------------------------------------------
    ! dump the Grid coordinate arrays for reference      
    !----------------------------------------------------------

    call Grid_Write(gridAtm, 'array_med_atm', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call Grid_Write(gridOcn, 'array_med_ocn', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call Grid_Write(gridIce, 'array_med_ice', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call Grid_Write(gridLnd, 'array_med_lnd', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call Grid_Write(gridHyd, 'array_med_hyd', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call Grid_Write(gridMed, 'array_med_med', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

#endif

    !----------------------------------------------------------
    ! NOW allocate other Mediator datatypes
    !----------------------------------------------------------

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !----------------------------------------------------------
    ! Initialize FB for each model import states on each grid
    !----------------------------------------------------------

    !--- atm

    call fieldBundle_init(is_local%wrap%FBAtm_a, grid=gridAtm, &
      state=NState_AtmImp, name='FBAtm_a', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBAtm_o, grid=gridOcn, &
      state=NState_AtmImp, name='FBAtm_o', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBAtm_i, grid=gridIce, &
      state=NState_AtmImp, name='FBAtm_i', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBAtm_l, grid=gridLnd, &
      state=NState_AtmImp, name='FBAtm_l', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBAtm_h, grid=gridHyd, &
      state=NState_AtmImp, name='FBAtm_h', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- ocn

    call fieldBundle_init(is_local%wrap%FBOcn_a, grid=gridAtm, &
      state=NState_OcnImp, name='FBOcn_a', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBOcn_o, grid=gridOcn, &
      state=NState_OcnImp, name='FBOcn_o', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBOcn_i, grid=gridIce, &
      state=NState_OcnImp, name='FBOcn_i', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- ice

    call fieldBundle_init(is_local%wrap%FBIce_a, grid=gridAtm, &
      state=NState_IceImp, name='FBIce_a', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBIce_o, grid=gridOcn, &
      state=NState_IceImp, name='FBIce_o', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBIce_i, grid=gridIce, &
      state=NState_IceImp, name='FBIce_i', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBIce_if, grid=gridIce, &
      state=NState_IceImp, name='FBIce_if', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- lnd

    call fieldBundle_init(is_local%wrap%FBLnd_a, grid=gridAtm, &
      state=NState_LndImp, name='FBLnd_a', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBLnd_l, grid=gridLnd, &
      state=NState_LndImp, name='FBLnd_l', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBLnd_h, grid=gridHyd, &
      state=NState_LndImp, name='FBLnd_h', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- hyd

    call fieldBundle_init(is_local%wrap%FBHyd_l, grid=gridLnd, &
      state=NState_HydImp, name='FBHyd_l', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBHyd_a, grid=gridAtm, &
      state=NState_HydImp, name='FBHyd_a', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBHyd_h, grid=gridHyd, &
      state=NState_HydImp, name='FBHyd_h', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !----------------------------------------------------------
    ! Initialize Accumulators
    !----------------------------------------------------------

    call fieldBundle_init(is_local%wrap%FBaccumAtm, grid=gridAtm, &
      state=NState_AtmImp, name='FBaccumAtm', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBaccumOcn, grid=gridOcn, &
      state=NState_OcnImp, name='FBaccumOcn', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBaccumIce, grid=gridIce, &
      state=NState_IceImp, name='FBaccumIce', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBaccumLnd, grid=gridLnd, &
      state=NState_LndImp, name='FBaccumLnd', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBaccumHyd, grid=gridHyd, &
      state=NState_HydImp, name='FBaccumHyd', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !----------------------------------------------------------
    ! Initialize AtmOcn FBs
    !----------------------------------------------------------

    call fieldBundle_init(is_local%wrap%FBAtmOcn_o, grid=gridOcn, &
      fieldnamelist=fldsAtmOcn%shortname(1:fldsAtmOcn%num), name='FBAtmOcn_o', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBAtmOcn_a, grid=gridAtm, &
      fieldnamelist=fldsAtmOcn%shortname(1:fldsAtmOcn%num), name='FBAtmOcn_a', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBaccumAtmOcn, grid=gridOcn, &
      fieldnamelist=fldsAtmOcn%shortname(1:fldsAtmOcn%num), name='FBaccumAtmOcn', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !----------------------------------------------------------
    ! Initialize FB for export to models
    !----------------------------------------------------------

    call fieldBundle_init(is_local%wrap%FBforAtm, &
      state=NState_AtmExp, name='FBforAtm', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBforOcn, &
      state=NState_OcnExp, name='FBforOcn', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBforIce, &
      state=NState_IceExp, name='FBforIce', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBforLnd, &
      state=NState_LndExp, name='FBforLnd', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_init(is_local%wrap%FBforHyd, &
      state=NState_HydExp, name='FBforHyd', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !----------------------------------------------------------
    !--- Check for active regrid directions
    !----------------------------------------------------------
    
    ! initialize
    is_local%wrap%a2o_active = .false.
    is_local%wrap%a2i_active = .false.
    is_local%wrap%a2l_active = .false.
    is_local%wrap%a2h_active = .false.
    is_local%wrap%o2a_active = .false.
    is_local%wrap%o2i_active = .false.
    is_local%wrap%i2a_active = .false.
    is_local%wrap%i2o_active = .false.
    is_local%wrap%l2a_active = .false.
    is_local%wrap%l2h_active = .false.
    is_local%wrap%h2l_active = .false.
    is_local%wrap%h2a_active = .false.

    ! a2o, a2i, a2l, a2h
    call ESMF_FieldBundleGet(is_local%wrap%FBAtm_a, fieldCount=fieldCount, rc=rc) ! Atmosphere Export Field Count
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforOcn, fieldCount=fieldCount, rc=rc) ! Ocean Import Field Count
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (fieldCount > 0) then
        is_local%wrap%a2o_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforIce, fieldCount=fieldCount, rc=rc) ! Ice Import Field Count
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (fieldCount > 0) then
        is_local%wrap%a2i_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforLnd, fieldCount=fieldCount, rc=rc) ! Land Import Field Count
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (fieldCount > 0) then
        is_local%wrap%a2l_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforHyd, fieldCount=fieldCount, rc=rc) ! Hydro Import Field Count
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (fieldCount > 0) then
        is_local%wrap%a2h_active = .true.
      endif
    endif
    
    ! o2a, o2i
    call ESMF_FieldBundleGet(is_local%wrap%FBOcn_o, fieldCount=fieldCount, rc=rc) ! Ocean Export Field Count
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforAtm, fieldCount=fieldCount, rc=rc) ! Atmosphere Import Field Count
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (fieldCount > 0) then
        is_local%wrap%o2a_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforIce, fieldCount=fieldCount, rc=rc) ! Ice Import Field Count
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (fieldCount > 0) then
        is_local%wrap%o2i_active = .true.
      endif
    endif
    
    ! i2a, i2o
    call ESMF_FieldBundleGet(is_local%wrap%FBIce_i, fieldCount=fieldCount, rc=rc) ! Ice Export Field Count
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforAtm, fieldCount=fieldCount, rc=rc) ! Atmosphere Import Field Count
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (fieldCount > 0) then
        is_local%wrap%i2a_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforOcn, fieldCount=fieldCount, rc=rc) ! Ocean Import Field Count
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (fieldCount > 0) then
        is_local%wrap%i2o_active = .true.
      endif
    endif
    
    ! l2a, l2h
    call ESMF_FieldBundleGet(is_local%wrap%FBLnd_l, fieldCount=fieldCount, rc=rc) ! Land Export Field Count
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforAtm, fieldCount=fieldCount, rc=rc) ! Atmosphere Import Field Count
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (fieldCount > 0) then
        is_local%wrap%l2a_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforHyd, fieldCount=fieldCount, rc=rc) ! Hyd Import Field Count
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (fieldCount > 0) then
        is_local%wrap%l2h_active = .true.
      endif
    endif

    ! h2l, h2a
    call ESMF_FieldBundleGet(is_local%wrap%FBHyd_h, fieldCount=fieldCount, rc=rc) ! Hydro Export Field Count
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is_local%wrap%FBforLnd, fieldCount=fieldCount, rc=rc) ! Land Import Field Count
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (fieldCount > 0) then
        is_local%wrap%h2l_active = .true.
      endif
      call ESMF_FieldBundleGet(is_local%wrap%FBforAtm, fieldCount=fieldCount, rc=rc) ! Atmosphere Import Field Count
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (fieldCount > 0) then
        is_local%wrap%h2a_active = .true.
      endif
    endif

    write(msgString,*) is_local%wrap%a2o_active
    call ESMF_LogWrite(trim(subname)//": a2o active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%a2i_active
    call ESMF_LogWrite(trim(subname)//": a2i active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%a2l_active
    call ESMF_LogWrite(trim(subname)//": a2l active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%a2h_active
    call ESMF_LogWrite(trim(subname)//": a2h active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    write(msgString,*) is_local%wrap%o2a_active
    call ESMF_LogWrite(trim(subname)//": o2a active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%o2i_active
    call ESMF_LogWrite(trim(subname)//": o2i active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    write(msgString,*) is_local%wrap%i2a_active
    call ESMF_LogWrite(trim(subname)//": i2a active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%i2o_active
    call ESMF_LogWrite(trim(subname)//": i2o active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    write(msgString,*) is_local%wrap%l2a_active
    call ESMF_LogWrite(trim(subname)//": l2a active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%l2h_active
    call ESMF_LogWrite(trim(subname)//": l2h active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    write(msgString,*) is_local%wrap%h2l_active
    call ESMF_LogWrite(trim(subname)//": h2l active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) is_local%wrap%h2a_active
    call ESMF_LogWrite(trim(subname)//": h2a active: " // trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    !----------------------------------------------------------
    !--- Initialize route handles
    !----------------------------------------------------------

    if (dbug_flag > 1) then
      call ESMF_LogWrite("Starting to initialize RHs", ESMF_LOGMSG_INFO)
      call ESMF_LogFlush()
    endif

    if (is_local%wrap%a2o_active) then
      call Compute_RHs(FBsrc=is_local%wrap%FBAtm_a, FBdst=is_local%wrap%FBAtm_o, &
        bilnrmap=is_local%wrap%RH_a2o_bilnr, &
        consfmap=is_local%wrap%RH_a2o_consf, &
        consdmap=is_local%wrap%RH_a2o_consd, &
        patchmap=is_local%wrap%RH_a2o_patch, &
        fcopymap=is_local%wrap%RH_a2o_fcopy, &
        dstMaskValue=0, &
        fldlist1=FldsFrAtm, string='a2o_weights', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%a2i_active) then
      call Compute_RHs(FBsrc=is_local%wrap%FBAtm_a, FBdst=is_local%wrap%FBAtm_i, &
        bilnrmap=is_local%wrap%RH_a2i_bilnr, &
        consfmap=is_local%wrap%RH_a2i_consf, &
        consdmap=is_local%wrap%RH_a2i_consd, &
        patchmap=is_local%wrap%RH_a2i_patch, &
        fcopymap=is_local%wrap%RH_a2i_fcopy, &
        dstMaskValue=0, &
        fldlist1=FldsFrAtm, string='a2i_weights', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%a2l_active) then
      call Compute_RHs(FBsrc=is_local%wrap%FBAtm_a, FBdst=is_local%wrap%FBAtm_l, &
        bilnrmap=is_local%wrap%RH_a2l_bilnr, &
        consfmap=is_local%wrap%RH_a2l_consf, &
        consdmap=is_local%wrap%RH_a2l_consd, &
        patchmap=is_local%wrap%RH_a2l_patch, &
        fcopymap=is_local%wrap%RH_a2l_fcopy, &
        fldlist1=FldsFrAtm, string='a2l_weights', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%a2h_active) then
      call Compute_RHs(FBsrc=is_local%wrap%FBAtm_a, FBdst=is_local%wrap%FBAtm_h, &
        bilnrmap=is_local%wrap%RH_a2h_bilnr, &
        consfmap=is_local%wrap%RH_a2h_consf, &
        consdmap=is_local%wrap%RH_a2h_consd, &
        patchmap=is_local%wrap%RH_a2h_patch, &
        fcopymap=is_local%wrap%RH_a2h_fcopy, &
        fldlist1=FldsFrAtm, string='a2h_weights', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%o2a_active) then
      call Compute_RHs(FBsrc=is_local%wrap%FBOcn_o, FBdst=is_local%wrap%FBOcn_a, &
        bilnrmap=is_local%wrap%RH_o2a_bilnr, &
        consfmap=is_local%wrap%RH_o2a_consf, &
        consdmap=is_local%wrap%RH_o2a_consd, &
        patchmap=is_local%wrap%RH_o2a_patch, &
        fcopymap=is_local%wrap%RH_o2a_fcopy, &
        srcMaskValue=0, &
        fldlist1=FldsFrOcn, fldlist2=FldsAtmOcn, string='o2a_weights', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%o2i_active) then
      call Compute_RHs(FBsrc=is_local%wrap%FBOcn_o, FBdst=is_local%wrap%FBOcn_i, &
        bilnrmap=is_local%wrap%RH_o2i_bilnr, &
        consfmap=is_local%wrap%RH_o2i_consf, &
        consdmap=is_local%wrap%RH_o2i_consd, &
        patchmap=is_local%wrap%RH_o2i_patch, &
        fcopymap=is_local%wrap%RH_o2i_fcopy, &
        srcMaskValue=0, dstMaskValue=0, &
        fldlist1=FldsFrOcn, string='o2i_weights', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%i2a_active) then
      call Compute_RHs(FBsrc=is_local%wrap%FBIce_i, FBdst=is_local%wrap%FBIce_a, &
        bilnrmap=is_local%wrap%RH_i2a_bilnr, &
        consfmap=is_local%wrap%RH_i2a_consf, &
        consdmap=is_local%wrap%RH_i2a_consd, &
        patchmap=is_local%wrap%RH_i2a_patch, &
        fcopymap=is_local%wrap%RH_i2a_fcopy, &
        srcMaskValue=0, &
        fldlist1=FldsFrIce, string='i2a_weights', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%i2o_active) then
      call Compute_RHs(FBsrc=is_local%wrap%FBIce_i, FBdst=is_local%wrap%FBIce_o, &
        bilnrmap=is_local%wrap%RH_i2o_bilnr, &
        consfmap=is_local%wrap%RH_i2o_consf, &
        consdmap=is_local%wrap%RH_i2o_consd, &
        patchmap=is_local%wrap%RH_i2o_patch, &
        fcopymap=is_local%wrap%RH_i2o_fcopy, &
        srcMaskValue=0, dstMaskValue=0, &
        fldlist1=FldsFrIce, string='i2o_weights', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%l2a_active) then
      call Compute_RHs(FBsrc=is_local%wrap%FBLnd_l, FBdst=is_local%wrap%FBLnd_a, &
        bilnrmap=is_local%wrap%RH_l2a_bilnr, &
        consfmap=is_local%wrap%RH_l2a_consf, &
        consdmap=is_local%wrap%RH_l2a_consd, &
        patchmap=is_local%wrap%RH_l2a_patch, &
        fcopymap=is_local%wrap%RH_l2a_fcopy, &
        fldlist1=FldsFrLnd, string='l2a_weights', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%l2h_active) then
      call Compute_RHs(FBsrc=is_local%wrap%FBLnd_l, FBdst=is_local%wrap%FBLnd_h, &
        bilnrmap=is_local%wrap%RH_l2h_bilnr, &
        consfmap=is_local%wrap%RH_l2h_consf, &
        consdmap=is_local%wrap%RH_l2h_consd, &
        patchmap=is_local%wrap%RH_l2h_patch, &
        fcopymap=is_local%wrap%RH_l2h_fcopy, &
        fldlist1=FldsFrLnd, string='l2h_weights', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%h2a_active) then
      call Compute_RHs(FBsrc=is_local%wrap%FBHyd_h, FBdst=is_local%wrap%FBHyd_a, &
        bilnrmap=is_local%wrap%RH_h2a_bilnr, &
        consfmap=is_local%wrap%RH_h2a_consf, &
        consdmap=is_local%wrap%RH_h2a_consd, &
        patchmap=is_local%wrap%RH_h2a_patch, &
        fcopymap=is_local%wrap%RH_h2a_fcopy, &
        fldlist1=FldsFrHyd, string='h2a_weights', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%h2l_active) then
      call Compute_RHs(FBsrc=is_local%wrap%FBHyd_h, FBdst=is_local%wrap%FBHyd_l, &
        bilnrmap=is_local%wrap%RH_h2l_bilnr, &
        consfmap=is_local%wrap%RH_h2l_consf, &
        consdmap=is_local%wrap%RH_h2l_consd, &
        patchmap=is_local%wrap%RH_h2l_patch, &
        fcopymap=is_local%wrap%RH_h2l_fcopy, &
        fldlist1=FldsFrHyd, string='h2l_weights', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    !--- land mask

    if (generate_landmask) then

      call ESMF_GridGetItem(gridOcn, itemflag=ESMF_GRIDITEM_MASK, staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresentOcn, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call ESMF_GridGetItem(gridIce, itemflag=ESMF_GRIDITEM_MASK, staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresentIce, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      if (isPresentOcn .or. isPresentIce) then

        if (isPresentOcn .and. isPresentIce) then

          ! ocn mask from ocn grid

          call ESMF_GridGetItem(gridOcn, staggerLoc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_MASK, array=arrayOcn, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          call ESMF_GridGetItem(gridOcn, staggerLoc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dataPtr_arrayOcn, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          call ESMF_ArraySet(arrayOcn, name="ocean_mask", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          write (msgString,*) trim(subname)//"ocn_mask raw = ",minval(dataPtr_arrayOcn),maxval(dataPtr_arrayOcn)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          call ESMF_ArrayWrite(arrayOcn, 'field_med_ocn_a_ocean_mask.nc', rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

          ! ice mask from ice grid

          call ESMF_GridGetItem(gridIce, staggerLoc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_MASK, array=arrayIce, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          call ESMF_GridGetItem(gridIce, staggerLoc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dataPtr_arrayIce, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          call ESMF_ArraySet(arrayIce, name="ice_mask", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          write (msgString,*) trim(subname)//"ice_mask raw = ",minval(dataPtr_arrayIce),maxval(dataPtr_arrayIce)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
          call ESMF_ArrayWrite(arrayIce, 'field_med_ocn_a_ice_mask.nc', rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

          ! generate ocn grid with just coords, no mask or area
          ! create ocn/ice mask field on ocn grid, coords only

          call Grid_CreateCoords(gridOcnCoord, gridOcn, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          fieldOcn = ESMF_FieldCreate(gridOcnCoord, ESMF_TYPEKIND_R8, name='ocnice_mask', rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          call ESMF_FieldGet(fieldOcn, farrayPtr=dataPtr_fieldOcn, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

          ! generate atm grid with just coords, no mask or area
          ! create land mask field on atm grid, coords only

          call Grid_CreateCoords(gridAtmCoord, gridAtm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          fieldAtm = ESMF_FieldCreate(gridAtmCoord, ESMF_TYPEKIND_R8, name='land_mask', rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          call ESMF_FieldGet(fieldAtm, farrayPtr=dataPtr_FieldAtm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

          ! Here, the ocean/ice mask is the intersection of ocean and ice masks, which are integer fields of 0 or 1
          ! Convert to real and make sure values are only 0 or 1.

          do j = lbound(dataPtr_fieldOcn,2),ubound(dataPtr_fieldOcn,2)
          do i = lbound(dataPtr_fieldOcn,1),ubound(dataPtr_fieldOcn,1)
            dataPtr_fieldOcn(i,j) = min(dataPtr_arrayIce(i,j),dataPtr_arrayOcn(i,j))
            if (dataPtr_fieldOcn(i,j) < 0.50_ESMF_KIND_R8) then
              dataPtr_fieldOcn(i,j) = 0.0_ESMF_KIND_R8
            else
              dataPtr_fieldOcn(i,j) = 1.0_ESMF_KIND_R8
            endif
          enddo
          enddo

          ! generate a new RH from Atm and Ocn coords, no masks, no areas.  Should not use o2a_consd mapping
          ! because it has masks and area corrections.

          call ESMF_FieldRegridStore(fieldOcn, fieldAtm, routehandle=RH_mapmask, &
            regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
            srcTermProcessing=srcTermProcessing_Value, &
            ignoreDegenerate=.true., &
            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

          ! regrid ocean mask from ocn to atm grid using unmasked conservative mapping

          if (ESMF_RouteHandleIsCreated(RH_mapmask, rc=rc)) then
            dataPtr_fieldAtm = 0.0_ESMF_KIND_R8
            call ESMF_FieldRegrid(fieldOcn, fieldAtm, routehandle=RH_mapmask, &
              termorderflag=ESMF_TERMORDER_SRCSEQ, zeroregion=ESMF_REGION_TOTAL, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
            call ESMF_FieldRegridRelease(RH_mapmask, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          else
            call ESMF_LogWrite(trim(subname)//": ERROR RH_mapmask not created", ESMF_LOGMSG_INFO, rc=rc)
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
          endif

          ! convert from ocean mask to land mask
          ! check min/max
          ! also fill "land_mask" array and save it for later

          allocate(land_mask(lbound(dataPtr_fieldAtm,1):ubound(dataPtr_fieldAtm,1),lbound(dataPtr_fieldAtm,2):ubound(dataPtr_fieldAtm,2)))
          do j = lbound(dataPtr_fieldAtm,2),ubound(dataPtr_fieldAtm,2)
          do i = lbound(dataPtr_fieldAtm,1),ubound(dataPtr_fieldAtm,1)
            dataPtr_fieldAtm(i,j) = 1.0_ESMF_KIND_R8 - dataPtr_fieldAtm(i,j)
            if (dataPtr_fieldAtm(i,j) > 1.0_ESMF_KIND_R8) dataPtr_fieldAtm(i,j) = 1.0_ESMF_KIND_R8
            if (dataPtr_fieldAtm(i,j) < 1.0e-6_ESMF_KIND_R8) dataPtr_fieldAtm(i,j) = 0.0_ESMF_KIND_R8
            land_mask(i,j) = dataPtr_fieldAtm(i,j)
          enddo
          enddo

          ! write out masks

          call ESMF_FieldWrite(fieldOcn,'field_med_ocn_a_ocnice_mask.nc',overwrite=.true.,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__, file=__FILE__)) return
          write (msgString,*) trim(subname)//"ocean_mask = ",minval(dataPtr_fieldOcn),maxval(dataPtr_fieldOcn)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

          call ESMF_FieldWrite(fieldAtm,'field_med_atm_a_land_mask.nc',overwrite=.true.,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__, file=__FILE__)) return
          write (msgString,*) trim(subname)//"land_mask = ",minval(dataPtr_fieldAtm),maxval(dataPtr_fieldAtm)
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

          ! clean up

          call ESMF_GridDestroy(gridAtmCoord,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__, file=__FILE__)) return
          call ESMF_FieldDestroy(fieldAtm,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__, file=__FILE__)) return
          call ESMF_GridDestroy(gridOcnCoord,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__, file=__FILE__)) return
          call ESMF_FieldDestroy(fieldOcn,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__, file=__FILE__)) return

        else  ! isPresentOcn .and. isPresentIce
          call ESMF_LogWrite(trim(subname)//": ABORT more support needed for Ocn or Ice mask", ESMF_LOGMSG_INFO, rc=rc)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        endif

      endif    ! isPresentOcn .or. isPresentIce

    endif  ! generate_landmask

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine completeFieldInitialization(State,rc)

      type(ESMF_State)   , intent(inout) :: State
      integer            , intent(out)   :: rc
    
      integer                     :: n, fieldCount
      character(ESMF_MAXSTR),allocatable :: fieldNameList(:)
      character(ESMF_MAXSTR)      :: transferAction
      character(len=*),parameter  :: subname='(module_MEDIATOR:completeFieldInitialization)'
#ifndef NUOPC_DOES_SMART_GRID_TRANSFER
      type(ESMF_Grid)             :: grid
#endif

      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

      rc = ESMF_Success

      call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(State, itemNameList=fieldNameList, &
        itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      do n=1, fieldCount

        call ESMF_StateGet(State, field=field, itemName=fieldNameList(n), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
          value=transferAction, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        if (trim(transferAction) == "accept") then
          if (dbug_flag > 1) then
            call ESMF_LogWrite(subname//" is accepting grid for field "//trim(fieldNameList(n)), &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif
          ! the transferred Grid is already set, allocate field data memory
          ! DCR - The WRFHYDRO soil fields have an ungridded 3rd dimension.
          ! The ESMF_FieldEmptyComplete is not allocating memory for this 3rd dimension
          call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
#ifndef NUOPC_DOES_SMART_GRID_TRANSFER
          ! access the transferred Grid to use for other fields
          call ESMF_FieldGet(field, grid=grid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        elseif (trim(transferAction) == "accept-internal") then
          if (dbug_flag > 1) then
            call ESMF_LogWrite(subname//" is accepting INTERNAL grid for field "//trim(fieldNameList(n)), &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif
          ! now use the Grid object in other fields
          call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
#endif
        endif   ! accept

        call FldGrid_Print(field,fieldNameList(n),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

      enddo

      if (dbug_flag > 5) then
        call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

    end subroutine completeFieldInitialization

  end subroutine InitializeIPDv03p5
  
  !-----------------------------------------------------------------------------

  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: time
    type(ESMF_Field)            :: field
    type(ESMF_StateItem_Flag)   :: itemType
    logical                     :: atCorrectTime, allDone, connected
    type(InternalState)         :: is_local
    character(len=*), parameter :: subname='(module_MEDIATOR:DataInitialize)'
    integer                     :: n

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    rc = ESMF_SUCCESS

    ! the MED needs valid ATM export Fields to initialize its internal state

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! get the current time out of the clock
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    
    ! initialze cumulative flag
    allDone = .true.  ! reset if an item is found that is not done
    
    ! check that all imported fields from ATM show correct timestamp
    do n = 1,fldsFrAtm%num
      call ESMF_StateGet(NState_AtmImp, itemName=fldsFrAtm%shortname(n), &
        itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        connected = NUOPC_IsConnected(NState_AtmImp, &
          fieldName=fldsFrAtm%shortname(n), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (connected) then
          call ESMF_StateGet(NState_AtmImp, itemName=fldsFrAtm%shortname(n), &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          atCorrectTime = NUOPC_IsAtTime(field, time, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (.not.atCorrectTime) then
            call ESMF_LogWrite("MED - Initialize-Data-Dependency NOT YET SATISFIED!!!", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            allDone = .false.
            exit  ! break out of the loop when first not satisfied found
          else
            call ESMF_LogWrite("MED - Initialize-Data-Dependency SATISFIED!!!", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif
        endif
      endif
    enddo

    !TODO: need to loop through fields from all of the components from which
    !TODO: valid field data is expected at this time!!

    if (allDone) then
      ! -> set InitializeDataComplete Component Attribute to "true", indicating
      ! to the driver that this Component has fully initialized its data
      call NUOPC_CompAttributeSet(gcomp, &
        name="InitializeDataComplete", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! gjt: The above code ensures that the MED has initial conditions from ATM.
    
      ! TODO - For the real case this should probably use the fields from the
      ! importState and do something with it as a sensible starting point
      ! for the accumulation field so that the OCN receives a meaningful
      ! fields during its first time step. However, here for testing
      ! I simply initialize to zero.
          
      call state_reset(NState_atmImp, value=spval_init, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      call state_reset(NState_ocnImp, value=spval_init, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      call state_reset(NState_iceImp, value=spval_init, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      call state_reset(NState_lndImp, value=spval_init, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      call state_reset(NState_hydImp, value=spval_init, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      call fieldBundle_reset(is_local%wrap%FBaccumAtm, value=czero, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      is_local%wrap%accumcntAtm = 0

      call fieldBundle_reset(is_local%wrap%FBaccumOcn, value=czero, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      is_local%wrap%accumcntOcn = 0

      call fieldBundle_reset(is_local%wrap%FBaccumIce, value=czero, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      is_local%wrap%accumcntIce = 0

      call fieldBundle_reset(is_local%wrap%FBaccumLnd, value=czero, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      is_local%wrap%accumcntLnd = 0

      call fieldBundle_reset(is_local%wrap%FBaccumHyd, value=czero, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      is_local%wrap%accumcntHyd = 0

      !---------------------------------------
      ! read mediator restarts
      !---------------------------------------

      !---tcraig, turn if on to force no mediator restarts for testing
      !if (.not.coldstart) then
        call Mediator_restart(gcomp,'read','mediator',rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      !endif

      ! default initialize s_surf to work around limitations of current initialization sequence
      call ESMF_StateGet(NState_IceExp, itemName='s_surf', &
        itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        if (NUOPC_IsConnected(NState_IceExp,'s_surf',rc=rc)) then
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call State_SetFldPtr(NState_IceExp, 's_surf', 34.0_ESMF_KIND_R8, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif

      endif

    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine SetRunClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)           :: mediatorClock, driverClock
    type(ESMF_Time)            :: currTime
    type(ESMF_TimeInterval)    :: timeStep
    character(len=*),parameter :: subname='(module_MEDIATOR:SetRunClock)'

    rc = ESMF_SUCCESS

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    ! query the Mediator for clocks
    call NUOPC_MediatorGet(gcomp, mediatorClock=mediatorClock, &
      driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
       call ClockTimePrint(driverClock  ,trim(subname)//'driver clock1',rc)
       call ClockTimePrint(mediatorClock,trim(subname)//'mediat clock1',rc)
    endif

    ! set the mediatorClock to have the current start time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ClockSet(mediatorClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
       call ClockTimePrint(driverClock  ,trim(subname)//'driver clock2',rc)
       call ClockTimePrint(mediatorClock,trim(subname)//'mediat clock2',rc)
    endif

    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(gcomp, driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine SetRunClock

  !-----------------------------------------------------------------------------

  subroutine MedPhase_fast_before(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! This Mediator phase runs before ATM and ICE are being called and
    ! prepares the ATM and ICE import Fields.
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr1(:,:),dataPtr2(:,:),dataPtr3(:,:)
    integer                     :: i,j,n
    character(len=*),parameter :: subname='(module_MEDIATOR:MedPhase_fast_before)'
    
    if(profile_memory) call ESMF_VMLogMemInfo("Entering "//trim(subname))
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call MedPhase_prep_atm(gcomp,rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call MedPhase_prep_ice(gcomp,rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call MedPhase_prep_lnd(gcomp,rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call MedPhase_prep_hyd(gcomp,rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if(profile_memory) call ESMF_VMLogMemInfo("Leaving "//trim(subname))

  end subroutine MedPhase_fast_before

  !-----------------------------------------------------------------------------

  subroutine MedPhase_prep_atm(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! This Mediator phase runs before ATM and ICE are being called and
    ! prepares the ATM and ICE import Fields.
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr1(:,:),dataPtr2(:,:),dataPtr3(:,:),dataPtr4(:,:)
    real(ESMF_KIND_R8), pointer :: ifrac_i(:,:)                   ! ice fraction on ice grid
    real(ESMF_KIND_R8), pointer :: ifrac_af(:,:), ifrac_afr(:,:)  ! ice fraction on atm grid consf map
    real(ESMF_KIND_R8), pointer :: ifrac_ad(:,:), ifrac_adr(:,:)  ! ice fraction on atm grid consd map
    real(ESMF_KIND_R8), pointer :: ifrac_ab(:,:), ifrac_abr(:,:)  ! ice fraction on atm grid bilnr map
    real(ESMF_KIND_R8), pointer :: ifrac_ap(:,:), ifrac_apr(:,:)  ! ice fraction on atm grid patch map
    real(ESMF_KIND_R8), pointer :: ocnwgt(:,:),icewgt(:,:),customwgt(:,:)
    integer                     :: i,j,n
    character(len=*),parameter :: subname='(module_MEDIATOR:MedPhase_prep_atm)'
    
    if(profile_memory) call ESMF_VMLogMemInfo("Entering "//trim(subname))
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    call ESMF_TimeGet(time,timestring=timestr)
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (dbug_flag > 1) then
      call ESMF_ClockPrint(clock, options="currTime", &
        preString="-------->"//trim(subname)//" mediating for: ", &
        unit=msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    !---------------------------------------
    !--- this is fast, no accumulator needed
    !---------------------------------------

    if (dbug_flag > 1) then
      call fieldBundle_reset(is_local%wrap%FBAtm_a, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBOcn_o, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBIce_i, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBLnd_l, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBHyd_h, value=czero, rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBAtm_a, trim(subname)//' FBAtm_a zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBOcn_o, trim(subname)//' FBOcn_o zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBIce_i, trim(subname)//' FBIce_i zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBLnd_l, trim(subname)//' FBLnd_l zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBHyd_h, trim(subname)//' FBhyd_h zero', rc=rc)
      call State_diagnose(NState_AtmImp, trim(subname)//' AtmImp ', rc=rc)
      call State_diagnose(NState_OcnImp, trim(subname)//' OcnImp ', rc=rc)
      call State_diagnose(NState_IceImp, trim(subname)//' IceImp ', rc=rc)
      call State_diagnose(NState_LndImp, trim(subname)//' LndImp ', rc=rc)
      call State_diagnose(NState_HydImp, trim(subname)//' HydImp ', rc=rc)
    endif

    call fieldBundle_copy(is_local%wrap%FBAtm_a, NState_AtmImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBOcn_o, NState_OcnImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBIce_i, NState_IceImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBLnd_l, NState_LndImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBHyd_h, NState_HydImp, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBAtm_a, trim(subname)//' FBAtm_a ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBOcn_o, trim(subname)//' FBOcn_o ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBIce_i, trim(subname)//' FBIce_i ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBLnd_l, trim(subname)//' FBLnd_l ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBHyd_h, trim(subname)//' FBHyd_h ', rc=rc)
    endif

    ! Regrid Full Field Bundles conservatively

    call fieldBundle_reset(is_local%wrap%FBOcn_a, value=czero, rc=rc)
    call fieldBundle_reset(is_local%wrap%FBIce_a, value=czero, rc=rc)
    call fieldBundle_reset(is_local%wrap%FBIce_if, value=czero, rc=rc)
    call fieldBundle_reset(is_local%wrap%FBLnd_a, value=czero, rc=rc)
    call fieldBundle_reset(is_local%wrap%FBHyd_a, value=czero, rc=rc)
    call fieldBundle_reset(is_local%wrap%FBAtmOcn_a, value=czero, rc=rc)

    if (is_local%wrap%o2a_active) then
      call Fieldbundle_Regrid(fldsFrOcn, is_local%wrap%FBOcn_o, is_local%wrap%FBOcn_a, &
         consfmap=is_local%wrap%RH_o2a_consf, &
         consdmap=is_local%wrap%RH_o2a_consd, &
         bilnrmap=is_local%wrap%RH_o2a_bilnr, &
         patchmap=is_local%wrap%RH_o2a_patch, &
         string='o2a', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      call Fieldbundle_Regrid(fldsAtmOcn, is_local%wrap%FBAtmOcn_o, is_local%wrap%FBAtmOcn_a, &
         consfmap=is_local%wrap%RH_o2a_consf, &
         consdmap=is_local%wrap%RH_o2a_consd, &
         bilnrmap=is_local%wrap%RH_o2a_bilnr, &
         patchmap=is_local%wrap%RH_o2a_patch, &
         string='o2aatmocn', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%i2a_active) then
      if (FieldBundle_FldChk(is_local%wrap%FBIce_i, 'ice_fraction', rc=rc) .and. &
          FieldBundle_FldChk(is_local%wrap%FBIce_a, 'ice_fraction', rc=rc)) then
        !--- tcraig, need to weight the ice2atm regrid by the ice fraction
        !--- need to compute weight by the frac mapped with the correct mapping
        !--- first compute the ice fraction on the atm grid for all active mappings

        call FieldBundle_GetFldPtr(is_local%wrap%FBIce_i, 'ice_fraction', dataPtr1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        allocate(ifrac_i (lbound(dataPtr1,1):ubound(dataPtr1,1),lbound(dataPtr1,2):ubound(dataPtr1,2)))

        !--- conservative frac
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_consf, rc=rc)) then
          call FieldBundle_FieldRegrid(is_local%wrap%FBIce_i,'ice_fraction', &
                                       is_local%wrap%FBIce_a,'ice_fraction', &
                                       is_local%wrap%RH_i2a_consf, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          !--- copy out the ifrac on ice grid and ifrac on atm grid
          call FieldBundle_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          allocate(ifrac_afr(lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))
          allocate(ifrac_af (lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))

          do j=lbound(dataptr1,2),ubound(dataptr1,2)
          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            ifrac_i(i,j) = dataPtr1(i,j)
          enddo
          enddo

          do j=lbound(dataptr2,2),ubound(dataptr2,2)
          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            !--- compute ice fraction on atm grid and reciprocal
            ifrac_af(i,j) = dataPtr2(i,j)
            if (dataPtr2(i,j) == 0._ESMF_KIND_R8) then
              ifrac_afr(i,j) = 1.0_ESMF_KIND_R8
            else
              ifrac_afr(i,j) = 1.0_ESMF_KIND_R8/dataPtr2(i,j)
            endif
          enddo
          enddo
        endif

        !--- conservative dst
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_consd, rc=rc)) then
          call FieldBundle_FieldRegrid(is_local%wrap%FBIce_i,'ice_fraction', &
                                       is_local%wrap%FBIce_a,'ice_fraction', &
                                       is_local%wrap%RH_i2a_consd, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          !--- copy out the ifrac on ice grid and ifrac on atm grid
          call FieldBundle_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          allocate(ifrac_adr(lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))
          allocate(ifrac_ad (lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))

          do j=lbound(dataptr1,2),ubound(dataptr1,2)
          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            ifrac_i(i,j) = dataPtr1(i,j)
          enddo
          enddo

          do j=lbound(dataptr2,2),ubound(dataptr2,2)
          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            !--- compute ice fraction on atm grid and reciprocal
            ifrac_ad(i,j) = dataPtr2(i,j)
            if (dataPtr2(i,j) == 0._ESMF_KIND_R8) then
              ifrac_adr(i,j) = 1.0_ESMF_KIND_R8
            else
              ifrac_adr(i,j) = 1.0_ESMF_KIND_R8/dataPtr2(i,j)
            endif
          enddo
          enddo
        endif

        !--- bilinear
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_bilnr, rc=rc)) then
          call FieldBundle_FieldRegrid(is_local%wrap%FBIce_i,'ice_fraction', &
                                       is_local%wrap%FBIce_a,'ice_fraction', &
                                       is_local%wrap%RH_i2a_bilnr, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          !--- copy out the ifrac on ice grid and ifrac on atm grid
          call FieldBundle_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          allocate(ifrac_abr(lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))
          allocate(ifrac_ab (lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))

          do j=lbound(dataptr1,2),ubound(dataptr1,2)
          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            ifrac_i(i,j) = dataPtr1(i,j)
          enddo
          enddo

          do j=lbound(dataptr2,2),ubound(dataptr2,2)
          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            !--- compute ice fraction on atm grid and reciprocal
            ifrac_ab(i,j) = dataPtr2(i,j)
            if (dataPtr2(i,j) == 0._ESMF_KIND_R8) then
              ifrac_abr(i,j) = 1.0_ESMF_KIND_R8
            else
              ifrac_abr(i,j) = 1.0_ESMF_KIND_R8/dataPtr2(i,j)
            endif
          enddo
          enddo
        endif

        !--- patch
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_patch, rc=rc)) then
          call FieldBundle_FieldRegrid(is_local%wrap%FBIce_i,'ice_fraction', &
                                       is_local%wrap%FBIce_a,'ice_fraction', &
                                       is_local%wrap%RH_i2a_patch, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          !--- copy out the ifrac on ice grid and ifrac on atm grid
          call FieldBundle_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr2, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          allocate(ifrac_apr(lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))
          allocate(ifrac_ap (lbound(dataptr2,1):ubound(dataptr2,1),lbound(dataptr2,2):ubound(dataptr2,2)))

          do j=lbound(dataptr1,2),ubound(dataptr1,2)
          do i=lbound(dataptr1,1),ubound(dataptr1,1)
            ifrac_i(i,j) = dataPtr1(i,j)
          enddo
          enddo

          do j=lbound(dataptr2,2),ubound(dataptr2,2)
          do i=lbound(dataptr2,1),ubound(dataptr2,1)
            !--- compute ice fraction on atm grid and reciprocal
            ifrac_ap(i,j) = dataPtr2(i,j)
            if (dataPtr2(i,j) == 0._ESMF_KIND_R8) then
              ifrac_apr(i,j) = 1.0_ESMF_KIND_R8
            else
              ifrac_apr(i,j) = 1.0_ESMF_KIND_R8/dataPtr2(i,j)
            endif
          enddo
          enddo
        endif

        !--- multiply FBIce_i by ifrac_i

        do n = 1,fldsFrIce%num
          if (FieldBundle_FldChk(is_local%wrap%FBIce_i, fldsFrIce%shortname(n), rc=rc) .and. &
              FieldBundle_FldChk(is_local%wrap%FBIce_if,fldsFrIce%shortname(n), rc=rc)) then
            call FieldBundle_GetFldPtr(is_local%wrap%FBIce_i , fldsFrIce%shortname(n), dataPtr3, rc=rc)
            call FieldBundle_GetFldPtr(is_local%wrap%FBIce_if, fldsFrIce%shortname(n), dataPtr4, rc=rc)
            do j=lbound(dataptr3,2),ubound(dataptr3,2)
            do i=lbound(dataptr3,1),ubound(dataptr3,1)
              dataPtr4(i,j) = dataPtr3(i,j) * ifrac_i(i,j)
            enddo
            enddo
          endif
        enddo

        !--- regrid FBIce_if, fields with fraction multiplied

        call Fieldbundle_Regrid(fldsFrIce, is_local%wrap%FBIce_if, is_local%wrap%FBIce_a, &
           consfmap=is_local%wrap%RH_i2a_consf, &
           consdmap=is_local%wrap%RH_i2a_consd, &
           bilnrmap=is_local%wrap%RH_i2a_bilnr, &
           patchmap=is_local%wrap%RH_i2a_patch, &
           string='i2a', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        !--- divide FBIce_a by ifrac_a, interpolated ice fraction
        !--- actually multiply by reciprocal of ifrac_a, ifrac_ar

        do n = 1,fldsFrIce%num
          if (FieldBundle_FldChk(is_local%wrap%FBIce_a, fldsFrIce%shortname(n), rc=rc)) then
            call FieldBundle_GetFldPtr(is_local%wrap%FBIce_a, fldsFrIce%shortname(n), dataPtr3, rc=rc)
            if (fldsFrIce%mapping(n) == "conservefrac") then
              do j=lbound(dataptr3,2),ubound(dataptr3,2)
              do i=lbound(dataptr3,1),ubound(dataptr3,1)
                dataPtr3(i,j) = dataPtr3(i,j) * ifrac_afr(i,j)
              enddo
              enddo
            elseif (fldsFrIce%mapping(n) == "conservedst") then
              do j=lbound(dataptr3,2),ubound(dataptr3,2)
              do i=lbound(dataptr3,1),ubound(dataptr3,1)
                dataPtr3(i,j) = dataPtr3(i,j) * ifrac_adr(i,j)
              enddo
              enddo
            elseif (fldsFrIce%mapping(n) == 'bilinear') then
              do j=lbound(dataptr3,2),ubound(dataptr3,2)
              do i=lbound(dataptr3,1),ubound(dataptr3,1)
                dataPtr3(i,j) = dataPtr3(i,j) * ifrac_abr(i,j)
              enddo
              enddo
            elseif (fldsFrIce%mapping(n) == 'patch') then
              do j=lbound(dataptr3,2),ubound(dataptr3,2)
              do i=lbound(dataptr3,1),ubound(dataptr3,1)
                dataPtr3(i,j) = dataPtr3(i,j) * ifrac_apr(i,j)
              enddo
              enddo
            else
              call ESMF_LogWrite(trim(subname)//": mapping name error "//trim(fldsFrIce%mapping(n)), ESMF_LOGMSG_INFO, rc=rc)
              rc=ESMF_FAILURE
              return
            endif
          endif
        enddo
        !--- make sure ifrac_a in the mapped bundle is correct
        call FieldBundle_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', dataPtr3, rc=rc)
        do j=lbound(dataptr3,2),ubound(dataptr3,2)
        do i=lbound(dataptr3,1),ubound(dataptr3,1)
          dataPtr3(i,j) = ifrac_af(i,j)
        enddo
        enddo

        deallocate(ifrac_i)
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_consf, rc=rc)) &
          deallocate(ifrac_af, ifrac_afr)
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_consd, rc=rc)) &
          deallocate(ifrac_ad, ifrac_adr)
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_bilnr, rc=rc)) &
          deallocate(ifrac_ab, ifrac_abr)
        if (ESMF_RouteHandleIsCreated(is_local%wrap%RH_i2a_patch, rc=rc)) &
          deallocate(ifrac_ap, ifrac_apr)

      else
        call Fieldbundle_Regrid(fldsFrIce, is_local%wrap%FBIce_i, is_local%wrap%FBIce_a, &
           consfmap=is_local%wrap%RH_i2a_consf, &
           consdmap=is_local%wrap%RH_i2a_consd, &
           bilnrmap=is_local%wrap%RH_i2a_bilnr, &
           patchmap=is_local%wrap%RH_i2a_patch, &
           string='i2a', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif

    if (is_local%wrap%l2a_active) then
      call Fieldbundle_Regrid(fldsFrLnd, is_local%wrap%FBLnd_l, is_local%wrap%FBLnd_a, &
         consfmap=is_local%wrap%RH_l2a_consf, &
         consdmap=is_local%wrap%RH_l2a_consd, &
         bilnrmap=is_local%wrap%RH_l2a_bilnr, &
         patchmap=is_local%wrap%RH_l2a_patch, &
         string='l2a', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%h2a_active) then
      call Fieldbundle_Regrid(fldsFrHyd, is_local%wrap%FBHyd_h, is_local%wrap%FBHyd_a, &
         consfmap=is_local%wrap%RH_h2a_consf, &
         consdmap=is_local%wrap%RH_h2a_consd, &
         bilnrmap=is_local%wrap%RH_h2a_bilnr, &
         patchmap=is_local%wrap%RH_h2a_patch, &
         string='h2a', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBOcn_a, trim(subname)//' FBOcn_a ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBIce_a, trim(subname)//' FBIce_a ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBLnd_a, trim(subname)//' FBLnd_a ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBHyd_a, trim(subname)//' FBHyd_a ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBAtmOcn_a, trim(subname)//' FBAtmOcn_a ', rc=rc)
    endif

    call fieldBundle_copy(is_local%wrap%FBforAtm, is_local%wrap%FBOcn_a, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBforAtm, is_local%wrap%FBIce_a, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBforAtm, is_local%wrap%FBLnd_a, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBforAtm, is_local%wrap%FBHyd_a, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBforAtm, is_local%wrap%FBAtmOcn_a, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBforAtm, trim(subname)//' FBforAtm ', rc=rc)
    endif

    if (statewrite_flag) then
      ! write the fields imported from ocn to file
      call ESMF_FieldBundleWrite(is_local%wrap%FBOcn_a, 'fields_med_ocn_a.nc', &
        singleFile=.true., overwrite=.true., timeslice=is_local%wrap%fastcntr, &
        iofmt=ESMF_IOFMT_NETCDF, rc=rc)  
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      call ESMF_FieldBundleWrite(is_local%wrap%FBIce_a, 'fields_med_ice_a.nc', &
        singleFile=.true., overwrite=.true., timeslice=is_local%wrap%fastcntr, &
        iofmt=ESMF_IOFMT_NETCDF, rc=rc)  
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    !---------------------------------------
    !--- custom calculations to atm
    !---------------------------------------

    !---  ocn and ice fraction for merges

    call FieldBundle_GetFldPtr(is_local%wrap%FBIce_a, 'ice_fraction', icewgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(ocnwgt(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    do j=lbound(icewgt,2),ubound(icewgt,2)
    do i=lbound(icewgt,1),ubound(icewgt,1)
      ocnwgt = 1.0_ESMF_KIND_R8 - icewgt
    enddo
    enddo

    !--- fill land mask every coupling from initial computation

    if (generate_landmask) then
      call FieldBundle_GetFldPtr(is_local%wrap%FBforAtm, 'land_mask', dataPtr3, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      do j=lbound(dataPtr3,2),ubound(dataPtr3,2)
      do i=lbound(dataPtr3,1),ubound(dataPtr3,1)
        dataPtr3(i,j) = land_mask(i,j)
      enddo
      enddo
    else
      call ESMF_LogWrite(trim(subname)//": ERROR generate_landmask must be true ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    !--- merges

    call fieldBundle_FieldMerge(is_local%wrap%FBforAtm  ,'surface_temperature' , & 
                                is_local%wrap%FBOcn_a   ,'sea_surface_temperature',ocnwgt, &
                                is_local%wrap%FBIce_a   ,'sea_ice_temperature',icewgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforAtm  ,'mean_sensi_heat_flx' , & 
                                is_local%wrap%FBAtmOcn_a,'mean_sensi_heat_flx_atm_into_ocn',ocnwgt, &
                                is_local%wrap%FBIce_a   ,'mean_sensi_heat_flx_atm_into_ice',icewgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforAtm  ,'mean_laten_heat_flx' , & 
                                is_local%wrap%FBAtmOcn_a,'mean_laten_heat_flx_atm_into_ocn',ocnwgt, &
                                is_local%wrap%FBIce_a   ,'mean_laten_heat_flx_atm_into_ice',icewgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforAtm  ,'mean_up_lw_flx' , & 
                                is_local%wrap%FBAtmOcn_a,'mean_up_lw_flx_ocn',ocnwgt, &
                                is_local%wrap%FBIce_a   ,'mean_up_lw_flx_ice',icewgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforAtm  ,'mean_evap_rate' , & 
                                is_local%wrap%FBAtmOcn_a,'mean_evap_rate_atm_into_ocn',ocnwgt, &
                                is_local%wrap%FBIce_a   ,'mean_evap_rate_atm_into_ice',icewgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforAtm  ,'mean_zonal_moment_flx' , & 
                                is_local%wrap%FBAtmOcn_a,'stress_on_air_ocn_zonal',ocnwgt, &
                                is_local%wrap%FBIce_a   ,'stress_on_air_ice_zonal',icewgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforAtm  ,'mean_merid_moment_flx' , & 
                                is_local%wrap%FBAtmOcn_a,'stress_on_air_ocn_merid',ocnwgt, &
                                is_local%wrap%FBIce_a   ,'stress_on_air_ice_merid',icewgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    deallocate(ocnwgt)

    !---------------------------------------
    !--- set export State to special value for testing
    !---------------------------------------

    call state_reset(NState_AtmExp, value=spval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
      call State_diagnose(NState_AtmExp, trim(subname)//' AtmExp_99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call fieldBundle_copy(NState_AtmExp, is_local%wrap%FBforAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
      call state_diagnose(NState_AtmExp, trim(subname)//' AtmExp_final ', rc=rc)
    endif

    if (statewrite_flag) then
      ! write the fields exported to atm to file
      call NUOPC_Write(NState_AtmExp, &
        fldsToAtm%shortname(1:fldsToAtm%num), &
        "field_med_to_atm_", timeslice=is_local%wrap%fastcntr, &
        relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif
    
    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if(profile_memory) call ESMF_VMLogMemInfo("Leaving "//trim(subname))

  end subroutine MedPhase_prep_atm

  !-----------------------------------------------------------------------------
  
  subroutine TimestampExport_prep_atm(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! This attaches an invalid timestamp on fields sometimes.
    ! Otherwise, it just sets the timestamp to the current clock.

    ! local variables
    integer                 :: n, fieldcount
    type(ESMF_Clock)        :: driverClock
    type(ESMF_Clock)        :: clock
    type(ESMF_time)         :: currtime
    type(InternalState)     :: is_local
    type(ESMF_Field), pointer  :: fieldList(:)
    character(ESMF_MAXSTR),allocatable :: fieldNameList(:)
    character(len=*),parameter :: subname='(module_MEDIATOR:TimestampExport_prep_atm)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! query the Component for info
    call NUOPC_MediatorGet(gcomp, driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set the Clock to have the current time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ClockSet(Clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !---------------------------
    ! validate all data by default
    !---------------------------

    call NUOPC_UpdateTimestamp(NState_AtmExp, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !---------------------------
    ! COLDSTART:
    ! invalidate all data on timestep 1
    ! invalidate SST on all timesteps
    !---------------------------

    if (coldstart) then
      if (is_local%wrap%fastcntr == 1) then
        call NUOPC_UpdateTimestamp(NState_AtmExp, clock_invalidTimeStamp, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        call ESMF_StateGet(NState_AtmExp, itemCount=fieldCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        allocate(fieldNameList(fieldCount))
        call ESMF_StateGet(NState_AtmExp, itemNameList=fieldNameList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        nullify(fieldList)
        call NUOPC_GetStateMemberLists(NState_AtmExp, fieldList=fieldList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        do n = 1, fieldCount
          if (trim(fieldNameList(n))=="sea_surface_temperature") then
             call NUOPCplus_UpdateTimestamp(fieldList(n), time_invalidTimeStamp, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) return  ! bail out
          endif
        enddo
        if (associated(fieldList)) deallocate(fieldList)
        deallocate(fieldNameList)
      endif
    endif

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine TimestampExport_prep_atm

  !-----------------------------------------------------------------------------

  subroutine MedPhase_prep_ice(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! This Mediator phase runs before ATM and ICE are being called and
    ! prepares the ATM and ICE import Fields.
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    type(InternalState)         :: is_local
    real(ESMF_KIND_R8), pointer :: dataPtr1(:,:),dataPtr2(:,:),dataPtr3(:,:)
    real(ESMF_KIND_R8), pointer :: temperature(:,:), humidity(:,:), pressure(:,:)
    real(ESMF_KIND_R8), pointer :: air_density(:,:)
    integer                     :: i,j,n
    character(len=*),parameter :: subname='(module_MEDIATOR:MedPhase_prep_ice)'
    
    if(profile_memory) call ESMF_VMLogMemInfo("Entering "//trim(subname))
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_TimeGet(time,timestring=timestr)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (dbug_flag > 1) then
      call ESMF_ClockPrint(clock, options="currTime", &
        preString="-------->"//trim(subname)//" mediating for: ", &
        unit=msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    !---------------------------------------
    !--- this is fast, no accumulator needed
    !---------------------------------------

    if (dbug_flag > 1) then
      call fieldBundle_reset(is_local%wrap%FBAtm_a, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBOcn_o, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBIce_i, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBLnd_l, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBHyd_h, value=czero, rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBAtm_a, trim(subname)//' FBAtm_a zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBOcn_o, trim(subname)//' FBOcn_o zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBIce_i, trim(subname)//' FBIce_i zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBLnd_l, trim(subname)//' FBLnd_l zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBHyd_h, trim(subname)//' FBhyd_h zero', rc=rc)
      call State_diagnose(NState_AtmImp, trim(subname)//' AtmImp ', rc=rc)
      call State_diagnose(NState_OcnImp, trim(subname)//' OcnImp ', rc=rc)
      call State_diagnose(NState_IceImp, trim(subname)//' IceImp ', rc=rc)
      call State_diagnose(NState_LndImp, trim(subname)//' LndImp ', rc=rc)
      call State_diagnose(NState_HydImp, trim(subname)//' HydImp ', rc=rc)
    endif

    call fieldBundle_copy(is_local%wrap%FBAtm_a, NState_AtmImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBOcn_o, NState_OcnImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBIce_i, NState_IceImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBLnd_l, NState_LndImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBHyd_h, NState_HydImp, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBAtm_a, trim(subname)//' FBAtm_a ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBOcn_o, trim(subname)//' FBOcn_o ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBIce_i, trim(subname)//' FBIce_i ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBLnd_l, trim(subname)//' FBLnd_l ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBHyd_h, trim(subname)//' FBHyd_h ', rc=rc)
    endif

    ! Regrid Full Field Bundles conservatively

    call fieldBundle_reset(is_local%wrap%FBAtm_i, value=czero, rc=rc)
    call fieldBundle_reset(is_local%wrap%FBOcn_i, value=czero, rc=rc)

    if (is_local%wrap%a2i_active) then
      call Fieldbundle_Regrid(fldsFrAtm, is_local%wrap%FBAtm_a, is_local%wrap%FBAtm_i, &
         consfmap=is_local%wrap%RH_a2i_consf, &
         consdmap=is_local%wrap%RH_a2i_consd, &
         bilnrmap=is_local%wrap%RH_a2i_bilnr, &
         patchmap=is_local%wrap%RH_a2i_patch, &
         string='a2i', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%o2i_active) then
      call Fieldbundle_Regrid(fldsFrOcn, is_local%wrap%FBOcn_o, is_local%wrap%FBOcn_i, &
         consfmap=is_local%wrap%RH_o2i_consf, &
         consdmap=is_local%wrap%RH_o2i_consd, &
         bilnrmap=is_local%wrap%RH_o2i_bilnr, &
         patchmap=is_local%wrap%RH_o2i_patch, &
         fcopymap=is_local%wrap%RH_o2i_fcopy, &
         string='o2i', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBAtm_i, trim(subname)//' FBAtm_i ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBOcn_i, trim(subname)//' FBOcn_i ', rc=rc)
    endif

    call fieldBundle_copy(is_local%wrap%FBforIce, is_local%wrap%FBAtm_i, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBforIce, is_local%wrap%FBOcn_i, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBforIce, trim(subname)//' FBforIce ', rc=rc)
    endif

    !---------------------------------------
    !--- merges to ice
    !---------------------------------------

    !--- calculate air density for cice

    call FieldBundle_GetFldPtr(is_local%wrap%FBforIce, 'inst_temp_height_lowest', temperature, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBforIce, 'inst_pres_height_lowest', pressure, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBforIce, 'inst_spec_humid_height_lowest', humidity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call FieldBundle_GetFldPtr(is_local%wrap%FBforIce, 'air_density_height_lowest', air_density, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    do j = lbound(temperature,2),ubound(temperature,2)
    do i = lbound(temperature,1),ubound(temperature,1)
      if (temperature(i,j) /= 0._ESMF_KIND_R8) then
        air_density(i,j) = pressure(i,j)/&
          (287.058_ESMF_KIND_R8*(1._ESMF_KIND_R8+0.608_ESMF_KIND_R8*humidity(i,j))*temperature(i,j))
      else
        air_density(i,j) = 0._ESMF_KIND_R8
      endif
    enddo
    enddo

    !---------------------------------------
    !--- set export State to special value for testing
    !---------------------------------------

    call state_reset(NState_IceExp, value=spval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
      call State_diagnose(NState_IceExp, trim(subname)//' IceExp_99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call fieldBundle_copy(NState_IceExp, is_local%wrap%FBforIce, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
      call state_diagnose(NState_IceExp, trim(subname)//' IceExp_final ', rc=rc)
    endif

    if (statewrite_flag) then
      ! write the fields exported to ice to file
      call NUOPC_Write(NState_IceExp, &
        fldsToIce%shortname(1:fldsToIce%num), &
        "field_med_to_ice_", timeslice=is_local%wrap%fastcntr, &
        relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif
    
    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if(profile_memory) call ESMF_VMLogMemInfo("Leaving "//trim(subname))

  end subroutine MedPhase_prep_ice

  !-----------------------------------------------------------------------------

  subroutine MedPhase_prep_lnd(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! This Mediator phase preps land exports
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(module_MEDIATOR:MedPhase_prep_lnd)'
    
    if(profile_memory) call ESMF_VMLogMemInfo("Entering "//trim(subname))
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_TimeGet(time,timestring=timestr)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (dbug_flag > 1) then
      call ESMF_ClockPrint(clock, options="currTime", &
        preString="-------->"//trim(subname)//" mediating for: ", &
        unit=msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    !---------------------------------------
    !--- this is fast, no accumulator needed
    !---------------------------------------

    if (dbug_flag > 1) then
      call fieldBundle_reset(is_local%wrap%FBAtm_a, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBOcn_o, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBIce_i, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBLnd_l, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBHyd_h, value=czero, rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBAtm_a, trim(subname)//' FBAtm_a zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBOcn_o, trim(subname)//' FBOcn_o zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBIce_i, trim(subname)//' FBIce_i zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBLnd_l, trim(subname)//' FBLnd_l zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBHyd_h, trim(subname)//' FBhyd_h zero', rc=rc)
      call State_diagnose(NState_AtmImp, trim(subname)//' AtmImp ', rc=rc)
      call State_diagnose(NState_OcnImp, trim(subname)//' OcnImp ', rc=rc)
      call State_diagnose(NState_IceImp, trim(subname)//' IceImp ', rc=rc)
      call State_diagnose(NState_LndImp, trim(subname)//' LndImp ', rc=rc)
      call State_diagnose(NState_HydImp, trim(subname)//' HydImp ', rc=rc)
    endif

    call fieldBundle_copy(is_local%wrap%FBAtm_a, NState_AtmImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBOcn_o, NState_OcnImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBIce_i, NState_IceImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBLnd_l, NState_LndImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBHyd_h, NState_HydImp, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBAtm_a, trim(subname)//' FBAtm_a ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBOcn_o, trim(subname)//' FBOcn_o ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBIce_i, trim(subname)//' FBIce_i ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBLnd_l, trim(subname)//' FBLnd_l ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBHyd_h, trim(subname)//' FBHyd_h ', rc=rc)
    endif

    ! Regrid Full Field Bundles conservatively

    call fieldBundle_reset(is_local%wrap%FBAtm_l, value=czero, rc=rc)
    call fieldBundle_reset(is_local%wrap%FBHyd_l, value=czero, rc=rc)

    if (is_local%wrap%a2l_active) then
      call Fieldbundle_Regrid(fldsFrAtm, is_local%wrap%FBAtm_a, is_local%wrap%FBAtm_l, &
         consfmap=is_local%wrap%RH_a2l_consf, &
         consdmap=is_local%wrap%RH_a2l_consd, &
         bilnrmap=is_local%wrap%RH_a2l_bilnr, &
         patchmap=is_local%wrap%RH_a2l_patch, &
         string='a2l', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%h2l_active) then
      call Fieldbundle_Regrid(fldsFrHyd, is_local%wrap%FBHyd_h, is_local%wrap%FBHyd_l, &
         consfmap=is_local%wrap%RH_h2l_consf, &
         consdmap=is_local%wrap%RH_h2l_consd, &
         bilnrmap=is_local%wrap%RH_h2l_bilnr, &
         patchmap=is_local%wrap%RH_h2l_patch, &
         string='h2l', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBAtm_l, trim(subname)//' FBAtm_l ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBHyd_l, trim(subname)//' FBHyd_l ', rc=rc)
    endif

    call fieldBundle_copy(is_local%wrap%FBforLnd, is_local%wrap%FBAtm_l, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBforLnd, is_local%wrap%FBHyd_l, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBforLnd, trim(subname)//' FBforLnd ', rc=rc)
    endif

    !---------------------------------------
    !--- custom calculations to lnd
    !---------------------------------------

    ! None yet

    !---------------------------------------
    !--- set export State to special value for testing
    !---------------------------------------

    call state_reset(NState_LndExp, value=spval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
      call State_diagnose(NState_LndExp, trim(subname)//' LndExp_99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call fieldBundle_copy(NState_LndExp, is_local%wrap%FBforLnd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
      call state_diagnose(NState_LndExp, trim(subname)//' LndExp_final ', rc=rc)
    endif

    if (statewrite_flag) then
      ! write the fields exported to lnd to file
      call NUOPC_Write(NState_LndExp, &
        fieldNameList=fldsToLnd%shortname(1:fldsToLnd%num), &
        fileNamePrefix="field_med_to_lnd_", timeslice=is_local%wrap%fastcntr, &
        relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif
    
    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if(profile_memory) call ESMF_VMLogMemInfo("Leaving "//trim(subname))

  end subroutine MedPhase_prep_lnd

  !-----------------------------------------------------------------------------

  subroutine MedPhase_prep_hyd(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! This Mediator phase prepares data for they hyd component
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    type(InternalState)         :: is_local
    character(len=*),parameter :: subname='(module_MEDIATOR:MedPhase_prep_hyd)'
    
    if(profile_memory) call ESMF_VMLogMemInfo("Entering "//trim(subname))
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_TimeGet(time,timestring=timestr)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (dbug_flag > 1) then
      call ESMF_ClockPrint(clock, options="currTime", &
        preString="-------->"//trim(subname)//" mediating for: ", &
        unit=msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    !---------------------------------------
    !--- this is fast, no accumulator needed
    !---------------------------------------

    if (dbug_flag > 1) then
      call fieldBundle_reset(is_local%wrap%FBAtm_a, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBOcn_o, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBIce_i, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBLnd_l, value=czero, rc=rc)
      call fieldBundle_reset(is_local%wrap%FBHyd_h, value=czero, rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBAtm_a, trim(subname)//' FBAtm_a zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBOcn_o, trim(subname)//' FBOcn_o zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBIce_i, trim(subname)//' FBIce_i zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBLnd_l, trim(subname)//' FBLnd_l zero', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBHyd_h, trim(subname)//' FBhyd_h zero', rc=rc)
      call State_diagnose(NState_AtmImp, trim(subname)//' AtmImp ', rc=rc)
      call State_diagnose(NState_OcnImp, trim(subname)//' OcnImp ', rc=rc)
      call State_diagnose(NState_IceImp, trim(subname)//' IceImp ', rc=rc)
      call State_diagnose(NState_LndImp, trim(subname)//' LndImp ', rc=rc)
      call State_diagnose(NState_HydImp, trim(subname)//' HydImp ', rc=rc)
    endif

    call fieldBundle_copy(is_local%wrap%FBAtm_a, NState_AtmImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBOcn_o, NState_OcnImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBIce_i, NState_IceImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBLnd_l, NState_LndImp, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBHyd_h, NState_HydImp, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBAtm_a, trim(subname)//' FBAtm_a ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBOcn_o, trim(subname)//' FBOcn_o ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBIce_i, trim(subname)//' FBIce_i ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBLnd_l, trim(subname)//' FBLnd_l ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBHyd_h, trim(subname)//' FBHyd_h ', rc=rc)
    endif

    ! Regrid Full Field Bundles conservatively

    call fieldBundle_reset(is_local%wrap%FBAtm_h, value=czero, rc=rc)
    call fieldBundle_reset(is_local%wrap%FBLnd_h, value=czero, rc=rc)

    if (is_local%wrap%a2h_active) then
      call Fieldbundle_Regrid(fldsFrAtm, is_local%wrap%FBAtm_a, is_local%wrap%FBAtm_h, &
         consfmap=is_local%wrap%RH_a2h_consf, &
         consdmap=is_local%wrap%RH_a2h_consd, &
         bilnrmap=is_local%wrap%RH_a2h_bilnr, &
         patchmap=is_local%wrap%RH_a2h_patch, &
         string='a2h', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%l2h_active) then
      call Fieldbundle_Regrid(fldsFrLnd, is_local%wrap%FBLnd_l, is_local%wrap%FBLnd_h, &
         consfmap=is_local%wrap%RH_l2h_consf, &
         consdmap=is_local%wrap%RH_l2h_consd, &
         bilnrmap=is_local%wrap%RH_l2h_bilnr, &
         patchmap=is_local%wrap%RH_l2h_patch, &
         string='l2h', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBAtm_h, trim(subname)//' FBAtm_h ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBLnd_h, trim(subname)//' FBLnd_h ', rc=rc)
    endif

    call fieldBundle_copy(is_local%wrap%FBforHyd, is_local%wrap%FBLnd_h, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBforHyd, is_local%wrap%FBAtm_h, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBforHyd, trim(subname)//' FBforHyd ', rc=rc)
    endif

    !---------------------------------------
    !--- custom calculations to hyd
    !---------------------------------------

    ! None yet

    !---------------------------------------
    !--- set export State to special value for testing
    !---------------------------------------

    call state_reset(NState_HydExp, value=spval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
      call State_diagnose(NState_HydExp, trim(subname)//' HydExp_99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call fieldBundle_copy(NState_HydExp, is_local%wrap%FBforHyd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
      call state_diagnose(NState_HydExp, trim(subname)//' HydExp_final ', rc=rc)
    endif

    if (statewrite_flag) then
      ! write the fields exported to hyd to file
      call NUOPC_Write(NState_HydExp, &
        fieldNameList=fldsToHyd%shortname(1:fldsToHyd%num), &
        fileNamePrefix="field_med_to_hyd_", timeslice=is_local%wrap%fastcntr, &
        relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif
    
    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if(profile_memory) call ESMF_VMLogMemInfo("Leaving "//trim(subname))

  end subroutine MedPhase_prep_hyd

  !-----------------------------------------------------------------------------

  subroutine MedPhase_fast_after(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    integer                     :: i,j,n
    real(ESMF_KIND_R8), pointer :: zbot(:,:),ubot(:,:),vbot(:,:),thbot(:,:), &
                                   qbot(:,:),rbot(:,:),tbot(:,:), pbot(:,:)
    real(ESMF_KIND_R8), pointer :: us  (:,:),vs  (:,:),ts  (:,:),mask(:,:)
    real(ESMF_KIND_R8), pointer :: sen (:,:),lat (:,:),lwup(:,:),evap(:,:), &
                                   taux(:,:),tauy(:,:),tref(:,:),qref(:,:),duu10n(:,:)
    real(ESMF_KIND_R8)          :: zbot1(1),ubot1(1),vbot1(1),thbot1(1), &
                                   qbot1(1),rbot1(1),tbot1(1)
    integer                     :: mask1(1)
    real(ESMF_KIND_R8)          :: us1  (1),vs1  (1),ts1  (1)
    real(ESMF_KIND_R8)          :: sen1 (1),lat1 (1),lwup1(1),evap1(1), &
                                   taux1(1),tauy1(1),tref1(1),qref1(1),duu10n1(1)
    character(len=*),parameter :: subname='(module_MEDIATOR:MedPhase_fast_after)'
    
    if(profile_memory) call ESMF_VMLogMemInfo("Entering "//trim(subname))
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    call MedPhase_atm_ocn_flux(gcomp,rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call MedPhase_accum_fast(gcomp,rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if(profile_memory) call ESMF_VMLogMemInfo("Leaving "//trim(subname))

  end subroutine MedPhase_fast_after

  !-----------------------------------------------------------------------------

  subroutine MedPhase_accum_fast(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    integer                     :: i,j,n
    character(len=*),parameter :: subname='(module_MEDIATOR:MedPhase_accum_fast)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_TimeGet(time,timestring=timestr)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (dbug_flag > 1) then
      call ESMF_ClockPrint(clock, options="currTime", &
        preString="-------->"//trim(subname)//" mediating for: ", &
        unit=msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (statewrite_flag) then
      ! write the fields imported from atm to file
      call NUOPC_Write(NState_AtmImp, &
        fldsFrAtm%shortname(1:fldsFrAtm%num), &
        "field_med_from_atm_", timeslice=is_local%wrap%fastcntr, &
        relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! write the fields imported from ice to file
      call NUOPC_Write(NState_IceImp, &
        fldsFrIce%shortname(1:fldsFrIce%num), &
        "field_med_from_ice_", timeslice=is_local%wrap%fastcntr, &
        relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! write the fields imported from lnd to file
      call NUOPC_Write(NState_LndImp, &
        fieldNameList=fldsFrLnd%shortname(1:fldsFrLnd%num), &
        fileNamePrefix="field_med_from_lnd_", timeslice=is_local%wrap%fastcntr, &
        relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! write the fields imported from hyd to file
      call NUOPC_Write(NState_HydImp, &
        fieldNameList=fldsFrHyd%shortname(1:fldsFrHyd%num), &
        fileNamePrefix="field_med_from_hyd_", timeslice=is_local%wrap%fastcntr, &
        relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    !---------------------------------------
    !--- atm, ice, lnd, hyd accumulator for ocean
    !---------------------------------------

    if (dbug_flag > 1) then
      call State_diagnose(NState_AtmImp, trim(subname)//' AtmImp ', rc=rc)
      call State_diagnose(NState_IceImp, trim(subname)//' IceImp ', rc=rc)
      call State_diagnose(NState_LndImp, trim(subname)//' LndImp ', rc=rc)
      call State_diagnose(NState_HydImp, trim(subname)//' HydImp ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBaccAtm_B4accum ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBaccIce_B4accum ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBaccLnd_B4accum ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumHyd, trim(subname)//' FBaccHyd_B4accum ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBaccAtmOcn_B4accum ', rc=rc)
    endif

    call fieldBundle_accum(is_local%wrap%FBaccumAtm, NState_AtmImp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is_local%wrap%accumcntAtm = is_local%wrap%accumcntAtm + 1

    call fieldBundle_accum(is_local%wrap%FBaccumIce, NState_IceImp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is_local%wrap%accumcntIce = is_local%wrap%accumcntIce + 1

    call fieldBundle_accum(is_local%wrap%FBaccumLnd, NState_LndImp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is_local%wrap%accumcntLnd = is_local%wrap%accumcntLnd + 1

    call fieldBundle_accum(is_local%wrap%FBaccumHyd, NState_HydImp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is_local%wrap%accumcntHyd = is_local%wrap%accumcntHyd + 1

    call fieldBundle_accum(is_local%wrap%FBaccumAtmOcn, is_local%wrap%FBAtmOcn_o, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is_local%wrap%accumcntAtmOcn = is_local%wrap%accumcntAtmOcn + 1

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBaccAtm_AFaccum ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBaccIce_AFaccum ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBaccLnd_AFaccum ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumHyd, trim(subname)//' FBaccHyd_AFaccum ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBaccAtmOcn_AFaccum ', rc=rc)
    endif

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    !---------------------------------------

    is_local%wrap%fastcntr = is_local%wrap%fastcntr + 1

    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine MedPhase_accum_fast

  !-----------------------------------------------------------------------------

  subroutine MedPhase_atm_ocn_flux(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is_local
    integer                     :: i,j,n
    real(ESMF_KIND_R8), pointer :: zbot(:,:),ubot(:,:),vbot(:,:),thbot(:,:), &
                                   qbot(:,:),rbot(:,:),tbot(:,:), pbot(:,:)
    real(ESMF_KIND_R8), pointer :: us  (:,:),vs  (:,:),ts  (:,:),mask(:,:)
    real(ESMF_KIND_R8), pointer :: sen (:,:),lat (:,:),lwup(:,:),evap(:,:), &
                                   taux(:,:),tauy(:,:),tref(:,:),qref(:,:),duu10n(:,:)
    real(ESMF_KIND_R8)          :: zbot1(1),ubot1(1),vbot1(1),thbot1(1), &
                                   qbot1(1),rbot1(1),tbot1(1)
    integer                     :: mask1(1)
    real(ESMF_KIND_R8)          :: us1  (1),vs1  (1),ts1  (1)
    real(ESMF_KIND_R8)          :: sen1 (1),lat1 (1),lwup1(1),evap1(1), &
                                   taux1(1),tauy1(1),tref1(1),qref1(1),duu10n1(1)
    character(len=*),parameter :: subname='(module_MEDIATOR:MedPhase_atm_ocn_flux)'
    
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_TimeGet(time,timestring=timestr)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (dbug_flag > 1) then
      call ESMF_ClockPrint(clock, options="currTime", &
        preString="-------->"//trim(subname)//" mediating for: ", &
        unit=msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !---------------------------------------
    !--- compute atm/ocn fluxes
    !---------------------------------------

    call fieldBundle_reset(is_local%wrap%FBAtmOcn_o, value=czero, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- atm fields on ocean grid input
    if (is_local%wrap%a2o_active) then
      
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//' calling FBRegrid FBAtm_a to FBAtm_o', ESMF_LOGMSG_INFO, rc=rc)
      endif

      call FieldBundle_Regrid(fldsFrAtm, is_local%wrap%FBAtm_a, is_local%wrap%FBAtm_o, &
        consfmap=is_local%wrap%RH_a2o_consf, &
        consdmap=is_local%wrap%RH_a2o_consd, &
        bilnrmap=is_local%wrap%RH_a2o_bilnr, &
        patchmap=is_local%wrap%RH_a2o_patch, &
        string='a2oflx', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

    endif

    call FieldBundle_GetFldPtr(is_local%wrap%FBAtm_o, 'inst_height_lowest', zbot, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtm_o, 'inst_zonal_wind_height_lowest', ubot, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtm_o, 'inst_merid_wind_height_lowest', vbot, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
!    call FieldBundle_GetFldPtr(is_local%wrap%FBAtm_o, 'inst_pot_temp_height_lowest', thbot, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtm_o, 'inst_spec_humid_height_lowest', qbot, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
!    call FieldBundle_GetFldPtr(is_local%wrap%FBAtm_o, 'inst_density_height_lowest', rbot, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtm_o, 'inst_temp_height_lowest', tbot, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtm_o, 'inst_pres_height_lowest', pbot, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- ocean fields input
    call FieldBundle_GetFldPtr(is_local%wrap%FBOcn_o, 'ocean_mask', mask, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBOcn_o, 'ocn_current_zonal', us, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBOcn_o, 'ocn_current_merid', vs, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBOcn_o, 'sea_surface_temperature', ts, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- atm/ocn fluxes output
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtmOcn_o, 'mean_up_lw_flx_ocn', lwup, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtmOcn_o, 'mean_sensi_heat_flx_atm_into_ocn', sen, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtmOcn_o, 'mean_laten_heat_flx_atm_into_ocn', lat, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtmOcn_o, 'mean_evap_rate_atm_into_ocn', evap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtmOcn_o, 'stress_on_air_ocn_zonal', taux, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtmOcn_o, 'stress_on_air_ocn_merid', tauy, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtmOcn_o, 'temperature_2m', tref, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtmOcn_o, 'humidity_2m', qref, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call FieldBundle_GetFldPtr(is_local%wrap%FBAtmOcn_o, 'wind_speed_squared_10m', duu10n, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !--- flux calculation
    do j=lbound(zbot,2),ubound(zbot,2)
    do i=lbound(zbot,1),ubound(zbot,1)
      zbot1(1)  = zbot(i,j)
      ubot1(1)  = ubot(i,j)
      vbot1(1)  = vbot(i,j)
      thbot1(1) = tbot(i,j)*((100000._ESMF_KIND_R8/pbot(i,j))**0.286_ESMF_KIND_R8)  ! tcx temporary
!tcx      thbot1(1) = thbot(i,j)
      qbot1(1)  = qbot(i,j)
      rbot1(1)  =pbot(i,j)/(287.058_ESMF_KIND_R8*(1._ESMF_KIND_R8+0.608_ESMF_KIND_R8*qbot(i,j))*tbot(i,j)) ! tcx temporary
!tcx      rbot1(1)  = rbot(i,j)
      tbot1(1)  = tbot(i,j)
      us1(1)    = us(i,j)
      vs1(1)    = vs(i,j)
      ts1(1)    = ts(i,j)

      mask1(1)  = nint(mask(i,j))
      call shr_flux_atmOcn(1         ,zbot1(1)  ,ubot1(1)  ,vbot1(1)  ,thbot1(1) ,   &
                           qbot1(1)  ,rbot1(1)  ,tbot1(1)  ,us1(1)    ,vs1(1)    ,   &
                           ts1(1)    ,mask1(1)  ,sen1(1)   ,lat1(1)   ,lwup1(1)  ,   &
!                           evap1(1)  ,taux1(1)  ,tauy1(1)  ,tref1(1)  ,qref1(1)  ,duu10n1(1))
!tcx include this for the time being to get over the initialization hump
                           evap1(1)  ,taux1(1)  ,tauy1(1)  ,tref1(1)  ,qref1(1)  ,duu10n1(1), &
                           missval = 0.0_ESMF_KIND_R8  )
      sen(i,j)    = sen1(1)
      lat(i,j)    = lat1(1)
      lwup(i,j)   = lwup1(1)
      evap(i,j)   = evap1(1)
      taux(i,j)   = taux1(1)
      tauy(i,j)   = tauy1(1)
      tref(i,j)   = tref1(1)
      qref(i,j)   = qref1(1)
      duu10n(i,j) = duu10n1(1)
    enddo
    enddo

    !---------------------------------------
    !--- atmocn diagnostics
    !---------------------------------------

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBAtmOcn_o, trim(subname)//' FBAtmOcn_o ', rc=rc)
    endif

    if (statewrite_flag) then
      ! write the fields imported from ocn to file
      call ESMF_FieldBundleWrite(is_local%wrap%FBAtmOcn_o, 'fields_med_atmocn.nc', &
        singleFile=.true., overwrite=.true., timeslice=is_local%wrap%fastcntr, &
        iofmt=ESMF_IOFMT_NETCDF, rc=rc)  
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine MedPhase_atm_ocn_flux

  !-----------------------------------------------------------------------------

  subroutine MedPhase_slow(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_StateItem_Flag)   :: itemType
    type(InternalState)         :: is_local
    integer                     :: i,j,n
    character(ESMF_MAXSTR)      :: fieldname1(10),fieldname2(10),fieldname3(10)
    real(ESMF_KIND_R8), pointer :: dataPtr1(:,:),dataPtr2(:,:),dataPtr3(:,:)
    real(ESMF_KIND_R8), pointer :: atmwgt(:,:),icewgt(:,:),customwgt(:,:)
    logical                     :: checkOK, checkOK1, checkOK2
    character(len=*),parameter  :: subname='(module_MEDIATOR:MedPhase_slow)'

    if(profile_memory) call ESMF_VMLogMemInfo("Entering "//trim(subname))
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    call MedPhase_prep_ocn(gcomp,rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if(profile_memory) call ESMF_VMLogMemInfo("Leaving "//trim(subname))

  end subroutine MedPhase_slow

  !-----------------------------------------------------------------------------

  subroutine MedPhase_prep_ocn(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_StateItem_Flag)   :: itemType
    type(InternalState)         :: is_local
    integer                     :: i,j,n
    character(ESMF_MAXSTR)      :: fieldname1(10),fieldname2(10),fieldname3(10)
    real(ESMF_KIND_R8), pointer :: dataPtr1(:,:),dataPtr2(:,:),dataPtr3(:,:)
    real(ESMF_KIND_R8), pointer :: atmwgt(:,:),icewgt(:,:),customwgt(:,:)
    real(ESMF_KIND_R8), pointer :: atmwgt1(:,:),icewgt1(:,:),wgtp01(:,:),wgtm01(:,:)
    logical                     :: checkOK, checkOK1, checkOK2
    character(len=*),parameter  :: subname='(module_MEDIATOR:MedPhase_prep_ocn)'

    if(profile_memory) call ESMF_VMLogMemInfo("Entering "//trim(subname))
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_TimeGet(time,timestring=timestr)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (dbug_flag > 1) then
      call ESMF_ClockPrint(clock, options="currTime", &
        preString="-------->"//trim(subname)//" mediating for: ", &
        unit=msgString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (statewrite_flag) then
      ! write the fields imported from ocn to file
      call NUOPC_Write(NState_OcnImp, &
        fldsFrOcn%shortname(1:fldsFrOcn%num), &
        "field_med_from_ocn_", timeslice=is_local%wrap%slowcntr, &
        overwrite=.true., relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    !---------------------------------------
    !--- average atm, ice, lnd accumulators
    !---------------------------------------

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBaccA_B4avg ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBaccI_B4avg ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBaccL_B4avg ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumHyd, trim(subname)//' FBaccH_B4avg ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBaccAO_B4avg ', rc=rc)
    endif

    call FieldBundle_average(is_local%wrap%FBaccumAtm, is_local%wrap%accumcntAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call FieldBundle_average(is_local%wrap%FBaccumIce, is_local%wrap%accumcntIce, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call FieldBundle_average(is_local%wrap%FBaccumLnd, is_local%wrap%accumcntLnd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call FieldBundle_average(is_local%wrap%FBaccumHyd, is_local%wrap%accumcntHyd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    
    call FieldBundle_average(is_local%wrap%FBaccumAtmOcn, is_local%wrap%accumcntAtmOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBaccA_avg ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBaccI_avg ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBaccL_avg ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumHyd, trim(subname)//' FBaccH_avg ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBaccAO_avg ', rc=rc)
    endif

    !---------------------------------------
    !--- regrid average atm+ice+lnd+hyd fields to ocean grid
    !---------------------------------------

    if (is_local%wrap%a2o_active) then
      call ESMF_LogWrite(trim(subname)//' calling FBRegrid FBaccumAtm to FBAtm_o', ESMF_LOGMSG_INFO, rc=rc)
      call FieldBundle_Regrid(fldsFrAtm, is_local%wrap%FBaccumAtm, is_local%wrap%FBAtm_o, &
        consfmap=is_local%wrap%RH_a2o_consf, &
        consdmap=is_local%wrap%RH_a2o_consd, &
        bilnrmap=is_local%wrap%RH_a2o_bilnr, &
        patchmap=is_local%wrap%RH_a2o_patch, &
        string='a2o', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is_local%wrap%i2o_active) then
      call ESMF_LogWrite(trim(subname)//' calling FBRegrid FBaccumIce to FBIce_o', ESMF_LOGMSG_INFO, rc=rc)
      call FieldBundle_Regrid(fldsFrIce, is_local%wrap%FBaccumIce, is_local%wrap%FBIce_o, &
        consfmap=is_local%wrap%RH_i2o_consf, &
        consdmap=is_local%wrap%RH_i2o_consd, &
        bilnrmap=is_local%wrap%RH_i2o_bilnr, &
        patchmap=is_local%wrap%RH_i2o_patch, &
        fcopymap=is_local%wrap%RH_i2o_fcopy, &
        string='i2o', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBAtm_o, trim(subname)//' FBAtm_o ', rc=rc)
      call FieldBundle_diagnose(is_local%wrap%FBIce_o, trim(subname)//' FBIce_o ', rc=rc)
    endif

! tcx Xgrid
    ! XGrid intermediary required? instantiate FBXgrid FieldBundle?
    ! call ESMF_FieldBundleRegrid(is_local%wrap%FBaccumAtm, FBXgrid, is_local%wrap%RHa2x, &
    !    termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    ! call ESMF_FieldBundleRegrid(FBXgrid, is_local%wrap%FBforOcn  , is_local%wrap%RHx2o, &
    !    termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    ! tcraig temporarily copy
    
    call fieldBundle_copy(is_local%wrap%FBforOcn, is_local%wrap%FBAtm_o, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBforOcn, is_local%wrap%FBIce_o, rc=rc)
    call fieldBundle_copy(is_local%wrap%FBforOcn, is_local%wrap%FBAccumAtmOcn, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBforOcn, trim(subname)//' FB4ocn_AFregrid ', rc=rc)
    endif

    !---------------------------------------
    !--- custom calculations to ocn
    !---------------------------------------

!    if (dbug_flag > 1) then
!      call FieldBundle_diagnose(is_local%wrap%FBforOcn, trim(subname)//' FB4ocn_AFcc ', rc=rc)
!    endif

    !---------------------------------------
    !--- merges to ocn
    !---------------------------------------

    if (is_local%wrap%i2a_active) then

    ! atm and ice fraction
    ! atmwgt and icewgt are the "normal" fractions
    ! atmwgt1, icewgt1, and wgtp01 are the fractions that switch between atm and mediator fluxes
    ! wgtp01 and wgtm01 are the same just one is +1 and the other is -1 to change sign
    !   depending on the ice fraction.  atmwgt1+icewgt1+wgtp01 = 1.0 always, either
    !   wgtp01 is 1 (when ice fraction is 0) or wgtp01 is zero (when ice fraction is > 0)
    call FieldBundle_GetFldPtr(is_local%wrap%FBIce_o, 'ice_fraction', icewgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(atmwgt(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    allocate(customwgt(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    allocate(atmwgt1(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    allocate(icewgt1(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    allocate(wgtp01(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    allocate(wgtm01(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    do j=lbound(icewgt,2),ubound(icewgt,2)
    do i=lbound(icewgt,1),ubound(icewgt,1)
      atmwgt(i,j)  = 1.0_ESMF_KIND_R8 - icewgt(i,j)
      atmwgt1(i,j) = atmwgt(i,j)
      icewgt1(i,j) = icewgt(i,j)
      wgtp01(i,j)  = 0.0_ESMF_KIND_R8
      wgtm01(i,j)  = 0.0_ESMF_KIND_R8
      if (atmocn_flux_from_atm .and. icewgt(i,j) <= 0.0_ESMF_KIND_R8) then
        atmwgt1(i,j) = 0.0_ESMF_KIND_R8
        icewgt1(i,j) = 0.0_ESMF_KIND_R8
        wgtp01(i,j)  = 1.0_ESMF_KIND_R8
        wgtm01(i,j)  = -1.0_ESMF_KIND_R8
      endif
      ! check wgts do add to 1 as expected
      if (abs(atmwgt(i,j) + icewgt(i,j) - 1.0_ESMF_KIND_R8) > 1.0e-12 .or. &
          abs(atmwgt1(i,j) + icewgt1(i,j) + wgtp01(i,j) - 1.0_ESMF_KIND_R8) > 1.0e-12 .or. &
          abs(atmwgt1(i,j) + icewgt1(i,j) - wgtm01(i,j) - 1.0_ESMF_KIND_R8) > 1.0e-12) then
        call ESMF_LogWrite(trim(subname)//": ERROR atm + ice fracs inconsistent", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        return
      endif
    enddo
    enddo

    !-------------
    ! mean_evap_rate = mean_laten_heat_flux * (1-ice_fraction)/const_lhvap
    !-------------

    customwgt = wgtm01 / const_lhvap
    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn     , 'mean_evap_rate'             , & 
                                is_local%wrap%FBAccumAtmOcn, 'mean_evap_rate_atm_into_ocn', atmwgt1, &
                                is_local%wrap%FBAtm_o      , 'mean_laten_heat_flx'        , customwgt, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !-------------
    ! field_for_ocn = field_from_atm * (1-ice_fraction)
    !-------------

    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn, 'mean_fprec_rate', & 
                                is_local%wrap%FBAtm_o , 'mean_fprec_rate', atmwgt, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

! not used by mom, mom uses net, also mean_down_lw_flx not connected to ocn
!    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn, 'mean_down_lw_flx', & 
!                                is_local%wrap%FBAtm_o , 'mean_down_lw_flx', atmwgt, &
!                                rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out

! not used by mom, mom uses evap
! hycom uses latent heat flux
    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn     , 'mean_laten_heat_flx'             , & 
                                is_local%wrap%FBAccumAtmOcn, 'mean_laten_heat_flx_atm_into_ocn', atmwgt1, &
                                is_local%wrap%FBAtm_o      , 'mean_laten_heat_flx'             , wgtm01, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn     , 'mean_net_lw_flx'   , & 
                                is_local%wrap%FBAtm_o      , 'mean_down_lw_flx'  , atmwgt1, &
                                is_local%wrap%FBAccumAtmOcn, 'mean_up_lw_flx_ocn', atmwgt1, &
                                is_local%wrap%FBAtm_o      , 'mean_net_lw_flx'   , wgtp01, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

! not used by mom, mom uses net, also mean_up_lw_flx not recvd from atm
!    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn     , 'mean_up_lw_flx'    , & 
!                                is_local%wrap%FBAccumAtmOcn, 'mean_up_lw_flx_ocn', atmwgt1, &
!                                is_local%wrap%FBAtm_o      , 'mean_up_lw_flx'    , wgtp01, &
!                                rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out

    !-------------
    ! field_for_ocn = field_from_atm * (1-ice_fraction) + field_from_ice * (ice_fraction)
    !-------------

    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn, 'mean_prec_rate'                , & 
                                is_local%wrap%FBAtm_o , 'mean_prec_rate'                , atmwgt, &
                                is_local%wrap%FBIce_o , 'mean_fresh_water_to_ocean_rate', icewgt, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn     , 'mean_sensi_heat_flx'             , & 
                                is_local%wrap%FBAccumAtmOcn, 'mean_sensi_heat_flx_atm_into_ocn', atmwgt1, &
                                is_local%wrap%FBIce_o      , 'net_heat_flx_to_ocn'             , icewgt1, &
                                is_local%wrap%FBAtm_o      , 'mean_sensi_heat_flx'             , wgtm01, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn     , 'mean_zonal_moment_flx'  , & 
                                is_local%wrap%FBAccumAtmOcn, 'stress_on_air_ocn_zonal', atmwgt1, &
                                is_local%wrap%FBIce_o      , 'stress_on_ocn_ice_zonal', icewgt1, &
                                is_local%wrap%FBAtm_o      , 'mean_zonal_moment_flx'  , wgtm01, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn     , 'mean_merid_moment_flx'  , & 
                                is_local%wrap%FBAccumAtmOcn, 'stress_on_air_ocn_merid', atmwgt1, &
                                is_local%wrap%FBIce_o      , 'stress_on_ocn_ice_merid', icewgt1, &
                                is_local%wrap%FBAtm_o      , 'mean_merid_moment_flx'  , wgtm01, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !-------------
    ! netsw_for_ocn = downsw_from_atm * (1-ocn_albedo) * (1-ice_fraction) + pensw_from_ice * (ice_fraction)
    !-------------

    customwgt = atmwgt * (1.0 - 0.06)
!    customwgt = (1.0 - 0.06)
    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_flx' , & 
                                is_local%wrap%FBAtm_o ,'mean_down_sw_flx',customwgt, &
                                is_local%wrap%FBIce_o ,'mean_sw_pen_to_ocn' ,icewgt, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_vis_dir_flx' , & 
                                is_local%wrap%FBAtm_o ,'mean_down_sw_vis_dir_flx',customwgt, &
                                is_local%wrap%FBIce_o ,'mean_net_sw_vis_dir_flx' ,icewgt, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_vis_dif_flx' , & 
                                is_local%wrap%FBAtm_o ,'mean_down_sw_vis_dif_flx',customwgt, &
                                is_local%wrap%FBIce_o ,'mean_net_sw_vis_dif_flx',icewgt, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_ir_dir_flx' , & 
                                is_local%wrap%FBAtm_o ,'mean_down_sw_ir_dir_flx',customwgt, &
                                is_local%wrap%FBIce_o ,'mean_net_sw_ir_dir_flx',icewgt, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_FieldMerge(is_local%wrap%FBforOcn,'mean_net_sw_ir_dif_flx' , & 
                                is_local%wrap%FBAtm_o ,'mean_down_sw_ir_dif_flx',customwgt, &
                                is_local%wrap%FBIce_o ,'mean_net_sw_ir_dif_flx',icewgt, &
                                rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !-------------
    ! End merges
    !-------------

    deallocate(atmwgt,customwgt,atmwgt1,icewgt1,wgtp01)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is_local%wrap%FBforOcn, trim(subname)//' FB4ocn_AFmrg ', rc=rc)
    endif
    
    endif

    !---------------------------------------
    !--- zero accumulator
    !---------------------------------------

    is_local%wrap%accumcntAtm = 0
    call fieldBundle_reset(is_local%wrap%FBaccumAtm, value=czero, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    is_local%wrap%accumcntIce = 0
    call fieldBundle_reset(is_local%wrap%FBaccumIce, value=czero, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    is_local%wrap%accumcntLnd = 0
    call fieldBundle_reset(is_local%wrap%FBaccumLnd, value=czero, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    is_local%wrap%accumcntHyd = 0
    call fieldBundle_reset(is_local%wrap%FBaccumHyd, value=czero, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    is_local%wrap%accumcntAtmOcn = 0
    call fieldBundle_reset(is_local%wrap%FBaccumAtmOcn, value=czero, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
!tcx      call FieldBundle_diagnose(is_local%wrap%FBaccumAtm, trim(subname)//' FBacc_AFzero ', rc=rc)
!tcx      call FieldBundle_diagnose(is_local%wrap%FBaccumIce, trim(subname)//' FBacc_AFzero ', rc=rc)
!dcr      call FieldBundle_diagnose(is_local%wrap%FBaccumLnd, trim(subname)//' FBacc_AFzero ', rc=rc)
!dcr      call FieldBundle_diagnose(is_local%wrap%FBaccumHyd, trim(subname)//' FBacc_AFzero ', rc=rc)
!tcx      call FieldBundle_diagnose(is_local%wrap%FBaccumAtmOcn, trim(subname)//' FBacc_AFzero ', rc=rc)
    endif

    !--- set export State to special value for testing

    call state_reset(NState_OcnExp, value=spval, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
      call State_diagnose(NState_OcnExp, trim(subname)//' es_AF99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call fieldBundle_copy(NState_OcnExp, is_local%wrap%FBforOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 1) then
      call State_diagnose(NState_OcnExp, trim(subname)//' es_AFcp ', rc=rc)
    endif

    if (statewrite_flag) then
      ! write the fields exported to ocn to file
      call NUOPC_Write(NState_OcnExp, &
        fldsToOcn%shortname(1:fldsToOcn%num), &
        "field_med_to_ocn_", timeslice=is_local%wrap%slowcntr, &
        relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    !---------------------------------------

    is_local%wrap%slowcntr = is_local%wrap%slowcntr + 1

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if(profile_memory) call ESMF_VMLogMemInfo("Leaving "//trim(subname))

  end subroutine MedPhase_prep_ocn

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine MedPhase_write_restart(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime
    type(ESMF_Time)            :: startTime
    type(ESMF_TimeInterval)    :: elapsedTime
    ! ESMF_TimeInterval
    integer*8                  :: sec8
    integer                    :: yr,mon,day,hr,min,sec
    character(len=128)         :: fname
    character(len=*),parameter :: subname='(module_MEDIATOR:MedPhase_write_restart)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    if (restart_interval > 0) then
      call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      call ESMF_ClockGet(clock,currTime=currTime,startTime=startTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      elapsedTime = currTime - startTime

      call ESMF_TimeIntervalGet(elapsedTime,s_i8=sec8,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out 

      if (mod(sec8,restart_interval) == 0) then
        write(msgString,*) trim(subname)//' restart at sec8= ',sec8,restart_interval
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=dbrc)

        call ESMF_TimeGet(currTime,yy=yr,mm=mon,dd=day,h=hr,m=min,s=sec,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        write(fname,'(i4.4,2i2.2,a,3i2.2,a)') yr,mon,day,'-',hr,min,sec,'_mediator'
        write(msgString,*) trim(subname)//' restart to '//trim(fname)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=dbrc)

        call Mediator_restart(gcomp,'write',trim(fname),rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine MedPhase_write_restart

  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(InternalState)  :: is_local
    integer              :: stat
    character(len=*),parameter :: subname='(module_MEDIATOR:Finalize)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call Mediator_restart(gcomp,'write','mediator',rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    ! Destroy objects inside of internal state.
    ! TODO: destroy objects inside objects

    call fieldBundle_clean(is_local%wrap%FBaccumAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

! tcraig - generates errors
!    call fieldBundle_clean(is_local%wrap%FBaccumOcn, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_clean(is_local%wrap%FBaccumIce, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_clean(is_local%wrap%FBaccumLnd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_clean(is_local%wrap%FBaccumHyd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_clean(is_local%wrap%FBaccumAtmOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_clean(is_local%wrap%FBforAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_clean(is_local%wrap%FBforOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_clean(is_local%wrap%FBforIce, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_clean(is_local%wrap%FBforLnd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_clean(is_local%wrap%FBforHyd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Deallocate the internal state memory.
    deallocate(is_local%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    call ESMF_LogWrite(trim(subname)//" complete", ESMF_LOGMSG_INFO, rc=dbrc)

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Finalize

  !-----------------------------------------------------------------------------
  subroutine Mediator_restart(gcomp,mode,bfname,rc)
    !
    ! read/write mediator restart file
    !
    type(ESMF_GridComp)  :: gcomp
    character(len=*), intent(in)    :: mode
    character(len=*), intent(in)    :: bfname
    integer         , intent(inout) :: rc

    type(InternalState)  :: is_local
    character(len=1280)  :: fname
    integer              :: funit
    logical              :: fexists
    character(len=*),parameter :: subname='(module_MEDIATOR:Mediator_restart)'

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    if (mode /= 'write' .and. mode /= 'read') then
      call ESMF_LogWrite(trim(subname)//": ERROR mode not allowed "//trim(mode), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    ! Get the internal state from Component.
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    fname = trim(bfname)//'_FBaccumAtm_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumAtm,read_rest_FBaccumAtm,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    fname = trim(bfname)//'_FBaccumOcn_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumOcn,read_rest_FBaccumOcn,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    fname = trim(bfname)//'_FBaccumIce_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumIce,read_rest_FBaccumIce,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    fname = trim(bfname)//'_FBaccumLnd_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumLnd,read_rest_FBaccumLnd,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    fname = trim(bfname)//'_FBaccumHyd_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumHyd,read_rest_FBaccumHyd,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    fname = trim(bfname)//'_FBaccumAtmOcn_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBaccumAtmOcn,read_rest_FBaccumAtmOcn,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    fname = trim(bfname)//'_FBAtm_a_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBAtm_a,read_rest_FBAtm_a,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (mode == 'read') then
      call fieldBundle_copy(NState_AtmImp, is_local%wrap%FBAtm_a, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    fname = trim(bfname)//'_FBIce_i_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBIce_i,read_rest_FBIce_i,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (mode == 'read') then
      call fieldBundle_copy(NState_IceImp, is_local%wrap%FBIce_i, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    fname = trim(bfname)//'_FBOcn_o_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBOcn_o,read_rest_FBOCN_o,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (mode == 'read') then
      call fieldBundle_copy(NState_OcnImp, is_local%wrap%FBOcn_o, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    fname = trim(bfname)//'_FBLnd_l_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBLnd_l,read_rest_FBLnd_l,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (mode == 'read') then
      call fieldBundle_copy(NState_LndImp, is_local%wrap%FBLnd_l, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    fname = trim(bfname)//'_FBHyd_h_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBHyd_h,read_rest_FBHyd_h,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (mode == 'read') then
      call fieldBundle_copy(NState_HydImp, is_local%wrap%FBHyd_h, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    fname = trim(bfname)//'_FBAtmOcn_o_restart.nc'
    call FieldBundle_RWFields(mode,fname,is_local%wrap%FBAtmOcn_o,read_rest_FBAtmOcn_o,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    funit = 1101
    fname = trim(bfname)//'_scalars_restart.txt'
    if (mode == 'write') then
      call ESMF_LogWrite(trim(subname)//": write "//trim(fname), ESMF_LOGMSG_INFO, rc=dbrc)
      open(funit,file=fname,form='formatted')
      write(funit,*) is_local%wrap%accumcntAtm
      write(funit,*) is_local%wrap%accumcntOcn
      write(funit,*) is_local%wrap%accumcntIce
      write(funit,*) is_local%wrap%accumcntAtmOcn
      write(funit,*) is_local%wrap%accumcntLnd
      write(funit,*) is_local%wrap%accumcntHyd
      close(funit)
    elseif (mode == 'read') then
      inquire(file=fname,exist=fexists)
      if (fexists) then
        call ESMF_LogWrite(trim(subname)//": read "//trim(fname), ESMF_LOGMSG_INFO, rc=dbrc)
        open(funit,file=fname,form='formatted')
        ! DCR - temporary skip reading Lnd and Hyd until components are added to test case
        !       restart files
        is_local%wrap%accumcntAtm=0
        is_local%wrap%accumcntOcn=0
        is_local%wrap%accumcntIce=0
        is_local%wrap%accumcntAtmOcn=0
        is_local%wrap%accumcntLnd=0
        is_local%wrap%accumcntHyd=0
        read (funit,*) is_local%wrap%accumcntAtm
        read (funit,*) is_local%wrap%accumcntOcn
        read (funit,*) is_local%wrap%accumcntIce
        read (funit,*) is_local%wrap%accumcntAtmOcn
!        read (funit,*) is_local%wrap%accumcntLnd
!        read (funit,*) is_local%wrap%accumcntHyd
        close(funit)
      else
        read_rest_FBaccumAtm = .false.
        read_rest_FBaccumOcn = .false.
        read_rest_FBaccumIce = .false.
        read_rest_FBaccumLnd = .false.
        read_rest_FBaccumHyd = .false.
        read_rest_FBaccumAtmOcn = .false.
      endif
    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Mediator_restart

  !-----------------------------------------------------------------------------

  subroutine FieldBundle_RWFields(mode,fname,FB,flag,rc)
    character(len=*) :: mode
    character(len=*) :: fname
    type(ESMF_FieldBundle) :: FB
    logical,optional :: flag
    integer,optional :: rc

    ! local variables
    type(ESMF_Field) :: field
    character(len=ESMF_MAXSTR) :: name
    integer :: fieldcount, n
    logical :: fexists
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_RWFields)'

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(fname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (mode == 'write') then
      call ESMF_LogWrite(trim(subname)//": write "//trim(fname), ESMF_LOGMSG_INFO, rc=dbrc)
      call ESMF_FieldBundleWrite(FB, fname, &
        singleFile=.true., status=ESMF_FILESTATUS_REPLACE, iofmt=ESMF_IOFMT_NETCDF, rc=rc)  
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call fieldBundle_diagnose(FB, 'write '//trim(fname), rc)
    elseif (mode == 'read') then
      inquire(file=fname,exist=fexists)
      if (fexists) then
        call ESMF_LogWrite(trim(subname)//": read "//trim(fname), ESMF_LOGMSG_INFO, rc=dbrc)
!-----------------------------------------------------------------------------------------------------
! tcraig, ESMF_FieldBundleRead fails if a field is not on the field bundle, but we really want to just 
! ignore that field and read the rest, so instead read each field one at a time through ESMF_FieldRead
!        call ESMF_FieldBundleRead (FB, fname, &
!          singleFile=.true., iofmt=ESMF_IOFMT_NETCDF, rc=rc)  
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!          line=__LINE__, file=__FILE__)) call ESMF_LogWrite(trim(subname)//' WARNING missing fields',rc=dbrc)
!-----------------------------------------------------------------------------------------------------
        call ESMF_FieldBundleGet(FB, fieldCount=fieldCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        do n = 1,fieldCount
          call fieldBundle_getName(FB, n, name, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call fieldBundle_getFieldN(FB, n, field, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldRead (field, fname, iofmt=ESMF_IOFMT_NETCDF, rc=rc)  
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) call ESMF_LogWrite(trim(subname)//' WARNING missing field '//trim(name),rc=dbrc)
        enddo

        call fieldBundle_diagnose(FB, 'read '//trim(fname), rc)
        if (present(flag)) flag = .true.
      endif
    else
      call ESMF_LogWrite(trim(subname)//": mode WARNING "//trim(fname)//" mode="//trim(mode), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(fname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine FieldBundle_RWFields

  !-----------------------------------------------------------------------------

  subroutine Compute_RHs(FBsrc, FBdst, bilnrmap, consfmap, consdmap, patchmap, fcopymap, &
                         srcMaskValue, dstMaskValue, &
                         fldlist1, fldlist2, fldlist3, fldlist4, string, rc)
    type(ESMF_FieldBundle) :: FBsrc
    type(ESMF_FieldBundle) :: FBdst
    type(ESMF_Routehandle),optional :: bilnrmap
    type(ESMF_Routehandle),optional :: consfmap
    type(ESMF_Routehandle),optional :: consdmap
    type(ESMF_Routehandle),optional :: patchmap
    type(ESMF_Routehandle),optional :: fcopymap
    integer               ,optional :: srcMaskValue
    integer               ,optional :: dstMaskValue
    type(fld_list_type)   ,optional :: fldlist1
    type(fld_list_type)   ,optional :: fldlist2
    type(fld_list_type)   ,optional :: fldlist3
    type(fld_list_type)   ,optional :: fldlist4
    character(len=*)      ,optional :: string
    integer               ,optional :: rc

    ! local variables
    integer :: n
    character(len=128) :: lstring
    logical :: do_consf, do_consd, do_bilnr, do_patch, do_fcopy
    integer :: lsrcMaskValue, ldstMaskValue
    type(ESMF_Field)            :: fldsrc, flddst
    real(ESMF_KIND_R8), pointer :: factorList(:)
    character(len=*),parameter :: subname='(module_MEDIATOR:Compute_RHs)'
type(ESMF_VM):: vm

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(lstring)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (present(string)) then
      lstring = trim(string)
    else
      lstring = " "
    endif

    if (present(srcMaskValue)) then
      lsrcMaskValue = srcMaskValue 
    else
      lsrcMaskValue = ispval_mask  ! chosen to be ignored
    endif

    if (present(dstMaskValue)) then
      ldstMaskValue = dstMaskValue 
    else
      ldstMaskValue = ispval_mask  ! chosen to be ignored
    endif

    if (.not.present(rc)) then
      call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR rc expected", ESMF_LOGMSG_INFO, rc=rc)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    !---------------------------------------------------
    !--- decide which map files to generate.
    !--- check fldlist mapping types.
    !--- if there are no fldlists, then generate them all.
    !--- but only for mapfiles that are passed into the subroutine.
    !---------------------------------------------------

    if (.not.present(fldlist1) .and. .not.present(fldlist2) .and. &
        .not.present(fldlist3) .and. .not.present(fldlist4)) then
      do_bilnr = .true.
      do_consf = .true.
      do_consd = .true.
      do_patch = .true.
      do_fcopy = .true.
    else
      do_bilnr = .false.
      do_consf = .false.
      do_consd = .false.
      do_patch = .false.
      do_fcopy = .false.
    endif

    if (present(fldlist1)) then
      do n = 1,fldlist1%num
        if (fldlist1%mapping(n) == 'bilinear'    ) do_bilnr = .true.
        if (fldlist1%mapping(n) == "conservefrac") do_consf = .true.
        if (fldlist1%mapping(n) == "conservedst" ) do_consd = .true.
        if (fldlist1%mapping(n) == 'patch'       ) do_patch = .true.
        if (fldlist1%mapping(n) == 'copy'        ) do_fcopy = .true.
      enddo
    endif

    if (present(fldlist2)) then
      do n = 1,fldlist2%num
        if (fldlist2%mapping(n) == 'bilinear'    ) do_bilnr = .true.
        if (fldlist2%mapping(n) == "conservefrac") do_consf = .true.
        if (fldlist2%mapping(n) == "conservedst" ) do_consd = .true.
        if (fldlist2%mapping(n) == 'patch'       ) do_patch = .true.
        if (fldlist2%mapping(n) == 'copy'        ) do_fcopy = .true.
      enddo
    endif

    if (present(fldlist3)) then
      do n = 1,fldlist3%num
        if (fldlist3%mapping(n) == 'bilinear'    ) do_bilnr = .true.
        if (fldlist3%mapping(n) == "conservefrac") do_consf = .true.
        if (fldlist3%mapping(n) == "conservedst" ) do_consd = .true.
        if (fldlist3%mapping(n) == 'patch'       ) do_patch = .true.
        if (fldlist3%mapping(n) == 'copy'        ) do_fcopy = .true.
      enddo
    endif

    if (present(fldlist4)) then
      do n = 1,fldlist4%num
        if (fldlist4%mapping(n) == 'bilinear'    ) do_bilnr = .true.
        if (fldlist4%mapping(n) == "conservefrac") do_consf = .true.
        if (fldlist4%mapping(n) == "conservedst" ) do_consd = .true.
        if (fldlist4%mapping(n) == 'patch'       ) do_patch = .true.
        if (fldlist4%mapping(n) == 'copy'        ) do_fcopy = .true.
      enddo
    endif

    if (.not.present(bilnrmap)) do_bilnr = .false.
    if (.not.present(consfmap)) do_consf = .false.
    if (.not.present(consdmap)) do_consd = .false.
    if (.not.present(patchmap)) do_patch = .false.
    if (.not.present(fcopymap)) do_fcopy = .false.

    !---------------------------------------------------
    !--- get single fields from bundles
    !--- assumes all fields in the bundle are on identical grids
    !---------------------------------------------------

    call fieldBundle_getFieldN(FBsrc, 1, fldsrc, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call fieldBundle_getFieldN(FBdst, 1, flddst, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !---------------------------------------------------
    !--- bilinear
    !---------------------------------------------------

    if (do_bilnr) then
      call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=bilnrmap, &
        srcMaskValues=(/lsrcMaskValue/), dstMaskValues=(/ldstMaskValue/), &
        regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
        polemethod=polemethod, &
        srcTermProcessing=srcTermProcessing_Value, &
        factorList=factorList, ignoreDegenerate=.true., &
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#if 0
if (trim(string)=="o2a_weights") then 
  call ESMF_VMGetCurrent(vm)
  call ESMF_VMBarrier(vm)
  call ESMF_Finalize(endflag=ESMF_END_ABORT)
endif
#endif
      if (rhprint_flag) then
        call NUOPC_Write(factorList, "array_med_"//trim(lstring)//"_bilnr.nc", rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_RouteHandlePrint(bilnrmap, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (ESMF_RouteHandleIsCreated(bilnrmap, rc=rc)) then
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": computed RH bilnr", ESMF_LOGMSG_INFO, rc=dbrc)
      else
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": failed   RH bilnr", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif
      
    !---------------------------------------------------
    !--- conservative frac
    !---------------------------------------------------

    if (do_consf) then
      call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=consfmap, &
        srcMaskValues=(/lsrcMaskValue/), dstMaskValues=(/ldstMaskValue/), &
        regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
        normType=ESMF_NORMTYPE_FRACAREA, &
        srcTermProcessing=srcTermProcessing_Value, &
        factorList=factorList, ignoreDegenerate=.true., &
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#if 0
if (trim(string)=="o2a_weights") then
  call ESMF_VMGetCurrent(vm)
  call ESMF_VMBarrier(vm)
  call ESMF_Finalize(endflag=ESMF_END_ABORT)
endif
#endif
      if (rhprint_flag) then
        call NUOPC_Write(factorList, "array_med_"//trim(lstring)//"_consf.nc", rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_RouteHandlePrint(consfmap, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (ESMF_RouteHandleIsCreated(consfmap, rc=rc)) then
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": computed RH consf", ESMF_LOGMSG_INFO, rc=dbrc)
      else
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": failed   RH consf", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
     endif
      
    !---------------------------------------------------
    !--- conservative dst
    !---------------------------------------------------

    if (do_consd) then
      call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=consdmap, &
        srcMaskValues=(/lsrcMaskValue/), dstMaskValues=(/ldstMaskValue/), &
        regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
        normType=ESMF_NORMTYPE_DSTAREA, &
        srcTermProcessing=srcTermProcessing_Value, &
        factorList=factorList, ignoreDegenerate=.true., &
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (rhprint_flag) then
        call NUOPC_Write(factorList, "array_med_"//trim(lstring)//"_consd.nc", rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_RouteHandlePrint(consdmap, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (ESMF_RouteHandleIsCreated(consdmap, rc=rc)) then
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": computed RH consd", ESMF_LOGMSG_INFO, rc=dbrc)
      else
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": failed   RH consd", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
     endif
      
    !---------------------------------------------------
    !--- patch
    !---------------------------------------------------

    if (do_patch) then
      call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=patchmap, &
        srcMaskValues=(/lsrcMaskValue/), dstMaskValues=(/ldstMaskValue/), &
        regridmethod=ESMF_REGRIDMETHOD_PATCH, &
        polemethod=polemethod, &
        srcTermProcessing=srcTermProcessing_Value, &
        factorList=factorList, ignoreDegenerate=.true., &
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (rhprint_flag) then
        call NUOPC_Write(factorList, "array_med_"//trim(lstring)//"_patch.nc", rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_RouteHandlePrint(patchmap, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (ESMF_RouteHandleIsCreated(patchmap, rc=rc)) then
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": computed RH patch", ESMF_LOGMSG_INFO, rc=dbrc)
      else
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": failed   RH patch", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    !---------------------------------------------------
    !--- copy
    !---------------------------------------------------

    if (do_fcopy) then
      call ESMF_FieldRedistStore(fldsrc, flddst, &
        routehandle=fcopymap, &
        ignoreUnmatchedIndices=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (rhprint_flag) then
        call ESMF_RouteHandlePrint(fcopymap, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (ESMF_RouteHandleIsCreated(fcopymap, rc=rc)) then
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": computed RH fcopy", ESMF_LOGMSG_INFO, rc=dbrc)
      else
        call ESMF_LogWrite(trim(subname)//trim(lstring)//": failed   RH fcopy", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(lstring)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Compute_RHs

  !-----------------------------------------------------------------------------
  subroutine Grid_CreateCoords(gridNew,gridOld,rc)

    ! ----------------------------------------------
    ! Create FieldBundle from another FieldBundle.
    ! Zero out new FieldBundle
    ! If grid is not passed, use grid from FBin
    ! ----------------------------------------------
    type(ESMF_Grid), intent(inout) :: gridNew
    type(ESMF_Grid), intent(inout) :: gridOld
    integer        , intent(out)   :: rc

    ! local variables
    integer :: localDE, localDECount
    type(ESMF_DistGrid)        :: distgrid
    type(ESMF_CoordSys_Flag)   :: coordSys
    type(ESMF_Index_Flag)      :: indexflag
    real(ESMF_KIND_R8),pointer :: dataPtr1(:,:), dataPtr2(:,:)
    integer                    :: dimCount
    integer, pointer           :: gridEdgeLWidth(:), gridEdgeUWidth(:)
    character(len=*),parameter :: subname='(module_MEDIATOR:Grid_createCoords)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_LogWrite(trim(subname)//": tcxA", ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_GridGet(gridold, dimCount=dimCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(gridEdgeLWidth(dimCount),gridEdgeUWidth(dimCount))
    call ESMF_GridGet(gridold,distgrid=distgrid, coordSys=coordSys, indexflag=indexflag, dimCount=dimCount, &
       gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, localDECount=localDECount, rc=rc)
!       localDECount=localDECount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_LogWrite(trim(subname)//": tcxB", ESMF_LOGMSG_INFO, rc=dbrc)

    write(msgString,*) trim(subname)//' localDECount = ',localDECount
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) trim(subname)//' dimCount = ',dimCount
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) trim(subname)//' size(gELW) = ',size(gridEdgeLWidth)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) trim(subname)//' gridEdgeLWidth = ',gridEdgeLWidth
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    write(msgString,*) trim(subname)//' gridEdgeUWidth = ',gridEdgeUWidth
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_LogWrite(trim(subname)//": tcxC", ESMF_LOGMSG_INFO, rc=dbrc)

    gridnew = ESMF_GridCreate(distgrid=distgrid, coordSys=coordSys, indexflag=indexflag, &
       gridEdgeLWidth=gridEdgeLWidth, gridEdgeUWidth=gridEdgeUWidth, rc=rc)
!       rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    deallocate(gridEdgeLWidth, gridEdgeUWidth)

    call ESMF_GridAddCoord(gridnew, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridAddCoord(gridnew, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    do localDE = 0,localDeCount-1

      call ESMF_GridGetCoord(gridold, coordDim=1, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=dataPtr1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_GridGetCoord(gridnew, coordDim=1, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=dataPtr2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      dataPtr2 = dataPtr1

      call ESMF_GridGetCoord(gridold, coordDim=2, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=dataPtr1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_GridGetCoord(gridnew, coordDim=2, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=dataPtr2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      dataPtr2 = dataPtr1

      call ESMF_GridGetCoord(gridold, coordDim=1, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=dataPtr1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_GridGetCoord(gridnew, coordDim=1, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=dataPtr2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      dataPtr2 = dataPtr1

      call ESMF_GridGetCoord(gridold, coordDim=2, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=dataPtr1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_GridGetCoord(gridnew, coordDim=2, localDE=localDE,  &
        staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=dataPtr2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      dataPtr2 = dataPtr1

    enddo

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Grid_CreateCoords

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_initFromFB(FBout, FBin, grid, name, rc)
    ! ----------------------------------------------
    ! Create FieldBundle from another FieldBundle.
    ! Zero out new FieldBundle
    ! If grid is not passed, use grid from FBin
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    type(ESMF_Grid)       , intent(in), optional :: grid
    character(len=*)      , intent(in), optional :: name
    integer               , intent(out)   :: rc

    ! local variables
    integer                    :: i,j,n
    integer                    :: fieldCount
    character(ESMF_MAXSTR) ,pointer :: fieldNameList(:)
    type(ESMF_Field)           :: field
    type(ESMF_Grid)            :: lgrid
    character(ESMF_MAXSTR)     :: lname
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_initFromFB)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
      
    lname = 'undefined'
    if (present(name)) then
       lname = trim(name)
    endif

    call ESMF_FieldBundleGet(FBin, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FBin, fieldNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (present(grid)) then
      call fieldBundle_init(FBout, fieldNameList=fieldNameList, grid=grid, name=trim(lname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else
      call ESMF_FieldBundleGet(FBin, grid=lgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call fieldBundle_init(FBout, fieldNameList=fieldNameList, grid=lgrid, name=trim(lname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif
    deallocate(fieldNameList)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldbundle_initFromFB

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_init(FieldBundle, fieldNameList, grid, State, name, rc)
    ! ----------------------------------------------
    ! Create FieldBundle from fieldNameList and grid OR
    ! from State with State field grids or argument grid
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    character(len=*)      , intent(in), optional  :: fieldNameList(:)
    type(ESMF_Grid)       , intent(in), optional  :: grid
    type(ESMF_State)      , intent(in), optional  :: State  ! check if fieldnames are there
    character(len=*)      , intent(in), optional  :: name
    integer               , intent(out) :: rc

    ! local variables
    integer                    :: i,j,n,fieldCount
    character(ESMF_MAXSTR)     :: lname
    character(ESMF_MAXSTR),allocatable :: lfieldNameList(:)
    type(ESMF_Field)           :: field,lfield
    type(ESMF_Grid)            :: lgrid
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_init)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lname = 'undefined'
    if (present(name)) then
       lname = trim(name)
    endif

    !--- check argument consistency

    if (present(fieldNameList)) then
      if (.not. present(grid)) then
        call ESMF_LogWrite(trim(subname)//": ERROR fieldNameList requires grid", ESMF_LOGMSG_INFO, rc=rc)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
      if (present(State)) then
        call ESMF_LogWrite(trim(subname)//": ERROR fieldNameList cannot pass State", ESMF_LOGMSG_INFO, rc=rc)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    endif

    FieldBundle = ESMF_FieldBundleCreate(name=trim(lname), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (present(fieldNameList)) then
      do n = 1, size(fieldNameList)
        field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name=fieldNameList(n), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_FieldBundleAdd(FieldBundle, (/field/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (dbug_flag > 1) then
          call ESMF_LogWrite(trim(subname)//":"//trim(lname)//":add  "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
        endif
      enddo  ! fieldNameList
    endif  ! present fldnamelist

    if (present(State)) then
      call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(lfieldNameList(fieldCount))
      call ESMF_StateGet(State, itemNameList=lfieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      do n = 1, fieldCount
        if (present(grid)) then
          field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name=lfieldNameList(n), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldBundleAdd(FieldBundle, (/field/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dbug_flag > 1) then
            call ESMF_LogWrite(trim(subname)//":"//trim(lname)//":add  "//trim(lfieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          endif

        else
          call ESMF_StateGet(State, itemName=trim(lfieldNameList(n)), field=lfield, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldGet(lfield, grid=lgrid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          field = ESMF_FieldCreate(lgrid, ESMF_TYPEKIND_R8, name=lfieldNameList(n), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldBundleAdd(FieldBundle, (/field/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (dbug_flag > 1) then
            call ESMF_LogWrite(trim(subname)//":"//trim(lname)//":add  "//trim(lfieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          endif
        endif
      enddo  ! fieldCount
      deallocate(lfieldNameList)
    endif  ! present State

    call fieldBundle_reset(FieldBundle, value=spval_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
  end subroutine fieldBundle_init

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_getName(FieldBundle, fieldnum, fieldname, rc)
    ! ----------------------------------------------
    ! Destroy fields in FieldBundle and FieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    integer               , intent(in)    :: fieldnum
    character(len=*)      , intent(out)   :: fieldname
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_getName)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    fieldname = ' '

    call ESMF_FieldBundleGet(FieldBundle, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (fieldnum > fieldCount) then
      call ESMF_LogWrite(trim(subname)//": ERROR fieldnum > fieldCount ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FieldBundle, fieldNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    fieldname = fieldNameList(fieldnum)

    deallocate(fieldNameList)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_getName

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_getFieldN(FieldBundle, fieldnum, field, rc)
    ! ----------------------------------------------
    ! Destroy fields in FieldBundle and FieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    integer               , intent(in)    :: fieldnum
    type(ESMF_Field)      , intent(inout) :: field
    integer               , intent(out)   :: rc

    ! local variables
    character(len=ESMF_MAXSTR) :: name
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_getFieldN)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call fieldBundle_getName(FieldBundle, fieldnum, name, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_FieldBundleGet(FieldBundle, fieldName=name, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_getFieldN

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_getFieldName(FieldBundle, fieldname, field, rc)
    ! ----------------------------------------------
    ! Destroy fields in FieldBundle and FieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    character(len=*)      , intent(in)    :: fieldname
    type(ESMF_Field)      , intent(inout) :: field
    integer               , intent(out)   :: rc

    ! local variables
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_getFieldName)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(FieldBundle, fieldName=fieldname, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_getFieldName

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_clean(FieldBundle, rc)
    ! ----------------------------------------------
    ! Destroy fields in FieldBundle and FieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    type(ESMF_Field)            :: field
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_clean)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(FieldBundle, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FieldBundle, fieldNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    do n = 1, fieldCount
      call ESMF_FieldBundleGet(FieldBundle, fieldName=fieldNameList(n), field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_FieldDestroy(field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo
    call ESMF_FieldBundleDestroy(FieldBundle, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    deallocate(fieldNameList)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_clean

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_reset(FieldBundle, value, rc)
    ! ----------------------------------------------
    ! Set all fields to value in FieldBundle
    ! If value is not provided, reset to 0.0
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    real(ESMF_KIND_R8)    , intent(in), optional :: value
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    real(ESMF_KIND_R8)          :: lvalue
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_reset)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lvalue = czero
    if (present(value)) then
      lvalue = value
    endif

    call ESMF_FieldBundleGet(FieldBundle, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FieldBundle, fieldNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    do n = 1, fieldCount
      call FieldBundle_GetFldPtr(FieldBundle, fieldNameList(n), dataPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      do j=lbound(dataPtr,2),ubound(dataPtr,2)
      do i=lbound(dataPtr,1),ubound(dataPtr,1)
        dataPtr(i,j) = lvalue
      enddo
      enddo

    enddo
    deallocate(fieldNameList)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_reset

  !-----------------------------------------------------------------------------

  subroutine FieldBundle_FieldCopy(FBin,fldin,FBout,fldout,rc)
    ! ----------------------------------------------
    ! Copy a field in a field bundle to another field in a field bundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBin
    character(len=*)      , intent(in)    :: fldin
    type(ESMF_FieldBundle), intent(inout) :: FBout
    character(len=*)      , intent(in)    :: fldout
    integer               , intent(out)   :: rc

    ! local
    real(ESMF_KIND_R8), pointer :: dataPtrIn(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtrOut(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_FieldCopy)'

    rc = ESMF_SUCCESS
    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (FieldBundle_FldChk(FBin , trim(fldin) , rc=rc) .and. &
        FieldBundle_FldChk(FBout, trim(fldout), rc=rc)) then

      call FieldBundle_GetFldPtr(FBin, trim(fldin), dataPtrIn, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call FieldBundle_GetFldPtr(FBout, trim(fldout), dataPtrOut, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      if (.not.FldPtr_SameCheck(dataPtrIn, dataPtrOut, subname, rc)) then
        call ESMF_LogWrite(trim(subname)//": ERROR fname not present with FBin", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        return
      endif

      dataPtrOut = dataPtrIn

    else

       if (dbug_flag > 1) then
         call ESMF_LogWrite(trim(subname)//" field not found: "//trim(fldin)//","//trim(fldout), ESMF_LOGMSG_INFO, rc=dbrc)
       endif

    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine FieldBundle_FieldCopy

  !-----------------------------------------------------------------------------

  subroutine Fieldbundle_Regrid(fldlist, FBin, FBout, consfmap, consdmap, bilnrmap, patchmap, &
                                fcopymap, string, rc)
    type(fld_list_type)    :: fldlist
    type(ESMF_FieldBundle) :: FBin
    type(ESMF_FieldBundle) :: FBout
    type(ESMF_Routehandle),optional :: consfmap
    type(ESMF_Routehandle),optional :: consdmap
    type(ESMF_Routehandle),optional :: bilnrmap
    type(ESMF_Routehandle),optional :: patchmap
    type(ESMF_Routehandle),optional :: fcopymap
    character(len=*)      ,optional :: string
    integer               ,optional :: rc

    ! local variables
    integer :: n
    character(len=64) :: lstring
    logical :: okconsf, okconsd, okbilnr, okpatch, okfcopy
    character(len=*),parameter :: subname='(module_MEDIATOR:Fieldbundle_Regrid)'

    rc = ESMF_SUCCESS
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(lstring)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (present(string)) then
      lstring = trim(string)
    else
      lstring = " "
    endif

    if (.not.present(rc)) then
      call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR rc expected", ESMF_LOGMSG_INFO, rc=rc)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    okconsf = .false.
    if (present(consfmap)) then
      if (ESMF_RouteHandleIsCreated(consfmap, rc=rc)) okconsf = .true.
    endif

    okconsd = .false.
    if (present(consdmap)) then
      if (ESMF_RouteHandleIsCreated(consdmap, rc=rc)) okconsd = .true.
    endif

    okbilnr = .false.
    if (present(bilnrmap)) then
      if (ESMF_RouteHandleIsCreated(bilnrmap, rc=rc)) okbilnr = .true.
    endif

    okpatch = .false.
    if (present(patchmap)) then
      if (ESMF_RouteHandleIsCreated(patchmap, rc=rc)) okpatch = .true.
    endif

    okfcopy = .false.
    if (present(fcopymap)) then
      if (ESMF_RouteHandleIsCreated(fcopymap, rc=rc)) okfcopy = .true.
    endif

    do n = 1,fldlist%num
      if (FieldBundle_FldChk(FBin , fldlist%shortname(n), rc=rc) .and. &
          FieldBundle_FldChk(FBout, fldlist%shortname(n), rc=rc)) then

        if (dbug_flag > 1) then
          call ESMF_LogWrite(trim(subname)//trim(lstring)//": map="//trim(fldlist%mapping(n))// &
            ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
        endif

        if (fldlist%mapping(n) == 'bilinear') then
          if (.not. okbilnr) then
            call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR RH not available for "//trim(fldlist%mapping(n))// &
              ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
            rc = ESMF_FAILURE
            return
          endif
          call FieldBundle_FieldRegrid(FBin,fldlist%shortname(n), &
                                       FBout,fldlist%shortname(n), &
                                       bilnrmap,rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

        elseif (fldlist%mapping(n) == "conservefrac") then
          if (.not. okconsf) then
            call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR RH not available for "//trim(fldlist%mapping(n))// &
              ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
            rc = ESMF_FAILURE
            return
          endif
          call FieldBundle_FieldRegrid(FBin ,fldlist%shortname(n), &
                                       FBout,fldlist%shortname(n), &
                                       consfmap, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

        elseif (fldlist%mapping(n) == "conservedst") then
          if (.not. okconsd) then
            call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR RH not available for "//trim(fldlist%mapping(n))// &
              ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
            rc = ESMF_FAILURE
            return
          endif
          call FieldBundle_FieldRegrid(FBin ,fldlist%shortname(n), &
                                       FBout,fldlist%shortname(n), &
                                       consdmap, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

        elseif (fldlist%mapping(n) == 'patch') then
          if (.not. okpatch) then
            call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR RH not available for "//trim(fldlist%mapping(n))// &
              ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
            rc = ESMF_FAILURE
            return
          endif
          call FieldBundle_FieldRegrid(FBin ,fldlist%shortname(n), &
                                       FBout,fldlist%shortname(n), &
                                       patchmap,rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

        elseif (fldlist%mapping(n) == 'copy') then
          !-------------------------------------------
          ! copy will not exist for some grid combinations
          ! so fall back to conservative frac as a secondary option
          !-------------------------------------------
          if (.not. okfcopy) then
            if (.not. okconsf) then
              call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR RH not available for "//trim(fldlist%mapping(n))// &
                ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
              rc = ESMF_FAILURE
              return
            else
              call ESMF_LogWrite(trim(subname)//trim(lstring)//": NOTE using conservative instead of copy for"// &
                " fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
              call FieldBundle_FieldRegrid(FBin ,fldlist%shortname(n), &
                                           FBout,fldlist%shortname(n), &
                                           consfmap,rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif
          else
            call FieldBundle_FieldRegrid(FBin ,fldlist%shortname(n), &
                                         FBout,fldlist%shortname(n), &
                                         fcopymap,rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif

        else
          call ESMF_LogWrite(trim(subname)//trim(lstring)//": ERROR unrecognized mapping "//trim(fldlist%mapping(n))// &
            ": fld="//trim(fldlist%shortname(n)), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return
        endif

      else
        if (dbug_flag > 1) then
          call ESMF_LogWrite(trim(subname)//" field not found in FB: "//trim(fldlist%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
        endif
      endif
    enddo

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//trim(lstring)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Fieldbundle_Regrid
      
  !-----------------------------------------------------------------------------

  subroutine FieldBundle_FieldRegrid(FBin,fldin,FBout,fldout,RH,rc)
    ! ----------------------------------------------
    ! Regrid a field in a field bundle to another field in a field bundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBin
    character(len=*)      , intent(in)    :: fldin
    type(ESMF_FieldBundle), intent(inout) :: FBout
    character(len=*)      , intent(in)    :: fldout
    type(ESMF_RouteHandle), intent(inout) :: RH
    integer               , intent(out)   :: rc

    ! local
    type(ESMF_Field) :: field1, field2
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_FieldRegrid)'

    rc = ESMF_SUCCESS
    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (FieldBundle_FldChk(FBin , trim(fldin) , rc=rc) .and. &
        FieldBundle_FldChk(FBout, trim(fldout), rc=rc)) then

      call FieldBundle_GetFieldName(FBin, trim(fldin), field1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call FieldBundle_GetFieldName(FBout, trim(fldout), field2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      call ESMF_FieldRegrid(field1, field2, routehandle=RH, &
        termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else

      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//" field not found: "//trim(fldin)//","//trim(fldout), ESMF_LOGMSG_INFO, rc=dbrc)
      endif

    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine FieldBundle_FieldRegrid

  !-----------------------------------------------------------------------------

  subroutine FieldBundle_FieldMerge(FBout, fnameout, &
                                    FBinA, fnameA, wgtA, &
                                    FBinB, fnameB, wgtB, &
                                    FBinC, fnameC, wgtC, &
                                    FBinD, fnameD, wgtD, &
                                    FBinE, fnameE, wgtE, rc)
    ! ----------------------------------------------
    ! Supports up to a five way merge
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    character(len=*)      , intent(in)    :: fnameout
    type(ESMF_FieldBundle), intent(in), optional :: FBinA
    character(len=*)      , intent(in), optional :: fnameA
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtA(:,:)
    type(ESMF_FieldBundle), intent(in), optional :: FBinB
    character(len=*)      , intent(in), optional :: fnameB
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtB(:,:)
    type(ESMF_FieldBundle), intent(in), optional :: FBinC
    character(len=*)      , intent(in), optional :: fnameC
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtC(:,:)
    type(ESMF_FieldBundle), intent(in), optional :: FBinD
    character(len=*)      , intent(in), optional :: fnameD
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtD(:,:)
    type(ESMF_FieldBundle), intent(in), optional :: FBinE
    character(len=*)      , intent(in), optional :: fnameE
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtE(:,:)
    integer               , intent(out)   :: rc

    ! local variables
    real(ESMF_KIND_R8), pointer :: dataOut(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    real(ESMF_KIND_R8), pointer :: wgt(:,:)
    character(len=ESMF_MAXSTR) :: fname
    integer :: lb1,ub1,lb2,ub2,i,j,n
    logical :: wgtfound, FBinfound
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_FieldMerge)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc=ESMF_SUCCESS

    if (.not. FieldBundle_FldChk(FBout, trim(fnameout), rc=rc)) then
      call ESMF_LogWrite(trim(subname)//": WARNING output field not in FBout, skipping merge of: "//trim(fnameout), ESMF_LOGMSG_WARNING, line=__LINE__, file=__FILE__, rc=dbrc)
      return
    endif
    call FieldBundle_GetFldPtr(FBout, trim(fnameout), dataOut, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    lb1 = lbound(dataOut,1)
    ub1 = ubound(dataOut,1)
    lb2 = lbound(dataOut,2)
    ub2 = ubound(dataOut,2)

    dataOut = czero

    ! check each field has a fieldname passed in
    if ((present(FBinA) .and. .not.present(fnameA)) .or. &
        (present(FBinB) .and. .not.present(fnameB)) .or. &
        (present(FBinC) .and. .not.present(fnameC)) .or. &
        (present(FBinD) .and. .not.present(fnameD)) .or. &
        (present(FBinE) .and. .not.present(fnameE))) then
      call ESMF_LogWrite(trim(subname)//": ERROR fname not present with FBin", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    ! check that each field passed in actually exists, if not DO NOT do any merge
    FBinfound = .true.
    if (present(FBinA)) then
      fname = fnameA
      if (.not. FieldBundle_FldChk(FBinA, trim(fname), rc=rc)) then
        FBinfound = .false.
      endif
    endif
    if (present(FBinB)) then
      fname = fnameB
      if (.not. FieldBundle_FldChk(FBinB, trim(fname), rc=rc)) then
        FBinfound = .false.
      endif
    endif
    if (present(FBinC)) then
      fname = fnameC
      if (.not. FieldBundle_FldChk(FBinC, trim(fname), rc=rc)) then
        FBinfound = .false.
      endif
    endif
    if (present(FBinD)) then
      fname = fnameD
      if (.not. FieldBundle_FldChk(FBinD, trim(fname), rc=rc)) then
        FBinfound = .false.
      endif
    endif
    if (present(FBinE)) then
      fname = fnameE
      if (.not. FieldBundle_FldChk(FBinE, trim(fname), rc=rc)) then
        FBinfound = .false.
      endif
    endif
    if (.not. FBinfound) then
      call ESMF_LogWrite(trim(subname)//": WARNING field: "//trim(fname)//" :not found in FBin, skipping merge of: "//trim(fnameout), ESMF_LOGMSG_WARNING, line=__LINE__, file=__FILE__, rc=dbrc)
      return
    endif

    ! n=1,5 represents adding A to E inputs if they exist
    do n = 1,5
      FBinfound = .false.
      wgtfound = .false.

      if (n == 1 .and. present(FBinA)) then
        fname = fnameA
        FBinfound = .true.
        call FieldBundle_GetFldPtr(FBinA, trim(fname), dataPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (present(wgtA)) then
          wgtfound = .true.
          wgt => wgtA
        endif

      elseif (n == 2 .and. present(FBinB)) then
        fname = fnameB
        FBinfound = .true.
        call FieldBundle_GetFldPtr(FBinB, trim(fname), dataPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (present(wgtB)) then
          wgtfound = .true.
          wgt => wgtB
        endif

      elseif (n == 3 .and. present(FBinC)) then
        fname = fnameC
        FBinfound = .true.
        call FieldBundle_GetFldPtr(FBinC, trim(fname), dataPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (present(wgtC)) then
          wgtfound = .true.
          wgt => wgtC
        endif

      elseif (n == 4 .and. present(FBinD)) then
        fname = fnameD
        FBinfound = .true.
        call FieldBundle_GetFldPtr(FBinD, trim(fname), dataPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (present(wgtD)) then
          wgtfound = .true.
          wgt => wgtD
        endif

      elseif (n == 5 .and. present(FBinE)) then
        fname = fnameE
        FBinfound = .true.
        call FieldBundle_GetFldPtr(FBinE, trim(fname), dataPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (present(wgtE)) then
          wgtfound = .true.
          wgt => wgtE
        endif

      endif

      if (FBinfound) then
        if (.not.FldPtr_SameCheck(dataPtr, dataOut, subname, rc)) then
          call ESMF_LogWrite(trim(subname)//": ERROR FBin wrong size", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return
        endif

        if (wgtfound) then
          if (.not.FldPtr_SameCheck(dataPtr, wgt, subname, rc)) then
            call ESMF_LogWrite(trim(subname)//": ERROR wgt wrong size", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
            rc = ESMF_FAILURE
            return
          endif
          do j = lb2,ub2
          do i = lb1,ub1
            dataOut(i,j) = dataOut(i,j) + dataPtr(i,j) * wgt(i,j)
          enddo
          enddo
        else
          do j = lb2,ub2
          do i = lb1,ub1
            dataOut(i,j) = dataOut(i,j) + dataPtr(i,j)
          enddo
          enddo
        endif  ! wgtfound

      endif  ! FBin found
    enddo  ! n

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine FieldBundle_FieldMerge

  !-----------------------------------------------------------------------------

  subroutine state_reset(State, value, rc)
    ! ----------------------------------------------
    ! Set all fields to value in State
    ! If value is not provided, reset to 0.0
    ! ----------------------------------------------
    type(ESMF_State)  , intent(inout) :: State
    real(ESMF_KIND_R8), intent(in), optional :: value
    integer           , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    real(ESMF_KIND_R8)          :: lvalue
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:state_reset)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lvalue = czero
    if (present(value)) then
      lvalue = value
    endif

    call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(fieldNameList(fieldCount))
    call ESMF_StateGet(State, itemNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    do n = 1, fieldCount
      call State_GetFldPtr(State, fieldNameList(n), dataPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      do j=lbound(dataPtr,2),ubound(dataPtr,2)
      do i=lbound(dataPtr,1),ubound(dataPtr,1)
        dataPtr(i,j) = lvalue
      enddo
      enddo

    enddo
    deallocate(fieldNameList)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine state_reset

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_average(FieldBundle, count, rc)
    ! ----------------------------------------------
    ! Set all fields to zero in FieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    integer               , intent(in)    :: count
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_average)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    if (count == 0) then

      call ESMF_LogWrite(trim(subname)//": WARNING count is 0", ESMF_LOGMSG_INFO, rc=dbrc)
!      call ESMF_LogWrite(trim(subname)//": WARNING count is 0 set avg to spval", ESMF_LOGMSG_INFO, rc=dbrc)
!      call fieldBundle_reset(FieldBundle, value=spval, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!        line=__LINE__, file=__FILE__)) return  ! bail out

    else

      call ESMF_FieldBundleGet(FieldBundle, fieldCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(fieldNameList(fieldCount))
      call ESMF_FieldBundleGet(FieldBundle, fieldNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      do n = 1, fieldCount
        call FieldBundle_GetFldPtr(FieldBundle, fieldNameList(n), dataPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        do j=lbound(dataPtr,2),ubound(dataPtr,2)
        do i=lbound(dataPtr,1),ubound(dataPtr,1)
          dataPtr(i,j) = dataPtr(i,j) / real(count, ESMF_KIND_R8)
        enddo
        enddo
      enddo
      deallocate(fieldNameList)

    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_average

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_diagnose(FieldBundle, string, rc)
    ! ----------------------------------------------
    ! Diagnose status of fieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    character(len=*)      , intent(in), optional :: string
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    character(len=64)           :: lstring
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_diagnose)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lstring = ''
    if (present(string)) then
       lstring = trim(string)
    endif

    call ESMF_FieldBundleGet(FieldBundle, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FieldBundle, fieldNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    do n = 1, fieldCount
      call FieldBundle_GetFldPtr(FieldBundle, fieldNameList(n), dataPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      write(msgString,'(A,3g14.7)') trim(subname)//' '//trim(lstring)//':'//trim(fieldNameList(n)), &
        minval(dataPtr),maxval(dataPtr),sum(dataPtr)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
    deallocate(fieldNameList)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_diagnose

  !-----------------------------------------------------------------------------

  subroutine array_diagnose(array, string, rc)
    ! ----------------------------------------------
    ! Diagnose status of fieldBundle
    ! ----------------------------------------------
    type(ESMF_Array), intent(inout) :: array
    character(len=*), intent(in), optional :: string
    integer         , intent(out) :: rc

    ! local variables
    character(len=64)           :: lstring
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:array_diagnose)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! this is not working yet, not sure about dataPtr dim/type
    return

    lstring = ''
    if (present(string)) then
       lstring = trim(string)
    endif

    call ESMF_ArrayGet(Array, farrayPtr=dataPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    write(msgString,'(A,3g14.7)') trim(subname)//' '//trim(lstring), &
        minval(dataPtr),maxval(dataPtr),sum(dataPtr)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine array_diagnose

  !-----------------------------------------------------------------------------

  subroutine state_diagnose(State, string, rc)
    ! ----------------------------------------------
    ! Diagnose status of fieldBundle
    ! ----------------------------------------------
    type(ESMF_State), intent(inout) :: State
    character(len=*), intent(in), optional :: string
    integer         , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    character(len=64)           :: lstring
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:state_diagnose)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lstring = ''
    if (present(string)) then
       lstring = trim(string)
    endif

    call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(fieldNameList(fieldCount))
    call ESMF_StateGet(State, itemNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    do n = 1, fieldCount
      call State_GetFldPtr(State, fieldNameList(n), dataPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      write(msgString,'(A,3g14.7)') trim(subname)//' '//trim(lstring)//':'//trim(fieldNameList(n)), &
        minval(dataPtr),maxval(dataPtr),sum(dataPtr)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
    deallocate(fieldNameList)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine state_diagnose

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_copyFB2FB(FBout, FBin, rc)
    ! ----------------------------------------------
    ! Copy common field names from FBin to FBout
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    integer               , intent(out)   :: rc

    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_copyFB2FB)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call fieldBundle_accum(FBout, FBin, copy=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_copyFB2FB

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_copyFB2ST(STout, FBin, rc)
    ! ----------------------------------------------
    ! Copy common field names from FBin to STout
    ! ----------------------------------------------
    type(ESMF_State)      , intent(inout) :: STout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    integer               , intent(out)   :: rc

    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_copyFB2ST)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call fieldBundle_accum(STout, FBin, copy=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_copyFB2ST

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_copyST2FB(FBout, STin, rc)
    ! ----------------------------------------------
    ! Copy common field names from STin to FBout
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_State)      , intent(in)    :: STin
    integer               , intent(out)   :: rc

    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_copyST2FB)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call fieldBundle_accum(FBout, STin, copy=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_copyST2FB

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_accumFB2FB(FBout, FBin, copy, rc)
    ! ----------------------------------------------
    ! Accumulate common field names from FBin to FBout
    ! If copy is passed in and true, the this is a copy
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    logical, optional     , intent(in)    :: copy
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    logical                     :: exists
    logical                     :: lcopy
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: dataPtri(:,:), dataPtro(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_accumFB2FB)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lcopy = .false.  ! accumulate by default
    if (present(copy)) then
      lcopy = copy
    endif

    call ESMF_FieldBundleGet(FBout, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FBout, fieldNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    do n = 1, fieldCount
      call ESMF_FieldBundleGet(FBin, fieldName=fieldNameList(n), isPresent=exists, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (exists) then
        call FieldBundle_GetFldPtr(FBin,  fieldNameList(n), dataPtri, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call FieldBundle_GetFldPtr(FBout, fieldNameList(n), dataPtro, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        if (.not.FldPtr_SameCheck(dataPtro, dataPtri, subname, rc)) then
           call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
           return
        endif

        if (lcopy) then
          do j=lbound(dataPtri,2),ubound(dataPtri,2)
          do i=lbound(dataPtri,1),ubound(dataPtri,1)
            dataPtro(i,j) = dataPtri(i,j)
          enddo
          enddo
        else
          do j=lbound(dataPtri,2),ubound(dataPtri,2)
          do i=lbound(dataPtri,1),ubound(dataPtri,1)
            dataPtro(i,j) = dataPtro(i,j) + dataPtri(i,j)
          enddo
          enddo
        endif

      endif
    enddo

    deallocate(fieldNameList)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_accumFB2FB
  !-----------------------------------------------------------------------------

  subroutine fieldBundle_accumST2FB(FBout, STin, copy, rc)
    ! ----------------------------------------------
    ! Accumulate common field names from State to FieldBundle
    ! If copy is passed in and true, the this is a copy
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_State)      , intent(in)    :: STin
    logical, optional     , intent(in)    :: copy
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    logical                     :: lcopy
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: dataPtrS(:,:), dataPtrB(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_accumST2FB)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lcopy = .false.
    if (present(copy)) then
      lcopy = copy
    endif

    call ESMF_FieldBundleGet(FBout, fieldCount=fieldCount, rc=rc)
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FBout, fieldNameList=fieldNameList, rc=rc)
    do n = 1, fieldCount
      call ESMF_StateGet(STin, itemName=fieldNameList(n), itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then

        call State_GetFldPtr(STin, fieldNameList(n), dataPtrS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call FieldBundle_GetFldPtr(FBout, fieldNameList(n), dataPtrB, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        if (.not.FldPtr_SameCheck(dataPtrS, dataPtrB, subname, rc)) then
           call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
           return
        endif

        if (lcopy) then
          do j=lbound(dataPtrB,2),ubound(dataPtrB,2)
          do i=lbound(dataPtrB,1),ubound(dataPtrB,1)
            dataPtrB(i,j) = dataPtrS(i,j)
          enddo
          enddo
        else
          do j=lbound(dataPtrB,2),ubound(dataPtrB,2)
          do i=lbound(dataPtrB,1),ubound(dataPtrB,1)
            dataPtrB(i,j) = dataPtrB(i,j) + dataPtrS(i,j)
          enddo
          enddo
        endif

      endif  ! statefound
    enddo  ! fieldCount

    deallocate(fieldNameList)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_accumST2FB

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_accumFB2ST(STout, FBin, copy, rc)
    ! ----------------------------------------------
    ! Accumulate common field names from FieldBundle to State
    ! If copy is passed in and true, the this is a copy
    ! ----------------------------------------------
    type(ESMF_State)      , intent(inout) :: STout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    logical, optional     , intent(in)    :: copy
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    logical                     :: lcopy
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: dataPtrS(:,:), dataPtrB(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_accumFB2ST)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lcopy = .false.
    if (present(copy)) then
      lcopy = copy
    endif

    call ESMF_FieldBundleGet(FBin, fieldCount=fieldCount, rc=rc)
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FBin, fieldNameList=fieldNameList, rc=rc)
    do n = 1, fieldCount
      call ESMF_StateGet(STout, itemName=fieldNameList(n), itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then

        call FieldBundle_GetFldPtr(FBin, fieldNameList(n), dataPtrB, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call State_GetFldPtr(STout, fieldNameList(n), dataPtrS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        if (.not.FldPtr_SameCheck(dataPtrS, dataPtrB, subname, rc)) then
           call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
           return
        endif

        if (lcopy) then
          do j=lbound(dataPtrB,2),ubound(dataPtrB,2)
          do i=lbound(dataPtrB,1),ubound(dataPtrB,1)
            dataPtrS(i,j) = dataPtrB(i,j)
          enddo
          enddo
        else
          do j=lbound(dataPtrB,2),ubound(dataPtrB,2)
          do i=lbound(dataPtrB,1),ubound(dataPtrB,1)
            dataPtrS(i,j) = dataPtrS(i,j) + dataPtrB(i,j)
          enddo
          enddo
        endif

      endif  ! statefound
    enddo  ! fieldCount

    deallocate(fieldNameList)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_accumFB2ST

  !-----------------------------------------------------------------------------

  logical function FieldBundle_FldChk(FB, fldname, rc)
    type(ESMF_FieldBundle), intent(in)  :: FB
    character(len=*)      , intent(in)  :: fldname
    integer               , intent(out) :: rc

    ! local variables
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_FldChk)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    FieldBundle_FldChk = .false.

    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isPresent) then
       FieldBundle_FldChk = .true.
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end function FieldBundle_FldChk

  !-----------------------------------------------------------------------------

  subroutine FieldBundle_GetFldPtr(FB, fldname, fldptr, rc)
    type(ESMF_FieldBundle), intent(in)  :: FB
    character(len=*)      , intent(in)  :: fldname
    real(ESMF_KIND_R8), pointer, intent(in) :: fldptr(:,:)
    integer               , intent(out) :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_GetFldPtr)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    if (.not. FieldBundle_FldChk(FB, trim(fldname), rc=rc)) then
      call ESMF_LogWrite(trim(subname)//": ERROR field not in FB "//trim(fldname), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), field=lfield, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine FieldBundle_GetFldPtr

  !-----------------------------------------------------------------------------

  subroutine FieldBundle_SetFldPtr(FB, fldname, val, rc)
    type(ESMF_FieldBundle), intent(in)  :: FB
    character(len=*)      , intent(in)  :: fldname
    real(ESMF_KIND_R8)    , intent(in)  :: val
    integer               , intent(out) :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    real(ESMF_KIND_R8), pointer :: fldptr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_SetFldPtr)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    if (.not. FieldBundle_FldChk(FB, trim(fldname), rc=rc)) then
      call ESMF_LogWrite(trim(subname)//": ERROR field not in FB "//trim(fldname), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), field=lfield, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    fldptr = val

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine FieldBundle_SetFldPtr

  !-----------------------------------------------------------------------------

  subroutine State_GetFldPtr(ST, fldname, fldptr, rc)
    type(ESMF_State), intent(in)  :: ST
    character(len=*), intent(in)  :: fldname
    real(ESMF_KIND_R8), pointer, intent(in) :: fldptr(:,:)
    integer         , intent(out) :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    character(len=*),parameter :: subname='(module_MEDIATOR:State_GetFldPtr)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_StateGet(ST, itemName=trim(fldname), field=lfield, rc=rc)
    if (dbug_flag > 5) then
       call ESMF_LogWrite(trim(subname)//": fldname ="//trim(fldname), ESMF_LOGMSG_INFO,rc=dbrc)
    endif
!    call ESMF_StatePrint(ST,rc=dbrc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine State_GetFldPtr

  !-----------------------------------------------------------------------------

  subroutine State_SetFldPtr(ST, fldname, val, rc)
    type(ESMF_State), intent(in)  :: ST
    character(len=*), intent(in)  :: fldname
    real(ESMF_KIND_R8),          intent(in) :: val
    integer         , intent(out) :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    real(ESMF_KIND_R8), pointer :: fldptr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:State_SetFldPtr)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_StateGet(ST, itemName=trim(fldname), field=lfield, rc=rc)
    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": fldname ="//trim(fldname), ESMF_LOGMSG_INFO,rc=dbrc)
    endif
!    call ESMF_StatePrint(ST,rc=dbrc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    fldptr = val

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine State_SetFldPtr

  !-----------------------------------------------------------------------------

  logical function FldPtr_SameCheck(fldptr1, fldptr2, cstring, rc)
    real(ESMF_KIND_R8), pointer, intent(in)  :: fldptr1(:,:)
    real(ESMF_KIND_R8), pointer, intent(in)  :: fldptr2(:,:)
    character(len=*)           , intent(in)  :: cstring
    integer                    , intent(out) :: rc

    ! local variables
    character(len=*),parameter :: subname='(module_MEDIATOR:FldPtr_SameCheck)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    FldPtr_SameCheck = .false.
    if (lbound(fldptr2,2) /= lbound(fldptr1,2) .or. &
        lbound(fldptr2,1) /= lbound(fldptr1,1) .or. &
        ubound(fldptr2,2) /= ubound(fldptr1,2) .or. &
        ubound(fldptr2,1) /= ubound(fldptr1,1)) then
      call ESMF_LogWrite(trim(subname)//": ERROR in data size "//trim(cstring), ESMF_LOGMSG_ERROR, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      write(msgString,*) trim(subname)//': fldptr2 ',lbound(fldptr2),ubound(fldptr2)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
      write(msgString,*) trim(subname)//': fldptr1 ',lbound(fldptr1),ubound(fldptr1)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    else
      FldPtr_SameCheck = .true.
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end function FldPtr_SameCheck

  !-----------------------------------------------------------------------------

  subroutine FldGrid_Print(field, string, rc)

    type(ESMF_Field), intent(in)  :: field
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    type(ESMF_Grid)     :: grid
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter  :: subname='(module_MEDIATOR:FldGrid_Print)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    call Grid_Print(grid, string, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_FieldGet(field,farrayPtr=dataptr,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    write (msgString,*) trim(subname)//":"//trim(string)//": dataptr bounds dim=1 ",lbound(dataptr,1),ubound(dataptr,1)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    write (msgString,*) trim(subname)//":"//trim(string)//": dataptr bounds dim=2 ",lbound(dataptr,2),ubound(dataptr,2)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine FldGrid_Print

  !-----------------------------------------------------------------------------

  subroutine Grid_Print(grid, string, rc)

    type(ESMF_Grid) , intent(in)  :: grid
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    type(ESMF_Distgrid) :: distgrid  
    character(ESMF_MAXSTR)      :: transferAction
    integer                     :: localDeCount
    integer                     :: dimCount, tileCount
    integer, allocatable        :: minIndexPTile(:,:), maxIndexPTile(:,:)
    character(len=*),parameter  :: subname='(module_MEDIATOR:Grid_Print)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! access localDeCount to show this is a real Grid
    call ESMF_GridGet(grid, localDeCount=localDeCount, distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
   
    write (msgString,*) trim(subname)//":"//trim(string)//": localDeCount=", localDeCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    
    ! get dimCount and tileCount
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    
    write (msgString,*) trim(subname)//":"//trim(string)//": dimCount=", dimCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    write (msgString,*) trim(subname)//":"//trim(string)//": tileCount=", tileCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
    allocate(minIndexPTile(dimCount, tileCount), &
             maxIndexPTile(dimCount, tileCount))
    
    ! get minIndex and maxIndex arrays
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
       maxIndexPTile=maxIndexPTile, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
      
    write (msgString,*) trim(subname)//":"//trim(string)//": minIndexPTile=", minIndexPTile
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    write (msgString,*) trim(subname)//":"//trim(string)//": maxIndexPTile=", maxIndexPTile
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    deallocate(minIndexPTile, maxIndexPTile)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Grid_Print

!-----------------------------------------------------------------------------
  subroutine ClockTimePrint(clock,string,rc)

    type(ESMF_Clock),intent(in) :: clock
    character(len=*),intent(in),optional :: string
    integer, intent(out) :: rc

    type(ESMF_Time)      :: time
    type(ESMF_TimeInterval) :: timeStep
    character(len=64)    :: timestr
    character(len=512)   :: lstring
    character(len=*),parameter :: subname='(module_MEDIATOR:ClockTimePrint)'

    rc = ESMF_SUCCESS

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (present(string)) then
      lstring = trim(subname)//":"//trim(string)
    else
      lstring = trim(subname)
    endif

    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_TimeGet(time,timestring=timestr,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_LogWrite(trim(lstring)//": currtime = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_ClockGet(clock,starttime=time,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_TimeGet(time,timestring=timestr,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_LogWrite(trim(lstring)//": startime = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_ClockGet(clock,stoptime=time,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_TimeGet(time,timestring=timestr,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_LogWrite(trim(lstring)//": stoptime = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_ClockGet(clock,timestep=timestep,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_TimeIntervalGet(timestep,timestring=timestr,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_LogWrite(trim(lstring)//": timestep = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)

    if (dbug_flag > 5) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine ClockTimePrint

  !-----------------------------------------------------------------------------

  subroutine Grid_Write(grid, string, rc)
    type(ESMF_Grid) ,intent(in)  :: grid
    character(len=*),intent(in)  :: string
    integer         ,intent(out) :: rc
  
    ! local 
    type(ESMF_Array)            :: array
    character(len=*),parameter  :: subname='(module_MEDIATOR:Grid_Write)'

    ! -- centers --

    rc = ESMF_SUCCESS
    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    ! -- centers --

    call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      call ESMF_GridGetCoord(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call ESMF_ArraySet(array, name="lon_center", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call Array_diagnose(array,trim(string)//"_grid_coord1", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayWrite(array, trim(string)//"_grid_coord1.nc", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call ESMF_GridGetCoord(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call ESMF_ArraySet(array, name="lat_center", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call Array_diagnose(array,trim(string)//"_grid_coord2", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayWrite(array, trim(string)//"_grid_coord2.nc", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    endif

    ! -- corners --

    call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      call ESMF_GridGetCoord(grid, coordDim=1, staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
      if (.not. ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) then
        call ESMF_ArraySet(array, name="lon_corner", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call Array_diagnose(array,trim(string)//"_grid_corner1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call ESMF_ArrayWrite(array, trim(string)//"_grid_corner1.nc", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif
      call ESMF_GridGetCoord(grid, coordDim=2, staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
      if (.not. ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) then
        call ESMF_ArraySet(array, name="lat_corner", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call Array_diagnose(array,trim(string)//"_grid_corner2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call ESMF_ArrayWrite(array, trim(string)//"_grid_corner2.nc", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif
    endif


    ! -- mask --

    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_MASK, staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call ESMF_ArraySet(array, name="mask", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call Array_diagnose(array,trim(string)//"_grid_mask", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayWrite(array, trim(string)//"_grid_mask.nc", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    endif

    ! -- area --

    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, itemflag=ESMF_GRIDITEM_AREA, array=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call ESMF_ArraySet(array, name="area", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call Array_diagnose(array,trim(string)//"_grid_area", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayWrite(array, trim(string)//"_grid_area.nc", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Grid_Write

  !-----------------------------------------------------------------------------

  subroutine fld_list_add(fldlist, stdname, transferOffer, mapping)
    ! ----------------------------------------------
    ! Set up a list of field information
    ! ----------------------------------------------
    type(fld_list_type), intent(inout)  :: fldlist
    character(len=*),    intent(in)     :: stdname
    character(len=*),    intent(in)     :: transferOffer
    character(len=*),    intent(in), optional  :: mapping

    ! local variables
    integer :: cnum    ! current size of array
    integer :: nnum    ! new size of array
    integer :: rc
    character(len=256), pointer :: tmpString(:)
    character(len=*), parameter :: subname='(module_MEDIATOR:fld_list_add)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    ! make sure that stdname is in the NUOPC Field Dictionary 
    call NUOPC_FieldDictionaryGetEntry(stdname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=trim(subname)//&
      ": invalid stdname: "//trim(stdname), &
      line=__LINE__, file=__FILE__)) return  ! bail out
    
    ! potentially extend the existing lists

    if (fldlist%num < 0) then
       nnum = 10
       fldlist%num = 0
       allocate(fldlist%stdname(nnum))
       allocate(fldlist%shortname(nnum))
       allocate(fldlist%transferOffer(nnum))
       allocate(fldlist%mapping(nnum))
    endif

    cnum = size(fldlist%stdname)
    if (fldlist%num > cnum) then
      call ESMF_LogWrite(trim(subname)//&
      ": ERROR in num for fld "//trim(stdname), ESMF_LOGMSG_ERROR)
      return
    endif
    if (fldlist%num == cnum) then
      nnum = cnum + 10
      allocate(tmpString(cnum))
      tmpString(1:cnum) = fldlist%stdname(1:cnum)
      deallocate(fldlist%stdname)
      allocate(fldlist%stdname(nnum))
      fldlist%stdname(1:cnum) = tmpString(1:cnum)
      tmpString(1:cnum) = fldlist%shortname(1:cnum)
      deallocate(fldlist%shortname)
      allocate(fldlist%shortname(nnum))
      fldlist%shortname(1:cnum) = tmpString(1:cnum)
      tmpString(1:cnum) = fldlist%transferOffer(1:cnum)
      deallocate(fldlist%transferOffer)
      allocate(fldlist%transferOffer(nnum))
      fldlist%transferOffer(1:cnum) = tmpString(1:cnum)
      tmpString(1:cnum) = fldlist%mapping(1:cnum)
      deallocate(fldlist%mapping)
      allocate(fldlist%mapping(nnum))
      fldlist%mapping(1:cnum) = tmpString(1:cnum)
      deallocate(tmpString)
    endif
    
    ! fill in the new entry

    fldlist%num = fldlist%num + 1
    fldlist%stdname       (fldlist%num) = trim(stdname)
    fldlist%shortname     (fldlist%num) = trim(stdname)
    fldlist%transferOffer (fldlist%num) = trim(transferOffer)
    if (present(mapping)) then
       if (trim(mapping) /= "conservefrac" .and. trim(mapping) /= 'bilinear' .and. &
           trim(mapping) /= 'conservedst'  .and. &
           trim(mapping) /= 'patch'    .and. trim(mapping) /= 'copy') then
          call ESMF_LogWrite(trim(subname)//": ERROR mapping not allowed "//trim(mapping), ESMF_LOGMSG_ERROR, rc=rc)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
       endif
       fldlist%mapping    (fldlist%num) = trim(mapping)
    else
       fldlist%mapping    (fldlist%num) = 'undefined'
    endif

    if (dbug_flag > 10) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fld_list_add

  !-----------------------------------------------------------------------------

  function NEMS_DistGridMatch(distGrid1, distGrid2, rc)

    ! Arguments
    type(ESMF_DistGrid), intent(in)     :: distGrid1
    type(ESMF_DistGrid), intent(in)     :: distGrid2
    integer, intent(out), optional  :: rc

    ! Return Value
    logical                         :: NEMS_DistGridMatch

    ! Local Variables
    integer                         :: dimCount1, dimCount2
    integer                         :: tileCount1, tileCount2
    integer, allocatable            :: minIndexPTile1(:,:), minIndexPTile2(:,:)
    integer, allocatable            :: maxIndexPTile1(:,:), maxIndexPTile2(:,:)
    integer, allocatable            :: elementCountPTile1(:), elementCountPTile2(:)
    CHARACTER(LEN=*), PARAMETER :: SUBNAME='(module_MEDIATOR:NEMS_DistGridMatch)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if(present(rc)) rc = ESMF_SUCCESS
    NEMS_DistGridMatch = .true.

    call ESMF_DistGridGet(distGrid1, &
      dimCount=dimCount1, tileCount=tileCount1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,file=__FILE__)) return  ! bail out

    call ESMF_DistGridGet(distGrid2, &
      dimCount=dimCount2, tileCount=tileCount2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,file=__FILE__)) return  ! bail out

    if ( dimCount1 /= dimCount2) then
      NEMS_DistGridMatch = .false.
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Grid dimCount MISMATCH ", &
          ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    if ( tileCount1 /= tileCount2) then
      NEMS_DistGridMatch = .false.
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Grid tileCount MISMATCH ", &
          ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    allocate(elementCountPTile1(tileCount1))
    allocate(elementCountPTile2(tileCount2))
    allocate(minIndexPTile1(dimCount1,tileCount1))
    allocate(minIndexPTile2(dimCount2,tileCount2))
    allocate(maxIndexPTile1(dimCount1,tileCount1))
    allocate(maxIndexPTile2(dimCount2,tileCount2))

    call ESMF_DistGridGet(distGrid1, &
      elementCountPTile=elementCountPTile1, &
      minIndexPTile=minIndexPTile1, &
      maxIndexPTile=maxIndexPTile1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,file=__FILE__)) return  ! bail out

    call ESMF_DistGridGet(distGrid2, &
      elementCountPTile=elementCountPTile2, &
      minIndexPTile=minIndexPTile2, &
      maxIndexPTile=maxIndexPTile2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,file=__FILE__)) return  ! bail out

    if ( ANY((elementCountPTile1 - elementCountPTile2) .NE. 0) ) then
      NEMS_DistGridMatch = .false.
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Grid elementCountPTile MISMATCH ", &
          ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    if ( ANY((minIndexPTile1 - minIndexPTile2) .NE. 0) ) then
      NEMS_DistGridMatch = .false.
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Grid minIndexPTile MISMATCH ", &
          ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    if ( ANY((maxIndexPTile1 - maxIndexPTile2) .NE. 0) ) then
      NEMS_DistGridMatch = .false.
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Grid maxIndexPTile MISMATCH ", &
          ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    deallocate(elementCountPTile1)
    deallocate(elementCountPTile2)
    deallocate(minIndexPTile1)
    deallocate(minIndexPTile2)
    deallocate(maxIndexPTile1)
    deallocate(maxIndexPTile2)

    ! TODO: Optionally Check Coordinates


    if (dbug_flag > 10) then
      call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end function NEMS_DistGridMatch

  !-----------------------------------------------------------------------------

  subroutine NEMS_GridCopyCoord(gridcomp, gridSrc, gridDst, staggerloc, tolerance, compare, invert, rc)

    ! Arguments
    type(ESMF_GridComp),intent(in)      :: gridcomp
    type(ESMF_Grid), intent(in)         :: gridSrc
    type(ESMF_Grid), intent(in)         :: gridDst
    type(ESMF_StaggerLoc),intent(in)    :: staggerloc(:)
    real, intent(in), optional          :: tolerance
    logical, intent(in), optional       :: compare
    integer, intent(in), optional       :: invert(:)
    integer, intent(out),optional       :: rc

    ! Local Variables
    real                                :: l_tolerance
    logical                             :: l_compare
    integer, allocatable                :: l_invert(:)
    integer                             :: i
    type(ESMF_VM)                       :: vm
    type(ESMF_DistGrid)                 :: distGridSrc, distGridDst
    type(ESMF_Array)                    :: coordArraySrc, coordArrayDst
    integer(ESMF_KIND_I4),allocatable   :: factorList(:)
    integer, allocatable                :: factorIndexList(:,:)
    type(ESMF_RouteHandle)          :: routehandle
    integer                         :: dimCountSrc, dimCountDst
    integer                         :: deCountDst
    integer, allocatable            :: elementCountPDeDst(:)
    integer(ESMF_KIND_I8)           :: sumElementCountPDeDst
    type(ESMF_TypeKind_Flag)        :: coordTypeKindSrc, coordTypeKindDst
    type(ESMF_CoordSys_Flag)        :: coordSysSrc, coordSysDst
    logical                         :: isPresentSrc, isPresentDst
    integer                         :: dimIndex, staggerlocIndex
    integer                         :: localPet
    character(len=10)               :: numString
    CHARACTER(LEN=*), PARAMETER :: SUBNAME='(module_MEDIATOR:NEMS_GridCopyCoord)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    l_tolerance = 0.0
    if (present(tolerance)) l_tolerance = tolerance
    l_compare = .FALSE.
    if (present(compare)) l_compare = compare
    if (present(invert)) then
      allocate(l_invert(size(invert)))
      l_invert = invert
    else
      allocate(l_invert(1))
      l_invert = -1
    endif

    call ESMF_GridGet(gridSrc, distGrid=distGridSrc, &
      dimCount=dimCountSrc, coordTypeKind=coordTypeKindSrc, &
      coordSys=coordSysSrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,file=__FILE__)) return  ! bail out

    call ESMF_GridGet(gridDst, distGrid=distGridDst, &
      dimCount=dimCountDst, coordTypeKind=coordTypeKindDst, &
      coordSys=coordSysDst, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,file=__FILE__)) return  ! bail out

    if (.NOT. NEMS_DistGridMatch(distGrid1=distGridSrc, distGrid2=distGridDst, rc=rc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=SUBNAME//": Unable to redistribute coordinates. DistGrids do not match.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    if ( dimCountSrc /= dimCountDst) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=SUBNAME//": DIMCOUNT MISMATCH", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    if ( coordTypeKindSrc /= coordTypeKindDst) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=SUBNAME//": COORDTYPEKIND MISMATCH", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    if ( coordSysSrc /= coordSysDst) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=SUBNAME//": COORDSYS MISMATCH", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    do dimIndex=1, dimCountDst
    do staggerlocIndex=1, size(staggerloc)
      call ESMF_GridGetCoord(gridSrc, staggerloc=staggerloc(staggerlocIndex), &
        isPresent=isPresentSrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
      if(isPresentSrc) then
        call ESMF_GridGetCoord(gridSrc, coordDim=dimIndex, &
          staggerloc=staggerloc(staggerlocIndex), &
          array=coordArraySrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
        call ESMF_GridGetCoord(gridDst, &
          staggerloc=staggerloc(staggerlocIndex), &
          isPresent=isPresentDst, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
        if(.NOT.isPresentDst) then
          call ESMF_GridAddCoord(gridDst, &
            staggerLoc=staggerloc(staggerlocIndex), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,file=__FILE__)) return  ! bail out
        else
          if(l_compare .EQV. .TRUE.) then
            ! TODO: Compare existing coordinates
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg=SUBNAME//": Cannot compare existing coordinates.", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
              return  ! bail out
          end if
        endif
        call ESMF_GridGetCoord(gridDst, coordDim=dimIndex, &
          staggerloc=staggerloc(staggerlocIndex), &
          array=coordArrayDst, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
        call ESMF_ArrayGet(coordArraySrc, distGrid=distGridSrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
        call ESMF_ArrayGet(coordArrayDst, distGrid=distGridDst, &
          dimCount=dimCountDst, deCount=deCountDst, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
        if (.NOT. NEMS_DistGridMatch(distGrid1=distGridSrc, distGrid2=distGridDst, rc=rc)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=SUBNAME//": Unable to redistribute coordinates. DistGrids do not match.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
          return  ! bail out
        endif

        if ( ANY( l_invert == dimIndex )) then
          call ESMF_GridCompGet(gridcomp, vm=vm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,file=__FILE__)) return  ! bail out

          call ESMF_VMGet(vm, localPet=localPet, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,file=__FILE__)) return  ! bail out            

          if (localPet == 0) then
            call ESMF_DistGridGet(distGridDst, deCount=deCountDst, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,file=__FILE__)) return  ! bail out

            allocate(elementCountPDeDst(deCountDst))
            call ESMF_DistGridGet(distGridDst, &
              elementCountPDe=elementCountPDeDst, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,file=__FILE__)) return  ! bail out

            sumElementCountPDeDst = SUM(elementCountPDeDst)
            if (dbug_flag >= 2) then
              write (numString, "(I10)") sumElementCountPDeDst
              call ESMF_LogWrite(SUBNAME//": sumElementCountPDeDst: "//trim(adjustl(numString)), ESMF_LOGMSG_INFO, rc=dbrc)
            endif

            allocate(factorList(sumElementCountPDeDst))
            allocate(factorIndexList(2,sumElementCountPDeDst))

            factorList(:) = 1
            factorIndexList(1,:) = (/(i, i=1, sumElementCountPDeDst, 1)/)
            factorIndexList(2,:) = (/(i, i=sumElementCountPDeDst, 1, -1)/)

            if (dbug_flag >= 2) then
              write (numString, "(I10)") factorIndexList(1,1)
              write (msgString, "(A)") "Src=>Dst: "//trim(adjustl(numString))//"=>"
              write (numString, "(I10)") factorIndexList(2,1)
              write (msgString, "(A)") trim(msgString)//trim(adjustl(numString))
              write (numString, "(I10)") factorIndexList(1,sumElementCountPDeDst) 
              write (msgString, "(A)") trim(msgString)//" "//trim(adjustl(numString))//"=>"
              write (numString, "(I10)") factorIndexList(2,sumElementCountPDEDst)
     	      write (msgString, "(A)") trim(msgString)//trim(adjustl(numString))
              call ESMF_LogWrite(SUBNAME//": Invert Mapping: "//msgString, ESMF_LOGMSG_INFO, rc=dbrc)
            endif

            call ESMF_ArraySMMStore(srcArray=coordArraySrc, dstArray=coordArrayDst, &
              routehandle=routehandle, factorList=factorList, &
              factorIndexList=factorIndexList, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,file=__FILE__)) return  ! bail out
            deallocate(elementCountPDeDst)
            deallocate(factorList)
            deallocate(factorIndexList)
          else
            call ESMF_ArraySMMStore(srcArray=coordArraySrc, dstArray=coordArrayDst, &
              routehandle=routehandle, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,file=__FILE__)) return  ! bail out
          endif

          call ESMF_ArraySMM(srcArray=coordArraySrc, dstArray=coordArrayDst, &
            routehandle=routehandle, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,file=__FILE__)) return  ! bail out
          call ESMF_ArraySMMRelease(routehandle=routehandle, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,file=__FILE__)) return  ! bail out

        else
          call ESMF_ArrayRedistStore(coordArraySrc, coordArrayDst, routehandle=routehandle, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,file=__FILE__)) return  ! bail out
          call ESMF_ArrayRedist(coordArraySrc, coordArrayDst, routehandle=routehandle, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,file=__FILE__)) return  ! bail out
          call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,file=__FILE__)) return  ! bail out
        endif
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=SUBNAME//": SOURCE GRID MISSING STAGGER LOCATION", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif
    enddo
    enddo

    deallocate(l_invert)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine NEMS_GridCopyCoord

  !-----------------------------------------------------------------------------

  subroutine NEMS_GridCopyItem(gridcomp, gridSrc, gridDst, item, tolerance, compare, invert, rc)

    ! Arguments
    type(ESMF_GridComp),intent(in)      :: gridcomp
    type(ESMF_Grid), intent(in)         :: gridSrc
    type(ESMF_Grid), intent(in)         :: gridDst
    type(ESMF_GridItem_Flag),intent(in) :: item(:)
    real, intent(in), optional          :: tolerance
    logical, intent(in), optional       :: compare
    integer, intent(in), optional       :: invert(:)
    integer, intent(out),optional       :: rc

    ! Local Variables
    real                                :: l_tolerance
    logical                             :: l_compare
    integer, allocatable                :: l_invert(:)
    type(ESMF_StaggerLoc)               :: l_staggerloc
    type(ESMF_DistGrid)                 :: distGridSrc, distGridDst
    type(ESMF_Array)                    :: itemArraySrc, itemArrayDst
    type(ESMF_RouteHandle)          :: routehandle
    integer                         :: dimCountSrc, dimCountDst
    type(ESMF_TypeKind_Flag)        :: coordTypeKindSrc, coordTypeKindDst
    type(ESMF_CoordSys_Flag)        :: coordSysSrc, coordSysDst
    logical                         :: isPresentSrc, isPresentDst
    integer                         :: itemIndex
    integer                         :: localPet
    character(len=10)               :: numString
    CHARACTER(LEN=*), PARAMETER :: SUBNAME='(module_MEDIATOR:NEMS_GridCopyItem)'

    if (dbug_flag > 10) then
      call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    l_tolerance = 0.0
    if (present(tolerance)) l_tolerance = tolerance
    l_compare = .FALSE.
    if (present(compare)) l_compare = compare
    if (present(invert)) then
      allocate(l_invert(size(invert)))
      l_invert = invert
    else
      allocate(l_invert(1))
      l_invert = -1
    endif
    l_staggerloc = ESMF_STAGGERLOC_CENTER

    call ESMF_GridGet(gridSrc, distGrid=distGridSrc, &
      dimCount=dimCountSrc, coordTypeKind=coordTypeKindSrc, &
      coordSys=coordSysSrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,file=__FILE__)) return  ! bail out

    call ESMF_GridGet(gridDst, distGrid=distGridDst, &
      dimCount=dimCountDst, coordTypeKind=coordTypeKindDst, &
      coordSys=coordSysDst, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,file=__FILE__)) return  ! bail out

    if (.NOT. NEMS_DistGridMatch(distGrid1=distGridSrc, distGrid2=distGridDst, rc=rc)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=SUBNAME//": Unable to redistribute coordinates. DistGrids do not match.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    if ( dimCountSrc /= dimCountDst) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=SUBNAME//": DIMCOUNT MISMATCH", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    if ( coordTypeKindSrc /= coordTypeKindDst) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=SUBNAME//": COORDTYPEKIND MISMATCH", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    if ( coordSysSrc /= coordSysDst) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=SUBNAME//": COORDSYS MISMATCH", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    do itemIndex=1, size(item)
      call ESMF_GridGetItem(gridSrc, itemflag=item(itemIndex), &
        staggerloc=l_staggerloc, isPresent=isPresentSrc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
      if(isPresentSrc) then
        call ESMF_GridGetItem(gridSrc, itemflag=item(itemIndex), &
          staggerloc=l_staggerloc, array=itemArraySrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
        call ESMF_GridGetItem(gridDst, itemflag=item(itemIndex), &
          staggerloc=l_staggerloc, isPresent=isPresentDst, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
        if(.NOT.isPresentDst) then
          call ESMF_GridAddItem(gridDst, itemflag=item(itemIndex), &
            staggerLoc=l_staggerloc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
        else
          if(l_compare .EQV. .TRUE.) then
            ! TODO: Compare existing coordinates
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg=SUBNAME//": Cannot compare existing coordinates.", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
              return  ! bail out
          end if
        endif
        call ESMF_GridGetItem(gridDst, itemflag=item(itemIndex), &
          staggerloc=l_staggerloc, array=itemArrayDst, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
        call ESMF_ArrayGet(itemArraySrc, distGrid=distGridSrc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
        call ESMF_ArrayGet(itemArrayDst, distGrid=distGridDst, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,file=__FILE__)) return  ! bail out
        if (.NOT. NEMS_DistGridMatch(distGrid1=distGridSrc, distGrid2=distGridDst, rc=rc)) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg=SUBNAME//": Unable to redistribute coordinates. DistGrids do not match.", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
            return  ! bail out
        endif

        if ( ANY( l_invert > 0 )) then
          ! TODO: Invert Item
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg=SUBNAME//": Cannot invert item.", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
              return  ! bail out
        else
          call ESMF_ArrayRedistStore(itemArraySrc, itemArrayDst, routehandle=routehandle, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,file=__FILE__)) return  ! bail out
          call ESMF_ArrayRedist(itemArraySrc, itemArrayDst, routehandle=routehandle, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,file=__FILE__)) return  ! bail out
          call ESMF_ArrayRedistRelease(routehandle=routehandle, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,file=__FILE__)) return  ! bail out
        endif
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=SUBNAME//": SOURCE GRID MISSING ITEM", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      endif
    enddo

    deallocate(l_invert)

    if (dbug_flag > 10) then
      call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine NEMS_GridCopyItem

  !-----------------------------------------------------------------------------

  subroutine NUOPCplus_UpdateTimestampS(state, time, rc)
    type(ESMF_State)      :: state
    type(ESMF_Time)       :: time
    integer, intent(out)  :: rc

    ! local variables
    integer               :: i
    type(ESMF_Field),       pointer       :: fieldList(:)

    rc = ESMF_SUCCESS

    nullify(fieldList)
    call NUOPC_GetStateMemberLists(state, fieldList=fieldList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do i=1, size(fieldList)
      call NUOPCplus_UpdateTimestamp(fieldList(i), time, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    if (associated(fieldList)) deallocate(fieldList)
    
  end subroutine NUOPCplus_UpdateTimestampS

  !-----------------------------------------------------------------------------

  subroutine NUOPCplus_UpdateTimestampF(field, time, rc)
    type(ESMF_Field)      :: field
    type(ESMF_Time)       :: time
    integer, intent(out)  :: rc

    ! local variables
    integer               :: yy, mm, dd, h, m, s, ms, us, ns

    rc = ESMF_SUCCESS
    
    call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, ms=ms, us=us, &
      ns=ns, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeSet(field, &
      name="TimeStamp", valueList=(/yy,mm,dd,h,m,s,ms,us,ns/), &
      convention="NUOPC", purpose="Instance", &
      attnestflag=ESMF_ATTNEST_ON, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine NUOPCplus_UpdateTimestampF

  !-----------------------------------------------------------------------------

end module
