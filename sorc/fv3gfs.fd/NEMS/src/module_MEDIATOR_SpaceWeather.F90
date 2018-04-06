#include "./ESMFVersionDefine.h"

module module_MEDSpaceWeather

  use ESMF
  use ESMF_IO_NCPutGetMod

  use NUOPC
  use NUOPC_Mediator, &
    mediator_routine_SS             => SetServices, &
    mediator_routine_Run            => routine_Run, &
    mediator_label_DataInitialize   => label_DataInitialize, &
    mediator_label_Advance          => label_Advance, &
    mediator_label_CheckImport      => label_CheckImport, &
    mediator_label_TimestampExport  => label_TimestampExport, &
    mediator_label_SetRunClock      => label_SetRunClock, &
    mediator_label_Finalize         => label_Finalize

#define ESMF_NETCDF
#ifdef ESMF_NETCDF
  use netcdf
#endif

  implicit none
  
  private

  include "mpif.h"

!#define USE_CART3D_COORDSYS
!#define OUT_WEIGHT

  integer, parameter :: MAXNAMELEN = 128

  ! private internal state to keep instance data
  type InternalStateStruct
    integer :: wamdims(3)
    real(ESMF_KIND_R8), pointer :: wamhgt(:)
    integer :: myrows, startlevel, totallevels
    integer :: wamtotalnodes, localnodes
    type(ESMF_Mesh):: wam2dMesh, wamMesh, ipeMesh
    type(ESMF_RouteHandle) :: routehandle
    integer :: PetNo, PetCnt
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type

  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(mediator, mediator_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Provide InitializeP0 to switch from default IPDv00 to IPDv03
    call ESMF_GridCompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p4"/), userRoutine=InitializeP4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p5"/), userRoutine=InitializeP5, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing method(s)

     call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_DataInitialize, &
       specRoutine=DataInitialize, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_Advance, &
      specRoutine=MediatorAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_Finalize, &
      specRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(mediator, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: mediator
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    rc = ESMF_SUCCESS

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(mediator, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  subroutine InitializeAdvertise(mediator, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    ! exportable fields: WAM export fields
    call NUOPC_Advertise(importState, StandardNames=(/ &
      "northward_wind_neutral         ", &
      "eastward_wind_neutral          ", &
      "temp_neutral                   ", &
      "height                         "  &
      /), transferOfferGeomObject = "will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable fields: IPE import fields
    call NUOPC_Advertise(exportState, StandardNames=(/ &
      "northward_wind_neutral         ", &
      "eastward_wind_neutral          ", &
!      "upward_wind_neutral            ", &
      "temp_neutral                   " &
!      "O_Density                      ", &
!      "O2_Density                     ", &
!      "N2_Density                     " &
      /), TransferOfferGeomObject="cannot provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeRealize(mediator, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    type(ESMF_Mesh):: wam2dMesh

    rc = ESMF_SUCCESS

    ! calling into Peggy's code
    call initGrids(mediator, wam2dMesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! create fields
    call realizeConnectedFields(importState, wam2dMesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  contains

    subroutine realizeConnectedFields(state, mesh, rc)
      ! TODO: this method may move into the NUOPC_ utility layer
      type(ESMF_State)                :: state
      type(ESMF_Mesh)                 :: mesh
      integer, intent(out), optional  :: rc

      ! local variables
      character(len=80), allocatable  :: fieldNameList(:)
      integer                         :: i, itemCount, k
      type(ESMF_ArraySpec)            :: arrayspec
      type(ESMF_Field)                :: field
      integer                         :: levels

      if (present(rc)) rc = ESMF_SUCCESS

      levels = 150 ! hardcode level, probably should get it from internal state

      call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      allocate(fieldNameList(itemCount))
      call ESMF_StateGet(state, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      k=1 ! initialize
      do i=1, itemCount
          ! create a Field with one undistributed dimension
          call ESMF_ArraySpecSet(arrayspec,2,ESMF_TYPEKIND_R8, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          field = ESMF_FieldCreate(mesh, arrayspec, &
	   ungriddedLBound=(/1/), ungriddedUBound=(/levels/), &
	   name=fieldNameList(i), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ! realize the connected Field using the just created Field
          call NUOPC_Realize(state, field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
      enddo

    end subroutine realizeConnectedFields

  end subroutine initializeRealize
    
  subroutine InitializeP4(mediator, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    type(ESMF_VM)        :: vm
    type(ESMF_Mesh)      :: ipemesh, medmesh
    type(ESMF_Field)     :: field
    character(len=80), allocatable  :: fieldNameList(:)
    integer              :: i, itemCount, k
    type(ESMF_DistGrid)  :: ipedistgrid, meddistgrid
    integer              :: minIndices(1,1), maxIndices(1,1)
    integer              :: decount, PetNo, PetCnt
    type(InternalState)  :: is
    logical              :: freeflag

    rc = ESMF_SUCCESS

  ! query component for its internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    PetNo = is%wrap%PetNo
    PetCnt = is%wrap%PetCnt

    ! Get the IPE field from the IPE module, get the elemDistgrid and redistribute in over
    ! the number of processors used by the Mediator
    
      call ESMF_StateGet(exportState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      allocate(fieldNameList(itemCount))
      call ESMF_StateGet(exportState, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! Get the mesh from the first itme
      call ESMF_StateGet(exportState, field=field, itemName=fieldNameList(1), rc=rc)   
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_FieldGet(field, mesh=ipemesh, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_MeshGet(ipemesh, nodalDistgrid=ipedistgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_DistGridGet(ipedistgrid, deCount=decount, &
      	   minIndexPTile=minIndices, maxIndexPTile=maxIndices, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! If the original distribution (decount) use the same number of 
      ! PETs (PetCnt), not need to redistribute.  Otherwise, redistribute
      ! the elementDistgrid to use PetCnt processors
      if (PetCnt /= decount) then
         ! create a new distgrid evenly distribute the nodes over all the PEs.
         meddistgrid = ESMF_DistGridCreate((/minIndices(1,1)/), (/maxIndices(1,1)/), &
	 	       rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            	line=__LINE__, &
            	file=__FILE__)) &
            	return  ! bail out
         medmesh = ESMF_MeshCreate(meddistgrid, meddistgrid, &
             	  rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            	line=__LINE__, &
        	file=__FILE__)) &
        	return  ! bail out
         call ESMF_MeshDestroy(ipemesh, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            	line=__LINE__, &
        	file=__FILE__)) &
        	return  ! bail out
        ! replace the field with new mesh     
        k=1 ! initialize
        do i=1, itemCount
          ! create a Field with one undistributed dimension
          call ESMF_StateGet(exportState, field=field, itemName=fieldNameList(i), rc=rc)   
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
        	file=__FILE__)) &
		return  ! bail out
            		
          call ESMF_FieldEmptySet(field, medmesh, meshloc=ESMF_MESHLOC_NODE,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
        	file=__FILE__)) &
		return  ! bail out
        enddo
     endif

  end subroutine initializeP4

  !----------------------------------------------------------------------------

  subroutine InitializeP5(mediator, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_VM)        :: vm
    character(len=80), allocatable  :: fieldNameList(:)
    integer              :: i, itemCount, k
    type(ESMF_Field)     :: field, wamfield, ipefield
    type(ESMF_ArraySpec) :: arrayspec
    real(ESMF_KIND_R8)   :: starttime, endtime, timesend(1), timereport(1)
    type(InternalState)  :: is

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  ! query component for its internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_StateGet(exportState, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    allocate(fieldNameList(itemCount))
    call ESMF_StateGet(exportState, itemNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    k=1 ! initialize
    do i=1, itemCount
       ! create a Field with one undistributed dimension
       call ESMF_StateGet(exportState, field=field, itemName=fieldNameList(i), rc=rc)   
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
        	file=__FILE__)) &
		return  ! bail out
            		
       call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
        	file=__FILE__)) &
		return  ! bail out

      if (i==1) then
         call ESMF_FieldGet(field, mesh=is%wrap%ipeMesh, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
      endif
    enddo

      ! Now we have both wammesh and ipemesh, call ESMF_FieldRegridStore() to create
      ! a routehandle
      ! Create src and dst fields and run RegridStore()
      call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            	line=__LINE__, &
        	file=__FILE__)) &
        	return  ! bail out

      wamField = ESMF_FieldCreate(is%wrap%wamMesh, arrayspec, meshloc=ESMF_MESHLOC_NODE,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            	line=__LINE__, &
        	file=__FILE__)) &
        	return  ! bail out
      ipeField = ESMF_FieldCreate(is%wrap%ipeMesh, arrayspec, meshloc=ESMF_MESHLOC_NODE,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            	line=__LINE__, &
        	file=__FILE__)) &
        	return  ! bail out

#if 1
      call ESMF_VMBarrier(vm)
      call ESMF_VMWTime(starttime)
#endif
      call ESMF_FieldRegridStore(wamField, ipeField, &
       	 unmappedaction =ESMF_UNMAPPEDACTION_IGNORE, &
	 regridmethod = ESMF_REGRIDMETHOD_BILINEAR, &
	 polemethod = ESMF_POLEMETHOD_NONE, &
         lineType = ESMF_LINETYPE_GREAT_CIRCLE, &
	 routehandle = is%wrap%routehandle, &
	 rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            	line=__LINE__, &
        	file=__FILE__)) &
        	return  ! bail out

#if 1
      call ESMF_VMWtime(endtime)
      timesend(1)=endtime-starttime
      call ESMF_VMReduce(vm, sendData=timesend, recvData=timereport, count=1, &
    	 reduceflag=ESMF_REDUCE_MAX, rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            	line=__LINE__, &
        	file=__FILE__)) &
        	return  ! bail out
      if (is%wrap%PetNo==0) then
         print *, 'Time to do RegridStore WAM->IPE is ', timereport(1)*1000, 'msec'
      endif
#endif
      call ESMF_FieldDestroy(wamfield)
      call ESMF_FieldDestroy(ipefield)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(mediator, rc)
     type(ESMF_GridComp)  :: mediator
     integer, intent(out) :: rc

     ! indicate that data initialization is complete (breaking out of init-loop)
     call NUOPC_CompAttributeSet(mediator, &
       name="InitializeDataComplete", value="true", rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

   end subroutine

  !-----------------------------------------------------------------------------

  subroutine MediatorAdvance(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(mediator, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the 
    ! mediators Clock.
    
    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.
    
    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields and update the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="-------->MED Advance() mediating for: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! calling into Peggy's code
    call RunRegrid(mediator, importstate, exportstate, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="----------------> model time step to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
     
  end subroutine

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Peggy's routines below....
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine initGrids(model, wam2dMesh, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_MESH) :: wam2dmesh
    integer, intent(out) :: rc
    

  type(ESMF_VM) :: vm
  type(InternalState)     :: is
  type(ESMF_MESH) :: wammesh
  real(ESMF_KIND_R8), pointer :: wamlon(:,:), wamlat(:,:), wamhgt(:)
  real(ESMF_KIND_R8), pointer :: ipelon(:,:), ipelat(:,:), ipehgt(:), ipedata(:)
  real(ESMF_KIND_R8), pointer :: hgtbuf(:,:,:), varbuf(:,:,:,:)
  real(ESMF_KIND_R8), pointer :: databuf(:)
  integer(ESMF_KIND_I4), pointer :: maxlevs(:)
  integer(ESMF_KIND_I4), pointer :: numPerRow(:), shuffleOrder(:)
  integer :: nc1, nc2
  integer :: varid
  integer :: ndims, dimids(3)
  integer :: wamdims(3), ipedims(3)
  integer :: PetNo, PetCnt
  integer(ESMF_KIND_I4), pointer :: elementIds(:), elementTypes(:), elementConn(:)
  integer(ESMF_KIND_I4), pointer :: nodeIds(:), nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer(ESMF_KIND_I4), pointer :: southind(:), northind(:), totallats(:), gap(:)
  integer :: minheight, maxheight, halo, neighbor, remind
  integer(ESMF_KIND_I4), pointer :: totalheight(:)
  real(ESMF_KIND_R8) :: lon, lat, hgt, lon1, lat1, hgt1
  real(ESMF_KIND_R4) :: interval
  integer :: i,j, k, l, ii, jj, kk, count1, count3, count8, localcount, countup, save, base, base1
  logical :: even
  integer :: start, count, diff, lastlat, totalelements, totalnodes, localnodes, startid
  integer :: wamtotalnodes
  integer :: elmtcount, increment
  integer :: startlevel, next, ind, ind1, totalnodes2d, totallevels, myrows, trigs
  integer, pointer :: rowinds(:), petTable(:), baseind(:)
  integer(ESMF_KIND_I4), pointer :: elementCnt(:), nodeCnt(:), sendbuf(:), recvbuf(:)
  integer(ESMF_KIND_I4), allocatable :: indList(:)
  real(ESMF_KIND_R8), pointer :: conntbl(:), globalCoords(:,:), fptr2d(:,:), fptr1d(:)
  type(ESMF_Arrayspec) :: arrayspec
  type(ESMF_Array) :: array, array1, array2
  real(ESMF_KIND_R8) :: maxerror, minerror, totalerrors, deg2rad
  real(ESMF_KIND_R8) :: starttime, endtime, timesend(1), timereport(1)
  real(ESMF_KIND_R8) :: differr
  real(ESMF_KIND_R8), pointer :: varout(:), lonbuf(:)
  real(ESMF_KIND_R8), pointer :: weights(:)
  integer(ESMF_KIND_I4), pointer :: indices(:,:)
  character(len=MAXNAMELEN) :: wamfilename
  integer :: wgtcount(1)
  integer, pointer :: allCounts(:), connectbase(:)
  real, parameter :: PI=3.1415927
  integer :: j1
  integer :: localrc, status
  real, parameter :: earthradius=6371.0  !in kilometers

  ! For output
  real(ESMF_KIND_R8), pointer :: lontbl(:), lattbl(:), hgttbl(:)
  integer :: lonid, latid, hgtid, vertid, elmtid, nodeid, numid, connid, timeid
  integer :: data1id, data2id, wgtid, wamid, ipeid
  integer :: globalTotal, globalTotalelmt, nodestartid, totalwgts
  type(ESMF_Distgrid) :: nodalDistgrid, distgrid

  rc = ESMF_SUCCESS

  !------------------------------------------------------------------------
  ! get global vm information
  !
  call ESMF_VMGetCurrent(vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  ! set up local pet info
  call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  !------------------------------------------------------------------------
  ! Allocate memory for the internal state and set it in the Component.
    allocate(is%wrap, stat=rc)
    if (ESMF_LogFoundAllocError(statusToCheck=rc, &
      msg="Allocation of the internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  wamfilename = 'data/wam3dgridnew.nc'

  minheight = 90
  maxheight = 800
  deg2rad = PI/180.0

#ifdef ESMF_NETCDF
  !!-------------------------------------
  !! Create WAM mesh
  !!-------------------------------------
  ! We need to create a 2D mesh with distgrid only and use it to get the data
  ! from the DATAWAM
  ! we also need to create the 3D intermediate WAM mesh to be used to regrid
  ! with IPE grid
 
  ! Read in WAM grid from wam3dgrid.nc
  !!
  status = nf90_open(path= wamfilename, mode=nf90_nowrite, ncid=nc1)
  call CheckNCError(status, wamfilename)
  status = nf90_inq_varid(nc1,'lons', varid)
  call CheckNCError(status, 'lons')
  status = nf90_inquire_variable(nc1, varid, ndims=ndims, dimids = dimids)
  call CheckNCError(status, 'lons')
  status = nf90_inquire_dimension(nc1,dimids(1), len=wamdims(1))
  call CheckNCError(status, 'lons 1st dimension')
  status = nf90_inquire_dimension(nc1,dimids(2), len=wamdims(2))
  call CheckNCError(status, 'lons 2nd dimension')

  ! WAM dimension order:  lons, lats (192, 94)
  allocate(wamlon(wamdims(1), wamdims(2)), &
  	   wamlat(wamdims(1), wamdims(2)))
  status = nf90_get_var(nc1, varid, wamlon)
  call CheckNCError(status, 'lons')
  status = nf90_inq_varid(nc1,'lats', varid)
  call CheckNCError(status, 'lats')
  status = nf90_get_var(nc1, varid, wamlat)
  call CheckNCError(status, 'lats')

  ! intermediate height fields
  status = nf90_inq_varid(nc1,'height', varid)
  call CheckNCError(status, 'height')
  status = nf90_inquire_variable(nc1, varid, ndims=ndims, dimids = dimids)
  call CheckNCError(status, 'height')
  status = nf90_inquire_dimension(nc1,dimids(1), len=wamdims(3))
  call CheckNCError(status, 'height 1st dimension')

  allocate(wamhgt(wamdims(3)))
  status = nf90_get_var(nc1, varid, wamhgt)
  call CheckNCError(status, 'height')
  allocate(NumPerRow(wamdims(2)), ShuffleOrder(wamdims(2)))
  status = nf90_inq_varid(nc1,'NumPerRow', varid)
  call CheckNCError(status, 'NumPerRow')
  status = nf90_get_var(nc1, varid, NumPerRow)
  call CheckNCError(status, 'NumPerRow')
  status = nf90_inq_varid(nc1,'ShuffleOrder', varid)
  call CheckNCError(status, 'ShuffleOrder')
  status = nf90_get_var(nc1, varid, ShuffleOrder)
  call CheckNCError(status, 'ShuffleOrder')
  status= nf90_close(nc1)
  call CheckNCError(status, wamfilename)

  ! Use the shuffle order to create the 2D mesh first
  ! find the total number of nodes in each processor and create local index table
  localnodes=0
  myrows = wamdims(2)/PetCnt
  if ((wamdims(2)-myrows*PetCnt) > PetNo) myrows = myrows+1
  allocate(rowinds(myrows))      !my local row index
  allocate(petTable(wamdims(2))) !the owner PET for each row
  next = 0
  ind1 = 0
  do i=1,wamdims(2)
    ind=ShuffleOrder(i)
    petTable(ind)=next
    if (next == PetNo) then
       ind1=ind1+1
       rowinds(ind1)=ind
       localnodes=localnodes + numPerRow(ind)
    endif
    next=next+1
    if (next == PetCnt) next=0
  enddo

  ! sort rowinds
  call ESMF_UtilSort(rowinds, ESMF_SORTFLAG_ASCENDING, rc)

  ! Create a distgrid using a collapsed 1D index array based on the local row index
  allocate(indList(localnodes))
  k=1
  do i=1,myrows
    ind=rowinds(i)
    do j=1,numPerRow(ind)
       indList(k)=(ind-1)*wamdims(1)+j
       k=k+1
    enddo
  enddo

  distgrid = ESMF_DistGridCreate(indList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
  ! Create mesh using the distgrid as the nodaldistgrid,  no elemdistgrid available
  ! just use nodeldistgrid for both
  wam2dmesh = ESMF_MeshCreate(distgrid,distgrid,rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
  
  ! Create the 3D mesh with fixed height
  ! find the lower height level where height > minheight
  do i=1,wamdims(3)
     if (wamhgt(i) > minheight) then
    	startlevel = i-1
	exit
     endif
  enddo
  totallevels = wamdims(3)-startlevel+1

  ! create the node table, find the total number of nodes in each processor, including the not-owned node
  totalnodes=0
  totalelements=0 
  allocate(baseind(myrows))
  do i=1,myrows
     ind=rowinds(i)
     baseind(i)=totalnodes
     ! Add the neighbor nodes
     ! If PetCnt==1, no need to add neighbor node
     ! If last row, no need to add neighbors
     ! If the neighbor is local, no need to add
     if (ind < wamdims(2)) then
       if (PetCnt>1) then
        if ((i < myrows .and. rowinds(i+1) /= ind+1) .or. i==myrows) then
          totalnodes=totalnodes+numPerRow(ind)+numPerRow(ind+1)
        else
          totalnodes=totalnodes+numPerRow(ind)
        endif
       endif
       if (numPerRow(ind) >= numPerRow(ind+1)) then
         totalelements = totalelements + numPerRow(ind)
       else
         totalelements = totalelements + numPerRow(ind+1)
       endif
     else
       totalnodes=totalnodes+numPerRow(ind)
       !Add extra elements at the top
       totalelements = totalelements+numPerRow(ind)-2
     endif
  enddo
  if (PetCnt == 1) then
     baseind(1)=0
     do i=2,wamdims(2)
       baseind(i)=baseind(i-1)+numPerRow(i-1)
     enddo
  endif

  totalnodes2d=totalnodes  ! totalnodes includes neighboring nodes and my own nodes
  totalnodes = totalnodes * totallevels 
  localnodes = localnodes * totallevels  ! localnodes are locally owned nodes
  totalelements = totalelements * (totallevels-1)
  allocate(nodeIds(totalnodes), nodeOwners(totalnodes), nodeCoords(totalnodes*3))

  ! Fill nodeIds, nodeOwners, and nodeCoords arrays, longitude first, latitude, then height
  count1=1
  localcount=1
  count3=1
  if (PetCnt > 1) then
  do k=1, totallevels
    do i=1,myrows
       ind=rowinds(i)
       do j=1,numPerRow(ind)
          ! Global id based on the 3D indices
       	  nodeIds(count1)= j+wamdims(1)*(ind-1)+wamdims(1)*wamdims(2)*(k-1)
          nodeOwners(count1)=PetNo
	  lon = wamlon(j,ind)
          lat  = wamlat(j,ind)
          hgt = wamhgt(startlevel+k-1)
          call convert2Cart(lon, lat, hgt, nodeCoords(count3:count3+2))
          count1=count1+1
          localcount=localcount+1
          count3=count3+3
       enddo
       ! if not the last row, add the neighbor row's nodes and the neighbor
       ! is not local
       if (ind < wamdims(2)) then
         if (i==myrows .or. (i < myrows .and. rowinds(i+1)/= ind+1)) then
       	  do j=1, numPerRow(ind+1)
            ! Global id based on the 3D indices
            nodeIds(count1)= j+wamdims(1)*ind+wamdims(1)*wamdims(2)*(k-1)
	    if (PetTable(ind+1) == PetNo) then
	       print *, PetNo, 'wrong neighbor ', count1, PetTable(ind+1)
            endif
            nodeOwners(count1)=PetTable(ind+1)
	    lon = wamlon(j,ind+1)
            lat  = wamlat(j,ind+1)
            hgt = wamhgt(k+startlevel-1)
            call convert2Cart(lon, lat, hgt, nodeCoords(count3:count3+2))
            count1=count1+1
            count3=count3+3
          enddo
         endif
	endif
     enddo
  enddo
  else ! PetCnt==1
  ! For sequential case, store the rows in its order, do not shuffle
  do k=1, totallevels
    do ind=1,myrows
       do j=1,numPerRow(ind)
          ! Global id based on the 3D indices
       	  nodeIds(count1)= j+wamdims(1)*(ind-1)+wamdims(1)*wamdims(2)*(k-1)
          nodeOwners(count1)=PetNo
	  lon = wamlon(j,ind)
          lat  = wamlat(j,ind)
          hgt = wamhgt(k+startlevel-1)
          call convert2Cart(lon, lat, hgt, nodeCoords(count3:count3+2))
          count1=count1+1
          localcount=localcount+1
          count3=count3+3
       enddo
     enddo
  enddo
  endif ! PetCnt > 1

  if (count1-1 /= totalnodes .or. localcount-1 /= localnodes) then
     print *, 'totalcount mismatch ', count1-1, totalnodes, localcount-1, localnodes
  endif

#ifdef USE_CART3D_COORDSYS
  wamMesh = ESMF_MeshCreate(3,3,coordSys=ESMF_COORDSYS_CART, rc=rc)
#else
  wamMesh = ESMF_MeshCreate(3,3,coordSys=ESMF_COORDSYS_SPH_DEG, rc=rc)
#endif
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
  call ESMF_MeshAddNodes(wamMesh, nodeIds, nodeCoords, nodeOwners, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
  
  deallocate(wamlon, wamlat)
  deallocate(nodeIds, nodeCoords, nodeOwners)

  allocate(elementIds(totalelements), elementTypes(totalelements), &
           elementConn(totalelements*8))

  elementTypes(:)=ESMF_MESHELEMTYPE_HEX

  ! find out the starting global id of the local element
  allocate(elementCnt(PetCnt),sendbuf(1))
  sendbuf(1)=totalelements
  call ESMF_VMAllGather(vm, sendbuf, elementCnt, 1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
  
  ! find the starting elementID
  startid=0
  do i=1,PetNo
    startid=startid+elementCnt(i)
  enddo
  globaltotalelmt = 0
  do i=1,PetCnt
    globaltotalelmt=globaltotalelmt+elementCnt(i)
  enddo
  deallocate(elementCnt, sendbuf)

  ! Build the local elementConn table using local node indices
  ! If PetCnt=1, no shuffle, the rows are in order
  count1=1
  count8=1
  do k=1, totallevels-1
    do i=1,myrows
       if (PetCnt > 1) then
           ind=rowinds(i)
       else
	   ind = i
       endif
       base = baseind(i)+totalnodes2d*(k-1)
       if (ind == wamdims(2)) then
#if 0
         ! create dummy elements by connecting every other nodes to form a triangle to cover the pole
         do j=1, numPerRow(ind)-2, 2
       	   elementIds(count1)=startid+count1
	   elementConn(count8)= base+j
	   elementConn(count8+1)=base+j+1
	   elementConn(count8+2)=base+j+2
	   elementConn(count8+3)=base+j+2
	   elementConn(count8+4)=base+totalnodes2d+j
	   elementConn(count8+5)=base+totalnodes2d+j+1
	   elementConn(count8+6)=base+totalnodes2d+j+2
	   elementConn(count8+7)=base+totalnodes2d+j+2
	   count1=count1+1
	   count8=count8+8
         enddo
         ! Last one, connect it back to the first node 
     	 elementIds(count1)=startid+count1
	 elementConn(count8)= base+j
	 elementConn(count8+1)=base+j+1
	 elementConn(count8+2)=base+1
	 elementConn(count8+3)=base+1
	 elementConn(count8+4)=base+totalnodes2d+j
	 elementConn(count8+5)=base+totalnodes2d+j+1
	 elementConn(count8+6)=base+totalnodes2d+1
	 elementConn(count8+7)=base+totalnodes2d+1
	 count1=count1+1
	 count8=count8+8
         cycle       
#else
         ! using the zigzag method to create triangles that covers the pole
         ! First half
         do j=1, numPerRow(ind)/2-1
       	   elementIds(count1)=startid+count1
	   elementConn(count8)= base+j
	   elementConn(count8+1)=base+j+1
	   elementConn(count8+2)=base+numPerRow(ind)-j
	   elementConn(count8+3)=base+numPerRow(ind)-j
	   elementConn(count8+4)=base+totalnodes2d+j
	   elementConn(count8+5)=base+totalnodes2d+j+1
	   elementConn(count8+6)=base+totalnodes2d+numPerRow(ind)-j
	   elementConn(count8+7)=base+totalnodes2d+j+numPerRow(ind)-j
	   count1=count1+1
	   count8=count8+8
         enddo
         ! second half
         do j=numPerRow(ind)/2+1, numPerRow(ind)-1
       	   elementIds(count1)=startid+count1
	   elementConn(count8)= base+j
	   elementConn(count8+1)=base+j+1
	   elementConn(count8+2)=base+numPerRow(ind)-j
	   elementConn(count8+3)=base+numPerRow(ind)-j
	   elementConn(count8+4)=base+totalnodes2d+j
	   elementConn(count8+5)=base+totalnodes2d+j+1
	   elementConn(count8+6)=base+totalnodes2d+numPerRow(ind)-j
	   elementConn(count8+7)=base+totalnodes2d+j+numPerRow(ind)-j
	   count1=count1+1
	   count8=count8+8
         enddo	   
         cycle
#endif
       endif
       ! the two adjacent rows have the same number of points, elements are cubes
       if (numPerRow(ind+1) == numPerRow(ind)) then
         do j=1,numPerRow(ind)-1
       	   elementIds(count1)=startid+count1
	   elementConn(count8)= base+j
	   elementConn(count8+1)=base+j+1
	   elementConn(count8+2)=base+numPerRow(ind)+j+1
	   elementConn(count8+3)=base+numPerRow(ind)+j
	   elementConn(count8+4)=base+totalnodes2d+j
	   elementConn(count8+5)=base+totalnodes2d+j+1
	   elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+j+1
	   elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+j
	   count1=count1+1
	   count8=count8+8
         enddo
         ! last one in the row, wrap around
       	 elementIds(count1)=startid+count1
	 elementConn(count8)= base+j
	 elementConn(count8+1)=base+1
	 elementConn(count8+2)=base+numPerRow(ind)+1
	 elementConn(count8+3)=base+numPerRow(ind)+j
	 elementConn(count8+4)=base+totalnodes2d+j
	 elementConn(count8+5)=base+totalnodes2d+1
	 elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+1
	 elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+j
	 count1=count1+1
	 count8=count8+8
       else
         ! the number of nodes are different, make prism elements
	 diff=numPerRow(ind)-numPerRow(ind+1)
	 if (diff > 0) then 
          ! make triangles with base at lower row
          ! triangles will be evenly distributed
	  interval = real(numPerRow(ind))/(diff+1)
	  jj=1
          trigs=1
          do j=1,numPerRow(ind)-1
	     if (j > trigs*interval) then
!	     if (mod(j,increment)==0) then
               ! triangles - base at bottom
               trigs=trigs+1
               elementIds(count1)=startid+count1
	       elementConn(count8)= base+j
	       elementConn(count8+1)=base+j+1
	       elementConn(count8+2)=base+numPerRow(ind)+jj
	       elementConn(count8+3)=base+numPerRow(ind)+jj
	       elementConn(count8+4)=base+totalnodes2d+j
	       elementConn(count8+5)=base+totalnodes2d+j+1
	       elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+jj
	       elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+jj
             else
               elementIds(count1)=startid+count1
	       elementConn(count8)= base+j
	       elementConn(count8+1)=base+j+1
	       elementConn(count8+2)=base+numPerRow(ind)+jj+1
	       elementConn(count8+3)=base+numPerRow(ind)+jj
	       elementConn(count8+4)=base+totalnodes2d+j
	       elementConn(count8+5)=base+totalnodes2d+j+1
	       elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+jj+1
	       elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+jj
	       jj=jj+1
	     endif
	     count1=count1+1
	     count8=count8+8
           enddo
           ! last one in the row, wrap around
           elementIds(count1)=startid+count1
	   elementConn(count8)= base+j
	   elementConn(count8+1)=base+1
	   elementConn(count8+2)=base+numPerRow(ind)+1
	   elementConn(count8+3)=base+numPerRow(ind)+jj
	   elementConn(count8+4)=base+totalnodes2d+j
	   elementConn(count8+5)=base+totalnodes2d+1
	   elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+1
	   elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+jj
  	   count1=count1+1
	   count8=count8+8
	   if (k==1 .and. (jj /= numPerRow(ind+1))) then
	      print *, PetNo, 'Upper row index mismatch', ind, jj, numPerRow(ind+1)
           endif
        else  ! diff < 0 
          ! make triangles with base at upper row
          ! triangles will be evenly distributed
	  interval = real(numPerRow(ind+1))/(-1*diff+1)
	  jj=1
	  trigs=1
          do j=1,numPerRow(ind+1)-1
	     if (j > trigs*interval) then
	       trigs = trigs+1              
               ! triangles - base at bottom
               elementIds(count1)=startid+count1
	       elementConn(count8)= base+jj
	       elementConn(count8+1)=base+jj
	       elementConn(count8+2)=base+numPerRow(ind)+j+1
	       elementConn(count8+3)=base+numPerRow(ind)+j
	       elementConn(count8+4)=base+totalnodes2d+jj
	       elementConn(count8+5)=base+totalnodes2d+jj
	       elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+j+1
	       elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+j
             else
               elementIds(count1)=startid+count1
	       elementConn(count8)= base+jj
	       elementConn(count8+1)=base+jj+1
	       elementConn(count8+2)=base+numPerRow(ind)+j+1
	       elementConn(count8+3)=base+numPerRow(ind)+j
	       elementConn(count8+4)=base+totalnodes2d+jj
	       elementConn(count8+5)=base+totalnodes2d+jj+1
	       elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+j+1
	       elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+j
	       jj=jj+1
	     endif
	     count1=count1+1
	     count8=count8+8
           enddo
           ! last one in the row, wrap around
       	   elementIds(count1)=startid+count1
	   elementConn(count8)= base+jj
	   elementConn(count8+1)=base+1
	   elementConn(count8+2)=base+numPerRow(ind)+1
	   elementConn(count8+3)=base+numPerRow(ind)+j
	   elementConn(count8+4)=base+totalnodes2d+jj
	   elementConn(count8+5)=base+totalnodes2d+1
	   elementConn(count8+6)=base+numPerRow(ind)+totalnodes2d+1
	   elementConn(count8+7)=base+numPerRow(ind)+totalnodes2d+j
 	   count1=count1+1
	   count8=count8+8
	   if (k==1 .and. (jj /= numPerRow(ind))) then
	      print *, PetNo, 'Lower row index mismatch', ind, jj, numPerRow(ind)
           endif
        endif
       endif         	            
    enddo
  enddo 

  if (count1-1 /= totalelements) then
     print *, 'total element mismatch ', count1-1, totalelements
  endif

  do i=1, totalelements*8
     if (elementConn(i) > totalnodes) then
          print *, PetNo, 'node id out of bound', i/8, elementConn(i)
     endif
  enddo  
  call ESMF_MeshAddElements(wamMesh, elementIds, elementTypes, elementConn,rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  deallocate(NumPerRow, ShuffleOrder, rowinds, petTable)
  deallocate(indList, baseind)
  deallocate(elementIds, elementTypes, elementConn)

  ! Info passed to the run routine
  is%wrap%wamdims = wamdims
  is%wrap%wamhgt => wamhgt
  is%wrap%startlevel = startlevel
  is%wrap%totallevels = totallevels
  is%wrap%wamtotalnodes = totalnodes
  is%wrap%localnodes = localnodes
  is%wrap%wam2dMesh = wam2dMesh
  is%wrap%wamMesh = wamMesh
  is%wrap%PetNo = PetNo
  is%wrap%PetCnt = PetCnt

  return 
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled") 
    return
#endif

end subroutine InitGrids

! Run Routine
subroutine RunRegrid(model, importState, exportState, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    integer, intent(out) :: rc
    
  type(ESMF_VM) :: vm
  type(InternalState)     :: is
  type(ESMF_RouteHandle)  :: routehandle
  type(ESMF_Field)        :: hgtfield, datafield, ipefield, wamfield
  real(ESMF_KIND_R8), pointer :: hgtbuf(:,:), varbuf(:,:)
  real(ESMF_KIND_R8), pointer :: databuf(:), dstdata(:), wamhgt(:)
  real(ESMF_KIND_R8), pointer :: wamdata(:,:)
  integer :: nc1, nc2
  integer :: varid, data2id
  integer :: ndims, dimids(3), wamdims(3)
  integer :: PetNo, PetCnt
  integer :: totalnodes
  real(ESMF_KIND_R8), pointer :: fptr2d(:,:), fptr1d(:), fieldarray(:)
  type(ESMF_Arrayspec) :: arrayspec
  type(ESMF_Array) :: array
  type(ESMF_DistGrid) :: distgrid
  real(ESMF_KIND_R8) :: maxerror, minerror, totalerrors, deg2rad
  real(ESMF_KIND_R8) :: starttime, endtime, timesend(1), timereport(1)
  real(ESMF_KIND_R8) :: differr
  real(ESMF_KIND_R8), pointer :: varout(:), lonbuf(:)
  character(len=80) :: filename
  real, parameter :: PI=3.1415927
  integer :: i, ii, j, jj, j1, k, kk, l, count1
  integer :: localrc, status
  integer :: itemCount, localnodes, startlevel, totallevels, inlevels
  integer :: ubnd(2), lbnd(2)
  character(len=80), allocatable :: fieldNameList(:)
  integer :: numNodes, numElmts
  integer, save      :: slice=1

  rc = ESMF_SUCCESS

  !------------------------------------------------------------------------
  ! get global vm information
  !
  ! query component for its internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  wamdims=is%wrap%wamdims
  wamhgt => is%wrap%wamhgt
  PetCnt = is%wrap%PetCnt
  PetNo  = is%wrap%PetNo  
  startlevel = is%wrap%startlevel
  totallevels = is%wrap%totallevels

  ! Get the data from DATAWAM import fields
  ! Do 1D linear interpolation of the variables in the z direction to the fixed height grid first,

  call ESMF_StateGet(importstate, itemCount=itemCount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
  allocate(fieldNameList(itemCount))
  call ESMF_StateGet(importstate, itemNameList=fieldNameList, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

  call ESMF_StateGet(importState, itemName="height", &
       field=hgtfield, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
       return  ! bail out
#if 0
  if (slice == 2) then
  call ESMF_FieldGet(hgtfield, array=array, rc=rc)
  call ESMF_ArrayWrite(array, 'wamhgt.nc', rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
       return  ! bail out
  endif
#endif
  call ESMF_FieldGet(hgtfield, farrayPtr=hgtbuf, computationalLbound=lbnd, &
       computationalUbound=ubnd, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
       return  ! bail out
  inlevels = ubnd(2)-lbnd(2)+1

#if 0
   if (slice==2) then
        ! write out text file for heights
        write(filename, "(A7,I2)") 'height.', PetNo+32
        open(100, file=filename)
        write(100, '(8(1X, F6.2))') wamhgt
        do i=lbnd(1), ubnd(1)
          write(100, '(8(1X, F6.2))') hgtbuf(i,:)
        enddo
        close(100)
   endif
#endif

   do j=1, itemCount
     if (fieldNameList(j) .ne. "height") then
        call ESMF_StateGet(importstate, itemname=fieldNameList(j), &
	   	   field=datafield, rc=rc)
  	if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
	call ESMF_FieldGet(datafield, farrayPtr=varbuf, computationalLbound=lbnd, &
       	   	computationalUbound=ubnd, rc=rc)
  	if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
	localnodes=ubnd(1)-lbnd(1)+1
        totalnodes = localnodes * totallevels

	! interpolate dataptr(localnodes, inlevels) to wamdata(localnodes, totallevels) 
        ! alone the second dimension
        ! using hgtbuf(localnodes,inlevels) as the source height and wamhgt(totallevels)
        ! as the destination heights 
        ! kk is the source index in 2nd dimension, k is the destination index   
        ! note hgtbuf has the heigth of the original WAM grid (150), the wamdim(3) is the
        ! fixed height WAM grid extended to 800KM, which is > 150

        if (j==1) allocate(wamdata(localnodes, totallevels))
        ! At the first time step, the values from the importstate are all invalid
        if (slice==1) then 
	   wamdata(:,:)=1.0
        else
  	  do i=1,localnodes
            kk = 1  ! source ind
            do k=startlevel, wamdims(3)
               do while (kk<=inlevels .and. hgtbuf(i,kk)<wamhgt(k))
	          kk=kk+1
               enddo
	       if (kk>inlevels) then
                  do l=k,wamdims(3) ! use the value as the highest level in the source grid
                                    ! to fill the remaining levels in the destination
 	             wamdata(i,l-startlevel+1)=varbuf(i,kk-1)
                  enddo
	          exit
               endif
               if (kk>1) then
                  wamdata(i,k-startlevel+1)=(varbuf(i,kk)*(wamhgt(k)-hgtbuf(i,kk-1))+ &
	               varbuf(i,kk-1)*(hgtbuf(i,kk)-wamhgt(k)))/ &
		       (hgtbuf(i,kk)-hgtbuf(i,kk-1))
               else 
	          wamdata(i,k-startlevel+1)=varbuf(i,kk)
               endif
            enddo
          enddo
        endif

        wamfield=ESMF_FieldCreate(is%wrap%wammesh, reshape(wamdata, (/localnodes*totallevels/)), &
		ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, meshloc=ESMF_MESHLOC_NODE, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
   
        ! Find the field of the same name in the export state -- that is built on IPE mesh
        call ESMF_StateGet(exportstate, itemname=fieldNameList(j), &
     	  field=ipefield, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
        ! Regrid!!
        call ESMF_FieldRegrid(wamfield, ipefield, is%wrap%routehandle, &
	       zeroregion=ESMF_REGION_TOTAL, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out

        call ESMF_FieldDestroy(wamfield)
      endif
   enddo
   deallocate(wamdata)

    ! advance the time slice counter
    slice = slice + 1

  return

end subroutine RunRegrid

!Finalize Routine
subroutine Finalize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

  type(ESMF_VM) :: vm
  type(InternalState)     :: is

  ! query component for its internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  
  ! Destroy ESMF objects
  call ESMF_MeshDestroy(is%wrap%wam2dmesh)
  call ESMF_MeshDestroy(is%wrap%wammesh)
  call ESMF_MeshDestroy(is%wrap%ipemesh)
  call ESMF_FieldRegridRelease(is%wrap%routehandle)

  ! deallocate
  deallocate(is%wrap%wamhgt)
  deallocate(is%wrap)
  
  print *, 'Complete MEDIATOR'
end subroutine Finalize

subroutine ErrorMsgAndAbort(localPet)
    integer ::  localPet
  
    if (localPet >= 0) then
      write(*,*) "ERROR: Problem on processor ",localPet,". Please see the PET*.ESMF_LogFile files for a traceback."
    else
      write(*,*) "ERROR: Please see the PET*.LogFile files for a traceback."
    endif
  
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
end subroutine ErrorMsgAndAbort

!------------------------------------------------------------------------------
!
!  check CDF file error code
!
#undef  ESMF_METHOD
#define ESMF_METHOD "CheckNCError"
subroutine CheckNCError (ncStatus, errmsg)

    integer,          intent(in)  :: ncStatus
    character(len=*), intent(in)  :: errmsg

    integer, parameter :: nf90_noerror = 0

#ifdef ESMF_NETCDF
    if ( ncStatus .ne. nf90_noerror) then
        print '("NetCDF Error: ", A, " : ", A)', &
    		trim(errmsg),trim(nf90_strerror(ncStatus))
        call ErrorMsgAndAbort(-1)
    end if
#else
    call ESMF_LogSetError(ESMF_RC_LIB_NOT_PRESENT, & 
                 msg="- ESMF_NETCDF not defined when lib was compiled") 
    return
#endif

end subroutine CheckNCError

!------------------------------------------------------------------------------
!
!  Convert 3D Spherical to 3D Cartisian if USE_CART3D_COORDSYS is set,
!    otherwise, just normalize the z field
!
#undef  ESMF_METHOD
#define ESMF_METHOD "convet2Cart"
#ifdef USE_CART3D_COORDSYS
subroutine convert2Cart (lon, lat, hgt, coords, rc)
   real(ESMF_KIND_R8):: lon, lat, hgt
   real(ESMF_KIND_R8):: coords(3)
   integer, optional :: rc

   real(ESMF_KIND_R8) :: earthradius, nhgt
   integer :: localrc

   if (present(rc)) rc=ESMF_FAILURE
   earthradius = 6371.0
   nhgt = 1+hgt/earthradius

   call c_esmc_sphdeg_to_cart(lon, lat, &
               coords(1), coords(2), coords(3), &
               localrc)
   if (localrc /= ESMF_SUCCESS) return 

   coords(1)=nhgt*coords(1)
   coords(2)=nhgt*coords(2)
   coords(3)=nhgt*coords(3)

   if (present(rc)) rc=ESMF_SUCCESS

end subroutine convert2Cart
#else
subroutine convert2Cart (lon, lat, hgt, coords, rc)
   real(ESMF_KIND_R8):: lon, lat, hgt
   real(ESMF_KIND_R8):: coords(3)
   integer, optional :: rc

   real(ESMF_KIND_R8) :: earthradius, nhgt
   integer :: localrc

   if (present(rc)) rc=ESMF_FAILURE
   earthradius = 6371.0
   nhgt = 1+hgt/earthradius

   coords(1)=lon
   coords(2)=lat
   coords(3)=nhgt

   if (present(rc)) rc=ESMF_SUCCESS

end subroutine convert2Cart
#endif
#undef  ESMF_METHOD
#define ESMF_METHOD "convet2Sphdeg"
subroutine convert2Sphdeg (coord1, coord2, coord3, lon, lat, hgt)
   real(ESMF_KIND_R8):: coord1, coord2, coord3
   real(ESMF_KIND_R8):: lon, lat, hgt

   real(ESMF_KIND_R8) :: earthradius, nhgt, rad2deg
   real, parameter :: PI=3.1415927
   integer :: localrc

   earthradius = 6371.0
   rad2deg = 180.0/PI
   nhgt = sqrt(coord1*coord1+coord2*coord2+coord3*coord3)
   hgt = (nhgt-1)*earthradius
   lon = atan(coord2/coord1)*rad2deg
   if (coord1 < 0) lon = lon + 180.0
   if (coord1 > 0 .and. coord2 < 0) lon = 360.0 + lon
   lat = 90-acos(coord3/nhgt)*rad2deg

end subroutine convert2Sphdeg

end module
