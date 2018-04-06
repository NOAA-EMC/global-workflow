!--------------- FV3GFS solo model -----------------
!
!*** The FV3 atmosphere grid component nuopc cap
!
! Author:  Jun Wang@noaa.gov
!
! revision history
! 11 Oct 2016: J. Wang          Initial code
! 18 Apr 2017: J. Wang          set up fcst grid component and write grid components
! 02 Nov 2017: J. Wang          Use Gerhard's transferable RouteHandle
!

module fv3gfs_cap_mod

  use ESMF
  use NUOPC
  use NUOPC_Model,        only: model_routine_SS      => SetServices,  &
                                model_label_Advance   => label_Advance,&
                                model_label_Finalize  => label_Finalize
!
  use module_fv3_config,  only: quilting, restart_interval,             &
                                nfhout, nfhout_hf, nsout, dt_atmos,     &
                                nfhmax, nfhmax_hf,output_hfmax,         &
                                output_interval,output_interval_hf,     &
                                alarm_output_hf, alarm_output,          &
                                calendar, calendar_type,                &
                                force_date_from_configure
  use module_fv3_io_def,  only: num_pes_fcst,write_groups,              &
                                num_files, filename_base,               &
                                wrttasks_per_group, n_group,            &
                                lead_wrttask, last_wrttask,             &
                                output_grid, output_file,               &
                                imo, jmo, write_nemsioflip,             &
                                write_fsyncflag
!
  use module_fcst_grid_comp,  only: fcstSS => SetServices
  use module_wrt_grid_comp,   only: wrtSS => SetServices
!
  implicit none
  private
  public SetServices
!
!-----------------------------------------------------------------------
!
  type(ESMF_Clock),save                       :: clock_fv3

  type(ESMF_GridComp)                         :: fcstComp
  type(ESMF_State)                            :: fcstState
  character(len=80),         allocatable      :: fcstItemNameList(:)
  type(ESMF_StateItem_Flag), allocatable      :: fcstItemTypeList(:)
  type(ESMF_FieldBundle),    allocatable      :: fcstFB(:)
  integer, save                               :: FBCount

  type(ESMF_GridComp),    allocatable         :: wrtComp(:)
  type(ESMF_State),       allocatable         :: wrtState(:)
  type(ESMF_FieldBundle), allocatable         :: wrtFB(:,:)

  type(ESMF_RouteHandle), allocatable         :: routehandle(:,:)
  integer, allocatable                        :: fcstPetList(:)
  
  logical :: profile_memory = .true.

!-----------------------------------------------------------------------

  contains

  !-----------------------------------------------------------------------
  !------------------- Solo fv3gfs code starts here ----------------------
  !-----------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    character(len=*),parameter  :: subname='(fv3gfs_cap:SetServices)'

    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! initialization, switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! model advance method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! model finalize method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=atmos_model_finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    character(len=10)                         :: value

    rc = ESMF_SUCCESS

    ! Switch to IPDv01 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv00p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_AttributeGet(gcomp, name="ProfileMemory", value=value, defaultValue="true", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    profile_memory=(trim(value)/="false")

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)

    type(ESMF_GridComp)                    :: gcomp
    type(ESMF_State)                       :: importState, exportState
    type(ESMF_Clock)                       :: clock
    integer, intent(out)                   :: rc
!
! local variables
    type(ESMF_VM)                          :: vm
    type(ESMF_Time)                        :: CurrTime, starttime, StopTime
    type(ESMF_Time)                        :: alarm_output_hf_ring, alarm_output_ring 
    type(ESMF_Time)                        :: alarm_output_hf_stop, alarm_output_stop 
    type(ESMF_TimeInterval)                :: RunDuration, timeStep
    type(ESMF_Config)                      :: cf
    type(ESMF_RegridMethod_Flag)           :: regridmethod

    integer,dimension(6)                   :: date, date_init
    integer                                :: mpi_comm_atm
    integer                                :: i, j, k, io_unit, urc
    integer                                :: petcount, mype
    logical                                :: OPENED
    character(ESMF_MAXSTR)                 :: name
    logical                                :: fcstpe
    logical,dimension(:), allocatable      :: wrtpe
    integer,dimension(:), allocatable      :: petList, originPetList, targetPetList
    character(20)                          :: cwrtcomp
    character(160)                         :: msg
    integer                                :: isrctermprocessing

    character(len=*),parameter  :: subname='(mom_cap:InitializeAdvertise)'
    integer nfmout, nfsout , nfmout_hf, nfsout_hf
    real(kind=8) :: MPI_Wtime, timewri, timeis,timeie,timerhs, timerhe
!
!------------------------------------------------------------------------
!
    rc = ESMF_SUCCESS
    timeis = mpi_wtime()

    call ESMF_GridCompGet(gcomp,name=name,vm=vm,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, mpiCommunicator=mpi_comm_atm,petCount=petcount, &
             localpet = mype,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!    print *,'in fv3_cap,initAdvertize,name=',trim(name),'mpi_comm=',mpi_comm_atm, &
!       'petcount=',petcount,'mype=',mype

    clock_fv3=clock
!
!------------------------------------------------------------------------
! get config variables
!
    CF=ESMF_ConfigCreate(rc=RC)
    CALL ESMF_ConfigLoadFile(config=CF ,filename='model_configure' ,rc=RC)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!
    CALL ESMF_ConfigGetAttribute(config=CF,value=restart_interval, &
                                 label ='restart_interval:',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!
    CALL ESMF_ConfigGetAttribute(config=CF,value=calendar, &
                                 label ='calendar:',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!
    CALL ESMF_ConfigGetAttribute(config=CF,value=quilting, &
                                 label ='quilting:',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if(mype==0) print *,'af nems config,quilting=',quilting,'calendar=', &
     trim(calendar)
!
    nfhout=0; nfhmax_hf=0; nfhout_hf=0; nsout=0
    if ( quilting ) then
      CALL ESMF_ConfigGetAttribute(config=CF,value=write_groups, &
                                 label ='write_groups:',rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!
      CALL ESMF_ConfigGetAttribute(config=CF,value=wrttasks_per_group, &
                                   label ='write_tasks_per_group:',rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if(mype==0) print *,'af nems config,restart_interval=',restart_interval, &
      'quilting=',quilting,'write_groups=',write_groups,wrttasks_per_group, &
      'calendar=',trim(calendar),'calendar_type=',calendar_type
!
      CALL ESMF_ConfigGetAttribute(config=CF,value=num_files, &
                                 label ='num_files:',rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!
      allocate(filename_base(num_files))
      CALL ESMF_ConfigFindLabel(CF,'filename_base:',rc=RC)
      do i=1,num_files
        CALL ESMF_ConfigGetAttribute(config=CF,value=filename_base(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      enddo
      if(mype==0) print *,'af nems config,num_files=',num_files, &
      'filename_base=',filename_base
!
! variables for alarms
      call ESMF_ConfigGetAttribute(config=CF,value=nfhout,   label ='nfhout:',rc=rc)
      call ESMF_ConfigGetAttribute(config=CF,value=nfhmax_hf,label ='nfhmax_hf:',rc=rc)
      call ESMF_ConfigGetAttribute(config=CF,value=nfhout_hf,label ='nfhout_hf:',rc=rc)
      call ESMF_ConfigGetAttribute(config=CF,value=nsout,    label ='nsout:',rc=rc)
      if(mype==0) print *,'af nems config,nfhout=',nfhout,nfhmax_hf,nfhout_hf, nsout
! variables for I/O options
      call ESMF_ConfigGetAttribute(config=CF,value=output_grid, label ='output_grid:',rc=rc)
      if(mype==0) print *,'af nems config,output_grid=',trim(output_grid)
      call ESMF_ConfigGetAttribute(config=CF,value=output_file, label ='output_file:',rc=rc)
      if(mype==0) print *,'af nems config,output_file=',trim(output_file)
      write_nemsioflip=.false.
      write_fsyncflag =.false.
      if(trim(output_grid) == 'gaussian_grid') then
        call ESMF_ConfigGetAttribute(config=CF,value=imo, label ='imo:',rc=rc)
        call ESMF_ConfigGetAttribute(config=CF,value=jmo, label ='jmo:',rc=rc)
        call ESMF_ConfigGetAttribute(config=CF,value=write_nemsioflip, label ='write_nemsioflip:',rc=rc)
        call ESMF_ConfigGetAttribute(config=CF,value=write_fsyncflag, label ='write_fsyncflag:',rc=rc)
        if(mype==0) print *,'af nems config,imo=',imo,'jmo=',jmo
        if(mype==0) print *,'af nems config,write_nemsioflip=',write_nemsioflip,'write_fsyncflag=',write_fsyncflag
      endif
!end quilting
    endif
!
    call ESMF_ConfigGetAttribute(config=CF,value=dt_atmos, label ='dt_atmos:',rc=rc)
    call ESMF_ConfigGetAttribute(config=CF,value=nfhmax,   label ='nhours_fcst:',rc=rc)
    if(mype==0) print *,'af nems config,dt_atmos=',dt_atmos,'nfhmax=',nfhmax
    call ESMF_TimeIntervalSet(timeStep,s=dt_atmos,rc=rc)
    call ESMF_ClockSet(clock_fv3,timeStep=timeStep, rc=rc)
!
!------------------------------------------------------------------------
! may need to set currTime for restart
!
    call ESMF_ClockGet(clock_fv3, currTIME=CurrTime,  StartTime=startTime,    &
                       RunDuration=RunDuration, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    StopTime = startTime + RunDuration

! *** read restart time from restart file
    do i=751,899
       inquire(i,opened=OPENED)
       if(.not.OPENED)then
         io_unit=i
         exit
       endif
    enddo
!
    date = 0; date_init = 0
    force_date_from_configure = .true.
!
    open(unit=io_unit, file=trim('INPUT/coupler.res'),status="old",err=998 )
    read(io_unit,*,err=999) calendar_type
    read (io_unit,*) date_init
    read (io_unit,*) date
    close(io_unit)
    force_date_from_configure = .false.
!
    if(date(1)==0 .and. date_init(1) /=0) date = date_init
    if(mype==0) print *,'bf clock_fv3,date=',date,'date_init=',date_init

    call ESMF_VMbroadcast(vm, date, 6, 0)
    call ESMF_TimeSet(time=CurrTime,yy=date(1),mm=date(2),dd=date(3),h=date(4), &
                      m=date(5),s=date(6),rc=rc)
999 continue
998 continue
!    if(mype==0) print *,'final date =',date,'date_init=',date_init

!reset CurrTime in clock
    call ESMF_ClockSet(clock_fv3, currTIME=CurrTime, startTime=startTime,  &
                       stopTime=stopTime, timeStep=timeStep, rc=rc)

!
!#######################################################################
! set up fcst grid component
!
!----------------------------------------------------------------------
!*** create fv3 atm tasks and quilt servers
!-----------------------------------------------------------------------
!
! create fcst grid component

    fcstpe = .false.
    num_pes_fcst = petcount - write_groups * wrttasks_per_group
    allocate(fcstPetList(num_pes_fcst))
    do j=1, num_pes_fcst
      fcstPetList(j) = j - 1
      if(mype == fcstPetlist(j)) fcstpe = .true.
    enddo
    fcstComp = ESMF_GridCompCreate(petList=fcstPetList, name='fv3_fcst', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
    call ESMF_GridCompSetServices(fcstComp, fcstSS, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

! create fcst state
    fcstState = ESMF_StateCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

! call fcst Initialize (including creating fcstgrid and fcst fieldbundle)
    call ESMF_GridCompInitialize(fcstComp, exportState=fcstState,    &
         clock=clock_fv3, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
! reconcile the fcstComp's import state
    call ESMF_StateReconcile(fcstState, attreconflag= ESMF_ATTRECONCILE_ON, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
! determine number elements in fcstState
    call ESMF_StateGet(fcstState, itemCount=FBCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if(mype==0) print *,'af fcstCom FBCount= ',FBcount
!
! allocate arrays
    allocate(fcstFB(FBCount), fcstItemNameList(FBCount), fcstItemTypeList(FBCount))
!
! pull out the item names and item types from fcstState
    call ESMF_StateGet(fcstState, itemNameList=fcstItemNameList, &
      itemTypeList=fcstItemTypeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
! loop over all items in the fcstState and collect all FieldBundles
    do i=1, FBcount
      if (fcstItemTypeList(i)==ESMF_STATEITEM_FIELDBUNDLE) then
        ! access the FieldBundle
        call ESMF_StateGet(fcstState, itemName=fcstItemNameList(i), &
          fieldbundle=fcstFB(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!    if(mype==0.or.mype==144) print *,'af fcstFB,i=',i,'name=',trim(fcstItemNameList(i))
      else

      !***### anything but a FieldBundle in the state is unexpected here
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Only FieldBundles supported in fcstState.", &
            line=__LINE__, &
            file=__FILE__)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    enddo
!
!-----------------------------------------------------------------------
!***  create and initialize Write component(s).
!-----------------------------------------------------------------------
!
    if( quilting ) then

!
      allocate(wrtComp(write_groups), wrtState(write_groups) )
      allocate(wrtFB(5,write_groups), routehandle(5,write_groups))
      allocate(lead_wrttask(write_groups), last_wrttask(write_groups))
!
      allocate(petList(wrttasks_per_group))
      if(mype==0) print *,'af allco wrtComp,write_groups=',write_groups
!
      allocate(originPetList(num_pes_fcst+wrttasks_per_group))
      allocate(targetPetList(num_pes_fcst+wrttasks_per_group))
!
      k = num_pes_fcst
      timerhs = mpi_wtime()
      do i=1, write_groups

! prepare petList for wrtComp(i)
        lead_wrttask(i) = k
        do j=1, wrttasks_per_group
          petList(j) = k + j-1
        enddo
        k = k + wrttasks_per_group
        last_wrttask(i) = k - 1
!        if(mype==0)print *,'af wrtComp(i)=',i,'k=',k

! prepare name of the wrtComp(i)
        write(cwrtcomp,"(A,I2.2)") "wrtComp_", i
! create wrtComp(i)
        wrtComp(i) = ESMF_GridCompCreate(petList=petList, name=trim(cwrtcomp), rc=rc)
!      print *,'af wrtComp(i)=',i,'name=',trim(cwrtcomp),'rc=',rc
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)

! call into wrtComp(i) SetServices
        call ESMF_GridCompSetServices(wrtComp(i), wrtSS, userRc=urc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)

! create wrtstate(i)
        wrtstate(i) = ESMF_StateCreate(rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)

! add the fcst FieldBundles to the wrtState(i) so write component can
        call ESMF_AttributeCopy(fcstState, wrtState(i), &
           attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)

! use this info to create mirror objects
        call ESMF_StateAdd(wrtState(i), fcstFB, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)

! call into wrtComp(i) Initialize
        call ESMF_GridCompInitialize(wrtComp(i), importState=wrtstate(i), &
           clock=clock_fv3, phase=1, userRc=urc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)

! remove fcst FieldBundles from the wrtState(i) because done with it
        call ESMF_StateRemove(wrtState(i), fcstItemNameList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)

! reconcile the wrtComp(i)'s export state
        call ESMF_StateReconcile(wrtState(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        if(mype==0) print *,'af wrtState reconcile, FBcount=',FBcount

        call ESMF_AttributeCopy(fcstState, wrtState(i), &
           attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)

! loop over all FieldBundle in the states and precompute Regrid operation
        do j=1, FBcount

          ! access the mirrored FieldBundle in the wrtState(i)
          call ESMF_StateGet(wrtState(i), &
             itemName="mirror_"//trim(fcstItemNameList(j)), &
             fieldbundle=wrtFB(j,i), rc=rc)
      if(mype==0) print *,'af get wrtfb=',"mirror_"//trim(fcstItemNameList(j)),'rc=',rc
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)

! determine regridmethod
          if (index(fcstItemNameList(j),"_bilinear") >0 )  then
            regridmethod = ESMF_REGRIDMETHOD_BILINEAR
          else if (index(fcstItemNameList(j),"_patch") >0)  then
            regridmethod = ESMF_REGRIDMETHOD_PATCH
          else if (index(fcstItemNameList(j),"_nearest_stod") >0) then
            regridmethod = ESMF_REGRIDMETHOD_NEAREST_STOD
          else if (index(fcstItemNameList(j),"_nearest_dtos") >0) then
            regridmethod = ESMF_REGRIDMETHOD_NEAREST_DTOS
          else if (index(fcstItemNameList(j),"_conserve") >0) then
            regridmethod = ESMF_REGRIDMETHOD_CONSERVE
          else
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Unable to determine regrid method.", &
            line=__LINE__, &
            file=__FILE__)
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
          endif

          call ESMF_LogWrite('bf FieldBundleRegridStore', ESMF_LOGMSG_INFO, rc=rc)
          write(msg,"(A,I2.2,',',I2.2,A)") "calling into wrtFB(",j,i, ") FieldBundleRegridStore()...."
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)

          if (i==1) then
          ! this is a Store() for the first wrtComp -> must do the Store()
            timewri = mpi_wtime()

            isrctermprocessing = 1
            call ESMF_FieldBundleRegridStore(fcstFB(j), wrtFB(j,i), &
              regridMethod=regridmethod, routehandle=routehandle(j,i),  &
              srcTermProcessing=isrctermprocessing, rc=rc)

           if(mype==0) print *,'after regrid store, group i=',i,' fb=',j,' time=',mpi_wtime()-timewri
            call ESMF_LogWrite('af FieldBundleRegridStore', ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              call ESMF_Finalize(endflag=ESMF_END_ABORT)

            originPetList(1:num_pes_fcst)  = fcstPetList(:)
            originPetList(num_pes_fcst+1:) = petList(:)

          else
            targetPetList(1:num_pes_fcst)  = fcstPetList(:)
            targetPetList(num_pes_fcst+1:) = petList(:)
            routehandle(j,i) = ESMF_RouteHandleCreate(routehandle(j,1), &
              originPetList=originPetList, targetPetList=targetPetList, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              call ESMF_Finalize(endflag=ESMF_END_ABORT)

          endif
          write(msg,"(A,I2.2,',',I2.2,A)") "... returned from wrtFB(",j,i, ") FieldBundleRegridStore()."
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        enddo

! end write_groups
      enddo
      if(mype==0) print *,'in fv3cap init, time wrtcrt/regrdst',mpi_wtime()-timerhs
      deallocate(petList)
      deallocate(originPetList)
      deallocate(targetPetList)
!
!---------------------------------------------------------------------------------
!---  SET UP ALARM
!
!--- for every time step output, overwrite nfhout

      if(nsout>0) then
        nfhout = int(nsout*dt_atmos/3600.)
        nfmout = int((nsout*dt_atmos-nfhout*3600.)/60.)
        nfsout = int(nsout*dt_atmos-nfhout*3600.-nfmout*60)
      else
        nfmout = 0
        nfsout = 0
      endif
      call ESMF_TimeIntervalSet(output_interval, h=nfhout, m=nfmout,  s=nfsout, rc=rc)
      if(mype==0) print *,'af set up output_interval,rc=',rc,'nfhout=',nfhout,nfmout,nfsout

      if (nfhmax_hf > 0) then

        nfmout_hf = 0; nfsout_hf = 0
        call ESMF_TimeIntervalSet(output_interval_hf, h=nfhout_hf, m=nfmout_hf, &
                                 s=nfsout_hf, rc=rc)
        call ESMF_TimeIntervalSet(output_hfmax, h=nfhmax_hf, m=0, s=0, rc=rc)
        alarm_output_hf_stop = starttime + output_hfmax + output_interval_hf 
        if (currtime <= starttime+output_hfmax) then
          alarm_output_hf_ring = currtime  + output_interval_hf
          alarm_output_hf = ESMF_AlarmCreate(clock_fv3,name='ALARM_OUTPUT_HF',  &
                                               ringTime =alarm_output_hf_ring,&
                                               ringInterval =output_interval_hf,  &  !<-- Time interval between
                                               stoptime =alarm_output_hf_stop,&  !<-- Time interval between
                                               ringTimeStepCount=1, &  !<-- The Alarm rings for this many timesteps
                                               sticky           =.false., &  !<-- Alarm does not ring until turned off
                                               rc               =RC)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
          alarm_output_ring = currtime + output_hfmax + output_interval
        else
          alarm_output_ring = currtime + output_interval
        endif
      else
         alarm_output_ring = currtime + output_interval
      endif

      call ESMF_TimeIntervalSet(output_interval, h=nfhout, m=nfmout, &
                              s=nfsout, rc=rc)
      alarm_output = ESMF_AlarmCreate(clock_fv3, name      ='ALARM_OUTPUT',    &
                                    ringTime         =alarm_output_ring, & !<-- Forecast/Restart start time (ESMF)
                                    ringInterval     =output_interval,   & !<-- Time interval between
                                    ringTimeStepCount=1,                 & !<-- The Alarm rings for this many timesteps
                                    sticky           =.false.,           & !<-- Alarm does not ring until turned off
                                    rc               =RC)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!***  SET THE FIRST WRITE GROUP AS THE FIRST ONE TO ACT.
!-----------------------------------------------------------------------
!
      n_group = 1
!
!end quilting
    endif
!
    if(mype==0) print *,'in fv3_cap, init time=',mpi_wtime()-timeis
!-----------------------------------------------------------------------
!
  end subroutine InitializeAdvertise
!
  !-----------------------------------------------------------------------------

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Grid)         :: grid

    rc = ESMF_SUCCESS

    ! nothing is realized in the import/export States

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)                    :: gcomp
    integer, intent(out)                   :: rc
    
    ! local variables
    type(ESMF_Field)                       :: field_work
    type(ESMF_State)                       :: importState, exportState
    type(ESMF_Clock)                       :: clock
    type(ESMF_Time)                        :: currTime
    type(ESMF_TimeInterval)                :: timeStep
    type(ESMF_Time)                        :: startTime, stopTime
    type(ESMF_TimeInterval)                :: time_elapsed
    integer(ESMF_KIND_I8)                  :: n_interval, time_elapsed_sec
    character(len=64)                      :: timestamp
!
    integer :: na,i,j,i1,j1, urc
    logical :: lalarm, reconcileFlag
    character(len=*),parameter  :: subname='(fv3_cap:ModelAdvance)'
    character(240)              :: msgString
!jw debug
    character(ESMF_MAXSTR)      :: name
    type(ESMF_VM)               :: vm
    integer :: mype,date(6), fieldcount, fcst_nfld
    real(kind=ESMF_KIND_R4), pointer  :: dataPtr(:,:,:), dataPtr2d(:,:)
    character(64)  :: fcstbdl_name
    real(kind=8)   :: MPI_Wtime
    real(kind=8)   :: timeri, timewri, timewr, timerhi, timerh

!-----------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    if(profile_memory) call ESMF_VMLogMemInfo("Entering FV3 Model_ADVANCE: ")

    timeri = mpi_wtime()
!    
    call ESMF_GridCompGet(gcomp,name=name,vm=vm,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, localpet = mype,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.
    
    call ESMF_ClockPrint(clock_fv3, options="currTime", &
      preString="------>Advancing FV3 from: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(clock_fv3, startTime=startTime, currTime=currTime, &
      timeStep=timeStep, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!   if(mype==0)  print *,'total steps=', nint((stopTime-startTime)/timeStep)
!   if(mype==lead_wrttask(1))  print *,'on wrt lead,total steps=', nint((stopTime-startTime)/timeStep)
    call ESMF_TimeGet(time=stopTime,yy=date(1),mm=date(2),dd=date(3),h=date(4), &
                      m=date(5),s=date(6),rc=rc)
!    if(mype==0) print *,'af clock,stop date=',date
!    if(mype==lead_wrttask(1)) print *,'on wrt lead,af clock,stop date=',date
    call ESMF_TimeIntervalGet(timeStep,yy=date(1),mm=date(2),d=date(3),h=date(4), &
                      m=date(5),s=date(6),rc=rc)
!    if(mype==0) print *,'af clock,timestep date=',date
!    if(mype==lead_wrttask(1)) print *,'on wrt lead,af clock,timestep date=',date
    
    call ESMF_TimePrint(currTime + timeStep, &
      preString="--------------------------------> to: ", &
      unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!
!-----------------------------------------------------------------------------
!*** integration loop

    reconcileFlag = .true.
    call esmf_clockget(clock_fv3, timestep=timestep, starttime=starttime, &
                       currtime=currtime, rc=rc)

    integrate: do while(.NOT.ESMF_ClockIsStopTime(clock_fv3, rc = RC))
!
!*** for forecast tasks
     
      timewri = mpi_wtime()
      call ESMF_LogWrite('Model Advance: before fcstcomp run ', ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_GridCompRun(fcstComp, exportState=fcstState, clock=clock_fv3,userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_LogWrite('Model Advance: after fcstcomp run ', ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_ClockAdvance(clock = clock_fv3, rc = RC)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call esmf_clockget(clock_fv3, currtime=currtime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      time_elapsed  = currtime - starttime
      na = nint(time_elapsed/timeStep)
!
!    if(mype==0) print *,'in fv3_cap,in model run, advance,na=',na

!-------------------------------------------------------------------------------
!*** if alarms ring, call data transfer and write grid comp run
     if( quilting ) then

       lalarm = .false.
       if (nfhmax_hf > 0) then

         if(currtime <= starttime+output_hfmax) then
           if(ESMF_AlarmIsEnabled(alarm = ALARM_OUTPUT_HF, rc = RC)) then
             if( ESMF_AlarmIsRinging(alarm = ALARM_OUTPUT_HF,rc = Rc)) LALARM = .true.
           endif
         else
           if(ESMF_AlarmIsEnabled(alarm = ALARM_OUTPUT, rc = RC)) then
             if(ESMF_AlarmIsRinging(alarm = ALARM_OUTPUT,rc = Rc)) LALARM = .true.
           endif
         endif

       endif
!
       if(ESMF_AlarmIsEnabled(alarm = ALARM_OUTPUT, rc = RC)) then
         if(ESMF_AlarmIsRinging(alarm = ALARM_OUTPUT,rc = Rc)) LALARM = .true.
       endif
!      if (mype == 0 .or. mype == lead_wrttask(1)) print *,' aft fcst run lalarm=',lalarm, &
!      'FBcount=',FBcount,'na=',na

       output: IF(lalarm .or. na==1 ) then

         timerhi = mpi_wtime()
         do i=1, FBCount
!
! get fcst fieldbundle
!
           call ESMF_FieldBundleRegrid(fcstFB(i), wrtFB(i,n_group),    &
              routehandle=routehandle(i, n_group),                     &
              termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
           timerh = mpi_wtime()
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, &
             file=__FILE__)) &
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!end FBcount
          enddo
!       if (mype == 0 .or. mype == lead_wrttask(n_group)) print *,'aft fieldbundleregrid,na=',na,  &
!       ' time=', timerh- timerhi

!      if(mype==0 .or. mype==lead_wrttask(1))  print *,'on wrt bf wrt run, na=',na
          call ESMF_LogWrite('Model Advance: before wrtcomp run ', ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)

          timerhi = mpi_wtime()
          call ESMF_GridCompRun(wrtComp(n_group), importState=wrtState(n_group), clock=clock_fv3,userRc=urc,rc=rc)
          timerh = mpi_wtime()
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
          if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
!       if (mype == 0 .or. mype == lead_wrttask(n_group)) print *,'aft wrtgridcomp run,na=',na,  &
!        ' time=', timerh- timerhi

          call ESMF_LogWrite('Model Advance: after wrtcomp run ', ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            call ESMF_Finalize(endflag=ESMF_END_ABORT)

!       if (mype == 0 .or. mype == lead_wrttask(n_group)) print *,'fv3_cap,aft model advance,na=', &
!       na,' time=', mpi_wtime()- timewri


          if(n_group == write_groups) then
            n_group = 1
          else
            n_group = n_group + 1
          endif

        endif output

! end quilting
      endif

!      if (mype == 0 .or. mype == 1536 .or. mype==2160) then
!        print *,'fv3_cap,end integrate,na=',na,' time=',mpi_wtime()- timewri
!      endif

!*** end integreate loop
    enddo integrate
    print *,'fv3_cap,end integrate,na=',na,' time=',mpi_wtime()- timeri

    if(profile_memory) call ESMF_VMLogMemInfo("Leaving FV3 Model_ADVANCE: ")

  end subroutine ModelAdvance

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

  subroutine atmos_model_finalize(gcomp, rc)

    ! input arguments
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
!
    integer :: i, unit, date(6), mype, urc
    type(ESMF_VM)  :: vm
    real(kind=8) :: MPI_Wtime, timeffs
!
!-----------------------------------------------------------------------------
!*** finialize forecast

    timeffs = mpi_wtime()
    rc = ESMF_SUCCESS
!
    call ESMF_GridCompGet(gcomp,vm=vm,rc=rc)
    call ESMF_VMGet(vm, localpet = mype,rc=rc)
!
!*** finalize grid comps
    if( quilting ) then
      do i = 1, write_groups
        call ESMF_GridCompFinalize(wrtComp(i), importState=wrtstate(i),userRc=urc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      enddo
    endif

    call ESMF_GridCompFinalize(fcstComp, exportState=fcststate,userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!*** destroy grid comps
    if( quilting ) then
      do i = 1, write_groups
        call ESMF_StateDestroy(wrtState(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
        call ESMF_GridCompDestroy(wrtComp(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      enddo
    endif

    call ESMF_StateDestroy(fcstState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridCompDestroy(fcstComp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
    if(mype==0)print *,' wrt grid comp destroy time=',mpi_wtime()-timeffs


  end subroutine atmos_model_finalize

!#######################################################################
!
!-----------------------------------------------------------------------------

end module fv3gfs_cap_mod
