!-----------------------------------------------------------------------
!
  module module_fcst_grid_comp
!
!-----------------------------------------------------------------------
!***  Forecast gridded component.
!-----------------------------------------------------------------------
!***
!***  HISTORY   
!***
!       Apr 2017:  J. Wang  - initial code for forecast grid component
!
!---------------------------------------------------------------------------------
!
  use time_manager_mod,   only: time_type, set_calendar_type, set_time,    &
                                set_date, days_in_month, month_name,       &
                                operator(+), operator (<), operator (>),   &
                                operator (/=), operator (/), operator (==),&
                                operator (*), THIRTY_DAY_MONTHS, JULIAN,   &
                                NOLEAP, NO_CALENDAR, date_to_string,       &
                                get_date

  use  atmos_model_mod,   only: atmos_model_init, atmos_model_end,  &
                                update_atmos_model_dynamics,        &
                                update_atmos_radiation_physics,     &
                                update_atmos_model_state,           &
                                atmos_data_type, atmos_model_restart

  use constants_mod,      only: constants_init
  use       fms_mod,      only: open_namelist_file, file_exist, check_nml_error, &
                                error_mesg, fms_init, fms_end, close_file,       &
                                write_version_number, uppercase

  use mpp_mod,            only: mpp_init, mpp_pe, mpp_root_pe, mpp_npes, mpp_get_current_pelist, &
                                mpp_set_current_pelist, stdlog, mpp_error, NOTE, FATAL, WARNING
  use mpp_mod,            only: mpp_clock_id, mpp_clock_begin, mpp_clock_end, mpp_sync

  use mpp_io_mod,         only: mpp_open, mpp_close, MPP_NATIVE, MPP_RDONLY, MPP_DELETE

  use mpp_domains_mod,    only: mpp_get_global_domain, mpp_global_field, CORNER
  use memutils_mod,       only: print_memuse_stats
  use sat_vapor_pres_mod, only: sat_vapor_pres_init

  use diag_manager_mod,   only: diag_manager_init, diag_manager_end, &
                                get_base_date, diag_manager_set_time_end

  use data_override_mod,  only: data_override_init
  use fv_nggps_diags_mod, only: fv_dyn_bundle_setup
  use fv3gfs_io_mod,      only: fv_phys_bundle_setup

  use esmf
!
  use module_fv3_io_def, only:  num_pes_fcst, num_files, filename_base, nbdlphys
  use module_fv3_config, only:  dt_atmos, calendar, restart_interval, &
                                quilting, calendar_type,              &
                                force_date_from_configure
!
!-----------------------------------------------------------------------
!
  implicit none
!
  include 'mpif.h'
!
!-----------------------------------------------------------------------
!
  private
!
!---- model defined-types ----

  type atmos_internalstate_type
    type(atmos_data_type)  :: Atm
    type (time_type)       :: Time_atmos, Time_init, Time_end,  &
                              Time_step_atmos, Time_step_ocean, &
                              Time_restart, Time_step_restart
    integer :: num_atmos_calls, ret, intrm_rst
  end type

  type atmos_internalstate_wrapper
    type(atmos_internalstate_type), pointer :: ptr
  end type

  type(atmos_internalstate_type),pointer,save :: atm_int_state
  type(atmos_internalstate_wrapper),save      :: wrap
  type(ESMF_VM),save                          :: VM

!----- coupled model date -----

  integer :: date_init(6)
!
!-----------------------------------------------------------------------
!
  public SetServices
!
  contains
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
  subroutine SetServices(fcst_comp, rc)
!
    type(ESMF_GridComp)  :: fcst_comp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(fcst_comp, ESMF_METHOD_INITIALIZE, &
         userRoutine=fcst_initialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!
    call ESMF_GridCompSetEntryPoint(fcst_comp, ESMF_METHOD_RUN, &
         userRoutine=fcst_run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!
    call ESMF_GridCompSetEntryPoint(fcst_comp, ESMF_METHOD_FINALIZE, &
         userRoutine=fcst_finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine SetServices
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
  subroutine fcst_initialize(fcst_comp, importState, exportState, clock, rc)
!
!-----------------------------------------------------------------------
!***  INITIALIZE THE WRITE GRIDDED COMPONENT.
!-----------------------------------------------------------------------
!
    type(esmf_GridComp)                    :: fcst_comp
    type(ESMF_State)                       :: importState, exportState
    type(esmf_Clock)                       :: clock
    integer,intent(out)                    :: rc
!
!***  LOCAL VARIABLES
!
    integer                                :: tl, i, j
    integer,dimension(2,6)                 :: decomptile                  !define delayout for the 6 cubed-sphere tiles
    type(ESMF_FieldBundle)                 :: fieldbundle
    type(ESMF_Grid)                        :: fcstGrid
!
    type(ESMF_Time)                        :: CurrTime, TINI, StopTime
    type(ESMF_TimeInterval)                :: TINT, RunDuration, TimeElapsed
    type(ESMF_Config)                      :: cf

    integer                                :: Run_length
    integer,dimension(6)                   :: date, date_end
    integer                                :: res_intvl
    integer                                :: mpi_comm_comp
!
    logical,save                           :: first=.true.
    character(len=9) :: month
    integer :: initClock, unit, nfhour
    integer :: mype, ntasks
    character(3) cfhour
    character(4) dateSY
    character(2) dateSM,dateSD,dateSH,dateSN,dateSS
    character(128) name_FB, name_FB1, dateS
    real,    allocatable, dimension(:,:) :: glon_bnd, glat_bnd
    type(ESMF_FieldBundle),dimension(:), allocatable  :: fieldbundlephys

    real(8) mpi_wtime, timeis
!
!----------------------------------------------------------------------- 
!*********************************************************************** 
!----------------------------------------------------------------------- 
!
    timeis = mpi_wtime()
    rc     = ESMF_SUCCESS
!
!----------------------------------------------------------------------- 
!***  ALLOCATE THE WRITE COMPONENT'S INTERNAL STATE.
!----------------------------------------------------------------------- 
!
    allocate(atm_int_state,stat=rc)
!
!----------------------------------------------------------------------- 
!***  ATTACH THE INTERNAL STATE TO THE WRITE COMPONENT.
!----------------------------------------------------------------------- 
!
    wrap%ptr => atm_int_state
    call ESMF_GridCompSetInternalState(fcst_comp, wrap, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!
    call ESMF_VMGetCurrent(vm=VM,rc=RC)        
    call ESMF_VMGet(vm=VM, localPet=mype, mpiCommunicator=mpi_comm_comp, &
                    petCount=ntasks, rc=rc)
    if(mype==0) print *,'in fcst comp init, ntasks=',ntasks
!
    call fms_init(mpi_comm_comp)
    call mpp_init()
    initClock = mpp_clock_id( 'Initialization' )
    call mpp_clock_begin (initClock) !nesting problem

    call fms_init
    call constants_init
    call sat_vapor_pres_init
!
    if ( force_date_from_configure ) then

      select case( uppercase(trim(calendar)) )
      case( 'JULIAN' )
          calendar_type = JULIAN
      case( 'NOLEAP' )
          calendar_type = NOLEAP
      case( 'THIRTY_DAY' )
          calendar_type = THIRTY_DAY_MONTHS
      case( 'NO_CALENDAR' )
          calendar_type = NO_CALENDAR
      case default
          call mpp_error ( FATAL, 'COUPLER_MAIN: coupler_nml entry calendar must '// &
                                  'be one of JULIAN|NOLEAP|THIRTY_DAY|NO_CALENDAR.' )
      end select

    endif
!
    call set_calendar_type (calendar_type         )
!
!----------------------------------------------------------------------- 
!***  set atmos time
!----------------------------------------------------------------------- 
!
    call ESMF_ClockGet(clock, currTIME=CurrTime, rc=rc)
    call ESMF_ClockGet(clock, TimeStep=TINT, rc=rc)
    call ESMF_ClockGet(clock, StartTime=TINI, rc=rc)
    call ESMF_ClockGet(clock, RunDuration=RunDuration, rc=rc)
    call ESMF_ClockGet(clock, currTIME=CurrTime, TimeStep=TINT,      &
                       StartTime=TINI, RunDuration=RunDuration, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    date=0
    call ESMF_TimeGet (CurrTime,                    &
                       YY=date(1), MM=date(2), DD=date(3), &
                       H=date(4),  M =date(5), S =date(6), RC=rc )
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if(mype==0) print *,'CurrTime=',date

    atm_int_state%Time_atmos = set_date (date(1), date(2), date(3),  &
                            date(4), date(5), date(6))

    date_init=0
    call ESMF_TimeGet (TINI,                      &
                       YY=date_init(1), MM=date_init(2), DD=date_init(3), &
                       H=date_init(4),  M =date_init(5), S =date_init(6), RC=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if ( date_init(1) == 0 ) date_init = date
    atm_int_state%Time_init  = set_date (date_init(1), date_init(2), date_init(3), &
                            date_init(4), date_init(5), date_init(6))
    if(mype==0) print *,'InitTime=',date_init
!
    stopTime = CurrTime + RunDuration
    date_end=0
    call ESMF_TimeGet (StopTime,                      &
                       YY=date_end(1), MM=date_end(2), DD=date_end(3), &
                       H=date_end(4),  M =date_end(5), S =date_end(6), RC=rc )
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if ( date_end(1) == 0 ) date_end = date
    atm_int_state%Time_end   = set_date (date_end(1), date_end(2), date_end(3),  &
                            date_end(4), date_end(5), date_end(6))
    if(mype==0) print *,'StopTime=',date_end,'date=',date
!
    call diag_manager_set_time_end(atm_int_state%Time_end)
!
    CALL ESMF_TimeIntervalGet(RunDuration, S=Run_length, RC=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!
    call diag_manager_init (TIME_INIT=date)
    call diag_manager_set_time_end(atm_int_state%Time_end)
!
    atm_int_state%Time_step_atmos = set_time (dt_atmos,0)
    atm_int_state%num_atmos_calls = Run_length / dt_atmos
    if(mype==0) print *,'num_atmos_calls=',atm_int_state%num_atmos_calls,'time_init=', &
       date_init,'time_atmos=',date,'time_end=',date_end,'dt_atmos=',dt_atmos, &
       'Run_length=',Run_length
     res_intvl = restart_interval*3600
     atm_int_state%Time_step_restart = set_time (res_intvl, 0)
     atm_int_state%Time_restart = atm_int_state%Time_atmos + atm_int_state%Time_step_restart
     atm_int_state%intrm_rst = .false.
     if (res_intvl>0) atm_int_state%intrm_rst = .true.
!
!
!----- write time stamps (for start time and end time) ------

     call mpp_open( unit, 'time_stamp.out', nohdrs=.TRUE. )
     month = month_name(date(2))
     if ( mpp_pe() == mpp_root_pe() ) write (unit,20) date, month(1:3)
     month = month_name(date_end(2))
     if ( mpp_pe() == mpp_root_pe() ) write (unit,20) date_end, month(1:3)
     call mpp_close (unit)
 20  format (6i4,2x,a3)
!
!------ initialize component models ------

     call  atmos_model_init (atm_int_state%Atm,  atm_int_state%Time_init, &
                             atm_int_state%Time_atmos, atm_int_state%Time_step_atmos)
!
     call data_override_init ( ) ! Atm_domain_in  = Atm%domain, &
                                 ! Ice_domain_in  = Ice%domain, &
                                 ! Land_domain_in = Land%domain )

!-----------------------------------------------------------------------
!---- open and close dummy file in restart dir to check if dir exists --

      if (mpp_pe() == 0 ) then
         call mpp_open( unit, 'RESTART/file' )
         call mpp_close(unit, MPP_DELETE)
      endif
!
!
!-----------------------------------------------------------------------
!*** create grid for oupout fields
!*** first try: Create cubed sphere grid from file
!-----------------------------------------------------------------------
!
      if(mype==0) print *,'be create fcst grid'
      if( quilting ) then

        do tl=1,6
          decomptile(1,tl) = atm_int_state%Atm%layout(1)
          decomptile(2,tl) = atm_int_state%Atm%layout(2)
        enddo
        fcstGrid = ESMF_GridCreateMosaic(filename='INPUT/grid_spec.nc',      &
                              regDecompPTile=decomptile,tileFilePath='INPUT/',          &
                              staggerlocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
                              name='fcst_grid', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

! Add dimension Attributes to Grid
        call ESMF_AttributeAdd(fcstgrid, convention="NetCDF", purpose="FV3",  &
          attrList=(/"ESMF:gridded_dim_labels"/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_AttributeSet(fcstGrid, convention="NetCDF", purpose="FV3", &
          name="ESMF:gridded_dim_labels", valueList=(/"grid_xt", "grid_yt"/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
!
! Add time Attribute to the exportState
        call ESMF_AttributeAdd(exportState, convention="NetCDF", purpose="FV3", &
          attrList=(/"time        ","time:long_name","time:units    ",          &
          "time:cartesian_axis","time:calendar_type","time:calendar "/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_AttributeSet(exportState, convention="NetCDF", purpose="FV3", &
          name="time", value=real(0,ESMF_KIND_R8), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        write(dateSY,'(I4.4)')date_init(1)
        write(dateSM,'(I2.2)')date_init(2)
        write(dateSD,'(I2.2)')date_init(3)
        write(dateSH,'(I2.2)')date_init(4)
        write(dateSN,'(I2.2)')date_init(5)
        write(dateSS,'(I2.2)')date_init(6)
        dateS="hours since "//dateSY//'-'//dateSM//'-'//dateSD//' '//dateSH//':'//    &
            dateSN//":"//dateSS
        if(mype==0) print *,'dateS=',trim(dateS),'date_init=',date_init
        call ESMF_AttributeSet(exportState, convention="NetCDF", purpose="FV3", &
          name="time:units", value=trim(dateS), rc=rc)
!          name="time:units", value="hours since 2016-10-03 00:00:00", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
        call ESMF_AttributeSet(exportState, convention="NetCDF", purpose="FV3", &
          name="time:long_name", value="time", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_AttributeSet(exportState, convention="NetCDF", purpose="FV3", &
          name="time:cartesian_axis", value="T", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_AttributeSet(exportState, convention="NetCDF", purpose="FV3", &
          name="time:calendar_type", value="JULIAN", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_AttributeSet(exportState, convention="NetCDF", purpose="FV3", &
          name="time:calendar", value="JULIAN", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
!
! Create FieldBundle for Fields that need to be regridded bilinear

        do i=1,num_files
!
         name_FB = filename_base(i)
!
         if( i==1 ) then
! for dyn
           name_FB1 = trim(name_FB)//'_bilinear'
           fieldbundle = ESMF_FieldBundleCreate(name=trim(name_FB1),rc=rc)
           if(mype==0) print *,'af create fcst fieldbundle, name=',trim(name_FB),'rc=',rc
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, &
             file=__FILE__)) &
             return  ! bail out
           call fv_dyn_bundle_setup(atm_int_state%Atm%axes,          &
                fieldbundle, fcstgrid, quilting)

           ! Add the field to the importState so parent can connect to it
           call ESMF_StateAdd(exportState, (/fieldbundle/), rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, &
             file=__FILE__)) &
             return  ! bail out

         else if( i==2 ) then
! for phys
           nbdlphys = 2
           allocate(fieldbundlephys(nbdlphys))
           do j=1, nbdlphys
             if( j==1 ) then
               name_FB1 = trim(name_FB)//'_nearest_stod'
             else
               name_FB1 = trim(name_FB)//'_bilinear'
             endif
             fieldbundlephys(j) = ESMF_FieldBundleCreate(name=trim(name_FB1),rc=rc)
             if(mype==0) print *,'af create fcst fieldbundle, name=',trim(name_FB1),'rc=',rc
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out
           enddo
!
           call fv_phys_bundle_setup(atm_int_state%Atm%axes,          &
                fieldbundlephys, fcstgrid, quilting, nbdlphys)
!
           ! Add the field to the importState so parent can connect to it
           do j=1,nbdlphys
             call ESMF_StateAdd(exportState, (/fieldbundlephys(j)/), rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out
           enddo

         endif
!
        enddo

!end qulting
      endif
!
!-----------------------------------------------------------------------
!
      IF(rc /= ESMF_SUCCESS) THEN
        WRITE(0,*)"FAIL: Fcst_Initialize."
!      ELSE
!        WRITE(0,*)"PASS: Fcst_Initialize."
      ENDIF
!
      if(mype==0) print *,'in fcst,init total time: ', mpi_wtime() - timeis
!
!----------------------------------------------------------------------- 
!
   end subroutine fcst_initialize
!
!----------------------------------------------------------------------- 
!####################################################################### 
!----------------------------------------------------------------------- 
!
   subroutine fcst_run(fcst_comp, importState, exportState,clock,rc)
!
!----------------------------------------------------------------------- 
!***  the run step for the fcst gridded component.  
!----------------------------------------------------------------------- 
!
      type(ESMF_GridComp)        :: fcst_comp
      type(ESMF_State)           :: importState, exportState
      type(ESMF_Clock)           :: clock
      integer,intent(out)        :: rc
!
!-----------------------------------------------------------------------
!***  local variables
!
      type(ESMF_FieldBundle)     :: file_bundle 
!
      integer                    :: i,j, mype, na, date(6)
      character(20)              :: compname

      type(ESMF_Time)            :: currtime
      integer(kind=ESMF_KIND_I8) :: ntimestep_esmf
      character(len=64)          :: timestamp
!
!-----------------------------------------------------------------------
!
      real(kind=8)   :: mpi_wtime, tbeg1
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      tbeg1 = mpi_wtime()
      rc     = esmf_success
!
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(fcst_comp, name=compname, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!
      call ESMF_VMGetCurrent(VM,rc=RC)
!
      call ESMF_VMGet(VM, localpet=mype,rc=rc)
      call ESMF_ClockGet(clock, advanceCount=NTIMESTEP_ESMF, rc=rc)   
      
      na = NTIMESTEP_ESMF
!     if(mype==0) print *,'in fcst run, na=',na
!
!-----------------------------------------------------------------------
! *** call fcst integration subroutines  

      call get_date (atm_int_state%Time_atmos, date(1), date(2), date(3),  &
                               date(4), date(5), date(6))
      atm_int_state%Time_atmos = atm_int_state%Time_atmos + atm_int_state%Time_step_atmos

      call update_atmos_model_dynamics (atm_int_state%Atm)

      call update_atmos_radiation_physics (atm_int_state%Atm)

      call update_atmos_model_state (atm_int_state%Atm)

!--- intermediate restart
      if (atm_int_state%intrm_rst) then
        if ((na /= atm_int_state%num_atmos_calls) .and.   &
           (atm_int_state%Time_atmos == atm_int_state%Time_restart)) then
          timestamp = date_to_string (atm_int_state%Time_restart)
          call atmos_model_restart(atm_int_state%Atm, timestamp)

          call wrt_atmres_timestamp(atm_int_state,timestamp)
          atm_int_state%Time_restart = atm_int_state%Time_restart + atm_int_state%Time_step_restart
        endif
      endif
!
      call print_memuse_stats('after full step')
!
!-----------------------------------------------------------------------
!
      IF(RC /= ESMF_SUCCESS) THEN
        WRITE(0,*)"FAIL: fcst_RUN"
!      ELSE
!        WRITE(0,*)"PASS: fcstRUN, na=",na
      ENDIF
!
!     if(mype==0) print *,'fcst _run time is ', mpi_wtime()-tbeg1
!
!-----------------------------------------------------------------------
!
   end subroutine fcst_run
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
   subroutine fcst_finalize(fcst_comp, importState, exportState,clock,rc)
!
!-----------------------------------------------------------------------
!***  finalize the forecast grid component.
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp)            :: fcst_comp
      type(ESMF_State)               :: importState, exportState
      type(ESMF_Clock)               :: clock
      integer,intent(out)            :: rc
!
!***  local variables
!
      integer :: unit
      integer,dimension(6)           :: date
      
      real(8) mpi_wtime, tfs, tfe
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      tfs = mpi_wtime()
      rc  = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  retrieve the fcst component's esmf internal state
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGetInternalState(fcst_comp, wrap, rc)  
      atm_int_state => wrap%ptr   
!
!-----------------------------------------------------------------------
!
      call atmos_model_end (atm_int_state%atm)
!
!*** check time versus expected ending time

      if (atm_int_state%Time_atmos /= atm_int_state%Time_end)  &
        call error_mesg ('program coupler',  &
           'final time does not match expected ending time', WARNING)

!*** write restart file

      call get_date (atm_int_state%Time_atmos, date(1), date(2), date(3),  &
                               date(4), date(5), date(6))
      call mpp_open( unit, 'RESTART/coupler.res', nohdrs=.TRUE. )
      if (mpp_pe() == mpp_root_pe())then
         write( unit, '(i6,8x,a)' )calendar_type, &
              '(Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)'

         write( unit, '(6i6,8x,a)' )date_init, &
              'Model start time:   year, month, day, hour, minute, second'
         write( unit, '(6i6,8x,a)' )date, &
              'Current model time: year, month, day, hour, minute, second'
      endif
      call mpp_close(unit)
!
      call diag_manager_end(atm_int_state%Time_atmos )

      call fms_end
!
!-----------------------------------------------------------------------
!
      IF(RC /= ESMF_SUCCESS)THEN
        WRITE(0,*)'FAIL: Write_Finalize.'
!      ELSE
!        WRITE(0,*)'PASS: Write_Finalize.'
      ENDIF
!
      tfe = mpi_wtime()
!      print *,'fms end time: ', tfe-tfs
!-----------------------------------------------------------------------
!
  end subroutine fcst_finalize
!
!#######################################################################
!-- change name from coupler_res to wrt_res_stamp to avoid confusion,
!-- here we only write out atmos restart time stamp
!
  subroutine wrt_atmres_timestamp(atm_int_state,timestamp)
    type(atmos_internalstate_type), intent(in) :: atm_int_state
    character(len=32), intent(in) :: timestamp

    integer :: unit, date(6)

!----- compute current date ------

    call get_date (atm_int_state%Time_atmos, date(1), date(2), date(3),  &
                                 date(4), date(5), date(6))

!----- write restart file ------

    if (mpp_pe() == mpp_root_pe())then
        call mpp_open( unit, 'RESTART/'//trim(timestamp)//'.coupler.res', nohdrs=.TRUE. )
        write( unit, '(i6,8x,a)' )calendar_type, &
             '(Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)'

        write( unit, '(6i6,8x,a)' )date_init, &
             'Model start time:   year, month, day, hour, minute, second'
        write( unit, '(6i6,8x,a)' )date, &
             'Current model time: year, month, day, hour, minute, second'
        call mpp_close(unit)
    endif
  end subroutine wrt_atmres_timestamp
!
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
end module  module_fcst_grid_comp
!
!----------------------------------------------------------------------------
