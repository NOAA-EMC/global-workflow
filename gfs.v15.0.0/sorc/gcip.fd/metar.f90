!------------------------------------------------------------------------------
!
! MODULE: Metar
!
! DESCRIPTION:
!> Read METAR/SPECI or SHIPS data from dump bufr
!
! REVISION HISTORY:
! September 2011                                                                                  
! January 2014 - modified
!
! Notes: Comparing to Metar, there are two different ways to decode Ships
!        reports.
!    (1) station type is manual, the result means ship reports provide all kinds
!        of weather, no limitation.
!        Unlike most ASOS Metar stations, which can not detect freezing drizzle. 
!    (2) cloud coverage and cloud base. In practice, ships seldom provide
!        layered cloud information. But it provides the lowest cloud amount(*)
!        and the range of cloud base.
!        So cloud coverage is set:
!          -- as is if the total cloud coverage is clear or few
!          -- as is if the lowest cloud amount is SCT (**) or more
!          -- as missing if the lowest cloud amount is FEW or less, since there
!             is no way to tell whether there are more clouds at higher levels.
!        And set cloud base in middle of the following range:                  
!                      0 -- 0 to 50 m
!                      1 -- 50 to 100 m
!                      2 -- 100 to 200 m
!                      3 -- 200 to 300 m
!                      4 -- 300 to 600 m
!                      5 -- 600 to 1000 m
!                      6 -- 1000 to 1500 m
!                      7 -- 1500 to 2000 m
!                      8 -- 2000 to 2500 m
!                      9 -- above 2500 m
!                      / -- unknown !
! if there are only high clouds, no low or middle clouds, the lowest cloud
! amount is used in two places in icing algorithm, sat_cld() and baseK()
!   in baseK(), no issue
!   in sat_cld(), there is no difference setting cloud coverage is SCT or
!   NONE/MISSING when the lowest cloud amount is SCT in m_checkCloudCoverSHIPs()
!
!------------------------------------------------------------------------------                    

module Metar

  use Kinds
  use Config
  use GridTemplate

  implicit none

  private
  public runMetar

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! Enumerations
  type :: m_cloud_coverage_t
     integer :: NONE=0
     integer :: CLR=1
     integer :: FEW=2
     integer :: SCT=3
     integer :: BKN=4
     integer :: OVC=5
     integer :: XOB=6
  end type m_cloud_coverage_t
  type(m_cloud_coverage_t), parameter :: Cloud_Coverage = m_cloud_coverage_t()

  type :: m_station_type_t
     integer :: UNKNOWN=0
     integer :: AWOS=1
     integer :: ASOS=2
     integer :: MANUAL=3
     integer :: SHIPS = 4
  end type m_station_type_t
  type(m_station_type_t), parameter :: Station_Types = m_station_type_t()

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! Structures
  type :: metar_component_t
     character(len=4) :: stationName
     real :: latitude
     real :: longitude
     real :: elevation
     integer(kind=BYTE8) :: obsTime ! minutes since julian day 00:00
     integer :: stationType   ! be one of Station_Types
     integer :: cloudCoverage ! be one of Cloud_Coverage
     real    :: cloudBase
     logical :: isFreezingDrizzle
     logical :: isFreezingRain
     logical :: isSnow
     logical :: isRain
     logical :: isDrizzle
     logical :: isIcePellets
     logical :: isThunder
     logical :: isFog
     logical :: isPresentWxSensorOk
     logical :: isFreezingRainSensorOk
     logical :: isThunderSensorOk
     !
     real :: distance ! between a station and a grid point; set when gridding
  end type metar_component_t

  ! Data structure: stack
  !               : %next==NULL means empty, the first element is useless.
  type :: stack_station_t
     type(metar_component_t) :: metar 
     type(stack_station_t), pointer :: next
  end type stack_station_t

  type :: MetarGridPoint_t
     logical :: hasMetar
     ! The array holds a copy of observations, organized by ringed distance
     ! between a gridpoint to its nearby observations.
     type(stack_station_t), allocatable :: ringedMetars(:)
  end type MetarGridPoint_t

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! private members (configration related)
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  real :: CFG_RadiusOfInfluence ! = 125.0  Metar station radius of influence (km)
  ! Processing begins with distances from closest to farthest.
  ! This approach effectively treats all Metars within a given processing bin
  ! equally with respect to distance.
  real, pointer :: CFG_ProcessingLimits(:)	! = (/ 40., 60., 80., 100., 125. /)
  integer :: CFG_NumProcessingRanges		!= 5
  integer :: CFG_MinNumMetars
  integer, pointer :: CFG_TimeWindow(:) ! data collecting time window, in minutes

contains

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> public subroutine to be called to read, translate and grid Metar/Ship data
  !
  !> @param[in] filename   - input bufr file name
  !> @param[in] iruntime   - array of run time, YYYY MM DD ZZ HH MM SS MS
  !> @param[in] cfg        - configuration file
  !> @param[in] kgds       - model grid information
  !> @param[out] metarData2D - translated and gridded Metar data output
  !> @param[out] iret        - status; -1 if failure
  !----------------------------------------------------------------------------

  subroutine runMetar(filename, iruntime, cfg, kgds, metarData2D, iret)
    implicit none
    character(len=*),  intent(in) :: filename
    ! YYYY MM DD ZZ HH MM SS MS to call /nwprod/lib/sorc/w3nco/w3movdat.f
    integer, intent(in) :: iruntime(8) 
    type(config_t), target :: cfg
    integer, intent(in) :: kgds(:)
    type(metar_data_t), allocatable, intent(out) :: metarData2D(:,:)
    integer, intent(out) :: iret

    type(stack_station_t) :: stations
    integer :: nstations

    ! configuration fields
    CFG_RadiusOfInfluence = cfg%metar% radiusOfInfluence
    CFG_ProcessingLimits => cfg%metar% weatherProcessingLimits
    CFG_NumProcessingRanges = cfg%metar% weatherProcessingLimits_n
    CFG_MinNumMetars = cfg%metar% minNumMetars
    CFG_TimeWindow => cfg%metar% timeWindow

    iret = -1
print *, filename, iruntime, CFG_TimeWindow
    nstations =  getStationMetars(filename, iruntime, CFG_TimeWindow, stations)
    if(nstations < CFG_MinNumMetars) then
      write(*,*) "There are not enough surface/SHIP observations", nstations
      call m_freeStationStack(stations)
      return
    end if

write(*,*) "getStationMetars=", nstations

    call gridpointMetar(kgds, CFG_RadiusOfInfluence, stations, metarData2D, iret)

    ! release memory of stations
    call m_freeStationStack(stations)

    return
  end subroutine runMetar

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Read Metar data from BUFR dump file into stations (stack, unique elements).
  !> Collect data in [-30, +30] minute time window of the run hour.
  !> The stations are unique with observation time closest to run_time
  !
  !> @param[in] infile     - input bufr file name
  !> @param[in] iruntime   - array of run time, YYYY MM DD ZZ HH MM SS MS
  !> @param[in] timeWindow - data collecting time window, in minutes
  !> @param[out] stations
  !> @Return: the number of stations/observations; -1 if failure
  !----------------------------------------------------------------------------

  integer function getStationMetars(infile, iruntime, timeWindow, stations)
    implicit none
    character(len=*),  intent(in) :: infile
    ! YYYY MM DD ZZ HH MM SS MS to call /nwprod/lib/sorc/w3nco/w3movdat.f
    integer, intent(in) :: iruntime(8)
    integer, intent(in), pointer :: timeWindow(:)  ! in minutes
    type(stack_station_t), target, intent(out) :: stations

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! BUFR related variables
    integer :: ireadmg, ireadsb ! bufr lib functions
    integer :: unit            ! bufr file unit
    ! BUFR message information
    integer(kind=BYTE8) :: report_date
    character(len=BYTE8) :: msgtype
    ! For station information (including temperature)
    ! SELV - station height, in meter
    character(60), target :: headerStringMetar = 'RPID YEAR MNTH DAYS HOUR MINU CLAT CLON SELV TMDB AUTO'
    ! HBLCS - http://www.nco.ncep.noaa.gov/sib/decoders/dclsfc/intf/
    character(60), target :: headerStringShips = 'RPID YEAR MNTH DAYS HOUR MINU CLAT CLON SELV TMDB TOCC HBLCS'
    character(60), pointer :: headerString
    real(kind=BYTE8) :: headers(12)
    character(len=BYTE8) :: stationName
    equivalence(stationName, headers(1))
    integer :: ireport_time(5) ! YYYY MM DD HH MM
    integer :: stationType
    ! For clouds information
    real(kind=BYTE8) :: clouds(2, 10)
    ! For present weather
    real(kind=BYTE8) :: prwes(10)
    ! For raw report, for more decoding purposes
    real(kind=BYTE8) :: raw(BUFR_REPORT_LENGTH/BYTE8)
    character(len=BUFR_REPORT_LENGTH) :: rawReport
    equivalence(rawReport, raw)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! other variables
    type(stack_station_t), pointer :: stationPointer
    type(metar_component_t) :: aRecord
    integer :: iret, i
    integer :: mvtime(8) ! YYYY MM DD ZZ HH MM SS MS
    real    :: rinc(5)  ! interval (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
    character(len=12) :: start_time, end_time, report_time  ! YYYYMMDDHHMM
    integer(kind=BYTE8) :: lreport_time, lrun_time
    logical :: matching

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! For translation 
    character(5) :: whichReport ! either METAR or SHIPS
    real :: temperature
    ! For ships reports
    integer :: cloudAmountTotal
    integer :: cloudBaseCode
    integer, dimension(10) :: cloudAmounts
    real, dimension(10) :: cloudBaseHeights
    integer :: cloudLevels
    integer, dimension(10) :: iprwes             ! present weather in integer
    integer :: prweNum                           ! how many present weather
    integer :: rawLength

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! functions outside 
    integer :: IW3JDN

    headers = BUFR_MISSING
    clouds  = BUFR_MISSING

    getStationMetars = 0 ! no metar observations 
    nullify(stations%next) 

    ! convert run time to minutes since julian day 00:00
    lrun_time = IW3JDN(iruntime(1), iruntime(2), iruntime(3))
    lrun_time = lrun_time*24*60 + iruntime(5)*60 + iruntime(6)
    ! Convert time window to YYYYMMDDHHMM
    rinc(:) = 0.
    rinc(3) = timeWindow(1)
    call W3MOVDAT(rinc, iruntime, mvtime)
    write(start_time, "((I4,4I2.2))") mvtime(1), mvtime(2), mvtime(3), mvtime(5), mvtime(6)
    rinc(3) = timeWindow(2)
    call W3MOVDAT(rinc, iruntime, mvtime)
    write(end_time, "((I4,4I2.2))") mvtime(1), mvtime(2), mvtime(3), mvtime(5), mvtime(6)

    unit = 12
    call closbf(unit)
    open(unit,file=infile,form='unformatted', iostat=iret)
    if(iret /= 0) then
       getStationMetars = -1
       write(*,*) "Metar::getStationMetars() not able to open input BUFR file=", trim(infile)
       return
    end if
    call openbf(unit,'IN',unit)
    call datelen(10)

    loop_msg: do while(ireadmg(unit, msgtype, report_date) == 0)
      if(msgtype == 'NC000007') then
        headerString => headerStringMetar
        whichReport = "METAR"
      else if (msgtype == 'NC001001') then
        headerString => headerStringShips
        whichReport = "SHIPS"
      else
        cycle
      end if

      loop_readsb: do while(ireadsb(unit) == 0)

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        ! extract information
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

        ! station ID, time, latitude, longitude, elevation, temperature, station type
        !                time=(YEAR MNTH DAYS HOUR MINU)          
        ! headerString = 'RPID YEAR MNTH DAYS HOUR MINU CLAT CLON SELV TMDB AUTO'

        ! station type:
        ! http://www.emc.ncep.noaa.gov/mmb/data_processing/table_local_await-val.htm#0-02-194
        !0       Either SAO report or METAR/SPECI report, and 'AUTO' found in report
        !1       METAR/SPECI report with both 'AUTO' and 'A01' found in report
        !2       METAR/SPECI report with both 'AUTO' and 'A02' found in report
        !3       METAR/SPECI report with 'A01' found in report, but 'AUTO' not found in report
        !4       METAR/SPECI report with 'A02' found in report, but 'AUTO' not found in report

        call ufbint(unit, headers, size(headers), 1, iret, headerString)
        ! only collect METAR data in [-30, 30] minutes of run time
        ireport_time(1:5) = headers(2:6)
        write(report_time, "((I4,4I2.2))") (ireport_time(i), i=1,5) 
!print *, report_time, ireport_time, start_time, end_time
        if(report_time < start_time .or. report_time > end_time) cycle
!write(*,*) "report time ", report_time, " ",  start_time, " ", end_time   

        ! convert report time to minutes since julian day 00:00
        lreport_time = IW3JDN(ireport_time(1), ireport_time(2), ireport_time(3))
        lreport_time = lreport_time*24*60 + ireport_time(4)*60 + ireport_time(5)

        ! cloud amount and cloud base height (in meter)
        ! HOCB - cloud base height, in meter
        call ufbint(unit, clouds, 2, 10, cloudLevels, 'CLAM HOCB')

        ! present weather
        call ufbint(unit, prwes, 1, 10, prweNum, 'PRWE')

        ! raw report
        call ufbint(unit, raw, 1, size(raw), rawLength, 'RRSTG')

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        ! do some basic assignments
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        aRecord%stationName = trim(ADJUSTL(stationName))
        aRecord%latitude    = headers(7)
        aRecord%longitude   = headers(8)
        aRecord%elevation   = headers(9)
        aRecord%obsTime     = lreport_time
        temperature         = headers(10)

        ! if very basic station information is incomplete, do not continue
        if(len(aRecord%stationName) == 0 .or. aRecord%latitude > BUFR_MISSING .or. &
           aRecord%longitude > BUFR_MISSING .or. aRecord%elevation  > BUFR_MISSING) cycle
        !
        if(msgtype == 'NC000007') then
           if(headers(11) < BUFR_MISSING -100) then
              stationType = headers(11)
           else
              stationType = 10
           end if
           if(stationType == 1 .or. stationType== 3) then
              aRecord%stationType = Station_Types%AWOS
           else if(stationType == 2 .or. stationType== 4) then
              aRecord%stationType = Station_Types%ASOS
           else
              aRecord%stationType = Station_Types%MANUAL
           end if
        else if (msgtype == 'NC001001') then
           if(headers(11) < BUFR_MISSING -100) then
              cloudAmountTotal = nint(headers(11))
           else
              cloudAmountTotal = -1
           end if
           if(cloudAmountTotal > 0 .and. headers(12)< BUFR_MISSING -100) then
              cloudBaseCode = nint(headers(12))
           else
              cloudBaseCode = -9
           end if
           ! Suppose manual
           ! Means all ships provide all kinds of weathers
           aRecord%stationType = Station_Types%MANUAL
        end if

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        ! translate observation
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        ! First assign local variables to data members.
        ! Meanwhile, assign real variables to integers.
        ! The assignments do not consider the array size, 
        ! which will be considered in the tranlation.
        do i = 1, cloudLevels
           if(clouds(1,i)< BUFR_MISSING-100) then
              cloudAmounts = nint(clouds(1, i))
           end if
           cloudBaseHeights = clouds(2, i)
        end do
        do i = 1, 10
           if(prwes(i) < BUFR_MISSING-100) then
              iprwes(i) = prwes(i) ! real => integer
           else
              iprwes(i) = -1
           end if
        end do
        call m_translateObservation(whichReport, iprwes, prweNum, rawReport,&
                       temperature, cloudAmountTotal, cloudBaseCode, &
                       cloudAmounts, cloudBaseHeights, cloudLevels, aRecord)

!call printMetarComponent(aRecord)
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        ! Add/update aRecord to stations stack (by the closest time of run time)
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        ! skip the first useless element
        stationPointer => stations%next

        ! step 1
        ! Determine if this station already has an observation in the stations
        ! stack, by looping through the stack and comparing stations' name.
        matching = .false.
        loop_station: do while(associated(stationPointer))
          if(trim(ADJUSTL(stationName)) == trim(stationPointer%metar%stationName)) then
            matching = .true.
            exit
          end if
          stationPointer => stationPointer%next
        end do loop_station

        ! step 2
        ! Take different actions according to matching or not
        if (matching) then
          ! ----------------- YES --------------------------
          ! update the record if this observation closer to run_time
          if(abs(lreport_time - lrun_time) < abs(stationPointer%metar%obsTime - lrun_time)) then
             stationPointer%metar = aRecord
!write(*,*) stationPointer%metar%stationName, ", updated"
!else
!write(*,*) stationPointer%metar%stationName, ", no updated"
          end if
        else
          ! ----------------- NO ---------------------------
          ! This station has no observation in the stations stack yet, add it
          call m_addMetar2Stack(aRecord, stations)

          getStationMetars = getStationMetars + 1

        end if
 
!write(*,*) trim(m_rawReport)
!write(*,*) m_cloudBaseHeights
!write(*,*) m_cloudAmounts

      end do loop_readsb

    end do loop_msg

stationPointer => stations%next
!i = 0
!do while( associated(stationPointer))
!i = i + 1
!stationPointer => stationPointer%next
!end do
!write(*,*) i, " verified"


    close(unit)
    return
  end function getStationMetars

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Add a Metar/Ship observation to a (stack_station_t) stack
  !
  !> @param[in]    metar - an observation element to be added
  !> @param[inout] stack - the stack_station_t stack
  !----------------------------------------------------------------------------

  subroutine m_addMetar2Stack(metar, stack)
    implicit none
    type(metar_component_t), intent(in) :: metar
    type(stack_station_t), intent(inout) :: stack

    type(stack_station_t), pointer :: stackPointer

    allocate(stackPointer)
    stackPointer%metar = metar
    stackPointer%next => stack%next
    stack%next        => stackPointer

    return
  end subroutine m_addMetar2Stack

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Free the memory allocated for a stack_station_t object 
  !
  !> @param[inout] stack - the stack_station_t stack
  !----------------------------------------------------------------------------

  subroutine m_freeStationStack(stack)
    type(stack_station_t), target, intent(inout) :: stack

    type(stack_station_t), pointer :: iterator

    ! Skip the very first element which is a useless varaible
    iterator => stack%next
    do while(associated(iterator))
      iterator => iterator%next
      deallocate(stack%next)
      stack%next => iterator
    end do

    return
  end subroutine m_freeStationStack

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Decode BUFR metar code, translate cloud information and present weather.
  !
  !> @param[in] whichReport        - either METAR or SHIP
  !> @param[in] iprwes             - present weather code
  !> @param[in] prweNum            - how many present weather codes
  !> @param[in] rawReport          - raw report
  !> @param[in] temperature
  !> @param[in] cloudAmountTotal   - total cloud amount, for SHIP only
  !> @param[in] cloudBaseCode      - cloud base code, for SHIP only
  !> @param[in] cloudAmounts       - array of cloud amount code at levels
  !> @param[in] cloudBaseHeights   - array of cloud base height at levels
  !> @param[in] cloudLevels        - how many cloud levels
  !> @param[inout] aRecord         - decoded/translated metar observation
  !----------------------------------------------------------------------------

  subroutine m_translateObservation(whichReport, iprwes, prweNum, rawReport,&
                       temperature, cloudAmountTotal, cloudBaseCode, &
                       cloudAmounts, cloudBaseHeights, cloudLevels, aRecord)
    implicit none
    character(len=*), intent(in) :: whichReport ! either METAR or SHIPS
    integer, dimension(10), intent(in) :: iprwes ! present weather in integer
    integer, intent(in) :: prweNum              ! how many present weather
    character(len=*), intent(in) :: rawReport
    real, intent(in) :: temperature
    ! For ships reports
    integer, intent(in) :: cloudAmountTotal
    integer, intent(in) :: cloudBaseCode
    integer, dimension(10), intent(in) :: cloudAmounts
    real, dimension(10), intent(in) :: cloudBaseHeights
    integer, intent(in) :: cloudLevels
    type(metar_component_t), intent(inout) :: aRecord

    logical :: freezePrecipFound

    freezePrecipFound = .false.

    !For BUFR, no need to convert from feet to meters
    !metar = foot * 0.3048

    ! reset variables to defaults before assignments
    aRecord% cloudBase         = MISSING
    aRecord% cloudCoverage     = Cloud_Coverage%NONE ! be one of Cloud_Coverage
    aRecord% isFreezingDrizzle = .false.
    aRecord% isFreezingRain    = .false.
    aRecord% isSnow            = .false.
    aRecord% isRain            = .false.
    aRecord% isDrizzle         = .false.
    aRecord% isIcePellets      = .false.
    aRecord% isThunder         = .false.
    aRecord% isFog             = .false.

    ! for automated stations, check instruments operable or not
    call m_checkSensorsOK(rawReport, aRecord)

    call m_checkWeathers(iprwes, prweNum, freezePrecipFound, aRecord)

    ! relies on m_isFog, so must be after m_checkWeathers()
    if(whichReport == "METAR") then
      call m_checkCloudCoverMetar(cloudAmounts, cloudBaseHeights, cloudLevels, aRecord)
    else if (whichReport == "SHIPS") then
       call m_checkCloudCoverSHIPs(cloudAmounts, cloudAmountTotal, cloudBaseCode, cloudLevels, aRecord)
    end if

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Modify present weather type.

    if(aRecord%isFreezingRain) then
       if(aRecord%isFreezingRainSensorOk) then
          freezePrecipFound = .true.
          aRecord%isFreezingRain = .true.
       else
          freezePrecipFound = .false.
          aRecord%isFreezingRain = .false.
       end if
    end if

    if(aRecord%isFreezingDrizzle) then
       if( aRecord%isFreezingRainSensorOk) then
          freezePrecipFound = .true.
          aRecord%isFreezingDrizzle = .true.
       else
          freezePrecipFound = .false.
          aRecord%isFreezingDrizzle = .false.
       end if
    end if

    if(aRecord%isRain) then
       if(temperature <= -1.0) then
          if(freezePrecipFound .or. aRecord%cloudCoverage>= Cloud_Coverage%OVC) then
             aRecord%isFreezingRain = .true.
             aRecord%isRain = .false.
          end if
       end if
    end if

    if(aRecord%isIcePellets) then
       if(temperature <= 5.0 .or. temperature > BUFR_MISSING) then
          aRecord%isIcePellets = .true.
       else
          aRecord%isIcePellets = .false.
       end if
    end if

    if(aRecord%isSnow) then
       if(temperature <= 5.0 .or. temperature > BUFR_MISSING) then
          aRecord%isSnow = .true.
       else
          aRecord%isSnow = .false.
       end if
    end if

    if(aRecord%isDrizzle) then
       if(temperature <= -1.0) then
          if(freezePrecipFound .or. aRecord%cloudCoverage>= Cloud_Coverage%OVC) then
             aRecord%isFreezingDrizzle = .true.
             aRecord%isDrizzle = .false.
          end if
       end if
    end if

    if(aRecord%isThunder) then
       if(.not.  aRecord%isThunderSensorOk) aRecord%isThunder = .false.
    end if

    return
  end subroutine m_translateObservation 


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> check senors  OK or not and update information of the observation
  !
  !> @param[in]  rawReport - raw Metar/Ship report in ASCII
  !> @param[inout] aRecord   - an observation record to be updated
  !----------------------------------------------------------------------------

  subroutine m_checkSensorsOK(rawReport, aRecord)
    implicit none
    character(len=*), intent(in) :: rawReport
    type(metar_component_t), intent(inout) :: aRecord

    ! look through remark for 'FZRANO', 'PWINO' and 'TSNO' which
    ! indicate that the freezing rain, present weather info, and thunder
    ! detect instruments are present, but not operable.
    !
    aRecord% isPresentWxSensorOk = .true. ! always true, not change
    aRecord% isFreezingRainSensorOk = .true.
    aRecord% isThunderSensorOk = .true.

    ! skip station name to check
    if(index(rawReport(5:), "FZRANO") /= 0) aRecord%isFreezingRainSensorOk = .false.
    if(index(rawReport(5:), "TSNO") /= 0)   aRecord%isThunderSensorOk = .false.
    if(index(rawReport(5:), "PWINO") /= 0)  aRecord%isThunderSensorOk = .false.

    return
  end subroutine m_checkSensorsOK

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> METAR: Look through all cloud layers and locate the densest layer as the
  !> cloud coverage and locate the lowest (SCT or more) layer as the cloud base
  !
  !> @param[in] amounts       - array of cloud amount code at levels
  !> @param[in] baseHeights   - array of cloud base height at levels
  !> @param[in] levels        - how many cloud levels
  !> @param[out] aRecord      - decoded/translated metar observation
  !----------------------------------------------------------------------------
 
  subroutine m_checkCloudCoverMetar(amounts, baseHeights, levels, aRecord)
    implicit none
    integer, dimension(10), intent(in) :: amounts
    real, dimension(10), intent(in) :: baseHeights
    integer, intent(in) :: levels
    type(metar_component_t), intent(inout) :: aRecord

    logical :: base_not_found
    integer :: i, coverInt

    base_not_found = .true.

    ! 1) cloudCoverage to the densest coverage reported
    ! 2) cloudBase to the lowest qualified (SCT or more) cloud layer
    do i = 1, levels

       ! matching BUFR cloud amount to standard cloud coverage
       coverInt = m_standardCloudCoverage(amounts(i))

       if (coverInt /= -1 .and. coverInt > aRecord%cloudCoverage) then
          aRecord%cloudCoverage = coverInt
          if(base_not_found) then
             ! Find valid cloud layer with scattered or greater density.
             ! Cloud coverage without height is decoded as 10 in BUFR, 
             ! the returned m_standardCloudCoverage() will be -1
             ! some cloud base heights are reported below 0 as a mistake
             if(aRecord%cloudCoverage >= Cloud_Coverage%SCT .and. &
                (baseHeights(i) < BUFR_MISSING .and. baseHeights(i) > 0.0)) then
                aRecord%cloudBase = baseHeights(i)
                base_not_found = .false.! control the lowest SCT(or more) cloud
             end if
          end if
       end if

    end do

    if(aRecord%isFog .and. (aRecord%cloudCoverage < Cloud_Coverage%CLR)) then
       aRecord%cloudBase = 0.0
       aRecord%cloudCoverage = Cloud_Coverage%XOB
    end if

    return
  end subroutine m_checkCloudCoverMetar


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> SHIP: Look through all cloud layers and locate the densest layer as the
  !> cloud coverage and locate the lowest (SCT or more) layer as the cloud base
  !    
  !> @param[in] amounts       - array of cloud amount code at levels
  !> @param[in] amountTotal   - total cloud amount, for SHIP only
  !> @param[in] baseCode      - cloud base code, for SHIP only
  !> @param[in] levels        - how many cloud levels
  !> @param[inout] aRecord    - decoded/translated metar observation
  !----------------------------------------------------------------------------

  subroutine m_checkCloudCoverSHIPs(amounts, amountTotal, baseCode, levels, aRecord)
    implicit none
    integer, dimension(10), intent(in) :: amounts
    integer, intent(in) :: amountTotal
    integer, intent(in) :: baseCode
    integer, intent(in) :: levels
    type(metar_component_t), intent(inout) :: aRecord

    integer :: i, coverIntTotal, coverInt

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! if the total cloud coverage is clear or few
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    coverIntTotal = m_standardCloudCoverage(amountTotal)
    if(coverIntTotal <= Cloud_Coverage%FEW .and. coverIntTotal /= -1) then
       aRecord%cloudCoverage = coverIntTotal
       ! leave cloud base as MISSING, no assignment needed
       return
    end if

    coverInt = 0
    ! Find out the lowest cloud amount
    do i = 1, levels
       coverInt = m_standardCloudCoverage(amounts(i))
       if (coverInt /= -1) exit
    end do

    if(coverInt >= Cloud_Coverage%SCT) then
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       ! if the lowest cloud amount is SCT or more
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       ! if the lowest cloud amount is SCT, it provides useful
       ! information for baseK() in icing algorithm; no difference
       ! from being NONE/MISSING for sat_cld() in icing algorithm
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       aRecord%cloudCoverage = coverInt
       aRecord%cloudBase = m_getShipsCloudBase(baseCode)
    else
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       ! if the lowest cloud amount is FEW or CLR or -1
       ! leave cloud base as MISSING, leave cloud amount as 
       ! NONE/UNKNOWN, no assignment needed.
       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       ! there is no way to tell whether there are more
       ! clouds at higher levels
    end if

    if(aRecord%isFog .and. (coverInt < Cloud_Coverage%CLR) .and.&
                    (coverIntTotal < Cloud_Coverage%CLR)) then
       aRecord%cloudBase = 0.0
       aRecord%cloudCoverage = Cloud_Coverage%XOB
    end if

    return
  end subroutine m_checkCloudCoverSHIPs

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Convert cloud coverage from BUFR to standard
  !> Notes: 
  !>   BUFR dump cloud amount code figure is here:
  !>     http://www.emc.ncep.noaa.gov/mmb/data_processing/table_20.htm#0-20-011
  !>   10 is not counted. Cloud coverages without a height are all decoded as 10
  !>   in BUFR, no matter FEW///, SCT///, BKN///, OVC///, or VV///
  !
  !> @param[in] amount   - BUFR cloud amount
  !> @return standard cloud coverage; -1 if no matching
  !----------------------------------------------------------------------------

  elemental integer function m_standardCloudCoverage(amount)
    implicit none
    integer, intent(in) :: amount

    select case(amount)
    case(0)
       m_standardCloudCoverage = Cloud_Coverage%CLR
    case(1:2, 13)
       m_standardCloudCoverage = Cloud_Coverage%FEW
    case(3:4, 11)
       m_standardCloudCoverage = Cloud_Coverage%SCT
    case(5:7, 12)
       m_standardCloudCoverage = Cloud_Coverage%BKN
    case(8)
       m_standardCloudCoverage = Cloud_Coverage%OVC
    case(9)
       m_standardCloudCoverage = Cloud_Coverage%XOB
    case default
       m_standardCloudCoverage = -1
    end select

    return
  end function m_standardCloudCoverage


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Convert SHIP cloud base height from BUFR to meter 
  !
  !> @param[in] baseCode   - SHIP BUFR cloud base code
  !> @return  cloud base in middle of the following range:                  
  !>          0 -- 0 to 50 m
  !>          1 -- 50 to 100 m
  !>          2 -- 100 to 200 m
  !>          3 -- 200 to 300 m
  !>          4 -- 300 to 600 m
  !>          5 -- 600 to 1000 m
  !>          6 -- 1000 to 1500 m
  !>          7 -- 1500 to 2000 m
  !>          8 -- 2000 to 2500 m
  !>          9 -- above 2500 m
  !>          / -- unknown
  !----------------------------------------------------------------------------

  elemental real function m_getShipsCloudBase(baseCode)
    implicit none
    integer, intent(in) :: baseCode

    select case(baseCode)
    case(0)
       m_getShipsCloudBase = 25.
    case(1)
       m_getShipsCloudBase = 75.
    case(2)
       m_getShipsCloudBase = 150.
    case(3)
       m_getShipsCloudBase = 250.
    case(4)
       m_getShipsCloudBase = 450.
    case(5)
       m_getShipsCloudBase = 800.
    case(6)
       m_getShipsCloudBase = 1250.
    case(7)
       m_getShipsCloudBase = 1750.
    case(8)
       m_getShipsCloudBase = 2250.
    case(9)
       m_getShipsCloudBase = MISSING ! no range
    case default
       m_getShipsCloudBase = MISSING
    end select

    return
  end function m_getShipsCloudBase

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Matching present weather from BUFR dump file. Code figure:
  !> http://www.emc.ncep.noaa.gov/mmb/data_processing/table_20.htm#0-20-003
  !> ---------
  !> 00-49 and 120-125 (no precipitation at the time of observation) are not
  !> counted for precipitation types (isFreezingDrizzle, isFreezingRain,
  !> isSnow, isRain, isDrizzle, isIcePellets), except 16.
  !> For 16 (VCSH): will be treat as rain at this station
  !> ---------
  !> 28, 120 (not at the time of observation) will not be counted for FOG.
  !
  !> @param[in] iprwes             - present weather code
  !> @param[in] prweNum            - how many present weather codes
  !> @param[in] freezePrecipFound
  !> @param[inout] aRecord         - decoded/translated metar observation
  !----------------------------------------------------------------------------

  subroutine m_checkWeathers(iprwes, prweNum, freezePrecipFound, aRecord)
    implicit none
    integer, intent(in) :: iprwes(:), prweNum
    logical, intent(inout) :: freezePrecipFound
    type(metar_component_t), intent(inout) :: aRecord

    integer :: i

    do i = 1, prweNum
       select case(iprwes(i))
       case(11, 12, 40:49, 130:135)
          aRecord% isFog = .true.
       case(66:67, 164:166)
          aRecord% isFreezingRain = .true.
       case(56:57, 154:156)
          aRecord% isFreezingDrizzle = .true.
       case(147, 148)  ! for a case not specified to freezing rain or drizzle
          freezePrecipFound = .true.
       case(79, 174:176)
          aRecord% isIcePellets = .true.
       case(16, 60:65, 80:82, 91:92, 160:163, 181:184)
          ! * excluding freezing rain
          ! * 16            : VCSH, will be treat as rain at this station
          ! * 60-65
          ! * 80-82, 181-184: rain shower
          ! * 91-92         : rain, thunderstorm during the preceding hour
          ! * 160-163
          aRecord% isRain = .true.
       case(58:59, 157:158)
          ! * excluding freezing rain/drizzle
          ! * 58-59, 157-158: Rain and drizzle
          aRecord% isRain = .true.
          aRecord% isDrizzle = .true.
       case(50:55, 150:153)
          ! * excluding freezing drizzle
          ! * 50-55, 150-153
          ! * Notes: 68-69, 167-168: Rain or drizzle and snow, not counted 
          ! *        as drizzle, was treated as rain+snow
          aRecord% isDrizzle = .true.
       case(68:69, 167:168, 83:84)
          ! * excluding freezing rain
          ! * 68-69, 167-168: Rain or drizzle and snow,  will be treated as rain+snow
          ! * 83-84         : rain shower and snow
          aRecord% isRain = .true.
          aRecord% isSnow = .true.
       case(70:75, 77, 78, 85:88, 93:94, 170:173, 177, 185:187)
          ! * 70-75, 170-173
          ! * 77            : snow grain
          ! * 78            : Isolated star-like snow crystals
          ! * 85-86, 185-187: shower snow
          ! * 87-88         : shower snow pelletes
          ! * 93-94         : snow, thunderstorm during the preceding hour
          ! * 177           : snow grains
          ! * Notes: 127-129 (blowing or drifting snow or sand) was excluded, but
          ! *        they were used in RUC version CIP.
          aRecord% isSnow = .true.
       case(95:97, 192:195)
          ! * excluding freezing rain
          ! * 95-97, 192-195: rain, thunderstorm at the time of observation
          aRecord% isRain = .true.
          aRecord% isThunder = .true.
       case(17, 98:99, 126, 196)
          ! * 17            : no precipitation
          ! * 95-99         : with precipitation
          ! * 126           : incorrect? Should be 191?
          ! * 190-196       : of automated stations
          ! * Notes: 91-94 during the preceding hour
          aRecord% isThunder = .true.
       end select
    end do

    return
  end subroutine m_checkWeathers

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> public subroutine to print a Metar observation 
  !
  !> @param[in]   metarObs   - translated Metar observation 
  !----------------------------------------------------------------------------

  subroutine printMetarComponent(metarObs)
    implicit none
    type(metar_component_t), intent(in) :: metarObs
  
    write(*,*) "Station Name: "         ,metarObs%stationName
    write(*,*) "Time: "                 ,metarObs%obsTime
    write(*,*) "Distance: "             ,metarObs%distance
    write(*,*) "Station Type: "         ,metarObs%stationType
    write(*,*) "Latitude: "             ,metarObs%latitude
    write(*,*) "Longitude: "            ,metarObs%longitude
    write(*,*) "Elevation: "            ,metarObs%elevation
    write(*,*) "Cloud Base: "           ,metarObs%cloudBase
    write(*,*) "Cloud Type: "           ,metarObs%cloudCoverage
    write(*,*) "Freezing Drizzle: "     ,metarObs%isFreezingDrizzle
    write(*,*) "Freezing Rain: "        ,metarObs%isFreezingRain
    write(*,*) "Snow: "                 ,metarObs%isSnow
    write(*,*) "Rain: "                 ,metarObs%isRain
    write(*,*) "Drizzle: "              ,metarObs%isDrizzle
    write(*,*) "Ice Pellets: "          ,metarObs%isIcePellets
    write(*,*) "Thunder: "              ,metarObs%isThunder
    write(*,*) "Fog: "                  ,metarObs%isFog
    write(*,*) "Present Wx Sensor: "    ,metarObs%isPresentWxSensorOk
    write(*,*) "Freezing Rain Sensor: " ,metarObs%isFreezingRainSensorOk
    write(*,*) "Thunder Sensor: "       ,metarObs%isThunderSensorOk

    return
  end subroutine printMetarComponent


!------------------------------------------------------------------------------
! The following functions/subroutines are all about gridding and translating
!------------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Assign observations to grid points, depending on the input model grid
  !> information (kgds)
  !
  !> @param[in] kgds           - model grid information
  !> @param[in] radius         - within a distance an observation influences
  !> @param[in] stations       - station observations obtained from BUFR
  !> @param[inout] metarData2D - translated and gridded Metar data output
  !> @param[out]   iret        - status
  !----------------------------------------------------------------------------

  subroutine gridpointMetar(kgds, radius, stations, metarData2D, iret)
    implicit none
    integer, intent(in) :: kgds(:)
    real, intent(in) :: radius
    type(stack_station_t), intent(in) :: stations
    type(metar_data_t), allocatable, intent(inout) :: metarData2D(:,:)
    integer, intent(out) :: iret

    type(MetarGridPoint_t), allocatable :: gridPoints2D(:,:)
    integer :: nx, ny, i, j, k

    nx = kgds(2)
    ny = kgds(3)

    allocate(gridPoints2D(nx, ny), stat=iret)
    allocate(metarData2D(nx, ny), stat=iret)

    do j = 1, ny
    do i = 1, nx
       allocate(gridPoints2D(i,j)%ringedMetars(CFG_NumProcessingRanges))

       call  m_initializeMetarData(metarData2D(i,j))

       gridPoints2D(i,j)%hasMetar = .false.
       do k = 1, CFG_NumProcessingRanges
          nullify(gridPoints2D(i,j)%ringedMetars(k)%next)
       end do
    end do
    end do

    ! 1/3 step of nearby gridpoints in GridTemplate
    call initNearbyGridPoints(kgds, radius)

    ! Gridding
    ! 2/3 step of nearby gridpoints in GridTemplate
    call m_assignMetars2GridPts2D(stations, gridPoints2D)

    ! final metar data output after more translation 
    do j = 1, ny
    do i = 1, nx
      call m_setMetarDataFromGridPts(gridPoints2D(i,j), metarData2D(i, j))
      ! Release the station stacks of gridPoints2D(i,j) after gridding
      do k = 1, CFG_NumProcessingRanges
        call m_freeStationStack(gridPoints2D(i,j)%ringedMetars(k))
      end do
      deallocate(gridPoints2D(i,j)%ringedMetars)
    end do
    end do

    ! 3/3 step of nearby gridpoints in GridTemplate
    call doneNearbyGridPoints()

    ! release memory of gridPoints2D
    deallocate(gridPoints2D)

    return
  end subroutine gridpointMetar


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> initialize the output metar data 
  !
  !> @param[in] metarData - the output metar data
  !----------------------------------------------------------------------------

  subroutine m_initializeMetarData(metarData)
    implicit none
    type(metar_data_t), intent(inout) :: metarData

    metarData%affected                   = .false.! not affected by any observation
    metarData%cloudBaseHeight            = MISSING
    metarData%dist2CloudBaseHeight       = MISSING
    metarData%cloudCoverage              = Cloud_Coverage%NONE
    metarData%dist2CloudCoverage         = MISSING
    metarData%dist2FreezingDrizzle       = MISSING
    metarData%dist2FreezingRain          = MISSING
    metarData%dist2IcePellets            = MISSING
    metarData%dist2Rain                  = MISSING
    metarData%dist2Snow                  = MISSING
    metarData%dist2Drizzle               = MISSING
    metarData%dist2Thunder               = MISSING

    return
  end subroutine m_initializeMetarData

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Gridding: Assign Metar station observations to their affecting gridpoints
  !
  !> @param[in]    stations     - all Metar station observations
  !> @param[inout] gridPoints2D - all (array) of gridded Metar grid points
  !----------------------------------------------------------------------------

  subroutine m_assignMetars2GridPts2D(stations, gridPoints2D)
    implicit none
    type(stack_station_t), target, intent(in) :: stations
    type(MetarGridPoint_t), target, intent(inout) :: gridPoints2D(:,:)

    type(MetarGridPoint_t), pointer :: gridPointer

    type(stack_station_t), pointer :: stationPointer
    type(metar_component_t) :: metarObs

    type(nearby_gridpoint_t) :: nearbyGridPts
    type(nearby_gridpoint_t), pointer :: nearbyPointer

    integer :: i
 
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Loop through Metar stations and assign to its affected grid points
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! skip the first useless element
    stationPointer => stations%next
    loop_station2points: do while(associated(stationPointer))
      metarObs = stationPointer%metar
      call getNearbyGridPoints(metarObs%latitude, metarObs%longitude, nearbyGridPts)
      
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
      ! Loop through grid points influenced by this station.
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
      ! skip the first useless element
      nearbyPointer => nearbyGridPts%next
      loop_nearbypoints: do while (associated(nearbyPointer))
        metarObs%distance = nearbyPointer%distance

        ! Add the observation to the appropriate ringe of this nearby gridpoint
        gridPointer => gridPoints2D(nearbyPointer%i,nearbyPointer%j)
        do i = 1, CFG_NumProcessingRanges
           if (metarObs%distance < CFG_ProcessingLimits(i)) then
              gridPointer%hasMetar = .true.
              ! add the obs to the array of ringed stack!
              call m_addMetar2Stack(metarObs, gridPointer%ringedMetars(i))
              ! break the loop once the proper ringed stack is found and set
              exit
           end if
        end do

        nearbyPointer => nearbyPointer%next
      end do loop_nearbypoints

      stationPointer => stationPointer%next

      call freeNearbyGridPoints(nearbyGridPts)
    end do loop_station2points

    return

  end subroutine m_assignMetars2GridPts2D

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Set the output Metar data from all observations influencing one gridpoint
  ! 
  !> @param[in]    gridpt    - one gridpoint with influencing observations
  !> @param[inout] metardata - output Metar data
  !----------------------------------------------------------------------------
  subroutine m_setMetarDataFromGridPts(gridpt, metardata)
    implicit none
    type(MetarGridPoint_t), target, intent(in) :: gridpt
    type(metar_data_t), intent(inout) :: metardata

    if (.not. gridpt%hasMetar) return

    ! metarData%affected=.false. at initialization
    metarData%affected = .true.

    ! The following calls are based on: there are Metar observations

    call m_setMetarCloudBaseAndDistance(gridpt, metardata)
    call m_setMetarCloudCoverageAndDistance(gridpt, metardata)
    call m_setMetarDist2FreezingDrizzle(gridpt, metardata)
    call m_setMetarDist2FreezingRain(gridpt, metardata)
    call m_setMetarDist2IcePellets(gridpt, metardata)
    call m_setMetarDist2Rain(gridpt, metardata)
    call m_setMetarDist2Snow(gridpt, metardata)
    call m_setMetarDist2Drizzle(gridpt, metardata)
    call m_setMetarDist2Thunder(gridpt, metardata)

    if(metardata%cloudBaseHeight < MISSING+1.) &
         metardata%cloudCoverage = Cloud_Coverage%NONE

    return
  end subroutine m_setMetarDataFromGridPts
  
  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Set the output Metar data from all observations influencing one gridpoint
  !> The following values are set:
  !>     metardata%dist2CloudBaseHeight
  !>     metardata%cloudBaseHeight
  ! Note: Cloud overages are:
  !       CT_NONE=0, CT_CLR=1, CT_FEW=2, CT_SCT=3, CT_BKN=4, CT_OVC=5, CT_XOB=6
  ! 
  !> @param[in]    gridpt    - one gridpoint with influencing observations
  !> @param[inout] metardata - output Metar data
  !----------------------------------------------------------------------------

  subroutine m_setMetarCloudBaseAndDistance(gridpt, metardata)
    implicit none
    type(MetarGridPoint_t), target, intent(in) :: gridpt
    type(metar_data_t), intent(inout) :: metardata

    type(stack_station_t), pointer :: stationPointer
    integer :: indexRangeBin
    logical :: foundMetar
    real    :: metarDistance, distance

    integer :: metarCloudCoverage
    real    :: metarCloudBase, cloudBase 

    ! There are Metar observations

    metarCloudBase = MISSING
    cloudBase      = MISSING
    distance       = MISSING

    metarDistance = MISSING
    foundMetar  = .false.

    ! Loop through range bins. (different way of copy of all observations)
    loop_rangeBin: do indexRangeBin = 1, CFG_NumProcessingRanges
 
       if(foundMetar) exit ! cloud is found

       ! Loop through the ringed observations associated with current grid point.
       stationPointer => gridpt%ringedMetars(indexRangeBin)%next
       loop_stationStack: do while(associated(stationPointer))

          if(stationPointer%metar%cloudCoverage == Cloud_Coverage%NONE) then
             stationPointer => stationPointer%next
             cycle
          end if

          ! Cloud is found
          foundMetar = .true.
         
          metarDistance = stationPointer%metar%distance
          metarCloudCoverage  = stationPointer%metar%cloudCoverage

          ! Do not included in cloud base height calculation 
          ! unless Metar cloud cover is scattered or greater.
          if(metarCloudCoverage >= Cloud_Coverage%SCT) then
            ! Get cloud base in meters ASL.
            if(stationPointer%metar%cloudBase < abs(MISSING) - .5) then  
              metarCloudBase = stationPointer%metar%cloudBase + stationPointer%metar%elevation
              if(abs(metarCloudBase) < abs(cloudBase)) then
                ! If current Metar has a lower cloud base than the consensus cloud base (and if the
                ! current Metar is associated with a cloud cover of scattered or greater), assign the
                ! current Metar cloud base to the consensus cloud base.
                distance  = metarDistance
                cloudBase = metarCloudBase
              end if
            endif
          end if

          stationPointer => stationPointer%next
       end do loop_stationStack

    end do loop_rangeBin 

    metardata%cloudBaseHeight      = cloudBase
    metardata%dist2CloudBaseHeight = distance

    return
  end subroutine m_setMetarCloudBaseAndDistance

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Set the output Metar data from all observations influencing one gridpoint
  !> The following values are set:
  !>     metardata%cloudCoverage
  !>     metardata%dist2CloudCoverage
  ! Note: Cloud overages are:
  !       CT_NONE=0, CT_CLR=1, CT_FEW=2, CT_SCT=3, CT_BKN=4, CT_OVC=5, CT_XOB=6
  ! 
  !> @param[in]    gridpt    - one gridpoint with influencing observations
  !> @param[inout] metardata - output Metar data
  !----------------------------------------------------------------------------

  subroutine m_setMetarCloudCoverageAndDistance(gridpt, metardata)
    implicit none
    type(MetarGridPoint_t), target, intent(in) :: gridpt
    type(metar_data_t), intent(inout) :: metardata

    type(stack_station_t), pointer :: stationPointer
    integer :: indexRangeBin
    logical :: foundMetar
    real    :: metarDistance, distance

    integer :: cloudCoverage, metarCloudCoverage

    cloudCoverage = Cloud_Coverage%NONE
    distance      = MISSING
    metarDistance = MISSING
    foundMetar    = .false.

    ! There are Metar observations

    ! Loop through range bin.
    loop_rangeBin: do indexRangeBin = 1, CFG_NumProcessingRanges

       if(foundMetar) exit ! cloud is found

       ! Loop through the ringed observations associated with current grid point.
       ! skip the very first useless elmement
       stationPointer => gridpt%ringedMetars(indexRangeBin)%next
       loop_stationStack: do while(associated(stationPointer))
          metarDistance = stationPointer%metar%distance

          metarCloudCoverage = stationPointer%metar%cloudCoverage
          if(metarCloudCoverage > cloudCoverage) then
             cloudCoverage = metarCloudCoverage
             distance = metarDistance
          end if
          stationPointer => stationPointer%next
       end do loop_stationStack

       if(cloudCoverage > Cloud_Coverage%NONE) then
          foundMetar = .true. ! Cloud is found, end searching
       end if

    end do loop_rangeBin

    metardata%cloudCoverage      = cloudCoverage
    metardata%dist2CloudCoverage = distance

    return
  end subroutine m_setMetarCloudCoverageAndDistance

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Set the output Metar data from all observations influencing one gridpoint
  !> The following values are set:
  !>    metardata%dist2FreezingDrizzle 
  ! 
  !> @param[in]    gridpt    - one gridpoint with influencing observations
  !> @param[inout] metardata - output Metar data
  !----------------------------------------------------------------------------

  subroutine m_setMetarDist2FreezingDrizzle(gridpt, metardata)
    implicit none
    type(MetarGridPoint_t), target, intent(in) :: gridpt
    type(metar_data_t), intent(inout) :: metardata
          
    type(stack_station_t), pointer :: stationPointer
    integer :: indexRangeBin
    logical :: foundMetar
    real    :: metarDistance, distance

    integer :: stationType

    distance       = MISSING
    metarDistance  = MISSING
    foundMetar     = .false.

    ! There are Metar observations

    ! Loop through range bins.
    loop_rangeBin: do indexRangeBin = 1, CFG_NumProcessingRanges

       if(foundMetar) exit ! metar is found 

       ! Loop through the ringed observations associated with current grid point.
       ! skip the very first useless elmement
       stationPointer => gridpt%ringedMetars(indexRangeBin)%next
       loop_stationStack: do while(associated(stationPointer))
          metarDistance = stationPointer%metar%distance
          stationType   = stationPointer%metar%stationType

          ! Station-type dependency -- Most Station_Types%AWOS do not report precip 
          ! Most Station_Types%ASOS can not detect FRDZ. Only Station_Types%MANUAL 
          ! can issue a non-FZDR report.
          if(stationType == Station_Types%MANUAL) then
             foundMetar = .true.
          end if

          if(abs(metarDistance) < abs(distance)) then
             if(stationPointer%metar%isFreezingDrizzle) then
                distance = metarDistance
                foundMetar = .true.
             end if
          end if
          stationPointer => stationPointer%next
       end do loop_stationStack

    end do loop_rangeBin

    metardata%dist2FreezingDrizzle = distance

    return
  end subroutine m_setMetarDist2FreezingDrizzle

!----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Set the output Metar data from all observations influencing one gridpoint
  !> The following values are set:
  !>    metardata%dist2FreezingRain
  ! 
  !> @param[in]    gridpt    - one gridpoint with influencing observations
  !> @param[inout] metardata - output Metar data
  !----------------------------------------------------------------------------

  subroutine m_setMetarDist2FreezingRain(gridpt, metardata)
    implicit none
    type(MetarGridPoint_t), target, intent(in) :: gridpt
    type(metar_data_t), intent(inout) :: metardata

    type(stack_station_t), pointer :: stationPointer
    integer :: indexRangeBin
    logical :: foundMetar
    real    :: metarDistance, distance

    integer :: stationType

    distance      = MISSING
    metarDistance = MISSING
    foundMetar    = .false.

    ! There are Metar observations

    ! Loop through range bins.
    loop_rangeBin: do indexRangeBin = 1, CFG_NumProcessingRanges

       if(foundMetar) exit ! metar is found

       ! Loop through the ringed observations associated with current grid point.
       ! skip the very first useless elmement
       stationPointer => gridpt%ringedMetars(indexRangeBin)%next
       loop_stationStack: do while(associated(stationPointer))
          metarDistance = stationPointer%metar%distance
          stationType   = stationPointer%metar%stationType

          ! Station-type dependency -- Most Station_Types%AWOS do not report precip
          if(stationType == Station_Types%MANUAL) then
             foundMetar = .true.
          else if(stationType == Station_Types%ASOS) then
             if(stationPointer%metar%isFreezingRainSensorOk) then
                foundMetar = .true.
             end if
          end if

          if(abs(metarDistance) < abs(distance)) then
             if(stationPointer%metar%isFreezingRain) then
                distance = metarDistance
                foundMetar = .true.
             end if
          end if
          stationPointer => stationPointer%next
       end do loop_stationStack

       if(foundMetar) foundMetar = .true.
    end do loop_rangeBin

    metardata%dist2FreezingRain = distance

    return
  end subroutine m_setMetarDist2FreezingRain

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Set the output Metar data from all observations influencing one gridpoint
  !> The following values are set:
  !>    metardata%dist2IcePellets
  ! 
  !> @param[in]    gridpt    - one gridpoint with influencing observations
  !> @param[inout] metardata - output Metar data
  !----------------------------------------------------------------------------

  subroutine m_setMetarDist2IcePellets(gridpt, metardata)
    implicit none
    type(MetarGridPoint_t), target, intent(in) :: gridpt
    type(metar_data_t), intent(inout) :: metardata

    type(stack_station_t), pointer :: stationPointer
    integer :: indexRangeBin
    logical :: foundMetar
    real    :: metarDistance, distance

    integer :: stationType

    distance      = MISSING
    metarDistance = MISSING
    foundMetar    = .false.

    ! There are Metar observations

   ! Loop through range bins.
    loop_rangeBin: do indexRangeBin = 1, CFG_NumProcessingRanges

       if(foundMetar) exit ! metar is found 

       ! Loop through the ringed observations associated with current grid point.
       ! skip the very first useless elmement
       stationPointer => gridpt%ringedMetars(indexRangeBin)%next
       loop_stationStack: do while(associated(stationPointer))
          metarDistance = stationPointer%metar%distance
          stationType   = stationPointer%metar%stationType

          ! Station-type dependency -- Most Station_Types%AWOS do not report precip
          if(stationType == Station_Types%MANUAL) then
             foundMetar = .true.
          else if(stationType == Station_Types%ASOS) then
             if(stationPointer%metar%isPresentWxSensorOk) then
                foundMetar = .true.
             end if
          end if

          if(abs(metarDistance) < abs(distance)) then
             if(stationPointer%metar%isIcePellets) then
                distance = metarDistance
                foundMetar = .true.
             end if
          end if

          stationPointer => stationPointer%next
       end do loop_stationStack

    end do loop_rangeBin

    metardata%dist2IcePellets = distance

    return
  end subroutine m_setMetarDist2IcePellets

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Set the output Metar data from all observations influencing one gridpoint
  !> The following values are set:
  !>    metardata%dist2Rain
  ! 
  !> @param[in]    gridpt    - one gridpoint with influencing observations
  !> @param[inout] metardata - output Metar data
  !----------------------------------------------------------------------------

  subroutine m_setMetarDist2Rain(gridpt, metardata)
    implicit none
    type(MetarGridPoint_t), target, intent(in) :: gridpt
    type(metar_data_t), intent(inout) :: metardata

    type(stack_station_t), pointer :: stationPointer
    integer :: indexRangeBin
    logical :: foundMetar
    real    :: metarDistance, distance

    integer :: stationType

    distance      = MISSING
    metarDistance = MISSING
    foundMetar    = .false.

    ! There are Metar observations

    ! Loop through range bins.
    loop_rangeBin: do indexRangeBin = 1, CFG_NumProcessingRanges

       if(foundMetar) exit ! metar is found 

       ! Loop through the ringed observations associated with current grid point.
       ! skip the very first useless elmement
       stationPointer => gridpt%ringedMetars(indexRangeBin)%next
       loop_stationStack: do while(associated(stationPointer))
          metarDistance = stationPointer%metar%distance
          stationType   = stationPointer%metar%stationType

          ! Station-type dependency -- Most Station_Types%AWOS do not report precip
          if(stationType == Station_Types%MANUAL) then
             foundMetar = .true.
          else if(stationType == Station_Types%ASOS) then
             if(stationPointer%metar%isPresentWxSensorOk) then
                foundMetar = .true.
             end if
          end if

          if(abs(metarDistance) < abs(distance)) then
             if(stationPointer%metar%isRain) then
                distance = metarDistance
                foundMetar = .true.
             end if
          end if

          stationPointer => stationPointer%next
       end do loop_stationStack

    end do loop_rangeBin

    metardata%dist2Rain = distance

    return
  end subroutine m_setMetarDist2Rain

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Set the output Metar data from all observations influencing one gridpoint
  !> The following values are set:
  !>    metardata%dist2Snow
  ! 
  !> @param[in]    gridpt    - one gridpoint with influencing observations
  !> @param[inout] metardata - output Metar data
  !----------------------------------------------------------------------------

  subroutine m_setMetarDist2Snow(gridpt, metardata)
    implicit none
    type(MetarGridPoint_t), target, intent(in) :: gridpt
    type(metar_data_t), intent(inout) :: metardata

    type(stack_station_t), pointer :: stationPointer
    integer :: indexRangeBin
    logical :: foundMetar
    real    :: metarDistance, distance

    integer :: stationType

    distance       = MISSING
    metarDistance  = MISSING
    foundMetar     = .false.
 
    ! There are Metar observations

    ! Loop through range bins.
    loop_rangeBin: do indexRangeBin = 1, CFG_NumProcessingRanges
 
       if(foundMetar) exit ! metar is found 

       ! Loop through the ringed observations associated with current grid
       ! point.
       ! skip the very first useless elmement
       stationPointer => gridpt%ringedMetars(indexRangeBin)%next
       loop_stationStack: do while(associated(stationPointer))
          metarDistance = stationPointer%metar%distance
          stationType   = stationPointer%metar%stationType

          ! Station-type dependency -- Most Station_Types%AWOS do not report precip
          if(stationType == Station_Types%MANUAL) then
             foundMetar = .true.
          else if(stationType == Station_Types%ASOS) then
             if(stationPointer%metar%isPresentWxSensorOk) then
                foundMetar = .true.
             end if
          end if

          if(abs(metarDistance) < abs(distance)) then
             if(stationPointer%metar%isSnow) then
                distance = metarDistance
                foundMetar = .true.
             end if
          end if

          stationPointer => stationPointer%next
       end do loop_stationStack

    end do loop_rangeBin

    metardata%dist2Snow = distance

    return
  end subroutine m_setMetarDist2Snow

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Set the output Metar data from all observations influencing one gridpoint
  !> The following values are set:
  !>    metardata%dist2Drizzle
  ! 
  !> @param[in]    gridpt    - one gridpoint with influencing observations
  !> @param[inout] metardata - output Metar data
  !----------------------------------------------------------------------------

  subroutine m_setMetarDist2Drizzle(gridpt, metardata)
    implicit none
    type(MetarGridPoint_t), target, intent(in) :: gridpt
    type(metar_data_t), intent(inout) :: metardata

    type(stack_station_t), pointer :: stationPointer
    integer :: indexRangeBin
    logical :: foundMetar
    real    :: metarDistance, distance

    integer :: stationType

    distance       = MISSING
    metarDistance  = MISSING
    foundMetar     = .false.

    ! There are Metar observations

    ! Loop through range bins.
    loop_rangeBin: do indexRangeBin = 1, CFG_NumProcessingRanges

       if(foundMetar) exit ! metar is found 

       ! Loop through the ringed observations associated with current grid point.
       ! skip the very first useless elmement
       stationPointer => gridpt%ringedMetars(indexRangeBin)%next
       loop_stationStack: do while(associated(stationPointer))
          metarDistance = stationPointer%metar%distance
          stationType   = stationPointer%metar%stationType

          ! Station-type dependency -- Most Station_Types%AWOS do not report precip
          ! Most Station_Types%ASOS can not detect drizzle. Only Station_Types%MANUAL 
          ! can issue a non-drizzle report. 
          if(stationType == Station_Types%MANUAL) then
             foundMetar = .true.
          end if

          if(abs(metarDistance) < abs(distance)) then
             if(stationPointer%metar%isDrizzle) then
                distance = metarDistance
                foundMetar = .true.
             end if
          end if

          stationPointer => stationPointer%next
       end do loop_stationStack

    end do loop_rangeBin
 
    metardata%dist2Drizzle = distance

    return
  end subroutine m_setMetarDist2Drizzle

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Set the output Metar data from all observations influencing one gridpoint
  !> The following values are set:
  !>    metardata%dist2Thunder
  ! 
  !> @param[in]    gridpt    - one gridpoint with influencing observations
  !> @param[inout] metardata - output Metar data
  !----------------------------------------------------------------------------

  subroutine m_setMetarDist2Thunder(gridpt, metardata)
    implicit none
    type(MetarGridPoint_t), target, intent(in) :: gridpt
    type(metar_data_t), intent(inout) :: metardata

    type(stack_station_t), pointer :: stationPointer
    integer :: indexRangeBin
    logical :: foundMetar
    real    :: metarDistance, distance

    integer :: stationType

    distance       = MISSING
    metarDistance  = MISSING
    foundMetar     = .false.

    ! There are Metar observations

    ! Loop through range bins.
    loop_rangeBin: do indexRangeBin = 1, CFG_NumProcessingRanges

       if(foundMetar) exit ! metar is found 

       ! Loop through the ringed observations associated with current grid point.
       ! skip the very first useless elmement
       stationPointer => gridpt%ringedMetars(indexRangeBin)%next
       loop_stationStack: do while(associated(stationPointer))
          metarDistance = stationPointer%metar%distance
          stationType   = stationPointer%metar%stationType

          ! Station-type dependency -- Most Station_Types%AWOS do not report precip
          if(stationType == Station_Types%MANUAL) then
             foundMetar = .true.
          else if(stationType == Station_Types%ASOS) then
             if(stationPointer%metar%isThunderSensorOk) then
                foundMetar = .true.
             end if
          end if

          if(abs(metarDistance) < abs(distance)) then
             if(stationPointer%metar%isThunder) then
                distance = metarDistance
                foundMetar = .true.
             end if
          end if

          stationPointer => stationPointer%next
       end do loop_stationStack

    end do loop_rangeBin

    metardata%dist2Thunder = distance

    return
  end subroutine m_setMetarDist2Thunder

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> print all Metar observatons influecing the gridPoint
  !----------------------------------------------------------------------------

  subroutine printGridPoint(ix, jy, gridPoint)
    integer, intent(in) :: ix, jy
    type(MetarGridPoint_t), target, intent(in) :: gridPoint

    integer :: nbrObs, i
    type(stack_station_t), pointer :: stationPointer

    write(*,*) "==========Grid point (", ix, ",", jy, ") has ", &
               nbrObs, " observations.=========="

    do i = 1, CFG_NumProcessingRanges
       nbrObs = 0
       stationPointer => gridPoint%ringedMetars(i)%next
       do while(associated(stationPointer))
          call printMetarComponent(stationPointer%metar)
          nbrObs = nbrObs + 1
          stationPointer => stationPointer%next
       end do
       write(*,*) "------Grid point (", ix, ",", jy, ") at bin", i, "has", nbrObs, "observations-------"
    end do

    return
  end subroutine printGridPoint

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Print an output grid data, type of metar_data_t
  !----------------------------------------------------------------------------
 
  subroutine printMetarData(metardata)
    type(metar_data_t), intent(in) :: metardata

    write(*, *) "Grid Point Metar Data"
    write(*, *) "  Cloud base height: "             , metardata%cloudBaseHeight
    write(*, *) "  Distance to cloud base height: " , metardata%dist2CloudBaseHeight
    write(*, *) "  Cloud coverage: "                , metardata%cloudCoverage
    write(*, *) "  Distance to cloud coverage: "    , metardata%dist2CloudCoverage
    write(*, *) "  Distance to freezing drizzle: "  , metardata%dist2FreezingDrizzle
    write(*, *) "  Distance to freezing rain: "     , metardata%dist2FreezingRain
    write(*, *) "  Distance to ice pellets: "       , metardata%dist2IcePellets
    write(*, *) "  Distance to rain: "              , metardata%dist2Rain
    write(*, *) "  Distance to snow: "              , metardata%dist2Snow
    write(*, *) "  Distance to drizzle: "           , metardata%dist2Drizzle
    write(*, *) "  Distance to thunder: "           , metardata%dist2Thunder

    return
  end subroutine printMetarData

end module Metar

