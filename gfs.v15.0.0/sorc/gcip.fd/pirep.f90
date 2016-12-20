!------------------------------------------------------------------------------
!
! Module:      Pirep
!
! DESCRIPTION:
!> Read PIREPs data from dump bufr
!
! REVISION HISTORY:
! August 2010
! February 2014 - modified
!
! Notes:       CIP should continue to run if no Pirep exists
!
!------------------------------------------------------------------------------

module Pirep

  use Kinds
  use GridTemplate
  use Config

  implicit none

  private
  public runPirep

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! parameters
  integer, parameter :: TEMPORAL_START_THRESHOLD = 900
  integer, parameter :: TEMPORAL_END_THRESHOLD = 7200
  real, parameter :: RADIAL_DIST_THRESHOLD = 200.0    ! km
  real, parameter :: VERTICAL_DIST_THRESHOLD = 1220.0 ! m  1220m ~ 4000 ft
  real, parameter :: NUM_INTENSITY_LEVELS = 8.0

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! Structures
  type :: pirep_report_t
     integer(kind=BYTE8) :: obsTime ! minutes since julian day 00:00
     real     :: lat        ! Location
     real     :: lon
     real     :: fltLevel
     real     :: iceBase1   ! Ice 1
     real     :: iceTop1
     integer  :: iceIntens1
     real     :: iceBase2   ! Ice 2
     real     :: iceTop2
     integer  :: iceIntens2
     !
     type(pirep_report_t), pointer :: next
  end type pirep_report_t

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! private members (configration related)
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  integer, pointer :: CFG_TimeWindow(:) ! data collecting time window, in minutes 

contains

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> public subroutine to be called to read, and grid Pirep data
  !
  !> @param[in] filename   - input bufr file name
  !> @param[in] iruntime   - array of run time, YYYY MM DD ZZ HH MM SS MS
  !> @param[in] cfg        - configuration file
  !> @param[in] kgds       - model grid information
  !> @param[in] heights    - model heights
  !> @param[out] pirepData - translated and gridded Metar data output
  !> @param[out] iret        - status; -1 if failure
  !----------------------------------------------------------------------------

  subroutine runPirep(filename, iruntime, cfg, kgds, heights, pirepData, iret)
    implicit none
    character(len=*),  intent(in) :: filename
    ! YYYY MM DD ZZ HH MM SS MS to call /nwprod/lib/sorc/w3nco/w3movdat.f
    integer, intent(in) :: iruntime(8)
    type(config_t), target, intent(in) :: cfg
    integer, intent(in) :: kgds(:)
    real,    intent(in) :: heights(:,:,:)
    type(pirep_data_t), allocatable, intent(out) :: pirepData(:,:,:)
    integer, intent(out) :: iret

    integer, allocatable :: pirepCounts(:, :, :)

    type(pirep_report_t), target :: pireps
    integer :: nPireptReports
    type(pirep_report_t), pointer :: iterator

    ! configuration fields
    CFG_TimeWindow => cfg%pirep% timeWindow

    iret = -1

    ! read in PIREPs from BUFR
    nPireptReports = getPireps(trim(filename), iruntime, CFG_TimeWindow, pireps)

    if(nPireptReports == 0) then
       write(*,*) "PirepMain::runPirep() -- there is no Pirep report."
       return
    else if(nPireptReports < 0) then
       write(*,*) "PirepMain::runPirep() error -- not able to read in BUFR data sucessfully."
       return
    end if

    write(*,*) "  Total number of PIREPs read: ", nPireptReports


    ! mapping, gridding & calculation of output
    call gridpointPirep(kgds, iruntime, RADIAL_DIST_THRESHOLD, pireps, heights, pirepData, iret)

    ! release memory of pireps
    call m_freePirepStack(pireps)

    write(*,*) "               PIREPs DONE               ", LF

    return       
  end subroutine runPirep


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Read Pirep data from BUFR dump file. Collect data in [-120, 0]
  !> minute time window of the run time. ! related to m_getTemporalInterest()
  !
  !> @param[in] infile     - input bufr file name
  !> @param[in] iruntime   - array of run time, YYYY MM DD ZZ HH MM SS MS
  !> @param[in] timeWindow - data collecting time window, in minutes
  !> @param[out] pireps
  !> @Return: the number of strikes; -1 if failure
  !----------------------------------------------------------------------------

  integer function getPireps(infile, iruntime, timeWindow, pireps)
    implicit none
    character(len=*),  intent(in) :: infile
    ! YYYY MM DD ZZ HH MM SS MS to call /nwprod/lib/sorc/w3nco/w3movdat.f
    integer, intent(in) :: iruntime(8)
    integer, intent(in), pointer :: timeWindow(:)  ! in minutes
    type(pirep_report_t), target, intent(inout) :: pireps

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! BUFR related variables
    integer :: ireadmg, ireadsb ! bufr lib functions
    integer :: iunit            ! bufr file unit
    ! BUFR message level information
    integer(kind=BYTE8) :: report_date
    character(len=BYTE8) :: msgtype
    ! For location information
    ! FLVL: FLIGHT LEVEL, in pirep 
    character(*), parameter :: headerString = 'YEAR MNTH DAYS HOUR MINU CLAT CLON FLVL'
    real(kind=BYTE8) :: headers(8)
    ! For icing information
    real(kind=BYTE8) :: icing(2, 10)
    ! For raw PIREP report, for more decoding purposes
    real(kind=BYTE8) :: raw(BUFR_REPORT_LENGTH/BYTE8)
    character(len=BUFR_REPORT_LENGTH) :: rawPirep 
    equivalence(rawPirep, raw)
    integer :: ireport_time(5) ! YYYY MM DD HH MM

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! other variables
    type(pirep_report_t), pointer :: pirepPointer
    integer :: icingNumReports ! how many icing reports in one Pirep record
   integer :: i, iret
    integer :: mvtime(8) ! YYYY MM DD ZZ HH MM SS MS
    real    :: rinc(5)  ! interval (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
    character(len=12) :: start_time, end_time, report_time  ! YYYYMMDDHHMM
    integer(kind=BYTE8) :: lreport_time

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! functions outside 
    integer :: IW3JDN

    integer :: count_valid, count_reports

    getPireps = 0
    nullify(pireps%next)

    ! Convert time window to YYYYMMDDHHMM
    rinc(:) = 0.
    rinc(3) = timeWindow(1) 
    call W3MOVDAT(rinc, iruntime, mvtime)
!write(start_time, "((I4,4I2.2))") mvtime(1),mvtime(2),mvtime(3),mvtime(5),mvtime(6)
    rinc(3) = timeWindow(2)
    call W3MOVDAT(rinc, iruntime, mvtime)
!write(end_time, "((I4,4I2.2))") mvtime(1),mvtime(2),mvtime(3),mvtime(5),mvtime(6)

    iunit = 14
    call closbf(iunit)
    open(iunit,file=infile,form='unformatted', iostat=iret)
    if(iret /= 0) then
       getPireps = -1
       write(*,*) "Pirep::getPireps() not able to open input Pirep BUFR file=", trim(infile)
       return
    end if
    call openbf(iunit,'IN',iunit)
    call datelen(10)

    count_reports = 0
    count_valid = 0

    loop_msg: do while(ireadmg(iunit, msgtype, report_date) == 0)

       if(msgtype /= 'NC004002')  cycle

       loop_readsb: do while(ireadsb(iunit) == 0)

          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
          ! extract information
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

          ! time, latitude, longitude, flight level
          !     time=(YEAR MNTH DAYS HOUR MINU)          
          ! headerString = 'YEAR MNTH DAYS HOUR MINU CLAT CLON FLVL'

          call ufbint(iunit, headers, size(headers), 1, iret, headerString)

          ! only collect PIREP data in the time window
          ireport_time(1:5) = headers(1:5)
          write(report_time, "((I4,4I2.2))") (ireport_time(i), i=1,5)
          !write(*,*) report_time, " ", start_time, " ", end_time
          count_reports = count_reports + 1
          if(report_time < start_time .or. report_time > end_time) cycle

          ! airframe icing (height of base of icing, height of top of icing)
          ! in meter
          call ufbint(iunit, icing, 2, 10, icingNumReports, 'HBOI HTOI')

          ! raw PIREP report
          call ufbint(iunit, raw, 1, size(raw), iret, 'RRSTG')
          count_valid = count_valid + 1
          ! Must have a valid location
          if(headers(6) > BUFR_MISSING .or. headers(7) > BUFR_MISSING) cycle

          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
          ! add the report to pirep
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
          ! convert report time to minutes since julian day 00:00
          lreport_time = IW3JDN(ireport_time(1), ireport_time(2),ireport_time(3))
          lreport_time = lreport_time*24*60 + ireport_time(4)*60 + ireport_time(5)

          allocate(pirepPointer)

          pirepPointer%obsTime = lreport_time

          pirepPointer%lat      = headers(6)
          pirepPointer%lon      = headers(7)

          if(headers(8) > BUFR_MISSING) then
             pirepPointer%fltLevel = MISSING
          else
             pirepPointer%fltLevel = headers(8)
          end if

          pirepPointer%iceBase1   = MISSING
          pirepPointer%iceTop1    = MISSING
          pirepPointer%iceIntens1 = MISSING_INT

          pirepPointer%iceBase2   = MISSING
          pirepPointer%iceTop2    = MISSING
          pirepPointer%iceIntens2 = MISSING_INT
print *, icingNumReports, icing(1,1),  icing(2,1), icing(1,2),  icing(2,2)
          if(icingNumReports >= 1) then
             if(icing(1, 1) < BUFR_MISSING) pirepPointer%iceBase1   = icing(1,1)
             if(icing(2, 1) < BUFR_MISSING) pirepPointer%iceTop1    = icing(2,1)
             call m_setIntensity(rawPirep, 1, pirepPointer%iceIntens1)
          end if

          if(icingNumReports >= 2) then
             if(icing(1, 2) < BUFR_MISSING) pirepPointer%iceBase2   = icing(1,2)
             if(icing(2, 2) < BUFR_MISSING) pirepPointer%iceTop2    = icing(2,2)
             call m_setIntensity(rawPirep, 2, pirepPointer%iceIntens2)
          end if

          pirepPointer%next => pireps%next
          pireps%next => pirepPointer

          getPireps = getPireps + 1

       end do loop_readsb
    end do loop_msg

    print *, "PIREPs count(valid, reports) =", count_valid, count_reports

    close(iunit)

    return
  end function getPireps

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Set icing intensity from raw Pirep report
  !
  !> @param[in]  rawPirep - The raw pirep report
  !> @param[in]  seq      - which icing report (2 possible reports)
  !> @param[out] intensity 
  !
  ! Notes:  from AWRP's Pirep decoding Perl code
  !
  !   Airframe icing
  !---------------------------------------------
  !   0 No icing                           NON NIL NEG
  !   1 Light icing                        LI?G?H?T
  !   2 Light icing In cloud               
  !   3 Light icing In precipitation
  !   4 Moderate icing                     MDT MOD
  !   5 Moderate icing in cloud
  !   6 Moderate icing in precipitation
  !   7 Severe icing                       SEV SVR EXT(reme)
  !   8 Severe icing in cloud
  !   9 Severe icing in precipitation
  !  10 Trace of icing                     TRA TRC
  !  11 Trace of icing in cloud
  !  12 Trace of icing in precipitation
  !  13 Reserved
  !  14 Reserved
  !  15 Missing value
  !                                        HEAV HVY
  !----------------------------------------------------------------------------

  subroutine m_setIntensity(rawPirep, seq, intensity)
    character(len=*), intent(in) :: rawPirep
    integer, intent(in) :: seq ! means 1st/2nd icing
    integer, intent(out) :: intensity

    character(len=50) :: icingPirep

    integer, parameter :: SEV     = 8
    integer, parameter :: HVY     = 7
    integer, parameter :: MOD_SEV = 6  ! MOD_HVY is the same
    integer, parameter :: MOD     = 5
    integer, parameter :: LGT_MOD = 4
    integer, parameter :: LGT     = 3
    integer, parameter :: TRC_LGT = 2
    integer, parameter :: TRC     = 1
    integer, parameter :: NEG     = -1

    logical :: isSev, isHvy, isMod, isLgt, isTrc, isNeg

    integer :: idx, idx2

    isSev = .false.
    isHvy = .false.
    isMod = .false.
    isLgt = .false.
    isTrc = .false.
    isNeg = .false.

    icingPirep = ""

    ! check whether there is an icing report
    idx = index(rawPirep, "/IC")
    if(idx == 0) then
       return
    end if

    ! for 2nd icing report
    if(seq == 2) then
       idx2 = index(rawPirep(idx+3:), "/IC")
       if(idx == 0) then
          return
       end if
       idx = idx + idx2 - 1
    end if

    idx = idx + 3 ! skip "/IC"
    icingPirep = rawPirep(idx : )

    ! only keep icing observation, get rid of others
    idx = index(icingPirep, "/")
    if(idx > 0) then
       icingPirep(idx :) = ""
    end if

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! decode intensity
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    if(index(icingPirep, "SVR") > 0 .or. &
       index(icingPirep, "SEV") > 0 .or. &
       index(icingPirep, "EXT") > 0)        isSev = .true.

    if(index(icingPirep, "HEA") > 0 .or. &
       index(icingPirep, "HVY") > 0)        isHvy = .true.

    if(index(icingPirep, "MDT") > 0 .or. &
       index(icingPirep, "MOD") > 0)        isMod = .true.

    if(index(icingPirep, "LI") > 0 .or. &
       index(icingPirep, "LG") > 0 .or. &
       index(icingPirep, "LH") > 0 .or. &
       index(icingPirep, "LT") > 0)         isLgt = .true.

    if(index(icingPirep, "TRA") > 0 .or. &
       index(icingPirep, "TRC") > 0)        isTrc = .true.

    if(index(icingPirep, "NON") > 0 .or. &
       index(icingPirep, "NIL") > 0 .or. &
       index(icingPirep, "NEG") > 0)        isNeg = .true.

    if(isSev .and. isMod) then
       intensity = MOD_SEV
    else if(isHvy .and. isMod) then
       intensity = MOD_SEV
    else if(isMod .and. isLgt) then
       intensity = LGT_MOD
    else if(isLgt .and. isTrc) then
       intensity = TRC_LGT
    else if(isSev) then
       intensity = SEV
    else if(isHvy) then
       intensity = HVY
    else if(isMod) then
       intensity = MOD
    else if(isLgt) then
       intensity = LGT
    else if(isTrc) then
       intensity = TRC
    else if(isNeg) then
       intensity = NEG
    end if

    return

  end subroutine m_setIntensity

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Free the memory allocated for a pirep_report_t object 
  !
  !> @param[inout] stack - the pirep_report_t stack
  !----------------------------------------------------------------------------

  subroutine m_freePirepStack(stack)
    implicit none
    type(pirep_report_t), target, intent(inout) :: stack

    type(pirep_report_t), pointer :: iterator

    iterator => stack%next

    ! Skip the very first element which is a useless varaible
    do while(associated(iterator))
       iterator => iterator%next
       deallocate(stack%next)
       stack%next => iterator
    end do

    return
  end subroutine m_freePirepStack

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> subroutine to print a Pirep report
  !----------------------------------------------------------------------------

  subroutine printPirep(pirep)
    type(pirep_report_t), intent(in) :: pirep
    write(*,*) "  obsTime=", pirep%obsTime
    write(*,*) "  lat=", pirep%lat
    write(*,*) "  lon=", pirep%lon
    write(*,*) "  fltLevel=", pirep%fltLevel
    write(*,*) "  iceIntens1=", pirep%iceIntens1
    write(*,*) "  iceBase1=", pirep%iceBase1
    write(*,*) "  iceTop1=", pirep%iceTop1
    write(*,*) "  iceIntens2=", pirep%iceIntens2
    write(*,*) "  iceBase2=", pirep%iceBase2
    write(*,*) "  iceTop2=", pirep%iceTop2
  end subroutine printPirep

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Assign observations to grid points, depending on the input model grid
  !> information (kgds)
  !
  !> @param[in] kgds         - model grid information
  !> @param[in] iruntime     - array of run time, YYYY MM DD ZZ HH MM SS MS
  !> @param[in] radius       - within a distance a Pirep pirepCounts
  !> @param[in] pireps       - pireps obtained from BUFR
  !> @param[in] heights      - model heights
  !> @param[inout] pirepData - gridded Pirep data output
  !----------------------------------------------------------------------------

  subroutine gridpointPirep(kgds, iruntime, radius, pireps, heights, pirepData, iret)
    implicit none
    integer, intent(in) :: kgds(:)
    integer, intent(in) :: iruntime(8)
    real, intent(in) :: radius
    type(pirep_report_t), intent(in) :: pireps
    real,    intent(in) :: heights(:,:,:)
    type(pirep_data_t), allocatable, intent(out) :: pirepData(:,:,:)
    integer, intent(out) :: iret

    integer :: nx, ny, nz
    integer, allocatable :: pirepCounts(:, :, :)

    integer(kind=BYTE8) :: lrun_time

    type(pirep_report_t), pointer :: pirepPointer

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! functions outside 
    integer :: IW3JDN

    nx = kgds(2)
    ny = kgds(3)

    nz = size(heights, 3)

    allocate(pirepData(nx, ny, nz), stat=iret)
    pirepData(:,:,:)%interest = 0.0
    pirepData(:,:,:)%weight   = 0.0

    allocate(pirepCounts(nx, ny, nz), stat=iret)
    pirepCounts(:, :, :) = 0

    ! convert run time to minutes since julian day 00:00
    lrun_time = IW3JDN(iruntime(1), iruntime(2), iruntime(3))
    lrun_time = lrun_time*24*60 + iruntime(5)*60 + iruntime(6)

    ! 1/3 step of nearby gridpoints in GridTemplate
    call initNearbyGridPoints(kgds, radius)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! mapping, gridding & calculation of output
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

    ! skip the first useless element
    pirepPointer => pireps%next

    ! loop through the pireps
    ! 2/3 step of nearby gridpoints in GridTemplate
    loop_pirep: do while(associated(pirepPointer))
      call m_mapPirep(lrun_time, nz, heights, pirepPointer, pirepCounts, pirepData)
      pirepPointer => pirepPointer%next
    end do loop_pirep
 
    ! 3/3 step of nearby gridpoints in GridTemplate
    call doneNearbyGridPoints()

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! adjust accordingly
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    where(pirepCounts > 0 .and. pirepData%weight > 0.01)
       pirepData%interest = pirepData%interest / (pirepData%weight*NUM_INTENSITY_LEVELS)
    elsewhere
       pirepData%interest = MISSING
       pirepData%weight   = MISSING
    end where

   ! release memory of pirepCounts
    deallocate(pirepCounts)

    return
  end subroutine gridpointPirep

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> mapping, gridding & calculation of Pirep output
  !
  !> @param[in] lruntime       - minutes since julian day 00:00
  !> @param[in] nz             - model vertical levels
  !> @param[in] heights        - model heights
  !> @param[in] pireps         - pireps obtained from BUFR
  !> @param[inout] pirepCounts - an array to record how many pireps affecting a grid
  !> @param[inout] pirepData   - gridded Pirep data output
  !----------------------------------------------------------------------------

  subroutine m_mapPirep(lrun_time, nz, heights, pirep, pirepCounts, pirepData)
    implicit none
    integer(kind=BYTE8), intent(in) :: lrun_time
    integer, intent(in) :: nz
    real, INTENT(in) :: heights(:, :, :)  ! On pressure level
    type(pirep_report_t), intent(in) :: pirep
    integer,  intent(inout) :: pirepCounts(:, :, :)
    type(pirep_data_t), intent(inout) :: pirepData(:,:,:)

    integer :: deltaT
    real(kind=BYTE8) :: temporalInterest

    real :: dist
    real(kind=BYTE8) :: radialInterest

    ! used for logging and debugging  
    real(kind=BYTE8) :: minRadialInterest, maxRadialInterest
    real(kind=BYTE8) :: minVerticalInterest, maxVerticalInterest
    real(kind=BYTE8) :: minDist, maxDist, minGap, maxGap
    real(kind=BYTE8) :: minWeight, maxWeight, minInterest, maxInterest

    ! there may be two icing observations per pirep -- so handle the situation
    integer :: iceIntensity(2)
    real :: iceBase(2), iceTop(2)

    integer :: ib1, it1, iint1, ib2, it2, iint2
    real(kind=BYTE8) :: pirepAlt
    real :: gap  ! Vertical distance
    real(kind=BYTE8) :: verticalIterest, weight, interest

    ! organize grid points affected by the Pirep report in a stack 
    type(nearby_gridpoint_t) :: nearbyGridPts
    type(nearby_gridpoint_t), pointer :: nearbyPointer

    integer :: i, j, k, m

    deltaT = (lrun_time - pirep%obsTime) * 60 ! convert minutes to seconds
    temporalInterest = m_getTemporalInterest(deltaT)

    ! pull out the icing info
    ib1   = pirep%iceBase1    ! In BUFR, the unit is meter
    it1   = pirep%iceTop1
    iint1 = pirep%iceIntens1
    ib2   = pirep%iceBase2
    it2   = pirep%iceTop2
    iint2 = pirep%iceIntens2
    pirepAlt = pirep%fltLevel ! In BUFR, the unit is meter

    if( (iint1 < 1) .and. (iint1 > -2) ) then ! 0 or -1 is null report
       ! SMUELLER & FMCD TEST
       iint1 = 0
       !return true;
       ! END SMUELLER TEST
    end if


    !!!!! Frank add this !!!!!!!!!!!!!!!!!!!!!!!!
    if( (iint2 < 1) .and. (iint2 > -2) ) then ! 0 or -1 is null report
       ! SMUELLER & FMCD TEST
       iint2 = 0
       !return true;
       ! END SMUELLER TEST
    end if

    ! Modified to use 1-8 scale (vs previous 1-5 scale)
    iceIntensity(1) = iint1
    iceIntensity(2) = iint2

    ! use pirep report altitude if icing top and base values are missing
    if(ib1 > 0) then
       iceBase(1) = ib1
    else
       iceBase(1) = pirepAlt
    end if
    !
    if(it1 > 0) then
       iceTop(1) = it1
    else
       iceTop(1) = pirepAlt
    end if
    !
    if(ib2 > 0) then
       iceBase(2) = ib2
    else
       iceBase(2) = pirepAlt
    end if
    !
    if(it2 > 0) then
       iceTop(2) = it2
    else
       iceTop(2) = pirepAlt
    end if

    ! the following variables used for logging and debugging  
    minRadialInterest = 9999.0
    maxRadialInterest = -9999.0
    minVerticalInterest = 9999.0
    maxVerticalInterest = -9999.0
    minDist = 9999.0
    maxDist = -9999.0
    minGap = 9999.0
    maxGap = -9999.0
    minWeight =  9999.0
    maxWeight =  -9999.0
    minInterest =  9999.0
    maxInterest =  -9999.0

    ! latitude and longitude (valid value)
    call getNearbyGridPoints(pirep%lat, pirep%lon, nearbyGridPts)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Loop through grid points influenced by this report.
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! skip the first useless element
    nearbyPointer => nearbyGridPts%next

    loop_nearbypoints: do while (associated(nearbyPointer))

      i = nearbyPointer%i
      j = nearbyPointer%j
      dist = nearbyPointer%distance

      radialInterest = m_getRadialInterest(dist)

      loop_nIceReports: do m = 1, 2 ! possible icing reports in pirep

        ! don't continue if icing intensity is MISSING
        if( (iceIntensity(m) < -1) ) cycle

        ! don't continue if icing height is MISSING
        if(iceTop(m) < 0 .and. iceBase(m) < 0) cycle

        ! identify the vertical bounds
        loop_nz: do k = 1, nz

          ! 
          ! the smaller gap between the current altitude and the current
          ! icing top and icing base will be the one to test against
          !
          !   test alt.  *  -----------------------
          !                    +               +
          !                    |               |
          !                    | == top gap    |
          !                    |               |
          !                    +               |
          !     top  ---------------           |
          !                                    | == base gap
          !                                    |
          !                                    |
          !                                    |
          !                                    +
          !   base -------------------------------
          !

          gap = min(abs(heights(i, j, k) - iceTop(m)), abs(heights(i, j, k) - iceBase(m)))

          if(gap < VERTICAL_DIST_THRESHOLD) then
             verticalIterest = m_getVerticalInterest(gap)
             weight = radialInterest * verticalIterest * temporalInterest
             interest = weight * iceIntensity(m)

             ! accumulation of two PIREP icing reports
             pirepData(i, j, k)%weight   = pirepData(i, j, k)%weight + weight
             pirepData(i, j, k)%interest = pirepData(i, j, k)%interest + interest
             pirepCounts(i, j, k) = pirepCounts(i, j, k) + 1

             minRadialInterest = min(minRadialInterest, radialInterest)
             maxRadialInterest = max(maxRadialInterest, radialInterest)
             minVerticalInterest = min(minVerticalInterest, verticalIterest)
             maxVerticalInterest = max(maxVerticalInterest, verticalIterest)
             minDist = min(minDist, dist)
             maxDist = max(maxDist, dist)
             minGap = min(minGap, gap)
             maxGap = max(maxGap, gap)
             minInterest = min(minInterest, interest)
             maxInterest = max(maxInterest, interest)
             minWeight = min(minWeight, weight)
             maxWeight = max(maxWeight, weight)
          end if

        end do loop_nz

      end do loop_nIceReports

      nearbyPointer => nearbyPointer%next
    end do loop_nearbypoints

    call freeNearbyGridPoints(nearbyGridPts)

!    write(*,*) "PIREP INFO:\n FL = ", pirepAlt, " lat = ", lat, " lon = ", lon, &
!         " i = ", pirep_i, " j = ", pirep_j
!    write(*,*) "Int1 = ",  iceIntensity(1), " Int2 = ", iceIntensity(2), &
!         " minD = ", minDist, " maxD = ", maxDist, " minG = ", minGap, " maxG = ", maxGap, &
!         " DT = ", deltaT, "\nTI = ", temporalInterest, " minRI = ", minRadialInterest, &
!         " maxRI = ", maxRadialInterest, " minVI = ", minVerticalInterest, " maxVI = ", &
!         maxVerticalInterest, "\nSev1 = ", iceIntensity(1), " Sev2 = ", iceIntensity(2), &
!         " minI = ", minInterest, " maxI = ", maxInterest, &
!         " minW = ", minWeight, " maxW = ", maxWeight,"\n"

    return
  end subroutine m_mapPirep

    
  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Get temporal interest
  !
  !> @param[in] deltaT  - time difference between run time and report time
  !> @return            - temporal interest
  !----------------------------------------------------------------------------

  real(kind=BYTE8) function m_getTemporalInterest(deltaT)
    integer, intent(in) :: deltaT

    real(kind=BYTE8) :: interest, arg
    integer :: break_1, break_2

    interest = 0.0
    break_1 = TEMPORAL_START_THRESHOLD
    break_2 = TEMPORAL_END_THRESHOLD

    if(deltaT < 0.0)  then
       interest = 0.0
    else if(deltaT <= break_1) then
       interest = 1.0
    else if(deltaT <= break_2) then
       arg = (real(deltaT, BYTE8) - break_1) / real((break_2-break_1), BYTE8)
       interest = 1.0 - arg**0.667
    end if

    m_getTemporalInterest = interest

    return
  end function m_getTemporalInterest

  !---------------------------------------------------------------------------- 
  ! DESCRIPTION:
  !> Get temporal interest                                                                      
  !
  !> @param[in] deltaT  - time difference between run time and report time                      
  !> @return            - temporal interest
  !----------------------------------------------------------------------------

  real(kind=BYTE8) function m_getRadialInterest(dist)
    real, intent(in) :: dist

    real(kind=BYTE8) interest, break_1, break_2, arg

    interest = 0.0
    break_1 = 30.0
    break_2 = RADIAL_DIST_THRESHOLD

    if(dist <= break_1) then
       interest = 1.0
    else if(dist <= break_2) then
       arg = (dist-break_1) / (break_2-break_1)
       interest = 1.0 - arg**0.6
    end if

    m_getRadialInterest = interest

    return
  end function m_getRadialInterest

  !----------------------------------------------------------------------------                 
  ! DESCRIPTION:
  !> Get temporal interest                                                                      
  !
  !> @param[in] deltaT  - time difference between run time and report time                      
  !> @return            - temporal interest
  !----------------------------------------------------------------------------

  real(kind=BYTE8) function m_getVerticalInterest(dist)
    real, intent(in) :: dist

   real(kind=BYTE8) interest, break_1

   interest = 0.0
   break_1 = VERTICAL_DIST_THRESHOLD

   if(dist <= break_1)  interest = 1.0 - dist/break_1
  
   m_getVerticalInterest = interest

    return
  end function m_getVerticalInterest

end module Pirep
