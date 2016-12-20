!------------------------------------------------------------------------------
!
! MODULE: Lightning
!
! DESCRIPTION:
!> Read lightning data from dump bufr
!
! REVISION HISTORY:
! September 2011                                                                                  
! January 2014 - modified
!
! Notes:       CIP should continue to run if no lightning exists
!
!------------------------------------------------------------------------------                    

module Lightning
  use Kinds
  use GridTemplate
  use Config

  implicit none

  private
  public runLightning

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! Structures
  type :: LTG_strike_t
     real :: latitude
     real :: longitude
     integer :: amplitude
     type(LTG_strike_t), pointer :: next
  end type LTG_strike_t

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! private members (configration related)
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! Lightning effecting radius in grid units. If set to 0, only the grid square
  ! in which the lightning strike occurred will include the strike. If set
  ! higher, any grid square whose center lies in the circle of the given radius
  ! from the center of the grid square in which the strike occurred will be
  ! affected by the strike (its total will be incremented).
  real :: CFG_RadiusOfRate	! = 0.0
  ! Maximum radius/distance(km) to lightning strike
  real :: CFG_RadiusOfDistance	! = 50.0
  ! polarity of the lightning strikes
  ! POS(polar postive), NEG(polar negative), BTH(polar both)
  character(3) :: CFG_Polarity_Flag	! = "BTH"
  ! Minimum amplitude for strikes to be included in the resulting grids. 
  ! If less than zero, no minimum amplitude is used.
  real :: CFG_Min_Amplitude 	! = -1
  ! Maximum amplitude for strikes to be included in the resulting grids. 
  ! If less than zero, no maximum amplitude is used.
  real :: CFG_Max_Amplitude 	! = -1
  integer, pointer :: CFG_TimeWindow(:) ! data collecting time window, in minutes

contains 

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> public subroutine to be called to read and grid Lightning data
  !
  !> @param[in] filename   - input bufr file name
  !> @param[in] iruntime   - array of run time, YYYY MM DD ZZ HH MM SS MS
  !> @param[in] cfg        - configuration file 
  !> @param[in] kgds       - model grid information
  !> @param[out] lightningData - gridded lightning data output
  !> @param[out] iret          - status; -1 if failure
  !
  !----------------------------------------------------------------------------
  subroutine runLightning(filename, iruntime, cfg, kgds, lightningData, iret)
    implicit none
    character(len=*),  intent(in) :: filename
    ! YYYY MM DD ZZ HH MM SS MS to call /nwprod/lib/sorc/w3nco/w3movdat.f
    integer, intent(in) :: iruntime(8)
    type(config_t), target, intent(in) :: cfg
    integer, intent(in) :: kgds(:)
    type(lightning_data_t), allocatable, intent(out) :: lightningData(:,:)
    integer, intent(out) :: iret

    type(LTG_strike_t) :: strikes
    integer :: nstrikes

    ! configuration fields
    CFG_RadiusOfRate = cfg%lightning%ltg_radius
    CFG_RadiusOfDistance = cfg%lightning%max_strike_radius
    CFG_Polarity_Flag = cfg%lightning%polarity_flag
    CFG_Min_Amplitude = cfg%lightning%min_amplitude
    CFG_Max_Amplitude = cfg%lightning%max_amplitude
    CFG_TimeWindow => cfg%lightning% timeWindow 

    iret = -1

    ! Read in lightning data into strikes
    nstrikes = getLightning(filename, iruntime, CFG_TimeWindow, strikes)
    write(*,*) "There are ", nstrikes, "lightning strikes reported."

    ! Grid strikes to lightningData
    if(nStrikes > 0) call gridpointLightning(kgds, CFG_RadiusOfRate, &
                       CFG_RadiusOfDistance, strikes, lightningData, iret)

    ! release memory of strikes
    call m_freeStrikesStack(strikes)
  end subroutine runLightning

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Read lightning data from BUFR dump file into strikes (a stack).
  !> Collect data in [-15, +15] minute time window of the run hour.
  !
  !> @param[in] infile     - input bufr file name
  !> @param[in] iruntime   - array of run time, YYYY MM DD ZZ HH MM SS MS
  !> @param[in] timeWindow - data collecting time window, in minutes
  !> @param[out] strikes
  !> @Return: the number of strikes; -1 if failure
  !----------------------------------------------------------------------------

  integer function getLightning(infile, iruntime, timeWindow, strikes)
    implicit none
    character(len=*),  intent(in) :: infile
    ! YYYY MM DD ZZ HH MM SS MS to call /nwprod/lib/sorc/w3nco/w3movdat.f
    integer, intent(in) :: iruntime(8)
    integer, intent(in) :: timeWindow(2)  ! in minutes
    type(LTG_strike_t), target, intent(out) :: strikes

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! BUFR related variables
    integer :: ireadmg, ireadsb ! bufr lib functions
    integer :: iunit            ! bufr file unit
    ! BUFR message level information
    integer(kind=BYTE8) :: report_date
    character(len=BYTE8) :: msgtype
    ! For location information
    ! FLVL: FLIGHT LEVEL, in meter
    character(*), parameter :: headerString = 'YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH AMPLS'
    real(kind=BYTE8) :: headers(9)
    integer :: ireport_time(5) ! YYYY MM DD HH MM

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! other variables
    type(LTG_strike_t), pointer :: strikePointer
    integer :: iret, i
    integer :: mvtime(8) ! YYYY MM DD ZZ HH MM SS MS
    real    :: rinc(5)  ! interval (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
    character(len=12) :: start_time, end_time, report_time  ! YYYYMMDDHHMM
    real :: amplitude

    getLightning = 0 ! No strikes
    nullify(strikes%next)

    ! Convert time window to YYYYMMDDHHMM
    rinc(:) = 0.
    rinc(3) = timeWindow(1)
    call W3MOVDAT(rinc, iruntime, mvtime)
    write(start_time, "((I4,4I2.2))") mvtime(1),mvtime(2),mvtime(3),mvtime(5),mvtime(6)
    rinc(3) = timeWindow(2)
    call W3MOVDAT(rinc, iruntime, mvtime)
    write(end_time, "((I4,4I2.2))") mvtime(1),mvtime(2),mvtime(3),mvtime(5),mvtime(6)

    iunit = 15
    call closbf(iunit)
    open(iunit,file=infile,form='unformatted', iostat=iret)
    if(iret /= 0) then
       getLightning = -1
       write(*,*) "Lightning::getLightning() not able to open input Lightning BUFR file=", trim(infile)
       return
    end if
    call openbf(iunit,'IN',iunit)
    call datelen(10)

    loop_msg: do while(ireadmg(iunit, msgtype, report_date) == 0)

      if(msgtype /= 'NC007001' .and. msgtype /= 'NC007002')  cycle

      loop_readsb: do while(ireadsb(iunit) == 0)

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        ! extract information
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

        ! time, latitude, longitude, Amplitude, Plolarity
        !     time=(YEAR MNTH DAYS HOUR MINU SECO)          
        ! headerString = 'YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH AMPLS PLRTS'

        call ufbint(iunit, headers, size(headers), 1, iret, headerString)

        ! only collect lightning data in the time window
        ireport_time(1:5) = headers(1:5)
        write(report_time, "((I4,4I2.2))") (ireport_time(i), i=1,5)
        if(report_time < start_time .or. report_time > end_time) cycle
!write(*,*) "report time ", report_time, " ",  start_time, " ", end_time

        ! if the information is incomplete, do not continue
        if(headers(7) > BUFR_MISSING .or. headers(8) > BUFR_MISSING .or. &
           headers(9) > BUFR_MISSING) cycle

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        ! add the strike to strikes if accepted
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        amplitude = headers(9)
        if(m_acceptStrike(amplitude)) then
          allocate(strikePointer)

          strikePointer%latitude  = headers(7)
          strikePointer%longitude = headers(8)
          strikePointer%amplitude = headers(9)

          strikePointer%next => strikes%next
          strikes%next => strikePointer

          getLightning = getLightning + 1
        end if

       end do loop_readsb

    end do loop_msg
    close(iunit)

    return

  end function getLightning


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> check whether to accept the strike
  !
  !> @param[in] amplitude - amplitude of a strike
  !> @Return: true if qualified; false otherwise
  !----------------------------------------------------------------------------

  logical function m_acceptStrike(amplitude)
    implicit none
    real, intent(in) :: amplitude

    real :: amp_magnitude

    m_acceptStrike = .true.

    ! Check the polarity of the strike
    if((CFG_Polarity_Flag == 'POS' .and. amplitude < 0) .or. &
       (CFG_Polarity_Flag == 'NEG' .and. amplitude > 0)) then
      m_acceptStrike = .false.
      write(*,*) "Lightning::m_acceptStrike():  Lightning data with wrong polarity"
      write(*,*) "     amplitude", amplitude, " polarity_flag:POLAR_",CFG_Polarity_Flag
    end if

    ! Check the amplitude
    amp_magnitude = abs(amplitude)
    if ((CFG_Min_Amplitude >= 0 .and. amp_magnitude < CFG_Min_Amplitude) .or. &
        (CFG_Max_Amplitude >= 0 .and. amp_magnitude > CFG_Max_Amplitude)) then
      m_acceptStrike = .false.
      write(*,*) "Lightning::m_acceptStrike():  Lightning data outside amplitude range"
      write(*,*) "  amplitude:", amplitude, ", min_amplitude: ", CFG_Min_Amplitude, ", max_amplitude: ", CFG_Max_Amplitude
    end if

    return
  end function m_acceptStrike

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Free the memory allocated for a LTG_strike_t object 
  !
  !> @param[inout] stack - the LTG_strike_t stack
  !----------------------------------------------------------------------------

  subroutine m_freeStrikesStack(stack)
    implicit none
    type(LTG_strike_t), target, intent(inout) :: stack

    type(LTG_strike_t), pointer :: iterator

    iterator => stack%next

    ! Skip the very first element which is a useless varaible
    do while(associated(iterator))
       iterator => iterator%next
       deallocate(stack%next)
       stack%next => iterator
    end do
    
    return
  end subroutine m_freeStrikesStack


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Print a strike record
  !
  !> @param[in] strike - a strike record
  !----------------------------------------------------------------------------
  subroutine printStrike(aStrike)
    implicit none
    type(LTG_strike_t) :: aStrike

    write(*,*) "strike values: latitude=", aStrike%latitude, &
               "               longitude=", aStrike%longitude, &
               "               amplitude=", aStrike%amplitude

    return
  end subroutine printStrike

!------------------------------------------------------------------------------
! The following functions/subroutines are all about gridding
!------------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Assign observations to grid points, depending on the input model grid
  !> information (kgds)
  !
  !> @param[in] kgds             - model grid information
  !> @param[in] radius1          - within a distance a strike counts
  !> @param[in] radius2          - within a distance a strike influences
  !> @param[in] strikes          - strikes obtained from BUFR
  !> @param[inout] lightningData - gridded lightning data output
  !----------------------------------------------------------------------------
  subroutine gridpointLightning(kgds, radius1, radius2, strikes, lightningData, iret)
    implicit none
    integer, intent(in) :: kgds(:)
    real, intent(in) :: radius1, radius2
    type(LTG_strike_t), intent(in) :: strikes
    type(lightning_data_t), allocatable, intent(out) :: lightningData(:,:)
    integer, intent(out) :: iret

    integer :: nx, ny, i, j

    nx = kgds(2)
    ny = kgds(3)

    allocate(lightningData(nx, ny), stat=iret)
    lightningData(:,:)%dist = MISSING
    lightningData(:,:)%rate = 0.0

    ! the following field calculations have different GridTemplate

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Create a circular template object for rate.
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! 1/3 step of nearby gridpoints in GridTemplate
    call initNearbyGridPoints(kgds, radius1)

    ! 2/3 step of nearby gridpoints in GridTemplate
    call m_loadRateField(strikes, lightningData)
 
    ! 3/3 step of nearby gridpoints in GridTemplate
    call doneNearbyGridPoints()

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Create a circular template object for distance.
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! 1/3 step of nearby gridpoints in GridTemplate
    call initNearbyGridPoints(kgds, radius2)

    ! 2/3 step of nearby gridpoints in GridTemplate
    call m_loadDistanceField(strikes, lightningData)

    ! 3/3 step of nearby gridpoints in GridTemplate
    call doneNearbyGridPoints()

    return
  end subroutine gridpointLightning

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> set the rate field 
  !
  !> @param[in] strikes          - strikes obtained from BUFR
  !> @param[inout] lightningData - gridded lightning data output
  !----------------------------------------------------------------------------
 
  subroutine m_loadRateField(strikes, ltgData)
    implicit none
    type(LTG_strike_t), target, intent(in) :: strikes
    type(lightning_data_t), intent(inout) :: ltgData(:, :)
    
    type(nearby_gridpoint_t) :: nearbyGridPts
    type(nearby_gridpoint_t), pointer :: nearbyPointer

    type(LTG_strike_t), pointer :: strikePointer


    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! loop through the strikes
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

    ! skip the first useless element
    strikePointer => strikes%next

    loop_strike: do while(associated(strikePointer))
      call getNearbyGridPoints(strikePointer%latitude, strikePointer%longitude, nearbyGridPts)

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
      ! Loop through grid points influenced by this strike.
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
      ! skip the first useless element
      nearbyPointer => nearbyGridPts%next
      loop_nearbypoints: do while (associated(nearbyPointer))
        ltgData(nearbyPointer%i, nearbyPointer%j)%rate = ltgData(nearbyPointer%i, nearbyPointer%j)%rate + 1

        nearbyPointer => nearbyPointer%next
      end do loop_nearbypoints

      strikePointer => strikePointer%next

      call freeNearbyGridPoints(nearbyGridPts)      
    end do loop_strike

    return
  end subroutine m_loadRateField

  !----------------------------------------------------------------------------                   
  ! DESCRIPTION:                                                                                  
  !> set the distance field                                                                           
  !                                                                                               
  !> @param[in] strikes          - strikes obtained from BUFR
  !> @param[inout] lightningData - gridded lightning data output
  !----------------------------------------------------------------------------

  subroutine m_loadDistanceField(strikes, ltgData)
    implicit none
    type(LTG_strike_t), target, intent(in) :: strikes
    type(lightning_data_t), intent(inout) :: ltgData(:, :)

    type(nearby_gridpoint_t) :: nearbyGridPts
    type(nearby_gridpoint_t), pointer :: nearbyPointer

    type(LTG_strike_t), pointer :: strikePointer


    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! loop through the strikes
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

    ! skip the first useless element
    strikePointer => strikes%next

    loop_strike: do while(associated(strikePointer))
      call getNearbyGridPoints(strikePointer%latitude, strikePointer%longitude, nearbyGridPts)

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
      ! Loop through grid points influenced by this strike.
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
      ! skip the first useless element
      nearbyPointer => nearbyGridPts%next
      loop_nearbypoints: do while (associated(nearbyPointer))
        if(ltgData(nearbyPointer%i, nearbyPointer%j)%dist < MISSING + 1.) then
          ltgData(nearbyPointer%i, nearbyPointer%j)%dist = nearbyPointer%distance
        elseif(ltgData(nearbyPointer%i, nearbyPointer%j)%dist > nearbyPointer%distance) then
          ltgData(nearbyPointer%i, nearbyPointer%j)%dist = nearbyPointer%distance
        endif
        nearbyPointer => nearbyPointer%next
      end do loop_nearbypoints

      strikePointer => strikePointer%next

      call freeNearbyGridPoints(nearbyGridPts)

    end do loop_strike

    return
  end subroutine m_loadDistanceField

end module Lightning
