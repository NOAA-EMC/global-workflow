!*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
!
! Module:	Config
!
! Author:	Yali Mao
!
! Date:	July 2010, modified in Nov 2011
!
! Description: read configuration file Config.CIP.XXX
!
!*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*

module Config

  ! space, tab, carriage return, line feed
  use Kinds, only : SP, TB, CR, LF

  IMPLICIT NONE

!**********************************************************************
! * private and public declaration
! *

  private
  public config_t, map2D, map3D, sat_calibration_t
  public setConfig, doneConfig, printConfig

!**********************************************************************
! * Configuration types
! *

  !================================================!
  ! map types
  !================================================!
  type :: map2D
     real :: key
     real :: value
  end type map2D

  type :: map3D
     integer :: index
     real :: key
     real :: value
  end type map3D

  !================================================!
  ! Model information
  !================================================!
  type :: m_model_t
     character(len=3) :: name
     integer :: grib
  end type m_model_t

  !================================================!
  ! Model pressure levels
  !================================================!
  type :: m_levels_t
     real :: max_pressure 
     real :: pressure_step
     integer :: num_pressure
     integer :: num_hybrid ! for RAP only
  end type m_levels_t

  !================================================!
  ! Hybrid2Pressure: For RAP
  !================================================!

  type :: m_smooth_info_t
     logical :: extrap_at_top
     logical :: smooth
     integer, dimension(:), allocatable :: passes
     real :: factor
  end type m_smooth_info_t

  type :: m_hybrid2pressure_t
    integer :: below_ground_level_index
    integer :: lapse_rate_top_index
    integer :: extrapolate_offset

    type(m_smooth_info_t) :: hgt ! height
    type(m_smooth_info_t) :: tmp
    type(m_smooth_info_t) :: pvv
    type(m_smooth_info_t) :: wvm
    type(m_smooth_info_t) :: cwm
    type(m_smooth_info_t) :: rwm
    type(m_smooth_info_t) :: snm
    type(m_smooth_info_t) :: icm
    type(m_smooth_info_t) :: gpm
  end type m_hybrid2pressure_t


  !================================================!
  ! Pressure2Flight
  !================================================!

  type :: m_pressure2flight_t
    integer :: num_flight_levels
    real :: start_flight_level
    real :: flight_level_dz
    real :: sld_special_value
  end type m_pressure2flight_t


  !================================================!
  ! Severity Category
  !================================================!
  !---------------------------------------------------
  ! Severity category is integrated in Pressure2Flight,
  ! for the sake of simplification
  !---------------------------------------------------
  ! Notes: 
  !		1 = trace
  !		2 = light
  !		3 = moderate
  !		4 = heavy
  ! Though categories should be integer, 
  ! they are transferred to real when saved as GRIB
  ! For the sake of simplification, 
  ! the categories are input as real, instead of integer
  !
  ! defined in type :: config_t
  ! type(map2D), allocatable :: sev_cat_map(:)

  !================================================!
  ! Metar
  !================================================!
  type :: m_metar_t
     real :: radiusOfInfluence
     real, dimension(:), allocatable :: weatherProcessingLimits
     integer :: weatherProcessingLimits_n
     integer :: minNumMetars
     integer, allocatable :: timeWindow(:) ! in minutes
  end type m_metar_t


  !================================================!
  ! Satellite
  !================================================!

  type :: sat_calibration_t
     character(len=15) :: unit ! unit used by the channel
     ! Represents the lower/upper limit of the calibrated. 
     ! data. Input (uncalibrated) values that do not satisfy
     ! this minimum/maximum are assigned output (calibrated)
     ! values based on the MISSING_DATA_VALUE parameter.
     real :: min
     real :: max
     ! input values which represent bad values
     integer, allocatable :: bad_values(:)
     ! representing the input calibration data.
     ! The following arrays must have the same dimension
     integer, allocatable :: x_values(:)
     real, allocatable :: offset_coeffs(:)   !a in y = a + bx + cx^2
     real, allocatable :: linear_coeffs(:)   !b in y = a + bx + cx^2
     real, allocatable :: quadratic_coeffs(:)!c in y = a + bx + cx^2
  end type sat_calibration_t

  type :: m_satellite_t
     type(sat_calibration_t) :: calibrations(3) ! sequence: VIS, CH2, CH4

     ! satellite mosaic
     character(len=256) :: dir
     character(len=6) :: format
     integer :: nfiner
     type(map2D), allocatable :: ss(:) ! satellite source

     ! satellite derive
     character(len=256) :: shortwaveLUTTable
     real :: micron_110ShortwaveReflThreshold
     real :: solarAngleMax
  end type m_satellite_t

  !================================================!
  ! Pirep
  !================================================!
  type :: m_pirep_t
     integer, allocatable :: timeWindow(:) ! in minutes
  end type m_pirep_t

  !================================================!
  ! Lightning
  !================================================!
  type :: m_Lightning_t
     integer :: ltg_radius
     real :: max_strike_radius
     character(len=3) :: polarity_flag ! POS, NEG, BTH (BOTH)
     integer :: min_amplitude
     integer :: max_amplitude
     integer, allocatable :: timeWindow(:) ! in minutes
  end type m_Lightning_t


  !================================================!
  ! Radar
  !================================================!

  type :: m_Unisys_t
     integer :: valid_source_id
     real :: data_scale
     real :: data_bias
     integer :: igrid ! KGDS(1): projection type in GRIB1
     real :: lat1 ! lambert_lat1
     real :: lat2 ! lambert_lat2
  end type m_Unisys_t

  type :: m_NSSL_t
     ! base refletivity
     integer :: nz_base
     integer, allocatable :: pds7_base(:)
     ! composite
     ! n/a
  end type m_NSSL_t

  type :: m_Radar_t
     type(m_Unisys_t) :: unisys
     type(m_NSSL_t) :: nssl

     ! radar processing
     character(len=10) :: source
     integer :: nfiner
     real, allocatable, dimension(:) :: percentiles
     integer :: minVipPoints
     integer :: minDBzPoints
  end type m_Radar_t

  !================================================!
  ! Configurations for all
  !================================================!

  type :: config_t

     type(m_model_t) :: model

     type(m_levels_t) :: levels

     ! For RAP only
     type(m_hybrid2pressure_t) :: h2p
     
     type(m_pressure2flight_t) :: p2f

     type(map2D), allocatable :: sev_cat_map(:)
    
     type(m_metar_t) :: metar

     type(m_satellite_t) :: sat

     type(m_pirep_t) :: pirep

     type(m_Lightning_t) :: lightning

     type(m_Radar_t) :: radar

  end type config_t

  !================================================!
  ! Local variables, shared by individual calls of m_assignPreviousMemberValue
  !================================================!
  integer :: m_n
  character(len=3) :: m_fields

  !================================================!
  ! interface defined for reading configurations
  !================================================!
  interface m_setValue
     module procedure m_setValue_Integer, m_setValue_Logical, m_setValue_Real
     module procedure m_setValue_Integer_Array, m_setValue_Real_Array, m_setValue_String_Array
     module procedure m_setValue_Map2D, m_setValue_Map3D
  end interface m_setValue

contains


!**********************************************************************
! * Subroutine: setConfig() 
! * set all CIP parameter configurations from config file
! *

  subroutine setConfig(config_file, cfg, iret)
    character(len=*), intent(in) :: config_file
    type(config_t), intent(out) :: cfg
    integer, intent(out) :: iret

    integer, parameter :: MAX_NAMES = 64
    integer, parameter :: MAX_RAWVALUE_LENGTH = 300 ! array may be the longest
    ! Most inputs has only one rawValue(s), seperated by space
    ! Some maps are 2D or 3D rawValues, seperated by comma
    integer, parameter :: MAX_NUMBER_RAWVALUES = 60
    !
    character(len=MAX_NAMES) :: sectionName, memberName
    character(len=MAX_RAWVALUE_LENGTH) :: rawValues(MAX_NUMBER_RAWVALUES)
    integer :: numberOfRawValues

    integer :: unit_config
    character(len=256) :: line

    character(1) :: cfirst
    integer :: ifirst

    integer :: i

    numberOfRawValues = 0

    iret = 0

    unit_config = 30
    open(unit=unit_config, file=config_file, action='read', status='old', iostat=iret)
    if (iret /= 0) then
      write(*, *) 'Config::setConfig() - Cannot open config file ', trim(config_file), ', iostat = ', iret
      return
    end if

    do
       read(unit_config, '(a256)', iostat=iret) line
       if (iret < 0) then !loop breaks at the end of file/record
          iret = 0
          exit
       else if (iret > 0) then
          write(*, *) 'Config::setConfig() - Error in reading line, iostat=', iret
          exit
       end if

       ifirst = m_firstNonBlank(line)
       if(ifirst == 0) then 
          ! === 1/5 ===  blank line, do the same as a comment line
          call m_assignPreviousMemberValue(trim(sectionName), trim(memberName), &
                                          numberOfRawValues, rawValues, cfg, iret)
       else
          cfirst = line(ifirst:ifirst)
          if (cfirst == '#') then
             ! === 2/5 ===  comment line, do the same as a blank line
             call m_assignPreviousMemberValue(trim(sectionName), trim(memberName), &
                                          numberOfRawValues, rawValues, cfg, iret)
          elseif (cfirst == '[') then
             ! === 3/5 === section tag
             call m_assignPreviousMemberValue(trim(sectionName), trim(memberName), &
                                          numberOfRawValues, rawValues, cfg, iret)
             i = index(line, ']')
             if (i > 0) then
                sectionName = trim(line(ifirst+1:i-1))
             else
                write(*, *) "Config::setConfig() - Section syntax error, line must be ended with ] ", trim(line)
                iret = 1
                return
             end if
          elseif(cfirst == ',' .or. cfirst == '+' ) then
             ! === 4/5 === assignment continued line to section member
             if (numberOfRawValues > 0) then
                call m_parseValues(line(ifirst:), numberOfRawValues, rawValues, iret)
             else
                iret = 2
             end if
          else
             i = index(line, '=')
             ! === 5/5 === assignment to section member
             if_i: if (i > 0) then
                call m_assignPreviousMemberValue(trim(sectionName), trim(memberName), &
                     numberOfRawValues, rawValues, cfg, iret)
                call m_parseMember(line, memberName, numberOfRawValues, rawValues, iret)
             else
                iret = 3
             end if if_i
          end if
       end if

       if (iret /= 0) then
          write(*, *) "Config::setConfig() - Section member syntax error on line ", trim(line), " iret =", iret
          exit
       end if

    end do ! do

    close(unit_config, iostat=iret)
    if(iret /= 0) write(*, *)"Config::setConfig() - Unable to close file ", config_file

    return
  end subroutine setConfig


!**********************************************************************
! * Subroutine: doneConfig() 
! *

  subroutine doneConfig(cfg)
    type(config_t), intent(inout) :: cfg

    integer :: i, iret

    ! Hybrid2Pressure: For RAP
    deallocate(cfg%h2p%hgt%passes, stat=iret)
    deallocate(cfg%h2p%tmp%passes, stat=iret)
    deallocate(cfg%h2p%pvv%passes, stat=iret)
    deallocate(cfg%h2p%wvm%passes, stat=iret)
    deallocate(cfg%h2p%cwm%passes, stat=iret)
    deallocate(cfg%h2p%rwm%passes, stat=iret)
    deallocate(cfg%h2p%snm%passes, stat=iret)
    deallocate(cfg%h2p%icm%passes, stat=iret)
    deallocate(cfg%h2p%gpm%passes, stat=iret)

    ! Severity
    deallocate(cfg%sev_cat_map, stat=iret)

    ! Metar
    deallocate(cfg%metar%weatherProcessingLimits, stat=iret)
    deallocate(cfg%metar%timeWindow, stat=iret)

    ! Satellite
    do i = 1, 3 ! VIS, CH2, CH4
       deallocate(cfg%sat%calibrations(i)%bad_values, stat=iret)
       deallocate(cfg%sat%calibrations(i)%x_values, stat=iret)
       deallocate(cfg%sat%calibrations(i)%offset_coeffs, stat=iret)
       deallocate(cfg%sat%calibrations(i)%linear_coeffs, stat=iret)
       deallocate(cfg%sat%calibrations(i)%quadratic_coeffs, stat=iret)
    end do
    deallocate(cfg%sat%ss, stat=iret)

    ! Pirep
    deallocate(cfg%pirep%timeWindow, stat=iret)

    ! Lightning
    deallocate(cfg%lightning%timeWindow, stat=iret)

    ! Radar
    deallocate(cfg%radar%nssl%pds7_base, stat=iret)
    deallocate(cfg%radar%percentiles, stat=iret)

    return
  end subroutine doneConfig


!**********************************************************************
! * Subroutine: m_parseMember()
! * 
! * Description:
! *   parse a line in the form of "name  '='  value" to name and 
! *   values of a memeber
! *

  subroutine m_parseMember(line,  memberName, numberOfRawValues, rawValues, iret)
    character(len=*), intent(in) :: line
    character(len=*), intent(inout) :: memberName
    integer, intent(inout) :: numberOfRawValues
    character(len=*), intent(inout) :: rawValues(:)
    integer, intent(out) :: iret

    integer :: i, iFirst

    iret = 0

    iFirst = m_firstNonBlank(line)
    i = index(line, '=')

    memberName = line(iFirst:i-1) !  no trim, spaces will be appended anyway

    if ((index(trim(memberName), SP) + index(trim(memberName), TB)) > 0) then
      iret = 10
      write(*, *) "Config::m_parseMember() - Blank character inside memeber: ", memberName
      return
    end if

    iFirst =  i + m_firstNonBlank(line(i+1:))
    call m_parseValues(line(iFirst:), numberOfRawValues, rawValues, iret)

    return
  end subroutine m_parseMember

!**********************************************************************
! * Subroutine: m_parseValues()
! * 
! * Description:
! *   parse values. 
! *   The first character is either value or delimited values 
! *    or new line starting with ',' or '+'
! *   
! *

  recursive subroutine m_parseValues(line, numberOfRawValues, rawValues, iret)
    character(len=*), intent(in) :: line
    integer, intent(inout) :: numberOfRawValues
    character(len=*), intent(inout) :: rawValues(:)
    integer, intent(out) :: iret

    integer :: i, iNext

    select case(line(1:1))
    case(',')
       ! 1/4 continued line of map2D/map3D values
       ! get rid of prefix ',' and recursive call
       iNext =  1 + m_firstNonBlank(line(2:))
       call m_parseValues(line(iNext:), numberOfRawValues, rawValues, iret)
    case('+')
       ! 2/4 continued array values
       ! assign value, point where recursion breaks
       ! numberOfRawValues is not increased
       if(len_trim(line(2:)) == 0) then
          iret = -10
       else
          rawValues(numberOfRawValues) = trim(rawValues(numberOfRawValues)) // " "// trim(line(2:))
       end if
    case default
       ! numberOfRawValues is increased by 1
       numberOfRawValues = numberOfRawValues + 1
       i = index(line, ',')
       if (i == 0) then
          ! 3/4 single value or array values
          ! assign value, point where recursion breaks
          if(len_trim(line) == 0) then
             iret = -11
          else
             rawValues(numberOfRawValues) = trim(line)
          end if
       else
          ! 4/4 line of map2D/map3D values
          ! recursive call
          rawValues(numberOfRawValues) = trim(line(:i-1))
          iNext =  i + m_firstNonBlank(line(i+1:))
          ! recursive call
          call m_parseValues(line(iNext:), numberOfRawValues, rawValues, iret)
       end if
    end select

    return
  end subroutine m_parseValues

!**********************************************************************
! * Subroutine: m_firstNonBlank()
! * 
! * Outputs:
! *   return the index of the first non-blank character
! *
  integer function m_firstNonBlank(line)
    character(len=*), intent(in) :: line

    integer :: i

    m_firstNonBlank = 0
    do i = 1, len(trim(line))
      select case(line(i:i))
      case (SP, TB, CR, LF)
        cycle
      case default
        m_firstNonBlank = i
        exit
      end select
    end do

    return
  end function m_firstNonBlank

!**********************************************************************
! * Subroutine: m_assignPreviousMemberValue()
! * 
! * Description:
! *   parse rawValues to assign the appropriate values
! *

  subroutine m_assignPreviousMemberValue(sectionName, &
               memberName, numberOfRawValues, rawValues, cfg, iret)
    IMPLICIT NONE

    character(len=*), intent(in) :: sectionName, memberName
    ! numberOfRawValues will be set to ZERO at the end of assignment
    integer, intent(inout) :: numberOfRawValues
    character(len=*), intent(in) :: rawValues(:)
    type(config_t), target, intent(inout) :: cfg
    integer, intent(out) :: iret

    integer :: i

    if(numberOfRawValues == 0) return

    write(*,*) trim(sectionName), " ", trim(memberName), numberOfRawValues
    write(*,*) (trim(rawValues(i)), "  ", i=1, numberOfRawValues)

    select case(sectionName)
    case("model")		!=====  model  =====!
       select case(memberName)
       case("name")
          select case(trim(ADJUSTL(rawValues(1))))
          case("RAP", "NAM", "GFS")
             cfg%model%name = trim(ADJUSTL(rawValues(1)))
          case default
             write(*, *) "Config::_assignPreviousMemberValue -- only for RAP/NAM/GFS "
             iret = 1
             return
          end select
       case("grib")
          call m_setValue(rawValues(1), cfg%model%grib, iret)
          if(cfg%model%grib /= 1 .and. cfg%model%grib /= 2) then
             write(*, *) "Config::_assignPreviousMemberValue -- only for Grib 1/2 "
             iret = 1
             return
          end if
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("levels")		!=====  levels  =====!
       select case(memberName)
       case("max_pressure")
          call m_setValue(rawValues(1), cfg%levels%max_pressure, iret)
       case("pressure_step")
          call m_setValue(rawValues(1), cfg%levels%pressure_step, iret)
       case("num_pressure")
          call m_setValue(rawValues(1), cfg%levels%num_pressure, iret)
       case("num_hybrid")
          call m_setValue(rawValues(1), cfg%levels%num_hybrid, iret)
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("Hybrid2Pressure")	!=====  Hybrid2Pressure  =====!
       select case(memberName)
       case("below_ground_level_index")
          call m_setValue(rawValues(1), cfg%h2p%below_ground_level_index, iret)
       case("lapse_rate_top_index")
          call m_setValue(rawValues(1), cfg%h2p%lapse_rate_top_index, iret)
       case("extrapolate_offset")
          call m_setValue(rawValues(1), cfg%h2p%extrapolate_offset, iret)
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("Smooth_Info")		!=====  Hybrid2Pressure:Smooth_Info  =====!
       select case(memberName)
       case("field_name")
          m_fields = trim(rawValues(1))
       case("extrap_at_top")
          select case(m_fields)
          case("HGT")
             call m_setValue(rawValues(1), cfg%h2p%hgt%extrap_at_top, iret)
          case("VPT")
             call m_setValue(rawValues(1), cfg%h2p%tmp%extrap_at_top, iret)
          case("PVV")
             call m_setValue(rawValues(1), cfg%h2p%pvv%extrap_at_top, iret)
          case("WVM")
             call m_setValue(rawValues(1), cfg%h2p%wvm%extrap_at_top, iret)
          case("CWM")
             call m_setValue(rawValues(1), cfg%h2p%cwm%extrap_at_top, iret)
          case("RWM")
             call m_setValue(rawValues(1), cfg%h2p%rwm%extrap_at_top, iret)
          case("SNM")
             call m_setValue(rawValues(1), cfg%h2p%snm%extrap_at_top, iret)
          case("ICM")
             call m_setValue(rawValues(1), cfg%h2p%icm%extrap_at_top, iret)
          case("GPM")
             call m_setValue(rawValues(1), cfg%h2p%gpm%extrap_at_top, iret)
          case default
             write(*, *) "Config::_assignPreviousMemberValue -- not a valid field_name ", m_fields
             iret = 1
             return
          end select
       case("smooth")
          select case(m_fields)
          case("HGT")
             call m_setValue(rawValues(1), cfg%h2p%hgt%smooth, iret)
          case("VPT")
             call m_setValue(rawValues(1), cfg%h2p%tmp%smooth, iret)
          case("PVV")
             call m_setValue(rawValues(1), cfg%h2p%pvv%smooth, iret)
          case("WVM")
             call m_setValue(rawValues(1), cfg%h2p%wvm%smooth, iret)
          case("CWM")
             call m_setValue(rawValues(1), cfg%h2p%cwm%smooth, iret)
          case("RWM")
             call m_setValue(rawValues(1), cfg%h2p%rwm%smooth, iret)
          case("SNM")
             call m_setValue(rawValues(1), cfg%h2p%snm%smooth, iret)
          case("ICM")
             call m_setValue(rawValues(1), cfg%h2p%icm%smooth, iret)
          case("GPM")
             call m_setValue(rawValues(1), cfg%h2p%gpm%smooth, iret)
          end select
       case("smoothPasses")
          select case(m_fields)
          case("HGT")
             call m_setValue(rawValues(1), cfg%h2p%hgt%passes, iret)
          case("VPT")
             call m_setValue(rawValues(1), cfg%h2p%tmp%passes, iret)
          case("PVV")
             call m_setValue(rawValues(1), cfg%h2p%pvv%passes, iret)
          case("WVM")
             call m_setValue(rawValues(1), cfg%h2p%wvm%passes, iret)
          case("CWM")
             call m_setValue(rawValues(1), cfg%h2p%cwm%passes, iret)
          case("RWM")
             call m_setValue(rawValues(1), cfg%h2p%rwm%passes, iret)
          case("SNM")
             call m_setValue(rawValues(1), cfg%h2p%snm%passes, iret)
          case("ICM")
             call m_setValue(rawValues(1), cfg%h2p%icm%passes, iret)
          case("GPM")
             call m_setValue(rawValues(1), cfg%h2p%gpm%passes, iret)
          end select
       case("smoothFactor")
          select case(m_fields)
          case("HGT")
             call m_setValue(rawValues(1), cfg%h2p%hgt%factor, iret)
          case("VPT")
             call m_setValue(rawValues(1), cfg%h2p%tmp%factor, iret)
          case("PVV")
             call m_setValue(rawValues(1), cfg%h2p%pvv%factor, iret)
          case("WVM")
             call m_setValue(rawValues(1), cfg%h2p%wvm%factor, iret)
          case("CWM")
             call m_setValue(rawValues(1), cfg%h2p%cwm%factor, iret)
          case("RWM")
             call m_setValue(rawValues(1), cfg%h2p%rwm%factor, iret)
          case("SNM")
             call m_setValue(rawValues(1), cfg%h2p%snm%factor, iret)
          case("ICM")
             call m_setValue(rawValues(1), cfg%h2p%icm%factor, iret)
          case("GPM")
             call m_setValue(rawValues(1), cfg%h2p%gpm%factor, iret)
          end select
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("Pressure2Flight")	!=====  Pressure2Flight  =====!
       select case(memberName)
       case("num_flight_levels")
          call m_setValue(rawValues(1), cfg%p2f%num_flight_levels, iret)
       case("start_flight_level")
          call m_setValue(rawValues(1), cfg%p2f%start_flight_level, iret)
       case("flight_level_dz")
          call m_setValue(rawValues(1), cfg%p2f%flight_level_dz, iret)
       case("sld_special_value")
          call m_setValue(rawValues(1), cfg%p2f%sld_special_value, iret)
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("SeverityCategory")	!=====  Severity Category  =====!
       select case(memberName)
       case("severity_category_map")
          call m_setValue(numberOfRawValues, rawValues, cfg%sev_cat_map, iret)
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("METAR_PARAMS")	!=====  METAR  =====!
       select case(memberName)
       case("radiusOfInfluence")
          call m_setValue(rawValues(1), cfg%metar%radiusOfInfluence, iret)
       case("weatherProcessingLimits")
          call m_setValue(rawValues(1), cfg%metar%weatherProcessingLimits, iret)
          cfg%metar%weatherProcessingLimits_n = size(cfg%metar%weatherProcessingLimits)
       case("minNumMetars")
          call m_setValue(rawValues(1), cfg%metar%minNumMetars, iret)
       case("timeWindow")
          call m_setValue(rawValues(1), cfg%metar%timeWindow, iret)
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("Sat_Calibration")	!=====  Satellite:Calibration  =====!
       select case(memberName)
       case("name")
          select case(trim(rawValues(1)))
          case("VIS")
             m_n = 1
          case("3.9")
             m_n = 2
          case("IR")
             m_n = 3
          case default
             write(*, *) "Config::_assignPreviousMemberValue -- not a valid field_name ", trim(rawValues(1))
             iret = 1
             return
          end select
       case("unit")
          cfg%sat%calibrations(m_n)%unit = trim(rawValues(1))
       case("min")
          call m_setValue(rawValues(1), cfg%sat%calibrations(m_n)%min, iret)
       case("max")
          call m_setValue(rawValues(1), cfg%sat%calibrations(m_n)%max, iret)
       case("bad_input_values")
          call m_setValue(rawValues(1), cfg%sat%calibrations(m_n)%bad_values, iret)
       case("x_values")
          call m_setValue(rawValues(1), cfg%sat%calibrations(m_n)%x_values, iret)
       case("offset_coeffs")
          call m_setValue(rawValues(1), cfg%sat%calibrations(m_n)%offset_coeffs, iret)
       case("linear_coeffs")
          call m_setValue(rawValues(1), cfg%sat%calibrations(m_n)%linear_coeffs, iret)
       case("quadratic_coeffs")
          call m_setValue(rawValues(1), cfg%sat%calibrations(m_n)%quadratic_coeffs, iret)
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("Sat_Mosaic")		!=====  Satellite:Mosaic  =====!
       select case(memberName)
       case("format")
          cfg%sat%format = trim(ADJUSTL(rawValues(1)))
       case("nfiner")
          call m_setValue(rawValues(1), cfg%sat%nfiner, iret)
       case("satellite_source")
          call m_setValue(numberOfRawValues, rawValues, cfg%sat%ss, iret)
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("Sat_Derive")		!=====  Satellite:Derive  =====!
       select case(memberName)
       case("shortwaveLUTTable")
          cfg%sat%shortwaveLUTTable = ADJUSTL(rawValues(1))
       case("micron_110ShortwaveReflThreshold")
          call m_setValue(rawValues(1), cfg%sat%micron_110ShortwaveReflThreshold, iret)
       case("solarAngleMax")
          call m_setValue(rawValues(1), cfg%sat%solarAngleMax, iret)
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("Pirep_Params")	!=====  PIREPs  =====!
       select case(memberName)
       case("timeWindow")
          call m_setValue(rawValues(1), cfg%pirep%timeWindow, iret)
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("Lightning_Params")	!=====  Lightning  =====!
       select case(memberName)
       case("ltg_radius")
          call m_setValue(rawValues(1),cfg%lightning%ltg_radius, iret)
       case("max_strike_radius")
          call m_setValue(rawValues(1), cfg%lightning%max_strike_radius, iret)
       case("polarity_flag")
          cfg%lightning%polarity_flag = ADJUSTL(rawValues(1))
      case("min_amplitude")
          call m_setValue(rawValues(1), cfg%lightning%min_amplitude, iret)
       case("max_amplitude")
          call m_setValue(rawValues(1), cfg%lightning%max_amplitude, iret)
       case("timeWindow")
          call m_setValue(rawValues(1), cfg%lightning%timeWindow, iret)
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("Unisys")		!=====  Radar:Unisys  =====!
       select case(memberName)
       case("valid_source_id")
          call m_setValue(rawValues(1), cfg%radar%unisys%valid_source_id, iret)
       case("data_scale")
          call m_setValue(rawValues(1), cfg%radar%unisys%data_scale, iret)
       case("data_bias")
          call m_setValue(rawValues(1), cfg%radar%unisys%data_bias, iret)
       case("igrid")
          call m_setValue(rawValues(1), cfg%radar%unisys%igrid, iret)
       case("lambert_lat1")
           call m_setValue(rawValues(1), cfg%radar%unisys%lat1, iret) !lambert_lat1
       case("lambert_lat2")
           call m_setValue(rawValues(1), cfg%radar%unisys%lat2, iret) !lambert_lat2
        case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case("NSSL")		!=====  Radar:NSSL  =====!
       select case(memberName)
      case("nz_base")
          call m_setValue(rawValues(1), cfg%radar%nssl%nz_base, iret)
      case("pds7_base")
          call m_setValue(rawValues(1), cfg%radar%nssl%pds7_base, iret)
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if
 
    case("RadarProcessing")	!=====  Radar:Processing  =====!
       select case(memberName)
       case("radar_source")
          cfg%radar%source = ADJUSTL(rawValues(1))
       case("nfiner")
          call m_setValue(rawValues(1), cfg%radar%nfiner, iret)
       case("percentiles")
          call m_setValue(rawValues(1), cfg%radar%percentiles, iret)
       case("minVipPoints")
          call m_setValue(rawValues(1), cfg%radar%minVipPoints, iret)
       case("minDBzPoints")
          call m_setValue(rawValues(1), cfg%radar%minDBzPoints, iret)
       case default
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " doesn't include member: ", memberName
          iret = 1
          return
       end select

       if (iret /= 0) then
          write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, "::member ", memberName, " setting error"
          return
       end if

    case default
       write(*, *) "Config::_assignPreviousMemberValue -- Section ", sectionName, " is not necessory"
       iret = 1
    end select

    numberOfRawValues = 0

    return
  end subroutine m_assignPreviousMemberValue

!**********************************************************************
! * function:_lowercase()
! * 
! * Description:
! *   convert every letter in a string to lower case
! 

  function m_lowercase(string)
    character (len=*), intent(in) :: string
    character (len=len(string)) :: m_lowercase

    integer :: gap, i
    character :: ch

    gap = ichar('A') - ichar('a')
    m_lowercase = string

    do i = 1, len(string)
      ch = string(i:i)
      if ((ch >= 'A') .and. (ch <= 'Z')) &
           m_lowercase(i:i) = char(ichar(ch) - gap)
   end do

    return
  end function m_lowercase


!**********************************************************************
! * Subroutine: m_numberOfValues()
! * 
! * Description:
! *   get how many values delimited by ' ' 
! *   Recursive
! *

  recursive integer function m_numberOfValues(rawValues) result(ans)
    implicit none

    character(len=*), intent(in) :: rawValues

    integer :: i, istart
    i = index(trim(rawValues), " ", back=.true.)
    istart = m_firstNonBlank(rawValues)

    if (i < istart) then ! where recursion breaks
       ans = 1
       return
    else  if (i > istart) then ! conintue...
       i = istart + index(rawValues(istart:), " ")
       ans = 1 +  m_numberOfValues(trim(rawValues(i:)))
    end if

    return
  end function m_numberOfValues


!**********************************************************************
! * Subroutine: m_setValue_Logical()
! * 
! * Description:
! *   set a logical value depending on the raw string inputs
! *
! * Inputs:
! *   rawValues - the raw string input
! *   var       - the variable which will be assigned a value
! *   iret      - error signal, 0 is successful, 1 is failed.
! *

  subroutine m_setValue_Logical(rawValues, var, iret)
    character(len=*), intent(in) :: rawValues
    logical, intent(out) :: var
    integer, intent(out) :: iret

    iret = 0

    if (m_lowercase(trim(rawValues)) == "true") then
      var = .true.
    else if (m_lowercase(trim(rawValues)) == "false") then
      var = .false.
    else
      iret = 1
    end if

    return
  end subroutine m_setValue_Logical

!**********************************************************************
! * Subroutine: m_setValue_Integer()
! * 
! * Description:
! *   set an integer value depending on the raw string inputs
! *
! * Inputs:
! *   rawValues - the raw string input
! *   var       - the variable which will be assigned a value
! *   iret      - error signal, 0 is successful, 1 is failed.
! *

  subroutine m_setValue_Integer(rawValues, var, iret)
    character(len=*), intent(in) :: rawValues
    integer, intent(out) :: var
    integer, intent(out) :: iret

    read(rawValues, *, iostat=iret) var

    return
  end subroutine m_setValue_Integer

!**********************************************************************
! * Subroutine: m_setValue_Real()
! * 
! * Description:
! *   set a real value depending on the raw string inputs
! *
! * Inputs:
! *   rawValues - the raw string input
! *   var       - the variable which will be assigned a value
! *   iret      - error signal, 0 is successful, 1 is failed.
! *

  subroutine m_setValue_Real(rawValues, var, iret)
    character(len=*), intent(in) :: rawValues
    real, intent(out) :: var
    integer, intent(out) :: iret

    read(rawValues, *, iostat=iret) var

    return
  end subroutine m_setValue_Real

!**********************************************************************
! * Subroutine: m_setValue_Integer_Array()
! * 
! * Description:
! *   allocate and set an integer array depending on the raw string inputs
! *
! * Inputs:
! *   rawValues - the raw string input
! *   var       - the variable which will be assigned a value
! *   iret      - error signal, 0 is successful, 1 is failed.
! *

  recursive subroutine m_setValue_Integer_Array(rawValues, vars, iret)
    character(len=*), intent(in) :: rawValues
    integer, dimension(:), allocatable, intent(inout) :: vars
    integer, intent(inout) :: iret

    integer :: numberOfValues, i

    ! call recursive function
    numberOfValues = m_numberOfValues(rawValues)

    allocate(vars(numberOfValues), stat=iret)

    read(rawValues, *, iostat=iret) (vars(i), i = 1, numberOfValues)

    return
  end subroutine m_setValue_Integer_Array

!**********************************************************************
! * Subroutine: m_setValue_Real_Array()
! * 
! * Description:
! *   set a real vector depending on the raw string inputs
! *
! * Inputs:
! *   rawValues - the raw string input
! *   var       - the variable which will be assigned a value
! *   iret      - error signal, 0 is successful, 1 is failed.
! *

  recursive subroutine m_setValue_Real_Array(rawValues, vars, iret)
    character(len=*), intent(in) :: rawValues
    real, dimension(:), allocatable, intent(inout) :: vars
    integer, intent(inout) :: iret

    integer :: numberOfValues, i

    ! call recursive function
    numberOfValues = m_numberOfValues(rawValues)

    allocate(vars(numberOfValues), stat=iret)

    read(rawValues, *, iostat=iret) (vars(i), i = 1, numberOfValues)

    return
  end subroutine m_setValue_Real_Array

!**********************************************************************
! * Subroutine: m_setValue_String_Array()
! * 
! * Description:
! *   set a string array depending on the raw string inputs
! *
! * Inputs:
! *   rawValues - the raw string input
! *   var       - the variable which will be assigned a value
! *   iret      - error signal, 0 is successful, 1 is failed.
! *

  recursive subroutine m_setValue_String_Array(rawValues, vars, iret)
    character(len=*), intent(in) :: rawValues
    character(256), dimension(:), allocatable, intent(inout) :: vars
    integer, intent(inout) :: iret

    integer :: numberOfValues, i, ii, istart, inext

    iret = -1

    ! call recursive function
    numberOfValues = m_numberOfValues(rawValues)

    if(numberOfValues <= 0) return
    allocate(character(256)::vars(numberOfValues), stat=iret)


    i = 1
    ii = m_firstNonBlank(rawValues)
    istart = ii
    do while (ii > 0)
       inext = istart + index(rawValues(istart:) // " ", " ")
       vars(i) = rawValues(istart:inext-1)
       ii = m_firstNonBlank(rawValues(inext:))
       istart = inext-1 + ii
       i = i + 1
    end do

    return
  end subroutine m_setValue_String_Array

!**********************************************************************
! * Subroutine: m_setValue_Map2D()
! * 
! * Description:
! *   set map values
! *
! * Inputs:
! *   rawValues - the raw array of string input
! *   var       - the variable which will be assigned a value
! *   iret      - error signal, 0 is successful, 1 is failed.
! *

  subroutine m_setValue_Map2D(numberOfRawValues, rawValues, map, iret)
    integer, intent(in) :: numberOfRawValues
    character(len=*), intent(in) :: rawValues(:)
    type(map2D), allocatable, intent(inout) :: map(:)
    integer, intent(inout) :: iret

    integer :: i

    allocate(map(numberOfRawValues), stat=iret)

    do i = 1, numberOfRawValues
       read(rawValues(i), *, iostat=iret) map(i)%key, map(i)%value
    end do

    return
  end subroutine m_setValue_Map2D

!**********************************************************************
! * Subroutine: m_setValue_Map3D()
! * 
! * Description:
! *   set 3D map values
! *
! * Inputs:
! *   rawValues - the raw array of string input
! *   var       - the variable which will be assigned a value
! *   iret      - error signal, 0 is successful, 1 is failed.
! *

  subroutine m_setValue_Map3D(numberOfRawValues, rawValues, map, iret)
    integer, intent(in) :: numberOfRawValues
    character(len=*), intent(in) :: rawValues(:)
    type(map3D), allocatable, intent(inout) :: map(:)
    integer, intent(inout) :: iret

    integer :: i

    allocate(map(numberOfRawValues), stat=iret)

    do i = 1, numberOfRawValues
       read(rawValues(i), *, iostat=iret) map(i)%index, map(i)%key, map(i)%value
    end do

    return
  end subroutine m_setValue_Map3D

  subroutine printConfig(cfg)
    type(config_t), intent(in) :: cfg

    integer :: i

    write(*,*) LF, "==================== Configurations ========================="
    write(*,*) "model=", cfg%model%name,  cfg%model%grib
    write(*,*) "levels=", cfg%levels 
    write(*,*) "hybrid2pressure=",cfg% h2p% below_ground_level_index
    write(*,*) cfg% h2p% lapse_rate_top_index
    write(*,*) cfg% h2p% extrapolate_offset
    write(*,*) cfg% h2p%hgt%extrap_at_top
    write(*,*) cfg% h2p%hgt%smooth
    write(*,*) cfg% h2p%hgt%passes
    write(*,*) cfg% h2p%hgt%factor
    write(*,*) cfg% h2p%pvv%extrap_at_top
    write(*,*) cfg% h2p%pvv%smooth
    write(*,*) cfg% h2p%pvv%passes
    write(*,*) cfg% h2p%pvv%factor
    write(*,*) cfg% h2p%snm%extrap_at_top
    write(*,*) cfg% h2p%snm%smooth
    write(*,*) cfg% h2p%snm%passes
    write(*,*) cfg% h2p%snm%factor
    write(*,*) cfg% h2p%gpm%extrap_at_top
    write(*,*) cfg% h2p%gpm%smooth
    write(*,*) cfg% h2p%gpm%passes
    write(*,*) cfg% h2p%gpm%factor
    write(*,*) "pressure2flight=",cfg% p2f
    write(*,*) "severity category map=",cfg% sev_cat_map
    write(*,*) "metar=", cfg% metar%radiusOfInfluence
    write(*,*) cfg% metar%weatherProcessingLimits
    write(*,*) cfg% metar%weatherProcessingLimits_n
    write(*,*) cfg% metar%minNumMetars
    write(*,*) cfg% metar%timeWindow
    write(*,*) "satellite=", cfg% sat%calibrations(1)%min
    write(*,*) cfg% sat%calibrations(1)%max
    write(*,*) cfg% sat%calibrations(1)%bad_values
    write(*,*) cfg% sat%calibrations(1)%x_values
    write(*,*) cfg% sat%calibrations(1)%offset_coeffs
    write(*,*) cfg% sat%calibrations(1)%linear_coeffs
    write(*,*) cfg% sat%calibrations(1)%quadratic_coeffs
    write(*,*) cfg% sat%calibrations(2)%min
    write(*,*) cfg% sat%calibrations(2)%max
    write(*,*) cfg% sat%calibrations(2)%bad_values
    write(*,*) cfg% sat%calibrations(2)%x_values
    write(*,*) cfg% sat%calibrations(2)%offset_coeffs
    write(*,*) cfg% sat%calibrations(2)%linear_coeffs
    write(*,*) cfg% sat%calibrations(2)%quadratic_coeffs
    write(*,*) cfg% sat%calibrations(3)%min
    write(*,*) cfg% sat%calibrations(3)%max
    write(*,*) cfg% sat%calibrations(3)%bad_values
    write(*,*) cfg% sat%calibrations(3)%x_values
    write(*,*) cfg% sat%calibrations(3)%offset_coeffs
    write(*,*) cfg% sat%calibrations(3)%linear_coeffs
    write(*,*) cfg% sat%calibrations(3)%quadratic_coeffs
    write(*,*) cfg% sat%format
    write(*,*) cfg% sat%nfiner
    write(*,*) cfg% sat%ss
    write(*,*) cfg% sat%micron_110ShortwaveReflThreshold
    write(*,*) trim(cfg% sat%shortwaveLUTTable)
    write(*,*) cfg% sat%solarAngleMax
    write(*,*) "pirep=", cfg% pirep%timeWindow
    write(*,*) "lightning=", cfg% lightning%ltg_radius
    write(*,*) cfg% lightning%max_strike_radius
    write(*,*) cfg% lightning%polarity_flag
    write(*,*) cfg% lightning%min_amplitude
    write(*,*) cfg% lightning%max_amplitude
    write(*,*) cfg% lightning%timeWindow
    write(*,*) "radar=", cfg%radar%unisys
    write(*,*) cfg%radar%nssl%nz_base
    write(*,*) cfg%radar%nssl%pds7_base
    write(*,*) cfg%radar%source
    write(*,*) cfg%radar%percentiles
    write(*,*) cfg%radar%minVipPoints
    write(*,*) cfg%radar%minDBzPoints
  end subroutine printConfig

end module Config
