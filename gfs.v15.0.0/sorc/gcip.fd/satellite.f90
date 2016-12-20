!------------------------------------------------------------------------------
!
! MODULE: Satellite
!
! DESCRIPTION:
!> Ingest satellite data
!>   - calibration
!>   - re-projection to model grid @ finer resolution
!> Derive other fields
!>   - Angels calcuation are sector/satellite related
!
! REVISION HISTORY:
! January 2012
! March 2014 - modified
!
!------------------------------------------------------------------------------
MODULE Satellite
  use Kinds
  use Config
  use GridTemplate, only : zoominGDS, convertProjection

  IMPLICIT NONE

  PRIVATE
  PUBLIC runSat

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! angles parameters and type(s)
  ! mean distance from the center of the sun to the earth
  REAL(kind=BYTE8), PARAMETER :: Earth2SunDist = 1.4959965e8   !kilometers 
  ! mean distance from a geostationary satellite to the center of the earth
  REAL(kind=BYTE8), PARAMETER :: GeostatAltitude = 35786.0     !kilometers
  REAL(kind=BYTE8), PARAMETER :: EquatorEarthRadius = 6378.145 !kilometers
  ! type(s)
  TYPE :: xyz_t
     REAL(kind=BYTE8) :: x
     REAL(kind=BYTE8) :: y
     REAL(kind=BYTE8) :: z
  END TYPE xyz_t

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! shortwave reflectance parameters
  INTEGER, PARAMETER :: NUM_SAT_ARF = 40
  INTEGER, PARAMETER :: NUM_SUN_ARF = 40
  INTEGER, PARAMETER :: NUM_REL_AZIM_ARF = 37

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! satellite icing thresholds
  REAL, parameter :: SatZenithThreshold = 75.0
  REAL, parameter :: CH1_Threshold = 25.0
  REAL, parameter :: CH2_RelfectanceThreshold = 8.0
  REAL, parameter :: CH4_MinThreshold = -20.0
  REAL, parameter :: CH4_MaxThreshold = 0.0
  REAL, parameter :: CH2MinusCH4Threshold = 10.0

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! private members (configration related)
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! calibrations
  type(sat_calibration_t), pointer :: CFG_calibrations(:)
  ! mosaic
  character(6) :: CFG_format		! satellite data format MCIDAS/NETCDF
  integer :: CFG_nfiner 		! resolution ratio of satellite to model 
  type(map2D), pointer :: CFG_ss(:) 	! satellite locations
  ! derive 
  character(256) :: CFG_arfLUTfile 	! ARF lookup table for shortwave reflectance
  real :: CFG_MICRON_110ShortwaveReflThreshold ! = -60.0 ! > -300. and < 50.
  real :: CFG_SolarAngleMax		! = 80.


CONTAINS

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Data files should be prepared: ONLY ONE complete set, with time closest to
  !> the run_time, is provided.
  !
  !> @param[in] filenames  - input satellite files: vis, sir lir ss
  !> @param[in] iruntime   - array of  YYYY MM DD ZZ HH MM SS MS
  !> @param[in] cfg    	   _ configurations
  !> @param[in] kgds       - grid information
  !> @param[out] satData   - type of satellite_data_t
  !> @param[out] iret      - status; -1 if failure
  !
  ! Note:
  !  filenames: For GINI: East - 3 files; West - 3 files
  !             For McIDAS: 4 files, vis, shortwave, infrared, sensor source
  !----------------------------------------------------------------------------

  SUBROUTINE runSat(filenames, iruntime, cfg, kgds, satData, iret)
    IMPLICIT NONE
    character(len=*), intent(in) :: filenames(:)
    ! YYYY MM DD ZZ HH MM SS MS to call /nwprod/lib/sorc/w3nco/w3movdat.f
    integer, intent(in) :: iruntime(8) 
    type(config_t), target :: cfg
    integer, intent(in) :: kgds(:)
    type(satellite_data_t), target, INTENT(out) :: satData
    integer, intent(out) :: iret

    ! since 00:00, Jan 1 1970 UTC, converted from iruntime (Julian day 00:00)
    INTEGER(kind=BYTE8) :: mRuntime

    ! for calibration
    integer(kind=BYTE2), allocatable :: ibrightness(:,:)
    REAL :: calibrationCurve(0:255)
    real, allocatable :: calibratedData(:,:)

     ! for projection
    integer :: sgds(200) ! satellite data GDS
    integer :: tgds(200) ! satellite converted projection, nfiner than model
    real, allocatable, target :: ss(:,:) ! satellite sensor source

    ! functions outside 
    integer :: IW3JDN

    INTEGER :: nx, ny, ii, jj, i, j, n

    ! configuration fields
    CFG_calibrations => cfg%sat%calibrations
    CFG_format = cfg%sat%format
    CFG_nfiner = cfg%sat%nfiner	
    CFG_ss => cfg%sat%ss    
    CFG_arfLUTfile = cfg%sat%shortwaveLUTTable
    CFG_MICRON_110ShortwaveReflThreshold = cfg%sat%micron_110ShortwaveReflThreshold
    CFG_SolarAngleMax = cfg%sat%solarAngleMax

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! read, calibrate and convert projection of satellite data
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

    loop_n: DO n = 1, size(filenames) ! n represents a channel

       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       ! read in, ibrightness is allocated inside the decoding subroutine
       if(CFG_format == "NETCDF") then
       else if(CFG_format == "MCIDAS") then
          call decodeMcIDAS(trim(filenames(n)), sgds, ibrightness, iret)
          IF(iret /= 0) THEN
             WRITE(*,*) "Satellite::runSat() error: in reading file:", TRIM(filenames(n))
             if(allocated(ibrightness)) deallocate(ibrightness)
             RETURN
          END IF
       end if

       ! data dimension before projection conversion
       nx = sgds(2)
       ny = sgds(3)

       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       ! calibrate
       allocate(calibratedData(nx, ny), stat=iret)
       if(n == 4) then
          ! no calibration of satellite sensor source
          calibratedData(:,:) = ibrightness(:,:)
       else
          call setCalibrationCurve(CFG_calibrations(n), calibrationCurve)
          do j = 1, ny
             do i = 1, nx
                calibratedData(i,j) = calibrationCurve(ibrightness(i,j))
             end do
          end do
       end if
       deallocate(ibrightness)


       !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
       ! projection conversion

       call zoominGDS(kgds, CFG_nfiner, tgds)

       nx = tgds(2)
       ny = tgds(3)

       if(n == 1) then		! visible channel
          allocate(satData%vis(nx, ny), stat=iret)
          call convertProjection(sgds, calibratedData, tgds, satData%vis, iret)
       elseif(n == 2) then	! shortwave channel
          allocate(satData%ch2(nx, ny), stat=iret)
          call convertProjection(sgds, calibratedData, tgds, satData%ch2, iret)
       elseif(n == 3) then	! infrared channel
          allocate(satData%ch4(nx, ny), stat=iret)
          call convertProjection(sgds, calibratedData, tgds, satData%ch4, iret)
       elseif(n == 4) then	! satellite sensor sources
          allocate(ss(nx, ny), stat=iret)
          call convertProjection(sgds, calibratedData, tgds, ss, iret)

          ! some printouts
          do jj = 1, 2
             j = ny/3 * jj
             do ii = 1, 5
                i=nx/6*ii
                write(*,"(A,2I6,1X,A,F10.2)") "i,j=",i,j,"ss =", ss(i,j)
             end do
          end do

          ! assign satellite longitude to the corresponding sensor source (ss) number
          do j = 1, ny
          do i = 1, nx
             do ii = 1, size(CFG_ss)
                if(abs(ss(i,j)-CFG_ss(ii).key)  < 0.1) then
                   ss(i,j) = CFG_ss(ii).value
                   exit
                end if
             end do
             ! no matching satellite, try the well known SS numbers
             if (ii > size(CFG_ss)) then
                if(ss(i,j) >= 51. .and. ss(i,j) <= 58.) then
                   ss(i,j) = 0.0
                elseif(ss(i,j) >= 84. .and. ss(i,j) <= 85.) then
                   ss(i,j) = 145.0
                else
                   ss(i,j)=MISSING
                end if
             end if
          end do
          end do
       end if

       if(iret /= 0) then
          write(*,*) "Satellite::runSat() - projection error", iret
          return
       end if

       deallocate(calibratedData)

    END DO loop_n

    where(ss(:,:) < MISSING + 1)
       satData%vis(:,:) = MISSING
       satData%ch2(:,:) = MISSING
       satData%ch4(:,:) = MISSING
    endwhere

    ! If ch2 or ch4 is missing, mark this grid point useless
    where(satData%ch2(:,:) < MISSING + 1 .or. satData%ch4(:,:) < MISSING + 1)
       satData%vis(:,:) = MISSING
       satData%ch2(:,:) = MISSING
       satData%ch4(:,:) = MISSING
    end where

    ! some printouts
    do jj = 1, 2
       j = ny/3 * jj
       do ii = 1, 5
          i=nx/6*ii
          write(*,"(A,2I6,1X,A,4F10.2)") "i,j=",i,j,"vis ch2 ch4 ss=",satData%vis(i,j),satData%ch2(i,j),satData%ch4(i,j), ss(i,j)
       end do
    end do


    ! Now all data on the target satellite projection are ready
    ! (an expanded version of model projection)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Derive more satellite fields
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

    ! convert report time to minutes since julian day 00:00
    mRuntime = IW3JDN(iruntime(1), iruntime(2), iruntime(3))
    ! iruntime(4) - time zone; iruntime(5) - hour
    mRuntime = mRuntime*24*60 + (iruntime(4) + iruntime(5))*60
    ! convert to since 00:00, Jan 1 1970 UTC
    mRuntime = mRuntime - IW3JDN(1970, 1, 1)*24*60

    ! Only angles are related to satellite sensor source, other fields are not.
    CALL runSatDerive(mRuntime, CFG_arfLUTfile, tgds, ss, satData, iret)

    if(iret /= 0) then
       deallocate(satData%vis)
       deallocate(satData%ch2)
       deallocate(satData%ch4)
       deallocate(satData%ch2mch4)
       deallocate(satData%sunz)
       deallocate(satData%ch2_ref)
       deallocate(satData%satice)
    end if

    deallocate(ss)

    RETURN
  END SUBROUTINE runSat

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Decode satellite in McIDAS, meanwile get grid information
  !
  !> @param[in]  filename    - satellite data file to be decoded.
  !> @param[out] kgds        - satellite grid information
  !> @param[out] ibrightness - decoded brightness temperature
  !> @param[out] iret        - status; -1 if failure
  !
  !----------------------------------------------------------------------------

  subroutine decodeMcIDAS(filename, kgds, ibrightness, iret)
    implicit none
    character(len=*), intent(in) :: filename
    integer, intent(out) :: kgds(:) ! satellite grid info
    integer(kind=BYTE2), allocatable, intent(out) :: ibrightness(:,:)
    integer, intent(out) :: iret

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! header type and header
    type :: McIDAS_header_t
       integer :: year
       integer :: month
       integer :: date
       integer :: hour
       integer :: minute
       integer :: nx
       integer :: ny
       integer :: gapx
       integer :: gapy
       integer :: nBytes ! number of bytes per element 1/2/4
       integer :: nBands ! max number of bands per line
       integer :: nPrefix
       integer :: nPrefixDoc
       integer :: nPrefixCal
       integer :: nPrefixMap
       integer :: validity
       integer :: offsetData
       integer :: lengthData ! total data block length in bytes

       character(len=4) :: sourceType
       character(len=4) :: calibrationType
       character(len=4) :: originalSourceType

       integer :: ulImageLine    ! upper-left image line coordinate
       integer :: ulImageElement ! upper-left image element coordinate

       character(len=4) :: navigationType

       integer :: npImageLine    ! image line of the equator
       integer :: npImageElement ! image element of the normal longitude

       real :: stdlat
       real :: dx
    end type McIDAS_header_t
    type(McIDAS_header_t) :: header
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! parameter
    integer, parameter :: IADIR_LEN = 64
    integer, parameter :: IANAV_LEN = 640
    integer, parameter :: IACAL_LEN = 128
    integer, parameter :: ITLEN = IADIR_LEN + IANAV_LEN + IACAL_LEN

    integer(kind=BYTE4) :: iarray(ITLEN)
    integer(kind=BYTE4) :: iadir(IADIR_LEN), ianav(IANAV_LEN), iacal(IACAL_LEN)
    integer(kind=BYTE1)   :: barray(4, ITLEN)
    equivalence(iarray, barray)
    equivalence(iarray, iadir)
    equivalence(iarray(IADIR_LEN+1), ianav)
    equivalence(iarray(IADIR_LEN+IACAL_LEN+1), iacal)

    integer(kind=BYTE1), allocatable :: b1Data(:)
    integer(kind=BYTE2), allocatable :: b2Data(:)
    integer(kind=BYTE4), allocatable :: b4Data(:)
    integer :: lengthAllData
    integer :: dataStartPoint

    integer :: iunit

    integer :: dd, mm, ss ! temporary variables for longitude, latitude DDD:MM:SS
    real :: stdlat, clon, dx, radius, rcos, ecc
    real :: rxp, ryp ! area coordination of natural origin 
    real :: lat, lon
    real :: dxp, dyp, arg

    integer :: i, j

    iret = -1

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Step 1/2: Read and analyze the header
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

    iunit = 13
    open(unit=iunit,file=trim(filename),status='old',access='direct',iostat=iret,recl=ITLEN*BYTE4)
    if(iret /= 0) return
    read(unit=iunit, rec=1, iostat=iret) (iarray(i), i=1, ITLEN)
    close(unit=iunit, iostat=iret)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! iadir(2) should always be 4; otherwise needs byte-swapping
    ! little endian <-> big endian
    if(iadir(2) /= 4) then
       barray(:,  1:24) = barray(BYTE4:1:-1, 1:24)  ! 25-32 ASCII, no swapping
       barray(:, 33:51) = barray(BYTE4:1:-1, 33:51) ! 52-53 ASCII, no swapping
       barray(:, 54:56) = barray(BYTE4:1:-1, 54:56) ! 57    ASCII, no swapping
       barray(:, 58:64) = barray(BYTE4:1:-1, 58:64)

       ! swap NAV block except 1st word
       barray(:, IADIR_LEN+2:IADIR_LEN+IANAV_LEN) = &
            barray(BYTE4:1:-1, IADIR_LEN+2:IADIR_LEN+IANAV_LEN) 

       ! swap CAL block if it exists.
       if(iadir(63) /= 0 ) then 
          barray(:, IADIR_LEN+IANAV_LEN+2:ITLEN) = &
               barray(BYTE4:1:-1, IADIR_LEN+IANAV_LEN+2:ITLEN)
       end if
    end if
       
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! area directory block

    header% year = iadir(4)/1000 + 1900 ! YYDDD
    call m_yyddd2date(header% year, mod(iadir(4), 1000), header% month, header% date, iret)
    header% hour = iadir(5) / 10000
    header% minute = iadir(5)/100 - header% hour * 100

    header% ny = iadir(9)
    header% nx = iadir(10)

    header% gapy = iadir(12)
    header% gapx = iadir(13)

    header% nBytes = iadir(11)
    header% nBands = iadir(14)

    header% validity = iadir(36)

    header% nPrefix = iadir(15)
    header% nPrefixDoc = iadir(49)
    header% nPrefixCal = iadir(50)
    header% nPrefixMap = iadir(51)

    header% offsetData = iadir(34)
    header% lengthData = (header%nx * header%nBytes * header%nBands + header%nPrefix) * header%ny

    call m_int2string(iadir(52), header%sourceType)
    call m_int2string(iadir(53), header%calibrationType)
    call m_int2string(iadir(57), header%originalSourceType)

    header% ulImageLine = iadir(6)
    header% ulImageElement = iadir(7)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! navigation type
    call m_int2string(ianav(1), header%navigationType)

    header% npImageLine = ianav(2)
    header% npImageElement = ianav(3)

    lengthAllData = header% offsetData + header% lengthData
    if(mod(lengthAllData, header%nBytes) /= 0) then
       write(*,*) "McIDAS headers are in words, must times of 4"
       return
    end if

    if(header%nBytes == BYTE1) then
       allocate(b1Data(lengthAllData/header%nBytes), stat=iret)
    else if(header%nBytes == BYTE2) then
       allocate(b2Data(lengthAllData/header%nBytes), stat=iret)
    else if(header%nBytes == BYTE4) then
       allocate(b4Data(lengthAllData/header%nBytes), stat=iret)
    else
       return
    end if

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Step 2/2: Read data and set GDS
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! read data into a 1-dimension array
    open(unit=iunit, file=filename, status='old', access='direct', iostat=iret,recl=lengthAllData)
    if(header%nBytes == BYTE1) then
       read(unit=iunit, rec=1, iostat=iret) b1Data
    else if(header%nBytes == BYTE2) then
       read(unit=iunit, rec=1, iostat=iret) b2Data
    else if(header%nBytes == BYTE4) then
       read(unit=iunit, rec=1, iostat=iret) b4Data
    end if
    close(unit=iunit, iostat=iret)

    allocate(ibrightness(header%nx, header%ny), stat=iret)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! convert the 1-dimension array to 2-dimension array
    dataStartPoint = header% offsetData / header%nBytes
    do j = 1, header%ny
       do i = 1, header%nx
          if(header%nBytes == BYTE1) then
             ibrightness(i,j) = b1Data(dataStartPoint + header%nx * (j-1) + i)
             IF(ibrightness(i,j) < 0) THEN
                ! FORTRAN does not have unsigned one-byte integer (-128, 127)
                ! From 1-byte integer to 2-byte integer, the highest bit
                ! of a byte '1' is interpreted:
                !   1) -- a negative integer, for a one-byte integer
                !   2) -- 128 for a two or more byte integer
                ibrightness(i,j)  = 256 + ibrightness(i,j)
             END IF
          else if(header%nBytes == BYTE2) then
             ibrightness(i,j) = b2Data(dataStartPoint + header%nx * (j-1) + i)
          else if(header%nBytes == BYTE4) then
             ibrightness(i,j) = b4Data(dataStartPoint + header%nx * (j-1) + i)
          end if
       end do
    end do

    if(allocated(b1Data)) deallocate(b1Data)
    if(allocated(b2Data)) deallocate(b2Data)
    if(allocated(b4Data)) deallocate(b4Data)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! NAV block

    ! standard latitude    DDD::MM:SSS
    dd = ianav(4) / 10000
    mm = ianav(4) / 100 - dd * 100
    ss = ianav(4) - dd * 10000 - mm * 100
    stdlat = dd + mm/60. + ss/3600.

    ! BYTE4 longitude    DDD::MM:SSS
    ! If west postitive, make west negative.
    dd = ianav(6) / 10000
    mm = ianav(6) / 100 - dd * 100
    ss = ianav(6) - dd * 10000 - mm * 100
    clon = dd + mm/60. + ss/3600.
    if( ianav(10) >= 0 ) clon = -clon

    ! set pixel/grid spacing and earth radius and eccentricity
    dx = ianav(5) * header%gapx ! in metar
    radius = ianav(7)           ! in metar
    ecc = ianav(8)/1000000.
    ! Since dx may vary, calculate dx
    dx = PI * 2 * radius / header% nx

    ! area coordination of natural origin 
    rxp = real(ianav(3)-iadir(7)) / iadir(13) + 1.
    ryp = header%ny - real(ianav(2) - iadir(6)) / iadir(12)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! set GDS information
    kgds(:) = -1
    select case(trim(header%navigationType))
    case('MERC')

       rcos = radius * cos(stdlat * D2R)

       kgds(1) = 1 ! Mercator Projection Grid
       kgds(2) = header% nx
       kgds(3) = header% ny

       ! Compute lat/lon of the top-left corner.
       dxp   = 1. - rxp
       dyp   = header% ny - ryp
       arg   = EXP ( dx * dyp / rcos )
       lat = ( 2. * ATAN ( arg ) - PI/2.) * R2D
       lon = clon + ( dx * dxp / rcos ) * R2D
       call m_prnlon(lon)
       kgds(4) = lat * 1000
       kgds(5) = lon * 1000
       kgds(6) = 128
       ! Compute lat/lon of the bottom-right corner point.
       dxp = header% nx - rxp
       dyp = 1 - ryp
       arg = EXP ( dx * dyp / rcos )
       lat = ( 2. * ATAN ( arg ) - PI/2. ) * R2D
       lon = clon + ( dx * dxp / rcos ) * R2D
       call m_prnlon(lon)
       kgds(7) = lat * 1000
       kgds(8) = lon * 1000
       ! for global mosaic, longitude of the start may be close to that of the end
       if(kgds(8) - kgds(5) <= 5 * 1000.0) then
           kgds(5) = kgds(5) - 360 * 1000.0
       endif

       kgds(9) = stdlat
       ! scanning mode:
       ! Satellite data is stored from North/top to South/bottom
       !                          from West/left to East/right
       kgds(11) = 0 
       kgds(12) = dx  ! unit: meter
       kgds(13) = dx  ! unit: meter
       kgds(20) = 255
    case default
       write(*,*) "Satellite Projection ", trim(header%navigationType), " isn't supported yet."
       iret = -1
       return
    end select

    iret = 0
    return
  end subroutine decodeMcIDAS
 

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Interpret a word in integer to a string
  !
  !> @param[in]  number - a word of 4 bytes
  !> @param[out] cOut   - a len=4 string matching to each byte of number
  !----------------------------------------------------------------------------

  subroutine m_int2string(number, cOut)
    IMPLICIT NONE
    integer(kind=BYTE4), intent(in) :: number
    character(len=*), intent(out) :: cOut

    integer(kind=BYTE1) :: cNumber(BYTE4)
    integer :: aNumber, i

    equivalence(aNumber, cNumber) ! equivalence can not apply to a dummy argument

    aNumber = number

    do i = 1, BYTE4
       cOut(i:i) = char(cNumber(i))
    end do
  end subroutine m_int2string

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> convert the day of a year to month and date
  !
  !> @param[in]  year   - which year
  !> @param[in]  day    - day of the year
  !> @param[out] month  - the month of the year
  !> @param[out] date   - the date of the month
  !> @param[out] iret   - status; -1 if failure
  !
  !----------------------------------------------------------------------------
  subroutine m_yyddd2date(year, day, month, date, iret)
    IMPLICIT NONE
    integer, intent(in) :: year
    integer, intent(in) :: day
    integer, intent(inout) :: month
    integer, intent(inout) :: date
    integer, intent(out) :: iret

    integer :: daysInMonths(12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
    integer :: days(12)

    integer :: i

    if((year < 1900) .or. ((day < 0) .or. (day > 366))) then
       iret = -1
       return
    end if

    do i = 1, 12
       days(i) = sum(daysInMonths(1:i))
    end do

    ! if a leap year
    if((mod(year, 4) == 0 .and. mod(year, 100) /= 0) .or. &
       (mod(year, 400) == 0)) then
       days(2:) = days(2:) + 1
    end if

    do i = 1, 12
       if(day - days(i) <= 0) exit
    end do

    month = i
    date  = daysInMonths(i) - (days(i)-day)

    iret = 0
    return
  end subroutine m_yyddd2date

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Make sure longitude to fall in [-180, 180]
  !
  !> @param[inout] dlon - longitude
  !
  !----------------------------------------------------------------------------
  subroutine m_prnlon(dlon)
    IMPLICIT NONE
    real, intent(inout) :: dlon

    real :: dln
    dln   = dlon - IFIX ( dlon / 360. ) * 360.
    IF ( dln  .lt. -180. ) dln = dln + 360.
    IF ( dln  .gt.  180. ) dln = dln - 360.
    dlon = dln
    return
  end subroutine m_prnlon


  !------------------------------------------------------------------------------
  ! Description:
  !> Creates a calibration curve for a channel, for integers from 0-255.
  !
  !> @param[in]  channel - which channel, 1-VIS, 2-shortwave, 3-infrared
  !> @param[out] curve   - calibration curve for a channel
  !
  !------------------------------------------------------------------------------

  SUBROUTINE setCalibrationCurve(CFG_calibration, curve)
    IMPLICIT NONE
    type(sat_calibration_t), target :: CFG_calibration
    REAL, intent(out) :: curve(0:255)



    INTEGER :: indx, xValueIndex, i
    REAL :: offset, linear, quadratic

    ! configration related
    real ::  CFG_CAL_min, CFG_CAL_max
    integer, pointer :: CFG_CAL_bad_values(:)
    ! curve: y = a + bx + cx^2
    integer, pointer :: CFG_CAL_x(:)
    real, pointer :: CFG_CAL_a(:)
    real, pointer :: CFG_CAL_b(:)
    real, pointer :: CFG_CAL_c(:)

    ! configuration fields
    CFG_CAL_min = CFG_calibration% min
    CFG_CAL_max = CFG_calibration% max
    CFG_CAL_bad_values => CFG_calibration% bad_values
    ! y = a + bx + cx^2
    CFG_CAL_x => CFG_calibration% x_values
    CFG_CAL_a => CFG_calibration% offset_coeffs
    CFG_CAL_b => CFG_calibration% linear_coeffs
    CFG_CAL_c => CFG_calibration% quadratic_coeffs

    DO indx = 0, 255

       ! Determine appropriate piecewise linear coefficients.
       xValueIndex = 1   
       DO i = 1, size(CFG_CAL_x)
          ! Compare index to each element of x_Values. Increment xValueIndex 
          ! for each value of x_Values that is less than index.
          IF( indx >  CFG_CAL_x(i)) THEN
             xValueIndex = xValueIndex + 1
          ELSE
             EXIT
          END IF
       END DO

       offset    = CFG_CAL_a(xValueIndex)
       linear    = CFG_CAL_b(xValueIndex)
       quadratic = CFG_CAL_c(xValueIndex)
       curve(indx) = offset + linear*indx + quadratic*indx*indx

       ! No bounds checks if CFG_CAL_min >= CFG_CAL_max.
       if(CFG_CAL_min < CFG_CAL_max) THEN
          IF(curve(indx) < CFG_CAL_min) THEN
             curve(indx) = MISSING
          END IF
          IF(curve(indx) > CFG_CAL_max) THEN
             curve(indx) = MISSING
          END IF
       END IF

    END DO

    ! Override bad input data values with output flag (MISSING).
    DO i = 1, size(CFG_CAL_bad_values)
       indx = CFG_CAL_bad_values(i)
       curve(indx) = MISSING
    END DO

    RETURN
  END SUBROUTINE setCalibrationCurve

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Derive satellite fields: ch2-ch4, solar zenith, sw refl, sat ice
  !
  !> @param[in]  minutes    - run time in minutes since 00:00, Jan 1 1970 UTC
  !> @param[in]  arfLUTfile - ARF lookup table file for shortwave reflectance
  !> @param[in]  kgds       - satellite data grid information
  !> @param[in]  satLon     - longitude of satellite sensor source
  !> @param[inout] satData  - satellite data set
  !> @param[out]   iret     - status; -1 if failure
  !
  !----------------------------------------------------------------------------
  SUBROUTINE runSatDerive(minutes, arfLUTfile, kgds, satLon, satData, iret)
    IMPLICIT NONE
    integer(kind=BYTE8), intent(in) :: minutes
    CHARACTER(len=*), INTENT(in) :: arfLUTfile
    integer, intent(in) :: kgds(:)
    real, intent(in) :: satLon(:,:)
    type(satellite_data_t), target, INTENT(inout) :: satData
    INTEGER, INTENT(out) :: iret

    ! satellite zenith, solar zenith and relative azimuth angle
    REAL, DIMENSION(:,:), ALLOCATABLE :: satZenith, satAzimuth, solarAzimuth, relAzimuth

    integer :: nx, ny

    nx = kgds(2)
    ny = kgds(3)

    ! allocate memory for the final satellite output
    allocate(satData%ch2mch4(nx, ny))
    allocate(satData%sunz(nx, ny))
    allocate(satData%ch2_ref(nx, ny))
    allocate(satData%satice(nx, ny))

    ! allocate memory for internal arrays
    ALLOCATE(satZenith(nx, ny))    !"Satellite Zenith Angle", "SAT_ZENITH"
    ALLOCATE(satAzimuth(nx, ny))   !"Satellite Azimuth Angle", "SAT_AZIMUTH"
    ALLOCATE(solarAzimuth(nx, ny)) !"Solar Azimuth Angle", "SUN_AZIMUTH"
    ALLOCATE(relAzimuth(nx, ny))   !"Relative Azimuth Angle", "REL_AZIMUTH"

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Calculating angles.  %sunz
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! out of these 5 angles, only solarZenith will be saved toward CIP algo
    WRITE(*, *) "Calculating angles..."
    CALL getAngles(minutes, kgds, satLon, &
         satZenith, satAzimuth, satData%sunz, solarAzimuth, relAzimuth, iret)
    IF(iret /= 0) THEN
       WRITE(*,*) "Satellite::runSatDerive Error -- when calculating angles..."
       RETURN
    END IF

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Calculating shortwave reflectance  %ch2_ref
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    WRITE(*,*) "Calculating shortwave reflectance."
    call getShortwaveReflectance(nx, ny, arfLUTfile, satData%ch2, satData%ch4, &
                 satZenith, satData%sunz, relAzimuth, satData%ch2_ref, iret)
    IF(iret /= 0) THEN
       WRITE(*,*) "Satellite::runSatDerive Error -- when calculating shortwave reflectance."
       RETURN
    END IF

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Calculating normalized albedo.  %vis
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! from this point, visible is converted to albedo !
    WRITE(*, *) "Calculating normalized albedo.", LF
    CALL calcNormalizedAlbedo(satData%sunz, satData%vis)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Calculating satellite icing index.  %ch2mch4
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    WRITE(*, *) "Calculating ch2-ch4."
    CALL calcDifferenceFields(satData%ch2, satData%ch4, satData%ch2mch4)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Calculating satellite icing index.  %satice
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    WRITE(*, *) "Calculating satellite icing."
    CALL calcSatelliteIcing(satData%vis, satData%ch2, satData%ch4, &
                            satZenith, satData%ch2_ref, satData%satice)

    ! do clean up
    DEALLOCATE(satZenith, stat=iret)
    DEALLOCATE(satAzimuth, stat=iret)
    DEALLOCATE(solarAzimuth, stat=iret)
    DEALLOCATE(relAzimuth, stat=iret)

    iret = 0

    RETURN
  END SUBROUTINE runSatDerive

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> calculates the satellite based icing index.
  !> It executes G. Thompson's satellite-based icing algorithm.
  !
  !> @param[in]  visible   - visiable channel
  !> @param[in]  micron039 - shortwave infrared channel
  !> @param[in]  micron110 - infrared channel
  !> @param[in]  sat_zen   - satellite zenith
  !> @param[in]  sw_refl   - shortwave reflectance
  !> @param[inout] sat_ice - satellite icing
  !
  !----------------------------------------------------------------------------
  SUBROUTINE calcSatelliteIcing(visible, micron039, micron110, sat_zen, sw_refl, sat_ice)
    IMPLICIT NONE
    REAL,  DIMENSION(:,:), INTENT(in) :: visible, micron039, micron110, sat_zen, sw_refl
    REAL, DIMENSION(:,:), ALLOCATABLE,  INTENT(inout) :: sat_ice

    sat_ice = 0
    WHERE((ABS(sat_zen) < SatZenithThreshold) .AND. &
          (visible > CH1_Threshold) .AND. &
          (sw_refl > CH2_RelfectanceThreshold) .AND. &
          (micron110 > CH4_MinThreshold) .AND. &
          (micron110 < CH4_MaxThreshold) .AND. &
          ((micron039-micron110) > CH2MinusCH4Threshold))  sat_ice = 1

    RETURN
  END SUBROUTINE calcSatelliteIcing


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculates difference between shortwave infrared and infrared channels.
  !
  !> @param[in]  micron039 - shortwave infrared channel
  !> @param[in]  micron110 - infrared channel
  !> @param[out] diff_fld  - difference between sw infrared & infrared channels
  !
  !----------------------------------------------------------------------------
  SUBROUTINE calcDifferenceFields(micron039, micron110, diff_fld)
    IMPLICIT NONE
    REAL, INTENT(in) :: micron039(:,:), micron110(:,:)
    REAL, INTENT(inout) :: diff_fld(:,:)

    diff_fld(:,:) = MISSING

    WHERE( micron039 > MISSING+1. .AND. micron110 > MISSING+1 )
       diff_fld = micron039 - micron110
    endwhere

    RETURN
  END SUBROUTINE calcDifferenceFields


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> convert visible channel to albedo
  !
  !> @param[in]    sol_zen - solar zenith
  !> @param[inout] visible - visible channel
  !
  !----------------------------------------------------------------------------
  elemental SUBROUTINE calcNormalizedAlbedo(sol_zen, visible)
    IMPLICIT NONE
    REAL, INTENT(in) :: sol_zen
    REAL, INTENT(inout) :: visible

    IF( visible <  MISSING+1) THEN
       visible = MISSING
    ELSE IF (ABS(sol_zen) < CFG_SolarAngleMax) THEN
       visible = visible / COS(sol_zen * D2R)
       ! Can get rid of some calibration error
       if (visible > 100.) visible = MISSING
    else
       visible = MISSING
    END IF

    RETURN
  END SUBROUTINE calcNormalizedAlbedo


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculates zenith and azimuth of solar and satellites
  !
  !> @param[in] minutes - run time in minutes since 00:00, Jan 1 1970 UTC
  !> @param[in] kgds    - satellite data grid information
  !> @param[in] ss      - satellite sensor source
  !> @param[inout] sat_zen - satellite zenith
  !> @param[inout] sat_az  - satellite azimuth
  !> @param[inout] sol_zen - solar zenith
  !> @param[inout] sol_az  - solar azimuth
  !> @param[inout] rel_az  - relative azimuth between solar and satellite
  !> @param[out]    iret   - status; -1 if failure
  !
  !----------------------------------------------------------------------------
  SUBROUTINE getAngles(minutes, kgds, satLon, &
                       sat_zen, sat_az, sol_zen, sol_az, rel_az, iret)
    IMPLICIT NONE
    INTEGER(kind=BYTE8), INTENT(in) :: minutes
    integer, intent(in) :: kgds(:)
    real, intent(in) :: satLon(:,:)
    REAL, DIMENSION(:,:), INTENT(inout) :: sat_zen, sat_az, sol_az, rel_az
    REAL, DIMENSION(:,:), INTENT(inout) :: sol_zen
    INTEGER, INTENT(out) :: iret

    integer :: nx, ny, ii, jj, i, j

    TYPE(xyz_t), ALLOCATABLE :: gridGeoPos(:,:), gridTanPlaneE(:,:), gridTanPlaneN(:,:)

    REAL(kind=BYTE8) :: sunLat, sunLon, sunDist
    REAL(kind=BYTE8) :: satLat, satDist

    iret = -1

    sol_zen = MISSING

    nx = kgds(2)
    ny = kgds(3)

    ALLOCATE(gridGeoPos(nx, ny), stat=iret)
    ALLOCATE(gridTanPlaneE(nx, ny), stat=iret)
    ALLOCATE(gridTanPlaneN(nx, ny), stat=iret)

    ! calculate the values grid by grid

    call m_calcGridValues(kgds, gridGeoPos, gridTanPlaneE, gridTanPlaneN)

    ! calculate solar angles

    CALL m_getSolarPosition(minutes*60, sunLon, sunLat, sunDist)

    CALL m_getObjAngles(sunLat, sunLon, sunDist, &
                       gridGeoPos(:,:), gridTanPlaneE(:,:), gridTanPlaneN(:,:), &
                       sol_zen(:,:), sol_az(:,:))

    ! calculate satellite angels

    satDist = GeostatAltitude + EquatorEarthRadius
    satLat = 0.

    CALL m_getObjAngles(satLat, real(satLon,BYTE8), satDist, &
              gridGeoPos(:,:), gridTanPlaneE(:,:), gridTanPlaneN(:,:), &
              sat_zen(:,:), sat_az(:,:))

    where( satLon(:,:) > MISSING + 1.)
       ! Calculate relative azimuth
       rel_az(:,:) = abs(sat_az(:,:) - sol_az(:,:))
       where(rel_az(:,:) > 180.) rel_az(:,:) = 360 - rel_az(:,:)
    elsewhere
       rel_az(:,:) = MISSING
    end where


    ! Azimuth should be added 270 degree. will not add 270 since only relative azimuth is used.

    ! some printouts
    do jj = 1, 2
       j = ny/3 * jj
       do ii = 1, 5
          i=nx/6*ii
          write(*,"(A,2I6,1X,A,F10.2,1X,A,F10.2)") "i,j=",i,j,"sunLat=",sunLat, "relative azimuth=", rel_az(i,j)
          write(*,"(A,3F10.2)") "sun      lon,zenith,azimuth=",sunLon,sol_zen(i,j),sol_az(i,j)
          write(*,"(A,3F10.2)") "satelite lon,zenith,azimuth=",satLon(i,j),sat_zen(i,j),sat_az(i,j)
       end do
    end do

    DEALLOCATE(gridGeoPos)
    DEALLOCATE(gridTanPlaneE)
    DEALLOCATE(gridTanPlaneN)

    RETURN
  END SUBROUTINE getAngles


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Determines the sun's subsatellite point on the earth given the time.
  !
  !> @param[in] data_time - run time in seconds
  !> @param[out] sunLon   - sun longitude
  !> @param[out] sunLat   - sun latitude
  !> @param[out] sunDist  - sun distance
  !
  ! Note:
  !  Since the reference system is longitude is positive westward, the
  !  calculated solar longitude is in [0,180] West, and in [180, 360] East
  !  Then converted to positive eastward.
  !
  !----------------------------------------------------------------------------
  SUBROUTINE m_getSolarPosition(data_time, sunLon, sunLat, sunDist)
    implicit none
    INTEGER(kind=BYTE8), INTENT(in) :: data_time
    REAL(kind=BYTE8), INTENT(out) :: sunLat
    REAL(kind=BYTE8), INTENT(out) :: sunLon
    REAL(kind=BYTE8), INTENT(out) :: sunDist

    REAL(kind=BYTE8) ::  rightAscension, declination

    CALL m_calcSolarRadec(data_time, rightAscension, declination)
    write(*, "(A, I12, 2F10.3)")"sun time,right ascension, declination=", data_time, rightAscension, declination

    sunLon = (m_gmt2gst(data_time) - rightAscension) + 360.0
    sunLon = sunLon - 360.0*FLOOR(sunLon/360.0)
    sunLat = declination
    sunDist = Earth2SunDist + EquatorEarthRadius
    sunLon = 360. - sunLon
    print *, "sunlon=", sunLon

    RETURN
  END SUBROUTINE m_getSolarPosition


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculates the solar right ascension and declination
  !
  !> @param[in]  gmt - greenwich mean time
  !> @param[out] ra  - solar right ascension
  !> @param[out] dec - solar declination
  !
  ! Note:
  !  This subroutine originally was written by R. Bullock.
  !----------------------------------------------------------------------------
  SUBROUTINE m_calcSolarRadec(gmt, ra, dec)
    implicit none
    INTEGER(kind=BYTE8), INTENT(in) :: gmt
    REAL(kind=BYTE8), INTENT(inout) :: ra, dec

    REAL(kind=BYTE8) :: t, lzero, m, mrad, eps, epsrad, c, theta
    REAL(kind=BYTE8) :: omega, omegarad, lambda, lambdarad, x, y, z, d

    REAL(kind=BYTE8), PARAMETER :: tol = 1.0e-6

    t = gmt
    t = ((t/86400.0) - 10957.5)/36525.0

    lzero = 280.46645 + 36000.76983*t + 0.0003032*t*t
    lzero = lzero - 360.0*FLOOR(lzero/360.0)

    m = 357.52910 + 35999.05030*t - 0.0001559*t*t - 0.00000048*t*t*t
    m = m - 360.0*FLOOR(m/360.0)
    mrad = m*D2R

    eps = 23.439291111111 - (46.815*t + 0.00059*t*t - 0.001813*t*t*t)/3600.0

    ! ecc = 0.016708617 - 0.000042037*t - 0.0000001236*t*t

    c = (1.9146 - 0.004817*t - 0.000014*t*t)*SIN(mrad) + &
        (0.019933 - 0.000101*t)*SIN(2.0*mrad)+ 0.00029*SIN(3.0*mrad)

    theta = lzero + c

    omega = 125.04 - 1934.136*t
    omega = omega - 360.0*FLOOR(omega/360.0)
    omegarad = omega*D2R

    lambda = theta - 0.00569 - 0.00478*SIN(omegarad)
    lambdarad = lambda*D2R

    eps = eps + 0.00256*COS(omegarad)
    epsrad = eps*D2R

    x = COS(lambda*D2R)
    y = COS(epsrad)*SIN(lambdarad)
    z = SIN(epsrad)*SIN(lambdarad)

    d = ABS(x) + ABS(y)

    dec = R2D*ASIN(z)
    IF( d < tol ) THEN
       ra = 0.0
    ELSE
       ra = R2D*ATAN2(y, x)
    END IF
    ra = ra - 360.0*FLOOR(ra/360.0)

    RETURN
  END SUBROUTINE m_calcSolarRadec


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Convert Greenwich Mean Time (GMT) to Greenwich Sidereal Time (GST)
  !
  !> @param[in]  gmt - greenwich mean time
  !> @return:    greenwich sidereal time
  !
  !----------------------------------------------------------------------------
  REAL(kind=BYTE8) FUNCTION m_gmt2gst(gmt)
    IMPLICIT NONE
    INTEGER(kind=BYTE8), INTENT(in) :: gmt

    INTEGER :: date, time
    REAL(kind=BYTE8) :: eta, t, ans

    date = gmt/86400
    time = MOD(gmt, 86400_BYTE8)

    eta = 1.00273790935

    t = (date - 10957.5)/36525.0

    ans = 100.46061837 + 36000.770053608*t + 0.000387933*t*t - t*t*t/38710000.0

    ans = ans/360.0

    ans = ans - FLOOR(ans)

    ans = ans + eta*(time/86400.0)

    ans = ans - FLOOR(ans)

    ans = ans * 360.0

    m_gmt2gst = ans

    RETURN
  END FUNCTION m_gmt2gst

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculate grid values of vectors.
  !
  !> @param[in]    kgds - satellite data grid information
  !> @param[inout] p    - a (x,y,z) position in a geocentric coordinate system
  !> @param[inout] e    - e-w basis vector for a tangent plane
  !> @param[inout] n    - n-s basis vector for a tangent plane
  !
  !----------------------------------------------------------------------------

  SUBROUTINE m_calcGridValues(kgds, p, e, n)
    implicit none
    integer, intent(in) :: kgds(:)
    TYPE(xyz_t), intent(inout), dimension(:,:) ::  p, e, n
 
    ! variables for GDSWIZ
    real, allocatable :: lat(:,:), lon(:,:), x(:,:), y(:,:)
    integer :: iopt, npts, lrot, nret
    real :: fill, crot, srot

    integer :: nx, ny, ii, jj, i, j

    real(kind=BYTE8), allocatable :: rlat(:,:), rlon(:,:)

    nx = kgds(2)
    ny = kgds(3)
    allocate(lat(nx, ny))
    allocate(lon(nx, ny))
    allocate(x(nx, ny))
    allocate(y(nx, ny))

    fill = MISSING

    iopt = 0       ! COMPUTE EARTH COORDS OF ALL THE GRID POINTS
    npts = nx * ny
    lrot = 0       ! only '1' is a valid value to turn on crot and srot.
    call GDSWIZ(kgds,iopt,npts,fill,x,y,lon,lat,nret,lrot,crot,srot)
    deallocate(x)
    deallocate(y)

    allocate(rlat(nx, ny))
    allocate(rlon(nx, ny))
    rlat=lat
    rlon=lon

    p(:,:) = m_geocentricP(1.0_BYTE8, rlat(:,:), rlon(:,:))
    e(:,:) = m_tangentPlaneE(rlon(:,:))
    n(:,:) = m_tangentPlaneN(rlat(:,:), rlon(:,:))

    ! some printouts
    do jj = 1, 2
       j = ny/3 * jj
       do ii = 1, 5
          i=nx/6*ii
          write(*,"(A,2I6,1X,A,2F10.2)") "i,j=",i,j,"satellite grid (lat, lon)=",lat(i,j), lon(i,j)
       end do
    end do

    deallocate(lat)
    deallocate(lon)

    RETURN
  END SUBROUTINE m_calcGridValues

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculates the (x,y,z) components of a vector in a geocentric coordinate
  !> system.
  !
  !> @param[in] rng  - ratio of range to earth radius
  !> @param[in] lat  - latitude
  !> @param[in] lon  - longitude
  !> @return:  the (x,y,z) components of a vector in a geocentric coordinate
  !
  ! Note:
  !  Assumes a unit magnitude for the vector. 
  !  The units of the angles are degrees
  !
  !----------------------------------------------------------------------------
  elemental function m_geocentricP(rng, lat, lon)
    IMPLICIT NONE
    REAL(kind=BYTE8), INTENT(in) :: rng, lat, lon
    TYPE(xyz_t) :: m_geocentricP

    TYPE(xyz_t) :: p
    REAL(kind=BYTE8) :: radLat, radLon
    radLat = lat * D2R
    radLon = lon * D2R

    ! rng used to be a distance. Modified to be ratio of distances - Y Mao

    ! p%x = (rng/m_earthRadius(lat))*COS(radLat)*SIN(radLon)
    p%x = rng*COS(radLat)*SIN(radLon)

    ! p%y = (rng/m_earthRadius(lat))*COS(radLat)*COS(radLon)
    p%y = rng*COS(radLat)*COS(radLon)

    ! p%z = (rng/m_earthRadius(lat))*SIN(radLat)
    p%z = rng*SIN(radLat)

    m_geocentricP = p
    RETURN
  END function m_geocentricP


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculates the normalized vector that represents the baering between
  !> two point in a geocentric coordinate system.
  !
  !> @param[in]   p_i  - one point
  !> @param[in]   p_s  - the other point
  !> @return: the normalized vector that represents the baering
  !
  ! Note:
  !  Assumes a unit magnitude for the vector. 
  !
  !----------------------------------------------------------------------------

  elemental function  m_geocentricQ(p_i, p_s)
    implicit none
    TYPE(xyz_t), INTENT(in) :: p_i, p_s
    TYPE(xyz_t) :: m_geocentricQ

    TYPE(xyz_t) :: q

    REAL(kind=BYTE8) :: x, y, z, mag

    x = p_s%x - p_i%x
    y = p_s%y - p_i%y
    z = p_s%z - p_i%z

    mag = SQRT(x*x + y*y + z*z)

    q%x = x/mag
    q%y = y/mag
    q%z = z/mag

    m_geocentricQ = q

    RETURN
  END function m_geocentricQ
  

  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculate  the e-w basis vector for a tangent plane at a point in 
  !> a geocentric coordinate system.
  !
  !> @param[in] lon - longitude
  !> @return:  e-w basis vector for a tangent plane 
  !
  ! Note:
  !  The units of the angles are degrees.
  !
  !----------------------------------------------------------------------------
  elemental function m_tangentPlaneE(lon)
    IMPLICIT NONE
    REAL(kind=BYTE8), INTENT(in) :: lon
    TYPE(xyz_t) :: m_tangentPlaneE

    TYPE(xyz_t) :: e

    REAL(kind=BYTE8) :: radLon

    radLon = lon*D2R

    e%x = -COS(radLon)
    e%y = SIN(radLon)
    e%z = 0.0

    m_tangentPlaneE = e
    RETURN
  END function m_tangentPlaneE


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculate  the n-s basis vector for a tangent plane at a point in 
  !> a geocentric coordinate system.
  !
  !> @param[in] lat - latitude
  !> @param[in] lon - longitude
  !> @return:  n-s basis vector for a tangent plane 
  !
  ! Note:
  !  The units of the angles are degrees.
  !
  !----------------------------------------------------------------------------
  elemental function m_tangentPlaneN(lat, lon)
    IMPLICIT NONE
    REAL(kind=BYTE8), INTENT(in) :: lat, lon
    TYPE(xyz_t) :: m_tangentPlaneN

    TYPE(xyz_t) :: n
    REAL(kind=BYTE8) :: radLon, radLat

    radLon = lon*D2R
    radLat = lat*D2R

    n%x = -SIN(radLat)*SIN(radLon)
    n%y = -SIN(radLat)*COS(radLon)
    n%z = COS(radLat)

    m_tangentPlaneN = n
    RETURN
  END function m_tangentPlaneN


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculates the earth's radius as a function of geographic latitude.
  !> This function determines the International Ellipsoid. 
  !
  !> @param[in]  lat - latitude
  !
  ! Note:
  !  Taken from "Orbital Motion" 3rd ed. by A. E. Roy, pg 305
  !
  !----------------------------------------------------------------------------
  elemental  REAL(kind=BYTE8) FUNCTION m_earthRadius(lat)
    IMPLICIT NONE
    REAL(kind=BYTE8), INTENT(in) :: lat

    REAL(kind=BYTE8) :: sinLat, sin2Lat, radius
    sinLat = SIN(D2R*lat)
    sin2Lat = SIN(2*D2R*lat)
    radius = 6378.388 - 0.0033670*sinLat*sinLat + 7.1e-6*sin2Lat*sin2Lat

    m_earthRadius = radius

    RETURN
  END FUNCTION m_earthRadius


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculate the zenith and azimuth angles for the object at a given location
  !
  !> @param[in] obj_lat  - latitude
  !> @param[in] obj_lon  - longitude
  !> @param[in] obj_dist - distance
  !> @param[in] p        - position
  !> @param[in] e        - e-w basis vector 
  !> @param[in] n        - n-s basis vector
  !> @param[inout] obj_zen - zenith
  !> @param[inout] obj_azm - azimuth
  !
  !----------------------------------------------------------------------------
  elemental SUBROUTINE m_getObjAngles(obj_lat, obj_lon, obj_dist, p, e, n, &
                                      obj_zen, obj_azm)
    IMPLICIT NONE
    REAL(kind=BYTE8), INTENT(in) :: obj_lat, obj_lon, obj_dist
    TYPE(xyz_t), INTENT(in) :: p, e, n
    REAL, INTENT(inout) :: obj_zen , obj_azm

    ! determine the geocentric position vector of the satellite and sun
    TYPE(xyz_t) :: objGeoPos, objGeoQVec

    ! Simplify calculation of satellite angles to work on 'elemental'
    if(obj_lon < MISSING + 1.) then
       obj_zen = MISSING
       obj_azm = MISSING
       return
    end if

    objGeoPos = m_geocentricP(obj_dist/m_earthRadius(obj_lat), obj_lat, obj_lon)

    objGeoQVec =  m_geocentricQ(p, objGeoPos)

    obj_zen = m_calcZenith(objGeoQVec, p)
    obj_azm = m_calcAzimuth(objGeoQVec, e, n)

    RETURN
  END SUBROUTINE m_getObjAngles


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculates the zenith angle.
  !
  !> @param[in] q - normalized vector
  !> @param[in] p - position vector
  !> @return:  the zenith angle
  !
  !----------------------------------------------------------------------------
  elemental REAL(kind=BYTE8) FUNCTION m_calcZenith(q, p)
    IMPLICIT NONE
    TYPE(xyz_t), INTENT(in) :: q, p

    REAL(kind=BYTE8) :: dotProduct

    dotProduct = p%x*q%x + p%y*q%y + p%z*q%z
    m_calcZenith  = 90 - R2D*ASIN(dotProduct)

    RETURN 
  END FUNCTION m_calcZenith


  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculates the azimuth angle
  !
  !> @param[in] q - normalized vector
  !> @param[in] e - e-w basis vector 
  !> @param[in] n - n-s basis vector
  !> @return: the azimuth angle
  !
  !----------------------------------------------------------------------------
  elemental REAL(kind=BYTE8) FUNCTION m_calcAzimuth(q, e, n)
    IMPLICIT NONE
    TYPE(xyz_t), INTENT(in) :: q, e, n

    REAL(kind=BYTE8) :: num, den

    num = e%x*q%x + e%y*q%y + e%z*q%z
    den = n%x*q%x + n%y*q%y + n%z*q%z

    m_calcAzimuth = R2D*ATAN2(den,num)

    RETURN
  END FUNCTION m_calcAzimuth


!**********************************************************************
! * subroutine: getShortwaveReflectance()
! *
! * Description:This method creates the shortwave reflectance data.
! *
! * Returns:
! *
! * Notes:
! *
! * Notes: the units for the input are:
! *          ch4_thresh -- degrees Celsius
! *          ch2_data   -- degrees Celsius
! *          ch4_data   -- degrees Celsius
! *          sat_zen    -- degrees
! *          sun_zen    -- degrees
! *          rel_azm    -- degrees
! *
  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Calculate shortwave reflectance
  !
  !> @param[in] nx         - data dimension
  !> @param[in] ny         - data dimension
  !> @param[in] arfLUTfile - ARF lookup table for shortwave reflectance
  !> @param[in] micron039  - shortwave infrared channel
  !> @param[in] micron110  - infrared channel
  !> @param[in] sat_zen    - satellite zenith
  !> @param[in] sol_zen    - solar zenith
  !> @param[in] rel_az     - relative azimuth between solar and satellite
  !> @param[inout] sw_refl - shortwave reflectance
  !> @param[out]   iret    - status; -1 if failure
  !
  !----------------------------------------------------------------------------

  subroutine getShortwaveReflectance(nx, ny, arfLUTfile, micron039, micron110, sat_zen, sol_zen, rel_az, sw_refl, iret)
    implicit none
    integer, intent(in) :: nx, ny
    CHARACTER(len=*), INTENT(in) :: arfLUTfile
    REAL, DIMENSION(:,:), INTENT(in) :: micron039, micron110, sat_zen, sol_zen,rel_az
    REAL, DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: sw_refl
    INTEGER, intent(out) :: iret


    REAL, allocatable :: satZenithBox(:)
    REAL, allocatable :: sunZenithBox(:)
    REAL, allocatable :: relAzimuthBox(:)
    REAL, allocatable :: lowCloudArf(:, :, :)

    REAL(kind=BYTE8) :: ch2, ch4, arf, reflect
    REAL(kind=BYTE8) :: satz, sunz, azim  ! cosine of an angle

    integer :: i, j

    iret = -1

    ! allocate memory before m_readIrTable()
    allocate(satZenithBox(NUM_SAT_ARF-1))
    allocate(sunZenithBox(NUM_SUN_ARF-1))
    allocate(relAzimuthBox(NUM_REL_AZIM_ARF-1))
    allocate(lowCloudArf(NUM_SUN_ARF, NUM_REL_AZIM_ARF, NUM_SAT_ARF))

    ! Anisotropic Reflectance Factors (ARF)
    CALL m_readIrTable(arfLUTfile, satZenithBox, sunZenithBox, relAzimuthBox, lowCloudArf, iret)
    IF(iret /= 0 ) THEN
       deallocate(satZenithBox)
       deallocate(sunZenithBox)
       deallocate(relAzimuthBox)
       deallocate(lowCloudArf)
       RETURN
    END IF

    sw_refl = MISSING

    loop_j: DO j = 1, ny
       loop_i: DO i = 1, nx

          ! if either channel 2 or channel 4 are bad go on to next point.
          IF( micron039(i,j) < MISSING + 1. .OR. micron110(i,j) <  MISSING + 1. &
             .or. sat_zen(i,j) < MISSING + 1. .OR. rel_az(i,j) <  MISSING + 1.) THEN
             CYCLE ! sw_refl (i, j) = MISSING
          END IF

          ch2 = micron039(i, j)
          ch4 = micron110(i, j)
          satz = COS(D2R*sat_zen(i, j))
          sunz = COS(D2R*sol_zen(i, j))
          azim = COS(D2R*rel_az(i, j))
      
          ! Locate the arf value and compute reflectance
          IF ((satz >= -1 .AND. satz <= 1) .AND. &
              (sunz >= -1 .AND. sunz <= 1) .AND. &
              (azim >= -1 .AND. azim <= 1)) THEN
             arf = m_getArf(satz, sunz, azim, satZenithBox, sunZenithBox, &
                            relAzimuthBox, lowCloudArf)
             reflect = m_reflectance(ch2, ch4, sunz, arf)
          END IF

          IF ((reflect > MISSING + 1.) .AND. (reflect < 0.0 .OR. reflect > 100.0)) THEN
!write(*,*) "big or small (never happen")", i, j, reflect
             reflect = MISSING
          END IF

          sw_refl(i, j) = reflect

       END DO loop_i
    END DO loop_j

    ! release memory
    deallocate(satZenithBox)
    deallocate(sunZenithBox)
    deallocate(relAzimuthBox)
    deallocate(lowCloudArf)

    iret = 0

    RETURN
  END subroutine getShortwaveReflectance

!**********************************************************************
! * subroutine: m_readIrTable()
! *
! * Description: creates lookup table from IR files
! *
! * Returns:
! *
! * Notes: Our definition of relazm is opposite of the definition
! *		in DISORT or 2-STREAM 
! *       
  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> convert the day of a year to month and date
  !
  !> @param[in] 
  !> @param[in] 
  !> @param[out] 
  !> @param[out] 
  !> @param[out] 
  !
  !----------------------------------------------------------------------------

  SUBROUTINE m_readIrTable(filename, satZenithBox, sunZenithBox, relAzimuthBox, lowCloudArf, iret)
    IMPLICIT NONE
    CHARACTER(len=*), INTENT(in) :: filename
    real, intent(inout) :: satZenithBox(:)
    real, intent(inout) :: sunZenithBox(:)
    real, intent(inout) :: relAzimuthBox(:)
    real, intent(inout) :: lowCloudArf(:,:,:)
    INTEGER, INTENT(out) :: iret

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! important note: Our definition of relazm is opposite of the
    ! definition in DISORT or 2-STREAM 
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

    REAL(kind=BYTE8) :: umu0(0:NUM_SAT_ARF-1), phi(0:NUM_REL_AZIM_ARF-1), umu(0:NUM_SAT_ARF-1)
    REAL(kind=BYTE8) :: dsatzen

    INTEGER :: iunit, i, j, k
    CHARACTER(len=20) :: fmtStr

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Read in the channel 3 ARF table 
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    iunit = 20
    OPEN(unit=iunit, file=filename, action='read', status='old', iostat=iret)
    IF (iret /= 0) THEN
      WRITE(*, *) 'Satellite::m_readIrTable() Error open IR file ', TRIM(filename), ', iostat = ', iret
      RETURN
    END IF

    WRITE(fmtStr, "(A4, I2.2, A5)") "(1x,",NUM_SUN_ARF , "F8.4)"
    DO k = 1,  NUM_SAT_ARF! difference between Fortran & C++
       DO j = 1, NUM_REL_AZIM_ARF
          READ(iunit, fmtStr) lowCloudArf(:, j, k)
       END DO
    END DO

    CLOSE(iunit, iostat=iret)

    ! some printouts
    write(*,*)"ARF table 5,5,1=", lowCloudArf( 5,5,1)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Calculate the (upper) box boundaries 
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

    ! satZenithBox

    dsatzen = D2R*89.0/NUM_SAT_ARF
    DO i = 0, NUM_SAT_ARF-1
       umu(NUM_SAT_ARF-i-1) = COS(i*dsatzen)
    END DO

    DO i = 1, NUM_SAT_ARF-1
       satZenithBox(i) = (umu(i-1)+umu(i))/2.0
    END DO

    ! sunZenithBox

    DO i = 0, NUM_SUN_ARF-1
       umu0(i) = 0.02 + i*0.025
    END DO

    DO i = 1, NUM_SUN_ARF-1
       sunZenithBox(i) = (umu0(i-1)+umu0(i))/2.0
    END DO

    ! relAzimuthBox

    DO i = 0, NUM_REL_AZIM_ARF-1
       phi(i) = i * 5.0
    END DO

    DO i = 1, NUM_REL_AZIM_ARF-1
       relAzimuthBox(i) = (phi(i-1)+phi(i))/2.0
    END DO

    iret = 0

    RETURN
  END SUBROUTINE m_readIrTable

!**********************************************************************
! * function: m_getArf()
! *
! * Description:fetches the arf value from the lookup table
! *
! * Returns:
! *
! * Notes: Important note: Our definition of relazm is opposite of the
! *	   definition in DISORT or 2-STREAM   
! *    
! *		_lowCloudArf  array 1st element = cos(sun angle), 
! *             2nd element = azimuth in deg, 3rd element = cos(sat angle)
! *		azim        + relative azimuth angle index
! *		satz        + satellite zenith angle index
! *		sunz        + solar zenith angle index
!  
  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> convert the day of a year to month and date
  !
  !> @param[in] 
  !> @param[in] 
  !> @param[out] 
  !> @param[out] 
  !> @param[out] 
  !
  !----------------------------------------------------------------------------

  REAL(kind=BYTE8) FUNCTION m_getArf(satz, sunz, azim, &
       satZenithBox, sunZenithBox, relAzimuthBox, lowCloudArf)
    implicit none
    REAL(kind=BYTE8), INTENT(in) :: satz, sunz, azim
    real, intent(in) :: satZenithBox(:)
    real, intent(in) :: sunZenithBox(:)
    real, intent(in) :: relAzimuthBox(:)
    real, intent(in) :: lowCloudArf(:,:,:)

    INTEGER :: isat, isun, iazm
    REAL(kind=BYTE8) :: relazm, qi

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Determine satellite zenith angle bin number 
    ! Make sure this agrees with the ascending or descending  
    ! nature of the table (boxes are upper bounds)  
    DO isat = 1, NUM_SAT_ARF-1
       IF(satz <= satZenithBox(isat)) EXIT
    END DO
    ! isat is NUM_SAT_ARF now if the 'exit' condition is never met.

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Determine sun zenith angle bin number
    DO isun = 1, NUM_SUN_ARF-1
       IF(sunz <= sunZenithBox(isun)) EXIT
    END DO
    ! isun is  NUM_SUN_ARF now if the 'exit' condition is never met.

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Determine relative azimuth angle bin number  
    ! important note: Our definition of relazm is opposite of the 
    ! definition in DISORT or 2-STREAM
    IF (azim >= -1.0 .AND. azim <= 1.0) THEN
       relazm= R2D*ACOS(azim)
    ELSE 
       WRITE(*, *) "Satellite::m_getArf() Error in ACOS  azim= ", azim, " satz= ", satz, " sunz= ", sunz
       m_getArf = 0.0
       RETURN
    END IF

    qi = ABS(180.0 - relazm)
    DO iazm = 1, NUM_REL_AZIM_ARF-1
       IF (qi <= relAzimuthBox(iazm)) EXIT
    END DO

    ! In C++, it should be lowCloudArf(isat, iazm, isun)
    ! In Fortran, memory storage sequence is inverted
    m_getArf = lowCloudArf(isun, iazm, isat)

    RETURN
  END FUNCTION m_getArf

!**********************************************************************
! * function: m_reflectance()
! *
! * Description: Compute channel 2 reflectance using the arf value 
! *
! * Returns:
! *
! * Notes: This method is tailored for gvar channels 2 and 4 
! *		For channel 2, the reflectance is the difference between measured
! *		radiance and estimated thermal emission. For channel 4 and 5, 
! *		reflectance is estimated to be approximately 0.
! *     
! *		option   reflectance calculation options:
! *		1   with solar zenith angle scaling
! *		2   like 1 plus the low cloud asymmetry factor
! *
! *
  !----------------------------------------------------------------------------
  ! DESCRIPTION:
  !> convert the day of a year to month and date
  !
  !> @param[in] 
  !> @param[in] 
  !> @param[out] 
  !> @param[out] 
  !> @param[out] 
  !
  !----------------------------------------------------------------------------

  REAL(kind=BYTE8) FUNCTION m_reflectance(ch2, ch4, sunz, arf)
    IMPLICIT NONE
    REAL(kind=BYTE8), INTENT(in) :: ch2, ch4, sunz, arf

   ! Constants for Planck function
    REAL(kind=BYTE8), PARAMETER :: c1 = 1.191066e-5
    REAL(kind=BYTE8), PARAMETER :: c2 = 1.438833

    REAL(kind=BYTE8) :: nu, btemp, radiance, emission, top, bottom

   ! Solar constant
    REAL(kind=BYTE8) :: f0_gvar_ch2
    f0_gvar_ch2 = 1.458E-6 ! W m^-2 um^-1, this is the value found in modtran3
    f0_gvar_ch2 = 1.0E7*f0_gvar_ch2/PI  ! convert it to mW m^-2 cm^-1 / pi 

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    !  If sun angle is larger than SolarAngleMax 
    !  (to be consistent with visible channel)
    !  If ch4 temperature is too cold then return 
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    IF (sunz < COS(CFG_SolarAngleMax*D2R) .OR. ch4 < CFG_MICRON_110ShortwaveReflThreshold) THEN
       m_reflectance = 0.0
       RETURN
    END IF

    ! Calculate channel 2 radiance
    nu = 2556.90  ! channel 2 on GOES-8, the low-IR 
    btemp = ch2 + 273.15
    radiance = c1*nu*nu*nu/(EXP(c2*nu/btemp) - 1.0)

    ! Calculate channel 4 radiance, assuming that ch2 emission = ch4 radiance
    ! nu= 934.30   ! channel 4 on GOES-8, the 11 um IR
    btemp = ch4 + 273.15
    emission = c1*nu*nu*nu/(EXP(c2*nu/btemp) - 1.0)

    top = radiance - emission
    bottom = f0_gvar_ch2*sunz*arf  - emission

    if(abs(bottom) < 0.01) then
       m_reflectance = 0.0
       return
    endif

    !Avoid false positives   
    IF (top <= 0.0 .OR. bottom <= 0.0) THEN
       m_reflectance = MISSING
       RETURN 
    END IF

    m_reflectance  = 100.0*(top/bottom)

    RETURN
  END FUNCTION m_reflectance


END MODULE Satellite
