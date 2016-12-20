!*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
!
! Module:	RadarMain
!
! Author:	Yali Mao
!
! Date:	        January 2011
!
! Description:  
!
!*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*

module Radar

  use Kinds
  use GridTemplate, only : zoominGDS, convertProjection
  use Config
  use Grib1
  use Grib2

  IMPLICIT NONE

  private
  public runRadar

  ! parameters for radar processing: processRadar()
  integer, parameter :: N_VIP_LEVEL = 6
  real, parameter :: VipMap(N_VIP_LEVEL) = (/ 57.0, 50.0, 46.0, 41.0, 30.0, 18.0 /)
  ! 6 levels divide radar reflectivity to 7 fields
  ! "VIP>=1", "VIP==1", "VIP==2", "VIP==3", "VIP==4", "VIP==5", "VIP==6"
  ! details:
  ! "VIP greater than or equal to 1",  "VIP equal to 1", "VIP equal to 2"
  ! "VIP equal to 3", "VIP equal to 4", "VIP equal to 5", "VIP equal to 6"
  integer, parameter :: N_VIP_FIELDS = 7
  real, parameter :: MAX_DBZ = 120

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! private members (configration related)
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  integer :: CFG_GRIB
  ! Unisys
  integer :: CFG_Unisys_VALID_SOURCE_ID	! = 10000
  real :: CFG_Unisys_SCALE	! = 1.0
  real :: CFG_Unisys_BIAS	! = 0.0
  integer :: CFG_Unisys_Igrid	! = 3 ! LAMBERT CONFORMAL CONICAL
  integer :: CFG_Unisys_Lat1	! LATIN 1 - FIRST LAT FROM POLE OF SECANT CONE INTER
  integer :: CFG_Unisys_Lat2	! LATIN 2 - SECOND LAT FROM POLE OF SECANT CONE INTER
  ! NSSL, configuration only for base reflectivity
  integer :: CFG_NSSL_nz_base
  integer, pointer :: CFG_NSSL_pds7_base(:)
  ! Radar processing
  character(len=10) :: CFG_source ! UNISYS/NSSL
  integer :: CFG_nfiner
  real, pointer :: CFG_PERCENTILES(:) ! = (/ 25.0, 75.0 /) 
  integer :: CFG_NPERCENTILES   ! = 2
  integer :: CFG_MinVipPoints 	! = 10
  integer :: CFG_MinDBzPoints   ! = 10

contains




!**********************************************************************
! * Description: provide interface to get radar data.
! *              the closest data to the run time 
! *
! * Inputs:
! *        csource: which radar source. -1 is default, it is Unisys
! *

  subroutine runRadar(filename, cfg, topography, kgds, radarData, iret)
    IMPLICIT NONE

    character(len=*), intent(in) :: filename  ! input radar file
    type(config_t), target :: cfg
    real, intent(in) :: topography(:, :) ! at model's dimension, for NSSL radar data
    integer, intent(in) :: kgds(:) ! model GDS
    type(radar_data_t), INTENT(inout) :: radarData
    integer,            intent(out) :: iret

    ! radar reflectivity (base/composite)
    ! Fortran doesn't have 'unsigned' type, use real instead
    real, allocatable :: rfl(:,:) 

    ! for projection
    integer :: sgds(200) ! radar data GDS
    integer :: tgds(200) ! radar converted projection, nfiner than model
    real, allocatable :: r_dbzData(:,:)
    integer(kind=BYTE2), allocatable :: dbzData(:,:)

    integer :: nx, ny

    ! configuration fields
    CFG_GRIB = cfg% model% grib ! use the same Grib # as model's
    ! Unisys
    CFG_Unisys_VALID_SOURCE_ID = cfg%radar%unisys%valid_source_id
    CFG_Unisys_SCALE = cfg%radar%unisys%data_scale
    CFG_Unisys_BIAS = cfg%radar%unisys%data_bias
    CFG_Unisys_Igrid = cfg%radar%unisys%igrid
    CFG_Unisys_Lat1 = cfg%radar%unisys%lat1
    CFG_Unisys_Lat2 = cfg%radar%unisys%lat2
    ! NSSL
    CFG_NSSL_nz_base = cfg%radar%nssl%nz_base
    CFG_NSSL_pds7_base => cfg%radar%nssl%pds7_base

    ! Radar processing
    CFG_source = cfg%radar%source
    CFG_nfiner = cfg%radar%nfiner
    CFG_PERCENTILES => cfg%radar%percentiles
    CFG_NPERCENTILES = size(CFG_PERCENTILES)
    CFG_MinVipPoints = cfg%radar%minVipPoints
    CFG_MinDBzPoints = cfg%radar%minDBzPoints

    iret = -1

    write(*,*) "Reading Radar Data file: ", trim(filename), LF
    if(CFG_source == "NSSL") then
       call readRadarNSSL_Composite(CFG_GRIB, filename, rfl, sgds, iret)
!       call readRadarNSSL_Base(CFG_GRIB, filename, topography, kgds, rfl, sgds, iret)
    else if(CFG_source == "UNISYS") then
       call readRadarUnisys(filename, rfl, sgds, iret)
    end if
    if(iret /= 0) then
       write(*,*) "Radar::runRadar(), Error when reading radar data file"
       return
    end if

    ! Convert radar's projection similar to model's, but at higher resolution

    call zoominGDS(kgds, CFG_nfiner, tgds)

    nx = tgds(2)
    ny = tgds(3)
    allocate(dbzData(nx, ny))
    allocate(r_dbzData(nx, ny))


    call convertProjection(sgds, rfl, tgds, r_dbzData, iret)
    dbzData(:,:) = nint(r_dbzData(:,:))
    where (dbzData(:,:) < 0) dbzData(:,:) = MISSING_INT
    deallocate(rfl)
    deallocate(r_dbzData)


    !================================================!
    ! Process radar data, get vip percentiles and dbz precentiles
    !================================================!

    write(*,*) LF, "Processing Radar data ..."
    call processRadar(dbzData, kgds, CFG_nfiner, radarData, iret)

    !================================================!
    ! release related memories
    !================================================!
    ! dbzData
    deallocate(dbzData)

    return
  end subroutine runRadar


!**********************************************************************
! * Subroutine: readRadarNSSL_Base()
! *
! * Description:
! *   Read in Level-II radar 3D mosaic data in Grib1
! *
! * Inputs:
! *   infile      : input radar data file name
! *   topography  : at model's dimension
! *
! * Output:
! *   baseRfl     : radar data at a higher resolution
!     kgds
! *   iret  : status (0 is successful)
! *
! * Notes: 
! *   (1) The 3D mosaic data are on the level of geometric height, topography of
! *       GFS is geopotential height. On the surface, the difference between 
! *       these two heights can be ignored.
! *   (2) The 3D mosaic data are float/real, it can and will be transferred to 
! *       integer, to save calculation. From radar's usage in CIP algorithm later,
! *       there will be no big issue.
! *   (3) For NSSL radar data, no remap is needed for GFS.
! *   (4) To extract base reflectivity, topography needs to be remapped to radar's
! *       grid from model's grid. Topography is at a lower resolution of model's 
! *       grid and at a higher resolution of radar's. It will be fine since radar
! *       data isn't usually accurate over complicated orography.
! *   

  subroutine readRadarNSSL_Base(igrib,infile, topography, kgds_model, baseRfl, kgds, iret)
    implicit NONE
    integer, intent(in) :: igrib
    character(len=*), intent(in) :: infile  ! Grid file
    real, intent(in) :: topography(:, :)    ! at model's dimension
    integer, intent(in) :: kgds_model(:)
    real, allocatable, intent(out) :: baseRfl(:,:)
    integer, intent(out) :: kgds(:)         ! original radar GDS
    integer, intent(out) :: iret

    ! For a single radar, the height of its max detection range at 0.5 degree angle
    real, parameter :: MAX_HEIGHT_BASE_REFLECTIVITY = 2000.0 ! meter

    integer :: nx, ny, nxy
    real, allocatable :: mosaic3Ddata(:, :, :)
    real, allocatable :: topo(:, :) ! at radar's dimension

    integer :: i, j, k

    integer :: iunit ! 001 - 999

    ! Grib 1
    integer :: pds5=211, pds6=103, pds7
    ! Grib 2
    integer :: npdt=0, icat=16, iprm=195, ilev=102
    integer :: igrid

    character(len=*), parameter :: myself =  'Radar::readRadarNSSL_Base() '

    !================================================!
    ! Open Grib file
    !================================================!
    iunit = 16
    call BAOPENR(iunit, infile, iret)
    if (iret /= 0) then
       write(*, *) myself, 'open file error: ', trim(infile), ' iret=', iret
       return
    end if

    !================================================!
    ! Get the header of radar data (kgds)
    !================================================!
    if(igrib == 1) then
       call getHeaderGB1(iunit, kgds)
    elseif(igrib == 2) then
       call getHeaderGB2(iunit, kgds, igrid, iret)
    end if
    nx = kgds(2)
    ny = kgds(3)
    nxy = nx * ny

    ! allocate memory for 3D mosaic radar data
    allocate(mosaic3Ddata(nx, ny, CFG_NSSL_nz_base), stat=iret)
    if (iret /= 0) then
       write(*, *)myself, 'unable to allocate memory for source radar data. iret=', iret
       return
    end if

    !================================================!
    ! read the 3D radar data
    !================================================!
    do k = 1, CFG_NSSL_nz_base
       pds7 = CFG_NSSL_pds7_base(k)
       if(igrib == 1) then
          call readGB1(iunit, nxy, pds5, pds6, pds7, mosaic3Ddata(:,:,k), iret)
          if (iret /= 0) then
             write(*, *)myself, "reading radar error ",pds5, pds6, pds7, " iret=", iret
             return
          end if
       elseif(igrib == 2) then
          call readGB2(iunit,npdt,icat,iprm,ilev,pds7,mosaic3Ddata(:,:,k),iret)
          if (iret /= 0) then
             write(*, *)myself, "reading radar error ",npdt,icat,iprm,ilev,pds7, " iret=", iret
             return
          end if
       end if
    end do

    call BACLOSE(iunit, iret)

print *, "reading is done"

    !================================================!
    ! derive base reflectivity from 3D mosaic radar data
    !================================================!

    ! remap topography from model-grid to radar-grid
    allocate(topo(nx, ny))
    call convertProjection(kgds_model, topography, kgds, topo, iret)
print *, "first conversion is done"

    ! for each grid (i,j), search 3D radar data from the lowest level to top,
    ! within 2km above the surface, the first valid data is base reflectivity;
    ! if no valid data is found within 2km, the base relectivity is marked MISSING
    allocate(baseRfl(nx, ny))

    baseRfl(:,:) = MISSING
    do j = 1, ny
    do i = 1, nx
       do k = 1, CFG_NSSL_nz_base
          if(CFG_NSSL_pds7_base(k) - topo(i,j) <= MAX_HEIGHT_BASE_REFLECTIVITY) then
             if(mosaic3Ddata(i,j,k) >= 0 ) then
                baseRfl(i,j) = mosaic3Ddata(i,j,k)
                exit
             end if
          else
             exit
          end if
       end do
    end do
    end do

    deallocate(mosaic3Ddata)
    deallocate(topo)

    iret = 0
    return
  end subroutine readRadarNSSL_Base


  subroutine readRadarNSSL_Composite(igrib, infile, rfl, kgds, iret)
    implicit NONE
    integer, intent(in) :: igrib
    character(len=*), intent(in) :: infile  ! Grid file
    real, allocatable, intent(out) :: rfl(:,:)
    integer, intent(out) :: kgds(:)         ! original radar GDS
    integer, intent(out) :: iret

    integer :: nx, ny, nxy

    integer :: iunit ! 001 - 999

    ! Grib 1
    integer :: pds5=212, pds6=200, pds7
    ! Grib 2
    integer :: npdt=0, icat=16, iprm=196, ilev=200
    integer :: igrid

    character(len=*), parameter :: myself = 'Radar::readRadarNSSL_Composite() '

    !================================================!
    ! Open Grib file
    !================================================!
    iunit = 16
    call BAOPENR(iunit, infile, iret)
    if (iret /= 0) then
       write(*, *) myself, 'open file error: ', trim(infile), ' iret=', iret
       return
    end if

    !================================================!
    ! Get the header of radar data (kgds)
    !================================================!
    if(igrib == 1) then
       call getHeaderGB1(iunit, kgds)
    elseif(igrib == 2) then
       call getHeaderGB2(iunit, kgds, igrid, iret)
    end if
    nx = kgds(2)
    ny = kgds(3)
    nxy = nx * ny

    ! allocate memory for radar data
    allocate(rfl(nx, ny), stat=iret)
    if (iret /= 0) then
       write(*, *)myself, 'unable to allocate memory for source radar data. iret=', iret
       return
    end if

    !================================================!
    ! read the 2D radar data (column composite)
    !================================================!
    pds7 = 0
    if(igrib == 1) then
       call readGB1(iunit, nxy, pds5, pds6, pds7, rfl(:,:), iret)
       if (iret /= 0) then
          write(*, *)myself, "reading radar error ",pds5, pds6, pds7, " iret=", iret
          return
       end if
    elseif(igrib == 2) then
       call readGB2(iunit,npdt,icat,iprm,ilev,pds7,rfl(:,:),iret)
       if (iret /= 0) then
          write(*, *)myself, "reading radar error ",npdt,icat,iprm,ilev,pds7, " iret=", iret
          return
       end if
    end if


    where (rfl < 0.) rfl = MISSING

    call BACLOSE(iunit, iret)

print *, "reading is done"

    iret = 0
    return
  end subroutine readRadarNSSL_Composite



!**********************************************************************
! * subroutine: readRadarUnisys() - Reads the given Unisys mosiac file into the internal
! *           buffer.
! *
! * Output:
! *   iret == 0 if successfull, non-zero otherwise.
! *

  subroutine readRadarUnisys(infile, rfl, kgds, iret)
    implicit none

    character(len=*), intent(in) :: infile
     ! Fortran doesn't have 'unsigned' type, use real instead
    real, allocatable, intent(out) :: rfl(:,:) ! only save integer part
    integer, intent(out) :: kgds(:)
    integer, intent(out) :: iret

    integer, parameter :: NUM_HEADER_SHORTS = 79

    integer :: iunit
    integer :: data_buffer_alloc, shorts_read, rec


    logical :: ex
    integer :: i

    integer(kind=BYTE2), allocatable :: data_buffer(:)
    integer(kind=BYTE2) :: oneData

    integer(kind=BYTE2) :: valueTable(16) ! Fortran doesn't have 'unsigned' type

    integer :: nx, ny

    ! Status of the input file to make sure it exists and to get the file
    ! size for allocating the input buffer below
    inquire(file=infile, exist = ex, size = data_buffer_alloc)
    if(.not. ex) then
       write(*, *) "Radar::readRadarUnisys() ERROR status (inquire) for input file: ",  infile
       iret = -10
       return
    end if

write(*,*) "File size =", data_buffer_alloc, LF
  
    !================================================!
    ! Open the input file, recl=Short
    !================================================!
    iunit = 16
    open(unit=iunit, file=infile, form='unformatted', iostat=iret, action="read", &
         access="direct", recl=BYTE2)
    if(iret /= 0) then
       write(*,*) "Radar::readRadarUnisys() error: ", iret, " can not open file: ", trim(infile)
       return
    end if

    !================================================!
    ! Read the header information
    !================================================!
    call m_readHeader(NUM_HEADER_SHORTS, iunit, kgds, valueTable, iret)
    if (iret /= 0) then
       write(*,*) "reading headers error, iret=", iret
       return
    end if

    !================================================!
    ! Create memory for the data buffer
    !================================================!

    ! data_buffer_alloc: from bytes to shorts (2-bytes)
    data_buffer_alloc = data_buffer_alloc/2 - NUM_HEADER_SHORTS 

write(*,*) LF, "The original data size is ", data_buffer_alloc
    allocate(data_buffer(data_buffer_alloc), stat=iret)
    if (iret /= 0) then
       write(*,*)  "Radar::readRadarUnisys() ERROR when allocate memory of data_buffer. iret=", iret
       return
    end if

    !================================================!
    ! Read the data from the input file
    !================================================!

    shorts_read = 0
    rec = NUM_HEADER_SHORTS+1
    do ! read in data 2-bytes by 2-bytes
       read(iunit, rec=rec, iostat=iret) oneData
       if(iret < 0) then  !loop breaks at the end of file/record
          exit
       else if (iret > 0) then
!write(*,*) shorts_read, rec ! rec-shorts_read shoud equal to 80
          if(iret == 1 .or. iret == 2) then  !loop breaks when finishing reading Unisys radar
             exit
          else  ! Something error            
             write(*, *) 'Radar::readRadarUnisys() Error reading line, iostat=', iret
             close(iunit)
             return
          end if
       end if

       rec = rec + 1
       shorts_read = shorts_read + 1
       data_buffer(shorts_read) = oneData
    end do

    if (shorts_read /= data_buffer_alloc) then
       write(*,*) "Radar::readRadarUnisys() ERROR reading input file. ", &
            "Expected ", data_buffer_alloc, "bytes, got ", shorts_read, " bytes"
       close(iunit)
       iret = -8
       return
    end if

    close(iunit, iostat=iret)


    !================================================!
    ! Expand the data from the original RLE format
    ! Expanded to 2-Dimension rfl
    ! RLE : Run Length Encoded data
    !================================================!
    ! data_buffer => rfl,    release data_buffer
    !================================================!

    nx = kgds(2)
    ny = kgds(3)

    ! Allocate space for two-dimension radar image data
    allocate(rfl(nx, ny), stat=iret)
    if (iret /= 0) then
       write(*,*)  "Radar::readRadarUnisys() ERROR when allocate memory of rfl. iret=", iret
       return
    end if
    rfl = 0.

    if (.not. m_expandRLE(data_buffer, valueTable, nx, ny, rfl)) then
       write(*,*) "Radar::readRadarUnisys() error - expanding the Unisys RLE data"    
       iret = -7
       return
    end if
    deallocate(data_buffer, stat=iret)

    iret = 0
    return
  end subroutine readRadarUnisys


!**********************************************************************
! * subroutine:  m_readHeader() - Read the header information from the given
! *            file and update the local header members.
! *
! * Output:
! *   0 on success, others on failure.
! *

  subroutine m_readHeader(length_header, iunit, kgds, valueTable, iret)
    IMPLICIT NONE
    integer, intent(in) :: length_header
    integer, intent(in) :: iunit ! file unit
    integer, intent(out) :: kgds(:)
    integer(kind=BYTE2), intent(out) :: valueTable(16)
    integer, intent(out) :: iret

    integer :: rec, i

    real :: unisys_value_table(16)
    integer :: itable_value


    real :: unisysPixelRes

    integer(kind=BYTE2) :: headers(length_header)

    ! The source ID rather than being the site ID is a number greater than 10000 
    ! (10000 for national mosaics, 10001-10007 for the regional mosaics).  
    ! The rastor data is placed on a Lambert Conformal projection centered at 
    ! 98 west and having true latitudes of 33 and 45 north.  
    type :: unisys_header_t
       !================================================!
       ! Variable names and descriptions are from:
       !
       ! WeatherMAX(TM) - Site & Product Catalog
       ! Appendix A
       ! MOSAIC RASTER FORMAT PRODUCT FILE FORMAT
       ! VER 3.00
       ! Pages A-1 through A-6
       !
       ! Each field is specified as a 16 BIT INTEGER
       ! and some variables span multiple fields
       !
       ! [Most of the header fields need byte swapping. -mp]
       !
       !================================================!
       !
       !                           ! FIELD COMMENTS
       !                           ! ----- -------------------------------------------
       integer(kind=BYTE2) :: ProductID              ! 01    Product ID
       integer(kind=BYTE2) :: DaysSince1970          ! 02    Date (Days since 1/1/1970)
       integer(kind=BYTE4) :: SecsSinceMidnight     ! 03,04 Time (Seconds since midnight)
       integer(kind=BYTE4) :: TotalLengthBytes      ! 05,06 Total Length in bytes
       !                           !       (Field 05 = MSW, 06 = LSW)
       !                           !       [seems to be length of entire file -mp]
       integer(kind=BYTE2) :: SourceID               ! 07    Source ID
       !                           !       (Site Dependent. e.g., National = 10000)
       integer(kind=BYTE2) :: DestinationID          ! 08    Destination ID (unused, Constant = 0)
       integer(kind=BYTE2) :: NumberOfBlocks         ! 09    Number of Blocks (Constant = 3)
       integer(kind=BYTE2) :: BlockDivider1          ! 10    Block Divider (Constant = -1)
       integer(kind=BYTE4) :: CenterLatitude        ! 11,12 Center Latitude (Degrees * 1000)
       !                           !       (e.g., 38000 indicates 38.0 north
       !                           !       (Field 11 = MSW, 12 = LSW)
       integer(kind=BYTE4) :: CenterLongitude        ! 13,14 Center Longitude (Degrees * 1000)
       !                           !       (e.g., -98000 indicates 98.0 west
       !                           !       (Field 13 = MSW, 14 = LSW)
       integer(kind=BYTE2) :: HeightOfRadar          ! 15    Height of Radar (unused, Constant = 0)
       integer(kind=BYTE2) :: ProductID2             ! 16    Product ID (same as field #1)
       integer(kind=BYTE2) :: OperationalMode        ! 17    Operational Mode (Constant = 2)
       integer(kind=BYTE2) :: VolumeCoveragePattern  ! 18    Volume Coverage Pattern (Constant = 0)
       integer(kind=BYTE2) :: SequenceNumber         ! 19    Sequence Number (unused, Constant = 0)
       integer(kind=BYTE2) :: VolumeScanNumber       ! 20    Volume Scan Nummber (unused, Constant = 0)
       integer(kind=BYTE2) :: Date_2                 ! 21    Date (Days since 1/1/1970)(same as field #2)
       integer(kind=BYTE4) :: Time_2                ! 22,23 Time (Seconds since midnight)
       !                           !       (same as fields #3 and #4)
       !                           !       (Field 22 = MSW, 23 = LSW)
       integer(kind=BYTE2) :: Date_3                 ! 24    Date (Days since 1/1/1970)(same as field #2)
       integer(kind=BYTE4) :: Time_3                ! 25,26 Time (Seconds since midnight)
       !                           !       (same as fields #3 and #4)
       !                           !       (Field 25 = MSW, 26 = LSW)
       integer(kind=BYTE2) :: ProductDependent1      ! 27    Product Dependent (unused, Constant = 0)
       integer(kind=BYTE2) :: ProductDependent2      ! 28    Product Dependent (unused, Constant = 0)
       integer(kind=BYTE2) :: ElevationNumber        ! 29    Elevation Number (unused, Constant = 0)
       integer(kind=BYTE2) :: ProductDependent3      ! 30    Product Dependent (unused, Constant = 0)
       integer(kind=BYTE2) :: DataLevel1             ! 31    Data Level 1 (same format as NEXRAD products)
       integer(kind=BYTE2) :: DataLevel2             ! 32    Data Level 2
       integer(kind=BYTE2) :: DataLevel3             ! 33    Data Level 3
       integer(kind=BYTE2) :: DataLevel4             ! 34    Data Level 4
       integer(kind=BYTE2) :: DataLevel5             ! 35    Data Level 5
       integer(kind=BYTE2) :: DataLevel6             ! 36    Data Level 6
       integer(kind=BYTE2) :: DataLevel7             ! 37    Data Level 7
       integer(kind=BYTE2) :: DataLevel8             ! 38    Data Level 8
       integer(kind=BYTE2) :: DataLevel9             ! 39    Data Level 9
       integer(kind=BYTE2) :: DataLevel10            ! 40    Data Level 10
       integer(kind=BYTE2) :: DataLevel11            ! 41    Data Level 11
       integer(kind=BYTE2) :: DataLevel12            ! 42    Data Level 12
       integer(kind=BYTE2) :: DataLevel13            ! 43    Data Level 13
       integer(kind=BYTE2) :: DataLevel14            ! 44    Data Level 14
       integer(kind=BYTE2) :: DataLevel15            ! 45    Data Level 15
       integer(kind=BYTE2) :: DataLevel16            ! 46    Data Level 16
       integer(kind=BYTE2) :: MaximumDataLevel       ! 47    Maximum Data Level
       integer(kind=BYTE2) :: ProductDependent4      ! 48    Product Dependent (unused, Constant = 0)
       integer(kind=BYTE2) :: ProductDependent5      ! 49    Product Dependent (unused, Constant = 0)
       integer(kind=BYTE2) :: ProductDependent6      ! 50    Product Dependent (unused, Constant = 0)
       integer(kind=BYTE2) :: ProductDependent7      ! 51    Product Dependent (unused, Constant = 0)
       integer(kind=BYTE2) :: ProductDependent8      ! 52    Product Dependent (unused, Constant = 0)
       integer(kind=BYTE2) :: NumberOfColumns        ! 53    Number of Columns
       !                           !       (Use this for raster display)
       integer(kind=BYTE2) :: NumberOfMaps           ! 54    Number of Maps (unused, Constant = 0)
       integer(kind=BYTE4) :: OffsetToSymbology     ! 55,56 Offset to Symbology (bytes, = 60)
       !                           !       (Field 55 = MSW, 56 = LSW)
       integer(kind=BYTE4) :: OffsetToGraphic       ! 57,58 Offset to Graphic (bytes, = 0)
       !                           !       (Field 57 = MSW, 58 = LSW)
       integer(kind=BYTE4) :: OffsetToTabular        ! 59,60 Offset to Tabular (bytes, = 0)
       !                           !       (Field 59 = MSW, 60 = LSW)
       !                           !
       ! ************************ PRODUCT SYMBOLOGY BLOCK ******************************!
       integer(kind=BYTE2) :: BlockDivider2          ! 61    Block Divider (Constant = -1)
       integer(kind=BYTE2) :: BlockID                ! 62    Block ID (Constant = 1)
       integer(kind=BYTE4) :: LengthOfBLock         ! 63,64 Length of Block (bytes)
       !                           !       (Field 63 = MSW, 64 = LSW)
       integer(kind=BYTE2) :: NumberOfLayers         ! 65    Number of Layers (Value of 2 indcates that the
       !                           !       "Site Identifier Layer" is present)
       !                           !
       ! ************************ THE "RASTER DATA LAYER" ******************************!
       integer(kind=BYTE2) :: LayerDivider1          ! 66    Layer Divider (Constant = -1)
       integer(kind=BYTE4) :: LengthOfRasterLayer   ! 67,68 Length of (Raster) Layer
       ! unsigned in C++           !       (Field 67 = MSW, 68 = LSW)
       integer(kind=BYTE2) :: RasterOpCode           ! 69    Raster Opcode (Constant= HEX BA07=DEC 47623)
       integer(kind=BYTE2) :: RasterConstant1        ! 70    Raster Constant (Constant= HEX 8000=DEC 32768)
       integer(kind=BYTE2) :: RasterConstant2        ! 71    Raster Constant (Constant= HEX 00C0=DEC 192)
       !
       integer(kind=BYTE2) :: XOrigin                ! 72    X Origin (unused, Constant = 0)
       integer(kind=BYTE2) :: YOrigin                ! 73    Y Origin (unused, Constant = 0)
       integer(kind=BYTE2) :: XScaleInt              ! 74    X Scale Integer (unused, Constant = 1)
       integer(kind=BYTE2) :: XScaleFrac             ! 75    X Scale Fractional (unused, Constant = 0)
       integer(kind=BYTE2) :: YScaleInt              ! 76    Y Scale Integer (unused, Constant = 1)
       integer(kind=BYTE2) :: YScaleFrac             ! 77    Y Scale Fractional (unused, Constant = 0)
       integer(kind=BYTE2) :: NumberOfRows           ! 78    Number of Rows (use for raster display)
       integer(kind=BYTE2) :: PackingDescriptor      ! 79    PackingDescriptor (Constant = 2)
       !
       ! The raster data begin at this point in the file:
       !                           !.....................................................
       !                           ! 80   Bytes in this row          (Row #1)
       !                           ! 81   Run0000000 0 Col 0  Run 1 Col 1   (Run length encoded
       !                           ! 82   Run 2 Col 2  Run 3 Col 3   data - Same format
       !                           ! 83   ...                        as NEXRAD)
       !                           !      ...                      
       !                           !      Run n Col n  0   0 0   0   [Run = run length as
       !                           !      |___| |___|  |___| |___|   number of colums,
       !                           !      HiNib LoNib  HiNib LoNib   [Col = color -mp]
       !                           !        
       !                           !      |_________|  |_________|   [Note that the last
       !                           !       High byte    Low byte     byte in the row
       !                           !                                 may be 0 -mp]
       !                           !      |______________________|
       !                           !           16 bit integer
       !                           !.....................................................
       !                           !      Bytes in this row          (Row #2)
       !                           !      Run 0 Col 0  Run 1 Col 1   (Run length encoded
       !                           !      Run 2 Col 2  Run 3 Col 3   data - Same format
       !                           !      ...                        as NEXRAD)
       !                           !      Run n Col n  0   0 0   0
       !                           !.....................................................
       !                           !      Bytes in this row          (Last Row
       !                           !                                 unisysHeader.NumberOfRows)
       !                           !      Run 0 Col 0  Run 1 Col 1   (Run length encoded
       !                           !      Run 2 Col 2  Run 3 Col 3   data - Same format
       !                           !      ...                        as NEXRAD)
       !                           !      Run n Col n  0   0 0   0
       !                           !.....................................................
       !                           !
       ! ************************ THE "SITE IDENTIFIER LAYER" **************************!
       !   [The site identifier layer is not being used in unisys2mdv at this time -mp]
       !
       !================================================!
       !
       ! Note: [from documentation]
       !
       ! 1.  National Mosaics are projected with a Lambert Conformal Conic Projection.
       !
       ! 2.  Mosic product format is nearly identical to NEXRAD (e.g., Composite
       !     Reflectivity) product format. Some fields may not apply and are set
       !     to constant values.
       !
       !     The date and time formats are the same (in (UTC). The data levels
       !     are encoded as in NEXRAD products. The raster image is in the NEXRAD
       !     de facto standard run length encoded format.
       !
       ! 3.  The 8 data lavel reflectivity mosaics use the NEXRAD 8 level
       !     Composite Reflectivity (Precipitation mode) levels:
       !     ND,5,18,30,41,46,50,57 dBZ.
       !
       ! 4.  The 16 data lavel reflectivity mosaics use the NEXRAD 16 level
       !     Composite Reflectivity  (Precipitation mode) levels:
       !     ND,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75 dBZ.
       !
       ! 5.  There can be an additional layer in the Product Symbology Block
       !     which identifies the included and excluded sites. This is present
       !     when the number of layers equals 2.
       !
       ! 6.  The national mosaic products contain a site ID of 10000.
       !     The center is at 38.0 north latitude and 98.0 west longitude.
       !
       ! 7.  The following relates spatial resolution to columns and rows
       !     for national mosaic products:
       !          2 km x  2 km    2560 columns x 1792 rows
       !          4 km x  4 km    1280 columns x  896 rows
       !          8 km x  8 km     640 columns x  448 rows
       !         16 km x 16 km     320 columns x  224 rows
       !         32 km x 32 km     160 columns x  112 rows
       !
       ! 8.  The number of columns is assigned to location 53. The number of rows
       !     is assigned to  location 78.
       !
       ! 9.  The Surface Rainfall Accumulation (e.g., 1-Hour Precipitation) msaic
       !     products use the NEXRAD 1-Hour Precipitation product data level values:
       !     ND, 0.0 ,0.1 ,0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0,
       !     6.0, 8.0, 10.0 inches.
       !
       ! 10. The Echo Tops mosaic products use the NEXRAD Echo Tops product data
       !     level values: 
       !     ND,5,10,15,20,25,30,35,40,45,50,55,60,65,70 kft MSL.
       !
       ! 11. The maximum data level value is assigned to field 47.
       !
       !     For Composite and Base Reflectivity mosaic products, the maximum data
       !     level is in dBZ (range -33 to 95, where -33 means no data).
       !
       !     For Surface Rainfall Accumulation (e.g., 1-Hour Precipitation) mosaic
       !     products the maximum data value is in tenths of inches
       !     (ramge: 0.0 to 198.0 inches).
       !
       !     For Echo Tops mosaic products, the maximum data level is in
       !     1000's of feet above mean sea lavel (MSL) (range: 0 to 70 kft MSL).
       !
       ! 12. Regional mosaic products are based on the national mosaic projection
       !     Lambert Conic Conformal centered at
       !             38.0 degrees north and 98.0 degrees west).
       !     The regional mosaic center point latitude/longitude appears in
       !     fields 11 to 14. Each region has a unique site ID number
       !     (listed in this ICD) which appears in field 7.
       !
       !     The following relates spatial resolution to columns and rows:
       !         4 km x 4 km    640 columns x 448 rows
       !
       ! 13. Custom mosaic products are based on the national mosaic projection
       !     Lambert Conic Conformal centered at
       !             38.0 degrees north and 98.0 degrees west).
       !     The custom mosaic center point latitude/longitude appears in
       !     fields 11 to 14. Each region has a unique site ID number
       !     (listed in this ICD) which appears in field 7.
       !
       !     The following relates spatial resolution to columns and rows:
       !         4 km x 4 km    232 columns x 232 rows   or
       !         4 km x 4 km    332 columns x 332 rows
       !
       !================================================!
       !
    end type unisys_header_t
    type(unisys_header_t) ::  unisysHeader

    headers = 0

    ! Read the header information from the given file
    ! all shorts are NUM_HEADER_SHORTS = 79
    do rec = 1, length_header
       read(iunit, rec=rec, iostat=iret) headers(rec)
       if(iret /= 0) exit
    end do
    if (iret /= 0) then
       write(*,*) "Radar::m_readHeader() ERROR reading header information from input file"
       return
    end if

    unisysHeader%ProductID = headers(1)
    unisysHeader%DaysSince1970 =  headers(2)
    unisysHeader%SecsSinceMidnight = m_16BitsTo32Bits(headers(3), headers(4))
    unisysHeader%TotalLengthBytes = m_16BitsTo32Bits(headers(5), headers(6))
    unisysHeader%SourceID = headers(7)
    unisysHeader%DestinationID = headers(8)
    unisysHeader%NumberOfBlocks = headers(9)
    unisysHeader%BlockDivider1 = headers(10)
    unisysHeader%CenterLatitude = m_16BitsTo32Bits(headers(11), headers(12))
    unisysHeader%CenterLongitude = m_16BitsTo32Bits(headers(13), headers(14))
    unisysHeader%HeightOfRadar = headers(15)
    unisysHeader%ProductID2 = headers(16)
    unisysHeader%OperationalMode = headers(17)
    unisysHeader%VolumeCoveragePattern = headers(18)
    unisysHeader%SequenceNumber = headers(19)
    unisysHeader%VolumeScanNumber = headers(20)
    unisysHeader%Date_2 = headers(21)
    unisysHeader%Time_2 =  m_16BitsTo32Bits(headers(22), headers(23))
    unisysHeader%Date_3 = headers(24)
    unisysHeader%Time_3 = m_16BitsTo32Bits(headers(25), headers(26))
    unisysHeader%ProductDependent1 = headers(27)
    unisysHeader%ProductDependent2 = headers(28)
    unisysHeader%ElevationNumber = headers(29)
    unisysHeader%ProductDependent3 = headers(30)
    unisysHeader%DataLevel1 = headers(31)
    unisysHeader%DataLevel2 = headers(32)
    unisysHeader%DataLevel3 = headers(33)
    unisysHeader%DataLevel4 = headers(34)
    unisysHeader%DataLevel5 = headers(35)
    unisysHeader%DataLevel6 = headers(36)
    unisysHeader%DataLevel7 = headers(37)
    unisysHeader%DataLevel8 = headers(38)
    unisysHeader%DataLevel9 = headers(39)
    unisysHeader%DataLevel10 = headers(40)
    unisysHeader%DataLevel11 = headers(41)
    unisysHeader%DataLevel12 = headers(42)
    unisysHeader%DataLevel13 = headers(43)
    unisysHeader%DataLevel14 = headers(44)
    unisysHeader%DataLevel15 = headers(45)
    unisysHeader%DataLevel16 = headers(46)
    unisysHeader%MaximumDataLevel = headers(47)
    unisysHeader%ProductDependent4 = headers(48)
    unisysHeader%ProductDependent5 = headers(49)
    unisysHeader%ProductDependent6 = headers(50)
    unisysHeader%ProductDependent7 = headers(51)
    unisysHeader%ProductDependent8 = headers(52)
    unisysHeader%NumberOfColumns = headers(53)
    unisysHeader%NumberOfMaps = headers(54)
    unisysHeader%OffsetToSymbology = m_16BitsTo32Bits(headers(55), headers(56))
    unisysHeader%OffsetToGraphic = m_16BitsTo32Bits(headers(57), headers(58))
    unisysHeader%OffsetToTabular = m_16BitsTo32Bits(headers(59), headers(60))
    unisysHeader%BlockDivider2 = headers(61)
    unisysHeader%BlockID = headers(62)
    unisysHeader%LengthOfBLock = m_16BitsTo32Bits(headers(63), headers(64))
    unisysHeader%NumberOfLayers = headers(65)
    unisysHeader%LayerDivider1 = headers(66)
    unisysHeader%LengthOfRasterLayer = m_16BitsTo32Bits(headers(67), headers(68))
    unisysHeader%RasterOpCode = headers(69)
    unisysHeader%RasterConstant1 = headers(70)
    unisysHeader%RasterConstant2 = headers(71)
    unisysHeader%XOrigin = headers(72)
    unisysHeader%YOrigin = headers(73)
    unisysHeader%XScaleInt = headers(74)
    unisysHeader%XScaleFrac = headers(75)
    unisysHeader%YScaleInt = headers(76)
    unisysHeader%YScaleFrac = headers(77)
    unisysHeader%NumberOfRows = headers(78)
    unisysHeader%PackingDescriptor = headers(79)


    ! Check for valid Source ID

    if (unisysHeader%SourceID /= CFG_Unisys_VALID_SOURCE_ID) then
       write(*,*)  "Radar::readRadarUnisys() ERROR - Source_ID ", unisysHeader%SourceID, &
            " in radar source does not match VALID_SOURCE_ID of ", CFG_Unisys_VALID_SOURCE_ID
       iret = -9
       return
    end if

    ! set up GDS information

    ! The following relates spatial resolution to columns and rows
    ! for national mosaic products:
    !          2 km x  2 km    2560 columns x 1792 rows
    !          4 km x  4 km    1280 columns x  896 rows
    !          8 km x  8 km     640 columns x  448 rows
    !         16 km x 16 km     320 columns x  224 rows
    !         32 km x 32 km     160 columns x  112 rows
    if (unisysHeader%NumberOfColumns == 2560) then
       unisysPixelRes = 2.0
    else if (unisysHeader%NumberOfColumns == 1280) then
       unisysPixelRes = 4.0
    else if (unisysHeader%NumberOfColumns == 640) then
       unisysPixelRes = 8.0
    else if (unisysHeader%NumberOfColumns == 320) then
       unisysPixelRes = 16.0
    else if (unisysHeader%NumberOfColumns == 160) then
       unisysPixelRes = 32.0
    else
       write(*,*) "RadarUnisys Error - Unknown Unisys mosaic type for ", &
            unisysHeader%NumberOfColumns, "data columns"
       iret = -1
       return
    end if

    kgds(:) = -1
    kgds(1) = CFG_Unisys_Igrid
    kgds(2) = unisysHeader%NumberOfColumns
    kgds(3) = unisysHeader%NumberOfRows
    kgds(4) = unisysHeader%CenterLatitude ! LA1 LAT OF ORIGIN (LOWER LEFT)
    kgds(5) = unisysHeader%CenterLongitude! LO1 LON OF ORIGIN (LOWER LEFT)
    kgds(6) = 8                           ! RESOLUTION 
    kgds(7) = unisysHeader%CenterLongitude! LOV - ORIENTATION OF GRID
    kgds(8) = unisysPixelRes * 1000       ! DX - X-DIR INCREMENT
    kgds(9) = unisysPixelRes * 1000       ! DY - Y-DIR INCREMENT
    kgds(10) = 0                          ! PROJECTION CENTER FLAG
    kgds(11) = 64                         ! SCANNING MODE FLAG
    kgds(12) = CFG_Unisys_Lat1
    kgds(13) = CFG_Unisys_Lat2


    !================================================!
    ! Load up the Unisys valueTable (in real world data units)
    !================================================!

    unisys_value_table(1) =  MISSING
    unisys_value_table(2) = unisysHeader%DataLevel2
    unisys_value_table(3) = unisysHeader%DataLevel3
    unisys_value_table(4) = unisysHeader%DataLevel4
    unisys_value_table(5) = unisysHeader%DataLevel5
    unisys_value_table(6) = unisysHeader%DataLevel6
    unisys_value_table(7) = unisysHeader%DataLevel7
    unisys_value_table(8) = unisysHeader%DataLevel8
    unisys_value_table(9) = unisysHeader%DataLevel9
    unisys_value_table(10) = unisysHeader%DataLevel10
    unisys_value_table(11) = unisysHeader%DataLevel11
    unisys_value_table(12) = unisysHeader%DataLevel12
    unisys_value_table(13) = unisysHeader%DataLevel13
    unisys_value_table(14) = unisysHeader%DataLevel14
    unisys_value_table(15) = unisysHeader%DataLevel15
    unisys_value_table(16) = unisysHeader%DataLevel16

    ! Load up the valueTable (in 0-255 range) using scale and bias

    do i = 1, 16
       itable_value = int(((unisys_value_table(i)-CFG_Unisys_BIAS)/CFG_Unisys_SCALE)+0.5)

       if (itable_value < 0) itable_value = 0

       if (itable_value > 255) itable_value = 255

       valueTable(i) = itable_value

    end do

    iret = 0
    return
  end subroutine m_readHeader


!**********************************************************************
! * function:  m_16BitsTo32Bits()
! *
! * Description: convert two one-byte integers to a four-byte integer
! *
! * Inputs:
! *   high - the high two bytes
! *   low  - the low two bytes
! *
! * Output:
! *   Return the combined four-byte integer
! *
! * Notes:
! *   Machine/compiler dependant
! *

  integer(kind=BYTE4) function m_16BitsTo32Bits(high, low)
    IMPLICIT NONE
    integer(kind=BYTE2), intent(in) :: high, low

    integer(kind=BYTE4) :: result

    ! -2_2                        1111111111111110
    ! int(-2, 4)  11111111111111111111111111111110
    call mvbits(int(high, BYTE4), 0, 16, result, 16)
    call mvbits(int(low, BYTE4), 0, 16, result, 0)

    m_16BitsTo32Bits = result
    return
  end function m_16BitsTo32Bits

!**********************************************************************
! * function:  m_8BitsTo16Bits()
! *
! * Description: convert two one-byte integers to a two-byte integer
! *
! * Inputs:
! *   high - the high byte
! *   low  - the low byte
! *
! * Output:
! *   Return the combined two-byte integer
! *
! * Notes:
! *   Machine/compiler dependant
! *

  integer(kind=BYTE2) function m_8BitsTo16Bits(high, low)
    IMPLICIT NONE
    integer(kind=BYTE1), intent(in) :: high, low

    integer(kind=BYTE4) :: result

    ! -2_1                11111110
    ! int(-2, 2)  1111111111111110
    call mvbits(int(high, BYTE4), 0, 8, result, 8)
    call mvbits(int(low, BYTE4), 0, 8, result, 0)

    m_8BitsTo16Bits = result
    return
  end function m_8BitsTo16Bits

!**********************************************************************
! * function:  m_expandRLE() - Expands the RLE data in the given buffer.
! *
! * Output:
! *   true on success, false on failure.
! *

  logical function m_expandRLE(data_buffer, valueTable, nx, ny, imageData)
    IMPLICIT NONE
    integer(kind=BYTE2), intent(in) :: data_buffer(:)
    integer(kind=BYTE2), intent(in) :: valueTable(16)
    integer, intent(in) :: nx, ny
    ! Fortran doesn't have 'unsigned' type, use real instead
    real, intent(inout) :: imageData(nx, ny)

    integer :: bytesInRow     ! Number of bytes of RLE data to make current row
    integer(kind=BYTE2) :: runLen(2)     ! Length of run
    integer(kind=BYTE2) :: colVal(2)     ! Color value from nibble
    integer(kind=BYTE2) :: sVal          ! Raster 2-byte/short data

    integer :: bufPos         ! current position in data_buffer
    integer(kind=BYTE8) :: imgPos         ! Array index of current pixel

    integer :: rleCnt         ! RLE position in a row of the data

    integer :: imageByteCount, rowByteCount, byteCnt, row, col, iret

    integer(kind=BYTE8) :: imageSize

    ! temporary one-dimension expanded radar data
    ! converted to two-deimension data at the end of this function
    integer(kind=BYTE2), allocatable, dimension(:) :: tmpImageData

    !================================================!
    ! Allocate space for temporary one-dimension image
    !================================================!

    imageSize = nx * ny

    allocate(tmpImageData(imageSize), stat = iret)
    if (iret /= 0) then
       write(*,*)  "Radar::m_expandRLE() ERROR when allocate memory of tmpImageData. iret=", iret
       m_expandRLE = .false.
       return
    end if
    tmpImageData = 0

    !================================================!
    ! Convert the Run Length Encoded data into tmpImageData array
    !================================================!

    !Start at the first raster data element
    imageByteCount = 0
    bufPos = 0
    do row = 1, ny

       bufPos = bufPos + 1 ! add one to read 'bytes in this row'

       !================================================!
       ! (1) Get the number of bytes in this row
       !================================================!
       bytesInRow = data_buffer(bufPos)

       ! Start at the first RLE data for current row

       rowByteCount = 0

       ! Get the position in tmpImageData array. Row #1 of Unisys data
       ! is at bottom of image, image data works up from there

       imgPos = (ny - row) * nx

!if (mod(row, 200) == 0) write(*, *)  "Bytes in row ", row,  " is " , bytesInRow, "bufPos = ", bufPos

       !================================================!
       ! (2) Reading 2-byte/Short numbers, 
       !     so we need half as many Short's as bytes
       !================================================!

       do rleCnt = 1, bytesInRow/2

          bufPos = bufPos + 1 ! add one to read next RLE data

          ! Read 2 bytes of RLE data

          runLen = 0
          colVal = 0                             ! runLen(1) colVal(1) runLen(2) colVal(2) 
          sVal = data_buffer(bufPos)             !   rrrr      cccc       rrrr      cccc
          call mvbits(sVal, 12, 4, runLen(1), 0) !   rrrr
          call mvbits(sVal,  8, 4, colVal(1), 0) !             cccc
          call mvbits(sVal,  4, 4, runLen(2), 0) !                        rrrr
          call mvbits(sVal,  0, 4, colVal(2), 0) !                                  cccc

          ! Process 2 bytes of RLE data

          do byteCnt = 1, 2

             rowByteCount = rowByteCount + runLen(byteCnt)
             imageByteCount = imageByteCount + runLen(byteCnt)

             ! Expand RLE data into tmpImageData array,
             ! Use lookup values in valueTable for data values

             do col = 1, runLen(byteCnt)
                imgPos = imgPos + 1
                if (colVal(byteCnt) <= 15) then ! radar 16-level data [0, 15]
                   tmpImageData(imgPos) = valueTable(colVal(byteCnt) + 1) ! from 0-index to 1-index
                else
                   tmpImageData(imgPos) = valueTable(1)
                end if
             end do !  do col = 1, runLen(byteCnt)

          end do !  do byteCnt = 1, 2

       end do !  do rleCnt = 1, bytesInRow/2

       if (rowByteCount - nx /= 0) then
          write(*,*) " Bytes found for RLE expansion: ", rowByteCount, &
               " , difference", rowByteCount - nx
       end if
    end do !  do row = 1, ny


    !================================================!
    ! print something
    !================================================!
    write(*,*) "Total RLE data bytes found for expansion: ", imageByteCount
    write(*,*) "Anticipated total ", imageSize, " for ", nx, &
         " cols by ", ny, " rows"
    write(*,*) "Difference of image size between found and expected: ", (imageByteCount - imageSize)

    !================================================!
    ! convert the temporary one-dimension to two-dimension
    !================================================!
    do row = 1, ny
       rowByteCount = (row-1) * nx
       imageData(:, row) = tmpImageData(rowByteCount+1 : rowByteCount+nx)
    end do

    !================================================!
    ! release the memory of the temporary one-dimension data
    !================================================!
    deallocate(tmpImageData, stat=iret)

    m_expandRLE = .true.
    return
  end function m_expandRLE


!**********************************************************************
! * subroutine:  processRadar() 
! * 
! * Description: responsible for encapsulating the actual processing
! *
! * Output:
! *   Iret = 0 on sucess, others on failure.
! *

  subroutine processRadar(dbzData, kgds, nfiner, radarData, iret)
    use Quicksort
    implicit none

    ! radar data at higher resolution (nfiner)
    integer(kind=BYTE2), intent(inout) :: dbzData(:, :)
    integer, intent(in) :: kgds(:) ! model's resolution
    integer, intent(in) :: nfiner
    type(radar_data_t), target, INTENT(inout) :: radarData
    integer, intent(out) :: iret

    REAL, pointer :: vipPctData(:, :, :), dbzPctlData(:, :, :)

    integer :: nx, ny
    integer(kind=BYTE1), dimension(:, :), allocatable  :: vipData ! values [0, 6]


    !********************************************!
    !* for calculating dBz percentiles          *!
    !*------------------------------------------*!
    !* Hold the original DBz input values       *!
    !* (neg, 5, 15, 20, ..., 75) --16 values    *!
    !********************************************!
    integer, allocatable, dimension(:) :: dbzVals
    !********************************************!
    !* actual number of valid dbzVals           *!
    !*------------------------------------------*!
    !* Hold number of points in model quadrangle*!
    !* Initialized to zero for each (i, j)      *!
    !********************************************!
    integer :: numDbzVals 

    integer :: idxVipPct, pctlIdx

    integer :: i, j, ii, jj, k


    nx = kgds(2)
    ny = kgds(3)



write(*,*) "org pass"
    !================================================!
    ! map radar data to vip levels
    !================================================!
    allocate(vipData(nx*nfiner, ny*nfiner), stat=iret)
    if(iret /= 0) then
       write(*,*) "Radar::processRadar() error: can not allocate memory for  vipData. iret=", iret
       return
    end if
    where(dbzData(:,:) == MISSING_INT .or. dbzData(:,:) >= MAX_DBZ)
       vipData(:,:) = MISSING_INT
    elsewhere
       vipData(:,:) = m_mapDbz2Vip(dbzData(:,:))
    end where


    !================================================!
    ! create output fields
    !================================================!
    ! vipPctData's 3rd dimension is N_VIP_FIELDS

    allocate(radarData%vipPct(nx, ny, N_VIP_FIELDS), stat=iret)
    if(iret /= 0) then
       write(*,*) "Radar::processRadar() error: can not allocate memory for output data. iret=", iret
       return
    end if
    vipPctData => radarData%vipPct
    vipPctData = 0.0

    ! dbzPctlData's 3rd dimension is CFG_NPERCENTILES
    allocate(radarData%dbzPct(nx, ny, CFG_NPERCENTILES), stat=iret)
    if(iret /= 0) then
       write(*,*) "Radar::processRadar() error: can not allocate memory for output data. iret=", iret
       return
    end if
    dbzPctlData => radarData%dbzPct
    dbzPctlData = MISSING

    !================================================!
    ! calculate the VIP percentage fields
    !================================================!
    !
    ! loop through grid points in the model grid and calculate the percentage
    ! of radar point inside the given model grid quadrangle equal to the vIP 
    ! levels 1,2,3,4,5,6 and the percentage of points greater or equal to 1. 

    !
    ! calculate the indices of the model grid quadrangle using the radar
    ! projection.


    ! allocate the possible maximum size of dbzVals (Usually 16 20 25 or 30)
    allocate(dbzVals(nfiner*nfiner), stat=iret)
    if(iret /= 0) then
       write(*,*) "Radar::processRadar() error: can not allocate memory for dbzVals. iret=", iret
       return
    end if

    !================================================!
    ! Loop on model grid (i, j)...
    !================================================!
    do j = 1, ny
    do i = 1, nx

       numDbzVals = 0 ! for each (i, j), reset numDbzVals to zero
       dbzVals = 0

       do_jj: do jj = (j-1)*nfiner+1, j*nfiner
       do_ii: do ii = (i-1)*nfiner+1, i*nfiner

          if(vipData(ii, jj) - MISSING_INT == 0) cycle

          numDbzVals = numDbzVals + 1
          dbzVals(numDbzVals) = dbzData(ii, jj)

	  ! assume that if VIP value is missing, then DBZ value must be missing
 
	  idxVipPct = vipData(ii, jj) ! [0, 6]

	  if(idxVipPct > 0) then
             ! VIP == 1,2,3,4,5,6 fields.   element index: 2, 3, 4, 5, 6, 7
             vipPctData(i, j, idxVipPct+1) = vipPctData(i, j, idxVipPct+1) + 1
             ! VIP >= 1 field.              element index: 1
             vipPctData(i, j, 1) = vipPctData(i, j, 1) + 1 
	  end if

       end do do_ii
       end do do_jj

       !================================================!
       ! take care of VIP percentages
       !================================================!
       do_vip_pct: do k = 1, N_VIP_FIELDS
          if(numDbzVals > CFG_MinVipPoints) then
             vipPctData(i, j, k) = 100.0 * vipPctData(i, j, k) / numDbzVals
             if(vipPctData(i, j, k) <= 0.0)  then
                vipPctData(i, j, k) = MISSING
             end if
          else 
             vipPctData(i, j, k) = MISSING
          end if
       end do do_vip_pct

       !================================================!
       ! take care of dBz percentiles
       !================================================!
       if_dbz_pct: if(numDbzVals > CFG_MinDBzPoints) then
          call quick_sort(dbzVals(1:numDbzVals), numDbzVals)
          do k = 1, CFG_NPERCENTILES
             pctlIdx = int( anint(CFG_PERCENTILES(k)*numDbzVals) / 100.0)
             ! here pctlIdx must be in the range of [0, numDbzVals)

             dbzPctlData(i, j, k) = dbzVals(pctlIdx + 1) ! from 0-index to 1-index
          end do ! do k = 1, CFG_NPERCENTILES
       end if if_dbz_pct

!if(mod(i,100) ==0 .and. mod(j, 100)==0) then
!print *,  i, j, vipPctData(i, j, :), "and",  dbzPctlData(i, j, :), numDbzVals
!end if
!if(numDbzVals > 0) print *, i, j, "vip", vipPctData(i, j, :), "dbz",  dbzPctlData(i, j, :), numDbzVals
    end do ! do i = begin_i, end_i
    end do ! do j = begin_j, end_j

    !================================================!
    ! release memories of all related variables
    !================================================!
    deallocate(vipData, stat=iret)
    deallocate(dbzVals, stat=iret)

    return
  end subroutine processRadar

  
!**********************************************************************
! * function: m_mapDbz2Vip() 
! * 
! * Description: this fuctions maps radar dbz values to VIP levels
! *
! * Output:
! *   iret = 0 on sucess, others on failure.
! *

  elemental integer(kind=BYTE1) function m_mapDbz2Vip(dbz)
    IMPLICIT NONE
    integer(kind=BYTE2), intent(in) :: dbz

    integer :: i
    do i = 1, N_VIP_LEVEL
       if(dbz >= VipMap(i)) then
          m_mapDbz2Vip = N_VIP_LEVEL - i + 1
          return
       end if
    end do

    m_mapDbz2Vip = 0

    return    
  end function m_mapDbz2Vip

end module Radar
