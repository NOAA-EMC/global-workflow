!------------------------------------------------------------------------------
!
! MODULE: Grib1
!
! DESCRIPTION:
!> Read data input and write icing output in Grib 1
!
! REVISION HISTORY:
! Feb 2015 - generated
!
!------------------------------------------------------------------------------

module Grib1
  use Kinds

  IMPLICIT NONE

  public 

contains

  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Read in all model fields
  !
  !> @param[in]  filename  - input model file name
  !> @param[in]  cmodel    - GFS/NAM/RAP
  !> @param[in]  pds7        - model vertical levels
  !> @param[out] fields    - storing 3D model fields
  !> @param[out] modelData - storing 2D model fields
  !> @param[out] kpds      - kpds(200)
  !> @param[out] kgds      - kgds(200)
  !> @param[out] iret      - status (0 is successful)
  !
  ! Notes:
  !   make sure pressure in Pascal for GFS & NAM; RAP pressure is in Pascal
  !------------------------------------------------------------------------------

  subroutine readModelGB1(filename, cmodel, pds7, fields, modelData, kpds, kgds, iret)
    implicit none
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: cmodel ! RAP/NAM/GFS
    integer,          intent(in) :: pds7(:)! For pressure level, in hPa
    type(model_fields3D_t), intent(out) :: fields
    type(model_inputs_t),   intent(out) :: modelData
    integer, intent(out) :: kpds(:)
    integer, intent(out) :: kgds(:)
    integer, intent(out) :: iret
 
    integer :: iunit, jret
    integer :: igrid       ! grid number
    integer :: nx, ny, nxy, nz, k

    type :: pds5_t
       !~~~~~~~~~~~~~~~~~~~~~3D~~~~~~~~~~~~~~~~~~~~~~!
       ! RAP NAM GFS
       integer :: p   ! pressure
       integer :: h   ! height
       integer :: pvv ! pressureVerticalVelocity
       integer :: cwm ! cloudWaterMixingRatio
       integer :: t   ! temperature
       integer :: wvm ! simply derived from specificHumidity
       ! RAP NAM
       integer :: rwm ! rainWaterMixingRatio
       integer :: snm ! snowMixingRatio
       ! RAP
       integer :: gpm ! graupelMixingRatio
       integer :: icm ! iceMixingRatio = CICE,  conventional difference
       ! NAM & GFS
       integer :: rh  ! relativeHumidity
       !~~~~~~~~~~~~~~~~~~~~~2D~~~~~~~~~~~~~~~~~~~~~~!
       ! topography: the same as height
    end type pds5_t
    type(pds5_t) :: pds5
    integer :: pds6

    character(len=*), parameter :: myself = 'Model::readModelGB1(), '


    iret = -1
    iunit = 11

    nz = size(pds7)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! open GRIB 1 file for reading
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    call BAOPENR(iunit, filename, iret)
    if (iret /= 0)   go to 10

    call getHeaderGB1(iunit, kgds, kpds)
    nx = kgds(2)
    ny = kgds(3)
    nxy = nx * ny
    print *, "model kgds=", kgds(1:22)

    ! return if not a proper grid number
    igrid = kpds(3)
    if( cmodel == 'RAP' .and. (igrid /= 252 .and. igrid /= 130)) then
       iret = -1
       write(*,*) "For RAP model, CIP only supports grid 252 and 130"
       return
    else if(cmodel == 'NAM' .and. (igrid /= 180 .and. igrid /= 221) )then
       iret = -1
       write(*,*) "For NAM model, CIP only supports grid 180 and 221"
       return
    end if

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! read in 2D fields of modelData
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! topography
    allocate(modelData% top(nx, ny))
    pds5%h = 7
    call readGB1(iunit,nxy,pds5%h, 1, 0, modelData%top,iret)
    if(iret /= 0)  then
       call BACLOSE(iunit, jret)
       return
    end if

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! read in 3D fields
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! preset by GFS/NAM, allocated fields%p at different time from other fields.
    allocate(fields%p(nx, ny, nz))

    select case(cmodel)
    case("GFS")
       ! pds5:        p  h  vv  cwm  t  spfh rwm  snm  gpm  icm  rh
       pds5 = pds5_t(-1, 7, 39, 153, 11, 51,  -1,  -1,  -1,  -1, 52)
       ! pds6: 3D on pressure leve
       pds6 = 100
       ! for pressure field, make sure it's in pascal
       do k = 1, nz
          fields%p(:, :, k) = pds7(k) * 100.0 ! from millibar to pascal
       end do
    case("RAP")
       ! pds5:       p  h  vv  cwm   t  spfh rwm  snm  gpm  cice  rh
       pds5 = pds5_t(1, 7, 39, 153,  11, 51, 170, 171, 179,  58,  -1)
       ! pds6: 3D on Sigma Level
       pds6 = 109
    case("NAM")
       ! pds5:        p  h  vv  cwm  t   spfh rwm  snm  gpm  icm  rh
       pds5 = pds5_t(-1, 7, 39, 153, 11,  51, 170, 171,  -1,  58, 52)
       ! pds6: 3D on pressure level
       pds6 = 100
       ! for pressure field, make sure it's in pascal
       do k = 1, nz
          fields%p(:, :, k) = pds7(k) * 100.0 ! from millibar to pascal
       end do
       ! gpm is set to zero for NAM
       allocate(fields%gpm(nx, ny, nz))
       fields%gpm = 0.0
    case default
       write(*,*) myself, "error -- Model ", cmodel, " is not supported."
       return
    end select

    if(pds5%h > 0) allocate(fields%h(nx, ny, nz))
    if(pds5%pvv > 0) allocate(fields%pvv(nx, ny, nz))
    if(pds5%cwm > 0) allocate(fields%cwm(nx, ny, nz))
    if(pds5%t > 0) allocate(fields%t(nx, ny, nz))
    if(pds5%wvm > 0) allocate(fields%wvm(nx, ny, nz))
    if(pds5%rwm > 0) allocate(fields%rwm(nx, ny, nz))
    if(pds5%snm > 0) allocate(fields%snm(nx, ny, nz))
    if(pds5%gpm > 0) allocate(fields%gpm(nx, ny, nz))
    if(pds5%icm > 0) allocate(fields%icm(nx, ny, nz))
    if(pds5%rh > 0) allocate(fields%rh(nx, ny, nz))

    do k = 1, nz
       if(pds5%p > 0) then
          call readGB1(iunit,nxy,pds5%p,  pds6,pds7(k),fields%p(:,:,k),  iret)
          if(iret /= 0)  go to 10 ! close iunit and return
       end if
       if(pds5%h > 0) then
          call readGB1(iunit,nxy,pds5%h,  pds6,pds7(k),fields%h(:,:,k),  iret)
          if(iret /= 0)  go to 10
       end if
       if(pds5%pvv > 0) then
          call readGB1(iunit,nxy,pds5%pvv,pds6,pds7(k),fields%pvv(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       if(pds5%cwm > 0) then
          call readGB1(iunit,nxy,pds5%cwm,pds6,pds7(k),fields%cwm(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       if(pds5%t > 0) then
          call readGB1(iunit,nxy,pds5%t,  pds6,pds7(k),fields%t(:,:,k),  iret)
          if(iret /= 0)  go to 10
       end if
       ! specific humidity for GFS, to be converted to water vapor mixing ratio
       if(pds5%wvm > 0) then
          call readGB1(iunit,nxy,pds5%wvm,pds6,pds7(k),fields%wvm(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       if(pds5%rwm > 0) then
          call readGB1(iunit,nxy,pds5%rwm,pds6,pds7(k),fields%rwm(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       if(pds5%snm > 0) then
          call readGB1(iunit,nxy,pds5%snm,pds6,pds7(k),fields%snm(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       ! was set to zero for NAM
       if(pds5%gpm > 0) then
          call readGB1(iunit,nxy,pds5%gpm,pds6,pds7(k),fields%gpm(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       ! ICMR and CICE are the same for RAP
       if(pds5%icm > 0) then
          call readGB1(iunit,nxy,pds5%icm,pds6,pds7(k),fields%icm(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       if(pds5%rh > 0) then
          call readGB1(iunit,nxy,pds5%rh, pds6,pds7(k),fields%rh(:,:,k), iret)
          if(iret /= 0)  go to 10
       end if
    end do

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! close GRIB 1 file
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
10  call BACLOSE(iunit, jret)
    return
  end subroutine readModelGB1


  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> Almost the same function as getgbh, but as a simpler version
  !
  !> @param[in]  iunit		    - opened file handler
  !> @param[out] kgds		    - kgds(200)
  !> @param[out] kpds_out, optional - kpds_out(200)
  !
  !------------------------------------------------------------------------------

  subroutine getHeaderGB1(iunit, kgds, kpds_out)
    IMPLICIT NONE
    integer, intent(in) :: iunit
    integer, intent(out) :: kgds(:)
    integer, intent(out), optional :: kpds_out(:)

    integer :: kg, kf, k, iret
    integer, dimension(200) :: jpds, jgds, kpds

    jpds(:) = -1
    jgds(:) = -1

    call getgbh(iunit, 0, 0, jpds, jgds, kg, kf, k, kpds, kgds, iret)

    if(present(kpds_out)) kpds_out = kpds

    return
  end subroutine getHeaderGB1


  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> use GETBG from W3LIB library to read 2D data in GRIB1
  !
  !> @param[in]  iunit      - opened file handler
  !> @param[in]  nxy       - model 2D dimension
  !> @param[in]  pds5      - jpds(5)
  !> @param[in]  pds6      - jpds(6)
  !> @param[in]  pds7      - jpds(7)
  !> @param[inout] data    - storing data read in
  !> @param[out]   iret    - status (0 is successful)
  !
  ! Notes:
  !  for some field, jpds(14), jpds(15) have different specification
  !------------------------------------------------------------------------------

  subroutine readGB1(iunit, nxy, pds5, pds6, pds7, data, iret, pds14, pds15)
    IMPLICIT NONE
    integer, intent(in) :: iunit
    integer, intent(in) :: nxy
    integer, intent(in) :: pds5, pds6, pds7
    real,    intent(inout) :: data(:,:)
    integer, intent(out) :: iret
    integer, intent(in), optional :: pds14, pds15

    ! More arguments for GETGB
    integer, dimension(200) :: jpds, jgds, kpds, kgds
    integer :: kf, k
    logical :: bitmap(nxy)

    character(len=*), parameter :: myself = 'Model::readGB1() '

    jpds(:) = -1
    jgds(:) = -1

    jpds(5) = pds5
    jpds(6) = pds6
    jpds(7) = pds7
    if (present(pds14)) jpds(14) = pds14
    if (present(pds15)) jpds(15) = pds15

    bitmap = .false.

    call GETGB(iunit, 0, nxy, 0, jpds, jgds, kf, k, kpds, kgds, bitmap, data, iret)

    if (iret /= 0) then
      write(*, *) myself, "can not read field", jpds(5:7), " iret=", iret
      return
    end if

    return
  end subroutine readGB1


  !------------------------------------------------------------------------------
  ! DESCRIPTION:
  !> write out grib 1
  !
  !> @param[in]  filename  - output file name
  !> @param[in]  kkpds     - PDS
  !> @param[in]  kgds      - GDS
  !> @param[in]  nz        - model vertical levels
  !> @param[in]  icingData - type of icing_output_t, including all icing products
  !> @param[out] iret      - status (0 is successful)
  !
  !------------------------------------------------------------------------------
  subroutine writeIcingGB1(filename, kkpds, kgds, icingData, iret)
    IMPLICIT NONE
    character(*), intent(in) :: filename
    integer, intent(in) :: kkpds(:)
    integer, intent(in) :: kgds(:)
    type(icing_output_t), intent(in) :: icingData
    integer, intent(out) :: iret

    integer :: kpds(200) ! modified PDS
    integer :: nxy
    integer :: iunit, z
 
    integer :: nz
    integer, allocatable :: levels(:)

    logical, allocatable :: bitmap(:)

    character(len=*), parameter :: myself = 'Model::writeIcingGB1() '


    write(*,*) "Writing icing to file: ", trim(filename)

    iret = -1
    iunit = 30

    nz = size(icingData%levels)
    allocate(levels(nz))

    nxy = kgds(2) * kgds(3)
    allocate(bitmap(nxy))

    kpds(:) = kkpds(:)

    ! check output vertical levels: flight level or pressure level
    if( icingData%ctype == 'FLT') then
       kpds(6)  = 103 ! altitude above MSL
       kpds(22) = 2   ! UNITS DECIMAL SCALE FACTOR
       ! set the flight levels in meters
       levels(:) = icingData%levels(:) 
    else if( icingData%ctype == 'PRS') then
       kpds(6) = 100 ! pressure level
       kpds(22) = 2  ! UNITS DECIMAL SCALE FACTOR
       ! set the pressure levels in hPa
       levels(:) = int(icingData%levels(:) / 100)
    else
       write(*,*)  myself, icingData%ctype, " level is not supported."
       return
    end if

    ! open file to write
    call BAOPENWT(iunit, trim(filename), iret)
    if (iret /= 0) then
       write(*, *) myself, 'open file error: ', trim(filename), ' iret=', iret
       return
    end if

    ! write out
    do z = 1, nz
       kpds(7) = levels(z)
       ! probability
       kpds(5)  = 168
       kpds(19) = 140 ! grib table version
       call PUTGB(iunit, nxy, kpds, kgds, bitmap, icingData%probability(:, :, z), iret)
       ! SLD
       kpds(5)  = 236
       kpds(19) = 2
       call PUTGB(iunit, nxy, kpds, kgds, bitmap, icingData%sld(:, :, z), iret)
       ! severity
       kpds(5)  = 175
       kpds(19) = 129
       call PUTGB(iunit, nxy, kpds, kgds, bitmap, icingData%severity(:, :, z), iret)
    end do

    deallocate(levels)
    deallocate(bitmap)

    call BACLOSE(iunit, iret)
    return
  end subroutine writeIcingGB1

end module Grib1
