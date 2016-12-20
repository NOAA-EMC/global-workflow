! waf_grib2.f90
! grib2 IO
! Yali Mao, IMSG/EMC/NCEP, August 2014
module Grib2
  use Grib_MOD
  use Kinds

  implicit none

  public

  ! for output, grib2 parameters of template 4 & 5
  type pdt45_t
     ! values used to write data to GRIB 2 file
     integer :: npdt   ! number of template 4
     integer :: icat   ! catogory
     integer :: iprm   ! parameter
     integer :: ilev   ! type of level (code table 4.5)
     integer :: stat   ! TYPE OF STATISTICAL PROCESSING
     !
     integer :: ndrt   ! number of template 5
     integer :: drt2   ! Binary scale factor
     integer :: drt3   ! Decimal scale factor
     integer :: drt4   ! Number of bits to hold data
     !
     real :: msng   ! missing data (below surface)
     logical :: bitmap           ! whether to use bitmap for sparse data
  end type pdt45_t

contains

!----------------------------------------------------------------------------
  subroutine readModelGB2(filename, cmodel, pds7, fields, modelData, gfld, kgds, iret)
    implicit none
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: cmodel ! RAP/NAM/GFS
    integer,          intent(in) :: pds7(:)! vertical levels, in hPa if pressure
    type(model_fields3D_t), intent(out) :: fields
    type(model_inputs_t),   intent(out) :: modelData
    type(gribfield), intent(out) :: gfld
    integer, intent(out) :: kgds(:)
    integer, intent(out) :: iret

    integer :: iunit, jret
    integer :: igrid       ! grid number
    integer :: nx, ny, nz
    integer :: k
    integer, allocatable :: level(:)

    ! values of product definition template needed to read a Grib2 file
    !  ( category, parameter)
    type m_pdt_t
       integer :: icat   ! catogory 
       integer :: iprm   ! parameter
    end type m_pdt_t
    type m_all_pdt_t
       !~~~~~~~~~~~~~~~~~~~~~3D~~~~~~~~~~~~~~~~~~~~~~!
       ! RAP NAM GFS
       type(m_pdt_t) :: p   ! pressure
       type(m_pdt_t) :: h   ! height
       type(m_pdt_t) :: pvv ! pressureVerticalVelocity
       type(m_pdt_t) :: cwm ! cloudWaterMixingRatio
       type(m_pdt_t) :: t   ! temperature
       type(m_pdt_t) :: wvm ! simply derived from specificHumidity
       ! RAP NAM
       type(m_pdt_t) :: rwm ! rainWaterMixingRatio
       type(m_pdt_t) :: snm ! snowMixingRatio
       ! RAP
       type(m_pdt_t) :: gpm ! graupelMixingRatio
       type(m_pdt_t) :: icm ! iceMixingRatio = CICE,  conventional difference
       ! NAM & GFS
       type(m_pdt_t) :: rh  ! relativeHumidity
       !~~~~~~~~~~~~~~~~~~~~~2D~~~~~~~~~~~~~~~~~~~~~~!
       ! topography: the same as height
    end type m_all_pdt_t
    type(m_all_pdt_t) :: pdt
    integer :: npdt ! template 4 number
    integer :: ilev ! type of level (code table 4.5), as pds6 in Grib1

    character(len=*), parameter :: myself = 'readModelGB2(): '

    iret = -1
    iunit = 11

    nz = size(pds7)
    allocate(level(nz))

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! open GRIB 1 file for reading
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    call baopenr(iunit, filename, iret)
    if (iret /= 0) then
       write(*, *) myself, 'open file error: ', trim(filename), ' iret=', iret
       return
    end if

    call getHeaderGB2(iunit, kgds, igrid, iret, gfld)
    if (iret /= 0) then
       write(*, *) myself, 'unable to get gfld information. iret=', iret
       return
    end if
    nx = kgds(2)
    ny = kgds(3)

print *, "model kgds=", kgds(1:22)

    ! return if not a proper grid number
    if( cmodel == 'RAP' .and. (igrid /= 252 .and. igrid /= 130)) then
       iret = -1
       write(*,*) "For RAP model, CIP only supports grid 252 and 130"
       return
    else if(cmodel == 'NAM' .and. (igrid /= 180 .and. igrid /= 221) )then
       iret = -1
       write(*,*) "For NAM model, CIP only supports grid 180 and 221"
       return
    end if

    npdt = 0

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! read in 2D fields of modelData
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! topography
    allocate(modelData% top(nx, ny))
    pdt%h = m_pdt_t(3, 5)
    call readGB2(iunit,npdt, pdt%h%icat, pdt%h%iprm, 1, 0, modelData%top, iret)
    if(iret /= 0)  then
       call BACLOSE(iunit, jret)
       return
    end if

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! read in 3D fields
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! fields%p is preset by GFS/NAM, allocated at different time from other fields.
    allocate(fields%p(nx, ny, nz))

    select case(cmodel)
    case("GFS")
       ! ( category, parameter)
       pdt%p	= m_pdt_t(-1, -1)
       pdt%h	= m_pdt_t(3, 5)
       pdt%pvv	= m_pdt_t(2, 8)
       pdt%cwm	= m_pdt_t(1, 22)
       pdt%t	= m_pdt_t(0, 0)
       pdt%wvm	= m_pdt_t(1, 0) ! derived from spfh
       pdt%rwm	= m_pdt_t(-1, -1)
       pdt%snm	= m_pdt_t(-1, -1)
       pdt%gpm	= m_pdt_t(-1, -1)
       pdt%icm	= m_pdt_t(-1, -1)
       pdt%rh	= m_pdt_t(1, 1)
       ! ilev: 3D on pressure leve
       ilev = 100
       ! for pressure field, make sure it's in pascal
       level(:) = pds7(:) * 100
       do k = 1, nz
          fields%p(:, :, k) = pds7(k) * 100.0 ! from millibar to pascal
       end do
    case("RAP")
       ! ( category, parameter)
       pdt%p	= m_pdt_t(3, 0)
       pdt%h	= m_pdt_t(3, 5)
       pdt%pvv	= m_pdt_t(2, 8)
       pdt%cwm	= m_pdt_t(1, 22)
       pdt%t	= m_pdt_t(0, 0)
       pdt%wvm	= m_pdt_t(1, 0) ! derived from spfh
       pdt%rwm	= m_pdt_t(1, 24)
       pdt%snm	= m_pdt_t(1, 25)
       pdt%gpm	= m_pdt_t(32, 32)
       pdt%icm	= m_pdt_t(1, 23)
       pdt%rh	= m_pdt_t(-1, -1)
       ! ilev: 3D on Hybrid Level
       ilev = 105
       level(:) = pds7(:)
    case("NAM")
       ! ( category, parameter)
       pdt%p	= m_pdt_t(-1, -1)
       pdt%h	= m_pdt_t(3, 5)
       pdt%pvv	= m_pdt_t(2, 8)
       pdt%cwm	= m_pdt_t(1, 22)
       pdt%t	= m_pdt_t(0, 0)
       pdt%wvm	= m_pdt_t(1, 0) ! derived from spfh
       pdt%rwm	= m_pdt_t(1, 24)
       pdt%snm	= m_pdt_t(1, 25)
       pdt%gpm	= m_pdt_t(-1, -1)
       pdt%icm	= m_pdt_t(1, 23)
       pdt%rh	= m_pdt_t(1, 1)
       ! ilev: 3D on pressure leve
       ilev = 100
       ! for pressure field, make sure it's in pascal
       level(:) = pds7(:) * 100
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

    if(pdt%h  %icat >= 0) allocate(fields%h(nx, ny, nz))
    if(pdt%pvv%icat >= 0) allocate(fields%pvv(nx, ny, nz))
    if(pdt%cwm%icat >= 0) allocate(fields%cwm(nx, ny, nz))
    if(pdt%t  %icat >= 0) allocate(fields%t(nx, ny, nz))
    if(pdt%wvm%icat >= 0) allocate(fields%wvm(nx, ny, nz))
    if(pdt%rwm%icat >= 0) allocate(fields%rwm(nx, ny, nz))
    if(pdt%snm%icat >= 0) allocate(fields%snm(nx, ny, nz))
    if(pdt%gpm%icat >= 0) allocate(fields%gpm(nx, ny, nz))
    if(pdt%icm%icat >= 0) allocate(fields%icm(nx, ny, nz))
    if(pdt%rh %icat >= 0) allocate(fields%rh(nx, ny, nz))

    do k = 1, nz
       if(pdt%p%icat >= 0) then
          call readGB2(iunit,npdt,pdt%p%icat,pdt%p%iprm,  ilev,level(k),fields%p(:,:,k),  iret)
          if(iret /= 0)  go to 10 ! close iunit and return
       end if
       if(pdt%h%icat >= 0) then
          call readGB2(iunit,npdt,pdt%h%icat,pdt%h%iprm,ilev,level(k),fields%h(:,:,k),  iret)
          if(iret /= 0)  go to 10
       end if
       if(pdt%pvv%icat >= 0) then
          call readGB2(iunit,npdt,pdt%pvv%icat,pdt%pvv%iprm,ilev,level(k),fields%pvv(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       if(pdt%cwm%icat >= 0) then
          call readGB2(iunit,npdt,pdt%cwm%icat,pdt%cwm%iprm,ilev,level(k),fields%cwm(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       if(pdt%t%icat >= 0) then
          call readGB2(iunit,npdt,pdt%t%icat,pdt%t%iprm,ilev,level(k),fields%t(:,:,k),  iret)
          if(iret /= 0)  go to 10
       end if
       ! specific humidity for GFS, to be converted to water vapor mixing ratio
       if(pdt%wvm%icat >= 0) then
          call readGB2(iunit,npdt,pdt%wvm%icat,pdt%wvm%iprm,ilev,level(k),fields%wvm(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       if(pdt%rwm%icat >= 0) then
          call readGB2(iunit,npdt,pdt%rwm%icat,pdt%rwm%iprm,ilev,level(k),fields%rwm(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       if(pdt%snm%icat >= 0) then
          call readGB2(iunit,npdt,pdt%snm%icat,pdt%snm%iprm,ilev,level(k),fields%snm(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       ! was set to zero for NAM
       if(pdt%gpm%icat >= 0) then
          call readGB2(iunit,npdt,pdt%gpm%icat,pdt%gpm%iprm,ilev,level(k),fields%gpm(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       ! ICMR and CICE are the same for RAP
       if(pdt%icm%icat >= 0) then
          call readGB2(iunit,npdt,pdt%icm%icat,pdt%icm%iprm,ilev,level(k),fields%icm(:,:,k),iret)
          if(iret /= 0)  go to 10
       end if
       if(pdt%rh%icat >= 0) then
          call readGB2(iunit,npdt,pdt%rh%icat,pdt%rh%iprm,ilev,level(k),fields%rh(:,:,k), iret)
          if(iret /= 0)  go to 10
       end if
    end do

10    deallocate(level)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! close GRIB 1 file
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    call BACLOSE(iunit, jret)

    return
  end subroutine readModelGB2

!----------------------------------------------------------------------------
  subroutine getHeaderGB2(iunit, kgds, igrid, iret, gfld_out)
    use grib_mod
    use params
    implicit none
    integer, intent(in) :: iunit
    integer, intent(out) :: kgds(:)
    integer, intent(out) :: igrid
    integer, intent(out) :: iret
    type(gribfield), intent(out), optional :: gfld_out

    integer, parameter :: msk1=32000

    integer :: lskip, lgrib		! output of skgb()
    integer :: currlen
    CHARACTER(1), allocatable, dimension(:) :: cgrib
    integer :: lengrib			! output of baread()
    integer :: listsec0(3), listsec1(13)! output of gb_info
    integer :: numfields, numlocal, maxlocal ! output of gb_info 
    logical :: unpack, expand		! output of gf_getfld
    type(gribfield) :: gfld

    integer :: iseek, n

    character(len=*), parameter :: myself = 'Grib2::getHeaderGB2() '

    iseek = 0
    currlen = 0

    do
       call skgb(iunit, iseek, msk1, lskip, lgrib)
       if (lgrib == 0) exit    ! end loop at EOF or problem
       if (lgrib > currlen) then ! allocate cgrib if size is expanded.
          if (allocated(cgrib)) deallocate(cgrib)
          allocate(cgrib(lgrib), stat=iret)
          currlen = lgrib
       endif

       call baread(iunit, lskip, lgrib, lengrib, cgrib)
       if (lgrib /= lengrib) then
          print *,' degrib2: IO Error.'
          call errexit(9)
       endif

       iseek = lskip + lgrib
       ! GRIB MESSAGE starts at lskip+1

       ! Unpack GRIB2 field
       call gb_info(cgrib,lengrib,listsec0,listsec1,numfields,numlocal,maxlocal,iret)
       if (iret /= 0) then
          write(*, *) myself, 'ERROR querying GRIB2 message = ',iret
          stop 10
       endif

       ! only need to find one valid field, any field
       do n = 1, numfields
          call gf_getfld(cgrib, lengrib, n, unpack, expand, gfld, iret)
          if (iret /= 0) then
             write(*,*) myself, 'ERROR extracting field = ', iret
             cycle
          end if
          iseek = -1 ! one valid field is found, no more seeking
          exit
       end do

       if (iseek < 0) then
          call m_pdt2gds(gfld, kgds, igrid)
          if (present(gfld_out)) gfld_out = gfld
          call gf_free(gfld)
          exit
       end if
    end do

    return
  end subroutine getHeaderGB2

!----------------------------------------------------------------------------
  subroutine readGB2(iunit, npdt, icat, iprm, ilev, level, data, iret)
    implicit none
    integer, intent(in) :: iunit! opened file handler
    integer, intent(in) :: npdt	! number of product defination template 4
    integer, intent(in) :: icat	! category  
    integer, intent(in) :: iprm	! parameter number
    integer, intent(in) :: ilev	! type of level (code table 4.5)
    integer, intent(in) :: level! if pressure level, in Pa
    real,  intent(inout) :: data(:,:)
    integer, intent(out) :: iret

    type(gribfield) :: gfld
    integer j,jdisc,jpdtn,jgdtn
    integer,dimension(200) :: jids,jpdt,jgdt
    logical :: unpack

    integer :: nx, ny

    character(len=*), parameter :: myself = 'Grib2::readGB2() '

    iret = -1

    j        = 0          ! search from 0
    jdisc    = 0          ! for met field:0 hydro: 1, land: 2
    jids(:)  = -9999
    !-- set product defination template 4
    jpdtn    = npdt   ! number of product defination template 4
    jpdt(:)  = -9999
    jpdt(1)  = icat   ! category 
    jpdt(2)  = iprm   ! parameter number
    jpdt(10) = ilev   ! type of level (code table 4.5)
    jpdt(12) = level  ! level value
    !-- set grid defination template/section 3
    jgdtn    = -1  
    jgdt(:)  = -9999
    unpack=.true.
    ! Get field from file
    call getgb2(iunit, 0, j, jdisc, jids, jpdtn, jpdt, &
                jgdtn, jgdt, unpack, j, gfld, iret)
    if( iret /= 0) then
       print *, myself, 'iret=',iret, npdt, icat, iprm, ilev, "on level=",level
       return
    endif

    nx = gfld%igdtmpl(8)
    ny = gfld%igdtmpl(9)

    data = reshape(gfld%fld, (/ nx, ny /))
    call gf_free(gfld)

    return
  end subroutine readGB2


!----------------------------------------------------------------------------
  subroutine m_pdt2gds(gfld, kgds, igrid)
    implicit none
    type(gribfield), intent(in) :: gfld
    integer, intent(out) :: kgds(:)
    integer, intent(out) :: igrid ! NCEP predefined GRIB1 grid number; 255 if undefined

    ! prepare for gdt2gds() to get kgds used by gdswiz()
    integer :: igds(5)
    integer :: ideflist(1)
    integer :: iret

    igds(1) = gfld%griddef
    igds(2) = gfld%ngrdpts
    igds(3) = gfld%numoct_opt
    igds(4) = gfld%interp_opt
    igds(5) = gfld%igdtnum
    CALL gdt2gds(igds, gfld%igdtmpl, 0, ideflist, kgds, igrid, iret)

    return
  end subroutine m_pdt2gds

!----------------------------------------------------------------------------
  subroutine writeIcingGB2(filename, gfld, icingData, iret)
    implicit none
    character(*), intent(in) :: filename
    type(gribfield), intent(in) :: gfld    ! a sample input carrying information
    type(icing_output_t), intent(in) :: icingData
    integer, intent(out) :: iret

    integer :: nx, ny, nz
    integer :: iunit, k

    integer, allocatable :: levels(:)

    type(pdt45_t) :: pot_gparms, sld_gparms, sev_gparms

    character(len=*), parameter :: myself = 'Model::writeIcingGB2() '

    write(*,*) "Writing icing to file: ", trim(filename)

    iret = -1
    iunit = 30

    nz = size(icingData%levels)
    allocate(levels(nz))

    ! Use PDT 4.0, different from WAFS icing, which uses PDT 4.15 for mean/max
    pot_gparms = pdt45_t(0, 19, 20,  -1, 0, 40, 0, 2, 7, -0.01, .false.)
    sld_gparms = pdt45_t(0, 19, 23,  -1, 0, 40, 0, 2, 7, -0.01, .false.)
    sev_gparms = pdt45_t(0, 19, 234, -1, 0, 40, 0, 2, 7, -0.01, .false.)
    ! check output vertical levels: flight level or pressure level
    if( icingData%ctype == 'FLT') then
       pot_gparms%ilev = 102 ! altitude above MSL
       sld_gparms%ilev = 102
       sev_gparms%ilev = 102
       ! set the flight levels in meters
       levels(:) = icingData%levels(:) 
    else if( icingData%ctype == 'PRS') then
       pot_gparms%ilev = 100 ! pressure level
       sld_gparms%ilev = 100
       sev_gparms%ilev = 100
       ! set the pressure levels in Pa
       levels(:) = int(icingData%levels(:))
    else
       write(*,*)  myself, icingData%ctype, " level is not supported."
       return
    end if

    ! open file to write. It is BAOPENWT() for Grib1
    call BAOPENW(iunit, trim(filename), iret)
    if (iret /= 0) then
       write(*, *) myself, 'open file error: ', trim(filename), ' iret=', iret
       return
    end if

    ! write out
    nx = gfld%igdtmpl(8)
    ny = gfld%igdtmpl(9)
    do k = 1, nz
       call m_putGB2(iunit,pot_gparms,levels(k),gfld,nx,ny,icingData%probability(:,:,k),iret)
       call m_putGB2(iunit,sld_gparms,levels(k),gfld,nx,ny,icingData%sld(:,:,k),iret)
       call m_putGB2(iunit,sev_gparms,levels(k),gfld,nx,ny,icingData%severity(:,:,k),iret)
    end do

    deallocate(levels)

    call BACLOSE(iunit, iret)

    return
  end subroutine writeIcingGB2

!----------------------------------------------------------------------------
  subroutine m_putGB2(iunit, parms, nlevel, gfld, nx, ny, fld, iret)
    ! basically the same as putgb2, but with flexible template 4 and template 5
    ! writes calculated values for one field at all pressure levels
    implicit none
    integer, intent(in) :: iunit
    type(pdt45_t), intent(in) :: parms    ! grib2 parameters of template 4 & 5
    integer, intent(in) :: nlevel          ! pressure level in Pa, integer
    type(gribfield), intent(in) :: gfld    ! a sample input carrying information
    integer, intent(in) :: nx, ny
    real(4), intent(in) :: fld(nx, ny)     ! the data to be written
    integer, intent(out) :: iret           ! return status code  

    CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:) :: CGRIB
    integer(4) :: lcgrib, lengrib
    integer :: listsec0(2)
    integer :: igds(5)
    real    :: coordlist=0.0
    integer :: ilistopt=0
    ! flexible arrays of template 4, 5
    integer, allocatable :: ipdtmpl(:), idrtmpl(:)
    logical(kind=1), dimension(nx,ny) :: bmap
    integer :: ibmap ! indicator whether to use bitmap

    character(len=*), parameter :: myself = 'm_putGB2(): '

!   ALLOCATE ARRAY FOR GRIB2 FIELD
    lcgrib=gfld%ngrdpts*4
    allocate(cgrib(lcgrib),stat=iret)
    if ( iret.ne.0 ) then
       print *, myself, iret
       iret=2
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CREATE NEW MESSAGE
    listsec0(1)=gfld%discipline
    listsec0(2)=gfld%version
    if ( associated(gfld%idsect) ) then
       call gribcreate(cgrib,lcgrib,listsec0,gfld%idsect,iret)
       if (iret .ne. 0) then
          write(*,*) myself, ' ERROR creating new GRIB2 field = ',iret
       endif
    else
       print *, myself, ' No Section 1 info available. '
       iret=10
       deallocate(cgrib)
       return
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ADD GRID TO GRIB2 MESSAGE (Grid Definition Section 3)
    igds(1)=gfld%griddef    ! Source of grid definition (see Code Table 3.0)
    igds(2)=gfld%ngrdpts    ! Number of grid points in the defined grid.
    igds(3)=gfld%numoct_opt ! Number of octets needed for each additional grid points definition
    igds(4)=gfld%interp_opt ! Interpretation of list for optional points definition (Code Table 3.11)
    igds(5)=gfld%igdtnum    ! Grid Definition Template Number (Code Table3.1)
    if ( associated(gfld%igdtmpl) ) then
       call addgrid(cgrib, lcgrib, igds, gfld%igdtmpl, gfld%igdtlen,&
            ilistopt, gfld%num_opt, iret)
       if (iret.ne.0) then
          write(*,*) myself, ' ERROR adding grid info = ',iret
       endif
    else
       print *, myself, ' No GDT info available. '
       iret=11
       deallocate(cgrib)
       return
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ADD DATA FIELD TO GRIB2 MESSAGE
    ! template 4
    if( parms%npdt == 0 .or. parms%npdt == 7) then
       allocate(ipdtmpl(15))
    else
       allocate(ipdtmpl(18))
       ipdtmpl(16) = parms%stat
       ipdtmpl(17) = 3
       ipdtmpl(18) = 1
    endif
    ipdtmpl(1:15) = gfld%ipdtmpl(1:15)
    ipdtmpl(1)    = parms%icat
    ipdtmpl(2)    = parms%iprm
    ipdtmpl(10)   = parms%ilev
    ipdtmpl(12)   = nlevel
    ! template 5
    if( parms%ndrt == 40) then
       allocate(idrtmpl(7))
    endif
    idrtmpl(1) = 0 ! Any value. Will be overwritten
    idrtmpl(2) = parms%drt2
    idrtmpl(3) = parms%drt3
    idrtmpl(4) = parms%drt4
    idrtmpl(5) = 0
    idrtmpl(6) = 0
    idrtmpl(7) = 255
    ! bitmap
    if (parms%bitmap) then
       bmap = fld /= parms%msng
       ibmap = 0
    else
       bmap = .false.
       ibmap = 255
    end if
    ! call addfield
    call addfield(cgrib, lcgrib, parms%npdt, ipdtmpl, & 
                  size(ipdtmpl), coordlist, gfld%num_coord, &
                  parms%ndrt, idrtmpl, size(idrtmpl), &
                  fld, gfld%ngrdpts, ibmap, bmap, iret)
    if (iret .ne. 0) then
       write(*,*) myself, 'ERROR adding data field = ',iret
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CLOSE GRIB2 MESSAGE AND WRITE TO FILE
    call gribend(cgrib, lcgrib, lengrib, iret)
    call wryte(iunit, lengrib, cgrib)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    deallocate(cgrib)
    deallocate(ipdtmpl)
    deallocate(idrtmpl)
    RETURN
  end subroutine m_putGB2

end module Grib2
