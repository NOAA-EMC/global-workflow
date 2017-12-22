!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_diag                         read ozone diag file
!   prgmmr: hliu           org: np20                date: 2009-04-15
!
! abstract:  This module contains code to process ozone
!            diagnostic files.  The module defines structures
!            to contain information from the ozone
!            diagnostic files and then provides two routines
!            to access contents of the file.
!
! program history log:
!
! contains
!   read_diag_header - read ozone diagnostic file header
!   read_diag_data   - read ozone diagnostic file data
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!------------------------------------------------------------
!

module read_diag

  ! USE:
  use kinds, only: r_single,i_kind

  !--- implicit

  implicit none


  !--- public & private

  private

  public :: diag_header_fix_list
  public :: diag_header_nlev_list
  public :: diag_data_fix_list
  public :: diag_data_nlev_list
  public :: diag_data_extra_list

  public :: read_diag_header
  public :: read_diag_data



  !--- diagnostic file format - header
  
  type diag_header_fix_list
    sequence
    character(len=20) :: isis           ! sat and sensor type
    character(len=10) :: id             ! sat type
    character(len=10) :: obstype	! observation type
    integer(i_kind)   :: jiter          ! outer loop counter
    integer(i_kind)   :: nlevs		! number of levels (layer amounts + total column) per obs
    integer(i_kind)   :: ianldate	! analysis date in YYYYMMDDHH 
    integer(i_kind)   :: iint		! mpi task number
    integer(i_kind)   :: ireal		! # of real elements in the fix part of a data record
    integer(i_kind)   :: iextra		! # of extra elements for each level
  end type diag_header_fix_list

  type diag_header_nlev_list
    sequence
    real(r_single) :: pob		! SBUV/2,omi and gome-2 obs pressure level
    real(r_single) :: grs		! gross error
    real(r_single) :: err		! observation error
    integer(i_kind):: iouse		! use flag
  end type diag_header_nlev_list

  !--- diagnostic file format - data

  integer,parameter :: IREAL_RESERVE  = 3
  
  type diag_data_fix_list
    sequence
    real(r_single) :: lat            ! latitude (deg)
    real(r_single) :: lon            ! longitude (deg)
    real(r_single) :: obstime        ! observation time relative to analysis
  end type diag_data_fix_list

  type diag_data_nlev_list
    sequence
    real(r_single) :: ozobs              ! ozone (obs)
    real(r_single) :: ozone_inv          ! obs-ges
    real(r_single) :: varinv             ! inverse obs error **2
    real(r_single) :: sza                ! solar zenith angle
    real(r_single) :: fovn               ! scan position (field of view)
    real(r_single) :: toqf               ! omi row anomaly index or MLS o3mr precision
  end type diag_data_nlev_list

  type diag_data_extra_list
  sequence
    real(r_single) :: extra              ! extra information
  end type diag_data_extra_list


 contains


  !------------------------------------------------------------
  ! Read a header record of a diagnostic file
  !------------------------------------------------------------

  subroutine read_diag_header( ftin, header_fix, header_nlev )

    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(out) :: header_fix
    type(diag_header_nlev_list),pointer     :: header_nlev(:)

    
    !--- variables
    
    integer,save :: nlevs_last = -1
    integer :: ilev,k
    character(len=10):: id,obstype
    character(len=20):: isis
    integer(i_kind):: jiter,nlevs,ianldate,iint,ireal,iextra
    integer(i_kind),dimension(:),allocatable:: iouse
    real(r_single),dimension(:),allocatable:: pob,grs,err
    
    !--- read header (fix part)

    read(ftin) isis,id,obstype,jiter,nlevs,ianldate,iint,ireal,iextra
    header_fix%isis      = isis
    header_fix%id        = id
    header_fix%obstype   = obstype
    header_fix%jiter     = jiter
    header_fix%nlevs     = nlevs
    header_fix%ianldate  = ianldate
    header_fix%iint      = iint
    header_fix%ireal     = ireal
    header_fix%iextra    = iextra


    print*,'header_fix=', header_fix
    print*,'header_fix%mpi=', header_fix%iint

    !--- check header
    
    if( header_fix%ireal  /= IREAL_RESERVE  ) then

      print *, '### ERROR: UNEXPECTED DATA RECORD FORMAT'
      print *, 'ireal  =', header_fix%ireal  
      stop 99

    endif

    if (header_fix%iextra /= 0) then
       write(6,*)'READ_DIAG_HEADER:  extra diagnostic information available, ',&
            'iextra=',header_fix%iextra
    endif

    !--- allocate if necessary

    if( header_fix%nlevs /= nlevs_last )then
      if( nlevs_last > 0 )then
        deallocate( header_nlev )
      endif
      allocate( header_nlev( header_fix%nlevs ) )
      nlevs_last = header_fix%nlevs
      allocate (pob(header_fix%nlevs))
      allocate (grs(header_fix%nlevs))
      allocate (err(header_fix%nlevs))
      allocate (iouse(header_fix%nlevs))
    endif

    !--- read header (level part)
    
    read(ftin)  pob,grs,err,iouse
    do k=1,header_fix%nlevs
       header_nlev(k)%pob = pob(k)
       header_nlev(k)%grs = grs(k)
       header_nlev(k)%err = err(k)
       header_nlev(k)%iouse = iouse(k)
    end do
    deallocate (pob,grs,err,iouse)
!   print*,'header_nlev%pob=', header_nlev%pob
!   print*,'header_nlev%grs=', header_nlev%grs
!   print*,'header_nlev%err=', header_nlev%err
!   print*,'header_nlev%iouse=', header_nlev%iouse


  end subroutine read_diag_header



  !------------------------------------------------------------
  ! Read a data record of the diagnostic file
  !------------------------------------------------------------

  subroutine read_diag_data( ftin, header_fix, data_fix, data_nlev, data_extra, ntobs, iflag )
  
    !--- interface

    integer                    ,intent(in)  :: ftin
    type(diag_header_fix_list ),intent(in)  :: header_fix
    type(diag_data_fix_list),   pointer     :: data_fix(:)
    type(diag_data_nlev_list)  ,pointer     :: data_nlev(:,:)
    type(diag_data_extra_list) ,pointer     :: data_extra(:,:)
    integer                    ,intent(out) :: iflag
    integer(i_kind)            ,intent(out) :: ntobs
    integer(i_kind)            ,pointer     :: data_mpi(:)
    
    !--- variables

    integer,save :: nlevs_last = -1
    integer,save :: iextra_last = -1
    integer :: iev,iobs,i,j
    real(r_single),allocatable,dimension(:,:)  :: tmp_fix
    real(r_single),allocatable,dimension(:,:,:):: tmp_nlev
    real(r_single),allocatable,dimension(:,:)  :: tmp_extra

    !--- allocate if necessary

    print*, 'nlevs_last, header_fix%nlevs=',nlevs_last,header_fix%nlevs

    read(ftin,IOSTAT=iflag) ntobs
    print*,'ntobs=',ntobs

    if( header_fix%nlevs /= nlevs_last )then
      if( nlevs_last > 0 )then
        print*, 'deallocate array data_nlev, data_mpi and data_fix'
        deallocate( data_nlev )
        deallocate( data_fix )
        deallocate( data_mpi )
      endif
      print*, 'allocate array data_nlev, data_mpi and data_fix'
      allocate( data_mpi( ntobs ) )
      allocate( data_fix( ntobs ) )
      allocate( data_nlev( header_fix%nlevs,ntobs ) )
      allocate( tmp_fix(3,ntobs))
      allocate( tmp_nlev(6,header_fix%nlevs,ntobs))
      nlevs_last = header_fix%nlevs
    endif

    if (header_fix%iextra /= iextra_last) then
       if (iextra_last > 0) then
          deallocate (data_extra)
       endif
       allocate( data_extra(header_fix%iextra,ntobs) )
       allocate(  tmp_extra(header_fix%iextra,ntobs) )
       iextra_last = header_fix%iextra
    endif

    !--- read a record

!   print*, 'iextra=', header_fix%iextra
    if (header_fix%iextra == 0) then
       read(ftin,IOSTAT=iflag) data_mpi, tmp_fix, tmp_nlev
    else
       read(ftin,IOSTAT=iflag) data_mpi, tmp_fix, tmp_nlev, tmp_extra
       do j=1,ntobs
          do i=1,header_fix%iextra
             data_extra(i,j)%extra=tmp_extra(i,j)
          end do
       end do
       deallocate(tmp_extra)
    endif
    do j=1,ntobs
       data_fix(j)%lat     = tmp_fix(1,j)
       data_fix(j)%lon     = tmp_fix(2,j)
       data_fix(j)%obstime = tmp_fix(3,j)
    end do
    deallocate(tmp_fix)
    do j=1,ntobs
       do i=1,header_fix%nlevs
          data_nlev(i,j)%ozobs  = tmp_nlev(1,i,j)
          data_nlev(i,j)%ozone_inv= tmp_nlev(2,i,j)
          data_nlev(i,j)%varinv = tmp_nlev(3,i,j)
          data_nlev(i,j)%sza    = tmp_nlev(4,i,j)
          data_nlev(i,j)%fovn   = tmp_nlev(5,i,j)
          data_nlev(i,j)%toqf   = tmp_nlev(6,i,j)
       end do
    end do
    deallocate(tmp_nlev)

!   do iobs=1,ntobs
!      print*,'iobs,data_mpi,data_fix%lat,data_nlev%ozobs= ',  iobs,data_mpi(iobs),data_fix(iobs)%lat,data_nlev(1,iobs)%ozobs
!   end do

    nlevs_last = -1

  end subroutine read_diag_data


end module read_diag

