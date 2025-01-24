!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_raddiag                       read rad diag file
!   prgmmr: tahara           org: np20                date: 2003-01-01
!
! abstract:  This module contains code to process radiance
!            diagnostic files.  The module defines structures
!            to contain information from the radiance
!            diagnostic files and then provides two routines
!            to access contents of the file.
!
! program history log:
!   2005-07-22 treadon - add this doc block
!   2010-10-05 treadon - refactor code to GSI standard
!   2010-10-08 zhu     - use data_tmp to handle various npred values
!   2011-02-22 kleist  - changes related to memory allocate/deallocate
!   2011-04-08 li      - add tref, dtw, dtc to diag_data_fix_list, add tb_tz to diag_data_chan_list
!                      - correspondingly, change ireal_radiag (26 -> 30) and ipchan_radiag (7 -> 8)
!   2011-07-24 safford - make structure size for reading data_fix data version dependent 
!   2013-11-21 todling - revisit how versions are set (add set/get_radiag)
!   2014-01-27 todling - add ob sensitivity index
!   2016-11-12 shlyaeva - add H(x) jacobian for EnKF
!   2017-07-13 mccarty - incorporate hooks for nc4/binary diag reading
!
! contains
!   read_radiag_header - read radiance diagnostic file header
!   read_radiag_data   - read radiance diagnostic file data
!   set_netcdf_read    - call set_netcdf_read(.true.) to use nc4 hooks, otherwise read file as 
!                           traditional binary format
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

module read_diag

  use kinds, only:  i_kind,r_single,r_kind
  use sparsearr, only: sparr, sparr2, readarray, assignment(=), delete
  use nc_diag_read_mod, only: nc_diag_read_get_var,  nc_diag_read_get_global_attr
  use nc_diag_read_mod, only: nc_diag_read_get_dim
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_close
  implicit none

! Declare public and private
  private

  public :: diag_header_fix_list
  public :: diag_header_chan_list
  public :: diag_data_name_list
  public :: diag_data_fix_list
  public :: diag_data_chan_list
  public :: diag_data_extra_list
  public :: open_radiag
  public :: close_radiag
  public :: read_radiag_header
  public :: read_radiag_data
  public :: iversion_radiag
  public :: iversion_radiag_1
  public :: iversion_radiag_2
  public :: iversion_radiag_3
  public :: iversion_radiag_4
  public :: iversion_radiag_5
  public :: ireal_old_radiag
  public :: set_netcdf_read
! public :: iversion_radiag
! public :: iversion_radiag_1
! public :: iversion_radiag_2
! public :: iversion_radiag_3
! public :: iversion_radiag_4
  public :: ireal_radiag
  public :: ipchan_radiag
  public :: set_radiag
  public :: get_radiag

  interface set_radiag
         module procedure set_radiag_int_ ! internal procedure for integers
  end interface
  interface get_radiag
         module procedure get_radiag_int_ ! internal procedure for integers
  end interface

  integer(i_kind),parameter :: ireal_radiag  = 30   ! number of real entries per spot in radiance diagnostic file
  integer(i_kind),parameter :: ireal_old_radiag  = 26   ! number of real entries per spot in versions older than iversion_radiag_2
  integer(i_kind),parameter :: ipchan_radiag = 8    ! number of entries per channel per spot in radiance diagnostic file

! Declare structures for radiance diagnostic file information
  type diag_header_fix_list
     character(len=20) :: isis           ! sat and sensor type
     character(len=11) :: id             ! sat type
     character(len=10) :: obstype        ! observation type
     integer(i_kind) :: jiter            ! outer loop counter
     integer(i_kind) :: nchan            ! number of channels in the sensor
     integer(i_kind) :: npred            ! number of updating bias correction predictors
     integer(i_kind) :: idate            ! time (yyyymmddhh)
     integer(i_kind) :: ireal            ! # of real elements in the fix part of a data record
     integer(i_kind) :: ipchan           ! # of elements for each channel except for bias correction terms
     integer(i_kind) :: iextra           ! # of extra elements for each channel
     integer(i_kind) :: jextra           ! # of extra elements
     integer(i_kind) :: idiag            ! first dimension of diag_data_chan
     integer(i_kind) :: angord           ! order of polynomial for adp_anglebc option
     integer(i_kind) :: iversion         ! radiance diagnostic file version number
     integer(i_kind) :: inewpc           ! indicator of newpc4pred (1 on, 0 off)
     integer(i_kind) :: ijacob        ! indicates whether jacobian included (1 yes, 0 no)
     integer(i_kind) :: isens            ! sensitivity index
  end type diag_header_fix_list

  type diag_data_name_list
     character(len=10),dimension(ireal_radiag) :: fix
     character(len=10),dimension(:),allocatable :: chn
  end type diag_data_name_list
  
  type diag_header_chan_list
     real(r_single) :: freq              ! frequency (Hz)
     real(r_single) :: polar             ! polarization
     real(r_single) :: wave              ! wave number (cm^-1)
     real(r_single) :: varch             ! error variance (or SD error?)
     real(r_single) :: tlapmean          ! mean lapse rate
     integer(i_kind):: iuse              ! use flag
     integer(i_kind):: nuchan            ! sensor relative channel number
     integer(i_kind):: iochan            ! satinfo relative channel number
  end type diag_header_chan_list

  type diag_data_fix_list
     real(r_single) :: lat               ! latitude (deg)
     real(r_single) :: lon               ! longitude (deg)
     real(r_single) :: zsges             ! guess elevation at obs location (m)
     real(r_single) :: obstime           ! observation time relative to analysis
     real(r_single) :: senscn_pos        ! sensor scan position (integer(i_kind))
     real(r_single) :: satzen_ang        ! satellite zenith angle (deg)
     real(r_single) :: satazm_ang        ! satellite azimuth angle (deg)
     real(r_single) :: solzen_ang        ! solar zenith angle (deg)
     real(r_single) :: solazm_ang        ! solar azimumth angle (deg)
     real(r_single) :: sungln_ang        ! sun glint angle (deg)
     real(r_single) :: water_frac        ! fractional coverage by water
     real(r_single) :: land_frac         ! fractional coverage by land
     real(r_single) :: ice_frac          ! fractional coverage by ice
     real(r_single) :: snow_frac         ! fractional coverage by snow
     real(r_single) :: water_temp        ! surface temperature over water (K)
     real(r_single) :: land_temp         ! surface temperature over land (K)
     real(r_single) :: ice_temp          ! surface temperature over ice (K)
     real(r_single) :: snow_temp         ! surface temperature over snow (K)
     real(r_single) :: soil_temp         ! soil temperature (K)
     real(r_single) :: soil_mois         ! soil moisture 
     real(r_single) :: land_type         ! land type (integer(i_kind))
     real(r_single) :: veg_frac          ! vegetation fraction
     real(r_single) :: snow_depth        ! snow depth
     real(r_single) :: sfc_wndspd        ! surface wind speed
     real(r_single) :: qcdiag1           ! ir=cloud fraction, mw=cloud liquid water
     real(r_single) :: qcdiag2           ! ir=cloud top pressure, mw=total column water
     real(r_single) :: tref              ! reference temperature (Tr) in NSST
     real(r_single) :: dtw               ! dt_warm at zob
     real(r_single) :: dtc               ! dt_cool at zob
     real(r_single) :: tz_tr             ! d(Tz)/d(Tr)
  end type diag_data_fix_list

  type diag_data_chan_list
     real(r_single) :: tbobs              ! Tb (obs) (K)
     real(r_single) :: omgbc              ! Tb_(obs) - Tb_(simulated w/ bc)  (K)
     real(r_single) :: omgnbc             ! Tb_(obs) - Tb_(simulated_w/o bc) (K)
     type(sparr)    :: dhx_dx             ! profile of dH(x) / dx
     real(r_single) :: sprd               ! ensemble spread
     real(r_single) :: errinv             ! inverse error (K**(-1))
     real(r_single) :: qcmark             ! quality control mark
     real(r_single) :: emiss              ! surface emissivity
     real(r_single) :: tlap               ! temperature lapse rate
     real(r_single) :: tb_tz              ! d(Tb)/d(Tz)
     real(r_single) :: bicons             ! constant bias correction term
     real(r_single) :: biang              ! scan angle bias correction term
     real(r_single) :: biclw              ! CLW bias correction term
     real(r_single) :: bilap2             ! square lapse rate bias correction term
     real(r_single) :: bilap              ! lapse rate bias correction term
     real(r_single) :: bicos              ! node*cos(lat) bias correction term
     real(r_single) :: bisin              ! sin(lat) bias correction term
     real(r_single) :: biemis             ! emissivity sensitivity bias correction term
     real(r_single),dimension(:),allocatable :: bifix          ! angle dependent bias
     real(r_single) :: bisst              ! SST bias correction term
  end type diag_data_chan_list

  type diag_data_extra_list
     real(r_single) :: extra              ! extra information
  end type diag_data_extra_list

  integer(i_kind),save     :: iversion_radiag             ! Current version (see set routine)
  integer(i_kind),parameter:: iversion_radiag_1 = 11104   ! Version when bias-correction entries were modified 
  integer(i_kind),parameter:: iversion_radiag_2 = 13784   ! Version when NSST entries were added 
  integer(i_kind),parameter:: iversion_radiag_3 = 19180   ! Version when SSMIS added
  integer(i_kind),parameter:: iversion_radiag_4 = 30303   ! Version when emissivity predictor added
  integer(i_kind),parameter:: iversion_radiag_5 = 40000   ! Version when ensemble spread (and optional jacobian) added

  real(r_single),parameter::  rmiss_radiag    = -9.9e11_r_single

  logical,save            ::  netcdf = .false. 

  type ncdiag_status
     logical :: nc_read
     integer(i_kind) :: cur_ob_idx
     integer(i_kind) :: num_records
     type(diag_data_fix_list),  allocatable  :: all_data_fix(:)
     type(diag_data_chan_list), allocatable  :: all_data_chan(:,:)
     type(diag_data_extra_list), allocatable :: all_data_extra(:,:,:)
  end type ncdiag_status

  integer(i_kind), parameter :: MAX_OPEN_NCDIAG = 2
  integer(i_kind), save :: nopen_ncdiag = 0
  integer(i_kind), dimension(MAX_OPEN_NCDIAG), save     :: ncdiag_open_id = (/-1, -1/)
  type(ncdiag_status), dimension(MAX_OPEN_NCDIAG), save :: ncdiag_open_status
      
contains

subroutine set_radiag_int_ (what,iv,ier)
character(len=*),intent(in) :: what
integer(i_kind),intent(in) :: iv
integer(i_kind),intent(out):: ier
ier=-1
if(trim(what)=='version') then
  iversion_radiag = iv
  ier=0
endif
end subroutine set_radiag_int_

subroutine get_radiag_int_ (what,iv,ier)
character(len=*),intent(in) :: what
integer(i_kind),intent(out):: iv
integer(i_kind),intent(out):: ier
ier=-1
if(trim(what)=='version') then
  iv = iversion_radiag
  ier=0
endif
end subroutine get_radiag_int_

subroutine set_netcdf_read(use_netcdf)
!                .      .    .                                       .
! subprogram:    read_diag_header_bin             read rad diag header
!   prgmmr: mccarty           org: gmao                date: 2015-08-06
!
! abstract:  This routine sets the routines to read from a netcdf file.
!            The default currently is to read binary files
!
! program history log:
!   2015-08-06 mccarty - created routine 
!
! input argument list:
!   use_netcdf    - logical .true. tells routine to read netcdf diag 
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  logical,intent(in)                     :: use_netcdf

  netcdf = use_netcdf
end subroutine set_netcdf_read


subroutine open_radiag(filename, ftin, istatus)
   character*500,   intent(in) :: filename
   integer(i_kind), intent(inout) :: ftin
   integer(i_kind), intent(out):: istatus

   integer(i_kind) :: i

   istatus = -999
   if (netcdf) then
      if (nopen_ncdiag >= MAX_OPEN_NCDIAG) then
         write(6,*) 'OPEN_RADIAG:  ***ERROR*** Cannot open more than ', &
                    MAX_OPEN_NCDIAG, ' netcdf diag files.'
         call stop2(456)
      endif
      call nc_diag_read_init(filename,ftin)
      istatus=0
      do i = 1, MAX_OPEN_NCDIAG
        if (ncdiag_open_id(i) < 0) then
           ncdiag_open_id(i) = ftin
           ncdiag_open_status(i)%nc_read = .false.
           ncdiag_open_status(i)%cur_ob_idx = -9999
           ncdiag_open_status(i)%num_records = -9999
           if (allocated(ncdiag_open_status(i)%all_data_fix)) then
              deallocate(ncdiag_open_status(i)%all_data_fix)
           endif
           if (allocated(ncdiag_open_status(i)%all_data_chan)) then
              deallocate(ncdiag_open_status(i)%all_data_chan)
           endif
           if (allocated(ncdiag_open_status(i)%all_data_extra)) then
              deallocate(ncdiag_open_status(i)%all_data_extra)
           endif
           nopen_ncdiag = nopen_ncdiag + 1
           exit
        endif
      enddo
   else
      open(ftin,form="unformatted",file=filename,iostat=istatus)
      rewind(ftin)
   endif

end subroutine open_radiag

subroutine close_radiag(filename, ftin)
   character*500,   intent(in) :: filename
   integer(i_kind), intent(inout) :: ftin

   integer(i_kind) :: id

   if (netcdf) then
      id = find_ncdiag_id(ftin)
      if (id < 0) then
         write(6,*) 'CLOSE_RADIAG:  ***ERROR*** ncdiag file ', filename,   &
                    ' was not opened'
         call stop2(456)
      endif
      call nc_diag_read_close(filename)
      ncdiag_open_id(id) = -1
      ncdiag_open_status(id)%nc_read = .false.
      ncdiag_open_status(id)%cur_ob_idx = -9999
      ncdiag_open_status(id)%num_records = -9999
      if (allocated(ncdiag_open_status(id)%all_data_fix)) then
         deallocate(ncdiag_open_status(id)%all_data_fix)
      endif
      if (allocated(ncdiag_open_status(id)%all_data_chan)) then
         deallocate(ncdiag_open_status(id)%all_data_chan)
      endif
      if (allocated(ncdiag_open_status(id)%all_data_extra)) then
         deallocate(ncdiag_open_status(id)%all_data_extra)
      endif
      nopen_ncdiag = nopen_ncdiag - 1
   else
      close(ftin)
   endif

end subroutine close_radiag

subroutine read_radiag_header(ftin,npred_radiag,retrieval,header_fix,header_chan,data_name,iflag,lverbose)
!                .      .    .                                       .
! subprogram:    read_diag_header_bin             read rad diag header
!   prgmmr: mccarty           org: gmao                date: 2015-08-06
!
! abstract:  This routine reads the header record from a radiance
!            diagnostic file
!
! program history log:
!   2015-08-06 mccarty - created routine w/ fork for ncdiag or binary
!
! input argument list:
!   ftin          - unit number connected to diagnostic file 
!   npred_radiag  - number of bias correction terms
!   retrieval     - .true. if sst retrieval
!
! output argument list:
!   header_fix    - header information structure
!   header_chan   - channel information structure
!   data_name     - diag file data names
!   iflag         - error code
!   lverbose      - optional flag to turn off default output to standard out 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

! Declare passed arguments
  integer(i_kind),intent(in)             :: ftin
  integer(i_kind),intent(in)             :: npred_radiag
  logical,intent(in)                     :: retrieval
  type(diag_header_fix_list ),intent(out):: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  type(diag_data_name_list)              :: data_name
  integer(i_kind),intent(out)            :: iflag
  logical,optional,intent(in)            :: lverbose

  iflag = 0
  if (netcdf) then
     call read_radiag_header_nc(ftin,header_fix,header_chan,iflag)
  else
     call read_radiag_header_bin(ftin,npred_radiag,retrieval,header_fix,header_chan,data_name,iflag,lverbose)
  endif

end subroutine read_radiag_header

subroutine read_radiag_header_nc(ftin,header_fix,header_chan,iflag)
!                .      .    .                                       .
! subprogram:    read_diag_header_nc              read rad diag header
!   prgmmr: mccarty           org: gmao                date: 2003-01-01
!
! abstract:  This routine reads the header record from a radiance
!            diagnostic file
!
! program history log:
!   2015-08-06 mccarty - Created routine for ncdiag header reading
!
! input argument list:
!   ftin          - unit number connected to diagnostic file 
!
! output argument list:
!   header_fix    - header information structure
!   header_chan   - channel information structure
!   iflag         - error code
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
! Declare passed arguments
  integer(i_kind),intent(in)             :: ftin
  type(diag_header_fix_list ),intent(out):: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  integer(i_kind),intent(out)            :: iflag

! local variables
  integer(i_kind)                        :: nchan_dim
  real(r_kind),allocatable,dimension(:)               :: r_var_stor
  integer(i_kind),allocatable,dimension(:)            :: i_var_stor
  character(20)                          :: isis
  character(11)                          :: id
  character(10)                          :: obstype
!  integer(i_kind),dimension(:),allocatable           :: jiter, nchan_diag, npred, idate, &
  integer(i_kind)                        :: jiter, nchan_diag, npred, idate, &
                                            ireal, ipchan, iextra, jextra,   &
                                            idiag, angord, iversion, inewpc, &
                                            isens, ijacob

  iflag = 0
!  allocate(nchan_diag(1) )
  nchan_dim = nc_diag_read_get_dim(ftin,'nchans')
  header_fix%nchan = nchan_dim
  
  call nc_diag_read_get_global_attr(ftin, "Number_of_channels", nchan_diag)

  if (nchan_dim /= nchan_diag) then
     write(*,*)'ERROR: Number of channels from dimension do not match those from header, aborting.'
     call stop2(321)
  endif  

  call nc_diag_read_get_global_attr(ftin, "Satellite_Sensor", isis)      ; header_fix%isis = isis
  call nc_diag_read_get_global_attr(ftin, "Satellite", id)               ; header_fix%id = id
  call nc_diag_read_get_global_attr(ftin, "Observation_type", obstype)   ; header_fix%obstype = obstype
  call nc_diag_read_get_global_attr(ftin, "Outer_Loop_Iteration", jiter) ; header_fix%jiter = jiter
  call nc_diag_read_get_global_attr(ftin, "Number_of_Predictors", npred) ; header_fix%npred = npred
  call nc_diag_read_get_global_attr(ftin, "date_time", idate)            ; header_fix%idate = idate
  call nc_diag_read_get_global_attr(ftin, "ireal_radiag", ireal)         ; header_fix%ireal = ireal
  call nc_diag_read_get_global_attr(ftin, "ipchan_radiag", ipchan)       ; header_fix%ipchan = ipchan
  call nc_diag_read_get_global_attr(ftin, "iextra", iextra)              ; header_fix%iextra = iextra
  call nc_diag_read_get_global_attr(ftin, "jextra", jextra)              ; header_fix%jextra = jextra
  call nc_diag_read_get_global_attr(ftin, "idiag", idiag)                ; header_fix%idiag = idiag
  call nc_diag_read_get_global_attr(ftin, "angord", angord)              ; header_fix%angord = angord
  call nc_diag_read_get_global_attr(ftin, "iversion_radiag", iversion)   ; header_fix%iversion = iversion
  call nc_diag_read_get_global_attr(ftin, "New_pc4pred", inewpc)         ; header_fix%inewpc = inewpc
  call nc_diag_read_get_global_attr(ftin, "ioff0", isens)                ; header_fix%isens = isens
  call nc_diag_read_get_global_attr(ftin, "ijacob", ijacob)              ; header_fix%ijacob = ijacob


  if (allocated(header_chan)) deallocate(header_chan)
  allocate(header_chan(nchan_dim)   )

  if (allocated(r_var_stor))  deallocate(r_var_stor)
  if (allocated(i_var_stor))  deallocate(i_var_stor)
  allocate(r_var_stor(nchan_dim), &
           i_var_stor(nchan_dim)  )
!  call nc_diag_read_get_var('Var', var_stor)
  call nc_diag_read_get_var(ftin, 'frequency',r_var_stor)      ; header_chan%freq     = r_var_stor
  call nc_diag_read_get_var(ftin, 'polarization',i_var_stor)   ; header_chan%polar    = i_var_stor
  call nc_diag_read_get_var(ftin, 'wavenumber',r_var_stor)     ; header_chan%wave     = r_var_stor
  call nc_diag_read_get_var(ftin, 'error_variance',r_var_stor) ; header_chan%varch    = r_var_stor
  call nc_diag_read_get_var(ftin, 'mean_lapse_rate',r_var_stor); header_chan%tlapmean = r_var_stor
  call nc_diag_read_get_var(ftin, 'use_flag',i_var_stor)       ; header_chan%iuse     = i_var_stor
  call nc_diag_read_get_var(ftin, 'sensor_chan',i_var_stor)    ; header_chan%nuchan   = i_var_stor
  call nc_diag_read_get_var(ftin, 'satinfo_chan',i_var_stor)   ; header_chan%iochan   = i_var_stor


end subroutine read_radiag_header_nc

subroutine read_radiag_header_bin(ftin,npred_radiag,retrieval,header_fix,header_chan,data_name,iflag,lverbose)
!                .      .    .                                       .
! subprogram:    read_diag_header_bin             read rad diag header
!   prgmmr: tahara           org: np20                date: 2003-01-01
!
! abstract:  This routine reads the header record from a radiance
!            diagnostic file
!
! program history log:
!   2010-10-05 treadon - add this doc block
!   2011-02-22 kleist  - changes related to memory allocation and standard output
!   2014-07-25 sienkiewicz - supress warning if npred_radiag == 0
!   2017-07-17 mccarty - renamed routine to _bin suffix for ncdiag
!
! input argument list:
!   ftin          - unit number connected to diagnostic file 
!   npred_radiag  - number of bias correction terms
!   retrieval     - .true. if sst retrieval
!
! output argument list:
!   header_fix    - header information structure
!   header_chan   - channel information structure
!   data_name     - diag file data names
!   iflag         - error code
!   lverbose      - optional flag to turn off default output to standard out 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

! Declare passed arguments
  integer(i_kind),intent(in)             :: ftin
  integer(i_kind),intent(in)             :: npred_radiag
  logical,intent(in)                     :: retrieval
  type(diag_header_fix_list ),intent(out):: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  type(diag_data_name_list)              :: data_name
  integer(i_kind),intent(out)            :: iflag
  logical,optional,intent(in)            :: lverbose    

!  Declare local variables
  character(len=2):: string
  character(len=11):: satid
  character(len=10):: sentype
  character(len=20):: sensat
  integer(i_kind) :: i,ich
  integer(i_kind):: jiter,nchanl,npred,ianldate,ireal,ipchan,iextra,jextra
  integer(i_kind):: idiag,angord,iversion,inewpc,isens,ijacob
  integer(i_kind):: iuse_tmp,nuchan_tmp,iochan_tmp
  real(r_single) :: freq_tmp,polar_tmp,wave_tmp,varch_tmp,tlapmean_tmp
  logical loutall

  loutall=.true.
  if(present(lverbose)) loutall=lverbose

! Read header (fixed_part).
  read(ftin,IOSTAT=iflag)  sensat,satid,sentype,jiter,nchanl,npred,ianldate,&
          ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc,isens,ijacob
  if (iflag/=0) then
     rewind(ftin)
     read(ftin,IOSTAT=iflag) sensat,satid,sentype,jiter,nchanl,npred,ianldate,&
          ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc,isens
     ijacob=0
     if (iflag/=0) then
        rewind(ftin)
        read(ftin,IOSTAT=iflag) sensat,satid,sentype,jiter,nchanl,npred,ianldate,&
             ireal,ipchan,iextra,jextra,idiag,angord,iversion,inewpc
        isens=0
     end if
  end if

  if (iflag/=0) then
     rewind(ftin)
     read(ftin,IOSTAT=iflag) sensat,satid,sentype,jiter,nchanl,npred,ianldate,&
          ireal,ipchan,iextra,jextra
     idiag=ipchan+npred+1
     angord=0
     iversion=0
     inewpc=0
     isens=0
     if (iflag/=0) then
        write(6,*)'READ_RADIAG_HEADER:  ***ERROR*** Unknown file format.  Cannot read'
        return
     endif
  endif

  header_fix%isis    = sensat
  header_fix%id      = satid
  header_fix%obstype = sentype
  header_fix%jiter   = jiter
  header_fix%nchan   = nchanl
  header_fix%npred   = npred
  header_fix%idate   = ianldate
  header_fix%ireal   = ireal
  header_fix%ipchan  = ipchan
  header_fix%iextra  = iextra
  header_fix%jextra  = jextra
  header_fix%idiag   = idiag
  header_fix%ijacob  = ijacob
  header_fix%angord  = angord
  header_fix%iversion= iversion
  header_fix%inewpc  = inewpc
  header_fix%isens   = isens

  if (loutall) then
     write(6,*)'READ_RADIAG_HEADER:  isis=',header_fix%isis,&
          ' nchan=',header_fix%nchan,&
          ' npred=',header_fix%npred,&
          ' angord=',header_fix%angord,&
          ' idiag=',header_fix%idiag,&
          ' iversion=',header_fix%iversion,&
          ' inewpc=',header_fix%inewpc,&
          ' isens=',header_fix%isens,&
          ' ijacob=',header_fix%ijacob
     
     if ( header_fix%iextra /= 0) &
          write(6,*)'READ_RADIAG_HEADER:  extra diagnostic information available, ',&
          'iextra=',header_fix%iextra
  end if

  if (header_fix%npred  /= npred_radiag .and. npred_radiag /= 0) &
       write(6,*) 'READ_RADIAG_HEADER:  **WARNING** header_fix%npred,npred=',&
       header_fix%npred,npred_radiag
  
! Allocate and initialize as needed
     if (allocated(header_chan)) deallocate(header_chan)
     if (allocated(data_name%chn))  deallocate(data_name%chn)

     allocate(header_chan( header_fix%nchan))
     allocate(data_name%chn(header_fix%idiag))

     data_name%fix(1) ='lat       '
     data_name%fix(2) ='lon       '
     data_name%fix(3) ='zsges     '
     data_name%fix(4) ='obstim    '
     data_name%fix(5) ='scanpos   '
     data_name%fix(6) ='satzen    '
     data_name%fix(7) ='satazm    '
     data_name%fix(8) ='solzen    '
     data_name%fix(9) ='solazm    '
     data_name%fix(10)='sungln    '
     data_name%fix(11)='fwater    '
     data_name%fix(12)='fland     '
     data_name%fix(13)='fice      '
     data_name%fix(14)='fsnow     '
     data_name%fix(15)='twater    '
     data_name%fix(16)='tland     '
     data_name%fix(17)='tice      '
     data_name%fix(18)='tsnow     '
     data_name%fix(19)='tsoil     '
     data_name%fix(20)='soilmoi   '
     data_name%fix(21)='landtyp   '
     data_name%fix(22)='vegfrac   '
     data_name%fix(23)='snowdep   '
     data_name%fix(24)='wndspd    '
     data_name%fix(25)='qc1       '
     data_name%fix(26)='qc2       '
     data_name%fix(27)='tref      '
     data_name%fix(28)='dtw       '
     data_name%fix(29)='dtc       '
     data_name%fix(30)='tz_tr     '

     data_name%chn(1)='obs       '
     data_name%chn(2)='omgbc     '
     data_name%chn(3)='omgnbc    '
     data_name%chn(4)='errinv    '
     data_name%chn(5)='qcmark    '
     data_name%chn(6)='emiss     '
     data_name%chn(7)='tlap      '
     data_name%chn(8)='tb_tz     '

     if (header_fix%iversion < iversion_radiag_1) then
        data_name%chn( 8)= 'bifix     '
        data_name%chn( 9)= 'bilap     '
        data_name%chn(10)= 'bilap2    '
        data_name%chn(11)= 'bicons    '
        data_name%chn(12)= 'biang     '
        data_name%chn(13)= 'biclw     '
        if (retrieval) data_name%chn(13)= 'bisst     '
     elseif ( header_fix%iversion < iversion_radiag_2 .and. header_fix%iversion >= iversion_radiag_1 ) then
        data_name%chn( 8)= 'bicons    '
        data_name%chn( 9)= 'biang     '
        data_name%chn(10)= 'biclw     '
        data_name%chn(11)= 'bilap2    '
        data_name%chn(12)= 'bilap     '
        do i=1,header_fix%angord
           write(string,'(i2.2)') header_fix%angord-i+1
           data_name%chn(12+i)= 'bifix' // string
        end do
        data_name%chn(12+header_fix%angord+1)= 'bifix     '
        data_name%chn(12+header_fix%angord+2)= 'bisst     '
     elseif ( header_fix%iversion < iversion_radiag_3 .and. header_fix%iversion >= iversion_radiag_2 ) then
        data_name%chn( 9)= 'bicons    '
        data_name%chn(10)= 'biang     '
        data_name%chn(11)= 'biclw     '
        data_name%chn(12)= 'bilap2    '
        data_name%chn(13)= 'bilap     '
        do i=1,header_fix%angord
           write(string,'(i2.2)') header_fix%angord-i+1
           data_name%chn(13+i)= 'bifix' // string
        end do
        data_name%chn(13+header_fix%angord+1)= 'bifix     '
        data_name%chn(13+header_fix%angord+2)= 'bisst     '
     elseif ( header_fix%iversion < iversion_radiag_4 .and. header_fix%iversion >= iversion_radiag_3 ) then
        data_name%chn( 9)= 'bicons    '
        data_name%chn(10)= 'biang     '
        data_name%chn(11)= 'biclw     '
        data_name%chn(12)= 'bilap2    '
        data_name%chn(13)= 'bilap     '
        data_name%chn(14)= 'bicos     '
        data_name%chn(15)= 'bisin     '
        do i=1,header_fix%angord
           write(string,'(i2.2)') header_fix%angord-i+1
           data_name%chn(15+i)= 'bifix' // string
        end do
        data_name%chn(15+header_fix%angord+1)= 'bifix     '
        data_name%chn(15+header_fix%angord+2)= 'bisst     '
     else
        data_name%chn( 9)= 'bicons    '
        data_name%chn(10)= 'biang     '
        data_name%chn(11)= 'biclw     '
        data_name%chn(12)= 'bilap2    '
        data_name%chn(13)= 'bilap     '
        data_name%chn(14)= 'bicos     '
        data_name%chn(15)= 'bisin     '
        data_name%chn(16)= 'biemis    '
        do i=1,header_fix%angord
           write(string,'(i2.2)') header_fix%angord-i+1
           data_name%chn(16+i)= 'bifix' // string
        end do
        data_name%chn(16+header_fix%angord+1)= 'bifix     '
        data_name%chn(16+header_fix%angord+2)= 'bisst     '
     endif

! Read header (channel part)
  do ich=1, header_fix%nchan
      read(ftin,IOSTAT=iflag) freq_tmp,polar_tmp,wave_tmp,varch_tmp,tlapmean_tmp,iuse_tmp,nuchan_tmp,iochan_tmp
      header_chan(ich)%freq     = freq_tmp
      header_chan(ich)%polar    = polar_tmp
      header_chan(ich)%wave     = wave_tmp
      header_chan(ich)%varch    = varch_tmp
      header_chan(ich)%tlapmean = tlapmean_tmp
      header_chan(ich)%iuse     = iuse_tmp
      header_chan(ich)%nuchan   = nuchan_tmp
      header_chan(ich)%iochan   = iochan_tmp
     if (iflag/=0) return
  end do

! Construct array containing menonics for data record entries
  
     
end subroutine read_radiag_header_bin

integer(i_kind) function find_ncdiag_id(ftin)
  integer(i_kind), intent(in) :: ftin

  integer(i_kind) :: i

  find_ncdiag_id = -1
  do i = 1, MAX_OPEN_NCDIAG
     if (ncdiag_open_id(i) == ftin) then
        find_ncdiag_id = i
        return
     endif
  enddo
  return

end function find_ncdiag_id

subroutine read_radiag_data(ftin,header_fix,retrieval,data_fix,data_chan,data_extra,iflag )
!                .      .    .                                       .
! subprogram:    read_radiag_dat                    read rad diag data
!   prgmmr: tahara           org: np20                date: 2003-01-01
!
! abstract:  This routine reads the data record from a radiance
!            diagnostic file
!
! program history log:
!   2010-10-05 treadon - add this doc block
!   2011-02-22 kleist  - changes related to memory allocation
!   2017-07-17 mccarty - change routine to be generalized for bin/nc4 files
!
! input argument list:
!   ftin - unit number connected to diagnostic file
!   header_fix - header information structure
!
! output argument list:
!   data_fix   - spot header information structure
!   data_chan  - spot channel information structure
!   data_extra - spot extra information
!   iflag      - error code
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$


! Declare passed arguments
  integer(i_kind),intent(in)             :: ftin
  type(diag_header_fix_list ),intent(in) :: header_fix
  logical,intent(in)                     :: retrieval
  type(diag_data_fix_list)   ,intent(out):: data_fix
  type(diag_data_chan_list)  ,allocatable :: data_chan(:)
  type(diag_data_extra_list) ,allocatable :: data_extra(:,:)
  integer(i_kind),intent(out)            :: iflag

  integer(i_kind) :: id

  if (netcdf) then

     id = find_ncdiag_id(ftin)
     if (id < 0) then
        write(6,*) 'READ_RADIAG_DATA:  ***ERROR*** netcdf diag file ', ftin, ' has not been opened yet.'
        iflag = -2
        return
     endif

     if (.not. ncdiag_open_status(id)%nc_read) then
        call read_radiag_data_nc_init(ftin, ncdiag_open_status(id), header_fix, retrieval, iflag)
     endif

     if (iflag /= 0) then
        return
     endif

     if (ncdiag_open_status(id)%cur_ob_idx == ncdiag_open_status(id)%num_records ) then
       iflag = 0
     else if (ncdiag_open_status(id)%cur_ob_idx > ncdiag_open_status(id)%num_records) then
       iflag = -1
     else
       iflag = 1
     endif

     if (iflag >= 0) then
        call read_radiag_data_nc(ncdiag_open_status(id),header_fix,data_fix,data_chan,data_extra,iflag)
     endif

  else
     call read_radiag_data_bin(ftin,header_fix,retrieval,data_fix,data_chan,data_extra,iflag )
  endif

end subroutine read_radiag_data

subroutine read_radiag_data_nc_init(ftin, diag_status, header_fix, retrieval, iflag)
!                .      .    .                                       .
! subprogram:    read_radiag_data_nc_init           read rad diag data
!   prgmmr: mccarty          org: np20                date: 2015-08-10
!
! abstract:  This routine reads the data record from a netcdf radiance
!            diagnostic file
!
! program history log:
!   2015-06-10 mccarty  - create routine
!
! input argument list:
!   ftin - unit number connected to diagnostic file
!   header_fix - header information structure
!
! output argument list:
!   data_fix   - spot header information structure
!   data_chan  - spot channel information structure
!   data_extra - spot extra information
!   iflag      - error code
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

! Declare passed arguments
  integer(i_kind),intent(in)             :: ftin
  type(ncdiag_status), intent(inout)     :: diag_status
  type(diag_header_fix_list ),intent(in) :: header_fix
  logical,intent(in)                     :: retrieval
  integer(i_kind),intent(out)            :: iflag

! Declare local variables
  integer(i_kind)                          :: nrecord, ndatum, nangord
  integer(i_kind)                          :: cch, ic, ir, cdatum, nsdim
  real(r_single), allocatable, dimension(:)  :: Latitude, Longitude, Elevation, Obs_Time, Scan_Position, &
                                              Sat_Zenith_Angle, Sat_Azimuth_Angle, Sol_Zenith_Angle, Sol_Azimuth_Angle,  &
                                              Sun_Glint_Angle, Water_Fraction, Land_Fraction, Ice_Fraction,  &
                                              Snow_Fraction, Water_Temperature, Land_Temperature, Ice_Temperature,  &
                                              Snow_Temperature, Soil_Temperature, Soil_Moisture,  &
                                              tsavg5, sstcu, sstph, sstnv, dta, dqa, dtp_avh, Vegetation_Fraction,  &
                                              Snow_Depth, clw_guess_retrieval, Sfc_Wind_Speed,  &
                                              Cloud_Frac, CTP, CLW, TPWC, clw_obs, clw_guess, Foundation_Temperature, SST_Warm_layer_dt,  &
                                              SST_Cool_layer_tdrop, SST_dTz_dTfound, Observation, Obs_Minus_Forecast_adjusted,  &
                                              Obs_Minus_Forecast_unadjusted, Inverse_Observation_Error, QC_Flag, Emissivity,  &
                                              Weighted_Lapse_Rate, dTb_dTs, BC_Constant, BC_Scan_Angle,  &
                                              BC_Cloud_Liquid_Water, BC_Lapse_Rate_Squared, BC_Lapse_Rate, BC_Cosine_Latitude_times_Node,  &
                                              BC_Sine_Latitude,BC_Emissivity,BC_Fixed_Scan_Position, Press_Max_Weight_Function
  integer(i_kind), allocatable, dimension(:)  :: Channel_Index, Land_Type_Index
  real(r_single), allocatable, dimension(:,:)   :: BC_angord ! (nobs, BC_angord_arr_dim) ;
  real(r_single), allocatable, dimension(:,:) :: Observation_Operator_Jacobian

  real(r_kind)                                :: clat, clon

  ndatum = nc_diag_read_get_dim(ftin,'nobs')
  if (ndatum <= 0) then
     iflag = -3
     return
  endif

  if (header_fix%angord > 0) then
     nangord = nc_diag_read_get_dim(ftin,'BC_angord_arr_dim')
  end if

  nrecord = ndatum / header_fix%nchan
  diag_status%num_records = nrecord

  allocate( Channel_Index(ndatum),                                                                                                     &
            Latitude(ndatum),                 Longitude(ndatum),                        Elevation(ndatum),                             &
            Obs_Time(ndatum),                 Scan_Position(ndatum),                    Sat_Zenith_Angle(ndatum),                      &
            Sat_Azimuth_Angle(ndatum),        Sol_Zenith_Angle(ndatum),                 Sol_Azimuth_Angle(ndatum),                     &
            Sun_Glint_Angle(ndatum),          Water_Fraction(ndatum),                   Land_Fraction(ndatum),                         &
            Ice_Fraction(ndatum),             Snow_Fraction(ndatum),                    Water_Temperature(ndatum),                     &
            Land_Temperature(ndatum),         Ice_Temperature(ndatum),                  Snow_Temperature(ndatum),                      &
            Soil_Temperature(ndatum),         Soil_Moisture(ndatum),                    tsavg5(ndatum),                                &
            sstcu(ndatum),                    sstph(ndatum),                            sstnv(ndatum),                                 &
            dta(ndatum),                      dqa(ndatum),                              dtp_avh(ndatum),                               &
            Vegetation_Fraction(ndatum),      Snow_Depth(ndatum),                                                                      & 
            clw_guess_retrieval(ndatum),      Sfc_Wind_Speed(ndatum),                   Cloud_Frac(ndatum),                            &
            CTP(ndatum),                      CLW(ndatum),                              TPWC(ndatum),                                  &
            clw_obs(ndatum),                  clw_guess(ndatum),                        Foundation_Temperature(ndatum),                &
            SST_Warm_layer_dt(ndatum),        SST_Cool_layer_tdrop(ndatum),             SST_dTz_dTfound(ndatum),                       &
            Observation(ndatum),              Obs_Minus_Forecast_adjusted(ndatum),Obs_Minus_Forecast_unadjusted(ndatum),               &
            Inverse_Observation_Error(ndatum),QC_Flag(ndatum),                          Emissivity(ndatum),                            &
            Weighted_Lapse_Rate(ndatum),      dTb_dTs(ndatum),                          BC_Constant(ndatum),                           &
            BC_Scan_Angle(ndatum),            BC_Cloud_Liquid_Water(ndatum),            BC_Lapse_Rate_Squared(ndatum),                 &
            BC_Lapse_Rate(ndatum),            BC_Cosine_Latitude_times_Node(ndatum),    BC_Sine_Latitude(ndatum),                      &
            BC_Emissivity(ndatum),            BC_Fixed_Scan_Position(ndatum),           Land_Type_Index(ndatum)                        )

  if (header_fix%iextra > 0) then
     allocate(Press_Max_Weight_Function(ndatum))
  endif
  if (header_fix%angord > 0) then
     allocate( BC_angord(nangord, ndatum)    )
  end if
  if (header_fix%ijacob > 0) then
     call nc_diag_read_get_global_attr(ftin, "Number_of_state_vars", nsdim)
     allocate(Observation_Operator_Jacobian(nsdim, ndatum))
  endif

  if (allocated(diag_status%all_data_fix))   deallocate(diag_status%all_data_fix)
  if (allocated(diag_status%all_data_chan))  deallocate(diag_status%all_data_chan)
  if (allocated(diag_status%all_data_extra)) deallocate(diag_status%all_data_extra)
  allocate(  diag_status%all_data_fix(nrecord)        )
  allocate(  diag_status%all_data_chan(nrecord, header_fix%nchan))
  allocate(  diag_status%all_data_extra(nrecord, header_fix%iextra, header_fix%jextra) )

  call nc_diag_read_get_var(ftin, 'Channel_Index', Channel_Index)
  call nc_diag_read_get_var(ftin, 'Latitude', Latitude)
  call nc_diag_read_get_var(ftin, 'Longitude', Longitude)
  call nc_diag_read_get_var(ftin, 'Elevation', Elevation)
  call nc_diag_read_get_var(ftin, 'Obs_Time', Obs_Time)
  call nc_diag_read_get_var(ftin, 'Scan_Position', Scan_Position)
  call nc_diag_read_get_var(ftin, 'Sat_Zenith_Angle', Sat_Zenith_Angle)
  call nc_diag_read_get_var(ftin, 'Sat_Azimuth_Angle', Sat_Azimuth_Angle)
  call nc_diag_read_get_var(ftin, 'Sol_Zenith_Angle', Sol_Zenith_Angle)
  call nc_diag_read_get_var(ftin, 'Sol_Azimuth_Angle', Sol_Azimuth_Angle)
  call nc_diag_read_get_var(ftin, 'Sun_Glint_Angle', Sun_Glint_Angle)
  call nc_diag_read_get_var(ftin, 'Water_Fraction', Water_Fraction)
  call nc_diag_read_get_var(ftin, 'Land_Fraction', Land_Fraction)
  call nc_diag_read_get_var(ftin, 'Ice_Fraction', Ice_Fraction)
  call nc_diag_read_get_var(ftin, 'Snow_Fraction', Snow_Fraction)
  call nc_diag_read_get_var(ftin, 'Water_Temperature', Water_Temperature)
  call nc_diag_read_get_var(ftin, 'Land_Temperature', Land_Temperature)
  call nc_diag_read_get_var(ftin, 'Ice_Temperature', Ice_Temperature)
  call nc_diag_read_get_var(ftin, 'Snow_Temperature', Snow_Temperature)
  call nc_diag_read_get_var(ftin, 'Soil_Temperature', Soil_Temperature)
  call nc_diag_read_get_var(ftin, 'Soil_Moisture', Soil_Moisture)
  call nc_diag_read_get_var(ftin, 'tsavg5', tsavg5)
  call nc_diag_read_get_var(ftin, 'sstcu', sstcu)
  call nc_diag_read_get_var(ftin, 'sstph', sstph)
  call nc_diag_read_get_var(ftin, 'sstnv', sstnv)
  call nc_diag_read_get_var(ftin, 'dta', dta)
  call nc_diag_read_get_var(ftin, 'dqa', dqa)
  call nc_diag_read_get_var(ftin, 'dtp_avh', dtp_avh)
  call nc_diag_read_get_var(ftin, 'Vegetation_Fraction', Vegetation_Fraction)
  call nc_diag_read_get_var(ftin, 'Snow_Depth', Snow_Depth)
  call nc_diag_read_get_var(ftin, 'clw_guess_retrieval', clw_guess_retrieval)
  call nc_diag_read_get_var(ftin, 'Sfc_Wind_Speed', Sfc_Wind_Speed)
  call nc_diag_read_get_var(ftin, 'Cloud_Frac', Cloud_Frac)
  call nc_diag_read_get_var(ftin,'CTP', CTP)
  call nc_diag_read_get_var(ftin, 'CLW', CLW)
  call nc_diag_read_get_var(ftin, 'TPWC', TPWC)
  call nc_diag_read_get_var(ftin, 'clw_obs', clw_obs)
  call nc_diag_read_get_var(ftin, 'clw_guess', clw_guess)
  call nc_diag_read_get_var(ftin, 'Foundation_Temperature', Foundation_Temperature)
  call nc_diag_read_get_var(ftin, 'SST_Warm_layer_dt', SST_Warm_layer_dt)
  call nc_diag_read_get_var(ftin, 'SST_Cool_layer_tdrop', SST_Cool_layer_tdrop)
  call nc_diag_read_get_var(ftin, 'SST_dTz_dTfound', SST_dTz_dTfound)
  call nc_diag_read_get_var(ftin, 'Observation', Observation)
  call nc_diag_read_get_var(ftin, 'Obs_Minus_Forecast_adjusted', Obs_Minus_Forecast_adjusted)
  call nc_diag_read_get_var(ftin, 'Obs_Minus_Forecast_unadjusted', Obs_Minus_Forecast_unadjusted)
  call nc_diag_read_get_var(ftin, 'Inverse_Observation_Error', Inverse_Observation_Error)
  call nc_diag_read_get_var(ftin, 'QC_Flag', QC_Flag)
  call nc_diag_read_get_var(ftin, 'Emissivity', Emissivity)
  call nc_diag_read_get_var(ftin, 'Weighted_Lapse_Rate', Weighted_Lapse_Rate)
  call nc_diag_read_get_var(ftin, 'dTb_dTs', dTb_dTs)
  call nc_diag_read_get_var(ftin, 'BC_Constant', BC_Constant)
  call nc_diag_read_get_var(ftin, 'BC_Scan_Angle', BC_Scan_Angle)
  call nc_diag_read_get_var(ftin, 'BC_Cloud_Liquid_Water', BC_Cloud_Liquid_Water)
  call nc_diag_read_get_var(ftin, 'BC_Lapse_Rate_Squared', BC_Lapse_Rate_Squared)
  call nc_diag_read_get_var(ftin, 'BC_Lapse_Rate', BC_Lapse_Rate)
  call nc_diag_read_get_var(ftin, 'BC_Cosine_Latitude_times_Node', BC_Cosine_Latitude_times_Node)
  call nc_diag_read_get_var(ftin, 'BC_Sine_Latitude', BC_Sine_Latitude)
  call nc_diag_read_get_var(ftin, 'BC_Emissivity', BC_Emissivity)
  call nc_diag_read_get_var(ftin, 'BC_Fixed_Scan_Position', BC_Fixed_Scan_Position)
  call nc_diag_read_get_var(ftin, 'Land_Type_Index', Land_Type_Index)
  if (header_fix%iextra > 0) then
     call nc_diag_read_get_var(ftin, 'Press_Max_Weight_Function', Press_Max_Weight_Function)
  endif
  if (header_fix%angord > 0) then
     call nc_diag_read_get_var(ftin, 'BC_angord ', BC_angord )
  end if
  if (header_fix%ijacob > 0) then
     call nc_diag_read_get_var(ftin, 'Observation_Operator_Jacobian', Observation_Operator_Jacobian)
  endif
  cdatum = 1

!  allocate(  all_data_fix(nrecord)        )
!  allocate(  all_data_chan(nrecord, nchan))

  
  do ir=1,nrecord
    clat = Latitude(cdatum)
    clon = Longitude(cdatum)
    diag_status%all_data_fix(ir)%lat               = Latitude(cdatum)
    diag_status%all_data_fix(ir)%lon               = Longitude(cdatum)     
    diag_status%all_data_fix(ir)%zsges             = Elevation(cdatum)
    diag_status%all_data_fix(ir)%obstime           = Obs_Time(cdatum)
    diag_status%all_data_fix(ir)%senscn_pos        = Scan_Position(cdatum)
    diag_status%all_data_fix(ir)%satzen_ang        = Sat_Zenith_Angle(cdatum)
    diag_status%all_data_fix(ir)%satazm_ang        = Sat_Azimuth_Angle(cdatum)
    diag_status%all_data_fix(ir)%solzen_ang        = Sol_Zenith_Angle(cdatum)
    diag_status%all_data_fix(ir)%solazm_ang        = Sol_Azimuth_Angle(cdatum)
    diag_status%all_data_fix(ir)%sungln_ang        = Sun_Glint_Angle(cdatum)
    diag_status%all_data_fix(ir)%water_frac        = Water_Fraction(cdatum)
    diag_status%all_data_fix(ir)%land_frac         = Land_Fraction(cdatum)
    diag_status%all_data_fix(ir)%ice_frac          = Ice_Fraction(cdatum)
    diag_status%all_data_fix(ir)%snow_frac         = Snow_Fraction(cdatum)
    diag_status%all_data_fix(ir)%water_temp        = Water_Temperature(cdatum)
    diag_status%all_data_fix(ir)%land_temp         = Land_Temperature(cdatum)
    diag_status%all_data_fix(ir)%ice_temp          = Ice_Temperature(cdatum)
    diag_status%all_data_fix(ir)%snow_temp         = Snow_Temperature(cdatum)
    diag_status%all_data_fix(ir)%soil_temp         = Soil_Temperature(cdatum)
    diag_status%all_data_fix(ir)%soil_mois         = Soil_Moisture(cdatum)
    diag_status%all_data_fix(ir)%land_type         = Land_Type_Index(cdatum)
    diag_status%all_data_fix(ir)%veg_frac          = Vegetation_Fraction(cdatum)
    diag_status%all_data_fix(ir)%snow_depth        = Snow_Depth(cdatum)
    diag_status%all_data_fix(ir)%sfc_wndspd        = Sfc_Wind_Speed(cdatum)
    diag_status%all_data_fix(ir)%qcdiag1           = Cloud_Frac(cdatum)
    diag_status%all_data_fix(ir)%qcdiag2           = CTP(cdatum)
    diag_status%all_data_fix(ir)%tref              = Foundation_Temperature(cdatum)
    diag_status%all_data_fix(ir)%dtw               = SST_Warm_layer_dt(cdatum)
    diag_status%all_data_fix(ir)%dtc               = SST_Cool_layer_tdrop(cdatum)
    diag_status%all_data_fix(ir)%tz_tr             = SST_dTz_dTfound(cdatum)

    if (retrieval) then
      diag_status%all_data_fix(ir)%water_temp        = tsavg5(cdatum)
      diag_status%all_data_fix(ir)%land_temp         = sstcu(cdatum)
      diag_status%all_data_fix(ir)%ice_temp          = sstph(cdatum)
      diag_status%all_data_fix(ir)%snow_temp         = sstnv(cdatum)
      diag_status%all_data_fix(ir)%soil_temp         = dta(cdatum)
      diag_status%all_data_fix(ir)%soil_mois         = dqa(cdatum)
      diag_status%all_data_fix(ir)%land_type         = dtp_avh(cdatum)
    endif

    do ic=1,header_fix%nchan
      if (clat /= Latitude(cdatum) .or. clon /= Longitude(cdatum)) then
        write(*,*) 'ERROR: Lats & Lons are mismatched.  This is bad'
        print *,'irecord=',ir
        print *,'clat,clon=',clat,clon
        print *,'lat/lon(datum)=',Latitude(cdatum), Longitude(cdatum)
        call abort
      endif
      cch = Channel_Index(cdatum)
      if (allocated(diag_status%all_data_chan(ir,cch)%bifix)) deallocate(diag_status%all_data_chan(ir,cch)%bifix )
      if (header_fix%angord > 0) then
         allocate(diag_status%all_data_chan(ir,cch)%bifix(nangord))
      else
         allocate(diag_status%all_data_chan(ir,cch)%bifix(1))
      end if

      diag_status%all_data_chan(ir,cch)%tbobs = Observation(cdatum)
      diag_status%all_data_chan(ir,cch)%omgbc = Obs_Minus_Forecast_adjusted(cdatum)
      diag_status%all_data_chan(ir,cch)%omgnbc= Obs_Minus_Forecast_unadjusted(cdatum)
      diag_status%all_data_chan(ir,cch)%errinv= Inverse_Observation_Error(cdatum)
      diag_status%all_data_chan(ir,cch)%qcmark= QC_Flag(cdatum)
      diag_status%all_data_chan(ir,cch)%emiss = Emissivity(cdatum)
      diag_status%all_data_chan(ir,cch)%tlap  = Weighted_Lapse_Rate(cdatum)
      diag_status%all_data_chan(ir,cch)%tb_tz = dTb_dTs(cdatum)
      diag_status%all_data_chan(ir,cch)%bicons= BC_Constant(cdatum)
      diag_status%all_data_chan(ir,cch)%biang = BC_Scan_Angle(cdatum)
      diag_status%all_data_chan(ir,cch)%biclw = BC_Cloud_Liquid_Water(cdatum)
      diag_status%all_data_chan(ir,cch)%bilap2= BC_Lapse_Rate_Squared(cdatum)
      diag_status%all_data_chan(ir,cch)%bilap = BC_Lapse_Rate(cdatum)
      diag_status%all_data_chan(ir,cch)%bicos = BC_Cosine_Latitude_times_Node(cdatum)
      diag_status%all_data_chan(ir,cch)%bisin = BC_Sine_Latitude(cdatum)
      diag_status%all_data_chan(ir,cch)%biemis= BC_Emissivity(cdatum)
      if (header_fix%angord > 0) then
         diag_status%all_data_chan(ir,cch)%bifix = BC_angord(1:nangord,cdatum)
      else
         diag_status%all_data_chan(ir,cch)%bifix(1) = BC_Fixed_Scan_Position(cdatum)
      endif
      if (header_fix%ijacob > 0) then
         diag_status%all_data_chan(ir,cch)%dhx_dx = Observation_Operator_Jacobian(1:nsdim,cdatum)
      endif
      ! placeholder for SST BC
      if (header_fix%iextra > 0) then
         diag_status%all_data_extra(ir,1,cch)%extra = Press_Max_Weight_Function(cdatum)
      endif
      cdatum = cdatum + 1
    enddo
  enddo

  diag_status%nc_read = .true.
  diag_status%cur_ob_idx = 1


  deallocate(Channel_Index, Latitude, Longitude, Elevation, Obs_Time, Scan_Position,    &
             Sat_Zenith_Angle, Sat_Azimuth_Angle, Sol_Zenith_Angle, Sol_Azimuth_Angle,  &
             Sun_Glint_Angle, Water_Fraction, Land_Fraction, Ice_Fraction,              &
             Snow_Fraction, Water_Temperature, Land_Temperature, Ice_Temperature,       &
             Snow_Temperature, Soil_Temperature, Soil_Moisture, tsavg5, sstcu, sstph,   &
             sstnv, dta, dqa, dtp_avh, Vegetation_Fraction, Snow_Depth,                 &
             clw_guess_retrieval, Sfc_Wind_Speed, Cloud_Frac, CTP, CLW, TPWC, clw_obs,  &
             clw_guess, Foundation_Temperature, SST_Warm_layer_dt, SST_Cool_layer_tdrop, &
             SST_dTz_dTfound, Observation, Obs_Minus_Forecast_adjusted,                 &
             Obs_Minus_Forecast_unadjusted, Inverse_Observation_Error, QC_Flag,         &
             Emissivity, Weighted_Lapse_Rate, dTb_dTs, BC_Constant, BC_Scan_Angle,      &
             BC_Cloud_Liquid_Water, BC_Lapse_Rate_Squared, BC_Lapse_Rate,               &
             BC_Cosine_Latitude_times_Node, BC_Sine_Latitude, BC_Emissivity,            &
             BC_Fixed_Scan_Position, Land_Type_Index)

  if (header_fix%iextra > 0) then
     deallocate(Press_Max_Weight_Function)
  endif
  if (header_fix%angord > 0) then
     deallocate(BC_angord)
  end if
  if (header_fix%ijacob > 0) then
     deallocate(Observation_Operator_Jacobian)
  endif


end subroutine read_radiag_data_nc_init

subroutine read_radiag_data_nc(diag_status,header_fix,data_fix,data_chan,data_extra,iflag )
!                .      .    .                                       .
! subprogram:    read_radiag_dat                    read rad diag data
!   prgmmr: tahara           org: np20                date: 2015-08-10
!
! abstract:  This routine reads the data record from a netcdf radiance
!            diagnostic file
!
! program history log:
!   2015-08-10 mccarty  - create routine
!
! input argument list:
!   header_fix - header information structure
!
! output argument list:
!   data_fix   - spot header information structure
!   data_chan  - spot channel information structure
!   data_extra - spot extra information
!   iflag      - error code
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

! Declare passed arguments
  type(ncdiag_status), intent(inout)     :: diag_status
  type(diag_header_fix_list ),intent(in) :: header_fix
  type(diag_data_fix_list)   ,intent(out):: data_fix
  type(diag_data_chan_list)  ,allocatable :: data_chan(:)
  type(diag_data_extra_list) ,allocatable :: data_extra(:,:)
  integer(i_kind),intent(out)            :: iflag

  iflag = 0
  if (.not. allocated(data_chan)) allocate(data_chan(header_fix%nchan) )
  if (.not. allocated(data_extra)) allocate(data_extra(header_fix%iextra, header_fix%nchan) )

  data_fix        = diag_status%all_data_fix(diag_status%cur_ob_idx)
  data_chan(:)    = diag_status%all_data_chan(diag_status%cur_ob_idx,:)
  if (header_fix%iextra > 0) then
     data_extra(:,:) = diag_status%all_data_extra(diag_status%cur_ob_idx,:,:)
  endif

  diag_status%cur_ob_idx = diag_status%cur_ob_idx + 1

end subroutine read_radiag_data_nc

subroutine read_radiag_data_bin(ftin,header_fix,retrieval,data_fix,data_chan,data_extra,iflag )
!                .      .    .                                       .
! subprogram:    read_radiag_dat                    read rad diag data
!   prgmmr: tahara           org: np20                date: 2003-01-01
!
! abstract:  This routine reads the data record from a radiance
!            diagnostic file
!
! program history log:
!   2010-10-05 treadon - add this doc block
!   2011-02-22 kleist  - changes related to memory allocation
!   2017-07-17 mccarty - rename binary-specific procedure
!
! input argument list:
!   ftin - unit number connected to diagnostic file
!   header_fix - header information structure
!
! output argument list:
!   data_fix   - spot header information structure
!   data_chan  - spot channel information structure
!   data_extra - spot extra information
!   iflag      - error code
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$


! Declare passed arguments
  integer(i_kind),intent(in)             :: ftin
  type(diag_header_fix_list ),intent(in) :: header_fix
  logical,intent(in)                     :: retrieval
  type(diag_data_fix_list)   ,intent(out):: data_fix
  type(diag_data_chan_list)  ,allocatable :: data_chan(:)
  type(diag_data_extra_list) ,allocatable :: data_extra(:,:)
  integer(i_kind),intent(out)            :: iflag
    
  integer(i_kind) :: ich,iang,i,j
  real(r_single),dimension(:,:),allocatable :: data_tmp
  real(r_single),dimension(:),allocatable   :: fix_tmp
  real(r_single),dimension(:,:),allocatable :: extra_tmp

  type(sparr2) :: dhx_dx

! Allocate arrays as needed
  if (allocated(data_chan)) deallocate(data_chan)
  allocate(data_chan(header_fix%nchan))

  do ich=1,header_fix%nchan
     if (allocated(data_chan(ich)%bifix)) deallocate(data_chan(ich)%bifix)
     allocate(data_chan(ich)%bifix(header_fix%angord+1))
  end do

  if (header_fix%iextra > 0) then
     if (allocated(data_extra))   deallocate(data_extra)
     allocate(data_extra(header_fix%iextra,header_fix%jextra))
     allocate(extra_tmp(header_fix%iextra,header_fix%jextra))
  end if

! Allocate arrays to hold data record
  allocate(data_tmp(header_fix%idiag,header_fix%nchan))

  if (header_fix%iversion < iversion_radiag_2) then
     allocate( fix_tmp( ireal_old_radiag ) )
  else
     allocate( fix_tmp( ireal_radiag ) )
  end if

! Read data record

  if (header_fix%iextra == 0) then
     read(ftin,IOSTAT=iflag) fix_tmp, data_tmp
  else
     read(ftin,IOSTAT=iflag) fix_tmp, data_tmp, extra_tmp
  endif

  if (iflag /= 0) return

! Transfer fix_tmp record to output structure
  data_fix%lat = fix_tmp(1)
  data_fix%lon = fix_tmp(2)
  data_fix%zsges = fix_tmp(3)
  data_fix%obstime = fix_tmp(4) 
  data_fix%senscn_pos = fix_tmp(5)
  data_fix%satzen_ang = fix_tmp(6)
  data_fix%satazm_ang = fix_tmp(7)
  data_fix%solzen_ang = fix_tmp(8)
  data_fix%solazm_ang = fix_tmp(9)
  data_fix%sungln_ang = fix_tmp(10)
  data_fix%water_frac = fix_tmp(11)
  data_fix%land_frac = fix_tmp(12)
  data_fix%ice_frac = fix_tmp(13)
  data_fix%snow_frac = fix_tmp(14)
  data_fix%water_temp = fix_tmp(15)
  data_fix%land_temp = fix_tmp(16)
  data_fix%ice_temp = fix_tmp(17)
  data_fix%snow_temp = fix_tmp(18)
  data_fix%soil_temp = fix_tmp(19)
  data_fix%soil_mois = fix_tmp(20)
  data_fix%land_type = fix_tmp(21)
  data_fix%veg_frac = fix_tmp(22)
  data_fix%snow_depth = fix_tmp(23)
  data_fix%sfc_wndspd = fix_tmp(24)
  data_fix%qcdiag1 = fix_tmp(25)
  data_fix%qcdiag2 = fix_tmp(26)

  if ( header_fix%iversion <= iversion_radiag_1 ) then
     data_fix%tref = rmiss_radiag
     data_fix%dtw = rmiss_radiag
     data_fix%dtc = rmiss_radiag
     data_fix%tz_tr = rmiss_radiag
  else
     data_fix%tref = fix_tmp(27)
     data_fix%dtw = fix_tmp(28)
     data_fix%dtc = fix_tmp(29)
     data_fix%tz_tr = fix_tmp(30)
  end if


! Transfer data record to output structure
  do ich=1,header_fix%nchan
     data_chan(ich)%tbobs =data_tmp(1,ich)
     data_chan(ich)%omgbc =data_tmp(2,ich)
     data_chan(ich)%omgnbc=data_tmp(3,ich)
     data_chan(ich)%errinv=data_tmp(4,ich)
     data_chan(ich)%qcmark=data_tmp(5,ich)
     data_chan(ich)%emiss =data_tmp(6,ich)
     data_chan(ich)%tlap  =data_tmp(7,ich)
     data_chan(ich)%tb_tz =data_tmp(8,ich)
  end do
  if (header_fix%iversion < iversion_radiag_1) then
     do ich=1,header_fix%nchan
        data_chan(ich)%bifix(1)=data_tmp(8,ich)
        data_chan(ich)%bilap   =data_tmp(9,ich)
        data_chan(ich)%bilap2  =data_tmp(10,ich)
        data_chan(ich)%bicons  =data_tmp(11,ich)
        data_chan(ich)%biang   =data_tmp(12,ich)
        data_chan(ich)%biclw   =data_tmp(13,ich)
        data_chan(ich)%bisst   = rmiss_radiag
        if (retrieval) then
           data_chan(ich)%biclw   =rmiss_radiag
           data_chan(ich)%bisst   =data_tmp(13,ich) 
        endif
     end do
  elseif ( header_fix%iversion < iversion_radiag_2 .and. header_fix%iversion >= iversion_radiag_1 ) then
     do ich=1,header_fix%nchan
        data_chan(ich)%bicons=data_tmp(8,ich)
        data_chan(ich)%biang =data_tmp(9,ich)
        data_chan(ich)%biclw =data_tmp(10,ich)
        data_chan(ich)%bilap2=data_tmp(11,ich)
        data_chan(ich)%bilap =data_tmp(12,ich)
     end do
     do ich=1,header_fix%nchan
        do iang=1,header_fix%angord+1
           data_chan(ich)%bifix(iang)=data_tmp(12+iang,ich)
        end do
        data_chan(ich)%bisst = data_tmp(12+header_fix%angord+2,ich)  
     end do
  elseif ( header_fix%iversion < iversion_radiag_3 .and. header_fix%iversion >= iversion_radiag_2 ) then
     do ich=1,header_fix%nchan
        data_chan(ich)%bicons=data_tmp(9,ich)
        data_chan(ich)%biang =data_tmp(10,ich)
        data_chan(ich)%biclw =data_tmp(11,ich)
        data_chan(ich)%bilap2=data_tmp(12,ich)
        data_chan(ich)%bilap =data_tmp(13,ich)
     end do
     do ich=1,header_fix%nchan
        do iang=1,header_fix%angord+1
           data_chan(ich)%bifix(iang)=data_tmp(13+iang,ich)
        end do
        data_chan(ich)%bisst = data_tmp(13+header_fix%angord+2,ich)
     end do
  elseif ( header_fix%iversion < iversion_radiag_4 .and. header_fix%iversion >= iversion_radiag_3 ) then
     do ich=1,header_fix%nchan
        data_chan(ich)%bicons=data_tmp(9,ich)
        data_chan(ich)%biang =data_tmp(10,ich)
        data_chan(ich)%biclw =data_tmp(11,ich)
        data_chan(ich)%bilap2=data_tmp(12,ich)
        data_chan(ich)%bilap =data_tmp(13,ich)
        data_chan(ich)%bicos =data_tmp(14,ich) ! 1st bias correction term node*cos(lat) for SSMIS
        data_chan(ich)%bisin =data_tmp(15,ich) ! 2nd bias correction term sin(lat) for SSMI      
     end do
     do ich=1,header_fix%nchan
        do iang=1,header_fix%angord+1
           data_chan(ich)%bifix(iang)=data_tmp(15+iang,ich)
        end do
        data_chan(ich)%bisst = data_tmp(15+header_fix%angord+2,ich)
     end do
  elseif ( header_fix%iversion < iversion_radiag_5 .and. header_fix%iversion >= iversion_radiag_4 ) then
     do ich=1,header_fix%nchan
        data_chan(ich)%bicons=data_tmp(9,ich)
        data_chan(ich)%biang =data_tmp(10,ich)
        data_chan(ich)%biclw =data_tmp(11,ich)
        data_chan(ich)%bilap2=data_tmp(12,ich)
        data_chan(ich)%bilap =data_tmp(13,ich)
        data_chan(ich)%bicos =data_tmp(14,ich) 
        data_chan(ich)%bisin =data_tmp(15,ich)
        data_chan(ich)%biemis=data_tmp(16,ich)
     end do
     do ich=1,header_fix%nchan
        do iang=1,header_fix%angord+1
           data_chan(ich)%bifix(iang)=data_tmp(16+iang,ich)
        end do
        data_chan(ich)%bisst = data_tmp(16+header_fix%angord+2,ich)
     end do
  else
     do ich=1,header_fix%nchan
        data_chan(ich)%bicons=data_tmp(9,ich)
        data_chan(ich)%biang =data_tmp(10,ich)
        data_chan(ich)%biclw =data_tmp(11,ich)
        data_chan(ich)%bilap2=data_tmp(12,ich)
        data_chan(ich)%bilap =data_tmp(13,ich)
        data_chan(ich)%bicos =data_tmp(14,ich)
        data_chan(ich)%bisin =data_tmp(15,ich)
        data_chan(ich)%biemis=data_tmp(16,ich)
     end do
     do ich=1,header_fix%nchan
        do iang=1,header_fix%angord+1
           data_chan(ich)%bifix(iang)=data_tmp(16+iang,ich)
        end do
        data_chan(ich)%bisst = data_tmp(16+header_fix%angord+2,ich)  
        data_chan(ich)%sprd  = data_tmp(16+header_fix%angord+3,ich)
     end do

     do ich=1,header_fix%nchan
        if (header_fix%ijacob==1) then
           call readarray(dhx_dx, & !data_chan(ich)%dhx_dx,                        &
                          data_tmp(16+header_fix%angord+4:header_fix%idiag,ich))
           data_chan(ich)%dhx_dx = dhx_dx
           call delete(dhx_dx)
        endif
     enddo
  endif

  if (header_fix%iextra > 0) then
     do j=1,header_fix%jextra
        do i=1,header_fix%iextra
           data_extra(i,j)%extra=extra_tmp(i,j)
        end do
     end do
  endif

  deallocate(data_tmp, fix_tmp)
  if (header_fix%iextra > 0) deallocate(extra_tmp)

end subroutine read_radiag_data_bin

end module read_diag

