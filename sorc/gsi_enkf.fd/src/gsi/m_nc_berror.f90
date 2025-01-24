module m_nc_berror
use netcdf
implicit none
private

public :: nc_berror_vars_init
public :: nc_berror_vars_final
public :: nc_berror_vars_comp
public :: nc_berror_vars_copy
public :: nc_berror_vars
public :: nc_berror_dims
public :: nc_berror_read
public :: nc_berror_getpointer

type nc_berror_vars
   logical :: initialized=.false.
   integer :: nlon,nlat,nsig
   real(4),pointer,dimension(:,:,:):: tcon
   real(4),pointer,dimension(:,:)  :: sfvar,vpvar,tvar,qvar,cvar,nrhvar,ozvar
   real(4),pointer,dimension(:,:)  :: qivar,qlvar,qrvar,qsvar
   real(4),pointer,dimension(:,:)  :: sfhln,vphln,thln,qhln,chln,ozhln
   real(4),pointer,dimension(:,:)  :: qihln,qlhln,qrhln,qshln
   real(4),pointer,dimension(:,:)  :: sfvln,vpvln,tvln,qvln,cvln,ozvln
   real(4),pointer,dimension(:,:)  :: qivln,qlvln,qrvln,qsvln
   real(4),pointer,dimension(:,:)  :: vpcon,pscon,varsst,corlsst
   real(4),pointer,dimension(:)    :: psvar,pshln
   real(4),pointer,dimension(:)    :: v1d
   real(4),pointer,dimension(:,:)  :: v2d
   real(4),pointer,dimension(:,:,:):: v3d
end type nc_berror_vars

character(len=*), parameter :: myname = 'm_nc_berror'

integer, parameter :: nv1d = 2
character(len=4),parameter :: cvars1d(nv1d) = (/ 'ps  ', 'hps ' /)

integer, parameter :: nv2d = 33
character(len=5),parameter :: cvars2d(nv2d) = (/ &
                                              'sf   ', 'hsf  ', 'vsf  ', &
                                              'vp   ', 'hvp  ', 'vvp  ', &
                                              't    ', 'ht   ', 'vt   ', &
                                              'q    ', 'hq   ', 'vq   ', &
                                              'qi   ', 'hqi  ', 'vqi  ', &
                                              'ql   ', 'hql  ', 'vql  ', &
                                              'qr   ', 'hqr  ', 'vqr  ', &
                                              'qs   ', 'hqs  ', 'vqs  ', &
                                              'oz   ', 'hoz  ', 'voz  ', &
                                              'cw   ', 'hcw  ', 'vcw  ', &
                                              'pscon', 'vpcon', 'nrh  '  &
                                              /)

integer, parameter :: nvmll = 1  ! meriodional, level, level
character(len=4),parameter :: cvarsMLL(nvmll) = (/ 'tcon' /)

integer, parameter :: nv2dx = 2
character(len=4),parameter :: cvars2dx(nv2dx) = (/ 'sst ', 'hsst' /)

interface nc_berror_dims; module procedure    &
  read_dims_ ; end interface
interface nc_berror_read; module procedure    &
  read_berror_ ; end interface
interface nc_berror_vars_init; module procedure    &
  init_berror_vars_ ; end interface
interface nc_berror_vars_final; module procedure    &
  final_berror_vars_ ; end interface
interface nc_berror_vars_comp; module procedure    &
  comp_berror_vars_ ; end interface
interface nc_berror_vars_copy; module procedure    &
  copy_ ; end interface
interface nc_berror_getpointer 
  module procedure get_pointer_1d_ 
  module procedure get_pointer_2d_
end interface

contains

subroutine read_dims_ (fname,nlat,nlon,nlev,rc, myid,root)
  implicit none
  character(len=*), intent(in)    :: fname ! input filename
  integer, intent(out) :: rc
  integer, intent(out) :: nlat,nlon,nlev
  integer, intent(in), optional :: myid, root

! This will be the netCDF ID for the file and data variable.
  integer :: ncid, varid, ier
  integer :: mype_,root_

! Local variables
  character(len=*), parameter :: myname_ = myname//"::dims_"
   
! Return code (status)
  rc=0; mype_=0; root_=0
  if(present(myid) .and. present(root) ) then
     mype_ = myid
     root_ = root
  endif
 
! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
! the file.
  call check_( nf90_open(fname, NF90_NOWRITE, ncid), rc, mype_, root_ )
  if(rc/=0) return

! Read global attributes
  call check_( nf90_inq_dimid(ncid, "lon", varid), rc, mype_, root_)
  call check_( nf90_inquire_dimension(ncid, varid, len=nlon), rc, mype_, root_ )
  call check_( nf90_inq_dimid(ncid, "lat", varid), rc, mype_, root_ )
  call check_( nf90_inquire_dimension(ncid, varid, len=nlat), rc, mype_, root_ )
  call check_( nf90_inq_dimid(ncid, "lev", varid), rc, mype_, root_ )
  call check_( nf90_inquire_dimension(ncid, varid, len=nlev), rc, mype_, root_ )

! Close the file, freeing all resources.
  call check_( nf90_close(ncid), rc, mype_, root_ )

  return

end subroutine read_dims_

subroutine read_berror_ (fname,bvars,rc, myid,root)
  implicit none
  character(len=*), intent(in)    :: fname ! input filename
  type(nc_berror_vars),intent(inout) :: bvars ! background error variables
  integer, intent(out) :: rc
  integer, intent(in), optional :: myid,root ! accommodate MPI calling programs

! This will be the netCDF ID for the file and data variable.
  integer :: ncid, varid

! Local variables
  character(len=*), parameter :: myname_ = myname//"::read_"
  character(len=4) :: cindx
  integer :: nv,nl,nlat,nlon,nlev
  integer :: ndims_, nvars_, ngatts_, unlimdimid_
  integer :: nlat_,nlon_,nlev_
  integer :: mype_,root_
  real(4), allocatable :: data_in(:,:,:)
  logical :: verbose
  logical :: init_
  
   
! Return code (status)
  rc=0; mype_=0; root_=0
  verbose=.true.
  init_=.false.
  if(present(myid).and.present(root) )then
    if(myid/=root) verbose=.false.
    mype_ = myid
    root_ = root
  endif
 
! Get dimensions
  call read_dims_ (fname,nlat_,nlon_,nlev_,rc, mype_,root_)

  init_ = bvars%initialized
  if ( init_ ) then
!   Set dims
    nlat=bvars%nlat
    nlon=bvars%nlon
    nlev=bvars%nsig

!   Consistency check
    if (nlon_ /= nlon .or. nlat_ /=nlat .or. nlev_/=nlev ) then
       rc=1
       if(myid==root) then
         print *, 'nlat(file) = ', nlat_, 'nlat(required) = ', nlat
         print *, 'nlon(file) = ', nlon_, 'nlon(required) = ', nlon
         print *, 'nlev(file) = ', nlev_, 'nlev(required) = ', nlev
         print *, myname_,  'Inconsistent dimensions, aborting ... '
       endif
       return
    endif
  else
!   Set dims
    nlat=nlat_
    nlon=nlon_
    nlev=nlev_
    call init_berror_vars_(bvars,nlon,nlat,nlev)
  endif

! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
! the file.

  call check_( nf90_open(fname, NF90_NOWRITE, ncid), rc, mype_, root_ )
  if(rc/=0) return

! Read data to file
  allocate(data_in(1,nlat,1))
  do nv = 1, nv1d
     call check_( nf90_inq_varid(ncid, trim(cvars1d(nv)), varid), rc, mype_, root_ )
     call check_( nf90_get_var(ncid, varid, data_in(1,:,1)), rc, mype_, root_ )
     if(trim(cvars1d(nv))=="ps"  ) bvars%psvar = data_in(1,:,1)
     if(trim(cvars1d(nv))=="hps" ) bvars%pshln = data_in(1,:,1)
  enddo
  deallocate(data_in)
  allocate(data_in(1,nlat,nlev))
  do nv = 1, nv2d
     call check_( nf90_inq_varid(ncid, trim(cvars2d(nv)), varid), rc, mype_, root_  )
     call check_( nf90_get_var(ncid, varid, data_in(1,:,:)), rc, mype_, root_ )

     if(trim(cvars2d(nv))=="sf" ) bvars%sfvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hsf") bvars%sfhln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vsf") bvars%sfvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="vp" ) bvars%vpvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hvp") bvars%vphln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vvp") bvars%vpvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="t"  ) bvars%tvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="ht" ) bvars%thln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vt" ) bvars%tvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="q"  ) bvars%qvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hq" ) bvars%qhln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vq" ) bvars%qvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="qi" ) bvars%qivar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hqi") bvars%qihln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vqi") bvars%qivln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="ql" ) bvars%qlvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hql") bvars%qlhln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vql") bvars%qlvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="qr" ) bvars%qrvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hqr") bvars%qrhln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vqr") bvars%qrvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="nrh") bvars%nrhvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="qs" ) bvars%qsvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hqs") bvars%qshln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vqs") bvars%qsvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="cw" ) bvars%cvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hcw") bvars%chln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vcw") bvars%cvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="oz" ) bvars%ozvar = data_in(1,:,:)
     if(trim(cvars2d(nv))=="hoz") bvars%ozhln = data_in(1,:,:)
     if(trim(cvars2d(nv))=="voz") bvars%ozvln = data_in(1,:,:)
!
     if(trim(cvars2d(nv))=="pscon") bvars%pscon = data_in(1,:,:)
     if(trim(cvars2d(nv))=="vpcon") bvars%vpcon = data_in(1,:,:)
!
  enddo

! Get matrix NLATxNLEVxNLEV that has been written as NLEV 2d-fields 
  do nv = 1, nvmll
     do nl = 1, nlev
        write(cindx,'(i4.4)') nl
        if(trim(cvarsMLL(nv))=="tcon") then
           call check_( nf90_inq_varid(ncid, trim(cvarsMLL(nv))//cindx, varid), rc, mype_, root_ )
           call check_( nf90_get_var(ncid, varid, data_in(1,:,:)), rc, mype_, root_ )
           bvars%tcon(:,:,nl) = data_in(1,:,:)
        endif
     enddo
  enddo
  deallocate(data_in)

! Read in lat/lon fields
  allocate(data_in(nlon,nlat,1))
  do nv = 1, nv2dx
     call check_( nf90_inq_varid(ncid, trim(cvars2dx(nv)), varid), rc, mype_, root_ )
     call check_( nf90_get_var(ncid, varid, data_in(:,:,1)), rc, mype_, root_ )
     if(trim(cvars2dx(nv))=="sst"     ) then
        bvars%varsst = transpose(data_in(:,:,1))
     endif
     if(trim(cvars2dx(nv))=="hsst" ) then 
        bvars%corlsst = transpose(data_in(:,:,1))
     endif
  enddo
  deallocate(data_in)

! Close the file, freeing all resources.
  call check_( nf90_close(ncid), rc, mype_, root_ )

  if(verbose) print *,"*** Finish reading file: ", trim(fname)

  return

end subroutine read_berror_

subroutine init_berror_vars_(vr,nlon,nlat,nsig)

  integer,intent(in) :: nlon,nlat,nsig
  type(nc_berror_vars) vr

  if(vr%initialized) return

  vr%nlon=nlon 
  vr%nlat=nlat
  vr%nsig=nsig

! allocate single precision arrays
  allocate(vr%sfvar(nlat,nsig),vr%vpvar(nlat,nsig),vr%tvar(nlat,nsig),vr%qvar(nlat,nsig),  &  
           vr%qivar(nlat,nsig),vr%qlvar(nlat,nsig),vr%qrvar(nlat,nsig),vr%qsvar(nlat,nsig),&
           vr%cvar(nlat,nsig),vr%nrhvar(nlat,nsig),vr%ozvar(nlat,nsig))
  allocate(vr%sfhln(nlat,nsig),vr%vphln(nlat,nsig),vr%thln(nlat,nsig),vr%qhln(nlat,nsig),  &
           vr%qihln(nlat,nsig),vr%qlhln(nlat,nsig),vr%qrhln(nlat,nsig),vr%qshln(nlat,nsig),&
           vr%chln(nlat,nsig), vr%ozhln(nlat,nsig))
  allocate(vr%sfvln(nlat,nsig),vr%vpvln(nlat,nsig),vr%tvln(nlat,nsig),vr%qvln(nlat,nsig),  &
           vr%qivln(nlat,nsig),vr%qlvln(nlat,nsig),vr%qrvln(nlat,nsig),vr%qsvln(nlat,nsig),&
           vr%cvln(nlat,nsig), vr%ozvln(nlat,nsig))
  allocate(vr%pscon(nlat,nsig),vr%vpcon(nlat,nsig))
  allocate(vr%varsst(nlat,nlon),vr%corlsst(nlat,nlon))
  allocate(vr%tcon(nlat,nsig,nsig))
  allocate(vr%psvar(nlat),vr%pshln(nlat))
  vr%initialized=.true.
  end subroutine init_berror_vars_

  subroutine final_berror_vars_(vr)
  type(nc_berror_vars) vr
! deallocate arrays
  if(.not. vr%initialized) return
  deallocate(vr%sfvar,vr%vpvar,vr%tvar,vr%qvar,  &  
             vr%qivar,vr%qlvar,vr%qrvar,vr%qsvar,&
             vr%cvar,vr%nrhvar,vr%ozvar)
  deallocate(vr%sfhln,vr%vphln,vr%thln,vr%qhln,  &
             vr%qihln,vr%qlhln,vr%qrhln,vr%qshln,&
             vr%chln, vr%ozhln)
  deallocate(vr%sfvln,vr%vpvln,vr%tvln,vr%qvln,  &
             vr%qivln,vr%qlvln,vr%qrvln,vr%qsvln,&
             vr%cvln, vr%ozvln)
  deallocate(vr%pscon,vr%vpcon)
  deallocate(vr%varsst,vr%corlsst)
  deallocate(vr%tcon)
  deallocate(vr%psvar,vr%pshln)
  vr%initialized=.false.
end subroutine final_berror_vars_

subroutine comp_berror_vars_(va,vb,rc, myid,root)
  type(nc_berror_vars) va
  type(nc_berror_vars) vb
  integer, intent(out) :: rc
  integer, intent(in), optional :: myid,root        ! accommodate MPI calling programs
  character(len=*), parameter :: myname_ = myname//"::comp_berror_vars_"
  integer :: ii,jj,ier(50)
  logical :: verbose, failed
  real :: tolerance = 10.e-10
!
  rc=0
  verbose=.true.
  if(present(myid).and.present(root) )then
    if(myid/=root) verbose=.false.
  endif
! Consistency check
  if (va%nlon/=vb%nlon .or. va%nlat/=vb%nlat .or. va%nsig/=vb%nsig ) then
     rc=1
     if(myid==root) then
       print *, 'nlat(va) = ', va%nlat, 'nlat(vb) = ', vb%nlat
       print *, 'nlon(va) = ', va%nlon, 'nlon(vb) = ', vb%nlon
       print *, 'nlev(va) = ', va%nsig, 'nlev(vb) = ', vb%nsig
       print *, myname_,  'Inconsistent dimensions, aborting ... '
     endif
     return
  endif

  ii=0;ier=0
  ii=ii+1; if(abs(sum(va%sfvar - vb%sfvar)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%vpvar - vb%vpvar)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%tvar  - vb%tvar))  >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qvar  - vb%qvar )) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qivar - vb%qivar)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qlvar - vb%qlvar)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qrvar - vb%qrvar)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qsvar - vb%qsvar) )>tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%cvar  - vb%cvar )) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%nrhvar- vb%nrhvar))>tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%ozvar - vb%ozvar)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%sfhln - vb%sfhln)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%vphln - vb%vphln ))>tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%thln  - vb%thln))  >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qhln  - vb%qhln) ) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qihln - vb%qihln)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qlhln - vb%qlhln)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qrhln - vb%qrhln) )>tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qshln - vb%qshln ))>tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%chln  - vb%chln )) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%ozhln - vb%ozhln)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%sfvln - vb%sfvln)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%vpvln - vb%vpvln)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%tvln  - vb%tvln))  >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qvln  - vb%qvln )) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qivln - vb%qivln)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qlvln - vb%qlvln)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qrvln - vb%qrvln)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%qsvln - vb%qsvln) )>tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%cvln  - vb%cvln )) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%ozvln - vb%ozvln)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%pscon - vb%pscon)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%vpcon - vb%vpcon)) >tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%varsst- vb%varsst))>tolerance) ier(ii)=ii
  ii=ii+1; if(abs(sum(va%corlsst-vb%corlsst))>tolerance)ier(ii)=ii
  ii=ii+1; if(abs(sum(va%tcon  - vb%tcon))  >tolerance) ier(ii)=ii
  failed=.false.
  do jj=1,ii
     if(ier(jj)/=0.and.verbose) then
       print *, 'Found field ', jj, ' not to match'
       failed=.true.
     endif
  enddo
  if (.not.failed) then
       if(verbose) print *, 'Comp finds all fields to match'
  endif
end subroutine comp_berror_vars_

subroutine copy_(ivars,ovars,hydro)
  type(nc_berror_vars) ivars
  type(nc_berror_vars) ovars
  logical, intent(in), optional :: hydro

  logical wrtall,hydro_

  hydro_=.true.
  wrtall=.true.
  if (ovars%nlon/=ivars%nlon .or. &
      ovars%nlat/=ivars%nlat      ) then
      print*, 'copy_berror_vars_: Trying to copy inconsistent vectors, aborting ...'
      call exit(1)
  endif
  if ( ovars%nsig/=ivars%nsig ) then
     wrtall=.false.
  endif
  if(present(hydro)) then
    hydro_ = hydro
  endif

  if (wrtall) then
     ovars%tcon    = ivars%tcon
     ovars%vpcon   = ivars%vpcon
     ovars%pscon   = ivars%pscon
     ovars%sfvar   = ivars%sfvar
     ovars%sfhln   = ivars%sfhln
     ovars%sfvln   = ivars%sfvln
     ovars%vpvar   = ivars%vpvar
     ovars%vphln   = ivars%vphln
     ovars%vpvln   = ivars%vpvln
     ovars%tvar    = ivars%tvar
     ovars%thln    = ivars%thln
     ovars%tvln    = ivars%tvln
     ovars%qvar    = ivars%qvar
     ovars%nrhvar  = ivars%nrhvar
     ovars%qhln    = ivars%qhln
     ovars%qvln    = ivars%qvln
     if(hydro_) then
       ovars%qivar   = ivars%qivar
       ovars%qihln   = ivars%qihln
       ovars%qivln   = ivars%qivln
       ovars%qlvar   = ivars%qlvar
       ovars%qlhln   = ivars%qlhln
       ovars%qlvln   = ivars%qlvln
       ovars%qrvar   = ivars%qrvar
       ovars%qrhln   = ivars%qrhln
       ovars%qrvln   = ivars%qrvln
       ovars%qsvar   = ivars%qsvar
       ovars%qshln   = ivars%qshln
       ovars%qsvln   = ivars%qsvln
     endif
     ovars%ozvar   = ivars%ozvar
     ovars%ozhln   = ivars%ozhln
     ovars%ozvln   = ivars%ozvln
     ovars%cvar    = ivars%cvar
     ovars%chln    = ivars%chln
     ovars%cvln    = ivars%cvln
  endif

  ovars%psvar   = ivars%psvar
  ovars%pshln   = ivars%pshln
  ovars%varsst  = ivars%varsst
  ovars%corlsst = ivars%corlsst

end subroutine copy_

subroutine get_pointer_1d_ (vname, bvars, ptr, rc )
implicit none
character(len=*), intent(in) :: vname
type(nc_berror_vars) bvars
real(4),pointer,intent(inout) :: ptr(:)
integer,intent(out) :: rc
rc=-1
if(trim(vname)=='ps') then
  ptr => bvars%psvar
  rc=0
endif
if(trim(vname)=='hps') then
  ptr => bvars%pshln
  rc=0
endif
end subroutine get_pointer_1d_

subroutine get_pointer_2d_ (vname, bvars, ptr, rc )
implicit none
character(len=*), intent(in) :: vname
type(nc_berror_vars) bvars
real(4),pointer,intent(inout) :: ptr(:,:)
integer,intent(out) :: rc
character(len=5) :: var
rc=-1
!
var='sst'
if(trim(vname)==trim(var)) then
  ptr => bvars%varsst
  rc=0
  return
endif
if(trim(vname)=='h'//trim(var)) then
  ptr => bvars%corlsst
  rc=0
  return
endif
!
var='sf'
if(trim(vname)==trim(var)) then
  ptr => bvars%sfvar
  rc=0
  return
endif
if(trim(vname)=='h'//trim(var)) then
  ptr => bvars%sfhln
  rc=0
  return
endif
if(trim(vname)=='v'//trim(var)) then
  ptr => bvars%sfvln
  rc=0
  return
endif
!
var='vp'
if(trim(vname)==trim(var)) then
  ptr => bvars%vpvar
  rc=0
  return
endif
if(trim(vname)=='h'//trim(var)) then
  ptr => bvars%vphln
  rc=0
  return
endif
if(trim(vname)=='v'//trim(var)) then
  ptr => bvars%vpvln
  rc=0
  return
endif
!
var='t'
if(trim(vname)==trim(var)) then
  ptr => bvars%tvar
  rc=0
  return
endif
if(trim(vname)=='h'//trim(var)) then
  ptr => bvars%thln
  rc=0
  return
endif
if(trim(vname)=='v'//trim(var)) then
  ptr => bvars%tvln
  rc=0
  return
endif
!
var='q'
if(trim(vname)==trim(var)) then
  ptr => bvars%qvar
  rc=0
  return
endif
if(trim(vname)=='h'//trim(var)) then
  ptr => bvars%qhln
  rc=0
  return
endif
if(trim(vname)=='v'//trim(var)) then
  ptr => bvars%qvln
  rc=0
  return
endif
!
var='cw'
if(trim(vname)==trim(var)) then
  ptr => bvars%cvar
  rc=0
  return
endif
if(trim(vname)=='h'//trim(var)) then
  ptr => bvars%chln
  rc=0
  return
endif
if(trim(vname)=='v'//trim(var)) then
  ptr => bvars%cvln
  rc=0
  return
endif
!
var='qi'
if(trim(vname)==trim(var)) then
  ptr => bvars%qivar
  rc=0
  return
endif
if(trim(vname)=='h'//trim(var)) then
  ptr => bvars%qihln
  rc=0
  return
endif
if(trim(vname)=='v'//trim(var)) then
  ptr => bvars%qivln
  rc=0
  return
endif
!
var='ql'
if(trim(vname)==trim(var)) then
  ptr => bvars%qlvar
  rc=0
  return
endif
if(trim(vname)=='h'//trim(var)) then
  ptr => bvars%qlhln
  rc=0
  return
endif
if(trim(vname)=='v'//trim(var)) then
  ptr => bvars%qlvln
  rc=0
  return
endif
!
var='qr'
if(trim(vname)==trim(var)) then
  ptr => bvars%qrvar
  rc=0
  return
endif
if(trim(vname)=='h'//trim(var)) then
  ptr => bvars%qrhln
  rc=0
  return
endif
if(trim(vname)=='v'//trim(var)) then
  ptr => bvars%qrvln
  rc=0
  return
endif
!
var='qs'
if(trim(vname)==trim(var)) then
  ptr => bvars%qsvar
  rc=0
  return
endif
if(trim(vname)=='h'//trim(var)) then
  ptr => bvars%qshln
  rc=0
  return
endif
if(trim(vname)=='v'//trim(var)) then
  ptr => bvars%qsvln
  rc=0
  return
endif
!
var='oz'
if(trim(vname)==trim(var)) then
  ptr => bvars%ozvar
  rc=0
  return
endif
if(trim(vname)=='h'//trim(var)) then
  ptr => bvars%ozhln
  rc=0
  return
endif
if(trim(vname)=='v'//trim(var)) then
  ptr => bvars%ozvln
  rc=0
  return
endif
!
var='nrh'
if(trim(vname)==trim(var)) then
  ptr => bvars%nrhvar
  rc=0
  return
endif
end subroutine get_pointer_2d_

subroutine check_(status,rc, myid, root)
    integer, intent ( in) :: status
    integer, intent (out) :: rc
    integer, intent ( in) :: myid, root
    rc=0
    if(status /= nf90_noerr) then 
      if(myid==root) print *, trim(nf90_strerror(status))
      rc=999
    end if
end subroutine check_  

end module m_nc_berror
