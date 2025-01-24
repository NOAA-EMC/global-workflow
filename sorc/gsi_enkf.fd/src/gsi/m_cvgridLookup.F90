
module m_cvgridLookup
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_cvgridLookup
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:      2015-08-18
!
! abstract: cv (control-vector) grid utilities for obs. partitioning
!
! program history log:
!   2015-08-18  j guo   - initial implementation, from m_obsNode.F90
!   2016-06-22  j guo   - added []_sdget() to support m_latlonRange
!                       . refined []_islocal() algorithm and related module
!                         variables, ilon_lbound, ilon_ubound, etc., to support
!                         regional/non-periodic subdomain grid types.
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

  use kinds, only: i_kind,r_kind
  use mpeu_util, only: tell,perr,die
  use mpeu_util, only: assert_
  use mpeu_util, only: stdout
  implicit none
  private       ! except
  public :: cvgridLookup_sdget        ! get subdomain parameters
  public :: cvgridLookup_islocal      ! a lat-lon pair is "local" on a PE.
  public :: cvgridLookup_isluse       ! a lat-lon pair is "luse" on a PE
  public :: cvgridLookup_getiw        ! get interpolation operator (index,weight)

  interface cvgridLookup_sdget  ; module procedure sdget_  ; end interface
  interface cvgridLookup_islocal; module procedure islocal_; end interface
                        !       ... = []_local(elat,elon,myPE)
  interface cvgridLookup_isluse ; module procedure isluse_ ; end interface
                        !       ... = []_luse(elat,elon,myPE)
  interface cvgridLookup_getiw; module procedure &
        get_Dij_ , &    !       call []_getiw(elat,elon,ij(1:4),wij(1:4) [, debugging arguments])
        get_Dijk_, &    !       call []_getiw(elat,elon,dlev,ijk(1:8),wijk(1:8) [, debugging arguments])
        get_Hij_ , &    !       call []_getiw(elat,elon,ij(1:4),wij(1:4))
        get_Hijk_       !       call []_getiw(elat,elon,dlev,ijk(1:8),wijk(1:8))
  end interface
        
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='m_cvgridLookup'

  logical,save:: isubdom_configured_=.false.
  integer(i_kind),allocatable,dimension(:),save:: ilatbox_lbound, ilatbox_ubound
  integer(i_kind),allocatable,dimension(:),save:: ilonbox_lbound, ilonbox_ubound

  real(r_kind),save:: sdlat_ref_ , sdlon_ref_     ! sub-domain reference in degree lat-lon
  real(r_kind),save:: sdlat_lbnd_, sdlon_lbnd_    ! sub-domain lower-left corner in degree lat-lon
  real(r_kind),save:: sdlat_ubnd_, sdlon_ubnd_    ! sub-domain upper-right corner in degree lat-lon

  real(r_kind),parameter:: DEG_IN_RAD = 4._r_kind*atan(1._r_kind)/180._r_kind
  real(r_kind),parameter:: RAD_IN_DEG = 1._r_kind/DEG_IN_RAD

  integer(i_kind),parameter:: iHalo = 1

#include "mytrace.H"
#include "myassert.H"
contains
!---- local routine: configure sub-domains of the control-vector grid.
subroutine isubdom_config_()
  use gridmod, only: nlon, rlons, jlon1, jstart !, periodic_s
  use gridmod, only: nlat, rlats, ilat1, istart
  use gridmod, only: regional
  use mpimod , only: nPEs => npe        ! number of PEs in GSI_comm_world
  use mpimod , only: myPE               ! my rank in GSI_comm_world
  implicit none

  character(len=*),parameter:: myname_=myname//"::isubdom_config_"
  integer(i_kind):: ipe,ip1
  integer(i_kind):: ilon,ilat
  !integer(i_kind):: mlat_lbound, mlat_ubound    ! lower/upper lat. boundaries of the local subdomain
  !integer(i_kind):: mlon_lbound, mlon_ubound    ! lower/upper lon. boundaries of the local subdomain

  real(r_kind):: r,rad_to_deg
  rad_to_deg(r) = r*RAD_IN_DEG

        ! The lower and the upper grid index boundaries of the subdomains of
        ! iHalo==1, are defined in the same way as they are in obs_para(),
        ! except that the array index are confined in 0:nPEs-1, instead of 1:nPEs.

  allocate(ilatbox_lbound(0:nPEs-1),ilatbox_ubound(0:nPEs-1))
  allocate(ilonbox_lbound(0:nPEs-1),ilonbox_ubound(0:nPEs-1))

        ! It was assumed in this isubdom_ module, the halo grid is +/-1 grid
        ! line.  The current implementation let a single iHalo be a configurable
        ! parameter for both latitude and longitude grids.  However, no test has
        ! been done for iHalo>1, which would need some changes to other part of
        ! the GSI to run.

  do ipe=0,nPEs-1

        ! However, the periodicity variable periodic_s(:) are not used here,
        ! since its true definition is unknown.  For example, they are undefined
        ! in at least one case of global periodic longitude grid.  Their
        ! definitions for some regional/non-periodic longitude grids (expected
        ! to be .false.?) are also suspecious.

    ip1=ipe+1

        ! latitude grid box index range of the local subdomain
    ilatbox_lbound(ipe) = istart(ip1)-iHalo             ! lower grid box boundary
    ilatbox_ubound(ipe) = istart(ip1)+ilat1(ip1)-1
    ilatbox_ubound(ipe) = ilatbox_ubound(ipe)-1 +iHalo  ! upper grid box boundary without halo

        ! longitude grid box index range of the local subdomain
    ilonbox_lbound(ipe) = jstart(ip1)-iHalo             ! lower grid box boundary
    ilonbox_ubound(ipe) = jstart(ip1)+jlon1(ip1)-1
    ilonbox_ubound(ipe) = ilonbox_ubound(ipe)-1 +iHalo  ! upper grid box boundary without halo
  enddo

  ip1=myPE+1
  sdlat_ref_   = rad_to_deg(rlats(istart(ip1)))
  sdlon_ref_   = rad_to_deg(rlons(jstart(ip1)))

        ! Map local grid box range into local grid point range
        ! First, for the local latitude grid, the lower-bound and the upper-bound

  ilat= ilatbox_lbound(myPE)            ! the lower grid point is the lower grid box
  ilat= min(max(1,ilat),nlat)           ! map it to a valid grid index
  sdlat_lbnd_= rad_to_deg(rlats(ilat))  ! get the grid value

  ilat= ilatbox_ubound(myPE)+1          ! the upper grid point is the outer edge of the upper grid box
  ilat= min(max(1,ilat),nlat)           ! map it to a valid grid index
  sdlat_ubnd_= rad_to_deg(rlats(ilat))  ! get the grid value

        ! Then, for the local longitude grid, the lower-bound and the upper-bound

  ilon=ilonbox_lbound(myPE)             ! the lower grid point is the lower grid box
  if(regional) then
    ilon=min(max(1,ilon),nlon)          ! map it to a valid non-periodic grid index
  else
    if(ilon<1) ilon= nlon+ilon          ! map it to a valid periodic grid index
  endif
  sdlon_lbnd_= rad_to_deg(rlons(ilon))  ! get the grid value

  ilon=ilonbox_ubound(myPE)+1           ! the upper grid point is the outer edge of the upper grid box
  if(regional) then
    ilon=min(max(1,ilon),nlon)          ! map it to a valid non-periodic grid index
  else
    if(ilon>nlon) ilon= ilon-nlon       ! map it to a valid periodic grid index
  endif
  sdlon_ubnd_= rad_to_deg(rlons(ilon))  ! get the grid value

  isubdom_configured_ = .true.
end subroutine isubdom_config_

!---- local routine: get the grid indices on the cv-grid.
subroutine isubdom_index_(elat,elon,ilat,ilon)
  use gridmod, only: nlat,rlats
  use gridmod, only: nlon,rlons
  use kinds, only: i_kind, r_kind
  use constants, only: deg2rad
  implicit none
  real(kind=r_kind),intent(in ):: elat,elon
  integer(kind=i_kind),intent(out):: ilat,ilon

  real(kind=r_kind):: dlat,dlon
  dlat=elat*deg2rad
  dlon=elon*deg2rad
  call grdcrd1(dlat,rlats,nlat,1)       ! returns dlat:=<i>.<w>
  call grdcrd1(dlon,rlons,nlon,1)       ! returns dlon:=<i>.<w>
  ilat=dlat     ! truncate out <w> to <i> only
  ilon=dlon     ! truncate out <w> to <i> only

        ! For non-periodic (regional) grid, given a precondition of
        ! elon in [rlons(1),rlons(nlon)), the returned value of index,
        ! floor(dlon), is in [1,nlon-1].
        !
        ! For periodic (global) grid, given a precondition of elon is in
        ! [rlons(1),rlons(1)+360.), assuming ascending, or rlons(1)<rlons(nlon),
        ! the returned value of index, floor(dlon), is in [1,nlon].

        ! These assertions are based on the understanding that every location
        ! should fall in one of grid boxes.  And all these grid boxes are
        ! indexed with (ilat,ilon) in [1:nlat,1:nlon], either periodic
        ! (longitudes) or not.

  ASSERT(ilat>=1)
  ASSERT(ilat<=nlat)

  ASSERT(ilon>=0)
  ASSERT(ilon<=nlon)

end subroutine isubdom_index_

!---- get subdomain boundaries
subroutine sdget_( sdlat_ref, sdlat_lbnd, sdlat_ubnd, &
                   sdlon_ref, sdlon_lbnd, sdlon_ubnd  )
  use kinds, only: r_kind
  implicit none
        ! subdomain grid reference, lower-bound, and upper-bound values
  real(r_kind),optional,intent(out):: sdlat_ref, sdlat_lbnd, sdlat_ubnd ! degree latitude
  real(r_kind),optional,intent(out):: sdlon_ref, sdlon_lbnd, sdlon_ubnd ! degree longitude

  if(.not.isubdom_configured_) call isubdom_config_()

  if(present(sdlat_ref )) sdlat_ref =sdlat_ref_
  if(present(sdlat_lbnd)) sdlat_lbnd=sdlat_lbnd_
  if(present(sdlat_ubnd)) sdlat_ubnd=sdlat_ubnd_

  if(present(sdlon_ref )) sdlon_ref =sdlon_ref_
  if(present(sdlon_lbnd)) sdlon_lbnd=sdlon_lbnd_
  if(present(sdlon_ubnd)) sdlon_ubnd=sdlon_ubnd_

end subroutine sdget_

!----- external routine: determine if (elat,elon) is local on the cv-grid
function islocal_(elat,elon,iPE)
  use gridmod, only: nlat
  use gridmod, only: nlon !,periodic_s
  use gridmod, only: regional
  implicit none
  logical :: islocal_
  real   (r_kind), intent(in):: elat,elon
  integer(i_kind), intent(in):: iPE

  integer(kind=i_kind):: ilat,ilon
_ENTRY_('isluse_')
  if(.not.isubdom_configured_) call isubdom_config_()
  call isubdom_index_(elat,elon,ilat,ilon)

  ASSERT(  allocated(ilonbox_lbound  ))
  ASSERT(iPE>=lbound(ilonbox_lbound,1))
  ASSERT(iPE<=ubound(ilonbox_lbound,1))

        ! The conditions defining "local" were original kept the same as they
        ! were in obs_para().  However, the way it uses periodic_s(:) was
        ! questionable, either for a global periodic grid or for a regional grid.
        !
        ! The current implementation avoids the use of periodic_s(:), and uses
        ! configuration variable _regional_ instead.

  ilat=min(max(1,ilat),nlat)    ! ilat==nlat should not happen.  See grdcrd() through isubdom_index_()
  islocal_ = ilat>=ilatbox_lbound(iPE) .and. ilat<=ilatbox_ubound(iPE)
  if(islocal_) then
    ilon=min(max(0,ilon),nlon)  ! ilon==0 should not happen.  See grdcrd() through isubdom_index_()
                                ! ilon==nlon should happend only for periodic grid
    islocal_ = ilon>=ilonbox_lbound(iPE) .and. ilon<=ilonbox_ubound(iPE)
    if(.not.islocal_) then
      if(.not.regional) then    ! periodicity applies
        islocal_ = ilon-nlon >= ilonbox_lbound(iPE) .or. & ! left to grid box 1
                   ilon+nlon <= ilonbox_ubound(iPE)        ! right to grid box nlon-1
      endif
    endif

    !-- This is the original algorithm as it is defined in obs_para() ---
    !islocal_ = ( ilon>=ilon_west(iPE) .and. ilon<=ilon_east(iPE) ) .or. &
    !           ( ilon==0    .and. ilon_east(iPE)>=nlon ) .or. &
    !           ( ilon==nlon .and. ilon_west(iPE)<=1    ) .or. ilon_periodic(iPE)
  endif
_EXIT_('isluse_')
end function islocal_

!----- external routine: determine if (elat,elon) is luse local on the cv-grid
function isluse_(elat,elon,mype) result(luse)
        ! a wrapper to use the subroutine as a function.
  implicit none
  logical:: luse
  real   (r_kind), intent(in ):: elat,elon
  integer(i_kind), intent(in ):: myPE
  call setluse_(luse,elat,elon,mype)
end function isluse_
subroutine setluse_(luse,elat,elon,mype)
  implicit none
  logical        , intent(out):: luse
  real   (r_kind), intent(in ):: elat,elon
  integer(i_kind), intent(in ):: myPE

  integer(kind=i_kind):: ipe
_ENTRY_('setluse_')
        ! Similar to what it was in obs_para() (and almost the same lately), for
        ! a given location (elat,elon), luse is set to .true., if and only if,
        !       islocal(myPE) .and. .not.any(islocal_(myPE-1:0:-1))

  luse=islocal_(elat,elon,mype)
        ! It would not be here, if it were not even islocal() on the current PE.
        ! So does this assertion.
  ASSERT(luse)

  do ipe=mype-1,0,-1
        ! On any PE of lower rank (iPE = mype-1, mype-2, ..., 0), if this ob
        ! islocal(), then it is .not. luse.
    if(islocal_(elat,elon,iPE)) then
      luse=.false.
      exit
    endif
  enddo
_EXIT_('setluse_')
end subroutine setluse_

subroutine get_Hij_(elat,elon,ij,wij)
  use gridmod, only: nlat,rlats
  use gridmod, only: nlon,rlons
  use gridmod, only: get_ij
  use constants, only: deg2rad
  use mpimod, only: myPE
  implicit none
  real(r_kind),intent(in):: elat,elon
  integer(i_kind),dimension(:),intent(inout)::  ij
  real   (r_kind),dimension(:),intent(inout):: wij

  real   (r_kind):: dlat,dlon
_ENTRY_('get_Hij_')
  dlat=elat*deg2rad
  dlon=elon*deg2rad
  call grdcrd1(dlat,rlats,nlat,1)       ! returns dlat:=<i>.<w>
  call grdcrd1(dlon,rlons,nlon,1)       ! returns dlon:=<i>.<w>
  call get_ij(myPE+1,dlat,dlon,ij(1:4),wij(1:4))
_EXIT_('get_Hij_')
end subroutine get_Hij_

subroutine get_Hijk_(elat,elon,dlev,ijk,wijk)
  use gridmod, only: nlat,rlats
  use gridmod, only: nlon,rlons
  use gridmod, only: get_ijk
  use constants, only: deg2rad
  use mpimod, only: myPE
  implicit none
  real   (r_kind),intent(in):: elat,elon,dlev
  integer(i_kind),dimension(:),intent(inout)::  ijk
  real   (r_kind),dimension(:),intent(inout):: wijk

  real   (r_kind):: dlat,dlon
_ENTRY_('get_Hijk_')
  dlat=elat*deg2rad
  dlon=elon*deg2rad
  call grdcrd1(dlat,rlats,nlat,1)       ! returns dlat:=<i>.<w>
  call grdcrd1(dlon,rlons,nlon,1)       ! returns dlon:=<i>.<w>
  call get_ijk(myPE+1,dlat,dlon,dlev,ijk(1:8),wijk(1:8))
_EXIT_('get_Hijk_')
end subroutine get_Hijk_

subroutine get_Dij_(elat,elon,dlat,dlon,ij,wij,jtype,asis)
  use gridmod, only: nlat,rlats
  use gridmod, only: nlon,rlons
  use gridmod, only: get_ij
  use constants, only: deg2rad
  use mpimod, only: myPE
  implicit none
  real(r_kind),intent(in):: elat,elon
  real(r_kind),intent(in):: dlat,dlon
  integer(i_kind),dimension(:),intent(inout)::  ij
  real   (r_kind),dimension(:),intent(inout):: wij
  integer(i_kind),intent(in)::  jtype
  logical,optional,intent(in):: asis

  real   (r_kind):: dlat_,dlon_
  integer(i_kind),dimension(4)::  ij_
  real   (r_kind),dimension(4):: wij_
!  integer(i_kind):: ilat,ilon
!  real   (r_kind):: wlat,wlon
  logical:: asis_
_ENTRY_('get_Dij_')
  asis_=.false.
  if(present(asis)) asis_=asis
  if(asis_) then
    _EXIT_('get_Dij_')
    return
  endif

  dlat_=elat*deg2rad
  dlon_=elon*deg2rad
  call grdcrd1(dlat_,rlats,nlat,1)       ! returns dlat:=<i>.<w>
  call grdcrd1(dlon_,rlons,nlon,1)       ! returns dlon:=<i>.<w>

   ij_(1:4)= ij(1:4)
  wij_(1:4)=wij_(1:4)
  call get_ij(myPE+1,dlat_,dlon_,ij(1:4),wij(1:4))
!  call get_ij(myPE+1,dlat_,dlon_,ij(1:4),wij(1:4), &
!        jjlat=ilat,jjlon=ilon,wwlat=wlat,wwlon=wlon,verbose=.true.)
  if(.not.all(ij_(1:4)==ij(1:4))) then
    call tell('get_Dij_','all( ij_(1:4)== ij(1:4)) =',all( ij_(1:4)== ij(1:4)))
    call tell('get_Dij_','all(wij_(1:4)==wij(1:4)) =',all(wij_(1:4)==wij(1:4)))
    call tell('get_Dij_','                   jtype =',jtype)
    call tell('get_Dij_','                    elat =',elat )
    call tell('get_Dij_','                    dlat =',dlat_)
    call tell('get_Dij_','                    dlat0=',dlat )
    call tell('get_Dij_','               dlat-dlat0=',dlat_-dlat)
    call tell('get_Dij_','                    elon =',elon )
    call tell('get_Dij_','                    dlon =',dlon_)
    call tell('get_Dij_','                    dlon0=',dlon )
    call tell('get_Dij_','               dlon-dlon0=',dlon_-dlon)
!    call tell('get_Dij_','                    ilat =',ilat)
!    call tell('get_Dij_','                    ilon =',ilon)
!    call tell('get_Dij_','                    wlat =',wlat)
!    call tell('get_Dij_','                    wlon =',wlon)
    write(stdout,'(a,8i8  )') 'get_Dij():  ij (1:4) =', ij (1:4)
    write(stdout,'(a,8i8  )') 'get_Dij():  ij~(1:4) =', ij_(1:4)
    write(stdout,'(a,8f8.4)') 'get_Dij(): wij (1:4) =',wij (1:4)
    write(stdout,'(a,8f8.4)') 'get_Dij(): wij~(1:4) =',wij_(1:4)
    call die('get_Dij_')
  endif
!  ASSERT(all( ij_(1:4)== ij(1:4)))
!  ASSERT(all(wij_(1:4)==wij(1:4)))
_EXIT_('get_Dij_')
end subroutine get_Dij_

subroutine get_Dijk_(elat,elon,dlat,dlon,dlev,ijk,wijk,jtype,asis)
  use gridmod, only: nlat,rlats
  use gridmod, only: nlon,rlons
  use gridmod, only: get_ijk
  use constants, only: deg2rad
  use mpimod, only: myPE
  implicit none
  real   (r_kind),intent(in):: elat,elon,dlev
  real   (r_kind),intent(in):: dlat,dlon
  integer(i_kind),dimension(:),intent(inout)::  ijk
  real   (r_kind),dimension(:),intent(inout):: wijk
  integer(i_kind) ,intent(in):: jtype
  logical,optional,intent(in):: asis

  integer(i_kind),dimension(8)::  ijk_
  real   (r_kind),dimension(8):: wijk_
  real   (r_kind):: dlat_,dlon_
  logical:: asis_
_ENTRY_('get_Dijk_')
  asis_=.false.
  if(present(asis)) asis_=asis
  if(asis_) then
    _EXIT_('get_Dijk_')
    return
  endif

  dlat_=elat*deg2rad
  dlon_=elon*deg2rad
  call grdcrd1(dlat_,rlats,nlat,1)       ! returns dlat:=<i>.<w>
  call grdcrd1(dlon_,rlons,nlon,1)       ! returns dlon:=<i>.<w>
   ijk_(1:8)= ijk(1:8)
  wijk_(1:8)=wijk(1:8)
  call get_ijk(myPE+1,dlat_,dlon_,dlev,ijk(1:8),wijk(1:8))
  if(.not.all(ijk_(1:8)==ijk(1:8))) then
    call tell('get_Dijk_','all( ijk_(1:8)== ijk(1:8)) =',all( ijk_(1:8)== ijk(1:8)))
    call tell('get_Dijk_','all(wijk_(1:8)==wijk(1:8)) =',all(wijk_(1:8)==wijk(1:8)))
    call tell('get_Dijk_','                   jtype =',jtype)
    call tell('get_DijK_','                    elat =',elat )
    call tell('get_Dijk_','                    dlat =',dlat_)
    call tell('get_Dijk_','                    dlat0=',dlat )
    call tell('get_Dijk_','               dlat-dlat0=',dlat_-dlat)
    call tell('get_DijK_','                    elon =',elon )
    call tell('get_Dijk_','                    dlon =',dlon_)
    call tell('get_Dijk_','                    dlon0=',dlon )
    call tell('get_Dijk_','               dlon-dlon0=',dlon_-dlon)
    call tell('get_Dijk_','                    dlev =',dlev )
    write(stdout,'(a,8i8  )') 'get_Dijk():  ijk (1:8) =', ijk (1:8)
    write(stdout,'(a,8i8  )') 'get_Dijk():  ijk~(1:8) =', ijk_(1:8)
    write(stdout,'(a,8f8.4)') 'get_Dijk(): wijk (1:8) =',wijk (1:8)
    write(stdout,'(a,8f8.4)') 'get_Dijk(): wijk~(1:8) =',wijk_(1:8)
    call die('get_Dijk_')
  endif
!  ASSERT(all( ijk_(1:8)/= 0))
!  ASSERT(all( ijk_(1:8)== ijk(1:8)))
!  ASSERT(all(wijk_(1:8)==wijk(1:8)))
_EXIT_('get_Dijk_')
end subroutine get_Dijk_

end module m_cvgridLookup
