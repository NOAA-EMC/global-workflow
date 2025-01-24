module tendsmod
!$$$   module documentation block
!                .      .    .                                       .
! module:    tendsmod           contains stuff for time tendencies
!
!   prgmmr: kleist           org: np20                date: 2005-10-28
!
! abstract:  contains routines and variables for time tendency 
!            calculations
!
! program history log:
!   2005-10-28  kleist
!   2006-02-24  kleist - additions for divergence and ageostrophic vorticity tendencies
!   2006-12-15  todling - protection against over-initizing
!   2007-05-08  kleist - add arrays for generalized coordinate
!   2009-08-20  parrish - replace curvfct with curvx, curvy.  this allows tendency computation to
!                          work for any general orthogonal coordinate.
!   2013-10-19  todling - considerable revision of tendencies - now in bundle
!
! subroutines included:
!   sub create_tendvars        - allocate load Jc related variables
!   sub destroy_tendvars       - deallocate Jc related arrays
!
! Variable Definitions:
!   def what9                - basic state value vertical velocity
!   def prsth9               - basic state value for horizontal component of pressure tendency
!   def r_prsum9             - basic state value of 1/(p(k)+p(k+1))
!   def prdif9               - basic state value of p(k)-p(k+1)
!   def r_prdif9             - basic state value of 1/(p(k)-p(k+1))
!   def pr_xsum9             - basic state value of p_x(k)+p_x(k+1)
!   def pr_xdif9             - basic state value of p_x(k)-p_x(k+1)
!   def pr_ysum9             - basic state value of p_y(k)+p_y(k+1)
!   def pr_ydif9             - basic state value of p_y(k)-p_y(k+1)
!   def curvx,curvy          - factors for curvature terms in wind tendencies
!   def coriolis             - coriolis parameter
!   def t_over_pbar          - horizontal mean Tv/p for calculation of mass variable tendency
!   def dp_over_pbar         - horizontal mean dp/p for calculation of mass variable tendency
!   def factk9               - basic state factor used in vertical velocity term
!   def adiag9               - basic state a-diagonal term used for vertical velocity
!   def bdiag9               - basic state b-diagonal term used for vertical velocity
!   def cdiag9               - basic state c-diagonal term used for vertical velocity
!   def r_bdiag9             - reciprocol of basic state b-diagonal term used for vertical velocity
!   def wint9                - intermediate basic state variable use for vertical velocity
!   def wint9_f              - intermediate basic state variable use for vertical velocity
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind
  use constants, only: max_varname_length
  use gridmod, only: lat2,lon2,nsig
  use mpimod, only : mype
  use GSI_BundleMod, only : GSI_BundleCreate
  use GSI_BundleMod, only : GSI_Bundle
  use GSI_BundleMod, only : GSI_BundleGetPointer
  use GSI_BundleMod, only : GSI_BundleDestroy
  use GSI_BundleMod, only : GSI_BundleUnset

  use GSI_BundleMod, only : GSI_Grid
  use GSI_BundleMod, only : GSI_GridCreate

  use GSI_MetGuess_Mod, only: gsi_metguess_bundle
  use GSI_ChemGuess_Mod, only: gsi_chemguess_bundle
  implicit none

! set default to private
  private
  save
! set subroutiens to public
  public :: create_ges_tendencies
  public :: destroy_ges_tendencies
  public :: tnd_initialized
  public :: gsi_tendency_bundle
! set passed variables to public
  public :: pr_ydif9,pr_ysum9,pr_xdif9,coriolis,curvy,curvx,r_prsum9,prsth9
  public :: what9,pr_xsum9,prdif9,r_prdif9,wint9,wint9_f,r_bdiag9,factk9
  public :: adiag9,bdiag9,cdiag9,tlm0,stph0,ctph0

  logical,save :: tnd_initialized = .false.

  real(r_kind),allocatable,dimension(:,:,:):: what9,prsth9,r_prsum9,prdif9,r_prdif9,&
     pr_xsum9,pr_xdif9,pr_ysum9,pr_ydif9
  real(r_kind),allocatable,dimension(:,:):: curvx,curvy,coriolis
  real(r_kind),allocatable,dimension(:,:,:):: factk9,adiag9,bdiag9,cdiag9,wint9,wint9_f,&
       r_bdiag9
  real(r_kind),allocatable,dimension(:):: t_over_pbar,dp_over_pbar
  real(r_kind) ctph0,stph0,tlm0

  type(gsi_bundle) :: gsi_tendency_bundle
  character(len=max_varname_length),allocatable,dimension(:):: tvars2d, tvars3d
  character(len=max_varname_length),allocatable,dimension(:):: tsrcs2d, tsrcs3d

! below this point: declare vars not to be made public
  logical,save :: tnd_set_        = .false.
  integer(i_kind),allocatable,dimension(:):: levels

  character(len=*),parameter:: myname='tendsmod'
contains

subroutine set_ (iamroot,rcname)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 define tendencies
!   prgmmr:	 todling
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.1
!     date:	 2013-02-04
!
! abstract: - set which guess fields should code calculate tendencies for
!
! program history log:
!   2013-09-27  todling  - initial code
!   2014-02-03  todling  - negative levels entry means rank-3 array
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
use file_utility, only : get_lun
use mpeu_util, only: gettablesize
use mpeu_util, only: gettable
use mpeu_util, only: getindex
implicit none

logical,optional,intent(in) :: iamroot         ! optional root processor id
character(len=*),optional,intent(in) :: rcname ! optional input filename

character(len=*),parameter::myname_=myname//'*set_'
character(len=*),parameter:: tbname='state_tendencies::'
integer(i_kind) luin,ii,nrows,ntot,ipnt,istatus
integer(i_kind) i2d,i3d,n2d,n3d,irank
integer(i_kind),allocatable,dimension(:)::nlevs
character(len=256),allocatable,dimension(:):: utable
character(len=max_varname_length),allocatable,dimension(:):: vars
character(len=max_varname_length),allocatable,dimension(:):: sources
logical iamroot_,matched

if(tnd_set_) return

iamroot_=mype==0
if(present(iamroot)) iamroot_=iamroot 

! load file
if (present(rcname)) then
   luin=get_lun()
   open(luin,file=trim(rcname),form='formatted')
else
   luin=5
endif

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,nrows)
if(nrows==0) then
   if(luin/=5) close(luin)
   return
endif

! Get contents of table
allocate(utable(nrows))
call gettable(tbname,luin,ntot,nrows,utable)

! release file unit
if(luin/=5) close(luin)

! allocate space for entries from table
allocate(vars(nrows),nlevs(nrows),sources(nrows))

! Retrieve each token of interest from table and define
! variables participating in state vector
n2d=0; n3d=0
do ii=1,nrows
   read(utable(ii),*) vars(ii),&  ! variable name
                      nlevs(ii), &! number of levels
                      sources(ii) ! source
   if (nlevs(ii)==1) then
      n2d=n2d+1
   else
      n3d=n3d+1
   endif
enddo

deallocate(utable)

if(n2d >0)allocate(tvars2d(n2d),tsrcs2d(n2d))
if(n3d >0)allocate(tvars3d(n3d),tsrcs3d(n3d),levels(n3d))

! loop over variables and identify them by comparison
i2d=0; i3d=0
do ii=1,nrows
   matched=.false.
   if(trim(sources(ii))=='met_guess') then
      if(associated(gsi_metguess_bundle)) then
         call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(vars(ii)),ipnt,istatus,irank=irank)
         if (ipnt>0) then
            if(irank==2) then 
               i2d=i2d+1
               tvars2d(i2d)=trim(vars(ii))
               tsrcs2d(i2d)=trim(sources(ii))
               matched=.true.
            endif
            if(irank==3) then 
               i3d=i3d+1
               tvars3d(i3d)=trim(vars(ii))
               tsrcs3d(i3d)=trim(sources(ii))
               levels(i3d) =abs(nlevs(ii))
               matched=.true.
            endif
         endif
      endif
   endif
   if(trim(sources(ii))=='chem_guess') then
      if(associated(gsi_chemguess_bundle)) then
         call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(vars(ii)),ipnt,istatus,irank=irank)
         if (ipnt>0) then
            if(irank==2) then
               i2d=i2d+1
               tvars2d(i2d)=trim(vars(ii))
               tsrcs2d(i2d)=trim(sources(ii))
               matched=.true.
            endif
            if(irank==3) then 
               i3d=i3d+1
               tvars3d(i3d)=trim(vars(ii))
               tsrcs3d(i3d)=trim(sources(ii))
               levels(i3d) =abs(nlevs(ii))
               matched=.true.
            endif
         endif
      endif
   endif
   ! now care for variables not in guess (usually, derived tendencies)
   if (.not.matched) then
      if(nlevs(ii)==1) then
         i2d=i2d+1
         tvars2d(i2d)=trim(vars(ii))
         tsrcs2d(i2d)='derived'
      else
         i3d=i3d+1
         tvars3d(i3d)=trim(vars(ii))
         tsrcs3d(i3d)='derived'
         levels(i3d) =abs(nlevs(ii))
      endif
   endif
enddo

if (iamroot_) then
    write(6,*) myname_,':  TENDENCY VARIABLES: '
    write(6,*) myname_,':  2D-TEND STATE VARIABLES: '
    do ii=1,n2d
       write(6,*) trim(tvars2d(ii))
    enddo
    write(6,*) myname_,':  3D-TEND STATE VARIABLES:'
    do ii=1,n3d
       write(6,*) trim(tvars3d(ii))
    enddo
end if

deallocate(vars,nlevs,sources)
tnd_set_=.true.

end subroutine set_

subroutine create_tendvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_tendvars     allocate d/dt related arrays
!   prgmmr: kleist          org: np20                date: 2005-10-27
!
! abstract: allocate dynamic constraint term arrays
!
! program history log:
!   2005-10-27  kleist
!   2006-02-24  kleist, new arrays for mass variable tendency
!   2006-12-15  todling, protection against over-initizing
!   2008-04-03  safford - rm unused vars and uses
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    implicit none   

    allocate( what9(lat2,lon2,nsig+1),prsth9(lat2,lon2,nsig+1),&
             r_prsum9(lat2,lon2,nsig),prdif9(lat2,lon2,nsig),&
             r_prdif9(lat2,lon2,nsig),&
             pr_xsum9(lat2,lon2,nsig),pr_xdif9(lat2,lon2,nsig),&
             pr_ysum9(lat2,lon2,nsig),pr_ydif9(lat2,lon2,nsig) )
    allocate(factk9(lat2,lon2,nsig),adiag9(lat2,lon2,nsig),&
             bdiag9(lat2,lon2,nsig),cdiag9(lat2,lon2,nsig),&
             r_bdiag9(lat2,lon2,nsig),&
             wint9_f(lat2,lon2,nsig+1),wint9(lat2,lon2,nsig+1))
    allocate(coriolis(lat2,lon2),curvx(lat2,lon2),curvy(lat2,lon2))

    allocate(t_over_pbar(nsig),dp_over_pbar(nsig))

    return
end subroutine create_tendvars

subroutine create_ges_tendencies(tendsflag)
  implicit none
  logical,intent(in) :: tendsflag

  character(len=32) bname
  integer(i_kind) ierror
  type(gsi_grid) :: grid

  if(.not.tendsflag) return
  if(tnd_initialized) return

! set variables in tendency bundle
  call set_(rcname='anavinfo')

! create tendency bundle
  call GSI_GridCreate(grid,lat2,lon2,nsig)
  write(bname,'(a)') 'Tendency Vector'
  if (allocated(tvars2d) .and. allocated(tvars3d) ) then
      call GSI_BundleCreate(gsi_tendency_bundle,grid,bname,ierror, &
                            names2d=tvars2d,names3d=tvars3d,levels=levels,bundle_kind=r_kind)
  else if (allocated(tvars2d)) then
      call GSI_BundleCreate(gsi_tendency_bundle,grid,bname,ierror, &
                            names2d=tvars2d,bundle_kind=r_kind)
  else if (allocated(tvars3d)) then
      call GSI_BundleCreate(gsi_tendency_bundle,grid,bname,ierror, &
                            names3d=tvars3d,levels=levels,bundle_kind=r_kind)
  else
       call stop2(999) ! should never get here
  endif

! create wired-in fields
  call create_tendvars

  tnd_initialized = .true.

  if(mype==0) write(6,*) 'create_ges_tendencies: successfully complete'
end subroutine create_ges_tendencies

subroutine destroy_tendvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_tendvars     deallocate d/dt related arrays
!   prgmmr: kleist          org: np20                date: 2005-10-27
!
! abstract: deallocate tendency arrays
!
! program history log:
!   2005-10-27  kleist
!   2006-02-24  kleist, new variables for mass variable tendency
!   2006-12-15  todling, protection against over-initizing
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    implicit none

    deallocate(what9,prsth9,r_prsum9,r_prdif9,prdif9,pr_xsum9,&
       pr_xdif9,pr_ysum9,pr_ydif9)
    deallocate(factk9,adiag9,bdiag9,r_bdiag9,cdiag9,wint9_f,wint9)
    deallocate(curvx,curvy,coriolis)
    deallocate(t_over_pbar,dp_over_pbar)

    return
end subroutine destroy_tendvars

subroutine destroy_ges_tendencies
  use mpimod, only: mpi_comm_world
  implicit none
  integer(i_kind) ierror

  if(.not.tnd_initialized) return

  call destroy_tendvars

  call GSI_BundleDestroy(gsi_tendency_bundle,ierror)
  if(ierror/=0) then
     if(mype==0) write(6,*)'destroy_ges_tendencies warning: vector not allocated'
  endif

  if(allocated(tvars2d))deallocate(tvars2d)
  if(allocated(tvars3d))deallocate(tvars3d)
  if(allocated(tsrcs2d))deallocate(tsrcs2d)
  if(allocated(tsrcs3d))deallocate(tsrcs3d)
  if(allocated(levels))deallocate(levels)

  tnd_initialized = .false.
  if(mype==0) write(6,*) 'destroy_ges_tendencies: successfully complete'
end subroutine destroy_ges_tendencies

end module tendsmod
