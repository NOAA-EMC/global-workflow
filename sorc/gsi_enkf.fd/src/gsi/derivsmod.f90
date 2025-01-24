module derivsmod
!$$$   module documentation block
!                .      .    .                                       .
! module:    derivsmod
!
! abstract:  This module defines and holds bundles that contain 
!            guess derivative fields.
!
! program history log:
!   2013-10-19 Todling - Initial code.
!   2014-06-18 Carley - add lgues and dlcbasdlog
!   2015-07-10 Pondeca - add cldchgues and dcldchdlog
!   2016-05-10 Thomas - remove references to cwgues0
!   2019-05-08 mtong - replace set_ with init_anadv 
!   2019-05-08 eliu - recover logic (drv_set_) to indicate the derivative
!                     vars are allocated and defined
!
! public subroutines:
!  drv_initialized         - initialize name of fields to calc derivs for
!  create_ges_derivatives  - initialize bundles holding derivatives
!  destroy_ges_derivatives - finalize bundles holding derivatives

! public variables:
!  gsi_xderivative_bundle  - bundle holding longitudinal derivatives
!  gsi_yderivative_bundle  - bundle holding latitudinal derivatives
!  dvars2d, dvars3d        - names of 2d/3d derivatives
!  dsrcs2d, dsrcs3d        - names of where original fields reside
!  drv_initialized         - flag indicating initialization status
!  drv_set_                - flag indicating the variables are allocated and defined 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

use kinds, only: i_kind, r_kind
use mpimod, only: mype
use gridmod, only: lat2,lon2,nsig
use constants, only: zero,max_varname_length
use state_vectors, only: svars2d,svars3d
use GSI_BundleMod, only : GSI_BundleCreate
use GSI_BundleMod, only : GSI_Bundle
use GSI_BundleMod, only : GSI_BundleGetPointer
use GSI_BundleMod, only : GSI_BundleDestroy
use GSI_BundleMod, only : GSI_BundleUnset

use GSI_BundleMod, only : GSI_Grid
use GSI_BundleMod, only : GSI_GridCreate

use GSI_MetGuess_Mod, only: gsi_metguess_bundle
use GSI_ChemGuess_Mod, only: gsi_chemguess_bundle

use mpeu_util, only: getindex
implicit none
save
private

public :: drv_initialized
public :: drv_set_         
public :: create_ges_derivatives
public :: destroy_ges_derivatives

public :: gsi_xderivative_bundle
public :: gsi_yderivative_bundle
public :: dvars2d, dvars3d
public :: dsrcs2d, dsrcs3d
public :: cwgues,cfgues 
public :: ggues,vgues,pgues,lgues,dvisdlog,dlcbasdlog
public :: w10mgues,howvgues,cldchgues,dcldchdlog
public :: qsatg,qgues,dqdt,dqdrh,dqdp
public :: init_anadv

logical :: drv_initialized = .false.

type(gsi_bundle),pointer :: gsi_xderivative_bundle(:)
type(gsi_bundle),pointer :: gsi_yderivative_bundle(:)
character(len=max_varname_length),allocatable,dimension(:):: dvars2d, dvars3d
character(len=max_varname_length),allocatable,dimension(:):: dsrcs2d, dsrcs3d

real(r_kind),allocatable,dimension(:,:,:):: qsatg,qgues,dqdt,dqdrh,dqdp
real(r_kind),allocatable,dimension(:,:):: ggues,vgues,pgues,lgues,dvisdlog,dlcbasdlog
real(r_kind),allocatable,dimension(:,:):: w10mgues,howvgues,cldchgues,dcldchdlog
real(r_kind),target,allocatable,dimension(:,:,:):: cwgues,cfgues 

! below this point: declare vars not to be made public

character(len=*),parameter:: myname='derivsmod'
logical,save :: drv_set_=.false.  
integer(i_kind),allocatable,dimension(:):: levels
contains

subroutine init_anadv
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 define derivatives
!   prgmmr:	 todling
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.1
!     date:	 2013-10-19
!
! abstract: - set which derivatives to calculate from guess fields
!
! program history log:
!   2013-09-27  todling  - initial code
!   2014-02-03  todling  - negative levels mean rank-3 array
!   2019-05-08  mtong    - replace set_ with init_anadv 
!   2019-05-08  eliu     - recover logic (drv_set_) to indicate the derivative
!                          vars are allocated and defined
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
use mpeu_util, only: gettablesize
use mpeu_util, only: gettable
use mpeu_util, only: getindex
implicit none

character(len=*),parameter:: rcname='anavinfo'

character(len=*),parameter::myname_=myname//'*set_'
character(len=*),parameter:: tbname='state_derivatives::'
integer(i_kind) luin,ii,nrows,ntot,ipnt,istatus
integer(i_kind) i2d,i3d,n2d,n3d,irank
integer(i_kind),allocatable,dimension(:)::nlevs
character(len=256),allocatable,dimension(:):: utable
character(len=max_varname_length),allocatable,dimension(:):: vars
character(len=max_varname_length),allocatable,dimension(:):: sources
logical matched

if(drv_set_) return 

open(newunit=luin,file=trim(rcname),form='formatted')

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
                      nlevs(ii),& ! number of levels
                      sources(ii) ! source
   if (nlevs(ii)==1) then
      n2d=n2d+1
   else
      n3d=n3d+1
   endif
enddo

deallocate(utable)

allocate(dvars2d(n2d),dvars3d(n3d),&
         dsrcs2d(n2d),dsrcs3d(n3d),levels(n3d))

! loop over variables and identify them by comparison
i2d=0; i3d=0
do ii=1,nrows
   matched=.false.
   if(trim(sources(ii))=='met_guess') then
      if(associated(gsi_metguess_bundle)) then
         call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(vars(ii)),ipnt,istatus,irank=irank);
         if (ipnt>0) then
            if(irank==2) then 
               i2d=i2d+1
               dvars2d(i2d)=trim(vars(ii))
               dsrcs2d(i2d)=trim(sources(ii))
               matched=.true.
            endif
            if(irank==3) then 
               i3d=i3d+1
               dvars3d(i3d)=trim(vars(ii))
               dsrcs3d(i3d)=trim(sources(ii))
               levels(i3d) =abs(nlevs(ii))
               matched=.true.
            endif
         endif
      endif
   endif
   if(trim(sources(ii))=='chem_guess') then
      if(associated(gsi_chemguess_bundle)) then
         call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(vars(ii)),ipnt,istatus,irank=irank);
         if (ipnt>0) then
            if(irank==2) then
               i2d=i2d+1
               dvars2d(i2d)=trim(vars(ii))
               dsrcs2d(i2d)=trim(sources(ii))
               matched=.true.
            endif
            if(irank==3) then 
               i3d=i3d+1
               dvars3d(i3d)=trim(vars(ii))
               dsrcs3d(i3d)=trim(sources(ii))
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
         dvars2d(i2d)=trim(vars(ii))
         dsrcs2d(i2d)='derived'
      else
         i3d=i3d+1
         dvars3d(i3d)=trim(vars(ii))
         dsrcs3d(i3d)='derived'
         levels(i3d) =abs(nlevs(ii))
      endif
   endif
enddo

if (mype == 0) then
    write(6,*) myname_,':  DERIVATIVE VARIABLES: '
    write(6,*) myname_,':  2D-DERV STATE VARIABLES: '
    do ii=1,n2d
       write(6,*) trim(dvars2d(ii))
    enddo
    write(6,*) myname_,':  3D-DERV STATE VARIABLES:'
    do ii=1,n3d
       write(6,*) trim(dvars3d(ii))
    enddo
end if

deallocate(vars,nlevs,sources)
drv_set_=.true.  

 end subroutine init_anadv

 subroutine create_ges_derivatives(switch_on_derivatives,nfldsig)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 create derivatives
!   prgmmr:	 todling
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.1
!     date:	 2013-10-19
!
! abstract: - allocate space for bundle-kept derivatives
!
! program history log:
!   2013-09-27  todling  - initial code
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
 implicit none

 logical, intent(in) :: switch_on_derivatives
 integer(i_kind),intent(in) :: nfldsig

 integer nt,ierror
 character(len=32) bname
 type(gsi_grid) :: grid

! Extra mambo-jambo
  call create_auxiliar_

  if (.not.switch_on_derivatives) return
  if (drv_initialized) return 

! create derivative grid
  call GSI_GridCreate(grid,lat2,lon2,nsig)

! allocate structures
  allocate(gsi_xderivative_bundle(nfldsig))
  allocate(gsi_yderivative_bundle(nfldsig))

! Note: 
!   Original code needed ps derivatives in all time slots
!   Present code creates all derivatives in all time slots
  do nt=1,nfldsig

!    create logitudinal derivative bundle
     write(bname,'(a,i3.3)') 'Lon Derivative Vector-',nt
     call GSI_BundleCreate(gsi_xderivative_bundle(nt),grid,bname,ierror, &
                           names2d=dvars2d,names3d=dvars3d,levels=levels,bundle_kind=r_kind)

!    create latidutinal derivative bundle
     write(bname,'(a,i3.3)') 'Lat Derivative Vector-',nt
     call GSI_BundleCreate(gsi_yderivative_bundle(nt),grid,bname,ierror, &
                           names2d=dvars2d,names3d=dvars3d,levels=levels,bundle_kind=r_kind)

  enddo

  drv_initialized = .true.

  if(mype==0) write(6,*) 'create_ges_derivatives: successfully complete'
  end subroutine create_ges_derivatives

  subroutine destroy_ges_derivatives
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 destroy derivatives
!   prgmmr:	 todling
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.1
!     date:	 2013-10-19
!
! abstract: - deallocates bundles keeping derivatives
!
! program history log:
!   2013-09-27  todling  - initial code
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
  use mpimod, only: mpi_comm_world
  implicit none
  integer(i_kind) nt,ierror

  if(.not.drv_initialized) return

! destroy mambo-jambo
  call destroy_auxiliar_

! destroy each instance of derivatives
  do nt=1,size(gsi_yderivative_bundle)

!    create logitudinal derivative bundle
     call GSI_BundleDestroy(gsi_yderivative_bundle(nt),ierror)
     if(ierror/=0) then
        if(mype==0) write(6,*)'warning: y-derivative not properly destroyed'
     endif

!    create latidutinal derivative bundle
     call GSI_BundleDestroy(gsi_xderivative_bundle(nt),ierror)
     if(ierror/=0) then
        if(mype==0) write(6,*)'warning: x-derivative not properly destroyed'
     endif

  enddo

! deallocate structures
  deallocate(gsi_xderivative_bundle)
  deallocate(gsi_yderivative_bundle)

! destroy derivative grid
! call GSI_GridDestroy(grid,lat2,lon2,nsig)

  deallocate(dvars2d,dvars3d,&
             dsrcs2d,dsrcs3d,levels)

  if(mype==0) write(6,*) 'destroy_ges_derivatives: successfully complete'
  end subroutine destroy_ges_derivatives

  subroutine create_auxiliar_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_auxiliar_
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: allocate memory for cost function variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2004-07-28  treadon - simplify subroutine argument list
!   2005-03-28  wu - replace mlath with mlat, modify dim of varq 
!   2005-06-15  treadon - remove "use guess_grids"
!   2008-05-12  safford - rm unused uses
!   2011-02-16  zhu     - add ggues,vgues,pgues
!   2011-07-15  zhu     - add cwgues
!   2013-10-25  todling - revisit variable initialization
!   2013-11-12  lueken - revisit logic around cwgues
!   2014-02-03  todling - CV length and B-dims here (no longer in observer)
!   2014-03-19  pondeca - add w10mgues
!   2014-05-07  pondeca - add howvgues
!   2014-06-18  carley - add lgues and dlcbasdlog
!   2015-07-10  pondeca- add cldchgues and dcldchdlog
!
!   input argument list:
!    mlat
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: zero
    use gridmod, only: lat2,lon2,nsig
    implicit none

    integer(i_kind) i,j,k

    if (getindex(svars3d,'q')>0) then
       allocate(qsatg(lat2,lon2,nsig),&
            dqdt(lat2,lon2,nsig),dqdrh(lat2,lon2,nsig),&
            dqdp(lat2,lon2,nsig),&
            qgues(lat2,lon2,nsig))

       do k=1,nsig
          do j=1,lon2
             do i=1,lat2
                qsatg(i,j,k)=zero
                dqdt(i,j,k)=zero
                dqdrh(i,j,k)=zero
                dqdp(i,j,k)=zero
                qgues(i,j,k)=zero
             end do
          end do
       end do
    endif

    allocate(cwgues(lat2,lon2,nsig))
    do k=1,nsig
       do j=1,lon2
          do i=1,lat2
             cwgues(i,j,k)=zero
          end do
        end do
    end do

    allocate(cfgues(lat2,lon2,nsig))
    do k=1,nsig
       do j=1,lon2
          do i=1,lat2
             cfgues(i,j,k)=zero
          end do
        end do
    end do

    if (getindex(svars2d,'gust')>0) then
       allocate(ggues(lat2,lon2))
       do j=1,lon2
          do i=1,lat2
             ggues(i,j)=zero
          end do
       end do
    end if
    if (getindex(svars2d,'vis')>0) then
       allocate(vgues(lat2,lon2),dvisdlog(lat2,lon2))
       do j=1,lon2
          do i=1,lat2
             vgues(i,j)=zero
             dvisdlog(i,j)=zero
          end do
       end do
    end if
    if (getindex(svars2d,'pblh')>0) then
       allocate(pgues(lat2,lon2))
       do j=1,lon2
          do i=1,lat2
             pgues(i,j)=zero
          end do
       end do
    end if
    if (getindex(svars2d,'lcbas')>0) then
       allocate(lgues(lat2,lon2),dlcbasdlog(lat2,lon2))
       do j=1,lon2
          do i=1,lat2
             lgues(i,j)=zero
             dlcbasdlog(i,j)=zero
          end do
       end do
    end if
    if (getindex(svars2d,'wspd10m')>0) then
       allocate(w10mgues(lat2,lon2))
       do j=1,lon2
          do i=1,lat2
             w10mgues(i,j)=zero
          end do
       end do
    end if
    if (getindex(svars2d,'howv')>0) then
       allocate(howvgues(lat2,lon2))
       do j=1,lon2
          do i=1,lat2
             howvgues(i,j)=zero
          end do
       end do
    end if
    if (getindex(svars2d,'cldch')>0) then
       allocate(cldchgues(lat2,lon2),dcldchdlog(lat2,lon2))
       do j=1,lon2
          do i=1,lat2
             cldchgues(i,j)=zero
             dcldchdlog(i,j)=zero
          end do
       end do
    end if


    return
  end subroutine create_auxiliar_
    
  subroutine destroy_auxiliar_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_auxliar_
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: deallocate memory from cost function variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2011-02-16  zhu     - add ggues,vgues,pgues
!   2011-07-15  zhu     - add cwgues
!   2013-10-25  todling, revisit deallocs
!   2014-03-19  pondeca - add w10mgues
!   2014-05-07  pondeca - add howvgues
!   2014-06-18  carley - add lgues and dlcbasdlog 
!   2015-07-10  pondeca- add cldchgues and dcldchdlog
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    if(allocated(dqdt)) deallocate(dqdt)
    if(allocated(dqdrh)) deallocate(dqdrh)
    if(allocated(dqdp)) deallocate(dqdp)
    if(allocated(qsatg)) deallocate(qsatg)
    if(allocated(qgues)) deallocate(qgues)
    if(allocated(cwgues)) deallocate(cwgues)
    if(allocated(cfgues)) deallocate(cfgues) 
    if(allocated(ggues)) deallocate(ggues)
    if(allocated(vgues)) deallocate(vgues)
    if(allocated(dvisdlog)) deallocate(dvisdlog)
    if(allocated(pgues)) deallocate(pgues)
    if(allocated(lgues)) deallocate(lgues)
    if(allocated(dlcbasdlog)) deallocate(dlcbasdlog)
    if(allocated(w10mgues)) deallocate(w10mgues)
    if(allocated(howvgues)) deallocate(howvgues)
    if(allocated(cldchgues)) deallocate(cldchgues)
    if(allocated(dcldchdlog)) deallocate(dcldchdlog)

    return
  end subroutine destroy_auxiliar_
end module derivsmod
