module statevec
!$$$  module documentation block
!
! module: statevec            read ensemble members, write out
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: ensemble IO.
!
! Public Subroutines:
!  init_statevec: read anavinfo table
!  read_state: read ensemble members on IO tasks
!  statevec_cleanup: deallocate allocatable arrays.
!
! Public Variables:
!  nanals: (integer scalar) number of ensemble members (from module params)
!  nlevs: number of analysis atmos vertical levels (from module params).
!  ns3d: number of 3D variables
!  ns2d: number of 2D variables
!  svars3d: names of 3D variables
!  svars2d: names of 2D variables
!  nsdim: total number of 2D fields to update (ns3d*nlevs+ns2d)
!  nstatefields:  number of time levels in background
!  state_d: ensemble perturbations
!   
! Modules Used: mpisetup, params, kinds, loadbal, gridio, gridinfo
!
! program history log:
!   2009-02-23  Initial version.
!   2009-11-28  revamped to improve IO speed
!   2015-06-29  add multiple time levels to background
!   2016-05-02  shlyaeva: Modification for reading state vector from table
!   2016-09-07  shlyaeva: moved distribution of ens members to loadbal
!   2016-11-29  shlyaeva: separated controlvec from statevec; gridinfo init and
!               cleanup are called from here now
!
! attributes:
!   language: f95
!
!$$$

use gridio, only: readgriddata, readgriddata_pnc
use mpisetup, only: mpi_real4,mpi_sum,mpi_comm_io,mpi_in_place,numproc,nproc
use mpimod, only: mpi_comm_world
use gridinfo, only: getgridinfo, gridinfo_cleanup,               &
                    npts, vars3d_supported, vars2d_supported
use params, only: nlevs,nstatefields,nanals,statefileprefixes,&
                  ntasks_io,nanals_per_iotask,nanal1,nanal2, &
                  statesfcfileprefixes, paranc
use kinds, only: r_kind, i_kind, r_double, r_single
use mpeu_util, only: gettablesize, gettable, getindex
use constants, only : max_varname_length
implicit none
private
public :: read_state, statevec_cleanup, init_statevec
real(r_single),public, allocatable, dimension(:,:,:,:) :: state_d

integer(i_kind), public :: ns2d, ns3d, nsdim
character(len=max_varname_length), allocatable, dimension(:), public :: svars3d
character(len=max_varname_length), allocatable, dimension(:), public :: svars2d
integer(i_kind), allocatable, dimension(:), public                   :: slevels

contains

subroutine init_statevec()
! read table with state vector variables
! (code adapted from GSI state_vectors.f90 init_anasv routine
!  by Anna Shlyaeva, April 18, 2016)
implicit none
character(len=*),parameter:: rcname='anavinfo'
character(len=*),parameter:: tbname='state_vector::'
character(len=256),allocatable,dimension(:):: utable
character(len=20) var,source,funcof
integer(i_kind) luin,ii,i,ntot,nvars
integer(i_kind) ilev, itracer

! load file
luin=914
open(luin,file=rcname,form='formatted')

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,nvars)

! Get contents of table
allocate(utable(nvars))
call gettable(tbname,luin,ntot,nvars,utable)

! release file unit
close(luin)

! Retrieve each token of interest from table and define
! variables participating in state vector

! Count variables first
ns3d=0; ns2d=0; nsdim=0;
do ii=1,nvars
   read(utable(ii),*) var, ilev, itracer, source, funcof
   if(ilev==1) then
       ns2d=ns2d+1
       nsdim=nsdim+1
   else
       ns3d=ns3d+1
       nsdim=nsdim+ilev
   endif
enddo

allocate(svars3d(ns3d),svars2d(ns2d),slevels(0:ns3d))

! Now load information from table
ns3d=0;ns2d=0
slevels = 0
do ii=1,nvars
   read(utable(ii),*) var, ilev, itracer, source, funcof
   if(ilev==1) then
      ns2d=ns2d+1
      svars2d(ns2d)=trim(adjustl(var))
   else if (ilev==nlevs .or. ilev==nlevs+1) then
      ns3d=ns3d+1
      svars3d(ns3d)=trim(adjustl(var))
      slevels(ns3d)=ilev + slevels(ns3d-1)
   else 
      if (nproc .eq. 0) print *,'Error statevec: - only ', nlevs, ' and ', nlevs+1,' number of levels is supported in current version, got ',ilev
      call stop2(503)
   endif
enddo

deallocate(utable)

! sanity checks
if (nsdim == 0) then
  if (nproc == 0) print *, 'Error: there are no variables in state vector.'
  call stop2(501)
endif

do i = 1, ns2d
  if (getindex(vars2d_supported, svars2d(i))<0) then
    if (nproc .eq. 0) then
      print *,'Error: state 2D variable ', svars2d(i), ' is not supported in current version.'
      print *,'Supported variables: ', vars2d_supported
    endif
    call stop2(502)
  endif
enddo
do i = 1, ns3d
  if (getindex(vars3d_supported, svars3d(i))<0) then
    if (nproc .eq. 0) then 
       print *,'Error: state 3D variable ', svars3d(i), ' is not supported in current version.'
       print *,'Supported variables: ', vars3d_supported
    endif
    call stop2(502)
  endif
enddo

if (nproc == 0) then 
  print *, '2D state variables: ', svars2d
  print *, '3D state variables: ', svars3d
  print *, '3D levels :', slevels
  print *, 'ns3d: ', ns3d, ', ns2d: ', ns2d, ', nsdim: ', nsdim
endif

call getgridinfo(statefileprefixes(nstatefields/2+1), .false.)

end subroutine init_statevec


subroutine read_state()
! read ensemble members on IO tasks,
implicit none
integer(i_kind) nanal, i, nb, ne
real(r_double), allocatable, dimension(:,:,:,:) :: qsat
real(r_single), allocatable, dimension(:) :: state_mean
integer(i_kind) ierr

! must at least nanals tasks allocated.
if (numproc < ntasks_io) then
  print *,'need at least ntasks_io =',ntasks_io,'MPI tasks, exiting ...'
  call mpi_barrier(mpi_comm_world,ierr)
  call mpi_finalize(ierr)
end if
if (npts < numproc) then
  print *,'cannot allocate more than npts =',npts,'MPI tasks, exiting ...'
  call mpi_barrier(mpi_comm_world,ierr)
  call mpi_finalize(ierr)
end if

! read in whole state vector on i/o procs - keep in memory 
allocate(state_d(npts,nsdim,nstatefields,nanals_per_iotask))
allocate(qsat(npts,nlevs,nstatefields,nanals_per_iotask))
if (paranc) then
   call readgriddata_pnc(svars3d,svars2d,ns3d,ns2d,slevels,nsdim,nstatefields, &
                         statefileprefixes,statesfcfileprefixes,.false.,state_d,qsat)
end if

if (nproc <= ntasks_io-1) then
   nanal = nproc + 1
   if ( .not. paranc) then
      call readgriddata(nanal1(nproc),nanal2(nproc),svars3d,svars2d,ns3d,ns2d,slevels,nsdim,nstatefields, &
                     statefileprefixes,statesfcfileprefixes,.false.,state_d,qsat)
   end if

   ! subtract the mean
   allocate(state_mean(npts)) 
   do nb = 1, nstatefields
     do i = 1, nsdim
       state_mean = sum(state_d(:,i,nb,:),dim=2)/real(nanals_per_iotask)
       call mpi_allreduce(mpi_in_place,state_mean,npts,mpi_real4,mpi_sum,mpi_comm_io,ierr)
       state_mean = state_mean/real(ntasks_io)
       do ne=1,nanals_per_iotask
          state_d(:,i,nb,ne) = state_d(:,i,nb,ne) - state_mean
       enddo
     enddo
   enddo
   deallocate(state_mean)
   deallocate(qsat)
else
   deallocate(state_d)
endif

end subroutine read_state

subroutine statevec_cleanup()
! deallocate module-level allocatable arrays.
if (allocated(svars3d)) deallocate(svars3d)
if (allocated(svars2d)) deallocate(svars2d)
if (allocated(slevels)) deallocate(slevels)
if (nproc <= ntasks_io-1 .and. allocated(state_d)) deallocate(state_d)
call gridinfo_cleanup()
end subroutine statevec_cleanup

end module statevec
