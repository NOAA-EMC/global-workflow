#ifdef GFS
module smooth_mod

! model dependent smoothing of inflation estimate.
! This version is for GFS, expects data to be on global gaussian grids (full or
! reduced). Isotropic spectral smoothing (gaussian) is used.

use mpisetup
use params, only:  ndim, nlons, nlats, reducedgrid, smoothparm
use kinds, only:  r_kind, i_kind, r_single
use gridinfo, only: npts, ntrunc
use constants, only: zero
use reducedgrid_mod, only: regtoreduced, reducedtoreg

implicit none

private
public :: smooth

contains 

subroutine smooth(grids)
! horizontal smoothing of 2d grids.
! version for gaussian grids (works for GFS).
! when called, grids contains unsmoothed grids on
! root task.  on return, grids on root will contain
! smoothed grids.
! smoothing controlled by parameter smoothparm.
use specmod, only: sptez_s, init_spec_vars, jcap, isinitialized
implicit none
integer(i_kind) np,ierr,m,nmdim,nm,nn,n,delta,npmax
real(r_single), intent(inout) :: grids(npts,ndim) ! there are ndim 2d grids.
real(r_single) smoothfact ! smoothing parameter.
real(r_kind) reggrd(nlons*nlats)
real(r_kind), allocatable, dimension(:) :: specdat
integer(i_kind) n1(0:numproc-1),n2(0:numproc-1)
delta = ndim/numproc
if (delta*numproc < ndim) delta = delta + 1
npmax = 0
do np=0,numproc-1
   n1(np) = 1 + np*delta
   n2(np) = (np+1)*delta
   if (n2(np) > ndim) n2(np) = ndim
   if (n1(np) > ndim .and. npmax == 0) npmax = np-1
enddo
! spectrally smooth the grids
! bcast out to all procs.
if (nproc <= npmax) then
  if (.not. isinitialized) call init_spec_vars(nlons,nlats,ntrunc,4)
  do nn=1,ndim
    if (nn < n1(nproc) .or. nn > n2(nproc)) grids(:,nn)=zero
  enddo
  nmdim = (ntrunc+1)*(ntrunc+2)/2
  allocate(specdat(2*nmdim))
  do nn=n1(nproc),n2(nproc)
     if (reducedgrid) then
        call reducedtoreg(grids(:,nn),reggrd)
     else
        reggrd = grids(:,nn) 
     endif
     call sptez_s(specdat,reggrd,-1)
     nm = 1
     do m=0,ntrunc
        do n=m,ntrunc
           smoothfact = exp(-(float(n)/smoothparm)**2)
           specdat(nm) = smoothfact*specdat(nm)
           specdat(nm+1) = smoothfact*specdat(nm+1)
           nm = nm + 2
        enddo
     enddo
     call sptez_s(specdat,reggrd,1)
     if (reducedgrid) then
        call regtoreduced(reggrd,grids(:,nn))
     else
        grids(:,nn) = reggrd
     endif
  enddo !nn=1,ndim
  deallocate(specdat)
else ! np > npmax
  grids = zero
end if 
!call mpi_allreduce(mpi_in_place,grids,npts*ndim,mpi_real4,mpi_sum,mpi_comm_world,ierr)
do nn=1,ndim
  call mpi_allreduce(mpi_in_place,grids(1,nn),npts,mpi_real4,mpi_sum,mpi_comm_world,ierr)
enddo
end subroutine smooth
end module smooth_mod
#endif
#ifdef WRF
module smooth_mod

use mpisetup
use params, only:  ndim,nlons,nlats,smoothparm
use kinds, only: r_kind
use gridinfo, only: npts

implicit none

private
public :: smooth

contains

subroutine smooth(grids)
real(r_single), intent(inout) :: grids(npts,ndim) ! there are ndim 2d grids.
! stub - not yet implemented.
if (nproc .eq. 0) print *,'wrf inflation smoothing not yet implemented!'
end subroutine smooth

end module smooth_mod
#endif

#ifdef NMMB
module smooth_mod

use mpisetup
use params, only:  ndim,nlons,nlats,smoothparm
use kinds, only: r_kind
use gridinfo, only: npts

implicit none

private
public :: smooth

contains

subroutine smooth(grids)
real(r_single), intent(inout) :: grids(npts,ndim) ! there are ndim 2d grids.
! stub - not yet implemented.
if (nproc .eq. 0) print *,'nmmb inflation smoothing not yet implemented!'
end subroutine smooth

end module smooth_mod
#endif
