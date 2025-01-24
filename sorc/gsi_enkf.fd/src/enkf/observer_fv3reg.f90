module observer_enkf
! a dummy module ,modified from observer_gfs.f90
use statevec, only: nsdim, ns3d, ns2d, slevels
use params, only: nlevs, neigv

private
public init_observer_enkf, setup_linhx, calc_linhx, calc_linhx_modens,&
       destroy_observer_enkf
integer, allocatable, dimension(:) ::  kindx

contains

subroutine init_observer_enkf
   write(6,*)'this is a dummy subroutine, running this means something wrong ,stop'
   call stop2(555)
   return
end subroutine init_observer_enkf

subroutine destroy_observer_enkf
   write(6,*)'this is a dummy subroutine, running this means something wrong ,stop'
   call stop2(555)
end subroutine destroy_observer_enkf

subroutine setup_linhx(rlat, rlon, time, ix, delx, ixp, delxp, iy, dely,  &
                       iyp, delyp, it, delt, itp, deltp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calc_linhx
!   prgmmr: shlyaeva         org: esrl/psd            date: 2016-11-29
!
! abstract: 
!
! program history log:
!   2016-11-29  shlyaeva
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f95
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use params, only: nstatefields, nlons, nlats, nlevs, nhr_state, fhr_assim
  use gridinfo, only: npts, latsgrd, lonsgrd
  use statevec, only: nsdim
  use constants, only: izero,zero,one,pi
  use mpisetup
  implicit none

! Declare passed variables
  real(r_single)                                   ,intent(in   ) :: rlat, rlon   ! observation lat and lon in radians
  real(r_single)                                   ,intent(in   ) :: time         ! observation time relative to middle of window
  integer(i_kind), intent(out) :: ix, iy, it, ixp, iyp, itp
  real(r_kind), intent(out) :: delx, dely, delxp, delyp, delt, deltp
  ix=izero
  iy=izero
  it=izero
  ixp=izero
  iyp=izero
  itp=izero
  delx=zero
  dely=zero
  delxp=zero
  delyp=zero
  delt=zero
  deltp=zero
   write(6,*)'this is a dummy subroutine, running this means something wrong ,stop'
   call stop2(555)


  ! find interplation indices and deltas

end subroutine setup_linhx


subroutine calc_linhx(hx, dens, dhx_dx, hxpert, hx_ens, &
                      ix, delx, ixp, delxp, iy, dely, iyp, delyp, &
                      it, delt, itp, deltp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calc_linhx
!   prgmmr: shlyaeva         org: esrl/psd            date: 2016-11-29
!
! abstract: 
!
! program history log:
!   2016-11-29  shlyaeva
!
!   input argument list:
!     hx: observation prior ensemble mean
!     dens:  state space ensemble perturbations
!     dhx_dx: Jacobian
!     ix,delx,ixp,delxp,iy,dely,iyp,delyp,it,delt,itp,deltp: horizontal
!       and temporal linear interpolation indices and weights.
!
!   output argument list:
!     hx_ens: observation prior ensemble perturbation
!     hxpert: ens pert profile that multiplies dhx_dx to yield hx_ens (in
!     compressed format - temporally and horizontally interpolated)
!
! attributes:
!   language: f95
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use params, only: nstatefields, nlons
  use gridinfo, only: npts
  use statevec, only: nsdim
  use constants, only: zero,one
  use sparsearr, only: sparr, raggedarr
  use mpisetup
  implicit none

! Declare passed variables
  real(r_single)                                   ,intent(in   ) :: hx           ! H(x_mean)
  real(r_single),dimension(npts,nsdim,nstatefields),intent(in   ) :: dens         ! x_ens - x_mean, state vector space
  integer(i_kind), intent(in) :: ix, iy, it, ixp, iyp, itp
  real(r_kind), intent(in) :: delx, dely, delxp, delyp, delt, deltp
  type(sparr)                                      ,intent(in   ) :: dhx_dx       ! dH(x)/dx |x_mean profiles
  type(raggedarr)                                  ,intent(inout) :: hxpert       ! interpolated background
  real(r_single)                                   ,intent(  out) :: hx_ens       ! H (x_ens)
  integer(i_kind) i,j
  hx_ens=zero
   write(6,*)'this is a dummy subroutine, running this means something wrong ,stop'
   call stop2(555)

  return
end subroutine calc_linhx

subroutine calc_linhx_modens(hx, dhx_dx, hxpert, hx_ens, vscale)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calc_linhx
!   prgmmr: shlyaeva         org: esrl/psd            date: 2016-11-29
!
! abstract: 
!
! program history log:
!   2016-11-29  shlyaeva, initial version
!   2019-12-09  whitaker, optimizations
!
!   input argument list:
!     hx: observation prior ensemble mean
!     dhx_dx: Jacobian
!     hxpert: 'unmodulated' ens pert profile that multiplies dhx_dx
!     vscale: vertical scaling from vertical localization eigenvectors used
!       to generate modulated ensemble.
!
!   output argument list:
!     hx_ens: observation prior ensemble perturbation for each verticali
!      localization eigenvector
!
! attributes:
!   language: f95
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use sparsearr, only: sparr, raggedarr
  use constants, only: zero
  use mpisetup
  implicit none

! Declare passed variables
  real(r_single)                                   ,intent(in   ) :: hx           ! H(x_mean)
  type(sparr)                                      ,intent(in   ) :: dhx_dx       ! dH(x)/dx |x_mean profiles
  type(raggedarr)                                  ,intent(in   ) :: hxpert       ! interpolated background
  real(r_single)                                   ,intent(  out) :: hx_ens(neigv)! H (x_ens)
  real(r_double),dimension(neigv,nlevs+1)          ,intent(in   ) :: vscale       ! vertical scaling (for modulated ens)
  integer(i_kind) i
  hx_ens=zero
  write(6,*)'this is a dummy subroutine, running this means something wrong ,stop'
   call stop2(555)


  return
end subroutine calc_linhx_modens

end module observer_enkf
