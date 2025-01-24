module observer_enkf
use general_tll2xy_mod, only: llxy_cons
use statevec, only: nsdim, ns3d, ns2d, slevels
use params, only: nlevs, neigv

private
public init_observer_enkf, setup_linhx, calc_linhx, calc_linhx_modens,&
       destroy_observer_enkf
integer, allocatable, dimension(:) ::  kindx

type(llxy_cons) :: gt_data


contains

subroutine init_observer_enkf
  use kinds, only: r_kind, i_kind
  use params, only: nlons, nlats
  use gridinfo, only: latsgrd, lonsgrd
  use general_tll2xy_mod, only: general_create_llxy_transform
  implicit none

  integer(i_kind) :: i, j
  integer(i_kind) :: nn,n,k,nl
  real(r_kind), dimension(nlats, nlons) :: lats, lons


  do i = 1,nlons
     do j = 1,nlats
        lats(j,i) = latsgrd((j-1)*nlons+i)
        lons(j,i) = lonsgrd((j-1)*nlons+i)
     enddo
  enddo
  call general_create_llxy_transform(lats, lons, nlats, nlons, gt_data)

  nn = 0
  do n=1,ns3d
    if (n .eq. 1) then
      nl = slevels(n)
    else
      nl = slevels(n)-slevels(n-1)
    endif
    !print *,'ns3d,levs',n,nl
    do k=1,nl
       nn = nn + 1
       kindx(nn) = k
       ! FIXME - deal with state variables with nlevs+1 levels (like prse)
       if (kindx(nn) > nlevs) kindx(nn)=nlevs
    enddo
  enddo
  do n=1,ns2d ! 2d fields are treated as surface fields.
    nn = nn + 1
    kindx(nn) = 1
  enddo

end subroutine init_observer_enkf

subroutine destroy_observer_enkf
 if (allocated(kindx)) deallocate(kindx)
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
!   2018-09-05  Guoqing Ge -Added this fuction in observer_wrf to be 
!                           consistent with  observer_gfs.f90
!
!   input argument list:
!     rlat: latitude of ob
!     rlon: longitude of ob
!     time: time offset for ob
!
!   output argument list:
!     ix,delx,ixp,delxp,iy,dely,iyp,delyp,it,delt,itp,deltp: horizontal
!       and temporal linear interpolation indices and weights.
!
! attributes:
!   language: f95
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use params, only: nstatefields, nlons, nlats, nlevs, nhr_state, fhr_assim
  use gridinfo, only: npts, latsgrd, lonsgrd
  use statevec, only: nsdim
  use constants, only: zero,one,pi
  use general_tll2xy_mod, only: general_tll2xy
  use mpisetup
  implicit none

! Declare passed variables
  real(r_single)                                   ,intent(in   ) :: rlat, rlon   ! observation lat and lon in radians
  real(r_single)                                   ,intent(in   ) :: time         ! observation time relative to middle of window
  integer(i_kind), intent(out) :: ix, iy, it, ixp, iyp, itp
  real(r_kind), intent(out) :: delx, dely, delxp, delyp, delt, deltp
  real(r_kind)    :: dx, dy
  logical :: outside

  call general_tll2xy(gt_data, real(rlon,r_kind), real(rlat,r_kind), dx, dy, outside)

  ix = max(1,min(int(dx),nlons))
  iy = max(1,min(int(dy),nlats))

  delx = max(zero, min(dx - float(ix), one))
  dely = max(zero, min(dy - float(iy), one))

  ixp = min(ix + 1, nlons)
  iyp = min(iy + 1, nlats)

  iy = iy - 1; iyp = iyp - 1

  it = 1
  do while (time + fhr_assim > nhr_state(it) .and. it < nstatefields)
    it = it + 1
  enddo
  itp = it
  it = max(1,itp-1)
  if (it /= itp) then
     delt = (time + fhr_assim - nhr_state(it)) / (nhr_state(itp) - nhr_state(it))
  else
     delt = one
  endif

  deltp = one - delt
  delxp = one - delx
  delyp = one - dely

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
!
! attributes:
!   language: f95
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use params, only: nstatefields, nlons, nlats, nlevs, nhr_state, fhr_assim
  use gridinfo, only: npts, latsgrd, lonsgrd
  use statevec, only: nsdim
  use constants, only: zero,one,pi
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
  integer(i_kind) i,j,k

  ! interpolate state horizontally and in time and do  dot product with dHx/dx profile
  ! saves from calculating interpolated x_ens for each state variable
  hx_ens = hx
  do i = 1, dhx_dx%nnz
     j = dhx_dx%ind(i)
     k = kindx(j)
     hxpert%val(i) = (( dens( ix*nlons  + iy , j, it) *delxp*delyp          &
                      + dens( ixp*nlons + iy , j, it) *delx *delyp          &
                      + dens( ix*nlons  + iyp, j, it) *delxp*dely           &
                      + dens( ixp*nlons + iyp, j, it) *delx *dely )*deltp   &
                    + ( dens( ix*nlons  + iy , j, itp)*delxp*delyp          &
                      + dens( ixp*nlons + iy , j, itp)*delx *delyp          &
                      + dens( ix*nlons  + iyp, j, itp)*delxp*dely           &
                      + dens( ixp*nlons + iyp, j, itp)*delx *dely )*delt)
     hx_ens = hx_ens + dhx_dx%val(i) * hxpert%val(i)
  enddo

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
  use mpisetup
  implicit none

! Declare passed variables
  real(r_single)                                   ,intent(in   ) :: hx           ! H(x_mean)
  type(sparr)                                      ,intent(in   ) :: dhx_dx       ! dH(x)/dx |x_mean profiles
  type(raggedarr)                                  ,intent(in   ) :: hxpert       ! interpolated background
  real(r_single)                                   ,intent(  out) :: hx_ens(neigv)! H (x_ens)
  real(r_double),dimension(neigv,nlevs+1)          ,intent(in   ) :: vscale       ! vertical scaling (for modulated ens)
  integer(i_kind) i

  ! calculate modulated ensemble in ob space
  hx_ens = hx
  do i = 1, dhx_dx%nnz
     hx_ens(:) = hx_ens(:) + dhx_dx%val(i) * vscale(:,kindx(dhx_dx%ind(i))) * hxpert%val(i)
  enddo

  return
end subroutine calc_linhx_modens

end module observer_enkf
