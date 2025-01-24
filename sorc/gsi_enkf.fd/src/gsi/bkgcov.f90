subroutine bkgcov(cstate)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bkgcov    perform hor & vert of background error 
!   prgmmr: kleist         org: np22                date: 2004-07-22
!
! abstract: perform horizontal and vertical parts of background error
!
! program history log:
!   2004-07-22  kleist
!   2004-10-26  kleist - remove u,v
!   2004-11-03  treadon - move definition of horizontal scale weighting 
!                         factors to namelist
!   2004-11-22  derber - add openMP
!   2008-06-05  safford - rm unused vars
!   2010-03-01  zhu     - make changes for generalizing control vectors
!                       - replace explicit use of each control variable 
!                         by a control_state 'cstate'
!                       - use nrf* for generalized control variables
!                       - make changes to interfaces of sub2grid and grid2sub
!   2010-04-28  todling - update to use gsi_bundle
!   2011-06-29  todling - no explict reference to internal bundle arrays
!   2012-06-25  parrish - replace sub2grid/grid2sub with general_sub2grid/general_grid2sub.
!                         Remove arrays sst, slndt, sicet.  These are now contained as
!                         motley variables in input/output bundle cstate.  Remove unused variables
!                         nnnn1o,latlon11.
!
!   input argument list:
!     cstate   - bundle containing control fields
!
!   output argument list:
!                 all after smoothing, combining scales
!     cstate   - bundle containing control fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g_raf
  implicit none

! Passed Variables
  type(gsi_bundle),intent(inout) :: cstate

! Local Variables
  integer(i_kind) n,n3d,istatus,nlevs
  real(r_kind),dimension(nlat*nlon*s2g_raf%nlevs_alloc):: hwork
  real(r_kind),pointer,dimension(:,:,:):: ptr3d=>NULL()

  nlevs=s2g_raf%nlevs_loc
  n3d=cstate%n3d

! Multiply by background error variances, and break up skin temp
! into components
  call bkgvar(cstate,0)

! Apply vertical smoother
!$omp parallel do  schedule(dynamic,1) private(n,ptr3d,istatus)
  do n=1,n3d
     call gsi_bundlegetpointer ( cstate,cstate%r3(n)%shortname,ptr3d,istatus )
     call frfhvo(ptr3d,n)
  end do

! Convert from subdomain to full horizontal field distributed among processors
  call general_sub2grid(s2g_raf,cstate%values,hwork)

! Apply horizontal smoother for number of horizontal scales
  call smoothrf(hwork,nlevs)

! Put back onto subdomains
  call general_grid2sub(s2g_raf,hwork,cstate%values)

! Apply vertical smoother
!$omp parallel do  schedule(dynamic,1) private(n,ptr3d,istatus)
  do n=1,n3d
     call gsi_bundlegetpointer ( cstate,cstate%r3(n)%shortname,ptr3d,istatus )
     call frfhvo(ptr3d,n)
  end do

! Multiply by background error variances, and combine sst,sldnt, and sicet
! into skin temperature field
  call bkgvar(cstate,1)

  return
end subroutine bkgcov
! -----------------------------------------------------------------------------
subroutine ckgcov(z,cstate,nval_lenz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ckgcov   sqrt of bkgcov
!   prgmmr: kleist         org: np22                date: 2004-07-22
!
! abstract: perform horizontal and vertical parts of background error
!
! program history log:
!   2007-04-24  parrish
!   2008-12-04  todling - turn sst,slndt,sicet to locals per GSI May08 
!                         update to bkgcov above.
!   2010-03-15  zhu - use nrf* and cstate for generalized control variable
!                   - make changes to interface of grid2sub
!   2010-04-28  todling - udpate to use gsi_bundle
!   2011-06-29  todling - no explict reference to internal bundle arrays
!   2011-09-05  todling - add explicit reference to navl_lenz, and remove connection through jfunc
!   2012-06-25  parrish - replace grid2sub with general_grid2sub.
!                         Remove arrays sst, slndt, sicet.  These are now contained as
!                         motley variables in input/output bundle cstate.  Remove unused variables
!                         nnnn1o,latlon11,nval_levs.
!   2016-03-25  todling - revisit beta multiplier term
!
!   input argument list:
!     z        - long vector input control fields
!     cstate   - bundle containing control fields
!     nlevs    - number of vertical levels for smoothing
!     nval_lenz- length of sqrt-B control vector
!
!   output argument list:
!                 all after smoothing, combining scales
!     cstate   - bundle containing control fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use general_sub2grid_mod, only: general_grid2sub
  use general_commvars_mod, only: s2g_raf
  use hybrid_ensemble_parameters, only: l_hyb_ens
  use hybrid_ensemble_isotropic, only: sqrt_beta_s_mult
  implicit none

! Passed Variables
  integer(i_kind)    ,intent(in   ) :: nval_lenz
  type(gsi_bundle),intent(inout) :: cstate
  real(r_kind),dimension(nval_lenz),intent(in   ) :: z

! Local Variables
  integer(i_kind) k,n3d,istatus,nlevs
  real(r_kind),dimension(nlat*nlon*s2g_raf%nlevs_alloc):: hwork
  real(r_kind),dimension(:,:,:),pointer:: ptr3d=>NULL()

  nlevs=s2g_raf%nlevs_loc

! Apply horizontal smoother for number of horizontal scales
  call sqrt_smoothrf(z,hwork,nlevs)

! Put back onto subdomains
  call general_grid2sub(s2g_raf,hwork,cstate%values)

! Apply vertical smoother
  n3d=cstate%n3d
!$omp parallel do  schedule(dynamic,1) private(k,ptr3d,istatus)
  do k=1,n3d
     call gsi_bundlegetpointer ( cstate,cstate%r3(k)%shortname,ptr3d,istatus )
     call frfhvo(ptr3d,k)
  end do

! Multiply by background error variances, and combine sst,sldnt, and sicet
! into skin temperature field
  call bkgvar(cstate,1)

! Apply static betas
  if(l_hyb_ens) call sqrt_beta_s_mult(cstate)

  return
end subroutine ckgcov
! -----------------------------------------------------------------------------
subroutine ckgcov_ad(z,cstate,nval_lenz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ckgcov_ad  adjoint of ckgcov
!   prgmmr: kleist         org: np22                date: 2004-07-22
!
! abstract: perform horizontal and vertical parts of background error
!
! program history log:
!   2007-04-24  parrish
!   2008-12-04  todling - turn sst,slndt,sicet to locals per GSI May08 
!                         update to bkgcov above.
!   2010-03-15  zhu - use nrf* and cstate for generalized control variable
!                   - make changes to interface of sub2grid 
!   2010-04-15  treadon - add %values to cstate in bkgvar call
!   2010-04-28  todling - update to use gsi_bundle
!   2011-06-29  todling - no explict reference to internal bundle arrays
!   2011-09-05  todling - add explicit reference to navl_lenz, and remove connection through jfunc
!   2012-06-25  parrish - replace sub2grid with general_sub2grid.
!                         Remove arrays sst, slndt, sicet.  These are now contained as
!                         motley variables in input/output bundle cstate.  Remove unused variables
!                         nnnn1o,latlon11.
!   2016-03-25  todling - revisit beta multiplier term
!
!   input argument list:
!     z        - long vector adjoint input/output control fields
!     cstate   - bundle containing control fields
!     nlevs    - number of vertical levels for smoothing
!     nval_lenz- length of sqrt-B control vector
!
!   output argument list:
!                 all after smoothing, combining scales
!     cstate   - bundle containing adjoint control fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: nlat,nlon
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use general_sub2grid_mod, only: general_sub2grid
  use general_commvars_mod, only: s2g_raf
  use hybrid_ensemble_parameters, only: l_hyb_ens
  use hybrid_ensemble_isotropic, only: sqrt_beta_s_mult
  implicit none

! Passed Variables
  integer(i_kind)    ,intent(in   ) :: nval_lenz
  type(gsi_bundle),intent(inout) :: cstate
  real(r_kind),dimension(nval_lenz),intent(inout) :: z

! Local Variables
  integer(i_kind) k,n3d,istatus,nlevs
  real(r_kind),dimension(nlat*nlon*s2g_raf%nlevs_alloc):: hwork
  real(r_kind),dimension(:,:,:),pointer:: ptr3d=>NULL()

  nlevs=s2g_raf%nlevs_loc

! Apply static betas
  if(l_hyb_ens) call sqrt_beta_s_mult(cstate)

! Multiply by background error variances, and break up skin temp
! into components
  call bkgvar(cstate,0)

! Apply vertical smoother
  n3d=cstate%n3d
!$omp parallel do  schedule(dynamic,1) private(k,ptr3d,istatus)
  do k=1,n3d
     call gsi_bundlegetpointer ( cstate,cstate%r3(k)%shortname,ptr3d,istatus )
     call frfhvo(ptr3d,k)
  end do

! Convert from subdomain to full horizontal field distributed among processors
  call general_sub2grid(s2g_raf,cstate%values,hwork)

! Apply horizontal smoother for number of horizontal scales
  call sqrt_smoothrf_ad(z,hwork,nlevs)

  return
end subroutine ckgcov_ad
