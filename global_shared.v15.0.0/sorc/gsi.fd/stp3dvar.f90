subroutine stp3dvar(dirx,dir_dt)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stp3dvar    calculate penalty and stepsize
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: calculate contribution from constraints to current penalty and stepsize
!               (nonlinear qc version)
!
! program history log:
!   2003-12-18  derber,j.
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - add nonlinear qc option
!   2004-10-06  kleist  - separate control vector for u,v, get search
!                         direction for u,v from dir for st,vp
!   2004-11-30  treadon - add brightness temperatures to nonlinear
!                         quality control
!   2005-01-20  okamoto - add u,v to stprad_qc
!   2005-01-26  cucurull- implement local GPS RO linear operator
!   2005-02-10  treadon - add u,v to stprad_qc (okamoto change not present)
!   2005-02-23  wu      - add call to normal_rh_to_q to convert normalized 
!                         RH to q
!   2005-04-11  treadon - rename stpcalc_qc as stpcalc
!   2005-05-21  yanqiu zhu - add 'use stp*mod', and modify call interfaces for using these modules
!   2005-05-27  derber - remove linear stepsize estimate
!   2005-06-03  parrish - add horizontal derivatives
!   2005-07-10  kleist  - add dynamic constraint term (linear)
!   2005-09-29  kleist  - expand Jc term, include time derivatives vector
!   2005-11-21  kleist  - separate tendencies from Jc term, add call to calctends tlm
!   2005-12-01  cucurull - add code for GPS local bending angle, add use obsmod for ref_obs
!   2005-12-20  parrish - add arguments to call to stpt to enable boundary layer forward
!                         model option.
!   2006-04-18  derber - add explicit iteration over stepsize (rather than 
!                        repeated calls) - clean up and simplify
!   2006-04-24  kleist - include both Jc formulations
!   2006-05-26  derber - modify to improve convergence checking
!   2006-07-26  parrish - correct inconsistency in computation of space and time derivatives of q
!                          currently, if derivatives computed, for q it is normalized q, but
!                          should be mixing ratio.
!   2006-08-04  parrish - add strong constraint initialization option
!   2006-09-18  derber - modify output from nonlinear operators to make same as linear operators
!   2006-09-20  derber - add sensible temperatures for conventional obs.
!   2006-10-12  treadon - replace virtual temperature with sensible in stppcp
!   2007-02-15  rancic  - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-04-13  tremolet - split 3dvar specific components from sptcalc
!   2007-04-16  kleist  - modified calls to tendency and constraint routines
!   2007-06-04  derber  - use quad precision to get reproduceability over number of processors
!   2007-07-26  cucurull - update gps code to generalized vertical coordinate;
!                          get current solution for 3d pressure (xhat_3dp);
!                          move getprs_tl out of calctends_tl; add dirx3dp
!                          and remove ps in calctends_tl argument list;
!                          use getprs_tl 
!   2007-08-08  derber - optimize, ensure that only necessary time derivatives are calculated
!   2008-11-28  todling - updated call to tv_to_tsen 
!   2008-04-11  safford - rm unused vars and uses
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2008-10-08  derber - move strong balance constraint to background error covariance
!   2008-12-02  todling - revisited split of stpcalc in light of 4dvar merge with May08 version
!   2010-05-13  todling - update to use gsi_bundle
!   2013-10-19  todling - update interface to calctends_tl
!   2013-10-28  todling - rename p3d to prse
!
!   input argument list:
!     stpinout - guess stepsize
!     dirx     - search direction for x
!
!   output argument list:
!     dirx     - search direction for state vectors
!     stpinout - final estimate of stepsize
!
!
! remarks:
!     The part of xhat and dirx containing temps and psfc are values before strong initialization,
!     xhatt, xhatp and dirxt, dirxp contain temps and psfc after strong initialization.
!     If strong initialization is turned off, then xhatt, etc are equal to the corresponding 
!     fields in xhat, dirx.
!     xhatuv, xhat_t and dirxuv, dirx_x, dirx_y, are all after
!     strong initialization if it is turned on.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind
  use mpimod, only: mype
  use gridmod, only: nnnn1o
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(gsi_bundle), intent(in   ) :: dirx
  type(gsi_bundle), intent(  out) :: dir_dt

! Declare local variables
  integer(i_kind) ier,istatus
  real(r_kind),pointer,dimension(:,:,:)::dirx_u
  real(r_kind),pointer,dimension(:,:,:)::dirx_v
  real(r_kind),pointer,dimension(:,:,:)::dirx_t
  real(r_kind),pointer,dimension(:,:,:)::dirx_q
  real(r_kind),pointer,dimension(:,:,:)::dirx_oz
  real(r_kind),pointer,dimension(:,:,:)::dirx_cw
  real(r_kind),pointer,dimension(:,:,:)::dirx_prse

  real(r_kind),pointer,dimension(:,:,:)::dir_dt_u=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::dir_dt_v=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::dir_dt_t=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::dir_dt_q=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::dir_dt_oz=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::dir_dt_cw=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::dir_dt_prse=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::dir_dt_tsen=>NULL()

!************************************************************************************  

  ier=0
  call gsi_bundlegetpointer(dirx,'u',  dirx_u,  istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dirx,'v',  dirx_v,  istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dirx,'tv' ,dirx_t,  istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dirx,'q',  dirx_q,  istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dirx,'oz' ,dirx_oz, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dirx,'cw' ,dirx_cw, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dirx,'prse',dirx_prse,istatus);ier=istatus+ier
  if(ier/=0) return

  call gsi_bundlegetpointer(dir_dt,'u',   dir_dt_u,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dir_dt,'v',   dir_dt_v,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dir_dt,'tv',  dir_dt_t,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dir_dt,'q',   dir_dt_q,   istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dir_dt,'oz' , dir_dt_oz  ,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dir_dt,'cw' , dir_dt_cw  ,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dir_dt,'prse', dir_dt_prse, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dir_dt,'tsen',dir_dt_tsen,istatus);ier=istatus+ier
  if(ier/=0) return

  call calctends_tl(dirx,dir_dt,mype)

! Convert virtual temperature to sensible temperature for time derivatives
! for search direction
  call tv_to_tsen(dir_dt_t,dir_dt_q,dir_dt_tsen)


  return
end subroutine stp3dvar

