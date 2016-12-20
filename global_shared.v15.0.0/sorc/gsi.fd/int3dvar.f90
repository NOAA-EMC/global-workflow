subroutine int3dvar(rval,rval_dt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intall      calculate RHS for analysis equation
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: complementary components pertinent to the RHS of 3dvar 
!
! program history log:
!   2003-12-18  derber
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - add nonlinear qc option
!   2004-10-06  kleist  - separate control vector for u,v, & convert int
!                         for wind components into int for st,vp
!   2004-11-30  treadon - add brightness temperatures to nonlinear 
!                         quality control
!   2004-12-03  treadon - replace mpe_iallreduce (IBM extension) with
!                         standard mpi_allreduce
!   2005-01-20  okamoto - add u,v to intrad
!   2005-02-23  wu      - changes related to normalized rh option
!   2005-04-11  treadon - rename intall_qc as intall
!   2005-05-18  yanqiu zhu - add 'use int*mod',and modify call interfaces for using these modules
!   2005-05-24  pondeca - take into consideration that npred=npredp=0
!                         for 2dvar only surface analysis option
!   2005-06-03  parrish - add horizontal derivatives
!   2005-07-10  kleist  - add dynamic constraint term
!   2005-09-29  kleist  - expand Jc term, include time derivatives vector
!   2005-11-21  kleist  - separate tendencies from Jc term, add call to calctends adjoint
!   2005-12-01  cucurull - add code for GPS local bending angle, add use obsmod for ref_obs
!   2005-12-20  parrish - add arguments to call to intt to allow for option of using boundary
!                         layer forward tlm.
!   2006-02-03  derber  - modify to increase reproducibility
!   2006-03-17  park    - correct error in call to intt--rval,sval --> rvaluv,svaluv
!                          in order to correctly pass wind variables.
!   2006-04-06  kleist  - include both Jc formulations
!   2006-07-26  parrish - correct inconsistency in computation of space and time derivatives of q
!                          currently, if derivatives computed, for q it is normalized q, but
!                          should be mixing ratio.
!   2006-07-26  parrish - add strong constraint initialization option
!   2006-09-20  derber  - add sensible temperatures for conventional temperatures
!   2006-10-12  treadon - replace virtual temperature with sensible in intpcp
!   2007-03-13  derber  - modify call to strong_bal_correction remove u_t_g, etc. arrays
!   2007-03-13  derber  - fix bug when nstrong > 1
!   2007-02-15  rancic  - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-04-13  tremolet - split 3dvar specific components from intall
!   2007-04-16  kleist  - modified calls to tendency and constraint routines
!   2007-07-26 cucurull - update gps code to generalized vertical coordinate;
!                         get current solution for 3d pressure (sval3dp);
!                         move getprs_ad out of calctends_ad; add pri and 
!                         remove ps in calctends_ad argument list;
!                         add rval3dp; use getprs_ad
!   2007-08-08  derber - optimize - ensure that only necessary time derivatives are calculated
!   2008-06-02  safford - rm unused vars
!   2008-10-08  derber - move strong balance constraint to background error covariance
!   2008-12-01  todling - merge with 4dvar code (split of intall)
!   2010-05-13  todling - update to use gsi_bundle
!   2013-10-19  todling - update interface to calctends_ad
!
!   input argument list:
!
!   output argument list:      
!     rval     - RHS on grid
!
!
! remarks:
!       are all grid fields after strong initialization.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind
  use mpimod, only: mype
  use jfunc, only: l_foto
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none
  
! Declare passed variables  
  type(gsi_bundle),intent(inout) :: rval
  type(gsi_bundle),intent(inout) :: rval_dt

! Declare local variables
  integer(i_kind) ier,istatus

  real(r_kind),pointer,dimension(:,:,:) :: rval_dt_t=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: rval_dt_q=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: rval_dt_tsen=>NULL()

!******************************************************************************

 if(l_foto )then

!   Get required pointers
    ier=0
    call gsi_bundlegetpointer(rval_dt,'t',   rval_dt_t,   istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval_dt,'q',   rval_dt_q,   istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval_dt,'tsen',rval_dt_tsen,istatus);ier=istatus+ier
    if(ier/=0) then
       write(6,*) 'int3dvar: pointers not found, ier=', ier
       call stop2(999)
    endif

!   Adjoint of virtual to sensible temperature conversion
    call tv_to_tsen_ad(rval_dt_t,rval_dt_q,rval_dt_tsen)

    call calctends_ad(rval,rval_dt,mype)

  end if

  return
  end subroutine int3dvar
