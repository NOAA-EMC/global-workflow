module stpjomod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpjo     calculate penalty and stepsize
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: calculate observation term to penalty and estimate stepsize
!               (nonlinear qc version)
!
! program history log:
!   2003-12-18  derber,j.       -
!   2016-08-22  guo, j.         - Wrapped simple subroutines to a module, with
!                                 private module variables from obsmod.F90 moved
!                                 here.
!                               . For the earlier program history log, see the
!                                 "program history log" blocks below, in
!                                 individual subroutines/module-procedures.
!                               . Changed if/elseif/else blocks to select-case
!                                 blocks, using enumerated i_ob_type to replace
!                                 locally hard-wired index values (ll=1,2,3,..).
!                                 This is a step moving toward using type-bound-
!                                 procedures.
!   2018-08-10  guo     - a new implementation replacing typs specific stpXYZ()
!                         calls to polymorphic %stpjo() calls.

  use kinds , only: i_kind

  implicit none

  private

        ! Usecase 1: as-is
        !       call stpjo_setup(yobs,size(yobs))
        !       call stpjo(yobs,dval,dbias,xval,xbias,sges,pbcjo,nstep,size(yobs))
  public:: stpjo
  public:: stpjo_setup

        ! Usecase 2: Renamed with the same functionalities but more explicit names
  public:: stpjo_reset  ! always re-set, either undefined or already defined.
           interface stpjo_reset; module procedure stpjo_setup; end interface
  public:: stpjo_calc   ! 
           interface stpjo_calc ; module procedure stpjo      ; end interface

! Moved here from obsmod.F90
!   def stpcnt         - number of non-zero obs types (including time domain) on
!                        processor - used for threading of stpjo
!   def ll_jo          - points at ob type for location in stpcnt - used for
!                        threading of stpjo
!   def ib_jo          - points at time bin for location in stpcnt - used for
!                        threading of stpjo

  integer(i_kind),save:: stpcnt                          ! count of stpjo threads
  integer(i_kind),save,allocatable,dimension(:):: ll_jo  ! enumerated iobtype of threads
  integer(i_kind),save,allocatable,dimension(:):: ib_jo  ! ob-bin index values of threads
  logical:: omptasks_configured_ = .false.

  character(len=*),parameter:: myname="stpjomod"
contains

subroutine init_(nobs_type,nobs_bins)
!> initialize a task distribution list (stpcnt, ll_jo(:),ib_jo(:))
  implicit none
  integer(i_kind),intent(in):: nobs_type
  integer(i_kind),intent(in):: nobs_bins

  if(omptasks_configured_) call final_()

  allocate(ll_jo(nobs_bins*nobs_type), &
           ib_jo(nobs_bins*nobs_type)  )

  ll_jo(:)=0
  ib_jo(:)=0
  stpcnt  =0
end subroutine init_

subroutine final_()
  implicit none
  if(allocated(ll_jo)) deallocate(ll_jo)
  if(allocated(ib_jo)) deallocate(ib_jo)
  stpcnt=0
  omptasks_configured_=.false.
end subroutine final_

subroutine stpjo(dval,dbias,xval,xbias,sges,pbcjo,nstep)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpjo     calculate penalty and stepsize
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: calculate observation term to penalty and estimate stepsize
!               (nonlinear qc version)
!
!    A description of nonlinear qc follows:
!
!    The observation penalty Jo is defined as
!
!          Jo =  - (sum over obs) 2*log(Po)
!
!      where,
!
!          Po = Wnotgross*exp(-.5*(Hn(x+xb) - yo)**2 ) + Wgross
!            with
!                Hn = the forward model (possibly non-linear) normalized by 
!                     observation error
!                x  = the current estimate of the analysis increment
!                xb = the background state
!                yo = the observation normalized by observation error
!
!            Note:  The factor 2 in definition of Jo is present because the 
!                   penalty Jo as used in this code is 2*(usual definition 
!                   of penalty)
!
!          Wgross = Pgross*cg
!
!          Wnotgross = 1 - Wgross
!
!          Pgross = probability of gross error for observation (assumed
!                   here to have uniform distribution over the possible
!                   range of values)
!
!          cg = sqrt(2*pi)/2b
!
!          b = possible range of variable for gross errors, normalized by 
!              observation error
!
!    The values for the above parameters that Bill Collins used in the
!    eta 3dvar are:
!
!          cg = cg_term/b, where cg_term = sqrt(2*pi)/2 
!
!          b = 10.        ! range for gross errors, normalized by obs error
!
!          pg_q=.002      ! probability of gross error for specific humidity
!          pg_pw=.002     ! probability of gross error for precipitable water
!          pg_p=.002      ! probability of gross error for pressure
!          pg_w=.005      ! probability of gross error for wind
!          pg_t=.007      ! probability of gross error for temperature
!          pg_rad=.002    ! probability of gross error for radiances
!
!
!    Given the above Jo, the gradient of Jo is as follows:
!
!                                             T
!        gradx(Jo) = - (sum over observations) 2*H (Hn(x+xb)-yo)*(Po - Wgross)/Po
!
!      where, 
!
!          H = tangent linear model of Hn about x+xb
!
! 
!    Note that if Pgross = 0.0, then Wnotgross=1.0 and Wgross=0.0.  That is,
!    the code runs as though nonlinear quality control were not present
!    (which is indeed the case since the gross error probability is 0).  
!
!    As a result the same stp* routines may be used for use with or without
!    nonlinear quality control.
!    
!    Please note, however, that using the nonlinear qc routines makes the
!    stp* and int* operators nonlinear.  Hence, the need to evaluate the
!    step size operators twice for each observation type, give the current
!    step size algorithm coded below. 
!
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
!   2007-03-19  tremolet - binning of observations
!   2007-04-13  tremolet - split jo from other components of stpcalc
!   2007-04-16  kleist  - modified calls to tendency and constraint routines
!   2007-06-04  derber  - use quad precision to get reproduceability over number of processors
!   2007-07-26  cucurull - update gps code to generalized vertical coordinate;
!                          get current solution for 3d pressure (xhat_3dp);
!                          move getprs_tl out of calctends_tl; add dirx3dp
!                          and remove ps in calctends_tl argument list;
!                          use getprs_tl 
!   2007-08-08  derber - optimize, ensure that only necessary time derivatives are calculated
!   2008-12-02  todling - revisited split of stpcalc in light of 4dvar merge with May08 version
!   2009-01-08  todling - remove reference to ozohead
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-03-25  zhu     - change the interfaces of stprad,stpt,stppcp;add nrf* conditions 
!   2010-05-13  todling - harmonized all stp interfaces to use state vector; gsi_bundle use
!   2010-06-14  todling - add stpco call 
!   2010-07-10  todling - somebody reordered calls to stpw, stpq, and stpoz - any reason?
!   2010-10-15  pagowski - add stppm2_5 call 
!   2011-02-24  zhu    - add gust,vis,pblh calls
!   2013-05-23  zhu    - add bias correction contribution from aircraft T bias correction
!   2014-03-19  pondeca - add wspd10m
!   2014-04-10  pondeca - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add howv
!   2014-06-17  carley/zhu - add lcbas and tcamt
!   2015-07-10  pondeca - add cldch
!   2016-05-05  pondeca - add uwnd10m, vwnd10m
!   2016-08-26  guo     - separated a single stpoz() call into stpozlay() and
!                         stpozlev() calls.  This is a next-step fix of the
!                         minimum fix in stpjo_setup() below, to let output
!                         pbcjo(:,:,:) to reflect individual ob-types correctly.
!   2018-01-01  apodaca - add lightning (light) call
!
!   input argument list:
!     yobs
!     dval     - current solution
!     dbias    - 
!     xval     -
!     xbias    -
!     sges
!     nstep    - number of steps
!
!   output argument list:
!     pbcjo
!
!
! remarks:
!  1. The part of xhat and dirx containing temps and psfc are values before strong initialization,
!     xhatt, xhatp and dirxt, dirxp contain temps and psfc after strong initialization.
!     If strong initialization is turned off, then xhatt, etc are equal to the corresponding 
!     fields in xhat, dirx.
!     xhatuv, xhat_t and dirxuv, dirx_t are all after
!     strong initialization if it is turned on.
!  2. Notice that now (2010-05-13) stp routines handle non-essential variables
!     internally; also, when pointers non-existent, stp routines simply return.
!     
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind,r_quad
  use bias_predictors, only: predictors
  use gsi_bundlemod, only: gsi_bundle

  use gsi_obOper, only: obOper
  use m_obsdiags, only: obOper_create
  use m_obsdiags, only: obOper_destroy
  use gsi_obOperTypeManager, only: obOper_typeInfo

  use mpeu_util, only: perr,die
  use mpeu_util, only: tell
  use mpeu_mpif, only: MPI_comm_world
  implicit none

! Declare passed variables
  type(gsi_bundle)   ,dimension(:),intent(in   ) :: dval        ! (nobs_bins)
  type(predictors)                ,intent(in   ) :: dbias
  type(gsi_bundle)   ,dimension(:),intent(in   ) :: xval        ! (nobs_bins)
  type(predictors)                ,intent(in   ) :: xbias
  integer(i_kind)                 ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)) ,intent(in   ) :: sges
  real(r_quad),dimension(:,:,:)  ,intent(inout) :: pbcjo        !  (:,obOper_count,nobs_bins)

! Declare local variables
  character(len=*),parameter:: myname_=myname//"::stpjo"

  integer(i_kind) :: ll,mm,ib
  class(obOper),pointer:: it_obOper
!************************************************************************************  


!$omp parallel do  schedule(dynamic,1) private(ll,mm,ib,it_obOper)
  do mm=1,stpcnt
    ll=ll_jo(mm)
    ib=ib_jo(mm)

    it_obOper => obOper_create(ll)

        if(.not.associated(it_obOper)) then
          call perr(myname_,'unexpected obOper, associated(it_obOper) =',associated(it_obOper))
          call perr(myname_,'                  obOper_typeInfo(ioper) =',obOper_typeInfo(ll))
          call perr(myname_,'                                   iOper =',ll)
          call perr(myname_,'                                    ibin =',ib)
          call perr(myname_,'                                      mm =',mm)
          call perr(myname_,'                                  stpcnt =',stpcnt)
          call  die(myname_)
        endif

        if(.not.associated(it_obOper%obsLL)) then
          call perr(myname_,'unexpected components, associated(%obsLL) =',associated(it_obOper%obsLL))
          call perr(myname_,'                   obOper_typeInfo(ioper) =',obOper_typeInfo(ll))
          call perr(myname_,'                                    iOper =',ll)
          call perr(myname_,'                                     ibin =',ib)
          call perr(myname_,'                                       mm =',mm)
          call perr(myname_,'                                   stpcnt =',stpcnt)
          call  die(myname_)
        endif

    call it_obOper%stpjo(ib,dval(ib),xval(ib),pbcjo(:,ll,ib),sges,nstep,dbias,xbias) 
    call obOper_destroy(it_obOper)
  enddo

return
end subroutine stpjo

subroutine stpjo_setup(nobs_bins)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpjo_setup     setup loops for stpjo
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: setup parallel loops for stpjo
!
! program history log:
!   2015-01-18  derber,j.
!   2016-08-26  guo, j.   - patched with ".or.associated(yobs%o3l)" checking at
!                           the checking of "associated(yobs%oz)", as a minimum
!                           bug fix.
!
!   input argument list:
!     yobs
!     nobs_bins - number of obs bins
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind,r_quad
  use gsi_bundlemod, only: gsi_bundle
  use gsi_obOperTypeManager, only: obOper_count
  use gsi_obOperTypeManager, only: obOper_typeInfo
  use gsi_obOper, only: obOper
  use m_obsdiags, only: obOper_create
  use m_obsdiags, only: obOper_destroy
  use m_obsNode , only: obsNode
  use m_obsLList, only: obsLList_headNode
  use mpeu_util , only: perr, die
  use mpeu_util , only: tell
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: nobs_bins

! Declare local variables
  character(len=*),parameter:: myname_=myname//"::stpjo_setup"

  integer(i_kind) ll,ib
  class(obsNode),pointer:: headNode
  class(obOper ),pointer:: it_obOper
!************************************************************************************
  call init_(obOper_count,nobs_bins)

    stpcnt = 0
    do ll = 1, obOper_count       ! Not nobs_type anymore

      it_obOper => obOper_create(ll)

        if(.not.associated(it_obOper)) then
          call perr(myname_,'unexpected obOper, associated(it_obOper) =',associated(it_obOper))
          call perr(myname_,'                  obOper_typeInfo(ioper) =',obOper_typeInfo(ll))
          call perr(myname_,'                                   ioper =',ll)
          call perr(myname_,'                            obOper_count =',obOper_count)
          call  die(myname_)
        endif

        if(.not.associated(it_obOper%obsLL)) then
          call perr(myname_,'unexpected component, associated(%obsLL) =',associated(it_obOper%obsLL))
          call perr(myname_,'                  obOper_typeInfo(ioper) =',obOper_typeInfo(ll))
          call perr(myname_,'                                   ioper =',ll)
          call perr(myname_,'                            obOper_count =',obOper_count)
          call  die(myname_)
        endif

      do ib = 1,size(it_obOper%obsLL)   ! for all bins
        headNode => obsLList_headNode(it_obOper%obsLL(ib))
        if(.not.associated(headNode)) cycle     ! there is no observation node in this bin

        stpcnt = stpcnt +1
        ll_jo(stpcnt) = ll
        ib_jo(stpcnt) = ib

      enddo     ! ib
      headNode => null()
      call obOper_destroy(it_obOper)
    enddo       ! ll, i.e. ioper of 1:obOper_ubound

    omptasks_configured_ = .true.

  return
end subroutine stpjo_setup

end module stpjomod
