module intjomod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intjo    module for intjo
!   prgmmr:
!
! abstract: module for H'R^{-1}H
!
! program history log:
!   2008-12-01  Todling - wrap in module
!   2009-08-13  lueken - update documentation
!   2015-09-03  guo     - obsmod::obs_handle has been replaced with obsHeadBundle,
!                         defined by m_obsHeadBundle.
!   2016-08-29  J Guo   - Separated calls to intozlay() and intozlev()
!   2018-08-10  guo     - a new generic intjo() implementation replaced type
!                         specific intXYZ() calls with polymorphic %intjo().
!   2022-03-15 K Apodaca - add CYGNSS and Spire ocean wind speed                        
!
! subroutines included:
!   sub intjo_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use gsi_obOperTypeManager, only: obOper_count
use gsi_obOperTypeManager, only: obOper_typeInfo
use gsi_obOperTypeManager, only: &
  iobOper_t,          iobOper_pw,         iobOper_q,                                                    &
                                          iobOper_cldtot,     iobOper_w,          iobOper_dw,           &
  iobOper_rw,         iobOper_dbz,        iobOper_fed,        iobOper_gnssrspd,                         &
                      iobOper_spd,        iobOper_oz,         iobOper_o3l,        iobOper_colvk,        &
  iobOper_pm2_5,      iobOper_pm10,       iobOper_ps,         iobOper_tcp,        iobOper_sst,          &
  iobOper_gpsbend,    iobOper_gpsref,                                                                   &
                      iobOper_rad,        iobOper_pcp,        iobOper_aero,       iobOper_gust,         &
  iobOper_vis,        iobOper_pblh,       iobOper_wspd10m,    iobOper_td2m,       iobOper_mxtm,         &
  iobOper_mitm,       iobOper_pmsl,       iobOper_howv,       iobOper_tcamt,      iobOper_lcbas,        &
  iobOper_cldch,      iobOper_uwnd10m,    iobOper_vwnd10m,    iobOper_swcp,       iobOper_lwcp,         &
  iobOper_light
use kinds, only: i_kind
use mpeu_util, only: perr,die

implicit none

PRIVATE
PUBLIC intjo

interface intjo; module procedure &
          intjo_, intjo_reduced_
end interface

! This is a mapping to the exact %intjo() calling sequence in the earlier
! non-polymorphic implementation, to reproduce the exactly same summation
! ordering for rval and qpred.  It is not necessary, and can be removed once
! some non-zero-diff modifications are introduced.

integer(i_kind),parameter,dimension(obOper_count):: ix_obtype = (/ &
  iobOper_t,          iobOper_pw,         iobOper_q,                                                    &
                                          iobOper_cldtot,     iobOper_w,          iobOper_dw,           &
  iobOper_rw,         iobOper_dbz,        iobOper_fed,        iobOper_gnssrspd,                         &
                      iobOper_spd,        iobOper_oz,         iobOper_o3l,        iobOper_colvk,        &
  iobOper_pm2_5,      iobOper_pm10,       iobOper_ps,         iobOper_tcp,        iobOper_sst,          &
  iobOper_gpsbend,    iobOper_gpsref,                                                                   &
                      iobOper_rad,        iobOper_pcp,        iobOper_aero,       iobOper_gust,         &
  iobOper_vis,        iobOper_pblh,       iobOper_wspd10m,    iobOper_td2m,       iobOper_mxtm,         &
  iobOper_mitm,       iobOper_pmsl,       iobOper_howv,       iobOper_tcamt,      iobOper_lcbas,        &
  iobOper_cldch,      iobOper_uwnd10m,    iobOper_vwnd10m,    iobOper_swcp,       iobOper_lwcp,         &
  iobOper_light                                                                                         /)
!...|....1....|....2....|....3....|....4....|....5....|....6....|....7....|....8....|....9....|....0

character(len=*),parameter:: myname="intjomod"

contains

subroutine intjo_(rval,qpred,sval,sbias)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intjo      calculate RHS for analysis equation
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: calculate RHS for all variables (nonlinear qc version)
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
!    As a result the same int* routines may be used for use with or without
!    nonlinear quality control.
!    
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
!   2007-03-19  tremolet - binning of observations
!   2007-04-13  tremolet - split jo from other components of intall
!   2007-06-04  derber  - use quad precision to get reproducibility over number of processors
!   2008-11-27  todling  - add tendencies for FOTO support and new interface to int's
!   2009-01-08  todling  - remove reference to ozohead
!   2009-03-23  meunier  - Add call to intlag (lagrangian observations)
!   2010-01-11  zhang,b  - Bug fix: bias predictors need to be accumulated over nbins
!   2010-03-24  zhu      - change the interfaces of intt,intrad,intpcp for generalizing control variable
!   2010-05-13  todling  - harmonized interfaces to int* routines when it comes to state_vector (add only's)
!   2010-06-13  todling  - add intco call
!   2010-10-15  pagowski - add intpm2_5 call
!   2010-10-20  hclin    - added aod
!   2011-02-20  zhu      - add intgust,intvis,intpblh calls
!   2013-05-20  zhu      - add codes related to aircraft temperature bias correction
!   2014-06-18  carley/zhu - add lcbas and tcamt 
!   2014-03-19  pondeca  - add intwspd10m
!   2014-04-10  pondeca  - add inttd2m,intmxtm,intmitm,intpmsl
!   2014-05-07  pondeca  - add inthowv
!   2015-07-10  pondeca  - add intcldch
!   2016-03-07  pondeca  - add intuwnd10m,intvwnd10m
!
!   input argument list:
!     ibin
!     yobs
!     sval     - solution on grid
!     sbias
!     rval
!     qpred
!
!   output argument list:      
!     rval     - RHS on grid
!     qpred
!
! remarks:
!     1) if strong initialization, then svalt, svalp, svaluv
!         are all grid fields after strong initialization.
!
!     2) The two interfaces to the int-routines should be temporary.
!        In the framework of the 4dvar-code, foto can be re-implemented as 
!        an approximate M and M' to the model matrices in 4dvar. Once that
!        is done, the int-routines should no longer need the time derivatives.
!        (Todling)
!     3) Notice that now (2010-05-13) int routines handle non-essential
!        variables internally; also, when pointers non-existent, int routines 
!        simply return (Todling).
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
use kinds, only: i_kind,r_quad
use gsi_bundlemod, only: gsi_bundle
use bias_predictors, only: predictors
use m_obsdiags, only: obOper_create
use m_obsdiags, only: obOper_destroy
use gsi_obOper, only: obOper

implicit none

! Declare passed variables
type(gsi_bundle), dimension(  :), intent(inout) :: rval         ! (nobs_bins)
type(gsi_bundle), dimension(  :), intent(in   ) :: sval         ! (nobs_bins)
real(r_quad    ), dimension(:,:), intent(inout) :: qpred        ! (:,nobs_bins)
type(predictors),                 intent(in   ) :: sbias

character(len=*),parameter:: myname_=myname//"::intjo_"

! Declare local variables
integer(i_kind):: ibin,it,ix
class(obOper),pointer:: it_obOper

!******************************************************************************

! "RHS for jo", as it was labeled in intall().
!$omp parallel do  schedule(dynamic,1) private(ibin,it,ix,it_obOper)
  do ibin=1,size(sval)
    do it=1,obOper_count
      !ix=ix_obtype(it)  ! Use this line to ensure the same jo summartion
                         ! sequence as intjo was in its early implementation,
                         ! for reproducibility.

      ix=it     ! Using this line, jo summation sequence is not the same as
                ! it used to be, nor the same if someone chooses to change
                ! enumration sequence of obOpers in gsi_obOperTypeManager.F90.
                ! But it would make this code more portable to new obOper
                ! extensions.

      it_obOper => obOper_create(ix)

        if(.not.associated(it_obOper)) then
          call perr(myname_,'unexpected obOper, associated(it_obOper) =',associated(it_obOper))
          call perr(myname_,'                  obOper_typeInfo(ioper) =',obOper_typeInfo(ix))
          call perr(myname_,'                                   ioper =',ix)
          call perr(myname_,'                                      it =',it)
          call perr(myname_,'                            obOper_count =',obOper_count)
          call perr(myname_,'                                    ibin =',ibin)
          call  die(myname_)
        endif

        if(.not.associated(it_obOper%obsLL)) then
          call perr(myname_,'unexpected component, associated(%obsLL) =',associated(it_obOper%obsLL))
          call perr(myname_,'                  obOper_typeInfo(ioper) =',obOper_typeInfo(ix))
          call perr(myname_,'                                   ioper =',ix)
          call perr(myname_,'                                      it =',it)
          call perr(myname_,'                            obOper_count =',obOper_count)
          call perr(myname_,'                                    ibin =',ibin)
          call  die(myname_)
        endif

      call it_obOper%intjo(ibin,rval(ibin),sval(ibin),qpred(:,ibin),sbias)
      call obOper_destroy(it_obOper)
    enddo
  end do

return
end subroutine intjo_

subroutine intjo_reduced_(rval,qpred,sval,sbias)
  use kinds, only: i_kind,r_quad
  use gsi_bundlemod, only: gsi_bundle
  use bias_predictors, only: predictors
  use constants, only: zero_quad
  implicit none
! Declare passed variables
  type(gsi_bundle), dimension(:), intent(inout) :: rval
  real(r_quad    ), dimension(:), intent(inout) :: qpred

  type(gsi_bundle), dimension(:), intent(in   ) :: sval
  type(predictors),               intent(in   ) :: sbias

!----------------------------------------
  real(kind(qpred)),allocatable,dimension(:,:):: qpred_bin
  integer(i_kind):: ibin

  allocate(qpred_bin(size(qpred),size(rval)))
  qpred_bin=zero_quad

  call intjo_(rval,qpred_bin,sval,sbias)

  do ibin=1,size(rval)
    qpred(:)=qpred(:)+qpred_bin(:,ibin)
  enddo

  deallocate(qpred_bin)

end subroutine intjo_reduced_

end module intjomod
