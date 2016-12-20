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

use mpl_allreducemod, only: mpl_allreduce
implicit none

PRIVATE
PUBLIC intjo

interface intjo; module procedure &
          intjo_
end interface

contains

subroutine intjo_(yobs,rval,qpred,sval,sbias,ibin)

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
!   2009-11-15  todling  - Protect call to mpl_allreduce (evaljo calls it as well)
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
use kinds, only: r_kind,i_kind,r_quad
use obsmod, only: obs_handle
use jfunc, only: nrclen,nsclen,npclen,ntclen,l_foto,xhat_dt
use bias_predictors, only: predictors
use intaodmod, only: intaod
use inttmod, only: intt
use intwmod, only: intw
use intpsmod, only: intps
use intpwmod, only: intpw
use intqmod, only: intq
use intradmod, only: intrad
use inttcpmod, only: inttcp
use intgpsmod, only: intgps
use intrwmod, only: intrw
use intspdmod, only: intspd
use intsrwmod, only: intsrw
use intsstmod, only: intsst
use intdwmod, only: intdw
use intpcpmod, only: intpcp
use intozmod, only: intoz
use intcomod, only: intco
use intpm2_5mod, only: intpm2_5
use intpm10mod, only: intpm10
use intlagmod, only: intlag
use intgustmod, only: intgust
use intvismod, only: intvis
use intpblhmod, only: intpblh
use intwspd10mmod, only: intwspd10m
use inttd2mmod, only: inttd2m
use intmxtmmod, only: intmxtm
use intmitmmod, only: intmitm
use intpmslmod, only: intpmsl
use inthowvmod, only: inthowv
use inttcamtmod, only: inttcamt
use intlcbasmod, only: intlcbas
use intcldchmod, only: intcldch
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
implicit none

! Declare passed variables
integer(i_kind) , intent(in   ) :: ibin
type(obs_handle), intent(in   ) :: yobs
type(gsi_bundle), intent(in   ) :: sval
type(predictors), intent(in   ) :: sbias
type(gsi_bundle), intent(inout) :: rval
real(r_quad),dimension(max(1,nrclen)), intent(inout) :: qpred

! Declare local variables
integer(i_kind) :: ier
real(r_kind),pointer,dimension(:,:,:) :: xhat_dt_tsen,xhat_dt_q,xhat_dt_t


!******************************************************************************

! Calculate sensible temperature time derivative
  if(l_foto)then
     call gsi_bundlegetpointer(xhat_dt,'tv'  ,xhat_dt_t,   ier)
     call gsi_bundlegetpointer(xhat_dt,'q'   ,xhat_dt_q,   ier)
     call gsi_bundlegetpointer(xhat_dt,'tsen',xhat_dt_tsen,ier)
     call tv_to_tsen(xhat_dt_t,xhat_dt_q,xhat_dt_tsen)
  endif

! RHS for conventional temperatures
  if (ntclen>0) then
     call intt(yobs%t,rval,sval,qpred(nsclen+npclen+1:nrclen),sbias%predt)
  else
     call intt(yobs%t,rval,sval)
  end if

! RHS for precipitable water
  call intpw(yobs%pw,rval,sval)

! RHS for conventional moisture
  call intq(yobs%q,rval,sval)

! RHS for conventional winds
  call intw(yobs%w,rval,sval)

! RHS for radar superob winds
  call intsrw(yobs%srw,rval,sval)

! RHS for lidar winds
  call intdw(yobs%dw,rval,sval)

! RHS for radar winds
  call intrw(yobs%rw,rval,sval)

! RHS for wind speed observations
  call intspd(yobs%spd,rval,sval)

! RHS for ozone observations
  call intoz(yobs%oz,yobs%o3l,rval,sval)

! RHS for carbon monoxide
  call intco(yobs%colvk,rval,sval)

! RHS for pm2_5
  call intpm2_5(yobs%pm2_5,rval,sval)

! RHS for pm10
  call intpm10(yobs%pm10,rval,sval)

! RHS for surface pressure observations
  call intps(yobs%ps,rval,sval)

! RHS for MSLP obs for TCs
  call inttcp(yobs%tcp,rval,sval)

! RHS for conventional sst observations
  call intsst(yobs%sst,rval,sval)

! RHS for GPS local observations
  call intgps(yobs%gps,rval,sval)

! RHS for conventional lag observations
  call intlag(yobs%lag,rval,sval,ibin)

! RHS calculation for radiances
  call intrad(yobs%rad,rval,sval,qpred(1:nsclen),sbias%predr)

! RHS calculation for precipitation
  call intpcp(yobs%pcp,rval,sval)

! RHS calculation for AOD
  call intaod(yobs%aero,rval,sval)

! RHS for conventional gust observations
  call intgust(yobs%gust,rval,sval)

! RHS for conventional vis observations
  call intvis(yobs%vis,rval,sval)

! RHS for conventional pblh observations
  call intpblh(yobs%pblh,rval,sval)

! RHS for conventional wspd10m observations
  call intwspd10m(yobs%wspd10m,rval,sval)

! RHS for conventional td2m observations
  call inttd2m(yobs%td2m,rval,sval)

! RHS for conventional mxtm observations
  call intmxtm(yobs%mxtm,rval,sval)

! RHS for conventional mitm observations
  call intmitm(yobs%mitm,rval,sval)

! RHS for conventional pmsl observations
  call intpmsl(yobs%pmsl,rval,sval)

! RHS for conventional howv observations
  call inthowv(yobs%howv,rval,sval)

! RHS for tcamt observations
  call inttcamt(yobs%tcamt,rval,sval)

! RHS for lcbas observations
  call intlcbas(yobs%lcbas,rval,sval)

! RHS for cldch observations
  call intcldch(yobs%cldch,rval,sval)

! Take care of background error for bias correction terms

return
end subroutine intjo_

end module intjomod
