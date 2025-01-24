module intallmod

!$$$ mdoule documentation block
!           .      .    .                                       .
! module:   intallmod    module for intall and its tangent linear intall_tl
!   prgmmr:
!
! abstract: module for intall and its tangent linear intall_tl
!
! program history log:
!   2005-05-18  Yanqiu zhu - wrap intall and its tangent linear intall_tl into one module
!   2005-11-21  Derber - remove interface and clean up code
!   2008-11-26  Todling - remove intall_tl
!   2009-08-13  lueken - update documentation
!   2012-02-08  kleist - changes related to 4d-ensemble-var additions and consolidation of 
!                   int... individual modules to one intjcmod
!   2015-09-03  guo     - obsmod::yobs has been replaced with m_obsHeadBundle,
!                         where yobs is created and destroyed when and where it
!                         is needed.
!   2018-08-10  guo     - removed obsHeadBundle references.
!                       - replaced intjo() related implementations with a new
!                         polymorphic implementation of intjomod::intjo().
!
! subroutines included:
!   sub intall
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block


implicit none

PRIVATE
PUBLIC intall


contains

subroutine intall(sval,sbias,rval,rbias)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intall      calculate RHS for analysis equation
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
!   2007-04-13  tremolet - split Jo and 3dvar components into intjo and int3dvar
!   2007-10-01  todling  - add timers
!   2011-10-20  todling  - observation operators refer to state- not control-vec (cvars->svars)
!   2014-03-19  pondeca -  Add RHS calculation for wspd10m constraint
!   2014-05-07  pondeca -  Add RHS calculation for howv constraint
!   2014-06-17  carley/zhu  - Add RHS calculation for lcbas constraint
!   2015-07-10  pondeca - Add RHS calculation for cldch constraint
!   2019-03-13  eliu    - add precipitation component
!
!   input argument list:
!     sval     - solution on grid
!     sbias
!     rval
!     rbias
!
!   output argument list:      
!     rval     - RHS on grid
!     rbias
!
! remarks:
!     if strong initialization, then svalt, svalp, svaluv, sval_x, sval_y, sval_t
!       are all grid fields after strong initialization.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_quad
  use gsi_4dvar, only: nobs_bins,ltlint,ibin_anl
  use constants, only: zero,zero_quad
  use jcmod, only: ljcpdry,ljc4tlevs,ljcdfi,ljclimqc 
  use jfunc, only: nrclen,nsclen,npclen,ntclen
  use intjomod, only: intjo
  use bias_predictors, only : predictors,assignment(=)
  use state_vectors, only: allocate_state,deallocate_state
  use state_vectors, only: qgpresent,qspresent,qrpresent,qipresent,qlpresent
  use state_vectors, only: cldchpresent,lcbaspresent,howvpresent,wspd10mpresent,pblhpresent,vispresent,gustpresent
  use intjcmod, only: intlimq,intlimg,intlimv,intlimp,intlimw10m,intlimhowv,intlimcldch,&
      intliml,intjcpdry1,intjcpdry2,intjcdfi,intlimqc  
  use timermod, only: timer_ini,timer_fnl
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: assignment(=)
  use guess_grids, only: ntguessig,nfldsig
  use mpl_allreducemod, only: mpl_allreduce

  implicit none

! Declare passed variables
  type(gsi_bundle), intent(in   ) :: sval(nobs_bins)
  type(predictors), intent(in   ) :: sbias
  type(gsi_bundle), intent(inout) :: rval(nobs_bins)
  type(predictors), intent(inout) :: rbias
  real(r_quad),dimension(max(1,nrclen)) :: qpred
  real(r_quad),dimension(2*nobs_bins) :: mass

! Declare local variables
  integer(i_kind) :: ibin,ii,it,i

!******************************************************************************
! Initialize timer
  call timer_ini('intall')

! Zero gradient arrays

  do ii=1,nobs_bins
     rval(ii)=zero
  enddo

  qpred=zero_quad

! Compute RHS in physical space (rval,qpred)
  call intjo(rval,qpred,sval,sbias)

  if(.not.ltlint)then
! RHS for moisture constraint
     if (.not.ljc4tlevs) then
        call intlimq(rval(ibin_anl),sval(ibin_anl),ntguessig)
     else
        do ibin=1,nobs_bins
           if (nobs_bins /= nfldsig) then
              it=ntguessig
           else
              it=ibin
           end if
           call intlimq(rval(ibin),sval(ibin),it)
        end do
     end if
     if (ljclimqc) then
        if (.not.ljc4tlevs) then
           if (qlpresent) call intlimqc(rval(ibin_anl),sval(ibin_anl),ntguessig,'ql')
           if (qipresent) call intlimqc(rval(ibin_anl),sval(ibin_anl),ntguessig,'qi')
           if (qrpresent) call intlimqc(rval(ibin_anl),sval(ibin_anl),ntguessig,'qr')
           if (qspresent) call intlimqc(rval(ibin_anl),sval(ibin_anl),ntguessig,'qs')
           if (qgpresent) call intlimqc(rval(ibin_anl),sval(ibin_anl),ntguessig,'qg')
        else
           do ibin=1,nobs_bins
              if (nobs_bins /= nfldsig) then
                 it=ntguessig
              else
                 it=ibin
              end if
              if (qlpresent) call intlimqc(rval(ibin),sval(ibin),it,'ql')
              if (qipresent) call intlimqc(rval(ibin),sval(ibin),it,'qi')
              if (qrpresent) call intlimqc(rval(ibin),sval(ibin),it,'qr')
              if (qspresent) call intlimqc(rval(ibin),sval(ibin),it,'qs')
              if (qgpresent) call intlimqc(rval(ibin),sval(ibin),it,'qg')
           end do
        end if
     end if  ! ljclimqc
! RHS for gust constraint
     if (gustpresent)call intlimg(rval(1),sval(1))

! RHS for vis constraint
     if (vispresent) call intlimv(rval(1),sval(1))

! RHS for pblh constraint
     if (pblhpresent) call intlimp(rval(1),sval(1))

! RHS for wspd10m constraint
     if (wspd10mpresent) call intlimw10m(rval(1),sval(1))

! RHS for howv constraint
     if (howvpresent) call intlimhowv(rval(1),sval(1))

! RHS for lcbas constraint
     if (lcbaspresent) call intliml(rval(1),sval(1))

! RHS for cldch constraint
     if (cldchpresent) call intlimcldch(rval(1),sval(1))

  end if

! RHS for dry ps constraint: part 1
  if(ljcpdry)then

    if (.not.ljc4tlevs) then
      call intjcpdry1(sval(ibin_anl),1,mass)
    else 
      call intjcpdry1(sval,nobs_bins,mass)
    end if

!   Put reduces together to minimize wait time
!   First, use MPI to get global mean increment
    call mpl_allreduce(2*nobs_bins,qpvals=mass)

  end if

! Sum over all processors for bias correction terms

  call mpl_allreduce(nrclen,qpvals=qpred)


! RHS for dry ps constraint: part 2
  if(ljcpdry)then
    if (.not.ljc4tlevs) then
      call intjcpdry2(rval(ibin_anl),1,mass)
    else 
      call intjcpdry2(rval,nobs_bins,mass)
    end if
  end if

! RHS for Jc DFI
  if (ljcdfi .and. nobs_bins>1) call intjcdfi(rval,sval)

!  Put bias correction terms in correct location
  if(nsclen > 0)then
     do i=1,nsclen
        rbias%predr(i)=qpred(i)
     end do
  end if
  if(npclen > 0)then
     do i=1,npclen
        rbias%predp(i)=qpred(nsclen+i)
     end do
  end if
  if (ntclen>0) then
     do i=1,ntclen
        rbias%predt(i)=qpred(nsclen+npclen+i)
     end do
  end if

! Finalize timer
  call timer_fnl('intall')

return
end subroutine intall

end module intallmod
