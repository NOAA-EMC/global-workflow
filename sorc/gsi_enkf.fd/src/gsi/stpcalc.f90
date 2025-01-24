module stpcalcmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpcalcmod    module for stpcalc 
!  prgmmr:
!
! abstract: module for stpcalc 
!
! program history log:
!   2005-05-21  Yanqiu zhu - wrap stpcalc and its tangent linear stpcalc_tl into one module
!   2005-11-21  Derber - remove interfaces and clean up code
!   2008-12-02  Todling - remove stpcalc_tl
!   2009-08-12  lueken  - updated documentation
!   2012-02-08  kleist  - consolidate weak constaints into one module stpjcmod.
!   2015-09-03  guo     - obsmod::yobs has been replaced with m_obsHeadBundle,
!                         where yobs is created and destroyed when and where it
!                         is needed.
!   2018-05-19  eliu    - add precipitation component in moisture constraint
!   2018-08-10  guo     - removed obsHeadBundle references.
!                       - replaced stpjo() with a new polymorphic stpjomod::stpjo().
!   2019-08-06  guo     - corrected ctype contents for new moisture constaints.
!                       . added n0 to the argument list of prnt_j() to separate
!                         the observation section from the leading section of
!                         pj, to help future pj content extension.
!
! subroutines included:
!   sub stpcalc
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpcalc

contains

subroutine stpcalc(stpinout,sval,sbias,dirx,dval,dbias, &
                   diry,penalty,penaltynew,pjcost,pjcostnew,end_iter)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpcalc     calculate penalty and stepsize
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: calculate current penalty and estimate stepsize
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
!    step size operators each stepsize estimate for each observation type, 
!    given the current step size algorithm coded below. 
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
!   2007-03-19  tremolet - binning of observations
!   2007-04-13  tremolet - split Jo and 3dvar components into stpjo and stp3dvar
!   2006-07-26  parrish - correct inconsistency in computation of space and time derivatives of q
!                          currently, if derivatives computed, for q it is normalized q, but
!                          should be mixing ratio.
!   2006-08-04  parrish - add strong constraint initialization option
!   2006-09-18  derber - modify output from nonlinear operators to make same as linear operators
!   2006-09-20  derber - add sensible temperatures for conventional obs.
!   2006-10-12  treadon - replace virtual temperature with sensible in stppcp
!   2007-04-16  kleist  - modified calls to tendency and constraint routines
!   2007-06-04  derber  - use quad precision to get reproduceability over number of processors
!   2007-07-26  cucurull - update gps code to generalized vertical coordinate;
!                          get current solution for 3d pressure (xhat_3dp);
!                          move getprs_tl out of calctends_tl; add dirx3dp
!                          and remove ps in calctends_tl argument list;
!                          use getprs_tl 
!   2007-08-08  derber - optimize, ensure that only necessary time derivatives are calculated
!   2007-10-01  todling - add timers
!   2008-11-28  todling - revisited Tremolet's split in light of changes from May08 version
!   2009-06-02  derber - modify the calculation of the b term for the background to increase accuracy
!   2010-06-01  treadon - accumulate pbcjo over nobs_bins 
!   2010-08-19  lueken - add only to module use
!   2010-09-14  derber - clean up quad precision
!   2011-02-25  zhu    - add gust,vis,pblh calls
!   2013-03-19  pondeca - add wspd10m call. introduce parameter n0 to make it  easier to add
!                         more weak constraint contributions. update comment block to indicate 
!                         the correct observation type associated with each pbc(*,j) term
!   2014-05-07  pondeca - add howv call
!   2014-06-17  carley/zhu - add tcamt and lcbas
!   2015-07-10  pondeca - add cldch
!   2016-02-03  derber - add code to search through all of the possible stepsizes tried, to find the 
!               one that minimizes the most and use that one.
!   2016-08-08  j guo   - tried to edit some comments for a better description on pbc(*,:) elements
!                         reflecting jo terms.
!
!   input argument list:
!     stpinout - guess stepsize
!     sval     - current solution
!     dirx     - search direction for x
!     diry     - search direction for y (B-1 dirx)
!     end_iter - end iteration flag
!     dval
!     sbias,dbias
!
!   output argument list:
!     stpinout    - final estimate of stepsize
!     penalty     - penalty current solution
!     penaltynew  - estimate of penalty for new solution
!     end_iter    - end iteration flag false if stepsize successful
!     pjcost      - 4 major penalty terms current solution
!     pjcostnew   - 4 major penalty terms estimate new solution
!
! remarks:
!     The part of xhat and dirx containing temps and psfc are values before strong initialization,
!     xhatt, xhatp and dirxt, dirxp contain temps and psfc after strong initialization.
!     If strong initialization is turned off, then xhatt, etc are equal to the corresponding 
!     fields in xhat, dirx.
!     xhatuv, xhat_y, xhat_t and dirxuv, dirx_t are all after
!     strong initialization if it is turned on.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad,r_single
  use mpimod, only: mype
  use constants, only: zero,one_quad,zero_quad
  use gsi_4dvar, only: nobs_bins,ltlint,ibin_anl
  use jfunc, only: iout_iter,nclen,xhatsave,yhatsave,&
       iter,nrclen
  use jcmod, only: ljcpdry,ljc4tlevs,ljcdfi,ljclimqc 
  use gsi_obOperTypeManager, only: nobs_type => obOper_count
  use stpjcmod, only: stplimq,stplimg,stplimv,stplimp,stplimw10m,&
       stplimhowv,stplimcldch,stpjcdfi,stpjcpdry,stpliml,stplimqc  
  use bias_predictors, only: predictors
  use control_vectors, only: control_vector,qdot_prod_sub  
  use state_vectors, only: qgpresent,qspresent,qrpresent,qipresent,qlpresent
  use state_vectors, only: cldchpresent,lcbaspresent,howvpresent,wspd10mpresent,pblhpresent,vispresent,gustpresent
  use state_vectors, only: allocate_state,deallocate_state
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: assignment(=)
  use guess_grids, only: ntguessig,nfldsig
  use mpl_allreducemod, only: mpl_allreduce
  use timermod, only: timer_ini,timer_fnl
  use stpjomod, only: stpjo
  use gsi_io, only: verbose
  use gridmod, only: minmype
  implicit none

! Declare passed variables
  real(r_kind)        ,intent(inout) :: stpinout
  logical             ,intent(inout) :: end_iter
  real(r_kind)        ,intent(  out) :: penalty,penaltynew
  real(r_kind)        ,intent(  out) :: pjcost(4),pjcostnew(4)

  type(control_vector),intent(in   ) :: dirx,diry
  type(gsi_bundle)    ,intent(inout) :: sval(nobs_bins)
  type(gsi_bundle)    ,intent(in   ) :: dval(nobs_bins)
  type(predictors)    ,intent(inout) :: sbias
  type(predictors)    ,intent(in   ) :: dbias


! Declare local parameters
  integer(i_kind),parameter:: n0 = 17 
  integer(i_kind),parameter:: ipen = n0+nobs_type
  integer(i_kind),parameter:: istp_iter = 5
  integer(i_kind),parameter:: ipenlin = 3
  integer(i_kind),parameter:: ioutpen = istp_iter*4
  real(r_quad),parameter:: one_tenth_quad = 0.1_r_quad 

! Declare local variables
  integer(i_kind) i,j,mm1,ii,final_ii,ibin,ipenloc,it
  integer(i_kind) istp_use,nstep,nsteptot,kprt
  real(r_quad),dimension(4,ipen):: pbc
  real(r_quad),dimension(4,nobs_type):: pbcjo 
  real(r_quad),dimension(4,nobs_type,nobs_bins):: pbcjoi 
  real(r_quad),dimension(4):: pbcqmin,pbcqmax
  real(r_quad),dimension(4,nobs_bins):: pbcql,pbcqi,pbcqr,pbcqs,pbcqg  
  real(r_quad),dimension(ipen):: pen_est
  real(r_quad),dimension(3,ipenlin):: pstart 
  real(r_quad) bx,cx,ccoef,bcoef,dels,sges1,sgesj
  real(r_quad),dimension(0:istp_iter):: stp   
  real(r_kind),dimension(istp_iter):: stprat
  real(r_quad),dimension(ipen):: bsum,csum
  real(r_quad),dimension(ipen,nobs_bins):: pj
  real(r_kind) delpen
  real(r_kind) outpensave
  real(r_kind),dimension(4)::sges
  real(r_kind),dimension(ioutpen):: outpen,outstp
  logical :: print_verbose,pjcalc


!************************************************************************************  
! Initialize timer
  call timer_ini('stpcalc')

! Initialize variable
  print_verbose=.false.
  if(verbose)print_verbose=.true.
  mm1=mype+1
  stp(0)=stpinout
  outpen = zero
  nsteptot=0
  istp_use=0
  kprt=3
  pjcalc=.false.
  pj=zero_quad
  final_ii=1

!   Begin calculating contributions to penalty and stepsize for various terms
!
!   stepsize = sum(b)/sum(c)
!
!   Differences used for 2-4 to reduce round-off error
!
!    pbc(1,*) - stepsize sges(1) penalty 
!    pbc(2,*) - stepsize sges(2) penalty - sges(1) penalty
!    pbc(3,*) - stepsize sges(3) penalty - sges(1) penalty
!    pbc(4,*) - stepsize sges(4) penalty - sges(1) penalty
!
!    linear terms -> pbc(*,1:ipenlin=3)
!    pbc(*,1)  contribution from background, sat rad bias, and precip bias
!    pbc(*,2)  placeholder for future linear linear term
!    pbc(*,3)  contribution from dry pressure constraint term (Jc)
!
!    nonlinear terms -> pbc(*,4:n0)
!    pbc(*,4)  contribution from negative moisture constraint term (Jl/Jq)
!    pbc(*,5)  contribution from excess moisture term (Jl/Jq)
!    pbc(*,6) contribution from negative gust constraint term (Jo)
!    pbc(*,7) contribution from negative vis constraint term (Jo)
!    pbc(*,8) contribution from negative pblh constraint term (Jo)
!    pbc(*,9) contribution from negative wspd10m constraint term (Jo)
!    pbc(*,10) contribution from negative howv constraint term (Jo)
!    pbc(*,11) contribution from negative lcbas constraint term (Jo)
!    pbc(*,12) contribution from negative cldch constraint term (Jo)
!    pbc(*,13) contribution from negative ql constraint term (Jl/Jg)
!    pbc(*,14) contribution from negative qi constraint term (Jl/Jg)
!    pbc(*,15) contribution from negative qr constraint term (Jl/Jg)
!    pbc(*,16) contribution from negative qs constraint term (Jl/Jg)
!    pbc(*,17) contribution from negative qg constraint term (Jl/Jg)
!
!    Under polymorphism the following is the contents of pbs:
!    linear terms => pbcjo(*,n0+1:n0+nobs_type),
!       pbc  (*,n0+j) := pbcjo(*,j); for j=1,nobs_type
!    where,
!       pbcjo(*,   j) := sum( pbcjoi(*,j,1:nobs_bins) )
!
!    The original (wired) implementation of obs-types has
!    the extra contents of pbc defined as:
!
!    pbc(*,18) contribution from ps observation  term (Jo)
!    pbc(*,19) contribution from t observation  term (Jo)
!    pbc(*,20) contribution from w observation  term (Jo)
!    pbc(*,21) contribution from q observation  term (Jo)
!    pbc(*,22) contribution from spd observation  term (Jo)
!    pbc(*,23) contribution from rw observation  term (Jo)
!    pbc(*,24) contribution from dw observation  term (Jo)
!    pbc(*,25) contribution from sst observation  term (Jo)
!    pbc(*,26) contribution from pw observation  term (Jo)
!    pbc(*,27) contribution from pcp observation  term (Jo)
!    pbc(*,28) contribution from oz observation  term (Jo)
!    pbc(*,29) contribution from o3l observation  term (Jo)(not used)
!    pbc(*,30) contribution from gps bending angle observation  term (Jo)
!    pbc(*,31) contribution from gps refractivity  observation  term (Jo)
!    pbc(*,32) contribution from rad observation  term (Jo)
!    pbc(*,33) contribution from tcp observation  term (Jo)
!    pbc(*,34) contribution from lag observation  term (Jo)
!    pbc(*,35) contribution from colvk observation  term (Jo)
!    pbc(*,36) contribution from aero observation  term (Jo)
!    pbc(*,37) contribution from aerol observation  term (Jo)
!    pbc(*,38) contribution from pm2_5 observation  term (Jo)
!    pbc(*,39) contribution from gust observation  term (Jo)
!    pbc(*,40) contribution from vis observation  term (Jo)
!    pbc(*,41) contribution from pblh observation  term (Jo)
!    pbc(*,42) contribution from wspd10m observation  term (Jo)
!    pbc(*,43) contribution from td2m observation  term (Jo)
!    pbc(*,44) contribution from mxtm observation  term (Jo)
!    pbc(*,45) contribution from mitm observation  term (Jo)
!    pbc(*,46) contribution from pmsl observation  term (Jo)
!    pbc(*,47) contribution from howv observation  term (Jo)
!    pbc(*,48) contribution from tcamt observation  term (Jo)
!    pbc(*,49) contribution from lcbas observation  term (Jo)
!    pbc(*,50) contribution from pm10 observation  term (Jo)
!    pbc(*,51) contribution from cldch observation  term (Jo)
!    pbc(*,52) contribution from uwnd10m observation  term (Jo)
!    pbc(*,53) contribution from vwnd10m observation  term (Jo)
!
!    Users should be awared that under polymorphism, obOper types are defined on
!    the fly.  Such that the second index of pbc(*,:) listed above for n0:1 and
!    above, is no longer reflecting their actual location in arrays, e.g. pbc,
!    pj, etc..  The actual indices for all obOper types are defined as
!    enumerators in module gsi_obOperTypeManager, for any given build.  These
!    indices are referenceable as public iobOper_xxx integer parameters from
!    there, if one has to know or to reference them explicitly.

  pstart=zero_quad
  if(iter == 0 .and. kprt >= 2)pjcalc=.true.



! penalty, b and c for background terms

  pstart(1,1) = qdot_prod_sub(xhatsave,yhatsave)
  if(pjcalc)pj(1,1)=pstart(1,1)

!  two terms in next line should be the same, but roundoff makes average more accurate.

  pstart(2,1) =-0.5_r_quad*(qdot_prod_sub(dirx,yhatsave)+qdot_prod_sub(diry,xhatsave))

  pstart(3,1) = qdot_prod_sub(dirx,diry)


! Contraints and 3dvar terms

! Penalty, b, c for JcDFI

  if (ljcdfi .and. nobs_bins>1) then
    call stpjcdfi(dval,sval,pstart(1,2),pstart(2,2),pstart(3,2))
    if(pjcalc)pj(2,1)=pstart(1,2)
  end if

! Penalty, b, c for dry pressure
  if(ljcpdry)then
    if (.not.ljc4tlevs) then
       call stpjcpdry(dval(ibin_anl),sval(ibin_anl),pstart(1,3),pstart(2,3),pstart(3,3),1)
    else
       call stpjcpdry(dval,sval,pstart(1,3),pstart(2,3),pstart(3,3),nobs_bins)
    end if
    if(pjcalc)pj(3,1)=pstart(1,3)
  end if

! iterate over number of stepsize iterations (istp_iter - currently set to a maximum of 5)
  dels = one_tenth_quad
  stepsize: do ii=1,istp_iter

     pbc=zero_quad
     pjcalc=.false.
     if(iter == 0 .and. kprt >= 2 .and. ii == 1)pjcalc=.true.
!    Delta stepsize
  
     sges(1)= stp(ii-1)
     sges(2)=(one_quad-dels)*stp(ii-1)
     sges(3)=(one_quad+dels)*stp(ii-1)


     if(ii == 1)then
!       First stepsize iteration include current J calculation in position ipenloc
        nstep=4
        sges(4)=zero
        ipenloc=4
     else
!       Later stepsize iteration include only stepsize and stepsize +/- dels
        nstep=3
     end if

!    Calculate penalty values for linear terms
     do i=1,ipenlin
        sges1=real(sges(1),r_quad)
        pbc(1,i)=pstart(1,i)-(2.0_r_quad*pstart(2,i)-pstart(3,i)*sges1)*sges1
        do j=2,nstep
           sgesj=real(sges(j),r_quad)
           pbc(j,i)=(-2.0_r_quad*pstart(2,i)+pstart(3,i)*(sgesj+sges1))*(sgesj-sges1)
        end do
     end do

!    Do nonlinear terms

!    penalties for moisture constraint
     if(.not. ltlint)then
!$omp parallel sections
!$omp section
        if(.not.ljc4tlevs) then
           call stplimq(dval(ibin_anl),sval(ibin_anl),sges,pbc(1,4),pbc(1,5),nstep,ntguessig)
           if(pjcalc)then
               pj(4,1)=pbc(1,4)+pbc(ipenloc,4)
               pj(5,1)=pbc(1,5)+pbc(ipenloc,5)
           end if
        else
           do ibin=1,nobs_bins
              if (nobs_bins /= nfldsig) then
                 it=ntguessig
              else
                 it=ibin
              end if
              call stplimq(dval(ibin),sval(ibin),sges,pbcqmin,pbcqmax,nstep,it)
              do j=1,nstep
                 pbc(j,4) = pbc(j,4)+pbcqmin(j)
                 pbc(j,5) = pbc(j,5)+pbcqmax(j)
              end do
              if(pjcalc)then
                 pj(4,ibin)=pj(4,ibin)+pbcqmin(1)+pbcqmin(ipenloc)
                 pj(5,ibin)=pj(5,ibin)+pbcqmax(1)+pbcqmax(ipenloc)
              end if
           end do
        end if
!$omp section
!       penalties for gust constraint
        if(gustpresent) then
           call stplimg(dval(1),sval(1),sges,pbc(1,6),nstep)
           if(pjcalc)pj(6,1)=pbc(1,6)+pbc(ipenloc,6)
        end if

!$omp section
!       penalties for vis constraint
        if(vispresent) then
           call stplimv(dval(1),sval(1),sges,pbc(1,7),nstep)
           if(pjcalc)pj(7,1)=pbc(1,7)+pbc(ipenloc,7)
        end if

!       penalties for pblh constraint
!$omp section
        if(pblhpresent) then
           call stplimp(dval(1),sval(1),sges,pbc(1,8),nstep)
           if(pjcalc)pj(8,1)=pbc(1,8)+pbc(ipenloc,8)
        end if

!       penalties for wspd10m constraint
!$omp section
        if(wspd10mpresent) then
           call stplimw10m(dval(1),sval(1),sges,pbc(1,9),nstep)
           if(pjcalc)pj(9,1)=pbc(1,9)+pbc(ipenloc,9)
        end if

!       penalties for howv constraint
!$omp section
        if(howvpresent) then
           call stplimhowv(dval(1),sval(1),sges,pbc(1,10),nstep)
           if(pjcalc)pj(10,1)=pbc(1,10)+pbc(ipenloc,10)
        end if

!       penalties for lcbas constraint
!$omp section
        if(lcbaspresent) then
           call stpliml(dval(1),sval(1),sges,pbc(1,11),nstep) 
           if(pjcalc)pj(11,1)=pbc(1,11)+pbc(ipenloc,11)
        end if

!       penalties for cldch constraint
!$omp section
        if(cldchpresent) then
           call stplimcldch(dval(1),sval(1),sges,pbc(1,12),nstep)
           if(pjcalc)pj(12,1)=pbc(1,12)+pbc(ipenloc,12)
        end if

!       if (ljclimqc) then
!$omp section
       if (qlpresent .and. ljclimqc ) then
         if(.not.ljc4tlevs) then
            call stplimqc(dval(ibin_anl),sval(ibin_anl),sges,pbc(1,13),nstep,ntguessig,'ql')
            if(pjcalc) pj(13,1)=pbc(1,13)+pbc(ipenloc,13)
         else
            do ibin=1,nobs_bins
               if (nobs_bins /= nfldsig) then
                  it=ntguessig
               else
                  it=ibin
               end if
               call stplimqc(dval(ibin),sval(ibin),sges,pbcql(1,ibin),nstep,it,'ql')
            end do
            do ibin=1,nobs_bins
               do j=1,nstep
                  pbc(j,13) = pbc(j,13)+pbcql(j,ibin)
               end do
            end do
            if(pjcalc)then
               do ibin=1,nobs_bins
                  pj(13,ibin)=pj(13,ibin)+pbcql(1,ibin)+pbcql(ipenloc,ibin)
               end do
            end if
         end if
       end if
!$omp section
       if (qipresent .and. ljclimqc ) then
         if(.not.ljc4tlevs) then
            call stplimqc(dval(ibin_anl),sval(ibin_anl),sges,pbc(1,14),nstep,ntguessig,'qi')
            if(pjcalc) pj(14,1)=pbc(1,14)+pbc(ipenloc,14)
         else
            do ibin=1,nobs_bins
               if (nobs_bins /= nfldsig) then
                  it=ntguessig
               else
                  it=ibin
               end if
               call stplimqc(dval(ibin),sval(ibin),sges,pbcqi(1,ibin),nstep,it,'qi')
            end do
            do ibin=1,nobs_bins
               do j=1,nstep
                  pbc(j,14) = pbc(j,14)+pbcqi(j,ibin)
               end do
            end do
            if(pjcalc)then
               do ibin=1,nobs_bins
                  pj(14,ibin)=pj(14,ibin)+pbcqi(1,ibin)+pbcqi(ipenloc,ibin)
               end do
            end if
         end if
       end if
!$omp section
       if (qrpresent .and. ljclimqc ) then
         if(.not.ljc4tlevs) then
            call stplimqc(dval(ibin_anl),sval(ibin_anl),sges,pbc(1,15),nstep,ntguessig,'qr')
            if(pjcalc) pj(15,1)=pbc(1,15)+pbc(ipenloc,15)
         else
            do ibin=1,nobs_bins
               if (nobs_bins /= nfldsig) then
                  it=ntguessig
               else
                  it=ibin
               end if
               call stplimqc(dval(ibin),sval(ibin),sges,pbcqr(1,ibin),nstep,it,'qr')
            end do
            do ibin=1,nobs_bins
               do j=1,nstep
                  pbc(j,15) = pbc(j,15)+pbcqr(j,ibin)
               end do
            end do
            if(pjcalc)then
               do ibin=1,nobs_bins
                  pj(15,ibin)=pj(15,ibin)+pbcqr(1,ibin)+pbcqr(ipenloc,ibin)
               end do
            end if
         end if
       end if
!$omp section
       if (qspresent .and. ljclimqc ) then
         if(.not.ljc4tlevs) then
            call stplimqc(dval(ibin_anl),sval(ibin_anl),sges,pbc(1,16),nstep,ntguessig,'qs')
            if(pjcalc) pj(16,1)=pbc(1,16)+pbc(ipenloc,16)
         else
            do ibin=1,nobs_bins
               if (nobs_bins /= nfldsig) then
                  it=ntguessig
               else
                  it=ibin
               end if
               call stplimqc(dval(ibin),sval(ibin),sges,pbcqs(1,ibin),nstep,it,'qs')
            end do
            do ibin=1,nobs_bins
               do j=1,nstep
                  pbc(j,16) = pbc(j,16)+pbcqs(j,ibin)
               end do
            end do
            if(pjcalc)then
               do ibin=1,nobs_bins
                  pj(16,ibin)=pj(16,ibin)+pbcqs(1,ibin)+pbcqs(ipenloc,ibin)
               end do
            end if
         end if
       end if
!$omp section
       if (qgpresent .and. ljclimqc ) then
         if(.not.ljc4tlevs) then
            call stplimqc(dval(ibin_anl),sval(ibin_anl),sges,pbc(1,17),nstep,ntguessig,'qg')
            if(pjcalc) pj(17,1)=pbc(1,17)+pbc(ipenloc,17)
         else
            do ibin=1,nobs_bins
               if (nobs_bins /= nfldsig) then
                  it=ntguessig
               else
                  it=ibin
               end if
               call stplimqc(dval(ibin),sval(ibin),sges,pbcqg(1,ibin),nstep,it,'qg')
            end do
            do ibin=1,nobs_bins
               do j=1,nstep
                  pbc(j,17) = pbc(j,17)+pbcqg(j,ibin)
               end do
            end do
            if(pjcalc)then
               do ibin=1,nobs_bins
                  pj(17,ibin)=pj(17,ibin)+pbcqg(1,ibin)+pbcqg(ipenloc,ibin)
               end do
            end if
         end if
       end if
!$omp end parallel sections
!       end if ! ljclimqc
     end if


!    penalties for Jo
     pbcjoi=zero_quad 
     call stpjo(dval,dbias,sval,sbias,sges,pbcjoi,nstep)

     pbcjo=zero_quad
     do ibin=1,nobs_bins          ! == obs_bins
        do j=1,nobs_type
           do i=1,nstep
              pbcjo(i,j)=pbcjo(i,j)+pbcjoi(i,j,ibin) 
           end do 
        end do 
     enddo
     do j=1,nobs_type
        do i=1,nstep
           pbc(i,n0+j)=pbcjo(i,j) 
        end do 
     end do 
     if(pjcalc)then
        do ibin=1,nobs_bins
           do j=1,nobs_type
              pj(n0+j,ibin)=pj(n0+j,ibin)+pbcjoi(ipenloc,j,ibin)+pbcjoi(1,j,ibin)
           end do 
        enddo
     endif

!    Gather J contributions
     call mpl_allreduce(4,ipen,pbc)
  
!    save penalty  and stepsizes
     nsteptot=nsteptot+1
     do j=1,ipen
        outpen(nsteptot) = outpen(nsteptot)+pbc(1,j)
     end do
     outstp(nsteptot) = sges(1)
     do i=2,nstep
        nsteptot=nsteptot+1
        do j=1,ipen
           outpen(nsteptot) = outpen(nsteptot)+pbc(i,j)+pbc(1,j)
        end do
        outstp(nsteptot) = sges(i)
     end do

!    estimate and sum b and c
!    estimate stepsize contributions for each term
     bcoef=0.25_r_quad/(dels*stp(ii-1))
     ccoef=0.5_r_quad/(dels*dels*stp(ii-1)*stp(ii-1))
     bx=zero_quad
     cx=zero_quad
     do i=1,ipen
        bsum(i)=bcoef*(pbc(2,i)-pbc(3,i))
        csum(i)=ccoef*(pbc(2,i)+pbc(3,i))
        bx=bx+bsum(i)
        cx=cx+csum(i)
     end do

!    estimate of stepsize

     istp_use=ii
     stp(ii)=stp(ii-1)
     if(cx > 1.e-20_r_quad) stp(ii)=stp(ii)+bx/cx         ! step size estimate

!    estimate various terms in penalty on first iteration
     if(ii == 1)then
        pjcost(1) =  pbc(1,1)+pbc(ipenloc,1)                    ! Jb
        pjcost(2) = zero_quad
        do i=1,nobs_type
           pjcost(2) = pjcost(2)+pbc(1,n0+i)+pbc(ipenloc,n0+i)  ! Jo
        end do
        pjcost(3) = pbc(1,2)   + pbc(1,3)+pbc(ipenloc,3)     ! Jc
        pjcost(4) = zero_quad
        do i=4,n0
           pjcost(4) = pjcost(4) + pbc(1,i)+pbc(ipenloc,i)      ! Jl
        end do

        penalty=pjcost(1)+pjcost(2)+pjcost(3)+pjcost(4)    ! J = Jb + Jo + Jc +Jl

!    Write out detailed results to iout_iter
        if(mype == minmype) then
           write(iout_iter,100) (pbc(1,i)+pbc(ipenloc,i),i=1,ipen)
           if(print_verbose)then
              write(iout_iter,105) (bsum(i),i=1,ipen)
              write(iout_iter,110) (csum(i),i=1,ipen)
           end if
        end if
     endif

     if(cx <= 1.e-20_r_quad .or. stp(ii) <= zero_quad)then
!    Check for cx <= 0 or. stp(ii) < zero. (probable error or large nonlinearity)
        if(mype == minmype) then
          write(iout_iter,*) ' entering cx <=0 or stp <= 0 stepsize option',cx,stp(ii)
          write(iout_iter,105) (bsum(i),i=1,ipen)
          write(iout_iter,110) (csum(i),i=1,ipen)
        end if
        stp(ii)=outstp(ipenloc)
        outpensave=outpen(ipenloc)
        do i=1,ii
           if(outpen(i) < outpensave)then
              outpensave=outpen(i)
              istp_use=i
           end if
        end do
        if(istp_use /= ii .and. stp(istp_use) > zero_quad)then
           if(mype == minmype)then
              write(iout_iter,*) ' early termination due to cx or stp  <=0 ',cx,stp(ii)
              write(iout_iter,*) ' better stepsize found',cx,stp(ii)
           end if
           final_ii=ii
           exit stepsize
        else if(ii == istp_iter)then
           if(mype == minmype)then
              write(iout_iter,*) ' early termination due to no decrease in penalty ',cx,stp(ii)
           end if
           stp(istp_use)=zero
           end_iter = .true.
           final_ii=ii
           exit stepsize
        else
!       Try different (better?) stepsize
           stp(ii)=one_tenth_quad*max(outstp(1),1.0e-20_r_kind)
        end if
     else

!    estimate of change in penalty
        delpen = stp(ii)*(bx - 0.5_r_quad*stp(ii)*cx ) 

!    If change in penalty is very small end stepsize calculation
        if(abs(delpen/penalty) < 1.e-17_r_kind) then
           if(mype == minmype)then
              write(iout_iter,*) ' minimization has converged '
              write(iout_iter,140) ii,delpen,bx,cx,stp(ii)
              write(iout_iter,100) (pbc(1,i)+pbc(ipenloc,i),i=1,ipen)
              if(print_verbose)then
                 write(iout_iter,105) (bsum(i),i=1,ipen)
                 write(iout_iter,110) (csum(i),i=1,ipen)
              end if
           end if
           end_iter = .true.
!          Finalize timer
           call timer_fnl('stpcalc')
           final_ii=ii
           exit stepsize
        end if
!       Check for convergence in stepsize estimation
        stprat(ii)=zero
        if(stp(ii) > zero_quad)stprat(ii)=abs((stp(ii)-stp(ii-1))/stp(ii))
        if(stprat(ii) < 1.e-4_r_kind) then
           final_ii=ii
           exit stepsize
        end if
        dels = one_tenth_quad*dels
     end if

100  format(' J=',3e25.18/,(3x,3e25.18))
101  format('EJ=',3e25.18/,(3x,3e25.18))
105  format(' b=',3e25.18/,(3x,3e25.18))
110  format(' c=',3e25.18/,(3x,3e25.18))
130  format('***WARNING***  negative or small cx inner', &
            ' iteration terminated - probable error',i2,3e25.18)
140  format('***WARNING***  expected penalty reduction small ',/,  &
            ' inner iteration terminated - probable convergence',i2,4e25.18)
141  format('***WARNING***  reduced penalty not found in search direction',/,  &
            ' - probable error',(5e25.18))

!    If stepsize estimate has not converged use best stepsize estimate or zero
     if( ii == istp_iter)then
        stp(ii)=outstp(ipenloc)
        outpensave=outpen(ipenloc)
!       Find best stepsize to this point
        do i=1,nsteptot
           if(outpen(i) < outpensave)then
              stp(ii)=outstp(i)
              outpensave=outpen(i)
              istp_use=ii
           end if
        end do
        if(istp_use /= nsteptot) then
           final_ii=ii
           exit stepsize
        end if
!       If no best stepsize set to zero and end minimization
        if(mype == minmype)then
           write(iout_iter,141)(outpen(i),i=1,nsteptot)
        end if
        end_iter = .true.
        stp(ii)=zero_quad
        istp_use=ii
        final_ii=ii
        exit stepsize
     end if
     final_ii=ii
  end do stepsize

  if(kprt >= 2 .and. iter == 0)then
     call mpl_allreduce(ipen,nobs_bins,pj)
     if(mype == minmype)call prnt_j(pj,n0,ipen,kprt)
  end if

  stpinout=stp(istp_use)

! Check for final stepsize negative (probable error)
  if(stpinout <= zero)then
     if(mype == minmype)then
        do i=1,ipen
            pen_est(i)=pbc(1,i)-(stpinout-stp(0))*(2.0_r_quad*bsum(i)- &
                       (stpinout-stp(0))*csum(i))
        end do
        write(iout_iter,130) final_ii,bx,cx,stp(final_ii)
        write(iout_iter,105) (bsum(i),i=1,ipen)
        write(iout_iter,110) (csum(i),i=1,ipen)
        write(iout_iter,101) (pbc(1,i)-pen_est(i),i=1,ipen)
     end if
     end_iter = .true.
  end if
199 format(' stepsize stprat    = ',6(e25.18,1x))
200 format(' stepsize estimates = ',6(e25.18,1x))
201 format(' stepsize guesses   = ',(10(e13.6,1x)))
202 format(' penalties          = ',(10(e13.6,1x)))

! If convergence or failure of stepsize calculation return

!    Estimate terms in penalty
  if(mype == minmype)then
     pjcostnew(1) = pbc(1,1)                                  ! Jb
     pjcostnew(3) = pbc(1,2)+pbc(1,3)                         ! Jc
     pjcostnew(4)=zero
     do i=4,n0
        pjcostnew(4) =  pjcostnew(4) + pbc(1,i) ! Jl
     end do
     pjcostnew(2) = zero 
     do i=1,nobs_type
        pjcostnew(2) = pjcostnew(2)+pbc(1,n0+i)               ! Jo
     end do
     penaltynew=pjcostnew(1)+pjcostnew(2)+pjcostnew(3)+pjcostnew(4)

     if(print_verbose)then
        write(iout_iter,200) (stp(i),i=0,istp_use)
        write(iout_iter,199) (stprat(i),i=1,istp_use)
        write(iout_iter,201) (outstp(i),i=1,nsteptot)
        write(iout_iter,202) (outpen(i)-outpen(4),i=1,nsteptot)
     end if
  end if

  if (.not. end_iter) then
! Update solution
!$omp parallel do schedule(dynamic,1) private(i,ii)
     do ii=1,nobs_bins+2
        if(ii <= nobs_bins)then
           do i=1,sval(ii)%ndim
              sval(ii)%values(i)=sval(ii)%values(i)+stpinout*dval(ii)%values(i)
           end do
        else if(ii == nobs_bins+1)then
           do i=1,nrclen
              sbias%values(i)=sbias%values(i)+stpinout*dbias%values(i)
           end do
        else
           do i=1,nclen
              xhatsave%values(i)=xhatsave%values(i)+stpinout*dirx%values(i)
              yhatsave%values(i)=yhatsave%values(i)+stpinout*diry%values(i)
           end do
        end if
     end do
  endif


! Finalize timer
  call timer_fnl('stpcalc')

  return
end subroutine stpcalc

subroutine prnt_j(pj,n0,ipen,kprt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prnt_j
!   prgmmr: derber
!
! abstract: prints J components
!
! program history log:
!   2015=03-06  derber
!
!   input argument list:
!   pj   - array containing contributions to penalty
!   ipen - number of penalty terms
!   kprt - print type flag
!
!   output argument list:
!
! attributes:
!   language: f90
  use kinds, only: r_kind,i_kind,r_quad
  use gsi_4dvar, only: nobs_bins
  use constants, only: zero_quad
  use jfunc, only: jiter,iter
  use mpimod, only: mype
  use gsi_obOperTypeManager, only: nobs_type => obOper_count
  use gsi_obOperTypeManager, only: obOper_typeInfo
  use gridmod, only: minmype
  real(r_quad),dimension(ipen,nobs_bins),intent(in   ) :: pj
  integer(i_kind)                       ,intent(in   ) :: n0,ipen,kprt

        ! pj(   1:n0  ): leading section for contributions from linear and nonlinear terms
        ! pj(n0+1:ipen): remaining section for contributations from observation terms

  real(r_quad),dimension(ipen) :: zjt
  real(r_quad)                 :: zj
  integer(i_kind)              :: ii,jj
  character(len=20) :: ctype(ipen)

  if(kprt <=0 .or. mype /=minmype)return
  ctype(:)=".unknown."
  ctype(1)='background          '
  ctype(2)='                    '
  ctype(3)='dry mass constraint '
  ctype(4)='negative moisture   '
  ctype(5)='excess   moisture   '
  ctype(6)='negative gust       '
  ctype(7)='negative visability '
  ctype(8)='negative boundary Lr'
  ctype(9)='negative 10m wind ssp'
  ctype(10)='negative howv       '
  ctype(11)='negative lcbas      '
  ctype(12)='negative cldch      '
  ctype(13)='negative ql         '
  ctype(14)='negative qi         '
  ctype(15)='negative qr         '
  ctype(16)='negative qs         '
  ctype(17)='negative qg         '
  do ii=1,nobs_type
    ctype(n0+ii)=obOper_typeInfo(ii)
  end do

  zjt=zero_quad
  do ii=1,nobs_bins
     zjt(:)=zjt(:)+pj(:,ii)
  end do

  zj=zero_quad
  do ii=1,ipen
     zj=zj+zjt(ii)
  end do

! Prints
  if (kprt>=2) write(6,*)'Begin J table inner/outer loop',iter,jiter

   if (kprt>=3.and.nobs_bins>1) then
      write(6,410)'J contribution  ',(jj,jj=1,nobs_bins)
      do ii=1,ipen
         if (zjt(ii)>zero_quad) then
            write(6,100)ctype(ii),(real(pj(ii,jj),r_kind),jj=1,nobs_bins)
         endif
      enddo
   endif
   write(6,400)' J term         ',' ',' J  '
   do ii=1,ipen
      if (zjt(ii)>zero_quad) then
         write(6,200)ctype(ii),real(zjt(ii),r_kind)
      endif
   enddo

   write(6,*)'----------------------------------------------------- '
   write(6,200)"J Global           ",real(zj,r_kind)

   write(6,*)'End Jo table inner/outer loop',iter,jiter

100 format(a20,2x,10es14.6)
410 format(a20,2x,10I14)
200 format(a20,2x,3x,2x,es24.16)
400 format(a20,2x,a3,2x,a24)
   end subroutine prnt_j

end module stpcalcmod
