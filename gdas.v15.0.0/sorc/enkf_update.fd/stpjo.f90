subroutine stpjo(yobs,dval,dbias,xval,xbias,sges,pbcjo,nstep,nobs_bins)

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
!   2007-02-15  rancic  - add foto
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
  use obsmod, only: obs_handle, &
                  & i_ps_ob_type, i_t_ob_type, i_w_ob_type, i_q_ob_type, &
                  & i_spd_ob_type, i_srw_ob_type, i_rw_ob_type, i_dw_ob_type, &
                  & i_sst_ob_type, i_pw_ob_type, i_oz_ob_type, i_colvk_ob_type, &
                  & i_gps_ob_type, i_rad_ob_type, i_pcp_ob_type,i_tcp_ob_type, &
                  & i_pm2_5_ob_type, i_gust_ob_type, i_vis_ob_type, i_pblh_ob_type, &
                  & i_pm10_ob_type, &
                  & i_wspd10m_ob_type,i_td2m_ob_type,i_mxtm_ob_type,i_mitm_ob_type, &
                    i_pmsl_ob_type,i_howv_ob_type,i_tcamt_ob_type,i_lcbas_ob_type,  &
                    i_aero_ob_type,i_cldch_ob_type,nobs_type,stpcnt,ll_jo,ib_jo
  use stptmod, only: stpt
  use stpwmod, only: stpw
  use stppsmod, only: stpps
  use stppwmod, only: stppw
  use stpqmod, only: stpq
  use stpradmod, only: stprad
  use stpgpsmod, only: stpgps
  use stprwmod, only: stprw
  use stpspdmod, only: stpspd
  use stpsrwmod, only: stpsrw
  use stpsstmod, only: stpsst
  use stptcpmod, only: stptcp
  use stpdwmod, only: stpdw
  use stppcpmod, only: stppcp
  use stpozmod, only: stpoz
  use stpcomod, only: stpco
  use stppm2_5mod, only: stppm2_5
  use stppm10mod, only: stppm10
  use stpaodmod, only: stpaod
  use stpgustmod, only: stpgust
  use stpvismod, only: stpvis
  use stppblhmod, only: stppblh
  use stpwspd10mmod, only: stpwspd10m
  use stptd2mmod, only: stptd2m
  use stpmxtmmod, only: stpmxtm
  use stpmitmmod, only: stpmitm
  use stppmslmod, only: stppmsl
  use stphowvmod, only: stphowv
  use stptcamtmod, only: stptcamt
  use stplcbasmod, only: stplcbas
  use stpcldchmod, only: stpcldch
  use bias_predictors, only: predictors
  use aircraftinfo, only: aircraft_t_bc_pof,aircraft_t_bc
  use gsi_bundlemod, only: gsi_bundle
  use control_vectors, only: cvars2d
  use mpeu_util, only: getindex
  implicit none

! Declare passed variables
  type(obs_handle),dimension(nobs_bins),intent(in   ) :: yobs
  type(gsi_bundle),dimension(nobs_bins),intent(in   ) :: dval
  type(predictors)                     ,intent(in   ) :: dbias
  type(gsi_bundle),dimension(nobs_bins),intent(in   ) :: xval
  type(predictors)                     ,intent(in   ) :: xbias
  integer(i_kind)                      ,intent(in   ) :: nstep,nobs_bins
  real(r_kind),dimension(max(1,nstep)) ,intent(in   ) :: sges
  real(r_quad),dimension(4,nobs_type,nobs_bins)  ,intent(inout) :: pbcjo

! Declare local variables

  integer(i_kind) :: ll,mm,ib
!************************************************************************************  

!$omp parallel do  schedule(dynamic,1) private(ll,mm,ib)
    do mm=1,stpcnt
       ll=ll_jo(mm)
       ib=ib_jo(mm)
!   penalty, b, and c for radiances
       if(ll == 1)then
          call stprad(yobs(ib)%rad,dval(ib),xval(ib),dbias%predr,xbias%predr,&
                pbcjo(1,i_rad_ob_type,ib),sges,nstep)

!   penalty, b, and c for temperature
       else if(ll == 2) then
          if (.not. (aircraft_t_bc_pof .or. aircraft_t_bc)) then
             call stpt(yobs(ib)%t,dval(ib),xval(ib),pbcjo(1,i_t_ob_type,ib),sges,nstep) 
          else
             call stpt(yobs(ib)%t,dval(ib),xval(ib),pbcjo(1,i_t_ob_type,ib),sges,nstep, &
                 dbias%predt,xbias%predt) 
          end if

!   penalty, b, and c for winds
       else if(ll == 3) then
          call stpw(yobs(ib)%w,dval(ib),xval(ib),pbcjo(1,i_w_ob_type,ib),sges,nstep)

!   penalty, b, and c for precipitable water
       else if(ll == 4) then
          call stppw(yobs(ib)%pw,dval(ib),xval(ib),pbcjo(1,i_pw_ob_type,ib),sges,nstep)

!   penalty, b, and c for ozone
       else if(ll == 5) then
          call stpco(yobs(ib)%colvk,dval(ib),xval(ib),pbcjo(1,i_colvk_ob_type,ib),sges,nstep)

!   penalty, b, and c for ozone
       else if(ll == 6) then
          call stppm2_5(yobs(ib)%pm2_5,dval(ib),xval(ib),pbcjo(1,i_pm2_5_ob_type,ib),sges,nstep)

!   penalty, b, and c for wind lidar
       else if(ll == 7) then
          call stpdw(yobs(ib)%dw,dval(ib),xval(ib),pbcjo(1,i_dw_ob_type,ib),sges,nstep) 

!   penalty, b, and c for radar
       else if(ll == 8) then
          call stprw(yobs(ib)%rw,dval(ib),xval(ib),pbcjo(1,i_rw_ob_type,ib),sges,nstep) 

!   penalty, b, and c for moisture
       else if(ll == 9) then
          call stpq(yobs(ib)%q,dval(ib),xval(ib),pbcjo(1,i_q_ob_type,ib),sges,nstep)

!   penalty, b, and c for ozone
       else if(ll == 10) then
          call stpoz(yobs(ib)%oz,yobs(ib)%o3l,dval(ib),xval(ib),pbcjo(1,i_oz_ob_type,ib),sges,nstep)

!   penalty, b, and c for radar superob wind
       else if(ll == 11) then
          call stpsrw(yobs(ib)%srw,dval(ib),xval(ib),pbcjo(1,i_srw_ob_type,ib),sges,nstep)

!   penalty, b, and c for GPS local observation
       else if(ll == 12) then
          call stpgps(yobs(ib)%gps,dval(ib),xval(ib),pbcjo(1,i_gps_ob_type,ib),sges,nstep) 

!   penalty, b, and c for conventional sst
       else if(ll == 13) then
          call stpsst(yobs(ib)%sst,dval(ib),xval(ib),pbcjo(1,i_sst_ob_type,ib),sges,nstep)

!   penalty, b, and c for wind speed
       else if(ll == 14) then
          call stpspd(yobs(ib)%spd,dval(ib),xval(ib),pbcjo(1,i_spd_ob_type,ib),sges,nstep) 

!   penalty, b, and c for precipitation
       else if(ll == 15) then
          call stppcp(yobs(ib)%pcp,dval(ib),xval(ib),pbcjo(1,i_pcp_ob_type,ib),sges,nstep)

!   penalty, b, and c for surface pressure
       else if(ll == 16) then
          call stpps(yobs(ib)%ps,dval(ib),xval(ib),pbcjo(1,i_ps_ob_type,ib),sges,nstep)

!   penalty, b, and c for MSLP TC obs
       else if(ll == 17) then
          call stptcp(yobs(ib)%tcp,dval(ib),xval(ib),pbcjo(1,i_tcp_ob_type,ib),sges,nstep)

!   penalty, b, and c for conventional gust
       else if(ll == 18) then
          if (getindex(cvars2d,'gust')>0) &
          call stpgust(yobs(ib)%gust,dval(ib),xval(ib),pbcjo(1,i_gust_ob_type,ib),sges,nstep)

!   penalty, b, and c for conventional vis
       else if(ll == 19) then
          if (getindex(cvars2d,'vis')>0) &
          call stpvis(yobs(ib)%vis,dval(ib),xval(ib),pbcjo(1,i_vis_ob_type,ib),sges,nstep)

!   penalty, b, and c for conventional pblh
       else if(ll == 20) then
          if (getindex(cvars2d,'pblh')>0) &
          call stppblh(yobs(ib)%pblh,dval(ib),xval(ib),pbcjo(1,i_pblh_ob_type,ib),sges,nstep)

!   penalty, b, and c for conventional wspd10m
       else if(ll == 21) then
          if (getindex(cvars2d,'wspd10m')>0) &
          call stpwspd10m(yobs(ib)%wspd10m,dval(ib),xval(ib),pbcjo(1,i_wspd10m_ob_type,ib),sges,nstep)

!   penalty, b, and c for conventional td2m
       else if(ll == 22) then
          if (getindex(cvars2d,'td2m')>0) &
          call stptd2m(yobs(ib)%td2m,dval(ib),xval(ib),pbcjo(1,i_td2m_ob_type,ib),sges,nstep)

!   penalty, b, and c for conventional mxtm
       else if(ll == 23) then
          if (getindex(cvars2d,'mxtm')>0) &
          call stpmxtm(yobs(ib)%mxtm,dval(ib),xval(ib),pbcjo(1,i_mxtm_ob_type,ib),sges,nstep)

!   penalty, b, and c for conventional mitm
       else if(ll == 24) then
          if (getindex(cvars2d,'mitm')>0) &
          call stpmitm(yobs(ib)%mitm,dval(ib),xval(ib),pbcjo(1,i_mitm_ob_type,ib),sges,nstep)

!   penalty, b, and c for conventional pmsl
       else if(ll == 25) then
          if (getindex(cvars2d,'pmsl')>0) &
          call stppmsl(yobs(ib)%pmsl,dval(ib),xval(ib),pbcjo(1,i_pmsl_ob_type,ib),sges,nstep)

!   penalty, b, and c for conventional howv
       else if(ll == 26) then
          if (getindex(cvars2d,'howv')>0) &
          call stphowv(yobs(ib)%howv,dval(ib),xval(ib),pbcjo(1,i_howv_ob_type,ib),sges,nstep)

!   penalty, b, and c for total cloud amount
       else if(ll == 27) then
          if (getindex(cvars2d,'tcamt')>0) &
          call stptcamt(yobs(ib)%tcamt,dval(ib),xval(ib),pbcjo(1,i_tcamt_ob_type,ib),sges,nstep)

!   penalty, b, and c for cloud base of lowest cloud
       else if(ll == 28) then
          if (getindex(cvars2d,'lcbas')>0) &
          call stplcbas(yobs(ib)%lcbas,dval(ib),xval(ib),pbcjo(1,i_lcbas_ob_type,ib),sges,nstep)

!   penalty, b, and c for aod
       else if(ll == 29) then
          call stpaod(yobs(ib)%aero,dval(ib),xval(ib),pbcjo(1,i_aero_ob_type,ib),sges,nstep)

       else if(ll == 30) then
          call stppm10(yobs(ib)%pm10,dval(ib),xval(ib),pbcjo(1,i_pm10_ob_type,ib),sges,nstep)

!   penalty, b, and c for conventional cldch
       else if(ll == 31) then
          if (getindex(cvars2d,'cldch')>0) &
          call stpcldch(yobs(ib)%cldch,dval(ib),xval(ib),pbcjo(1,i_cldch_ob_type,ib),sges,nstep)
       end if
    end do

  return
end subroutine stpjo
subroutine stpjo_setup(yobs,nobs_bins)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpjo_setup     setup loops for stpjo
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: setup parallel loops for stpjo
!
! program history log:
!   2015-01-18  derber,j.
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
  use obsmod, only: obs_handle, nobs_type,stpcnt,ll_jo,ib_jo
  use gsi_bundlemod, only: gsi_bundle
  implicit none

! Declare passed variables
  type(obs_handle),dimension(nobs_bins),intent(in   ) :: yobs
  integer(i_kind),intent(in   )  :: nobs_bins

! Declare local variables

  integer(i_kind) ll,ib
!************************************************************************************
    stpcnt = 0
    do ll = 1, nobs_type
     do ib = 1,nobs_bins

       if(ll == 1)then
!         penalty, b, and c for radiances
          if(associated(yobs(ib)%rad)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 2)then
!         penalty, b, and c for temperature
          if(associated(yobs(ib)%t)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 3)then
!         penalty, b, and c for winds
          if(associated(yobs(ib)%w)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 4)then
!         penalty, b, and c for precipitable water
          if(associated(yobs(ib)%pw)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 5)then
!         penalty, b, and c for ozone
          if(associated(yobs(ib)%colvk)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 6)then
!         penalty, b, and c for pm2_5
          if(associated(yobs(ib)%pm2_5)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 7)then
!         penalty, b, and c for wind lidar
          if(associated(yobs(ib)%dw)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 8)then
!         penalty, b, and c for radar
          if(associated(yobs(ib)%rw)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 9)then
!         penalty, b, and c for moisture
          if(associated(yobs(ib)%q)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 10)then
!         penalty, b, and c for ozone
          if(associated(yobs(ib)%oz) .or. associated(yobs(ib)%o3l)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 11)then
!         penalty, b, and c for radar superob wind
          if(associated(yobs(ib)%srw)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 12)then
!         penalty, b, and c for GPS local observation
          if(associated(yobs(ib)%gps)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 13)then
!         penalty, b, and c for conventional sst
          if(associated(yobs(ib)%sst)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 14)then
!         penalty, b, and c for wind speed
          if(associated(yobs(ib)%spd)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 15)then
!         penalty, b, and c for precipitation
          if(associated(yobs(ib)%pcp)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 16)then
!         penalty, b, and c for surface pressure
          if(associated(yobs(ib)%ps)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 17)then
!         penalty, b, and c for MSLP TC obs
          if(associated(yobs(ib)%tcp)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 18)then
!         penalty, b, and c for conventional gust
          if(associated(yobs(ib)%gust)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 19)then
!         penalty, b, and c for conventional vis
          if(associated(yobs(ib)%vis)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 20)then
!         penalty, b, and c for conventional pblh
          if(associated(yobs(ib)%pblh)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if

       else if (ll == 21)then
!         penalty, b, and c for conventional pblh
          if(associated(yobs(ib)%wspd10m)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 22)then
!         penalty, b, and c for conventional pblh
          if(associated(yobs(ib)%td2m)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 23)then
!         penalty, b, and c for conventional pblh
          if(associated(yobs(ib)%mxtm)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 24)then
!         penalty, b, and c for conventional pblh
          if(associated(yobs(ib)%mitm)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 25)then
!         penalty, b, and c for conventional pblh
          if(associated(yobs(ib)%pmsl)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 26)then
!         penalty, b, and c for conventional pblh
          if(associated(yobs(ib)%howv)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 27)then
!         penalty, b, and c for conventional pblh
          if(associated(yobs(ib)%tcamt)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 28)then
!         penalty, b, and c for conventional pblh
          if(associated(yobs(ib)%lcbas)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 29)then
!         penalty, b, and c for aod
          if(associated(yobs(ib)%aero)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 30)then
!         penalty, b, and c for pm10
          if(associated(yobs(ib)%pm10)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       else if (ll == 31)then
!         penalty, b, and c for conventional cldch
          if(associated(yobs(ib)%cldch)) then
             stpcnt = stpcnt +1
             ll_jo(stpcnt) = ll
             ib_jo(stpcnt) = ib
          end if
       end if
     end do
    end do
!   write(6,*) 'stpjo - stpcnt = ',stpcnt,nobs_bins*nobs_type

  return
end subroutine stpjo_setup


