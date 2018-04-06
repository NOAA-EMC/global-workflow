!> \file gscond.f
!! This file contains the subroutine that calculates grid-scale 
!! condensation and evaporation for use in the Zhao and Carr (1997) 
!! \cite zhao_and_carr_1997 scheme.

!> \defgroup MPscheme Grid-scale Condensation, Evaporation and Precipitation
!! @{
!! \brief The GFS scheme for large-scale condensation and precipitation
!! , based on Zhao and Carr (1997) \cite zhao_and_carr_1997
!! and Sundqvist et al. (1989) \cite sundqvist_et_al_1989 .
!! \image  html  schematic_MPS.png "Figure 1: Schematic illustration of the precipitation scheme" width=10cm
!! \details Figure 1 shows a  schematic illustration of this scheme.
!! There are two sources of prognostic cloud condensate, convective 
!! detrainment (see convection) and grid-sale
!! condensate. The sinks of cloud condensate are grid-scale 
!! precipitation and evaporation of the cloud condensate. Evaporation 
!! of rain in the unsaturated layers below the level of condensation 
!! is also taken into account. All precipitation that penetrates the 
!! lowest atmospheric layer is allowed to fall to the surface. 
!! Subsequent to the May 2001 implementation, excessive amounts of 
!! light precipitation were noted. This was addressed through a minor 
!! implementation in August 2001, which involved a slight modification 
!! of the autoconversion rate of ice. At the same time, an 
!! empirically-based calculation of the effective radius for ice 
!! crystals (Heymsfield and McFarquhar 1996 
!! \cite heymsfield_and_mcfarquhar_1996) was introduced.
!> \section tune Important Tunable Parameters
!! The parameters below, which can be set through a namelist, influence
!! the amount of cloud condensate in the atmosphere and thus the cloud 
!! radiative properties:
!! - PSAUTCO, PRAUTCO: Auto conversion coefficients (ice and water)
!! - WMINCO(2): Coefficients for minimum value of cloud condensate to 
!! conversion from condensate (water and ice)  to precipitation
!! - EVPCO: Coefficient for evaporation of precipitation
!!
!! \section intramps Intraphysics Communication
!! - Routine GSCOND is called from GBPHYS after call to SHALCNV
!! - Routine PRECPD is called from GBPHYS after call to GSCOND

!> \defgroup condense Grid-Scale Condensation and Evaporation of Cloud
!! This subroutine computes grid-scale condensation and evaporation of 
!! cloud condensate.
!!
!> There are two sources of condensation, one from large-scale 
!! processes and the other from convective processes. Both of them 
!! produce either cloud water or cloud ice, depending on the cloud 
!! substance at and above the grid point at current and previous time
!! steps, and on the temperature. Evaporation of cloud is allowed at 
!! points where the relative humidity is lower than the critical value
!! required for condensation. 
!! @{

!> \param[in] ix         horizontal dimension
!! \param[in] im         horizontal number of used pts
!! \param[in] km         vertical layer dimension
!! \param[in] dt         physics time step in seconds
!! \param[in] dtf        dynamics time step in seconds
!! \param[in] prsl       pressure values for model layers
!! \param[in] ps         surface pressure (Pa)
!! \param[in,out] q      model layer specific humidity (gm/gm)
!! \param[in,out] cwm    model layer cloud condensate 
!! \param[in,out] t      model layer mean temperature (K)
!! \param[in,out] tp     model layer mean temperature (K) saved for 
!!                       restart
!! \param[in,out] qp     model layer specific humidity (gm/gm) saved 
!!                       for restart
!! \param[in,out] psp    surface pressure (Pa) saved for restart
!! \param[in,out] tp1    updated model layer mean temperature (K) saved
!!                       for restart
!! \param[in,out] qp1    updated model layer specific humidity (gm/gm)
!!                       saved for restart
!! \param[in,out] psp1   updated surface pressure (Pa) saved for 
!!                       restart
!! \param[in] u          the critical value of relative humidity for 
!!                       large-scale condensation
!! \param[in] lprnt      logical print flag
!! \param[in] ipr        check print point for debugging
!! 
!! \section def Definition of symbols
!! - \f$C_{g}\f$: grid-scale condensation rate (\f$s^{-1}\f$)
!! - \f$E_{c}\f$: evaporation rate of cloud (\f$s^{-1}\f$)
!> \section gen_algorithm General Algorithm
!> @{
      subroutine gscond (im,ix,km,dt,dtf,prsl,ps,q,cwm,t                &
     &,                  tp, qp, psp, tp1, qp1, psp1, u, lprnt, ipr)
!
!     ******************************************************************
!     *                                                                *
!     *  subroutine for grid-scale condensation & evaporation          *
!     *  for the mrf model at ncep.                                    *
!     *                                                                *
!     ******************************************************************
!     *                                                                *
!     *  created by:   q.  zhao         jan. 1995                      *
!     *  modified by:  h.-l. pan        sep. 1998                      *
!     *  modified by:  s. moorthi       aug. 1998, 1999, 2000          *
!     *                                                                *
!     *  references:                                                   *
!     *                                                                *
!     ******************************************************************
!
      use machine , only : kind_phys
      use funcphys , only : fpvs
      use physcons, psat => con_psat, hvap => con_hvap, grav => con_g
     &,             hfus => con_hfus, ttp => con_ttp, rd => con_rd
     &,             cp => con_cp, eps => con_eps, epsm1 => con_epsm1
     &,             rv => con_rv
!      use namelist_def, only: nsdfi,fhdfi
      implicit none
!
      real (kind=kind_phys) h1
     &,                     d00,  elwv, eliv
     &,                     epsq
     &,                     r,     cpr,  rcp
      parameter (h1=1.e0,       d00=0.e0
     &,          elwv=hvap,     eliv=hvap+hfus
     &,          epsq=2.e-12,   r=rd
     &,          cpr=cp*r,      rcp=h1/cp)
!
      real(kind=kind_phys), parameter :: cons_0=0.0, cons_m15=-15.0
!
      integer im, ix, km, ipr
      real (kind=kind_phys) q(ix,km),    t(ix,km),    cwm(ix,km)        &
     &,                     prsl(ix,km), ps(im), dt,  dtf               &
     &,                     tp(ix,km),   qp(ix,km),   psp(im)           &
     &,                     tp1(ix,km),  qp1(ix,km),  psp1(im)
!
      real (kind=kind_phys)  qi(im), qint(im), u(im,km), ccrik, e0
     &,                      cond,   rdt, us, cclimit, climit
     &,                      tmt0, tmt15, qik, cwmik
     &,                      ai, qw, u00ik, tik, pres, pp0, fi 
     &,                      at, aq, ap, fiw, elv, qc, rqik
     &,                      rqikk, tx1, tx2, tx3, es, qs
     &,                      tsq, delq, condi, cone0, us00, ccrik1
     &,                      aa, ab, ac, ad, ae, af, ag
     &,                      el2orc, albycp
!     real (kind=kind_phys) vprs(im)
      integer iw(im,km), i, k, iwik
      logical lprnt
!
!-----------------prepare constants for later uses-----------------
!
      el2orc = hvap*hvap / (rv*cp)
      albycp = hvap / cp
!     write(0,*)' in gscond im=',im,' ix=',ix
!
      rdt     = h1/dt
      us      = h1
      cclimit = 1.0e-3
      climit  = 1.0e-20
!
      do  i = 1, im
        iw(i,km) = d00
      enddo
!
!  check for first time step
!
!      if (tp(1,1) < 1.) then
!        do k = 1, km
!          do i = 1, im
!            tp(i,k) = t(i,k)
!            qp(i,k) = max(q(i,k),epsq)
!            tp1(i,k) = t(i,k)
!            qp1(i,k) = max(q(i,k),epsq)
!          enddo
!        enddo
!        do i = 1, im
!          psp(i)  = ps(i)
!          psp1(i) = ps(i)
!        enddo
!      endif
!
!*************************************************************
!> -# Begining of  grid-scale condensation/evaporation loop (start of 
!! k-loop, i-loop)
!*************************************************************
!
!     do k = km-1,2,-1
      do k = km,1,-1
!       vprs(:) = 0.001 * fpvs(t(:,k))       ! fpvs in pa
!-----------------------------------------------------------------------
!------------------qw, qi and qint--------------------------------------
        do i = 1, im                                    
          tmt0  = t(i,k)-273.16
          tmt15 = min(tmt0,cons_m15)
          qik   = max(q(i,k),epsq)
          cwmik = max(cwm(i,k),climit)
!
!         ai    = 0.008855
!         bi    = 1.0
!         if (tmt0 .lt. -20.0) then
!           ai = 0.007225
!           bi = 0.9674
!         end if
!
!  the global qsat computation is done in pa
          pres    = prsl(i,k)
!
!         qw      = vprs(i)
          qw      = min(pres, fpvs(t(i,k)))
!
          qw      = eps * qw / (pres + epsm1 * qw)
          qw      = max(qw,epsq)
!         qi(i)   = qw *(bi+ai*min(tmt0,cons_0))
!         qint(i) = qw *(1.-0.00032*tmt15*(tmt15+15.))
          qi(i)   = qw
          qint(i) = qw
!         if (tmt0 .le. -40.) qint(i) = qi(i)

!> -# Compute ice-water identification number IW.
!!\n  The distinction between cloud water and cloud ice is made by the
!! cloud identification number IW, which is zero for cloud water and 
!! unity for cloud ice (Table 2 in zhao and Carr (1997) 
!! \cite zhao_and_carr_1997):
!!  - All clouds are defined to consist of liquid water below the 
!! freezing level (\f$T\geq 0^oC\f$) and of ice particles above the 
!! \f$T=-15^oC\f$ level.
!!  - In the temperature region between \f$-15^oC\f$ and \f$0^oC\f$, 
!! clouds may be composed of liquid water or ice. If there are cloud 
!! ice particles above this point at the previous or current time step,
!! or if the cloud at this point at the previous time step consists of 
!! ice particles, then the cloud substance at this point is considered 
!! to be ice particles because of the cloud seeding effect and the
!! memory of its content. Otherwise, all clouds in this region are 
!! considered to contain supercooled cloud water.

!-------------------ice-water id number iw------------------------------
          if(tmt0.lt.-15.0) then
            u00ik = u(i,k)
            fi    = qik - u00ik*qi(i)    
            if(fi > d00.or.cwmik > climit) then                    
               iw(i,k) = 1
            else
              iw(i,k) = 0
            end if
          end if
!
          if(tmt0.ge.0.0) then
            iw(i,k) = 0
          end if
!
          if (tmt0 < 0.0 .and. tmt0 >= -15.0) then
            iw(i,k) = 0
            if (k < km) then
            if (iw(i,k+1)  == 1 .and. cwmik > climit) iw(i,k) = 1
            endif
          end if
        enddo
!> -# Condensation and evaporation of cloud
!--------------condensation and evaporation of cloud--------------------
        do i = 1, im
!>  - Compute the changes in t, q and p (\f$A_{t}\f$,\f$A_{q}\f$ and 
!! \f$A_{p}\f$) caused by all the processes except grid-scale 
!! condensation and evaporation.
!!\f[
!!   A_{t}=(t-tp)/dt
!!\f]
!!\f[
!!   A_{q}=(q-qp)/dt
!!\f]
!!\f[
!!   A_{p}=(prsl-\frac{prsl}{ps} \times psp)/dt
!!\f]
!------------------------at, aq and dp/dt-------------------------------
          qik   = max(q(i,k),epsq)
          cwmik = max(cwm(i,k),climit)
          iwik  = iw(i,k)
          u00ik = u(i,k)
          tik   = t(i,k)
          pres  = prsl(i,k)
          pp0   = (pres / ps(i)) * psp(i)
          at    = (tik-tp(i,k)) * rdt
          aq    = (qik-qp(i,k)) * rdt
          ap    = (pres-pp0)    * rdt
!>  - Calculate the saturation specific humidity \f$q_{s}\f$ and the 
!! relative humidity \f$f\f$ using IW.
!----------------the satuation specific humidity------------------------
          fiw   = float(iwik)
          elv   = (h1-fiw)*elwv    + fiw*eliv
          qc    = (h1-fiw)*qint(i) + fiw*qi(i)
!     if (lprnt) print *,' qc=',qc,' qint=',qint(i),' qi=',qi(i)
!----------------the relative humidity----------------------------------
          if(qc.le.1.0e-10) then
            rqik=d00 
          else
            rqik = qik/qc
          endif

!>  - According to Sundqvist et al. (1989) \cite sundqvist_et_al_1989, 
!! estimate cloud fraction \f$b\f$ at a grid point from relative 
!! humidity \f$f\f$ using the equation 
!!\f[
!!       b=1-\left ( \frac{f_{s}-f}{f_{s}-u} \right )^{1/2}
!!\f]
!! for \f$f>u\f$; and \f$b=0\f$ for \f$f<u\f$. where \f$f_{s}=1.0\f$ is
!! the relative humidity in a cloud region and \f$u\f$ ,which is an 
!! input parameter accounts for the effects of subgrid-scale variations
!! in moisture on large-scale condensation. Since both temperature and 
!! moisture may vary at scales smaller than the model grid scale, it is
!! possible for condensation to occur before the grid-average relative
!! humidity reaches 100%. Therefore \f$u\f$ needs to be less than 1.0 
!! to account for the subgrid-scale variation of temperature and 
!! moisture fields and allow subgrid-scale condensation.
!!  - If cloud fraction \f$b\leq 1.0\times10^{-3}\f$, then evaporate 
!! any existing cloud condensate using evaporation rate \f$E_{c}\f$ as
!! computed below.
!!\n If \f$q_{0}\f$ represents the specific humidity at relative 
!! humidity \f$u\f$, then
!!\f[
!!           q_{0}=uq_{s}
!!\f]
!!\n if the cloud water/ice at this point is enough to be evaporated 
!! until \f$u\f$ is reached, then the evaporation rate \f$E_{c}\f$, 
!! assuming that the evaporation process occurs in one time step, is
!! determined by
!!\f[
!!           E_{c}=\frac{q_{0}-q}{dt}
!!\f]
!!\n  Using \f$q_{0}=uq_{s}\f$ and the equation \f$q=fq_{s}\f$, 
!! \f$E_{c}\f$ then becomes
!!\f[
!!  E_{c}=\frac{q_{s}}{dt}(u-f)
!!\f]
!! where \f$dt\f$ is the time step for precipitation calculation in the
!! model. It is a simplified version of a higher-order cloud 
!! evaporation algorithm (Rutledge and Hobbs 1983 
!! \cite rutledge_and_hobbs_1983). In the case where all clouds will
!! evaporate before \f$u\f$ is reached, the following equation is used:
!! \f[
!!  E_{c}=\frac{cwm}{dt}
!! \f]
!!  - If cloud fraction \f$b>1.0\times10^{-3}\f$, condense water vapor
!! in to cloud condensate (\f$C_{g}\f$).
!!\n Using \f$q=fq_{s}\f$, \f$q_{s}=\epsilon e_{s}/p\f$, and the 
!! Clausius-Clapeyron equation \f$de_{s}/dT=\epsilon Le_{s}/RT^{2}\f$, 
!! where \f$q_{s}\f$ is the saturation specific humidity,\f$e_{s}\f$
!! is the saturation vapor pressure, \f$R\f$ is the specific gas
!! constant for dry air, \f$f\f$ is the relative humidity, and 
!! \f$\epsilon=0.622\f$, the expression for \f$C_{g}\f$ has the form
!!\f[
!!  C_{g}=\frac{M-q_{s}f_{t}}{1+(f\epsilon L^{2}q_{s}/RC_{p}T^{2})}+E_{c}
!!\f]
!! where
!!\f[
!!   M=A_{q}-\frac{f\epsilon Lq_{s}}{RT^{2}}A_{t}+\frac{fq_{s}}{p}A_{p}
!!\f]
!! To close the system, an equation for the relative humidity tendency
!! \f$f_{t}\f$ was derived by Sundqvist et al. (1989) 
!! \cite sundqvist_et_al_1989 using the hypothesis that the quantity
!! \f$M+E_{c}\f$ is divided into one part,\f$bM\f$,which condenses
!! in the already cloudy portion of a grid square, and another part, 
!! \f$(1-b)M+E_{c}\f$,which is used to increase the relative humidity
!! of the cloud-free portion and the cloudiness in the square. The
!! equation is written as
!!\f[
!!  f_{t}=\frac{2(1-b)(f_{s}-u)[(1-b)M+E_{c}]}{2q_{s}(1-b)(f_{s}-u)+cwm/b}
!!\f]
!!  - Check and correct if over condensation occurs.
!!  - Update  t, q and cwm (according to Eqs(6) and (7) in Zhao and 
!! Carr (1997) \cite zhao_and_carr_1997)
!!\f[
!!   cwm=cwm+(C_{g}-E_{c})\times dt
!!\f]
!!\f[
!!   q=q-(C_{g}-E_{c})\times dt
!!\f]
!!\f[
!!   t=t+\frac{L}{C_{p}}(C_{g}-E_{c})\times dt
!!\f]
!!\n where \f$L\f$ is the latent heat of condensation/deposition, and 
!! \f$C_{p}\f$ is the specific heat of air at constant pressure.

!----------------cloud cover ratio ccrik--------------------------------
          if (rqik .lt. u00ik) then
             ccrik = d00
          elseif(rqik.ge.us) then
             ccrik = us
          else
             rqikk  = min(us,rqik)
             ccrik = h1-sqrt((us-rqikk)/(us-u00ik))
          endif
!-----------correct ccr if it is too small in large cwm regions--------
!         if(ccrik.ge.0.01.and.ccrik.le.0.2.and
!    &          .cwmik.ge.0.2e-3) then
!          ccrik=min(1.0,cwmik*1.0e3)
!         end if
!----------------------------------------------------------------------
!   if no cloud exists then evaporate any existing cloud condensate
!----------------evaporation of cloud water-----------------------------
          e0 = d00
          if (ccrik <= cclimit.and. cwmik > climit)  then 
!
!   first iteration - increment halved
!
            tx1 = tik
            tx3 = qik
!
            es   = min(pres, fpvs(tx1))
            qs   = u00ik * eps * es / (pres + epsm1*es)
            tsq  = tx1 * tx1
            delq = 0.5 * (qs - tx3) * tsq / (tsq + el2orc * qs)
!
            tx2   = delq
            tx1   = tx1 - delq * albycp
            tx3   = tx3 + delq
!
!   second iteration
!
            es   = min(pres, fpvs(tx1))
            qs   = u00ik * eps * es / (pres + epsm1*es)
            tsq  = tx1 * tx1
            delq = (qs - tx3) * tsq / (tsq + el2orc * qs)
!
            tx2  = tx2 + delq
            tx1  = tx1 - delq * albycp
            tx3  = tx3 + delq
!
!   third iteration
!
            es   = min(pres, fpvs(tx1))
            qs   = u00ik * eps * es / (pres + epsm1*es)
            tsq  = tx1 * tx1
            delq = (qs - tx3) * tsq / (tsq + el2orc * qs)
            tx2  = tx2 + delq
!
            e0   = max(tx2*rdt, cons_0)
!     if (lprnt .and. i .eq. ipr .and. k .eq. 34)
!    & print *,' tx2=',tx2,' qc=',qc,' u00ik=',u00ik,' rqik=',rqik
!    &,' cwmik=',cwmik,' e0',e0

!           e0 = max(qc*(u00ik-rqik)*rdt, cons_0)
            e0 = min(cwmik*rdt,   e0)
            e0 = max(cons_0,e0)
          end if
!   if cloud cover > 0.2 condense water vapor in to cloud condensate
!-----------the eqs. for cond. has been reorganized to reduce cpu------
          cond = d00
!         if (ccrik .gt. 0.20 .and. qc .gt. epsq) then
          if (ccrik .gt. cclimit .and. qc .gt. epsq) then
             us00   = us  - u00ik 
             ccrik1 = 1.0 - ccrik
             aa     = eps*elv*pres*qik
             ab     = ccrik*ccrik1*qc*us00
             ac     = ab + 0.5*cwmik
             ad     = ab * ccrik1
             ae     = cpr*tik*tik
             af     = ae * pres
             ag     = aa * elv
             ai     = cp * aa
             cond   = (ac-ad)*(af*aq-ai*at+ae*qik*ap)/(ac*(af+ag))
!-----------check & correct if over condensation occurs-----------------
             condi  = (qik   -u00ik   *qc*1.0)*rdt
             cond   = min(cond, condi)
!----------check & correct if supersatuation is too high----------------
!             qtemp=qik-max(0.,(cond-e0))*dt
!             if(qc.le.1.0e-10) then
!               rqtmp=0.0
!             else
!               rqtmp=qtemp/qc
!             end if
!             if(rqtmp.ge.1.10) then
!               cond=(qik-1.10*qc)*rdt
!             end if
!-----------------------------------------------------------------------
             cond = max(cond, d00)
!-------------------update of t, q and cwm------------------------------
          end if
          cone0    = (cond-e0) * dt
          cwm(i,k) = cwm(i,k) + cone0
!     if (lprnt .and. i .eq. ipr) print *,' t=',t(i,k),' cone0',cone0
!    &,' cond=',cond,' e0=',e0,' elv=',elv,' rcp=',rcp,' k=',k
!    &,' cwm=',cwm(i,k)
          t(i,k)   = t(i,k)   + elv*rcp*cone0
          q(i,k)   = q(i,k)   - cone0
        enddo                                  ! end of i-loop!
      enddo                                    ! end of k-loop!
!
!*********************************************************************
!> -# End of the condensation/evaporation loop (end of i-loop,k-loop)
!*********************************************************************
!

!> -# Store \f$t\f$, \f$q\f$, \f$ps\f$ for next time step.

      if (dt > dtf+0.001) then     ! three time level
        do k = 1, km
          do i = 1, im
            tp(i,k)  = tp1(i,k)
            qp(i,k)  = qp1(i,k)
!
            tp1(i,k) = t(i,k)
            qp1(i,k) = max(q(i,k),epsq)
          enddo
        enddo
        do i = 1, im
          psp(i)  = psp1(i)
          psp1(i) = ps(i)
        enddo
      else                   ! two time level scheme - tp1, qp1, psp1 not used
        do k = 1, km
!     write(0,*)' in gscond k=',k,' im=',im,' km=',km
          do i = 1, im
!     write(0,*)' in gscond i=',i
            tp(i,k)  = t(i,k)
            qp(i,k)  = max(q(i,k),epsq)
!           qp(i,k)  = q(i,k)
            tp1(i,k) = tp(i,k)
            qp1(i,k) = qp(i,k)
          enddo
        enddo
        do i = 1, im
          psp(i)  = ps(i)
          psp1(i) = ps(i)
        enddo
      endif
!-----------------------------------------------------------------------
      return
      end
!> @}
!! @}
!! @}
