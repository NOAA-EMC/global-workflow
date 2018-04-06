!> \defgroup SAS Simplified Arakawa-Schubert Deep Convection
!! @{
!!  \brief The Simplified Arakawa-Schubert scheme parameterizes the effect of deep convection on the environment (represented by the model state variables) in the following way. First, a simple cloud model is used to determine the change in model state variables due to one entraining/detraining cloud type, per unit cloud-base mass flux. Next, the total change in state variables is retrieved by determining the actual cloud base mass flux using the quasi-equilibrium assumption, whereby convection is assumed to be steady-state. This implies that the generation of the cloud work function (interpreted as entrainment-moderated convective available potential energy (CAPE)) by the large scale dynamics is in balance with the consumption of the cloud work function by the convection.
!!
!!  The SAS scheme uses the working concepts put forth in Arakawa and Schubert (1974) \cite arakawa_and_schubert_1974 but includes modifications and simplifications from Grell (1993) \cite grell_1993 such as saturated downdrafts and only one cloud type (the deepest possible), rather than a spectrum based on cloud top heights or assumed entrainment rates. The scheme was implemented for the GFS in 1995 by Pan and Wu \cite pan_and_wu_1995, with further modifications discussed in Han and Pan (2011) \cite han_and_pan_2011 , including the calculation of cloud top, a greater CFL-criterion-based maximum cloud base mass flux, updated cloud model entrainment and detrainment, improved convective transport of horizontal momentum, a more general triggering function, and the inclusion of convective overshooting.
!!
!!  \section diagram Calling Hierarchy Diagram
!!  \image html SAS_Flowchart.png "Diagram depicting how the SAS deep convection scheme is called from the GSM physics time loop" height=2cm
!!  \section intraphysics Intraphysics Communication
!!  This space is reserved for a description of how this scheme uses information from other scheme types and/or how information calculated in this scheme is used in other scheme types.

!> \file sascnvn.f
!!  Contains the entire SAS deep convection scheme.

!>  \brief This subroutine contains the entirety of the SAS deep convection scheme.
!!
!!  As in Grell (1993) \cite grell_1993 , the SAS convective scheme can be described in terms of three types of "controls": static, dynamic, and feedback. The static control component consists of the simple entraining/detraining updraft/downdraft cloud model and is used to determine the cloud properties, convective precipitation, as well as the convective cloud top height. The dynamic control is the determination of the potential energy available for convection to "consume", or how primed the large-scale environment is for convection to occur due to changes by the dyanmics of the host model. The feedback control is the determination of how the parameterized convection changes the large-scale environment (the host model state variables) given the changes to the state variables per unit cloud base mass flux calculated in the static control portion and the deduced cloud base mass flux determined from the dynamic control.
!!
!!  \param[in] im number of used points
!!  \param[in] ix horizontal dimension
!!  \param[in] km vertical layer dimension
!!  \param[in] jcap number of spectral wave trancation
!!  \param[in] delt physics time step in seconds
!!  \param[in] delp pressure difference between level k and k+1 (Pa)
!!  \param[in] prslp mean layer presure (Pa)
!!  \param[in] psp surface pressure (Pa)
!!  \param[in] phil layer geopotential (\f$m^2/s^2\f$)
!!  \param[inout] ql cloud water or ice (kg/kg)
!!  \param[inout] q1 updated tracers (kg/kg)
!!  \param[inout] t1 updated temperature (K)
!!  \param[inout] u1 updated zonal wind (\f$m s^{-1}\f$)
!!  \param[inout] v1 updated meridional wind (\f$m s^{-1}\f$)
!!  \param[out] cldwrk cloud workfunction (\f$m^2/s^2\f$)
!!  \param[out] rn convective rain (m)
!!  \param[out] kbot index for cloud base
!!  \param[out] ktop index for cloud top
!!  \param[out] kcnv flag to denote deep convection (0=no, 1=yes)
!!  \param[in] islimsk sea/land/ice mask (=0/1/2)
!!  \param[in] dot layer mean vertical velocity (Pa/s)
!!  \param[in] ncloud number of cloud species
!!  \param[out] ud_mf updraft mass flux multiplied by time step (\f$kg/m^2\f$)
!!  \param[out] dd_mf downdraft mass flux multiplied by time step (\f$kg/m^2\f$)
!!  \param[out] dt_mf ud_mf at cloud top (\f$kg/m^2\f$)
!!  \param[out] cnvw convective cloud water (kg/kg)
!!  \param[out] cnvc convective cloud cover (unitless)
!!
!!  \section general General Algorithm
!!  -# Compute preliminary quantities needed for static, dynamic, and feedback control portions of the algorithm.
!!  -# Perform calculations related to the updraft of the entraining/detraining cloud model ("static control").
!!  -# Perform calculations related to the downdraft of the entraining/detraining cloud model ("static control").
!!  -# Using the updated temperature and moisture profiles that were modified by the convection on a short time-scale, recalculate the total cloud work function to determine the change in the cloud work function due to convection, or the stabilizing effect of the cumulus.
!!  -# For the "dynamic control", using a reference cloud work function, estimate the change in cloud work function due to the large-scale dynamics. Following the quasi-equilibrium assumption, calculate the cloud base mass flux required to keep the large-scale convective destabilization in balance with the stabilization effect of the convection.
!!  -# For the "feedback control", calculate updated values of the state variables by multiplying the cloud base mass flux and the tendencies calculated per unit cloud base mass flux from the static control.
!!  \section detailed Detailed Algorithm
!!  @{
      subroutine sascnvn(im,ix,km,jcap,delt,delp,prslp,psp,phil,ql,     &
     &     q1,t1,u1,v1,cldwrk,rn,kbot,ktop,kcnv,islimsk,                &
     &     dot,ncloud,ud_mf,dd_mf,dt_mf,cnvw,cnvc,                      &
!    &     q1,t1,u1,v1,rcs,cldwrk,rn,kbot,ktop,kcnv,islimsk,
!    &     dot,ncloud,ud_mf,dd_mf,dt_mf,me)
     &     clam,c0,c1,betal,betas,evfact,evfactl,pgcon)
!
      use machine , only : kind_phys
      use funcphys , only : fpvs
      use physcons, grav => con_g, cp => con_cp, hvap => con_hvap       &
     &,             rv => con_rv, fv => con_fvirt, t0c => con_t0c       &
     &,             cvap => con_cvap, cliq => con_cliq                  &
     &,             eps => con_eps, epsm1 => con_epsm1
      implicit none
!
      integer            im, ix,  km, jcap, ncloud,                     &
     &                   kbot(im), ktop(im), kcnv(im)                  
!    &,                  me
      real(kind=kind_phys) delt
      real(kind=kind_phys) psp(im),    delp(ix,km), prslp(ix,km)
      real(kind=kind_phys) ps(im),     del(ix,km),  prsl(ix,km),        &
     &                     ql(ix,km,2),q1(ix,km),   t1(ix,km),          &
     &                     u1(ix,km),  v1(ix,km),                       & !rcs(im),
     &                     cldwrk(im), rn(im),                          &
     &                     dot(ix,km), phil(ix,km),                     &
     &                     cnvw(ix,km), cnvc(ix,km),                    &
     &                     ud_mf(im,km),dd_mf(im,km),dt_mf(im,km) ! hchuang code change mass flux output
!
      integer              i, indx, jmn, k, kk, km1
      integer, dimension(im), intent(in) :: islimsk
!     integer              latd,lond
!
      real(kind=kind_phys) clam, cxlamu, xlamde, xlamdd
!
!     real(kind=kind_phys) detad
      real(kind=kind_phys) adw,     aup,     aafac,
     &                     beta,    betal,   betas,
     &                     c0,      dellat,  delta,
     &                     desdt,   dg,
     &                     dh,      dhh,     dp,
     &                     dq,      dqsdp,   dqsdt,   dt,
     &                     dt2,     dtmax,   dtmin,   dv1h,
     &                     dv1q,    dv2h,    dv2q,    dv1u,
     &                     dv1v,    dv2u,    dv2v,    dv3q,
     &                     dv3h,    dv3u,    dv3v,
     &                     dz,      dz1,     e1,      edtmax,
     &                     edtmaxl, edtmaxs, el2orc,  elocp,
     &                     es,      etah,    cthk,    dthk,
     &                     evef,    evfact,  evfactl, fact1,
     &                     fact2,   factor,  fjcap,   fkm,
     &                     g,       gamma,   pprime,
     &                     qlk,     qrch,    qs,      c1,
     &                     rain,    rfact,   shear,   tem1,
     &                     val,     val1,
     &                     val2,    w1,      w1l,     w1s,
     &                     w2,      w2l,     w2s,     w3,
     &                     w3l,     w3s,     w4,      w4l,
     &                     w4s,     xdby,    xpw,     xpwd,
     &                     xqrch,   mbdt,    tem,
     &                     ptem,    ptem1,   pgcon
!
      integer              kb(im), kbcon(im), kbcon1(im),
     &                     ktcon(im), ktcon1(im),
     &                     jmin(im), lmin(im), kbmax(im),
     &                     kbm(im), kmax(im)
!
      real(kind=kind_phys) aa1(im),     acrt(im),   acrtfct(im),
     &                     delhbar(im), delq(im),   delq2(im),
     &                     delqbar(im), delqev(im), deltbar(im),
     &                     deltv(im),   dtconv(im), edt(im),
     &                     edto(im),    edtx(im),   fld(im),
     &                     hcdo(im,km), hmax(im),   hmin(im),
     &                     ucdo(im,km), vcdo(im,km),aa2(im),
     &                     pbcdif(im),  pdot(im),   po(im,km),
     &                     pwavo(im),   pwevo(im),  xlamud(im),
     &                     qcdo(im,km), qcond(im),  qevap(im),
     &                     rntot(im),   vshear(im), xaa0(im),
     &                     xk(im),      xlamd(im),
     &                     xmb(im),     xmbmax(im), xpwav(im),
     &                     xpwev(im),   delubar(im),delvbar(im)
cj
      real(kind=kind_phys) cincr, cincrmax, cincrmin
cj
c  physical parameters
      parameter(g=grav)
      parameter(elocp=hvap/cp,
     &          el2orc=hvap*hvap/(rv*cp))
!     parameter(c0=.002,c1=.002,delta=fv)
      parameter(delta=fv)
      parameter(fact1=(cvap-cliq)/rv,fact2=hvap/rv-fact1*t0c)
      parameter(cthk=150.,cincrmax=180.,cincrmin=120.,dthk=25.)
c  local variables and arrays
      real(kind=kind_phys) pfld(im,km),    to(im,km),     qo(im,km),
     &                     uo(im,km),      vo(im,km),     qeso(im,km)
c  cloud water
!     real(kind=kind_phys) tvo(im,km)
      real(kind=kind_phys) qlko_ktcon(im), dellal(im,km), tvo(im,km),
     &                     dbyo(im,km),    zo(im,km),     xlamue(im,km),
     &                     fent1(im,km),   fent2(im,km),  frh(im,km),
     &                     heo(im,km),     heso(im,km),
     &                     qrcd(im,km),    dellah(im,km), dellaq(im,km),
     &                     dellau(im,km),  dellav(im,km), hcko(im,km),
     &                     ucko(im,km),    vcko(im,km),   qcko(im,km),
     &                     eta(im,km),     etad(im,km),   zi(im,km),
     &                     qrcko(im,km),   qrcdo(im,km),
     &                     pwo(im,km),     pwdo(im,km),
     &                     tx1(im),        sumx(im),      cnvwt(im,km)
!    &,                    rhbar(im)
!
      logical totflg, cnvflg(im), flg(im)
!
      real(kind=kind_phys) pcrit(15), acritt(15), acrit(15)
!     save pcrit, acritt
      data pcrit/850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,
     &           350.,300.,250.,200.,150./
      data acritt/.0633,.0445,.0553,.0664,.075,.1082,.1521,.2216,
     &           .3151,.3677,.41,.5255,.7663,1.1686,1.6851/
c  gdas derived acrit
c     data acritt/.203,.515,.521,.566,.625,.665,.659,.688,
c    &            .743,.813,.886,.947,1.138,1.377,1.896/
      real(kind=kind_phys) tf, tcr, tcrf
      parameter (tf=233.16, tcr=263.16, tcrf=1.0/(tcr-tf))
!
c-----------------------------------------------------------------------
!>  ## Compute preliminary quantities needed for static, dynamic, and feedback control portions of the algorithm.
!>  - Convert input pressure terms to centibar units.
!************************************************************************
!     convert input pa terms to cb terms  -- moorthi
      ps   = psp   * 0.001
      prsl = prslp * 0.001
      del  = delp  * 0.001
!************************************************************************
!
!
      km1 = km - 1
!>  - Initialize column-integrated and other single-value-per-column variable arrays.
c
c  initialize arrays
c
      do i=1,im
        cnvflg(i) = .true.
        rn(i)=0.
        kbot(i)=km+1
        ktop(i)=0
        kbcon(i)=km
        ktcon(i)=1
        dtconv(i) = 3600.
        cldwrk(i) = 0.
        pdot(i) = 0.
        pbcdif(i)= 0.
        lmin(i) = 1
        jmin(i) = 1
        qlko_ktcon(i) = 0.
        edt(i)  = 0.
        edto(i) = 0.
        edtx(i) = 0.
        acrt(i) = 0.
        acrtfct(i) = 1.
        aa1(i)  = 0.
        aa2(i)  = 0.
        xaa0(i) = 0.
        pwavo(i)= 0.
        pwevo(i)= 0.
        xpwav(i)= 0.
        xpwev(i)= 0.
        vshear(i) = 0.
      enddo
!>  - Initialize convective cloud water and cloud cover to zero.
      do k = 1, km
        do i = 1, im
          cnvw(i,k) = 0.
          cnvc(i,k) = 0.
        enddo
      enddo
!>  - Initialize updraft, downdraft, detrainment mass fluxes to zero.
! hchuang code change
      do k = 1, km
        do i = 1, im
          ud_mf(i,k) = 0.
          dd_mf(i,k) = 0.
          dt_mf(i,k) = 0.
        enddo
      enddo
!>  - Initialize the reference cloud work function, define min/max convective adjustment timescales, and tunable parameters.
c
      do k = 1, 15
        acrit(k) = acritt(k) * (975. - pcrit(k))
      enddo
      dt2 = delt
      val   =         1200.
      dtmin = max(dt2, val )
      val   =         3600.
      dtmax = max(dt2, val )
c  model tunable parameters are all here
      mbdt    = 10.
      edtmaxl = .3
      edtmaxs = .3
!     clam    = .1
      aafac   = .1
!     betal   = .15
!     betas   = .15
!     betal   = .05
!     betas   = .05
c     evef    = 0.07
!     evfact  = 0.3
!     evfactl = 0.3
!
      cxlamu  = 1.0e-4
      xlamde  = 1.0e-4
      xlamdd  = 1.0e-4
!
!     pgcon   = 0.7     ! gregory et al. (1997, qjrms)
!     pgcon   = 0.55    ! zhang & wu (2003,jas)
      fjcap   = (float(jcap) / 126.) ** 2
      val     =           1.
      fjcap   = max(fjcap,val)
      fkm     = (float(km) / 28.) ** 2
      fkm     = max(fkm,val)
      w1l     = -8.e-3
      w2l     = -4.e-2
      w3l     = -5.e-3
      w4l     = -5.e-4
      w1s     = -2.e-4
      w2s     = -2.e-3
      w3s     = -1.e-3
      w4s     = -2.e-5
!>  - Determine maximum indices for the parcel starting point (kbm), LFC (kbmax), and cloud top (kmax).
c
c  define top layer for search of the downdraft originating layer
c  and the maximum thetae for updraft
c
      do i=1,im
        kbmax(i) = km
        kbm(i)   = km
        kmax(i)  = km
        tx1(i)   = 1.0 / ps(i)
      enddo
!
      do k = 1, km
        do i=1,im
          if (prsl(i,k)*tx1(i) .gt. 0.04) kmax(i)  = k + 1
          if (prsl(i,k)*tx1(i) .gt. 0.45) kbmax(i) = k + 1
          if (prsl(i,k)*tx1(i) .gt. 0.70) kbm(i)   = k + 1
        enddo
      enddo
      do i=1,im
        kmax(i)  = min(km,kmax(i))
        kbmax(i) = min(kbmax(i),kmax(i))
        kbm(i)   = min(kbm(i),kmax(i))
      enddo
c
c  hydrostatic height assume zero terr and initially assume
c    updraft entrainment rate as an inverse function of height
c
!>  - Calculate hydrostatic height at layer centers assuming a flat surface (no terrain) from the geopotential.
      do k = 1, km
        do i=1,im
          zo(i,k) = phil(i,k) / g
        enddo
      enddo
!>  - Calculate interface height and the initial entrainment rate as an inverse function of height.
      do k = 1, km1
        do i=1,im
          zi(i,k) = 0.5*(zo(i,k)+zo(i,k+1))
          xlamue(i,k) = clam / zi(i,k)
        enddo
      enddo
!>  - Convert prsl from centibar to millibar, set normalized mass fluxes to 1, cloud properties to 0, and save model state variables (after advection/turbulence).
c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c   convert surface pressure to mb from cb
c
      do k = 1, km
        do i = 1, im
          if (k .le. kmax(i)) then
            pfld(i,k) = prsl(i,k) * 10.0
            eta(i,k)  = 1.
            fent1(i,k)= 1.
            fent2(i,k)= 1.
            frh(i,k)  = 0.
            hcko(i,k) = 0.
            qcko(i,k) = 0.
            qrcko(i,k)= 0.
            ucko(i,k) = 0.
            vcko(i,k) = 0.
            etad(i,k) = 1.
            hcdo(i,k) = 0.
            qcdo(i,k) = 0.
            ucdo(i,k) = 0.
            vcdo(i,k) = 0.
            qrcd(i,k) = 0.
            qrcdo(i,k)= 0.
            dbyo(i,k) = 0.
            pwo(i,k)  = 0.
            pwdo(i,k) = 0.
            dellal(i,k) = 0.
            to(i,k)   = t1(i,k)
            qo(i,k)   = q1(i,k)
            uo(i,k)   = u1(i,k)
            vo(i,k)   = v1(i,k)
!           uo(i,k)   = u1(i,k) * rcs(i)
!           vo(i,k)   = v1(i,k) * rcs(i)
            cnvwt(i,k)= 0.
          endif
        enddo
      enddo
c
c  column variables
c  p is pressure of the layer (mb)
c  t is temperature at t-dt (k)..tn
c  q is mixing ratio at t-dt (kg/kg)..qn
c  to is temperature at t+dt (k)... this is after advection and turbulan
c  qo is mixing ratio at t+dt (kg/kg)..q1
c
!>  - Calculate saturation mixing ratio and enforce minimum moisture values.
      do k = 1, km
        do i=1,im
          if (k .le. kmax(i)) then
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (pfld(i,k) + epsm1*qeso(i,k))
            val1      =             1.e-8
            qeso(i,k) = max(qeso(i,k), val1)
            val2      =           1.e-10
            qo(i,k)   = max(qo(i,k), val2 )
!           qo(i,k)   = min(qo(i,k),qeso(i,k))
!           tvo(i,k)  = to(i,k) + delta * to(i,k) * qo(i,k)
          endif
        enddo
      enddo
c
c  compute moist static energy
c
!>  - Calculate moist static energy (heo) and saturation moist static energy (heso).
      do k = 1, km
        do i=1,im
          if (k .le. kmax(i)) then
!           tem       = g * zo(i,k) + cp * to(i,k)
            tem       = phil(i,k) + cp * to(i,k)
            heo(i,k)  = tem  + hvap * qo(i,k)
            heso(i,k) = tem  + hvap * qeso(i,k)
c           heo(i,k)  = min(heo(i,k),heso(i,k))
          endif
        enddo
      enddo

!> ## Perform calculations related to the updraft of the entraining/detraining cloud model ("static control").
c
c  determine level with largest moist static energy
c  this is the level where updraft starts
c
!> - Search below index "kbm" for the level of maximum moist static energy.
      do i=1,im
        hmax(i) = heo(i,1)
        kb(i)   = 1
      enddo
      do k = 2, km
        do i=1,im
          if (k .le. kbm(i)) then
            if(heo(i,k).gt.hmax(i)) then
              kb(i)   = k
              hmax(i) = heo(i,k)
            endif
          endif
        enddo
      enddo
!> - Calculate the temperature, water vapor mixing ratio, and pressure at interface levels.
c
      do k = 1, km1
        do i=1,im
          if (k .le. kmax(i)-1) then
            dz      = .5 * (zo(i,k+1) - zo(i,k))
            dp      = .5 * (pfld(i,k+1) - pfld(i,k))
            es      = 0.01 * fpvs(to(i,k+1))      ! fpvs is in pa
            pprime  = pfld(i,k+1) + epsm1 * es
            qs      = eps * es / pprime
            dqsdp   = - qs / pprime
            desdt   = es * (fact1 / to(i,k+1) + fact2 / (to(i,k+1)**2))
            dqsdt   = qs * pfld(i,k+1) * desdt / (es * pprime)
            gamma   = el2orc * qeso(i,k+1) / (to(i,k+1)**2)
            dt      = (g * dz + hvap * dqsdp * dp) / (cp * (1. + gamma))
            dq      = dqsdt * dt + dqsdp * dp
            to(i,k) = to(i,k+1) + dt
            qo(i,k) = qo(i,k+1) + dq
            po(i,k) = .5 * (pfld(i,k) + pfld(i,k+1))
          endif
        enddo
      enddo
!> - Recalculate saturation mixing ratio, moist static energy, saturation moist static energy, and horizontal momentum on interface levels. Enforce minimum mixing ratios and calculate \f$(1 - RH)\f$.
!
      do k = 1, km1
        do i=1,im
          if (k .le. kmax(i)-1) then
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (po(i,k) + epsm1*qeso(i,k))
            val1      =             1.e-8
            qeso(i,k) = max(qeso(i,k), val1)
            val2      =           1.e-10
            qo(i,k)   = max(qo(i,k), val2 )
!           qo(i,k)   = min(qo(i,k),qeso(i,k))
            frh(i,k)  = 1. - min(qo(i,k)/qeso(i,k), 1.)
            heo(i,k)  = .5 * g * (zo(i,k) + zo(i,k+1)) +
     &                  cp * to(i,k) + hvap * qo(i,k)
            heso(i,k) = .5 * g * (zo(i,k) + zo(i,k+1)) +
     &                  cp * to(i,k) + hvap * qeso(i,k)
            uo(i,k)   = .5 * (uo(i,k) + uo(i,k+1))
            vo(i,k)   = .5 * (vo(i,k) + vo(i,k+1))
          endif
        enddo
      enddo
c
c  look for the level of free convection as cloud base
c
!> - Search below the index "kbmax" for the level of free convection (LFC) where the condition \f$h_b > h^*\f$ is first met, where \f$h_b, h^*\f$ are the state moist static energy at the parcel's starting level and saturation moist static energy, respectively. Set "kbcon" to the index of the LFC.
      do i=1,im
        flg(i)   = .true.
        kbcon(i) = kmax(i)
      enddo
      do k = 1, km1
        do i=1,im
          if (flg(i).and.k.le.kbmax(i)) then
            if(k.gt.kb(i).and.heo(i,kb(i)).gt.heso(i,k)) then
              kbcon(i) = k
              flg(i)   = .false.
            endif
          endif
        enddo
      enddo
!> - If no LFC, return to the calling routine without modifying state variables.
c
      do i=1,im
        if(kbcon(i).eq.kmax(i)) cnvflg(i) = .false.
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
c
c  determine critical convective inhibition
c  as a function of vertical velocity at cloud base.
c
!> - Determine the vertical pressure velocity at the LFC. After Han and Pan (2011) \cite han_and_pan_2011 , determine the maximum pressure thickness between a parcel's starting level and the LFC. If a parcel doesn't reach the LFC within the critical thickness, then the convective inhibition is deemed too great for convection to be triggered, and the subroutine returns to the calling routine without modifying the state variables.
      do i=1,im
        if(cnvflg(i)) then
!         pdot(i)  = 10.* dot(i,kbcon(i))
          pdot(i)  = 0.01 * dot(i,kbcon(i)) ! now dot is in pa/s
        endif
      enddo
      do i=1,im
        if(cnvflg(i)) then
          if(islimsk(i) == 1) then
            w1 = w1l
            w2 = w2l
            w3 = w3l
            w4 = w4l
          else
            w1 = w1s
            w2 = w2s
            w3 = w3s
            w4 = w4s
          endif
          if(pdot(i).le.w4) then
            tem = (pdot(i) - w4) / (w3 - w4)
          elseif(pdot(i).ge.-w4) then
            tem = - (pdot(i) + w4) / (w4 - w3)
          else
            tem = 0.
          endif
          val1    =             -1.
          tem = max(tem,val1)
          val2    =             1.
          tem = min(tem,val2)
          tem = 1. - tem
          tem1= .5*(cincrmax-cincrmin)
          cincr = cincrmax - tem * tem1
          pbcdif(i) = pfld(i,kb(i)) - pfld(i,kbcon(i))
          if(pbcdif(i).gt.cincr) then
             cnvflg(i) = .false.
          endif
        endif
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
c
c  assume that updraft entrainment rate above cloud base is
c    same as that at cloud base
c
!> - Calculate the entrainment rate according to Han and Pan (2011) \cite han_and_pan_2011 , equation 8, after Bechtold et al. (2008) \cite bechtold_et_al_2008, equation 2 given by:
!!  \f[
!!  \epsilon = \epsilon_0F_0 + d_1\left(1-RH\right)F_1
!!  \f]
!!  where \f$\epsilon_0\f$ is the cloud base entrainment rate, \f$d_1\f$ is a tunable constant, and \f$F_0=\left(\frac{q_s}{q_{s,b}}\right)^2\f$ and \f$F_1=\left(\frac{q_s}{q_{s,b}}\right)^3\f$ where \f$q_s\f$ and \f$q_{s,b}\f$ are the saturation specific humidities at a given level and cloud base, respectively. The detrainment rate in the cloud is assumed to be equal to the entrainment rate at cloud base.
      do k = 2, km1
        do i=1,im
          if(cnvflg(i).and.
     &      (k.gt.kbcon(i).and.k.lt.kmax(i))) then
              xlamue(i,k) = xlamue(i,kbcon(i))
          endif
        enddo
      enddo
c
c  assume the detrainment rate for the updrafts to be same as
c  the entrainment rate at cloud base
c
!> - The updraft detrainment rate is set constant and equal to the entrainment rate at cloud base.
      do i = 1, im
        if(cnvflg(i)) then
          xlamud(i) = xlamue(i,kbcon(i))
        endif
      enddo
c
c  functions rapidly decreasing with height, mimicking a cloud ensemble
c    (bechtold et al., 2008)
c
      do k = 2, km1
        do i=1,im
          if(cnvflg(i).and.
     &      (k.gt.kbcon(i).and.k.lt.kmax(i))) then
              tem = qeso(i,k)/qeso(i,kbcon(i))
              fent1(i,k) = tem**2
              fent2(i,k) = tem**3
          endif
        enddo
      enddo
c
c  final entrainment rate as the sum of turbulent part and organized entrainment
c    depending on the environmental relative humidity
c    (bechtold et al., 2008)
c
      do k = 2, km1
        do i=1,im
          if(cnvflg(i).and.
     &      (k.ge.kbcon(i).and.k.lt.kmax(i))) then
              tem = cxlamu * frh(i,k) * fent2(i,k)
              xlamue(i,k) = xlamue(i,k)*fent1(i,k) + tem
          endif
        enddo
      enddo
c
c  determine updraft mass flux for the subcloud layers
c
!> - Calculate the normalized mass flux for subcloud and in-cloud layers according to Pan and Wu (1995) \cite pan_and_wu_1995 equation 1:
!!  \f[
!!  \frac{1}{\eta}\frac{\partial \eta}{\partial z} = \lambda_e - \lambda_d
!!  \f]
!!  where \f$\eta\f$ is the normalized mass flux, \f$\lambda_e\f$ is the entrainment rate and \f$\lambda_d\f$ is the detrainment rate.
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i)) then
            if(k.lt.kbcon(i).and.k.ge.kb(i)) then
              dz       = zi(i,k+1) - zi(i,k)
              ptem     = 0.5*(xlamue(i,k)+xlamue(i,k+1))-xlamud(i)
              eta(i,k) = eta(i,k+1) / (1. + ptem * dz)
            endif
          endif
        enddo
      enddo
c
c  compute mass flux above cloud base
c
      do k = 2, km1
        do i = 1, im
         if(cnvflg(i))then
           if(k.gt.kbcon(i).and.k.lt.kmax(i)) then
              dz       = zi(i,k) - zi(i,k-1)
              ptem     = 0.5*(xlamue(i,k)+xlamue(i,k-1))-xlamud(i)
              eta(i,k) = eta(i,k-1) * (1 + ptem * dz)
           endif
         endif
        enddo
      enddo
c
c  compute updraft cloud properties
c
!> - Set initial cloud properties equal to the state variables at cloud base.
      do i = 1, im
        if(cnvflg(i)) then
          indx         = kb(i)
          hcko(i,indx) = heo(i,indx)
          ucko(i,indx) = uo(i,indx)
          vcko(i,indx) = vo(i,indx)
          pwavo(i)     = 0.
        endif
      enddo
c
c  cloud property is modified by the entrainment process
c
!> - Calculate the cloud properties as a parcel ascends, modified by entrainment and detrainment. Discretization follows Appendix B of Grell (1993) \cite grell_1993 . Following Han and Pan (2006) \cite han_and_pan_2006, the convective momentum transport is reduced by the convection-induced pressure gradient force by the constant "pgcon", currently set to 0.55 after Zhang and Wu (2003) \cite zhang_and_wu_2003 .
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k.gt.kb(i).and.k.lt.kmax(i)) then
              dz   = zi(i,k) - zi(i,k-1)
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5 * xlamud(i) * dz
              factor = 1. + tem - tem1
              ptem = 0.5 * tem + pgcon
              ptem1= 0.5 * tem - pgcon
              hcko(i,k) = ((1.-tem1)*hcko(i,k-1)+tem*0.5*
     &                     (heo(i,k)+heo(i,k-1)))/factor
              ucko(i,k) = ((1.-tem1)*ucko(i,k-1)+ptem*uo(i,k)
     &                     +ptem1*uo(i,k-1))/factor
              vcko(i,k) = ((1.-tem1)*vcko(i,k-1)+ptem*vo(i,k)
     &                     +ptem1*vo(i,k-1))/factor
              dbyo(i,k) = hcko(i,k) - heso(i,k)
            endif
          endif
        enddo
      enddo
c
c   taking account into convection inhibition due to existence of
c    dry layers below cloud base
c
!> - With entrainment, recalculate the LFC as the first level where buoyancy is positive. The difference in pressure levels between LFCs calculated with/without entrainment must be less than a threshold (currently 25 hPa). Otherwise, convection is inhibited and the scheme returns to the calling routine without modifying the state variables. This is the subcloud dryness trigger modification discussed in Han and Pan (2011) \cite han_and_pan_2011.
      do i=1,im
        flg(i) = cnvflg(i)
        kbcon1(i) = kmax(i)
      enddo
      do k = 2, km1
      do i=1,im
        if (flg(i).and.k.lt.kmax(i)) then
          if(k.ge.kbcon(i).and.dbyo(i,k).gt.0.) then
            kbcon1(i) = k
            flg(i)    = .false.
          endif
        endif
      enddo
      enddo
      do i=1,im
        if(cnvflg(i)) then
          if(kbcon1(i).eq.kmax(i)) cnvflg(i) = .false.
        endif
      enddo
      do i=1,im
        if(cnvflg(i)) then
          tem = pfld(i,kbcon(i)) - pfld(i,kbcon1(i))
          if(tem.gt.dthk) then
             cnvflg(i) = .false.
          endif
        endif
      enddo
!!
      totflg = .true.
      do i = 1, im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
c
c  determine first guess cloud top as the level of zero buoyancy
c
!> - Calculate the cloud top as the first level where parcel buoyancy becomes negative. If the thickness of the calculated convection is less than a threshold (currently 150 hPa), then convection is inhibited, and the scheme returns to the calling routine.
      do i = 1, im
        flg(i) = cnvflg(i)
        ktcon(i) = 1
      enddo
      do k = 2, km1
      do i = 1, im
        if (flg(i).and.k .lt. kmax(i)) then
          if(k.gt.kbcon1(i).and.dbyo(i,k).lt.0.) then
             ktcon(i) = k
             flg(i)   = .false.
          endif
        endif
      enddo
      enddo
c
      do i = 1, im
        if(cnvflg(i)) then
          tem = pfld(i,kbcon(i))-pfld(i,ktcon(i))
          if(tem.lt.cthk) cnvflg(i) = .false.
        endif
      enddo
!!
      totflg = .true.
      do i = 1, im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
c
c  search for downdraft originating level above theta-e minimum
c
!> - To originate the downdraft, search for the level above the minimum in moist static energy. Return to the calling routine without modification if this level is determined to be outside of the convective cloud layers.
      do i = 1, im
        if(cnvflg(i)) then
           hmin(i) = heo(i,kbcon1(i))
           lmin(i) = kbmax(i)
           jmin(i) = kbmax(i)
        endif
      enddo
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i) .and. k .le. kbmax(i)) then
            if(k.gt.kbcon1(i).and.heo(i,k).lt.hmin(i)) then
               lmin(i) = k + 1
               hmin(i) = heo(i,k)
            endif
          endif
        enddo
      enddo
c
c  make sure that jmin(i) is within the cloud
c
      do i = 1, im
        if(cnvflg(i)) then
          jmin(i) = min(lmin(i),ktcon(i)-1)
          jmin(i) = max(jmin(i),kbcon1(i)+1)
          if(jmin(i).ge.ktcon(i)) cnvflg(i) = .false.
        endif
      enddo
c
c  specify upper limit of mass flux at cloud base
c
!> - Calculate the maximum value of the cloud base mass flux using the CFL-criterion-based formula of Han and Pan (2011) \cite han_and_pan_2011, equation 7.
      do i = 1, im
        if(cnvflg(i)) then
!         xmbmax(i) = .1
!
          k = kbcon(i)
          dp = 1000. * del(i,k)
          xmbmax(i) = dp / (g * dt2)
!
!         tem = dp / (g * dt2)
!         xmbmax(i) = min(tem, xmbmax(i))
        endif
      enddo
c
c  compute cloud moisture property and precipitation
c
!> - Initialize the cloud moisture at cloud base and set the cloud work function to zero.
      do i = 1, im
        if (cnvflg(i)) then
          aa1(i) = 0.
          qcko(i,kb(i)) = qo(i,kb(i))
          qrcko(i,kb(i)) = qo(i,kb(i))
!         rhbar(i) = 0.
        endif
      enddo
!> - Calculate the moisture content of the entraining/detraining parcel (qcko) and the value it would have if just saturated (qrch), according to equation A.14 in Grell (1993) \cite grell_1993 . Their difference is the amount of convective cloud water (qlk = rain + condensate). Determine the portion of convective cloud water that remains suspended and the portion that is converted into convective precipitation (pwo). Calculate and save the negative cloud work function (aa1) due to water loading. The liquid water in the updraft layer is assumed to be detrained from the layers above the level of the minimum moist static energy into the grid-scale cloud water (dellal).
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k.gt.kb(i).and.k.lt.ktcon(i)) then
              dz    = zi(i,k) - zi(i,k-1)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              qrch = qeso(i,k)
     &             + gamma * dbyo(i,k) / (hvap * (1. + gamma))
cj
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5 * xlamud(i) * dz
              factor = 1. + tem - tem1
              qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*
     &                     (qo(i,k)+qo(i,k-1)))/factor
              qrcko(i,k) = qcko(i,k)
cj
              dq = eta(i,k) * (qcko(i,k) - qrch)
c
!             rhbar(i) = rhbar(i) + qo(i,k) / qeso(i,k)
c
c  check if there is excess moisture to release latent heat
c
              if(k.ge.kbcon(i).and.dq.gt.0.) then
                etah = .5 * (eta(i,k) + eta(i,k-1))
                dp = 1000. * del(i,k)
                if(ncloud.gt.0..and.k.gt.jmin(i)) then
                  qlk = dq / (eta(i,k) + etah * (c0 + c1) * dz)
                  dellal(i,k) = etah * c1 * dz * qlk * g / dp
                else
                  qlk = dq / (eta(i,k) + etah * c0 * dz)
                endif
                aa1(i) = aa1(i) - dz * g * qlk
                qcko(i,k) = qlk + qrch
                pwo(i,k) = etah * c0 * dz * qlk
                pwavo(i) = pwavo(i) + pwo(i,k)
!               cnvwt(i,k) = (etah*qlk + pwo(i,k)) * g / dp
                cnvwt(i,k) = etah * qlk * g / dp
              endif
            endif
          endif
        enddo
      enddo
c
!     do i = 1, im
!       if(cnvflg(i)) then
!         indx = ktcon(i) - kb(i) - 1
!         rhbar(i) = rhbar(i) / float(indx)
!       endif
!     enddo
c
c  calculate cloud work function
c
!> - Calculate the cloud work function according to Pan and Wu (1995) \cite pan_and_wu_1995 equation 4:
!!  \f[
!!  A_u=\int_{z_0}^{z_t}\frac{g}{c_pT(z)}\frac{\eta}{1 + \gamma}[h(z)-h^*(z)]dz
!!  \f]
!! (discretized according to Grell (1993) \cite grell_1993 equation B.10 using B.2 and B.3 of Arakawa and Schubert (1974) \cite arakawa_and_schubert_1974 and assuming \f$\eta=1\f$) where \f$A_u\f$ is the updraft cloud work function, \f$z_0\f$ and \f$z_t\f$ are cloud base and cloud top, respectively, \f$\gamma = \frac{L}{c_p}\left(\frac{\partial \overline{q_s}}{\partial T}\right)_p\f$ and other quantities are previously defined.
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k.ge.kbcon(i).and.k.lt.ktcon(i)) then
              dz1 = zo(i,k+1) - zo(i,k)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              rfact =  1. + delta * cp * gamma
     &                 * to(i,k) / hvap
              aa1(i) = aa1(i) +
     &                 dz1 * (g / (cp * to(i,k)))
     &                 * dbyo(i,k) / (1. + gamma)
     &                 * rfact
              val = 0.
              aa1(i)=aa1(i)+
     &                 dz1 * g * delta *
     &                 max(val,(qeso(i,k) - qo(i,k)))
            endif
          endif
        enddo
      enddo
!> - If the updraft cloud work function is negative, convection does not occur, and the scheme returns to the calling routine.
      do i = 1, im
        if(cnvflg(i).and.aa1(i).le.0.) cnvflg(i) = .false.
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
c
c  estimate the onvective overshooting as the level
c    where the [aafac * cloud work function] becomes zero,
c    which is the final cloud top
c
!> - Continue calculating the cloud work function past the point of neutral buoyancy to represent overshooting according to Han and Pan (2011) \cite han_and_pan_2011 . Convective overshooting stops when \f$ cA_u < 0\f$ where \f$c\f$ is currently 10%, or when 10% of the updraft cloud work function has been consumed by the stable buoyancy force.
      do i = 1, im
        if (cnvflg(i)) then
          aa2(i) = aafac * aa1(i)
        endif
      enddo
c
      do i = 1, im
        flg(i) = cnvflg(i)
        ktcon1(i) = kmax(i) - 1
      enddo
      do k = 2, km1
        do i = 1, im
          if (flg(i)) then
            if(k.ge.ktcon(i).and.k.lt.kmax(i)) then
              dz1 = zo(i,k+1) - zo(i,k)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              rfact =  1. + delta * cp * gamma
     &                 * to(i,k) / hvap
              aa2(i) = aa2(i) +
     &                 dz1 * (g / (cp * to(i,k)))
     &                 * dbyo(i,k) / (1. + gamma)
     &                 * rfact
              if(aa2(i).lt.0.) then
                ktcon1(i) = k
                flg(i) = .false.
              endif
            endif
          endif
        enddo
      enddo
c
c  compute cloud moisture property, detraining cloud water
c    and precipitation in overshooting layers
c
!> - For the overshooting convection, calculate the moisture content of the entraining/detraining parcel as before. Partition convective cloud water and precipitation and detrain convective cloud water above the mimimum in moist static energy.
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k.ge.ktcon(i).and.k.lt.ktcon1(i)) then
              dz    = zi(i,k) - zi(i,k-1)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              qrch = qeso(i,k)
     &             + gamma * dbyo(i,k) / (hvap * (1. + gamma))
cj
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5 * xlamud(i) * dz
              factor = 1. + tem - tem1
              qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*
     &                     (qo(i,k)+qo(i,k-1)))/factor
              qrcko(i,k) = qcko(i,k)
cj
              dq = eta(i,k) * (qcko(i,k) - qrch)
c
c  check if there is excess moisture to release latent heat
c
              if(dq.gt.0.) then
                etah = .5 * (eta(i,k) + eta(i,k-1))
                dp = 1000. * del(i,k)
                if(ncloud.gt.0.) then
                  qlk = dq / (eta(i,k) + etah * (c0 + c1) * dz)
                  dellal(i,k) = etah * c1 * dz * qlk * g / dp
                else
                  qlk = dq / (eta(i,k) + etah * c0 * dz)
                endif
                qcko(i,k) = qlk + qrch
                pwo(i,k) = etah * c0 * dz * qlk
                pwavo(i) = pwavo(i) + pwo(i,k)
!               cnvwt(i,k) = (etah*qlk + pwo(i,k)) * g / dp
                cnvwt(i,k) = etah * qlk * g / dp
              endif
            endif
          endif
        enddo
      enddo
c
c exchange ktcon with ktcon1
c
!> - Swap the indices of the convective cloud top (ktcon) and the overshooting convection top (ktcon1) to use the same cloud top level in the calculations of \f$A^+\f$ and \f$A^*\f$.
      do i = 1, im
        if(cnvflg(i)) then
          kk = ktcon(i)
          ktcon(i) = ktcon1(i)
          ktcon1(i) = kk
        endif
      enddo
c
c  this section is ready for cloud water
c
!> - Separate the total updraft cloud water at cloud top into vapor and condensate.
      if(ncloud.gt.0) then
c
c  compute liquid and vapor separation at cloud top
c
      do i = 1, im
        if(cnvflg(i)) then
          k = ktcon(i) - 1
          gamma = el2orc * qeso(i,k) / (to(i,k)**2)
          qrch = qeso(i,k)
     &         + gamma * dbyo(i,k) / (hvap * (1. + gamma))
          dq = qcko(i,k) - qrch
c
c  check if there is excess moisture to release latent heat
c
          if(dq.gt.0.) then
            qlko_ktcon(i) = dq
            qcko(i,k) = qrch
          endif
        endif
      enddo
      endif
c
ccccc if(lat.eq.latd.and.lon.eq.lond.and.cnvflg(i)) then
ccccc   print *, ' aa1(i) before dwndrft =', aa1(i)
ccccc endif
c
c------- downdraft calculations
c
c--- compute precipitation efficiency in terms of windshear
c
!> ## Perform calculations related to the downdraft of the entraining/detraining cloud model ("static control").
!! - First, in order to calculate the downdraft mass flux (as a fraction of the updraft mass flux), calculate the wind shear and precipitation efficiency according to equation 58 in Fritsch and Chappell (1980) \cite fritsch_and_chappell_1980 :
!! \f[
!! E = 1.591 - 0.639\frac{\Delta V}{\Delta z} + 0.0953\left(\frac{\Delta V}{\Delta z}\right)^2 - 0.00496\left(\frac{\Delta V}{\Delta z}\right)^3
!! \f]
!! where \f$\Delta V\f$ is the integrated horizontal shear over the cloud depth, \f$\Delta z\f$, (the ratio is converted to units of \f$10^{-3} s^{-1}\f$). The variable "edto" is \f$1-E\f$ and is constrained to the range \f$[0,0.9]\f$.
      do i = 1, im
        if(cnvflg(i)) then
          vshear(i) = 0.
        endif
      enddo
      do k = 2, km
        do i = 1, im
          if (cnvflg(i)) then
            if(k.gt.kb(i).and.k.le.ktcon(i)) then
              shear= sqrt((uo(i,k)-uo(i,k-1)) ** 2
     &                  + (vo(i,k)-vo(i,k-1)) ** 2)
              vshear(i) = vshear(i) + shear
            endif
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
          vshear(i) = 1.e3 * vshear(i) / (zi(i,ktcon(i))-zi(i,kb(i)))
          e1=1.591-.639*vshear(i)
     &       +.0953*(vshear(i)**2)-.00496*(vshear(i)**3)
          edt(i)=1.-e1
          val =         .9
          edt(i) = min(edt(i),val)
          val =         .0
          edt(i) = max(edt(i),val)
          edto(i)=edt(i)
          edtx(i)=edt(i)
        endif
      enddo
c
c  determine detrainment rate between 1 and kbcon
c
!> - Next, calculate the variable detrainment rate between the surface and the LFC according to:
!! \f[
!! \lambda_d = \frac{1-\beta^{\frac{1}{k_{LFC}}}}{\overline{\Delta z}}
!! \f]
!! \f$\lambda_d\f$ is the detrainment rate, \f$\beta\f$ is a constant currently set to 0.05, \f$k_{LFC}\f$ is the vertical index of the LFC level, and \f$\overline{\Delta z}\f$ is the average vertical grid spacing below the LFC.
      do i = 1, im
        if(cnvflg(i)) then
          sumx(i) = 0.
        endif
      enddo
      do k = 1, km1
      do i = 1, im
        if(cnvflg(i).and.k.ge.1.and.k.lt.kbcon(i)) then
          dz = zi(i,k+1) - zi(i,k)
          sumx(i) = sumx(i) + dz
        endif
      enddo
      enddo
      do i = 1, im
        beta = betas
        if(islimsk(i) == 1) beta = betal
        if(cnvflg(i)) then
          dz  = (sumx(i)+zi(i,1))/float(kbcon(i))
          tem = 1./float(kbcon(i))
          xlamd(i) = (1.-beta**tem)/dz
        endif
      enddo
c
c  determine downdraft mass flux
c
!> - Calculate the normalized downdraft mass flux from equation 1 of Pan and Wu (1995) \cite pan_and_wu_1995 . Downdraft entrainment and detrainment rates are constants from the downdraft origination to the LFC.
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i) .and. k .le. kmax(i)-1) then
           if(k.lt.jmin(i).and.k.ge.kbcon(i)) then
              dz        = zi(i,k+1) - zi(i,k)
              ptem      = xlamdd - xlamde
              etad(i,k) = etad(i,k+1) * (1. - ptem * dz)
           else if(k.lt.kbcon(i)) then
              dz        = zi(i,k+1) - zi(i,k)
              ptem      = xlamd(i) + xlamdd - xlamde
              etad(i,k) = etad(i,k+1) * (1. - ptem * dz)
           endif
          endif
        enddo
      enddo
c
c--- downdraft moisture properties
c
!> - Set initial cloud downdraft properties equal to the state variables at the downdraft origination level.
      do i = 1, im
        if(cnvflg(i)) then
          jmn = jmin(i)
          hcdo(i,jmn) = heo(i,jmn)
          qcdo(i,jmn) = qo(i,jmn)
          qrcdo(i,jmn)= qo(i,jmn)
          ucdo(i,jmn) = uo(i,jmn)
          vcdo(i,jmn) = vo(i,jmn)
          pwevo(i) = 0.
        endif
      enddo
cj
!> - Calculate the cloud properties as a parcel descends, modified by entrainment and detrainment. Discretization follows Appendix B of Grell (1993) \cite grell_1993 .
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i) .and. k.lt.jmin(i)) then
              dz = zi(i,k+1) - zi(i,k)
              if(k.ge.kbcon(i)) then
                 tem  = xlamde * dz
                 tem1 = 0.5 * xlamdd * dz
              else
                 tem  = xlamde * dz
                 tem1 = 0.5 * (xlamd(i)+xlamdd) * dz
              endif
              factor = 1. + tem - tem1
              ptem = 0.5 * tem - pgcon
              ptem1= 0.5 * tem + pgcon
              hcdo(i,k) = ((1.-tem1)*hcdo(i,k+1)+tem*0.5*
     &                     (heo(i,k)+heo(i,k+1)))/factor
              ucdo(i,k) = ((1.-tem1)*ucdo(i,k+1)+ptem*uo(i,k+1)
     &                     +ptem1*uo(i,k))/factor
              vcdo(i,k) = ((1.-tem1)*vcdo(i,k+1)+ptem*vo(i,k+1)
     &                     +ptem1*vo(i,k))/factor
              dbyo(i,k) = hcdo(i,k) - heso(i,k)
          endif
        enddo
      enddo
c
!> - Compute the amount of moisture that is necessary to keep the downdraft saturated.
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i).and.k.lt.jmin(i)) then
              gamma      = el2orc * qeso(i,k) / (to(i,k)**2)
              qrcdo(i,k) = qeso(i,k)+
     &                (1./hvap)*(gamma/(1.+gamma))*dbyo(i,k)
!             detad      = etad(i,k+1) - etad(i,k)
cj
              dz = zi(i,k+1) - zi(i,k)
              if(k.ge.kbcon(i)) then
                 tem  = xlamde * dz
                 tem1 = 0.5 * xlamdd * dz
              else
                 tem  = xlamde * dz
                 tem1 = 0.5 * (xlamd(i)+xlamdd) * dz
              endif
              factor = 1. + tem - tem1
              qcdo(i,k) = ((1.-tem1)*qrcdo(i,k+1)+tem*0.5*
     &                     (qo(i,k)+qo(i,k+1)))/factor
cj
!             pwdo(i,k)  = etad(i,k+1) * qcdo(i,k+1) -
!    &                     etad(i,k) * qrcdo(i,k)
!             pwdo(i,k)  = pwdo(i,k) - detad *
!    &                    .5 * (qrcdo(i,k) + qrcdo(i,k+1))
cj
              pwdo(i,k)  = etad(i,k) * (qcdo(i,k) - qrcdo(i,k))
              pwevo(i)   = pwevo(i) + pwdo(i,k)
          endif
        enddo
      enddo
c
c--- final downdraft strength dependent on precip
c--- efficiency (edt), normalized condensate (pwav), and
c--- evaporate (pwev)
c
!> - Update the precipitation efficiency (edto) based on the ratio of normalized cloud condensate (pwavo) to normalized cloud evaporate (pwevo).
      do i = 1, im
        edtmax = edtmaxl
        if(islimsk(i) == 0) edtmax = edtmaxs
        if(cnvflg(i)) then
          if(pwevo(i).lt.0.) then
            edto(i) = -edto(i) * pwavo(i) / pwevo(i)
            edto(i) = min(edto(i),edtmax)
          else
            edto(i) = 0.
          endif
        endif
      enddo
c
c--- downdraft cloudwork functions
c
!> - Calculate downdraft cloud work function (\f$A_d\f$) according to equation A.42 (discretized by B.11) in Grell (1993) \cite grell_1993 . Add it to the updraft cloud work function, \f$A_u\f$.
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i) .and. k .lt. jmin(i)) then
              gamma = el2orc * qeso(i,k) / to(i,k)**2
              dhh=hcdo(i,k)
              dt=to(i,k)
              dg=gamma
              dh=heso(i,k)
              dz=-1.*(zo(i,k+1)-zo(i,k))
              aa1(i)=aa1(i)+edto(i)*dz*(g/(cp*dt))*((dhh-dh)/(1.+dg))
     &               *(1.+delta*cp*dg*dt/hvap)
              val=0.
              aa1(i)=aa1(i)+edto(i)*
     &        dz*g*delta*max(val,(qeso(i,k)-qo(i,k)))
          endif
        enddo
      enddo
!> - Check for negative total cloud work function; if found, return to calling routine without modifying state variables.
      do i = 1, im
        if(cnvflg(i).and.aa1(i).le.0.) then
           cnvflg(i) = .false.
        endif
      enddo
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
c
c--- what would the change be, that a cloud with unit mass
c--- will do to the environment?
c
!> - Calculate the change in moist static energy, moisture mixing ratio, and horizontal winds per unit cloud base mass flux near the surface using equations B.18 and B.19 from Grell (1993) \cite grell_1993, for all layers below cloud top from equations B.14 and B.15, and for the cloud top from B.16 and B.17.
      do k = 1, km
        do i = 1, im
          if(cnvflg(i) .and. k .le. kmax(i)) then
            dellah(i,k) = 0.
            dellaq(i,k) = 0.
            dellau(i,k) = 0.
            dellav(i,k) = 0.
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
          dp = 1000. * del(i,1)
          dellah(i,1) = edto(i) * etad(i,1) * (hcdo(i,1)
     &                   - heo(i,1)) * g / dp
          dellaq(i,1) = edto(i) * etad(i,1) * (qrcdo(i,1)
     &                   - qo(i,1)) * g / dp
          dellau(i,1) = edto(i) * etad(i,1) * (ucdo(i,1)
     &                   - uo(i,1)) * g / dp
          dellav(i,1) = edto(i) * etad(i,1) * (vcdo(i,1)
     &                   - vo(i,1)) * g / dp
        endif
      enddo
c
c--- changed due to subsidence and entrainment
c
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i).and.k.lt.ktcon(i)) then
              aup = 1.
              if(k.le.kb(i)) aup = 0.
              adw = 1.
              if(k.gt.jmin(i)) adw = 0.
              dp = 1000. * del(i,k)
              dz = zi(i,k) - zi(i,k-1)
c
              dv1h = heo(i,k)
              dv2h = .5 * (heo(i,k) + heo(i,k-1))
              dv3h = heo(i,k-1)
              dv1q = qo(i,k)
              dv2q = .5 * (qo(i,k) + qo(i,k-1))
              dv3q = qo(i,k-1)
              dv1u = uo(i,k)
              dv2u = .5 * (uo(i,k) + uo(i,k-1))
              dv3u = uo(i,k-1)
              dv1v = vo(i,k)
              dv2v = .5 * (vo(i,k) + vo(i,k-1))
              dv3v = vo(i,k-1)
c
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1))
              tem1 = xlamud(i)
c
              if(k.le.kbcon(i)) then
                ptem  = xlamde
                ptem1 = xlamd(i)+xlamdd
              else
                ptem  = xlamde
                ptem1 = xlamdd
              endif
cj
              dellah(i,k) = dellah(i,k) +
     &     ((aup*eta(i,k)-adw*edto(i)*etad(i,k))*dv1h
     &    - (aup*eta(i,k-1)-adw*edto(i)*etad(i,k-1))*dv3h
     &    - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2h*dz
     &    +  aup*tem1*eta(i,k-1)*.5*(hcko(i,k)+hcko(i,k-1))*dz
     &    +  adw*edto(i)*ptem1*etad(i,k)*.5*(hcdo(i,k)+hcdo(i,k-1))*dz
     &         ) *g/dp
cj
              dellaq(i,k) = dellaq(i,k) +
     &     ((aup*eta(i,k)-adw*edto(i)*etad(i,k))*dv1q
     &    - (aup*eta(i,k-1)-adw*edto(i)*etad(i,k-1))*dv3q
     &    - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2q*dz
     &    +  aup*tem1*eta(i,k-1)*.5*(qrcko(i,k)+qcko(i,k-1))*dz
     &    +  adw*edto(i)*ptem1*etad(i,k)*.5*(qrcdo(i,k)+qcdo(i,k-1))*dz
     &         ) *g/dp
cj
              dellau(i,k) = dellau(i,k) +
     &     ((aup*eta(i,k)-adw*edto(i)*etad(i,k))*dv1u
     &    - (aup*eta(i,k-1)-adw*edto(i)*etad(i,k-1))*dv3u
     &    - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2u*dz
     &    +  aup*tem1*eta(i,k-1)*.5*(ucko(i,k)+ucko(i,k-1))*dz
     &    +  adw*edto(i)*ptem1*etad(i,k)*.5*(ucdo(i,k)+ucdo(i,k-1))*dz
     &    -  pgcon*(aup*eta(i,k-1)-adw*edto(i)*etad(i,k))*(dv1u-dv3u)
     &         ) *g/dp
cj
              dellav(i,k) = dellav(i,k) +
     &     ((aup*eta(i,k)-adw*edto(i)*etad(i,k))*dv1v
     &    - (aup*eta(i,k-1)-adw*edto(i)*etad(i,k-1))*dv3v
     &    - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2v*dz
     &    +  aup*tem1*eta(i,k-1)*.5*(vcko(i,k)+vcko(i,k-1))*dz
     &    +  adw*edto(i)*ptem1*etad(i,k)*.5*(vcdo(i,k)+vcdo(i,k-1))*dz
     &    -  pgcon*(aup*eta(i,k-1)-adw*edto(i)*etad(i,k))*(dv1v-dv3v)
     &         ) *g/dp
cj
          endif
        enddo
      enddo
c
c------- cloud top
c
      do i = 1, im
        if(cnvflg(i)) then
          indx = ktcon(i)
          dp = 1000. * del(i,indx)
          dv1h = heo(i,indx-1)
          dellah(i,indx) = eta(i,indx-1) *
     &                     (hcko(i,indx-1) - dv1h) * g / dp
          dv1q = qo(i,indx-1)
          dellaq(i,indx) = eta(i,indx-1) *
     &                     (qcko(i,indx-1) - dv1q) * g / dp
          dv1u = uo(i,indx-1)
          dellau(i,indx) = eta(i,indx-1) *
     &                     (ucko(i,indx-1) - dv1u) * g / dp
          dv1v = vo(i,indx-1)
          dellav(i,indx) = eta(i,indx-1) *
     &                     (vcko(i,indx-1) - dv1v) * g / dp
c
c  cloud water
c
          dellal(i,indx) = eta(i,indx-1) *
     &                     qlko_ktcon(i) * g / dp
        endif
      enddo
c
c------- final changed variable per unit mass flux
c
!> - Calculate the change in the temperature and moisture profiles per unit cloud base mass flux.
      do k = 1, km
        do i = 1, im
          if (cnvflg(i).and.k .le. kmax(i)) then
            if(k.gt.ktcon(i)) then
              qo(i,k) = q1(i,k)
              to(i,k) = t1(i,k)
            endif
            if(k.le.ktcon(i)) then
              qo(i,k) = dellaq(i,k) * mbdt + q1(i,k)
              dellat = (dellah(i,k) - hvap * dellaq(i,k)) / cp
              to(i,k) = dellat * mbdt + t1(i,k)
              val   =           1.e-10
              qo(i,k) = max(qo(i,k), val  )
            endif
          endif
        enddo
      enddo
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c--- the above changed environment is now used to calulate the
c--- effect the arbitrary cloud (with unit mass flux)
c--- would have on the stability,
c--- which then is used to calculate the real mass flux,
c--- necessary to keep this change in balance with the large-scale
c--- destabilization.
c
c--- environmental conditions again, first heights
c
!> ## Using the updated temperature and moisture profiles that were modified by the convection on a short time-scale, recalculate the total cloud work function to determine the change in the cloud work function due to convection, or the stabilizing effect of the cumulus.
!! - Using notation from Pan and Wu (1995) \cite pan_and_wu_1995, the previously calculated cloud work function is denoted by \f$A^+\f$. Now, it is necessary to use the entraining/detraining cloud model ("static control") to determine the cloud work function of the environment after the stabilization of the arbitrary convective element (per unit cloud base mass flux) has been applied, denoted by \f$A^*\f$.
!! - Recalculate saturation specific humidity.
      do k = 1, km
        do i = 1, im
          if(cnvflg(i) .and. k .le. kmax(i)) then
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (pfld(i,k)+epsm1*qeso(i,k))
            val       =             1.e-8
            qeso(i,k) = max(qeso(i,k), val )
!           tvo(i,k)  = to(i,k) + delta * to(i,k) * qo(i,k)
          endif
        enddo
      enddo
c
c--- moist static energy
c
!! - Recalculate moist static energy and saturation moist static energy.
      do k = 1, km1
        do i = 1, im
          if(cnvflg(i) .and. k .le. kmax(i)-1) then
            dz = .5 * (zo(i,k+1) - zo(i,k))
            dp = .5 * (pfld(i,k+1) - pfld(i,k))
            es = 0.01 * fpvs(to(i,k+1))      ! fpvs is in pa
            pprime = pfld(i,k+1) + epsm1 * es
            qs = eps * es / pprime
            dqsdp = - qs / pprime
            desdt = es * (fact1 / to(i,k+1) + fact2 / (to(i,k+1)**2))
            dqsdt = qs * pfld(i,k+1) * desdt / (es * pprime)
            gamma = el2orc * qeso(i,k+1) / (to(i,k+1)**2)
            dt = (g * dz + hvap * dqsdp * dp) / (cp * (1. + gamma))
            dq = dqsdt * dt + dqsdp * dp
            to(i,k) = to(i,k+1) + dt
            qo(i,k) = qo(i,k+1) + dq
            po(i,k) = .5 * (pfld(i,k) + pfld(i,k+1))
          endif
        enddo
      enddo
      do k = 1, km1
        do i = 1, im
          if(cnvflg(i) .and. k .le. kmax(i)-1) then
            qeso(i,k) = 0.01 * fpvs(to(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (po(i,k) + epsm1 * qeso(i,k))
            val1      =             1.e-8
            qeso(i,k) = max(qeso(i,k), val1)
            val2      =           1.e-10
            qo(i,k)   = max(qo(i,k), val2 )
!           qo(i,k)   = min(qo(i,k),qeso(i,k))
            heo(i,k)   = .5 * g * (zo(i,k) + zo(i,k+1)) +
     &                    cp * to(i,k) + hvap * qo(i,k)
            heso(i,k) = .5 * g * (zo(i,k) + zo(i,k+1)) +
     &                  cp * to(i,k) + hvap * qeso(i,k)
          endif
        enddo
      enddo
      do i = 1, im
        if(cnvflg(i)) then
          k = kmax(i)
          heo(i,k) = g * zo(i,k) + cp * to(i,k) + hvap * qo(i,k)
          heso(i,k) = g * zo(i,k) + cp * to(i,k) + hvap * qeso(i,k)
c         heo(i,k) = min(heo(i,k),heso(i,k))
        endif
      enddo
c
c**************************** static control
c
c------- moisture and cloud work functions
c
!> - As before, recalculate the updraft cloud work function.
      do i = 1, im
        if(cnvflg(i)) then
          xaa0(i) = 0.
          xpwav(i) = 0.
        endif
      enddo
c
      do i = 1, im
        if(cnvflg(i)) then
          indx = kb(i)
          hcko(i,indx) = heo(i,indx)
          qcko(i,indx) = qo(i,indx)
        endif
      enddo
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k.gt.kb(i).and.k.le.ktcon(i)) then
              dz = zi(i,k) - zi(i,k-1)
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5 * xlamud(i) * dz
              factor = 1. + tem - tem1
              hcko(i,k) = ((1.-tem1)*hcko(i,k-1)+tem*0.5*
     &                     (heo(i,k)+heo(i,k-1)))/factor
            endif
          endif
        enddo
      enddo
      do k = 2, km1
        do i = 1, im
          if (cnvflg(i)) then
            if(k.gt.kb(i).and.k.lt.ktcon(i)) then
              dz = zi(i,k) - zi(i,k-1)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              xdby = hcko(i,k) - heso(i,k)
              xqrch = qeso(i,k)
     &              + gamma * xdby / (hvap * (1. + gamma))
cj
              tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
              tem1 = 0.5 * xlamud(i) * dz
              factor = 1. + tem - tem1
              qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*
     &                     (qo(i,k)+qo(i,k-1)))/factor
cj
              dq = eta(i,k) * (qcko(i,k) - xqrch)
c
              if(k.ge.kbcon(i).and.dq.gt.0.) then
                etah = .5 * (eta(i,k) + eta(i,k-1))
                if(ncloud.gt.0..and.k.gt.jmin(i)) then
                  qlk = dq / (eta(i,k) + etah * (c0 + c1) * dz)
                else
                  qlk = dq / (eta(i,k) + etah * c0 * dz)
                endif
                if(k.lt.ktcon1(i)) then
                  xaa0(i) = xaa0(i) - dz * g * qlk
                endif
                qcko(i,k) = qlk + xqrch
                xpw = etah * c0 * dz * qlk
                xpwav(i) = xpwav(i) + xpw
              endif
            endif
            if(k.ge.kbcon(i).and.k.lt.ktcon1(i)) then
              dz1 = zo(i,k+1) - zo(i,k)
              gamma = el2orc * qeso(i,k) / (to(i,k)**2)
              rfact =  1. + delta * cp * gamma
     &                 * to(i,k) / hvap
              xaa0(i) = xaa0(i)
     &                + dz1 * (g / (cp * to(i,k)))
     &                * xdby / (1. + gamma)
     &                * rfact
              val=0.
              xaa0(i)=xaa0(i)+
     &                 dz1 * g * delta *
     &                 max(val,(qeso(i,k) - qo(i,k)))
            endif
          endif
        enddo
      enddo
c
c------- downdraft calculations
c
c--- downdraft moisture properties
c
!> - As before, recalculate the downdraft cloud work function.
      do i = 1, im
        if(cnvflg(i)) then
          jmn = jmin(i)
          hcdo(i,jmn) = heo(i,jmn)
          qcdo(i,jmn) = qo(i,jmn)
          qrcd(i,jmn) = qo(i,jmn)
          xpwev(i) = 0.
        endif
      enddo
cj
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i) .and. k.lt.jmin(i)) then
              dz = zi(i,k+1) - zi(i,k)
              if(k.ge.kbcon(i)) then
                 tem  = xlamde * dz
                 tem1 = 0.5 * xlamdd * dz
              else
                 tem  = xlamde * dz
                 tem1 = 0.5 * (xlamd(i)+xlamdd) * dz
              endif
              factor = 1. + tem - tem1
              hcdo(i,k) = ((1.-tem1)*hcdo(i,k+1)+tem*0.5*
     &                     (heo(i,k)+heo(i,k+1)))/factor
          endif
        enddo
      enddo
cj
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i) .and. k .lt. jmin(i)) then
              dq = qeso(i,k)
              dt = to(i,k)
              gamma    = el2orc * dq / dt**2
              dh       = hcdo(i,k) - heso(i,k)
              qrcd(i,k)=dq+(1./hvap)*(gamma/(1.+gamma))*dh
!             detad    = etad(i,k+1) - etad(i,k)
cj
              dz = zi(i,k+1) - zi(i,k)
              if(k.ge.kbcon(i)) then
                 tem  = xlamde * dz
                 tem1 = 0.5 * xlamdd * dz
              else
                 tem  = xlamde * dz
                 tem1 = 0.5 * (xlamd(i)+xlamdd) * dz
              endif
              factor = 1. + tem - tem1
              qcdo(i,k) = ((1.-tem1)*qrcd(i,k+1)+tem*0.5*
     &                     (qo(i,k)+qo(i,k+1)))/factor
cj
!             xpwd     = etad(i,k+1) * qcdo(i,k+1) -
!    &                   etad(i,k) * qrcd(i,k)
!             xpwd     = xpwd - detad *
!    &                 .5 * (qrcd(i,k) + qrcd(i,k+1))
cj
              xpwd     = etad(i,k) * (qcdo(i,k) - qrcd(i,k))
              xpwev(i) = xpwev(i) + xpwd
          endif
        enddo
      enddo
c
      do i = 1, im
        edtmax = edtmaxl
        if(islimsk(i) == 0) edtmax = edtmaxs
        if(cnvflg(i)) then
          if(xpwev(i).ge.0.) then
            edtx(i) = 0.
          else
            edtx(i) = -edtx(i) * xpwav(i) / xpwev(i)
            edtx(i) = min(edtx(i),edtmax)
          endif
        endif
      enddo
c
c
c--- downdraft cloudwork functions
c
c
      do k = km1, 1, -1
        do i = 1, im
          if (cnvflg(i) .and. k.lt.jmin(i)) then
              gamma = el2orc * qeso(i,k) / to(i,k)**2
              dhh=hcdo(i,k)
              dt= to(i,k)
              dg= gamma
              dh= heso(i,k)
              dz=-1.*(zo(i,k+1)-zo(i,k))
              xaa0(i)=xaa0(i)+edtx(i)*dz*(g/(cp*dt))*((dhh-dh)/(1.+dg))
     &                *(1.+delta*cp*dg*dt/hvap)
              val=0.
              xaa0(i)=xaa0(i)+edtx(i)*
     &        dz*g*delta*max(val,(qeso(i,k)-qo(i,k)))
          endif
        enddo
      enddo
c
c  calculate critical cloud work function
c
!> ## For the "dynamic control", using a reference cloud work function, estimate the change in cloud work function due to the large-scale dynamics. Following the quasi-equilibrium assumption, calculate the cloud base mass flux required to keep the large-scale convective destabilization in balance with the stabilization effect of the convection.
!! - Calculate the reference, or "critical", cloud work function derived from observations, denoted by \f$A^0\f$.
      do i = 1, im
        if(cnvflg(i)) then
          if(pfld(i,ktcon(i)).lt.pcrit(15))then
            acrt(i)=acrit(15)*(975.-pfld(i,ktcon(i)))
     &              /(975.-pcrit(15))
          else if(pfld(i,ktcon(i)).gt.pcrit(1))then
            acrt(i)=acrit(1)
          else
            k =  int((850. - pfld(i,ktcon(i)))/50.) + 2
            k = min(k,15)
            k = max(k,2)
            acrt(i)=acrit(k)+(acrit(k-1)-acrit(k))*
     &           (pfld(i,ktcon(i))-pcrit(k))/(pcrit(k-1)-pcrit(k))
          endif
        endif
      enddo
!> - Calculate a correction factor, "acrtfct", that is a function of the cloud base vertical velocity, to multiply the critical cloud work function.
      do i = 1, im
        if(cnvflg(i)) then
          if(islimsk(i) == 1) then
            w1 = w1l
            w2 = w2l
            w3 = w3l
            w4 = w4l
          else
            w1 = w1s
            w2 = w2s
            w3 = w3s
            w4 = w4s
          endif
c
c  modify critical cloud workfunction by cloud base vertical velocity
c
          if(pdot(i).le.w4) then
            acrtfct(i) = (pdot(i) - w4) / (w3 - w4)
          elseif(pdot(i).ge.-w4) then
            acrtfct(i) = - (pdot(i) + w4) / (w4 - w3)
          else
            acrtfct(i) = 0.
          endif
          val1    =             -1.
          acrtfct(i) = max(acrtfct(i),val1)
          val2    =             1.
          acrtfct(i) = min(acrtfct(i),val2)
          acrtfct(i) = 1. - acrtfct(i)
c
c  modify acrtfct(i) by colume mean rh if rhbar(i) is greater than 80 percent
c
c         if(rhbar(i).ge..8) then
c           acrtfct(i) = acrtfct(i) * (.9 - min(rhbar(i),.9)) * 10.
c         endif
c
c  modify adjustment time scale by cloud base vertical velocity
c
!> - Also, modify the time scale over which the large-scale destabilization takes place (dtconv) according to the cloud base vertical velocity, ensuring that this timescale stays between previously calculated minimum and maximum values.
          dtconv(i) = dt2 + max((1800. - dt2),0.) *
     &                (pdot(i) - w2) / (w1 - w2)
c         dtconv(i) = max(dtconv(i), dt2)
c         dtconv(i) = 1800. * (pdot(i) - w2) / (w1 - w2)
          dtconv(i) = max(dtconv(i),dtmin)
          dtconv(i) = min(dtconv(i),dtmax)
c
        endif
      enddo
c
c--- large scale forcing
c
!> - Calculate the large scale destabilization as in equation 5 of Pan and Wu (1995) \cite pan_and_wu_1995 :
!! \f[
!!  \frac{\partial A}{\partial t}_{LS}=\frac{A^+-cA^0}{\Delta t_{LS}}
!! \f]
!! where \f$c\f$ is the correction factor "acrtfct", \f$\Delta t_{LS}\f$ is the modified timescale over which the environment is destabilized, and the other quantities have been previously defined.
      do i= 1, im
        if(cnvflg(i)) then
          fld(i)=(aa1(i)-acrt(i)* acrtfct(i))/dtconv(i)
          if(fld(i).le.0.) cnvflg(i) = .false.
        endif
!> - Calculate the stabilization effect of the convection (per unit cloud base mass flux) as in equation 6 of Pan and Wu (1995) \cite pan_and_wu_1995 :
!! \f[
!! \frac{\partial A}{\partial t}_{cu}=\frac{A^*-A^+}{\Delta t_{cu}}
!! \f]
!! \f$\Delta t_{cu}\f$ is the short timescale of the convection.
        if(cnvflg(i)) then
c         xaa0(i) = max(xaa0(i),0.)
          xk(i) = (xaa0(i) - aa1(i)) / mbdt
          if(xk(i).ge.0.) cnvflg(i) = .false.
        endif
c
c--- kernel, cloud base mass flux
c
!> - The cloud base mass flux (xmb) is then calculated from equation 7 of Pan and Wu (1995) \cite pan_and_wu_1995
!! \f[
!! M_c=\frac{-\frac{\partial A}{\partial t}_{LS}}{\frac{\partial A}{\partial t}_{cu}}
!! \f]
        if(cnvflg(i)) then
          xmb(i) = -fld(i) / xk(i)
          xmb(i) = min(xmb(i),xmbmax(i))
        endif
      enddo
!!
!> - If the large scale destabilization is less than zero, or the stabilization by the convection is greater than zero, then the scheme returns to the calling routine without modifying the state variables.
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
c
c  restore to,qo,uo,vo to t1,q1,u1,v1 in case convection stops
c

      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. k .le. kmax(i)) then
            to(i,k) = t1(i,k)
            qo(i,k) = q1(i,k)
            uo(i,k) = u1(i,k)
            vo(i,k) = v1(i,k)
            qeso(i,k) = 0.01 * fpvs(t1(i,k))      ! fpvs is in pa
            qeso(i,k) = eps * qeso(i,k) / (pfld(i,k) + epsm1*qeso(i,k))
            val     =             1.e-8
            qeso(i,k) = max(qeso(i,k), val )
          endif
        enddo
      enddo
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c--- feedback: simply the changes from the cloud with unit mass flux
c---           multiplied by  the mass flux necessary to keep the
c---           equilibrium with the larger-scale.
c
!> ## For the "feedback" control, calculate updated values of the state variables by multiplying the cloud base mass flux and the tendencies calculated per unit cloud base mass flux from the static control.
!> - Calculate the temperature tendency from the moist static energy and specific humidity tendencies.
!> - Update the temperature, specific humidity, and horiztonal wind state variables by multiplying the cloud base mass flux-normalized tendencies by the cloud base mass flux.
!> - Accumulate column-integrated tendencies.
      do i = 1, im
        delhbar(i) = 0.
        delqbar(i) = 0.
        deltbar(i) = 0.
        delubar(i) = 0.
        delvbar(i) = 0.
        qcond(i) = 0.
      enddo
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. k .le. kmax(i)) then
            if(k.le.ktcon(i)) then
              dellat = (dellah(i,k) - hvap * dellaq(i,k)) / cp
              t1(i,k) = t1(i,k) + dellat * xmb(i) * dt2
              q1(i,k) = q1(i,k) + dellaq(i,k) * xmb(i) * dt2
!             tem = 1./rcs(i)
!             u1(i,k) = u1(i,k) + dellau(i,k) * xmb(i) * dt2 * tem
!             v1(i,k) = v1(i,k) + dellav(i,k) * xmb(i) * dt2 * tem
              u1(i,k) = u1(i,k) + dellau(i,k) * xmb(i) * dt2
              v1(i,k) = v1(i,k) + dellav(i,k) * xmb(i) * dt2
              dp = 1000. * del(i,k)
              delhbar(i) = delhbar(i) + dellah(i,k)*xmb(i)*dp/g
              delqbar(i) = delqbar(i) + dellaq(i,k)*xmb(i)*dp/g
              deltbar(i) = deltbar(i) + dellat*xmb(i)*dp/g
              delubar(i) = delubar(i) + dellau(i,k)*xmb(i)*dp/g
              delvbar(i) = delvbar(i) + dellav(i,k)*xmb(i)*dp/g
            endif
          endif
        enddo
      enddo
!> - Recalculate saturation specific humidity using the updated temperature.
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. k .le. kmax(i)) then
            if(k.le.ktcon(i)) then
              qeso(i,k) = 0.01 * fpvs(t1(i,k))      ! fpvs is in pa
              qeso(i,k) = eps * qeso(i,k)/(pfld(i,k) + epsm1*qeso(i,k))
              val     =             1.e-8
              qeso(i,k) = max(qeso(i,k), val )
            endif
          endif
        enddo
      enddo
c
!> - Add up column-integrated convective precipitation by multiplying the normalized value by the cloud base mass flux.
      do i = 1, im
        rntot(i) = 0.
        delqev(i) = 0.
        delq2(i) = 0.
        flg(i) = cnvflg(i)
      enddo
      do k = km, 1, -1
        do i = 1, im
          if (cnvflg(i) .and. k .le. kmax(i)) then
            if(k.lt.ktcon(i)) then
              aup = 1.
              if(k.le.kb(i)) aup = 0.
              adw = 1.
              if(k.ge.jmin(i)) adw = 0.
              rain =  aup * pwo(i,k) + adw * edto(i) * pwdo(i,k)
              rntot(i) = rntot(i) + rain * xmb(i) * .001 * dt2
            endif
          endif
        enddo
      enddo
!> - Determine the evaporation of the convective precipitation and update the integrated convective precipitation.
!> - Update state temperature and moisture to account for evaporation of convective precipitation.
!> - Update column-integrated tendencies to account for evaporation of convective precipitation.
      do k = km, 1, -1
        do i = 1, im
          if (k .le. kmax(i)) then
            deltv(i) = 0.
            delq(i) = 0.
            qevap(i) = 0.
            if(cnvflg(i).and.k.lt.ktcon(i)) then
              aup = 1.
              if(k.le.kb(i)) aup = 0.
              adw = 1.
              if(k.ge.jmin(i)) adw = 0.
              rain =  aup * pwo(i,k) + adw * edto(i) * pwdo(i,k)
              rn(i) = rn(i) + rain * xmb(i) * .001 * dt2
            endif
            if(flg(i).and.k.lt.ktcon(i)) then
              evef = edt(i) * evfact
              if(islimsk(i) == 1) evef=edt(i) * evfactl
!             if(islimsk(i) == 1) evef=.07
c             if(islimsk(i) == 1) evef = 0.
              qcond(i) = evef * (q1(i,k) - qeso(i,k))
     &                 / (1. + el2orc * qeso(i,k) / t1(i,k)**2)
              dp = 1000. * del(i,k)
              if(rn(i).gt.0..and.qcond(i).lt.0.) then
                qevap(i) = -qcond(i) * (1.-exp(-.32*sqrt(dt2*rn(i))))
                qevap(i) = min(qevap(i), rn(i)*1000.*g/dp)
                delq2(i) = delqev(i) + .001 * qevap(i) * dp / g
              endif
              if(rn(i).gt.0..and.qcond(i).lt.0..and.
     &           delq2(i).gt.rntot(i)) then
                qevap(i) = 1000.* g * (rntot(i) - delqev(i)) / dp
                flg(i) = .false.
              endif
              if(rn(i).gt.0..and.qevap(i).gt.0.) then
                q1(i,k) = q1(i,k) + qevap(i)
                t1(i,k) = t1(i,k) - elocp * qevap(i)
                rn(i) = rn(i) - .001 * qevap(i) * dp / g
                deltv(i) = - elocp*qevap(i)/dt2
                delq(i) =  + qevap(i)/dt2
                delqev(i) = delqev(i) + .001*dp*qevap(i)/g
              endif
              dellaq(i,k) = dellaq(i,k) + delq(i) / xmb(i)
              delqbar(i) = delqbar(i) + delq(i)*dp/g
              deltbar(i) = deltbar(i) + deltv(i)*dp/g
            endif
          endif
        enddo
      enddo
cj
!     do i = 1, im
!     if(me.eq.31.and.cnvflg(i)) then
!     if(cnvflg(i)) then
!       print *, ' deep delhbar, delqbar, deltbar = ',
!    &             delhbar(i),hvap*delqbar(i),cp*deltbar(i)
!       print *, ' deep delubar, delvbar = ',delubar(i),delvbar(i)
!       print *, ' precip =', hvap*rn(i)*1000./dt2
!       print*,'pdif= ',pfld(i,kbcon(i))-pfld(i,ktcon(i))
!     endif
!     enddo
c
c  precipitation rate converted to actual precip
c  in unit of m instead of kg
c
      do i = 1, im
        if(cnvflg(i)) then
c
c  in the event of upper level rain evaporation and lower level downdraft
c    moistening, rn can become negative, in this case, we back out of the
c    heating and the moistening
c

          if(rn(i).lt.0..and..not.flg(i)) rn(i) = 0.
          if(rn(i).le.0.) then
            rn(i) = 0.
          else
            ktop(i) = ktcon(i)
            kbot(i) = kbcon(i)
            kcnv(i) = 1
            cldwrk(i) = aa1(i)
          endif
        endif
      enddo
c
c  convective cloud water
c
!> - Calculate convective cloud water.
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. rn(i).gt.0.) then
            if (k.ge.kbcon(i).and.k.lt.ktcon(i)) then
              cnvw(i,k) = cnvwt(i,k) * xmb(i) * dt2
            endif
          endif
        enddo
      enddo
c
c  convective cloud cover
c
!> - Calculate convective cloud cover.
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. rn(i).gt.0.) then
            if (k.ge.kbcon(i).and.k.lt.ktcon(i)) then
              cnvc(i,k) = 0.04 * log(1. + 675. * eta(i,k) * xmb(i))
              cnvc(i,k) = min(cnvc(i,k), 0.6)
              cnvc(i,k) = max(cnvc(i,k), 0.0)
            endif
          endif
        enddo
      enddo

c
c  cloud water
c
!> - Separate detrained cloud water into liquid and ice species as a function of temperature only.
      if (ncloud.gt.0) then
!
      do k = 1, km
        do i = 1, im
          if (cnvflg(i) .and. rn(i).gt.0.) then
            if (k.gt.kb(i).and.k.le.ktcon(i)) then
              tem  = dellal(i,k) * xmb(i) * dt2
              tem1 = max(0.0, min(1.0, (tcr-t1(i,k))*tcrf))
              if (ql(i,k,2) .gt. -999.0) then
                ql(i,k,1) = ql(i,k,1) + tem * tem1            ! ice
                ql(i,k,2) = ql(i,k,2) + tem *(1.0-tem1)       ! water
              else
                ql(i,k,1) = ql(i,k,1) + tem
              endif
            endif
          endif
        enddo
      enddo
!
      endif
c
!> - If convective precipitation is zero or negative, reset the updated state variables back to their original values (negating convective changes).
      do k = 1, km
        do i = 1, im
          if(cnvflg(i).and.rn(i).le.0.) then
            if (k .le. kmax(i)) then
              t1(i,k) = to(i,k)
              q1(i,k) = qo(i,k)
              u1(i,k) = uo(i,k)
              v1(i,k) = vo(i,k)
            endif
          endif
        enddo
      enddo
!
! hchuang code change
!
!> - Calculate the updraft convective mass flux.
      do k = 1, km
        do i = 1, im
          if(cnvflg(i).and.rn(i).gt.0.) then
            if(k.ge.kb(i) .and. k.lt.ktop(i)) then
              ud_mf(i,k) = eta(i,k) * xmb(i) * dt2
            endif
          endif
        enddo
      enddo
!> - Calculate the detrainment mass flux at cloud top.
      do i = 1, im
        if(cnvflg(i).and.rn(i).gt.0.) then
           k = ktop(i)-1
           dt_mf(i,k) = ud_mf(i,k)
        endif
      enddo
!> - Calculate the downdraft convective mass flux.
      do k = 1, km
        do i = 1, im
          if(cnvflg(i).and.rn(i).gt.0.) then
            if(k.ge.1 .and. k.le.jmin(i)) then
              dd_mf(i,k) = edto(i) * etad(i,k) * xmb(i) * dt2
            endif
          endif
        enddo
      enddo
!!
      return
!> @}
!! @}
      end
!  \section original Original Documentation
!  Penetrative convection is simulated following Pan and Wu (1994), which is based on Arakawa and Schubert(1974) as simplified by Grell (1993) and with a saturated downdraft. Convection occurs when the cloud work function (CWF) exceeds a certain threshold. Mass flux of the cloud is determined using a quasi-equilibrium assumption based on this threshold CWF. The CWF is a function of temperature and moisture in each air column of the model gridpoint. The temperature and moisture profiles are adjusted towards the equilibrium CWF within a specified time scale using the deduced mass flux. A major simplification of the original Arakawa-Shubert scheme is to consider only the deepest cloud and not the spectrum of clouds. The cloud model incorporates a downdraft mechanism as well as the evaporation of precipitation. Entrainment of the updraft and detrainment of the downdraft in the sub-cloud layers are included. Downdraft strength is based on the vertical wind shear through the cloud. The critical CWF is a function of the cloud base vertical motion. As the large-scale rising motion becomes strong, the CWF [similar to convective available potential energy (CAPE)] is allowed to approach zero (therefore approaching neutral stability).
!
!  Mass fluxes induced in the updraft and the downdraft are allowed to transport momentum. The momentum exchange is calculated through the mass flux formulation in a manner similar to that for heat and moisture. The effect of the convection-induced pressure gradient force on cumulus momentum transport is parameterized in terms of mass flux and vertical wind shear (Han and Pan, 2006). As a result, the cumulus momentum exchange is reduced by about 55 % compared to the full exchange.
!
!  The entrainment rate in cloud layers is dependent upon environmental humidity (Han and Pan, 2010). A drier environment increases the entrainment, suppressing the convection. The entrainment rate in sub-cloud layers is given as inversely proportional to height. The detrainment rate is assumed to be a constant in all layers and equal to the entrainment rate value at cloud base, which is O(10-4). The liquid water in the updraft layer is assumed to be detrained from the layers above the level of the minimum moist static energy into the grid-scale cloud water with conversion parameter of 0.002 m-1, which is same as the rain conversion parameter.
!
!  Following Han and Pan (2010), the trigger condition is that a parcel lifted from the convection starting level without entrainment must reach its level of free convection within 120-180 hPa of ascent, proportional to the large-scale vertical velocity. This is intended to produce more convection in large-scale convergent regions but less convection in large-scale subsidence regions. Another important trigger mechanism is to include the effect of environmental humidity in the sub-cloud layer, taking into account convection inhibition due to existence of dry layers below cloud base. On the other hand, the cloud parcel might overshoot beyond the level of neutral buoyancy due to its inertia, eventually stopping its overshoot at cloud top. The CWF is used to model the overshoot. The overshoot of the cloud top is stopped at the height where a parcel lifted from the neutral buoyancy level with energy equal to 10% of the CWF would first have zero energy.
!
!  Deep convection parameterization (SAS) modifications include:
!  - Detraining cloud water from every updraft layer
!  - Starting convection from the level of maximum moist static energy within PBL
!  - Random cloud top is eliminated and only deepest cloud is considered
!  - Cloud water is detrained from every cloud layer
!  - Finite entrainment and detrainment rates for heat, moisture, and momentum are specified
!  - Similar to shallow convection scheme,
!  - entrainment rate is given to be inversely proportional to height in sub-cloud layers
!  - detrainment rate is set to be a constant as entrainment rate at the cloud base.
!  -Above cloud base, an organized entrainment is added, which is a function of environmental relative humidity
