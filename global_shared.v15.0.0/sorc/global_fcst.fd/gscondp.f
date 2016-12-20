      subroutine gscondp (im,ix,km,dt,dtf,prsl,ps,q,cwm,t
     &,                  tp, qp, psp, tp1, qp1, psp1 
     &,			 u,deltaq, sup, lprnt, ipr, kdt)
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
!     *  modified by:  s. moorthi       aug. 1999, 2000                *
!     *                                                                *
!     *  references:                                                   *
!     *                                                                *
!     ******************************************************************
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!
      use machine , only : kind_phys
      use funcphys , only : fpvs
      use physcons, psat => con_psat, hvap => con_hvap, grav => con_g
     &,             hfus => con_hfus, ttp => con_ttp, rd => con_rd
     &,             cp => con_cp, eps => con_eps, epsm1 => con_epsm1
     &,             rv => con_rv, thgni => con_thgni
!      use namelist_def, only: nsdfi,fhdfi

      implicit none
!
      real (kind=kind_phys) g,    h1,   h2, h1000
     &,                     d00,  d125,  d5,   elwv, eliv
     &,                     epsq, tm10,  eliw, arcp
     &,                     a1,   r,     cpr,  rcpr, rcp
      parameter (h1=1.e0,       h2=2.e0,               h1000=1000.0
     &,          d00=0.e0,      d125=.125e0,           d5=0.5e0
     &,          a1=psat  
     &,          elwv=hvap,     eliv=hvap+hfus, g=grav
     &,          epsq=2.e-12,   tm10=ttp-10.,   r=rd
     &,          cpr=cp*r,      rcpr=h1/cpr,    rcp=h1/cp)
!
      real(kind=kind_phys), parameter :: cons_0=0.0, cons_m15=-15.0
!
      integer im, ix, km, ipr
      real (kind=kind_phys) q(ix,km),    t(ix,km),    cwm(ix,km)
     &,                     prsl(ix,km), ps(im), dt,  dtf
     &,                     tp(ix,km),   qp(ix,km),   psp(im)
     &,                     tp1(ix,km),  qp1(ix,km),  psp1(im)
     &,                     deltaq(ix,km),deltaqik
       real (kind=kind_phys) qtmp,qwtmp,qtpd,qsc,usc
       real (kind=kind_phys), intent(in) ::  sup
       logical iice

!
      real (kind=kind_phys)  qi(im), qint(im), u(im,km), ccrik, e0
     &,                      cond,   rdt, us, cclimit, climit
     &,                      u00b,   u00t, tmt0, tmt15, qik, cwmik
     &,                 ai, bi, qw(im,km), u00ik, tik, pres, pp0, fi 
     &,                      at, aq, ap, fiw, elv, qc, rqik
     &,                      rqikk, tx1, tx2, tx3, es, qs
     &,                      tsq, delq, condi, cone0, us00, ccrik1
     &,                      aa, ab, ac, ad, ae, af, ag
     &,                      el2orc, albycp, vprs(im)
      integer iw(im,km), i, k, iwik, kdt 
      logical lprnt
!
!-----------------prepare constants for later uses-----------------
!
      
      el2orc = hvap*hvap / (rv*cp)
      albycp = hvap / cp
!
      rdt     = h1/dt
      us      = h1
      cclimit = 1.0e-3
      climit  = 1.0e-20
!
      do  i = 1, im
        iw(i,km) = d00
      enddo
!  the global qsat computation is done in cb
      do i = 1, im
      do k = 1, km
          pres  = prsl(i,k)
          qwtmp = min(pres, fpvs(t(i,k)))
          qw(i,k) = eps * qwtmp / (pres + epsm1 * qwtmp)
          qw(i,k) = max(qw(i,k),epsq)
      enddo
      enddo
!      if (tp(1,1) .lt. 1.) then
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
!        do k = 1, km
!          do i = 1, im
!            deltaq(i,k) = (1-u(i,k))*qw(i,k)
!          enddo
!        enddo
!      endif
      if(kdt == 1) then 
        do k = 1, km
          do i = 1, im
            deltaq(i,k) = (1-u(i,k))*qw(i,k)
          enddo
        enddo
      endif 
c
c*************************************************************
c*******begining of  grid-scale condensation/evap. loop*******
c*************************************************************
c
!     do k = km-1,2,-1
      do k = km,1,-1
!       vprs(:) = fpvs(t(:,k))       ! fpvs in pa
c-----------------------------------------------------------------------
c------------------qw, qi and qint--------------------------------------
        do i = 1, im                                    
          tmt0  = t(i,k)-273.16
          tmt15 = min(tmt0,cons_m15)
          qik   = max(q(i,k),epsq)
          cwmik = max(cwm(i,k),climit)
          deltaqik = deltaq(i,k)
          qi(i)   = qw(i,k)
          qint(i) = qw(i,k)
!         if (tmt0 .le. -40.) qint(i) = qi(i)
c-------------------ice-water id number iw------------------------------
          if(tmt0 < -15.0) then
            u00ik = u(i,k)
            fi = qik + deltaqik -qi(i)
            if(fi > d00.or.cwmik > climit) then
               iw(i,k) = 1
            else
              iw(i,k) = 0
            end if
          end if
c
          if(tmt0 >= 0.0) then
            iw(i,k) = 0
          end if
c
          if (tmt0 < 0.0 .and. tmt0 >= -15.0) then
            iw(i,k) = 0
            if (k < km) then
            if (iw(i,k+1) == 1 .and. cwmik > climit) iw(i,k) = 1
            endif
          end if
        enddo
c--------------condensation and evaporation of cloud--------------------
        do i = 1, im
c------------------------at, aq and dp/dt-------------------------------
          qik   = max(q(i,k),epsq)
          cwmik = max(cwm(i,k),climit)
          iwik  = iw(i,k)
          u00ik = u(i,k)
          iice = .false.
          deltaqik = deltaq(i,k)
          tik   = t(i,k)
          pres  = prsl(i,k) 
          pp0   = (pres / ps(i)) * psp(i)
          at    = (tik-tp(i,k)) * rdt
          aq    = (qik-qp(i,k)) * rdt
          ap    = (pres-pp0)    * rdt
          if(tik - 273.16 < thgni) then  ! has to be consistent with radiation
            iice = .true.
          endif
c----------------the satuation specific humidity------------------------
          fiw   = float(iwik)
          elv   = (h1-fiw)*elwv    + fiw*eliv
          qc    = (h1-fiw)*qint(i) + fiw*qi(i)
!     if (lprnt) print *,' qc=',qc,' qint=',qint(i),' qi=',qi(i)
c----------------the relative humidity----------------------------------
          if(qc <= 1.0e-10) then
            rqik=d00 
          else
            rqik = qik/qc
          endif
          if(iice) then
            qsc = qc * sup
            usc = sup
          else
            qsc = qc
            usc = us
          endif
c----------------cloud cover ratio ccrik--------------------------------
           if(rqik >= usc) then
              ccrik = us 
           else
              qtmp = qik + cwmik -qsc
              if(deltaqik > epsq) then
                if(qtmp <= -deltaqik) then
                  ccrik = d00
                elseif(qtmp >= deltaqik) then
                  ccrik = us
                else
                  ccrik = 0.5 * qtmp / deltaqik + 0.5
                  ccrik = max(ccrik, 0.)
                  ccrik = min(ccrik, 1.)
                endif
              else
                if(qtmp > 0.) then
                  ccrik = us
                else
                  ccrik = d00
                endif
              endif
           endif
c-----------correct ccr if it is too small in large cwm regions--------
c         if(ccrik.ge.0.01.and.ccrik.le.0.2.and
c    &          .cwmik.ge.0.2e-3) then
c          ccrik=min(1.0,cwmik*1.0e3)
c         end if
c----------------------------------------------------------------------
!   if no cloud exists then evaporate any existing cloud condensate
c----------------evaporation of cloud water-----------------------------
          e0 = d00
          if (ccrik <= cclimit.and.cwmik > climit) then 
!
!   first iteration - increment halved
!
            tx1 = tik
            tx3 = qik

            es   = min(pres, fpvs(tx1))
            qs   = eps * es / (pres + epsm1*es) 
            if(qs < cons_0) then 
              print*, 'warning : qs - deltaqik < 0.0'
              print*, 'qs, deltaqik:',qs,deltaqik
            endif 
            tsq  = tx1 * tx1
            delq = 0.5*(qs-deltaqik-tx3)*tsq / (tsq + el2orc*qs)
!
            tx2   = delq
            tx1   = tx1 - delq * albycp
            tx3   = tx3 + delq
!
!   second iteration
!
            es   = min(pres, fpvs(tx1))
            qs   = eps * es / (pres + epsm1*es) 
            if(qs < cons_0) then 
              print*, 'warning : qs - deltaqik < 0.0'
              print*, 'qs, deltaqik:',qs,deltaqik
            endif 
            tsq  = tx1 * tx1
            delq = (qs-deltaqik-tx3)*tsq / (tsq + el2orc * qs)
!
            tx2  = tx2 + delq
            tx1  = tx1 - delq * albycp
            tx3  = tx3 + delq
!
!   third iteration
!
            es   = min(pres, fpvs(tx1))
            qs   = eps * es / (pres + epsm1*es)
            if(qs < cons_0) then 
              print*, 'warning : qs - deltaqik < 0.0'
              print*, 'qs, deltaqik:',qs,deltaqik
            endif 
            tsq  = tx1 * tx1
            delq = (qs-deltaqik-tx3)*tsq / (tsq + el2orc * qs)

            tx2 = tx2 + delq

            e0 = max(tx2*rdt, cons_0)
            e0 = min(cwmik*rdt,   e0)
            e0 = max(cons_0,e0)
          end if
!   if cloud cover > 0.2 condense water vapor in to cloud condensate
c-----------the eqs. for cond. has been reorganized to reduce cpu------
          cond = d00
!         if (ccrik .gt. 0.20 .and. qc .gt. epsq) then
          if (ccrik > cclimit .and. qc > epsq) then
             us00   = deltaqik / qc
             ccrik1 = 1.0 - ccrik
             aa     = eps*elv*pres*qik
             ab     = ccrik*ccrik1*deltaqik
             ac     = ab + 0.5*cwmik+ 0.5 * ccrik * (qc-qsc)
             ad     = ab * ccrik1 + 0.5*ccrik*ccrik1*qc*(us-usc)
             ae     = cpr*tik*tik
             af     = ae * pres
             ag     = aa * elv
             ai     = cp * aa
             cond   = (ac-ad)*(af*aq-ai*at+ae*qik*ap)/(ac*(af+ag))
c-----------check & correct if over condensation occurs-----------------
             condi  = (qik   -u00ik   *qc*1.0)*rdt
             cond   = min(cond, condi)
c----------check & correct if supersatuation is too high----------------
c             qtemp=qik-max(0.,(cond-e0))*dt
c             if(qc.le.1.0e-10) then
c               rqtmp=0.0
c             else
c               rqtmp=qtemp/qc
c             end if
c             if(rqtmp.ge.1.10) then
c               cond=(qik-1.10*qc)*rdt
c             end if
c-----------------------------------------------------------------------
             cond = max(cond, d00)
c-------------------update of t, q and cwm------------------------------
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
!****************end of the condensation/evaporation loop*************
!*********************************************************************
!
!----------------store t, q, ps for next time step

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
          do i = 1, im
            tp(i,k)  = t(i,k)
            qp(i,k)  = max(q(i,k),epsq)
            qp(i,k)  = q(i,k)
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
