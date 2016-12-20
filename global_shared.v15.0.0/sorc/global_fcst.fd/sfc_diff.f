      subroutine sfc_diff(im,ps,u1,v1,t1,q1,z1,
     &                    tskin,z0rl,cm,ch,rb,
     &                    prsl1,prslki,islimsk,
     &                    stress,fm,fh,
clu_q2m_iter [-1l/+2l]: add tsurf, flag_iter
!*   &                    ustar,wind,ddvel,fm10,fh2)
     +                    ustar,wind,ddvel,fm10,fh2,
     +                    sigmaf,vegtype,shdmax,
     +                    tsurf,flag_iter,redrag)
!
      use machine , only : kind_phys
      use funcphys, only : fpvs    
      use physcons, grav => con_g, sbc => con_sbc 
     &,             cp => con_cp, hfus => con_hfus 
     &,             rvrdm1 => con_fvirt, rd => con_rd
     &,             eps => con_eps, epsm1 => con_epsm1

      implicit none
!
      integer              im
!
      real(kind=kind_phys) ps(im),       u1(im),      v1(im),
     &                     t1(im),       q1(im),      z1(im),      
     &                     tskin(im),    z0rl(im),
     &                     cm(im),       ch(im),      rb(im),
     &                                   prsl1(im),   prslki(im),
     &                     stress(im), 
     &                     fm(im),       fh(im),      ustar(im),
     &                     wind(im),     ddvel(im),    
     &                     fm10(im),     fh2(im),     z1i(im),
     &                     sigmaf(im),   shdmax(im)   
      integer vegtype(im),islimsk(im)

clu_q2m_iter [+1l]: add flag_iter
      logical              flag_iter(im)
! flag for reduced drag coeff. over sea in high wind condition (j.han)
      logical              redrag
!
!     locals
!
      integer              i
!
      real(kind=kind_phys) dtv(im),     hl1(im),     hl12(im),
     &                     hlinf(im),   ph(im),
     &                     ph2(im),     pm(im),      pm10(im),
     &                                  q0(im),      rat(im),
     &                     theta1(im),  thv1(im),
     &                     tsurf(im),   tv1(im),
     &                     tvs(im),     z0(im),      z0max(im),
     &                     ztmax(im),   qs1(im)

!
!     real(kind=kind_phys) aa1,bb1,bb2,cc,cc1,cc2,arnu
      real(kind=kind_phys) a0,    a0p,    a1,    a1p,   aa,   aa0,
     &                     adtv,  alpha, b1,   b1p,
     &                     b2,    b2p,    bb,    bb0, 
     &                     ca,    charnock, 
     &                     cq,    fms,    fhs,   g,     hl0,  hl0inf,
     &                     hl110, hlt,    hltinf,olinf,   
     &                     restar, rnu,   vis,   z0s_max, czilc
!
cc
      parameter (charnock=.014,ca=.4)!c ca is the von karman constant
      parameter (g=grav)
      parameter (alpha=5.,a0=-3.975,a1=12.32,b1=-7.755,b2=6.041)
      parameter (a0p=-7.941,a1p=24.75,b1p=-8.705,b2p=7.899,vis=1.4e-5)
!     parameter (aa1=-1.076,bb1=.7045,cc1=-.05808)
!     parameter (bb2=-.1954,cc2=.009999)
      parameter (rnu=1.51e-5,z0s_max=.317e-2)
!     parameter (arnu=.135*rnu)
!
!  z0s_max is a limiting value in high winds over sea
!    z0s_max=.196e-2 for u10_crit=25 m/s
!    z0s_max=.317e-2 for u10_crit=30 m/s
!    z0s_max=.479e-2 for u10_crit=35 m/s
!
c mbek -- toga-coare flux algorithm
!     parameter (rnu=1.51e-5,arnu=0.11*rnu)
c
c  initialize variables. all units are supposedly m.k.s. unless specified
c  ps is in pascals
c  wind is wind speed, theta1 is adiabatic surface temp from level 1
c  surface roughness length is converted to m from cm
c
      do i=1,im
        if(flag_iter(i)) then 
!**       tsurf(i)  = tskin(i)                 !! <---- clu_q2m_iter [-1l]
          wind(i)   = sqrt(u1(i) * u1(i) + v1(i) * v1(i))
     &              + max(0.0, min(ddvel(i), 30.0))
          wind(i)   = max(wind(i),1.)
          q0(i)     = max(q1(i),1.e-8)
          theta1(i) = t1(i) * prslki(i)
          tv1(i)    = t1(i) * (1. + rvrdm1 * q0(i))
          thv1(i)   = theta1(i) * (1. + rvrdm1 * q0(i))
clu_q2m_iter[-1l/+2l]: tvs is computed from avg(tsurf,tskin)
!**       tvs(i)    = tsurf(i) * (1. + rvrdm1 * q0(i))
          tvs(i)    = 0.5 * (tsurf(i)+tskin(i)) * (1. + rvrdm1 * q0(i))
          qs1(i)    = fpvs(t1(i))
          qs1(i)    = eps * qs1(i) / (prsl1(i) + epsm1 * qs1(i))
          qs1(i)    = max(qs1(i), 1.e-8)
          q0(i)     = min(qs1(i),q0(i))

          z0(i)     = .01 * z0rl(i)
!         z1(i)     = -rd * tv1(i) * log(ps1(i)/psurf(i)) / g
          z1i(i)    = 1.0 / z1(i)
        endif
      enddo
!!
c
c  compute stability dependent exchange coefficients
c
c  this portion of the code is presently suppressed
c
      do i=1,im
       if(flag_iter(i)) then 
        if(islimsk(i).eq.0) then
          ustar(i) = sqrt(g * z0(i) / charnock)
        endif
c
c  compute stability indices (rb and hlinf)
c
        z0max(i) = min(z0(i),1. * z1(i))

!**  test xubin's new z0
        if (islimsk(i) .ne. 0) then
          z0max(i) = exp( ((1.-shdmax(i))**2)*log(0.01)+
     &       (1-((1.-shdmax(i))**2))*log(z0max(i)) )
          if (vegtype(i) == 7) then
            z0max(i) = exp( ((1.-shdmax(i))**2)*log(0.01)+
     &       (1-((1.-shdmax(i))**2))*log(0.07) )
          endif
          if (vegtype(i) == 8) then
            z0max(i) = exp( ((1.-shdmax(i))**2)*log(0.01)+
     &        (1-((1.-shdmax(i))**2))*log(0.05) )
          endif
          if (vegtype(i) == 9) then
            z0max(i) = exp( ((1.-shdmax(i))**2)*log(0.01)+
     &       (1-((1.-shdmax(i))**2))*log(0.01) )
          endif
          if (vegtype(i) == 11) then
          z0max(i) = exp( ((1.-shdmax(i))**2)*log(0.01)+
     &       (1-((1.-shdmax(i))**2))*log(0.01) )
          endif
        endif
c fei's canopy height dependance of czil
        czilc = 10.0 ** ( -0.40 * ( z0(i) / 0.07 ) )

        ztmax(i) = z0max(i)*exp( - ((1.-sigmaf(i))**2)
     &             *czilc*ca*sqrt(ustar(i)*0.01/(1.5e-05)))

!**  test xubin's new z0

!       ztmax(i) = z0max(i)

        if(islimsk(i).eq.0) then
          restar   = ustar(i) * z0max(i) / vis
          restar   = max(restar,.000001)
c         restar   = log(restar)
c         restar   = min(restar,5.)
c         restar   = max(restar,-5.)
c         rat(i)   = aa1 + bb1 * restar + cc1 * restar ** 2
c         rat(i)   = rat(i) / (1. + bb2 * restar
c    &                       + cc2 * restar ** 2)
c  rat taken from zeng, zhao and dickinson 1997
          rat(i)   = 2.67 * restar ** .25 - 2.57
          rat(i)   = min(rat(i),7.)
          ztmax(i) = z0max(i) * exp(-rat(i))
        endif
       endif
      enddo
c##dg  if(lat.eq.latd) then
c##dg    print *, ' z0max, ztmax, restar, rat(i) =', 
c##dg &   z0max, ztmax, restar, rat(i)
c##dg  endif
      do i = 1, im
       if(flag_iter(i)) then 
        dtv(i)   = thv1(i) - tvs(i)
        adtv     = abs(dtv(i))
        adtv     = max(adtv,.001)
        dtv(i)   = sign(1.,dtv(i)) * adtv
        rb(i)    = g * dtv(i) * z1(i) / (.5 * (thv1(i) + tvs(i))
     &           * wind(i) * wind(i))
        rb(i)    = max(rb(i),-5000.)
        fm(i)    = log((z0max(i)+z1(i)) / z0max(i))
        fh(i)    = log((ztmax(i)+z1(i)) / ztmax(i))
        hlinf(i) = rb(i) * fm(i) * fm(i) / fh(i)
        fm10(i)  = log((z0max(i)+10.) / z0max(i))
        fh2(i)   = log((ztmax(i)+2.) / ztmax(i))
       endif
      enddo
c##dg  if(lat.eq.latd) then
c##dg    print *, ' dtv, rb(i), fm(i), fh(i), hlinf =',
c##dg &   dtv, rb, fm(i), fh(i), hlinf
c##dg  endif
c
c  stable case
c
      do i = 1, im
       if(flag_iter(i)) then 
        if(dtv(i).ge.0.) then
          hl1(i) = hlinf(i)
        endif
        if(dtv(i).ge.0..and.hlinf(i).gt..25) then
          hl0inf = z0max(i) * hlinf(i) * z1i(i)
          hltinf = ztmax(i) * hlinf(i) * z1i(i)
          aa     = sqrt(1. + 4. * alpha * hlinf(i))
          aa0    = sqrt(1. + 4. * alpha * hl0inf)
          bb     = aa
          bb0    = sqrt(1. + 4. * alpha * hltinf)
          pm(i)  = aa0 - aa + log((aa + 1.) / (aa0 + 1.))
          ph(i)  = bb0 - bb + log((bb + 1.) / (bb0 + 1.))
          fms    = fm(i) - pm(i)
          fhs    = fh(i) - ph(i)
          hl1(i) = fms * fms * rb(i) / fhs
        endif
       endif
      enddo
c
c  second iteration
c
      do i = 1, im
       if(flag_iter(i)) then 
        if(dtv(i).ge.0.) then
          hl0     = z0max(i) * hl1(i) * z1i(i)
          hlt     = ztmax(i) * hl1(i) * z1i(i)
          aa      = sqrt(1. + 4. * alpha * hl1(i))
          aa0     = sqrt(1. + 4. * alpha * hl0)
          bb      = aa
          bb0     = sqrt(1. + 4. * alpha * hlt)
          pm(i)   = aa0 - aa + log((aa + 1.) / (aa0 + 1.))
          ph(i)   = bb0 - bb + log((bb + 1.) / (bb0 + 1.))
          hl110   = hl1(i) * 10. * z1i(i)
          aa      = sqrt(1. + 4. * alpha * hl110)
          pm10(i) = aa0 - aa + log((aa + 1.) / (aa0 + 1.))
          hl12(i) = hl1(i) * 2. * z1i(i)
c         aa      = sqrt(1. + 4. * alpha * hl12(i))
          bb      = sqrt(1. + 4. * alpha * hl12(i))
          ph2(i)  = bb0 - bb + log((bb + 1.) / (bb0 + 1.))
        endif
       endif
      enddo
!!
c##dg  if(lat.eq.latd) then
c##dg    print *, ' hl1(i), pm, ph =',
c##dg &   hl1(i),  pm, ph
c##dg  endif
c
c  unstable case
c
c
c  check for unphysical obukhov length
c
      do i=1,im
       if(flag_iter(i)) then 
        if(dtv(i).lt.0.) then
          olinf = z1(i) / hlinf(i)
          if(abs(olinf).le.50. * z0max(i)) then
            hlinf(i) = -z1(i) / (50. * z0max(i))
          endif
        endif
       endif
      enddo
c
c  get pm and ph
c
      do i = 1, im
       if(flag_iter(i)) then 
        if(dtv(i).lt.0..and.hlinf(i).ge.-.5) then
          hl1(i)  = hlinf(i)
          pm(i)   = (a0 + a1 * hl1(i)) * hl1(i)
     &            / (1. + b1 * hl1(i) + b2 * hl1(i) * hl1(i))
          ph(i)   = (a0p + a1p * hl1(i)) * hl1(i)
     &            / (1. + b1p * hl1(i) + b2p * hl1(i) * hl1(i))
          hl110   = hl1(i) * 10. * z1i(i)
          pm10(i) = (a0 + a1 * hl110) * hl110
     &            / (1. + b1 * hl110 + b2 * hl110 * hl110)
          hl12(i) = hl1(i) * 2. * z1i(i)
          ph2(i)  = (a0p + a1p * hl12(i)) * hl12(i)
     &            / (1. + b1p * hl12(i) + b2p * hl12(i) * hl12(i))
        endif
        if(dtv(i).lt.0.and.hlinf(i).lt.-.5) then
          hl1(i)  = -hlinf(i)
          pm(i)   = log(hl1(i)) + 2. * hl1(i) ** (-.25) - .8776
          ph(i)   = log(hl1(i)) + .5 * hl1(i) ** (-.5) + 1.386
          hl110   = hl1(i) * 10. * z1i(i)
          pm10(i) = log(hl110) + 2. * hl110 ** (-.25) - .8776
          hl12(i) = hl1(i) * 2. * z1i(i)
          ph2(i)  = log(hl12(i)) + .5 * hl12(i) ** (-.5) + 1.386
        endif
       endif
      enddo
c
c  finish the exchange coefficient computation to provide fm and fh
c
      do i = 1, im
       if(flag_iter(i)) then 
        fm(i) = fm(i) - pm(i)
        fh(i) = fh(i) - ph(i)
        fm10(i) = fm10(i) - pm10(i)
        fh2(i) = fh2(i) - ph2(i)
        cm(i) = ca * ca / (fm(i) * fm(i))
        ch(i) = ca * ca / (fm(i) * fh(i))
        cq = ch(i)
        stress(i) = cm(i) * wind(i) * wind(i)
        ustar(i)  = sqrt(stress(i))
!       ustar(i) = sqrt(cm(i) * wind(i) * wind(i))
       endif
      enddo
c##dg  if(lat.eq.latd) then
c##dg    print *, ' fm, fh, cm, ch(i), ustar =',
c##dg &   fm, fh, cm, ch, ustar
c##dg  endif
c
c  update z0 over ocean
c
      do i = 1, im
       if(flag_iter(i)) then 
        if(islimsk(i).eq.0) then
c          z0(i) = (charnock / g) * ustar(i) ** 2
           z0(i) = (charnock / g) * ustar(i) * ustar(i)
c mbek -- toga-coare flux algorithm
!         z0(i) = (charnock / g) * ustar(i)*ustar(i) +  arnu/ustar(i)
c  new implementation of z0
c         cc = ustar(i) * z0 / rnu
c         pp = cc / (1. + cc)
c         ff = g * arnu / (charnock * ustar(i) ** 3)
c         z0 = arnu / (ustar(i) * ff ** pp)
          if (redrag) then
            z0(i) = min(z0(i),z0s_max)
          else
            z0(i) = min(z0(i),.1)
          endif
          z0(i) = max(z0(i),1.e-7)
          z0rl(i) = 100. * z0(i)
        endif
       endif
      enddo

      return
      end
