      subroutine sfc_diag(im,km,ps,u1,v1,t1,q1,
     &                  tskin,qsurf,f10m,u10m,v10m,t2m,q2m,
     &                  prslki,evap,fm,fh,fm10,fh2)
!
      use machine , only : kind_phys
      use funcphys, only : fpvs
      use physcons, grav => con_g, sbc => con_sbc, hvap => con_hvap
     &,             cp => con_cp, hfus => con_hfus, jcal => con_jcal
     &,             eps => con_eps, epsm1 => con_epsm1
     &,             rvrdm1 => con_fvirt, rd => con_rd
      implicit none
!
      integer              im, km
!
      real(kind=kind_phys) ps(im),       u1(im),      v1(im),
     &                     t1(im),       q1(im),  
     &                     tskin(im),    qsurf(im), 
     &                     f10m(im),     u10m(im),
     &                     v10m(im),     t2m(im),     q2m(im),
     &                                   prslki(im),
     &                     evap(im),    
     &                     fm(im),       fh(im),
     &                     fm10(im),     fh2(im)
!
!     locals
!
      real (kind=kind_phys), parameter :: qmin=1.0e-8
      integer              k,i
!
      real(kind=kind_phys) qss(im), theta1(im)
!
      real(kind=kind_phys) g,    sig2k, fhi
!
      parameter (g=grav)
!
!
!
!     estimate sigma ** k at 2 m
!
!     sig2k = 1. - 4. * g * 2. / (cp * 280.)
!
!  initialize variables. all units are supposedly m.k.s. unless specifie
!  ps is in pascals
!  theta1 is adiabatic surface temp from level 1
!
!!
      do i=1,im
        theta1(i) = t1(i) * prslki(i)
      enddo
!!
!
      do i = 1, im
        f10m(i) = fm10(i) / fm(i)
!       f10m(i) = min(f10m(i),1.)
        u10m(i) = f10m(i) *  u1(i)
        v10m(i) = f10m(i) * v1(i)
        fhi     = fh2(i) / fh(i)
        t2m(i)  = tskin(i)*(1. - fhi) + theta1(i)*fhi
        sig2k = 1. - g * 2. / (cp * t2m(i))
        t2m(i)  = t2m(i) * sig2k
        if(evap(i) >= 0.) then !  for evaporation>0, use inferred qsurf to deduce q2m
          q2m(i) = qsurf(i)*(1.-fhi) + max(qmin,q1(i))*fhi
        else                   !  for dew formation, use saturated q at tskin
          qss(i) = fpvs(tskin(i))
          qss(i) = eps * qss(i) / (ps(i) + epsm1 * qss(i))
          q2m(i) = qss(i)*(1.-fhi) + max(qmin,q1(i))*fhi
        endif
        qss(i) = fpvs(t2m(i))
        qss(i) = eps * qss(i) / (ps(i) + epsm1 * qss(i))
        q2m(i) = min(q2m(i),qss(i))
      enddo

      return
      end
