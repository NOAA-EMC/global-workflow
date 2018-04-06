      subroutine h2ophys (ix, im, levs, kh2o, dt, h2oi, h2oo, ph2o,
     &                    prsl, h2opltc, h2o_coeff, ldiag3d, h2op,me)
!
! May 2015 - Shrinivas Moorthi - Adaptation of NRL H2O physics for
!                                stratosphere and mesosphere
!
!     this code assumes that both prsl and ph2o are from bottom to top
!     as are all other variables
!
      use machine , only : kind_phys
      implicit none
!
      integer im, ix, levs, kh2o, h2o_coeff,me
      real(kind=kind_phys) h2oi(ix,levs),  h2oo(ix,levs), ph2o(kh2o),
     &                     prsl(ix,levs),  tin(ix,levs),
     &                     h2opltc(ix,kh2o,h2o_coeff),
     &                     h2op(ix,levs,h2o_coeff),  dt
!
      integer k,kmax,kmin,l,i,j
      logical              ldiag3d, flg(im)
      real(kind=kind_phys) pmax, pmin, tem, temp
      real(kind=kind_phys) wk1(im), wk2(im), wk3(im), pltc(im,h2o_coeff)
     &,                    h2oib(im)
      real, parameter :: prsmax=10000.0, pmaxl=log(prsmax)
!
!     write(1000+me,*)' in h2ophys ix=',ix, im, levs, kh2o, dt
      do l=1,levs
        pmin =  1.0e10
        pmax = -1.0e10
!
        do i=1,im
          wk1(i) = log(prsl(i,l))
          pmin   = min(wk1(i), pmin)
          pmax   = max(wk1(i), pmax)
          pltc(i,:) = 0.0
        enddo
        if (pmin < pmaxl) then
          kmax = 1
          kmin = 1
          do k=1,kh2o-1
            if (pmin < ph2o(k)) kmax = k
            if (pmax < ph2o(k)) kmin = k
          enddo
!
          do k=kmin,kmax
            temp = 1.0 / (ph2o(k) - ph2o(k+1))
            do i=1,im
              flg(i) = .false.
              if (wk1(i) < ph2o(k) .and. wk1(i) >= ph2o(k+1)) then
                flg(i) = .true.
                wk2(i) = (wk1(i) - ph2o(k+1)) * temp
                wk3(i) = 1.0 - wk2(i)
              endif
            enddo
            do j=1,h2o_coeff
              do i=1,im
                if (flg(i)) then
                  pltc(i,j)  = wk2(i) * h2opltc(i,k,j)
     &                       + wk3(i) * h2opltc(i,k+1,j)
                endif
              enddo
            enddo
          enddo
!
          do j=1,h2o_coeff
            do i=1,im
              if (wk1(i) < ph2o(kh2o)) then
                pltc(i,j) = h2opltc(i,kh2o,j)
              endif
              if (wk1(i) >= ph2o(1)) then
                pltc(i,j) = h2opltc(i,1,j)
              endif
            enddo
          enddo
        endif
        do i=1,im
          if (prsl(i,l) < prsmax) then
            h2oib(i)   = h2oi(i,l)           ! no filling
            tem        = 1.0 / pltc(i,2)     ! 1/teff
            h2oo(i,l)  = (h2oib(i) + (pltc(i,1)+pltc(i,3)*tem)*dt)
     &                 / (1.0 + tem*dt)
          else
            h2oo(i,l)  = h2oi(i,l)
          endif

!           if (i == 1) write(1000+me,*)' h2oib=',h2oib(i),' pltc1=',
!    &pltc(i,1),' pltc2=', pltc(i,2),' tem=',tem ,' dt=',dt
!    &,' l=',l
        enddo
!
!       if (ldiag3d) then     !    h2o change diagnostics
!         do i=1,im
!           h2op(i,l,1) = h2op(i,l,1) + pltc(i,1)*dt
!           h2op(i,l,2) = h2op(i,l,2) + (h2oo(i,l) - h2oib(i))
!         enddo
!       endif
      enddo                   ! vertical loop
!
      return
      end
