      subroutine ozphys (ix, im, levs, ko3, dt, ozi, ozo, tin, po3,
     &                   prsl, prdout, pl_coeff, delp, ldiag3d,
     &                   ozp,me)
!
!     this code assumes that both prsl and po3 are from bottom to top
!     as are all other variables
!
      use machine , only : kind_phys
      use physcons, only : grav => con_g
      implicit none
!
      real, parameter :: gravi=1.0/grav
      integer im, ix, levs, ko3, pl_coeff,me
      real(kind=kind_phys) ozi(ix,levs),   ozo(ix,levs), po3(ko3),
     &                     prsl(ix,levs),  tin(ix,levs), delp(ix,levs),
     &                     prdout(ix,ko3,pl_coeff),
     &                     ozp(ix,levs,pl_coeff),  dt
!
      integer k,kmax,kmin,l,i,j
      logical              ldiag3d, flg(im)
      real(kind=kind_phys) pmax, pmin, tem, temp
      real(kind=kind_phys) wk1(im), wk2(im), wk3(im), prod(im,pl_coeff),
     &                     ozib(im),  colo3(im,levs+1)
!
      if (pl_coeff > 2) then
        colo3(:,levs+1) = 0.0
        do l=levs,1,-1
          do i=1,im
            colo3(i,l) = colo3(i,l+1) + ozi(i,l) * delp(i,l) * gravi
          enddo
        enddo
      endif
!
      do l=1,levs
        pmin =  1.0e10
        pmax = -1.0e10
!
        do i=1,im
          wk1(i) = log(prsl(i,l))
          pmin   = min(wk1(i), pmin)
          pmax   = max(wk1(i), pmax)
          prod(i,:) = 0.0
        enddo
        kmax = 1
        kmin = 1
        do k=1,ko3-1
          if (pmin < po3(k)) kmax = k
          if (pmax < po3(k)) kmin = k
        enddo
!
        do k=kmin,kmax
          temp = 1.0 / (po3(k) - po3(k+1))
          do i=1,im
            flg(i) = .false.
            if (wk1(i) < po3(k) .and. wk1(i) >= po3(k+1)) then
              flg(i) = .true.
              wk2(i) = (wk1(i) - po3(k+1)) * temp
              wk3(i) = 1.0 - wk2(i)
            endif
          enddo
          do j=1,pl_coeff
            do i=1,im
              if (flg(i)) then
                prod(i,j)  = wk2(i) * prdout(i,k,j)
     &                     + wk3(i) * prdout(i,k+1,j)
              endif
            enddo
          enddo
        enddo
!
        do j=1,pl_coeff
          do i=1,im
            if (wk1(i) < po3(ko3)) then
              prod(i,j) = prdout(i,ko3,j)
            endif
            if (wk1(i) >= po3(1)) then
              prod(i,j) = prdout(i,1,j)
            endif
          enddo
        enddo
        if (pl_coeff == 2) then 
          do i=1,im
            ozib(i)   = ozi(i,l)           ! no filling
            ozo(i,l)  = (ozib(i) + prod(i,1)*dt) / (1.0 + prod(i,2)*dt)
          enddo
!
          if (ldiag3d) then     !     ozone change diagnostics
            do i=1,im
              ozp(i,l,1) = ozp(i,l,1) + prod(i,1)*dt
              ozp(i,l,2) = ozp(i,l,2) + (ozo(i,l) - ozib(i))
            enddo
          endif
        endif
        if (pl_coeff == 4) then 
          do i=1,im
            ozib(i)  = ozi(i,l)            ! no filling
            tem      = prod(i,1) + prod(i,3)*tin(i,l)
     &                           + prod(i,4)*colo3(i,l+1)
!     if (me .eq. 0) print *,'ozphys tem=',tem,' prod=',prod(i,:)
!    &,' ozib=',ozib(i),' l=',l,' tin=',tin(i,l),'colo3=',colo3(i,l+1)
            ozo(i,l) = (ozib(i)  + tem*dt) / (1.0 + prod(i,2)*dt)
          enddo
          if (ldiag3d) then     !     ozone change diagnostics
            do i=1,im
              ozp(i,l,1) = ozp(i,l,1) + prod(i,1)*dt
              ozp(i,l,2) = ozp(i,l,2) + (ozo(i,l) - ozib(i))
              ozp(i,l,3) = ozp(i,l,3) + prod(i,3)*tin(i,l)*dt
              ozp(i,l,4) = ozp(i,l,4) + prod(i,4)*colo3(i,l+1)*dt
            enddo
          endif
        endif
      enddo                                ! vertical loop
!
      return
      end
