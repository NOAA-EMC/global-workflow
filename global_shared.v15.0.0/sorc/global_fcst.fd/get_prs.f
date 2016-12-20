      subroutine get_prs(im,ix,levs,ntrac,t,q,
     &                   thermodyn_id, sfcpress_id,
     &                   gen_coord_hybrid,
     &                   prsi,prki,prsl,prkl,phii,phil,del)
!
      use machine ,              only : kind_phys
!     use resol_def ,            only : thermodyn_id, sfcpress_id
!     use namelist_physics_def , only : gen_coord_hybrid 
      use physcons ,             only : cp => con_cp, nu => con_fvirt
     &,                                 rd => con_rd, rkap => con_rocp
      use tracer_const
      implicit none
!
      integer im, ix, levs, ntrac, thermodyn_id, sfcpress_id
      logical gen_coord_hybrid
      real(kind=kind_phys) prsi(ix,levs+1), prki(ix,levs+1)
     &,                    phii(ix,levs+1), phil(ix,levs)
     &,                    prsl(ix,levs),   prkl(ix,levs)
     &,                    del(ix,levs),    t(ix,levs)
     &,                    q(ix,levs,ntrac)
      real(kind=kind_phys) xcp(ix,levs), xr(ix,levs), kappa(ix,levs)
      real(kind=kind_phys) tem, dphib, dphit, dphi
      real (kind=kind_phys), parameter :: zero=0.0, p00i=1.0e-5
     &,                                rkapi=1.0/rkap, rkapp1=1.0+rkap
      integer i, k, n
!
      do k=1,levs
        do i=1,im
          del(i,k) = prsi(i,k) - prsi(i,k+1)
        enddo
      enddo
!
      if( gen_coord_hybrid ) then                                       ! hmhj
        if( thermodyn_id.eq.3 ) then      ! enthalpy case
!
! hmhj : this is for generalized hybrid (henry) with finite difference
!        in the vertical and enthalpy as the prognostic (thermodynamic)
!        variable.  however, the input "t" here is the temperature,
!        not enthalpy (because this subroutine is called by gbphys where
!        only temperature is available).
!
          if (prki(1,1) <= zero .or. prkl(1,1) <= zero) then
            call get_cpr(im,ix,levs,ntrac,q,xcp,xr)
!
            do k=1,levs
              do i=1,im
                kappa(i,k) = xr(i,k)/xcp(i,k)
                prsl(i,k)  = (prsi(i,k) + prsi(i,k+1))*0.5
                prkl(i,k)  = (prsl(i,k)*p00i) ** kappa(i,k)
              enddo
            enddo
            do k=2,levs
              do i=1,im
                tem = 0.5 * (kappa(i,k) + kappa(i,k-1))
                prki(i,k-1) = (prsi(i,k)*p00i) ** tem
              enddo
            enddo
            do i=1,im
              prki(i,1) = (prsi(i,1)*p00i) ** kappa(i,1)
            enddo
            k = levs + 1
            if (prsi(1,k) .gt. 0.0) then
              do i=1,im
                prki(i,k) = (prsi(i,k)*p00i) ** kappa(i,levs)
              enddo
            endif
!
            do i=1,im
              phii(i,1)   = 0.0           ! ignoring topography height here
            enddo
            do k=1,levs
              do i=1,im
                tem         = xr(i,k) * t(i,k)
                dphi        = (prsi(i,k) - prsi(i,k+1)) * tem
     &                      / (prsi(i,k) + prsi(i,k+1))
                phil(i,k)   = phii(i,k) + dphi
                phii(i,k+1) = phil(i,k) + dphi
!     if(k == 1 .and. i == 1) print *,' xr=',xr(1,1),' t=',t(1,1)
!    &,' prsi=',prsi(1,1),prsi(1,2),' tem=',tem,' dphi=',dphi
              enddo
            enddo
          endif
          if (prsl(1,1) <= 0.0) then
            do k=1,levs
              do i=1,im
                prsl(i,k)  = (prsi(i,k) + prsi(i,k+1))*0.5
              enddo
            enddo
          endif
          if (phil(1,levs) <= 0.0) then ! if geopotential is not given, calculate
            do i=1,im
              phii(i,1)   = 0.0           ! ignoring topography height here
            enddo
            call get_r(im,ix,levs,ntrac,q,xr)
            do k=1,levs
              do i=1,im
                tem         = xr(i,k) * t(i,k)
                dphi        = (prsi(i,k) - prsi(i,k+1)) * tem
     &                      / (prsi(i,k) + prsi(i,k+1))
                phil(i,k)   = phii(i,k) + dphi
                phii(i,k+1) = phil(i,k) + dphi
!     if(k == 1 .and. i == 1) print *,' xr=',xr(1,1),' t=',t(1,1)
!    &,' prsi=',prsi(1,1),prsi(1,2),' tem=',tem,' dphi=',dphi
              enddo
            enddo
          endif
        else                                 ! gc virtual temp case
          if (prki(1,1) <= zero .or. prkl(1,1) <= zero) then
            do k=1,levs
              do i=1,im
                prsl(i,k) = (prsi(i,k) + prsi(i,k+1))*0.5
                prkl(i,k) = (prsl(i,k)*p00i) ** rkap
                enddo
              enddo
              do k=1,levs+1
                do i=1,im
                  prki(i,k) = (prsi(i,k)*p00i) ** rkap
                enddo
              enddo
              do i=1,im
                phii(i,1)   = 0.0           ! ignoring topography height here
              enddo
              do k=1,levs
                do i=1,im
                  tem         = rd * t(i,k)*(1.0+nu*max(q(i,k,1),zero))
                  dphi        = (prsi(i,k) - prsi(i,k+1)) * tem
     &                        / (prsi(i,k) + prsi(i,k+1))
                  phil(i,k)   = phii(i,k) + dphi
                  phii(i,k+1) = phil(i,k) + dphi
                enddo
              enddo
          endif
          if (prsl(1,1) <= 0.0) then
            do k=1,levs
              do i=1,im
                prsl(i,k)  = (prsi(i,k) + prsi(i,k+1))*0.5
              enddo
            enddo
          endif
          if (phil(1,levs) <= 0.0) then ! if geopotential is not given, calculate
            do i=1,im
              phii(i,1)   = 0.0         ! ignoring topography height here
            enddo
            do k=1,levs
              do i=1,im
                tem         = rd * t(i,k)*(1.0+nu*max(q(i,k,1),zero))
                dphi        = (prsi(i,k) - prsi(i,k+1)) * tem
     &                      / (prsi(i,k) + prsi(i,k+1))
                phil(i,k)   = phii(i,k) + dphi
                phii(i,k+1) = phil(i,k) + dphi
              enddo
            enddo
          endif
        endif
      else                                   ! not gc virtual temp (orig joe)
        if (prki(1,1) <= zero) then
!                                      pressure is in pa!!!!
          do i=1,im
            prki(i,1) = (prsi(i,1)*p00i) ** rkap
          enddo
          do k=1,levs
            do i=1,im
             prki(i,k+1) = (prsi(i,k+1)*p00i) ** rkap
             tem         = rkapp1 * del(i,k)
             prkl(i,k)   = (prki(i,k)*prsi(i,k)-prki(i,k+1)*prsi(i,k+1))
     &                   / tem
            enddo
          enddo
        
        elseif (prkl(1,1) <= zero) then
          do k=1,levs
            do i=1,im
             tem         = rkapp1 * del(i,k)
             prkl(i,k)   = (prki(i,k)*prsi(i,k)-prki(i,k+1)*prsi(i,k+1))
     &                   / tem
            enddo
          enddo
        endif
        if (prsl(1,1) <= 0.0) then
          do k=1,levs
            do i=1,im
              prsl(i,k)   = 100.0 * prkl(i,k) ** rkapi
            enddo
          enddo
        endif
        if (phil(1,levs) <= 0.0) then ! if geopotential is not given, calculate
          do i=1,im
            phii(i,1)   = 0.0         ! ignoring topography height here
          enddo
          do k=1,levs
            do i=1,im
              tem         = cp * t(i,k) * (1.0 + nu*max(q(i,k,1),zero))
     &                    / prkl(i,k)
              dphib       = (prki(i,k) - prkl(i,k)) * tem
              dphit       = (prkl(i,k  ) - prki(i,k+1)) * tem
              phil(i,k)   = phii(i,k) + dphib
              phii(i,k+1) = phil(i,k) + dphit
            enddo
          enddo
        endif
      endif
!
      return
      end
      subroutine get_phi(im,ix,levs,ntrac,t,q,
     &                   thermodyn_id, sfcpress_id,
     &                   gen_coord_hybrid,
     &                   prsi,prki,prsl,prkl,phii,phil)
!
      use machine ,              only : kind_phys
!     use resol_def ,            only : thermodyn_id, sfcpress_id
!     use namelist_physics_def , only : gen_coord_hybrid
      use physcons ,             only : cp => con_cp, nu => con_fvirt
     &,                                 rd => con_rd, rkap => con_rocp
      use tracer_const
      implicit none
!
      integer im, ix, levs, ntrac, thermodyn_id, sfcpress_id
      logical gen_coord_hybrid
      real(kind=kind_phys) prsi(ix,levs+1), prsl(ix,levs)
     &,                    prki(ix,levs+1), prkl(ix,levs)
     &,                    phii(ix,levs+1), phil(ix,levs)
     &,                    t(ix,levs),      q(ix,levs,ntrac)
      real(kind=kind_phys) xr(ix,levs)
      real(kind=kind_phys) tem, dphib, dphit, dphi
      real (kind=kind_phys), parameter :: zero=0.0
      integer i, k, n
!
      do i=1,im
        phii(i,1)   = zero                     ! ignoring topography height here
      enddo
      if( gen_coord_hybrid ) then              ! hmhj
        if( thermodyn_id.eq.3 ) then           ! enthalpy case
          call get_r(im,ix,levs,ntrac,q,xr)
          do k=1,levs
            do i=1,im
              tem         = xr(i,k) * t(i,k)
              dphi        = (prsi(i,k) - prsi(i,k+1)) * tem
     &                     /(prsi(i,k) + prsi(i,k+1))
              phil(i,k)   = phii(i,k) + dphi
              phii(i,k+1) = phil(i,k) + dphi
!     if(k <= 4 .and. i == 1) print *,' xr=',xr(1,k),' t=',t(1,k)
!    &,' prsi=',prsi(1,k),prsi(1,k+1),' tem=',tem,' dphi=',dphi,' k=',k
            enddo
          enddo
!
        else                                 ! gc virtual temp
          do k=1,levs
            do i=1,im
              tem         = rd * t(i,k) * (1.0 + nu*max(q(i,k,1),zero))
              dphi        = (prsi(i,k) - prsi(i,k+1)) * tem
     &                     /(prsi(i,k) + prsi(i,k+1))
              phil(i,k)   = phii(i,k) + dphi
              phii(i,k+1) = phil(i,k) + dphi
            enddo
          enddo
        endif
      else                                   ! not gc virt temp (orig joe)
        do k=1,levs
          do i=1,im
            tem         = cp * t(i,k) * (1.0 + nu*max(q(i,k,1),zero))
     &                  / prkl(i,k)
            dphib       = (prki(i,k) - prkl(i,k)) * tem
            dphit       = (prkl(i,k  ) - prki(i,k+1)) * tem
            phil(i,k)   = phii(i,k) + dphib
            phii(i,k+1) = phil(i,k) + dphit
          enddo
        enddo
      endif
!
      return
      end
      subroutine get_cpr(im,ix,levs,ntrac,q,xcp,xr)
!
      use machine ,      only : kind_phys
      use tracer_const
      implicit none
!
      real (kind=kind_phys), parameter :: zero=0.0
      integer im, ix, levs, ntrac
      real(kind=kind_phys) q(ix,levs,ntrac)
      real(kind=kind_phys) xcp(ix,levs),xr(ix,levs),sumq(ix,levs)
      integer i, k, n
!
      sumq = zero
      xr   = zero
      xcp  = zero
      do n=1,ntrac
        if( ri(n) > 0.0 ) then
          do k=1,levs
            do i=1,im
              xr(i,k)   = xr(i,k)   + q(i,k,n) * ri(n)
              xcp(i,k)  = xcp(i,k)  + q(i,k,n) * cpi(n)
              sumq(i,k) = sumq(i,k) + q(i,k,n)
            enddo
          enddo
        endif
      enddo
      do k=1,levs
        do i=1,im
          xr(i,k)    = (1.-sumq(i,k))*ri(0)  + xr(i,k)
          xcp(i,k)   = (1.-sumq(i,k))*cpi(0) + xcp(i,k)
        enddo
      enddo
!
      return
      end
      subroutine get_r(im,ix,levs,ntrac,q,xr)
!
      use machine ,      only : kind_phys
      use tracer_const
      implicit none
!
      real (kind=kind_phys), parameter :: zero=0.0
      integer im, ix, levs, ntrac
      real(kind=kind_phys) q(ix,levs,ntrac)
      real(kind=kind_phys) xr(ix,levs),sumq(ix,levs)
      integer i, k, n
!
      sumq = zero
      xr   = zero
      do n=1,ntrac
        if( ri(n) > 0.0 ) then
          do k=1,levs
            do i=1,im
              xr(i,k)   = xr(i,k)   + q(i,k,n) * ri(n)
              sumq(i,k) = sumq(i,k) + q(i,k,n)
            enddo
          enddo
        endif
      enddo
      do k=1,levs
        do i=1,im
          xr(i,k)    = (1.-sumq(i,k))*ri(0)  + xr(i,k)
        enddo
      enddo
!
      return
      end
      subroutine get_cp(im,ix,levs,ntrac,q,xcp)
!
      use machine ,      only : kind_phys
      use tracer_const
      implicit none
!
      real (kind=kind_phys), parameter :: zero=0.0
      integer im, ix, levs, ntrac
      real(kind=kind_phys) q(ix,levs,ntrac)
      real(kind=kind_phys) xcp(ix,levs),sumq(ix,levs)
      integer i, k, n
!
      sumq = zero
      xcp  = zero
      do n=1,ntrac
        if( cpi(n) > 0.0 ) then
          do k=1,levs
            do i=1,im
              xcp(i,k)  = xcp(i,k)  + q(i,k,n) * cpi(n)
              sumq(i,k) = sumq(i,k) + q(i,k,n)
            enddo
          enddo
        endif
      enddo
      do k=1,levs
        do i=1,im
          xcp(i,k)   = (1.-sumq(i,k))*cpi(0) + xcp(i,k)
        enddo
      enddo
!
      return
      end
