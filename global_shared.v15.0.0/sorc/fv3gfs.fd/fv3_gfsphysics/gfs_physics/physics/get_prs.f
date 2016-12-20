      subroutine GET_PRS(im,ix,levs,ntrac,t,q,
     &                   thermodyn_id, sfcpress_id,
     &                   gen_coord_hybrid,
     &                   prsi,prki,prsl,prkl,phii,phil,del)
!    &                   prsi,prki,prsl,prkl,phii,phil,del,lprnt)
!
      USE MACHINE ,              ONLY : kind_phys
!     use resol_def ,            only : thermodyn_id, sfcpress_id
!     use namelist_physics_def , only : gen_coord_hybrid 
      use physcons ,             only : cp => con_cp, nu => con_fvirt
     &,                                 rd => con_rd, rkap => con_rocp
      USE tracer_const
      implicit none
!
      integer im, ix, levs, ntrac, thermodyn_id, sfcpress_id
      logical gen_coord_hybrid
!     logical gen_coord_hybrid, lprnt
      real(kind=kind_phys) prsi(ix,levs+1), prki(ix,levs+1)
     &,                    phii(ix,levs+1), phil(ix,levs)
     &,                    prsl(ix,levs),   prkl(ix,levs)
     &,                    del(ix,levs),    T(ix,levs)
     &,                    q(ix,levs,ntrac)
      real(kind=kind_phys) xcp(ix,levs), xr(ix,levs), kappa(ix,levs)
      real(kind=kind_phys) tem, dphib, dphit, dphi
      real (kind=kind_phys), parameter :: zero=0.0, p00i=1.0e-5
     &,                                rkapi=1.0/rkap, rkapp1=1.0+rkap
      integer i, k, n
!
      do k=1,levs
        do i=1,im
          del(i,k) = PRSI(i,k) - PRSI(i,k+1)
        enddo
      enddo
!
      if( gen_coord_hybrid ) then                                       ! hmhj
        if( thermodyn_id.eq.3 ) then      ! Enthalpy case
!
! hmhj : This is for generalized hybrid (Henry) with finite difference
!        in the vertical and enthalpy as the prognostic (thermodynamic)
!        variable.  However, the input "t" here is the temperature,
!        not enthalpy (because this subroutine is called by gbphys where
!        only temperature is available).
!
          if (prki(1,1) <= zero .or. prkl(1,1) <= zero) then
            call GET_CPR(im,ix,levs,ntrac,q,xcp,xr)
!
            do k=1,levs
              do i=1,im
                kappa(i,k) = xr(i,k)/xcp(i,k)
                prsl(i,k)  = (PRSI(i,k) + PRSI(i,k+1))*0.5
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
              phii(i,1)   = 0.0           ! Ignoring topography height here
            enddo
            DO k=1,levs
              do i=1,im
                TEM         = xr(i,k) * T(i,k)
                DPHI        = (PRSI(i,k) - PRSI(i,k+1)) * TEM
     &                      / (PRSI(i,k) + PRSI(i,k+1))
                phil(i,k)   = phii(i,k) + DPHI
                phii(i,k+1) = phil(i,k) + DPHI
!     if(k == 1 .and. i == 1) print *,' xr=',xr(1,1),' T=',t(1,1)
!    &,' prsi=',prsi(1,1),prsi(1,2),' tem=',tem,' dphi=',dphi
              ENDDO
            ENDDO
          endif
          if (prsl(1,1) <= 0.0) then
            do k=1,levs
              do i=1,im
                prsl(i,k)  = (PRSI(i,k) + PRSI(i,k+1))*0.5
              enddo
            enddo
          endif
          if (phil(1,levs) <= 0.0) then ! If geopotential is not given, calculate
            do i=1,im
              phii(i,1)   = 0.0           ! Ignoring topography height here
            enddo
            call GET_R(im,ix,levs,ntrac,q,xr)
            DO k=1,levs
              do i=1,im
                TEM         = xr(i,k) * T(i,k)
                DPHI        = (PRSI(i,k) - PRSI(i,k+1)) * TEM
     &                      / (PRSI(i,k) + PRSI(i,k+1))
                phil(i,k)   = phii(i,k) + DPHI
                phii(i,k+1) = phil(i,k) + DPHI
!     if(k == 1 .and. i == 1) print *,' xr=',xr(1,1),' T=',t(1,1)
!    &,' prsi=',prsi(1,1),prsi(1,2),' tem=',tem,' dphi=',dphi
              ENDDO
            ENDDO
          endif
        else                                 ! gc Virtual Temp case
          if (prki(1,1) <= zero .or. prkl(1,1) <= zero) then
            do k=1,levs
              do i=1,im
                prsl(i,k) = (PRSI(i,k) + PRSI(i,k+1))*0.5
                prkl(i,k) = (prsl(i,k)*p00i) ** rkap
                enddo
              enddo
              do k=1,levs+1
                do i=1,im
                  prki(i,k) = (prsi(i,k)*p00i) ** rkap
                enddo
              enddo
              do i=1,im
                phii(i,1)   = 0.0           ! Ignoring topography height here
              enddo
              DO k=1,levs
                do i=1,im
                  TEM         = rd * T(i,k)*(1.0+NU*max(Q(i,k,1),zero))
                  DPHI        = (PRSI(i,k) - PRSI(i,k+1)) * TEM
     &                        / (PRSI(i,k) + PRSI(i,k+1))
                  phil(i,k)   = phii(i,k) + DPHI
                  phii(i,k+1) = phil(i,k) + DPHI
      if (k == 1 .and. phil(i,k) < 0.0) write(0,*)' phil=',phil(i,k)
     &,' dphi=',dphi,' prsi=',prsi(i,k),prsi(i,k+1),' tem=',tem
                ENDDO
              ENDDO
          endif
          if (prsl(1,1) <= 0.0) then
            do k=1,levs
              do i=1,im
                prsl(i,k)  = (PRSI(i,k) + PRSI(i,k+1))*0.5
              enddo
            enddo
          endif
          if (phil(1,levs) <= 0.0) then ! If geopotential is not given, calculate
            do i=1,im
              phii(i,1)   = 0.0         ! Ignoring topography height here
            enddo
            DO k=1,levs
              do i=1,im
                TEM         = rd * T(i,k)*(1.0+NU*max(Q(i,k,1),zero))
                DPHI        = (PRSI(i,k) - PRSI(i,k+1)) * TEM
     &                      / (PRSI(i,k) + PRSI(i,k+1))
                phil(i,k)   = phii(i,k) + DPHI
                phii(i,k+1) = phil(i,k) + DPHI
              ENDDO
            ENDDO
          endif
        endif
      else                                   ! Not gc Virtual Temp (Orig Joe)
        if (prki(1,1) <= zero) then
!                                      Pressure is in Pa!!!!
!     if (lprnt) write(0,*)' prsi=',prsi(1,:)
          do i=1,im
            prki(i,1) = (prsi(i,1)*p00i) ** rkap
          enddo
          do k=1,levs
            do i=1,im
             prki(i,k+1) = (prsi(i,k+1)*p00i) ** rkap
             tem         = rkapp1 * del(i,k)
             prkl(i,k)   = (prki(i,k)*PRSI(i,k)-prki(i,k+1)*PRSI(i,k+1))
     &                   / tem
            enddo
          enddo
!     if (lprnt) write(0,*)' prki=',prki(1,:)
!     if (lprnt) write(0,*)' prkl=',prkl(1,:)
        
        elseif (prkl(1,1) <= zero) then
          do k=1,levs
            do i=1,im
             tem         = rkapp1 * del(i,k)
             prkl(i,k)   = (prki(i,k)*PRSI(i,k)-prki(i,k+1)*PRSI(i,k+1))
     &                   / tem
            enddo
          enddo
        endif
        if (prsl(1,1) <= 0.0) then
          do k=1,levs
            do i=1,im
              PRSL(i,k)   = 100.0 * PRKL(i,k) ** rkapi
            enddo
          enddo
        endif
        if (phil(1,levs) <= 0.0) then ! If geopotential is not given, calculate
          do i=1,im
            phii(i,1)   = 0.0         ! Ignoring topography height here
          enddo
          DO k=1,levs
            do i=1,im
              TEM         = CP * T(i,k) * (1.0 + NU*max(Q(i,k,1),zero))
     &                    / PRKL(i,k)
              DPHIB       = (PRKI(i,k) - PRKL(i,k)) * TEM
              DPHIT       = (PRKL(i,k  ) - PRKI(i,k+1)) * TEM
              phil(i,k)   = phii(i,k) + DPHIB
              phii(i,k+1) = phil(i,k) + DPHIT
            ENDDO
          ENDDO
!       if (lprnt)write(0,*)' in get_prs phil=',phil(1,1),'t=',t(1,1),
!    &' q=',q(1,1,1),' nu=',nu,' prkl=',prkl(1,1),prki(1,1),' cp=',cp
        endif
      endif
!
      return
      end
      subroutine GET_PHI(im,ix,levs,ntrac,t,q,
     &                   thermodyn_id, sfcpress_id,
     &                   gen_coord_hybrid,
     &                   prsi,prki,prsl,prkl,phii,phil)
!
      USE MACHINE ,              ONLY : kind_phys
!     use resol_def ,            only : thermodyn_id, sfcpress_id
!     use namelist_physics_def , only : gen_coord_hybrid
      use physcons ,             only : cp => con_cp, nu => con_fvirt
     &,                                 rd => con_rd, rkap => con_rocp
      USE tracer_const
      implicit none
!
      integer im, ix, levs, ntrac, thermodyn_id, sfcpress_id
      logical gen_coord_hybrid
      real(kind=kind_phys) prsi(ix,levs+1), prsl(ix,levs)
     &,                    prki(ix,levs+1), prkl(ix,levs)
     &,                    phii(ix,levs+1), phil(ix,levs)
     &,                    T(ix,levs),      q(ix,levs,ntrac)
      real(kind=kind_phys) xr(ix,levs)
      real(kind=kind_phys) tem, dphib, dphit, dphi
      real (kind=kind_phys), parameter :: zero=0.0
      integer i, k, n
!
      do i=1,im
        phii(i,1)   = zero                     ! Ignoring topography height here
      enddo
      if( gen_coord_hybrid ) then              ! hmhj
        if( thermodyn_id.eq.3 ) then           ! Enthalpy case
          call GET_R(im,ix,levs,ntrac,q,xr)
          DO k=1,levs
            do i=1,im
              TEM         = xr(i,k) * T(i,k)
              DPHI        = (PRSI(i,k) - PRSI(i,k+1)) * TEM
     &                     /(PRSI(i,k) + PRSI(i,k+1))
              phil(i,k)   = phii(i,k) + DPHI
              phii(i,k+1) = phil(i,k) + DPHI
!     if(k <= 4 .and. i == 1) print *,' xr=',xr(1,k),' T=',t(1,k)
!    &,' prsi=',prsi(1,k),prsi(1,k+1),' tem=',tem,' dphi=',dphi,' k=',k
            ENDDO
          ENDDO
!
        else                                 ! gc Virtual Temp
          DO k=1,levs
            do i=1,im
              TEM         = RD * T(i,k) * (1.0 + NU*max(Q(i,k,1),zero))
              DPHI        = (PRSI(i,k) - PRSI(i,k+1)) * TEM
     &                     /(PRSI(i,k) + PRSI(i,k+1))
              phil(i,k)   = phii(i,k) + DPHI
              phii(i,k+1) = phil(i,k) + DPHI
            ENDDO
          ENDDO
        endif
      else                                   ! Not gc Virt Temp (Orig Joe)
        DO k=1,levs
          do i=1,im
            TEM         = CP * T(i,k) * (1.0 + NU*max(Q(i,k,1),zero))
     &                  / PRKL(i,k)
            DPHIB       = (PRKI(i,k) - PRKL(i,k)) * TEM
            DPHIT       = (PRKL(i,k  ) - PRKI(i,k+1)) * TEM
            phil(i,k)   = phii(i,k) + DPHIB
            phii(i,k+1) = phil(i,k) + DPHIT
          ENDDO
        ENDDO
      endif
!
      return
      end
      subroutine GET_CPR(im,ix,levs,ntrac,q,xcp,xr)
!
      USE MACHINE ,      ONLY : kind_phys
      USE tracer_const
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
      subroutine GET_R(im,ix,levs,ntrac,q,xr)
!
      USE MACHINE ,      ONLY : kind_phys
      USE tracer_const
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
      subroutine GET_CP(im,ix,levs,ntrac,q,xcp)
!
      USE MACHINE ,      ONLY : kind_phys
      USE tracer_const
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
