!-----------------------------------
      subroutine sfc_sice                                               &
!...................................
!  ---  inputs:
     &     ( im, km, ps, u1, v1, t1, q1, delt,                          &
     &       sfcemis, dlwflx, sfcnsw, sfcdsw, srflag,                   &
     &       cm, ch, prsl1, prslki, islimsk, ddvel,                     &
     &       flag_iter, mom4ice,                                        &
!  ---  input/outputs:
     &       hice, fice, tice, weasd, tskin, tprcp, stc, ep,            &
!  ---  outputs:
     &       snwdph, qsurf, snowmt, gflux, cmm, chh, evap, hflx         &
     &     )

! ===================================================================== !
!  description:                                                         !
!                                                                       !
!  usage:                                                               !
!                                                                       !
!    call sfc_sice                                                      !
!       inputs:                                                         !
!          ( im, km, ps, u1, v1, t1, q1, delt,                          !
!            sfcemis, dlwflx, sfcnsw, sfcdsw, srflag,                   !
!            cm, ch, prsl1, prslki, islimsk, ddvel,                     !
!            flag_iter, mom4ice,                                        !
!       input/outputs:                                                  !
!            hice, fice, tice, weasd, tskin, tprcp, stc, ep,            !
!       outputs:                                                        !
!            snwdph, qsurf, snowmt, gflux, cmm, chh, evap, hflx )       !
!                                                                       !
!  subprogram called:  ice3lay.                                         !
!                                                                       !
!  program history log:                                                 !
!         2005  --  xingren wu created  from original progtm and added  !
!                     two-layer ice model                               !
!         200x  -- sarah lu    added flag_iter                          !
!    oct  2006  -- h. wei      added cmm and chh to output              !
!         2007  -- x. wu modified for mom4 coupling (i.e. mom4ice)      !
!         2007  -- s. moorthi micellaneous changes                      !
!    may  2009  -- y.-t. hou   modified to include surface emissivity   !
!                     effect on lw radiation. replaced the confusing    !
!                     slrad with sfc net sw sfcnsw (dn-up). reformatted !
!                     the code and add program documentation block.     !
!    sep  2009 -- s. moorthi removed rcl, changed pressure units and    !
!                     further optimized                                 !
!                                                                       !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
!  inputs:                                                       size   !
!     im, km   - integer, horiz dimension and num of soil layers   1    !
!     ps       - real, surface pressure                            im   !
!     u1, v1   - real, u/v component of surface layer wind         im   !
!     t1       - real, surface layer mean temperature ( k )        im   !
!     q1       - real, surface layer mean specific humidity        im   !
!     delt     - real, time interval (second)                      1    !
!     sfcemis  - real, sfc lw emissivity ( fraction )              im   !
!     dlwflx   - real, total sky sfc downward lw flux ( w/m**2 )   im   !
!     sfcnsw   - real, total sky sfc netsw flx into ground(w/m**2) im   !
!     sfcdsw   - real, total sky sfc downward sw flux ( w/m**2 )   im   !
!     srflag   - real, snow/rain flag for precipitation            im   !
!     cm       - real, surface exchange coeff for momentum (m/s)   im   !
!     ch       - real, surface exchange coeff heat & moisture(m/s) im   !
!     prsl1    - real, surface layer mean pressure                 im   !
!     prslki   - real,                                             im   !
!     islimsk  - integer, sea/land/ice mask (=0/1/2)               im   !
!     ddvel    - real,                                             im   !
!     flag_iter- logical,                                          im   !
!     mom4ice  - logical,                                          im   !
!                                                                       !
!  input/outputs:                                                       !
!     hice     - real, sea-ice thickness                           im   !
!     fice     - real, sea-ice concentration                       im   !
!     tice     - real, sea-ice surface temperature                 im   !
!     weasd    - real, water equivalent accumulated snow depth (mm)im   !
!     tskin    - real, ground surface skin temperature ( k )       im   !
!     tprcp    - real, total precipitation                         im   !
!     stc      - real, soil temp (k)                              im,km !
!     ep       - real, potential evaporation                       im   !
!                                                                       !
!  outputs:                                                             !
!     snwdph   - real, water equivalent snow depth (mm)            im   !
!     qsurf    - real, specific humidity at sfc                    im   !
!     snowmt   - real, snow melt (m)                               im   !
!     gflux    - real, soil heat flux (w/m**2)                     im   !
!     cmm      - real,                                             im   !
!     chh      - real,                                             im   !
!     evap     - real, evaperation from latent heat flux           im   !
!     hflx     - real, sensible heat flux                          im   !
!                                                                       !
! ===================================================================== !
!
      use machine , only : kind_phys
      use funcphys, only : fpvs
      use physcons, only : sbc => con_sbc, hvap => con_hvap,            &
     &                     tgice => con_tice, cp => con_cp,             &
     &                     eps => con_eps, epsm1 => con_epsm1,          &
     &                     grav => con_g, rvrdm1 => con_fvirt,          &
     &                     t0c => con_t0c, rd => con_rd
!
      implicit none
!
!  ---  constant parameters:
      integer,              parameter :: kmi   = 2        ! 2-layer of ice
      real(kind=kind_phys), parameter :: cpinv = 1.0/cp
      real(kind=kind_phys), parameter :: hvapi = 1.0/hvap
      real(kind=kind_phys), parameter :: elocp = hvap/cp
      real(kind=kind_phys), parameter :: himax = 8.0      ! maximum ice thickness allowed
      real(kind=kind_phys), parameter :: himin = 0.1      ! minimum ice thickness required
      real(kind=kind_phys), parameter :: hsmax = 2.0      ! maximum snow depth allowed
      real(kind=kind_phys), parameter :: timin = 173.0    ! minimum temperature allowed for snow/ice
      real(kind=kind_phys), parameter :: albfw = 0.06     ! albedo for lead
      real(kind=kind_phys), parameter :: dsi   = 1.0/0.33

!  ---  inputs:
      integer, intent(in) :: im, km

      real (kind=kind_phys), dimension(im), intent(in) :: ps, u1, v1,   &
     &       t1, q1, sfcemis, dlwflx, sfcnsw, sfcdsw, srflag, cm, ch,   &
     &       prsl1, prslki, ddvel

      integer, dimension(im), intent(in) :: islimsk
      real (kind=kind_phys), intent(in) :: delt

      logical, intent(in) :: flag_iter(im), mom4ice

!  ---  input/outputs:
      real (kind=kind_phys), dimension(im), intent(inout) :: hice,      &
     &       fice, tice, weasd, tskin, tprcp, ep

      real (kind=kind_phys), dimension(im,km), intent(inout) :: stc

!  ---  outputs:
      real (kind=kind_phys), dimension(im), intent(out) :: snwdph,      &
     &       qsurf, snowmt, gflux, cmm, chh, evap, hflx

!  ---  locals:
      real (kind=kind_phys), dimension(im) :: ffw, evapi, evapw,        &
     &       hflxi, hflxw, sneti, snetw, qssi, qssw, hfd, hfi,          &
     &       focn, snof, hi_save, hs_save, psurf, q0, qs1, rch, rho,    &
     &       snowd, theta1, tv1, ps1, wind
      real (kind=kind_phys), dimension(im) :: hfw

      real (kind=kind_phys) :: cimin, t12, t14, tem, stsice(im,kmi)

      integer :: i, k
 
      logical :: flag(im)
!
!===> ...  begin here
!
!  --- ...  set minimum ice concentration required

      if (mom4ice) then
         cimin = 0.15          ! mom4ice and mask
      else
         cimin = 0.50          ! gfs only
      endif

!  --- ...  set flag for sea-ice

      do i = 1, im
         flag(i) = (islimsk(i)>=2) .and. flag_iter(i)
      enddo

      do i = 1, im
        if (flag_iter(i) .and. islimsk(i)<2) then
          hice(i) = 0.0
          fice(i) = 0.0
        endif
      enddo

      if (mom4ice) then
        do i = 1, im
          if (flag(i)) then
            hi_save(i) = hice(i)
            hs_save(i) = weasd(i) * 0.001
          endif
        enddo

!  --- ...  snow-rain detection

      else
        do i = 1, im
          if (flag(i)) then
            if (srflag(i) == 1.0) then
              ep(i) = 0.0
              weasd(i) = weasd(i) + 1.e3*tprcp(i)
              tprcp(i)  = 0.0
            endif
          endif
        enddo
      endif

!  --- ...  initialize variables. all units are supposedly m.k.s. unless specifie
!           psurf is in pascals, wind is wind speed, theta1 is adiabatic surface
!           temp from level 1, rho is density, qs1 is sat. hum. at level1 and qss
!           is sat. hum. at surface
!           convert slrad to the civilized unit from langley minute-1 k-4

      do i = 1, im
        if (flag(i)) then
          psurf(i) = 1000.0 * ps(i)
          ps1(i)   = 1000.0 * prsl1(i)

!         dlwflx has been given a negative sign for downward longwave
!         sfcnsw is the net shortwave flux (direction: dn-up)

          wind(i)   = sqrt(u1(i)*u1(i) + v1(i)*v1(i))                   &
     &              + max(0.0, min(ddvel(i), 30.0))
          wind(i)   = max(wind(i), 1.0)

          q0(i)     = max(q1(i), 1.0e-8)
!         tsurf(i)  = tskin(i)
          theta1(i) = t1(i) * prslki(i)
          tv1(i)    = t1(i) * (1.0 + rvrdm1*q0(i))
          rho(i)    = prsl1(i) / (rd*tv1(i))
          qs1(i)    = fpvs(t1(i))
          qs1(i)    = eps*qs1(i) / (prsl1(i) + epsm1*qs1(i))
          qs1(i)    = max(qs1(i), 1.e-8)
          q0 (i)    = min(qs1(i), q0(i))

          ffw(i)    = 1.0 - fice(i)
          if (fice(i) < cimin) then
            print *,'warning: ice fraction is low:', fice(i)
            fice(i) = cimin
            ffw (i) = 1.0 - fice(i)
            tice(i) = tgice
            tskin(i)= tgice
            print *,'fix ice fraction: reset it to:', fice(i)
          endif

          qssi(i) = fpvs(tice(i))
          qssi(i) = eps*qssi(i) / (ps(i) + epsm1*qssi(i))
          qssw(i) = fpvs(tgice)
          qssw(i) = eps*qssw(i) / (ps(i) + epsm1*qssw(i))

!  --- ...  snow depth in water equivalent is converted from mm to m unit

          if (mom4ice) then
            snowd(i) = weasd(i) * 0.001 / fice(i)
          else
            snowd(i) = weasd(i) * 0.001
          endif
!         flagsnw(i) = .false.

!  --- ...  when snow depth is less than 1 mm, a patchy snow is assumed and
!           soil is allowed to interact with the atmosphere.
!           we should eventually move to a linear combination of soil and
!           snow under the condition of patchy snow.
        endif
      enddo


      do i = 1, im
        if (flag(i)) then

!  --- ...  rcp = rho cp ch v

          cmm(i) = cm(i)  * wind(i)
          chh(i) = rho(i) * ch(i) * wind(i)
          rch(i) = chh(i) * cp

!  --- ...  sensible and latent heat flux over open water & sea ice

          evapi(i) = elocp * rch(i) * (qssi(i) - q0(i))
          evapw(i) = elocp * rch(i) * (qssw(i) - q0(i))
!         evap(i)  = fice(i)*evapi(i) + ffw(i)*evapw(i)
        endif
      enddo

!  --- ...  update sea ice temperature

      do k = 1, kmi
        do i = 1, im
          if (flag(i)) then
            stsice(i,k) = stc(i,k)
          endif
        enddo
      enddo

      do i = 1, im
        if (flag(i)) then
          snetw(i) = sfcdsw(i) * (1.0 - albfw)
          snetw(i) = min(3.0*sfcnsw(i)/(1.0+2.0*ffw(i)), snetw(i))
          sneti(i) = (sfcnsw(i) - ffw(i)*snetw(i)) / fice(i)

          t12 = tice(i) * tice(i)
          t14 = t12 * t12

!  --- ...  hfi = net non-solar and upir heat flux @ ice surface

          hfi(i) = -dlwflx(i) + sfcemis(i)*sbc*t14 + evapi(i)           &
     &           + rch(i)*(tice(i) - theta1(i))
          hfd(i) = 4.0*sfcemis(i)*sbc*tice(i)*t12                       &
     &           + (1.0 + elocp*eps*hvap*qs1(i)/(rd*t12)) * rch(i)

          t12 = tgice * tgice
          t14 = t12 * t12

!  --- ...  hfw = net heat flux @ water surface (within ice)

!         hfw(i) = -dlwflx(i) + sfcemis(i)*sbc*t14 + evapw(i)           &
!    &           + rch(i)*(tgice - theta1(i)) - snetw(i)

          focn(i) = 2.0     ! heat flux from ocean - should be from ocn model
          snof(i) = 0.0     ! snowfall rate - snow accumulates in gbphys

          hice(i) = max( min( hice(i), himax ), himin )
          snowd(i) = min( snowd(i), hsmax )

          if (snowd(i) > (2.0*hice(i))) then
            print *, 'warning: too much snow :',snowd(i)
            snowd(i) = 2.0 * hice(i)
            print *,'fix: decrease snow depth to:',snowd(i)
          endif
        endif
      enddo

      call ice3lay
!  ---  inputs:                                                         !
!    &     ( im, kmi, fice, flag, hfi, hfd, sneti, focn, delt,          !
!  ---  outputs:                                                        !
!    &       snowd, hice, stsice, tice, snof, snowmt, gflux )           !

      if (mom4ice) then
        do i = 1, im
          if (flag(i)) then
            hice(i)  = hi_save(i)
            snowd(i) = hs_save(i)
          endif
        enddo
      endif

      do i = 1, im
        if (flag(i)) then
          if (tice(i) < timin) then
            print *,'warning: snow/ice temperature is too low:',tice(i)
            tice(i) = timin
            print *,'fix snow/ice temperature: reset it to:',tice(i)
          endif

          if (stsice(i,1) < timin) then
            print *,'warning: layer 1 ice temp is too low:',stsice(i,1)
            stsice(i,1) = timin
            print *,'fix layer 1 ice temp: reset it to:',stsice(i,1)
          endif

          if (stsice(i,2) < timin) then
            print *,'warning: layer 2 ice temp is too low:',stsice(i,2)
            stsice(i,2) = timin
            print *,'fix layer 2 ice temp: reset it to:',stsice(i,2)
          endif

          tskin(i) = tice(i)*fice(i) + tgice*ffw(i)
        endif
      enddo

      do k = 1, kmi
        do i = 1, im
          if (flag(i)) then
            stc(i,k) = min(stsice(i,k), t0c)
          endif
        enddo
      enddo

!  --- ...  calculate sensible heat flux (& evap over sea ice)

      do i = 1, im
        if (flag(i)) then
          hflxi(i) = rch(i) * (tice(i) - theta1(i))
          hflxw(i) = rch(i) * (tgice - theta1(i))
          hflx(i)  = fice(i)*hflxi(i) + ffw(i)*hflxw(i)
          evap(i)  = fice(i)*evapi(i) + ffw(i)*evapw(i)
        endif
      enddo

!  --- ...  the rest of the output

      do i = 1, im
        if (flag(i)) then
          qsurf(i) = q1(i) + evap(i) / (elocp*rch(i))

!  --- ...  convert snow depth back to mm of water equivalent

          weasd(i)  = snowd(i) * 1000.0
          snwdph(i) = weasd(i) * dsi             ! snow depth in mm
        endif
      enddo

      do i = 1, im
        if (flag(i)) then
          tem     = 1.0 / rho(i)
          hflx(i) = hflx(i) * tem * cpinv
          evap(i) = evap(i) * tem * hvapi
        endif
      enddo
!
      return

! =================
      contains
! =================


!-----------------------------------
      subroutine ice3lay
!...................................
!  ---  inputs:
!    &     ( im, kmi, fice, flag, hfi, hfd, sneti, focn, delt,          &
!  ---  input/outputs:
!    &       snowd, hice, stsice, tice, snof,                           &
!  ---  outputs:
!    &       snowmt, gflux                                              &
!    &     )

!**************************************************************************
!                                                                         *
!            three-layer sea ice vertical thermodynamics                  *
!                                                                         *
! based on:  m. winton, "a reformulated three-layer sea ice model",       *
! journal of atmospheric and oceanic technology, 2000                     *
!                                                                         *
!                                                                         *
!        -> +---------+ <- tice - diagnostic surface temperature ( <= 0c )*
!       /   |         |                                                   *
!   snowd   |  snow   | <- 0-heat capacity snow layer                     *
!       \   |         |                                                   *
!        => +---------+                                                   *
!       /   |         |                                                   *
!      /    |         | <- t1 - upper 1/2 ice temperature; this layer has *
!     /     |         |         a variable (t/s dependent) heat capacity  *
!   hice    |...ice...|                                                   *
!     \     |         |                                                   *
!      \    |         | <- t2 - lower 1/2 ice temp. (fixed heat capacity) *
!       \   |         |                                                   *
!        -> +---------+ <- base of ice fixed at seawater freezing temp.   *
!                                                                         *
!  =====================  defination of variables  =====================  !
!                                                                         !
!  inputs:                                                         size   !
!     im, kmi  - integer, horiz dimension and num of ice layers      1    !
!     fice     - real, sea-ice concentration                         im   !
!     flag     - logical, ice mask flag                              1    !
!     hfi      - real, net non-solar and heat flux @ surface(w/m^2)  im   !
!     hfd      - real, heat flux derivatice @ sfc (w/m^2/deg-c)      im   !
!     sneti    - real, net solar incoming at top  (w/m^2)            im   !
!     focn     - real, heat flux from ocean    (w/m^2)               im   !
!     delt     - real, timestep                (sec)                 1    !
!                                                                         !
!  input/outputs:                                                         !
!     snowd    - real, surface pressure                              im   !
!     hice     - real, sea-ice thickness                             im   !
!     stsice   - real, temp @ midpt of ice levels  (deg c)          im,kmi!     
!     tice     - real, surface temperature     (deg c)               im   !
!     snof     - real, snowfall rate           (m/sec)               im   !
!                                                                         !
!  outputs:                                                               !
!     snowmt   - real, snow melt during delt   (m)                   im   !
!     gflux    - real, conductive heat flux    (w/m^2)               im   !
!                                                                         !
!  locals:                                                                !
!     hdi      - real, ice-water interface     (m)                   im   !
!     hsni     - real, snow-ice                (m)                   im   !
!                                                                         !
! ======================================================================= !
!

!  ---  constant parameters: (properties of ice, snow, and seawater)
      real (kind=kind_phys), parameter :: ds   = 330.0    ! snow (ov sea ice) density (kg/m^3)
      real (kind=kind_phys), parameter :: dw   =1000.0    ! fresh water density  (kg/m^3)
      real (kind=kind_phys), parameter :: t0c  =273.15    ! freezing temp of fresh ice (k)
      real (kind=kind_phys), parameter :: ks   = 0.31     ! conductivity of snow   (w/mk)
      real (kind=kind_phys), parameter :: i0   = 0.3      ! ice surface penetrating solar fraction
      real (kind=kind_phys), parameter :: ki   = 2.03     ! conductivity of ice  (w/mk)
      real (kind=kind_phys), parameter :: di   = 917.0    ! density of ice   (kg/m^3)
      real (kind=kind_phys), parameter :: ci   = 2054.0   ! heat capacity of fresh ice (j/kg/k)
      real (kind=kind_phys), parameter :: li   = 3.34e5   ! latent heat of fusion (j/kg-ice)
      real (kind=kind_phys), parameter :: si   = 1.0      ! salinity of sea ice
      real (kind=kind_phys), parameter :: mu   = 0.054    ! relates freezing temp to salinity
      real (kind=kind_phys), parameter :: tfi  = -mu*si   ! sea ice freezing temp = -mu*salinity
      real (kind=kind_phys), parameter :: tfw  = -1.8     ! tfw - seawater freezing temp (c)
      real (kind=kind_phys), parameter :: tfi0 = tfi-0.0001
      real (kind=kind_phys), parameter :: dici = di*ci 
      real (kind=kind_phys), parameter :: dili = di*li 
      real (kind=kind_phys), parameter :: dsli = ds*li 
      real (kind=kind_phys), parameter :: ki4  = ki*4.0

!  ---  inputs:
!     integer, intent(in) :: im, kmi

!     real (kind=kind_phys), dimension(im), intent(in) :: fice, hfi,    &
!    &       hfd, sneti, focn

!     real (kind=kind_phys), intent(in) :: delt

!     logical, dimension(im), intent(in) :: flag

!  ---  input/outputs:
!     real (kind=kind_phys), dimension(im), intent(inout) :: snowd,     &
!    &       hice, tice, snof

!     real (kind=kind_phys), dimension(im,kmi), intent(inout) :: stsice

!  ---  outputs:
!     real (kind=kind_phys), dimension(im), intent(out) :: snowmt,      &
!    &       gflux

!  ---  locals:
      real (kind=kind_phys), dimension(im) :: hdi, hsni, a, b, ip,      &
     &      a1, b1, c1, a10, b10, k12, k32, h1, h2, dh, f1, tsf,        &
     &      tmelt, bmelt

      real (kind=kind_phys) :: dt2, dt4, dt6

      integer :: i
!
!===> ...  begin here
!
      dt2 = 2.0 * delt
      dt4 = 4.0 * delt
      dt6 = 6.0 * delt

      do i = 1, im
        if (flag(i)) then
          snowd(i) = snowd(i) * dw / ds
          hdi(i) = (ds*snowd(i) + di*hice(i)) / dw

          if (hice(i) < hdi(i)) then
            snowd(i) = snowd(i) + hice(i) - hdi(i)
            hsni (i) = (hdi(i) - hice(i)) * ds / di
            hice (i) = hice(i) + hsni(i)
          endif

          snof(i) = snof(i) * dw / ds
          tice(i) = tice(i) - t0c
          stsice(i,1) = min(stsice(i,1)-t0c, tfi0)     ! degc
          stsice(i,2) = min(stsice(i,2)-t0c, tfi0)     ! degc

          ip(i) = i0 * sneti(i)         ! ip +v (in winton ip=-i0*sneti as sol -v)
          if (snowd(i) > 0.0) then
            tsf(i) = 0.0
            ip (i) = 0.0
          else
            tsf(i) = tfi
            ip (i) = i0 * sneti(i)      ! ip +v here (in winton ip=-i0*sneti)
          endif
          tice(i) = min(tice(i), tsf(i))

!  --- ...  compute ice temperature

          b(i) = hfd(i)
          a(i) = hfi(i) - sneti(i) + ip(i) - tice(i)*b(i)  ! +v sol input here
          k12(i) = ki4*ks / (ks*hice(i) + ki4*snowd(i))
          k32(i) = 2.0 * ki / hice(i)

          a10(i) = dici*hice(i)/dt2 + k32(i)*(dt4*k32(i) + dici*hice(i))&
     &           / (dt6*k32(i) + dici*hice(i))
          b10(i) = -di*hice(i) * (ci*stsice(i,1) + li*tfi/stsice(i,1))  &
     &           / dt2 - ip(i)                                          &
     &           - k32(i)*(dt4*k32(i)*tfw + dici*hice(i)*stsice(i,2))   &
     &           / (dt6*k32(i) + dici*hice(i))

          a1(i) = a10(i) + k12(i)*b(i) / (k12(i) + b(i))
          b1(i) = b10(i) + a(i)*k12(i) / (k12(i) + b(i))
          c1(i) = dili*tfi / dt2*hice(i)

          stsice(i,1) = -(sqrt(b1(i)*b1(i) - 4.0*a1(i)*c1(i))           &
     &                + b1(i))/(2.0*a1(i))
          tice(i) = (k12(i)*stsice(i,1) - a(i)) / (k12(i) + b(i))

          if (tice(i) > tsf(i)) then
            a1(i) = a10(i) + k12(i)
            b1(i) = b10(i) - k12(i)*tsf(i)
            stsice(i,1) = -(sqrt(b1(i)*b1(i) - 4.0*a1(i)*c1(i))         &
     &                  + b1(i)) / (2.0*a1(i))
            tice(i) = tsf(i)
            tmelt(i) = (k12(i)*(stsice(i,1) - tsf(i))                   &
     &               - (a(i) + b(i)*tsf(i))) * delt
          else
            tmelt(i) = 0.0
            snowd(i) = snowd(i) + snof(i)*delt
          endif

          stsice(i,2) = (dt2*k32(i)*(stsice(i,1) + 2.0*tfw)             &
     &                + dici*hice(i)*stsice(i,2))                       &
     &                / (dt6*k32(i) + dici*hice(i))

          bmelt(i) = (focn(i) + ki4*(stsice(i,2) - tfw)/hice(i)) * delt

!  --- ...  resize the ice ...

          h1(i) = 0.5 * hice(i)
          h2(i) = 0.5 * hice(i)

!  --- ...  top ...

          if (tmelt(i) <= snowd(i)*dsli) then
            snowmt(i) = tmelt(i) / dsli
            snowd (i) = snowd(i) - tmelt(i)/dsli
          else
            snowmt(i) = snowd(i)
            h1(i) = h1(i) - (tmelt(i) - snowd(i)*dsli)                  &
     &            / (di * (ci - li/stsice(i,1)) * (tfi - stsice(i,1)))
            snowd(i) = 0.0
          endif

!  --- ...  and bottom

          if (bmelt(i) < 0.0) then
            dh(i) = -bmelt(i) / (dili + dici*(tfi - tfw))
            stsice(i,2) = (h2(i)*stsice(i,2) + dh(i)*tfw)               &
     &                  / (h2(i) + dh(i))
            h2(i) = h2(i) + dh(i)
          else
            h2(i) = h2(i) - bmelt(i) / (dili + dici*(tfi - stsice(i,2)))
          endif

!  --- ...  if ice remains, even up 2 layers, else, pass negative energy back in snow

          hice(i) = h1(i) + h2(i)

          if (hice(i) > 0.0) then
            if (h1(i) > 0.5*hice(i)) then
              f1(i) = 1.0 - 2.0*h2(i) / hice(i)
              stsice(i,2) = f1(i)                                       &
     &                    * (stsice(i,1) + li*tfi/(ci*stsice(i,1)))     &
     &                    + (1.0 - f1(i))*stsice(i,2)

              if (stsice(i,2) > tfi) then
                hice(i) = hice(i) - h2(i)*ci*(stsice(i,2) - tfi)        &
     &                  / (li*delt)
                stsice(i,2) = tfi
              endif
            else
              f1(i) = 2.0 * h1(i) / hice(i)
              stsice(i,1) = f1(i)                                       &
     &                    * (stsice(i,1) + li*tfi/(ci*stsice(i,1)))     &
     &                    + (1.0 - f1(i))*stsice(i,2)
              stsice(i,1) = (stsice(i,1) - sqrt(stsice(i,1)*stsice(i,1) &
     &                    - 4.0*tfi*li/ci)) / 2.0
            endif

            k12(i) = ki4*ks / (ks*hice(i) + ki4*snowd(i))
            gflux(i) = k12(i) * (stsice(i,1) - tice(i))
          else
            snowd(i) = snowd(i) + (h1(i)*(ci*(stsice(i,1) - tfi)        &
     &               - li*(1.0 - tfi/stsice(i,1)))                      &
     &               + h2(i)*(ci*(stsice(i,2) - tfi) - li)) / li

            hice(i) = max(0.0, snowd(i)*ds/di)
            snowd(i) = 0.0
            stsice(i,1) = tfw
            stsice(i,2) = tfw
            gflux(i) = 0.0
          endif   ! end if_hice_block

          gflux(i) = fice(i) * gflux(i)
          snowmt(i) = snowmt(i) * ds / dw
          snowd(i) = snowd(i) * ds / dw
          tice(i) = tice(i) + t0c
          stsice(i,1) = stsice(i,1) + t0c
          stsice(i,2) = stsice(i,2) + t0c
        endif   ! end if_flag_block
      enddo   ! end do_i_loop

      return
!...................................
      end subroutine ice3lay
!-----------------------------------

! =========================== !
!     end contain programs    !
! =========================== !

!...................................
      end subroutine sfc_sice
!-----------------------------------
