!-----------------------------------
      subroutine sfc_land                                               &
!...................................
!  ---  inputs:
     &     ( im, km, ps, u1, v1, t1, q1, smc, soiltyp,                  &
     &       sigmaf, vegtype, sfcemis, dlwflx, swnet, delt,             &
     &             tg3, cm, ch, prsl1, prslki, islimsk,                 &
!    &       zorl, tg3, cm, ch, prsl1, prslki, islimsk,                 &
     &       ddvel, flag_iter, flag_guess,                              &
!  ---  input/outputs:
     &       weasd, tskin, tprcp, srflag, stc, canopy, tsurf,           & 
!  ---  outputs:
     &       qsurf, snowmt, gflux, zsoil, rhscnpy, rhsmc,               &
     &       aim, bim, cim, drain, evap, hflx, ep, cmm, chh,            &
     &       evbs, evcw, trans, sbsno, snowc, stm, snohf,               &
     &       twilt, tref  
     &     )

! ===================================================================== !
!  description:  osu land surface model                                 !
!                                                                       !
!  usage:                                                               !
!                                                                       !
!    call sfc_land                                                      !
!       inputs:                                                         !
!          ( im, km, ps, u1, v1, t1, q1, smc, soiltyp,                  !
!            sigmaf, vegtype, sfcemis, dlwflx, swnet, delt,             !
!            zorl, tg3, cm, ch, prsl1, prslki, islimsk,                  !
!            ddvel, flag_iter, flag_guess,                              !
!       input/outputs:                                                  !
!            weasd, tskin, tprcp, srflag, stc, canopy, tsurf,           !
!       outputs:                                                        !
!            qsurf, snowmt, gflux, zsoil, rhscnpy, rhsmc,               !
!            aim, bim, cim, drain, evap, hflx, ep, cmm, chh,            !
!            evbs, evcw, trans, sbsno, snowc, stm, snohf,               !
!            twilt, tref )                                              !
!                                                                       !
!  subprograms called: none                                             !
!                                                                       !
!                                                                       !
!  program history log:                                                 !
!         xxxx  -- original version created from Hula Lu's progtm       !                                  !
!         200x  -- sarah lu    modified (need description)              !
!    oct  2006  -- h. wei      modified (need description)              !
!    apr  2009  -- y.-t. hou   modified to include surface emissivity   !
!                     effect on lw radiation. also replaced slrad (a    !
!                     confussing term) with sfc net sw flux swnet       !
!                     that is redefined as (du-up). rewrite the code    !
!                     and add program documentation block.              !
!    sep  2009  -- s. moorthi some additional modification              !
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
!     smc      - real, soil moisture content (fractional)        im,km  !
!     soiltyp  - integer, soil type (integer index)                im   !
!     sigmaf   - real, areal fractional cover of green vegetation  im   !
!     vegtype  - integer, vegetation type (integer index)          im   !
!     sfcemis  - real, sfc lw emissivity ( fraction )              im   !
!     dlwflx   - real, total sky sfc downward lw flux ( w/m**2 )   im   !
!     swnet    - real, total sky sfc netsw flx into ground(w/m**2) im   !
!     delt     - real, time interval (second)                      1    !
!     zorl     - real, surface roughness                           im   !
!     tg3      - real, deep soil temperature                       im   !
!     cm       - real, surface exchange coeff for momentum (m/s)   im   !
!     ch       - real, surface exchange coeff heat & moisture(m/s) im   !
!     prsl1    - real, surface layer mean pressure                 im   !
!     prslki   - real,                                             im   !
!     islimsk  - integer, sea/land/ice mask (=0/1/2)               im   !
!     ddvel    - real,                                             im   !
!     flag_iter- logical,                                          im   !
!     flag_guess-logical,                                          im   !
!                                                                       !
!  input/outputs:                                                       !
!     weasd    - real, water equivalent accumulated snow depth (mm)im   !
!     tskin    - real, ground surface skin temperature ( k )       im   !
!     tprcp    - real, total precipitation                         im   !
!     srflag   - real, snow/rain flag for precipitation            im   !
!     stc      - real, soil temp (k)                              im,km !
!     canopy   - real, canopy moisture content (m)                 im   !
!     tsurf    - real, surface skin temperature (after iteration)  im   !
!                                                                       !
!  outputs:                                                             !
!     qsurf    - real, specific humidity at sfc                    im   !
!     snowmt   - real, snow melt (m)                               im   !
!     gflux    - real, soil heat flux (w/m**2)                     im   !
!     zsoil    - real, soil depth                                 im,km !
!     rhscnpy  - real,                                             im   !
!     rhsmc    - real,                                            im,km !
!     aim      - real, tridiagonal matrix coeff for soil moist    im,km !
!     bim      - real, tridiagonal matrix coeff for soil moist    im,km !
!     cim      - real, tridiagonal matrix coeff for soil moist    im,km !
!     drain    - real, subsurface runoff                           im   !
!     evap     - real, evaperation from latent heat flux           im   !
!     hflx     - real, sensible heat flux                          im   !
!     ep       - real, potential evaporation                       im   !
!     cmm      - real,                                             im   !
!     chh      - real,                                             im   !
!     evbs     - real, direct soil evaporation (m/s)               im   !
!     evcw     - real, canopy water evaporation (m/s)              im   !
!     trans    - real,                                             im   !
!     sbsno    - real, sublimation/deposit from snopack (m/s)      im   !
!     snowc    - real, fractional snow cover                       im   !
!     stm      - real, total soil column moisture content (m)      im   !
!     snohf    - real, snow/freezing-rain latent heat flux (w/m**2)im   !
!     twilt    - real, dry soil moisture threshold                 im   !
!     tref     - real, soil moisture threshold                     im   !
!                                                                       !
!  ====================    end of description    =====================  !
!
      use machine , only : kind_phys
      use funcphys, only : fpvs
      use physcons, only : grav => con_g, sbc => con_sbc, cp => con_cp, &
     &                     hvap => con_hvap, hfus => con_hfus,          &
     &                     eps => con_eps, epsm1 => con_epsm1,          &
     &                     t0c => con_t0c, rvrdm1 => con_fvirt,         &
     &                     rd => con_rd
!
      implicit none
!
!  ---  constant parameters:
      real (kind=kind_phys), parameter :: cpinv  = 1.0/cp
      real (kind=kind_phys), parameter :: hvapi  = 1.0/hvap
      real (kind=kind_phys), parameter :: elocp  = hvap/cp
      real (kind=kind_phys), parameter :: dfsnow = 0.31
      real (kind=kind_phys), parameter :: ch2o   = 4.2e6
      real (kind=kind_phys), parameter :: csoil  = 1.26e6
      real (kind=kind_phys), parameter :: scanop = 0.5
      real (kind=kind_phys), parameter :: cfactr = 0.5
      real (kind=kind_phys), parameter :: zbot   =-3.0
      real (kind=kind_phys), parameter :: topt   = 298.0
      real (kind=kind_phys), parameter :: rhoh2o = 1000.0
      real (kind=kind_phys), parameter :: ctfil1 = 0.5
      real (kind=kind_phys), parameter :: ctfil2 = 1.0-ctfil1
      real (kind=kind_phys), parameter :: snomin = 1.0e-9

!  ---  input:
      integer, intent(in) :: im, km
      integer, dimension(im), intent(in) :: islimsk, soiltyp, vegtype

      real (kind=kind_phys), dimension(im),   intent(in) :: ps, u1, v1, &
     &      t1, q1, sigmaf, sfcemis, dlwflx, swnet,       tg3, cm, ch,  &
!    &      t1, q1, sigmaf, sfcemis, dlwflx, swnet, zorl, tg3, cm, ch,  &
     &      prsl1, prslki, ddvel

      real (kind=kind_phys), dimension(im,km), intent(in) :: smc

      real (kind=kind_phys), intent(in) :: delt

      logical, intent(in) :: flag_iter(im), flag_guess(im)

!  ---  in/out:
      real (kind=kind_phys), dimension(im),    intent(inout) :: weasd,  &
     &       tskin, tprcp, srflag, canopy, tsurf

      real (kind=kind_phys), dimension(im,km), intent(inout) :: stc

!  ---  output:
      real (kind=kind_phys), dimension(im),    intent(out) :: qsurf,    &
     &       snowmt, gflux, rhscnpy, drain, evap, hflx, ep, chh, cmm,   &
     &       evbs, evcw, trans, sbsno, snowc, stm, snohf, twilt,        &
     &       tref

      real (kind=kind_phys), dimension(im,km), intent(out) :: zsoil,    &
     &       rhsmc, aim, bim, cim

!  ---  external functions:
      real (kind=kind_phys) :: funcdf, funckt, ktsoil

!  ---  locals:
      real (kind=kind_phys), dimension(im) :: weasd_old, tprcp_old,     &
     &       srflag_old, tskin_old, canopy_old, wind, canfac, ddz,      &
     &       ddz2, delta, dew, df1, dft0, dft1, dft2, dmdz, dmdz2,      &
     &       dtdz1, dtdz2, ec, edir, etpfac, factsnw, fx, gx, hcpct,    &
     &       partlnd, q0, qs1, qss, rcap, rch, rho, rs, rsmall,         &
     &       slwd, smcz, snoevp, snowd, term1, term2, theta1, tv1,      &
     &       tsea, xx, yy, zz, kt1, kt2


      real (kind=kind_phys), dimension(im,km) :: stc_old, et, stsoil,   &
     &       ai, bi, ci, rhstc

      real (kind=kind_phys) :: bfact, cc, delt2, df2, eth, ff, g, rcq,  &
     &       rcs, rct, rsi, rss, smcdry, t12, t14, tflx, tem

      integer :: i, k

      logical :: flag(im), flagsnw(im)

!  ---  local data arrays:
!     the 13 vegetation types are:
!      1  ...  broadleave-evergreen trees (tropical forest)
!      2  ...  broadleave-deciduous trees
!      3  ...  broadleave and needle leave trees (mixed forest)
!      4  ...  needleleave-evergreen trees
!      5  ...  needleleave-deciduous trees (larch)
!      6  ...  broadleave trees with groundcover (savanna)
!      7  ...  groundcover only (perenial)
!      8  ...  broadleave shrubs with perenial groundcover
!      9  ...  broadleave shrubs with bare soil
!     10  ...  dwarf trees and shrubs with ground cover (trunda)
!     11  ...  bare soil
!     12  ...  cultivations (use parameters from type 7)
!     13  ...  glacial

      real(kind=kind_phys),dimension(13), save :: rsmax, rsmin, rgl, hs

      data rsmax / 13*5000.0 /
      data rsmin / 150., 100., 125., 150., 100.,  70.,  40.,            &
     &             300., 400., 150., 999., 040., 999. /

      data rgl   / 5*30., 65., 4*100., 999., 100., 999. /

      data hs    / 41.69, 54.53, 51.93, 47.35, 47.35, 54.53, 36.35,     &
     &             3*42.00,      999.0, 36.35, 999.0 /

      real(kind=kind_phys), dimension(9), save :: smdry, smref,  smwlt
      data smdry / .07, .14, .22, .08, .18, .16, .12, .10, .07  /
      data smref / .283,.387,.412,.312,.338,.382,.315,.329,.283 /
      data smwlt / .029,.119,.139,.047,.010,.103,.069,.066,.029 /
!
!===> ...  begin here
!
      delt2 = delt * 2.0

!  --- ...  set default flag for land

      do i = 1, im
        flag(i) = ( islimsk(i) == 1 )
      enddo

!  --- ...  save land-related prognostic fields for guess run

      do i = 1, im
        if (flag(i) .and. flag_guess(i)) then
          weasd_old(i)  = weasd(i)
          tskin_old (i) = tskin(i)
          canopy_old(i) = canopy(i)
          tprcp_old (i) = tprcp(i)
          srflag_old(i) = srflag(i)

          do k = 1, km
           stc_old(i,k) = stc(i,k)
          enddo
        endif
      enddo

!  --- ...  initialize variables. all units are supposedly m.k.s. unless
!           specifie ps is in pascals
!           wind is wind speed, theta1 is adiabatic surface temp from
!           level 1, rho is density, qs1 is sat. hum. at level1 and qss
!           is sat. hum. at surface
!           surface roughness length is converted to m from cm
!           net sw flux swnet is dn-up, and dlw is positive dnwd

!     qs1 = fpvs(t1)
!     qss = fpvs(tskin)

      do i = 1, im

        if (flag_iter(i) .and. flag(i)) then
          slwd(i)  = swnet(i) + dlwflx(i)

          wind(i) = sqrt(u1(i)*u1(i) + v1(i)*v1(i))                     &
     &            + max(0.0, min(ddvel(i), 30.0))
          wind(i) = max(wind(i), 1.0)

          q0(i) = max(q1(i), 1.e-8)
!         tsurf(i) = tskin(i)
          tsea(i) = tsurf(i)
          theta1(i) = t1(i) * prslki(i)
          tv1(i)    = t1(i) * (1.0 + rvrdm1*q0(i))
!         thv1(i)   = theta1(i) * (1.0 + rvrdm1*q0(i))
!         tvs(i)    = tsea(i) * (1.0 + rvrdm1*q0(i))
          rho(i)    = prsl1(i) / (rd * tv1(i))

          qs1(i) = fpvs(t1(i))
          qs1(i) = eps*qs1(i) / (prsl1(i) + epsm1*qs1(i))
          qs1(i) = max(qs1(i), 1.e-8)
          q0(i)  = min(qs1(i), q0(i))

          qss(i) = fpvs(tskin(i))              !!! change to tsurf?
          qss(i) = eps*qss(i) / (ps(i) + epsm1*qss(i))

!         rs = plantr
          rs(i) = 0.
          if (vegtype(i) > 0.0) rs(i) = rsmin(vegtype(i))

          canopy(i)  = max(canopy(i), 0.0)
          factsnw(i) = 10.0

!  --- ...  snow depth in water equivalent is converted from mm to m unit

          snowd(i)   = weasd(i) * 0.001
          flagsnw(i) = .false.

!  --- ...  when snow depth is less than 1 mm, a patchy snow is assumed
!           and soil is allowed to interact with the atmosphere.
!           we should eventually move to a linear combination of soil and
!           snow under the condition of patchy snow.

          if (snowd(i)>0.001 .or. islimsk(i) == 2) rs(i) = 0.0
          if (snowd(i)>0.001) flagsnw(i) = .true.
        endif   ! end if_flag_iter_block

      enddo   ! end do_i_loop

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          zsoil(i,1) = -0.10
       
          do k = 2, km
            zsoil(i,k) = zsoil(i,k-1) + (-2.0 - zsoil(i,1)) / (km - 1)
          enddo

!  --- ...  wei: use the same soil layer structure as noah if running with 4-layer

          if (km > 0.2)then
            zsoil(i,2) = -0.4
            zsoil(i,3) = -1.0
            zsoil(i,4) = -2.0
          endif
        endif
      enddo

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          drain(i) = 0.0
        endif
      enddo

      do k = 1, km
        do i = 1, im
          if (flag_iter(i) .and. flag(i)) then
            et   (i,k)  = 0.0
            rhsmc(i,k)  = 0.0
            aim  (i,k)  = 0.0
            bim  (i,k)  = 1.0
            cim  (i,k)  = 0.0
            stsoil(i,k) = stc(i,k)
          endif
        enddo
      enddo

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          edir(i) = 0.0
          ec  (i) = 0.0
          evap(i) = 0.0
          hflx(i) = 0.0
          ep  (i) = 0.0
          fx  (i) = 0.0

          snowmt(i) = 0.0
          gflux (i) = 0.0
          rhscnpy(i)= 0.0
          etpfac(i) = 0.0
          canfac(i) = 0.0

          evbs (i) = 0.0
          evcw (i) = 0.0
          trans(i) = 0.0
          sbsno(i) = 0.0
          snowc(i) = 0.0
          snohf(i) = 0.0
        endif
      enddo

!  --- ...  rcp = rho cp ch v

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          rch(i) = rho(i) * cp * ch(i) * wind(i)

          cmm(i) = cm(i) * wind(i)
          chh(i) = rho(i) * ch(i) * wind(i)
        endif
      enddo

!  --- ...  compute soil/snow/ice heat flux in preparation for surface 
!           energy balance calculation

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          smcz(i) = 0.5 * (smc(i,1) + 0.20)
          dft0(i) = ktsoil(smcz(i), soiltyp(i))
        endif
      enddo

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          if (flagsnw(i)) then

!  --- ...  when snow covered, ground heat flux comes from snow

            tflx     = min(t1(i), tsea(i))
            gflux(i) = -dfsnow * (tflx - stsoil(i,1))                   &
     &               / (factsnw(i) * max(snowd(i), 0.001))
          else

            gflux(i) = dft0(i) * (stsoil(i,1) - tsea(i))                &
     &               / (-0.5 * zsoil(i,1))

          endif

          gflux(i) = max(gflux(i), -200.0)
          gflux(i) = min(gflux(i),  200.0)
        endif
      enddo

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          partlnd(i) = 1.0

          if (snowd(i)>0.0 .and. snowd(i)<=0.001) then
            partlnd(i) = 1.0 - snowd(i) / 0.001
          endif
        endif
      enddo

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          snoevp(i) = 0.0
          if (snowd(i) > 0.001) partlnd(i) = 0.0
        endif
      enddo

!  --- ...  compute potential evaporation for land and sea ice

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          t12 = t1(i) * t1(i)
          t14 = t12 * t12

!  --- ...  rcap = fnet - sigma t**4 + gflx - rho cp ch v (t1-theta1)

          rcap(i) = slwd(i) - sfcemis(i)*sbc*t14 + gflux(i)             &
     &            - rch(i)*(t1(i) - theta1(i))

!  --- ...  rsmall = 4 sigma t**3 / rch(i) + 1

          rsmall(i) = 4.0*sfcemis(i)*sbc*t1(i)*t12 / rch(i) + 1.0

!  --- ...  delta = l / cp * dqs/dt

          delta(i) = elocp*eps*hvap*qs1(i) / (rd*t12)

!  --- ...  potential evapotranspiration ( watts / m**2 ) and
!           potential evaporation

          term1(i) = elocp*rsmall(i)*rch(i) * (qs1(i) - q0(i))
          term2(i) = rcap(i) * delta(i)
          ep(i)    = (elocp*rsmall(i)*rch(i) * (qs1(i) - q0(i))         &
     &             + rcap(i)*delta(i))
          ep(i)    = ep(i) / (rsmall(i) + delta(i))
        endif
      enddo

!  --- ...  actual evaporation over land in three parts : edir, et, and ec
!           direct evaporation from soil, the unit goes from m s-1 to kg m-2 s-1

      do i = 1, im
        flag(i) = (islimsk(i) == 1) .and. (ep(i) > 0.0)
      enddo

      do i = 1, im
        if (flag_iter(i))then
          if (flag(i)) then
            df1(i) = funcdf(smc(i,1),soiltyp(i))
            kt1(i) = funckt(smc(i,1),soiltyp(i))
          endif

          if (flag(i) .and. stc(i,1)<t0c) then
            df1(i) = 0.0
            kt1(i) = 0.0
          endif

          if (flag(i)) then
!           tref = .75 * thsat(soiltyp(i))
            tref(i) = smref(soiltyp(i))
!           twilt = twlt(soiltyp(i))
            twilt(i) = smwlt(soiltyp(i))
            smcdry = smdry(soiltyp(i))
!           fx(i)  = -2.0*df1(i)*(smc(i,1) - 0.23) / zsoil(i,1) - kt1(i)
            fx(i)  = -2.0*df1(i)*(smc(i,1) - smcdry)/zsoil(i,1) - kt1(i)
            fx(i)  = max( min(fx(i), ep(i)/hvap), 0.0)

!  --- ...  sigmaf is the fraction of area covered by vegetation

            edir(i) = fx(i) * (1.0 - sigmaf(i)) * partlnd(i)
          endif
        endif
      enddo

!  --- ...  calculate stomatal resistance

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then

!  --- ...  resistance due to par. we use net solar flux as proxy at the present time

          ff  = 0.55*2.0*swnet(i) / rgl(vegtype(i))
          rcs = (ff + rs(i)/rsmax(vegtype(i))) / (1.0 + ff)
          rcs = max(rcs, 0.0001)
          rct = 1.0
          rcq = 1.0

!  --- ...  resistance due to thermal effect

!         rct = 1.0 - 0.0016 * (topt - theta1)**2
!         rct = max(rct, 0.0001)

!  --- ...  resistance due to humidity

!         rcq = 1.0 / (1.0 + hs(vegtype(i)) * (qs1(i) - q0(i)))
!         rcq = max(rcq, 0.0001)

!  --- ...  compute resistance without the effect of soil moisture

          rs(i) = rs(i) / (rcs*rct*rcq)
        endif
      enddo

!  --- ...  transpiration from all levels of the soil

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          canfac(i) = (canopy(i)/scanop) ** cfactr
          etpfac(i) = sigmaf(i) * (1.0 - canfac(i)) / hvap
          gx(i) = (smc(i,1) - twilt(i)) / (tref(i) - twilt(i))
          gx(i) = max( min(gx(i), 1.0), 0.0)

!  --- ...  resistance due to soil moisture deficit

          rss = gx(i) * (zsoil(i,1) / zsoil(i,km))
          rss = max(rss, 0.0001)
          rsi = rs(i) / rss

!  --- ...  transpiration a la monteith

          eth = (term1(i) + term2(i))                                   &
     &        / (delta(i) + rsmall(i)*(1.0 + rsi*ch(i)*wind(i)))
          et(i,1) = etpfac(i) * eth * partlnd(i)
        endif
      enddo

      do k = 2, km
        do i = 1, im
          if (flag_iter(i) .and. flag(i)) then
            gx(i) = (smc(i,k) - twilt(i)) / (tref(i) - twilt(i))
            gx(i) = max( min(gx(i), 1.0), 0.0)

!  --- ...  resistance due to soil moisture deficit

          rss = gx(i) * ((zsoil(i,k) - zsoil(i,k-1))/zsoil(i,km))
          rss = max(rss, 1.e-6)
          rsi = rs(i) / rss

!  --- ...  transpiration a la monteith

          eth = (term1(i) + term2(i))                                   &
     &        / (delta(i) + rsmall(i) * (1.0 + rsi*ch(i)*wind(i)))
            et(i,k) = eth * etpfac(i) * partlnd(i)
          endif
        enddo
      enddo

!  --- ...  canopy re-evaporation

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          ec(i) = sigmaf(i)*canfac(i)*ep(i) / hvap
          ec(i) = ec(i) * partlnd(i)
          ec(i) = min(ec(i), canopy(i)/delt)
        endif
      enddo

!  --- ...  sum up total evaporation

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
         evap(i) = edir(i) + ec(i)
        endif
      enddo

      do k = 1, km
        do i = 1, im
          if (flag_iter(i) .and. flag(i)) then
            evap(i) = evap(i) + et(i,k)
          endif
        enddo
      enddo

!  --- ...  return evap unit from kg m-2 s-1 to watts m-2

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          evap(i) = min(evap(i)*hvap, ep(i))
        endif
      enddo

!  --- ...  treat downward moisture flux situation (evap was preset to
!           zero so no update needed) dew is converted from kg m-2 to m
!           to conform to precip unit

      do i = 1, im
        flag(i) = (islimsk(i) == 1 .and. ep(i) <= 0)
        dew(i) = 0.0
      enddo

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          dew(i) = -ep(i)*delt / (hvap*rhoh2o)
          evap(i) = ep(i)
          dew(i) = dew(i) * partlnd(i)
          evap(i) = evap(i) * partlnd(i)
        endif
      enddo

!  --- ...  snow covered land 

      do i = 1, im
        flag(i) = (islimsk(i) == 1 .and. snowd(i) > 0)
      enddo

!  --- ...  change of snow depth due to evaporation or sublimation
!           convert evap from kg m-2 s-1 to m s-1 to determine the reduction of s

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          bfact = snowd(i) / (delt*ep(i) / (hvap*rhoh2o))
          bfact = min(bfact, 1.0)

!  --- ...  the evaporation of snow

          if (ep(i) <= 0.0) bfact = 1.0

          if (snowd(i) <= 0.001) then
!           evap = (snowd(i)/0.001) * bfact*ep(i) + evap
!           snoevp(i) = bfact*ep(i) * (1.0 - partlnd(i))
!           evap = evap + snoevp(i)
            snoevp(i) = bfact * ep(i)
!           evap   = evap + snoevp(i) * (1.0 - partlnd(i))
            evap(i) = evap(i) + snoevp(i) * (1.0 - partlnd(i))
          else
!           evap(i) = bfact * ep(i)
            snoevp(i) = bfact * ep(i)
            evap(i) = snoevp(i)
          endif

          tsea(i) = t1(i)                                               &
     &            + (rcap(i) - gflux(i) - dfsnow*(t1(i) - stsoil(i,1))  &
     &            / (factsnw(i) * max(snowd(i), 0.001))                 &
!    &            + theta1 - t1                                         &
!    &           - bfact * ep(i)) / (rsmall(i) * rch(i)                 &
     &           - snoevp(i)) / (rsmall(i) * rch(i)                     &
     &           + dfsnow / (factsnw(i) * max(snowd(i), 0.001)))

!         snowd(i) = snowd(i) - bfact*ep(i)*delt / (rhoh2o*hvap)
          snowd(i) = snowd(i) - snoevp(i)*delt / (rhoh2o*hvap)
          snowd(i) = max(snowd(i), 0.0)
        endif
      enddo

!  --- ...  snow melt (m)

      do i = 1, im
        flag(i) = (islimsk(i) == 1) .and. (snowd(i) > 0)
      enddo

      do i = 1, im
        if (flag_iter(i)) then
          if (flag(i) .and. tsea(i)>t0c) then
            snowmt(i) = rch(i)*rsmall(i)*delt * (tsea(i) - t0c)         &
     &                / (rhoh2o*hfus)
            snowmt(i) = min(snowmt(i), snowd(i))
            snowd(i) = snowd(i) - snowmt(i)
            snowd(i) = max(snowd(i), 0.0)
            tsea (i) = max(t0c, tsea(i) - hfus*snowmt(i)*rhoh2o         &
     &               / (rch(i)*rsmall(i)*delt))
          endif
        endif
      enddo

!  --- ...  we need to re-evaluate evaporation because of snow melt
!           the skin temperature is now bounded to 0 deg c

!     qss = fpvs(tsea)
      do i = 1, im
        flag(i) = (islimsk(i) == 1)

        if (flag_iter(i) .and. flag(i))then
!         if (snowd(i) > 0.0) then
          if (snowd(i) > snomin) then
!jfe        qss(i) = 1000.0 * fpvs(tsea(i))
            qss(i) = fpvs(tsea(i))
            qss(i) = eps*qss(i) / (ps(i) + epsm1*qss(i))
            evap(i) = elocp*rch(i) * (qss(i) - q0(i))
          endif
        endif
      enddo

!  --- ...  prepare tendency terms for the soil moisture field without
!           precipitat. the unit of moisture flux needs to become m s-1
!           for soil moisture. hence the factor of rhoh2o

      do i = 1, im
        if (flag_iter(i)) then
          if (flag(i)) then
            df1(i) = funcdf(smcz(i),soiltyp(i))
            kt1(i) = funckt(smcz(i),soiltyp(i))
          endif

          if (flag(i) .and. stc(i,1)<t0c) then
            df1(i) = 0.0
            kt1(i) = 0.0
          endif

          if (flag(i)) then
            rhscnpy(i) = -ec(i) + sigmaf(i)*rhoh2o*dew(i) / delt
            smcz(i) = max(smc(i,1), smc(i,2))
            dmdz(i) = (smc(i,1) - smc(i,2)) / (-0.5*zsoil(i,2))
            rhsmc(i,1) = (df1(i)*dmdz(i) + kt1(i)                       &
     &                 + (edir(i) + et(i,1))) / (zsoil(i,1)*rhoh2o)
            rhsmc(i,1) = rhsmc(i,1) - (1.0 - sigmaf(i))*dew(i)          &
     &                 / (zsoil(i,1)*delt)
            ddz(i) = 1.0 / (-0.5*zsoil(i,2))

!  --- ...  aim, bim, and cim are the elements of the tridiagonal matrix for
!           the implicit update of the soil moisture

            aim(i,1) = 0.0
            bim(i,1) = df1(i)*ddz(i) / (-zsoil(i,1)*rhoh2o)
            cim(i,1) = -bim(i,1)
          endif
        endif
      enddo

      do k = 2, km
        if (k < km) then

          do i = 1, im
            if (flag_iter(i)) then
              if (flag(i)) then
                df2 = funcdf(smcz(i),soiltyp(i))
                kt2(i) = funckt(smcz(i),soiltyp(i))
              endif

              if (flag(i) .and. stc(i,k)<t0c) then
                df2 = 0.0
                kt2(i) = 0.0
              endif

              if (flag(i)) then
                dmdz2(i) = (smc(i,k) - smc(i,k+1))                      &
     &                   / (0.5 * (zsoil(i,k-1) - zsoil(i,k+1)))
                smcz(i) = max(smc(i,k), smc(i,k+1))

                rhsmc(i,k) = (df2*dmdz2(i) + kt2(i)                     &
     &                     - df1(i)*dmdz(i) - kt1(i) + et(i,k))         &
     &                     / (rhoh2o*(zsoil(i,k) - zsoil(i,k-1)))

                ddz2(i) = 2.0 / (zsoil(i,k-1) - zsoil(i,k+1))
                cim(i,k) = -df2*ddz2(i)                                 &
     &                   / ((zsoil(i,k-1) - zsoil(i,k))*rhoh2o)
              endif
            endif
          enddo

        else       ! if_k_block

          do i = 1, im
            if (flag_iter(i)) then
              if (flag(i)) then
                kt2(i) = funckt(smc(i,k),soiltyp(i))
              endif

              if (flag(i) .and. stc(i,k)<t0c) kt2(i) = 0.0

              if (flag(i)) then
                rhsmc(i,k) = (kt2(i) - df1(i)*dmdz(i) - kt1(i)+et(i,k)) &
     &                     / (rhoh2o*(zsoil(i,k) - zsoil(i,k-1)))
                drain(i) = kt2(i)
                cim(i,k) = 0.0
              endif
            endif
          enddo

        endif   ! end if_k_block

        do i = 1, im
          if (flag_iter(i) .and. flag(i)) then
            aim(i,k) = -df1(i)*ddz(i)                                   &
     &               / ((zsoil(i,k-1) - zsoil(i,k))*rhoh2o)
            bim(i,k) = -(aim(i,k) + cim(i,k))
            df1(i) = df2
            kt1(i) = kt2(i)
            dmdz(i) = dmdz2(i)
            ddz(i) = ddz2(i)
          endif
        enddo
      enddo   ! end do_k_loop

!  --- ...  update soil temperature

      do i = 1, im
!       flag(i) = islimsk(i) /= 0
        flag(i) = islimsk(i) == 1
      enddo

!  --- ...  surface temperature is part of the update when snow is absent

      do i = 1, im
!       if (flag(i) .and. snowd(i)<=0.001) then
        if (flag_iter(i)) then
          if (flag(i) .and. .not.flagsnw(i)) then
            yy(i) = t1(i)                                               &
     &            + (rcap(i) - gflux(i) - evap(i)) / (rsmall(i)*rch(i))
            zz(i) = 1.0 + dft0(i) / (-0.5*zsoil(i,1)*rch(i)*rsmall(i))
            xx(i) = dft0(i) * (stsoil(i,1) - yy(i))                     &
     &            / (0.5*zsoil(i,1)*zz(i))
          endif

!         if (flag(i) .and. snowd(i)> 0.001) then
          if (flag(i) .and. flagsnw(i)) then
            yy(i) = stsoil(i,1)

!  --- ...  heat flux from snow is explicit in time

            zz(i) = 1.0
            xx(i) = dfsnow * (stsoil(i,1) - tsea(i))                    &
     &            / (-factsnw(i) * max(snowd(i), 0.001))
          endif
        endif
      enddo

!  --- ...  compute the forcing and the implicit matrix elements for update
!           ch2o is the heat capacity of water and csoil is the heat capacity
!           of soil

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          smcz(i) = max(smc(i,1), smc(i,2))
          dtdz1(i) = (stsoil(i,1) - stsoil(i,2)) / (-0.5*zsoil(i,2))
          dft1(i) = ktsoil(smcz(i),soiltyp(i))
          hcpct(i) = smc(i,1)*ch2o + (1.0 - smc(i,1)) * csoil
          dft2(i) = dft1(i)
          ddz(i) = 1.0 / (-0.5*zsoil(i,2))

!  --- ...  ai, bi, and ci are the elements of the tridiagonal matrix for the
!           implicit update of the soil temperature

          ai(i,1) = 0.0
          bi(i,1) = dft1(i)*ddz(i) / (-zsoil(i,1)*hcpct(i))
          ci(i,1) = -bi(i,1)
          bi(i,1) = bi(i,1)                                             &
     &            + dft0(i) / (0.5*zsoil(i,1)**2 * hcpct(i)*zz(i))
          rhstc(i,1) = (dft1(i)*dtdz1(i) - xx(i))/(zsoil(i,1)*hcpct(i))
        endif
      enddo

      do k = 2, km
        do i = 1, im
          if (flag_iter(i) .and. flag(i)) then
            hcpct(i) = smc(i,k)*ch2o + (1.0 - smc(i,k)) * csoil
          endif
        enddo

        if (k < km) then

          do i = 1, im
            if (flag_iter(i) .and. flag(i)) then
              dtdz2(i) = (stsoil(i,k) - stsoil(i,k+1))                  &
     &                 / (0.5 * (zsoil(i,k-1) - zsoil(i,k+1)))
              smcz(i) = max(smc(i,k), smc(i,k+1))
              dft2(i) = ktsoil(smcz(i),soiltyp(i))
              ddz2(i) = 2.0 / (zsoil(i,k-1) - zsoil(i,k+1))
              ci(i,k) = -dft2(i) * ddz2(i)                              &
     &                / ((zsoil(i,k-1) - zsoil(i,k)) * hcpct(i))
            endif
          enddo

        else   ! if_k_block

!  --- ...  at the bottom, climatology is assumed at 2m depth for land and
!           freezing temperature is assumed for sea ice at z(km)

          do i = 1, im
            if (flag_iter(i) .and. flag(i)) then
              dtdz2(i) = (stsoil(i,k) - tg3(i))                         &
     &                 / (0.5 * (zsoil(i,k-1) + zsoil(i,k)) - zbot)
              dft2(i) = ktsoil(smc(i,k),soiltyp(i))
              ci(i,k) = 0.0
            endif
          enddo

        endif   ! end if_k_block

        do i = 1, im
          if (flag_iter(i) .and. flag(i)) then
            rhstc(i,k) = (dft2(i)*dtdz2(i) - dft1(i)*dtdz1(i))          &
     &                 / ((zsoil(i,k) - zsoil(i,k-1)) * hcpct(i))
            ai(i,k) = -dft1(i) * ddz(i)                                 &
     &                / ((zsoil(i,k-1) - zsoil(i,k)) * hcpct(i))
            bi(i,k) = -(ai(i,k) + ci(i,k))
            dft1(i) = dft2(i)
            dtdz1(i) = dtdz2(i)
            ddz(i) = ddz2(i)
          endif
        enddo
      enddo   ! end do_k_loop

!  --- ...  solve the tri-diagonal matrix

      do k = 1, km
        do i = 1, im
          if (flag_iter(i) .and. flag(i))  then
            rhstc(i,k) = rhstc(i,k) * delt2
            ai(i,k) = ai(i,k) * delt2
            bi(i,k) = 1.0 + bi(i,k)*delt2
            ci(i,k) = ci(i,k) * delt2
          endif
        enddo
      enddo

!  --- ...  forward elimination

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          ci(i,1) = -ci(i,1) / bi(i,1)
          rhstc(i,1) = rhstc(i,1) / bi(i,1)
        endif
      enddo

      do k = 2, km
        do i = 1, im
          if (flag_iter(i) .and. flag(i)) then
            cc = 1.0 / (bi(i,k) + ai(i,k)*ci(i,k-1))
            ci(i,k) = -ci(i,k) * cc
            rhstc(i,k) = (rhstc(i,k) - ai(i,k)*rhstc(i,k-1)) * cc
          endif
        enddo
      enddo

!  --- ...  backward substituttion

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          ci(i,km) = rhstc(i,km)
        endif
      enddo

      do k = km-1, 1
        do i = 1, im
          if (flag_iter(i) .and. flag(i)) then
            ci(i,k) = ci(i,k)*ci(i,k+1) + rhstc(i,k)
          endif
        enddo
      enddo

!  --- ...  update soil and ice temperature

      do k = 1, km
        do i = 1, im
          if (flag_iter(i) .and. flag(i)) then
            stsoil(i,k) = stsoil(i,k) + ci(i,k)
          endif
        enddo
      enddo

!  --- ...  update surface temperature for snow free surfaces

      do i = 1, im
        if (flag_iter(i)) then
          if (flag(i) .and. .not.flagsnw(i)) then
            tsea(i) = (yy(i) + (zz(i) - 1.0) * stsoil(i,1)) / zz(i)
          endif
        endif
      enddo

!  --- ...  time filter for soil and skin temperature

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          tsurf(i) = ctfil1*tsea(i) + ctfil2*tsurf(i)
        endif
      enddo

      do k = 1, km
        do i = 1, im
          if (flag_iter(i) .and. flag(i)) then
            stc(i,k) = ctfil1*stsoil(i,k) + ctfil2*stc(i,k)
          endif
        enddo
      enddo

!  --- ...  gflux calculation

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          if (flagsnw(i)) then
            gflux(i) = -dfsnow * (tsurf(i) - stc(i,1))                  &
     &               / (factsnw(i) * max(snowd(i), 0.001))
          else
            gflux(i) = dft0(i) * (stc(i,1) - tsurf(i))                  &
     &               / (-0.5*zsoil(i,1))
          endif
        endif
      enddo

!  --- ...  calculate sensible heat flux

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          hflx(i) = rch(i) * (tsurf(i) - theta1(i))
        endif
      enddo

!  --- ...  the rest of the output

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          qsurf(i) = q1(i) + evap(i) / (elocp*rch(i))

          evbs (i) = edir(i)
          evcw (i) = ec(i)
          sbsno(i) = snoevp(i)
          snowc(i) = 1.0 - partlnd(i)
          stm  (i) = -smc(i,1) * zsoil(i,1)
          snohf(i) = dfsnow * (t1(i) - stsoil(i,1))
          trans(i) = et(i,1)

          do k = 2, km
            stm(i) = stm(i) + smc(i,k) * (zsoil(i,k-1) - zsoil(i,k))
            trans(i) = trans(i) + et(i,k)
          enddo 

!  --- ...  convert snow depth back to mm of water equivalent

          weasd(i) = snowd(i) * 1000.0
        endif
      enddo

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          tem     = 1.0 / rho(i)
          hflx(i) = hflx(i) * tem * cpinv
          evap(i) = evap(i) * tem * hvapi
        endif
      enddo

!  --- ...  restore land-related prognostic fields for guess run

      do i = 1, im
        if (flag(i)) then
          if (flag_guess(i)) then
            weasd(i) = weasd_old(i)
            tskin(i)  = tskin_old(i)
            canopy(i) = canopy_old(i)
            tprcp(i)  = tprcp_old(i)
            srflag(i) = srflag_old(i)

            do k = 1, km
              stc(i,k) = stc_old(i,k)
            enddo
          else
            tskin(i) = tsurf(i)
          endif
        endif
      enddo

      return
      end
