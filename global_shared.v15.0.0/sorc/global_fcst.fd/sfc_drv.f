! ===================================================================== !
!  description:                                                         !
!                                                                       !
!  usage:                                                               !
!                                                                       !
!      call sfc_drv                                                     !
!  ---  inputs:                                                         !
!          ( im, km, ps, u1, v1, t1, q1, soiltyp, vegtype, sigmaf,      !
!            sfcemis, dlwflx, dswsfc, snet, delt, tg3, cm, ch,          !
!            prsl1, prslki, zf, islimsk, ddvel, slopetyp,               !
!            shdmin, shdmax, snoalb, sfalb, flag_iter, flag_guess,      !
!  ---  in/outs:                                                        !
!            weasd, snwdph, tskin, tprcp, srflag, smc, stc, slc,        !
!            canopy, trans, tsurf,                                      !
!  ---  outputs:                                                        !
!            sncovr1, qsurf, gflux, drain, evap, hflx, ep, runoff,      !
!            cmm, chh, evbs, evcw, sbsno, snowc, stm, snohf,            !
!            smcwlt2, smcref2, zorl )                                         !
!                                                                       !
!                                                                       !
!  subprogram called:  sflx                                             !
!                                                                       !
!  program history log:                                                 !
!         xxxx  --             created                                  !
!         200x  -- sarah lu    modified                                 !
!    oct  2006  -- h. wei      modified                                 !
!    apr  2009  -- y.-t. hou   modified to include surface emissivity   !
!                     effect on lw radiation. replaced the comfussing   !
!                     slrad (net sw + dlw) with sfc net sw snet=dsw-usw !
!    sep  2009  -- s. moorthi modification to remove rcl and unit change!
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
!  inputs:                                                       size   !
!     im       - integer, horiz dimention and num of used pts      1    !
!     km       - integer, vertical soil layer dimension            1    !
!     ps       - real, surface pressure (pa)                       im   !
!     u1, v1   - real, u/v component of surface layer wind         im   !
!     t1       - real, surface layer mean temperature (k)          im   !
!     q1       - real, surface layer mean specific humidity        im   !
!     soiltyp  - integer, soil type (integer index)                im   !
!     vegtype  - integer, vegetation type (integer index)          im   !
!     sigmaf   - real, areal fractional cover of green vegetation  im   !
!     sfcemis  - real, sfc lw emissivity ( fraction )              im   !
!     dlwflx   - real, total sky sfc downward lw flux ( w/m**2 )   im   !
!     dswflx   - real, total sky sfc downward sw flux ( w/m**2 )   im   !
!     snet     - real, total sky sfc netsw flx into ground(w/m**2) im   !
!     delt     - real, time interval (second)                      1    !
!     tg3      - real, deep soil temperature (k)                   im   !
!     cm       - real, surface exchange coeff for momentum (m/s)   im   !
!     ch       - real, surface exchange coeff heat & moisture(m/s) im   !
!     prsl1    - real, sfc layer 1 mean pressure (pa)              im   !
!     prslki   - real,                                             im   !
!     zf       - real, height of bottom layer (m)                  im   !
!     islimsk  - integer, sea/land/ice mask (=0/1/2)               im   !
!     ddvel    - real,                                             im   !
!     slopetyp - integer, class of sfc slope (integer index)       im   !
!     shdmin   - real, min fractional coverage of green veg        im   !
!     shdmax   - real, max fractnl cover of green veg (not used)   im   !
!     snoalb   - real, upper bound on max albedo over deep snow    im   !
!     sfalb    - real, mean sfc diffused sw albedo (fractional)    im   !
!     flag_iter- logical,                                          im   !
!     flag_guess-logical,                                          im   !
!                                                                       !
!  input/outputs:                                                       !
!     weasd    - real, water equivalent accumulated snow depth (mm) im  !
!     snwdph   - real, snow depth (water equiv) over land          im   !
!     tskin    - real, ground surface skin temperature ( k )       im   !
!     tprcp    - real, total precipitation                         im   !
!     srflag   - real, snow/rain flag for precipitation            im   !
!     smc      - real, total soil moisture content (fractional)   im,km !
!     stc      - real, soil temp (k)                              im,km !
!     slc      - real, liquid soil moisture                       im,km !
!     canopy   - real, canopy moisture content (m)                 im   !
!     trans    - real, total plant transpiration (m/s)             im   !
!     tsurf    - real, surface skin temperature (after iteration)  im   !
!                                                                       !
!  outputs:                                                             !
!     sncovr1  - real, snow cover over land (fractional)           im   !
!     qsurf    - real, specific humidity at sfc                    im   !
!     gflux    - real, soil heat flux (w/m**2)                     im   !
!     drain    - real, subsurface runoff (mm/s)                    im   !
!     evap     - real, evaperation from latent heat flux           im   !
!     hflx     - real, sensible heat flux                          im   !
!     ep       - real, potential evaporation                       im   !
!     runoff   - real, surface runoff (m/s)                        im   !
!     cmm      - real,                                             im   !
!     chh      - real,                                             im   !
!     evbs     - real, direct soil evaporation (m/s)               im   !
!     evcw     - real, canopy water evaporation (m/s)              im   !
!     sbsno    - real, sublimation/deposit from snopack (m/s)      im   !
!     snowc    - real, fractional snow cover                       im   !
!     stm      - real, total soil column moisture content (m)      im   !
!     snohf    - real, snow/freezing-rain latent heat flux (w/m**2)im   !
!     smcwlt2  - real, dry soil moisture threshold                 im   !
!     smcref2  - real, soil moisture threshold                     im   !
!     zorl     - real, surface roughness                           im   !
!                                                                       !
!  ====================    end of description    =====================  !

!-----------------------------------
      subroutine sfc_drv                                                &
!...................................
!  ---  inputs:
     &     ( im, km, ps, u1, v1, t1, q1, soiltyp, vegtype, sigmaf,      &
     &       sfcemis, dlwflx, dswsfc, snet, delt, tg3, cm, ch,          &
     &       prsl1, prslki, zf, islimsk, ddvel, slopetyp,               &
     &       shdmin, shdmax, snoalb, sfalb, flag_iter, flag_guess,      &
!  ---  in/outs:
     &       weasd, snwdph, tskin, tprcp, srflag, smc, stc, slc,        &
     &       canopy, trans, tsurf,                                      &
!  ---  outputs:
     &       sncovr1, qsurf, gflux, drain, evap, hflx, ep, runoff,      &
     &       cmm, chh, evbs, evcw, sbsno, snowc, stm, snohf,            &
     &       smcwlt2, smcref2,zorl                                      &
     &     )
!
      use machine ,   only : kind_phys
      use funcphys,   only : fpvs
      use physcons,   only : con_g, con_hvap, con_cp, con_jcal,         &
     &                       con_eps, con_epsm1, con_fvirt, con_rd

      implicit none

!  ---  constant parameters:
      real(kind=kind_phys), parameter :: cpinv   = 1.0/con_cp
      real(kind=kind_phys), parameter :: hvapi   = 1.0/con_hvap
      real(kind=kind_phys), parameter :: elocp   = con_hvap/con_cp
      real(kind=kind_phys), parameter :: rhoh2o  = 1000.0
!     real(kind=kind_phys), parameter :: convrad = con_jcal*1.e4/60.0
      real(kind=kind_phys), parameter :: a2      = 17.2693882
      real(kind=kind_phys), parameter :: a3      = 273.16
      real(kind=kind_phys), parameter :: a4      = 35.86
      real(kind=kind_phys), parameter :: a23m4   = a2*(a3-a4)

      real(kind=kind_phys), save         :: zsoil_noah(4)
      data zsoil_noah / -0.1, -0.4, -1.0, -2.0 /

!  ---  input:
      integer, intent(in) :: im, km

      integer, dimension(im), intent(in) :: soiltyp, vegtype, slopetyp

      real (kind=kind_phys), dimension(im), intent(in) :: ps, u1, v1,   &
     &       t1, q1, sigmaf, sfcemis, dlwflx, dswsfc, snet, tg3, cm,    &
     &       ch, prsl1, prslki, ddvel, shdmin, shdmax,                  &
     &       snoalb, sfalb, zf

      integer, dimension(im), intent(in) :: islimsk
      real (kind=kind_phys),  intent(in) :: delt

      logical, dimension(im), intent(in) :: flag_iter, flag_guess

!  ---  in/out:
      real (kind=kind_phys), dimension(im), intent(inout) :: weasd,     &
     &       snwdph, tskin, tprcp, srflag, canopy, trans, tsurf

      real (kind=kind_phys), dimension(im,km), intent(inout) ::         &
     &       smc, stc, slc

!  ---  output:
      real (kind=kind_phys), dimension(im), intent(out) :: sncovr1,     &
     &       qsurf, gflux, drain, evap, hflx, ep, runoff, cmm, chh,     &
     &       evbs, evcw, sbsno, snowc, stm, snohf, smcwlt2, smcref2,    &
     &       zorl
    
!  ---  locals:
      real (kind=kind_phys), dimension(im) :: rch, rho,                 &
     &       q0, qs1, theta1, tv1, wind, weasd_old, snwdph_old,         &
     &       tprcp_old, srflag_old, tskin_old, canopy_old

      real (kind=kind_phys), dimension(km) :: et, sldpth, stsoil,       &
     &       smsoil, slsoil

      real (kind=kind_phys), dimension(im,km) :: zsoil, smc_old,        &
     &       stc_old, slc_old

      real (kind=kind_phys) :: alb, albedo, beta, chx, cmx, cmc,        &
     &       dew, drip, dqsdt2, ec, edir, ett, eta, esnow, etp,         &
     &       flx1, flx2, flx3, ffrozp, lwdn, pc, prcp, ptu, q2,         &
     &       q2sat, solnet, rc, rcs, rct, rcq, rcsoil, rsmin,           &
     &       runoff1, runoff2, runoff3, sfcspd, sfcprs, sfctmp,         &
     &       sfcems, sheat, shdfac, shdmin1d, shdmax1d, smcwlt,         &
     &       smcdry, smcref, smcmax, sneqv, snoalb1d, snowh,            &
     &       snomlt, sncovr, soilw, soilm, ssoil, tsea, th2, tbot,      &
     &       xlai, zlvl, swdn, tem,z0

      integer :: couple, ice, nsoil, nroot, slope, stype, vtype 
      integer :: i, k

      logical :: flag(im)
!
!===> ...  begin here
!

!  --- ...  set flag for land points

      do i = 1, im
        flag(i) = (islimsk(i) == 1)
      enddo

!  --- ...  save land-related prognostic fields for guess run

      do i = 1, im
        if (flag(i) .and. flag_guess(i)) then
          weasd_old(i)  = weasd(i)
          snwdph_old(i) = snwdph(i)
          tskin_old(i)  = tskin(i)
          canopy_old(i) = canopy(i)
          tprcp_old(i)  = tprcp(i)
          srflag_old(i) = srflag(i)

          do k = 1, km
           smc_old(i,k) = smc(i,k)
           stc_old(i,k) = stc(i,k)
           slc_old(i,k) = slc(i,k)
          enddo
        endif
      enddo

!  --- ...  initialization block

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          ep(i)     = 0.0
          evap (i)  = 0.0
          hflx (i)  = 0.0
          gflux(i)  = 0.0
          drain(i)  = 0.0
          canopy(i) = max(canopy(i), 0.0)

          evbs (i)  = 0.0
          evcw (i)  = 0.0
          trans(i)  = 0.0
          sbsno(i)  = 0.0
          snowc(i)  = 0.0
          snohf(i)  = 0.0
        endif
      enddo

!  --- ...  initialize variables 

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          wind(i) = sqrt( u1(i)*u1(i) + v1(i)*v1(i) )                   &
     &            + max(0.0, min(ddvel(i), 30.0))
          wind(i) = max(wind(i), 1.0)

          q0(i)   = max(q1(i), 1.e-8)   !* q1=specific humidity at level 1 (kg/kg)
          theta1(i) = t1(i) * prslki(i) !* adiabatic temp at level 1 (k)

          tv1(i) = t1(i) * (1.0 + con_fvirt*q0(i))
          rho(i) = prsl1(i) / (con_rd * tv1(i))
          qs1(i) = fpvs( t1(i) )        !* qs1=sat. humidity at level 1 (kg/kg)
          qs1(i) = con_eps*qs1(i) / (prsl1(i) + con_epsm1*qs1(i))
          qs1(i) = max(qs1(i), 1.e-8)
          q0 (i) = min(qs1(i), q0(i))
        endif
      enddo

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          do k = 1, km
            zsoil(i,k) = zsoil_noah(k)
          enddo
        endif
      enddo

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then

!  --- ...  noah: prepare variables to run noah lsm
!   1. configuration information (c):
!      ------------------------------
!    couple  - couple-uncouple flag (=1: coupled, =0: uncoupled) 
!    ffrozp  - flag for snow-rain detection (1.=snow, 0.=rain)                
!    ice     - sea-ice flag (=1: sea-ice, =0: land)
!    dt      - timestep (sec) (dt should not exceed 3600 secs) = delt
!    zlvl    - height (m) above ground of atmospheric forcing variables
!    nsoil   - number of soil layers (at least 2)
!    sldpth  - the thickness of each soil layer (m)

          couple = 1                      ! run noah lsm in 'couple' mode

          if     (srflag(i) == 1.0) then  ! snow phase
            ffrozp = 1.0
          elseif (srflag(i) == 0.0) then  ! rain phase
            ffrozp = 0.0
          endif
          ice = 0

          zlvl = zf(i)

          nsoil = km
          sldpth(1) = - zsoil(i,1)
          do k = 2, km
            sldpth(k) = zsoil(i,k-1) - zsoil(i,k)
          enddo

!   2. forcing data (f):
!      -----------------
!    lwdn    - lw dw radiation flux (w/m2)
!    solnet  - net sw radiation flux (dn-up) (w/m2)
!    sfcprs  - pressure at height zlvl above ground (pascals)
!    prcp    - precip rate (kg m-2 s-1)
!    sfctmp  - air temperature (k) at height zlvl above ground
!    th2     - air potential temperature (k) at height zlvl above ground
!    q2      - mixing ratio at height zlvl above ground (kg kg-1)

          lwdn   = dlwflx(i)         !..downward lw flux at sfc in w/m2
          swdn   = dswsfc(i)         !..downward sw flux at sfc in w/m2
          solnet = snet(i)           !..net sw rad flx (dn-up) at sfc in w/m2
          sfcems = sfcemis(i)

          sfcprs = prsl1(i) 
          prcp   = rhoh2o * tprcp(i) / delt
          sfctmp = t1(i)  
          th2    = theta1(i)
          q2     = q0(i)

!   3. other forcing (input) data (i):
!      ------------------------------
!    sfcspd  - wind speed (m s-1) at height zlvl above ground
!    q2sat   - sat mixing ratio at height zlvl above ground (kg kg-1)
!    dqsdt2  - slope of sat specific humidity curve at t=sfctmp (kg kg-1 k-1)

          sfcspd = wind(i)
          q2sat =  qs1(i)
          dqsdt2 = q2sat * a23m4/(sfctmp-a4)**2

!   4. canopy/soil characteristics (s):
!      --------------------------------
!    vegtyp  - vegetation type (integer index)                       -> vtype
!    soiltyp - soil type (integer index)                             -> stype
!    slopetyp- class of sfc slope (integer index)                    -> slope
!    shdfac  - areal fractional coverage of green vegetation (0.0-1.0)
!    shdmin  - minimum areal fractional coverage of green vegetation -> shdmin1d
!    ptu     - photo thermal unit (plant phenology for annuals/crops)
!    alb     - backround snow-free surface albedo (fraction)
!    snoalb  - upper bound on maximum albedo over deep snow          -> snoalb1d
!    tbot    - bottom soil temperature (local yearly-mean sfc air temp)

          vtype = vegtype(i)
          stype = soiltyp(i)
          slope = slopetyp(i)
          shdfac= sigmaf(i)

          shdmin1d = shdmin(i)   
          shdmax1d = shdmax(i)     
          snoalb1d = snoalb(i)    

          ptu  = 0.0
          alb  = sfalb(i)
          tbot = tg3(i)

!   5. history (state) variables (h):
!      ------------------------------
!    cmc     - canopy moisture content (m)
!    t1      - ground/canopy/snowpack) effective skin temperature (k)   -> tsea
!    stc(nsoil) - soil temp (k)                                         -> stsoil
!    smc(nsoil) - total soil moisture content (volumetric fraction)     -> smsoil
!    sh2o(nsoil)- unfrozen soil moisture content (volumetric fraction)  -> slsoil
!    snowh   - actual snow depth (m)
!    sneqv   - liquid water-equivalent snow depth (m)
!    albedo  - surface albedo including snow effect (unitless fraction)
!    ch      - surface exchange coefficient for heat and moisture (m s-1) -> chx
!    cm      - surface exchange coefficient for momentum (m s-1)          -> cmx

          cmc = canopy(i)/1000.              ! convert from mm to m
          tsea = tsurf(i)                    ! clu_q2m_iter

          do k = 1, km
            stsoil(k) = stc(i,k)
            smsoil(k) = smc(i,k)
            slsoil(k) = slc(i,k)
          enddo

          snowh = snwdph(i) * 0.001         ! convert from mm to m
          sneqv = weasd(i)  * 0.001         ! convert from mm to m
          if (sneqv /= 0.0 .and. snowh == 0.0) then
            snowh = 10.0 * sneqv
          endif

          chx    = ch(i)  * wind(i)              ! compute conductance
          cmx    = cm(i)  * wind(i)
          chh(i) = chx * rho(i)
          cmm(i) = cmx

!  --- ...  call noah lsm

          call sflx                                                     &
!  ---  inputs:
     &     ( nsoil, couple, ice, ffrozp, delt, zlvl, sldpth,            &
     &       swdn, solnet, lwdn, sfcems, sfcprs, sfctmp,                &
     &       sfcspd, prcp, q2, q2sat, dqsdt2, th2,                      &
     &       vtype, stype, slope, shdmin1d, alb, snoalb1d,              &
!  ---  input/outputs:
     &       tbot, cmc, tsea, stsoil, smsoil, slsoil, sneqv, chx, cmx,  &
!  ---  outputs:
     &       nroot, shdfac, snowh, albedo, eta, sheat, ec,              &
     &       edir, et, ett, esnow, drip, dew, beta, etp, ssoil,         &
     &       flx1, flx2, flx3, runoff1, runoff2, runoff3,               &
     &       snomlt, sncovr, rc, pc, rsmin, xlai, rcs, rct, rcq,        &
     &       rcsoil, soilw, soilm, smcwlt, smcdry, smcref, smcmax,      &
     &       z0 )

!  --- ...  noah: prepare variables for return to parent mode
!   6. output (o):
!      -----------
!    eta     - actual latent heat flux (w m-2: positive, if upward from sfc)
!    sheat   - sensible heat flux (w m-2: positive, if upward from sfc)
!    beta    - ratio of actual/potential evap (dimensionless)
!    etp     - potential evaporation (w m-2)
!    ssoil   - soil heat flux (w m-2: negative if downward from surface)
!    runoff1 - surface runoff (m s-1), not infiltrating the surface
!    runoff2 - subsurface runoff (m s-1), drainage out bottom

          evap(i)  = eta
          hflx(i)  = sheat
          gflux(i) = ssoil

          evbs(i)  = edir
          evcw(i)  = ec
          trans(i) = ett
          sbsno(i) = esnow
          snowc(i) = sncovr
          stm(i)   = soilm
          snohf(i) = flx1 + flx2 + flx3

          smcwlt2(i) = smcwlt
          smcref2(i) = smcref

          ep(i)      = etp
          tsurf(i)   = tsea

          do k = 1, km
            stc(i,k) = stsoil(k) 
            smc(i,k) = smsoil(k)
            slc(i,k) = slsoil(k)
          enddo

!  --- ...  unit conversion (from m s-1 to mm s-1)
          runoff(i)  = runoff1 * 1000.0
          drain (i)  = runoff2 * 1000.0

!  --- ...  unit conversion (from m to mm)
          canopy(i)  = cmc   * 1000.0
          snwdph(i)  = snowh * 1000.0
          weasd(i)   = sneqv * 1000.0
          sncovr1(i) = sncovr
!  ---- ... outside sflx, roughness uses cm as unit
          zorl(i) = z0*100.

!  --- ...  do not return the following output fields to parent model
!    ec      - canopy water evaporation (m s-1)
!    edir    - direct soil evaporation (m s-1)
!    et(nsoil)-plant transpiration from a particular root layer (m s-1)
!    ett     - total plant transpiration (m s-1)
!    esnow   - sublimation from (or deposition to if <0) snowpack (m s-1)
!    drip    - through-fall of precip and/or dew in excess of canopy
!              water-holding capacity (m)
!    dew     - dewfall (or frostfall for t<273.15) (m)
!    beta    - ratio of actual/potential evap (dimensionless)
!    flx1    - precip-snow sfc (w m-2)
!    flx2    - freezing rain latent heat flux (w m-2)
!    flx3    - phase-change heat flux from snowmelt (w m-2)
!    snomlt  - snow melt (m) (water equivalent)
!    sncovr  - fractional snow cover (unitless fraction, 0-1)
!    runoff3 - numerical trunctation in excess of porosity (smcmax)
!              for a given soil layer at the end of a time step
!    rc      - canopy resistance (s m-1)
!    pc      - plant coefficient (unitless fraction, 0-1) where pc*etp
!              = actual transp
!    xlai    - leaf area index (dimensionless)
!    rsmin   - minimum canopy resistance (s m-1)
!    rcs     - incoming solar rc factor (dimensionless)
!    rct     - air temperature rc factor (dimensionless)
!    rcq     - atmos vapor pressure deficit rc factor (dimensionless)
!    rcsoil  - soil moisture rc factor (dimensionless)
!    soilw   - available soil moisture in root zone (unitless fraction
!              between smcwlt and smcmax)
!    soilm   - total soil column moisture content (frozen+unfrozen) (m)
!    smcwlt  - wilting point (volumetric)
!    smcdry  - dry soil moisture threshold where direct evap frm top
!              layer ends (volumetric)
!    smcref  - soil moisture threshold where transpiration begins to
!              stress (volumetric)
!    smcmax  - porosity, i.e. saturated value of soil moisture
!              (volumetric)
!    nroot   - number of root layers, a function of veg type, determined
!              in subroutine redprm.

        endif   ! end if_flag_iter_and_flag_block
      enddo   ! end do_i_loop

!   --- ...  compute qsurf (specific humidity at sfc)

      do i = 1, im
        if (flag_iter(i) .and. flag(i)) then
          rch(i)   = rho(i) * con_cp * ch(i) * wind(i)
          qsurf(i) = q1(i)  + evap(i) / (elocp * rch(i))
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
            weasd(i)  = weasd_old(i)
            snwdph(i) = snwdph_old(i)
            tskin(i)  = tskin_old(i)
            canopy(i) = canopy_old(i)
            tprcp(i)  = tprcp_old(i)
            srflag(i) = srflag_old(i)

            do k = 1, km
              smc(i,k) = smc_old(i,k)
              stc(i,k) = stc_old(i,k)
              slc(i,k) = slc_old(i,k)
            enddo
          else
            tskin(i) = tsurf(i)    
          endif
        endif
      enddo
!
      return
!...................................
      end subroutine sfc_drv
!-----------------------------------

