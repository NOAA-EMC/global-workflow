
      subroutine sfc_nst                                                &
!...................................
!  ---  inputs:
     &     ( im, km, ps, u1, v1, t1, q1, tref, cm, ch,                  &
     &       prsl1, prslki, islimsk, xlon, sinlat, stress,              &
     &       sfcemis, dlwflx, sfcnsw, rain, timestep, kdt,              &
     &       ddvel, flag_iter, flag_guess, nst_fcst, lprnt, ipr,        &
!  --- input/output
     &       tskin, tsurf, xt, xs, xu, xv, xz, zm, xtts, xzts, dt_cool, &
     &       z_c,   c_0,   c_d,   w_0, w_d, d_conv, ifd, qrain,         &
!  ---  outputs:
     &       qsurf, gflux, cmm, chh, evap, hflx, ep                     &
     &      )
!
! ===================================================================== !
!  description:                                                         !
!                                                                       !
!                                                                       !
!  usage:                                                               !
!                                                                       !
!    call sfc_nst                                                       !
!       inputs:                                                         !
!          ( im, km, ps, u1, v1, t1, q1, tref, cm, ch,                  !
!            prsl1, prslki, islimsk, xlon, sinlat, stress,              !
!            sfcemis, dlwflx, sfcnsw, rain, timestep, kdt,              !
!            ddvel, flag_iter, flag_guess, nst_fcst, lprnt, ipr,        !
!       input/outputs:                                                  !
!            tskin, tsurf, xt, xs, xu, xv, xz, zm, xtts, xzts, dt_cool, !
!            z_c, c_0,   c_d,   w_0, w_d, d_conv, ifd, qrain,           !
!  --   outputs:
!            qsurf, gflux, cmm, chh, evap, hflx, ep                     !
!           )
!                                                                       !
!                                                                       !
!  subprogram/functions called: w3movdat, iw3jdn, fpvs, density,        !
!       rhocoef, cool_skin, warm_layer, jacobi_temp.                    !
!                                                                       !
!  program history log:                                                 !
!         2007  -- xu li       createad original code                   !
!         2008  -- s. moorthi  adapted to the parallel version          !
!    may  2009  -- y.-t. hou   modified to include input lw surface     !
!                     emissivity from radiation. also replaced the      !
!                     often comfusing combined sw and lw suface         !
!                     flux with separate sfc net sw flux (defined       !
!                     as dn-up) and lw flux. added a program doc block. !
!    sep  2009 --  s. moorthi removed rcl and additional reformatting   !
!                     and optimization + made pa as input pressure unit.!
!         2009  -- xu li       recreatead the code                      !
!    feb  2010  -- s. moorthi added ass the changed made to the previous!
!                  version                                              !
!                                                                       !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
!  inputs:                                                       size   !
!     im       - integer, horiz dimension                          1    !
!     km       - integer, vertical dimension                       1    !
!     ps       - real, surface pressure (pa)                       im   !
!     u1, v1   - real, u/v component of surface layer wind (m/s)   im   !
!     t1       - real, surface layer mean temperature ( k )        im   !
!     q1       - real, surface layer mean specific humidity        im   !
!     tref     - real, reference/foundation temperature ( k )      im   !
!     cm       - real, surface exchange coeff for momentum (m/s)   im   !
!     ch       - real, surface exchange coeff heat & moisture(m/s) im   !
!     prsl1    - real, surface layer mean pressure (pa)            im   !
!     prslki   - real,                                             im   !
!     islimsk  - integer, sea/land/ice mask (=0/1/2)               im   !
!     xlon     - real, longitude         (radians)                 im   !
!     sinlat   - real, sin of latitude                             im   !
!     stress   - real, wind stress       (n/m**2)                  im   !
!     sfcemis  - real, sfc lw emissivity (fraction)                im   !
!     dlwflx   - real, total sky sfc downward lw flux (w/m**2)     im   !
!     sfcnsw   - real, total sky sfc netsw flx into ocean (w/m**2) im   !
!     rain     - real, rainfall rate     (kg/m**2/s)               im   !
!     timestep - real, timestep interval (second)                  1    !
!     kdt      - integer, time step counter                        1    !
!     ddvel    - real, wind enhancement due to convection (m/s)    im   !
!     flag_iter- logical,                                          im   !
!     flag_guess-logical,                                          im   !
!     nst_fcst  -integer, flag 0 for no nst, 1 for uncoupled nst        !
!                          and 2 for coupled nst                   1    !
!     lprnt    - logical, control flag for check print out         1    !
!     ipr      - integer, grid index for check print out           1    !
!                                                                       !
!  input/outputs:
! li added for oceanic components
!     tskin    - real, ocean surface skin temperature ( k )        im   !
!     tsurf    - real, ocean surface skin temperature ( k )??      im   !
!     xt       - real, heat content in dtl                         im   !
!     xs       - real, salinity  content in dtl                    im   !
!     xu       - real, u-current content in dtl                    im   !
!     xv       - real, v-current content in dtl                    im   !
!     xz       - real, dtl thickness                               im   !
!     zm       - real, mxl thickness                               im   !
!     xtts     - real, d(xt)/d(ts)                                 im   !
!     xzts     - real, d(xz)/d(ts)                                 im   !
!     dt_cool  - real, sub-layer cooling amount                    im   !
!     d_conv   - real, thickness of free convection layer (fcl)    im   !
!     z_c      - sub-layer cooling thickness                       im   !
!     c_0      - coefficient1 to calculate d(tz)/d(ts)             im   !
!     c_d      - coefficient2 to calculate d(tz)/d(ts)             im   !
!     w_0      - coefficient3 to calculate d(tz)/d(ts)             im   !
!     w_d      - coefficient4 to calculate d(tz)/d(ts)             im   !
!     ifd      - real, index to start dtm run or not               im   !
!     qrain    - real, sensible heat flux due to rainfall (watts)  im   !

!  outputs:                                                             !

!     qsurf    - real, surface air saturation specific humidity    im   !
!     gflux    - real, soil heat flux (w/m**2)                     im   !
!     cmm      - real,                                             im   !
!     chh      - real,                                             im   !
!     evap     - real, evaperation from latent heat flux           im   !
!     hflx     - real, sensible heat flux                          im   !
!     ep       - real, potential evaporation                       im   !
!                                                                       !
! ===================================================================== !
      use machine , only : kind_phys
      use funcphys, only : fpvs
      use physcons, hvap => con_hvap                                    &
     &,             cp => con_cp, hfus => con_hfus, jcal => con_jcal    &
     &,             eps => con_eps, epsm1 => con_epsm1                  &
     &,             rvrdm1 => con_fvirt, rd => con_rd                   &
     &,             rhw0 => con_rhw0,sbc => con_sbc,pi => con_pi
      use date_def, only: idate
      use module_nst_parameters, only : t0k,cp_w,omg_m,omg_sh,          &
     &    sigma_r,gray,solar_time_6am,ri_c,z_w_max,delz,wd_max,         &
     &    rad2deg,const_rot,tau_min,tw_max,sst_max
      use module_nst_water_prop, only: solar_time_from_julian,          &
     &                                 density,rhocoef,compjd,grv       &
     &,                                sw_ps_9b
      use nst_module, only : cool_skin,dtm_1p,cal_w,cal_ttop,           &
     &                       convdepth,dtm_1p_fca,dtm_1p_tla,           &
     &                       dtm_1p_mwa,dtm_1p_mda,dtm_1p_mta,          &
     &                       dtl_reset
!
      implicit none
!
!  ---  constant parameters:
      real (kind=kind_phys), parameter :: cpinv=1.0/cp, hvapi=1.0/hvap
      real (kind=kind_phys), parameter :: f24   = 24.0     ! hours/day
      real (kind=kind_phys), parameter :: f1440 = 1440.0   ! minutes/day

!  ---  inputs:
      integer, intent(in) :: im, km, kdt, ipr, nst_fcst
      real (kind=kind_phys), dimension(im), intent(in) :: ps, u1, v1,   &
     &       t1, q1, tref, cm, ch, prsl1, prslki, xlon,                 &
     &       sinlat, stress, sfcemis, dlwflx, sfcnsw, rain, ddvel
      integer, intent(in), dimension(im):: islimsk
      real (kind=kind_phys), intent(in) :: timestep

      logical, intent(in) :: flag_iter(im), flag_guess(im), lprnt

!  ---  input/outputs:
! control variables of dtl system (5+2) and sl (2) and coefficients for d(tz)/d(ts) calculation
      real (kind=kind_phys), dimension(im), intent(inout) :: tskin,     &
     &      tsurf, xt, xs, xu, xv, xz, zm, xtts, xzts, dt_cool,         &
     &      z_c, c_0, c_d, w_0, w_d, d_conv, ifd, qrain

!  ---  outputs:
      real (kind=kind_phys), dimension(im), intent(out) ::              &
     &       qsurf, gflux, cmm, chh, evap, hflx, ep

!
!     locals
!
      integer :: k,i
!
      real (kind=kind_phys), dimension(im) ::  q0, qss, rch, 
     &                     rho_a, theta1, tv1, wind, wndmag

      real(kind=kind_phys) elocp,tem
!
!    nstm related prognostic fields
!
      logical flag(im)
      real (kind=kind_phys), dimension(im) ::        
     &   xt_old, xs_old, xu_old, xv_old, xz_old,zm_old,xtts_old,
     &   xzts_old, ifd_old, tref_old, tskin_old, dt_cool_old,z_c_old

      real(kind=kind_phys) ulwflx(im), nswsfc(im)
!     real(kind=kind_phys) rig(im),
!    &                     ulwflx(im),dlwflx(im),
!    &                     slrad(im),nswsfc(im)
      real(kind=kind_phys) alpha,beta,rho_w,f_nsol,sss,sep,soltim,
     &                     cosa,sina,taux,tauy,grav,dz,t0,ttop0,ttop

      integer :: iyear,imon,iday,ihr,imin
      integer :: idat(8),jdat(8)

      real(kind=kind_phys) le,fc,dwat,dtmp,wetc,alfac,ustar_a,rich
      real(kind=kind_phys) rnl_ts,hs_ts,hl_ts,rf_ts,q_ts
      real(kind=kind_phys) jday,fw,q_warm
      real(kind=kind_phys) t12,alon,tsea,sstc,dta
      real(kind=kind_phys) fjd1,jd1,jd0
      real(kind=kind_phys) rinc(5) 

!  external functions called: iw3jdn
      integer :: iw3jdn
!======================================================================================================
cc
      parameter (elocp=hvap/cp)

      sss = 34.0             ! temporarily, when sea surface salinity data is not ready

      idat(1) = idate(4)
      idat(2) = idate(2)
      idat(3) = idate(3)
      idat(4) = 0
      idat(5) = idate(1)
      idat(6) = 0
      idat(7) = 0
      idat(8) = 0

      rinc(1) = 0.
      rinc(2) = 0.
      rinc(3) = float(kdt)*timestep/60.0
      rinc(4) = 0.
      rinc(5) = 0.
      call w3movdat(rinc, idat, jdat)

      iyear = jdat(1)
      imon  = jdat(2)
      iday  = jdat(3)
      ihr   = jdat(5)
      imin  = jdat(6)

!  --- ...  calculate forecast julian day and fraction of julian day

      jd0 = iw3jdn(1899,12,31)
      jd1 = iw3jdn(iyear,imon,iday)

!  --- ...  unlike in normal applications, where day starts from 0 hr,
!           in astronomy applications, day stats from noon.
      if (ihr < 12) then
        jd1 = jd1 - 1
        fjd1= 0.5 + float(ihr)/f24 + float(imin)/f1440
      else
        fjd1= float(ihr - 12)/f24 + float(imin)/f1440
      endif
      jday  = jd1 - jd0 + fjd1 
!
! flag for open water
!
      do i = 1, im
         flag(i) = islimsk(i) == 0 .and. flag_iter(i)
      enddo

!
!  save nst-related prognostic fields for guess run
!
      do i=1, im
        if((islimsk(i) == 0) .and. flag_guess(i)) then
          xt_old(i)      = xt(i)
          xs_old(i)      = xs(i)
          xu_old(i)      = xu(i)
          xv_old(i)      = xv(i)
          xz_old(i)      = xz(i)
          zm_old(i)      = zm(i)
          xtts_old(i)    = xtts(i)
          xzts_old(i)    = xzts(i)
          ifd_old(i)     = ifd(i)
          tskin_old(i)   = tskin(i)
          dt_cool_old(i) = dt_cool(i)
          z_c_old(i)     = z_c(i)
        endif
      enddo

!  --- ...  initialize variables. all units are m.k.s. unless specified.
!           ps is in pascals, wind is wind speed, theta1 is surface air
!           estimated from level 1 temperature, rho_a is air density and
!           qss is saturation specific humidity at the water surface
!!
      do i = 1, im
        if ( flag(i) ) then

          nswsfc(i) = sfcnsw(i) ! net solar radiation at the air-sea surface (positive downward)
          wndmag(i) = sqrt(u1(i)*u1(i) + v1(i)*v1(i))
          wind(i)   = wndmag(i) + max( 0.0, min( ddvel(i), 30.0 ) )
          wind(i)   = max( wind(i), 1.0 )

          q0(i)     = max(q1(i), 1.0e-8)
          theta1(i) = t1(i) * prslki(i)
          tv1(i)    = t1(i) * (1.0 + rvrdm1*q0(i))
          rho_a(i)  = prsl1(i) / (rd*tv1(i))
          qss(i)    = fpvs(tsurf(i))                          ! pa
          qss(i)    = eps*qss(i) / (ps(i) + epsm1*qss(i))     ! pa
!
          evap(i)    = 0.0
          hflx(i)    = 0.0
          gflux(i)   = 0.0
          ep(i)      = 0.0

!  --- ...  rcp = rho cp ch v

          rch(i)     = rho_a(i) * cp * ch(i) * wind(i)
          cmm(i)     = cm (i)   * wind(i)
          chh(i)     = rho_a(i) * ch(i) * wind(i)

!  --- ...  latent and sensible heat flux over open water with tskin
!           at previous time step
          evap(i)    = elocp * rch(i) * (qss(i) - q0(i))
          qsurf(i)   = qss(i)
          hflx(i)    = rch(i) * (tsurf(i) - theta1(i))

!     if (lprnt .and. i == ipr) print *,' tskin=',tskin(i),' theta1=',
!    & theta1(i),' hflx=',hflx(i),' t1=',t1(i),'prslki=',prslki(i)
!    &,' tsurf=',tsurf(i)
        endif
      enddo

! run nst model: dtm + slm   
! 
      do i = 1, im
        if ( flag(i) ) then
          tsea      = tsurf(i)
          t12       = tsea*tsea
          ulwflx(i) = sfcemis(i) * sbc * t12 * t12
          alon      = xlon(i)*rad2deg
          grav      = grv(sinlat(i))
          call solar_time_from_julian(jday,alon,soltim)
          call density(tsea,sss,rho_w)                     ! sea water density
          call rhocoef(tsea,sss,rho_w,alpha,beta)          ! alpha & beta
!
!  calculate sensible heat flux due to rainfall 
!
          le       = (2.501-.00237*tsea)*1e6
          dwat     = 2.11e-5*(t1(i)/t0k)**1.94               ! water vapor diffusivity
          dtmp     = (1.+3.309e-3*(t1(i)-t0k)-1.44e-6*(t1(i)-t0k)*
     &              (t1(i)-t0k))*0.02411/(rho_a(i)*cp)       ! heat diffusivity
          wetc     = 622.0*le*qss(i)/(rd*t1(i)*t1(i))
          alfac    = 1/(1+(wetc*le*dwat)/(cp*dtmp))          ! wet bulb factor
          qrain(i) =  (1000.*rain(i)/rho_w)*alfac*cp_w*
     &                (tsea-t1(i)+(1000.*qss(i)-1000.*q0(i))*le/cp)

!  --- ...  input non solar heat flux as upward = positive to models here

          f_nsol   = hflx(i) + evap(i) + ulwflx(i) - dlwflx(i)
     &             + omg_sh*qrain(i)

!     if (lprnt .and. i == ipr) print *,' f_nsol=',f_nsol,' hflx=',
!    &hflx(i),' evap=',evap(i),' ulwflx=',ulwflx(i),' dlwflx=',dlwflx(i)
!    &,' omg_sh=',omg_sh,' qrain=',qrain(i)

          sep      =  sss*(evap(i)/le-rain(i))/rho_w
          ustar_a  = sqrt(stress(i)/rho_a(i))          ! air friction velocity
!
!  sensitivities of heat flux components to ts
!
          rnl_ts = 4.0*gray*sigma_r*tsea*tsea*tsea     ! d(rnl)/d(ts)
          hs_ts  = rch(i)
          hl_ts  = rch(i)*elocp*eps*hvap*qss(i)/(rd*t12)
          rf_ts  = (1000.*rain(i)/rho_w)*alfac*cp_w*(1.0+rch(i)*hl_ts)
          q_ts   = rnl_ts + hs_ts + hl_ts + omg_sh*rf_ts
!
!    run sub-layer cooling model/parameterization & calculate c_0, c_d
!
          call cool_skin(ustar_a,f_nsol,nswsfc(i),evap(i),sss,alpha,beta
     &,                  rho_w,rho_a(i),tsea,q_ts,hl_ts,grav,le
     &,                  dt_cool(i),z_c(i),c_0(i),c_d(i))

          tem  = 1.0 / wndmag(i)
          cosa = u1(i)*tem
          sina = v1(i)*tem
          taux = max(stress(i),tau_min)*cosa
          tauy = max(stress(i),tau_min)*sina
          fc   = const_rot*sinlat(i)
!
!     run dtm-1p system
!
          if ( (soltim > solar_time_6am .and. ifd(i) == 0.0) ) then
          else
            ifd(i) = 1.0
!
!     calculate fcl thickness with current forcing and previous time's profile
!
!     if (lprnt .and. i == ipr) print *,' beg xz=',xz(i)

            if ( f_nsol > 0.0 .and. xt(i) > 0.0 ) then
              call convdepth(kdt,timestep,nswsfc(i),f_nsol,sss,sep,rho_w
     &,                      alpha,beta,xt(i),xs(i),xz(i),d_conv(i))
            else
              d_conv(i) = 0.0
            endif

!     if (lprnt .and. i == ipr) print *,' beg xz1=',xz(i)
!
!    determine rich: wind speed dependent (right now)
!
!           if ( wind(i) < 1.0 ) then
!             rich = 0.25 + 0.03*wind(i)
!           elseif ( wind(i) >= 1.0 .and. wind(i) < 1.5 ) then
!             rich = 0.25 + 0.1*wind(i)
!           elseif ( wind(i) >= 1.5 .and. wind(i) < 6.0 ) then
!             rich = 0.25 + 0.6*wind(i)
!           elseif ( wind(i) >= 6.0 ) then
!             rich = 0.25 + min(0.8*wind(i),0.50)
!           endif

            rich = ri_c
         
            call dtm_1p(kdt,timestep,rich,taux,tauy,nswsfc(i),
     &                  f_nsol,sss,sep,q_ts,hl_ts,rho_w,alpha,beta,alon,
     &                  sinlat(i),soltim,grav,le,d_conv(i),
     &                  xt(i),xs(i),xu(i),xv(i),xz(i),xzts(i),xtts(i))

!     if (lprnt .and. i == ipr) print *,' beg xz2=',xz(i)

!  apply mda
            if ( xt(i) > 0.0 ) then
              call dtm_1p_mda(xt(i),xtts(i),xz(i),xzts(i))
              if ( xz(i) >= z_w_max ) then
                call dtl_reset(xt(i),xs(i),xu(i),xv(i),xz(i),xtts(i),
     &                                                       xzts(i))

!     if (lprnt .and. i == ipr) print *,' beg xz3=',xz(i),' z_w_max='
!    &,z_w_max
              endif

!  apply fca
              if ( d_conv(i) > 0.0 ) then
                call dtm_1p_fca(d_conv(i),xt(i),xtts(i),xz(i),xzts(i))
                if ( xz(i) >= z_w_max ) then
                  call dtl_reset
     &              (xt(i),xs(i),xu(i),xv(i),xz(i),xzts(i),xtts(i))
                endif
              endif

!     if (lprnt .and. i == ipr) print *,' beg xz4=',xz(i)

!  apply tla
              dz = min(xz(i),max(d_conv(i),delz))
!
              call sw_ps_9b(delz,fw)
              q_warm = fw*nswsfc(i)-f_nsol    !total heat absorbed in warm layer
              if ( q_warm > 0.0 ) then
                call cal_ttop(kdt,timestep,q_warm,rho_w,dz,
     &                        xt(i),xz(i),ttop0)

!     if (lprnt .and. i == ipr) print *,' d_conv=',d_conv(i),' delz=',
!    &delz,' kdt=',kdt,' timestep=',timestep,' nswsfc=',nswsfc(i),
!    &' f_nsol=',f_nsol,' rho_w=',rho_w,' dz=',dz,' xt=',xt(i),
!    &' xz=',xz(i),' qrain=',qrain(i)

                ttop = ((xt(i)+xt(i))/xz(i))*(1.0-dz/((xz(i)+xz(i))))

!     if (lprnt .and. i == ipr) print *,' beg xz4a=',xz(i)
!    &,' ttop=',ttop,' ttop0=',ttop0,' xt=',xt(i),' dz=',dz
!    &,' xznew=',(xt(i)+sqrt(xt(i)*(xt(i)-dz*ttop0)))/ttop0

                if ( ttop > ttop0 ) then
                  call dtm_1p_tla(dz,ttop0,xt(i),xtts(i),xz(i),xzts(i))

!     if (lprnt .and. i == ipr) print *,' beg xz4b=',xz(i),'z_w_max=',
!    &z_w_max
                  if ( xz(i) >= z_w_max ) then
                    call dtl_reset
     &                   (xt(i),xs(i),xu(i),xv(i),xz(i),xzts(i),xtts(i))
                  endif
                endif
              endif           ! if ( q_warm > 0.0 ) then

!     if (lprnt .and. i == ipr) print *,' beg xz5=',xz(i)

!  apply mwa
              t0 = (xt(i)+xt(i))/xz(i)
              if ( t0 > tw_max ) then
                call dtm_1p_mwa(xt(i),xtts(i),xz(i),xzts(i))
                if ( xz(i) >= z_w_max ) then
                  call dtl_reset
     &                 (xt(i),xs(i),xu(i),xv(i),xz(i),xzts(i),xtts(i))
                endif
              endif

!     if (lprnt .and. i == ipr) print *,' beg xz6=',xz(i)

!  apply mta
              sstc = tref(i) + (xt(i)+xt(i))/xz(i) - dt_cool(i)
              if ( sstc > sst_max ) then
                dta = sstc - sst_max
                call  dtm_1p_mta(dta,xt(i),xtts(i),xz(i),xzts(i))
!               write(*,'(a,f3.0,7f8.3)') 'mta, sstc,dta :',islimsk(i),
!    &          sstc,dta,tref(i),xt(i),xz(i),2.0*xt(i)/xz(i),dt_cool(i)
               if ( xz(i) >= z_w_max ) then
                  call dtl_reset
     &                 (xt(i),xs(i),xu(i),xv(i),xz(i),xzts(i),xtts(i))
                endif
              endif
!
            endif             ! if ( xt(i) > 0.0 ) then
!           reset dtl at mignight
            if ( abs(soltim) < 2.0*timestep ) then
              call dtl_reset
     &           (xt(i),xs(i),xu(i),xv(i),xz(i),xzts(i),xtts(i))
            endif

          endif             ! if (solar_time > solar_time_6am .and. ifd(i) == 0.0 ) then: too late to start the first day

!     if (lprnt .and. i == ipr) print *,' beg xz7=',xz(i)

          tsurf(i) = max(271.0, tref(i)+(xt(i)+xt(i))/xz(i)-dt_cool(i))

!     if (lprnt .and. i == ipr) print *,' tsurf=',tsurf(i),' tref=',
!    &tref(i),' xz=',xz(i),' dt_cool=',dt_cool(i)

          if ( xt(i) > 0.0 ) then
            call cal_w(kdt,xz(i),xt(i),xzts(i),xtts(i),w_0(i),w_d(i))
          else
            w_0(i) = 0.0
            w_d(i) = 0.0
          endif

!         if ( xt(i) > 0.0 ) then
!           rig(i) = grav*xz(i)*xz(i)*(alpha*xt(i)-beta*xs(i))
!    &             /(2.0*(xu(i)*xu(i)+xv(i)*xv(i)))
!         else
!           rig(i) = 0.25
!         endif

!         qrain(i) = rig(i)
          zm(i) = wind(i)
        
        endif
      enddo

! restore nst-related prognostic fields for guess run
      do i=1, im
        if((islimsk(i) == 0) ) then
          if(flag_guess(i)) then
            xt(i)      = xt_old(i)
            xs(i)      = xs_old(i)
            xu(i)      = xu_old(i)
            xv(i)      = xv_old(i)
            xz(i)      = xz_old(i)
            zm(i)      = zm_old(i)
            xtts(i)    = xtts_old(i)
            xzts(i)    = xzts_old(i)
            ifd(i)     = ifd_old(i)
            tskin(i)   = tskin_old(i)
            dt_cool(i) = dt_cool_old(i)
            z_c(i)     = z_c_old(i)
          else
            if ( nst_fcst > 1 ) then
              tskin(i) = tsurf(i)
            endif                  ! if ( nst_fcst > 1 ) then
          endif                    ! if(flag_guess(i)) then
        endif                      ! if((islimsk(i).eq. 0.) ) then
      enddo

!     if (lprnt .and. i == ipr) print *,' beg xz8=',xz(i)

      if ( nst_fcst > 1 ) then
!  --- ...  latent and sensible heat flux over open water with updated tskin
        do i = 1, im
          if ( flag(i) ) then
            qss(i)   = fpvs( tskin(i) )
            qss(i)   = eps*qss(i) / (ps(i) + epsm1*qss(i))
            qsurf(i) = qss(i)
            evap(i)  = elocp*rch(i) * (qss(i) - q0(i))
            hflx(i)  = rch(i) * (tskin(i) - theta1(i))
          endif
        enddo
      endif                   ! if ( nst_fcst > 1 ) then

!
      do i=1,im
        if ( flag(i) ) then
          tem     = 1.0 / rho_a(i)
          hflx(i) = hflx(i) * tem * cpinv
          evap(i) = evap(i) * tem * hvapi
        endif
      enddo
!
!     if (lprnt) print *,' tskin=',tskin(ipr)

      return
      end
