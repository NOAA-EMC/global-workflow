!**********************************************************************c
      SUBROUTINE CALVIS_GSD(CZEN,VIS)

! SUBPROGRAM:    CALVIS      CALCULATE HORIZONTAL VISIBILITY   
!
!   PRGMMR:  BENJAMIN, STAN ORG: NOAA/FSL       DATE: 99-09-07
!
! ABSTRACT:
!
!   Started with Stoelinga-Warner algorithm for hydrometeors only.
!    Added coefficients for graupel.
!    Added algorithm for clear-air RH-based visibility.
!
!   This routine computes horizontal visibility (in km) at the
!   surface or lowest model layer, from qc, qr, qi, qs, and qg.  
!   qv--water vapor mixing ratio (kg/kg)
!   qc--cloud water mixing ratio (kg/kg)
!   qr--rain water mixing ratio  (kg/kg)
!   qi--cloud ice mixing ratio   (kg/kg)
!   qs--snow mixing ratio        (kg/kg)
!   qg--graupel mixing ratio     (kg/kg)
!   u/v - u/v wind components    (m/s)  
!   tb --            temperature (k)
!   pp--pressure                 (Pa)
!   rhb-- relative humidity      (0-100%)
!
!
!   Independent of the above definitions, the scheme can use different
!   assumptions of the state of hydrometeors:
!        meth='r': Uses the four mixing ratios qrain, qsnow, qclw,
!           and qclice
!
!   The routine uses the following
!   expressions for extinction coefficient, beta (in km**-1),
!   with C being the mass concentration (in g/m**3):
!
!      cloud water:  beta = 144.7 * C ** (0.8800)
!      rain water:   beta =  2.24 * C ** (0.7500)
!      cloud ice:    beta = 327.8 * C ** (1.0000)
!      snow:         beta = 10.36 * C ** (0.7776)
!      graupel:      beta =  8.0  * C ** (0.7500)
!
!   These expressions were obtained from the following sources:
!
!      for cloud water: from Kunkel (1984)
!      for rainwater: from M-P dist'n, with No=8e6 m**-4 and
!         rho_w=1000 kg/m**3
!      for cloud ice: assume randomly oriented plates which follow
!         mass-diameter relationship from Rutledge and Hobbs (1983)
!      for snow: from Stallabrass (1985), assuming beta = -ln(.02)/vis
!      for graupel: guestimate by John Brown and Stan Benjamin,
!         similar to snow, but a smaller extinction coef seemed
!         reasonable.  27 Aug 99
!
!   The extinction coefficient for each water species present is
!   calculated, and then all applicable betas are summed to yield
!   a single beta. Then the following relationship is used to
!   determine visibility (in km), where epsilon is the threshhold
!   of contrast, usually taken to be .02:
!
!      vis = -ln(epsilon)/beta      [found in Kunkel (1984)]
!
! HISTORY
! PROGRAM HISTORY LOG:
!    99-05-                        Version from Eta model and from 
!                                    Mark Stoelinga and Tom Warner
!    99-09-07      S. Benjamin     Modified for MM5 microphysics variables
!                                    include graupel mixing ratio
!    99-09         S. Benjamin     Added algorithm for RH-based clear-air
!                                    visibility
!    00-08         S. Benjamin     Added mods for base of 60km instead of 90km,
!                                    max RH from lowest 2 levels instead of
!                                    lev 2 only, max hydrometeor mix ratio
!                                    from lowest 5 levs instead of lev 1 only
!                                  Based on Schwartz stats and Smirnova et al
!                                    paper, and on METAR verif started this week
!    Dec 03        S. Benjamin     - updates
!                              - day/night distinction for vis constants
!                                  from Roy Rasmussen
!                              - low-level wind shear term 
!                                  - recommended by Evan Kuchera
!                           
!------------------------------------------------------------------
!

      use vrbls3d, only: qqw, qqi, qqs, qqr, qqg, t, pmid, q, u, v, extcof55
      use params_mod, only: h1, d608, rd
      use ctlblk_mod, only: jm, im, jsta_2l, jend_2u, jsta, jend, lm, &
                            modelname

      implicit none

      integer :: j, i, k, ll
      real :: tx, pol, esx, es, e
      REAL VIS(IM,jsta_2l:jend_2u) ,RHB(IM,jsta_2l:jend_2u,LM), CZEN(IM,jsta_2l:jend_2u)


      real celkel,tice,coeflc,coeflp,coeffc,coeffp,coeffg
      real exponlc,exponlp,exponfc,exponfp,exponfg,const1
      real rhoice,rhowat,qrain,qsnow,qgraupel,qclw,qclice,tv,rhoair,  &
        vovermd,conclc,conclp,concfc,concfp,concfg,betav

      real coeffp_dry, coeffp_wet, shear_fac, temp_fac
      real coef_snow, shear

      real coefrh,qrh,visrh
      real rhmax,shear5_cnt, shear8_cnt
      real shear5_cnt_lowvis, shear8_cnt_lowvis
      real shear4_cnt, shear4_cnt_lowvis
      integer night_cnt, lowsun_cnt

      real visrh10_cnt, vis1km_cnt, visrh_lower
      real vis3km_cnt
      real vis5km_cnt
      real vis_min, visrh_min
      real vis_night, zen_fac
!------------------------------------------------------------------

      CELKEL     = 273.15
      TICE       = CELKEL-10.
      COEFLC     = 144.7
      COEFLP     =   2.24
      COEFFC     = 327.8
      COEFFP     =  10.36

! - Initialize counters

      shear4_cnt = 0 
      shear5_cnt = 0 
      shear8_cnt = 0
      shear4_cnt_lowvis = 0
      shear5_cnt_lowvis = 0 
      shear8_cnt_lowvis = 0
      night_cnt = 0 
      lowsun_cnt = 0
      visrh10_cnt = 0 
      visrh_lower = 0
      vis1km_cnt = 0 
      vis3km_cnt = 0
      vis5km_cnt = 0

! - values from Roy Rasmussen - Dec 2003
!      COEFFP_dry =  17.7
!      COEFFP_wet =   4.18
! - modified number - Stan B. - Dec 2007
!     after quick talks with John Brown and Ismail Gultepe
      COEFFP_dry =  10.0
      COEFFP_wet =   6.0


!     COEFFg     =   8.0
! - values from Roy Rasmussen - Dec 2003
      COEFFg     =   4.0

      EXPONLC    =   0.8800
      EXPONLP    =   0.7500
      EXPONFC    =   1.0000
!     EXPONFP    =   0.7776
! - new value from Roy Rasmussen - Dec 2003  
      EXPONFP    =   1.0

      EXPONFg    =   0.75  
!     CONST1=-LOG(.02)
      CONST1= 3.912

! visibility with respect to RH is
!   calculated from optical depth linearly
!   related to RH as follows:

!    vis = 60 exp (-3 * (RH-15)/80)
!       changed on 8/23/00
!    vis = 60 exp (-2.5 * (RH-15)/80)
!       changed on 3/14/01
!    Previous algorithm gave vis of 3km, not 5 km
!         at 95% RH, now fixed.  Stan B.

!  coefficient of 3 gives visibility of 5 km
!    at 95% RH

! Total visibility is minimum of vis-rh
!   and vis-hydrometeors from Stoelinga/Warner

      RHOICE=917.
      RHOWAT=1000.

      vis_min = 1.e6
      visrh_min = 1.e6
 
      DO J=jsta,jend
      DO I=1,IM
!  - take max hydrometeor mixing ratios in lowest 25 mb (lowest 5 levels)
!  - change - 3/8/01 - Stan B.  - based on apparent underforecasting
!      of visibility (vis too low from 20km RUC)
!  - take max hydrometeor mixing ratios in lowest 15 mb (lowest 4 levels)
      qrain = 0.
      qsnow = 0.
      qgraupel = 0.
      qclw = 0.
      qclice = 0.

!???? - Stan B. - questions as of 1/1/04
!???? -  use mean of these values over
!         lowest 2 levels

!          do k = 1,4
!           QRAIN   = qrain+QR(I,J,k)
!           QSNOW   = qsnow+max(0.,QS(I,J,k) )
!           Qgraupel= qgraupel+Qg(I,J,k)
!          end do

          do k = 1,3
               LL=LM-k+1
            QCLW    = max(qclw, QQW(I,J,ll) )
            QCLICE  = max(qclice, QQI(I,J,ll) )
            Qsnow   = max(qsnow, QQS(I,J,ll) )
            Qrain   = max(qrain, QQR(I,J,ll) )
            Qgraupel   = max(qgraupel, QQG(I,J,ll) )
! - compute relative humidity
        Tx=T(I,J,LL)-273.15
        POL = 0.99999683       + TX*(-0.90826951E-02 +    &
           TX*(0.78736169E-04   + TX*(-0.61117958E-06 +   &
           TX*(0.43884187E-08   + TX*(-0.29883885E-10 +   &
           TX*(0.21874425E-12   + TX*(-0.17892321E-14 +   &
           TX*(0.11112018E-16   + TX*(-0.30994571E-19)))))))))
        esx = 6.1078/POL**8

          ES = esx
          E = PMID(I,J,LL)/100.*Q(I,J,LL)/(0.62197+Q(I,J,LL)*0.37803)
          RHB(I,J,LL) = 100.*AMIN1(1.,E/ES)

          enddo

!         qrain     = max(0., qrain / 4.)
!         qsnow     = max(0., qsnow / 4.)
!         qgraupel  = max(0., qgraupel / 4.)
!         qclw      = max(0., qclw / 2.)
!         qclice    = max(0., qclice / 2.)

!  - take max RH of levels 1 and 2 near the sfc
          rhmax = max (rhb(i,j,lm),rhb(i,j,lm-1))
!          rhmax = max (rhb(i,j,1),rhb(i,j,2))
!  - vary RH coefficient between 75% up to max at 95%
!      to give 5.4 km visibility at 95%.
!         qrh = max(0.0,min(1.0,(rhb(i,j)/100.-0.15)/0.80))
          qrh = max(0.0,min(0.8,(rhmax/100.-0.15)))

!15aug11          visrh = 80. * exp(-2.5*qrh)
!        visrh = 60. * exp(-2.5*qrh)
!tgs 23 feb 2017 - incrrease of base value to 90 km to reduce effect
!                  from RH visibility.
       IF(MODELNAME  == 'RAPR') then
          visrh = 90. * exp(-2.5*qrh)
       else
          visrh = 60. * exp(-2.5*qrh)
       endif

!  -- add term to increase RH vis term for
!     low-level wind shear increasing from 4 to 6 ms-1
!     (using Evan Kuchera's paper as a guideline)

! -- calculate term for shear between levels 1 and 4
!   (about 15 mb)
         shear = sqrt( (u(i,j,lm-3)-u(i,j,lm))**2         &
                     +(v(i,j,lm-3)-v(i,j,lm))**2  )
!         shear = sqrt( (u(i,j,4)-u(i,j,1))**2
!     1               +(v(i,j,4)-v(i,j,1))**2  )

!       shear_fac = min(1.,max(0.,(shear-5.)/3.) )
        shear_fac = min(1.,max(0.,(shear-4.)/2.) )
        if (visrh.lt.10.) visrh = visrh + (10.-visrh)*    &
           shear_fac

        if (shear.gt.4.) shear4_cnt = shear4_cnt +1
        if (shear.gt.5.) shear5_cnt = shear5_cnt +1
        if (shear.gt.6.) shear8_cnt = shear8_cnt +1

        if (shear.gt.4..and.visrh.lt.10)                  &
          shear4_cnt_lowvis = shear4_cnt_lowvis +1
        if (shear.gt.5..and.visrh.lt.10)                  &
          shear5_cnt_lowvis = shear5_cnt_lowvis +1
        if (shear.gt.6..and.visrh.lt.10)                  &
          shear8_cnt_lowvis = shear8_cnt_lowvis +1

        if (visrh.lt.10.) visrh10_cnt = visrh10_cnt+1
!new
        if (czen(i,j).lt.0.) night_cnt = night_cnt + 1
        if (czen(i,j).lt.0.1) lowsun_cnt = lowsun_cnt + 1

        TV=T(I,J,lm)*(H1+D608*Q(I,J,lm))
!        tv = t(i,j,1)*(1. + 0.6078*q(i,j,1))

        RHOAIR=PMID(I,J,lm)/(RD*TV)
!        RHOAIR=PMID(I,J,1)/(RD*TV)

          VOVERMD=(1.+Q(I,J,lm))/RHOAIR+(QCLW+QRAIN)/RHOWAT+    &
!          VOVERMD=(1.+Q(I,J,1))/RHOAIR+(QCLW+QRAIN)/RHOWAT+
                  (qgraupel+QCLICE+QSNOW)/RHOICE
          CONCLC=QCLW/VOVERMD*1000.
          CONCLP=QRAIN/VOVERMD*1000.
          CONCFC=QCLICE/VOVERMD*1000.
          CONCFP=QSNOW/VOVERMD*1000.
          CONCFg=Qgraupel/VOVERMD*1000.

          temp_fac = min(1.,max((t(i,j,lm)-271.15),0.) )
!          temp_fac = min(1.,max((t(i,j,1)-271.15),0.) )
         coef_snow = coeffp_dry*(1.-temp_fac)                   &
                   + coeffp_wet* temp_fac    

          if (t(i,j,lm).lt. 270. .and. temp_fac.eq.1.)          &
!          if (t(i,j,1).lt. 270. .and. temp_fac.eq.1.)
             write (6,*) 'Problem w/ temp_fac - calvis'

!       BETAV=COEFFC*CONCFC**EXPONFC+COEFFP*CONCFP**EXPONFP

        BETAV=COEFFC*CONCFC**EXPONFC                            &
             + coef_SNOW*CONCFP**EXPONFP                        &
             + COEFLC*CONCLC**EXPONLC+COEFLP*CONCLP**EXPONLP    &
             + coeffg*concfg**exponfg  +1.E-10

       if (i.eq.290 .and. j.eq.112) then
         write (6,*) 'BETAV, extcof55 =',BETAV,extcof55(i,j,lm)
       end if

        VIS(I,J)=MIN(90.,CONST1/BETAV+extcof55(i,j,lm))      ! max of 90km

        if (vis(i,j).lt.vis_min) vis_min = vis(i,j)
        if (visrh.lt.visrh_min) visrh_min = visrh

        if (visrh.lt.vis(i,j)) visrh_lower = visrh_lower + 1


! -- Dec 2003 - Roy Rasmussen (NCAR) expression for night vs. day vis
!   1.609 factor is number of km in mile.
       vis_night = 1.69 * ((vis(i,j)/1.609)**0.86) * 1.609

       zen_fac = min(0.1,max(czen(i,j),0.))/ 0.1
       if (i.eq.290 .and. j.eq.112) then
         write (6,*) 'zen_fac,vis_night, vis =',zen_fac,vis_night, vis(i,j)
       end if

       vis(i,j) = zen_fac * vis(i,j) + (1.-zen_fac)*vis_night

       if (i.eq.290 .and. j.eq.112) then
         write (6,*) 'visrh, vis =',visrh, vis(i,j)
       end if

        vis(i,j) = min(vis(i,j),visrh)

        if (vis(i,j).lt.1.) vis1km_cnt = vis1km_cnt + 1
        if (vis(i,j).lt.3.) vis3km_cnt = vis3km_cnt + 1
        if (vis(i,j).lt.5.) vis5km_cnt = vis5km_cnt + 1
! convert to [m]
        vis(i,j) = vis(i,j) * 1000.

      ENDDO
      ENDDO

!      write (6,*)
!      write (6,*) ' Visibility diagnostics follow: ------------'
!      write (6,*) ' -------------------------------------------'
!      write (6,*)                                                   &
!       '                                   any vis  /  vis < 10 km '
!      write (6,*)'No. of grid pts with shear (lev4-1) > 4m/s',      &
!          shear4_cnt, shear4_cnt_lowvis
!      write (6,*)'No. of grid pts with shear (lev4-1) > 5m/s',      &
!          shear5_cnt, shear5_cnt_lowvis
!      write (6,*)'No. of grid pts with shear (lev4-1) > 6m/s',      &
!          shear8_cnt, shear8_cnt_lowvis
!      write (6,*)
!      write (6,*)'No. of grid pts with vis-RH < 10 km',             &
!          visrh10_cnt
!      write (6,*)'No. of grid pts with vis    <  1 km',             &
!          vis1km_cnt
!      write (6,*)'No. of grid pts with vis    <  3 km',             &
!          vis3km_cnt
!      write (6,*)'No. of grid pts with vis    <  5 km',             &
!          vis5km_cnt
!      write (6,*)
!      write (6,*)'Min vis-hydrometeor, vis-RH', vis_min, visrh_min
!
!      write (6,*)'No. of grid pts with visRH < vis(hydrometeor)',   & 
!          visrh_lower
!      write (6,*)'% grid pts with night/cos(zen) < 0.1',            &
!          float(night_cnt)/float(IM*JM),float(lowsun_cnt)/          &
!          float(IM*JM)
!      write (6,*)
!
      RETURN
      END
