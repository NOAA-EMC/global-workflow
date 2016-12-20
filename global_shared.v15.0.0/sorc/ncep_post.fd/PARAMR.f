!    SUBPROGRAM DOCUMENTATION BLOCK

!      HISTORY
 
!        Original MM5 code received frm Roelof Bruintjes, 8 Jan 96.

!        1996 - early 1998: Numerous small changes by John B., Tanya Smirnova
!                           and Stan Benjamin.

!        2000 Jan: Changes by John B.
!                  in conjunction with major revamping of EXMOISG
!                  by Roy Rasmussen, Greg Thompson, Kevin Manning of NCAR.  
 
!      PURPOSE: Sets utility constants and physical parameters used in 
!               EXMOISG, mixed phase microphysics routine. 

!      CALLING ROUTINES: INITHYBV
 
!      SUBPROGRAMS CALLED: fgamma (Computes Gamma function) 

!      REMARKS:   

             SUBROUTINE PARAMR
!      module PARAMR_mod


      use pmicrph_mod, only: pi, r1, ron, ron2, son, gon, br, bs, bg, arain,&
              asnow, agraupel, rho_not, drain, dsnow, xr0s, xr0g, xm01, xm0s,&
              xm0g, diace_min, topr, tops, topg, ron_min, qr0, dgraupel, dice,&
              drain2, dsnow2, delqr0, const1r, const2r, const1a, const1b,&
              const_ns1, const_ns2, const_ng1, const_ng2, slor_r1, slos_r1,&
              tno, ato, int0, berc1, bp, ap, cnp, xnu, frd1, fra1, efis, efir,&
              efsr, efcs, slog_r1, efgi, efgc, efgr, efgs, efcr, acris, bacris,&
              cir, cirf, cpiacr0, cpiacr1, cpiacr2, cpiacr3, frain, fsnow,&
              fgraupel, csr, alpha1, beta1, gamma3, crs, acrcs, bacrcs, acrcs_new,&
              acrls, acrcg, bacrcg, bacrcs_new, bacls, acrcg_new, bacrcg_new,&
              acrig, bacrig, crg, csg, depg1, depg2, depg3, depg4, deps1, deps2,&
              deps3, deps4, c1, xsmax, qck1, qcth, acrcr, bacrcr, depr1, depr2,&
              depr3, depr4, psm1, psm2, psm3, psm4, pgm1, pgm2, pgm3, pgm4,&
              cw, hgfr

      implicit none

     REAL ABER1(31),ABER2(31)
      REAL fgamma
!     REAL xnu      ! Added 01 Dec 2004  GREG T.
!     COMMON/BER/ ABER1(31),ABER2(31)
! LOOKUP TABLE FOR A1 AND A2 IN BERGERGON PROCESS


      REAL GI,GS,GR,GG

!jmb--declare local variables
      real consta,constd,constgb
      real cpiacr7,cpiacr8,cpiacr9
      real const1,const2

      DATA ABER1/.7939E-07,.7841E-06,.3369E-05,.4336E-05,      &
       .5285E-05,.3728E-05,.1852E-05,.2991E-06,.4248E-06,      &
       .7434E-06,.1812E-05,.4394E-05,.9145E-05,.1725E-06,      &
       .3348E-04,.1725E-04,.9175E-05,.4412E-05,.2252E-05,      &
       .9115E-06,.4876E-06,.3473E-06,.4758E-06,.6306E-06,      &
       .8573E-06,.7868E-06,.7192E-06,.6513E-06,.5956E-06,      &
       .5333E-06,.4834E-06/

      DATA ABER2/.4006,.4831,.5320,.5307,.5319,.5249,          &
       .4888,.3894,.4047,.4318,.4771,.5183,.5463,.5651,        &
       .5813,.5655,.5478,.5203,.4906,.4447,.4126,.3960,        &
       .4149,.4320,.4506,.4483,.4460,.4433,.4413,.4382,        &
       .4361/
!gt
!gt  The value .4506 replaces .4406 which was a typo - check original
!gt  Koenig, 1971 paper.
!gt

      PI=ACOS(-1.)

!     Min value for hydrometeor mixing ratios
      R1 = 1.E-15
 
! SLOPE INTERCEPT FOR RAIN, SNOW, AND GRAUPEL

!jmb--Roy R. suggests a larger value for the slope-intercept for rain.
!      This will slow down the fall speed.--16dec98
      RON=8.E6          ! Original M-P value.
!gt   RON2=1.E10        ! GREG T.  MWR Part1 vrbl intercept
      RON2=1.E9         ! GREG T.  changed 01 Dec 2004
!     SON=2.E6          ! Original M-P value.
      SON=2.E7
!jmb--According to Roy Rasmussens data (from a QJRMS paper he was reviewing)
!      the value of the M-P slope intercept can be as large as 3.E7 for 
!      graupel.  The value GON = 4.E6 as an upper bound on the intercept value
!      appears too small.  Use same value as for snow.--17oct96
!     GON=4.E6          ! Original M-P value.
!gt   GON=5.e7          ! Roy R., summer 1998, 19 Jan 00, Oct 00
      GON=4.E6          ! Original M-P value.  GREG T.  changed 01 Dec 2004

! EXPONENT FOR RAIN, SNOW, AND GRAUPEL, IN FALL SPEED V(D)=A*D**B
!     THIS FROM SEKHON AND SRIVASTAVA (1970,JAS)

      BR=0.8
      BS=0.41
      BG=0.37

! A IN FALL SPEED

      ARAIN=842.
      ASNOW=11.72
      AGRAUPEL=19.3

!jmb--Standard density (p/RT, values from ICAO standard atmosphere) 
!      used in computing density correction to fall
!      speeds--22jan99

      rho_not = 101325.0/(287.0586*298.0) 
 
! DENSITY OF RAIN, SNOW, AND GRAUPEL

      DRAIN=1000.
      DSNOW=100.
      DGRAUPEL=400.
!     DICE=150.
      DICE=500.
!jmb--added square of rain and snow densities--24jun96
      drain2=drain*drain
      dsnow2=dsnow*dsnow

!      SMALLEST SIZE OF SNOW AND GRAUPEL (RADIUS, METERS)
!gt   XR0S=0.75E-4
!gt   XR0G=0.457E-4
      XR0S= 75.E-6
      XR0G=150.E-6
!      MINIMUM MASS OF ICE, SNOW, GRAUPEL
      XM01=1.0E-12
      XM0S=4.*PI/3.*DSNOW*XR0S**3     !                 GREG T.
      XM0G=4.*PI/3.*DGRAUPEL*XR0G**3  !                 GREG T.
      DIACE_min = 2.0 * (3.0*XM01/(4.0*PI*DICE))**0.3333   !   GREG T.

! TOP OF SLOPE FOR RAIN, SNOW, AND GRAUPEL
!jmb--By top of slope is meant numerator Marshall-Palmer slope parameter
!      [See (4) in Reisner et al 1998, QJRMS].

      TOPR=PI*DRAIN*RON
      TOPS=PI*DSNOW*SON
      TOPG=PI*DGRAUPEL*GON

!CONSTANTS FOR VARIABLE RON

!jmb  qr0 is center value of rain mixing ratio for transition from
!       M-P slope-intercept for drizzle formed by a collision-coalescence
!       process to M-P slope-intercept for traditional rain.--nov00
!jmb  delqr0 governs sharpness of transition: small delt_qr0 makes the
!      transition sharper:
!      if the rate of change of zero intercept wrt rain mixing ratio
!      were linear, with the slope at QR0 given by present tanh formula,
!      the transition would occur between qr0-delqr0 and qr0+delqr0.--nov00
!gt   RON_min = RON
      RON_min = 2.e7
!gt   qr0 = 0.0001  !  Roy R. 26oct00
      qr0 = 0.0002  !  GREG T.  01 Dec 2004
!gt   delqr0 = 0.25*qr0
      delqr0 = 0.5*qr0    !  GREG T.  01 Dec 2004
      const1r=(ron2-ron_min)*0.5
      const2r=(ron2+ron_min)*0.5

!CONSTANTS FOR VARIABLE SON

!     CONST1A is for unit conversion of Sekhon and Srivastava (1970, JAS, Eq 45)
!        from M-P intercept in [m^-3 mm^-1] to [m^-4], and rain rate in 
!        [mm/h] to [m/s].
      CONST1A=2.5E6*(1./1000*1./3600.)**(0.94)  ! = 1.718
      GI = 4.+BS
      CONSTA=fgamma(GI)
      CONST1B=ASNOW*CONSTA/6.
      CONST1=2.5E6*(1./1000*1./3600.)**(0.94)  ! = 1.718 Reisner et al (5)
      CONST2=fgamma(4.+BS)
      const_ns1 = (const1*                                              &
       (((asnow*const2)/(6.*drain*((pi*dsnow)**(0.25*bs))))**(-0.94)))  &
       **(4./(4. - 0.94*bs))
      const_ns2 = -0.94*(1.+.25*bs)*(4./(4.-0.94*bs))
!     const_ns2 is exponent on the product rho*qnib (or rho*qnia)

!CONSTANTS FOR VARIABLE GON
!     Based on Roy R s formulation, Jun 96

      constd=1./0.52
      constgb=12./13.
      const_ng1=(1.57**constd)*((pi*dgraupel)**constgb)
      const_ng2=-constgb

!jmb--Specify inverse slope values when q_r, q_s and q_g are default 
!      small values.--22jan99
!jmb--Dry air density assumed unity.

      slor_r1=(1.*R1/TOPR)**0.25
      slos_r1=(1.*R1/TOPS)**0.25
      slog_r1=(1.*R1/TOPG)**0.25
      
! CONSTANT IN FLETCHER CURVES

!c    TNO=1.E-2    
!c    ATO=0.6     

! CONSTANT IN COOPER CURVES   !  GREG T.
      TNO=5.0                 !  GREG T.
      ATO=0.304               !  GREG T. 

! CONSTANTS FOR BERGERON PROCESS

      INT0=273
      BERC1=PI*50.0E-06*50.0E-06

! FREEZING OF CLOUD DROPLETS, MURAKAMI (1989) 
      BP=100.
      AP=0.66
      CNP=100.E6              !  GREG T.  Jul98
!gt
!gt  Berry and Reinhardt autoconversion uses this cloud drop
!gt  shape parameter (effectively a measure of dispersion) and
!gt  if CNP is changed, be sure to change the section of code
!gt  in EXMOISG too.
!gt
      xnu = max(0.0, (CNP*1E-6 - 100.)/100.)

! FREEZING OF RAIN DROPLETS, LIN ET AL (45)

      FRD1=PI*PI*20.*BP*RON
      FRA1=AP

! COLLECTION EFFICIENCIES

      EFIS=0.1
      EFIR=1.0
      EFSR=1.0
      EFCS=1.0
      EFGI=0.1
      EFGC=1.0
      EFGR=1.0
      EFGS=0.1
      EFCR=1.0


! COLLECTION OF CLOUD ICE BY SNOW

      GI=3.+BS
      GS=fgamma(GI)
      ACRIS=0.25*PI*ASNOW*EFIS*SON*GS
      BACRIS=3+BS

! COLLECTION OF CLOUD ICE BY RAIN

      CIR=0.25*PI*EFIR*RON

! RATE AT WHICH RAIN IS FROZEN BY COLLISION WITH CLOUD ICE

      CIRF=1./24.*PI*PI*DRAIN*RON*EFIR
!jmb--Additional constants for PIACR--26may97
      cpiacr0 = cirf*.267*120.
      cpiacr7 =  6.*5.15e3/.267
      cpiacr8 =  6.*7.*1.0225E6/.267
      cpiacr9 =  6.*7.*8.*7.55e7/.267
      cpiacr1 =  cpiacr7
      cpiacr2 =  cpiacr8/cpiacr7
      cpiacr3 =  cpiacr9/cpiacr8
 

! PARAMETERS FOR MEAN FALL SPEED

      GI=4.+BR
      GR=fgamma(GI)
      FRAIN=ARAIN*GR/6.
      GI=4.+BS
      GS=fgamma(GI)
      FSNOW=ASNOW*GS/6.
      GI=4.+BG
      GG=fgamma(GI)
      FGRAUPEL=AGRAUPEL*GG/6.

! COLLECTION OF SNOW BY RAIN

      CSR=PI*PI*EFSR*DRAIN*RON*SON
      ALPHA1=1.2
      BETA1=0.95
!jmb--Changed GAMMA1-->GAMMA3 in FSL version of PARAMR 
!jmb   to avoid conflicts with GAMMA1 in CUP.--12feb99
!jmb****GAMMA1 is used in MM5 EXMOISG in loop 30 for PSACR, PRACS.  Make sure
!jmb**** these are changed to GAMMA3 in FSL version.--26jan00
      GAMMA3=0.08

! COLLECTION OF RAIN BY SNOW

      CRS=PI*PI*EFSR*DSNOW*RON*SON

! COLLECTION OF CLOUD WATER BY SNOW

!     Old particle size distribution for snow 
      GI=BS+3.  
      GS=fgamma(GI)
      ACRCS=0.25*PI*ASNOW*EFCS*SON*GS
      BACRCS=3.+BS
 
!     New particle size distribution for snow (Roy R., Jul 99)   
      GI=BS+4.  
      GS=fgamma(GI)
      ACRCS_new=0.25*PI*ASNOW*EFCS*SON*GS
      BACRCS_new=4.+BS  !  New particle size distribution for snow 

! LOSS OF SNOW DUE TO COLLISION WITH CLOUD WATER 
!jmb--These constants follow Reisner et al (1998) (A.43)--20jan99
      GI = 2.*BS + 2.0
      GS = fgamma(GI)
      ACRLS = 3.0*PI*SON*GS*ASNOW*ASNOW
      BACLS = GI
!c      RMC=4.E-12
!c      GI=6.+BS
!c      GS=GAMMA(GI)
!c      ACRLS=(1./24.)*PI*PI*EFCS*SON*ASNOW*DSNOW*GS
!c      BACLS=6.+BS

! COLLECTION OF CLOUD WATER BY GRAUPEL

!   Old particle size distribution for graupel 
!gt As of 01 Dec 2004, again use this, not new gamma distrib below - GREG T.
      GI=3.+BG   
      GG=fgamma(GI)
      ACRCG=0.25*PI*AGRAUPEL*EFGC*GON*GG
      BACRCG=3.+BG 

!   New particle size distribution for graupel (Roy R, Jul 99)
      GI=4.+BG   
      GG=fgamma(GI)
      ACRCG_new=0.25*PI*AGRAUPEL*EFGC*GON*GG
      BACRCG_new=4.+BG 

! COLLECTION OF CLOUD ICE BY GRAUPEL

      GI=3.+BG
      GG=fgamma(GI)
      ACRIG=0.25*PI*AGRAUPEL*EFGI*GON*GG
      BACRIG=3.+BG

! COLLECTION OF RAIN BY GRAUPEL

      CRG=PI*PI*EFGR*DRAIN*RON*GON

! COLLECTION OF SNOW BY GRAUPEL

      CSG=PI*PI*EFGS*DSNOW*SON*GON

! DEPOSITIONAL GROWTH OF GRAUPEL

      GI=BG/2.+2.5
      GG=fgamma(GI)
      DEPG1=2*PI*GON
      DEPG2=AGRAUPEL
      DEPG3=0.31*GG
      DEPG4=BG/2.+2.5

! DEPOSITIONAL GROWTH OF SNOW

      GI=BS/2.+2.5
      GS=fgamma(GI)
      DEPS1=4.*SON
      DEPS2=ASNOW
      DEPS3=0.44*GS
      DEPS4=BS/2.+2.5

! AGGREGATION OF CLOUD ICE

      C1=700.*0.1*0.25/DICE   !   GREG T. to conform to R:Eq (A.33)  Jul98 

! AUTOCONVERSION TO SNOW

      XSMAX=9.4E-10

! AUTOCONVERSION OF CLOUD WATER TO RAINWATER [Reisner et al 1998, (A.60)]
!jmb--Moved QCK1 and QCTH definition from INITHYBV to here to keep it with 
!      other setting of microphysics constants.

      QCK1 = 1.E-3  
!c    QCTH = .00010    !   Used in RUC Apr 98 -
!c    QCTH = .00015    !   Suggested by Greg T., 26feb99
!c    QCTH = .00025    !   Desired by Roy R., 12 Jan 00
      QCTH = .00035    !   Desired by Roy R., Oct 00
!gt                    !   No longer used, replaced by Berry&Reinhardt - GREG T.

! COLLECTION OF CLOUD WATER BY RAIN

      GI=3.+BR
      GR=fgamma(GI)
      ACRCR=0.25*PI*ARAIN*EFCR*RON*GR
      BACRCR=3.+BR


! DEPOSITIONAL GROWTH OF RAIN

      GI=BR/2.+2.5
      GR=fgamma(GI)
      DEPR1=2*PI*RON
      DEPR2=ARAIN
      DEPR3=0.31*GR
      DEPR4=BR/2.+2.5

! FOR MELTING OF SNOW 

      GI=BS/2.+2.5
      GS=fgamma(GI)
      PSM1=2*PI*SON
      PSM2=ASNOW
      PSM3=0.44*GS
      PSM4=BS/2.+2.5

! FOR MELTING OF GRAUPEL

      GI=BG/2.+2.5
      GG=fgamma(GI)
      PGM1=2*PI*GON
      PGM2=AGRAUPEL
      PGM3=0.31*GG
      PGM4=BG/2.+2.5

! CONSTANT FOR ENHANCED MELTING OF GRAUPEL BY RAIN AND CLOUD WATER

      CW=4218.
   

! CONSTANT FOR HOMOGENEOUS FREEZING OF CLOUD DROPLETS

      HGFR=233.15

      RETURN
      END
!      end module PARAMR_mod
