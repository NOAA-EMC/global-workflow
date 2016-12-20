      SUBROUTINE CALMICT_new(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL,     &
                        QW1,QI1,QR1,QS1,DBZ1,DBZR1,DBZI1,DBZC1,NLICE1,NRAIN1)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALMIC      COMPUTES HYDROMETEORS 
!   PRGRMMR: JIN         ORG: W/NP2      DATE: 01-08-14       
!     
! ABSTRACT:  
!     THIS ROUTINE COMPUTES THE MIXING RATIOS OF CLOUD WATER,
!     CLOUD ICE, RAIN, AND SNOW.  THE CODE IS BASED ON SUBROUTINES
!     GSMDRIVE & GSMCOLUMN IN THE NMM MODEL. 
!     
! PROGRAM HISTORY LOG:
!   01-08-14  YI JIN 
!   02-02-11  Brad Ferrier - Minor changes for consistency w/ NMM model
!   04-11-10  Brad Ferrier - Removed cloud fraction algorithm
!   04-11-17  H CHUANG - WRF VERSION     
!   14-03-11  B Ferrier - Created new & old versions of this subroutine
!                         to process new & old versions of the microphysics
!
! USAGE:    CALL CALMICT_new(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL
!     &,                QW1,QI1,QR1,QS1,DBZ1,DBZR1,DBZI1,DBZC1)
!   INPUT ARGUMENT LIST:
!     P1D     - PRESSURE (PA)
!     T1D     - TEMPERATURE (K)
!     Q1D     - SPECIFIC HUMIDITY (KG/KG)
!     C1D     - TOTAL CONDENSATE (CWM, KG/KG)
!     FI1D    - F_ice (fraction of condensate in form of ice)
!     FR1D    - F_rain (fraction of liquid water in form of rain)
!     FS1D    - F_RimeF ("Rime Factor", ratio of total ice growth 
!                       to deposition growth)
!     CUREFL  - Radar reflectivity contribution from convection (mm**6/m**3)
!
!   OUTPUT ARGUMENT LIST:
!     QW1   - CLOUD WATER MIXING RATIO (KG/KG)
!     QI1   - CLOUD ICE MIXING RATIO (KG/KG)
!     QR1   - RAIN MIXING RATIO (KG/KG)
!     QS1   - "SNOW" (precipitation ice) MIXING RATIO (KG/KG)
!     DBZ1  - Equivalent radar reflectivity factor in dBZ; i.e., 10*LOG10(Z)
!     DBZR  - Equivalent radar reflectivity factor from rain in dBZ
!     DBZI  - Equivalent radar reflectivity factor from ice (all forms) in dBZ
!     DBZC  - Equivalent radar reflectivity factor from parameterized convection in dBZ
!
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     FUNCTIONS:
!        FPVS
!     UTILITIES:
!     LIBRARY:
!       NONE
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM SP
!$$$  
!
      use params_mod, only: dbzmin, epsq, tfrz, eps, rd, d608
      use ctlblk_mod, only: jsta, jend, jsta_2l,jend_2u,im
      use cmassi_mod, only: t_ice, rqr_drmin, n0rmin, cn0r_dmrmin,      &
              mdrmin, rqr_drmax, cn0r_dmrmax, mdrmax, n0r0, xmrmin,     &
              xmrmax, massi, cn0r0, mdimin, xmimax, mdimax 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      INTEGER INDEXS, INDEXR
!aligo
      REAL, PARAMETER :: Cice=1.634e13, Cwet=1./.189, Cboth=Cice/.224,   &
     &  NLI_min=1.E3, RFmax=45.259, RQmix=0.1E-3,NSI_max=250.E3
!aligo
      real,dimension(IM,jsta_2l:jend_2u),intent(in)    :: P1D,T1D,Q1D,C1D,FI1D,FR1D, &
                                                          FS1D,CUREFL
      real,dimension(IM,jsta_2l:jend_2u),intent(inout) :: QW1,QI1,QR1,QS1,DBZ1,DBZR1,&
                                                          DBZI1,DBZC1,NLICE1,NRAIN1
      
      integer I,J
      real :: TC,Frain,Fice,RimeF,Xsimass,Qice,Qsat,ESAT,WV,RHO,RRHO,    &
     &        RQR,DRmm,Qsigrd,WVQW,Dum,XLi,Qlice,WC,DLI,NLImax,NSImax,   &
     &        RQLICE, N0r,Ztot,Zrain,Zice,Zconv,Zmin,Zmix,NLICE,NSmICE,  &
     &        QSmICE,NRAIN,NMIX,Zsmice
      logical :: LARGE_RF, HAIL
      real,external :: fpvs
!************************************************************************
!--- Determine composition of condensate in the form of cloud water, 
!    total ice (cloud ice & snow), & rain following GSMDRIVE in NMM model
!
      Zmin=10.**(0.1*DBZmin)
      DO J=JSTA,JEND
        DO I=1,IM
          QW1(I,J)=0.
          QI1(I,J)=0.
          QR1(I,J)=0.
          QS1(I,J)=0.
          NLICE1(I,J)=0.
          NRAIN1(I,J)=0.
          DBZ1(I,J)=DBZmin
          DBZR1(I,J)=DBZmin
          DBZI1(I,J)=DBZmin
          DBZC1(I,J)=DBZmin
        ENDDO
      ENDDO
      DO J=JSTA,JEND
        DO I=1,IM
          Ztot=0.             !--- Total radar reflectivity
          Zrain=0.            !--- Radar reflectivity from rain
          Zice=0.             !--- Radar reflectivity from ice
          Zsmice=0.           !--- Radar reflectivity from small ice
          Zconv=CUREFL(I,J)   !--- Radar reflectivity from convection
          IF (C1D(I,J) .LE. EPSQ) THEN
!
!--- Skip rest of calculatiions if no condensate is present
!
            GO TO 10
          ELSE
            WC=C1D(I,J)
          ENDIF
!
!--- Code below is from GSMDRIVE for determining:
!    QI1 - total ice (cloud ice & snow) mixing ratio
!    QW1 - cloud water mixing ratio
!    QR1 - rain mixing ratio
!
          TC=T1D(I,J)-TFRZ
          Fice=FI1D(I,J)
          Frain=FR1D(I,J)
          IF (TC.LE.T_ICE .OR. Fice.GE.1.) THEN
!          IF (Fice.GE.1.) THEN  
            QI1(I,J)=WC
          ELSE IF (Fice .LE. 0.) THEN
            QW1(I,J)=WC
          ELSE
            QI1(I,J)=Fice*WC
            QW1(I,J)=WC-QI1(I,J)
          ENDIF   
          IF (QW1(I,J).GT.0. .AND. Frain.GT.0.) THEN
            IF (Frain .GE. 1.) THEN 
              QR1(I,J)=QW1(I,J)
              QW1(I,J)=0.
            ELSE
              QR1(I,J)=Frain*QW1(I,J)
              QW1(I,J)=QW1(I,J)-QR1(I,J)
            ENDIF 
          ENDIF
          WV=Q1D(I,J)/(1.-Q1D(I,J))
!
!--- Saturation vapor pressure w/r/t water ( >=0C ) or ice ( <0C )
!
          ESAT=1000.*FPVS(T1D(I,J))
          QSAT=EPS*ESAT/(P1D(I,J)-ESAT)
          RHO=P1D(I,J)/(RD*T1D(I,J)*(1.+D608*Q1D(I,J)))
          RRHO=1./RHO
  !
  !--- Based on code from GSMCOLUMN in model to determine reflectivity from rain
  !
          RQR=0.
          IF (QR1(I,J) .GT. EPSQ) THEN
            RQR=RHO*QR1(I,J)
            IF (RQR .LE. RQR_DRmin) THEN
              N0r=MAX(N0rmin, CN0r_DMRmin*RQR)
              INDEXR=MDRmin
            ELSE IF (RQR .GE. RQR_DRmax) THEN
              N0r=CN0r_DMRmax*RQR
              INDEXR=MDRmax
            ELSE
              N0r=N0r0
              INDEXR=MAX( XMRmin, MIN(CN0r0*RQR**.25, XMRmax) )
            ENDIF
  !
  !--- INDEXR is the mean drop size in microns; convert to mm
  !
            DRmm=1.e-3*REAL(INDEXR)
  !
  !--- Number concentration of rain drops (convert INDEXR to m)
  !
            NRAIN=N0r*1.E-6*REAL(INDEXR)
            NRAIN1(I,J)=NRAIN
            Zrain=0.72*N0r*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm
          ENDIF        !--- End IF (QR1(I,J) .GT. EPSQ) block
!
!--- Based on code from GSMCOLUMN in model to determine partition of 
!    total ice into cloud ice & snow (precipitation ice)
!
          RQLICE=0.
          IF (QI1(I,J) .GT. EPSQ) THEN
            QICE=QI1(I,J)
!
!  ->  Small ice particles are assumed to have a mean diameter of 50 microns.
!  * INDEXS  - mean size of snow to the nearest micron (units of microns)
!  * RimeF   - Rime Factor, which is the mass ratio of total (unrimed &
!              rimed) ice mass to the unrimed ice mass (>=1)
!  * QTICE   - time-averaged mixing ratio of total ice
!  * QLICE   - mixing ratio of large ice
!  * RQLICE  - mass content of large ice
!  * NLICE1  - time-averaged number concentration of large ice
!
            IF (TC>=0.) THEN
   !
   !--- Eliminate small ice particle contributions for melting & sublimation
   !
              NSmICE=0.
              QSmICE=0.
            ELSE
!
!--- Max # conc of small ice crystals based on 10% of total ice content
!
!              NSImax=0.1*RHO*QICE/MASSI(MDImin)
!aligo
               NSImax=MAX(NSI_max,0.1*RHO*QICE/MASSI(MDImin) )
!aligo
!
!-- Specify Fletcher, Cooper, Meyers, etc. here for ice nuclei concentrations
!
              NSmICE=MIN(0.01*EXP(-0.6*TC), NSImax)       !- Fletcher (1962)
              DUM=RRHO*MASSI(MDImin)
              NSmICE=MIN(NSmICE, QICE/DUM)
              QSmICE=NSmICE*DUM
            ENDIF            ! End IF (TC>=0.) THEN
            QLICE=MAX(0., QICE-QSmICE)
            QS1(I,J)=QLICE
            QI1(I,J)=QSmICE
            RimeF=AMAX1(1., FS1D(I,J) )
            RimeF=MIN(RimeF, RFmax)
            RQLICE=RHO*QLICE
            DUM=XMImax*EXP(.0536*TC)
            INDEXS=MIN(MDImax, MAX(MDImin, INT(DUM) ) )
!
!-- Specify NLImax depending on presence of high density ice (rime factors >10)
!
            IF (RimeF>10.) THEN
              LARGE_RF=.TRUE.         !-- For convective precipitation
              NLImax=1.E3
            ELSE
              LARGE_RF=.FALSE.        !-- For non-convective precipitation
              DUM=MAX(TC, T_ICE)
              NLImax=10.E3*EXP(-0.017*DUM)
            ENDIF
            NLICE=RQLICE/(RimeF*MASSI(INDEXS))
            DUM=NLI_min*MASSI(MDImin)     !-- Minimum large ice mixing ratio
new_nlice:  IF (RQLICE<DUM) THEN
              NLICE=RQLICE/MASSI(MDImin)
            ELSE IF (NLICE<NLI_min .OR. NLICE>NLImax) THEN  new_nlice
!
!--- Force NLICE to be between NLI_min and NLImax, but allow for exceptions
!
              HAIL=.FALSE.
              NLICE=MAX(NLI_min, MIN(NLImax, NLICE) )
              XLI=RQLICE/(NLICE*RimeF)
new_size:     IF (XLI .LE. MASSI(MDImin) ) THEN
                INDEXS=MDImin
              ELSE IF (XLI .LE. MASSI(450) ) THEN   new_size
                DLI=9.5885E5*XLI**.42066         ! DLI in microns
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
              ELSE IF (XLI .LE. MASSI(MDImax) ) THEN   new_size
                DLI=3.9751E6*XLI**.49870         ! DLI in microns
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
              ELSE   new_size
                INDEXS=MDImax
                IF (LARGE_RF)  HAIL=.TRUE.
              ENDIF    new_size
no_hail:      IF (.NOT. HAIL) THEN
                NLICE=RQLICE/(RimeF*MASSI(INDEXS))   !-- NLICE > NLImax
              ENDIF    no_hail
            ENDIF      new_nlice
            NLICE1(I,J)=NLICE
   !
   !--- Equation (C.8) in Ferrier (1994, JAS, p. 272), which when
   !    converted from cgs units to mks units results in the same
   !    value for Cice, which is equal to the {} term below:
   !
   !    Zi={.224*720*(10**18)/[(PI*RHOL)**2]}*(RHO*QLICE)**2/NLICE1(I,J),
   !    where RHOL=1000 kg/m**3 is the density of liquid water
   !
   !--- Valid only for exponential ice distributions
   !
            IF (NSmICE > 0.) THEN
               Zsmice=Cice*RHO*RHO*QSmICE*QSmICE/NSmICE
            ENDIF
            if (NLICE1(I,J) /= 0.0) Zice=Cice*RQLICE*RQLICE/NLICE1(I,J)
            IF (TC>=0.) Zice=Cwet*Zice      ! increased for wet ice
          ENDIF                 ! End IF (QI1(I,J) .GT. 0.) THEN
!
!--- Assumed enhanced radar reflectivity when rain and ice coexist
!    above an assumed threshold mass content, RQmix
!
dbz_mix:  IF (RQR>RQmix .AND. RQLICE>RQmix) THEN
            IF (RQR>RQLICE) THEN
              NMIX=NRAIN
            ELSE
              NMIX=NLICE1(I,J)
            ENDIF
            DUM=RQR+RQLICE
            Zmix=Cboth*DUM*DUM/NMIX
            IF (Zmix > Zrain+Zice) THEN
              IF (RQR>RQLICE) THEN
                Zrain=Zmix-Zice
              ELSE
                Zice=Zmix-Zrain
              ENDIF
            ENDIF
          ENDIF  dbz_mix
!
!---  Calculate total (convective + grid-scale) radar reflectivity
!
10        Zice=Zice+Zsmice
          Ztot=Zrain+Zice+Zconv
          IF (Ztot .GT. Zmin)  DBZ1(I,J)= 10.*ALOG10(Ztot)
          IF (Zrain .GT. Zmin) DBZR1(I,J)=10.*ALOG10(Zrain)
          IF (Zice .GT. Zmin)  DBZI1(I,J)=10.*ALOG10(Zice)
!          IF (Zconv .GT. Zmin) DBZC1(I,J)=10.*ALOG10(Zsmice)
          IF (Zconv .GT. Zmin) DBZC1(I,J)=10.*ALOG10(Zconv)
        ENDDO
      ENDDO
!
      RETURN
      END
!
!-- For the old version of the microphysics:
!
      SUBROUTINE CALMICT_old(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL,     &
                        QW1,QI1,QR1,QS1,DBZ1,DBZR1,DBZI1,DBZC1,NLICE1,NRAIN1)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM: CALMICT_old COMPUTES HYDROMETEORS FROM THE OLDER VERSION
!                         OF THE MICROPHYSICS
!
!   PRGRMMR: JIN         ORG: W/NP2      DATE: 01-08-14
!
! ABSTRACT:
!     THIS ROUTINE COMPUTES THE MIXING RATIOS OF CLOUD WATER, CLOUD ICE,
!     RAIN, AND SNOW.  THE CODE IS BASED ON OPTION MP_PHYSICS==95 IN THE
!     WRF NAMELIST AND OPTION MICRO='fer' in NMMB CONFIGURE FILES.
!
! PROGRAM HISTORY LOG:
!   01-08-14  YI JIN
!   02-02-11  Brad Ferrier - Minor changes for consistency w/ NMM model
!   04-11-10  Brad Ferrier - Removed cloud fraction algorithm
!   04-11-17  H CHUANG - WRF VERSION
!   14-03-11  B Ferrier - Created new & old versions of this subroutine
!                         to process new & old versions of the microphysics
!
! USAGE:    CALL CALMICT_old(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL
!     &,                QW1,QI1,QR1,QS1,DBZ1,DBZR1,DBZI1,DBZC1)
!
!   INPUT ARGUMENT LIST:
!     P1D     - PRESSURE (PA)
!     T1D     - TEMPERATURE (K)
!     Q1D     - SPECIFIC HUMIDITY (KG/KG)
!     C1D     - TOTAL CONDENSATE (CWM, KG/KG)
!     FI1D    - F_ice (fraction of condensate in form of ice)
!     FR1D    - F_rain (fraction of liquid water in form of rain)
!     FS1D    - F_RimeF ("Rime Factor", ratio of total ice growth
!                       to deposition growth)
!     CUREFL  - Radar reflectivity contribution from convection (mm**6/m**3)
!
!   OUTPUT ARGUMENT LIST:
!     QW1   - CLOUD WATER MIXING RATIO (KG/KG)
!     QI1   - CLOUD ICE MIXING RATIO (KG/KG)
!     QR1   - RAIN MIXING RATIO (KG/KG)
!     QS1   - "SNOW" (precipitation ice) MIXING RATIO (KG/KG)
!     DBZ1  - Equivalent radar reflectivity factor in dBZ; i.e., 10*LOG10(Z)
!     DBZR  - Equivalent radar reflectivity factor from rain in dBZ
!     DBZI  - Equivalent radar reflectivity factor from ice (all forms) in dBZ
!     DBZC  - Equivalent radar reflectivity factor from parameterized convection
!     in dBZ
!
!   OUTPUT FILES:
!     NONE
!
!   SUBPROGRAMS CALLED:
!     FUNCTIONS:
!        FPVS
!     UTILITIES:
!     LIBRARY:
!       NONE
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM SP
!$$$
!
      use params_mod, only: dbzmin, epsq, tfrz, eps, rd, d608, oneps, nlimin
      use ctlblk_mod, only: jsta, jend, jsta_2l, jend_2u, im
      use rhgrd_mod, only: rhgrd
      use cmassi_mod, only: t_ice, rqr_drmin, n0rmin, cn0r_dmrmin, mdrmin, &
              rqr_drmax,cn0r_dmrmax, mdrmax, n0r0, xmrmin, xmrmax,flarge2, &
              massi, cn0r0, mdimin, xmimax, mdimax,NLImax 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      INTEGER INDEXS, INDEXR
      REAL, PARAMETER :: Cice=1.634e13

      real,dimension(IM,jsta_2l:jend_2u),intent(in)    :: P1D,T1D,Q1D,C1D,FI1D,FR1D, &
                                                          FS1D,CUREFL
      real,dimension(IM,jsta_2l:jend_2u),intent(inout) :: QW1,QI1,QR1,QS1,DBZ1,DBZR1,&
                                                          DBZI1,DBZC1,NLICE1,NRAIN1

      REAL N0r,Ztot,Zrain,Zice,Zconv,Zmin
      integer I,J
      real TC, Frain,Fice,Flimass,FLARGE,          &
           Fsmall,RimeF,Xsimass,Qice,Qsat,ESAT,WV,RHO,RRHO,RQR,          &
           DRmm,Qsigrd,WVQW,Dum,XLi,Qlice,WC,DLI,xlimass
      real,external :: fpvs
!************************************************************************
!--- Determine composition of condensate in the form of cloud water,
!    total ice (cloud ice & snow), & rain following GSMDRIVE in NMM model
!
      Zmin=10.**(0.1*DBZmin)
      DO J=JSTA,JEND
        DO I=1,IM
          QW1(I,J)=0.
          QI1(I,J)=0.
          QR1(I,J)=0.
          QS1(I,J)=0.
          NLICE1(I,J)=0.
          NRAIN1(I,J)=0.
          DBZ1(I,J)=DBZmin
          DBZR1(I,J)=DBZmin
          DBZI1(I,J)=DBZmin
          DBZC1(I,J)=DBZmin
        ENDDO
      ENDDO
      DO J=JSTA,JEND
        DO I=1,IM
          Zrain=0.            !--- Radar reflectivity from rain
          Zice=0.             !--- Radar reflectivity from ice
          Zconv=CUREFL(I,J)   !--- Radar reflectivity from convection
          IF (C1D(I,J) .LE. EPSQ) THEN
!
!--- Skip rest of calculatiions if no condensate is present
!
            GO TO 10
          ELSE
            WC=C1D(I,J)
          ENDIF
!
!--- Code below is from GSMDRIVE for determining:
!    QI1 - total ice (cloud ice & snow) mixing ratio
!    QW1 - cloud water mixing ratio
!    QR1 - rain mixing ratio
!
          TC=T1D(I,J)-TFRZ
          Fice=FI1D(I,J)
          Frain=FR1D(I,J)
          IF (TC.LE.T_ICE .OR. Fice.GE.1.) THEN
!          IF (Fice.GE.1.) THEN
            QI1(I,J)=WC
          ELSE IF (Fice .LE. 0.) THEN
            QW1(I,J)=WC
          ELSE
            QI1(I,J)=Fice*WC
            QW1(I,J)=WC-QI1(I,J)
          ENDIF
          IF (QW1(I,J).GT.0. .AND. Frain.GT.0.) THEN
            IF (Frain .GE. 1.) THEN
              QR1(I,J)=QW1(I,J)
              QW1(I,J)=0.
            ELSE
              QR1(I,J)=Frain*QW1(I,J)
              QW1(I,J)=QW1(I,J)-QR1(I,J)
            ENDIF
          ENDIF
          WV=Q1D(I,J)/(1.-Q1D(I,J))
!
!--- Saturation vapor pressure w/r/t water ( >=0C ) or ice ( <0C )
!
          ESAT=1000.*FPVS(T1D(I,J))
          QSAT=EPS*ESAT/(P1D(I,J)-ESAT)
          RHO=P1D(I,J)/(RD*T1D(I,J)*(1.+D608*Q1D(I,J)))
          RRHO=1./RHO
  !
  !--- Based on code from GSMCOLUMN in model to determine reflectivity from rain
  !
          IF (QR1(I,J) .GT. EPSQ) THEN
            RQR=RHO*QR1(I,J)
            IF (RQR .LE. RQR_DRmin) THEN
              N0r=MAX(N0rmin, CN0r_DMRmin*RQR)
              INDEXR=MDRmin
            ELSE IF (RQR .GE. RQR_DRmax) THEN
              N0r=CN0r_DMRmax*RQR
              INDEXR=MDRmax
            ELSE
              N0r=N0r0
              INDEXR=MAX( XMRmin, MIN(CN0r0*RQR**.25, XMRmax) )
            ENDIF
  !
  !--- INDEXR is the mean drop size in microns; convert to mm
  !
            DRmm=1.e-3*REAL(INDEXR)
            Zrain=0.72*N0r*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm
  !
  !--- Number concentration of rain drops (convert INDEXR to m)
  !
            NRAIN1(I,J)=N0r*1.E-6*REAL(INDEXR)
          ENDIF        !--- End IF (QR1(I,J) .GT. EPSQ) block
!
!--- Based on code from GSMCOLUMN in model to determine partition of
!    total ice into cloud ice & snow (precipitation ice)
!
          IF (QI1(I,J) .GT. EPSQ) THEN
            QICE=QI1(I,J)
            RHO=P1D(I,J)/(RD*T1D(I,J)*(1.+ONEPS*Q1D(I,J)))
            RRHO=1./RHO
            QSIgrd=RHgrd*QSAT
            WVQW=WV+QW1(I,J)
!
! * FLARGE  - ratio of number of large ice to total (large & small) ice
! * FSMALL  - ratio of number of small ice crystals to large ice particles
!  ->  Small ice particles are assumed to have a mean diameter of 50 microns.
!  * XSIMASS - used for calculating small ice mixing ratio
!  * XLIMASS - used for calculating large ice mixing ratio
!  * INDEXS  - mean size of snow to the nearest micron (units of microns)
!  * RimeF   - Rime Factor, which is the mass ratio of total (unrimed &
!              rimed) ice mass to the unrimed ice mass (>=1)
!  * FLIMASS - mass fraction of large ice
!  * QTICE   - time-averaged mixing ratio of total ice
!  * QLICE   - time-averaged mixing ratio of large ice
!  * NLICE1   - time-averaged number concentration of large ice
!
            IF (TC.GE.0. .OR. WVQW.LT.QSIgrd) THEN
              FLARGE=1.
            ELSE
              FLARGE=FLARGE2    !-- specified in MICROINIT.f
!!              IF (TC.GE.-8. .AND. TC.LE.-3.) FLARGE=.5*FLARGE
            ENDIF
            FSMALL=(1.-FLARGE)/FLARGE
            XSIMASS=RRHO*MASSI(MDImin)*FSMALL
            DUM=XMImax*EXP(.0536*TC)
            INDEXS=MIN(MDImax, MAX(MDImin, INT(DUM) ) )
            RimeF=AMAX1(1., FS1D(I,J) )
            XLIMASS=RRHO*RimeF*MASSI(INDEXS)
            FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
            QLICE=FLIMASS*QICE
            NLICE1(I,J)=QLICE/XLIMASS
            IF (NLICE1(I,J).LT.NLImin .OR. NLICE1(I,J).GT.NLImax) THEN
!
!--- Force NLICE1 to be between NLImin and NLImax
!
              DUM=MAX(NLImin, MIN(NLImax, NLICE1(I,J)) )
              XLI=RHO*(QICE/DUM-XSIMASS)/RimeF
              IF (XLI .LE. MASSI(MDImin) ) THEN
                INDEXS=MDImin
              ELSE IF (XLI .LE. MASSI(450) ) THEN
                DLI=9.5885E5*XLI**.42066         ! DLI in microns
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
              ELSE IF (XLI .LE. MASSI(MDImax) ) THEN
                DLI=3.9751E6*XLI**.49870         ! DLI in microns
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
              ELSE
                INDEXS=MDImax
!
!--- 8/22/01: Increase density of large ice if maximum limits
!    are reached for number concentration (NLImax) and mean size
!    (MDImax).  Done to increase fall out of ice.
!
                IF (DUM .GE. NLImax)                             &
                  RimeF=RHO*(QICE/NLImax-XSIMASS)/MASSI(INDEXS)
              ENDIF             ! End IF (XLI .LE. MASSI(MDImin) )
              XLIMASS=RRHO*RimeF*MASSI(INDEXS)
              FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
              QLICE=FLIMASS*QICE
              NLICE1(I,J)=QLICE/XLIMASS
            ENDIF               ! End IF (NLICE.LT.NLImin ...
            QS1(I,J)=AMIN1(QI1(I,J), QLICE)
            QI1(I,J)=AMAX1(0., QI1(I,J)-QS1(I,J))
   !
   !--- Equation (C.8) in Ferrier (1994, JAS, p. 272), which when
   !    converted from cgs units to mks units results in the same
   !    value for Cice, which is equal to the {} term below:
   !
   !    Zi={.224*720*(10**18)/[(PI*RHOL)**2]}*(RHO*QLICE)**2/NLICE1(I,J),
   !    where RHOL=1000 kg/m**3 is the density of liquid water
   !
   !--- Valid only for exponential ice distributions
   !
            Zice=Cice*RHO*RHO*QLICE*QLICE/NLICE1(I,J)
          ENDIF                 ! End IF (QI1(I,J) .GT. 0.) THEN
!
!---  Calculate total (convective + grid-scale) radar reflectivity
10        Ztot=Zrain+Zice+Zconv
          IF (Ztot .GT. Zmin)  DBZ1(I,J)= 10.*ALOG10(Ztot)
          IF (Zrain .GT. Zmin) DBZR1(I,J)=10.*ALOG10(Zrain)
          IF (Zice .GT. Zmin)  DBZI1(I,J)=10.*ALOG10(Zice)
          IF (Zconv .GT. Zmin) DBZC1(I,J)=10.*ALOG10(Zconv)
        ENDDO
      ENDDO
!
      RETURN
      END
