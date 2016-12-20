      module sfccyc_module
      implicit none
      SAVE
!
!  GRIB code for each parameter - Used in subroutines SFCCYCLE and SETRMSK.
!
      INTEGER kpdtsf,kpdwet,kpdsno,kpdzor,kpdais,kpdtg3,kpdplr,kpdgla,
     &        kpdmxi,kpdscv,kpdsmc,kpdoro,kpdmsk,kpdstc,kpdacn,kpdveg,
     &        kpdvet,kpdsot
!Clu [+1L] add kpd() for vmn, vmx, slp, abs
     &,       kpdvmn,kpdvmx,kpdslp,kpdabs
!cggg snow mods start  add snow depth
     &,       kpdsnd, kpdabs_0, kpdabs_1, kpdalb(4)
!cggg snow mods end
      PARAMETER(KPDTSF=11,  KPDWET=86, KPDSNO=65,  KPDZOR=83,
!    1          KPDALB=84,  KPDAIS=91, KPDTG3=11,  KPDPLR=224,
     1          KPDAIS=91,  KPDTG3=11, KPDPLR=224,
     2          KPDGLA=238, KPDMXI=91, KPDSCV=238, KPDSMC=144,
     3          KPDORO=8,   KPDMSK=81, KPDSTC=11,  KPDACN=91, KPDVEG=87,
!Clu [+1L] add kpd() for vmn, vmx, slp, abs
!cbosu  max snow albedo uses a grib id number of 159, not 255.
     &          kpdvmn=255, kpdvmx=255,kpdslp=236, kpdabs_0=255,    
     &          kpdvet=225, kpdsot=224,kpdabs_1=159,
!cggg snow mods start
     &          kpdsnd=66 )
!cggg snow mods end
!
      integer, parameter :: kpdalb_0(4)=(/212,215,213,216/)
      integer, parameter :: kpdalb_1(4)=(/189,190,191,192/)
      integer, parameter :: kpdalf(2)=(/214,217/)
!
      integer, parameter :: xdata=5000, ydata=2500, mdata=xdata*ydata
!
      end module sfccyc_module
      SUBROUTINE SFCCYCLE(LUGB,LEN,LSOIL,SIG1T,DELTSFC
     &,                   IY,IM,ID,IH,FH
     &,                   RLA, RLO, SLMASK,OROG,orog_uf,use_ufo
!Cwu [+1L] add SIHFCS and SICFCS
     &,                   SIHFCS,SICFCS,SITFCS                 
!Clu [+2L] add SWD, SLC, VMN, VMX, SLP, ABS
     &,                   SWDFCS,SLCFCS      
     &,                   VMNFCS,VMXFCS,SLPFCS,ABSFCS
     &,                   TSFFCS,SNOFCS,ZORFCS,ALBFCS,TG3FCS
     &,                   CNPFCS,SMCFCS,STCFCS,SLIFCS,AISFCS,F10M
     &,                   VEGFCS,VETFCS,SOTFCS,ALFFCS
     &,                   CVFCS,CVBFCS,CVTFCS,me,NLUNIT,IALB)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      USE sfccyc_module
      implicit none
      logical use_ufo
      real (kind=kind_io8) sllnd,slsea,aicice,aicsea,tgice,rlapse,
     &                     orolmx,orolmn,oroomx,oroomn,orosmx,
     &                     orosmn,oroimx,oroimn,orojmx,orojmn,
     &                     alblmx,alblmn,albomx,albomn,albsmx,
     &                     albsmn,albimx,albimn,albjmx,albjmn,
     &                     wetlmx,wetlmn,wetomx,wetomn,wetsmx,
     &                     wetsmn,wetimx,wetimn,wetjmx,wetjmn,
     &                     snolmx,snolmn,snoomx,snoomn,snosmx,
     &                     snosmn,snoimx,snoimn,snojmx,snojmn,
     &                     zorlmx,zorlmn,zoromx,zoromn,zorsmx,
     &                     zorsmn,zorimx,zorimn,zorjmx, zorjmn,
     &                     plrlmx,plrlmn,plromx,plromn,plrsmx,
     &                     plrsmn,plrimx,plrimn,plrjmx,plrjmn,
     &                     tsflmx,tsflmn,tsfomx,tsfomn,tsfsmx,
     &                     tsfsmn,tsfimx,tsfimn,tsfjmx,tsfjmn,
     &                     tg3lmx,tg3lmn,tg3omx,tg3omn,tg3smx,
     &                     tg3smn,tg3imx,tg3imn,tg3jmx,tg3jmn,
     &                     stclmx,stclmn,stcomx,stcomn,stcsmx,
     &                     stcsmn,stcimx,stcimn,stcjmx,stcjmn,
     &                     smclmx,smclmn,smcomx,smcomn,smcsmx,
     &                     smcsmn,smcimx,smcimn,smcjmx,smcjmn,
     &                     scvlmx,scvlmn,scvomx,scvomn,scvsmx,
     &                     scvsmn,scvimx,scvimn,scvjmx,scvjmn,
     &                     veglmx,veglmn,vegomx,vegomn,vegsmx,
     &                     vegsmn,vegimx,vegimn,vegjmx,vegjmn,
     &                     vetlmx,vetlmn,vetomx,vetomn,vetsmx,
     &                     vetsmn,vetimx,vetimn,vetjmx,vetjmn,
     &                     sotlmx,sotlmn,sotomx,sotomn,sotsmx,
     &                     sotsmn,sotimx,sotimn,sotjmx,sotjmn,
     &                     alslmx,alslmn,alsomx,alsomn,alssmx,
     &                     alssmn,alsimx,alsimn,alsjmx,alsjmn,
     &                     epstsf,epsalb,epssno,epswet,epszor,
     &                     epsplr,epsoro,epssmc,epsscv,eptsfc,
     &                     epstg3,epsais,epsacn,epsveg,epsvet,
     &                     epssot,epsalf,qctsfs,qcsnos,qctsfi,
     &                     aislim,snwmin,snwmax,cplrl,cplrs,
     &                     cvegl,czors,csnol,csnos,czorl,csots,
     &                     csotl,cvwgs,cvetl,cvets,calfs,
     &                     fcalfl,fcalfs,ccvt,ccnp,ccv,ccvb,
     &                     calbl,calfl,calbs,ctsfs,grboro,
     &                     grbmsk,ctsfl,deltf,caisl,caiss,
     &                     fsalfl,fsalfs,flalfs,falbl,ftsfl,
     &                     ftsfs,fzorl,fzors,fplrl,fsnos,faisl,
     &                     faiss,fsnol,bltmsk,falbs,cvegs,percrit,
     &                     deltsfc,critp2,critp3,blnmsk,critp1,
     &                     fcplrl,fcplrs,fczors,fvets,fsotl,fsots,
     &                     fvetl,fplrs,fvegl,fvegs,fcsnol,fcsnos,
     &                     fczorl,fcalbs,fctsfl,fctsfs,fcalbl,
     &                     falfs,falfl,fh,crit,zsca,ZTSFC,tem1,tem2
!Cwu [+2L] add f()l,f()s,c()l,c()s,eps() for sih, sic
     &,                    fsihl,fsihs,fsicl,fsics,
     &                     csihl,csihs,csicl,csics,epssih,epssic
!Clu [+4L] add f()l,f()s,c()l,c()s,eps() for vmn, vmx, slp, abs
     &,                    fvmnl,fvmns,fvmxl,fvmxs,fslpl,fslps,
     &                     fabsl,fabss,cvmnl,cvmns,cvmxl,cvmxs,
     &                     cslpl,cslps,cabsl,cabss,epsvmn,epsvmx,
     &                     epsslp,epsabs
!Cwu [+4L] add min/max for sih and sic
     &,                    sihlmx,sihlmn,sihomx,sihomn,sihsmx,
     &                     sihsmn,sihimx,sihimn,sihjmx,sihjmn,
     &                     siclmx,siclmn,sicomx,sicomn,sicsmx,
     &                     sicsmn,sicimx,sicimn,sicjmx,sicjmn
     &,                    glacir_hice
!Clu [+8L] add min/max for vmn, vmx, slp, abs
     &,                    vmnlmx,vmnlmn,vmnomx,vmnomn,vmnsmx,
     &                     vmnsmn,vmnimx,vmnimn,vmnjmx,vmnjmn,
     &                     vmxlmx,vmxlmn,vmxomx,vmxomn,vmxsmx,
     &                     vmxsmn,vmximx,vmximn,vmxjmx,vmxjmn,
     &                     slplmx,slplmn,slpomx,slpomn,slpsmx,
     &                     slpsmn,slpimx,slpimn,slpjmx,slpjmn,
     &                     abslmx,abslmn,absomx,absomn,abssmx,
     &                     abssmn,absimx,absimn,absjmx,absjmn
!Cwu [+1L] add sihnew
     &,                    sihnew

      INTEGER imsk,jmsk,ifp,irtscv,irtacn,irtais,irtsno,irtzor,
     &        irtalb,irtsot,irtalf,j,irtvet,irtsmc,irtstc,irtveg,
     &        irtwet,k,iprnt,kk,irttsf,iret,i,igrdbg,iy,im,id,
     &        icalbl,icalbs,icalfl,ictsfs,lugb,len,lsoil,ih,
     &        ictsfl,iczors,icplrl,icplrs,iczorl,icalfs,icsnol,
     &        icsnos,irttg3,me,KQCM, NLUNIT,IALB
!Clu [+1L] add irt() for vmn, vmx, slp, abs
     &,       irtvmn, irtvmx, irtslp, irtabs
      LOGICAL GAUSM, DEADS, QCMSK, ZNLST, MONCLM, MONANL,
!cggg landice mods start. 
!     &        MONFCS, MONMER, MONDIF
     &        MONFCS, MONMER, MONDIF, LANDICE
!cggg landice mods end

      integer NUM_PARTHDS
!
!  THIS IS A limited point VERSION of SURFACE PROGRAM.
!
!  This program runs in two different modes:
!
!  1.  Analysis mode (FH=0.)
!
!      This program merges climatology, analysis and forecast guess to create
!      new surface fields.  If analysis file is given, the program
!      uses it if date of the analysis matches with IY,IM,ID,IH (see Note
!      below).
!
!  2.  Forecast mode (FH.GT.0.)
!
!      This program interpolates climatology to the date corresponding to the
!      forecast hour.  If surface analysis file is given, for the corresponding
!      dates, the program will use it.
!
!   NOTE:
!
!      If the date of the analysis does not match given IY,IM,ID,IH, (and FH),
!      the program searches an old analysis by going back 6 hours, then 12 hours,
!      then one day upto NREPMX days (parameter statement in the SUBROTINE FIXRD.
!      Now defined as 8).  This allows the user to provide non-daily analysis to
!      be used.  If matching field is not found, the forecast guess will be used.
!
!      Use of a combined earlier surface analyses and current analysis is
!      NOT allowed (as was done in the old version for snow analysis in which
!      old snow analysis is used in combination with initial guess), except
!      for sea surface temperature.  For sst anolmaly interpolation, you need to
!      set LANOM=.TRUE. and must provide sst analysis at initial time.
!
!      If you want to do complex merging of past and present surface field analysis,
!      YOU NEED TO CREATE a separate file that contains DAILY SURFACE FIELD.
!
!      For a dead start, do not supply FNBGSI or set FNBGSI='        '
!
!  LUGB           is the unit number used in this subprogram
!  LEN ...        Number of points on which sfccyc operates
!  LSOIL .. 	  Number of soil layers (2 as of April, 1994)
!  IY,IM,ID,IH .. Year, month, day, and hour of initial state.
!  FH ..          Forecast hour
!  RLA, RLO --    Latitude and longitudes of the LEN points
!  SIG1T .. Sigma level 1 temperature for dead start.  Should be on Gaussian
!           grid.  If not dead start, no need for dimension but set to zero
!           as in the example below.
!
!  Variable naming conventions:
!
!     ORO .. Orography
!     ALB .. Albedo
!     WET .. Soil wetness as defined for bucket model
!     SNO .. Snow DEPTH
!     ZOR .. Surface roughness length
!     VET .. Vegetation type
!     PLR .. Plant evaporation resistance
!     TSF .. Surface skin temperature.  Sea surface temp. over ocean.
!     TG3 .. Deep soil temperature (at 500cm)
!     STC .. Soil temperature (LSOIL layrs)
!     SMC .. Soil moisture (LSOIL layrs)
!     SCV .. Snow cover (not snow depth)
!     AIS .. Sea ice mask (0 or 1)
!     ACN .. Sea ice concentration (fraction)
!     GLA .. Glacier (permanent snow) mask (0 or 1)
!     MXI .. Maximum sea ice extent (0 or 1)
!     MSK .. Land ocean mask (0=ocean 1=land)
!     CNP .. Canopy water content
!     CV  .. Convective cloud cover
!     CVB .. Convective cloud base
!     CVT .. Convective cloud top
!     SLI .. LAND/SEA/SEA-ICE mask. (1/0/2 respectively)
!     VEG .. Vegetation cover
!     SOT .. Soil type
!Cwu [+2L] add SIH & SIC
!     SIH .. Sea ice thickness
!     SIC .. Sea ice concentration
!Clu [+6L] add SWD,SLC,VMN,VMX,SLP,ABS
!     SWD .. Actual snow depth
!     SLC .. Liquid soil moisture (LSOIL layers)
!     VMN .. Vegetation cover minimum
!     VMX .. Vegetation cover maximum
!     SLP .. Slope type
!     ABS .. Maximum snow albedo

!
!  Definition of Land/Sea mask. SLLND for land and SLSEA for sea.
!  Definition of Sea/ice mask. AICICE for ice, AICSEA for sea.
!  TGICE=max ice temperature
!  RLAPSE=lapse rate for sst correction due to surface angulation
!
      PARAMETER(SLLND =1.0,SLSEA =0.0)
      PARAMETER(AICICE=1.0,AICSEA=0.0)
      PARAMETER(TGICE=271.2)
      PARAMETER(RLAPSE=0.65E-2)
!
!  Max/Min of fields for check and replace.
!
!     ???LMX .. Max over bare land
!     ???LMN .. Min over bare land
!     ???OMX .. Max over open ocean
!     ???OMN .. Min over open ocean
!     ???SMX .. Max over snow surface (land and sea-ice)
!     ???SMN .. Min over snow surface (land and sea-ice)
!     ???IMX .. Max over bare sea ice
!     ???IMN .. Min over bare sea ice
!     ???JMX .. Max over snow covered sea ice
!     ???JMN .. Min over snow covered sea ice
!
      PARAMETER(OROLMX=8000.,OROLMN=-1000.,OROOMX=3000.,OROOMN=-1000.,
     &          OROSMX=8000.,OROSMN=-1000.,OROIMX=3000.,OROIMN=-1000.,
     &          OROJMX=3000.,OROJMN=-1000.)
!     PARAMETER(ALBLMX=0.80,ALBLMN=0.06,ALBOMX=0.06,ALBOMN=0.06,
!    &          ALBSMX=0.80,ALBSMN=0.06,ALBIMX=0.80,ALBIMN=0.80,
!    &          ALBJMX=0.80,ALBJMN=0.80)
!Cwu [-3L/+9L] change min/max for ALB; add min/max for SIH & SIC
!     PARAMETER(ALBLMX=0.80,ALBLMN=0.01,ALBOMX=0.01,ALBOMN=0.01,
!    &          ALBSMX=0.80,ALBSMN=0.01,ALBIMX=0.01,ALBIMN=0.01,
!    &          ALBJMX=0.01,ALBJMN=0.01)
!  note: the range values for bare land and snow covered land
!        (ALBLMX, ALBLMN, ALBSMX, ALBSMN) are set below
!        based on whether the old or new radiation is selected
      PARAMETER(ALBOMX=0.06,ALBOMN=0.06,
     &          ALBIMX=0.80,ALBIMN=0.06,
     &          ALBJMX=0.80,ALBJMN=0.06)
      PARAMETER(SIHLMX=0.0,SIHLMN=0.0,SIHOMX=5.0,SIHOMN=0.0,
     &          SIHSMX=5.0,SIHSMN=0.0,SIHIMX=5.0,SIHIMN=0.10,
     &          SIHJMX=5.0,SIHJMN=0.10,glacir_hice=3.0)
      PARAMETER(SICLMX=0.0,SICLMN=0.0,SICOMX=1.0,SICOMN=0.0,
     &          SICSMX=1.0,SICSMN=0.0,SICIMX=1.0,SICIMN=0.50,
     &          SICJMX=1.0,SICJMN=0.50)
!
!     PARAMETER(SIHLMX=0.0,SIHLMN=0.0,SIHOMX=8.0,SIHOMN=0.0,
!    &          SIHSMX=8.0,SIHSMN=0.0,SIHIMX=8.0,SIHIMN=0.10,
!    &          SIHJMX=8.0,SIHJMN=0.10,glacir_hice=3.0)
!     PARAMETER(SICLMX=0.0,SICLMN=0.0,SICOMX=1.0,SICOMN=0.0,
!    &          SICSMX=1.0,SICSMN=0.0,SICIMX=1.0,SICIMN=0.15,
!    &          SICJMX=1.0,SICJMN=0.15)

      PARAMETER(WETLMX=0.15,WETLMN=0.00,WETOMX=0.15,WETOMN=0.15,
     &          WETSMX=0.15,WETSMN=0.15,WETIMX=0.15,WETIMN=0.15,
     &          WETJMX=0.15,WETJMN=0.15)
!Clu [-1L/+1L] revise SNOSMN (for Noah LSM)
      PARAMETER(SNOLMX=0.0,SNOLMN=0.0,SNOOMX=0.0,SNOOMN=0.0,
!*   &          SNOSMX=55000.,SNOSMN=0.01,SNOIMX=0.,SNOIMN=0.0,
!cggg landice mods start, should SNOSMN be set to .001 as in noah
!cggg     &          SNOSMX=55000.,SNOSMN=0.0001,SNOIMX=0.,SNOIMN=0.0,
     &          SNOSMX=55000.,SNOSMN=0.001,SNOIMX=0.,SNOIMN=0.0,
!cggg landice mods end
     &          SNOJMX=10000.,SNOJMN=0.01)
      PARAMETER(ZORLMX=300.,ZORLMN=1.0,ZOROMX=1.0,ZOROMN=1.E-05,
     &          ZORSMX=300.,ZORSMN=1.0,ZORIMX=1.0,ZORIMN=1.0,
     &          ZORJMX=1.0,ZORJMN=1.0)
      PARAMETER(PLRLMX=1000.,PLRLMN=0.0,PLROMX=1000.0,PLROMN=0.0,
     &          PLRSMX=1000.,PLRSMN=0.0,PLRIMX=1000.,PLRIMN=0.0,
     &          PLRJMX=1000.,PLRJMN=0.0)
!Clu [-1L/+1L] relax TSFSMX (for Noah LSM)
      PARAMETER(TSFLMX=353.,TSFLMN=173.0,TSFOMX=313.0,TSFOMN=271.2,
     &          TSFSMX=305.0,TSFSMN=173.0,TSFIMX=271.2,TSFIMN=173.0,
     &          TSFJMX=273.16,TSFJMN=173.0)
!     PARAMETER(TSFLMX=353.,TSFLMN=173.0,TSFOMX=313.0,TSFOMN=271.21,
!*   &          TSFSMX=273.16,TSFSMN=173.0,TSFIMX=271.21,TSFIMN=173.0,
!    &          TSFSMX=305.0,TSFSMN=173.0,TSFIMX=271.21,TSFIMN=173.0,
      PARAMETER(TG3LMX=310.,TG3LMN=200.0,TG3OMX=310.0,TG3OMN=200.0,
     &          TG3SMX=310.,TG3SMN=200.0,TG3IMX=310.0,TG3IMN=200.0,
     &          TG3JMX=310.,TG3JMN=200.0)
      PARAMETER(STCLMX=353.,STCLMN=173.0,STCOMX=313.0,STCOMN=200.0,
     &          STCSMX=310.,STCSMN=200.0,STCIMX=310.0,STCIMN=200.0,
     &          STCJMX=310.,STCJMN=200.0)
!cggg landice mods start.  force a flag value of soil moisture of 1.0
!                          at non-land points
!      PARAMETER(SMCLMX=0.55,SMCLMN=0.0,SMCOMX=0.55,SMCOMN=0.0,
!     &          SMCSMX=0.55,SMCSMN=0.0,SMCIMX=0.55,SMCIMN=0.0,
!     &          SMCJMX=0.55,SMCJMN=0.0)
      PARAMETER(SMCLMX=0.55,SMCLMN=0.0,SMCOMX=1.0,SMCOMN=1.0,
     &          SMCSMX=0.55,SMCSMN=0.0,SMCIMX=1.0,SMCIMN=1.0,
     &          SMCJMX=1.0,SMCJMN=1.0)
!cggg landice mods end.
      PARAMETER(SCVLMX=0.0,SCVLMN=0.0,SCVOMX=0.0,SCVOMN=0.0,
     &          SCVSMX=1.0,SCVSMN=1.0,SCVIMX=0.0,SCVIMN=0.0,
     &          SCVJMX=1.0,SCVJMN=1.0)
      PARAMETER(VEGLMX=1.0,VEGLMN=0.0,VEGOMX=0.0,VEGOMN=0.0,
     &          VEGSMX=1.0,VEGSMN=0.0,VEGIMX=0.0,VEGIMN=0.0,
     &          VEGJMX=0.0,VEGJMN=0.0)
!Clu [+12L] set min/max for VMN, VMX, SLP, ABS
      PARAMETER(VMNLMX=1.0,VMNLMN=0.0,VMNOMX=0.0,VMNOMN=0.0,
     &          VMNSMX=1.0,VMNSMN=0.0,VMNIMX=0.0,VMNIMN=0.0,
     &          VMNJMX=0.0,VMNJMN=0.0)   
      PARAMETER(VMXLMX=1.0,VMXLMN=0.0,VMXOMX=0.0,VMXOMN=0.0,
     &          VMXSMX=1.0,VMXSMN=0.0,VMXIMX=0.0,VMXIMN=0.0,
     &          VMXJMX=0.0,VMXJMN=0.0)  
      PARAMETER(SLPLMX=9.0,SLPLMN=1.0,SLPOMX=0.0,SLPOMN=0.0,
!cggg landice mods start
!cggg     &          SLPSMX=9.0,SLPSMN=1.0,SLPIMX=9.0,SLPIMN=9.0,
!cggg     &          SLPJMX=9.0,SLPJMN=9.0) 
     &          SLPSMX=9.0,SLPSMN=1.0,SLPIMX=0.,SLPIMN=0.,
     &          SLPJMX=0.,SLPJMN=0.) 
!cggg landice mods end
!  note: the range values for bare land and snow covered land
!        (ALBLMX, ALBLMN, ALBSMX, ALBSMN) are set below
!        based on whether the old or new radiation is selected
      PARAMETER(ABSOMX=0.0,ABSOMN=0.0,
     &          ABSIMX=0.0,ABSIMN=0.0,
     &          ABSJMX=0.0,ABSJMN=0.0)    
!  vegetation type
      PARAMETER(VETLMX=13.,VETLMN=1.0,VETOMX=0.0,VETOMN=0.0,
!cggg landice mods start
!cggg     &          VETSMX=13.,VETSMN=1.0,VETIMX=13.,VETIMN=13.0,
!cggg     &          VETJMX=13.,VETJMN=13.0)
     &          VETSMX=13.,VETSMN=1.0,VETIMX=0.,VETIMN=0.,
     &          VETJMX=0.,VETJMN=0.)
!cggg landice mods end
!  soil type
      PARAMETER(SOTLMX=9.,SOTLMN=1.0,SOTOMX=0.0,SOTOMN=0.0,
!cggg landice mods start
!cggg     &          SOTSMX=9.,SOTSMN=1.0,SOTIMX=9.,SOTIMN=9.0,
!cggg     &          SOTJMX=9.,SOTJMN=0.0)
     &          SOTSMX=9.,SOTSMN=1.0,SOTIMX=0.,SOTIMN=0.,
     &          SOTJMX=0.,SOTJMN=0.)
!cggg landice mods end
!  fraction of vegetation for strongly and weakly zeneith angle dependent
!  albedo
      PARAMETER(ALSLMX=1.0,ALSLMN=0.0,ALSOMX=0.0,ALSOMN=0.0,
     &          ALSSMX=1.0,ALSSMN=0.0,ALSIMX=0.0,ALSIMN=0.0,
     &          ALSJMX=0.0,ALSJMN=0.0)
!
!  Criteria used for monitoring
!
      PARAMETER(EPSTSF=0.01,EPSALB=0.001,EPSSNO=0.01,
     &          EPSWET=0.01,EPSZOR=0.0000001,EPSPLR=1.,EPSORO=0.,
     &          EPSSMC=0.0001,EPSSCV=0.,EPTSFC=0.01,EPSTG3=0.01,
     &          EPSAIS=0.,EPSACN=0.01,EPSVEG=0.01,
!Cwu [+1L] add eps() for sih, sic
     &          EPSSIH=0.001,EPSSIC=0.001,
!Clu [+1L] add eps() for vmn, vmx, abs, slp
     &          EPSVMN=0.01,EPSVMX=0.01,EPSABS=0.001,EPSSLP=0.01,
     &          epsvet=.01,epssot=.01,epsalf=.001)
!
!  Quality control of analysis snow and sea ice
!
!   QCTSFS .. Surface temperature above which no snow allowed
!   QCSNOS .. Snow depth above which snow must exist
!   QCTSFI .. SST above which sea-ice is not allowed
!
!Clu relax QCTSFS (for Noah LSM)
!*    PARAMETER(QCTSFS=283.16,QCSNOS=100.,QCTSFI=280.16)
!*    PARAMETER(QCTSFS=288.16,QCSNOS=100.,QCTSFI=280.16)
      PARAMETER(QCTSFS=293.16,QCSNOS=100.,QCTSFI=280.16)
!
!Cwu [-2L]
!* Ice concentration for ice limit (55 percent)
!
!*    PARAMETER(AISLIM=0.55)
!
!  Parameters to obtain snow depth from snow cover and temperature
!
!     PARAMETER(SNWMIN=25.,SNWMAX=100.)
      PARAMETER(SNWMIN=5.0,SNWMAX=100.)
      real (kind=kind_io8), parameter :: ten=10.0, one=1.0
!
!  COEEFICIENTS OF BLENDING FORECAST AND INTERPOLATED CLIM
!  (OR ANALYZED) FIELDS OVER SEA OR LAND(L) (NOT FOR CLOUDS)
!  1.0 = USE OF FORECAST
!  0.0 = REPLACE WITH INTERPOLATED ANALYSIS
!
!    These values are set for analysis mode.
!
!   Variables                  Land                 Sea
!   ---------------------------------------------------------
!   Surface temperature        Forecast             Analysis
!Cwu [+1L]
!   Surface temperature        Forecast             Forecast (over sea ice)
!   Albedo                     Analysis             Analysis
!   Sea-ice                    Analysis             Analysis
!   Snow                       Analysis             Forecast (over sea ice)
!   Roughness                  Analysis             Forecast
!   Plant resistance           Analysis             Analysis
!   Soil wetness (layer)       Weighted average     Analysis
!   Soil temperature           Forecast             Analysis
!   Canopy waver content       Forecast             Forecast
!   Convective cloud cover     Forecast             Forecast
!   Convective cloud bottm     Forecast             Forecast
!   Convective cloud top       Forecast             Forecast
!   Vegetation cover           Analysis             Analysis
!   vegetation type            Analysis             Analysis
!   soil type                  Analysis             Analysis
!Cwu [+2L]
!   Sea-ice thickness          Forecast             Forecast
!   Sea-ice concentration      Analysis             Analysis
!Clu [+6L]
!   Vegetation cover min       Analysis             Analysis
!   Vegetation cover max       Analysis             Analysis
!   Max snow albedo            Analysis             Analysis
!   Slope type                 Analysis             Analysis
!   Liquid Soil wetness        Analysis-weighted    Analysis
!   Actual snow depth          Analysis-weighted    Analysis
!
!  Note: If analysis file is not given, then time interpolated climatology
!        is used.  If analyiss file is given, it will be used as far as the
!        date and time matches.  If they do not match, it uses forecast.
!
!  Critical percentage value for aborting bad points when LGCHEK=.TRUE.
!
      LOGICAL LGCHEK
      DATA LGCHEK/.TRUE./
      DATA CRITP1,CRITP2,CRITP3/80.,80.,25./
!
!     integer kpdalb(4), kpdalf(2)
!     data kpdalb/212,215,213,216/, kpdalf/214,217/
!     save kpdalb, kpdalf
!
!  MASK OROGRAPHY AND VARIANCE ON GAUSSIAN GRID
!
      REAL (KIND=KIND_IO8) SLMASK(LEN),OROG(LEN), orog_uf(len)
     &,                    orogd(len)
      REAL (KIND=KIND_IO8) RLA(LEN), RLO(LEN)
!
!  Permanent/extremes
!
      CHARACTER*500 FNGLAC,FNMXIC
      real (kind=kind_io8), allocatable :: GLACIR(:),AMXICE(:),TSFCL0(:)
!
!     TSFCL0 is the climatological TSF at FH=0
!
!  CLIMATOLOGY SURFACE FIELDS (Last character 'C' or 'CLM' indicate CLIMATOLOGY)
!
      CHARACTER*500 FNTSFC,FNWETC,FNSNOC,FNZORC,FNALBC,FNAISC,
     &              FNPLRC,FNTG3C,FNSCVC,FNSMCC,FNSTCC,FNACNC,
     &              FNVEGC,fnvetc,fnsotc
!Clu [+1L] add FN()C for vmn, vmx, slp, abs
     &,             FNVMNC,FNVMXC,FNSLPC,FNABSC, FNALBC2 
      REAL (KIND=KIND_IO8) TSFCLM(LEN), WETCLM(LEN),   SNOCLM(LEN),
     &     ZORCLM(LEN), ALBCLM(LEN,4), AISCLM(LEN),
     &     TG3CLM(LEN), ACNCLM(LEN),   CNPCLM(LEN),
     &     CVCLM (LEN), CVBCLM(LEN),   CVTCLM(LEN),
     &     SCVCLM(LEN), TSFCL2(LEN),   VEGCLM(LEN),
     &     vetclm(LEN), sotclm(LEN),   ALFCLM(LEN,2), SLICLM(LEN),
     &     SMCCLM(LEN,LSOIL), STCCLM(LEN,LSOIL)
!Cwu [+1L] add ()CLM for sih, sic
     &,    SIHCLM(LEN), SICCLM(LEN)
!Clu [+1L] add ()CLM for vmn, vmx, slp, abs
     &,    VMNCLM(LEN), VMXCLM(LEN), SLPCLM(LEN), ABSCLM(LEN)
!
!  ANALYZED SURFACE FIELDS (Last character 'A' or 'ANL' indicate ANALYSIS)
!
      CHARACTER*500 FNTSFA,FNWETA,FNSNOA,FNZORA,FNALBA,FNAISA,
     &             FNPLRA,FNTG3A,FNSCVA,FNSMCA,FNSTCA,FNACNA,
     &             FNVEGA,fnveta,fnsota
!Clu [+1L] add FN()A for vmn, vmx, slp, abs
     &,            FNVMNA,FNVMXA,FNSLPA,FNABSA       
!
      REAL (KIND=KIND_IO8) TSFANL(LEN), WETANL(LEN),   SNOANL(LEN),
     &     ZORANL(LEN), ALBANL(LEN,4), AISANL(LEN),
     &     TG3ANL(LEN), ACNANL(LEN),   CNPANL(LEN),
     &     CVANL (LEN), CVBANL(LEN),   CVTANL(LEN),
     &     SCVANL(LEN), TSFAN2(LEN),   VEGANL(LEN),
     &     vetanl(LEN), sotanl(LEN),   ALFANL(LEN,2), SLIANL(LEN),
     &     SMCANL(LEN,LSOIL), STCANL(LEN,LSOIL)
!Cwu [+1L] add SIHANL & SICANL
     &,    SIHANL(LEN), SICANL(LEN)
!Clu [+1L] add ()ANL for vmn, vmx, slp, abs
     &,    VMNANL(LEN), VMXANL(LEN), SLPANL(LEN), ABSANL(LEN)
!
      REAL (KIND=KIND_IO8) TSFAN0(LEN) !  Sea surface temperature analysis at FT=0.
!
!  PREDICTED SURFACE FIELDS (Last characters 'FCS' indicates FORECAST)
!
      REAL (KIND=KIND_IO8) TSFFCS(LEN), WETFCS(LEN),   SNOFCS(LEN),
     &     ZORFCS(LEN), ALBFCS(LEN,4), AISFCS(LEN),
     &     TG3FCS(LEN), ACNFCS(LEN),   CNPFCS(LEN),
     &     CVFCS (LEN), CVBFCS(LEN),   CVTFCS(LEN),
     &     SLIFCS(LEN), VEGFCS(LEN),
     &     vetfcs(LEN), sotfcs(LEN),   alffcs(LEN,2),
     &     SMCFCS(LEN,LSOIL), STCFCS(LEN,LSOIL)
!Cwu [+1L] add SIHFCS & SICFCS
     &,    SIHFCS(LEN), SICFCS(LEN), SITFCS(LEN)
!Clu [+2L] add ()FCS for VMN, VMX, SLP, ABS, SWD, SLC
     &,    VMNFCS(LEN), VMXFCS(LEN), SLPFCS(LEN), ABSFCS(LEN)
     &,    SWDFCS(LEN), SLCFCS(LEN,LSOIL)
!
! Ratio of sigma level 1 wind and 10m wind (diagnozed by model and not touched
! in this program).
!
      REAL (KIND=KIND_IO8) F10M  (LEN)
      REAL (KIND=KIND_IO8) FSMCL(25),FSMCS(25),FSTCL(25),FSTCS(25)
      REAL (KIND=KIND_IO8) FCSMCL(25),FCSMCS(25),FCSTCL(25),FCSTCS(25)

!Clu [+1L] add SWRATIO (soil moisture liquid-to-total ratio)
      REAL (KIND=KIND_IO8) SWRATIO(LEN,LSOIL)
!Clu [+1L] add FIXRATIO (option to adjust slc from smc)
      LOGICAL FIXRATIO(LSOIL)
!
      INTEGER ICSMCL(25), ICSMCS(25), ICSTCL(25), ICSTCS(25)
!
      REAL (KIND=KIND_IO8) CSMCL(25), CSMCS(25)
      REAL (KIND=KIND_IO8) CSTCL(25), CSTCS(25)
!
      REAL (KIND=KIND_IO8) SLMSKH(mdata)
      CHARACTER*500 FNMSKH
      Integer kpd7, kpd9
!
      logical icefl1(len), icefl2(len)
!
!  Input and output SURFACE FIELDS (BGES) file names
!
!
!  Sigma level 1 temperature for dead start
!
      REAL (KIND=KIND_IO8) SIG1T(LEN)
!
      CHARACTER*32 LABEL
!
!  = 1 ==> FORECAST IS USED
!  = 0 ==> ANALYSIS (OR CLIMATOLOGY) IS USED
!
!     OUTPUT FILE  ... PRIMARY SURFACE FILE FOR RADIATION AND FORECAST
!
!       REC.  1    LABEL
!       REC.  2    DATE RECORD
!       REC.  3    TSF
!       REC.  4    SOILM(TWO LAYERS)              ----> 4 layers
!       REC.  5    SNOW
!       REC.  6    SOILT(TWO LAYERS)              ----> 4 layers
!       REC.  7    TG3
!       REC.  8    ZOR
!       REC.  9    CV
!       REC. 10    CVB
!       REC. 11    CVT
!       REC. 12    ALBEDO (four types)
!       REC. 13    SLIMSK
!       REC. 14    vegetation cover
!       REC. 14    PLANTR                         -----> skip this record
!       REC. 15    F10M                           -----> CANOPY
!       REC. 16    CANOPY WATER CONTENT (CNPANL)  -----> F10M
!       REC. 17    vegetation type
!       REC. 18    soil type
!       REC. 19    zeneith angle dependent vegetation fraction (two types)
!       REC. 20    UUSTAR
!       REC. 21    FFMM
!       REC. 22    FFHH
!Cwu add SIH & SIC
!       REC. 23    SIH(one category only)
!       REC. 24    SIC
!Clu [+8L] add PRCP, FLAG, SWD, SLC, VMN, VMX, SLP, ABS
!       REC. 25    TPRCP
!       REC. 26    SRFLAG
!       REC. 27    SWD
!       REC. 28    SLC (4 LAYERS)
!       REC. 29    VMN
!       REC. 30    VMX
!       REC. 31    SLP
!       REC. 32    ABS

!
!  Debug only
!   LDEBUG=.TRUE. creates BGES files for climatology and analysis
!   LQCBGS=.TRUE. Quality controls input BGES file before merging (should have been
!              QCed in the forecast program)
!
      LOGICAL LDEBUG,LQCBGS
      logical lprnt
!
!  Debug only
!
      CHARACTER*500 FNDCLM,FNDANL
!
      LOGICAL LANOM

!
      NAMELIST/NAMSFC/FNGLAC,FNMXIC,
     &                FNTSFC,FNWETC,FNSNOC,FNZORC,FNALBC,FNAISC,
     &                FNPLRC,FNTG3C,FNSCVC,FNSMCC,FNSTCC,FNACNC,
     &                FNVEGC,fnvetc,fnsotc,FNALBC2,
!Clu [+1L]  add fn()c for vmn, vmx, slp, abs
     &                FNVMNC,FNVMXC,FNSLPC,FNABSC,
     &                FNTSFA,FNWETA,FNSNOA,FNZORA,FNALBA,FNAISA,
     &                FNPLRA,FNTG3A,FNSCVA,FNSMCA,FNSTCA,FNACNA,
     &                FNVEGA,fnveta,fnsota,
!Clu [+1L]  add fn()a for vmn, vmx, slp, abs
     &                FNVMNA,FNVMXA,FNSLPA,FNABSA,
     &                FNMSKH,
     &                LDEBUG,LGCHEK,LQCBGS,CRITP1,CRITP2,CRITP3,
     &                FNDCLM,FNDANL,
     &                LANOM,
     &                FTSFL,FTSFS,FALBL,FALBS,FAISL,FAISS,FSNOL,FSNOS,
     &                FZORL,FZORS,FPLRL,FPLRS,FSMCL,FSMCS,
     &                FSTCL,FSTCS,fvegl,fvegs,fvetl,fvets,fsotl,fsots,
     &                FCTSFL,FCTSFS,FCALBL,FCALBS,FCSNOL,FCSNOS,
     &                FCZORL,FCZORS,FCPLRL,FCPLRS,FCSMCL,FCSMCS,
     &                FCSTCL,FCSTCS,fsalfl,fsalfs,fcalfl,flalfs,
!Cwu [+1L]  add f()l and f()s for sih, sic and aislim, sihnew
     &                FSIHL,FSICL,FSIHS,FSICS,AISLIM,SIHNEW,
!Clu [+2L]  add f()l and f()s for vmn, vmx, slp, abs
     &                FVMNL,FVMNS,FVMXL,FVMXS,FSLPL,FSLPS,
     &                FABSL,FABSS,
     &                ICTSFL,ICTSFS,ICALBL,ICALBS,ICSNOL,ICSNOS,
     &                ICZORL,ICZORS,ICPLRL,ICPLRS,ICSMCL,ICSMCS,
     &                ICSTCL,ICSTCS,icalfl,icalfs,
!
     &                GAUSM,  DEADS, QCMSK, ZNLST,
     &                MONCLM, MONANL, MONFCS, MONMER, MONDIF, IGRDBG,
!cggg landice mods start
!     &                BLNMSK, BLTMSK
     &                BLNMSK, BLTMSK, LANDICE
!cggg landice mods end
!
      DATA GAUSM/.TRUE./,  DEADS/.FALSE./, BLNMSK/0.0/, BLTMSK/90.0/
     &,    QCMSK/.FALSE./, ZNLST/.FALSE./, IGRDBG/-1/
     &,    MONCLM/.FALSE./, MONANL/.FALSE./, MONFCS/.FALSE./
!cggg landice mods start
!     &,    MONMER/.FALSE./,  MONDIF/.FALSE./
     &,    MONMER/.FALSE./,  MONDIF/.FALSE./,  LANDICE/.TRUE./
!cggg landice mods end
!
!  Defaults file names
!
      DATA FNMSKH/'global_slmask.t126.grb'/
      DATA FNALBC/'global_albedo4.1x1.grb'/
      DATA FNALBC2/'global_albedo4.1x1.grb'/
      DATA FNTSFC/'global_sstclim.2x2.grb'/
      DATA FNSOTC/'global_soiltype.1x1.grb'/
      DATA FNVEGC/'global_vegfrac.1x1.grb'/
      DATA FNVETC/'global_vegtype.1x1.grb'/
      DATA FNGLAC/'global_glacier.2x2.grb'/
      DATA FNMXIC/'global_maxice.2x2.grb'/
      DATA FNSNOC/'global_snoclim.1.875.grb'/
      DATA FNZORC/'global_zorclim.1x1.grb'/
      DATA FNAISC/'global_iceclim.2x2.grb'/
      DATA FNTG3C/'global_tg3clim.2.6x1.5.grb'/
      DATA FNSMCC/'global_soilmcpc.1x1.grb'/
!Clu [+4L] add fn()c for vmn, vmx, abs, slp
      DATA FNVMNC/'global_shdmin.0.144x0.144.grb'/
      DATA FNVMXC/'global_shdmax.0.144x0.144.grb'/
      DATA FNSLPC/'global_slope.1x1.grb'/
      DATA FNABSC/'global_snoalb.1x1.grb'/
!
      DATA FNWETC/'        '/
      DATA FNPLRC/'        '/
      DATA FNSTCC/'        '/
      DATA FNSCVC/'        '/
      DATA FNACNC/'        '/
!
      DATA FNTSFA/'        '/
      DATA FNWETA/'        '/
      DATA FNSNOA/'        '/
      DATA FNZORA/'        '/
      DATA FNALBA/'        '/
      DATA FNAISA/'        '/
      DATA FNPLRA/'        '/
      DATA FNTG3A/'        '/
      DATA FNSMCA/'        '/
      DATA FNSTCA/'        '/
      DATA FNSCVA/'        '/
      DATA FNACNA/'        '/
      DATA FNVEGA/'        '/
      DATA FNVETA/'        '/
      DATA FNSOTA/'        '/
!Clu [+4L] add fn()a for vmn, vmx, abs, slp
      DATA FNVMNA/'        '/
      DATA FNVMXA/'        '/
      DATA FNSLPA/'        '/
      DATA FNABSA/'        '/
!
      DATA LDEBUG/.FALSE./, LQCBGS/.TRUE./
      DATA FNDCLM/'        '/
      DATA FNDANL/'        '/
      DATA LANOM/.FALSE./
!
!  DEFAULT RELAXATION TIME IN HOURS TO ANALYSIS OR CLIMATOLOGY
      DATA FTSFL/99999.0/,  FTSFS/0.0/
      DATA FALBL/0.0/,      FALBS/0.0/
      DATA FALFL/0.0/,      FALFS/0.0/
      DATA FAISL/0.0/,      FAISS/0.0/
      DATA FSNOL/0.0/,      FSNOS/99999.0/
      DATA FZORL/0.0/,      FZORS/99999.0/
      DATA FPLRL/0.0/,      FPLRS/0.0/
      DATA FvetL/0.0/,      FvetS/99999.0/
      DATA FsotL/0.0/,      FsotS/99999.0/
      DATA FVegL/0.0/,      FvegS/99999.0/
!Cwu [+4L] add f()l and f()s for sih, sic and aislim, sihlim
      DATA FsihL/99999.0/,  FsihS/99999.0/
!     DATA FsicL/99999.0/,  FsicS/99999.0/
      DATA FsicL/0.0/,      FsicS/0.0/
!  DEFAULT ice concentration limit (50%), new ice thickness (20cm)
      DATA AISLIM/0.50/,    SIHNEW/0.2/
!Clu [+4L] add f()l and f()s for vmn, vmx, abs, slp
      DATA FvmnL/0.0/,      FvmnS/99999.0/
      DATA FvmxL/0.0/,      FvmxS/99999.0/
      DATA FslpL/0.0/,      FslpS/99999.0/
      DATA FabsL/0.0/,      FabsS/99999.0/
!  DEFAULT RELAXATION TIME IN HOURS TO CLIMATOLOGY IF ANALYSIS MISSING
      DATA FCTSFL/99999.0/, FCTSFS/99999.0/
      DATA FCALBL/99999.0/, FCALBS/99999.0/
      DATA FCSNOL/99999.0/, FCSNOS/99999.0/
      DATA FCZORL/99999.0/, FCZORS/99999.0/
      DATA FCPLRL/99999.0/, FCPLRS/99999.0/
!  DEFAULT FLAG TO APPLY CLIMATOLOGICAL ANNUAL CYCLE
      DATA ICTSFL/0/, ICTSFS/1/
      DATA ICALBL/1/, ICALBS/1/
      DATA ICALFL/1/, ICALFS/1/
      DATA ICSNOL/0/, ICSNOS/0/
      DATA ICZORL/1/, ICZORS/0/
      DATA ICPLRL/1/, ICPLRS/0/
!
      DATA CCNP/1.0/
      DATA CCV/1.0/,   CCVB/1.0/, CCVT/1.0/
!
      DATA IFP/0/
!
      SAVE IFP,FNGLAC,FNMXIC,
     &     FNTSFC,FNWETC,FNSNOC,FNZORC,FNALBC,FNAISC,
     &     FNPLRC,FNTG3C,FNSCVC,FNSMCC,FNSTCC,FNACNC,FNVEGC,
     &     FNTSFA,FNWETA,FNSNOA,FNZORA,FNALBA,FNAISA,
     &     FNPLRA,FNTG3A,FNSCVA,FNSMCA,FNSTCA,FNACNA,FNVEGA,
     &     fnvetc,fnveta,
     &     fnsotc,fnsota,
!Clu [+2L] add fn()c and fn()a for vmn, vmx, slp, abs
     &     FNVMNC,FNVMXC,FNABSC,FNSLPC,
     &     FNVMNA,FNVMXA,FNABSA,FNSLPA,
     &     LDEBUG,LGCHEK,LQCBGS,CRITP1,CRITP2,CRITP3,
     &     FNDCLM,FNDANL,
     &     LANOM,
     &     FTSFL,FTSFS,FALBL,FALBS,FAISL,FAISS,FSNOL,FSNOS,
     &     FZORL,FZORS,FPLRL,FPLRS,FSMCL,FSMCS,falfl,falfs,
     &     FSTCL,FSTCS,fvegl,fvegs,fvetl,fvets,fsotl,fsots,
     &     FCTSFL,FCTSFS,FCALBL,FCALBS,FCSNOL,FCSNOS,
     &     FCZORL,FCZORS,FCPLRL,FCPLRS,FCSMCL,FCSMCS,
     &     FCSTCL,FCSTCS,fcalfl,fcalfs,
!Cwu [+1L] add f()l and f()s for sih, sic and aislim, sihnew
     &     FSIHL,FSIHS,FSICL,FSICS,AISLIM,SIHNEW,
!Clu [+2L] add f()l and f()s for vmn, vmx, slp, abs
     &     FVMNL,FVMNS,FVMXL,FVMXS,FSLPL,FSLPS,
     &     FABSL,FABSS,
     &     ICTSFL,ICTSFS,ICALBL,ICALBS,ICSNOL,ICSNOS,
     &     ICZORL,ICZORS,ICPLRL,ICPLRS,ICSMCL,ICSMCS,
     &     ICSTCL,ICSTCS,icalfl,icalfs,
     &     GAUSM, DEADS, QCMSK,
     &     MONCLM, MONANL, MONFCS, MONMER, MONDIF, IGRDBG,
     &     GRBORO, GRBMSK,
!
     &     CTSFL,  CTSFS,  CALBL, CALFL, CALBS, CALFS, CSMCS,
     &     CSNOL,  CSNOS,  CZORL, CZORS, CPLRL, CPLRS, CSTCL,
     &     CSTCS,  CvegL,  CvwgS, CvetL, CvetS, CsotL, CsotS,
     &     CSMCL
!Cwu [+1L] add c()l and c()s for sih, sic
     &,    CSIHL,  CSIHS,  CSICL, CSICS
!Clu [+2L] add c()l and c()s for vmn, vmx, slp, abs
     &,    CVMNL,  CVMNS,  CVMXL, CVMXS, CSLPL, CSLPS,
     &     CABSL,  CABSS
     &,    IMSK, JMSK, SLMSKH, BLNMSK, BLTMSK
     &,    GLACIR, AMXICE, TSFCL0
     &,    caisl, caiss, cvegs
!
      lprnt = .false.
      iprnt = 1
!     do i=1,len
!       if (ifp .eq. 0 .and. rla(i) .gt. 80.0) print *,' rla=',rla(i)
!    *,' rlo=',rlo(i)
!       tem1 = abs(rla(i) - 48.75)
!       tem2 = abs(rlo(i) - (-68.50))
!       if(tem1 .lt. 0.25 .and. tem2 .lt. 0.50) then
!         lprnt = .true.
!         iprnt = i
!         print *,' lprnt=',lprnt,' iprnt=',iprnt
!         print *,' rla(i)=',rla(i),' rlo(i)=',rlo(i)
!       endif
!     enddo
      if (ialb == 1) then
        kpdabs = kpdabs_1
        kpdalb = kpdalb_1
        alblmx = .99
        albsmx = .99
        alblmn = .01
        albsmn = .01
        abslmx = 1.0
        abssmx = 1.0
        abssmn = .01
        abslmn = .01
      else
        kpdabs = kpdabs_0
        kpdalb = kpdalb_0
        alblmx = .80
        albsmx = .80
        alblmn = .06
        albsmn = .06
        abslmx = .80
        abssmx = .80
        abslmn = .01
        abssmn = .01
      endif
      IF(IFP.EQ.0) THEN
        IFP = 1
        DO K=1,LSOIL
          FSMCL(K) = 99999.
          FSMCS(K) = 0.
          FSTCL(K) = 99999.
          FSTCS(K) = 0.
        ENDDO
!     print *,' IN SFCSUB NLUNIT=',NLUNIT,' me=',me,' ialb=',ialb
        rewind(NLUNIT)
        READ (NLUNIT,NAMSFC)
!       WRITE(6,NAMSFC)
!
        if (me .eq. 0) then
          print *,'FTSFL,FALBL,FAISL,FSNOL,FZORL=',
     &    FTSFL,FALBL,FAISL,FSNOL,FZORL
          print *,'FSMCL=',FSMCL(1:LSOIL)
          print *,'FSTCL=',FSTCL(1:LSOIL)
          print *,'FTSFS,FALBS,FAISS,FSNOS,FZORS=',
     &    FTSFS,FALBS,FAISS,FSNOS,FZORS
          print *,'FSMCS=',FSMCS(1:LSOIL)
          print *,'FSTCS=',FSTCS(1:LSOIL)
          print *,' AISLIM=',aislim,' SIHNEW=',SIHNEW
        endif
!
        DELTF = DELTSFC / 24.0
!
        CTSFL=0.                       !...  tsfc over land
        IF(FTSFL.GE.99999.) CTSFL=1.
        IF((FTSFL.GT.0.).AND.(FTSFL.LT.99999))  CTSFL=EXP(-DELTF/FTSFL)
!
        CTSFS=0.                       !...  tsfc over sea
        IF(FTSFS.GE.99999.) CTSFS=1.
        IF((FTSFS.GT.0.).AND.(FTSFS.LT.99999))  CTSFS=EXP(-DELTF/FTSFS)
!
        DO K=1,LSOIL
          CSMCL(K)=0.                  !...  soilm over land
          IF(FSMCL(K).GE.99999.) CSMCL(K)=1.
          IF((FSMCL(K).GT.0.).AND.(FSMCL(K).LT.99999))
     &                           CSMCL(K)=EXP(-DELTF/FSMCL(K))
          CSMCS(K)=0.                  !...  soilm over sea
          IF(FSMCS(K).GE.99999.) CSMCS(K)=1.
          IF((FSMCS(K).GT.0.).AND.(FSMCS(K).LT.99999))
     &                           CSMCS(K)=EXP(-DELTF/FSMCS(K))
        ENDDO
!
        CALBL=0.                       !...  albedo over land
        IF(FALBL.GE.99999.) CALBL=1.
        IF((FALBL.GT.0.).AND.(FALBL.LT.99999))  CALBL=EXP(-DELTF/FALBL)
!
        CALFL=0.                       !...  fraction field for albedo over land
        IF(FALFL.GE.99999.) CALFL=1.
        IF((FALFL.GT.0.).AND.(FALFL.LT.99999))  CALFL=EXP(-DELTF/FALFL)
!
        CALBS=0.                       !...  albedo over sea
        IF(FALBS.GE.99999.) CALBS=1.
        IF((FALBS.GT.0.).AND.(FALBS.LT.99999))  CALBS=EXP(-DELTF/FALBS)
!
        CALFS=0.                       !...  fraction field for albedo over sea
        IF(FALFS.GE.99999.) CALFS=1.
        IF((FALFS.GT.0.).AND.(FALFS.LT.99999))  CALFS=EXP(-DELTF/FALFS)
!
        CAISL=0.                       !...  sea ice over land
        IF(FAISL.GE.99999.) CAISL=1.
        IF((FAISL.GT.0.).AND.(FAISL.LT.99999))  CAISL=1.
!
        CAISS=0.                       !...  sea ice over sea
        IF(FAISS.GE.99999.) CAISS=1.
        IF((FAISS.GT.0.).AND.(FAISS.LT.99999))  CAISS=1.
!
        CSNOL=0.                       !...  snow over land
        IF(FSNOL.GE.99999.) CSNOL=1.
        IF((FSNOL.GT.0.).AND.(FSNOL.LT.99999))  CSNOL=EXP(-DELTF/FSNOL)
!       Using the same way to bending snow as NARR when FSNOL is the negative value
!       The magnitude of FSNOL is the thread to determine the lower and upper bound
!       of final SWE
        IF(FSNOL.LT.0.)CSNOL=FSNOL
!
        CSNOS=0.                       !...  snow over sea
        IF(FSNOS.GE.99999.) CSNOS=1.
        IF((FSNOS.GT.0.).AND.(FSNOS.LT.99999))  CSNOS=EXP(-DELTF/FSNOS)
!
        CZORL=0.                       !...  roughness length over land
        IF(FZORL.GE.99999.) CZORL=1.
        IF((FZORL.GT.0.).AND.(FZORL.LT.99999))  CZORL=EXP(-DELTF/FZORL)
!
        CZORS=0.                       !...  roughness length over sea
        IF(FZORS.GE.99999.) CZORS=1.
        IF((FZORS.GT.0.).AND.(FZORS.LT.99999))  CZORS=EXP(-DELTF/FZORS)
!
!       CPLRL=0.                       !...  plant resistance over land
!       IF(FPLRL.GE.99999.) CPLRL=1.
!       IF((FPLRL.GT.0.).AND.(FPLRL.LT.99999))  CPLRL=EXP(-DELTF/FPLRL)
!
!       CPLRS=0.                       !...  plant resistance over sea
!       IF(FPLRS.GE.99999.) CPLRS=1.
!       IF((FPLRS.GT.0.).AND.(FPLRS.LT.99999))  CPLRS=EXP(-DELTF/FPLRS)
!
        DO K=1,LSOIL
           CSTCL(K)=0.                 !...  soilt over land
           IF(FSTCL(K).GE.99999.) CSTCL(K)=1.
           IF((FSTCL(K).GT.0.).AND.(FSTCL(K).LT.99999))
     &                            CSTCL(K)=EXP(-DELTF/FSTCL(K))
          CSTCS(K)=0.                  !...  soilt over sea
          IF(FSTCS(K).GE.99999.) CSTCS(K)=1.
          IF((FSTCS(K).GT.0.).AND.(FSTCS(K).LT.99999))
     &                           CSTCS(K)=EXP(-DELTF/FSTCS(K))
        ENDDO
!
        CvegL=0.                       !...  Vegetation fraction over land
        IF(FvegL.GE.99999.) CvegL=1.
        IF((FvegL.GT.0.).AND.(FvegL.LT.99999))  CvegL=EXP(-DELTF/FvegL)
!
        CvegS=0.                       !...  Vegetation fraction over sea
        IF(FvegS.GE.99999.) CvegS=1.
        IF((FvegS.GT.0.).AND.(FvegS.LT.99999))  CvegS=EXP(-DELTF/FvegS)
!
        CvetL=0.                       !...  Vegetation type over land
        IF(FvetL.GE.99999.) CvetL=1.
        IF((FvetL.GT.0.).AND.(FvetL.LT.99999))  CvetL=EXP(-DELTF/FvetL)
!
        CvetS=0.                       !...  Vegetation type over sea
        IF(FvetS.GE.99999.) CvetS=1.
        IF((FvetS.GT.0.).AND.(FvetS.LT.99999))  CvetS=EXP(-DELTF/FvetS)
!
        CsotL=0.                       !...  Soil type over land
        IF(FsotL.GE.99999.) CsotL=1.
        IF((FsotL.GT.0.).AND.(FsotL.LT.99999))  CsotL=EXP(-DELTF/FsotL)
!
        CsotS=0.                       !...  Soil type over sea
        IF(FsotS.GE.99999.) CsotS=1.
        IF((FsotS.GT.0.).AND.(FsotS.LT.99999))  CsotS=EXP(-DELTF/FsotS)

!Cwu [+16L]---------------------------------------------------------------
!
        CsihL=0.                       !...  Sea ice thickness over land
        IF(FsihL.GE.99999.) CsihL=1.
        IF((FsihL.GT.0.).AND.(FsihL.LT.99999))  CsihL=EXP(-DELTF/FsihL)
!
        CsihS=0.                       !...  Sea ice thickness over sea
        IF(FsihS.GE.99999.) CsihS=1.
        IF((FsihS.GT.0.).AND.(FsihS.LT.99999))  CsihS=EXP(-DELTF/FsihS)
!
        CsicL=0.                       !...  Sea ice concentration over land
        IF(FsicL.GE.99999.) CsicL=1.
        IF((FsicL.GT.0.).AND.(FsicL.LT.99999))  CsicL=EXP(-DELTF/FsicL)
!
        CsicS=0.                       !...  Sea ice concentration over sea
        IF(FsicS.GE.99999.) CsicS=1.
        IF((FsicS.GT.0.).AND.(FsicS.LT.99999))  CsicS=EXP(-DELTF/FsicS)

!Clu [+32L]---------------------------------------------------------------
!
        CvmnL=0.                       !...  Min Veg cover over land
        IF(FvmnL.GE.99999.) CvmnL=1.
        IF((FvmnL.GT.0.).AND.(FvmnL.LT.99999))  CvmnL=EXP(-DELTF/FvmnL)
!
        CvmnS=0.                       !...  Min Veg cover over sea
        IF(FvmnS.GE.99999.) CvmnS=1.
        IF((FvmnS.GT.0.).AND.(FvmnS.LT.99999))  CvmnS=EXP(-DELTF/FvmnS)
!
        CvmxL=0.                       !...  Max Veg cover over land
        IF(FvmxL.GE.99999.) CvmxL=1.
        IF((FvmxL.GT.0.).AND.(FvmxL.LT.99999))  CvmxL=EXP(-DELTF/FvmxL)
!
        CvmxS=0.                       !...  Max Veg cover over sea
        IF(FvmxS.GE.99999.) CvmxS=1.
        IF((FvmxS.GT.0.).AND.(FvmxS.LT.99999))  CvmxS=EXP(-DELTF/FvmxS)
!
        CslpL=0.                       !... Slope type over land
        IF(FslpL.GE.99999.) CslpL=1.
        IF((FslpL.GT.0.).AND.(FslpL.LT.99999))  CslpL=EXP(-DELTF/FslpL)
!
        CslpS=0.                       !...  Slope type over sea
        IF(FslpS.GE.99999.) CslpS=1.
        IF((FslpS.GT.0.).AND.(FslpS.LT.99999))  CslpS=EXP(-DELTF/FslpS)
!
        CabsL=0.                       !... Snow albedo over land
        IF(FabsL.GE.99999.) CabsL=1.
        IF((FabsL.GT.0.).AND.(FabsL.LT.99999))  CabsL=EXP(-DELTF/FabsL)
!
        CabsS=0.                       !... Snow albedo over sea
        IF(FabsS.GE.99999.) CabsS=1.
        IF((FabsS.GT.0.).AND.(FabsS.LT.99999))  CabsS=EXP(-DELTF/FabsS)
!Clu ----------------------------------------------------------------------
!
!     Read a high resolution MASK field for use in grib interpolation
!
        CALL HMSKRD(LUGB,IMSK,JMSK,FNMSKH,
     &              KPDMSK,SLMSKH,GAUSM,BLNMSK,BLTMSK,me)
!       IF (QCMSK) CALL QCMASK(SLMSKH,SLLND,SLSEA,IMSK,JMSK,RLA,RLO)
!
        if (me .eq. 0) then
          WRITE(6,*) ' '
          WRITE(6,*) ' LUGB=',LUGB,' LEN=',LEN, ' LSOIL=',LSOIL
          WRITE(6,*) 'IY=',IY,' IM=',IM,' ID=',ID,' IH=',IH,' FH=',FH
     &,            ' SIG1T(1)=',SIG1T(1)
     &,            ' gausm=',gausm,' blnmsk=',blnmsk,' bltmsk=',bltmsk
          WRITE(6,*) ' '
        endif
!
!  Reading Permanent/extreme features (glacier points and maximum ice extent)
!
        allocate (TSFCL0(LEN))
        allocate (GLACIR(LEN))
        allocate (AMXICE(LEN))
!
!  Read Glacier
!
        kpd9 = -1
        kpd7 = -1
        CALL FIXRDC(LUGB,FNGLAC,KPDGLA,kpd7,kpd9,SLMASK,
     &              GLACIR,LEN,IRET
     &,             IMSK, JMSK, SLMSKH, GAUSM, BLNMSK, BLTMSK
     &,             RLA, RLO, me)
!     ZNNT=1.
!     CALL NNTPRT(GLACIR,LEN,ZNNT)
!
!  Read Maximum ice extent
!
        kpd7 = -1
        CALL FIXRDC(LUGB,FNMXIC,KPDMXI,kpd7,kpd9,SLMASK,
     &              AMXICE,LEN,IRET
     &,             IMSK, JMSK, SLMSKH, GAUSM, BLNMSK, BLTMSK
     &,             RLA, RLO, me)
!     ZNNT=1.
!     CALL NNTPRT(AMXICE,LEN,ZNNT)
!
        CRIT=0.5
        CALL ROF01(GLACIR,LEN,'GE',CRIT)
        CALL ROF01(AMXICE,LEN,'GE',CRIT)
!
!  Quality control max ice limit based on glacier points
!
        CALL QCMXICE(GLACIR,AMXICE,LEN,me)
!
      ENDIF                       ! First time loop finished
!
      DO I=1,LEN
        SLICLM(I) = 1.
        SNOCLM(I) = 0.
        icefl1(i) = .true.
      ENDDO
!     if(lprnt) print *,' tsffcsIN=',tsffcs(iprnt)
!
!  Read climatology fields
!
      if (me .eq. 0) then
        WRITE(6,*) '=============='
        WRITE(6,*) 'CLIMATOLOGY'
        WRITE(6,*) '=============='
      endif
!
      PERCRIT=CRITP1
!
      CALL CLIMA(LUGB,IY,IM,ID,IH,FH,LEN,LSOIL,SLMASK,
     &           FNTSFC,FNWETC,FNSNOC,FNZORC,FNALBC,FNAISC,
     &           FNTG3C,FNSCVC,FNSMCC,FNSTCC,FNACNC,FNVEGC,
     &           fnvetc,fnsotc,
!Clu [+1L] add fn()c for vmn, vmx, slp, abs
     &           FNVMNC,FNVMXC,FNSLPC,FNABSC,
     &           TSFCLM,TSFCL2,WETCLM,SNOCLM,ZORCLM,ALBCLM,AISCLM,
     &           TG3CLM,CVCLM ,CVBCLM,CVTCLM,
     &           CNPCLM,SMCCLM,STCCLM,SLICLM,SCVCLM,ACNCLM,VEGCLM,
     &           vetclm,sotclm,ALFCLM,
!Clu [+1L] add ()clm for vmn, vmx, slp, abs
     &           VMNCLM,VMXCLM,SLPCLM,ABSCLM,
     &           KPDTSF,KPDWET,KPDSNO,KPDZOR,KPDALB,KPDAIS,
     &           KPDTG3,KPDSCV,KPDACN,KPDSMC,KPDSTC,KPDVEG,
     &           kpdvet,kpdsot,kpdalf,TSFCL0,
!Clu [+1L] add kpd() for vmn, vmx, slp, abs
     &           KPDVMN,KPDVMX,KPDSLP,KPDABS,
     &           DELTSFC, LANOM
     &,          IMSK, JMSK, SLMSKH, RLA, RLO, GAUSM, BLNMSK, BLTMSK,me
     &,          lprnt, iprnt, FNALBC2, IALB)
!     if(lprnt) print *,'tsfclm=',tsfclm(iprnt),' tsfcl2=',tsfcl2(iprnt)
!
!  Scale surface roughness and albedo to model required units
!
      ZSCA=100.
      CALL SCALE(ZORCLM,LEN,ZSCA)
      ZSCA=0.01
      CALL SCALE(ALBCLM,LEN,ZSCA)
      CALL SCALE(ALBCLM(1,2),LEN,ZSCA)
      CALL SCALE(ALBCLM(1,3),LEN,ZSCA)
      CALL SCALE(ALBCLM(1,4),LEN,ZSCA)
      CALL SCALE(ALFCLM,LEN,ZSCA)
      CALL SCALE(ALFCLM(1,2),LEN,ZSCA)
!Clu [+4L] scale vmn, vmx, abs from percent to fraction
      ZSCA=0.01
      CALL SCALE(VMNCLM,LEN,ZSCA)
      CALL SCALE(VMXCLM,LEN,ZSCA)
      CALL SCALE(ABSCLM,LEN,ZSCA)

!
!  Set albedo over ocean to ALBOMX
!
      CALL ALBOCN(ALBCLM,SLMASK,ALBOMX,LEN)
!
!  make sure vegetation type and soil type are non zero over land
!
!Clu [-1L/+1L]: add slpclm
!Clu  call landtyp(vetclm,sotclm,slmask,LEN)
      call landtyp(vetclm,sotclm,slpclm,slmask,LEN)
!
!Cwu [-1L/+1L]
!* Ice concentration or ice mask (only ice mask used in the model now)
!  Ice concentration and ice mask (both are used in the model now)
!
      IF(FNAISC(1:8).NE.'        ') THEN
!Cwu [+5L/-1L] Update SIHCLM, SICCLM
        DO I=1,LEN
         SIHCLM(I) = 3.0*AISCLM(I)
         SICCLM(I) = AISCLM(I)
          IF(SLMASK(I).EQ.0..AND.GLACIR(I).EQ.1..AND.
     &      SICCLM(I).NE.1.) THEN
            SICCLM(I) = SICIMX
            SIHFCS(I) = glacir_hice
          ENDIF
        ENDDO
        CRIT=AISLIM
!*      CRIT=0.5
        CALL ROF01(AISCLM,LEN,'GE',CRIT)
      ELSEIF(FNACNC(1:8).NE.'        ') THEN
!Cwu [+4L] Update SIHCLM, SICCLM
        DO I=1,LEN
         SIHCLM(I) = 3.0*ACNCLM(I)
         SICCLM(I) = ACNCLM(I)
          IF(SLMASK(I).EQ.0..AND.GLACIR(I).EQ.1..AND.
     &      SICCLM(I).NE.1.) THEN
            SICCLM(I) = SICIMX
            SIHFCS(I) = glacir_hice
          ENDIF
        ENDDO
        CALL ROF01(ACNCLM,LEN,'GE',AISLIM)
        DO I=1,LEN
         AISCLM(I) = ACNCLM(I)
        ENDDO
      ENDIF
!
!  Quality control of sea ice mask
!
      CALL QCSICE(AISCLM,GLACIR,AMXICE,AICICE,AICSEA,SLLND,SLMASK,
     &            RLA,RLO,LEN,me)
!
!  Set ocean/land/sea-ice mask
!
      CALL SETLSI(SLMASK,AISCLM,LEN,AICICE,SLICLM)
!     if(lprnt) print *,' aisclm=',aisclm(iprnt),' sliclm='
!    *,sliclm(iprnt),' slmask=',slmask(iprnt)
!
!     WRITE(6,*) 'SLICLM'
!     ZNNT=1.
!     CALL NNTPRT(SLICLM,LEN,ZNNT)
!
!  Quality control of snow
!
!cggg landice mods start
!       CALL QCSNOW(SNOCLM,SLMASK,AISCLM,GLACIR,LEN,SNOSMX,me)
       CALL QCSNOW(SNOCLM,SLMASK,AISCLM,GLACIR,LEN,SNOSMX,LANDICE,me)
!cggg landice mods end
!
      CALL SETZRO(SNOCLM,EPSSNO,LEN)
!
!  Snow cover handling (We assume climatological snow depth is available)
!  Quality control of snow depth (Note that Snow should be corrected first
!  because it influences TSF
!
      KQCM=1
      CALL QCMXMN('Snow    ',SNOCLM,SLICLM,SNOCLM,icefl1,
     &            SNOLMX,SNOLMN,SNOOMX,SNOOMN,SNOIMX,SNOIMN,
     &            SNOJMX,SNOJMN,SNOSMX,SNOSMN,EPSSNO,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!     WRITE(6,*) 'SNOCLM'
!     ZNNT=1.
!     CALL NNTPRT(SNOCLM,LEN,ZNNT)
!
!  Get snow cover from snow depth array
!
      IF(FNSCVC(1:8).EQ.'        ') THEN
        CALL GETSCV(SNOCLM,SCVCLM,LEN)
      ENDIF
!
!  Set TSFC over snow to TSFSMX if greater
!
      CALL SNOSFC(SNOCLM,TSFCLM,TSFSMX,LEN,me)
!     CALL SNOSFC(SNOCLM,TSFCL2,TSFSMX,LEN)

!
!  Quality control
!
      do i=1,len
        icefl2(i) = sicclm(i) .gt. 0.99999
      enddo
      KQCM=1
      CALL QCMXMN('TSFc    ',TSFCLM,SLICLM,SNOCLM,icefl2,
     &            TSFLMX,TSFLMN,TSFOMX,TSFOMN,TSFIMX,TSFIMN,
     &            TSFJMX,TSFJMN,TSFSMX,TSFSMN,EPSTSF,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('TSf2    ',TSFCL2,SLICLM,SNOCLM,icefl2,
     &            TSFLMX,TSFLMN,TSFOMX,TSFOMN,TSFIMX,TSFIMN,
     &            TSFJMX,TSFJMN,TSFSMX,TSFSMN,EPSTSF,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      do kk = 1, 4
      CALL QCMXMN('ALBc    ',ALBCLM(1,kk),SLICLM,SNOCLM,icefl1,
     &            ALBLMX,ALBLMN,ALBOMX,ALBOMN,ALBIMX,ALBIMN,
     &            ALBJMX,ALBJMN,ALBSMX,ALBSMN,EPSALB,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      enddo
      IF(FNWETC(1:8).NE.'        ') THEN
        CALL QCMXMN('WETc    ',WETCLM,SLICLM,SNOCLM,icefl1,
     &              WETLMX,WETLMN,WETOMX,WETOMN,WETIMX,WETIMN,
     &              WETJMX,WETJMN,WETSMX,WETSMN,EPSWET,
     &              RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      ENDIF
      CALL QCMXMN('ZORc    ',ZORCLM,SLICLM,SNOCLM,icefl1,
     &            ZORLMX,ZORLMN,ZOROMX,ZOROMN,ZORIMX,ZORIMN,
     &            ZORJMX,ZORJMN,ZORSMX,ZORSMN,EPSZOR,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!     IF(FNPLRC(1:8).NE.'        ') THEN
!     CALL QCMXMN('PLNTc   ',PLRCLM,SLICLM,SNOCLM,icefl1,
!    &            PLRLMX,PLRLMN,PLROMX,PLROMN,PLRIMX,PLRIMN,
!    &            PLRJMX,PLRJMN,PLRSMX,PLRSMN,EPSPLR,
!    &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!     ENDIF
      CALL QCMXMN('TG3c    ',TG3CLM,SLICLM,SNOCLM,icefl1,
     &            TG3LMX,TG3LMN,TG3OMX,TG3OMN,TG3IMX,TG3IMN,
     &            TG3JMX,TG3JMN,TG3SMX,TG3SMN,EPSTG3,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!
!  Get soil temp and moisture (after all the QCs are completed)
!
      IF(FNSMCC(1:8).EQ.'        ') THEN
        CALL GETSMC(WETCLM,LEN,LSOIL,SMCCLM,me)
      ENDIF
      CALL QCMXMN('SMC1c   ',SMCCLM(1,1),SLICLM,SNOCLM,icefl1,
     &            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SMC2c   ',SMCCLM(1,2),SLICLM,SNOCLM,icefl1,
     &            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu [+8L] add smcclm(3:4)
      IF(LSOIL.GT.2) THEN
      CALL QCMXMN('SMC3c   ',SMCCLM(1,3),SLICLM,SNOCLM,icefl1,
     &            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SMC4c   ',SMCCLM(1,4),SLICLM,SNOCLM,icefl1,
     &            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      ENDIF
      IF(FNSTCC(1:8).EQ.'        ') THEN
        CALL GETSTC(TSFCLM,TG3CLM,SLICLM,LEN,LSOIL,STCCLM,TSFIMX)
      ENDIF
      CALL QCMXMN('STC1c   ',STCCLM(1,1),SLICLM,SNOCLM,icefl1,
     &            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('STC2c   ',STCCLM(1,2),SLICLM,SNOCLM,icefl1,
     &            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu [+8L] add stcclm(3:4)
      IF(LSOIL.GT.2) THEN
      CALL QCMXMN('STC3c   ',STCCLM(1,3),SLICLM,SNOCLM,icefl1,
     &            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('STC4c   ',STCCLM(1,4),SLICLM,SNOCLM,icefl1,
     &            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      ENDIF
      CALL QCMXMN('VEGc    ',VEGCLM,SLICLM,SNOCLM,icefl1,
     &            VEGLMX,VEGLMN,VEGOMX,VEGOMN,VEGIMX,VEGIMN,
     &            VEGJMX,VEGJMN,VEGSMX,VEGSMN,EPSVEG,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('VETc    ',VETCLM,SLICLM,SNOCLM,icefl1,
     &            VETLMX,VETLMN,VETOMX,VETOMN,VETIMX,VETIMN,
     &            VETJMX,VETJMN,VETSMX,VETSMN,EPSVET,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SOTc    ',SOTCLM,SLICLM,SNOCLM,icefl1,
     &            SOTLMX,SOTLMN,SOTOMX,SOTOMN,SOTIMX,SOTIMN,
     &            SOTJMX,SOTJMN,SOTSMX,SOTSMN,EPSSOT,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Cwu [+8L] ---------------------------------------------------------------
      CALL QCMXMN('SIHc    ',SIHCLM,SLICLM,SNOCLM,icefl1,
     &            SIHLMX,SIHLMN,SIHOMX,SIHOMN,SIHIMX,SIHIMN,
     &            SIHJMX,SIHJMN,SIHSMX,SIHSMN,EPSSIH,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SICc    ',SICCLM,SLICLM,SNOCLM,icefl1,
     &            SICLMX,SICLMN,SICOMX,SICOMN,SICIMX,SICIMN,
     &            SICJMX,SICJMN,SICSMX,SICSMN,EPSSIC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu [+16L] ---------------------------------------------------------------
      CALL QCMXMN('VMNc    ',VMNCLM,SLICLM,SNOCLM,icefl1,
     &            VMNLMX,VMNLMN,VMNOMX,VMNOMN,VMNIMX,VMNIMN,
     &            VMNJMX,VMNJMN,VMNSMX,VMNSMN,EPSVMN,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('VMXc    ',VMXCLM,SLICLM,SNOCLM,icefl1,
     &            VMXLMX,VMXLMN,VMXOMX,VMXOMN,VMXIMX,VMXIMN,
     &            VMXJMX,VMXJMN,VMXSMX,VMXSMN,EPSVMX,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SLPc    ',SLPCLM,SLICLM,SNOCLM,icefl1,
     &            SLPLMX,SLPLMN,SLPOMX,SLPOMN,SLPIMX,SLPIMN,
     &            SLPJMX,SLPJMN,SLPSMX,SLPSMN,EPSSLP,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('ABSc    ',ABSCLM,SLICLM,SNOCLM,icefl1,
     &            ABSLMX,ABSLMN,ABSOMX,ABSOMN,ABSIMX,ABSIMN,
     &            ABSJMX,ABSJMN,ABSSMX,ABSSMN,EPSABS,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu ----------------------------------------------------------------------
!
!  MONITORING PRINTS
!
      IF (MONCLM) THEN
       if (me .eq. 0) then
        PRINT *,' '
        PRINT *,'MONITOR OF TIME AND SPACE INTERPOLATED CLIMATOLOGY'
        PRINT *,' '
!       CALL COUNT(SLICLM,SNOCLM,LEN)
        PRINT *,' '
!        CALL MONITR('TSFCLM',TSFCLM,SLICLM,SNOCLM,LEN)
!        CALL MONITR('ALBCLM',ALBCLM(1,1),SLICLM,SNOCLM,LEN)
!        CALL MONITR('ALBCLM',ALBCLM(1,2),SLICLM,SNOCLM,LEN)
!        CALL MONITR('ALBCLM',ALBCLM(1,3),SLICLM,SNOCLM,LEN)
!        CALL MONITR('ALBCLM',ALBCLM(1,4),SLICLM,SNOCLM,LEN)
!        CALL MONITR('AISCLM',AISCLM,SLICLM,SNOCLM,LEN)
!        CALL MONITR('SNOCLM',SNOCLM,SLICLM,SNOCLM,LEN)
!        CALL MONITR('SCVCLM',SCVCLM,SLICLM,SNOCLM,LEN)
!        CALL MONITR('SMCCLM1',SMCCLM(1,1),SLICLM,SNOCLM,LEN)
!        CALL MONITR('SMCCLM2',SMCCLM(1,2),SLICLM,SNOCLM,LEN)
!        CALL MONITR('STCCLM1',STCCLM(1,1),SLICLM,SNOCLM,LEN)
!        CALL MONITR('STCCLM2',STCCLM(1,2),SLICLM,SNOCLM,LEN)
!Clu [+4L] add smcclm(3:4) and stcclm(3:4)
        IF(LSOIL.GT.2) THEN
!        CALL MONITR('SMCCLM3',SMCCLM(1,3),SLICLM,SNOCLM,LEN)
!        CALL MONITR('SMCCLM4',SMCCLM(1,4),SLICLM,SNOCLM,LEN)
!        CALL MONITR('STCCLM3',STCCLM(1,3),SLICLM,SNOCLM,LEN)
!        CALL MONITR('STCCLM4',STCCLM(1,4),SLICLM,SNOCLM,LEN)
        ENDIF
!        CALL MONITR('TG3CLM',TG3CLM,SLICLM,SNOCLM,LEN)
!        CALL MONITR('ZORCLM',ZORCLM,SLICLM,SNOCLM,LEN)
!       IF (GAUS) THEN
!          CALL MONITR('CVACLM',CVCLM ,SLICLM,SNOCLM,LEN)
!          CALL MONITR('CVBCLM',CVBCLM,SLICLM,SNOCLM,LEN)
!          CALL MONITR('CVTCLM',CVTCLM,SLICLM,SNOCLM,LEN)
!       ENDIF
!        CALL MONITR('SLICLM',SLICLM,SLICLM,SNOCLM,LEN)
!       CALL MONITR('PLRCLM',PLRCLM,SLICLM,SNOCLM,LEN)
!        CALL MONITR('OROG  ',OROG  ,SLICLM,SNOCLM,LEN)
!        CALL MONITR('VEGCLM',VEGCLM,SLICLM,SNOCLM,LEN)
!        CALL MONITR('VETCLM',VETCLM,SLICLM,SNOCLM,LEN)
!        CALL MONITR('SOTCLM',SOTCLM,SLICLM,SNOCLM,LEN)
!Cwu [+2L] add sih, sic
!        CALL MONITR('SIHCLM',SIHCLM,SLICLM,SNOCLM,LEN)
!        CALL MONITR('SICCLM',SICCLM,SLICLM,SNOCLM,LEN)
!Clu [+4L] add vmn, vmx, slp, abs
!        CALL MONITR('VMNCLM',VMNCLM,SLICLM,SNOCLM,LEN)
!        CALL MONITR('VMXCLM',VMXCLM,SLICLM,SNOCLM,LEN)
!        CALL MONITR('SLPCLM',SLPCLM,SLICLM,SNOCLM,LEN)
!        CALL MONITR('ABSCLM',ABSCLM,SLICLM,SNOCLM,LEN)
       endif
      ENDIF
!
!
      if (me .eq. 0) then
        WRITE(6,*) '=============='
        WRITE(6,*) '   ANALYSIS'
        WRITE(6,*) '=============='
      endif
!
!  Fill in analysis array with climatology before reading analysis.
!
      CALL FILANL(TSFANL,TSFAN2,WETANL,SNOANL,ZORANL,ALBANL,AISANL,
     &            TG3ANL,CVANL ,CVBANL,CVTANL,
     &            CNPANL,SMCANL,STCANL,SLIANL,SCVANL,VEGANL,
     &            vetanl,sotanl,ALFANL,
!Cwu [+1L] add ()anl for sih, sic
     &            SIHANL,SICANL,
!Clu [+1L] add ()anl for vmn, vmx, slp, abs
     &            VMNANL,VMXANL,SLPANL,ABSANL, 
     &            TSFCLM,TSFCL2,WETCLM,SNOCLM,ZORCLM,ALBCLM,AISCLM,
     &            TG3CLM,CVCLM ,CVBCLM,CVTCLM,
     &            CNPCLM,SMCCLM,STCCLM,SLICLM,SCVCLM,VEGCLM,
     &            vetclm,sotclm,ALFCLM,
!Cwu [+1L] add ()clm for sih, sic
     &            SIHCLM,SICCLM,
!Clu [+1L] add ()clm for vmn, vmx, slp, abs
     &            VMNCLM,VMXCLM,SLPCLM,ABSCLM,      
     &            LEN,LSOIL)
!
!  Reverse scaling to match with grib analysis input
!
      ZSCA=0.01
      CALL SCALE(ZORANL,LEN, ZSCA)
      ZSCA=100.
      CALL SCALE(ALBANL,LEN,ZSCA)
      CALL SCALE(ALBANL(1,2),LEN,ZSCA)
      CALL SCALE(ALBANL(1,3),LEN,ZSCA)
      CALL SCALE(ALBANL(1,4),LEN,ZSCA)
      CALL SCALE(ALFANL,LEN,ZSCA)
      CALL SCALE(ALFANL(1,2),LEN,ZSCA)
!Clu [+4L] reverse scale for vmn, vmx, abs
      ZSCA=100.
      CALL SCALE(VMNANL,LEN,ZSCA)
      CALL SCALE(VMXANL,LEN,ZSCA)
      CALL SCALE(ABSANL,LEN,ZSCA)
!
      PERCRIT=CRITP2
!
!  READ ANALYSIS FIELDS
!
      CALL ANALY(LUGB,IY,IM,ID,IH,FH,LEN,LSOIL,SLMASK,
     &           FNTSFA,FNWETA,FNSNOA,FNZORA,FNALBA,FNAISA,
     &           FNTG3A,FNSCVA,FNSMCA,FNSTCA,FNACNA,FNVEGA,
     &           fnveta,fnsota,
!Clu [+1L] add fn()a for vmn, vmx, slp, abs
     &           FNVMNA,FNVMXA,FNSLPA,FNABSA,      
     &           TSFANL,WETANL,SNOANL,ZORANL,ALBANL,AISANL,
     &           TG3ANL,CVANL ,CVBANL,CVTANL,
     &           SMCANL,STCANL,SLIANL,SCVANL,ACNANL,VEGANL,
     &           vetanl,sotanl,ALFANL,TSFAN0,
!Clu [+1L] add ()anl for vmn, vmx, slp, abs
     &           VMNANL,VMXANL,SLPANL,ABSANL,      
!cggg snow mods start     &   KPDTSF,KPDWET,KPDSNO,KPDZOR,KPDALB,KPDAIS,
     &           KPDTSF,KPDWET,KPDSNO,KPDSND,KPDZOR,KPDALB,KPDAIS,
!cggg snow mods end
     &           KPDTG3,KPDSCV,KPDACN,KPDSMC,KPDSTC,KPDVEG,
     &           kpdvet,kpdsot,kpdalf,
!Clu [+1L] add kpd() for vmn, vmx, slp, abs
     &           KPDVMN,KPDVMX,KPDSLP,KPDABS,      
     &           IRTTSF,IRTWET,IRTSNO,IRTZOR,IRTALB,IRTAIS,
     &           IRTTG3,IRTSCV,IRTACN,IRTSMC,IRTSTC,IRTVEG,
     &           irtvet,irtsot,irtalf
!Clu [+1L] add irt() for vmn, vmx, slp, abs
     &,          IRTVMN,IRTVMX,IRTSLP,IRTABS, 
     &           IMSK, JMSK, SLMSKH, RLA, RLO, GAUSM, BLNMSK, BLTMSK,me)
!     if(lprnt) print *,' tsfanl=',tsfanl(iprnt)


!
!  Scale ZOR and ALB to match forecast model units
!
      ZSCA=100.
      CALL SCALE(ZORANL,LEN, ZSCA)
      ZSCA=0.01
      CALL SCALE(ALBANL,LEN,ZSCA)
      CALL SCALE(ALBANL(1,2),LEN,ZSCA)
      CALL SCALE(ALBANL(1,3),LEN,ZSCA)
      CALL SCALE(ALBANL(1,4),LEN,ZSCA)
      CALL SCALE(ALFANL,LEN,ZSCA)
      CALL SCALE(ALFANL(1,2),LEN,ZSCA)
!Clu [+4] scale vmn, vmx, abs from percent to fraction
      ZSCA=0.01
      CALL SCALE(VMNANL,LEN,ZSCA)
      CALL SCALE(VMXANL,LEN,ZSCA)
      CALL SCALE(ABSANL,LEN,ZSCA)
!
!  Interpolate climatology but fixing initial anomaly
!
      IF(FH.GT.0.0.AND.FNTSFA(1:8).NE.'        '.AND.LANOM) THEN
        CALL ANOMINT(TSFAN0,TSFCLM,TSFCL0,TSFANL,LEN)
      ENDIF
!
!    If the TSFANL is at sea level, then bring it to the surface using
!    unfiltered orography (for lakes).  If the analysis is at lake surface
!    as in the NST model, then this call should be removed - Moorthi 09/23/2011
!
        if (use_ufo) then
          ZTSFC = 0.0
          CALL TSFCOR(TSFANL,OROG_uf,SLMASK,ZTSFC,LEN,RLAPSE)
        endif
!
!  Ice concentration or ice mask (only ice mask used in the model now)
!
      IF(FNAISA(1:8).NE.'        ') THEN
!Cwu [+5L/-1L] Update SIHANL, SICANL
        DO I=1,LEN
         SIHANL(I) = 3.0*AISANL(I)
         SICANL(I) = AISANL(I)
          IF(SLMASK(I).EQ.0..AND.GLACIR(I).EQ.1..AND.
     &      SICANL(I).NE.1.) THEN
            SICANL(I) = SICIMX
            SIHFCS(I) = glacir_hice
          ENDIF
        ENDDO
        CRIT=AISLIM
!*      CRIT=0.5
        CALL ROF01(AISANL,LEN,'GE',CRIT)
      ELSEIF(FNACNA(1:8).NE.'        ') THEN
!Cwu [+17L] update SIHANL, SICANL
        DO I=1,LEN
          SIHANL(I) = 3.0*ACNANL(I)
          SICANL(I) = ACNANL(I)
          IF(SLMASK(I).EQ.0..AND.GLACIR(I).EQ.1..AND.
     &     SICANL(I).NE.1.) THEN
            SICANL(I) = SICIMX
            SIHFCS(I) = glacir_hice
          ENDIF
        ENDDO
        CRIT=AISLIM
        DO I=1,LEN
          IF((SLIANL(I).EQ.0.).AND.(SICANL(I).GE.CRIT)) THEN
            SLIANL(I)=2.
!           PRINT *,'cycle - NEW ICE FORM: FICE=',SICANL(I)
          ELSE IF((SLIANL(I).GE.2.).AND.(SICANL(I).LT.CRIT)) THEN
            SLIANL(I)=0.
!           PRINT *,'cycle - ICE FREE: FICE=',SICANL(I)
          ELSE IF((SLIANL(I).EQ.1.).AND.(SICANL(I).GE.SICIMN)) THEN
!           PRINT *,'cycle - LAND COVERED BY SEA-ICE: FICE=',SICANL(I)
            SICANL(I)=0.
          ENDIF
        ENDDO
!       ZNNT=10.
!       CALL NNTPRT(ACNANL,LEN,ZNNT)
!     if(lprnt) print *,' acnanl=',acnanl(iprnt)
!       DO I=1,LEN
!         if (ACNANL(I) .GT. 0.3 .AND. AISCLM(I) .EQ. 1.0
!    &     .AND. AISFCS(I) .GE. 0.75)   ACNANL(I) = AISLIM
!       ENDDO
!     if(lprnt) print *,' acnanl=',acnanl(iprnt)
        CALL ROF01(ACNANL,LEN,'GE',AISLIM)
        DO I=1,LEN
          AISANL(I)=ACNANL(I)
        ENDDO
      ENDIF
!     if(lprnt) print *,' aisanl1=',aisanl(iprnt),' glacir='
!    &,glacir(iprnt),' slmask=',slmask(iprnt)
!
      CALL QCSICE(AISANL,GLACIR,AMXICE,AICICE,AICSEA,SLLND,SLMASK,
     &            RLA,RLO,LEN,me)
!
!  Set ocean/land/sea-ice mask
!
      CALL SETLSI(SLMASK,AISANL,LEN,AICICE,SLIANL)
!     if(lprnt) print *,' aisanl=',aisanl(iprnt),' slianl='
!    *,slianl(iprnt),' slmask=',slmask(iprnt)
!
!
      do k=1,lsoil
        do i=1,len
          if (slianl(i) .eq. 0) then
            smcanl(i,k) = smcomx
            stcanl(i,k) = tsfanl(i)
          endif
        enddo
      enddo

!     WRITE(6,*) 'SLIANL'
!     ZNNT=1.
!     CALL NNTPRT(SLIANL,LEN,ZNNT)
!Cwu [+8L]----------------------------------------------------------------------
      CALL QCMXMN('SIHa    ',SIHANL,SLIANL,SNOANL,icefl1,
     &            SIHLMX,SIHLMN,SIHOMX,SIHOMN,SIHIMX,SIHIMN,
     &            SIHJMX,SIHJMN,SIHSMX,SIHSMN,EPSSIH,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SICa    ',SICANL,SLIANL,SNOANL,icefl1,
     &            SICLMX,SICLMN,SICOMX,SICOMN,SICIMX,SICIMN,
     &            SICJMX,SICJMN,SICSMX,SICSMN,EPSSIC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!
!  Set albedo over ocean to ALBOMX
!
      CALL ALBOCN(ALBANL,SLMASK,ALBOMX,LEN)
!
!  Quality control of snow and sea-ice
!    Process snow depth or snow cover
!
      IF(FNSNOA(1:8).NE.'        ') THEN
        CALL SETZRO(SNOANL,EPSSNO,LEN)
!cggg landice mods start
!         CALL QCSNOW(SNOANL,SLMASK,AISANL,GLACIR,LEN,10.,me)
         CALL QCSNOW(SNOANL,SLMASK,AISANL,GLACIR,LEN,ten,LANDICE,me)
!cggg landice mods end
!cggg landice mods start
!       CALL SNODPTH2(GLACIR,SNOSMX,SNOANL, LEN, me)
        IF (.NOT.LANDICE) THEN
          CALL SNODPTH2(GLACIR,SNOSMX,SNOANL, LEN, me)
        ENDIF
!cggg landice mods end
        KQCM=1
        CALL SNOSFC(SNOANL,TSFANL,TSFSMX,LEN,me)
        CALL QCMXMN('Snoa    ',SNOANL,SLIANL,SNOANL,icefl1,
     &              SNOLMX,SNOLMN,SNOOMX,SNOOMN,SNOIMX,SNOIMN,
     &              SNOJMX,SNOJMN,SNOSMX,SNOSMN,EPSSNO,
     &              RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
        CALL GETSCV(SNOANL,SCVANL,LEN)
        CALL QCMXMN('Sncva   ',SCVANL,SLIANL,SNOANL,icefl1,
     &              SCVLMX,SCVLMN,SCVOMX,SCVOMN,SCVIMX,SCVIMN,
     &              SCVJMX,SCVJMN,SCVSMX,SCVSMN,EPSSCV,
     &              RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      ELSE
        CRIT=0.5
        CALL ROF01(SCVANL,LEN,'GE',CRIT)
!cggg landice mods start
!        CALL QCSNOW(SCVANL,SLMASK,AISANL,GLACIR,LEN,1.,me)
        CALL QCSNOW(SCVANL,SLMASK,AISANL,GLACIR,LEN,one,LANDICE,me)
!cggg landice mods end
        CALL QCMXMN('SNcva   ',SCVANL,SLIANL,SCVANL,icefl1,
     &              SCVLMX,SCVLMN,SCVOMX,SCVOMN,SCVIMX,SCVIMN,
     &              SCVJMX,SCVJMN,SCVSMX,SCVSMN,EPSSCV,
     &              RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!cggg landice mods start
!        CALL SNODPTH(SCVANL,SLIANL,TSFANL,SNOCLM,
!     &               GLACIR,SNWMAX,SNWMIN,LEN,SNOANL,me)
        CALL SNODPTH(SCVANL,SLIANL,TSFANL,SNOCLM,
     &               GLACIR,SNWMAX,SNWMIN,LANDICE,LEN,SNOANL,me)
!cggg landice mods end
!cggg landice mods start
!        CALL QCSNOW(SCVANL,SLMASK,AISANL,GLACIR,LEN,SNOSMX,me)
        CALL QCSNOW(SCVANL,SLMASK,AISANL,GLACIR,LEN,SNOSMX,LANDICE,me)
!cggg landice mods end
        CALL SNOSFC(SNOANL,TSFANL,TSFSMX,LEN,me)
        CALL QCMXMN('SNowa   ',SNOANL,SLIANL,SNOANL,icefl1,
     &              SNOLMX,SNOLMN,SNOOMX,SNOOMN,SNOIMX,SNOIMN,
     &              SNOJMX,SNOJMN,SNOSMX,SNOSMN,EPSSNO,
     &              RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      ENDIF
!
      do i=1,len
        icefl2(i) = sicanl(i) .gt. 0.99999
      enddo
      CALL QCMXMN('TSFa    ',TSFANL,SLIANL,SNOANL,icefl2,
     &            TSFLMX,TSFLMN,TSFOMX,TSFOMN,TSFIMX,TSFIMN,
     &            TSFJMX,TSFJMN,TSFSMX,TSFSMN,EPSTSF,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      do kk = 1, 4
      CALL QCMXMN('ALBa    ',ALBANL(1,kk),SLIANL,SNOANL,icefl1,
     &            ALBLMX,ALBLMN,ALBOMX,ALBOMN,ALBIMX,ALBIMN,
     &            ALBJMX,ALBJMN,ALBSMX,ALBSMN,EPSALB,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      enddo
      IF(FNWETC(1:8).NE.'        ' .OR. FNWETA(1:8).NE.'        ' ) THEN
      CALL QCMXMN('WETa    ',WETANL,SLIANL,SNOANL,icefl1,
     &            WETLMX,WETLMN,WETOMX,WETOMN,WETIMX,WETIMN,
     &            WETJMX,WETJMN,WETSMX,WETSMN,EPSWET,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      ENDIF
      CALL QCMXMN('ZORa    ',ZORANL,SLIANL,SNOANL,icefl1,
     &            ZORLMX,ZORLMN,ZOROMX,ZOROMN,ZORIMX,ZORIMN,
     &            ZORJMX,ZORJMN,ZORSMX,ZORSMN,EPSZOR,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!     IF(FNPLRC(1:8).NE.'        ' .OR. FNPLRA(1:8).NE.'        ' ) THEN
!     CALL QCMXMN('PLNa    ',PLRANL,SLIANL,SNOANL,icefl1,
!    &            PLRLMX,PLRLMN,PLROMX,PLROMN,PLRIMX,PLRIMN,
!    &            PLRJMX,PLRJMN,PLRSMX,PLRSMN,EPSPLR,
!    &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!     ENDIF
      CALL QCMXMN('TG3a    ',TG3ANL,SLIANL,SNOANL,icefl1,
     &            TG3LMX,TG3LMN,TG3OMX,TG3OMN,TG3IMX,TG3IMN,
     &            TG3JMX,TG3JMN,TG3SMX,TG3SMN,EPSTG3,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!
!  Get soil temp and moisture
!
      IF(FNSMCA(1:8).EQ.'        ' .AND. FNSMCC(1:8).EQ.'        ') THEN
        CALL GETSMC(WETANL,LEN,LSOIL,SMCANL,me)
      ENDIF
      CALL QCMXMN('SMC1a   ',SMCANL(1,1),SLIANL,SNOANL,icefl1,
     &            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SMC2a   ',SMCANL(1,2),SLIANL,SNOANL,icefl1,
     &            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu [+8L] add smcanl(3:4)
      IF(LSOIL.GT.2) THEN
      CALL QCMXMN('SMC3a   ',SMCANL(1,3),SLIANL,SNOANL,icefl1,
     &            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SMC4a   ',SMCANL(1,4),SLIANL,SNOANL,icefl1,
     &            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      ENDIF
      IF(FNSTCA(1:8).EQ.'        ') THEN
        CALL GETSTC(TSFANL,TG3ANL,SLIANL,LEN,LSOIL,STCANL,TSFIMX)
      ENDIF
      CALL QCMXMN('STC1a   ',STCANL(1,1),SLIANL,SNOANL,icefl1,
     &            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('STC2a   ',STCANL(1,2),SLIANL,SNOANL,icefl1,
     &            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu [+8L] add stcanl(3:4)
      IF(LSOIL.GT.2) THEN
      CALL QCMXMN('STC3a   ',STCANL(1,3),SLIANL,SNOANL,icefl1,
     &            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('STC4a   ',STCANL(1,4),SLIANL,SNOANL,icefl1,
     &            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      ENDIF
      CALL QCMXMN('VEGa    ',VEGANL,SLIANL,SNOANL,icefl1,
     &            VEGLMX,VEGLMN,VEGOMX,VEGOMN,VEGIMX,VEGIMN,
     &            VEGJMX,VEGJMN,VEGSMX,VEGSMN,EPSVEG,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('VETa    ',VETANL,SLIANL,SNOANL,icefl1,
     &            VETLMX,VETLMN,VETOMX,VETOMN,VETIMX,VETIMN,
     &            VETJMX,VETJMN,VETSMX,VETSMN,EPSVET,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SOTa    ',SOTANL,SLIANL,SNOANL,icefl1,
     &            SOTLMX,SOTLMN,SOTOMX,SOTOMN,SOTIMX,SOTIMN,
     &            SOTJMX,SOTJMN,SOTSMX,SOTSMN,EPSSOT,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu [+16L]----------------------------------------------------------------------
      CALL QCMXMN('VMNa    ',VMNANL,SLIANL,SNOANL,icefl1,
     &            VMNLMX,VMNLMN,VMNOMX,VMNOMN,VMNIMX,VMNIMN,
     &            VMNJMX,VMNJMN,VMNSMX,VMNSMN,EPSVMN,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('VMXa    ',VMXANL,SLIANL,SNOANL,icefl1,
     &            VMXLMX,VMXLMN,VMXOMX,VMXOMN,VMXIMX,VMXIMN,
     &            VMXJMX,VMXJMN,VMXSMX,VMXSMN,EPSVMX,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SLPa    ',SLPANL,SLIANL,SNOANL,icefl1,
     &            SLPLMX,SLPLMN,SLPOMX,SLPOMN,SLPIMX,SLPIMN,
     &            SLPJMX,SLPJMN,SLPSMX,SLPSMN,EPSSLP,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('ABSa    ',ABSANL,SLIANL,SNOANL,icefl1,
     &            ABSLMX,ABSLMN,ABSOMX,ABSOMN,ABSIMX,ABSIMN,
     &            ABSJMX,ABSJMN,ABSSMX,ABSSMN,EPSABS,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu ----------------------------------------------------------------------------
!
!  MONITORING PRINTS
!
      IF (MONANL) THEN
       if (me .eq. 0) then
        PRINT *,' '
        PRINT *,'MONITOR OF TIME AND SPACE INTERPOLATED ANALYSIS'
        PRINT *,' '
!       CALL COUNT(SLIANL,SNOANL,LEN)
        PRINT *,' '
!        CALL MONITR('TSFANL',TSFANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('ALBANL',ALBANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('AISANL',AISANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('SNOANL',SNOANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('SCVANL',SCVANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('SMCANL1',SMCANL(1,1),SLIANL,SNOANL,LEN)
!        CALL MONITR('SMCANL2',SMCANL(1,2),SLIANL,SNOANL,LEN)
!        CALL MONITR('STCANL1',STCANL(1,1),SLIANL,SNOANL,LEN)
!        CALL MONITR('STCANL2',STCANL(1,2),SLIANL,SNOANL,LEN)
!Clu [+4L] add smcanl(3:4) and stcanl(3:4)
        IF(LSOIL.GT.2) THEN
!        CALL MONITR('SMCANL3',SMCANL(1,3),SLIANL,SNOANL,LEN)
!        CALL MONITR('SMCANL4',SMCANL(1,4),SLIANL,SNOANL,LEN)
!        CALL MONITR('STCANL3',STCANL(1,3),SLIANL,SNOANL,LEN)
!        CALL MONITR('STCANL4',STCANL(1,4),SLIANL,SNOANL,LEN)
        ENDIF
!        CALL MONITR('TG3ANL',TG3ANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('ZORANL',ZORANL,SLIANL,SNOANL,LEN)
!       IF (GAUS) THEN
!          CALL MONITR('CVAANL',CVANL ,SLIANL,SNOANL,LEN)
!          CALL MONITR('CVBANL',CVBANL,SLIANL,SNOANL,LEN)
!          CALL MONITR('CVTANL',CVTANL,SLIANL,SNOANL,LEN)
!       ENDIF
!        CALL MONITR('SLIANL',SLIANL,SLIANL,SNOANL,LEN)
!       CALL MONITR('PLRANL',PLRANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('OROG  ',OROG  ,SLIANL,SNOANL,LEN)
!        CALL MONITR('VEGANL',VEGANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('VETANL',VETANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('SOTANL',SOTANL,SLIANL,SNOANL,LEN)
!Cwu [+2L] add sih, sic
!        CALL MONITR('SIHANL',SIHANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('SICANL',SICANL,SLIANL,SNOANL,LEN)
!Clu [+4L] add vmn, vmx, slp, abs
!        CALL MONITR('VMNANL',VMNANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('VMXANL',VMXANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('SLPANL',SLPANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('ABSANL',ABSANL,SLIANL,SNOANL,LEN)
       endif

      ENDIF
!
!  Read in forecast fields if needed
!
      if (me .eq. 0) then
        WRITE(6,*) '=============='
        WRITE(6,*) '  FCST GUESS'
        WRITE(6,*) '=============='
      endif
!
        PERCRIT=CRITP2
!
      IF(DEADS) THEN
!
!  Fill in guess array with Analysis if dead start.
!
        PERCRIT=CRITP3
        if (me .eq. 0) WRITE(6,*) 'THIS RUN IS DEAD START RUN'
        CALL FILFCS(TSFFCS,WETFCS,SNOFCS,ZORFCS,ALBFCS,
     &              TG3FCS,CVFCS ,CVBFCS,CVTFCS,
     &              CNPFCS,SMCFCS,STCFCS,SLIFCS,AISFCS,
     &              VEGFCS,vetfcs,sotfcs,alffcs,
!Cwu [+1L] add ()fcs for sih, sic
     &              SIHFCS,SICFCS,
!Clu [+1L] add ()fcs for vmn, vmx, slp, abs
     &              VMNFCS,VMXFCS,SLPFCS,ABSFCS,
     &              TSFANL,WETANL,SNOANL,ZORANL,ALBANL,
     &              TG3ANL,CVANL ,CVBANL,CVTANL,
     &              CNPANL,SMCANL,STCANL,SLIANL,AISANL,
     &              VEGANL,vetanl,sotanl,ALFANL,
!Cwu [+1L] add ()anl for sih, sic
     &              SIHANL,SICANL,
!Clu [+1L] add ()anl for vmn, vmx, slp, abs
     &              VMNANL,VMXANL,SLPANL,ABSANL,     
     &              LEN,LSOIL)
        IF(SIG1T(1).NE.0.) THEN
          CALL USESGT(SIG1T,SLIANL,TG3ANL,LEN,LSOIL,TSFFCS,STCFCS,
     &                TSFIMX)
         do i=1,len
            icefl2(i) = sicfcs(i) .gt. 0.99999
          enddo
          KQCM=1
          CALL QCMXMN('TSFf    ',TSFFCS,SLIFCS,SNOFCS,icefl2,
     &                TSFLMX,TSFLMN,TSFOMX,TSFOMN,TSFIMX,TSFIMN,
     &                TSFJMX,TSFJMN,TSFSMX,TSFSMN,EPSTSF,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('STC1f   ',STCFCS(1,1),SLIFCS,SNOFCS,icefl1,
     &                STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &                STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('STC2f   ',STCFCS(1,2),SLIFCS,SNOFCS,icefl1,
     &                STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &                STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
        ENDIF
      ELSE
        PERCRIT=CRITP2
!
!  Make reverse angulation correction to TSF
!  Make reverse orography correction to TG3
!
        if (use_ufo) then
          ZTSFC = 1.0
          orogd = orog - orog_uf
          CALL TSFCOR(TG3FCS,OROGd,SLMASK,ZTSFC,LEN,-RLAPSE)
          ZTSFC = 0.
          CALL TSFCOR(TSFFCS,OROGd,SLMASK,ZTSFC,LEN,-RLAPSE)
        else
          ZTSFC = 0.
          CALL TSFCOR(TSFFCS,OROG,SLMASK,ZTSFC,LEN,-RLAPSE)
        endif

!Clu [+12L]  --------------------------------------------------------------
!
!  Compute soil moisture liquid-to-total ratio over land
!
        DO J=1, LSOIL
        DO I=1, LEN
         IF(SMCFCS(I,J) .NE. 0.)  THEN
            SWRATIO(I,J) = SLCFCS(I,J)/SMCFCS(I,J)
           ELSE
            SWRATIO(I,J) = -999.
         ENDIF
        ENDDO
        ENDDO
!Clu -----------------------------------------------------------------------
!
        IF(LQCBGS .and. irtacn .eq. 0) THEN
          CALL QCSLI(SLIANL,SLIFCS,LEN,me)
          CALL ALBOCN(ALBFCS,SLMASK,ALBOMX,LEN)
         do i=1,len
            icefl2(i) = sicfcs(i) .gt. 0.99999
          enddo
          KQCM=1
          CALL QCMXMN('Snof    ',SNOFCS,SLIFCS,SNOFCS,icefl1,
     &                SNOLMX,SNOLMN,SNOOMX,SNOOMN,SNOIMX,SNOIMN,
     &                SNOJMX,SNOJMN,SNOSMX,SNOSMN,EPSSNO,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('TSFf    ',TSFFCS,SLIFCS,SNOFCS,icefl2,
     &                TSFLMX,TSFLMN,TSFOMX,TSFOMN,TSFIMX,TSFIMN,
     &                TSFJMX,TSFJMN,TSFSMX,TSFSMN,EPSTSF,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          do kk = 1, 4
          CALL QCMXMN('ALBf    ',ALBFCS(1,kk),SLIFCS,SNOFCS,icefl1,
     &                ALBLMX,ALBLMN,ALBOMX,ALBOMN,ALBIMX,ALBIMN,
     &                ALBJMX,ALBJMN,ALBSMX,ALBSMN,EPSALB,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          enddo
        IF(FNWETC(1:8).NE.'        ' .OR. FNWETA(1:8).NE.'        ' )
     &                                                          THEN
          CALL QCMXMN('WETf    ',WETFCS,SLIFCS,SNOFCS,icefl1,
     &                WETLMX,WETLMN,WETOMX,WETOMN,WETIMX,WETIMN,
     &                WETJMX,WETJMN,WETSMX,WETSMN,EPSWET,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
        ENDIF
          CALL QCMXMN('ZORf    ',ZORFCS,SLIFCS,SNOFCS,icefl1,
     &                ZORLMX,ZORLMN,ZOROMX,ZOROMN,ZORIMX,ZORIMN,
     &                ZORJMX,ZORJMN,ZORSMX,ZORSMN,EPSZOR,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!       IF(FNPLRC(1:8).NE.'        ' .OR. FNPLRA(1:8).NE.'        ' )
!         CALL QCMXMN('PLNf    ',PLRFCS,SLIFCS,SNOFCS,icefl1,
!    &                PLRLMX,PLRLMN,PLROMX,PLROMN,PLRIMX,PLRIMN,
!    &                PLRJMX,PLRJMN,PLRSMX,PLRSMN,EPSPLR,
!    &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!       ENDIF
          CALL QCMXMN('TG3f    ',TG3FCS,SLIFCS,SNOFCS,icefl1,
     &                TG3LMX,TG3LMN,TG3OMX,TG3OMN,TG3IMX,TG3IMN,
     &                TG3JMX,TG3JMN,TG3SMX,TG3SMN,EPSTG3,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Cwu [+8L] ---------------------------------------------------------------
          CALL QCMXMN('SIHf    ',SIHFCS,SLIFCS,SNOFCS,icefl1,
     &                SIHLMX,SIHLMN,SIHOMX,SIHOMN,SIHIMX,SIHIMN,
     &                SIHJMX,SIHJMN,SIHSMX,SIHSMN,EPSSIH,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('SICf    ',SICFCS,SLIFCS,SNOFCS,icefl1,
     &                SICLMX,SICLMN,SICOMX,SICOMN,SICIMX,SICIMN,
     &                SICJMX,SICJMN,SICSMX,SICSMN,EPSSIC,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('SMC1f    ',SMCFCS(1,1),SLIFCS,SNOFCS,icefl1,
     &                SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &                SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('SMC2f   ',SMCFCS(1,2),SLIFCS,SNOFCS,icefl1,
     &                SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &                SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu [+8L] add smcfcs(3:4)
          IF(LSOIL.GT.2) THEN
          CALL QCMXMN('SMC3f    ',SMCFCS(1,3),SLIFCS,SNOFCS,icefl1,
     &                SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &                SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('SMC4f   ',SMCFCS(1,4),SLIFCS,SNOFCS,icefl1,
     &                SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &                SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          ENDIF
          CALL QCMXMN('STC1f   ',STCFCS(1,1),SLIFCS,SNOFCS,icefl1,
     &                STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &                STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('STC2f   ',STCFCS(1,2),SLIFCS,SNOFCS,icefl1,
     &                STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &                STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu [+8L] add stcfcs(3:4)
         IF(LSOIL.GT.2) THEN
          CALL QCMXMN('STC3f   ',STCFCS(1,3),SLIFCS,SNOFCS,icefl1,
     &                STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &                STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('STC4f   ',STCFCS(1,4),SLIFCS,SNOFCS,icefl1,
     &                STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &                STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
         ENDIF
          CALL QCMXMN('VEGf    ',VEGFCS,SLIFCS,SNOFCS,icefl1,
     &                VEGLMX,VEGLMN,VEGOMX,VEGOMN,VEGIMX,VEGIMN,
     &                VEGJMX,VEGJMN,VEGSMX,VEGSMN,EPSVEG,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('VETf    ',VETFCS,SLIFCS,SNOFCS,icefl1,
     &                VETLMX,VETLMN,VETOMX,VETOMN,VETIMX,VETIMN,
     &                VETJMX,VETJMN,VETSMX,VETSMN,EPSVET,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('SOTf    ',SOTFCS,SLIFCS,SNOFCS,icefl1,
     &                SOTLMX,SOTLMN,SOTOMX,SOTOMN,SOTIMX,SOTIMN,
     &                SOTJMX,SOTJMN,SOTSMX,SOTSMN,EPSSOT,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)

!Clu [+16L] ---------------------------------------------------------------
          CALL QCMXMN('VMNf    ',VMNFCS,SLIFCS,SNOFCS,icefl1,
     &                VMNLMX,VMNLMN,VMNOMX,VMNOMN,VMNIMX,VMNIMN,
     &                VMNJMX,VMNJMN,VMNSMX,VMNSMN,EPSVMN,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('VMXf    ',VMXFCS,SLIFCS,SNOFCS,icefl1,
     &                VMXLMX,VMXLMN,VMXOMX,VMXOMN,VMXIMX,VMXIMN,
     &                VMXJMX,VMXJMN,VMXSMX,VMXSMN,EPSVMX,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('SLPf    ',SLPFCS,SLIFCS,SNOFCS,icefl1,
     &                SLPLMX,SLPLMN,SLPOMX,SLPOMN,SLPIMX,SLPIMN,
     &                SLPJMX,SLPJMN,SLPSMX,SLPSMN,EPSSLP,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
          CALL QCMXMN('ABSf    ',ABSFCS,SLIFCS,SNOFCS,icefl1,
     &                ABSLMX,ABSLMN,ABSOMX,ABSOMN,ABSIMX,ABSIMN,
     &                ABSJMX,ABSJMN,ABSSMX,ABSSMN,EPSABS,
     &                RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu -----------------------------------------------------------------------
        ENDIF
      ENDIF
!
      IF (MONFCS) THEN
       if (me .eq. 0) then
        PRINT *,' '
        PRINT *,'MONITOR OF GUESS'
        PRINT *,' '
!       CALL COUNT(SLIFCS,SNOFCS,LEN)
        PRINT *,' '
!        CALL MONITR('TSFFCS',TSFFCS,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('ALBFCS',ALBFCS,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('AISFCS',AISFCS,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('SNOFCS',SNOFCS,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('SMCFCS1',SMCFCS(1,1),SLIFCS,SNOFCS,LEN)
!        CALL MONITR('SMCFCS2',SMCFCS(1,2),SLIFCS,SNOFCS,LEN)
!        CALL MONITR('STCFCS1',STCFCS(1,1),SLIFCS,SNOFCS,LEN)
!        CALL MONITR('STCFCS2',STCFCS(1,2),SLIFCS,SNOFCS,LEN)
!Clu [+4L] add smcfcs(3:4) and stcfcs(3:4)
        IF(LSOIL.GT.2) THEN
!        CALL MONITR('SMCFCS3',SMCFCS(1,3),SLIFCS,SNOFCS,LEN)
!        CALL MONITR('SMCFCS4',SMCFCS(1,4),SLIFCS,SNOFCS,LEN)
!        CALL MONITR('STCFCS3',STCFCS(1,3),SLIFCS,SNOFCS,LEN)
!        CALL MONITR('STCFCS4',STCFCS(1,4),SLIFCS,SNOFCS,LEN)
        ENDIF
!        CALL MONITR('TG3FCS',TG3FCS,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('ZORFCS',ZORFCS,SLIFCS,SNOFCS,LEN)
!       IF (GAUS) THEN
!          CALL MONITR('CVAFCS',CVFCS ,SLIFCS,SNOFCS,LEN)
!          CALL MONITR('CVBFCS',CVBFCS,SLIFCS,SNOFCS,LEN)
!          CALL MONITR('CVTFCS',CVTFCS,SLIFCS,SNOFCS,LEN)
!       ENDIF
!        CALL MONITR('SLIFCS',SLIFCS,SLIFCS,SNOFCS,LEN)
!       CALL MONITR('PLRFCS',PLRFCS,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('OROG  ',OROG  ,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('VEGFCS',VEGFCS,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('VETFCS',VETFCS,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('SOTFCS',SOTFCS,SLIFCS,SNOFCS,LEN)
!Cwu [+2L] add sih, sic
!        CALL MONITR('SIHFCS',SIHFCS,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('SICFCS',SICFCS,SLIFCS,SNOFCS,LEN)
!Clu [+4L] add vmn, vmx, slp, abs
!        CALL MONITR('VMNFCS',VMNFCS,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('VMXFCS',VMXFCS,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('SLPFCS',SLPFCS,SLIFCS,SNOFCS,LEN)
!        CALL MONITR('ABSFCS',ABSFCS,SLIFCS,SNOFCS,LEN)
       endif
      ENDIF
!
!...   update annual cycle in the sst guess..
!
!     if(lprnt) print *,'tsfclm=',tsfclm(iprnt),' tsfcl2=',tsfcl2(iprnt)
!    *,' tsffcs=',tsffcs(iprnt),' slianl=',slianl(iprnt)
      DO I=1,LEN
        IF(SLIANL(I) .EQ. 0.0) THEN
          TSFFCS(I)=TSFFCS(I) + (TSFCLM(I) - TSFCL2(I))
        ENDIF
      ENDDO
!
!  Quality control analysis using forecast guess
!
      CALL QCBYFC(TSFFCS,SNOFCS,QCTSFS,QCSNOS,QCTSFI,LEN,LSOIL,
     &            SNOANL,AISANL,SLIANL,TSFANL,ALBANL,
     &            ZORANL,SMCANL,
     &            SMCCLM,TSFSMX,ALBOMX,ZOROMX,me)
!
!  BLEND CLIMATOLOGY AND PREDICTED FIELDS
!
      if(me .eq. 0) then
        WRITE(6,*) '=============='
        WRITE(6,*) '   MERGING'
        WRITE(6,*) '=============='
      endif
!     if(lprnt) print *,' tsffcs=',tsffcs(iprnt)
!
      PERCRIT=CRITP3
!
!  Merge analysis and forecast.  Note TG3, AIS are not merged
!

      CALL MERGE(LEN,LSOIL,IY,IM,ID,IH,FH,
!Cwu [+1L] add ()fcs for sih, sic
     &           SIHFCS,SICFCS,
!Clu [+1L] add ()fcs for vmn, vmx, slp, abs
     &           VMNFCS,VMXFCS,SLPFCS,ABSFCS, 
     &           TSFFCS,WETFCS,SNOFCS,ZORFCS,ALBFCS,AISFCS,
     &           CVFCS ,CVBFCS,CVTFCS,
     &           CNPFCS,SMCFCS,STCFCS,SLIFCS,VEGFCS,
     &           vetfcs,sotfcs,alffcs,
!Cwu [+1L] add ()anl for sih, sic
     &           SIHANL,SICANL,                
!Clu [+1L] add ()anl for vmn, vmx, slp, abs
     &           VMNANL,VMXANL,SLPANL,ABSANL,       
     &           TSFANL,TSFAN2,WETANL,SNOANL,ZORANL,ALBANL,AISANL,
     &           CVANL ,CVBANL,CVTANL,
     &           CNPANL,SMCANL,STCANL,SLIANL,VEGANL,
     &           vetanl,sotanl,ALFANL,
     &           CTSFL,CALBL,CAISL,CSNOL,CSMCL,CZORL,CSTCL,CVEGL,
     &           CTSFS,CALBS,CAISS,CSNOS,CSMCS,CZORS,CSTCS,CVEGS,
     &           CCV,CCVB,CCVT,CCNP,cvetl,cvets,csotl,csots,
     &           calfl,calfs,
!Cwu [+1L] add c()l, c()s  for sih, sic
     &           CSIHL,CSIHS,CSICL,CSICS,
!Clu [+1L] add c()l, c()s  for vmn, vmx, slp, abs
     &           CVMNL,CVMNS,CVMXL,CVMXS,CSLPL,CSLPS,CABSL,CABSS, 
     &           IRTTSF,IRTWET,IRTSNO,IRTZOR,IRTALB,IRTAIS,
     &           IRTTG3,IRTSCV,IRTACN,IRTSMC,IRTSTC,IRTVEG,
!Clu [+1L] add irt() for vmn, vmx, slp, abs
     &           IRTVMN,IRTVMX,IRTSLP,IRTABS,        
!cggg landice start
!cggg     &           irtvet,irtsot,irtalf,me)
     &           irtvet,irtsot,irtalf,landice,me)
!cggg landice end
      CALL SETZRO(SNOANL,EPSSNO,LEN)
!     if(lprnt) print *,' tanlm=',tsfanl(iprnt),' tfcsm=',tsffcs(iprnt)
!     if(lprnt) print *,' sliam=',slianl(iprnt),' slifm=',slifcs(iprnt)

!
!  New ice/Melted ice
!
      CALL NEWICE(SLIANL,SLIFCS,TSFANL,TSFFCS,LEN,LSOIL,
!Cwu [+1L] add SIHNEW, AISLIM, SIHANL & SICANL
     &            SIHNEW,AISLIM,SIHANL,SICANL,      
     &            ALBANL,SNOANL,ZORANL,SMCANL,STCANL,
     &            ALBOMX,SNOOMX,ZOROMX,SMCOMX,SMCIMX,
!Cwu [-1L/+1L] change ALBIMX to ALBIMN - NOTE ALBIMX & ALBIMN have been modified
!    &            TSFOMN,TSFIMX,ALBIMX,ZORIMX,TGICE,
     &            TSFOMN,TSFIMX,ALBIMN,ZORIMX,TGICE,
     &            RLA,RLO,me)

!     if(lprnt) print *,'tsfanl=',tsfanl(iprnt),' tsffcs=',tsffcs(iprnt)
!     if(lprnt) print *,' slian=',slianl(iprnt),' slifn=',slifcs(iprnt)
!
!  Set tsfc to TSNOW over snow
!
      CALL SNOSFC(SNOANL,TSFANL,TSFSMX,LEN,me)
!
      do i=1,len
        icefl2(i) = sicanl(i) .gt. 0.99999
      enddo
      KQCM=0
      CALL QCMXMN('SnowM   ',SNOANL,SLIANL,SNOANL,icefl1,
     &            SNOLMX,SNOLMN,SNOOMX,SNOOMN,SNOIMX,SNOIMN,
     &            SNOJMX,SNOJMN,SNOSMX,SNOSMN,EPSSNO,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('TsfM    ',TSFANL,SLIANL,SNOANL,icefl2,
     &            TSFLMX,TSFLMN,TSFOMX,TSFOMN,TSFIMX,TSFIMN,
     &            TSFJMX,TSFJMN,TSFSMX,TSFSMN,EPSTSF,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      do kk = 1, 4
      CALL QCMXMN('AlbM    ',ALBANL(1,kk),SLIANL,SNOANL,icefl1,
     &            ALBLMX,ALBLMN,ALBOMX,ALBOMN,ALBIMX,ALBIMN,
     &            ALBJMX,ALBJMN,ALBSMX,ALBSMN,EPSALB,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      enddo
      IF(FNWETC(1:8).NE.'        ' .OR. FNWETA(1:8).NE.'        ' )
     &                                                 THEN
      CALL QCMXMN('WetM    ',WETANL,SLIANL,SNOANL,icefl1,
     &            WETLMX,WETLMN,WETOMX,WETOMN,WETIMX,WETIMN,
     &            WETJMX,WETJMN,WETSMX,WETSMN,EPSWET,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      ENDIF
      CALL QCMXMN('ZorM    ',ZORANL,SLIANL,SNOANL,icefl1,
     &            ZORLMX,ZORLMN,ZOROMX,ZOROMN,ZORIMX,ZORIMN,
     &            ZORJMX,ZORJMN,ZORSMX,ZORSMN,EPSZOR,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!     IF(FNPLRC(1:8).NE.'        ' .OR. FNPLRA(1:8).NE.'        ' )
!    &                                                 THEN
!     CALL QCMXMN('PlntM   ',PLRANL,SLIANL,SNOANL,icefl1,
!    &            PLRLMX,PLRLMN,PLROMX,PLROMN,PLRIMX,PLRIMN,
!    &            PLRJMX,PLRJMN,PLRSMX,PLRSMN,EPSPLR,
!    &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!     ENDIF
      CALL QCMXMN('Stc1M   ',STCANL(1,1),SLIANL,SNOANL,icefl1,
     &            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('Stc2M   ',STCANL(1,2),SLIANL,SNOANL,icefl1,
     &            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu [+8L] add stcanl(3:4)
       IF(LSOIL.GT.2) THEN
      CALL QCMXMN('Stc3M   ',STCANL(1,3),SLIANL,SNOANL,icefl1,
     &            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('Stc4M   ',STCANL(1,4),SLIANL,SNOANL,icefl1,
     &            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     &            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
       ENDIF
      CALL QCMXMN('Smc1M   ',SMCANL(1,1),SLIANL,SNOANL,icefl1,
     &            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('Smc2M   ',SMCANL(1,2),SLIANL,SNOANL,icefl1,
     &            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu [+8L] add smcanl(3:4)
       IF(LSOIL.GT.2) THEN
      CALL QCMXMN('Smc3M   ',SMCANL(1,3),SLIANL,SNOANL,icefl1,
     &            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('Smc4M   ',SMCANL(1,4),SLIANL,SNOANL,icefl1,
     &            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     &            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      ENDIF
      KQCM=1
      CALL QCMXMN('VEGm    ',VEGANL,SLIANL,SNOANL,icefl1,
     &            VEGLMX,VEGLMN,VEGOMX,VEGOMN,VEGIMX,VEGIMN,
     &            VEGJMX,VEGJMN,VEGSMX,VEGSMN,EPSVEG,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('VETm    ',VETANL,SLIANL,SNOANL,icefl1,
     &            VETLMX,VETLMN,VETOMX,VETOMN,VETIMX,VETIMN,
     &            VETJMX,VETJMN,VETSMX,VETSMN,EPSVET,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SOTm    ',SOTANL,SLIANL,SNOANL,icefl1,
     &            SOTLMX,SOTLMN,SOTOMX,SOTOMN,SOTIMX,SOTIMN,
     &            SOTJMX,SOTJMN,SOTSMX,SOTSMN,EPSSOT,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Cwu [+8L] add sih, sic,
      CALL QCMXMN('SIHm    ',SIHANL,SLIANL,SNOANL,icefl1,
     &            SIHLMX,SIHLMN,SIHOMX,SIHOMN,SIHIMX,SIHIMN,
     &            SIHJMX,SIHJMN,SIHSMX,SIHSMN,EPSSIH,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SICm    ',SICANL,SLIANL,SNOANL,icefl1,
     &            SICLMX,SICLMN,SICOMX,SICOMN,SICIMX,SICIMN,
     &            SICJMX,SICJMN,SICSMX,SICSMN,EPSSIC,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
!Clu [+16L] add vmn, vmx, slp, abs
      CALL QCMXMN('VMNm    ',VMNANL,SLIANL,SNOANL,icefl1,
     &            VMNLMX,VMNLMN,VMNOMX,VMNOMN,VMNIMX,VMNIMN,
     &            VMNJMX,VMNJMN,VMNSMX,VMNSMN,EPSVMN,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('VMXm    ',VMXANL,SLIANL,SNOANL,icefl1,
     &            VMXLMX,VMXLMN,VMXOMX,VMXOMN,VMXIMX,VMXIMN,
     &            VMXJMX,VMXJMN,VMXSMX,VMXSMN,EPSVMX,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('SLPm    ',SLPANL,SLIANL,SNOANL,icefl1,
     &            SLPLMX,SLPLMN,SLPOMX,SLPOMN,SLPIMX,SLPIMN,
     &            SLPJMX,SLPJMN,SLPSMX,SLPSMN,EPSSLP,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)
      CALL QCMXMN('ABSm    ',ABSANL,SLIANL,SNOANL,icefl1,
     &            ABSLMX,ABSLMN,ABSOMX,ABSOMN,ABSIMX,ABSIMN,
     &            ABSJMX,ABSJMN,ABSSMX,ABSSMN,EPSABS,
     &            RLA,RLO,LEN,KQCM,PERCRIT,LGCHEK,me)

!
      if(me .eq. 0) then
        WRITE(6,*) '=============='
        WRITE(6,*) 'FINAL RESULTS'
        WRITE(6,*) '=============='
      endif
!
!  Foreward correction to TG3 and TSF at the last stage
!
!     if(lprnt) print *,' tsfbc=',tsfanl(iprnt)
      if (use_ufo) then
        ZTSFC = 1.
        CALL TSFCOR(TG3ANL,OROGd,SLMASK,ZTSFC,LEN,RLAPSE)
        ZTSFC = 0.
        CALL TSFCOR(TSFANL,OROGd,SLMASK,ZTSFC,LEN,RLAPSE)
      else
        ZTSFC = 0.
        CALL TSFCOR(TSFANL,OROG,SLMASK,ZTSFC,LEN,RLAPSE)
      endif
!     if(lprnt) print *,' tsfaf=',tsfanl(iprnt)
!
!  CHECK THE FINAL MERGED PRODUCT
!
      IF (MONMER) THEN
       if(me .eq. 0) then
        PRINT *,' '
        PRINT *,'MONITOR OF UPDATED SURFACE FIELDS'
        PRINT *,'   (Includes angulation correction)'
        PRINT *,' '
!       CALL COUNT(SLIANL,SNOANL,LEN)
        PRINT *,' '
!        CALL MONITR('TSFANL',TSFANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('ALBANL',ALBANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('AISANL',AISANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('SNOANL',SNOANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('SMCANL1',SMCANL(1,1),SLIANL,SNOANL,LEN)
!        CALL MONITR('SMCANL2',SMCANL(1,2),SLIANL,SNOANL,LEN)
!        CALL MONITR('STCANL1',STCANL(1,1),SLIANL,SNOANL,LEN)
!        CALL MONITR('STCANL2',STCANL(1,2),SLIANL,SNOANL,LEN)
!Clu [+4L] add smcanl(3:4) and stcanl(3:4)
        IF(LSOIL.GT.2) THEN
!        CALL MONITR('SMCANL3',SMCANL(1,3),SLIANL,SNOANL,LEN)
!        CALL MONITR('SMCANL4',SMCANL(1,4),SLIANL,SNOANL,LEN)
!        CALL MONITR('STCANL3',STCANL(1,3),SLIANL,SNOANL,LEN)
!        CALL MONITR('STCANL4',STCANL(1,4),SLIANL,SNOANL,LEN)
!        CALL MONITR('TG3ANL',TG3ANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('ZORANL',ZORANL,SLIANL,SNOANL,LEN)
        ENDIF
!       IF (GAUS) THEN
!          CALL MONITR('CVAANL',CVANL ,SLIANL,SNOANL,LEN)
!          CALL MONITR('CVBANL',CVBANL,SLIANL,SNOANL,LEN)
!          CALL MONITR('CVTANL',CVTANL,SLIANL,SNOANL,LEN)
!       ENDIF
!        CALL MONITR('SLIANL',SLIANL,SLIANL,SNOANL,LEN)
!       CALL MONITR('PLRANL',PLRANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('OROG  ',OROG  ,SLIANL,SNOANL,LEN)
!        CALL MONITR('CNPANL',CNPANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('VEGANL',VEGANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('VETANL',VETANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('SOTANL',SOTANL,SLIANL,SNOANL,LEN)
!Cwu [+2L] add sih, sic,
!        CALL MONITR('SIHANL',SIHANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('SICANL',SICANL,SLIANL,SNOANL,LEN)
!Clu [+4L] add vmn, vmx, slp, abs
!        CALL MONITR('VMNANL',VMNANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('VMXANL',VMXANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('SLPANL',SLPANL,SLIANL,SNOANL,LEN)
!        CALL MONITR('ABSANL',ABSANL,SLIANL,SNOANL,LEN)
       endif
      ENDIF
!
      IF (MONDIF) THEN
        DO I=1,LEN
          TSFFCS(I) = TSFANL(I) - TSFFCS(I)
          SNOFCS(I) = SNOANL(I) - SNOFCS(I)
          TG3FCS(I) = TG3ANL(I) - TG3FCS(I)
          ZORFCS(I) = ZORANL(I) - ZORFCS(I)
!         PLRFCS(I) = PLRANL(I) - PLRFCS(I)
!         ALBFCS(I) = ALBANL(I) - ALBFCS(I)
          SLIFCS(I) = SLIANL(I) - SLIFCS(I)
          AISFCS(I) = AISANL(I) - AISFCS(I)
          CNPFCS(I) = CNPANL(I) - CNPFCS(I)
          VEGFCS(I) = VEGANL(I) - VEGFCS(I)
          VETFCS(I) = VETANL(I) - VETFCS(I)
          SOTFCS(I) = SOTANL(I) - SOTFCS(I)
!Clu [+2L] add sih, sic
          SIHFCS(I) = SIHANL(I) - SIHFCS(I)
          SICFCS(I) = SICANL(I) - SICFCS(I)
!Clu [+4L] add vmn, vmx, slp, abs
          VMNFCS(I) = VMNANL(I) - VMNFCS(I)
          VMXFCS(I) = VMXANL(I) - VMXFCS(I)
          SLPFCS(I) = SLPANL(I) - SLPFCS(I)
          ABSFCS(I) = ABSANL(I) - ABSFCS(I)
        ENDDO
        DO J = 1,LSOIL
          DO I = 1,LEN
            SMCFCS(I,J) = SMCANL(I,J) - SMCFCS(I,J)
            STCFCS(I,J) = STCANL(I,J) - STCFCS(I,J)
          ENDDO
        ENDDO
        DO J = 1,4
          DO I = 1,LEN
            ALBFCS(I,J) = ALBANL(I,J) - ALBFCS(I,J)
          ENDDO
        ENDDO
!
!  MONITORING PRINTS
!
       if(me .eq. 0) then
        PRINT *,' '
        PRINT *,'MONITOR OF DIFFERENCE'
        PRINT *,'   (Includes angulation correction)'
        PRINT *,' '
!        CALL MONITR('TSFDIF',TSFFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('ALBDIF',ALBFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('ALBDIF1',ALBFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('ALBDIF2',ALBFCS(1,2),SLIANL,SNOANL,LEN)
!        CALL MONITR('ALBDIF3',ALBFCS(1,3),SLIANL,SNOANL,LEN)
!        CALL MONITR('ALBDIF4',ALBFCS(1,4),SLIANL,SNOANL,LEN)
!        CALL MONITR('AISDIF',AISFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('SNODIF',SNOFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('SMCANL1',SMCFCS(1,1),SLIANL,SNOANL,LEN)
!        CALL MONITR('SMCANL2',SMCFCS(1,2),SLIANL,SNOANL,LEN)
!        CALL MONITR('STCANL1',STCFCS(1,1),SLIANL,SNOANL,LEN)
!        CALL MONITR('STCANL2',STCFCS(1,2),SLIANL,SNOANL,LEN)
!Clu [+4L] add smcfcs(3:4) and stc(3:4)
        IF(LSOIL.GT.2) THEN
!        CALL MONITR('SMCANL3',SMCFCS(1,3),SLIANL,SNOANL,LEN)
!        CALL MONITR('SMCANL4',SMCFCS(1,4),SLIANL,SNOANL,LEN)
!        CALL MONITR('STCANL3',STCFCS(1,3),SLIANL,SNOANL,LEN)
!        CALL MONITR('STCANL4',STCFCS(1,4),SLIANL,SNOANL,LEN)
        ENDIF
!        CALL MONITR('TG3DIF',TG3FCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('ZORDIF',ZORFCS,SLIANL,SNOANL,LEN)
!       IF (GAUS) THEN
!          CALL MONITR('CVADIF',CVFCS ,SLIANL,SNOANL,LEN)
!          CALL MONITR('CVBDIF',CVBFCS,SLIANL,SNOANL,LEN)
!          CALL MONITR('CVTDIF',CVTFCS,SLIANL,SNOANL,LEN)
!       ENDIF
!        CALL MONITR('SLIDIF',SLIFCS,SLIANL,SNOANL,LEN)
!       CALL MONITR('PLRDIF',PLRFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('CNPDIF',CNPFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('VEGDIF',VEGFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('VETDIF',VETFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('SOTDIF',SOTFCS,SLIANL,SNOANL,LEN)
!Cwu [+2L] add sih, sic
!        CALL MONITR('SIHDIF',SIHFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('SICDIF',SICFCS,SLIANL,SNOANL,LEN)
!Clu [+4L] add vmn, vmx, slp, abs
!        CALL MONITR('VMNDIF',VMNFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('VMXDIF',VMXFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('SLPDIF',SLPFCS,SLIANL,SNOANL,LEN)
!        CALL MONITR('ABSDIF',ABSFCS,SLIANL,SNOANL,LEN)
       endif
      ENDIF
!
!
      DO I=1,LEN
        TSFFCS(I) = TSFANL(I)
        SNOFCS(I) = SNOANL(I)
        TG3FCS(I) = TG3ANL(I)
        ZORFCS(I) = ZORANL(I)
!       PLRFCS(I) = PLRANL(I)
!       ALBFCS(I) = ALBANL(I)
        SLIFCS(I) = SLIANL(I)
        AISFCS(I) = AISANL(I)
        CVFCS(I)  = CVANL(I)
        CVBFCS(I) = CVBANL(I)
        CVTFCS(I) = CVTANL(I)
        CNPFCS(I) = CNPANL(I)
        vegFCS(I) = vegANL(I)
        vetFCS(I) = vetANL(I)
        sotFCS(I) = sotANL(I)
!Clu [+4L] add vmn, vmx, slp, abs
        VMNFCS(I) = VMNANL(I)
        VMXFCS(I) = VMXANL(I)
        SLPFCS(I) = SLPANL(I)
        ABSFCS(I) = ABSANL(I)
      ENDDO
      DO J = 1,LSOIL
        DO I = 1,LEN
          SMCFCS(I,J) = SMCANL(I,J)
          IF (SLIFCS(I) .GT. 0.0) THEN
             STCFCS(I,J) = STCANL(I,J)
          ELSE
             STCFCS(I,J) = TSFFCS(I)
          ENDIF
        ENDDO
      ENDDO
      DO J = 1,4
        DO I = 1,LEN
          ALBFCS(I,J) = ALBANL(I,J)
        ENDDO
      ENDDO
      DO J = 1,2
        DO I = 1,LEN
          ALFFCS(I,J) = ALFANL(I,J)
        ENDDO
      ENDDO

!Cwu [+20L] update SIHFCS, SICFCS. Remove sea ice over non-ice points
      CRIT=AISLIM
      DO I=1,LEN
        SIHFCS(I) = SIHANL(I)
        SITFCS(I) = TSFFCS(I)
        IF (SLIFCS(I).GE.2.) THEN
          IF (SICFCS(I).GT.CRIT) THEN
            TSFFCS(I) = (SICANL(I)*TSFFCS(I)
     &                + (SICFCS(I)-SICANL(I))*TGICE)/SICFCS(I)
            SITFCS(I) = (TSFFCS(I)-TGICE*(1.0-SICFCS(I))) / SICFCS(I)
          ELSE
            TSFFCS(I) = Tsfanl(i)
!           TSFFCS(I) = TGICE
            SIHFCS(I) = SIHNEW
          ENDIF
        ENDIF
        SICFCS(I) = SICANL(I)
      ENDDO
      DO I=1,LEN
        IF (SLIFCS(I).LT.1.5) THEN
          SIHFCS(I) = 0.
          SICFCS(I) = 0.
          SITFCS(I) = TSFFCS(I)
        ELSE IF ((SLIFCS(I).GE.1.5).AND.(SICFCS(I).LT.CRIT)) THEN
          PRINT *,'WARNING: CHECK, SLIFCS and SICFCS',
     &            SLIFCS(I),SICFCS(I)
        ENDIF
      ENDDO

!Clu [+44L]--------------------------------------------------------------------
!
! ensure the consistency between slc and smc
!
       DO K=1, LSOIL
        FIXRATIO(K) = .False.
        IF (FSMCL(K).LT.99999.) FIXRATIO(K) = .True.
       ENDDO

       if(me .eq. 0) then
       print *,'DBGX --fixratio:',(FIXRATIO(K),K=1,LSOIL)
       endif

       DO K=1, LSOIL
        IF(FIXRATIO(K)) THEN
         DO I = 1, LEN
           IF(SWRATIO(I,K) .EQ. -999.) THEN
            SLCFCS(I,K) = SMCFCS(I,K)
           ELSE
            SLCFCS(I,K) = SWRATIO(I,K) * SMCFCS(I,K)
           ENDIF
!cggg
           if (slifcs(i) .ne. 1.0) slcfcs(i,k) = 1.0  ! flag value for non-land points.
         ENDDO
        ENDIF
       ENDDO
!cggg landice start
!cggg set liquid soil moisture to a flag value of 1.0
       IF (LANDICE) THEN
         DO I = 1, LEN
           IF (SLIFCS(I) .EQ. 1.0 .AND. VETFCS(I) == 13.0) THEN
             DO K=1, LSOIL
               SLCFCS(I,K) = 1.0
             ENDDO
           ENDIF
         ENDDO
       END IF
!cggg landice end
!
! ensure the consistency between snwdph and sheleg
!
      IF(FSNOL .LT. 99999.) THEN  
       if(me .eq. 0) then
       print *,'DBGX -- scale snwdph from sheleg'
       endif
       DO I = 1, LEN
        IF(SLIFCS(I).EQ.1.) SWDFCS(I) = 10.* SNOFCS(I)
       ENDDO
      ENDIF

! sea ice model only uses the liquid equivalent depth.
! so update the physical depth only for display purposes.
! use the same 3:1 ratio used by ice model.

      do i = 1, len
        if (slifcs(i).ne.1) swdfcs(i) = 3.*snofcs(i)
      enddo

      DO I = 1, LEN
        IF(SLIFCS(I).EQ.1.) THEN
        IF(SNOFCS(I).NE.0. .AND. SWDFCS(I).EQ.0.) THEN
          print *,'DBGX --scale snwdph from sheleg',
     +        I, SWDFCS(I), SNOFCS(I)
          SWDFCS(I) = 10.* SNOFCS(I)
        ENDIF
        ENDIF
      ENDDO
!cggg landice mods start  - impose same minimum snow depth at
!cggg                       landice as noah lsm.  also ensure
!cggg                       lower thermal boundary condition
!cggg                       and skin t is no warmer than freezing
!cggg                       after adjustment to terrain.
       IF (LANDICE) THEN
         DO I = 1, LEN
           IF (SLIFCS(I) .EQ. 1.0 .AND. VETFCS(I) == 13.0) THEN
             SNOFCS(I) = MAX(SNOFCS(I),100.0)  ! IN MM
             SWDFCS(I) = MAX(SWDFCS(I),1000.0) ! IN MM
             TG3FCS(I) = MIN(TG3FCS(I),273.15)
             TSFFCS(I) = MIN(TSFFCS(I),273.15)
           ENDIF
         ENDDO
       END IF
!cggg landice mods end
!Clu---------------------------------------------------------------------------
!
!     if(lprnt) print *,' tsffcsF=',tsffcs(iprnt)
      RETURN
      END SUBROUTINE SFCCYCLE 
      SUBROUTINE COUNT(SLIMSK,SNO,IJMAX)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      real (kind=kind_io8) rl3,rl1,rl0,rl2,rl6,rl7,rl4,rl5
      integer l8,l7,l1,l2,ijmax,l0,l3,l5,l6,l4,ij
!
      REAL (KIND=KIND_IO8) SLIMSK(1),SNO(1)
!
!  COUNT NUMBER OF POINTS FOR THE FOUR SURFACE CONDITIONS
!
      L0 = 0
      L1 = 0
      L2 = 0
      L3 = 0
      L4 = 0
      DO IJ=1,IJMAX
        IF(SLIMSK(IJ).EQ.0.) L1 = L1 + 1
        IF(SLIMSK(IJ).EQ.1. .AND. SNO(IJ).LE.0.) L0 = L0 + 1
        IF(SLIMSK(IJ).EQ.2. .AND. SNO(IJ).LE.0.) L2 = L2 + 1
        IF(SLIMSK(IJ).EQ.1. .AND. SNO(IJ).GT.0.) L3 = L3 + 1
        IF(SLIMSK(IJ).EQ.2. .AND. SNO(IJ).GT.0.) L4 = L4 + 1
      ENDDO
      L5  = L0 + L3
      L6  = L2 + L4
      L7  = L1 + L6
      L8  = L1 + L5 + L6
      RL0 = FLOAT(L0) / FLOAT(L8)*100.
      RL3 = FLOAT(L3) / FLOAT(L8)*100.
      RL1 = FLOAT(L1) / FLOAT(L8)*100.
      RL2 = FLOAT(L2) / FLOAT(L8)*100.
      RL4 = FLOAT(L4) / FLOAT(L8)*100.
      RL5 = FLOAT(L5) / FLOAT(L8)*100.
      RL6 = FLOAT(L6) / FLOAT(L8)*100.
      RL7 = FLOAT(L7) / FLOAT(L8)*100.
      PRINT *,'1) NO. OF NOT SNOW-COVERED LAND POINTS   ',L0,' ',RL0,' '
      PRINT *,'2) NO. OF SNOW COVERED LAND POINTS       ',L3,' ',RL3,' '
      PRINT *,'3) NO. OF OPEN SEA POINTS                ',L1,' ',RL1,' '
      PRINT *,'4) NO. OF NOT SNOW-COVERED SEAICE POINTS ',L2,' ',RL2,' '
      PRINT *,'5) NO. OF SNOW COVERED SEA ICE POINTS    ',L4,' ',RL4,' '
      PRINT *,' '
      PRINT *,'6) NO. OF LAND POINTS                    ',L5,' ',RL5,' '
      PRINT *,'7) NO. SEA POINTS (INCLUDING SEA ICE)    ',L7,' ',RL7,' '
      PRINT *,'   (NO. OF SEA ICE POINTS)          (',L6,')',' ',RL6,' '
      PRINT *,' '
      PRINT *,'9) NO. OF TOTAL GRID POINTS               ',L8
!     PRINT *,' '
!     PRINT *,' '

!
!     if(lprnt) print *,' tsffcsF=',tsffcs(iprnt)
      RETURN
      END
      SUBROUTINE MONITR(LFLD,FLD,SLIMSK,SNO,IJMAX)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer ij,n,ijmax
!
      REAL (KIND=KIND_IO8) FLD(IJMAX), SLIMSK(IJMAX),SNO(IJMAX)
!
      REAL (KIND=KIND_IO8) RMAX(5),RMIN(5)
      CHARACTER*8 LFLD
!
!  FIND MAX/MIN
!
      DO N=1,5
        RMAX(N) = -9.E20
        RMIN(N) =  9.E20
      ENDDO
!
      DO IJ=1,IJMAX
         IF(SLIMSK(IJ).EQ.0.) THEN
            RMAX(1) = MAX(RMAX(1), FLD(IJ))
            RMIN(1) = MIN(RMIN(1), FLD(IJ))
         ELSEIF(SLIMSK(IJ).EQ.1.) THEN
            IF(SNO(IJ).LE.0.) THEN
               RMAX(2) = MAX(RMAX(2), FLD(IJ))
               RMIN(2) = MIN(RMIN(2), FLD(IJ))
            ELSE
               RMAX(4) = MAX(RMAX(4), FLD(IJ))
               RMIN(4) = MIN(RMIN(4), FLD(IJ))
            ENDIF
         ELSE
            IF(SNO(IJ).LE.0.) THEN
               RMAX(3) = MAX(RMAX(3), FLD(IJ))
               RMIN(3) = MIN(RMIN(3), FLD(IJ))
            ELSE
               RMAX(5) = MAX(RMAX(5), FLD(IJ))
               RMIN(5) = MIN(RMIN(5), FLD(IJ))
            ENDIF
         ENDIF
      ENDDO
!
      PRINT 100,LFLD
      PRINT 101,RMAX(1),RMIN(1)
      PRINT 102,RMAX(2),RMIN(2), RMAX(4), RMIN(4)
      PRINT 103,RMAX(3),RMIN(3), RMAX(5), RMIN(5)
!
!     PRINT 102,RMAX(2),RMIN(2)
!     PRINT 103,RMAX(3),RMIN(3)
!     PRINT 104,RMAX(4),RMIN(4)
!     PRINT 105,RMAX(5),RMIN(5)
  100 FORMAT('0  *** ',A8,' ***')
  101 FORMAT(' OPEN SEA  ......... MAX=',E12.4,' MIN=',E12.4)
  102 FORMAT(' LAND NOSNOW/SNOW .. MAX=',E12.4,' MIN=',E12.4
     &,                          ' MAX=',E12.4,' MIN=',E12.4)
  103 FORMAT(' SEAICE NOSNOW/SNOW  MAX=',E12.4,' MIN=',E12.4
     &,                          ' MAX=',E12.4,' MIN=',E12.4)
!
! 100 FORMAT('0',2X,'*** ',A8,' ***')
! 102 FORMAT(2X,' LAND WITHOUT SNOW ..... MAX=',E12.4,' MIN=',E12.4)
! 103 FORMAT(2X,' SEAICE WITHOUT SNOW ... MAX=',E12.4,' MIN=',E12.4)
! 104 FORMAT(2X,' LAND WITH SNOW ........ MAX=',E12.4,' MIN=',E12.4)
! 105 FORMAT(2X,' SEA ICE WITH SNOW ..... MAX=',E12.4,' MIN=',E12.4)
!
      RETURN
      END
      SUBROUTINE DAYOYR(IYR,IMO,IDY,LDY)
      implicit none
      integer ldy,i,idy,iyr,imo
!
!  THIS ROUTINE FIGURES OUT THE DAY OF THE YEAR GIVEN IMO AND IDY
!
      INTEGER MONTH(13)
      DATA MONTH/0,31,28,31,30,31,30,31,31,30,31,30,31/
      IF(MOD(IYR,4).EQ.0) MONTH(3) = 29
      LDY = IDY
      DO I = 1, IMO
        LDY = LDY + MONTH(I)
      ENDDO
      RETURN
      END
      SUBROUTINE HMSKRD(LUGB,IMSK,JMSK,FNMSKH,
     &                  KPDS5,SLMSKH,GAUSM,BLNMSK,BLTMSK,me)
      USE MACHINE , ONLY : kind_io8,kind_io4
      use sfccyc_module, only : mdata, xdata, ydata
      implicit none
      integer kpds5,me,i,imsk,jmsk,lugb
!
      CHARACTER*500 FNMSKH
!
      REAL (KIND=KIND_IO8) SLMSKH(mdata)
      LOGICAL GAUSM
      REAL (KIND=KIND_IO8) BLNMSK,BLTMSK
!
      IMSK = xdata
      JMSK = ydata

      if (me .eq. 0) then
      write(6,*)' IMSK=',IMSK,' JMSK=',JMSK,' xdata=',xdata,' ydata='
     &,ydata
      endif
      CALL FIXRDG(LUGB,IMSK,JMSK,FNMSKH,
     &            KPDS5,SLMSKH,GAUSM,BLNMSK,BLTMSK,me)
      DO I=1,IMSK*JMSK
         SLMSKH(I) = NINT(SLMSKH(I))
      ENDDO
!
      RETURN
      END
      SUBROUTINE FIXRDG(LUGB,IDIM,JDIM,FNGRIB,
     &                  KPDS5,GDATA,GAUS,BLNO,BLTO,me)
      USE MACHINE , ONLY : kind_io8,kind_io4
      use sfccyc_module, only : mdata
      implicit none
      integer lgrib,n,lskip,jret,j,ndata,lugi,jdim,idim,lugb,
     &        iret, me,kpds5,kdata,i
!
      CHARACTER*(*) FNGRIB
!
      REAL (KIND=KIND_IO8) GDATA(IDIM*JDIM)
      LOGICAL GAUS
      REAL (KIND=KIND_IO8) BLNO,BLTO
      real(kind=kind_io8) data4(idim*jdim)
!
      LOGICAL*1 LBMS(mdata)
!
      INTEGER KPDS(200),KGDS(200)
      INTEGER JPDS(200),JGDS(200), KPDS0(200)
!
!     if(me .eq. 0) then
!     WRITE(6,*) ' '
!     WRITE(6,*) '************************************************'
!     endif
!
      CLOSE(LUGB)
      call baopenr(lugb,fngrib,iret)
      IF (IRET .NE. 0) THEN
        WRITE(6,*) ' ERROR IN OPENING FILE ',trim(FNGRIB)
        PRINT *,'ERROR IN OPENING FILE ',trim(FNGRIB)
        CALL ABORT
      ENDIF
      if (me .eq. 0) WRITE(6,*) ' FILE ',trim(FNGRIB),
     &              ' opened. Unit=',LUGB
      lugi    = 0
      lskip   = -1
      N       = 0
      JPDS    = -1
      JGDS    = -1
      JPDS(5) = KPDS5
      KPDS    = JPDS
!
      call getgbh(lugb,lugi,lskip,jpds,jgds,lgrib,ndata,
     &            lskip,kpds,kgds,iret)
!
      if(me .eq. 0) then
        WRITE(6,*) ' First grib record.'
        WRITE(6,*) ' KPDS( 1-10)=',(KPDS(J),J= 1,10)
        WRITE(6,*) ' KPDS(11-20)=',(KPDS(J),J=11,20)
        WRITE(6,*) ' KPDS(21-  )=',(KPDS(J),J=21,22)
      endif
!
      KPDS0=JPDS
      KPDS0(4)=-1
      KPDS0(18)=-1
      IF(IRET.NE.0) THEN
        WRITE(6,*) ' Error in GETGBH. IRET: ', iret
        IF (IRET == 99) WRITE(6,*) ' Field not found.'
        CALL ABORT
      ENDIF
!
      jpds = kpds0
      lskip = -1
      kdata=idim*jdim
      call getgb(lugb,lugi,kdata,lskip,jpds,jgds,ndata,lskip,
     &                kpds,kgds,lbms,data4,jret)
!
      if(jret.eq.0) then
        IF(NDATA.EQ.0) THEN
          WRITE(6,*) ' Error in getgb'
          WRITE(6,*) ' KPDS=',KPDS
          WRITE(6,*) ' KGDS=',KGDS
          CALL ABORT
        ENDIF
        IDIM=KGDS(2)
        JDIM=KGDS(3)
        gaus=kgds(1).eq.4
        blno=kgds(5)*1.d-3
        blto=kgds(4)*1.d-3
        gdata(1:idim*jdim)=data4(1:idim*jdim)
        if (me .eq. 0) WRITE(6,*) 'IDIM,JDIM=',IDIM,JDIM
     &,                ' gaus=',gaus,' blno=',blno,' blto=',blto
      ELSE
        if (me .eq. 0) WRITE(6,*) 'IDIM,JDIM=',IDIM,JDIM
     &,                ' gaus=',gaus,' blno=',blno,' blto=',blto
        WRITE(6,*) ' Error in GETGB : JRET=',JRET
        WRITE(6,*) ' KPDS(13)=',KPDS(13),' KPDS(15)=',KPDS(15)
        CALL ABORT
      ENDIF
!
      RETURN
      END
      SUBROUTINE GETAREA(KGDS,DLAT,DLON,RSLAT,RNLAT,WLON,ELON,IJORDR
     &,                  me)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer j,me,kgds11
      real (kind=kind_io8) f0lon,f0lat,elon,dlon,dlat,rslat,wlon,rnlat
!
!  Get area of the grib record
!
      Integer KGDS(22)
      LOGICAL IJORDR
!
      if (me .eq. 0) then
       WRITE(6,*) ' KGDS( 1-12)=',(KGDS(J),J= 1,12)
       WRITE(6,*) ' KGDS(13-22)=',(KGDS(J),J=13,22)
      endif
!
      IF(KGDS(1).EQ.0) THEN                      !  Lat/Lon grid
!
        if (me .eq. 0) WRITE(6,*) 'LAT/LON GRID'
        DLAT   = FLOAT(KGDS(10)) * 0.001
        DLON   = FLOAT(KGDS( 9)) * 0.001
        F0LON  = FLOAT(KGDS(5))  * 0.001
        F0LAT  = FLOAT(KGDS(4))  * 0.001
        KGDS11 = KGDS(11)
        IF(KGDS11.GE.128) THEN
          WLON = F0LON - DLON*(KGDS(2)-1)
          ELON = F0LON
          IF(DLON*KGDS(2).GT.359.99) THEN
            WLON =F0LON - DLON*KGDS(2)
          ENDIF
          DLON   = -DLON
          KGDS11 = KGDS11 - 128
        ELSE
          WLON = F0LON
          ELON = F0LON + DLON*(KGDS(2)-1)
          IF(DLON*KGDS(2).GT.359.99) THEN
            ELON = F0LON + DLON*KGDS(2)
          ENDIF
        ENDIF
        IF(KGDS11.GE.64) THEN
          RNLAT  = F0LAT + DLAT*(KGDS(3)-1)
          RSLAT  = F0LAT
          KGDS11 = KGDS11 - 64
        ELSE
          RNLAT = F0LAT
          RSLAT = F0LAT - DLAT*(KGDS(3)-1)
          DLAT  = -DLAT
        ENDIF
        IF(KGDS11.GE.32) THEN
          IJORDR = .FALSE.
        ELSE
          IJORDR = .TRUE.
        ENDIF

        IF(WLON.GT.180.) WLON = WLON - 360.
        IF(ELON.GT.180.) ELON = ELON - 360.
        WLON  = NINT(WLON*1000.)  * 0.001
        ELON  = NINT(ELON*1000.)  * 0.001
        RSLAT = NINT(RSLAT*1000.) * 0.001
        RNLAT = NINT(RNLAT*1000.) * 0.001
        RETURN
!
      ELSEIF(KGDS(1).EQ.1) THEN                  !  Mercator projection
        WRITE(6,*) 'Mercator GRID'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
!
      ELSEIF(KGDS(1).EQ.2) THEN                  !  Gnomonic projection
        WRITE(6,*) 'Gnomonic GRID'
        WRITE(6,*) 'ERROR!! Gnomonic projection not coded'
        CALL ABORT
!
      ELSEIF(KGDS(1).EQ.3) THEN                  !  Lambert conformal
        WRITE(6,*) 'Lambert conformal'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
      ELSEIF(KGDS(1).EQ.4) THEN                  !  Gaussian grid
!
        if (me .eq. 0) WRITE(6,*) 'Gaussian GRID'
        DLAT   = 99.
        DLON   = FLOAT(KGDS( 9)) / 1000.0
        F0LON  = FLOAT(KGDS(5))  / 1000.0
        F0LAT  = 99.
        KGDS11 = KGDS(11)
        IF(KGDS11.GE.128) THEN
          WLON = F0LON
          ELON = F0LON
          IF(DLON*KGDS(2).GT.359.99) THEN
            WLON = F0LON - DLON*KGDS(2)
          ENDIF
          DLON   = -DLON
          KGDS11 = KGDS11-128
        ELSE
          WLON = F0LON
          ELON = F0LON + DLON*(KGDS(2)-1)
          IF(DLON*KGDS(2).GT.359.99) THEN
            ELON = F0LON + DLON*KGDS(2)
          ENDIF
        ENDIF
        IF(KGDS11.GE.64) THEN
          RNLAT  = 99.
          RSLAT  = 99.
          KGDS11 = KGDS11 - 64
        ELSE
          RNLAT = 99.
          RSLAT = 99.
          DLAT  = -99.
        ENDIF
        IF(KGDS11.GE.32) THEN
          IJORDR = .FALSE.
        ELSE
          IJORDR = .TRUE.
        ENDIF
        RETURN
!
      ELSEIF(KGDS(1).EQ.5) THEN                  !  Polar Strereographic
        WRITE(6,*) 'Polar Stereographic GRID'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
        RETURN
!
      ELSEIF(KGDS(1).EQ.13) THEN                 !  Oblique Lambert conformal
        WRITE(6,*) 'Oblique Lambert conformal GRID'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
!
      ELSEIF(KGDS(1).EQ.50) THEN                 !  Spherical Coefficient
        WRITE(6,*) 'Spherical Coefficient'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
        RETURN
!
      ELSEIF(KGDS(1).EQ.90) THEN                 !  Space view perspective
!                                                  (orthographic grid)
        WRITE(6,*) 'Space view perspective GRID'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
        RETURN
!
      ELSE                                       !  Unknown projection.  Abort.
        WRITE(6,*) 'ERROR!! Unknown map projection'
        WRITE(6,*) 'KGDS(1)=',KGDS(1)
        PRINT *,'ERROR!! Unknown map projection'
        PRINT *,'KGDS(1)=',KGDS(1)
        CALL ABORT
      ENDIF
!
      RETURN
      END
      SUBROUTINE SUBST(DATA,IMAX,JMAX,DLON,DLAT,IJORDR)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,j,ii,jj,jmax,imax,iret
      REAL (KIND=KIND_IO8) dlat,dlon
!
      LOGICAL IJORDR
!
      REAL (KIND=KIND_IO8) DATA(imax,jmax)
      REAL (KIND=KIND_IO8), allocatable ::  WORK(:,:)
!
      IF(.NOT.IJORDR.OR.
     &  (IJORDR.AND.(DLAT.GT.0..OR.DLON.LT.0.))) THEN
        allocate (WORK(imax,jmax))

        IF(.NOT.IJORDR) THEN
          DO J=1,JMAX
            DO I=1,IMAX
              work(i,j) = data(j,i)
            ENDDO
          ENDDO
        ELSE
          DO J=1,JMAX
            DO I=1,IMAX
              work(i,j) = data(i,j)
            ENDDO
          ENDDO
        ENDIF
        if (dlat > 0.0) then
          if (dlon > 0.0) then
            do j=1,jmax
              jj = jmax - j + 1
              do i=1,imax
                data(i,jj) = work(i,j)
              enddo
            enddo
          else
            do i=1,imax
              data(imax-i+1,jj) = work(i,j)
            enddo
          endif
        else
          if (dlon > 0.0) then
            do j=1,jmax
              do i=1,imax
                data(i,j) = work(i,j)
              enddo
            enddo
          else
            do j=1,jmax
              do i=1,imax
                data(imax-i+1,j) = work(i,j)
              enddo
            enddo
          endif
        endif
        deallocate (WORK, stat=iret)
      ENDIF
      RETURN
      END
      SUBROUTINE LA2GA(REGIN,IMXIN,JMXIN,RINLON,RINLAT,RLON,RLAT,INTTYP,
     &                 GAUOUT,LEN,LMASK,RSLMSK,SLMASK
     &,                OUTLAT, OUTLON,me)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      REAL (KIND=KIND_IO8) wei4,wei3,wei2,sum2,sum1,sum3,wei1,sum4,
     &                     wsum,tem,wsumiv,sums,sumn,wi2j2,x,y,wi1j1,
     &                     wi1j2,wi2j1,rlat,rlon,aphi,
     &                     rnume,alamd,denom
      integer jy,ifills,ix,len,inttyp,me,i,j,jmxin,imxin,jq,jx,j1,j2,
     &        ii,i1,i2,KMAMI,it
      integer nx,kxs,kxt
      integer, allocatable, save :: imxnx(:)
      integer, allocatable       :: ifill(:)
!
!  INTERPOLATION FROM LAT/LON OR GAUSSIAN GRID TO OTHER LAT/LON GRID
!
      REAL (KIND=KIND_IO8) OUTLON(LEN),OUTLAT(LEN),GAUOUT(LEN),
     &                     SLMASK(LEN)
      REAL (KIND=KIND_IO8) REGIN (IMXIN,JMXIN),RSLMSK(IMXIN,JMXIN)
!
      REAL (KIND=KIND_IO8)    RINLAT(JMXIN),  RINLON(IMXIN)
      INTEGER IINDX1(LEN),    IINDX2(LEN)
      INTEGER JINDX1(LEN),    JINDX2(LEN)
      REAL (KIND=KIND_IO8)    DDX(LEN),       DDY(LEN),   WRK(LEN)
!
      LOGICAL LMASK
!
      logical first
      integer   NUM_THREADS
      data first /.true./
      save NUM_THREADS, first
!
      integer LEN_THREAD_M, LEN_THREAD, I1_T, I2_T
      integer NUM_PARTHDS
!
      if (first) then
         NUM_THREADS = NUM_PARTHDS()
         first = .false.
         if (.not. allocated(imxnx)) allocate (imxnx(NUM_THREADS))
      endif
!
      if (me == 0) print *,' NUM_THREADS =',NUM_THREADS,' me=',me
!
!     if(me .eq. 0) then
!     PRINT *,'RLON=',RLON,' me=',me
!     PRINT *,'RLAT=',RLAT,' me=',me,' imxin=',imxin,' jmxin=',jmxin
!     endif
!
!     DO J=1,JMXIN
!       IF(RLAT.GT.0.) THEN
!         RINLAT(J) = RLAT - FLOAT(J-1)*DLAIN
!       ELSE
!         RINLAT(J) = RLAT + FLOAT(J-1)*DLAIN
!       ENDIF
!     ENDDO
!
!     if (me .eq. 0) then
!       PRINT *,'RINLAT='
!       PRINT *,(RINLAT(J),J=1,JMXIN)
!       PRINT *,'RINLON='
!       PRINT *,(RINLON(I),I=1,IMXIN)
!
!       PRINT *,'OUTLAT='
!       PRINT *,(OUTLAT(J),J=1,LEN)
!       PRINT *,(OUTLON(J),J=1,LEN)
!     endif
!
!     DO I=1,IMXIN
!       RINLON(I) = RLON + FLOAT(I-1)*DLOIN
!     ENDDO
!
!     PRINT *,'RINLON='
!     PRINT *,(RINLON(I),I=1,IMXIN)
!
      LEN_THREAD_M  = (LEN+NUM_THREADS-1) / NUM_THREADS

      if (INTTYP /=1) allocate (ifill(NUM_THREADS))
!
!$OMP PARALLEL DO PRIVATE(I1_T,I2_T,LEN_THREAD,IT,I,II,I1,I2)
!$OMP+PRIVATE(J,J1,J2,JQ,IX,JY,NX,KXS,KXT,KMAMI)
!$OMP+PRIVATE(ALAMD,DENOM,RNUME,APHI,X,Y,WSUM,WSUMIV,SUM1,SUM2)
!$OMP+PRIVATE(SUM3,SUM4,WI1J1,WI2J1,WI1J2,WI2J2,WEI1,WEI2,WEI3,WEI4)
!$OMP+PRIVATE(SUMN,SUMS)
!$OMP+SHARED(IMXIN,JMXIN,IFILL)
!$OMP+SHARED(OUTLON,OUTLAT,WRK,IINDX1,RINLON,JINDX1,RINLAT,DDX,DDY)
!$OMP+SHARED(RLON,RLAT,REGIN,GAUOUT,IMXNX)
!
      DO IT=1,NUM_THREADS   ! START OF THREADED LOOP ...................
        I1_T       = (IT-1)*LEN_THREAD_M+1
        I2_T       = MIN(I1_T+LEN_THREAD_M-1,LEN)
        LEN_THREAD = I2_T-I1_T+1
!
!       FIND I-INDEX FOR INTERPOLATION
!
        DO I=I1_T, I2_T
          ALAMD = OUTLON(I)
          IF (ALAMD .LT. RLON)   ALAMD = ALAMD + 360.0
          IF (ALAMD .GT. 360.0+RLON) ALAMD = ALAMD - 360.0
          WRK(I)    = ALAMD
          IINDX1(I) = IMXIN
        ENDDO
        DO I=I1_T,I2_T
          DO II=1,IMXIN
            IF(WRK(I) .GE. RINLON(II)) IINDX1(I) = II
          ENDDO
        ENDDO
        DO I=I1_T,I2_T
          I1 = IINDX1(I)
          IF (I1 .LT. 1) I1 = IMXIN
          I2 = I1 + 1
          IF (I2 .GT. IMXIN) I2 = 1
          IINDX1(I) = I1
          IINDX2(I) = I2
          DENOM     = RINLON(I2) - RINLON(I1)
          IF(DENOM.LT.0.) DENOM = DENOM + 360.
          RNUME = WRK(I) - RINLON(I1)
          IF(RNUME.LT.0.) RNUME = RNUME + 360.
          DDX(I) = RNUME / DENOM
        ENDDO
!
!  FIND J-INDEX FOR INTERPLATION
!
        IF(RLAT.GT.0.) THEN
          DO J=I1_T,I2_T
            JINDX1(J)=0
          ENDDO
          DO JX=1,JMXIN
            DO J=I1_T,I2_T
              IF(OUTLAT(J).LE.RINLAT(JX)) JINDX1(J) = JX
            ENDDO
          ENDDO
          DO J=I1_T,I2_T
            JQ = JINDX1(J)
            APHI=OUTLAT(J)
            IF(JQ.GE.1 .AND. JQ .LT. JMXIN) THEN
              J2=JQ+1
              J1=JQ
             DDY(J)=(APHI-RINLAT(J1))/(RINLAT(J2)-RINLAT(J1))
            ELSEIF (JQ .EQ. 0) THEN
              J2=1
              J1=1
              IF(ABS(90.-RINLAT(J1)).GT.0.001) THEN
                DDY(J)=(APHI-RINLAT(J1))/(90.-RINLAT(J1))
              ELSE
                DDY(J)=0.0
              ENDIF
            ELSE
              J2=JMXIN
              J1=JMXIN
              IF(ABS(-90.-RINLAT(J1)).GT.0.001) THEN
                DDY(J)=(APHI-RINLAT(J1))/(-90.-RINLAT(J1))
              ELSE
                DDY(J)=0.0
              ENDIF
            ENDIF
            JINDX1(J)=J1
            JINDX2(J)=J2
          ENDDO
        ELSE
          DO J=I1_T,I2_T
            JINDX1(J) = JMXIN+1
          ENDDO
          DO JX=JMXIN,1,-1
            DO J=I1_T,I2_T
              IF(OUTLAT(J).LE.RINLAT(JX)) JINDX1(J) = JX
            ENDDO
          ENDDO
          DO J=I1_T,I2_T
            JQ = JINDX1(J)
            APHI=OUTLAT(J)
            IF(JQ.GT.1 .AND. JQ .LE. JMXIN) THEN
              J2=JQ
              J1=JQ-1
              DDY(J)=(APHI-RINLAT(J1))/(RINLAT(J2)-RINLAT(J1))
            ELSEIF (JQ .EQ. 1) THEN
              J2=1
              J1=1
              IF(ABS(-90.-RINLAT(J1)).GT.0.001) THEN
                 DDY(J)=(APHI-RINLAT(J1))/(-90.-RINLAT(J1))
              ELSE
                 DDY(J)=0.0
              ENDIF
            ELSE
              J2=JMXIN
              J1=JMXIN
              IF(ABS(90.-RINLAT(J1)).GT.0.001) THEN
                 DDY(J)=(APHI-RINLAT(J1))/(90.-RINLAT(J1))
              ELSE
                 DDY(J)=0.0
              ENDIF
            ENDIF
            JINDX1(J)=J1
            JINDX2(J)=J2
          ENDDO
        ENDIF
!
!     if (me .eq. 0 .and. inttyp .eq. 1) then
!       PRINT *,'LA2GA'
!       PRINT *,'IINDX1'
!       PRINT *,(IINDX1(N),N=1,LEN)
!       PRINT *,'IINDX2'
!       PRINT *,(IINDX2(N),N=1,LEN)
!       PRINT *,'JINDX1'
!       PRINT *,(JINDX1(N),N=1,LEN)
!       PRINT *,'JINDX2'
!       PRINT *,(JINDX2(N),N=1,LEN)
!       PRINT *,'DDY'
!       PRINT *,(DDY(N),N=1,LEN)
!       PRINT *,'DDX'
!       PRINT *,(DDX(N),N=1,LEN)
!     endif
!
        SUM1 = 0.
        SUM2 = 0.
        SUM3 = 0.
        SUM4 = 0.
        IF (LMASK) THEN
          WEI1 = 0.
          WEI2 = 0.
          WEI3 = 0.
          WEI4 = 0.
          DO I=1,IMXIN
            SUM1 = SUM1 + REGIN(I,1) * RSLMSK(I,1)
            SUM2 = SUM2 + REGIN(I,JMXIN) * RSLMSK(I,JMXIN)
            WEI1 = WEI1 + RSLMSK(I,1)
            WEI2 = WEI2 + RSLMSK(I,JMXIN)
!
            SUM3 = SUM3 + REGIN(I,1) * (1.0-RSLMSK(I,1))
            SUM4 = SUM4 + REGIN(I,JMXIN) * (1.0-RSLMSK(I,JMXIN))
            WEI3 = WEI3 + (1.0-RSLMSK(I,1))
            WEI4 = WEI4 + (1.0-RSLMSK(I,JMXIN))
          ENDDO
!
          IF(WEI1.GT.0.) THEN
            SUM1 = SUM1 / WEI1
          ELSE
            SUM1 = 0.
          ENDIF
          IF(WEI2.GT.0.) THEN
            SUM2 = SUM2 / WEI2
          ELSE
            SUM2 = 0.
          ENDIF
          IF(WEI3.GT.0.) THEN
            SUM3 = SUM3 / WEI3
          ELSE
            SUM3 = 0.
          ENDIF
          IF(WEI4.GT.0.) THEN
            SUM4 = SUM4 / WEI4
          ELSE
            SUM4 = 0.
          ENDIF
        ELSE
          DO I=1,IMXIN
            SUM1 = SUM1 + REGIN(I,1)
            SUM2 = SUM2 + REGIN(I,JMXIN)
          ENDDO
          SUM1 = SUM1 / IMXIN
          SUM2 = SUM2 / IMXIN
          SUM3 = SUM1
          SUM4 = SUM2
        ENDIF
!
!     print *,' SUM1=',SUM1,' SUM2=',SUM2
!    *,' SUM3=',SUM3,' SUM4=',SUM4
!     print *,' RSLMSK=',(RSLMSK(I,1),I=1,IMXIN)
!     print *,' SLMASK=',(SLMASK(I),I=1,IMXOUT)
!    *,' j1=',jindx1(1),' j2=',jindx2(1)
!
!
!  INTTYP=1  Take the closest point value
!
        IF(INTTYP.EQ.1) THEN

          DO I=I1_T,I2_T
            JY = JINDX1(I)
            IF(DDY(I) .GE. 0.5) JY = JINDX2(I)
            IX = IINDX1(I)
            IF(DDX(I) .GE. 0.5) IX = IINDX2(I)
!
!cggg start
!
            if (.not. lmask) then

              GAUOUT(I) = REGIN(IX,JY)

            else

              IF(SLMASK(I).EQ.RSLMSK(IX,JY)) THEN

                GAUOUT(I) = REGIN(IX,JY)

              else

                i1 = ix
                j1 = jy

! SPIRAL AROUND UNTIL MATCHING MASK IS FOUND.
                DO NX=1,JMXIN*IMXIN/2
                  KXS=SQRT(4*NX-2.5)
                  KXT=NX-INT(KXS**2/4+1)
                  SELECT CASE(MOD(KXS,4))
                  CASE(1)
                    IX=I1-KXS/4+KXT
                    JX=J1-KXS/4
                  CASE(2)
                    IX=I1+1+KXS/4
                    JX=J1-KXS/4+KXT
                  CASE(3)
                    IX=I1+1+KXS/4-KXT
                    JX=J1+1+KXS/4
                  CASE DEFAULT
                    IX=I1-KXS/4
                    JX=J1+KXS/4-KXT
                  END SELECT
                  IF(JX.LT.1) THEN
                    IX=IX+IMXIN/2
                    JX=2-JX
                  ELSEIF(JX.GT.JMXIN) THEN
                    IX=IX+IMXIN/2
                    JX=2*JMXIN-JX
                  ENDIF
                  IX=MODULO(IX-1,IMXIN)+1
                  IF(SLMASK(I).EQ.RSLMSK(IX,JX)) THEN
                    GAUOUT(I) = REGIN(IX,JX)
                    GO TO 81
                  ENDIF
                ENDDO

!cggg here, set the gauout value to be 0, and let's sarah's land
!cggg routine assign a default.

              if (NUM_THREADS == 1) then
                print*,'no matching mask found ',i,i1,j1,ix,jx
                print*,'set to default value.'
              endif
              gauout(i) = 0.0


   81  continue

              end if

            end if

!cggg end

          ENDDO
          KMAMI=1
          if (me == 0 .and. NUM_THREADS == 1)
     &                  CALL MAXMIN(GAUOUT(I1_T),LEN_THREAD,KMAMI)
        ELSE  ! nearest neighbor interpolation

!
!  QUASI-BILINEAR INTERPOLATION
!
          IFILL(it) = 0
          IMXNX(it) = 0
          DO I=I1_T,I2_T
            Y  = DDY(I)
            J1 = JINDX1(I)
            J2 = JINDX2(I)
            X  = DDX(I)
            I1 = IINDX1(I)
            I2 = IINDX2(I)
!
            WI1J1 = (1.-X) * (1.-Y)
            WI2J1 =     X  *( 1.-Y)
            WI1J2 = (1.-X) *      Y
            WI2J2 =     X  *      Y
!
            TEM = 4.*SLMASK(I) - RSLMSK(I1,J1) - RSLMSK(I2,J1)
     &                         - RSLMSK(I1,J2) - RSLMSK(I2,J2)
            IF(LMASK .AND. ABS(TEM) .GT. 0.01) THEN
              IF(SLMASK(I).EQ.1.) THEN
                  WI1J1 = WI1J1 * RSLMSK(I1,J1)
                  WI2J1 = WI2J1 * RSLMSK(I2,J1)
                  WI1J2 = WI1J2 * RSLMSK(I1,J2)
                  WI2J2 = WI2J2 * RSLMSK(I2,J2)
              ELSE
                  WI1J1 = WI1J1 * (1.0-RSLMSK(I1,J1))
                  WI2J1 = WI2J1 * (1.0-RSLMSK(I2,J1))
                  WI1J2 = WI1J2 * (1.0-RSLMSK(I1,J2))
                  WI2J2 = WI2J2 * (1.0-RSLMSK(I2,J2))
              ENDIF
            ENDIF
!
            WSUM   = WI1J1 + WI2J1 + WI1J2 + WI2J2
            WRK(I) = WSUM
            IF(WSUM.NE.0.) THEN
              WSUMIV = 1./WSUM
!
              IF(J1.NE.J2) THEN
                GAUOUT(I) = (WI1J1*REGIN(I1,J1) + WI2J1*REGIN(I2,J1) +
     &                       WI1J2*REGIN(I1,J2) + WI2J2*REGIN(I2,J2))
     &                    *WSUMIV
              ELSE
!
                IF (RLAT .GT. 0.0) THEN
                  IF (SLMASK(I) .EQ. 1.0) THEN
                    SUMN = SUM1
                    SUMS = SUM2
                  ELSE
                    SUMN = SUM3
                    SUMS = SUM4
                  ENDIF
                  IF( J1 .EQ. 1) THEN
                    GAUOUT(I) = (WI1J1*SUMN        +WI2J1*SUMN        +
     &                           WI1J2*REGIN(I1,J2)+WI2J2*REGIN(I2,J2))
     &                        * WSUMIV
                  ELSEIF (J1 .EQ. JMXIN) THEN
                    GAUOUT(I) = (WI1J1*REGIN(I1,J1)+WI2J1*REGIN(I2,J1)+
     &                           WI1J2*SUMS        +WI2J2*SUMS        )
     &                        * WSUMIV
                  ENDIF
!       print *,' slmask=',slmask(i),' sums=',sums,' sumn=',sumn
!    &  ,' regin=',regin(i1,j2),regin(i2,j2),' j1=',j1,' j2=',j2
!    &  ,' wij=',wi1j1, wi2j1, wi1j2, wi2j2,wsumiv
                ELSE
                  IF (SLMASK(I) .EQ. 1.0) THEN
                    SUMS = SUM1
                    SUMN = SUM2
                  ELSE
                    SUMS = SUM3
                    SUMN = SUM4
                  ENDIF
                  IF( J1 .EQ. 1) THEN
                    GAUOUT(I) = (WI1J1*REGIN(I1,J1)+WI2J1*REGIN(I2,J1)+
     &                           WI1J2*SUMS        +WI2J2*SUMS        )
     &                        * WSUMIV
                  ELSEIF (J1 .EQ. JMXIN) THEN
                    GAUOUT(I) = (WI1J1*SUMN        +WI2J1*SUMN        +
     &                           WI1J2*REGIN(I1,J2)+WI2J2*REGIN(I2,J2))
     &                        * WSUMIV
                  ENDIF
                ENDIF
              ENDIF            ! if j1 .ne. j2
            ENDIF
          ENDDO
          DO I=I1_T,I2_T
            J1 = JINDX1(I)
            J2 = JINDX2(I)
            I1 = IINDX1(I)
            I2 = IINDX2(I)
            IF(WRK(I) .EQ. 0.0) THEN
              IF(.NOT.LMASK) THEN
                if (NUM_THREADS == 1)
     &            WRITE(6,*) ' LA2GA called with LMASK=.TRUE. but bad',
     &                     ' RSLMSK or SLMASK given'
                CALL ABORT
              ENDIF
              IFILL(it) = IFILL(it) + 1
              IF(IFILL(it) <= 2 ) THEN
                if (me == 0 .and. NUM_THREADS == 1) then
                  WRITE(6,*) 'I1,I2,J1,J2=',I1,I2,J1,J2
                  WRITE(6,*) 'RSLMSK=',RSLMSK(I1,J1),RSLMSK(I1,J2),
     &                                 RSLMSK(I2,J1),RSLMSK(I2,J2)
!                 WRITE(6,*) 'I,J=',I,J,' SLMASK(I)=',SLMASK(I)
                  WRITE(6,*) 'I=',I,' SLMASK(I)=',SLMASK(I)
     &,           ' outlon=',outlon(i),' outlat=',outlat(i)
                endif
              ENDIF
! SPIRAL AROUND UNTIL MATCHING MASK IS FOUND.
              DO NX=1,JMXIN*IMXIN/2
                KXS=SQRT(4*NX-2.5)
                KXT=NX-INT(KXS**2/4+1)
                SELECT CASE(MOD(KXS,4))
                CASE(1)
                  IX=I1-KXS/4+KXT
                  JX=J1-KXS/4
                CASE(2)
                  IX=I1+1+KXS/4
                  JX=J1-KXS/4+KXT
                CASE(3)
                  IX=I1+1+KXS/4-KXT
                  JX=J1+1+KXS/4
                CASE DEFAULT
                  IX=I1-KXS/4
                  JX=J1+KXS/4-KXT
                END SELECT
                IF(JX.LT.1) THEN
                  IX=IX+IMXIN/2
                  JX=2-JX
                ELSEIF(JX.GT.JMXIN) THEN
                  IX=IX+IMXIN/2
                  JX=2*JMXIN-JX
                ENDIF
                IX=MODULO(IX-1,IMXIN)+1
                IF(SLMASK(I).EQ.RSLMSK(IX,JX)) THEN
                  GAUOUT(I) = REGIN(IX,JX)
                  IMXNX(it) = MAX(IMXNX(it),NX)
                  GO TO 71
                ENDIF
              ENDDO
!
              if (NUM_THREADS == 1) then
                WRITE(6,*) ' ERROR!!! No filling value found in LA2GA'
!               WRITE(6,*) ' I IX JX SLMASK(I) RSLMSK ',
!    &                       I,IX,JX,SLMASK(I),RSLMSK(IX,JX)
              endif
              CALL ABORT
!
   71         CONTINUE
            ENDIF
!
          ENDDO
        ENDIF
      ENDDO            ! END OF THREADED LOOP ...................
!$OMP END PARALLEL DO
!
      IF(INTTYP /= 1)then
        ifills = 0
        do it=1,num_threads
          ifills = ifills + ifill(it)
        enddo

        IF(IFILLS.GT.1) THEN
          if (me .eq. 0) then
          WRITE(6,*) ' Unable to interpolate.  Filled with nearest',
     &               ' point value at ',IFILLS,' points'
!    &               ' point value at ',IFILLS,' points  imxnx=',imxnx(:)
          endif
        ENDIF
        deallocate (ifill)
      ENDIF
!
      KMAMI=1
      if (me .eq. 0) CALL MAXMIN(GAUOUT,LEN,KMAMI)
!
      RETURN
      END SUBROUTINE LA2GA
      SUBROUTINE MAXMIN(F,IMAX,KMAX)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,iimin,iimax,kmax,imax,k
      REAL (KIND=KIND_IO8) fmin,fmax
!
      REAL (KIND=KIND_IO8) F(IMAX,KMAX)
!
      DO K=1,KMAX
!
        FMAX = F(1,K)
        FMIN = F(1,K)
!
        DO I=1,IMAX
          IF(FMAX.LE.F(I,K)) THEN
            FMAX  = F(I,K)
            IIMAX = I
          ENDIF
          IF(FMIN.GE.F(I,K)) THEN
            FMIN  = F(I,K)
            IIMIN = I
          ENDIF
        ENDDO
!
      WRITE(6,100) K,FMAX,IIMAX,FMIN,IIMIN
  100 FORMAT(2X,'LEVEL=',I2,' MAX=',E11.4,' AT I=',I7,
     &                      ' MIN=',E11.4,' AT I=',I7)
!
      ENDDO
!
      RETURN
      END
      SUBROUTINE FILANL(TSFANL,TSFAN2,WETANL,SNOANL,ZORANL,ALBANL,
     &                  AISANL,
     &                  TG3ANL,CVANL ,CVBANL,CVTANL,
     &                  CNPANL,SMCANL,STCANL,SLIANL,SCVANL,VEGANL,
     &                  vetanl,sotanl,ALFANL,
!Cwu [+1L] add ()anl for sih, sic
     &                  SIHANL,SICANL,
!Clu [+1L] add ()anl for vmn, vmx, slp, abs
     &                  VMNANL,VMXANL,SLPANL,ABSANL,
     &                  TSFCLM,TSFCL2,WETCLM,SNOCLM,ZORCLM,ALBCLM,
     &                  AISCLM,
     &                  TG3CLM,CVCLM ,CVBCLM,CVTCLM,
     &                  CNPCLM,SMCCLM,STCCLM,SLICLM,SCVCLM,VEGCLM,
     &                  vetclm,sotclm,ALFCLM,
!Cwu [+1L] add ()clm for sih, sic
     &                  SIHCLM,SICCLM,
!Clu [+1L] add ()clm for vmn, vmx, slp, abs
     &                  VMNCLM,VMXCLM,SLPCLM,ABSCLM,
     &                  LEN,LSOIL)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,j,len,lsoil
!
      REAL (KIND=KIND_IO8) TSFANL(LEN),TSFAN2(LEN),WETANL(LEN),
     &     SNOANL(LEN),
     &     ZORANL(LEN),ALBANL(LEN,4),AISANL(LEN),
     &     TG3ANL(LEN),
     &     CVANL (LEN),CVBANL(LEN),CVTANL(LEN),
     &     CNPANL(LEN),
     &     SMCANL(LEN,LSOIL),STCANL(LEN,LSOIL),
     &     SLIANL(LEN),SCVANL(LEN),VEGANL(LEN),
     &     vetanl(LEN),sotanl(LEN),ALFANL(LEN,2)
!Cwu [+1L] add ()anl for sih, sic
     &,    SIHANL(LEN),SICANL(LEN)
!Clu [+1L] add ()anl for vmn, vmx, slp, abs
     &,    VMNANL(LEN),VMXANL(LEN),SLPANL(LEN),ABSANL(LEN)
      REAL (KIND=KIND_IO8) TSFCLM(LEN),TSFCL2(LEN),WETCLM(LEN),
     &     SNOCLM(LEN),
     &     ZORCLM(LEN),ALBCLM(LEN,4),AISCLM(LEN),
     &     TG3CLM(LEN),
     &     CVCLM (LEN),CVBCLM(LEN),CVTCLM(LEN),
     &     CNPCLM(LEN),
     &     SMCCLM(LEN,LSOIL),STCCLM(LEN,LSOIL),
     &     SLICLM(LEN),SCVCLM(LEN),VEGCLM(LEN),
     &     vetclm(LEN),sotclm(LEN),ALFCLM(LEN,2)
!Cwu [+1L] add ()clm for sih, sic
     &,    SIHCLM(LEN),SICCLM(LEN)
!Clu [+1L] add ()clm for vmn, vmx, slp, abs
     &,    VMNCLM(LEN),VMXCLM(LEN),SLPCLM(LEN),ABSCLM(LEN)
!
      DO I=1,LEN
        TSFANL(I)   = TSFCLM(I)      !  Tsf at t
        TSFAN2(I)   = TSFCL2(I)      !  Tsf at t-deltsfc
        WETANL(I)   = WETCLM(I)      !  Soil Wetness
        SNOANL(I)   = SNOCLM(I)      !  SNOW
        SCVANL(I)   = SCVCLM(I)      !  SNOW COVER
        AISANL(I)   = AISCLM(I)      !  SEAICE
        SLIANL(I)   = SLICLM(I)      !  LAND/SEA/SNOW mask
        ZORANL(I)   = ZORCLM(I)      !  Surface roughness
!       PLRANL(I)   = PLRCLM(I)      !  Maximum stomatal resistance
        TG3ANL(I)   = TG3CLM(I)      !  Deep soil temperature
        CNPANL(I)   = CNPCLM(I)      !  Canopy water content
        VEGANL(I)   = VEGCLM(I)      !  Vegetation cover
        VEtANL(I)   = VEtCLM(I)      !  Vegetation type
        sotANL(I)   = sotCLM(I)      !  Soil type
        CVANL(I)    = CVCLM(I)       !  CV
        CVBANL(I)   = CVBCLM(I)      !  CVB
        CVTANL(I)   = CVTCLM(I)      !  CVT
!Cwu [+4L] add sih, sic
        SIHANL(I)   = SIHCLM(I)      !  Sea ice thickness
        SICANL(I)   = SICCLM(I)      !  Sea ice concentration
!Clu [+4L] add vmn, vmx, slp, abs
        VMNANL(I)   = VMNCLM(I)      !  Min vegetation cover
        VMXANL(I)   = VMXCLM(I)      !  Max vegetation cover 
        SLPANL(I)   = SLPCLM(I)      !  slope type
        ABSANL(I)   = ABSCLM(I)      !  Max snow albedo
      ENDDO
!
      DO J=1,LSOIL
        DO I=1,LEN
          SMCANL(I,J) = SMCCLM(I,J)  !   Layer soil wetness
          STCANL(I,J) = STCCLM(I,J)  !   Soil temperature
        ENDDO
      ENDDO
      DO J=1,4
        DO I=1,LEN
          ALBANL(I,J) = ALBCLM(I,J)  !  Albedo
        ENDDO
      ENDDO
      DO J=1,2
        DO I=1,LEN
          ALFANL(I,J) = ALFCLM(I,J)  !  Vegetation fraction for Albedo
        ENDDO
      ENDDO
!
      RETURN
      END
      SUBROUTINE ANALY(LUGB,IY,IM,ID,IH,FH,LEN,LSOIL,
     &                 SLMASK,FNTSFA,FNWETA,FNSNOA,FNZORA,FNALBA,FNAISA,
     &                 FNTG3A,FNSCVA,FNSMCA,FNSTCA,FNACNA,FNVEGA,
     &                 fnveta,fnsota,
!Clu [+1L] add fn()a for vmn, vmx, slp, abs
     &                 FNVMNA,FNVMXA,FNSLPA,FNABSA,
     &                 TSFANL,WETANL,SNOANL,ZORANL,ALBANL,AISANL,
     &                 TG3ANL,CVANL ,CVBANL,CVTANL,
     &                 SMCANL,STCANL,SLIANL,SCVANL,ACNANL,VEGANL,
     &                 vetanl,sotanl,ALFANL,TSFAN0,
!Clu [+1L] add ()anl for vmn, vmx, slp, abs
     &                 VMNANL,VMXANL,SLPANL,ABSANL,
!cggg snow mods start    &        KPDTSF,KPDWET,KPDSNO,KPDZOR,KPDALB,KPDAIS,
     &                 KPDTSF,KPDWET,KPDSNO,KPDSND,KPDZOR,KPDALB,KPDAIS,
!cggg snow mods end
     &                 KPDTG3,KPDSCV,KPDACN,KPDSMC,KPDSTC,KPDVEG,
     &                 kprvet,kpdsot,kpdalf,
!Clu [+1L] add kpd() for vmn, vmx, slp, abs
     &                 KPDVMN,KPDVMX,KPDSLP,KPDABS,
     &                 IRTTSF,IRTWET,IRTSNO,IRTZOR,IRTALB,IRTAIS,
     &                 IRTTG3,IRTSCV,IRTACN,IRTSMC,IRTSTC,IRTVEG,
     &                 irtvet,irtsot,irtalf
!Clu [+1L] add irt() for vmn, vmx, slp, abs
     &,                IRTVMN,IRTVMX,IRTSLP,IRTABS
     &,                IMSK, JMSK, SLMSKH, OUTLAT, OUTLON
     &,                GAUS, BLNO, BLTO, me)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer irtsmc,irtacn,irtstc,irtvet,irtveg,irtscv,irtzor,irtsno,
     &        irtalb,irttg3,irtais,iret,me,kk,kpdvet,i,irtalf,irtsot,
!cggg snow mods start     & imsk,jmsk,irtwet,lsoil,len, kpdtsf,kpdsno,kpdwet,iy,
     &        imsk,jmsk,irtwet,lsoil,len,kpdtsf,kpdsno,kpdsnd,kpdwet,iy,
!cggg snow mods end
     &        lugb,im,ih,id,kpdveg,kpdstc,kprvet,irttsf,kpdsot,kpdsmc,
     &        kpdais,kpdzor,kpdtg3,kpdacn,kpdscv,j
!Clu [+1L] add kpd() and irt() for vmn, vmx, slp, abs
     &,       kpdvmn,kpdvmx,kpdslp,kpdabs,irtvmn,irtvmx,irtslp,irtabs
      REAL (KIND=KIND_IO8) blto,blno,fh
!
      REAL (KIND=KIND_IO8)    SLMASK(LEN)
      REAL (KIND=KIND_IO8)    SLMSKH(IMSK,JMSK)
      REAL (KIND=KIND_IO8)    OUTLAT(LEN), OUTLON(LEN)
      INTEGER kpdalb(4),   kpdalf(2)
!cggg snow mods start
      INTEGER KPDS(1000),KGDS(1000),JPDS(1000),JGDS(1000)
      INTEGER LUGI, LSKIP, LGRIB, NDATA
!cggg snow mods end
!
      CHARACTER*500 FNTSFA,FNWETA,FNSNOA,FNZORA,FNALBA,FNAISA,
     &             FNTG3A,FNSCVA,FNSMCA,FNSTCA,FNACNA,FNVEGA,
     &             fnveta,fnsota
!Clu [+1L] add fn()a for vmn, vmx, slp, abs
     &,            FNVMNA,FNVMXA,FNSLPA,FNABSA

      REAL (KIND=KIND_IO8) TSFANL(LEN), WETANL(LEN),   SNOANL(LEN),
     &     ZORANL(LEN), ALBANL(LEN,4), AISANL(LEN),
     &     TG3ANL(LEN), ACNANL(LEN),
     &     CVANL (LEN), CVBANL(LEN),   CVTANL(LEN),
     &     SLIANL(LEN), SCVANL(LEN),   VEGANL(LEN),
     &     vetanl(LEN), sotanl(LEn),   ALFANL(LEN,2),
     &     SMCANL(LEN,LSOIL), STCANL(LEN,LSOIL),
     &     TSFAN0(LEN)
!Clu [+1L] add ()anl for vmn, vmx, slp, abs
     &,    VMNANL(LEN),VMXANL(LEN),SLPANL(LEN),ABSANL(LEN)
!
      LOGICAL GAUS
!
! TSF
!
      IRTTSF=0
      IF(FNTSFA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNTSFA,KPDTSF,SLMASK,
     &             IY,IM,ID,IH,FH,TSFANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTTSF=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'T SURFACE ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD T SURFACE ANALYSIS PROVIDED, Indicating proper',
     &            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'T SURFACE ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO TSF ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        endif
      ENDIF
!
! TSF0
!
!     IF(FNTSFA(1:8).NE.'        ') THEN
!       CALL FIXRDA(LUGB,FNTSFA,KPDTSF,SLMASK,
!    &             IY,IM,ID,IH,0.,TSFAN0,LEN,IRET
!    &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
!    &,            OUTLAT, OUTLON, me)
!       IF(IRET.EQ.1) THEN
!         WRITE(6,*) 'T SURFACE AT FT=0 ANALYSIS READ ERROR'
!         CALL ABORT
!       ELSEIF(IRET.EQ.-1) THEN
!         WRITE(6,*) 'COULD NOT FIND T SURFACE ANALYSIS AT FT=0'
!         CALL ABORT
!       ELSE
!         PRINT *,'T SURFACE ANALYSIS AT FT=0 FOUND.'
!       ENDIF
!     ELSE
!       DO I=1,LEN
!         TSFAN0(I)=-999.9
!       ENDDO
!     ENDIF
!
!  ALBEDO
!
      IRTALB=0
      IF(FNALBA(1:8).NE.'        ') THEN
        DO KK = 1, 4
          CALL FIXRDA(LUGB,FNALBA,KPDALB(KK),SLMASK,
     &               IY,IM,ID,IH,FH,ALBANL(1,KK),LEN,IRET
     &,              IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,              OUTLAT, OUTLON, me)
          IRTALB=IRET
          IF(IRET.EQ.1) THEN
            WRITE(6,*) 'ALBEDO ANALYSIS READ ERROR'
            CALL ABORT
          ELSEIF(IRET.EQ.-1) THEN
            if (me .eq. 0) then
            PRINT *,'OLD ALBEDO ANALYSIS PROVIDED, Indicating proper',
     &              ' file name is given.  No error suspected.'
            WRITE(6,*) 'FORECAST GUESS WILL BE USED'
            endif
          ELSE
            if (me .eq. 0 .and. kk .eq. 4)
     &                  PRINT *,'ALBEDO ANALYSIS PROVIDED.'
          ENDIF
        ENDDO
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO ALBEDO ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        endif
      ENDIF
!
!  Vegetation Fraction for albedo
!
      IRTALF=0
      IF(FNALBA(1:8).NE.'        ') THEN
        DO KK = 1, 2
          CALL FIXRDA(LUGB,FNALBA,KPDALF(KK),SLMASK,
     &               IY,IM,ID,IH,FH,ALFANL(1,KK),LEN,IRET
     &,              IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,              OUTLAT, OUTLON, me)
          IRTALF=IRET
          IF(IRET.EQ.1) THEN
            WRITE(6,*) 'ALBEDO ANALYSIS READ ERROR'
            CALL ABORT
          ELSEIF(IRET.EQ.-1) THEN
            if (me .eq. 0) then
            PRINT *,'OLD ALBEDO ANALYSIS PROVIDED, Indicating proper',
     &              ' file name is given.  No error suspected.'
            WRITE(6,*) 'FORECAST GUESS WILL BE USED'
            endif
          ELSE
            if (me .eq. 0 .and. kk .eq. 4)
     &                  PRINT *,'ALBEDO ANALYSIS PROVIDED.'
          ENDIF
        ENDDO
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO VEGFALBEDO ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        endif
      ENDIF
!
!  Soil Wetness
!
      IRTWET=0
      IRTSMC=0
      IF(FNWETA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNWETA,KPDWET,SLMASK,
     &             IY,IM,ID,IH,FH,WETANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTWET=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'BUCKET WETNESS ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD WETNESS ANALYSIS PROVIDED, Indicating proper',
     &            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'BUCKET WETNESS ANALYSIS PROVIDED.'
        ENDIF
      ELSEIF(FNSMCA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNSMCA,KPDSMC,SLMASK,
     &             IY,IM,ID,IH,FH,SMCANL(1,1),LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        CALL FIXRDA(LUGB,FNSMCA,KPDSMC,SLMASK,
     &             IY,IM,ID,IH,FH,SMCANL(1,2),LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTSMC=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'LAYER SOIL WETNESS ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD LAYER SOIL WETNESS ANALYSIS PROVIDED',
     &            ' Indicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'LAYER SOIL WETNESS ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO SOIL WETNESS ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        endif
      ENDIF
!
!  READ IN SNOW DEPTH/SNOW COVER
!
      IRTSCV=0
      IF(FNSNOA(1:8).NE.'        ') THEN
        DO I=1,LEN
          SCVANL(I)=0.
        ENDDO
!cggg snow mods start
!cggg need to determine if the snow data is on the gaussian grid
!cggg or not.  if gaussian, then data is a depth, not liq equiv
!cggg depth. if not gaussian, then data is from hua-lu's
!cggg program and is a liquid equiv.  need to communicate
!cggg this to routine fixrda via the 3rd argument which is
!cggg the grib parameter id number.
        CALL BAOPENR(LUGB,FNSNOA,IRET)
        IF (IRET .NE. 0) THEN
          WRITE(6,*) ' ERROR IN OPENING FILE ',trim(FNSNOA)
          PRINT *,'ERROR IN OPENING FILE ',trim(FNSNOA)
          CALL ABORT
        ENDIF
        LUGI=0
        lskip=-1
        JPDS=-1
        JGDS=-1
        KPDS=JPDS
        CALL GETGBH(LUGB,LUGI,LSKIP,JPDS,JGDS,LGRIB,NDATA,
     &              LSKIP,KPDS,KGDS,IRET)
        CLOSE(LUGB)
        IF (IRET .NE. 0) THEN
          WRITE(6,*) ' ERROR READING HEADER OF FILE: ',trim(FNSNOA)
          PRINT *,'ERROR READING HEADER OF FILE: ',trim(FNSNOA)
          CALL ABORT
        ENDIF
        IF (KGDS(1) == 4) THEN  ! GAUSSIAN DATA IS DEPTH
          CALL FIXRDA(LUGB,FNSNOA,KPDSND,SLMASK,
     &                IY,IM,ID,IH,FH,SNOANL,LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
          SNOANL=SNOANL*100.  ! CONVERT FROM METERS TO LIQ. EQ.
                              ! DEPTH IN MM USING 10:1 RATIO
        ELSE                    ! LAT/LON DATA IS LIQ EQUV. DEPTH
          CALL FIXRDA(LUGB,FNSNOA,KPDSNO,SLMASK,
     &                IY,IM,ID,IH,FH,SNOANL,LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
        ENDIF
!cggg snow mods end
        IRTSCV=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'SNOW DEPTH ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD SNOW DEPTH ANALYSIS PROVIDED, Indicating proper',
     &            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'SNOW DEPTH ANALYSIS PROVIDED.'
        ENDIF
        IRTSNO=0
      ELSEIF(FNSCVA(1:8).NE.'        ') THEN
        DO I=1,LEN
          SNOANL(I)=0.
        ENDDO
        CALL FIXRDA(LUGB,FNSCVA,KPDSCV,SLMASK,
     &             IY,IM,ID,IH,FH,SCVANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTSNO=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'SNOW COVER ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD SNOW COVER ANALYSIS PROVIDED, Indicating proper',
     &            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'SNOW COVER ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO SNOW/SNOCOV ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        endif
      ENDIF
!
!  Sea ice mask
!
      IRTACN=0
      IRTAIS=0
      IF(FNACNA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNACNA,KPDACN,SLMASK,
     &             IY,IM,ID,IH,FH,ACNANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTACN=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'ICE CONCENTRATION ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD ICE CONCENTRATION ANALYSIS PROVIDED',
     &            ' Indicating proper file name is given'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'ICE CONCENTRATION ANALYSIS PROVIDED.'
        ENDIF
      ELSEIF(FNAISA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNAISA,KPDAIS,SLMASK,
     &             IY,IM,ID,IH,FH,AISANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTAIS=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'ICE MASK ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD ICE-MASK ANALYSIS PROVIDED, Indicating proper',
     &            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'ICE MASK ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO SEA-ICE ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        endif
      ENDIF
!
!  Surface Roughness
!
      IRTZOR=0
      IF(FNZORA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNZORA,KPDZOR,SLMASK,
     &             IY,IM,ID,IH,FH,ZORANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTZOR=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'ROUGHNESS ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD ROUGHNESS ANALYSIS PROVIDED, Indicating proper',
     &            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'ROUGHNESS ANALYSIS PROVIDED.'
        ENDIF
      ELSE
          if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO SRFC ROUGHNESS ANALYSIS AVAILABLE. CLIMATOLOGY USED'
        endif
      ENDIF
!
!  Deep Soil Temperature
!
      IRTTG3=0
      IRTSTC=0
      IF(FNTG3A(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNTG3A,KPDTG3,SLMASK,
     &             IY,IM,ID,IH,FH,TG3ANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTTG3=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'DEEP SOIL TMP ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD DEEP SOIL TEMP ANALYSIS PROVIDED',
     &            ' Indicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'DEEP SOIL TMP ANALYSIS PROVIDED.'
        ENDIF
      ELSEIF(FNSTCA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNSTCA,KPDSTC,SLMASK,
     &             IY,IM,ID,IH,FH,STCANL(1,1),LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        CALL FIXRDA(LUGB,FNSTCA,KPDSTC,SLMASK,
     &             IY,IM,ID,IH,FH,STCANL(1,2),LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTSTC=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'LAYER SOIL TMP ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD DEEP SOIL TEMP ANALYSIS PROVIDED',
     &            'iIndicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'LAYER SOIL TMP ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO DEEP SOIL TEMP ANALY AVAILABLE.  CLIMATOLOGY USED'
        endif
      ENDIF
!
!  VEGETATION COVER
!
      IRTVEG=0
      IF(FNVEGA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNVEGA,KPDVEG,SLMASK,
     &             IY,IM,ID,IH,FH,VEGANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTVEG=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'VEGETATION COVER ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD VEGETATION COVER ANALYSIS PROVIDED',
     &            ' Indicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'GEGETATION COVER ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO VEGETATION COVER ANLY AVAILABLE. CLIMATOLOGY USED'
        endif
      ENDIF
!
!  VEGETATION type
!
      IRTVEt=0
      IF(FNVEtA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNVEtA,KPDVEt,SLMASK,
     &             IY,IM,ID,IH,FH,VEtANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTVEt=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'VEGETATION type ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD VEGETATION type ANALYSIS PROVIDED',
     &            ' Indicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'VEGETATION type ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO VEGETATION type ANLY AVAILABLE. CLIMATOLOGY USED'
        endif
      ENDIF
!
!  soil type
!
      IRTsot=0
      IF(FNsotA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNsotA,KPDsot,SLMASK,
     &             IY,IM,ID,IH,FH,sotANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTsot=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'soil type ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD soil type ANALYSIS PROVIDED',
     &            ' Indicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'soil type ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO soil type ANLY AVAILABLE. CLIMATOLOGY USED'
        endif
      ENDIF

!Clu [+120L]--------------------------------------------------------------
!
!  Min vegetation cover
!
      IRTvmn=0
      IF(FNvmnA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNvmnA,KPDvmn,SLMASK,
     &             IY,IM,ID,IH,FH,vmnANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTvmn=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'shdmin ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD shdmin ANALYSIS PROVIDED',
     &            ' Indicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'shdmin ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO shdmin ANLY AVAILABLE. CLIMATOLOGY USED'
        endif
      ENDIF

!
!  Max vegetation cover
!
      IRTvmx=0
      IF(FNvmxA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNvmxA,KPDvmx,SLMASK,
     &             IY,IM,ID,IH,FH,vmxANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTvmx=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'shdmax ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD shdmax ANALYSIS PROVIDED',
     &            ' Indicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'shdmax ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO shdmax ANLY AVAILABLE. CLIMATOLOGY USED'
        endif
      ENDIF

!
!  slope type
!
      IRTslp=0
      IF(FNslpA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNslpA,KPDslp,SLMASK,
     &             IY,IM,ID,IH,FH,slpANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTslp=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'slope type ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD slope type ANALYSIS PROVIDED',
     &            ' Indicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'slope type ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO slope type ANLY AVAILABLE. CLIMATOLOGY USED'
        endif
      ENDIF

!
!  Max snow albedo
!
      IRTabs=0
      IF(FNabsA(1:8).NE.'        ') THEN
        CALL FIXRDA(LUGB,FNabsA,KPDabs,SLMASK,
     &             IY,IM,ID,IH,FH,absANL,LEN,IRET
     &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,            OUTLAT, OUTLON, me)
        IRTabs=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'snoalb ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          if (me .eq. 0) then
          PRINT *,'OLD snoalb ANALYSIS PROVIDED',
     &            ' Indicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
          endif
        ELSE
          if (me .eq. 0) PRINT *,'snoalb ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        if (me .eq. 0) then
!       PRINT *,'************************************************'
        PRINT *,'NO snoalb ANLY AVAILABLE. CLIMATOLOGY USED'
        endif
      ENDIF

!Clu ----------------------------------------------------------------------
!
      RETURN
      END
      SUBROUTINE FILFCS(TSFFCS,WETFCS,SNOFCS,ZORFCS,ALBFCS,
     &                  TG3FCS,CVFCS ,CVBFCS,CVTFCS,
     &                  CNPFCS,SMCFCS,STCFCS,SLIFCS,AISFCS,
     &                  VEGFCS, vetfcs, sotfcs, alffcs,
!Cwu [+1L] add ()fcs for sih, sic
     &                  SIHFCS,SICFCS,
!Clu [+1L] add ()fcs for vmn, vmx, slp, abs
     &                  VMNFCS,VMXFCS,SLPFCS,ABSFCS,
     &                  TSFANL,WETANL,SNOANL,ZORANL,ALBANL,
     &                  TG3ANL,CVANL ,CVBANL,CVTANL,
     &                  CNPANL,SMCANL,STCANL,SLIANL,AISANL,
     &                  VEGANL, vetanl, sotanl, ALFANL,
!Cwu [+1L] add ()anl for sih, sic
     &                  SIHANL,SICANL,
!Clu [+1L] add ()anl for vmn, vmx, slp, abs
     &                  VMNANL,VMXANL,SLPANL,ABSANL,
     &                  LEN,LSOIL)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,j,len,lsoil
      REAL (KIND=KIND_IO8) TSFFCS(LEN),WETFCS(LEN),SNOFCS(LEN),
     &     ZORFCS(LEN),ALBFCS(LEN,4),AISFCS(LEN),
     &     TG3FCS(LEN),
     &     CVFCS (LEN),CVBFCS(LEN),CVTFCS(LEN),
     &     CNPFCS(LEN),
     &     SMCFCS(LEN,LSOIL),STCFCS(LEN,LSOIL),
     &     SLIFCS(LEN),VEGFCS(LEn),
     &     vetfcs(LEN),sotfcs(LEN),alffcs(LEN,2)
!Cwu [+1L] add ()fcs for sih, sic
     &,    SIHFCS(LEN),SICFCS(LEN)
!Clu [+1L] add ()fcs for vmn, vmx, slp, abs
     &,    VMNFCS(LEN),VMXFCS(LEN),SLPFCS(LEN),ABSFCS(LEN)
      REAL (KIND=KIND_IO8) TSFANL(LEN),WETANL(LEN),SNOANL(LEN),
     &     ZORANL(LEN),ALBANL(LEN,4),AISANL(LEN),
     &     TG3ANL(LEN),
     &     CVANL (LEN),CVBANL(LEN),CVTANL(LEN),
     &     CNPANL(LEN),
     &     SMCANL(LEN,LSOIL),STCANL(LEN,LSOIL),
     &     SLIANL(LEN),VEGANL(LEN),
     &     vetanl(LEN),sotanl(LEN),ALFANL(LEN,2)
!Cwu [+1L] add ()anl for sih, sic
     &,    SIHANL(LEN),SICANL(LEN)
!Clu [+1L] add ()anl for vmn, vmx, slp, abs
     &,    VMNANL(LEN),VMXANL(LEN),SLPANL(LEN),ABSANL(LEN)
!
      WRITE(6,*) '  THIS IS A DEAD START RUN, TSFC OVER LAND IS',
     &           ' SET AS LOWEST SIGMA LEVEL TEMPERTURE IF GIVEN.'
      WRITE(6,*) '  IF NOT, SET TO CLIMATOLOGICAL TSF OVER LAND IS USED'
!
!
      DO I=1,LEN
        TSFFCS(I)   = TSFANL(I)      !  Tsf
        ALBFCS(I,1) = ALBANL(I,1)    !  Albedo
        ALBFCS(I,2) = ALBANL(I,2)    !  Albedo
        ALBFCS(I,3) = ALBANL(I,3)    !  Albedo
        ALBFCS(I,4) = ALBANL(I,4)    !  Albedo
        WETFCS(I)   = WETANL(I)      !  Soil Wetness
        SNOFCS(I)   = SNOANL(I)      !  SNOW
        AISFCS(I)   = AISANL(I)      !  SEAICE
        SLIFCS(I)   = SLIANL(I)      !  LAND/SEA/SNOW mask
        ZORFCS(I)   = ZORANL(I)      !  Surface roughness
!       PLRFCS(I)   = PLRANL(I)      !  Maximum stomatal resistance
        TG3FCS(I)   = TG3ANL(I)      !  Deep soil temperature
        CNPFCS(I)   = CNPANL(I)      !  Canopy water content
        CVFCS(I)    = CVANL(I)       !  CV
        CVBFCS(I)   = CVBANL(I)      !  CVB
        CVTFCS(I)   = CVTANL(I)      !  CVT
        VEGFCS(I)   = VEGANL(I)      !  Vegetation Cover
        vetfcs(I)   = vetanl(I)      !  Vegetation Type
        sotfcs(I)   = sotanl(I)      !  Soil type
        alffcs(I,1) = ALFANL(I,1)    !  Vegetation fraction for albedo
        alffcs(I,2) = ALFANL(I,2)    !  Vegetation fraction for albedo
!Cwu [+2L] add sih, sic
        SIHFCS(I)   = SIHANL(I)      !  Sea ice thickness
        SICFCS(I)   = SICANL(I)      !  Sea ice concentration
!Clu [+4L] add vmn, vmx, slp, abs
        VMNFCS(I)   = VMNANL(I)      !  Min vegetation Cover
        VMXFCS(I)   = VMXANL(I)      !  Max vegetation Cover
        SLPFCS(I)   = SLPANL(I)      !  Slope type
        ABSFCS(I)   = ABSANL(I)      !  Max snow albedo
      ENDDO
!
      DO J=1,LSOIL
        DO I=1,LEN
          SMCFCS(I,J) = SMCANL(I,J)  !   Layer soil wetness
          STCFCS(I,J) = STCANL(I,J)  !   Soil temperature
        ENDDO
      ENDDO
!
      RETURN
      END
      SUBROUTINE BKTGES(SMCFCS,SLIANL,STCFCS,LEN,LSOIL)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,j,len,lsoil,k
      REAL (KIND=KIND_IO8) SMCFCS(LEN,LSOIL), STCFCS(LEN,LSOIL),
     &                     SLIANL(LEN)
!
!  Note that SMFCS comes in with the original unit (cm?) (not GRIB file)
!
      DO I = 1, LEN
        SMCFCS(I,1) = (SMCFCS(I,1)/150.) * .37 + .1
      ENDDO
      DO K = 2, LSOIL
        DO I = 1, LEN
          SMCFCS(I,K) = SMCFCS(I,1)
        ENDDO
      ENDDO
      IF(LSOIL.GT.2) THEN
        DO K = 3, LSOIL
          DO I = 1, LEN
            STCFCS(I,K) = STCFCS(I,2)
          ENDDO
        ENDDO
      ENDIF
!
      RETURN
      END
      SUBROUTINE ROF01(AISFLD,LEN,OP,CRIT)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,len
      REAL (KIND=KIND_IO8) AISFLD(LEN),crit
      CHARACTER*2 OP
!
      IF(OP.EQ.'GE') THEN
        DO I=1,LEN
          IF(AISFLD(I).GE.CRIT) THEN
            AISFLD(I)=1.
          ELSE
            AISFLD(I)=0.
          ENDIF
        ENDDO
      ELSEIF(OP.EQ.'GT') THEN
        DO I=1,LEN
          IF(AISFLD(I).GT.CRIT) THEN
            AISFLD(I)=1.
          ELSE
            AISFLD(I)=0.
          ENDIF
        ENDDO
      ELSEIF(OP.EQ.'LE') THEN
        DO I=1,LEN
          IF(AISFLD(I).LE.CRIT) THEN
            AISFLD(I)=1.
          ELSE
            AISFLD(I)=0.
          ENDIF
        ENDDO
      ELSEIF(OP.EQ.'LT') THEN
        DO I=1,LEN
          IF(AISFLD(I).LT.CRIT) THEN
            AISFLD(I)=1.
          ELSE
            AISFLD(I)=0.
          ENDIF
        ENDDO
      ELSE
        WRITE(6,*) ' Illegal operator in ROF01.  OP=',OP
        CALL ABORT
      ENDIF
!
      RETURN
      END
      SUBROUTINE TSFCOR(TSFC,OROG,SLMASK,UMASK,LEN,RLAPSE)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,len
      REAL (KIND=KIND_IO8) rlapse,umask
      REAL (KIND=KIND_IO8) TSFC(LEN), OROG(LEN), SLMASK(LEN)
!
      DO I=1,LEN
        IF(SLMASK(I).EQ.UMASK) THEN
          TSFC(I) = TSFC(I) - OROG(I)*RLAPSE
        ENDIF
      ENDDO
      RETURN
      END
!cggg landice mods start
!      SUBROUTINE SNODPTH(SCVANL,SLIANL,TSFANL,SNOCLM,
!     &                   GLACIR,SNWMAX,SNWMIN,LEN,SNOANL, me)
      SUBROUTINE SNODPTH(SCVANL,SLIANL,TSFANL,SNOCLM,
     &                   GLACIR,SNWMAX,SNWMIN,LANDICE,LEN,SNOANL, me)
!cggg landice mods end
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,me,len
!cggg landice mods start
      LOGICAL, INTENT(IN) :: LANDICE
!cggg landice mods end
      REAL (KIND=KIND_IO8) sno,snwmax,snwmin
!
      REAL (KIND=KIND_IO8) SCVANL(LEN), SLIANL(LEN), TSFANL(LEN),
     &     SNOCLM(LEN), SNOANL(LEN), GLACIR(LEN)
!
      if (me .eq. 0) WRITE(6,*) 'SNODPTH'
!
!  USE SURFACE TEMPERATURE TO GET SNOW DEPTH ESTIMATE
!
      DO I=1,LEN
        SNO = 0.0
!
!  OVER LAND
!
        IF(SLIANL(I).EQ.1.) THEN
          IF(SCVANL(I).EQ.1.0) THEN
            IF(TSFANL(I).LT.243.0) THEN
              SNO = SNWMAX
            ELSEIF(TSFANL(I).LT.273.0) THEN
              SNO = SNWMIN+(SNWMAX-SNWMIN)*(273.0-TSFANL(I))/30.0
            ELSE
              SNO = SNWMIN
            ENDIF
          ENDIF
!
!  IF GLACIAL POINTS HAS SNOW IN CLIMATOLOGY, SET SNO TO SNOMAX
!
!cggg landice mods start
          IF (.NOT.LANDICE) THEN
!cggg landice mods end
            IF(GLACIR(I).EQ.1.0) THEN
              SNO = SNOCLM(I)
              IF(SNO.EQ.0.) SNO=SNWMAX
            ENDIF
!cggg landice mods start
          ENDIF
!cggg landice mods end
        ENDIF
!
!  OVER SEA ICE
!
!  Snow over sea ice is cycled as of 01/01/94.....Hua-Lu Pan
!
        IF(SLIANL(I).EQ.2.0) THEN
          SNO=SNOCLM(I)
          IF(SNO.EQ.0.) SNO=SNWMAX
        ENDIF
!
        SNOANL(I) = SNO
      ENDDO
      RETURN
      END
      SUBROUTINE MERGE(LEN,LSOIL,IY,IM,ID,IH,FH,
!Cwu [+1L] add SIHFCS & SICFCS
     &                 SIHFCS,SICFCS,
!Clu [+1L] add ()fcs for vmn, vmx, slp, abs
     &                 VMNFCS,VMXFCS,SLPFCS,ABSFCS,
     &                 TSFFCS,WETFCS,SNOFCS,ZORFCS,ALBFCS,AISFCS,
     &                 CVFCS ,CVBFCS,CVTFCS,
     &                 CNPFCS,SMCFCS,STCFCS,SLIFCS,VEGFCS,
     &                 vetfcs,sotfcs,alffcs,
!Cwu [+1L] add SIHANL & SICANL
     &                 SIHANL,SICANL,                 
!Clu [+1L] add ()anl for vmn, vmx, slp, abs
     &                 VMNANL,VMXANL,SLPANL,ABSANL,
     &                 TSFANL,TSFAN2,WETANL,SNOANL,ZORANL,ALBANL,AISANL,
     &                 CVANL ,CVBANL,CVTANL,
     &                 CNPANL,SMCANL,STCANL,SLIANL,VEGANL,
     &                 vetanl,sotanl,ALFANL,
     &                 CTSFL,CALBL,CAISL,CSNOL,CSMCL,CZORL,CSTCL,CVEGL,
     &                 CTSFS,CALBS,CAISS,CSNOS,CSMCS,CZORS,CSTCS,CVEGS,
     &                 CCV,CCVB,CCVT,CCNP,cvetl,cvets,csotl,csots,
     &                 calfl,calfs,
!Cwu [+1L] add c()l and c()s for sih, sic
     &                 CSIHL,CSIHS,CSICL,CSICS,
!Clu [+1L] add c()l and c()s for vmn, vmx, slp, abs
     &                 CVMNL,CVMNS,CVMXL,CVMXS,CSLPL,CSLPS,CABSL,CABSS,
     &                 IRTTSF,IRTWET,IRTSNO,IRTZOR,IRTALB,IRTAIS,
     &                 IRTTG3,IRTSCV,IRTACN,IRTSMC,IRTSTC,IRTVEG,
!Clu [+1L] add irt() for vmn, vmx, slp, abs
     &                 IRTVMN,IRTVMX,IRTSLP,IRTABS,
!cggg landice start
!cggg     &                 irtvet,irtsot,irtalf, me)
     &                 irtvet,irtsot,irtalf, landice, me)
!cggg landice end
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer k,i,im,id,iy,len,lsoil,ih,irtacn,irtsmc,irtscv,irtais,
     &        irttg3,irtstc,irtalf,me,irtsot,irtveg,irtvet, irtzor,
     &        irtalb,irtsno,irttsf,irtwet,j
!Clu [+1L] add irt() for vmn, vmx, slp, abs
     &,       irtvmn,irtvmx,irtslp,irtabs
!cggg landice start
      logical, intent(in)  :: landice
!cggg landice end
      REAL (KIND=KIND_IO8) rvegs,rvets,rzors,raiss,rsnos,rsots,rcnp,
     &                     rcvt,rcv,rcvb,rsnol,rzorl,raisl,ralbl,
     &                     ralfl,rvegl,ralbs,ralfs,rtsfs,rvetl,rsotl,
     &                     qzors,qvegs,qsnos,qalfs,qaiss,qvets,qcvt,
     &                     qcnp,qcvb,qsots,qcv,qaisl,qsnol,qalfl,
     &                     qtsfl,qalbl,qzorl,qtsfs,qalbs,qsotl,qvegl,
     &                     qvetl,rtsfl,calbs,caiss,ctsfs,czorl,cvegl,
     &                     csnos,ccvb,ccvt,ccv,czors,cvegs,caisl,csnol,
     &                     calbl,fh,ctsfl,ccnp,csots,calfl,csotl,cvetl,
     &                     cvets,calfs
!Cwu [+3L] add c(), q(), r() for sih, sic
     &,                    csihl,csihs,csicl,csics,
     &                     rsihl,rsihs,rsicl,rsics,
     &                     qsihl,qsihs,qsicl,qsics
!Clu [+4L] add c(), q(), r() for vmn, vmx, slp, abs
     &,                    cvmnl,cvmns,cvmxl,cvmxs,cslpl,cslps
     &,                    cabsl,cabss,rvmnl,rvmns,rvmxl,rvmxs
     &,                    rslpl,rslps,rabsl,rabss,qvmnl,qvmns
     &,                    qvmxl,qvmxs,qslpl,qslps,qabsl,qabss
!
      REAL (KIND=KIND_IO8) TSFFCS(LEN), WETFCS(LEN),   SNOFCS(LEN),
     &     ZORFCS(LEN), ALBFCS(LEN,4), AISFCS(LEN),
     &     CVFCS (LEN), CVBFCS(LEN),   CVTFCS(LEN),
     &     CNPFCS(LEN),
     &     SMCFCS(LEN,LSOIL),STCFCS(LEN,LSOIL),
     &     SLIFCS(LEN), VEGFCS(LEN),
     &     vetfcs(LEN), sotfcs(LEN),   alffcs(LEN,2)
!Cwu [+1L] add SIHFCS & SICFCS
     &,    SIHFCS(LEN), SICFCS(LEN)
!Clu [+1L] add ()fcs for vmn, vmx, slp, abs
     &,    VMNFCS(LEN),VMXFCS(LEN),SLPFCS(LEN),ABSFCS(LEN)
      REAL (KIND=KIND_IO8) TSFANL(LEN),TSFAN2(LEN),
     &     WETANL(LEN),SNOANL(LEN),
     &     ZORANL(LEN), ALBANL(LEN,4), AISANL(LEN),
     &     CVANL (LEN), CVBANL(LEN),   CVTANL(LEN),
     &     CNPANL(LEN),
     &     SMCANL(LEN,LSOIL),STCANL(LEN,LSOIL),
     &     SLIANL(LEN), VEGANL(LEN),
     &     vetanl(LEN), sotanl(LEN),   ALFANL(LEN,2)
!Cwu [+1L] add SIHANL & SICANL
     &,    SIHANL(LEN),SICANL(LEN)           
!Clu [+1L] add ()anl for vmn, vmx, slp, abs
     &,    VMNANL(LEN),VMXANL(LEN),SLPANL(LEN),ABSANL(LEN)
!    &,    TSFAN2(LEN)
!
      REAL (KIND=KIND_IO8) CSMCL(LSOIL), CSMCS(LSOIL),
     &                     CSTCL(LSOIL), CSTCS(LSOIL)
      REAL (KIND=KIND_IO8) RSMCL(LSOIL), RSMCS(LSOIL),
     &                     RSTCL(LSOIL), RSTCS(LSOIL)
      REAL (KIND=KIND_IO8) QSMCL(LSOIL), QSMCS(LSOIL),
     &                     QSTCL(LSOIL), QSTCS(LSOIL)
      logical first
      integer   NUM_THREADS
      data first /.true./
      save NUM_THREADS, first
!
      integer LEN_THREAD_M, I1_T, I2_T, IT
      integer NUM_PARTHDS
!
      if (first) then
         NUM_THREADS = NUM_PARTHDS()
         first = .false.
      endif
!
!  COEEFICIENTS OF BLENDING FORECAST AND INTERPOLATED CLIM
!  (OR ANALYZED) FIELDS OVER SEA OR LAND(L) (NOT FOR CLOUDS)
!  1.0 = USE OF FORECAST
!  0.0 = REPLACE WITH INTERPOLATED ANALYSIS
!
!  Merging coefficients are defined by PARAMETER statement in calling program
!  and therefore they should not be modified in this program.
!
!
      RTSFL = CTSFL
      RALBL = CALBL
      RALFL = CALFL
      RAISL = CAISL
      RSNOL = CSNOL
!Clu  RSMCL = CSMCL
      RZORL = CZORL
      RVEGL = CVEGL
      rvetl = cvetl
      rsotl = csotl
!Cwu [+2L] add sih, sic
      RsihL = CsihL
      RsicL = CsicL
!Clu [+4L] add vmn, vmx, slp, abs
      RvmnL = CvmnL
      RvmxL = CvmxL
      RslpL = CslpL
      RabsL = CabsL
!
      RTSFS = CTSFS
      RALBS = CALBS
      RALFS = CALFS
      RAISS = CAISS
      RSNOS = CSNOS
!     RSMCS = CSMCS
      RZORS = CZORS
      RVEGS = CVEGS
      rvets = cvets
      rsots = csots
!Cwu [+2L] add sih, sic
      RsihS = CsihS
      RsicS = CsicS
!Clu [+4L] add vmn, vmx, slp, abs
      RvmnS = CvmnS
      RvmxS = CvmxS
      RslpS = CslpS
      RabsS = CabsS
!
      RCV  = CCV
      RCVB = CCVB
      RCVT = CCVT
      RCNP = CCNP
!
      DO K=1,LSOIL
        RSMCL(K) = CSMCL(K)
        RSMCS(K) = CSMCS(K)
        RSTCL(K) = CSTCL(K)
        RSTCS(K) = CSTCS(K)
      ENDDO
!
!  If analysis file name is given but no matching analysis date found,
!  use guess (these are flagged by IRT???=1).
!
      IF(IRTTSF.EQ.-1) THEN
        RTSFL = 1.
        RTSFS = 1.
      ENDIF
      IF(IRTALB.EQ.-1) THEN
        RALBL = 1.
        RALBS = 1.
        ralfl = 1.
        ralfs = 1.
      ENDIF
      IF(IRTAIS.EQ.-1) THEN
        RAISL = 1.
        RAISS = 1.
      ENDIF
      IF(IRTSNO.EQ.-1.OR.IRTSCV.EQ.-1) THEN
        RSNOL = 1.
        RSNOS = 1.
      ENDIF
      IF(IRTSMC.EQ.-1.OR.IRTWET.EQ.-1) THEN
!       RSMCL = 1.
!       RSMCS = 1.
        DO K=1,LSOIL
          RSMCL(K) = 1.
          RSMCS(K) = 1.
        ENDDO
      ENDIF
      IF(IRTSTC.EQ.-1) THEN
        DO K=1,LSOIL
          RSTCL(K) = 1.
          RSTCS(K) = 1.
        ENDDO
      ENDIF
      IF(IRTZOR.EQ.-1) THEN
        RZORL = 1.
        RZORS = 1.
      ENDIF
      IF(IRTVEG.EQ.-1) THEN
        RVEGL = 1.
        RVEGS = 1.
      ENDIF
      IF(IRTvet.EQ.-1) THEN
        RvetL = 1.
        RvetS = 1.
      ENDIF
      IF(IRTsot.EQ.-1) THEN
        RsotL = 1.
        RsotS = 1.
      ENDIF

!Cwu [+4L] -----------------------------------------------------------------
      IF(IRTacn.EQ.-1) THEN
        RsicL = 1.
        RsicS = 1.
      ENDIF
!Clu [+16L] -----------------------------------------------------------------
      IF(IRTvmn.EQ.-1) THEN
        RvmnL = 1.
        RvmnS = 1.
      ENDIF
      IF(IRTvmx.EQ.-1) THEN
        RvmxL = 1.
        RvmxS = 1.
      ENDIF
      IF(IRTslp.EQ.-1) THEN
        RslpL = 1.
        RslpS = 1.
      ENDIF
      IF(IRTabs.EQ.-1) THEN
        RabsL = 1.
        RabsS = 1.
      ENDIF
!Clu --------------------------------------------------------------------------
!
      if(raiss.eq.1..or.irtacn.eq.-1) then
        if (me .eq. 0) print *,'use forecast land-sea-ice mask'
        do i = 1, LEN
          aisanl(i) = aisfcs(i)
          slianl(i) = slifcs(i)
        enddo
      endif
!
      if (me .eq. 0) then
      WRITE(6,100) RTSFL,RALBL,RAISL,RSNOL,RSMCL,RZORL,RVEGL
  100 FORMAT('RTSFL,RALBL,RAISL,RSNOL,RSMCL,RZORL,RVEGL=',10F7.3)
      WRITE(6,101) RTSFS,RALBS,RAISS,RSNOS,RSMCS,RZORS,RVEGS
  101 FORMAT('RTSFS,RALBS,RAISS,RSNOS,RSMCS,RZORS,RVEGS=',10F7.3)
!     print *,' ralfl=',ralfl,' ralfs=',ralfs,' rsotl=',rsotl
!    *,' rsots=',rsots,' rvetl=',rvetl,' rvets=',rvets
      endif
!
      QTSFL = 1. - RTSFL
      QALBL = 1. - RALBL
      QALFL = 1. - RALFL
      QAISL = 1. - RAISL
      QSNOL = 1. - RSNOL
!     QSMCL = 1. - RSMCL
      QZORL = 1. - RZORL
      QVEGL = 1. - RVEGL
      QVETL = 1. - RVETL
      QsoTL = 1. - RsoTL
!Cwu [+2L] add sih, sic
      QsihL = 1. - RsihL
      QsicL = 1. - RsicL
!Clu [+4L] add vmn, vmx, slp, abs
      QvmnL = 1. - RvmnL
      QvmxL = 1. - RvmxL
      QslpL = 1. - RslpL
      QabsL = 1. - RabsL
!
      QTSFS = 1. - RTSFS
      QALBS = 1. - RALBS
      QALFS = 1. - RALFS
      QAISS = 1. - RAISS
      QSNOS = 1. - RSNOS
!     QSMCS = 1. - RSMCS
      QZORS = 1. - RZORS
      QVEGS = 1. - RVEGS
      QVEtS = 1. - RVEtS
      QsotS = 1. - RsotS
!Cwu [+2L] add sih, sic
      QsihS = 1. - RsihS
      QsicS = 1. - RsicS
!Clu [+4L] add vmn, vmx, slp, abs
      QvmnS = 1. - RvmnS
      QvmxS = 1. - RvmxS
      QslpS = 1. - RslpS
      QabsS = 1. - RabsS
!
      QCV   = 1. - RCV
      QCVB  = 1. - RCVB
      QCVT  = 1. - RCVT
      QCNP  = 1. - RCNP
!
      DO K=1,LSOIL
        QSMCL(K) = 1. - RSMCL(K)
        QSMCS(K) = 1. - RSMCS(K)
        QSTCL(K) = 1. - RSTCL(K)
        QSTCS(K) = 1. - RSTCS(K)
      ENDDO
!
!  Merging
!

!CluX  
      if(me .eq. 0) then
        print *, 'DBGX-- CSMCL:', (CSMCL(K),K=1,LSOIL)
        print *, 'DBGX-- RSMCL:', (RSMCL(K),K=1,LSOIL)
        print *, 'DBGX-- CSNOL, CSNOS:',CSNOL,CSNOS
        print *, 'DBGX-- RSNOL, RSNOS:',RSNOL,RSNOS
      endif

!     print *, RTSFS, QTSFS, RAISS , QAISS
!    *,        RSNOS , QSNOS, RZORS , QZORS, RVEGS , QVEGS
!    *,        RvetS , QvetS, RsotS , QsotS
!    *,        RCV, RCVB, RCVT, QCV, QCVB, QCVT
!    *,        RALBS, QALBS, RALFS, QALFS
!     print *, RTSFL, QTSFL, RAISL , QAISL
!    *,        RSNOL , QSNOL, RZORL , QZORL, RVEGL , QVEGL
!    *,        RvetL , QvetL, RsotL , QsotL
!    *,        RALBL, QALBL, RALFL, QALFL
!
!
      LEN_THREAD_M  = (LEN+NUM_THREADS-1) / NUM_THREADS
!
!$OMP PARALLEL DO PRIVATE(I1_T,I2_T,IT,I,K)
!!$OMP+PRIVATE(ALAMD,DENOM,RNUME,APHI,X,Y,WSUM,WSUMIV,SUM1,SUM2)
!
      DO IT=1,NUM_THREADS   ! START OF THREADED LOOP ...................
        I1_T       = (IT-1)*LEN_THREAD_M+1
        I2_T       = MIN(I1_T+LEN_THREAD_M-1,LEN)
!
      DO I=I1_T,I2_T
        IF(SLIANL(I).EQ.0.) THEN
!.... tsffc2 is the previous anomaly + today's climatology
!         TSFFC2 = (TSFFCS(I)-TSFAN2(I))+TSFANL(I)
!         TSFANL(I) = TSFFC2    *RTSFS+TSFANL(I)*QTSFS
!
          TSFANL(I) = TSFFCS(I)*RTSFS + TSFANL(I)*QTSFS
!         ALBANL(I) = ALBFCS(I)*RALBS + ALBANL(I)*QALBS
          AISANL(I) = AISFCS(I)*RAISS + AISANL(I)*QAISS
          SNOANL(I) = SNOFCS(I)*RSNOS + SNOANL(I)*QSNOS
          
          ZORANL(I) = ZORFCS(I)*RZORS + ZORANL(I)*QZORS
          VEGANL(I) = VEGFCS(I)*RVEGS + VEGANL(I)*QVEGS
          vetANL(I) = vetFCS(I)*RvetS + vetANL(I)*QvetS
          sotANL(I) = sotFCS(I)*RsotS + sotANL(I)*QsotS
!Cwu [+2L] add sih, sic
          SIHANL(I) = SIHFCS(I)*RSIHS + SIHANL(I)*QSIHS
          SICANL(I) = SICFCS(I)*RSICS + SICANL(I)*QSICS
!Clu [+4L] add vmn, vmx, slp, abs
          VMNANL(I) = VMNFCS(I)*RVMNS + VMNANL(I)*QVMNS
          VMXANL(I) = VMXFCS(I)*RVMXS + VMXANL(I)*QVMXS
          SLPANL(I) = SLPFCS(I)*RSLPS + SLPANL(I)*QSLPS
          ABSANL(I) = ABSFCS(I)*RABSS + ABSANL(I)*QABSS
!Cwu [+3L] add "SLIANL(I).GE.2" for sih, sic
!mi     ELSE IF(SLIANL(I).GE.2.) THEN
!mi       SIHANL(I) = SIHFCS(I)*RSIHS + SIHANL(I)*QSIHS
!mi       SICANL(I) = SICFCS(I)*RSICS + SICANL(I)*QSICS
        ELSE
          vetANL(I) = vetFCS(I)*RvetL + vetANL(I)*QvetL
          TSFANL(I) = TSFFCS(I)*RTSFL + TSFANL(I)*QTSFL
!         ALBANL(I) = ALBFCS(I)*RALBL + ALBANL(I)*QALBL
          AISANL(I) = AISFCS(I)*RAISL + AISANL(I)*QAISL
         IF(RSNOL.GE.0)THEN
          SNOANL(I) = SNOFCS(I)*RSNOL + SNOANL(I)*QSNOL
         ELSE
          IF(SNOANL(I).NE.0)THEN
           SNOANL(I) = MAX(-SNOANL(I)/RSNOL,
     &                 MIN(-SNOANL(I)*RSNOL, SNOFCS(I)))
          ENDIF
         ENDIF
          ZORANL(I) = ZORFCS(I)*RZORL + ZORANL(I)*QZORL
!cggg landice start
!cggg at landice points (vegetation type 13) set the
!cggg soil type, slope type and greenness fields to flag values.
!cggg otherwise, perform merging.
          IF (LANDICE           .AND. 
     &        SLIANL(I) == 1.0  .AND. 
     &        VETANL(I) == 13.0) THEN
            VEGANL(I) = 0.0
            SOTANL(I) = 9.0
            SLPANL(I) = 9.0
            VMNANL(I) = 0.0
            VMXANL(I) = 0.0
          ELSE
            VEGANL(I) = VEGFCS(I)*RVEGL + VEGANL(I)*QVEGL
            sotANL(I) = sotFCS(I)*RsotL + sotANL(I)*QsotL
            VMNANL(I) = VMNFCS(I)*RVMNL + VMNANL(I)*QVMNL
            VMXANL(I) = VMXFCS(I)*RVMXL + VMXANL(I)*QVMXL
            SLPANL(I) = SLPFCS(I)*RSLPL + SLPANL(I)*QSLPL
          END IF
!cggg landice end
          ABSANL(I) = ABSFCS(I)*RABSL + ABSANL(I)*QABSL
          SIHANL(I) = SIHFCS(I)*RSIHL + SIHANL(I)*QSIHL
          SICANL(I) = SICFCS(I)*RSICL + SICANL(I)*QSICL
        ENDIF
          CNPANL(I) = CNPFCS(I)*RCNP + CNPANL(I)*QCNP
!
!  snow over sea ice is cycled
!
        if(slianl(i).eq.2.) then
          snoanl(i) = snofcs(i)
        endif
      ENDDO
      DO I=I1_T,I2_T
        CVANL(I)  = CVFCS(I)*RCV   + CVANL(I)*QCV
        CVBANL(I) = CVBFCS(I)*RCVB + CVBANL(I)*QCVB
        CVTANL(I) = CVTFCS(I)*RCVT + CVTANL(I)*QCVT
      ENDDO
!
      DO K = 1, 4
        DO I=I1_T,I2_T
          IF(SLIANL(I).EQ.0.) THEN
            ALBANL(I,K) = ALBFCS(I,K)*RALBS + ALBANL(I,K)*QALBS
          ELSE
            ALBANL(I,K) = ALBFCS(I,K)*RALBL + ALBANL(I,K)*QALBL
          ENDIF
        ENDDO
      ENDDO
!
      DO K = 1, 2
        DO I=I1_T,I2_T
          IF(SLIANL(I).EQ.0.) THEN
            ALFANL(I,K) = ALFFCS(I,K)*RALFS + ALFANL(I,K)*QALFS
          ELSE
            ALFANL(I,K) = ALFFCS(I,K)*RALFL + ALFANL(I,K)*QALFL
          ENDIF
        ENDDO
      ENDDO
!
      DO K = 1, LSOIL
        DO I=I1_T,I2_T
          IF(SLIANL(I).EQ.0.) THEN
            SMCANL(I,K) = SMCFCS(I,K)*RSMCS(K) + SMCANL(I,K)*QSMCS(K)
            STCANL(I,K) = STCFCS(I,K)*RSTCS(K) + STCANL(I,K)*QSTCS(K)
          ELSE
!cggg landice start  soil moisture not used at landice points, so
!cggg don't bother merging it.  also, for now don't allow nudging
!cggg to raise subsurface temperature above freezing.
            STCANL(I,K) = STCFCS(I,K)*RSTCL(K) + STCANL(I,K)*QSTCL(K)
            IF (LANDICE .AND. SLIANL(I) == 1.0 .AND.
     &          VETANL(I) == 13.0) THEN
              SMCANL(I,K) = 1.0  ! use value as flag
              STCANL(I,K) = MIN(STCANL(I,K), 273.15)
            ELSE
              SMCANL(I,K) = SMCFCS(I,K)*RSMCL(K) + SMCANL(I,K)*QSMCL(K)
            END IF
!cggg landice end
          ENDIF
        ENDDO
      ENDDO
!
      ENDDO            ! END OF THREADED LOOP ...................
!$OMP END PARALLEL DO
      RETURN
      END
      SUBROUTINE NEWICE(SLIANL,SLIFCS,TSFANL,TSFFCS,LEN,LSOIL,
!Cwu [+1L] add SIHNEW,SICNEW,SIHANL,SICANL
     &                   SIHNEW,SICNEW,SIHANL,SICANL,      
     &                   ALBANL,SNOANL,ZORANL,SMCANL,STCANL,
     &                   ALBSEA,SNOSEA,ZORSEA,SMCSEA,SMCICE,
     &                   TSFMIN,TSFICE,ALBICE,ZORICE,TGICE,
     &                   RLA,RLO,me)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      real (kind=kind_io8), parameter :: one=1.0
      REAL (KIND=KIND_IO8) tgice,albice,zorice,tsfice,albsea,snosea,
     &                     smcice,tsfmin,zorsea,smcsea
!Cwu [+1L] add sicnew,sihnew
     &,                    sicnew,sihnew   
      integer i,me,kount1,kount2,k,len,lsoil
      REAL (KIND=KIND_IO8) SLIANL(LEN),   SLIFCS(LEN),
     &                     TSFFCS(LEN),TSFANL(LEN)
      REAL (KIND=KIND_IO8) ALBANL(LEN,4), SNOANL(LEN), ZORANL(LEN)
      REAL (KIND=KIND_IO8) SMCANL(LEN,LSOIL), STCANL(LEN,LSOIL)
!Cwu [+1L] add sihanl & sicanl
      REAL (KIND=KIND_IO8) SIHANL(LEN), SICANL(LEN)
!
      REAL (KIND=KIND_IO8) RLA(LEN), RLO(LEN)
!
      if (me .eq. 0) WRITE(6,*) 'NEWICE'
!
      KOUNT1 = 0
      KOUNT2 = 0
      DO I=1,LEN
        IF(SLIFCS(I).NE.SLIANL(I)) THEN
          IF(SLIFCS(I).EQ.1..OR.SLIANL(I).EQ.1.) THEN
            PRINT *,'INCONSISTENCY IN SLIFCS OR SLIANL'
            PRINT 910,RLA(I),RLO(I),SLIFCS(I),SLIANL(I),
     &                TSFFCS(I),TSFANL(I)
  910       FORMAT(2X,'AT LAT=',F5.1,' LON=',F5.1,' SLIFCS=',F4.1,
     &          ' SLIMSK=',F4.1,' TSFFCS=',F5.1,' SET TO TSFANL=',F5.1)
            CALL ABORT
          ENDIF
!
!  INTERPOLATED CLIMATOLOGY INDICATES MELTED SEA ICE
!
          IF(SLIANL(I).EQ.0..AND.SLIFCS(I).EQ.2.) THEN
            TSFANL(I)   = TSFMIN
            ALBANL(I,1) = ALBSEA
            ALBANL(I,2) = ALBSEA
            ALBANL(I,3) = ALBSEA
            ALBANL(I,4) = ALBSEA
            SNOANL(I)   = SNOSEA
            ZORANL(I)   = ZORSEA
            DO K = 1, LSOIL
              SMCANL(I,K) = SMCSEA
!Cwu [+1L] set STCANL to TGICE (over SEA-ICE)
              STCANL(I,K) = TGICE 
            ENDDO
!Cwu [+2L] set siganl and sicanl
            SIHANL(I) = 0.
            SICANL(I) = 0.
            KOUNT1 = KOUNT1 + 1
          ENDIF
!
!  INTERPLATED CLIMATOLOYG/ANALYSIS INDICATES NEW SEA ICE
!
          IF(SLIANL(I).EQ.2..AND.SLIFCS(I).EQ.0.) THEN
            TSFANL(I)   = TSFICE
            ALBANL(I,1) = ALBICE
            ALBANL(I,2) = ALBICE
            ALBANL(I,3) = ALBICE
            ALBANL(I,4) = ALBICE
            SNOANL(I)   = 0.
            ZORANL(I)   = ZORICE
            DO K = 1, LSOIL
              SMCANL(I,K) = SMCICE
              STCANL(I,K) = TGICE
            ENDDO
!Cwu [+2L] add SIHANL & SICANL
            SIHANL(I) = SIHNEW
            SICANL(I) = min(one, max(SICNEW,SICANL(i)))
            KOUNT2 = KOUNT2 + 1
          ENDIF
        ENDIF
      ENDDO
!
      if (me .eq. 0) then
      IF(KOUNT1.GT.0) THEN
        WRITE(6,*) 'Sea ice melted.  TSF,ALB,ZOR are filled',
     &             ' at ',KOUNT1,' points'
      ENDIF
      IF(KOUNT2.GT.0) THEN
        WRITE(6,*) 'Sea ice formed.  TSF,ALB,ZOR are filled',
     &             ' at ',KOUNT2,' points'
      ENDIF
      endif
!
      RETURN
      END
!cggg landice mods start
!      SUBROUTINE QCSNOW(SNOANL,SLMASK,AISANL,GLACIR,LEN,SNOVAL,me)
      SUBROUTINE QCSNOW(SNOANL,SLMASK,AISANL,GLACIR,LEN,SNOVAL,
     &                  LANDICE,me)
!cggg landice mods end
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer kount,i,len,me
!cggg landice mods start
      LOGICAL, INTENT(IN)  :: LANDICE
!cggg landice mods end
      REAL (KIND=KIND_IO8) per,snoval
      REAL (KIND=KIND_IO8) SNOANL(LEN),SLMASK(LEN),
     &                     AISANL(LEN),GLACIR(LEN)
      if (me .eq. 0) then
        WRITE(6,*) ' '
        WRITE(6,*) 'QC of SNOW'
      endif
!cggg landice mods start
      IF (.NOT.LANDICE) THEN
!cggg landice mods end
        KOUNT=0
        DO I=1,LEN
          IF(GLACIR(I).NE.0..AND.SNOANL(I).EQ.0.) THEN
!         IF(GLACIR(I).NE.0..AND.SNOANL(I).LT.SNOVAL*0.5) THEN
            SNOANL(I) = SNOVAL
            KOUNT     = KOUNT + 1
          ENDIF
        ENDDO
        PER = FLOAT(KOUNT) / FLOAT(LEN)*100.
        IF(KOUNT.GT.0) THEN
          if (me .eq. 0) then
          PRINT *,'SNOW filled over glacier points at ',KOUNT,
     &            ' POINTS (',PER,'percent)'
          endif
        ENDIF
!cggg landice mods start
      ENDIF ! LANDICE CHECK
!cggg landice mods end
      KOUNT = 0
      DO I=1,LEN
        IF(SLMASK(I).EQ.0.AND.AISANL(I).EQ.0) THEN
          SNOANL(I) = 0.
          KOUNT     = KOUNT + 1
        ENDIF
      ENDDO
      PER = FLOAT(KOUNT) / FLOAT(LEN)*100.
      IF(KOUNT.GT.0) THEN
        if (me .eq. 0) then
        PRINT *,'SNOW set to zero over open sea at ',KOUNT,
     &          ' POINTS (',PER,'percent)'
        endif
      ENDIF
      RETURN
      END
      SUBROUTINE QCSICE(AIS,GLACIR,AMXICE,AICICE,AICSEA,SLLND,SLMASK,
     &                  RLA,RLO,LEN,me)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer kount1,kount,i,me,len
      REAL (KIND=KIND_IO8) per,aicsea,aicice,sllnd
!
      REAL (KIND=KIND_IO8) AIS(LEN), GLACIR(LEN),
     &                     AMXICE(LEN), SLMASK(LEN)
      REAL (KIND=KIND_IO8) RLA(LEN), RLO(LEN)
!
!  CHECK SEA-ICE COVER MASK AGAINST LAND-SEA MASK
!
      if (me .eq. 0) WRITE(6,*) 'QC of sea ice'
      KOUNT  = 0
      KOUNT1 = 0
      DO I=1,LEN
        IF(AIS(I).NE.AICICE.AND.AIS(I).NE.AICSEA) THEN
          PRINT *,'SEA ICE MASK NOT ',AICICE,' OR ',AICSEA
          PRINT *,'AIS(I),AICICE,AICSEA,RLA(I),RLO(I,=',
     &             AIS(I),AICICE,AICSEA,RLA(I),RLO(I)
          CALL ABORT
        ENDIF
        IF(SLMASK(I).EQ.0..AND.GLACIR(I).EQ.1..AND.
!       IF(SLMASK(I).EQ.0..AND.GLACIR(I).EQ.2..AND.
     &     AIS(I).NE.1.) THEN
          KOUNT1 = KOUNT1 + 1
          AIS(I) = 1.
        ENDIF
        IF(SLMASK(I).EQ.SLLND.AND.AIS(I).EQ.AICICE) THEN
          KOUNT  = KOUNT + 1
          AIS(I) = AICSEA
        ENDIF
      ENDDO
!     ENDDO
      PER = FLOAT(KOUNT) / FLOAT(LEN)*100.
      IF(KOUNT.GT.0) THEN
        if(me .eq. 0) then
        PRINT *,' Sea ice over land mask at ',KOUNT,' points (',PER,
     &          'percent)'
        endif
      ENDIF
      PER = FLOAT(KOUNT1) / FLOAT(LEN)*100.
      IF(KOUNT1.GT.0) THEN
        if(me .eq. 0) then
        PRINT *,' Sea ice set over glacier points over ocean at ',
     &          KOUNT1,' points (',PER,'percent)'
        endif
      ENDIF
!     KOUNT=0
!     DO J=1,JDIM
!     DO I=1,IDIM
!       IF(AMXICE(I,J).NE.0..AND.AIS(I,J).EQ.0.) THEN
!         AIS(I,J)=0.
!         KOUNT=KOUNT+1
!       ENDIF
!     ENDDO
!     ENDDO
!     PER=FLOAT(KOUNT)/FLOAT(IDIM*JDIM)*100.
!     IF(KOUNT.GT.0) THEN
!       PRINT *,' Sea ice exceeds maxice at ',KOUNT,' points (',PER,
!    &          'percent)'
!     ENDIF
!
!  Remove isolated open ocean surrounded by sea ice and/or land
!
!  Remove isolated open ocean surrounded by sea ice and/or land
!
!     IJ = 0
!     DO J=1,JDIM
!       DO I=1,IDIM
!         IJ = IJ + 1
!         IP = I  + 1
!         IM = I  - 1
!         JP = J  + 1
!         JM = J  - 1
!         IF(JP.GT.JDIM) JP = JDIM - 1
!         IF(JM.LT.1)    JM = 2
!         IF(IP.GT.IDIM) IP = 1
!         IF(IM.LT.1)    IM = IDIM
!         IF(SLMASK(I,J).EQ.0..AND.AIS(I,J).EQ.0.) THEN
!           IF((SLMASK(IP,JP).EQ.1..OR.AIS(IP,JP).EQ.1.).AND.
!    &         (SLMASK(I ,JP).EQ.1..OR.AIS(I ,JP).EQ.1.).AND.
!    &         (SLMASK(IM,JP).EQ.1..OR.AIS(IM,JP).EQ.1.).AND.
!    &         (SLMASK(IP,J ).EQ.1..OR.AIS(IP,J ).EQ.1.).AND.
!    &         (SLMASK(IM,J ).EQ.1..OR.AIS(IM,J ).EQ.1.).AND.
!    &         (SLMASK(IP,JM).EQ.1..OR.AIS(IP,JM).EQ.1.).AND.
!    &         (SLMASK(I ,JM).EQ.1..OR.AIS(I ,JM).EQ.1.).AND.
!    &         (SLMASK(IM,JM).EQ.1..OR.AIS(IM,JM).EQ.1.)) THEN
!               AIS(I,J) = 1.
!             WRITE(6,*) ' Isolated open sea point surrounded by',
!    &                   ' sea ice or land modified to sea ice',
!    &                   ' at LAT=',RLA(I,J),' LON=',RLO(I,J)
!           ENDIF
!         ENDIF
!       ENDDO
!     ENDDO
      RETURN
      END
      SUBROUTINE SETLSI(SLMASK,AISFLD,LEN,AICICE,SLIFLD)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,len
      REAL (KIND=KIND_IO8) aicice
      REAL (KIND=KIND_IO8) SLMASK(LEN), SLIFLD(LEN), AISFLD(LEN)
!
!  Set surface condition indicator slimsk
!
      DO I=1,LEN
        SLIFLD(I) = SLMASK(I)
!       IF(AISFLD(I).EQ.AICICE) SLIFLD(I) = 2.0
        IF(AISFLD(I).EQ.AICICE .AND. SLMASK(I) .EQ. 0.0)
     &                                SLIFLD(I) = 2.0
      ENDDO
      RETURN
      END
      SUBROUTINE SCALE(FLD,LEN,SCL)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,len
      REAL (KIND=KIND_IO8) FLD(LEN),scl
      DO I=1,LEN
        FLD(I) = FLD(I) * SCL
      ENDDO
      RETURN
      END
      SUBROUTINE QCMXMN(TTL,FLD,SLIMSK,SNO,ICEFLG,
     &                  FLDLMX,FLDLMN,FLDOMX,FLDOMN,FLDIMX,FLDIMN,
     &                  FLDJMX,FLDJMN,FLDSMX,FLDSMN,EPSFLD,
     &                  RLA,RLO,LEN,MODE,PERCRIT,LGCHEK,me)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      REAL (KIND=KIND_IO8) permax,per,fldimx,fldimn,fldjmx,fldomn,
     &                     fldlmx,fldlmn,fldomx,fldjmn,percrit,
     &                     fldsmx,fldsmn,epsfld
      integer kmaxi,kmini,kmaxj,kmino,kmaxl,kminl,kmaxo,mmprt,kminj,
     &        ij,nprt,kmaxs,kmins,i,me,len,mode
      PARAMETER(MMPRT=2)
!
      CHARACTER*8 TTL
      logical iceflg(LEN)
      REAL (KIND=KIND_IO8) FLD(LEN),SLIMSK(LEN),SNO(LEN),
     &                     RLA(LEN), RLO(LEN)
      INTEGER IWK(LEN)
      LOGICAL LGCHEK
!
      logical first
      integer   NUM_THREADS
      data first /.true./
      save NUM_THREADS, first
!
      integer LEN_THREAD_M, I1_T, I2_T, IT
      integer NUM_PARTHDS
!
      if (first) then
         NUM_THREADS = NUM_PARTHDS()
         first = .false.
      endif
!
!  CHECK AGAINST LAND-SEA MASK AND ICE COVER MASK
!
      if(me .eq. 0) then
!     PRINT *,' '
      PRINT *,'Performing QC of ',TTL,' MODE=',MODE,
     &        '(0=count only, 1=replace)'
      endif
!
      LEN_THREAD_M  = (LEN+NUM_THREADS-1) / NUM_THREADS
!
!$OMP PARALLEL DO PRIVATE(I1_T,I2_T,IT,I)
!$OMP+PRIVATE(nprt,ij,iwk,KMAXS,KMINS)
!$OMP+PRIVATE(KMAXL,KMINL,KMAXO,KMINO,KMAXI,KMINI,KMAXJ,KMINJ)
!$OMP+SHARED(mode,epsfld)
!$OMP+SHARED(fldlmx,fldlmn,fldomx,fldjmn,fldsmx,fldsmn)
!$OMP+SHARED(fld,slimsk,sno,rla,rlo)
!
      DO IT=1,NUM_THREADS   ! START OF THREADED LOOP ...................
        I1_T       = (IT-1)*LEN_THREAD_M+1
        I2_T       = MIN(I1_T+LEN_THREAD_M-1,LEN)
!
        KMAXL = 0
        KMINL = 0
        KMAXO = 0
        KMINO = 0
        KMAXI = 0
        KMINI = 0
        KMAXJ = 0
        KMINJ = 0
        KMAXS = 0
        KMINS = 0
!
!
!  Lower bound check over bare land
!
        IF (FLDLMN .NE. 999.0) THEN
          DO I=I1_T,I2_T
            IF(SLIMSK(I).EQ.1..AND.SNO(I).LE.0..AND.
     &         FLD(I).LT.FLDLMN-EPSFLD) THEN
               KMINL=KMINL+1
               IWK(KMINL) = I
            ENDIF
          ENDDO
          if(me == 0 . and. it == 1 .and. NUM_THREADS == 1) then
            NPRT = MIN(MMPRT,KMINL)
            DO I=1,NPRT
              IJ = IWK(I)
              PRINT 8001,RLA(IJ),RLO(IJ),FLD(IJ),FLDLMN
 8001         FORMAT(' Bare land min. check. LAT=',F5.1,
     &             ' LON=',F6.1,' FLD=',E13.6, ' to ',E13.6)
            ENDDO
          endif
          IF (MODE .EQ. 1) THEN
            DO I=1,KMINL
              FLD(IWK(I)) = FLDLMN
            ENDDO
          ENDIF
        ENDIF
!
!  Upper bound check over bare land
!
        IF (FLDLMX .NE. 999.0) THEN
          DO I=I1_T,I2_T
            IF(SLIMSK(I).EQ.1..AND.SNO(I).LE.0..AND.
     &         FLD(I).GT.FLDLMX+EPSFLD) THEN
               KMAXL=KMAXL+1
               IWK(KMAXL) = I
            ENDIF
          ENDDO
          if(me == 0 . and. it == 1 .and. NUM_THREADS == 1) then
            NPRT = MIN(MMPRT,KMAXL)
            DO I=1,NPRT
              IJ = IWK(I)
              PRINT 8002,RLA(IJ),RLO(IJ),FLD(IJ),FLDLMX
 8002         FORMAT(' Bare land max. check. LAT=',F5.1,
     &             ' LON=',F6.1,' FLD=',E13.6, ' to ',E13.6)
            ENDDO
          endif
          IF (MODE .EQ. 1) THEN
            DO I=1,KMAXL
              FLD(IWK(I)) = FLDLMX
            ENDDO
          ENDIF
        ENDIF
!
!  Lower bound check over snow covered land
!
        IF (FLDSMN .NE. 999.0) THEN
          DO I=I1_T,I2_T
            IF(SLIMSK(I).EQ.1..AND.SNO(I).GT.0..AND.
     &         FLD(I).LT.FLDSMN-EPSFLD) THEN
               KMINS=KMINS+1
               IWK(KMINS) = I
            ENDIF
          ENDDO
          if(me == 0 . and. it == 1 .and. NUM_THREADS == 1) then
            NPRT = MIN(MMPRT,KMINS)
            DO I=1,NPRT
              IJ = IWK(I)
              PRINT 8003,RLA(IJ),RLO(IJ),FLD(IJ),FLDSMN
 8003         FORMAT(' Sno covrd land min. check. LAT=',F5.1,
     &             ' LON=',F6.1,' FLD=',E11.4, ' to ',E11.4)
            ENDDO
          endif
          IF (MODE .EQ. 1) THEN
            DO I=1,KMINS
              FLD(IWK(I)) = FLDSMN
            ENDDO
          ENDIF
        ENDIF
!
!  Upper bound check over snow covered land
!
        IF (FLDSMX .NE. 999.0) THEN
          DO I=I1_T,I2_T
            IF(SLIMSK(I).EQ.1..AND.SNO(I).GT.0..AND.
     &         FLD(I).GT.FLDSMX+EPSFLD) THEN
               KMAXS=KMAXS+1
               IWK(KMAXS) = I
            ENDIF
          ENDDO
          if(me == 0 . and. it == 1 .and. NUM_THREADS == 1) then
            NPRT = MIN(MMPRT,KMAXS)
            DO I=1,NPRT
              IJ = IWK(I)
              PRINT 8004,RLA(IJ),RLO(IJ),FLD(IJ),FLDSMX
 8004         FORMAT(' Snow land max. check. LAT=',F5.1,
     &             ' LON=',F6.1,' FLD=',E11.4, ' to ',E11.4)
            ENDDO
          endif
          IF (MODE .EQ. 1) THEN
            DO I=1,KMAXS
              FLD(IWK(I)) = FLDSMX
            ENDDO
          ENDIF
        ENDIF
!
!  Lower bound check over open ocean
!
        IF (FLDOMN .NE. 999.0) THEN
          DO I=I1_T,I2_T
            IF(SLIMSK(I).EQ.0..AND.
     &         FLD(I).LT.FLDOMN-EPSFLD) THEN
               KMINO=KMINO+1
               IWK(KMINO) = I
            ENDIF
          ENDDO
          if(me == 0 . and. it == 1 .and. NUM_THREADS == 1) then
            NPRT = MIN(MMPRT,KMINO)
            DO I=1,NPRT
              IJ = IWK(I)
              PRINT 8005,RLA(IJ),RLO(IJ),FLD(IJ),FLDOMN
 8005         FORMAT(' Open ocean min. check. LAT=',F5.1,
     &             ' LON=',F6.1,' FLD=',E11.4,' to ',E11.4)
            ENDDO
          endif
          IF (MODE .EQ. 1) THEN
            DO I=1,KMINO
              FLD(IWK(I)) = FLDOMN
            ENDDO
          ENDIF
      ENDIF
!
!  Upper bound check over open ocean
!
        IF (FLDOMX .NE. 999.0) THEN
          DO I=I1_T,I2_T
            IF(FLDOMX.NE.999..AND.SLIMSK(I).EQ.0..AND.
     &         FLD(I).GT.FLDOMX+EPSFLD) THEN
               KMAXO=KMAXO+1
               IWK(KMAXO) = I
            ENDIF
          ENDDO
          if(me == 0 . and. it == 1 .and. NUM_THREADS == 1) then
            NPRT = MIN(MMPRT,KMAXO)
            DO I=1,NPRT
              IJ = IWK(I)
              PRINT 8006,RLA(IJ),RLO(IJ),FLD(IJ),FLDOMX
 8006         FORMAT(' Open ocean max. check. LAT=',F5.1,
     &             ' LON=',F6.1,' FLD=',E11.4, ' to ',E11.4)
            ENDDO
          endif
          IF (MODE .EQ. 1) THEN
            DO I=1,KMAXO
              FLD(IWK(I)) = FLDOMX
            ENDDO
          ENDIF
        ENDIF
!
!  Lower bound check over sea ice without snow
!
        IF (FLDIMN .NE. 999.0) THEN
          DO I=I1_T,I2_T
            IF(SLIMSK(I).EQ.2..AND.SNO(I).LE.0..AND.
     &         FLD(I).LT.FLDIMN-EPSFLD) THEN
               KMINI=KMINI+1
               IWK(KMINI) = I
            ENDIF
          ENDDO
          if(me == 0 . and. it == 1 .and. NUM_THREADS == 1) then
            NPRT = MIN(MMPRT,KMINI)
            DO I=1,NPRT
              IJ = IWK(I)
              PRINT 8007,RLA(IJ),RLO(IJ),FLD(IJ),FLDIMN
 8007         FORMAT(' Seaice no snow min. check LAT=',F5.1,
     &             ' LON=',F6.1,' FLD=',E11.4, ' to ',E11.4)
            ENDDO
          endif
          IF (MODE .EQ. 1) THEN
            DO I=1,KMINI
              FLD(IWK(I)) = FLDIMN
            ENDDO
          ENDIF
        ENDIF
!
!  Upper bound check over sea ice without snow
!
        IF (FLDIMX .NE. 999.0) THEN
          DO I=I1_T,I2_T
            IF(SLIMSK(I).EQ.2..AND.SNO(I).LE.0..AND.
     &         FLD(I).GT.FLDIMX+EPSFLD  .AND. ICEFLG(I)) THEN
!    &         FLD(I).GT.FLDIMX+EPSFLD) THEN
               KMAXI=KMAXI+1
               IWK(KMAXI) = I
            ENDIF
          ENDDO
          if(me == 0 . and. it == 1 .and. NUM_THREADS == 1) then
            NPRT = MIN(MMPRT,KMAXI)
            DO I=1,NPRT
              IJ = IWK(I)
              PRINT 8008,RLA(IJ),RLO(IJ),FLD(IJ),FLDIMX
 8008         FORMAT(' Seaice no snow max. check LAT=',F5.1,
     &             ' LON=',F6.1,' FLD=',E11.4, ' to ',E11.4)
            ENDDO
          endif
          IF (MODE .EQ. 1) THEN
            DO I=1,KMAXI
              FLD(IWK(I)) = FLDIMX
            ENDDO
          ENDIF
        ENDIF
!
!  Lower bound check over sea ice with snow
!
        IF (FLDJMN .NE. 999.0) THEN
          DO I=I1_T,I2_T
            IF(SLIMSK(I).EQ.2..AND.SNO(I).GT.0..AND.
     &         FLD(I).LT.FLDJMN-EPSFLD) THEN
               KMINJ=KMINJ+1
               IWK(KMINJ) = I
            ENDIF
          ENDDO
          if(me == 0 . and. it == 1 .and. NUM_THREADS == 1) then
            NPRT = MIN(MMPRT,KMINJ)
            DO I=1,NPRT
              IJ = IWK(I)
              PRINT 8009,RLA(IJ),RLO(IJ),FLD(IJ),FLDJMN
 8009         FORMAT(' Sea ice snow min. check LAT=',F5.1,
     &             ' LON=',F6.1,' FLD=',E11.4, ' to ',E11.4)
            ENDDO
          endif
          IF (MODE .EQ. 1) THEN
            DO I=1,KMINJ
              FLD(IWK(I)) = FLDJMN
            ENDDO
          ENDIF
        ENDIF
!
!  Upper bound check over sea ice with snow
!
        IF (FLDJMX .NE. 999.0) THEN
          DO I=I1_T,I2_T
            IF(SLIMSK(I).EQ.2..AND.SNO(I).GT.0..AND.
     &         FLD(I).GT.FLDJMX+EPSFLD  .AND. ICEFLG(I)) THEN
!    &         FLD(I).GT.FLDJMX+EPSFLD) THEN
               KMAXJ=KMAXJ+1
               IWK(KMAXJ) = I
            ENDIF
          ENDDO
          if(me == 0 . and. it == 1 .and. NUM_THREADS == 1) then
            NPRT = MIN(MMPRT,KMAXJ)
            DO I=1,NPRT
              IJ = IWK(I)
              PRINT 8010,RLA(IJ),RLO(IJ),FLD(IJ),FLDJMX
 8010         FORMAT(' Seaice snow max check LAT=',F5.1,
     &             ' LON=',F6.1,' FLD=',E11.4, ' to ',E11.4)
            ENDDO
          endif
          IF (MODE .EQ. 1) THEN
            DO I=1,KMAXJ
              FLD(IWK(I)) = FLDJMX
            ENDDO
          ENDIF
        ENDIF
      ENDDO            ! END OF THREADED LOOP ...................
!$OMP END PARALLEL DO
!
!  Print results
!
      if(me .eq. 0) then
!     WRITE(6,*) 'SUMMARY OF QC'
      PERMAX=0.
      IF(KMINL.GT.0) THEN
        PER=FLOAT(KMINL)/FLOAT(LEN)*100.
        PRINT 9001,FLDLMN,KMINL,PER
 9001   FORMAT(' Bare land min check.  Modified to ',F8.1,
     &         ' at ',I5,' points ',F8.1,'percent')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMAXL.GT.0) THEN
        PER=FLOAT(KMAXL)/FLOAT(LEN)*100.
        PRINT 9002,FLDLMX,KMAXL,PER
 9002   FORMAT(' Bare land max check. Modified to ',F8.1,
     &         ' at ',I5,' points ',F4.1,'percent')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMINO.GT.0) THEN
        PER=FLOAT(KMINO)/FLOAT(LEN)*100.
        PRINT 9003,FLDOMN,KMINO,PER
 9003   FORMAT(' Open ocean min check.  Modified to ',F8.1,
     &         ' at ',I5,' points ',F4.1,'percent')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMAXO.GT.0) THEN
        PER=FLOAT(KMAXO)/FLOAT(LEN)*100.
        PRINT 9004,FLDOMX,KMAXO,PER
 9004   FORMAT(' Open sea max check. Modified to ',F8.1,
     &         ' at ',I5,' points ',F4.1,'percent')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMINS.GT.0) THEN
        PER=FLOAT(KMINS)/FLOAT(LEN)*100.
        PRINT 9009,FLDSMN,KMINS,PER
 9009   FORMAT(' Snow covered land min check. Modified to ',F8.1,
     &         ' at ',I5,' points ',F4.1,'percent')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMAXS.GT.0) THEN
        PER=FLOAT(KMAXS)/FLOAT(LEN)*100.
        PRINT 9010,FLDSMX,KMAXS,PER
 9010   FORMAT(' Snow covered land max check. Modified to ',F8.1,
     &         ' at ',I5,' points ',F4.1,'percent')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMINI.GT.0) THEN
        PER=FLOAT(KMINI)/FLOAT(LEN)*100.
        PRINT 9005,FLDIMN,KMINI,PER
 9005   FORMAT(' Bare ice min check.  Modified to ',F8.1,
     &         ' at ',I5,' points ',F4.1,'percent')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMAXI.GT.0) THEN
        PER=FLOAT(KMAXI)/FLOAT(LEN)*100.
        PRINT 9006,FLDIMX,KMAXI,PER
 9006   FORMAT(' Bare ice max check. Modified to ',F8.1,
     &         ' at ',I5,' points ',F4.1,'percent')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMINJ.GT.0) THEN
        PER=FLOAT(KMINJ)/FLOAT(LEN)*100.
        PRINT 9007,FLDJMN,KMINJ,PER
 9007   FORMAT(' Snow covered ice min check.  Modified to ',F8.1,
     &         ' at ',I5,' points ',F4.1,'percent')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMAXJ.GT.0) THEN
        PER=FLOAT(KMAXJ)/FLOAT(LEN)*100.
        PRINT 9008,FLDJMX,KMAXJ,PER
 9008   FORMAT(' Snow covered ice max check. Modified to ',F8.1,
     &         ' at ',I5,' points ',F4.1,'percent')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
!     Commented on 06/30/99  -- Moorthi
!     IF(LGCHEK) THEN
!       IF(PERMAX.GT.PERCRIT) THEN
!         WRITE(6,*) ' Too many bad points.  Aborting ....'
!         CALL ABORT
!       ENDIF
!     ENDIF
!
      endif
!
      RETURN
      END
      SUBROUTINE SETZRO(FLD,EPS,LEN)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,len
      REAL (KIND=KIND_IO8) FLD(LEN),eps
      DO I=1,LEN
        IF(ABS(FLD(I)).LT.EPS) FLD(I) = 0.
      ENDDO
      RETURN
      END
      SUBROUTINE GETSCV(SNOFLD,SCVFLD,LEN)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,len
      REAL (KIND=KIND_IO8) SNOFLD(LEN),SCVFLD(LEN)
!
      DO I=1,LEN
        SCVFLD(I) = 0.
        IF(SNOFLD(I).GT.0.) SCVFLD(I) = 1.
      ENDDO
      RETURN
      END
      SUBROUTINE GETSTC(TSFFLD,TG3FLD,SLIFLD,LEN,LSOIL,STCFLD,TSFIMX)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer k,i,len,lsoil
      REAL (KIND=KIND_IO8) factor,tsfimx
      REAL (KIND=KIND_IO8) TSFFLD(LEN), TG3FLD(LEN), SLIFLD(LEN)
      REAL (KIND=KIND_IO8) STCFLD(LEN,LSOIL)
!
!  Layer Soil temperature
!
      DO K = 1, LSOIL
        DO I = 1, LEN
          IF(SLIFLD(I).EQ.1.0) THEN
            FACTOR = ((K-1) * 2 + 1) / (2. * LSOIL)
            STCFLD(I,K) = FACTOR*TG3FLD(I)+(1.-FACTOR)*TSFFLD(I)
          ELSEIF(SLIFLD(I).EQ.2.0) THEN
            FACTOR = ((K-1) * 2 + 1) / (2. * LSOIL)
            STCFLD(I,K) = FACTOR*TSFIMX+(1.-FACTOR)*TSFFLD(I)
          ELSE
            STCFLD(I,K) = TG3FLD(I)
          ENDIF
        ENDDO
      ENDDO
      IF(LSOIL.GT.2) THEN
        DO K = 3, LSOIL
          DO I = 1, LEN
            STCFLD(I,K) = STCFLD(I,2)
          ENDDO
        ENDDO
      ENDIF
      RETURN
      END
      SUBROUTINE GETSMC(WETFLD,LEN,LSOIL,SMCFLD,me)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer k,i,len,lsoil,me
      REAL (KIND=KIND_IO8) WETFLD(LEN), SMCFLD(LEN,LSOIL)
!
      if (me .eq. 0) WRITE(6,*) 'GETSMC'
!
!  Layer Soil wetness
!
      DO K = 1, LSOIL
        DO I = 1, LEN
          SMCFLD(I,K) = (WETFLD(I)*1000./150.)*.37 + .1
        ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE USESGT(SIG1T,SLIANL,TG3ANL,LEN,LSOIL,TSFANL,STCANL,
     &                  TSFIMX)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,len,lsoil
      REAL (KIND=KIND_IO8) tsfimx
      REAL (KIND=KIND_IO8) SIG1T(LEN), SLIANL(LEN), TG3ANL(LEN)
      REAL (KIND=KIND_IO8) TSFANL(LEN), STCANL(LEN,LSOIL)
!
!  Soil temperature
!
      IF(SIG1T(1).GT.0.) THEN
        DO I=1,LEN
          IF(SLIANL(I).NE.0.) THEN
            TSFANL(I) = SIG1T(I)
          ENDIF
        ENDDO
      ENDIF
      CALL GETSTC(TSFANL,TG3ANL,SLIANL,LEN,LSOIL,STCANL,TSFIMX)
!
      RETURN
      END
      SUBROUTINE SNOSFC(SNOANL,TSFANL,TSFSMX,LEN,me)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer kount,i,len,me
      REAL (KIND=KIND_IO8) per,tsfsmx
      REAL (KIND=KIND_IO8) SNOANL(LEN), TSFANL(LEN)
!
      if (me .eq. 0) WRITE(6,*) 'Set snow temp to TSFSMX if greater'
      KOUNT=0
      DO I=1,LEN
        IF(SNOANL(I).GT.0.) THEN
          IF(TSFANL(I).GT.TSFSMX) TSFANL(I)=TSFSMX
          KOUNT = KOUNT + 1
        ENDIF
      ENDDO
      IF(KOUNT.GT.0) THEN
        if(me .eq. 0) then
        PER=FLOAT(KOUNT)/FLOAT(LEN)*100.
        WRITE(6,*) 'Snow sfc.  TSF set to ',TSFSMX,' at ',
     &              KOUNT, ' POINTS ',PER,'percent'
        endif
      ENDIF
      RETURN
      END
      SUBROUTINE ALBOCN(ALBCLM,SLMASK,ALBOMX,LEN)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,len
      REAL (KIND=KIND_IO8) albomx
      REAL (KIND=KIND_IO8) ALBCLM(LEN,4), SLMASK(LEN)
      DO I=1,LEN
        IF(SLMASK(I).EQ.0) THEN
          ALBCLM(I,1) = ALBOMX
          ALBCLM(I,2) = ALBOMX
          ALBCLM(I,3) = ALBOMX
          ALBCLM(I,4) = ALBOMX
        ENDIF
      ENDDO
      RETURN
      END
      SUBROUTINE QCMXICE(GLACIR,AMXICE,LEN,me)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,kount,len,me
      REAL (KIND=KIND_IO8) GLACIR(LEN),AMXICE(LEN),per
      if (me .eq. 0) WRITE(6,*) 'QC of maximum ice extent'
      KOUNT=0
      DO I=1,LEN
        IF(GLACIR(I).EQ.1..AND.AMXICE(I).EQ.0.) THEN
          AMXICE(I) = 0.
          KOUNT     = KOUNT + 1
        ENDIF
      ENDDO
      IF(KOUNT.GT.0) THEN
        PER = FLOAT(KOUNT) / FLOAT(LEN)*100.
        if(me .eq. 0) WRITE(6,*) ' Max ice limit less than glacier'
     &,            ' coverage at ', KOUNT, ' POINTS ',PER,'percent'
      ENDIF
      RETURN
      END
      SUBROUTINE QCSLI(SLIANL,SLIFCS,LEN,me)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,kount,len,me
      REAL (KIND=KIND_IO8) SLIANL(LEN), SLIFCS(LEN),per
      if (me .eq. 0) then
      WRITE(6,*) ' '
      WRITE(6,*) 'QCSLI'
      endif
      KOUNT=0
      DO I=1,LEN
        IF(SLIANL(I).EQ.1..AND.SLIFCS(I).EQ.0.) THEN
          KOUNT      = KOUNT + 1
          SLIFCS(I) = 1.
        ENDIF
        IF(SLIANL(I).EQ.0..AND.SLIFCS(I).EQ.1.) THEN
          KOUNT      = KOUNT + 1
          SLIFCS(I) = 0.
        ENDIF
        IF(SLIANL(I).EQ.2..AND.SLIFCS(I).EQ.1.) THEN
          KOUNT      = KOUNT + 1
          SLIFCS(I) = 0.
        ENDIF
        IF(SLIANL(I).EQ.1..AND.SLIFCS(I).EQ.2.) THEN
          KOUNT      = KOUNT + 1
          SLIFCS(I) = 1.
        ENDIF
      ENDDO
      IF(KOUNT.GT.0) THEN
        PER=FLOAT(KOUNT)/FLOAT(LEN)*100.
        if(me .eq. 0) then
        WRITE(6,*) ' Inconsistency of SLMASK between forecast and',
     &             ' analysis corrected at ',KOUNT, ' POINTS ',PER,
     &             'percent'
        endif
      ENDIF
      RETURN
      END
!     SUBROUTINE NNTPRT(DATA,IMAX,FACT)
!     REAL (KIND=KIND_IO8) DATA(IMAX)
!     ILAST=0
!     I1=1
!     I2=80
!1112 CONTINUE
!     IF(I2.GE.IMAX) THEN
!       ILAST=1
!       I2=IMAX
!     ENDIF
!     WRITE(6,*) ' '
!     DO J=1,JMAX
!       WRITE(6,1111) (NINT(DATA(IMAX*(J-1)+I)*FACT),I=I1,I2)
!     ENDDO
!     IF(ILAST.EQ.1) RETURN
!     I1=I1+80
!     I2=I1+79
!     IF(I2.GE.IMAX) THEN
!       ILAST=1
!       I2=IMAX
!     ENDIF
!     GO TO 1112
!1111 FORMAT(80I1)
!     RETURN
!     END
      SUBROUTINE QCBYFC(TSFFCS,SNOFCS,QCTSFS,QCSNOS,QCTSFI,
     &                  LEN,LSOIL,SNOANL,AISANL,SLIANL,TSFANL,ALBANL,
     &                  ZORANL,SMCANL,
     &                  SMCCLM,TSFSMX,ALBOMX,ZOROMX, me)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer kount,me,k,i,lsoil,len
      REAL (KIND=KIND_IO8) zoromx,per,albomx,qctsfi,qcsnos,qctsfs,tsfsmx
      REAL (KIND=KIND_IO8) TSFFCS(LEN), SNOFCS(LEN)
      REAL (KIND=KIND_IO8) SNOANL(LEN), AISANL(LEN),
     &     SLIANL(LEN), ZORANL(LEN),
     &     TSFANL(LEN), ALBANL(LEN,4),
     &     SMCANL(LEN,LSOIL)
      REAL (KIND=KIND_IO8) SMCCLM(LEN,LSOIL)
!
      if (me .eq. 0) WRITE(6,*) 'QC of snow and sea-ice ANALYSIS'
!
! QC of snow analysis
!
!  Questionable snow cover
!
      KOUNT = 0
      DO I=1,LEN
        IF(SLIANL(I).GT.0..AND.
     &     TSFFCS(I).GT.QCTSFS.AND.SNOANL(I).GT.0.) THEN
          KOUNT      = KOUNT + 1
          SNOANL(I) = 0.
          TSFANL(I) = TSFFCS(I)
        ENDIF
      ENDDO
      IF(KOUNT.GT.0) THEN
        PER=FLOAT(KOUNT)/FLOAT(LEN)*100.
        if (me .eq. 0) then
        WRITE(6,*) ' Guess surface temp .GT. ',QCTSFS,
     &             ' but snow analysis indicates snow cover'
        WRITE(6,*) ' Snow analysis set to zero',
     &             ' at ',KOUNT, ' POINTS ',PER,'percent'
        endif
      ENDIF
!
!  Questionable no snow cover
!
      KOUNT = 0
      DO I=1,LEN
        IF(SLIANL(I).GT.0..AND.
     &     SNOFCS(I).GT.QCSNOS.AND.SNOANL(I).LT.0.) THEN
          KOUNT      = KOUNT + 1
          SNOANL(I) = SNOFCS(I)
          TSFANL(I) = TSFFCS(I)
        ENDIF
      ENDDO
      IF(KOUNT.GT.0) THEN
        PER=FLOAT(KOUNT)/FLOAT(LEN)*100.
        if (me .eq. 0) then
        WRITE(6,*) ' Guess snow depth .GT. ',QCSNOS,
     &             ' but snow analysis indicates no snow cover'
        WRITE(6,*) ' Snow analysis set to guess value',
     &             ' at ',KOUNT, ' POINTS ',PER,'percent'
        endif
      ENDIF
!
!  Questionable sea ice cover ! This QC is disable to correct error in
!  surface temparature over observed sea ice points
!
!     KOUNT = 0
!     DO I=1,LEN
!       IF(SLIANL(I).EQ.2..AND.
!    &     TSFFCS(I).GT.QCTSFI.AND.AISANL(I).EQ.1.) THEN
!         KOUNT        = KOUNT + 1
!         AISANL(I)   = 0.
!         SLIANL(I)   = 0.
!         TSFANL(I)   = TSFFCS(I)
!         SNOANL(I)   = 0.
!         ZORANL(I)   = ZOROMX
!         ALBANL(I,1) = ALBOMX
!         ALBANL(I,2) = ALBOMX
!         ALBANL(I,3) = ALBOMX
!         ALBANL(I,4) = ALBOMX
!         DO K=1,LSOIL
!           SMCANL(I,K) = SMCCLM(I,K)
!         ENDDO
!       ENDIF
!     ENDDO
!     IF(KOUNT.GT.0) THEN
!       PER=FLOAT(KOUNT)/FLOAT(LEN)*100.
!       if (me .eq. 0) then
!       WRITE(6,*) ' Guess surface temp .GT. ',QCTSFI,
!    &             ' but sea-ice analysis indicates sea-ice'
!       WRITE(6,*) ' Sea-ice analysis set to zero',
!    &             ' at ',KOUNT, ' POINTS ',PER,'percent'
!       endif
!     ENDIF
!
      RETURN
      END
      SUBROUTINE SETRMSK(KPDS5,SLMASK,IGAUL,JGAUL,WLON,RNLAT,
     &                   DATA,IMAX,JMAX,RLNOUT,RLTOUT,LMASK,RSLMSK
     &,                  GAUS,BLNO, BLTO, kgds1, kpds4, lbms)
      USE MACHINE , ONLY : kind_io8,kind_io4
      USE sfccyc_module
      implicit none
      REAL (KIND=KIND_IO8) blno,blto,wlon,rnlat,crit,data_max
      integer i,j,ijmax,jgaul,igaul,kpds5,jmax,imax, kgds1, kspla
      integer, intent(in)   :: kpds4
      logical*1, intent(in) :: lbms(imax,jmax)
      real*4                :: dummy(imax,jmax)

      REAL (KIND=KIND_IO8)    SLMASK(IGAUL,JGAUL)
      REAL (KIND=KIND_IO8)    DATA(IMAX,JMAX),RSLMSK(IMAX,JMAX)
     &,                       RLNOUT(IMAX), RLTOUT(JMAX)
      REAL (KIND=KIND_IO8)    A(JMAX), W(JMAX), RADI, dlat, dlon
      LOGICAL LMASK, GAUS
!
!     Set the longitude and latitudes for the grib file
!
      if (kgds1 .eq. 4) then         ! grib file on Gaussian grid
        KSPLA=4
        CALL SPLAT(KSPLA, JMAX, A, W)
!
        RADI = 180.0 / (4.*ATAN(1.))
        DO  J=1,JMAX
          RLTOUT(J) = ACOS(A(J)) * RADI
        ENDDO
!
        if (rnlat .gt. 0.0) then
          DO J=1,JMAX
            RLTOUT(J) = 90. - RLTOUT(J)
          ENDDO
        else
          DO J=1,JMAX
            RLTOUT(J) = -90. + RLTOUT(J)
          ENDDO
        endif
      elseif (kgds1 .eq. 0) then     ! grib file on lat/lon grid
        DLAT = -(RNLAT+RNLAT) / FLOAT(JMAX-1)
        DO J=1,JMAX
         RLTOUT(J) = RNLAT + (J-1) * DLAT
        ENDDO
      else                           ! grib file on some other grid
        call abort
      endif
      dlon = 360.0 / imax
      DO I=1,IMAX
        RLNOUT(I) = WLON + (I-1)*DLON
      ENDDO
!
!
      IJMAX  = IMAX*JMAX
      RSLMSK = 0.
!
!  Surface temperature
!
      IF(KPDS5.EQ.KPDTSF) THEN
!       LMASK=.FALSE.
        CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT
     &,            RLNOUT, RLTOUT, GAUS, BLNO, BLTO)
!    &,            DLON, DLAT, GAUS, BLNO, BLTO)
        CRIT=0.5
        CALL ROF01(RSLMSK,IJMAX,'GE',CRIT)
        LMASK=.TRUE.
!
!  Bucket soil wetness
!
      ELSEIF(KPDS5.EQ.KPDWET) THEN
        CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT
     &,            RLNOUT, RLTOUT, GAUS, BLNO, BLTO)
!    &,            DLON, DLAT, GAUS, BLNO, BLTO)
        CRIT=0.5
        CALL ROF01(RSLMSK,IJMAX,'GE',CRIT)
        LMASK=.TRUE.
!       WRITE(6,*) 'WET RSLMSK'
!       ZNNT=1.
!       CALL NNTPRT(RSLMSK,IJMAX,ZNNT)
!
!  Snow depth
!
      ELSEIF(KPDS5.EQ.KPDSND) THEN
        IF(KPDS4 == 192) THEN  ! USE THE BITMAP
          RSLMSK = 0.
          DO J = 1, JMAX
            DO I = 1, IMAX
              IF (LBMS(I,J)) THEN
                RSLMSK(I,J) = 1.
              END IF
            ENDDO
          ENDDO
          LMASK=.TRUE.
        ELSE
          LMASK=.FALSE.
        END IF
!
! SNOW LIQ EQUIVALENT DEPTH
!
      ELSEIF(KPDS5.EQ.KPDSNO) THEN
        CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT
     &,            RLNOUT, RLTOUT, GAUS, BLNO, BLTO)
!    &,            DLON, DLAT, GAUS, BLNO, BLTO)
        CRIT=0.5
        CALL ROF01(RSLMSK,IJMAX,'GE',CRIT)
        LMASK=.TRUE.
!       WRITE(6,*) 'SNO RSLMSK'
!       ZNNT=1.
!       CALL NNTPRT(RSLMSK,IJMAX,ZNNT)
!
!  Soil Moisture
!
      ELSEIF(KPDS5.EQ.KPDSMC) THEN
        IF(KPDS4 == 192) THEN  ! USE THE BITMAP
          RSLMSK = 0.
          DO J = 1, JMAX
            DO I = 1, IMAX
              IF (LBMS(I,J)) THEN
                RSLMSK(I,J) = 1.
              END IF
            ENDDO
          ENDDO
          LMASK=.TRUE.
        ELSE
          CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT
     &,            RLNOUT, RLTOUT, GAUS, BLNO, BLTO)
          CRIT=0.5
          CALL ROF01(RSLMSK,IJMAX,'GE',CRIT)
          LMASK=.TRUE.
        ENDIF
!
!  Surface roughness
!
      ELSEIF(KPDS5.EQ.KPDZOR) THEN
        DO J=1,JMAX
          DO I=1,IMAX
            RSLMSK(I,J)=DATA(I,J)
          ENDDO
        ENDDO
        CRIT=9.9
        CALL ROF01(RSLMSK,IJMAX,'LT',CRIT)
        LMASK=.TRUE.
!       WRITE(6,*) 'ZOR RSLMSK'
!       ZNNT=1.
!       CALL NNTPRT(RSLMSK,IJMAX,ZNNT)
!
!  Albedo
!
!     ELSEIF(KPDS5.EQ.KPDALB) THEN
!       DO J=1,JMAX
!         DO I=1,IMAX
!           RSLMSK(I,J)=DATA(I,J)
!         ENDDO
!       ENDDO
!       CRIT=99.
!       CALL ROF01(RSLMSK,IJMAX,'LT',CRIT)
!       LMASK=.TRUE.
!       WRITE(6,*) 'ALB RSLMSK'
!       ZNNT=1.
!       CALL NNTPRT(RSLMSK,IJMAX,ZNNT)
!
!  Albedo
!
!cbosu  new snowfree albedo database has bitmap, use it.
      ELSEIF(KPDS5.EQ.KPDALB(1)) THEN
        if (kpds4 == 192) then  ! use the bitmap
          rslmsk = 0.
          do j = 1, jmax
            do i = 1, imax
              if (lbms(i,j)) then
                rslmsk(i,j) = 1.  
              end if
            enddo
          enddo
          lmask = .true.
        else  ! no bitmap. old database has no water flag.
          LMASK=.FALSE.
        end if
      ELSEIF(KPDS5.EQ.KPDALB(2)) THEN
!cbosu
        if (kpds4 == 192) then  ! use the bitmap
          rslmsk = 0.
          do j = 1, jmax
            do i = 1, imax
              if (lbms(i,j)) then
                rslmsk(i,j) = 1.  
              end if
            enddo
          enddo
          lmask = .true.
        else  ! no bitmap. old database has no water flag.
          LMASK=.FALSE.
        end if
      ELSEIF(KPDS5.EQ.KPDALB(3)) THEN
!cbosu
        if (kpds4 == 192) then  ! use the bitmap
          rslmsk = 0.
          do j = 1, jmax
            do i = 1, imax
              if (lbms(i,j)) then
                rslmsk(i,j) = 1.  
              end if
            enddo
          enddo
          lmask = .true.
        else  ! no bitmap. old database has no water flag.
          LMASK=.FALSE.
        end if
      ELSEIF(KPDS5.EQ.KPDALB(4)) THEN
!cbosu
        if (kpds4 == 192) then  ! use the bitmap
          rslmsk = 0.
          do j = 1, jmax
            do i = 1, imax
              if (lbms(i,j)) then
                rslmsk(i,j) = 1.  
              end if
            enddo
          enddo
          lmask = .true.
        else  ! no bitmap. old database has no water flag.
          LMASK=.FALSE.
        end if
!
!  Vegetation fraction for Albedo
!
      ELSEIF(KPDS5.EQ.KPDALF(1)) THEN
!       RSLMSK=DATA
!       CRIT=0.
!       CALL ROF01(RSLMSK,IJMAX,'GT',CRIT)
!       LMASK=.TRUE.
        LMASK=.FALSE.
      ELSEIF(KPDS5.EQ.KPDALF(2)) THEN
!       RSLMSK=DATA
!       CRIT=0.
!       CALL ROF01(RSLMSK,IJMAX,'GT',CRIT)
!       LMASK=.TRUE.
        LMASK=.FALSE.
!
!  Sea ice
!
      ELSEIF(KPDS5.EQ.KPDAIS) THEN
        LMASK=.FALSE.
!       CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT
!    &,            DLON, DLAT, GAUS, BLNO, BLTO)
!       CRIT=0.5
!       CALL ROF01(RSLMSK,IJMAX,'GE',CRIT)
!
        data_max = 0.0
        do j=1,jmax
          do i=1,imax
              rslmsk(i,j) = data(i,j)
              data_max= max(data_max,data(i,j))
          enddo
        enddo
        CRIT=1.0
        if (data_max .gt. CRIT) then
          CALL ROF01(RSLMSK,IJMAX,'GT',CRIT)
          LMASK=.TRUE.
        else
          LMASK=.FALSE.
        endif
!       WRITE(6,*) 'ACN RSLMSK'
!       ZNNT=1.
!       CALL NNTPRT(RSLMSK,IJMAX,ZNNT)
!
!  Deep soil temperature
!
      ELSEIF(KPDS5.EQ.KPDTG3) THEN
        LMASK=.FALSE.
!       CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT
!    &,            RLNOUT, RLTOUT, GAUS, BLNO, BLTO)
!    &,            DLON, DLAT, GAUS, BLNO, BLTO)
!       CRIT=0.5
!       CALL ROF01(RSLMSK,IJMAX,'GE',CRIT)
!       LMASK=.TRUE.
!
!  Plant resistance
!
!     ELSEIF(KPDS5.EQ.KPDPLR) THEN
!       CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT
!    &,            RLNOUT, RLTOUT, GAUS, BLNO, BLTO)
!    &,            DLON, DLAT, GAUS, BLNO, BLTO)
!       CRIT=0.5
!       CALL ROF01(RSLMSK,IJMAX,'GE',CRIT)
!       LMASK=.TRUE.
!
!       WRITE(6,*) 'PLR RSLMSK'
!       ZNNT=1.
!       CALL NNTPRT(RSLMSK,IJMAX,ZNNT)
!
!  Glacier points
!
      ELSEIF(KPDS5.EQ.KPDGLA) THEN
        LMASK=.FALSE.
!
!  Max ice extent
!
      ELSEIF(KPDS5.EQ.KPDMXI) THEN
        LMASK=.FALSE.
!
!  Snow cover
!
      ELSEIF(KPDS5.EQ.KPDSCV) THEN
        CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT
     &,            RLNOUT, RLTOUT, GAUS, BLNO, BLTO)
!    &,            DLON, DLAT, GAUS, BLNO, BLTO)
        CRIT=0.5
        CALL ROF01(RSLMSK,IJMAX,'GE',CRIT)
        LMASK=.TRUE.
!       WRITE(6,*) 'SCV RSLMSK'
!       ZNNT=1.
!       CALL NNTPRT(RSLMSK,IJMAX,ZNNT)
!
!  Sea ice concentration
!
      ELSEIF(KPDS5.EQ.KPDACN) THEN
        LMASK=.FALSE.
        CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT
     &,            RLNOUT, RLTOUT, GAUS, BLNO, BLTO)
!    &,            DLON, DLAT, GAUS, BLNO, BLTO)
        CRIT=0.5
        CALL ROF01(RSLMSK,IJMAX,'GE',CRIT)
        LMASK=.TRUE.
!       WRITE(6,*) 'ACN RSLMSK'
!       ZNNT=1.
!       CALL NNTPRT(RSLMSK,IJMAX,ZNNT)
!
!  Vegetation cover
!
      ELSEIF(KPDS5.EQ.KPDVEG) THEN
!cggg
        if (kpds4 == 192) then  ! use the bitmap
          rslmsk = 0.
          do j = 1, jmax
            do i = 1, imax
              if (lbms(i,j)) then
                rslmsk(i,jmax-j+1) = 1.  ! need to flip grid in n/s direction
              end if
            enddo
          enddo
          lmask = .true.
        else  ! no bitmap, set mask the old way.

          CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT
     &,              RLNOUT, RLTOUT, GAUS, BLNO, BLTO)
          CRIT=0.5
          CALL ROF01(RSLMSK,IJMAX,'GE',CRIT)
          LMASK=.TRUE.

       end if
!
!  Soil type
!
      ELSEIF(KPDS5.EQ.KPDSOT) THEN
!       CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT
!    &,            RLNOUT, RLTOUT, GAUS, BLNO, BLTO)
!    &,            DLON, DLAT, GAUS, BLNO, BLTO)


!cggg soil type is zero over water, use this to get a bitmap.

        do j = 1, jmax
          do i = 1, imax
            rslmsk(i,j) = data(i,j)
          enddo
        enddo

        CRIT=0.1
        CALL ROF01(RSLMSK,IJMAX,'GT',CRIT)
        LMASK=.TRUE.

!
!  Vegetation type
!
      ELSEIF(KPDS5.EQ.KPDVET) THEN
!       CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT
!    &,            RLNOUT, RLTOUT, GAUS, BLNO, BLTO)
!    &,            DLON, DLAT, GAUS, BLNO, BLTO)


!cggg veg type is zero over water, use this to get a bitmap.

        do j = 1, jmax
          do i = 1, imax
            rslmsk(i,j) = data(i,j)
          enddo
        enddo

        CRIT=0.1
        CALL ROF01(RSLMSK,IJMAX,'GT',CRIT)
        LMASK=.TRUE.
!
!      These are for four new data type added by Clu -- not sure its correct!
!
      ELSEIF(KPDS5.EQ.KPDVMN) THEN
!
!cggg  greenness is zero over water, use this to get a bitmap.
!
        do j = 1, jmax
          do i = 1, imax
            rslmsk(i,j) = data(i,j)
          enddo
        enddo
!
        CRIT=0.1
        CALL ROF01(RSLMSK,IJMAX,'GT',CRIT)
        LMASK=.TRUE.
!cggg        LMASK=.FALSE.
!
      ELSEIF(KPDS5.EQ.KPDVMX) THEN
!
!cggg  greenness is zero over water, use this to get a bitmap.
!
        do j = 1, jmax
          do i = 1, imax
            rslmsk(i,j) = data(i,j)
          enddo
        enddo
!
        CRIT=0.1
        CALL ROF01(RSLMSK,IJMAX,'GT',CRIT)
        LMASK=.TRUE.
!cggg        LMASK=.FALSE.
!
      ELSEIF(KPDS5.EQ.KPDSLP) THEN
!
!cggg slope type is zero over water, use this to get a bitmap.
!
        do j = 1, jmax
          do i = 1, imax
            rslmsk(i,j) = data(i,j)
          enddo
        enddo
!
        CRIT=0.1
        CALL ROF01(RSLMSK,IJMAX,'GT',CRIT)
        LMASK=.TRUE.
!cggg        LMASK=.FALSE.
!
!cbosu new maximum snow albedo database has bitmap
      ELSEIF(KPDS5.EQ.KPDABS) THEN
        if (kpds4 == 192) then  ! use the bitmap
          rslmsk = 0.
          do j = 1, jmax
            do i = 1, imax
              if (lbms(i,j)) then
                rslmsk(i,j) = 1.  
              end if
            enddo
          enddo
          lmask = .true.
        else  ! no bitmap. old database has zero over water
          do j = 1, jmax
            do i = 1, imax
              rslmsk(i,j) = data(i,j)
            enddo
          enddo
          CRIT=0.1
          CALL ROF01(RSLMSK,IJMAX,'GT',CRIT)
          LMASK=.TRUE.
        end if
      ENDIF
!
      RETURN
      END
      SUBROUTINE GA2LA(GAUIN,IMXIN,JMXIN,REGOUT,IMXOUT,JMXOUT,
     &                 WLON,RNLAT,RLNOUT,RLTOUT,GAUS,BLNO, BLTO)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i1,i2,j2,ishft,i,jj,j1,jtem,jmxout,imxin,jmxin,imxout,
     &        j,iret
      REAL (KIND=KIND_IO8) alamd,dxin,aphi,x,sum1,sum2,y,dlati,wlon,
     &                     rnlat,dxout,dphi,dlat,facns,tem,blno,
     &                     blto
!
!  INTERPOLATION FROM LAT/LON GRID TO OTHER LAT/LON GRID
!
      REAL (KIND=KIND_IO8) GAUIN (IMXIN,JMXIN), REGOUT(IMXOUT,JMXOUT)
     &,                    RLNOUT(IMXOUT), RLTOUT(JMXOUT)
      LOGICAL GAUS
!
      Real, allocatable :: GAUL(:)
      REAL (KIND=KIND_IO8) DDX(IMXOUT),DDY(JMXOUT)
      Integer IINDX1(IMXOUT), IINDX2(IMXOUT),
     &        JINDX1(JMXOUT), JINDX2(JMXOUT)
      integer JMXSAV,N,KSPLA
      data    JMXSAV/0/
      save    jmxsav, gaul, dlati
      REAL (KIND=KIND_IO8) radi
      REAL (KIND=KIND_IO8) A(jmxin), W(jmxin)
!
!
      logical first
      integer   NUM_THREADS
      data first /.true./
      save NUM_THREADS, first
!
      integer LEN_THREAD_M, J1_T, J2_T, IT
      integer NUM_PARTHDS
!
      if (first) then
         NUM_THREADS = NUM_PARTHDS()
         first = .false.
      endif
!
      if (jmxin .ne. jmxsav) then
        if (jmxsav .gt. 0) deallocate (GAUL, STAT=iret)
        allocate (GAUL(JMXIN))
        jmxsav = JMXIN
        IF (GAUS) THEN
cjfe      CALL GAULAT(GAUL,JMXIN)
cjfe
!
          KSPLA=4
          CALL SPLAT(KSPLA, JMXIN, A, W)
!
          RADI = 180.0 / (4.*ATAN(1.))
          DO  N=1,JMXIN
            GAUL(N) = ACOS(A(N)) * RADI
          ENDDO
cjfe
          DO J=1,JMXIN
            GAUL(J) = 90. - GAUL(J)
          ENDDO
        ELSE
          DLAT = -2*BLTO / FLOAT(JMXIN-1)
          DLATI = 1 / DLAT
          DO J=1,JMXIN
           GAUL(J) = BLTO + (J-1) * DLAT
          ENDDO
        ENDIF
      ENDIF
!
!
      DXIN  = 360. / FLOAT(IMXIN )
!
      DO I=1,IMXOUT
        ALAMD = RLNOUT(I)
        I1     = FLOOR((ALAMD-BLNO)/DXIN) + 1
        DDX(I) = (ALAMD-BLNO)/DXIN-(I1-1)
        IINDX1(I) = MODULO(I1-1,IMXIN) + 1
        IINDX2(I) = MODULO(I1  ,IMXIN) + 1
      ENDDO
!
!
      LEN_THREAD_M  = (JMXOUT+NUM_THREADS-1) / NUM_THREADS
!
      IF (GAUS) THEN
!
!$OMP PARALLEL DO PRIVATE(J1_T,J2_T,IT,J1,J2,JJ)
!$OMP+PRIVATE(APHI)
!$OMP+SHARED(NUM_THREADS,LEN_THREAD_M)
!$OMP+SHARED(JMXIN,JMXOUT,GAUL,RLTOUT,JINDX1,DDY)
!
        DO IT=1,NUM_THREADS   ! START OF THREADED LOOP ...................
          J1_T       = (IT-1)*LEN_THREAD_M+1
          J2_T       = MIN(J1_T+LEN_THREAD_M-1,JMXOUT)
!
          J2=1
          DO 40 J=J1_T,J2_T
            APHI=RLTOUT(J)
            DO 50 JJ=1,JMXIN
              IF(APHI.LT.GAUL(JJ)) GO TO 50
              J2=JJ
              GO TO 42
   50       CONTINUE
   42       CONTINUE
            IF(J2.GT.2) GO TO 43
            J1=1
            J2=2
            GO TO 44
   43       CONTINUE
            IF(J2.LE.JMXIN) GO TO 45
            J1=JMXIN-1
            J2=JMXIN
            GO TO 44
   45       CONTINUE
            J1=J2-1
   44       CONTINUE
            JINDX1(J)=J1
            JINDX2(J)=J2
            DDY(J)=(APHI-GAUL(J1))/(GAUL(J2)-GAUL(J1))
   40     CONTINUE
        ENDDO             ! END OF THREADED LOOP ...................
!$OMP   END PARALLEL DO
!
      ELSE
!$OMP PARALLEL DO PRIVATE(J1_T,J2_T,IT,J1,J2,JTEM)
!$OMP+PRIVATE(APHI)
!$OMP+SHARED(NUM_THREADS,LEN_THREAD_M)
!$OMP+SHARED(JMXIN,JMXOUT,GAUL,RLTOUT,JINDX1,DDY,DLATI,BLTO)
!
        DO IT=1,NUM_THREADS   ! START OF THREADED LOOP ...................
          J1_T       = (IT-1)*LEN_THREAD_M+1
          J2_T       = MIN(J1_T+LEN_THREAD_M-1,JMXOUT)
!
          J2=1
          DO 400 J=J1_T,J2_T
            APHI=RLTOUT(J)
            JTEM = (APHI - BLTO) * DLATI + 1
            IF (JTEM .GE. 1 .AND. JTEM .LT. JMXIN) THEN
              J1 = JTEM
              J2 = J1 + 1
              DDY(J)=(APHI-GAUL(J1))/(GAUL(J2)-GAUL(J1))
            ELSEIF (JTEM .EQ. JMXIN) THEN
              J1 = JMXIN
              J2 = JMXIN
              DDY(J)=1.0
            ELSE
              J1 = 1
              J2 = 1
              DDY(J)=1.0
            ENDIF
!
            JINDX1(J) = J1
            JINDX2(J) = J2
  400     CONTINUE
        ENDDO             ! END OF THREADED LOOP ...................
!$OMP   END PARALLEL DO
      ENDIF
!
!     WRITE(6,*) 'GA2LA'
!     WRITE(6,*) 'IINDX1'
!     WRITE(6,*) (IINDX1(N),N=1,IMXOUT)
!     WRITE(6,*) 'IINDX2'
!     WRITE(6,*) (IINDX2(N),N=1,IMXOUT)
!     WRITE(6,*) 'JINDX1'
!     WRITE(6,*) (JINDX1(N),N=1,JMXOUT)
!     WRITE(6,*) 'JINDX2'
!     WRITE(6,*) (JINDX2(N),N=1,JMXOUT)
!     WRITE(6,*) 'DDY'
!     WRITE(6,*) (DDY(N),N=1,JMXOUT)
!     WRITE(6,*) 'DDX'
!     WRITE(6,*) (DDX(N),N=1,JMXOUT)
!
!
!$OMP PARALLEL DO PRIVATE(J1_T,J2_T,IT,I,I1,I2)
!$OMP+PRIVATE(J,J1,J2,X,Y)
!$OMP+SHARED(NUM_THREADS,LEN_THREAD_M)
!$OMP+SHARED(IMXOUT,IINDX1,JINDX1,DDX,DDY,GAUIN,REGOUT)
!
      DO IT=1,NUM_THREADS   ! START OF THREADED LOOP ...................
        J1_T       = (IT-1)*LEN_THREAD_M+1
        J2_T       = MIN(J1_T+LEN_THREAD_M-1,JMXOUT)
!
        DO  J=J1_T,J2_T
          Y  = DDY(J)
          J1 = JINDX1(J)
          J2 = JINDX2(J)
          DO I=1,IMXOUT
            X  = DDX(I)
            I1 = IINDX1(I)
            I2 = IINDX2(I)
            REGOUT(I,J) = (1.-X)*((1.-Y)*GAUIN(I1,J1) + Y*GAUIN(I1,J2))
     &                  +     X *((1.-Y)*GAUIN(I2,J1) + Y*GAUIN(I2,J2))
          ENDDO
        ENDDO
      ENDDO             ! END OF THREADED LOOP ...................
!$OMP END PARALLEL DO
!
      SUM1 = 0.
      SUM2 = 0.
      DO I=1,IMXIN
        SUM1 = SUM1 + GAUIN(I,1)
        SUM2 = SUM2 + GAUIN(I,JMXIN)
      ENDDO
      SUM1 = SUM1 / FLOAT(IMXIN)
      SUM2 = SUM2 / FLOAT(IMXIN)
!
      IF (GAUS) THEN
        IF (RNLAT .GT. 0.0) THEN
          DO I=1,IMXOUT
            REGOUT(I,     1) = SUM1
            REGOUT(I,JMXOUT) = SUM2
          ENDDO
        ELSE
          DO I=1,IMXOUT
            REGOUT(I,     1) = SUM2
            REGOUT(I,JMXOUT) = SUM1
          ENDDO
        ENDIF
      ELSE
        IF (BLTO .LT. 0.0) THEN
          IF (RNLAT .GT. 0.0) THEN
            DO I=1,IMXOUT
              REGOUT(I,     1) = SUM2
              REGOUT(I,JMXOUT) = SUM1
            ENDDO
          ELSE
            DO I=1,IMXOUT
              REGOUT(I,     1) = SUM1
              REGOUT(I,JMXOUT) = SUM2
            ENDDO
          ENDIF
        ELSE
          IF (RNLAT .LT. 0.0) THEN
            DO I=1,IMXOUT
              REGOUT(I,     1) = SUM2
              REGOUT(I,JMXOUT) = SUM1
            ENDDO
          ELSE
            DO I=1,IMXOUT
              REGOUT(I,     1) = SUM1
              REGOUT(I,JMXOUT) = SUM2
            ENDDO
          ENDIF
        ENDIF
      ENDIF
!
      RETURN
      END
!Clu [-1L/+1L] add slptype
!Clu  subroutine landtyp(vegtype,soiltype,slmask,LEN)
      subroutine landtyp(vegtype,soiltype,slptype,slmask,LEN)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,len
      REAL (KIND=KIND_IO8) vegtype(LEN),soiltype(LEN),slmask(LEN)
!Clu [+1L] add slptype
     +,                    slptype(LEN)  
!
!  make sure that the soil type and veg type are non-zero over land
!
      do i = 1, LEN
        if (slmask(i) .eq. 1) then
          if (vegtype(i)  .eq. 0.)  vegtype(i)  = 7
          if (soiltype(i) .eq. 0.)  soiltype(i) = 2
!Clu [+1L] add slptype
          if (slptype(i)  .eq. 0.)  slptype(i)  = 1
        endif
      enddo
      return
      end
      SUBROUTINE GAULAT(GAUL,K)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer n,k
      REAL (KIND=KIND_IO8) radi
      REAL (KIND=KIND_IO8) A(K), W(K), GAUL(K)
!
      CALL SPLAT(4, K, A, W)
!
      RADI = 180.0 / (4.*ATAN(1.))
      DO  N=1,K
        GAUL(N) = ACOS(A(N)) * RADI
      ENDDO
!
!     PRINT *,'GAUSSIAN LAT (DEG) FOR JMAX=',K
!     PRINT *,(GAUL(N),N=1,K)
!
      RETURN
   70 WRITE(6,6000)
 6000 FORMAT(//5X,'ERROR IN GAUAW'//)
      STOP
      END
!-----------------------------------------------------------------------
      SUBROUTINE ANOMINT(TSFAN0,TSFCLM,TSFCL0,TSFANL,LEN)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,len
      REAL (KIND=KIND_IO8) TSFANL(LEN), TSFAN0(LEN),
     &                     TSFCLM(LEN), TSFCL0(LEN)
!
!  Time interpolation of anomalies
!  Add initial anomaly to date interpolated climatology
!
      WRITE(6,*) 'ANOMINT'
      DO I=1,LEN
        TSFANL(I) = TSFAN0(I) - TSFCL0(I) + TSFCLM(I)
      ENDDO
      RETURN
      END
      SUBROUTINE CLIMA(LUGB,IY,IM,ID,IH,FH,LEN,LSOIL,
     &                 SLMASK,FNTSFC,FNWETC,FNSNOC,FNZORC,FNALBC,FNAISC,
     &                 FNTG3C,FNSCVC,FNSMCC,FNSTCC,FNACNC,FNVEGC,
     &                 fnvetc,fnsotc,
!Clu [+1L] add fn()c for vmn, vmx, slp, abs
     &                 FNVMNC,FNVMXC,FNSLPC,FNABSC,
     &                 TSFCLM,TSFCL2,WETCLM,SNOCLM,ZORCLM,ALBCLM,AISCLM,
     &                 TG3CLM,CVCLM ,CVBCLM,CVTCLM,
     &                 CNPCLM,SMCCLM,STCCLM,SLICLM,SCVCLM,ACNCLM,VEGCLM,
     &                 vetclm,sotclm,ALFCLM,
!Clu [+1L] add ()clm for vmn, vmx, slp, abs
     &                 VMNCLM,VMXCLM,SLPCLM,ABSCLM,
     &                 KPDTSF,KPDWET,KPDSNO,KPDZOR,KPDALB,KPDAIS,
     &                 KPDTG3,KPDSCV,KPDACN,KPDSMC,KPDSTC,KPDVEG,
     &                 kpdvet,kpdsot,kpdalf,TSFCL0,
!Clu [+1L] add kpd() for vmn, vmx, slp, abs
     &                 KPDVMN,KPDVMX,KPDSLP,KPDABS,
     &                 DELTSFC, LANOM
     &,                IMSK, JMSK, SLMSKH, OUTLAT, OUTLON
     &,                GAUS, BLNO, BLTO, me,lprnt,iprnt, FNALBC2, IALB)
!
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      REAL (KIND=KIND_IO8) rjday,wei1x,wei2x,rjdayh,wei2m,wei1m,wei1s,
     &                     wei2s,fh,stcmon1s,blto,blno,deltsfc,rjdayh2
      integer jdoy,jday,jh,jdow,mmm,mmp,mm,iret,monend,i,k,jm,jd,iy4,
     &        jy,mon1,is2,isx,kpd9,is1,l,nn,mon2,mon,is,kpdsno,
     &        kpdzor,kpdtsf,kpdwet,kpdscv,kpdacn,kpdais,kpdtg3,im,id,
     &        lugb,iy,len,lsoil,ih,kpdsmc,iprnt,me,m1,m2,k1,k2,
     &        kpdvet,kpdsot,kpdstc,kpdveg,jmsk,imsk,j,ialb
!Clu [+1L] add kpd() for vmn, vmx, slp, abs
     &,       kpdvmn,kpdvmx,kpdslp,kpdabs
      INTEGER kpdalb(4), kpdalf(2)
!
!cbosu

      CHARACTER*500 FNTSFC,FNWETC,FNSNOC,FNZORC,FNALBC,FNAISC,
     &             FNTG3C,FNSCVC,FNSMCC,FNSTCC,FNACNC,FNVEGC,
     &             fnvetc,fnsotc,fnalbc2
!Clu [+1L] add fn()c for vmn, vmx, slp, abs
     &,            FNVMNC,FNVMXC,FNSLPC,FNABSC
      REAL (KIND=KIND_IO8) TSFCLM(LEN),TSFCL2(LEN),
     &     WETCLM(LEN),SNOCLM(LEN),
     &     ZORCLM(LEN),ALBCLM(LEN,4),AISCLM(LEN),
     &     TG3CLM(LEN),ACNCLM(LEN),
     &     CVCLM (LEN),CVBCLM(LEN),CVTCLM(LEN),
     &     CNPCLM(LEN),
     &     SMCCLM(LEN,LSOIL),STCCLM(LEN,LSOIL),
     &     SLICLM(LEN),SCVCLM(LEN),VEGCLM(LEN),
     &     vetclm(LEN),sotclm(LEN),ALFCLM(LEN,2)
!Clu [+1L] add ()cm for vmn, vmx, slp, abs
     &,    VMNCLM(LEN),VMXCLM(LEN),SLPCLM(LEN),ABSCLM(LEN)
      REAL (KIND=KIND_IO8) SLMSKH(IMSK,JMSK)
      REAL (KIND=KIND_IO8) OUTLAT(LEN), OUTLON(LEN)
!
      REAL (KIND=KIND_IO8) SLMASK(LEN), TSFCL0(LEN)
      REAL (KIND=KIND_IO8), ALLOCATABLE :: SLMASK_NOICE(:)
!
      LOGICAL LANOM, GAUS, first
!
! set z0 based on sib vegetation type
      REAL (KIND=KIND_IO8) Z0_SIB(13)
      DATA Z0_SIB /2.653, 0.826, 0.563, 1.089, 0.854, 0.856,
     &             0.035, 0.238, 0.065, 0.076, 0.011, 0.035,
     &             0.011 /
!
! DAYHF : JULIAN DAY OF THE MIDDLE OF EACH MONTH
!
      REAL (KIND=KIND_IO8) DAYHF(13)
      DATA DAYHF/ 15.5, 45.0, 74.5,105.0,135.5,166.0,
     &           196.5,227.5,258.0,288.5,319.0,349.5,380.5/
!
      real (kind=kind_io8) fha(5)
      integer ida(8),jda(8),ivtyp, kpd7
!
      real (kind=kind_io8), allocatable :: tsf(:,:),sno(:,:),
     &                     zor(:,:),wet(:,:),
     &                     ais(:,:), acn(:,:),   scv(:,:), smc(:,:,:),
     &                     tg3(:),   alb(:,:,:), alf(:,:),
     &                     vet(:),   sot(:),     tsf2(:),
     &                     veg(:,:), stc(:,:,:)
!Clu [+1L] add vmn, vmx, slp, abs
     &,                    vmn(:), vmx(:),  slp(:), abs(:)
!
      integer mon1s, mon2s, sea1s, sea2s, sea1, sea2
      data first/.true./
      data mon1s/0/, mon2s/0/, sea1s/0/, sea2s/0/
!
      save first, tsf, sno, zor, wet,  ais, acn, scv, smc, tg3,
     &     alb,   alf, vet, sot, tsf2, veg, stc
!Clu [+1L] add vmn, vmx, slp, abs
     &,    vmn,   vmx, slp, abs,
     &     mon1s, mon2s, sea1s, sea2s, dayhf, k1, k2, m1, m2
!
      logical lprnt
!
      DO I=1,LEN
        TSFCLM(I) = 0.0
        TSFCL2(I) = 0.0
        SNOCLM(I) = 0.0
        WETCLM(I) = 0.0
        ZORCLM(I) = 0.0
        AISCLM(I) = 0.0
        TG3CLM(I) = 0.0
        ACNCLM(I) = 0.0
        CVCLM(I)  = 0.0
        CVBCLM(I) = 0.0
        CVTCLM(I) = 0.0
        CNPCLM(I) = 0.0
        SLICLM(I) = 0.0
        SCVCLM(I) = 0.0
!Clu [+4L]  add ()clm for vmn, vmx, slp, abs
        VMNCLM(I) = 0.0
        VMXCLM(I) = 0.0
        SLPCLM(I) = 0.0
        ABSCLM(I) = 0.0
      ENDDO
      DO K=1,LSOIL
        DO I=1,LEN
          SMCCLM(I,K) = 0.0
          STCCLM(I,K) = 0.0
        ENDDO
      ENDDO
      DO K=1,4
        DO I=1,LEN
          ALBCLM(I,K) = 0.0
        ENDDO
      ENDDO
      DO K=1,2
        DO I=1,LEN
          ALFCLM(I,K) = 0.0
        ENDDO
      ENDDO
!
      IRET   = 0
      MONEND = 9999
!
      if (first) then
!
!    Allocate variables to be saved
!
       Allocate (tsf(len,2), sno(len,2),      zor(len,2),
     &           wet(len,2), ais(len,2),      acn(len,2),
     &           scv(len,2), smc(len,lsoil,2),
     &           tg3(len),   alb(len,4,2),    alf(len,2),
     &           vet(len),   sot(len), tsf2(len),
!Clu [+1L] add vmn, vmx, slp, abs
     &           vmn(len),   vmx(len), slp(len), abs(len),
     &           veg(len,2), stc(len,lsoil,2))
!
!     Get TSF climatology for the begining of the forecast
!
        if (fh .gt. 0.0) then
!cbosu
          if (me == 0) print*,'bosu fh gt 0'

          iy4=iy
          if(iy.lt.101) iy4=1900+iy4
          fha=0
          ida=0
          jda=0
!         fha(2)=nint(fh)
          ida(1)=iy
          ida(2)=im
          ida(3)=id
          ida(5)=ih
          call w3movdat(fha,ida,jda)
          jy=jda(1)
          jm=jda(2)
          jd=jda(3)
          jh=jda(5)
          if (me .eq. 0) write(6,*) ' Forecast JY,JM,JD,JH',
     &                   jy,jm,jd,jh
          jdow = 0
          jdoy = 0
          jday = 0
          call w3doxdat(jda,jdow,jdoy,jday)
          rjday=jdoy+jda(5)/24.
          IF(RJDAY.LT.DAYHF(1)) RJDAY=RJDAY+365.
!
          if (me .eq. 0) WRITE(6,*) 'Forecast JY,JM,JD,JH=',JY,JM,JD,JH
!
!         For monthly mean climatology
!
          MONEND = 12
          DO MM=1,MONEND
            MMM=MM
            MMP=MM+1
            IF(RJDAY.GE.DAYHF(MMM).AND.RJDAY.LT.DAYHF(MMP)) THEN
               MON1=MMM
               MON2=MMP
               GO TO 10
            ENDIF
          ENDDO
          PRINT *,'WRONG RJDAY',RJDAY
          CALL ABORT
   10     CONTINUE
          WEI1M = (DAYHF(MON2)-RJDAY)/(DAYHF(MON2)-DAYHF(MON1))
          WEI2M = (RJDAY-DAYHF(MON1))/(DAYHF(MON2)-DAYHF(MON1))
          IF(MON2.EQ.13) MON2=1
          if (me .eq. 0) PRINT *,'RJDAY,MON1,MON2,WEI1M,WEI2M=',
     &                   RJDAY,MON1,MON2,WEI1M,WEI2M
!
!       Read Monthly mean climatology of TSF
!
          kpd7 = -1
          do nn=1,2
            MON = MON1
            if (NN .eq. 2) MON = MON2
            CALL FIXRDC(LUGB,FNTSFC,KPDTSF,kpd7,MON,SLMASK,
     &                 TSF(1,NN),LEN,IRET
     &,                IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                OUTLAT, OUTLON, me)
          enddo
!
!  TSF AT THE BEGINING OF FORECAST I.E. FH=0
!
          DO I=1,LEN
            TSFCL0(I) = wei1m * tsf(i,1) + wei2m * tsf(i,2)
          ENDDO
        endif
      endif
!
!  Compute current JY,JM,JD,JH of forecast and the day of the year
!
      iy4=iy
      if(iy.lt.101) iy4=1900+iy4
      fha    = 0
      ida    = 0
      jda    = 0
      fha(2) = nint(fh)
      ida(1) = iy
      ida(2) = im
      ida(3) = id
      ida(5) = ih
      call w3movdat(fha,ida,jda)
      jy     = jda(1)
      jm     = jda(2)
      jd     = jda(3)
      jh     = jda(5)
!     if (me .eq. 0) write(6,*) ' Forecast JY,JM,JD,JH,rjday=',
!    &               jy,jm,jd,jh,rjday
      jdow   = 0
      jdoy   = 0
      jday   = 0
      call w3doxdat(jda,jdow,jdoy,jday)
      rjday  = jdoy+jda(5)/24.
      IF(RJDAY.LT.DAYHF(1)) RJDAY=RJDAY+365.

      if (me .eq. 0) write(6,*) ' Forecast JY,JM,JD,JH,rjday=',
     &               jy,jm,jd,jh,rjday
!
      if (me .eq. 0) WRITE(6,*) 'Forecast JY,JM,JD,JH=',JY,JM,JD,JH
!
!     For monthly mean climatology
!
      MONEND = 12
      DO MM=1,MONEND
         MMM=MM
         MMP=MM+1
         IF(RJDAY.GE.DAYHF(MMM).AND.RJDAY.LT.DAYHF(MMP)) THEN
            MON1=MMM
            MON2=MMP
            GO TO 20
         ENDIF
      ENDDO
      PRINT *,'WRONG RJDAY',RJDAY
      CALL ABORT
   20 CONTINUE
      WEI1M=(DAYHF(MON2)-RJDAY)/(DAYHF(MON2)-DAYHF(MON1))
      WEI2M=(RJDAY-DAYHF(MON1))/(DAYHF(MON2)-DAYHF(MON1))
      IF(MON2.EQ.13) MON2=1
      if (me .eq. 0) PRINT *,'RJDAY,MON1,MON2,WEI1M,WEI2M=',
     &               RJDAY,MON1,MON2,WEI1M,WEI2M
!
!     For seasonal mean climatology
!
      MONEND = 4
      IS     = IM/3 + 1
      IF (IS.EQ.5) IS = 1
      DO MM=1,MONEND
        MMM = MM*3 - 2
        MMP = (MM+1)*3 - 2
        IF(RJDAY.GE.DAYHF(MMM).AND.RJDAY.LT.DAYHF(MMP)) THEN
          SEA1 = MMM
          SEA2 = MMP
          GO TO 30
        ENDIF
      ENDDO
      PRINT *,'WRONG RJDAY',RJDAY
      CALL ABORT
   30 CONTINUE
      WEI1S = (DAYHF(SEA2)-RJDAY)/(DAYHF(SEA2)-DAYHF(SEA1))
      WEI2S = (RJDAY-DAYHF(SEA1))/(DAYHF(SEA2)-DAYHF(SEA1))
      IF(SEA2.EQ.13) SEA2=1
      if (me .eq. 0) PRINT *,'RJDAY,SEA1,SEA2,WEI1S,WEI2S=',
     &               RJDAY,SEA1,SEA2,WEI1S,WEI2S
!
!  START READING IN CLIMATOLOGY AND INTERPOLATE TO THE DATE
!
      FIRST_TIME : if (first) then
!cbosu
        if (me == 0) print*,'bosu first time thru'
!
!     Annual mean climatology
!
!  Fraction of vegetation field for albedo --  There are two
!  fraction fields in this version: strong zeneith angle dependent
!  and weak zeneith angle dependent
!
        kpd9 = -1
cjfe
        alf=0.
cjfe

        kpd7=-1
        if (ialb == 1) then
!cbosu    still need facsf and facwf.  read them from the production
!cbosu    file
!cbosu
          CALL FIXRDC(LUGB,FNALBC2,KPDALF(1),kpd7,kpd9,SLMASK
     &,               ALF,LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
        else
          CALL FIXRDC(LUGB,FNALBC,KPDALF(1),kpd7,kpd9,SLMASK
     &,               ALF,LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
        endif
        do I = 1, LEN
          if(slmask(I).eq.1.) then
            alf(I,2) = 100. - alf(I,1)
          endif
        enddo
!
!  Deep Soil Temperature
!
        IF(FNTG3C(1:8).NE.'        ') THEN
          kpd7=-1
          CALL FIXRDC(LUGB,FNTG3C,KPDTG3,kpd7,kpd9,SLMASK,
     &                TG3,LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
        ENDIF
!
!  Vegetation type
!
        IF(FNVEtC(1:8).NE.'        ') THEN
          kpd7=-1
          CALL FIXRDC(LUGB,FNVEtC,KPDVEt,kpd7,kpd9,SLMASK,
     &                VEt,LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
          if (me .eq. 0) WRITE(6,*) 'Climatological vegetation',
     &                              ' type read in.'
        ENDIF
!
!  soil type
!
        IF(FNsotC(1:8).NE.'        ') THEN
          kpd7=-1
          CALL FIXRDC(LUGB,FNsotC,KPDsot,kpd7,kpd9,SLMASK,
     &                sot,LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
          if (me .eq. 0) WRITE(6,*) 'Climatological soil type read in.'
        ENDIF

!Clu ----------------------------------------------------------------------
!
!  Min vegetation cover
!
        IF(FNvmnC(1:8).NE.'        ') THEN
          kpd7=-1
          CALL FIXRDC(LUGB,FNvmnC,KPDvmn,kpd7,kpd9,SLMASK,
     &                vmn,LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
          if (me .eq. 0) WRITE(6,*) 'Climatological shdmin read in.'
        ENDIF
!
!  Max vegetation cover
!
        IF(FNvmxC(1:8).NE.'        ') THEN
          kpd7=-1
          CALL FIXRDC(LUGB,FNvmxC,KPDvmx,kpd7,kpd9,SLMASK,
     &                vmx,LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
          if (me .eq. 0) WRITE(6,*) 'Climatological shdmax read in.'
        ENDIF
!
!  Slope type
!
        IF(FNslpC(1:8).NE.'        ') THEN
          kpd7=-1
          CALL FIXRDC(LUGB,FNslpC,KPDslp,kpd7,kpd9,SLMASK,
     &                slp,LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
          if (me .eq. 0) WRITE(6,*) 'Climatological slope read in.'
        ENDIF
!
!  Max snow albeod
!
        IF(FNabsC(1:8).NE.'        ') THEN
          kpd7=-1
          CALL FIXRDC(LUGB,FNabsC,KPDabs,kpd7,kpd9,SLMASK,
     &                abs,LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
          if (me .eq. 0) WRITE(6,*) 'Climatological snoalb read in.'
        ENDIF
!Clu ----------------------------------------------------------------------
!
        IS1 = SEA1/3 + 1
        IS2 = SEA2/3 + 1
        IF (IS1 .EQ. 5) IS1 = 1
        IF (IS2 .EQ. 5) IS2 = 1
        DO NN=1,2
!
!     Seasonal mean climatology
          IF(NN.EQ.1) THEN
             ISX=IS1
          ELSE
             ISX=IS2
          ENDIF
          IF(ISX.EQ.1) kpd9 = 12
          IF(ISX.EQ.2) kpd9 = 3
          IF(ISX.EQ.3) kpd9 = 6
          IF(ISX.EQ.4) kpd9 = 9
!
!         Seasonal mean climatology
!
!  ALBEDO
!  There are four albedo fields in this version:
!  two for strong zeneith angle dependent (visible and near IR)
!  and two for weak zeneith angle dependent (VIS ANS NIR)
!
          if (ialb == 0) then
            kpd7=-1
            DO K = 1, 4
              CALL FIXRDC(LUGB,FNALBC,KPDALB(K),kpd7,kpd9,SLMASK,
     &                    ALB(1,K,NN),LEN,IRET
     &,                   IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                   OUTLAT, OUTLON, me)
            ENDDO
          endif
!
!         Monthly mean climatology
!
          MON = MON1
          if (NN .eq. 2) MON = MON2
!cbosu
!cbosu  new snowfree albedo database is monthly.  
          if (ialb == 1) then
            print*,'first call to fixrdc for snowfree alb ', first,
     &              nn, mon
            kpd7=-1
            DO K = 1, 4
              CALL FIXRDC(LUGB,FNALBC,KPDALB(K),kpd7,mon,SLMASK,
     &                    ALB(1,K,NN),LEN,IRET
     &,                   IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                   OUTLAT, OUTLON, me)
            ENDDO
          endif

!     if(lprnt) print *,' mon1=',mon1,' mon2=',mon2
!
!  TSF AT THE CURRENT TIME T
!
          kpd7=-1
          CALL FIXRDC(LUGB,FNTSFC,KPDTSF,kpd7,MON,SLMASK,
     &               TSF(1,NN),LEN,IRET
     &,              IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,              OUTLAT, OUTLON, me)
!     if(lprnt) print *,' tsf=',tsf(iprnt,nn),' nn=',nn
!
!  TSF...AT TIME T-DELTSFC
!
!     FH2 = FH - DELTSFC
!     IF (FH2 .GT. 0.0) THEN
!       CALL FIXRD(LUGB,FNTSFC,KPDTSF,LCLIM,SLMASK,
!    &             IY,IM,ID,IH,FH2,TSFCL2,LEN,IRET
!    &,            IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
!    &,            OUTLAT, OUTLON, me)
!     ELSE
!       DO I=1,LEN
!         TSFCL2(I) = TSFCLM(I)
!       ENDDO
!     ENDIF
!
!  SOIL WETNESS
!
          IF(FNWETC(1:8).NE.'        ') THEN
            kpd7=-1
            CALL FIXRDC(LUGB,FNWETC,KPDWET,kpd7,MON,SLMASK,
     &                  WET(1,NN),LEN,IRET
     &,                 IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                 OUTLAT, OUTLON, me)
          ELSEIF(FNSMCC(1:8).NE.'        ') THEN
            IF (INDEX(FNSMCC,'global_soilmcpc.1x1.grb') /= 0) THEN ! the old climo data
              kpd7=-1
              CALL FIXRDC(LUGB,FNSMCC,KPDSMC,kpd7,MON,SLMASK,
     &                    SMC(1,LSOIL,NN),LEN,IRET
     &,                   IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                   OUTLAT, OUTLON, me)
              do l=1,lsoil-1
                do i = 1, LEN
                 smc(i,l,NN) = smc(i,LSOIL,NN)
                enddo
              enddo
            ELSE
              DO K = 1, LSOIL
                IF (K==1) kpd7=10     ! 0_10 cm    (pds octs 11 and 12)
                IF (K==2) kpd7=2600   ! 10_40 cm
                IF (K==3) kpd7=10340  ! 40_100 cm
                IF (K==4) kpd7=25800  ! 100_200 cm
                ALLOCATE(SLMASK_NOICE(LEN))
                SLMASK_NOICE=0.0
                WHERE (NINT(VET)>0 .AND. NINT(VET)<13) SLMASK_NOICE=1.0
                CALL FIXRDC(LUGB,FNSMCC,KPDSMC,kpd7,MON,SLMASK_NOICE,
     &                      SMC(1,K,NN),LEN,IRET
     &,                     IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                     OUTLAT, OUTLON, me)
                DEALLOCATE(SLMASK_NOICE)
              ENDDO
            ENDIF
          ELSE
            WRITE(6,*) 'Climatological Soil wetness file not given'
            CALL ABORT
          ENDIF
!
!  SOIL TEMPERATURE
!
          IF(FNSTCC(1:8).NE.'        ') THEN
            kpd7=-1
            CALL FIXRDC(LUGB,FNSTCC,KPDSTC,kpd7,MON,SLMASK,
     &                  STC(1,LSOIL,NN),LEN,IRET
     &,                 IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                 OUTLAT, OUTLON, me)
            do l=1,lsoil-1
              do i = 1, LEN
               stc(i,l,NN) = stc(i,LSOIL,NN)
              enddo
            enddo
          ENDIF
!
!  SEA ICE
!
          kpd7=-1
          IF(FNACNC(1:8).NE.'        ') THEN
            CALL FIXRDC(LUGB,FNACNC,KPDACN,kpd7,MON,SLMASK,
     &                  ACN(1,NN),LEN,IRET
     &,                 IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                 OUTLAT, OUTLON, me)
          ELSEIF(FNAISC(1:8).NE.'        ') THEN
            CALL FIXRDC(LUGB,FNAISC,KPDAIS,kpd7,MON,SLMASK,
     &                  AIS(1,NN),LEN,IRET
     &,                 IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                 OUTLAT, OUTLON, me)
          ELSE
            WRITE(6,*) 'Climatological ice cover file not given'
            CALL ABORT
          ENDIF
!
!  SNOW DEPTH
!
          kpd7=-1
          CALL FIXRDC(LUGB,FNSNOC,KPDSNO,kpd7,MON,SLMASK,
     &                SNO(1,NN),LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
!
!  SNOW COVER
!
          IF(FNSCVC(1:8).NE.'        ') THEN
            kpd7=-1
            CALL FIXRDC(LUGB,FNSCVC,KPDSCV,kpd7,MON,SLMASK,
     &                  SCV(1,NN),LEN,IRET
     &,                 IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                 OUTLAT, OUTLON, me)
            WRITE(6,*) 'Climatological snow cover read in.'
          ENDIF
!
!  SURFACE ROUGHNESS
!
      IF(FNZORC(1:3) == 'sib') THEN
        if (me == 0) then
          WRITE(6,*) 'Roughness length to be set from sib veg type'
        endif
      ELSE
        kpd7=-1
        CALL FIXRDC(LUGB,FNZORC,KPDZOR,kpd7,MON,SLMASK,
     &              ZOR(1,NN),LEN,IRET
     &,             IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,             OUTLAT, OUTLON, me)
      ENDIF
!
          DO I = 1, LEN
!                           Set clouds climatology to zero
            CVCLM (I) = 0.
            CVBCLM(I) = 0.
            CVTCLM(I) = 0.
!
            CNPCLM(I) = 0.  !Set canopy water content climatology to zero
          ENDDO
!
!  Vegetation cover
!
          IF(FNVEGC(1:8).NE.'        ') THEN
            kpd7=-1
            CALL FIXRDC(LUGB,FNVEGC,KPDVEG,kpd7,MON,SLMASK,
     &                  VEG(1,NN),LEN,IRET
     &,                 IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                 OUTLAT, OUTLON, me)
            if (me .eq. 0) WRITE(6,*) 'Climatological vegetation',
     &                                ' cover read in for mon=',mon
          ENDIF

        ENDDO
!
      mon1s = mon1 ; mon2s = mon2 ; sea1s = sea1 ; sea2s = sea2
!
      if (me .eq. 0) print *,' mon1s=',mon1s,' mon2s=',mon2s
     &,' sea1s=',sea1s,' sea2s=',sea2s
!
        k1  = 1 ; k2 = 2
        m1  = 1 ; m2 = 2
!
        first = .false.
      endif FIRST_TIME
!
!     To get TSF climatology at the previous call to SFCCYCLE
!
      rjdayh = rjday - deltsfc/24.0
!     if(lprnt) print *,' rjdayh=',rjdayh,' mon1=',mon1,' mon2='
!    &,mon2,' mon1s=',mon1s,' mon2s=',mon2s,' k1=',k1,' k2=',k2
      if (rjdayh .ge. DAYHF(mon1)) then
        if (mon2 .eq. 1) mon2 = 13
        WEI1X = (DAYHF(mon2)-rjdayh)/(DAYHF(mon2)-DAYHF(mon1))
        WEI2X = 1.0 - WEI1X
        if (mon2 .eq. 13) mon2 = 1
      else
        rjdayh2 = rjdayh
        IF (RJDAYh .LT. DAYHF(1)) RJDAYh2 = RJDAYh2 + 365.0
        if (mon1s .eq. mon1) then
          mon1s = mon1 - 1
          if (mon1s .eq. 0) mon1s = 12
          k2  = k1
          k1  = mod(k2,2) + 1
          MON = mon1s
          kpd7=-1
          CALL FIXRDC(LUGB,FNTSFC,KPDTSF,kpd7,MON,SLMASK,
     &               TSF(1,k1),LEN,IRET
     &,              IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,              OUTLAT, OUTLON, me)
        endif
        mon2s = mon1s + 1
!       if (mon2s .eq. 1) mon2s = 13
        WEI1X = (DAYHF(mon2s)-rjdayh2)/(DAYHF(mon2s)-DAYHF(mon1s))
        WEI2X = 1.0 - WEI1X
        if (mon2s .eq. 13) mon2s = 1
        do i=1,len
          tsf2(I) = wei1x * tsf(i,k1) + wei2x * tsf(i,k2)
        enddo
      endif
!
!cbosu new albedo is monthly
      if (sea1 .ne. sea1s) then
         sea1s = sea1
         sea2s = sea2
         m1    = mod(m1,2) + 1
         m2    = mod(m1,2) + 1
!
!     Seasonal mean climatology
!
         ISX   = SEA2/3 + 1
         IF (ISX .EQ. 5) ISX = 1
         IF(ISX.EQ.1) kpd9 = 12
         IF(ISX.EQ.2) kpd9 = 3
         IF(ISX.EQ.3) kpd9 = 6
         IF(ISX.EQ.4) kpd9 = 9
!
!  ALBEDO
!  There are four albedo fields in this version:
!  two for strong zeneith angle dependent (visible and near IR)
!  and two for weak zeneith angle dependent (VIS ANS NIR)
!
!cbosu  
        if (ialb == 0) then
           kpd7=-1
           DO K = 1, 4
             CALL FIXRDC(LUGB,FNALBC,KPDALB(K),kpd7,kpd9,SLMASK
     &,                 ALB(1,K,m2),LEN,IRET
     &,                 IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                 OUTLAT, OUTLON, me)
           ENDDO
        endif

      endif

      if (mon1 .ne. mon1s) then

         mon1s = mon1
         mon2s = mon2
         k1    = mod(k1,2) + 1
         k2    = mod(k1,2) + 1
!
!     Monthly mean climatology
!
          MON = MON2
          NN  = k2
!cbosu
          if (ialb == 1) then
            if (me == 0) print*,'bosu 2nd time in clima for month ',
     &                   mon, k1,k2
            kpd7=-1
            DO K = 1, 4
              CALL FIXRDC(LUGB,FNALBC,KPDALB(K),kpd7,MON,SLMASK,
     &                    ALB(1,K,NN),LEN,IRET
     &,                   IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                   OUTLAT, OUTLON, me)
            ENDDO
          endif
!
!  TSF AT THE CURRENT TIME T
!
          kpd7=-1
          CALL FIXRDC(LUGB,FNTSFC,KPDTSF,kpd7,MON,SLMASK,
     &               TSF(1,NN),LEN,IRET
     &,              IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,              OUTLAT, OUTLON, me)
!
!  SOIL WETNESS
!
          IF(FNWETC(1:8).NE.'        ') THEN
            kpd7=-1
            CALL FIXRDC(LUGB,FNWETC,KPDWET,kpd7,MON,SLMASK,
     &                  WET(1,NN),LEN,IRET
     &,                 IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                 OUTLAT, OUTLON, me)
          ELSEIF(FNSMCC(1:8).NE.'        ') THEN
            IF (INDEX(FNSMCC,'global_soilmcpc.1x1.grb') /= 0) THEN ! the old climo data
              kpd7=-1
              CALL FIXRDC(LUGB,FNSMCC,KPDSMC,kpd7,MON,SLMASK,
     &                    SMC(1,LSOIL,NN),LEN,IRET
     &,                   IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                   OUTLAT, OUTLON, me)
              do l=1,lsoil-1
                do i = 1, LEN
                 smc(i,l,NN) = smc(i,LSOIL,NN)
                enddo
              enddo
            ELSE
              DO K = 1, LSOIL
                IF (K==1) kpd7=10     ! 0_10 cm   (pds octs 11 and 12)
                IF (K==2) kpd7=2600   ! 10_40 cm
                IF (K==3) kpd7=10340  ! 40_100 cm
                IF (K==4) kpd7=25800  ! 100_200 cm
                ALLOCATE(SLMASK_NOICE(LEN))
                SLMASK_NOICE=0.0
                WHERE (NINT(VET)>0 .AND. NINT(VET)<13) SLMASK_NOICE=1.0
                CALL FIXRDC(LUGB,FNSMCC,KPDSMC,kpd7,MON,SLMASK_NOICE,
     &                      SMC(1,K,NN),LEN,IRET
     &,                     IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                     OUTLAT, OUTLON, me)
                DEALLOCATE(SLMASK_NOICE)
              ENDDO
            ENDIF
          ELSE
            WRITE(6,*) 'Climatological Soil wetness file not given'
            CALL ABORT
          ENDIF
!
!  SEA ICE
!
          kpd7=-1
          IF(FNACNC(1:8).NE.'        ') THEN
            CALL FIXRDC(LUGB,FNACNC,KPDACN,kpd7,MON,SLMASK,
     &                  ACN(1,NN),LEN,IRET
     &,                 IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                 OUTLAT, OUTLON, me)
          ELSEIF(FNAISC(1:8).NE.'        ') THEN
            CALL FIXRDC(LUGB,FNAISC,KPDAIS,kpd7,MON,SLMASK,
     &                  AIS(1,NN),LEN,IRET
     &,                 IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                 OUTLAT, OUTLON, me)
          ELSE
            WRITE(6,*) 'Climatological ice cover file not given'
            CALL ABORT
          ENDIF
!
!  SNOW DEPTH
!
          kpd7=-1
          CALL FIXRDC(LUGB,FNSNOC,KPDSNO,kpd7,MON,SLMASK,
     &                SNO(1,NN),LEN,IRET
     &,               IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,               OUTLAT, OUTLON, me)
!
!  SNOW COVER
!
          IF(FNSCVC(1:8).NE.'        ') THEN
            kpd7=-1
            CALL FIXRDC(LUGB,FNSCVC,KPDSCV,kpd7,MON,SLMASK,
     &                  SCV(1,NN),LEN,IRET
     &,                 IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                 OUTLAT, OUTLON, me)
            WRITE(6,*) 'Climatological snow cover read in.'
          ENDIF
!
!  SURFACE ROUGHNESS
!
      IF(FNZORC(1:3) == 'sib') THEN
        if (me == 0) then
          WRITE(6,*) 'Roughness length to be set from sib veg type'
        endif
      ELSE
        kpd7=-1
        CALL FIXRDC(LUGB,FNZORC,KPDZOR,kpd7,MON,SLMASK,
     &              ZOR(1,NN),LEN,IRET
     &,             IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,             OUTLAT, OUTLON, me)
      ENDIF
!
!  Vegetation cover
!
          IF(FNVEGC(1:8).NE.'        ') THEN
            kpd7=-1
            CALL FIXRDC(LUGB,FNVEGC,KPDVEG,kpd7,MON,SLMASK,
     &                  VEG(1,NN),LEN,IRET
     &,                 IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                 OUTLAT, OUTLON, me)
!           if (me .eq. 0) WRITE(6,*) 'Climatological vegetation',
!    &                                ' cover read in for mon=',mon
          ENDIF
!
      endif
!
!     Now perform the time interpolation
!
! when chosen, set the z0 based on the vegetation type.
! for this option to work, namelist variable FNVETC must be
! set to point at the sib vegetation type file.
      IF(FNZORC(1:3) == 'sib') THEN
        IF(FNVETC(1:4) == '   ') THEN
          IF (ME==0) WRITE(6,*) "MUST CHOOSE SIB VEG TYPE CLIMO FILE"
          CALL ABORT
        ENDIF
        ZORCLM = 0.0
        DO I=1,LEN
          IVTYP=NINT(VET(I))
          IF (IVTYP >= 1 .AND. IVTYP <= 13) THEN
            ZORCLM(I) = Z0_SIB(IVTYP)
          ENDIF
        ENDDO
      ELSE
        DO I=1,LEN
          ZORCLM(I) = wei1m * zor(i,k1) + wei2m * zor(i,k2)
        ENDDO
      ENDIF
!
      DO I=1,LEN
        TSFCLM(I) = wei1m * tsf(i,k1) + wei2m * tsf(i,k2)
        SNOCLM(I) = wei1m * sno(i,k1) + wei2m * sno(i,k2)
        CVCLM(I)  = 0.0
        CVBCLM(I) = 0.0
        CVTCLM(I) = 0.0
        CNPCLM(I) = 0.0
        tsfcl2(I) = tsf2(i)
      ENDDO
!     if(lprnt) print *,' tsfclm=',tsfclm(iprnt),' wei1m=',wei1m
!    &,' wei2m=',wei2m,' tsfk12=',tsf(iprnt,k1),tsf(iprnt,k2)
!
      if (fh .eq. 0.0) then
        do i=1,len
          TSFCL0(i) = tsfclm(i)
        enddo
      endif
      if (rjdayh .ge. DAYHF(mon1)) then
        do i=1,len
          tsf2(I)   = wei1x * tsf(i,k1) + wei2x * tsf(i,k2)
          tsfcl2(I) = tsf2(i)
        enddo
      endif
!     if(lprnt) print *,' tsf2=',tsf2(iprnt),' wei1x=',wei1x
!    &,' wei2x=',wei2x,' tsfk12=',tsf(iprnt,k1),tsf(iprnt,k2)
!    &,' mon1s=',mon1s,' mon2s=',mon2s
!    &,' slmask=',slmask(iprnt)
!
      IF(FNACNC(1:8).NE.'        ') THEN
        DO I=1,LEN
          ACNCLM(I) = wei1m * acn(i,k1) + wei2m * acn(i,k2)
        ENDDO
      ELSEIF(FNAISC(1:8).NE.'        ') THEN
        DO I=1,LEN
          AISCLM(I) = wei1m * ais(i,k1) + wei2m * ais(i,k2)
        ENDDO
      endif
!
      IF(FNWETC(1:8).NE.'        ') THEN
        DO I=1,LEN
          WETCLM(I) = wei1m * wet(i,k1) + wei2m * wet(i,k2)
        ENDDO
      ELSEIF(FNSMCC(1:8).NE.'        ') THEN
        DO K=1,LSOIL
          DO I=1,LEN
            SMCCLM(I,K) = wei1m * smc(i,k,k1) + wei2m * smc(i,k,k2)
          ENDDO
        ENDDO
      endif
!
      IF(FNSCVC(1:8).NE.'        ') THEN
        DO I=1,LEN
          SCVCLM(I) = wei1m * scv(i,k1) + wei2m * scv(i,k2)
        ENDDO
      endif
!
      IF(FNTG3C(1:8).NE.'        ') THEN
        DO I=1,LEN
          TG3CLM(I) =         TG3(i)
        ENDDO
      ELSEIF(FNSTCC(1:8).NE.'        ') THEN
        DO K=1,LSOIL
          DO I=1,LEN
            STCCLM(I,K) = wei1m * stc(i,k,k1) + wei2m * stc(i,k,k2)
          ENDDO
        ENDDO
      endif
!
      IF(FNVEGC(1:8).NE.'        ') THEN
        DO I=1,LEN
          VEGCLM(I) = wei1m * veg(i,k1) + wei2m * veg(i,k2)
        ENDDO
      endif
!
      IF(FNVEtC(1:8).NE.'        ') THEN
        DO I=1,LEN
          VETCLM(I) =         vet(i)
        ENDDO
      endif
!
      IF(FNsotC(1:8).NE.'        ') THEN
        DO I=1,LEN
          SOTCLM(I) =         sot(i)
        ENDDO
      endif


!Clu ----------------------------------------------------------------------
!
      IF(FNvmnC(1:8).NE.'        ') THEN
        DO I=1,LEN
          VMNCLM(I) =         vmn(i)
        ENDDO
      endif
!
      IF(FNvmxC(1:8).NE.'        ') THEN
        DO I=1,LEN
          VMXCLM(I) =         vmx(i)
        ENDDO
      endif
!
      IF(FNslpC(1:8).NE.'        ') THEN
        DO I=1,LEN
          SLPCLM(I) =         slp(i)
        ENDDO
      endif
!
      IF(FNabsC(1:8).NE.'        ') THEN
        DO I=1,LEN
          ABSCLM(I) =         abs(i)
        ENDDO
      endif
!Clu ----------------------------------------------------------------------
!
!cbosu  diagnostic print
      if (me == 0) print*,'monthly albedo weights are ', 
     &             wei1m,' for k', k1, wei2m, ' for k', k2

      if (ialb == 1) then
        DO K=1,4
          DO I=1,LEN
            ALBCLM(I,K) = wei1m * alb(i,k,k1) + wei2m * alb(i,k,k2)
          ENDDO
        ENDDO
      else
        DO K=1,4
          DO I=1,LEN
            ALBCLM(I,K) = wei1s * alb(i,k,m1) + wei2s * alb(i,k,m2)
          ENDDO
        ENDDO
      endif
!
      DO K=1,2
        DO I=1,LEN
          ALFCLM(I,K) = alf(i,k)
        ENDDO
      ENDDO
!
!  END OF CLIMATOLOGY READS
!
      RETURN
      END
      SUBROUTINE FIXRDC(LUGB,FNGRIB,KPDS5,KPDS7,MON,SLMASK,
     &                 GDATA,LEN,IRET
     &,                IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                OUTLAT, OUTLON, me)
      USE MACHINE ,      ONLY : kind_io8,kind_io4
      use sfccyc_module, only : mdata
      implicit none
      integer imax,jmax,ijmax,i,j,n,jret,inttyp,iret,imsk,
     &        jmsk,len,lugb,kpds5,mon,lskip,lgrib,ndata,lugi,me,KMAMI
     &,       jj
      REAL (KIND=KIND_IO8) wlon,elon,rnlat,dlat,dlon,rslat,blno,blto
!
!   Read in GRIB climatology files and interpolate to the input
!   grid.  GRIB files should allow all the necessary parameters
!   to be extracted from the description records.
!
!
      CHARACTER*500 FNGRIB
!     CHARACTER*80 FNGRIB, ASGNSTR
!
      REAL (KIND=KIND_IO8) SLMSKH(IMSK,JMSK)
!
      REAL (KIND=KIND_IO8) GDATA(LEN), SLMASK(LEN)
      REAL (KIND=KIND_IO8), allocatable :: DATA(:,:), RSLMSK(:,:)
      real(kind=kind_io8) data4(mdata)
      real (kind=kind_io8), allocatable :: rlngrb(:), rltgrb(:)
!
      LOGICAL LMASK, YR2KC, GAUS, IJORDR
      LOGICAL*1 LBMS(mdata)
!
      INTEGER, INTENT(IN) :: KPDS7
      INTEGER KPDS(1000),KGDS(1000)
      INTEGER JPDS(1000),JGDS(1000), KPDS0(1000)
      real (kind=kind_io8) outlat(len), outlon(len)
!
!     integer imax_sv, jmax_sv, wlon_sv, rnlat_sv, kpds1_sv
!     date imax_sv/0/, jmax_sv/0/, wlon_sv/999.0/, rnlat_sv/999.0/
!    &,    kpds1_sv/-1/
!     save imax_sv, jmax_sv, wlon_sv, rnlat_sv, kpds1_sv
!    &,    rlngrb, rltgrb
!
      IRET   = 0
!
      if (me .eq. 0) WRITE(6,*) ' IN FIXRDC for MON=',MON
     &,' FNGRIB=',trim(FNGRIB)
!
      CLOSE(LUGB)
      call baopenr(lugb,fngrib,iret)
      IF (IRET .NE. 0) THEN
        WRITE(6,*) ' ERROR IN OPENING FILE ',trim(FNGRIB)
        PRINT *,'ERROR IN OPENING FILE ',trim(FNGRIB)
        CALL ABORT
      ENDIF
      if (me .eq. 0) WRITE(6,*) ' FILE ',trim(FNGRIB),
     &             ' opened. Unit=',LUGB
!
      lugi = 0
!
      lskip   = -1
      JPDS    = -1
      JGDS    = -1
      JPDS(5) = KPDS5
      JPDS(7) = KPDS7
      kpds    = jpds
      call getgbh(lugb,lugi,lskip,jpds,jgds,lgrib,ndata,
     &            lskip,kpds,kgds,iret)
      if (me .eq. 0) then
      WRITE(6,*) ' First grib record.'
      WRITE(6,*) ' KPDS( 1-10)=',(KPDS(J),J= 1,10)
      WRITE(6,*) ' KPDS(11-20)=',(KPDS(J),J=11,20)
      WRITE(6,*) ' KPDS(21-  )=',(KPDS(J),J=21,22)
      endif
      yr2kc     = (kpds(8) / 100) .gt. 0
      KPDS0     = JPDS
      KPDS0(4)  = -1
      KPDS0(18) = -1
      IF(IRET.NE.0) THEN
        WRITE(6,*) ' Error in GETGBH. IRET: ', iret
        IF (IRET==99) WRITE(6,*) ' Field not found.'
        CALL ABORT
      ENDIF
!
!   Handling climatology file
!
      lskip   = -1
      N       = 0
      JPDS    = KPDS0
      JPDS(9) = MON
      IF(JPDS(9).EQ.13) JPDS(9) = 1
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &          kpds,kgds,lbms,data4,jret)
      if (me .eq. 0) WRITE(6,*) ' Input grib file dates=',
     &              (KPDS(I),I=8,11)
      if(jret.eq.0) then
        IF(NDATA.EQ.0) THEN
          WRITE(6,*) ' Error in getgb'
          WRITE(6,*) ' KPDS=',KPDS
          WRITE(6,*) ' KGDS=',KGDS
          CALL ABORT
        ENDIF
        IMAX=KGDS(2)
        JMAX=KGDS(3)
        IJMAX=IMAX*JMAX
        allocate (data(imax,jmax))
        do j=1,jmax
          jj = (j-1)*imax
          do i=1,imax
            data(i,j) = data4(jj+i)
          enddo
        enddo
        if (me .eq. 0) WRITE(6,*) 'IMAX,JMAX,IJMAX=',IMAX,JMAX,IJMAX
      ELSE
        WRITE(6,*) ' Error in getgb - jret=', jret
        CALL ABORT
      ENDIF
!
      if (me .eq. 0) then
      WRITE(6,*) ' MAXMIN of input as is'
      KMAMI=1
      CALL MAXMIN(DATA(1,1),IJMAX,KMAMI)
      endif
!
      CALL GETAREA(KGDS,DLAT,DLON,RSLAT,RNLAT,WLON,ELON,IJORDR,me)
      if (me .eq. 0) then
      WRITE(6,*) 'IMAX,JMAX,IJMAX,DLON,DLAT,IJORDR,WLON,RNLAT='
      WRITE(6,*)  IMAX,JMAX,IJMAX,DLON,DLAT,IJORDR,WLON,RNLAT
      endif
      CALL SUBST(DATA,IMAX,JMAX,DLON,DLAT,IJORDR)
!
!   First get SLMASK over input grid
!
        allocate (rlngrb(imax), rltgrb(jmax))
        allocate (rslmsk(imax,jmax))

        CALL SETRMSK(KPDS5,SLMSKH,IMSK,JMSK,WLON,RNLAT,
     &               DATA,IMAX,JMAX,RLNGRB,RLTGRB,LMASK,RSLMSK
!    &               DATA,IMAX,JMAX,ABS(DLON),ABS(DLAT),LMASK,RSLMSK
!cggg
     &,                  GAUS,BLNO, BLTO, kgds(1), kpds(4), lbms)
!       WRITE(6,*) ' KPDS5=',KPDS5,' LMASK=',LMASK
!
                         INTTYP = 0
        IF(KPDS5.EQ.225) INTTYP = 1
        IF(KPDS5.EQ.230) INTTYP = 1
!Clu [+1L] add slope (=236)
        IF(KPDS5.EQ.236) INTTYP = 1  
        if (me .eq. 0) then
        if(inttyp.eq.1) print *, ' Nearest grid point used'
     &,   ' kpds5=',kpds5, ' lmask = ',lmask
        endif
!
        CALL LA2GA(DATA,IMAX,JMAX,RLNGRB,RLTGRB,WLON,RNLAT,INTTYP,
     &             GDATA,LEN,LMASK,RSLMSK,SLMASK
     &,            OUTLAT, OUTLON,me)
!
        deallocate (rlngrb, STAT=iret)
        deallocate (rltgrb, STAT=iret)
        deallocate (data, STAT=iret)
        deallocate (rslmsk, STAT=iret)
      call baclose(lugb,iret)
!
      RETURN
      END
      SUBROUTINE FIXRDA(LUGB,FNGRIB,KPDS5,SLMASK,
     &                 IY,IM,ID,IH,FH,GDATA,LEN,IRET
     &,                IMSK, JMSK, SLMSKH, GAUS,BLNO, BLTO
     &,                OUTLAT, OUTLON, me)
      USE MACHINE      , ONLY : kind_io8,kind_io4
      use sfccyc_module, only : mdata
      implicit none
      integer nrepmx,nvalid,imo,iyr,idy,jret,ihr,nrept,lskip,lugi,
     &        lgrib,j,ndata,i,inttyp,jmax,imax,ijmax,ij,jday,len,iret,
     &        jmsk,imsk,ih,kpds5,lugb,iy,id,im,jh,jd,jdoy,jdow,jm,me,
     &        monend,jy,iy4,KMAMI,iret2,jj
      REAL (KIND=KIND_IO8) rnlat,rslat,wlon,elon,dlon,dlat,fh,blno,
     &                     rjday,blto
!
!   Read in GRIB climatology/analysis files and interpolate to the input
!   dates and the grid.  GRIB files should allow all the necessary parameters
!   to be extracted from the description records.
!
!  NREPMX:  Max number of days for going back date search
!  NVALID:  Analysis later than (Current date - NVALID) is regarded as
!           valid for current analysis
!
      PARAMETER(NREPMX=15, NVALID=4)
!
      CHARACTER*500 FNGRIB
!     CHARACTER*80 FNGRIB, ASGNSTR
!
      REAL (KIND=KIND_IO8) SLMSKH(IMSK,JMSK)
!
      REAL (KIND=KIND_IO8) GDATA(LEN), SLMASK(LEN)
      REAL (KIND=KIND_IO8), allocatable :: DATA(:,:),RSLMSK(:,:)
      real(kind=kind_io8) data4(mdata)
      real (kind=kind_io8), allocatable :: rlngrb(:), rltgrb(:)
!
      LOGICAL LMASK, YR2KC, GAUS, IJORDR
      LOGICAL*1  LBMS(mdata)
!
      INTEGER KPDS(1000),KGDS(1000)
      INTEGER JPDS(1000),JGDS(1000), KPDS0(1000)
      real (kind=kind_io8) outlat(len), outlon(len)
!
! DAYHF : JULIAN DAY OF THE MIDDLE OF EACH MONTH
!
      REAL (KIND=KIND_IO8) DAYHF(13)
      DATA DAYHF/ 15.5, 45.0, 74.5,105.0,135.5,166.0,
     &           196.5,227.5,258.0,288.5,319.0,349.5,380.5/
!
! MJDAY : NUMBER OF DAYS IN A MONTH
!
      INTEGER MJDAY(12)
      DATA MJDAY/31,28,31,30,31,30,31,31,30,31,30,31/
!
      real (kind=kind_io8) fha(5)
      integer ida(8),jda(8)

      IRET   = 0
      MONEND = 9999
!
!  Compute JY,JM,JD,JH of forecast and the day of the year
!
      iy4=iy
      if(iy.lt.101) iy4=1900+iy4
      fha=0
      ida=0
      jda=0
      fha(2)=nint(fh)
      ida(1)=iy
      ida(2)=im
      ida(3)=id
      ida(5)=ih
      call w3movdat(fha,ida,jda)
      jy=jda(1)
      jm=jda(2)
      jd=jda(3)
      jh=jda(5)
!     if (me .eq. 0) write(6,*) ' Forecast JY,JM,JD,JH,rjday=',
!    &               jy,jm,jd,jh,rjday
      jdow = 0
      jdoy = 0
      jday = 0
      call w3doxdat(jda,jdow,jdoy,jday)
      rjday=jdoy+jda(5)/24.
      IF(RJDAY.LT.DAYHF(1)) RJDAY=RJDAY+365.

      if (me .eq. 0) write(6,*) ' Forecast JY,JM,JD,JH,rjday=',
     &               jy,jm,jd,jh,rjday
!
      if (me .eq. 0) then
      WRITE(6,*) 'Forecast JY,JM,JD,JH=',JY,JM,JD,JH
!
      WRITE(6,*) ' '
      WRITE(6,*) '************************************************'
      endif
!
      CLOSE(LUGB)
      call baopenr(lugb,fngrib,iret)
      IF (IRET .NE. 0) THEN
        WRITE(6,*) ' ERROR IN OPENING FILE ',trim(FNGRIB)
        PRINT *,'ERROR IN OPENING FILE ',trim(FNGRIB)
        CALL ABORT
      ENDIF
      if (me .eq. 0) WRITE(6,*) ' FILE ',trim(FNGRIB),
     &             ' opened. Unit=',LUGB
!
      lugi = 0
!
      lskip=-1
      JPDS=-1
      JGDS=-1
      JPDS(5)=KPDS5
      kpds = jpds
      call getgbh(lugb,lugi,lskip,jpds,jgds,lgrib,ndata,
     &            lskip,kpds,kgds,iret)
      if (me .eq. 0) then
      WRITE(6,*) ' First grib record.'
      WRITE(6,*) ' KPDS( 1-10)=',(KPDS(J),J= 1,10)
      WRITE(6,*) ' KPDS(11-20)=',(KPDS(J),J=11,20)
      WRITE(6,*) ' KPDS(21-  )=',(KPDS(J),J=21,22)
      endif
      yr2kc = (kpds(8) / 100) .gt. 0
      KPDS0=JPDS
      KPDS0(4)=-1
      KPDS0(18)=-1
      IF(IRET.NE.0) THEN
        WRITE(6,*) ' Error in GETGBH. IRET: ', iret
        IF(IRET==99) WRITE(6,*) ' Field not found.'
        CALL ABORT
      ENDIF
!
!  Handling analysis file
!
!  Find record for the given hour/day/month/year
!
      NREPT=0
      JPDS=KPDS0
      lskip = -1
      IYR=JY
      if(iyr.le.100) iyr=2050-mod(2050-iyr,100)
      IMO=JM
      IDY=JD
      IHR=JH
!     Year 2000 compatible data
      if (yr2kc) then
         jpds(8) = iyr
      else
         jpds(8) = mod(iyr,1900)
      endif
   50 CONTINUE
      JPDS( 8)=MOD(IYR-1,100)+1
      JPDS( 9)=IMO
      JPDS(10)=IDY
      JPDS(11)=IHR
      JPDS(21)=(IYR-1)/100+1
      if (me .eq. 0) write(6,*) ' Will search for date ',jpds(8:11)
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data4,jret)
      if (me .eq. 0) WRITE(6,*) ' Input grib file dates=',
     &              (KPDS(I),I=8,11)
      IF(jret.eq.0) THEN
        IF(NDATA.EQ.0) THEN
          WRITE(6,*) ' Error in getgb'
          WRITE(6,*) ' KPDS=',KPDS
          WRITE(6,*) ' KGDS=',KGDS
          CALL ABORT
        ENDIF
        IMAX=KGDS(2)
        JMAX=KGDS(3)
        IJMAX=IMAX*JMAX
        allocate (data(imax,jmax))
        do j=1,jmax
          jj = (j-1)*imax
          do i=1,imax
            data(i,j) = data4(jj+i)
          enddo
        enddo
      ELSE
        IF(NREPT.EQ.0) THEN
          if (me .eq. 0) then
          WRITE(6,*) ' No matching dates found.  Start searching',
     &               ' nearest matching dates (going back).'
          endif
        ENDIF
!
!  No matching IH found. Search nearest hour
!
        IF(IHR.GE.1.AND.IHR.LE.23) THEN
          IHR=IHR-1
          GO TO 50
        ELSEIF(IHR.EQ.0.OR.IHR.EQ.-1) THEN
          IDY=IDY-1
          IF(IDY.EQ.0) THEN
            IMO=IMO-1
            IF(IMO.EQ.0) THEN
              IYR=IYR-1
              IF(IYR.LT.0) IYR=99
              IMO=12
            ENDIF
            IDY=31
            IF(IMO.EQ.4.OR.IMO.EQ.6.OR.IMO.EQ.9.OR.IMO.EQ.11) IDY=30
            IF(IMO.EQ.2) THEN
              IF(MOD(IYR,4).EQ.0) THEN
                IDY=29
              ELSE
                IDY=28
              ENDIF
            ENDIF
          ENDIF
          IHR=-1
          if (me .eq. 0) WRITE(6,*) ' Decremented dates=',
     &                              IYR,IMO,IDY,IHR
          NREPT=NREPT+1
          IF(NREPT.GT.NVALID) IRET=-1
          IF(NREPT.GT.NREPMX) THEN
            if (me .eq. 0) then
              WRITE(6,*) ' <WARNING:CYCL> Searching range exceeded.'
     &,                  ' May be WRONG grib file given'
              WRITE(6,*) ' <WARNING:CYCL> FNGRIB=',trim(FNGRIB)
              WRITE(6,*) ' <WARNING:CYCL> Terminating search and',
     &                   ' and setting gdata to -999'
              WRITE(6,*) ' Range max=',NREPMX
            endif
!           IMAX=KGDS(2)
!           JMAX=KGDS(3)
!           IJMAX=IMAX*JMAX
!           DO IJ=1,IJMAX
!             DATA(IJ)=0.
!           ENDDO
            GO TO 100
          ENDIF
          GO TO 50
        ELSE
          if (me .eq. 0) then
            WRITE(6,*) ' Search of analysis for IHR=',IHR,' failed.'
            WRITE(6,*) ' KPDS=',KPDS
            WRITE(6,*) ' IYR,IMO,IDY,IHR=',IYR,IMO,IDY,IHR
          endif
          GO TO 100
        ENDIF
      ENDIF
!
   80 CONTINUE
      if (me .eq. 0) then
      WRITE(6,*) ' MAXMIN of input as is'
      KMAMI=1
      CALL MAXMIN(DATA(1,1),IJMAX,KMAMI)
      endif
!
      CALL GETAREA(KGDS,DLAT,DLON,RSLAT,RNLAT,WLON,ELON,IJORDR,me)
      if (me .eq. 0) then
      WRITE(6,*) 'IMAX,JMAX,IJMAX,DLON,DLAT,IJORDR,WLON,RNLAT='
      WRITE(6,*)  IMAX,JMAX,IJMAX,DLON,DLAT,IJORDR,WLON,RNLAT
      endif
      CALL SUBST(DATA,IMAX,JMAX,DLON,DLAT,IJORDR)
!
!   First get SLMASK over input grid
!
        allocate (rlngrb(imax), rltgrb(jmax))
        allocate (rslmsk(imax,jmax))
        CALL SETRMSK(KPDS5,SLMSKH,IMSK,JMSK,WLON,RNLAT,
     &               DATA,IMAX,JMAX,RLNGRB,RLTGRB,LMASK,RSLMSK
!    &               DATA,IMAX,JMAX,ABS(DLON),ABS(DLAT),LMASK,RSLMSK
!cggg     &,                  GAUS,BLNO, BLTO, kgds(1))
     &,                  GAUS,BLNO, BLTO, kgds(1), kpds(4), lbms)

!       WRITE(6,*) ' KPDS5=',KPDS5,' LMASK=',LMASK
!
                         INTTYP = 0
        IF(KPDS5.EQ.225) INTTYP = 1
        IF(KPDS5.EQ.230) INTTYP = 1
        IF(KPDS5.EQ.66)  INTTYP = 1
        if(inttyp.eq.1) print *, ' Nearest grid point used'
!
        CALL LA2GA(DATA,IMAX,JMAX,RLNGRB,RLTGRB,WLON,RNLAT,INTTYP,
     &             GDATA,LEN,LMASK,RSLMSK,SLMASK
     &,            OUTLAT, OUTLON, me)
!
      deallocate (rlngrb, STAT=iret)
      deallocate (rltgrb, STAT=iret)
      deallocate (data, STAT=iret)
      deallocate (rslmsk, STAT=iret)
      call baclose(lugb,iret2)
!     WRITE(6,*) ' '
      RETURN
!
  100 CONTINUE
      IRET=1
      DO I=1,LEN
        GDATA(I) = -999.
      ENDDO
!
      call baclose(lugb,iret2)
!
      RETURN
      END SUBROUTINE FIXRDA
      SUBROUTINE SNODPTH2(GLACIR,SNWMAX,SNOANL, LEN, me)
      USE MACHINE , ONLY : kind_io8,kind_io4
      implicit none
      integer i,me,len
      REAL (KIND=KIND_IO8) snwmax
!
      REAL (KIND=KIND_IO8) SNOANL(LEN), GLACIR(LEN)
!
      if (me .eq. 0) WRITE(6,*) 'SNODPTH2'
!
      DO I=1,LEN
!
!  IF GLACIAL POINTS HAS SNOW IN CLIMATOLOGY, SET SNO TO SNOMAX
!
        IF(GLACIR(I).NE.0..AND.SNOANL(I).LT.SNWMAX*0.5) THEN
            SNOANL(I) = SNWMAX + SNOANL(I)
        ENDIF
!
      ENDDO
      RETURN
      END
