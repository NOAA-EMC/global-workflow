  module CMASSI_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   implicit none
!
!-----------------------------------------------------------------------
      REAL, PARAMETER :: DMImin=.05e-3, DMImax=1.e-3,      &
     &  XMImin=1.e6*DMImin, XMImax=1.e6*DMImax
      INTEGER, PARAMETER :: MDImin=XMImin, MDImax=XMImax
!-----------------------------------------------------------------------
!--- Mean mass of precpitation ice particles as functions of their mean
!    size (in microns)
!
      REAL MASSI(MDImin:MDImax)
!
!--- Mean rain drop diameters vary from 50 microns to 1000 microns
! DMRmax definition is moved to microinit and has different values depending on imp_physics
      REAL, PARAMETER :: DMRmin=.05E-3, DelDMR=1.E-6    &
     &, XMRmin=1.E6*DMRmin, N0r0=8.E6, N0rmin=1.e4
      REAL DMRmax,XMRmax
      INTEGER, PARAMETER :: MDRmin=XMRmin
      INTEGER MDRmax
!
!--- Various rain lookup tables
!
      REAL RQR_DRmin,RQR_DRmax,           &
           CN0r0,CN0r_DMRmin,CN0r_DMRmax
!
!--- Other important parameters
!    (NLImax, FLARGE2 are used for the older version of the microphysics)
!
      REAL T_ICE,NLImax,FLARGE2,TRAD_ice
!
  end module  CMASSI_mod
