module params_mod

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!
! file: params.f
! defines physical constants and smoothing coefficients
! last update: 09/06/2006
!
!     SMOOTHING PARAMETERS.
    integer, parameter :: KSMUD=4
    integer, parameter :: KSLPD=1
    integer, parameter :: KSSLP=2
    integer, parameter :: KSFI=2
    integer, parameter :: KST=2
    integer, parameter :: KSUV=2
    integer, parameter :: KSRH=2
    integer, parameter :: KSAV=2
    integer, parameter :: KSLI=2
    integer, parameter :: KSLP=2
    integer, parameter :: NSUMD=1
                                                                                   
    real, parameter :: SMTHA=-1.8E-4
    real, parameter :: SMTHB=16.0
!
! CONSTANTS.
    real, parameter :: CM1=2937.4
    real, parameter :: CM2=4.9283
    real, parameter :: CM3=23.5518
    real, parameter :: D00=0.0
    real, parameter :: D1=0.1
    real, parameter :: D01=0.01
    real, parameter :: D001=0.001
    real, parameter :: D50=0.50
    real, parameter :: H999=999.
    real, parameter :: H1=1.0
    real, parameter :: H2=2.0
    real, parameter :: H4=4.0
    real, parameter :: H99999=99999.
    real, parameter :: H1M12=1.E-12
    real, parameter :: H1000=1000.
    real, parameter :: H1M5=1.E-5
    real, parameter :: D125=.125
    real, parameter :: D25=0.25
    real, parameter :: H100=100.
    real, parameter :: H10000=10000.
    real, parameter :: H10E5=1.E5
    real, parameter :: CAPA=0.28589641E0
    real, parameter :: D115=0.115
    real, parameter :: D608=0.608
    real, parameter :: D05=0.05
    real, parameter :: D0065=0.0065
    real, parameter :: H1E2=100.
    real, parameter :: H1E4=1.E4
    real, parameter :: H1M2=1.E-2
    real, parameter :: G=9.81
    real, parameter :: RD=287.04
    real, parameter :: ROG=RD/G
    real, parameter :: GI=1./G
    real, parameter :: OVERRC=1.10
    real, parameter :: AD05=OVERRC*D05
    real, parameter :: CFT0=OVERRC-H1
    real, parameter :: PI=3.141592653589793
    real, parameter :: DTR=3.1415926/180.
    real, parameter :: RTD=1./DTR
    real, parameter :: ERAD=6.371E6
    real, parameter :: CP=1004.6
    real, parameter :: RCAPA=1./CAPA
    real, parameter :: P1000=1000.E2,DP10M=110
    real, parameter :: ELWV=2.50E6
    real, parameter :: CPDR=CP/RD
    real, parameter :: RG=1./GI
    real, parameter :: ELDR=ELWV/RD
    real, parameter :: BETA=.00367
    real, parameter :: BTG=BETA*G
    real, parameter :: FMW=18.015
    real, parameter :: FMD=28.964
    real, parameter :: EPS=FMW/FMD
    real, parameter :: ONEPS=1.-EPS
    real, parameter :: TFRZ=273.15
    real, parameter :: RDOCP=RD/CP
    real, parameter :: LHEAT=2.5E6
    real, parameter :: LHTOCP=LHEAT/CP
    real, parameter :: EPSQ2=0.02
    real, parameter :: PQ0=379.90516
    real, parameter :: A2=17.2693882
    real, parameter :: A3=273.16
    real, parameter :: A4=35.86
    real, parameter :: EPSQ=1.E-12
#ifdef GSDRAPR
    real, parameter :: QCLDmin=1.E-6  ! was 1.E-6
#else
    real, parameter :: QCLDmin=1.E-5  ! was 1.E-6
#endif
    real, parameter :: CLFRmin=D1
    real, parameter :: NLImin=1.E3
! move definition of NLImax and T_ICE to MICROINIT 2012012018
!    real, parameter :: NLImax=5.E3
!    real, parameter :: T_ICE=-30.
!    real, parameter :: TRAD_ice=0.5*T_ICE+TFRZ
    real, parameter :: Qconv=0.1E-3
!
    real, parameter :: STBOL=1./5.67E-8
    real, parameter :: DBZmin=-20.
    real, parameter :: abscoef=8.0
    real, parameter :: abscoefi=5.0
    real, parameter :: XLAI=4.0         ! Leaf area index
    real, parameter :: RHmin=1.0E-6     ! minimal RH bound
    real, parameter :: SMALL=1.E-6
!    real, parameter :: PTHRESH=0.000000
    real, parameter :: ELIVW=2.72E6
    real, parameter :: ELOCP=ELIVW/CP
!
    real, parameter :: GAMMA=6.5E-3
    real, parameter :: RGAMOG=RD*GAMMA/G

end module params_mod
