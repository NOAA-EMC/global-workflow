!!!!!  ==========================================================  !!!!!
!!!!!                 module  'physcons' description               !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!   this module contains some the most frequently used math and        !
!   physics constatns for gcm models.                                  !
!                                                                      !
!   references:                                                        !
!     as set in nmc handbook from smithsonian tables.                  !
!                                                                      !
!   modification history:                                              !
!                                                                      !
!     1990-04-30  g and rd are made consistent with nws usage          !
!     2001-10-22  g made consistent with si usage                      !
!     2005-04-13  added molicular weights for gases          - y-t hou !
!     2013-07-12  added temperature for homogen. nuc. for ice. - r.sun !
!                                                                      !
!   external modules referenced:                                       !
!                                                                      !
!       'module machine'                    in 'machine.f'             !
!                                                                      !
!                                                                      !
!!!!!  ==========================================================  !!!!!
!!!!!                       end descriptions                       !!!!!
!!!!!  ==========================================================  !!!!!

!========================================!
          module physcons                !
!........................................!
!
  use machine,      only : kind_phys
!
  implicit none
!
  public

!  --- ...  math constants

  real(kind=kind_phys),parameter:: con_pi     =3.1415926535897931 ! pi
  real(kind=kind_phys),parameter:: con_sqrt2  =1.414214e+0        ! square root of 2
  real(kind=kind_phys),parameter:: con_sqrt3  =1.732051e+0        ! square root of 3

!  --- ...  geophysics/astronomy constants

  real(kind=kind_phys),parameter:: con_rerth  =6.3712e+6      ! radius of earth   (m)
  real(kind=kind_phys),parameter:: con_g      =9.80665e+0     ! gravity           (m/s2)
  real(kind=kind_phys),parameter:: con_omega  =7.2921e-5      ! ang vel of earth  (1/s)
  real(kind=kind_phys),parameter:: con_p0     =1.01325e5      ! std atms pressure (pa)
! real(kind=kind_phys),parameter:: con_solr   =1.36822e+3     ! solar constant    (w/m2)-aer(2001)
  real(kind=kind_phys),parameter:: con_solr_old =1.3660e+3    ! solar constant    (w/m2)-liu(2002)
  real(kind=kind_phys),parameter:: con_solr   =1.3608e+3      ! solar constant    (w/m2)-nasa-sorce tim (2008)
! real(kind=kind_phys),parameter:: con_solr   =1.36742732e+3  ! solar constant    (w/m2)-gfdl(1989) - opr as of jan 2006

!  --- ...  thermodynamics constants

  real(kind=kind_phys),parameter:: con_rgas   =8.314472       ! molar gas constant  (j/mol/k)
  real(kind=kind_phys),parameter:: con_rd     =2.8705e+2      ! gas constant air    (j/kg/k)
  real(kind=kind_phys),parameter:: con_rv     =4.6150e+2      ! gas constant h2o    (j/kg/k)
  real(kind=kind_phys),parameter:: con_cp     =1.0046e+3      ! spec heat air @p    (j/kg/k)
  real(kind=kind_phys),parameter:: con_cv     =7.1760e+2      ! spec heat air @v    (j/kg/k)
  real(kind=kind_phys),parameter:: con_cvap   =1.8460e+3      ! spec heat h2o gas   (j/kg/k)
  real(kind=kind_phys),parameter:: con_cliq   =4.1855e+3      ! spec heat h2o liq   (j/kg/k)
  real(kind=kind_phys),parameter:: con_csol   =2.1060e+3      ! spec heat h2o ice   (j/kg/k)
  real(kind=kind_phys),parameter:: con_hvap   =2.5000e+6      ! lat heat h2o cond   (j/kg)
  real(kind=kind_phys),parameter:: con_hfus   =3.3358e+5      ! lat heat h2o fusion (j/kg)
  real(kind=kind_phys),parameter:: con_psat   =6.1078e+2      ! pres at h2o 3pt     (pa)  
  real(kind=kind_phys),parameter:: con_t0c    =2.7315e+2      ! temp at 0c          (k)
  real(kind=kind_phys),parameter:: con_ttp    =2.7316e+2      ! temp at h2o 3pt     (k)
  real(kind=kind_phys),parameter:: con_tice   =2.7120e+2      ! temp freezing sea     (k)
  real(kind=kind_phys),parameter:: con_jcal   =4.1855e+0      ! joules per calorie  ()
  real(kind=kind_phys),parameter:: con_rhw0   =1022.0         ! sea water reference density (kg/m^3)
  real(kind=kind_phys),parameter:: con_epsq   =1.0e-12        ! min q for computing precip type

!  secondary constants

  real(kind=kind_phys),parameter:: con_rocp   =con_rd/con_cp
  real(kind=kind_phys),parameter:: con_cpor   =con_cp/con_rd
  real(kind=kind_phys),parameter:: con_rog    =con_rd/con_g
  real(kind=kind_phys),parameter:: con_fvirt  =con_rv/con_rd-1.
  real(kind=kind_phys),parameter:: con_eps    =con_rd/con_rv
  real(kind=kind_phys),parameter:: con_epsm1  =con_rd/con_rv-1.
  real(kind=kind_phys),parameter:: con_dldt   =con_cvap-con_cliq
  real(kind=kind_phys),parameter:: con_xpona  =-con_dldt/con_rv
  real(kind=kind_phys),parameter:: con_xponb  =-con_dldt/con_rv+con_hvap/(con_rv*con_ttp)

!  --- ...  other physics/chemistry constants (source: 2002 codata)

  real(kind=kind_phys),parameter:: con_c      =2.99792458e+8  ! speed of light      (m/s)
  real(kind=kind_phys),parameter:: con_plnk   =6.6260693e-34  ! planck constatn     (j/s)
  real(kind=kind_phys),parameter:: con_boltz  =1.3806505e-23  ! boltzmann constant  (j/k)
  real(kind=kind_phys),parameter:: con_sbc    =5.670400e-8    ! stefan-boltzmann    (w/m2/k4)
  real(kind=kind_phys),parameter:: con_avgd   =6.0221415e23   ! avogadro constant   (1/mol)
  real(kind=kind_phys),parameter:: con_gasv   =22413.996e-6   ! vol of ideal gas at 273.15k, 101.325kpa (m3/mol)
! real(kind=kind_phys),parameter:: con_amd    =28.970         ! molecular wght of dry air (g/mol)
  real(kind=kind_phys),parameter:: con_amd    =28.9644        ! molecular wght of dry air (g/mol)
  real(kind=kind_phys),parameter:: con_amw    =18.0154        ! molecular wght of water vapor (g/mol)
  real(kind=kind_phys),parameter:: con_amo3   =47.9982        ! molecular wght of o3  (g/mol)
! real(kind=kind_phys),parameter:: con_amo3   =48.0           ! molecular wght of o3  (g/mol)
  real(kind=kind_phys),parameter:: con_amco2  =44.011         ! molecular wght of co2 (g/mol)
  real(kind=kind_phys),parameter:: con_amo2   =31.9999        ! molecular wght of o2  (g/mol)
  real(kind=kind_phys),parameter:: con_amch4  =16.043         ! molecular wght of ch4 (g/mol)
  real(kind=kind_phys),parameter:: con_amn2o  =44.013         ! molecular wght of n2o (g/mol)
  real(kind=kind_phys),parameter:: con_thgni  =-38.15         ! temperature the h.g.nuc. ice starts        


!........................................!
      end module physcons                !
!========================================!
