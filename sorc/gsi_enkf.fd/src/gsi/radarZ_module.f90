! Adopted from RADAREMUL library of ARPS system
! Used for direct reflectity DA capability
!
! !DESCRIPTION: module containing radaremul library
!               which is used for TM operator of EnKF application
!
! !REVISION HISTORY:
!   2021-05-10  J. Park(CAPS) - modified with the GSI coding standards
!                             - radaremul_dualpolepara.f90 are
!                             - radaremul_convert2radar.f90 merged.
!   2021-05-17  J. Park(CAPS) - radaremul renamed to radarz
! 

MODULE RADARZ_MODULE
!########################################################################
!########################################################################
!#########                                                      #########
!#########                  module radarz_module                #########
!#########                                                      #########
!#########                     Developed by                     #########
!#########     Center for Analysis and Prediction of Storms     #########
!#########                University of Oklahoma                #########
!#########                                                      #########
!########################################################################
!########################################################################

  use kinds, only: r_kind,r_single,r_double,i_kind

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! Declare some constants used for calculaion of dual polarization
! parameters such as Zhh, Zdr, and Kdp. (It can be expanded...)
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 12/3/2004
!
!-----------------------------------------------------------------------
! Declare parameters.
!-----------------------------------------------------------------------

  IMPLICIT NONE
  SAVE
  PRIVATE

  PUBLIC :: pi, lambda, Kw2
  PUBLIC :: alphaa, alphab
  PUBLIC :: beta_ra, beta_rb
  PUBLIC :: alphak, alphask
  PUBLIC :: alphaa_ds, alphab_ds, alphaa_tom_ds, alphab_tom_ds
  PUBLIC :: beta_sa, beta_sb, beta_tom_dsa, beta_tom_dsb
  PUBLIC :: alphaa_dh, alphab_dh, beta_ha, beta_hb
  PUBLIC :: alphaa_dg, alphab_dg, beta_ga, beta_gb
  PUBLIC :: alphak_ds, alphak_tom_ds, alphak_dh, alphak_dg
  PUBLIC :: betak_s, betak_tom_ds, betak_h, betak_g
  PUBLIC :: rho_0r, rho_0s, rho_0h, rho_0g
  PUBLIC :: rho_0rsi, rho_0rsf, rho_0rhi, rho_0rhf
  PUBLIC :: rho_0rgi, rho_0rgf
  PUBLIC :: degKtoC, rhoi, mm3todBZ
  PUBLIC :: thom_lam0, thom_lam1, thom_k0, thom_k1
  PUBLIC :: unit_factor
  PUBLIC :: kdpCoefIce, c_x, ta
  PUBLIC :: missing
  PUBLIC :: grpl_miss, hl_miss
  PUBLIC :: firstcall
  PUBLIC :: grpl_ON, hl_ON
  PUBLIC :: qgh_opt, attn_ON, dualpol_opt
  PUBLIC :: gamma7_08, gamma6_81, gamma6_54
  PUBLIC :: gamma5_63, gamma4_16, gamma3_97
  PUBLIC :: N0r, N0s, N0h, N0g, N0s2
  PUBLIC :: N0ms, N0ms2, N0mh, N0mg
  PUBLIC :: rhor, rhoh, rhos, rhog
  PUBLIC :: alphar, alphas, alphah
  PUBLIC :: alphag, alphas2
  PUBLIC :: lamdar, lamdas, lamdas2
  PUBLIC :: lamdams, lamdams2
  PUBLIC :: lamdag, lamdamg
  PUBLIC :: lamdah, lamdamh
  PUBLIC :: fos, foh, fog
  PUBLIC :: radar_const, constKdpr
  PUBLIC :: sigmas
  PUBLIC :: As, Bs, Cs, Ds, Cks
  PUBLIC :: sigmahd
  PUBLIC :: Ahd, Bhd, Chd, Dhd, Ckhd
  PUBLIC :: q_threshold
  PUBLIC :: sf
  PUBLIC :: sigmah, Ah, Bh, Ch, Dh, Ckh
  PUBLIC :: sigmagd
  PUBLIC :: Agd, Bgd, Cgd, Dgd, Ckgd
  PUBLIC :: sigmag, Ag, Bg, Cg, Dg, Ckg
  PUBLIC :: T_obs_dual
  PUBLIC :: T_para_dsd

  PUBLIC :: calcMDR
  PUBLIC :: calcConstants
  PUBLIC :: init_fox
  PUBLIC :: init_fox_no_grpl
  PUBLIC :: init_fox_no_hail
  PUBLIC :: model_dsd
  PUBLIC :: coeff_hail
  PUBLIC :: coeff_grpl
  PUBLIC :: assign_Refl
  PUBLIC :: init_Refl
  PUBLIC :: init_para_dsd
  PUBLIC :: assign_para_dsd_TM
  PUBLIC :: rainIceRefl
  PUBLIC :: calculate_obs
  PUBLIC :: snow_alpha_a
  PUBLIC :: snow_alpha_b
  PUBLIC :: hail_alpha_a
  PUBLIC :: hail_alpha_b
  PUBLIC :: grpl_alpha_a
  PUBLIC :: grpl_alpha_b
  PUBLIC :: get_qgh_opt
  PUBLIC :: partialRefRain
  PUBLIC :: partialRhoRain
  PUBLIC :: partialRefIce
  PUBLIC :: partialRhoIce
  PUBLIC :: fractionWater
  PUBLIC :: fractionWater_temperature_snow
  PUBLIC :: fractionWater_temperature_hail
  PUBLIC :: power_mom
  PUBLIC :: calc_N0x_mp
  PUBLIC :: calc_N0x_melt
  PUBLIC :: gamma
  PUBLIC :: cal_N0
  PUBLIC :: calc_lamda_mp
  PUBLIC :: cal_Nt
  PUBLIC :: cal_lamda
  PUBLIC :: set_dsd_para
  PUBLIC :: rdr_obs

  ! Variables

  REAL(r_kind), PARAMETER :: pi = 3.141592_r_kind   ! pi

  REAL(r_kind) :: lambda           ! wavelength of radar (mm)

  REAL(r_kind),PARAMETER :: Kw2 = 0.93_r_kind ! Dielectric factor for water.

  REAL(r_kind),PARAMETER :: alphaa = 4.28e-4_r_kind   ! backscattering amplitude constant
                                                      ! along major axis for rain
  REAL(r_kind),PARAMETER :: beta_ra = 3.04_r_kind
  REAL(r_kind),PARAMETER :: alphab = 4.28e-4_r_kind   ! backscattering amplitude constant
                                                      ! along minor axis for rain
  REAL(r_kind),PARAMETER :: beta_rb = 2.77_r_kind
  REAL(r_kind),PARAMETER :: alphak = 3.88e-4_r_kind   ! differential forward scattering
                                                      ! amplitude for rain
  REAL(r_kind),PARAMETER :: alphask = 8.53e-7_r_kind  ! differential forward scattering
                                                      ! amplitude for snow
  REAL(r_kind),PARAMETER :: alphaa_ds = 1.94e-5_r_kind ! for dry snow at horz plane
  REAL(r_kind),PARAMETER :: alphab_ds = 1.91e-5_r_kind ! for dry snow at vert plane
  REAL(r_kind),PARAMETER :: alphaa_tom_ds = 2.8e-5_r_kind !for dry snow at horz plane for Thomposon scheme
  REAL(r_kind),PARAMETER :: alphab_tom_ds = 2.6e-5_r_kind !for dry snow at vert plane for the Thompson scheme 

  REAL(r_kind), PARAMETER :: beta_sa = 3.0_r_kind
  REAL(r_kind), PARAMETER :: beta_sb = 3.0_r_kind
  REAL(r_kind), PARAMETER :: beta_tom_dsa = 1.95_r_kind  ! Special expon for Thompson scheme
  REAL(r_kind), PARAMETER :: beta_tom_dsb = 1.965_r_kind ! Special expon for Thompson scheme

  REAL(r_kind),PARAMETER :: alphaa_dh = 1.91e-4_r_kind ! for dry hail at horz plane
  REAL(r_kind),PARAMETER :: alphab_dh = 1.65e-4_r_kind ! for dry hail at vert plane

  REAL(r_kind),PARAMETER :: beta_ha = 3.0_r_kind
  REAL(r_kind),PARAMETER :: beta_hb = 3.0_r_kind

  REAL(r_kind),PARAMETER :: alphaa_dg = 0.81e-4_r_kind ! for dry graupel at horz plane
  REAL(r_kind),PARAMETER :: alphab_dg = 0.76e-4_r_kind ! for dry graupel at vert plane

  REAL(r_kind),PARAMETER :: beta_ga = 3.0_r_kind
  REAL(r_kind),PARAMETER :: beta_gb = 3.0_r_kind

  REAL(r_kind),PARAMETER :: alphak_ds = 0.03e-5_r_kind     ! alphaa_ds - alphab_ds
  REAL(r_kind),PARAMETER :: alphak_tom_ds = 1.05e-6_r_kind !alphaa_ds - alphab_ds for Thompson scheme
  REAL(r_kind),PARAMETER :: alphak_dh = 0.26e-4_r_kind ! alphaa_dh - alphab_dh
  REAL(r_kind),PARAMETER :: alphak_dg = 0.05e-4_r_kind ! alphaa_dh - alphab_dh
  REAL(r_kind),PARAMETER :: betak_s = 3.0_r_kind
  REAL(r_kind),PARAMETER :: betak_tom_ds = 2.04_r_kind !For Thompson Scheme 
  REAL(r_kind),PARAMETER :: betak_h = 3.0_r_kind
  REAL(r_kind),PARAMETER :: betak_g = 3.0_r_kind

  REAL(r_kind),PARAMETER :: rho_0r = 1.0_r_kind      ! rho_0 for rain
  REAL(r_kind),PARAMETER :: rho_0s = 1.0_r_kind      ! rho_0 for snow
  REAL(r_kind),PARAMETER :: rho_0h = 0.97_r_kind     ! rho_0 for hail
  REAL(r_kind),PARAMETER :: rho_0g = 0.95_r_kind     ! rho_0 for hail
  REAL(r_kind),PARAMETER :: rho_0rsi = 0.82_r_kind   ! lower limit of rho_0rs (rain-snow mixture)
  REAL(r_kind),PARAMETER :: rho_0rsf = 0.95_r_kind   ! upper limit of rho_0rs (rain-snow mixture)
  REAL(r_kind),PARAMETER :: rho_0rhi = 0.85_r_kind   ! lower limit of rho_0rh (rain-hail mixture)
  REAL(r_kind),PARAMETER :: rho_0rhf = 0.95_r_kind   ! upper limit of rho_0rh (rain-hail mixture)
  REAL(r_kind),PARAMETER :: rho_0rgi = 0.82_r_kind   ! lower limit of rho_0rg (rain-graupel mixture)
  REAL(r_kind),PARAMETER :: rho_0rgf = 0.95_r_kind   ! upper limit of rho_0rg (rain-graupel mixture)

  REAL(r_kind),PARAMETER :: degKtoC = 273.15_r_kind ! Conversion factor from degrees K to
                                                    ! degrees C

  REAL(r_kind),PARAMETER :: rhoi = 917._r_kind  ! Density of ice (kg m**-3)

  REAL(r_kind),PARAMETER :: mm3todBZ = 1.0E+9_r_kind ! Conversion factor from mm**3 to
                                                     !   mm**6 m**-3.
 
  REAL(r_kind),PARAMETER :: thom_lam0 = 20.78_r_kind
  REAL(r_kind),PARAMETER :: thom_lam1 = 3.29_r_kind
  REAL(r_kind),PARAMETER :: thom_k0 = 490.6_r_kind
  REAL(r_kind),PARAMETER :: thom_k1 = 17.46_r_kind

  REAL(r_kind),PARAMETER :: unit_factor = 1.e-2_r_kind  ! Unit conversion factor not addressed
                                         ! in the T-matrix scattering amplitude (size D is in cm in T-matrix)

  REAL(r_kind) :: kdpCoefIce 

  REAL(r_kind) :: c_x(6)  !(PI/6)*rho_qx

  REAL(r_kind) :: ta = 273.16_r_kind

  REAL(r_kind),PARAMETER :: missing = -9999.0_r_kind
  REAL(r_kind) :: grpl_miss
  REAL(r_kind) :: hl_miss 
  
  LOGICAL :: firstcall = .true.
  INTEGER(i_kind) :: grpl_ON
  INTEGER(i_kind) :: hl_ON 
  INTEGER(i_kind) :: qgh_opt

  INTEGER(i_kind) :: attn_ON

  INTEGER(i_kind) :: dualpol_opt

!-----------------------------------------------------------------------
! Precalculated complete gamma function values
!-----------------------------------------------------------------------
  REAL(r_kind),PARAMETER :: gamma7_08 = 836.7818_r_kind
  REAL(r_kind),PARAMETER :: gamma6_81 = 505.8403_r_kind
  REAL(r_kind),PARAMETER :: gamma6_54 = 309.3308_r_kind
  REAL(r_kind),PARAMETER :: gamma5_63 = 64.6460_r_kind
  REAL(r_kind),PARAMETER :: gamma4_16 = 7.3619_r_kind
  REAL(r_kind),PARAMETER :: gamma3_97 = 5.7788_r_kind

!-----------------------------------------------------------------------
! Variables to can be changed by parameter retrieval
!-----------------------------------------------------------------------
  REAL(r_kind) :: N0r        ! Intercept parameter in 1/(m^4) for rain
  REAL(r_kind) :: N0s        ! Intercept parameter in 1/(m^4) for snow
  REAL(r_kind) :: N0h        ! Intercept parameter in 1/(m^4) for hail
  REAL(r_kind) :: N0g        ! Intercept parameter in 1/(m^4) for hail
  REAL(r_kind) :: N0s2       ! Second intercept parameter in 1/(m^4) for snow

  REAL(r_kind) :: N0ms       !Intercept parameter for melting species 
  REAL(r_kind) :: N0ms2 
  REAL(r_kind) :: N0mh
  REAL(r_kind) :: N0mg 

  REAL(r_kind) :: rhor = 1000._r_kind ! Density of rain (kg m**-3)
  REAL(r_kind) :: rhoh                ! Density of hail (kg m**-3)
  REAL(r_kind) :: rhos                ! Density of snow (kg m**-3)
  REAL(r_kind) :: rhog                ! Density of graupel (kg m**-3)

  REAL(r_kind) :: alphar     !Shape parameter for rain
  REAL(r_kind) :: alphas     !Shape parameter for snow
  REAL(r_kind) :: alphah     !SHape parameter for hail
  REAL(r_kind) :: alphag     !SHape parameter for graupel
  REAL(r_kind) :: alphas2

  REAL(r_kind) :: lamdar     !slope parameter for rain (1/m)
  REAL(r_kind) :: lamdas     
  REAL(r_kind) :: lamdas2  
  REAL(r_kind) :: lamdams
  REAL(r_kind) :: lamdams2  
  REAL(r_kind) :: lamdag
  REAL(r_kind) :: lamdamg
  REAL(r_kind) :: lamdah
  REAL(r_kind) :: lamdamh 

!-----------------------------------------------------------------------
! Variables to can be changed for meling ice
!-----------------------------------------------------------------------
  REAL(r_kind) :: fos        ! Maximum fraction of rain-snow mixture
  REAL(r_kind) :: foh        ! Maximum fraction of rain-hail mixture
  REAL(r_kind) :: fog        ! Maximum fraction of rain-hail mixture

!-----------------------------------------------------------------------
! Constants
!-----------------------------------------------------------------------

 
  REAL(r_kind) :: radar_const    !(4*lambda**4)/(pi*kw2)
  
  REAL(r_kind) :: constKdpr

!-----------------------------------------------------------------------
! Scattering matrix coefficient for snow
!
! phi=0._r_kind       (Mean orientation)
! sigmas=pi/9
! As=1/8*(3+4*cos(2*phi)*exp(-2*sigmas**2)+cos(4*phi)*exp(-8*sigmas**2))
! Bs=1/8*(3-4*cos(2*phi)*exp(-2*sigmas**2)+cos(4*phi)*exp(-8*sigmas**2))
! Cs=1/8*(1-cos(4*phi)*exp(-8*sigmas**2))
! Ds=1/8*(3+cos(4*phi)*exp(-8*sigmas**2))
! Cks=cos(2*phi)*exp(-2*sigmas**2)
!-----------------------------------------------------------------------

  REAL(r_kind),PARAMETER :: sigmas = 0.3491_r_kind
  REAL(r_kind),PARAMETER :: As = 0.8140_r_kind
  REAL(r_kind),PARAMETER :: Bs = 0.0303_r_kind
  REAL(r_kind),PARAMETER :: Cs = 0.0778_r_kind
  REAL(r_kind),PARAMETER :: Ds = 0.4221_r_kind
  REAL(r_kind),PARAMETER :: Cks = 0.7837_r_kind

!-----------------------------------------------------------------------
! Scattering matrix coefficient for hail
!
! phi=0._r_kind     (Mean orientation)
! sigmah=pi/3*(1-sf*fw)
! Ah=1/8*(3+4*cos(2*phi)*exp(-2*sigmah**2)+cos(4*phi)*exp(-8*sigmah**2))
! Bh=1/8*(3-4*cos(2*phi)*exp(-2*sigmah**2)+cos(4*phi)*exp(-8*sigmah**2))
! Ch=1/8*(1-cos(4*phi)*exp(-8*sigmah**2))
! Dh=1/8*(3+cos(4*phi)*exp(-8*sigmah**2))
! Ckh=cos(2*phi)*exp(-2*sigmah**2)
!
! corresponding coefficient for dry hail: Ahd, Bhd, Chd, Dhd, Ckhd
!-----------------------------------------------------------------------

  REAL(r_kind),PARAMETER :: sigmahd = 1.0472_r_kind
  REAL(r_kind),PARAMETER :: Ahd = 0.4308_r_kind
  REAL(r_kind),PARAMETER :: Bhd = 0.3192_r_kind
  REAL(r_kind),PARAMETER :: Chd = 0.1250_r_kind
  REAL(r_kind),PARAMETER :: Dhd = 0.3750_r_kind
  REAL(r_kind),PARAMETER :: Ckhd = 0.1116_r_kind

  REAL(r_kind),PARAMETER :: q_threshold = 2.e-4_r_kind
  REAL(r_kind) :: sf
  REAL(r_kind) :: sigmah, Ah, Bh, Ch, Dh, Ckh

!-----------------------------------------------------------------------
! Scattering matrix coefficient for graupel
! 
! phi=0._r_kind     (Mean orientation)
! sigmag=pi/3*(1-sf*fw)
! Ag=1/8*(3+4*cos(2*phi)*exp(-2*sigmag**2)+cos(4*phi)*exp(-8*sigmag**2))
! Bg=1/8*(3-4*cos(2*phi)*exp(-2*sigmag**2)+cos(4*phi)*exp(-8*sigmag**2))
! Cg=1/8*(1-cos(4*phi)*exp(-8*sigmag**2))
! Dg=1/8*(3+cos(4*phi)*exp(-8*sigmag**2))
! Ckg=cos(2*phi)*exp(-2*sigmag**2)
! 
! corresponding coefficient for dry graupel: Agd, Bgd, Cgd, Dgd, Ckgd
!-----------------------------------------------------------------------
  
  REAL(r_kind),PARAMETER :: sigmagd = 1.0472_r_kind
  REAL(r_kind),PARAMETER :: Agd = 0.4308_r_kind
  REAL(r_kind),PARAMETER :: Bgd = 0.3192_r_kind
  REAL(r_kind),PARAMETER :: Cgd = 0.1250_r_kind
  REAL(r_kind),PARAMETER :: Dgd = 0.3750_r_kind
  REAL(r_kind),PARAMETER :: Ckgd = 0.1116_r_kind
  
  REAL(r_kind) :: sigmag, Ag, Bg, Cg, Dg, Ckg

!-----------------------------------------------------------------------
!  Declare new observation type
!-----------------------------------------------------------------------

  TYPE T_obs_dual
       REAL(r_kind) :: T_log_ref, T_sum_ref_h, T_sum_ref_v
       REAL(r_kind) :: T_log_zdr, T_sum_ref_hv, T_kdp
       REAL(r_kind) :: T_Ahh,     T_Avv
       REAL(r_kind) :: T_ref_r_h, T_ref_s_h, T_ref_h_h,T_ref_g_h
       REAL(r_kind) :: T_ref_rs_h,T_ref_rh_h,T_ref_rg_h
       REAL(r_kind) :: T_ref_r_v, T_ref_s_v, T_ref_h_v, T_ref_g_v
       REAL(r_kind) :: T_ref_rs_v, T_ref_rh_v, T_ref_rg_v
  END TYPE T_obs_dual

!-----------------------------------------------------------------------
!  Declare new DSD parameter data type
!-----------------------------------------------------------------------

  TYPE T_para_dsd
    REAL(r_kind) :: T_qr, T_qs, T_qh, T_qg
    REAL(r_kind) :: T_Ntr, T_Nts, T_Nth, T_Ntg
    REAL(r_kind) :: T_alfr,T_alfs,T_alfh,T_alfg
  END TYPE T_para_dsd

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! SUBROUTINES AND FUNCTIONS
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  CONTAINS


  SUBROUTINE calcMDR()
!-----------------------------------------------------------------------
!
! PURPOSE:
!
! Calculates mass-diameter relation based on MP scheme. 
!
!-----------------------------------------------------------------------
!
! Author:  Bryan Putnam, 4/16/2013
!
!-----------------------------------------------------------------------

  USE radarz_cst, only: mphyopt

  IMPLICIT NONE

  c_x(1) = (pi/6._r_kind)*rhor
  c_x(2) = (pi/6._r_kind)*rhor
  c_x(3) = 440.0_r_kind

  SELECT CASE (mphyopt)
    CASE(1:12,106,109:110,116)
      c_x(4) = (pi/6._r_kind)*rhos
    CASE(108)
      c_x(4) = .069_r_kind 
  END SELECT 

  c_x(5) = (pi/6._r_kind)*rhog
  c_x(6) = (pi/6._r_kind)*rhoh

  END SUBROUTINE calcMDR

  SUBROUTINE calcConstants()

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! Precalculate commonly unsed constants to save computations.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 1/28/2005
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Constant in front of dual pol calculations (4*lambda**4)/(pi*kw2)
!-----------------------------------------------------------------------

   radar_const = (4._r_kind * lambda**4._r_kind)/(pi**4 * Kw2)

!-----------------------------------------------------------------------
! For Kdp constants
!-----------------------------------------------------------------------

    constKdpr = 180._r_kind * lambda  * alphak * 1.0e6_r_kind / pi !rain
    kdpCoefIce = (180*lambda*1.e6_r_kind)/pi                       !ice 

  END SUBROUTINE calcConstants

  SUBROUTINE init_fox()

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! Setup default maximum fraction of water in the melting ice.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 1/20/2006
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Variables can vary depend on whether graupel/hail exists. 
!-----------------------------------------------------------------------
  fos = 0.3_r_kind             ! Maximum fraction of rain-snow mixture
  foh = 0.2_r_kind             ! Maximum fraction of rain-hail mixture
  fog = 0.25_r_kind            ! Maximum fraction of rain-hail mixture

  END SUBROUTINE init_fox

  SUBROUTINE init_fox_no_grpl()

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! Setup default maximum fraction of water in the melting ice 
! when graupel is suppressed.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 1/20/2006
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Variables can be changed by parameter retrieval
!-----------------------------------------------------------------------
  fos = 0.5_r_kind         ! Maximum fraction of rain-snow mixture
  foh = 0.3_r_kind         ! Maximum fraction of rain-hail mixture
  fog = 0.0_r_kind         ! Maximum fraction of rain-hail mixture
      
  END SUBROUTINE init_fox_no_grpl


  SUBROUTINE init_fox_no_hail() 

!-----------------------------------------------------------------------
!
! PURPOSE:  
!
!  Setup default maximum fraction of water in the melting ice 
!  when hail is suprressed. 
!
!-----------------------------------------------------------------------
!
! AUTHOR: Bryan Putnam, 12/14/10
!
!-----------------------------------------------------------------------
! Force explicit declarations. 
!-----------------------------------------------------------------------

  IMPLICIT NONE 

!-----------------------------------------------------------------------
! Variables can be changed by parameter retrieval 
!-----------------------------------------------------------------------

  fos = 0.5_r_kind      ! Maximum fraction of rain-snow mixture
  foh = 0.0_r_kind      ! Maximum fraction of rain-hail mixture
  fog = 0.3_r_kind      ! Maximum fraction of rain-hail mixture

  END SUBROUTINE init_fox_no_hail

  SUBROUTINE model_dsd(n0rain,n0snow,n0hail,n0grpl,rhosnow,rhohail,rhogrpl, &
             alpharain,alphasnow,alphagrpl,alphahail)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! Set dsd values to those used in the arps forecasts
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 1/20/2006
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Define variables:
!-----------------------------------------------------------------------


  REAL(r_kind), INTENT(IN    ) :: n0rain,n0snow,n0hail,n0grpl
  REAL(r_kind), INTENT(IN    ) :: rhosnow,rhohail,rhogrpl
  REAL(r_kind), INTENT(IN    ) :: alpharain,alphasnow,alphagrpl,alphahail

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  N0r=n0rain
  N0s=n0snow
  N0h=n0hail
  N0g=n0grpl

  rhos=rhosnow
  rhoh=rhohail
  rhog=rhogrpl

  alphar = alpharain
  alphas = alphasnow
  alphag = alphagrpl
  alphah = alphahail

  END SUBROUTINE model_dsd

  SUBROUTINE coeff_hail(fw)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! Scattering matrix coefficient for hail
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 1/27/2007
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Define variables:
!-----------------------------------------------------------------------

  REAL(r_kind), INTENT(IN   ) :: fw
  
  sf = 0.8_r_kind

  sigmah=pi/3_r_kind*(1_r_kind-sf*fw)
  Ah=.125_r_kind*(3_r_kind+4_r_kind*exp(-2_r_kind*sigmah**2)+exp(-8_r_kind*sigmah**2))
  Bh=.125_r_kind*(3_r_kind-4_r_kind*exp(-2_r_kind*sigmah**2)+exp(-8_r_kind*sigmah**2))
  Ch=.125_r_kind*(1_r_kind-exp(-8_r_kind*sigmah**2))
  Dh=.125_r_kind*(3_r_kind+exp(-8_r_kind*sigmah**2))
  Ckh=exp(-2_r_kind*sigmah**2)

  END SUBROUTINE coeff_hail

SUBROUTINE coeff_grpl(fw)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! Scattering matrix coefficient for graupel
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 1/27/2007
!
! MODIFIED: Dan Dawson, 02/16/2012
!           Made separate version for graupel.
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Define variables:
!-----------------------------------------------------------------------

  REAL(r_kind), INTENT(IN   ) :: fw

  sf = 0.8_r_kind

  sigmag=pi/3_r_kind*(1_r_kind-sf*fw)
  Ag=.125_r_kind*(3_r_kind+4_r_kind*exp(-2_r_kind*sigmag**2)+exp(-8_r_kind*sigmag**2))
  Bg=.125_r_kind*(3_r_kind-4_r_kind*exp(-2_r_kind*sigmag**2)+exp(-8_r_kind*sigmag**2))
  Cg=.125_r_kind*(1_r_kind-exp(-8_r_kind*sigmag**2))
  Dg=.125_r_kind*(3_r_kind+exp(-8_r_kind*sigmag**2))
  Ckg=exp(-2_r_kind*sigmag**2)

  END SUBROUTINE coeff_grpl

  TYPE(T_obs_dual) FUNCTION assign_Refl(var1,var2,var3,var4)
       REAL(r_kind), INTENT(IN   ) :: var1,var2,var3,var4
       assign_Refl%T_sum_ref_h = var1
       assign_Refl%T_sum_ref_v = var2
       assign_Refl%T_log_zdr = var3
       assign_Refl%T_log_ref = var4
  END FUNCTION assign_Refl

  TYPE(T_obs_dual) FUNCTION init_Refl()
       init_Refl%T_sum_ref_h = 0._r_kind
       init_Refl%T_sum_ref_v = 0._r_kind
       init_Refl%T_log_zdr = missing
       init_Refl%T_log_ref = 0._r_kind
       init_Refl%T_sum_ref_hv = 0._r_kind
       init_Refl%T_kdp = 0._r_kind
       init_Refl%T_Ahh = 0._r_kind
       init_Refl%T_Avv = 0._r_kind
       init_Refl%T_ref_r_h = 0._r_kind
       init_Refl%T_ref_s_h = 0._r_kind
       init_Refl%T_ref_h_h = 0._r_kind
       init_Refl%T_ref_g_h = 0._r_kind
       init_Refl%T_ref_rs_h = 0._r_kind
       init_Refl%T_ref_rh_h = 0._r_kind
       init_Refl%T_ref_rg_h = 0._r_kind
       init_Refl%T_ref_r_v = 0._r_kind
       init_Refl%T_ref_s_v = 0._r_kind
       init_Refl%T_ref_h_v = 0._r_kind
       init_Refl%T_ref_g_v = 0._r_kind
       init_Refl%T_ref_rs_v = 0._r_kind
       init_Refl%T_ref_rh_v = 0._r_kind
       init_Refl%T_ref_rg_v = 0._r_kind
  END FUNCTION init_Refl

  TYPE(T_para_dsd) FUNCTION init_para_dsd()
    init_para_dsd%T_qr = 0.0_r_kind
    init_para_dsd%T_qs = 0.0_r_kind
    init_para_dsd%T_qh = 0.0_r_kind
    init_para_dsd%T_qg = 0.0_r_kind
    init_para_dsd%T_Ntr = 0.0_r_kind
    init_para_dsd%T_Nts = 0.0_r_kind
    init_para_dsd%T_Nth = 0.0_r_kind
    init_para_dsd%T_Ntg = 0.0_r_kind
    init_para_dsd%T_alfr = 0.0_r_kind
    init_para_dsd%T_alfs = 0.0_r_kind
    init_para_dsd%T_alfh = 0.0_r_kind
    init_para_dsd%T_alfg = 0.0_r_kind
  END FUNCTION init_para_dsd

  TYPE(T_para_dsd) FUNCTION assign_para_dsd_TM(var1,var2,var3,var4, &
                            var5,var6,var7,var8,var9,var10,var11,var12)
    REAL(r_kind), INTENT(IN   ) :: var1,var2,var3,var4,var5,var6,var7,var8
    REAL(r_kind), INTENT(IN   ) :: var9,var10,var11,var12

    assign_para_dsd_TM%T_qr = var1
    assign_para_dsd_TM%T_qs = var2
    assign_para_dsd_TM%T_qh = var3
    assign_para_dsd_TM%T_qg = var4
    assign_para_dsd_TM%T_Ntr = var5
    assign_para_dsd_TM%T_Nts = var6
    assign_para_dsd_TM%T_Nth = var7
    assign_para_dsd_TM%T_Ntg = var8
    assign_para_dsd_TM%T_alfr = var9
    assign_para_dsd_TM%T_alfs = var10
    assign_para_dsd_TM%T_alfh = var11
    assign_para_dsd_TM%T_alfg = var12
  END FUNCTION assign_para_dsd_TM


TYPE(T_obs_dual) FUNCTION rainIceRefl(var_dsd,rho,flg)
!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This subroutine calculates the partial reflectivity factor
! of melting(wet) snow/hail at horizontal polarization
! and compute total reflectivity as a sum of those.
! The same formula used in shfactor is used with different
! alpha and beta coefficients that contain the effect of the fraction
! of water in the melting snow to take the melting layer into account.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 1/29/2007
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  use radarz_cst, only: mphyopt, MFflg

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare local variables.
!-----------------------------------------------------------------------
 
  TYPE(T_para_dsd), INTENT(IN   ) :: var_dsd
  INTEGER(i_kind),  INTENT(IN   ) :: flg
  REAL(r_kind),     INTENT(IN   ) :: rho

  REAL(r_kind) :: qr,qs,qh,qg,ntr,nts,nth,ntg 
  REAL(r_kind) :: rainIceRefl_hh,rainIceRefl_vv,rainIceRefl_hv,zdr
  REAL(r_kind) :: fracqrs,fracqrh,fracqrg
  REAL(r_kind) :: fracqs,fracqh,fracqg
  REAL(r_kind) :: fms,fmh,fmg,fws,fwh,fwg,rhoms,rhomh,rhomg
  REAL(r_kind) :: qrf,qsf,qhf,qgf
  REAL(r_kind) :: alphaa_ws,alphab_ws,alphaa_wh,alphab_wh,alphaa_wg,alphab_wg
  REAL(r_kind) :: alphak_ws,alphak_wh,alphak_wg
  REAL(r_kind) :: rainReflH,ZdrysnowH,ZwetsnowH
  REAL(r_kind) :: rainReflV,ZdrysnowV,ZwetsnowV
  REAL(r_kind) :: ZdryhailH,ZwethailH,ZdrygrplH,ZwetgrplH
  REAL(r_kind) :: ZdryhailV,ZwethailV,ZdrygrplV,ZwetgrplV
  REAL(r_kind) :: rainReflHV,ZdrysnowHV,ZwetsnowHV
  REAL(r_kind) :: ZdryhailHV,ZwethailHV,ZdrygrplHV,ZwetgrplHV
  REAL(r_kind) :: log_ref
  REAL(r_kind) :: rho_0rs,rho_0rh,rho_0rg,temp
  REAL(r_kind) :: temH,temV,temHV
  REAL(r_kind) :: tair_C

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  IF(firstcall) THEN
    qgh_opt = get_qgh_opt(grpl_ON,hl_ON)

    SELECT CASE (qgh_opt)
      CASE (1)
        fos = 0.5_r_kind; foh = 0.0_r_kind; fog = 0.0_r_kind
      CASE (2)
        CALL init_fox_no_grpl()
      CASE (3)
        CALL init_fox_no_hail()
      CASE (4)
        CALL init_fox()
    END SELECT

    firstcall = .false. 
  END IF

  qrf = 0._r_kind; qsf = 0._r_kind; qhf = 0._r_kind; qgf = 0._r_kind
  fracqs = 0._r_kind; fracqh = 0._r_kind; fracqg = 0._r_kind
  fracqrs = 0._r_kind; fracqrh = 0._r_kind; fracqrg = 0._r_kind

  fms = 0._r_kind; fmh = 0._r_kind; fmg = 0._r_kind
  fws = 0._r_kind; fwh = 0._r_kind; fwg = 0._r_kind
  rhoms = 100._r_kind; rhomh = 913._r_kind; rhomg = 400._r_kind

  rainReflH = 0._r_kind
  rainReflV = 0._r_kind
  rainReflHV = 0._r_kind
  ZdrysnowH = 0._r_kind
  ZdrysnowV = 0._r_kind
  ZdrysnowHV = 0._r_kind
  ZwetsnowH = 0._r_kind
  ZwetsnowV = 0._r_kind
  ZwetsnowHV = 0._r_kind
  ZdryhailH = 0._r_kind
  ZdryhailV = 0._r_kind
  ZdryhailHV = 0._r_kind
  ZwethailH = 0._r_kind
  ZwethailV = 0._r_kind
  ZwethailHV = 0._r_kind
  ZdrygrplH = 0._r_kind
  ZdrygrplV = 0._r_kind
  ZdrygrplHV = 0._r_kind
  ZwetgrplH = 0._r_kind
  ZwetgrplV = 0._r_kind
  ZwetgrplHV = 0._r_kind

  temH = 0._r_kind
  temV = 0._r_kind
  temHV = 0._r_kind 

  rainIceRefl_hh = 0._r_kind
  rainIceRefl_vv = 0._r_kind
  rainIceRefl_hv = 0._r_kind
  zdr = missing
  log_ref = 0._r_kind

  rho_0rs = rho_0rsf
  rho_0rh = rho_0rhf
  rho_0rg = rho_0rgf

  qr = var_dsd%T_qr
  qs = var_dsd%T_qs
  qh = var_dsd%T_qh
  qg = var_dsd%T_qg
  ntr = var_dsd%T_Ntr
  nts = var_dsd%T_Nts
  nth = var_dsd%T_Nth
  ntg = var_dsd%T_Ntg
  
  if(qr < 0.0_r_kind) qr =0.0_r_kind
  if(qs < 0.0_r_kind) qs =0.0_r_kind
  if(qh < 0.0_r_kind) qh =0.0_r_kind
  if(qg < 0.0_r_kind) qg =0.0_r_kind

!-----------------------------------------------------------------------
! Calculate the fraction of water and ice.
!   qrf  pure rain water mixing ratio
!   qsf  dry snow mixing ratio
!   qhf  dry hail mixing ratio
!   qgf  dry graupel mixing ratio
!   fms  wet snow mixing ratio
!   fmh  wet hail mixing ratio
!   fmg  wet graupel mixing ratio
!   rhoms  density of wet snow (kg/m-3)
!   rhomh  density of wet hail (kg/m-3)
!   rhomg  density of wet graupel (kg/m-3)
!-----------------------------------------------------------------------

  IF (MFflg == 0) THEN

    CALL fractionWater(qr,qs,fos,rhos,fracqrs,fracqs,fms,fws,rhoms)
    IF(hl_ON == 1)  &
      CALL fractionWater(qr,qh,foh,rhoh,fracqrh,fracqh,fmh,fwh,rhomh)
    IF(grpl_ON == 1) &
      CALL fractionWater(qr,qg,fog,rhog,fracqrg,fracqg,fmg,fwg,rhomg)

    qrf = qr - fracqrs - fracqrh - fracqrg
    if(qrf < 0.0_r_kind) qrf = 0.0_r_kind
    qsf = qs - fracqs
    if(qsf < 0.0_r_kind) qsf = 0.0_r_kind
    qhf = qh - fracqh
    if(qhf < 0.0_r_kind) qhf = 0.0_r_kind
    qgf = qg - fracqg
    if(qgf < 0.0_r_kind) qgf = 0.0_r_kind

  ELSE IF (MFflg == 2) THEN

    qrf = qr
    qsf = qs
    IF(hl_ON == 1)   qhf = qh
    IF(grpl_ON == 1) qgf = qg

  ELSE IF (MFflg == 3) THEN    ! Temperature-based melting.

    tair_C = ta - degKtoC

    CALL fractionWater_temperature_snow(qs,rhos,fms,fws,rhoms,tair_C)
    IF(hl_ON == 1)  &
    CALL fractionWater_temperature_hail(qh,rhoh,fmh,fwh,rhomh,tair_C)
    IF(grpl_ON == 1)  &
    CALL fractionWater_temperature_hail(qg,rhog,fmg,fwg,rhomg,tair_C)

    qrf = qr
    qsf = qs-fms
    qhf = qh-fmh
    qgf = qg-fmg

  END IF

  qrf = qr - fracqrs - fracqrh - fracqrg
  if(qrf < 0.0_r_kind) qrf = 0.0_r_kind
  qsf = qs - fracqs
  if(qsf < 0.0_r_kind) qsf = 0.0_r_kind
  qhf = qh - fracqh
  if(qhf < 0.0_r_kind) qhf = 0.0_r_kind
  qgf = qg - fracqg
  if(qgf < 0.0_r_kind) qgf = 0.0_r_kind

!-----------------------------------------------------------------------
! Calculate the matrix coefficient for hail (Ah,Bh,Ch,Ckh)
!-----------------------------------------------------------------------
  IF(hl_ON == 1)   CALL coeff_hail(fwh)
  IF(grpl_ON == 1) CALL coeff_grpl(fwg)

!-----------------------------------------------------------------------
! Calculate alpha values
!-----------------------------------------------------------------------
  IF(fms > 0._r_kind) THEN
    alphaa_ws = snow_alpha_a(fws)
    alphab_ws = snow_alpha_b(fws)
    alphak_ws = alphaa_ws - alphab_ws
  ENDIF

  IF(hl_ON == 1 .and. fmh > 0._r_kind) THEN
    alphaa_wh = hail_alpha_a(fwh)
    alphab_wh = hail_alpha_b(fwh)
    alphak_wh = alphaa_wh - alphab_wh
  ENDIF

  IF(grpl_ON == 1 .and. fmg > 0._r_kind) THEN
    alphaa_wg = grpl_alpha_a(fwg)
    alphab_wg = grpl_alpha_b(fwg)
    alphak_wg = alphaa_wg - alphab_wg
  ENDIF

!-----------------------------------------------------------------------
! Calculate rho_0rs, rho_0rh, and rho_0rg
!-----------------------------------------------------------------------
  IF(flg > 2 .and. fms > 0._r_kind) THEN
    temp=rho*fms*1.e3_r_kind
    if(temp > 1._r_kind) then
      rho_0rs = rho_0rsi
    else if (1.e-2_r_kind > temp .and. temp <= 1._r_kind) then
      rho_0rs = rho_0rsi - .5_r_kind*log10(temp)*(rho_0rsf-rho_0rsi)
    endif
  ENDIF

  IF(hl_ON == 1 .and. flg > 2 .and. fmh > 0._r_kind) THEN
    temp=rho*fmh*1.e3_r_kind
    if(temp > 1._r_kind) then
      rho_0rh = rho_0rhi
    else if (1.e-2_r_kind > temp .and. temp <= 1._r_kind) then
      rho_0rh = rho_0rhi - .5_r_kind*log10(temp)*(rho_0rhf-rho_0rhi)
    endif
  ENDIF

  IF(grpl_ON == 1 .and. flg > 2 .and. fmg > 0._r_kind) THEN
    temp=rho*fmg*1.e3_r_kind
    if(temp > 1._r_kind) then
      rho_0rg = rho_0rgi
    else if (1.e-2_r_kind > temp .and. temp <= 1._r_kind) then
      rho_0rg = rho_0rgi - .5_r_kind*log10(temp)*(rho_0rgf-rho_0rgi)
    endif
  ENDIF

!-----------------------------------------------------------------------
! Calculate reflectivity (Zhh and Zvv (and Zhv, if necessary))
!-----------------------------------------------------------------------

  CALL calc_N0x_mp(rho,rhoms,rhomh,rhomg,ntr,nts,nth,ntg,qrf,qsf,  &
                    fms,qhf,fmh,qgf,fmg)

  CALL calc_lamda_mp(rho,rhoms,rhomh,rhomg,ntr,nts,nth,ntg,  &
                    qrf,qsf,fms,qhf,fmh,qgf,fmg) 


  SELECT CASE (mphyopt)
    CASE(1:12,106,109:110,116)
      IF(lamdas > 0._r_kind) THEN
        CALL partialRefIce(N0s,alphas,As,Bs,Cs,alphaa_ds,       &
                           alphab_ds,beta_sa, beta_sb,lamdas,   & 
                           ZdrysnowH,ZdrysnowV)
        IF(flg > 2) THEN
          CALL partialRhoIce(N0s,alphas,Cs,Ds,alphaa_ds,        &
                           alphab_ds,beta_sa,beta_sb,rho_0s,    &
                           lamdas,ZdrysnowHV)
        ENDIF
      ENDIF
      IF(lamdams > 0._r_kind) THEN
        CALL partialRefIce(N0ms,alphas,As,Bs,Cs,alphaa_ws,       &
                           alphab_ws,beta_sa,beta_sb,lamdams,    &
                           ZwetsnowH,ZwetsnowV)
        IF(flg > 2) THEN
          CALL partialRhoIce(N0ms,alphas,Cs,Ds,alphaa_ws,        &
                           alphab_ws,beta_sa,beta_sb,rho_0rs,    &
                           lamdams,ZwetsnowHV)
        ENDIF
      ENDIF
    CASE(108)
      IF(lamdas > 0._r_kind .and. qsf > 0._r_kind) THEN
        CALL partialRefIce(N0s,alphas,As,Bs,Cs,alphaa_tom_ds,     &
                          alphab_tom_ds,beta_tom_dsa,beta_tom_dsb,  &
                          lamdas,ZdrysnowH,ZdrysnowV)

        CALL partialRefIce(N0s2,alphas2,As,Bs,Cs,alphaa_tom_ds,     &
                          alphab_tom_ds,beta_tom_dsa,beta_tom_dsb,  &
                          lamdas2,temH,temV) 

        ZdrysnowH = ZdrysnowH + temH
        ZdrysnowV = ZdrysnowV + temV

        IF(flg > 2) THEN
          CALL partialRhoIce(N0s,alphas,Cs,Ds,alphaa_ds,        &
                          alphab_ds,beta_tom_dsa,beta_tom_dsb,    &
                          rho_0s,lamdas,ZdrysnowHV)
          CALL partialRhoIce(N0s2,alphas2,Cs,Ds,alphaa_ds,        &
                          alphab_ds,beta_tom_dsa,beta_tom_dsb,    &
                          rho_0s,lamdas2,temHV)

          ZdrysnowHV = ZdrysnowHV + temHV
        ENDIF
      END IF 
      IF(lamdams > 0._r_kind .and. fms > 0._r_kind) THEN
         CALL partialRefIce(N0ms,alphas,As,Bs,Cs,alphaa_ws,      &
                           alphab_ws,beta_sa,beta_sb,lamdams,    &
                           ZwetsnowH,ZwetsnowV) 
         IF(flg > 2) THEN
         CALL partialRhoIce(N0ms,alphas,Cs,Ds,alphaa_ws,           &
                           alphab_ws,beta_sa,beta_sb,rho_0rs,      &
                           lamdams,ZwetsnowHV)
         END IF
      ENDIF 
  END SELECT 


  IF(hl_ON == 1) THEN
    IF(lamdah > 0._r_kind .and. qhf > 0._r_kind)THEN
      CALL partialRefIce(N0h,alphah,Ahd,Bhd,Chd,alphaa_dh,    &
                         alphab_dh,beta_ha,beta_hb,lamdah,    &
                         ZdryhailH,ZdryhailV)
      IF(flg > 2) THEN
        CALL partialRhoIce(N0h,alphah,Chd,Dhd,alphaa_dh,      &
                         alphab_dh,beta_ha,beta_hb,rho_0h,    &
                         lamdah,ZdryhailHV)
      ENDIF
    ENDIF
    IF(lamdamh > 0._r_kind .and. fmh > 0._r_kind) THEN
      CALL partialRefIce(N0mh,alphah,Ah,Bh,Ch,alphaa_wh,       &
                         alphab_wh,beta_ha,beta_hb,lamdamh,    &
                         ZwethailH,ZwethailV)
      IF(flg > 2) THEN
        CALL partialRhoIce(N0mh,alphah,Ch,Dh,alphaa_wh,        &
                         alphab_wh,beta_ha,beta_hb,rho_0rh,    &
                         lamdamh,ZwethailHV)
      ENDIF
    ENDIF
  ENDIF 

  IF(grpl_ON == 1) THEN
    IF(lamdag > 0._r_kind .and. qgf > 0._r_kind)THEN
      CALL partialRefIce(N0g,alphag,Agd,Bgd,Cgd,alphaa_dg,    &
                         alphab_dg,beta_ga, beta_gb,lamdag,   &
                         ZdrygrplH,ZdrygrplV)
      IF(flg > 2) THEN
        CALL partialRhoIce(N0g,alphag,Cgd,Dgd,alphaa_dg,      &
                         alphab_dg,beta_ga,beta_gb,rho_0g,    &
                         lamdag,ZdrygrplHV)
      ENDIF
    ENDIF
     IF(lamdamg > 0._r_kind .and. fmg > 0._r_kind) THEN 
      CALL partialRefIce(N0mg,alphag,Ag,Bg,Cg,alphaa_wg,       &
                         alphab_wg,beta_ga,beta_gb,lamdamg,    &
                         ZwetgrplH,ZwetgrplV)
      IF(flg > 2) THEN
        CALL partialRhoIce(N0mg,alphag,Cg,Dg,alphaa_wg,        &
                         alphab_wg,beta_ga,beta_gb,rho_0rg,    &
                         lamdamg,ZwetgrplHV)
      ENDIF
    ENDIF
  ENDIF

  IF(lamdar > 0._r_kind) THEN
    CALL partialRefRain(N0r,alphar,alphaa,alphab,beta_ra,beta_rb,  &
                       lamdar,rainReflH,rainReflV)
    rainReflV = MIN(rainReflV,rainReflH)
    IF(flg > 2) THEN

    CALL partialRhoRain(N0r,alphar,alphaa,alphab,beta_ra,beta_rb,  &
                        lamdar,rainReflHV)
    ENDIF
  ENDIF

  rainIceRefl_hh=rainReflH+ZdrysnowH+ZwetsnowH+ZdryhailH+ZwethailH &
                 +ZdrygrplH+ZwetgrplH
 
  log_ref = 10._r_kind*LOG10(MAX(1.0_r_kind,rainIceRefl_hh))

  IF(flg == 1) THEN
    rainIceRefl = assign_Refl(rainIceRefl_hh,rainIceRefl_vv,zdr,log_ref)

  ELSE IF(flg > 1) THEN
!-----------------------------------------------------------------------
! Calculate differential reflectivity (Zdr)
!-----------------------------------------------------------------------
    rainIceRefl_vv=rainReflV+ZdrysnowV+ZwetsnowV+ZdryhailV+ZwethailV &
                  +ZdrygrplV+ZwetgrplV

    if(rainIceRefl_vv > 0._r_kind) then
      zdr = 10._r_kind*LOG10(MAX(1.0_r_kind,rainIceRefl_hh/rainIceRefl_vv))
    endif

    rainIceRefl = assign_Refl(rainIceRefl_hh,rainIceRefl_vv,zdr,log_ref)

    IF(flg > 2) THEN

      rainIceRefl_hv=rainReflHV+ZdrysnowHV+ZwetsnowHV                  &
                   +ZdryhailHV+ZwethailHV+ZdrygrplHV+ZwetgrplHV

!-----------------------------------------------------------------------
! Safety block to ensure r_hv <= 1.
!-----------------------------------------------------------------------
      IF(rainIceRefl_hv > SQRT(rainIceRefl_hh*rainIceRefl_vv)) &
         rainIceRefl_hv = SQRT(rainIceRefl_hh*rainIceRefl_vv)
!-----------------------------------------------------------------------

      rainIceRefl%T_sum_ref_hv = rainIceRefl_hv

    ENDIF
  ENDIF

END FUNCTION rainIceRefl

!########################################################################
!########################################################################
!#########                                                      #########
!#########              FUNCTION calculate_obs                  #########
!#########                                                      #########
!#########                     Developed by                     #########
!#########     Center for Analysis and Prediction of Storms     #########
!#########                University of Oklahoma                #########
!#########                                                      #########
!########################################################################
!########################################################################

 TYPE(T_obs_dual) FUNCTION calculate_obs(rho,var_dsd,flg)

!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 2/27/2007
!
! flg == (1: Zh, 2: Zdr, 3: rho_hv)
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare arguments.
!-----------------------------------------------------------------------

  REAL(r_kind),     INTENT(IN   ) :: rho ! Air density (kg m**-3)

  TYPE(T_para_dsd), INTENT(IN   ) :: var_dsd

  INTEGER(i_kind),  INTENT(IN   ) :: flg   ! flag for ref(1) and zdr(2)

  REAL(r_kind) :: qr
  REAL(r_kind) :: qs
  REAL(r_kind) :: qh
  REAL(r_kind) :: qg

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  calculate_obs = init_Refl()

!-----------------------------------------------------------------------
! Check for bad air density value.
!-----------------------------------------------------------------------
 
  qr = var_dsd%T_qr
  qs = var_dsd%T_qs
  qh = var_dsd%T_qh
  qg = var_dsd%T_qg


  IF (rho > 0.0_r_kind .and. &
      (qr > 0._r_kind .or. qs > 0._r_kind .or. qh > 0._r_kind .or. qg > 0._r_kind)) THEN 
     calculate_obs = rainIceRefl(var_dsd,rho,flg)
  END IF

END FUNCTION  calculate_obs

!--- from convert2radar
REAL(r_kind) FUNCTION snow_alpha_a(fw)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This user defined function calculates alpha for Zhh
! for dry/melting snow.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 5/30/2006
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare local variables.
!-----------------------------------------------------------------------

  REAL(r_kind),INTENT(IN   ) :: fw

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  snow_alpha_a = (0.194_r_kind + 7.094_r_kind*fw &
                  + 2.135_r_kind*fw**2._r_kind   &
                  - 5.225_r_kind*fw**3._r_kind)*10._r_kind**(-4)

END FUNCTION snow_alpha_a

REAL(r_kind) FUNCTION snow_alpha_b(fw)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This user defined function calculates alpha for Zvv
! for dry/melting snow.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 5/30/2006
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare local variables.
!-----------------------------------------------------------------------

  REAL(r_kind),INTENT(IN   ) :: fw

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  snow_alpha_b = (0.191_r_kind + 6.916_r_kind*fw &
                  - 2.841_r_kind*fw**2._r_kind &
                  - 1.160_r_kind*fw**3._r_kind)*10._r_kind**(-4)

END FUNCTION snow_alpha_b

!########################################################################
!########################################################################
!#########                                                      #########
!#########                  FUNCTIONS for hail                  #########
!#########                                                      #########
!#########                     Developed by                     #########
!#########     Center for Analysis and Prediction of Storms     #########
!#########                University of Oklahoma                #########
!#########                                                      #########
!########################################################################
!########################################################################

REAL(r_kind) FUNCTION hail_alpha_a(fw)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This user defined function calculates alpha for Zhh
! for dry/melting hail.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 1/22/2007
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare local variables.
!-----------------------------------------------------------------------

  REAL(r_kind),INTENT(IN   ) :: fw

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  hail_alpha_a = (0.191_r_kind + 2.39_r_kind*fw &
                  - 12.57_r_kind*fw**2._r_kind  &
                  + 38.71_r_kind*fw**3._r_kind  &
                  - 65.53_r_kind*fw**4._r_kind  &
                  + 56.16_r_kind*fw**5._r_kind  &
                  - 18.98_r_kind*fw**6._r_kind)*10._r_kind**(-3)

END FUNCTION hail_alpha_a

REAL(r_kind) FUNCTION hail_alpha_b(fw)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This user defined function calculates alpha for Zvv
! for dry/melting hail.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 1/22/2007
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare local variables.
!-----------------------------------------------------------------------

  REAL(r_kind),INTENT(IN   ) :: fw

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  hail_alpha_b = (0.165_r_kind + 1.72_r_kind*fw &
                  - 9.92_r_kind*fw**2._r_kind   &
                  + 32.15_r_kind*fw**3._r_kind  &
                  - 56.0_r_kind*fw**4._r_kind   &
                  + 48.83_r_kind*fw**5._r_kind  &
                  - 16.69_r_kind*fw**6._r_kind)*10._r_kind**(-3)

END FUNCTION hail_alpha_b
!
!########################################################################
!########################################################################
!#########                                                      #########
!#########                  FUNCTIONS for graupel               #########
!#########                                                      #########
!#########                     Developed by                     #########
!#########     Center for Analysis and Prediction of Storms     #########
!#########                University of Oklahoma                #########
!#########                                                      #########
!########################################################################
!########################################################################
!

REAL(r_kind) FUNCTION grpl_alpha_a(fw)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This user defined function calculates alpha for Zhh
! for dry/melting graupel.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 3/10/2010
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare local variables.
!-----------------------------------------------------------------------
  REAL(r_kind),INTENT(IN   ) :: fw

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  grpl_alpha_a = (0.081_r_kind + 2.04_r_kind*fw &
                  - 7.39_r_kind*fw**2._r_kind   &
                  + 18.14_r_kind*fw**3._r_kind  &
                  - 26.02_r_kind*fw**4._r_kind  &
                  + 19.37_r_kind*fw**5._r_kind  &
                  - 5.75_r_kind*fw**6._r_kind)*10._r_kind**(-3)

END FUNCTION grpl_alpha_a

REAL(r_kind) FUNCTION grpl_alpha_b(fw)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This user defined function calculates alpha for Zvv
! for dry/melting graupel.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 3/10/2010
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare local variables.
!-----------------------------------------------------------------------

  REAL(r_kind),INTENT(IN   ) :: fw

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  grpl_alpha_b = (0.076_r_kind + 1.74_r_kind*fw &
                  - 7.52_r_kind*fw**2._r_kind   &
                  + 20.22_r_kind*fw**3._r_kind  &
                  - 30.42_r_kind*fw**4._r_kind  &
                  + 23.31_r_kind*fw**5._r_kind  &
                  - 7.06_r_kind*fw**6._r_kind)*10._r_kind**(-3)

END FUNCTION grpl_alpha_b

INTEGER(i_kind) FUNCTION get_qgh_opt(graupel_ON, hail_ON)

  INTEGER(i_kind), INTENT(IN   ) :: graupel_ON,hail_ON

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  IF(graupel_ON == 0 .and. hail_ON == 0) THEN
    get_qgh_opt = 1
  ELSE IF(graupel_ON == 0 .and. hail_ON == 1) THEN
    get_qgh_opt = 2
  ELSE IF(graupel_ON == 1 .and. hail_ON == 0) THEN
    get_qgh_opt = 3
  ELSE IF(graupel_ON == 1 .and. hail_ON == 1) THEN
    get_qgh_opt = 4
  ENDIF

END FUNCTION get_qgh_opt

! Adopted from RADREMUL library of ARPS system
! Used for direct reflectity DA capability
!########################################################################
!########################################################################
!#########                                                      #########
!#########                     convert2radar                    #########
!#########                                                      #########
!#########                     Developed by                     #########
!#########     Center for Analysis and Prediction of Storms     #########
!#########                University of Oklahoma                #########
!#########                                                      #########
!########################################################################
!########################################################################

SUBROUTINE partialRefRain(N0,alpha,alp_a,alp_b,beta_a,beta_b,lamda,    &
                           refRainHH,refRainVV)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This function calculates the partial reflectivity for rain species
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Bryan Putnam, 4/16/2013
!
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare variables.
!-----------------------------------------------------------------------

  REAL(r_kind),INTENT(IN   ) :: N0,alpha,alp_a,alp_b
  REAL(r_kind),INTENT(IN   ) :: lamda,beta_a,beta_b
  REAL(r_kind),INTENT(  OUT) :: refRainHH,refRainVV

  !local variables
  REAL(r_kind) :: expon_h
  REAL(r_kind) :: gamma_h
  REAL(r_kind) :: expon_v
  REAL(r_kind) :: gamma_v
  REAL(r_kind) :: N0_units

  REAL(r_double) :: gamma

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  N0_units = (1.e-3_r_kind)**(4.0_r_kind+alpha)

  gamma_h = sngl(gamma(dble(alpha)+2.0_r_double*dble(beta_a)+1.0_r_double))
  expon_h = -(alpha+2*beta_a+1)
  gamma_v = sngl(gamma(dble(alpha)+2.0_r_double*dble(beta_b)+1.0_r_double))
  expon_v = -(alpha+2*beta_b+1)

  refRainHH = mm3todBZ*radar_const*alp_a**2*(N0*N0_units)*gamma_h* &
              (lamda*1.e-3_r_kind)**expon_h

  refRainVV = mm3todBZ*radar_const*alp_b**2*(N0*N0_units)*gamma_v* &
              (lamda*1.e-3_r_kind)**expon_v


END SUBROUTINE partialRefRain


SUBROUTINE partialRhoRain(N0,alpha,alp_a,alp_b,beta_a,beta_b,         &
                          lamda,refRainHV)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This function calculates the cross components, Z_hv, for rain species
! for rho_hv calculation.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Bryan Putnam, 4/16/2013
!
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare variables.
!-----------------------------------------------------------------------

  REAL(r_kind),INTENT(IN   ) :: N0,alpha,alp_a,alp_b,beta_a,beta_b,lamda

  REAL(r_kind),INTENT(  OUT) :: refRainHV

  !local variables
  REAL(r_kind) :: expon_hv
  REAL(r_kind) :: gamma_hv
  REAL(r_kind) :: N0_units

  REAL(r_double) :: gamma

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  N0_units = (1.e-3_r_kind)**(4.0_r_kind+alpha)

  gamma_hv = sngl(gamma(dble(beta_a)+dble(beta_b)+dble(alpha)+1.0_r_double))
  expon_hv = -(alpha+beta_a+beta_b+1)

  refRainHV = mm3todBZ*radar_const*alp_a*alp_b*(N0*N0_units)*           &
                gamma_hv*(lamda*1.e-3_r_kind)**expon_hv

END SUBROUTINE partialRhoRain


SUBROUTINE partialRefIce(N0,alpha,Ai,Bi,Ci,alp_a,alp_b,beta_a,beta_b,   &
                         lamda,refIceHH,refIceVV)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This function calculates the partial reflectivity for each species
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 1/22/2007
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare variables.
!-----------------------------------------------------------------------
  REAL(r_kind),INTENT(IN   ) :: N0,alpha,Ai,Bi,Ci,alp_a,alp_b,beta_a,beta_b
  REAL(r_kind),INTENT(IN   ) :: lamda

  REAL(r_kind),INTENT(  OUT) :: refIceHH,refIceVV

  !local variables
  REAL(r_kind) :: gamma_h, gamma_v, expon_h, expon_v
  REAL(r_kind) :: N0_units
  REAL(r_double) :: gamma

  gamma_h = sngl(gamma(dble(alpha) + 2.0_r_double*dble(beta_a)+1.0_r_double))
  expon_h = -(alpha+2*beta_a+1)
  gamma_v = sngl(gamma(dble(alpha) + 2.0_r_double*dble(beta_b)+1.0_r_double))
  expon_v = -(alpha + 2*beta_b+1)

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  N0_units = (1.e-3_r_kind)**(4.0_r_kind+alpha)

  refIceHH = mm3toDBZ*radar_Const*gamma_h*(N0*N0_units)*                    &
             (Ai*alp_a**2+Bi*alp_b**2+2_r_kind*Ci*alp_a*alp_b)*             &
             (lamda*1.e-3_r_kind)**expon_h

  refIceVV = mm3toDBZ*radar_Const*gamma_v*(N0*N0_units)*                    &
             (Bi*alp_a**2+Ai*alp_b**2+2_r_kind*Ci*alp_a*alp_b)*             &
             (lamda*1.e-3_r_kind)**expon_v


END SUBROUTINE partialRefIce

SUBROUTINE partialRhoIce(N0,alpha,Ci,Di,alp_a,alp_b,beta_a,beta_b,rho_0,lamda,refIceHV)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This function calculates the cross components, Z_hv, for each species
! for rho_hv calculation.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 12/16/2007
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------
  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare variables.
!-----------------------------------------------------------------------
  REAL(r_kind),INTENT(IN   ) :: N0,alpha,Ci,Di,alp_a,alp_b,beta_a,beta_b
  REAL(r_kind),INTENT(IN   ) :: rho_0,lamda

  REAL(r_kind),INTENT(  OUT) :: refIceHV

  !local variables
  REAL(r_kind) :: gamma_hv, expon
  REAL(r_kind) :: N0_units
  REAL(r_double) :: gamma

  gamma_hv = sngl(gamma(dble(beta_a)+dble(beta_b)+dble(alpha) + 1.0_r_double))
  expon = -(alpha + beta_a + beta_b + 1)

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  N0_units = (1.e-3_r_kind)**(4.0_r_kind+alpha)

  refIceHV = mm3todBZ*radar_Const*gamma_hv*(N0*N0_units)*                &
             (Ci*alp_a**2+Ci*alp_b**2+2_r_kind*Di*alp_a*alp_b*rho_0)*    &
             (lamda*1.e-3_r_kind)**expon

END SUBROUTINE partialRhoIce

SUBROUTINE fractionWater(qr,qi,fo,density_ice,fracqr,fracqi,fm,fw,rhom)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This subroutine calculates the fractions of water, dry ice (snow or
! hail), the mixture. It also calculate the density of mixture.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 5/30/2006
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare variables.
!-----------------------------------------------------------------------

  REAL(r_kind),INTENT(IN   ) :: qr, qi, fo, density_ice
  REAL(r_kind),INTENT(  OUT) :: fracqr, fracqi, fm, fw, rhom

  REAL(r_kind) :: fr

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  fr = 0._r_kind
  fw = 0._r_kind
  fracqr = 0._r_kind
  fracqi = 0._r_kind
  fm = 0._r_kind
  rhom = 0._r_kind

!-----------------------------------------------------------------------
! Calculate the fraction of mleting ice (fr) based on the ratio between
! qr and qi. fo is the maximum allowable fraction of melting snow.
!-----------------------------------------------------------------------
  IF (qr > 0._r_kind .AND. qi > 0._r_kind) THEN
    fr = fo*(MIN(qi/qr,qr/qi))**.3_r_kind
  ENDIF

!-----------------------------------------------------------------------
! Calculate the faction of water and ice.
! fracqr : the mass of water in the melting ice
! fracqi : the mass of ice in the melting ice
! fm     : total mass of melting ice
! fw     : the fraction of water within melting ice
! rhom   : density of mixture
!-----------------------------------------------------------------------
  fracqr = fr * qr
  fracqi = fr * qi
  fm = fracqr + fracqi

  IF (fm == 0._r_kind .AND. qr > 0._r_kind) THEN
    fw = 1._r_kind
  ELSE IF (fm > 0._r_kind) THEN
    fw = fracqr/fm
  ENDIF

  rhom = 1000._r_kind*fw**2._r_kind + (1._r_kind-fw**2._r_kind)*density_ice

END SUBROUTINE fractionWater

SUBROUTINE fractionWater_temperature_snow (qi,density_ice,fm,fw,rhom,tair_C)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This subroutine calculates the fractions of water, dry ice (snow or
! hail), the mixture based on the air temperature.
! It also calculate the density of mixture.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 7/25/2014
!
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare variables.
!-----------------------------------------------------------------------

  REAL(r_kind),INTENT(IN   ) :: qi, density_ice, tair_C
  REAL(r_kind),INTENT(  OUT) :: fm, fw, rhom
  REAL(r_kind) :: layer_tmax, layer_tmin

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  fw = 0._r_kind
  fm = 0._r_kind
  rhom = 0._r_kind

!-----------------------------------------------------------------------
! Calculate the faction of water.
! fm     : total mass of melting ice
! fw     : the fraction of water within melting ice
! rhom   : density of mixture
!-----------------------------------------------------------------------

  fm = qi

! Compute the degree of wet in percentage based on air temperature
  layer_tmax = 2.5_r_kind
  layer_tmin = -2.5_r_kind
  if(tair_C >= layer_tmin .and. tair_C < layer_tmax) then
    fw = (tair_C - layer_tmin)/(layer_tmax-layer_tmin)
  else if(tair_C >= layer_tmax) then
    fw = 1._r_kind
  else
    fm = 0._r_kind
    fw = 0._r_kind
  endif

  rhom = 1000._r_kind*fw**2._r_kind + (1._r_kind-fw**2._r_kind)*density_ice

END SUBROUTINE fractionWater_temperature_snow

SUBROUTINE fractionWater_temperature_hail(qi,density_ice,fm,fw,rhom,tair_C)
  
!-----------------------------------------------------------------------
! 
! PURPOSE:
!
! This subroutine calculates the fractions of water, dry ice (snow or
! hail), the mixture based on the air temperature.
! It also calculate the density of mixture.
! 
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 7/25/2014
! 
!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare variables.
!-----------------------------------------------------------------------

  REAL(r_kind),INTENT(IN   ) :: qi, density_ice, tair_C
  REAL(r_kind),INTENT(  OUT) :: fm, fw, rhom
  REAL(r_kind) :: layer_tmax, layer_tmin
  REAL(r_kind) :: maxfrac

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  fw = 0._r_kind
  fm = 0._r_kind
  rhom = 0._r_kind
  maxfrac = 0.6_r_kind

!-----------------------------------------------------------------------
! Calculate the faction of water.
! fm     : total mass of melting ice
! fw     : the fraction of water within melting ice
! rhom   : density of mixture
!-----------------------------------------------------------------------

  fm = qi

! Compute the degree of wet in percentage based on air temperature
  layer_tmax = 5.0_r_kind
  layer_tmin = 0.0_r_kind
  if(tair_C >= layer_tmin .and. tair_C < layer_tmax) then
    fw = (tair_C - layer_tmin)/(layer_tmax-layer_tmin) * maxfrac
  else if(tair_C >= layer_tmax) then
    fw = maxfrac
  else
    fm = 0._r_kind
    fw = 0._r_kind
  endif

  rhom = 1000._r_kind*fw**2._r_kind + (1._r_kind-fw**2._r_kind)*density_ice

END SUBROUTINE fractionWater_temperature_hail

SUBROUTINE power_mom(power,cx,t,rhoa,q,moment)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! Calculates moments of the PSD based on the Field et al. 2005 power law
! relations. Used for Thompson scheme.
!
!
! AUTHOR:  Bryan Putnam, 4/16/2013
!
!-----------------------------------------------------------------------

  USE radarz_cst, only: mphyopt

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare arguments.
!-----------------------------------------------------------------------

  REAL(r_kind),    INTENT(IN   ) :: rhoa
  INTEGER(i_kind), INTENT(IN   ) :: power
  REAL(r_kind),    INTENT(IN   ) :: t,q,cx
  REAL(r_kind),    INTENT(  OUT) :: moment

!-----------------------------------------------------------------------
! Declare local variables.
!-----------------------------------------------------------------------

  REAL(r_kind) :: a,b
  REAL(r_kind) :: rpower  
  REAL(r_double) :: log_a
  REAL(r_kind) :: second_moment
  REAL(r_kind) :: T_c

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


  T_c = t-273.16_r_kind

  SELECT CASE (mphyopt)
    CASE(108)

      second_moment = rhoa * (q/cx)

      IF(power == 2) THEN
        moment = second_moment
      ELSE
 
        rpower = REAL(power)

        log_a = dble(5.065339_r_kind-.062659_r_kind*T_c                    &
                     - 3.032362_r_kind*rpower + 0.029469_r_kind*T_c*rpower &
                     - 0.000285_r_kind*(T_c**2._r_kind)                    &
                     + 0.312550_r_kind*(rpower**2._r_kind)                 &
                     + 0.000204_r_kind*(T_c**2._r_kind)*rpower             &
                     + 0.003199_r_kind*T_c*(rpower**2._r_kind)             &
                     + 0.000000_r_kind*(T_c**3._r_kind)                    &
                     - 0.015952_r_kind*(rpower**3._r_kind))

        a = sngl(10.0_r_double**log_a)

        b = 0.476221_r_kind - 0.015896_r_kind*T_c + 0.165977_r_kind*rpower &
            + 0.007468_r_kind*T_c*rpower - 0.000141_r_kind*(T_c**2._r_kind)&
            + 0.060366_r_kind*(rpower**2._r_kind)                          &
            + 0.000079_r_kind*(T_c**2._r_kind)*rpower                      &
            + 0.000594_r_kind*T_c*(rpower**2._r_kind)                      &
            + 0.000000_r_kind*(T_c**3._r_kind)                             &
            - 0.003577_r_kind*(rpower**3._r_kind)

        moment = a*(second_moment)**b
      END IF

  END SELECT

END SUBROUTINE


SUBROUTINE calc_N0x_mp(rhoa,rhoms,rhomh,rhomg,ntr,nts,nth,ntg,qrf,  &
                       qsf,fms,qhf,fmh,qgf,fmg)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This subroutine calculates intercep parameter based on MP scheme.
!
!
!
! AUTHOR:  Bryan Putnam, 4/16/2013
!
!-----------------------------------------------------------------------

  USE radarz_cst, only: mphyopt

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare arguments.
!-----------------------------------------------------------------------

  REAL(r_kind), INTENT(IN   ) :: rhoa,rhoms,rhomh,rhomg
  REAL(r_kind), INTENT(IN   ) :: ntr,nts,nth,ntg
  REAL(r_kind), INTENT(INOUT) :: qrf,qsf,qhf,qgf
  REAL(r_kind), INTENT(INOUT) :: fms,fmh,fmg

!-----------------------------------------------------------------------
! Declare local variables.
!-----------------------------------------------------------------------
  REAL(r_kind) :: moma,momb
  REAL(r_kind) :: no_value = missing

  REAL(r_kind), PARAMETER :: D0r = 50.e-5_r_kind
  REAL(r_kind), PARAMETER :: R1 = 1.e-12_r_kind
  REAL(r_kind), PARAMETER :: R2 = 1.e-6_r_kind
  REAL(r_kind), PARAMETER :: gonv_min = 1.e4_r_kind
  REAL(r_kind), PARAMETER :: gonv_max = 3.e6_r_kind
  REAL(r_kind), PARAMETER :: bm_g = 3.0_r_kind

  LOGICAL :: L_qr
  REAL(r_kind) :: mvd_r  
  REAL(r_double) :: dble_alfr
  REAL(r_double) :: lamr 
  REAL(r_double) :: gamma  
  REAL(r_kind) :: xslwq,ygra1,zans1
  REAL(r_kind) :: N0_exp,N0_min
  REAL(r_kind) :: rg,am_g,oge1,cgg_1,cgg_2,cgg_3,ogg1,ogg2,ogmg,cge_1
  REAL(r_kind) :: lam_exp,lamg

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

   SELECT CASE (mphyopt)
     CASE(9:12,109)

       CALL calc_N0x_melt(rhoa,rhoms,rhomh,rhomg,ntr,nts,nth,ntg,qrf,   &
                           qsf,fms,qhf,fmh,qgf,fmg)
     CASE(106)

       N0r = 8.0E06_r_kind

       N0g = 4.0E06_r_kind
       N0mg = N0g

       N0s = 2.0E06_r_kind*exp((.12_r_kind*(273.16_r_kind-ta)))
       N0ms = N0s 
     CASE(108) 

       CALL calc_N0x_melt(rhoa,no_value,no_value,no_value,ntr,no_value, &
                          no_value,no_value,qrf,no_value,no_value,      &
                          no_value,no_value,no_value,no_value)

     
       IF(qrf > R1) THEN
         L_qr = .true.  
         dble_alfr = dble(alphar)
         lamr = 0.0_r_kind
         CALL cal_lamda(rhoa,qrf,ntr,rhor,dble_alfr,lamr)
         mvd_r = (3.0_r_kind + alphar + 0.672_r_kind)/sngl(lamr) 
         IF(mvd_r > 2.5e-3_r_kind) THEN
           mvd_r = 2.5e-3_r_kind
         ELSE IF(mvd_r < ((D0r)*(0.75_r_kind))) THEN
           mvd_r =  D0r*0.75_r_kind
         END IF 
       ELSE
         L_qr = .false. 
         qrf = 0.0_r_kind
       END IF

       IF(qgf > R1) THEN
         rg = qgf * rhoa
       ELSE
         rg = R1
       END IF 

       IF((ta < 270.65_r_kind) .and. L_qr .and. (mvd_r > 100.0e-6_r_kind)) THEN
         xslwq = 4.01_r_kind + log10(mvd_r)
       ELSE
         xslwq = 0.01_r_kind
       END IF

       N0_min = gonv_max 
       ygra1 = 4.31_r_kind + log10(max(5.e-5_r_kind,rg))
       zans1 = 3.1_r_kind + (100.0_r_kind/(300.0_r_kind*xslwq*ygra1 &
               /(10.0_r_kind/xslwq+1.0_r_kind+0.25_r_kind*ygra1)    &
               +30.0_r_kind+10.0_r_kind*ygra1))         
       N0_exp = 10.0_r_kind**zans1
       N0_exp = MAX(gonv_min,MIN(N0_exp,gonv_max))
       N0_min = MIN(N0_exp,N0_min)
       N0_exp = N0_min        
       am_g = c_x(5)
       oge1 = 1._r_kind/(bm_g + 1._r_kind)
       cgg_1 = sngl(gamma(dble(bm_g) + 1.0_r_double)) 
       cgg_2 = sngl(gamma(dble(alphag) + 1.0_r_double))  
       cgg_3 = sngl(gamma(dble(bm_g) + dble(alphag) + 1.0_r_double))
       ogg1 = 1._r_kind/cgg_1
       ogg2 = 1._r_kind/cgg_2
       ogmg = 1._r_kind/bm_g 
       cge_1 = alphag + 1.0_r_kind
       lam_exp = (N0_exp*am_g*cgg_1/rg)**oge1
       lamg = lam_exp*(cgg_3*ogg2*ogg1)**ogmg
       N0g = N0_exp/(cgg_2*lam_exp)*lamg**cge_1

       IF(fmg > R1) THEN
         rg = fmg * rhoa
       ELSE
         rg = R1
       END IF

       N0_min = gonv_max
       ygra1 = 4.31_r_kind + log10(max(5.e-5_r_kind,rg))
       zans1 = 3.1_r_kind + (100.0_r_kind/(300.0_r_kind*xslwq*ygra1 &
               /(10.0_r_kind/xslwq+1.0_r_kind+0.25_r_kind*ygra1)    &
               +30.0_r_kind+10.0_r_kind*ygra1))
       N0_exp = 10.0_r_kind**zans1
       N0_exp = MAX(gonv_min,MIN(N0_exp,gonv_max))
       N0_min = MIN(N0_exp,N0_min)
       N0_exp = N0_min
       am_g = c_x(5)
       oge1 = 1._r_kind/(bm_g + 1._r_kind)
       cgg_1 = sngl(gamma(dble(bm_g) + 1.0_r_double))
       cgg_2 = sngl(gamma(dble(alphag) + 1.0_r_double))
       cgg_3 = sngl(gamma(dble(bm_g) + dble(alphag) + 1.0_r_double))
       ogg1 = 1._r_kind/cgg_1
       ogg2 = 1._r_kind/cgg_2
       ogmg = 1._r_kind/bm_g
       cge_1 = alphag + 1.0_r_kind
       lam_exp = (N0_exp*am_g*cgg_1/rg)**oge1
       lamg = lam_exp*(cgg_3*ogg2*ogg1)**ogmg
       N0mg = N0_exp/(cgg_2*lam_exp)*lamg**cge_1

       IF(qsf >= 1.e-14_r_kind) THEN  

         CALL  power_mom(2,c_x(4),ta,rhoa,qsf,moma) 
         CALL  power_mom(3,c_x(4),ta,rhoa,qsf,momb)

         N0s = sngl(((dble(moma)**4.0_r_double)/(dble(momb)**3.0_r_double))*dble(thom_k0))
         N0s2 = sngl(((dble(moma)**4.0_r_double)/(dble(momb)**3.0_r_double))*dble(thom_k1)*      &
                    ((dble(moma)/dble(momb))**dble(alphas2)))

       ELSE
         N0s = 3.0E06_r_kind
         N0s2 = 3.0E06_r_kind
       END IF 

   END SELECT 

END SUBROUTINE

SUBROUTINE calc_N0x_melt(rhoa,rhoms,rhomh,rhomg,ntr,nts,nth,ntg,qrf,   &
                         qsf,fms,qhf,fmh,qgf,fmg)

!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Calculate intercept parameter including melting species
!
!-----------------------------------------------------------------------
!
!  AUTHOR: Bryan Putnam
!    04/16/2013.
!
!-----------------------------------------------------------------------

  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------

  REAL(r_kind), INTENT(IN   )   :: rhoa,rhoms,rhomh,rhomg
  REAL(r_kind), INTENT(IN   )   :: ntr,nts,nth,ntg
  REAL(r_kind), INTENT(INOUT)   :: qrf,qsf,qhf,qgf,fms,fmh,fmg
  REAL(r_double) :: db_N0r, db_N0s, db_N0h, db_N0g
  REAL(r_double) :: db_alfr,db_alfs,db_alfh,db_alfg
  REAL(r_kind), PARAMETER :: epsQ  = 1.e-14_r_kind
  REAL(r_kind), PARAMETER :: epsN  = 1.e-3_r_kind
  REAL(r_kind), PARAMETER :: maxN0 = 4.e+37_r_kind


!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  db_alfr = dble(alphar); db_alfs = dble(alphas); db_alfh = dble(alphah);
  db_alfg = dble(alphag)


  IF(qrf >= epsQ .AND. ntr >= epsN) THEN
     CALL cal_N0(rhoa,qrf,ntr,rhor,db_alfr,db_N0r)
     N0r = MIN(maxN0,sngl(db_N0r))
  ELSE
     qrf = 0.0_r_kind
  ENDIF

  IF(qsf >= epsQ .AND. nts >= epsN) THEN
     CALL cal_N0(rhoa,qsf,nts,rhos,db_alfs,db_N0s)
     N0s = MIN(maxN0,sngl(db_N0s))
  ELSE
     qsf = 0.0_r_kind
  ENDIF

  IF(fms >= epsQ .AND. nts >= epsN) THEN
     CALL cal_N0(rhoa,fms,nts,rhoms,db_alfs,db_N0s)
     N0ms = MIN(maxN0,sngl(db_N0s))
  ELSE
     fms = 0.0_r_kind
  ENDIF

  IF(qhf >= epsQ .AND. nth >= epsN) THEN
     CALL cal_N0(rhoa,qhf,nth,rhoh,db_alfh,db_N0h)
     N0h = MIN(maxN0,sngl(db_N0h))
  ELSE
     qhf = 0.0_r_kind
  ENDIF

  IF(fmh >= epsQ .AND. nth >= epsN) THEN
     CALL cal_N0(rhoa,fmh,nth,rhomh,db_alfh,db_N0h)
     N0mh = MIN(maxN0,sngl(db_N0h))
  ELSE
     fmh = 0.0_r_kind
  ENDIF

  IF(qgf >= epsQ .AND. ntg >= epsN) THEN
     CALL cal_N0(rhoa,qgf,ntg,rhog,db_alfg,db_N0g)
     N0g = MIN(maxN0,sngl(db_N0g))
  ELSE
     qgf = 0.0_r_kind
  ENDIF

  IF(fmg >= epsQ .AND. ntg >= epsN) THEN
     CALL cal_N0(rhoa,fmg,ntg,rhomg,db_alfg,db_N0g)
     N0mg = MIN(maxN0,sngl(db_N0g))
  ELSE
     fmg = 0.0_r_kind
  ENDIF

END SUBROUTINE calc_N0x_melt

FUNCTION gamma(xx)

!  Modified from "Numerical Recipes"

  IMPLICIT NONE

! PASSING PARAMETERS:
  REAL(r_double), INTENT(IN   ) :: xx

! LOCAL PARAMETERS:
  REAL(r_double) :: gamma
  REAL(r_double) :: ser,stp,tmp,x,y,cof(6)
  INTEGER(i_kind)  :: j


  SAVE cof,stp
  DATA cof,stp/76.180091729471460_r_double,-86.505320329416770_r_double,               &
       24.014098240830910_r_double,-1.2317395724501550_r_double,.1208650973866179e-2_r_double,  &
       -.5395239384953e-5_r_double,2.50662827463100050_r_double/
  x=xx
  y=x
  tmp=x+5.50_r_double
  tmp=(x+0.50_r_double)*log(tmp)-tmp
  ser=1.0000000001900150_r_double
  do j=1,4
     y=y+1.0_r_double
     ser=ser+cof(j)/y
  enddo
  gamma=tmp+log(stp*ser/x)
  gamma= exp(gamma)

END FUNCTION gamma

SUBROUTINE cal_N0(rhoa,q,Ntx,rhox,alpha,N0)
!
!-----------------------------------------------------------------------
!  PURPOSE:  Calculates intercept parameter and "effective" intercept parameter
!
!-----------------------------------------------------------------------
!
!  AUTHOR: Dan Dawson
!  (02/06/2008)
!
!  MODIFICATION HISTORY:
!
!  (03/26/2008)
!  Recast N0 as a double precision variable, and used double precision for
!  all intermediate calculations.  The calling subroutine should
!  also define it as double precision.  For situations with large alpha,
!  N0 can become very large, and loss of precision can result.
!  Also tweaked the calculation of N0 a bit to avoid overflow, in keeping
!  With Jason Milbrandt's calculation of N0 just before evaporation in
!  the multi-moment code.
!
!-----------------------------------------------------------------------
!  Variable Declarations:
!-----------------------------------------------------------------------
!
  IMPLICIT NONE

  REAL(r_kind),   INTENT(IN   ) :: rhoa,q,Ntx
  REAL(r_kind),   INTENT(IN   ) :: rhox
  REAL(r_double), INTENT(IN   ) :: alpha
  REAL(r_double), INTENT(  OUT) :: N0

  REAL(r_kind), PARAMETER :: pi = 3.141592_r_kind   ! pi
  REAL(r_double) :: gamma1, gamma4

  REAL(r_double) :: gamma

  REAL(r_double) :: lamda

  gamma1 = gamma(1.0_r_double+dble(alpha))
  gamma4 = gamma(4.0_r_double+dble(alpha))

  IF(rhoa > 0.0_r_kind .and. q > 0.0_r_kind) THEN
    lamda = ((gamma4/gamma1)*dble(pi/6._r_kind*rhox)*dble(Ntx)/(dble(rhoa)*  &
        dble(q)))**(1.0_r_double/3.0_r_double)
  ELSE
    lamda = 0.0_r_double
  END IF

  N0 = dble(Ntx)*lamda**(0.50_r_double*(1.0_r_double+dble(alpha)))*                         &
              (1.0_r_double/gamma1)*lamda**(0.50_r_double*(1.0_r_double+dble(alpha)))

END SUBROUTINE cal_N0


SUBROUTINE calc_lamda_mp(rhoa,rhoms,rhomh,rhomg,ntr,nts,nth,ntg,  &
                             qrf,qsf,fms,qhf,fmh,qgf,fmg)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! Calculate slope parameter for PSD based on MP scheme.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Bryan Putnam, 4/16/2013
!
!-----------------------------------------------------------------------

  USE radarz_cst, only: mphyopt

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare arguments.
!-----------------------------------------------------------------------


  REAL(r_kind), INTENT(IN   ) :: rhoa,rhoms,rhomh,rhomg
  REAL(r_kind), INTENT(IN   ) :: ntr,nts,nth,ntg
  REAL(r_kind), INTENT(IN   ) :: qrf,qsf,fms,qhf,fmh,qgf,fmg

!-----------------------------------------------------------------------
! Declare local variables.
!-----------------------------------------------------------------------


  REAL(r_double) :: db_N0,dble_alfr,dble_alfs,dble_alfg,dble_alfh
  REAL(r_double) :: lamr,lams,lamrs,lamh,lamrh,lamg,lamrg
  REAL(r_kind) :: Ntw,Ntd

  REAL(r_kind) :: tem1,tem2

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


  dble_alfr = dble(alphar)
  dble_alfs = dble(alphas)
  dble_alfg = dble(alphag)
  dble_alfh = dble(alphah)

  if (qrf > 0.0_r_kind) then

     Ntw = 0._r_kind
     if (ntr > 0.0_r_kind) then
        Ntw = ntr
        CALL cal_lamda(rhoa,qrf,Ntw,rhor,dble_alfr,lamr)
        lamdar = sngl(lamr)
     else
        db_N0 = dble(N0r)
        CALL cal_Nt(rhoa,qrf,db_N0,c_x(2),dble_alfr,Ntw)
        CALL cal_lamda(rhoa,qrf,Ntw,rhor,dble_alfr,lamr)
        lamdar = sngl(lamr)
     end if
  else
     lamdar = 0.0_r_kind
  end if

  SELECT CASE (mphyopt)
     CASE(1:11,106,109,110,116)
        if (qsf > 0.0_r_kind) then
           Ntd = 0._r_kind
           if (nts > 0.0_r_kind) then
              Ntd = nts
              CALL cal_lamda(rhoa,qsf,Ntd,rhos,dble_alfs,lams)
              lamdas = sngl(lams)
           else
              db_N0 = dble(N0s)
              CALL cal_Nt(rhoa,qsf,db_N0,c_x(4),dble_alfs,Ntd)
              CALL cal_lamda(rhoa,qsf,Ntd,rhos,dble_alfs,lams)
              lamdas = sngl(lams)
           end if
        else
           lamdas = 0.0_r_kind
        end if

        if (fms > 0.0_r_kind) then
           Ntw = 0._r_kind
           if (nts > 0.0_r_kind) then
              Ntw = nts
              CALL cal_lamda(rhoa,fms,Ntw,rhoms,dble_alfs,lamrs)
              lamdams = sngl(lamrs)
           else
              db_N0 = dble(N0s)
              CALL cal_Nt(rhoa,fms,db_N0,c_x(4),dble_alfs,ntw)
              CALL cal_lamda(rhoa,fms,Ntw,rhoms,dble_alfs,lamrs)
              lamdams = sngl(lamrs)
           end if
        else
           lamdams = 0.0_r_kind
        end if

     CASE(108)
        if (qsf > 0.0_r_kind) then

           CALL power_mom(2,c_x(4),ta,rhoa,qsf,tem1)
           CALL power_mom(3,c_x(4),ta,rhoa,qsf,tem2)
           lamdas = (tem1/tem2)*thom_lam0
           lamdas2  = (tem1/tem2)*thom_lam1
        else
           lamdas = 0.0_r_kind
           lamdas2 = 0.0_r_kind
        end if

        if (fms > 0.0_r_kind) then
           Ntw = 0._r_kind
           if (nts > 0.0_r_kind) then
              Ntw = nts
              CALL cal_lamda(rhoa,fms,Ntw,rhoms,dble_alfs,lamrs)
              lamdams = sngl(lamrs)
           else
              db_N0 = dble(N0ms)
              CALL cal_Nt(rhoa,fms,db_N0,c_x(4),dble_alfs,ntw)
              CALL cal_lamda(rhoa,fms,Ntw,rhoms,dble_alfs,lamrs)
              lamdams = sngl(lamrs)
           end if
        end if
  END SELECT

  if (hl_ON == 1) then
     if (qhf > 0._r_kind) then
        Ntd = 0._r_kind
        if (nth > 0.0_r_kind) then
           Ntd = nth
           CALL cal_lamda(rhoa,qhf,Ntd,rhoh,dble_alfh,lamh)
           lamdah = sngl(lamh)
        else
           db_N0 = dble(N0h)
           CALL cal_Nt(rhoa,qhf,db_N0,c_x(6),dble_alfh,Ntd)
           CALL cal_lamda(rhoa,qhf,Ntd,rhoh,dble_alfh,lamh)
           lamdah = sngl(lamh)
        end if
     else
        lamdah = 0.0_r_kind
     end if

     if (fmh > 0._r_kind) then
        Ntw = 0._r_kind
        if (nth > 0.0_r_kind) then
           Ntw = nth
           CALL cal_lamda(rhoa,fmh,Ntw,rhomh,dble_alfh,lamrh)
           lamdamh = sngl(lamrh)
        else
           db_N0 = dble(N0mh)
           CALL cal_Nt(rhoa,fmh,db_N0,c_x(6),dble_alfh,Ntw)
           CALL cal_lamda(rhoa,fmh,Ntw,rhomh,dble_alfh,lamrh)
           lamdamh = sngl(lamrh)
        end if
     else
        lamdamh = 0.0_r_kind
     end if
  end if

  if (grpl_ON == 1) then
     if (qgf > 0._r_kind) then
        Ntd = 0._r_kind
        if (ntg > 0.0_r_kind) then
           Ntd = ntg
           CALL cal_lamda(rhoa,qgf,Ntd,rhog,dble_alfg,lamg)
           lamdag = sngl(lamg)
        else
           db_N0 = dble(N0g)
           CALL cal_Nt(rhoa,qgf,db_N0,c_x(5),dble_alfg,Ntd)
           CALL cal_lamda(rhoa,qgf,Ntd,rhog,dble_alfg,lamg)
           lamdag = sngl(lamg)
        end if
     else
        lamdag = 0.0_r_kind
     end if

     if (fmg > 0._r_kind) then
        Ntw = 0._r_kind
        if (ntg > 0.0_r_kind) then
           Ntw = ntg
           CALL cal_lamda(rhoa,fmg,Ntw,rhomg,dble_alfg,lamrg)
           lamdamg = sngl(lamrg)
        else
           db_N0 = dble(N0mg)
           CALL cal_Nt(rhoa,fmg,db_N0,c_x(5),dble_alfg,Ntw)
           CALL cal_lamda(rhoa,fmg,Ntw,rhomg,dble_alfg,lamrg)
           lamdamg = sngl(lamrg)
        end if
     else
        lamdamg = 0.0_r_kind
     end if
  end if

END SUBROUTINE

SUBROUTINE cal_Nt(rhoa,q,N0,cx,alpha,Ntx)
!
!-----------------------------------------------------------------------
!  PURPOSE:  Calculates number concentration at scalar points
!-----------------------------------------------------------------------
!
!  AUTHOR: Dan Dawson
!  (02/06/2008)
!
!  MODIFICATION HISTORY:
!
!  03/31/08 - converted intermediate calculations to double precision
!             as well as a few of the input arguments.
!
!-----------------------------------------------------------------------
!  Variable Declarations:
!-----------------------------------------------------------------------
!
  IMPLICIT NONE

  REAL(r_kind),  INTENT(IN   ) :: rhoa,q
  REAL(r_double),INTENT(IN   ) :: alpha,N0
  REAL(r_kind),  INTENT(IN   ) :: cx
  REAL(r_kind),  INTENT(  OUT) :: Ntx
  REAL(r_double) :: gamma1,gamma4

  REAL(r_double) :: gamma

  gamma1 = gamma(1.0_r_double+dble(alpha))
  gamma4 = gamma(4.0_r_double+dble(alpha))

  Ntx = sngl((dble(N0)*gamma1)**(3.0_r_double/(4.0_r_double+dble(alpha)))*   &
            ((gamma1/gamma4)*dble(rhoa)* &
            dble(q)/dble(cx))**((1.0_r_double+dble(alpha))/(4.0_r_double+dble(alpha))))

END SUBROUTINE cal_Nt

SUBROUTINE cal_lamda(rhoa,q,Ntx,rhox,alpha,lamda)
!
!-----------------------------------------------------------------------
!  PURPOSE:  Calculates slope parameter lamda
!
!-----------------------------------------------------------------------
!
!  AUTHOR: Dan Dawson!  (02/06/2008)
!
!  MODIFICATION HISTORY:
!  (03/31/2008)
!  Converted intermediate calculations and arrays alpha and lamda to
!  double precision.
!-----------------------------------------------------------------------
!  Variable Declarations:
!-----------------------------------------------------------------------
!

  REAL(r_kind),  INTENT(IN   ) :: rhoa,q
  REAL(r_double),INTENT(IN   ) :: alpha
  REAL(r_kind),  INTENT(IN   ) :: Ntx
  REAL(r_kind),  INTENT(IN   ) :: rhox
  REAL(r_double),INTENT(  OUT) :: lamda

  REAL(r_kind), PARAMETER :: pi = 3.141592_r_kind ! pi
  REAL(r_double) :: gamma1, gamma4

  REAL(r_double) :: gamma

  gamma1 = gamma(1.0_r_double+dble(alpha))
  gamma4 = gamma(4.0_r_double+dble(alpha))

  IF(rhoa > 0.0_r_kind .and. q > 0.0_r_kind) THEN
    lamda = sngl(((gamma4/gamma1)*dble(pi/6._r_kind*rhox)*dble(Ntx)/(dble(rhoa)*  &
          dble(q)))**(1.0_r_double/3.0_r_double))

  ELSE
    lamda = 0.0_r_double
  END IF

END SUBROUTINE cal_lamda

!
!########################################################################
!########################################################################
!#########                                                      #########
!#########               SUBROUTINE RSET_DSD_PARA               #########
!#########                                                      #########
!#########                     Developed by                     #########
!#########     Center for Analysis and Prediction of Storms     #########
!#########                University of Oklahoma                #########
!#########                                                      #########
!########################################################################
!########################################################################

SUBROUTINE set_dsd_para()

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! This subroutine sets intercept parameters for rain/snow/hail and
! densities for snow/hail based on values in history dump.
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, Spring 2010
!
!-----------------------------------------------------------------------
! Include global variables only for dual-pol calculations
!-----------------------------------------------------------------------

  USE radarz_cst, only: mphyopt,n0rain,n0snow,n0hail,n0grpl
  USE radarz_cst, only: rhosnow,rhohail,rhogrpl
  USE radarz_cst, only: alpharain,alphasnow,alphagrpl,alphahail

!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Include files.
!-----------------------------------------------------------------------

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  CALL model_dsd(n0rain,n0snow,n0hail,n0grpl,rhosnow,rhohail,rhogrpl,  &
       alpharain,alphasnow,alphagrpl,alphahail)

  IF (rhos <= 0.0_r_kind) THEN
    rhos = 100._r_kind
  END IF

  IF (rhoh <= 0.0_r_kind) THEN
    rhoh = 913._r_kind
  END IF

  IF (rhog <= 0.0_r_kind) THEN

    SELECT CASE (mphyopt)
      CASE(1:12,108:110)
        rhog = 400._r_kind
      CASE(106,116)
        rhog = 500._r_kind
    END SELECT

  END IF

  IF (N0r <= 0.0_r_kind) THEN
    N0r = 8.0E+06_r_kind
  END IF

  IF (N0s <= 0.0_r_kind) THEN
    N0s = 3.0E+06_r_kind
    SELECT CASE (mphyopt)
      CASE(1:12,106,108:110,116)
        N0s2 = 0.0_r_kind
    END SELECT
  END IF

  IF (N0h <= 0.0_r_kind) THEN
    N0h = 4.0E+04_r_kind
  END IF

  IF (N0g <= 0.0_r_kind) THEN
    SELECT CASE (mphyopt)
      CASE(1:12,108:110)
        N0g = 4.0E+05_r_kind
      CASE(106,116)
        N0g = 4.0E+06_r_kind
    END SELECT
  END IF

  N0ms = N0s
  N0ms2 = N0s2
  N0mh = N0h
  N0mg = N0g

  IF (alphar <= 0.0_r_kind) THEN
    SELECT CASE (mphyopt)
      CASE(1:12,106,108:110)
        alphar = 0.0_r_kind
    END SELECT
  END IF

  IF (alphas <= 0.0_r_kind) THEN
    alphas = 0.0_r_kind
  END IF

  SELECT CASE (mphyopt)
    CASE(1:12,106,109,110,116)
      alphas2 = 0.0_r_kind
    CASE(108)
      alphas2 = 0.6357_r_kind
  END SELECT

  IF (alphah <= 0.0_r_kind) THEN
    alphah = 0.0_r_kind
  END IF

  IF (alphag <= 0.0_r_kind) THEN
     alphag = 0.0_r_kind
  END IF

  lamdar = 0.0_r_kind
  lamdas = 0.0_r_kind
  lamdas2 = 0.0_r_kind
  lamdams = 0.0_r_kind
  lamdams2 = 0.0_r_kind
  lamdag = 0.0_r_kind
  lamdamg = 0.0_r_kind
  lamdah = 0.0_r_kind
  lamdamh = 0.0_r_kind

  RETURN
END SUBROUTINE set_dsd_para

!########################################################################
!########################################################################
!#########                                                      #########
!#########               SUBROUTINE rdr_obs                     #########
!#########                                                      #########
!#########                     Developed by                     #########
!#########     Center for Analysis and Prediction of Storms     #########
!#########                University of Oklahoma                #########
!#########                                                      #########
!########################################################################
!########################################################################

SUBROUTINE rdr_obs (rho,qscalar,obs_dual,var_dsd,    &
                       var_idx,dualpol)

!-----------------------------------------------------------------------
!
! PURPOSE:
!
! A shell subroutine to assign DSD parameters for the simulated
! radar parameters using parameterized formula based on Jung et al.(2008a).
!
!-----------------------------------------------------------------------
!
! AUTHOR:  Youngsun Jung, 12/14/2010
!
! MODIFICATION HISTORY:
!
!  Bryan Putnam 4/16/2013: Added in information for all radar parameters and
!  all operators, replaces rdr_obs_SM.
!
!-----------------------------------------------------------------------
! Include global variables only for dual-pol calculations
!-----------------------------------------------------------------------

  USE radarz_cst, only: mphyopt, nscalar
  USE radarz_cst, only: P_qr, P_qs, P_qg, P_qh, P_nr

!-----------------------------------------------------------------------
! Force explicit declarations.
!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare arguments.
!-----------------------------------------------------------------------
  REAL(r_kind), INTENT(IN   ) :: qscalar(nscalar)
  REAL(r_kind), INTENT(IN   ) :: rho

  INTEGER(i_kind), INTENT(IN   ) :: var_idx,dualpol

  TYPE(T_obs_dual), INTENT(INOUT) :: obs_dual
  TYPE(T_para_dsd), INTENT(INOUT) :: var_dsd

 !local variables
  REAL(r_kind) :: no_value = missing

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  SELECT CASE (mphyopt)
  CASE(2:8,106)  ! single moment schemes 
    SELECT CASE (qgh_opt)
      CASE (1)                       ! graupel off, hail off
        var_dsd = assign_para_dsd_TM(qscalar(P_qr),qscalar(P_qs),no_value, &
              no_value, no_value, no_value, no_value, no_value, alphar,    &
              alphas,no_value,no_value)
      CASE (2)                       ! graupel off, hail on
        var_dsd = assign_para_dsd_TM(qscalar(P_qr),qscalar(P_qs),        &
                   qscalar(P_qh),no_value,no_value,no_value,no_value,  &
                   no_value,alphar,alphas,alphah,no_value)
      CASE (3)                       ! graupel on, hail off
        var_dsd = assign_para_dsd_TM(qscalar(P_qr),qscalar(P_qs),no_value, &
                   qscalar(P_qg),no_value,no_value,no_value,no_value,    &
                   alphar,alphas,no_value,alphag)
      CASE (4)                       ! graupel on, hail on
        var_dsd = assign_para_dsd_TM(qscalar(P_qr),qscalar(P_qs),        &
                  qscalar(P_qh),qscalar(P_qg),no_value,no_value,        &
                  no_value,no_value,alphar,alphas,alphah,alphag)
    END SELECT
  CASE(108,116) ! double moment for rain only
    SELECT CASE (qgh_opt)
      CASE(1)
        var_dsd = assign_para_dsd_TM(qscalar(P_qr),qscalar(P_qs),        &
                   no_value,no_value,qscalar(P_nr),no_value,no_value,    &
                   no_value,alphar,alphas,no_value,no_value)
      CASE(2)
        var_dsd = assign_para_dsd_TM(qscalar(P_qr),qscalar(P_qs),        &
                    qscalar(P_qh),no_value,qscalar(P_nr),no_value,     &
                    no_value,no_value,alphar,alphas,         &
                    alphah,no_value)
      CASE(3)
        var_dsd = assign_para_dsd_TM(qscalar(P_qr),qscalar(P_qs),        &
                   no_value,qscalar(P_qg),qscalar(P_nr),no_value,      &
                   no_value,no_value,alphar,alphas,no_value,  &
                   alphag)
      CASE(4)
       var_dsd = assign_para_dsd_TM(qscalar(P_qr),qscalar(P_qs),         &
                   qscalar(P_qh),qscalar(P_qg),qscalar(P_nr),        &
                   no_value,no_value,no_value,alphar,alphas,  &
                   alphah,alphag)
    END SELECT
  END SELECT

  dualpol_opt = dualpol
  IF(dualpol == 1) THEN
     IF(var_idx <= 3) THEN
        obs_dual = calculate_obs(rho,var_dsd,var_idx)
     END IF
   END IF

  RETURN

END SUBROUTINE rdr_obs

END MODULE RADARZ_MODULE
