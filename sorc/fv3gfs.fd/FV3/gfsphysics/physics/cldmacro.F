      module cldmacro
!=======================================================================
!      Anning Cheng 2/18/2016 replaced GEO condensation scheme
!      with those from 2M microphysics
       use wv_saturation, only:
     &     epsqs,ttrice,hlatv,hlatf,pcf,rgasv
!    &     ,vqsatd2_water_single,
!    &     vqsatd2_ice_single,vqsatd2_single
       use funcphys,      only : fpvs, fpvsl, fpvsi
!      use GEOS_UtilsMod, only:QSAT=>GEOS_Qsat, DQSAT=>GEOS_DQsat,
!    &                   QSATLQ=>GEOS_QsatLQU, QSATIC=>GEOS_QsatICE
#ifdef GEOS5
       use MAPL_ConstantsMod, only: MAPL_TICE , MAPL_CP , MAPL_GRAV ,
     & MAPL_ALHS , MAPL_ALHL , MAPL_ALHF , MAPL_RGAS , MAPL_H2OMW,
     & MAPL_AIRMW, MAPL_RVAP , MAPL_PI , MAPL_R8 , MAPL_R4
       use MAPL_BaseMod, only: MAPL_UNDEF
#endif
#ifdef NEMS_GSM
      use physcons, MAPL_TICE => con_t0c,  MAPL_GRAV => con_g,
     &              MAPL_CP   => con_cp,   MAPL_ALHL => con_hvap,
     &              MAPL_ALHF => con_hfus, MAPL_PI   => con_pi,
     &              MAPL_RGAS => con_rd,   MAPL_RVAP => con_rv
#endif


       implicit none


!      save
       private

       PUBLIC MACRO_CLOUD
       PUBLIC UPDATE_CLD
       public meltfrz_inst
       public fix_up_clouds_2M
       PUBLIC CLOUD_PTR_STUBS

!! Some parameters set by PHYSPARAMS

       integer :: NSMAX,     DISABLE_RAD, ICEFRPWR,  pdfflag
     &,           FR_LS_WAT, FR_LS_ICE,   FR_AN_WAT, FR_AN_ICE

       real    :: CNV_BETA
       real    :: ANV_BETA
       real    :: LS_BETA
       real    :: RH00
       real    :: C_00
       real    :: LWCRIT
       real    :: C_ACC
       real    :: C_EV_R
       real    :: C_EV_S
       real    :: CLDVOL2FRC
       real    :: RHSUP_ICE
       real    :: SHR_EVAP_FAC
       real    :: MIN_CLD_WATER
       real    :: CLD_EVP_EFF
       real    :: LS_SDQV2
       real    :: LS_SDQV3
       real    :: LS_SDQVT1
       real    :: ANV_SDQV2
       real    :: ANV_SDQV3
       real    :: ANV_SDQVT1
       real    :: ANV_TO_LS
       real    :: N_WARM
       real    :: N_ICE
       real    :: N_ANVIL
       real    :: N_PBL
       real    :: ANV_ICEFALL_C
       real    :: LS_ICEFALL_C
       real    :: REVAP_OFF_P
       real    :: CNVENVFC
       real    :: WRHODEP
       real    :: T_ICE_ALL
       real    :: CNVICEPARAM
       real    :: CNVDDRFC
       real    :: ANVDDRFC
       real    :: LSDDRFC
!      integer :: tanhrhcrit
!      real    :: minrhcrit
!      real    :: maxrhcrit
!      real    :: maxrhcritland
       real    :: turnrhcrit
       real    :: turnrhcrit_upper
       real    :: MIN_RI, MAX_RI, MIN_RL, MAX_RL, RI_ANV


       real, parameter :: T_ICE_MAX = MAPL_TICE
       real, parameter :: RHO_W = 1.0e3
       real, parameter :: MIN_CLD_FRAC = 1.0e-8
       real, parameter :: MAPL_ALHS = MAPL_ALHL+MAPL_ALHF

       real, parameter :: alhlbcp = MAPL_ALHL/MAPL_CP
     &,                   alhfbcp = MAPL_ALHF/MAPL_CP
     &,                   alhsbcp = alhlbcp+alhfbcp


       real, parameter :: PI_0 = 4.*atan(1.)
       real omeps, trinv, t_ice_denom

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       contains


       subroutine macro_cloud(IRUN, LM, DT, PP_dev, PPE_dev
     &,                                   FRLAND_dev, RMFDTR_dev
     &,                       QLWDTR_dev, QRN_CU_dev, CNV_UPDFRC_dev
     &,                       U_dev,      V_dev,      TH_dev, Q_dev
     &,                       QLW_LS_dev, QLW_AN_dev, QIW_LS_dev
     &,                       QIW_AN_dev, ANVFRC_dev, CLDFRC_dev
     &,                       PRECU_dev,  CUARF_dev,  SNRCU_dev
     &,                       PHYSPARAMS, SCLMFDFR,   QST3_dev
     &,                       DZET_dev,   QDDF3_dev,  RHX_dev
     &,                       REV_CN_dev, RSU_CN_dev, ACLL_CN_dev
     &,                       ACIL_CN_dev,PFL_CN_dev, PFI_CN_dev
     &,                       PDFL_dev,   PDFI_dev
     &,                       ALPHT_dev,  CFPDF_dev,  DQRL_dev
     &,                       VFALLSN_CN_dev
     &,                       VFALLRN_CN_dev, CNV_FICE_dev
     &,                       CNV_NDROP_dev,  CNV_NICE_dev, SCICE_dev
     &,                       NCPL_dev,    NCPI_dev,   PFRZ_dev
     &,                                    QRAIN_CN,   QSNOW_CN
     &,                       KCBL, lprnt, ipr, rhc )

       integer, intent(in ) :: IRUN, LM
       real, intent(in )    :: DT
       real, intent(in ),   dimension(IRUN, LM)  :: PP_dev
       real, intent(in ),   dimension(IRUN,0:LM) :: PPE_dev
       real, intent(in ),   dimension(IRUN )     :: FRLAND_dev
       real, intent(in ),   dimension(IRUN, LM)  :: RMFDTR_dev
       real, intent(in ),   dimension(IRUN, LM)  :: QLWDTR_dev
       real, intent(inout), dimension(IRUN, LM)  :: QRN_CU_dev
       real, intent(inout), dimension(IRUN, LM)  :: CNV_UPDFRC_dev
       real, intent(in ),   dimension(IRUN, LM)  :: U_dev
       real, intent(in ),   dimension(IRUN, LM)  :: V_dev
       real, intent(in ),   dimension(IRUN, LM)  :: rhc
       real, intent(inout), dimension(IRUN, LM)  :: TH_dev
       real, intent(inout), dimension(IRUN, LM)  :: Q_dev
       real, intent(inout), dimension(IRUN, LM)  :: QLW_LS_dev
       real, intent(inout), dimension(IRUN, LM)  :: QLW_AN_dev
       real, intent(inout), dimension(IRUN, LM)  :: QIW_LS_dev
       real, intent(inout), dimension(IRUN, LM)  :: QIW_AN_dev
       real, intent(inout), dimension(IRUN, LM)  :: ANVFRC_dev
       real, intent(inout), dimension(IRUN, LM)  :: CLDFRC_dev
       real, intent( out),  dimension(IRUN )     :: PRECU_dev
       real, intent( out),  dimension(IRUN )     :: CUARF_dev
       real, intent( out),  dimension(IRUN )     :: SNRCU_dev
       real, intent(in ),   dimension(58 )       :: PHYSPARAMS
       real, intent(in )                         :: SCLMFDFR
       real, intent(in ),   dimension(IRUN, LM)  :: QST3_dev
       real, intent(in ),   dimension(IRUN, LM)  :: DZET_dev
       real, intent(in ),   dimension(IRUN, LM)  :: QDDF3_dev
       real, intent( out),  dimension(IRUN, LM)  :: RHX_dev
       real, intent( out),  dimension(IRUN, LM)  :: REV_CN_dev
       real, intent( out),  dimension(IRUN, LM)  :: RSU_CN_dev
       real, intent( out),  dimension(IRUN, LM)  :: ACLL_CN_dev
       real, intent( out),  dimension(IRUN, LM)  :: ACIL_CN_dev
       real, intent( out),  dimension(IRUN,0:LM) :: PFL_CN_dev
       real, intent( out),  dimension(IRUN,0:LM) :: PFI_CN_dev
       real, intent( out),  dimension(IRUN, LM)  :: PDFL_dev
       real, intent( out),  dimension(IRUN, LM)  :: PDFI_dev
       real, intent( out),  dimension(IRUN, LM)  :: ALPHT_dev
       real, intent( out),  dimension(IRUN, LM)  :: CFPDF_dev
       real, intent( out),  dimension(IRUN, LM)  :: DQRL_dev
       real, intent( out),  dimension(IRUN, LM)  :: VFALLSN_CN_dev
       real, intent( out),  dimension(IRUN, LM)  :: VFALLRN_CN_dev
       real, intent(inout), dimension(IRUN, LM)  :: CNV_FICE_dev
       real, intent(inout), dimension(IRUN, LM)  :: CNV_NDROP_dev
       real, intent(inout), dimension(IRUN, LM)  :: CNV_NICE_dev
       real, intent(inout), dimension(IRUN, LM)  :: SCICE_dev
       real, intent(inout), dimension(IRUN, LM)  :: NCPL_dev
       real, intent(inout), dimension(IRUN, LM)  :: NCPI_dev
       real, intent(out),   dimension(IRUN, LM)  :: PFRZ_dev
       real, intent(out),   dimension(IRUN, LM)  :: QRAIN_CN
       real, intent(out),   dimension(IRUN, LM)  :: QSNOW_CN

       real, dimension(IRUN, LM)                 :: FRZ_PP_dev
       integer, intent(in),  dimension(IRUN)     :: KCBL
       logical lprnt
       integer ipr


! GPU The GPUs need to know how big local arrays are during compile-time
!     as the GPUs cannot allocate memory themselves. This command resets
!     this a priori size to LM for the CPU.


       integer :: I , J , K , L

       integer :: FRACTION_REMOVAL

       real :: MASS,  iMASS, TOTFRC, QRN_CU_1D, QSN_CU, QRN_ALL, QSN_ALL
     &,        QTMP1, QTMP2, QTMP3, QTOT, TEMP, RHCRIT, AA3, BB3, ALPHA
     &,        VFALL, VFALLRN, VFALLSN,  TOT_PREC_UPD, AREA_UPD_PRC
     &,        AREA_UPD_PRC_tolayer  
     &,        PRN_CU_above, PSN_CU_above
!    &,        AREA_UPD_PRC_tolayer, U_above,U_below,  V_above,V_below
!    &,        DZET_above,DZET_below, PRN_CU_above, PSN_CU_above
     &,        EVAP_DD_CU_above, SUBL_DD_CU_above
     &,        NIX, TOTAL_WATER, dti, tx1, tend, fqi
!    &,        NIX, TOTAL_WATER, dti, tx1, tend, fqi, psinv, pops

       logical :: use_autoconv_timescale
!
       real,    parameter :: RL_cub = 1.0e-15, RI_cub = 6.4e-14
!

       omeps  = 1. - epsqs
       dti    = 1.0 /dt
       trinv  = 1.0/ttrice

       CNV_BETA      = PHYSPARAMS(1)
       ANV_BETA      = PHYSPARAMS(2)
       LS_BETA       = PHYSPARAMS(3)
       RH00          = PHYSPARAMS(4)
       C_00          = PHYSPARAMS(5)
       LWCRIT        = PHYSPARAMS(6)
       C_ACC         = PHYSPARAMS(7)
       C_EV_R        = PHYSPARAMS(8)
       C_EV_S        = PHYSPARAMS(56)
       CLDVOL2FRC    = PHYSPARAMS(9)
       RHSUP_ICE     = PHYSPARAMS(10)
       SHR_EVAP_FAC  = PHYSPARAMS(11)
       MIN_CLD_WATER = PHYSPARAMS(12)
       CLD_EVP_EFF   = PHYSPARAMS(13)
       NSMAX         = INT( PHYSPARAMS(14) )
       LS_SDQV2      = PHYSPARAMS(15)
       LS_SDQV3      = PHYSPARAMS(16)
       LS_SDQVT1     = PHYSPARAMS(17)
       ANV_SDQV2     = PHYSPARAMS(18)
       ANV_SDQV3     = PHYSPARAMS(19)
       ANV_SDQVT1    = PHYSPARAMS(20)
       ANV_TO_LS     = PHYSPARAMS(21)
       N_WARM        = PHYSPARAMS(22)
       N_ICE         = PHYSPARAMS(23)
       N_ANVIL       = PHYSPARAMS(24)
       N_PBL         = PHYSPARAMS(25)
       DISABLE_RAD   = INT( PHYSPARAMS(26) )
       ANV_ICEFALL_C = PHYSPARAMS(28)
       LS_ICEFALL_C  = PHYSPARAMS(29)
       REVAP_OFF_P   = PHYSPARAMS(30)
       CNVENVFC      = PHYSPARAMS(31)
       WRHODEP       = PHYSPARAMS(32)
       T_ICE_ALL     = PHYSPARAMS(33) + MAPL_TICE
       CNVICEPARAM   = PHYSPARAMS(34)
       ICEFRPWR      = INT( PHYSPARAMS(35) + .001 )
       CNVDDRFC      = PHYSPARAMS(36)
       ANVDDRFC      = PHYSPARAMS(37)
       LSDDRFC       = PHYSPARAMS(38)
!      tanhrhcrit    = INT( PHYSPARAMS(41) )
!      minrhcrit     = PHYSPARAMS(42)
!      maxrhcrit     = PHYSPARAMS(43)
       turnrhcrit    = PHYSPARAMS(45) * 0.001
!      maxrhcritland = PHYSPARAMS(46)
       fr_ls_wat     = INT( PHYSPARAMS(47) )
       fr_ls_ice     = INT( PHYSPARAMS(48) )
       fr_an_wat     = INT( PHYSPARAMS(49) )
       fr_an_ice     = INT( PHYSPARAMS(50) )
       MIN_RL        = PHYSPARAMS(51)
       MIN_RI        = PHYSPARAMS(52)
       MAX_RL        = PHYSPARAMS(53)
       MAX_RI        = PHYSPARAMS(54)
       RI_ANV        = PHYSPARAMS(55)
       pdfflag       = INT(PHYSPARAMS(57))


       turnrhcrit_upper = PHYSPARAMS(58) * 0.001

       use_autoconv_timescale = .false.

       t_ice_denom = 1.0 / (T_ICE_MAX-T_ICE_ALL)

       RUN_LOOP: DO I = 1, IRUN
!      Anning initialization here
         PRN_CU_above     = 0.
         PSN_CU_above     = 0.
         EVAP_DD_CU_above = 0.
         SUBL_DD_CU_above = 0.
!        psinv            = 1.0 / ppe_dev(i,lm)

         K_LOOP: DO K = 1, LM

           if (K == 1) then
             TOT_PREC_UPD = 0.
             AREA_UPD_PRC = 0.
           end if

           if (K == LM ) then
             PRECU_dev(I) = 0.
             SNRCU_dev(I) = 0.
             CUARF_dev(I) = 0.
           end if

           QRN_CU_1D = 0.
           QSN_CU    = 0.
           VFALL     = 0.

           PFL_CN_dev(I,K) = 0.
           PFI_CN_dev(I,K) = 0.

           IF (K == 1) THEN
             PFL_CN_dev(I,0) = 0.
             PFI_CN_dev(I,0) = 0.
           END IF

           RHX_dev(I,K)        = 0.0
           REV_CN_dev(I,K)     = 0.0
           RSU_CN_dev(I,K)     = 0.0
           ACLL_CN_dev(I,K)    = 0.0
           ACIL_CN_dev(I,K)    = 0.0
           PDFL_dev(I,K)       = 0.0
           PDFI_dev(I,K)       = 0.0
           ALPHT_dev(I,K)      = 0.0
           CFPDF_dev(I,K)      = 0.0
           DQRL_dev(I,K)       = 0.0
           VFALLSN_CN_dev(I,K) = 0.0
           VFALLRN_CN_dev(I,K) = 0.0
           VFALLSN             = 0.0
           VFALLRN             = 0.0

!          DNDCNV_dev(I, K) = 0.0
!          DNCCNV_dev(I, K) = 0.0
!          RAS_DT_dev(I, K) = 0.0

           QRAIN_CN(I,K)    = 0.0
           QSNOW_CN(I,K)    = 0.0
           NIX = 0.0

           QRN_CU_1D       = QRN_CU_dev(I,K)

           MASS            = (PPE_dev(I,K) - PPE_dev(I,K-1))
     &                     * (100./MAPL_GRAV)
           iMASS           = 1.0 / MASS
           TEMP            = TH_dev(I,K)
           FRZ_PP_dev(I,K) = 0.00


!  NOT USED??? - Moorthi
!          TOTAL_WATER = (QIW_AN_dev(I,K) + QLW_AN_dev(I,K)
!    &                 +  QIW_LS_dev(I,K) + QLW_LS_dev(I,K))*MASS
!    &                 +  QLWDTR_dev(I,K)*DT


!  update of number concentration due to convective detrainment

           if (TEMP < T_ICE_ALL) then
             fQi = 1.0
           elseif (TEMP > T_ICE_MAX) then
             fQi = 0.0
           else
             fQi = CNV_FICE_dev(I,K)
           end if
           tx1 = (1.0-fQi)*QLWDTR_dev(I,K)
           if (tx1 > 0.0 .and. CNV_NDROP_dev(I,K) <= 0.0) then
             CNV_NDROP_dev(I,K) = tx1 / ( 1.333 * MAPL_PI *RL_cub*997.0)
           end if

           tx1 = fQi*QLWDTR_dev(I,K)
           if (tx1 > 0.0 .and. CNV_NICE_dev(I,K) <= 0.0) then
             CNV_NICE_dev(I,K) = tx1 / ( 1.333 * MAPL_PI *RI_cub*500.0)
           end if

           tx1 = iMASS*DT
           NCPL_dev(I,K) = max(NCPL_dev(I,K)+CNV_NDROP_dev(I,K)*tx1,0.0)
           NCPI_dev(I,K) = max(NCPI_dev(I,K)+CNV_NICE_dev(I,K)*tx1,0.0)

 
           TEND = RMFDTR_dev(I,K)*iMASS * SCLMFDFR
           ANVFRC_dev(I,K)   = min(ANVFRC_dev(I,K) + TEND*DT, 1.0)

!
!          DCNVi_dev(I,K)  = (QIW_AN_dev(I,K) - DCNVi_dev(I,K) ) * DTi
!          DCNVL_dev(I,K)  = (QLW_AN_dev(I,K) - DCNVL_dev(I,K) ) * DTi
!          DNDCNV_dev(I,K) = (NCPL_dev(I,K)   - DNDCNV_dev(I,K)) * DTi
!          DNCCNV_dev(I,K) = (NCPI_dev(I,K)   - DNCCNV_dev(I,K)) * DTi


!          if (k == 1 .or. k == lm) then
!            U_above = 0.0
!            U_below = 0.0
!            V_above = 0.0
!            V_below = 0.0
!            DZET_above = 0.0
!            DZET_below = 0.0
!          else
!            U_above = U_dev(i,k-1)
!            U_below = U_dev(i,k+1)
!            V_above = V_dev(i,k-1)
!            V_below = V_dev(i,k+1)
!            DZET_above = DZET_dev(i,k-1)
!            DZET_below = DZET_dev(i,k+1)
!          end if

!          call pdf_spread (K, LM, U_dev(I,K), U_above, U_below,
!    &                      V_dev(I,K), V_above, V_below,
!    &                      DZET_above, DZET_below, CNV_UPDFRC_dev(I,K),
!    &                      PP_dev(I,K), ALPHA, ALPHT_dev(I,K),
!    &                      FRLAND_dev(I) )
!          pops = PP_dev(I,K) * psinv

!          call pdf_spread (K, LM,  PP_dev(I,K), ALPHA,  ALPHT_dev(I,K),
!          call pdf_spread (K, LM,  pops, ALPHA,  ALPHT_dev(I,K),
!    &                      FRLAND_dev(I), rhc(i) )

           ALPHA = max(1.0e-4, 1.0-rhc(i,k))
           ALPHT_dev(I,K) = ALPHA

           RHCRIT = 1.0 - ALPHA

!================================


             call Pfreezing (ALPHA , PP_dev(I,K) , TEMP , Q_dev(I,K),
     &                       QLW_LS_dev(I,K),  QLW_AN_dev(I,K),
     &                       QIW_LS_dev(I,K),  QIW_AN_dev(I,K),
     &                       SCICE_dev(I,K) ,  CLDFRC_dev(I,K),
     &                       ANVFRC_dev(I,K),  PFRZ_dev(I,K) )


!=============Collect convective precip==============

!*********************** begin of if(false)********************************
           if(.false.) then
             QTMP1   = 0.
             QTMP2   = 0.
             QTMP3   = 0.
             QRN_ALL = 0.
             QSN_ALL = 0.

             if ( TEMP < MAPL_TICE ) then
!              QTMP2     = QRN_CU_1D
               QSN_CU    = QRN_CU_1D
               QRN_CU_1D = 0.
               TEMP      = TEMP + QSN_CU * ALHFbCP
             end if

             AREA_UPD_PRC_tolayer = 0.0


             TOT_PREC_UPD = TOT_PREC_UPD + ((QRN_CU_1D + QSN_CU) * MASS)
             AREA_UPD_PRC = AREA_UPD_PRC + (CNV_UPDFRC_dev(I,K)*
     &                                     (QRN_CU_1D + QSN_CU )* MASS)

             if ( TOT_PREC_UPD > 0.0 ) AREA_UPD_PRC_tolayer =
     &                           MAX( AREA_UPD_PRC/TOT_PREC_UPD, 1.E-6 )

             AREA_UPD_PRC_tolayer = CNV_BETA * AREA_UPD_PRC_tolayer

             IF (K == LM) THEN
               if (TOT_PREC_UPD > 0.0) AREA_UPD_PRC = MAX( AREA_UPD_PRC/
     &                                             TOT_PREC_UPD, 1.E-6 )
               AREA_UPD_PRC = CNV_BETA * AREA_UPD_PRC
               CUARF_dev(I) = MIN( AREA_UPD_PRC, 1.0 )
             END IF


             CALL MICRO_AA_BB_3 (TEMP,PP_dev(I,K),QST3_dev(I,K),AA3,BB3)


             QTMP1 = QLW_LS_dev(I,K) + QLW_AN_dev(I,K)
             QTMP2 = QIW_LS_dev(I,K) + QIW_AN_dev(I,K)
             QTOT  = QTMP1 + QTMP2
  
             call PRECIP3 (K, LM, DT, FRLAND_dev(I), RHCRIT, QRN_CU_1D,
     &                     QSN_CU, QTMP1, QTMP2, TEMP, Q_dev(I,K), mass,
     &                     imass, PP_dev(I,K), DZET_dev(I,K),
     &                     QDDF3_dev(I,K), AA3,BB3,AREA_UPD_PRC_tolayer,
     &                     PRECU_dev(I),     SNRCU_dev(I), PRN_CU_above,
     &                     PSN_CU_above,     EVAP_DD_CU_above,
     &                     SUBL_DD_CU_above, REV_CN_dev(I,K),
     &                     RSU_CN_dev(I,K),  ACLL_CN_dev(I,K),
     &                     ACIL_CN_dev(I,K), PFL_CN_dev(I,K),
     &                     PFI_CN_dev(I,K),  VFALLRN,  VFALLSN,
     &                     FRZ_PP_dev(I,K),  CNVENVFC, CNVDDRFC,
     &                     ANVFRC_dev(I,k),  CLDFRC_dev(I,k),
     &                     PP_dev(I,KCBL(I)),i)

             VFALLSN_CN_dev(I,K) = VFALLSN
             VFALLRN_CN_dev(I,K) = VFALLRN

             if (.not. use_autoconv_timescale) then
               if (VFALLSN .NE. 0.) then
                 QSN_ALL = QSN_ALL + PFI_CN_dev(I,K)/VFALLSN
               end if
               if (VFALLRN .NE. 0.) then
                 QRN_ALL = QRN_ALL + PFL_CN_dev(I,K)/VFALLRN
               end if
             end if

!            if (.true.) then

               tx1 = QLW_LS_dev(I,K) + QLW_AN_dev(I,K)
               IF (tx1 > 1.e-20 ) THEN
                 QTMP3 = 1.0 / tx1
               ELSE
                 QTMP3 = 0.0
               END IF
               tx1 = QTMP1 * QTMP3
               QLW_LS_dev(I,K) = QLW_LS_dev(I,K) * tx1
               QLW_AN_dev(I,K) = QLW_AN_dev(I,K) * tx1
               NCPL_dev(I, K)  = NCPL_dev(I,K)   * tx1

               tx1 = QIW_LS_dev(I,K) + QIW_AN_dev(I,K)
               IF (tx1 > 1.0e-20 ) THEN
                 QTMP3 = 1.0 / tx1
               ELSE
                 QTMP3 = 0.0
               END IF
               tx1 = QTMP2 * QTMP3
               QIW_LS_dev(I,K) = QIW_LS_dev(I,K) * tx1
               QIW_AN_dev(I,K) = QIW_AN_dev(I,K) * tx1
               NCPI_dev(I, K)  = NCPI_dev(I,K)   * tx1


               QTMP3 = QIW_LS_dev(I,K) + QIW_AN_dev(I,K)
     &               + QLW_LS_dev(I,K) + QLW_AN_dev(I,K)

               If (QTOT > 0.0) then
                 tx1 = QTMP3/QTOT
                 CLDFRC_dev(I,k) = CLDFRC_dev(I,k)*tx1
                 ANVFRC_dev(I,k) = ANVFRC_dev(I,k)*tx1
               end if

!            end if


             tx1 = (MAPL_RGAS*0.01) * temp / PP_dev(I,K)

             QRAIN_CN(I,K)   = QRN_ALL * tx1
             QSNOW_CN(I,K)   = QSN_ALL * tx1
             QRN_CU_dev(I,K) = QRN_CU_1D

             TOTFRC = CLDFRC_dev(I,K) + ANVFRC_dev(I,K)

             IF ( TOTFRC > 1.00 ) THEN
               tx1 = 1.0 / TOTFRC
               CLDFRC_dev(I,k) = CLDFRC_dev(I,k) * tx1
               ANVFRC_dev(I,k) = ANVFRC_dev(I,k) * tx1
             END IF

             TOTFRC = CLDFRC_dev(I,K) + ANVFRC_dev(I,K)

           end if
!*********************** end of if(false)********************************
      if (lprnt .and. i== ipr) write(0,*)'in macrocld1 clffrc=',
     & CLDFRC_dev(I,K) ,' k=',k

           CALL fix_up_clouds_2M( Q_dev(I,K) , TEMP , QLW_LS_dev(I,K),
     &              QIW_LS_dev(I,K), CLDFRC_dev(I,K), QLW_AN_dev(I,K),
     &              QIW_AN_dev(I,K), ANVFRC_dev(I,K), NCPL_dev(I, K),
     &              NCPI_dev(I, K))

      if (lprnt .and. i== ipr) write(0,*)'in macrocld1 clffrc=',
     &    CLDFRC_dev(I,K) , ' k=',k

           TH_dev(I,K) = TEMP

         end do K_LOOP


       end do RUN_LOOP

       END SUBROUTINE MACRO_CLOUD


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              P R O C E S S   S U B R O U T I N E S                 !!
!!                         * * * * *                                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              P R O C E S S   S U B R O U T I N E S                 !!
!!                         * * * * *                                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!                                                                    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              P R O C E S S   S U B R O U T I N E S                 !!
!!                         * * * * *                                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine pdf_spread (K, LM, PP, ALPHA, ALPHT_DIAG, FRLAND, rhc)

       integer, intent(in)  :: k,lm
       real,    intent(in)  :: PP, FRLAND, rhc

       real,    intent(out) :: ALPHA, ALPHT_DIAG

!      real, parameter      :: slope = 20.0, slope_up = 20.0
       real, parameter      :: slope = 0.02, slope_up = 0.02

       real                 :: aux1, aux2, maxalpha

!      maxalpha = 1.0 - minrhcrit
       maxalpha = 1.0 - rhc

       aux1 = min(max((pp - turnrhcrit)/slope, -20.0), 20.0)
       aux2 = min(max((turnrhcrit_upper - pp)/slope_up, -20.0), 20.0)

       if (frland > 0.05) then
!        aux1 = 1.0
         aux1 = 1.0 / (1.0+exp(aux1+aux1))
       else
         aux1 = 2.0 / (1.0+exp(aux1+aux1))
       end if

       aux2  = 1.0 / (1.0+exp(aux2))

       alpha = max(1.0e-4, min(0.3, maxalpha*aux1*aux2))
!      alpha = min(0.3, maxalpha*aux1*aux2)    !Anning

       ALPHT_DIAG = ALPHA

       end subroutine pdf_spread

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine fix_up_clouds_2M( QV, TE, QLC, QIC, CF, QLA, QIA, AF,
     &                              NL, NI )

       real, intent(inout) :: TE,QV,QLC,CF,QLA,AF,QIC,QIA, NL, NI

       real, parameter     :: qmin = 1.0e-11, cfmin = 1.0e-4
     &,                       nmin = 1.0e-3,  RL_cub = 1.0e-15
     &,                       RI_cub = 6.4e-14

!      if(.false.) then
!      if (AF < cfmin) then
!        QV = QV + QLA + QIA
!        TE = TE - ALHLbCP*QLA - ALHSbCP*QIA
!        AF = 0.
!        QLA = 0.
!        QIA = 0.


!        if ( CF < cfmin) then
!          QV = QV + QLC + QIC
!          TE = TE - ALHLbCP*QLC - ALHSbCP*QIC
!          CF = 0.
!          QLC = 0.
!          QIC = 0.
!        end if
!      end if
!      end if

!      Anning make some changes here
!      if (AF<cfmin.and.QLA+QIA>qmin) AF=cfmin
!      if(CF<cfmin.and.QLC+QIC>qmin) CF=cfmin


       if (QLC < qmin .and. QLC > 0.) then
         QV  = QV + QLC
         TE  = TE - ALHLbCP*QLC
         QLC = 0.
       end if

       if (QIC < qmin .and. QIC > 0.) then
         QV  = QV + QIC
         TE  = TE - ALHSbCP*QIC
         QIC = 0.
       end if


       if (QLA < qmin .and. QLA > 0.) then
         QV  = QV + QLA
         TE  = TE - ALHLbCP*QLA
         QLA = 0.
       end if

       if (QIA < qmin .and. QIA > 0.) then
         QV  = QV + QIA
         TE  = TE - ALHSbCP*QIA
         QIA = 0.
       end if


       if (QLA+QIA < qmin .and.  QLA+QIA > 0.) then
         QV  = QV + QLA + QIA
         TE  = TE - ALHLbCP*QLA - ALHSbCP*QIA
         AF  = 0.
         QLA = 0.
         QIA = 0.
       end if

       if (QLC+QIC < qmin .and. QLC+QIC > 0. ) then
         QV  = QV + QLC + QIC
         TE  = TE - ALHLbCP*QLC - ALHSbCP*QIC
         CF  = 0.
         QLC = 0.
         QIC = 0.
       end if

       if ((QLA+QLC) <= qmin) then
         NL = 0.0
       end if

       if ((QIA+QIC) <= qmin) then
         NI = 0.0
       end if

! make sure N > 0 if Q >0
       if (QLA+QLC > qmin .and. NL <= nmin) then
         NL = max((QLA+QLC)/( 1.333 * MAPL_PI *RL_cub*997.0), nmin)
       end if

       if (QIA+QIC > qmin .and. NI <= nmin) then
         NI = max((QIA+QIC)/( 1.333 * MAPL_PI *RI_cub*500.0), nmin)
       end if


       end subroutine fix_up_clouds_2M



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine update_cld( irun, lm, DT, ALPHA,
     &                        PDFSHAPE, PL, QV, QCl, QAl,
     &                        QCi, QAi, TE, CF, AF,
     &                        SCICE, NI, NL)
!    &                        SCICE, NI, NL, NCnuc)

       integer, intent(in) :: irun, lm, pdfshape
       real,    intent(in) :: DT
       real,    intent(in),    dimension(irun,lm) :: ALPHA, PL
!      real,    intent(in),    dimension(irun,lm) :: ALPHA, PL, NCnuc
       real,    intent(inout), dimension(irun,lm) :: te, qv, qcl, qci
     &,                        CF, QAl, QAi, AF, NI, NL, SCICE

       real :: CFO, pl100, QT, DQ, QSx, DQsx, QCx, QC, QA
     &,        QX, QSLIQ, QSICE, CFALL, DQx, FQA

       real :: esl, esi, esn !temp use only Anning

       integer :: i,k

       do k=1,lm
         do i=1,irun
           if(QV(i,k) > 1.e-6) then
              pl100 = pl(i,k)*100
              QC    = QCl(i,k) + QCi(i,k)
              QA    = QAl(i,k) + QAi(i,k)
             !Anning do not let empty cloud exist
              if(QC <= 0.) CF(i,k) = 0. 
              if(QA <= 0.) AF(i,k) = 0. 
              QT    = QC + QA  + QV(i,k)
              CFALL = AF(i,k)  + CF(i,k)

              if (QA+QC > 0.0) then
                FQA = QA / (QA+QC)
              else
                FQA   = 0.0
              end if
!================================================
! First find the cloud fraction that would correspond to the current
! condensate
!      QSLIQ = QSATLQ( TE , PL*100.0 , DQ=DQx )
!      QSICE = QSATIC( TE , PL*100.0 , DQ=DQx )
!           call vqsatd2_water_single(TE(i,k),PL(i,k)*100.0,
!    &                                esl,QSLIQ,DQx)
!           call vqsatd2_ice_single(TE(i,k),PL(i,k)*100.0,
!    &                              esi,QSICE,DQx)
             esl   = min(fpvsl(TE(i,k)),pl100)
             QSLIQ = min(epsqs*esl/(pl100-omeps*esl),1.)
             esi   = min(fpvsi(TE(i,k)),pl100)
             QSICE = min(epsqs*esi/(pl100-omeps*esi),1.)

             if ((QC+QA) > 0.0) then
               QSx = ( (QCl(i,k)+QAl(i,k))*QSLIQ
     &               + (QCi(i,k)+QAi(i,k))*QSICE ) / (QC+QA)
             else
!              DQSx = DQSAT( TEo , PL , 35.0, QSAT=QSx )
!             call vqsatd2_single( TE(i,k), pl(i,k)*100., esl,QSx,DQSx)
               esn = min(fpvs(TE(i,k)),pl100)
               QSx = min(epsqs*esn/(pl100-omeps*esn),1.)

             end if

!            if (TE(i,k) > T_ICE_ALL) SCICE(i,k) = 1.0

             QCx = QC + QA
             QX  = QT - QSx*SCICE(i,k)
             CFo = 0.
!      recalculate QX if too low and SCICE<SHOM

             if ((QX <= QCx) .and. (QCx > 0.0)) then
               CFo = (1.0+SQRT(1.0-(QX/QCx)))
               if (CFo > 1.e-6) then
                 CFo = min(1.0/CFo, 1.0)
                 DQ  = 2.0*QCx/(CFo*CFo)
               else
                 CFo = 0.0
               end if
             else
               if (QCx > 0.0) then
                 CFo = 1.0
               end if
               DQ = 2.0*ALPHA(i,k)*QSx
             end if

             CFALL = max(CFo, 0.0)
             CFALL = min(CFo, 1.0)

             CF(i,k) = CFALL*(1.0-FQA)
             AF(i,k) = CFALL*FQA


!            if ((TE(i,k) <= T_ICE_ALL)) cycle


             call hystpdf( DT, ALPHA(i,k), PDFSHAPE, PL(i,k), QV(i,k)
     &,                    QCl(i,k),   QAl(i,k), QCi(i,k)
     &,                    QAi(i,k),   TE(i,k),  CF(i,k), AF(i,k)
     &,                    SCICE(i,k), NI(i,k),  NL(i,k), i, k )

            !Anning do not let empty cloud exist
             if(QCl(i,k)+QCi(i,k) <= 0.0) CF(i,k) = 0. 
             if(QAl(i,k)+QAi(i,k) <= 0.0) AF(i,k) = 0. 
           end if
         enddo
       enddo


       end subroutine update_cld

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine hystpdf( DT, ALPHA, PDFSHAPE, PL, QV, QCl, QAl,
     &                     QCi, QAi, TE, CF, AF, SCICE, NI, NL, i, k)

       real, intent(in)    :: DT,ALPHA,PL
       integer, intent(in) :: pdfshape
       real, intent(inout) :: TE,QV,QCl,QCi,CF,QAl,QAi,AF, NI, NL,
     &                        SCICE

       integer, parameter :: nmax=20

       real :: QCO, QVO, CFO, QAO, TAU
       real :: QT, QMX, QMN, DQ, QVtop, sigmaqt1, sigmaqt2, qsnx

       real :: TEO, QSx, DQsx, QS,  DQs
     &,        TEp, QSp, CFp,  QVp, QCp
     &,        TEn, QSn, CFn,  QVn, QCn

       real :: QCx, QVx, CFx, QAx, QC, QA, fQi, fQi_A
     &,        dQAi, dQAl, dQCi, dQCl

       real :: QX, QSLIQ, QSICE, CFALL, DQx, FQA, pl100, tmpARR
     &,        ALHX, DQCALL, esn, desdt, tc, hltalt, tterm

       integer :: N, i, k

       pdfflag = PDFSHAPE
       pl100 = pl*100

       QC     = QCl + QCi
       QA     = QAl + QAi
       QT     = QC  + QA + QV
       CFALL  = AF  + CF
       FQA    = 0.0
       fQi    = 0.0
       tmpARR = 0.0
       QAx    = 0.0

       if (QA+QC > 0.0)       FQA    = QA / (QA+QC)
       if (QA    > 0.0)       fQi_A  = QAi / QA
       if (QT    > 0.0)       fQi    = (QCI+QAI) / QT
       if (TE    < T_ICE_ALL) fQi    = 1.0
       if ( AF   < 1.0 )      tmpARR = 1.0 / (1.0-AF)

       TEo = TE

!
       fqi = 1.0 - max(0.0, min(1.0, (te-t_ice_all)*t_ice_denom))
       fqi = (max(0.0,min(1.0,fqi))) ** ICEFRPWR

!      fQi = ice_fraction( TE  )
!      fQi = ice_fraction( TEn )
!      DQS = DQSAT( TE, PL, QSAT=QSx ) Anning changed to the foollowing
!      DQSx = DQSAT( TE, PL, QSAT=QSx )
!      call vqsatd2_single( TE, pl*100., esn,QSx,DQSx)

       esn    = min(fpvs(TE),pl100)
       QSx    = min(epsqs*esn/(pl100-omeps*esn),1.)

       if (qsx < 1.0) then
         tc     = TE - MAPL_TICE
         if (TE < MAPL_TICE) then
           hltalt = hlatv + hlatf * min(-tc*trinv,1.0)
         else
           hltalt = hlatv - 2369.0*tc
         end if
         if (tc >= -ttrice .and. tc < 0.0) then
           tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3)
     &                    + tc*(pcf(4) + tc*pcf(5))))
         else
           tterm = 0.0
         end if
         desdt = hltalt*esn/(rgasv*TE*TE) + tterm*trinv
         dqsx  = qsx*pl100*desdt/(esn*(pl100-omeps*esn))
       else
         DQSx  = 0.0
       endif

       CFx = CF*tmpARR
       QCx = QC*tmpARR
       QVx = ( QV - QSx*AF )*tmpARR

!      if ( AF >= 1.0 ) QVx = QSx*1.e-4
       if ( AF > 0.0 )   QAx = QA/AF

       QT  = QCx + QVx

       TEp = TEo
       QSn = QSx
       TEn = TEo
       CFn = CFx
       QVn = QVx
       QCn = QCx
       DQS = DQSx

       do n=1,nmax

         QVp = QVn
         QCp = QCn
         CFp = CFn
         TEp = TEn
!        fQip= fQi

         if(pdfflag < 2) then
           sigmaqt1 = ALPHA*QSn
           sigmaqt2 = ALPHA*QSn
         elseif(pdfflag == 2) then
           sigmaqt1 = ALPHA*QSn
           sigmaqt2 = ALPHA*QSn
         elseif(pdfflag == 4) then
           sigmaqt1 = max(ALPHA/sqrt(3.0), 0.001)
         endif

         qsnx = qsn*SCICE
         if (QCI >= 0.0 .and. qsn > qt) qsnx = qsn

         call pdffrac(PDFSHAPE,qt,sigmaqt1,sigmaqt2,qsnx,CFn)
         call pdfcondensate(PDFSHAPE,qt,sigmaqt1,sigmaqt2,qsnx,QCn,CFn)

         DQCALL = QCn - QCp
         CF = CFn * ( 1.0-AF)

!        call Bergeron_iter (DT, PL, TEp, QT, QCi, QAi, QCl, QAl,
!    &                       CF, AF, NL, NI, DQCALL, fQi)

         if ( AF > 0. ) then
           QAo = QAx
         else
           QAo = 0.
         end if


         ALHX = (1.0-fQi)*alhlbcp + fQi*alhsbcp

         if(pdfflag == 1) then
           QCn = QCp + (QCn- QCp)
     &               / (1.0 - (CFn*(ALPHA-1.0) - QCn/QSn) *DQS*ALHX)
         elseif(pdfflag == 2) then
           if (n < nmax) QCn = QCp + ( QCn - QCp ) * 0.5
         endif

         QVn = QVp - (QCn - QCp)
         TEn = TEp + ALHX * ((QCn-QCp)*(1.0-AF) + (QAo-QAx)*AF)

!        fqi = 1.0 - max(0.0, min(1.0, (ten-t_ice_all)*t_ice_denom))
!        fqi = (max(0.0,min(1.0,fqi))) ** ICEFRPWR

         if (abs(Ten-Tep) < 0.00001) exit

!        DQS = DQSAT( TEn, PL, QSAT=QSn )
!        call vqsatd2_single( TEn, pl*100., esn,QSn,DQS)
         esn    = min(fpvs(TEn),pl100)
         QSn    = min(epsqs*esn/(pl100-omeps*esn),1.0)

         if (qsx < 1.0) then
           tc     = TEn - MAPL_TICE
           if (TEn < MAPL_TICE) then
             hltalt = hlatv + hlatf * min(-tc*trinv,1.0)
           else
             hltalt = hlatv - 2369.0*tc
           end if
           if (tc >= -ttrice .and. tc < 0.0) then
             tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3)
     &                      + tc*(pcf(4) + tc*pcf(5))))
           else
             tterm = 0.0
           end if
           desdt = hltalt*esn/(rgasv*TEn*TEn) + tterm*trinv
           dqs   = QSn*pl100*desdt/(esn*(pl100-omeps*esn))
         else
           DQS = 0.0
         endif

       enddo

       CFo = CFn
       CF  = CFn
       QCo = QCn
!      QVo = QVn
!      TEo = TEn
!      TE  = TEn

       if ( AF < 1.0 ) then
         CF  = CFo * ( 1.0-AF)
         QCo = QCo * ( 1.0-AF)
         QAo = QAo * AF
       else
         CF  = 0.0
         QAo = QA + QC
         QCo = 0.0
         QT  = QAo + QV
         QAo = MAX(QT-QSx, 0.0)
       end if

       dQCl = 0.0
       dQCi = 0.0
       dQAl = 0.0
       dQAi = 0.0

!large scale  QCx is not in envi 

       QCx = QCo - QC
!      Anning Cheng prevented unstable here
       if (QCx < -1.e-3) QCx = -1.e-3
       if (QCx < 0.0) then
         dQCl = max(QCx, -QCl)
         dQCi = max(QCx-dQCl, -QCi)
       else
         dQCi = QCx * fQi
         dQCl = QCx - dQCi
       end if

!Anvil QAx is not in anvil 
       QAx = QAo - QA
!      Anning Cheng prevented unstable here
       if(QAx < -1.e-3) QAx = -1.e-3

       if (QAx < 0.0) then
         dQAl = max(QAx, -QAl)
         dQAi = max(QAx-dQAl, -QAi)
       else
         dQAi = QAx * fQi
         dQAl = QAx - dQAi
       end if

!     if(.false.) then !Anning turn it off causing unstable
!     if ( AF < 1.e-5 ) then
!      dQAi = -QAi
!      dQAl = -QAl
!     end if
!     if ( CF < 1.e-5 ) then
!      dQCi = -QCi
!      dQCl = -QCl
!     end if
!     end if

      QAi = QAi + dQAi
      QAl = QAl + dQAl
      QCi = QCi + dQCi
      QCl = QCl + dQCl
      QV  = QV - ( dQAi+dQCi+dQAl+dQCl)


      TE = TE + (alhlbcp * (dQAi+dQCi+dQAl+dQCl)
     &        +  alhfbcp * (dQAi+dQCi))



       if ( QAo <= 0. ) then
         QV  = QV + QAi + QAl
         TE  = TE - alhsbcp*QAi - alhlbcp*QAl
         QAi = 0.
         QAl = 0.
         AF  = 0.
       end if

       CALL fix_up_clouds_2M(QV, TE, QCl, QCi, CF, QAl, QAi, AF, NL, NI)

       end subroutine hystpdf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine pdffrac (flag,qtmean,sigmaqt1,sigmaqt2,qstar,clfrac)
       implicit none

       integer flag

       real :: qtmean, sigmaqt1, sigmaqt2, qstar, clfrac

       real :: qtmode, qtmin, qtmax, qtmedian, aux

       if(flag == 1) then
         aux = qtmean + sigmaqt1 - qstar
         if (aux < 0.0) then
           clfrac = 0.
         else
           if(sigmaqt1 > 0.0) then
             clfrac = min(0.5*aux/sigmaqt1, 1.0)
           else
             clfrac = 1.
           endif
         endif
       elseif(flag == 2) then
         qtmode = qtmean + (sigmaqt1-sigmaqt2)/3.
         qtmin  = min(qtmode-sigmaqt1,0.)
         qtmax  = qtmode + sigmaqt2
         if(qtmax < qstar) then
           clfrac = 0.
         elseif ( (qtmode <= qstar).and.(qstar < qtmax) ) then
           clfrac = (qtmax-qstar)*(qtmax-qstar) / 
     &            ((qtmax-qtmin)*(qtmax-qtmode))
         elseif ( (qtmin <= qstar).and.(qstar < qtmode) ) then
           clfrac = 1. - ((qstar-qtmin)*(qstar-qtmin)
     &            /( (qtmax-qtmin)*(qtmode-qtmin)))
         elseif ( qstar <= qtmin ) then
           clfrac = 1.
         endif
       elseif(flag == 4) then
         if (qtmean > 1.0e-20) then
           qtmedian = qtmean*exp(-0.5*sigmaqt1*sigmaqt1)
           aux      = log(qtmedian/qstar)/sqrt(2.0)/sigmaqt1
           aux      = min(max(aux, -20.0), 20.0)
           clfrac   = 0.5*(1.0+erf_app(aux))
         else
           clfrac   = 0.0
         end if
       endif

       return
       end subroutine pdffrac

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


       subroutine pdfcondensate (flag,qtmean4,sigmaqt14,sigmaqt24,
     &                           qstar4,condensate4, clfrac4)
       implicit none

       integer flag

       real qtmean4, sigmaqt14, sigmaqt24, qstar4, condensate4, clfrac4

       real *8 :: qtmode, qtmin, qtmax, constA, constB, cloudf
     &,           term1, term2, term3
     &,           qtmean, sigmaqt1, sigmaqt2, qstar, condensate
     &,           qtmedian, aux, clfrac, tx1

       qtmean   = dble(qtmean4)
       sigmaqt1 = dble(sigmaqt14)
       sigmaqt2 = dble(sigmaqt24)
       qstar    = dble(qstar4)
       clfrac   = dble(clfrac4)

       if(flag == 1) then
         if(qtmean+sigmaqt1 < qstar) then
           condensate = 0.d0
         elseif(qstar > qtmean-sigmaqt1) then
           if(sigmaqt1 > 0.d0) then
             tx1 = min(qtmean+sigmaqt1-qstar, 2.d0*sigmaqt1)
             condensate = tx1*tx1 / (4.d0*sigmaqt1)
           else
             condensate = qtmean - qstar
           endif
         else
           condensate = qtmean - qstar
         endif
       elseif(flag == 2) then
         qtmode = qtmean + (sigmaqt1-sigmaqt2)/3.d0
         qtmin = min(qtmode-sigmaqt1,0.d0)
         qtmax = qtmode + sigmaqt2
         if ( qtmax < qstar ) then
           condensate = 0.d0
         elseif ( qtmode <= qstar .and. qstar < qtmax ) then
           constB = 2.d0 / ( (qtmax - qtmin)*(qtmax-qtmode) )
           cloudf = (qtmax-qstar)*(qtmax-qstar) * 0.5d0 * constB
           term1 = (qstar*qstar*qstar)/3.d0
           term2 = (qtmax*qstar*qstar)/2.d0
           term3 = (qtmax*qtmax*qtmax)/6.d0
           condensate = constB * (term1-term2+term3) - qstar*cloudf
         elseif ( qtmin <= qstar .and. qstar < qtmode ) then
           constA = 2.d0 / ((qtmax-qtmin)*(qtmode-qtmin))
           cloudf = 1.d0 - (qstar-qtmin)*(qstar-qtmin)*0.5d0*constA
           term1 = qstar*qstar*qstar/3.d0
           term2 = qtmin*qstar*qstar/2.d0
           term3 = qtmin*qtmin*qtmin/6.d0
           condensate = qtmean - (constA*(term1-term2+term3))
     &                         - qstar*cloudf
         elseif ( qstar <= qtmin ) then
           condensate = qtmean - qstar
         endif

       elseif(flag == 4) then

         if (qtmean  > 1.0e-20) then
           aux = 0.5*sigmaqt1*sigmaqt1
           qtmedian = qtmean*exp(-aux)
           aux = (aux + log(qtmedian/qstar))/(sqrt(2.0)*sigmaqt1)
           aux = min(max(aux, -20.0), 20.0)
           aux = 1.0 + dble(erf_app(sngl(aux)))
           condensate = (0.5*qtmean*aux/qstar - clfrac) * qstar
           condensate= min(condensate, qtmean)
         else
           condensate = 0.0
         end if

       endif
       condensate4 = real(condensate)

       return
       end subroutine pdfcondensate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine cnvsrc( DT, ICEPARAM, SCLMFDFR, MASS, iMASS, PL,
     &                    TE, QV, DCF, DMF, QLA, QIA, CF, AF, QS,
     &                    NL, NI, CNVFICE, CNVNDROP, CNVNICE)

       real, intent(in)    :: DT, ICEPARAM, SCLMFDFR, MASS, iMASS, QS
     &,                       DMF,PL, DCF, CF
       real, intent(inout) :: TE, AF,QV, QLA, QIA, NI, NL
     &,                       CNVFICE, CNVNDROP, CNVNICE

       real    :: TEND,QVx,QCA,fQi

       integer, parameter :: STRATEGY = 3
       real,    parameter :: RL_cub = 1.0e-15, RI_cub = 6.4e-14
     &,                      minrhx = 0.001

       fQi = 0.0

       if (TE < T_ICE_ALL) then
         fQi = 1.0
       elseif (TE > T_ICE_MAX) then
         fQi = 0.0
       else
         fQi = CNVFICE
       end if


!      TEND = DCF*iMASS

!      QLA = QLA + (1.0-fQi)* TEND*DT
!      QIA = QIA + fQi * TEND*DT


       if ( ( (1.0-fQi)*DCF > 0.0) .and. (CNVNDROP <= 0.0)) then
         CNVNDROP = (1.0-fQi)*DCF/( 1.333 * MAPL_PI *RL_cub*997.0)
       end if

       if ((fQi*DCF > 0.0) .and. (CNVNICE <= 0.0)) then
         CNVNICE = fQi*DCF/( 1.333 * MAPL_PI *RI_cub*500.0)
       end if

       NL = max(NL + CNVNDROP*iMASS*DT, 0.0)
       NI = max(NI + CNVNICE*iMASS*DT, 0.0)

!      TE = TE + (alhsbcp-alhlbcp) * fQi * TEND * DT

!      QCA = QLA + QIA

       TEND = DMF*iMASS * SCLMFDFR
       AF   = min(AF + TEND*DT, 1.0)

       if ( AF < 1.0 ) then
         QVx = ( QV - QS * AF )/(1.-AF)
       else
         QVx = QS
       end if

!      if (STRATEGY == 1) then
!        if ( (( QVx - minrhx*QS ) < 0.0 ) .and. (AF > 0.) ) then
!          AF = (QV - minrhx*QS )/( QS*(1.0-minrhx) )
!        end if
!        if ( AF < 0. ) then
!          AF = 0.
!          QV = QV + QLA + QIA
!          TE = TE - (alhlbcp*QLA + alhsbcp*QIA)
!          QLA = 0.
!          QIA = 0.
!        end if
!      else if (STRATEGY == 2) then
!        if ( (( QVx - minrhx*QS ) < 0.0 ) .and. (AF > 0.) ) then
!          QV  = QV  + (1.-AF)*( minrhx*QS - QVx )
!          QCA = QCA - (1.-AF)*( minrhx*QS - QVx )
!          TE  = TE  - (1.-AF)*( minrhx*QS - QVx )* alhlbcp
!        end if
!      end if

       end subroutine cnvsrc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine PRECIP3( K,LM , DT , FRLAND , RHCR3 , QPl , QPi ,
     & QCl , QCi , TE , QV , mass , imass , PL , dZE , QDDF3 , AA , BB ,
     & AREA , RAIN , SNOW , PFl_above , PFi_above , EVAP_DD_above,
     & SUBL_DD_above, REVAP_DIAG , RSUBL_DIAG , ACRLL_DIAG ,
     & ACRIL_DIAG , PFL_DIAG , PFI_DIAG , VFALLRN , VFALLSN , FRZ_DIAG ,
     & ENVFC,DDRFC, AF, CF, PCBL,i )


       integer, intent(in) :: K,LM,i

       real, intent(in ) :: DT

       real, intent(inout) :: QV,QPl,QPi,QCl,QCi,TE

       real, intent(in ) :: mass,imass
       real, intent(in ) :: PL
       real, intent(in ) :: AA,BB
       real, intent(in ) :: RHCR3
       real, intent(in ) :: dZE
       real, intent(in ) :: QDDF3
       real, intent( out) :: RAIN,SNOW
       real, intent(in ) :: AREA
       real, intent(in ) :: FRLAND

       real, intent(inout) :: PFl_above, PFi_above
       real, intent(inout) :: EVAP_DD_above, SUBL_DD_above

       real, intent( out) :: REVAP_DIAG
       real, intent( out) :: RSUBL_DIAG
       real, intent( out) :: ACRLL_DIAG,ACRIL_DIAG
       real, intent( out) :: PFL_DIAG, PFI_DIAG
       real, intent(inout) :: FRZ_DIAG
       real, intent( out) :: VFALLSN, VFALLRN

       real, intent(in ) :: ENVFC,DDRFC

       real, intent(in ) :: AF,CF, PCBL


       real :: PFi,PFl,QS,dQS,ENVFRAC
       real,save :: TKo,QKo,QSTKo,DQSTKo,RH_BOX,T_ED,QPlKo,QPiKo
       real :: Ifactor,RAINRAT0,SNOWRAT0
       real :: FALLRN,FALLSN,VEsn,VErn,NRAIN,NSNOW,Efactor

       real :: TinLAYERrn,DIAMrn,DROPRAD
       real :: TinLAYERsn,DIAMsn,FLAKRAD,pl100

       real :: EVAP,SUBL,ACCR,MLTFRZ,EVAPx,SUBLx
       real :: EVAP_DD,SUBL_DD,DDFRACT
       real :: LANDSEAF

       real :: tmpARR, CFR, aux

       real, parameter :: TRMV_L = 1.0

       real :: TAU_FRZ, TAU_MLT, QSICE, DQSI

       integer :: NS, NSMX, itr,L

       logical, parameter :: taneff = .true.

       real, parameter :: B_SUB = 1.00
       real esl,esi, esn,desdt,weight,tc,hlatsb,hlatvp,hltalt,tterm,
     &      gam
       logical lflg


       pl100=pl*100
       if(taneff) then
         aux = min(max((pl- PCBL)/10.0, -20.0), 20.0)
         aux = 1.0/(1.0+exp(-aux))
         envfrac = ENVFC + (1.0-ENVFC)*aux
         envfrac = min(envfrac,1.)
       else
         ENVFRAC = ENVFC
       endif

       CFR= AF+CF
       if ( CFR < 0.99) then
         tmpARR = 1./(1.-CFR)
       else
         tmpARR = 0.0
       end if


       IF ( AREA > 0. ) THEN
         Ifactor = 1./ ( AREA )
       ELSE
         Ifactor = 1.00
       END if

       Ifactor = MAX( Ifactor, 1.)

       PFL_DIAG = 0.
       PFI_DIAG = 0.
       ACRIL_DIAG = 0.
       ACRLL_DIAG = 0.
       REVAP_DIAG = 0.
       RSUBL_DIAG = 0.


!      dQS = DQSAT( TE, PL, QSAT = QS )
!      call vqsatd2_single( TE, pl*100., esl,QS,DQS)
       esn=min(fpvs(TE),pl100)
       QS= min(epsqs*esn/(pl100-omeps*esn),1.)
!      TKO is not defined yet here Anning Cheng
       TKO = TE
!      QSICE = QSATIC( min(TKo, T_ICE_MAX), PL*100.0 , DQ=DQSI )
!      call vqsatd2_ice_single(min(TKo, T_ICE_MAX),PL*100.0,
!    &  esl,QSICE,DQSI)
       esi=min(fpvsi(TKo),pl100)
       QSICE= min(epsqs*esi/(pl100-omeps*esi),1.)
       hltalt = hlatv + hlatf
       desdt  = hltalt*esi/(rgasv*TKo*TKo)
       if (QSICE < 1.0) then
         gam = hltalt*QSICE*pl100*desdt/(MAPL_CP*esi
     &       * (pl100 - omeps*esi))
       else
         gam = 0.0
       endif
       DQSI = (MAPL_CP/hltalt)*gam


       DDFRACT = DDRFC

       IF (K == 1) THEN
         PFl = QPl*MASS
         PFi = QPi*MASS

         EVAP_DD = 0.
         SUBL_DD = 0.

         VFALLRN = 0.0
         VFALLSN = 0.0
       ELSE
         QPl = QPl + PFl_above * iMASS
         PFl = 0.00
         QPi = QPi + PFi_above * iMASS
         PFi = 0.00


         ACCR = B_SUB * C_ACC * ( QPl*MASS ) *QCl

         ACCR = MIN( ACCR , QCl )

         QPl = QPl + ACCR
         QCl = QCl - ACCR

         ACRLL_DIAG = ACCR / DT


         ACCR = B_SUB * C_ACC * ( QPi*MASS ) *QCl

         ACCR = MIN( ACCR , QCl )

         QPi = QPi + ACCR
         QCl = QCl - ACCR

        TE = TE + alhfbcp*ACCR

        ACRIL_DIAG = ACCR / DT

        RAINRAT0 = Ifactor*QPl*MASS/DT
        SNOWRAT0 = Ifactor*QPi*MASS/DT
 
        call MARSHPALMQ2(RAINRAT0,PL,DIAMrn,NRAIN,FALLrn,VErn)
        call MARSHPALMQ2(SNOWRAT0,PL,DIAMsn,NSNOW,FALLsn,VEsn)

        IF ( FRLAND < 0.1 ) THEN

        END IF

       VFALLRN = FALLrn
       VFALLSN = FALLsn

       TinLAYERrn = dZE / ( max(FALLrn,0.)+0.01 )
       TinLAYERsn = dZE / ( max(FALLsn,0.)+0.01 )

       TAU_FRZ = 5000.

       MLTFRZ = 0.0
       IF ( (TE > MAPL_TICE ) .and. (TE <= MAPL_TICE+5. ) ) THEN
         MLTFRZ = TinLAYERsn * QPi *( TE - MAPL_TICE ) / TAU_FRZ
         MLTFRZ = MIN( QPi , MLTFRZ )
         TE     = TE - alhfbcp*MLTFRZ
         QPl    = QPl + MLTFRZ
         QPi   = QPi - MLTFRZ
       END IF
       FRZ_DIAG = FRZ_DIAG - MLTFRZ / DT

       MLTFRZ = 0.0
       IF ( TE > MAPL_TICE+5. ) THEN
         MLTFRZ = QPi
         TE     = TE - alhfbcp*MLTFRZ
         QPl    = QPl + MLTFRZ
         QPi    = QPi - MLTFRZ
       END IF
       FRZ_DIAG = FRZ_DIAG - MLTFRZ / DT

       MLTFRZ = 0.0
       if ( K >= LM-1 ) THEN
         IF ( TE > MAPL_TICE+0. ) THEN
           MLTFRZ = QPi
           TE     =  TE - alhfbcp*MLTFRZ
           QPl    = QPl + MLTFRZ
           QPi    = QPi - MLTFRZ
         END IF
       endif
       FRZ_DIAG = FRZ_DIAG - MLTFRZ / DT


       MLTFRZ = 0.0
       IF ( TE <= MAPL_TICE ) THEN
         TE     = TE + alhfbcp*QPl
         QPi    = QPl + QPi
         MLTFRZ = QPl
         QPl    = 0.
       END IF
       FRZ_DIAG = FRZ_DIAG + MLTFRZ / DT


       QKo = QV
       TKo = TE
       QPlKo = QPl
       QPiKo = QPi

       do itr = 1,3

!      DQSTKo = DQSAT ( TKo , PL,QSAT=QSTko )
!      call vqsatd2_single(TKo, pl*100., esl,QSTko,DQSTKo)
       esn=min(fpvs(TKo),pl100)
       QSTko= min(epsqs*esn/(pl100-omeps*esn),1.)
       tc = TKo - MAPL_TICE
       lflg = (tc >= -ttrice .and. tc < 0.)
       weight = min(-tc*trinv,1.0)
       hlatsb = hlatv + weight*hlatf
       hlatvp = hlatv - 2369.0*tc
       if (TKo < MAPL_TICE) then
         hltalt = hlatsb
       else
         hltalt = hlatvp
       end if
       if (lflg) then
         tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3)
     &         +tc*(pcf(4) + tc*pcf(5))))
       else
         tterm = 0.
       end if
       desdt = hltalt*esn/(rgasv*TKo*TKo) + tterm*trinv
       DQSTKo=(epsqs + omeps*QSTko)/(pl100 - omeps*esn)*desdt

!      QSICE = QSATIC( min(TKo, T_ICE_MAX), PL*100.0 , DQ=DQSI )
!        call vqsatd2_ice_single(min(TKo, T_ICE_MAX),PL*100.0,
!    &                           esl,QSICE,DQSI)
         esi=min(fpvsi(TKo),pl100)
         QSICE= min(epsqs*esi/(pl100-omeps*esi),1.)
         hltalt = hlatv + hlatf
         desdt  = hltalt*esi/(rgasv*TKo*TKo)
         if (QSICE < 1.0) then
           gam = hltalt*QSICE*pl100*desdt/(MAPL_CP*esi
     &         * (pl100 - omeps*esi))
         else
           gam = 0.0
         endif
         DQSI = (MAPL_CP/hltalt)*gam

         QSTKo = MAX( QSTKo , 1.0e-7 )
         QSICE = MAX( QSICE , 1.0e-7 )

         if (tmpARR > 0.0) then
           QKo =(QKo -QSTKo*CFR)*tmpARR
           RH_BOX = QKo/QSTKo
         else
           RH_BOX = QKo/QSTKo
         end if

         IF ( RH_BOX < RHCR3 ) THEN
           Efactor = RHO_W * ( AA + BB ) / (RHCR3 - RH_BOX )
         else
           Efactor = 9.99e9
         end if


         LANDSEAF = 1.00


         if ( ( RH_BOX < RHCR3 ) .AND. ( DIAMrn > 0.00 ) .AND.
     &              ( PL > 100.) .AND. ( PL < REVAP_OFF_P ) ) then
           DROPRAD = 0.5*DIAMrn
           T_ED    = Efactor * DROPRAD**2
           T_ED    = T_ED * ( 1.0 + DQSTKo*alhlbcp )

           EVAP    = QPl*(1.0 - EXP( -C_EV_R * VErn * LANDSEAF *ENVFRAC*
     &                               TinLAYERrn / T_ED ) )
         ELSE
           EVAP = 0.0
         END if



         if (tmpARR > 0.0) then
           QKo = (QKo -QSICE*CFR)*tmpARR
           RH_BOX = QKo/QSICE
         else
           RH_BOX = QKo/QSICE
         end if
         IF ( RH_BOX < RHCR3 ) THEN
           Efactor = 0.5*RHO_W * ( AA+BB ) / (RHCR3-RH_BOX )
         else
           Efactor = 9.99e9
         end if


         if ( ( RH_BOX < RHCR3 ) .AND. ( DIAMsn > 0.00 ) .AND.
     &             ( PL > 100. ) .AND. ( PL < REVAP_OFF_P ) ) then
           FLAKRAD = 0.5*DIAMsn
           T_ED    = Efactor * FLAKRAD**2
           T_ED    = T_ED * ( 1.0 + DQSI*alhsbcp)
           SUBL    = QPi*(1.0 - EXP( -C_EV_S * VEsn * LANDSEAF * ENVFRAC
     &                               * TinLAYERsn / T_ED ) )

         ELSE
           SUBL = 0.0
         END IF

         if (itr == 1) then
           EVAPx = EVAP
           SUBLx = SUBL
         else
           EVAP = (EVAP+EVAPx) /2.0
           SUBL = (SUBL+SUBLx) /2.0
         endif

         EVAP = EVAP*(1.-CFR)
         SUBL = SUBL*(1.-CFR)
!      Anning prevent negative QPi and QPl
         SUBL = min(QPi, max(SUBL,0.))
         EVAP = min(QPl, max(EVAP,0.))


         QKo =QKo + EVAP + SUBL
         TKo = TKo - EVAP * alhlbcp - SUBL * alhsbcp

       enddo
       QPi = QPi - SUBL
       QPl = QPl - EVAP


       EVAP_DD = EVAP_DD_above + DDFRACT*EVAP*MASS
       EVAP = EVAP - DDFRACT*EVAP
       SUBL_DD = SUBL_DD_above + DDFRACT*SUBL*MASS
       SUBL = SUBL - DDFRACT*SUBL


       QV = QV + EVAP + SUBL
       TE = TE - EVAP * alhlbcp - SUBL * alhsbcp

       REVAP_DIAG = EVAP / DT
       RSUBL_DIAG = SUBL / DT

       PFl = QPl*MASS
       PFi = QPi*MASS

       PFL_DIAG = PFl/DT
       PFI_DIAG = PFi/DT
       end if



       EVAP = QDDF3*EVAP_DD/MASS
       SUBL = QDDF3*SUBL_DD/MASS
!      Anning prevent negative QPi and QPl
       SUBL = min(QPi, max(SUBL,0.))
       EVAP = min(QPl, max(EVAP,0.))
       QV   = QV + EVAP + SUBL
       TE   = TE - EVAP * alhlbcp - SUBL * alhsbcp
       REVAP_DIAG = REVAP_DIAG + EVAP / DT
       RSUBL_DIAG = RSUBL_DIAG + SUBL / DT

       IF (K == LM) THEN
       RAIN = PFl/DT
       SNOW = PFi/DT
       END IF

       QPi = 0.
       QPl = 0.

       PFl_above = PFl
       PFi_above = Pfi

       EVAP_DD_above = EVAP_DD
       SUBL_DD_above = SUBL_DD

       end subroutine precip3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine MARSHPALMQ2(RAIN,PR,DIAM3,NTOTAL,W,VE)

       real, intent(in ) :: RAIN,PR
       real, intent(out) :: DIAM3,NTOTAL,W,VE

       real :: RAIN_DAY,LAMBDA,A,B,SLOPR,DIAM1

       real, parameter :: N0 = 0.08

       INTEGER :: IQD

       real :: RX(8) , D3X(8)



       RX = (/ 0. , 5. , 20. , 80. , 320. , 1280., 4*1280., 16*1280. /)
       D3X = (/ 0.019, 0.032, 0.043, 0.057, 0.076, 0.102, 0.137 , 
     &    0.183 /)

       RAIN_DAY = RAIN * 3600. *24.

       IF ( RAIN_DAY <= 0.00 ) THEN
         DIAM1 = 0.00
         DIAM3 = 0.00
         NTOTAL= 0.00
         W = 0.00
       END IF

       DO IQD = 1,7
         IF ( (RAIN_DAY <= RX(IQD+1)) .AND. (RAIN_DAY > RX(IQD))) THEN
           SLOPR =( D3X(IQD+1)-D3X(IQD) ) / ( RX(IQD+1)-RX(IQD))
           DIAM3 = D3X(IQD) + (RAIN_DAY-RX(IQD))*SLOPR
         END IF
       END DO

       IF ( RAIN_DAY >= RX(8) ) THEN
         DIAM3=D3X(8)
       END IF

       NTOTAL = 0.019*DIAM3

       DIAM3 = 0.664 * DIAM3

       W = (2483.8 * DIAM3 + 80.)*SQRT(1000./PR)

       VE = MAX( 0.99*W/100. , 1.000 )

       DIAM1 = 3.0*DIAM3

       DIAM1 = DIAM1/100.
       DIAM3 = DIAM3/100.
       W = W/100.
       NTOTAL = NTOTAL*1.0e6

       end subroutine MARSHPALMQ2
!==========================================================

       subroutine MICRO_AA_BB_3(TEMP,PR,Q_SAT,AA,BB)

       real, intent(in ) :: TEMP,Q_SAT
       real, intent(in ) :: PR
       real, intent(out) :: AA,BB

       real :: E_SAT

       real, parameter :: EPSILON = 0.622
       real, parameter :: K_COND = 2.4e-2
       real, parameter :: DIFFU = 2.2e-5

       E_SAT = 100.* PR * Q_SAT /( (EPSILON) + (1.0-(EPSILON))*Q_SAT )

       AA = ( GET_ALHX3(TEMP)**2 ) / ( K_COND*MAPL_RVAP*(TEMP**2) )


       BB = MAPL_RVAP*TEMP / ( DIFFU*(1000./PR)*E_SAT )

       end subroutine MICRO_AA_BB_3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       function LDRADIUS3(PL,TE,QCL,NN) RESULT(RADIUS)

       real, intent(in) :: TE,PL,NN,QCL
       real :: RADIUS

       real :: MUU,RHO


       RHO = 100.*PL / (MAPL_RGAS*TE )
       MUU = QCL * RHO
       RADIUS = MUU/(NN*RHO_W*(4./3.)*MAPL_PI)
       RADIUS = RADIUS**(1./3.)


       end function LDRADIUS3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


       function ICE_FRACTION (TEMP) RESULT(ICEFRCT)
       real, intent(in) :: TEMP
       real :: ICEFRCT

       ICEFRCT = 0.00
       if ( TEMP <= T_ICE_ALL ) then
         ICEFRCT = 1.000
       else if ( (TEMP > T_ICE_ALL) .AND. (TEMP <= T_ICE_MAX) ) then
         ICEFRCT = 1.00 - ( TEMP - T_ICE_ALL ) / ( T_ICE_MAX -
     &   T_ICE_ALL )
       end if
       ICEFRCT = MIN(ICEFRCT,1.00)
       ICEFRCT = MAX(ICEFRCT,0.00)

       ICEFRCT = ICEFRCT**ICEFRPWR

       end function ICE_FRACTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       function GET_ALHX3(T) RESULT(ALHX3)

       real, intent(in) :: T
       real :: ALHX3

       real :: T_X

       T_X = T_ICE_MAX

       if ( T < T_ICE_ALL ) then
         ALHX3=MAPL_ALHS
       end if

       if ( T > T_X ) then
         ALHX3=MAPL_ALHL
       end if

       if ( (T <= T_X) .and. (T >= T_ICE_ALL) ) then
         ALHX3 = MAPL_ALHS + (MAPL_ALHL-MAPL_ALHS)
     &         *( T - T_ICE_ALL )/( T_X - T_ICE_ALL )
       end if

       end function GET_ALHX3


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


       real function ICEFRAC(T,T_TRANS,T_FREEZ)

       real, intent(in) :: T
       real, intent(in),optional :: T_TRANS
       real, intent(in),optional :: T_FREEZ

       real :: T_X,T_F

       if (present( T_TRANS )) then
         T_X = T_TRANS
       else
         T_X = T_ICE_MAX
       endif
       if (present( T_FREEZ )) then
         T_F = T_FREEZ
       else
         T_F = T_ICE_ALL
       endif


       if ( T < T_F ) ICEFRAC=1.000

       if ( T > T_X ) ICEFRAC=0.000

       if ( T <= T_X .and. T >= T_F ) then
         ICEFRAC = 1.00 - ( T - T_F ) /( T_X - T_F )
       endif

       end function ICEFRAC



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Parititions DQ into ice and liquid. Follows Morrison and Gettelman, 2008
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine Bergeron_iter ( DTIME , PL , TE , QV , QILS , QICN ,
     & QLLS , QLCN , CF , AF , NL , NI , DQALL , FQI )

      real , intent(in ) :: DTIME, PL, TE
      real , intent(inout ) :: DQALL
      real , intent(in) :: QV, QLLS, QLCN, QICN, QILS
      real , intent(in) :: CF, AF, NL, NI
      real, intent (out) :: FQI
      real :: DC, TEFF,QCm,DEP, QC, QS, RHCR, DQSL, DQSI, QI, TC, DIFF,
     & DENAIR, DENICE, AUX, DCF, QTOT, LHCORR, QL, DQI, DQL, QVINC,
     & QSLIQ, CFALL, new_QI, new_QL, QSICE, fQI_0, QS_0, DQS_0, FQA,
     & NIX
      real esl,esi, esn,desdt,weight,hlatsb,hlatvp,hltalt,tterm,
     &      gam,pl100
       

       pl100=pl*100
       DIFF = 0.0
       DEP=0.0
       QI = QILS + QICN
       QL = QLLS +QLCN
       QTOT=QI+QL
       FQA = 0.0
       if (QTOT .gt. 0.0) FQA = (QICN+QILS)/QTOT
       NIX= (1.0-FQA)*NI

       DQALL=DQALL/DTIME
       CFALL= min(CF+AF, 1.0)
       TC=TE-273.0
       fQI_0 = fQI



       if (TE .ge. T_ICE_MAX) then
           FQI = 0.0
       elseif(TE .le. T_ICE_ALL) then
           FQI = 1.0
       else


         FQI = 0.0
         if (QILS .le. 0.0) return

         QVINC= QV
!        QSLIQ = QSATLQ( TE , PL*100.0 , DQ=DQSL )
!        call vqsatd2_water_single(TE,PL*100.0,esl,QSLIQ,DQSL)
         esl=min(fpvsl(TE),pl100)
         QSLIQ= min(epsqs*esl/(pl100-omeps*esl),1.)

!        QSICE = QSATIC( TE , PL*100.0 , DQ=DQSI )
!        call vqsatd2_ice_single(TE,PL*100.0,esl,QSICE,DQSI)
         esi=min(fpvsi(TE),pl100)
         QSICE= min(epsqs*esi/(pl100-omeps*esi),1.)
         hltalt = hlatv + hlatf
         desdt  = hltalt*esi/(rgasv*TE*TE)
         if (QSICE < 1.0) then
           gam = hltalt*QSICE*pl100*desdt/(MAPL_CP*esi
     &         * (pl100 - omeps*esi))
         else
           gam = 0.0
         endif
         DQSI = (MAPL_CP/hltalt)*gam




         QVINC =MIN(QVINC, QSLIQ)

         DIFF=(0.211*1013.25/(PL+0.1))*(((TE+0.1)/273.0)**1.94)*1e-4
         DENAIR = PL*100.0/MAPL_RGAS/TE
         DENICE = 1000.0*(0.9167 - 1.75e-4*TC -5.0e-7*TC*TC)
         LHcorr = ( 1.0 + DQSI*alhsbcp)

         if ((NIX .gt. 1.0) .and. (QILS .gt. 1.0e-10)) then
           DC = max((QILS/(NIX*DENICE*MAPL_PI))**(0.333), 20.0e-6)
         else
           DC = 20.0e-6
         end if


         TEFF= NIX*DENAIR*2.0*MAPL_PI*DIFF*DC/LHcorr

         DEP=0.0
         if ((TEFF .gt. 0.0) .and. (QILS .gt. 1.0e-14)) then
           AUX =max(min(DTIME*TEFF, 20.0), 0.0)
           DEP=(QVINC-QSICE)*(1.0-EXP(-AUX))/DTIME
         end if

         DEP=MAX(DEP, -QILS/DTIME)

         DQI = 0.0
         DQL = 0.0
         FQI=0.0

         if (DQALL .ge. 0.0) then

           if (DEP .gt. 0.0) then
             DQI = min(DEP, DQALL + QLLS/DTIME)
             DQL = DQALL - DQI
           else
             DQL=DQALL
             DQI = 0.0
           end if
         end if


         if (DQALL .lt. 0.0) then
           DQL = max(DQALL, -QLLS/DTIME)
           DQI = max(DQALL - DQL, -QILS/DTIME)
         end if

         if (DQALL .ne. 0.0) FQI=max(min(DQI/DQALL, 1.0), 0.0)
       end if
      end subroutine Bergeron_iter



!=============================================================================
! Subroutine Pfreezing: calculates the probability of finding a supersaturated parcel in the grid cell
!SC_ICE is the effective freezing point for ice (Barahona & Nenes. 2009)
! Modified 02/19/15. in situ nucleation only occurs in the non_convective part of the grid cell


      subroutine Pfreezing ( ALPHA , PL , TE , QV , QCl , QAl , QCi ,
     &                       QAi , SC_ICE , CF , AF , PF )

      real , intent(in) :: PL,ALPHA, QV, SC_ICE, AF, TE, QCl, QCi, QAl,
     &                     QAi, CF
      real , intent(out) :: PF

      real :: qt, QCx, QSn, tmpARR, CFALL, QVx, CFio, QA, QAx, QC, QI,
     &        QL, DQSx, sigmaqt1, sigmaqt2, qsnx, esl, esi,pl100

      pl100 = pl*100

      QA = QAl + QAi
      QC = QCl + QCi
      CFALL = AF

      if ( CFALL >= 1.0 ) then
        PF = 0.0
        return
      end if

!     QSn = QSATIC( TE , PL*100.0 , DQ=DQSx )
!     call vqsatd2_ice_single(TE,PL*100.0,esl,QSn,DQSx)

      esi = min(fpvsi(TE),pl100)
      QSn = max(min(epsqs*esi/(pl100-omeps*esi), 1.0), 1.0e-9)

      tmpARR = 0.0
      if ( CFALL < 0.99 ) then
        tmpARR = 1./(1.0-CFALL)
      end if

      QCx = QC*tmpARR
      QVx = ( QV - QSn*CFALL )*tmpARR
!     QVx = QV*tmpARR

      qt  = QCx + QVx

      CFio = 0.0

      QSn  = QSn*SC_ICE

      if(pdfflag < 2) then
        sigmaqt1 = max(ALPHA, 0.1) * QSn
        sigmaqt2 = max(ALPHA, 0.1) * QSn
      elseif(pdfflag == 2) then
! for triangular, symmetric: sigmaqt1 = sigmaqt2 = alpha*qsn (alpha is half width)
! for triangular, skewed r : sigmaqt1 < sigmaqt2
! try: skewed right below 500 mb
!!!       if(pl.lt.500.) then
        sigmaqt1 = ALPHA * QSn
        sigmaqt2 = ALPHA * QSn
      elseif(pdfflag == 4) then
        sigmaqt1 = max(ALPHA/sqrt(3.0), 0.001)
      endif

      call pdffrac(pdfflag,qt,sigmaqt1,sigmaqt2,qsn,CFio)

      PF = min(max(CFio*(1.0-CFALL), 0.0), 0.999)


      end subroutine Pfreezing




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Instantaneous freezing of condensate!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine meltfrz_inst(IM, LM, TE, QCL, QAL, QCI, QAI, NL, NI)

      integer, intent(in) :: IM, LM
      real , intent(inout), dimension(:,:) :: TE,QCL,QCI, QAL, QAI,
     &                                        NI, NL

      real , dimension(im,lm) :: fQi,dQil, DQmax, QLTOT, QITOT, FQAL,
     &                           FQAI, dNil, FQA

      QITOT = QCI+QAI
      QLTOT = QCL + QAL
      FQA   = 0.0

      where (QITOT+QLTOT > 0.0)
        FQA= (QAI+QAL)/(QITOT+QLTOT)
      end where

       dQil  = 0.0
       dNil  = 0.0
       DQmax = 0.0

       where( TE <= T_ICE_ALL )
         DQmax = (T_ICE_ALL - TE)/(alhsbcp-alhlbcp)
         dQil  = min(QLTOT , DQmax)
       end where

       where ((dQil <= DQmax) .and. (dQil > 0.0))
         dNil = NL
       end where

       where ((dQil > DQmax) .and. (dQil > 0.0))
         dNil = NL*DQmax/dQil
       end where

       dQil  = max( 0., dQil )
!      Anning for moisture conservation 11/22/2016
!      QITOT = max(QITOT + dQil, 0.0)
!      QLTOT = max(QLTOT - dQil, 0.0)
       dQil = min(QLTOT,dQil)
       QITOT = QITOT + dQil
       QLTOT = QLTOT - dQil
       NL = NL - dNil
       NI = NI + dNil
       TE = TE + (alhsbcp-alhlbcp)*dQil

       dQil  = 0.0
       dNil  = 0.0
       DQmax = 0.0


       where( TE > T_ICE_MAX )
         DQmax = (TE-T_ICE_MAX) / (alhsbcp-alhlbcp)
         dQil = min(QITOT, DQmax)
       endwhere

       where ((dQil .le. DQmax) .and. (dQil .gt. 0.0))
         dNil = NI
       end where
       where ((dQil .gt. DQmax) .and. (dQil .gt. 0.0))
         dNil = NI*DQmax/dQil
       end where
       dQil = max( 0., dQil )
!      Anning for moisture conservation 11/22/2016
!      QLTOT = max(QLTOT+ dQil, 0.)
!      QITOT = max(QITOT - dQil, 0.)
       dQil = min(QITOT,dQil)
       QITOT = QITOT - dQil
       QLTOT = QLTOT + dQil
       NL = NL + dNil
       NI = NI - dNil

       TE = TE - (alhsbcp-alhlbcp)*dQil

       QCI = QITOT*(1.0-FQA)
       QAI = QITOT*FQA
       QCL = QLTOT*(1.0-FQA)
       QAL = QLTOT*FQA

      end subroutine meltfrz_inst



!======================================
      subroutine cloud_ptr_stubs (
     & SMAXL, SMAXI, WSUB, CCN01, CCN04, CCN1, NHET_NUC, NLIM_NUC, SO4,
     & ORG, BCARBON, DUST, SEASALT, NCPL_VOL, NCPI_VOL, NRAIN, NSNOW,
     & CDNC_NUC, INC_NUC, SAT_RAT, QSTOT, QRTOT, CLDREFFS, CLDREFFR,
     & DQVDT_micro,DQIDT_micro, DQLDT_micro, DTDT_micro, RL_MASK,
     & RI_MASK, KAPPA, SC_ICE, CFICE, CFLIQ, RHICE, RHLIQ, RAD_CF,
     & RAD_QL, RAD_QI, RAD_QS, RAD_QR, RAD_QV, CLDREFFI, CLDREFFL,
     & NHET_IMM, NHET_DEP, NHET_DHF, DUST_IMM, DUST_DEP, DUST_DHF, SCF,
     & SCF_ALL, SIGW_GW, SIGW_CNV, SIGW_TURB, SIGW_RC, RHCmicro,
     & DNHET_IMM, NONDUST_IMM, NONDUST_DEP, BERG, BERGSO, MELT,
     & DNHET_CT, DTDT_macro, QCRES, DT_RASP, FRZPP_LS, SNOWMELT_LS,
     & QIRES, AUTICE, PFRZ, DNCNUC, DNCHMSPLIT, DNCSUBL, DNCAUTICE,
     & DNCACRIS, DNDCCN, DNDACRLS, DNDEVAPC, DNDACRLR, DNDAUTLIQ)
!    & DNDCNV, DNCCNV)



      real , pointer , dimension(:,:,:) :: SMAXL,SMAXI, WSUB, CCN01,
     & CCN04, CCN1, NHET_NUC, NLIM_NUC, SO4, ORG, BCARBON, DUST,
     & SEASALT, NCPL_VOL, NCPI_VOL, NRAIN, NSNOW, CDNC_NUC, INC_NUC,
     & SAT_RAT, QSTOT, QRTOT, CLDREFFS, CLDREFFR, DQVDT_micro,
     &DQIDT_micro, DQLDT_micro, DTDT_micro, RL_MASK, RI_MASK, KAPPA,
     & SC_ICE, CFICE, CFLIQ, RHICE, RHLIQ, ALPH, RAD_CF, RAD_QL, RAD_QI,
     & RAD_QS, RAD_QR, RAD_QV, CLDREFFI, CLDREFFL, NHET_IMM, NHET_DEP,
     & NHET_DHF, DUST_IMM, DUST_DEP, DUST_DHF, SCF, SCF_ALL, SIGW_GW,
     & SIGW_CNV, SIGW_TURB, SIGW_RC, RHCmicro, DNHET_IMM, NONDUST_IMM,
     & NONDUST_DEP, BERG, BERGSO, MELT, DNHET_CT, DTDT_macro, QCRES,
     & DT_RASP, FRZPP_LS, SNOWMELT_LS, QIRES, AUTICE, PFRZ, DNCNUC,
     & DNCHMSPLIT, DNCSUBL, DNCAUTICE, DNCACRIS, DNDCCN, DNDACRLS,
     & DNDEVAPC, DNDACRLR, DNDAUTLIQ
!    & DNDEVAPC, DNDACRLR, DNDAUTLIQ, DNDCNV, DNCCNV


!DONIF

      IF( ASSOCIATED(SMAXL) ) SMAXL = 0.
      IF( ASSOCIATED(SMAXI) ) SMAXI = 0.
      IF( ASSOCIATED(WSUB) ) WSUB = 0.
      IF( ASSOCIATED(CCN01) ) CCN01 = 0.
      IF( ASSOCIATED(CCN04) ) CCN04 = 0.
      IF( ASSOCIATED(CCN1) ) CCN1 = 0.
      IF( ASSOCIATED(NHET_NUC) ) NHET_NUC = 0.
      IF( ASSOCIATED(NLIM_NUC) ) NLIM_NUC = 0.
      IF( ASSOCIATED(SO4) ) SO4 = 0.
      IF( ASSOCIATED(ORG) ) ORG = 0.
      IF( ASSOCIATED(BCARBON) ) BCARBON = 0.
      IF( ASSOCIATED(DUST) ) DUST = 0.
      IF( ASSOCIATED(SEASALT) ) SEASALT = 0.
      IF( ASSOCIATED(NCPL_VOL) ) NCPL_VOL = 0.
      IF( ASSOCIATED(NCPI_VOL) ) NCPI_VOL = 0.

      IF( ASSOCIATED(NRAIN) ) NRAIN = 0.
      IF( ASSOCIATED(NSNOW) ) NSNOW = 0.
      IF( ASSOCIATED(CDNC_NUC) ) CDNC_NUC = 0.
      IF( ASSOCIATED(INC_NUC) ) INC_NUC = 0.
      IF( ASSOCIATED(SAT_RAT) ) SAT_RAT = 0.
      IF( ASSOCIATED(QSTOT) ) QSTOT = 0.
      IF( ASSOCIATED(QRTOT) ) QRTOT = 0.

      IF( ASSOCIATED(DQVDT_micro) ) DQVDT_micro = 0.
      IF( ASSOCIATED(DQIDT_micro) ) DQIDT_micro = 0.
      IF( ASSOCIATED(DQLDT_micro) ) DQLDT_micro = 0.
      IF( ASSOCIATED(DTDT_micro) ) DTDT_micro = 0.
      IF( ASSOCIATED(DTDT_macro) ) DTDT_macro = 0.

      IF( ASSOCIATED(RL_MASK) ) RL_MASK = 0.
      IF( ASSOCIATED(RI_MASK) ) RI_MASK = 0.
      IF( ASSOCIATED(KAPPA) ) KAPPA = 0.
      IF( ASSOCIATED(SC_ICE)) SC_ICE = 0.
      IF( ASSOCIATED(RHICE) ) RHICE = 0.
      IF( ASSOCIATED(RHLIQ) ) RHLIQ = 0.
      IF( ASSOCIATED(CFICE) ) CFICE = 0.
      IF( ASSOCIATED(CFLIQ) ) CFLIQ = 0.
      IF( ASSOCIATED(ALPH) ) ALPH = 0.


      IF( ASSOCIATED(RAD_CF) ) RAD_CF = 0.
      IF( ASSOCIATED(RAD_QL) ) RAD_QL = 0.
      IF( ASSOCIATED(RAD_QI) ) RAD_QI = 0.
      IF( ASSOCIATED(RAD_QS) ) RAD_QS = 0.
      IF( ASSOCIATED(RAD_QR) ) RAD_QR = 0.
      IF( ASSOCIATED(RAD_QV) ) RAD_QV = 0.
      IF( ASSOCIATED(CLDREFFI) ) CLDREFFI = 0.
      IF( ASSOCIATED(CLDREFFL) ) CLDREFFL = 0.
      IF( ASSOCIATED(CLDREFFS) ) CLDREFFS = 0.
      IF( ASSOCIATED(CLDREFFR) ) CLDREFFR = 0.

      IF( ASSOCIATED(NHET_IMM) ) NHET_IMM = 0.
      IF( ASSOCIATED(NHET_DEP) ) NHET_DEP = 0.
      IF( ASSOCIATED(NHET_DHF) ) NHET_DHF = 0.
      IF( ASSOCIATED(DUST_IMM) ) DUST_IMM = 0.
      IF( ASSOCIATED(DUST_DEP) ) DUST_DEP = 0.
      IF( ASSOCIATED(DUST_DHF) ) DUST_DHF = 0.
      IF( ASSOCIATED(NONDUST_IMM) ) NONDUST_IMM = 0.
      IF( ASSOCIATED(NONDUST_DEP) ) NONDUST_DEP = 0.


      IF( ASSOCIATED(SCF) ) SCF = 0.
      IF( ASSOCIATED(SCF_ALL) ) SCF_ALL = 0.
      IF( ASSOCIATED(SIGW_GW) ) SIGW_GW = 0.
      IF( ASSOCIATED(SIGW_CNV) ) SIGW_CNV = 0.
      IF( ASSOCIATED(SIGW_TURB) ) SIGW_TURB = 0.
      IF( ASSOCIATED(SIGW_RC) ) SIGW_RC = 0.
      IF( ASSOCIATED(RHCmicro) ) RHCmicro = 0.
      IF( ASSOCIATED(DNHET_IMM) ) DNHET_IMM = 0.
      IF( ASSOCIATED(BERG) ) BERG = 0.
      IF( ASSOCIATED(BERGSO)) BERGSO = 0.
      IF( ASSOCIATED(MELT) ) MELT = 0.
      IF( ASSOCIATED(DNHET_CT) ) DNHET_CT = 0.
      IF( ASSOCIATED(DT_RASP) ) DT_RASP = 0.

      IF( ASSOCIATED(QCRES) ) QCRES = 0.
      IF( ASSOCIATED(QIRES) ) QIRES = 0.
      IF( ASSOCIATED(AUTICE) ) AUTICE = 0.
      IF( ASSOCIATED(FRZPP_LS) ) FRZPP_LS = 0.
      IF( ASSOCIATED(SNOWMELT_LS) ) SNOWMELT_LS = 0.
      IF( ASSOCIATED(PFRZ) ) PFRZ = 0.

      IF( ASSOCIATED(DNCNUC) ) DNCNUC = 0.
      IF( ASSOCIATED(DNCSUBL) ) DNCSUBL = 0.
      IF( ASSOCIATED(DNCHMSPLIT) ) DNCHMSPLIT = 0.
      IF( ASSOCIATED(DNCAUTICE) ) DNCAUTICE = 0.
      IF( ASSOCIATED(DNCACRIS) ) DNCACRIS = 0.
      IF( ASSOCIATED(DNDCCN) ) DNDCCN = 0.
      IF( ASSOCIATED(DNDACRLS) ) DNDACRLS = 0.
      IF( ASSOCIATED(DNDACRLR) ) DNDACRLR = 0.
      IF( ASSOCIATED(DNDEVAPC) ) DNDEVAPC = 0.
      IF( ASSOCIATED(DNDAUTLIQ) ) DNDAUTLIQ = 0.
!     IF( ASSOCIATED(DNDCNV) ) DNDCNV = 0.
!     IF( ASSOCIATED(DNCCNV) ) DNCCNV = 0.

      end subroutine cloud_ptr_stubs

!C=======================================================================
!C
!C *** REAL FUNCTION erf (overwrites previous versions)
!C *** THIS SUBROUTINE CALCULATES THE ERROR FUNCTION USING A
!C *** POLYNOMIAL APPROXIMATION
!C
!C=======================================================================
!C
       REAL FUNCTION erf_app(x)
       REAL :: x
       REAL*8:: AA(4), axx, y
       DATA AA /0.278393d0,0.230389d0,0.000972d0,0.078108d0/

       y = dabs(dble(x))
       axx = 1.d0 + y*(AA(1)+y*(AA(2)+y*(AA(3)+y*AA(4))))
       axx = axx*axx
       axx = axx*axx
       axx = 1.d0 - (1.d0/axx)
       if(x.le.0.) then
         erf_app = sngl(-axx)
       else
         erf_app = sngl(axx)
       endif
       RETURN
       END FUNCTION

      end module cldmacro
