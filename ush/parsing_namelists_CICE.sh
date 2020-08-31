# parsing namelist of CICE

CICE_namelists(){

cat > ice_in <<eof  
&setup_nml
    days_per_year  = 365
  , use_leap_years = .true.
  , year_init      = $year
  , istep0         = $steps
  , dt             = $ICETIM
  , npt            = $npt
  , ndtd           = 1
  , runtype        = '$RUNTYPE' 
  , runid          = 'cpcice' 
  , ice_ic         = '$iceic'
  , restart        = .true.
  , restart_ext    = .false.
  , use_restart_time = $USE_RESTART_TIME
  , restart_format = 'nc'
  , lcdf64         = .false.
  , restart_dir    = './restart/'
  , restart_file   = 'iced'
  , pointer_file   = './restart/ice.restart_file'
  , dumpfreq       = '$dumpfreq'
  , dumpfreq_n     =  $dumpfreq_n
  , dump_last      = .false.  
  , diagfreq       = 6
  , diag_type      = 'file'
  , diag_file      = 'ice_diag.d'
  , print_global   = .true.
  , print_points   = .true.
  , latpnt(1)      =  90.
  , lonpnt(1)      =   0.
  , latpnt(2)      = -65.
  , lonpnt(2)      = -45.
  , dbug           = .false.
  , histfreq       = 'm','d','h','x','x'
  , histfreq_n     =  0 , 0 , 6 , 1 , 1
  , hist_avg       = $cice_hist_avg
  , history_dir    = './history/'
  , history_file   = 'iceh'
  , write_ic       = .true.
  , incond_dir     = './history/'
  , incond_file    = 'iceh_ic'
/

&grid_nml
    grid_format  = 'nc'
  , grid_type    = 'displaced_pole'
  , grid_file    = '$ice_grid_file'
  , kmt_file     = '$ice_kmt_file'
  , kcatbound    = 0
/

&domain_nml
    nprocs = 48 
  , processor_shape   = 'slenderX2'
  , distribution_type = 'cartesian'
  , distribution_wght = 'latitude'
  , ew_boundary_type  = 'cyclic'
  , ns_boundary_type  = 'tripole'
  , maskhalo_dyn      = .false.
  , maskhalo_remap    = .false.
  , maskhalo_bound    = .false.
/

&tracer_nml
    tr_iage      = .true.
  , restart_age  = .false.
  , tr_FY        = .true.
  , restart_FY   = .false.
  , tr_lvl       = .true.
  , restart_lvl  = .false.
  , tr_pond_cesm = .false.
  , restart_pond_cesm = .false.
  , tr_pond_topo = .false.
  , restart_pond_topo = .false.
  , tr_pond_lvl  = $tr_pond_lvl
  , restart_pond_lvl  = $restart_pond_lvl
  , tr_aero      = .false.
  , restart_aero = .false.
/

&thermo_nml
    kitd              = 1
  , ktherm            = 1
  , conduct           = 'MU71'
  , a_rapid_mode      =  0.5e-3
  , Rac_rapid_mode    =    10.0
  , aspect_rapid_mode =     1.0
  , dSdt_slow_mode    = -5.0e-8
  , phi_c_slow_mode   =    0.05
  , phi_i_mushy       =    0.85
/

&dynamics_nml
    kdyn            = 1
  , ndte            = 120
  , revised_evp     = .false.
  , advection       = 'remap'
  , kstrength       = 1
  , krdg_partic     = 1
  , krdg_redist     = 1
  , mu_rdg          = 3
/

&shortwave_nml
    shortwave       = 'dEdd'
  , albedo_type     = 'default'
  , albicev         = 0.78
  , albicei         = 0.36
  , albsnowv        = 0.98
  , albsnowi        = 0.70 
  , ahmax           = 0.3
  , R_ice           = 0.
  , R_pnd           = 0.
  , R_snw           = 1.5
  , dT_mlt          = 1.5
  , rsnw_mlt        = 1500.
/

&ponds_nml
    hp1             = 0.01
  , hs0             = 0.
  , hs1             = 0.03
  , dpscale         = 1.0e-3
  , frzpnd          = 'hlid'
  , snowinfil       = .true.
  , rfracmin        = 0.15
  , rfracmax        = 1.
  , pndaspect       = 0.8
/

&zbgc_nml
    tr_brine        = .false.
  , restart_hbrine  = .false.
  , skl_bgc         = .false.
  , bgc_flux_type   = 'Jin2006'
  , restart_bgc     = .false.
  , restore_bgc     = .false.
  , bgc_data_dir    = 'unknown_bgc_data_dir'
  , sil_data_type   = 'default'
  , nit_data_type   = 'default'
  , tr_bgc_C_sk     = .false.
  , tr_bgc_chl_sk   = .false.
  , tr_bgc_Am_sk    = .false.
  , tr_bgc_Sil_sk   = .false.
  , tr_bgc_DMSPp_sk = .false.
  , tr_bgc_DMSPd_sk = .false.
  , tr_bgc_DMS_sk   = .false.
  , phi_snow        = 0.5
/

&forcing_nml
    formdrag        = .false.
  , atmbndy         = 'default'
  , fyear_init      = 1997
  , ycycle          = 1
  , atm_data_format = 'bin'
  , atm_data_type   = 'none'
  , atm_data_dir    = '/home/Fei.Liu/noscrub/lanl_cice_data/'
  , calc_strair     = .true.
  , calc_Tsfc       = .true.
  , precip_units    = 'mm_per_month'
  , ustar_min       = 0.0005
  , update_ocn_f    = $FRAZIL_FWSALT
  , oceanmixed_ice  = .false.
  , ocn_data_format = 'bin'
  , sss_data_type   = 'default'
  , sst_data_type   = 'default'
  , ocn_data_dir    = 'unknown_ocn_data_dir'
  , oceanmixed_file = 'unknown_oceanmixed_file'
  , restore_sst     = .false.
  , trestore        =  90
  , restore_ice     = .false.
/

&icefields_nml
    f_tmask         = .true.
  , f_tarea         = .true.
  , f_uarea         = .true.
  , f_dxt           = .false.
  , f_dyt           = .false.
  , f_dxu           = .false.
  , f_dyu           = .false.
  , f_HTN           = .false.
  , f_HTE           = .false.
  , f_ANGLE         = .true.
  , f_ANGLET        = .true.
  , f_NCAT          = .true.
  , f_VGRDi         = .false.
  , f_VGRDs         = .false.
  , f_VGRDb         = .false.
  , f_bounds        = .false.
  , f_aice          = 'mdhxx' 
  , f_hi            = 'mdhxx'
  , f_hs            = 'mdhxx' 
  , f_Tsfc          = 'mdhxx' 
  , f_sice          = 'x' 
  , f_uvel          = 'mdhxx' 
  , f_vvel          = 'mdhxx' 
  , f_fswdn         = 'mdhxx' 
  , f_flwdn         = 'mdhxx'
  , f_snow          = 'mdhxx' 
  , f_snow_ai       = 'xxxxx' 
  , f_rain          = 'mdhxx' 
  , f_rain_ai       = 'xxxxx' 
  , f_sst           = 'mdhxx' 
  , f_sss           = 'mdhxx' 
  , f_uocn          = 'x' 
  , f_vocn          = 'x' 
  , f_frzmlt        = 'x'
  , f_fswfac        = 'x'
  , f_fswabs        = 'mdhxx' 
  , f_fswabs_ai     = 'xxxxx' 
  , f_albsni        = 'mdhxx' 
  , f_alvdr         = 'x'
  , f_alidr         = 'x'
  , f_albice        = 'x'
  , f_albsno        = 'x'
  , f_albpnd        = 'x'
  , f_coszen        = 'x'
  , f_flat          = 'mdhxx' 
  , f_flat_ai       = 'xxxxx' 
  , f_fsens         = 'mdhxx' 
  , f_fsens_ai      = 'xxxxx' 
  , f_flwup         = 'mdhxx' 
  , f_flwup_ai      = 'xxxxx' 
  , f_evap          = 'mdhxx' 
  , f_evap_ai       = 'xxxxx' 
  , f_Tair          = 'mdhxx' 
  , f_Tref          = 'mdhxx' 
  , f_Qref          = 'x'
  , f_congel        = 'x' 
  , f_frazil        = 'x' 
  , f_snoice        = 'x' 
  , f_dsnow         = 'x' 
  , f_melts         = 'x'
  , f_meltt         = 'mdhxx'
  , f_meltb         = 'mdhxx'
  , f_meltl         = 'x'
  , f_fresh         = 'x'
  , f_fresh_ai      = 'xxxxx'
  , f_fsalt         = 'x'
  , f_fsalt_ai      = 'xxxxx'
  , f_fhocn         = 'x' 
  , f_fhocn_ai      = 'xxxxx' 
  , f_fswthru       = 'x' 
  , f_fswthru_ai    = 'xxxxx' 
  , f_fsurf_ai      = 'xxxxx'
  , f_fcondtop_ai   = 'xxxxx'
  , f_fmeltt_ai     = 'xxxxx' 
  , f_strairx       = 'mdhxx' 
  , f_strairy       = 'mdhxx' 
  , f_strtltx       = 'x' 
  , f_strtlty       = 'x' 
  , f_strcorx       = 'x' 
  , f_strcory       = 'x' 
  , f_strocnx       = 'x' 
  , f_strocny       = 'x' 
  , f_strintx       = 'x' 
  , f_strinty       = 'x'
  , f_strength      = 'x'
  , f_divu          = 'mdhxx'
  , f_shear         = 'mdhxx'
  , f_sig1          = 'x' 
  , f_sig2          = 'x' 
  , f_dvidtt        = 'mdhxx' 
  , f_dvidtd        = 'mdhxx' 
  , f_daidtt        = 'mdhxx'
  , f_daidtd        = 'mdhxx' 
  , f_mlt_onset     = 'mdhxx'
  , f_frz_onset     = 'mdhxx'
  , f_hisnap        = 'x'
  , f_aisnap        = 'x'
  , f_trsig         = 'x'
  , f_icepresent    = 'x'
  , f_iage          = 'x'
  , f_FY            = 'x'
  , f_aicen         = 'xxxxx'
  , f_vicen         = 'xxxxx'
  , f_Tinz          = 'x'
  , f_Sinz          = 'x'
  , f_Tsnz          = 'x'
  , f_fsurfn_ai     = 'xxxxx'
  , f_fcondtopn_ai  = 'xxxxx'
  , f_fmelttn_ai    = 'xxxxx'
  , f_flatn_ai      = 'xxxxx'
  , f_s11           = 'mdhxx'
  , f_s12           = 'mdhxx'
  , f_s22           = 'mdhxx'
  , f_yieldstress11 = 'mdhxx'
  , f_yieldstress12 = 'mdhxx'
  , f_yieldstress22 = 'mdhxx'
/

&icefields_mechred_nml
    f_alvl         = 'mdhxx'
  , f_vlvl         = 'mdhxx'
  , f_ardg         = 'mdhxx'
  , f_vrdg         = 'mdhxx'
  , f_dardg1dt     = 'x'
  , f_dardg2dt     = 'x'
  , f_dvirdgdt     = 'x'
  , f_opening      = 'mdhxx'
  , f_ardgn        = 'xxxxx'
  , f_vrdgn        = 'xxxxx'
  , f_dardg1ndt    = 'x'
  , f_dardg2ndt    = 'x'
  , f_dvirdgndt    = 'x'
  , f_krdgn        = 'x'
  , f_aparticn     = 'x'
  , f_aredistn     = 'x'
  , f_vredistn     = 'x'
  , f_araftn       = 'x'
  , f_vraftn       = 'x'
/

&icefields_pond_nml
    f_apondn       = 'xxxxx'
  , f_apeffn       = 'xxxxx'
  , f_hpondn       = 'xxxxx'
  , f_apond        = 'mdhxx'
  , f_hpond        = 'mdhxx'
  , f_ipond        = 'mdhxx'
  , f_apeff        = 'mdhxx'
  , f_apond_ai     = 'xxxxx'
  , f_hpond_ai     = 'xxxxx'
  , f_ipond_ai     = 'xxxxx'
  , f_apeff_ai     = 'xxxxx'
/

&icefields_bgc_nml
    f_faero_atm    = 'x'
  , f_faero_ocn    = 'x'
  , f_aero         = 'x'
  , f_fNO          = 'x'
  , f_fNO_ai       = 'x'
  , f_fNH          = 'x'
  , f_fNH_ai       = 'x'
  , f_fN           = 'x'
  , f_fN_ai        = 'x'
  , f_fSil         = 'x'
  , f_fSil_ai      = 'x'
  , f_bgc_N_sk     = 'x'
  , f_bgc_C_sk     = 'x'
  , f_bgc_chl_sk   = 'x'
  , f_bgc_Nit_sk   = 'x'
  , f_bgc_Am_sk    = 'x'
  , f_bgc_Sil_sk   = 'x'
  , f_bgc_DMSPp_sk = 'x'
  , f_bgc_DMSPd_sk = 'x'
  , f_bgc_DMS_sk   = 'x'
  , f_bgc_Nit_ml   = 'x'
  , f_bgc_Am_ml    = 'x'
  , f_bgc_Sil_ml   = 'x'  
  , f_bgc_DMSP_ml  = 'x'
  , f_bTin         = 'x'
  , f_bphi         = 'x' 
  , f_fbri         = 'x'    
  , f_hbri         = 'x'
  , f_grownet      = 'x'
  , f_PPnet        = 'x'
/

&icefields_drag_nml
    f_drag         = 'mdhxx'
  , f_Cdn_atm      = 'mdhxx'
  , f_Cdn_ocn      = 'mdhxx'
/
eof

}
