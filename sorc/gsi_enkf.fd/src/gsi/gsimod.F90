!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: gsimod  ---

!
! !INTERFACE:
!
  module gsimod

! !USES:

  use kinds, only: i_kind,r_kind
  use obsmod, only: dmesh,dval,dthin,dtype,dfile,dplat,dsfcalc,ndat,&
     init_obsmod_dflts,create_obsmod_vars,write_diag,reduce_diag,oberrflg,&
     time_window,perturb_obs,perturb_fact,sfcmodel,destroy_obsmod_vars,dsis,&
     dtbduv_on,time_window_max,offtime_data,init_directories,oberror_tune,ext_sonde, &
     blacklst,init_obsmod_vars,lobsdiagsave,lobskeep,lobserver,hilbert_curve,&
     lread_obs_save,lread_obs_skip,time_window_rad,tcp_posmatch,tcp_box, &
     neutral_stability_windfact_2dvar,use_similarity_2dvar,ta2tb
  use gsi_dbzOper, only: diag_radardbz
  use gsi_fedOper, only: diag_fed

  use obsmod, only: doradaroneob,dofedoneob,oneoblat,oneoblon,oneobheight,oneobvalue,oneobddiff,oneobradid,&
     radar_no_thinning,ens_hx_dbz_cut,static_gsi_nopcp_dbz,rmesh_dbz,&
     rmesh_vr,zmesh_dbz,zmesh_vr,if_vterminal, if_model_dbz,if_model_fed,innov_use_model_fed,if_vrobs_raw,if_use_w_vr,&
     minobrangedbz,maxobrangedbz,maxobrangevr,maxtiltvr,inflate_dbz_obserr,missing_to_nopcp,&
     ntilt_radarfiles,whichradar,&
     minobrangevr,maxtiltdbz,mintiltvr,mintiltdbz,l2rwthin,hurricane_radar,&
     r_hgt_fed

  use obsmod, only: lwrite_predterms, &
     lwrite_peakwt,use_limit,lrun_subdirs,l_foreaft_thin,lobsdiag_forenkf,&
     obsmod_init_instr_table,obsmod_final_instr_table
  use obsmod, only: luse_obsdiag
  use obsmod, only: netcdf_diag, binary_diag
  use obsmod, only: l_wcp_cwm,ompslp_mult_fact
  use obsmod, only: l_obsprvdiag
  use obsmod, only: aircraft_recon, &
       
       ! The following variables are the coefficients that describe
       ! the linear regression fits that are used to define the
       ! dynamic observation error (DOE) specifications for all
       ! reconnissance observations collected within
       ! hurricanes/tropical cyclones; these apply only to the
       ! regional forecast models (e.g., HWRF); Henry R. Winterbottom
       ! (henry.winterbottom@noaa.gov).
       
       q_doe_a_136,q_doe_a_137,q_doe_b_136,q_doe_b_137, &
       t_doe_a_136,t_doe_a_137,t_doe_b_136,t_doe_b_137, &
       uv_doe_a_236,uv_doe_a_237,uv_doe_a_213,uv_doe_b_236,uv_doe_b_237,&
       uv_doe_b_213

  use obsmod, only: vad_near_analtime
  
  use aircraftinfo, only: init_aircraft,hdist_aircraft,aircraft_t_bc_pof,aircraft_t_bc, &
                          aircraft_t_bc_ext,biaspredt,upd_aircraft,cleanup_tail
       
  use obs_sensitivity, only: lobsensfc,lobsensincr,lobsensjb,lsensrecompute, &
                             lobsensadj,lobsensmin,iobsconv,llancdone,init_obsens
  use gsi_4dvar, only: setup_4dvar,init_4dvar,nhr_assimilation,min_offset, &
                       l4dvar,nhr_obsbin,nhr_subwin,nwrvecs,iorthomax,&
                       lbicg,lsqrtb,lcongrad,lbfgsmin,ltlint,ladtest,ladtest_obs, lgrtest,&
                       idmodel,clean_4dvar,iwrtinc,lanczosave,jsiga,ltcost,liauon, &
                       l4densvar,ens_nstarthr,lnested_loops,lwrite4danl,nhr_anal,thin4d,tau_fcst,efsoi_order
  use gsi_4dvar, only: mPEs_observer
  use m_obsdiags, only: alwaysLocal => obsdiags_alwaysLocal
  use obs_ferrscale, only: lferrscale
  use mpimod, only: npe,mpi_comm_world,ierror,mype
  use radinfo, only: retrieval,diag_rad,init_rad,init_rad_vars,adp_anglebc,angord,upd_pred,&
                       biaspredvar,use_edges,passive_bc,newpc4pred,final_rad_vars,emiss_bc,&
                       ssmis_method,ssmis_precond,gmi_method,amsr2_method,bias_zero_start, &
                       reset_bad_radbc,cld_det_dec2bin,diag_version,lupdqc,lqcoef
  use radinfo, only: tzr_qc,tzr_bufrsave
  use radinfo, only: crtm_coeffs_path,optconv
  use ozinfo, only: diag_ozone,init_oz
  use aeroinfo, only: diag_aero, init_aero, init_aero_vars, final_aero_vars
  use coinfo, only: diag_co,init_co
  use convinfo, only: init_convinfo, &
                      diag_conv,&
                      use_prepb_satwnd,id_drifter, ec_amv_qc,&
                      id_ship
  use lightinfo, only: diag_light,init_light

  use oneobmod, only: oblon,oblat,obpres,obhourset,obdattim,oneob_type,&
     oneobtest,magoberr,maginnov,init_oneobmod,pctswitch,lsingleradob,obchan,&
     anaz_rw,anel_rw,range_rw,sstn,lsingleradar,singleradar,learthrel_rw
  use balmod, only: init_balmod,fstat,lnobalance
  use turblmod, only: use_pbl,init_turbl
  use qcmod, only: dfact,dfact1,create_qcvars,destroy_qcvars,&
      erradar_inflate,tdrerr_inflate,use_poq7,qc_satwnds,&
      init_qcvars,vadfile,noiqc,c_varqc,gps_jacqc,qc_noirjaco3,qc_noirjaco3_pole,&
      buddycheck_t,buddydiag_save,njqc,vqc,nvqc,hub_norm,vadwnd_l2rw_qc, &
      pvis,pcldch,scale_cv,estvisoe,estcldchoe,vis_thres,cldch_thres,cao_check, &
      cris_cads, iasi_cads, iasing_cads, airs_cads
  use qcmod, only: troflg,lat_c,nrand
  use cads, only: M__Sensor,N__Num_Bands,N__GradChkInterval,N__Band_Size,N__Bands,N__Window_Width, &
      N__Window_Bounds,R__BT_Threshold,R__Grad_Threshold,R__Window_Grad_Threshold, L__Do_Quick_Exit, &
      L__Do_CrossBand, N__BandToUse,L__Do_Imager_Cloud_Detection, N__Num_Imager_Chans, &
      N__Num_Imager_Clusters,N__Imager_Chans,R__Stddev_Threshold,R__Coverage_Threshold, &
      R__FG_Departure_Threshold, CADS_Setup_Cloud
  use pcpinfo, only: npredp,diag_pcp,dtphys,deltim,init_pcp
  use jfunc, only: iout_iter,iguess,miter,factqmin,factqmax,superfact,limitqobs, &
     factql,factqi,factqr,factqs,factqg, &  
     factv,factl,factp,factg,factw10m,facthowv,factcldch,niter,niter_no_qc,biascor,&
     init_jfunc,qoption,cwoption,switch_on_derivatives,tendsflag,jiterstart,jiterend,R_option,&
     bcoption,diurnalbc,print_diag_pcg,tsensible,diag_precon,step_start,pseudo_q2,&
     clip_supersaturation,cnvw_option,hofx_2m_sfcfile
  use state_vectors, only: init_anasv,final_anasv
  use control_vectors, only: init_anacv,final_anacv,nrf,nvars,nrf_3d,cvars3d,cvars2d,&
     nrf_var,lcalc_gfdl_cfrac,incvars_to_zero,incvars_zero_strat,incvars_efold 
  use derivsmod, only: init_anadv
  use berror, only: norh,ndeg,vs,bw,init_berror,hzscl,hswgt,pert_berr,pert_berr_fct,&
     bkgv_flowdep,bkgv_rewgtfct,bkgv_write,fpsproj,nhscrf,adjustozvar,fut2ps,cwcoveqqcov
  use m_berror_stats, only: usenewgfsberror
  use anberror, only: anisotropic,ancovmdl,init_anberror,npass,ifilt_ord,triad4, &
     binom,normal,ngauss,rgauss,anhswgt,an_vs,&
     grid_ratio,grid_ratio_p,an_flen_u,an_flen_t,an_flen_z, &
     rtma_subdomain_option,rtma_bkerr_sub2slab,nsmooth,nsmooth_shapiro,&
     pf2aP1,pf2aP2,pf2aP3,afact0,covmap,lreadnorm
  use compact_diffs, only: noq,init_compact_diffs
  use jcmod, only: init_jcvars,ljcdfi,alphajc,ljcpdry,bamp_jcpdry,eps_eer,ljc4tlevs,ljclimqc 
  use tendsmod, only: ctph0,stph0,tlm0
  use mod_vtrans, only: nvmodes_keep,init_vtrans
  use mod_strong, only: l_tlnmc,reg_tlnmc_type,nstrong,tlnmc_option,&
       period_max,period_width,init_strongvars,baldiag_full,baldiag_inc
  use gridmod, only: nlat,nlon,nsig,wrf_nmm_regional,nems_nmmb_regional,fv3_regional,cmaq_regional,fv3_cmaq_regional,&
     nmmb_reference_grid,grid_ratio_nmmb,grid_ratio_wrfmass,grid_ratio_fv3_regional,fv3_io_layout_y,&
     filled_grid,half_grid,wrf_mass_regional,nsig1o,nnnn1o,update_regsfc,&
     diagnostic_reg,gencode,nlon_regional,nlat_regional,nvege_type,&
     twodvar_regional,regional,init_grid,init_reg_glob_ll,init_grid_vars,netcdf,&
     nlayers,use_gfs_ozone,check_gfs_ozone_date,regional_ozone,jcap,jcap_b,vlevs,&
     use_gfs_nemsio,sfcnst_comb,use_readin_anl_sfcmask,use_sp_eqspace,final_grid_vars,&
     jcap_gfs,nlat_gfs,nlon_gfs,jcap_cut,wrf_mass_hybridcord,use_gfs_ncio,write_fv3_incr,&
     use_fv3_aero,grid_type_fv3_regional
  use gridmod,only: l_reg_update_hydro_delz,fv3_cmaq_regional
  use guess_grids, only: ifact10,sfcmod_gfs,sfcmod_mm5,use_compress,nsig_ext,gpstop,commgpstop,commgpserrinf
  use gsi_io, only: init_io,lendian_in,verbose,print_obs_para
  use regional_io_mod, only: regional_io_class
  use wrf_params_mod, only: update_pint, preserve_restart_date
  use constants, only: zero,one,init_constants,gps_constants,init_constants_derived,three
  use fgrid2agrid_mod, only: nord_f2a,init_fgrid2agrid,final_fgrid2agrid,set_fgrid2agrid
  use smooth_polcarf, only: norsp,init_smooth_polcas
  use read_l2bufr_mod, only: minnum,del_azimuth,del_elev,del_range,del_time,&
     range_max,elev_angle_max,initialize_superob_radar,l2superob_only,radar_sites,radar_box,radar_rmesh,radar_zmesh
  use m_berror_stats,only : berror_stats ! filename if other than "berror_stats"
  use lag_fields,only : infile_lag,lag_nmax_bal,&
                        &lag_vorcore_stderr_a,lag_vorcore_stderr_b,lag_modini
  use lag_interp,only : lag_accur
  use lag_traj,only   : lag_stepduration
  use hybrid_ensemble_parameters,only : l_hyb_ens,uv_hyb_ens,aniso_a_en,generate_ens,&
                         n_ens,nlon_ens,nlat_ens,jcap_ens,jcap_ens_test,oz_univ_static,&
                         regional_ensemble_option,fv3sar_ensemble_opt,merge_two_grid_ensperts, &
                         full_ensemble,pseudo_hybens,pwgtflg,&
                         beta_s0,beta_e0,s_ens_h,s_ens_v,init_hybrid_ensemble_parameters,&
                         readin_localization,write_ens_sprd,eqspace_ensgrid,grid_ratio_ens,&
                         readin_beta,use_localization_grid,use_gfs_ens,q_hyb_ens,i_en_perts_io, &
                         l_ens_in_diff_time,ensemble_path,ens_fast_read,sst_staticB,limqens, &
                         ntotensgrp,nsclgrp,naensgrp,ngvarloc,ntlevs_ens,naensloc, &
                         r_ensloccov4tim,r_ensloccov4var,r_ensloccov4scl,l_timloc_opt,&
                         vdl_scale,vloc_varlist,&
                         global_spectral_filter_sd,assign_vdl_nml,parallelization_over_ensmembers,l_mgbf_loc
  use hybrid_ensemble_parameters,only : l_both_fv3sar_gfs_ens,n_ens_gfs,n_ens_fv3sar,weight_ens_gfs,weight_ens_fv3sar
  use rapidrefresh_cldsurf_mod, only: init_rapidrefresh_cldsurf, &
                            dfi_radar_latent_heat_time_period,metar_impact_radius,&
                            metar_impact_radius_lowcloud,l_gsd_terrain_match_surftobs, &
                            l_metar_impact_radius_change, &
                            metar_impact_radius_max,metar_impact_radius_min,&
                            metar_impact_radius_max_height,metar_impact_radius_min_height,&
                            l_sfcobserror_ramp_t, l_sfcobserror_ramp_q, &
                            l_pbl_pseudo_surfobst,l_pbl_pseudo_surfobsq,l_pbl_pseudo_surfobsuv, &
                            pblh_ration,pps_press_incr,l_gsd_limit_ocean_q, &
                            l_pw_hgt_adjust, l_limit_pw_innov, max_innov_pct, &
                            l_cleansnow_warmts,l_conserve_thetaV,r_cleansnow_warmts_threshold, &
                            i_conserve_thetav_iternum,l_gsd_soiltq_nudge,l_cld_bld, cld_bld_hgt, &
                            build_cloud_frac_p, clear_cloud_frac_p,       &
                            l_hydrometeor_bkio,nesdis_npts_rad, & 
                            iclean_hydro_withRef,iclean_hydro_withRef_allcol, &
                            i_use_2mq4b,i_use_2mt4b,i_gsdcldanal_type,i_gsdsfc_uselist, &
                            i_lightpcp,i_sfct_gross,l_use_hydroretrieval_all,l_numconc,l_closeobs,&
                            i_coastline,i_gsdqc,qv_max_inc,ioption,l_precip_clear_only,l_fog_off,&
                            cld_bld_coverage,cld_clr_coverage,&
                            i_cloud_q_innovation,i_ens_mean,DTsTmax,&
                            i_T_Q_adjust,l_saturate_bkCloud,l_rtma3d,i_precip_vertical_check, &
                            corp_howv, hwllp_howv, corp_gust, hwllp_gust, oerr_gust
  use gsi_metguess_mod, only: gsi_metguess_init,gsi_metguess_final
  use gsi_chemguess_mod, only: gsi_chemguess_init,gsi_chemguess_final
  use tcv_mod, only: init_tcps_errvals,tcp_refps,tcp_width,tcp_ermin,tcp_ermax
  use chemmod, only : init_chem,berror_chem,berror_fv3_cmaq_regional,oneobtest_chem,&
       berror_fv3_sd_regional,&
       maginnov_chem,magoberr_chem,&
       oneob_type_chem,oblat_chem,&
       anowbufr_ext,&
       oblon_chem,obpres_chem,diag_incr,elev_tolerance,tunable_error,&
       in_fname,out_fname,incr_fname, &
       laeroana_gocart, l_aoderr_table, aod_qa_limit, luse_deepblue, lread_ext_aerosol, &
       laeroana_fv3cmaq,laeroana_fv3smoke,pm2_5_innov_threshold,pm2_5_urban_innov_threshold,pm2_5_bg_threshold,&
       crtm_aerosol_model,crtm_aerosolcoeff_format,crtm_aerosolcoeff_file, &
       icvt_cmaq_fv3, raod_radius_mean_scale,raod_radius_std_scale 

  use chemmod, only : wrf_pm2_5,aero_ratios
  use gfs_stratosphere, only: init_gfs_stratosphere,use_gfs_stratosphere,pblend0,pblend1
  use gfs_stratosphere, only: broadcast_gfs_stratosphere_vars
  use general_commvars_mod, only: init_general_commvars,destroy_general_commvars
  use radiance_mod, only: radiance_mode_init,radiance_mode_destroy, &
       radiance_obstype_destroy
  use gsi_nstcouplermod, only: gsi_nstcoupler_init_nml
  use gsi_nstcouplermod, only: nst_gsi,nstinfo,zsea1,zsea2,fac_dtl,fac_tsl
  use ncepnems_io, only: init_nems,imp_physics,lupp
  use wrf_vars_mod, only: init_wrf_vars,fed_exist,dbz_exist
  use gsi_rfv3io_mod,only : fv3sar_bg_opt
  use radarz_cst,            only: mphyopt, MFflg
  use radarz_iface,          only: init_mphyopt
  use directDA_radaruse_mod, only: init_radaruse_directDA
  use directDA_radaruse_mod, only: coef4dbzfwrd
  use directDA_radaruse_mod, only: oe_rw, oe_dbz, refl_lowbnd_rw, refl_lowbnd_dbz, &
                               be_sf, hscl_sf, vscl_sf, be_vp, hscl_vp, vscl_vp,   &
                               be_t,  hscl_t,  vscl_t,  be_q,  hscl_q,  vscl_q,    &
                               be_qr, be_qs, be_qg, hscl_qx, vscl_qx,              &
                               l_decouple_sf_vp, l_decouple_sf_tps,                &
                               l_set_be_rw, l_set_be_dbz,                          &
                               l_set_oerr_ratio_rw, l_set_oerr_ratio_dbz,          &
                               l_use_rw_columntilt, l_use_dbz_directDA,            &
                               rw_obs4wrd_bmwth, lvldbg,                           &
                               l_correct_azmu, l_correct_tilt, i_correct_tilt,     &
                               l_azm_east1st, l_use_cvpqx,                         &
                               cvpqx_pval,                                         &
                               l_plt_be_stats, l_be_T_dep, l_gpht2gmht,            &
                               l_plt_diag_rw, l_chk_bmwth,                         &
                               i_melt_snow, i_melt_graupel,                        &
                               cld_cv, cld_nt_updt,  i_w_updt,                     &
                               l_cvpnr, cvpnr_pval, l_use_tdep_radarz

  implicit none

  private

! !PUBLIC ROUTINES:

   public gsimain_initialize
   public gsimain_run
   public gsimain_finalize

!
! !DESCRIPTION: This module contains code originally in the GSI main program.
! The main
!               program has been split in initialize/run/finalize segments, and
!               subroutines
!  created for these steps: gsimain_initialize(), gsimain_run() and
!  gsimain_finalize().
!  In non-ESMF mode (see below) a main program is assembled by calling these 3
!  routines in
!  sequence.
                                                                                                                         
                    
!  This file can be compiled in 2 different modes: an ESMF and a non-ESMF mode.
!  When HAVE_ESMF
!  is defined (ESMF mode), a few I/O related statements are skipped during
!  initialize() and
!  a main program is not provided. These is no dependency on the ESMF in this
!  file and in the
!  routines called from here. The ESMF interface is implemented in
!  GSI_GridCompMod which in
!  turn calls the initialize/run/finalize routines defined here.
!
! !REVISION HISTORY:
!
!  01Jul2006  Cruz      Initial code.
!  19Oct2006  da Silva  Updated prologue.
!  10Apr2007  Todling   Created from gsimain
!  13Jan2007  Tremolet  Updated interface to setup_4dvar
!  03Oct2007  Todling   Add lobserver
!  03Oct2007  Tremolet  Add DFI and lanczos-save
!  04Jan2008  Tremolet  Add forecast sensitivity to observations options
!  10Sep2008  Guo       Add CRTM files directory path
!  02Dec2008  Todling   Remove reference to old TLM of analysis  
!  20Nov2008  Todling   Add lferrscale to scale OMF w/ Rinv (actual fcst not guess)
!  08Dec2008  Todling   Placed switch_on_derivatives,tendsflag in jcopts namelist
!  28Jan2009  Todling   Remove original GMAO interface
!  06Mar2009  Meunier   Add initialisation for lagrangian data
!  04-21-2009 Derber    Ensure that ithin is positive if neg. set to zero
!  07-08-2009 Sato      Update for anisotropic mode (global/ensemble based)
!  08-31-2009 Parrish   Add changes for version 3 regional tangent linear normal mode constraint
!  09-22-2009 Parrish   Add read of namelist/hybrid_ensemble/.  contains parameters used for hybrid
!                        ensemble option.
!  10-09-2009 Wu        replace nhr_offset with min_offset since it''s 1.5 hr for regional
!  02-17-2010 Parrish   add nlon_ens, nlat_ens, jcap_ens to namelist/hybrid_ensemble/, in preparation for 
!                         dual resolution capability when running gsi in hybrid ensemble mode.
!  02-20-2010 Zhu       Add init_anacv,nrf,nvars,nrf_3d for control variables;
!  02-21-2010 Parrish   add jcap_ens_test to namelist/hybrid_ensemble/ so can simulate lower resolution
!                         ensemble compared to analysis for case when ensemble and analysis resolution are
!                         the same.  used for preliminary testing of dual resolution hybrid ensemble option.
!  02-25-2010 Zhu       Remove berror_nvars
!  03-06-2010 Parrish   add flag use_gfs_ozone to namelist SETUP--allows read of gfs ozone for regional runs
!  03-09-2010 Parrish   add flag check_gfs_ozone_date to namelist SETUP--if true, date check gfs ozone
!  03-15-2010 Parrish   add flag regional_ozone to namelist SETUP--if true, then turn on ozone in 
!                         regional analysis
!  03-17-2010 todling   add knob for analysis error estimate (jsiga)
!  03-17-2010 Zhu       Add nc3d and nvars in init_grid_vars interface
!  03-29-2010 hu        add namelist variables for controling rapid refesh options
!                                 including cloud analysis and surface enhancement
!                       add and read namelist for RR
!  03-31-2010 Treadon   replace init_spec, init_spec_vars, destroy_spec_vars with general_* routines
!  04-07-2010 Treadon   write rapidrefresh_cldsurf settings to stdout
!  04-10-2010 Parrish   add vlevs from gridmod, so can pass as argument to init_mpi_vars, which must
!                        be called after init_grid_vars, as it currently is.  This must be done to
!                        avoid "use gridmod" in mpimod.F90, which causes compiler conflict, since
!                        "use mpimod" appears in gridmod.f90.
!  04-22-2010 Tangborn  add carbon monoxide settings
!  04-25-2010 Zhu       Add option newpc4pred for new pre-conditioning of predictors
!  05-05-2010 Todling   replace parallel_init w/ corresponding from gsi_4dcoupler
!  05-06-2010 Zhu       Add option adp_anglebc for radiance variational angle bias correction;
!                       npred was removed from setup namelist
!  05-12-2010 Zhu       Add option passive_bc for radiance bias correction for monitored channels
!  05-30-2010 Todling   reposition init of control and state vectors; add init_anasv; update chem
!  06-04-2010 Todling   update interface to init_grid_vars
!  06-05-2010 Todling   remove as,tsfc_sdv,an_amp0 from bkgerr namelist (now in anavinfo table)
!  08-10-2010 Wu        add nvege_type to gridopts namelist 
!  08-24-2010 hcHuang   add diag_aero and init_aero for aerosol observations
!  08-26-2010 Cucurull  add use_compress to setup namelist, add a call to gps_constants
!  09-06-2010 Todling   add Errico-Ehrendorfer parameter for E-norm used in DFI
!  09-03-2010 Todling   add opt to output true J-cost from within Lanczos (beware: expensive)
!  10-05-2010 Todling   add lbicg option
!  09-02-2010 Zhu       Add option use_edges for the usage of radiance data on scan edges
!  10-18-2010 hcHuang   Add option use_gfs_nemsio to read global model NEMS/GFS first guess
!  11-17-2010 Pagowski  add chemical species and related namelist
!  12-20-2010 Cucurull  add nsig_ext to setup namelist for the usage of gpsro bending angle
!  01-05-2011 Cucurull  add gpstop to setup namelist for the usage of gpsro data assimilation
!  04-08-2011 Li        (1) add integer variable nst_gsi and nstinfo for the use of oceanic first guess
!                       (2) add integer variable fac_dtl & fac_tsl to control the use of NST model
!                       (3) add integer variable tzr_qc to control the Tzr QC
!                       (4) add integer tzr_bufrsave to control if save Tz retrieval or not
!  04-07-2011 todling   move newpc4pred to radinfo
!  04-19-2011 El Akkraoui add iorthomax to control numb of vecs in orthogonalization for CG opts
!  05-05-2011 mccarty   removed references to repe_dw
!  05-21-2011 todling   add call to setservice
!  06-01-2011 guo/zhang add liauon
!  07-27-2011 todling   add use_prepb_satwnd to control usage of satwnd''s in prepbufr files
!  08-15-2011 gu/todling add pseudo-q2 option
!  09-10-2011 parrish   add use_localization_grid to handle (global) non-gaussian ensemble grid
!  09-14-2011 parrish/todling   add use_sp_eqspace for handling lat/lon grids
!  09-14-2011 todling   add use_gfs_ens to control global ensemble; also use_localization_grid
!  11-14-2011  wu       add logical switch to use extended forward model for sonde data
!  01-16-2012 m. tong   add parameter pseudo_hybens to turn on pseudo ensemble hybrid
!  01-17-2012 wu        add switches: gefs_in_regional,full_ensemble,pwgtflg
!  01-18-2012 parrish   add integer parameter regional_ensemble_option to select ensemble source.
!                                 =1: use GEFS internally interpolated to ensemble grid.
!                                 =2: ensembles are WRF NMM format.
!                                 =3: ensembles are ARW netcdf format.
!                                 =4: ensembles are NEMS NMMB format.
!  02-07-2012 tong      remove parameter gefs_in_regional and reduce regional_ensemble_option to
!                       4 options
!  02-08-2012 kleist    add parameters to control new 4d-ensemble-var features.
!  02-17-2012 tong      add parameter merge_two_grid_ensperts to merge ensemble perturbations
!                       from two forecast domains to analysis domain  
!  05-25-2012 li/wang   add TDR fore/aft sweep separation for thinning,xuguang.wang@ou.edu
!  06-12-2012 parrish   remove calls to subroutines init_mpi_vars, destroy_mpi_vars.
!                       add calls to init_general_commvars, destroy_general_commvars.
!  10-11-2012 eliu      add wrf_nmm_regional in determining logic for use_gfs_stratosphere                
!  05-14-2012 wargan    add adjustozvar to adjust ozone in stratosphere
!  05-14-2012 todling   defense to set nstinfo only when nst_gsi>0
!  05-23-2012 todling   add lnested_loops option
!  09-10-2012 Gu        add fut2ps to project unbalanced temp to surface pressure in static B modeling
!  12-05-2012 el akkraoui  hybrid beta parameters now vertically varying
!  07-10-2012 sienkiewicz  add ssmis_method control for noise reduction
!  02-19-2013 sienkiewicz  add ssmis_precond for SSMIS bias coeff weighting
!  04-15-2013 zhu       add aircraft_t_bc_pof and aircraft_t_bc for aircraft temperature bias correction
!  04-24-2013 parrish   move calls to subroutines init_constants and
!                       gps_constants before convert_regional_guess
!                       so that rearth is defined when used
!  05-07-2013 tong      add tdrerr_inflate for tdr obs err inflation and
!                       tdrgross_fact for tdr gross error adjustment
!  05-31-2013 wu        write ext_sonde output to standard out
!  07-02-2013 parrish   change tlnmc_type to reg_tlnmc_type.  tlnmc_type no
!                         longer used for global analysis.  
!                         for regional analysis, reg_tlnmc_type=1 or 2 for two
!                         different regional balance methods.
!  07-10-2013 zhu       add upd_pred as bias update indicator for radiance bias correction
!  07-19-2013 zhu       add emiss_bc for emissivity predictor in radiance bias correction scheme
!  08-20-2013 s.liu     add option to use reflectivity
!  09-27-2013 todling   redefine how instrument information is read into code (no longer namelist)
!  10-26-2013 todling   add regional_init; revisit init of aniso-filter arrays;
!                       revisit various init/final procedures
!  10-30-2013 jung      added clip_supersaturation to setup namelist
!  12-02-2013 todling   add call to set_fgrid2agrid
!  12-03-2013 Hu        add parameter grid_ratio_wrfmass for analysis on larger
!                              grid than mass background grid
!  12-10-2013 zhu       add cwoption
!  02-05-2014 todling   add parameter cwcoveqqcov (cw_cov=q_cov)
!  02-24-2014 sienkiewicz added aircraft_t_bc_ext for GMAO external aircraft temperature bias correction
!  05-29-2014 Thomas    add lsingleradob logical for single radiance ob test
!                       (originally of mccarty)
!  06-19-2014 carley/zhu  add factl and R_option for twodvar_regional lcbas/ceiling analysis
!  08-05-2014 carley    add safeguard so that oneobtest disables hilbert_curve if user accidentally sets hilbert_curve=.true.
!  10-04-2014 todling   revised meanning of parameter bcoption
!  08-18-2014 tong      add jcap_gfs to allow spectral transform to a coarser resolution grid,
!                       when running with use_gfs_ozone = .true. or use_gfs_stratosphere = .true. for
!                       regional analysis
!  10-07-2014 carley    added buddy check options under obsqc
!  11-12-2014 pondeca   must read in from gridopts before calling obsmod_init_instr_table. swap order
!  01-30-2015 Hu        added option i_en_perts_io,l_ens_in_diff_time under hybrid_ensemble
!  01-15-2015 Hu        added options i_use_2mq4b,i_use_2mt4b, i_gsdcldanal_type
!                              i_gsdsfc_uselist,i_lightpcp,i_sfct_gross under
!                              rapidrefresh_cldsurf
!  02-09-2015 Sienkiewicz id_drifter flag - modify KX values for drifting buoys if set
!  02-29-2015 S.Liu     added option l_use_hydroretrieval_all
!  03-01-2015 Li        add zsea1 & zsea2 to namelist for vertical mean temperature based on NSST T-Profile
!  05-02-2015 Parrish   add option rtma_bkerr_sub2slab to allow dual resolution for application of
!                       anisotropic recursive filter (RTMA application only for now).
!  05-13-2015 wu        remove check to turn off regional 4densvar
!  01-13-2015 Ladwig    added option l_numconc
!  09-01-2015 Hu        added option l_closeobs
!  10-01-2015 guo       option to redistribute observations in 4d observer mode
!  07-20-2015 zhu       re-structure codes for enabling all-sky/aerosol radiance assimilation, 
!                       add radiance_mode_init, radiance_mode_destroy & radiance_obstype_destroy
!  01-28-2016 mccarty   add netcdf_diag capability
!  03-02-2016 s.liu/carley - remove use_reflectivity and use i_gsdcldanal_type
!  03-10-2016 ejones    add control for gmi noise reduction
!  03-25-2016 ejones    add control for amsr2 noise reduction
!  04-18-2016 Yang      add closest_obs for selecting obs. from multi-report at a surface observation.
!  06-24-2016 j. guo    added alwaysLocal => m_obsdiags::obsdiags_alwaysLocal to
!                       namelist /SETUP/.
!  08-12-2016 lippi     added namelist parameters for single radial wind
!                       experiment (anaz_rw,anel_rw,range_rw,sstn,lsingleradar,
!                       singleradar,learthrel_rw). added a radar station look-up
!                       table.
!  08-12-2016 Mahajan   NST stuff belongs in NST module, Adding a NST namelist
!                       option
!  08-24-2016 lippi     added nml option lnobalance to zero out all balance correlation
!                       matricies for univariate analysis.
!  08-28-2016 li - tic591: add use_readin_anl_sfcmask for consistent sfcmask
!                          between analysis grids and others
!  11-29-2016 shlyaeva  add lobsdiag_forenkf option for writing out linearized
!                       H(x) for EnKF
!  12-14-2016 lippi     added nml variable learthrel_rw for single radial
!                       wind observation test, and nml option for VAD QC
!                       vadwnd_l2rw_qc of level 2 winds.
!  02-02-2017 Hu        added option i_coastline to turn on the observation
!                              operator for surface observations along the coastline area
!  04-01-2017 Hu        added option i_gsdqc to turn on special observation qc
!                              from GSD (for RAP/HRRR application)
!  02-15-2016 Y. Wang, Johnson, X. Wang - added additional options if_vterminal, if_model_dbz,
!                                         for radar DA, POC: xuguang.wang@ou.edu
!  08-31-2017 Li        add sfcnst_comb for option to read sfc & nst combined file 
!  10-10-2017 Wu,W      added option fv3_regional and rid_ratio_fv3_regional, setup FV3, earthuv
!  01-11-2018 Yang      add namelist variables required by the nonlinear transform to vis and cldch
!                      (Jim Purser 2018). Add estvisoe and estcldchoe to replace the hardwired 
!                       prescribed vis/cldch obs. errort in read_prepbufr. (tentatively?)
!  03-22-2018 Yang      remove "logical closest_obs", previously applied to the analysis of vis and cldch.
!                       The option to use only the closest ob to the analysis time is now handled
!                       by Ming Hu's "logical l_closeobs" for all variables.
!  01-04-2018 Apodaca   add diag_light and lightinfo for GOES/GLM lightning
!                           data assimilation
!  08-16-2018 akella    id_ship flag - modify KX values for ships if set
!  08-25-2018 Collard   Introduce bias_zero_start
!  03-29-2019 lei       add integer parameter fv3sar_ensemble_opt to select the format of the FV3SAR ensembles 
!                                 =0;  restart files
!                                 =1;  cold start IC files from CHGRES
!  09-12-2018 Ladwig    added option l_precip_clear_only
!  03-28-2019 Ladwig    merging additional options for cloud product assimilation
!  03-11-2019 Collard   Introduce ec_amv_qc as temporary control of GOES-16/17 AMVS
!  03-14-2019 eliu      add logic to turn on using full set of hydrometeors in
!                       obs operator and analysis
!  03-14-2019 eliu      add precipitation component 
!  05-09-2019 mtong     move initializing derivative vector here
!  06-19-2019 Hu        Add option reset_bad_radbc for reseting radiance bias correction when it is bad
!  06-25-2019 Hu        Add option print_obs_para to turn on OBS_PARA list
!  07-09-2019 Todling   Introduce cld_det_dec2bin and diag_version
!  07-11-2019 Todling   move vars imp_physics,lupp from CV to init_nems
!  07-29-2019 pondeca   add logical variable "neutral_stability_windfact_2dvar" that provides option to use a simple,
!                       similarity theory-based approach to compute the 10-m wind factor for
!                       near-surface observations
!  08-14-2019 W. Gu     add lupdqc to replace the obs errors from satinfo with diag of est(R)
!  08-14-2019 W. Gu     add lqcoef to combine the inflation coefficients generated by qc with est(R)
!  08-23-2019 pondeca   add logical variable "use_similarity_2dvar" that provides option to use
!                       similarity theory from the mm5 sfc model to compute the 10-m wind factor for
!                       near-surface observations
!  09-04-2019 Martin    Add option write_fv3_incr to write netCDF increment rather than NEMSIO analysis
!  09-13-2019 Martin    Add option incvars_to_zero(nvars) to zero out netCDF increment fields
!  09-20-2019 Su        add new variational QC and hub norm option
!  09-23-2019 Martin    Add option use_gfs_ncio to read in first-guess netCDF file
!  10-15-2019 Wei/Martin   added option lread_ext_aerosol to read in aerfXX file for NEMS aerosols;
!                          added option use_fv3_aero to choose between NGAC and FV3GFS-GSDChem 
!  10-28-2019 Martin    Add option incvars_zero_strat(nvars) to zero out increments above tropopause
!                          added option use_fv3_aero to choose between NGAC and FV3GFS-GSDChem
!  01-27-2020 Winterbottom Moved regression coeffcients for regional
!                          model (e.g., HWRF) aircraft recon dynamic
!                          observation error (DOE) specification to
!                          GSI namelist level (beneath obsmod.F90).
!  09-15-2020 Wu        Add option tcp_posmatch to mitigate possibility of erroneous TC initialization
!  2021-01-05  x.zhang/lei  - add code for updating delz analysis in regional da
!  09-07-2020 CAPS            Add options for directDA_radaruse_mod to use direct radar DA capabilities
!  02-09-2021 CAPS(J. Park)   Add vad_near_analtime flag (obsqc) to assimilate newvad obs around analysis time only
!  10-10-2019 Zhao      added options l_rtma3d and l_precip_vertical_check
!                       (adjustment to the cloud-analysis retrieved profile of
!                        Qg/Qs/Qr/QnrQto to alleviate the reflectivity ghost in
!                        RTMA3D.)
!  04-16-2020 Zhao      change option l_precip_vertical_check to i_precip_vertical_check
!                       option for checking and adjusting the profile of Qr/Qs/Qg/Qnr
!                       retrieved through cloud analysis to reduce the background
!                       reflectivity ghost in analysis. (default is 0)
!  2021-11-16 Zhao    - add option l_obsprvdiag (if true) to trigger the output of
!                       observation provider and sub-provider information into
!                       obsdiags files (used for AutoObsQC)
!  01-07-2022 Hu        Add fv3_io_layout_y to let fv3lam interface read/write subdomain restart
!                       files. The fv3_io_layout_y needs to match fv3lam model
!                       option io_layout(2).
!  05-24-2022 H.Wang    Add PM2.5 and AOD DA for regional FV3-CMAQ (RRFS-CMAQ).
!                       GSI will perform aerosol analysis when 
!                           1. laeroana_fv3cmaq =  .true.
!                           2. fv3_regional =      .true.  
!                           3. fv3_cmaq_regional = .true. 
!                           4. berror_fv3_cmaq_regional = .true. 
!  09-02-2022 Jung      Added namelist entries to call a new IR cloud detection routine
!                       the original cloud detection routine is the default.  To use the new 
!                       cloud detection routine, set the flags to .true.
!  09-15-2022 yokota  - add scale/variable/time-dependent localization
!  2023-07-30 Zhao    - added namelist options for analysis of significant wave height
!                       (aka howv in GSI code): corp_howv, hwllp_howv
!                       (in namelist session rapidrefresh_cldsurf)
!  
!  2023-09-14 H. Wang - add namelist option for FED EnVar DA. 
!                        - if_model_fed=.true.        :  FED in background and ens. If
!                          perform FED DA, this has to be true along with fed in
!                          control/analysis and metguess vectors. If only run GSI observer,
!                          it can be false.
!                        - innov_use_model_fed=.true. :  Use FED from BG to calculate innovation.
!                          this requires if_model_fed=.true. 
!                          it works either an EnVar DA run or a GSI observer run.
!  02-20-2024 yokota  - add MGBF-based localization
!
!EOP
!-------------------------------------------------------------------------

! Declare variables.
  logical:: writediag,l_foto
  integer(i_kind) i,ngroup

  integer(i_kind):: iret_init_mphyopt
  integer(i_kind):: iret_coef4dbzfwrd

! Declare namelists with run-time gsi options.
!
! Namelists:  setup,gridopts,jcopts,bkgerr,anbkgerr,obsqc,obs_input,
!             singleob_test,superob_radar,emissjac,chem,nst
!
! SETUP (general control namelist) :
!
!     gencode  - source generation code
!     factqmin - weighting factor for negative moisture constraint
!     factqmax - weighting factor for supersaturated moisture constraint
!     superfact- amount of supersaturation allowed 1.01 = 1% supersaturation
!     limitqobs- limit q obs to be <= 100%RH based on model temperatures
!     clip_supersaturation - flag to remove supersaturation during each outer loop default=.false.
!     deltim   - model timestep
!     dtphys   - physics timestep
!     biascor  - background error bias correction coefficient
!     bcoption - =0:do-nothing; =1:sibc; when <0 will estimate but not correct bkg bias
!     diurnalbc- 1= diurnal bias; 0= persistent bias
!     niter()  - number of inner interations for each outer iteration
!     niter_no_qc() - number of inner interations without nonlinear qc for each outer iteration
!     miter    - number of outer iterations
!     qoption  - option of analysis variable; 1:q/qsatg-bkg 2:norm RH
!     pseudo_q2- breed between q1/q2 options, that is, (q1/sig(q))
!     fstat    - logical to seperate f from balance projection
!     nhr_assimilation - assimilation time interval (currently 6hrs for global, 3hrs for reg)
!     min_offset       - time in minutes of analysis in assimilation window (default 3 hours)
!     l4dvar           - turn 4D-Var on/off (default=off=3D-Var)
!     liauon           - treat 4dvar CV as tendency perturbation (default=false)
!     lnested_loops    - allow for nested resolution outer/inner loops
!     jsiga            - calculate approximate analysis errors from lanczos for jiter=jsiga
!     idmodel          - uses identity model when running 4D-Var (test purposes)
!     iwrtinc          - when >0, writes out increments from iwrtinc-index slot
!     nhr_obsbin       - length of observation bins
!     nhr_subwin       - length of weak constraint 4d-Var sub-window intervals
!     iout_iter- output file number for iteration information
!     npredp   - number of predictors for precipitation bias correction
!     retrieval- logical to turn off or on the SST physical retrieval
!     tzr_qc  - indicator to control the Tzr_QC mode: 0 = no Tz retrieval;
!                                                     1 = Do Tz retrieval and applied to QC
!     tzr_bufrsave - logical to turn off or on the bufr Tz retrieval file true=on
!     diag_rad - logical to turn off or on the diagnostic radiance file true=on
!     diag_conv-logical to turn off or on the diagnostic conventional file (true=on)
!     diag_ozone - logical to turn off or on the diagnostic ozone file (true=on)
!     diag_aero  - logical to turn off or on the diagnostic aerosol file (true=on)
!     diag_co - logical to turn off or on the diagnostic carbon monoxide file (true=on)
!     diag_light - logical to turn off or on the diagnostic lightning file (true=on)
!     diag_radardbz - logical to turn off or on the diagnostic radar reflectivity file (true=on)
!     diag_fed - logical to turn off or on the diagnostic flash extent density file (true=on)
!     write_diag - logical to write out diagnostic files on outer iteration
!     lobsdiagsave - write out additional observation diagnostics
!     ltlint       - linearize inner loop
!     lobskeep     - keep obs from first outer loop for subsequent OL
!     lobsensfc    - compute forecast sensitivity to observations
!     lobsensjb    - compute Jb sensitivity to observations
!     lobsensincr  - compute increment sensitivity to observations
!     lobsensadj   - use adjoint of approx. Hessian to compute obs sensitivity
!     llancdone    - use to tell adjoint that Lanczos vecs have been pre-computed
!     lsensrecompute - does adjoint by recomputing forward solution
!     lobsensmin   - use minimisation to compute obs sensitivity
!     lbicg        - use B-precond w/ bi-conjugate gradient for minimization
!     iobsconv     - compute convergence test in observation space
!                     =1 at final point, =2 at every iteration
!     lobserver    - when .t., calculate departure vectors only
!     lanczosave   - save lanczos vectors for forecast sensitivity computation
!     ltcost       - calculate true cost when using Lanczos (this is very expensive)
!     lferrscale   - apply H^TR^{-1}H to a forecast error vector read on the fly
!     iguess   - flag for guess solution (currently not working)
!                iguess = -1  do not use guess file
!                iguess =  0  write only guess file
!                iguess =  1  read and write guess file
!                iguess =  2  read only guess file
!     oneobtest- one ob test flag true=on
!     switch_on_derivatives - if true, then compute horizontal derivatives of all state variables
!                           (to be used eventually for time derivatives, dynamic constraints,
!                            and observation forward models that need horizontal derivatives)
!     tendsflag - if true, compute time tendencies
!     sfcmodel - if true, then use boundary layer forward model for surface temperature data.
!     dtbduv_on - if true, use d(microwave brightness temperature)/d(uv wind) in inner loop
!     ifact10 - flag for recomputing 10m wind factor
!               ifact10 = 1 compute using GFS surface physics
!               ifact10 = 2 compute using MM5 surface physics
!               ifact10 = 0 or any other value - DO NOT recompute - use value from guess file
!     offtime_data - if true, then allow use of obs files with ref time different
!                        from analysis time.  default value = .false., in which case
!                        analysis fails if obs file ref time is different from analysis time.
!
!     perturb_obs - logical flag to perutrb observation (true=on)
!     tcp_posmatch - integer =1 to move TC to guess position,
!                            =2 set pges to the minimum Psfc 
!     tcp_box      - integer to define the search box size in gridpoints, default =5 
!                    used with option tcp_posmatch
!     oberror_tune - logical flag to tune oberror table  (true=on)
!     perturb_fact -  magnitude factor for observation perturbation
!     crtm_coeffs_path - path of directory w/ CRTM coeffs files
!     print_diag_pcg - logical turn on of printing of GMAO diagnostics in pcgsoi.f90
!     preserve_restart_date - if true, then do not update regional restart file date.
!     tsensible - option to use sensible temperature as the analysis variable. works
!                 only for twodvar_regional=.true.
!     hilbert_curve - option for hilbert-curve based cross-validation. works only
!                     with twodvar_regional=.true.
!     neutral_stability_windfact_2dvar - option to use simple, similarity
!                                        theory-based approach to compute 10-m wind factor
!     use_similarity_2dvar - option to use similarity theory from the mm5 sfc model
!                            to compute 10-m wind factor
!     lread_obs_save - option to write out collective obs selection info
!     lread_obs_skip - option to read in collective obs selection info
!     use_gfs_ozone  - option to read in gfs ozone and interpolate to regional model domain
!     check_gfs_ozone_date  - option to date check gfs ozone before interpolating to regional model domain
!     regional_ozone  - option to turn on ozone in regional analysis
!     lwrite_predterms - option to write out actual predictor terms instead of predicted bias to the
!                        radiance diagnostic files
!     lwrite_peakwt    - option to writ out the approximate pressure of the peak of the weighting function
!                        for satellite data to the radiance diagnostic files
!     adp_anglebc - option to perform variational angle bias correction
!     angord      - order of polynomial for variational angle bias correction
!     newpc4pred  - option for additional preconditioning for pred coeff.
!     passive_bc  - option to turn on bias correction for passive (monitored) channels
!     reset_bad_radbc - option to turn on reseting bias correction coefficient when it is bad
!     use_edges   - option to exclude radiance data on scan edges
!     biaspredvar - set background error variance for radiance bias coeffs
!     (default 0.1K)
!     use_compress - option to turn on the use of compressibility factors in geopotential heights
!     nsig_ext - number of layers above the model top which are necessary to compute the bending angle for gpsro
!     gpstop - maximum height for gpsro data assimilation. Reject anything above this height. 
!     commgpstop -Reject commercial ro above this height. Logic in setupbend assumes commgpstop <= gpstop.
!     commgpserrinf - optional error inflation factor for commercial gpsro data
!     use_gfs_nemsio  - option to use nemsio to read global model NEMS/GFS first guess
!     use_gfs_ncio - option to use netCDF to read global model FV3-GFS first guess
!     use_fv3_aero - option to use FV3-Chem vs NGAC for global aerosol analysis
!     sfcnst_comb   - option to use nemsio sfc history file by regriding FV3 grid
!     use_readin_anl_sfcmask  - option to use readin surface mask
!     use_prepb_satwnd - allow using satwnd''s from prepbufr (historical) file
!     id_drifter  -  option to identify drifting buoy observations (modify KX from 180/280)
!     id_ship     -  option to identify ship          observations (modify KX from 180)
!     use_gfs_stratosphere - for now, can only be set to true if nems_nmmb_regional=true.  Later extend
!                             to other regional models.  When true, a guess gfs valid at the same time
!                             as the nems-nmmb guess is used to replace the upper levels with gfs values.
!                             The nems-nmmb vertical coordinate is smoothly merged between pressure values
!                             pblend0,pblend1 so that below pblend0 the vertical coordinate is the original
!                             nems-nmmb, and above pblend1 it becomes the gfs vertical coordinate.  For
!                             the current operational nems-nmmb and gfs vertical coordinates and
!                             pblend0=152mb, pblend1=79mb, the merged nems-nmmb/gfs vertical coordinate
!                             has 75 levels compared to nems-nmmb original 60 levels.  The purpose of this
!                             is to allow direct use of gdas derived sat radiance bias correction coefs,
!                             since it has been determined that height of top level and stratosphere
!                             resolution are key to successful assimilation of most channels.
!                                   (NOTE: I have not actually verified this statement yet!)
!     pblend0,pblend1 - see above comment for use_gfs_stratosphere
!     l4densvar - logical to turn on ensemble 4dvar
!     ens_nstarthr - start hour for ensemble perturbations (generally should match min_offset)
!     lwrite4danl - logical to write out 4d analysis states if 4dvar or 4denvar mode
!     nhr_anal - forecast hours to write out if lwrite4danal=T
!     ladtest -  if true, doing the adjoint test for the operator that maps
!                    control_vector to the model state_vector
!     ladtest_obs -  if true, doing the adjoint adjoint check for the
!                     observation operators that are currently used in the NCEP GSI variational
!                     analysis scheme
!     lrun_subdirs - logical to toggle use of subdirectires at runtime for pe specific files
!     mpes_observer - informs Solver number of PEs used to run Observer
!     emiss_bc    - option to turn on emissivity bias predictor
!     lsingleradob - logical for single radiance observation assimilation.
!                   Uses existing bufr file and rejects all radiances that don''t fall within a tight threshold around
!                   oblat/oblon (SINGLEOB_TEST)
!
!     ssmis_method - choose method for SSMIS noise reduction 0=no smoothing 1=default
!     ssmis_precond - weighting factor for SSMIS preconditioning (if not using newpc4pred)
!     gmi_method - choose method for GMI noise reduction. 0=no smoothing, 4=default
!     amsr2_method - choose method for AMSR2 noise reduction. 0=no smoothing, 5=default
!     bias_zero_start - Initialise bias correction from zero (default=true,
!                        false=mode start method)
!     ec_amv_qc - If true use additional QC from ECMWF addressing issues with
!                         upper level GOES-16/17 winds (default = true)
!     R_option   - Option to use variable correlation length for lcbas based on data
!                    density - follows Hayden and Purser (1995) (twodvar_regional only)
!     thin4d - if true, removes thinning of observations due to the location in
!              the time window
!     lobsdiag_forenkf - if true, save linearized H operator (jacobian) in
!     diagnostic file on 1st outer iteration.  The Jacobian can then be used by
!     the EnKF to compute ensemble perturbations in observation space.
!     luse_obsdiag - use obsdiags (useful when running EnKF observers; e.g., echo Jo table) 
!     imp_physics - type of GFS microphysics
!     lupp - if T, UPP is used and extra variables are output
!     lcalc_gfdl_cfrac - if T, calculate and use GFDL cloud fraction in observation operator 
!     cao_check - if T, turn on cold-air-outbreak screening for quality control
!     binary_diag - trigger binary diag-file output (being phased out)
!     netcdf_diag - trigger netcdf diag-file output
!     write_fv3_incr - trigger writing out FV3 netCDF increment file
!                      rather than NEMSIO analysis
!     incvars_to_zero - list of strings of variable names in FV3 netCDF
!                       increment file that should be forced to be zero
!     incvars_zero_strat - list of strings of variable names in FV3 netcdf
!                          increment file that will be reduced to zero
!                          above the tropopause
!     incvars_efold - scale factor x in which e^(-(k-ktrop)/x) for above fields 
!
!     diag_version - specifies desired version of diag files
!     l_wcp_cwm - namelist logical whether to use swcp/lwcp operator that includes cwm
!     aircraft_recon - namelist logical whether to apply DOE to aircraft data
!     tau_fcst - controls EFSOI-like calculation
!     efsoi_order - sets order of EFSOI-like calculation
!     lupdqc - logical to replace the obs errors from satinfo with diag of est(R) in the case of correlated obs
!     lqcoef - logical to combine the inflation coefficients generated by qc with est(R)
!     ta2tb - logical to use brightness temperature (SDR) instead of antenna
!             temperature (TDR) for assimilation
!     l_use_rw_columntilt - option to assimilate radar column-tilt radial wind obs in GSI 
!                     (.TRUE.: on; .FALSE.: off) / Inputfile: l2rwbufr_cltl (bufr format)
!     l_use_dbz_directDA - option to assimilate radar reflectivity obs directly in GSI 
!                     (.TRUE.: on; .FALSE.: off) / Inputfile: dbzbufr (bufr format)
!     l_obsprvdiag - trigger (if true) writing out observation provider and sub-provider
!                    information into obsdiags files (used for AutoObsQC)
!     optconv - downweighting option for iasi and cris for moisture channels to
!     improve convergence.  default 0.0 (no change).  Larger number improves
!     convergence.
!     inflate_dbz_obserr - logical that controls inflation of reflectivity ob error
!                          for obs that exceed gross error magnitude
!                          if true, inflate ob error
!                          if false, reject ob
!
!     NOTE:  for now, if in regional mode, then iguess=-1 is forced internally.
!            add use of guess file later for regional mode.

  namelist/setup/gencode,factqmin,factqmax,superfact,limitqobs,clip_supersaturation, &
       factql,factqi,factqr,factqs,factqg, &     
       factv,factl,factp,factg,factw10m,facthowv,factcldch,R_option,deltim,dtphys,&
       biascor,bcoption,diurnalbc,&
       neutral_stability_windfact_2dvar,use_similarity_2dvar,&
       niter,niter_no_qc,miter,qoption,cwoption,nhr_assimilation,&
       min_offset,pseudo_q2,&
       iout_iter,npredp,retrieval,&
       tzr_qc,tzr_bufrsave,&
       diag_rad,diag_pcp,diag_conv,diag_ozone,diag_aero,diag_co,diag_light,diag_radardbz,diag_fed, &
       iguess,write_diag,reduce_diag, &
       oneobtest,sfcmodel,dtbduv_on,ifact10,l_foto,offtime_data,&
       use_pbl,use_compress,nsig_ext,gpstop,commgpstop, commgpserrinf, &
       perturb_obs,perturb_fact,oberror_tune,preserve_restart_date, &
       crtm_coeffs_path,berror_stats,tcp_posmatch,tcp_box, &
       newpc4pred,adp_anglebc,angord,passive_bc,use_edges,emiss_bc,upd_pred,reset_bad_radbc,&
       ssmis_method, ssmis_precond, gmi_method, amsr2_method, bias_zero_start, &
       ec_amv_qc, lobsdiagsave, lobsdiag_forenkf, &
       l4dvar,lbicg,lsqrtb,lcongrad,lbfgsmin,ltlint,nhr_obsbin,nhr_subwin,&
       mPES_observer,&
       alwaysLocal,&
       use_fv3_aero,&
       nwrvecs,iorthomax,ladtest,ladtest_obs, lgrtest,lobskeep,lsensrecompute,jsiga,ltcost, &
       lobsensfc,lobsensjb,lobsensincr,lobsensadj,lobsensmin,iobsconv, &
       idmodel,iwrtinc,lwrite4danl,nhr_anal,jiterstart,jiterend,lobserver,lanczosave,llancdone, &
       lferrscale,print_diag_pcg,tsensible,lread_obs_save,lread_obs_skip, &
       use_gfs_ozone,check_gfs_ozone_date,regional_ozone,lwrite_predterms,&
       lwrite_peakwt,use_gfs_nemsio,use_gfs_ncio,sfcnst_comb,liauon,use_prepb_satwnd,l4densvar,ens_nstarthr,&
       use_gfs_stratosphere,pblend0,pblend1,step_start,diag_precon,lrun_subdirs,&
       use_sp_eqspace,lnested_loops,lsingleradob,thin4d,use_readin_anl_sfcmask,&
       luse_obsdiag,id_drifter,id_ship,verbose,print_obs_para,lsingleradar,singleradar,lnobalance, &
       inflate_dbz_obserr,missing_to_nopcp,minobrangedbz,minobrangedbz,maxobrangedbz,&
       maxobrangevr,maxtiltvr,whichradar,doradaroneob,dofedoneob,oneoblat,&
       oneoblon,oneobheight,oneobvalue,oneobddiff,oneobradid,&
       rmesh_vr,zmesh_dbz,zmesh_vr, ntilt_radarfiles, whichradar,&
       radar_no_thinning,ens_hx_dbz_cut,static_gsi_nopcp_dbz,rmesh_dbz,&
       minobrangevr, maxtiltdbz, mintiltvr,mintiltdbz,if_vterminal,if_vrobs_raw,if_use_w_vr,&
       if_model_dbz,if_model_fed,innov_use_model_fed,imp_physics,lupp,netcdf_diag,binary_diag,l_wcp_cwm,aircraft_recon,diag_version,&
       write_fv3_incr,incvars_to_zero,incvars_zero_strat,incvars_efold,diag_version,&
       cao_check,lcalc_gfdl_cfrac,tau_fcst,efsoi_order,lupdqc,lqcoef,cnvw_option,l2rwthin,hurricane_radar,&
       l_reg_update_hydro_delz, l_obsprvdiag,&
       l_use_dbz_directDA, l_use_rw_columntilt, ta2tb, optconv, &
       r_hgt_fed

! GRIDOPTS (grid setup variables,including regional specific variables):
!     jcap     - spectral resolution
!     nsig     - number of sigma levels
!     nlat     - number of latitudes
!     nlon     - number of longitudes
!     hybrid   - logical hybrid data file flag true=hybrid
!     nlon_regional - 
!     nlat_regional
!     diagnostic_reg - logical for regional debugging
!     update_regsfc - logical to write out updated surface fields to the
!                     regional analysis file (default = false)
!     netcdf            - if true, then wrf files are in netcdf format,
!                       -   otherwise wrf files are in binary format.
!     regional          - logical for regional GSI run
!     wrf_nmm_regional  - logical for input from WRF NMM
!     fv3_regional      - logical for input from FV3 regional
!     wrf_mass_regional - logical for input from WRF MASS-CORE
!     cmaq_regional     - logical for input from CMAQ
!     nems_nmmb_regional- logical for input from NEMS NMMB
!     nmmb_reference_grid= 'H', then analysis grid covers H grid domain
!                                = 'V', then analysis grid covers V grid domain
!     grid_ratio_nmmb   - ratio of analysis grid to nmmb model grid in nmmb model grid units.
!     grid_ratio_fv3_regional - ratio of analysis grid to fv3 grid in fv3 grid units.
!     fv3_io_layout_y    - set to the same number as io_layout of fv3 regional model in y direction.
!     grid_ratio_wrfmass - ratio of analysis grid to wrf mass grid in wrf grid units.
!     grid_type_fv3_regional - type of fv3 model grid (grid orientation).
!     twodvar_regional  - logical for regional 2d-var analysis
!     filled_grid       - logical to fill in puts on WRF-NMM E-grid
!     half_grid         - logical to use every other row of WRF-NMM E-Grid
!     nvege_type - number of types of vegetation; old=24, IGBP=20
!     nlayers    - number of sub-layers to break indicated model layer into
!                  prior to calling radiative transfer model
!     jcap_gfs   - spectral truncation used to transform high wavenumber
!                  spectral coefficients to a coarser resolution grid,
!                  when use_gfs_ozone = .true. or use_gfs_stratosphere = .true.   
!     use_sp_eqspac     - if .true., then ensemble grid is equal spaced, staggered 1/2 grid unit off
!                         poles.  if .false., then gaussian grid assumed for ensemble (global only)
!     wrf_mass_hybridcord - logical for using WRF MASS CORE with hybrid vertical coordinate


  namelist/gridopts/jcap,jcap_b,nsig,nlat,nlon,nlat_regional,nlon_regional,&
       diagnostic_reg,update_regsfc,netcdf,regional,wrf_nmm_regional,nems_nmmb_regional,fv3_regional,fv3_cmaq_regional,&
       wrf_mass_regional,twodvar_regional,filled_grid,half_grid,nvege_type,nlayers,cmaq_regional,&
       nmmb_reference_grid,grid_ratio_nmmb,grid_ratio_fv3_regional,grid_ratio_wrfmass,jcap_gfs,jcap_cut,&
       wrf_mass_hybridcord,grid_type_fv3_regional,fv3_io_layout_y

! BKGERR (background error related variables):
!     vs       - scale factor for vertical correlation lengths for background error
!     nhscrf   - number of horizontal scales for recursive filter
!     hzscl(n) - scale factor for horizontal smoothing, n=1,number of scales (3 for now)
!                specifies factor by which to reduce horizontal scales (i.e. 2 would
!                then apply 1/2 of the horizontal scale
!     hswgt(n) - empirical weights to apply to each horizontal scale
!     norh     - order of interpolation in smoothing
!     ndeg     - degree of smoothing in recursive filters
!     noq      - 1/4 of accuracy in compact finite differencing
!     bw       - factor in background error calculation
!     norsp    - order of interpolation for smooth polar cascade routine
!                 default is norsp=0, in which case norh is used with original
!                 polar cascade interpolation.
!     pert_berror - logical to turn on random inflation/deflation of background error
!                   tuning parameters
!     pert_berr_fct - factor for increasing/decreasing berror parameters, this is multiplied
!                     by random number
!     bkgv_flowdep  - flag to turn on flow dependence to background error variances
!     bkgv_rewgtfct - factor used to perform flow dependent reweighting of error variances
!     bkgv_write - flag to turn on=.true. /off=.false. generation of binary file with reweighted variances
!     fpsproj  - controls full nsig projection to surface pressure
!     fut2ps  - controls the projection from unbalance T to surface pressure
!     adjustozvar - adjusts ozone variances in the stratosphere based on guess field
!     cwcoveqqcov  - sets cw Bcov to be the same as B-cov(q) (presently glb default)

  namelist/bkgerr/vs,nhscrf,hzscl,hswgt,norh,ndeg,noq,bw,norsp,fstat,pert_berr,pert_berr_fct, &
      bkgv_flowdep,bkgv_rewgtfct,bkgv_write,fpsproj,adjustozvar,fut2ps,cwcoveqqcov,usenewgfsberror

! ANBKGERR (anisotropic background error related variables):
!     anisotropic - if true, then use anisotropic background error
!     ancovmdl    - covariance model settings - 0: pt-based, 1: ensemble based
!     triad4      - for 2d variables, if true, use blended triad algorithm
!     ifilt_ord   - filter order for anisotropic filters
!     npass       - 2*npass = number of factors in background error
!     normal      - number of random vectors to use for filter normalization
!                     ( if < 0 then slightly slower, but results independent of
!                       number of processors)
!     binom       - if true, weight correlation lengths of factors using binomial
!                      distribution, with shortest scales on outside, longest scales
!                      on inside.  This can help to produce smoother correlations in the
!                      presence of strong anisotrophy
!     grid_ratio  - ratio of coarse to fine grid in fine grid units
!     grid_ratio_p- ratio of coarse to fine grid in fine grid units for polar patches
!     nord_f2a    - order of interpolation for transfer operators between filter grid and analysis grid
!     ngauss      - number of gaussians to add together in each factor
!     rgauss      - multipliers on reference aspect tensor for each gaussian factor
!     anhswgt     - empirical weights to apply to each gaussian
!     an_vs       - scale factor for background error vertical scales (temporary carry over from
!                    isotropic inhomogeneous option)
!     an_flen_u   -  coupling parameter for connecting horizontal wind to background error
!     an_flen_t   -  coupling parameter for connecting grad(pot temp) to background error
!     an_flen_z   -  coupling parameter for connecting grad(terrain) to background error
!     afact0      - anistropy effect parameter, the range must be in 0.0-1.0.
!     covmap      - if true, covariance map would be drawn
!     rtma_subdomain_option - if true, then call alternative code which calls recursive filter
!                              directly from subdomain mode, bypassing transition to/from
!                              horizontal slabs.  This is mainly to improve efficiency for
!                              2d rtma analysis.  at the moment, this only works for
!                              twodvar_regional=.true.  rtma_subdomain_option will be forced
!                              to false when twodvar_regional=.false.
!     rtma_bkerr_sub2slab - if true, then run recursive filter in slab mode
!     lreadnorm   -  if true, then read normalization from fixed files
!     nsmooth     -  number of 1-2-1 smoothing passes before and after background error application
!     nsmooth_shapiro - number of 2nd moment preserving (shapiro) smoothing passes before and after
!                       background error application.
!                        NOTE:  default for nsmooth and nsmooth_shapiro is 0.
!                               if both are > 0, then nsmooth will be forced to zero.

  namelist/anbkgerr/anisotropic,ancovmdl,triad4,ifilt_ord,npass,normal,binom,&
       ngauss,rgauss,anhswgt,an_vs, &
       grid_ratio,grid_ratio_p,nord_f2a,an_flen_u,an_flen_t,an_flen_z, &
       rtma_subdomain_option,rtma_bkerr_sub2slab,lreadnorm,nsmooth,nsmooth_shapiro, &
       afact0,covmap

! JCOPTS (Jc term)
!                 if .false., uses original formulation based on wind, temp, and ps tends
!     ljcdfi      - when .t. uses digital filter initialization of increments (4dvar)
!     alphajc     - parameter for digital filter
!     ljpdry      - when .t. uses dry pressure constraint on increment
!     bamp_jcpdry - parameter for pdry_jc
!     eps_eer     - Errico-Ehrendofer parameter for q-term in energy norm
!     ljc4tlevs    - when true and in 4D mode, apply any weak constraints over all time levels
!                   instead of just at a single time
!

  namelist/jcopts/ljcdfi,alphajc,switch_on_derivatives,tendsflag,ljcpdry,bamp_jcpdry,eps_eer,&
      ljc4tlevs,ljclimqc 

! STRONGOPTS (strong dynamic constraint)
!     reg_tlnmc_type -  =1 for 1st version of regional strong constraint
!                       =2 for 2nd version of regional strong constraint
!     nstrong  - if > 0, then number of iterations of implicit normal mode initialization
!                   to apply for each inner loop iteration
!     period_max     - cutoff period for gravity waves included in implicit normal mode
!                    initialization (units = hours)
!     period_width   - defines width of transition zone from included to excluded gravity waves
!     period_max - cutoff period for gravity waves included in implicit normal mode
!                   initialization (units = hours)
!     period_width - defines width of transition zone from included to excluded gravity waves
!     nvmodes_keep - number of vertical modes to use in implicit normal mode initialization
!     baldiag_full 
!     baldiag_inc
!     tlnmc_option : integer flag for strong constraint (various capabilities for hybrid)
!                   =0: no TLNMC
!                   =1: TLNMC for 3DVAR mode
!                   =2: TLNMC on total increment for single time level only (for 3D EnVar)
!                       or if 4D EnVar mode, TLNMC applied to increment in center of window
!                   =3: TLNMC on total increment over all time levels (if in 4D EnVar mode)
!                   =4: TLNMC on static contribution to increment ONLY for any EnVar mode

  namelist/strongopts/reg_tlnmc_type,tlnmc_option, &
                      nstrong,period_max,period_width,nvmodes_keep, &
		      baldiag_full,baldiag_inc

! OBSQC (observation quality control variables):
!
!     Parameters used for gross error checks
!        obserrx = max(ermin,min(ermax,obs error)
!        if(abs(simulated)-observation)/obserrx > gross observation rejected
!
!
!     Parameters below use for nonlinear (variational) quality control
!     dfact    - factor for duplicate obs at same location for conv. data
!     dfact1   - time factor for duplicate obs at same location for conv. data
!     erradar_inflate - radar error inflation factor
!     tdrerr_inflate - logical for tdr obs error inflation
!     oberrflg - logical for reading in new obs error table (if set to true)
!     vadfile  - character(10) variable holding name of vadwnd bufr file
!     noiqc    - logical flag to bypass OIQC (if set to true)
!     c_varqc - constant number to control var. qc turnning on speed
!     blacklst - logical for reading in raob blacklist (if set to true)
!     use_poq7 - logical flag to accept (.true.) sbuv profile quality flag 7
!     tcp_refps  - reference pressure for tcps oberr calculation (mb)
!     tcp_width  - parameter for tcps oberr inflation (width, mb)
!     tcp_ermin  - parameter for tcps oberr inflation (minimum oberr, mb)
!     tcp_ermax  - parameter for tcps oberr inflation (maximum oberr, mb)
!     gps_jacqc  - logical to turn on GNSSRO Jacobian QC (default is off)
!     qc_noirjaco3 - controls whether to use O3 Jac from IR instruments
!     qc_noirjaco3_pole - controls wheter to use O3 Jac from IR instruments near poles
!     qc_satwnds - allow bypass sat-winds qc normally removing lots of mid-tropo obs
!     aircraft_t_bc_pof  - logical for aircraft temperature bias correction, pof
!                          is used for predictor
!     aircraft_t_bc  - logical for aircraft temperature bias correction
!     aircraft_t_bc_ext - logical for reading aircraft temperature bias correction from external file
!     buddycheck_t - When true, run buddy check algorithm on temperature observations
!     buddydiag_save - When true, output files containing buddy check QC info for all
!                      obs run through the buddy check
!     njqc  -  When true, use Purser''s non linear QC
!     vqc   -  when true, use ECMWF's non linear QC
!     nvqc   -  when true, use Dr. Purser's variational QC 
!     hub_norm - when true,use huber norm format distribution 
!     closest_obs- when true, choose the timely closest surface observation from
!     multiple observations at a station.  Currently only applied to Ceiling
!     height and visibility.
!     pvis   - power parameter in nonlinear transformation for vis 
!     pcldch - power parameter in nonlinear transformation for cldch
!     scale_cv - scaling constant in meter
!     estvisoe - estimate of vis observation error
!     estcldchoe - estimate of cldch observation error
!     vis_thres  - threshold value for both vis observation and input first guess
!     cldch_thres  - threshold value for both cldch observation and input first guess
!     cld_det_dec2bin - re-interprets cld_det in satinfo as binary entries
!     ompslp_mult_fact - multiplication factor for OMPS LP obserror read in

! The following variables are the coefficients that describe the
! linear regression fits that are used to define the dynamic
! observation error (DOE) specifications for all reconnissance
! observations collected within hurricanes/tropical cyclones; these
! apply only to the regional forecast models (e.g., HWRF); Henry
! R. Winterbottom (henry.winterbottom@noaa.gov).

! Observation types:

! 1/236: HDOB (e.g., flight-level) observations.
       
! 1/237: Dropsonde observations.

! 213: SFMR observations.

! The following correspond to the specific humidity (q) observations:

!     q_doe_a_136, q_doe_a_137 - specific humidity linear regression
!                                derived 'a' coefficients for specific
!                                humidity observations.

!     q_doe_b_136, q_doe_b_137 - specific humidity linear regression
!                                derived 'b' coefficients for specific
!                                humidity observations.

!     t_doe_a_136, t_doe_a_137 - temperature linear regression derived
!                                'a' coefficients for temperature
!                                observations.

!     t_doe_b_136, t_doe_b_137 - temperature linear regression derived
!                                'b' coefficients for temperature
!                                observations.  

!     uv_doe_a_236, uv_doe_a_237, uv_doe_a_213 - wind linear
!                                                regression derived
!                                                'a' coefficients for
!                                                wind observations.

!     uv_doe_b_236, uv_doe_b_237, uv_doe_b_213 - wind linear
!                                                regression derived
!                                                'b' coefficients for
!                                                wind observations.

!     vad_near_analtime - assimilate newvadwnd obs around analysis time only
!
!     Flags to use the new IR cloud detection routine.  Flag must be set to true to use the new routine.  The default
!     (no flag or .false.) will use the default.
!     airs_cads  : use the cloud and aerosol detection software for AIRS instrument
!     cris_cads  : use the cloud and aerosol detection software for CrIS instruments
!     iasi_cads  : use the cloud and aerosol detection software for IASI instruments
!     iasing_cads: use the cloud and aerosol detection software for IASI-NG instruments
!     
  
  namelist/obsqc/dfact,dfact1,erradar_inflate,tdrerr_inflate,oberrflg,&
       vadfile,noiqc,c_varqc,blacklst,use_poq7,hilbert_curve,tcp_refps,tcp_width,&
       tcp_ermin,tcp_ermax,gps_jacqc,qc_noirjaco3,qc_noirjaco3_pole,qc_satwnds,njqc,vqc,nvqc,hub_norm,troflg,lat_c,nrand,&
       aircraft_t_bc_pof,aircraft_t_bc,aircraft_t_bc_ext,biaspredt,upd_aircraft,cleanup_tail,&
       hdist_aircraft,buddycheck_t,buddydiag_save,vadwnd_l2rw_qc,ompslp_mult_fact,  &
       pvis,pcldch,scale_cv,estvisoe,estcldchoe,vis_thres,cldch_thres,cld_det_dec2bin, &
       q_doe_a_136,q_doe_a_137,q_doe_b_136,q_doe_b_137, &
       t_doe_a_136,t_doe_a_137,t_doe_b_136,t_doe_b_137, &
       uv_doe_a_236,uv_doe_a_237,uv_doe_a_213,uv_doe_b_236,uv_doe_b_237,uv_doe_b_213, &
       vad_near_analtime,airs_cads,cris_cads,iasi_cads, iasing_cads

! OBS_INPUT (controls input data):
!      dmesh(max(dthin))- thinning mesh for each group
!      time_window_max  - upper limit on time window for all input data
!      time_window_rad  - upper limit on time window for certain radiance input data
!      ext_sonde        - logical for extended forward model on sonde data
!      l_foreaft_thin -   separate TDR fore/aft scan for thinning

  namelist/obs_input/dmesh,time_window_max,time_window_rad, &
       ext_sonde,l_foreaft_thin,hofx_2m_sfcfile

! SINGLEOB_TEST (one observation test case setup):
!      maginnov   - magnitude of innovation for one ob
!      magoberr   - magnitude of observational error
!      oneob_type - observation type (lsingleradob: platform type, i.e. 'airs')
!      oblat      - observation latitude (lsingleradob: footprint cenlat)
!      oblon      - observation longitude (lsingleradob: footprint cenlon)
!      obpres     - observation pressure
!      obdattim   - observation date
!      obhourset  - observation delta time from analysis time
!      pctswitch  - if .true. innovation & oberr are relative (%) of background value
!                      (level ozone only)
!      obchan     - if > 0, selects the channel number.  If <= zero, it will use
!                   all channels that pass qc in setuprad.    

  namelist/singleob_test/maginnov,magoberr,oneob_type,&
       oblat,oblon,obpres,obdattim,obhourset,pctswitch,&
       obchan,anel_rw,anaz_rw,range_rw,sstn,learthrel_rw

! SUPEROB_RADAR (level 2 bufr file to radar wind superobs):
!      del_azimuth     - azimuth range for superob box  (default 5 degrees)
!      del_elev        - elevation angle range for superob box  (default .05 degrees)
!      del_range       - radial range for superob box  (default 5 km)
!      del_time        - 1/2 time range for superob box  (default .5 hours)
!      elev_angle_max  - max elevation angle (default of 5 deg recommended by S. Liu)
!      minnum                  - minimum number of samples needed to make a superob
!      range_max       - max radial range to use in constructing superobs  (default 100km)
!      l2superob_only  - if true, then process level 2 data creating superobs, then quit.
!                          (added for easier retrospective testing, since level 2 bufr
!                             files are very large and hard to work with)

  namelist/superob_radar/del_azimuth,del_elev,del_range,del_time,&
       elev_angle_max,minnum,range_max,l2superob_only,radar_sites,radar_box,radar_rmesh,radar_zmesh

! RADARUSE_directDA
!     mphyopt          - microphysics scheme to use in the forward operator
!                        (2-6(LIN) and 108(TM) are supported for now)
!     oe_rw            - observerion error of radar radial wind obs (m/s)
!                        default=1.0
!     oe_dbz           - observerion error of radar reflectivity (dbz)
!                        default=1.0
!     l_set_be_rw      - re-set background error statistics for using radar wind
!                        obs (.TRUE.: on  ; .FALSE.: off)
!                        default=.false.
!     l_set_be_dbz     - re-set background error statistics for using radar dbz
!                        obs (.TRUE.: on  ; .FALSE.: off)
!                        default=.false.
!    l_set_oerr_ratio_rw  - re-set obs error (inflation ratio) for radar wind
!                           assimilation (.TRUE.: on  ; .FALSE.: off)
!                           default=.false.
!    l_set_oerr_ratio_dbz - re-set obs error (inflation ratio) for radar 
!                           reflectivity assimilation 
!                           (.TRUE.: on  ; .FALSE.: off)
!                           default=.false.
!     be_sf            - multiplying factor to tune the background error
!                        standard deviation of stream function (s.f.)
!                        default=0.2/4.5 
!     hscl_sf          - horizontal background error correlation length scale of
!                        stream function (meter)
!                        default=20000.
!     vscl_sf          - vertical background error correlation length scale of
!                        stream function
!                        default=1.5
!     be_vp            - multiplying factor to tune the background error
!                        standard deviation of velocity potential (v.p.)
!                        default=0.2/4.5 
!     hscl_vp          - horizontal background error correlation length scale of
!                        velocity potential (meter)
!                        default=20000.
!     vscl_vp          - vertical background error correlation length scale of
!                        velocity potential
!                        default=1.5
!     be_t             - multiplying factor to tune the background error
!                        standard deviation of temperature (t)
!                        default=-1.0
!     hscl_t           - horizontal background error correlation length scale of
!                        temperature
!                        default=-20000.
!     vscl_t           - vertical background error correlation length scale of
!                        temperature
!                        default=-1.5
!     be_q             - multiplying factor to tune the background error
!                        standard deviation of moisture mixing ratio (q)
!                        default=-1.0
!     hscl_q           - horizontal background error correlation length scale of
!                        moisture mixing ratio
!                        default=-20000.
!     vscl_q           - vertical background error correlation length scale of
!                        moisture mixing ratio
!                        default=-1.5
!     be_qr            - background error standard deviation for mixing ratio of
!                        rain water (kg/kg)
!                        default=1.0E-3
!     be_qs            - background error standard deviation for mixing ratio of
!                        snow water (kg/kg)
!                        default=1.0E-3
!     be_qg            - background error standard deviation for mixing ratio of
!                        graupel (kg/kg)
!                        default=1.0E-3
!     hscl_qx          - horizontal correlation length scale for mixing ratio of
!                        cloud hydrometers (meter)
!                        default=6000.
!     vscl_qx          - vertical   correlation length scale for mixing ratio of
!                        cloud hydrometers
!                        default=1.5
!     l_decouple_sf_vp  - de-couple the correlation/balance 
!                         between s.f. and v.p.
!                         (.TRUE.: on  ; .FALSE.: off)
!                         default=.false.
!     l_decouple_sf_tps - de-couple the correlation/balance
!                         between s.f. and temperature, ps
!                         (.TRUE.: on  ; .FALSE.: off)
!                         default=.false.
!     rw_obs4wrd_bmwth - beam width impact on radar wind obs forward operator
!                        default=2
!                        ! 1: GSI original (vrminmax)
!                        ! 2: simple vertical interpolation
!                        ! 3: weighted average of multiple-layers
!     lvldbg           - debugging level regarding to directDA code
!                        default=0
!     l_correct_azmu   - options for correction of azimuth angles of 
!                        radar observations (used in read_radar.f90)
!                        (.TRUE.: on  ; .FALSE.: off)
!                        default=.true.
!     l_correct_tilt   - options for correction of tilt angles of 
!                        radar observations (used in read_radar.f90)
!                        (.TRUE.: on  ; .FALSE.: off)
!                        default=.true.
!     i_correct_tilt   - options for algorithm to compute corrected tilt
!                        default=2
!                        ! 1. equations used in GSI;
!                        ! 2. equations used in ARPS
!     l_azm_east1st    - change azimuth to east as 0 before correct it
!                        default=.true.
!     l_use_cvpqx      - use power transform to qx (qr/qs/qg)
!                        (.TRUE.: on  ; .FALSE.: off)
!                        default=.false.
!     cvpqx_pval       - power value to qx(qr/qs/qg)
!                        default=0.000001
!     l_plt_be_stats   - output background error statistics for plot
!                        (.TRUE.: on  ; .FALSE.: off)
!                        default=.true.
!     l_be_T_dep       - temperature dependent error variance
!                        (.TRUE.: on  ; .FALSE.: off)
!                        default=.false.
!     l_gpht2gmht      - convert goepotential height to geometric height 
!                        (used in setupdbz.f90)
!                        (.TRUE.: on  ; .FALSE.: off)
!                        default=.false.
!     refl_lowbnd_rw   - lower-bound of obs dbz for rw assimilation
!                        default=5.
!                        (if obs_dbz < dbz_lowbnd_dbz, then
!                         the rw (wind) obs accompanied with
!                         this obs_dbz  is rejected for rw assimilation)
!     refl_lowbnd_dbz  - lower-bound of obs dbz for dbz assimilation
!                        default=0.
!                        (if obs_dbz < dbz_lowbnd_dbz, then
!                         this obs_dbz is rejected for dbz assimilation)
!     l_plt_diag_rw    - options for checking-up and diagnose of radial wind
!                        (used in setuprw and read_radar)
!                        (.TRUE.: on  ; .FALSE.: off)
!                        default=.false.
!     l_chk_bmwth      - options for checking-up and diagnose of radial wind
!                        (used in setuprw and read_radar)
!                        (.TRUE.: on  ; .FALSE.: off)
!                        default=.false.
!     i_melt_snow      - control the melting effect 
!                        in dbz obs forward operator for snow
!                        default=0
!                        ! < 0 : no melting, and keeping dry at
!                        !       any temperature
!                        ! >=0 : melting depends on
!                        !       temperature ! (273.15 K)
!                        ! =100: melting and keeping wet at
!                        !       any temperature
!     i_melt_graupel   - control the melting effect 
!                        in dbz obs forward operator for graupel
!                        default=0
!                        ! < 0 : no melting, and keeping dry all
!                        !       the time
!                        ! >=0 : melting depends on
!                        !       temperature ! (273.15 K)
!                        ! =100: melting and keeping wet at
!                        !       any temperature
!     cld_cv           - cloud hydrometers used as control variables 
!                        in analysis
!                        default=0
!     cld_nt_updt      - cloud hydrometer number concentration 
!                        default=1
!                        ! 0: no update to number concentration
!                        ! 1: updated through analysis (for now,
!                        ! only in hybrid analysis)
!     i_w_updt         - w (vertical velocity) is analysis variable 
!                        and updated
!                        default=0
!                        ! 0: not analyzed ; 1: analyzed
!     l_cvpnr          - use power tranform for qnr
!                        (.TRUE.: on  ; .FALSE.: off)
!                        default=.false.
!     cvpnr_pval       - power value for qnr
!                        default=.0.6
!     MFflg            - Flag to determine what options are selected
!                        to calculate fraction of wet mixing ratio (melting)
!                        Affects only for TM operator in EnKF application
!                        default = 3
!                        1 – melting based on the ratio between qr and qx
!                        2 – melting is not considered
!                        3 – temperature-based melting
!     l_use_tdep_radarz – use temperature dependent feature in radarZ,
!                         if it is set as .TRUE., GSI provide tk to radarZ
!                         if not used, ta is set as constant (273.16K)
!                         Affects only for TM operator in EnKF
!                         default = .true.
!
  namelist/radaruse_directDA/mphyopt,oe_rw,oe_dbz,l_set_be_rw,l_set_be_dbz,   &
                         l_set_oerr_ratio_rw, l_set_oerr_ratio_dbz,       &
                         be_sf,hscl_sf,vscl_sf,be_vp,hscl_vp,vscl_vp,     &
                         be_q, hscl_q, vscl_q, be_t, hscl_t, vscl_t,      &
                         be_qr, be_qs, be_qg, hscl_qx, vscl_qx,           &
                         l_decouple_sf_vp,l_decouple_sf_tps,              &
                         rw_obs4wrd_bmwth, lvldbg,                        &
                         l_correct_azmu, l_correct_tilt, i_correct_tilt,  &
                         l_azm_east1st, l_use_cvpqx, cvpqx_pval,          &
                         l_plt_be_stats,                                  &
                         l_be_T_dep, l_gpht2gmht,                         &
                         refl_lowbnd_rw, refl_lowbnd_dbz,                 &
                         l_plt_diag_rw, l_chk_bmwth,                      &
                         i_melt_snow, i_melt_graupel,                     &
                         cld_cv, cld_nt_updt, i_w_updt,                   &
                         l_cvpnr, cvpnr_pval, MFflg, l_use_tdep_radarz

! LAG_DATA (lagrangian data assimilation related variables):
!     lag_accur - Accuracy used to decide whether or not a balloon is on the grid
!     infile_lag- File containing the initial position of the balloon
!     lag_stepduration- Duration of one time step for the propagation model
!     lag_nmax_bal- Maximum number of balloons at starting time
!     lag_vorcore_stderr_a - Observation error for vorcore balloon
!     lag_vorcore_stderr_b -   error = b + a*timestep(in hours)
  namelist/lag_data/lag_accur,infile_lag,lag_stepduration,lag_nmax_bal,&
      lag_vorcore_stderr_a,lag_vorcore_stderr_b

! HYBRID_ENSEMBLE (parameters for use with hybrid ensemble option)
!     l_hyb_ens     - if true, then turn on hybrid ensemble option
!     uv_hyb_ens    - if true, then ensemble perturbation wind variables are u,v,
!                       otherwise, ensemble perturbation wind variables are stream, pot. functions.
!     q_hyb_ens     - if true, then use specific humidity ensemble perturbations,
!                       otherwise, use relative humidity
!     oz_univ_static- if true, decouple ozone from other variables and defaults to static B (ozone only)
!     aniso_a_en - if true, then use anisotropic localization of hybrid ensemble control variable a_en.
!     generate_ens - if true, then generate internal ensemble based on existing background error
!     n_ens        - number of ensemble members.
!     nlon_ens     - number of longitudes on ensemble grid (may be different from analysis grid nlon)
!     nlat_ens     - number of latitudes on ensemble grid (may be different from analysis grid nlat)
!     jcap_ens     - for global spectral model, spectral truncation
!     jcap_ens_test- for global spectral model, test spectral truncation (to test dual resolution)
!     beta_s0      -  the default weight given to static background error covariance if (.not. readin_beta)
!                              0 <= beta_s0 <= 1,  tuned for optimal performance
!                             =1, then ensemble information turned off
!                             =0, then static background turned off
!                            the weights are applied per vertical level such that : 
!                                        beta_s(:) = beta_s0     , vertically varying weights given to static B ; 
!                                        beta_e(:) = 1 - beta_s0 , vertically varying weights given ensemble derived covariance.
!                            If (readin_beta) then beta_s and beta_e are read from a file and beta_s0 is not used.
!     beta_e0 - default weight given to ensemble background error covariance
!               (if .not. readin_beta). if beta_e0<0, then it is set to
!               1.-beta_s0 (this is the default)
!     s_ens_h - horizontal localization correlation length of Gaussian exp(-0.5*(r/L)**2)
!               (units of km), default = 2828.0
!     s_ens_v - vertical localization correlation length of Gaussian exp(-0.5*(r/L)**2)
!               (grid units if s_ens_v>=0, or units of ln(p) if s_ens_v<0), default = 30.0
!                  in scale/variable/time-dependent localization (SDL/VDL/TDL),
!                  localization length for i-th scale, j-th variable, and k-th time is
!                     s_ens_[hv]( i + nsclgrp*(j-1) + nsclgrp*ngvarloc*(k-1) )
!                        in SDL(nsclgrp>1),         i = 1(largest scale)  .. nsclgrp(smallest scale)
!                        in VDL(ngvarloc=2),        j = 1(itracer<=10)    .. 2(itracer>=11)
!                        in TDL(l_timloc_opt=true), k = 1(first time bin) .. ntlevs_ens(last time bin)
!                  in SDL, scale separation length for i-th scale is also set here as
!                     s_ens_[hv]( naensgrp+i ) - naensgrp is the total number of localization lengths for SDL/VDL/TDL
!                        in applying SDL only horizontally, set s_ens_v(naensgrp+i)=0.0
!     use_gfs_ens  - controls use of global ensemble: .t. use GFS (default); .f. uses user-defined ens
!     readin_localization - flag to read (.true.)external localization information file
!     readin_beta         - flag to read (.true.) the vertically varying beta parameters beta_s and beta_e
!                              from a file.
!     eqspace_ensgrid     - if .true., then ensemble grid is equal spaced, staggered 1/2 grid unit off
!                               poles.  if .false., then gaussian grid assumed
!                               for ensemble (global only)
!     use_localization_grid - if true, then use extra lower res gaussian grid for horizontal localization
!                                   (global runs only--allows possiblity for non-gaussian ensemble grid)
!     pseudo_hybens    - if true, turn on pseudo ensemble hybrid for HWRF
!     merge_two_grid_ensperts  - if true, merge ensemble perturbations from two forecast domains
!                                to analysis domain (one way to deal with hybrid DA for HWRF moving nest)
!     regional_ensemble_option - integer, used to select type of ensemble to read in for regional
!                              application.  Currently takes values from 1 to 4.
!                                 =1: use GEFS internally interpolated to ensemble grid.
!                                 =2: ensembles are WRF NMM format
!                                 =3: ensembles are ARW netcdf format.
!                                 =4: ensembles are NEMS NMMB format.
!     full_ensemble    - if true, first ensemble perturbation on first guess istead of on ens mean
!     pwgtflg          - if true, use vertical integration function on ensemble contribution of Psfc
!     grid_ratio_ens   - for regional runs, ratio of ensemble grid resolution to analysis grid resolution
!                            default value = 1  (dual resolution off)
!     i_en_perts_io - flag to read in ensemble perturbations in ensemble grid.
!                         This is to speed up RAP/HRRR hybrid runs because the
!                         same ensemble perturbations are used in 6 cycles    
!                           =0:  No ensemble perturbations IO (default)
!                           =2:  skip get_gefs_for_regional and read in ensemble
!                                 perturbations from saved files.
!     l_ens_in_diff_time  -  if use ensembles that are available at different time
!                              from analysis time.
!                             =false: only ensembles available at analysis time
!                                      can be used for hybrid. (default)
!                             =true: ensembles available time can be different
!                                      from analysis time in hybrid analysis
!     ensemble_path - path to ensemble members; default './'
!     ens_fast_read - read ensemble in parallel; default '.false.'
!     sst_staticB - use only static background error covariance for SST statistic
!     nsclgrp - number of scale-dependent localization lengths
!     l_timloc_opt - if true, then turn on time-dependent localization
!     ngvarloc - number of variable-dependent localization lengths
!     naensloc - total number of spatial localization lengths and scale separation lengths (should be naensgrp+nsclgrp-1)
!     r_ensloccov4tim - factor multiplying to cross-time covariance
!                         For example,
!                         =0.0: cross-time covariance is decreased to zero
!                         =0.5: cross-time covariance is decreased to half
!                         =1.0: cross-time covariance is retained
!     r_ensloccov4var - factor multiplying to cross-variable covariance
!                         For example,
!                         =0.0: cross-variable covariance is decreased to zero
!                         =0.5: cross-variable covariance is decreased to half
!                         =1.0: cross-variable covariance is retained
!     r_ensloccov4scl - factor multiplying to cross-scale covariance
!                         For example,
!                         =0.0: cross-scale covariance is decreased to zero
!                         =0.5: cross-scale covariance is decreased to half
!                         =1.0: cross-scale covariance is retained
!     global_spectral_filter_sd - if true, use spectral filter function for
!                                 scale decomposition in the global application (Huang et al. 2021)
!     assign_vdl_nml - if true, vdl_scale, and vloc_varlist will be used for
!                      assigning variable-dependent localization upon SDL in gsiparm.anl.
!                      This method described in (Wang and Wang 2022, JAMES) is
!                      equivalent to, but different from the method associated
!                      with the parameter r_ensloccov4var.
!     vloc_varlist - list of control variables using the same localization length,
!                     effective only with assign_vdl_nml=.true. For example,
!                     vloc_varlist(1,:) = 'sf','vp','ps','t',
!                     vloc_varlist(2,:) = 'q',
!                     vloc_varlist(3,:) = 'qr','qs','qg','dbz','w','ql','qi',
!                     vloc_varlist(4,:) = 'sf','vp','ps','t','q',
!                     vloc_varlist(5,:) = 'qr','qs','qg','dbz','w','ql','qi',
!                     This example indicates that 3 variable-groups will be adopted for VDL. 
!                     'sf','vp','ps','t' will share the same localization length of v1L1; 
!                     'q' will have the localization lenth of v2L1
!                     'qr','qs','qg','dbz','w','ql','qi', use the same localization length of v3L1
!
!                     For L2, a different configuration of VDL can be applied:
!                               ~~~~~~~~~
!                     'sf','vp','ps','t','q' will share the same localization length of v2L2; 
!                     'qr','qs','qg','dbz','w','ql','qi', use the same localization length of v2L2
!     vdl_scale - number of variables in each variable-group, effective only with assign_vdl_nml=.true.
!                 if 3 variable-groups with 2 separated scale is set, 
!                 vdl_scale = 3,    3,    3,   2,    2
!                             ^     ^     ^    ^     ^ 
!                 s_ens_h  = v1L1  v2L1  v3L1  v1L2 v2L2
!                 Then localization lengths will be assigned as above.
!     l_mgbf_loc - if true, multi-grid beta filter is used for localization instead of recursive filter
!
  namelist/hybrid_ensemble/l_hyb_ens,uv_hyb_ens,q_hyb_ens,aniso_a_en,generate_ens,n_ens,&
                l_both_fv3sar_gfs_ens,n_ens_gfs,n_ens_fv3sar,weight_ens_gfs,weight_ens_fv3sar,nlon_ens,nlat_ens,jcap_ens,&
                pseudo_hybens,merge_two_grid_ensperts,regional_ensemble_option,fv3sar_bg_opt,fv3sar_ensemble_opt,full_ensemble,pwgtflg,&
                jcap_ens_test,beta_s0,beta_e0,s_ens_h,s_ens_v,readin_localization,eqspace_ensgrid,readin_beta,&
                grid_ratio_ens, &
                oz_univ_static,write_ens_sprd,use_localization_grid,use_gfs_ens, &
                i_en_perts_io,l_ens_in_diff_time,ensemble_path,ens_fast_read,sst_staticB,limqens, &
                nsclgrp,l_timloc_opt,ngvarloc,naensloc,r_ensloccov4tim,r_ensloccov4var,r_ensloccov4scl,&
                vdl_scale,vloc_varlist,&
                global_spectral_filter_sd,assign_vdl_nml,parallelization_over_ensmembers,l_mgbf_loc

! rapidrefresh_cldsurf (options for cloud analysis and surface 
!                             enhancement for RR appilcation  ):
!      dfi_radar_latent_heat_time_period     -   DFI forward integration window in minutes
!      metar_impact_radius  - metar low cloud observation impact radius in grid number
!      l_metar_impact_radius_change - if .true. the impact radius will change
!                            with height that set up with the metar_impact_radius_max, min,
!                            max_height, min_height, (default:false)
!      metar_impact_radius_max  - The max impact radius of metar cloud observation
!                            in meter (default: 100000 m).
!      metar_impact_radius_min  - The min impact radius of metar cloud observation
!                            in meter (default: 10000 m).
!      metar_impact_radius_max_height - The hight above which metar_impact_radius_max apply
!                            in meter (default: 1200m).
!      metar_impact_radius_min_height - The hight below which metar_impact_radius_min apply
!                            in meter (default: 200m).
!      l_gsd_terrain_match_surftobs - if .true., GSD terrain match for surface temperature observation
!      l_sfcobserror_ramp_t  - namelist logical for adjusting surface temperature observation error
!      l_sfcobserror_ramp_q  - namelist logical for adjusting surface moisture observation error
!      l_pbl_pseudo_surfobst  - if .true. produce pseudo-obs in PBL layer based on surface obs T
!      l_pbl_pseudo_surfobsq  - if .true. produce pseudo-obs in PBL layer based on surface obs Q
!      l_pbl_pseudo_surfobsuv - if .true. produce pseudo-obs in PBL layer based on surface obs UV
!      pblh_ration - percent of the PBL height within which to add pseudo-obs (default:0.75)
!      pps_press_incr - pressure increase for each additional pseudo-obs 
!                       on top of previous level (default:30hPa)
!      l_gsd_limit_ocean_q      - if .true. do GSD limitation of Q over ocean
!      l_pw_hgt_adjust      - if .true. do GSD PW adjustment for model vs. obs station height
!      l_limit_pw_innov     - if .true. do GSD limitation of PW obs
!      max_innov_pct        - sets limit of PW ob to a percent of the background value (0-1)
!      l_cleansnow_warmts   - if .true. do GSD limitation of using retrieved snow over warn area
!                                               (Ts > r_cleansnow_warmts_threshold) 
!      r_cleansnow_warmts_threshold - threshold for using retrieved snow over warn area
!      l_conserve_thetaV    - if .true. conserve thetaV during moisture adjustment in cloud analysis
!      i_conserve_thetav_iternum    - iteration number for conserving thetaV during moisture adjustment
!      l_gsd_soiltq_nudge   - if .true. do GSD soil T and Q nudging based on the lowest t analysis inc
!      l_cld_bld            - if .true. do GSD GOES cloud building
!      cld_bld_hgt          - sets limit below which GOES cloud building occurs (default:1200m)
!      build_cloud_frac_p   - sets the threshold for building clouds from satellite
!      clear_cloud_frac_p   - sets the threshold for clearing clouds from satellite
!      nesdis_npts_rad  - NESDIS cloud product impact radiu (grid points)
!      iclean_hydro_withRef - if =1, then clean hydrometeors if the grid point
!                               has no echo and maxref=0
!      iclean_hydro_withRef_allcol - if =1, then clean whole column hydrometeors
!                      if the observed max ref =0 and satellite cloud shows
!                      clean
!      i_use_2mq4b    -  background used for calculate surface moisture observation
!                               innovation
!                         =0  Use Q from the 1st model level. (default) 
!                         =1  use 2m Q as part of background
!      i_use_2mt4b    -  background used for calculate surface temperature         
!                             observation innovation
!                         =0  Use T from the 1st model level. (default)
!                         =1  use 2m T as part of background 
!      i_gsdcldanal_type    - options for how GSD cloud analysis should be conducted         
!                         =0. no cloud analysis (default)
!                         =1.  cloud analysis after var analysis for WRF_ARW
!                         =2.  cloud analysis after var analysis for NMMB
!                         =3.  cloud analysis only; var is skipped
!                         =5.  skip cloud analysis and updating NETCDF result file at
!                                         the end of the analysis
!                         =6.  skip NETCDF background read step and do cloud analysis only
!                         =7   cloud analysis in observer with I/O
!                         =30  cloud analysis for GFS
!                         =99  only read hydrometer fields but no cloud analysis

!      i_gsdsfc_uselist  - options for how to use surface observation use or
!                          rejection list
!                         =0 . EMC method (default)
!                         =1 . GSD method
!      i_lightpcp        - options for how to deal with light precipitation
!                         =0 . don''t add light precipitation (default)
!                         =1 . add light precipitation in warm section
!      i_sfct_gross      - if use extended threshold for surface T gross check
!                         =0 use threshold from convinfo (default)
!                         =1 for cold surface, threshold for gross check is
!                         enlarged to bring more large negative innovation into
!                         analysis.
!      l_numconc         - namelist logical to update cloud water and cloud ice
!                          number concentrations. 
!                         =false do not update num conc
!                         =true update num conc
!      l_use_hydroretrieval_all - the precipitation analysis use reflectivity
!                                 purely
!      l_closeobs        - namelist logical to pick the obs close to analysis
!                          time.
!                         =false do not pick, use obs error inflation with duplication
!                         =true only pick the obs close to analysis time only.
!      i_coastline        - options to turn on observation operator for coastline surface observations
!                         =0. turn off observation operator for coastline
!                         surface observations (default)
!                         =1.  for temperature surface observations
!                         =2.  for moisture surface observations
!                         =3.  for temperature and moisture surface observations
!      i_gsdqc            - option i_gsdqc to turn on special observation qc
!                              from GSD (for RAP/HRRR application)
!                         =0 turn off
!                         =2 turn on
!      qv_max_inc        - threshold to limit the maximum water vapor increment
!      ioption           - interpolation option for satellite mapping 
!                         =1  if selection is nearest neighbor
!                         =2  if selection is median of samples
!      l_precip_clear_only - the precipitation analysis only clears; it does not
!                            make any updates for positive precipitating hydrometeors
!      l_fog_off           - turn off using fog observations
!      cld_bld_coverage    - cloud coverage required for qc/qi building
!      cld_clr_coverage    - cloud coverage required for qc/qi clearing
!      i_cloud_q_innovation - integer to choose if and how cloud obs are used
!                          0= no innovations 
!                          1= cloud total innovations
!                          20= cloud build/clear derived water vapor innovations
!                          21= cloud build derived water vapor innovations
!                          22= cloud clear derived water vapor innovations
!                          3= cloud total & water vapor innovations
!      i_ens_mean    - integer for setupcldtot behavior
!                           0=single model run
!                           1=ensemble mean
!                           2=ensemble members
!      DTsTmax       - maximum allowed difference between Tskin and the first
!                           level T. This is to safety guard soil T adjustment.
!      i_T_Q_adjust -     =0 no temperature and moisture adjustment in hydrometeor analyis
!                         =1 (default) temperature and moisture are adjusted in hydrometeor analyis
!                         =2 temperature and moisture only adjusted for clearing (warmer, drier)
!      l_saturate_bkCloud - if .true. ensure saturation for all cloud 3-d points in background
!                           where observed cloud cover is missing (default:true).
!      l_rtma3d      - logical option for turning on configuration for RTMA3D
!                           (default is .FALSE.)
!      i_precip_vertical_check - integer option for checking and adjusting
!                                Qr/Qs/Qg and Qnr after cloud analysis
!                                to reduce the background reflectivity ghost in
!                                analysis. (default is 0)
!                           = 0(no adjustment)
!                           = 1(Clean off Qg only, where dbz_obs_max<=35dbz in the profile)
!                           = 2(clean Qg as in 1, and adjustment to the retrieved Qr/Qs/Qnr throughout the whole profile)
!                           = 3(similar to 2, but adjustment to Qr/Qs/Qnr only below maximum reflectivity level
!                             and where the dbz_obs is missing);
!      corp_howv     - real, static background error of howv (stddev error)
!                           = 0.42 meters (default)
!      hwllp_howv    - real, background error de-correlation length scale of howv 
!                           = 170,000.0 meters (default 170 km)
!      corp_gust     - real, static background error of gust (stddev error)
!      hwllp_gust    - real, background error de-correlation length scale of gust 
!      oerr_gust     - real, observation error of gust
!
  namelist/rapidrefresh_cldsurf/dfi_radar_latent_heat_time_period, &
                                metar_impact_radius,metar_impact_radius_lowcloud, &
                                l_metar_impact_radius_change,metar_impact_radius_max,&
                                metar_impact_radius_min,metar_impact_radius_max_height,&
                                metar_impact_radius_min_height,l_gsd_terrain_match_surftobs, &
                                l_sfcobserror_ramp_t,l_sfcobserror_ramp_q, &
                                l_pbl_pseudo_surfobst,l_pbl_pseudo_surfobsq,l_pbl_pseudo_surfobsuv, &
                                pblh_ration,pps_press_incr,l_gsd_limit_ocean_q, &
                                l_pw_hgt_adjust, l_limit_pw_innov, max_innov_pct, &
                                l_cleansnow_warmts,l_conserve_thetaV,r_cleansnow_warmts_threshold,  &
                                i_conserve_thetav_iternum,l_gsd_soiltq_nudge,l_cld_bld, cld_bld_hgt, &
                                build_cloud_frac_p, clear_cloud_frac_p,   &
                                nesdis_npts_rad, &
                                iclean_hydro_withRef,iclean_hydro_withRef_allcol,&
                                i_use_2mq4b,i_use_2mt4b,i_gsdcldanal_type,i_gsdsfc_uselist, &
                                i_lightpcp,i_sfct_gross,l_use_hydroretrieval_all,l_numconc,l_closeobs,&
                                i_coastline,i_gsdqc,qv_max_inc,ioption,l_precip_clear_only,l_fog_off,&
                                cld_bld_coverage,cld_clr_coverage,&
                                i_cloud_q_innovation,i_ens_mean,DTsTmax, &
                                i_T_Q_adjust,l_saturate_bkCloud,l_rtma3d,i_precip_vertical_check, &
                                corp_howv, hwllp_howv, corp_gust, hwllp_gust, oerr_gust

! chem(options for gsi chem analysis) :
!     berror_chem       - .true. when background  for chemical species that require
!                          conversion to lower case and/or species names longer than 5 chars
!     berror_fv3_cmaq_regional   - .true. use background error stat for online
!                                         RRFS_CMAQ model. Control variable
!                                         names extended up to 10 chars
!     berror_fv3_sd_regional     - .true. use background error stat for online
!                                         RRFS_SD model. Control variable
!                                         names extended up to 10 chars
!     oneobtest_chem    - one-ob trigger for chem constituent analysis
!     maginnov_chem     - O-B make-believe residual for one-ob chem test
!     magoberr_chem     - make-believe obs error for one-ob chem test
!     oneob_type_chem   - type of chem-ob for one-ob test
!     oblat_chem        - latitude of make-believe chem obs
!     oblon_chem        - longitude of make-believe chem obs
!     obpres_chem       - pressure level of make-believe chem obs
!     diag_incr         - increment for CMAQ
!     elev_tolerance    - in meters when surface PM observation rejected due to elevation
!                         disparity in background and observation
!     tunable_error     - a factor to calculate representativeness error for PM observations
!     in_fname          - CMAQ input filename
!     out_fname         - CMAQ output filename
!     incr_fname        - CMAQ increment filename
!     laeroana_gocart   - when true, do chem analysis with wrfchem (or NGAC)
!     laeroana_fv3cmaq  - when true, do chem analysis with fv3 lam (both fv3_cmaq_regional and fv3_regional are true!) 
!     l_aoderr_table    - whethee to use aod error table or default error
!     aod_qa_limit      - minimum acceptable value of error flag for total column AOD
!     luse_deepblue     - whether to use MODIS AOD from the deepblue   algorithm
!     lread_ext_aerosol - if true, reads aerfNN file for aerosol arrays rather than sigfNN (NGAC NEMS IO)

  namelist/chem/berror_chem,berror_fv3_cmaq_regional,berror_fv3_sd_regional,& 
       oneobtest_chem,anowbufr_ext,maginnov_chem,magoberr_chem,&
       oneob_type_chem,oblat_chem,oblon_chem,obpres_chem,&
       diag_incr,elev_tolerance,tunable_error,&
       in_fname,out_fname,incr_fname,&
       laeroana_gocart, laeroana_fv3cmaq,laeroana_fv3smoke,l_aoderr_table, aod_qa_limit, &
       crtm_aerosol_model,crtm_aerosolcoeff_format,crtm_aerosolcoeff_file, &
       icvt_cmaq_fv3,pm2_5_innov_threshold, &
       pm2_5_innov_threshold,pm2_5_urban_innov_threshold,pm2_5_bg_threshold,&
       raod_radius_mean_scale,raod_radius_std_scale, luse_deepblue,&
       aero_ratios,wrf_pm2_5, lread_ext_aerosol

! NST (NSST control namelist) :
!     nst_gsi  - indicator to control the Tr Analysis mode: 0 = no nst info in gsi at all;
!                                                           1 = input nst info, but used for monitoring only
!                                                           2 = input nst info, and used in CRTM simulation, but no Tr analysis
!                                                           3 = input nst info, and used in CRTM simulation and Tr analysis is on
!     nstinfo  - number of nst variables
!     zsea1    - upper depth (in mm) for vertical mean of T based on NSST T-Profile
!     zsea2    - lower depth (in mm) for vertical mean of T based on NSST T-Profile
!     fac_dtl  - index to apply diurnal thermocline layer  or not: 0 = no; 1 = yes.
!     fac_tsl  - index to apply thermal skin layer or not: 0 = no; 1 = yes.
   namelist/nst/nst_gsi,nstinfo,zsea1,zsea2,fac_dtl,fac_tsl

!  Initialize the Cloud and Aerosol Detection Software (CADS) 
!
!     M__Sensor                     Unique ID for sensor
!     N__Num_Bands                  Number of channel bands
!     N__Band_Size(:)               Number of channels in each band
!     N__Bands(:,:)                 Channel lists
!     N__Window_Width(:)            Smoothing filter window widths per band
!     N__Window_Bounds(:,:)         Channels in the spectral window gradient check
!     N__GradChkInterval(:)         Window width used in gradient calculation
!     R__BT_Threshold(:)            BT threshold for cloud contamination
!     R__Grad_Threshold(:)          Gradient threshold for cloud contamination
!     R__Window_Grad_Threshold(:)   Threshold for window gradient check in QE
!     L__Do_Quick_Exit              On/off switch for the Quick Exit scenario
!     L__Do_CrossBand               On/off switch for the cross-band method
!     N__BandToUse(:)               Band number assignment for each channel
!     L__Do_Imager_Cloud_Detection  On/off switch for the imager cloud detection
!     N__Num_Imager_Chans           No. of imager channels
!     N__Num_Imager_Clusters        No. of clusters to be expected
!     N__Imager_Chans(:)            List of imager channels
!     R__Stddev_Threshold(:)        St. Dev. threshold, one for each imager channel
!     R__Coverage_Threshold         Threshold for fractional coverage of a cluster
!     R__FG_Departure_Threshold     Threshold for imager FG departure

   NAMELIST / Cloud_Detect_Coeffs / M__Sensor, N__Num_Bands,            &
           N__Band_Size, N__Bands, N__Window_Width, N__Window_Bounds,   &
           N__GradChkInterval, R__BT_Threshold, R__Grad_Threshold,      &
           R__Window_Grad_Threshold, L__Do_Quick_Exit,                  &
           L__Do_CrossBand, N__BandToUse,                               &
           L__Do_Imager_Cloud_Detection, N__Num_Imager_Chans,           &
           N__Num_Imager_Clusters, N__Imager_Chans,                     &
           R__Stddev_Threshold, R__Coverage_Threshold,                  &
           R__FG_Departure_Threshold


!EOC

!---------------------------------------------------------------------------

   CONTAINS

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOP

! ! IROUTINE: gsimain_initialize

! ! INTERFACE:

  subroutine gsimain_initialize

!*************************************************************
! Begin gsi code
!
  use m_gpsStats, only: gpsStats_create ! was done within obsmod::create_obsmod_vars()
  use m_prad    , only: prad_create     ! was obsmod::create_passive_obsmod_vars()
  use m_obsdiags, only: obsdiags_create ! was done within obsmod::create_obsmod_vars()

  use mpeu_util,only: die
  use gsi_4dcouplermod, only: gsi_4dcoupler_parallel_init
  use gsi_4dcouplermod, only: gsi_4dcoupler_setservices
  implicit none
  character(len=*),parameter :: myname_='gsimod.gsimain_initialize'
  integer:: ier,ios
  real(r_kind):: varqc_max,c_varqc_new
  type(regional_io_class) :: regional_io

  call gsi_4dcoupler_parallel_init

  call mpi_comm_size(mpi_comm_world,npe,ierror)
  call mpi_comm_rank(mpi_comm_world,mype,ierror)
  if (mype==0) call w3tagb('GSI_ANL',1999,0232,0055,'NP23')

! Initialize defaults of vars in modules
  call init_4dvar
  call regional_io%init_regional_io
  call init_nems

! Read in user specification of state and control variables
  call gsi_metguess_init
  call gsi_chemguess_init
  call init_anasv
  call init_anacv
  call init_wrf_vars
  call radiance_mode_init

  call init_constants_derived
  call init_oneobmod
  call init_qcvars
  call init_obsmod_dflts
  call init_pcp
  call init_light
  call init_rad
  call init_oz
  call init_aero
  call init_co
  call init_convinfo
  call init_jfunc
  call init_balmod
  call init_berror
  call init_anberror  ! RTodling: alloc vectors should move to create
  call init_grid
  call init_turbl
  call init_compact_diffs
  call init_smooth_polcas  
  call init_jcvars
  call init_strongvars
  call initialize_superob_radar
  call init_io(mype,npe-1)
  call init_vtrans
  call init_obsens
  call init_hybrid_ensemble_parameters
  call init_rapidrefresh_cldsurf
  call init_chem
  call init_tcps_errvals
  call init_aircraft
  call init_gfs_stratosphere
  call set_fgrid2agrid
  call gsi_nstcoupler_init_nml
  call init_radaruse_directDA
  call CADS_Setup_Cloud

 if(mype==0) write(6,*)' at 0 in gsimod, use_gfs_stratosphere,nems_nmmb_regional = ', &
                       use_gfs_stratosphere,nems_nmmb_regional


! Read user input from namelists.  All processor elements 
! read the namelist input.  SGI MPI FORTRAN does not allow
! all tasks to read from standard in (unit 5).  Hence, open
! namelist to different unit number and have each task read 
! namelist file.
#ifdef ibm_sp
! Initialize table of instruments and data types
  read(5,setup) 
  read(5,gridopts)

! call to obsmod_init_instr_table must be after setup and gridopts are read in
  call obsmod_init_instr_table(nhr_assimilation,ndat)
  read(5,bkgerr)
  read(5,anbkgerr)
  read(5,jcopts)
  read(5,strongopts)
  read(5,obsqc)
  read(5,obs_input)
  read(5,superob_radar)
  read(5,lag_data)
  read(5,hybrid_ensemble)
  read(5,rapidrefresh_cldsurf)
  read(5,chem)
  read(5,nst)
#else
! Initialize table of instruments and data types
  call obsmod_init_instr_table(nhr_assimilation,ndat,rcname='gsiparm.anl')
  open(11,file='gsiparm.anl')
  read(11,setup,iostat=ios)
  if(ios/=0) call die(myname_,'read(setup)',ios)  
  close(11)

  open(11,file='gsiparm.anl')
  read(11,gridopts,iostat=ios)
  if(ios/=0) call die(myname_,'read(gridopts)',ios)

! call to obsmod_init_instr_table must be after setup and gridopts are read in

  read(11,bkgerr,iostat=ios)
  if(ios/=0) call die(myname_,'read(bkgerr)',ios)

  read(11,anbkgerr,iostat=ios)
  if(ios/=0) call die(myname_,'read(anbkgerr)',ios)

  read(11,jcopts,iostat=ios)
  if(ios/=0) call die(myname_,'read(jcopts)',ios)

  read(11,strongopts,iostat=ios)
  if(ios/=0) call die(myname_,'read(strongopts)',ios)

  read(11,obsqc,iostat=ios)

  if(ios/=0) call die(myname_,'read(obsqc)',ios)

  read(11,obs_input,iostat=ios)
  if(ios/=0) call die(myname_,'read(obs_input)',ios)

  read(11,superob_radar,iostat=ios)
  if(ios/=0) call die(myname_,'read(superob_radar)',ios)

  read(11,lag_data,iostat=ios)
  if(ios/=0) call die(myname_,'read(lag_data)',ios)

  read(11,hybrid_ensemble,iostat=ios)
  if(ios/=0) call die(myname_,'read(hybrid_ensemble)',ios)

  read(11,rapidrefresh_cldsurf,iostat=ios)
  if(ios/=0) call die(myname_,'read(rapidrefresh_cldsurf)',ios)

  read(11,chem,iostat=ios)
  if(ios/=0) call die(myname_,'read(chem)',ios)

  read(11,nst,iostat=ios)
  if(ios/=0) call die(myname_,'read(nst)',ios)

  close(11)
#endif
  if(jcap > jcap_cut)then
    jcap_cut = jcap+1
    if(mype == 0)then
      write(6,*) ' jcap_cut increased to jcap+1 = ', jcap+1
      write(6,*) ' jcap_cut < jcap+1 not allowed '
    end if
  end if
  if(vqc .and. niter_no_qc(1) < niter(1))then
     varqc_max=c_varqc*(niter(1)-niter_no_qc(1))
     if(varqc_max < one .and. varqc_max > zero)then
        c_varqc_new=one/(niter(1)-niter_no_qc(1))
        if(mype == 0) write(6,*) 'Warning - value for c_varqc does not allow ',&
            'variational qc to turn on completely in first outer iteration', &
            'c_varqc adjusted c_varqc - old =',c_varqc,  &
            'c_varqc - new  =',c_varqc_new
        c_varqc=c_varqc_new
     end if
  end if
  if(l_both_fv3sar_gfs_ens) then
    if(n_ens /= n_ens_gfs + n_ens_fv3sar .or. regional_ensemble_option /= 5 ) then 
       write(6,*)'the set up for l_both_fv3sar_gfs_ens=.true. is wrong,stop'
       call stop2(137)
    endif
  else
    if (regional_ensemble_option==5) then 
       n_ens_gfs=0
       n_ens_fv3sar=n_ens
    elseif (regional_ensemble_option==1) then 
       n_ens_gfs=n_ens
       n_ens_fv3sar=0
    else 
       if(mype == 0)write(6,*)'n_ens_gfs and n_ens_fv3sar won"t be used if not regional_ensemble_option==5' 
    endif
    weight_ens_gfs=one
    weight_ens_fv3sar=one
  endif
  if(ltlint) then
     if(vqc .or. njqc .or. nvqc)then
       vqc = .false.
       nvqc = .false.
       njqc = .false.
       if(mype == 0) write(6,*) ' ltlint = true, so vqc and njqc must be false'
     end if
  end if
  if (fv3sar_bg_opt /= 0) l_reg_update_hydro_delz=.false.
  if(regional_ensemble_option == 5 .and. (fv3sar_ensemble_opt /= fv3sar_bg_opt)) then
    write(6,*)'this setup doesn"t work, stop'
    call stop2(137)
  endif 
  if (anisotropic) then
      call init_fgrid2agrid(pf2aP1)
      call init_fgrid2agrid(pf2aP2)
      call init_fgrid2agrid(pf2aP3)
  endif

! 4D-Var setup
  call setup_4dvar(mype)
  if (l4dvar) then
     if(reduce_diag) &
     call die(myname_,'Options l4dvar and reduce_diag not allowed together',99)
  end if 

  if( (.not.l4dvar) .and. (.not.l4densvar) ) ljcdfi=.false.
 
  if (l4dvar.and.lsensrecompute) then
     lobsensfc  =lobsensfc  .and.(jiterstart==jiterend)
     lobsensjb  =lobsensjb  .and.(jiterstart==jiterend)
     lobsensincr=lobsensincr.and.(jiterstart==jiterend)
  endif
  lobsensfc=lobsensfc.or.lobsensjb.or.lobsensincr
  lsensrecompute=lsensrecompute.and.lobsensfc
  if (lobsensadj .and. .not.lcongrad) then
     write(6,*)'gsimod: adjoint computation requires congrad',lobsensadj,lcongrad
     call stop2(137)
  end if
  if (jsiga>0 .and. .not.lcongrad) then
     write(6,*)'gsimod: analysis error estimate requires congrad',jsiga,lcongrad
     call stop2(137)
  endif

  ntotensgrp=nsclgrp*ngvarloc
  if(l_timloc_opt) then
     naensgrp=ntotensgrp*ntlevs_ens
  else
     naensgrp=ntotensgrp
  endif
  if( (.not. global_spectral_filter_sd) .and. (.not. assign_vdl_nml) .and. naensloc<naensgrp+nsclgrp-1) then
     naensloc=naensgrp+nsclgrp-1
  end if
  if(mype==0) write(6,*) 'in gsimod: naensgrp,ntotensgrp,nsclgrp,ngvarloc,ntlevs_ens= ', &
                          naensgrp,ntotensgrp,nsclgrp,ngvarloc,ntlevs_ens

  call gsi_4dcoupler_setservices(rc=ier)
  if(ier/=0) call die(myname_,'gsi_4dcoupler_setServices(), rc =',ier)


! Check user input for consistency among parameters for given setups.

! Set regional parameters
  if(filled_grid.and.half_grid) filled_grid=.false.
  regional=wrf_nmm_regional.or.wrf_mass_regional.or.twodvar_regional.or.nems_nmmb_regional .or. cmaq_regional
  regional=regional.or.fv3_regional.or.fv3_cmaq_regional

! Force turn off MGBF-based localization except for regional application
  if(.not.regional.and.l_mgbf_loc) then
     l_mgbf_loc=.false.
     if(mype==0) write(6,*)'GSIMOD: for global app, l_mgbf_loc is not applicable, reset l_mgbf_loc=',l_mgbf_loc
  end if

! Force turn off MGBF-based localization for lsqrtb=.true.
  if(lsqrtb.and.l_mgbf_loc) then
     l_mgbf_loc=.false.
     if(mype==0) write(6,*)'GSIMOD: for lsqrtb=.true., l_mgbf_loc is not applicable, reset l_mgbf_loc=',l_mgbf_loc
  end if

! Currently only able to have use_gfs_stratosphere=.true. for nems_nmmb_regional=.true.
  use_gfs_stratosphere=use_gfs_stratosphere.and.(nems_nmmb_regional.or.wrf_nmm_regional)   
  if(mype==0) write(6,*) 'in gsimod: use_gfs_stratosphere,nems_nmmb_regional,wrf_nmm_regional= ', &  
                          use_gfs_stratosphere,nems_nmmb_regional,wrf_nmm_regional                  
! Given the requested resolution, set dependent resolution parameters
  if(jcap_gfs == 1534)then
     nlat_gfs=1538
     nlon_gfs=3072
  else if(jcap_gfs == 574)then
     nlat_gfs=578
     nlon_gfs=1152
  else if(jcap_gfs == 382)then
     nlat_gfs=386
     nlon_gfs=768
  else if(jcap_gfs == 126)then
     nlat_gfs=130
     nlon_gfs=256
  else if(jcap_gfs == 62)then
     nlat_gfs=96
     nlon_gfs=192
  else
     if(mype == 0) write(6,*)' Invalid jcap_gfs'
     call stop2(329)
  end if

!  reg_tlnmc_type=2 currently requires that 2*nvmodes_keep <= npe
  if(reg_tlnmc_type==2) then
     if(2*nvmodes_keep>npe) then
        if(mype==0) write(6,*)' reg_tlnmc_type=2 and nvmodes_keep > npe'
        if(mype==0) write(6,*)' npe, old value of nvmodes_keep=',npe,nvmodes_keep
        nvmodes_keep=npe/2
        if(mype==0) write(6,*)'    new nvmodes_keep, npe=',nvmodes_keep,npe
     end if
  end if

  if (tlnmc_option>=2 .and. tlnmc_option<=4) then
     if (.not.l_hyb_ens) then
     if(mype==0) write(6,*)' GSIMOD: inconsistent set of options for Hybrid/EnVar & TLNMC = ',l_hyb_ens,tlnmc_option
     if(mype==0) write(6,*)' GSIMOD: resetting tlnmc_option to 1 for 3DVAR mode'
     tlnmc_option=1
     end if
  else if (tlnmc_option<0 .or. tlnmc_option>4) then
     if(mype==0) write(6,*)' GSIMOD: This option does not yet exist for tlnmc_option: ',tlnmc_option
     if(mype==0) write(6,*)' GSIMOD: Reset to default 0'
     tlnmc_option=0
  end if
  if (tlnmc_option>0 .and. tlnmc_option<5) then
     l_tlnmc=.true.
     if(mype==0) write(6,*)' GSIMOD: valid TLNMC option chosen, setting l_tlnmc logical to true'
  end if
! check consistency in q option
  if(pseudo_q2 .and. qoption==1)then
     if(mype==0)then
       write(6,*)' pseudo-q2 = ', pseudo_q2, ' qoption = ', qoption
       write(6,*)' pseudo-q2 must be used together w/ qoption=2 only, aborting.'
       call stop2(999)
     endif
  endif

  if (innov_use_model_fed .and. .not.if_model_fed) then
     if(mype==0) write(6,*)' GSIMOD: invalid innov_use_model_fed=.true. but if_model_fed=.false.'
     call die(myname_,'invalid innov_use_model_fed,if_model_fed, check namelist settings',330)
  end if

  if (.not. (miter == 0 .or. lobserver) .and. if_model_fed .and. .not. fed_exist) then
     if(mype==0) write(6,*)' GSIMOD: .not. (miter == 0 .or. lobserver) and if_model_fed=.true. but fed is not in anavinfo file'
     call die(myname_,'Please check namelist parameters and/or add fed in anavinfo (contro/state_vector and met_guess) when miter > 0 and if_model_fed=.true.',332)
  end if

  if (.not. (miter == 0 .or. lobserver) .and. if_model_dbz .and. .not. dbz_exist) then
     if(mype==0) write(6,*)' GSIMOD: .not. (miter == 0 .or. lobserver) and if_model_dbz=.true. but dbz is not in anavinfo file'
     call die(myname_,'Please check namelist parameters and/or add dbz in anavinfo (contro/state_vector and met_guess) when miter > 0 and if_model_fed=.true.',334)
  end if

! Ensure valid number of horizontal scales
  if (nhscrf<0 .or. nhscrf>3) then
     if(mype==0) write(6,*)' GSIMOD: invalid specifications for number of horizontal scales nhscrf = ',nhscrf
     call die(myname_,'invalid nhscrf, check namelist settings',336)
  end if


  writediag=.false.
  do i=1,miter+1
     if(write_diag(i))writediag=.true.
  end do
  if(.not. writediag)then
     diag_rad=.false.
     diag_conv=.false.
     diag_ozone=.false.
     diag_aero=.false.
     diag_co=.false.
     diag_pcp=.false.
     diag_light=.false.
     diag_radardbz=.false.
     diag_fed=.false.
     use_limit = 0
  end if
  if(reduce_diag) use_limit = 0

! Force use of perturb_obs for oberror_tune
  if (oberror_tune ) then
     perturb_obs=.true.
     if (mype==0) write(6,*)'GSIMOD:  ***WARNING*** reset perturb_obs=',perturb_obs
  endif

! Force turn off cloud analysis and hydrometeor IO
  if (i_gsdcldanal_type==0) then
     l_hydrometeor_bkio = .false.
     if (mype==0) write(6,*)'GSIMOD:  ***WARNING*** set l_hydrometeor_bkio=false'
  else if(i_gsdcldanal_type==1 .or. i_gsdcldanal_type==2 .or. &
          i_gsdcldanal_type==3 .or. i_gsdcldanal_type==5 .or. &
          i_gsdcldanal_type==6 .or. i_gsdcldanal_type==7 .or. &
          i_gsdcldanal_type==99 ) then
     l_hydrometeor_bkio = .true.
     if (mype==0) write(6,*)'GSIMOD:  set l_hydrometeor_bkio=true:',i_gsdcldanal_type
  endif
! turn on hydrometeor IO for direct reflectivity DA
  if ( l_use_dbz_directDA) then
     l_hydrometeor_bkio = .true.  ! activate hydrometer IO
     if (mype==0) write(6,*)'GSIMOD:  ***WARNING*** set l_hydrometeor_bkio=true &
                            for direct Reflectivity DA capability', l_hydrometeor_bkio
  end if

  if((i_coastline == 1 .or. i_coastline == 3) .and. i_use_2mt4b==0) then
     i_coastline=0
     if (mype==0) write(6,*)'GSIMOD:  ***WARNING*** ',&
                    'set i_coastline=0 because i_use_2mt4b=0'
  endif
  if((i_coastline == 2 .or. i_coastline == 3) .and. i_use_2mq4b==0) then
     i_coastline=0
     if (mype==0) write(6,*)'GSIMOD:  ***WARNING*** ',&
                    'set i_coastline=0 because i_use_2mq4b=0'
  endif

! Finish initialization of observation setup
  call init_obsmod_vars(nhr_assimilation,mype)


! Force use of external observation error table for regional runs
  if (regional .and. .not.oberrflg) then
     oberrflg=.true.
     if (mype==0) write(6,*)'GSIMOD:  ***WARNING*** reset oberrflg=',oberrflg
  endif


! Set 10m wind factor logicals based on ifact10
  if (ifact10==1 .or. ifact10==2) then
     if (ifact10==1) sfcmod_gfs = .true.
     if (ifact10==2) sfcmod_mm5 = .true.
  endif


! If strong constraint is turned off, force other strong constraint variables to zero
  if ((.not.l_tlnmc) .and. nstrong/=0 ) then
     nstrong=0
     if (mype==0) write(6,*)'GSIMOD:  reset nstrong=',nstrong,&
          ' because TLNMC option is set to off= ',tlnmc_option
  endif
  if (.not.l_tlnmc) then
     baldiag_full=.false.
     baldiag_inc =.false.
  end if

! Warning of reflectivity assimilation with static B
  if ( beta_s0 > 0.0_r_kind )then
    ! skipped in case of direct reflectivity DA because it works in Envar and hybrid
    if ( l_use_rw_columntilt .or. l_use_dbz_directDA) then
       do i=1,ndat
          if ( if_model_dbz .and. (index(dtype(i), 'dbz') /= 0) )then
             if (mype==0) then
                write(6,*)'GSIMOD:  ***WARNING*** static B for reflectivity is regarded as zero in this GSI version &
                           even though beta_s0 =',beta_s0
                write(6,*)'Static B extended for radar reflectivity assimilation will be included in future version.'
             end if
          end if
       end do
    end if
  end if

! Turn off uv option if hybrid/ensemble options is false for purposes 
! of TLNMC 
  if (.not.l_hyb_ens) uv_hyb_ens=.false.

! Turn on derivatives if using dynamic constraint
! For now if wrf mass or 2dvar no dynamic constraint
  if (l_tlnmc) tendsflag=.true.
  if (l_foto) then
     if(mype == 0)write(6,*) 'Warning foto option has been removed'
     call stop2(899)
      
  end if
  if (tendsflag) switch_on_derivatives=.true.


! Turn off Jc-pdry weak constraint if regional application
  if (regional) ljcpdry=.false.

! Initialize lagrangian data assimilation - must be called after gsi_4dvar
  call lag_modini()


! Ensure tendency flag is on if preciptation data listed as observation type.
! NOTE: only applicable for global run when not using observer
  if (.not.tendsflag .and. .not.regional) then
     check_pcp: do i=1,ndat
        if ( .not.tendsflag .and. index(dtype(i),'pcp') /=0 ) then
           tendsflag = .true.
           switch_on_derivatives=.true.
           if (mype==0) write(6,*)'GSIMOD:  set tendsflag,switch_on_derivatives=',&
                tendsflag,switch_on_derivatives,' for pcp data'
           exit check_pcp
        endif
     end do check_pcp
  endif

! Ensure no conflict between flag lread_obs_save and lread_obs_skip
  if (lread_obs_save .and. lread_obs_skip) then
     if (mype==0) write(6,*)'GSIMOD:  ***ERROR*** lread_obs_save=',lread_obs_save,&
          ' and lread_obs_skip=',lread_obs_skip,' can not both be TRUE'
     call stop2(329)
  endif
  if (l4densvar .and. (.not.ljc4tlevs) ) then
     if( ljcpdry .or. (factqmin>zero) .or. (factqmax>zero) )  then
        if (mype==0) write(6,*)'GSIMOD: **WARNING**, option for Jc terms over all time', &
                              ' levels not activated with 4Densvar'
        if (mype==0) write(6,*)'GSIMOD: **WARNING**, This configuration not recommended,',&
                              ' limq/pdry will only be applied to center of window '
     end if
  end if

! Optionally read in namelist for single observation run
  if (oneobtest) then
     miter=1
     dfile(1)='prepqc'
     time_window(1)=three
     dplat='oneob'
     dthin=1
     dval=one
     dmesh=one
     factqmin=zero
     factqmax=zero
     superfact=1._r_kind
     limitqobs=.false.
     if (hilbert_curve) then
        write(6,*) 'Disabling hilbert_curve cross validation when oneobtest=.true.'
        hilbert_curve=.false.
     end if
#ifdef ibm_sp
     read(5,singleob_test)
#else
     open(11,file='gsiparm.anl')
     read(11,singleob_test,iostat=ios)
        if(ios/=0) call die(myname_,'read(singleob_test)',ios)
     close(11)
#endif
     dtype(1)=oneob_type
     if(dtype(1)=='u' .or. dtype(1)=='v')dtype(1)='uv'
     dsis(1)=dtype(1)
     if(trim(dtype(1))=='rw') then ! Reset some values for radial wind single ob.
        dfile(1)='l2rwbufr'
        dplat='null'
        dsis(1)='l2rw'
     endif
  endif

! Single radiance assimilation case
  if (lsingleradob) then
#ifdef ibm_sp 
     read(5,singleob_test)
#else 
     open(11,file='gsiparm.anl')
     read(11,singleob_test,iostat=ios)
     if(ios/=0) call die(myname_,'read(singleob_test)',ios)
     close(11)
#endif 
  endif

! reading namelist for using directly radar DA capabilities
  if (l_use_rw_columntilt .or. l_use_dbz_directDA) then
#ifdef ibm_sp
     read(5,radaruse_directDA)
#else
     open(11,file='gsiparm.anl')
     read(11,radaruse_directDA,iostat=ios)
     if(ios/=0) call die(myname_,'read(radaruse_directDA)',ios)
     close(11)
#endif

  endif


! Write namelist output to standard out
  if(mype==0) then
     write(6,200)
200  format(' calling gsisub with following input parameters:',//)
     write(6,setup)
     write(6,gridopts)
     write(6,bkgerr)
     write(6,anbkgerr)
     write(6,jcopts)
     write(6,strongopts)
     write(6,obsqc)
     write(6,*)'EXT_SONDE on type 120 =',ext_sonde
     ngroup=0
     do i=1,ndat
        dthin(i) = max(dthin(i),0)
        if(dthin(i) > ngroup)ngroup=dthin(i)
     end do
     if(ngroup>0) then
       if (ngroup<size(dmesh)) then
          write(6,*)' ngroup = ',ngroup,' dmesh = ',(dmesh(i),i=1,ngroup)
 400      format(' ngroup = ',I5,' dmesh = ',(5f10.2))
       else
          call die(myname_,'dmesh size needs increasing',99)
       endif
     endif
     do i=1,ndat
        write(6,401)dfile(i),dtype(i),dplat(i),dsis(i),dval(i),dthin(i),dsfcalc(i),time_window(i)
 401    format(1x,a20,1x,a10,1x,a12,1x,a20,1x,f10.2,1x,I3,1x,I3,1x,f10.2)
     end do
     write(6,superob_radar)
     write(6,lag_data)
     write(6,hybrid_ensemble)
     write(6,rapidrefresh_cldsurf)
     write(6,chem)
     if (oneobtest) write(6,singleob_test)
     write(6,nst)
     if (l_use_rw_columntilt .or. l_use_dbz_directDA) write(6,radaruse_directDA)
  endif

! Set up directories (or pe specific filenames)
  call init_directories(mype,npe)

! Initialize space for qc
  call create_qcvars

! Initialize constants
  call init_constants(regional)
  call gps_constants(use_compress)

! If this is a wrf regional run, then run interface with wrf
  update_pint=.false.
  if (regional) then
     if (fv3_regional) then
        call convert_fv3_regional
     else
        if(i_gsdcldanal_type.ne.6) call regional_io%convert_regional_guess(mype,ctph0,stph0,tlm0)
     endif
  endif
            
  if (regional.and.use_gfs_stratosphere) call broadcast_gfs_stratosphere_vars


! Initialize variables, create/initialize arrays
  call init_reg_glob_ll(mype,lendian_in)
  call init_grid_vars(jcap,npe,cvars3d,cvars2d,nrf_var,mype)
  if (switch_on_derivatives) call init_anadv  ! moved from derivsmod 
  call init_general_commvars
  call create_obsmod_vars
  call gpsStats_create()                ! extracted from obsmod::create_obsmod_vars()
  call obsdiags_create()                ! extracted from obsmod::create_obsmod_vars()
  if (passive_bc) call prad_create()    ! replacing -- call obsmod::create_passive_obsmod_vars()

  
! Initialize values in radinfo
  call init_rad_vars

! Initialize values in aeroinfo
  call init_aero_vars

! Initialize values in the radar emulator
  iret_init_mphyopt = -1
  if ( l_use_dbz_directDA ) then
! Stop GSI if 'if_model_dbz' and 'l_use_dbz_directDA' are set as .true. 
     if ( if_model_dbz ) then
         call die('init_mphyopt', 'if_model_dbz in &SETUP should be set as .false.', mphyopt)
     end if

     call init_mphyopt(mype,iret_init_mphyopt)
! Check microphysics scheme if assimilating radar dBZ
     if ( iret_init_mphyopt /= 0 ) then
         call die('init_mphyopt', 'Invalid microphysics option for dbz assimilation:', mphyopt)
     end if

! Initialize coefficients and power index numers used in the dbz obs operator
! for single moment scheme
     if (mphyopt  >= 2 .and. mphyopt  <= 7) then
        iret_coef4dbzfwrd = -1
        call coef4dbzfwrd(mphyopt,iret_coef4dbzfwrd)
        if ( iret_coef4dbzfwrd /= 0 ) then
            call die('COEF4DBZFWRD', 'Invalid microphysics option for single moment MP scheme:', mphyopt)
        end if
     end if
   end if



  end subroutine gsimain_initialize

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOP

! ! IROUTINE: gsimain_run

! ! INTERFACE:

  subroutine gsimain_run(init_pass,last_pass)
    use mpeu_util, only: tell
    implicit none
    logical, optional, intent(in):: init_pass  ! initial pass through multiple background bins
    logical, optional, intent(in):: last_pass  ! last pass through multiple background bins

!EOC

!---------------------------------------------------------------------------
  logical :: init_pass_
  logical :: last_pass_
 
  init_pass_ =.false.
  if(present(init_pass)) init_pass_=init_pass
  last_pass_  =.false.
  if(present(last_pass)) last_pass_ =last_pass

! Call the main gsi driver routine
  call gsisub(init_pass_,last_pass_)

  end subroutine gsimain_run

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOP

! ! IROUTINE: gsimain_finalize

 subroutine gsimain_finalize

! !REVISION HISTORY:
!
!  30May2010 Todling - add final_anasv and final_anacv
!  27Oct2013 Todling - get mype again for consistency w/ initialize
!                      revisit a few of finalizes for consistency w/ init
!
!EOC
!---------------------------------------------------------------------------

  use m_prad    , only: prad_destroy     ! was obsmod::destroyobs_passive()
  use m_obsdiags, only: obsdiags_destroy

  implicit none
! Deallocate arrays
! RTodling debug: PROG HANGS; needs ATTENTION
  call mpi_comm_rank(mpi_comm_world,mype,ierror)
  if (anisotropic) then
     call final_fgrid2agrid(pf2aP3)
     call final_fgrid2agrid(pf2aP2)
     call final_fgrid2agrid(pf2aP1)
  endif
  call radiance_obstype_destroy
  call final_aero_vars
  call final_rad_vars
  if(passive_bc) call prad_destroy()    ! replacing -- call destroyobs_passive
  call obsdiags_destroy()               !
  call destroy_obsmod_vars
  call destroy_general_commvars
  call final_grid_vars
!_TBDone  call final_reg_glob_ll ! gridmod
  call radiance_mode_destroy
  call final_anacv
  call final_anasv
  call obsmod_final_instr_table
  call gsi_chemguess_final
  call gsi_metguess_final
  call clean_4dvar
  call destroy_qcvars

! Done with GSI.
  if (mype==0)  call w3tage('GSI_ANL')
  
  call mpi_finalize(ierror)
 
 end subroutine gsimain_finalize

 end module gsimod
