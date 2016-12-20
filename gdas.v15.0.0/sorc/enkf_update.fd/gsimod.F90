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
     lread_obs_save,lread_obs_skip,create_passive_obsmod_vars,lwrite_predterms, &
     lwrite_peakwt,use_limit,lrun_subdirs,l_foreaft_thin,&
     obsmod_init_instr_table,obsmod_final_instr_table,destroyobs_passive
  use aircraftinfo, only: init_aircraft,aircraft_t_bc_pof,aircraft_t_bc, &
                          aircraft_t_bc_ext,biaspredt,upd_aircraft,cleanup_tail
  use obs_sensitivity, only: lobsensfc,lobsensincr,lobsensjb,lsensrecompute, &
                             lobsensadj,lobsensmin,iobsconv,llancdone,init_obsens
  use gsi_4dvar, only: setup_4dvar,init_4dvar,nhr_assimilation,min_offset, &
                       l4dvar,nhr_obsbin,nhr_subwin,nwrvecs,iorthomax,&
                       lbicg,lsqrtb,lcongrad,lbfgsmin,ltlint,ladtest,ladtest_obs, lgrtest,&
                       idmodel,clean_4dvar,iwrtinc,lanczosave,jsiga,ltcost,liauon, &
                       l4densvar,ens_nstarthr,lnested_loops,lwrite4danl,thin4d
  use obs_ferrscale, only: lferrscale
  use mpimod, only: npe,mpi_comm_world,ierror,mype
  use radinfo, only: retrieval,diag_rad,init_rad,init_rad_vars,adp_anglebc,angord,upd_pred,&
                       biaspredvar,use_edges,passive_bc,newpc4pred,final_rad_vars,emiss_bc,&
                       ssmis_method,ssmis_precond,gmi_method,amsr2_method
  use radinfo, only: tzr_qc,tzr_bufrsave
  use radinfo, only: crtm_coeffs_path
  use ozinfo, only: diag_ozone,init_oz
  use aeroinfo, only: diag_aero, init_aero, init_aero_vars, final_aero_vars
  use coinfo, only: diag_co,init_co
  use convinfo, only: init_convinfo,npred_conv_max, &
                      id_bias_ps,id_bias_t,id_bias_spd, &
                      conv_bias_ps,conv_bias_t,conv_bias_spd, &
                      stndev_conv_ps,stndev_conv_t,stndev_conv_spd,diag_conv,&
                      id_bias_pm2_5,conv_bias_pm2_5,&
                      id_bias_pm10,conv_bias_pm10,&
                      use_prepb_satwnd

  use oneobmod, only: oblon,oblat,obpres,obhourset,obdattim,oneob_type,&
     oneobtest,magoberr,maginnov,init_oneobmod,pctswitch,lsingleradob,obchan
  use balmod, only: fstat
  use turblmod, only: use_pbl,init_turbl
  use qcmod, only: dfact,dfact1,create_qcvars,destroy_qcvars,&
      erradar_inflate,tdrerr_inflate,tdrgross_fact,use_poq7,qc_satwnds,&
      init_qcvars,vadfile,noiqc,c_varqc,qc_noirjaco3,qc_noirjaco3_pole,&
      buddycheck_t,buddydiag_save,njqc,vqc
  use pcpinfo, only: npredp,diag_pcp,dtphys,deltim,init_pcp
  use jfunc, only: iout_iter,iguess,miter,factqmin,factqmax, &
     factv,factl,factp,factg,factw10m,facthowv,factcldch,niter,niter_no_qc,biascor,&
     init_jfunc,qoption,cwoption,switch_on_derivatives,tendsflag,l_foto,jiterstart,jiterend,R_option,&
     bcoption,diurnalbc,print_diag_pcg,tsensible,lgschmidt,diag_precon,step_start,pseudo_q2,&
     clip_supersaturation
  use state_vectors, only: init_anasv,final_anasv
  use control_vectors, only: init_anacv,final_anacv,nrf,nvars,nrf_3d,cvars3d,cvars2d,nrf_var
  use berror, only: norh,ndeg,vs,bw,init_berror,hzscl,hswgt,pert_berr,pert_berr_fct,&
     bkgv_flowdep,bkgv_rewgtfct,bkgv_write,fpsproj,nhscrf,adjustozvar,fut2ps,cwcoveqqcov
  use anberror, only: anisotropic,ancovmdl,init_anberror,npass,ifilt_ord,triad4, &
     binom,normal,ngauss,rgauss,anhswgt,an_vs,&
     grid_ratio,grid_ratio_p,an_flen_u,an_flen_t,an_flen_z, &
     rtma_subdomain_option,rtma_bkerr_sub2slab,nsmooth,nsmooth_shapiro,&
     pf2aP1,pf2aP2,pf2aP3,afact0,covmap,lreadnorm
  use compact_diffs, only: noq,init_compact_diffs
  use jcmod, only: init_jcvars,ljcdfi,alphajc,ljcpdry,bamp_jcpdry,eps_eer,ljc4tlevs
  use tendsmod, only: ctph0,stph0,tlm0
  use mod_vtrans, only: nvmodes_keep,init_vtrans
  use mod_strong, only: l_tlnmc,reg_tlnmc_type,nstrong,tlnmc_option,&
       period_max,period_width,init_strongvars,baldiag_full,baldiag_inc
  use gridmod, only: nlat,nlon,nsig,wrf_nmm_regional,nems_nmmb_regional,cmaq_regional,&
     nmmb_reference_grid,grid_ratio_nmmb,grid_ratio_wrfmass,&
     filled_grid,half_grid,wrf_mass_regional,nsig1o,nnnn1o,update_regsfc,&
     diagnostic_reg,gencode,nlon_regional,nlat_regional,nvege_type,&
     twodvar_regional,regional,init_grid,init_reg_glob_ll,init_grid_vars,netcdf,&
     nlayers,use_gfs_ozone,check_gfs_ozone_date,regional_ozone,jcap,jcap_b,vlevs,&
     use_gfs_nemsio,use_readin_anl_sfcmask,use_sp_eqspace,final_grid_vars,&
     jcap_gfs,nlat_gfs,nlon_gfs,jcap_cut
  use guess_grids, only: ifact10,sfcmod_gfs,sfcmod_mm5,use_compress,nsig_ext,gpstop
  use gsi_io, only: init_io,lendian_in
  use regional_io, only: convert_regional_guess,update_pint,init_regional_io,preserve_restart_date
  use constants, only: zero,one,init_constants,gps_constants,init_constants_derived,three
  use fgrid2agrid_mod, only: nord_f2a,init_fgrid2agrid,final_fgrid2agrid,set_fgrid2agrid
  use smooth_polcarf, only: norsp,init_smooth_polcas
  use read_l2bufr_mod, only: minnum,del_azimuth,del_elev,del_range,del_time,&
     range_max,elev_angle_max,initialize_superob_radar,l2superob_only
  use m_berror_stats,only : berror_stats ! filename if other than "berror_stats"
  use lag_fields,only : infile_lag,lag_nmax_bal,&
                        &lag_vorcore_stderr_a,lag_vorcore_stderr_b,lag_modini
  use lag_interp,only : lag_accur
  use lag_traj,only   : lag_stepduration
  use hybrid_ensemble_parameters,only : l_hyb_ens,uv_hyb_ens,aniso_a_en,generate_ens,&
                         n_ens,nlon_ens,nlat_ens,jcap_ens,jcap_ens_test,oz_univ_static,&
                         regional_ensemble_option,merge_two_grid_ensperts, &
                         full_ensemble,pseudo_hybens,betaflg,pwgtflg,coef_bw,&
                         beta1_inv,s_ens_h,s_ens_v,init_hybrid_ensemble_parameters,&
                         readin_localization,write_ens_sprd,eqspace_ensgrid,grid_ratio_ens,enspreproc,&
                         readin_beta,use_localization_grid,use_gfs_ens,q_hyb_ens,i_en_perts_io, &
                         l_ens_in_diff_time,ensemble_path
  use rapidrefresh_cldsurf_mod, only: init_rapidrefresh_cldsurf, &
                            dfi_radar_latent_heat_time_period,metar_impact_radius,&
                            metar_impact_radius_lowcloud,l_gsd_terrain_match_surftobs, &
                            l_sfcobserror_ramp_t, l_sfcobserror_ramp_q, &
                            l_pbl_pseudo_surfobst,l_pbl_pseudo_surfobsq,l_pbl_pseudo_surfobsuv, &
                            pblh_ration,pps_press_incr,l_gsd_limit_ocean_q, &
                            l_pw_hgt_adjust, l_limit_pw_innov, max_innov_pct, &
                            l_cleansnow_warmts,l_conserve_thetaV,r_cleansnow_warmts_threshold, &
                            i_conserve_thetav_iternum,l_gsd_soiltq_nudge,l_cld_bld, cld_bld_hgt, &
                            build_cloud_frac_p, clear_cloud_frac_p,       &
                            l_cloud_analysis,nesdis_npts_rad, & 
                            iclean_hydro_withRef,iclean_hydro_withRef_allcol, &
                            i_use_2mq4b,i_use_2mt4b,i_gsdcldanal_type,i_gsdsfc_uselist, &
                            i_lightpcp,i_sfct_gross,l_use_hydroretrieval_all
  use gsi_metguess_mod, only: gsi_metguess_init,gsi_metguess_final
  use gsi_chemguess_mod, only: gsi_chemguess_init,gsi_chemguess_final
  use tcv_mod, only: init_tcps_errvals,tcp_refps,tcp_width,tcp_ermin,tcp_ermax
  use chemmod, only : init_chem,berror_chem,oneobtest_chem,&
       maginnov_chem,magoberr_chem,&
       oneob_type_chem,oblat_chem,&
       oblon_chem,obpres_chem,diag_incr,elev_tolerance,tunable_error,&
       in_fname,out_fname,incr_fname, &
       laeroana_gocart, l_aoderr_table, aod_qa_limit, luse_deepblue
  use chemmod, only : wrf_pm2_5,aero_ratios
  use gfs_stratosphere, only: init_gfs_stratosphere,use_gfs_stratosphere,pblend0,pblend1
  use gfs_stratosphere, only: broadcast_gfs_stratosphere_vars
  use general_commvars_mod, only: init_general_commvars,destroy_general_commvars
  use gsi_nstcouplermod, only: gsi_nstcoupler_init_nml
  use gsi_nstcouplermod, only: nst_gsi,nstinfo,zsea1,zsea2,fac_dtl,fac_tsl

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
!  10-09-2009 Wu        replace nhr_offset with min_offset since it's 1.5 hr for regional
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
!  07-27-2011 todling   add use_prepb_satwnd to control usage of satwnd's in prepbufr files
!  08-15-2011 gu/todling add pseudo-q2 option
!  09-10-2011 parrish   add use_localization_grid to handle (global) non-gaussian ensemble grid
!  09-14-2011 parrish/todling   add use_sp_eqspace for handling lat/lon grids
!  09-14-2011 todling   add use_gfs_ens to control global ensemble; also use_localization_grid
!  11-14-2011  wu       add logical switch to use extended forward model for sonde data
!  01-16-2012 m. tong   add parameter pseudo_hybens to turn on pseudo ensemble hybrid
!  01-17-2012 wu        add switches: gefs_in_regional,full_ensemble,betaflg,pwgtflg
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
!  12-03-2013 wu        add parameter coef_bw for option:betaflg
!  12-03-2013 Hu        add parameter grid_ratio_wrfmass for analysis on larger
!                              grid than mass background grid
!  12-10-2013 zhu       add cwoption
!  02-05-2014 todling   add parameter cwcoveqqcov (cw_cov=q_cov)
!  02-24-2014 sienkiewicz added aircraft_t_bc_ext for GMAO external aircraft temperature bias correction
!  05-29-2014 Thomas    add lsingleradob logical for single radiance ob test
!                       (originally of mccarty)
!  06-19-2014 carley/zhu  add factl and R_option for twodvar_regional lcbas/ceiling analysis
!  08-05-2014 carley    add safeguard so that oneobtest disables hilbert_curve if user accidentally sets hilbert_curve=.true.
!  08-18-2014 tong      add jcap_gfs to allow spectral transform to a coarser resolution grid,
!                       when running with use_gfs_ozone = .true. or use_gfs_stratosphere = .true. for
!                       regional analysis
!  10-07-2014 carley    added buddy check options under obsqc
!  11-12-2014 pondeca   must read in from gridopts before calling obsmod_init_instr_table. swap order
!  01-30-2015 Hu        added option i_en_perts_io,l_ens_in_diff_time under hybrid_ensemble
!  01-15-2015 Hu        added options i_use_2mq4b,i_use_2mt4b, i_gsdcldanal_type
!                              i_gsdsfc_uselist,i_lightpcp,i_sfct_gross under
!                              rapidrefresh_cldsurf
!  03-01-2015 Li        add zsea1 & zsea2 to namelist for vertical mean temperature based on NSST T-Profile
!  05-02-2015 Parrish   add option rtma_bkerr_sub2slab to allow dual resolution for application of
!                       anisotropic recursive filter (RTMA application only for now).
!  05-13-2015 wu        remove check to turn off regional 4densvar
!  02-29-2015 S.Liu     added option l_use_hydroretrieval_all
!  03-02-2016 s.liu/carley - remove use_reflectivity and use i_gsdcldanal_type
!  03-10-2016 ejones    add control for gmi noise reduction
!  03-25-2016 ejones    add control for amsr2 noise reduction
!  08-12-2016 Mahajan   NST stuff belongs in NST module, Adding a NST namelist
!                       option
!  08-28-2016 li - tic591: add use_readin_anl_sfcmask for consistent sfcmask
!                          between analysis grids and others
!
!EOP
!-------------------------------------------------------------------------

! Declare variables.
  logical:: writediag
  integer(i_kind) i,ngroup


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
!     clip_supersaturation - flag to remove supersaturation during each outer loop default=.false.
!     deltim   - model timestep
!     dtphys   - physics timestep
!     biascor  - background error bias correction coefficient
!     bcoption - 0=ibc; 1=sbc
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
!     lferrscale   - apply H'R^{-1}H to a forecast error vector read on the fly
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
!     l_foto   - option for First-Order Time extrapolation to Observation
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
!     oberror_tune - logical flag to tune oberror table  (true=on)
!     perturb_fact -  magnitude factor for observation perturbation
!     crtm_coeffs_path - path of directory w/ CRTM coeffs files
!     print_diag_pcg - logical turn on of printing of GMAO diagnostics in pcgsoi.f90
!     preserve_restart_date - if true, then do not update regional restart file date.
!     tsensible - option to use sensible temperature as the analysis variable. works
!                 only for twodvar_regional=.true.
!     lgschmidt - option for re-biorthogonalization of the {gradx} and {grady} sets
!                 from pcgsoi when twodvar_regional=.true.
!     hilbert_curve - option for hilbert-curve based cross-validation. works only
!                     with twodvar_regional=.true.
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
!     use_edges   - option to exclude radiance data on scan edges
!     biaspredvar - set background error variance for radiance bias coeffs
!     (default 0.1K)
!     use_compress - option to turn on the use of compressibility factors in geopotential heights
!     nsig_ext - number of layers above the model top which are necessary to compute the bending angle for gpsro
!     gpstop - maximum height for gpsro data assimilation. Reject anything above this height. 
!     use_gfs_nemsio  - option to use nemsio to read global model NEMS/GFS first guess
!     use_readin_anl_sfcmask  - option to use readin surface mask
!     use_prepb_satwnd - allow using satwnd's from prepbufr (historical) file
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
!     ladtest -  if true, doing the adjoint test for the operator that maps
!                    control_vector to the model state_vector
!     ladtest_obs -  if true, doing the adjoint adjoint check for the
!                     observation operators that are currently used in the NCEP GSI variational
!                     analysis scheme
!     lrun_subdirs - logical to toggle use of subdirectires at runtime for pe specific files
!     emiss_bc    - option to turn on emissivity bias predictor
!     lsingleradob - logical for single radiance observation assimilation.
!                   Uses existing bufr file and rejects all radiances that don't fall within a tight threshold around
!                   oblat/oblon (SINGLEOB_TEST)
!
!     ssmis_method - choose method for SSMIS noise reduction 0=no smoothing 1=default
!     ssmis_precond - weighting factor for SSMIS preconditioning (if not using newpc4pred)
!     gmi_method - choose method for GMI noise reduction. 0=no smoothing, 4=default
!     amsr2_method - choose method for AMSR2 noise reduction. 0=no smoothing, 5=default
!     R_option   - Option to use variable correlation length for lcbas based on data
!                    density - follows Hayden and Purser (1995) (twodvar_regional only)
!     thin4d - if true, removes thinning of observations due to the location in
!              the time window
!
!     NOTE:  for now, if in regional mode, then iguess=-1 is forced internally.
!            add use of guess file later for regional mode.

  namelist/setup/gencode,factqmin,factqmax,clip_supersaturation, &
       factv,factl,factp,factg,factw10m,facthowv,factcldch,R_option,deltim,dtphys,&
       biascor,bcoption,diurnalbc,&
       niter,niter_no_qc,miter,qoption,cwoption,nhr_assimilation,&
       min_offset,pseudo_q2,&
       iout_iter,npredp,retrieval,&
       tzr_qc,tzr_bufrsave,&
       diag_rad,diag_pcp,diag_conv,diag_ozone,diag_aero,diag_co,iguess, &
       write_diag,reduce_diag, &
       oneobtest,sfcmodel,dtbduv_on,ifact10,l_foto,offtime_data,&
       npred_conv_max,&
       id_bias_ps,id_bias_t,id_bias_spd, &
       conv_bias_ps,conv_bias_t,conv_bias_spd, &
       id_bias_pm2_5,conv_bias_pm2_5,id_bias_pm10,conv_bias_pm10, &
       stndev_conv_ps,stndev_conv_t,stndev_conv_spd,use_pbl,use_compress,nsig_ext,gpstop,&
       perturb_obs,perturb_fact,oberror_tune,preserve_restart_date, &
       crtm_coeffs_path,berror_stats, &
       newpc4pred,adp_anglebc,angord,passive_bc,use_edges,emiss_bc,upd_pred, &
       ssmis_method, ssmis_precond, gmi_method, amsr2_method, &
       lobsdiagsave, &
       l4dvar,lbicg,lsqrtb,lcongrad,lbfgsmin,ltlint,nhr_obsbin,nhr_subwin,&
       nwrvecs,iorthomax,ladtest,ladtest_obs, lgrtest,lobskeep,lsensrecompute,jsiga,ltcost, &
       lobsensfc,lobsensjb,lobsensincr,lobsensadj,lobsensmin,iobsconv, &
       idmodel,iwrtinc,lwrite4danl,jiterstart,jiterend,lobserver,lanczosave,llancdone, &
       lferrscale,print_diag_pcg,tsensible,lgschmidt,lread_obs_save,lread_obs_skip, &
       use_gfs_ozone,check_gfs_ozone_date,regional_ozone,lwrite_predterms,&
       lwrite_peakwt, use_gfs_nemsio,liauon,use_prepb_satwnd,l4densvar,ens_nstarthr,&
       use_gfs_stratosphere,pblend0,pblend1,step_start,diag_precon,lrun_subdirs,&
       use_sp_eqspace,lnested_loops,lsingleradob,thin4d,use_readin_anl_sfcmask

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
!     wrf_mass_regional - logical for input from WRF MASS-CORE
!     cmaq_regional     - logical for input from CMAQ
!     nems_nmmb_regional- logical for input from NEMS NMMB
!     nmmb_reference_grid= 'H', then analysis grid covers H grid domain
!                                = 'V', then analysis grid covers V grid domain
!     grid_ratio_nmmb   - ratio of analysis grid to nmmb model grid in nmmb model grid units.
!     grid_ratio_wrfmass - ratio of analysis grid to wrf mass grid in wrf grid units.
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


  namelist/gridopts/jcap,jcap_b,nsig,nlat,nlon,nlat_regional,nlon_regional,&
       diagnostic_reg,update_regsfc,netcdf,regional,wrf_nmm_regional,nems_nmmb_regional,&
       wrf_mass_regional,twodvar_regional,filled_grid,half_grid,nvege_type,nlayers,cmaq_regional,&
       nmmb_reference_grid,grid_ratio_nmmb,grid_ratio_wrfmass,jcap_gfs,jcap_cut

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
	bkgv_flowdep,bkgv_rewgtfct,bkgv_write,fpsproj,adjustozvar,fut2ps,cwcoveqqcov

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
      ljc4tlevs

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
!     tdrgross_fact - factor applies to tdr gross error
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
!     njqc  -  When true, use Purser's non linear QC

  namelist/obsqc/ dfact,dfact1,erradar_inflate,tdrerr_inflate,tdrgross_fact,oberrflg,&
       vadfile,noiqc,c_varqc,blacklst,use_poq7,hilbert_curve,tcp_refps,tcp_width,&
       tcp_ermin,tcp_ermax,qc_noirjaco3,qc_noirjaco3_pole,qc_satwnds,njqc,vqc,&
       aircraft_t_bc_pof,aircraft_t_bc,aircraft_t_bc_ext,biaspredt,upd_aircraft,cleanup_tail,&
       buddycheck_t,buddydiag_save

! OBS_INPUT (controls input data):
!      dmesh(max(dthin))- thinning mesh for each group
!      time_window_max  - upper limit on time window for all input data
!      ext_sonde        - logical for extended forward model on sonde data
!      l_foreaft_thin -   separate TDR fore/aft scan for thinning

  namelist/obs_input/dmesh,time_window_max, &
       ext_sonde,l_foreaft_thin

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
       obchan

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
       elev_angle_max,minnum,range_max,l2superob_only

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
!     beta1_inv           - 1/beta1, the default weight given to static background error covariance if (.not. readin_beta)
!                              0 <= beta1_inv <= 1,  tuned for optimal performance
!                             =1, then ensemble information turned off
!                             =0, then static background turned off
!                            the weights are applied per vertical level such that : 
!                                        betas_inv(:) = beta1_inv     , vertically varying weights given to static B ; 
!                                        betae_inv(:) = 1 - beta1_inv , vertically varying weights given ensemble derived covariance.
!                            If (readin_beta) then betas_inv and betae_inv are read from a file and beta1_inv is not used.
!     s_ens_h             - homogeneous isotropic horizontal ensemble localization scale (km)
!     s_ens_v             - vertical localization scale (grid units for now)
!                              s_ens_h, s_ens_v, and beta1_inv are tunable parameters.
!     use_gfs_ens  - controls use of global ensemble: .t. use GFS (default); .f. uses user-defined ens
!     readin_localization - flag to read (.true.)external localization information file
!     readin_beta         - flag to read (.true.) the vertically varying beta parameters betas_inv and betae_inv
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
!     betaflg          - if true, use vertical weighting on beta1_inv and beta2_inv, for regional
!     coef_bw          - fraction of weight given to the vertical boundaries when betaflg is true
!     pwgtflg          - if true, use vertical integration function on ensemble contribution of Psfc
!     grid_ratio_ens   - for regional runs, ratio of ensemble grid resolution to analysis grid resolution
!                            default value = 1  (dual resolution off)
!     enspreproc - flag to read(.true.) pre-processed ensemble data already
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
!              
!                         
  namelist/hybrid_ensemble/l_hyb_ens,uv_hyb_ens,q_hyb_ens,aniso_a_en,generate_ens,n_ens,nlon_ens,nlat_ens,jcap_ens,&
                pseudo_hybens,merge_two_grid_ensperts,regional_ensemble_option,full_ensemble,betaflg,pwgtflg,&
                jcap_ens_test,beta1_inv,s_ens_h,s_ens_v,readin_localization,eqspace_ensgrid,readin_beta,&
                grid_ratio_ens, &
                oz_univ_static,write_ens_sprd,enspreproc,use_localization_grid,use_gfs_ens,coef_bw, &
                i_en_perts_io,l_ens_in_diff_time,ensemble_path

! rapidrefresh_cldsurf (options for cloud analysis and surface 
!                             enhancement for RR appilcation  ):
!      dfi_radar_latent_heat_time_period     -   DFI forward integration window in minutes
!      metar_impact_radius  - metar low cloud observation impact radius in grid number
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
!                         =5.  skip cloud analysis and NETCDF file update
!      i_gsdsfc_uselist  - options for how to use surface observation use or
!                          rejection list
!                         =0 . EMC method (default)
!                         =1 . GSD method
!      i_lightpcp        - options for how to deal with light precipitation
!                         =0 . don't add light precipitation (default)
!                         =1 . add light precipitation in warm section
!      i_sfct_gross      - if use extended threshold for surface T gross check
!                         =0 use threshold from convinfo (default)
!                         =1 for cold surface, threshold for gross check is
!                         enlarged to bring more large negative innovation into
!                         analysis.
!
  namelist/rapidrefresh_cldsurf/dfi_radar_latent_heat_time_period, &
                                metar_impact_radius,metar_impact_radius_lowcloud, &
                                l_gsd_terrain_match_surftobs, &
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
                                i_lightpcp,i_sfct_gross,l_use_hydroretrieval_all

! chem(options for gsi chem analysis) :
!     berror_chem       - .true. when background  for chemical species that require
!                          conversion to lower case and/or species names longer than 5 chars
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
!     laeroana_gocart   - when true, do chem analysis with wrfchem and modis
!     l_aoderr_table    - whethee to use aod error table or default error
!     aod_qa_limit      - minimum acceptable value of error flag for total column AOD
!     luse_deepblue     - whether to use MODIS AOD from the deepblue   algorithm

  namelist/chem/berror_chem,oneobtest_chem,maginnov_chem,magoberr_chem,&
       oneob_type_chem,oblat_chem,oblon_chem,obpres_chem,&
       diag_incr,elev_tolerance,tunable_error,&
       in_fname,out_fname,incr_fname,&
       laeroana_gocart, l_aoderr_table, aod_qa_limit, luse_deepblue,&
       aero_ratios,wrf_pm2_5

! NST (NSST control namelist) :
!     nst_gsi  - indicator to control the Tr Analysis mode: 0 = no nst info ingsi at all;
!                                                           1 = input nst info, but used for monitoring only
!                                                           2 = input nst info, and used in CRTM simulation, but no Tr analysis
!                                                           3 = input nst info, and used in CRTM simulation and Tr analysis is on
!     nstinfo  - number of nst variables
!     zsea1    - upper depth (in mm) for vertical mean of T based on NSST T-Profile
!     zsea2    - lower depth (in mm) for vertical mean of T based on NSST T-Profile
!     fac_dtl  - index to apply diurnal thermocline layer  or not: 0 = no; 1 = yes.
!     fac_tsl  - index to apply thermal skin layer or not: 0 = no; 1 = yes.
   namelist/nst/nst_gsi,nstinfo,zsea1,zsea2,fac_dtl,fac_tsl

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
  use mpeu_util,only: die
  use gsi_4dcouplermod, only: gsi_4dcoupler_parallel_init
  use gsi_4dcouplermod, only: gsi_4dcoupler_setservices
  implicit none
  character(len=*),parameter :: myname_='gsimod.gsimain_initialize'
  integer:: ier,ios
  real(r_kind):: varqc_max,c_varqc_new

  call gsi_4dcoupler_parallel_init

  call mpi_comm_size(mpi_comm_world,npe,ierror)
  call mpi_comm_rank(mpi_comm_world,mype,ierror)
  if (mype==0) call w3tagb('GSI_ANL',1999,0232,0055,'NP23')

! Initialize defaults of vars in modules
  call init_4dvar

  call init_regional_io
! Read in user specification of state and control variables
  call gsi_metguess_init
  call gsi_chemguess_init
  call init_anasv
  call init_anacv

  call init_constants_derived
  call init_oneobmod
  call init_qcvars
  call init_obsmod_dflts
  call init_pcp
  call init_rad
  call init_oz
  call init_aero
  call init_co
  call init_convinfo
  call init_jfunc
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
  open(11,file='gsiparm.anl')
  read(11,setup,iostat=ios)
  if(ios/=0) call die(myname_,'read(setup)',ios)  
  close(11)

  open(11,file='gsiparm.anl')
  read(11,gridopts,iostat=ios)
  if(ios/=0) call die(myname_,'read(gridopts)',ios)

! call to obsmod_init_instr_table must be after setup and gridopts are read in
  call obsmod_init_instr_table(nhr_assimilation,ndat,rcname='gsiparm.anl')

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
  if(ltlint) then
     if(vqc .or. njqc)then
       vqc = .false.
       njqc = .false.
       if(mype == 0) write(6,*) ' ltlint = true, so vqc and njqc must be false'
     end if
  end if
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

! Diagonal preconditioning is necessary for new bias correction
  if(newpc4pred .and. .not. diag_precon)then
    diag_precon=.true.
    step_start=8.e-4_r_kind
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

  call gsi_4dcoupler_setservices(rc=ier)
  if(ier/=0) call die(myname_,'gsi_4dcoupler_setServices(), rc =',ier)


! Check user input for consistency among parameters for given setups.

! Set regional parameters
  if(filled_grid.and.half_grid) filled_grid=.false.
  regional=wrf_nmm_regional.or.wrf_mass_regional.or.twodvar_regional.or.nems_nmmb_regional .or. cmaq_regional

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
     use_limit = 0
  end if
  if(reduce_diag) use_limit = 0

! Force use of perturb_obs for oberror_tune
  if (oberror_tune ) then
     perturb_obs=.true.
     if (mype==0) write(6,*)'GSIMOD:  ***WARNING*** reset perturb_obs=',perturb_obs
  endif

! Force turn of cloud analysis and hydrometeor IO
  if (i_gsdcldanal_type==0) then
     l_cloud_analysis = .false.
     if (mype==0) write(6,*)'GSIMOD:  ***WARNING*** set l_cloud_analysis=false'
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

! Turn off uv option if hybrid/ensemble options is false for purposes 
! of TLNMC 
  if (.not.l_hyb_ens) uv_hyb_ens=.false.

! Turn on derivatives if using dynamic constraint
! For now if wrf mass or 2dvar no dynamic constraint
  if (l_tlnmc) tendsflag=.true.
  if (l_foto) then
     tendsflag=.true.
     if(mype == 0)write(6,*) 'Warning foto option will be removed in the near future'
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
 401    format(1x,a20,1x,a10,1x,a10,1x,a20,1x,f10.2,1x,I3,1x,I3,1x,f10.2)
     end do
     write(6,superob_radar)
     write(6,lag_data)
     write(6,hybrid_ensemble)
     write(6,rapidrefresh_cldsurf)
     write(6,chem)
     if (oneobtest) write(6,singleob_test)
     write(6,nst)
  endif

! Set up directories (or pe specific filenames)
  call init_directories(mype)

! Initialize space for qc
  call create_qcvars

! Initialize constants
  call init_constants(regional)
  call gps_constants(use_compress)

! If this is a wrf regional run, then run interface with wrf
  update_pint=.false.
  if (regional) call convert_regional_guess(mype,ctph0,stph0,tlm0)
  if (regional.and.use_gfs_stratosphere) call broadcast_gfs_stratosphere_vars


! Initialize variables, create/initialize arrays
  call init_reg_glob_ll(mype,lendian_in)
  call init_grid_vars(jcap,npe,cvars3d,cvars2d,nrf_var,mype)
  call init_general_commvars
  call create_obsmod_vars
  if (passive_bc) call create_passive_obsmod_vars

  
! Initialize values in radinfo
  call init_rad_vars

! Initialize values in aeroinfo
  call init_aero_vars

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
  call gsisub(mype, init_pass_,last_pass_)

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

  implicit none
! Deallocate arrays
! RTodling debug: PROG HANGS; needs ATTENTION
  call mpi_comm_rank(mpi_comm_world,mype,ierror)
  if (anisotropic) then
     call final_fgrid2agrid(pf2aP3)
     call final_fgrid2agrid(pf2aP2)
     call final_fgrid2agrid(pf2aP1)
  endif
  call final_aero_vars
  call final_rad_vars
  if(passive_bc) call destroyobs_passive
  call destroy_obsmod_vars
  call destroy_general_commvars
  call final_grid_vars
!_TBDone  call final_reg_glob_ll ! gridmod
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

