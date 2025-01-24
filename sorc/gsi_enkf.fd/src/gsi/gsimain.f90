!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOP

! ! IPROGRAM: gsimain -- runs NCEP gsi

! ! INTERFACE:

   program gsi

   use gsi_fixture, only: my_fixture_config => fixture_config

   use gsimod, only: gsimain_initialize,gsimain_run,gsimain_finalize
   use gsi_4dvar, only: l4dvar
   use gsi_4dcouplermod, only: gsi_4dcoupler_init_traj
   use gsi_4dcouplermod, only: gsi_4dcoupler_final_traj
   use timermod, only: timer_pri
   use kinds, only: i_kind
   use mpeu_util, only: die
   implicit none

!$$$  main program documentation block
!                .      .    .                                       .
! main program: GSI_ANL
!   PRGMMR: DERBER           ORG: NP23                DATE: 1999-08-20
!
! abstract: The gridpoint statistical interpolation (GSI) analysis code
!   performs an atmospheric analysis over a specified domain (global
!   or regional) in which guess fields from a forecast model are combined
!   with available observations using a 3D-VAR approach.
!
! program history log:
!   1991-xx-xx  parrish/derber   initial SSI code
!   1991-12-10  parrish/derber   fixed coding error in near sfc anal
!   1992-09-14  derber           improved version of global ssi analysis
!   1998-05-15  weiyu yang       mpp version of global ssi
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-09-02  wu,treadon,kleist,parrish,yang adapt for global/regional 
!                                unified version (see note below)
!   2003-12-24  derber, j. add documentation
!   2004-07-19  parrish - add mass core to regional gsi 
!   2004-07-23  derber - modify to include conventional sst
!   2004-08-04  treadon - add only on use declarations; add intent in/out;
!                         remove eta_regional from namelist (not used)
!   2004-10-12  parrish - namelist modifications for nonlinear qc
!   2004-11-03  treadon - add horizontal scale weighting factors to namelist
!   2004-11-10  treadon - add comments for error codes; initialize variables
!                         wrf_anl_filename and wrf_ges_filename
!   2004-11-29  parrish - remove code to handle regional binary update
!   2004-12-08  xu li   - add logical variable retrieval for SST physical retrieval
!                         algorithm
!   2004-12-15  treadon - update documentation; simplify regional ges & anl i/o
!   2004-12-22  treadon - rename diagnostic output logical flags; add logical
!                         array write_diag to control computation/output of 
!                         diagnostic files on each outer iter
!   2004-12-29  treadon - replace code to handle regional i/o conversions/updates
!                         with a single call to an interface module
!   2005-01-22  parrish - add compact_diffs
!   2005-01-24  kleist  - reorganize namelists
!   2005-02-23  wu      - add namelist variable qoption
!   2005-03-01  parrish - add namelist group for anisotropic background error
!   2005-03-07  dee     - add logical gmao_intfc for gmao model interface!
!   2005-04-08  treadon - add call set_nlnqc_flags
!   2005-05-24  pondeca - add 2dvar only surface analysis option
!   2005-05-27  yanqiu  - added obs_sen to control GSI's TLM
!   2005-05-27  kleist/derber - add option to read in new ob error table
!   2005-05-27  kleist/parrish - add option to use new patch interpolation
!                                if (norsp==0) will default to polar cascade
!   2005-06-01  treadon - add nlayers to namelist gridopts
!   2005-06-06  wu      - add namelist variable fstat, logical to seperate f
!                         from balance projection
!   2005-07-06  parrish - add/initialize logical variable update_pint
!   2005-07-10  kleist  - add options for Jc term
!   2005-08-01  parrish,lee - add changes to include new surface temperature 
!                             forward model
!   2005-08-03  derber - remove gross and variational qc conventional 
!                        variables from namelists
!   2005-03-24  derber - remove call set_nlnqc_flags
!   2005-09-08  derber - modify to use input group time window clean up unused variables
!   2005-09-28  parrish - modify namelist parameters for radar wind superobs
!   2005-09-29  kleist - expanded namelist for Jc option
!   2005-10-17  parrish - add ctph0,stph0,tlm0 to call convert_regional_guess
!   2005-10-18  treadon - remove dload from OBS_INPUT namelist
!   2005-11-09  wu - turn off limq when using qoption=2
!   2005-11-21  kleist - use tendency module, force flags to true if necessary
!   2005-11-22  wu - add perturb_conv and pfact to SETUP namelist
!   2005-12-01  cucurull - update information to include GPS bending angle code
!   2005-12-20  parrish - add parameter sfcmodel for option to select boundary layer
!                         forward model for surface temperature observations
!   2006-01-10  treadon - move deallocate array calls from gsisub to gsimain
!   2006-01-27  guo     - add namelist to handle gmao grid
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-03-01  eliu    - add logical variable gmao_rtm to the namelist to handle gmao rtm
!   2006-03-21  treadon - modify optional perturbation to observation
!   2006-04-06  middlecoff - added three exit states
!   2006-04-19  treadon - add logical switch dtbduv_on to namelist setup
!   2006-04-20  wu - check OBS_INPUT time_window against obsmod default
!   2006-04-20  kistler - added init_conv for conv ob bias correction
!   2006-04-21  parrish - modifications for new treatment of level 2 radar winds
!   2006-04-21  kleist  - Jc namelist generalized
!   2006-05-22  su - add noiqc flag
!   2006-07-28  derber  - add dfact1 namelist parameter, remove jppf
!   2006-08-15  parrish - add namelist STRONGOPTS for new strong constraint option
!   2006-08-30  zhang,b - add diurnal cycle bias correction capability
!   2006-09-29  treadon - add ifact10 logic, allow limq with qoption=2
!   2006-10-12  treadon - set tendsflag and switch_on_derivatives for pcp data
!   2006-10-25  sienkiewicz - add blacklist flag to namelist
!   2006-11-30  todling - add fpsproj parameter to bkgerr namelist
!   2007-03-12       su - add perturb_obs,perturb_fact,c_varqc
!   2007-04-10  todling - split following C.Cruz and da Silva's modification to ESMF
!   2007-04-13  tremolet - add error code 100
!   2007-06-08  kleist/treadon - add init_directories
!   2007-06-20  rancic/derber - add pbl option
!   2007-07-03  kleist - add variance reweighting option
!   2007-09-30  todling  - add timer
!   2007-10-24  parrish - add l2superob_only option
!   2008-03-24      wu - add oberror tuning option 
!   2008-05-20  Guo, J. - removed obsolete gmao_rtm control
!			- removed diurnal bias correction implmented by Zhang, B.
!   2010-03-18  treadon - add comment for return code 330
!   2010-04-26  kistler - add comment for return code 331
!   2010-05-27  todling - error codes 127-130 no longer apply (slots emptied)
!   2011-03-14  guo     - Moved gsi_4dcoupler calls into here, to split
!			  gsi_4dcoupler_init_traj() from gsimain_initialize(),
!			  and gsi_4dcoupler_final_traj() from gsimain_finalize(),
!   2011-08-01  lueken  - replaced F90 with f90 (no machine logic)
!   2013-07-02  parrish - remove error message 328 - tlnmc_type > 2 not allowed
!   2018-02-15  wu      - add fv3_regional
!   2017-11-29  apodaca - add information, source codes, and exit states
!                         related to the GOES/GLM lightnig assimilation
!   2019-07-09  todling - add initialization of abstract layer defining use of GFS ensemble
!   2019-08-04  guo     - moved ensemble object configuration into module gsi_fixture.
!   2022-03-15  K Apodaca - add CYGNSS and Spire Ocean wind speed observations
!   2023-03-15  K Apodaca - add GNSS-R Doppler Delay Map observations
!
! usage:
!   input files:
! **************************
!    input observation data file names are specified in namelist obs_input
! **************************
!     berror_stats  - background error statistics
!     emissivity_coefficients     - IR surface emissivity coefficient file
!     ozinfo        - ozone observation information file
!     pcpinfo       - precipitation rate observation info file
!     satbias_angle - satellite angle dependent file
!     satbias_in    - satellite bias correction coefficient file
!     satinfo       - satellite channel info file
!     lightinfo     - lightning flash rate observation info file
!     sfcf**        - background surface files (typically sfcf03,sfcf06 and sfcf09)
!     sigf**        - background forecast files (typically sigf03,sigf06 and sigf09)
!     spectral_coefficients       - radiative transfer spectral coefficient file
!     transmittance_coefficients  - radiative transfer transmittance coefficient file
!
!     NOTE:  input observation data file names are specified in namelist obs_input
!
!   output files:  (including scratch files)
!     fort.2*      - diagnostic output from setup routines (fort.220 contains
!                    output from the inner loop minimization --> pcgsoi.f90)
!     fort.6       - runtime output
!     pcpbias_out  - output precipitation bias correction file
!     satbias_out  - output satellite bias correction file
!     sfcanl.gsi   - output surface file
!     siganl       - output atmospheric analysis file
!
!     conv."processor"       - conventional observation diagnostic file written
!                              by mpi task "processor"
!     pcp_"type"."processor" - precipitation rate diagnostic file for
!                              satellite/sensor "type" written by mpi task 
!                              "processor"
!     "sattype"*".processor" - brightness temperature diagnostic file for
!                              satellite/sensor "sattype" written by mpi task 
!                              "processor"
!     sbuv2."id"."processor" - sbuv2 ozone diagnostic file for satellite
!                              "id" written by mpi task "processor"
!     
!
!   scratch files
!     obs_setup1***,obs_inner1***,obs_input***** 
!
!   subprograms called:
!     source code files
!         ajmpcp, balance, berror, bkerror, bkgcov, bkgvar, 
!         constants, deter_subdomain, dprodx, dtast, dvast, emiss, emiss_ssmi,
!         fill_mass_grid2, fill_nmm_grid2, fpvsx_ad, gengrid_vars, genqsat,
!         glbsoi, grdcrd, grdsphdp, grid2sub, gridmod, gscond_ad,
!         gsimain, gsisub, guess_grids, half_nmm_grid2, hopers, iceem_amsu,
!         inguesfc, inisph, intall, intall_qc, intdw, intlight, intlimq,
!         intoz, intpcp, intps, intpw, intq, intrad, intref, intbend, intrp2a, intrp3,
!         intrp3oz, intrppx, intrw, intspd, intsst, intt, intw, jfunc,
!         kinds, landem, lightbias, lightinfo, locatelat_reg, mpimod, nlmsas_ad, 
!         obs_para, obsmod, omegas_ad, oneobmod, ozinfo, pcgsoi, pcpinfo, polcarf, 
!         precpd_ad, prewgt, prewgt_reg, psichi2uv_reg, psichi2uvt_reg,
!         qcmod, rad_tran_k, radinfo, rdgesig, rdgstat_reg, rdsfull,
!         read_airs, read_avhrr_navy, read_bufrtovs, read_files, read_goesimg,
!         read_goesglm, read_goesndr, read_gps_ref, read_guess, read_ieeetovs, 
!         read_lidar, read_obs, read_ozone, read_pcp, read_prepbufr, read_radar, 
!         read_superwinds, read_wrf_mass_files, read_wrf_mass_guess, 
!         read_wrf_nmm_files, read_wrf_nmm_guess, read_gnssrspd, rfdpar, rsearch, satthin,
!         setupdw, setupoz, setuppcp, setupps, setuppw, setupq, setuprad,
!         setupref, setupbend, setuplight, setuprhsall, setuprw, setupspd, setupsst,
!         setupt, setupw, simpin1, simpin1_init, smooth121, smoothrf,
!         smoothwwrf, smoothzrf, snwem_amsu, specmod, 
!         sst_retrieval, statsconv, statslight, statsoz, statspcp, statsrad, stop2, stpbend,
!         stpcalc, stpcalc_qc, stpdw, stplight, stplimq, stpoz, stppcp, stpps, stppw,
!         stpq, stprad, stpref, stprw, stpspd, stpsst, stpt, stpw,
!         stvp2uv, stvp2uv_reg, sub2grid, sumslightbias, tbalance, tintrp2a, tintrp3,
!         tpause, tpause_t, transform, tstvp2uv, tstvp2uv_reg, unfill_mass_grid2,
!         unfill_nmm_grid2, unhalf_nmm_grid2, update_ggrid, wrf_binary_interface,
!         wrf_netcdf_interface, write_all, wrsfca, wrsiga, wrwrfmassa, wrwrfnmma,
!
!     modules:
!       From GSI:
!         berror, constants, gridmod, guess_grids, jfunc, kinds, mpimod, obsmod, 
!         oneobmod, ozinfo, pcpinfo, qcmod, radinfo, satthin, specmod 
!
!       From Community Radiative Transfer Model (CRTM)):
!         error_handler, initialize, k_matrix_model, spectral_coefficients
!
!       From InfraRed Sea-Surface Emissivity (IRSSE) model:
!         irsse_model
!
!
!      
!     libraries (for NCEP ibm):
!       w3_d      - NCEP W3 library
!       bufr_d_64 - 64 bit NCEP BUFR library
!       sp_d      - NCEP spectral-grid transform library
!       bacio_4   - byte addressable I/O library
!       sigio     - NCEP GFS sigma file I/O library
!       sfcio     - NCEP GFS surface file I/O library
!       CRTM      - Community Radiative Transfer Model
!       ESSL      - fast scientific calculation subroutines
!       MASS      - fast intrinsic function replacements
!       NETCDF    - the netcdf library
!       WRF       - the WRF library
!
!   exit states:
!     cond =   0 - successful run
!          =  24 - problem in update_start_date   
!          =  31 - extrapolation error (interp_a2e)
!          =  32 - failure in sort routine (indexx, in satthin)
!          =  33 - error in coarse --> fine grid interolation operator (get_3ops)
!          =  35 - model top pressure above RTM top pressure (add_layers_rtm)
!          =  36 - total number of model layers > RTM layers 
!          =  41 - illegal min,max horizontal scale (prewgt)
!          =  44 - illegal surface emissivity type(emiss)
!          =  45 - IR surface emissivity failure (emiss)
!          =  48 - allocation or i/o error (convinfo)
!          =  49 - error in call rdmemm (read_prepbufr)
!          =  50 - ndata > maxobs  (read_prepbufr)
!          =  51 - invalid pflag (convthin:make3grids)
!          =  54 - data handling mix up(setuprhsall)
!          =  55 - NOBS > NSDATA (setuprhsall-tran)
!          =  56 - iobs > maxobs (read_amsr2)
!          =  59 - problems reading sst analysis (rdgrbsst)
!          =  60 - inconsistent dimensions (rdgrbsst)
!          =  61 - odd number of longitudes (inisph)
!          =  62 - latitude not between +/- pi (inisph)
!          =  63 - error in LUD matrix dcomposition (inisph)
!          =  64 - singular matrix (inisph)
!          =  65 - vanishing row in L-D-U decomposition (ldum)
!          =  66 - singular matrix in L-D-U decomposition (ldum)
!          =  67 - matrix too large in L-D-U decomposition (ldum)
!          =  68 - raflib error (raflib, raflib_8)
!          =  69 - imaginary root to large (rfdpar1)
!          =  70 - error setting up assimilation time window
!          =  71 - channel number mix up (setuprad)
!          =  73 - incompatable horizontal or vertical resolution for statistics (prewgt)
!          =  74 - error reading regional or global guess file
!          =  75 - error writing regional or global analysis file
!          =  76 - error reading guess solution(glbsoi)
!          =  77 - error reading pcpinfo file(pcpinfo)
!          =  78 - incorrect number of records in pcpinfo file(pcpinfo)
!          =  79 - problem reading satellite information file (radinfo)
!          =  80 - problem reading global surface guess file (inguesfc,rdsfull,wrsfca)
!          =  81 - surface guess field tag not yet supported (inguesfc,rdsfull
!          =  82 - problem writing global surface analysis file (wrsfca)
!          =  83 - problem in gps statistics generating code (genstats*)
!          =  84 - buffer size too small for radar data (read_radar)
!          =  85 - guess resolution incompatable with namelist (gsisub)
!          =  86 - too many profile levels (read_gps_ref)
!          =  87 - too many input observation files(assumed max is 55)(gsisub)
!          =  88 - failure in radiative transfer code (rad_tran_k)
!          =  89 - problem reading namelist input (gsimain.F90)
!          =  91 - incompatable observation and analysis dates (read_lidar)
!          =  92 - incompatable observation and analysis dates (read_radar)
!          =  93 - incompatable observation and analysis dates (read_pcp)
!          =  94 - incompatable observation and analysis dates (read_prepbufr)
!          =  95 - incompatable observation and analysis dates (read_ozone)
!          =  96 - incompatable observation and analysis dates (read_gps_ref)
!          =  97 - error in radar wind superob specifications (read_superwinds)
!          =  99 - problem with numerical precision of ges_* and/or bias_* arrays
!          = 101 - prebal: inconsistent msig,nsig 
!          = 102 - allocate_preds: vector already allocated
!          = 103 - allocate_preds: error length
!          = 104 - control2model: assumes sqrt(B) but not specified
!          = 105 - control2model: error 3dvar
!          = 106 - control2state: not used for sqrt(B), but called
!          = 107 - control2state: error 3dvar
!          = 108 - allocate_cv: vector already allocated
!          = 109 - allocate_mods: error length
!          = 110 - assign_cv2cv: error length
!          = 111 - assign_array2cv: array wrong length
!          = 112 - assign_cv2array: array wrong length
!          = 113 - dot_prod_cv: error length
!          = 114 - qdot_prod_cv: error length
!          = 115 - axpy: error length
!          = 116 - read_cv: wrong length
!          = 117 - maxval_cv: MPI error
!          = 118 - qdot_product: inconsistent dims.
!          = 119 - set_cvsection: kbgn out of range
!          = 120 - set_cvsection: kend out of range
!          = 121 - all_cvsection: kbgn out of range
!          = 122 - all_cvsection: kend out of range
!          = 123 - enorm_state: negative dsig
!          = 124 - enorm_state: error in ilat
!          = 125 - evaljo: obscounts not allocated
!          = 126 - check_bks: troubled vertical coord system
!          = 127 -
!          = 128 - 
!          = 129 - 
!          = 130 - 
!          = 131 - grtest: pdx too small
!          = 132 - gsi_4dvar: Error in observation binning
!          = 133 - gsi_4dvar: Error in sub-windows definition
!          = 134 - setup_4dvar: unable to fullfil request for increment output
!          = 135 - setup_4dvar: iwrtinc or lwrite4danl inconsistent
!          = 136 - time_4dvar: minutes should be 0
!          = 137 - gsimod: adjoint computation requires contrad
!          = 138 - setup_congrad: kamxit>maxiter
!          = 139 - save_precond: error number of vectors
!          = 140 - setup_precond: no vectors for preconditioner
!          = 141 - read_lanczos: kamxit>maxiter
!          = 142 - read_lanczos: kiter>maxiter
!          = 143 - m_stats: MPI_allreduce(dot-sum)
!          = 144 - m_stats: MPI_allreduce(min-max)
!          = 145 - m_stats: MPI_allreduce(dim)
!          = 146 - control2model_ad: assumes lsqrtb
!          = 147 - model_tl: error nstep
!          = 148 - model_tl: error nfrctl
!          = 149 - model_tl: error nfrobs
!          = 150 - model_tl: error ndt
!          = 151 - model_tl: error xini
!          = 152 - model_tl: error xobs
!          = 153 - mpl_allgatherq: troubled jdim/npe
!          = 154 - mpl_bcast: MPI error
!          = 155 - init_fc_sens: unknown method
!          = 156 - save_fc_sens: obscounts not allocated
!          = 157 - observer: observer should only be called in 4dvar
!          = 158 - lbfgs: maxvecs is not positive.
!          = 159 - lbfgs: GTOL is smaller than 1.0e-4
!          = 160 - lbfgs: line search failed
!          = 161 - mcsrch: error input
!          = 162 - mcsrch: the search direction is not a descent direction
!          = 163 - mcstep: error in input values
!          = 164 - m1qn3: inconsistent call
!          = 165 - m1qn3: initial gradient is too small
!          = 166 - m1qn3: d is not a descent direction
!          = 167 - m1qn3: the scalar product (y,s) is not positive
!          = 168 - mlis0: error input parameters
!          = 169 - read_files: 0 atm fields
!          = 170 - read_files: 0 sfc fields
!          = 171 - read_obsdiags: error open
!          = 172 - read_obsdiags: error ii
!          = 173 - read_obsdiags: error jj
!          = 174 - read_obsdiags: error kiter
!          = 175 - read_obsdiags: error kiter
!          = 176 - read_obsdiags: error kiter
!          = 177 - read_obsdiags: fail to allocate obsdiags
!          = 178 - read_obsdiags: fail to allocate next obsdiags
!          = 179 - read_obsdiags: error ii
!          = 180 - read_obsdiags: error jj
!          = 181 - read_pshead: unmatched ob type
!          = 182 - read_pshead: error reading record
!          = 183 - read_pshead: error counting ob
!          = 184 - read_thead: unmatched ob type
!          = 185 - read_thead: error reading record
!          = 186 - read_thead: error counting ob
!          = 187 - read_whead: unmatched ob type
!          = 188 - read_whead: error reading record
!          = 189 - read_whead: error counting ob
!          = 190 - read_qhead: unmatched ob type
!          = 191 - read_qhead: error reading record
!          = 192 - read_qhead: error counting ob
!          = 193 - read_spdhead: unmatched ob type
!          = 194 - read_spdhead: error reading record
!          = 195 - read_spdhead: error counting ob
!          = 196 - read_swrhead: unmatched ob type
!          = 197 - read_swrhead: error reading record
!          = 198 - read_swrhead: error counting ob
!          = 199 - read_rwhead: unmatched ob type
!          = 200 - read_rwhead: error reading record
!          = 201 - read_rwhead: error counting ob
!          = 202 - read_dwhead: unmatched ob type
!          = 203 - read_dwhead: error reading record
!          = 204 - read_dwhead: error counting ob
!          = 205 - read_ssthead: unmatched ob type
!          = 206 - read_ssthead: error reading record
!          = 207 - read_ssthead: error counting ob
!          = 208 - read_pwhead: unmatched ob type
!          = 209 - read_pwhead: error reading record
!          = 210 - read_pwhead: unmatched nsig
!          = 211 - read_pwhead: error counting ob
!          = 212 - read_ozhead: unmatched ob type
!          = 213 - read_ozhead: error reading record
!          = 213 - read_ozhead: error large counter
!          = 214 - read_ozhead: error counting ob
!          = 215 - read_o3lhead: unmatched ob type
!          = 216 - read_o3lhead: error reading record
!          = 217 - read_o3lhead: error counting ob
!          = 218 - read_pcphead: unmatched ob type
!          = 219 - read_pcphead: unmatched number of predictors
!          = 220 - read_pcphead: unmatched number of layers
!          = 221 - read_pcphead: error reading record
!          = 222 - read_pcphead: error counting ob
!          = 223 - read_gpshead: unmatched ob type
!          = 224 - read_gpshead: unmatched number of layers
!          = 225 - read_gpshead: error reading record
!          = 226 - read_gpshead: error counting ob
!          = 227 - read_radhead: cannot handle retrieval
!          = 228 - read_radhead: unmatched ob type
!          = 229 - read_radhead: unmatched number of predictors
!          = 230 - read_radhead: unmatched number of layers
!          = 231 - read_radhead: error reading record nchan
!          = 232 - read_radhead: alloc(radhead)
!          = 233 - read_radhead: alloc(radtail%llpoint)
!          = 234 - read_radhead: fail to alloc various
!          = 235 - read_radhead: error reading record time
!          = 236 - read_radhead: error reading record res
!          = 237 - read_radhead: error reading record err2
!          = 238 - read_radhead: error reading record raterr2
!          = 239 - read_radhead: error reading record pred1
!          = 240 - read_radhead: error reading record pred2
!          = 241 - read_radhead: error reading record icx
!          = 242 - read_radhead: error reading record dtb_dvar
!          = 243 - read_radhead: fail to alloc radtail%various
!          = 244 - read_radhead: fail to dealloc various
!          = 245 - read_radhead: error troubled obs counter 1
!          = 246 - read_radhead: unmatched iii/nchan
!          = 247 - read_radhead: error troubled obs counter 2
!          = 248 - read_radhead: error radtail final obs counter
!          = 249 - read_radhead: error obsdiag final obs counter
!          = 250 - setupbend: failure to allocate obsdiags
!          = 251 - setupbend: failure to allocate obsdiags
!          = 252 - setupbend: index error
!          = 253 - setupdw: failure to allocate obsdiags
!          = 254 - setupdw: failure to allocate obsdiags
!          = 255 - setupdw: index error
!          = 256 - setupozlev: failure to allocate obsdiags
!          = 257 - setupozlev: failure to allocate obsdiags
!          = 258 - setupozlev: index error
!          = 259 - setupozlay: nobskeep
!          = 260 - setupozlay: failure to allocate obsdiags
!          = 261 - setupozlay: failure to allocate obsdiags
!          = 262 - setupozlay: index error
!          = 263 - setuppcp: failure to allocate obsdiags
!          = 264 - setuppcp: failure to allocate obsdiags
!          = 265 - setuppcp: index error
!          = 266 - setupps: failure to allocate obsdiags
!          = 267 - setupps: failure to allocate obsdiags
!          = 268 - setupps: index error
!          = 269 - setuppw: failure to allocate obsdiags
!          = 270 - setuppw: failure to allocate obsdiags
!          = 271 - setuppw: index error
!          = 272 - setupq: failure to allocate obsdiags
!          = 273 - setupq: failure to allocate obsdiags
!          = 274 - setupq: index error
!          = 275 - setuprad: nobskeep
!          = 276 - setuprad: failure to allocate obsdiags
!          = 277 - setuprad: failure to allocate obsdiags
!          = 278 - setuprad: index error
!          = 279 - setuprad: error iii icc
!          = 280 - setuprad: error obsptr
!          = 281 - setuprad: error writing diagnostics
!          = 282 - setupref: failure to allocate obsdiags
!          = 283 - setupref: failure to allocate obsdiags
!          = 284 - setupref: index error
!          = 285 - setuprhsall: obscount allocated
!          = 286 - setuprw: failure to allocate obsdiags
!          = 287 - setuprw: failure to allocate obsdiags
!          = 288 - setuprw: index error
!          = 289 - setupspd: failure to allocate obsdiags
!          = 290 - setupspd: failure to allocate obsdiags
!          = 291 - setupspd: index error
!          = 295 - setupsst: failure to allocate obsdiags
!          = 296 - setupsst: failure to allocate obsdiags
!          = 297 - setupsst: index error
!          = 298 - setupt: failure to allocate obsdiags
!          = 299 - setupt: failure to allocate obsdiags
!          = 300 - setupt: index error
!          = 301 - setuptcp: failure to allocate obsdiags
!          = 302 - setuptcp: failure to allocate obsdiags
!          = 303 - setuptcp: index error
!          = 304 - setupw: failure to allocate obsdiags
!          = 305 - setupw: failure to allocate obsdiags
!          = 306 - setupw: index error
!          = 307 - sqrtmin: lsqrtb false
!          = 308 - sqrtmin: congrad requires ltlint
!          = 309 - sqrtmin: congrad requires ltlint
!          = 310 - sqrtmin: error estimated gradient
!          = 311 - control2state_ad: not for sqrt(B)
!          = 312 - allocate_state: state already allocated
!          = 313 - allocate_state:  error length
!          = 315 - test_obsens: only for validation
!          = 316 - write_obsdiags: error open
!          = 317 - bkerror: not for use with lsqrtb
!          = 318 - init_jcdfi: Sum of weights is not 1
!          = 319 - steqr: r_kind is neither default real nor double precision
!          = 320 - steqr: SSTEQR/DSTEQR returned non-zero info
!          = 321 - ptsv: r_kind is neither default real nor double precision
!          = 322 - ptsv: SPTSV/DPTSV returned non-zero info
!          = 323 - save_precond: r_kind is neither default real nor double precision
!          = 324 - save_precond: error computing Cholesky decomposition
!          = 325 - setup_precond: r_kind is neither default real nor double precision
!          = 326 - setup_precond:  SSYEV/DSYEV returned non-zero return code
!          = 327 - PRECOND: invalid value for kmat
!          = 328 -
!          = 329 - problem with logicals or collective obs selection info file
!          = 330 - grid --> spectral transform not safe for sptranf_s,v_b
!          = 331 - trouble writing analysis errors
!          = 332 - co2 file i/o error
!          = 333 - mismatch between variable info file and background error fixed file
!          = 334 - newpc4pred: not for use with lsqrtb
!          = 335 - error reading radiance diagnostic file
!          = 336 - invalid namlist setting for nhsrf
!          = 337 - inconsitent tlnmc namelist settings
!          = 338 - error reading MLS vertical levels from MLS bufr 
!          = 339 - error:more than one MLS  data type not allowed
!          = 340 - error reading aircraft temperature bias file
!          = 341 - aircraft tail number exceeds maximum
!          = 342 - setuplight: failure to allocate obsdiags
!          = 343 - setuplight: failure to allocate obsdiags
!          = 344 - setuplight: index error
!          = 899 - foto no longer available
!
!
! remarks: resolution, unit numbers and several constants are
!          in the input data cards
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

!    NOTE:  PARAMETERS RELATED TO GLOBAL/REGIONAL ANALYSIS:

!     This program has been adapted for use for global or regional analysis.
!     Originally, only one regional model was allowed for, the WRF version of
!     the NCEP non-hydrostatic mesoscale model (WRF NMM).  This model uses a
!     rotated lat-lon coordinate so it was relatively easy to adapt the global
!     code to this rotated grid.  However, to run with other regional models
!     with different map definitions, it would be necessary to make special
!     rules for every model grid.  An alternative has been introduced here, 
!     which requires only input of the earth latitudes and longitudes of the
!     model grid points.  An inverse interpolation scheme is then used to 
!     convert from earth coordinates to model coordinates.  This is a universal
!     technique which works for any regional model input grid, with the exception 
!     that the regional grid cannot have a polar singularity or periodicity.
!     The interpolation introduces small errors, but these errors are proportional
!     to the model resolution.  For 10km models, the maximum coordinate transformation
!     error is < .5km.
!
!     The analysis does not currently work with staggered grids, so some
!     interpolation in the horizontal is required for regional models with
!     grid staggering.  The WRF NMM uses an E-grid, and there are 2 interpolation
!     options, one which fills the holes in the E-grid for more accurate, but
!     much more expensive option, and the other which takes every other row of
!     the E grid, which has no interpolation for mass variables, but winds must
!     be interpolated.  To minimize the impact of interpolation errors, only
!     the analysis increment on the analysis grid is interpolated back to the
!     model grid and added onto the guess.  This is a well-known technique for
!     reducing interpolation error in data assimilation when multiple grids are
!     used.
!
!     There are currently two regional models accepted by the analysis:

!             wrf_nmm_regional = .true.   input is from WRF NMM  (NCEP model)
!             wrf_mass_regional = .true.  input is from WRF MASS-CORE (NCAR model)
!
!      new regional model added:
!
!             nems_nmmb_regional = .true.  input is from NEMS NMMB model
!             fv3_regional = .true.  input is from fv3 model
!             cmaq_regional = .true.  input is from CMAQ model
!
!     For a regional run, several additional namelist parameters must be specified:
!
!           diagnostic_reg   -- if .true., then run diagnostic tests for debugging
!           update_regsfc    -- if .true., then write updated surface fields to analysis file
!           nhr_assimilation -- assimilation interval in hours, =3 for current NMM assimilation
!           nhr_offset       -- time of analysis in assimilation window (hours)
!                    (following only needed for wrf_nmm_regional =.true.
!           filled_grid      -- if .true. fill in points on WRF NMM E-grid (expensive, but most accurate)
!           half_grid        -- if .true. use every other row of WRF NMM E-grid
!
!     Additional notes:
!
!
!        1.  For regional runs, there are specialized routines at the beginning and end of the
!               analysis for I/O.  Currently the options are for the WRF NMM and the WRF mass core.
!
!        2.  WRF restart files can now be directly read into GSI. There are currently 4 options,
!                a)  WRF NMM binary format
!                b)  WRF NMM netcdf format
!                c)  WRF MC  binary format
!                d)  WRF MC  netcdf format
!            To simplify the initial introduction of direct connection to WRF files, interface
!               routines are called at the beginning and end of gsimain, creating an intermediate
!               binary file which the code currently expects.  However this is now invisible
!               to the user.
!
!
!==================================================================================================

   integer(i_kind):: ier
   character(len=*),parameter:: myname='gsimain'

   call gsimain_initialize

   call my_fixture_config()     ! Choose configurable extensions for a
                                ! particular system fixture.  Note a user
                                ! defined gsi_fixture implementation is uniquely
                                ! selected in CMakeLists.txt at build-time.

! Initialize atmospheric AD and TL model trajectory
!  if(l4dvar) then
!     call gsi_4dcoupler_init_traj(idmodel,rc=ier)
!     if(ier/=0) call die(myname,'gsi_4dcoupler_init_traj(), rc =',ier)
!  endif

   call gsimain_run(init_pass=.true.,last_pass=.true.)

! Finalize atmospheric AD and TL model trajectory
   if(l4dvar) then
      call gsi_4dcoupler_final_traj(rc=ier)
      if(ier/=0) call die(myname,'gsi_4dcoupler_final_traj(), rc =',ier)
   endif

   call timer_pri(6)

   call gsimain_finalize

   end program gsi

