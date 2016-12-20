SHELL=/bin/sh

#==============================================================================
#
# GSI Makefile
#
# <Usage>
#   0) Export this makefile name to a variable 'MAKE_FILE' as 
#       export MAKE_FILE = makefile
#      If this file is named neither 'makefile' nor 'Makefile' but 
#      'makeairs' for instance, then call this makefile by typing
#      'make -f makeairs' instead of 'make'.
#
#  0a) Modify the include link to either use compile.config.ibm
#      or compile.config.sgi for compilation on the ibm sp or sgi
#
#   1) To make a GSI executable file, type
#         > make  or  > make all
#
#   2) To make a GSI executable file with debug options, type
#         > make debug
#
#   3) To copy the GSI load module to installing directory, type
#         > make install
#      . Specify the directory to a variable 'INSTALL_DIR' below.
#
#   4) To crean up files created by make, type
#         > make clean
#
#   5) To create a library, libgsi.a, in the lib directory, type
#         > make library
#
#
# Created by Y.Tahara in May,2002
# Edited by D.Kleist Oct. 2003
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Parent make (calls child make) --
#-----------------------------------------------------------------------------

# -----------------------------------------------------------
# Default configuration, possibily redefined in Makefile.conf
# -----------------------------------------------------------

ARCH	 = `uname -s`
SED      = sed
DASPERL  = /usr/bin/perl
COREROOT = ../../..
COREBIN  = $(COREROOT)/bin
CORELIB  = $(COREROOT)/lib
COREINC  = $(COREROOT)/include
COREETC  = $(COREROOT)/etc


# -------------
# General Rules
# -------------

CP              = /bin/cp -p
RM              = /bin/rm -f
MKDIR           = /bin/mkdir -p
AR              = ar cq
PROTEX          = protex -f # -l
ProTexMake      = protex -S # -l
LATEX           = pdflatex
DVIPS           = dvips

# Preprocessing
# -------------
_DDEBUG =
_D      = $(_DDEBUG)

# ---------
# Libraries
# ---------
LIBmpeu   = -L$(CORELIB) -lmpeu
LIBbfr    = -L$(CORELIB) -lbfr
LIBw3     = -L$(CORELIB) -lw3
LIBsp     = -L$(CORELIB) -lsp
LIBbacio  = -L$(CORELIB) -lbacio
LIBsfcio  = -L$(CORELIB) -lsfcio
LIBsigio  = -L$(CORELIB) -lsigio
LIBcrtm   = -L$(CORELIB) -lcrtm_2.1.3
LIBtransf = -L$(CORELIB) -ltransf
LIBhermes = -L$(CORELIB) -lhermes
LIBgfio   = -L$(CORELIB) -lgfio

# --------------------------
# Default Baselibs Libraries
# --------------------------
INChdf          = -I$(BASEDIR)/$(ARCH)/include/hdf
LIBhdf          = -L$(BASEDIR)/$(ARCH)/lib  -lmfhdf -ldf -lhdfjpeg -lhdfz -lsz
LIBnetcdf       = -L$(BASEDIR)/$(ARCH)/lib -lnetcdf
LIBwrf          = -L$(BASEDIR)/$(ARCH)/lib -lwrflib
LIBwrfio_int    = -L$(BASEDIR)/$(ARCH)/lib -lwrfio_int
LIBwrfio_netcdf = -L$(BASEDIR)/$(ARCH)/lib -lwrfio_nf

# ------------------------
# Default System Libraries
# ------------------------
LIBmpi          = -lmpi
LIBsys          = 


#------------
# Include machine dependent compile & load options
#------------
  MAKE_CONF = Makefile.conf
include $(MAKE_CONF)


# -------------
# This makefile
# -------------

  MAKE_FILE = Makefile


# -----------
# Load module
# -----------

  EXE_FILE = global_gsi


# --------------------
# Installing directory
# --------------------

  INSTALL_DIR = ../bin


# --------
# Log file
# --------

  LOG_FILE = log.make.$(EXE_FILE)


# ---------------
# Call child make
# ---------------

"" :
	@$(MAKE) -f $(MAKE_FILE) all


# ------------
# Make install
# ------------

install:
	@echo
	@echo '==== INSTALL ================================================='
	@if [ -e $(INSTALL_DIR) ]; then \
	  if [ ! -d $(INSTALL_DIR) ]; then \
	    echo '### Fail to create installing directory ###' ;\
	    echo '### Stop the installation               ###' ;\
	    exit ;\
	  fi ;\
	else \
	  echo "	mkdir -p $(INSTALL_DIR)" ;\
	  mkdir -p $(INSTALL_DIR) ;\
	fi
	cp $(EXE_FILE) $(INSTALL_DIR)
	@cd $(INSTALL_DIR) ; ls -l `pwd`/$(EXE_FILE)


# ----------
# Make clean
# ----------

clean:
	@echo
	@echo '==== CLEAN ==================================================='
	- $(RM) $(EXE_FILE) *.o *.mod *.MOD *.lst *.a *.x *__genmod*
	- $(RM) loadmap.txt log.make.$(EXE_FILE)
	- $(MAKE) -f ${MAKE_FILE} doclean


#-----------------------------------------------------------------------------
#                          -- Child make --
#-----------------------------------------------------------------------------

# ------------
# Source files
# ------------

  SRCSF90C = \
	gfs_stratosphere.f90 \
	adjtest.f90 \
        adjtest_obs.f90 \
        adjust_cloudobs_mod.f90 \
	aeroinfo.f90 \
	aircraftinfo.f90 \
        aircraftobsqc.f90 \
	anberror.f90 \
	anbkerror.f90 \
	aniso_ens_util.f90 \
	anisofilter.f90 \
	anisofilter_glb.f90 \
	antcorr_application.f90 \
	antest_maps0.f90 \
	antest_maps0_glb.f90 \
	atms_spatial_average_mod.f90 \
	balmod.f90 \
	berror.f90 \
	bias_predictors.f90 \
	bicg.f90 \
	bicglanczos.F90 \
	bkerror.f90 \
	bkgcov.f90 \
	bkgvar.f90 \
	bkgvar_rewgt.f90 \
	blacklist.f90 \
	blendmod.f90 \
	calc_fov_conical.f90 \
	calc_fov_crosstrk.f90 \
	calctends.f90 \
	calctends_ad.f90 \
	calctends_tl.f90 \
	calctends_no_ad.f90 \
	calctends_no_tl.f90 \
	chemmod.f90 \
	clw_mod.f90 \
	cloud_efr_mod.f90 \
	cmaq_routines.f90 \
	co_mop_ak.f90 \
	coinfo.f90 \
	combine_radobs.f90 \
	compact_diffs.f90 \
	compute_derived.f90 \
	compute_fact10.f90 \
	compute_qvar3d.f90 \
	constants.f90 \
	control2model.f90 \
	control2state.f90 \
	control_vectors.f90 \
	converr.f90 \
        converr_ps.f90 \
        converr_q.f90 \
        converr_t.f90 \
        converr_uv.f90 \
        converr_pw.f90 \
        convb_ps.f90 \
        convb_q.f90 \
        convb_t.f90 \
        convb_uv.f90 \
	convinfo.f90 \
	convthin.f90 \
	convthin_time.f90 \
        correlated_obsmod.F90 \
	crtm_interface.f90 \
	cvsection.f90 \
	cwhydromod.f90 \
	dtast.f90 \
	deter_sfc_mod.f90 \
        derivsmod.f90 \
	egrid2agrid_mod.f90 \
	enorm_state.f90 \
	ensctl2state.f90 \
	ensctl2model.f90 \
        en_perts_io.f90 \
	evaljgrad.f90 \
	evaljo.f90 \
	evalqlim.f90 \
	fgrid2agrid_mod.f90 \
	fill_mass_grid2.f90 \
	fill_nmm_grid2.f90 \
	fpvsx_ad.f90 \
        general_commvars_mod.f90 \
	general_read_gfsatm.f90 \
        general_read_nmmb.f90 \
	general_specmod.f90 \
	general_spectral_transforms.f90 \
	general_sub2grid_mod.f90 \
	general_transform.f90 \
        general_tll2xy_mod.f90 \
	general_write_gfsatm.f90 \
	gengrid_vars.f90 \
	genqsat.f90 \
	genstats_gps.f90 \
	gesinfo.f90 \
        getcount_bufr.f90 \
	get_derivatives.f90 \
	get_derivatives2.f90 \
	get_gefs_for_regional.f90 \
        get_nmmb_ensperts.f90 \
        get_pseudo_ensperts.F90 \
	get_wrf_mass_ensperts_netcdf.F90 \
        get_wrf_nmm_ensperts.F90 \
	get_semimp_mats.f90 \
	getprs.f90 \
	getsiga.f90 \
	getuv.f90 \
	getvvel.f90 \
	glbsoi.f90 \
	grtest.f90 \
	grdcrd.f90 \
	gridmod.F90 \
	gscond_ad.f90 \
	gsd_terrain_match_surfTobs.f90 \
	gsdcloudanalysis.F90 \
	gsdcloudanalysis4NMMB.F90 \
        gsd_update_mod.f90 \
	gsi_4dvar.f90 \
	gsi_4dcouplermod.f90 \
	gsi_bundlemod.F90 \
	gsi_chemguess_mod.F90 \
	gsi_enscouplermod.f90 \
	gsi_nstcouplermod.f90 \
	gsi_io.f90 \
	gsi_metguess_mod.F90 \
	gsi_nemsio_mod.f90 \
	gsimain.f90 \
	gsimod.F90 \
	gsisub.F90 \
	guess_grids.F90 \
	half_nmm_grid2.f90 \
	hilbert_curve.f90 \
	hybrid_ensemble_isotropic.F90 \
	hybrid_ensemble_parameters.f90 \
	inc2guess.f90 \
	init_jcdfi.f90 \
	insitu_info.f90 \
	int3dvar.f90 \
	intall.f90 \
        intaod.f90 \
	intco.f90 \
	intdw.f90 \
	intgps.f90 \
	intgust.f90 \
	inthowv.f90 \
	intcldch.f90 \
        intjcmod.f90 \
	intjo.f90 \
	intlag.f90 \
	intlcbas.f90 \
	intmitm.f90 \
	intmxtm.f90 \
	intoz.f90 \
	intpblh.f90 \
	intpcp.f90 \
	intpm2_5.f90 \
        intpm10.f90 \
	intpmsl.f90 \
	intps.f90 \
	intpw.f90 \
	intq.f90 \
	intrad.f90 \
        intrp_msk.f90 \
	intrp2a.f90 \
	intrp3oz.f90 \
	intrw.f90 \
	intspd.f90 \
	intsrw.f90 \
	intsst.f90 \
	intt.f90 \
	inttcamt.f90 \
	inttcp.f90 \
	inttd2m.f90 \
	intvis.f90 \
	intw.f90 \
	intwspd10m.f90 \
	jcmod.f90 \
	jfunc.f90 \
	jgrad.f90 \
	kinds.F90 \
	lag_fields.f90 \
	lag_interp.f90 \
	lag_traj.f90 \
	lagmod.f90 \
	lanczos.F90 \
	logcldch_to_cldch.f90 \
	loglcbas_to_lcbas.f90 \
	logvis_to_vis.f90 \
	looplimits.f90 \
	m_berror_stats.f90 \
	m_berror_stats_reg.f90 \
	m_dgeevx.F90 \
	m_distance.f90	\
	m_dtime.F90	\
	m_find.f90 \
	m_gpsrhs.F90 \
	m_gsiBiases.f90 \
	m_rerank.f90 \
	m_obdiag.F90	\
	m_rhs.F90	\
	m_sortind.f90 \
	m_stats.f90 \
	m_tick.F90 \
	m_uniq.f90 \
	mpeu_mpif.F90 \
	mpeu_util.F90 \
	mod_nmmb_to_a.f90 \
	mod_strong.f90 \
	mod_vtrans.f90 \
        mod_wrfmass_to_a.f90 \
	model_ad.F90 \
	model_tl.F90 \
	control2model_ad.f90 \
	ensctl2model_ad.f90 \
	mp_compact_diffs_mod1.f90 \
	mp_compact_diffs_support.f90 \
	mpimod.F90 \
	mpl_allreduce.F90 \
	mpl_bcast.f90 \
        native_endianness.f90 \
	netcdf_mod.f90 \
	ncepgfs_ghg.f90 \
	ncepgfs_io.f90 \
	ncepnems_io.f90 \
	nlmsas_ad.f90 \
	normal_rh_to_q.f90 \
	nstio_module.f90 \
	Nst_Var_ESMFMod.f90 \
        obserr_allsky_mw.f90 \
	obs_ferrscale.F90 \
	obs_sensitivity.f90 \
	obsmod.F90 \
	omegas_ad.f90 \
	ozinfo.f90 \
	patch2grid_mod.f90 \
	pcgsoi.f90 \
	pcgsqrt.f90 \
	pcp_k.f90 \
	pcpinfo.f90 \
	penal.f90 \
	phil.f90 \
	phil1.f90 \
	plib8.f90 \
	polcarf.f90 \
	prad_bias.f90 \
	precond.f90 \
	precpd_ad.f90 \
	prewgt.f90 \
	prewgt_reg.f90 \
	projmethod_support.f90 \
	prt_guess.f90 \
	psichi2uv_reg.f90 \
	psichi2uvt_reg.f90 \
	q_diag.f90 \
	qcmod.f90 \
	qnewton3.f90 \
	raflib.f90 \
	rapidrefresh_cldsurf_mod.f90 \
	rdgrbsst.f90 \
	read_files.f90 \
	read_gfs_ozone_for_regional.f90 \
	read_guess.F90 \
        read_mitm_mxtm.f90 \
	read_obsdiags.F90 \
	read_wrf_mass_files.F90 \
	read_wrf_mass_guess.F90 \
	read_wrf_nmm_files.F90 \
	read_wrf_nmm_guess.F90 \
	regional_io.f90 \
	reorg_metar_cloud.f90 \
	rfdpar.f90 \
	rsearch.F90 \
	rtlnmc_version3.f90 \
	satthin.F90 \
	set_crtm_aerosolmod.f90 \
	set_crtm_cloudmod.f90 \
        setupaod.f90 \
        setuppm10.f90 \
	setupyobs.f90 \
	sfc_model.f90 \
	sfcobsqc.f90 \
	simpin1.f90 \
	simpin1_init.f90 \
	smooth_polcarf.f90 \
	smoothrf.f90 \
	smoothwwrf.f90 \
	smoothzrf.f90 \
	sqrtmin.f90 \
	ssmis_spatial_average_mod.f90 \
	control2state_ad.f90 \
	ensctl2state_ad.f90 \
	state_vectors.f90 \
	statsco.f90 \
	statsconv.f90 \
	statsoz.f90 \
	statspcp.f90 \
	statsrad.f90 \
	stop1.f90 \
	stp3dvar.f90 \
        stpaod.f90 \
	stpcalc.f90 \
	stpco.f90 \
	stpdw.f90 \
	stpgps.f90 \
	stpgust.f90 \
	stphowv.f90 \
	stpcldch.f90 \
	stpjo.f90 \
        stpjcmod.f90 \
	stplcbas.f90 \
	stpoz.f90 \
	stppblh.f90 \
	stppcp.f90 \
	stppm2_5.f90 \
	stppm10.f90 \
	stppmsl.f90 \
	stpmitm.f90 \
	stpmxtm.f90 \
	stpps.f90 \
	stppw.f90 \
	stpq.f90 \
	stprad.f90 \
	stprw.f90 \
	stpspd.f90 \
	stpsrw.f90 \
	stpsst.f90 \
	stpt.f90 \
	stptcamt.f90 \
	stptcp.f90 \
	stptd2m.f90 \
	stpvis.f90 \
	stpw.f90 \
	stpwspd10m.f90 \
	strong_bal_correction.f90 \
	strong_baldiag_inc.f90 \
	strong_fast_global_mod.f90 \
	sub2fslab_mod.f90 \
	support_2dvar.f90 \
	cplr_gfs_ensmod.f90 \
	cplr_gfs_nstmod.f90 \
	stub_pertmod.F90 \
	stub_set_crtm_aerosol.f90 \
	stub_timermod.f90 \
	tendsmod.f90 \
	test_obsens.f90 \
	tcv_mod.f90 \
	timermod.f90 \
	tintrp2a.f90 \
	tintrp3.f90 \
	tpause.f90 \
	tpause_t.F90 \
        tune_pbl_height.f90 \
	turbl.f90 \
	turbl_ad.f90 \
	turbl_tl.f90 \
	turblmod.f90 \
	tv_to_tsen.f90 \
	unfill_mass_grid2.f90 \
	unfill_nmm_grid2.f90 \
	unhalf_nmm_grid2.f90 \
	update_guess.f90 \
	update_geswtend.f90 \
	wind_fft.f90 \
	wrf_mass_guess_mod.f90 \
	wrf_binary_interface.F90 \
	wrf_netcdf_interface.F90 \
	write_all.F90 \
	write_bkgvars_grid.f90 \
	write_obsdiags.F90 \
	wrwrfmassa.F90 \
	wrwrfnmma.F90 \
	xhat_vordivmod.f90 \
	zrnmi_mod.f90

  SRCSF90C_NOSWAP = \
	buddycheck_mod.f90 \
	get_gefs_ensperts_dualres.f90 \
	gsi_unformatted.F90 \
        m_extOzone.F90 \
	obs_para.f90 \
	observer.F90 \
	oneobmod.F90 \
	radinfo.f90 \
	read_Lightning.f90 \
	read_nasa_larc.f90 \
	read_radarref_mosaic.f90 \
	read_aerosol.f90 \
	read_airs.f90 \
	read_amsr2.f90 \
	read_amsre.f90 \
	read_anowbufr.f90 \
	read_avhrr.f90 \
	read_avhrr_navy.f90 \
	read_atms.f90 \
	read_bufrtovs.f90 \
	read_co.f90 \
	read_cris.f90 \
	read_diag.f90 \
        read_fl_hdob.f90 \
	read_gmi.f90 \
	read_goesimg.f90 \
	read_ahi.f90 \
	read_goesimgr_skycover.f90 \
	read_goesndr.f90 \
	read_gps.f90 \
	read_lag.f90 \
	read_iasi.f90 \
	read_l2bufr_mod.f90 \
	read_lidar.f90 \
	read_modsbufr.f90 \
	read_nsstbufr.f90 \
	read_obs.F90 \
	read_ozone.f90 \
	read_pblh.f90 \
	read_pcp.f90 \
	read_prepbufr.f90 \
	read_radar.f90 \
	read_saphir.f90 \
	read_satwnd.f90 \
	read_sfcwnd.f90 \
	read_rapidscat.f90 \
	read_seviri.f90 \
	read_ssmi.f90 \
	read_ssmis.f90 \
	read_superwinds.f90 \
	read_tcps.f90 \
	setupbend.f90 \
	setupco.f90 \
	setupdw.f90 \
	setupgust.f90 \
	setuphowv.f90 \
	setupcldch.f90 \
	setuplag.f90 \
	setuplcbas.f90 \
	setupmitm.f90 \
	setupmxtm.f90 \
	setupoz.f90 \
	setuppblh.f90 \
	setuppcp.f90 \
	setuppmsl.f90 \
	setuppm2_5.f90 \
	setupps.f90 \
	setuppw.f90 \
	setupq.f90 \
	setuprad.f90 \
	setupref.f90 \
	setuprhsall.f90 \
	setuprw.f90 \
	setupspd.f90 \
	setupsrw.f90 \
	setupsst.f90 \
	setupt.f90 \
	setuptcamt.f90 \
	setuptcp.f90 \
	setuptd2m.f90 \
	setupvis.f90 \
	setupw.f90 \
	setupwspd10m.f90 \
	sst_retrieval.f90 \
        read_NASA_LaRC_cloud.f90

  GSIGC_SRCS =

  SRCSF77 =

  SRCSC = blockIO.c

  SRCS = $(SRCSF90C) $(GSIGC_SRCS) $(SRCSF77) $(SRCSC) $(XSRCSC)

  DOCSRCS = *.f90 *.F90

# ------------
# Object files
# ------------

  SRCSF90	= ${SRCSF90C:.F90=.f90}
  SRCSF90_NOSWAP= ${SRCSF90C_NOSWAP:.F90=.f90}

  OBJS 		= ${SRCSF90:.f90=.o} ${SRCSF77:.f=.o} ${SRCSC:.c=.o}
  OBJS_NOSWAP	= ${SRCSF90_NOSWAP:.f90=.o}


# -----------------------
# Default compiling rules
# -----------------------

.SUFFIXES :
.SUFFIXES : .F90 .f90 .f .c .o

.F90.o  :
	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGS) $(_D) -c $<

.f90.o  :
	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGS) -c $<

.f.o  :
	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGS_f) -c $<

.c.o  :
	@echo
	@echo '---> Compiling $<'
	$(CC) $(CFLAGS) -c $<

$(OBJS_NOSWAP) :
	@echo '---> Special handling of Fortran "native" BUFR-OPEN $<'
	$(CF) -c $(FFLAGS_NOSWAP) $<


# ------------
# Dependencies
# ------------
  MAKE_DEPEND = Makefile.dependency
include $(MAKE_DEPEND)

# ----

$(EXE_FILE) :  $(OBJS) $(OBJS_NOSWAP)
	$(LD) $(LDFLAGS) -o $@ $(OBJS) $(OBJS_NOSWAP) $(LIBS)


# ------------------------
# Call compiler and linker
# ------------------------

all :
	@$(MAKE) -f $(MAKE_FILE) "COMP_MODE=$@" check_mode
	@echo
	@echo '==== COMPILE ================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"FFLAGS=$(FFLAGS_N)" \
		"FFLAGS_NOSWAP=$(FFLAGS_NOSWAP_N)" \
		"CFLAGS=$(CFLAGS_N)" \
		$(OBJS) $(OBJS_NOSWAP)
	@echo
	@echo '==== LINK ===================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"LIBS=$(LIBS_N)" "LDFLAGS=$(LDFLAGS_N)" \
		$(EXE_FILE)

library :
	@$(MAKE) -f $(MAKE_FILE) "COMP_MODE=$@" check_mode
	@echo
	@echo '==== COMPILE ================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"FFLAGS=$(FFLAGS_N)" \
		"FFLAGS_NOSWAP=$(FFLAGS_NOSWAP_N)" \
		"CFLAGS=$(CFLAGS_N)" \
		$(OBJS) $(OBJS_NOSWAP)
	@echo
	@echo '==== CREATING LIBRARY ========================================'
	$(MAKE) lib
	mv $(LIB) ../lib

debug :
	@$(MAKE) -f $(MAKE_FILE) "COMP_MODE=$@" check_mode
	@echo
	@echo '==== COMPILE ================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"FFLAGS=$(FFLAGS_D)" \
		"FFLAGS_NOSWAP=$(FFLAGS_NOSWAP_D)" \
		"CFLAGS=$(CFLAGS_D)" \
		$(OBJS) $(OBJS_NOSWAP)
	@echo
	@echo '==== LINK ===================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"LIBS=$(LIBS_D)" "LDFLAGS=$(LDFLAGS_D)" \
		$(EXE_FILE)

check_mode :
	@if [ -e $(LOG_FILE) ]; then \
	  if [ '$(COMP_MODE)' != `head -n 1 $(LOG_FILE)` ]; then \
	    echo ;\
	    echo "### COMPILE MODE WAS CHANGED ###" ;\
	    $(MAKE) -f $(MAKE_FILE) clean ;\
	  fi ;\
	else \
	  echo ;\
	  echo "### NO LOG FILE ###" ;\
	  $(MAKE) -f $(MAKE_FILE) clean ;\
	fi
	@echo $(COMP_MODE) > $(LOG_FILE)

# -------------------------
# GMAO Nomenclature/targets
# -------------------------
LIB =   libgsi.a

lib: $(LIB)

gsi.x:  $(OBJS) $(OBJS_NOSWAP) $(LIB)
	$(FC) $(LDFLAGS) -o gsi.x gsimain.o libgsi.a $(LIBcrtm) $(LIBsfcio) $(LIBsigio) $(LIBw3) $(LIBbacio) $(LIBbfr) $(LIBsp) $(LIBtransf) $(LIBhermes) $(LIBmpeu) $(LIBgfio) $(LIBhdf) $(LIBmpi) $(LIBsys)

ut_gsibundle.x:  $(OBJS) $(OBJS_NOSWAP) $(LIB) ut_gsibundle.o
	$(FC) $(LDFLAGS) -o ut_gsibundle.x ut_gsibundle.o libgsi.a $(LIBcrtm) $(LIBsfcio) $(LIBsigio) $(LIBw3) $(LIBbacio) $(LIBbfr) $(LIBsp) $(LIBtransf) $(LIBhermes) $(LIBmpeu) $(LIBgfio) $(LIBhdf) $(LIBmpi) $(LIBsys)

prepbykx.x: prepbykx.o
	$(FC) $(LDFLAGS) -o prepbykx.x prepbykx.o $(LIBbfr)

$(LIB): $(OBJS) $(OBJS_NOSWAP)
	$(RM) $(LIB)
	$(AR) $@ $(OBJS) $(OBJS_NOSWAP)

MAIN_OBJS = gsimain.o
LIBS_OBJS = $(OBJS) $(OBJS_NOSWAP)

list-main_objs:
	@ for f in $(MAIN_OBJS); do echo $$f; done | env LC_ALL=C sort -u

list-libs_objs:
	@ for f in $(LIBS_OBJS); do echo $$f; done | grep -v `for p in $(MAIN_OBJS); do echo "-e $$p"; done` | env LC_ALL=C sort -u

export: libgsi.a gsi.x prepbykx.x
	$(MKDIR)               $(COREBIN)
	$(CP) $(LIB)           $(CORELIB)
	$(CP) gsi.x            $(COREBIN)
	$(CP) gsi.rc.sample    $(COREETC)/gsi.rc
	$(CP) tlmadj_parameter.rc.sample $(COREETC)/tlmadj_parameter.rc
	$(CP) gmao_airs_bufr.tbl       $(COREETC)/gmao_airs_bufr.tbl
	$(CP) gmao_global_pcpinfo.txt  $(COREETC)/gmao_global_pcpinfo.rc
	$(CP) gmao_global_satinfo.txt  $(COREETC)/gmao_global_satinfo.rc
	$(CP) gmao_global_ozinfo.txt   $(COREETC)/gmao_global_ozinfo.rc
	$(CP) gmao_global_convinfo.txt $(COREETC)/gmao_global_convinfo.rc
	$(SED) -e "s^@DASPERL^$(DASPERL)^" < analyzer > $(COREBIN)/analyzer
	chmod 755 $(COREBIN)/analyzer

doc:              AnIntro $(DOCSRC)
	$(PROTEX) AnIntro *.f90 *.F90 >  gsi.tex
	$(LATEX) gsi.tex
	$(LATEX) gsi.tex

doclean:
	- $(RM) *.tex *.dvi *.aux *.toc *.log *.ps *.pdf

help:
	@ echo "Available targets:"
	@ echo "NCEP:  make             creates gsi executable        "
	@ echo "NPEP:  make debug       created gsi exec for debugging purposes"
	@ echo "NCEP:  make install     creates gsi exec & places it in bin"
	@ echo "GMAO:  make lib         creates gsi library"
	@ echo "GMAO:  make export      creates lib, exec, & copies all to bin/inc/etc"
	@ echo "       make clean       cleans objects, exec, and alien files"  
	@ echo "       make doc         creates documentation"
	@ echo "       make doclean     clean doc-related temporary files"

