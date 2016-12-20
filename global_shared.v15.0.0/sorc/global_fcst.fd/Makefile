EXEC	 = $(EXECM)
F77	 = $(F77M)   
F90	 = $(F90M)   
CC	 = $(FCC)
#
FINCS    = $(FINC)

FRR 	 = $(FRRM)
OPTS     = $(OPTSM)
OPTS_SER = $(OPTS_SERM)
OPTS90   = $(OPTS90M)
OPTS90A  = $(OPTS90AM)
OPTSIO	 = $(OPTSIOM)
FFLAG90  = $(OPTS90) $(FINCS) $(FINCM) $(FRR)
FFLAG90A = $(OPTS90A) $(FINCS) $(FRR)
FFLAGS   = $(OPTS) 
FFLAGIO  = $(OPTSIO) $(FINCM)
FFLAGIOX = $(OPTSIOX) $(FINCM)
FFLAGM   = $(OPTS)  $(DEBUG)  
FFLAGM3  = $(OPTS)  $(DEBUG) $(FRR)
FFLAGSI  = $(OPTSBT) $(FRR)
FFLAG_SER = $(OPTS_SER) $(DOUBLEFLAG)  $(DEBUG) 

LDR	 = $(LDRM)
LDFLAGS =  $(LDFLAGSM)
LIBS =  $(LIBSM)

.SUFFIXES: .o .f .F .h

#
# *****************************************************************
#
OBJS0	= NameList_ESMFMod.o                       \
	  Sfc_Var_ESMFMod.o                        \
	  Nst_Var_ESMFMod.o                        \
	  GFS_ErrMsgMod.o                          \
	  Lib_ESMFStateAddGetMod.o                 \
	  GFS_InternalState_ESMFMod.o              \
	  GFS_ESMFStateMod.o                       \
	  GFS_GetCf_ESMFMod.o                      \
	  GFS_Initialize_ESMFMod.o                 \
	  GFS_Run_ESMFMod.o                        \
	  GFS_Finalize_ESMFMod.o                   \
	  GFS_AddParameterToStateMod.o             \
	  GFS_GridComp_ESMFMod.o                   \
	  Grid_ESMFCreate.o                        \
	  StartTimeGet_ESMF.o                      \
	  Ensemble_sub.o                           \
	  GEFS_Cpl_InternalState_ESMFMod.o         \
	  GEFS_CplState_ESMFMod.o                  \
	  GEFS_four2fln.o                          \
	  GEFS_uvoedz.o                            \
	  GEFS_uveodz.o                            \
	  GEFS_sumfln.o                            \
	  GEFS_dozeuv.o                            \
	  GEFS_dezouv.o                            \
	  GEFS_Sto_Per_Scheme_Step1.o              \
	  GEFS_Sto_Per_Scheme_Step2.o              \
	  GEFS_Cpl_Run_ESMFMod.o                   \
	  GEFS_Cpl_ESMFMod.o                       \
	  GEFS_GetParameterFromStateMod.o          \
	  GEFS_CplComp_ESMFMod.o                   \
	  GFS_ESMF.o                               \
	  Cal_Sto_Coef.o                           \
	  GEFS_bcst_global.o

OBJ_MOD	= machine.o                 \
	  num_parthds.o             \
	  iounitdef.o               \
	  physpara.o                \
	  physcons.o                \
	  funcphys.o                \
	  rascnvv2.o                \
	  mersenne_twister.o        \
	  resol_def.o               \
	  gg_def.o                  \
	  vert_def.o                \
	  sig_io.o                  \
	  date_def.o                \
	  layout1.o                 \
	  layout_grid_tracers.o     \
	  namelist_def.o            \
	  namelist_soilveg.o        \
	  coordinate_def.o          \
	  tracer_const_h-new.o      \
	  reduce_lons_grid_module.o \
	  mpi_def.o                 \
	  semi_lag_def.o            \
	  deldifs_def.o             \
	  bfilt_def.o               \
	  sfcio_module.o            \
	  gfsio_module.o            \
	  gfsio_def.o               \
	  d3d_def.o                 \
          nstio_module.o            \
          module_nst_parameters.o   \
          module_nst_water_prop.o   \
          module_nst_model.o        \
	  calpreciptype.o           \
	  module_bfmicrophysics.o   \
 	  kinds.o                   \
 	  peuc.o                    \
 	  pran.o                    \
	  prana.o                   \
	  layout_lag.o              \
	  slglat.o                  \
	  slgshr.o                  \
	  pmgrid.o                  \
	  get_cd_hyb_slg.o          \
	  sltini_all.o              \
	  set_halos_lag_sn.o        \
	  getcon_lag.o              \
	  gfidi_speed.o             \
	  setlats_lag.o             \
	  gfidi_tracers.o           \
          slgscan_all_redgg.o       \
	  get_topo_grid_grad.o      \
	  mpi_tracers_a_to_b.o      \
	  gloopa_tracers.o          \
	  ndslfv_base.o             \
	  ndslfv_para.o             \
	  ndslfv_ppm_pack.o         \
 	  sicdif_hyb_slg.o          \
 	  patterngenerator.o        \
          iau_module.o              \
 	  stoch_data.o

OBJS	=                                 \
	get_cd_hyb.o get_am_bm_hyb.o      \
	get_cd_hyb_gc.o get_am_bm_hyb_gc.o\
	setlats.o                         \
	getysk.o                          \
	getysk_gc.o                       \
	ifshuff.o                         \
	digifilt.o                        \
	gcycle.o                          \
	wrtout.o                          \
	grid_collect.o                    \
	GEFS_four2grid_thread.o           \
	info.o                            \
	four_to_grid.o                    \
	damp_speed.o                      \
	deldifs.o                         \
	deldifs_tracers.o                 \
	fix_fields.o                      \
	spect_fields.o                    \
	delnpe.o                          \
	delnpo.o                          \
	dezouv.o                          \
	impadj_slg.locl.o                 \
	impadj_hyb.locl.o                 \
	impadj_hyb.locl_gc.o              \
	dozeuv.o                          \
	epslon.o                          \
	filtr1eo.o                        \
	filtr2eo.o                        \
	get_ls_node.o                     \
	get_lats_node_shuff_a_fix.o       \
	get_lats_node_shuff_r_fix.o       \
	getcon.o                          \
	gfidi_hyb.o                       \
	gfidi_hyb_gc.o                    \
	gfidi_hyb_gc_h-new.o              \
	gfidi_hyb_gc_ndsl.o               \
	ndslfv_ppm_advecth1.o             \
	ndslfv_ppm_advecth.o              \
	glats.o                           \
	gloopa_opt_gg.o                   \
	lon_lat_para.o                    \
	compns.o                          \
	gozrineo.o                        \
	pln2eo.o                          \
	rms_spect.o                       \
	uvoedz.o                          \
	mpi_quit.o                        \
	sicdif_hyb.o                      \
	sicdif_hyb_gc.o                   \
	dotstep_tracers.o                 \
	ensemble_wrt.o                    \
	four2fln_gg.o                     \
	sortrx.o                          \
	sumfln_slg_gg.o                   \
	sumder2_slg_gg.o                  \
	triseori.o                        \
	uveodz.o                          \
	ysminv.o                          \
	fftpack.o                         

OBJS_PORT	=           \
	noblas.o
#	fftpack.o           \
#	four2grid.fftpack.o \

OBJS_RAD	=              \
	radlw_param.o          \
	radlw_datatb.o         \
	radlw_main.o           \
	radsw_param.o          \
	radsw_datatb.o         \
	radsw_main.o           \
	radiation_astronomy.o  \
	radiation_aerosols.o   \
	radiation_gases.o      \
	radiation_clouds.o     \
	radiation_surface.o    \
	grrad.o                \
	rad_initialize.o       \
	gloopr.o

OBJS_PHY=                         \
	ozinterp.o                \
	ozphys.o                  \
	gloopb.o                  \
	gbphys.o                  \
	gbphys_adv_hyb_gc.o       \
	gbphys_adv_hyb_gc_h-new.o \
	dcyc2.o                   \
	dcyc2.pre.rad.o           \
	set_soilveg.o             \
	sfc_drv.o                 \
	sfc_sice.o                \
	sfc_ocean.o               \
	sfc_nst.o                 \
	sfc_diff.o                \
	sfc_diag.o                \
	sflx.o                    \
	moninq.o                  \
	moninq1.o                 \
	moninedmf.o               \
	mfpbl.o                   \
	gwdps.o                   \
	rayleigh_damp.o           \
	gwdc.o                    \
	sascnvn.o                 \
	cnvc90.o                  \
	shalcnv.o                 \
	gscond.o                  \
	gscondp.o                 \
	precpd.o                  \
	precpdp.o                 \
	get_prs.o                 \
	gsmddrive.o               \
	omegtes.o                 \
	omegtes_gc.o              \
	hyb2press.o               \
	hyb2press_gc.o            \
	get_pattern.o

OBJS_IO=                      \
	mygather.o            \
	mpi_GV.o              \
	read_fix.o            \
	gribit.o              \
	wrt3d_hyb.o           \
	wrtsfc.o              \
	para_fixio_w.o        \
	para_nstio_w.o        \
	sfcsub.o              \
	treadeo.io.o          \
	treadeo.gfsio.o       \
	grid_to_spec.o        \
	spect_to_grid.o       \
	spect_tv_enthalpy_ps.o\
	twriteeo.o            \
	spect_write.o

OBJS_CC=            \
	mpi_more.o  \
	cmp.comm.o  \
	atm.comm.o  \
	tiles.o

SRC	= $(OBJS0:.o=.f) $(OBJ_MOD:.o=.f) $(OBJS:.o=.f) $(OBJS_RAD:.o=.f) $(OBJS_PHY:.o=.f) $(OBJS_IO:.o=.f) $(OBJS_CC:.o=.f)
#
INCS = mpi_inc.h function_indlsev function_indlsod

#
# *****************************************************************
#
all: model-mpi

model-mpi: $(OBJ_MOD) $(OBJS_CC) $(OBJS0) $(OBJS) $(OBJS_PHY) $(OBJS_RAD) $(OBJS_IO) 
	$(LDR) -o $(EXEC) $(OBJ_MOD) $(OBJS_CC) $(OBJS0) $(OBJS) $(OBJS_PHY) $(OBJS_RAD) $(OBJS_IO) $(LIBS) $(LDFLAGS)

model-mpi-port: $(OBJ_MOD) $(OBJS_CC) $(OBJS0) $(OBJS) $(OBJS_PHY) $(OBJS_RAD) $(OBJS_IO) $(OBJS_PORT)
	$(LDR) -o $(EXEC) $(OBJ_MOD) $(OBJS_CC) $(OBJS0) $(OBJS) $(OBJS_PHY) $(OBJS_RAD) $(OBJS_IO) $(OBJS_PORT) $(LIBS) $(LDFLAGS)

install:
	cp -p $(EXEC) ../../exec/.
clean:
	rm -f $(OBJ_MOD) $(OBJS0) $(OBJS) $(OBJS_RAD) $(OBJS_PHY) $(OBJS_IO) $(OBJS_CC) *.mod

tar:
	tar -cvf tar.gfs.r4r8 $(SRC) $(INCS) $(COMS) $(OBJS_PORT:.o=.f) lonsper* res* xx* Makefile* ini.* scr.* m*real_?

.F.o:
	$(F77) $(FFLAGS) -c -d $< 
	#$(F77) $(FFLAGS) -c -d -WF,-DCLR:${RASV} $< 
.f.o:
	$(F77) $(FFLAGS) -c $< 

#
# *****************************************************************
#
cnvc90.o:	cnvc90.f
		$(F77) $(FFLAGM) -c cnvc90.f

calpreciptype.o:	calpreciptype.f
		$(F77) $(FFLAGM3) -c calpreciptype.f

dcyc2.o:	dcyc2.f
		$(F77) $(FFLAGM) -c dcyc2.f

dcyc2.pre.rad.o:	dcyc2.pre.rad.f
		$(F77) $(FFLAGM) -c dcyc2.pre.rad.f

digifilt.o:	digifilt.f
		$(F77) $(FFLAGS) -c digifilt.f

gbphys_adv_hyb_gc.o:	gbphys_adv_hyb_gc.f
		$(F77) $(FFLAGM) -c gbphys_adv_hyb_gc.f

gbphys_adv_hyb_gc_h-new.o:	gbphys_adv_hyb_gc_h-new.f
		$(F77) $(FFLAGM) -c gbphys_adv_hyb_gc_h-new.f

gbphys.o:	gbphys.f
		$(F77) $(FFLAGM) -c gbphys.f

get_prs.o:	get_prs.f
		$(F77) $(FFLAGM) -c get_prs.f

filtr1eo.o:	filtr1eo.f 
		$(F77) $(FFLAG_SER)   -c filtr1eo.f 

filtr2eo.o:	filtr2eo.f 
		$(F77) $(FFLAG_SER)   -c filtr2eo.f 

gscond.o:	gscond.f
		$(F77) $(FFLAGM) -c gscond.f

gscondp.o:	gscondp.f
		$(F77) $(FFLAGM) -c gscondp.f

gsmddrive.o:	gsmddrive.f
		$(F77) $(FFLAGM) -c gsmddrive.f

reduce_lons_grid_module.o:	reduce_lons_grid_module.f
		$(F77) $(FFLAGM) -c reduce_lons_grid_module.f

module_bfmicrophysics.o:	module_bfmicrophysics.f
		$(F77) $(FFLAGM) -c module_bfmicrophysics.f

gwdps.o:	gwdps.f
		$(F77) $(FFLAGM) -c gwdps.f

rayleigh_damp.o:	rayleigh_damp.f
		$(F77) $(FFLAGM) -c rayleigh_damp.f

gwdc.o:		gwdc.f
		$(F77) $(FFLAGM) -c gwdc.f

hyb2press_gc.o:	hyb2press_gc.f
		$(F77) $(FFLAGM) -c hyb2press_gc.f

hyb2press.o:	hyb2press.f
		$(F77) $(FFLAGM) -c hyb2press.f

moninq.o:	moninq.f
		$(F77) $(FFLAGM) -c moninq.f

moninq1.o:	moninq1.f
		$(F77) $(FFLAGM) -c moninq1.f

moninedmf.o:	moninedmf.f
		$(F77) $(FFLAGM) -c moninedmf.f

mfpbl.o:	mfpbl.f
		$(F77) $(FFLAGM) -c mfpbl.f

omegtes_gc.o:	omegtes_gc.f
		$(F77) $(FFLAGM) -c omegtes_gc.f

omegtes.o:	omegtes.f
		$(F77) $(FFLAGM) -c omegtes.f

ozinterp.o:	ozinterp.f
		$(F77) $(FFLAGM) -c ozinterp.f

ozphys.o:	ozphys.f
		$(F77) $(FFLAGM) -c ozphys.f

precpd.o:	precpd.f
		$(F77) $(FFLAGM) -c precpd.f

precpdp.o:	precpdp.f
		$(F77) $(FFLAGM) -c precpdp.f

rascnvv2.o:	rascnvv2.f
		$(F77) $(FFLAGM) -c rascnvv2.f

sfc_sice.o:	sfc_sice.f
		$(F77) $(FFLAGM) -c sfc_sice.f

set_soilveg.o:	set_soilveg.f
		$(F77) $(FFLAGM) -c set_soilveg.f

namelist_soilveg.o:	namelist_soilveg.f
		$(F77) $(FFLAGM) -c namelist_soilveg.f

sfc_drv.o:	sfc_drv.f
		$(F77) $(FFLAGM) -c sfc_drv.f

sflx.o: 	sflx.f
		$(F77) $(FFLAGM) -c sflx.f

sfc_ocean.o:	sfc_ocean.f
		$(F77) $(FFLAGM) -c sfc_ocean.f

sfc_nst.o:	sfc_nst.f
		$(F77) $(FFLAGM) -c sfc_nst.f

sfc_diff.o:	sfc_diff.f
		$(F77) $(FFLAGM) -c sfc_diff.f

sfc_diag.o:	sfc_diag.f
		$(F77) $(FFLAGM) -c sfc_diag.f

sascnvn.o:	sascnvn.f
		$(F77) $(FFLAGM) -c sascnvn.f

shalcnv.o:	shalcnv.f
		$(F77) $(FFLAGM) -c shalcnv.f

# *****************************************************************

radlw_param.o:	radlw_param.f
		$(F77) $(FFLAGM) -c radlw_param.f


radlw_datatb.o:	radlw_datatb.f
		$(F77) $(FFLAGM) -c radlw_datatb.f


radlw_main.o:	radlw_main.f
		$(F77) $(FFLAGM) -c radlw_main.f

radsw_param.o:	radsw_param.f
		$(F77) $(FFLAGM) -c radsw_param.f

radsw_datatb.o:	radsw_datatb.f
		$(F77) $(FFLAGM) -c radsw_datatb.f

radsw_main.o:	radsw_main.f
		$(F77) $(FFLAGM) -c radsw_main.f

radiation_astronomy.o:	radiation_astronomy.f
		$(F77) $(FFLAGM) -c radiation_astronomy.f

radiation_aerosols.o:	radiation_aerosols.f
		$(F77) $(FFLAGM) -c radiation_aerosols.f

radiation_gases.o:	radiation_gases.f
		$(F77) $(FFLAGM) -c radiation_gases.f

radiation_clouds.o:	radiation_clouds.f
		$(F77) $(FFLAGM) -c radiation_clouds.f

radiation_surface.o:	radiation_surface.f
		$(F77) $(FFLAGM) -c radiation_surface.f

grrad.o:	grrad.f
		$(F77) $(FFLAGM) -c grrad.f

rad_initialize.o:	rad_initialize.f
		$(F77) $(FFLAGM) -c rad_initialize.f

machine.o:	machine.f
		$(F77) $(FFLAGM) -c machine.f

num_parthds.o:	num_parthds.f
		$(F77) $(FFLAGM) -c num_parthds.f

kinds.o:	GEFS_Cpl_Cal_Sto_Coef.fd/kinds.f90
		$(F90) $(FFLAG90) -c GEFS_Cpl_Cal_Sto_Coef.fd/kinds.f90

peuc.o:		GEFS_Cpl_Cal_Sto_Coef.fd/peuc.f90
		$(F90) $(FFLAG90) -c GEFS_Cpl_Cal_Sto_Coef.fd/peuc.f90

pran.o:		GEFS_Cpl_Cal_Sto_Coef.fd/pran.f90
		$(F90) $(FFLAG90) -c GEFS_Cpl_Cal_Sto_Coef.fd/pran.f90

prana.o:	GEFS_Cpl_Cal_Sto_Coef.fd/prana.f90
		$(F90) $(FFLAG90) -c GEFS_Cpl_Cal_Sto_Coef.fd/prana.f90

#
# *****************************************************************
#
physpara.o:	physpara.f
		$(F77) $(FFLAG90) -c physpara.f

physcons.o:	physcons.f
		$(F77) $(FFLAG90) -c physcons.f

iounitdef.o:	iounitdef.f
		$(F77) $(FFLAG90) -c iounitdef.f

funcphys.o:	funcphys.f
		$(F77) $(FFLAG90) -c funcphys.f

sfcio_module.o:	sfcio_module.f
		$(F77) $(FFLAGSI) -c sfcio_module.f

gfsio_def.o:	gfsio_def.f
		$(F77) $(FFLAGSI) -c gfsio_def.f

gfsio_module.o:	gfsio_module.f
		$(F77) $(FFLAGSI) -c gfsio_module.f

read_fix.o:	read_fix.f
		$(F77) $(FFLAGS) -c read_fix.f

softcount.o:	softcount.f
		$(F77) $(FFLAGS) -c softcount.f

gloopr.o:	gloopr.f
		$(F77) $(FFLAGS) -c gloopr.f

sfcsub.o:	sfcsub.f
		$(F77B) $(FFLAGS) -c sfcsub.f

gcycle.o:	gcycle.f
		$(F77) $(FFLAGM) -c gcycle.f

gribit.o:	gribit.f
		$(F77) $(FFLAGIO) -c gribit.f

wrtsfc.o:	wrtsfc.f
		$(F77) $(FFLAGIO) -c wrtsfc.f

para_fixio_w.o:		para_fixio_w.f
		$(F77) $(FFLAGIO) -c para_fixio_w.f

para_nstio_w.o:		para_nstio_w.f
		$(F77) $(FFLAGIO) -c para_nstio_w.f

treadeo.io.o:	treadeo.io.f
		$(F77) $(FFLAGIO) -c treadeo.io.f

treadeo.gfsio.o:	treadeo.gfsio.f
		$(F77) $(FFLAGIO) -c treadeo.gfsio.f

twriteeo.o:	twriteeo.f
		$(F77) $(FFLAGIO) -c twriteeo.f

#spect_send.o:	spect_send.f
#		$(F77) $(FFLAGIO) -c spect_send.f

spect_write.o:	spect_write.f
		$(F77) $(FFLAGIO) -c spect_write.f

spect_to_grid.o:	spect_to_grid.f
		$(F77) $(FFLAGIO) -c spect_to_grid.f

spect_tv_enthalpy_ps.o:	spect_tv_enthalpy_ps.f
		$(F77) $(FFLAGIO) -c spect_tv_enthalpy_ps.f

grid_to_spec.o:	grid_to_spec.f
		$(F77) $(FFLAGIO) -c grid_to_spec.f

wrtout.o:	wrtout.f
		$(F77) $(FFLAGIOX) -c wrtout.f

nstio_module.o:	 nstio_module.f
		$(F77) $(FFLAG90) -c nstio_module.f

module_nst_parameters.o:	module_nst_parameters.f
		$(F77) $(FFLAG90) -c module_nst_parameters.f

module_nst_water_prop.o:	 module_nst_water_prop.f
		$(F77) $(FFLAG90) -c module_nst_water_prop.f

module_nst_model.o:		module_nst_model.f
		$(F77) $(FFLAG90) -c module_nst_model.f

NameList_ESMFMod.o:	 NameList_ESMFMod.f
		$(F77) $(FFLAG90) -c NameList_ESMFMod.f

Sfc_Var_ESMFMod.o:	Sfc_Var_ESMFMod.f
		$(F77) $(FFLAG90) -c Sfc_Var_ESMFMod.f

Nst_Var_ESMFMod.o:	Nst_Var_ESMFMod.f
		$(F77) $(FFLAG90) -c Nst_Var_ESMFMod.f

Lib_ESMFStateAddGetMod.o:	Lib_ESMFStateAddGetMod.f
		$(F77) $(FFLAG90) -c Lib_ESMFStateAddGetMod.f

GFS_InternalState_ESMFMod.o:	 GFS_InternalState_ESMFMod.f
		$(F77) $(FFLAG90) -c GFS_InternalState_ESMFMod.f

GFS_ESMFStateMod.o:	 GFS_ESMFStateMod.f
		$(F77) $(FFLAG90) -c GFS_ESMFStateMod.f

GFS_ErrMsgMod.o:	 GFS_ErrMsgMod.f
		$(F77) $(FFLAG90) -c GFS_ErrMsgMod.f

GFS_GetCf_ESMFMod.o:	 GFS_GetCf_ESMFMod.f
		$(F77) $(FFLAG90) -c GFS_GetCf_ESMFMod.f

GFS_ESMFMod.o:	 GFS_ESMFMod.f
		$(F77) $(FFLAG90) -c GFS_ESMFMod.f

GFS_GridComp_ESMFMod.o:	 GFS_GridComp_ESMFMod.f
		$(F77) $(FFLAG90A) -c GFS_GridComp_ESMFMod.f

GFS_Initialize_ESMFMod.o:	 GFS_Initialize_ESMFMod.f
		$(F77) $(FFLAG90) -c GFS_Initialize_ESMFMod.f

GFS_Run_ESMFMod.o:	GFS_Run_ESMFMod.f
		$(F77) $(FFLAG90) -c GFS_Run_ESMFMod.f

GFS_Finalize_ESMFMod.o:	GFS_Finalize_ESMFMod.f
		$(F77) $(FFLAG90) -c GFS_Finalize_ESMFMod.f

GFS_ESMF.o:	GFS_ESMF.f
		$(F77) $(FFLAG90) -c GFS_ESMF.f

Grid_ESMFCreate.o:	 Grid_ESMFCreate.f
		$(F77) $(FFLAG90) -c Grid_ESMFCreate.f

StartTimeGet_ESMF.o:	 StartTimeGet_ESMF.f
		$(F77) $(FFLAG90) -c StartTimeGet_ESMF.f

Ensemble_sub.o:	 Ensemble_sub.f
		$(F77) $(FFLAG90) -c Ensemble_sub.f

mpi_more.o:	mpi_more.f
		$(F77) $(FFLAGS) -c mpi_more.f

cmp.comm.o:	cmp.comm.f
		$(F77) $(FFLAGS) -c cmp.comm.f

atm.comm.o:	atm.comm.f
		$(F77) $(FFLAGS) -c atm.comm.f

patterngenerator.o:     patterngenerator.f90
	        $(F90) $(FFLAG90) -c patterngenerator.f90

iau_module.o:   iau_module.f90
	        $(F90) $(FFLAG90) -c iau_module.f90

stoch_data.o:   stoch_data.f90
	        $(F90) $(FFLAG90) -c stoch_data.f90

get_pattern.o:  get_pattern.f90
	        $(F90) $(FFLAG90) -c get_pattern.f90

tiles.o:	tiles.f
		$(F77) $(FFLAGS) -c tiles.f

GEFS_Cpl_InternalState_ESMFMod.o:	GEFS_Cpl_InternalState_ESMFMod.f
		$(F77) $(FFLAG90) -c GEFS_Cpl_InternalState_ESMFMod.f

GEFS_CplState_ESMFMod.o:	GEFS_CplState_ESMFMod.f
		$(F77) $(FFLAG90) -c GEFS_CplState_ESMFMod.f

GEFS_Sto_Per_Scheme.o:	GEFS_Sto_Per_Scheme.f
		$(F77) $(FFLAG90) -c GEFS_Sto_Per_Scheme.f

GEFS_Cpl_Run_ESMFMod.o:	GEFS_Cpl_Run_ESMFMod.f
		$(F77) $(FFLAG90) -c GEFS_Cpl_Run_ESMFMod.f

GEFS_Cpl_ESMFMod.o:	GEFS_Cpl_ESMFMod.f
		$(F77) $(FFLAG90) -c GEFS_Cpl_ESMFMod.f

GEFS_CplComp_ESMFMod.o:	GEFS_CplComp_ESMFMod.f
		$(F77) $(FFLAG90) -c GEFS_CplComp_ESMFMod.f

GFS_AddParameterToStateMod.o:	GFS_AddParameterToStateMod.f
		$(F77) $(FFLAG90) -c GFS_AddParameterToStateMod.f

GEFS_Sto_Per_Scheme_Step1.o:	GEFS_Sto_Per_Scheme_Step1.f
		$(F77) $(FFLAG90) -c GEFS_Sto_Per_Scheme_Step1.f

GEFS_Sto_Per_Scheme_Step2.o:	GEFS_Sto_Per_Scheme_Step2.f
		$(F77) $(FFLAG90) -c GEFS_Sto_Per_Scheme_Step2.f

GEFS_GetParameterFromStateMod.o:	GEFS_GetParameterFromStateMod.f
		$(F77) $(FFLAG90) -c GEFS_GetParameterFromStateMod.f

Cal_Sto_Coef.o:		GEFS_Cpl_Cal_Sto_Coef.fd/Cal_Sto_Coef.f90
		$(F90) $(FFLAG90) -c GEFS_Cpl_Cal_Sto_Coef.fd/Cal_Sto_Coef.f90

GEFS_bcst_global.o:	GEFS_bcst_global.f
		$(F77) $(FFLAG90) -c GEFS_bcst_global.f

mpi_GV.o:		mpi_GV.f
		$(F77) $(FFLAGS) -c mpi_GV.f

mygather.o:		mygather.c
		$(CC) -c mygather.c
