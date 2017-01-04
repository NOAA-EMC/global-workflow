include ../../conf/configure.nems

MAKEFILE = makefile

UTILINCS = -I../share -I../libutil
ARCH=$(shell uname -s)

LIBRARY  = libstochy_data.a

OPTS       = $(FFLAGS) $(R8) $(UTILINCS)

FFLAG90  = $(OPTS) $(FREE)
FFLAGS1  = $(OPTS) $(FIXED)

OBJS   = \
	mersenne_twister_stochy.o   \
	spectral_layout.o    \
	stochy_gg_def.o      \
	stochy_resol_def.o   \
	stochy_mpi_def.o     \
	stochy_layout_lag.o  \
	four_to_grid_stochy.o       \
	glats_stochy.o              \
	sumfln_stochy.o             \
	gozrineo_stochy.o           \
	num_parthds_stochy.o        \
	intlon_stochy.o             \
	uninterpred_stochy.o        \
	mpi_quit_stochy.o           \
	get_ls_node_stochy.o        \
	get_lats_node_a_stochy.o    \
	setlats_a_stochy.o          \
	epslon_stochy.o             \
	setlats_lag_stochy.o        \
	getcon_lag_stochy.o         \
	pln2eo_stochy.o             \
	dozeuv_stochy.o      \
	dezouv_stochy.o      \

OBJS0=                     \
	getcon_spectral.o           \
	stochy_namelist_def.o              \
	interpolation_interface.o   \
	compns_stochy.o             \
	stochy_internal_state_mod.o \
	stochy_patterngenerator.o   \
	stochy_data_mod.o           \
	get_stochy_pattern.o        \
	initialize_spectral_mod.o 



	DEPEND_FILES = ${OBJS0:.o=.f} ${OBJS:.o=.f}

all default: depend
	@gmake -f $(MAKEFILE) $(LIBRARY)

$(LIBRARY): $(OBJS0) $(OBJS) 
	$(AR) $(ARFLAGS) $@ $?

$(OBJS): %.o: %.f90
	$(FC) $(FFLAGS1) -c $*.f90
$(OBJS0): %.o: %.f90
	$(FC) $(FFLAG90) -c $*.f90

clean:
	$(RM) -f $(LIBRARY) *.o *.mod *.lst *.f90 lm map depend

#
# *****************************************************************
#

.SUFFIXES: .f .f90 .o

.f.f90:
	$(CPP) $< > $*.f90


#gfs_dyn_physcons.o:	gfs_dyn_physcons.f90
#		$(FC) $(FFLAG90) -c gfs_dyn_physcons.f90

#

MKDEPENDS = ../../../exe/mkDepends.pl

include ../../conf/make.rules

include depend
