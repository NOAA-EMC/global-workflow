SHELL = /bin/sh

inside_nems := $(wildcard ../../../conf/configure.nems)
ifneq ($(strip $(inside_nems)),)
    include ../../../conf/configure.nems
else
    exist_configure_fv3 := $(wildcard ../conf/configure.fv3)
    ifneq ($(strip $(exist_configure_fv3)),)
        include ../conf/configure.fv3
    else
        $(error "../conf/configure.fv3 file is missing. Run ./configure")
    endif
    $(info )
    $(info Build standalone FV3 fv3core ...)
    $(info )
endif

LIBRARY  = libfv3core.a

FFLAGS   += -I../fms -I../fms/include -I../gfsphysics -I../io

SRCS_f   =

SRCS_f90 =

SRCS_F   =

SRCS_F90 = \
		   ./model/a2b_edge.F90                           \
		   ./model/boundary.F90                           \
		   ./model/dyn_core.F90                           \
		   ./model/fv_arrays.F90                          \
		   ./model/fv_cmp.F90                             \
		   ./model/fv_control.F90                         \
		   ./model/fv_dynamics.F90                        \
		   ./model/fv_fill.F90                            \
		   ./model/fv_grid_utils.F90                      \
 		   ./model/fv_mapz.F90                            \
		   ./model/fv_nesting.F90                         \
		   ./model/fv_sg.F90                              \
		   ./model/fv_tracer2d.F90                        \
		   ./model/fv_update_phys.F90                     \
		   ./model/sw_core.F90                            \
		   ./model/tp_core.F90                            \
		   ./model/nh_core.F90                            \
		   ./model/nh_utils.F90                           \
		   ./tools/external_ic.F90                        \
		   ./tools/external_sst.F90                       \
		   ./tools/fv_diagnostics.F90                     \
		   ./tools/fv_eta.F90                             \
		   ./tools/fv_grid_tools.F90                      \
		   ./tools/fv_io.F90                              \
		   ./tools/fv_mp_mod.F90                          \
		   ./tools/fv_nudge.F90                           \
		   ./tools/fv_treat_da_inc.F90                    \
		   ./tools/fv_iau_mod.F90                         \
		   ./tools/fv_restart.F90                         \
		   ./tools/fv_surf_map.F90                        \
		   ./tools/fv_timing.F90                          \
		   ./tools/init_hydro.F90                         \
		   ./tools/sim_nc_mod.F90                         \
		   ./tools/sorted_index.F90                       \
		   ./tools/test_cases.F90                         \
		   ./driver/fvGFS/DYCORE_typedefs.F90             \
		   ./driver/fvGFS/fv_nggps_diag.F90               \
		   ./driver/fvGFS/atmosphere.F90

SRCS_c   = 

DEPEND_FILES = $(SRCS_f) $(SRCS_f90) $(SRCS_F) $(SRCS_F90)

OBJS_f   = $(SRCS_f:.f=.o)
OBJS_f90 = $(SRCS_f90:.f90=.o)
OBJS_F   = $(SRCS_F:.F=.o)
OBJS_F90 = $(SRCS_F90:.F90=.o)
OBJS_c   = $(SRCS_c:.c=.o)

OBJS = $(OBJS_f) $(OBJS_f90) $(OBJS_F) $(OBJS_F90) $(OBJS_c)

all default: depend $(LIBRARY)

$(LIBRARY): $(OBJS)
	$(AR) $(ARFLAGS) $@ $?

# this is the place to override default (implicit) compilation rules
# and create specific (explicit) rules

./model/nh_utils.o : ./model/nh_utils.F90
	$(FC) $(CPPDEFS) $(FPPFLAGS) $(FFLAGS) $(OTHER_FFLAGS) $(FAST) -c $< -o $@

./model/fv_mapz.o : ./model/fv_mapz.F90
	$(FC) $(CPPDEFS) $(FPPFLAGS) $(FFLAGS) $(OTHER_FFLAGS) $(FAST) -c $< -o $@

./driver/fvGFS/fv_nggps_diag.o : ./driver/fvGFS/fv_nggps_diag.F90
	$(FC) $(CPPDEFS) $(FPPFLAGS) $(FFLAGS) $(OTHER_FFLAGS) $(ESMF_INC) -c $< -o $@

.PHONY: clean
clean:
	@echo "Cleaning fv3core ... "
	@echo
	$(RM) -f $(LIBRARY) *__genmod.f90 */*.o */*/*.o  *.mod *.lst *.i depend

MKDEPENDS = ../mkDepends.pl
include ../conf/make.rules

# do not include 'depend' file if the target contains string 'clean'
ifneq (clean,$(findstring clean,$(MAKECMDGOALS)))
    -include depend
endif

