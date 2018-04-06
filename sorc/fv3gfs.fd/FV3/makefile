SHELL = /bin/sh

include conf/configure.fv3

FFLAGS   += -Ifms -Igfsphysics -Iio -Iatmos_cubed_sphere

FV3_EXE  = fv3.exe
FV3CAP_LIB  = libfv3cap.a

all: libs
	$(MAKE) $(FV3_EXE) $(MAKEFLAGS)

nems: libs
	$(MAKE) $(FV3CAP_LIB) $(MAKEFLAGS)
	$(MAKE) esmf_make_fragment

libs:
	$(MAKE) -C fms                 $(MAKEFLAGS)
	$(MAKE) -C gfsphysics          $(MAKEFLAGS) 32BIT=N  # force gfs physics to 64bit
	$(MAKE) -C io                  $(MAKEFLAGS)
	$(MAKE) -C atmos_cubed_sphere  $(MAKEFLAGS)
	$(MAKE) -C stochastic_physics  $(MAKEFLAGS) 32BIT=N  # force gfs physics to 64bit

$(FV3_EXE): atmos_model.o coupler_main.o atmos_cubed_sphere/libfv3core.a io/libfv3io.a gfsphysics/libgfsphys.a stochastic_physics/libstochastic_physics.a fms/libfms.a 
	$(LD) -o $@ $^ $(NCEPLIBS) $(LDFLAGS)

$(FV3CAP_LIB): atmos_model.o module_fv3_config.o module_fcst_grid_comp.o time_utils.o fv3_cap.o
	ar rv $(FV3CAP_LIB) $?

module_fv3_config.o: module_fv3_config.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) $(ESMF_INC) -c module_fv3_config.F90
module_fcst_grid_comp.o: module_fcst_grid_comp.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) $(ESMF_INC) -c module_fcst_grid_comp.F90
time_utils.o: time_utils.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) $(ESMF_INC) -c time_utils.F90
fv3_cap.o: fv3_cap.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) $(OTHER_FFLAGS) $(ESMF_INC) -c fv3_cap.F90

DEPEND_FILES = time_utils.F90 module_fv3_config.F90 atmos_model.F90 module_fcst_grid_comp.F90 fv3_cap.F90 coupler_main.F90

esmf_make_fragment:
	@rm -rf nems_dir; mkdir nems_dir
	@cp $(FV3CAP_LIB) atmos_cubed_sphere/libfv3core.a io/libfv3io.a gfsphysics/libgfsphys.a fms/libfms.a stochastic_physics/libstochastic_physics.a nems_dir
#	@cp $(FV3CAP_LIB) atmos_cubed_sphere/libfv3core.a io/libfv3io.a gfsphysics/libgfsphys.a fms/libfms.a nems_dir
	@cp fv3gfs_cap_mod.mod nems_dir
	@echo "# ESMF self-describing build dependency makefile fragment" > fv3.mk
	@echo "# src location $(PWD)" >> fv3.mk
	@echo  >> fv3.mk
	@echo "ESMF_DEP_FRONT     = fv3gfs_cap_mod"  >> fv3.mk
	@echo "ESMF_DEP_INCPATH   = $(PWD)/nems_dir" >> fv3.mk
	@echo "ESMF_DEP_CMPL_OBJS ="                 >> fv3.mk
	@echo "ESMF_DEP_LINK_OBJS = $(addprefix $(PWD)/nems_dir/, libfv3cap.a libfv3core.a libfv3io.a libgfsphys.a libfms.a libstochastic_physics.a)"  >> fv3.mk
#	@echo "ESMF_DEP_LINK_OBJS = $(addprefix $(PWD)/nems_dir/, libfv3cap.a libfv3core.a libfv3io.a libgfsphys.a libfms.a )"  >> fv3.mk
	@echo "ESMF_DEP_SHRD_PATH ="                 >> fv3.mk
	@echo "ESMF_DEP_SHRD_LIBS ="                 >> fv3.mk
	@echo
	@echo "Finished generating ESMF self-describing build dependency makefile fragment:" fv3.mk
	@echo

# fv3 library installation defaults (for NEMS):
DESTDIR  := $(PWD)
INSTDIR  := FV3_INSTALL

nemsinstall: nems
	@mkdir -p $(DESTDIR)/$(INSTDIR)
	@cp nems_dir/* $(DESTDIR)/$(INSTDIR)
	@sed -e 's;'$(PWD)/nems_dir';'$(DESTDIR)/$(INSTDIR)';g' fv3.mk > $(DESTDIR)/$(INSTDIR)/fv3.mk
	@echo Installation into \"$(DESTDIR)/$(INSTDIR)\" complete!
	@echo

.PHONY: clean cleanall
clean:
	@echo "Cleaning ... "
	@echo
	(cd fms                 && make clean)
	(cd gfsphysics          && make clean)
	(cd stochastic_physics  && make clean)
	(cd io                  && make clean)
	(cd atmos_cubed_sphere  && make clean)
	$(RM) -f $(FV3_EXE) $(FV3CAP_LIB) *.o *.mod *.lst depend

cleanall: clean
	$(RM) -rf nems_dir fv3.mk $(INSTDIR)
	$(RM) -f conf/modules.fv3
	$(RM) -f conf/configure.fv3

MKDEPENDS = ./mkDepends.pl
include conf/make.rules

# do not include 'depend' file if the target contains string 'clean'
ifneq (clean,$(findstring clean,$(MAKECMDGOALS)))
    -include depend
endif

