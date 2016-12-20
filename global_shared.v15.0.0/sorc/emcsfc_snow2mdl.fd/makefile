SHELL=	/bin/sh

#------------------------------------------------------------------------
#  Makefile for emcsfc_snow2mdl program.
#
#  Before invoking, read the README.build file for details.
#------------------------------------------------------------------------

CMD= emcsfc_snow2mdl

OBJS = driver.o \
       program_setup.o \
       snowdat.o \
       model_grid.o \
       grib_utils.o \
       snow2mdl.o 

LIBS= $(G2_LIBd) $(IP_LIBd) $(SP_LIBd) $(LANDSFCUTIL_LIBd) $(BACIO_LIB4) \
      $(W3NCO_LIBd) $(JASPER_LIB) $(PNG_LIB) $(Z_LIB)

$(CMD):	$(OBJS)
	$(FCOMP) $(FFLAGS) -o $(CMD) $(OBJS) $(LIBS)

driver.o: snowdat.o model_grid.o snow2mdl.o program_setup.o driver.f
	$(FCOMP) $(FFLAGS) -c driver.f
program_setup.o: program_setup.f
	$(FCOMP) $(FFLAGS) -c program_setup.f
snowdat.o: program_setup.o model_grid.o snowdat.f
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -c snowdat.f
model_grid.o: program_setup.o model_grid.f
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -c model_grid.f
snow2mdl.o: snowdat.o program_setup.o model_grid.o snow2mdl.f
	$(FCOMP) $(FFLAGS) -I$(LANDSFCUTIL_INCd) -I$(G2_INCd) -c snow2mdl.f
grib_utils.o: grib_utils.f
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -c grib_utils.f
all: check_prereqs $(CMD)
clean:
	rm -f *.o *.mod $(CMD) ../../exec/$(CMD)
install:
	/usr/bin/install -c -d ../../exec
	/usr/bin/install -c $(CMD) ../../exec
check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)
	/nwprod/spa_util/check_incs.bash $(G2_INCd)/grib_mod.mod $(LANDSFCUTIL_INCd)/consts.mod
