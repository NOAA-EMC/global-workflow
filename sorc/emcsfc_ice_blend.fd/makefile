#!/bin/sh

#------------------------------------------------------------------------
#  Makefile for the emcsfc_ice_blend program.
#
#  Before invoking, read the README.build for details.
#------------------------------------------------------------------------

EXE=emcsfc_ice_blend

OBJS= emcsfc_ice_blend.o

LIBS= $(BACIO_LIB4) $(G2_LIB4) $(W3NCO_LIB4) $(JASPER_LIB) $(PNG_LIB) $(Z_LIB)

$(EXE):	$(OBJS)
	$(FCOMP) $(FFLAGS) -o $(EXE) $(OBJS) $(LIBS)

emcsfc_ice_blend.o:  emcsfc_ice_blend.f90
	$(FCOMP) $(FFLAGS) -I$(G2_INC4) -c emcsfc_ice_blend.f90

all: check_prereqs $(EXE)
clean:
	rm -f *.o $(EXE) ../../exec/$(EXE)
install:
	/usr/bin/install -c -d ../../exec
	/usr/bin/install -c $(EXE) ../../exec
check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)
	/nwprod/spa_util/check_incs.bash $(G2_INC4)/grib_mod.mod
