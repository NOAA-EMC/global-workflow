SHELL=  /bin/sh
ISIZE = 4
RSIZE = 8
COMP=   ifort
##LIBS_SUP=   -L/contrib/nceplibs/nwprod/lib -lw3emc_d -lw3nco_d -lg2_d -lbacio_4 -ljasper -lpng -lz
LDFLAGS= 
##ccs FFLAGS= -O -qflttrap=ov:zero:inv:enable -qcheck -qextchk -qwarn64 -qintsize=$(ISIZE) -qrealsize=$(RSIZE)
# FFLAGS= -O2 -check bounds -check format -xHost -fpe0
# DEBUG= -check bounds -check format
FFLAGS= -O2 -g -i$(ISIZE) -r$(RSIZE)

supvit:     supvit_main.f supvit_modules.o
	@echo " "
	@echo "  Compiling program that sorts and updates vitals records...."
	$(COMP) $(FFLAGS) $(LDFLAGS) supvit_modules.o supvit_main.f $(LIBS_SUP) -o supvit
	@echo " "

supvit_modules.o:   supvit_modules.f
	@echo " "
	@echo "  Compiling the modules....."
	$(COMP) -c supvit_modules.f -o supvit_modules.o
	@echo " "

CMD =   supvit

clean:
	-rm -f  *.o  *.mod

install:
	mv $(CMD) ../../exec/$(CMD)

