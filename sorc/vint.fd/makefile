SHELL=  /bin/sh
ISIZE = 4
RSIZE = 8
COMP=   ifort
##INC = /contrib/nceplibs/nwprod/lib/incmod/g2_d
##LIBS=   -L/contrib/nceplibs/nwprod/lib -lw3emc_d -lw3nco_d -lg2_d -lbacio_4 -ljasper -lpng -lz
LDFLAGS=
# FFLAGS= -O3 -I $(INC)  -i$(ISIZE) -r$(RSIZE)
# DEBUG= -check all -debug all -traceback
FFLAGS= -O2 -g -traceback -I $(INC)  -i$(ISIZE) -r$(RSIZE)

vint:      vint.f
	@echo " "
	@echo "  Compiling the interpolation program....."
	$(COMP) $(FFLAGS) $(LDFLAGS) vint.f $(LIBS) -o vint.x
	@echo " "

.PHONY: clean

CMD =   vint.x

clean:
	-rm -f  *.o  *.mod

install:
	mv $(CMD) ../../exec/$(CMD)

