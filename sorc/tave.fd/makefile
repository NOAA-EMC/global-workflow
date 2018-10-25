SHELL=  /bin/sh
ISIZE = 4
RSIZE = 8
COMP=   ifort
##INC = /contrib/nceplibs/nwprod/lib/incmod/g2_d
##LIBS=    -L/contrib/nceplibs/nwprod/lib -lw3emc_d -lw3nco_d -lg2_d -lbacio_4 -ljasper -lpng -lz
LDFLAGS= 
# DEBUG= -check all -debug all -traceback
FFLAGS= -O2 -g -traceback -I $(INC)  -i$(ISIZE) -r$(RSIZE)
# FFLAGS= -O3 -I $(INC)  -i$(ISIZE) -r$(RSIZE) 

tave:      tave.f
	@echo " "
	@echo "  Compiling the interpolation program....."
	$(COMP) $(FFLAGS) $(LDFLAGS) tave.f $(LIBS) -o tave.x
	@echo " "

CMD =   tave.x

clean:
	-rm -f  *.o  *.mod

install:
	mv $(CMD) ../../exec/$(CMD)

