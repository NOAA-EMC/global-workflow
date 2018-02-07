SHELL=  /bin/sh
ISIZE = 4
RSIZE = 8
FCOMP=  ifort -nofree
CCOMP=  icc
##INC = /contrib/nceplibs/nwprod/lib/incmod/g2_d
# LIBS=   -L/nwprod/lib -lw3emc_d -lw3nco_d -lbacio_4
##LIBS=   -L/contrib/nceplibs/nwprod/lib -lw3emc_d -lw3nco_d -lg2_d -lbacio_4 -ljasper -lpng -lz
LDFLAGS= 
FFLAGS= -O2 -fpe0 -I $(INC)   -i$(ISIZE) -r$(RSIZE)
CFLAGS= -O2

gettrk:      gettrk_main.f gettrk_modules.o module_waitfor.o cwaitfor.o
	@echo " "
	@echo "  Compiling the main tracking program and subroutines....."
	$(FCOMP) $(FFLAGS) $(LDFLAGS) gettrk_modules.o module_waitfor.o cwaitfor.o gettrk_main.f $(LIBS) -o gettrk
	@echo " "

cwaitfor.o: cwaitfor.c
	@echo " "
	@echo "  Compiling the waitfor C routine...."
	$(CCOMP) $(CFLAGS) -c cwaitfor.c -o cwaitfor.o

module_waitfor.o: module_waitfor.f
	@echo " "
	@echo "  Compiling the waitfor fortran module...."
	$(FCOMP) $(FFLAGS) -c module_waitfor.f -o module_waitfor.o

gettrk_modules.o:    gettrk_modules.f
	@echo " "
	@echo "  Compiling the regular tracker fortran modules....."
	$(FCOMP) $(FFLAGS) -c gettrk_modules.f -o gettrk_modules.o
	@echo " "

CMD =   gettrk

clean:
	-rm -f  *.o  *.mod

install:
	mv $(CMD) ../../exec/$(CMD)
