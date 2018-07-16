SHELL=	/bin/sh

LIBS= $(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd) $(IP_LIBd) $(SP_LIBd)

CMD= chgres_recenter.exe

OBJS = driver.o output_data.o setup.o utils.o grid2grid.o printrusage.o

$(CMD):	$(OBJS)
	$(FC) $(FFLAGS) -o $(CMD) $(OBJS) $(LIBS)

printrusage.o: printrusage.c
	$(CC) $(CFLAGS) -c printrusage.c

driver.o:  setup.o output_data.o grid2grid.o driver.f90
	$(FC) $(FFLAGS) -I$(NEMSIO_INC) -c driver.f90

output_data.o: setup.o utils.o grid2grid.o output_data.f90
	$(FC) $(FFLAGS) -I$(NEMSIO_INC) -c output_data.f90

setup.o:  setup.f90
	$(FC) $(FFLAGS) -I$(NEMSIO_INC) -c setup.f90

utils.o:  utils.f90
	$(FC) $(FFLAGS) -I$(NEMSIO_INC) -c utils.f90

grid2grid.o: utils.o setup.o grid2grid.f90
	$(FC) $(FFLAGS) -I$(NEMSIO_INC) -c grid2grid.f90

clean:
	rm -f *.o *.mod ${CMD}
install:
	-cp $(CMD) ../../exec/.
