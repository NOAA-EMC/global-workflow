SHELL=  /bin/sh

LIBS= $(NETCDF)/lib/libnetcdff.so $(FV3GFS_NCIO_LIB) $(BACIO_LIB4) $(W3NCO_LIB4) $(IP_LIB4) $(SP_LIB4)

CMD= chgres_recenter_ncio.exe 

OBJS = driver.o input_data.o interp.o output_data.o utils.o setup.o

$(CMD): $(OBJS)
	$(FC) $(FFLAGS) -o $(CMD) $(OBJS) $(LIBS)

driver.o:  setup.o output_data.o interp.o input_data.o driver.f90
	$(FC) $(FFLAGS) -I$(FV3GFS_NCIO_INC) -I$(NETCDF)/include -c driver.f90
interp.o:  setup.o utils.o output_data.o input_data.o interp.f90
	$(FC) $(FFLAGS) -I$(FV3GFS_NCIO_INC) -I$(NETCDF)/include -c interp.f90
input_data.o:  setup.o utils.o input_data.f90
	$(FC) $(FFLAGS) -I$(FV3GFS_NCIO_INC) -I$(NETCDF)/include -c input_data.f90
output_data.o: setup.o utils.o  input_data.o output_data.f90
	$(FC) $(FFLAGS) -I$(FV3GFS_NCIO_INC) -I$(NETCDF)/include -c output_data.f90
setup.o:  setup.f90
	$(FC) $(FFLAGS) -I$(FV3GFS_NCIO_INC) -I$(NETCDF)/include -c setup.f90
utils.o:  utils.f90
	$(FC) $(FFLAGS) -I$(FV3GFS_NCIO_INC) -I$(NETCDF)/include -c utils.f90
clean:
	rm -f *.o *.mod ${CMD}
install:
	-cp $(CMD) ../../exec/.

