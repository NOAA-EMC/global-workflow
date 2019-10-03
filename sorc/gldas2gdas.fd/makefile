SHELL=	/bin/sh

include $(ESMFMKFILE)

CMD= gldas2gdas.exe

OBJS = driver.o \
       utils.o \
       setup.o \
       input_data.o \
       interp.o \
       write_data.o \
       model_grid.o

$(CMD):	$(OBJS)
	$(FCOMP) $(FFLAGS) $(ESMF_F90COMPILEPATHS) -o $(CMD) $(OBJS) $(SP_LIBd) $(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd) $(ESMF_F90LINKPATHS) $(ESMF_F90ESMFLINKRPATHS) $(ESMF_F90ESMFLINKLIBS) -g -traceback

driver.o:  write_data.o interp.o input_data.o setup.o model_grid.o driver.F90
	$(FCOMP) $(FFLAGS) $(ESMF_F90COMPILEPATHS) -c driver.F90

input_data.o:  model_grid.o input_data.F90
	$(FCOMP) $(FFLAGS) -I$(NEMSIO_INC) $(ESMF_F90COMPILEPATHS) -c input_data.F90

interp.o:  model_grid.o input_data.o interp.F90
	$(FCOMP) $(FFLAGS) $(ESMF_F90COMPILEPATHS) -c interp.F90

write_data.o:  interp.o model_grid.o write_data.F90
	$(FCOMP) $(FFLAGS) $(ESMF_F90COMPILEPATHS) -c write_data.F90

model_grid.o:  setup.o model_grid.F90
	$(FCOMP) $(FFLAGS) $(ESMF_F90COMPILEPATHS) -c model_grid.F90

utils.o:  utils.f90
	$(FCOMP) $(FFLAGS) $(ESMF_F90COMPILEPATHS) -c utils.f90

setup.o:  setup.f90
	$(FCOMP) $(FFLAGS) $(ESMF_F90COMPILEPATHS) -c setup.f90

clean:
	rm -f *.o *.mod ${CMD} ../../exec/${CMD}

install:
	mv $(CMD) ../../exec
