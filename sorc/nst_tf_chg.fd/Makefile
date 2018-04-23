SHELL=	/bin/sh

CMD= nst_tf_chg.x

OBJS =  setup.o nst_tf_chg.o  read_tfs_nc.o  smth9_msk.o nc_check.o

build: $(CMD)

$(CMD):	$(OBJS)
	$(FCOMP) $(FFLAGS) -I$(NEMSIO_INC) $(NETCDF_INCLUDE) -o $(CMD) $(OBJS) $(NETCDF_LDFLAGS_F) $(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd)

setup.o:  setup.f90
	$(FCOMP) $(FFLAGS) -c setup.f90
nst_tf_chg.o:  nst_tf_chg.f90
	$(FCOMP) $(FFLAGS) -I$(NEMSIO_INC) $(NETCDF_INCLUDE) -c nst_tf_chg.f90
nc_check.o:  nc_check.f90
	$(FCOMP) $(FFLAGS) $(NETCDF_INCLUDE) -c nc_check.f90
read_tfs_nc.o:  read_tfs_nc.f90
	$(FCOMP) $(FFLAGS) $(NETCDF_INCLUDE) -c read_tfs_nc.f90
smth9_msk.o:  smth9_msk.f90
	$(FCOMP) $(FFLAGS) -c smth9_msk.f90

install:
	cp ${CMD} ../../exec

clean:
	rm -f *.o *.mod ${CMD} ../../exec/${CMD}

test:
	@echo NO TESTS YET
