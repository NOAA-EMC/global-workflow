SHELL=	/bin/sh

CMD= gaussian_sfcanl.exe

OBJS = gaussian_sfcanl.o

build: $(CMD)

$(CMD):	$(OBJS)
	$(FCOMP) $(FFLAGS) -I$(NEMSIO_INC) $(NETCDF_INCLUDE) -o $(CMD) $(OBJS) $(NETCDF_LDFLAGS_F) $(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd) $(SP_LIB4)

gaussian_sfcanl.o:  gaussian_sfcanl.f90
	$(FCOMP) $(FFLAGS) -I$(NEMSIO_INC) $(NETCDF_INCLUDE) -c gaussian_sfcanl.f90

install:
	cp ${CMD} ../../exec

clean:
	rm -f *.o *.mod ${CMD} ../../exec/${CMD}

test:
	@echo NO TESTS YET
