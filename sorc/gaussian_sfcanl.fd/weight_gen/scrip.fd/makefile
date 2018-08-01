SHELL=	/bin/sh

CMD= scrip.exe

OBJS = scrip.o

$(CMD):	$(OBJS)
	$(FCOMP) $(FFLAGS) $(NETCDF_INCLUDE) -o $(CMD) $(OBJS) $(NETCDF_LDFLAGS_F) $(SP_LIBd)

scrip.o:  scrip.f90
	$(FCOMP) $(FFLAGS) $(NETCDF_INCLUDE) -c scrip.f90

clean:
	rm -f *.o *.mod ${CMD} *.exe.*
