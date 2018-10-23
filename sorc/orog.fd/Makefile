SHELL	=/bin/ksh
LIBS    =$(LIBSM)

SRCM	=mtnlm7_oclsm.f
SRCS	=

OBJS	=mtnlm7_oclsm.o netcdf_io.o
FC	=$(FCMP)
FC90    =$(FCMP95)
LDR	=$(FCMP)

FFLAGS	=$(FFLAGSM)
OMPFLAG=$(OMPFLAGM)
LDFLAGS	=$(LDFLAGSM)

CMD	=orog.x
$(CMD):	$(OBJS)
	$(LDR) $(LDFLAGS) $(OBJS) $(LIBS) -o $(CMD)

netcdf_io.o:  netcdf_io.F90
	$(FC) $(FFLAGS) $(OMPFLAG) -c netcdf_io.F90
mtnlm7_oclsm.o:	mtnlm7_oclsm.f
	$(FC) $(FFLAGS) $(OMPFLAG) -c mtnlm7_oclsm.f
clean:
	-rm -f $(OBJS) *.mod
install:
	-cp $(CMD) ../../exec/.
clobber:        clean
	-rm -f $(CMD)
