SHELL	=/bin/ksh
INCMOD	=$(INCS)             
LIBS    =$(LIBSM)

SRCM	=chgres.f
SRCS	=

OBJS	=num_parthds.o \
         nstio_module.o \
         nsst_chgres.o \
         funcphys.o \
         physcons.o \
         machine_8.o \
         surface_chgres.o \
         sfcsub.o \
         chgres_utils.o \
         nrlmsise00_sub.o \
         read_write.o \
         chgres.o

FC	=$(FCMP)
FC90    =$(FCMP95)
LDR	=$(FCMP)

FFLAGS	=$(FFLAGSM)
OMPFLAG=$(OMPFLAGM)
LDFLAGS	=$(LDFLAGSM)

CMD	=global_chgres
$(CMD):	$(OBJS)
	$(LDR) $(LDFLAGS) $(OBJS) $(LIBS) -o $(CMD)

read_write.o:  nstio_module.o surface_chgres.o read_write.f90
	$(FC90) $(FFLAGS) $(INCMOD) $(OMPFLAG) -c read_write.f90
num_parthds.o:	num_parthds.f90
	$(FC90) $(FFLAGS) $(OMPFLAG) -c num_parthds.f90
nstio_module.o : nstio_module.f90
	$(FC90) $(FFLAGS) $(RECURS) -c nstio_module.f90
nsst_chgres.o : nsst_chgres.f90
	$(FC90) $(FFLAGS) $(RECURS) -c nsst_chgres.f90
chgres_utils.o:	physcons.o funcphys.o chgres_utils.f90
	$(FC90) $(FFLAGS) $(RECURS) $(INCMOD) -c chgres_utils.f90
nrlmsise00_sub.o:	nrlmsise00_sub.f90
	$(FC90) $(FFLAGS)  $(RECURS) -c nrlmsise00_sub.f90
funcphys.o:     funcphys.f90 physcons.o machine_8.o
	$(FC90) $(FFLAGS) $(RECURS) -c funcphys.f90
physcons.o:     physcons.f90 machine_8.o
	$(FC90) $(FFLAGS) $(RECURS) -c physcons.f90
machine_8.o:      machine_8.f90
	$(FC90) $(FFLAGS) $(RECURS) -c machine_8.f90
sfcsub.o:      machine_8.o sfcsub.F
	$(FC) $(FFLAGS) $(OMPFLAG) $(INCMOD) -c sfcsub.F
surface_chgres.o: sfcsub.o machine_8.o surface_chgres.f90
	$(FC90) $(FFLAGS) $(INCMOD) $(RECURS) -c surface_chgres.f90
chgres.o: surface_chgres.o funcphys.o chgres.f90
	$(FC90) $(FFLAGS) $(OMPFLAG) $(INCMOD) -c chgres.f90
clean:
	-rm -f $(OBJS) *.mod
install:
	-cp $(CMD) ../../exec/.
clobber:        clean
	-rm -f $(CMD)
