SHELL=	/bin/ksh
FC      =$(FCMP)
LIBS    =$(LIBSM)
OBJS	=num_parthds.o machine.o cycle.o sfcsub.o read_write_data.o

CMD     =global_cycle
$(CMD):	$(OBJS)
	${FC} ${DEBUG} $(FFLAGS) $(LDFLG) $(OBJS) $(LIBS) -o $(CMD)

num_parthds.o:	num_parthds.f
	$(FC) $(FFLAGS) $(OMPFLAG) -c num_parthds.f

machine.o:	machine.f
	${FC} $(FFLAGS) -c machine.f

read_write_data.o:	read_write_data.f90
	${FC} $(FFLAGS) -c read_write_data.f90

sfcsub.o:	sfcsub.f
	$(FC)  $(FFLAGS) $(OMPFLAG) -c sfcsub.f

cycle.o: read_write_data.o cycle.f
	$(FC)  $(FFLAGS) -c cycle.f

install:
	-cp -p $(CMD) ../../exec/.
clean:
	-rm -f $(OBJS) *.mod
