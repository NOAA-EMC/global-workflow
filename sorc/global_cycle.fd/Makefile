SHELL=	/bin/ksh
FC      =$(FCMP)
LIBS    =$(LIBSM)
OBJS	=num_parthds.o machine.o cycle.o sfcsub.o read_write_data.o

CMD     =global_cycle
$(CMD):	$(OBJS)
	${FC} ${DEBUG} $(FFLAGS) $(LDFLG) $(OBJS) $(LIBS) -o $(CMD)

num_parthds.o:	num_parthds.f90
	$(FC) $(FFLAGS) $(OMPFLAG) -c num_parthds.f90

machine.o:	machine.f90
	${FC} $(FFLAGS) -c machine.f90

read_write_data.o:	read_write_data.f90
	${FC} $(FFLAGS) -c read_write_data.f90

sfcsub.o: machine.o sfcsub.F
	$(FC)  $(FFLAGS) $(OMPFLAG) -c sfcsub.F

cycle.o: read_write_data.o cycle.f90
	$(FC)  $(FFLAGS) -c cycle.f90

install:
	-cp -p $(CMD) ../../exec/.
clean:
	-rm -f $(OBJS) *.mod $(CMD)
