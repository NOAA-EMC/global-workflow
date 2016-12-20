SHELL=	/bin/ksh
FC      =$(FCMP)
LIBS    =$(LIBSM)
OBJS	=num_parthds.o machine.o cycle.o sfcsub.o

CMD     =global_cycle
$(CMD):	$(OBJS)
	${FC} ${DEBUG} $(FFLAGS) $(LDFLG) $(OBJS) $(LIBS) -o $(CMD)

num_parthds.o:	num_parthds.f
	$(FC) $(FFLAGS) $(OMPFLAG) -c num_parthds.f

machine.o:	machine.f
	${FC} $(FFLAGS) -c machine.f

sfcsub.o:	sfcsub.f
	$(FC)  $(FFLAGS) $(OMPFLAG) -c sfcsub.f

cycle.o:	cycle.f
	$(FC)  $(FFLAGS) -c cycle.f

install:
	-cp -p $(CMD) ../../exec/.
clean:
	-rm -f $(OBJS) *.mod
