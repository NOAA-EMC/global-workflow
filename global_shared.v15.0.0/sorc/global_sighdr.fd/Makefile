SHELL   =/bin/sh
INCMOD  =$(SIGIO_INC4)               
FC      =$(FCMP)
LIBS    =$(LIBSM)

SRCM    =sighdr.f
OBJS    =

CMD     =global_sighdr
$(CMD): $(SRCM) $(OBJS)
	$(FC) -I $(INCMOD) $(FFLAGS) $(SRCM) $(OBJS) $(LIBS) -o $(CMD)
install:
	-cp -p $(CMD) ../../exec/.
clean:
	-rm -f $(OBJS) $(CMD)
