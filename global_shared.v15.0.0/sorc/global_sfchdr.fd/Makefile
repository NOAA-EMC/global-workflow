SHELL   =/bin/sh
INCMOD  =$(SFCIO_INC4)               
LIBS    =$(LIBSM)
FC      =$(FCMP)

SRCM    =sfchdr.f
OBJS    =

CMD     =global_sfchdr
$(CMD): $(SRCM) $(OBJS)
	$(FC) -I $(INCMOD) $(FFLAGS) $(SRCM) $(OBJS) $(LIBS) -o $(CMD)
install:
	-cp -p $(CMD) ../../exec/.
clean:
	-rm -f $(OBJS) $(CMD)
