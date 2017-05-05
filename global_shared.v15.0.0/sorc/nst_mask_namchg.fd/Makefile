SHELL	=/bin/ksh
INCMOD	=$(INCS)             
LIBS    =$(LIBSM)

SRCM	=nst_mask_namchg.f90
SRCS	=

OBJS	=nst_mask_namchg.o 

FC	=$(FCMP)
FC90    =$(FCMP95)
LDR	=$(FCMP)

FFLAGS	=$(FFLAGSM)
OMPFLAG=$(OMPFLAGM)
LDFLAGS	=$(LDFLAGSM)

CMD	=nst_mask_namchg
$(CMD):	$(OBJS)
	$(LDR) $(LDFLAGS) $(OBJS) $(LIBS) -o $(CMD)

nst_mask_namchg.o: nst_mask_namchg.o nst_mask_namchg.f90
	$(FC90) $(FFLAGS) $(OMPFLAG) $(INCMOD) -c nst_mask_namchg.f90

clean:
	-rm -f $(OBJS) *.mod $(CMD)
install:
	-cp $(CMD) ../../exec/.
clobber:        clean
	-rm -f $(CMD)
