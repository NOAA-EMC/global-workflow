SHELL	=/bin/sh
INCMOD =${INCS}

SRCM	=reg2grb2.f
OBJS	=regdiag.o
FC	=ifort
FC90    =ifort

FFLAGS	= ${FFLAGSM} 
FFLAGS2 = ${FFLAGSM2}

NCDFLIB = ${NCDF}
LIBS	= ${LIBSM} ${NCDFLIB}

CMD	= reg2grb2.x
$(CMD):	$(SRCM) $(OBJS)
	$(FCMP) $(FFLAGS) -o $(CMD) $(LDFLAGS) $(SRCM) $(OBJS) $(LIBS) ${MOD2} ${LIB2}

regdiag.o:regdiag.f
	$(FCMP) $(FFLAGS) -I $(INCMOD) -c regdiag.f ${MOD2} ${LIB2}

clean:
	-rm -f $(OBJS) *.mod

install:
	mv $(CMD) ../../exec/$(CMD)
