# *****************************************************************
#  makefile.bcoef
#
#  Make the radmon_bcoef executable. 
#
# *****************************************************************

BINDIR = ../../exec

LIBS   = $(W3NCO_LIB4)

OBJS   = kinds.o read_diag.o bcoef.o create_ctl_bcoef.o

#
# *****************************************************************
#

CMD = radmon_bcoef

all:  $(CMD)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f radmon_bcoef 

check_prereqs:
	$(CHECK_LIBS) $(LIBS)

install:
	cp -f $(CMD) $(BINDIR)/

debug:  FFLAGS = $(D_FFLAGS)
debug:  $(CMD)

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

read_diag.o : read_diag.f90
	$(CF) $(FFLAGS) -c $(*).f90

bcoef.o : bcoef.f90
	$(CF) $(FFLAGS) -c $(*).f90

create_ctl_bcoef.o : create_ctl_bcoef.f90
	$(CF) $(FFLAGS) -c $(*).f90

kinds.o : kinds.F90
	$(CF) $(FFLAGS) -c $(*).F90
