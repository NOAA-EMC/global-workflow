# *****************************************************************
#  makefile.bcor
#
#  Make the radmon_bcor executable.
#
# *****************************************************************

BINDIR = ../../exec

LIBS   = $(W3NCO_LIB4)

OBJS =  kinds.o read_diag.o bcor.o avgsdv.o create_ctl_bcor.o

#
# *****************************************************************
#

CMD = radmon_bcor

all:  $(CMD)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f radmon_bcor

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

bcor.o : bcor.f90
	$(CF) $(FFLAGS) -c $(*).f90

avgsdv.o : avgsdv.f90
	$(CF) $(FFLAGS) -c $(*).f90

create_ctl_bcor.o : create_ctl_bcor.f90
	$(CF) $(FFLAGS) -c $(*).f90

kinds.o : kinds.F90
	$(CF) $(FFLAGS) -c $(*).F90
