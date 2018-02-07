# *****************************************************************
#  makefile
#
#  Make the radmon_time executable.
#
# *****************************************************************

BINDIR = ../../exec

LIBS   = $(W3NCO_LIB4)

OBJS = kinds.o read_diag.o bad_obs.o bad_penalty.o bad_chan.o valid.o \
	time.o create_ctl_time.o

#
# *****************************************************************
#

CMD = radmon_time

all:  $(CMD)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f radmon_time

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

bad_obs.o : bad_obs.f90
	$(CF) $(FFLAGS) -c $(*).f90

bad_penalty.o : bad_penalty.f90
	$(CF) $(FFLAGS) -c $(*).f90

bad_chan.o : bad_chan.f90
	$(CF) $(FFLAGS) -c $(*).f90

valid.o : valid.f90
	$(CF) $(FFLAGS) -c $(*).f90

time.o : time.f90
	$(CF) $(FFLAGS) -c $(*).f90

create_ctl_time.o : create_ctl_time.f90
	$(CF) $(FFLAGS) -c $(*).f90

kinds.o : kinds.F90
	$(CF) $(FFLAGS) -c $(*).F90
