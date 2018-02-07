# *****************************************************************
#  makefile.angle_bias
#
#  Make the radmon_angle executable. 
#
# *****************************************************************

BINDIR = ../../exec

LIBS   = $(W3NCO_LIB4)

OBJS   = kinds.o read_diag.o angle_bias.o create_ctl_angle.o 

#
# *****************************************************************
#

CMD = radmon_angle

all:  $(CMD)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f $(CMD)

check_prereqs:
	$(CHECK_LIBS) $(LIBS)

install:
	cp -f $(CMD) $(BINDIR)/

debug:	FFLAGS = $(D_FFLAGS) 
debug:	$(CMD)

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)


kinds.o : kinds.F90
	$(CF) $(FFLAGS) -c $(*).F90

read_diag.o : read_diag.f90
	$(CF) $(FFLAGS) -c $(*).f90

angle_bias.o : angle_bias.f90
	$(CF) $(FFLAGS) -c $(*).f90

create_ctl_angle.o : create_ctl_angle.f90
	$(CF) $(FFLAGS) -c $(*).f90

