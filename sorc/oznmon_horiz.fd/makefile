# *****************************************************************
#  makefile.horiz
#
#  Make the OzMon horiz executable
#
#  To build enter these commands:
#    > module use -a ../nwprod/ozmon_shared.v2.0.0/modulefiles/cray|theia|wcoss
#    > module load OzMonBuild
#    > make -f makefile
#    > make -f makefile install
#    > module unload OzMonBuild
#
#  Notes:
#    1) Makefile.conf.* have been replaced with the build modules
#    2) make options supported include:
#       a) make all (default if all is not specified)
#       b) clean
#       c) debug
#       d) install
# *****************************************************************
SHELL=/bin/sh

BINDIR = ../../exec

LIBS   = $(W3NCO_LIB4)

SRCS = kinds.f90 read_diag.f90 horiz.f90 create_ctl_horiz.f90 \
	update_ctl_horiz.f90

OBJS = kinds.o read_diag.o horiz.o create_ctl_horiz.o \
	update_ctl_horiz.o

#
# *****************************************************************
#

CMD = oznmon_horiz.x

all:  $(CMD)

clean:
	rm -f *.x 
	rm -f *.o 
	rm -f *.mod 

install:
	cp -f $(CMD) $(BINDIR)/

debug:  FFLAGS = $(D_FFLAGS)
debug:  $(CMD)

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

kinds.o : kinds.f90
	$(CF) $(FFLAGS) -c $(*).f90

read_diag.o : read_diag.f90
	$(CF) $(FFLAGS) -c $(*).f90

horiz.o : horiz.f90
	$(CF) $(FFLAGS) -c $(*).f90

create_ctl_horiz.o : create_ctl_horiz.f90
	$(CF) $(FFLAGS) -c $(*).f90

update_ctl_horiz.o : update_ctl_horiz.f90
	$(CF) $(FFLAGS) -c $(*).f90

