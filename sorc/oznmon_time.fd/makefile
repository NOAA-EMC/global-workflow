# *****************************************************************
#  makefile.time
#
#  Make the OzMon time executable 
#
#  To build enter these commands:
#    > module use -a ../nwprod/ozmon_shared.v2.0.0/modulefiles/cray|theia|wcoss
#    > module load OzMonBuild
#    > make 
#    > make install
#    > module unload OzMonBuild
#
#  Notes:
#    1) Makefile.conf.* have been replaced with the build modules
#    2) make options supported include:
#       a) make all (default if "all" is not specified)
#       b) clean
#       c) debug
#       d) install
# *****************************************************************

SHELL=/bin/sh

BINDIR = ../../exec

LIBS   = $(W3NCO_LIB4)

SRCS = kinds.f90 read_diag.f90 time.f90 create_ctl_time.f90 avgsdv.f90 \
	update_ctl_time.f90

OBJS = kinds.o read_diag.o time.o create_ctl_time.o avgsdv.o \
	update_ctl_time.o

#
# *****************************************************************
#

CMD = oznmon_time.x

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

time.o : time.f90
	$(CF) $(FFLAGS) -c $(*).f90

create_ctl_time.o : create_ctl_time.f90
	$(CF) $(FFLAGS) -c $(*).f90

avgsdv.o : avgsdv.f90
	$(CF) $(FFLAGS) -c $(*).f90

update_ctl_time.o : update_ctl_time.f90
	$(CF) $(FFLAGS) -c $(*).f90
