# Modified BSM for WCOSS build 1/30/2013
SHELL=/bin/sh

SRCS=   webtitle.f                                              
OBJS=   webtitle.o                                              
# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =	ifort 

LIBS=   ${W3NCO_LIB4}

CMD =	webtitle       
FFLAGS = 
#FFLAGS = -debug 

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all: $(CMD)

$(CMD):	$(OBJS)
	$(FC) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD)

void:	clobber
	-rm -f $(SRCS) makefile
