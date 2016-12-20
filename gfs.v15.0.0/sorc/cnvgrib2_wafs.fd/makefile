SHELL=/bin/sh
#
SRCS=	gds2gdt.f pds2pdt.f pds2pdtens.f cnvgrib2_wafs.f cnvwafs12.f cnvwafs22.f \
        cnvwafs21.f gdt2gds.f makepds.f putgbexn.f makepdsens.f setbit.f
OBJS=	gds2gdt.o pds2pdt.o pds2pdtens.o cnvgrib2_wafs.o cnvwafs12.o cnvwafs22.o \
        cnvwafs21.o gdt2gds.o makepds.o putgbexn.o makepdsens.o setbit.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =		ifort

#INC= /nwprod/lib/incmod/g2_4

#LIBS= -L/nwprod/lib -lg2_4 -lw3nco_4 -lbacio_4 \
#        -ljasper -lpng -lz

CMD =		cnvgrib2_wafs
PROFLIB =	-lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
FFLAGS = ${FFLAGScnv}
#FFLAGS =        -O3 -g -I $(INC)
#FFLAGS =	 -F
#FFLAGS =	 -Wf"-ez"

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile

install:
	mv $(CMD) ../../exec/.
