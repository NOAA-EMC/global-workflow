SHELL=/bin/sh
#

SRCS=   upaupa.f forupa.f redupa.f redhgt.f fndhgt.f brdhgt.f \
        conaft.f contrl.f dosbyt.f foraft.f conwnd.f forwnd.f \
        consat.f forsat.f

OBJS=   upaupa.o forupa.o redupa.o redhgt.o fndhgt.o brdhgt.o \
        conaft.o contrl.o dosbyt.o foraft.o conwnd.o forwnd.o \
        consat.o forsat.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =		 ifort

LIBS = ${GPHCNTR_LIB8} ${GPHFONT_LIB8} ${GPH_LIB8} ${UTIL_LIB}  \
       ${IP_LIB8} ${SP_LIB8} ${W3EMC_LIB8} ${W3NCO_LIB8} ${BACIO_LIB8} ${BUFR_LIB8}

CMD =		upaprep        

FFLAGS = -D_LITTLE_ENDIAN -g -i8 -r8 -assume byterecl -convert big_endian \
        -assume noold_ldout_format

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
# $(CMD).prof:	$(OBJS)
#	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
