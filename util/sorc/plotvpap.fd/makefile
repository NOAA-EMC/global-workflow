SHELL=/bin/sh
#
SRCS=   main.f afpltf.f afzoom.f aplgnd.f b4plotx.f   \
        ckparm.f dixie.f genafplt.f goesxy.f hgtdif.f  \
        ijbsirch.f interp.f koptn.f presort.f maplop.f   \
        rd_uupa.f rdafil.f rdcar1.f rdcar2.f sepcar.f setflg.f\
        thin_upa.f wibound.f cutstr.f gtnxtwrd.f tesrduupa.f \
        truijx.f asc2int.f sortem.f mksorwrd.f inidra.f mercxy.f \
        titles.f titlen.f idtitl.f titleo.f titlej.f lgnd1d.f readupa.f \
        moveid.f movobs.f pltdat.f setkeil.f rdcar3.f titlhc.f \
        readair.f rd_airc.f fillubin.f setaltit.f tstbnd.f 
OBJS=   main.o afpltf.o afzoom.o aplgnd.o b4plotx.o   \
        ckparm.o dixie.o genafplt.o goesxy.o hgtdif.o  \
        ijbsirch.o interp.o koptn.o presort.o  maplop.o \
        rd_uupa.o rdafil.o rdcar1.o rdcar2.o sepcar.o setflg.o\
        thin_upa.o wibound.o cutstr.o gtnxtwrd.o tesrduupa.o \
        truijx.o asc2int.o sortem.o mksorwrd.o inidra.o mercxy.o \
        titles.o titlen.o idtitl.o titleo.o titlej.o lgnd1d.o readupa.o \
        moveid.o movobs.o pltdat.o setkeil.o rdcar3.o titlhc.o \
        readair.o rd_airc.o fillubin.o setaltit.o tstbnd.o 

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =		ifort	
#LDFLAGS =       -lessl                   

LIBS = ${GPH_LIB8} ${GPHCNTR_LIB8} ${UTIL_LIB} \
       ${W3EMC_LIB8} ${W3NCO_LIB8} ${IP_LIB8} ${BACIO_LIB8} ${SP_LIB8}

CMD =		plotvpap            

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
FFLAGS =  -D_LITTLE_ENDIAN -g -i8 -r8 -convert big_endian -assume byterecl       

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
# $(CMD).prof:	$(OBJS)
#	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
