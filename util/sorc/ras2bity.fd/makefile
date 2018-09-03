SHELL=/bin/sh
#
SRCS= ras2bity.f 	

OBJS= ras2bity.o 

FC =	ifort     
LFLAGS =
LIBS =    ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4}

CMD =	 ras2bity     
PROFLIB =	-lprof

FFLAGS = -O -g -traceback -C -assume byterecl -convert big_endian -assume nobscc -assume noold_ldout_format -warn usage 

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	echo 'HEAP=2500000+5000000' >HEAP.RAS2BITY
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)
clean:
	-rm -f ras2bity *.o

