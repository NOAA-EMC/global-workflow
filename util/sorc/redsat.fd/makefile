SHELL=/bin/sh
#
SRCS= redsat.f	

OBJS= redsat.o	

FC =	ifort  
LDFLAGS =
LIBS=   ${W3EMC_LIB8} ${W3NCO_LIB8} ${BACIO_LIB8} ${BUFR_LIB8}

CMD =	 redsat
PROFLIB =	-lprof

FFLAGS = -O -g -traceback -C -i8 -r8 -assume byterecl -convert big_endian -assume nobscc -assume noold_ldout_format   

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	echo 'HEAP=2500000+5000000' >HEAP.RAS2BITY
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	rm -f $(OBJS) *.mod
