SHELL=/bin/sh
#
SRCS= ras2bit.f 	

OBJS= ras2bit.o 

FC =	ifort     
LFLAGS =
LIBS =    ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4}

CMD =	 ras2bit

FFLAGS = -O -assume byterecl 

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	echo 'HEAP=2500000+5000000' >HEAP.RAS2BITY
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)

