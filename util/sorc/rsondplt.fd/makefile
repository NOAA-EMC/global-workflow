SHELL=/bin/sh
#
#  makefile to make both rsondplt executables with
#  one make
SRCS= rsondplt.f
OBJS= rsondplt.o
FC =	ifort	
LDFLAGS =	
LIBS=   ${W3EMC_LIB8} ${W3NCO_LIB8} ${BACIO_LIB8} ${BUFR_LIB8}

CMD=  rsondplt

FFLAGS = -D_LITTLE_ENDIAN -g -i8 -r8 -assume byterecl -convert big_endian \
        -assume noold_ldout_format
#
all:		$(CMD) 

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	rm -f $(OBJS) *.mod
