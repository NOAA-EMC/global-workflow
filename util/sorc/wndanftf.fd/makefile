SHELL=/bin/sh
#
SRCS= graph_wndanftf.f       

OBJS=graph_wndanftf.o        

FC =    ifort 

XLIBS=/usr/lib64

LDFLAGS =  -L${NCARG_LIB} -L${XLIBS} \
           -lncarg -lncarg_gks -lncarg_c -lX11 -lXext -lcairo -lfontconfig -lpixman-1 \
           -lfreetype -lexpat -lpng -lz -lpthread -lbz2 -lXrender -lgfortran

LIBS=   ${IP_LIB4} ${SP_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} 

CMD =     wndanftf 
PROFLIB =       -lprof

FFLAGS = -O -g -traceback -C -qopenmp -assume byterecl -convert big_endian -assume nobscc -assume noold_ldout_format -warn usage

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:            $(CMD)
	
$(CMD):         $(OBJS)
	$(FC) -o $(@) $(OBJS) $(LIBS) $(LDFLAGS) $(FFLAGS)
clean:
	/bin/rm -f wndanftf *.o

