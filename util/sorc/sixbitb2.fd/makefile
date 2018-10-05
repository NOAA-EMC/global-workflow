SHELL=/bin/sh
#
SRCS=   sta.f ipak.f cntr.fake.f AVTTLS.f \
        BLOCKDATA001.f DOFRST.f FAXSHD.f \
        GETSXX.f GULFTL.f MERCIN.f NPOLUV.f PLTTLL.f \
        PLTWLL.f RNCNTL.f SLTCRD.f WNDIRT.f WNDRNH.f \
        main.f lputlab.f wndbrk.f box.f fonts.f fonts2.f \
        getbgnd.fake.f putlab.f reblkfx4.F 
 
OBJS=   sta.o ipak.o cntr.fake.o AVTTLS.o \
        BLOCKDATA001.o DOFRST.o FAXSHD.o \
        GETSXX.o GULFTL.o MERCIN.o NPOLUV.o PLTTLL.o \
        PLTWLL.o RNCNTL.o SLTCRD.o WNDIRT.o WNDRNH.o  \
        main.o lputlab.o wndbrk.o box.o fonts.o fonts2.o \
        getbgnd.fake.o putlab.o reblkfx4.o 

FC =            ifort 
LDFLAGS =        

LIBS = ${GPHCNTR_LIB8} ${GPHFONT_LIB8} ${GPH_LIB8} ${UTIL_LIB}  \
       ${IP_LIB8} ${SP_LIB8} ${W3EMC_LIB8} ${W3NCO_LIB8} ${BACIO_LIB8}

CMD =      sixbitb2
PROFLIB =       -lprof

#
FFLAGS = -g -traceback -C -D_LITTLE_ENDIAN -i8 -r8 -assume byterecl -convert big_endian -assume nobscc -assume noold_ldout_format

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
