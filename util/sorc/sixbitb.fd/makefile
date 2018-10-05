#
### Author: Krishna Kumar                   01/21/2000
#
###
### CAVEATS: getbgnd.fake.f is the same version as getbgnd.f from the
###          /nwprod/gphlib90/gphcntr.source. The getbgnd.f in the 
###          graphics library compiled with the -qnosave option 
###          (mandatory for threaded versions)  had problems
###          while linking with this code. The program ran successfully
###          when linked without -qnosave option. The reasons are 
###          still to be investigated !!!
###          
SHELL=/bin/sh

SRCS=   ipak.f cntr.fake.f putlab.f AVTTLS.f BLOCKDATA001.f DOFRST.f \
        FAXSHD.f GETSXX.f GULFTL.f MERCIN.f NPOLUV.f PLTTLL.f \
        PLTWLL.f RNCNTL.f SLTCRD.f WNDIRT.f WNDRNH.f \
        main.f lputlab.f getbgnd.fake.f  

OBJS=   ipak.o cntr.fake.o putlab.o AVTTLS.o BLOCKDATA001.o DOFRST.o \
        FAXSHD.o GETSXX.o GULFTL.o MERCIN.o NPOLUV.o PLTTLL.o \
        PLTWLL.o RNCNTL.o SLTCRD.o WNDIRT.o WNDRNH.o \
        main.o lputlab.o getbgnd.fake.o 

FC =	ifort

LIBS = ${GPHCNTR_LIB8} ${GPHFONT_LIB8} ${GPH_LIB8} ${UTIL_LIB} \
       ${IP_LIB8} ${SP_LIB8} ${W3EMC_LIB8} ${W3NCO_LIB8} ${BACIO_LIB8}

CMD =		sixbitb  

FFLAGS = -D_LITTLE_ENDIAN -g -i8 -r8 -assume byterecl -convert big_endian \
        -assume noold_ldout_format

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
# $(CMD).prof:    $(OBJS)
#	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS) 
#

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
