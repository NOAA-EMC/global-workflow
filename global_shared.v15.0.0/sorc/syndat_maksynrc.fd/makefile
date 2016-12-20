SHELL=		/bin/sh
#LIBS=		-L/nwprod/lib -lw3nco_v2.0.5_4 -lbacio_v2.0.1_4 
##LIBS_SYN_MAK=		-L/contrib/nceplibs/nwprod/lib -lw3nco_v2.0.5_4 -lbacio_v2.0.1_4 
FC=		ifort   
#DEBUG =          -ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector
FFLAGS=		-O3 -g -traceback -assume noold_ldout_format $(DEBUG)
LDFLAGS=	
SRCS=		maksynrc.f
OBJS=		maksynrc.o
CMD=		syndat_maksynrc

all:		$(CMD)

$(CMD):		$(OBJS)
		$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS_SYN_MAK)

clean:	
		-rm -f $(OBJS)

install:	
		mv $(CMD) ../../exec/$(CMD)
