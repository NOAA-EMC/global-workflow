SHELL=		/bin/sh
#LIBS=		-L/nwprod/lib -lw3nco_v2.0.5_8
##LIBS=		-L/contrib/nceplibs/nwprod/lib -lw3nco_v2.0.5_8
FC=		ifort
#DEBUG =		-ftrapuv -check all -check noarg_temp_created -fp-stack-check -fstack-protector
## if '-check all' enabled, include '-check noarg_temp_created' to avoid warning msgs indicating 
##   slight performance hit due to chosen method of passing array arguments to w3difdat  
FFLAGS=		-O3 -g -traceback -r8 -i8 -assume byterecl -assume noold_ldout_format $(DEBUG)
LDFLAGS=	
SRCS=		qctropcy.f
OBJS=		qctropcy.o
CMD=		syndat_qctropcy

all:		$(CMD)

$(CMD):		$(OBJS)
		$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS_SYN_QCT)

clean:
		-rm -f $(OBJS)

install:
		mv $(CMD) ../../exec/$(CMD)
