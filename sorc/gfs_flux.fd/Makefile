SHELL  =  /bin/sh
LIBS   =  $(LIBSMOD)

#FFLAGS=	-O -qmaxmem=-1 -l essl
FFLAGS =  $(FFLAGSM)

LDFLAGS=  $(FFLAGS)

SRCS=	extflx.f fillken.f kengrd.f kenmain.f kentest.f qpfint.f
OBJS=	extflx.o fillken.o kengrd.o kenmain.o kentest.o qpfint.o

FC=	$(FCMP)

CMD=    ../../exec/gfs_flux        

all:	$(CMD)

$(CMD):	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)
clean:  
	-rm -f $(OBJS) 
