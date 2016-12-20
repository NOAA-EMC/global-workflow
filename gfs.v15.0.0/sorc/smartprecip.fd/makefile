SHELL=/bin/sh
#
###SRCS=SMARTPRECIP.f90
OBJS=SMARTPRECIP.o

FC =   ifort -convert big_endian -g -traceback	

#### LIBS = -L/usr/local/lib ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${IP_LIB4}   
LIBS = ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${IP_LIB4}

NCEP_LIB_DIR = 

FFLAGS = -O -fp-model strict -list
LDFLAGS =-FR -assume byterecl -check all -debug all -warn nounused 

CMD =	../../exec/smartprecip    
PROFLIB =	-lprof

all:		$(CMD)

$(OBJS): %.o: %.f90
	$(FC) -c $*.f90


$(CMD):		$(OBJS) 
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)
