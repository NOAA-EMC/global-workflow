EXEC    =../../exec/nemsio_chgdate
FOPTS   =-O -FR -I$(NEMSIO_INC) -convert big_endian
LOPTS   = $(OPTS)
LIBS    =$(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd)
OBJS = nemsio_chgdate.o
SRCS = nemsio_chgdate.f
# *************************************************************************
all: ${OBJS}
$(OBJS): $(SRCS)
	$(FCMP)  $(FOPTS) $(LOPTS) $(SRCS) -o $(EXEC) $(LIBS)
