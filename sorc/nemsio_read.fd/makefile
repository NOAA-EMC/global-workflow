SHELL  =/bin/sh
EXEC   =../../exec/nemsio_read
FOPTS  = -O -FR -I$(NEMSIO_INC)
LOPTS  = $(OPTS)
LIBS   = $(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd)
OBJS = nemsio_read.o
SRCS = nemsio_read.f
# *************************************************************************
all: $(OBJS)

$(OBJS): $(SRCS)
	$(FCMP)  $(FOPTS) $(LOPTS) $(SRCS) -o $(EXEC) $(LIBS)

