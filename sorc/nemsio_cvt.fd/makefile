SHELL  =/bin/sh
EXEC   =../../exec/nemsio_cvt
FOPTS  = -O -FR -I$(NEMSIO_INC)
LOPTS  = $(OPTS)
LIBS   = $(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd)
OBJS   = nemsio_cvt
SRCS   = nemsio_cvt.f
# *************************************************************************
 all: $(OBJS)

 $(OBJS): $(SRCS)
	  $(FCMP)  $(FOPTS) $(LOPTS) $(SRCS) -o $(EXEC) $(LIBS)
