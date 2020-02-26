SHELL   =/bin/ksh
EXEC    =../../exec/mkgfsnemsioctl
FOPTS   = -O -FR -I$(NEMSIO_INC)
LOPTS   = $(OPTS)
LIBS    = $(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd)
OBJS = mkgfsnemsioctl.o
SRCS = mkgfsnemsioctl.f
# *************************************************************************
all: $(SRCS)
	$(FCMP) $(FOPTS) $(LOPTS) ${SRCS} -o $(EXEC) $(LIBS)
