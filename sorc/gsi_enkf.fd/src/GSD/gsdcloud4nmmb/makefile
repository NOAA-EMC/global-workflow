SHELL=/bin/sh

LIB = ./libgsdcloud.a

include make.filelist

FFLAGS = -O3 -g -fp-model strict -convert big_endian -assume byterecl -implicitnone -traceback #   -I../../../include 
.SUFFIXES: .f90 .o

.f90.o:
##	$(RM) $@ $*.mod
	ifort $(FFLAGS) -c $<


all:	$(LIB)

$(LIB):	$(OBJ_FILES)
	$(AR) -ruv $(LIB) $(OBJ_FILES)

.f90.a:
	ifort -c $(FFLAGS) $<
#	ar -ruv $(AFLAGS) $@ $*.o
#	rm -f $*.o

.c.a:
	$(CC) -c $(CFLAGS) $<
#	ar -ruv  $(AFLAGS) $@ $*.o
#	rm -f $*.o

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include make.dependencies

clean:
	rm -f *.o *.mod $(LIB)

