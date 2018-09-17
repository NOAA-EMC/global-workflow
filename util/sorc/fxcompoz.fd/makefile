CC = icc
CFLAGS = -DUNDERSCORE -O -g -traceback

FC = ifort
FFLAGS = -C -D_LITTLE_ENDIAN -fixed -O -g -traceback -i8 -r8 -assume byterecl -convert big_endian -assume nobscc -assume noold_ldout_format

OBJ = close99.o \
      fxcompozlib.o \
      open99.o \
      rd1byt99.o \
      reposi99.o \
      rewind99.o \
      wheren99.o

LIBS =  ${GPH_LIB8} ${UTIL_LIB} \
        ${IP_LIB8} ${SP_LIB8} ${W3EMC_LIB8} ${W3NCO_LIB8} ${BACIO_LIB8}

fxcompoz: fxcompoz.o $(OBJ)
	$(FC) -o $@ $(FFLAGS) fxcompoz.o $(OBJ) $(LIBS)

clean:
	/bin/rm -f *.o

.c.o:
	$(CC) -c $(CFLAGS) $<

.F.o:
	$(FC) -c $(FFLAGS) $<

.f.o:
	$(FC) -c $(FFLAGS) $<
