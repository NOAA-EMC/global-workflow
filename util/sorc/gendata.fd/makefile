FC = ifort
FFLAGS =  -O -g -traceback -C -i8 -r8 -assume byterecl -convert big_endian -assume nobscc -assume noold_ldout_format

OBJ = gendata.o \
      addlab.o \
      afzoom.o \
      bdsanam.o \
      bdsmnam.o \
      chkfil.o \
      cldhgt.o \
      clouds.o \
      datafs.o \
      filter.o \
      forsfc.o \
      getdat.o \
      heafos.o \
      ijafos.o \
      lklndnam.o \
      outplt.o \
      redsfc.o \
      thnstn.o


LIBS = ${GPHCNTR_LIB8} ${GPHFONT_LIB8} ${GPH_LIB8} ${UTIL_LIB}  \
        ${IP_LIB8} ${SP_LIB8} ${W3EMC_LIB8} ${W3NCO_LIB8} ${BACIO_LIB8} ${BUFR_LIB8}

gendata: $(OBJ)
	$(FC) -z muldefs -o $@ $(FFLAGS) $(OBJ) $(LIBS)

clean:
	/bin/rm -f gendata *.o
