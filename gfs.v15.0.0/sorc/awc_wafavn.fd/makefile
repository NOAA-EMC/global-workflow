# makefile for waf programs
#
#FC = ifort
#INC = /nwprod/lib/incmod/g2_4
#LIBS = -L /nwprod/lib -lg2_4 -lw3emc_4 -lw3nco_4 -lbacio_4 -lip_4 -lsp_4 -ljasper -lpng -lz
#FFLAGS = -FR -I$(INC) -g -O2 -convert big_endian -assume noold_ldout_format
FFLAGS = ${FFLAGSawc}
MKDEP = sfmakedepend.pl
DEPFILE = makedepend.out

.SUFFIXES:
%.o : %.f90
	$(FC) $(FFLAGS) -c $<

%.o : %.f
	$(FC) $(FFLAGS) -c $<

#------------------------------------------------------------------------------

OBJS = waf_main.o \
	waf_grib2.o \
	waf_grib1.o \
	waf_gfip.o \
	waf_calc.o \
	waf_cat.o \
	waf_cb.o \
	waf_config.o \
	waf_filter.o \
	waf_glob.o \
	waf_icng.o \
	waf_phys.o \
	waf_tcld.o \
	cfgini.o \
	funcphys.o \
	fuzzy.o \
	getoptions.o \
	kinds.o \
	physcons.o \
	tokenize.o

CMD = awc_wafavn

#SRC_ALL = $(patsubst %.o,%.f90, $(OBJS))
SRC_ALL = waf_main.f90 \
        waf_grib2.f90 \
	waf_grib1.f90 \
	waf_gfip.f90 \
        waf_calc.f90 \
        waf_cat.f90 \
        waf_cb.f90 \
        waf_config.f90 \
        waf_filter.f90 \
        waf_glob.f90 \
	waf_icng.f90 \
        waf_phys.f90 \
        waf_tcld.f90 \
        cfgini.f90 \
        funcphys.f90 \
        fuzzy.f90 \
        getoptions.f90 \
        kinds.f90 \
        physcons.f90 \
        tokenize.f90
#------------------------------------------------------------------------------
all:	$(CMD)

$(CMD):	$(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LIBS)

depend:
	rm -f $(DEPFILE)
	$(MKDEP) -f $(DEPFILE) $(SRC_ALL)

clean:
	rm -f *.o *.mod

install:
	mv $(CMD) ../../exec/.
#-----------------------------------------------------------------------------
# dependency file
-include $(DEPFILE)
# DO NOT DELETE THIS LINE - used by make depend
