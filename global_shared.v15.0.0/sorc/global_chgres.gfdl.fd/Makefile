SHELL	=/bin/ksh
INCMOD1	=$(SIGIO_INC4)              
INCMOD2	=$(SFCIO_INC4)             
INCMOD3	=$(LANDSFCUTIL_INCd)             
INCMOD4	=$(GFSIO_INC4)

SRCM	=chgres.f
SRCS	=
LIBS    =-L$(GFSIO_DIR) -l$(GFSIO_LIB4) -L$(SIGIO_DIR) -l$(SIGIO_LIB4) -L$(SFCIO_DIR) -l$(SFCIO_LIB4) -L$(BACIO_DIR) -l$(BACIO_LIB4) -L$(LANDSFCUTIL_DIR) -l$(LANDSFCUTIL_LIBd) -L$(IP_DIR) -l$(IP_LIBd) -L$(SP_DIR) -l$(SP_LIBd) -L$(W3EMC_DIR) -l$(W3EMC_LIBd) -L$(W3NCO_DIR) -l$(W3NCO_LIBd) -L$(NEMSIO_DIR) -l$(NEMSIO_LIB)
OBJS	=num_parthds.o nstio_module.o nsst_chgres.o gfsio_rst.o funcphys.o physcons.o machine_8.o surface_chgres.o sfcsub.o vintg_idea.o nrlmsise00_sub.o netcdf_io.o chgres.o
FC	=$(FCMP)
FC90    =$(FCMP95)
LDR	=$(FCMP)

FFLAGS	=$(FFLAGSM) -g -traceback #-check bounds -fpe0 -ftrapuv
FFLAGS2	=$(FFLAGS2M)
OMPFLAG=$(OMPFLAGM)
LDFLAGS	=$(LDFLAGSM)

CMD	=global_chgres
$(CMD):	$(OBJS)
	$(LDR) $(LDFLAGS) $(OBJS) $(LIBS) -o $(CMD)

netcdf_io.o:  netcdf_io.F90
	$(FC) $(FFLAGS) $(OMPFLAG) -c netcdf_io.F90
num_parthds.o:	num_parthds.f
	$(FC) $(FFLAGS) $(OMPFLAG) -c num_parthds.f
nstio_module.o : nstio_module.f
	$(FC90) $(FFLAGS2) $(RECURS) -c nstio_module.f
nsst_chgres.o : nsst_chgres.f
	$(FC90) $(FFLAGS2) $(RECURS) -c nsst_chgres.f
vintg_idea.o:	vintg_idea.f
	$(FC) $(FFLAGS) $(RECURS) -c vintg_idea.f
nrlmsise00_sub.o:	nrlmsise00_sub.f
	$(FC) $(FFLAGS)  $(RECURS) -c nrlmsise00_sub.f
gfsio_rst.o:      gfsio_rst.f
	$(FC90) $(FFLAGS2) $(RECURS) -c gfsio_rst.f
funcphys.o:     funcphys.f physcons.o machine_8.o
	$(FC90) $(FFLAGS2) $(RECURS) -c funcphys.f
physcons.o:     physcons.f machine_8.o
	$(FC90) $(FFLAGS2) $(RECURS) -c physcons.f
machine_8.o:      machine_8.f
	$(FC90) $(FFLAGS2) $(RECURS) -c machine_8.f
sfcsub.o:      sfcsub.f
	$(FC) $(FFLAGS) $(OMPFLAG) -c sfcsub.f
surface_chgres.o: sfcsub.o
	$(FC90) $(FFLAGS2) -I $(INCMOD3) $(RECURS) -c surface_chgres.f
chgres.o: chgres.f
	$(FC) $(FFLAGS) $(OMPFLAG) -I $(INCMOD1) -I $(INCMOD2) -I $(INCMOD3) -I $(INCMOD4) -c chgres.f
clean:
	-rm -f $(OBJS) *.mod $(CMD)
install:
	-cp $(CMD) ../../exec/.
clobber:        clean
	-rm -f $(CMD) ../exec/$(CMD)
