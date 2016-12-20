SHELL = /bin/sh

################################################################################
# 
#     Makefile for NCEP Post
#
#     Use:
#     make         -  build the executable
#     make clean   -  start with a clean slate
#
#################################################################################
#
# Define the name of the executable
#
TARGET = unipost.exe

#
# build configuration determined before compile
include ../../configure.upp

#
# directories for shared resources
LOCALINC    = -I$(INCMOD) -I$(INCMOD)/crtm2
NCDFINC     = -I$(NETCDFPATH)/include
WRFINC      = -I$(WRF_MODS)
GRIB2INC    = -I$(GRIB2SUPT_INC)

LLIBDIR     = -L$(LIBDIR)
GRIB2LIBS   = -lg2 -lg2tmpl -lxmlparse $(GRIB2SUPT_LIB)
UPPLIBS     = -lnemsio -lsigio -lsfcio -lgfsio -lsp -lw3nco -lw3emc -lbacio -lCRTM $(SERIAL_MPI_LIB)
WRFEXTLIBS  = $(WRF_LIB) $(WRF_LIB_EXTRA) $(WRF_LIB2)
NCDFLIBS    = -L$(NETCDFPATH)/lib $(NETCDFLIBS)

LIBS        = $(LLIBDIR) $(UPPLIBS) $(GRIB2LIBS) $(WRFEXTLIBS) $(NCDFLIBS)

MODULES     = $(WRF_MODS)

#
# Compilation / Link Flag Configuration
EXTRA_CPPFLAGS = 
EXTRA_FFLAGS   = -c $(LOCALINC) $(NETCDFINC)
#EXTRA_LDFLAGS  = $(LIBS) -Wl,-Map=lm
EXTRA_LDFLAGS  = $(LIBS) 

#
# -----------
# Threaded object files
# -----------
OBJS_FT = wrf_io_flags.o getVariable.o getIVariable.o  \
          getIVariableN.o kinds_mod.o machine.o physcons.o \
          count_recs_wrf_binary_file.o \
          inventory_wrf_binary_file.o next_buf.o retrieve_index.o ZENSUN.o \
          CLDFRAC_ZHAO.o GFSPOST.o \
          GETGBANDSCATTER.o

# -----------
# Non-threaded object files
# -----------
OBJXML =  post_t.o

OBJS_F =  VRBLS2D_mod.o VRBLS3D_mod.o VRBLS4D_mod.o MASKS_mod.o PMICRPH.o SOIL_mod.o CMASSI.o \
          CTLBLK.o GRIDSPEC.o \
          LOOKUP.o PARAMR.o RHGRD.o RQSTFLD.o cuparm.o params.o svptbl.o get_postfilename.o grib2_module.o \
          SET_LVLSXML.o FILL_PSETFLD.o BNDLYR.o  BOUND.o  CALCAPE.o \
          CALDWP.o  CALDRG.o CALHEL.o  CALLCL.o  CALMCVG.o CALPOT.o  CALPW.o CALRH.o  CALRCH.o \
          CALRH_GSD.o CALSTRM.o CALTAU.o CALTHTE.o CALVIS.o CALVIS_GSD.o CALVOR.o CALWXT.o $(LINUX_OBJ)\
          CALWXT_RAMER.o CALWXT_BOURG.o CALWXT_REVISED.o CALRH_PW.o CALWXT_EXPLICIT.o \
          CALWXT_DOMINANT.o CLDRAD.o \
          CLMAX.o COLLECT.o  COLLECT_LOC.o DEWPOINT.o FDLVL.o  FGAMMA.o FIXED.o  FRZLVL.o  FRZLVL2.o \
          GET_BITS.o  GRIBIT.o INITPOST.o LFMFLD.o  MAPSSLP.o MISCLN.o MIXLEN.o MDL2P.o \
          MDLFLD.o MPI_FIRST.o  MPI_LAST.o NGMFLD.o NGMSLP.o  OTLFT.o OTLIFT.o SLP_new.o SLP_NMM.o \
          EXCH.o PARA_RANGE.o PROCESS.o INITPOST_NMM.o EXCH2.o READCNTRL.o  READ_xml.o \
          SET_OUTFLDS.o SCLFLD.o  SERVER.o \
          SETUP_SERVERS.o SMOOTH.o SURFCE.o SPLINE.o  TABLE.o  TABLEQ.o  TRPAUS.o  TTBLEX.o WETBULB.o \
          WRFPOST.o CALMICT.o MICROINIT.o GPVS.o MDL2SIGMA.o ETCALC.o CANRES.o \
          CALGUST.o WETFRZLVL.o SNFRAC.o MDL2AGL.o SNFRAC_GFS.o AVIATION.o DEALLOCATE.o \
          INITPOST_NMM_BIN_MPIIO_READ.o INITPOST_NMM_BIN_MPIIO.o CALPBL.o MDL2SIGMA2.o INITPOST_GFS.o CALRH_GFS.o LFMFLD_GFS.o \
          CALRAD_WCLOUD_newcrtm.o MDL2THANDPV.o CALPBLREGIME.o POLEAVG.o INITPOST_NEMS.o \
          GETNEMSNDSCATTER.o ICAOHEIGHT.o INITPOST_GFS_NEMS.o INITPOST_BIN_MPIIO.o \
          GEO_ZENITH_ANGLE.o GFIP3.o GRIDAVG.o CALUPDHEL.o MSFPS.o INITPOST_GFS_SIGIO.o\
          AllGETHERV_GSD.o SELECT_CHANNELS.o ALLOCATE_ALL.o

OBJS   = $(OBJS_F) $(OBJXML) $(OBJS_FT)

# -----------
# Targets
# -----------
all: $(TARGET)

$(TARGET):	wrflink $(XML_DEPS) $(OBJS)
	$(F90) -o $@ $(FFLAGS) $(MODULES) $(OBJS) $(LDFLAGS) $(EXTRA_LDFLAGS)
	$(CP) $@ $(BINDIR)
#
# The following links are done for compilation/link errors found in various compilers
wrflink: $(WRF_DIR)/frame/module_internal_header_util.mod
	$(LN)  $(WRF_DIR)/frame/module_internal_header_util.mod $(INCMOD)/
	$(LN)  $(WRF_DIR)/external/io_int/module_io_int_idx.mod $(INCMOD)/
#
# This insures a dependency found in some files -- watch file order above remains -- should
# be done w/ dependencies
$(OBJS_F): $(OBJS_FT) $(OBJXML)

#
# These files are configurable, but rarely change
INITPOST_NMM_BIN_MPIIO.f:
	-$(RM) INITPOST_NMM_BIN_MPIIO.f
	$(LN) $(INITPOST_NMM_BIN_MPIIO) INITPOST_NMM_BIN_MPIIO.f

clean:	
	@echo -e "\n<><><><> CLEAN <><><><>\n$@ in `pwd`"
	$(RM) $(TARGET) $(OBJS) *.lst *.mod
	$(RM) $(BINDIR)/$(TARGET)
	$(RM) $(INCMOD)/module_internal_header_util.mod $(INCMOD)/module_ext_internal.mod
	for f in `ls -1 *.F|sed "s/.F$$/.f/"` ; do \
      $(RM) $$f   ; \
   done

distclean: clean

.IGNORE:
.PHONY: clean 

.SUFFIXES:
.SUFFIXES:	.F .f .f90 .o

.F.o:
	$(CPP) $(CPP_FLAGS) $(EXTRA_CPPFLAGS) $< > $*.f
	$(F90)  -c $(FFLAGS) $(EXTRA_FFLAGS) $*.f

.f.o: 
	$(F90)  -c $(FFLAGS) $(EXTRA_FFLAGS) $<

.f90.o:
	$(F90)  -c $(FFLAGS) $(EXTRA_FFLAGS) $<
