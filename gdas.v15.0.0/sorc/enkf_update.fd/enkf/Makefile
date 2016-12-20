SHELL=/bin/sh

#==============================================================================
#
# EnKF Makefile
#
# <Usage>
#   0) Export this makefile name to a variable 'MAKE_FILE' as 
#       export MAKE_FILE = makefile
#      If this file is named neither 'makefile' nor 'Makefile' but 
#      'makeairs' for instance, then call this makefile by typing
#      'make -f makeairs' instead of 'make'.
#
#  0a) Modify the include link to either use compile.config.ibm
#      or compile.config.sgi for compilation on the ibm sp or sgi
#
#   1) To make a EnKF executable file, type
#         > make  or  > make all
#
#   2) To make a EnKF executable file with debug options, type
#         > make debug
#
#   3) To copy the EnKF load module to installing directory, type
#         > make install
#      . Specify the directory to a variable 'INSTALL_DIR' below.
#
#   4) To crean up files created by make, type
#         > make clean
#
#
# Created by Y.Tahara in May,2002
# Edited by D.Kleist Oct. 2003
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Parent make (calls child make) --
#-----------------------------------------------------------------------------

# -----------------------------------------------------------
# Default configuration, possibily redefined in Makefile.conf
# -----------------------------------------------------------

ARCH	 = `uname -s`
SED      = sed
DASPERL  = /usr/bin/perl
COREROOT = ../../..
COREBIN  = $(COREROOT)/bin
CORELIB  = $(COREROOT)/lib
COREINC  = $(COREROOT)/include
COREETC  = $(COREROOT)/etc


# -------------
# General Rules
# -------------

CP              = /bin/cp -p
RM              = /bin/rm -f
MKDIR           = /bin/mkdir -p
AR              = ar cq
PROTEX          = protex -f # -l
ProTexMake      = protex -S # -l
LATEX           = pdflatex
DVIPS           = dvips

# Preprocessing
# -------------
_DDEBUG =
_D      = $(_DDEBUG)

# ---------
# Libraries
# ---------
LIBmpeu   = -L$(CORELIB) -lmpeu
LIBbfr    = -L$(CORELIB) -lbfr
LIBw3     = -L$(CORELIB) -lw3
LIBsp     = -L$(CORELIB) -lsp
LIBbacio  = -L$(CORELIB) -lbacio
LIBsfcio  = -L$(CORELIB) -lsfcio
LIBsigio  = -L$(CORELIB) -lsigio
LIBtransf = -L$(CORELIB) -ltransf
LIBhermes = -L$(CORELIB) -lhermes
LIBgfio   = -L$(CORELIB) -lgfio

# --------------------------
# Default Baselibs Libraries
# --------------------------
INChdf          = -I$(BASEDIR)/$(ARCH)/include/hdf
LIBhdf          = -L$(BASEDIR)/$(ARCH)/lib  -lmfhdf -ldf -lhdfjpeg -lhdfz -lsz
LIBnetcdf       = -L$(BASEDIR)/$(ARCH)/lib -lnetcdf -lnetcdff
LIBwrf          = -L$(BASEDIR)/$(ARCH)/lib -lwrflib
LIBwrfio_int    = -L$(BASEDIR)/$(ARCH)/lib -lwrfio_int
LIBwrfio_netcdf = -L$(BASEDIR)/$(ARCH)/lib -lwrfio_nf

# ------------------------
# Default System Libraries
# ------------------------
LIBmpi          = -lmpi
LIBsys          = 


#------------
# Include machine dependent compile & load options
#------------
  MAKE_CONF = Makefile.conf
include $(MAKE_CONF)


# -------------
# This makefile
# -------------

  MAKE_FILE = Makefile


# -----------
# Load module
# -----------

  #EXE_FILE = global_enkf


# --------------------
# Installing directory
# --------------------

  INSTALL_DIR = ../bin


# --------
# Log file
# --------

  LOG_FILE = log.make.$(EXE_FILE)


# ---------------
# Call child make
# ---------------

"" :
	@$(MAKE) -f $(MAKE_FILE) all


# ------------
# Make install
# ------------

install:
	@echo
	@echo '==== INSTALL ================================================='
	@if [ -e $(INSTALL_DIR) ]; then \
	  if [ ! -d $(INSTALL_DIR) ]; then \
	    echo '### Fail to create installing directory ###' ;\
	    echo '### Stop the installation               ###' ;\
	    exit ;\
	  fi ;\
	else \
	  echo "	mkdir -p $(INSTALL_DIR)" ;\
	  mkdir -p $(INSTALL_DIR) ;\
	fi
	cp $(EXE_FILE) $(INSTALL_DIR)
	@cd $(INSTALL_DIR) ; ls -l `pwd`/$(EXE_FILE)


# ----------
# Make clean
# ----------

clean:
	@echo
	@echo '==== CLEAN ==================================================='
	- $(RM) $(EXE_FILE) *.o *.mod *.MOD *.lst *.a *.x
	- $(RM) loadmap.txt log.make.$(EXE_FILE)
	- $(MAKE) -f ${MAKE_FILE} doclean


#-----------------------------------------------------------------------------
#                          -- Child make --
#-----------------------------------------------------------------------------

# ------------
# Source files
# ------------

  SRCSF90C = \
	covlocal.f90 \
	enkf.f90 \
	enkf_main.f90 \
	fftpack.f90 \
	genqsat1.f90 \
	gridinfo.F90 \
	gridio.F90 \
	inflation.f90 \
	innovstats.f90 \
	kdtree2.f90 \
	letkf.F90 \
	loadbal.f90 \
	mpi_readobs.f90 \
	mpisetup.F90 \
	netcdf_io_wrf.f90 \
	enkf_obsmod.f90 \
	params.f90 \
	sorting.f90 \
	radbias.f90 \
	read_locinfo.f90 \
	readconvobs.f90 \
	readozobs.f90 \
	readsatobs.f90 \
	reducedgrid.f90 \
	rnorm.f90 \
	smooth.F90 \
	specmod.f90 \
	statevec.f90 \
	write_logfile.f90

  SRCSF90C_NOSWAP =

  ENKFGC_SRCS =

  SRCSF77 =

  SRCSC =

  SRCS = $(SRCSF90C) $(ENKFGC_SRCS) $(SRCSF77) $(SRCSC) $(XSRCSC)

  DOCSRCS = *.f90 *.F90

# ------------
# Object files
# ------------

  SRCSF90	= ${SRCSF90C:.F90=.f90}
  SRCSF90_NOSWAP= ${SRCSF90C_NOSWAP:.F90=.f90}

  OBJS 		= ${SRCSF90:.f90=.o} ${SRCSF77:.f=.o} ${SRCSC:.c=.o}
  OBJS_NOSWAP	= ${SRCSF90_NOSWAP:.f90=.o}

  GSIOBJS	= $(shell find ../ -maxdepth 1 -name "*.o" -print | grep -v gsimain)


# -----------------------
# Default compiling rules
# -----------------------

.SUFFIXES :
.SUFFIXES : .F90 .f90 .f .c .o

.F90.o  :
	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGS) $(_D) -c $<

.f90.o  :
	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGS) -c $<

.f.o  :
	@echo
	@echo '---> Compiling $<'
	$(CF) $(FFLAGS_f) -c $<

.c.o  :
	@echo
	@echo '---> Compiling $<'
	$(CC) $(CFLAGS) -c $<

$(OBJS_NOSWAP) :
	@echo '---> Special handling of Fortran "native" BUFR-OPEN $<'
	$(CF) -c $(FFLAGS_NOSWAP) $<


# ------------
# Dependencies
# ------------
  MAKE_DEPEND = Makefile.dependency
include $(MAKE_DEPEND)

# ----

$(EXE_FILE) :  $(OBJS) $(OBJS_NOSWAP) $(GSIOBJS)
	$(LD) -o $@ $(OBJS) $(OBJS_NOSWAP) $(GSIOBJS) $(LIBS) $(LDFLAGS)


# ------------------------
# Call compiler and linker
# ------------------------

all :
	@$(MAKE) -f $(MAKE_FILE) "COMP_MODE=$@" check_mode
	@echo
	@echo '==== COMPILE ================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"FFLAGS=$(FFLAGS_N)" \
		"FFLAGS_NOSWAP=$(FFLAGS_NOSWAP_N)" \
		"CFLAGS=$(CFLAGS_N)" \
		$(OBJS) $(OBJS_NOSWAP)
	@echo
	@echo '==== LINK ===================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"LIBS=$(LIBS_N)" "LDFLAGS=$(LDFLAGS_N)" \
		$(EXE_FILE)

debug :
	@$(MAKE) -f $(MAKE_FILE) "COMP_MODE=$@" check_mode
	@echo
	@echo '==== COMPILE ================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"FFLAGS=$(FFLAGS_D)" \
		"FFLAGS_NOSWAP=$(FFLAGS_NOSWAP_D)" \
		"CFLAGS=$(CFLAGS_D)" \
		$(OBJS) $(OBJS_NOSWAP)
	@echo
	@echo '==== LINK ===================================================='
	@$(MAKE) -f $(MAKE_FILE) \
		"LIBS=$(LIBS_D)" "LDFLAGS=$(LDFLAGS_D)" \
		$(EXE_FILE)

check_mode :
	@if [ -e $(LOG_FILE) ]; then \
	  if [ '$(COMP_MODE)' != `head -n 1 $(LOG_FILE)` ]; then \
	    echo ;\
	    echo "### COMPILE MODE WAS CHANGED ###" ;\
	    $(MAKE) -f $(MAKE_FILE) clean ;\
	  fi ;\
	else \
	  echo ;\
	  echo "### NO LOG FILE ###" ;\
	  $(MAKE) -f $(MAKE_FILE) clean ;\
	fi
	@echo $(COMP_MODE) > $(LOG_FILE)

# -------------------------
# GMAO Nomenclature/targets
# -------------------------
LIB =   libenkf.a

lib: $(LIB)

enkf.x:  $(OBJS) $(OBJS_NOSWAP) $(LIB)
	$(FC) $(LDFLAGS) -o enkf.x enkf_main.o libenkf.a $(LIBsfcio) $(LIBsigio) $(LIBw3) $(LIBbacio) $(LIBbfr) $(LIBsp) $(LIBtransf) $(LIBhermes) $(LIBmpeu) $(LIBgfio) $(LIBhdf) $(LIBmpi) $(LIBsys)

$(LIB): $(OBJS) $(OBJS_NOSWAP)
	$(RM) $(LIB)
	$(AR) $@ $(OBJS) $(OBJS_NOSWAP)

export: libenkf.a enkf.x prepbykx.x
	$(MKDIR)               $(COREBIN)
	$(CP) $(LIB)           $(CORELIB)
	$(CP) enkf.x            $(COREBIN)
	$(CP) enkf.rc.sample    $(COREETC)/enkf.rc
	$(CP) tlmadj_parameter.rc.sample $(COREETC)/tlmadj_parameter.rc
	$(CP) gmao_airs_bufr.tbl       $(COREETC)/gmao_airs_bufr.tbl
	$(CP) gmao_global_pcpinfo.txt  $(COREETC)/gmao_global_pcpinfo.rc
	$(CP) gmao_global_satinfo.txt  $(COREETC)/gmao_global_satinfo.rc
	$(CP) gmao_global_ozinfo.txt   $(COREETC)/gmao_global_ozinfo.rc
	$(CP) gmao_global_convinfo.txt $(COREETC)/gmao_global_convinfo.rc
	$(SED) -e "s^@DASPERL^$(DASPERL)^" < analyzer > $(COREBIN)/analyzer
	chmod 755 $(COREBIN)/analyzer

doc:              AnIntro $(DOCSRC)
	$(PROTEX) AnIntro *.f90 *.F90 >  enkf.tex
	$(LATEX) enkf.tex
	$(LATEX) enkf.tex

doclean:
	- $(RM) *.tex *.dvi *.aux *.toc *.log *.ps *.pdf

help:
	@ echo "Available targets:"
	@ echo "NCEP:  make             creates enkf executable        "
	@ echo "NPEP:  make debug       created enkf exec for debugging purposes"
	@ echo "NCEP:  make install     creates enkf exec & places it in bin"
	@ echo "GMAO:  make lib         creates enkf library"
	@ echo "GMAO:  make export      creates lib, exec, & copies all to bin/inc/etc"
	@ echo "       make clean       cleans objects, exec, and alien files"  
	@ echo "       make doc         creates documentation"
	@ echo "       make doclean     clean doc-related temporary files"

