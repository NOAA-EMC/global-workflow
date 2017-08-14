#==============================================================================
#
# REGRID_NEMSIO Makefile
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Parent make (calls child make) --
#-----------------------------------------------------------------------------

# -------------
# General Rules
# -------------

SHELL=/bin/sh

RM              = /bin/rm -f
MKDIR           = /bin/mkdir -p

#------------
# Include machine dependent compile & load options
#------------

MAKE_CONF =
include $(MAKE_CONF)

# -------------
# This makefile
# -------------

MAKE_FILE = Makefile

# -----------
# Load module
# -----------

EXE_FILE = regrid_nemsio

# --------------------
# Installing directory
# --------------------

INSTALL_DIR = ../../exec/

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

#-----------
# Make clean
# ----------

clean:
	@echo
	@echo '==== CLEAN ==================================================='
	- $(RM) $(EXE_FILE) *.o *.mod
	- $(RM) log.make.$(EXE_FILE)

#-----------------------------------------------------------------------------
#                          -- Child make --
#-----------------------------------------------------------------------------

# ---------
# Libraries
# ---------

INCS = $(NETCDF_INCLUDE) -I$(NEMSIO_INC)
LIBS = $(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd) $(SP_LIB4) $(NETCDF_LDFLAGS)

# ------------
# Source files
# ------------

SRCSF90 =                                          \
     kinds.f90                                     \
     constants.f90                                 \
     physcons.f90                                  \
     mpi_interface.f90                             \
     namelist_def.f90                              \
     variable_interface.f90                        \
     netcdfio_interface.f90                        \
     interpolation_interface.f90                   \
     gfs_nems_interface.f90                        \
     fv3_interface.f90

SRCS =  $(SRCSF77) $(SRCSF90)

# ------------
# Object files
# ------------

OBJS = ${SRCSF90:.f90=.o} ${SRCSF77:.f=.o}

# ------------
# Dependencies
# ------------
MAKE_DEPEND = Makefile.dependency
include $(MAKE_DEPEND)

# -----------------------
# Default compiling rules
# -----------------------

.SUFFIXES :
.SUFFIXES : .F90 .f90 .f .c .o

.f90.o  :
	@echo
	@echo '---> Compiling $<'
	$(F90) $(FCFFLAGS) $(INCS) $(OPTIMIZATION) $(DEBUG) -c $<

.f.o  :
	@echo
	@echo '---> Compiling $<'
	$(F77) $(FCFFLAGS) $(OPTIMIZATION) $(DEBUG) -c $<

# ------------------------
# Call compiler and linker
# ------------------------

all: REGRID_NEMSIO

REGRID_NEMSIO: $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) $(INCS) main.f90 $(LIBS) -o $(EXE_FILE) > $(LOG_FILE)

help:
	@ echo "Available targets:"
	@ echo "  make             creates executable"
	@ echo "  make install     creates exec & places it in bin"
	@ echo "  make clean       cleans objects, exec, and alien files"
