#    Copyright (C) 2015 Henry R. Winterbottom

#    Email: Henry.Winterbottom@noaa.gov

#    Snail-mail:

#    Henry R. Winterbottom
#    NOAA/OAR/PSD R/PSD1
#    325 Broadway
#    Boulder, CO 80303-3328

#    This file is part of global-model-py.

#    global-model-py is free software: you can redistribute it and/or
#    modify it under the terms of the GNU General Public License as
#    published by the Free Software Foundation, either version 3 of
#    the License, or (at your option) any later version.

#    global-model-py is distributed in the hope that it will be
#    useful, but WITHOUT ANY WARRANTY; without even the implied
#    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#    See the GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with global-model-py.  If not, see
#    <http://www.gnu.org/licenses/>.

SHELL=/bin/sh

#==============================================================================
#
# INCREMENT_REMAPPING Makefile
#
# <Usage>
#   0) Export this makefile name to a variable 'MAKE_FILE' as 
#      export MAKE_FILE = makefile
#      If this file is named neither 'makefile' nor 'Makefile' but 
#      'makeairs' for instance, then call this makefile by typing
#      'make -f makeairs' instead of 'make'.
#
#   1) To make a INCREMENT_REMAPPING executable file, type
#         > make  or  > make all
#
#   2) To copy the INCREMENT_REMAPPING load module to installing directory,
#      type 
#         > make install 
#      Specify the directory to a variable 'INSTALL_DIR' below.
#
#   3) To clean up files created by make, type
#         > make clean
#
# Created by Y.Tahara in May,2002
# Edited by H. R. Winterbottom August, 2015
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Parent make (calls child make) --
#-----------------------------------------------------------------------------

# -------------
# General Rules
# -------------

RM              = /bin/rm -f
MKDIR           = /bin/mkdir -p

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

  EXE_FILE = calc_increment.x

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

  INCS = $(INCnetcdf) $(INCnemsio)
  LIBS = $(NEMSIO_LIB) $(BACIO_LIB4) $(W3NCO_LIBd) $(LIBnetcdf)

# ------------
# Source files
# ------------

  SRCSF90 =                                          \
       kinds.f90                                     \
       constants.f90                                 \
       namelist_def.f90                                  \
       variable_interface.f90                        \
       gfs_nems_interface.f90                        \
       fv3_interface.f90                             \
       calc_increment_interface.f90

  SRCSF77 =

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

all: INCREMENT_REMAPPING

INCREMENT_REMAPPING: $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) main.f90 $(LIBS) -o $(EXE_FILE) > $(LOG_FILE)

help:
	@ echo "Available targets:"
	@ echo "NCEP:  make             creates executable"
	@ echo "NCEP:  make install     creates exec & places it in bin"
	@ echo "       make clean       cleans objects, exec, and alien files"
