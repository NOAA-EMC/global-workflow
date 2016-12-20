# makefile for GCIP
#----------------------------------------
#ifort -O3 GFIPS_Exe.f90 -L /nwprod/lib  -lbacio_4 -lip_4 -lw3nco_4 -lw3emc_4  -lbacio_4 -lsp_4
#machine dependent: either zeus (hostname consists of 'fe') or wcoss
#FC = ifort
#FFLAGS = -FR -I$(INC) -g -O2
FFLAGS	= ${FFLAGSgcip}

#----------------------------------------
# Clear out all suffixes
.SUFFIXES:
# list what we use
.SUFFIXES: .f90 .o
# defines a suffix rule for .f90 -> .o
.f90.o :
	$(FC) $(FFLAGS) -c -C $<

#----------------------------------------
# Macros
# Can Not Dynamically Reset Macros
# The last macro declaration defines the overall macro definition to be used everywhere.
# It's discouraged to use globbing (*, ?). It works for GNU make, but not for the Cray or IBM SP.
CMD = gcip
SRCS = kinds.f90 \
	gridtemplate.f90 \
	config.f90 \
	quicksort.f90 \
	grib1.f90 \
	grib2.f90 \
	hybrid2pressure.f90 \
	derived_fields.f90 \
	model.f90 \
	metar.f90 \
	satellite.f90\
	pirep.f90 \
	lightning.f90 \
	radar.f90 \
	icing.f90 \
	algo.f90 \
	pressure2flight.f90 \
	gcip.f90

# To substitute ".f90" with ".o" in the list given by $(SRCS)
OBJS = $(SRCS:.f90=.o)
MODS = `echo $(SRCS:.f90=.mod)|tr A-Z a-z`

#----------------------------------------
# default target by convention is ``all''
all : $(CMD)
$(CMD) : $(OBJS)
	$(FC) $? $(LIBS) -o $(CMD)
 $(OBJS) :

#
# @ tells not to echo the command to stdout. 
# Echoing can be disabled globally with either the ``-s'' option or the fake target .SILENT. 
clean:
	@echo "rm -f $(OBJS) $(MODS)"
	@rm -f $(OBJS) $(MODS)
	@rm -f main.mod

install:
	mv $(CMD) ../../exec/.

#----------------------------------------
#----Predefined and Internal Macros------
# $@    the name of the file/target to be ``made''
# $? 	the set of dependent names that are younger than the target
# $< 	the name of the related file that caused the action (the precursor to the target) - this is only for suffix rules
# $* 	the shared prefix of the target and dependent - only for suffix rules
# $$ 	escapes macro substitution, returns a single ``$''.

# VPATH tells make where; A colon (:) delimited; Don't include "." since it's always searched.
# VPATH = ./data:../:../scripts


# If the make is not called with a proper argument or no argument 
# it uses the ``fake'' target .DEFAULT to execute the command given there.


# include file
# reads and evaluates file as if part of the current Makefile. 
# Must not have any white-space at beginning of line.
# useful for Recursive Make For Sub-directories
