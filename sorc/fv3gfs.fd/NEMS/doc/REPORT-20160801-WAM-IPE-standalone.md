Comparison with Stand-alone Components Test Report {#REPORT-20160801-WAM-IPE-standalone}
================================================== 

\date 08/1/2016

**User**: Robert.Oehmke

**Project**: nems

**Platform**: Theia Cray CS400

**ESMF Version**: ESMF_7_0_0

### Repositories:
     
Application:

    https://svnemc.ncep.noaa.gov/projects/ipe/WAM-IPE
        -r 80061 

NEMS:

    https://svnemc.ncep.noaa.gov/projects/nems/branches/WAM-IPE/milestone4
        -r 80061 

WAM/GSM:

    https://svnemc.ncep.noaa.gov/projects/gsm/branches/WAM-IPE/milestone4
        -r 76469 

IPE:

    https://github.com/IPEBestModelInTheWorld/ipe/trunk
        -r 362 

IPE_CAP:

    https://svnemc.ncep.noaa.gov/projects/ipe/branches/nuopc_cap
        -r 75858

Stand-alone Version of IPE Repository:

    https://github.com/IPEBestModelInTheWorld/ipe/trunk
        -r 368 

### Model Versions
      
 * NEMS Ionosphere Plasmasphere Electrodynamics (IPE) model R362 

 * NEMS Whole Atmosphere Model (WAM) R76469 

 * Stand-alone Ionosphere Plasmasphere Electrodynamics (IPE) model R368 

Execution
---------

### Environment  

    COMPONENTS=( GSM, IPE, DATAWAM, DATAIPE)
    IPE_SRCDIR=$ROOTDIR/IPE
    IPE_BINDIR=$ROOTDIR/IPE-INSTALL
    DATAWAM_SRCDIR=$ROOTDIR/DATAWAM
    DATAWAM_BINDIR=$ROOTDIR/DATAWAM-INSTALL
    DATAIPE_SRCDIR=$ROOTDIR/DATAIPE
    DATAIPE_BINDIR=$ROOTDIR/DATAIPE-INSTALL
    source /etc/profile
    module load intel impi netcdf
        Intel: 14.0.2
        Intel MPI: 4.1.3.048
        NetCDF: 4.3.0
    module use /scratch4/NCEPDEV/nems/save/Gerhard.Theurich/Modulefiles
    module load esmf/7.0.0

### NEMS Build Command

    NEMS/NEMSAppBuilder


### NEMS Compsets

#### `swpc%20090115_1hr_sbys_gsm%wam%T62_ipe%80x170`

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################
#
#  WAM-IPE side-by-side run
#
###############################################################################

export TEST_DESCR="WAM-IPE 1h side-by-side run"

# - gsm configuration ---
export_gsm
export CDATE=2009011500
export WLCLK=30
export NHRS=1
export FHOUT=1
export TASKS=32
export PE1=32
export THRD=1
export QUILT=.false.
export FDFI=0
export CP2=.false.
export IDEA=.true.
export IDVC=3
export THERMODYN_ID=3
export SFCPRESS_ID=2
export SPECTRALLOOP=2

# - IPE configuration ---
export IPECASE=20090115_1hr_sbys_80x170

# - nems.configure ---
export_nems
export nems_configure=atm_ipm
export atm_model=gsm
export atm_petlist_bounds="0 15"
export ipm_model=ipe
export ipm_petlist_bounds="16 31"
export coupling_interval_fast_sec=180.0
export coupling_interval_sec=180.0
export F107_KP_SIZE=56
export F107_KP_INTERVAL=10800
export WAM_IPE_COUPLING=.true.
export HEIGHT_DEPENDENT_G=.true.
export F107_KP_SKIP_SIZE=24

# - component specific setup calls ---
setup_wam_T62_2009011500
setup_ipe
setup_spaceweather_gsm%wam%T62_ipe%80x170

# -
RUN_SCRIPT=rt_gfs.sh

# - validation
export CNTL_DIR=swpc%20090115_1hr_sbys_gsm%wam%T62_ipe%80x170_V0002
export LIST_FILES="IPE.inp SMSnamelist \
                  sigf00 sigf01 sfcf00 sfcf01 flxf00 flxf01 \
                  plasma00 plasma01 plasma02 plasma03 plasma04 \
                  plasma05 plasma06 plasma07 plasma08 plasma09 \
                  plasma10 plasma11 plasma12 plasma13 plasma14 \
                  plasma15 plasma16 \
                  wam3dgridnew2.nc ipe3dgrid2.nc"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### `swpc%20090115_1hr_sbys_gsm%wam%T62`

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
###############################################################################
#
#  WAM side-by-side run
#
###############################################################################

export TEST_DESCR="WAM 1h side-by-side run"

# - gsm configuration ---
export_gsm
export CDATE=2009011500
export WLCLK=30
export NHRS=1
export FHOUT=1
export TASKS=32
export PE1=32
export THRD=1
export QUILT=.false.
export FDFI=0
export CP2=.false.
export IDEA=.true.
export IDVC=3
export THERMODYN_ID=3
export SFCPRESS_ID=2
export SPECTRALLOOP=2

# - nems.configure ---
export_nems
export nems_configure=atm
export atm_model=gsm
export atm_petlist_bounds="0 15"
export coupling_interval_fast_sec=180.0
export coupling_interval_sec=180.0
export F107_KP_SIZE=56
export F107_KP_INTERVAL=10800
#export WAM_IPE_COUPLING=.true.
export HEIGHT_DEPENDENT_G=.true.
export F107_KP_SKIP_SIZE=24

# - component specific setup calls ---
setup_wam_T62_2009011500
setup_spaceweather_gsm%wam%T62_ipe%80x170

# -
RUN_SCRIPT=rt_gfs.sh

# - validation
export CNTL_DIR=swpc%20090115_1hr_sbys_gsm%wam%T62_ipe%80x170_V0002
export LIST_FILES="sigf00 sigf01 sfcf00 sfcf01 flxf00 flxf01"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
### NEMS Configurations
     
#### `nems.configure.atm_ipm.IN`

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
#############################################
####  NEMS Run-Time Configuration File  #####
#############################################

# EARTH #
EARTH_component_list: MED ATM IPM
EARTH_attributes::
 Verbosity = max
::

# ATM #
ATM_model:                      _atm_model_
ATM_petlist_bounds:             _atm_petlist_bounds_
ATM_attributes::
Verbosity = max
::

# IPM #
IPM_model:                      _ipm_model_
IPM_petlist_bounds:             _ipm_petlist_bounds_
IPM_attributes::
 Verbosity = max
::

# Run Sequence #
runSeq::
 @_coupling_interval_sec_
   ATM
   IPM
 @
::
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### `nems.configure.atm.IN`

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
#############################################
####  NEMS Run-Time Configuration File  #####
#############################################

# EARTH #
EARTH_component_list: MED ATM IPM
EARTH_attributes::
 Verbosity = max
::

# ATM #
ATM_model:                      _atm_model_
ATM_petlist_bounds:             _atm_petlist_bounds_
ATM_attributes::
Verbosity = max
::

# Run Sequence #
runSeq::
 @_coupling_interval_sec_
   ATM
 @
::
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Runtime Data Files

IPE:

    /scratch3/NCEPDEV/swpc/noscrub/Naomi.Maruyama/ipe/grid/apex/1/GIP_apex_coords_global_lowres_new20120705
    /scratch4/NCEPDEV/nems/noscrub/NEMS-Data/IPE/cases/20090115_1hr_sbys_80x170/

WAM: 

    /scratch4/NCEPDEV/nems/noscrub/NEMS-Data/WAM/T62_2009011500/*anl* 
    /scratch4/NCEPDEV/nems/noscrub/NEMS-Data/WAM/T62_2009011500/wam_input_f107_kp.txt

### Run Directories

Side by Side WAM-IPE:

    /scratch3/NCEPDEV/swpc/scrub/Robert.Oehmke/rt_86860/swpc%20090115_1hr_sbys_gsm%wam%T62_ipe%80x170

Stand-alone IPE:

    /scratch3/NCEPDEV/swpc/noscrub/Robert.Oehmke/IPE/run/1469939250_ipe_theia_intel_parallel_40

Stand-alone WAM:

    /scratch3/NCEPDEV/swpc/scrub/Robert.Oehmke/rt_123761/swpc%20090115_1hr_sbys_gsm%wam%T62

Validation
----------
 

Validated by using the UNIX cmp command to do a byte by byte comparison of output files.

 
| File Name | File Source 1   | File Source 1                   | Number of Bytes Difference | Status |
| :-------: | :-------------: | :-----------------------------: | :------------------------: | :----: |
| plasma00  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma01  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma02  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma03  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma04  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma05  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma06  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma07  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma08  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma09  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma10  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma11  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma12  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma13  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma14  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma15  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| plasma16  | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| fort.2000 | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| fort.2001 | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| fort.2002 | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| fort.2003 | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| fort.2004 | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| fort.2005 | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| fort.2006 | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| fort.2007 | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| fort.2008 | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| fort.2009 | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |
| fort.2010 | Stand-alone IPE | IPE within side by side WAM-IPE |     0         | OK |

| File Name | File Source 1   | File Source 1                   | Number of Bytes Difference | Status |
| :-------: | :-------------: | :-----------------------------: | :------------------------: | :----: |
| flxf00 | Stand-alone WAM | WAM within side by side WAM-IPE |     0         | OK |
| flxf01 | Stand-alone WAM | WAM within side by side WAM-IPE |     0         | OK |
| sfcf00 | Stand-alone WAM | WAM within side by side WAM-IPE |     0         | OK |
| sfcf01 | Stand-alone WAM | WAM within side by side WAM-IPE |     0         | OK |
| sigf00 | Stand-alone WAM | WAM within side by side WAM-IPE |     0         | OK |
| sigf01 | Stand-alone WAM | WAM within side by side WAM-IPE |     0         | OK |

