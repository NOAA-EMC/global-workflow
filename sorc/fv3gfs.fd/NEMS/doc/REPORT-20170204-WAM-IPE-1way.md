One-Way WAM to IPE Coupling Test Report {#REPORT-20170204-WAM-IPE-1way}
=======================================

\date 2/4/2017

Versioning
----------

**User**: Robert.Oehmke

**Project**: nems

**Platform**: Theia Cray CS400

**ESMF version**: ESMF_7_0_0

### Repositories

Application:

    https://svnemc.ncep.noaa.gov/projects/ipe/WAM-IPE
    -r 87779 (February 2017)

NEMS:

    https://svnemc.ncep.noaa.gov/projects/nems/branches/WAM-IPE/milestone3
    -r 76674 (May 2016)

WAM/GSM:

    https://svnemc.ncep.noaa.gov/projects/gsm/branches/WAM-IPE/milestone3
    -r 76469 (May 2016)

IPE:

    https://github.com/IonospherePlasmasphereElectrodynamics/ipe
    -r 404 (January 2017)

IPE_CAP:

    https://svnemc.ncep.noaa.gov/projects/ipe/branches/nuopc_cap
    -r 87260 (January 2017)

### Model Versions
      
 * Ionosphere Plasmasphere Electrodynamics (IPE) model R362 (May 2016)

 * Whole Atmosphere Model (WAM) R76469 (May 2016)

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

### NEMS Compsets

#### `swpc%20130316_nodensities_6day_spacewx_gsm%wam%T62_ipe%80x170`

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################
#
#  WAM-IPE coupled run
#
###############################################################################

export TEST_DESCR="WAM-IPE 1h coupled run"

# - gsm configuration ---
export_gsm
export CDATE=2013031600
export WLCLK=480
export NDAYS=6
export FHOUT=1
export TASKS=104
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
export IPECASE=20130316_nodensities_6day_spacewx_80x170

# - nems.configure ---
export_nems
export nems_configure=med_atm_ipm
export atm_model=gsm
export atm_petlist_bounds="0 15"
export ipm_model=ipe
export ipm_petlist_bounds="16 23"
export med_model=spaceweather
export med_petlist_bounds="24 103"
export coupling_interval_fast_sec=180.0
export coupling_interval_sec=180.0

export F107_KP_SIZE=56
export F107_KP_INTERVAL=10800
export WAM_IPE_COUPLING=.true.
export HEIGHT_DEPENDENT_G=.true.
export F107_KP_SKIP_SIZE=24

# - component specific setup calls ---
setup_wam_T62_2013031600
setup_ipe
setup_spaceweather_gsm%wam%T62_ipe%80x170

# -
RUN_SCRIPT=rt_gfs.sh

# - validation
export CNTL_DIR=swpc%20130316_nodensities_6day_spacewx_gsm%wam%T62_ipe%80x170_V0002
export LIST_FILES="IPE.inp SMSnamelist \
                  sigf00 sigf01 sfcf00 sfcf01 flxf00 flxf01 \
                  plasma00 plasma01 plasma02 plasma03 plasma04 \
                  plasma05 plasma06 plasma07 plasma08 plasma09 \
                  plasma10 plasma11 plasma12 plasma13 plasma14 \
                  plasma15 plasma16 \
                  wam3dgridnew2.nc ipe3dgrid2.nc wam2dmesh.nc"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### NEMS Configuration

#### `nems.configure.med_atm_ipm.IN`

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#############################################
####  NEMS Run-Time Configuration File  #####
#############################################

# EARTH #
EARTH_component_list: MED ATM IPM
EARTH_attributes::
 Verbosity = max
::

# MED #
MED_model:                      _med_model_
MED_petlist_bounds:             _med_petlist_bounds_
MED_attributes::
 Verbosity = max
 DumpFields = false
 DumpRHs = false
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
   ATM -> MED :remapMethod=redist
   MED
   MED -> IPM :remapMethod=redist
   ATM
   IPM
 @
::
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Runtime Data Files


**IPE**:

    /scratch3/NCEPDEV/swpc/noscrub/Naomi.Maruyama/ipe/grid/apex/1/GIP_apex_coords_global_lowres_new20120705
    /scratch4/NCEPDEV/nems/noscrub/NEMS-Data/IPE/cases/20130316_nodensities_6day_spacewx_80x170/*

**WAM**: 

    /scratch4/NCEPDEV/nems/noscrub/NEMS-Data/WAM/T62_2013031600/*anl* 
    /scratch4/NCEPDEV/nems/noscrub/NEMS-Data/WAM/T62_2013031600/wam_input_f107_kp.txt

### Run Directory

    /scratch3/NCEPDEV/swpc/scrub/Robert.Oehmke/rt_74190/swpc%20130316_nodensities_6day_spacewx_gsm%wam%T62_ipe%80x170

Validation 
----------

To validate this run the total electron content from IPE was plotted
four times a day each day over the entire six day run. These plots
were then examined by the scientist working on IPE to ensure that they
looked correct.
