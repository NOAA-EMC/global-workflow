#%Module######################################################################
#  Module file for grib_util
##############################################################################
module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load intel/18.0.5.274 
module load bacio/2.0.3
module load w3emc/2.3.1
module load w3nco/2.0.6
module load ip/3.0.1
module load sp/2.0.2

module load jasper/1.900.1
module load png/1.2.44
module load z/1.2.11

export FCMP=ifort
export CCMP=icc

