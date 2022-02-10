#%Module#####################################################
## Module file for fv3nc2nemsio
#############################################################

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load netcdf/4.7.0
module load hdf5/1.10.5
module load bacio/2.0.2
module load nemsio/2.2.3
module load w3nco/2.0.6

export FCMP="ifort"
export FFLAGS="-g -O2 -traceback"
