#%Module#####################################################
## bufrsnd
#############################################################

# Loading Intel Compiler Suite
module load intel
module load impi
module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
# Loading nceplibs modules
module load sigio/2.1.1
module load bacio/2.0.3
module load w3nco/2.0.6
module load bufr/11.3.0
module load nemsio/2.2.3
module load intelpython/3.6.8
module load w3emc/2.3.1

module use /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load hdf5_parallel/1.10.6
module load netcdf_parallel/4.7.4
#module load w3emc_para/2.4.0          


export myFC=$FCOMP
export myFCFLAGS="-O3 -convert big_endian -traceback -g -fp-model source -qopenmp"
export myCPP=/lib/cpp
export myCPPFLAGS="-P"
