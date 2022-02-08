#%Module#####################################################
## bufrsnd
#############################################################

module load intel/2018.4
module load impi/2018.4

module load gempak/7.5.1

module use -a /apps/contrib/NCEPLIBS/orion/modulefiles
module load hdf5_parallel/1.10.6
module load netcdf_parallel/4.7.4
module load bacio/2.0.3
module load w3nco/2.0.7
module load sigio/2.1.1
module load bufr/11.3.0
module load nemsio/2.2.4
module load w3emc_para/2.4.0

export myFC=$FCOMP   
export myFCFLAGS="-O3 -convert big_endian -traceback -g -fp-model source -qopenmp"
export myCPP=/lib/cpp
export myCPPFLAGS="-P"
