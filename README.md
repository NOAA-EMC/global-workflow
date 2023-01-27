# global-workflow
Global Workflow currently supporting the Global Forecast System (GFS) with the UFS-weather-model and GSI-based Data Assimilation System.

The global-workflow depends on the following prerequisities to be available on the system:

* workload management platform / scheduler - LSF or SLURM
* workflow manager - ROCOTO (https://github.com/christopherwharrop/rocoto)
* modules - NCEPLIBS (various), esmf v8.0.0bs48, hdf5, intel/ips v18, impi v18, wgrib2, netcdf v4.7.0, hpss, gempak (see module files under /modulefiles for additional details)

The global-workflow current supports the following tier-1 machines:

* Hera
* Orion
* WCOSS2 - NOAA's operational HPC

Additionally, the following tier-2 machine is supported:
* S4 (Note that S2S+ experiments are not fully supported)

Documentation (in progress) is available [here](https://global-workflow.readthedocs.io/en/latest/)

