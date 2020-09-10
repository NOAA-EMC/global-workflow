# global-workflow
Global Superstructure/Workflow currently supporting the Finite-Volume on a Cubed-Sphere Global Forecast System (FV3GFS)

The global-workflow depends on the following prerequisities to be available on the system:

* workload management platform / scheduler - LSF or SLURM
* workflow manager - ROCOTO (https://github.com/christopherwharrop/rocoto)
* modules - NCEPLIBS (various), esmf v8.0.0bs48, hdf5, intel/ips v18, impi v18, wgrib2, netcdf v4.7.0, hpss, gempak (see module files under /modulefiles for additional details)

The global-workflow current supports the following machines:

* WCOSS-Dell
* WCOSS-Cray
* Hera

## Build global-workflow:

### 1. Check out components

While in /sorc folder:
```
$ sh checkout.sh
```

### 2. Build components

While in /sorc folder:
```
$ sh build_all.sh
```

### 3. Link components

While in /sorc folder:
```
$ sh link_fv3gfs.sh emc $MACHINE
```

...where $MACHINE is "dell", "cray", or "hera".
