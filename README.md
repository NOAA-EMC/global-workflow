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
* Orion

Quick-start instructions are below. Full instructions are available in the [wiki](https://github.com/NOAA-EMC/global-workflow/wiki/Run-Global-Workflow)

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

Or use an available option:  
```
build_all.sh [-a UFS_app][-c build_config][-h][-v]
  -a UFS_app:
    Build a specific UFS app instead of the default
  -c build_config:
    Selectively build based on the provided config instead of the default config
  -h:
    Print usage message and exit
  -v:
    Run all scripts in verbose mode
```

### 3. Link components

While in /sorc folder:

$ sh link_workflow.sh emc $MACHINE
...where $MACHINE is "dell", "cray", "hera", or "orion".
