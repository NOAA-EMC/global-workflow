# global-workflow
Global Superstructure/Workflow currently supporting the Finite-Volume on a Cubed-Sphere Global Forecast System (FV3GFS)

The global-workflow depends on the following prerequisities to be available on the system:

* workload management platform / scheduler - LSF or SLURM
* workflow manager - ROCOTO (https://github.com/christopherwharrop/rocoto)
* modules - NCEPLIBS (various), esmf v8.0.0bs48, hdf5, intel/ips v18, impi v18, wgrib2, netcdf v4.7.0, hpss, gempak (see module files under /modulefiles for additional details)
* manage_externals - A utility from ESMCI to checkout external dependencies. Manage_externals can be obtained at the following address and should be in the users PATH: https://github.com/ESMCI/manage_externals

The global-workflow current supports the following machines:

* WCOSS-Dell
* WCOSS-Cray
* Hera

## Build global-workflow:

### 1. Check out components

The global-workflow uses the manage_externals utility to handle checking out its components. The manic-v1.1.8 manage_externals tag is supported. The manage_externals utility will be replacing the current checkout.sh script.

Run manage_externals (checkout_externals) while at top of clone:

```
$ checkout_externals -e Externals.cfg
```

If checkout_externals is not in your $PATH then use full path to it:

* WCOSS-Dell: /gpfs/dell2/emc/modeling/noscrub/emc.glopara/git/manage_externals/manic-v1.1.8/checkout_externals
* WCOSS-Cray: /gpfs/hps3/emc/global/noscrub/emc.glopara/git/manage_externals/manic-v1.1.8/checkout_externals
* Hera: /scratch1/NCEPDEV/global/glopara/git/manage_externals/manic-v1.1.8/checkout_externals

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
