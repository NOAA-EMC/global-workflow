GFS V16.3.0 RELEASE NOTES

-------
PRELUDE
-------

The GFSv16.3 is upgraded with the following objectives: improving the internal flight planning and global aviation safety, ameliorating the snow depth prediction, and advancing the overall global analysis and forecast by improving the use of observations and adding newly available ones.

Three GFS components are upgraded to meet the objectives:

GFS Post Processing system and World Area Forecast System (WAFS):

* Per CAO milestones, WAFS products are upgraded to increase vertical/temporal resolution and forecast range on 0.25-degree products.
* Add new 1.25-degree aviation-related products with new WMO headers for FAA to retire WAFS octant files when FAA is ready.
* Minor changes in the GFS post-processing script to make a continuous bucket precipitation product for WGNE (Working Group for Numerical Experiment).
* Update synthetic satellite product generation with crtm/2.4.0.

FV3 model physics upgrade in Noah land surface model:

* GFS overestimates snow depth for heavy mixed precipitation events.
* It is found that the overestimates are associated with the GFDL microphysics scheme and the improper density used for different frozen precipitation types in the land surface model.
* Due to the scope of this upgrade, the overestimation from the surface land model is addressed first by giving proper density to various frozen hydrometeors in the land model.
* Tests indicated that the snow depth forecast has been significantly improved and the impact on overall forecast skills is not significant.

Grid-point Statistics Interpolation (GSI) Analysis - Enhance the use of observations, add new data, improve the near sea surface temperature (NSST) analysis, and bug fixes. The upgraded data types and features include:

* Feature-tracking winds from satellite:
  * Add high-latitude winds from combined geostationary and polar-orbiting satellite imagery (Leo-Geo winds)
  * Add AVHRR winds from MetOp-C
  * Revised observation error for VIIRS
* Scatterometry winds from satellite:
  * Add ASCAT winds from MetOp-C
  * Revised thinning box and observation error
* Retrieved ozone from satellite:
  * Add ozone profiles from  NOAA-20 OMPS Nadir Mapper
  * Include ozone data from the top 5 layers
* GNSS Radio Occultation:
  * Add Sentinel-6A from EUMETSAT
  * Add PAZ ROHPP (Spain)
  * Revised quality control procedures and observation operator
  * Code optimization
* Satellite Radiances:
  * Upgrade to Community Radiative Transfer Model (CRTM) v2.4.0
  * Switch from assimilating antenna temperatures (TDR) to antenna-correction temperatures (SDR) for AMSU-A, MHS, and ATMS
  * Revised all-sky assimilation framework for ATMS and AMSU-A
* Near Surface Sea Temperature Analysis:
  * Revised thinning box and quality control for AVHRR radiances
  * Relaxed quality control to include in-situ observations over the mixed surface type
  * Relaxed gross check thresholds for in-situ data
  * New in-situ observations from Saildrone, Argo, and Glider
  * New correlation length based on the first baroclinic Rossby radius of deformation
* Minimization improvement (code changes turned off in this upgrade)
  * Minimization scheme improvement to better detect nonlinearities
* Bug Fixes:
  * Variational QC for winds
  * IR quality control and bias correction
* Preparation for upcoming observations:
  * NOAA-21: ATMS CrIS, VIIRS, and OMPS
  * MetOp-C: GOME
  * GOES-18: ABI

This upgrade addresses the following bugzillas: 216, 1196, 1198, 1205, 1206, 1210, 1212, 1214, 1216, 1218, 1221, 1222

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and the NOAA-EMC and NCAR organization spaces on GitHub .com are used to manage the GFS.v16.3.0 code.  The SPA(s) handling the GFS.v16.3.0 implementation need to have permissions to clone VLab Gerrit repositories and private NCAR UPP_GTG repository. All NOAA-EMC organization repositories are publicly readable and do not require access permissions.  Please proceed with the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
mkdir gfs.v16.3.0
cd gfs.v16.3.0
git clone -b EMC-v16.3.0 https://github.com/NOAA-EMC/global-workflow.git .
cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component | Tag         | POC               |
| --------- | ----------- | ----------------- |
| MODEL     | GFS.v16.3.0   | Jun.Wang@noaa.gov |
| GLDAS     | gldas_gfsv16_release.v.2.1.0 | Helin.Wei@noaa.gov |
| GSI       | gfsda.v16.3.0 | Emily.Liu@noaa.gov |
| UFS_UTILS | ops-gfsv16.3.0 | George.Gayno@noaa.gov |
| POST      | upp_v8.2.0 | Wen.Meng@noaa.gov |
| WAFS      | gfs_wafs.v6.3.1 | Yali.Mao@noaa.gov |

Note: The library upp/8.2.0 needs to be pre-installed before proceeding with the build step.

To build all the GFS components, execute:
```bash
./build_all.sh
```
The `build_all.sh` script compiles all GFS components. Runtime output from the build for each package is written to log files in directory logs. To build an individual program, for instance, gsi, use `build_gsi.sh`.

Next, link the executables, fix files, parm files, etc in their final respective locations by executing:
```bash
./link_fv3gfs.sh nco wcoss2
```

Lastly, link the ecf scripts by moving back up to the ecf folder and executing:
```bash
cd ../ecf
./setup_ecf_links.sh
```

IMPLEMENTATION INSTRUCTIONS - SPECIFIC TO GSI
---------------------------------------------

This instruction is for ASCAT winds from MetOp-C (type 290, sub 5)

* The use flag for ASCAT winds from MetOp-C wind is currently set to -1 in `fix_gsi/global_convinfo.txt`

```
 uv    290  5  -1  3.0   0   0   0   5.0   6.1   1.4   5.0   0.000500   1   75.  1200.   0   0.   0.   0   0
```

* The Observation Processing team is preparing the data in prepbufr format.
* DA team will assess the data quality and impact as soon as they are available in the data dump.
* DA team POC (Emily.Liu@noaa.gov) will notify NCO to change the use flags from -1 to 1 (the 4th column in the rows) once the DA team completes the assessment and is satisfied with the results.

VERSION FILE CHANGES
--------------------

* `ecf/versions/gfs.ver` - change gfs_ver=v16.3, obsproc_ver=v1.1.0, crtm_ver=2.4.0, and bufr_ver=11.7.0
* `versions/build.ver` - change crtm_ver=2.4.0, change bufr_ver=11.7.0, and add ncio_ver=1.0.0
* `versions/hera.ver` - added for RDHPCS Hera support
* `versions/orion.ver` - added for RDHPCS Orion support
* `versions/run.ver` - change obsproc_ver=v1.1, crtm_ver=2.4.0, and bufr_ver=11.7.0
* `versions/wcoss2.ver` - update obsproc/prepobs versions and add tracker/fit2obs versions for dev

SORC CHANGES
------------

* GSI - highlight code changes related to the objectives of gfs.v16.3.0 upgrade; for more details, please refer to the amendment of gfs.v16.3.0 GSI code/script changes in the documentation section.
  * `sorc/gsi.fd/src/gsi`
    * Common updates:
      * `gsimod.F90`
      * `qcmod.f90`
      * `pcgsoi.f90`
      * `radinfo.f90`
      * `read_obs.F90`
      * `setuprad.f90`
    * RO updates
      * New observations: PAZ (ID 44)  and Sentinel-6 (ID 66)
      * Quality control procedures:
        * A more holistic approach for super-refraction criteria and QC over the different latitude ranges
        * Removal of Jacobian QC
        * Rejection of very large simulated bending angles (> 0.05 rad)
      * Bux fixes
        * Sign of the gradient of refractivity
        * Adjoint routine in the interpolation code - impact in very special cases, two ends of the interpolation grid
      * Code optimization
      * Code changes:
        * `lagmod.f90`
        * `setupbend.f90`
    * IR bug fixes
      * Numerical errors in surface emissivity check
      * Bias correction when air-mass bias terms are turned off
      * Data thinning
      * Code changes:
        * `read_airs.f90`
        * `read_cris.f90`
        * `read_iasi.f90`
    * MW updates
      * Switch from assimilating antenna temperature (TDR)  to antenna-corrected temperature (SDR) for AMSU-A, ATMS, and MHS
      * Include the assimilation of precipitation-affected AMSU-A and ATMS
        * Update GSI-model netcdf interface to read in precipitation hydrometeors
        * Use CRTM-2.4.0 and the cloud optical table based on GFDL cloud microphysics
        * Add the calculation of cloud effective radius
        * Add the calculation of GFDL cloud fraction for each field of view
        * Update data thinning and quality control procedures
      * Code changes:
        * `antcorr_application.f90`
        * `cloud_efr_mod.f90`
        * `clpr_gfs_ensmod.f90`
        * `crtm_interface.f90`
        * `general_read_gfatm.f90`
        * `netcdfgfs_io.f90`
        * `obsmod.F90`
        * `prt_guess.f90`
        * `read_atms.f90`
        * `read_bufrtovs.f90`
        * `read_gmi.f90`
        * `write_incr.f90`
    * Satellite wind (AMV)  updates
      * Add Leo-Geo winds
      * Add AVHRR winds from MetOp-C
      * Revised observation error for VIIRS
      * Code changes
        * `read_satwnd.f90`
        * `read_prepbufr.f90`
        * `setupw.f90`
    * Satellite scatterometry wind
      * Add ASCAT from MetOp-C
      * Data thinning with 75 km box
      * Inflate observation error from 1.5 m/s to 2.5 m/s
    * NSST updates
      * Add VIIRS radiances from NPP and N-20
      * Reduce the thinning fox from 140 km to 70 km for AVHRR and VIIRS
      * New correlation length
        * First baroclinic Rossby radius of deformation
        * Thresholds: minimum 25 km and maximum 75 km
      * Quality control
        * Exclusion of the partly clear AVHRR radiances
        * Inclusion of in-situ data over mixed surface type
        * Relaxation of gross QC check for in-situ data
      * Code changes:
        * `gsi_obOperTypeManager.F90`
        * `ncepnems_io.f90`
        * `radiance_mod.f90`
        * `read_avhrr.f90`
        * `read_nsstbufr.f90`
        * `read_viirs.f90`
        * `setthin.F90`
        * `setupsst.f90`
    * Bug fixes in variational quality control for winds
      * Code changes:
        * `setupw.f90`
        * `stepw.f90`
    * Preparation for coming observations
      * Add handling to monitor new observations from NOAA-21, GOES-18, and Himawari-9
      * Code changes:
        * `read_abi.f90`
        * `read_ahi.f90`
        * `read_virrs.f90`

* MODEL:
  * `sorc/ufs_model.fd/FV3/gfsphysics/`
    * `GFS_layer/GFS_physics_driver.F90`
    * `GFS_layer/GFS_typedefs.F90`
    * `physics/sfc_drv.f`
    * `physics/sflx.f`

* POST (UPP):
  * `sorc/gfs_ncep_post.fd/`
    * `MDL2STD_P.f`
    * `FDLVL.f`
    * `MDL2P.f`
    * `get_postfilename.f`
    * `gtg_algo.f90`
    * `CALRAD_WCLOUD_newcrtm.f`
    * `MDLFLD.f`

* UFS_UTILS:
  * Removed the following obsolete nemsio utility programs:
    * `sorc/mkgfsnemsioctl.fd`
    * `sorc/nemso_chgdate.fd`
    * `sorc/nemsio_get.fd`
    * `sorc/nemsio_read.fd`

* WAFS:
  * `sorc/wafs_awc_wafavn.fd/`
    * `makefile`
    * `waf_grib2.f90`
    * `waf_main.f90`
  * `sorc/wafs_blending_0p25.fd/`
    * `makefile`
    * `blending.f90`
  * `sorc/wafs_blending.fd/`
    * `makefile`
    * `blending.f`
  * `sorc/wafs_cnvgrib2.fd/`
    * `makefile`
  * `sorc/wafs_gcip.fd/`
    * `makefile`
    * `gcip.f90`
    * `pressure2flight.f90`
    * `satellite.f90`
  * `sorc/wafs_grib2_0p25.fd/`
    * `makefile`
    * `wafsgrib2.f90`
  * `sorc/wafs_makewafs.fd/`
    * `makefile`
  * `sorc/wafs_setmissing.fd/`
    * `makefile`

* Workflow
  * Cleanup and R&D support
    * `sorc/build_fv3.sh`
    * `sorc/build_fv3nc2nemsio.sh`
    * `sorc/build_gsi.sh`
    * `sorc/build_regrid_nemsio.sh`
    * `sorc/build_tropcy_NEMS.sh`
  * NCIO library update:
    * `sorc/build_enkf_chgres_recenter_nc.sh`
    * `sorc/enkf_chgres_recenter_nc.fd/input_data.f90`
    * `sorc/enkf_chgres_recenter_nc.fd/makefile`
    * `sorc/enkf_chgres_recenter_nc.fd/output_data.f90`
    * `sorc/enkf_chgres_recenter_nc.fd/utils.f90`
  * `sorc/gfs_bufr.fd/makefile_module`
  * `sorc/machine-setup.sh` - add support for R&Ds
  * Introduce FC/COMP variable:
    * `sorc/fbwndgfs.fd/makefile.GENERIC`
    * `sorc/supvit.fd/makefile`
    * `sorc/syndat_getjtbul.fd/makefile`
    * `sorc/syndat_maksynrc.fd/makefile`
    * `sorc/syndat_qctropcy.fd/makefile`
    * `sorc/tave.fd/makefile`
    * `sorc/vint.fd/makefile`

JOBS CHANGES
------------

* WAFS:
  * `JGFS_ATMOS_WAFS`
  * `JGFS_ATMOS_WAFS_BLENDING`
  * `JGFS_ATMOS_WAFS_BLENDING_0P25`
  * `JGFS_ATMOS_WAFS_GCIP`
  * `JGFS_ATMOS_WAFS_GRIB2`
  * `JGFS_ATMOS_WAFS_GRIB2_0P25`

* Workflow:
  * `JGFS_ATMOS_CYCLONE_TRACKER` - cleanup
  * `JGLOBAL_WAVE_PREP` - add missing "atmos" subfolder for dev

PARM/CONFIG CHANGES
-------------------

* POST (UPP):
  * Modified PARM files:
    * `post/post_avblflds.xml`
    * `post/postcntrl_gfs_wafs.xml`
    * `post/postcntrl_gfs_wafs_anl.xml`
    * `post/postxconfig-NT-GFS-WAFS-ANL.txt`
    * `post/postxconfig-NT-GFS-WAFS.txt`
  * New PARM files:
    * `post/postcntrl_gfs_wafs_ext.xml`
    * `post/postxconfig-NT-GFS-WAFS-EXT.txt`

* WAFS:
  * `parm/wafs/wafs_gcip_gfs.cfg`
  * `parm/wafs/legend`: new folder prepared for switching. When ICAO2023=yes, use parm/wafs; when ICAO2023=no, use parm/wafs/legend
  * Moved to parm/wafs/legend, from parm/wafs
    * `parm/wafs/legend/wafs_awc_wafavn.grb1.cfg`
    * `parm/wafs/legend/wafs_awc_wafavn.grb2.cfg`

* Workflow:
  * `parm/config.anal`
  * `parm/config.base.emc.dyn`
  * `parm/config.base.emc.nco.static`
  * `parm/config/config.fcst`
  * `parm/config/config.fv3.emc.dyn`
  * `parm/config/config.metp`
  * `parm/config/config.prep`
  * `parm/config/config.resources.emc.dyn`
  * `parm/config/config.resources.nco.static`
  * `parm/config/config.vrfy`
  * `parm/config/config.wafsblending0p25` - add export ICAO2023=no (change to yes later)
  * `parm/config/config.wafsgcip` - add export ICAO2023=no (change to yes later)
  * `parm/config/config.wafsgrib2` - add export ICAO2023=no (change to yes later)
  * `parm/config/config.wafsgrib20p25` - add export ICAO2023=no (change to yes later)

SCRIPT CHANGES
--------------

* GSI:
  * `scripts/exgdas_enkf_sfc.sh`
  * `scripts/exgdas_enkf_update.sh`
  * `scripts/exglobal_atmos_analysis.sh`
  * `scripts/exglobal_diag.sh`

* GLDAS:
  * `ush/gldas_forcing.sh` - fix to CPC gauge pathp1 setting

* POST (UPP):
  * `scripts/exgfs_atmos_nceppost.sh`
  * `ush/fv3gfs_downstream_nems.sh`

* WAFS:
  * `scripts/exgfs_atmos_wafs_blending_0p25.sh`
  * `scripts/exgfs_atmos_wafs_blending.sh`
  * `scripts/exgfs_atmos_wafs_gcip.sh`
  * `scripts/exgfs_atmos_wafs_grib2_0p25.sh`
  * `scripts/exgfs_atmos_wafs_grib2.sh`
  * `ush/wafs_blending.sh`

* Workflow:
  * `ush/drive_makeprepbufr.sh` - add missing $COMPONENT subfolder
  * `ush/ecflow/*` - add ecflow generator
  * `ush/load_fv3gfs_modules.sh` - add R&D support
  * `ush/rocoto/*` - bug fixes in rocoto setup scripts

FIX CHANGES
-----------

* GSI:
  * Modified:
    * `fix_gsi/global_anavinfo_l127.txt`
    * `fix_gsi/global_satinfo.txt`
    * `fix_gsi/global_convinfo.txt`
    * `fix_gsi/global_scaninfo.txt`
    * `fix_gsi/global_ozinfo.txt`
    * `fix_gsi/cloudy_radiance_info.txt`
    * `fix_gsi/prepobs_errtable.global`
    * `gdas/gdas_radmon_satype.txt`
    * `gdas/oznmon_satype.txt`
    * `gdas/radmon_scaninfo.txt`
  * New:
    * `fix_gsi/global_anavinfo_allhydro_l127.txt`
    * `fix_gsi/gfsv16_historical/gfsv16.3/global_convinfo.txt.2021091612`
    * `fix_gsi/gfsv16_historical/gfsv16.3/global_convinfo.txt.2022031612`
    * `fix_gsi/gfsv16_historical/gfsv16.3/global_satinfo.txt.2021092206`
    * `fix_gsi/gfsv16_historical/gfsv16.3/global_satinfo.txt.2021102612`

* UFS_UTILS: update NCO mode fix file install to copy instead of symlink

* WAFS:
  * `fix/wafs/legend`: new folder prepared for switching. When ICAO2023=yes, use fix/wafs; when ICAO2023=no, use fix/wafs/legend
  * New:
    * `fix/wafs/faa_gfsmaster.grb2.list`
    * `fix/wafs/gfs_master.grb2_0p25.list`
    * `fix/wafs/gfs_wafs.grb2_0p25.list`
    * `fix/wafs/grib2_gfs_awff00.45`
    * `fix/wafs/grib2_gfs_awff06.45`
    * `fix/wafs/grib2_gfs_awff09.45`
    * `fix/wafs/grib2_gfs_awff12.45`
    * `fix/wafs/grib2_gfs_awff15.45`
    * `fix/wafs/grib2_gfs_awff18.45`
    * `fix/wafs/grib2_gfs_awff21.45`
    * `fix/wafs/grib2_gfs_awff24.45`
    * `fix/wafs/grib2_gfs_awff27.45`
    * `fix/wafs/grib2_gfs_awff30.45`
    * `fix/wafs/grib2_gfs_awff33.45`
    * `fix/wafs/grib2_gfs_awff36.45`
    * `fix/wafs/grib2_gfs_awff42.45`
    * `fix/wafs/grib2_gfs_awff48.45`
    * `fix/wafs/grib2_gfs_awff54.45`
    * `fix/wafs/grib2_gfs_awff60.45`
    * `fix/wafs/grib2_gfs_awff66.45`
    * `fix/wafs/grib2_gfs_awff72.45`
  * Modified:
    * `fix/wafs/grib2_gfs_wafsf06.45`
    * `fix/wafs/grib2_gfs_wafsf09.45`
    * `fix/wafs/grib2_gfs_wafsf12.45`
    * `fix/wafs/grib2_gfs_wafsf15.45`
    * `fix/wafs/grib2_gfs_wafsf18.45`
    * `fix/wafs/grib2_gfs_wafsf21.45`
    * `fix/wafs/grib2_gfs_wafsf24.45`
    * `fix/wafs/grib2_gfs_wafsf27.45`
    * `fix/wafs/grib2_gfs_wafsf30.45`
    * `fix/wafs/grib2_gfs_wafsf33.45`
    * `fix/wafs/grib2_gfs_wafsf36.45`
    * `fix/wafs/wafs_0p25_admin_msg`
    * `fix/wafs/wafs_gfsmaster.grb2.list`
  * Moved to `fix/wafs/legend`, from `fix/wafs`:
    * `fix/wafs/legend/grib2_blended_wafs_wifs_f06.45`
    * `fix/wafs/legend/grib2_blended_wafs_wifs_f09.45`
    * `fix/wafs/legend/grib2_blended_wafs_wifs_f12.45`
    * `fix/wafs/legend/grib2_blended_wafs_wifs_f15.45`
    * `fix/wafs/legend/grib2_blended_wafs_wifs_f18.45`
    * `fix/wafs/legend/grib2_blended_wafs_wifs_f21.45`
    * `fix/wafs/legend/grib2_blended_wafs_wifs_f24.45`
    * `fix/wafs/legend/grib2_blended_wafs_wifs_f27.45`
    * `fix/wafs/legend/grib2_blended_wafs_wifs_f30.45`
    * `fix/wafs/legend/grib2_blended_wafs_wifs_f33.45`
    * `fix/wafs/legend/grib2_blended_wafs_wifs_f36.45`
    * `fix/wafs/legend/grib2_gfs_wafs_wifs_f06.45`
    * `fix/wafs/legend/grib2_gfs_wafs_wifs_f09.45`
    * `fix/wafs/legend/grib2_gfs_wafs_wifs_f12.45`
    * `fix/wafs/legend/grib2_gfs_wafs_wifs_f15.45`
    * `fix/wafs/legend/grib2_gfs_wafs_wifs_f18.45`
    * `fix/wafs/legend/grib2_gfs_wafs_wifs_f21.45`
    * `fix/wafs/legend/grib2_gfs_wafs_wifs_f24.45`
    * `fix/wafs/legend/grib2_gfs_wafs_wifs_f27.45`
    * `fix/wafs/legend/grib2_gfs_wafs_wifs_f30.45`
    * `fix/wafs/legend/grib2_gfs_wafs_wifs_f33.45`
    * `fix/wafs/legend/grib2_gfs_wafs_wifs_f36.45`
    * `fix/wafs/legend/wafs_admin_msg`
    * `fix/wafs/legend/wafs_gfsmaster.grb2_0p25.list`
  * Removed:
    * `fix/wafs/wafs_gfsmaster.grb1.list`

* Workflow:
  * Updated fix set: `fix_nco_gfsv16.3.0`
  * Corrected storm names:
    * `fix_am/syndat_stmnames`
  * Modified to correct some buoy coordinates:
    * `wave_gfs.buoys`
    * `wave_gfs.buoys.dat`
    * `wave_gfs.buoys.full`

MODULE CHANGES - related to WCOSS2
----------------------------------

* GSI:
  * `modulefiles/gsi_common.lua`
  * `modulefiles/modulefile.ProdGSI.wcoss2.lua`
* MODEL
  * `modulefiles/wcoss2/fv3`
* POST (UPP):
  * `scripts/exgfs_atmos_nceppost.sh`
  * `ush/fv3gfs_downstream_nems.sh`
* UFS_UTILS:
  * `modulefiles/fv3gfs/global_cycle.wcoss2.lua` - add a load of the hdf5 and zlib libraries
* Workflow:
  * `modulefiles/fv3gfs/enkf_chgres_recenter.wcoss2.lua` - remove hdf5/netcdf load
  * `modulefiles/fv3gfs/enkf_chgres_recenter_nc.wcoss2.lua` - add ncio load
  * `modulefiles/gfs_fbwndgfs.wcoss2.lua` - remove nemsio/sigio, add myFC setting
  * `modulefiles/module_base.wcoss2.lua` - add prepobs load (dev use only)
  * `modulefiles/modulefile.storm_reloc_v6.0.0.wcoss2.lua` - change `FC` to `myFC`

CHANGES TO FILE SIZES
-----------------------------------

* UPP (post): increased by 15G
* WAFS: increased by 2.5G

ENVIRONMENT AND RESOURCE CHANGES
--------------------------------

* New use of MPMD (load CFP at runtime):
  * `jgfs_atmos_wafs_grib2`
  * `jgfs_atmos_wafs_grib2_0p25`
* Runtime change:
  * `jgfs_atmos_wafs_grib2` - decrease from 15 minutes to 2.5 minutes
* Core number adjustments:
  * `jgfs_atmos_wafs_grib2` - from 1 to 18
  * `jgfs_atmos_wafs_grib2_0p25` - from 1 to 11
* Memory adjustments:
  * `jenkfgdas_diag` - from 24GB to 28GB
  * `jenkfgdas_sfc` - from 60GB to 80GB
  * `jgdas_atmos_gempak` - from 4GB to 20GB
  * `jgdas_atmos_gempak_meta_ncdc` - from 1GB to 20GB
  * `jgdas_atmos_emcsfc_sfc_prep` - from 2GB to 4GB
  * `jgdas_atmos_verfrad` - from 5GB to 10GB
  * `jgfs_atmos_gempak` - from 2GB to 200GB
  * `jgfs_atmos_gempak_meta` - from 2GB to 200GB
  * `jgfs_atmos_emcsfc_sfc_prep` - from 2GB to 4GB
  * `jgfs_atmos_awips_g2` - from 3GB to 10GB
  * `jgfs_atmos_wafs_blending_0p25` - from 1GB to 15GB
  * `jgfs_atmos_wafs_grib2` - from 5GB to 80GB
  * `jgfs_atmos_wafs_grib2_0p25` - from 1GB to 80GB
  * `jgfs_atmos_wafs_master` - from 1GB to 5GB
  * `jgfs_wave_gempak` - from 1GB to 10GB
  * `jgfs_wave_postsbs` - from 10GB to 40GB
  * `jgfs_wave_prdgen_gridded` - from 1GB to 2GB
  * `jgfs_wave_prep` - from 150GB to 220GB

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * The entire GFS v16.3.0 package needs to be installed and tested on WCOSS-2
* Does this change require a 30-day evaluation?
  * Yes

DISSEMINATION INFORMATION
-------------------------

* POST (UPP)
  * Where should this output be sent?
    * Disseminate new data file `gfs.t{CYC}z.wgne.f{HHH}` to operational gfs NOMADS directory
  * Who are the users?
    * Same as current operations plus WGNE users
  * Which output files should be transferred from PROD WCOSS to DEV WCOSS?
    * Same as current operational GFS, plus new WGNE data file
  * Directory changes
    * No changes
  * File changes
    * The POST (UPP) file changes can be referred to in this document: https://docs.google.com/spreadsheets/d/1ZV6Cr4qEt7-V-UAZjoCfwUJdhUyAnN7AMQrb-9lfGb4/edit#gid=0

* WAFS
  * Where should this output be sent?
    * WAFS product dissemination updates are documented: GFS_V16.3_WAFS_dbn_alert (https://docs.google.com/spreadsheets/d/1WvbTGFQVJG8oavl9Jmn6XuigUbxBi9AzOqtpSjA0Un8/edit?usp=sharing). New products or modifications are marked in red.
  * Who are the users?
    * Same as current operations WAFS users
  * Which output files should be transferred from PROD WCOSS to DEV WCOSS?
    * Same as current operational GFS, plus new files:
      * `gfs.tCCz.awf_grb45fFF.grib2`
      * `gfs.tCCz.awf_grb45fFF.grib2.idx`
      * `wmo/grib2.tCCz.awf_grbfFF.45`
      * `gfs.tCCz.awf_0p25.fFFF.grib2` (renamed from `gfs.tCCz.wafs_0p25.fFFF.grib2` in GFSv16.2)
      * `gfs.tCCz.awf_0p25.fFFF.grib2.idx`
      * `gfs.tCCz.wafs_0p25.fFFF.grib2 (new but named as a different product in GFSv16.2)
      * `gfs.tCCz.wafs_0p25.fFFF.grib2.idx`
  * Directory changes
    * No changes
  * File changes
    * The WAFS file changes can be referred to in this document: https://docs.google.com/spreadsheets/d/1dRnoTJZLtRPczFGFwSeIsQC8Tqr7bd47dYOuZdaEdi0/edit?usp=sharing

HPSS ARCHIVE
------------

* WAFS:
  * File name changed: Archive `gfs.tCCz.awf_0p25.fFFF.grib2` instead of archiving `gfs.tCCz.wafs_0p25.fFFF.grib2`
  * New Files: `gfs.tCCz.awf_grb45fFF.grib2`, 65M per cycle

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------

* No changes from GFS v16.2.2

DOCUMENTATION
-------------

* GFS.V16.3.0 Implementation Kick-off Meeting Slides: https://docs.google.com/presentation/d/1HIY8bB3YEj-DThUHnJPak8l0gA7lfOtT7nMpgY4yvB4/edit?usp=sharing
* Amendment of gfs.v16.3.0 GSI Code/Script Changes: https://docs.google.com/document/d/1Krg09tSGXpf4QHGFPwuEUpdMCxBVY6i_otRfSEzrrhs/edit?usp=sharing
* MODEL Documentation: https://docs.google.com/presentation/d/1FOeP3VZK7wbzOXdEqPJa8WbAbVJgHX-Icc235OOsXsc/edit?usp=sharing
* MODEL Verification: https://www.emc.ncep.noaa.gov/mmb/gcp/gfs/gfsv16c/
* GitHub Global Workflow Issue for GFS.V16.3.0 Implementation: https://github.com/NOAA-EMC/global-workflow/issues/744
* GitHub Issue for Real-time Parallel: https://github.com/NOAA-EMC/global-workflow/issues/952
* GitHub Issue for Retrospective Parallel: https://github.com/NOAA-EMC/global-workflow/issues/951

PREPARED BY
-----------
Kate.Friedman@noaa.gov
Lin.Gan@noaa.gov
George.Gayno@noaa.gov
Emily.Liu@noaa.gov
Rahul.Mahajan@noaa.gov
Yali.Mao@noaa.gov
Wen.Meng@noaa.gov
Russ.Treadon@noaa.gov
Jun.Wang@noaa.gov
Helin.Wei@noaa.gov
