GFS V16.2.0 RELEASE NOTES

-------
PRELUDE
-------

The GFSv16 is ported to the new WCOSS2 system (Cactus/Dogwood).

IMPLEMENTATION INSTRUCTIONS
---------------------------

The NOAA VLab and both the NOAA-EMC and NCAR organization spaces on GitHub.com are used to manage the GFS.v16.2.0 code. The SPA(s) handling the GFS.v16.2.0 implementation need to have permissions to clone VLab gerrit repositories and the private NCAR UPP_GTG repository. All NOAA-EMC organization repositories are publicly readable and do not require access permissions. Please follow the following steps to install the package on WCOSS2:

```bash
cd $PACKAGEROOT
mkdir gfs.v16.2.0
cd gfs.v16.2.0
git clone -b EMC-v16.2.0.1 https://github.com/NOAA-EMC/global-workflow.git .
cd sorc
./checkout.sh -o
```

The checkout script extracts the following GFS components:

| Component | Tag         | POC               |
| --------- | ----------- | ----------------- |
| MODEL     | GFS.v16.2.0   | Jun.Wang@noaa.gov |
| GSI       | gfsda.v16.2.0 | Russ.Treadon@noaa.gov |
| GLDAS     | gldas_gfsv16_release.v1.24.0 | Helin.Wei@noaa.gov |
| UFS_UTILS | ops-gfsv16.2.0 | George.Gayno@noaa.gov |
| POST      | upp_v8.1.0 | Wen.Meng@noaa.gov |
| WAFS      | gfs_wafs.v6.2.7 | Yali.Mao@noaa.gov |

To build all the GFS components, execute:
```bash
./build_all.sh
```
The `build_all.sh` script compiles all GFS components. Runtime output from the build for each package is written to log files in directory logs. To build an individual program, for instance, gsi, use `build_gsi.sh`.

Next, link the executables, fix files, parm files etc in their final respective locations by executing:
```bash
./link_fv3gfs.sh nco wcoss2
```

Lastly, link the ecf scripts by moving back up to the ecf folder and executing:
```bash
cd ../ecf
./setup_ecf_links.sh
```

SORC CHANGES
------------

Necessary code changes for WCOSS2:

* GSI:
  * `src/gsi/ensctl2state.f90`: Nullify pointer declarations
  * `src/gsi/ensctl2state_ad.f90`: Nullify pointer declarations
  * `src/gsi/general_read_gfsatm.f90`: Bug fix to remove MPI rank issue that causes `global_gsi.x` to hang while using 1020 or more tasks.
  * `src/gsi/read_obs.F90`: Remove verbose output causing a WARNING message to appear in the output.
  * `src/enkf/mpi_readobs.f90`: Remove omp parallel, since they didn’t speed up the code and caused errors on WCOSS2.

All components updated their codes to build on WCOSS2:

* GLDAS
  * `sorc/build_gdas2gldas.sh`
  * `sorc/build_gldas2gdas.sh`
  * `sorc/build_gldas_forcing.sh`
  * `sorc/build_gldas_model.sh`
  * `sorc/build_gldas_post.sh`
  * `sorc/build_gldas_rst.sh`
  * `sorc/gldas_model.fd/make/Makefile.noah`
* MODEL
  * `conf/configure.fv3.wcoss2` - created
* GSI
  * `ush/build_all_cmake.sh`
* POST
  * `Externals.cfg` and `manage_externals` directory were removed - retire usage of `manage_externals`, use `git submodule` for GTG subcomponent now
  * `sorc/build_ncep_post.sh`
  * `sorc/ncep_post.fd/build_upp_lib.sh`
  * `sorc/ncep_post.fd/makefile_lib`
  * `sorc/ncep_post.fd/makefile_lib_wcoss2`
  * `sorc/ncep_post.fd/makefile_module_wcoss2`
* UFS_UTILS
  * `sorc/build_cycle.sh`
  * `sorc/build_emcsfc.sh`
  * `sorc/link_fixdirs.sh`
  * `sorc/machine-setup.sh`
  * `sorc/ufs_build.cfg` - disable chgres build
* WAFS
  * `sorc/build_wafs.sh`
* Workflow - WCOSS2 and LUA module support changes throughout. Additional changes described below.
  * `sorc/build_all.sh`
  * `sorc/build_enkf_chgres_recenter.sh`
  * `sorc/build_enkf_chgres_recenter_nc.sh`
  * `sorc/build_fv3.sh` - add Orion support and consolidate compile commands
  * `sorc/build_fv3nc2nemsio.sh`
  * `sorc/build_gaussian_sfcanl.sh`
  * `sorc/build_gfs_bufrsnd.sh`
  * `sorc/build_gfs_fbwndgfs.sh`
  * `sorc/build_regrid_nemsio.sh`
  * `sorc/build_tropcy_NEMS.sh` - updated some library variable settings for hpc-stack
  * `sorc/checkout.sh` - update component tags and adjust how GTG checkout occurs (Using `git submodules` now, no longer using `manage_externals`)
  * `sorc/enkf_chgres_recenter_nc.fd/makefile` - hpc-stack library variable updates
  * `sorc/fv3gfs_build.cfg` - remove duplicate gldas line
  * `sorc/gaussian_sfcanl.fd/Makefile` - add CWD to line that needed it
  * `sorc/gaussian_sfcanl.fd/makefile.sh` - hpc-stack library variable update
  * `sorc/link_fv3gfs.sh` - add support for Orion, remove `global_chgres` exec link, add new emc/nco mode config check
  * `sorc/machine-setup.sh` - add support for Orion, add `build.ver` sourcing
  * `util/sorc/compile_gfs_util_wcoss.sh`
  * `util/sorc/mkgfsawps.fd/makefile.wcoss2` - created
  * `util/sorc/mkgfsawps.fd/compile_mkgfsawps_wcoss.sh`
  * `util/sorc/overgridid.fd/compile_overgridid_wcoss.sh`
  * `util/sorc/rdbfmsua.fd/makefile.wcoss2` - created
  * `util/sorc/rdbfmsua.fd/compile_rdbfmsua_wcoss.sh`
  * `util/sorc/webtitle.fd/compile_webtitle_wcoss.sh`

FIX CHANGES
-----------

* No changes from GFS v16.1.5

PARM/CONFIG CHANGES
-------------------

* Workflow
  * `env/WCOSS2.env` - created
  * `parm/config/config.anal` - tiny whitespace cleanup
  * `parm/config/config.base.emc.dyn` - a few QOL and Orion support updates for `dev`, also `jlogfile` removal, change `NWPROD` to `PACKAGEROOT`
  * `parm/config/config.base.nco.static` - change default `machine` to `WCOSS2`, change `NWPROD` to `PACKAGEROOT`
  * `parm/config/config.efcs` - update chunk settings for WCOSS2
  * `parm/config/config.fcst` - update chunk settings for WCOSS2 and set `io_layout="1,1"` when not C768
  * `parm/config/config.fv3.emc.dyn` - created to handle dev resource settings outside of ops for the FV3 dynamical core, link script picks when entered mode is “emc”
  * `parm/config/config.fv3.nco.static` - formerly just `config.fv3`, contains resource settings for the FV3 in operations, link script picks when entered mode is “nco”
  * `parm/config/config.gldas` - add needed `FINDDATE` setting
  * `parm/config/config.prepbufr` - `GESROOT` for Orion
  * `parm/config/config.resources.emc.dyn` - formerly known as `config.resources`, sets dev resource settings for running outside of ops, link script picks when entered mode is “emc”
  * `parm/config/config.resources.nco.static` - created to handle ops resource settings, link script picks when entered mode is “nco”
  * `parm/config/config.vrfy` - some dev updates
  * `parm/config/config.wavepostbndpnt` - update to set `FHMAX_WAV_IBP=$FHMAX_GFS` when `$FHMAX_GFS < 180`
  * `versions/build.ver` - created for WCOSS2 port
  * `versions/run.ver` - created for WCOSS2 port

JOBS CHANGES
------------

All job scripts listed here made the following changes:

* Remove `postmsg`/`jlogfile` usage and references
* Add WCOSS2 checks where needed

Other job script changes are noted under component bullets:

* GLDAS
  * `jobs/JGDAS_ATMOS_GLDAS`
  * `ush/gldas_forcing.sh` - introduce `USE_CFP` and `cpc_precip` for EMC mode
  * `ush/gldas_get_data.sh`
* GSI - The following job scripts were also updated to convert them from `ksh` to `bash`. A correction was also made to `JGDAS_ENKF_FCST` to fix `bash` treating `08` as an `octal`.
  * `jobs/JGDAS_ATMOS_ANALYSIS_DIAG`
  * `jobs/JGDAS_ATMOS_CHGRES_FORENKF`
  * `jobs/JGDAS_ENKF_DIAG`
  * `jobs/JGDAS_ENKF_ECEN`
  * `jobs/JGDAS_ENKF_FCST`
  * `jobs/JGDAS_ENKF_POST`
  * `jobs/JGDAS_ENKF_SELECT_OBS`
  * `jobs/JGDAS_ENKF_SFC`
  * `jobs/JGDAS_ENKF_UPDATE`
  * `jobs/JGLOBAL_ATMOS_ANALYSIS`
  * `jobs/JGLOBAL_ATMOS_ANALYSIS_CALC`
* MODEL
  * See main forecast job script (`JGLOBAL_FORECAST`) listed under workflow below
* POST
  * `jobs/JGLOBAL_ATMOS_NCEPPOST`
* WAFS
  * `jobs/JGFS_ATMOS_WAFS`
  * `jobs/JGFS_ATMOS_WAFS_BLENDING`
  * `jobs/JGFS_ATMOS_WAFS_BLENDING_0P25`
  * `jobs/JGFS_ATMOS_WAFS_GCIP`
  * `jobs/JGFS_ATMOS_WAFS_GRIB2`
  * `jobs/JGFS_ATMOS_WAFS_GRIB2_0P25`
* Workflow
  * `jobs/JGDAS_ATMOS_GEMPAK`
  * `jobs/JGDAS_ATMOS_GEMPAK_META_NCDC`
  * `jobs/JGFS_ATMOS_AWIPS_20KM_1P0DEG`
  * `jobs/JGFS_ATMOS_AWIPS_G2`
  * `jobs/JGFS_ATMOS_CYCLONE_GENESIS`
  * `jobs/JGFS_ATMOS_CYCLONE_TRACKER`
  * `jobs/JGFS_ATMOS_FBWIND`
  * `jobs/JGFS_ATMOS_FSU_GENESIS`
  * `jobs/JGFS_ATMOS_GEMPAK`
  * `jobs/JGFS_ATMOS_GEMPAK_META`
  * `jobs/JGFS_ATMOS_GEMPAK_NCDC_UPAPGIF`
  * `jobs/JGFS_ATMOS_GEMPAK_PGRB2_SPEC`
  * `jobs/JGFS_ATMOS_PGRB2_SPEC_NPOESS`
  * `jobs/JGFS_ATMOS_POSTSND`
  * `jobs/JGLOBAL_ATMOS_EMCSFC_SFC_PREP`
  * `jobs/JGLOBAL_ATMOS_TROPCY_QC_RELOC`
  * `jobs/JGLOBAL_FORECAST`
  * `jobs/JGLOBAL_WAVE_GEMPAK`
  * `jobs/JGLOBAL_WAVE_INIT`
  * `jobs/JGLOBAL_WAVE_POST_BNDPNT`
  * `jobs/JGLOBAL_WAVE_POST_BNDPNTBLL`
  * `jobs/JGLOBAL_WAVE_POST_PNT`
  * `jobs/JGLOBAL_WAVE_POST_SBS`
  * `jobs/JGLOBAL_WAVE_PRDGEN_BULLS`
  * `jobs/JGLOBAL_WAVE_PRDGEN_GRIDDED`
  * `jobs/JGLOBAL_WAVE_PREP`
  * `jobs/rocoto/awips.sh`
  * `jobs/rocoto/gempak.sh`
  * `jobs/rocoto/vrfy.sh`
  * `jobs/rocoto/wafsgcip.sh`
  * Remove duplicate rundir deletions:
    * `jobs/rocoto/waveinit.sh`
    * `jobs/rocoto/wavepostbndpnt.sh`
    * `jobs/rocoto/wavepostbndpntbll.sh`
    * `jobs/rocoto/wavepostpnt.sh`
    * `jobs/rocoto/wavepostsbs.sh`
    * `jobs/rocoto/waveprep.sh`

SCRIPT CHANGES
--------------

The following scripts were updated for WCOSS2 (includes postmsg/jlogfile removal - other changes mentioned below):

* GLDAS
  * `scripts/exgdas_atmos_gldas.sh` - replace `WCOSS_DELL_P3` machine check with `$USE_CFP` for CFP usage
* GSI (shell scripts also converted from `ksh` to `bash`)
  * `scripts/exgdas_atmos_chgres_forenkf.sh`
  * `scripts/exgdas_enkf_ecen.sh`
  * `scripts/exgdas_enkf_fcst.sh`
  * `scripts/exgdas_enkf_post.sh`
  * `scripts/exgdas_enkf_select_obs.sh`
  * `scripts/exgdas_enkf_sfc.sh`
  * `scripts/exgdas_enkf_update.sh`
  * `scripts/exglobal_atmos_analysis.sh` - also refactor for loop limits in `GENDIAGS` block.
  * `scripts/exglobal_atmos_analysis_calc.sh`
  * `scripts/exglobal_diag.sh`
  * `ush/calcanl_gfs.py` - add `mpiexec` launcher option
* MODEL
  * See main forecast driver script (`exglobal_forecast.sh`) listed under workflow below
* POST
  * `scripts/exgdas_atmos_nceppost.sh`
  * `scripts/exgfs_atmos_nceppost.sh`
  * `ush/fv3gfs_downstream_nems.sh`
* UFS_UTILS
  * `scripts/exemcsfc_global_sfc_prep.sh`
  * `ush/emcsfc_ice_blend.sh`
  * `ush/emcsfc_snow.sh`
* WAFS
  * `scripts/exgfs_atmos_wafs_blending.sh`
  * `scripts/exgfs_atmos_wafs_blending_0p25.sh`
  * `scripts/exgfs_atmos_wafs_gcip.sh`
  * `scripts/exgfs_atmos_wafs_grib.sh`
  * `scripts/exgfs_atmos_wafs_grib2.sh`
  * `scripts/exgfs_atmos_wafs_grib2_0p25.sh`
  * `ush/mkwfsgbl.sh`
  * `ush/wafs_blending.sh`
  * `ush/wafs_intdsk.sh`
* Workflow
  * `driver/*` - Remove `jlogfile` references from driver scripts and add new wcoss2 driver scripts
  * `gempak/ush/gempak_gdas_f000_gif.sh`
  * `gempak/ush/gempak_gfs_f00_gif.sh`
  * `gempak/ush/gempak_gfs_f12_gif.sh`
  * `gempak/ush/gempak_gfs_f24_gif.sh`
  * `gempak/ush/gempak_gfs_f36_gif.sh`
  * `gempak/ush/gempak_gfs_f48_gif.sh`
  * `ush/gfs_bufr.sh`
  * `ush/gfs_sndp.sh`
  * `scripts/exgdas_atmos_gempak_gif_ncdc.sh`
  * `scripts/exgdas_atmos_nawips.sh`
  * `scripts/exgfs_atmos_awips_20km_1p0deg.sh`
  * `scripts/exgfs_atmos_fbwind.sh`
  * `scripts/exgfs_atmos_gempak_gif_ncdc_skew_t.sh`
  * `scripts/exgfs_atmos_gempak_meta.sh`
  * `scripts/exgfs_atmos_goes_nawips.sh`
  * `scripts/exgfs_atmos_grib2_special_npoess.sh`
  * `scripts/exgfs_atmos_grib_awips.sh`
  * `scripts/exgfs_atmos_nawips.sh`
  * `scripts/exgfs_atmos_postsnd.sh`
  * `scripts/exgfs_wave_init.sh`
  * `scripts/exgfs_wave_nawips.sh`
  * `scripts/exgfs_wave_post_gridded_sbs.sh`
  * `scripts/exgfs_wave_post_pnt.sh`
  * `scripts/exgfs_wave_prdgen_bulls.sh`
  * `scripts/exgfs_wave_prdgen_gridded.sh`
  * `scripts/exgfs_wave_prep.sh`
  * `scripts/exglobal_atmos_tropcy_qc_reloc.sh`
  * `scripts/exglobal_forecast.sh` - update `$NWPROD` to `$PACKAGEROOT`
  * `ush/gaussian_sfcanl.sh` - update `$NWPROD` to `$PACKAGEROOT`
  * `ush/gfs_bufr.sh` - replace backticks with proper syntax
  * `ush/gfs_sndp.sh` - replace backticks with proper syntax
  * `ush/global_extrkr.sh`
  * `ush/load_fv3gfs_modules.sh` - update to source run.ver and load `module_base` in LUA format
  * `ush/rocoto/*` - numerous updates to rocoto setup scripts to support WCOSS2 and Orion, also scripts converted from `python2` to `python3`
  * `ush/syndat_getjtbul.sh`
  * `ush/syndat_qctropcy.sh`
  * `ush/tropcy_relocate.sh`
  * `ush/wave_grib2_sbs.sh`
  * `ush/wave_grid_interp.sh`
  * `ush/wave_grid_interp_sbs.sh`
  * `ush/wave_grid_moddef.sh`
  * `ush/wave_outp_cat.sh`
  * `ush/wave_outp_spec.sh`
  * `ush/wave_prnc_cur.sh`
  * `ush/wave_prnc_ice.sh`
  * `ush/wave_tar.sh`
  * `util/ush/mkawpgrb.sh`
  * `util/ush/ml7_slm30g.sh`
  * `util/ush/snd2forgn`
  * `util/ush/snd2forgntbl.sh`
  * `util/ush/sndncdc`

MODULE CHANGES
--------------

The following new WCOSS2 modulefiles were created and updated to conform with LUA format, as well as use new `*_ver` variables set by app-level build.ver versions files:

* GLDAS
  * `modulefiles/gdas2gldas.wcoss2.lua`
  * `modulefiles/gldas2gdas.wcoss2.lua`
  * `modulefiles/gldas_forcing.wcoss2.lua`
  * `modulefiles/gldas_model.wcoss2.lua`
  * `modulefiles/gldas_post.wcoss2.lua`
  * `modulefiles/gldas_rst.wcoss2.lua`
* GSI
  * `modulefiles/modulefile.ProdGSI.wcoss2.lua`
* MODEL
  * `modulefiles/wcoss2/fv3` - not LUA format, given waiver
* POST
  * `modulefiles/post/post_wcoss2.lua`
  * `modulefiles/upp/upp_wcoss2.lua`
* UFS_UTILS
  * `modulefiles/fv3gfs/global_cycle.wcoss2.lua`
  * `modulefiles/modulefile.global_emcsfc_ice_blend.wcoss2.lua`
  * `modulefiles/modulefile.global_emcsfc_snow2mdl.wcoss2.lua`
* WAFS
  * `modulefiles/wafs/wafs_v6.0.0-wcoss2.lua`
* Workflow
  * modulefiles to support Orion also included
  * `modulefiles/fv3gfs/enkf_chgres_recenter.wcoss2.lua`
  * `modulefiles/fv3gfs/enkf_chgres_recenter_nc.wcoss2.lua`
  * `modulefiles/fv3gfs/gaussian_sfcanl.wcoss2.lua`
  * `modulefiles/gfs_bufr.wcoss2.lua`
  * `modulefiles/gfs_fbwndgfs.wcoss2.lua`
  * `modulefiles/module-setup.csh.inc`
  * `modulefiles/module-setup.sh.inc`
  * `modulefiles/module_base.wcoss2.lua`
  * `modulefiles/modulefile.fv3nc2nemsio.wcoss2.lua`
  * `modulefiles/modulefile.regrid_nemsio.wcoss2.lua`
  * `modulefiles/modulefile.storm_reloc_v6.0.0.wcoss2.lua`
  * `util/modulefiles/gfs_util.wcoss2.lua`

CHANGES TO RESOURCES AND FILE SIZES
-----------------------------------

* File sizes
  * No change to GFSv16.1.5.
* Resource changes to meet operational time windows:
  * See updated Ecflow scripts for adjusted compute resources for WCOSS2.
  * Pre-hand-off development testing results:
    * Timing compared to WCOSS-Dell - Almost all jobs ran either within the +/-5min window or faster (exceptions noted below). Most serial jobs were faster. The largest MPI jobs were within their WCOSS-Dell windows and some even use fewer cores.
    * Cores compared to WCOSS-Dell - On average the jobs used ~30% more cores in development testing, mainly as a result of getting jobs into timing windows. Optimization can improve this, particularly with the smaller jobs that could share nodes. See additional note on this below.
    * A spreadsheet with development testing timings and resource settings is available upon request.
  * Notes on important resource changes:
    * Almost all 1 node jobs are serial and will use more cores due to the larger number of pes per node on WCOSS2 but they will also run considerably faster. Further optimization could improve this.
    * The wave post point jobs all require about double the number of cores compared to WCOSS-Dell. There are also timing issues partly related to runtime variability on WCOSS2. Further optimization may improve these jobs.
    * The `gdas[gfs]_analysis` jobs used just under double the number of cores but are snug in their WCOSS-Dell timing window. Further optimization could reduce the core number but retail timing.
    * The `gfs_forecast` job runtime is fast but just under the WCOSS-Dell core number. Further optimization is needed to get it back into its WCOSS-Dell window.

PRE-IMPLEMENTATION TESTING REQUIREMENTS
---------------------------------------

* Which production jobs should be tested as part of this implementation?
  * The entire GFS v16.2.0 package needs to be installed and tested.
* Does this change require a 30-day evaluation?
  * Yes.

DISSEMINATION INFORMATION
-------------------------

* Where should this output be sent?
  * No change from GFS v16.1.5
* Who are the users?
  * No change from GFS v16.1.5
* Which output files should be transferred from PROD WCOSS to DEV WCOSS?
  * No change from GFS v16.1.5
* Directory changes
  * No change from GFS v16.1.5
* File changes
  * No change from GFS v16.1.5

HPSS ARCHIVE
------------

* No change from GFS v16.1.5

JOB DEPENDENCIES AND FLOW DIAGRAM
---------------------------------
* No change from GFS v16.1.5
