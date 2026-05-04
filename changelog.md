# Changelog

## [Unreleased]

### 2026-04-23

#### Added — Forecast Manager System

Implements a dedicated forecast manager job (`fcst_manager`) that copies GFS
forecast products to COM in real time as the model writes them, removing
the dead pre-run symlinks (`DATAoutput → COM`) that caused issues on
restart/rerun.

**New files:**
- `ush/forecast_manager.sh` — `fcst_mgr_wait_and_copy` function that polls
  `DATAoutput` for per-file sentinel log files and copies data+log to COM
  with the correct ordering (data first, sentinel last).
- `dev/jobs/JGLOBAL_FORECAST_MGR` — Manager job script. Shares the
  `DATAjob` umbrella directory with `JGLOBAL_FORECAST`. Waits for product
  tables in `COMOUT_CONF`, then runs ATM and WW3 manager loops concurrently.
- `dev/job_cards/rocoto/fcst_manager.sh` — Rocoto job card for `fcst_manager`.

**Modified files:**
- `ush/forecast_postdet.sh`:
  - `FV3_postdet`: For GFS/GEFS/SFS/GCAFS, replaces pre-run NLN symlinks
    with product table entries written to `DATA/atm_products.txt` and
    published to `COMOUT_CONF`. GDAS/enkfGDAS behavior unchanged (NLN).
  - `WW3_postdet`: Same treatment for WW3 gridded and point output tables
    (`ww3_products.txt`). `log.ww3` becomes a real file in DATA for GFS.
  - `MOM6_postdet`: Removes NLN for GFS/GEFS/SFS/GCAFS; keeps NLN for
    GDAS/enkfGDAS (analysis jobs read ocean backgrounds during the run).
  - `CICE_postdet`: Same as MOM6.
  - `MOM6_out`: Adds end-of-job `cpfs` for MOM6 history files for
    GFS/GEFS/SFS/GCAFS.
  - `CICE_out`: Adds end-of-job `cpfs` for CICE history files and
    `iceh_ic` for GFS/GEFS/SFS/GCAFS.
  - `WW3_out`: Adds end-of-job `cpfs` for `log.ww3` for GFS/GEFS/SFS/GCAFS.
- `dev/parm/config/gfs/config.resources`: Added `fcst_manager` resource block
  (2 tasks, 12 h walltime).
- `dev/workflow/rocoto/tasks.py`: Added `'fcst_manager'` to `VALID_TASKS`.
- `dev/workflow/rocoto/gfs_tasks.py`: Added `fcst_manager()` method. Depends
  on `atm_products.txt` data file in `COMOUT_CONF` (age ≥ 60 s).
- `dev/workflow/applications/gfs_forecast_only.py`: Added `fcst_manager` task.
- `dev/workflow/applications/gfs_cycled.py`: Added `fcst_manager` for GFS run.
