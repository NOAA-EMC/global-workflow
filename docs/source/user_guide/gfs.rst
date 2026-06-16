############################
Global Forecast System (GFS)
############################

========
Overview
========

The Global Forecast System (GFS) is the flagship NOAA NWP model and produces a coupled global analysis and forecast for use in medium-range forecasting. GFS supports many different coupling and data assimilation configurations.

============
Key Features
============

* 3D-EnVar atmospheric data assimilation using both legacy GSI and new JEDI-based system
* 3D-EnVar coupled data assimilation via JEDI
* Fully coupled forecast with atmosphere, ocean, ice, and waves
* Wide range of supported resolutions
* Gridded forecasts on a variety of output grids
* Model forecast soundings
* Point wave forecasts
* MET-plus-based forecast verification (on select platforms)
* Archival to NOAA tape storage (HPSS) where available

============
GFS Workflow
============

GFS runs in multiple phases: an 'early' *gfs* phase, which in operations runs shortly after the initialization time, and a 'late' *gdas* phase, run later that includes late-arriving observations. The gfs phase produces an initial analysis using obs available at the time, which it then uses for initial conditions for the main GFS forecast. The gdas phase produces a new analysis incoroprating the late observations and runs the DA ensemble to produce background error covariances for the next cycle. Starting with GFS v17, the DA ensemble will also be run in the early (gfs) phase to provide improved initial conditions to GEFS.

For consistency, GFS uses this same pattern in development mode. Two sets of observations are preserved in the :ref:`_dump_archive <global dump archive>`, one with those observations available for the early phase, and one with those available for the late phase.

The GFS configuration in the GW is organized into a series of jobs that run in a defined sequence for each cycle. These jobs handle everything from preparing input data to running the forecast and generating post processed products. Each job is executed by a workflow manager (rocoto or ecFlow) and corresponds to a specific script within the workflow. Although the exact list of jobs varies depending on whether you are running gdas, gfs, or a specialized experiment, the major categories of jobs include:

1. **Preprocessing Jobs**:

These jobs prepare all required inputs before analysis or forecasting begins and examples include:

  - **prep**: runs data preprocessing prior to the analysis
  - **stage_ic**: stages the initial conditions needed to start the forecast
  - **waveinit/waveprep**: wave model initialization and preprocessing (when waves are enabled)

2. **Analysis Jobs (gdas phase)**:
These jobs perform data assimilation to produce the best estimate of the atmosphere at the cycle time and examples include:

  - **anal**: runs the atmospheric analysis (GSI) to produce analysis increments and update the surface guess
  - **analcalc**: adds the analysis increments to the previous cycle's forecast to produce the atmospheric analysis files
  - **analdiag**: creates netCDF diagnostic files (observation values, innovations, errors, QC)
  - **EnKF jobs (eobs, eupd, ecenN, esfc, efcs)**: Ensemble Kalman Filter (ENKF) data assimilation (when running cycled with an ensemble)
  - **updatebc**: updates background fields for the next cycle

3. **Forecast Jobs (gfs phase)**:
These jobs run the UFS-WM to produce the forecast and these jobs include:

  - **fcst**: main forecast model integration
  - **atmupp**: runs UPP on model output

4. **Post processing Jobs**:
These jobs generate downstream products used for verification, graphics, or distribution and examples include:

  - **atmos_prod**: regrids atmosphere forecast to lat-lon grids
  - **wave post jobs (wavepostsbs, wavepostpnt, wavepostbndpnt, wavepostbndpntbll)**: wave post-processing
  - **metp**: MET/METplus verification via EMC_verif-global
  - **awips / gempak**: downstream AWIPS and GEMPAK products (operations only; not normally run in experiments)

5. **Archiving and Cleanup Jobs (Experimental Mode Only)**:
These jobs are run only in development mode and examples include:

  - **arch_vrfy**: archives verification products
  - **arch_tars**: archives tarred workflow outputs (e.g., logs, restarts)
  - **cleanup**: removes temporary or intermediate files

In development, the workflow is managed by the rocoto workflow manager, with tasks defined in the
``dev/workflow/rocoto/gfs_tasks.py`` file.

For a list of descriptions for all jobs, see :ref:`job_descriptions`.

.. _gfs_config_files:

===================
Configuration Files
===================

GFS configuration is primarily managed through the config files in the ``dev/parm/config/gfs`` directory. Core settings and those related to job flow are located in ``config.base``, with the most common settings additionally parameterized to be filled in at experiment creation time. Job resources (cores, memory, etc.) are defined in ``config.resources`` with machine-specific overrides in the corresponding version for the machine. Beyond those, each job (or class of jobs) has its own config file for that job's settings.

===============
Output Products
===============

* Early and late analysis at model resolution
* EnKF ensemble to provide background errors and initial conditions for GEFS
* Data assimilation verification diagnostics
* grib2 gridded atmosphere forecast on various grids
* forecast model soundings
* NetCDF marine gridded forecast
* wave gridded and point forecasts
* MET-plus-based forecast verification

============================================
Experimental vs Operational Runs: A Snapshot
============================================

Experimental run is different from operational runs in the following ways:

* **Workflow manager**:

  - Operations use `ecFlow <https://www.ecmwf.int/en/learning/training/introduction-ecmwf-job-scheduler-ecflow>`__, whereas development use `rocoto <https://github.com/christopherwharrop/rocoto/wiki/documentation>`__.

.. note::

   Experiments can also be run with ecFlow if the platform has an ecFlow server.

* **Dump step**:

  - Not run in experiments but in real-time production. Dump data already exists in GDA on supported platforms.

* **Additional steps** in experimental mode:

  - **arch_vrfy**
  - **arch_tars**
  - **cleanup**

^^^^^^^^^^^^^^^^^^^
For New Users
^^^^^^^^^^^^^^^^^^^
.. note::

  - Operational systems include many additional downstream jobs that are not run in development mode.
  - The workflow manager (rocoto or ecFlow) determines job dependencies and ensures each job runs only when its prerequisites are complete.
  - The exact job list for your experiment is defined in the workflow XML (rocoto) or suite definition (ecFlow)
