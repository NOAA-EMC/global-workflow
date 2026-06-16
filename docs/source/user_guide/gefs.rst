######################################
Global Ensemble Forecast System (GEFS)
######################################

========
Overview
========

The Global Ensemble Forecast System (GEFS) produces a coupled global ensemble forecast for use in medium-range to sub-seasonal forecasting. It leverages the ensemble members and analysis from the GFS data assimilation to produce initial conditions.

============
Key Features
============

* Fully coupled ensemble forecast
* Capability to run forecast in segments for flexibility and resilience with longer forecasts
* Individual member and ensemble mean/spread output

=============
GEFS Workflow
=============

GEFS consists of a control forecast based on the GFS analysis and a number of ensemble members based on members of the GFS EnKF ensemble. Jobs are substantial similar to a GFS forecast-only run, with additional jobs to calculate ensemble forecast mean and spread. Due to the vast amount of data produces by the ensemble, there is also an optional additional job to reduce the number of variables for archival purposes.

The workflow is managed by the rocoto workflow manager, with tasks defined in the
``dev/workflow/rocoto/gefs_tasks.py`` file.

For a list of descriptions for all jobs, see :ref:`job_descriptions`.

===================
Configuration Files
===================

GEFS configuration is primarily managed through the config files in the ``dev/parm/config/gefs/`` directory. Structure is very similar to that of GFS, so see :ref:`_gfs_config_files` for more details.

===============
Output Products
===============

* grib2 gridded atmosphere forecast on various grids, including ensemble mean
* forecast model soundings
* NetCDF marine gridded forecast, including ensemble mean
* wave gridded and point forecasts, including ensemble mean
