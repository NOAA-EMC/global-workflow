##############################
Seasonal Forecast System (SFS)
##############################

========
Overview
========

The Seasonal Forecast System (SFS) produces a coupled global ensemble forecast for use in seasonal forecasting.

.. note::

   The SFS is still undergoing initial development and some key features are still incomplete.

============
Key Features
============
* 3D-EnVar coupled data assimilation via JEDI
* Fully coupled forecast with hydrostatic atmosphere, ocean, and ice
* Ensemble mean and spread

============
SFS Workflow
============

The SFS is planned to have its own data assimilation system that mirrors that of GFS. The resulting workflow will be a mixture of that of GFS, with a DA portion, and GEFS, with a full-length ensemble forecast. For now, only a forecast-only version is available.

The workflow is managed by the rocoto workflow manager, with tasks defined in the
``dev/workflow/rocoto/sfs_tasks.py`` file.

For a list of descriptions for all jobs, see :ref:`job_descriptions`.

===================
Configuration Files
===================

SFS configuration is primarily managed through the config files in the ``dev/parm/config/sfs/`` directory. Structure is very similar to that of GFS, so see :ref:`_gfs_config_files` for more details.

===============
Output Products
===============

* grib2 gridded atmosphere forecast on various grids, including ensemble mean
* NetCDF marine gridded forecast, including ensemble mean
* wave gridded and point forecasts, including ensemble mean
