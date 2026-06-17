###########################
Global Workflow Components
###########################

The Global Workflow is an end-to-end workflow includes several components stitched together to prepare, analyze, produce, and post-process forecast data.

The major components of the GW system are:

* Workflow
* Pre-processing
* Analysis/Data Assimilation
* Forecast
* Post-processing
* Verification
* Archiving

The GW repository contains the workflow and script layers. External components will be checked out as git submodules. All of the submodules of the system reside in their respective repositories on GitHub.

======================
Component Repositories
======================

Components included as submodules:

* **UFS-WM** (https://github.com/ufs-community/ufs-weather-model): This is the core weather model used by the GW to generate forecasts. The UFS-WM repository is an umbrella repository consisting of coupled component earth system that are all checked out when we check out the code at the top level of the repository.
* **Global Data Assimilation System (GDAS)** (https://github.com/NOAA-EMC/GDASApp): Joint Effort for Data assimilation Integration (JEDI) based DA system. This system is currently being developed for marine DA and in time will replace GSI for atmospheric DA as well.
* **Gridpoint Statistical Interpolation (GSI)** (https://github.com/NOAA-EMC/GSI): The core code base for atmospheric Data Assimilation (DA)
* **GSI UTILS** (https://github.com/NOAA-EMC/GSI-Utils): Utility codes needed by GSI to create analysis
* **GSI Monitor** (https://github.com/NOAA-EMC/GSI-Monitor): These are tools for monitoring GSI package's DA, and for detecting and reporting missing data sources, low observation counts, high penalty values etc.
* **GFS UTILS** (https://github.com/ufs-community/gfs_utils): Utility codes needed by GW to run the GFS configuration
* **NEXUS** (https://github.com/NOAA-OAR-ARL/NEXUS): Prepares emissions inputs for GCAFS
* **UFS UTILS** (https://github.com/ufs-community/UFS_UTILS): Utility codes needed for UFS-WM
* **wxflow** (https://github.com/NOAA-EMC/wxflow): Collection of python utilities for weather workflows
* **Verif global** (https://github.com/NOAA-EMC/EMC_verif-global): Verification package to evaluate GFS parallels. It uses Model Evaluation Tools (MET) and METplus verfication tools. At this moment the verification package is limited to providing atmospheric metrics only.
* **UPP** (https://github.com/NOAA-EMC/UPP): Unified Post Processor (UPP) is a submodule within UFS-WM. It is a software package designed to generate useful products from raw model output.

.. note::

   When running the system in forecast-only mode the DA components are not needed and are hence not built.

==================================
Additional Resources for New Users
==================================

Additional documentation and resources relevant to GW is shown in the table below for new users.

+---------------------+-----------------------------------------------------------------------------------------+
| **Documentation**   | **Location**                                                                            |
+=====================+=========================================================================================+
| UFS Weather Model   | https://ufs-weather-model.readthedocs.io/en/develop/                                    |
+---------------------+-----------------------------------------------------------------------------------------+
| UFS UTILS           | https://noaa-emcufs-utils.readthedocs.io/en/latest/                                     |
+---------------------+-----------------------------------------------------------------------------------------+
| GFS UTILS           | https://www.emc.ncep.noaa.gov/emc/pages/numerical_forecast_systems/gfs/documentation.php|
+---------------------+-----------------------------------------------------------------------------------------+
| WaveWatchIII        | https://github.com/NOAA-EMC/WW3/wiki/WAVEWATCH-III-User-Guide                           |
+---------------------+-----------------------------------------------------------------------------------------+
| GDAS                | https://www.ncei.noaa.gov/products/weather-climate-models/global-data-assimilation      |
+---------------------+-----------------------------------------------------------------------------------------+
| GSI                 | https://github.com/NOAA-EMC/GSI/tree/develop/doc                                        |
+---------------------+-----------------------------------------------------------------------------------------+
| wxflow              | https://wxflow.readthedocs.io/en/latest/                                                |
+---------------------+-----------------------------------------------------------------------------------------+
| Verif Global        | https://github.com/NOAA-EMC/EMC_verif-global/wiki                                       |
+---------------------+-----------------------------------------------------------------------------------------+
| UPP                 | https://upp.readthedocs.io/en/latest/                                                   |
+---------------------+-----------------------------------------------------------------------------------------+

=====================
External dependencies
=====================

^^^^^^^^^
Libraries
^^^^^^^^^

All the libraries that are needed to run the end-to-end GW are built using a package manager. These are served via spack-stack. These libraries are already installed on supported NOAA HPC platforms.

For more information:

https://github.com/JCSDA/spack-stack/wiki/Porting-spack-stack-to-a-new-system

.. _dump_archive:

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Observation data (OBSPROC/prep)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
****
Data
****

Observation data, also known as dump data, is prepared in production and then archived in the Global Dump Archive (GDA) for use in cycled experiments. The GDA is available on supported platforms and is identified as ``$DMPDIR`` in the workflow. The workflow automatically knows where to find GDA data on supported platforms:

* WCOSS2: ``/lfs/h2/emc/global/noscrub/emc.global/dump``
* Ursa: ``/scratch3/NCEPDEV/global/role.glopara/dump``
* Hercules/Orion: ``/work/noaa/rstprod/dump``
* Gaea C6: ``/gpfs/f6/drsa-precip3/world-shared/role.glopara/dump``
* Derecho: ``/lustre/desc1/p/nral0032/global/dump_nr`` (non-restricted)
* AWS S3 Bucket: https://noaa-epic-global-release-pds.s3.amazonaws.com/index.html#dump_nr/ (non-restricted)
   * Mounted in AWS instance to ``/lustre/noaa-epic-global-release-pds``

-------------
GDA Structure
-------------

The GDA mimics its production layout:

* GDAS/GFS: ``DMPDIR/gdas[gfs].PDY/CC/atmos/FILES``
* Real‑Time Ocean Forecast System (RTOFS): ``DMPDIR/rtofs.PDY/FILES``

The GDA also contains special versions of some datasets and experimental data that is being evaluated ahead of implementation into production. The special subfolder suffixes are:

+--------+------------------------------------------------------------------------------------------------------+
| SUFFIX | MEANING                                                                                              |
+========+======================================================================================================+
| nr     | Non-restricted versions of restricted files in production. Produced in production. Restricted data is|
|        | fully stripped from files. These files remain as is.                                                 |
+--------+------------------------------------------------------------------------------------------------------+
| ur     | Un-restricted versions of restricted files in production. Produced and archived on a 48hrs delay.    |
|        | Some restricted datasets are unrestricted. Data amounts: restricted > un-restricted > non-restricted |
|        | Limited availability. Discontinued in mid-2023.                                                      |
+--------+------------------------------------------------------------------------------------------------------+
| x      | Experimental global datasets under evaluation for production. Dates and types vary depending on      |
|        | upcoming upgrades.                                                                                   |
+--------+------------------------------------------------------------------------------------------------------+
| y      | Similar to "x" but only used when there is a duplicate experimental file in the x subfolder with the |
|        | same name. Different from both the production versions, if exists, and the x versions.               |
|        | This suffix is rarely used.                                                                          |
+--------+------------------------------------------------------------------------------------------------------+
| p      | Pre-production dataset copy, as produced by National Centers for Environmental                       |
|        | Prediction (NCEP) Central Operations (NCO) during final 30-day parallel ahead of implementation.     |
|        | Not always archived.                                                                                 |
+--------+------------------------------------------------------------------------------------------------------+

.. note::

   The provided dump on Derecho and AWS are both non-restricted, but omit the ``nr`` from the PDY directory.

***************
Data processing
***************

Upstream observation handling (collection, quality control, and packaging) of global workflow is performed using Observation Processing (OBSPROC) group's codes and scripts. The workflow uses two packages from OBSPROC to prepare observation (dump) data for use by the analysis system:

1. https://github.com/NOAA-EMC/obsproc
2. https://github.com/NOAA-EMC/prepobs

Package versions and paths on supported platforms are defined in the GW system configuration file and module files.
