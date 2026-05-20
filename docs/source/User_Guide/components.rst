###########################
Global Workflow Components
###########################

The Global Workflow is an end-to-end workflow includes several components stitched together to prepare, analyze, produce, and post-process forecast data.

The major components of the GW system are:

* Workflow
* Pre-processing
* Analysis
* Forecast
* Post-processing
* Verification
* Archiving

The GW repository contains the workflow and script layers. External components will be checked out as git submodules. All of the submodules of the system reside in their respective repositories on GitHub.

======================
Component repositories
======================

Components included as submodules:

* **GFS UTILS** (https://github.com/ufs-community/gfs_utils): Utility codes needed by GW to run the GFS configuration
* **UFS-WM** (https://github.com/ufs-community/ufs-weather-model): This is the core weather model used by the GW to generate forecasts. The UFS-WM repository is an umbrella repository consisting of coupled component earth system that are all checked out when we check out the code at the top level of the repoitory.
* **Gridpoint Statistical Interpolation (GSI)** (https://github.com/NOAA-EMC/GSI): The core code base for atmospheric Data Assimilation (DA)
* **GSI UTILS** (https://github.com/NOAA-EMC/GSI-Utils): Utility codes needed by GSI to create analysis
* **GSI Monitor** (https://github.com/NOAA-EMC/GSI-Monitor): These are tools for monitoring GSI package's DA, and for detecting and reporting missing data sources, low observation counts, high penalty values etc.
* **Global Data Assimilation System (GDAS)** (https://github.com/NOAA-EMC/GDASApp): Joint Effort for Data assimilation Integration (JEDI)* based DA system. This system is currently being developed for marine DA and in time will replace GSI for atmospheric DA as well.
* **UFS UTILS** (https://github.com/ufs-community/UFS_UTILS): Utility codes needed for UFS-WM
* **wxflow** (https://github.com/NOAA-EMC/wxflow): Collection of python utilities for weather workflows
* **Verif global** (https://github.com/NOAA-EMC/EMC_verif-global): Verification package to evaluate GFS parallels. It uses Model Evaluation Tools (MET) and METplus verfication tools. At this moment the verification package is limited to providing atmospheric metrics only.

.. note::
   When running the system in forecast-only mode the DA components are not needed and are hence not built.

=====================
External dependencies
=====================

^^^^^^^^^
Libraries
^^^^^^^^^

All the libraries that are needed to run the end-to-end GW are built using a package manager. These are served via spack-stack. These libraries are already available on supported NOAA HPC platforms.

To learn more about official spack-stack installations, visit:

https://github.com/JCSDA/spack-stack/wiki/Porting-spack-stack-to-a-new-system

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Observation data (OBSPROC/prep)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
****
Data
****

Observation data, also known as dump data, is prepared in production and then archived in a  Global Dump Archive (GDA) for use by users when running cycled experiments. The GDA (identified as ``$DMPDIR`` in the workflow) is available on supported platforms and the workflow system knows where to find the data.

* Ursa/Hera: ``/scratch3/NCEPDEV/global/role.glopara/dump``
* Orion/Hercules: ``/work/noaa/rstprod/dump``
* WCOSS2: ``/lfs/h2/emc/global/noscrub/emc.global/dump``
* Derecho: ``/lustre/desc1/p/nral0032/global/dump_nr`` (unrestricted)

-------------
GDA Structure
-------------

The GDA mimics the structure of its production source:

* GDAS/GFS: ``DMPDIR/gdas[gfs].PDY/CC/atmos/FILES``
* Real‑Time Ocean Forecast System (RTOFS): ``DMPDIR/rtofs.PDY/FILES``

The GDA also contains special versions of some datasets and experimental data that is being evaluated ahead of implementation into production. The following subfolder suffixes exist:

+--------+------------------------------------------------------------------------------------------------------+
| SUFFIX | MEANING                                                                                              |
+========+======================================================================================================+
| nr     | Non-restricted versions of restricted files in production. Produced in production. Restriced data is |
|        | fully stripped from files. These files remain as is.                                                 |
+--------+------------------------------------------------------------------------------------------------------+
| ur     | Un-restricted versions of restricted files in production. Produced and archived on a 48hrs delay.    |
|        | Some restricted datasets are unrestricted. Data amounts: restricted > un-restricted > non-restricted |
|        | Limited availability. Discontinued producing mid-2023.                                               |
+--------+------------------------------------------------------------------------------------------------------+
| x      | Experimental global datasets being evaluated for production. Dates and types vary depending on       |
|        | upcoming global upgrades.                                                                            |
+--------+------------------------------------------------------------------------------------------------------+
| y      | Similar to "x" but only used when there is a duplicate experimental file in the x subfolder with the |
|        | same name. These files will be different from both the production versions (if that exists already)  |
|        | and the x versions. This suffix is rarely used.                                                      |
+--------+------------------------------------------------------------------------------------------------------+
| p      | Pre-production copy of full dump dataset, as produced by National Centers for Environmental          |
|        | Prediction (NCEP) Central Operations (NCO) during final 30-day parallel ahead of implementation.     |
|        | Not always archived.                                                                                 |
+--------+------------------------------------------------------------------------------------------------------+

***************
Data processing
***************

Upstream of the GW is the collection, quality control, and packaging of observed weather. The handling of that data is done by the Observation Processing (OBSPROC) group's codes and scripts. The GW uses two packages from OBSPROC to run its prep step to prepare observation (dump) data for use by the analysis system:

1. https://github.com/NOAA-EMC/obsproc
2. https://github.com/NOAA-EMC/prepobs

Package versions and locations on supported platforms are set in the GW system configs, modulefiles, and version files.
