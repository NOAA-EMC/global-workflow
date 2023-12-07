###########################
Global Workflow Components
###########################

The global-workflow is a combination of several components working together to prepare, analyze, produce, and post-process forecast data.

The major components of the system are:

* Workflow
* Pre-processing
* Analysis
* Forecast
* Post-processing
* Verification

The Global Workflow repository contains the workflow and script layers. After running the checkout script, the code and additional offline scripts for the analysis, forecast, and post-processing components will be present. Any non-workflow component is known as a sub-module. All of the sub-modules of the system reside in their respective repositories on GitHub. The global-workflow sub-modules are obtained by running the checkout script found under the /sorc folder.

======================
Component repositories
======================

Components checked out via sorc/checkout.sh:

* **GFS UTILS** (https://github.com/ufs-community/gfs_utils): Utility codes needed by Global Workflow to run the GFS configuration
* **UFS-Weather-Model** (https://github.com/ufs-community/ufs-weather-model): This is the core model used by the Global-Workflow to provide forecasts. The UFS-weather-model repository is an umbrella repository consisting of cooupled component earth systeme that are all checked out when we check out the code at the top level of the repoitory
* **GSI** (https://github.com/NOAA-EMC/GSI): This is the core code base for atmospheric Data Assimilation
* **GSI UTILS** (https://github.com/NOAA-EMC/GSI-Utils): Utility codes needed by GSI to create analysis
* **GSI Monitor** (https://github.com/NOAA-EMC/GSI-Monitor): These tools monitor the GSI package's data assimilation, detecting and reporting missing data sources, low observation counts, and high penalty values
* **GDAS** (https://github.com/NOAA-EMC/GDASApp): Jedi based Data Assimilation system. This system is currently being developed for marine Data Assimilation and in time will replace GSI for atmospheric data assimilation as well
* **UFS UTILS** (https://github.com/ufs-community/UFS_UTILS): Utility codes needed for UFS-weather-model
* **Verif global** (https://github.com/NOAA-EMC/EMC_verif-global): Verification package to evaluate GFS parallels. It uses MET and METplus. At this moment the verification package is limited to providing atmospheric metrics only

.. note::
   When running the system in forecast-only mode the Data Assimilation components are not needed and are hence not checked out.

=====================
External dependencies
=====================

^^^^^^^^^
Libraries
^^^^^^^^^

All the libraries that are needed to run the end to end Global Workflow are built using a package manager. Currently these are served via HPC-STACK but will soon be available via SPACK-STACK. These libraries are already available on supported NOAA HPC platforms

Find information on official installations of HPC-STACK here:

https://github.com/NOAA-EMC/hpc-stack/wiki/Official-Installations

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Observation data (OBSPROC/prep)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
****
Data
****

Observation data, also known as dump data, is prepared in production and then archived in a global dump archive (GDA) for use by users when running cycled experiments. The GDA (identified as ``$DMPDIR`` in the workflow) is available on supported platforms and the workflow system knows where to find the data.

* Hera: /scratch1/NCEPDEV/global/glopara/dump
* Orion/Hercules: /work/noaa/rstprod/dump
* Jet: /mnt/lfs4/HFIP/hfv3gfs/glopara/dump
* WCOSS2: /lfs/h2/emc/global/noscrub/emc.global/dump
* S4: /data/prod/glopara/dump

-----------------------------
Global Dump Archive Structure
-----------------------------

The global dump archive (GDA) mimics the structure of its production source: ``DMPDIR/CDUMP.PDY/[CC/atmos/]FILES``

The ``CDUMP`` is either gdas, gfs, or rtofs. All three contain production output for each day (``PDY``). The gdas and gfs folders are further broken into cycle (``CC``) and component (``atmos``).

The GDA also contains special versions of some datasets and experimental data that is being evaluated ahead of implementation into production. The following subfolder suffixes exist:

+--------+------------------------------------------------------------------------------------------------------+
| SUFFIX | WHAT                                                                                                 |
+========+======================================================================================================+
| nr     | Non-restricted versions of restricted files in production. Produced in production. Restriced data is |
|        | fully stripped from files. These files remain as is.                                                 |
+--------+------------------------------------------------------------------------------------------------------+
| ur     | Un-restricted versions of restricted files in production. Produced and archived on a 48hrs delay.    |
|        | Some restricted datasets are unrestricted. Data amounts: restricted > un-restricted > non-restricted |
+--------+------------------------------------------------------------------------------------------------------+
| x      | Experimental global datasets being evaluated for production. Dates and types vary depending on       |
|        | upcoming global upgrades.                                                                            |
+--------+------------------------------------------------------------------------------------------------------+
| y      | Similar to "x" but only used when there is a duplicate experimental file in the x subfolder with the |
|        | same name. These files will be different from both the production versions (if that exists already)  |
|        | and the x versions. This suffix is rarely used.                                                      |
+--------+------------------------------------------------------------------------------------------------------+
| p      | Pre-production copy of full dump dataset, as produced by NCO during final 30-day parallel ahead of   |
|        | implementation. Not always archived.                                                                 |
+--------+------------------------------------------------------------------------------------------------------+

***************
Data processing
***************

Upstream of the global-workflow is the collection, quality control, and packaging of observed weather. The handling of that data is done by the OBSPROC group codes and scripts. The global-workflow uses two packages from OBSPROC to run its prep step to prepare observation (dump) data for use by the analysis system:

1. https://github.com/NOAA-EMC/obsproc
2. https://github.com/NOAA-EMC/prepobs

Package versions and locations on supported platforms are set in the global-workflow system configs, modulefiles, and version files.
