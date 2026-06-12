#####################
HPC Setup and Support
#####################

Running the GFS configurations (or nearly any GW configuration except the coarsest) is a resource intensive task. This page describes recommended Research and Development High‑Performance Computing Systems (RDHPCS) environment settings and provides contact information in case assistance is needed from a specific HPC helpdesk. While most of the documentation focuses on supported NOAA platforms, the guidance presented here can be applied to other systems.

====================================
Minimum system software requirements
====================================
The system software requirements listed in the following table represent the minimum standards for any new or existing system and reflect the development and testing environments on which the GW is maintained. Any system that does not meet these requirements will not be supported.

+--------------+-------------+---------------------------------------+
| Software     | Minimum     | Notes                                 |
|              | supported   |                                       |
|              | version(s)  |                                       |
+==============+=============+=======================================+
| Bash         | 4.4.20      |                                       |
+--------------+-------------+---------------------------------------+
| Python       | * 3.8.6     | * 3.10.x is not supported by METplus  |
|              | * 3.10.13+  |   verification software               |
|              | * 3.11.6+   | * 3.11.6 is packaged with spack-stack |
|              |             | * 3.9.x is untested                   |
+--------------+-------------+---------------------------------------+
| Spack-Stack  | 1.9.2       | * Available everywhere but WCOSS2     |
+--------------+-------------+---------------------------------------+
| lmod         | 8.3.1       |                                       |
+--------------+-------------+---------------------------------------+
| | Slurm OR   | | 23.02.7   | * Other schedulers may be supportable |
| | PBSpro     | | 2022.1.1  |                                       |
+--------------+-------------+---------------------------------------+
| Git          | 2.29.0      | * Some components e.g. GDASApp may    |
|              |             |   need Git Large File Storage         |
|              |             |   (Git-LFS) for downloading test data |
+--------------+-------------+---------------------------------------+
| Rocoto       | 1.3.5       | * 1.3.7 is required for newer         |
|              |             |   versions of Ruby (3.2+)             |
+--------------+-------------+---------------------------------------+
| Intel        | 2024.2.1    | * GNU compilers are not supported     |
| Compilers    |             | * Intel LLVM compilers are not yet    |
|              |             |   supported                           |
|              |             | * Intel 19.x is only supported on     |
|              |             |   WCOSS2                              |
+--------------+-------------+---------------------------------------+

==============================
Feature availability by RDHPCS
==============================

The GW provides capabilities for deterministic and ensemble forecasts, along with DA, across multiple platforms. However, not all features are currently supported on every system. The table below summarizes available features by platform and states their level of support.

.. _HPC_Capabilities_Matrix:

.. list-table:: Capabilities matrix by HPC
   :header-rows: 1
   :align: center

   * - HPC
     - Tier
     - Coupled
       GFS
     - Coupled
       GEFS
     - Coupled
       GCAFS
     - Coupled
       SFS
     - GSI
       DA
     - GDASApp
       DA
     - Coupled
       DA
     - TC Tracker
     - AWIPS
     - MOS
     - Fit2Obs
     - METplus
       Verification
     - HPSS
       Archiving
   * - WCOSS2
     - 1
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     - X
   * - Ursa
     - 1
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     -
     -
     - X
     -
     - X
   * - Hercules
     - 1
     - X
     - X
     -
     - X
     - X
     - X
     - X
     -
     -
     -
     - X
     -
     - X
   * - Gaea C6
     - 1
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     -
     -
     - X
     -
     - X
   * - Orion
     - 2
     - X
     - X
     -
     - X
     - X
     - X
     -
     - X
     -
     -
     - X
     -
     - X
   * - Derecho
     - 1
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     -
     -
     - X
     -
     -
   * - AWS (Native)
     - 2
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     - X
     -
     -
     - X
     -
     -
   * - AWS (PW)
     - 3
     - X
     - X
     -
     - X
     - X
     -
     -
     -
     -
     -
     -
     -
     -
   * - GCP (PW)
     - 3
     - X
     - X
     -
     - X
     -
     -
     -
     -
     -
     -
     -
     -
     -
   * - Azure
     - 3
     - X
     - X
     -
     - X
     -
     -
     -
     -
     -
     -
     -
     -
     -

=============
HPC Helpdesks
=============

* WCOSS2: hpc.wcoss2-help@noaa.gov
* Ursa: rdhpcs.ursa.help@noaa.gov
* Orion:  rdhpcs.orion.help@noaa.gov
* Hercules:  rdhpcs.hercules.help@noaa.gov
* HPSS: rdhpcs.hpss.help@noaa.gov
* Gaea: oar.gfdl.help@noaa.gov
* Cloud: rdhpcs.cloud.help@noaa.gov
* Derecho: https://rchelp.ucar.edu

======================
Restricted Data Access
======================

The GFS system ingests dump data files that contain global observational data. Several of these dump files include restricted data which requires additional level of permissions called **restricted** or ``rstprod``. Users who wish to run cycled GFS experiments, which both use restricted observational data and produce restricted output, must obtain ``rstprod`` access.

To request ``rstprod`` access, follow one or both of the options below depending on the platform you need access for:

* Requesting restricted data access on WCOSS2

If you need access on WCOSS2, review restricted data policy and submit the request here: 
https://www.nco.ncep.noaa.gov/sib/restricted_data/restricted_data_sib/


* Requesting restricted data access on NOAA RDHPCS

If you need restricted data access on NOAA RDHPCS platforms: 

1. Login to the NOAA AIM portal
2. select **Request new access to a project** 
3. Choose ``rstprod`` project 
4. Provide justification explaining why you need restricted data access
5. Submit the request here: https://aim.rdhpcs.noaa.gov/

.. note::
   Data that has been staged on Derecho or in an AWS S3 data bucket has already had restricted data removed.
