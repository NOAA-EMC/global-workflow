#####################
HPC Settings and Help
#####################

Running the GFS configurations (or almost any global workflow configuration except the coarsest) is a resource intensive exercise. This page discusses recommended HPC environmental settings and contact information in case you need assistance from a particular HPC helpdesk. While most of the documentation is based on supported NOAA platforms, the learnings here can hopefully apply to other platforms. 

====================================
Minimum system software requirements
====================================

The following system software requirements are the minimum for any new or existing system and reflect the development and testing environment on which the global workflow is maintained.  Any system that does not meet these requirements will not be supported.

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
| Spack-Stack  | 1.6.0       | * Available everywhere but WCOSS2     |
+--------------+-------------+---------------------------------------+
| lmod         | 8.3.1       |                                       |
+--------------+-------------+---------------------------------------+
| Slurm        | 23.02.7     | * Other schedulers may be supportable |
+--------------+-------------+---------------------------------------+
| PBSpro       | 2022.1.1    | * Other schedulers may be supportable |
+--------------+-------------+---------------------------------------+
| Git          | 2.29.0      | * Some components e.g. GDASApp may    |
|              |             |   need Git-LFS for downloading test   |
|              |             |   data                                |
+--------------+-------------+---------------------------------------+
| Rocoto       | 1.3.5       | * 1.3.7 is required for newer         |
|              |             |   versions of Ruby (3.2+)             |
+--------------+-------------+---------------------------------------+
| Intel        | 2021.5.1    | * GNU compilers are not supported     |
| Compilers    |             | * Intel LLVM compilers are not yet    |
|              |             |   supported                           |
|              |             | * Intel 19.x is only supported on     |
|              |             |   WCOSS2                              |
+--------------+-------------+---------------------------------------+

===========================
Feature availability by HPC
===========================

The Global Workflow provides capabilities for deterministic and ensemble forecasts along with data assimilation on multiple platforms.  However, not all features are currently supported on all platforms.  The following table lists the features by platform and states their level of support.

.. list-table:: Capabilities matrix by HPC
   :header-rows: 1
   :align: center

   * - HPC
     - Tier
     - Coupled
       GFS
     - Coupled
       GEFS
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
     - 
     - X    
     - X  
     - X 
     - X   
     - X        
     - X
   * - Hera    
     - 1   
     - X     
     - X     
     - X   
     - X   
     - X     
     - X    
     - X  
     - 
     - X   
     - X        
     - X
   * - Hercules
     - 1   
     - X     
     - X     
     - X   
     - X     
     - X     
     - 
     - 
     - 
     - X   
     - X        
     -
   * - Orion   
     - 2   
     - X     
     - X     
     - X   
     - X     
     - 
     - X    
     - 
     - 
     - X   
     - X        
     -
   * - Gaea C5 
     - 3   
     - X     
     - X     
     - X   
     - X     
     - 
     - 
     - 
     - 
     - 
     - 
     - X
   * - Gaea C6 
     - 3   
     - X     
     - X     
     - X   
     - X     
     - 
     - 
     - 
     - 
     - 
     - 
     - X
   * - AWS (PW)
     - 3   
     - X     
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
     - 
     - 
     - 
     - 
     - 
     - 
     - 
     -
   * - Jet     
     - 3   
     - X     
     - 
     - X   
     - 
     - 
     - X    
     - 
     - 
     - X   
     - X      
     - X
   * - S4      
     - 3   
     - 
     - 
     - X   
     - 
     - 
     - X    
     - 
     - 
     - X   
     - X      
     -

================================
Experiment troubleshooting help
================================

Users may email Kate Friedman (kate.friedman@noaa.gov) questions or requests for troubleshooting assistance with their global-workflow experiments/parallels on supported platforms. For troubleshooting, please provide a brief description of the issue(s) and include relevant error messages and/or paths to logs for failed jobs.

Any issues related to HPC/machine problems, and which are unrelated to the workflow itself, should go to the appropriate HPC helpdesk. 

=============
HPC helpdesks
=============

* WCOSS2: hpc.wcoss2-help@noaa.gov
* Hera: rdhpcs.hera.help@noaa.gov
* Orion:  rdhpcs.orion.help@noaa.gov
* Hercules:  rdhpcs.hercules.help@noaa.gov
* HPSS: rdhpcs.hpss.help@noaa.gov
* Gaea: oar.gfdl.help@noaa.gov
* S4: innocent.souopgui@noaa.gov
* Jet: rdhpcs.jet.help@noaa.gov
* Cloud: rdhpcs.cloud.help@noaa.gov

======================
Restricted data access
======================

The GFS system ingests dump data files that contain global observation data. A number of these dump files contain restricted data which means those files come with an extra level of permissions called restricted or ‘rstprod’. Users who wish to run cycled GFS experiments, which both utilizes restricted observation data and produces output containing restricted data, will need to gain rstprod group access.

NOTE: Only non-restricted data is available on S4.

To request rstprod access, do either a and/or b below:

a) If you need restricted data access on WCOSS2, read details about restricted data and fill out form here:

https://www.nco.ncep.noaa.gov/sib/restricted_data/restricted_data_sib/

b) If you need restricted data access on RDHPCS systems: go to the AIM system, click on "Request new access to a project", select the rstprod project, provide justification for needed access, and submit the request:

https://aim.rdhpcs.noaa.gov/

====================================
Optimizing the global workflow on S4
====================================

The S4 cluster is relatively small and so optimizations are recommended to improve cycled runtimes. Please contact Innocent Souopgui (innocent.souopgui@noaa.gov) if you are planning on running a cycled experiment on this system to obtain optimized configuration files.

==================================================
Stacksize on R&Ds (Hera, Orion, Hercules, Jet, S4)
==================================================

Some GFS components, like the UPP, need an unlimited stacksize. Add the following setting into your appropriate .*rc file to support these components:

csh::

    limit stacksize unlimited

sh/bash/ksh::

    ulimit -s unlimited

=========================================
Forecast hangs due to issue with ssh-keys
=========================================

Did you generate your ssh-keys with a passphrase? If so, remake them without one. To test this try ssh-ing to a different login node; you should be able to without being prompted for your passphrase.

Is your public key in the authorized_keys file? If not, add it::

   cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys

