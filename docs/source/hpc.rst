#####################
HPC Settings and Help
#####################

Running the GFS configurations (or almost any global workflow configuration except the coarsest) is a resource intensive exercise. This page discusses recommended HPC environmental settings and contact information in case you need assistance from a particular HPC helpdesk. While most of the documentation is based on supported NOAA platforms, the learnings here can hopefully apply to other platforms. 

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
* S4: david.huber@noaa.gov
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

The S4 cluster is relatively small and so optimizations are recommended to improve cycled runtimes. Please contact David Huber (david.huber@noaa.gov) if you are planning on running a cycled experiment on this system to obtain optimized configuration files.

============
Git settings
============

^^^^^^
Merges
^^^^^^

Use the following command to have merge commits include the one-line description of all the commits being merged (up to 200). You only need to do this once on each machine; it will be saved to your git settings::

   git config --global merge.log 200

Use the ``--no-ff`` option to make sure there is always a merge commit when a fast-forward only is available. Exception: If the merge contains only a single commit, it can be applied as a fast-forward.

For any merge with multiple commits, a short synopsis of the merge should appear between the title and the list of commit titles added by merge.log.

^^^^^^^
Version
^^^^^^^

It is advised to use Git v2+ when available. At the time of writing this documentation the default Git clients on the different machines were as noted in the table below. It is recommended that you check the default modules before loading recommended ones:

+----------+----------+---------------------------------------+
| Machine  | Default  | Recommended                           |
+----------+----------+---------------------------------------+
| Hera     | v2.18.0  | default                               |
+----------+----------+---------------------------------------+
| Hercules | v2.31.1  | default                               |
+----------+----------+---------------------------------------+
| Orion    | v1.8.3.1 | **module load git/2.28.0**            |
+----------+----------+---------------------------------------+
| Jet      | v2.18.0  | default                               |
+----------+----------+---------------------------------------+
| WCOSS2   | v2.35.3  | default                               |
+----------+----------+---------------------------------------+
| S4       | v1.8.3.1 | **module load git/2.30.0**            |
+----------+----------+---------------------------------------+
| AWS PW   | v1.8.3.1 | default                               |
+----------+----------+---------------------------------------+

^^^^^^^^^^^^^
Output format
^^^^^^^^^^^^^

For proper display of Git command output (e.g. git branch and git diff) type the following once per machine:

::

   git config --global core.pager 'less -FRX'

For the manage_externals utility functioning::

   Error: fatal: ssh variant 'simple' does not support setting port
   Fix: git config --global ssh.variant ssh

========================================
Stacksize on R&Ds (Hera, Orion, Hercules, Jet, S4)
========================================

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

