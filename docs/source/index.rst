
###############
Global Workflow
###############

**Global-workflow** is the end-to-end workflow designed to run global configurations of medium range weather forecasting for the UFS weather model. It supports both development and operational implementations. In its current format it supports the Global Forecast System (GFS) and the Global Ensemble Forecast System (GEFS) configurations

======
Status
======

* State of develop (HEAD) branch: GFSv17+ development
* State of operations (dev/gfs.v16 branch): GFS v16.3.13 `tag: [gfs.v16.3.13] <https://github.com/NOAA-EMC/global-workflow/releases/tag/gfs.v16.3.13>`_

=============
Code managers
=============

* Kate Friedman - @KateFriedman-NOAA / kate.friedman@noaa.gov
* Walter Kolczynski - @WalterKolczynski-NOAA / walter.kolczynski@noaa.gov

=============
Announcements
=============

General updates: NOAA employees and affiliates can join the gfs-announce distribution list to get updates on the GFS and global-workflow. Contact Kate Friedman (kate.friedman@noaa.gov) and Walter Kolczynski (walter.kolczynski@noaa.gov) to get added to the list or removed from it.

GitHub updates: Users should adjust their "Watch" settings for this repo so they receive notifications as they'd like to. Find the "Watch" or "Unwatch" button towards the top right of the `authoritative global-workflow repository page <https://github.com/NOAA-EMC/global-workflow>`_ and click it to adjust how you watch the repo.

=================
Table of Contents
=================

.. toctree::
   :numbered:
   :maxdepth: 3

   development.rst
   components.rst
   jobs.rst
   hpc.rst
   output.rst
   run.rst
   wave.rst
   noaa_csp.rst
   errors_faq.rst
