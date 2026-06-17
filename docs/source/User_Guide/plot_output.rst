###############
Plotting Output
###############

========================
Plotting Analysis Output
========================

The `GSI Monitor <https://github.com/NOAA-EMC/GSI-Monitor>`_ repository contains a monitoring package called **RadMon**. This package reads the information on the radiances contained in the radstat files, including quality control (QC) flags and departure statistics, and produces a webpage with many plots such as time series of data counts for a particular instrument.  You can also directly compare two different experiments with this tool. If there are parameters that you are interested in but the RadMon package is not plotting them for you, you can use the existing RadMon code as a guide for how to read them and plot them yourself.  The radstat files contain a wealth of useful information.

The RadMon package can be found under the ``src/Radiance_Monitor`` folder within the `GSI Monitor`_ repository. If checked out under GW, you will find it under ``gsi_monitor.fd/src/Radiance_Monitor``.

If you have questions or issues getting the package to work, contact the RadMon developer: Ed Safford (edward.safford@noaa.gov).

========================
Plotting Forecast Output
========================
This section will be updated when we have some basic plotting utilities using EMCPY
