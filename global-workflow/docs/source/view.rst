======================
View Experiment output
======================

The output from your run will be found in the ``COMROT/ROTDIR`` you established. This is also where you placed your initial conditions. Within your ``COMROT`` you will have the following directory structure (based on the type of experiment you run):

^^^^^^^^^^^^^
Forecast-only
^^^^^^^^^^^^^

::

   gfs.YYYYMMDD/CC/atmos          <- contains deterministic long forecast gfs inputs/outputs (atmosphere)
   gfs.YYYYMMDD/CC/wave           <- contains deterministic long forecast gfs inputs/outputs (wave)
   logs/                          <- logs for each cycle in the run
   vrfyarch/                      <- contains files related to verification and archival

^^^^^^
Cycled
^^^^^^

::

   enkfgdas.YYYYMMDD/CC/mem###/atmos    <- contains EnKF inputs/outputs for each cycle and each member
   gdas.YYYYMMDD/CC/atmos               <- contains deterministic gdas inputs/outputs (atmosphere)
   gdas.YYYYMMDD/CC/wave                <- contains deterministic gdas inputs/outputs (wave)
   gfs.YYYYMMDD/CC/atmos                <- contains deterministic long forecast gfs inputs/outputs (atmosphere)
   gfs.YYYYMMDD/CC/wave                 <- contains deterministic long forecast gfs inputs/outputs (wave)
   logs/                                <- logs for each cycle in the run
   vrfyarch/                            <- contains files related to verification and archival

Here is an example ``COMROT`` for a cycled run as it may look several cycles in (note the archival steps remove older cycle folders as the run progresses):

::

   -bash-4.2$ ll /scratch1/NCEPDEV/stmp4/Joe.Schmo/comrot/testcyc192
   total 88
   drwxr-sr-x   4 Joe.Schmo stmp  4096 Oct 22 04:50 enkfgdas.20190529
   drwxr-sr-x   4 Joe.Schmo stmp  4096 Oct 22 07:20 enkfgdas.20190530
   drwxr-sr-x   6 Joe.Schmo stmp  4096 Oct 22 03:15 gdas.20190529
   drwxr-sr-x   4 Joe.Schmo stmp  4096 Oct 22 07:15 gdas.20190530
   drwxr-sr-x   6 Joe.Schmo stmp  4096 Oct 22 03:15 gfs.20190529
   drwxr-sr-x   4 Joe.Schmo stmp  4096 Oct 22 07:15 gfs.20190530
   drwxr-sr-x 120 Joe.Schmo stmp 12288 Oct 22 07:15 logs
   drwxr-sr-x  13 Joe.Schmo stmp  4096 Oct 22 07:07 vrfyarch

