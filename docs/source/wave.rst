==============
Wave Component
==============

The wave model, WAVEWATCH III, is one component of the forecast model. This documentation is a work in progress 
and currently contains information about how to add a new grid to the workflow.

^^^^^^^^^^^^^^^^^^^^^^
Adding a new wave Grid 
^^^^^^^^^^^^^^^^^^^^^^

********************
Naming the Wave Grid 
********************

The naming convention of the wave grid within the workflow is expected to follow a convention of region underscore resolution. 
Here are several regional naming conventions: 

+-----------+-----------------------+
| Acronym   | Description           |
+===========+=======================+
| glo       | Global domain         |
+-----------+-----------------------+
| uglo      | Unstructured global   |
+-----------+-----------------------+
| ak        | Alaska                |
+-----------+-----------------------+
| ao or aoc | Arctic Ocean          |
+-----------+-----------------------+ 
| at        | Atlantic              |
+-----------+-----------------------+ 
| ep        | East Pacific          |
+-----------+-----------------------+ 
| gnh       | Global Northern Ocean |
+-----------+-----------------------+ 
| gsh       | Global Southern Ocean |
+-----------+-----------------------+ 
| so        | Southern Ocean        |
+-----------+-----------------------+ 
| wc        | West Coast            | 
+-----------+-----------------------+


Here are examples of resolution names: 

+---------+----------------------+
| Acronym | Description          |
+=========+======================+
| 15m     | 15 min               |
+---------+----------------------+
| 15mxt   | 15 min extended grid |
+---------+----------------------+ 
| 025     | 0.25 degrees         |
+---------+----------------------+ 
| 9km     | 9 km                 |
+---------+----------------------+ 

This new grid name will now be referred to as ${WAVEGRID} for the following documentation. 

****************
Adding Fix Files 
****************

The following is a table of fix files to be added with a new grid. 

+-------------------------------------+----------------------------------------+--------------------------------------------------+
| Name of File                        |  File Description                      | Notes/Requriements                               |
+=====================================+========================================+==================================================+ 
| ww3_grid.inp_${WAVEGRID}            | Grid Input File                        | Required for all new grids                       | 
+-------------------------------------+----------------------------------------+--------------------------------------------------+
| mesh.${WAVEGRID}.nc                 | Mesh grid file used in forecast        | This is required if this is a computational grid | 
+-------------------------------------+----------------------------------------+--------------------------------------------------+
| ww3_gint.WHTGRIDINT.bin.${WAVEGRID} | Weight file for ww3_gint interpolation | Required if output is interpolated to this grid  |
+-------------------------------------+----------------------------------------+--------------------------------------------------+


While the creation of these files are generally considered out of scope of this document here are some additional information: 

* The ww3_grid.inp_${WAVEGRID} for structured grids can be created with https://github.com/noaA-EMC/gridgen  
* The ww3_grid.inp_${WAVEGRID} file must be a single file for the workflow 
* Instructions for creating mesh.${WAVEGRID}.nc can be found at https://ufs-weather-model.readthedocs.io/en/latest/InputsOutputs.html#ww3 
* The ww3_gint.WHTGRIDINT.bin.${WAVEGRID} can be created by running the ww3_gint routine as desired and then saved. 

Once the new fix files have been created, `open an issue to have the master fix file directory updated <https://github.com/NOAA-EMC/global-workflow/issues/new?assignees=KateFriedman-NOAA%2CWalterKolczynski-NOAA&labels=Fix+Files&projects=&template=fix_file.md>`. This is a separate step than the process to update the workflow below.

********************************
Updating Config and Script Files  
******************************** 

You will need to update the following files: 
 
* parm/config/\*/config.ufs
* parm/config/\*/config.wave
* scripts/exgfs_wave_post_gridded_sbs.sh

You will need to add the following files: 

* parm/wave/ww3_grib2.${WAVEGRID}.inp.tmpl
* parm/wave/${WAVEGRID}_interp.inp.tmpl

For config.ufs: 

If this is a computational grid, you will need to update this file. If this is a new output file you can skip this update.
There are two sections to update: 
1. Add the new grid as a possible ww3 resolution at the top of the file 
2. In the WW3 specific settings section, add a case for the new grid and define ntasks_ww3 (number of tasks for WW3) and 
if the grid requires a different value of nthreads_ww3 (number of threads) add that to the grid specific case as well. 

For config.wave: 

If this is a computational grid, add a section for the newly added grid in the possible cases for waveGRD, adding the appropriate
non-default values for this specific grid.  If this is a new output grid, then update all of the waveGRD cases for waveinterpGRD as 
needed.  Please note that if you change the default wave grid, that initial conditions must be staged for this IC for all ICs used 
in global-workflow testing.  Ideally ICs for the grids being used for testing will be provided even if its not the default grid. 



For scripts/exgfs_wave_post_gridded_sbs.sh and the inp.tmpl files: 

There are two case settings for either interpolated grid output or grib output for a computational grid that should be updated to 
add to the possible cases for this new grid as appropriate.   If it's a new interpolated grid, you will also need to add a 
parm/wave/${WAVEGRID}_interp.inp.tmpl file.  See WW3/model/inp/ww3_gint.inp for information about this file.  If it's a new 
computational grid or just an output grid you will need a new parm/wave/ww3_grib2.${WAVEGRID}.inp.tmpl file.  See the file 
WW3/model/inp/ww3_grib.inp for more infomration about this input file.  The other existing templated input files can be used 
as guides to create these new files. 

*****************************
Updates for New Product Grids 
*****************************

If this is a new file for AWIPS Products, additional updates are likely required.

**************
New Staged ICs 
**************

Depending on the desired use of the new grid, or if the default grid is changed, staged initial conditions for use in 
workflow testing will also be needed. 

For example, C384 S2SW need an IC for 20130101 00z and low resolution tests need an IC for 20210323 12z. 
