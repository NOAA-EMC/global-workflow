==============
Wave Component
==============

The wave model, WAVEWATCH III, is one component of the forecast model. This documentation is a work in progress.

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


********************************
Updating Config and Script Files  
******************************** 





