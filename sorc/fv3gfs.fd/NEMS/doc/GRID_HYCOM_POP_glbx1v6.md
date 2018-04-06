HYbrid Coordinate Ocean Model (HYCOM) Grid POP glbx1v6 {#GRID_HYCOM_POP_glbx1v6}
======================================================

Description
-----------

HYCOM runs on a 1.125 degree global lat-lon POP grid (glbx1v6). The
`glbx1v6` grid is a regular spherical grid in both
hemispheres. Longitude runs from -116.95999° to 319.59000° with
1.125 degree spacing; and latitude runs from -79.22000° to
89.71000°.
 
| Long Name                     | Name   | Value      |
| :---------------------------- | :----- | :--------- |
| Number of longitudinal points | N<sub>i</sub>   | 320        |
| Number of latitudinal points  | N<sub>j</sub>   | 384        |
| Minimum longitude             | &nbsp; | -116.95999 |
| Maximum longitude             | &nbsp; | 319.59000  |
| Minimum latitude              | &nbsp; | -79.22000  |
| Maximum latitude              | &nbsp; | 89.31000   |


Longitude Plot
--------------

\image html GRID_HYCOM_POP_glbx1v6-hycom_GLBx_lon.png

Latitude Plot
-------------

\image html GRID_HYCOM_POP_glbx1v6-hycom_GLBx_lat.png

Mask Plot
---------

\image html GRID_HYCOM_POP_glbx1v6-hycom_GLBx_msk.png

Data Decomposition
------------------

The grid and data decomposition is done in the following manner:

1. The latitudes are regularly decomposed into jqr=5 bands, leading to
76 and 77 latitude wide bands.

2. Each latitude band is decomposed into blocks along the
longitude. The actual size of each block, and the number of blocks is
flexible within some limits in order to allow for load balancing. The
limits are set by iqr=8, the maximum number of blocks in each band,
and idm=77, the maximum number of longitudes per block.

3. Every PET (persistent execution thread, i.e. MPI rank) is
associated with exactly one lat-lon block. Here each blocks contains
ocean and must be associated with a PET. There are 40 blocks and 40
PETs.

 
\image html GRID_HYCOM_POP_glbx1v6-depth_POP1v6_01.040.png
