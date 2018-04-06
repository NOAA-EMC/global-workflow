CICE Grid gx3 {#GRID_CICE_gx3}
=============

Description
-----------

This pge describes a CICE 3 degree global lat-lon POP grid (gx3). The
gx3 grid is a regular spherical grid in both hemispheres.
 
| Long Name                     | Name   | Value   |
| :---------------------------- | :----- | :------ |
| Number of longitudinal points | N<sub>i</sub>   | 100     |
| Number of latitudinal points  | N<sub>j</sub>   | 116     |
| Minimum longitude             | &nbsp; | 0.0     |
| Maximum longitude             | &nbsp; | 360.0   |
| Minimum latitude              | &nbsp; | -78.007 |
| Maximum latitude              | &nbsp; | 89.90   |
 
Longitude Plot
--------------

\image html GRID_CICE_gx3-lon.gif

Latitude Plot
-------------

\image html GRID_CICE_gx3-lat.gif
 
Mask Plot
---------

  Not yet available

 
Data Decomposition
------------------

The CICE grid is decomposed into regular 2-dimensional blocks.  These
blocks are then distributed to the PETs.  The size of the blocks is a
compile time setting, while the distribution of the blocks is a run
time namelist setting.  The CICE model threads over blocks.  The
optimum decomposition is dependent on a number of things including the
cost of halo updates and the location of sea ice.  The user has quite
a bit of flexibility in setting the CICE decompositon.