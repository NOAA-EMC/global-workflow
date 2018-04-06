HYbrid Coordinate Ocean Model (HYCOM) Grid GLBa0.24 {#GRID_HYCOM_GLBa0p24}
===================================================

Description
-----------

HYCOM runs on a 0.24 degree tri-polar global grid (GLBa0.24). The
GLBa0.24 tri-polar grid is a regular spherical grid south of 46.9°N
and bipolar north of 46.9°N. Longitude runs from 74.24° to 434.0°
with exact 0.24 degree spacing; and latitude runs from -78.6080° to
89.9332°.
 
| Long Name                     | Name   | Value    |
| :---------------------------- | :----- | :------- |
| Number of longitudinal points | N<sub>i</sub>   | 1500     |
| Number of latitudinal points  | N<sub>j</sub>   | 1100     |
| Minimum longitude             | &nbsp; | 74.24    |
| Maximum longitude             | &nbsp; | 434      |
| Minimum latitude              | &nbsp; | -78.6080 |
| Maximum latitude              | &nbsp; | 89.9332  |
 
Data Decomposition
------------------

The grid and data decomposition is done in the following manner:

1. The latitudes are regularily decomposed into jqr=10 bands, leading
to 110 latitude values per band.

2. Each latitude band is decomposed into blocks along the
longitude. The actual size of each block, and the number of blocks is
flexible within some limits in order to allow for load balancing. The
limits are set by iqr=20, the maximum number of blocks in each band,
and idm=150, the maximum number of longitudes per block.

3. Every PET (persistent execution thread, i.e. MPI rank) is
associated with exactly one lat-lon block. Not all blocks need to be
associated with PETs, allowing to map out blocks that are fully over
land.

\image html GRID_HYCOM_GLBa0p24-hycom_GLBa0.24_lon.png
\image html GRID_HYCOM_GLBa0p24-hycom_GLBa0.24_lat.png
\image html GRID_HYCOM_GLBa0p24-hycom_GLBa0.24_msk.png

 
