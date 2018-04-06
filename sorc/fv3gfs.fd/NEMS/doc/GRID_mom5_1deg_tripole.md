Modular Ocean Model 5 (MOM5) Tripole Grid (1 deg) {#GRID_mom5_1deg_tripole}
=================================================

Description
-----------

This page describes a MOM5 grid that is a tripolar at 1 degree
resolution. The Murray (1996) tripolar grid is a regular spherical
grid south of 65N and bipolar north of 65N. Longitude runs from -279.5
to 79.5 degrees with exact 1 degree spacing; and laittude runs from
-81.5 (edge of antarctic land) to -89.5 degrees with uniform 1 degree
spacing outside the tropical region where gaussian spacing is used so
the equitorial region has a higher resolution.
 
| Long Name                     | Name   | Value  |
| :---------------------------- | :----- | :----- |
| Number of longitudinal points |   N<sub>i</sub>   |  360   |
| Number of latitudinal points  |   N<sub>j</sub>   |  200   |
| Minimum longitude             | &nbsp; | -279.5 |
| Maximum longitude             | &nbsp; | 79.5   |
| Minimum latitude              | &nbsp; | -81.5  |
| Maximum latitude              | &nbsp; | 89.5   |

Longitude Plot
--------------

\image html  GRID_mom5_1deg_tripole-ocnlon.gif

Latitude Plot
-------------

\image html  GRID_mom5_1deg_tripole-ocnlat.gif

Mask Plot
---------

\image html GRID_mom5_1deg_tripole-ocnmask.gif

Data Decomposition
------------------

The grid and data decomposition is done regularly on a preferred 6x4
processor layout. The 360 longitudinal values is regularly decomposed
into 6 ranges and the 200 latitudinal values is regularly decomposed
into 5 ranges.

* \subpage GRID_mom5_lonlat

For example, on the first processor, longitude range is [-279.5,
-220.5], latitude range is [-81.5, -32.5]; on the second processor,
longitude range is [-219.5, -160.5], latitude range is [-81.5, -32.5];
and finally on the last processor, longitude range is [20.5, 79.5],
latitude range is [40.5, 89.5].  

References
----------

1. Murray, R. J., 1996: Explicit generation of orthogonal grids for
  ocean models. Journal Computational Physics, 126, 251--273

2. http://nomads.gfdl.noaa.gov/CM2.X/documentation/ocean_grid_doc.html

3. Section 4.2 http://data1.gfdl.noaa.gov/~arl/pubrel/r/mom4p1/src/mom4p1/doc/guide4p0.pdf

4. GridSpec file describing the grid on Zeus:  /home/Fei.Liu/noscrub/global_box1/INPUT/grid_spec.nc

\todo The referenced GridSpec file on Zeus is gone because the NOAA
Zeus cluster was retired.  Either remove reference \#4 or find the
file