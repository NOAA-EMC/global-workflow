Global Spectral Model (GSM) Grid {#GRID_gsm}
================================

Description
-----------

The global Gaussian T126 grid was chosen for the initial
atmosphere-ocean coupled system.  A reduced Gaussian grid with
shuffled latitudes is used internally in the GSM.  As of DREV43400,
the full grid is used in the coupler, although this may change in the
future.  The reference for the table below is [1].

The latitudes in a Gaussian grid are nearly but not quite equally
spaced. The sines of the Gaussian grid latitudes are the zeroes of the
Legendre polynomial of order 190 and are used for Gaussian quadrature.
Note that the starting longitude is 0.0E and ending longitude is
359.0625E at the equator.  In the reduced grid, the ending longitudes
change as you go toward the poles.

| Long Name                                   | Nameg   | Value               |
| :------------------------------------------ | :----- | :------------------ |
| Number of longitudinal points               | N<sub>i</sub>   | 384                 |
| Number of latitudinal points                | N<sub>j</sub>   | 190                 |
| Northernmost latitude                       | La<sub>1</sub>  | 89.277N             |
| Easternmost longitude                       | Lo<sub>1</sub>  | 0.000E              |
| Southernmost latitude                       | La<sub>2</sub>  | 89.277S             |
| Westernmost longitude                       | Lo<sub>2</sub>  | 359.0625E = 0.9375W |
| Longitudinal increment                      | D<sub>i</sub>     | .9375 &deg          |
| Number of latitude circles, pole to equator | N      | 95                  |

 
Data Decomposition
------------------

The data decomposition is based on a "shuffled" row-only distribution
for better load balance.  The algorithm works as follows:

 * Sort the rows in the descending order of the number of points per
   row.  The rows with the most number of points will be the first.
   (See example for the 
   \ref GRID_wam "WAM grid".)

 * Card deal the rows in the above sorted order to each processors,
   one at a time.  For instance, if four processors are used,
   processor \#1 will get rows 47, 43, 39, 35, 60, ... and processor \#2
   will get rows 46, 42, 38, 48, 59, etc...)

Reference

1 [Master List of NCEP Storage Grids, GRIB Edition 1 (FM92)](http://www.nco.ncep.noaa.gov/pmb/docs/on388/tableb.html), grid is number 126.
