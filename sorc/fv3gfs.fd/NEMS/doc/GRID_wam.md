Whole Atmosphere Model (WAM) Grid {#GRID_wam}
=================================

Description
-----------

The WAM grid is the same as the 
\ref GRID_gsm "Global Spectral Model (GSM)"
grid except that it is extended in levels.  It is a global 3D reduced
Gaussian grid, and the target resolution for coupling to the
\ref GRID_IPE "Ionosphere Plasmasphere Electrodynamics (IPE)"
grid is T62.  The
vertical layer is in pressure and has to be converted to height in
order to couple with IPE.

<table>
<tr>
    <th>Long Name</th><th>Name</th><th>Value</th>
</tr><tr>
  <td>Number of longitudinal points (In Reduced Gaussian distribution
    with min. 30 at the highest and lowest latitudes and max. 192 at
    the equator)</td>
  <td>N<sub>i</sub></td><td>192 max</td>
</tr><tr>
  <td>Number of latitudinal points</td>
  <td>N<sub>j</sub></td>
  <td>94</td>
</tr><tr>
  <td>From .35km to 591.44km (may vary dynamically)</td>
  <td>Levels</td>
  <td>150</td>
</tr>
</table>
 
Data Decomposition
------------------

The data decomposition is based on a "shuffled" row-only distribution
for better load balance.  The algorithm works as follows:

 * Sort the rows in the descending order of the number of points per
   row.  The rows with the most number of points will be the first.
   (For example, for the WAM grid, the sorted row indices are: 47 46
   45 44 43 42 41 40 39 38 37 36 35 48 49 61 60 59 58 57 56 55 54 53
   52 51 50 34 29 64 65 66 67 62 63 28 33 32 31 30 27 69 68 26 70 71
   25 24 72 23 20 19 21 73 22 75 74 76 77 18 79 17 78 16 80 81 14 15
   13 82 83 12 11 84 85 10 9 8 87 86 88 76 89 90 5 91 4 93 92 1 3 2
   94)

 * Card deal the rows in the above sorted order to each processors,
   one at a time.  For instance, if four processors are used,
   processor #1 will get rows 47, 43, 39, 35, 60, ... and processor #2
   will get rows 46, 42, 38, 48, 59, etc...)

