Ionosphere-Plasmasphere Electrodynamics (IPE) Grid {#GRID_IPE}
==================================================

Description
-----------

Only a portion of the IPE grid (that overlaps with the 
\ref GRID_wam "Whole Atmosphere Model (WAM)"
grid up to ~700 km) needs to be represented for coupling to WAM.

| Long Name                                              | Name          | Value |
| :----------------------------------------------------- | :------------ | :---- |
| Maximum number of gridpoints along a single flux tube* | `MaxFluxTube` | 1115  |
| Number of flux tubes in magnetic latitude              | `NLP`         | 170   |
| Number of flux tubes in magnetic longitude             | `NMP`         | 80    |
 

\* Actual number of grid points along a single flux tube can vary from
11 to 1115 depending on the length of the flux tube.  

Data Decomposition
------------------

All the grid points along a flux tube are contained on the same
processor (equivalent to a vertical column for GSM).  Domain
decomposition is done in both latitude and longitude
directions. Several field lines may be on the same processor.  The
model usually runs on 80 processors for the coupling to GSM, although
the standalone IPE can run with flexible number of processors
speficied at run time.

Reference
---------

The entire IPE 3D grid file on Zeus:

 * /scratch1/portfolios/NCEPDEV/swpc/noscrub/Naomi.Maruyama/grid/apex/GIP_apex_coords_global_lowres_new20120705

\todo Remove reference to IPE 3D grid file on non-existent Zeus
machine, or move it to an accessible location.