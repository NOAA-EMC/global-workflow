Space Weather Mediator            {#sw_mediator}
======================

The Space Weather Mediator is designed to transfer 3D data between the
Whole Atmosphere Model (WAM) and the Ionosphere Plasmasphere
Electrodynamics (IPE) model. The Space Weather Mediator is a 
[National Unified Operational Prediction Capability (NUOPC)](https://www.earthsystemcog.org/projects/nuopc/)
mediator and so contains interfaces and methods to pass data between
two components as part of a NUOPC coupled system. Currently, this
mediator has only been tested with the
[DATAWAM](https://esgf.esrl.noaa.gov/projects/wam_ipe/DATA_WAM)
version of WAM and the
[DATAIPE](https://esgf.esrl.noaa.gov/projects/wam_ipe/DATA_IPE)
version of IPE. Over the next several months, this project's goal is
to connect it to the actual WAM and IPE models. Currently, this
mediator only transfers data from WAM to IPE. However, at the end of
the current phase of this project, the goal is to have data going in
both directions.

One special feature of this mediator is that it works with a varying
3D height field. The WAM grid's vertical coordinate is at fixed
pressure levels, so its actual height varies timestep by timestep. The
mediator is designed to operate with the varying height field without
the expense of recalculating the full regridding matrix every
timestep. It does this by using a 3D fixed-height intermediate grid.

During initialization in the mediator, it defines the 3D fixed-height
intermediate grid. The fixed heights are determined using global
average heights derived from a WAM model run in a "warm" simulation
time.  The heights are then extended up to 800 km because the height
at each point may vary and go beyond the highest average height level.
The 3D coordinates for this intermediate grid are precalculated and
stored in a NetCDF file. They are read in to create the grid during
initialization. Once this grid has been created, a regrid matrix is
calculated between the fixed-height intermediate grid and the IPE
grid. Since neither of these grids change, this matrix can be used to
interpolate between the intermediate grid and IPE grids during the
entire run.

While the system is running for each timestep, the 3D fields coming
from WAM are linearly interpolated into the 3D fixed-height
intermediate grid using the height field coming from WAM. The
intermediate grid is built to have the same distribution and
horizontal coordinates as the WAM grid, so the linear interpolation is
an inexpensive 1D linear interpolation. Once the data is on the
intermediate grid, the precalculated regrid matrix is used to transfer
it to the IPE grid.
