import sys
import xarray as xr
import numpy as np

# sys.argv is a list of command-line arguments, where sys.argv[0] is the script name
input_file = sys.argv[1]
output_file = sys.argv[2]

# 1. Load 4D dataset (Time, Depth, Lat, Lon) with Dask
# CRITICAL: depth must be contiguous (chunks=-1) for vertical integration
ds = xr.open_dataset(input_file, chunks={'time': 1, 'lat': 100, 'lon': 100})

#customize depth limit (in meters) for ocean heat content
depth_limit = 300

# 2. Define Physical Constants
RHO = 1025.0  # Seawater density (kg/m^3)
CP = 4000.0   # Specific heat capacity (J/kg*K)

# 3. Interpolate and Slice
# Interpolating ensures the 300m boundary is precisely included even if
# model levels are at 280m and 320m
depth_grid = ds.z_l.values
if depth_limit not in depth_grid:
    # Add 300m point to the grid and sort
    new_depths = np.unique(np.append(depth_grid[depth_grid < depth_limit], depth_limit))
    ds_300 = ds.interp(z_l=new_depths, method="linear")
else:
    ds_300 = ds.sel(z_l=slice(0, 300))

# 4. Compute OHC via Vertical Integration
# .integrate() uses the trapezoidal rule and handles irregular depth spacing
OHC = (ds_300.temp * RHO * CP).integrate("z_l")

# 5 Add name and attributes and Save results to a 3D NetCDF (Time, Lat, Lon)
OHC.attrs['units'] = 'J/m^2'
OHC.attrs['long_name'] = f'Ocean Heat Content (0 to {depth_limit}m)'
OHC.name = 'ocnheat'
OHC.to_netcdf(output_file)
