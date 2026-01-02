import sys
import xarray as xr
import numpy as np

# sys.argv is a list of command-line arguments, where sys.argv[0] is the script name
input_file = sys.argv[1]
output_file = sys.argv[2]

# 1. Load 4D dataset (Time, Depth, Lat, Lon) with Dask
ds = xr.open_dataset(input_file)

#set depth limit (in meters) for ocean heat content
depth_limit = 300

# 2. Define Physical Constants
RHO = 1025.0  # Seawater density (kg/m^3)
CP = 4000.0   # Specific heat capacity (J/kg*K)

# 3. Interpolate and Slice
ds_depth_limit = ds.sel(z_l=slice(0, 300))

# 4. Compute OHC via Vertical Integration
# .integrate() uses the trapezoidal rule and handles irregular depth spacing
OHC = (ds_depth_limit.temp * RHO * CP).integrate("z_l")

OHC.attrs['units'] = 'J/m^2'
OHC.attrs['long_name'] = f'Ocean Heat Content (0 to {depth_limit}m)'
OHC.name = 'ocnheat'

# 5. Save results to a 3D NetCDF (Time, Lat, Lon)
OHC.to_netcdf(output_file)
