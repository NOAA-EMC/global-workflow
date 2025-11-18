import sys
import os
import xarray as xr
import numpy as np

# sys.argv is a list of command-line arguments, where sys.argv[0] is the script name
input_file = sys.argv[1]
output_file = sys.argv[2]


# Open your netCDF file
ds = xr.open_dataset(input_file)

# Define the isotherm value
isotherm_value = 20.0
depth_dim_name = 'z_l'

# Find the depth where temperature is close to the isotherm value
# .max() or .min() depending on whether depth is positive down (standard for ocean data) or up
# This example uses interpolation for a cleaner result
d20c = ds.z_l.where(ds.temp >= isotherm_value).max(dim='z_l',skipna=True) 
d20c.attrs["units"]="Meters"
d20c.attrs["name"]="Depth of 20C isotherm"
d20c.name = 'D20'
# Save the result
d20c.to_netcdf(output_file)
