import sys
import xarray as xr
import numpy as np

# sys.argv is a list of command-line arguments, where sys.argv[0] is the script name
input_file = sys.argv[1]
output_file = sys.argv[2]

# Load your dataset (e.g., from a NetCDF file)
ds = xr.open_dataset(input_file)

# 1. Load large dataset with Dask chunking
# Chunking by spatial/temporal dimensions, keeping the vertical (depth) dimension contiguous
ds = xr.open_dataset(input_file)
temp = ds.temp  # Assuming 'temp' is temperature
depths = ds.z_l # Vertical coordinate


def find_iso_depth(t_profile, z_coords, iso_val=20.0):
    """Linearly interpolate depth at a specific temperature value."""
    # Ensure temperature is increasing for np.interp by flipping arrays
    # Ocean profiles usually go from warm (surface) to cold (deep)

    # Check if the isotherm is within the range of the profile
    if not (np.min(t_profile) <= iso_val <= np.max(t_profile) or \
            np.max(t_profile) <= iso_val <= np.min(t_profile)):
        return np.nan # Isotherm not found in this profile

    return np.interp(iso_val, t_profile[::-1], z_coords[::-1])


# 2. Vectorize the operation across all 4D dimensions
dt20c = xr.apply_ufunc(
    find_iso_depth,
    temp,
    depths,
    input_core_dims=[['z_l'], ['z_l']],  # Apply along the depth axis
    output_core_dims=[[]],
    vectorize=True,
    dask="parallelized",
    output_dtypes=[float]
)

# 3. Trigger computation and save results

dt20c.attrs["units"]="Meters"
dt20c.attrs["name"]="Depth of 20 degC isotherm"
dt20c.name = 'dt20c'
dt20c.to_netcdf(output_file)
