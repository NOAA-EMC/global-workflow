import sys
import os
import xarray as xr
import numpy as np

# sys.argv is a list of command-line arguments, where sys.argv[0] is the script name
input_file = sys.argv[1]
output_file = sys.argv[2]

# Open MOM6 history netCDF file (include layer thickness 'h')
ds = xr.open_dataset(input_file)

# Extract temperature data (e.g., 'temp')
temperature = ds['temp']

# Define the target temperature value for the isotherm
isotherm_temp = 20.0

# Calculate the depth of the interfaces (cumulative sum of layer thicknesses 'h')
# Assumes 'h' has dimensions ('time', 'z_l', 'yh', 'xh')
# Ensure 'z_l' is the vertical dimension name.

depth_bottom = ds['h'].cumsum(dim='z_l')
depth_top = depth_bottom.shift(z_l=1, fill_value=0.0)

# Create a DataArray for mid-layer depths (optional, but useful for understanding)
# depth_mid = (depth_top + depth_bottom) / 2.0


def find_isotherm_depth(temperature_profile, depth_top_profile, depth_bottom_profile, isotherm_temp):
    """
    Calculates the depth of a specific isotherm within a single vertical profile
    using linear interpolation.

    Parameters:
    temperature_profile (1D numpy array): Temperature values for a single vertical profile.
    depth_top_profile (1D numpy array): Top boundary depths corresponding to the temperature layers.
    depth_bottom_profile (1D numpy array): Bottom boundary depths corresponding to the temperature layers.
    isotherm_temp (scalar): The target temperature for which to find the depth.

    Returns:
    float64: The depth of the isotherm, or NaN if the isotherm is not present in the profile.
    """
    
    # 1. Combine top/bottom depths to get cell interfaces
    # We assume constant cell thickness or that depth_top_profile[i+1] == depth_bottom_profile[i]
    # If the provided depths are boundaries of the layers, we only need a single depth array.
    # A common approach in oceanography is to use the center of the cells for the temperature value.
    
    # For this function to work with linear interpolation across cell *interfaces*, 
    # we need the depth interfaces and corresponding temperatures.
    
    # A simple approach is to find where the temperature crosses the isotherm value.

    # Find where temperature is greater than or equal to the isotherm
    # and where it is less than the isotherm.
    
    # This works best if temperature decreases with depth (or increases consistently).
    # We look for the first crossing point from warm to cold or vice versa.

    depths = (depth_top_profile + depth_bottom_profile) / 2.0 # Use cell centers for simplicity, or adjust for layer boundaries

    # Check if the isotherm is within the range of the profile
    if not (np.min(temperature_profile) <= isotherm_temp <= np.max(temperature_profile) or \
            np.max(temperature_profile) <= isotherm_temp <= np.min(temperature_profile)):
        return np.nan # Isotherm not found in this profile

    # Find the indices where the temperature crosses the isotherm value
    # We compare adjacent layers
    cross_indices = np.where(np.diff(np.sign(temperature_profile - isotherm_temp)) != 0)[0]

    if cross_indices.size > 0:
        # Get the first crossing index (shallowest occurrence)
        idx = cross_indices[0]
        
        # Linear interpolation between the two adjacent depths/temperatures
        temp1 = temperature_profile[idx]
        temp2 = temperature_profile[idx + 1]
        depth1 = depths[idx]
        depth2 = depths[idx + 1]
        
        # Interpolation formula: depth = depth1 + (depth2 - depth1) * (isotherm_temp - temp1) / (temp2 - temp1)
        isotherm_depth = depth1 + (depth2 - depth1) * (isotherm_temp - temp1) / (temp2 - temp1)
        
        return float(isotherm_depth)
    else:
        # Handle cases where the value is exactly the max/min but never crosses,
        # or the profile is isothermal at the value (unlikely with real data)
        # If no crossing is found, it means the entire profile is above or below the isotherm,
        # but the initial range check should prevent reaching here if within range.
        # We return NaN as a fallback for safety.
        return np.nan

# Apply this function across all horizontal points and time steps
# We use apply_ufunc to handle the xarray dimensions correctly
dt20c = xr.apply_ufunc(
    find_isotherm_depth,
    temperature,
    depth_top,
    depth_bottom,
    isotherm_temp,
    input_core_dims=[['z_l'], ['z_l'], ['z_l'], []],
    output_core_dims=[[]],
    vectorize=True,
    dask='parallelized',
    output_dtypes=[np.float64]
)

# The result is a DataArray with dimensions ('time', 'yh', 'xh')

dt20c.attrs["units"]="Meters"
dt20c.attrs["name"]="Depth of 20C isotherm"
dt20c.name = 'dt20c'

# Save the result
dt20c.to_netcdf(output_file)
