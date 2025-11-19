import sys
import xarray as xr
import numpy as np

# sys.argv is a list of command-line arguments, where sys.argv[0] is the script name
input_file = sys.argv[1]
output_file = sys.argv[2]


def calculate_OHC(ds, depth_limit=300, temp_var_name='temp', depth_dim_name='depth'):
    """
    Calculates Ocean Heat Content (OHC) in J/m^2 from surface to depth_limit.

    Args:
        ds (xr.Dataset or xr.DataArray): Dataset with 'temp', and 'depth'.
        depth_limit (int): The maximum depth for integration in meters (e.g., 300).
                         Assumes positive downwards depths for input data indexing.
        temp_var_name (str): Name of the temperature variable.
        depth_dim_name (str): Name of the vertical dimension.

    Returns:
        xr.DataArray: OHC anomaly in J/m^2.
    """

    temp = ds[temp_var_name]
    depths = ds[depth_dim_name]

    # Ensure depths are positive downwards for selection
    if np.any(np.diff(depths) < 0):
        temp = temp.isel({depth_dim_name: slice(None, None, -1)})
        depths = temp[depth_dim_name]

    # Select data down to the specified limit
    # Using .sel ensures we get data near the target depth (e.g. 700m)
    # Be careful with depth_limit sign here.
    if depth_limit < 0:
        depth_limit_val = np.abs(depth_limit)
    else:
        depth_limit_val = depth_limit

    # Mask data deeper than the integration limit
    temp_sliced = temp.where(temp[depth_dim_name] <= depth_limit_val)

    rho = 1025  # kg/m^3 approximate for seawater.
    Cp = 4000   # Specific heat capacity of seawater (J/kg/K)

    # --- Integrate over depth ---
    # T_ref = temp.mean(dim='time') # Reference temperature (e.g., long-term mean)
    # Or Reference temperature (e.g., simply T_ref=0)
    T_ref = 0

    # Calculate the heat anomaly (T - T_ref)
    T_anomaly = temp_sliced - T_ref
 
    # Calculate layer thicknesses (delta_z)
    # xarray can approximate this if you have cell bounds, or we use manual diff
    dz = np.abs(ds[depth_dim_name].diff(dim=depth_dim_name).values)
    # Pad dz to match original dimensions for broadcasting (simple approximation)
    dz_padded = np.insert(dz, 0, dz[0])
 
    # OHC is integral(rho * Cp * T_anomaly) dz
    # Multiply by density, specific heat capacity, and layer thickness
    ohc_areal_density = rho * Cp * T_anomaly * xr.DataArray(dz_padded, coords={depth_dim_name: depths}, dims=[depth_dim_name])

    # Sum along the depth dimension to get OHC per unit area (J/m^2)
    OHC = ohc_areal_density.sum(dim=depth_dim_name, skipna=True)

    OHC.attrs['units'] = 'J/m^2'
    OHC.attrs['long_name'] = f'Ocean Heat Content (0 to {depth_limit_val}m)'
    OHC.name = 'OHC'

    return OHC


# Example Usage (You will need an actual netCDF file):
ds = xr.open_dataset(input_file)
ohc_result = calculate_OHC(ds, depth_limit=300, temp_var_name='temp', depth_dim_name='z_l')
# Save the data
ohc_result.to_netcdf(output_file)
