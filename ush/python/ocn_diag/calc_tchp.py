import sys
import xarray as xr
import numpy as np

# sys.argv is a list of command-line arguments, where sys.argv[0] is the script name
input_file = sys.argv[1]
output_file = sys.argv[2]


def calculate_TCHP(ds, temp_var_name='temp', depth_dim_name='depth'):
    """
    Calculates Tropical Cyclone Heat Potential (TCHP) in kJ/cm^2.

    Args:
        ds (xr.Dataset or xr.DataArray): Dataset containing 'temp' and 'salt'.
        temp_var_name (str): Name of the temperature variable.
        depth_dim_name (str): Name of the vertical dimension.

    Returns:
        xr.DataArray: TCHP in kJ/cm^2.
    """
 
    # We ignore the impact of temp and salinity on density
    temp = ds[temp_var_name]
    depths = ds[depth_dim_name]
 
    TARGET_TEMP = 26.0
 
    # --- Step 1: Find the depth of the 26°C isotherm (D26) ---
    # We use masking to find the shallowest depth where temp is >= 26C.
    # A precise, interpolated method is better, but this finds the top of the 26C layer.
    depth_26C = depths.where(temp >= TARGET_TEMP).max(dim=depth_dim_name, skipna=True)
 
    # Replace NaNs (where 26C isotherm is not present) with a deep depth for integration limit
    # (e.g., max depth of the data, or a standard deep value)
    max_depth = depths.max().item()
    d26_filled = depth_26C.fillna(max_depth)

    # --- Step 2: Integrate excess heat from surface to D26 ---
    # Excess heat is the difference between current temp and 26C, but only when >= 26C
    # Specific heat capacity of seawater (approx constant J/(kg*K))
    # Use rho and Cp directly
    rho = 1025.0  # kg/m^3 approximate for seawater.
    Cp = 4000  # J/(kg*K) - an approximation (3990 for seawater, 4200 for freshwater)

    # Mask temperatures below 26C to NaN so they are ignored in integration
    excess_temp = temp.where(temp >= TARGET_TEMP) - TARGET_TEMP

    # Calculate Ocean Heat Content (OHC) in J/m^2
    # The integration requires careful handling of vertical levels.
    # Integrate depth-by-depth up to D26
    # Create a mask that is True from surface down to (but not past) D26 at each point
    depth_mask = ds[depth_dim_name] <= d26_filled

    # Apply mask and calculate OHC (approximate integral using the depth delta)
    # A simplified calculation using layer thicknesses:
    # Calculate layer thicknesses (assuming uniform spacing for simplicity here, adjust as needed)
    dz = np.abs(ds[depth_dim_name].diff(dim=depth_dim_name))
    # Pad dz to match original dimensions for broadcasting
    dz = dz.pad({depth_dim_name: (0, 1)}, constant_values=0)

    # OHC is integral(rho * Cp * (T - 26)) dz
    ohc_J_per_m2 = (rho * Cp * excess_temp * dz).sum(dim=depth_dim_name, skipna=True)

    # Convert to TCHP units (kJ/cm^2)
    # 1 J/m^2 = 1e-3 kJ / 1e4 cm^2 = 1e-7 kJ/cm^2
    TCHP = ohc_J_per_m2 * 1e-7

    # Set NaN values back where TCHP couldn't be calculated (e.g. 26C isotherm wasn't present)
    TCHP = TCHP.where(~np.isnan(depth_26C))
 
    TCHP.attrs['units'] = 'kJ/cm^2'
    TCHP.attrs['long_name'] = 'Tropical Cyclone Heat Potential'
    TCHP.name = 'TCHP'

    return TCHP


# Example Usage:
ds = xr.open_dataset(input_file)
tchp_result = calculate_TCHP(ds, temp_var_name='temp', depth_dim_name='z_l')
tchp_result.to_netcdf(output_file)
