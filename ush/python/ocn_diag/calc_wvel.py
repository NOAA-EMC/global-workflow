import xarray as xr
import xgcm
from xgcm import Grid
import numpy as np
import sys

# sys.argv is a list of command-line arguments, where sys.argv[0] is the script name
input_file = sys.argv[1]
output_file = sys.argv[2]


def calculate_mom6_wvel(ds):
    """
    Calculates ocean vertical velocity (wo) in m/s.

    Args:
        ds (xr.Dataset or xr.DataArray): Dataset containing 'uh','vh','h','area_t',
                                         'deta_dt','xh','yh','xq','yq','z_l','z_i'
    Returns:
        xr.DataArray: wo in m/s.
    """

    # Define the axis configuration for xgcm
    # MOM6 typically uses 'xh'/'yh' for center points, 'xq'/'yq' for corner points
    # and 'z_l' for layer center, 'z_i' for layer interfaces
    # Create an xgcm Grid object, specifying the dimensions and their corresponding axes

    grid = xgcm.Grid(
        ds,
        coords={
            'X': {'center': 'xh', 'right': 'xq'},
            'Y': {'center': 'yh', 'right': 'yq'},
            'Z': {'center': 'z_l', 'outer': 'z_i'},
                },
        periodic=['X'], # Set to True if your domain is periodic in X or Y
        autoparse_metadata=False,
    )

    # 1. Select the thickness transorts (udxdy: m3/s) and relevant metrics
    uh = ds['uh']
    vh = ds['vh']
    h_t = ds['h']  # Layer thicknesses (h) at T-points in m
    area_t = ds['area_t']  # Surface area of T cells in m2
    w_0 = 0 - ds['deta_dt'].values[0,:,:]  # Surface vertical velocity approximated from the SSH tendency, positive downward

    # 2. Compute horizontal divergence of volume transports
    # The *grid.diff()* operation inherently handles the staggered grid locations

    div_h = (
        grid.diff(uh, 'X', boundary='fill') +
        grid.diff(vh, 'Y', boundary='fill')
    ) / area_t

    # 3. Calculate vertical velocity (w_l) by integrating from the bottom up (or top down)
    # boundary="fill" uses the fill_value at the starting boundary (top down if wvel_surf = 0 or bottom up from sea floor)
    # The result will be located at the 'outer' Z points (z_i)

    w_l = w_0 - (div_h * h_t).cumsum(dim='z_l', skipna=True)  # integrating from top (surface) down with surface boundary condition (w_0)

    # 4. Change the sign of w_l back to positive upward
    wvel = -w_l

    w55 = wvel.sel(z_l=55)  # at 55m depth
    w55.attrs['units'] = 'm/s'
    w55.attrs['long_name'] = 'Ocean Vertical Velocity at 55m'
    w55.attrs['positive'] = 'upward'
    w55.name = 'ocnvv55'

    return w55


# Example Usage:

# 1. Open MOM6 history files
ds = xr.open_dataset(input_file, decode_timedelta=True)

# 2. Modify data attributes related to both X/Y/Z-coordinates before creating Grid object

ds.xh.attrs['axis'] = 'X'
ds.xh.attrs['c_grid_axis_shift'] = 0.0  # Or 'center' (if supported)
ds.xq.attrs['axis'] = 'X'
ds.xq.attrs['c_grid_axis_shift'] = 0.5  # Or 'outer' (if supported)
ds.yh.attrs['axis'] = 'Y'
ds.yh.attrs['c_grid_axis_shift'] = 0.0  # Or 'center' (if supported)
ds.yq.attrs['axis'] = 'Y'
ds.yq.attrs['c_grid_axis_shift'] = 0.5  # Or 'outer' (if supported)
ds.z_l.attrs['axis'] = 'Z'
ds.z_l.attrs['c_grid_axis_shift'] = 0.0  # Or 'center' (if supported)
ds.z_i.attrs['axis'] = 'Z'
ds.z_i.attrs['c_grid_axis_shift'] = 0.5  # Or 'outer' (if supported)

# 3. Calculate w55 and save the data

wvel_result = calculate_mom6_wvel(ds)
wvel_result.to_netcdf(output_file)
