import sys
import xarray as xr
import numpy as np

# sys.argv is a list of command-line arguments, where sys.argv[0] is the script name
input_file = sys.argv[1]
output_file = sys.argv[2]

# 1. Load 4D dataset with Dask (Time, Depth, Lat, Lon)
# Important: Keep depth as a single chunk for interpolation/integration

ds = xr.open_dataset(input_file, chunks={'time': 1, 'lat': 100, 'lon': 100})
temp = ds.temp  # Temperature array
depths = ds.z_l  # Vertical coordinate

# Constants
RHO = 1025.0  # kg/m^3 approximate for seawater.
CP = 4000.0  # J/(kg*K) - an approximation (3990 for seawater, 4200 for freshwater)


def calc_tchp_profile(t_prof, z_coords):
    """Function to calculate TCHP for a single 1D vertical profile."""

    # If surface is already < 26C, TCHP is nan
    if t_prof[0] < 26:
        return np.nan

    # Find D26 depth via linear interpolation
    # Profiles usually go surface (warm) to deep (cold)

    d26 = np.interp(26, t_prof[::-1], z_coords[::-1])

    # Mask temperatures below 26C for integration
    #t_minus_26 = np.where(t_prof >= 26, t_prof - 26, 0)
    t_minus_26 = np.where(t_prof >= 26, t_prof - 26, np.nan)

    # Integrate (T-26) from 0 to D26 using trapezoidal rule
    # Note: Integration only goes up to the interpolated D26
    mask = z_coords <= d26
    integration = np.trapezoid(t_minus_26[mask], z_coords[mask])

    tchp_J_per_m2 = RHO * CP * integration  # in J/m^2

    # Convert to TCHP units (kJ/cm^2)
    # 1 J/m^2 = 1e-3 kJ / 1e4 cm^2 = 1e-7 kJ/cm^2
    tchp = tchp_J_per_m2 * 1e-7

    return tchp


# 2. Apply the function across 4D space using apply_ufunc
TCHP = xr.apply_ufunc(
    calc_tchp_profile,
    temp,
    depths,
    input_core_dims=[['z_l'], ['z_l']], # Core dimension for interpolation
    output_core_dims=[[]],
    vectorize=True,                         # Automatically loops over non-core dims
    dask="parallelized",                    # Enables Dask parallel processing
    output_dtypes=[float]
)

TCHP.attrs['units'] = 'kJ/cm^2'
TCHP.attrs['long_name'] = 'Tropical Cyclone Heat Potential'
TCHP.name = 'TCHP'

# 3. Compute and Save
TCHP.to_netcdf(output_file)
