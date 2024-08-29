
import os

from .task.analysis import Analysis
from .task.aero_emissions import AerosolEmissions
from .task.aero_analysis import AerosolAnalysis
from .task.atm_analysis import AtmAnalysis
from .task.atmens_analysis import AtmEnsAnalysis
from .task.snow_analysis import SnowAnalysis
from .task.snowens_analysis import SnowEnsAnalysis
from .task.upp import UPP
from .task.gfs_forecast import GFSForecast

# TODO Move this out of a try block once moving to spack-stack 1.8.0
try:
    # For spack-stack 1.6.0, the UPP requires its own environment
    # This environment does not include xarray, which is required
    # by marine tasks
    from .task.marine_bmat import MarineBMat
    from .task.oceanice_products import OceanIceProducts
    from .utils import marine_da_utils

except ModuleNotFoundError:
    print("WARNING: not all required python modules were found!")

__docformat__ = "restructuredtext"
__version__ = "0.1.0"
pygfs_directory = os.path.dirname(__file__)
