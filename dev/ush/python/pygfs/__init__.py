"""
pygfs
=====

This package provides task classes and utilities for the GFS workflow, including analysis, chemistry, ensemble, marine, snow, and forecast processing.

Modules
-------
- task.analysis: Analysis task
- task.chem_fire_emission: Chemistry fire emissions task
- task.nxs_emission: NEXUS emissions task
- task.aero_analysis: Aerosol analysis task
- task.aero_bmatrix: Aerosol background matrix task
- task.atm_analysis: Atmospheric analysis task
- task.atmens_analysis: Atmospheric ensemble analysis task
- task.ensemble_recenter: Ensemble recentering task
- task.fv3_analysis_calc: FV3 analysis calculation task
- task.marine_bmat: Marine background matrix task
- task.offline_analysis: Offline analysis task
- task.snow_analysis: Snow analysis task
- task.snowens_analysis: Snow ensemble analysis task
- task.upp: Unified Post Processor (UPP) task
- task.oceanice_products: Ocean/ice products task
- task.gfs_forecast: GFS forecast task
- utils.marine_da_utils: Marine data assimilation utilities
- task.fetch: Fetch task

Attributes
----------
__docformat__ : str
    The documentation format for the module.
__version__ : str
    The version of the pygfs package.
pygfs_directory : str
    The absolute path to the pygfs package directory.
"""

import os

from .task.analysis import Analysis
from .task.chem_fire_emission import ChemFireEmissions
from .task.nexus_emission import NEXUSEmissions
from .task.aero_analysis import AerosolAnalysis
from .task.aero_bmatrix import AerosolBMatrix
from .task.atm_analysis import AtmAnalysis
from .task.atmens_analysis import AtmEnsAnalysis
from .task.ensemble_recenter import EnsembleRecenter
from .task.fv3_analysis_calc import FV3AnalysisCalc
from .task.marine_bmat import MarineBMat
from .task.offline_analysis import OfflineAnalysis
from .task.snow_analysis import SnowAnalysis
from .task.snowens_analysis import SnowEnsAnalysis
from .task.upp import UPP
from .task.oceanice_products import OceanIceProducts
from .task.gfs_forecast import GFSForecast
from .utils import marine_da_utils
from .task.fetch import Fetch
from .task.marine_recenter import MarineRecenter

__docformat__ = "restructuredtext"
__version__ = "0.1.0"
pygfs_directory = os.path.dirname(__file__)
