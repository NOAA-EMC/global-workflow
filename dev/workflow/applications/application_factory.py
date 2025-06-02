"""
Factory module for application configurations.

This module provides a factory pattern implementation for creating
application configurations for different forecast systems including
GFS, GEFS, SFS, and GCAFS. It registers each application configuration
class with the wxflow Factory to allow dynamic creation based on
application name and mode.

Notes
-----
Uses the wxflow Factory class to register and create application configurations.
Each application has its own configuration class that inherits from AppConfig.
"""

from wxflow import Factory
from applications.gfs_cycled import GFSCycledAppConfig
from applications.gfs_forecast_only import GFSForecastOnlyAppConfig
from applications.gefs import GEFSAppConfig
from applications.sfs import SFSAppConfig
from applications.gcafs_forecast_only import GCAFSForecastOnlyAppConfig

# Initialize the application configuration factory
app_config_factory = Factory('AppConfig')

# Register application configurations
app_config_factory.register('gfs_cycled', GFSCycledAppConfig)
app_config_factory.register('gfs_forecast-only', GFSForecastOnlyAppConfig)
app_config_factory.register('gefs_forecast-only', GEFSAppConfig)
app_config_factory.register('sfs_forecast-only', SFSAppConfig)
app_config_factory.register('gcafs_forecast-only', GCAFSForecastOnlyAppConfig)
