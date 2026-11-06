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
import pkgutil
from applications.applications import AppConfig
from importlib import import_module
import inspect
import sys
import os

# Initialize the application configuration factory
app_config_factory = Factory('AppConfig')
# Write it to stderr as well

# Get this file's path to dynamically import all application modules
_applications_dir = os.path.join(os.path.dirname(__file__))

# Go through all of the modules in the applications package and import them dynamically
for loader, module_name, is_pkg in pkgutil.iter_modules([_applications_dir]):
    # Get the list of importable classes with AppConfig in their name
    module = import_module(f"applications.{module_name}")
    class_names = [name for name in dir(module)
                   if inspect.isclass(getattr(module, name)) and
                   issubclass(getattr(module, name), AppConfig) and
                   name != "AppConfig"]
    for class_name in class_names:
        app_class = getattr(module, class_name)
        # Register application configuration
        # AppConfig classes are expected to be named as <AppName>AppConfig, e.g.,
        # GFSCycledAppConfig
        # Name the registered name as <app_name>_<mode>, e.g., gfs_cycled
        app_name = class_name.split('AppConfig')[0].lower()
        if 'cycled' in class_name.lower():
            mode = 'cycled'
            _class_mode = 'cycled'
        elif 'forecastonly' in class_name.lower():
            mode = 'forecast-only'
            _class_mode = 'forecastonly'
        else:
            # GEFS and SFS only have one mode (forecast-only)
            mode = 'forecast-only'
            _class_mode = 'null'

        app = class_name.lower().split('appconfig')[0].split(_class_mode)[0]

        # Actually perform the registration
        app_config_factory.register(f"{app}_{mode}", app_class)
