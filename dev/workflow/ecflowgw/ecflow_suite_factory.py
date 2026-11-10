"""
Factory for creating ecFlow suite generators.

This module provides a factory that creates appropriate ecFlow suites
for different types of workflows (GFS, GEFS, SFS, GCAFS).
"""
from wxflow import Factory
import pkgutil
from importlib import import_module
from ecflowgw.ecflow_suite import EcFlowSuite
import inspect
import os

# Create a factory for ecFlow suite objects
ecflow_suite_factory = Factory('ecFlowSuite')

_ecflowgw_dir = os.path.join(os.path.dirname(__file__))

# Go through all of the modules in the ecflow package and import them dynamically
for loader, module_name, is_pkg in pkgutil.iter_modules([_ecflowgw_dir]):
    # Get the list of importable classes with EcFlowSuite in their name
    if not module_name.endswith('ecflow'):
        continue
    # Skip the ecflow_definitions and ecflow_setup modules as they do not contain EcFlowSuite classes
    # and require access to the ecflow module, which is not guaranteed at this point.
    module = import_module(f"ecflowgw.{module_name}")
    class_names = [name for name in dir(module)
                   if inspect.isclass(getattr(module, name)) and
                   issubclass(getattr(module, name), EcFlowSuite) and
                   name != "EcFlowSuite"]
    for class_name in class_names:
        app_class = getattr(module, class_name)
        # Register application configuration
        # EcFlowSuite classes are expected to be named as <AppName>EcFlowSuite, e.g.,
        # GFSCycledEcFlowSuite
        # Name the registered name as <app_name>_<mode>, e.g., gfs_cycled
        app_name = class_name.split('EcFlowSuite')[0].lower()
        if 'cycled' in class_name.lower():
            mode = 'cycled'
            _class_mode = 'cycled'
        elif 'forecastonly' in class_name.lower():
            mode = 'forecast-only'
            _class_mode = 'forecastonly'
        else:
            # This is a class with only a default mode (e.g., GEFS): forecast-only
            mode = 'forecast-only'
            _class_mode = 'null'

        app = class_name.lower().split('ecflowsuite')[0].split(_class_mode)[0]

        # Actually perform the registration
        ecflow_suite_factory.register(f"{app}_{mode}", app_class)
