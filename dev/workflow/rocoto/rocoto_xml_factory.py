"""
Factory for creating Rocoto XML generators.

This module provides a factory that creates appropriate Rocoto XML generators
for different types of workflows (GFS, GEFS, SFS, GCAFS).
"""
from wxflow import Factory
import pkgutil
from rocoto.rocoto_xml import RocotoXML
from importlib import import_module
import inspect
import os

# Create a factory for Rocoto XML generator objects
rocoto_xml_factory = Factory('RocotoXML')

_rocoto_dir = os.path.join(os.path.dirname(__file__))

# Go through all of the modules in the rocoto package and import them dynamically
for loader, module_name, is_pkg in pkgutil.iter_modules([_rocoto_dir]):
    # Only include modules with 'xml' in their name
    if 'xml' not in module_name.lower():
        continue
    # Get the list of importable classes with RocotoXML in their name
    module = import_module(f"rocoto.{module_name}")
    class_names = [name for name in dir(module)
                   if inspect.isclass(getattr(module, name)) and
                   issubclass(getattr(module, name), RocotoXML) and
                   name != "RocotoXML"]
    for class_name in class_names:
        app_class = getattr(module, class_name)
        # Register XML configuration
        # RocotoXML classes are expected to be named as <AppName>RocotoXML, e.g.,
        # GFSCycledRocotoXML
        # Name the registered name as <app_name>_<mode>, e.g., gfs_cycled
        app_name = class_name.split('RocotoXML')[0].lower()
        if 'cycled' in class_name.lower():
            mode = 'cycled'
            _class_mode = 'cycled'
        elif 'forecastonly' in class_name.lower():
            mode = 'forecast-only'
            _class_mode = 'forecastonly'
        else:
            # This is a RocotoXML class with only one mode (e.g. GEFS): forecast-only
            mode = 'forecast-only'
            _class_mode = 'null'

        app = class_name.lower().split('rocotoxml')[0].split(_class_mode)[0]

        # Actually perform the registration
        rocoto_xml_factory.register(f"{app}_{mode}", app_class)
