"""
Factory module for creating workflow task objects.

This module provides a factory pattern implementation for creating
task objects for different forecast systems including GFS, GEFS,
SFS, and GCAFS. Each task object handles the creation and
configuration of workflow tasks specific to that system.

Methods
-------
None

Notes
-----
Uses the wxflow Factory class to register and create task objects.
Each forecast system has its own Tasks class that inherits from the base Tasks class.

Examples
--------
>>> from rocoto.tasks_factory import tasks_factory
>>> task_obj = tasks_factory.create('gcafs', app_config, run)
>>> task = task_obj.get_task('fcst')
"""

from wxflow import Factory
import pkgutil
from rocoto.tasks import Tasks
from importlib import import_module
import inspect
import os

# Initialize the tasks factory
tasks_factory = Factory('Tasks')

_rocoto_dir = os.path.join(os.path.dirname(__file__))

for loader, module_name, is_pkg in pkgutil.iter_modules([_rocoto_dir]):
    # Get the list of importable classes with Tasks in their name
    if not module_name.endswith('tasks'):
        continue
    module = import_module(f"rocoto.{module_name}")
    class_names = [name for name in dir(module)
                   if inspect.isclass(getattr(module, name)) and
                   issubclass(getattr(module, name), Tasks) and
                   name != "Tasks"]
    for class_name in class_names:
        task_class = getattr(module, class_name)
        # Register task configuration
        # Tasks classes are expected to be named as <AppName>Tasks, e.g.,
        # GCAFSForecastOnlyTasks
        # Name the registered name as <app_name>, e.g., gcafs
        app_name = class_name.split('Tasks')[0].lower()
        tasks_factory.register(app_name, task_class)
