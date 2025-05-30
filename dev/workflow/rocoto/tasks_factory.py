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
from rocoto.gfs_tasks import GFSTasks
from rocoto.gefs_tasks import GEFSTasks
from rocoto.sfs_tasks import SFSTasks
from rocoto.gcafs_tasks import GCAFSTasks

# Initialize the tasks factory
tasks_factory = Factory('Tasks')

# Register task classes for each forecast system
tasks_factory.register('gfs', GFSTasks)
tasks_factory.register('gefs', GEFSTasks)
tasks_factory.register('sfs', SFSTasks)
tasks_factory.register('gcafs', GCAFSTasks)
