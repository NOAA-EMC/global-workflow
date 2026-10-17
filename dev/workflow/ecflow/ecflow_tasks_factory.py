"""
Factory for creating ecFlow task objects.

Mirrors ``rocoto/tasks_factory.py``: registers per-NET task classes
that the suite generator instantiates to get per-task ecFlow dicts.
"""
from wxflow import Factory
from ecflow.gfs_ecflow_tasks import GFSEcFlowTasks

ecflow_tasks_factory = Factory('ecFlowTasks')

ecflow_tasks_factory.register('gfs', GFSEcFlowTasks)
# ecflow_tasks_factory.register('gefs', GEFSEcFlowTasks)
# ecflow_tasks_factory.register('sfs', SFSEcFlowTasks)
# ecflow_tasks_factory.register('gcafs', GCAFSEcFlowTasks)
