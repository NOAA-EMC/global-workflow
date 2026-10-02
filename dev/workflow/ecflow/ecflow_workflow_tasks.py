#!/usr/bin/env python3

"""
Connector that produces ecFlow task dicts for all tasks in an AppConfig.

Mirrors ``rocoto/workflow_tasks.py``.  The suite generator already does
this internally, but this module provides a standalone entry point for
tools that need the full task dict list without constructing a suite.
"""

from typing import Dict, List

from applications.applications import AppConfig
from ecflow.ecflow_tasks_factory import ecflow_tasks_factory


__all__ = ['get_ecflow_tasks']


def get_ecflow_tasks(app_config: AppConfig) -> List[Dict]:
    """
    Return a list of ecFlow task dicts for every task in *app_config*.

    Iterates over all RUNs and their task lists, creating the
    appropriate ``EcFlowTasks`` subclass via ``ecflow_tasks_factory``
    and calling ``get_ecflow_task()`` for each task name.
    """
    tasks = []
    for run, run_tasks in app_config.task_names.items():
        task_obj = ecflow_tasks_factory.create(
            app_config.net, app_config, run)
        for task_name in run_tasks:
            tasks.append(task_obj.get_ecflow_task(task_name))
    return tasks
