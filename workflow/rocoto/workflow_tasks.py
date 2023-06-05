#!/usr/bin/env python3

from typing import List
from applications.applications import AppConfig
from rocoto.tasks_factory import tasks_factory


__all__ = ['get_wf_tasks']


def get_wf_tasks(app_config: AppConfig) -> List:
    """
    Take application configuration to return a list of all tasks for that application
    """

    tasks = []
    # Loop over all keys of cycles (CDUMP)
    for cdump, cdump_tasks in app_config.task_names.items():
        task_obj = tasks_factory.create(app_config.net, app_config, cdump)  # create Task object based on cdump
        for task_name in cdump_tasks:
            tasks.append(task_obj.get_task(task_name))

    return tasks
