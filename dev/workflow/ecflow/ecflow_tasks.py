#!/usr/bin/env python3

"""
Base class for ecFlow task definitions.

Extends ``rocoto.tasks.Tasks`` to reuse the resource extraction,
forecast hour grouping, and system configuration infrastructure.
Subclasses (e.g. ``GFSEcFlowTasks``) provide per-task methods that
return ecFlow task dictionaries instead of Rocoto XML strings.

Each task method returns a dict (or list of dicts for product tasks)
with keys consumed by the suite generator::

    {
        'task_name':    str,        # logical task name
        'jjob':         str,        # J-Job basename under dev/jobs/
        'resources':    dict,       # from get_resource()
        'step':         str,        # config.resources step name
        'trigger':      str | None, # ecFlow trigger expression
        'product_task': bool,       # True → emit as family with fhr children
        'service_task': bool,       # True → service partition
        'component':    str | None, # 'atmos', 'ocean', 'ice', 'wave'
        'config':       str | None, # config step name (for product tasks)
        'forecast_hours': list | None,  # forecast hours (for product tasks)
    }
"""

from logging import getLogger
from typing import Dict, List, Optional

from applications.applications import AppConfig
from rocoto.tasks import Tasks

logger = getLogger(__name__.split('.')[-1])


class EcFlowTasks(Tasks):
    """
    Base class for ecFlow task definitions.

    Inherits ``Tasks.__init__`` which sets up configs, options, base,
    system settings, and environment variables.  The Rocoto-specific
    envars (XML strings) are unused on the ecFlow side but harmless.

    Subclasses override individual task methods to return ecFlow task
    dicts.
    """

    def __init__(self, app_config: AppConfig, run: str) -> None:
        super().__init__(app_config, run)
        self._task_names = app_config.task_names[run]

    def get_ecflow_task(self, task_name: str) -> Dict:
        """
        Dispatch to the task method for *task_name* and return its
        ecFlow task dict.

        Mirrors ``Tasks.get_task()`` but returns a dict instead of
        Rocoto XML.
        """
        try:
            return getattr(self, task_name)()
        except AttributeError:
            raise AttributeError(
                f'"{task_name}" is not a valid ecFlow task.\n'
                f'Valid tasks are:\n'
                f'{", ".join(Tasks.VALID_TASKS)}')

    # ── Helpers for subclasses ────────────────────────────────────────

    def _has_task(self, name: str) -> bool:
        """Return True if *name* is in the active task list."""
        return name in self._task_names

    def _simple_task(self, task_name: str, *,
                     jjob: str,
                     trigger: Optional[str] = None,
                     resource_name: Optional[str] = None,
                     service: bool = False) -> Dict:
        """
        Build a standard (non-product) ecFlow task dict.

        Parameters
        ----------
        task_name : str
            Logical task name.
        jjob : str
            J-Job script basename under dev/jobs/.
        trigger : str, optional
            ecFlow trigger expression.
        resource_name : str, optional
            Config step name for ``get_resource()``.  Defaults to
            *task_name*.
        service : bool
            Whether this task runs on the service partition.
        """
        resources = self.get_resource(resource_name or task_name)
        step = resource_name or task_name
        return {
            'task_name': task_name,
            'jjob': jjob,
            'resources': resources,
            'step': step,
            'trigger': trigger,
            'product_task': False,
            'service_task': service,
            'component': None,
            'config': None,
            'forecast_hours': None,
        }

    def _product_task(self, task_name: str, *,
                      jjob: str,
                      config: str,
                      component: str,
                      trigger: Optional[str] = None,
                      resource_name: Optional[str] = None) -> Dict:
        """
        Build a product ecFlow task dict (emitted as a family of
        per-forecast-hour-group children).

        Parameters
        ----------
        task_name : str
            Logical task name.
        jjob : str
            J-Job script basename.
        config : str
            Config step name for resource + forecast hour lookup.
        component : str
            Component name ('atmos', 'ocean', 'ice', 'wave').
        trigger : str, optional
            ecFlow trigger expression.
        resource_name : str, optional
            Config step name for ``get_resource()``.  Defaults to
            *config*.
        """
        resources = self.get_resource(resource_name or config)
        step = resource_name or config
        fhrs = self._get_forecast_hours(self.run, self._configs[config], component)

        # Ocean/ice do not produce output at fhr 0
        if component in ('ocean', 'ice') and 0 in fhrs:
            fhrs.remove(0)

        return {
            'task_name': task_name,
            'jjob': jjob,
            'resources': resources,
            'step': step,
            'trigger': trigger,
            'product_task': True,
            'service_task': False,
            'component': component,
            'config': config,
            'forecast_hours': fhrs,
        }
