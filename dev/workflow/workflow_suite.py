#!/usr/bin/env python3

from typing import Dict
from applications.applications import AppConfig
from wxflow import to_timedelta
from abc import ABC, abstractmethod
from hosts import Host
from logging import getLogger

logger = getLogger(__name__.split('.')[-1])


class WorkflowSuite(ABC):

    def __init__(self, app_config: AppConfig, workflow_config: Dict) -> None:
        """
        Initializes the workflow suite with the provided application and workflow configurations.

        Parameters
        ----------
        app_config : AppConfig
            The application configuration object containing various settings and configurations.
        workflow_config : dict
            A dictionary containing workflow-specific configuration, including the workflow engine type.

        Raises
        ------
        ValueError
            If the specified workflow engine in `workflow_config` is not supported.

        Attributes
        ----------
        _app_config : AppConfig
            Stores the application configuration.
        workflow_config : Dict
            The workflow configuration for a valid workflow engine
        """

        self._app_config = app_config
        self.workflow_engine = workflow_config.get("workflow_engine", None)
        if self.workflow_engine in ["rocoto"]:
            self.rocoto_config = workflow_config
        elif self.workflow_engine in ["ecflow"]:
            self.ecflow_config = workflow_config
        else:
            raise ValueError(f"Unsupported workflow_engine: {self.workflow_engine}")

        # Use the first config.base (sourced with an arbitrary RUN)
        self._base = self._app_config.configs[next(iter(self._app_config.configs))]['base']
        self._base['interval_gdas'] = to_timedelta(f'{self._base["assim_freq"]}H')
        self._base['interval_gfs'] = to_timedelta(f'{self._base["INTERVAL_GFS"]}H')

        # Collect info needed to write an scrontab file
        self.host_info = Host().info
        self.use_scrontab = self.host_info.get("USE_SCRONTAB", False)
        # Add ACCOUNT to host_info, with that from config.base
        self.host_info.ACCOUNT = self._base['ACCOUNT']
        self.HOMEgfs = self._base['HOMEgfs']
        self.expdir = self._base['EXPDIR']
        self.pslot = self._base['PSLOT']

    @abstractmethod
    def get_cycledefs(self):
        pass

    @abstractmethod
    def write(self):
        pass
