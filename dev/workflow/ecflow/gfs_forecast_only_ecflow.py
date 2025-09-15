#!/usr/bin/env python3

"""
GFS forecast-only ecflow generator module.

This module provides functionality to generate Rocoto XML workflow configurations
for GFS forecast-only runs. It handles cycle definitions and specific task configurations
needed for the GFS workflow.
"""

from ecflow.ecflow_suite import EcFlowSuite
from applications.applications import AppConfig
from wxflow import to_timedelta, timedelta_to_HMS
from typing import Dict


class GFSForecastOnlyEcFlowSuite(EcFlowSuite):
    """
    Rocoto XML generator for GFS forecast-only workflows.

    This class handles the generation of Rocoto XML configuration for GFS
    forecast-only mode, including cycle definitions and workflow scheduling.

    Parameters
    ----------
    app_config : AppConfig
        Application configuration object containing GFS settings
    workflow_config : Dict
        Dictionary containing ecFlow-specific configuration
    """

    def __init__(self, app_config: AppConfig, workflow_config: Dict) -> None:
        """
        Initialize GFS forecast-only ecFlow generator.

        Parameters
        ----------
        app_config : AppConfig
            Application configuration object containing GFS settings
        workflow_config : Dict
            Dictionary containing ecFlow-specific configuration
        """
        super().__init__(app_config, workflow_config)

