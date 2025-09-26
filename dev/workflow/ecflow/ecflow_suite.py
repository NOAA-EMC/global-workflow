#!/usr/bin/env python3

from typing import Dict
from applications.applications import AppConfig
from workflow_suite import WorkflowSuite
from abc import ABC
from logging import getLogger

logger = getLogger(__name__.split('.')[-1])


class EcFlowSuite(WorkflowSuite, ABC):

    def __init__(self, app_config: AppConfig, ecflow_config: Dict) -> None:

        super().__init__(app_config, ecflow_config)
