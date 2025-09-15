#!/usr/bin/env python3

import os
import stat
from datetime import datetime
from collections import OrderedDict
from typing import Dict
from applications.applications import AppConfig
from workflow_suite import WorkflowSuite
from wxflow import to_timedelta, which, mkdir
from abc import ABC, abstractmethod
from logging import getLogger

logger = getLogger(__name__.split('.')[-1])


class EcFlowSuite(WorkflowSuite, ABC):

    def __init__(self, app_config: AppConfig, ecflow_config: Dict) -> None:

        super().__init__(app_config, ecflow_config)
