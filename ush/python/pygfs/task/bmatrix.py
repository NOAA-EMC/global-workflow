#!/usr/bin/env python3

import os
from logging import getLogger
from typing import List, Dict, Any, Union

from wxflow import (parse_j2yaml, FileHandler, logit,
                    Task, Executable, WorkflowException)

logger = getLogger(__name__.split('.')[-1])


class BMatrix(Task):
    """Parent class for GDAS BMatrix tasks

    The BMatrix class is the parent class for all
    Global Data Assimilation System (GDAS) BMatrix tasks
    """
    def __init__(self, config: Dict[str, Any]) -> None:
        super().__init__(config)
        # Store location of GDASApp jinja2 templates
        self.gdasapp_j2tmpl_dir = os.path.join(self.task_config.PARMgfs, 'gdas')

    def initialize(self) -> None:
        super().initialize()

    def finalize(self) -> None:
        super().finalize()
