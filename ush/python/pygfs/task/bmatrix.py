#!/usr/bin/env python3

import os
from logging import getLogger
from typing import List, Dict, Any, Union

logger = getLogger(__name__.split('.')[-1])


class BMatrix(Task):
    """Parent class for GDAS BMatrix tasks

    The BMatrix class is the parent class for all
    Global Data Assimilation System (GDAS) BMatrix tasks
    """
    def __init__(self, config: Dict[str, Any]) -> None:
        super().__init__(config)
        self.config.ntiles = 6
        # Store location of GDASApp jinja2 templates
        self.gdasapp_j2tmpl_dir = os.path.join(self.config.PARMgfs, 'gdas')

    def initialize(self) -> None:
        super().initialize()

