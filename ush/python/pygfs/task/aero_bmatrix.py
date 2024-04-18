#!/usr/bin/env python3

import os
from logging import getLogger
from typing import List, Dict, Any, Union

logger = getLogger(__name__.split('.')[-1])


class AerosolBMatrix(BMatrix):
    """
    Class for global aerosol BMatrix tasks
    """
    @logit(logger, name="AerosolBMatrix")
    def __init__(self, config: Dict[str, Any]) -> None:
        super().__init__(config)

    @logit(logger)
    def initialize(self: Analysis) -> None:
        super().initialize()

