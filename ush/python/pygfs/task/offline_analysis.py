#!/usr/bin/env python3

from typing import Dict
from wxflow import (Task,
                    FileHandler)

logger = getLogger(__name__.split('.')[-1])


class OfflineAnalysis(Task):
    """
    Class for tasks to compute analysis increments from
    an offline analysis and previous forecast
    """
