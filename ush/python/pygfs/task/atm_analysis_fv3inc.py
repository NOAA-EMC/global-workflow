#!/usr/bin/env python3

import os
from logging import getLogger
from wxflow import logit

from pygfs.task.jedi import JEDI
from pygfs.task.atm_analysis import AtmAnalysis

logger = getLogger(__name__.split('.')[-1])

class AtmAnalysisFV3Inc(AtmAnalysis):
    """
    Class for FV3 increment conversion task in global atm analysis

    This class inherits the task_config of AtmAnalysis, its parent
    class, but it inherits its methods from the JEDI class, the parent
    of AtmAnalysis.
    """
    @logit(logger, name="AtmAnalysisFV3Inc")
    def __init__(self, config):
        super().__init__(config)

    @logit(logger)
    def initialize(self) -> None:
        JEDI.initialize(self)

    @logit(logger)
    def execute(self, aprun_cmd: str) -> None:
        JEDI.execute(self, aprun_cmd)
        
    @logit(logger)
    def finalize(self) -> None:
        JEDI.finalize(self)

    def clean(self):
        JEDI.clean(self)
