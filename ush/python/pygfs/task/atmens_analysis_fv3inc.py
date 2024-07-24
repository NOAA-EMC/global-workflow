#!/usr/bin/env python3

import os
from logging import getLogger
from wxflow import logit

from pygfs.task.jedi import JEDI
from pygfs.task.atmens_analysis import AtmEnsAnalysis

logger = getLogger(__name__.split('.')[-1])

class AtmEnsAnalysisFV3Inc(AtmEnsAnalysis):
    """
    Class for FV3 increment conversion task in global atm analysis

    This class inherits the task_config of AtmEnsAnalysis, its parent
    class, but it inherits its methods from the JEDI class, the parent
    of AtmEnsAnalysis.
    """
    @logit(logger, name="AtmEnsAnalysisFV3Inc")
    def __init__(self, config):
        # Inherit task_config from AtmEnsAnalysis class
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
