#!/usr/bin/env python3

import os
from logging import getLogger
from wxflow import FileHandler, to_fv3time

from pygfs.task.jedi import JEDI

logger = getLogger(__name__.split('.')[-1])

class AtmAnalysisFV3Inc(JEDI):
    """
    Class for FV3 increment conversion task in global atm analysis
    """
    @logit(logger, name="AtmAnalysisFV3Inc")
    def __init__(self, config):
        super().__init__(config)

    @logit(logger)
    def initialize(self: AtmAnalysisFV3Inc) -> None:
        super().initialize()

    @logit(logger)
    def execute(self: AtmAnalysisFV3Inc, aprun_cmd: str) -> None:
        super().execute(aprun_cmd)
        
    @logit(logger)
    def finalize(self: AtmAnalysisFV3Inc) -> None:
        """Finalize FV3 increment conversion

        This method write the UFS model readable atm increment file

        """
        super().finalize()
        
        # Copy FV3 atm increment to comrot directory
        logger.info("Copy UFS model readable atm increment file")
        cdate = to_fv3time(self.task_config.current_cycle)
        cdate_inc = cdate.replace('.', '_')
        src = os.path.join(self.task_config.DATA, 'anl', f"atminc.{cdate_inc}z.nc4")
        dest = os.path.join(self.task_config.COM_ATMOS_ANALYSIS, f'{self.task_config.RUN}.t{self.task_config.cyc:02d}z.atminc.nc')
        logger.debug(f"Copying {src} to {dest}")
        inc_copy = {
            'copy': [[src, dest]]
        }
        FileHandler(inc_copy).sync()

    def clean(self: AtmAnalysisFV3Inc):
        super().clean()
