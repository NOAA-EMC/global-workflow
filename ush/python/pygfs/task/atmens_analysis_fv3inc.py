#!/usr/bin/env python3

import os
from logging import getLogger
from wxflow import FileHandler, to_fv3time, Template, TemplateConstants

from pygfs.task.jedi import JEDI

logger = getLogger(__name__.split('.')[-1])

class AtmEnsAnalysisFV3Inc(JEDI):
    """
    Class for FV3 increment conversion task in global atm analysis
    """
    @logit(logger, name="AtmEnsAnalysisFV3Inc")
    def __init__(self, config):
        super().__init__(config)

    @logit(logger)
    def initialize(self: AtmEnsAnalysisFV3Inc) -> None:
        super().initialize()

    @logit(logger)
    def execute(self: AtmEnsAnalysisFV3Inc, aprun_cmd: str) -> None:
        super().execute(aprun_cmd)
        
    @logit(logger)
    def finalize(self: AtmEnsAnalysisFV3Inc) -> None:
        """Finalize FV3 increment conversion

        This method write the UFS model readable atm increment file

        """
        super().finalize()

        # create template dictionaries
        template_inc = self.task_config.COM_ATMOS_ANALYSIS_TMPL
        tmpl_inc_dict = {
            'ROTDIR': self.task_config.ROTDIR,
            'RUN': self.task_config.RUN,
            'YMD': to_YMD(self.task_config.current_cycle),
            'HH': self.task_config.current_cycle.strftime('%H')
        }
        
        # copy FV3 atm increment to comrot directory
        logger.info("Copy UFS model readable atm increment file")
        cdate = to_fv3time(self.task_config.current_cycle)
        cdate_inc = cdate.replace('.', '_')
        
        # loop over ensemble members
        for imem in range(1, self.task_config.NMEM_ENS + 1):
            memchar = f"mem{imem:03d}"

            # create output path for member analysis increment
            tmpl_inc_dict['MEMDIR'] = memchar
            incdir = Template.substitute_structure(template_inc, TemplateConstants.DOLLAR_CURLY_BRACE, tmpl_inc_dict.get)
            src = os.path.join(self.task_config.DATA, 'anl', memchar, f"atminc.{cdate_inc}z.nc4")
            dest = os.path.join(incdir, f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.atminc.nc")

            # copy increment
            logger.debug(f"Copying {src} to {dest}")
            inc_copy = {
                'copy': [[src, dest]]
            }
            FileHandler(inc_copy).sync()

    def clean(self: AtmEnsAnalysisFV3Inc):
        super().clean()
