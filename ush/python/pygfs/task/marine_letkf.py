#!/usr/bin/env python3

from datetime import datetime, timedelta
import f90nml
from logging import getLogger
import os
from soca import bkg_utils
from typing import Dict
import ufsda
from ufsda.stage import soca_fix
from wxflow import (AttrDict,
                    chdir,
                    Executable,
                    FileHandler,
                    logit,
                    parse_j2yaml,
                    Task,
                    Template,
                    TemplateConstants,
                    WorkflowException,
                    YAMLFile)

logger = getLogger(__name__.split('.')[-1])


class MarineLETKF(Task):
    """
    Class for global ocean analysis LETKF task
    """

    @logit(logger, name="MarineLETKF")
    def __init__(self, config: Dict) -> None:
        """Constructor for ocean LETKF task
        Parameters:
        ------------
        config: Dict
            configuration, namely evironment variables
        Returns:
        --------
        None
        """

        logger.info("init")
        super().__init__(config)

    @logit(logger)
    def initialize(self):
        """Method initialize for ocean LETKF task
        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        logger.info("initialize")
        RUN = self.runtime_config.RUN

    @logit(logger)
    def run(self):
        """Method run for ocean LETKF task
        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        logger.info("run")

        chdir(self.runtime_config.DATA)

    @logit(logger)
    def finalize(self):
        """Method finalize for ocean LETKF task
        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        logger.info("finalize")

        RUN = self.runtime_config.RUN
        cyc = self.runtime_config.cyc
