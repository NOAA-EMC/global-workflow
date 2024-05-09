#!/usr/bin/env python3

from logging import getLogger
from typing import Dict
from wxflow import (chdir,
                    logit,
                    Task)

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

