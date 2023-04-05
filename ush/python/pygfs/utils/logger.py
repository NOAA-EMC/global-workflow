from pygw.logger import Logger as pygw_Logger
from pygw.logger import logit as pygw_logit

from dataclasses import dataclass

from typing import Dict

# ----


@dataclass
class Logger:
    """

    """

    def __init__(self: dataclass, config: Dict, level: str = None):
        """
        Description
        -----------

        Creates a new Logger object.

        """

        # Define the base-class attributes.
        if level is None:
            if getattr(config, "loglev") is None:
                level = "info"
            else:
                level = config.loglev

        self.logger = pygw_Logger(level=level, colored_log=True)
        self.logit = pygw_logit(self.logger)
