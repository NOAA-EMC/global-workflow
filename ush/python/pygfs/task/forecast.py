from typing import Dict

from pygw.task import Task
from pygw.logger import Logger

from pygfs.ufswm import UFSWM
from pygfs.exceptions import ForecastError

# ----


class Forecast(Task):
    """
    Description
    -----------

    This is the base-class object for the respective Unified Forecast
    System (UFS) forecast task; it is a sub-class of Task.

    """

    def __init__(self: Task, config: Dict, model: str, *args, **kwargs):
        """
        Description
        -----------

        Creates a new Forecast object.

        """

        # Define the base-class attributes.
        super().__init__(config, *args, *kwargs)
        self.logger = Logger(level=self.config.loglev, colored_log=True)
        self.config.model = model

        # Update the configuration accordingly.
        if self.config.model.lower() == "gfs":
            self.config.ntiles = 6
        if self.config.model.lower() != "gfs":
            raise ForecastError(msg=msg)

        self.ufswm = UFSWM(config=self.config)
