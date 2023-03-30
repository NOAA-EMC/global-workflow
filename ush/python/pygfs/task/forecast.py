from typing import Dict, List

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
        self.config.model = model

        # Define the appplication logger object.
        if getattr(self.config, "loglev") is None:
            self.config.loglev = "info"
        self.logger = Logger(level=self.config.loglev, colored_log=True)

        # Update the configuration accordingly.
        if self.config.model.lower() == "gfs":
            self.config.ntiles = 6

        if self.config.model.lower() != "gfs":
            raise ForecastError(msg=msg)

        self.ufswm = UFSWM(config=self.config)

    def get_fixedfiles_info(self: Task, app: str) -> List:
        """ """

        fixed_files_list = [
            item for item in self.config.keys() if f"FIX{app}" in item]

        if len(fixed_files_list) == 0:
            fixed_files_info = None

        return fixed_files_list

    def sync_fixedfiles(self: Task, fixedfiles_dict: Dict) -> None:
        """ 


        """
