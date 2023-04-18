import os
import logging
from typing import Dict, Any

from pygw.attrdict import AttrDict
from pygw.logger import logit
from pygw.task import Task
from pygfs.ufswm.gfs import GFS

logger = logging.getLogger(__name__.split('.')[-1])


class GFSForecast(Task):
    """
    UFS-weather-model forecast task for the GFS
    """

    @logit(logger, name="GFSForecast")
    def __init__(self, config: Dict[str, Any], *args, **kwargs):
        """
        Parameters
        ----------
        config : Dict
                 dictionary object containing configuration from environment

        *args : tuple
                Additional arguments to `Task`

        **kwargs : dict, optional
                   Extra keyword arguments to `Task`
        """

        super().__init__(config, *args, **kwargs)

        self.task_config = AttrDict(**self.config, **self.runtime_config)

        # Create and initialize the GFS variant of the UFS
        self.gfs = GFS(self.task_config)

    @logit(logger)
    def initialize(self) -> None:
        """
        Initialize the GFS forecast task
        """

        # Create the necessary directories
        self.gfs.prepare_DATA()

        # Stage the fix files
        self.gfs.stage_fix()
