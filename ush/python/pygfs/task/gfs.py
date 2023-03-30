
from pygfs.task.forecast import Forecast
from pygfs.exceptions import GFSError
from typing import Dict

# ----


class GFS(Forecast):
    """

    """

    def __init__(self: Forecast, config: Dict):
        """
        Description
        -----------

        Creates a new GFS object.

        """

        # Define the base-class attributes.
        super().__init__(config=config, model="GFS")

    def initialize(self: Forecast):
        """

        """

        super().initialize()

        # NEED TO READ YAMLS SOMEHOW.
