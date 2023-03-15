
# ----

import pygfs.forecast as forecast
from pygfs.exceptions import GFSError

from pygw.configuration import cast_strdict_as_dtypedict
from pygw.logger import Logger

# ----


class GFS(forecast.Forecast):
    """

    """

    def __init__(self: Forecast, config: object):
        """
        Creates a new GFS object.

        """

        # Define the base-class attributes.
        super().__init__(config=config)

    def initialize(self: Forecast):
        """ """

        super().initialize()
