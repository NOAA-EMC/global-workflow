
from pygfs.forecast import Forecast
from pygfs.exceptions import GFSError


class GFS(Forecast):
    """

    """

    def __init__(self: Forecast, config_obj: object):
        """
        Creates a new GFS object.

        """

        # Define the base-class attributes.
        super().__init__(config_obj=config_obj)

    def initialize(self: Forecast):
        """ """

        super().initialize()
