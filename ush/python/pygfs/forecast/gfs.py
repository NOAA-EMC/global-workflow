
# ----

from pygfs.forecast.forecast import Forecast
from pygfs.forecast.forecast import base_logger
from pygfs.exceptions import GFSForecastError

from pygw.configuration import cast_strdict_as_dtypedict
from pygw.logger import logit

# ----


# ----

class GFS(Forecast):
    """

    """

    def __init__(self: Forecast, config: object):
        """
        Description
        -----------

        Creates a new GFS object.

        """

        # Define the base-class attributes.
        super().__init__(config=config, model="GFSS")

    @logit(base_logger)
    def initialize(self: Forecast) -> None:
        """ """

        super().initialize()

        self.nems_configure()
