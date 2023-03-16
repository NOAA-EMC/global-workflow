
# ----

from pygfs.forecast.forecast import Forecast
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
        super().__init__(config=config, model="GFS")

    @logit(self.logger)
    def initialize(self: Forecast) -> None:
        """ """

        super().initialize()

        for (item, value) in self.config.items():
            print(item, value)

        self.nems_configure()
