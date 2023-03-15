

# ----

from pygfs.exceptions import ForecastError
from pygw.task import Task

# ----


class Forecast(Task):
    """ """

    def __init__(self: Task, config: object):
        """
        Creates a new Forecast object.

        """

        # Define the base-class attributes.
        super().__init__(config=config)

    def model_configure(self: Task) -> None:
        """Builds the model_configure file appropriate for the forecast
        application.

        """

    def nems_configure(self: Task) -> None:
        """ """

        nems_configure = self.config.get('')
