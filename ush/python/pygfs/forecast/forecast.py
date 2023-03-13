

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
