

# ----

from pygfs.exceptions import ForecastError
from pygw.attrdict import AttrDict
from pygw.logger import Logger, logit
from pygw.task import Task

# ----

base_logger = Logger(level="error", colored_log=True)

# ----


class Forecast(Task):
    """ """

    @logit(base_logger)
    def __init__(self: Task, config: object, model: str):
        """
        Description
        -----------

        Creates a new Forecast object.

        """

        # Define the base-class attributes.
        super().__init__(config=config)

        # Define the supported forecast models and the respective
        # attributes; build the base-class dictionary.
        self.fcst_model_dict = {'GFS': {"ntiles": 6,
                                        "configs": ["ATM"]
                                        }
                                }

        self.fcst_model_config = AttrDict()

    @logit(base_logger)
    def model_configure(self: Task) -> None:
        """
        Description
        -----------

        Builds the model_configure file appropriate for the forecast
        application.

        """

    def nems_configure(self: Task) -> None:
        """ """

        nems_configure = self.config.get('')
