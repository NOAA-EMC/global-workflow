

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

    def __init__(self: Task, config: object, model: str):
        """
        Description
        -----------

        Creates a new Forecast object.

        """

        # Define the base-class attributes.
        super().__init__(config=config)

        # Define the supported forecast models and the respective
        # attributes; build the base-class dictionary; model keys
        # (e.g., gfs) should be lowercase for generalization purposes.
        self.fcst_model_dict = {'gfs': {"ntiles": 6,
                                        "configs": ["ATM"]
                                        }
                                }

        if model.lower() not in self.fcst_model_dict:
            msg = f"Forecast model {model} is not supported. Aborting!!!"
            raise ForecastError(msg=msg)

        self.fcst_model_config = AttrDict()

    @logit(base_logger)
    def build_fcst_model_config(self: Task) -> None:
        """ """

    @logit(base_logger)
    def model_configure(self: Task) -> None:
        """
        Description
        -----------

        Builds the model_configure file appropriate for the forecast
        application.

        """

    @logit(base_logger)
    def nems_configure(self: Task) -> None:
        """ """

        nems_configure = self.config.get('')
