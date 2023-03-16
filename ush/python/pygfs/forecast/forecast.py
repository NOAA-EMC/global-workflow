

# ----

from typing import Dict

from pygfs.exceptions import ForecastError
from pygw.attrdict import AttrDict
from pygw.logger import Logger, logit
from pygw.task import Task

# ----

base_logger = Logger(level="error", colored_log=True)

# ----


class Forecast(Task):
    """ """

    def __init__(self: Task, config: object, model: str, app: str = None):
        """
        Description
        -----------

        Creates a new Forecast object.

        """

        # Define the base-class attributes.
        super().__init__(config=config)
        self.app = app
        self.model = model.lower()

        # Define the supported forecast models and the respective
        # attributes; build the base-class dictionary; model keys
        # (e.g., gfs) should be lowercase for generalization purposes.
        self.fcst_model_dict = {'gfs': {"ntiles": 6,
                                        "configs": ["ATM"],
                                        # THIS CAN BE EXPANDED LATER
                                        "res": ["C48", "C96"],
                                        }
                                }

        if self.model not in self.fcst_model_dict:
            msg = f"Forecast model {self.model} is not supported. Aborting!!!"
            raise ForecastError(msg=msg)

        self.fcst_model_config = self.build_fcst_model_config()

        # Check that the forecast model resolution is supported.
        self.check_resolution()

    @logit(base_logger)
    def build_fcst_model_config(self: Task) -> Dict:
        """ """

        # Define the configuration attributes for the respective
        # (supported) forecast model; proceed accordingly.
        fcst_model_dict = self.fcst_model_dict[self.model]

        if (self.app is not None) and (self.app not in fcst_model_dict["configs"]):
            msg = f"Forecast model {self.model} application {self.app} is not supported. Aborting!!!"
            raise ForecastError(msg=msg)

        fcst_model_config = self.fcst_model_dict[self.model]

        return fcst_model_config

    @logit(base_logger)
    def check_resolution(self: Task) -> None:
        """ """

        print(self.config.CASE)

        # if self.fcst_model_config["res"]:

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
