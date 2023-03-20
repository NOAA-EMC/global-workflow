

# ----

import os

from typing import Dict, Tuple

from pygw.task import Task

from pygfs.exceptions import ForecastError
from pygfs.config.gfs import GFS
from pygw.attrdict import AttrDict
from pygw.logger import Logger, logit

# ----

base_logger = Logger(level="info", colored_log=True)

# ----


class UFSWM(Task):
    """ """

    @logit(base_logger, name="UFSWM")
    def __init__(self: Task, config: object, model: str, app: str = None,
                 *args, **kwargs):
        """
        Description
        -----------

        Creates a new UFSWM object.

        """

        # Define the base-class attributes.
        super().__init__(config, *args, **kwargs)
        self.app = app
        self.config = config
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

        if self.model is "gfs":
            self.grid_config = GFS(case=self.config.CASE)

        self.fcst_model_config = self.build_fcst_model_config()

        # Check that the forecast model resolution is supported.
        self.build_grid_attrs()

    @logit(base_logger)
    def build_fcst_model_config(self: Task) -> Dict:
        """
        Description
        -----------

        Returns
        -------


        Raises
        ------



        """

        # Define the configuration attributes for the respective
        # (supported) forecast model; proceed accordingly.
        fcst_model_dict = self.fcst_model_dict[self.model]

        if (self.app is not None) and (self.app not in fcst_model_dict["configs"]):
            msg = f"Forecast model {self.model} application {self.app} is not supported. Aborting!!!"
            raise ForecastError(msg=msg)

        fcst_model_config = self.fcst_model_dict[self.model]

        return fcst_model_config

    @logit(base_logger)
    def build_grid_attrs(self: Task) -> Tuple[Dict]:
        """
        Description
        -----------

        Returns
        -------

        Raises
        ------


        """

        if self.config.CASE not in self.fcst_model_config["res"]:
            msg = f"The forecast model resolution {self.config.CASE} is not supported. Aborting!!!"
            raise ForecastError(msg=msg)

        # self.grid_config = self.grid_config

        # print(self.grid_config)

        # return (input_nml)

    @logit(base_logger)
    def configure_inputs(self: Task, template_path: str, output_path: str, value_dict: Dict,
                         default_value_dict: Dict = None) -> None:
        """
        Description
        -----------

        Parameters
        ----------

        """

    @logit(base_logger)
    def get_fixed_files(self: Task):
        """
        Description
        -----------

        Parameters
        ----------

        """

        # Define the fixed-file attributes.
        ufswm_config = AttrDict()

        FIX_dir = os.path.join(self.config.HOMEgfs, "fix")
        ufswm_config.FIX_am = os.path.join(FIX_dir, 'am')
        ufswm_config.FIX_aer = os.path.join(FIX_dir, 'aer')
        ufswm_config.FIX_orog = os.path.join(FIX_dir, 'orog')
        ufswm_config.FIX_ugwd = os.path.join(FIX_dir, 'ugwd')
        ufswm_config.FIX_lut = os.path.join(FIX_dir, 'lut')

        print(ufswm_config)

        return ufswm_config
