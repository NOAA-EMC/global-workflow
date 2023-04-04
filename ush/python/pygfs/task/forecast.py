
import os
from typing import Dict, List

from pygw.task import Task
from pygw.logger import Logger

from pygw.attrdict import AttrDict

from pygfs.ufswm import UFSWM
from pygfs.exceptions import ForecastError

from pygw.yaml_file import YAMLFile, parse_yamltmpl

from pygw.file_utils import FileHandler

# ----

# Define the valid forecast model list.
VALID_MODEL_LIST = ["gfs"]

# ----


class Forecast(Task):
    """
    Description
    -----------

    This is the base-class object for the respective Unified Forecast
    System (UFS) forecast task; it is a sub-class of Task.

    """

    def __init__(self: Task, config: Dict, model: str, *args, **kwargs):
        """
        Description
        -----------

        Creates a new Forecast object.

        """

        # Define the base-class attributes.
        super().__init__(config=config, *args, *kwargs)
        self.model = model
        self.fcst_config = AttrDict()

        # Define the appplication logger object.
        if getattr(self.config, "loglev") is None:
            self.config.loglev = "info"

        self.logger = Logger(
            level=self.config.loglev, colored_log=True)

        # Check that the specified forecast model is supported;
        # proceed accordingly.
        if model.lower() not in VALID_MODEL_LIST:
            msg = f"Forecast model {model} is not (yet) supported. Aborting!!!"
            raise ForecastError(msg=msg)

        try:
            self.fcst_config.config = YAMLFile(
                path=self.config.FCSTYAML).as_dict()["forecast"]
        except KeyError:
            msg = ("The attribute (e.g., YAML-key) `forecast` could not be determined "
                   f"from YAML-formatted file {self.config.FCSTYAML}. Aborting!!!"
                   )
            raise ForecastError(msg=msg)

    def build_dirtree(self: Task) -> None:
        """


        """

        fixed_yaml = self.fcst_config.config.fixed_file_yaml
        fixed_data = parse_yamltmpl(
            path=fixed_yaml, data=self.fcst_config)

        # Build the directory tree and link the fixed files to the
        # working directory.
        FileHandler(fixed_data.dirtree_atmos).sync()
        FileHandler(fixed_data.fix_atmos).sync()
        FileHandler(fixed_data.fix_land).sync()

        if self.fcst_config.coupled:
            FileHandler(fixed_data.dirtree_ocean).sync()
            FileHandler(fixed_data.fix_ocean).sync()
