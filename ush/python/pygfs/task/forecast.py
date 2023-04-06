"""
Module
------

    pygfs.task.forecast (pygfs/task/forecast.py)

Description
-----------

    This module contains the base-class module for forecast model
    applications.

Classes
-------

    Forecast(config, model, *args, **kwargs)

        This is the base-class object for the respective Unified
        Forecast System (UFS) forecast task; it is a sub-class of
        Task.

"""

# ----

__author__ = "Henry R. Winterbottom"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__version__ = 0.0

# ----

import os
from typing import Dict

from pygfs.exceptions import ForecastError
from pygfs.ufswm import UFSWM
from pygfs.utils.logger import Logger
from pygw.file_utils import FileHandler
from pygw.jinja import Jinja
from pygw.task import Task
from pygw.yaml_file import YAMLFile, parse_yamltmpl

# ----


class Forecast(Task):
    """
    Description
    -----------

    This is the base-class object for the respective Unified Forecast
    System (UFS) forecast task; it is a sub-class of Task.

    Parameters
    ----------

    config: Dict

        A Python dictionary containing the application configuration
        attributes.

    model: str

        A Python string specifying the valid forecast model.

    Raises
    ------

    ForecastError:

        - raised if the YAML key `forecast` could not be determined
          from the YAML-formatted configuration file path; see
          configuration variable `FCSTYAML`.

    """

    def __init__(self: Task, config: Dict, model: str, *args, **kwargs):
        """
        Description
        -----------

        Creates a new Forecast object.

        """

        # Define the base-class attributes.
        super().__init__(config=config, *args, *kwargs)
        self.config = config
        self.model = model.lower()
        self.logger = Logger(config=self.config).logger
        UFSWM(config=self.config, model=self.model)

        # Collect the forecast configuration attributes from the
        # external YAML-formatted file path.
        try:
            self.config.forecast = YAMLFile(path=self.config.FCSTYAML).as_dict()[
                "forecast"
            ]

        except KeyError as exc:
            msg = (
                "The attribute (e.g., YAML-key) `forecast` could not be determined "
                f"from YAML-formatted file {self.config.FCSTYAML}. Aborting!!!"
            )
            raise ForecastError(msg=msg) from exc

    def build_model_configure(self: Task) -> None:
        """
        Description
        -----------

        This method parses a Jinja2-formatted template and builds the
        UFS weather model forecast application `model_configure` file
        within the forecast application working directory.

        """

        model_configure_tmpl = self.config.forecast.model_configure
        model_configure_path = os.path.join(self.runtime_config.DATA, "model_configure")

        Jinja(model_configure_tmpl, data=self.config, allow_missing=True).save(
            model_configure_path
        )

    def build_nems_configure(self: Task) -> None:
        """
        Description
        -----------

        This method parses a Jinja2-formatted template and builds the
        UFS weather model forecast application `nems.configure` file
        within the forecast application working directory.

        """

        # Define then NEMS configuration template and write the file
        # accordingly.
        nems_configure_tmpl = self.config.forecast.nems_configure
        nems_configure_path = os.path.join(self.runtime_config.DATA, "nems.configure")

        # HRW: FORCING ALL TEMPLATE VARIABLES TO BE RENDERED IF
        # THEY ARE WITHOUT DEFAULT VALUES (IN THE JINJA TEMPLATE);
        # IF A VARIABLE IS NOT RENDERED CORRECTLY THE FORECAST
        # MODEL WILL FAIL; THIS IS ALSO USEFUL IN THE CASES WHEN A
        # USER HAS DEFINE THE INCORRECT nems.configure TEMPLATE.
        Jinja(nems_configure_tmpl, data=self.config, allow_missing=False).save(
            nems_configure_path
        )

        with open(nems_configure_path, "a", encoding="utf-8") as fout:
            fout.write(f"\n\n# Template: {nems_configure_tmpl}.")

    def config_dirtree(self: Task) -> None:
        """
        Description
        -----------

        This method builds the directory tree and collects the
        fixed-files for the respective forecast application.

        """

        # Build the directory tree and link the fixed files to the
        # working directory; proceed accordingly.
        dirtree_config = parse_yamltmpl(
            path=self.config.FCSTYAML, data=self.runtime_config
        )["forecast"]
        FileHandler(dirtree_config.dirtree_atmos).sync()

        atmos_fcst_config = parse_yamltmpl(
            path=dirtree_config.fixed_files.atmos, data=self.config
        )
        FileHandler(atmos_fcst_config).sync()
        land_fcst_config = parse_yamltmpl(
            path=dirtree_config.fixed_files.land, data=self.config
        )
        FileHandler(land_fcst_config).sync()

        if self.config.coupled:  # HRW: THIS NEEDS TO BE UPDATED.
            FileHandler(dirtree_config.dirtree_ocean).sync()
            ocean_fcst_config = parse_yamltmpl(
                path=dirtree_config.fixed_files.ocean, data=self.config
            )
            FileHandler(ocean_fcst_config).sync()
