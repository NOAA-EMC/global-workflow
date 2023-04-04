
import os
from typing import Dict, List

from pygw.task import Task
from pygw.logger import Logger, logit

from pygw.attrdict import AttrDict

from pygfs.ufswm import UFSWM
from pygfs.exceptions import ForecastError

from pygw.jinja import Jinja
from pygw.yaml_file import YAMLFile, parse_yamltmpl

from pygw.file_utils import FileHandler

# ----

# Define the valid forecast model list.
VALID_MODEL_LIST = ["gfs"]

# The following are the supported GFS applications.
GFS_APP_LIST = ["atm", "atmw", "atma", "s2s", "s2sw", "s2swa"]

# ----


class Forecast(Task):
    """
    Description
    -----------

    This is the base-class object for the respective Unified Forecast
    System (UFS) forecast task; it is a sub-class of Task.

    Parameters
    ----------

    Raises
    ------

    """

    def __init__(self: Task, config: Dict, model: str, *args, **kwargs):
        """
        Description
        -----------

        Creates a new Forecast object.

        """

        # Define the base-class attributes.
        super().__init__(config=config, *args, *kwargs)
        self.config.model = model

        # HRW: THIS ALLOWS ME TO BUILD ON THE DICTIONARY FOR THE
        # RESPECTIVE APPLICATION (i.e., FORECAST).
        self.fcst_config = AttrDict(self.config).deepcopy()

        try:
            self.fcst_config.config = YAMLFile(
                path=self.fcst_config.FCSTYAML).as_dict()["forecast"]
        except KeyError:
            msg = ("The attribute (e.g., YAML-key) `forecast` could not be determined "
                   f"from YAML-formatted file {self.fcst_config.FCSTYAML}. Aborting!!!"
                   )
            raise ForecastError(msg=msg)

        if self.fcst_config.model.lower() not in VALID_MODEL_LIST:
            msg = f"Forecast model {self.fcst_config.model} is not (yet) supported. Aborting!!!"
            raise ForecastError(msg=msg)

        if getattr(self.fcst_config, "loglev") is None:
            self.fcst_config.loglev = "info"
        self.logger = Logger(
            level=self.config.loglev, colored_log=True)

    def build_model_configure(self: Task) -> None:
        """ """

        model_configure_tmpl = self.fcst_config.config.model_configure
        model_configure_path = os.path.join(
            self.runtime_config.DATA, "model_configure")

    def build_nems_configure(self: Task) -> None:
        """
        Description
        -----------

        This method parses a Jinja2-formatted template and builds the
        UFS forecast application nems.configure file within the
        forecast task application working directory.

        """

        # Define then NEMS configuration template and write the file
        # accordingly.
        nems_configure_tmpl = self.fcst_config.config.nems_configure
        nems_configure_path = os.path.join(
            self.runtime_config.DATA, "nems.configure")

        # HRW: FORCING ALL TEMPLATE VARIABLES TO BE RENDERED IF
        # THEY ARE WITHOUT DEFAULT VALUES (IN THE JINJA TEMPLATE);
        # IF A VARIABLE IS NOT RENDERED CORRECTLY THE FORECAST
        # MODEL WILL FAIL; THIS IS ALSO USEFUL IN THE CASES WHEN A
        # USER HAS DEFINE THE INCORRECT nems.configure TEMPLATE.
        Jinja(nems_configure_tmpl, data=self.fcst_config,
              allow_missing=False).save(nems_configure_path)

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
            path=self.fcst_config.FCSTYAML, data=self.runtime_config)["forecast"]

        FileHandler(dirtree_config.dirtree_atmos).sync()
        atmos_fcst_config = parse_yamltmpl(
            path=dirtree_config.fixed_files.atmos, data=self.fcst_config)
        FileHandler(atmos_fcst_config).sync()
        land_fcst_config = parse_yamltmpl(
            path=dirtree_config.fixed_files.land, data=self.fcst_config)
        FileHandler(land_fcst_config).sync()

        if self.fcst_config.coupled:
            FileHandler(dirtree_config.dirtree_ocean).sync()
            ocean_fcst_config = parse_yamltmpl(
                path=dirtree_config.fixed_files.ocean, data=self.fcst_config)
            FileHandler(ocean_fcst_config).sync()
