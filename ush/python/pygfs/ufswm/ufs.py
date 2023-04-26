import re
import copy
import logging
from typing import Dict, Any, Union, List

from pygw.attrdict import AttrDict
from pygw.template import Template, TemplateConstants
from pygw.file_utils import FileHandler
from pygw.logger import logit
from pygw.yaml_file import parse_yamltmpl

logger = logging.getLogger(__name__.split(".")[-1])

UFS_VARIANTS = ["GFS"]


class UFS:
    @logit(logger, name="UFS")
    def __init__(self, model_name: str, config: Dict[str, Any], HOMEufs: str = None):
        """Initialize the UFS-weather-model generic class and check if the model_name is a valid variant

        Parameters
        ----------
        model_name: str
            UFS variant
        config : Dict
            Incoming configuration dictionary
        HOMEufs : str, optional
            Path to the UFS-weather-model directory (as if it were in a vertical structure), by default None
        """

        # First check if this is a valid variant
        if model_name not in UFS_VARIANTS:
            logger.warn(f"{model_name} is not a valid UFS variant")
            raise NotImplementedError(f"{model_name} is not yet implemented")

        # Make a deep copy of incoming config for caching purposes. _config should not be updated
        self._config = copy.deepcopy(config)

        # Collect all model specific configuration attributes in the ufs_model dict
        # Some will be from _config, others will be determined internally e.g. warm_start etc.
        self.ufs_model = AttrDict()  # Initialize the ufs_model container

        self.ufs_model.HOMEufs = HOMEufs

        # Add the yaml_config used to control the configuration defined in the config
        # This file will contain the list of fix files, diag_tables, etc.
        # Over time, this can be contain more information that typically was provided via _config.
        try:
            self.ufs_model.yaml_config = config["UFS_CONFIG_FILE"]
        except KeyError:
            raise KeyError(f"FATAL_ERROR: 'UFS_CONFIG_FILE' is not defined in the configuration, ABORT!")

        # Add basic keys from `config` to self.ufs_model
        # TODO: we will need COM keys for current and previous cycle dirs
        # TODO: see if this map can be abstracted out to a yaml or included in yaml_config
        config_keys = ["current_cycle", "previous_cycle", "DATA", "RUN"]
        for key in config_keys:
            self.ufs_model[key] = config[key]

    @staticmethod
    @logit(logger)
    def parse_ufs_templates(input_template, output_file, ctx: Dict) -> None:
        """
        This method parses UFS-weather-model templates of the pattern @[VARIABLE]
        drawing the value from ctx['VARIABLE']
        """

        with open(input_template, "r") as fhi:
            file_in = fhi.read()
            file_out = Template.substitute_structure(file_in, TemplateConstants.AT_SQUARE_BRACES, ctx.get)

        # If there are unrendered bits, find out what they are
        pattern = r"@\[.*?\]+"
        matches = re.findall(pattern, file_out)
        if matches:
            logger.warn(f"{input_template} was rendered incompletely")
            logger.warn(f"The following variables were not substituted")
            print(matches)  # TODO: improve the formatting of this message
        # TODO: Should we abort here? or continue to write output_file?

        with open(output_file, "w") as fho:
            fho.write(file_out)

    @staticmethod
    @logit(logger)
    def stage(data: Union[List[Dict[str, List]], Dict[str, List]]) -> None:
        """
        Description
        -----------
        This method stages the UFS-weather-model files to the appropriate location

        Parameters
        ----------
        data : dict or list[dict]
            List of dictionaries containing the source and destination paths
            or a single dictionary containing the source and destination paths

        Returns
        -------
        None
        """

        if isinstance(data, list):
            for item in data:
                FileHandler(item).sync()
        else:
            FileHandler(data).sync()

    def get_ics(self):

        ics = AttrDict()
        ics.atm_ics = self._get_atm_ics()

        return ics

    def _get_atm_ics(self):
        pass

    def stage_tables(self, tables, target):
        """
        Description
        -----------
        Concatenate the tables into a single file and stage it to the target location

        Parameters
        ----------
        tables : list
            List of tables to concatenate
        target : str
            Target location

        Returns
        -------
        None
        """

        if not isinstance(tables, list):
            tables = list(tables)

        with open(target, "w") as fh:
            for tt in tables:
                with open(tt, "r") as fih:
                    fh.write(fih.read())

    def input_nml_build(self, tmpl: str, target: str) -> None:
        """ 
        Description
        -----------

        This method prepares and builds the `input.nml` file for
        the UFS forecast.


        """
        pass

    def mdl_config_defs(self) -> AttrDict:
        """
        Description
        -----------

        This method assigns the default (i.e., non-resolution)
        dependent attributes for the UFS `model_configure` file.

        Returns
        -------

        cfg: AttrDict

            A Python dictionary containing the default
            `model_configure` attributes values.

        """

        cfg = AttrDict()

        # TODO: This could be cleaned up via a dictionary.
        cfg.SYEAR = self._config.current_cycle.year
        cfg.SMONTH = self._config.current_cycle.month
        cfg.SDAY = self._config.current_cycle.day
        cfg.SHOUR = self._config.current_cycle.hour

        cfg.FHMAX = self._config.FHMAX
        cfg.RESTART_INTERVAL = self._config.restart_interval

        # TODO: HRW: This is NoneType for now and will be constructed in a subsquent PR.
        cfg.FHOUT = None
        cfg.FILENAME_BASE = "atm", "sfc"
        cfg.IDEFLATE = self._config.ideflate

        # HRW: Is this resolution dependent?
        cfg.NBITS = self._config.deflate

        # HRW: What is this? Setting to NoneType for now and will fix
        # in a subsequent PR.
        cfg.NSOUT = None
        cfg.OUTPUT_FILE = (
            f"{self._config.OUTPUT_FILETYPE_ATM}",
            f"{self._config.OUTPUT_FILETYPE_SFC}",
        )
        cfg.OUTPUT_GRID = self._config.OUTPUT_GRID
        cfg.QUILTING = self._config.QUILTING
        cfg.WRITE_DOPOST = self._config.WRITE_DOPOST

        return cfg

    @logit(logger)
    def mdl_config(self) -> None:
        """
        Description
        -----------

        This method sets the UFS-weather-model `model_configure` template
        attributes.

        """

        # Initialize the Python dictionary with the default
        # `model_configure` attribute values.
        cfg = self.mdl_config_defs()

    @logit(logger)
    def nems_build(self, tmpl: str, target: str) -> None:
        """
        Description
        -----------

        This method prepares and builds the `nems.configure` file for
        the UFS forecast.

        """

        pass
