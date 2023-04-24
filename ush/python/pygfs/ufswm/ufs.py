import os
import re
import copy
import logging
from typing import Dict, Any, Union, List

from pygw.attrdict import AttrDict
from pygw.template import Template, TemplateConstants
from pygw.file_utils import FileHandler
from pygw.logger import logit
from pygw.timetools import datetime_to_YMDH

logger = logging.getLogger(__name__.split('.')[-1])

UFS_VARIANTS = ['GFS']


class UFS:

    @logit(logger, name="UFS")
    def __init__(self, model_name: str, config: Dict[str, Any]):
        """Initialize the UFS-weather-model generic class and check if the model_name is a valid variant

        Parameters
        ----------
        model_name: str
            UFS variant
        config : Dict
            Incoming configuration dictionary
        """

        # First check if this is a valid variant
        if model_name not in UFS_VARIANTS:
            logger.warn(f"{model_name} is not a valid UFS variant")
            raise NotImplementedError(f"{model_name} is not yet implemented")

        # Make a deep copy of incoming config for caching purposes. _config should not be updated
        self._config = copy.deepcopy(config)

    @staticmethod
    @logit(logger)
    def parse_ufs_templates(input_template, output_file, ctx: Dict) -> None:
        """
        This method parses UFS-weather-model templates of the pattern @[VARIABLE]
        drawing the value from ctx['VARIABLE']
        """

        with open(input_template, 'r') as fhi:
            file_in = fhi.read()
            file_out = Template.substitute_structure(
                file_in, TemplateConstants.AT_SQUARE_BRACES, ctx.get)

        # If there are unrendered bits, find out what they are
        pattern = r"@\[.*?\]+"
        matches = re.findall(pattern, file_out)
        if matches:
            logger.warn(f"{input_template} was rendered incompletely")
            logger.warn(f"The following variables were not substituted")
            print(matches)  # TODO: improve the formatting of this message
        # TODO: Should we abort here? or continue to write output_file?

        with open(output_file, 'w') as fho:
            fho.write(file_out)

    @staticmethod
    @logit(logger)
    def set_ufs_fix(FIX_dir: str) -> Dict[str, str]:
        """
        This method sets the paths to the UFS-weather-model fixed files based on the FIX_dir
        TODO:  extract this out to a YAML when we have a better idea of what the structure will be
        """

        fix = AttrDict()

        fix.FIX_aer = os.path.join(FIX_dir, 'aer')
        fix.FIX_am = os.path.join(FIX_dir, 'am')
        fix.FIX_lut = os.path.join(FIX_dir, 'lut')
        fix.FIX_orog = os.path.join(FIX_dir, 'orog')
        fix.FIX_ugwd = os.path.join(FIX_dir, 'ugwd')

        return fix

    @logit(logger)
    def set_ufs_config(self) -> Dict[str, Any]:
        """
        This method sets the UFS-weather-model configuration based on the big experiment config


        Returns
        -------
        cfg : Dict
            UFS-weather-model resolution and other configuration as necessary

        TODO: This method could be broken up into smaller methods for each component, but maintain this as the entry point
        """

        cfg = AttrDict()

        # TODO: break this into smaller methods for atmos, ocean, etc.
        cfg.atm_res = self._config.get('CASE', 'C96')
        cfg.atm_levs = self._config.get('LEVS', 127)

        cfg.ocn_res = self._config.get('OCNRES', '100')

        return cfg

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
        cfg.OUTPUT_FILE = f"{self._config.OUTPUT_FILETYPE_ATM}", f"{self._config.OUTPUT_FILETYPE_SFC}"
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

    @staticmethod
    @logit(logger)
    def stage(data: Union[List[Dict[str, List]], Dict[str, List]]) -> None:
        """
        This method stages the UFS-weather-model fixed files
        """

        if isinstance(data, list):
            for item in data:
                FileHandler(item).sync()
        else:
            FileHandler(data).sync()
