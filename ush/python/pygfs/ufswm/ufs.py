import re
import os
import copy
import logging
from typing import Dict, Any, List

from dataclasses import dataclass

from pygw.attrdict import AttrDict

from pygw.template import Template, TemplateConstants
from pygw.logger import logit

logger = logging.getLogger(__name__.split('.')[-1])

UFS_VARIANTS = ['GFS']


@dataclass
class UFS:

    @logit(logger, name="UFS")
    def __init__(self: dataclass, model_name: str, config: Dict[str, Any]):
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
        self.model_name = model_name.lower()

        # Make a deep copy of incoming config for caching purposes. _config should not be updated
        self._config = copy.deepcopy(config)

        # Determine the model configuration.
        self.ufs_config = self.get_ufs_config()

    @logit(logger)
    def parse_ufs_templates(self: dataclass, input_template, output_file, ctx: Dict) -> None:
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
            logger.warn(f"The following variables were not substituted:")
            logger.warn(f"{', '.join(matches)}.")

        with open(output_file, 'w') as fho:
            fho.write(file_out)

    @staticmethod
    def __get_fixedfile_list(fixedfile_path) -> List:
        """
        Description
        -----------

        This method defines and returns a Python list of
        YAML-formatted files beneath the directory tree
        `fixedfile_path`.

        Parameters
        ----------

        fixedfile_path: str

            A Python string specifying the directory tree beneath
            which to collect/define a list of YAML-formatted files.

        Returns
        -------

        fixedfile_list: List

            A Python list of fixed files beneath the directory tree
            `fixedfile_path` attribute upon entry.

        """

        try:
            fixedfile_list = os.listdir(fixedfile_path)
        except FileNotFoundError:
            msg = f"The directory path {fixedfile_path} could not be found."
            logger.error(msg=msg)
            raise FileNotFoundError(msg)

        msg = f"Found the following fixed files: {(', '.join(fixedfile_list))}."
        logger.warn(msg=msg)

        return fixedfile_list

    @logit(logger)
    def get_ufs_config(self: dataclass) -> AttrDict:
        """
        Description
        -----------

        This method builds a Python dictionary containing the
        attributes for the respective UFS forecast model.

        Returns
        -------

        ufs_config: AttrDict

            A Python object containing the configuration attributes
            for the respective UFS forecast model.

        """

        ufs_config = AttrDict()
        ufs_config.coupled = self._config.DO_COUPLED

        # Collect the list of fixed-file YAMLs.
        fixedfile_path = os.path.join(
            self._config.PARMgfs, "ufs", "fix", self.model_name)
        fixedfile_list = self.__get_fixedfile_list(
            fixedfile_path=fixedfile_path)
        ufs_config.fixedfile_list = \
            [os.path.join(fixedfile_path, fixedfile) for
             fixedfile in fixedfile_list]

        # Define the directories containing the forecast model static
        # inputs.

        return ufs_config
