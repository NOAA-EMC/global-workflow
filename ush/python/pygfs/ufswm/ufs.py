import re
import copy
import logging
from typing import Dict, Any

from wxflow.template import Template, TemplateConstants
from wxflow.logger import logit

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
