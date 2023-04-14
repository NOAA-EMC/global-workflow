import logging
from typing import Dict

from pygw.template import Template, TemplateConstants
from pygw.logger import logit

logger = logging.getLogger(__name__.split('.')[-1])


class UFS:

    @classmethod
    @logit(logger)
    def create(cls, model_name: str, config: Dict, *args, **kwargs):
        """
        Call the constructor of the appropriate variant of the UFS.
        The variant is defined via 'model_name'.
        E.g.
        GFS for all global variants
        """

        return next(cc for cc in cls.__subclasses__() if cc.__name__ == model_name)(config, *args, **kwargs)


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


