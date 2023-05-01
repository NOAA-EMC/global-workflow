import logging

from pygw.attrdict import AttrDict
from pygw.logger import logit
from pygw.template import Template, TemplateConstants

# ----

logger = logging.getLogger(__name__.split(".")[-1])

# ----

__all__ = ["WriteFromTemplate"]

# ----


class WriteFromTemplate:
    """
    Description
    -----------

    This is the base-class for writing an output file provided
    configuration values and template.

    Parameters
    ----------

    cfg: AttrDict

        A Python dictionary containing configuration values.

    tmpl: str

        A Python string specifying the path to the template.

    output: str

        A Python string specifying the path to the output file
        generated from the template and configuration values.

    """

    @logit(logger, name="WriteFromTemplate")
    def __init__(self: object, cfg: AttrDict, tmpl: str, output: str):
        """
        Description
        -----------

        Creates a new WriteFromTemplate object.

        """

        # Define the base-class attributes.
        self.cfg = cfg
        self.output = output
        self.tmpl = tmpl

    @logit(logger)
    def write(self: object):
        """
        Description
        -----------

        This method writes an output file using the specified template
        and configuration values.

        """

        # TODO: Logger messages regarding the template and output file
        # paths would be useful for debugging.
        with open(self.tmpl, "r", encoding="utf-8") as file_in:
            tmpl = file_in.read()
            tmpl_out = Template.substitute_structure(
                tmpl, TemplateConstants.AT_SQUARE_BRACES, self.cfg.get)

        with open(self.output, "w", encoding="utf-8") as file_out:
            file_out.write(tmpl_out)
