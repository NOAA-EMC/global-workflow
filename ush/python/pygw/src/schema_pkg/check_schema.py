import logging
from typing import Dict

from pygw.logger import logit
import pygw.schema
from pygw.schema import Schema

# ----

logger = logging.getLogger(__name__.split(".")[-1])

# ----

__all__ = ["CheckSchema"]

# ----


class CheckSchema:
    """
    Description
    -----------

    This is the base-class object for schema validation.

    Parameters
    ----------

    cls_schema: Dict

        A Python dictionary containing the schema; these are the
        attributes against which a given set of options (see
        `cls_opts`) would be compared.

    cls_opts: Dict

        A Python dictionary containing the provide application
        attributes; if the respective attribute datatypes in this
        dictionary do not match that of the schema (see `cls_schema`)
        an exception will be raised.

    """

    @logit(logger, name="CheckSchema")
    def __init__(self: object, cls_schema: Dict, cls_opts: Dict):
        """
        Description
        -----------

        Creates a new CheckSchema object.

        """

        # Define the base-class attributes.
        self.cls_schema = cls_schema
        self.cls_opts = cls_opts

    @logit(logger)
    def defaults(self: object) -> None:
        """
        Description
        -----------

        This method parses the calling class options (`cls_opts`),
        determines the optional values from the call class schema
        (`cls_schema`) and update `cls_opts` accordingly.

        """

        for (key, value) in self.cls_schema.items():

            # TODO: This may need to be updated.
            if isinstance(key, pygw.schema.Optional):

                # TODO: This will need to be cleanup up; it will be
                # confusing otherwise.
                if key not in self.cls_opts:
                    self.cls_opts[key.key] = key.default

    @logit(logger)
    def validate(self: object) -> None:
        """
        Description
        -----------

        Validates the schema.

        """

        # Update any not-specified optional schema values with the
        # default values if not specified explicitly.
        self.defaults()
        schema = Schema([self.cls_schema], ignore_extra_keys=True)
        schema.validate([self.cls_opts])

        # TODO: A logger message and exception if the validation fails
        # would be useful here.
        return self.cls_opts
