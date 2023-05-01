import logging
from pydoc import locate
from typing import Dict

import pygw.yaml_file
from pygw.attrdict import AttrDict
from pygw.logger import logit
from pygw.schema import Optional, Schema

# ----

logger = logging.getLogger(__name__.split(".")[-1])

# ----

__all__ = ["BuildSchema"]

# ----


class BuildSchema:
    """
    Description
    -----------

    This is the base-class object for setting up and defining the
    schema for a give application; it is based on
    https://tinyurl.com/pyutils-schema-build.

    Parameters
    ----------

    yaml_file: str

        A Python string containg the YAML-formatted file path
        containing the schema; the YAML-formatted file containing the
        schema attributes should be formatted similar to the example
        below.

        required:
            variable1: bool
            variable2: float
            variable3: int

        optional:
            variable4:
                type: bool
                default: False
            variable5:
                type: bool
                default: True
            variable6:
                type: float
                default: 1.0

    """

    @logit(logger, name="BuildSchema")
    def __init__(self: object, yaml_file: str):
        """
        Description
        -----------

        Creates a new BuildSchema object.

        """

        # Define the base-class attributes.
        self.schema = {}
        self.data = pygw.yaml_file.parse_yaml(path=yaml_file)

    @logit(logger)
    def reqschema(self: object) -> None:
        """
        Description
        -----------

        Collect any required schema attributes from the schema YAML
        file.

        """

        try:
            reqdict = self.data["required"]
            if len(reqdict) > 0:
                for (key, value) in reqdict.items():
                    self.schema[key] = locate(value)
        except KeyError:
            # TODO: A logger warning informing the user that no
            # required attributes were determined from the YAML
            # file; this would be useful for debugging.
            pass

    @logit(logger)
    def optschema(self: object) -> None:
        """
        Description
        -----------

        Collect any optional schema attributes and the default values
        from the schema YAML file.

        """

        try:
            optdict = self.data["optional"]
            if len(optdict) > 0:
                for (key, value) in optdict.items():
                    vardef = value["default"]
                    vartyp = value["type"]
                    self.schema[Optional(key, default=vardef)] = locate(vartyp)
        except KeyError:
            # TODO: A logger warning informing the user that no
            # optional attributes were determined from the YAML file;
            # this would be useful for debugging.
            pass

    @logit(logger)
    def build(self) -> None:
        """
        Description
        -----------

        This method performs the following tasks.

        - Builds the required (if any) schema attributes.

        - Build the optional (if any) schema attributes.

        Returns
        -------

        self.schema: Dict

            A Python dictionary containing the schema attributes.

        """

        self.reqschema()
        self.optschema()

        return self.schema
