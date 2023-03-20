#!/usr/bin/env python

# ----

"""
Module
------

    schema.py

Description
-----------

    This module provides various schema evaluation interfaces and/or
    check applications.

Functions
---------

    validate(cls_schema, cls_opts)

        This function validates the calling class schema; if the
        respective schema is not validated an exception will be
        raised; otherwise this function is passive.

Requirements
------------

schema; https://github.com/keleshev/schema

Author(s)
---------

    Henry R. Winterbottom; 20 March 2023

History
-------

    2023-03-20: Henry Winterbottom -- Initial implementation.

"""


from typing import Dict

from pygw.exceptions import SchemaException
from pygw.logger import Logger, logit
from schema import Schema

# ----

logger = Logger(level="info", colored_log=True)

__all__ = ["validate_opts"]

# ----


@logit(logger)
def validate_opts(cls_schema: Dict, cls_opts: Dict) -> None:
    """
    Description
    -----------

    This function validates the calling class schema; if the
    respective schema is not validated an exception will be raised;
    otherwise this function is passive.

    Parameters
    ----------

    cls_schema: dict

        A Python dictionary containing the calling class schema.

    cls_opts: dict

        A Python dictionary containing the options (i.e., parameter
        arguments, keyword arguments, etc.,) passed to the respective
        calling class.

    Raises
    ------

    SchemaException:

        * raised if an exception is encountered while validating the
          schema.

    """

    # Define the schema object.
    schema = Schema([cls_schema])

    # Check that the specified options (`cls_opts`) are valid; proceed
    # accordingly.
    try:

        # Validate the schema.
        schema.validate([cls_opts])

    except Exception as errmsg:

        msg = f"Schema validation failed with error {errmsg}. Aborting!!!"
        raise SchemaException(msg=msg) from errmsg
