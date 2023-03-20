#!/usr/bin/env python

# ----

"""
Module
------

    test_schema.py

Description
-----------

    This module provides a series of unit-tests for the pygw.schema
    module.

Usage
-----

    pytest tests/test_schema.py

Functions
---------

    test_schema_float()

        This function provides a unit test for the schema module; a
        float-value will be passed to the schema validator that
        accepts only string or integer instances; in order to pass,
        this unit test must raise a a SchemaException.

    test_schema_int()

        This function provides a unit test for the schema module; a
        integer-value is passed to the schema.

    test_schema_opt()

        This function provides a unit test for the schema module; a
        integer-value and an optional string value `opt_var` are
        passed to the schema.

    test_schema_str()

        This function provides a unit test for the schema module; a
        string is passed to the schema.

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

# ----

from pygw.exceptions import SchemaException
from pygw.logger import Logger, logit
from pygw.schema import validate_opts
from schema import Optional, Or

# ----

base_logger = Logger(level="info", colored_log=True)

# ----

# Define the schema.
CLS_SCHEMA = {
    "check_var": Or(str, int),
    Optional("opt_var"): str,
}

# ----


@logit(base_logger)
def test_schema_float() -> None:
    """
    Description
    -----------

    This function provides a unit test for the schema module; a
    float-value will be passed to the schema validator that accepts
    only string or integer instances; in order to pass, this unit test
    must raise a a SchemaException.

    """

    # Define the option values to be evaluated within the context of
    # the specified schema.
    cls_opts = {"check_var": 0.0000}

    # Evaluate the schema.
    try:
        validate_opts(cls_schema=CLS_SCHEMA, cls_opts=cls_opts)

    except SchemaException:
        pass


# ----


@logit(base_logger)
def test_schema_int() -> None:
    """
    Description
    -----------

    This function provides a unit test for the schema module; a
    integer-value is passed to the schema.

    """

    # Define the option values to be evaluated within the context of
    # the specified schema.
    cls_opts = {"check_var": 1}

    # Evaluate the schema.
    validate_opts(cls_schema=CLS_SCHEMA, cls_opts=cls_opts)

    assert True


# ----


@logit(base_logger)
def test_schema_opt() -> None:
    """
    Description
    -----------

    This function provides a unit test for the schema module; a
    integer-value and an optional string value `opt_var` are passed to
    the schema.

    """

    # Define the option values to be evaluated within the context of
    # the specified schema.
    cls_opts = {"check_var": 1, "opt_var": "I am an optional string."}

    # Evaluate the schema.
    validate_opts(cls_schema=CLS_SCHEMA, cls_opts=cls_opts)

    assert True


# ----


@logit(base_logger)
def test_schema_string() -> None:
    """
    Description
    -----------

    This function provides a unit test for the schema module; a
    string is passed to the schema.

    """

    # Define the option values to be evaluated within the context of
    # the specified schema.
    cls_opts = {"check_var": "I am a string."}

    # Evaluate the schema.
    validate_opts(cls_schema=CLS_SCHEMA, cls_opts=cls_opts)

    assert True
