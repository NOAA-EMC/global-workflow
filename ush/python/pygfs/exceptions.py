"""
Module
------

    exceptions.py

Description
-----------

    This module contains all pyufs package exceptions.

Classes
-------

    ForecastError(msg)

        This is the base-class for exceptions encountered within the
        ush/python/pygfs/task/forecast module; it is a sub-class of
        WorkflowException.

    GFSError(msg)

        This is the base-class for exceptions encountered within the
        ush/python/pygfs/task/gfs module; it is a sub-class of
        WorkflowException.

    UFSWMError(msg)

        This is the base-class for exceptions encountered within the
        ush/python/pygfs/ufswm module; it is a sub-class of
        WorkflowException.

"""

# ----

from pygw.exceptions import WorkflowException

# ----

# Define all available classes.
__all__ = ["ForecastError", "GFSError", "UFSWMError"]

# ----


class ForecastError(WorkflowException):
    """
    Description
    -----------

    This is the base-class for exceptions encountered within the
    ush/python/pygfs/task/forecast module; it is a sub-class of
    WorkflowException.

    """

# ----


class GFSError(WorkflowException):
    """
    Description
    -----------

    This is the base-class for exceptions encountered within the
    ush/python/pygfs/task/gfs module; it is a sub-class of
    WorkflowException.

    """

# ----


class UFSWMError(WorkflowException):
    """
    Description
    -----------

    This is the base-class for exceptions encountered within the
    ush/python/pygfs/ufswm module; it is a sub-class of
    WorkflowException.

    """
