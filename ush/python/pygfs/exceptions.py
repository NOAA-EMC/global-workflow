"""
Module
------

    pygfs.exceptions (pygfs/exceptions.py)

Description
-----------

    This module contains all pygfs package exceptions.

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

    GridsError(msg)

        This is the base-class for exceptions encountered within the
        ush/python/pygfs/utils/grids module; it is a sub-class of
        WorkflowException.

    TimestampsError(msg)

        This is the base-class for exceptions encountered within the
        ush/python/pygfs/utils/timestamps module; it is a sub-class of
        WorkflowException.

    UFSWMError(msg)

        This is the base-class for exceptions encountered within the
        ush/python/pygfs/ufswm module; it is a sub-class of
        WorkflowException.

"""

# ----

__author__ = "Henry R. Winterbottom"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__version__ = 0.0

# ----

from pygw.exceptions import WorkflowException

# ----

# Define all available classes.
__all__ = ["ForecastError", "GFSError", "GridsInfoError", "TimestampsError",
           "UFSWMError"]

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


class GridsError(WorkflowException):
    """
    Description
    -----------

    This is the base-class for exceptions encountered within the
    ush/python/pygfs/utils/grids module; it is a sub-class of
    WorkflowException.

    """

# ----


class TimestampsError(WorkflowException):
    """
    Description
    -----------

    This is the base-class for exceptions encountered within the
    ush/python/pygfs/utils/timestamps module; it is a sub-class of
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
