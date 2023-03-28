"""
Module
------

    exceptions.py

Description
-----------

    This module contains all pyufs package exceptions.

Classes
-------

    GFSError(msg)

        This is the base-class for exceptions encountered within the
        ush/python/pyufs/ufswm/gfs module; it is a sub-class of
        WorkflowException.

    UFSWMError(msg)

        This is the base-class for exceptions encountered within the
        ush/python/pyufs/ufswm module; it is a sub-class of
        WorkflowException.

Author(s)
---------


    Henry R. Winterbottom; 28 March 2023

History
-------

    2023-03-28: Henry Winterbottom -- Initial implementation.

"""

# ----

from pygw.exceptions import WorkflowException

# ----

# Define all available classes.
__all__ = ["GFSError", "UFSWMError"]

# ----


class GFSError(WorkflowException):
    """
    Description
    -----------

    This is the base-class for exceptions encountered within the
    ush/python/pyufs/ufswm/gfs module; it is a sub-class of
    WorkflowException.

    """


# ----


class UFSWMError(WorkflowException):
    """
    Description
    -----------

    This is the base-class for exceptions encountered within the
    ush/python/pyufs/ufswm module; it is a sub-class of
    WorkflowException.

    """
