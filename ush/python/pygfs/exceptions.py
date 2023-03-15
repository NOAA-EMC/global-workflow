from pygw.exceptions import WorkflowException

# ----

__all__ = ["ForecastError", "GFSForecastError"]

# ----


class ForecastError(WorkflowException):
    """This is the base-class for exceptions encountered within the
    ush/python/pygfs/forecast module; it is a sub-class of
    WorkflowException.

    """

# ----


class GFSForecastError(WorkflowException):
    """This is the base-class for exceptions encountered within the
    ush/python/pygfs/forecast/gfs module; it is a sub-class of
    WorkflowException.

    """
