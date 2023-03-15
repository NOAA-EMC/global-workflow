from pygw.exceptions import WorkflowException


# ----

class GFSForecastError(WorkflowException):
    """This is the base-class for exceptions encountered within the
    ush/python/pygfs/forecast module; it is a sub-class of
    WorkflowException.

    """
