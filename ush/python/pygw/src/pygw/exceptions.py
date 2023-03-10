# pylint: disable=unused-argument

# ----

from collections.abc import Callable

from pygw.logger import Logger, logit

logger = Logger(level="error", colored_log=True)

__all__ = ["Error", "msg_except_handle"]


class Error(Exception):
    """
    Description
    -----------

    This is the base-class for all exceptions; it is a sub-class of
    Exceptions.

    Parameters
    ----------

    msg: str

        A Python string containing a message to accompany the
        exception.

    """

    @logit(logger)
    def __init__(self: Exception, msg: str):
        """
        Description
        -----------

        Creates a new Error object.

        """

        # Define the base-class attributes.
        logger.error(msg=msg)
        super().__init__()


# ----


def msg_except_handle(err_cls: object) -> Callable:
    """
    Description
    -----------

    This function provides a decorator to be used to raise specified
    exceptions.

    Parameters
    ----------

    err_cls: object

        A Python object containing the Error subclass to be used for
        exception raises.

    Parameters
    ----------

    decorator: Callable

        A Python decorator.

    """

    # Define the decorator function.
    def decorator(func: Callable):

        # Execute the caller function; proceed accordingly.
        def call_function(msg: str) -> None:

            # If an exception is encountered, raise the respective
            # exception.
            raise err_cls(msg=msg)

        return call_function

    return decorator
