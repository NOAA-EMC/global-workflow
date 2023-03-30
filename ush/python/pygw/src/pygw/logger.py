"""
Module
------

    pygw.logger (pygw/src/pygw/logger.py)

Description
-----------

    This module contains the base-class object for all Logger
    instances.

Classes
-------

    ColoredFormatter(fmt)

        This is the base-class object for all logger object color
        formatting as a function logging level; it is a sub-class of
        logging.Formatter.

    Logger(name=None, level=None, _format=None, colored_log=False,
           logfile_path=None)

        This is the base-class object for all Logger object instances.

Functions
---------

    logit(logger, name= None, message= None)

        This function provides a decorator to be used for adding
        logging to a module, class, method, or function.

"""

# ----

__version__ = 1.0

# ----

import logging
import os
import sys
from dataclasses import dataclass
from functools import wraps
from pathlib import Path
from typing import Callable, List, Union

# ----


class ColoredFormatter(logging.Formatter):
    """
    Description
    -----------

    This is the base-class object for all logger object color
    formatting as a function logging level; it is a sub-class of
    logging.Formatter.

    Parameters
    ----------

    fmt: str

        A Python string defining the logging format.

    Returns
    -------

    formatter: object

        A Python object containing the format attributes for the
        logger object.

    Notes
    -----

    The methodology for this method has been collected from the
    following sources.

    - https://stackoverflow.com/a/56944256/3638629

    """

    # Define the colors available for the Formatter object.
    grey = "\x1b[38;21m"
    blue = "\x1b[38;5;39m"
    yellow = "\x1b[38;5;226m"
    red = "\x1b[38;5;196m"
    bold_red = "\x1b[31;1m"
    reset = "\x1b[0m"

    def __init__(self, fmt):
        """
        Description
        -----------

        Creates a new ColoredFormatter object.

        """

        # Define the base-class attributes.
        super().__init__()
        self.fmt = fmt
        self.formats = {
            logging.DEBUG: self.blue + self.fmt + self.reset,
            logging.INFO: self.grey + self.fmt + self.reset,
            logging.WARNING: self.yellow + self.fmt + self.reset,
            logging.ERROR: self.red + self.fmt + self.reset,
            logging.CRITICAL: self.bold_red + self.fmt + self.reset,
        }

    def format(self, record):
        """
        Description
        -----------

        This method defines and returns the logging object formatting
        attribute(s).

        Returns
        -------

        formatter: object

            A Python object containing the logging object formatting
            attributes.

        """

        # Define the logging object formatting attributes.
        log_fmt = self.formats.get(record.levelno)
        formatter = logging.Formatter(log_fmt).format(record)

        return formatter


# ----


@dataclass
class Logger:
    """
    Description
    -----------

    This is the base-class object for all Logger object instances.

    Keywords
    --------

    name: str, optional

        A Python object specifying the name for the respective Logger
        object.

    level: str, optional

        A Python string specifying the logging level for the
        respective Logger object; if NoneType upon entry, the run-time
        environment will be queried for the attribute `LOGGING_LEVEL`;
        if NoneTypee, the default value "INFO" will be assigned.

    _format: str, optional

        A Python string specifying the desired logging format; if
        NoneType upon entry the value for defined by the base-class
        attribute `DEFAULT_FORMAT` will be assigned.

    colored_log: bool, optional

        A Python boolean valued variable specifying whether to use the
        color attributes, defined within in the `ColoredFormatter`
        object, for the logging messages.

    logfile_path: Union[str, Path], optional

        A Python string or Path object defining the output filepath to
        which the logging information is to be written.

    Raises
    ------

    LookupError:

        - raised if the specified logging level is not supported.

    """

    # Define the supported (e.g., default) logging attributes.
    LOG_LEVELS = ["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"]

    DEFAULT_LEVEL = "INFO"

    DEFAULT_FORMAT = "%(asctime)s - %(levelname)-8s - %(name)-12s: %(message)s"

    def __init__(
        self: object,
        name: str = None,
        level: str = None,
        _format: str = DEFAULT_FORMAT,
        colored_log: bool = False,
        logfile_path: Union[str, Path] = None,
    ):
        """
        Description
        -----------

        Creates a new Logger object.

        """

        # Define the base-class attributes.
        self.name = name
        self.level = level
        self.format = _format
        self.colored_log = colored_log

        # Define the logger object attributes; proceed accordingly.
        if level is None:
            level = os.environ.get("LOGGING_LEVEL")

            if level is None:
                level = Logger.DEFAULT_LEVEL

        self.level = level.upper()

        if self.level not in Logger.LOG_LEVELS:
            raise LookupError(
                f"{self.level} is an unknown logging level; "
                + f'Currently supported log levels are {", ".join(Logger.LOG_LEVELS)}'
            )

        self._logger = logging.getLogger(name) if name else logging.getLogger()
        self._logger.setLevel(self.level)

        # Define the console handler for the logging object.
        _handlers = []

        _handler = Logger.add_stream_handler(
            level=self.level,
            _format=self.format,
            colored_log=self.colored_log,
        )
        _handlers.append(_handler)
        self._logger.addHandler(_handler)

        # Define the file handler for the logging object (if
        # applicable).
        if logfile_path is not None:
            _handler = Logger.add_file_handler(
                logfile_path, level=self.level, _format=self.format
            )
            self._logger.addHandler(_handler)
            _handlers.append(_handler)

    def __getattr__(self: object, attribute: str) -> Union[str, int, float, bool]:
        """
        Description
        -----------

        This method allows the calling logging module methods
        directly.

        Parameters
        ----------

        attribute: str

            A Python object specifying the attribute name for the
            logging object.

        Returns
        -------

        attribute : str

            A Python string defining the value for the specified
            attribute.

        """

        # Collect the attribute specified upon entry from the logger
        # object.
        logger_attr = getattr(self._logger, attribute)

        return logger_attr

    @classmethod
    def add_handlers(
        cls: object, logger: logging.Logger, handlers: List[logging.Handler]
    ) -> logging.Logger:
        """
        Description
        -----------

        This method adds a list of handlers to a logging object.

        Parameters
        ----------

        logger: logging.Logger

            A Python logging library Logger object to which a new
            handler is to be added.

        handlers: List

            A Python list of supported handlers to be added to the
            logger object.

        Returns
        -------

        logger: logging.logger

            A Python logging library Logger object containing the
            specified (supported) logging handler attributes.

        """

        # Update the logging object with the respective (supported)
        # logging handlers.
        for handler in handlers:
            logger.addHandler(handler)

        return logger

    @classmethod
    def add_file_handler(
        cls: object, logfile_path: Union[str, Path], level: str, _format: str
    ) -> logging.Handler:
        """
        Description
        -----------

        This method allows the setting of custom file handlers for
        caller modules.

        Parameters
        ----------

        logfile_path: str or Path

            A Python string or Path object to where the log files are
            to be written.

        level: str

            A Python string specifying the logging level for the
            respective Logger object.

        _format: str

            A Python string specifying the desired logging format.

        Returns
        -------

        handler: logging.Handler

            A Python logging.Handler object defining the file
            handler for the logging object.

        """

        # Create the directory containing the logfile_path
        logfile_path = Path(logfile_path)
        if not logfile_path.parent.is_dir():
            logfile_path.mkdir(parents=True, exist_ok=True)

        handler = logging.FileHandler(str(logfile_path))
        handler.setLevel(level)
        handler.setFormatter(logging.Formatter(_format))

        return handler

    @classmethod
    def add_stream_handler(
        cls: object, level: str, _format: str, colored_log: bool
    ) -> logging.Handler:
        """
        Description
        -----------

        This method creates a stream handler to allow the
        specification of custom stream handlers for caller modules.

        Parameters
        ----------

        level: str

            A Python string specifying the logging level for the
            respective Logger object.

        _format: str

            A Python string specifying the desired logging format.

        colored_log: bool

            A Python boolean valued variable specifying whether to use the
            color attributes, defined within in the `ColoredFormatter`
            object, for the logging messages.

        Returns
        -------

        handler: logging.Handler

            A Python logging.Handler object defining the stream
            handler for the logging object.

        """

        # Define the stream handler for the respective caller module.
        handler = logging.StreamHandler(sys.stdout)
        handler.setLevel(level)
        _format = (
            ColoredFormatter(
                _format) if colored_log else logging.Formatter(_format)
        )
        handler.setFormatter(_format)

        return handler

    def get_logger(self: object) -> object:
        """
        Description
        -----------

        This method returns the logging object.

        Returns
        -------

        logger: object

            A Python logging object.

        """

        return self._logger


# ----


def logit(logger: object, name: str = None, message: str = None) -> Callable:
    """
    Description
    -----------

    This function provides a decorator to be used for adding logging
    to a module, class, method, or function.

    Parameters
    ----------

    logger: object

        A Python defining the Python Logger object.

    Keywords
    --------

    name: str, optional

        A Python string defining the module to be logged; if NoneType
        upon entry this value will default to the calling module
        `__module__` attribute.  Name of the module to be logged

    message: str, optional

        A Python string the function to be logged; if NoneType upon
        entry this value will default to the calling function
        `__name__` attribute.

    Returns
    -------

    decorate: Callable

        The Python decorator containing the Python Logger attributes.

    """

    # Definee the decorator function.
    def decorate(func):

        # Define the logger attributes.
        log_name = name if name else func.__module__
        log_msg = message if message else log_name + "." + func.__name__

        # Execute the logger for the calling module, class, and/or
        # function; proceed accordingly.
        @wraps(func)
        def wrapper(*args, **kwargs):

            # Collect (any) arguments and keyword arguments.
            passed_args = [repr(arg) for arg in args]
            passed_kwargs = [
                f"{key}={repr(value)}" for (key, value) in list(kwargs.items())
            ]

            # Define the logger message string.
            msg = f"BEGIN: {log_msg}"
            logger.info(msg)
            if logger.level == "DEBUG":
                logger.debug(f"( {', '.join(passed_args + passed_kwargs)} )")

            # Call the appropriate logger function.
            retval = func(*args, **kwargs)

            # Define the logger message string, if the logging level
            # is DEBUG, return the value returned by the logger
            # function.
            msg = f"END: {log_msg}"
            logger.info(msg)
            if logger.level == "DEBUG":
                logger.debug(f"RETURNING: {retval}")

            return retval

        return wrapper

    return decorate
