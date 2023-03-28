"""
Module
------

    ufswm.py

Description
-----------

    This module contains the base-class object for all Unified
    Forecast System (UFS) weather-model (WM) applications.

Classes
-------

    UFSWM(config, model, attr, *args, **kwargs)

        This is the base-class object for all UFS weather-model
        forecast and related applications; it is a sub-class of Task.

Author(s)
---------


    Henry R. Winterbottom; 28 March 2023

History
-------

    2023-03-28: Henry Winterbottom -- Initial implementation.

"""

# ----

from pygw.logger import Logger
from pygw.task import Task
from pyufs.exceptions import UFSWMError

# ----


class UFSWM(Task):
    """
    Description
    -----------

    This is the base-class object for all UFS weather-model forecast
    and related applications; it is a sub-class of Task.

    Parameters
    ----------

    config: object

        A Python object containing the application configuration
        attributes.

    model: str

        A Python string specifying the supported forecast model
        application.

    app: str

        A Python string specifying the supported forecast model
        application/type.

    Other Parameters
    ----------------

    args: Tuple

        A Python tuple of arguments to be based to the base-class
        constructor.

    kwargs: Dict

        A Python dictionary containing keyword arguments to pass to
        the base-class constructor.

    Raises
    ------

    UFSWMError:

        - raised if a specified forecast model is not supported.

    """

    def __init__(self: Task, config: object, model: str, app: str, *args, **kwargs):
        """
        Description
        -----------

        Creates a new UFSWM object.

        """

        # Define the base-class attributes.
        super().__init__(config, *args, **kwargs)
        self.app = app
        self.config = config
        self.model = model.lower()
        self.logger = Logger(level="info", colored_log=True)

        # Define the supported forecast models and the respective
        # attributes; build the base-class dictionary; model keys
        # (e.g., gfs) should be lowercase for generalization purposes.
        self.fcst_model_dict = {"gfs": {"ntiles": 6}}

        if self.model not in self.fcst_model_dict:
            msg = f"Forecast model {self.model} is not supported. Aborting!!!"
            raise UFSWMError(msg=msg)
