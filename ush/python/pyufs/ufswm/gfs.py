"""
Module
------

    gfs.py

Description
-----------

    This module contains the base-class object for all Global
    Forecasting System (GFS) forecast applications.

Classes
-------

    GFS(config)

        This is the base-class object for all GFS forecast
        applications; it is a sub-class of UFSWM.

Author(s)
---------


    Henry R. Winterbottom; 28 March 2023

History
-------

    2023-03-28: Henry Winterbottom -- Initial implementation.

"""

# ----

from pyufs.ufswm.ufswm import UFSWM, base_logger
from pyufs.exceptions import GFSError

# ----


class GFS(UFSWM):
    """
    Description
    -----------

    This is the base-class object for all GFS forecast applications;
    it is a sub-class of UFSWM.

    Parameters
    ----------

    config: object

        A Python object containing the application configuration
        attributes.

    """

    def __init__(self: UFSWM, config: object):
        """
        Description
        -----------

        Creates a new GFS object.

        """

        # Define the base-class attributes.
        model = "GFS"
        super().__init__(config=config, model=model, app=self.config.APP)
