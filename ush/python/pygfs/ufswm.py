"""
Module
------

    pygfs.ufswm (pygfs/ufswm.py)

Description
-----------

    This module contains the base-class module for all Unified
    Forecast System (UFS) Weather Model (WM) applications.

Classes
-------

    UFSWM(config)

        This is the base-class object for all Unified Forecast System
        (UFS) Weather Model (WM) applications.

"""

# ----

__author__ = "Henry R. Winterbottom"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__version__ = 0.0

# ----

from dataclasses import dataclass
from typing import Dict

from pygw.decorators import private

from pygfs.utils.grids import FV3GFS as FV3GFS_grids
from pygfs.utils.layout import FV3GFS as FV3GFS_layout
from pygfs.utils.logger import Logger
from pygfs.utils.timestamps import Timestamps

# ----


@dataclass
class UFSWM:
    """
    Description
    -----------

    This is the base-class object for all Unified Forecast System
    (UFS) Weather Model (WM) applications.

    Parameters
    ----------

    config: Dict

        A Python dictionary containing the application configuration
        attributes.

    """

    def __init__(self: dataclass, config: Dict, model: str):
        """
        Description
        -----------

        Creates a new UFSWM object.

        """

        # Define the base-class attributes.
        self.config = config
        self.model = model.lower()
        self.logger = Logger(config=self.config).logger

        # Define the respective forecast model configuration
        # attributes.
        self.configure()

    @private
    def fv3gfs(self: dataclass) -> None:
        """
        Description
        -----------

        This method defines the configuration attributes for the UFS
        FV3 GFS forecast component model.

        """

        # Define the configuration attributes for the FV3 GFS forecast
        # model.
        self.config.ufswm.atmos.grids = FV3GFS_grids(
            config=self.config,
            model="FV3GFS",
            res=self.config.CASE,
            nlevs=self.config.LEVS,
        ).grids

        # HRW: The following may eventually be moved out of this
        # method into `configure`; this is dependent on the additional
        # UFS component model needs; TBD.
        self.config.ufswm.atmos.timestamps = Timestamps(
            datestr=self.config.CDATE, fmt="%Y-%m-%d %H:%M:%S"
        ).timestamps

        msg = (
            "\nThe atmosphere model configuration is as follows:\n\n"
            f"Cubed sphere resolution: {self.config.ufswm.atmos.grids.csres.upper()}\n"
            f"Vertical levels: {self.config.ufswm.atmos.grids.nlevs}\n"
            f"Number of cubed sphere tiles: {self.config.ufswm.atmos.grids.ntiles}.\n"
        )
        self.logger.warn(msg=msg)

        # Define the layout attribuites for the FV3 GFS forecast
        # model.
        self.config.ufswm.atmos.layout = FV3GFS_layout(
            config=self.config, res=self.config.CASE
        ).setup()

    def configure(self: dataclass) -> None:
        """
        Description
        -----------

        This method collects and defines the configuration attributes
        for the respective UFS forecast component model.

        """

        # UFS WM atmosphere FV3 GFS forecast model.
        if self.model == "fv3gfs":
            self.fv3gfs()
