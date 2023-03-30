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
from typing import Any, Dict

from pygw.attrdict import AttrDict
from pygw.logger import Logger

from pygfs.exceptions import UFSWMError

# ----

# Define the supported options for the respective forecast model
# attributes; only GFS configurations are currently supported.
ATM_RES_LIST = ["c48", "c96", "c192", "c384"]

ATM_LEVS_LIST = [64, 128]

ATM_NTILES_LIST = [6]

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

    def __init__(self, config: Dict):
        """
        Description
        -----------

        Creates a new UFSWM object.

        """

        # Define the base-class attributes.
        self.config = config
        self.logger = Logger(level=self.config.loglev, colored_log=True)

        # Configure the respective forecast models.
        self.atmos_grid = self.__atmos_grid_setup(
            atm_res=self.config.CASE,
            atm_levs=self.config.LEVS,
            atm_ntiles=self.config.ntiles,
            logger=self.logger,
        )

    @staticmethod
    def __atmos_grid_setup(
        atm_res: str, atm_levs: int, atm_ntiles: int, logger: object
    ) -> Dict[str, Any]:
        """
        Description
        -----------

        This method defines the atmosphere model (e.g., FV3) grid
        configuration attributes.

        Parameters
        ----------

        atm_res: str

            A Python string define the atmosphere model resolution;
            this should be of the form `C##` where `##` is the
            cubed-sphere resolution (e.g., 48, 96, 192, etc.,).

        atm_levs: int

            A Python integer defining the total number of levels for
            the atmosphere model configuration.

        atm_ntiles: int

            A Python integer defining the total number of tiles for
            the cubed-sphere (i.e., FV3 forecast model) application.

        logger: object

            A Python object containing the defined logger object.

        Returns
        -------

        atm_config: Dict

            A Python dictionary containing the atmosphere model
            configuration established via the parameter attributes.

        """

        # Check that the parameter attribute values are valid; proceed
        # accordingly.
        atm_config = AttrDict()
        if atm_res.lower() not in ATM_RES_LIST:
            msg = (
                f"The cubed sphere resolution {atm_res.upper()} is not "
                "supported; valid values are: "
                f"{', '.join([res.upper() for res in ATM_RES_LIST])}. "
                "Aborting!!!"
            )
            raise UFSWMError(msg=msg)

        if atm_levs not in ATM_LEVS_LIST:
            msg = (
                f"The number of vertical levels {atm_levs} is not supported; "
                f"valid values are {','.join(ATM_LEVS_LIST)}. "
                "Aborting!!!"
            )
            raise UFSWMError(msg=msg)

        if atm_ntiles not in ATM_NTILES_LIST:
            msg = (
                f"The specified number of cubed-sphere tiles {atm_ntiles} is not "
                f"supported; value values are {','.join(ATM_NTILES_LIST)}. "
                "Aborting!!!"
            )
            raise UFSWMError(msg=msg)

        msg = (
            "\nThe atmosphere model configuration is as follows:\n\n"
            f"Cubed sphere resolution: {atm_res.upper()}\n"
            f"Vertical levels: {atm_levs}\n"
            f"Number of cubed sphere tiles: {atm_ntiles}.\n"
        )
        logger.warn(msg=msg)

        # Define all atmosphere model configuration attributes with
        # respect to the parameter attributes.
        atm_config = AttrDict()

        atm_config.case_res = atm_res
        atm_config.csg_res = int(atm_config.case_res[1:])

        atm_config.jcap = int((atm_config.csg_res * 2) - 2)
        atm_config.lonb = int(4 * atm_config.csg_res)
        atm_config.lonb = int(2 * atm_config.csg_res)
        atm_config.npx = int(atm_config.csg_res + 1)
        atm_config.npy = int(atm_config.csg_res + 1)
        atm_config.npz = int(atm_levs - 1)

        return atm_config
