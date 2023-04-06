"""
Module
------

    pygfs.utils.grids (pygfs/utils/grids.py)

Description
-----------

    This module contains the base-class object for all grid definition
    applications; the module also contains sub-classes for the
    respective, and supported, Unified Forecast System (UFS) Weather
    Model (WM) component models.

Classes
-------

    FV3GFS(config, res, nlevs)

        This is the base-class object for all FV3 GFS grid attribute
        defintions as a function grid-spacing resolution; it is a
        sub-class of Grids.

    Grids()

        This is the base-class object for all forecast model grid
        attribute computations and definitions.

"""

# ----

__author__ = "Henry R. Winterbottom"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__version__ = 0.0

# ----

from typing import Dict, Union

from dataclasses import dataclass

from pygfs.exceptions import GridsError
from pygw.attrdict import AttrDict
from pygw.decorators import private

# ----


@dataclass
class Grids:
    """
    Description
    -----------

    This is the base-class object for all forecast model grid
    attribute computations and definitions.

    """

    def __init__(self: dataclass):
        """
        Description
        -----------

        Creates a new Grids object.

        """

        # Define the base-class attributes.
        self.grids = AttrDict()

# ----


class FV3GFS(Grids):
    """
    Description
    -----------

    This is the base-class object for all FV3 GFS grid attribute
    defintions as a function grid-spacing resolution; it is a
    sub-class of Grids.

    Parameters
    ----------

    config: Dict

        A Python dictionary containing the run-time environment
        configuration.

    res: str

        A Python string specifying a valid cubed-sphere resolution
        (e.g., C48, C96, etc.,)

    nlevs: int

        A Python integer specifying a valid number of staggered
        vertical levels.

    """

    # Define the supported options for the respective forecast model
    # attributes; only GFS configurations are currently supported.
    FV3GFS_RES_LIST = ["c48", "c96", "c192", "c384"]
    FV3GFS_LEVS_LIST = [64, 128]

    def __init__(self: Grids, config: Dict, res: str, nlevs: int):
        """
        Description
        -----------

        Creates a new GFSFV3 object.

        """

        # Define the base-class attributes.
        super().__init__()
        self.grids.ntiles = 6
        self.grids.nlevs = nlevs
        self.grids.csres = res
        self.grids.res = int(self.grids.csres[1:])

        # Validate and build the grids information attributes.
        self.check()
        self.config()

    @private
    def check(self: Grids):
        """
        Description
        -----------

        This method checks the validity of both the cubed-sphere
        resolution and total number of staggered vertical levels.

        Raises
        ------

        GridsError:

            - raised if the specified cubed-sphere resolution is not a
              valid cubed-sphere resolution.

            - raised if the number of staggered vertical levels is not
              supported.

        """
        # Check that the base-class object containing the FV3 GFS
        # forecast model attributes is valid; proceed accordingly.
        if self.grids.csres.lower() not in self.FV3GFS_RES_LIST:
            msg = (
                f"The cubed-sphere resolution {self.grids.csres.upper()} is not "
                "supported; valid values are: "
                f"{', '.join([res.upper() for res in FV3GFS_RES_LIST])}. Aborting!!!"
            )
            raise GridsError(msg=msg)

        if self.grids.nlevs not in self.FV3GFS_LEVS_LIST:
            msg = (f"The specified number of vertical levels {self.grids.levs}"
                   f"is not supported; valid values are {','.join(FV3GFS_LEVS_LIST)}. "
                   "Aborting!!!"
                   )
            raise GridsError(msg=msg)

    @private
    def config(self: Grids):
        """
        Description
        -----------

        This method defines the FV3 GFS atmosphere model grid
        configuration attributes.

        """

        # Compute and define the grid attributes using the resolution
        # of the cubed-sphere.
        grids_dict = {
            "jcap": int(self.grids.res * 2) - 2,
            "lonb": int(4*self.grids.res),
            "latb": int(2*self.grids.res),
            "npx": int(self.grids.res + 1),
            "npy": int(self.grids.res + 1),
            "npz": int(self.grids.nlevs - 1)
        }

        [setattr(self.grids, grids_key, grids_value)
         for (grids_key, grids_value) in grids_dict.items()]
