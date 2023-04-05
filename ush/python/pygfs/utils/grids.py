"""
Module
------

    pygfs.utils.grids (pygfs/utils/grids.py)

Description
-----------



Classes
-------



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

# ----

# Define the supported options for the respective forecast model
# attributes; only GFS configurations are currently supported.
FV3GFS_RES_LIST = ["c48", "c96", "c192", "c384"]
FV3GFS_LEVS_LIST = [64, 128]

# ----


@dataclass
class Grids:
    """
    Description
    -----------

    """

    def __init__(self: dataclass, model: str, res: Union[str, int]):
        """
        Description
        -----------

        Creates a new Grids object.

        """

        # Define the base-class attributes.
        self.grids = AttrDict()
        self.model = model.lower()
        self.res = str(res)

# ----


class FV3GFS(Grids):
    """ """

    def __init__(self: Grids, config: Dict, model: str, res: str,
                 nlevs: int):
        """
        Description
        -----------

        Creates a new GFSFV3 object.

        """

        # Define the base-class attributes.
        super().__init__(model=model, res=res)
        self.grids.ntiles = 6
        self.grids.nlevs = nlevs
        self.grids.csres = self.res
        self.grids.res = int(self.res[1:])

        # Validate and build the grids information attributes.
        self.__check()
        self.__config()

    def __check(self: Grids):
        """

        """
        # Check that the base-class object containing the FV3 GFS
        # forecast model attributes is valid; proceed accordingly.
        if self.grids.csres.lower() not in FV3GFS_RES_LIST:
            msg = (
                f"The cubed sphere resolution {self.grids.csres.upper()} is not "
                "supported; valid values are: "
                f"{', '.join([res.upper() for res in FV3GFS_RES_LIST])}. Aborting!!!"
            )
            raise GridsError(msg=msg)

        if self.grids.nlevs not in FV3GFS_LEVS_LIST:
            msg = (f"The specified number of vertical levels {self.grids.levs}"
                   f"is not supported; valid values are {','.join(FV3GFS_LEVS_LIST)}. "
                   "Aborting!!!"
                   )
            raise GridsError(msg=msg)

    def __config(self: Grids):
        """
        Description
        -----------

        This method defines the FV3 GFS atmosphere model grid
        configuration attributes.

        """

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
