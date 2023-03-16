

# ----

from dataclasses import dataclass

from typing import Dict, Tuple

from pygw.attrdict import AttrDict
from pygw.logger import Logger, logit

# ----

base_logger = Logger(level="error", colored_log=True)

# ----


class Grids:
    """

    """

    def __init__(self, case):
        """
        Description
        -----------

        Creates a new Grids object.

        """

        # Define the base-class attributes.
        self.case = case
        self.grid_config = AttrDict()

        # Define the allowable grid methods and collect the respective
        # resolution grid attributes accordingly.
        self.grid_res_dicts = {"c48": self.c48
                               }

        self.grid_config["input.nml"] = self.grid_res_dicts[self.case.lower()]

    def c48(self) -> Tuple[Dict]:
        """

        """

        # Define the default namelist (i.e., input.nml) attributes for
        # the respective grid resolution; these may be overridden by
        # the experiment configuration attributes.
        input_nml = {"deltim": 1200.0,
                     "layout_x": 1,
                     "layout_y": 1,
                     "layout_x_gfs": 1,
                     "layout_y_gfs": 1,
                     "cdmbgwd": "0.071,2.1,1.0,1.0"}

        return (input_nml)
