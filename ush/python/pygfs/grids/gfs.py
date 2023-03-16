

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

    @logit(base_logger)
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

        (self.grid_config["input.nml"],
         self.grid_config["model_configure"]) = \
            self.grid_res_dicts[self.case.lower()]

        # Define the atmosphere model (FV3) grid dimensions
        # accordingly.
        self.grid_config["atmos_dims"] = AttrDict()
        (self.grid_config["atmos_dims"]["npx"],
         self.grid_config["atmos_dims"]["npy"]) = \
            [(int(self.case[1::]) + 1) for idx in range(2)]

    @logit(base_logger)
    def c48(self) -> Tuple[Dict]:
        """

        """

        # Define the default namelist (i.e., input.nml) attributes for
        # the respective grid resolution; these may be overridden by
        # the experiment configuration attributes.
        input_nml = {"cdmbgwd": "0.071,2.1,1.0,1.0",
                     "layout_x": 1,
                     "layout_y": 1,
                     }

        # Define the model configuration (i.e., model_configure)
        # attributes for the respective grid resolution; these may be
        # overridden by the experiment configuration attributes.
        model_configure = {"deltim": 1200.0,
                           "WRITE_GROUP": 1,
                           }

        return (input_nml, model_configure)
