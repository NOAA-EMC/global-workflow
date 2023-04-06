"""
Module
------

    pygfs.utils.layout (pygfs/utils/layout.py)

Description
-----------

    This module contains the base-class objects for all Unified
    Forecast System (UFS) Weather Model (WM) component model layout
    attributes.

Classes
-------

    Layout()

        This is the base-class object for all Unified Forecast System
        (UFS) Weather Model (WM) configuration file attributes.

    FV3GFS(config, res)

        This is the base-class object for defining all FV3 GFS layout
        attributes as a function of cubed-sphere resolution; it is a
        sub-class of Layout.

"""

# ----

__author__ = "Henry R. Winterbottom"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__version__ = 0.0

# ----

from dataclasses import dataclass
from typing import Dict

from pygw.attrdict import AttrDict
from pygw.decorators import private

# ----


@dataclass
class Layout:
    """
    Description
    -----------

    This is the base-class object for all Unified Forecast System
    (UFS) Weather Model (WM) configuration file attributes.

    """

    def __init__(self: dataclass):
        """
        Description
        -----------

        Creates a new Layout object.

        """

        # Define the base-class attributes.
        self.layout = AttrDict()


# ----


class FV3GFS(Layout):
    """
    Description
    -----------

    This is the base-class object for defining all FV3 GFS layout
    attributes as a function of cubed-sphere resolution; it is a
    sub-class of Layout.

    Parameters
    ----------

    config: Dict

        A Python dictionary containing the run-time environment
        configuration.

    res: str

        A Python string specifying a valid cubed-sphere resolution
        (e.g., C48, C96, etc.,)

    """

    def __init__(self: Layout, config: Dict, res: str):
        """
        Description
        -----------

        Creates a new FV3GFS object.

        """

        # Define the base-class attributes.
        super().__init__()
        self.config = config
        self.csres = res.lower()
        self.res = int(self.csres[1:])

    @private
    def c48(self: Layout) -> Dict:
        """
        Description
        -----------

        This method defines the FV3 GFS layout attributes for a C48
        cubed-sphere resolution.

        Returns
        -------

        layout_dict: Dict

            A Python dictionary containing the layout attributes for
            the respective FV3 GFS cubed-sphere resolution.

        """

        # Define the FV3GFS C48 cubed-sphere resolution attributes.
        layout_dict = {
            "deltim": 1200,
            "layout_x": 1,
            "layout_y": 1,
            "layout_x_gfs": 1,
            "layout_y_gfs": 1,
            "nthreads_fv3": 1,
            "nthreads_fv3_gfs": 1,
            "cdmbgwd": [0.071, 2.1, 1.0, 1.0],
            "write_group": 1,
            "wrttask_per_group": 2,
            "write_group_gfs": 1,
            "wrttask_per_group_gfs": 2,
            "quilting": ".false.",
        }

        return layout_dict

    def setup(self: Layout):
        """
        Description
        -----------

        This method defines the FV3 GFS cubed-sphere resolution layout
        attributes.

        """

        # Define the FV3 GFS layout attributes as a function of the
        # cubed-sphere resolution.
        layout_methods_dict = {"c48": self.c48}

        layout_dict = layout_methods_dict[self.csres]()

        # Compute additional FV3 GFS layout attributes as a function
        # of the cubed-sphere resolution.
        layout_chunks_dict = {
            "ichunk2d": (4 * self.res),
            "ichunk3d": (4 * self.res),
            "jchunk2d": (2 * self.res),
            "jchunk3d": (2 * self.res),
            "kchunk3d": 1,
        }

        layout_dict = {**layout_dict, **layout_chunks_dict}

        # Update the base-class Python dictionary.
        for (layout_key, layout_value) in layout_dict.items():
            setattr(self.layout, layout_key, layout_value)
