"""
Module
------

    pygfs.task.fv3gfs_forecast (pygfs/task/fv3gfs_forecast.py)

Description
-----------

    This module contains the base-class module for Finite-Volume
    Cubed-Sphere (FV3) Global Forecast System (GFS) forecast model
    applications.

Classes
-------

    FV3GFS(config)

        This is the base-class object for all Finite-Volume
        Cubed-Sphere (FV3) Global Forecast System (GFS) forecast model
        applications; it is a sub-class of Forecast.

"""

# ----

__author__ = "Henry R. Winterbottom"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__version__ = 0.0

# ----

import os
from typing import Dict

from pygfs.exceptions import FV3GFSError
from pygfs.task.forecast import Forecast
from pygw.decorators import private

# ----


class FV3GFS(Forecast):
    """
    Description
    -----------

    This is the base-class object for all Finite-Volume Cubed-Sphere
    (FV3) Global Forecast System (GFS) forecast model applications; it
    is a sub-class of Forecast.

    Parameters
    ----------

    config: Dict

        A Python dictionary containing the application configuration
        attributes.

    """

    # The following attributes are mandatory for fixed-file syncing;
    # this should not be changed unless absolutely necessary.
    FIXED_MAND_ATTR_DICT = {
        "DATA": "DATA",
        "FIXgfs": "FIXgfs",
        "HOMEgfs": "HOMEgfs",
        "atm_res": "CASE",
        "ocn_res": "OCNRES",
    }

    def __init__(self: Forecast, config: Dict):
        """
        Description
        -----------

        Creates a new GFS object.

        """

        # Define the base-class attributes.
        super().__init__(config=config, model="FV3GFS")

        # HRW: THIS IS ALREADY DEFINED IN THE RUN-TIME ENVIRONMENT; DO
        # WE WANT TO KEEP IT HERE AS IT CURRENTLY HAS NOT BEARING BUT
        # COULD BE AN ATTRIBUTE COLLECTED FROM A YAML-FORMATTED
        # CONFIGURATION FILE IN THE FUTURE?
        # self.fcst_config.coupled = (self.fcst_config.app in [
        #                            "s2s", "s2sw", "s2swa"])

    @private
    def fixedfiles(self: Forecast) -> Dict:
        """
        Description
        -----------

        This method collects the fixed-file attributes from the
        run-time environment (e.g., configuration Python dictionary
        `config`), checks the validity of the respective files and/or
        attributes, and returns a Python dictionary containing the
        fixed-file attributes.

        Returns
        -------

        fixedfiles_dict: Dict

            A Python dictionary containing the fixed-file path
            attributes.

        Raises
        ------

        FV3GFSError:

            - raised a mandatory configuration attribute (see
              `FIXED_MAND_ATTR_DICT`) is not specified within the
              run-time environment/configuration.

            - raised if a mandatory directory tree (beneath `FIXgfs`)
              is either not a directory or does not exist.

            - raised if an application specific directory tree
              (beneath `FIXgfs`) is either not a dictionary or does
              not exist.

        """

        # Define the fixed-file attributes.
        for (fixed_attr_key, fixed_attr_value) in self.FIXED_MAND_ATTR_DICT.items():

            # Define the respective configuration attribute (i.e.,
            # `fixed_attr_key`) and assign the corresponding value
            # `fixed_attr_value`; proceed accordingly.
            value = self.config[fixed_attr_value]

            if isinstance(value, dict):
                value = self.runtime_config[fixed_attr_value]

            if value is None:
                msg = (
                    f"The configuration attribute {fixed_attr_key} could not "
                    "be determined from the run-time environment. Aborting!!!"
                )
                raise FV3GFSError(msg=msg)

            setattr(self.config, fixed_attr_key, value)

        # Define the top-level directory-tree path containing the
        # respective fixed-files and build the Python dictionary
        # required to link the application-specific fixed files.
        fix_dirpath = self.config.FIXgfs

        if (not os.path.isdir(fix_dirpath)) or (not os.path.exists(fix_dirpath)):
            msg = (
                f"The directory tree {fix_dirpath} is either not a directory "
                "or does not exist. Aborting!!!"
            )
            raise FV3GFSError(msg=msg)

        # Define the fixed-file directory tree attributes.
        fixed_attr_dict = {
            "FIX_aer": "aer",
            "FIX_am": "am",
            "FIX_lut": "lut",
            "FIX_orog": "orog",
            "FIX_ugwd": "ugwd",
        }

        for (fixed_attr_key, fixed_attr_value) in fixed_attr_dict.items():

            fix_subdirpath = os.path.join(fix_dirpath, fixed_attr_value)
            if (not os.path.isdir(fix_subdirpath)) and (
                not os.path.isfile(fix_subdirpath)
            ):
                msg = (
                    f"The directory tree path {fix_subdirpath} is either not "
                    "a directory does not exist; this may cause unexpected results "
                    "or failures."
                )
                self.logger.warning(msg=msg)

            setattr(
                self.config,
                fixed_attr_key,
                os.path.join(self.config.FIXgfs, fixed_attr_value),
            )

    def initialize(self: Forecast) -> None:
        """
        Description
        -----------

        This method initializes the FV3 GFS forecast application
        working directory; this includes the following:

        - Configuring the directory tree;

        - Copying the relevant fixed-files;

        - Building of the `model_configure` file for the respective
          application;

        - Building the `nems.configure` file for the respective
          application.

        """

        # Update the base-class method attributes.
        super().initialize()

        # Build the directory tree and copy the required fixed files
        # accordingly.
        self.fixedfiles()
        self.config_dirtree()

        # Build the respective UFS configuration files.
        self.build_model_configure()
        self.build_nems_configure()
