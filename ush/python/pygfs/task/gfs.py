
import os

from pygfs.task.forecast import Forecast
from pygfs.exceptions import GFSError
from typing import Dict, List
from pygw.logger import Logger, logit

from pygw.attrdict import AttrDict
from pygw.yaml_file import YAMLFile

# ----

logger = Logger(name="GFS", colored_log=True)

# ----

# The following attributes are mandatory for fixed-file syncing;
# this should not be changed unless absolutely necessary.
FIXED_MAND_ATTR_DICT = {"DATA": "DATA",
                        "FIXgfs": "FIXgfs",
                        "HOMEgfs": "HOMEgfs",
                        "atm_res": "CASE",
                        "coupled": "cpl",
                        "ocn_res": "OCNRES"
                        }

# ----


class GFS(Forecast):
    """

    """

    def __init__(self: Forecast, config: Dict):
        """
        Description
        -----------

        Creates a new GFS object.

        """

        # Define the base-class attributes.
        super().__init__(config=config, model="GFS")

        self.fcst_config.model = "gfs"
        self.fcst_config.ntiles = 6
        self.fcst_config.fix_path = self.config.FIXgfs

    def __fixedfiles(self: Forecast) -> Dict:
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

        GFSError:

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
        fixedfiles_dict = AttrDict()

        for (fixed_attr_key, fixed_attr_value) in FIXED_MAND_ATTR_DICT.items():

            # Define the respective configuration attribute (i.e.,
            # `fixed_attr_key`) and assign the corresponding value
            # `fixed_attr_value`; proceed accordingly.
            value = self.config[fixed_attr_value]

            if isinstance(value, dict):
                value = self.runtime_config[fixed_attr_value]

            if value is None:
                msg = (f"The configuration attribute {fixed_attr_key} could not "
                       "be determined from the run-time environment. Aborting!!!"
                       )
                raise GFSError(msg=msg)

            setattr(self.fcst_config, fixed_attr_key, value)

        # Define the top-level directory-tree path containing the
        # respective fixed-files and build the Python dictionary
        # required to link the application-specific fixed files.
        fix_dirpath = self.fcst_config.FIXgfs

        if (not os.path.isdir(fix_dirpath)) or (not os.path.exists(fix_dirpath)):
            msg = (f"The directory tree {fix_dirpath} is either not a directory "
                   "or does not exist. Aborting!!!"
                   )
            raise GFSError(msg=msg)

        # Define the fixed-file directory tree attributes.
        fixed_attr_dict = {"FIX_aer": "aer",
                           "FIX_am": "am",
                           "FIX_lut": "lut",
                           "FIX_orog": "orog",
                           "FIX_lut": "lut",
                           "FIX_ugwd": "ugwd",
                           }

        for (fixed_attr_key, fixed_attr_value) in fixed_attr_dict.items():

            fix_subdirpath = os.path.join(fix_dirpath, fixed_attr_value)
            if (not os.path.isdir(fix_subdirpath)) and (not os.path.isfile(fix_subdirpath)):
                msg = (f"The directory tree path {fix_subdirpath} is either not "
                       "a directory does not exist; this may cause unexpected results "
                       "or failures."
                       )
                self.logger.warning(msg=msg)

            setattr(self.fcst_config, fixed_attr_key, os.path.join(
                self.config.FIXgfs, fixed_attr_value))

    def initialize(self: Forecast):
        """

        """

        # Update the base-class method attributes.
        super().initialize()

        # Build the directory tree and copy the required fixed files
        # accordingly.
        self.__fixedfiles()
        self.build_dirtree()

        # Build the respective UFS configuration files.
