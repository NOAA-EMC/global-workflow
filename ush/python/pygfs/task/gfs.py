
import os

from pygfs.task.forecast import Forecast
from pygfs.exceptions import GFSError
from typing import Dict, List
from pygw.logger import logit

from pygw.attrdict import AttrDict

# ----

# The following attributes are mandatory for fixed-file syncing; this
# should not be changed unless absolutely necessary.
FIXED_MAND_ATTR_DICT = {"DATA": "DATA",
                        "FIXgfs": "FIXgfs",
                        "HOMEgfs": "HOMEgfs",
                        "atm_res": "CASE",
                        "ocn_res": "OCNRES"
                        }

# ----

logger = Logger().build_logger()

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
        self.gfs_app = self.config.APP
        self.fixed_path = self.config.FIXgfs

    @staticmethod
    @logit(logger)
    def __fixedfiles(config: Dict, logger: object,
                     fixed_list: List = None) -> Dict:
        """
        Description
        -----------

        This method collects the fixed-file attributes from the
        run-time environment (e.g., configuration Python dictionary
        `config`), checks the validity of the respective files and/or
        attributes, and returns a Python dictionary containing the
        fixed-file attributes.

        Parameters
        ----------

        config: Dict

            A Python dictionary containing the configuration
            attributes collected from the run-time environment.

        logger: object

            A Python object containing the defined logger object.

        Keywords
        --------

        fixed_list: List, optional

            A Python list of additional fixed-file paths to be synced
            in addition to the default values specified within this
            method.

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
            value = getattr(config, fixed_attr_key)
            if value is None:
                msg = (f"The configuration attribute {fixed_attr_key} could not "
                       "be determined from the run-time environment. Aborting!!!"
                       )
                raise GFSError(msg=msg)

            setattr(fixedfiles_dict, fixed_attr_key, value)

        # Define the top-level directory-tree path containing the
        # respective fixed-files.
        fix_dirpath = fixedfiles_dict.FIXgfs
        if fix_dirpath is None:
            msg = ("The attribute FIXgfs could not be determined from the "
                   "run-time environment. Aborting!!!"
                   )
            raise GFSError(msg=msg)

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
                           "FIX_lut": "lut"
                           }

        # Check whether the run-time environment contains additional
        # fixed-files to be synced; proceed accordingly.
        if fixed_list is not None:
            for item in fixed_list:
                dirtree_path = getattr(config, item)
                if (not os.path.isdir(dirtree_path)) or (not os.path.exists(dirpath_path)):
                    msg = (f"The directory tree path {dirtree_path} is either not "
                           "a directory does not exist; this may cause unexpected results "
                           "or failures."
                           )
                    logger.warning(msg=msg)

        for (fixed_attr_key, fixed_attr_value) in fixed_attr_dict.items():
            dirtree_path = os.path.join(fix_dirpath, fixed_attr_value)
            if (not os.path.isdir(dirtree_path)) or (not os.path.exists(dirpath_path)):
                msg = (f"The directory tree path {dirtree_path} is either not a directory "
                       "or does not exist. Aborting!!!"
                       )
                raise GFSError(msg=msg)

            setattr(fixedfiles_dict, fixed_attr_key, dirtree_path)

        return fixedfiles_dict

    def initialize(self: Forecast):
        """

        """

        # Define the base-class method attributes.
        super().initialize()

        # Sync the fixed files accordingly.
        fixed_list = self.get_fixedfiles_info(app=self.gfs_app)
        fixedfiles_dict = self.__fixedfiles(config=self.config, fixed_path=self.fixed_path,
                                            logger=self.logger, fixed_list=fixed_list)

        # NEED TO READ YAMLS SOMEHOW.
