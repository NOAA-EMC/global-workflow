import copy
import logging
import os

from pygw.logger import logit
from pygfs.ufswm.ufs import UFS

from pygw.file_utils import FileHandler
from pygw.yaml_file import YAMLFile, parse_yamltmpl


logger = logging.getLogger(__name__.split('.')[-1])

# ----

# Define the valid GFS configuration attributes.
VALID_APP_LIST = ["atm"]

# HRW: Only testing C48 for now.
VALID_ATMRES_LIST = ["c48"]
VALID_ATMLEVS_LIST = [64, 128]

# ----


class GFS(UFS):
    """
    Description
    -----------

    TBD

    Parameters
    ----------

    config: AttrDict

        A Python dictionary containing the run-time configuration
        attributes.

    """

    @logit(logger, name="GFS")
    def __init__(self: UFS, config):
        """
        Description
        -----------

        Creates a new GFS object.

        """

        # Define the base-class attributes.
        super().__init__("GFS", config)

        self.ufs_config.ntiles = 6
        self.config_dirtrees()
        self.config_fcstapp()

    @logit(logger)
    def build_dirtree(self: UFS) -> None:
        """
        Description
        -----------

        This method builds the directory tree and links the relevant
        files.

        """

        fixedfile_list = [item for item in self.ufs_config.fixedfile_list if (
            "atmos" in item.lower() or "land" in item.lower())]

        for fixedfile in fixedfile_list:
            msg = f"Building directory tree and linking files from {fixedfile}."
            logger.info(msg=msg)

            fixedfile_model = parse_yamltmpl(
                path=fixedfile, data=self.ufs_config)
            FileHandler(fixedfile_model.dirtree).sync()
            FileHandler(fixedfile_model.fixedfiles).sync()

    @logit(logger)
    def config_dirtrees(self: UFS) -> None:
        """
        Description
        -----------

        This method defines the directory paths (e.g., trees) for the
        GFS forecast model and updates the base-class object
        `ufs_config`.

        """

        # Define the directory tree attributes and update the
        # base-class object `ufs_config`.
        self.ufs_config.DATA = self._config.DATA

        self.ufs_config.FIX_aer = os.path.join(self._config.FIXgfs, 'aer')
        self.ufs_config.FIX_am = os.path.join(self._config.FIXgfs, 'am')
        self.ufs_config.FIX_lut = os.path.join(self._config.FIXgfs, 'lut')
        self.ufs_config.FIX_orog = os.path.join(self._config.FIXgfs, 'orog')
        self.ufs_config.FIX_ugwd = os.path.join(self._config.FIXgfs, 'ugwd')

        self.ufs_config.HOMEgfs = self._config.HOMEgfs

    @logit(logger)
    def config_fcstapp(self: UFS) -> None:
        """
        Description
        -----------

        This method configures the base-class object `ufs_config` in
        accordance with the respective GFS application.

        """

        # HRW: `atmos_run` will need to be defined dynamically once
        # additional GFS applications are added.
        config_attr_dict = {'aero_run': 'DO_AERO',
                            'app': 'APP',
                            'atm_levs': 'LEVS',
                            'atm_res': "CASE",
                            'chm_cpl': 'cplchm',
                            'ice_cpl': 'cplice',
                            'ice_res': 'ICERES',
                            'ice_run': 'DO_ICE',
                            'ocn_cpl': 'cpl',
                            'ocn_res': 'OCNRES',
                            'ocn_run': 'DO_OCN',
                            'wav_cpl': 'cplwav',
                            'wav_run': 'DO_WAVE',
                            }

        for (config_key, config_value) in config_attr_dict.items():
            self.ufs_config[config_key] = self._config[config_value]

        # Check that the GFS application configuration is valid.
        if self.ufs_config.app.lower() not in VALID_APP_LIST:
            msg = (f"The GFS application {self.ufs_config.app} is not supported; "
                   f"supported GFS applications are {', '.join(VALID_APP_LIST).upper()}."
                   )
            logger.error(msg=msg)

            # NEED TO RAISE AN EXCEPTION HERE.

        if self.ufs_config.atm_res.lower() not in VALID_ATMRES_LIST:
            msg = (f"The GFS cubed-sphere resolution {self.ufs_config.atm_res.upper()} "
                   "is not supported; supported resolutions are "
                   f"{', '.join(VALID_ATMRES_LIST)}."
                   )
            logger.error(msg=msg)

            # NEED TO RAISE AN EXCEPTION HERE.

        if self.ufs_config.atm_levs not in VALID_ATMLEVS_LIST:
            msg = (f"The number of unstaggered vertical levels {self.ufs_config.atm_levs} "
                   "is not supported; valid number of levels are "
                   f"{', '.join(str(atmlev) for atmlev in VALID_ATMLEVS_LIST)}"
                   )
            logger.error(msg=msg)

            # NEED TO RAISE AN EXCEPTION HERE.
