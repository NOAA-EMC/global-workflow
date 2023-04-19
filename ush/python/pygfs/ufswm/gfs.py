import os
import logging

from pygw.attrdict import AttrDict
from pygw.yaml_file import parse_yamltmpl
from pygw.logger import logit
from pygfs.ufswm.ufs import UFS

logger = logging.getLogger(__name__.split('.')[-1])


class GFS(UFS):

    @logit(logger, name="GFS")
    def __init__(self, config):

        super().__init__("GFS", config)

        # Start putting fixed properties of the GFS
        self.ntiles = 6

        # Get and read the information regarding configuration of the UFS for this instance
        self.ufs_config_yaml = self._config.UFS_CONFIG_FILE
        self.ufs_config_dict = parse_yamltmpl(self.ufs_config_yaml)

        # Set the paths to the various fix directories
        self.ufs_fix = self.set_ufs_fix(
            os.path.join(self._config.HOMEgfs, "fix"))

        # Determine coupled/uncoupled from config and define as appropriate
        self.ufs_config = self.set_ufs_config()

    @logit(logger)
    def prepare_DATA(self) -> None:
        """
        Prepare the DATA directory for the GFS
        Reads the `stage` section of UFS_CONFIG_FILE and creates the necessary directories
        """

        localconf = AttrDict()
        localconf.DATA = self._config.DATA

        data = parse_yamltmpl(self.ufs_config_yaml, localconf)
        self.stage(data.stage)
        self.mdl_config()

    @logit(logger)
    def stage_fix(self) -> None:

        localconf = AttrDict()
        localconf.HOMEgfs = self._config.HOMEgfs
        localconf.DATA = self._config.DATA

        # All the various fix directories
        for key, value in self.ufs_fix.items():
            localconf[key] = value

        # various resolution stuff that is needed
        localconf.atm_res = self.ufs_config.atm_res
        localconf.ocn_res = self.ufs_config.ocn_res

        data = parse_yamltmpl(self.ufs_config_yaml, localconf)
        self.stage(data.fix)
