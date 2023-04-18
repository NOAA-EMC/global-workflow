import copy
import logging

from pygw.logger import logit
from pygfs.ufswm.ufs import UFS

logger = logging.getLogger(__name__.split('.')[-1])


class GFS(UFS):

    @logit(logger, name="GFS")
    def __init__(self, config):

        super().__init__("GFS", config)

        # Start putting fixed properties of the GFS
        self.ntiles = 6

        # Determine coupled/uncoupled from config and define as appropriate
