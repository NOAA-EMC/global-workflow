import copy
import logging

from pygw.logger import logit
from pygfs.ufswm.ufs import UFS

logger = logging.getLogger(__name__.split('.')[-1])


class GFS(UFS):

    @logit(logger, name="GFS")
    def __init__(self, config):

        # Note there is no super() here needed since UFS does not initialize for all configurations equally.  If this need comes along, we can adjust.

        # Make a deep copy of incoming config for caching purposes. _config should not be updated
        self._config = copy.deepcopy(config)

        # Start putting fixed properties of the GFS
        self.ntiles = 6

        # Determine coupled/uncoupled from config and define as appropriate

