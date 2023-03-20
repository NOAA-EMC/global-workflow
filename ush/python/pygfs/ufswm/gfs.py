
# ----

from pygfs.ufswm.ufswm import UFSWM, base_logger
from pygfs.exceptions import GFSForecastError

from pygw.configuration import cast_strdict_as_dtypedict
from pygw.logger import logit

# ----


# ----

class GFS(UFSWM):
    """
    Description
    -----------

    Parameters
    ----------

    """

    @logit(base_logger, name="GFS")
    def __init__(self: UFSWM, config: object):
        """
        Description
        -----------

        Creates a new GFS object.

        """

        # Define the base-class attributes.
        super().__init__(config=config, model="GFS")

    @logit(base_logger)
    def initialize(self: UFSWM) -> None:
        """ 
        Description
        -----------

        Parameters
        ----------



        """

        super().initialize()

        # Link the fixed files to the application directory.
        self.get_fixed_files()
