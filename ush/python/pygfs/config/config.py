

# ----

from dataclasses import dataclass

from pygw.logger import Logger, logit

# ----

base_logger = Logger(level="info", colored_log=True)

# ----


@dataclass
class Config:
    """ 

    """

    @logit(base_logger)
    def __init__(self):
        """
        Description
        -----------

        Creates a new Config object.

        """

        # Define the base-class attributes.
        self.model_config = AttrDict()
