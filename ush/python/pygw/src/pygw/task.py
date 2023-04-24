import logging
from typing import Dict

from pygw.attrdict import AttrDict
from pygw.timetools import add_to_datetime, to_timedelta

logger = logging.getLogger(__name__.split('.')[-1])


class Task:
    """
    Base class for all tasks
    """

    def __init__(self, config: Dict, *args, **kwargs):
        """
        Every task needs a config.
        Additional arguments (or key-value arguments) can be provided.

        Parameters
        ----------
        config : Dict
                 dictionary object containing task configuration

        *args : tuple
                Additional arguments to `Task`

        **kwargs : dict, optional
                   Extra keyword arguments to `Task`
        """

        # Store the config and arguments as attributes of the object
        self.config = AttrDict(config)

        for arg in args:
            setattr(self, str(arg), arg)

        for key, value in kwargs.items():
            setattr(self, key, value)

        # Pull out basic runtime keys values from config into its own runtime config
        self.runtime_config = AttrDict()
        runtime_keys = ['PDY', 'cyc', 'DATA', 'RUN', 'CDUMP']  # TODO: eliminate CDUMP and use RUN instead
        for kk in runtime_keys:
            try:
                self.runtime_config[kk] = config[kk]
                logger.debug(f'Deleting runtime_key {kk} from config')
                del self.config[kk]
            except KeyError:
                raise KeyError(f"Encountered an unreferenced runtime_key {kk} in 'config'")

        # Any other composite runtime variables that may be needed for the duration of the task
        # can be constructed here

        # Construct the current cycle datetime object
        self.runtime_config['current_cycle'] = add_to_datetime(self.runtime_config['PDY'], to_timedelta(f"{self.runtime_config.cyc}H"))
        logger.debug(f"current cycle: {self.runtime_config['current_cycle']}")

        # Construct the previous cycle datetime object
        cadence = self.config.get('assim_freq', 0)
        self.runtime_config['previous_cycle'] = add_to_datetime(self.runtime_config.current_cycle, -to_timedelta(f"{cadence}H"))
        logger.debug(f"previous cycle: {self.runtime_config['previous_cycle']}")

        pass

    def initialize(self):
        """
        Initialize methods for a task
        """
        pass

    def configure(self):
        """
        Configuration methods for a task in preparation for execution
        """
        pass

    def execute(self):
        """
        Execute methods for a task
        """
        pass

    def finalize(self):
        """
        Methods for after the execution that produces output task
        """
        pass

    def clean(self):
        """
        Methods to clean after execution and finalization prior to closing out a task
        """
        pass
