#!/usr/bin/env python3

from typing import Dict
from wxflow import (Task,
                    FileHandler)

logger = getLogger(__name__.split('.')[-1])


class OfflineAnalysis(Task):
    """
    Class for tasks to compute analysis increments from
    an offline analysis and previous forecast
    """
    @logit(logger, name="SnowAnalysis")
    def __init__(self, config: Dict[str, Any]):
        """Constructor global offline analysis task

        This method will construct a global offline analysis task.
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task

        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration

        Returns
        ----------
        None
        """
        super().__init__(config)

        _res = int(self.task_config['CASE'][1:])

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global offline atmospheric analysis

        This method will initialize a global offline atmospheric analysis.
        This includes:
        - Creating working directories
        - Staging input files
        - Generating namelists from templates
        - Creating output directories if necessary

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

    @logit(logger)
    def interpolate_analysis(self) -> None:
        """If necessary, nterpolate the offline analysis
        from its original resolution to the resolution of the
        previous model forecast.

        Parameters
        ----------
        self : OfflineAnalysis
            Instance of the OfflineAnalysis object
        """

    @logit(logger)
    def calc_increment(self) -> None:
        """Compute the analysis increment for input to the forecast model
        by subtracting the previous model forecast from the provided analysis.

        Parameters
        ----------
        self : OfflineAnalysis
            Instance of the OfflineAnalysis object
        """

    @logit(logger)
    def finalize(self) -> None:
        """Performs closing actions of the offline analysis task
        This method:
        - copies the analysis files to the COM/
        - copies the increment files to the COM/

        Parameters
        ----------
        self : OfflineAnalysis
            Instance of the OfflineAnalysis object
        """
