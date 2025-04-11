#!/usr/bin/env python3

import os
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

        # fix ocnres
        self.task_config.OCNRES = f"{self.task_config.OCNRES:03d}"

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npz': self.task_config.LEVS - 1,
            }
        )
        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global offline atmospheric analysis

        This method will initialize a global offline atmospheric analysis.
        This includes:
        - Staging input files
        - Generating namelists from templates
        - copy executables to $DATA

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # stage analysis and forecast files
        logger.info("Copy input files from $COM to $DATA")
        files_to_copy = []
        fcst_file_in = os.path.join(self.task_config.COMIN_ATMOS_HISTORY_PREV,
                                    f"{GPREFIX}atmf006.nc")
        files_to_copy.append([fcst_file_in, os.path.join(self.task_config.DATA, "atmf006.nc")])
        anl_file_in = os.path.join(self.task_config.COMIN_ATMOS_ANALYSIS, f"{GPREFIX}atmanl.nc")
        files_to_copy.append([anl_file_in, os.path.join(self.task_config.DATA, "atmanl.input.nc")])
        sfcanl_file_in = os.path.join(self.task_config.COMIN_ATMOS_ANALYSIS, f"{GPREFIX}sfcanl.nc")
        files_to_copy.append([sfcanl_file_in, os.path.join(self.task_config.DATA, "sfcanl.input.nc")])
        FileHandler({'copy': files_to_copy}).sync()

        # generate namelists for the executables

        # copy executables to $DATA

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
