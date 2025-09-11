#!/usr/bin/env python3

from datetime import timedelta
from logging import getLogger
import os
from pprint import pformat
from wxflow import (AttrDict, FileHandler, Task, Executable, Template, TemplateConstants,
                    add_to_datetime, to_timedelta, to_isotime, to_YMD,
                    parse_j2yaml,
                    logit)
from pygfs.task.atm_analysis import AtmAnalysis
from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class EnsembleRecenter(AtmAnalysis):
    """
    Class for JEDI-based ensemble increment recentering
    """
    @logit(logger, name="EnsembleRecenter")
    def __init__(self, config):
        """Constructor for atmospheric ensemble increment recentering task

        This method will construct an ensemble increment recentering task
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task
        - instantiate the Jedi attribute object

        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration

        Returns
        ----------
        None
        """
        super().__init__(config)

        _iau_times_iso = []
        for hour in self.task_config.IAUFHRS:
            _iau_times_iso.append(to_isotime(_window_begin + to_timedelta(f"{str(hour)}H") - to_timedelta(f"{self.task_config.assim_freq}H") / 2))

        # Create a local dictionary that is repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'iau_times_iso': _iau_times_iso
            }
        )

        # Create dictionary of Jedi objects
        expected_keys = ['correction_increment', 'ensemble_recenter']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize the ensemble increment recentering task

        This method will initialize the ensemble increment recentering task.
        This includes:
        - initializing the JEDI recentering application
        - staging JEDI fix files
        - staging backgrounds and increments

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Stage files from COM
        logger.info(f"Staging files from COM")
        FileHandler(self.task_config.stage).sync()
        
        # Initialize JEDI ensemble increment recentering application
        logger.info(f"Initializing JEDI applications")
        self.jedi_dict['correction_increment'].initialize(self.task_config)
        self.jedi_dict['ensemble_recenter'].initialize(self.task_config)

    @logit(logger)
    def execute(self) -> None:
        """Run JEDI executable

        This method will run the JEDI executable for the ensemble increment recentering

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Compute correction increment for ensemble recentering
        self.jedi_dict['correction_increment'].execute()

        # Recenter increments
        self.jedi_dict['ensemble_recenter'].execute()

    @logit(logger)
    def finalize(self) -> None:
        """Finalize the ensemble increment recentering task

        This method will finalize the ensemble increment recentering task.
        This includes:
        - Move correction increment files to the comrot directory

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

         # Save output files to COM
        logger.info(f"Saving output files to COM")
        FileHandler(self.task_config.stage).sync()
