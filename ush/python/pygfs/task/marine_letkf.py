#!/usr/bin/env python3

import os
import pygfs.utils.marine_da_utils as mdau
from logging import getLogger
from pygfs.task.analysis import Analysis
from pygfs.jedi import Jedi
from typing import Dict
from wxflow import (AttrDict, Executable, FileHandler,
                    parse_j2yaml, save_as_yaml,
                    to_timedelta, to_YMDH,
                    logit)

logger = getLogger(__name__.split('.')[-1])


class MarineLETKF(Analysis):
    """
    Class for global ocean and sea ice analysis LETKF task
    """

    @logit(logger, name="MarineLETKF")
    def __init__(self, config: Dict) -> None:
        """Constructor for ocean and sea ice LETKF task

        This method will construct a marine analysis task
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task
        - loading the task configuration YAML
        - instantiating the dictionary of Jedi objects for each JEDI application

        Parameters:
        ------------
        config: Dict
            configuration, namely evironment variables
        Returns:
        --------
        None
        """

        logger.info("init")
        super().__init__(config)

        # compute the relative path from self.task_config.DATA to self.task_config.DATAenspert
        _enspert_relpath = os.path.relpath(self.task_config.DATAens, self.task_config.DATA)

        # Create a local dictionary that is repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'PARMmarine': os.path.join(self.task_config.PARMgfs, 'gdas', 'marine'),
                'ENSPERT_RELPATH': _enspert_relpath,
                'letkf_app': 'true',
                'DIST_HALO_SIZE': 3500000,
                'DOMAIN_STACK_SIZE': 116640000,  # TODO: Make the stack size resolution dependent
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Construct dictionary of JEDI objects, one for each JEDI application need for the analysis
        expected_keys = ['gridgen', 'letkf']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self):
        """Method initialize for ocean and sea ice LETKF task

        This method will initialize the marine analysis.
        This includes:
        - staging input files from COM and create output directories
        - preparing the namelist for MOM6
        - initializing all the JEDI applications required for marine LETKF

        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        # stage files from COM
        logger.info(f"Staging files from COM and creating input/output directories")
        FileHandler(self.task_config.data_in).sync()

        # prepare the ensemble MOM6 input.nml
        logger.info(f"Preparing ensemble MOM6 input namelist")
        mdau.prep_input_nml(self.task_config)

        # initialize JEDI applications
        logger.info(f"Initializing JEDI applications")
        self.jedi_dict['gridgen'].initialize(self.task_config)
        self.jedi_dict['letkf'].initialize(self.task_config, clean_empty_obsspaces=True)

    @logit(logger)
    def execute(self) -> None:
        """Execute JEDI application of marine analysis

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        self.jedi_dict['gridgen'].execute()
        self.jedi_dict['letkf'].execute()

    @logit(logger)
    def finalize(self):
        """Method finalize for ocean and sea ice LETKF task

        This method will finalize a global marine analysis.
        This includes:
        - Saving output files to COM

        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        # Save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()
