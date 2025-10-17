#!/usr/bin/env python3

import f90nml
import pygfs.utils.marine_da_utils as mdau
from logging import getLogger
import os
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
                'mom_input_nml_tmpl': os.path.join(self.task_config.DATA, 'mom_input.nml.tmpl'),
                'mom_input_nml': os.path.join(self.task_config.DATA, 'mom_input.nml'),
                'obs_dir': os.path.join(self.task_config.DATA, 'obs'),
                'ENSPERT_RELPATH': _enspert_relpath,
                'PARMmarine': os.path.join(self.task_config.PARMgfs, 'gdas', 'marine'),
                'app_path_observations': self.task_config.MARINE_JCB_GDAS_OBS,
                'letkf_app':  'true',
                'DIST_HALO_SIZE': 3500000,
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

        # initialize JEDI applications
        logger.info(f"Initializing JEDI applications")
        self.jedi_dict['gridgen'].initialize(self.task_config)
        self.jedi_dict['letkf'].initialize(self.task_config, clean_empty_obsspaces=True)

        # TODO(AFE) get rid of this, I think
        # swap date and stack size in mom_input.nml
        domain_stack_size = self.task_config.DOMAIN_STACK_SIZE
        ymdhms = [int(s) for s in self.task_config.WINDOW_BEGIN.strftime('%Y,%m,%d,%H,%M,%S').split(',')]
        with open(self.task_config.mom_input_nml_tmpl, 'r') as nml_file:
            nml = f90nml.read(nml_file)
            nml['ocean_solo_nml']['date_init'] = ymdhms
            nml['fms_nml']['domains_stack_size'] = int(domain_stack_size)
            nml.write(self.task_config.mom_input_nml, force=True)  # force to overwrite if necessary

    @logit(logger)
    def execute(self) -> None:
        """Execute JEDI application of marine analysis

        Parameters
        ----------
        jedi_dict_key
            key specifying particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        # Temporary fix to add halo distribution to all obs spaces
        for observer in self.jedi_dict['letkf'].jedi_config.input_config['observations']['observers']:
            if 'distribution' not in observer['obs space']:
                observer['obs space']['distribution'] = {'name': 'Halo', 'halo size': self.task_config['DIST_HALO_SIZE']}
        save_as_yaml(self.jedi_dict['letkf'].jedi_config.input_config, self.jedi_dict['letkf'].jedi_config.yaml)

        self.jedi_dict['gridgen'].execute()
        self.jedi_dict['letkf'].execute()

    @logit(logger)
    def finalize(self):
        """Method finalize for ocean and sea ice LETKF task
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
