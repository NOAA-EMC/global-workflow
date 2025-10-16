#!/usr/bin/env python3

import f90nml
import pygfs.utils.marine_da_utils as mdau
from logging import getLogger
import os
from pygfs.task.analysis import Analysis
from typing import Dict
from wxflow import (AttrDict, Executable, FileHandler,
                    parse_j2yaml,
                    to_timedelta, to_YMDH
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
                'mom_input_nml_tmpl': os.path.join(self.task_config.DATA, 'mom_input.nml.tmpl')
                'mom_input_nml': os.path.join(self.task_config.DATA, 'mom_input.nml')
                'obs_dir': os.path.join(self.task_config.DATA, 'obs')
                'ENSPERT_RELPATH': _enspert_relpath
                'PARMmarine': os.path.join(self.task_config.PARMgfs, 'gdas', 'marine')
                'app_path_observations': self.task_config.MARINE_JCB_GDAS_OBS
                'letkf_app':  "true"
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
    def execute(self, jedi_dict_key: str) -> None:
        """Execute JEDI application of marine analysis

        Parameters
        ----------
        jedi_dict_key
            key specifying particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        self.jedi_dict[jedi_dict_key].execute()

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

        logger.info("finalize")

        letkfsaveconf = AttrDict()
        keys = ['current_cycle', 'DATA', 'NMEM_ENS', 'WINDOW_BEGIN', 'GDUMP_ENS',
                'PARMgfs', 'ROTDIR', 'COM_OCEAN_LETKF_TMPL', 'COM_ICE_LETKF_TMPL',
                'COMOUT_OCEAN_LETKF', 'COMOUT_ICE_LETKF', 'WINDOW_MIDDLE',
                'OBS_LIST_YAML', 'COMOUT_CONF', 'letkf_yaml_file']
        for key in keys:
            letkfsaveconf[key] = self.task_config[key]

        # get the list of obs output file - letkf yaml is already complete
        letkf_config = parse_j2yaml(self.task_config.letkf_yaml_file, AttrDict())
        obs_files = []
        for observer in letkf_config['observations']['observers']:
            obs_files.append(observer['obs space']['obsdataout']['engine']['obsfile'])
        obs_files_to_copy = []
        # copy files from diags to COMOUT
        for obs_src in obs_files:
            obs_dst = os.path.join(letkfsaveconf.COMOUT_OCEAN_LETKF, 'diags',
                                   os.path.basename(obs_src))
            if os.path.exists(obs_src):
                obs_files_to_copy.append([obs_src, obs_dst])
        FileHandler({'mkdir': [os.path.join(letkfsaveconf.COMOUT_OCEAN_LETKF, 'diags')]}).sync()
        FileHandler({'copy': obs_files_to_copy}).sync()
        # yaml configurations
        yamls_to_copy = []
        yamls_to_copy.append([letkfsaveconf.letkf_yaml_file, os.path.join(letkfsaveconf.COMOUT_CONF, 'soca_letkf.yaml')])
        FileHandler({'copy': yamls_to_copy}).sync()
        save_dict = parse_j2yaml(self.task_config.SAVE_YAML, letkfsaveconf)
        FileHandler(save_dict).sync()
