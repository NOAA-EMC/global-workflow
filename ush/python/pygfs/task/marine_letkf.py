#!/usr/bin/env python3

import f90nml
from logging import getLogger
import os
from pygfs.task.analysis import Analysis
from typing import Dict
from wxflow import (AttrDict,
                    FileHandler,
                    logit,
                    parse_j2yaml,
                    to_timedelta,
                    to_YMDH)

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

        _half_assim_freq = to_timedelta(f"{self.task_config.assim_freq}H") / 2
        _letkf_yaml_file = 'letkf.yaml'
        _letkf_exec_args = [self.task_config.MARINE_LETKF_EXEC,
                            'soca',
                            'localensembleda',
                            _letkf_yaml_file]

        self.task_config.WINDOW_MIDDLE = self.task_config.current_cycle
        self.task_config.WINDOW_BEGIN = self.task_config.current_cycle - _half_assim_freq
        self.task_config.letkf_exec_args = _letkf_exec_args
        self.task_config.letkf_yaml_file = _letkf_yaml_file
        self.task_config.mom_input_nml_tmpl = os.path.join(self.task_config.DATA, 'mom_input.nml.tmpl')
        self.task_config.mom_input_nml = os.path.join(self.task_config.DATA, 'mom_input.nml')
        self.task_config.obs_dir = os.path.join(self.task_config.DATA, 'obs')

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

        logger.info("initialize")

        # make directories and stage ensemble background files
        ensbkgconf = AttrDict()
        keys = ['previous_cycle', 'current_cycle', 'DATA', 'NMEM_ENS',
                'PARMgfs', 'ROTDIR', 'COM_OCEAN_HISTORY_TMPL', 'COM_ICE_HISTORY_TMPL']
        for key in keys:
            ensbkgconf[key] = self.task_config[key]
        ensbkgconf.RUN = 'enkfgdas'
        soca_ens_bkg_stage_list = parse_j2yaml(self.task_config.SOCA_ENS_BKG_STAGE_YAML_TMPL, ensbkgconf)
        FileHandler(soca_ens_bkg_stage_list).sync()
        soca_fix_stage_list = parse_j2yaml(self.task_config.SOCA_FIX_STAGE_YAML_TMPL, self.task_config)
        FileHandler(soca_fix_stage_list).sync()
        letkf_stage_list = parse_j2yaml(self.task_config.MARINE_LETKF_STAGE_YAML_TMPL, self.task_config)
        FileHandler(letkf_stage_list).sync()

        obs_list = parse_j2yaml(self.task_config.OBS_YAML, self.task_config)

        # get the list of observations
        obs_files = []
        for ob in obs_list['observers']:
            obs_name = ob['obs space']['name'].lower()
            obs_filename = f"{self.task_config.RUN}.t{self.task_config.cyc}z.{obs_name}.{to_YMDH(self.task_config.current_cycle)}.nc"
            obs_files.append((obs_filename, ob))

        obs_files_to_copy = []
        obs_to_use = []
        # copy obs from COMIN_OBS to DATA/obs
        for obs_file, ob in obs_files:
            obs_src = os.path.join(self.task_config.COMIN_OBS, obs_file)
            obs_dst = os.path.join(self.task_config.DATA, self.task_config.obs_dir, obs_file)
            if os.path.exists(obs_src):
                obs_files_to_copy.append([obs_src, obs_dst])
                obs_to_use.append(ob)
            else:
                logger.warning(f"{obs_file} is not available in {self.task_config.COMIN_OBS}")

        # stage the desired obs files
        FileHandler({'copy': obs_files_to_copy}).sync()

        # make the letkf.yaml
        letkfconf = AttrDict()
        keys = ['WINDOW_BEGIN', 'WINDOW_MIDDLE', 'RUN', 'gcyc', 'NMEM_ENS']
        for key in keys:
            letkfconf[key] = self.task_config[key]
        letkfconf.RUN = 'enkfgdas'
        letkf_yaml = parse_j2yaml(self.task_config.MARINE_LETKF_YAML_TMPL, letkfconf)
        letkf_yaml.observations.observers = obs_to_use
        letkf_yaml.save(self.task_config.letkf_yaml_file)

        # swap date and stack size in mom_input.nml
        domain_stack_size = self.task_config.DOMAIN_STACK_SIZE
        ymdhms = [int(s) for s in self.task_config.WINDOW_BEGIN.strftime('%Y,%m,%d,%H,%M,%S').split(',')]
        with open(self.task_config.mom_input_nml_tmpl, 'r') as nml_file:
            nml = f90nml.read(nml_file)
            nml['ocean_solo_nml']['date_init'] = ymdhms
            nml['fms_nml']['domains_stack_size'] = int(domain_stack_size)
            nml.write(self.task_config.mom_input_nml, force=True)  # force to overwrite if necessary

    @logit(logger)
    def run(self):
        """Method run for ocean and sea ice LETKF task
        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        logger.info("run")

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
