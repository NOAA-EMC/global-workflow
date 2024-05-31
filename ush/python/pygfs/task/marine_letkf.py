#!/usr/bin/env python3

from datetime import timedelta
import f90nml
from glob import glob
from logging import getLogger
from os import path
from pygfs.task.analysis import Analysis
from soca import bkg_utils
from typing import Dict
import ufsda
from wxflow import (AttrDict,
                    Executable,
                    FileHandler,
                    logit,
                    parse_j2yaml,
                    WorkflowException,
                    YAMLFile)

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

        _half_assim_freq = timedelta(hours=int(self.config.assim_freq) / 2)
        _window_begin = self.runtime_config.current_cycle - _half_assim_freq
        _window_begin_iso = _window_begin.strftime('%Y-%m-%dT%H:%M:%SZ')
        _window_middle_iso = self.runtime_config.current_cycle.strftime('%Y-%m-%dT%H:%M:%SZ')
        _letkf_yaml_file = 'letkf.yaml'
        _letkf_exec_args = [self.config.MARINE_LETKF_EXEC,
                            'fv3jedi',
                            'localensembleda',
                            _letkf_yaml_file]

        local_dict = AttrDict(
            {
                'ATM_WINDOW_BEGIN': _window_begin_iso,
                'ATM_WINDOW_MIDDLE': _window_middle_iso,
                'window_begin': _window_begin,
                'letkf_exec_args': _letkf_exec_args,
                'letkf_yaml_file': _letkf_yaml_file,
                'mom_input_nml_tmpl':  path.join(self.runtime_config.DATA, 'mom_input.nml.tmpl'),
                'mom_input_nml': path.join(self.runtime_config.DATA, 'mom_input.nml'),
                'obs_dir': path.join(self.runtime_config.DATA, 'obs')
            }
        )

        self.task_config = AttrDict(dict(**self.config, **self.runtime_config, **local_dict))

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
        letkf_stage_list = parse_j2yaml(self.task_config.MARINE_LETKF_STAGE_YAML_TMPL, self.task_config)
        FileHandler(letkf_stage_list).sync()

        # TODO(AFE): probably needs to be jinjafied
        obs_list = YAMLFile(self.task_config.OBS_YAML)

        # get the list of observations
        CDATE = self.runtime_config.current_cycle.strftime("%Y%m%d%H")
        obs_files = []
        for ob in obs_list['observers']:
            obs_name = ob['obs space']['name'].lower()
            obs_filename = f"{self.task_config.RUN}.t{self.task_config.cyc}z.{obs_name}.{CDATE}.nc4"
            obs_files.append((obs_filename, ob))

        obs_files_to_copy = []
        obs_to_use = []
        # copy obs from COM_OBS to DATA/obs
        for obs_file, ob in obs_files:
            logger.info(f"******* {obs_file}")
            obs_src = path.join(self.task_config.COM_OBS, obs_file)
            obs_dst = path.join(self.task_config.DATA, self.task_config.obs_dir, obs_file)
            logger.info(f"******* {obs_src}")
            if path.exists(obs_src):
                logger.info(f"******* fetching {obs_file}")
                obs_files_to_copy.append([obs_src, obs_dst])
                obs_to_use.append(ob)
            else:
                logger.info(f"******* {obs_file} is not in the database")

        # stage the desired obs files
        FileHandler({'copy': obs_files_to_copy}).sync()

        # make the letkf.yaml
        letkf_yaml = parse_j2yaml(self.task_config.MARINE_LETKF_YAML_TMPL, self.task_config)
        letkf_yaml.observations.observers = obs_to_use
        letkf_yaml.save(self.task_config.letkf_yaml_file)

        # swap date and stack size in mom_input.nml
        domain_stack_size = self.task_config.DOMAIN_STACK_SIZE
        ymdhms = [int(s) for s in self.task_config.window_begin.strftime('%Y,%m,%d,%H,%M,%S').split(',')]
        with open(self.task_config.mom_input_nml_tmpl, 'r') as nml_file:
            nml = f90nml.read(nml_file)
            nml['ocean_solo_nml']['date_init'] = ymdhms
            nml['fms_nml']['domains_stack_size'] = int(domain_stack_size)
            ufsda.disk_utils.removefile(self.task_config.mom_input_nml)
            nml.write(self.task_config.mom_input_nml)

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
