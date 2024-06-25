#!/usr/bin/env python3

from datetime import timedelta
import f90nml
from logging import getLogger
from os import path
from pygfs.task.analysis import Analysis
from typing import Dict
from wxflow import (AttrDict,
                    datetime_to_YMDH,
                    FileHandler,
                    logit,
                    parse_j2yaml,
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

        _half_assim_freq = timedelta(hours=int(self.task_config.assim_freq) / 2)
        _letkf_yaml_file = 'letkf.yaml'
        _letkf_exec_args = [self.task_config.OCEAN_LETKF_EXEC,
                            'fv3jedi',
                            'localensembleda',
                            _letkf_yaml_file]

        self.task_config.WINDOW_MIDDLE = self.task_config.current_cycle
        self.task_config.WINDOW_BEGIN = self.task_config.current_cycle - _half_assim_freq
        self.task_config.letkf_exec_args = _letkf_exec_args
        self.task_config.letkf_yaml_file = _letkf_yaml_file
        self.task_config.mom_input_nml_tmpl = path.join(self.task_config.DATA, 'mom_input.nml.tmpl')
        self.task_config.mom_input_nml = path.join(self.task_config.DATA, 'mom_input.nml')
        self.task_config.obs_dir = path.join(self.task_config.DATA, 'obs')

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
        letkf_stage_list = parse_j2yaml(self.task_config.OCEAN_LETKF_STAGE_YAML_TMPL, self.task_config)
        FileHandler(letkf_stage_list).sync()
        letkf_stage_fix_list = parse_j2yaml(self.task_config.SOCA_FIX_STAGE_YAML_TMPL, self.task_config)
        FileHandler(letkf_stage_fix_list).sync()

        # TODO(AFE): probably needs to be jinjafied
        obs_list = YAMLFile(self.task_config.OBS_YAML)

        # get the list of observations
        CDATE = datetime_to_YMDH(self. task_config.current_cycle)
        obs_files = []
        for ob in obs_list['observers']:
            obs_name = ob['obs space']['name'].lower()
            obs_filename = f"{self.task_config.RUN}.t{self.task_config.cyc}z.{obs_name}.{CDATE}.nc4"
            obs_files.append((obs_filename, ob))

        obs_files_to_copy = []
        obs_to_use = []
        # copy obs from COMIN_OBS to DATA/obs
        for obs_file, ob in obs_files:
            obs_src = path.join(self.task_config.COMIN_OBS, obs_file)
            obs_dst = path.join(self.task_config.DATA, self.task_config.obs_dir, obs_file)
            if path.exists(obs_src):
                logger.info(f"will try to stage {obs_file}")
                obs_files_to_copy.append([obs_src, obs_dst])
                obs_to_use.append(ob)
            else:
                logger.info(f"{obs_file} is not available in {self.task_config.COMIN_OBS}")

        # stage the desired obs files
        FileHandler({'copy': obs_files_to_copy}).sync()

        # make the letkf.yaml
        letkf_yaml = parse_j2yaml(self.task_config.OCEAN_LETKF_YAML_TMPL, self.task_config)
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
