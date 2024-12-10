#!/usr/bin/env python3

import f90nml
import pygfs.utils.marine_da_utils as mdau
from logging import getLogger
import os
from pygfs.task.analysis import Analysis
from typing import Dict
from wxflow import (AttrDict,
                    Executable,
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
        # compute the relative path from self.task_config.DATA to self.task_config.DATAenspert
        _enspert_relpath = os.path.relpath(self.task_config.DATAens, self.task_config.DATA)

        self.task_config.WINDOW_MIDDLE = self.task_config.current_cycle
        self.task_config.WINDOW_BEGIN = self.task_config.current_cycle - _half_assim_freq
        self.task_config.letkf_exec_args = _letkf_exec_args
        self.task_config.letkf_yaml_file = _letkf_yaml_file
        self.task_config.mom_input_nml_tmpl = os.path.join(self.task_config.DATA, 'mom_input.nml.tmpl')
        self.task_config.mom_input_nml = os.path.join(self.task_config.DATA, 'mom_input.nml')
        self.task_config.obs_dir = os.path.join(self.task_config.DATA, 'obs')
        self.task_config.ENSPERT_RELPATH = _enspert_relpath

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
        soca_fix_stage_list = parse_j2yaml(self.task_config.SOCA_FIX_YAML_TMPL, self.task_config)
        FileHandler(soca_fix_stage_list).sync()
        stageconf = AttrDict()
        keys = ['current_cycle',
                'previous_cycle',
                'COM_ICE_LETKF_TMPL',
                'COM_OCEAN_LETKF_TMPL',
                'COM_ICE_HISTORY_TMPL',
                'COM_OCEAN_HISTORY_TMPL',
                'COMIN_OCEAN_HISTORY_PREV',
                'COMIN_ICE_HISTORY_PREV',
                'COMOUT_ICE_LETKF',
                'COMOUT_OCEAN_LETKF',
                'DATA',
                'ENSPERT_RELPATH',
                'GDUMP_ENS',
                'NMEM_ENS',
                'OPREFIX',
                'PARMgfs',
                'ROTDIR',
                'RUN',
                'WINDOW_BEGIN',
                'WINDOW_MIDDLE']
        for key in keys:
            stageconf[key] = self.task_config[key]

        # stage ensemble background files
        soca_ens_bkg_stage_list = parse_j2yaml(self.task_config.MARINE_ENSDA_STAGE_BKG_YAML_TMPL, stageconf)
        FileHandler(soca_ens_bkg_stage_list).sync()

        # stage letkf-specific files
        letkf_stage_list = parse_j2yaml(self.task_config.MARINE_LETKF_STAGE_YAML_TMPL, stageconf)
        FileHandler(letkf_stage_list).sync()

        obs_list = parse_j2yaml(self.task_config.MARINE_OBS_LIST_YAML, self.task_config)

        # get the list of observations
        obs_files = []
        for ob in obs_list['observers']:
            obs_name = ob['obs space']['name'].lower()
            # TODO(AFE) - this should be removed when the obs config yamls are jinjafied
            if 'distribution' not in ob['obs space']:
                ob['obs space']['distribution'] = {'name': 'Halo', 'halo size': self.task_config['DIST_HALO_SIZE']}
            obs_filename = f"{self.task_config.OPREFIX}{obs_name}.{to_YMDH(self.task_config.current_cycle)}.nc4"
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
        letkf_yaml = parse_j2yaml(self.task_config.MARINE_LETKF_YAML_TMPL, stageconf)
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

        exec_cmd_gridgen = Executable(self.task_config.APRUN_MARINEANLLETKF)
        exec_cmd_gridgen.add_default_arg(self.task_config.GRIDGEN_EXEC)
        exec_cmd_gridgen.add_default_arg(self.task_config.GRIDGEN_YAML)

        mdau.run(exec_cmd_gridgen)

        exec_cmd_letkf = Executable(self.task_config.APRUN_MARINEANLLETKF)
        for letkf_exec_arg in self.task_config.letkf_exec_args:
            exec_cmd_letkf.add_default_arg(letkf_exec_arg)

        mdau.run(exec_cmd_letkf)

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
                'PARMgfs', 'ROTDIR', 'COM_OCEAN_LETKF_TMPL', 'COM_ICE_LETKF_TMPL']
        for key in keys:
            letkfsaveconf[key] = self.task_config[key]
        letkf_save_list = parse_j2yaml(self.task_config.MARINE_LETKF_SAVE_YAML_TMPL, letkfsaveconf)
        FileHandler(letkf_save_list).sync()
