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

        self.config.gcyc = self.runtime_config.previous_cycle.strftime('%H')
        RUN = self.runtime_config.RUN
        DATA = path.realpath(self.runtime_config.DATA)

        gdas_home = path.join(self.config.HOMEgfs, 'sorc', 'gdas.cd')

        half_assim_freq = timedelta(hours=int(self.config.assim_freq) / 2)
        window_begin = self.runtime_config.current_cycle - half_assim_freq
        window_begin_iso = window_begin.strftime('%Y-%m-%dT%H:%M:%SZ')
        window_middle_iso = self.runtime_config.current_cycle.strftime('%Y-%m-%dT%H:%M:%SZ')
        self.config.ATM_WINDOW_BEGIN = window_begin_iso
        self.config.ATM_WINDOW_MIDDLE = window_middle_iso

        letkf_exec = path.join(self.config.JEDI_BIN, 'gdas.x')
        letkf_yaml_dir = path.join(gdas_home, 'parm', 'soca', 'letkf')
        self.config['letkf_yaml_template'] = path.join(letkf_yaml_dir, 'letkf.yaml.j2')
        self.config['letkf_stage_yaml_template'] = path.join(letkf_yaml_dir, 'letkf_stage.yaml.j2')
        letkf_yaml_file = path.join(DATA, 'letkf.yaml')
        self.config.letkf_exec_args = [letkf_exec,
                                       'fv3jedi',
                                       'localensembleda',
                                       letkf_yaml_file]
        self.config.letkf_yaml_file = letkf_yaml_file

        self.config.window_begin = window_begin
        self.config.BKG_LIST = 'bkg_list.yaml'
        self.config.bkg_dir = path.join(DATA, 'bkg')

        self.config.exec_name_gridgen = path.join(self.config.JEDI_BIN, 'gdas_soca_gridgen.x')
        self.config.gridgen_yaml = path.join(gdas_home, 'parm', 'soca', 'gridgen', 'gridgen.yaml')

        self.config.mom_input_nml_src = path.join(gdas_home, 'parm', 'soca', 'fms', 'input.nml')
        self.config.mom_input_nml_tmpl = path.join(DATA, 'mom_input.nml.tmpl')
        self.config.mom_input_nml = path.join(DATA, 'mom_input.nml')

        self.config.data_output_dir = path.join(DATA, 'data_output')
        self.config.ens_dir = path.join(DATA, 'ens')
        self.config.obs_dir = path.join(DATA, 'obs')

        # set up lists of files for ens background
        self.config.ocn_ens_bkg_filename = f"enkf{RUN}.ocean.t{self.config.gcyc}z.inst.f009.nc"
        self.config.ice_ens_bkg_filename = f"enkf{RUN}.ice.t{self.config.gcyc}z.inst.f009.nc"

        self.task_config = AttrDict(dict(**self.config, **self.runtime_config))


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
        letkf_stage_list = parse_j2yaml(self.task_config.letkf_stage_yaml_template, self.task_config)
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

        FileHandler({'copy': obs_files_to_copy}).sync()

        # make the letkf.yaml
        letkf_yaml = parse_j2yaml(self.task_config.letkf_yaml_template, self.task_config)
        letkf_yaml.observations.observers = obs_to_use
        letkf_yaml.save(self.task_config.letkf_yaml_file)

        FileHandler({'copy': [[self.task_config.mom_input_nml_src,
                               self.task_config.mom_input_nml_tmpl]]}).sync()

        # swap date and stack size
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

        exec_cmd_gridgen = Executable(self.task_config.APRUN_OCNANALLETKF)
        exec_cmd_gridgen.add_default_arg(self.task_config.exec_name_gridgen)
        exec_cmd_gridgen.add_default_arg(self.task_config.gridgen_yaml)

        try:
            logger.debug(f"Executing {exec_cmd_gridgen}")
            exec_cmd_gridgen()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd_gridgen}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd_gridgen}")
        pass

        exec_cmd_letkf = Executable(self.task_config.APRUN_OCNANALLETKF)
        for letkf_exec_arg in self.task_config.letkf_exec_args:
            exec_cmd_letkf.add_default_arg(letkf_exec_arg)

        try:
            logger.debug(f"Executing {exec_cmd_letkf}")
            exec_cmd_letkf()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd_letkf}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd_letkf}")
        pass

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
