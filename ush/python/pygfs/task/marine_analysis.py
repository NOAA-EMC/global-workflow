#!/usr/bin/env python3

import os
import glob
from logging import getLogger
import pygfs.utils.marine_da_utils as mdau

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_timedelta,
                    chdir,
                    parse_j2yaml,
                    logit,
                    Executable,
                    Task)

logger = getLogger(__name__.split('.')[-1])


class MarineAnalysis(Task):
    """
    Class for global marine analysis tasks
    """
    @logit(logger, name="MarineAnalysis")
    def __init__(self, config):
        super().__init__(config)
        _home_gdas = os.path.join(self.task_config.HOMEgfs, 'sorc', 'gdas.cd')
        _calc_scale_exec = os.path.join(self.task_config.HOMEgfs, 'ush', 'soca', 'calc_scales.py')
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)
        _window_end = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # compute the relative path from self.task_config.DATA to self.task_config.DATAenspert
        if self.task_config.NMEM_ENS > 0:
            _enspert_relpath = os.path.relpath(self.task_config.DATAenspert, self.task_config.DATA)
        else:
            _enspert_relpath = None

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'HOMEgdas': _home_gdas,
                'MARINE_WINDOW_BEGIN': _window_begin,
                'MARINE_WINDOW_END': _window_end,
                'MARINE_WINDOW_MIDDLE': self.task_config.current_cycle,
                'BERROR_YAML_DIR': os.path.join(_home_gdas, 'parm', 'soca', 'berror'),
                'UTILITY_YAML_TMPL': os.path.join(_home_gdas, 'parm', 'soca', 'soca_utils_stage.yaml.j2'),
                'MARINE_ENSDA_STAGE_BKG_YAML_TMPL': os.path.join(_home_gdas, 'parm', 'soca', 'ensda', 'stage_ens_mem.yaml.j2'),
                'MARINE_DET_STAGE_BKG_YAML_TMPL': os.path.join(_home_gdas, 'parm', 'soca', 'soca_det_bkg_stage.yaml.j2'),
                'ENSPERT_RELPATH': _enspert_relpath,
                'CALC_SCALE_EXEC': _calc_scale_exec,
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def initialize(self: Task) -> None:
        """Initialize the marine analysis

        This method will initialize the marine analysis.
        This includes:
        - staging the deterministic backgrounds (middle of window)
        - staging SOCA fix files
        - staging static ensemble members (optional)
        - staging ensemble members (optional)
        - generating the YAML files for the JEDI and GDASApp executables
        - creating output directories
        """
        super().initialize()

        # stage fix files
        logger.info(f"Staging SOCA fix files from {self.task_config.SOCA_INPUT_FIX_DIR}")
        soca_fix_list = parse_j2yaml(self.task_config.SOCA_FIX_YAML_TMPL, self.task_config)
        FileHandler(soca_fix_list).sync()

        # prepare the MOM6 input.nml
        mdau.prep_input_nml(self.task_config)

        # stage backgrounds
        # TODO(G): Check ocean backgrounds dates for consistency
        bkg_list = parse_j2yaml(self.task_config.MARINE_DET_STAGE_BKG_YAML_TMPL, self.task_config)
        FileHandler(bkg_list).sync()

        # stage the soca utility yamls (gridgen, fields and ufo mapping yamls)
        logger.info(f"Staging SOCA utility yaml files from {self.task_config.HOMEgfs}/parm/gdas/soca")
        soca_utility_list = parse_j2yaml(self.task_config.UTILITY_YAML_TMPL, self.task_config)
        FileHandler(soca_utility_list).sync()

        # hybrid EnVAR case
        if self.task_config.DOHYBVAR == "YES" or self.task_config.NMEM_ENS > 2:
            # stage ensemble membersfiles for use in hybrid background error
            logger.debug(f"Stage ensemble members for the hybrid background error")
            mdau.stage_ens_mem(self.task_config)

    @logit(logger)
    def variational(self: Task) -> None:
        # link gdas_soca_gridgen.x
        mdau.link_executable(self.task_config, 'gdas.x')
        exec_cmd = Executable(self.task_config.APRUN_MARINEBMAT)
        exec_name = os.path.join(self.task_config.DATA, 'gdas.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('var.yaml')

        mdau.run(exec_cmd)


    @logit(logger)
    def finalize(self: Task) -> None:
        """Finalize the marine analysis job

        This method will finalize the marine analysis job.
        This includes:
        -
        - ...

        """
