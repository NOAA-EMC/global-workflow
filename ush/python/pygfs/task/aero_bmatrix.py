#!/usr/bin/env python3

import os
from logging import getLogger
from typing import List, Dict

from wxflow import (AttrDict, FileHandler,
                    add_to_datetime, to_timedelta,
                    parse_j2yaml, logit, Task)
from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class AerosolBMatrix(Task):
    """
    Class for global aerosol BMatrix tasks
    """
    @logit(logger, name="AerosolBMatrix")
    def __init__(self, config):
        """Constructor global aero analysis bmatrix task

        This method will construct a global aero bmatrix task object.
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

        _res = int(self.task_config['CASE'][1:])
        _res_anl = int(self.task_config['CASE_ANL'][1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config['assim_freq']}H") / 2)

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.task_config['LEVS'] - 1,
                'AERO_WINDOW_BEGIN': _window_begin,
                'AERO_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'aero_bkg_fhr': map(int, str(self.task_config['aero_bkg_times']).split(',')),
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'aero_obsdatain_path': f"{self.task_config.DATA}/obs/",
                'aero_obsdataout_path': f"{self.task_config.DATA}/diags/",
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Create dictionary of Jedi objects
        expected_keys = ['aero_interpbkg', 'aero_diagb', 'aero_diffusion']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self: Task) -> None:
        """Initialize a global aerosol B-matrix

        This method will initialize a global aerosol B-Matrix.
        This includes:
        - staging the determinstic backgrounds
        - staging fix files
        - initializing the JEDI applications

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # stage fix files
        logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
        jedi_fix_list = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage backgrounds
        logger.info(f"Staging backgrounds prescribed from {self.task_config.AERO_BMATRIX_STAGE_TMPL}")
        aero_bmat_stage_list = parse_j2yaml(self.task_config.AERO_BMATRIX_STAGE_TMPL, self.task_config)
        FileHandler(aero_bmat_stage_list).sync()

        # initialize JEDI applications
        self.jedi_dict['aero_interpbkg'].initialize(self.task_config)
        self.jedi_dict['aero_diagb'].initialize(self.task_config)
        self.jedi_dict['aero_diffusion'].initialize(self.task_config)

    @logit(logger)
    def execute(self) -> None:
        """Generate the full B-matrix

        This method will generate the full B-matrix according to the configuration.
        This includes:
        - running all JEDI applications required to generate the B-matrix

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # interpolate backgrounds to analysis resolution
        self.jedi_dict['aero_interpbkg'].execute()

        # variance partitioning
        self.jedi_dict['aero_diagb'].execute()

        # diffusion
        self.jedi_dict['aero_diffusion'].execute()

    @logit(logger)
    def finalize(self) -> None:
        """Finalize a global aerosol bmatrix

        This method will finalize a global aerosol bmatrix using JEDI.
        This includes:
        - copying the bmatrix files to COM
        - copying YAMLs to COM

        """
        # save files to COMOUT
        logger.info(f"Saving files to COMOUT based on {self.task_config.AERO_BMATRIX_FINALIZE_TMPL}")
        aero_bmat_finalize_list = parse_j2yaml(self.task_config.AERO_BMATRIX_FINALIZE_TMPL, self.task_config)
        FileHandler(aero_bmat_finalize_list).sync()
