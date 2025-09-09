#!/usr/bin/env python3

from logging import getLogger
import os
from typing import Dict
import pygfs.utils.marine_da_utils as mdau
from pygfs.jedi import Jedi
from wxflow import (AttrDict, FileHandler, Task,
                    add_to_datetime, to_timedelta, to_fv3time, to_isotime,
                    parse_j2yaml,
                    logit)

logger = getLogger(__name__.split('.')[-1])


class MarineRecenter(Task):
    """
    Class for global ocean analysis recentering task
    """

    @logit(logger, name="MarineRecenter")
    def __init__(self, config: Dict) -> None:
        """Constructor for ocean recentering task
        Parameters:
        ------------
        config: Dict
            configuration, namely evironment variables
        Returns:
        --------
        None
        """

        super().__init__(config)

        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)
        _window_end = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"{self.task_config.assim_freq}H") / 2)
        _enspert_relpath = os.path.relpath(self.task_config.DATAens, self.task_config.DATA)
        if self.task_config.DOIAU:
            # forecast initialized at the begining of the DA window
            _cice_rst_date = to_fv3time(_window_begin)
        else:
            # forecast initialized at the middle of the DA window
            _cice_rst_date = to_fv3time(self.task_config.current_cycle)

        local_dict = AttrDict(
            {
                'PARMmarine': os.path.join(self.task_config.PARMgfs, 'gdas', 'marine'),
                'MARINE_WINDOW_BEGIN': _window_begin,
                'MARINE_WINDOW_END': _window_end,
                'MARINE_WINDOW_MIDDLE': self.task_config.current_cycle,
                'MARINE_WINDOW_BEGIN_ISO': to_isotime(_window_begin),
                'MARINE_WINDOW_END_ISO': to_isotime(_window_end),
                'MARINE_WINDOW_MIDDLE_ISO': to_isotime(self.task_config.current_cycle),
                'MARINE_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'ENSPERT_RELPATH': _enspert_relpath,
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'cice_rst_date': _cice_rst_date
            }
        )

        # Extend task_config with local_dict
        self.task_config.update(local_dict)

        # Construct dictionary of JEDI objects, one for each JEDI application need for the analysis
        expected_keys = ['gridgen', 'ens_handler']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self):
        """Method initialize for ocean recentering task
        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        # stage fix files
        logger.info(f"Staging SOCA fix files from {self.task_config.INPUT_FIX_DIR}")
        soca_fix_list = parse_j2yaml(self.task_config.STAGE_FIX_YAML, self.task_config)
        FileHandler(soca_fix_list).sync()

        # prepare the MOM6 input.nml
        mdau.prep_input_nml(self.task_config)

        # stage the soca utility yamls (gridgen, fields and ufo mapping yamls)
        logger.info(f"Staging SOCA utility yaml files from {self.task_config.PARMmarine}")
        soca_utility_list = parse_j2yaml(self.task_config.STAGE_UTILITIES_YAML, self.task_config)
        FileHandler(soca_utility_list).sync()

        # stage backgrounds
        bkg_list = parse_j2yaml(self.task_config.STAGE_DET_BKG_YAML, self.task_config)
        FileHandler(bkg_list).sync()

        # stage the ensemble members and CICE restarts
        logger.info("---------------- Stage ensemble members and CICE restarts")
        stage_dict = parse_j2yaml(self.task_config.STAGE_YAML, self.task_config)
        FileHandler(stage_dict).sync()

        # initialize JEDI applications
        logger.info(f"Initializing SOCA gridgen application")
        self.jedi_dict['gridgen'].initialize(self.task_config)

        logger.info(f"Initializing SOCA ensemble handler")
        self.jedi_dict['ens_handler'].initialize(self.task_config)

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
        """Method finalize for ocean recentering task
        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        # Save recentered increments and ensemble statistics
        logger.info("---------------- Save recentered increments and ensemble statistics")
        save_dict = parse_j2yaml(self.task_config.SAVE_YAML, self.task_config)
        FileHandler(save_dict).sync()
