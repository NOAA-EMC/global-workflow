#!/usr/bin/env python3

import os
from logging import getLogger
from typing import List, Dict, Any, Union

from wxflow import (AttrDict,
                    FileHandler)

logger = getLogger(__name__.split('.')[-1])


class AerosolBMatrix(BMatrix):
    """
    Class for global aerosol BMatrix tasks
    """
    @logit(logger, name="AerosolBMatrix")
    def __init__(self, config: Dict[str, Any]) -> None:
        super().__init__(config)

        _res = int(self.config['CASE'][1:])
        _res_anl = int(self.config['CASE_ANL'][1:])        

        _diagb_yaml = os.path.join(self.runtime_config.DATA, f"{self.runtime_config.CDUMP}.t{self.runtime_config['cyc']:02d}z.chem_diagb.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.config.LEVS - 1,
                'npz': self.config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.config['LEVS'] - 1,
                'AERO_WINDOW_BEGIN': _window_begin,
                'AERO_WINDOW_LENGTH': f"PT{self.config['assim_freq']}H",
                'aero_bkg_fhr': map(int, str(self.config['aero_bkg_times']).split(',')),
                'OPREFIX': f"{self.runtime_config.CDUMP}.t{self.runtime_config.cyc:02d}z.",  # TODO: CDUMP is being replaced by RUN
                'APREFIX': f"{self.runtime_config.CDUMP}.t{self.runtime_config.cyc:02d}z.",  # TODO: CDUMP is being replaced by RUN
                'GPREFIX': f"gdas.t{self.runtime_config.previous_cycle.hour:02d}z.",
                'diagb_yaml': _diagb_yaml,
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.config, **self.runtime_config, **local_dict)

    @logit(logger)
    def initialize(self: Analysis) -> None:
        super().initialize()
        # stage fix files
        logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
        jedi_fix_list = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage berror files
        # copy BUMP files, otherwise it will assume ID matrix
        if self.task_config.get('STATICB_TYPE', 'identity') in ['bump']:
            FileHandler(self.get_berror_dict(self.task_config)).sync()

        # stage backgrounds
        FileHandler(self.get_bkg_dict(AttrDict(self.task_config, **self.task_config))).sync()

        # generate diagb YAML file
        logger.debug(f"Generate diagb YAML file: {self.task_config.diagb_yaml}")
        save_as_yaml(self.task_config.bmat_config, self.task_config.diagb_yaml)
        logger.info(f"Wrote diagb YAML to: {self.task_config.diagb_yaml}")

    @logit(logger)
    def finalize(self) -> None:
        super().finalize()
