#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from logging import getLogger
from pprint import pformat
from typing import Dict, Any

from wxflow import (AttrDict, FileHandler, Task,
                    add_to_datetime, to_timedelta, to_YMD,
                    parse_j2yaml,
                    logit,
                    Template, TemplateConstants)
from pygfs.task.fv3_analysis import FV3Analysis
from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class AtmEnsAnalysis(FV3Analysis):
    """
    Class for JEDI-based global atmens analysis tasks
    """
    @logit(logger, name="AtmEnsAnalysis")
    def __init__(self, config: Dict[str, Any]):
        """Constructor global atmens analysis task

        This method will construct a global atmens analysis task.
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task
        - instantiate the Jedi attribute objects

        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration

        Returns
        ----------
        None
        """
        super().__init__(config)

        _res = int(self.task_config.CASE_ENS[1:])

        # Create a local dictionary that is repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
            })
        )

        # Create dictionary of JEDI objects
        expected_keys = ['atmensanlobs', 'atmensanlsol', 'atmensanlfv3inc', 'atmensanlletkf']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global atmens analysis

        This method will initialize a global atmens analysis.
        This includes:
        - initialize JEDI LETKF observer and FV3 increment converter applications
        - staging observation files
        - staging bias correction files
        - staging CRTM fix files
        - staging FV3-JEDI fix files
        - staging model backgrounds
        - creating output directories

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Stage files from COM
        logger.info(f"Staging files from COM")
        FileHandler(self.task_config.stage).sync()

        # Extract bias corrections from tar files
        logger.info(f"Extracting bias corrections from tar files")
        bias_file_list = []
        for ob in self.task_config.observations:
            if ob in self.task_config.bias_files and not self.task_config.bias_files[ob] in bias_file_list:
                bias_file_list.append(self.task_config.bias_files[ob])
                FV3Analysis.extract_tar(f'{self.task_config.DATA}/obs/{self.task_config.GPREFIX}{self.task_config.bias_files[ob]}')

        # initialize JEDI applications
        logger.info(f"Initializing JEDI LETKF observer application")
        self.jedi_dict['atmensanlobs'].initialize(self.task_config, clean_empty_obsspaces=True)
        self.jedi_dict['atmensanlsol'].initialize(self.task_config)
        self.jedi_dict['atmensanlfv3inc'].initialize(self.task_config)

    @logit(logger)
    def initialize_letkf(self) -> None:
        """Initialize a global atmens analysis

        Note: This would normally be done in AtmEnsAnalysis.initialize(), but that method
              now initializes the split observer-solver. This method is just for testing.

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        self.jedi_dict['atmensanlletkf'].initialize(self.task_config)

    @logit(logger)
    def execute(self, jedi_dict_key: str) -> None:
        """Execute JEDI application of atmens analysis

        Parameters
        ----------
        jedi_dict_key
            key specifying a particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        self.jedi_dict[jedi_dict_key].execute()

    @logit(logger)
    def finalize(self) -> None:
        """Finalize a global atmens analysis

        This method will finalize a global atmens analysis using JEDI.
        This includes:
        - tar output diag files and place in ROTDIR
        - copy the generated YAML file from initialize to the ROTDIR

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Compress and tar diag files in COM directory
        self.tar_diag_files(self.task_config.COMOUT_ATMOS_ANALYSIS_ENS,
                            f"{self.task_config.APREFIX_ENS}atmstat")

        # Save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.save).sync()

    def clean(self):
        super().clean()
