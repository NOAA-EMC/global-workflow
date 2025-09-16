#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from logging import getLogger
from pprint import pformat
from typing import Any, Dict
from wxflow import (AttrDict, FileHandler,
                    add_to_datetime, to_timedelta,
                    parse_j2yaml,
                    logit, save_as_yaml)
from pygfs.task.fv3_analysis import FV3Analysis
from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class AtmAnalysis(FV3Analysis):
    """
    Class for JEDI-based global atm deterministic analysis tasks
    """
    @logit(logger, name="AtmAnalysis")
    def __init__(self, config: Dict[str, Any]):
        """Constructor global atm analysis task

        This method will construct a global atm analysis task.
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

        _res = int(self.task_config.CASE[1:])
        if self.task_config.DOHYBVAR:
            _res_anl = int(self.task_config.CASE_ENS[1:])
        else:
            _res_anl = int(self.task_config.CASE[1:])

        _localization_type = 'bump'

        if self.task_config.DOHYBVAR:
            _BERROR_YAML = f"atmosphere_background_error_hybrid_{self.task_config.STATICB_TYPE}_{_localization_type}"
        else:
            _BERROR_YAML = f"atmosphere_background_error_static_{self.task_config.STATICB_TYPE}"

        # Create a local dictionary that is repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'observations': parse_j2yaml(self.task_config.OBS_LIST_YAML, self.task_config)['observations'],
                'bias_files': parse_j2yaml(self.task_config.BIAS_FILES_YAML, self.task_config)['bias_files'],
                'BERROR_YAML': _BERROR_YAML,
            }
        ))

        # Create dictionary of Jedi objects
        expected_keys = ['atmanlvar', 'atmanlfv3inc']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global atm analysis

        This method will initialize a global atm analysis.
        This includes:
        - initialize JEDI applications
        - staging observation files
        - staging bias correction files
        - staging CRTM fix files
        - staging FV3-JEDI fix files
        - staging B error files
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
                Jedi.extract_tar(f'{self.task_config.DATA}/obs/{self.task_config.GPREFIX}{self.task_config.bias_files[ob]}')

        # Initialize JEDI variational application
        logger.info(f"Initializing JEDI applications")
        self.jedi_dict['atmanlvar'].initialize(self.task_config, clean_empty_obsspaces=True)
        self.jedi_dict['atmanlfv3inc'].initialize(self.task_config)

    @logit(logger)
    def execute(self, jedi_dict_key: str) -> None:
        """Execute JEDI application of atm analysis

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
    def finalize(self) -> None:
        """Finalize a global atm analysis

        This method will finalize a global atm analysis using JEDI.
        This includes:
        - tar output diag files and place in ROTDIR
        - copy the generated YAML file from initialize to the ROTDIR
        - copy the updated bias correction files to ROTDIR

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Set paths of output tar files
        diagtar = os.path.join(self.task_config.COMOUT_ATMOS_ANALYSIS, f"{self.task_config.APREFIX}atmstat")
        radtar = os.path.join(self.task_config.COMOUT_ATMOS_ANALYSIS, f"{self.task_config.APREFIX}rad_varbc_params.tar")

        # Get lists of files to put in tarballs
        diaglist = glob.glob(os.path.join(self.task_config.DATA, 'diags', 'diag*nc'))
        satlist = glob.glob(os.path.join(self.task_config.DATA, 'bc', '*satbias*nc'))
        tlaplist = glob.glob(os.path.join(self.task_config.DATA, 'obs', '*tlapse.txt'))

        # Compress diag files
        logger.info(f"Compressing {len(diaglist)} diag files")
        for diagfile in diaglist:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # Create tarball of compressed diag files in COM
        logger.debug(f"Creating tarball {diagtar} with {len(diaglist)} compressed diag files")
        with tarfile.open(diagtar, "w") as archive:
            for diagfile in diaglist:
                diaggzip = f"{diagfile}.gz"
                archive.add(diaggzip, arcname=os.path.basename(diaggzip))

        # Create tarball of radiance bias correction files
        logger.info(f"Creating radiance bias correction tarball {radtar}")
        with tarfile.open(radtar, 'w') as radbcor:
            logger.info(f"Adding {radbcor.getnames()}")
            for satfile in satlist:
                radbcor.add(satfile, arcname=os.path.basename(satfile))
            for tlapfile in tlaplist:
                # Change OPREFIX to APREFIX in tlapse file name when adding to tarball
                radbcor.add(tlapfile, arcname=os.path.basename(tlapfile.replace(self.task_config.OPREFIX, self.task_config.APREFIX)))

        # Save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.save).sync()
        