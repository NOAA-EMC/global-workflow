#!/usr/bin/env python3

import os
from logging import getLogger
from pygfs.task.analysis import Analysis
from pygfs.jedi import Jedi
from typing import Any, Dict
from wxflow import AttrDict, FileHandler, parse_j2yaml, logit
from run_bufr2ioda import bufr2ioda
from pprint import pformat

logger = getLogger(__name__.split('.')[-1])


class AtmAnalysis(Analysis):
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
        _res_anl = int(self.task_config.CASE_ANL[1:])
        _res_his = int(self.task_config.CASE_HIST[1:])

        if self.task_config.DOHYBVAR:
            _BERROR_YAML = f"atmosphere_background_error_hybrid_{self.task_config.STATICB_TYPE}_{self.task_config.LOCALIZATION_TYPE}"
        else:
            _BERROR_YAML = f"atmosphere_background_error_static_{self.task_config.STATICB_TYPE}"

        # Create a local dictionary that is repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.task_config.LEVS - 1,
                'npx_his': _res_his + 1,
                'npy_his': _res_his + 1,
                'npz_his': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'BKG_TSTEP': "PT1H",  # Placeholder for 4D applications
                'BERROR_YAML': _BERROR_YAML,
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Create dictionary of Jedi objects
        expected_keys = ['atmanlvar', 'atmanlfv3inc']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global atm analysis

        This method will initialize a global atm analysis.
        This includes:
        - stage input files from COM and create output directories
        - extract bias corrections from tar files
        - initialize JEDI applications

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Stage files from COM
        logger.info(f"Staging files from COM and creating output directories")
        FileHandler(self.task_config.data_in).sync()

        # Extract bias corrections from tar files
        logger.info(f"Extracting bias corrections from tar files")
        self.untar_bias_corrections()

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
        - compress and tar output diag files in COM
        - tar radiative bias correction files and place in COM
        - save output files and YAMLs to COM

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Compress and tar diag files in COM directory
        self.tar_diag_files(self.task_config.COMOUT_ATMOS_ANALYSIS,
                            f"{self.task_config.APREFIX}atmstat")

        # Tar radiative bias correction files into COM directory
        self.tar_radiative_bias_corrections(self.task_config.COMOUT_ATMOS_ANALYSIS,
                                            f"{self.task_config.APREFIX}rad_varbc_params.tar")

        # Save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()

    @logit(logger)
    def generate_ioda_obs(self) -> None:
        """
        Generate IODA formatted atmospheric observations
        This method will generate IODA formatted atmospheric observations
        using the bufr2ioda.py script.
        Parameters
        ----------
        None
        Returns
        ----------
        None
        """
        # Call the GDASApp BUFR2IODA function
        bufr2ioda(self.task_config.current_cycle, self.task_config.RUN, self.task_config.DMPDIR,
                  os.path.join(self.task_config.PARMgfs, 'gdas', 'ioda', 'bufr2ioda'),
                  self.task_config.COMOUT_OBS)

    @logit(logger)
    def stage_ioda_obs(self) -> None:
        """
        Stage IODA formatted atmospheric observations
        This method will stage IODA formatted atmospheric observations
        from the COMINobsforge directory to the local COM directory.
        Parameters
        ----------
        None
        Returns
        ----------
        None
        """

        # stage observations
        logger.info(f"Staging list of IODA observation files from {self.task_config.COMINobsforge}")
        obs_dict = parse_j2yaml(self.task_config.STAGE_COM_IODA_OBS_YAML, self.task_config)
        FileHandler(obs_dict).sync()
        logger.debug(f"Staged IODA observation files:\n{pformat(obs_dict)}")
