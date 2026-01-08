#!/usr/bin/env python3

from logging import getLogger
from pygfs.task.analysis import Analysis
from pygfs.jedi import Jedi
from typing import Any, Dict
from wxflow import AttrDict, FileHandler, parse_j2yaml, logit

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
        - stage observation files
        - stage bias correction files
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

        # Stage observation files
        logger.info(f"Staging observation files")
        self.jedi_dict['atmanlvar'].stage_obsdatain(f"{self.task_config.COMIN_OBS}/atmos")

        # Stage bias correction files
        logger.info(f"Staging bias correction files")
        self.jedi_dict['atmanlvar'].stage_obsbiasin(self.task_config.COMIN_ATMOS_ANALYSIS_PREV)

        # Initialize JEDI variational application
        logger.info(f"Initializing JEDI applications")
        self.jedi_dict['atmanlvar'].initialize(clean_empty_obsspaces=True)
        self.jedi_dict['atmanlfv3inc'].initialize()

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
        - archive, compress, and save diag files to COM directory
        - tar radiative bias correction files to COM directory
        - save output files and YAMLs to COM

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Archive, compress, and save diag files to COM directory
        logger.info(f"Saving observation diag files to COM")
        self.jedi_dict['atmanlvar'].save_obsdataout(self.task_config.COMOUT_ATMOS_ANALYSIS,
                                                    f"{self.task_config.APREFIX}atmos_analysis.ioda_hofx")

        # Tar radiative bias correction files to COM directory
        logger.info(f"Saving radiative bias correction files to COM")
        self.jedi_dict['atmanlvar'].save_obsbiasout(self.task_config.COMOUT_ATMOS_ANALYSIS,
                                                    f"{self.task_config.APREFIX}rad_varbc_params")

        # Save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()
