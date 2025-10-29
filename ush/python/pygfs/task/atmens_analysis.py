#!/usr/bin/env python3

from logging import getLogger
from pygfs.task.analysis import Analysis
from pygfs.jedi import Jedi
from typing import Dict, Any
from wxflow import AttrDict, FileHandler, parse_j2yaml, logit

logger = getLogger(__name__.split('.')[-1])


class AtmEnsAnalysis(Analysis):
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
                'npz': self.task_config.LEVS - 1,
                'BKG_TSTEP': "PT1H",  # Placeholder for 4D applications
            })
        )

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Create dictionary of JEDI objects
        expected_keys = ['atmensanlobs', 'atmensanlsol', 'atmensanlfv3inc', 'atmensanlletkf']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global atmens analysis

        This method will initialize a global atmens analysis.
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
        logger.info(f"Staging files from COM")
        FileHandler(self.task_config.data_in).sync()

        # Extract bias corrections from tar files
        logger.info(f"Extracting bias corrections from tar files")
        self.untar_bias_corrections()

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
        - compress and tar output diag files and place in COM
        - save output files and YAMLs to COM

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Compress and tar diag files in COM directory
        self.tar_diag_files(self.task_config.COMOUT_ATMOS_ANALYSIS_ENS,
                            f"{self.task_config.APREFIX_ENS}stat.atm.tar")

        # Save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()
