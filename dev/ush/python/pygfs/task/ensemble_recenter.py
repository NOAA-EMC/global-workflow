#!/usr/bin/env python3

from logging import getLogger
from pygfs.task.analysis import Analysis
from pygfs.jedi import Jedi
from typing import Dict, Any
from wxflow import AttrDict, FileHandler, parse_j2yaml, logit

logger = getLogger(__name__.split('.')[-1])


class EnsembleRecenter(Analysis):
    """
    Class for JEDI-based ensemble increment recentering
    """
    def __init__(self, config: Dict[str, Any]):
        """Constructor for atmospheric ensemble increment recentering task

        This method will construct an ensemble increment recentering task
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

        _res = int(self.task_config.CASE[1:])
        _res_anl = int(self.task_config.CASE_ANL[1:])

        # Create a local dictionary that is repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Create dictionary of Jedi objects
        expected_keys = ['correction_increment', 'ensemble_recenter']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize the ensemble increment recentering task

        This method will initialize the ensemble increment recentering task.
        This includes:
        - stage input files from COM and create output directories
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

        # Initialize JEDI ensemble increment recentering application
        logger.info(f"Initializing JEDI applications")
        self.jedi_dict['correction_increment'].initialize()
        self.jedi_dict['ensemble_recenter'].initialize()

    @logit(logger)
    def execute(self) -> None:
        """Run JEDI executable

        This method will run the JEDI executable for the ensemble increment recentering

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Compute correction increment for ensemble recentering
        self.jedi_dict['correction_increment'].execute()

        # Recenter increments
        self.jedi_dict['ensemble_recenter'].execute()

    @logit(logger)
    def finalize(self) -> None:
        """Finalize the ensemble increment recentering task

        This method will finalize the ensemble increment recentering task.
        This includes:
        - save output files and YAMLs to COM

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Save output files to COM
        logger.info(f"Saving output files to COM")
        FileHandler(self.task_config.data_out).sync()
