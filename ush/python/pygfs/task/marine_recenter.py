#!/usr/bin/env python3

from logging import getLogger
import os
import sys
from typing import Dict
import pygfs.utils.marine_da_utils as mdau
from pygfs.jedi import Jedi
from pygfs.task.analysis import Analysis
from wxflow import (AttrDict, FileHandler,
                    add_to_datetime, to_timedelta, to_fv3time, to_isotime,
                    parse_j2yaml,
                    logit)

# Import com_paths from dev/workflow to get the canonical COM_*_TMPL definitions.
# HOMEglobal is always set in the job environment.
_homeglobal = os.environ.get('HOMEglobal', '')
_workflow_dir = os.path.join(_homeglobal, 'dev', 'workflow')
if _workflow_dir not in sys.path:
    sys.path.insert(0, _workflow_dir)
from com_paths import get_com_templates  # noqa: E402

logger = getLogger(__name__.split('.')[-1])


class MarineRecenter(Analysis):
    """
    Class for global ocean analysis recentering task
    """

    def __init__(self, config: Dict) -> None:
        """Constructor for ocean recentering task

        This method will construct a marine analysis task
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task
        - loading the task configuration YAML
        - instantiating the dictionary of Jedi objects for each JEDI application

        Parameters:
        ------------
        config: Dict
            configuration, namely evironment variables
        Returns:
        --------
        None
        """

        super().__init__(config)

        _enspert_relpath = os.path.relpath(self.task_config.DATAens, self.task_config.DATA)

        if self.task_config.DOIAU:
            # forecast initialized at the begining of the DA window
            _cice_rst_date = to_fv3time(self.task_config.WINDOW_BEGIN)
        else:
            # forecast initialized at the middle of the DA window
            _cice_rst_date = to_fv3time(self.task_config.current_cycle)

        # Create a local dictionary that is repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'PARMmarine': os.path.join(self.task_config.PARMglobal, 'gdas', 'marine'),
                'ENSPERT_RELPATH': _enspert_relpath,
                'cice_rst_date': _cice_rst_date,
                'DOMAIN_STACK_SIZE': 116640000,  # TODO: Make the stack size resolution dependent
            }
        ))

        # Load COM templates so they are available for Jinja2 rendering
        self.task_config.update(self._copy_com_templates())

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Construct dictionary of JEDI objects, one for each JEDI application need for the analysis
        expected_keys = ['ens_handler']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def _copy_com_templates(self) -> Dict[str, str]:
        """Copy COM templates needed for marine recenter jobs.

        Gets only the ocean and ice restart templates from dev/workflow/com_paths.py.
        Any matching environment variables override the defaults.

        Returns
        -------
        Dict[str, str]
            Dictionary with COM_OCEAN_RESTART_TMPL and COM_ICE_RESTART_TMPL
        """
        com_templates = get_com_templates()
        needed_templates = ['COM_OCEAN_RESTART_TMPL', 'COM_ICE_RESTART_TMPL']
        templates = {key: com_templates[key] for key in needed_templates if key in com_templates}
        env_overrides = {key: self.task_config[key] for key in needed_templates if key in self.task_config}
        templates.update(env_overrides)
        return templates

    @logit(logger)
    def initialize(self):
        """Method initialize for ocean recentering task

        This method will initialize the marine analysis.
        This includes:
        - staging input files from COM and create output directories
        - preparing the namelist for MOM6
        - initializing all the JEDI applications required for the marine recentering

        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        # stage files from COM
        logger.info(f"Staging files from COM and creating input/output directories")
        FileHandler(self.task_config.data_in).sync()

        # prepare the MOM6 input.nml
        mdau.prep_input_nml(self.task_config)

        # initialize JEDI applications
        logger.info(f"Initializing JEDI applications")
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

        This method will finalize a global marine analysis.
        This includes:
        - Saving output files to COM

        Parameters:
        ------------
        None
        Returns:
        --------
        None
        """

        # save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()
