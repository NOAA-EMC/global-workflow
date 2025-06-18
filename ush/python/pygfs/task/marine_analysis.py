#!/usr/bin/env python3

from datetime import datetime
from logging import getLogger
import os
import pygfs.utils.marine_da_utils as mdau
from pygfs.jedi import Jedi

from wxflow import (AttrDict, FileHandler, Task,
                    add_to_datetime, to_isotime, to_timedelta, to_YMD,
                    parse_j2yaml,
                    logit)

logger = getLogger(__name__.split('.')[-1])


class MarineAnalysis(Task):
    """
    Class for global marine analysis tasks
    """
    @logit(logger, name="MarineAnalysis")
    def __init__(self, config):
        """Constructor for global marine analysis

        This method will construct a marine analysis task
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task
        - instantiate the dictionary of Jedi objects for each JEDI application

        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration

        Returns
        ----------
        None
        """

        super().__init__(config)
        _calc_scale_exec = os.path.join(self.task_config.HOMEgfs, 'ush', 'soca', 'calc_scales.py')
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)
        _window_end = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # compute the relative path from self.task_config.DATA to self.task_config.DATAens
        if self.task_config.NMEM_ENS > 0:
            _enspert_relpath = os.path.relpath(self.task_config.DATAens, self.task_config.DATA)
        else:
            _enspert_relpath = None

        # Determine background error model
        if self.task_config.NMEM_ENS >= 2:
            _berror_model = 'marine_background_error_hybrid_diffusion_diffusion'
        else:
            _berror_model = 'marine_background_error_static_diffusion'

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'PARMsoca': os.path.join(self.task_config.PARMgfs, 'gdas', 'soca'),
                'MARINE_WINDOW_BEGIN': _window_begin,
                'MARINE_WINDOW_END': _window_end,
                'MARINE_WINDOW_MIDDLE': self.task_config.current_cycle,
                'MARINE_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'MARINE_WINDOW_BEGIN_ISO': to_isotime(_window_begin),
                'MARINE_WINDOW_MIDDLE_ISO': to_isotime(self.task_config.current_cycle),
                'ENSPERT_RELPATH': _enspert_relpath,
                'CALC_SCALE_EXEC': _calc_scale_exec,
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'berror_model': _berror_model,
                'MOM6_LEVS': mdau.get_mom6_levels(str(self.task_config.OCNRES).zfill(3)),
                'app_path_observations': self.task_config.MARINE_JCB_GDAS_OBS,
                'marine_pseudo_model_states': mdau.gen_bkg_list(bkg_path='./bkg',
                                                                window_begin=_window_begin)
            }
        )

        # Extend task_config with local_dict
        self.task_config.update(local_dict)

        # Construct dictionary of JEDI objects, one for each JEDI application need for the analysis
        expected_keys = ['var', 'soca_incpostproc', 'soca_diag_stats']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML_ANALYSIS, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self: Task) -> None:
        """Initialize the marine analysis task

        This method will initialize the marine analysis.
        This includes:
        - staging SOCA fix files
        - preparing the namelists for deterministic MOM6 and analysis geometry
        - staging observations
        - staging input YAMLs for SOCA utilities
        - staging the deterministic backgrounds (middle of window)
        - staging files and link directories from B-matrix job needed for deterministic analysis
        - generating list of model pseudo states
        - initializing all the JEDI applications required for the marine analysis

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # stage fix files
        logger.info(f"Staging SOCA fix files from {self.task_config.SOCA_INPUT_FIX_DIR}")
        soca_fix_list = parse_j2yaml(self.task_config.SOCA_FIX_YAML_TMPL, self.task_config)
        FileHandler(soca_fix_list).sync()

        # prepare the deterministic MOM6 input.nml
        logger.info(f"Preparing deterministic MOM6 input namelist")
        mdau.prep_input_nml(self.task_config)

        # prepare the input.nml for the analysis geometry
        logger.info(f"Preparing analysis geometry input namelist")
        mdau.prep_input_nml(self.task_config, output_nml="./anl_geom/mom_input.nml",
                            simple_geom=True, mom_input="./anl_geom/MOM_input")

        # fetch observations from COMROOT
        # TODO(G.V. or A.E.): Keep a copy of the obs in the scratch fs after the obs prep job
        logger.info(f"Staging observations from {self.task_config.COMIN_OBS}")
        obs_list = self.jedi_dict['var'].render_jcb(self.task_config, 'soca_obs_staging')
        FileHandler(obs_list).sync()

        # stage the soca utility yamls (gridgen, fields and ufo mapping yamls)
        logger.info(f"Staging SOCA utility yaml files from {self.task_config.PARMsoca}")
        soca_utility_list = parse_j2yaml(self.task_config.MARINE_UTILITY_YAML_TMPL, self.task_config)
        FileHandler(soca_utility_list).sync()

        # stage the ocean and ice backgrounds for FGAT
        logger.info(f"Staging files needed for deterministic analysis from COM")
        bkg_list = parse_j2yaml(self.task_config.MARINE_DET_STAGE_BKG_YAML_TMPL, self.task_config)
        FileHandler(bkg_list).sync()

        # stage files and link directories from B-matrix job needed for deterministic analysis
        logger.info(f"Staging files needed for deterministic analysis from COM")
        soca_files_list = parse_j2yaml(self.task_config.MARINE_DET_STAGE_FILES_YAML_TMPL, self.task_config)
        FileHandler(soca_files_list).sync()

        # assert that dates of the history files are correct
        mdau.test_hist_date('./INPUT/MOM.res.nc', self.task_config.MARINE_WINDOW_BEGIN)
        for state in self.task_config.marine_pseudo_model_states:
            mdau.test_hist_date(state['basename'] + state['ocn_filename'],
                                datetime.strptime(state['date'], '%Y-%m-%dT%H:%M:%SZ'))

        # initialize JEDI applications
        logger.info(f"Initializing SOCA variational application")
        self.jedi_dict['var'].initialize(self.task_config, clean_empty_obsspaces=True)

        logger.info(f"Initializing SOCA increment handler")
        self.jedi_dict['soca_incpostproc'].initialize(self.task_config)

        # This method is a bit of a hack that will be removed in the future when the anlstat
        # job fully replaces the SOCA obs_diag_stats application
        self.initialize_obs_stats()

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
    def finalize(self: Task) -> None:
        """Finalize a global marine analysis

        This method will finalize a global marine analysis.
        This includes:
        - Saving output files to COM
        - Saving observation statistics to COM

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Save output files to COM
        logger.info(f"Copy files from {self.task_config.DATA} to {self.task_config.COMOUT_OCEAN_ANALYSIS}")
        soca_finalize_list = parse_j2yaml(self.task_config.MARINE_DET_FINALIZE_YAML_TMPL, self.task_config)
        FileHandler(soca_finalize_list).sync()

        # Save obs diag statistics to COM
        diags_list = self.jedi_dict['soca_diag_stats'].render_jcb(self.task_config, 'soca_diags_finalize')
        FileHandler(diags_list).sync()

    @logit(logger)
    def initialize_obs_stats(self) -> None:
        """Initialize the observation statistics

        This method will initialize the observation statistics
        This includes:
        - ...

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        #
        cleaned_observations = []
        obs_variables = {}
        for obs_space in self.jedi_dict['var'].jedi_config.input_config['cost function']['observations']['observers']:
            name = obs_space['obs space']['name']
            variable = obs_space['obs space']['simulated variables'][0]

            cleaned_observations.append(name)
            obs_variables[name] = variable

        # Update the task_config with the observation variables
        self.task_config['cleaned_observations'] = cleaned_observations
        self.task_config['obs_variables'] = obs_variables

        # Initialize the observation statistics
        logger.info(f"Initializing JEDI SOCA observation statistics application")
        self.jedi_dict['soca_diag_stats'].initialize(self.task_config)
