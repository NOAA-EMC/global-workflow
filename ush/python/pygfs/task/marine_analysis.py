#!/usr/bin/env python3

from datetime import datetime, timedelta
import dateutil.parser as dparser
from netCDF4 import Dataset
from logging import getLogger
import os
from pygfs.jedi import Jedi
from pygfs.task.analysis import Analysis
from wxflow import (AttrDict, FileHandler,
                    to_timedelta, to_fv3time, to_isotime,
                    parse_j2yaml, parse_j2tmpl,
                    logit)

logger = getLogger(__name__.split('.')[-1])


class MarineAnalysis(Analysis):
    """
    Class for global marine analysis tasks
    """
    @logit(logger, name="MarineAnalysis")
    def __init__(self, config):
        """Constructor for global marine analysis

        This method will construct a marine analysis task
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task
        - loading the task configuration YAML
        - instantiating the dictionary of Jedi objects for each JEDI application

        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration

        Returns
        ----------
        None
        """

        super().__init__(config)

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

        # Get restart date
        if self.task_config.DOIAU:
            _rst_date = to_fv3time(self.task_config.WINDOW_BEGIN)
            _cice_rst_date = to_fv3time(self.task_config.WINDOW_BEGIN)
        else:
            _rst_date = to_fv3time(self.task_config.current_cycle)
            _cice_rst_date = to_fv3time(self.task_config.current_cycle)

        # Generate list of pseudo model states
        dt_pseudo = 3
        fcst_hour_list = list(range(6, 10, dt_pseudo))
        _marine_pseudo_model_states = []
        bkg_date = self.task_config.WINDOW_BEGIN
        for fcst_hour in fcst_hour_list:
            bkg_date = bkg_date + timedelta(hours=dt_pseudo)
            _marine_pseudo_model_states.append({'date': to_isotime(bkg_date),
                                                'basename': './bkg/',
                                                'ocn_filename': f"ocean.bkg.f{str(fcst_hour).zfill(3)}.nc",
                                                'ice_filename': f"ice.bkg.f{str(fcst_hour).zfill(3)}.nc",
                                                'read_from_file': 1})

        # Create a local dictionary that is repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'PARMmarine': os.path.join(self.task_config.PARMglobal, 'gdas', 'marine'),
                'ENSPERT_RELPATH': _enspert_relpath,
                'berror_model': _berror_model,
                'rst_date': _rst_date,
                'cice_rst_date': _cice_rst_date,
                'marine_pseudo_model_states': _marine_pseudo_model_states
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Construct dictionary of JEDI objects, one for each JEDI application need for the analysis
        expected_keys = ['var', 'soca_incpostproc', 'soca_diag_stats']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize the marine analysis task

        This method will initialize the marine analysis.
        This includes:
        - staging input files from COM and create output directories
        - staging observation files
        - preparing the namelists for deterministic MOM6 and analysis geometry
        - asserting that dates of the history files are correct
        - initializing all the JEDI applications required for the marine analysis
        - initialize obs stats application

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # stage files from COM
        logger.info(f"Staging files from COM and creating input/output directories")
        FileHandler(self.task_config.data_in).sync()

        # Stage observation files
        logger.info(f"Staging observations")
        self.jedi_dict['var'].stage_obsdatain(self.task_config.COMIN_OBS)

        # prepare the deterministic MOM6 input.nml
        logger.info(f"Preparing deterministic MOM6 input namelist")
        parse_j2tmpl(os.path.join(self.task_config.PARMmarine, 'mom_input.nml.j2'),
                     self.task_config,
                     output_file="mom_input.nml")

        # prepare the input.nml for the analysis geometry
        logger.info(f"Preparing analysis geometry input namelist")
        parse_j2tmpl(os.path.join(self.task_config.PARMmarine, 'mom_input_anlgeom.nml.j2'),
                     self.task_config,
                     output_file="./anl_geom/mom_input.nml")

        # assert that dates of the history files are correct
        test_hist_date('./INPUT/MOM.res.nc', self.task_config.WINDOW_BEGIN)
        for state in self.task_config.marine_pseudo_model_states:
            test_hist_date(state['basename'] + state['ocn_filename'],
                           datetime.strptime(state['date'], '%Y-%m-%dT%H:%M:%SZ'))

        # initialize JEDI applications
        logger.info(f"Initializing JEDI applications")
        self.jedi_dict['var'].initialize(clean_empty_obsspaces=True)
        self.jedi_dict['soca_incpostproc'].initialize()

        # This method is a bit of a hack that will be removed in the future when the anlstat
        # job fully replaces the SOCA obs_diag_stats application
        try:
            self.initialize_obs_stats()
        except Exception as e:
            logger.warning(f"Failed to initialize observation statistics: {e}")

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
    def finalize(self) -> None:
        """Finalize a global marine analysis

        This method will finalize a global marine analysis.
        This includes:
        - Saving output files to COM
        - Archiving, compressing, and saving diag files in COM directory
        - Saving (legacy) observation statistics to COM

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()

        # Archive, compress, and save diag files in COM directory
        logger.info(f"Saving observation diag files to COM")
        self.jedi_dict['var'].save_obsdataout(self.task_config.COMOUT_OCEAN_ANALYSIS,
                                              f"{self.task_config.APREFIX}marine_analysis.ioda_hofx")

        # Save obs diag statistics to COM (this is for legacy obs monitoring)
        logger.info(f"Copy (legacy) observation statistics from {self.task_config.DATA} to {self.task_config.COMOUT_OCEAN_ANALYSIS}")
        try:
            diags_list = self.jedi_dict['soca_diag_stats'].render_jcb_template(algorithm_in='soca_diags_finalize')
        except Exception as e:
            logger.warning(f"Failed to render JCB template, 'soca_diags_finalize': {e}")
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


@logit(logger)
def test_hist_date(histfile: str, ref_date: datetime) -> None:
    """
    Check that the date in the MOM6 history file is the expected one for the cycle.
    TODO: Implement the same for seaice
    """

    ncf = Dataset(histfile, 'r')
    hist_date = dparser.parse(ncf.variables['time'].units, fuzzy=True) + timedelta(hours=int(ncf.variables['time'][0]))
    ncf.close()
    logger.info(f"*** history file date: {hist_date} expected date: {ref_date}")

    if hist_date != ref_date:
        raise ValueError(f"FATAL ERROR: Inconsistent bkg date, Expected {ref_date}, {histfile} contains {hist_date}")
