#!/usr/bin/env python3

from logging import getLogger
from typing import Any, Dict
from pygfs.jedi import Jedi
from pygfs.task.analysis import Analysis
from pygfs.task.atm_analysis import atm_task_config
from pygfs.task.marine_analysis import marine_task_config
from pygfs.utils.marine_da_utils import (test_hist_date, prep_var_input_nml,
                                         initialize_obs_stats, save_obs_stats)
from wxflow import FileHandler, parse_j2yaml, logit

logger = getLogger(__name__.split('.')[-1])


class CoupledAnalysis(Analysis):
    """
    Class for the coupled (FV3JEDI atmosphere + SOCA ocean and sea ice) deterministic
    analysis tasks

    One JEDI variational application minimizes a single cost function over both
    components; the increment post-processing that follows stays per-component.
    """
    def __init__(self, config: Dict[str, Any]):
        """Constructor for the global coupled analysis task

        This method will construct a global coupled analysis task.
        This includes:
        - extending the task_config attribute AttrDict to include parameters required
          by both components
        - loading the task configuration YAML
        - instantiating the dictionary of Jedi objects

        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration

        Returns
        ----------
        None
        """

        super().__init__(config)

        # Both components contribute to a coupled task_config. These are the same
        # functions the single-component analyses use, so the coupled run sees the same
        # geometry, background error selection and restart dates they do.
        self.task_config.update(atm_task_config(self.task_config))
        self.task_config.update(marine_task_config(self.task_config))

        # The marine keys tag the JEDI analysis and increment files with this; the marine
        # default names its 3DFGAT, which this is not
        self.task_config.marine_var_exp = '3dvar_coupled'

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Construct dictionary of JEDI objects, one for each JEDI application
        expected_keys = ['coupledanlvar', 'coupledanlfv3inc', 'soca_incpostproc', 'soca_diag_stats']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global coupled analysis

        This method will initialize a global coupled analysis.
        This includes:
        - stage input files from COM and create output directories
        - stage observation files for both components
        - stage atmospheric bias correction files
        - prepare the namelists for deterministic MOM6 and analysis geometry
        - assert that dates of the background files are correct
        - initialize JEDI applications

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Stage files from COM
        logger.info(f"Staging files from COM and creating input/output directories")
        FileHandler(self.task_config.data_in).sync()

        # Stage observation files. The two components keep their observations in different
        # COM directories, so each gets its own path.
        logger.info(f"Staging observation files")
        self.jedi_dict['coupledanlvar'].stage_obsdatain(
            {'atmosphere': f"{self.task_config.COMIN_OBS}/atmos",
             'marine': self.task_config.COMIN_OBS})

        # Stage bias correction files. Only the atmosphere has any.
        logger.info(f"Staging bias correction files")
        self.jedi_dict['coupledanlvar'].stage_obsbiasin(self.task_config.COMIN_ATMOS_ANALYSIS_PREV)

        # Prepare the MOM6 namelists
        prep_var_input_nml(self.task_config)

        # Assert that the dates of the background files are correct. Coupled 3D-Var solves
        # for a single state at the window middle, so that is where the ocean and sea ice
        # background is read; the window-begin restart is still staged because
        # soca_incpostproc needs it for the vertical geometry and for soca2cice.
        test_hist_date('./bkg/ocean.bkg.f006.nc', self.task_config.WINDOW_MIDDLE)
        test_hist_date('./INPUT/MOM.res.nc', self.task_config.WINDOW_BEGIN)

        # Initialize JEDI applications
        logger.info(f"Initializing JEDI applications")
        self.jedi_dict['coupledanlvar'].initialize(clean_empty_obsspaces=True)
        self.jedi_dict['coupledanlfv3inc'].initialize()
        self.jedi_dict['soca_incpostproc'].initialize()

        # This method is a bit of a hack that will be removed in the future when the anlstat
        # job fully replaces the SOCA obs_diag_stats application
        try:
            initialize_obs_stats(self.jedi_dict['coupledanlvar'],
                                 self.jedi_dict['soca_diag_stats'],
                                 self.task_config,
                                 component='marine')
        except Exception as e:
            logger.warning(f"Failed to initialize observation statistics: {e}")

    @logit(logger)
    def execute(self, jedi_dict_key: str) -> None:
        """Execute a JEDI application of the coupled analysis

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
        """Finalize a global coupled analysis

        This method will finalize a global coupled analysis.
        This includes:
        - saving output files to COM
        - archiving, compressing, and saving diag files for both components to COM
        - tarring atmospheric bias correction files to COM
        - saving (legacy) marine observation statistics to COM

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Save files to COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()

        # Archive, compress, and save diag files to COM. One tarball per component, each
        # to its own component's COM directory.
        logger.info(f"Saving observation diag files to COM")
        self.jedi_dict['coupledanlvar'].save_obsdataout(
            {'atmosphere': self.task_config.COMOUT_ATMOS_ANALYSIS,
             'marine': self.task_config.COMOUT_OCEAN_ANALYSIS},
            {'atmosphere': f"{self.task_config.APREFIX}atmos_analysis.ioda_hofx",
             'marine': f"{self.task_config.APREFIX}marine_analysis.ioda_hofx"})

        # Tar radiative bias correction files to COM. Only the atmosphere has any.
        logger.info(f"Saving radiative bias correction files to COM")
        self.jedi_dict['coupledanlvar'].save_obsbiasout(self.task_config.COMOUT_ATMOS_ANALYSIS,
                                                        f"{self.task_config.APREFIX}varbc_params")

        # Save marine obs diag statistics to COM (this is for legacy obs monitoring)
        save_obs_stats(self.jedi_dict['soca_diag_stats'],
                       self.task_config.DATA,
                       self.task_config.COMOUT_OCEAN_ANALYSIS)
