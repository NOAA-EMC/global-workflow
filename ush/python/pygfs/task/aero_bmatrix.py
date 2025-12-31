#!/usr/bin/env python3

from logging import getLogger
from pygfs.task.analysis import Analysis
from pygfs.jedi import Jedi
from wxflow import AttrDict, FileHandler, add_to_datetime, to_timedelta, parse_j2yaml, logit

logger = getLogger(__name__.split('.')[-1])


class AerosolBMatrix(Analysis):
    """
    Class for global aerosol BMatrix tasks
    """
    @logit(logger, name="AerosolBMatrix")
    def __init__(self, config):
        """Constructor global aero analysis bmatrix task

        This method will construct a global aero bmatrix task object.
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

        _res = int(self.task_config['CASE'][1:])
        _res_anl = int(self.task_config['CASE_ANL'][1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config['assim_freq']}H") / 2)

        # Extend task_config with variables repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.task_config['LEVS'] - 1,
                'npz': self.task_config.LEVS - 1,
                'BERROR_YAML': f'aero_background_error_static_{self.task_config.STATICB_TYPE}',
                'BERROR_DATA_DIR': f'{self.task_config.FIXgfs}/gdas/aero/clim_b',
                'AERO_BMATRIX_RESCALE_YAML': 'aero_gen_bmatrix_rescale_default.yaml.j2',
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Create dictionary of Jedi objects
        expected_keys = ['aero_interpbkg', 'aero_diagb', 'aero_diffusion']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global aerosol B-matrix

        This method will initialize a global aerosol B-Matrix.
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

        # initialize JEDI applications
        logger.info(f"Initializing JEDI applications")
        self.jedi_dict['aero_interpbkg'].initialize()
        self.jedi_dict['aero_diagb'].initialize()
        self.jedi_dict['aero_diffusion'].initialize()

    @logit(logger)
    def execute(self) -> None:
        """Generate the full B-matrix

        This method will generate the full B-matrix according to the configuration.
        This includes:
        - running all JEDI applications required to generate the B-matrix

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # interpolate backgrounds to analysis resolution
        self.jedi_dict['aero_interpbkg'].execute()

        # variance partitioning
        self.jedi_dict['aero_diagb'].execute()

        # diffusion
        self.jedi_dict['aero_diffusion'].execute()

    @logit(logger)
    def finalize(self) -> None:
        """Finalize a global aerosol bmatrix

        This method will finalize a global aerosol bmatrix using JEDI.
        This includes:
        - save output files and YAMLs to COM

        """

        # Save files to COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()
