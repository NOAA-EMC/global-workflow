#!/usr/bin/env python3

import os
from logging import getLogger
from netCDF4 import Dataset
from typing import Dict, List
from pygfs.task.analysis import Analysis
from pygfs.jedi import Jedi
from wxflow import (
    AttrDict,
    FileHandler,
    to_fv3time, to_timedelta,
    YAMLFile, parse_j2yaml,
    logit
)
import numpy as np

logger = getLogger(__name__.split('.')[-1])


class AerosolAnalysis(Analysis):
    """
    Class for JEDI-based global aerosol analysis tasks
    """
    def __init__(self, config):
        """Constructor global aero analysis task

        This method will construct a global aero analysis task.
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

        if self.task_config.DOIAU:
            _anl_time = self.task_config.WINDOW_BEGIN
        else:
            _anl_time = self.task_config.current_cycle

        _bkg_times = []
        for hour in self.task_config.aero_bkg_times:
            _bkg_times.append(self.task_config.WINDOW_BEGIN + to_timedelta(f"{str(hour)}H") - to_timedelta(f"{self.task_config.assim_freq}H") / 2)

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
                'BKG_TSTEP': "PT3H",  # FGAT
                'BERROR_YAML': f'aero_background_error_static_{self.task_config.STATICB_TYPE}',
                'AERO_BMATRIX_RESCALE_YAML': 'aero_gen_bmatrix_rescale_default.yaml.j2',
                'anl_time': _anl_time,
                'bkg_times': _bkg_times,
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Create dictionary of Jedi objects
        expected_keys = ['aeroanlvar']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global aerosol analysis

        This method will initialize a global aerosol analysis using JEDI.
        This includes:
        - stage input files from COM and create output directories
        - stage observation files
        - stage bias correction files
        - initialize JEDI application
        """

        # Stage files from COM
        logger.info(f"Staging files from COM")
        FileHandler(self.task_config.data_in).sync()

        # Stage observation files
        logger.info(f"Staging observation files")
        self.jedi_dict['aeroanlvar'].stage_obsdatain(f"{self.task_config.COMIN_OBS}/chem")

        # Stage bias correction files
        logger.info(f"Staging bias correction files")
        self.jedi_dict['aeroanlvar'].stage_obsbiasin(self.task_config.COMIN_CHEM_ANALYSIS_PREV)

        # Initialize JEDI variational application
        logger.info(f"Initializing JEDI variational DA application")
        self.jedi_dict['aeroanlvar'].initialize(clean_empty_obsspaces=True)

    @logit(logger)
    def execute(self, jedi_dict_key: str) -> None:
        """Execute JEDI application of aero analysis

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
        """Finalize a global aerosol analysis

        This method will finalize a global aerosol analysis using JEDI.
        This includes:
        - apply increments to the original RESTART files
        - archive, compress, and save diag files to COM
        - archive and save radiative bias correction files to COM
        - save output files and YAMLs to COM
        """

        # ---- add increments to RESTART files
        logger.info('Adding increments to RESTART files')
        self._add_fms_cube_sphere_increments()

        # Archive, compress, and save diag files in COM directory
        logger.info(f"Saving observation diag files to COM")
        self.jedi_dict['aeroanlvar'].save_obsdataout(self.task_config.COMOUT_CHEM_ANALYSIS,
                                                     f"{self.task_config.APREFIX}aero_analysis.ioda_hofx")

        # Archive and save radiative bias correction files into COM directory
        logger.info(f"Saving radiative bias correction files to COM")
        self.jedi_dict['aeroanlvar'].save_obsbiasout(self.task_config.COMOUT_CHEM_ANALYSIS,
                                                     f"{self.task_config.APREFIX}aero_varbc_params")

        # Save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()

    def clean(self):
        super().clean()

    @logit(logger)
    def _add_fms_cube_sphere_increments(self) -> None:
        """This method adds increments to RESTART files to get an analysis
        """
        if self.task_config.DOIAU:
            bkgtime = self.task_config.AERO_WINDOW_BEGIN
        else:
            bkgtime = self.task_config.current_cycle
        # only need the fv_tracer files
        restart_template = f'{to_fv3time(bkgtime)}.fv_tracer.res.tile{{tilenum}}.nc'
        increment_template = f'{to_fv3time(self.task_config.current_cycle)}.fv_tracer.res.tile{{tilenum}}.nc'
        inc_template = os.path.join(self.task_config.DATA, 'anl', 'aeroinc.' + increment_template)
        bkg_template = os.path.join(self.task_config.DATA, 'anl', restart_template)
        # get list of increment vars
        incvars_list_path = os.path.join(self.task_config['PARMglobal'], 'gdas', 'aero', 'aero_det_inc_vars.yaml')
        incvars = YAMLFile(path=incvars_list_path)['incvars']
        self.add_fv3_increments(inc_template, bkg_template, incvars)

    @logit(logger)
    def add_fv3_increments(self, inc_file_YAML: str, bkg_file_YAML: str, incvars: List) -> None:
        """Add cubed-sphere increments to cubed-sphere backgrounds

        Parameters
        ----------
        inc_file_YAML : str
           template of the FV3 increment file of the form: 'filetype.tile{tilenum}.nc'
        bkg_file_YAML : str
           template of the FV3 background file of the form: 'filetype.tile{tilenum}.nc'
        incvars : List
           List of increment variables to add to the background
        """

        for itile in range(1, self.task_config.ntiles + 1):
            inc_path = inc_file_YAML.format(tilenum=itile)
            bkg_path = bkg_file_YAML.format(tilenum=itile)
            with Dataset(inc_path, mode='r') as incfile, Dataset(bkg_path, mode='a') as rstfile:
                for vname in incvars:
                    increment = incfile.variables[vname][:]
                    # round to 7th decimal due to JEDI reproducibility issues when changing PE count
                    increment = np.round(increment, 7)
                    bkg = rstfile.variables[vname][:]
                    anl = bkg + increment
                    rstfile.variables[vname][:] = anl[:]
                    try:
                        rstfile.variables[vname].delncattr('checksum')  # remove the checksum so fv3 does not complain
                    except (AttributeError, RuntimeError):
                        pass  # checksum is missing, move on
