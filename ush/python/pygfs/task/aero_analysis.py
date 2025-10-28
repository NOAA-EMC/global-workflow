#!/usr/bin/env python3

import os
from logging import getLogger
from netCDF4 import Dataset
from typing import Dict, List

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_timedelta,
                    to_fv3time,
                    Task, Jinja,
                    YAMLFile, parse_j2yaml,
                    logit)
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
from pygfs.task.upp import UPP

logger = getLogger(__name__.split('.')[-1])


class AerosolAnalysis(Analysis):
    """
    Class for JEDI-based global aerosol analysis tasks
    """
    @logit(logger, name="AerosolAnalysis")
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
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global aerosol analysis

        This method will initialize a global aerosol analysis using JEDI.
        This includes:
        - stage input files from COM and create output directories
        - extract bias corrections from tar files
        - initialize JEDI application
        """

        # Stage files from COM
        logger.info(f"Staging files from COM")
        FileHandler(self.task_config.data_in).sync()

        # Extract bias corrections from tar files
        logger.info(f"Extracting bias corrections from tar files")
        self.untar_bias_corrections()

        # initialize JEDI variational application
        logger.info(f"Initializing JEDI variational DA application")
        self.jedi_dict['aeroanlvar'].initialize(self.task_config, clean_empty_obsspaces=True)

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
        - compress and tar output diag files in COM
        - tar radiative bias correction files in COM
        - save output files and YAMLs to COM

        """

        # ---- add increments to RESTART files
        logger.info('Adding increments to RESTART files')
        self._add_fms_cube_sphere_increments()

        # Compress and tar diag files in COM directory
        self.tar_diag_files(self.task_config.COMOUT_CHEM_ANALYSIS,
                            f"{self.task_config['APREFIX']}aerostat.tgz")

        # Tar radiative bias correction files into COM directory
        self.tar_radiative_bias_corrections(self.task_config.COMOUT_CHEM_ANALYSIS,
                                            f"{self.task_config.APREFIX}aero_varbc_params.tar")

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
        incvars_list_path = os.path.join(self.task_config['PARMgfs'], 'gdas', 'aero', 'aero_det_inc_vars.yaml')
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

    @logit(logger)
    def upp_anlproc(self) -> None:
        """Process aerosol analysis to GRIB2

        This method processes aerosol analysis products from tracer fields using UPP.
        This includes:
        - Creating a UPP object
        - Staging UPP fix files
        - Creating the 'upp_dict' for UPP object
        - Generating the upp namelist
        - Adding atmos and aerosol increments to the background
        - Execute upp.x
        """

        local_dict = AttrDict(
            {
                'UPP_RUN': "analysis",
                'FORECAST_HOUR': 0
            }
        )
        self.task_config = AttrDict(**self.task_config, **local_dict)
        self.task_config.UPP_CONFIG = self.task_config.UPP_CONFIG_YAML
        upp = UPP(self.task_config)

        upp_yaml = upp.task_config.upp_yaml
        upp.initialize(upp_yaml)

        upp_dict = AttrDict()
        keys = ['APRUN_AEROANLFINAL', 'forecast_hour',
                'atmos_filename', 'flux_filename']

        upp_dict = AttrDict()
        for key in keys:
            upp_dict[key] = upp.task_config[key]

        upp_dict['NET'] = 'gfs'   # set to 'gfs' so upp can recognize
        upp_dict['valid_datetime'] = self.task_config.current_cycle
        upp_dict['DATA'] = os.path.join(self.task_config.DATA, 'upp')
        upp_dict.update(upp_yaml['upp']['config'])

        # Configure the namelist and write to file
        logger.info("Creating namelist for upp.x")
        nml_template = os.path.join(upp_dict.DATA, "itag.jinja")
        nml_data = Jinja(nml_template, upp_dict).render
        logger.debug(f"itag:\n{nml_data}")
        nml_file = os.path.join(upp_dict.DATA, 'itag')
        with open(nml_file, "w") as fho:
            fho.write(nml_data)

        # ---- add aero increments to atmf000 files
        logger.info('Adding aero increments to RESTART files')
        bkg_file = os.path.join(upp_dict.DATA, f"{upp_dict.atmos_filename}")
        inc_filename = f"aeroinc_gauss.{self.task_config.current_cycle.strftime('%Y-%m-%dT%H:%M:%S')}Z.gaussian.modelLevels.nc"
        inc_file = os.path.join(self.task_config.DATA, 'anl', inc_filename)
        allvars = upp_yaml['aeroincvars'][:]
        bkgvars = [var[0] for var in allvars]
        incvars = [var[1] for var in allvars]
        self.add_aero_gaussian_increments(inc_file, bkg_file, incvars, bkgvars)

        # ---- add atmo increments to atmf000 files
        logger.info('Adding atmo increments to RESTART files')
        inc_file = os.path.join(upp_dict.DATA, f"{self.task_config.APREFIX}atminc.nc")
        allvars = upp_yaml['atmincvars'][:]
        bkgvars = [var[0] for var in allvars]
        incvars = [var[1] for var in allvars]
        self.add_atm_gaussian_increments(inc_file, bkg_file, incvars, bkgvars)

        # reset time to 0 (analysis time)
        flux_file = os.path.join(upp_dict.DATA, f"{upp_dict.flux_filename}")
        with Dataset(flux_file, mode='a') as rstfile:
            time = rstfile.variables['time']
            time[:] = 0.0
            time.setncattr("units", f"hours since {self.task_config.current_cycle.strftime('%Y-%m-%d %H:%M:%S')}")

        upp.execute(upp_dict.DATA, upp_dict.APRUN_AEROANLFINAL, upp_dict.forecast_hour)

    @logit(logger)
    def add_aero_gaussian_increments(self, inc_file: str, bkg_file: str, incvars: List, bkgvars: List) -> None:
        """Add aero gaussian increments to gaussian backgrounds

        Parameters
        ----------
        inc_file : str
           increment file
        bkg_file : str
           background file
        incvars : List
           List of increment variables to add to the background
        bkgvars : List
           List of background variables to which the increment variables will be added.
        """
        with Dataset(inc_file, mode='r') as incfile, Dataset(bkg_file, mode='a') as rstfile:
            for incname, bkgname in zip(incvars, bkgvars):
                increment = incfile.variables[incname][:]
                # reordering the dimensions of increment to macth background
                increment_reshape = np.transpose(increment, (2, 0, 1))

                bkg = rstfile.variables[bkgname][:]
                anl = bkg + increment_reshape[np.newaxis, :, :, :]
                rstfile.variables[bkgname][:] = anl[:]
            time = rstfile.variables['time']
            time[:] = 0.0
            time.setncattr("units", f"hours since {self.task_config.current_cycle.strftime('%Y-%m-%d %H:%M:%S')}")

    @logit(logger)
    def add_atm_gaussian_increments(self, inc_file: str, bkg_file: str, incvars: List, bkgvars: List) -> None:
        """Add atm gaussian increments to gaussian backgrounds

        Parameters
        ----------
        inc_file : str
           increment file
        bkg_file : str
           background file
        incvars : List
           List of increment variables to add to the background
        bkgvars : List
           List of background variables to which the increment variables will be added.
        """
        with Dataset(inc_file, mode='r') as incfile, Dataset(bkg_file, mode='a') as rstfile:
            for incname, bkgname in zip(incvars, bkgvars):
                increment = incfile.variables[incname][:]
                # handel latitude inversion in atminc
                lat_axis_index = 1
                increment_lat_inversion = np.flip(increment, axis=lat_axis_index)

                bkg = rstfile.variables[bkgname][:]
                anl = bkg + increment_lat_inversion[np.newaxis, :, :, :]
                rstfile.variables[bkgname][:] = anl[:]
