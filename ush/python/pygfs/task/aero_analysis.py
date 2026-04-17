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

        _coldstart = self.task_config.get('COLDSTART', False)
        _fixaer = self.task_config.get('FIXaer')

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
                'coldstart': _coldstart,
                'fixaer': _fixaer,
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
        """Execute JEDI application of aero analysis.

        Parameters
        ----------
        jedi_dict_key : str
            Key specifying the particular Jedi object in self.jedi_dict.

        Returns
        -------
        None
        """
        coldstart = self.task_config.get('COLDSTART') or self.task_config.get('coldstart')

        logger.info(F"COLDSTART: {self.task_config.get('COLDSTART')}, coldstart: {self.task_config.get('coldstart')}")
        # Check if background files exist before executing JEDI
        if self.task_config.DOIAU:
            bkgtime = self.task_config.AERO_WINDOW_BEGIN
        else:
            bkgtime = self.task_config.current_cycle

        # Check for first tile of fv_tracer to decide if we run JEDI or climatology
        fv_tracer_file = f'{to_fv3time(bkgtime)}.fv_tracer.res.tile1.nc'
        bkg_path = os.path.join(self.task_config.DATA, 'anl', fv_tracer_file)

        # if self.task_config.coldstart:
        if coldstart:
            logger.info('Coldstart detected: applying MERRA2 climatology instead of running JEDI')
            self._apply_merra2_climo()
            return

        if not os.path.exists(bkg_path):
            logger.warning(f"Background file {bkg_path} not found. Skipping JEDI and using MERRA2 climatology.")
            self._apply_merra2_climo()
            return

        self.jedi_dict[jedi_dict_key].execute()

    @logit(logger)
    def finalize(self) -> None:
        """Finalize a global aerosol analysis.

        This method will finalize a global aerosol analysis using JEDI.
        This includes:
        - Applying increments to the original RESTART files or applying climatology.
        - Archiving, compressing, and saving diag files to COM.
        - Archiving and saving radiative bias correction files to COM.
        - Saving output files and YAMLs to COM.

        Returns
        -------
        None
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

    @logit(logger)
    def _apply_merra2_climo(self) -> None:
        """Apply MERRA2 climatology to tracer files when restarts are missing.

        This method iterates over all tiles and performs horizontal and vertical
        interpolation of MERRA2 aerosol climatology onto the GFS grid.

        Returns
        -------
        None
        """
        if self.task_config.DOIAU:
            bkgtime = self.task_config.AERO_WINDOW_BEGIN
        else:
            bkgtime = self.task_config.current_cycle

        current_month = bkgtime.strftime('%m')

        from pygfs.utils.merra2climo_to_gdas import (
            open_dataset, get_fv3_plevs, get_merra2_plevs,
            horizontal_interp, vertical_interp
        )

        # akbk.nc4 is staged by aero_stage_jedi_fix.yaml.j2 for all cases
        akbk_file = os.path.join(self.task_config.DATA, 'fv3jedi', 'akbk.nc4')
        merra_file = os.path.join(self.task_config.DATA, 'bkg', f"merra2.aerclim.2014-2023.m{current_month}.nc")

        ds_akbk = open_dataset(akbk_file)
        fv3_press = get_fv3_plevs(ds_akbk)
        ak, bk = get_fv3_plevs(ds_akbk, return_akbk=True)
        merra_press = get_merra2_plevs()[1:]

        ds_merra = open_dataset(merra_file).isel(time=0)

        merra_2_aerosols = ['BCPHILIC', 'BCPHOBIC', 'DMS',
                            'DU001', 'DU002', 'DU003', 'DU004', 'DU005',
                            'OCPHILIC', 'OCPHOBIC', 'SO2', 'SO4',
                            'SS001', 'SS002', 'SS003', 'SS004', 'SS005', 'MSA']
        rename_dict = dict(BCPHILIC='bc2', BCPHOBIC='bc1', DMS='dms',
                           DU001='dust1', DU002='dust2', DU003='dust3', DU004='dust4', DU005='dust5',
                           SS001='seas1', SS002='seas2', SS003='seas3', SS004='seas4', SS005='seas5',
                           OCPHILIC='oc2', OCPHOBIC='oc1', SO2='so2', SO4='so4', MSA='msa')
        fv3_units = dict(so2='ppm', so4='ug/kg', dms='ppm', msa='ppm', bc2='ug/kg', bc1='ug/kg',
                         dust1='ug/kg', dust2='ug/kg', dust3='ug/kg', dust4='ug/kg', dust5='ug/kg',
                         seas1='ug/kg', seas2='ug/kg', seas3='ug/kg', seas4='ug/kg', seas5='ug/kg',
                         oc1='ug/kg', oc2='ug/kg')

        ds_merra = ds_merra[merra_2_aerosols].rename(rename_dict)

        for itile in range(1, self.task_config.ntiles + 1):
            tracer_file = os.path.join(self.task_config.DATA, 'anl', f'{to_fv3time(bkgtime)}.fv_tracer.res.tile{itile}.nc')
            core_tile_file = os.path.join(self.task_config.DATA, 'bkg', f'{to_fv3time(bkgtime)}.fv_core.res.tile{itile}.nc')
            oro_file = os.path.join(self.task_config.FIXorog, self.task_config.CASE,
                                    f"{self.task_config.CASE}.mx{self.task_config.OCNRES}_oro_data.tile{itile}.nc")

            logger.info(f"Applying MERRA2 climatology to {tracer_file}")

            # Load all data eagerly so files can be closed before the in-place write
            with open_dataset(tracer_file) as ds_tracer:
                o3mr_shape = ds_tracer.o3mr.values.shape

            with open_dataset(core_tile_file) as ds_core_tile:
                temp = ds_core_tile.T.squeeze().values

            with open_dataset(oro_file) as ds_oro:
                grid = ds_oro[['geolon', 'geolat']].load()

            # Horizontal then vertical interpolation
            hinterp = horizontal_interp(ds_merra, grid)
            hvinterp = vertical_interp(hinterp, np.log(merra_press), np.log(fv3_press))

            # Pressure and density for unit conversion of gas-phase species
            pmid = 0.5 * ((ak[1:] + ak[:-1]) + (bk[1:] + bk[:-1]) * 101325.0)
            p = pmid.reshape(-1, 1, 1) * np.ones_like(np.empty(o3mr_shape))
            density = p / (287.0 * temp)

            # Build per-field arrays then write in one open of the restart file
            field_data = {}
            for orig_name, field in rename_dict.items():
                interp_data = hvinterp[field].fillna(0.).values

                unit = fv3_units[field]
                if unit == 'ug/kg':
                    interp_data = interp_data * 1e9
                elif unit == 'ppm':
                    mw = {'dms': 63.15, 'so2': 64.066, 'msa': 96.11}.get(field)
                    interp_data = interp_data * density * 1e6 * 24.45 / mw
                field_data[field] = interp_data

            # Write directly into the restart file to preserve dimension/variable order.
            # All xarray handles are already closed above.
            with Dataset(tracer_file, 'r+') as nc:
                for field, data in field_data.items():
                    nc[field][:] = data

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
