#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from logging import getLogger
from pprint import pformat
from netCDF4 import Dataset
from typing import Dict, List

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_fv3time, to_timedelta,
                    to_fv3time,
                    Task,
                    YAMLFile, parse_j2yaml,
                    logit)
from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class AerosolAnalysis(Task):
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
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config['assim_freq']}H") / 2)

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.task_config['LEVS'] - 1,
                'AERO_WINDOW_BEGIN': _window_begin,
                'AERO_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'aero_bkg_fhr': self.task_config['aero_bkg_times'],
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'aero_obsdatain_path': f"{self.task_config.DATA}/obs/",
                'aero_obsdataout_path': f"{self.task_config.DATA}/diags/",
                'BKG_TSTEP': "PT3H"  # FGAT
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Create dictionary of Jedi objects
        expected_keys = ['aeroanlvar']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global aerosol analysis

        This method will initialize a global aerosol analysis using JEDI.
        This includes:
        - initialize JEDI applications
        - staging observation files
        - staging bias correction files
        - staging CRTM fix files
        - staging FV3-JEDI fix files
        - staging B error files
        - staging model backgrounds
        - creating output directories
        """

        # initialize JEDI variational application
        logger.info(f"Initializing JEDI variational DA application")
        self.jedi_dict['aeroanlvar'].initialize(self.task_config)

        # stage observations
        logger.info(f"Staging list of observation files generated from JEDI config")
        obs_dict = self.jedi_dict['aeroanlvar'].render_jcb(self.task_config, 'aero_obs_staging')
        FileHandler(obs_dict).sync()
        logger.debug(f"Observation files:\n{pformat(obs_dict)}")

        # # stage bias corrections
        # logger.info(f"Staging list of bias correction files")
        # bias_dict = self.jedi_dict['aeroanlvar'].render_jcb(self.task_config, 'aero_bias_staging')
        # if bias_dict['copy'] is None:
        #     logger.info(f"No bias correction files to stage")
        # else:
        #     bias_dict['copy'] = Jedi.remove_redundant(bias_dict['copy'])
        #     FileHandler(bias_dict).sync()
        #     logger.debug(f"Bias correction files:\n{pformat(bias_dict)}")

        #     # extract bias corrections
        #     Jedi.extract_tar_from_filehandler_dict(bias_dict)

        # stage CRTM fix files
        logger.info(f"Staging CRTM fix files from {self.task_config.CRTM_FIX_YAML}")
        crtm_fix_dict = parse_j2yaml(self.task_config.CRTM_FIX_YAML, self.task_config)
        FileHandler(crtm_fix_dict).sync()
        logger.debug(f"CRTM fix files:\n{pformat(crtm_fix_dict)}")

        # stage fix files
        logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
        jedi_fix_dict = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
        FileHandler(jedi_fix_dict).sync()
        logger.debug(f"JEDI fix files:\n{pformat(jedi_fix_dict)}")

        # stage files from COM and create working directories
        logger.info(f"Staging files prescribed from {self.task_config.AERO_STAGE_VARIATIONAL_TMPL}")
        aero_var_stage_dict = parse_j2yaml(self.task_config.AERO_STAGE_VARIATIONAL_TMPL, self.task_config)
        FileHandler(aero_var_stage_dict).sync()
        logger.debug(f"Staging from COM:\n{pformat(aero_var_stage_dict)}")

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
        - tarring up output diag files and place in ROTDIR
        - copying the generated YAML file from initialize to the ROTDIR
        - copying the guess files to the ROTDIR
        - applying the increments to the original RESTART files
        - moving the increment files to the ROTDIR

        """
        # ---- tar up diags
        # path of output tar statfile
        logger.info('Preparing observation space diagnostics for archiving')
        aerostat = os.path.join(self.task_config.COMOUT_CHEM_ANALYSIS, f"{self.task_config['APREFIX']}aerostat")

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.task_config['DATA'], 'diags', 'diag*nc4'))

        # gzip the files first
        for diagfile in diags:
            logger.info(f'Adding {diagfile} to tar file')
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # ---- add increments to RESTART files
        logger.info('Adding increments to RESTART files')
        self._add_fms_cube_sphere_increments()

        # tar up bias correction files
        # NOTE TODO

        # copy files back to COM
        logger.info(f"Copying files to COM based on {self.task_config.AERO_FINALIZE_VARIATIONAL_TMPL}")
        aero_var_final_list = parse_j2yaml(self.task_config.AERO_FINALIZE_VARIATIONAL_TMPL, self.task_config)
        FileHandler(aero_var_final_list).sync()

        # open tar file for writing
        with tarfile.open(aerostat, "w") as archive:
            for diagfile in diags:
                diaggzip = f"{diagfile}.gz"
                archive.add(diaggzip, arcname=os.path.basename(diaggzip))
        logger.info(f'Saved diags to {aerostat}')

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
        incvars_list_path = os.path.join(self.task_config['PARMgfs'], 'gdas', 'aeroanl_inc_vars.yaml')
        incvars = YAMLFile(path=incvars_list_path)['incvars']
        self.add_fv3_increments(inc_template, bkg_template, incvars)

    @logit(logger)
    def add_fv3_increments(self, inc_file_tmpl: str, bkg_file_tmpl: str, incvars: List) -> None:
        """Add cubed-sphere increments to cubed-sphere backgrounds

        Parameters
        ----------
        inc_file_tmpl : str
           template of the FV3 increment file of the form: 'filetype.tile{tilenum}.nc'
        bkg_file_tmpl : str
           template of the FV3 background file of the form: 'filetype.tile{tilenum}.nc'
        incvars : List
           List of increment variables to add to the background
        """

        for itile in range(1, self.task_config.ntiles + 1):
            inc_path = inc_file_tmpl.format(tilenum=itile)
            bkg_path = bkg_file_tmpl.format(tilenum=itile)
            with Dataset(inc_path, mode='r') as incfile, Dataset(bkg_path, mode='a') as rstfile:
                for vname in incvars:
                    increment = incfile.variables[vname][:]
                    bkg = rstfile.variables[vname][:]
                    anl = bkg + increment
                    rstfile.variables[vname][:] = anl[:]
                    try:
                        rstfile.variables[vname].delncattr('checksum')  # remove the checksum so fv3 does not complain
                    except (AttributeError, RuntimeError):
                        pass  # checksum is missing, move on
