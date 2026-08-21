#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Dict, List, Optional, Any
from pprint import pformat
import glob
import gzip
import tarfile
import numpy as np
from netCDF4 import Dataset
from pygfs.task.analysis import Analysis
from pygfs.jedi import Jedi
from wxflow import (AttrDict,
                    FileHandler,
                    to_fv3time, to_YMD, to_YMDH, to_timedelta, add_to_datetime,
                    to_julian,
                    rm_p, cp,
                    parse_j2yaml, save_as_yaml,
                    Jinja,
                    logit,
                    Executable,
                    WorkflowException)

logger = getLogger(__name__.split('.')[-1])


class SoilLetkfAnalysis(Analysis):
    """
    Class for JEDI-based global soil LETKF analysis tasks
    """

    @logit(logger, name="SoilLetkfAnalysis")
    def __init__(self, config: Dict[str, Any]):
        """Constructor global soil LETKF analysis task

        This method will construct a global soil LETKF analysis task.
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

        _res = int(self.task_config['CASE_ENS'][1:])
        _res_anl = int(self.task_config.CASE_ANL[1:])
        _res_his = int(self.task_config.CASE_HIST[1:])

        # Extend task_config with variables repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.task_config.LEVS - 1,
                'npx_his': _res_his + 1,
                'npy_his': _res_his + 1,
                'npz_his': self.task_config.LEVS - 1,
                'CASE': self.task_config.CASE_ENS,
                'soil_bkg_path': os.path.join('.', 'bkg', 'ensmean/'),
                'soil_prepobs_path': os.path.join(self.task_config.DATA, 'prep'),
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Create JEDI object dictionary
        expected_keys = ['soilletkfanl']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global soil LETKF analysis

        This method will initialize a global soil LETKF analysis.
        This includes:
        - stage observation files
        - stage input files from COM and create output directories
        - initialize JEDI applications

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Stage observation files
        logger.info(f"Staging observation files")
        self.jedi_dict['soilletkfanl'].stage_obsdatain(f"{self.task_config.COMIN_OBS}/land") 

        # Stage files from COM
        logger.info(f"Staging files from COM and creating output directories")
        FileHandler(self.task_config.data_in).sync()

        # Initialize JEDI applications
        logger.info(f"Initializing JEDI applications")
        self.jedi_dict['soilletkfanl'].initialize(clean_empty_obsspaces=False)

    # TODO: prepbufr proc

    @logit(logger)
    def execute(self, jedi_dict_key: str) -> None:
        """Run JEDI executable

        This method will run JEDI executables for the global soil analysis

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
        """Performs closing actions of the Soil analysis task
        This method:
        - archive, compress, and save diag files in COM directory
        - save output files and YAMLs to COM

        Parameters
        ----------
        self : Analysis
            Instance of the SoilLetkfAnalysis object
        """

        # Compress and save diag files to COM directory
        logger.info(f"Saving observation diag files to COM")
        self.jedi_dict['soilletkfanl'].save_obsdataout(self.task_config.COMOUT_SOIL_DIAG_ENS,
                                                       f"{self.task_config.RUN}.{to_YMDH(self.task_config.current_cycle)}.soil_analysis.ioda_hofx")

        # Save files to COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()

    # note this doesn't apply for adding soil incs (yet)
    @logit(logger)
    def add_increments(self) -> None:

        """Executes the program "apply_soil_incr.x" to create analysis "sfc_data" files by adding increments to backgrounds

        Parameters
        ----------
        self : Analysis
            Instance of the SoilLetkfAnalysis object
        """

        # backgrounds needed to create analysis (b+inc) already copied to DATA/anl/mem by soil_letkf_config.yaml.j2
        # TODO: update this for csg files
        if self.task_config.DOIAU and not self.task_config.csg_increment:
            logger.info("Copying increments to beginning of window")
            template_in = f'soilinc.{to_fv3time(self.task_config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
            template_out = f'soilinc.{to_fv3time(self.task_config.WINDOW_BEGIN)}.sfc_data.tile{{tilenum}}.nc'
            for mem in range(1, self.task_config.NMEM_ENS + 1):
                inclist = []  # TODO: would taking this out of loop speed things up?
                for itile in range(1, self.task_config.ntiles + 1):
                    filename_in = template_in.format(tilenum=itile)
                    filename_out = template_out.format(tilenum=itile)
                    src = os.path.join(self.task_config.DATA, f'anl/mem{mem:03d}', filename_in)
                    dest = os.path.join(self.task_config.DATA, f'anl/mem{mem:03d}', filename_out)
                    inclist.append([src, dest])
                FileHandler({'copy': inclist}).sync()

        bkgtimes = []
        if self.task_config.DOIAU:
            # need both beginning and middle of window
            bkgtimes.append(self.task_config.WINDOW_BEGIN)
        bkgtimes.append(self.task_config.current_cycle)

        # Add ens increments in parallel
        logger.info(f"Adding increments to {self.task_config.NMEM_ENS} members")
        # loop over times to apply increments
        for bkgtime in bkgtimes:
            logger.info(f"Processing analysis valid: {bkgtime}")
            logger.info(f"Create namelist for APPLY_INCR_EXE")
            nml_template = self.task_config.APPLY_INCR_NML_TMPL
            if self.task_config.csg_increment:
                inc_prefix = f'soilinc_{self.task_config.GPREFIX_ENS}csg_sfc.f006'
            else:
                inc_prefix = self.task_config.INC_PREFIX
            nml_config = {
                'current_cycle': bkgtime,
                'CASE': self.task_config.CASE,
                'DATA': self.task_config.DATA,
                'FIXorog': self.task_config.FIXorog,
                'HOMEglobal': self.task_config.HOMEglobal,
                'OCNRES': self.task_config.OCNRES,
                'ens_size': self.task_config.ens_size,
                'ntiles': self.task_config.ntiles,
                'upd_stc': self.task_config.upd_stc,
                'upd_slc': self.task_config.upd_slc,
                'print_debug': self.task_config.print_debug,
                'lsoil_incr': self.task_config.LSOIL_INCR,
                'inc_prefix': inc_prefix,
                'csg_increment': self.task_config.csg_increment
            }
            nml_data = Jinja(nml_template, nml_config).render
            logger.debug(f"apply_incr_nml:\n{nml_data}")

            nml_file = os.path.join(self.task_config.DATA, "apply_incr_nml")
            if os.path.exists(nml_file):
                rm_p(nml_file)
            with open(nml_file, "w") as fho:
                fho.write(nml_data)

            logger.info("Link APPLY_INCR_EXE into DATA/")
            exe_src = self.task_config.APPLY_INCR_EXE
            exe_dest = os.path.join(self.task_config.DATA, os.path.basename(exe_src))
            if os.path.exists(exe_dest):
                rm_p(exe_dest)
            os.symlink(exe_src, exe_dest)

            # execute APPLY_INCR_EXE to create analysis files
            exe = Executable(self.task_config.APRUN_SOILLETKF_ADDINC)
            exe.add_default_arg(exe_dest)
            logger.info(f"Executing {exe}")
            try:
                logger.debug(f"Executing {exe}")
                exe()
            except OSError:
                logger.exception(f"Failed to execute {exe}")
                raise
            except Exception as err:
                logger.exception(f"An error occurred during execution of {exe}")
                raise WorkflowException(f"An error occurred during execution of {exe}") from err
