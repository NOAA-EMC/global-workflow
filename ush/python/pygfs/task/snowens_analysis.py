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


class SnowEnsAnalysis(Analysis):
    """
    Class for JEDI-based global snow ensemble analysis tasks
    """

    @logit(logger, name="SnowEnsAnalysis")
    def __init__(self, config: Dict[str, Any]):
        """Constructor global snow ensemble analysis task

        This method will construct a global snow ensemble analysis task.
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

        # if 00z, do SCF preprocessing
        _ims_file = os.path.join(self.task_config.COMIN_OBS, f'{self.task_config.OPREFIX}imssnow96.asc')
        logger.info(f"Checking for IMS file: {_ims_file}")
        if self.task_config.cyc == 0 and os.path.exists(_ims_file):
            _DO_IMS_SCF = True
        else:
            _DO_IMS_SCF = False

        # Check if SNOCVR or SNOMAD file exists, do SNOCVR_SNOMAD preprocessing
        _snocvr_file = os.path.join(self.task_config.COMIN_OBS, f'{self.task_config.OPREFIX}snocvr.tm00.bufr_d')
        _snomad_file = os.path.join(self.task_config.COMIN_OBS, f'{self.task_config.OPREFIX}snomad.tm00.bufr_d')
        _DO_SNOCVR_SNOMAD = (
            "snocvr_snomad" in self.task_config.observations and
            (os.path.exists(_snocvr_file) or os.path.exists(_snomad_file))
        )

        # Extend task_config with variables repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'CASE': self.task_config.CASE_ENS,
                'snow_bkg_path': os.path.join('.', 'bkg', 'ensmean/'),
                'ims_file': _ims_file,
                'DO_IMS_SCF': _DO_IMS_SCF,  # Boolean to decide if IMS snow cover processing is done
                'DO_SNOCVR_SNOMAD': _DO_SNOCVR_SNOMAD,  # Boolean to decide if SNOCVR_SNOMAD processing is done
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Create JEDI object dictionary
        expected_keys = ['scf_to_ioda', 'snowanlvar', 'esnowanlensmean']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global snow ensemble analysis

        This method will initialize a global snow ensemble analysis.
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
        logger.info(f"Staging files from COM and creating output directories")
        FileHandler(self.task_config.data_in).sync()

        # Initialize JEDI applications
        logger.info(f"Initializing JEDI applications")
        self.jedi_dict['snowanlvar'].initialize(self.task_config, clean_empty_obsspaces=False)
        self.jedi_dict['esnowanlensmean'].initialize(self.task_config)
        if self.task_config.DO_IMS_SCF:
            self.jedi_dict['scf_to_ioda'].initialize(self.task_config)

    @logit(logger)
    def execute(self, jedi_dict_key: str) -> None:
        """Run JEDI executable

        This method will run JEDI executables for the global snow analysis

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
        """Performs closing actions of the Snow analysis task
        This method:
        - compress and tar output diag files in COM
        - save output files and YAMLs to COM

        Parameters
        ----------
        self : Analysis
            Instance of the SnowEnsAnalysis object
        """

        # Compress and tar diag files into COM directory
        self.tar_diag_files(self.task_config.COMOUT_SNOW_ANALYSIS,
                            f"{self.task_config.APREFIX_ENS}snowstat.tgz")

        # Save files to COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()

    @logit(logger)
    def prepare_SNOCVR_SNOMAD(self) -> None:
        """Prepare the combined SNOCVR and SNOMAD data for a global snow analysis
        This includes:
        - creating combined SNOCVR and SNOMAD snowdepth data in IODA format.
        Parameters
        ----------
        self : Analysis
            Instance of the SnowAnalysis object
        Returns
        ----------
        None
        """

        # Read and render the prep_snocvr_snomad.yaml.j2
        logger.info(f"Reading {self.task_config.PREP_SNOCVR_SNOMAD_YAML}")
        prep_snocvr_snomad_config = parse_j2yaml(self.task_config.PREP_SNOCVR_SNOMAD_YAML, self.task_config)
        logger.debug(f"{self.task_config.PREP_SNOCVR_SNOMAD_YAML}:\n{pformat(prep_snocvr_snomad_config)}")

        # define these locations in gdas/snow/prep/prep_snocvr_snomad.yaml.j2
        logger.info("Copying SNOCVR and SNOMAD obs to DATA")
        FileHandler(prep_snocvr_snomad_config.stage).sync()

        # Execute obsBuilder to create the combined snocvr and snomad in IODA format
        logger.info("Create the combined snocvr and snomad data in IODA format")

        input_snocvr = f'{self.task_config.OPREFIX}snocvr.tm00.bufr_d'
        input_snomad = f'{self.task_config.OPREFIX}snomad.tm00.bufr_d'
        output_file = f'{self.task_config.OPREFIX}snocvr_snomad.tm00.nc'
        if os.path.exists(f"{os.path.join(self.task_config.DATA, output_file)}"):
            rm_p(output_file)

        logger.info("Link OBSBUILDER into DATA/")
        exe_src = self.task_config.OBSBUILDER
        exe_dest = os.path.join(self.task_config.DATA, os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        exe = Executable(exe_dest)
        if os.path.exists(input_snocvr):
            exe.add_default_arg(["--input_snocvr", f"{os.path.join(self.task_config.DATA, input_snocvr)}"])
        exe.add_default_arg(["--output", f"{os.path.join(self.task_config.DATA, output_file)}"])
        if os.path.exists(input_snomad):
            exe.add_default_arg(["--input_snomad", f"{os.path.join(self.task_config.DATA, input_snomad)}"])
        try:
            logger.debug(f"Executing {exe}")
            exe()
        except OSError:
            logger.exception(f"Failed to execute {exe}")
            raise
        except Exception as err:
            logger.exception(f"An error occured during execution of {exe}")
            raise WorkflowException(f"An error occured during execution of {exe}") from err

        # Ensure the IODA snow depth SNOCVR+SNOMAD file is produced by the obsBuilder
        # If so, copy to DATA/prep/
        if not os.path.isfile(f"{os.path.join(self.task_config.DATA, output_file)}"):
            logger.warning(f"{output_file} not produced - continuing without it.")
        else:
            logger.info(f"Copy {output_file} successfully generated")
            FileHandler(prep_snocvr_snomad_config.netcdf).sync()

    @logit(logger)
    def add_increments(self) -> None:
        """Executes the program "apply_incr.exe" to create analysis "sfc_data" files by adding increments to backgrounds

        Parameters
        ----------
        self : Analysis
            Instance of the SnowEnsAnalysis object
        """

        if self.task_config.DOIAU:
            logger.info("Copying increments to beginning of window")
            template_in = f'snowinc.{to_fv3time(self.task_config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
            template_out = f'snowinc.{to_fv3time(self.task_config.WINDOW_BEGIN)}.sfc_data.tile{{tilenum}}.nc'
            inclist = []
            for itile in range(1, self.task_config.ntiles + 1):
                filename_in = template_in.format(tilenum=itile)
                filename_out = template_out.format(tilenum=itile)
                src = os.path.join(self.task_config.DATA, 'anl', filename_in)
                dest = os.path.join(self.task_config.DATA, 'anl', filename_out)
                inclist.append([src, dest])
            FileHandler({'copy': inclist}).sync()

        bkgtimes = []
        if self.task_config.DOIAU:
            # need both beginning and middle of window
            bkgtimes.append(self.task_config.WINDOW_BEGIN)
        bkgtimes.append(self.task_config.current_cycle)

        # loop over members
        # TODO, make this better, or rewrite code to run in parallel
        for mem in range(1, self.task_config.NMEM_ENS + 1):
            logger.info(f"Processing member mem{mem:03d}")
            # loop over times to apply increments
            for bkgtime in bkgtimes:
                logger.info(f"Processing analysis valid: {bkgtime}")
                logger.info("Create namelist for APPLY_INCR_EXE")
                nml_template = self.task_config.ENS_APPLY_INCR_NML_TMPL
                nml_config = {
                    'current_cycle': bkgtime,
                    'CASE': self.task_config.CASE,
                    'DATA': self.task_config.DATA,
                    'HOMEgfs': self.task_config.HOMEgfs,
                    'OCNRES': self.task_config.OCNRES,
                    'MYMEM': f"{mem:03d}",
                    'CASE_ENS': self.task_config.CASE_ENS,
                    'ens_size': self.task_config.ens_size,
                    'ntiles': self.task_config.ntiles,
                    'noincr_threshold': self.task_config.noincr_threshold,
                    'print_debug': self.task_config.print_debug,
                    'truncate_incr': self.task_config.truncate_incr
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
                exe = Executable(self.task_config.APRUN_APPLY_INCR)
                exe.add_default_arg(exe_dest)
                logger.info(f"Executing {exe}")
                try:
                    logger.debug(f"Executing {exe}")
                    exe()
                except OSError:
                    logger.exception(f"Failed to execute {exe}")
                    raise
                except Exception as err:
                    logger.exception(f"An error occured during execution of {exe}")
                    raise WorkflowException(f"An error occured during execution of {exe}") from err
