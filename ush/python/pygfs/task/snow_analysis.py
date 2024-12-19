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

from wxflow import (AttrDict,
                    FileHandler,
                    to_fv3time, to_YMD, to_YMDH, to_timedelta, add_to_datetime,
                    rm_p, cp,
                    parse_j2yaml, save_as_yaml,
                    Jinja,
                    Task,
                    logit,
                    Executable,
                    WorkflowException)
from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class SnowAnalysis(Task):
    """
    Class for JEDI-based global snow analysis tasks
    """

    @logit(logger, name="SnowAnalysis")
    def __init__(self, config: Dict[str, Any]):
        """Constructor global snow analysis task

        This method will construct a global snow analysis task.
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
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config['assim_freq']}H") / 2)

        # fix ocnres
        self.task_config.OCNRES = f"{self.task_config.OCNRES:03d}"

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'SNOW_WINDOW_BEGIN': _window_begin,
                'SNOW_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'snow_obsdatain_path': os.path.join(self.task_config.DATA, 'obs'),
                'snow_obsdataout_path': os.path.join(self.task_config.DATA, 'diags'),
                'snow_bkg_path': os.path.join('.', 'bkg/'),
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Create JEDI object dictionary
        expected_keys = ['snowanlvar']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global snow analysis

        This method will initialize a global snow analysis.
        This includes:
        - initialize JEDI application
        - staging model backgrounds
        - staging observation files
        - staging FV3-JEDI fix files
        - staging B error files
        - creating output directories

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """
        # initialize JEDI variational application
        logger.info(f"Initializing JEDI variational DA application")
        self.jedi_dict['snowanlvar'].initialize(self.task_config)

        # stage backgrounds
        logger.info(f"Staging background files from {self.task_config.VAR_BKG_STAGING_YAML}")
        bkg_staging_dict = parse_j2yaml(self.task_config.VAR_BKG_STAGING_YAML, self.task_config)
        FileHandler(bkg_staging_dict).sync()
        logger.debug(f"Background files:\n{pformat(bkg_staging_dict)}")

        # stage observations
        logger.info(f"Staging list of observation files generated from JEDI config")
        obs_dict = self.jedi_dict['snowanlvar'].render_jcb(self.task_config, 'snow_obs_staging')
        FileHandler(obs_dict).sync()
        logger.debug(f"Observation files:\n{pformat(obs_dict)}")

        # stage GTS bufr2ioda mapping YAML files
        logger.info(f"Staging GTS bufr2ioda mapping YAML files from {self.task_config.GTS_SNOW_STAGE_YAML}")
        gts_mapping_list = parse_j2yaml(self.task_config.GTS_SNOW_STAGE_YAML, self.task_config)
        FileHandler(gts_mapping_list).sync()

        # stage FV3-JEDI fix files
        logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
        jedi_fix_dict = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
        FileHandler(jedi_fix_dict).sync()
        logger.debug(f"JEDI fix files:\n{pformat(jedi_fix_dict)}")

        # staging B error files
        logger.info("Stage files for static background error")
        berror_staging_dict = parse_j2yaml(self.task_config.BERROR_STAGING_YAML, self.task_config)
        FileHandler(berror_staging_dict).sync()
        logger.debug(f"Background error files:\n{pformat(berror_staging_dict)}")

        # need output dir for diags and anl
        logger.debug("Create empty output [anl, diags] directories to receive output from executable")
        newdirs = [
            os.path.join(self.task_config.DATA, 'anl'),
            os.path.join(self.task_config.DATA, 'diags'),
        ]
        FileHandler({'mkdir': newdirs}).sync()

    @logit(logger)
    def prepare_IMS(self) -> None:
        """Prepare the IMS data for a global snow analysis

        This method will prepare IMS data for a global snow analysis using JEDI.
        This includes:
        - staging model backgrounds
        - processing raw IMS observation data and prepare for conversion to IODA
        - creating IMS snowdepth data in IODA format.

        Parameters
        ----------
        Analysis: parent class for GDAS task

        Returns
        ----------
        None
        """

        # create a temporary dict of all keys needed in this method
        localconf = AttrDict()
        keys = ['DATA', 'current_cycle', 'COMIN_OBS', 'COMIN_ATMOS_RESTART_PREV',
                'OPREFIX', 'CASE', 'OCNRES', 'ntiles', 'FIXgfs']
        for key in keys:
            localconf[key] = self.task_config[key]

        localconf['ims_fcst_path'] = self.task_config['snow_bkg_path']

        # Read and render the IMS_OBS_LIST yaml
        logger.info(f"Reading {self.task_config.IMS_OBS_LIST}")
        prep_ims_config = parse_j2yaml(self.task_config.IMS_OBS_LIST, localconf)
        logger.debug(f"{self.task_config.IMS_OBS_LIST}:\n{pformat(prep_ims_config)}")

        # copy the IMS obs files from COMIN_OBS to DATA/obs
        logger.info("Copying IMS obs for CALCFIMSEXE")
        FileHandler(prep_ims_config.calcfims).sync()

        logger.info("Create namelist for CALCFIMSEXE")
        nml_template = self.task_config.FIMS_NML_TMPL
        nml_data = Jinja(nml_template, localconf).render
        logger.debug(f"fims.nml:\n{nml_data}")

        nml_file = os.path.join(localconf.DATA, "fims.nml")
        with open(nml_file, "w") as fho:
            fho.write(nml_data)

        logger.info("Link CALCFIMSEXE into DATA/")
        exe_src = self.task_config.CALCFIMSEXE
        exe_dest = os.path.join(localconf.DATA, os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # execute CALCFIMSEXE to calculate IMS snowdepth
        exe = Executable(self.task_config.APRUN_CALCFIMS)
        exe.add_default_arg(os.path.join(localconf.DATA, os.path.basename(exe_src)))
        logger.info(f"Executing {exe}")
        try:
            exe()
        except OSError:
            logger.exception(f"Failed to execute {exe}")
            raise
        except Exception as err:
            logger.exception(f"An error occured during execution of {exe}")
            raise WorkflowException(f"An error occured during execution of {exe}") from err

        # Ensure the snow depth IMS file is produced by the above executable
        input_file = f"IMSscf.{to_YMD(localconf.current_cycle)}.{localconf.CASE}_oro_data.nc"
        if not os.path.isfile(f"{os.path.join(localconf.DATA, input_file)}"):
            logger.exception(f"{self.task_config.CALCFIMSEXE} failed to produce {input_file}")
            raise FileNotFoundError(f"{os.path.join(localconf.DATA, input_file)}")

        # Execute imspy to create the IMS obs data in IODA format
        logger.info("Create IMS obs data in IODA format")

        output_file = f"ims_snow_{to_YMDH(localconf.current_cycle)}.nc4"
        if os.path.isfile(f"{os.path.join(localconf.DATA, output_file)}"):
            rm_p(output_file)

        exe = Executable(self.task_config.IMS2IODACONV)
        exe.add_default_arg(["-i", f"{os.path.join(localconf.DATA, input_file)}"])
        exe.add_default_arg(["-o", f"{os.path.join(localconf.DATA, output_file)}"])
        try:
            logger.debug(f"Executing {exe}")
            exe()
        except OSError:
            logger.exception(f"Failed to execute {exe}")
            raise
        except Exception as err:
            logger.exception(f"An error occured during execution of {exe}")
            raise WorkflowException(f"An error occured during execution of {exe}") from err

        # Ensure the IODA snow depth IMS file is produced by the IODA converter
        # If so, copy to DATA/obs/
        if not os.path.isfile(f"{os.path.join(localconf.DATA, output_file)}"):
            logger.exception(f"{self.task_config.IMS2IODACONV} failed to produce {output_file}")
            raise FileNotFoundError(f"{os.path.join(localconf.DATA, output_file)}")
        else:
            logger.info(f"Copy {output_file} to {os.path.join(localconf.DATA, 'obs')}")
            FileHandler(prep_ims_config.ims2ioda).sync()

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
        - tar and gzip the output diag files and place in COM/
        - copy the generated YAML file from initialize to the COM/
        - copy the analysis files to the COM/
        - copy the increment files to the COM/

        Parameters
        ----------
        self : Analysis
            Instance of the SnowAnalysis object
        """

        # ---- tar up diags
        # path of output tar statfile
        snowstat = os.path.join(self.task_config.COMOUT_SNOW_ANALYSIS, f"{self.task_config.APREFIX}snowstat.tgz")

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.task_config.DATA, 'diags', 'diag*nc'))

        logger.info(f"Compressing {len(diags)} diag files to {snowstat}")

        # gzip the files first
        logger.debug(f"Gzipping {len(diags)} diag files")
        for diagfile in diags:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # open tar file for writing
        logger.debug(f"Creating tar file {snowstat} with {len(diags)} gzipped diag files")
        with tarfile.open(snowstat, "w|gz") as archive:
            for diagfile in diags:
                diaggzip = f"{diagfile}.gz"
                archive.add(diaggzip, arcname=os.path.basename(diaggzip))

        # get list of yamls to copy to ROTDIR
        yamls = glob.glob(os.path.join(self.task_config.DATA, '*snow*yaml'))

        # copy full YAML from executable to ROTDIR
        for src in yamls:
            yaml_base = os.path.splitext(os.path.basename(src))[0]
            dest_yaml_name = f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.{yaml_base}.yaml"
            dest = os.path.join(self.task_config.COMOUT_CONF, dest_yaml_name)
            logger.debug(f"Copying {src} to {dest}")
            yaml_copy = {
                'copy': [[src, dest]]
            }
            FileHandler(yaml_copy).sync()

        logger.info("Copy analysis to COM")
        bkgtimes = []
        if self.task_config.DOIAU:
            # need both beginning and middle of window
            bkgtimes.append(self.task_config.SNOW_WINDOW_BEGIN)
        bkgtimes.append(self.task_config.current_cycle)
        anllist = []
        for bkgtime in bkgtimes:
            template = f'{to_fv3time(bkgtime)}.sfc_data.tile{{tilenum}}.nc'
            for itile in range(1, self.task_config.ntiles + 1):
                filename = template.format(tilenum=itile)
                src = os.path.join(self.task_config.DATA, 'anl', filename)
                dest = os.path.join(self.task_config.COMOUT_SNOW_ANALYSIS, filename)
                anllist.append([src, dest])
        FileHandler({'copy': anllist}).sync()

        logger.info('Copy increments to COM')
        template = f'snowinc.{to_fv3time(self.task_config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
        inclist = []
        for itile in range(1, self.task_config.ntiles + 1):
            filename = template.format(tilenum=itile)
            src = os.path.join(self.task_config.DATA, 'anl', filename)
            dest = os.path.join(self.task_config.COMOUT_SNOW_ANALYSIS, filename)
            inclist.append([src, dest])
        FileHandler({'copy': inclist}).sync()

    @logit(logger)
    def add_increments(self) -> None:
        """Executes the program "apply_incr.exe" to create analysis "sfc_data" files by adding increments to backgrounds

        Parameters
        ----------
        self : Analysis
            Instance of the SnowAnalysis object
        """

        # need backgrounds to create analysis from increments after LETKF
        logger.info("Copy backgrounds into anl/ directory for creating analysis from increments")
        bkgtimes = []
        if self.task_config.DOIAU:
            # want analysis at beginning and middle of window
            bkgtimes.append(self.task_config.SNOW_WINDOW_BEGIN)
        bkgtimes.append(self.task_config.current_cycle)
        anllist = []
        for bkgtime in bkgtimes:
            template = f'{to_fv3time(bkgtime)}.sfc_data.tile{{tilenum}}.nc'
            for itile in range(1, self.task_config.ntiles + 1):
                filename = template.format(tilenum=itile)
                src = os.path.join(self.task_config.COMIN_ATMOS_RESTART_PREV, filename)
                dest = os.path.join(self.task_config.DATA, "anl", filename)
                anllist.append([src, dest])
        FileHandler({'copy': anllist}).sync()

        if self.task_config.DOIAU:
            logger.info("Copying increments to beginning of window")
            template_in = f'snowinc.{to_fv3time(self.task_config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
            template_out = f'snowinc.{to_fv3time(self.task_config.SNOW_WINDOW_BEGIN)}.sfc_data.tile{{tilenum}}.nc'
            inclist = []
            for itile in range(1, self.task_config.ntiles + 1):
                filename_in = template_in.format(tilenum=itile)
                filename_out = template_out.format(tilenum=itile)
                src = os.path.join(self.task_config.DATA, 'anl', filename_in)
                dest = os.path.join(self.task_config.DATA, 'anl', filename_out)
                inclist.append([src, dest])
            FileHandler({'copy': inclist}).sync()

        # loop over times to apply increments
        for bkgtime in bkgtimes:
            logger.info("Processing analysis valid: {bkgtime}")
            logger.info("Create namelist for APPLY_INCR_EXE")
            nml_template = self.task_config.APPLY_INCR_NML_TMPL
            nml_config = {
                'current_cycle': bkgtime,
                'CASE': self.task_config.CASE,
                'DATA': self.task_config.DATA,
                'HOMEgfs': self.task_config.HOMEgfs,
                'OCNRES': self.task_config.OCNRES,
            }
            nml_data = Jinja(nml_template, nml_config).render
            logger.debug(f"apply_incr_nml:\n{nml_data}")

            nml_file = os.path.join(self.task_config.DATA, "apply_incr_nml")
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
            exe.add_default_arg(os.path.join(self.task_config.DATA, os.path.basename(exe_src)))
            logger.info(f"Executing {exe}")
            try:
                exe()
            except OSError:
                logger.exception(f"Failed to execute {exe}")
                raise
            except Exception as err:
                logger.exception(f"An error occured during execution of {exe}")
                raise WorkflowException(f"An error occured during execution of {exe}") from err
