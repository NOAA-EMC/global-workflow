#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from logging import getLogger
from pprint import pformat
from typing import Optional, Dict, Any

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_fv3time, to_timedelta, to_YMDH,
                    Task,
                    parse_j2yaml, save_as_yaml,
                    logit)
from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class AtmAnalysis(Task):
    """
    Class for JEDI-based global atm analysis tasks
    """
    @logit(logger, name="AtmAnalysis")
    def __init__(self, config: Dict[str, Any], yaml_name: Optional[str] = None):
        """Constructor global atm analysis task

        This method will construct a global atm analysis task.
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task
        - instantiate the Jedi attribute object

        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration
        yaml_name: str, optional
            name of YAML file for JEDI configuration

        Returns
        ----------
        None
        """
        super().__init__(config)

        _res = int(self.task_config.CASE[1:])
        _res_anl = int(self.task_config.CASE_ANL[1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.task_config.LEVS - 1,
                'ATM_WINDOW_BEGIN': _window_begin,
                'ATM_WINDOW_LENGTH': f"PT{self.task_config.assim_freq}H",
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'atm_obsdatain_path': f"{self.task_config.DATA}/obs/",
                'atm_obsdataout_path': f"{self.task_config.DATA}/diags/",
                'BKG_TSTEP': "PT1H"  # Placeholder for 4D applications
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Create JEDI object
        self.jedi = Jedi(self.task_config, yaml_name)

    @logit(logger)
    def initialize_jedi(self):
        """Initialize JEDI application

        This method will initialize a JEDI application used in the global atm analysis.
        This includes:
        - generating and saving JEDI YAML config
        - linking the JEDI executable

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # get JEDI-to-FV3 increment converter config and save to YAML file
        logger.info(f"Generating JEDI YAML config: {self.jedi.yaml}")
        self.jedi.set_config(self.task_config)
        logger.debug(f"JEDI config:\n{pformat(self.jedi.config)}")

        # save JEDI config to YAML file
        logger.debug(f"Writing JEDI YAML config to: {self.jedi.yaml}")
        save_as_yaml(self.jedi.config, self.jedi.yaml)

        # link JEDI executable
        logger.info(f"Linking JEDI executable {self.task_config.JEDIEXE} to {self.jedi.exe}")
        self.jedi.link_exe(self.task_config)

    @logit(logger)
    def initialize_analysis(self) -> None:
        """Initialize a global atm analysis

        This method will initialize a global atm analysis.
        This includes:
        - staging observation files
        - staging bias correction files
        - staging CRTM fix files
        - staging FV3-JEDI fix files
        - staging B error files
        - staging model backgrounds
        - creating output directories

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """
        super().initialize()

        # stage observations
        logger.info(f"Staging list of observation files generated from JEDI config")
        obs_dict = self.jedi.get_obs_dict(self.task_config)
        FileHandler(obs_dict).sync()
        logger.debug(f"Observation files:\n{pformat(obs_dict)}")

        # stage bias corrections
        logger.info(f"Staging list of bias correction files generated from JEDI config")
        self.task_config.VarBcDir = f"{self.task_config.COM_ATMOS_ANALYSIS_PREV}"
        bias_file = f"rad_varbc_params.tar"
        bias_dict = self.jedi.get_bias_dict(self.task_config, bias_file)
        FileHandler(bias_dict).sync()
        logger.debug(f"Bias correction files:\n{pformat(bias_dict)}")

        # extract bias corrections
        tar_file = os.path.join(self.task_config.DATA, 'obs', f"{self.task_config.GPREFIX}{bias_file}")
        logger.info(f"Extract bias correction files from {tar_file}")
        self.jedi.extract_tar(tar_file)

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

        # stage static background error files, otherwise it will assume ID matrix
        logger.info(f"Stage files for STATICB_TYPE {self.task_config.STATICB_TYPE}")
        if self.task_config.STATICB_TYPE != 'identity':
            berror_staging_dict = parse_j2yaml(self.task_config.BERROR_STAGING_YAML, self.task_config)
        else:
            berror_staging_dict = {}
        FileHandler(berror_staging_dict).sync()
        logger.debug(f"Background error files:\n{pformat(berror_staging_dict)}")

        # stage ensemble files for use in hybrid background error
        if self.task_config.DOHYBVAR:
            logger.debug(f"Stage ensemble files for DOHYBVAR {self.task_config.DOHYBVAR}")
            fv3ens_staging_dict = parse_j2yaml(self.task_config.FV3ENS_STAGING_YAML, self.task_config)
            FileHandler(fv3ens_staging_dict).sync()
            logger.debug(f"Ensemble files:\n{pformat(fv3ens_staging_dict)}")

        # stage backgrounds
        logger.info(f"Staging background files from {self.task_config.VAR_BKG_STAGING_YAML}")
        bkg_staging_dict = parse_j2yaml(self.task_config.VAR_BKG_STAGING_YAML, self.task_config)
        FileHandler(bkg_staging_dict).sync()
        logger.debug(f"Background files:\n{pformat(bkg_staging_dict)}")

        # need output dir for diags and anl
        logger.debug("Create empty output [anl, diags] directories to receive output from executable")
        newdirs = [
            os.path.join(self.task_config.DATA, 'anl'),
            os.path.join(self.task_config.DATA, 'diags'),
        ]
        FileHandler({'mkdir': newdirs}).sync()

    @logit(logger)
    def execute(self, aprun_cmd: str, jedi_args: Optional[str] = None) -> None:
        """Run JEDI executable

        This method will run JEDI executables for the global atm analysis

        Parameters
        ----------
        aprun_cmd : str
           Run command for JEDI application on HPC system
        jedi_args : List
           List of additional optional arguments for JEDI application

        Returns
        ----------
        None
        """

        if jedi_args:
            logger.info(f"Executing {self.jedi.exe} {' '.join(jedi_args)} {self.jedi.yaml}")
        else:
            logger.info(f"Executing {self.jedi.exe} {self.jedi.yaml}")

        self.jedi.execute(self.task_config, aprun_cmd, jedi_args)

    @logit(logger)
    def finalize(self) -> None:
        """Finalize a global atm analysis

        This method will finalize a global atm analysis using JEDI.
        This includes:
        - tar output diag files and place in ROTDIR
        - copy the generated YAML file from initialize to the ROTDIR
        - copy the updated bias correction files to ROTDIR

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # ---- tar up diags
        # path of output tar statfile
        atmstat = os.path.join(self.task_config.COM_ATMOS_ANALYSIS, f"{self.task_config.APREFIX}atmstat")

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.task_config.DATA, 'diags', 'diag*nc'))

        logger.info(f"Compressing {len(diags)} diag files to {atmstat}.gz")

        # gzip the files first
        logger.debug(f"Gzipping {len(diags)} diag files")
        for diagfile in diags:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # open tar file for writing
        logger.debug(f"Creating tar file {atmstat} with {len(diags)} gzipped diag files")
        with tarfile.open(atmstat, "w") as archive:
            for diagfile in diags:
                diaggzip = f"{diagfile}.gz"
                archive.add(diaggzip, arcname=os.path.basename(diaggzip))

        # get list of yamls to copy to ROTDIR
        yamls = glob.glob(os.path.join(self.task_config.DATA, '*atm*yaml'))

        # copy full YAML from executable to ROTDIR
        for src in yamls:
            yaml_base = os.path.splitext(os.path.basename(src))[0]
            dest_yaml_name = f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.{yaml_base}.yaml"
            dest = os.path.join(self.task_config.COM_ATMOS_ANALYSIS, dest_yaml_name)
            logger.debug(f"Copying {src} to {dest}")
            yaml_copy = {
                'copy': [[src, dest]]
            }
            FileHandler(yaml_copy).sync()

        # path of output radiance bias correction tarfile
        bfile = f"{self.task_config.APREFIX}rad_varbc_params.tar"
        radtar = os.path.join(self.task_config.COM_ATMOS_ANALYSIS, bfile)

        # rename and copy tlapse radiance bias correction files from obs to bc
        tlapobs = glob.glob(os.path.join(self.task_config.DATA, 'obs', '*tlapse.txt'))
        copylist = []
        for tlapfile in tlapobs:
            obsfile = os.path.basename(tlapfile).split('.', 2)
            newfile = f"{self.task_config.APREFIX}{obsfile[2]}"
            copylist.append([tlapfile, os.path.join(self.task_config.DATA, 'bc', newfile)])
        tlapse_dict = {
            'copy': copylist
        }
        FileHandler(tlapse_dict).sync()

        # get lists of radiance bias correction files to add to tarball
        satlist = glob.glob(os.path.join(self.task_config.DATA, 'bc', '*satbias*nc'))
        tlaplist = glob.glob(os.path.join(self.task_config.DATA, 'bc', '*tlapse.txt'))

        # tar radiance bias correction files to ROTDIR
        logger.info(f"Creating radiance bias correction tar file {radtar}")
        with tarfile.open(radtar, 'w') as radbcor:
            for satfile in satlist:
                radbcor.add(satfile, arcname=os.path.basename(satfile))
            for tlapfile in tlaplist:
                radbcor.add(tlapfile, arcname=os.path.basename(tlapfile))
            logger.info(f"Add {radbcor.getnames()}")

        # Copy FV3 atm increment to comrot directory
        logger.info("Copy UFS model readable atm increment file")
        cdate = to_fv3time(self.task_config.current_cycle)
        cdate_inc = cdate.replace('.', '_')
        src = os.path.join(self.task_config.DATA, 'anl', f"atminc.{cdate_inc}z.nc4")
        dest = os.path.join(self.task_config.COM_ATMOS_ANALYSIS, f'{self.task_config.RUN}.t{self.task_config.cyc:02d}z.atminc.nc')
        logger.debug(f"Copying {src} to {dest}")
        inc_copy = {
            'copy': [[src, dest]]
        }
        FileHandler(inc_copy).sync()

    def clean(self):
        super().clean()
