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
                    add_to_datetime, to_fv3time, to_timedelta, to_YMDH, to_YMD,
                    chdir,
                    Task,
                    parse_j2yaml, save_as_yaml,
                    logit,
                    Executable,
                    WorkflowException,
                    Template, TemplateConstants)
from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class AtmEnsAnalysis(Task):
    """
    Class for JEDI-based global atmens analysis tasks
    """
    @logit(logger, name="AtmEnsAnalysis")
    def __init__(self, config: Dict[str, Any], yaml_name: Optional[str] = None):
        """Constructor global atmens analysis task

        This method will construct a global atmens analysis task.
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

        _res = int(self.task_config.CASE_ENS[1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'ATM_WINDOW_BEGIN': _window_begin,
                'ATM_WINDOW_LENGTH': f"PT{self.task_config.assim_freq}H",
                'OPREFIX': f"{self.task_config.EUPD_CYC}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'atm_obsdatain_path': f"./obs/",
                'atm_obsdataout_path': f"./diags/",
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

        This method will initialize a JEDI application used in the global atmens analysis.
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

        # get JEDI config and save to YAML file
        logger.info(f"Generating JEDI config: {self.jedi.yaml}")
        self.jedi.set_config(self.task_config)
        logger.debug(f"JEDI config:\n{pformat(self.jedi.config)}")

        # save JEDI config to YAML file
        logger.info(f"Writing JEDI config to YAML file: {self.jedi.yaml}")
        save_as_yaml(self.jedi.config, self.jedi.yaml)

        # link JEDI-to-FV3 increment converter executable
        logger.info(f"Linking JEDI executable {self.task_config.JEDIEXE} to {self.jedi.exe}")
        self.jedi.link_exe(self.task_config)

    @logit(logger)
    def initialize_analysis(self) -> None:
        """Initialize a global atmens analysis

        This method will initialize a global atmens analysis.
        This includes:
        - staging observation files
        - staging bias correction files
        - staging CRTM fix files
        - staging FV3-JEDI fix files
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

        # stage backgrounds
        logger.info(f"Stage ensemble member background files")
        bkg_staging_dict = parse_j2yaml(self.task_config.LGETKF_BKG_STAGING_YAML, self.task_config)
        FileHandler(bkg_staging_dict).sync()
        logger.debug(f"Ensemble member background files:\n{pformat(bkg_staging_dict)}")

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

        This method will run JEDI executables for the global atmens analysis

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
        """Finalize a global atmens analysis

        This method will finalize a global atmens analysis using JEDI.
        This includes:
        - tar output diag files and place in ROTDIR
        - copy the generated YAML file from initialize to the ROTDIR

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # ---- tar up diags
        # path of output tar statfile
        atmensstat = os.path.join(self.task_config.COM_ATMOS_ANALYSIS_ENS, f"{self.task_config.APREFIX}atmensstat")

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.task_config.DATA, 'diags', 'diag*nc'))

        logger.info(f"Compressing {len(diags)} diag files to {atmensstat}.gz")

        # gzip the files first
        logger.debug(f"Gzipping {len(diags)} diag files")
        for diagfile in diags:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # open tar file for writing
        logger.debug(f"Creating tar file {atmensstat} with {len(diags)} gzipped diag files")
        with tarfile.open(atmensstat, "w") as archive:
            for diagfile in diags:
                diaggzip = f"{diagfile}.gz"
                archive.add(diaggzip, arcname=os.path.basename(diaggzip))

        # get list of yamls to cop to ROTDIR
        yamls = glob.glob(os.path.join(self.task_config.DATA, '*atmens*yaml'))

        # copy full YAML from executable to ROTDIR
        for src in yamls:
            logger.info(f"Copying {src} to {self.task_config.COM_ATMOS_ANALYSIS_ENS}")
            yaml_base = os.path.splitext(os.path.basename(src))[0]
            dest_yaml_name = f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.{yaml_base}.yaml"
            dest = os.path.join(self.task_config.COM_ATMOS_ANALYSIS_ENS, dest_yaml_name)
            logger.debug(f"Copying {src} to {dest}")
            yaml_copy = {
                'copy': [[src, dest]]
            }
            FileHandler(yaml_copy).sync()

        # create template dictionaries
        template_inc = self.task_config.COM_ATMOS_ANALYSIS_TMPL
        tmpl_inc_dict = {
            'ROTDIR': self.task_config.ROTDIR,
            'RUN': self.task_config.RUN,
            'YMD': to_YMD(self.task_config.current_cycle),
            'HH': self.task_config.current_cycle.strftime('%H')
        }

        # copy FV3 atm increment to comrot directory
        logger.info("Copy UFS model readable atm increment file")
        cdate = to_fv3time(self.task_config.current_cycle)
        cdate_inc = cdate.replace('.', '_')

        # loop over ensemble members
        for imem in range(1, self.task_config.NMEM_ENS + 1):
            memchar = f"mem{imem:03d}"

            # create output path for member analysis increment
            tmpl_inc_dict['MEMDIR'] = memchar
            incdir = Template.substitute_structure(template_inc, TemplateConstants.DOLLAR_CURLY_BRACE, tmpl_inc_dict.get)
            src = os.path.join(self.task_config.DATA, 'anl', memchar, f"atminc.{cdate_inc}z.nc4")
            dest = os.path.join(incdir, f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.atminc.nc")

            # copy increment
            logger.debug(f"Copying {src} to {dest}")
            inc_copy = {
                'copy': [[src, dest]]
            }
            FileHandler(inc_copy).sync()

    def clean(self):
        super().clean()
