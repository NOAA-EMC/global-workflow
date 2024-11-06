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
    def __init__(self, config: Dict[str, Any]):
        """Constructor global atmens analysis task

        This method will construct a global atmens analysis task.
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task
        - instantiate the Jedi attribute objects

        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration

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

        # Create dictionary of JEDI objects
        expected_keys = ['atmensanlobs', 'atmensanlsol', 'atmensanlfv3inc', 'atmensanlletkf']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global atmens analysis

        This method will initialize a global atmens analysis.
        This includes:
        - initialize JEDI applications
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

        # initialize JEDI LETKF observer application
        logger.info(f"Initializing JEDI LETKF observer application")
        self.jedi_dict['atmensanlobs'].initialize(self.task_config)

        # initialize JEDI LETKF solver application
        logger.info(f"Initializing JEDI LETKF solver application")
        self.jedi_dict['atmensanlsol'].initialize(self.task_config)

        # initialize JEDI FV3 increment conversion application
        logger.info(f"Initializing JEDI FV3 increment conversion application")
        self.jedi_dict['atmensanlfv3inc'].initialize(self.task_config)

        # stage observations
        logger.info(f"Staging list of observation files")
        obs_dict = self.jedi_dict['atmensanlobs'].render_jcb(self.task_config, 'atm_obs_staging')
        FileHandler(obs_dict).sync()
        logger.debug(f"Observation files:\n{pformat(obs_dict)}")

        # stage bias corrections
        logger.info(f"Staging list of bias correction files")
        bias_dict = self.jedi_dict['atmensanlobs'].render_jcb(self.task_config, 'atm_bias_staging')
        bias_dict['copy'] = Jedi.remove_redundant(bias_dict['copy'])
        FileHandler(bias_dict).sync()
        logger.debug(f"Bias correction files:\n{pformat(bias_dict)}")

        # extract bias corrections
        Jedi.extract_tar_from_filehandler_dict(bias_dict)

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
    def initialize_letkf(self) -> None:
        """Initialize a global atmens analysis

        Note: This would normally be done in AtmEnsAnalysis.initialize(), but that method
              now initializes the split observer-solver. This method is just for testing.

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        self.jedi_dict['atmensanlletkf'].initialize(self.task_config)

    @logit(logger)
    def execute(self, jedi_dict_key: str) -> None:
        """Execute JEDI application of atmens analysis

        Parameters
        ----------
        jedi_dict_key
            key specifying a particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        self.jedi_dict[jedi_dict_key].execute()

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
