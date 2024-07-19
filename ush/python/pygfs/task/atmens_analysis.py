#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from logging import getLogger
from typing import Dict, List

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_fv3time, to_timedelta, to_YMDH, to_YMD,
                    chdir,
                    parse_j2yaml, save_as_yaml,
                    logit,
                    Executable,
                    WorkflowException,
                    Template, TemplateConstants)
from pygfs.task.jedi import JEDI

logger = getLogger(__name__.split('.')[-1])


class AtmEnsAnalysis(JEDI):
    """
    Class for global atmens analysis tasks
    """
    @logit(logger, name="AtmEnsAnalysis")
    def __init__(self: AtmEnsAnalysis, config):
        super().__init__(config)

        _res = int(self.task_config.CASE_ENS[1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'jedi_args': ['fv3jedi', 'localensembleda']
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

    @logit(logger)
    def initialize(self: AtmEnsAnalysis) -> None:
        """Initialize a global atmens analysis

        This method will initialize a global atmens analysis using JEDI.
        This includes:
        - staging CRTM fix files
        - staging FV3-JEDI fix files
        - staging model backgrounds
        - creating output directories

        Parameters
        ----------
        Analysis: parent class for GDAS task

        Returns
        ----------
        None
        """
        super().initialize()

        # stage CRTM fix files
        logger.info(f"Staging CRTM fix files from {self.task_config.CRTM_FIX_YAML}")
        crtm_fix_list = parse_j2yaml(self.task_config.CRTM_FIX_YAML, self.task_config)
        FileHandler(crtm_fix_list).sync()

        # stage fix files
        logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
        jedi_fix_list = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage backgrounds
        logger.info(f"Stage ensemble member background files")
        bkg_staging_dict = parse_j2yaml(self.task_config.LGETKF_BKG_STAGING_YAML, self.task_config)
        FileHandler(bkg_staging_dict).sync()

        # need output dir for diags and anl
        logger.debug("Create empty output [anl, diags] directories to receive output from executable")
        newdirs = [
            os.path.join(self.task_config.DATA, 'anl'),
            os.path.join(self.task_config.DATA, 'diags'),
        ]
        FileHandler({'mkdir': newdirs}).sync()

    @logit(logger)
    def execute(self: AtmEnsAnalysis, aprun_cmd: str) -> None:
        super().execute(aprun_cmd)

    @logit(logger)
    def finalize(self: AtmEnsAnalysis) -> None:
        """Finalize a global atmens analysis

        This method will finalize a global atmens analysis using JEDI.
        This includes:
        - tar output diag files and place in ROTDIR
        - copy the generated YAML file from initialize to the ROTDIR

        Parameters
        ----------
        Analysis: parent class for GDAS task

        Returns
        ----------
        None
        """
        super().finalize()
        
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

        # copy full YAML from executable to ROTDIR
        logger.info(f"Copying {self.task_config.jedi_yaml} to {self.task_config.COM_ATMOS_ANALYSIS_ENS}")
        src = os.path.join(self.task_config.DATA, f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.atmens.yaml")
        dest = os.path.join(self.task_config.COM_ATMOS_ANALYSIS_ENS, f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.atmens.yaml")
        logger.debug(f"Copying {src} to {dest}")
        yaml_copy = {
            'mkdir': [self.task_config.COM_ATMOS_ANALYSIS_ENS],
            'copy': [[src, dest]]
        }
        FileHandler(yaml_copy).sync()

    def clean(self: AtmEnsAnalysis):
        super().clean()
