#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from logging import getLogger
from typing import Dict, List, Any

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_fv3time, to_timedelta, to_YMDH,
                    chdir,
                    parse_j2yaml, save_as_yaml,
                    logit,
                    Executable,
                    WorkflowException)
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class AtmAnalysis(Analysis):
    """
    Class for global atm analysis tasks
    """
    @logit(logger, name="AtmAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _res = int(self.task_config.CASE[1:])
        _res_anl = int(self.task_config.CASE_ANL[1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)
        _jedi_yaml = os.path.join(self.task_config.DATA, f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.atmvar.yaml")

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
                'jedi_yaml': _jedi_yaml,
                'atm_obsdatain_path': f"{self.task_config.DATA}/obs/",
                'atm_obsdataout_path': f"{self.task_config.DATA}/diags/",
                'BKG_TSTEP': "PT1H"  # Placeholder for 4D applications
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def initialize(self: Analysis) -> None:
        """Initialize a global atm analysis

        This method will initialize a global atm analysis using JEDI.
        This includes:
        - staging CRTM fix files
        - staging FV3-JEDI fix files
        - staging B error files
        - staging model backgrounds
        - generating a YAML file for the JEDI executable
        - creating output directories
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

        # stage static background error files, otherwise it will assume ID matrix
        logger.info(f"Stage files for STATICB_TYPE {self.task_config.STATICB_TYPE}")
        if self.task_config.STATICB_TYPE != 'identity':
            berror_staging_dict = parse_j2yaml(self.task_config.BERROR_STAGING_YAML, self.task_config)
        else:
            berror_staging_dict = {}
        FileHandler(berror_staging_dict).sync()

        # stage ensemble files for use in hybrid background error
        if self.task_config.DOHYBVAR:
            logger.debug(f"Stage ensemble files for DOHYBVAR {self.task_config.DOHYBVAR}")
            fv3ens_staging_dict = parse_j2yaml(self.task_config.FV3ENS_STAGING_YAML, self.task_config)
            FileHandler(fv3ens_staging_dict).sync()

        # stage backgrounds
        logger.info(f"Staging background files from {self.task_config.VAR_BKG_STAGING_YAML}")
        bkg_staging_dict = parse_j2yaml(self.task_config.VAR_BKG_STAGING_YAML, self.task_config)
        FileHandler(bkg_staging_dict).sync()

        # generate variational YAML file
        logger.debug(f"Generate variational YAML file: {self.task_config.jedi_yaml}")
        save_as_yaml(self.task_config.jedi_config, self.task_config.jedi_yaml)
        logger.info(f"Wrote variational YAML to: {self.task_config.jedi_yaml}")

        # need output dir for diags and anl
        logger.debug("Create empty output [anl, diags] directories to receive output from executable")
        newdirs = [
            os.path.join(self.task_config.DATA, 'anl'),
            os.path.join(self.task_config.DATA, 'diags'),
        ]
        FileHandler({'mkdir': newdirs}).sync()

    @logit(logger)
    def variational(self: Analysis) -> None:

        chdir(self.task_config.DATA)

        exec_cmd = Executable(self.task_config.APRUN_ATMANLVAR)
        exec_name = os.path.join(self.task_config.DATA, 'gdas.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('fv3jedi')
        exec_cmd.add_default_arg('variational')
        exec_cmd.add_default_arg(self.task_config.jedi_yaml)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

        pass

    @logit(logger)
    def init_fv3_increment(self: Analysis) -> None:
        # Setup JEDI YAML file
        self.task_config.jedi_yaml = os.path.join(self.task_config.DATA,
                                                  f"{self.task_config.JCB_ALGO}.yaml")
        save_as_yaml(self.get_jedi_config(self.task_config.JCB_ALGO), self.task_config.jedi_yaml)

        # Link JEDI executable to run directory
        self.task_config.jedi_exe = self.link_jediexe()

    @logit(logger)
    def fv3_increment(self: Analysis) -> None:
        # Run executable
        exec_cmd = Executable(self.task_config.APRUN_ATMANLFV3INC)
        exec_cmd.add_default_arg(self.task_config.jedi_exe)
        exec_cmd.add_default_arg(self.task_config.jedi_yaml)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

    @logit(logger)
    def finalize(self: Analysis) -> None:
        """Finalize a global atm analysis

        This method will finalize a global atm analysis using JEDI.
        This includes:
        - tar output diag files and place in ROTDIR
        - copy the generated YAML file from initialize to the ROTDIR
        - copy the updated bias correction files to ROTDIR
        - write UFS model readable atm incrment file

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

        # copy full YAML from executable to ROTDIR
        logger.info(f"Copying {self.task_config.jedi_yaml} to {self.task_config.COM_ATMOS_ANALYSIS}")
        src = os.path.join(self.task_config.DATA, f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.atmvar.yaml")
        dest = os.path.join(self.task_config.COM_ATMOS_ANALYSIS, f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.atmvar.yaml")
        logger.debug(f"Copying {src} to {dest}")
        yaml_copy = {
            'mkdir': [self.task_config.COM_ATMOS_ANALYSIS],
            'copy': [[src, dest]]
        }
        FileHandler(yaml_copy).sync()

        # copy bias correction files to ROTDIR
        logger.info("Copy bias correction files from DATA/ to COM/")
        biasdir = os.path.join(self.task_config.DATA, 'bc')
        biasls = os.listdir(biasdir)
        biaslist = []
        for bfile in biasls:
            src = os.path.join(biasdir, bfile)
            dest = os.path.join(self.task_config.COM_ATMOS_ANALYSIS, bfile)
            biaslist.append([src, dest])

        gprefix = f"{self.task_config.GPREFIX}"
        gsuffix = f"{to_YMDH(self.task_config.previous_cycle)}" + ".txt"
        aprefix = f"{self.task_config.APREFIX}"
        asuffix = f"{to_YMDH(self.task_config.current_cycle)}" + ".txt"

        logger.info(f"Copying {gprefix}*{gsuffix} from DATA/ to COM/ as {aprefix}*{asuffix}")
        obsdir = os.path.join(self.task_config.DATA, 'obs')
        obsls = os.listdir(obsdir)
        for ofile in obsls:
            if ofile.endswith(".txt"):
                src = os.path.join(obsdir, ofile)
                tfile = ofile.replace(gprefix, aprefix)
                tfile = tfile.replace(gsuffix, asuffix)
                dest = os.path.join(self.task_config.COM_ATMOS_ANALYSIS, tfile)
                biaslist.append([src, dest])

        bias_copy = {
            'mkdir': [self.task_config.COM_ATMOS_ANALYSIS],
            'copy': biaslist,
        }
        FileHandler(bias_copy).sync()

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
