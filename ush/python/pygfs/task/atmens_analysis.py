#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from logging import getLogger
from typing import Dict, List, Any

from pygw.attrdict import AttrDict
from pygw.file_utils import FileHandler
from pygw.timetools import add_to_datetime, to_fv3time, to_timedelta, to_YMDH
from pygw.fsutils import rm_p, chdir
from pygw.yaml_file import parse_yamltmpl, parse_j2yaml, save_as_yaml
from pygw.logger import logit
from pygw.executable import Executable
from pygw.exceptions import WorkflowException
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class AtmEnsAnalysis(Analysis):
    """
    Class for global atmens analysis tasks
    """
    @logit(logger, name="AtmEnsAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _res = int(self.config.CASE_ENKF[1:])
        _res_anl = int(self.config.CASE_ANL[1:])
        _window_begin = add_to_datetime(self.runtime_config.current_cycle, -to_timedelta(f"{self.config.assim_freq}H") / 2)
        _fv3jedi_yaml = os.path.join(self.runtime_config.DATA, f"{self.runtime_config.CDUMP}.t{self.runtime_config.cyc:02d}z.atmens.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.config.LEVS - 1,
                'npz': self.config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.config.LEVS - 1,
                'ATM_WINDOW_BEGIN': _window_begin,
                'ATM_WINDOW_LENGTH': f"PT{self.config.assim_freq}H",
                'comin_ges_atm': self.config.COMIN_GES,
                'comin_ges_atmens': self.config.COMIN_GES_ENS,
                'OPREFIX': f"{self.config.EUPD_CYC}.t{self.runtime_config.cyc:02d}z.",  # TODO: CDUMP is being replaced by RUN
                'APREFIX': f"{self.runtime_config.CDUMP}.t{self.runtime_config.cyc:02d}z.",  # TODO: CDUMP is being replaced by RUN
                'GPREFIX': f"gdas.t{self.runtime_config.previous_cycle.hour:02d}z.",
                'fv3jedi_yaml': _fv3jedi_yaml,
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.config, **self.runtime_config, **local_dict)

    @logit(logger)
    def initialize(self: Analysis) -> None:
        """Initialize a global atmens analysis

        This method will initialize a global atmens analysis using JEDI.
        This includes:
        - staging CRTM fix files
        - staging FV3-JEDI fix files
        - staging model backgrounds
        - generating a YAML file for the JEDI executable
        - linking the JEDI executable (TODO make it copyable, requires JEDI fix)
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
        crtm_fix_list_path = os.path.join(self.task_config.HOMEgfs, 'parm', 'parm_gdas', 'atm_crtm_coeff.yaml')
        logger.debug(f"Staging CRTM fix files from {crtm_fix_list_path}")
        crtm_fix_list = parse_yamltmpl(crtm_fix_list_path, self.task_config)
        FileHandler(crtm_fix_list).sync()

        # stage fix files
        jedi_fix_list_path = os.path.join(self.task_config.HOMEgfs, 'parm', 'parm_gdas', 'atm_jedi_fix.yaml')
        logger.debug(f"Staging JEDI fix files from {jedi_fix_list_path}")
        jedi_fix_list = parse_yamltmpl(jedi_fix_list_path, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage backgrounds
        FileHandler(self.get_bkg_dict(AttrDict(self.task_config))).sync()

        # generate ensemble da YAML file
        logger.debug(f"Generate ensemble da YAML file: {self.task_config.fv3jedi_yaml}")
        ensda_yaml = parse_j2yaml(self.task_config.ATMENSYAML, self.task_config)
        save_as_yaml(ensda_yaml, self.task_config.fv3jedi_yaml)
        logger.info(f"Wrote ensemble da YAML to: {self.task_config.fv3jedi_yaml}")

        # link executable to DATA/ directory
        exe_src = self.task_config.JEDIENSEXE
        logger.debug(f"Link executable {exe_src} to DATA/")  # TODO: linking is not permitted per EE2.  Needs work in JEDI to be able to copy the exec.
        exe_dest = os.path.join(self.task_config.DATA, os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # need output dir for diags and anl
        logger.debug("Create empty output [anl, diags] directories to receive output from executable")
        newdirs = [
            os.path.join(self.task_config.DATA, 'anl'),
            os.path.join(self.task_config.DATA, 'diags'),
        ]
        FileHandler({'mkdir': newdirs}).sync()

        # Make directories for member analysis files
        for imem in range(1, self.task_config.NMEM_ENKF + 1):
            memchar = f"mem{imem:03d}"
            anldir = [
                os.path.join(self.task_config.DATA, 'anl', memchar)
            ]
            FileHandler({'mkdir': anldir}).sync()

    @logit(logger)
    def execute(self: Analysis) -> None:
        """Execute a global atmens analysis

        This method will execute a global atmens analysis using JEDI.
        This includes:
        - changing to the run directory
        - running the global atmens analysis executable

        Parameters
        ----------
        Analysis: parent class for GDAS task

        Returns
        ----------
        None
        """
        chdir(self.task_config.DATA)

        exec_cmd = Executable(self.task_config.APRUN_ATMENSANL)
        exec_name = os.path.join(self.task_config.DATA, 'fv3jedi_letkf.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg(self.task_config.fv3jedi_yaml)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

        pass

    @logit(logger)
    def finalize(self: Analysis) -> None:
        """Finalize a global atmens analysis

        This method will finalize a global atmens analysis using JEDI.
        This includes:
        - tar output diag files and place in ROTDIR
        - copy the generated YAML file from initialize to the ROTDIR
        - write UFS model readable atm incrment file

        Parameters
        ----------
        Analysis: parent class for GDAS task

        Returns
        ----------
        None
        """
        # ---- tar up diags
        # path of output tar statfile
        atmensstat = os.path.join(self.task_config.COMOUT, f"{self.task_config.APREFIX}atmensstat")

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.task_config.DATA, 'diags', 'diag*nc4'))

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
        logger.info(f"Copying {self.task_config.fv3jedi_yaml} to {self.task_config.COMOUT}")
        src = os.path.join(self.task_config.DATA, f"{self.task_config.CDUMP}.t{self.task_config.cyc:02d}z.atmens.yaml")
        dest = os.path.join(self.task_config.COMOUT, f"{self.task_config.CDUMP}.t{self.task_config.cyc:02d}z.atmens.yaml")
        logger.debug(f"Copying {src} to {dest}")
        yaml_copy = {
            'mkdir': [self.task_config.COMOUT],
            'copy': [[src, dest]]
        }
        FileHandler(yaml_copy).sync()

        # Create UFS model readable atm increment file from UFS-DA atm increment
        logger.info("Create UFS model readable atm increment file from UFS-DA atm increment")
        self.jedi2fv3inc()

    def clean(self):
        super().clean()

    @logit(logger)
    def get_bkg_dict(self, task_config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method constructs a dictionary of FV3 RESTART files (coupler, core, tracer)
        that are needed for global atmens DA and returns said dictionary for use by the FileHandler class.

        Parameters
        ----------
        task_config: Dict
            a dictionary containing all of the configuration needed for the task

        Returns
        ----------
        bkg_dict: Dict
            a dictionary containing the list of model background files to copy for FileHandler
        """
        # NOTE for now this is FV3 RESTART files and just assumed to be fh006

        bkgdir = [
            os.path.join(task_config.DATA, 'bkg'),
        ]
        FileHandler({'mkdir': bkgdir}).sync()

        # loop over ensemble members
        dirlist = []
        bkglist = []
        for imem in range(1, task_config.NMEM_ENKF + 1):
            memchar = f"mem{imem:03d}"

            # accumulate directory list for member restart files
            dirlist.append(os.path.join(task_config.DATA, 'bkg', memchar))

            # get FV3 RESTART files, this will be a lot simpler when using history files
            rst_dir = os.path.join(task_config.comin_ges_atmens, memchar, 'atmos/RESTART')
            run_dir = os.path.join(task_config.DATA, 'bkg', memchar)

            # atmens DA needs coupler
            basename = f'{to_fv3time(task_config.current_cycle)}.coupler.res'
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(task_config.DATA, 'bkg', memchar, basename)])
            # atmens DA needs core, srf_wnd, tracer, phy_data, sfc_data
            for ftype in ['core', 'srf_wnd', 'tracer']:
                template = f'{to_fv3time(self.task_config.current_cycle)}.fv_{ftype}.res.tile{{tilenum}}.nc'
                for itile in range(1, task_config.ntiles + 1):
                    basename = template.format(tilenum=itile)
                    bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

            for ftype in ['phy_data', 'sfc_data']:
                template = f'{to_fv3time(self.task_config.current_cycle)}.{ftype}.tile{{tilenum}}.nc'
                for itile in range(1, task_config.ntiles + 1):
                    basename = template.format(tilenum=itile)
                    bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        bkg_dict = {
            'mkdir': dirlist,
            'copy': bkglist,
        }

        return bkg_dict

    @logit(logger)
    def jedi2fv3inc(self: Analysis) -> None:
        """Generate UFS model readable analysis increment

        This method writes a UFS DA atm increment in UFS model readable format.
        This includes:
        - write UFS-DA atm increments using variable names expected by UFS model
        - compute and write delp increment
        - compute and write hydrostatic delz increment

        Please note that some of these steps are temporary and will be modified
        once the modle is able to directly read atm increments.

        Parameters
        ----------
        Analysis: parent class for GDAS task

        Returns
        ----------
        None
        """
        # Select the atm guess file based on the analysis and background resolutions
        # Fields from the atm guess are used to compute the delp and delz increments
        cdate = to_fv3time(self.task_config.current_cycle)
        cdate_inc = cdate.replace('.', '_')

        # Reference the python script which does the actual work
        incpy = os.path.join(self.task_config.HOMEgfs, 'ush/jediinc2fv3.py')

        for imem in range(1, self.task_config.NMEM_ENKF + 1):
            memchar = f"mem{imem:03d}"

            # make output directory for member increment
            incdir = [
                os.path.join(self.task_config.COMOUT, memchar, 'atmos')
            ]
            FileHandler({'mkdir': incdir}).sync()

            # rewrite UFS-DA atmens increments
            atmges_fv3 = os.path.join(self.task_config.COMIN_GES_ENS, memchar, 'atmos',
                                      f"{self.task_config.CDUMP}.t{self.runtime_config.previous_cycle.hour:02d}z.atmf006.nc")
            atminc_jedi = os.path.join(self.task_config.DATA, 'anl', memchar, f'atminc.{cdate_inc}z.nc4')
            atminc_fv3 = os.path.join(self.task_config.COMOUT, memchar, 'atmos',
                                      f"{self.task_config.CDUMP}.t{self.runtime_config.cyc:02d}z.atminc.nc")

            # Execute incpy to create the UFS model atm increment file
            cmd = Executable(incpy)
            cmd.add_default_arg(atmges_fv3)
            cmd.add_default_arg(atminc_jedi)
            cmd.add_default_arg(atminc_fv3)
            logger.debug(f"Executing {cmd}")
            cmd(output='stdout', error='stderr')
