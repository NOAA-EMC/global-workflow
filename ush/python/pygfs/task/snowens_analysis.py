#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Dict, List, Any
from pprint import pformat
import numpy as np
from netCDF4 import Dataset

from wxflow import (AttrDict,
                    FileHandler,
                    to_fv3time, to_YMD, to_YMDH, to_timedelta, add_to_datetime,
                    rm_p, chdir,
                    parse_j2yaml, save_as_yaml,
                    Jinja,
                    logit,
                    Executable,
                    WorkflowException)
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class SnowEnsAnalysis(Analysis):
    """
    Class for global ensemble snow analysis tasks
    """

    @logit(logger, name="SnowEnsAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _res_det = int(self.config['CASE'][1:])
        _res_ens = int(self.config['CASE_ENS'][1:])
        _window_begin = add_to_datetime(self.runtime_config.current_cycle, -to_timedelta(f"{self.config['assim_freq']}H") / 2)
        _recenter_yaml = os.path.join(self.runtime_config.DATA, f"{self.runtime_config.RUN}.t{self.runtime_config['cyc']:02d}z.land_recenter.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res_ens + 1,
                'npy_ges': _res_ens + 1,
                'npz_ges': self.config.LEVS - 1,
                'npz': self.config.LEVS - 1,
                'SNOW_WINDOW_BEGIN': _window_begin,
                'SNOW_WINDOW_LENGTH': f"PT{self.config['assim_freq']}H",
                'ATM_WINDOW_BEGIN': _window_begin,
                'ATM_WINDOW_LENGTH': f"PT{self.config['assim_freq']}H",
                'OPREFIX': f"{self.runtime_config.RUN}.t{self.runtime_config.cyc:02d}z.",
                'APREFIX': f"{self.runtime_config.RUN}.t{self.runtime_config.cyc:02d}z.",
                'jedi_yaml': _recenter_yaml,
            }
        )
        bkg_time = _window_begin if self.config.DOIAU else self.runtime_config.current_cycle
        local_dict['bkg_time'] = bkg_time

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.config, **self.runtime_config, **local_dict)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize method for snow ensemble analysis
        This method:


        Parameters
        ----------
        self : Analysis
            Instance of the SnowEnsAnalysis object
        """

        super().initialize()

        # stage background and increment files
        logger.info(f"Staging files from {self.task_config.SNOW_ENS_STAGE_TMPL}")
        snow_stage_list = parse_j2yaml(self.task_config.SNOW_ENS_STAGE_TMPL, self.task_config)
        FileHandler(snow_stage_list).sync()

        # stage orography files
        logger.info(f"Staging orography files specified in {self.task_config.SNOW_OROG_STAGE_TMPL}")
        snow_orog_stage_list = parse_j2yaml(self.task_config.SNOW_OROG_STAGE_TMPL, self.task_config)
        FileHandler(snow_orog_stage_list).sync()

        # stage fix files for fv3-jedi
        logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
        jedi_fix_list = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # write land ensemble recentering YAML
        save_as_yaml(self.task_config.jedi_config, self.task_config.jedi_yaml)
        logger.info(f"Wrote recentering YAML to: {self.task_config.jedi_yaml}")

        # link recentering executable
        # placeholder, currently already done by the analysis parent class

        # copy fregrid executable
        fregrid_copy = {'copy': [[os.path.join(self.task_config.EXECgfs, 'fregrid'), os.path.join(self.task_config.DATA, 'fregrid.x')]]}
        FileHandler(fregrid_copy).sync()

    @logit(logger)
    def regridDetBkg(self) -> None:
        """Run fregrid to regrid the deterministic snow background
        to the ensemble resolution

        Parameters
        ----------
        self : Analysis
           Instance of the SnowEnsAnalysis object
        """

        chdir(self.task_config.DATA)

        arg_list = [
            f"--input_mosaic ./orog/det/{self.task_config.CASE}_mosaic.nc",
            f"--input_dir ./bkg/det/",
            f"--input_file {to_fv3time(self.task_config.bkg_time)}.sfc_data",
            f"--scalar_field snodl",
            f"--output_dir ./bkg/det_ensres/",
            f"--output_file {to_fv3time(self.task_config.bkg_time)}.ensres.sfc_data",
            f"--output_mosaic ./orog/ens/{self.task_config.CASE_ENS}_mosaic.nc",
            f"--interp_method conserve_order1",
            f"--weight_file ./orog/det/{self.task_config.CASE}.mx{self.task_config.OCNRES}_oro_data",
            f"--weight_field land_frac",
        ]
        fregrid = os.path.join(self.task_config.DATA, 'fregrid.x') + " " + " ".join(arg_list)
        exec_cmd = Executable(fregrid)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

        pass

    @logit(logger)
    def regridDetInc(self) -> None:
        """Run fregrid to regrid the deterministic snow increment
        to the ensemble resolution

        Parameters
        ----------
        self : Analysis
           Instance of the SnowEnsAnalysis object
        """

        chdir(self.task_config.DATA)

        arg_list = [
            f"--input_mosaic ./orog/det/{self.task_config.CASE}_mosaic.nc",
            f"--input_dir ./inc/det/",
            f"--input_file snowinc.{to_fv3time(self.task_config.bkg_time)}.sfc_data",
            f"--scalar_field snodl",
            f"--output_dir ./inc/det_ensres/",
            f"--output_file snowinc.{to_fv3time(self.task_config.bkg_time)}.ensres.sfc_data",
            f"--output_mosaic ./orog/ens/{self.task_config.CASE_ENS}_mosaic.nc",
            f"--interp_method conserve_order1",
            f"--weight_file ./orog/det/{self.task_config.CASE}.mx{self.task_config.OCNRES}_oro_data",
            f"--weight_field land_frac",
        ]
        fregrid = os.path.join(self.task_config.DATA, 'fregrid.x') + " " + " ".join(arg_list)
        exec_cmd = Executable(fregrid)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

        pass

    @logit(logger)
    def recenterEns(self) -> None:
        """Run recentering code to create an ensemble of snow increments
        based on the deterministic increment, and the difference
        between the determinstic and ensemble mean forecast

        Parameters
        ----------
        self : Analysis
           Instance of the SnowEnsAnalysis object
        """
        logger.info("Running recentering code")
        exec_cmd = Executable(self.task_config.APRUN_ESNOWANL)
        exec_name = os.path.join(self.task_config.DATA, 'gdasapp_land_ensrecenter.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg(self.task_config.jedi_yaml)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

    @logit(logger)
    def finalize(self) -> None:
        """Performs closing actions of the snow ensemble analysis task
        This method:
        - does nothing yet

        Parameters
        ----------
        self : Analysis
            Instance of the SnowEnsAnalysis object
        """

    @staticmethod
    @logit(logger)
    def get_bkg_dict(config: Dict) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method constructs a dictionary of FV3 RESTART files (coupler, sfc_data)
        that are needed for global snow DA and returns said dictionary for use by the FileHandler class.

        Parameters
        ----------
        config: Dict
            Dictionary of key-value pairs needed in this method
            Should contain the following keys:
            COM_ATMOS_RESTART_PREV
            DATA
            current_cycle
            ntiles

        Returns
        ----------
        bkg_dict: Dict
            a dictionary containing the list of model background files to copy for FileHandler
        """

        bkg_dict = {
            'mkdir': [],
            'copy': [],
        }
        return bkg_dict

    @staticmethod
    @logit(logger)
    def add_increments(config: Dict) -> None:
        """Executes the program "apply_incr.exe" to create analysis "sfc_data" files by adding increments to backgrounds

        Parameters
        ----------
         config: Dict
             Dictionary of key-value pairs needed in this method
             Should contain the following keys:
             HOMEgfs
             COM_ATMOS_RESTART_PREV
             DATA
             current_cycle
             CASE
             OCNRES
             ntiles
             APPLY_INCR_NML_TMPL
             APPLY_INCR_EXE
             APRUN_APPLY_INCR

        Raises
        ------
        OSError
            Failure due to OS issues
        WorkflowException
            All other exceptions
        """

        # need backgrounds to create analysis from increments after LETKF
        logger.info("Copy backgrounds into anl/ directory for creating analysis from increments")
        template = f'{to_fv3time(config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
        anllist = []
        for itile in range(1, config.ntiles + 1):
            filename = template.format(tilenum=itile)
            src = os.path.join(config.COM_ATMOS_RESTART_PREV, filename)
            dest = os.path.join(config.DATA, "anl", filename)
            anllist.append([src, dest])
        FileHandler({'copy': anllist}).sync()

        logger.info("Create namelist for APPLY_INCR_EXE")
        nml_template = config.APPLY_INCR_NML_TMPL
        nml_data = Jinja(nml_template, config).render
        logger.debug(f"apply_incr_nml:\n{nml_data}")

        nml_file = os.path.join(config.DATA, "apply_incr_nml")
        with open(nml_file, "w") as fho:
            fho.write(nml_data)

        logger.info("Link APPLY_INCR_EXE into DATA/")
        exe_src = config.APPLY_INCR_EXE
        exe_dest = os.path.join(config.DATA, os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # execute APPLY_INCR_EXE to create analysis files
        exe = Executable(config.APRUN_APPLY_INCR)
        exe.add_default_arg(os.path.join(config.DATA, os.path.basename(exe_src)))
        logger.info(f"Executing {exe}")
        try:
            exe()
        except OSError:
            raise OSError(f"Failed to execute {exe}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exe}")

    def get_obs_dict(self) -> Dict[str, Any]:
        obs_dict = {
            'mkdir': [],
            'copy': [],
        }
        return obs_dict

    def get_bias_dict(self) -> Dict[str, Any]:
        bias_dict = {
            'mkdir': [],
            'copy': [],
        }
        return bias_dict
