#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Dict, List, Any
import netCDF4 as nc
import numpy as np

from wxflow import (AttrDict,
                    FileHandler,
                    to_fv3time, to_timedelta, add_to_datetime,
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

        _res_det = int(self.task_config['CASE'][1:])
        _res_ens = int(self.task_config['CASE_ENS'][1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config['assim_freq']}H") / 2)
        _recenter_yaml = os.path.join(self.task_config.DATA, f"{self.task_config.RUN}.t{self.task_config['cyc']:02d}z.land_recenter.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res_ens + 1,
                'npy_ges': _res_ens + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'SNOW_WINDOW_BEGIN': _window_begin,
                'SNOW_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'ATM_WINDOW_BEGIN': _window_begin,
                'ATM_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'jedi_yaml': _recenter_yaml,
            }
        )
        bkg_time = _window_begin if self.task_config.DOIAU else self.task_config.current_cycle
        local_dict['bkg_time'] = bkg_time

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.task_config, **local_dict)

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
    def genWeights(self) -> None:
        """Create a modified land_frac file for use by fregrid
        to interpolate the snow background from det to ensres

        Parameters
        ----------
        self : Analysis
           Instance of the SnowEnsAnalysis object
        """

        chdir(self.task_config.DATA)

        # loop through tiles
        for tile in range(1, self.task_config.ntiles + 1):
            # open the restart and get the vegetation type
            rst = nc.Dataset(f"./bkg/det/{to_fv3time(self.task_config.bkg_time)}.sfc_data.tile{tile}.nc")
            vtype = rst.variables['vtype'][:]
            rst.close()
            # open the oro data and get the land fraction
            oro = nc.Dataset(f"./orog/det/{self.task_config.CASE}.mx{self.task_config.OCNRES}_oro_data.tile{tile}.nc")
            land_frac = oro.variables['land_frac'][:]
            oro.close()
            # create an output file
            ncfile = nc.Dataset(f"./orog/det/{self.task_config.CASE}.mx{self.task_config.OCNRES}_interp_weight.tile{tile}.nc", mode='w', format='NETCDF4')
            case_int = int(self.task_config.CASE[1:])
            lon = ncfile.createDimension('lon', case_int)
            lat = ncfile.createDimension('lat', case_int)
            lsm_frac_out = ncfile.createVariable('lsm_frac', np.float32, ('lon', 'lat'))
            # set the land fraction to 0 on glaciers to not interpolate that snow
            glacier = 15
            land_frac[np.where(vtype[0, ...] == glacier)] = 0
            lsm_frac_out[:] = land_frac
            # write out and close the file
            ncfile.close()

    @logit(logger)
    def genMask(self) -> None:
        """Create a mask for use by JEDI
        to mask out snow increments on non-LSM gridpoints

        Parameters
        ----------
        self : Analysis
           Instance of the SnowEnsAnalysis object
        """

        chdir(self.task_config.DATA)

        # loop through tiles
        for tile in range(1, self.task_config.ntiles + 1):
            # open the restart and get the vegetation type
            rst = nc.Dataset(f"./bkg/mem001/{to_fv3time(self.task_config.bkg_time)}.sfc_data.tile{tile}.nc", mode="r+")
            vtype = rst.variables['vtype'][:]
            slmsk = rst.variables['slmsk'][:]
            # slmsk(Time, yaxis_1, xaxis_1)
            # set the mask to 3 on glaciers
            glacier = 15
            slmsk[np.where(vtype == glacier)] = 3
            # write out and close the file
            rst.variables['slmsk'][:] = slmsk
            rst.close()

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
            "--input_mosaic", f"./orog/det/{self.task_config.CASE}_mosaic.nc",
            "--input_dir", f"./bkg/det/",
            "--input_file", f"{to_fv3time(self.task_config.bkg_time)}.sfc_data",
            "--scalar_field", f"snodl",
            "--output_dir", f"./bkg/det_ensres/",
            "--output_file", f"{to_fv3time(self.task_config.bkg_time)}.sfc_data",
            "--output_mosaic", f"./orog/ens/{self.task_config.CASE_ENS}_mosaic.nc",
            "--interp_method", f"conserve_order1",
            "--weight_file", f"./orog/det/{self.task_config.CASE}.mx{self.task_config.OCNRES}_interp_weight",
            "--weight_field", f"lsm_frac",
            "--remap_file", f"./remap",
        ]
        fregrid_exe = os.path.join(self.task_config.DATA, 'fregrid.x')
        exec_cmd = Executable(fregrid_exe)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd(*arg_list)
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

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
            "--input_mosaic", f"./orog/det/{self.task_config.CASE}_mosaic.nc",
            "--input_dir", f"./inc/det/",
            "--input_file", f"snowinc.{to_fv3time(self.task_config.bkg_time)}.sfc_data",
            "--scalar_field", f"snodl",
            "--output_dir", f"./inc/det_ensres/",
            "--output_file", f"snowinc.{to_fv3time(self.task_config.bkg_time)}.sfc_data",
            "--output_mosaic", f"./orog/ens/{self.task_config.CASE_ENS}_mosaic.nc",
            "--interp_method", f"conserve_order1",
            "--weight_file", f"./orog/det/{self.task_config.CASE}.mx{self.task_config.OCNRES}_interp_weight",
            "--weight_field", f"lsm_frac",
            "--remap_file", f"./remap",
        ]
        fregrid_exe = os.path.join(self.task_config.DATA, 'fregrid.x')
        exec_cmd = Executable(fregrid_exe)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd(*arg_list)
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

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
        exec_cmd = Executable(self.task_config.APRUN_ESNOWRECEN)
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
        - copies the ensemble snow analyses to the proper locations
        - copies the ensemble mean increment to COM

        Parameters
        ----------
        self : Analysis
            Instance of the SnowEnsAnalysis object
        """
        # save files to COM
        logger.info(f"Copying files described in {self.task_config.SNOW_ENS_FINALIZE_TMPL}")
        snow_final_list = parse_j2yaml(self.task_config.SNOW_ENS_FINALIZE_TMPL, self.task_config)
        FileHandler(snow_final_list).sync()

    @logit(logger)
    def addEnsIncrements(self) -> None:
        """Loop through all ensemble members and apply increment to create
        a surface analysis for snow

        Parameters
        ----------
        self : Analysis
            Instance of the SnowEnsAnalysis object
        """

        bkg_times = []
        # no matter what, we want to process the center of the window
        bkg_times.append(self.task_config.current_cycle)
        # if DOIAU, we need to copy the increment to be valid at the center of the window
        # and compute the analysis there to restart the model
        if self.task_config.DOIAU:
            logger.info("Copying increments to beginning of window")
            template_in = f'snowinc.{to_fv3time(self.task_config.SNOW_WINDOW_BEGIN)}.sfc_data.tile{{tilenum}}.nc'
            template_out = f'snowinc.{to_fv3time(self.task_config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
            inclist = []
            for itile in range(1, 7):
                filename_in = template_in.format(tilenum=itile)
                filename_out = template_out.format(tilenum=itile)
                src = os.path.join(self.task_config.DATA, 'inc', 'ensmean', filename_in)
                dest = os.path.join(self.task_config.DATA, 'inc', 'ensmean', filename_out)
                inclist.append([src, dest])
            FileHandler({'copy': inclist}).sync()
            # if running with IAU, we also need an analysis at the beginning of the window
            bkg_times.append(self.task_config.SNOW_WINDOW_BEGIN)

        for bkg_time in bkg_times:
            for mem in range(1, self.task_config.NMEM_ENS + 1):
                # for now, just looping serially, should parallelize this eventually
                logger.info(f"Now applying increment to member mem{mem:03}")
                logger.info(f'{os.path.join(self.task_config.DATA, "anl", f"mem{mem:03}")}')
                memdict = AttrDict(
                    {
                        'HOMEgfs': self.task_config.HOMEgfs,
                        'DATA': os.path.join(self.task_config.DATA, "anl", f"mem{mem:03}"),
                        'DATAROOT': self.task_config.DATA,
                        'current_cycle': bkg_time,
                        'CASE_ENS': self.task_config.CASE_ENS,
                        'OCNRES': self.task_config.OCNRES,
                        'ntiles': self.task_config.ntiles,
                        'ENS_APPLY_INCR_NML_TMPL': self.task_config.ENS_APPLY_INCR_NML_TMPL,
                        'APPLY_INCR_EXE': self.task_config.APPLY_INCR_EXE,
                        'APRUN_APPLY_INCR': self.task_config.APRUN_APPLY_INCR,
                        'MYMEM': f"{mem:03}",
                    }
                )
                self.add_increments(memdict)

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
            COMIN_ATMOS_RESTART_PREV
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
             DATA
             DATAROOT
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
        os.chdir(config.DATA)

        logger.info("Create namelist for APPLY_INCR_EXE")
        nml_template = config.ENS_APPLY_INCR_NML_TMPL
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
