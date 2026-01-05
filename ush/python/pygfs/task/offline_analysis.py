#!/usr/bin/env python3

import os
from collections import OrderedDict
from logging import getLogger
from typing import Dict, Any
import f90nml
from wxflow import (AttrDict,
                    Task,
                    FileHandler,
                    Executable,
                    logit,
                    WorkflowException)

logger = getLogger(__name__.split('.')[-1])


class OfflineAnalysis(Task):
    """
    Class for tasks to compute analysis increments from
    an offline analysis and previous forecast
    """
    @logit(logger, name="SnowAnalysis")
    def __init__(self, config: Dict[str, Any]):
        """Constructor global offline analysis task

        This method will construct a global offline analysis task.
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task

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

        # fix ocnres
        self.task_config.OCNRES = f"{self.task_config.OCNRES:03d}"

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npz': self.task_config.LEVS - 1,
                'nlon_interp': _res * 4,
                'nlat_interp': _res * 2,
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global offline atmospheric analysis

        This method will initialize a global offline atmospheric analysis.
        This includes:
        - Staging input files
        - Generating namelists from templates
        - copy executables to $DATA

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # stage analysis and forecast files
        logger.info("Copy input files from $COM to $DATA")
        files_to_copy = []
        fcst_file_in = os.path.join(self.task_config.COMIN_ATMOS_HISTORY_PREV,
                                    f"{self.task_config.GPREFIX}atm.f006.nc")
        files_to_copy.append([fcst_file_in, os.path.join(self.task_config.DATA, "atmges_mem001")])
        sfcfcst_file_in = os.path.join(self.task_config.COMIN_ATMOS_HISTORY_PREV,
                                       f"{self.task_config.GPREFIX}sfc.f006.nc")
        files_to_copy.append([sfcfcst_file_in, os.path.join(self.task_config.DATA, "sfcges_mem001")])
        # TODO: Re-stage all of the inputs on HPSS to match EE2-compliant filenames
        anl_file_in = os.path.join(self.task_config.COMIN_ATMOS_ANALYSIS.replace('analysis', ''), f"{self.task_config.APREFIX_IN}atmanl.nc")
        files_to_copy.append([anl_file_in, os.path.join(self.task_config.DATA, "atmanl.input.nc")])
        sfcanl_file_in = os.path.join(self.task_config.COMIN_ATMOS_ANALYSIS.replace('analysis', ''), f"{self.task_config.APREFIX_IN}sfcanl.nc")
        files_to_copy.append([sfcanl_file_in, os.path.join(self.task_config.DATA, "sfcanl.input.nc")])
        FileHandler({'copy': files_to_copy}).sync()

        # generate namelists for the executables
        # set up the namelist for the background interpolation code
        logger.info("Generating namelist for 'chgres_nc'")
        namelist = {
            'chgres_setup': {
                "i_output": self.task_config.nlon_interp,
                "j_output": self.task_config.nlat_interp,
                "input_file": "atmanl.input.nc",
                "output_file": "atmanl_mem001",
                "terrain_file": "atmges_mem001",
                "ref_file": "atmges_mem001",
            }
        }
        logger.info(namelist)

        with open(os.path.join(self.task_config.DATA, 'chgres_nc_gauss.nml'), 'w') as nmlfile:
            f90nml.write(namelist, nmlfile)
        logger.info(f"Wrote namelist to {os.path.join(self.task_config.DATA, 'chgres_nc_gauss.nml')}")

        logger.info("Generating namelist for 'calc_increment'")
        # set up the namelist for the calc increment code
        namelist = {
            "setup": {
                "datapath": "./",
                "analysis_filename": "atmanl",
                "firstguess_filename": "atmges",
                "increment_filename": "atminc",
                "debug": False,
                "nens": 1,
                "imp_physics": self.task_config.imp_physics
            },
            "zeroinc": {
                "incvars_to_zero": self.task_config.INCREMENTS_TO_ZERO
            }
        }
        logger.info(namelist)

        with open(os.path.join(self.task_config.DATA, 'calc_increment.nml'), 'w') as nmlfile:
            f90nml.write(namelist, nmlfile)
        logger.info(f"Wrote namelist to {os.path.join(self.task_config.DATA, 'calc_increment.nml')}")

        # setup namelist for tref increment calculation
        logger.info("Generating namelist for 'tref_calc'")
        namelist = {
            "tref_calc_setup": {
                "i_output": self.task_config.nlon_interp,
                "j_output": self.task_config.nlat_interp,
                "sfcanl_file": "sfcanl.input.nc",
                "sfcf006_file": "sfcges_mem001",
                "output_file": "dtfanl.nc",
            }
        }

        logger.info(namelist)
        with open(os.path.join(self.task_config.DATA, 'tref_calc.nml'), 'w') as nmlfile:
            f90nml.write(namelist, nmlfile)
        logger.info(f"Wrote namelist to {os.path.join(self.task_config.DATA, 'tref_calc.nml')}")

        # copy executables to $DATA
        executables_to_copy = []
        executable_list = ['enkf_chgres_recenter_nc.x', 'calc_increment_ens_ncio.x', 'tref_calc.x']
        for exec_name in executable_list:
            executables_to_copy.append([os.path.join(self.task_config.EXECgfs, exec_name),
                                        os.path.join(self.task_config.DATA, exec_name)])
        FileHandler({'copy': executables_to_copy}).sync()

    @logit(logger)
    def interpolate_analysis(self) -> None:
        """If necessary, interpolate the offline analysis
        from its original resolution to the resolution of the
        previous model forecast.

        Parameters
        ----------
        self : OfflineAnalysis
            Instance of the OfflineAnalysis object
        """

        # set up and run the executable
        exe = Executable(self.task_config.APRUN_CHGRES)
        exe.add_default_arg(os.path.join(self.task_config.DATA, 'enkf_chgres_recenter_nc.x'))
        exe.add_default_arg(os.path.join(self.task_config.DATA, 'chgres_nc_gauss.nml'))
        try:
            logger.debug(f"Executing {exe}")
            exe()
        except OSError:
            logger.exception(f"Failed to execute {exe}")
            raise
        except Exception as err:
            logger.exception(f"An error occured during execution of {exe}")
            raise WorkflowException(f"An error occured during execution of {exe}") from err

    @logit(logger)
    def calc_tref_inc(self) -> None:
        """Interpolate the tref analysis and compute the dtf increment.

        Parameters
        ----------
        self : OfflineAnalysis
            Instance of the OfflineAnalysis object
        """

        # set up and run the executable
        exe = Executable(self.task_config.APRUN_CHGRES)
        exe.add_default_arg(os.path.join(self.task_config.DATA, 'tref_calc.x'))
        exe.add_default_arg(os.path.join(self.task_config.DATA, 'tref_calc.nml'))
        try:
            logger.debug(f"Executing {exe}")
            exe()
        except OSError:
            logger.exception(f"Failed to execute {exe}")
            raise
        except Exception as err:
            logger.exception(f"An error occured during execution of {exe}")
            raise WorkflowException(f"An error occured during execution of {exe}") from err

    @logit(logger)
    def calc_increment(self) -> None:
        """Compute the analysis increment for input to the forecast model
        by subtracting the previous model forecast from the provided analysis.

        Parameters
        ----------
        self : OfflineAnalysis
            Instance of the OfflineAnalysis object
        """

        # set up and run the executable
        exe = Executable(self.task_config.APRUN_CALCINC)
        exe.add_default_arg(os.path.join(self.task_config.DATA, 'calc_increment_ens_ncio.x'))
        try:
            logger.debug(f"Executing {exe}")
            exe()
        except OSError:
            logger.exception(f"Failed to execute {exe}")
            raise
        except Exception as err:
            logger.exception(f"An error occured during execution of {exe}")
            raise WorkflowException(f"An error occured during execution of {exe}") from err

    @logit(logger)
    def finalize(self) -> None:
        """Performs closing actions of the offline analysis task
        This method:
        - copies the analysis files to the COM/
        - copies the increment files to the COM/
        - copy some files from GDAS COM/ to GCAFS COM/

        Parameters
        ----------
        self : OfflineAnalysis
            Instance of the OfflineAnalysis object
        """
        output_files = []
        output_files.append([os.path.join(self.task_config.DATA, 'atmanl_mem001'),
                             os.path.join(self.task_config.COMOUT_ATMOS_ANALYSIS, f"{self.task_config.APREFIX}analysis.atm.a006.nc")])
        output_files.append([os.path.join(self.task_config.DATA, 'atminc_mem001'),
                             os.path.join(self.task_config.COMOUT_ATMOS_ANALYSIS, f"{self.task_config.APREFIX}increment.atm.i006.nc")])
        FileHandler({'copy': output_files}).sync()
        # these files are for the surface analysis
        transfer_files = []
        transfer_files.append([os.path.join(self.task_config.COMIN_OBSPROC, f"{self.task_config.APREFIX_IN}rtgssthr.grb"),
                               os.path.join(self.task_config.COMOUT_OBS, f"{self.task_config.APREFIX}rtgssthr.grb")])
        transfer_files.append([os.path.join(self.task_config.COMIN_OBSPROC, f"{self.task_config.APREFIX_IN}seaice.5min.blend.grb"),
                               os.path.join(self.task_config.COMOUT_OBS, f"{self.task_config.APREFIX}seaice.5min.blend.grb")])
        transfer_files.append([os.path.join(self.task_config.COMIN_OBSPROC, f"{self.task_config.APREFIX_IN}snogrb_t1534.3072.1536"),
                               os.path.join(self.task_config.COMOUT_OBS, f"{self.task_config.APREFIX}snogrb_t1534.3072.1536")])
        # TODO: Re-stage the inputs for the GCDAS offline analysis on HPSS following EE2-compliant filenames, then update this line
        transfer_files.append([
            os.path.join(self.task_config.DATA, "dtfanl.nc"),
            os.path.join(self.task_config.COMOUT_ATMOS_ANALYSIS,
                         f"{self.task_config.APREFIX}increment.dtf.i006.nc")
        ])
        FileHandler({'copy': transfer_files}).sync()
