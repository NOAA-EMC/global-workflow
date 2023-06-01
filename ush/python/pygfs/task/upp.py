#!/usr/bin/env python3

import os
from logging import getLogger
from typing import List, Dict, Any, Union
from pprint import pformat

from pygw.attrdict import AttrDict
from pygw.yaml_file import YAMLFile, parse_j2yaml, parse_yamltmpl
from pygw.file_utils import FileHandler
from pygw.jinja import Jinja
from pygw.logger import logit
from pygw.task import Task
from pygw.timetools import add_to_datetime, to_timedelta, strftime
from pygw.exceptions import WorkflowException
from pygw.executable import Executable, which

logger = getLogger(__name__.split('.')[-1])


class UPP(Task):
    """_summary_

    Parameters
    ----------
    Task : _type_
        _description_
    """

    @logit(logger, name="UPP")
    def __init__(self, config: Dict[str, Any]) -> None:
        """_summary_

        Parameters
        ----------
        config : Dict[str, Any]
            _description_
        """
        super().__init__(config)

        self.task_config = AttrDict(**self.config, **self.runtime_config)

    @logit(logger)
    def initialize(self) -> None:
        """_summary_
        """
        localconf = AttrDict()
        keys = ['HOMEgfs', 'DATA', 'current_cycle', 'RUN',
                'COM_ATMOS_ANALYSIS', 'COM_ATMOS_HISTORY',
                'UPP_CONFIG']
        for key in keys:
            localconf[key] = self.task_config[key]

        # Read the upp.yaml file for common configuration
        logger.info("Read the UPP configuration yaml file {localconf.UPP_CONFIG}")
        upp_yaml = parse_j2yaml(localconf.UPP_CONFIG, localconf)
        logger.debug(f"upp_yaml.upp: {pformat(upp_yaml.upp)}")

        # Copy static data to run directory
        logger.info("Copy static data to run directory")
        FileHandler(upp_yaml.upp.fix_data).sync()

    @logit(logger)
    def pre_execute(self, upp_run: str, forecast_hour: int = 0) -> None:
        localconf = AttrDict()
        keys = ['HOMEgfs', 'DATA', 'current_cycle', 'RUN',
                'COM_ATMOS_ANALYSIS', 'COM_ATMOS_HISTORY',
                'UPP_CONFIG']
        for key in keys:
            localconf[key] = self.task_config[key]

        localconf.forecast_hour = forecast_hour
        localconf.valid_datetime = add_to_datetime(localconf.current_cycle, to_timedelta(f"{forecast_hour}H"))
        localconf.atmos_filename = f"atm_{localconf.valid_datetime.strftime('%Y%m%d%H%M%S')}.nc"
        localconf.flux_filename = f"sfc_{localconf.valid_datetime.strftime('%Y%m%d%H%M%S')}.nc"

        logger.info("Read the UPP configuration yaml file {localconf.UPP_CONFIG}")
        upp_yaml = parse_j2yaml(localconf.UPP_CONFIG, localconf)
        logger.debug(f"upp_config: {pformat(upp_yaml[upp_run])}")

        # Copy "upp_run" specific data to run directory
        logger.info("Copy {upp_run} data to run directory")
        FileHandler(upp_yaml[upp_run].data_in).sync()

        # Update the localconf with the upp_run specific configuration
        localconf.update(upp_yaml.upp.config)
        localconf.update(upp_yaml[upp_run].config)

        # Configure the namelist and write to file
        logger.info("Create namelist for upp.x")
        nml_template = os.path.join(localconf.DATA, "itag.jinja")
        nml_data = Jinja(nml_template, localconf).render
        logger.debug(f"itag:\n{nml_data}")
        nml_file = os.path.join(localconf.DATA, "itag")
        with open(nml_file, "w") as fho:
            fho.write(nml_data)

    @logit(logger)
    def execute(self) -> None:

        workdir = self.task_config.DATA
        aprun_cmd = self.task_config.APRUN_CMD

        # Run the UPP executable
        self.run_upp(workdir, aprun_cmd)

        # Index the output grib2 file
        self.index_grib2(workdir)

    @staticmethod
    @logit(logger)
    def run_upp(workdir: Union[str, os.PathLike], aprun_cmd: str, exec_name: str = 'upp.x', pgbout="pgbmaster.grb2") -> None:
        """
        Run the UPP executable
        Parameters
        ----------
        workdir : str | os.PathLike
            Working directory where to run containing the necessary files and executable
        aprun_cmd : str
            Launcher command e.g. mpirun -np <ntasks> or srun, etc.
        exec_name : str
            Name of the UPP executable e.g. upp.x
        Raises
        ------
        OSError
            Failure due to OS issues
        WorkflowException
            All other exceptions
        """

        os.chdir(workdir)

        exec_cmd = Executable(aprun_cmd)
        exec_cmd.add_default_arg(os.path.join(workdir, exec_name))
        #exec_cmd.add_default_env("PGBOUT", pgbout)

        UPP._call_executable(exec_cmd)

    @staticmethod
    @logit(logger)
    def index_grib2(workdir: Union[str, os.PathLike], grib2index_cmd=None, pgbout="pgbmaster.grb2", pgbindex='pgbmaster.idx') -> None:
        """
        Index the grib2file
        Parameters
        ----------
        workdir : str | os.PathLike
            Working directory where to run containing the necessary files and executable
        Raises
        ------
        OSError
            Failure due to OS issues
        WorkflowException
            All other exceptions
        """
        os.chdir(workdir)
        logger.info("Generate index file")

        exec_cmd = which("grib2index") if grib2index_cmd is None else Executable(grib2index_cmd)
        exec_cmd.add_default_arg(os.path.join(workdir, pgbout))
        exec_cmd.add_default_arg(os.path.join(workdir, pgbindex))

        UPP._call_executable(exec_cmd)

    @staticmethod
    @logit(logger)
    def _call_executable(exec_cmd: Union[str, Executable]) -> None:

        logger.info(f"Executing {exec_cmd}")
        try:
            exec_cmd()
        except OSError:
            logger.exception(f"FATAL ERROR: Failed to execute {exec_cmd}")
            raise OSError(f"{exec_cmd}")
        except Exception:
            logger.exception(f"FATAL ERROR: Error occurred during execution of {exec_cmd}")
            raise WorkflowException(f"{exec_cmd}")

    @logit(logger)
    def post_execute(self, upp_run: str, forecast_hour: int = 0) -> None:

        localconf = AttrDict()
        keys = ['HOMEgfs', 'DATA', 'current_cycle', 'RUN',
                'COM_ATMOS_ANALYSIS', 'COM_ATMOS_HISTORY',
                'UPP_CONFIG']
        for key in keys:
            localconf[key] = self.task_config[key]

        localconf.forecast_hour = forecast_hour
        localconf.valid_datetime = add_to_datetime(localconf.current_cycle, to_timedelta(f"{forecast_hour}H"))
        localconf.atmos_filename = f"atm_{localconf.valid_datetime.strftime('%Y%m%d%H%M%S')}.nc"
        localconf.flux_filename = f"sfc_{localconf.valid_datetime.strftime('%Y%m%d%H%M%S')}.nc"

        logger.info("Read the UPP configuration yaml file {localconf.UPP_CONFIG}")
        upp_yaml = parse_j2yaml(localconf.UPP_CONFIG, localconf)
        logger.debug(f"upp_config: {pformat(upp_yaml[upp_run])}")

        # Copy "upp_run" specific data COM/ to directory
        logger.info("Copy {upp_run} data to COM/ directory")
        FileHandler(upp_yaml[upp_run].data_out).sync()

    @logit(logger)
    def finalize(self) -> None:
        pass
