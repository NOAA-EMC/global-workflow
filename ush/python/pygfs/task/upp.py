#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Dict, Any, Union
from pprint import pformat

from wxflow import (AttrDict,
                    parse_j2yaml,
                    FileHandler,
                    Jinja,
                    logit,
                    Task,
                    add_to_datetime, to_timedelta,
                    WorkflowException,
                    Executable, which)

logger = getLogger(__name__.split('.')[-1])


class UPP(Task):
    """Unified Post Processor Task
    """

    VALID_UPP_RUN = ['analysis', 'forecast', 'goes', 'wafs']

    @logit(logger, name="UPP")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the UPP task
        The constructor is responsible for resolving the "UPP_CONFIG" based in the run-type "upp_run"
        Sections of upp.yaml outside of the `upp` block are all valid `upp_run` options
        E.g. "upp_run" sections are:
        analysis: process analysis output
        forecast: process UFS-weather-model forecast output
        goes: process UFS-weather-model forecast output for simulated satellite imagery
        wafs: process UFS-weather-model forecast output for WAFS products

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment

        Returns
        -------
        None
        """
        super().__init__(config)

        if self.task_config.UPP_RUN not in self.VALID_UPP_RUN:
            raise NotImplementedError(f'{self.task_config.UPP_RUN} is not a valid UPP run type.\n' +
                                      'Valid UPP_RUN values are:\n' +
                                      f'{", ".join(self.VALID_UPP_RUN)}')

        valid_datetime = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"{self.task_config.FORECAST_HOUR}H"))

        # Extend task_config with localdict
        localdict = AttrDict(
            {'upp_run': self.task_config.UPP_RUN,
             'forecast_hour': self.task_config.FORECAST_HOUR,
             'valid_datetime': valid_datetime,
             'atmos_filename': f"atm_{valid_datetime.strftime('%Y%m%d%H%M%S')}.nc",
             'flux_filename': f"sfc_{valid_datetime.strftime('%Y%m%d%H%M%S')}.nc"
             }
        )
        self.task_config = AttrDict(**self.task_config, **localdict)

        # Read the upp.yaml file for common configuration
        logger.info(f"Read the UPP configuration yaml file {self.task_config.UPP_CONFIG}")
        self.task_config.upp_yaml = parse_j2yaml(self.task_config.UPP_CONFIG, self.task_config)
        logger.debug(f"upp_yaml:\n{pformat(self.task_config.upp_yaml)}")

    @staticmethod
    @logit(logger)
    def initialize(upp_yaml: Dict) -> None:
        """Initialize the work directory by copying all the common fix data

        Parameters
        ----------
        upp_yaml: Dict
            Fully resolved upp.yaml dictionary
        """

        # Copy static data to run directory
        logger.info("Copy static data to run directory")
        FileHandler(upp_yaml.upp.fix_data).sync()

    @staticmethod
    @logit(logger)
    def configure(upp_dict: Dict, upp_yaml: Dict) -> None:
        """Configure the artifacts in the work directory.
        Copy run specific data to run directory
        Create namelist 'itag' from template

        Parameters
        ----------
        upp_dict : Dict
            Task specific keys e.g. upp_run
        upp_yaml : Dict
            Fully resolved upp.yaml dictionary
        """

        # Copy "upp_run" specific data to run directory
        logger.info(f"Copy '{upp_dict.upp_run}' data to run directory")
        FileHandler(upp_yaml[upp_dict.upp_run].data_in).sync()

        # Make a localconf with the upp_run specific configuration
        # First make a shallow copy for local use
        localconf = upp_dict.copy()
        # Update 'config' part of the 'run'
        localconf.update(upp_yaml.upp.config)
        localconf.update(upp_yaml[localconf.upp_run].config)
        logger.debug(f"Updated localconf with upp_run='{localconf.upp_run}':\n{pformat(localconf)}")

        # Configure the namelist and write to file
        logger.info("Create namelist for upp.x")
        nml_template = os.path.join(localconf.DATA, "itag.jinja")
        nml_data = Jinja(nml_template, localconf).render
        logger.debug(f"itag:\n{nml_data}")
        nml_file = os.path.join(localconf.DATA, "itag")
        with open(nml_file, "w") as fho:
            fho.write(nml_data)

    @staticmethod
    @logit(logger)
    def execute(workdir: Union[str, os.PathLike], aprun_cmd: str, forecast_hour: int = 0) -> None:
        """Run the UPP executable and index the output master and flux files

        Parameters
        ----------
        workdir : str | os.PathLike
            work directory with the staged data, parm files, namelists, etc.
        aprun_cmd : str
            launcher command for UPP.x
        forecast_hour : int
            default: 0
            forecast hour being processed

        Returns
        -------
        None
        """

        # Run the UPP executable
        UPP.run(workdir, aprun_cmd)

        # Index the output grib2 file
        UPP.index(workdir, forecast_hour)

    @classmethod
    @logit(logger)
    def run(cls, workdir: Union[str, os.PathLike], aprun_cmd: str, exec_name: str = 'upp.x') -> None:
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

        Returns
        -------
        None
        """
        os.chdir(workdir)

        exec_cmd = Executable(aprun_cmd)
        exec_cmd.add_default_arg(os.path.join(workdir, exec_name))

        UPP._call_executable(exec_cmd)

    @classmethod
    @logit(logger)
    def index(cls, workdir: Union[str, os.PathLike], forecast_hour: int) -> None:
        """
        Index the grib2file

        Parameters
        ----------
        workdir : str | os.PathLike
            Working directory where to run containing the necessary files and executable
        forecast_hour : int
            forecast hour to index

        Environment Parameters
        ----------------------
        GRB2INDEX : str (optional)
            path to executable "grb2index"
            Typically set in the modulefile

        Returns
        -------
        None
        """
        os.chdir(workdir)
        logger.info("Generate index file")

        grb2index_cmd = os.environ.get("GRB2INDEX", None)

        template = f"GFS{{file_type}}.GrbF{forecast_hour:02d}"

        for ftype in ['PRS', 'FLX', 'GOES']:
            grbfile = template.format(file_type=ftype)
            grbfidx = f"{grbfile}.idx"

            if not os.path.exists(grbfile):
                logger.info(f"No {grbfile} to process, skipping ...")
                continue

            logger.info(f"Creating index file for {grbfile}")
            exec_cmd = which("grb2index") if grb2index_cmd is None else Executable(grb2index_cmd)
            exec_cmd.add_default_arg(os.path.join(workdir, grbfile))
            exec_cmd.add_default_arg(os.path.join(workdir, grbfidx))

            UPP._call_executable(exec_cmd)

    @staticmethod
    @logit(logger)
    def _call_executable(exec_cmd: Executable) -> None:
        """Internal method to call executable

        Parameters
        ----------
        exec_cmd : Executable
            Executable to run

        Raises
        ------
        OSError
            Failure due to OS issues
        WorkflowException
            All other exceptions
        """

        logger.info(f"Executing {exec_cmd}")
        try:
            exec_cmd()
        except OSError:
            logger.exception(f"FATAL ERROR: Failed to execute {exec_cmd}")
            raise OSError(f"{exec_cmd}")
        except Exception:
            logger.exception(f"FATAL ERROR: Error occurred during execution of {exec_cmd}")
            raise WorkflowException(f"{exec_cmd}")

    @staticmethod
    @logit(logger)
    def finalize(upp_run: Dict, upp_yaml: Dict) -> None:
        """Perform closing actions of the task.
        Copy data back from the DATA/ directory to COMOUT/

        Parameters
        ----------
        upp_run: str
           Run type of UPP
        upp_yaml: Dict
            Fully resolved upp.yaml dictionary
        """

        # Copy "upp_run" specific generated data to COMOUT/ directory
        logger.info(f"Copy '{upp_run}' processed data to COMOUT/ directory")
        FileHandler(upp_yaml[upp_run].data_out).sync()
