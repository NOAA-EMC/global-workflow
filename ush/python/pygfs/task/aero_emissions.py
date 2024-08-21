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


class AerosolEmissions(Task):
    """Aerosol Emissions pre-processing Task
    """

    @logit(logger, name="AerosolEmissions")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the Aerosol Emissions task

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment

        Returns
        -------
        None
        """
        super().__init__(config)

        local_variable = "something"

        localdict = AttrDict(
            {'variable_used_repeatedly': local_variable}
        )

        # Extend task_config with localdict
        self.task_config = AttrDict(**self.task_config, **localdict)

    @staticmethod
    @logit(logger)
    def initialize() -> None:
        """Initialize the work directory
        """

    @staticmethod
    @logit(logger)
    def configure() -> None:
        """Configure the artifacts in the work directory.
        Copy run specific data to run directory
        """

    @staticmethod
    @logit(logger)
    def execute(workdir: Union[str, os.PathLike], aprun_cmd: str) -> None:
        """Run the executable (if any)

        Parameters
        ----------
        workdir : str | os.PathLike
            work directory with the staged data, parm files, namelists, etc.
        aprun_cmd : str
            launcher command for executable.x

        Returns
        -------
        None
        """

    @staticmethod
    @logit(logger)
    def finalize() -> None:
        """Perform closing actions of the task.
        Copy data back from the DATA/ directory to COM/
        """
