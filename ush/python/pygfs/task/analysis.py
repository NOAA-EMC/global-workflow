#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Any, Dict
from wxflow import (AttrDict, Task, WorkflowException, Executable,
                    add_to_datetime, to_timedelta, to_isotime,
                    parse_j2yaml,
                    logit)

logger = getLogger(__name__.split('.')[-1])


class Analysis(Task):
    """
    General class for JEDI-based global analysis tasks
    """
    @logit(logger, name="Analysis")
    def __init__(self, config: Dict[str, Any]):
        """Constructor global analysis task

        This method will construct a global analysis task.
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

        # Get assimilation window times
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)
        _window_end = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"{self.task_config.assim_freq}H") / 2)
        _next_cycle = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"{self.task_config.assim_freq}H"))

        # Get specific assimilation times within the assimulation window
        _iau_times_iso = []
        for hour in self.task_config.IAUFHRS:
            _iau_times_iso.append(to_isotime(_window_begin + to_timedelta(f"{str(hour)}H") - to_timedelta(f"{self.task_config.assim_freq}H") / 2))

        # Set prefix needed for GPREFIX, depedning on the model
        if self.task_config.NET == 'gcafs':
            _da_prefix = 'gcdas'
        else:
            _da_prefix = 'gdas'

        # Map ocean resolution to number of vertical levels
        _ocnres_to_nlev = {'500': 25,
                           '100': 75,
                           '050': 75,
                           '025': 75}

        # Extend task_config with variables that are repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'WINDOW_BEGIN': _window_begin,
                'WINDOW_MIDDLE': self.task_config.current_cycle,
                'WINDOW_END': _window_end,
                'WINDOW_LENGTH': f"PT{self.task_config.assim_freq}H",
                'next_cycle': _next_cycle,
                'OPREFIX': f"{self.task_config.RUN.replace('enkf', '')}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN.replace('enkf', '')}.t{self.task_config.cyc:02d}z.",
                'APREFIX_ENS': f"enkf{self.task_config.RUN.replace('enkf', '')}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"{_da_prefix}.t{self.task_config.previous_cycle.hour:02d}z.",
                'GPREFIX_ENS': f"enkf{_da_prefix}.t{self.task_config.previous_cycle.hour:02d}z.",
                'iau_times_iso': _iau_times_iso,
                'MOM6_LEVS': _ocnres_to_nlev[f"{self.task_config.OCNRES:03d}"],
                'mom_domain_stack_size': 116640000,  # TODO: Make the stack size resolution dependent
            }
        ))

    def initialize(self) -> None:
        self.initialize()

    def execute(self) -> None:
        super.execute()

    def finalize(self) -> None:
        super.finalize()

    def clean(self) -> None:
        super().clean()

    @staticmethod
    @logit(logger)
    def run(exec_cmd: Executable) -> None:
        """Run the executable command
        This method will run the executable command
        Parameters
        ----------
        exec_cmd: Executable
            executable command to run
        Returns
        ----------
        None
        """

        logger.info(f"Executing {exec_cmd}")
        try:
            exec_cmd()
        except WorkflowException as e:
            raise WorkflowException(f"An error occurred during execution of {exec_cmd}:\n{e}") from e
