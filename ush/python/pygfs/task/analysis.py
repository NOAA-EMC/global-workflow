#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Any, Dict
from wxflow import (AttrDict, Task,
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
                'OCNRES': f"{self.task_config.OCNRES:03d}",
                'iau_times_iso': _iau_times_iso,
                'snow_bkg_path': os.path.join('.', 'bkg/'),  # TODO: remove this line
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
