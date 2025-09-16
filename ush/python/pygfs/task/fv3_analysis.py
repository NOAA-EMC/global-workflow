#!/usr/bin/env python3

from logging import getLogger
from typing import Any, Dict
from wxflow import (AttrDict, Task,
                    add_to_datetime, to_timedelta, to_isotime,
                    parse_j2yaml,
                    logit)

logger = getLogger(__name__.split('.')[-1])


class FV3Analysis(Task):
    """
    General class for JEDI-based global FV3 analysis tasks
    """
    @logit(logger, name="FV3Analysis")
    def __init__(self, config: Dict[str, Any]):
        """Constructor global atm analysis task

        This method will construct a global atm analysis task.
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

        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        _iau_times_iso = []
        for hour in self.task_config.IAUFHRS:
            _iau_times_iso.append(to_isotime(_window_begin + to_timedelta(f"{str(hour)}H") - to_timedelta(f"{self.task_config.assim_freq}H") / 2))

        # Extend task_config with variables that are repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'npz_ges': self.task_config.LEVS - 1,
                'npz_anl': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'WINDOW_BEGIN': _window_begin,
                'WINDOW_LENGTH': f"PT{self.task_config.assim_freq}H",
                'OPREFIX': f"{self.task_config.RUN.replace('enkf','')}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN.replace('enkf','')}.t{self.task_config.cyc:02d}z.",
                'APREFIX_ENS': f"enkf{self.task_config.RUN.replace('enkf','')}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'GPREFIX_ENS': f"enkfgdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'iau_times_iso': _iau_times_iso,
                'BKG_TSTEP': "PT1H",  # Placeholder for 4D applications
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

    def initialize(self) -> None:
        self.initialize()

    def execute(self) -> None:
        super.execute()

    def finalize(self) -> None:
        super.finalize()

    def clean(self):
        super().clean()
