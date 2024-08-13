#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Any, Dict, List

from wxflow import (AttrDict, FileHandler, Task, cast_strdict_as_dtypedict,
                    logit, parse_j2yaml, strftime, to_YMD,
                    add_to_datetime, to_timedelta, Template, TemplateConstants)

logger = getLogger(__name__.split('.')[-1])


class Stage(Task):
    """Task to stage initial conditions
    """

    @logit(logger, name="Stage")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the Stage task
        The constructor is responsible for collecting necessary settings based on
        the runtime options and RUN.

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment

        Returns
        -------
        None
        """
        super().__init__(config)

        self.task_config = AttrDict(**self.task_config)

    @logit(logger)
    def configure(self, stage_dict: Dict[str, Any]) -> (Dict[str, Any]):
        """Determine stage settings based on configuration and runtime options.

        Parameters
        ----------
        stage_dict : Dict[str, Any]
            Configuration dictionary

        Returns
        -------
        stage_dict : Dict[str, Any]
            Configuration dictionary updated
        """

        # Add the os.path.exists function to the dict for yaml parsing
        stage_dict['path_exists'] = os.path.exists

        # Determine model start date
        current_cycle_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config['assim_freq']}H") / 2)
        current_cycle_end = add_to_datetime(self.task_config.current_cycle, to_timedelta(f"{self.task_config['assim_freq']}H") / 2)

        if self.task_config.DOIAU and self.task_config.MODE == "cycled":
            model_start_date_current_cycle = current_cycle_begin
        else:
            if self.task_config.REPLAY_ICS:
                model_start_date_current_cycle = current_cycle_end
            else:
                model_start_date_current_cycle = self.task_config.current_cycle

        stage_dict['model_start_date_current_cycle'] = model_start_date_current_cycle

        # Determine restart RUN
        rRUN = self.task_config.RUN
        if self.task_config.RUN == "gfs":
            rRUN = "gdas"
        stage_dict['rRUN'] = rRUN

        # Determine ensemble member settings
        MEM_START = -1  # Deterministic default, no members
        if self.task_config.NMEM_ENS > 0:
            if self.task_config.RUN == "gefs":
                MEM_START = 0
            elif self.task_config.RUN == "enkfgdas":
                MEM_START = 1

        if MEM_START >= 0:  # Ensemble RUN
            stage_dict['first_mem'] = MEM_START
            stage_dict['last_mem'] = self.task_config.NMEM_ENS
        else:  # Deteministic RUN
            stage_dict['first_mem'] = MEM_START
            stage_dict['last_mem'] = MEM_START

        return stage_dict

    @logit(logger)
    def execute_stage(self, stage_dict: Dict[str, Any]) -> None:
        """Perform local staging of initial condition files.

        Parameters
        ----------
        stage_dict : Dict[str, Any]
            Configuration dictionary

        Returns
        -------
        None
        """

        if not os.path.isdir(stage_dict.ROTDIR):
            raise FileNotFoundError(f"FATAL ERROR: The ROTDIR ({stage_dict.ROTDIR}) does not exist!")

        # Loop over members
        for mem in range(stage_dict.first_mem, stage_dict.last_mem + 1):
            stage_dict['mem'] = mem
            stage_set = parse_j2yaml(self.task_config.STAGE_IC_YAML_TMPL, stage_dict, allow_missing=False)
            # Copy files to ROTDIR
            for key in stage_set.keys():
                FileHandler(stage_set[key]).sync()
