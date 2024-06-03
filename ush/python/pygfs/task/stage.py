#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Any, Dict, List

from wxflow import (AttrDict, FileHandler, Task, cast_strdict_as_dtypedict,
                    logit, parse_j2yaml, strftime,
                    to_YMD, to_YMDH, Template, TemplateConstants)

logger = getLogger(__name__.split('.')[-1])

class Stage(Task):
    """Task to stage initial conditions
    """

    @logit(logger, name="Stage")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the Stage task
        The constructor is responsible for collecting necessary yamls based on
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

        rotdir = self.config.ROTDIR + os.sep

        self.task_config = AttrDict(**self.config, **self.runtime_config)

    @logit(logger)
    def determine_stage(self, stage_dict: Dict[str, Any]) -> List[str]:
        """Determine which initial condition files need to be placed in ROTDIR.

        Parameters
        ----------
        stage_dict : Dict[str, Any]
            Task specific keys, e.g. runtime options (DO_WAVE, DO_ICE, etc)

        Return
        ------
        stage_sets : List
            List of yamls to parse for staging
        """

        stage_sets = []

        if stage_dict.EXP_WARM_START:
            stage_sets.append("fv3_warm.yaml.j2")
        else:
            stage_sets.append("fv3_cold.yaml.j2")

        if stage_dict.DO_WAVE:
            stage_sets.append("wave.yaml.j2")

        if stage_dict.DO_OCN:
            stage_sets.append("ocean.yaml.j2")

        if stage_dict.DO_ICE:
            stage_sets.append("ice.yaml.j2")

        if stage_dict.DO_NEST:
            stage_sets.append("fv3_nest.yaml.j2")

        return stage_sets

    @logit(logger)
    def execute_stage(self, stage_dict: Dict[str, Any], stage_sets: List) -> None:
        """Perform local staging of initial condition files.

        Parameters
        ----------
        stage_sets : List
            List of stage sets to send to FileHandler
        stage_set : Dict[str, Any]
            FileHandler instructions to populate ROTDIR with

        Return
        ------
        None
        """

        stage_parm = os.path.join(stage_dict.PARMgfs, "stage")

        if not os.path.isdir(stage_dict.ROTDIR):
            raise FileNotFoundError(f"FATAL ERROR: The ROTDIR ({stage_dict.ROTDIR}) does not exist!")

        for set_yaml in stage_sets:

            stage_set = parse_j2yaml(os.path.join(stage_parm, set_yaml), stage_dict)

            # Copy files to ROTDIR
            for key in stage_set.keys():
                FileHandler(stage_set[key]).sync()
