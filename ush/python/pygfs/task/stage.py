#!/usr/bin/env python3

import glob
import os
import shutil
from datetime import timedelta
from logging import getLogger
from typing import Any, Dict, List

from wxflow import (AttrDict, FileHandler, Hsi, Htar, Task, cast_strdict_as_dtypedict,
                    chgrp, get_gid, logit, mkdir_p, parse_j2yaml, rm_p, strftime,
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
    def determine_stage(self, stage_dict: Dict[str, Any]) -> (Dict[str, Any], List[Dict[str, Any]]):
        """Determine which initial condition files need to be placed in ROTDIR.

        Parameters
        ----------
        stage_dict : Dict[str, Any]
            Task specific keys, e.g. runtime options (DO_WAVE, DO_ICE, etc)

        Return
        ------
        stage_set : Dict[str, Any]
            Set of FileHandler instructions to copy files to the ROTDIR
        """

        stage_parm = os.path.join(stage_dict.PARMgfs, "stage")

        # Add the os.path.exists function to the dict for yaml parsing
        stage_dict['path_exists'] = os.path.exists

        if not os.path.isdir(stage_dict.ROTDIR):
            raise FileNotFoundError(f"FATAL ERROR: The ROTDIR ({stage_dict.ROTDIR}) does not exist!")

        if stage_dict.RUN == "gfs" or stage_dict.RUN == "gdas":
            if stage_dict.MODE == "cycled":
                master_yaml = "master_cycled.yaml.j2"
            elif stage_dict.MODE == "forecast-only":
                #master_yaml = "master_forecast_only.yaml.j2"
                master_yaml = "fv3_cold.yaml.j2"
        elif stage_dict.RUN == "gefs":
            raise NotImplementedError("FATAL ERROR: Staging is not yet set up for GEFS runs")
        elif stage_dict.RUN == "sfs":
            raise NotImplementedError("FATAL ERROR: Staging is not yet set up for SFS runs")
        else:
            raise ValueError(f"FATAL ERROR: Staging is not enabled for {stage_dict.RUN} runs")

       #parsed_sets = parse_j2yaml(os.path.join(stage_parm, master_yaml), stage_dict)
        stage_set = parse_j2yaml(os.path.join(stage_parm, master_yaml), stage_dict)
       #print(f'parsed_sets = {parsed_sets}')

       #stage_sets = []

       #for dataset in parsed_sets.datasets.values():

       #    dataset["fileset"] = Stage._create_fileset(dataset)

       #    stage_sets.append(dataset)

       #return stage_sets
        return stage_set

    @logit(logger)
    def execute_stage(self, stage_set: Dict[str, Any]) -> None:
        """Perform local staging of initial condition files.

        Parameters
        ----------
        stage_set : Dict[str, Any]
            FileHandler instructions to populate ROTDIR with

        Return
        ------
        None
        """

        # Copy files to ROTDIR
        for key in stage_set.keys():
        #   print(f'key = {key}')
            FileHandler(stage_set[key]).sync()
