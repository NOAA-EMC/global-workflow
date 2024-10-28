#!/usr/bin/env python3

import glob
import os
from logging import getLogger
from typing import Any, Dict, List

from wxflow import (AttrDict, FileHandler, Task, cast_strdict_as_dtypedict,
                    logit, parse_j2yaml, strftime, to_YMD, to_YMDH,
                    add_to_datetime, to_timedelta, Template, TemplateConstants,
                    Hsi, Htar, which)

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
        YYYYMMDDHH = to_YMDH(stage_dict.current_cycle)
        YYYY = YYYYMMDDHH[0:4]
        MM = YYYYMMDDHH[4:6]

        if stage_dict.DO_DOWNLOAD_ICS is True:
            # Download ICs from HPSS to ICSDIR
            self.htar = Htar()
            self.xvf = self.htar.xvf
            self.xvf(stage_dict.HPSSICARCH + "/" + YYYYMMDDHH + ".tar")

        if stage_dict.DO_REPAIR_REPLAY and stage_dict.DO_DOWNLOAD_ANLY is True:
            # Download f03 replay analysis
            aws_cmd = which("aws")
            aws_cmd.add_default_arg("s3")
            aws_cmd.add_default_arg("cp")
            aws_cmd.add_default_arg("--no-sign-request")
            aws_url = "s3://noaa-ufs-gefsv13replay-pds/"

            aws_cmd(aws_url + YYYY + "/" + MM + "/" + YYYYMMDDHH + "/GFSPRS.GrbF03", "./")
            aws_cmd(aws_url + YYYY + "/" + MM + "/" + YYYYMMDDHH + "/GFSFLX.GrbF03", "./")

        if not os.path.isdir(stage_dict.ROTDIR):
            raise FileNotFoundError(f"FATAL ERROR: The ROTDIR ({stage_dict.ROTDIR}) does not exist!")

        # Add the os.path.exists function to the dict for yaml parsing
        stage_dict['path_exists'] = os.path.exists

        # Add the glob.glob function for capturing filenames
        stage_dict['glob'] = glob.glob

        # Parse stage yaml to get list of files to copy
        stage_set = parse_j2yaml(self.task_config.STAGE_IC_YAML_TMPL, stage_dict, allow_missing=False)

        # Copy files to ROTDIR
        for key in stage_set.keys():
            FileHandler(stage_set[key]).sync()
