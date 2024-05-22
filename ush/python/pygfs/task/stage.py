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

        # Find all absolute paths in the environment and get their relative paths from ${ROTDIR}
        path_dict = self._gen_relative_paths(rotdir)

        self.task_config = AttrDict(**self.config, **self.runtime_config, **path_dict)

    @logit(logger)
    def _gen_relative_paths(self, root_path: str) -> Dict:
        """Generate a dict of paths in self.config relative to root_path

        Parameters
        ----------
        root_path : str
            Path to base all relative paths off of

        Return
        ------
        rel_path_dict : Dict
            Dictionary of paths relative to root_path.  Members will be named
            based on the dict names in self.config.  For COM paths, the names will
            follow COM_<NAME> --> <name>_dir.  For all other directories, the
            names will follow <NAME> --> <name>_dir.
        """

        rel_path_dict = {}
        for key, value in self.config.items():
            if isinstance(value, str):
                if root_path in value:
                    rel_path = value.replace(root_path, "")
                    rel_key = (key[4:] if key.startswith("COM_") else key).lower() + "_dir"
                    rel_path_dict[rel_key] = rel_path

        return rel_path_dict

    @staticmethod
    @logit(logger)
    def _create_fileset(stage_set: Dict[str, Any]) -> List:
        """
        Collect the list of all available files from the parsed yaml dict.
        Globs are expanded and if required files are missing, an error is
        raised.

        TODO: expand all globs in the jinja yaml files instead of expanding
              them here and issue errors here if globbing patterns (*, ?, [])
              are found.

        Parameters
        ----------
        stage_set: Dict
            Contains full paths for required and optional files to be staged.
        """

        fileset = []
        if "required" in stage_set:
            if stage_set.required is not None:
                for item in stage_set.required:
                    glob_set = glob.glob(item)
                    if len(glob_set) == 0:
                        raise FileNotFoundError(f"FATAL ERROR: Required file, directory, or glob {item} not found!")
                    for entry in glob_set:
                        fileset.append(entry)

        if "optional" in stage_set:
            if stage_set.optional is not None:
                for item in stage_set.optional:
                    glob_set = glob.glob(item)
                    if len(glob_set) == 0:
                        logger.warning(f"WARNING: optional file/glob {item} not found!")
                    else:
                        for entry in glob_set:
                            fileset.append(entry)

        return fileset

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
