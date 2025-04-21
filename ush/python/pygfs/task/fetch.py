#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Any, Dict

from wxflow import (Hsi, Task, htar,
                    logit, parse_j2yaml, chdir)
# import tarfile


logger = getLogger(__name__.split('.')[-1])


class Fetch(Task):
    """Task to pull ROTDIR data from HPSS (or locally)
    """

    @logit(logger, name="Fetch")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the Fetch task
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

    @logit(logger)
    def configure(self, fetch_dict: Dict[str, Any]):
        """Determine which tarballs will need to be extracted

        Parameters
        ----------
        fetch_dict : Dict[str, Any]
            Task specific keys, e.g. COM directories, etc

        Return
        ------
        parsed_fetch: Dict[str, Any]
           Dictionary derived from the yaml file with necessary HPSS info.
        """
        self.hsi = Hsi()

        fetch_yaml = fetch_dict.FETCH_YAML_TMPL
        fetch_parm = os.path.join(fetch_dict.PARMgfs, "fetch")

        parsed_fetch = parse_j2yaml(os.path.join(fetch_parm, fetch_yaml),
                                    fetch_dict)
        return parsed_fetch

    @logit(logger)
    def execute_pull_data(self, fetchdir_set: Dict[str, Any]) -> None:
        """Pull data from HPSS based on a yaml dictionary and store at the
           specified destination.

        Parameters
        ----------
        fetchdir_set: Dict[str, Any],
            Dict defining set of tarballs to pull and where to put them.

        Return
            None
        """

        f_names = fetchdir_set.target.contents
        if len(f_names) <= 0:     # Abort if no files
            raise FileNotFoundError("FATAL ERROR: The tar ball has no files")

        on_hpss = fetchdir_set.target.on_hpss
        dest = fetchdir_set.target.destination
        tarball = fetchdir_set.targettarball

        # Select action whether no_hpss is True or not, and pull these
        #    data from tape or locally and place where it needs to go
        # DG - these need testing
        with chdir(dest):
            logger.info(f"Changed working directory to {dest}")
            if on_hpss is True:  # htar all files in fnames
                htar_obj = htar.Htar()
                htar_obj.xvf(tarball, f_names)
            else:  # tar all files in fnames
                raise NotImplementedError("The fetch job does not yet support pulling from local archives")

#                with tarfile.open(dest, "w") as tar:
#                    for filename in f_names:
#                        tar.add(filename)
            # Verify all data files were extracted
            missing_files = []
            for f in f_names:
                if not os.path.exists(f):
                    missing_files.append(f)
            if len(missing_files) > 0:
                message = "Failed to extract all required files.  Missing files:\n"
                for f in missing_files:
                    message += f"{f}\n"

                raise FileNotFoundError(message)
