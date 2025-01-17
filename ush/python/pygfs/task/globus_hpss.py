#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Any, Dict, List

from wxflow import (AttrDict, Task,
                    logit, parse_yaml)

logger = getLogger(__name__.split('.')[-1])


class GlobusHpss(Task):
    """Task to send tarballs (created by the archive task) to HPSS via Globus
    """

    @logit(logger, name="GlobusHpss")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the GlobusHpss task

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
    def configure(self, globus_dict: Dict[str, Any]) -> (Dict[str, Any], List[Dict[str, Any]]):
        """Collects the list of tarballs created by the arch task and writes
        instructions to send them to HPSS via Globus.

        Parameters
        ----------
        globus_dict : Dict[str, Any]
            Task specific keys, e.g. the name of the input YAML.

        Return
        ------
        globus_targets : List[Dict[str, Any]]
            List of tarballs and instructions for sending them to HPSS via Globus
        """

        globus_parm = os.path.join(globus_dict.PARMgfs, "globus")

        com_conf = globus_dict.COMIN_CONF

        # Collect the files and properties from the input YAML
        backup_yaml = os.path.join(com_conf, globus_dict.DATASETS_YAML)

        backup_set = AttrDict(**parse_yaml(backup_yaml))

        globus_instructions = []
        for name in backup_set.values():

            tarball = backup_set[name].target
            if backup_set[name].has_rstprod:
                globus_instructions.append(self._sven_rstprod_instructions(tarball))
            else:
                globus_instructions.append(self._sven_instructions(tarball))

        return globus_instructions

    @logit(logger)
    def execute_transfer_data(self, atardir_set: Dict[str, Any]) -> None:
        """Create a backup tarball from a yaml dict.

        Parameters
        ----------
        atardir_set: Dict[str, Any]
            Dict defining set of files to backup and the target tarball.

        Return
        ------
        None
        """

        if atardir_set.has_rstprod:

            try:
                self.cvf(atardir_set.target, atardir_set.fileset)
            # Regardless of exception type, attempt to remove the target
            except Exception:
                self.rm_cmd(atardir_set.target)
                raise RuntimeError(f"FATAL ERROR: Failed to create restricted archive {atardir_set.target}, deleting!")

            self._protect_rstprod(atardir_set)

        else:
            self.cvf(atardir_set.target, atardir_set.fileset)

    @logit(logger)
    def _protect_rstprod(self, atardir_set: Dict[str, Any]) -> None:
        """
        Changes the group of the target tarball to rstprod and the permissions to
        640.  If this fails for any reason, attempt to delete the file before exiting.

        """

        pass

    @logit(logger)
    def clean(self):
        """
        Remove the temporary directories/files created by the GlobusHpss task.
        Presently, this is only the ROTDIR/expdir directory if EXPDIR archiving
        was performed.
        """

        return
