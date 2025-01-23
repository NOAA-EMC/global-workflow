#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Any, Dict, List

from wxflow import (AttrDict, Task, to_YMD, strftime, logit, parse_yaml)

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

        # Declare these here so the jinja-templated scripts can be shellchecked
        cycle_YMD = to_YMD(self.task_config.current_cycle),
        cycle_HH = strftime(self.task_config.current_cycle, '%H')

        local_dict = AttrDict({
            'sven_dropbox': (f"{self.task_config.SVEN_DROPBOX_ROOT}/"
                             f"{self.task_config.PSLOT}/{self.task_config.RUN}.{cycle_YMD}/{cycle_HH}"),
            'doorman_gendel': (f"{self.task_config.GENERAL_DELIVERY_ROOT}/"
                               f"{self.task_config.PSLOT}/{self.task_config.RUN}.{cycle_YMD}/{cycle_HH}")
        })

        self.task_config = AttrDict(**self.task_config, **local_dict)

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
        print(globus_parm)

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
    def execute_transfer_data(self, tarball_set: Dict[str, Any]) -> None:
        """Interface function with Sven to send tarballs to HPSS via Niagara.

        Parameters
        ----------
        tarball_set: Dict[str, Any]
            Set of tarballs and properties to applicable to their transfer.

        Return
        ------
        None
        """

    pass

    @logit(logger)
    def clean(self):
        """
        Remove the temporary directories/files created by the GlobusHpss task.
        """

        return
