#!/usr/bin/env python3

import os
from logging import getLogger
from typing import List, Dict, Any, Union
from pprint import pformat

from wxflow import (parse_j2yaml, FileHandler, logit,
                    Task, Executable, WorkflowException)

logger = getLogger(__name__.split('.')[-1])


class BMatrix(Task):
    """Parent class for GDAS BMatrix tasks

    The BMatrix class is the parent class for all
    Global Data Assimilation System (GDAS) BMatrix tasks
    """
    def __init__(self, config: Dict[str, Any]) -> None:
        super().__init__(config)
        self.config.ntiles = 6
        # Store location of GDASApp jinja2 templates
        self.gdasapp_j2tmpl_dir = os.path.join(self.config.PARMgfs, 'gdas')

    def initialize(self) -> None:
        super().initialize()
        # all BMatrix tasks need a config
        self.task_config.bmat_config = self.get_bmat_config()

    def finalize(self) -> None:
        super().finalize()

    @logit(logger)
    def get_bmat_config(self) -> Dict[str, Any]:
        """Compile a dictionary of B Matrix configuration from BMATYAML template file

        Parameters
        ----------

        Returns
        ----------
        bmat_config : Dict
            a dictionary containing the fully rendered B matrix yaml configuration
        """

        # generate JEDI YAML file
        logger.info(f"Generate B Matrix YAML config: {self.task_config.bmat_yaml}")
        bmat_config = parse_j2yaml(self.task_config.BMATYAML, self.task_config, searchpath=self.gdasapp_j2tmpl_dir)
        logger.debug(f"BMAT config:\n{pformat(bmat_config)}")

        return bmat_config
