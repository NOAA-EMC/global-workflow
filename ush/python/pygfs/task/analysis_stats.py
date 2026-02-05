#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from logging import getLogger
from pprint import pformat
from typing import Optional, Dict, Any

from wxflow import (AttrDict,
                    FileHandler,
                    parse_j2yaml,
                    logit)
from pygfs.jedi import Jedi
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class AnalysisStats(Analysis):
    """
    Class for JEDI-based global analysis stats tasks
    """
    @logit(logger, name="AnalysisStats")
    def __init__(self, config: Dict[str, Any]):
        """
        Constructor global analysis stats task
        This method will construct a global analysis stats task.
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task
        - instantiate the Jedi attribute objects
        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration
        analysis: str
            type of analysis stats to be performed
        Returns
        ----------
        None
        """
        super().__init__(config)

        _outdir = {
            'atmos': self.task_config.COMOUT_ATMOS_ANLMON,
            'atmos_gsi': self.task_config.COMOUT_ATMOS_ANLMON,
        }
        _anldir = {
            'atmos': self.task_config.COMOUT_ATMOS_ANALYSIS,
            'atmos_gsi': self.task_config.COMOUT_ATMOS_ANALYSIS,
        }
        if self.task_config.DO_AERO_ANL:
            _outdir['aero'] = self.task_config.COMOUT_AERO_ANLMON
            _anldir['aero'] = self.task_config.COMOUT_AERO_ANALYSIS
        if self.task_config.DO_JEDISNOWDA:
            _outdir['snow'] = self.task_config.COMOUT_SNOW_ANLMON
            _anldir['snow'] = self.task_config.COMOUT_SNOW_ANALYSIS

        # Create a local dictionary that is repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                #
                'outdir': _outdir,
                'anldir': _anldir,
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Create dictionary of Jedi objects
        expected_keys = self.task_config.STAT_ANALYSES
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """
        This method will initialize a global analysis stats task.
        This includes:
        - initialize JEDI applications
        - copying stat files
        Parameters
        ----------
        None
        Returns
        ----------
        None
        """

        for analysis in self.task_config.STAT_ANALYSES:
            # Loop through a copy of ob space list
            logger.info(f"Working on analysis type: {analysis}")

            # Stage files from COM
            logger.info(f"Staging files from COM and creating output directories")
            FileHandler(self.task_config.data_in).sync()

            # Extract diag tar file
            jcb_config = self.jedi_dict[analysis].jcb_config
            component = self.jedi_dict[analysis].component
            diag_archive = os.path.join(jcb_config[f"{component}_obsdatain_path"],
                                        f"{self.task_config.APREFIX}{analysis}_analysis.ioda_hofx.tar.gz")
            Jedi.extract_tar(diag_archive)

            # Initialize JEDI application
            logger.info(f"Initializing JEDI ioda-stats extraction application")
            self.jedi_dict[analysis].initialize(clean_empty_obsspaces=True)

    @logit(logger)
    def execute(self, analysis: str) -> None:
        """Execute JEDI application of analysis stats

        Parameters
        ----------
        analysis
            key specifying particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        self.jedi_dict[analysis].execute()

    @logit(logger)
    def finalize(self, analysis: str) -> None:
        """Finalize the analysis statistics job.

        This method will finalize the analysis statistics job using JEDI.
        This includes:
        - copying stat files to specified outdir
        - tar and gzip stat files

        Parameters
        ----------
        analysis
            key specifying particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        for analysis in self.task_config.STAT_ANALYSES:
            self.jedi_dict[analysis].save_obsdataout(self.task_config.outdir[analysis],
                                                     f"{self.task_config.APREFIX}{analysis}_analysis.ioda_hofx_stats")

            # concatenate text files into one summary file
            jcb_config = self.jedi_dict[analysis].jcb_config
            component = self.jedi_dict[analysis].component
            summaryfile = os.path.join(jcb_config[f"{component}_obsdataout_path"], f"{self.task_config.APREFIX}{analysis}_stats.txt")
            with open(summaryfile, 'w') as outfile:
                for ob in self.jedi_dict[analysis].jcb_config.observations:
                    textfile = os.path.join(jcb_config[f"{component}_obsdataout_path"], f"{ob}_ioda_stats.txt")
                    if os.path.exists(textfile):
                        logger.info(f"Concatenating {textfile} to {summaryfile}")
                        with open(textfile, 'r') as infile:
                            outfile.write(infile.read())
                    else:
                        logger.warning(f"{textfile} does not exist to concatenate.")
                        logger.warning("Skipping this file ...")

        # Save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()

    @logit(logger)
    def convert_gsi_diags(self) -> None:
        """Convert GSI diag files to ioda-stat files for analysis stats

        This method will convert GSI diag files to ioda-stat files for analysis stats.
        This includes:
        - copying GSI diag files to DATA path
        - untarring and gunzipping GSI diag files
        - converting GSI diag files to ioda files using gsincdiag2ioda converter scripts

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """
        logger.info("Not supported for GCAFS workflow.")
