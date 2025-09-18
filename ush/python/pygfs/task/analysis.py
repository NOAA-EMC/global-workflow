#!/usr/bin/env python3

from logging import getLogger
import os
import tarfile
from typing import Any, Dict
from wxflow import (AttrDict, Task,
                    add_to_datetime, to_timedelta, to_isotime,
                    parse_j2yaml,
                    logit)

logger = getLogger(__name__.split('.')[-1])


class Analysis(Task):
    """
    General class for JEDI-based global FV3 analysis tasks
    """
    @logit(logger, name="FV3Analysis")
    def __init__(self, config: Dict[str, Any]):
        """Constructor global atm analysis task

        This method will construct a global atm analysis task.
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task

        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration

        Returns
        ----------
        None
        """
        super().__init__(config)

        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        _iau_times_iso = []
        for hour in self.task_config.IAUFHRS:
            _iau_times_iso.append(to_isotime(_window_begin + to_timedelta(f"{str(hour)}H") - to_timedelta(f"{self.task_config.assim_freq}H") / 2))

        if 'OBS_LIST_YAML' in self.task_config:
            _observations = parse_j2yaml(self.task_config.OBS_LIST_YAML, self.task_config)['observations']
        else:
            _observations = []
        if 'BIAS_FILES_YAML' in self.task_config:
            _bias_files = parse_j2yaml(self.task_config.BIAS_FILES_YAML, self.task_config)['bias_files']
        else:
            _bias_files = {}

        # Extend task_config with variables that are repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                'WINDOW_BEGIN': _window_begin,
                'WINDOW_LENGTH': f"PT{self.task_config.assim_freq}H",
                'BKG_TSTEP': "PT1H",  # Placeholder for 4D applications
                'OPREFIX': f"{self.task_config.RUN.replace('enkf','')}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN.replace('enkf','')}.t{self.task_config.cyc:02d}z.",
                'APREFIX_ENS': f"enkf{self.task_config.RUN.replace('enkf','')}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'GPREFIX_ENS': f"enkfgdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'iau_times_iso': _iau_times_iso,
                'observations': _observations,
                'bias_files': _bias_files,
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

    def initialize(self) -> None:
        self.initialize()

    def execute(self) -> None:
        super.execute()

    def finalize(self) -> None:
        super.finalize()

    def clean(self):
        super().clean()

    @logit(logger)
    def tar_diag_files(self, comout, tarball_name):
        """Compress and tar diag files into COM directory

        Parameters
        ----------
        comout: str
            path to COM output directory
        tarball_name: str
            name of output tar file

        Returns
        ----------
        None
        """

        # Set paths of output tar files
        diagtar = os.path.join(comout, tarball_name)

        # Get lists of files to put in tarballs
        diaglist = glob.glob(os.path.join(self.task_config.DATA, 'diags', 'diag*nc'))

        # Compress diag files
        logger.info(f"Compressing {len(diaglist)} diag files")
        for diagfile in diaglist:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # Create tarball of compressed diag files in COM
        logger.debug(f"Creating tarball {diagtar} with {len(diaglist)} compressed diag files")
        with tarfile.open(diagtar, "w") as archive:
            for diagfile in diaglist:
                diaggzip = f"{diagfile}.gz"
                archive.add(diaggzip, arcname=os.path.basename(diaggzip))

    def tar_radiative_bias_corrections(self, comout, tarball_name):
        """Tar radiative bias correction files and into COM directory

        Parameters
        ----------
        comout: str
            path to COM output directory
        tarball_name: str
            name of output tar file

        Returns
        ----------
        None
        """

        # Set paths of output tar files
        radtar = os.path.join(comout, tarball_name")

        # Get lists of files to put in tarballs
        satlist = glob.glob(os.path.join(self.task_config.DATA, 'bc', '*satbias*nc'))
        tlaplist = glob.glob(os.path.join(self.task_config.DATA, 'obs', '*tlapse.txt'))

        # Create tarball of radiance bias correction files
        logger.info(f"Creating radiance bias correction tarball {radtar}")
        with tarfile.open(radtar, 'w') as radbcor:
            logger.info(f"Adding {radbcor.getnames()}")
            for satfile in satlist:
                radbcor.add(satfile, arcname=os.path.basename(satfile))
            for tlapfile in tlaplist:
                # Change OPREFIX to APREFIX in tlapse file name when adding to tarball
                radbcor.add(tlapfile, arcname=os.path.basename(tlapfile.replace(self.task_config.OPREFIX, self.task_config.APREFIX)))

    @staticmethod
    @logit(logger)
    def extract_tar(tar_file: str) -> None:
        """Extract files from a tarball

        This method extract files from a tarball

        Parameters
        ----------
        tar_file
            path/name of tarball

        Returns
        ----------
        None
        """

        # extract files from tar file
        tar_path = os.path.dirname(tar_file)
        try:
            with tarfile.open(tar_file, "r") as tarball:
                tarball.extractall(path=tar_path)
                logger.info(f"Extract {tarball.getnames()}")
        except Exception as e:
            raise WorkflowException(f"An error occurred while extracting {tar_file}:\n{e}") from e