#!/usr/bin/env python3

import os
import glob
import tarfile
from logging import getLogger
from netCDF4 import Dataset
from typing import List, Dict, Any, Union

from pygw.yaml_file import parse_j2yaml
from pygw.file_utils import FileHandler
from pygw.fsutils import rm_p
from pygw.logger import logit
from pygw.task import Task
from pygw.executable import Executable
from pygw.exceptions import WorkflowException

logger = getLogger(__name__.split('.')[-1])


class Analysis(Task):
    """Parent class for GDAS tasks

    The Analysis class is the parent class for all
    Global Data Assimilation System (GDAS) tasks
    directly related to peforming an analysis
    """

    def __init__(self, config: Dict[str, Any]) -> None:
        super().__init__(config)
        self.config.ntiles = 6

    def initialize(self) -> None:
        super().initialize()
        # all analyses need to stage observations
        obs_dict = self.get_obs_dict()
        FileHandler(obs_dict).sync()

        # some analyses need to stage bias corrections
        bias_dict = self.get_bias_dict()
        FileHandler(bias_dict).sync()

        # link jedi executable to run directory
        self.link_jediexe()

    @logit(logger)
    def get_obs_dict(self) -> Dict[str, Any]:
        """Compile a dictionary of observation files to copy

        This method uses the OBS_LIST configuration variable to generate a dictionary
        from a list of YAML files that specify what observation files are to be
        copied to the run directory from the observation input directory

        Parameters
        ----------

        Returns
        ----------
        obs_dict: Dict
            a dictionary containing the list of observation files to copy for FileHandler
        """
        logger.debug(f"OBS_LIST: {self.task_config['OBS_LIST']}")
        obs_list_config = parse_j2yaml(self.task_config["OBS_LIST"], self.task_config)
        logger.debug(f"obs_list_config: {obs_list_config}")
        # get observers from master dictionary
        observers = obs_list_config['observers']
        copylist = []
        for ob in observers:
            obfile = ob['obs space']['obsdatain']['engine']['obsfile']
            basename = os.path.basename(obfile)
            copylist.append([os.path.join(self.task_config['COM_OBS'], basename), obfile])
        obs_dict = {
            'mkdir': [os.path.join(self.runtime_config['DATA'], 'obs')],
            'copy': copylist
        }
        return obs_dict

    @logit(logger)
    def get_bias_dict(self) -> Dict[str, Any]:
        """Compile a dictionary of observation files to copy

        This method uses the OBS_LIST configuration variable to generate a dictionary
        from a list of YAML files that specify what observation bias correction files
        are to be copied to the run directory from the observation input directory

        Parameters
        ----------

        Returns
        ----------
        bias_dict: Dict
            a dictionary containing the list of observation bias files to copy for FileHandler
        """
        logger.debug(f"OBS_LIST: {self.task_config['OBS_LIST']}")
        obs_list_config = parse_j2yaml(self.task_config["OBS_LIST"], self.task_config)
        logger.debug(f"obs_list_config: {obs_list_config}")
        # get observers from master dictionary
        observers = obs_list_config['observers']
        copylist = []
        for ob in observers:
            if 'obs bias' in ob.keys():
                obfile = ob['obs bias']['input file']
                obdir = os.path.dirname(obfile)
                basename = os.path.basename(obfile)
                prefix = '.'.join(basename.split('.')[:-2])
                for file in ['satbias.nc4', 'satbias_cov.nc4', 'tlapse.txt']:
                    bfile = f"{prefix}.{file}"
                    copylist.append([os.path.join(self.task_config.COM_ATMOS_ANALYSIS_PREV, bfile), os.path.join(obdir, bfile)])

        bias_dict = {
            'mkdir': [os.path.join(self.runtime_config.DATA, 'bc')],
            'copy': copylist
        }
        return bias_dict

    @logit(logger)
    def add_fv3_increments(self, inc_file_tmpl: str, bkg_file_tmpl: str, incvars: List) -> None:
        """Add cubed-sphere increments to cubed-sphere backgrounds

        Parameters
        ----------
        inc_file_tmpl : str
           template of the FV3 increment file of the form: 'filetype.tile{tilenum}.nc'
        bkg_file_tmpl : str
           template of the FV3 background file of the form: 'filetype.tile{tilenum}.nc'
        incvars : List
           List of increment variables to add to the background
        """

        for itile in range(1, self.config.ntiles + 1):
            inc_path = inc_file_tmpl.format(tilenum=itile)
            bkg_path = bkg_file_tmpl.format(tilenum=itile)
            with Dataset(inc_path, mode='r') as incfile, Dataset(bkg_path, mode='a') as rstfile:
                for vname in incvars:
                    increment = incfile.variables[vname][:]
                    bkg = rstfile.variables[vname][:]
                    anl = bkg + increment
                    rstfile.variables[vname][:] = anl[:]
                    try:
                        rstfile.variables[vname].delncattr('checksum')  # remove the checksum so fv3 does not complain
                    except (AttributeError, RuntimeError):
                        pass  # checksum is missing, move on

    @logit(logger)
    def get_bkg_dict(self, task_config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method is a placeholder for now... will be possibly made generic at a later date

        Parameters
        ----------
        task_config: Dict
            a dictionary containing all of the configuration needed for the task

        Returns
        ----------
        bkg_dict: Dict
            a dictionary containing the list of model background files to copy for FileHandler
        """
        bkg_dict = {'foo': 'bar'}
        return bkg_dict

    @logit(logger)
    def get_berror_dict(self, config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of background error files to copy

        This method is a placeholder for now... will be possibly made generic at a later date

        Parameters
        ----------
        config: Dict
            a dictionary containing all of the configuration needed

        Returns
        ----------
        berror_dict: Dict
            a dictionary containing the list of background error files to copy for FileHandler
        """
        berror_dict = {'foo': 'bar'}
        return berror_dict

    @logit(logger)
    def link_jediexe(self) -> None:
        """Compile a dictionary of background error files to copy

        This method links a JEDI executable to the run directory

        Parameters
        ----------
        Task: GDAS task

        Returns
        ----------
        None
        """
        exe_src = self.task_config.JEDIEXE

        # TODO: linking is not permitted per EE2.  Needs work in JEDI to be able to copy the exec.
        logger.info(f"Link executable {exe_src} to DATA/")
        logger.warn("Linking is not permitted per EE2.")
        exe_dest = os.path.join(self.task_config.DATA, os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        return

    @staticmethod
    @logit(logger)
    def execute_jediexe(workdir: Union[str, os.PathLike], aprun_cmd: str, jedi_exec: str, jedi_yaml: str) -> None:
        """
        Run a JEDI executable

        Parameters
        ----------
        workdir : str | os.PathLike
            Working directory where to run containing the necessary files and executable
        aprun_cmd : str
            Launcher command e.g. mpirun -np <ntasks> or srun, etc.
        jedi_exec : str
            Name of the JEDI executable e.g. fv3jedi_var.x
        jedi_yaml : str | os.PathLike
            Name of the yaml file to feed the JEDI executable e.g. fv3jedi_var.yaml

        Raises
        ------
        OSError
            Failure due to OS issues
        WorkflowException
            All other exceptions
        """

        os.chdir(workdir)

        exec_cmd = Executable(aprun_cmd)
        exec_cmd.add_default_arg([os.path.join(workdir, jedi_exec), jedi_yaml])

        logger.info(f"Executing {exec_cmd}")
        try:
            exec_cmd()
        except OSError:
            logger.exception(f"FATAL ERROR: Failed to execute {exec_cmd}")
            raise OSError(f"{exec_cmd}")
        except Exception:
            logger.exception(f"FATAL ERROR: Error occured during execution of {exec_cmd}")
            raise WorkflowException(f"{exec_cmd}")

    @staticmethod
    @logit(logger)
    def tgz_diags(statfile: str, diagdir: str) -> None:
        """tar and gzip the diagnostic files resulting from a JEDI analysis.

        Parameters
        ----------
        statfile : str | os.PathLike
            Path to the output .tar.gz .tgz file that will contain the diag*.nc4 files e.g. atmstat.tgz
        diagdir : str | os.PathLike
            Directory containing JEDI diag files
        """

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(diagdir, 'diags', 'diag*nc4'))

        logger.info(f"Compressing {len(diags)} diag files to {statfile}")

        # Open tar.gz file for writing
        with tarfile.open(statfile, "w:gz") as tgz:
            # Add diag files to tarball
            for diagfile in diags:
                tgz.add(diagfile, arcname=os.path.basename(diagfile))
