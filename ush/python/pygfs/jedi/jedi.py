#!/usr/bin/env python3

import glob
import gzip
import os
import tarfile
from logging import getLogger
from typing import List, Dict, Any, Optional
from jcb import render
from wxflow import (AttrDict, FileHandler, Task, Executable,
                    WorkflowException, WorkflowKeyError, WorkflowTypeError,
                    chdir,
                    parse_j2yaml, save_as_yaml,
                    logit)

logger = getLogger(__name__.split('.')[-1])

required_jedi_keys = ['jedi_app_name', 'rundir', 'exe_src', 'mpi_cmd', 'jcb_base_yaml']
optional_jedi_keys = ['jedi_args', 'jcb_algo', 'jcb_algo_yaml',
                      'obs_list_yaml', 'bias_files_yaml', 'app_test_yaml']


class Jedi:
    """
    Class for initializing and executing JEDI applications
    """

    @logit(logger, name="Jedi")
    def __init__(self, config: Dict[str, Any], task_config: AttrDict) -> None:
        """Constructor for JEDI objects

        This method will construct a Jedi object.
        This includes:
        - create the jedi_config AttrDict and extend it with additional required entries
        - create the jcb_config AttrDict by parsing the jcb_base_yaml and jcb_algo_yaml files
        - save a copy of jedi_config and jcb_config

        Parameters
        ----------
        config: Dict[str, Any]
            Dictionary of all configuration variables required for the Jedi class
        task_config: AttrDict
            Attribute-dictionary of all configuration variables associated with a GDAS task.

        Returns
        ----------
        None
        """

        # Parse inputs
        # ------------

        # Create the configuration dictionary for JEDI object
        local_dict = AttrDict(
            {
                'exe_config_yaml': os.path.join(config.rundir, config.jedi_app_name + '.yaml'),
            }
        )
        self.jedi_config = AttrDict(**config, **local_dict)

        # Make sure input dictionary for Jedi class constructor has the required keys
        for key in required_jedi_keys:
            if key not in self.jedi_config:
                raise WorkflowKeyError(f"Required key '{key}' not found in jedi_config")

        # Set optional keys in jedi_config to None if not already present
        for key in optional_jedi_keys:
            if key not in self.jedi_config:
                self.jedi_config[key] = None

        # Makes sure either jcb_algo or jcb_algo_yaml is specified, but not both
        if 'jcb_algo' not in config and 'jcb_algo_yaml' not in config:
            raise WorkflowKeyError("Either 'jcb_algo' or 'jcb_algo_yaml' must be specified in JEDI config")
        if 'jcb_algo' in config and 'jcb_algo_yaml' in config:
            raise WorkflowKeyError("Either 'jcb_algo' or 'jcb_algo_yaml' must be specified in JEDI config, but not both")

        # Construct JCB config dictionary
        # -------------------------------

        # Render JCB base config YAML
        self._jcb_base_config = parse_j2yaml(self.jedi_config.jcb_base_yaml, task_config)

        # Construct JCB config dictionary
        if self.jedi_config.jcb_algo_yaml is not None:
            # Render JCB algorithm config YAML if specified
            self._jcb_algo_config = parse_j2yaml(self.jedi_config.jcb_algo_yaml, task_config)
            if 'algorithm' not in self._jcb_algo_config:
                raise WorkflowKeyError("JCB algorithm not specified in jcb_algo_yaml")

            self.jcb_config = AttrDict({**self._jcb_base_config, **self._jcb_algo_config})
        else:
            self.jcb_config = AttrDict(self._jcb_base_config)

        # Set JCB algorithm if not already specified in JCB algorithm config YAML
        if self.jedi_config.jcb_algo is not None:
            self.jcb_config.algorithm = self.jedi_config.jcb_algo

        # Set observations list in JCB config if obs_list_yaml specified
        if self.jedi_config.obs_list_yaml is not None:
            self.jcb_config['observations'] = parse_j2yaml(self.jedi_config.obs_list_yaml, task_config)['observations']

        # Include test reference YAML in JCB config if app_test_yaml specified
        if task_config.DO_TEST_MODE and self.jedi_config.app_test_yaml is not None:
            self.jcb_config.update(parse_j2yaml(self.jedi_config.app_test_yaml, task_config))

        # Set object attributes
        # ---------------------

        # Set model attribute, checking that "app_path_observations" is present in jcb_config
        if 'app_path_model' in self.jcb_config:
            self.component = self.jcb_config['app_path_model'].split('/')[-1]
        elif 'app_path_observations' in self.jcb_config:
            self.component = self.jcb_config['app_path_observations'].split('/')[-1]
        else:
            raise WorkflowKeyError(f"Required key 'app_path_model' or 'app_path_observations'  not found in JCB config")

        # Initialize JEDI application configuration dictionary to None
        self.exe_config = None

        # Save a copy of jedi_config and jcb_config
        self._jedi_config = self.jedi_config.deepcopy()
        self._jcb_config = self.jcb_config.deepcopy()

    @staticmethod
    @logit(logger)
    def get_jedi_dict(jedi_config_dict: dict, task_config: AttrDict, expected_block_names: Optional[list] = None):
        """Get dictionary of Jedi objects from YAML specifying their configuration dictionaries

        Parameters
        ----------
        jedi_config_dict : dict
            dictionary parsed from a Jinja2-YAML file specifying configuration dictionaries for JEDI objects
        expected_block_names (optional) : str
            list of names of blocks expected to be in jedi_config_yaml YAML file

        Returns
        ----------
        None
        """

        # Initialize dictionary of Jedi objects
        jedi_dict = AttrDict()

        # Loop through dictionary of Jedi configuration dictionaries
        for block_name in jedi_config_dict:
            # jedi_app_name key is set to name for this block
            jedi_config_dict[block_name]['jedi_app_name'] = block_name

            # Construct JEDI object
            jedi_dict[block_name] = Jedi(jedi_config_dict[block_name], task_config)

        # Make sure jedi_dict has the blocks we expect
        if expected_block_names:
            for block_name in expected_block_names:
                if block_name not in jedi_dict:
                    raise WorkflowKeyError(f"Expected block key {block_name} not present {jedi_config_yaml}")

        # Return dictionary of JEDI objects
        return jedi_dict

    @logit(logger)
    def initialize(self, clean_empty_obsspaces: Optional[bool] = False) -> None:
        """Initialize JEDI application

        This method will initialize a JEDI application.
        This includes:
        - generating JEDI input YAML config
        - cleaning empty observation spaces from JEDI input config dictionary
        - saving JEDI input YAML config to run directory

        Parameters
        ----------
        clean_empty_obsspaces: bool
            Flag to clean empty observation spaces from JEDI input configuration dictionary.
            Default is False.

        Returns
        ----------
        None
        """

        # Render JEDI executable config dictionary
        logger.info(f"Generating JEDI YAML config: {self.jedi_config.exe_config_yaml}")
        self.exe_config = self.render_jcb_template()
        logger.debug(f"JEDI config:\n{self.exe_config}")

        # Remove obs spaces from JEDI executable config dictionary with missing obs files
        if clean_empty_obsspaces:
            logger.info(f"Clean empty obs spaces from JEDI YAML config: {self.jedi_config.exe_config_yaml}")
            self.clean_empty_obsspaces()

        # Save JEDI exectuable config dictionary to YAML in run directory
        logger.debug(f"Writing JEDI YAML config to: {self.jedi_config.exe_config_yaml}")
        save_as_yaml(self.exe_config, self.jedi_config.exe_config_yaml)

    @logit(logger)
    def execute(self) -> None:
        """Execute JEDI application

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # TODO: not sure if this chdir does anyththing
        chdir(self.jedi_config.rundir)

        exec_cmd = Executable(self.jedi_config.mpi_cmd)
        exec_cmd.add_default_arg(self.jedi_config.exe_src)
        if self.jedi_config.jedi_args is not None:
            for arg in self.jedi_config.jedi_args:
                exec_cmd.add_default_arg(arg)
        exec_cmd.add_default_arg(self.jedi_config.exe_config_yaml)

        logger.info(f"Executing {exec_cmd}")
        try:
            exec_cmd()
        except Exception as e:
            raise WorkflowException(f"An error occurred during execution of {exec_cmd}:\n{e}") from e

    @logit(logger)
    def render_jcb_template(self, algorithm_in: Optional[str] = None) -> AttrDict:
        """Compile a JEDI configuration dictionary from a template file and save to a YAML file

        Parameters
        ----------
        algorithm (optional) : str
            Name of the algorithm used to generate the JEDI configuration dictionary.
            It will override the algorithm set in the jedi_config.jcb_algo_yaml file.

        Returns
        ----------
        exe_config: AttrDict
            Attribute-dictionary of JEDI configuration rendered from a template.
        """

        # Set algorithm (method input algorithm takes precedence)
        if algorithm_in is not None:
            algorithm = algorithm_in
        elif 'algorithm' in self.jcb_config:
            algorithm = self.jcb_config['algorithm']
        else:
            raise WorkflowKeyError("JCB algorithm not specified")

        # Generate JEDI YAML config by rendering JCB config dictionary
        try:
            exe_config = render({**self.jcb_config, **{'algorithm': algorithm}})
        except Exception as e:
            raise WorkflowException(f"An error occurred while rendering JCB template for algorithm {algorithm}:\n{e}") from e

        return exe_config

    @logit(logger)
    def clean_empty_obsspaces(self):
        """
        Replace list of observers in JEDI input configuration dictionary with new list, removing
        any observers with missing observation files.

        Parameters
        ----------
        None

        Returns
        -------
        None
        """

        # Get observers from JEDI input config
        observers = find_value_in_nested_dict(self.exe_config, 'observers')

        # Check if observers list actually present
        if observers:
            # Create new list of observers
            cleaned_observers = []
            for obs_space in observers:
                fname = obs_space['obs space']['obsdatain']['engine']['obsfile']
                if os.path.isfile(fname):
                    cleaned_observers.append(obs_space)
                else:
                    logger.warning(f"{fname} does not exist, removing obs space")

            # Clear observers list in dictionary and replace with new list
            observers.clear()
            observers.extend(cleaned_observers)

            # Warn if no observers left in list
            if observers == []:
                logger.warning(f"No observers found in JEDI input config")

    @logit(logger)
    def stage_obsdatain(self, comin) -> None:
        """Stage observation input files specified in JCB configuration dictionary

        This method will stage observation data files specified in the JCB configuration
        dictionary

        Parameters
        ----------
        comin: str
            path to COM input directory

        Returns
        ----------
        None
        """

        # Check that other required keys are present in jcb_config
        for stem in ['obsdatain_path', 'obsdataout_path', 'obsdatain_prefix', 'obsdatain_suffix']:
            key = f'{self.component}_{stem}'
            if key not in self.jcb_config:
                raise WorkflowKeyError(f"Required key {key} not found in JCB config")

        # Initialize FileHandler input dictionary
        fh_dict = {'mkdir': [], 'copy_opt': []}

        # Make directories
        fh_dict['mkdir'].append(self.jcb_config[f'{self.component}_obsdatain_path'])
        fh_dict['mkdir'].append(self.jcb_config[f'{self.component}_obsdataout_path'])

        # Copy files
        ob_dest = self.jcb_config[f'{self.component}_obsdatain_path']
        for observation_from_jcb in self.jcb_config['observations']:
            # Observations
            ob_src = os.path.join(comin,
                                  self.jcb_config[f'{self.component}_obsdatain_prefix'] +
                                  observation_from_jcb +
                                  self.jcb_config[f'{self.component}_obsdatain_suffix'])

            fh_dict['copy_opt'].append([ob_src, ob_dest])

        # Execute FileHandler sync
        FileHandler(fh_dict).sync()

    @logit(logger)
    def save_obsdataout(self, comout: str, archive_name: str) -> None:
        """Archive observation output files and compress archive into COM directory

        Parameters
        ----------
        comout: str
            path to COM output directory
        archive_name: str
            name of output tar file

        Returns
        ----------
        None
        """

        # Check that other required keys are present in jcb_config
        for stem in ['obsdataout_path', 'obsdataout_prefix', 'obsdataout_suffix']:
            key = f'{self.component}_{stem}'
            if key not in self.jcb_config:
                raise WorkflowKeyError(f"Required key {key} not found in JCB config")

        # Set paths of output tar files
        tarball = os.path.join(self.jcb_config[f"{self.component}_obsdataout_path"], f"{archive_name}.tar.gz")

        # Create compressed tarball of obs output files in COM
        logger.info(f"Archiving observation output files to {tarball}")
        with tarfile.open(tarball, "w:gz") as archive:
            for observation_from_jcb in self.jcb_config['observations']:
                obsdataout_file = os.path.join(self.jcb_config[f"{self.component}_obsdataout_path"],
                                               self.jcb_config[f"{self.component}_obsdataout_prefix"] +
                                               observation_from_jcb +
                                               self.jcb_config[f"{self.component}_obsdataout_suffix"])
                if os.path.exists(obsdataout_file):
                    logger.info(f"Adding observation output file {obsdataout_file} to {tarball}")
                    archive.add(obsdataout_file, arcname=os.path.basename(obsdataout_file))
                else:
                    logger.warning(f"Observation output file {obsdataout_file} does not exist and will be skipped")

        # Copy files to COM
        FileHandler({'copy_opt': [[tarball, comout]]}).sync()

    @logit(logger)
    def stage_obsbiasin(self, comin) -> None:
        """Stage bias correction files specified in JEDI input configuration dictionary

        This method will stage bias correction files specified in the JEDI input
        configuration dictionary using a FileHandler object, and then extract the
        bias correction files from the tar files.

        Parameters
        ----------
        comin: str
            path to COMIN directory

        Returns
        ----------
        None
        """

        # Check that other required keys are present in jcb_config
        for stem in ['obsbiasin_path', 'obsbiasout_path', 'obsbiasin_prefix']:
            key = f'{self.component}_{stem}'
            if key not in self.jcb_config:
                raise WorkflowKeyError(f"Required key {key} not found in JCB config")

        # Initialize FileHandler input dictionary
        fh_dict = {'mkdir': [], 'copy_opt': []}

        # Make directories
        fh_dict['mkdir'].append(self.jcb_config[f'{self.component}_obsbiasin_path'])
        fh_dict['mkdir'].append(self.jcb_config[f'{self.component}_obsbiasout_path'])

        # Copy files
        files_already_copied = []
        bias_dest = self.jcb_config[f'{self.component}_obsbiasin_path']
        for observation_from_jcb in self.jcb_config['observations']:
            if observation_from_jcb in self.jcb_config.bias_files_dict and observation_from_jcb not in files_already_copied:
                bias_src = os.path.join(comin, self.jcb_config[f'{self.component}_obsbiasin_prefix'] + self.jcb_config.bias_files_dict[observation_from_jcb])

                fh_dict['copy_opt'].append([bias_src, bias_dest])

                # Don't copy same file multiple times
                files_already_copied.append(observation_from_jcb)

        # Execute FileHandler sync
        FileHandler(fh_dict).sync()

        # Untar bias corrections
        bias_file_list = []
        for ob in self.jcb_config['observations']:
            if ob in self.jcb_config.bias_files_dict and not self.jcb_config.bias_files_dict[ob] in bias_file_list:
                bias_file_list.append(self.jcb_config.bias_files_dict[ob])
                bias_file_path = os.path.join(self.jcb_config[f"{self.component}_obsbiasin_path"],
                                              self.jcb_config[f"{self.component}_obsbiasin_prefix"] + self.jcb_config.bias_files_dict[ob])
                if os.path.exists(bias_file_path):
                    Jedi.extract_tar(bias_file_path)
                else:
                    logger.warning(f"Bias correction file {bias_file_path} does not exist and will be skipped")

    @logit(logger)
    def save_obsbiasout(self, comout: str, archive_name: str) -> None:
        """Tar bias correction files and into COM directory

        Parameters
        ----------
        comout: str
            path to COM output directory
        archive_name: str
            name of output tar file

        Returns
        ----------
        None
        """

        # Check that other required keys are present in jcb_config
        for stem in ['obsbiasin_path', 'obsbiasout_path', 'obsbiasin_prefix',
                     'obsbiasout_prefix', 'obsbiasout_suffix', 'obsbiascovout_suffix',
                     'obstlapsein_suffix']:
            key = f'{self.component}_{stem}'
            if key not in self.jcb_config:
                raise WorkflowKeyError(f"Required key {key} not found in JCB config")

        # Set paths of output tar files
        tarball = f"{archive_name}.tar"

        # Get lists of files to put in tarballs
        satlist = []
        satcovlist = []
        tlaplist = []
        for ob in self.jcb_config['observations']:
            # Sat bias file
            satfile = os.path.join(self.jcb_config[f"{self.component}_obsbiasout_path"],
                                   self.jcb_config[f"{self.component}_obsbiasout_prefix"] + ob + self.jcb_config[f"{self.component}_obsbiasout_suffix"])
            if os.path.exists(satfile):
                satlist.append(satfile)

            # Sat bias cov file
            satcovfile = os.path.join(self.jcb_config[f"{self.component}_obsbiasout_path"],
                                      self.jcb_config[f"{self.component}_obsbiasout_prefix"] + ob + self.jcb_config[f"{self.component}_obsbiascovout_suffix"])
            if os.path.exists(satcovfile):
                satcovlist.append(satcovfile)

            # Temperature lapse rate file
            tlapfile = os.path.join(self.jcb_config[f"{self.component}_obsbiasin_path"],
                                    self.jcb_config[f"{self.component}_obsbiasin_prefix"] + ob + self.jcb_config[f"{self.component}_obstlapsein_suffix"])
            if os.path.exists(tlapfile):
                tlaplist.append(tlapfile)

        # Create tarball of bias correction files
        logger.info(f"Creating bias correction tarball {tarball}")
        with tarfile.open(tarball, 'w') as bcor:
            logger.info(f"Adding {bcor.getnames()}")
            for satfile in satlist + satcovlist:
                logger.info(f"Adding satellite bias correction file {satfile} to {tarball}")
                bcor.add(satfile, arcname=os.path.basename(satfile))
            for tlapfile in tlaplist:
                # Change GPREFIX to APREFIX in tlapse file name when adding to tarball
                tlapfile_rename = tlapfile.replace(self.jcb_config[f"{self.component}_obsbiasin_prefix"],
                                                   self.jcb_config[f"{self.component}_obsbiasout_prefix"])
                logger.info(f"Adding temperature lapse rate file {tlapfile_rename} to {tarball}")
                bcor.add(tlapfile, arcname=os.path.basename(tlapfile_rename))

        # Copy files to COM
        FileHandler({'copy_opt': [[tarball, comout]]}).sync()

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


@logit(logger)
def find_value_in_nested_dict(nested_dict: Dict, target_key: str) -> Any:
    """
    Recursively search through a nested dictionary and return the value for the target key.
    This returns the first target key it finds.  So if a key exists in a subsequent
    nested dictionary, it will not be found.

    Parameters
    ----------
    nested_dict : Dict
        Dictionary to search
    target_key : str
        Key to search for

    Returns
    -------
    Any
        Value of the target key

    Raises
    ------
    WorkflowTypeError
        If input is not a dictionary

    TODO: if this gives issues due to landing on an incorrect key in the nested
    dictionary, we will have to implement a more concrete method to search for a key
    given a more complete address.  See resolved conversations in PR 2387

    # Example usage:
    nested_dict = {
        'a': {
            'b': {
                'c': 1,
                'd': {
                    'e': 2,
                    'f': 3
                }
            },
            'g': 4
        },
        'h': {
            'i': 5
        },
        'j': {
            'k': 6
        }
    }

    user_key = input("Enter the key to search for: ")
    result = find_value_in_nested_dict(nested_dict, user_key)
    """

    if not isinstance(nested_dict, dict):
        raise WorkflowTypeError(f"Input is not of type(dict)")

    result = nested_dict.get(target_key)
    if result is not None:
        return result

    for value in nested_dict.values():
        if isinstance(value, dict):
            try:
                result = find_value_in_nested_dict(value, target_key)
                if result is not None:
                    return result
            except KeyError:
                pass

    logger.info(f"Key '{target_key}' not found in the nested dictionary")
    return None
