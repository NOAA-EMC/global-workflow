#!/usr/bin/env python3

import os
import tarfile
from logging import getLogger
from typing import List, Dict, Any, Optional
from pprint import pformat
from jcb import render
from wxflow import (AttrDict, FileHandler, Task, Executable,
                    WorkflowException, WorkflowKeyError, WorkflowTypeError,
                    chdir, rm_p,
                    parse_j2yaml, save_as_yaml,
                    logit)

logger = getLogger(__name__.split('.')[-1])

required_jedi_keys = ['rundir', 'exe_src', 'mpi_cmd']
optional_jedi_keys = ['jedi_args', 'jcb_base_yaml', 'jcb_algo', 'jcb_algo_yaml']


class Jedi:
    """
    Class for initializing and executing JEDI applications
    """

    @logit(logger, name="Jedi")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for JEDI objects

        This method will construct a Jedi object.
        This includes:
        - create the jedi_config AttrDict and extend it with additional required entries
        - save a copy of jedi_config

        Parameters
        ----------
        config: AttrDict
            Attribute-dictionary of all configuration variables required for the Jedi class

        Returns
        ----------
        None
        """

        # Make sure input dictionary for Jedi class constructor has the required keys
        if 'yaml_name' not in config:
            raise WorkflowKeyError(f"Required key 'yaml_name' not found in config")
        for key in required_jedi_keys:
            if key not in config:
                raise WorkflowKeyError(f"Required key '{key}' not found in config")

        # Create the configuration dictionary for JEDI object
        local_dict = AttrDict(
            {
                'exe': config.exe_src,
                'yaml': os.path.join(config.rundir, config.yaml_name + '.yaml'),
                'input_config': None
            }
        )
        self.jedi_config = AttrDict(**config, **local_dict)

        # Set optional keys in jedi_config to None if not already present
        for key in optional_jedi_keys:
            if key not in self.jedi_config:
                self.jedi_config[key] = None

        # Save a copy of jedi_config
        self._jedi_config = self.jedi_config.deepcopy()

    @logit(logger)
    def initialize(self, task_config: AttrDict, clean_empty_obsspaces=False) -> None:
        """Initialize JEDI application

        This method will initialize a JEDI application.
        This includes:
        - generating JEDI input YAML config
        - saving JEDI input YAML config to run directory
        - linking the JEDI executable to run directory

        Parameters
        ----------
        task_config: AttrDict
            Attribute-dictionary of all configuration variables associated with a GDAS task.
        clean_empty_obsspaces: bool
            Flag to clean empty observation spaces from JEDI input configuration dictionary.
            Default is False.

        Returns
        ----------
        None
        """

        # Render JEDI config dictionary
        logger.info(f"Generating JEDI YAML config: {self.jedi_config.yaml}")
        self.jedi_config.input_config = self.render_jcb(task_config)
        logger.debug(f"JEDI config:\n{pformat(self.jedi_config.input_config)}")

        # Remove obs spaces from JEDI config dictionary with missing obs files
        if clean_empty_obsspaces:
            logger.info(f"Clean empty obs spaces from JEDI YAML config: {self.jedi_config.yaml}")
            self.clean_empty_obsspaces()

        # Save JEDI config dictionary to YAML in run directory
        logger.debug(f"Writing JEDI YAML config to: {self.jedi_config.yaml}")
        save_as_yaml(self.jedi_config.input_config, self.jedi_config.yaml)

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

        chdir(self.jedi_config.rundir)

        exec_cmd = Executable(self.jedi_config.mpi_cmd)
        exec_cmd.add_default_arg(self.jedi_config.exe)
        if self.jedi_config.jedi_args is not None:
            for arg in self.jedi_config.jedi_args:
                exec_cmd.add_default_arg(arg)
        exec_cmd.add_default_arg(self.jedi_config.yaml)

        logger.info(f"Executing {exec_cmd}")
        try:
            exec_cmd()
        except Exception as e:
            raise WorkflowException(f"An error occurred during execution of {exec_cmd}:\n{e}") from e

    @logit(logger)
    def render_jcb(self, task_config: AttrDict, algorithm_in: Optional[str] = None) -> AttrDict:
        """Compile a JEDI configuration dictionary from a template file and save to a YAML file

        Parameters
        ----------
        task_config : AttrDict
            Dictionary of all configuration variables associated with a GDAS task.
        algorithm (optional) : str
            Name of the algorithm used to generate the JEDI configuration dictionary.
            It will override the algorithm set in the jedi_config.jcb_algo_yaml file.

        Returns
        ----------
        jedi_input_config: AttrDict
            Attribute-dictionary of JEDI configuration rendered from a template.
        """

        # Fill JCB base YAML template and build JCB config dictionary
        if self.jedi_config.jcb_base_yaml is not None:
            jcb_config = parse_j2yaml(self.jedi_config.jcb_base_yaml, task_config)
        else:
            raise WorkflowKeyError("JCB base YAML not specified as key 'jcb_base_yaml' in JEDI-class config dictionary")

        # Add JCB algorithm YAML, if it exists, to JCB config dictionary
        if self.jedi_config.jcb_algo_yaml is not None:
            jcb_config.update(parse_j2yaml(self.jedi_config.jcb_algo_yaml, task_config))

        # Set algorithm in JCB config dictionary (method input algorithm takes precedence)
        if algorithm_in is not None:
            algorithm = algorithm_in
        elif self.jedi_config.jcb_algo is not None:
            algorithm = self.jedi_config.jcb_algo
        elif 'algorithm' in jcb_config:
            algorithm = jcb_config.algorithm
        else:
            raise WorkflowKeyError("JCB algorithm not specified")
        jcb_config['algorithm'] = algorithm

        # Generate JEDI YAML config by rendering JCB config dictionary
        try:
            jedi_input_config = render(jcb_config)
        except Exception as e:
            raise WorkflowException(f"An error occurred while rendering JCB template for algorithm {algorithm}:\n{e}") from e

        return jedi_input_config

    @staticmethod
    @logit(logger)
    def get_jedi_dict(jedi_config_yaml: str, task_config: AttrDict, expected_block_names: Optional[list] = None):
        """Get dictionary of Jedi objects from YAML specifying their configuration dictionaries

        Parameters
        ----------
        jedi_config_yaml : str
            path to YAML specifying configuration dictionaries for Jedi objects
        task_config : str
            attribute-dictionary of all configuration variables associated with a GDAS task
        expected_block_names (optional) : str
            list of names of blocks expected to be in jedi_config_yaml YAML file

        Returns
        ----------
        None
        """

        # Initialize dictionary of Jedi objects
        jedi_dict = AttrDict()

        # Parse J2-YAML file for dictionary of JEDI configuration dictionaries
        jedi_config_dict = parse_j2yaml(jedi_config_yaml, task_config)

        # Loop through dictionary of Jedi configuration dictionaries
        for block_name in jedi_config_dict:
            # yaml_name key is set to name for this block
            jedi_config_dict[block_name]['yaml_name'] = block_name

            # Make sure all required keys present
            for key in required_jedi_keys:
                if key not in jedi_config_dict[block_name]:
                    raise WorkflowKeyError(f"Required key {key} not found in {jedi_config_yaml} for block {block_name}.")

            # Set optional keys to None
            for key in optional_jedi_keys:
                if key not in jedi_config_dict[block_name]:
                    jedi_config_dict[block_name][key] = None

            # Construct JEDI object
            jedi_dict[block_name] = Jedi(jedi_config_dict[block_name])

        # Make sure jedi_dict has the blocks we expect
        if expected_block_names:
            for block_name in expected_block_names:
                if block_name not in jedi_dict:
                    raise WorkflowKeyError(f"Expected block key {block_name} not present {jedi_config_yaml}")
            if len(jedi_dict) > len(expected_block_names):
                raise WorkflowException(f"{jedi_config_yaml} specifies more Jedi objects than expected.")

        # Return dictionary of JEDI objects
        return jedi_dict

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
        observers = find_value_in_nested_dict(self.jedi_config.input_config, 'observers')

        # Check if observers list actually present
        if observers:
            # Create new list of observers
            cleaned_observers = []
            for obs_space in observers:
                fname = obs_space['obs space']['obsdatain']['engine']['obsfile']
                if os.path.isfile(fname):
                    cleaned_observers.append(obs_space)
                else:
                    logger.warning(f"WARNING: {fname} does not exist, removing obs space")

            # Clear observers list in dictionary and replace with new list
            observers.clear()
            observers.extend(cleaned_observers)

            # If no observers left in list, raise error
            if observers == []:
                raise WorkflowException(f"No observers found in JEDI input config")

    @staticmethod
    @logit(logger)
    def remove_redundant(input_list: List) -> List:
        """Remove reduncancies from list with possible redundant, non-mutable elements

        Parameters
        ----------
        input_list : List
            List with possible redundant, non-mutable elements

        Returns
        ----------
        output_list : List
            Input list but with redundancies removed
        """

        output_list = []
        for item in input_list:
            if item not in output_list:
                output_list.append(item)

        return output_list

    @staticmethod
    @logit(logger)
    def extract_tar_from_filehandler_dict(filehandler_dict) -> None:
        """Extract tarballs from FileHandler input dictionary

        This method extracts files from tarballs specified in a FileHander
        input dictionary for the 'copy' action.

        Parameters
        ----------
        filehandler_dict
            Input dictionary for FileHandler

        Returns
        ----------
        None
        """

        for item in filehandler_dict['copy']:
            # Use the filename from the destination entry if it's a file path
            # Otherwise, it's a directory, so use the source entry filename
            if os.path.isfile(item[1]):
                filename = os.path.basename(item[1])
            else:
                filename = os.path.basename(item[0])

            # Check if file is a tar ball
            if os.path.splitext(filename)[1] == '.tar':
                tar_file = f"{os.path.dirname(item[1])}/{filename}"

                # Extract tarball
                logger.info(f"Extract files from {tar_file}")
                extract_tar(tar_file)


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
