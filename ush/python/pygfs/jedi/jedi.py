#!/usr/bin/env python3

import os
import tarfile
from logging import getLogger
from typing import List, Dict, Any, Optional
from pprint import pformat
from jcb import render
from wxflow import (AttrDict, FileHandler, Task, Executable,
                    chdir, rm_p,
                    parse_j2yaml, save_as_yaml,
                    logit,
                    WorkflowException)

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
            logger.error(f"FATAL ERROR: Key 'yaml_name' not found in config")
            raise KeyError(f"FATAL ERROR: Key 'yaml_name' not found in config")
        for key in required_jedi_keys:
            if key not in config:
                logger.error(f"FATAL ERROR: Required key '{key}' not found in config")
                raise KeyError(f"FATAL ERROR: Required key '{key}' not found in config")

        # Create the configuration dictionary for JEDI object
        local_dict = AttrDict(
            {
                'exe': os.path.join(config.rundir, os.path.basename(config.exe_src)),
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

        # Link JEDI executable to run directory
        logger.info(f"Linking JEDI executable {self.jedi_config.exe_src} to {self.jedi_config.exe}")
        self.link_exe()

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
        except OSError:
            logger.error(f"FATAL ERROR: Failed to execute {exec_cmd}")
            raise OSError(f"FATAL ERROR: Failed to execute {exec_cmd}")
        except Exception:
            logger.error(f"FATAL ERROR: An error occurred during execution of {exec_cmd}")
            raise WorkflowException(f"FATAL ERROR: An error occurred during execution of {exec_cmd}")

    @logit(logger)
    def render_jcb(self, task_config: AttrDict, algorithm: Optional[str] = None) -> AttrDict:
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
            logger.error(f"FATAL ERROR: JCB base YAML must be specified in order  to render YAML using JCB")
            raise KeyError(f"FATAL ERROR: JCB base YAML must be specified in order to render YAML using JCB")

        # Add JCB algorithm YAML, if it exists, to JCB config dictionary
        if self.jedi_config.jcb_algo_yaml is not None:
            jcb_config.update(parse_j2yaml(self.jedi_config.jcb_algo_yaml, task_config))

        # Set algorithm in JCB config dictionary
        if algorithm is not None:
            jcb_config['algorithm'] = algorithm
        elif self.jedi_config.jcb_algo is not None:
            jcb_config['algorithm'] = self.jedi_config.jcb_algo
        elif 'algorithm' in jcb_config:
            pass
        else:
            logger.error(f"FATAL ERROR: JCB algorithm must be specified as input to jedi.render_jcb(), " +
                         "in JEDI configuration dictionary as jcb_algo, or in JCB algorithm YAML")
            raise Exception(f"FATAL ERROR: JCB algorithm must be specified as input to jedi.render_jcb(), " +
                            "in JEDI configuration dictionary as jcb_algo, or in JCB algorithm YAML")

        # Generate JEDI YAML config by rendering JCB config dictionary
        jedi_input_config = render(jcb_config)

        return jedi_input_config

    @logit(logger)
    def link_exe(self) -> None:
        """Link JEDI executable to run directory

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # TODO: linking is not permitted per EE2.
        # Needs work in JEDI to be able to copy the exec. [NOAA-EMC/GDASApp#1254]
        logger.warn("Linking is not permitted per EE2.")
        if not os.path.exists(self.jedi_config.exe):
            os.symlink(self.jedi_config.exe_src, self.jedi_config.exe)

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
                    logger.error(f"FATAL ERROR: Required key {key} not found in {jedi_config_yaml} for block {block_name}.")
                    raise KeyError(f"FATAL ERROR: Required key {key} not found in {jedi_config_yaml} for block {block_name}.")

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
                    logger.error(f"FATAL ERROR: Expected block {block_name} not present {jedi_config_yaml}")
                    raise Exception(f"FATAL ERROR: Expected block {block_name} not present {jedi_config_yaml}")
            if len(jedi_dict) > len(expected_block_names):
                logger.error(f"FATAL ERROR: {jedi_config_yaml} specifies more Jedi objects than expected.")
                raise Exception(f"FATAL ERROR: {jedi_config_yaml} specifies more Jedi objects than expected.")

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
                logger.error(f"FATAL ERROR: No observers found in JEDI input config")
                raise Exception(f"FATAL ERROR: No observers found in JEDI input config")

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
    except tarfile.FileExistsError as err:
        logger.exception(f"FATAL ERROR: {tar_file} does not exist")
        raise tarfile.FileExistsError(f"FATAL ERROR: {tar_file} does not exist")
    except tarfile.ReadError as err:
        if tarfile.is_tarfile(tar_file):
            logger.error(f"FATAL ERROR: tar archive {tar_file} could not be read")
            raise tarfile.ReadError(f"FATAL ERROR: tar archive {tar_file} could not be read")
        else:
            logger.error(f"FATAL ERROR: {tar_file} is not a tar archive")
            raise tarfile.ReadError(f"FATAL ERROR: {tar_file} is not a tar archive")
    except tarfile.ExtractError as err:
        logger.exception(f"FATAL ERROR: unable to extract from {tar_file}")
        raise tarfile.ExtractError(f"FATAL ERROR: unable to extract from {tar_file}")


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
    KeyError
        If key is not found in dictionary

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
        raise TypeError(f"Input is not of type(dict)")

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
