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


class Jedi:
    """
    Class for initializing and executing JEDI applications
    """
    @logit(logger, name="Jedi")
    def __init__(self, config) -> None:
        """Constructor for JEDI objects

        This method will construct a Jedi object.
        This includes:
        - set the default JEDI YAML and executable names
        - set an empty AttrDict for the JEDI config
        - set the default directory for J2-YAML templates

        Parameters
        ----------
        task_config: AttrDict
            Attribute-dictionary of all configuration variables associated with a GDAS task.

        Returns
        ----------
        None
        """

        # Create the configuration dictionary for JEDI object
        self.jedi_config = config.deepcopy()

        local_dict = AttrDict(
            {
                'exe': os.path.join(self.config.run_dir, os.path.basename(self.config.exe_src)),
                'yaml': os.path.join(DATA, config.yaml_name + '.yaml'),
                'input_config': None
            }
        )
        self.jedi_config.update(local_dict)

    @logit(logger)
    def initialize(self, task_config: AttrDict) -> None:
        """Initialize JEDI application

        This method will initialize a JEDI application.
        This includes:
        - generating JEDI YAML config
        - saving JEDI YAML config to run directory
        - linking the JEDI executable to run directory
        """

        # Render JEDI config dictionary
        logger.info(f"Generating JEDI YAML config: {self.jedi_config.yaml}")
        self.jedi_config.input_config = self.render_jcb(task_config)
        logger.debug(f"JEDI config:\n{pformat(self.jedi_config.input_config)}")

        # Save JEDI config dictionary to YAML in run directory
        logger.debug(f"Writing JEDI YAML config to: {self.jedi_config.yaml}")
        save_as_yaml(self.jedi_config.input_config, self.jedi_config.yaml)

        # Link JEDI executable to run directory
        logger.info(f"Linking JEDI executable {self.jedi_config.exe_src} to {self.jedi_config.exe}")
        self.link_exe()

    @logit(logger)
    def execute(self, aprun_cmd: str, jedi_args: Optional[List] = None) -> None:
        """Execute JEDI application

        Parameters
        ----------
        aprun_cmd: str
            String comprising the run command for the JEDI executable.
        jedi_args (optional): List
            List of strings comprising optional input arguments for the JEDI executable.

        Returns
        ----------
        jedi_config: AttrDict
            Attribute-dictionary of JEDI configuration rendered from a template.
        """

        chdir(self.rundir)

        exec_cmd = Executable(self.jedi_config.aprun_cmd)
        exec_cmd.add_default_arg(self.jedi_config.exe)
        if self.jedi_config.jedi_args:
            for arg in self.jedi_config.jedi_args:
                exec_cmd.add_default_arg(arg)
        exec_cmd.add_default_arg(self.jedi_config.yaml)

        logger.info(f"Executing {exec_cmd}")
        try:
            exec_cmd()
        except OSError:
            raise OSError(f"FATAL ERROR: Failed to execute {exec_cmd}")
        except Exception:
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
        None
        """

        if not self.jedi_config.jcb_algo_yaml and not algorithm:
            logger.error(f"FATAL ERROR: Unable to compile JEDI configuration dictionary, ABORT!")
            logger.error(f"FATAL ERROR: JEDI config must contain jcb_algo_yaml or algorithm be
                         specified as an input to jedi.render_jcb()")

        # Fill JCB base YAML template and build JCB config dictionary
        jcb_config = parse_j2yaml(self.jedi_config.jcb_base_yaml, task_config)

        # Add JCB algorithm YAML, if it exists, to JCB config dictionary
        if self.jedi_config.jcb_algo_yaml:
            jcb_config.update(parse_j2yaml(self.jedi_config.jcb_algo_yaml, task_config))

        # Set algorithm in JCB config dictionary or override the one set by JCB_ALGO_YAML
        if algorithm:
            jcb_config['algorithm'] = algorithm

        # Generate JEDI YAML config by rendering JCB config dictionary
        jedi_config = render(jcb_config)

        return jedi_config

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
        except tarfile.ReadError as err:
            if tarfile.is_tarfile(tar_file):
                logger.error(f"FATAL ERROR: {tar_file} could not be read")
                raise tarfile.ReadError(f"FATAL ERROR: unable to read {tar_file}")
            else:
                logger.info()
        except tarfile.ExtractError as err:
            logger.exception(f"FATAL ERROR: unable to extract from {tar_file}")
            raise tarfile.ExtractError("FATAL ERROR: unable to extract from {tar_file}")


# TODO: remove since no longer used
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

    raise KeyError(f"Key '{target_key}' not found in the nested dictionary")
