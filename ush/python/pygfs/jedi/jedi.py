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
    def __init__(self, DATA: str, JEDIEXE: str, yaml_name: Optional[str]) -> None:
        """Constructor for JEDI objects

        This method will construct a Jedi object.
        This includes:
        - save a copy of task_config for provenance
        - set the default JEDI YAML and executable names
        - set an empty AttrDict for the JEDI config
        - set the default directory for J2-YAML templates

        Parameters
        ----------
        task_config: AttrDict
            Attribute-dictionary of all configuration variables associated with a GDAS task.
        yaml_name: str, optional
            Name of YAML file for JEDI configuration

        Returns
        ----------
        None
        """

        _exe_name = os.path.basename(JEDIEXE)

        self.exe_src = JEDIEXE
        self.rundir = DATA
        self.exe = os.path.join(DATA, _exe_name)
        if yaml_name:
            self.yaml = os.path.join(DATA, yaml_name + '.yaml')
        else:
            self.yaml = os.path.join(DATA, os.path.splitext(_exe_name)[0] + '.yaml')

        # Initialize empty JEDI input config attribute-dictionary
        self.config = AttrDict()
        
#        self.j2tmpl_dir = os.path.join(task_config.PARMgfs, 'gdas')

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
        logger.info(f"Generating JEDI YAML config: {self.yaml}")
        self.config = self.get_config(task_config)
        logger.debug(f"JEDI config:\n{pformat(self.config)}")

        # Save JEDI config dictionary to YAML in run directory
        logger.debug(f"Writing JEDI YAML config to: {self.yaml}")
        save_as_yaml(self.config, self.yaml)

        # Link JEDI executable to run directory
        logger.info(f"Linking JEDI executable {self.exe_src} to {self.exe}")
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

        exec_cmd = Executable(aprun_cmd)
        exec_cmd.add_default_arg(self.exe)
        if jedi_args:
            for arg in jedi_args:
                exec_cmd.add_default_arg(arg)
        exec_cmd.add_default_arg(self.yaml)

        logger.info(f"Executing {exec_cmd}")    
        try:
            exec_cmd()
        except OSError:
            raise OSError(f"FATAL ERROR: Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"FATAL ERROR: An error occurred during execution of {exec_cmd}")

    @logit(logger)
    def get_config(self, task_config: AttrDict, algorithm: Optional[str] = None) -> AttrDict:
        """Compile a JEDI configuration dictionary from a template file and save to a YAML file

        Parameters
        ----------
        task_config : AttrDict
            Dictionary of all configuration variables associated with a GDAS task.
        algorithm (optional) : str
            Name of the algorithm used to generate the JEDI configuration dictionary.
            It will override the algorithm set in the task_config.JCB_ALGO_YAML file.

        Returns
        ----------
        None
        """

        # Fill JCB base YAML template and build JCB config dictionary
        jcb_config = parse_j2yaml(task_config.JCB_BASE_YAML, task_config)
        
        # Add JCB algorithm YAML, if it exists, to JCB config dictionary
        if 'JCB_ALGO_YAML' in task_config.keys():
            jcb_config.update(parse_j2yaml(task_config.JCB_ALGO_YAML, task_config))

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
        if os.path.exists(self.exe):
            rm_p(self.exe)
        os.symlink(self.exe_src, self.exe)

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
                output_list.append(item);

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
