#!/usr/bin/env python3

import os
import tarfile
from logging import getLogger
from typing import List, Dict, Any, Optional
from jcb import render
from wxflow import (AttrDict,
                    FileHandler,
                    chdir, rm_p,
                    parse_j2yaml,
                    logit,
                    Task,
                    Executable,
                    WorkflowException)

logger = getLogger(__name__.split('.')[-1])


class Jedi:
    """
    Class for initializing and executing JEDI applications
    """
    @logit(logger, name="Jedi")
    def __init__(self, task_config: AttrDict, yaml_name: Optional[str] = None) -> None:
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

        # For provenance, save incoming task_config as a private attribute of JEDI object
        self._task_config = task_config

        _exe_name = os.path.basename(task_config.JEDIEXE)

        self.exe = os.path.join(task_config.DATA, _exe_name)
        if yaml_name:
            self.yaml = os.path.join(task_config.DATA, yaml_name + '.yaml')
        else:
            self.yaml = os.path.join(task_config.DATA, os.path.splitext(_exe_name)[0] + '.yaml')
        self.config = AttrDict()
        self.j2tmpl_dir = os.path.join(task_config.PARMgfs, 'gdas')

    @logit(logger)
    def set_config(self, task_config: AttrDict, algorithm: Optional[str] = None) -> AttrDict:
        """Compile a JEDI configuration dictionary from a template file and save to a YAML file

        Parameters
        ----------
        task_config : AttrDict
            Dictionary of all configuration variables associated with a GDAS task.
        algorithm (optional) : str
            Name of the algorithm used to generate the JEDI configuration dictionary.
            It will override the algorithm set in the task_config.JCB_<>_YAML file.

        Returns
        ----------
        None
        """

        if 'JCB_BASE_YAML' in task_config.keys():
            # Step 1: Fill templates of the JCB base YAML file
            jcb_config = parse_j2yaml(task_config.JCB_BASE_YAML, task_config)

            # Step 2: If algorithm is present then override the algorithm in the JEDI
            #         config. Otherwise, if the algorithm J2-YAML is present, fill
            #         its templates and merge.
            if algorithm:
                jcb_config['algorithm'] = algorithm
            elif 'JCB_ALGO' in task_config.keys():
                jcb_config['algorithm'] = task_config.JCB_ALGO
            elif 'JCB_ALGO_YAML' in task_config.keys():
                jcb_algo_config = parse_j2yaml(task_config.JCB_ALGO_YAML, task_config)
                jcb_config.update(jcb_algo_config)

            # Step 3: Generate the JEDI YAML using JCB
            self.config = render(jcb_config)
        elif 'JEDIYAML' in task_config.keys():
            # Generate JEDI YAML without using JCB
            self.config = parse_j2yaml(task_config.JEDIYAML, task_config,
                                       searchpath=self.j2tmpl_dir)
        else:
            logger.exception(f"FATAL ERROR: Unable to compile JEDI configuration dictionary, ABORT!")
            raise KeyError(f"FATAL ERROR: Task config must contain JCB_BASE_YAML or JEDIYAML")

    @logit(logger)
    def execute(self, task_config: AttrDict, aprun_cmd: str, jedi_args: Optional[List] = None) -> None:
        """Execute JEDI application

        Parameters
        ----------
        task_config: AttrDict
            Attribute-dictionary of all configuration variables associated with a GDAS task.
        aprun_cmd: str
            String comprising the run command for the JEDI executable.
        jedi_args (optional): List
            List of strings comprising optional input arguments for the JEDI executable.

        Returns
        ----------
        jedi_config: AttrDict
            Attribute-dictionary of JEDI configuration rendered from a template.
        """

        chdir(task_config.DATA)

        exec_cmd = Executable(aprun_cmd)
        exec_cmd.add_default_arg(self.exe)
        if jedi_args:
            for arg in jedi_args:
                exec_cmd.add_default_arg(arg)
        exec_cmd.add_default_arg(self.yaml)

        try:
            exec_cmd()
        except OSError:
            raise OSError(f"FATAL ERROR: Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"FATAL ERROR: An error occurred during execution of {exec_cmd}")

    @staticmethod
    @logit(logger)
    def link_exe(task_config: AttrDict) -> None:
        """Link JEDI executable to run directory

        Parameters
        ----------
        task_config: AttrDict
            Attribute-dictionary of all configuration variables associated with a GDAS task.

        Returns
        ----------
        None
        """

        # TODO: linking is not permitted per EE2.
        # Needs work in JEDI to be able to copy the exec. [NOAA-EMC/GDASApp#1254]
        logger.warn("Linking is not permitted per EE2.")
        exe_dest = os.path.join(task_config.DATA, os.path.basename(task_config.JEDIEXE))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(task_config.JEDIEXE, exe_dest)

    @logit(logger)
    def get_obs_dict(self, task_config: AttrDict) -> Dict[str, Any]:
        """Compile a dictionary of observation files to copy

        This method extracts 'observers' from the JEDI yaml and from that list, extracts a list of
        observation files that are to be copied to the run directory
        from the observation input directory

        Parameters
        ----------
        task_config: AttrDict
            Attribute-dictionary of all configuration variables associated with a GDAS task.

        Returns
        ----------
        obs_dict: Dict
            a dictionary containing the list of observation files to copy for FileHandler
        """

        observations = find_value_in_nested_dict(self.config, 'observations')

        copylist = []
        for ob in observations['observers']:
            obfile = ob['obs space']['obsdatain']['engine']['obsfile']
            basename = os.path.basename(obfile)
            copylist.append([os.path.join(task_config.COM_OBS, basename), obfile])
        obs_dict = {
            'mkdir': [os.path.join(task_config.DATA, 'obs')],
            'copy': copylist
        }
        return obs_dict

    @logit(logger)
    def get_bias_dict(self, task_config: AttrDict, bias_file) -> Dict[str, Any]:
        """Compile a dictionary of observation files to copy

        This method extracts 'observers' from the JEDI yaml and determines from that list
        if bias correction tar files are to be copied to the run directory
        from the component directory.

        Parameters
        ----------
        task_config: AttrDict
            Attribute-dictionary of all configuration variables associated with a GDAS task.
        bias_file
            name of bias correction tar file

        Returns
        ----------
        bias_dict: Dict
            a dictionary containing the list of observation bias files to copy for FileHandler
        """

        observations = find_value_in_nested_dict(self.config, 'observations')

        copylist = []
        for ob in observations['observers']:
            if 'obs bias' in ob.keys():
                obfile = ob['obs bias']['input file']
                obdir = os.path.dirname(obfile)
                basename = os.path.basename(obfile)
                prefix = '.'.join(basename.split('.')[:-3])
                bfile = f"{prefix}.{bias_file}"
                tar_file = os.path.join(obdir, bfile)
                copylist.append([os.path.join(task_config.VarBcDir, bfile), tar_file])
                break

        bias_dict = {
            'mkdir': [os.path.join(task_config.DATA, 'bc')],
            'copy': copylist
        }

        return bias_dict

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
