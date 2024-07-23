#!/usr/bin/env python3

import os
from logging import getLogger
from pprint import pformat
from typing import List, Dict, Any, Optional
from jcb import render
from wxflow import (AttrDict,
                    chdir, rm_p, 
                    parse_j2yaml, save_as_yaml,
                    logit,
                    Task,
                    Executable,
                    WorkflowException)

logger = getLogger(__name__.split('.')[-1])


class JEDI(Task):
    """Parent class for JEDI-based tasks

    The JEDI class is the parent class for all
    Global Data Assimilation System (GDAS) tasks
    that run JEDI-based applications.
    """

    def __init__(self, config: Dict[str, Any]) -> None:
        super().__init__(config)
        
        _exe_name = os.path.basename(self.task_config.JEDIEXE)
        
        local_dict = AttrDict(
            {
                'jedi_exe': os.path.join(self.task_config.DATA, _exe_name),
                'jedi_yaml': os.path.join(self.task_config.DATA, os.path.splitext(_exe_name)[0] + '.yaml'),
                'gdasapp_j2tmpl_dir': os.path.join(self.task_config.PARMgfs, 'gdas') 
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)
                                  
    @logit(logger)
    def initialize(self) -> None:
        super().initialize()

        # Generate and write JEDI input YAML file
        self.get_jedi_config()
        
        # link JEDI executable to run directory
        self.link_jedi_exe()
        
    @logit(logger)
    def execute(self, aprun_cmd: str, jedi_args: Optional[List] = None) -> None:

        chdir(self.task_config.DATA)

        exec_cmd = Executable(aprun_cmd)
        exec_cmd.add_default_arg(self.task_config.jedi_exe)
        if jedi_args:
            for arg in jedi_args:
                exec_cmd.add_default_arg(arg)
        exec_cmd.add_default_arg(self.task_config.jedi_yaml)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

        pass

    @logit(logger)
    def finalize(self) -> None:
        super().finalize()

    @logit(logger)
    def clean(self) -> None:
        super().clean()

    @logit(logger)
    def get_jedi_config(self, algorithm: Optional[str] = None) -> Dict[str, Any]:
        """Compile a dictionary of JEDI configuration from JEDIYAML template file

        Parameters
        ----------
        algorithm (optional) : str
            Name of the algorithm to use in the JEDI configuration. Will override the algorithm
            set in the self.task_config.JCB_<>_YAML file

        Returns
        ----------
        jedi_config : Dict
            a dictionary containing the fully rendered JEDI yaml configuration
        """

        # generate JEDI YAML file
        logger.info(f"Generate JEDI YAML config: {self.task_config.jedi_yaml}")

        if 'JCB_BASE_YAML' in self.task_config.keys():
            # Step 1: fill templates of the jcb base YAML file
            jcb_config = parse_j2yaml(self.task_config.JCB_BASE_YAML, self.task_config)

            # Step 2: If algorithm is present override the algorithm in the JEDI config,
            # or if algorithm yamls is present, fill templates of algorithm override
            # YAML and merge.
            if algorithm:
                jcb_config['algorithm'] = algorithm
            elif 'JCB_ALGO' in self.task_config.keys():
                jcb_config['algorithm'] = self.task_config.JCB_ALGO
            elif 'JCB_ALGO_YAML' in self.task_config.keys():
                jcb_algo_config = parse_j2yaml(self.task_config.JCB_ALGO_YAML, self.task_config)
                jcb_config = {**jcb_config, **jcb_algo_config}

            # Step 3: generate the JEDI Yaml using JCB driving YAML
            jedi_config = render(jcb_config)
        elif 'JEDIYAML' in self.task_config.keys():
            # Generate JEDI YAML file (without using JCB)
            jedi_config = parse_j2yaml(self.task_config.JEDIYAML, self.task_config,
                                       searchpath=self.gdasapp_j2tmpl_dir)
        else:
            raise KeyError(f"Task config must contain JCB_ALGO, JCB_BASE_YAML, or JEDIYAML")

        logger.debug(f"JEDI config:\n{pformat(jedi_config)}")

        # Save JEDI config to YAML file
        self.task_config.jedi_config = self.get_jedi_config()

    def link_jedi_exe(self) -> None:
        """Link JEDI executable to run directory

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
        
    @logit(logger)
    def get_obs_dict(self) -> Dict[str, Any]:
        """Compile a dictionary of observation files to copy

        This method extracts 'observers' from the JEDI yaml and from that list, extracts a list of
        observation files that are to be copied to the run directory
        from the observation input directory

        Parameters
        ----------

        Returns
        ----------
        obs_dict: Dict
            a dictionary containing the list of observation files to copy for FileHandler
        """

        logger.info(f"Extracting a list of observation files from Jedi config file")
        observations = find_value_in_nested_dict(self.task_config.jedi_config, 'observations')
        logger.debug(f"observations:\n{pformat(observations)}")

        copylist = []
        for ob in observations['observers']:
            obfile = ob['obs space']['obsdatain']['engine']['obsfile']
            basename = os.path.basename(obfile)
            copylist.append([os.path.join(self.task_config['COM_OBS'], basename), obfile])
        obs_dict = {
            'mkdir': [os.path.join(self.task_config['DATA'], 'obs')],
            'copy': copylist
        }
        return obs_dict

    @logit(logger)
    def get_bias_dict(self) -> Dict[str, Any]:
        """Compile a dictionary of observation files to copy

        This method extracts 'observers' from the JEDI yaml and from that list, extracts a list of
        observation bias correction files that are to be copied to the run directory
        from the component directory.
        TODO: COM_ATMOS_ANALYSIS_PREV is hardwired here and this method is not appropriate in
        `analysis.py` and should be implemented in the component where this is applicable.

        Parameters
        ----------

        Returns
        ----------
        bias_dict: Dict
            a dictionary containing the list of observation bias files to copy for FileHandler
        """

        logger.info(f"Extracting a list of bias correction files from Jedi config file")
        observations = find_value_in_nested_dict(self.task_config.jedi_config, 'observations')
        logger.debug(f"observations:\n{pformat(observations)}")

        copylist = []
        for ob in observations['observers']:
            if 'obs bias' in ob.keys():
                obfile = ob['obs bias']['input file']
                obdir = os.path.dirname(obfile)
                basename = os.path.basename(obfile)
                prefix = '.'.join(basename.split('.')[:-2])
                for file in ['satbias.nc', 'satbias_cov.nc', 'tlapse.txt']:
                    bfile = f"{prefix}.{file}"
                    copylist.append([os.path.join(self.task_config.COM_ATMOS_ANALYSIS_PREV, bfile), os.path.join(obdir, bfile)])
                    # TODO: Why is this specific to ATMOS?

        bias_dict = {
            'mkdir': [os.path.join(self.task_config.DATA, 'bc')],
            'copy': copylist
        }
        return bias_dict

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
