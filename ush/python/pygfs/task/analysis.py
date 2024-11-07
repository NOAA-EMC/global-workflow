#!/usr/bin/env python3

import os
import glob
import tarfile
from logging import getLogger
from pprint import pformat
from netCDF4 import Dataset
from typing import List, Dict, Any, Union, Optional

from jcb import render
from wxflow import (parse_j2yaml, FileHandler, rm_p, logit,
                    Task, Executable, WorkflowException, to_fv3time, to_YMD,
                    Template, TemplateConstants)

logger = getLogger(__name__.split('.')[-1])


class Analysis(Task):
    """Parent class for GDAS tasks

    The Analysis class is the parent class for all
    Global Data Assimilation System (GDAS) tasks
    directly related to peforming an analysis
    """

    def __init__(self, config: Dict[str, Any]) -> None:
        super().__init__(config)
        # Store location of GDASApp jinja2 templates
        self.gdasapp_j2tmpl_dir = os.path.join(self.task_config.PARMgfs, 'gdas')
        # fix ocnres
        self.task_config.OCNRES = f"{self.task_config.OCNRES :03d}"

    def initialize(self) -> None:
        super().initialize()

        # all JEDI analyses need a JEDI config
        self.task_config.jedi_config = self.get_jedi_config()

        # all analyses need to stage observations
        obs_dict = self.get_obs_dict()
        FileHandler(obs_dict).sync()

        # link jedi executable to run directory
        self.link_jediexe()

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

            # Step 2: (optional) fill templates of algorithm override YAML and merge
            if 'JCB_ALGO_YAML' in self.task_config.keys():
                jcb_algo_config = parse_j2yaml(self.task_config.JCB_ALGO_YAML, self.task_config)
                jcb_config = {**jcb_config, **jcb_algo_config}

            # If algorithm is present override the algorithm in the JEDI config
            if algorithm:
                jcb_config['algorithm'] = algorithm

            # Step 3: generate the JEDI Yaml using JCB driving YAML
            jedi_config = render(jcb_config)
        elif 'JEDIYAML' in self.task_config.keys():
            # Generate JEDI YAML file (without using JCB)
            logger.info(f"Generate JEDI YAML config: {self.task_config.jedi_yaml}")
            jedi_config = parse_j2yaml(self.task_config.JEDIYAML, self.task_config,
                                       searchpath=self.gdasapp_j2tmpl_dir)
            logger.debug(f"JEDI config:\n{pformat(jedi_config)}")
        else:
            raise KeyError(f"Task config must contain JCB_BASE_YAML or JEDIYAML")

        logger.debug(f"JEDI config:\n{pformat(jedi_config)}")

        return jedi_config

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

        for itile in range(1, self.task_config.ntiles + 1):
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
    def link_jediexe(self) -> None:
        """

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

        return exe_dest

    @staticmethod
    @logit(logger)
    def tgz_diags(statfile: str, diagdir: str) -> None:
        """tar and gzip the diagnostic files resulting from a JEDI analysis.

        Parameters
        ----------
        statfile : str | os.PathLike
            Path to the output .tar.gz .tgz file that will contain the diag*.nc files e.g. atmstat.tgz
        diagdir : str | os.PathLike
            Directory containing JEDI diag files
        """

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(diagdir, 'diags', 'diag*nc'))
        diags.extend(glob.glob(os.path.join(diagdir, 'diags', 'diag*nc4')))

        logger.info(f"Compressing {len(diags)} diag files to {statfile}")

        # Open tar.gz file for writing
        with tarfile.open(statfile, "w:gz") as tgz:
            # Add diag files to tarball
            for diagfile in diags:
                tgz.add(diagfile, arcname=os.path.basename(diagfile))


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
