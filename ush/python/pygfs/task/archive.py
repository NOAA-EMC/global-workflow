#!/usr/bin/env python3

import glob
import os
import shutil
from datetime import timedelta
from logging import getLogger
from typing import Any, Dict, List

import numpy as np
from wxflow import (AttrDict, FileHandler, Hsi, Htar, Task, cast_strdict_as_dtypedict,
                    chgrp, get_gid, logit, mkdir_p, parse_j2yaml, rm_p, strftime,
                    to_YMD, to_YMDH, Template, TemplateConstants)
from yaml import CLoader as Loader
from yaml import load

logger = getLogger(__name__.split('.')[-1])


class Archive(Task):
    """Task to archive ROTDIR data to HPSS (or locally)
    """

    @logit(logger, name="Archive")
    def __init__(self, config: Dict[str, Any]) -> None:
        """Constructor for the Archive task
        The constructor is responsible for collecting necessary yamls based on
        the runtime options and RUN.

        Parameters
        ----------
        config : Dict[str, Any]
            Incoming configuration for the task from the environment

        Returns
        -------
        None
        """
        super().__init__(config)

        rotdir = self.config.ROTDIR + os.sep

        # Find all absolute paths in the environment and get their relative paths from ${ROTDIR}
        path_dict = self._gen_relative_paths(rotdir)

        self.task_config = AttrDict(**self.config, **self.runtime_config, **path_dict)

    @logit(logger)
    def configure(self, arch_dict: Dict[str, Any]) -> (Dict[str, Any], List[Dict[str, Any]]):
        """Determine which tarballs will need to be created.

        Parameters
        ----------
        arch_dict : Dict[str, Any]
            Task specific keys, e.g. runtime options (DO_AERO, DO_ICE, etc)

        Return
        ------
        arcdir_set : Dict[str, Any]
            Set of FileHandler instructions to copy files to the ARCDIR
        atardir_sets : List[Dict[str, Any]]
            List of tarballs and instructions for creating them via tar or htar
        """

        archive_parm = os.path.join(arch_dict.PARMgfs, "archive")

        # Collect the dataset to archive locally
        arcdir_filename = os.path.join(archive_parm, "arcdir.yaml.j2")

        # Add the glob.glob function for capturing log filenames
        # TODO remove this kludge once log filenames are explicit
        arch_dict['glob'] = glob.glob

        # Add the os.path.exists function to the dict for yaml parsing
        arch_dict['path_exists'] = os.path.exists

        # Parse the input jinja yaml template
        arcdir_set = parse_j2yaml(arcdir_filename, arch_dict)

        # Collect datasets that need to be archived
        # Each dataset represents one tarball

        if arch_dict.HPSSARCH:
            self.tar_cmd = "htar"
            self.hsi = Hsi()
            self.htar = Htar()
        elif arch_dict.LOCALARCH:
            self.tar_cmd = "tar"
        else:  # Only perform local archiving.  Do not create tarballs.
            self.tar_cmd = ""
            return arcdir_set, []

        if not os.path.isdir(arch_dict.ROTDIR):
            raise FileNotFoundError(f"The ROTDIR ({arch_dict.ROTDIR}) does not exist!")

        if arch_dict.RUN == "gdas" or arch_dict.RUN == "gfs":

            # Copy the cyclone track files and rename the experiments
            Archive._rename_cyclone_expt(arch_dict)

        if arch_dict.RUN == "gdas":
            master_yaml = "master_gdas.yaml.j2"
        elif arch_dict.RUN == "gfs":
            master_yaml = "master_gfs.yaml.j2"
        elif arch_dict.RUN == "enkfgdas" or arch_dict.RUN == "enkfgfs":
            master_yaml = "master_enkf.yaml.j2"
        elif arch_dict.RUN == "gefs":
            raise NotImplementedError("Archiving is not yet set up for GEFS runs")
        else:
            raise ValueError(f"Archiving is not enabled for {arch_dict.RUN} runs")

        parsed_sets = parse_j2yaml(master_yaml, arch_dict)

        atardir_sets = []

        for dataset in parsed_set.datasets:

            dataset["fileset"] = Archive._create_fileset(dataset)
            dataset["has_rstprod"] = Archive._has_rstprod(dataset.fileset)

            atardir_sets.append(dataset)

        return arcdir_set, atardir_sets

    @logit(logger)
    def execute(self, arcdir_set: Dict[str, Any], atardir_sets: List[Dict[str, Any]]) -> None:
        """Perform local archiving to ARCDIR and create the tarballs from the list of yaml dicts.

        Parameters
        ----------
        arcdir_set : Dict[str, Any]
            FileHandler instructions to populate ARCDIR with
        atardir_sets: List[Dict[str, Any]]
            Sets of files to archive via tar or htar

        Return
        ------
        None
        """

        # Copy files to the local ARCDIR
        for key in arcdir_set.keys():
            FileHandler(arcdir_set[key]).sync()

        # Generate tarballs
        for atardir_set in atardir_sets:

            if len(atardir_set.fileset) == 0:
                print(f"WARNING skipping would-be empty archive {atardir_set.target}.")
                continue

            if self.tar_cmd == "htar":
                cvf = self.htar.cvf
                create = self.htar.create
                rm_cmd = self.hsi.rm
            else:
                cvf = Archive._create_tarball
                rm_cmd = rm_p

            if atardir_set.has_rstprod:

                try:
                    cvf(atardir_set.target, atardir_set.fileset)
                # Regardless of exception type, attempt to remove the target
                except Exception:
                    rm_cmd(atardir_set.target)
                    raise RuntimeError(f"Failed to create restricted archive {atardir_set.target}, deleting!")

                self._protect_rstprod(atardir_set)

            else:
                cvf(atardir_set.target, atardir_set.fileset)

    @logit(logger)
    @staticmethod
    def _create_fileset(atardir_set: Dict[str, Any]) -> List:
        """
        Collect the list of all available files from the parsed yaml dict.
        Globs are expanded and if mandatory files are missing, an error is
        raised.

        TODO: expand all globs in the jinja yaml files instead of expanding
              them here and issue errors here if globbing patterns (*, ?, [])
              are found.

        Parameters
        ----------
        atardir_set: Dict
            Contains full paths for mandatory and optional files to be archived.
        """

        fileset = []
        if "mandatory" in atardir_set:
            if atardir_set.mandatory is not None:
                for item in atardir_set.mandatory:
                    glob_set = glob.glob(item)
                    if len(glob_set) == 0:
                        raise FileNotFoundError(f"Mandatory file, directory, or glob {item} not found!")
                    for entry in glob_set:
                        fileset.append(entry)

        if "optional" in atardir_set:
            if atardir_set.optional is not None:
                for item in atardir_set.optional:
                    glob_set = glob.glob(item)
                    if len(glob_set) == 0:
                        print(f"WARNING: optional file/glob {item} not found!")
                    else:
                        for entry in glob_set:
                            fileset.append(entry)

        return fileset

    @logit(logger)
    @staticmethod
    def _has_rstprod(fileset: List) -> bool:
        """
        Checks if any files in the input fileset belongs to rstprod.

        Parameters
        ----------
        fileset : List
            List of filenames to check.
        """

        try:
            rstprod_gid = get_gid("rstprod")
        except KeyError:
            # rstprod does not exist on this machine
            return False

        # Expand globs and check each file for group ownership
        for file_or_glob in fileset:
            glob_set = glob.glob(file_or_glob)
            for filename in glob_set:
                if os.stat(filename).st_gid == rstprod_gid:
                    return True

        return False

    @logit(logger)
    def _protect_rstprod(self, atardir_set: Dict[str, any]) -> None:
        """
        Changes the group of the target tarball to rstprod and the permissions to
        640.  If this fails for any reason, attempt to delete the file before exiting.

        """

        if self.tar_cmd == "htar":
            chgrp_cmd = self.hsi.chgrp
            chmod_cmd = self.hsi.chmod
            rm_cmd = self.hsi.rm
        elif self.tar_cmd == "tar":
            chgrp_cmd = chgrp
            chmod_cmd = os.fchmod
            rm_cmd = rm_p
        else:
            raise KeyError(f"Invalid archiving command given: {self.tar_cmd}")

        try:
            if self.tar_cmd == "htar":
                self.hsi.chgrp("rstprod", atardir_set.target)
                self.hsi.chmod("640", atardir_set.target)
            else:
                chgrp("rstprod", atardir_set.target)
                os.chmod(atardir_set.target, 0o640)
        # Regardless of exception type, attempt to remove the target
        except Exception:
            try:
                if self.tar_cmd == "htar":
                    self.hsi.rm(atardir_set.target)
                else:
                    rm_p(atardir_set.target)
            finally:
                raise RuntimeError(f"Failed to protect {atardir_set.target}!\n"
                                   f"Please verify that it has been deleted!!")

    @logit(logger)
    @staticmethod
    def _create_tarball(target: str, fileset: List) -> None:
        """Method to create a local tarball.

        Parameters
        ----------
        target : str
            Tarball to create

        file_list : List
            List of files to add to an archive
        """
        import tarfile

        # TODO create a set of tar helper functions in wxflow
        # Attempt to create the parent directory if it does not exist
        mkdir_p(os.path.dirname(os.path.realpath(target)))

        # Create the archive
        with tarfile.open(target, "w") as tarball:
            for filename in fileset:
                tarball.add(filename)

    @logit(logger)
    def _gen_relative_paths(self, root_path: str) -> Dict:
        """Generate a dict of paths in self.config relative to root_path

        Parameters
        ----------
        root_path : str
            Path to base all relative paths off of

        Return
        ------
        rel_path_dict : Dict
            Dictionary of paths relative to root_path.  Members will be named
            based on the dict names in self.config.  For COM paths, the names will
            follow COM_<NAME> --> <name>_dir.  For all other directories, the
            names will follow <NAME> --> <name>_dir.
        """

        rel_path_dict = {}
        for key, value in self.config.items():
            if isinstance(value, str):
                if root_path in value:
                    rel_path = value.replace(root_path, "")
                    rel_key = (key[4:] if key.startswith("COM_") else key).lower() + "_dir"
                    rel_path_dict[rel_key] = rel_path

        return rel_path_dict

    @logit(logger)
    @staticmethod
    def _rename_cyclone_expt(arch_dict) -> None:

        # Rename the experiment in the tracker files from "AVNO" to the
        # first 4 letters of PSLOT.
        pslot4 = arch_dict.PSLOT.upper()
        if len(arch_dict.PSLOT) > 4:
            pslot4 = arch_dict.PSLOT[0:4].upper()

        track_dir = arch_dict.COM_ATMOS_TRACK
        run = arch_dict.RUN
        cycle_HH = strftime(arch_dict.current_cycle, "%H")

        if run == "gfs":
            in_track_file = (track_dir + "/avno.t" +
                             cycle_HH + "z.cycle.trackatcfunix")
            in_track_p_file = (track_dir + "/avnop.t" +
                               cycle_HH + "z.cycle.trackatcfunixp")
        elif run == "gdas":
            in_track_file = (track_dir + "/gdas.t" +
                             cycle_HH + "z.cycle.trackatcfunix")
            in_track_p_file = (track_dir + "/gdasp.t" +
                               cycle_HH + "z.cycle.trackatcfunixp")

        if not os.path.isfile(in_track_file):
            # Do not attempt to archive the outputs
            return

        out_track_file = track_dir + "/atcfunix." + run + "." + to_YMDH(arch_dict.current_cycle)
        out_track_p_file = track_dir + "/atcfunixp." + run + "." + to_YMDH(arch_dict.current_cycle)

        def replace_string_from_to_file(filename_in, filename_out, search_str, replace_str):

            """Write a new file from the contents of an input file while searching
            and replacing ASCII strings.  To prevent partial file creation, a
            temporary file is created and moved to the final location only
            after the search/replace is finished.

            Parameters
            ----------
            filename_in : str
                Input filename

            filename_out : str
                Output filename

            search_str : str
                ASCII string to search for

            replace_str : str
                ASCII string to replace the search_str with
            """
            with open(filename_in) as old_file:
                lines = old_file.readlines()

            out_lines = [line.replace(search, replace) for line in lines]

            with open("/tmp/track_file", "w") as new_file:
                new_file.writelines(out_lines)

            shutil.move("tmp/track_file", filename_out)

        replace_string_from_to_file(in_track_file, out_track_file, "AVNO", pslot4)
        replace_string_from_to_file(in_track_p_file, out_track_p_file, "AVNO", pslot4)

        return

    @staticmethod
    def _create_enkf_com_lists(arch_dict: Dict[str, any]) -> Dict:

        """Creates lists of ENKF member com directories from templates

        Parameters
        ----------
        arch_dict : dict
            Archiving context dictionary including COM_*_TMPL variables

        Return
        ------
        arch_dict : dict
            Updated context with lists for all ensemble members to be included
        """

        # Determine which members to archive
        first_mem = (arch_dict.ENSGRP - 1) * arch_dict.NMEM_EARCGRP + 1
        last_mem = min(arch_dict.NMEM_ENS,
                       arch_dict.ENSGRP * arch_dict.NMEM_EARCGRP)
        mem_list = [f"{mem:03d}" for mem in range(first_mem, last_mem + 1)]

        arch_dict["first_group_mem"] = first_mem
        arch_dict["last_group_mem"] = last_mem

        # Create a list of IAU forecast hours from IAUFHRS
        if isinstance(arch_dict.IAUFHRS, int):
            # First half-cycle or if 3dvar
            arch_dict["iaufhrs"] = [arch_dict.IAUFHRS]
        else:
            arch_dict["iaufhrs"] = [int(fhr) for fhr in arch_dict.IAUFHRS.split(",")]
        # Create a dict to define COM template parameters
        tmpl_dict = {
            'ROTDIR': arch_dict.ROTDIR,
            'RUN': arch_dict.RUN,
            'YMD': to_YMD(arch_dict.current_cycle),
            'HH': arch_dict.current_cycle.strftime('%H')
        }

        # Get a list of all COM templates (e.g. COM_ATMOS_ANALYSIS_TMPL)
        com_tmpl_names = [key for key in arch_dict.keys()
                     if key.startswith("COM_") and key.endswith("_TMPL")]

        # Go through each template and 
        for com_tmpl_name in com_tmpl_names: 

            com_tmpl = arch_dict[com_tmpl_name]

            # Construct lists of COM directories to archive data from
            mem_com_list = []
            for mem in mem_list:
                tmpl_dict["MEMDIR"] = "mem" + mem
                mem_com_list.append(Template.substitute_structure(
                                    com_tmpl,
                                    TemplateConstants.DOLLAR_CURLY_BRACE,
                                    tmpl_dict.get))
            mem_com_list_name = com_tmpl_name.replace("_TMPL","_MEM_list")
            arch_dict[mem_com_list_name] = mem_com_list

        return arch_dict
