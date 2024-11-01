#!/usr/bin/env python3

import glob
import os
import shutil
import tarfile
from logging import getLogger
from typing import Any, Dict, List

from wxflow import (AttrDict, FileHandler, Hsi, Htar, Task,
                    chgrp, get_gid, logit, mkdir_p, parse_j2yaml, rm_p, strftime,
                    to_YMDH)

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

        rotdir = self.task_config.ROTDIR + os.sep

        # Find all absolute paths in the environment and get their relative paths from ${ROTDIR}
        path_dict = self._gen_relative_paths(rotdir)

        # Extend task_config with path_dict
        self.task_config = AttrDict(**self.task_config, **path_dict)

    @logit(logger)
    def configure(self, arch_dict: Dict[str, Any]) -> (Dict[str, Any], List[Dict[str, Any]]):
        """Determine which tarballs will need to be created.

        Parameters
        ----------
        arch_dict : Dict[str, Any]
            Task specific keys, e.g. runtime options (DO_AERO_FCST, DO_ICE, etc)

        Return
        ------
        arcdir_set : Dict[str, Any]
            Set of FileHandler instructions to copy files to the ARCDIR
        atardir_sets : List[Dict[str, Any]]
            List of tarballs and instructions for creating them via tar or htar
        """

        if not os.path.isdir(arch_dict.ROTDIR):
            raise FileNotFoundError(f"FATAL ERROR: The ROTDIR ({arch_dict.ROTDIR}) does not exist!")

        if arch_dict.RUN in ["gdas", "gfs"]:

            # Copy the cyclone track files and rename the experiments
            # TODO This really doesn't belong in archiving and should be moved elsewhere
            Archive._rename_cyclone_expt(arch_dict)

        archive_parm = os.path.join(arch_dict.PARMgfs, "archive")

        # Collect the dataset to archive locally
        arcdir_j2yaml = os.path.join(archive_parm, f"{arch_dict.NET}_arcdir.yaml.j2")

        # Add the glob.glob function for capturing log filenames
        # TODO remove this kludge once log filenames are explicit
        arch_dict['glob'] = glob.glob

        # Add the os.path.exists function to the dict for yaml parsing
        arch_dict['path_exists'] = os.path.exists

        # Parse the input jinja yaml template
        arcdir_set = Archive._construct_arcdir_set(arcdir_j2yaml,
                                                   arch_dict)

        if not os.path.isdir(arch_dict.ROTDIR):
            raise FileNotFoundError(f"FATAL ERROR: The ROTDIR ({arch_dict.ROTDIR}) does not exist!")

        # Collect datasets that need to be archived
        # Each dataset represents one tarball

        if arch_dict.HPSSARCH:
            self.tar_cmd = "htar"
            self.hsi = Hsi()
            self.htar = Htar()
            self.cvf = self.htar.cvf
            self.rm_cmd = self.hsi.rm
            self.chgrp_cmd = self.hsi.chgrp
            self.chmod_cmd = self.hsi.chmod
        elif arch_dict.LOCALARCH:
            self.tar_cmd = "tar"
            self.cvf = Archive._create_tarball
            self.chgrp_cmd = chgrp
            self.chmod_cmd = os.chmod
            self.rm_cmd = rm_p
        else:  # Only perform local archiving.  Do not create tarballs.
            self.tar_cmd = ""
            return arcdir_set, []

        master_yaml = "master_" + arch_dict.RUN + ".yaml.j2"

        parsed_sets = parse_j2yaml(os.path.join(archive_parm, master_yaml),
                                   arch_dict,
                                   allow_missing=False)

        atardir_sets = []

        for dataset in parsed_sets.datasets.values():

            dataset["fileset"] = Archive._create_fileset(dataset)
            dataset["has_rstprod"] = Archive._has_rstprod(dataset.fileset)

            atardir_sets.append(dataset)

        return arcdir_set, atardir_sets

    @logit(logger)
    def execute_store_products(self, arcdir_set: Dict[str, Any]) -> None:
        """Perform local archiving of data products to ARCDIR.

        Parameters
        ----------
        arcdir_set : Dict[str, Any]
            FileHandler instructions to populate ARCDIR with

        Return
        ------
        None
        """

        # Copy files to the local ARCDIR
        FileHandler(arcdir_set).sync()

    @logit(logger)
    def execute_backup_dataset(self, atardir_set: Dict[str, Any]) -> None:
        """Create a backup tarball from a yaml dict.

        Parameters
        ----------
        atardir_set: Dict[str, Any]
            Dict defining set of files to backup and the target tarball.

        Return
        ------
        None
        """

        # Generate tarball
        if len(atardir_set.fileset) == 0:
            logger.warning(f"WARNING: skipping would-be empty archive {atardir_set.target}.")
            return

        if atardir_set.has_rstprod:

            try:
                self.cvf(atardir_set.target, atardir_set.fileset)
            # Regardless of exception type, attempt to remove the target
            except Exception:
                self.rm_cmd(atardir_set.target)
                raise RuntimeError(f"FATAL ERROR: Failed to create restricted archive {atardir_set.target}, deleting!")

            self._protect_rstprod(atardir_set)

        else:
            self.cvf(atardir_set.target, atardir_set.fileset)

    @staticmethod
    @logit(logger)
    def _create_fileset(atardir_set: Dict[str, Any]) -> List:
        """
        Collect the list of all available files from the parsed yaml dict.
        Globs are expanded and if required files are missing, an error is
        raised.

        TODO: expand all globs in the jinja yaml files instead of expanding
              them here and issue errors here if globbing patterns (*, ?, [])
              are found.

        Parameters
        ----------
        atardir_set: Dict
            Contains full paths for required and optional files to be archived.
        """

        fileset = []
        if "required" in atardir_set:
            if atardir_set.required is not None:
                for item in atardir_set.required:
                    glob_set = glob.glob(item)
                    if len(glob_set) == 0:
                        raise FileNotFoundError(f"FATAL ERROR: Required file, directory, or glob {item} not found!")
                    for entry in glob_set:
                        fileset.append(entry)

        if "optional" in atardir_set:
            if atardir_set.optional is not None:
                for item in atardir_set.optional:
                    glob_set = glob.glob(item)
                    if len(glob_set) == 0:
                        logger.warning(f"WARNING: optional file/glob {item} not found!")
                    else:
                        for entry in glob_set:
                            fileset.append(entry)

        return fileset

    @staticmethod
    @logit(logger)
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

        try:
            self.chgrp_cmd("rstprod", atardir_set.target)
            if self.tar_cmd == "htar":
                self.chmod_cmd("640", atardir_set.target)
            else:
                self.chmod_cmd(atardir_set.target, 0o640)
        # Regardless of exception type, attempt to remove the target
        except Exception:
            try:
                self.rm_cmd(atardir_set.target)
            finally:
                raise RuntimeError(f"FATAL ERROR: Failed to protect {atardir_set.target}!\n"
                                   f"Please verify that it has been deleted!!")

    @staticmethod
    @logit(logger)
    def _create_tarball(target: str, fileset: List) -> None:
        """Method to create a local tarball.

        Parameters
        ----------
        target : str
            Tarball to create

        file_list : List
            List of files to add to an archive
        """

        # TODO create a set of tar helper functions in wxflow
        # Attempt to create the parent directory if it does not exist
        mkdir_p(os.path.dirname(os.path.realpath(target)))

        # Create the archive
        with tarfile.open(target, "w") as tarball:
            for filename in fileset:
                tarball.add(filename)

    @logit(logger)
    def _gen_relative_paths(self, root_path: str) -> Dict:
        """Generate a dict of paths in self.task_config relative to root_path

        Parameters
        ----------
        root_path : str
            Path to base all relative paths off of

        Return
        ------
        rel_path_dict : Dict
            Dictionary of paths relative to root_path.  Members will be named
            based on the dict names in self.config.  For COM paths, the names will
            follow COMIN_<NAME> --> <name>_dir.  For all other directories, the
            names will follow <NAME> --> <name>_dir.
        """

        rel_path_dict = {}
        for key, value in self.task_config.items():
            if isinstance(value, str):
                if root_path in value:
                    rel_path = value.replace(root_path, "")
                    rel_key = (key[4:] if key.startswith("COMIN_") else key).lower() + "_dir"
                    rel_path_dict[rel_key] = rel_path

        return rel_path_dict

    @staticmethod
    @logit(logger)
    def _construct_arcdir_set(arcdir_j2yaml, arch_dict) -> Dict:
        """Construct the list of files to send to the ARCDIR and Fit2Obs
           directories from a template.

           TODO Copying Fit2Obs data doesn't belong in archiving should be
                moved elsewhere.

        Parameters
        ----------
        arcdir_j2yaml: str
            The filename of the ARCDIR jinja template to parse.

        arch_dict: Dict
            The context dictionary to parse arcdir_j2yaml with.

        Return
        ------
        arcdir_set : Dict
            FileHandler dictionary (i.e. with top level "mkdir" and "copy" keys)
            containing all directories that need to be created and what data
            files need to be copied to the ARCDIR and the Fit2Obs directory.
        """

        # Get the FileHandler dictionary for creating directories and copying
        # to the ARCDIR and VFYARC directories.
        arcdir_set = parse_j2yaml(arcdir_j2yaml,
                                  arch_dict,
                                  allow_missing=True)

        return arcdir_set

    @staticmethod
    @logit(logger)
    def _rename_cyclone_expt(arch_dict) -> None:

        # Rename the experiment in the tracker files from "AVNO" to the
        # first 4 letters of PSLOT.
        pslot4 = arch_dict.PSLOT.upper()
        if len(arch_dict.PSLOT) > 4:
            pslot4 = arch_dict.PSLOT[0:4].upper()

        track_dir_in = arch_dict.COMIN_ATMOS_TRACK
        track_dir_out = arch_dict.COMOUT_ATMOS_TRACK
        run = arch_dict.RUN
        cycle_HH = strftime(arch_dict.current_cycle, "%H")

        if run == "gfs":
            in_track_file = (track_dir_in + "/avno.t" +
                             cycle_HH + "z.cyclone.trackatcfunix")
            in_track_p_file = (track_dir_in + "/avnop.t" +
                               cycle_HH + "z.cyclone.trackatcfunix")
        elif run == "gdas":
            in_track_file = (track_dir_in + "/gdas.t" +
                             cycle_HH + "z.cyclone.trackatcfunix")
            in_track_p_file = (track_dir_in + "/gdasp.t" +
                               cycle_HH + "z.cyclone.trackatcfunix")

        if not os.path.isfile(in_track_file):
            # Do not attempt to archive the outputs
            return

        out_track_file = track_dir_out + "/atcfunix." + run + "." + to_YMDH(arch_dict.current_cycle)
        out_track_p_file = track_dir_out + "/atcfunixp." + run + "." + to_YMDH(arch_dict.current_cycle)

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

            out_lines = [line.replace(search_str, replace_str) for line in lines]

            with open("/tmp/track_file", "w") as new_file:
                new_file.writelines(out_lines)

            shutil.move("/tmp/track_file", filename_out)

        replace_string_from_to_file(in_track_file, out_track_file, "AVNO", pslot4)
        replace_string_from_to_file(in_track_p_file, out_track_p_file, "AVNO", pslot4)

        return
