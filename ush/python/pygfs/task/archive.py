#!/usr/bin/env python3

import glob
from yaml import load
from yaml import CLoader as Loader
from typing import Dict, Any, List
from wxflow import (
         logit,
         cast_strdict_as_dtypedict,
         AttrDict,
         get_gid,
         Task,
         Htar,
         Hsi,
         rm_p,
         mkdir_p,
         chgrp,
         parse_j2yaml)
from logging import getLogger
import os

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
        local_dict = AttrDict(
          {'cycle_HH': self.runtime_config.current_cycle.strftime("%H"),
           'cycle_YYYYMMDDHH': self.runtime_config.current_cycle.strftime("%Y%m%d%H"),
           'first_cycle': self.runtime_config.current_cycle == self.config.SDATE
           }
        )

        self.task_config = AttrDict(**self.config, **self.runtime_config, **path_dict, **local_dict)

    @classmethod
    @logit(logger)
    def configure(self, arch_dict: Dict[str, Any]) -> list[Dict[str, Any]]:
        """Determine which tarballs will need to be created.

        Parameters
        ----------
        arch_dict : Dict
            Task specific keys, e.g. runtime options (DO_AERO, DO_ICE, etc)
        """

        # Collect datasets that need to be archived
        # Each dataset represents one tarball

        if arch_dict.HPSSARCH:
            self.tar_cmd = "htar"
            self.hsi = staticmethod(Hsi())
            self.htar = staticmethod(Htar())
        elif arch_dict.LOCALARCH:
            self.tar_cmd = "tar"
        else:  # Nothing to do
            raise ValueError(f"Neither HPSSARCH nor LOCALARCH are set to YES.\n"
                             f"Unable to determine archiving method!")

        if(arch_dict.RUN == "gdas"):

            datasets = ['gdas', 'gdas_restarta', 'gdas_restartb']

            if(arch_dict.DO_ICE == "YES"):
                datasets.append('gdas_ice')
                datasets.append('gdas_ice_restart')

            if(arch_dict.DO_OCN == "YES"):
                datasets.append('gdas_ocean_6hravg')
                datasets.append('gdas_ocean_daily')
                datasets.append('gdas_ocean_grib2')

            if(arch_dict.DO_WAVE == "YES"):
                datasets.append('gdas_wave')
                datasets.append('gdas_wave_restart')

        elif (arch_dict.RUN == "gfs"):
            raise NotImplementedError("Archiving is not yet set up for GFS runs")

        elif (arch_dict.RUN == "enkfgdas"):
            raise NotImplementedError("Archiving is not yet set up for ENKF GDAS runs")

        elif (arch_dict.RUN == "enkfgfs"):
            raise NotImplementedError("Archiving is not yet set up for ENKF GFS runs")

        elif (arch_dict.RUN == "gefs"):
            raise NotImplementedError("Archiving is not yet set up for GEFS runs")

        else:
            raise ValueError(f'Archiving is not enabled for {arch_dict.RUN} runs')

        archive_sets = []

        archive_parm = os.path.join(arch_dict.PARMgfs, "archive")

        for dataset in datasets:

            archive_filename = os.path.join(archive_parm, dataset + ".yaml.j2")
            archive_set = parse_j2yaml(archive_filename, arch_dict)
            archive_set['fileset'] = Archive._create_fileset(archive_set)
            archive_set['has_rstprod'] = Archive._has_rstprod(archive_set.fileset)

            archive_sets.append(archive_set)

        return archive_sets

    @logit(logger)
    def execute(self, archive_sets: list[Dict[str, Any]]) -> None:
        """Create the tarballs from the list of yaml dicts.

        Parameters
        ----------
        arch_dict : Dict
            Task specific keys, e.g. runtime options (DO_AERO, DO_ICE, etc)
        """

        for archive_set in archive_sets:

            if self.tar_cmd == "htar":
                cvf = self.htar.cvf
                rm_cmd = self.hsi.rm
            else:
                cvf = Archive._create_tarball
                rm_cmd = rm_p

            if archive_set.has_rstprod:

                try:
                    cvf(archive_set.target, archive_set.fileset)
                # Regardless of exception type, attempt to remove the target
                except:  # noqa
                    rm_cmd(archive_set.target)
                    raise RuntimeError(f"Failed to create restricted archive {archive_set.target}, deleting!")

                self._protect_rstprod(archive_set)

            else:
                cvf(archive_set.target, archive_set.fileset)

    @logit(logger)
    @staticmethod
    def _create_fileset(archive_set: Dict[str, Any]) -> list:
        """
        Collect the list of all available files from the parsed yaml dict.
        Globs are expanded and if mandatory files are missing, an error is
        raised.

        TODO: expand all globs in the jinja yaml files instead of expanding
              them here and issue errors here if globbing patterns (*, ?, [])
              are found.

        Parameters
        ----------
        archive_set: Dict
            Contains full paths for mandatory and optional files to be archived.
        """

        fileset = []
        if 'mandatory' in archive_set:
            for item in archive_set.mandatory:
                glob_set = glob.glob(item)
                if len(glob_set) == 0:
                    raise FileNotFoundError(f'Mandatory file, directory, or glob {item} not found!')
                for entry in glob_set:
                    fileset.append(entry)

        if 'optional' in archive_set:
            for item in archive_set.optional:
                glob_set = glob.glob(item)
                if len(glob_set) == 0:
                    print(f'WARNING: optional file/glob {item} not found!')
                else:
                    for entry in glob_set:
                        fileset.append(entry)

        if len(fileset) == 0:
            print(f'WARNING: the fileset for the {archive_set.name} archive is empty!')

        return fileset

    @logit(logger)
    @staticmethod
    def _has_rstprod(fileset: list) -> bool:
        """
        Checks if any files in the input fileset belongs to rstprod.

        Parameters
        ----------
        fileset : list
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
    def _protect_rstprod(self, archive_set: Dict[str, any]) -> None:
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
                self.hsi.chgrp("rstprod", archive_set.target)
                self.hsi.chmod("640", archive_set.target)
            else:
                chgrp("rstprod", archive_set.target)
                os.chmod(archive_set.target, 0o640)
        # Regardless of exception type, attempt to remove the target
        except:  # noqa
            try:
                if self.tar_cmd == "htar":
                    self.hsi.rm(archive_set.target)
                else:
                    rm_p(archive_set.target)
            finally:
                raise RuntimeError(f"Failed to protect {archive_set.target}!\n"
                                   f"Please verify that it has been deleted!!")

    @logit(logger)
    @staticmethod
    def _create_tarball(target: str, fileset: list) -> None:
        """Method to create a local tarball.

        Parameters
        ----------
        target : str
            Tarball to create

        file_list : list
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
                    rel_path = value.replace(root_path, '')
                    rel_key = (key[4:] if key.startswith("COM_") else key).lower() + "_dir"
                    rel_path_dict[rel_key] = rel_path

        return rel_path_dict
