#!/usr/bin/env python3

from datetime import timedelta

import glob

from logging import getLogger

import os

from typing import Dict, Any, List

from yaml import load
from yaml import CLoader as Loader

from wxflow import (logit,
                    cast_strdict_as_dtypedict,
                    AttrDict,
                    get_gid,
                    Task,
                    Htar,
                    Hsi,
                    rm_p,
                    mkdir_p,
                    chgrp,
                    FileHandler,
                    parse_j2yaml)

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
             'cycle_YYYYMMDD': self.runtime_config.current_cycle.strftime("%Y%m%d"),
             'first_cycle': self.runtime_config.current_cycle == self.config.SDATE
             }
        )

        if self.config.REALTIME:
            local_dict['mos_YYYYMMDDHH'] = (self.runtime_config.current_cycle - timedelta(days=1)).strftime("%Y%m%d%H")
        else:
            local_dict['mos_YYYYMMDDHH'] = cycle_YYYYMMDDHH

        local_dict['mos_YYYYMMDD'] = local_dict['mos_YYYYMMDDHH'][:8]

        # Add the os.path.exists function for use in parsing the jinja files
        local_dict['path_exists'] = os.path.exists

        self.task_config = AttrDict(**self.config, **self.runtime_config, **path_dict, **local_dict)

    @classmethod
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
        arcdir_set = parse_j2yaml(arcdir_filename, arch_dict)

        # Collect datasets that need to be archived
        # Each dataset represents one tarball

        if arch_dict.HPSSARCH:
            self.tar_cmd = "htar"
            self.hsi = staticmethod(Hsi())
            self.htar = staticmethod(Htar())
        elif arch_dict.LOCALARCH:
            self.tar_cmd = "tar"
        else:  # Only perform local archiving.  Do not create tarballs.
            self.tar_cmd = ""
            return arcdir_set, []

        if not os.path.isdir(arch_dict.ROTDIR):
            raise FileNotFoundError(f"The ROTDIR ({arch_dict.ROTDIR}) does not exist!")

        # Pull out some common variables
        cycle_YYYYMMDDHH = arch_dict.cycle_YYYYMMDDHH
        cycle_HH = arch_dict.cycle_HH
        first_cycle = arch_dict.first_cycle
        ARCH_WARMICFREQ = arch_dict.ARCH_WARMICFREQ
        ARCH_FCSTICFREQ = arch_dict.ARCH_FCSTICFREQ
        assim_freq = arch_dict.assim_freq

        if arch_dict.RUN == "gdas" or arch_dict.RUN == "gfs":

            ARCHINC_CYC = arch_dict.ARCH_CYC
            ARCHICS_CYC = ARCHINC_CYC - assim_freq
            if ARCHICS_CYC < 0:
                ARCHICS_CYC += 24

            mm = cycle_YYYYMMDDHH[4:6]
            dd = cycle_YYYYMMDDHH[6:8]
            # TODO: This math yields multiple dates sharing the same nday
            nday = (int(mm) - 1) * 30 + int(dd)
            mod = nday % int(ARCH_WARMICFREQ)

            save_warm_ic_a = False
            save_warm_ic_b = False
            if first_cycle and cycle_HH == ARCHINC_CYC:
                save_warm_ic_a = True
                save_warm_ic_b = True
            elif mod == 0 and cycle_HH == ARCHINC_CYC:
                save_warm_ic_a = True
                save_warm_ic_b = True

            if ARCHICS_CYC == "18":
                nday1 = nday + 1
                mod1 = nday1 % int(ARCH_WARMICFREQ)

                if cycle_HH == ARCHICS_CYC:
                    if mod1 == 0:
                        save_warm_ic_b = True
                    elif first_cycle:
                        save_warm_ic_b = True
                    else:
                        save_warm_ic_b = False

            mod = nday % int(ARCH_FCSTICFREQ)

            SAVEFCSTIC = False
            if mod == 0 or first_day:
                SAVEFCSTIC = True

        if arch_dict.RUN == "gdas":

            datasets = ['gdas']

            if save_warm_ic_a or arch_dict.SAVEFCSTIC:
                datasets.append("gdas_restarta")
                if arch_dict.DO_WAVE:
                    datasets.append("gdaswave_restart")
                if arch_dict.DO_OCN:
                    datasets.append("gdasocean_restart")
                if arch_dict.DO_ICE:
                    datasets.append("gdasice_restart")

            if save_warm_ic_b or arch_dict.SAVEFCSTIC:
                datasets.append("gdas_restartb")

            if arch_dict.DO_ICE == "YES":
                datasets.append('gdasice')

            if arch_dict.DO_OCN == "YES":
                datasets.append('gdasocean')
                datasets.append('gdasocean_analysis')

            if arch_dict.DO_WAVE == "YES":
                datasets.append('gdaswave')

        elif arch_dict.RUN == "gfs":
            raise NotImplementedError("Archiving is not yet set up for GFS runs")

        elif arch_dict.RUN == "enkfgdas":
            raise NotImplementedError("Archiving is not yet set up for ENKF GDAS runs")

        elif arch_dict.RUN == "enkfgfs":
            raise NotImplementedError("Archiving is not yet set up for ENKF GFS runs")

        elif arch_dict.RUN == "gefs":
            raise NotImplementedError("Archiving is not yet set up for GEFS runs")

        else:
            raise ValueError(f'Archiving is not enabled for {arch_dict.RUN} runs')

        atardir_sets = []

        for dataset in datasets:

            archive_filename = os.path.join(archive_parm, dataset + ".yaml.j2")
            atardir_set = parse_j2yaml(archive_filename, arch_dict)
            atardir_set['fileset'] = Archive._create_fileset(atardir_set)
            atardir_set['has_rstprod'] = Archive._has_rstprod(atardir_set.fileset)

            atardir_sets.append(atardir_set)

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

            if self.tar_cmd == "htar":
                cvf = self.htar.cvf
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
        if 'mandatory' in atardir_set:
            for item in atardir_set.mandatory:
                glob_set = glob.glob(item)
                if len(glob_set) == 0:
                    raise FileNotFoundError(f'Mandatory file, directory, or glob {item} not found!')
                for entry in glob_set:
                    fileset.append(entry)

        if 'optional' in atardir_set:
            for item in atardir_set.optional:
                glob_set = glob.glob(item)
                if len(glob_set) == 0:
                    print(f'WARNING: optional file/glob {item} not found!')
                else:
                    for entry in glob_set:
                        fileset.append(entry)

        if len(fileset) == 0:
            print(f'WARNING: the fileset for the {atardir_set.name} archive is empty!')

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
                    rel_path = value.replace(root_path, '')
                    rel_key = (key[4:] if key.startswith("COM_") else key).lower() + "_dir"
                    rel_path_dict[rel_key] = rel_path

        return rel_path_dict
