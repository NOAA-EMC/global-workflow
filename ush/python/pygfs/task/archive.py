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

        rotdir=self.config.ROTDIR + os.sep

        # Collect COM locations relative to ${ROTDIR}
        local_dict = AttrDict(
            {'atmos_analysis_dir': self.config.COM_ATMOS_ANALYSIS.replace(rotdir,''),
            'atmos_bufr_dir': self.config.COM_ATMOS_BUFR.replace(rotdir,''),
            'atmos_gempak_dir': self.config.COM_ATMOS_GEMPAK.replace(rotdir,''),
            'atmos_grib_0p25_dir': self.config.COM_ATMOS_GRIB_0p25.replace(rotdir,''),
            'atmos_grib_0p50_dir': self.config.COM_ATMOS_GRIB_0p50.replace(rotdir,''),
            'atmos_grib_1p00_dir': self.config.COM_ATMOS_GRIB_1p00.replace(rotdir,''),
            'atmos_history_dir': self.config.COM_ATMOS_HISTORY.replace(rotdir,''),
            'atmos_input_dir': self.config.COM_ATMOS_INPUT.replace(rotdir,''),
            'atmos_master_dir': self.config.COM_ATMOS_MASTER.replace(rotdir,''),
            'atmos_minmon_dir': self.config.COM_ATMOS_MINMON.replace(rotdir,''),
            'atmos_oznmon_dir': self.config.COM_ATMOS_OZNMON.replace(rotdir,''),
            'atmos_radmon_dir': self.config.COM_ATMOS_RADMON.replace(rotdir,''),
            'atmos_restart_dir': self.config.COM_ATMOS_RESTART.replace(rotdir,''),
            'atmos_track_dir': self.config.COM_ATMOS_TRACK.replace(rotdir,''),
            'atmos_wmo_dir': self.config.COM_ATMOS_WMO.replace(rotdir,''),
            'chem_history_dir': self.config.COM_CHEM_HISTORY.replace(rotdir,''),
            'chem_analysis_dir': self.config.COM_CHEM_ANALYSIS.replace(rotdir,''),
            'conf_dir': self.config.COM_CONF.replace(rotdir,''),
            'ice_grib_dir': self.config.COM_ICE_GRIB.replace(rotdir,''),
            'ice_history_dir': self.config.COM_ICE_HISTORY.replace(rotdir,''),
            'med_restart_dir': self.config.COM_MED_RESTART.replace(rotdir,''),
            'obs_dir': self.config.COM_OBS.replace(rotdir,''),
            'ocean_analysis_dir': self.config.COM_OCEAN_ANALYSIS.replace(rotdir,''),
            'ocean_grib_dir': self.config.COM_OCEAN_GRIB.replace(rotdir,''),
            'ocean_history_dir': self.config.COM_OCEAN_HISTORY.replace(rotdir,''),
            'ocean_input_dir': self.config.COM_OCEAN_INPUT.replace(rotdir,''),
            'ocean_restart_dir': self.config.COM_OCEAN_RESTART.replace(rotdir,''),
            'wave_grid_dir': self.config.COM_WAVE_GRID.replace(rotdir,''),
            'wave_history_dir': self.config.COM_WAVE_HISTORY.replace(rotdir,''),
            'wave_restart_dir': self.config.COM_WAVE_RESTART.replace(rotdir,''),
            'wave_station_dir': self.config.COM_WAVE_STATION.replace(rotdir,'')
            }
        )

        self.task_config = AttrDict(**self.config, **self.runtime_config, **local_dict)

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
        else: #Nothing to do
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
                except:
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
                    print (f'WARNING: optional file/glob {item} not found!')
                else:
                    for entry in glob_set:
                        fileset.append(entry)

        if len(fileset) == 0:
            print (f'WARNING: the fileset for the {archive_set.name} archive is empty!')

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
        except:
            try:
                if self.tar_cmd == "htar":
                    self.hsi.rm(archive_set.target)
                else:
                    rm_p(archive_set.target)
            except:
                pass

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
