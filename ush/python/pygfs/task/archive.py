#!/usr/bin/env python3

from yaml import load
from yaml import CLoader as Loader
from typing import Dict, Any
from wxflow import (
         logit,
         cast_strdict_as_dtypedict,
         AttrDict,
         Task,
         parse_j2yaml,
         archive_utils)
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

    @staticmethod
    @logit(logger)
    def configure(arch_dict: Dict[str, Any]) -> None:
        """Determine which tarballs will need to be created.

        Parameters
        ----------
        arch_dict : Dict
            Task specific keys, e.g. runtime options (DO_AERO, DO_ICE, etc)
        """

        # Collect datasets that need to be archived
        # Each dataset represents one tarball

        if(arch_dict.RUN == "gdas"):

            datasets = ['gdas'] # , 'gdas_restarta', 'gdas_restartb']

            #if(arch_dict.DO_ICE == "YES"):
            #    datasets.append('gdas_ice')
            #    datasets.append('gdas_ice_restart')

            #if(arch_dict.DO_OCN == "YES"):
            #    datasets.append('gdas_ocean_6hravg')
            #    datasets.append('gdas_ocean_daily')
            #    datasets.append('gdas_ocean_grib2')

            #if(arch_dict.DO_WAVE == "YES"):
            #    datasets.append('gdas_wave')

        elif (arch_dict.RUN == "gfs"):
            print("Set me up!")

        else:
            raise NotImplementedError(f'Archiving is not enabled for {arch_dict.RUN} runs')

        tar_cmd = 'tar'
        if arch_dict.HPSSARCH:
            tar_cmd = 'htar'

        archive_sets = []

        archive_parm = os.path.join(arch_dict.PARMgfs, "archive")

        for dataset in datasets:

            archive_filename = os.path.join(archive_parm, dataset + ".yaml.j2")
            archive_set = parse_j2yaml(archive_filename, arch_dict)
            archive_set = Archive._create_fileset(archive_set)
            archive_set['protocol'] = tar_cmd

            archive_sets.append(archive_set)

        return archive_sets

    @staticmethod
    @logit(logger)
    def execute(archive_sets: list[Dict[str, Any]]) -> None:
        """Create the tarballs from the list of yaml dicts.

        Parameters
        ----------
        arch_dict : Dict
            Task specific keys, e.g. runtime options (DO_AERO, DO_ICE, etc)
        """

        import tarfile

        for archive_set in archive_sets:
            archive_utils.ArchiveHandler(archive_set).create()

    @logit(logger)
    def _create_fileset(archive_set: Dict[str, Any]) -> None:
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

        import glob

        archive_set.fileset = []
        if 'mandatory' in archive_set:
            for item in archive_set.mandatory:
                glob_set = glob.glob(item)
                if len(glob_set) == 0:
                    raise FileNotFoundError(f'Mandatory file or glob {item} not found!')
                for file in glob_set:
                    Archive._check_isfile(file)
                    archive_set.fileset.append(file)

        if 'optional' in archive_set:
            for item in archive_set.optional:
                glob_set = glob.glob(item)
                if len(glob_set) == 0:
                    print (f'WARNING: optional file/glob {item} not found!')
                else:
                    for file in glob_set:
                        Archive._check_isfile(file,False)
                        archive_set.fileset.append(file)

        if len(archive_set.fileset) == 0:
            print (f'WARNING: the fileset for the {archive_set.name} archive is empty!')

        return archive_set

    @logit(logger)
    def _check_isfile(filename: str, error_on_noexist = True) -> None:
        """
        Checks that the input filename is not a directory.
        TODO: Expand this to check that 'filename' is not a glob and that
              the file actually exists.

        Parameters
        ----------
        filename : str
            The name of the file to check including the path.

        error_on_noexist : boolean
            (not yet implemented)
            If the file does not exist, this flag determines if a warning or
            error should be generated.
            Errors will always be issued if filename points to a directory.
        """

        if os.path.isdir(filename):
            raise IsADirectoryError(f'{file} is a directory\n' + 
                  f'only files are allowed to be archived.')
