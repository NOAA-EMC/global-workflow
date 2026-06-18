#!/usr/bin/env python3

from logging import getLogger
from typing import Dict, Any
from wxflow import AttrDict, Task, add_to_datetime, to_timedelta, logit, FileHandler
from pygfs.obsprep.providers import ProviderConfig
from multiprocessing import Process, Manager
from os.path import join
from datetime import timedelta
import glob
from os.path import basename
import pathlib

logger = getLogger(__name__.split('.')[-1])


class MarineObsPrep(Task):
    """
    Class for preparing and managing marine observations
    """
    def __init__(self, config: Dict[str, Any]) -> None:
        super().__init__(config)

        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config['assim_freq']}H") / 2)
        _window_end = add_to_datetime(self.task_config.current_cycle, +to_timedelta(f"{self.task_config['assim_freq']}H") / 2)

        local_dict = AttrDict(
            {
                'window_begin': _window_begin,
                'window_end': _window_end,
                'PREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Initialize the Providers
        self.ghrsst = ProviderConfig.from_task_config("ghrsst", self.task_config)
        if self.task_config.cyc == 0:
            self.rads = ProviderConfig.from_task_config("rads", self.task_config)
        self.nesdis_amsr2 = ProviderConfig.from_task_config("nesdis_amsr2", self.task_config)
        self.nesdis_mirs = ProviderConfig.from_task_config("nesdis_mirs", self.task_config)
        self.nesdis_jpssrr = ProviderConfig.from_task_config("nesdis_jpssrr", self.task_config)
        self.smap = ProviderConfig.from_task_config("smap", self.task_config)
        self.smos = ProviderConfig.from_task_config("smos", self.task_config)

        # Initialize the list of processed ioda files
        # TODO: Does not work. This should be a list of gathered ioda files that are created
        #       across all processes
        self.ioda_files = []

    @logit(logger)
    def initialize(self) -> None:
        """
        """
        # Update the database with new files
        self.ghrsst.db.ingest_files()
        if self.task_config.cyc == 0:
            self.rads.db.ingest_files()
        self.nesdis_amsr2.db.ingest_files()
        self.nesdis_mirs.db.ingest_files()
        self.nesdis_jpssrr.db.ingest_files()
        self.smap.db.ingest_files()
        self.smos.db.ingest_files()

    @logit(logger)
    def execute(self) -> None:
        """
        """
        with Manager() as manager:
            # Use a Manager list to share ioda_files across processes
            shared_ioda_files = manager.list()

            processes = []
            for provider, obs_spaces in self.task_config.providers.items():
                logger.info(f"========= provider: {provider}")
                for obs_space in obs_spaces["list"]:
                    logger.info(f"========= obs_space: {obs_space}")

                    # Start a new process
                    process = Process(target=self.process_obs_space,
                                      args=(provider, obs_space, shared_ioda_files))
                    process.start()
                    processes.append(process)

            # Wait for all processes to complete
            for process in processes:
                process.join()

            # Convert the Manager list to a regular list
            self.ioda_files = list(shared_ioda_files)
            logger.info(f"Final ioda_files: {self.ioda_files}")

    @logit(logger)
    def process_obs_space(self,
                          provider: str,
                          obs_space: str,
                          shared_ioda_files) -> None:
        output_file = f"{self.task_config['RUN']}.t{self.task_config['cyc']:02d}z.{obs_space}.nc"

        # Process GHRSST
        if provider == "ghrsst":
            parts = obs_space.split("_")
            instrument = parts[1].upper()
            platform = parts[2].upper()

            # Process the observation space
            kwargs = {
                'provider': provider,
                'obs_space': obs_space,
                'instrument': instrument,
                'platform': platform,
                'obs_type': "SSTsubskin",
                'output_file': output_file,
                'window_begin': self.task_config.window_begin,
                'window_end': self.task_config.window_end,
                'task_config': self.task_config
            }
            result = self.ghrsst.process_obs_space(**kwargs)
            return result

        # Process RADS
        if provider == "rads":
            if not hasattr(self, 'rads'):
                logger.warning(
                    f"Skipping RADS processing — rads not initialized "
                    f"(cyc={self.task_config.cyc}, only runs at cyc=0)"
                )
                return None
            platform = obs_space.split("_")[2]
            instrument = None
            # TODO(G): Get the window size from the config
            window_begin = self.task_config.window_begin - timedelta(hours=72)
            window_end = self.task_config.window_begin + timedelta(hours=72)
            kwargs = {
                'provider': provider,
                'obs_space': obs_space,
                'instrument': instrument,
                'platform': platform,
                'obs_type': "",
                'output_file': output_file,
                'window_begin': window_begin,
                'window_end': window_end,
                'task_config': self.task_config
            }
            result = self.rads.process_obs_space(**kwargs)
            return result

        # Process NESDIS_AMSR2
        if provider == "nesdis_amsr2":
            # Only handling "icec_amsr2_" cases
            platform = "GW1"
            instrument = "AMSR2"
            satellite = "GW1"
            # TODO(G,M): Get the window size from the config
            window_begin = self.task_config.window_begin - timedelta(hours=30)
            window_end = self.task_config.window_begin + timedelta(hours=6)
            kwargs = {
                'provider': "amsr2",
                'obs_space': obs_space,
                'platform': platform,
                'instrument': instrument,
                'satellite': satellite,
                'obs_type': obs_space,
                'output_file': output_file,
                'window_begin': window_begin,
                'window_end': window_end,
                'task_config': self.task_config
            }
            result = self.nesdis_amsr2.process_obs_space(**kwargs)
            return result

        # Process NESDIS_MIRS
        if provider == "nesdis_mirs":
            # Handling all mirs cases
            platform = obs_space.split("_")[2]
            instrument = "MIRS"
            satellite = obs_space.split("_")[2]
            kwargs = {
                'provider': "mirs",
                'obs_space': obs_space,
                'platform': platform,
                'instrument': instrument,
                'satellite': satellite,
                'obs_type': obs_space,
                'output_file': output_file,
                'window_begin': self.task_config.window_begin,
                'window_end': self.task_config.window_end,
                'task_config': self.task_config
            }
            result = self.nesdis_mirs.process_obs_space(**kwargs)
            return result

        # Process NESDIS_JPSSRR
        if provider == "nesdis_jpssrr":
            platform = obs_space.split("_")[2]
            instrument = None
            kwargs = {
                'provider': "jpssrr",
                'obs_space': obs_space,
                'instrument': instrument,
                'platform': platform,
                'obs_type': "",
                'output_file': output_file,
                'window_begin': self.task_config.window_begin,
                'window_end': self.task_config.window_end,
                'task_config': self.task_config
            }
            result = self.nesdis_jpssrr.process_obs_space(**kwargs)
            return result

        # Process SMAP
        if provider == "smap":
            platform = None
            satellite = "SMAP"
            instrument = None
            kwargs = {
                'provider': provider,
                'obs_space': obs_space,
                'platform': platform,
                'instrument': instrument,
                'satellite': satellite,
                'obs_type': obs_space,
                'output_file': output_file,
                'window_begin': self.task_config.window_begin,
                'window_end': self.task_config.window_end,
                'task_config': self.task_config
            }
            result = self.smap.process_obs_space(**kwargs)
            return result

        # Process SMOS SSS
        if provider == "smos":
            platform = None
            satellite = "SMOS"
            instrument = None
            kwargs = {
                'provider': provider,
                'obs_space': obs_space,
                'platform': platform,
                'instrument': instrument,
                'satellite': satellite,
                'obs_type': obs_space,
                'output_file': output_file,
                'window_begin': self.task_config.window_begin,
                'window_end': self.task_config.window_end,
                'task_config': self.task_config
            }
            result = self.smos.process_obs_space(**kwargs)
            return result
        else:
            logger.error(f"Provider {provider} not supported")

    @logit(logger)
    def finalize(self) -> None:
        """
        """
        # Copy the processed ioda files to the destination directory
        logger.info("Copying ioda files to COMOUT_OBS directory")
        comout = self.task_config['COMOUT_OBS']

        # Loop through the observation types
        obs_types = ['sst', 'adt', 'icec', 'sss']
        src_dst_obs_list = []  # list of [src_file, dst_file]
        for obs_type in obs_types:
            # Glob the ioda files
            ioda_files = glob.glob(join(self.task_config['DATA'],
                                        f"{self.task_config['PREFIX']}*{obs_type}_*.nc"))
            for ioda_file in ioda_files:
                src_file = ioda_file
                dst_file = join(comout, basename(ioda_file))
                src_dst_obs_list.append([src_file, dst_file])

        FileHandler({'copy': src_dst_obs_list}).sync()

        # TODO add call to monitoring script for SDM monitoring
