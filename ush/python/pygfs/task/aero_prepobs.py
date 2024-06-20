#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
import re
from logging import getLogger
from typing import List, Dict, Any, Union

from wxflow import (AttrDict, FileHandler, rm_p, rmdir,
                    Task, add_to_datetime, to_timedelta, to_datetime,
                    datetime_to_YMD,
                    chdir, Executable, WorkflowException,
                    parse_j2yaml, save_as_yaml, logit)

logger = getLogger(__name__.split('.')[-1])


class AerosolObsPrep(Task):
    """
    Class for preparing and managing aerosol observations
    """
    def __init__(self, config: Dict[str, Any]) -> None:
        super().__init__(config)

        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config['assim_freq']}H") / 2)
        _window_end = add_to_datetime(self.task_config.current_cycle, +to_timedelta(f"{self.task_config['assim_freq']}H") / 2)

        local_dict = AttrDict(
            {
                'window_begin': _window_begin,
                'window_end': _window_end,
                'sensors': str(self.task_config['SENSORS']).split(','),
                'data_dir': self.task_config['VIIRS_DATA_DIR'],
                'input_files': '',
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z."
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def initialize(self) -> None:
        """
        List needed raw obs files.
        Copy the raw obs files to $DATA/obs.
        Link over the needed executable.
        Generate corresponding YAML file.
        Run IODA converter.
        """
        self.task_config.DATA_OBS = os.path.join(self.task_config.DATA, 'obs')
        if os.path.exists(self.task_config.DATA_OBS):
            rmdir(self.task_config.DATA_OBS)
        FileHandler({'mkdir': [self.task_config.DATA_OBS]}).sync()

        self.task_config.prepaero_yaml = []
        for sensor in self.task_config.sensors:
            raw_files = self.list_raw_files(sensor)
            self.task_config.input_files = self.copy_obs(raw_files)
            self.link_obsconvexe()
            self.task_config.prepaero_config = self.get_obsproc_config(sensor)

            # generate converter YAML file
            template = f"{self.task_config.RUN}.t{self.task_config['cyc']:02d}z.prepaero_viirs_{sensor}.yaml"
            _prepaero_yaml = os.path.join(self.task_config.DATA, template)
            self.task_config.prepaero_yaml.append(_prepaero_yaml)
            logger.debug(f"Generate PrepAeroObs YAML file: {_prepaero_yaml}")
            save_as_yaml(self.task_config.prepaero_config, _prepaero_yaml)
            logger.info(f"Wrote PrepAeroObs YAML to: {_prepaero_yaml}")

    @logit(logger)
    def list_raw_files(self, sensor) -> List[str]:
        """
        List all files in the predefined directory that match the predefined sensor and within the time window.
        """
        if sensor == 'n20':
            sensor = 'j01'
        dir1 = os.path.join(self.task_config.data_dir, datetime_to_YMD(self.task_config.window_begin))
        dir2 = os.path.join(self.task_config.data_dir, datetime_to_YMD(self.task_config.window_end))

        if dir1 == dir2:
            files = os.listdir(dir1)
            allfiles = [os.path.join(dir1, file) for file in files]
            allfiles.sort()
        else:
            files_1 = os.listdir(dir1)
            allfiles_1 = [os.path.join(dir1, file) for file in files_1]
            files_2 = os.listdir(dir2)
            allfiles_2 = [os.path.join(dir2, file) for file in files_2]
            allfiles = sorted(allfiles_1, allfiles_2)
        matching_files = []
        try:
            for file in allfiles:
                basename = os.path.basename(file)
                pattern = r"s(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{3})"
                match = re.match(pattern, basename.split('_')[3])
                yyyy, mm, dd, HH, MM = match.group(1), match.group(2), match.group(3), match.group(4), match.group(5)
                fstart = to_datetime(f'{yyyy}-{mm}-{dd}T{HH}:{MM}Z')
                if sensor == basename.split('_')[2]:
                    # temporally select obs files based on time stamp in the filename.
                    if (fstart > self.task_config.window_begin) and (fstart < self.task_config.window_end):
                        matching_files.append(os.path.join(self.task_config.data_dir, file))
            logger.info("Found %d matching files.", len(matching_files))
        except FileNotFoundError:
            logger.error("The specified file/directory does not exist.")
            raise
        return matching_files

    @logit(logger)
    def copy_obs(self, inputfiles) -> Dict[str, Any]:
        """
        Copy the raw obs files to $DATA/obs.
        """
        copylist = []
        destlist = []
        for filename in inputfiles:
            basename = os.path.basename(filename)
            dest = os.path.join(self.task_config.DATA_OBS, basename)
            copylist.append([filename, dest])
            destlist.append(dest)
        FileHandler({'copy': copylist}).sync()

        return destlist

    @logit(logger)
    def get_obsproc_config(self, sensor) -> Dict[str, Any]:
        """
        Compile a dictionary of obs proc configuration from OBSPROCYAML template file
        Parameters
        ----------
        Returns
        ----------
        obsproc_config : Dict
            a dictionary containing the fully rendered obs proc yaml configuration
        """
        self.task_config.sensor = sensor
        # generate JEDI YAML file
        logger.info(f"Generate gdas_obsprovider2ioda YAML config: {self.task_config.OBSPROCYAML}")
        prepaero_config = parse_j2yaml(self.task_config.OBSPROCYAML, self.task_config)

        return prepaero_config

    @logit(logger)
    def link_obsconvexe(self) -> None:
        """
        This method links the gdas executable to the run directory
        Parameters
        ----------
        Task: GDAS task
        Returns
        ----------
        None
        """
        exe_src = self.task_config.OBSPROCEXE

        logger.info(f"Link executable {exe_src} to DATA/")
        exe_dest = os.path.join(self.task_config.DATA, os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        return

    @logit(logger)
    def runConverter(self) -> None:
        """
        Run the IODA converter gdas_obsprovider2ioda.x
        """
        chdir(self.task_config.DATA)
        exec_cmd = Executable(self.task_config.APRUN_PREPOBSAERO)
        exec_name = os.path.join(self.task_config.DATA, 'gdas_obsprovider2ioda.x')
        exec_cmd.add_default_arg(exec_name)

        for prepaero_yaml in self.task_config.prepaero_yaml:
            try:
                logger.debug(f"Executing {exec_cmd} on {prepaero_yaml}")
                exec_cmd(f"{prepaero_yaml}")
            except OSError:
                raise OSError(f"Failed to execute {exec_cmd} on {prepaero_yaml}")
            except Exception:
                raise WorkflowException(f"An error occured during execution of {exec_cmd} on {prepaero_yaml}")

        pass

    @logit(logger)
    def finalize(self) -> None:
        """
        Copy the output viirs files to COMIN_OBS.
        Tar and archive the output files.
        Tar and archive the raw obs files.
        """
        # get list of viirs files
        obsfiles = glob.glob(os.path.join(self.task_config['DATA'], '*viirs*nc4'))
        copylist = []
        for obsfile in obsfiles:
            basename = os.path.basename(obsfile)
            src = os.path.join(self.task_config['DATA'], basename)
            dest = os.path.join(self.task_config.COMOUT_OBS, basename)
            copylist.append([src, dest])
        FileHandler({'copy': copylist}).sync()

        # gzip the files first
        for obsfile in obsfiles:
            with open(obsfile, 'rb') as f_in, gzip.open(f"{obsfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        aeroobs = os.path.join(self.task_config.COMOUT_OBS, f"{self.task_config['APREFIX']}aeroobs")
        # open tar file for writing
        with tarfile.open(aeroobs, "w") as archive:
            for obsfile in obsfiles:
                aeroobsgzip = f"{obsfile}.gz"
                archive.add(aeroobsgzip, arcname=os.path.basename(aeroobsgzip))
        # get list of raw viirs L2 files
        rawfiles = glob.glob(os.path.join(self.task_config.DATA_OBS, 'JRR-AOD*'))
        # gzip the raw L2 files first
        for rawfile in rawfiles:
            with open(rawfile, 'rb') as f_in, gzip.open(f"{rawfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        aerorawobs = os.path.join(self.task_config.COMOUT_OBS, f"{self.task_config['APREFIX']}aerorawobs")
        # open tar file for writing
        with tarfile.open(aerorawobs, "w") as archive:
            for rawfile in rawfiles:
                aerorawobsgzip = f"{rawfile}.gz"
                archive.add(aerorawobsgzip, arcname=os.path.basename(aerorawobsgzip))
        copylist = []
        for prepaero_yaml in self.task_config.prepaero_yaml:
            basename = os.path.basename(prepaero_yaml)
            dest = os.path.join(self.task_config.COMOUT_OBS, basename)
            copylist.append([prepaero_yaml, dest])
        FileHandler({'copy': copylist}).sync()

        pass
