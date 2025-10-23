#!/usr/bin/env python3

import os
import glob
import gsincdiag_to_ioda.proc_gsi_ncdiag as gsid
import gzip
import tarfile
from logging import getLogger
from pprint import pformat
from typing import Optional, Dict, Any

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_timedelta,
                    Task,
                    parse_j2yaml,
                    logit)
from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class AnalysisStats(Task):
    """
    Class for JEDI-based global analysis stats tasks
    """
    @logit(logger, name="AnalysisStats")
    def __init__(self, config: Dict[str, Any]):
        """
        Constructor global analysis stats task
        This method will construct a global analysis stats task.
        This includes:
        - extending the task_config attribute AttrDict to include parameters required for this task
        - instantiate the Jedi attribute objects
        Parameters
        ----------
        config: Dict
            dictionary object containing task configuration
        Returns
        ----------
        None
        """
        super().__init__(config)

        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config.assim_freq}H") / 2)

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'STAT_WINDOW_BEGIN': _window_begin,
                'STAT_WINDOW_LENGTH': f"PT{self.task_config.assim_freq}H",
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z."
            }
        )
        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def initialize(self) -> None:
        """
        This method will initialize a global analysis stats task.
        This includes:
        - initialize JEDI applications
        - copying stat files
        Parameters
        ----------
        None
        Returns
        ----------
        None
        """
        # Create dictionary of Jedi objects
        # Expected keys are what must be included from the JEDI config file. We can
        # then loop through ob space list from scripts/exglobal_analysis_stats.py
        expected_keys = ['aero', 'atmos', 'atmos_gsi', 'snow']
        jedi_config_dict = parse_j2yaml(self.task_config.JEDI_CONFIG_YAML, self.task_config)
        self.jedi_dict = Jedi.get_jedi_dict(jedi_config_dict, self.task_config, expected_keys)

        logger.info(f"Copying files to {self.task_config.DATA}/stats")

        # Extract info from stat config file
        analysis_config_dict = parse_j2yaml(self.task_config.BASE_CONFIG_YAML, self.task_config)

        # Loop through a copy of ob space list
        for analysis in self.task_config.STAT_ANALYSES[:]:
            logger.info(f"Working on analysis type: {analysis}")

            # Copy stat files to DATA path
            input_tar = os.path.join(analysis_config_dict[analysis]['stat_file_path'],
                                     f"{self.task_config['APREFIX']}{analysis_config_dict[analysis]['stat_file_name']}")
            diag_dir_path = os.path.join(self.task_config.DATA, analysis)

            dest = os.path.join(diag_dir_path, analysis_config_dict[analysis]['stat_file_name'])
            logger.info(f"Copying {input_tar} to {dest} ...")
            tarball_list = [[input_tar, dest]]
            FileHandler({'mkdir': [diag_dir_path], 'copy': tarball_list}).sync()

            # Open tar file
            logger.info(f"Open tarred diagnostic files in {dest}")
            with tarfile.open(dest, "r") as tar:
                # Check if tar file is empty
                if not tar.getnames():
                    logger.warning(f"WARNING. The tar file {dest} is empty. No files to extract.")
                    logger.warning("Moving to next analysis ...")
                    # Remove analysis from STAT_ANALYSES and move to next
                    self.task_config.STAT_ANALYSES.remove(analysis)
                    logger.info(f"current analysis list: {self.task_config.STAT_ANALYSES}")
                    continue
                # Extract all files to the current directory
                tar.extractall(path=diag_dir_path)

            self.task_config.OBSSPACES_LIST = []
            for analysis_dict in analysis_config_dict[analysis]['obs spaces']:
                # Gunzip .nc files
                gz_file = os.path.join(diag_dir_path, (analysis_dict['input file'] + ".gz"))

                # Check if the file exists
                if os.path.exists(gz_file):
                    logger.info(f"Now processing {gz_file}")
                    output_file = os.path.join(diag_dir_path, analysis_dict['input file'])
                    # Open the .gz file
                    with gzip.open(gz_file, 'rb') as f_in:
                        with open(output_file, 'wb') as f_out:
                            f_out.write(f_in.read())
                else:
                    logger.warning(f"WARNING. {gz_file} does not exist to extract.")
                    logger.warning("Moving to next analysis ...")
                    continue  # Skip current analysis and move to next

                self.task_config.OBSSPACES_LIST.append(analysis_dict['name'])

            # initialize JEDI application
            logger.info(f"Initializing JEDI ioda-stats extraction application")
            self.jedi_dict[analysis].initialize(self.task_config)

    @logit(logger)
    def execute(self, jedi_dict_key: str) -> None:
        """Execute JEDI application of analysis stats

        Parameters
        ----------
        jedi_dict_key
            key specifying particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        self.jedi_dict[jedi_dict_key].execute()

    @logit(logger)
    def finalize(self, jedi_dict_key: str) -> None:
        """Finalize the analysis statistics job.

        This method will finalize the analysis statistics job using JEDI.
        This includes:
        - copying stat files to specified outdir
        - tar and gzip stat files

        Parameters
        ----------
        jedi_dict_key
            key specifying particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        analysis_config_dict = parse_j2yaml(self.task_config.BASE_CONFIG_YAML, self.task_config)

        for analysis_dict in analysis_config_dict[jedi_dict_key]['obs spaces']:
            statfile = os.path.join(self.task_config.DATA, analysis_dict['output file'])
            if jedi_dict_key == 'atmos_gsi':
                outdir = self.task_config['COMOUT_ATMOS_ANLMON']
            else:
                outdir = self.task_config['COMOUT_' + jedi_dict_key.upper() + '_ANLMON']

            # Check if the directory exists; if not, create it
            if not os.path.exists(outdir):
                FileHandler({'mkdir': [outdir]}).sync()

            dest = os.path.join(outdir, f"{analysis_dict['output file']}")
            logger.debug(f"copying {statfile} to {dest}")
            stat_copy = {
                'copy_opt': [[statfile, dest]]
            }
            FileHandler(stat_copy).sync()

        # path of output tar statfile
        iodastatzipfile = os.path.join(outdir, f"{self.task_config.APREFIX}{jedi_dict_key}_iodastat.tgz")

        logger.info(f"Compressing ioda-stats generated files to {iodastatzipfile}")

        # get list of iodastat files to put in tarball
        iodastatfiles = glob.glob(os.path.join(outdir, '*output*nc'))

        logger.info(f"Gathering {len(iodastatfiles)} ioda-stat files to {iodastatzipfile}")

        with tarfile.open(iodastatzipfile, "w|gz") as archive:
            for targetfile in iodastatfiles:
                archive.add(targetfile, arcname=os.path.basename(targetfile))

    @logit(logger)
    def convert_gsi_diags(self) -> None:
        """Convert GSI diag files to ioda-stat files for analysis stats

        This method will convert GSI diag files to ioda-stat files for analysis stats.
        This includes:
        - copying GSI diag files to DATA path
        - untarring and gunzipping GSI diag files
        - converting GSI diag files to ioda files using gsincdiag2ioda converter scripts

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """
        logger.info("Converting GSI diag files to IODA files for analysis stats")
        # copy GSI diag files to DATA path
        diag_tars = ['cnvstat', 'radstat', 'oznstat']
        diag_dir_ges_path = os.path.join(self.task_config.DATA, 'atmos_gsi_ges')
        diag_dir_anl_path = os.path.join(self.task_config.DATA, 'atmos_gsi_anl')
        diag_dir_path = os.path.join(self.task_config.DATA, 'atmos_gsi_diags')
        FileHandler({'mkdir': [diag_dir_path, diag_dir_ges_path, diag_dir_anl_path]}).sync()
        diag_ioda_dir_ges_path = os.path.join(self.task_config.DATA, 'atmos_gsi_ioda_ges')
        diag_ioda_dir_anl_path = os.path.join(self.task_config.DATA, 'atmos_gsi_ioda_anl')
        output_dir_path = os.path.join(self.task_config.DATA, 'atmos_gsi_ioda')
        FileHandler({'mkdir': [diag_ioda_dir_ges_path, diag_ioda_dir_anl_path, output_dir_path]}).sync()
        diag_tar_copy_list = []
        for diag in diag_tars:
            input_tar_basename = f"{self.task_config.APREFIX}{diag}"
            input_tar = os.path.join(self.task_config.COMIN_ATMOS_ANALYSIS,
                                     input_tar_basename)
            dest = os.path.join(diag_dir_path, input_tar_basename)
            if os.path.exists(input_tar):
                diag_tar_copy_list.append([input_tar, dest])
        FileHandler({'copy_opt': diag_tar_copy_list}).sync()

        # Untar and gunzip diag files
        gsi_diag_tars = glob.glob(os.path.join(diag_dir_path, f"{self.task_config.APREFIX}*stat"))
        for diag_tar in gsi_diag_tars:
            logger.info(f"Untarring {diag_tar}")
            with tarfile.open(diag_tar, "r") as tar:
                tar.extractall(path=diag_dir_path)
        gsi_diags = glob.glob(os.path.join(diag_dir_path, "diag_*.nc4.gz"))
        for diag in gsi_diags:
            logger.info(f"Gunzipping {diag}")
            output_file = diag.rstrip('.gz')
            with gzip.open(diag, 'rb') as f_in:
                with open(output_file, 'wb') as f_out:
                    f_out.write(f_in.read())
            os.remove(diag)

        # Copy diag files to ges or anl directory
        anl_diags = glob.glob(os.path.join(diag_dir_path, "diag_*_anl*.nc4"))
        ges_diags = glob.glob(os.path.join(diag_dir_path, "diag_*_ges*.nc4"))
        copy_anl_diags = []
        for diag in anl_diags:
            copy_anl_diags.append([diag, os.path.join(diag_dir_anl_path, os.path.basename(diag))])
        FileHandler({'copy_opt': copy_anl_diags}).sync()
        copy_ges_diags = []
        for diag in ges_diags:
            copy_ges_diags.append([diag, os.path.join(diag_dir_ges_path, os.path.basename(diag))])
        FileHandler({'copy_opt': copy_ges_diags}).sync()

        # Convert GSI diag files to ioda files using gsincdiag2ioda converter scripts
        gsid.proc_gsi_ncdiag(ObsDir=diag_ioda_dir_ges_path, DiagDir=diag_dir_ges_path)
        gsid.proc_gsi_ncdiag(ObsDir=diag_ioda_dir_anl_path, DiagDir=diag_dir_anl_path)

        # now we need to combine the two sets of ioda files into one file
        # by adding certain groups from the anl file to the ges file
        ges_ioda_files = glob.glob(os.path.join(diag_ioda_dir_ges_path, '*nc4'))
        for ges_ioda_file in ges_ioda_files:
            anl_ioda_file = ges_ioda_file.replace('_ges_', '_anl_').replace(diag_ioda_dir_ges_path, diag_ioda_dir_anl_path)
            if os.path.exists(anl_ioda_file):
                logger.info(f"Combining {ges_ioda_file} and {anl_ioda_file}")
                out_ioda_file = os.path.join(output_dir_path, os.path.basename(ges_ioda_file).replace('_ges_', '_gsi_'))
                gsid.combine_ges_anl_ioda(ges_ioda_file, anl_ioda_file, out_ioda_file)
            else:
                logger.warning(f"WARNING: {anl_ioda_file} does not exist to combine with {ges_ioda_file}")
                logger.warning("Skipping this file ...")

        # Tar up the ioda files
        iodastatzipfile = os.path.join(self.task_config.DATA, 'atmos_gsi_ioda',
                                       f"{self.task_config.APREFIX}atmos_gsi_ioda_diags.tgz")
        logger.info(f"Compressing GSI IODA files to {iodastatzipfile}")
        # get list of iodastat files to put in tarball
        iodastatfiles = glob.glob(os.path.join(output_dir_path, '*nc4'))
        logger.info(f"Gathering {len(iodastatfiles)} GSI IODA files to {iodastatzipfile}")
        with tarfile.open(iodastatzipfile, "w|gz") as archive:
            for targetfile in iodastatfiles:
                # gzip the file before adding to tar
                with open(targetfile, 'rb') as f_in:
                    with gzip.open(f"{targetfile}.gz", 'wb') as f_out:
                        f_out.writelines(f_in)
                os.remove(targetfile)
                targetfile = f"{targetfile}.gz"
                archive.add(targetfile, arcname=os.path.basename(targetfile))
        logger.info(f"Finished compressing GSI IODA files to {iodastatzipfile}")
        # copy to COMOUT
        outdir = self.task_config.COMOUT_ATMOS_ANLMON
        if not os.path.exists(outdir):
            FileHandler({'mkdir': [outdir]}).sync()
        dest = os.path.join(outdir, os.path.basename(iodastatzipfile))
        logger.info(f"Copying {iodastatzipfile} to {dest}")
        FileHandler({'copy_opt': [[iodastatzipfile, dest]]}).sync()
        logger.info("Finished copying GSI IODA tar file to COMOUT")
