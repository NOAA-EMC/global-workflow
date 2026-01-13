#!/usr/bin/env python3

import os
import glob
import gsincdiag_to_ioda.proc_gsi_ncdiag as gsid
import gsincdiag_to_ioda.combine_obsspace as gsios
import gzip
import tarfile
from logging import getLogger
from pprint import pformat
from typing import Optional, Dict, Any

from wxflow import (AttrDict,
                    FileHandler,
                    parse_j2yaml,
                    logit)
from pygfs.jedi import Jedi
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class AnalysisStats(Analysis):
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
        analysis: str
            type of analysis stats to be performed
        Returns
        ----------
        None
        """
        super().__init__(config)

        _outdir = {
            'atmos': self.task_config.COMOUT_ATMOS_ANLMON,
            'atmos_gsi': self.task_config.COMOUT_ATMOS_ANLMON,
        }
        _anldir = {
            'atmos': self.task_config.COMOUT_ATMOS_ANALYSIS,
            'atmos_gsi': self.task_config.COMOUT_ATMOS_ANALYSIS,
        }
        if self.task_config.DO_AERO_ANL:
            _outdir['aero'] = self.task_config.COMOUT_AERO_ANLMON
            _anldir['aero'] = self.task_config.COMOUT_AERO_ANALYSIS
        if self.task_config.DO_JEDISNOWDA:
            _outdir['snow'] = self.task_config.COMOUT_SNOW_ANLMON
            _anldir['snow'] = self.task_config.COMOUT_SNOW_ANALYSIS

        # Create a local dictionary that is repeatedly used across this class
        self.task_config.update(AttrDict(
            {
                #
                'outdir': _outdir,
                'anldir': _anldir,
            }
        ))

        # Extend task_config with content of config yaml for this task
        self.task_config.update(parse_j2yaml(self.task_config.TASK_CONFIG_YAML, self.task_config))

        # Create dictionary of Jedi objects
        expected_keys = self.task_config.STAT_ANALYSES
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.jedi_config, self.task_config, expected_keys)

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

        for analysis in self.task_config.STAT_ANALYSES:
            # Loop through a copy of ob space list
            logger.info(f"Working on analysis type: {analysis}")

            # Stage files from COM
            logger.info(f"Staging files from COM and creating output directories")
            FileHandler(self.task_config.data_in).sync()

            # Extract diag tar file
            jcb_config = self.jedi_dict[analysis].jcb_config
            component = self.jedi_dict[analysis].component
            diag_archive = os.path.join(jcb_config[f"{component}_obsdatain_path"],
                                        f"{self.task_config.APREFIX}{analysis}_analysis.ioda_hofx.tar.gz")
            Jedi.extract_tar(diag_archive)

            # Initialize JEDI application
            logger.info(f"Initializing JEDI ioda-stats extraction application")
            self.jedi_dict[analysis].initialize(clean_empty_obsspaces=True)

    @logit(logger)
    def execute(self, analysis: str) -> None:
        """Execute JEDI application of analysis stats

        Parameters
        ----------
        analysis
            key specifying particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        self.jedi_dict[analysis].execute()

    @logit(logger)
    def finalize(self, analysis: str) -> None:
        """Finalize the analysis statistics job.

        This method will finalize the analysis statistics job using JEDI.
        This includes:
        - copying stat files to specified outdir
        - tar and gzip stat files

        Parameters
        ----------
        analysis
            key specifying particular Jedi object in self.jedi_dict

        Returns
        ----------
        None
        """

        for analysis in self.task_config.STAT_ANALYSES:
            self.jedi_dict[analysis].save_obsdataout(self.task_config.outdir[analysis],
                                                     f"{self.task_config.APREFIX}{analysis}_analysis.ioda_hofx_stats")

            # concatenate text files into one summary file
            jcb_config = self.jedi_dict[analysis].jcb_config
            component = self.jedi_dict[analysis].component
            summaryfile = os.path.join(jcb_config[f"{component}_obsdataout_path"], f"{self.task_config.APREFIX}{analysis}_stats.txt")
            with open(summaryfile, 'w') as outfile:
                for ob in self.jedi_dict[analysis].jcb_config.observations:
                    textfile = os.path.join(jcb_config[f"{component}_obsdataout_path"], f"{ob}_ioda_stats.txt")
                    if os.path.exists(textfile):
                        logger.info(f"Concatenating {textfile} to {summaryfile}")
                        with open(textfile, 'r') as infile:
                            outfile.write(infile.read())
                    else:
                        logger.warning(f"{textfile} does not exist to concatenate.")
                        logger.warning("Skipping this file ...")

        # Save files from COM
        logger.info(f"Saving files to COM")
        FileHandler(self.task_config.data_out).sync()

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
        diag_dir_ges_path = os.path.join(self.task_config.DATA, 'atmos_gsi', 'atmos_gsi_ges')
        diag_dir_anl_path = os.path.join(self.task_config.DATA, 'atmos_gsi', 'atmos_gsi_anl')
        diag_dir_path = os.path.join(self.task_config.DATA, 'atmos_gsi', 'atmos_gsi_diags')
        FileHandler({'mkdir': [diag_dir_path, diag_dir_ges_path, diag_dir_anl_path]}).sync()
        diag_ioda_dir_ges_path = os.path.join(self.task_config.DATA, 'atmos_gsi', 'atmos_gsi_ioda_ges')
        diag_ioda_dir_anl_path = os.path.join(self.task_config.DATA, 'atmos_gsi', 'atmos_gsi_ioda_anl')
        output_dir_path = os.path.join(self.task_config.DATA, 'atmos_gsi', 'atmos_gsi_ioda')
        FileHandler({'mkdir': [diag_ioda_dir_ges_path, diag_ioda_dir_anl_path, output_dir_path]}).sync()
        diag_tar_copy_list = []
        for diag in diag_tars:
            input_tar_basename = f"{self.task_config.APREFIX}{diag}.tar"
            input_tar = os.path.join(self.task_config.COMIN_ATMOS_ANALYSIS,
                                     input_tar_basename)
            dest = os.path.join(diag_dir_path, input_tar_basename)
            if os.path.exists(input_tar):
                logger.info(f"{input_tar} exists. Preparing to copy it to {dest}")
                diag_tar_copy_list.append([input_tar, dest])
            else:
                logger.warning(f"{input_tar} does not exist to copy. Skipping ...")
        FileHandler({'copy_opt': diag_tar_copy_list}).sync()

        # Untar and gunzip diag files
        gsi_diag_tars = glob.glob(os.path.join(diag_dir_path, f"{self.task_config.APREFIX}*stat.tar"))
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
        logger.info("Converting GSI guess diag files to IODA files")
        gsid.proc_gsi_ncdiag(ObsDir=diag_ioda_dir_ges_path, DiagDir=diag_dir_ges_path)
        logger.info("Converting GSI analysis diag files to IODA files")
        gsid.proc_gsi_ncdiag(ObsDir=diag_ioda_dir_anl_path, DiagDir=diag_dir_anl_path)

        # now we need to combine the two sets of ioda files into one file
        # by adding certain groups from the anl file to the ges file
        ges_ioda_files = glob.glob(os.path.join(diag_ioda_dir_ges_path, '*nc'))
        for ges_ioda_file in ges_ioda_files:
            anl_ioda_file = ges_ioda_file.replace('_ges_', '_anl_').replace(diag_ioda_dir_ges_path, diag_ioda_dir_anl_path)
            if os.path.exists(anl_ioda_file):
                logger.info(f"Combining {ges_ioda_file} and {anl_ioda_file}")
                out_ioda_file = os.path.join(output_dir_path, os.path.basename(ges_ioda_file).replace('_ges_', '_gsi_'))
                gsid.combine_ges_anl_ioda(ges_ioda_file, anl_ioda_file, out_ioda_file)
            else:
                logger.warning(f"{anl_ioda_file} does not exist to combine with {ges_ioda_file}")
                logger.warning("Skipping this file ...")

        # now, for conventional data, we need to combine certain obspaces
        logger.info("Combining conventional GSI IODA files by obspace")
        conv_obsspaces = ['sondes', 'aircraft', 'sfcship', 'sfc']
        for obspace in conv_obsspaces:
            logger.info(f"Combining conventional GSI IODA files for obspace {obspace}")
            FileList = glob.glob(os.path.join(output_dir_path, f"{obspace}_*_gsi_*.nc"))
            timestamp = self.task_config.current_cycle.strftime('%Y%m%d%H')
            combined_outfile = os.path.join(output_dir_path, f"{obspace}_gsi_{timestamp}.nc")
            gsios.combine_obsspace(FileList, combined_outfile, False)

        # Tar up the ioda files
        iodastatzipfile = os.path.join(self.task_config.DATA, 'atmos_gsi', 'atmos_gsi_ioda',
                                       f"{self.task_config.APREFIX}atmos_gsi_analysis.ioda_hofx.tar.gz")
        logger.info(f"Compressing GSI IODA files to {iodastatzipfile}")
        # get list of iodastat files to put in tarball
        iodastatfiles = glob.glob(os.path.join(output_dir_path, '*nc'))
        logger.info(f"Gathering {len(iodastatfiles)} GSI IODA files to {iodastatzipfile}")
        with tarfile.open(iodastatzipfile, "w|gz") as archive:
            for targetfile in iodastatfiles:
                archive.add(targetfile, arcname=os.path.basename(targetfile))
        logger.info(f"Finished compressing GSI IODA files to {iodastatzipfile}")
        # copy to COMOUT
        outdir = self.task_config.COMOUT_ATMOS_ANALYSIS
        if not os.path.exists(outdir):
            FileHandler({'mkdir': [outdir]}).sync()
        dest = os.path.join(outdir, os.path.basename(iodastatzipfile))
        logger.info(f"Copying {iodastatzipfile} to {dest}")
        FileHandler({'copy_opt': [[iodastatzipfile, dest]]}).sync()
        logger.info("Finished copying GSI IODA tar file to COMOUT")
