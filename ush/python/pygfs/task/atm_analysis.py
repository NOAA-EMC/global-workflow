#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from logging import getLogger
from pprint import pformat
from typing import Any, Dict
from wxflow import (AttrDict, FileHandler,
                    add_to_datetime, to_timedelta,
                    parse_j2yaml,
                    logit)
from pygfs.task.atm_analysis import AtmAnalysis
from pygfs.jedi import Jedi

logger = getLogger(__name__.split('.')[-1])


class AtmDetAnalysis(AtmAnalysis):
    """
    Class for JEDI-based global atm deterministic analysis tasks
    """
    @logit(logger, name="AtmDetAnalysis")
    def __init__(self, config: Dict[str, Any]):
        """Constructor global atm analysis task

        This method will construct a global atm analysis task.
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

        _localization_type = 'bump'
        if self.task_config.DOHYBVAR:
            _BERROR_YAML="atmosphere_background_error_hybrid_${self.task_config.STATICB_TYPE}_${_localization_type}"
        else:
            _BERROR_YAML="atmosphere_background_error_static_${self.task_config.STATICB_TYPE}"

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'BERROR_YAML': _BERROR_YAML,
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

        # Create dictionary of Jedi objects
        expected_keys = ['atmanlvar', 'atmanlfv3inc']
        self.jedi_dict = Jedi.get_jedi_dict(self.task_config.JEDI_CONFIG_YAML, self.task_config, expected_keys)

    @logit(logger)
    def initialize(self) -> None:
        """Initialize a global atm analysis

        This method will initialize a global atm analysis.
        This includes:
        - initialize JEDI applications
        - staging observation files
        - staging bias correction files
        - staging CRTM fix files
        - staging FV3-JEDI fix files
        - staging B error files
        - staging model backgrounds
        - creating output directories

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # Stage files from COM
        logger.info(f"Staging files from COM")
        FileHandler(self.task_config.stage).sync()

        # Extract bias corrections from tar files
        logger.info(f"Extracting bias corrections from tar files")
        if bias_dict['copy'] is None:
            logger.info(f"No bias correction files to stage")
        else:
            bias_dict['copy'] = Jedi.remove_redundant(bias_dict['copy'])
            FileHandler(bias_dict).sync()
            logger.debug(f"Bias correction files:\n{pformat(bias_dict)}")

            # extract bias corrections
            Jedi.extract_tar_from_filehandler_dict(bias_dict)

        # initialize JEDI variational application
        logger.info(f"Initializing JEDI variational DA application")
        self.jedi_dict['atmanlvar'].initialize(self.task_config, clean_empty_obsspaces=True)

        # initialize JEDI FV3 increment conversion application
        logger.info(f"Initializing JEDI FV3 increment conversion application")
        self.jedi_dict['atmanlfv3inc'].initialize(self.task_config)

    @logit(logger)
    def execute(self, jedi_dict_key: str) -> None:
        """Execute JEDI application of atm analysis

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
    def finalize(self) -> None:
        """Finalize a global atm analysis

        This method will finalize a global atm analysis using JEDI.
        This includes:
        - tar output diag files and place in ROTDIR
        - copy the generated YAML file from initialize to the ROTDIR
        - copy the updated bias correction files to ROTDIR

        Parameters
        ----------
        None

        Returns
        ----------
        None
        """

        # ---- tar up diags
        # path of output tar statfile
        atmstat = os.path.join(self.task_config.COMOUT_ATMOS_ANALYSIS, f"{self.task_config.APREFIX}atmstat")

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.task_config.DATA, 'diags', 'diag*nc'))

        logger.info(f"Compressing {len(diags)} diag files to {atmstat}.gz")

        # gzip the files first
        logger.debug(f"Gzipping {len(diags)} diag files")
        for diagfile in diags:
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # open tar file for writing
        logger.debug(f"Creating tar file {atmstat} with {len(diags)} gzipped diag files")
        with tarfile.open(atmstat, "w") as archive:
            for diagfile in diags:
                diaggzip = f"{diagfile}.gz"
                archive.add(diaggzip, arcname=os.path.basename(diaggzip))

        # get list of yamls to copy to ROTDIR
#        yamls = glob.glob(os.path.join(self.task_config.DATA, '*atm*yaml'))

        # copy full YAML from executable to ROTDIR
#        for src in yamls:
#            yaml_base = os.path.splitext(os.path.basename(src))[0]
#            dest_yaml_name = f"{self.task_config.APREFIX}{yaml_base}.yaml"
#            dest = os.path.join(self.task_config.COMOUT_CONF, dest_yaml_name)
#            logger.debug(f"Copying {src} to {dest}")
#            yaml_copy = {
#                'copy': [[src, dest]]
#            }
#            FileHandler(yaml_copy).sync()

        # path of output radiance bias correction tarfile
        bfile = f"{self.task_config.APREFIX}rad_varbc_params.tar"
        radtar = os.path.join(self.task_config.COMOUT_ATMOS_ANALYSIS, bfile)

        # rename and copy tlapse radiance bias correction files from obs to bc
        tlapobs = glob.glob(os.path.join(self.task_config.DATA, 'obs', '*tlapse.txt'))
        copylist = []
        for tlapfile in tlapobs:
            obsfile = os.path.basename(tlapfile).split('.', 2)
            newfile = f"{self.task_config.APREFIX}{obsfile[2]}"
            copylist.append([tlapfile, os.path.join(self.task_config.DATA, 'bc', newfile)])
        tlapse_dict = {
            'copy': copylist
        }
        FileHandler(tlapse_dict).sync()

        # get lists of radiance bias correction files to add to tarball
        satlist = glob.glob(os.path.join(self.task_config.DATA, 'bc', '*satbias*nc'))
        tlaplist = glob.glob(os.path.join(self.task_config.DATA, 'bc', '*tlapse.txt'))

        # tar radiance bias correction files to ROTDIR
        logger.info(f"Creating radiance bias correction tar file {radtar}")
        with tarfile.open(radtar, 'w') as radbcor:
            for satfile in satlist:
                radbcor.add(satfile, arcname=os.path.basename(satfile))
            for tlapfile in tlaplist:
                radbcor.add(tlapfile, arcname=os.path.basename(tlapfile))
            logger.info(f"Add {radbcor.getnames()}")

        # Copy FV3 atm increment to comrot directory
#        logger.info("Copy UFS model readable atm increment file")
#        inc_copy = {'copy': []}
#        for itile in range(6):
#            src = os.path.join(self.task_config.DATA, "anl",
#                               f"{self.task_config.APREFIX}cubed_sphere_grid_atminc.tile{itile+1}.nc")
#            dest = self.task_config.COMOUT_ATMOS_ANALYSIS
#            inc_copy['copy'].append([src, dest])

#        # copy increments
#        src_list, dest_list = zip(*inc_copy['copy'])
#        logger.debug(f"Copying {src_list}\nto {dest_list}")
#        FileHandler(inc_copy).sync()

    def clean(self):
        super().clean()
