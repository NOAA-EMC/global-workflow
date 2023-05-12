#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Dict, List
from pprint import pformat

from pygw.attrdict import AttrDict
from pygw.file_utils import FileHandler
from pygw.timetools import to_fv3time, to_YMD, to_YMDH
from pygw.fsutils import rm_p
from pygw.yaml_file import parse_j2yaml
from pygw.jinja import Jinja
from pygw.logger import logit
from pygw.executable import Executable
from pygw.exceptions import WorkflowException
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class LandAnalysis(Analysis):
    """
    Class for global land analysis tasks
    """
    @logit(logger, name="LandAnalysis")
    def __init__(self, config):
        super().__init__(config)

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'OPREFIX': f"{self.runtime_config.RUN}.t{self.runtime_config.cyc:02d}z.",
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.config, **self.runtime_config, **local_dict)

    @logit(logger)
    def prepare_IMS(self: Analysis) -> None:
        """Prepare the IMS data for a global land analysis

        This method will prepare IMS data for a global land analysis using JEDI.
        This includes:
        - staging model backgrounds
        - generating a card file for IMS processing
        - creating IMS snowdepth data in IODA format.

        Parameters
        ----------
        Analysis: parent class for GDAS task

        Returns
        ----------
        None
        """

        # create a temporary dict of all keys needed in this method
        cfg = AttrDict()
        keys = ['DATA', 'current_cycle', 'COM_OBS', 'COM_ATMOS_RESTART_PREV',
                'OPREFIX', 'CASE', 'ntiles']
        for key in keys:
            cfg[key] = self.task_config[key]

        # stage backgrounds
        logger.info("Staging backgrounds")
        FileHandler(self.get_bkg_dict(cfg)).sync()

        # Read and render the IMS_OBS_LIST yaml
        logger.info(f"Reading {self.task_config.IMS_OBS_LIST}")
        prep_ims_config = parse_j2yaml(self.task_config.IMS_OBS_LIST, cfg)
        logger.debug(f"{self.task_config.IMS_OBS_LIST}:\n{pformat(prep_ims_config)}")

        # copy the IMS obs files from COM_OBS to DATA/obs
        logger.info("Copying IMS obs for CALCFIMSEXE")
        FileHandler(prep_ims_config.calcfims).sync()

        logger.info("Create namelist for CALCFIMSEXE")
        nml_template = self.task_config.FIMS_NML_TMPL
        nml_data = Jinja(nml_template, cfg).render
        logger.debug(f"fims.nml:\n{nml_data}")

        nml_file = os.path.join(self.task_config.DATA, "fims.nml")
        with open(nml_file, "w") as fho:
            fho.write(nml_data)

        logger.info("Link CALCFIMSEXE into DATA/")
        exe_src = self.task_config.CALCFIMSEXE
        exe_dest = os.path.join(self.task_config.DATA, os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # execute CALCFIMSEXE to calculate IMS snowdepth
        exe = Executable(self.task_config.APRUN_CALCFIMS)
        exe.add_default_arg(os.path.join(self.task_config.DATA, os.path.basename(exe_src)))
        try:
            logger.debug(f"Executing {exe}")
            exe()
        except OSError:
            raise OSError(f"Failed to execute {exe}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exe}")

        # Ensure the snow depth IMS file is produced by the above executable
        input_file = f"IMSscf.{to_YMD(self.task_config.PDY)}.{self.task_config.CASE}_oro_data.nc"
        if not os.path.isfile(f"{os.path.join(self.task_config.DATA, input_file)}"):
            logger.exception(f"{self.task_config.CALCFIMSEXE} failed to produce {input_file}")
            raise FileNotFoundError(f"{os.path.join(self.task_config.DATA, input_file)}")

        # Execute imspy to create the IMS obs data in IODA format
        logger.info("Create IMS obs data in IODA format")

        output_file = f"ims_snow_{to_YMDH(self.task_config.current_cycle)}.nc4"
        if os.path.isfile(f"{os.path.join(self.task_config.DATA, output_file)}"):
            rm_p(output_file)

        exe = Executable(self.task_config.IMS2IODACONV)
        exe.add_default_arg(["-i", f"{os.path.join(self.task_config.DATA, input_file)}"])
        exe.add_default_arg(["-o", f"{os.path.join(self.task_config.DATA, output_file)}"])
        try:
            logger.debug(f"Executing {exe}")
            exe()
        except OSError:
            raise OSError(f"Failed to execute {exe}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exe}")

        # Ensure the IODA snow depth IMS file is produced by the IODA converter
        # If so, copy to COM_OBS/
        if not os.path.isfile(f"{os.path.join(self.task_config.DATA, output_file)}"):
            logger.exception(f"{self.task_config.IMS2IODACONV} failed to produce {output_file}")
            raise FileNotFoundError(f"{os.path.join(self.task_config.DATA, output_file)}")
        else:
            logger.info(f"Copy {output_file} to {self.task_config.COM_OBS}")
            FileHandler(prep_ims_config.ims2ioda).sync()

    @logit(logger)
    def get_bkg_dict(self, config: Dict) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method constructs a dictionary of FV3 RESTART files (coupler, sfc_data)
        that are needed for global land DA and returns said dictionary for use by the FileHandler class.

        Parameters
        ----------
        self: Analysis
            Instance of the current object class
        config: Dict
            Dictionary of key-value pairs needed in this method

        Returns
        ----------
        bkg_dict: Dict
            a dictionary containing the list of model background files to copy for FileHandler
        """
        # NOTE for now this is FV3 RESTART files and just assumed to be fh006

        # get FV3 RESTART files, this will be a lot simpler when using history files
        rst_dir = os.path.join(config.COM_ATMOS_RESTART_PREV)  # for now, option later?
        run_dir = os.path.join(config.DATA, 'bkg')

        # Start accumulating list of background files to copy
        bkglist = []

        # land DA needs coupler
        basename = f'{to_fv3time(config.current_cycle)}.coupler.res'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        # land DA only needs sfc_data
        for ftype in ['sfc_data']:
            template = f'{to_fv3time(config.current_cycle)}.{ftype}.tile{{tilenum}}.nc'
            for itile in range(1, config.ntiles + 1):
                basename = template.format(tilenum=itile)
                bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        bkg_dict = {
            'mkdir': [run_dir],
            'copy': bkglist
        }
        return bkg_dict
