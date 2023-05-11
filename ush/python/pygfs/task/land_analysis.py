#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Dict, List

from pygw.attrdict import AttrDict
from pygw.file_utils import FileHandler
from pygw.timetools import to_fv3time, to_YMD, to_julian
from pygw.fsutils import rm_p
from pygw.yaml_file import parse_j2yaml
from pygw.logger import logit
from pygw.executable import Executable
from pygw.exceptions import WorkflowException
from pygw.template import Template, TemplateConstants
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
    def prepare(self: Analysis) -> None:
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

        # stage backgrounds
        logger.info("Staging backgrounds")
        FileHandler(self.get_bkg_dict()).sync()

        # copy the IMS obs files from COM_OBS to DATA/obs
        logger.info("Copying IMS obs")
        # create a temporary dict for parsing the IMS_OBS_LIST yaml file
        cfg = AttrDict()
        keys = ['DATA', 'current_cycle', 'COM_OBS', 'OPREFIX', 'CASE']
        for key in keys:
            cfg[key] = self.task_config[key]
        prep_ims_config = parse_j2yaml(self.task_config.IMS_OBS_LIST, cfg)
        FileHandler(prep_ims_config.mkdir).sync()
        FileHandler(prep_ims_config.copy).sync()

        logger.info("Create namelist for calcfIMS.exe")
        cfg = AttrDict()
        cfg.DATA = self.task_config.DATA
        cfg.atm_res = self.task_config.CASE[1:]
        cfg.jdate = to_julian(self.task_config.current_cycle)
        cfg.yyyymmddhh = f"{to_YMD(self.task_config.current_cycle)}.{self.task_config.current_cycle.hour:02d}"

        fims_nml_tmpl = self.task_config.FIMS_NML_TMPL
        fims_nml = os.path.join(self.task_config.DATA, "fims.nml")
        with open(fims_nml_tmpl, "r") as fhi, open(fims_nml, "w") as fho:
            fims_in = fhi.read()
            fims_out = Template.substitute_structure(
                fims_in, TemplateConstants.DOLLAR_PARANTHESES, cfg.get)
            fho.write(fims_out)

        logger.info("Link calcfIMS.exe into DATA/")
        exe_src = self.task_config.CALCFIMSEXE
        exe_dest = os.path.join(self.task_config['DATA'], os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # execute calcfIMS.exe to calculate IMS snowdepth
        exec_cmd = Executable(self.task_config.APRUN_LANDANL)
        exec_name = os.path.join(self.task_config.DATA, 'calcfIMS.exe')
        exec_cmd.add_default_arg(exec_name)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

    @logit(logger)
    def get_bkg_dict(self) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method constructs a dictionary of FV3 RESTART files (coupler, sfc_data)
        that are needed for global land DA and returns said dictionary for use by the FileHandler class.

        Parameters
        ----------
        self: Analysis
            Instance of the current object class

        Returns
        ----------
        bkg_dict: Dict
            a dictionary containing the list of model background files to copy for FileHandler
        """
        # NOTE for now this is FV3 RESTART files and just assumed to be fh006

        # get FV3 RESTART files, this will be a lot simpler when using history files
        rst_dir = os.path.join(self.task_config.COM_ATMOS_RESTART_PREV)  # for now, option later?
        run_dir = os.path.join(self.task_config.DATA, 'bkg')

        # Start accumulating list of background files to copy
        bkglist = []

        # land DA needs coupler
        basename = f'{to_fv3time(self.task_config.current_cycle)}.coupler.res'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        # land DA only needs sfc_data
        for ftype in ['sfc_data']:
            template = f'{to_fv3time(self.task_config.current_cycle)}.{ftype}.tile{{tilenum}}.nc'
            for itile in range(1, self.task_config.ntiles + 1):
                basename = template.format(tilenum=itile)
                bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        bkg_dict = {
            'mkdir': [run_dir],
            'copy': bkglist
        }
        return bkg_dict
