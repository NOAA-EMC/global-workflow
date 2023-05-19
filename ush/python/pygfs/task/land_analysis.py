#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Dict, List
from pprint import pformat
import numpy as np
from netCDF4 import Dataset

from pygw.attrdict import AttrDict
from pygw.file_utils import FileHandler
from pygw.timetools import to_fv3time, to_YMD, to_YMDH
from pygw.fsutils import rm_p
from pygw.yaml_file import parse_j2yaml, parse_yamltmpl, save_as_yaml
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

    NMEM_LANDENS = 2  # The size of the land ensemble is fixed at 2.  Does this need to be a variable?
    BESTDDEV = 30.  # Background Error Std. Dev. for LETKFOI e.g. 30.  This needs to be user configurable

    @logit(logger, name="LandAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _letkfoi_yaml = os.path.join(self.runtime_config.DATA, f"{self.runtime_config.RUN}.t{self.runtime_config['cyc']:02d}z.letkfoi.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'OPREFIX': f"{self.runtime_config.RUN}.t{self.runtime_config.cyc:02d}z.",
                'jedi_yaml': _letkfoi_yaml
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
        - processing raw IMS observation data and prepare for conversion to IODA
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
        logger.info(f"Executing {exe}")
        try:
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
    def initialize(self: Analysis) -> None:

        super().initialize()

        # create a temporary dict of all keys needed in this method
        cfg = AttrDict()
        keys = ['DATA', 'current_cycle', 'COM_OBS', 'COM_ATMOS_RESTART_PREV',
                'OPREFIX', 'CASE', 'ntiles']
        for key in keys:
            cfg[key] = self.task_config[key]

        # Make member directories in DATA for background
        dirlist = []
        for imem in range(1, self.task_config.NMEM_LANDENS + 1):
            dirlist.append(os.path.join(self.task_config.DATA, 'bkg', f'mem{imem:03d}'))
        FileHandler({'mkdir': dirlist}).sync()

        # stage fix files
        jedi_fix_list_path = os.path.join(self.task_config['HOMEgfs'], 'parm', 'parm_gdas', 'land_jedi_fix.yaml')
        logger.info(f"Staging JEDI fix files from {jedi_fix_list_path}")
        jedi_fix_list = parse_yamltmpl(jedi_fix_list_path, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage backgrounds
        logger.info("Staging ensemble backgrounds")
        FileHandler(self.get_ens_bkg_dict(cfg)).sync()

        # generate letkfoi YAML file
        logger.info(f"Generate JEDI LETKF YAML file: {self.task_config.fv3jedi_yaml}")
        letkfoi_yaml = parse_j2yaml(self.task_config.JEDIYAML, self.task_config)
        save_as_yaml(letkfoi_yaml, self.task_config.jedi_yaml)
        logger.info(f"Wrote letkfoi YAML to: {self.task_config.jedi_yaml}")

        # need output dir for diags and anl
        logger.info("Create empty output [anl, diags] directories to receive output from executable")
        newdirs = [
            os.path.join(self.task_config['DATA'], 'anl'),
            os.path.join(self.task_config['DATA'], 'diags'),
        ]
        FileHandler({'mkdir': newdirs}).sync()

    @logit(logger)
    def execute(self: Analysis) -> None:

        # create a temporary dict of all keys needed in this method
        config = AttrDict()
        keys = ['DATA', 'current_cycle',
                'COM_LAND_ANALYSIS', 'APREFIX',
                'FRACGRID', 'CASE', 'ntiles',
                'APPLY_INCR_NML_TMPL', 'APPLY_INCR_EXE', 'APRUN_APPLY_INCR']
        for key in keys:
            config[key] = self.task_config[key]

        logger.info("Creating ensemble")
        self.create_ensemble(self.task_config.SNOWDEPTHVAR,
                             AttrDict({key: self.task_config[key] for key in ['DATA', 'ntiles', 'current_cycle']}))

        logger.info("Running JEDI LETKF")
        self.execute_jediexe(self.task_config.DATA,
                             self.task_config.APRUN_LANDANL,
                             os.path.basename(self.task_config.JEDIEXE),
                             self.task_config.jedi_yaml)

        logger.info("Creating analysis from backgrounds and increments")
        self.add_increments(config)

    @logit(logger)
    def finalize(self: Analysis) -> None:

        logger.info("Create diagnostic tarball of diag*.nc4 files")
        statfile = os.path.join(self.task_config.COM_LAND_ANALYSIS, f"{self.task_config.APREFIX}landstat.tgz")
        self.tar_jedidiags(statfile, self.task_config.DATA)

        logger.info("Copy analysis to COM")
        template = f'{to_fv3time(self.task_config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
        anllist = []
        for itile in range(1, self.task_config.ntiles + 1):
            filename = template.format(tilenum=itile)
            src = os.path.join(self.task_config.DATA, 'anl', filename)
            dest = os.path.join(self.task_config.COM_LAND_ANALYSIS, filename)
            anllist.append([src, dest])
        FileHandler({'copy': anllist}).sync()

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
            Should contain the following keys:
            COM_ATMOS_RESTART_PREV
            DATA
            current_cycle
            ntiles

        Returns
        ----------
        bkg_dict: Dict
            a dictionary containing the list of model background files to copy for FileHandler
        """
        # NOTE for now this is FV3 RESTART files and just assumed to be fh006

        # get FV3 sfc_data RESTART files, this will be a lot simpler when using history files
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

    @logit(logger)
    def get_ens_bkg_dict(self: Analysis, config: Dict) -> Dict:
        """Compile a dictionary of model background files to copy for the ensemble
        Note that a "Fake" 2-member ensemble backgroud is being created by copying FV3 RESTART files (coupler, sfc_data)
        from the deterministic background to DATA/bkg/mem001, 002.

         Parameters
         ----------
         self: Analysis
             Instance of the current object class
         config: Dict
             Dictionary of key-value pairs needed in this method
             Should contain the following keys:
             COM_ATMOS_RESTART_PREV
             DATA
             current_cycle
             ntiles

         Returns
         ----------
         bkg_dict: Dict
             a dictionary containing the list of model background files to copy for FileHandler
         """

        bkglist = []

        # get FV3 sfc_data RESTART files; Note an ensemble is being created
        rst_dir = os.path.join(config.COM_ATMOS_RESTART_PREV)

        for imem in range(1, LandAnalysis.NMEM_LANDENS + 1):
            memchar = f"mem{imem:03d}"

            run_dir = os.path.join(config.DATA, 'bkg', memchar, 'RESTART')

            # Land DA needs coupler
            basename = f'{to_fv3time(config.current_cycle)}.coupler.res'
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

            # Land DA only needs sfc_data
            for ftype in ['sfc_data']:
                template = f'{to_fv3time(config.current_cycle)}.{ftype}.tile{{tilenum}}.nc'
                for itile in range(1, config.ntiles + 1):
                    basename = template.format(tilenum=itile)
                    bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        bkg_dict = {
            'copy': bkglist,
        }

        return bkg_dict

    @logit(logger)
    def create_ensemble(vname: str, config: Dict):
        """Create an ensemble for Snow Depth analysis by perturbing snow depth with a prescribed variance.
        Additionally, remove glacier locations

        Parameters
        ----------
        vname : str
            snow depth variable to perturb. "snowdl" or "snwdph" depending on FRACGRID (YES or NO)
        config: Dict
            Dictionary of key-value pairs needed in this method.  It must contain the following keys:
            DATA
            current_cycle
            ntiles
        """

        # 2 ens members
        offset = LandAnalysis.BESTDDEV/np.sqrt(LandAnalysis.NMEM_LANDENS)

        logger.info(f"Creating ensemble for LETKFOI by offsetting with {offset}")

        workdir = os.path.join(config.DATA, 'bkg')

        sign = [1, -1]
        ens_dirs = ['mem001', 'mem002']

        for (memchar, value) in zip(ens_dirs, sign):
            logger.debug(f"creating ensemble member {memchar} with sign {value}")
            for tt in range(1, config.ntiles+1):
                logger.debug(f"perturbing tile {tt}")
                # open file
                out_netcdf = os.path.join(workdir, memchar, f"{to_fv3time(config.current_cycle)}.sfc_data.tile{tt}.nc")
                with Dataset(out_netcdf, "r+") as ncOut:
                    slmsk_array = ncOut.variables['slmsk'][:]
                    vtype_array = ncOut.variables['vtype'][:]
                    slmsk_array[vtype_array == 15] = 0  # remove glacier locations
                    var_array = ncOut.variables[vname][:]
                    var_array[slmsk_array == 1] = var_array[slmsk_array == 1] + value*offset
                    ncOut.variables[vname][0, :, :] = var_array[:]

    @logit(logger)
    def add_increments(config: Dict) -> None:
        """Executes the program "apply_incr.exe" to create analysis "sfc_data" files by adding increments to backgrounds

        Parameters
        ----------
         config: Dict
             Dictionary of key-value pairs needed in this method
             Should contain the following keys:
             DATA
             current_cycle
             FRACGRID
             CASE
             ntiles
             APPLY_INCR_NML_TMPL
             APPLY_INCR_EXE
             APRUN_APPLY_INCR

        Raises
        ------
        OSError
            Failure due to OS issues
        WorkflowException
            All other exceptions
        """

        logger.info("Copy backgrounds into anl/ directory")
        template = f'{to_fv3time(config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
        anllist = []
        for itile in range(1, config.ntiles + 1):
            filename = template.format(tilenum=itile)
            src = os.path.join(config.DATA, 'bkg', filename)
            dest = os.path.join(config.DATA, 'anl', filename)
            anllist.append([src, dest])
        FileHandler({'copy': anllist}).sync()

        logger.info("Create namelist for APPLY_INCR_EXE")
        nml_template = config.APPLY_INCR_NML_TMPL
        nml_data = Jinja(nml_template, config).render
        logger.debug(f"apply_incr_nml:\n{nml_data}")

        nml_file = os.path.join(config.DATA, "apply_incr_nml")
        with open(nml_file, "w") as fho:
            fho.write(nml_data)

        logger.info("Link APPLY_INCR_EXE into DATA/")
        exe_src = config.APPLY_INCR_EXE
        exe_dest = os.path.join(config.DATA, os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # execute APPLY_INCR_EXE to create analysis files
        exe = Executable(config.APRUN_APPLY_INCR)
        exe.add_default_arg(os.path.join(config.DATA, os.path.basename(exe_src)))
        logger.info(f"Executing {exe}")
        try:
            exe()
        except OSError:
            raise OSError(f"Failed to execute {exe}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exe}")
