#!/usr/bin/env python3

import os
from logging import getLogger
from typing import Dict, List
from pprint import pformat
import numpy as np
from netCDF4 import Dataset

from wxflow import (AttrDict,
                    FileHandler,
                    to_fv3time, to_YMD, to_YMDH, to_timedelta, add_to_datetime,
                    rm_p,
                    parse_j2yaml, save_as_yaml,
                    Jinja,
                    logit,
                    Executable,
                    WorkflowException)
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class SnowAnalysis(Analysis):
    """
    Class for global snow analysis tasks
    """

    NMEM_SNOWENS = 2

    @logit(logger, name="SnowAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _res = int(self.task_config['CASE'][1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config['assim_freq']}H") / 2)
        _letkfoi_yaml = os.path.join(self.task_config.DATA, f"{self.task_config.RUN}.t{self.task_config['cyc']:02d}z.letkfoi.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'SNOW_WINDOW_BEGIN': _window_begin,
                'SNOW_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'jedi_yaml': _letkfoi_yaml
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def prepare_IMS(self) -> None:
        """Prepare the IMS data for a global snow analysis

        This method will prepare IMS data for a global snow analysis using JEDI.
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
        localconf = AttrDict()
        keys = ['DATA', 'current_cycle', 'COM_OBS', 'COM_ATMOS_RESTART_PREV',
                'OPREFIX', 'CASE', 'OCNRES', 'ntiles', 'FIXgfs']
        for key in keys:
            localconf[key] = self.task_config[key]

        # stage backgrounds
        logger.info("Staging backgrounds")
        FileHandler(self.get_bkg_dict(localconf)).sync()

        # Read and render the IMS_OBS_LIST yaml
        logger.info(f"Reading {self.task_config.IMS_OBS_LIST}")
        prep_ims_config = parse_j2yaml(self.task_config.IMS_OBS_LIST, localconf)
        logger.debug(f"{self.task_config.IMS_OBS_LIST}:\n{pformat(prep_ims_config)}")

        # copy the IMS obs files from COM_OBS to DATA/obs
        logger.info("Copying IMS obs for CALCFIMSEXE")
        FileHandler(prep_ims_config.calcfims).sync()

        logger.info("Create namelist for CALCFIMSEXE")
        nml_template = self.task_config.FIMS_NML_TMPL
        nml_data = Jinja(nml_template, localconf).render
        logger.debug(f"fims.nml:\n{nml_data}")

        nml_file = os.path.join(localconf.DATA, "fims.nml")
        with open(nml_file, "w") as fho:
            fho.write(nml_data)

        logger.info("Link CALCFIMSEXE into DATA/")
        exe_src = self.task_config.CALCFIMSEXE
        exe_dest = os.path.join(localconf.DATA, os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        # execute CALCFIMSEXE to calculate IMS snowdepth
        exe = Executable(self.task_config.APRUN_CALCFIMS)
        exe.add_default_arg(os.path.join(localconf.DATA, os.path.basename(exe_src)))
        logger.info(f"Executing {exe}")
        try:
            exe()
        except OSError:
            raise OSError(f"Failed to execute {exe}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exe}")

        # Ensure the snow depth IMS file is produced by the above executable
        input_file = f"IMSscf.{to_YMD(localconf.current_cycle)}.{localconf.CASE}_oro_data.nc"
        if not os.path.isfile(f"{os.path.join(localconf.DATA, input_file)}"):
            logger.exception(f"{self.task_config.CALCFIMSEXE} failed to produce {input_file}")
            raise FileNotFoundError(f"{os.path.join(localconf.DATA, input_file)}")

        # Execute imspy to create the IMS obs data in IODA format
        logger.info("Create IMS obs data in IODA format")

        output_file = f"ims_snow_{to_YMDH(localconf.current_cycle)}.nc4"
        if os.path.isfile(f"{os.path.join(localconf.DATA, output_file)}"):
            rm_p(output_file)

        exe = Executable(self.task_config.IMS2IODACONV)
        exe.add_default_arg(["-i", f"{os.path.join(localconf.DATA, input_file)}"])
        exe.add_default_arg(["-o", f"{os.path.join(localconf.DATA, output_file)}"])
        try:
            logger.debug(f"Executing {exe}")
            exe()
        except OSError:
            raise OSError(f"Failed to execute {exe}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exe}")

        # Ensure the IODA snow depth IMS file is produced by the IODA converter
        # If so, copy to COM_OBS/
        if not os.path.isfile(f"{os.path.join(localconf.DATA, output_file)}"):
            logger.exception(f"{self.task_config.IMS2IODACONV} failed to produce {output_file}")
            raise FileNotFoundError(f"{os.path.join(localconf.DATA, output_file)}")
        else:
            logger.info(f"Copy {output_file} to {self.task_config.COM_OBS}")
            FileHandler(prep_ims_config.ims2ioda).sync()

    @logit(logger)
    def initialize(self) -> None:
        """Initialize method for snow analysis
        This method:
        - creates artifacts in the DATA directory by copying fix files
        - creates the JEDI LETKF yaml from the template
        - stages backgrounds, observations and ensemble members

        Parameters
        ----------
        self : Analysis
            Instance of the SnowAnalysis object
        """

        super().initialize()

        # create a temporary dict of all keys needed in this method
        localconf = AttrDict()
        keys = ['PARMgfs', 'DATA', 'current_cycle', 'COM_OBS', 'COM_ATMOS_RESTART_PREV',
                'OPREFIX', 'CASE', 'OCNRES', 'ntiles']
        for key in keys:
            localconf[key] = self.task_config[key]

        # Make member directories in DATA for background
        dirlist = []
        for imem in range(1, SnowAnalysis.NMEM_SNOWENS + 1):
            dirlist.append(os.path.join(localconf.DATA, 'bkg', f'mem{imem:03d}'))
        FileHandler({'mkdir': dirlist}).sync()

        # stage fix files
        logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
        jedi_fix_list = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage backgrounds
        logger.info("Staging ensemble backgrounds")
        FileHandler(self.get_ens_bkg_dict(localconf)).sync()

        # stage GTS bufr2ioda mapping YAML files
        logger.info(f"Staging GTS bufr2ioda mapping YAML files from {self.task_config.GTS_SNOW_STAGE_YAML}")
        gts_mapping_list = parse_j2yaml(self.task_config.GTS_SNOW_STAGE_YAML, localconf)
        FileHandler(gts_mapping_list).sync()

        # Write out letkfoi YAML file
        save_as_yaml(self.task_config.jedi_config, self.task_config.jedi_yaml)
        logger.info(f"Wrote letkfoi YAML to: {self.task_config.jedi_yaml}")

        # need output dir for diags and anl
        logger.info("Create empty output [anl, diags] directories to receive output from executable")
        newdirs = [
            os.path.join(localconf.DATA, "anl"),
            os.path.join(localconf.DATA, "diags"),
        ]
        FileHandler({'mkdir': newdirs}).sync()

    @logit(logger)
    def execute(self) -> None:
        """Run a series of tasks to create Snow analysis
        This method:
        - creates an 2 member ensemble
        - runs the JEDI LETKF executable to produce increments
        - creates analysis from increments

        Parameters
        ----------
        self : Analysis
           Instance of the SnowAnalysis object
        """

        # create a temporary dict of all keys needed in this method
        localconf = AttrDict()
        keys = ['HOMEgfs', 'DATA', 'current_cycle',
                'COM_ATMOS_RESTART_PREV', 'COM_SNOW_ANALYSIS', 'APREFIX',
                'SNOWDEPTHVAR', 'BESTDDEV', 'CASE', 'OCNRES', 'ntiles',
                'APRUN_SNOWANL', 'JEDIEXE', 'jedi_yaml', 'DOIAU', 'SNOW_WINDOW_BEGIN',
                'APPLY_INCR_NML_TMPL', 'APPLY_INCR_EXE', 'APRUN_APPLY_INCR']
        for key in keys:
            localconf[key] = self.task_config[key]

        logger.info("Creating ensemble")
        self.create_ensemble(localconf.SNOWDEPTHVAR,
                             localconf.BESTDDEV,
                             AttrDict({key: localconf[key] for key in ['DATA', 'ntiles', 'current_cycle']}))

        logger.info("Running JEDI LETKF")
        exec_cmd = Executable(localconf.APRUN_SNOWANL)
        exec_name = os.path.join(localconf.DATA, 'gdas.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('fv3jedi')
        exec_cmd.add_default_arg('localensembleda')
        exec_cmd.add_default_arg(localconf.jedi_yaml)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

        logger.info("Creating analysis from backgrounds and increments")
        self.add_increments(localconf)

    @logit(logger)
    def finalize(self) -> None:
        """Performs closing actions of the Snow analysis task
        This method:
        - tar and gzip the output diag files and place in COM/
        - copy the generated YAML file from initialize to the COM/
        - copy the analysis files to the COM/
        - copy the increment files to the COM/

        Parameters
        ----------
        self : Analysis
            Instance of the SnowAnalysis object
        """

        logger.info("Create diagnostic tarball of diag*.nc4 files")
        statfile = os.path.join(self.task_config.COM_SNOW_ANALYSIS, f"{self.task_config.APREFIX}snowstat.tgz")
        self.tgz_diags(statfile, self.task_config.DATA)

        logger.info("Copy full YAML to COM")
        src = os.path.join(self.task_config['DATA'], f"{self.task_config.APREFIX}letkfoi.yaml")
        dest = os.path.join(self.task_config.COM_CONF, f"{self.task_config.APREFIX}letkfoi.yaml")
        yaml_copy = {
            'mkdir': [self.task_config.COM_CONF],
            'copy': [[src, dest]]
        }
        FileHandler(yaml_copy).sync()

        logger.info("Copy analysis to COM")
        bkgtimes = []
        if self.task_config.DOIAU:
            # need both beginning and middle of window
            bkgtimes.append(self.task_config.SNOW_WINDOW_BEGIN)
        bkgtimes.append(self.task_config.current_cycle)
        anllist = []
        for bkgtime in bkgtimes:
            template = f'{to_fv3time(bkgtime)}.sfc_data.tile{{tilenum}}.nc'
            for itile in range(1, self.task_config.ntiles + 1):
                filename = template.format(tilenum=itile)
                src = os.path.join(self.task_config.DATA, 'anl', filename)
                dest = os.path.join(self.task_config.COM_SNOW_ANALYSIS, filename)
                anllist.append([src, dest])
        FileHandler({'copy': anllist}).sync()

        logger.info('Copy increments to COM')
        template = f'snowinc.{to_fv3time(self.task_config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
        inclist = []
        for itile in range(1, self.task_config.ntiles + 1):
            filename = template.format(tilenum=itile)
            src = os.path.join(self.task_config.DATA, 'anl', filename)
            dest = os.path.join(self.task_config.COM_SNOW_ANALYSIS, filename)
            inclist.append([src, dest])
        FileHandler({'copy': inclist}).sync()

    @staticmethod
    @logit(logger)
    def get_bkg_dict(config: Dict) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method constructs a dictionary of FV3 RESTART files (coupler, sfc_data)
        that are needed for global snow DA and returns said dictionary for use by the FileHandler class.

        Parameters
        ----------
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

        # snow DA needs coupler
        basename = f'{to_fv3time(config.current_cycle)}.coupler.res'
        bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        # snow DA only needs sfc_data
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

    @staticmethod
    @logit(logger)
    def get_ens_bkg_dict(config: Dict) -> Dict:
        """Compile a dictionary of model background files to copy for the ensemble
        Note that a "Fake" 2-member ensemble backgroud is being created by copying FV3 RESTART files (coupler, sfc_data)
        from the deterministic background to DATA/bkg/mem001, 002.

         Parameters
         ----------
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

        dirlist = []
        bkglist = []

        # get FV3 sfc_data RESTART files; Note an ensemble is being created
        rst_dir = os.path.join(config.COM_ATMOS_RESTART_PREV)

        for imem in range(1, SnowAnalysis.NMEM_SNOWENS + 1):
            memchar = f"mem{imem:03d}"

            run_dir = os.path.join(config.DATA, 'bkg', memchar, 'RESTART')
            dirlist.append(run_dir)

            # Snow DA needs coupler
            basename = f'{to_fv3time(config.current_cycle)}.coupler.res'
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

            # Snow DA only needs sfc_data
            for ftype in ['sfc_data']:
                template = f'{to_fv3time(config.current_cycle)}.{ftype}.tile{{tilenum}}.nc'
                for itile in range(1, config.ntiles + 1):
                    basename = template.format(tilenum=itile)
                    bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        bkg_dict = {
            'mkdir': dirlist,
            'copy': bkglist
        }

        return bkg_dict

    @staticmethod
    @logit(logger)
    def create_ensemble(vname: str, bestddev: float, config: Dict) -> None:
        """Create a 2-member ensemble for Snow Depth analysis by perturbing snow depth with a prescribed variance.
        Additionally, remove glacier locations

        Parameters
        ----------
        vname : str
            snow depth variable to perturb: "snodl"
        bestddev : float
            Background Error Standard Deviation to perturb around to create ensemble
        config: Dict
            Dictionary of key-value pairs needed in this method.  It must contain the following keys:
            DATA
            current_cycle
            ntiles
        """

        # 2 ens members
        offset = bestddev / np.sqrt(SnowAnalysis.NMEM_SNOWENS)

        logger.info(f"Creating ensemble for LETKFOI by offsetting with {offset}")

        workdir = os.path.join(config.DATA, 'bkg')

        sign = [1, -1]
        ens_dirs = ['mem001', 'mem002']

        for (memchar, value) in zip(ens_dirs, sign):
            logger.debug(f"creating ensemble member {memchar} with sign {value}")
            for tt in range(1, config.ntiles + 1):
                logger.debug(f"perturbing tile {tt}")
                # open file
                out_netcdf = os.path.join(workdir, memchar, 'RESTART', f"{to_fv3time(config.current_cycle)}.sfc_data.tile{tt}.nc")
                logger.debug(f"creating member {out_netcdf}")
                with Dataset(out_netcdf, "r+") as ncOut:
                    slmsk_array = ncOut.variables['slmsk'][:]
                    vtype_array = ncOut.variables['vtype'][:]
                    slmsk_array[vtype_array == 15] = 0  # remove glacier locations
                    var_array = ncOut.variables[vname][:]
                    var_array[slmsk_array == 1] = var_array[slmsk_array == 1] + value * offset
                    ncOut.variables[vname][0, :, :] = var_array[:]

    @staticmethod
    @logit(logger)
    def add_increments(config: Dict) -> None:
        """Executes the program "apply_incr.exe" to create analysis "sfc_data" files by adding increments to backgrounds

        Parameters
        ----------
         config: Dict
             Dictionary of key-value pairs needed in this method
             Should contain the following keys:
             HOMEgfs
             COM_ATMOS_RESTART_PREV
             DATA
             current_cycle
             CASE
             OCNRES
             ntiles
             APPLY_INCR_NML_TMPL
             APPLY_INCR_EXE
             APRUN_APPLY_INCR
             DOIAU
             SNOW_WINDOW_BEGIN

        Raises
        ------
        OSError
            Failure due to OS issues
        WorkflowException
            All other exceptions
        """

        # need backgrounds to create analysis from increments after LETKF
        logger.info("Copy backgrounds into anl/ directory for creating analysis from increments")
        bkgtimes = []
        if config.DOIAU:
            # want analysis at beginning and middle of window
            bkgtimes.append(config.SNOW_WINDOW_BEGIN)
        bkgtimes.append(config.current_cycle)
        anllist = []
        for bkgtime in bkgtimes:
            template = f'{to_fv3time(bkgtime)}.sfc_data.tile{{tilenum}}.nc'
            for itile in range(1, config.ntiles + 1):
                filename = template.format(tilenum=itile)
                src = os.path.join(config.COM_ATMOS_RESTART_PREV, filename)
                dest = os.path.join(config.DATA, "anl", filename)
                anllist.append([src, dest])
        FileHandler({'copy': anllist}).sync()

        if config.DOIAU:
            logger.info("Copying increments to beginning of window")
            template_in = f'snowinc.{to_fv3time(config.current_cycle)}.sfc_data.tile{{tilenum}}.nc'
            template_out = f'snowinc.{to_fv3time(config.SNOW_WINDOW_BEGIN)}.sfc_data.tile{{tilenum}}.nc'
            inclist = []
            for itile in range(1, config.ntiles + 1):
                filename_in = template_in.format(tilenum=itile)
                filename_out = template_out.format(tilenum=itile)
                src = os.path.join(config.DATA, 'anl', filename_in)
                dest = os.path.join(config.DATA, 'anl', filename_out)
                inclist.append([src, dest])
            FileHandler({'copy': inclist}).sync()

        # loop over times to apply increments
        for bkgtime in bkgtimes:
            logger.info("Processing analysis valid: {bkgtime}")
            logger.info("Create namelist for APPLY_INCR_EXE")
            nml_template = config.APPLY_INCR_NML_TMPL
            nml_config = {
                'current_cycle': bkgtime,
                'CASE': config.CASE,
                'DATA': config.DATA,
                'HOMEgfs': config.HOMEgfs,
                'OCNRES': config.OCNRES,
            }
            nml_data = Jinja(nml_template, nml_config).render
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
