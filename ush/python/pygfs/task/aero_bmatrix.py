#!/usr/bin/env python3

import os
from logging import getLogger
from typing import List, Dict, Any, Union

from wxflow import (AttrDict, FileHandler, rm_p,
                    add_to_datetime, to_fv3time, to_timedelta,
                    to_fv3time, chdir, Executable, WorkflowException,
                    parse_j2yaml, save_as_yaml, logit)
from pygfs.task.bmatrix import BMatrix

logger = getLogger(__name__.split('.')[-1])


class AerosolBMatrix(BMatrix):
    """
    Class for global aerosol BMatrix tasks
    """
    @logit(logger, name="AerosolBMatrix")
    def __init__(self, config: Dict[str, Any]) -> None:
        super().__init__(config)

        _res = int(self.config['CASE'][1:])
        _res_anl = int(self.config['CASE_ANL'][1:])

        _bmat_yaml = os.path.join(self.task_config.DATA, f"{self.task_config.CDUMP}.t{self.task_config['cyc']:02d}z.chem_diagb.yaml")
        _diffusion_yaml = os.path.join(self.task_config.DATA, f"{self.task_config.CDUMP}.t{self.task_config['cyc']:02d}z.chem_diffusion.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.config.LEVS - 1,
                'npz': self.config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.config['LEVS'] - 1,
                'aero_bkg_fhr': map(int, str(self.config['aero_bkg_times']).split(',')),
                'OPREFIX': f"{self.task_config.CDUMP}.t{self.task_config.cyc:02d}z.",  # TODO: CDUMP is being replaced by RUN
                'APREFIX': f"{self.task_config.CDUMP}.t{self.task_config.cyc:02d}z.",  # TODO: CDUMP is being replaced by RUN
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'bmat_yaml': _bmat_yaml,
                'diffusion_yaml': _diffusion_yaml,
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(.
        **self.task_config, **local_dict)

    @logit(logger)
    def initialize(self: BMatrix) -> None:
        super().initialize()
        # stage fix files
        logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
        jedi_fix_list = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage backgrounds
        logger.info(f"Staging backgrounds prescribed from {self.task_config.AERO_BMATRIX_STAGE_TMPL}")
        aero_bmat_stage_list = parse_j2yaml(self.task_config.AERO_BMATRIX_STAGE_TMPL, self.task_config)
        FileHandler(aero_bmat_stage_list).sync()

        # generate diagb YAML file
        logger.info(f"Generate bmat YAML file: {self.task_config.bmat_yaml}")
        save_as_yaml(self.task_config.bmat_config, self.task_config.bmat_yaml)
        logger.info(f"Wrote bmat YAML to: {self.task_config.bmat_yaml}")

        # generate diffusion parameters YAML file
        logger.info(f"Generate diffusion YAML file: {self.task_config.diffusion_yaml}")
        save_as_yaml(self.task_config.diffusion_config, self.task_config.diffusion_yaml)
        logger.info(f"Wrote diffusion YAML to: {self.task_config.diffusion_yaml}")

        # create output directory
        FileHandler({'mkdir': [os.path.join(self.task_config['DATA'], 'stddev')]}).sync()

        # create diffusion output directory
        FileHandler({'mkdir': [os.path.join(self.task_config['DATA'], 'diffusion')]}).sync()

        # link executable to run directory
        logger.info(f'before link_bmatexe')
        self.link_bmatexe()
        logger.info(f'before link_diffusion_exe')
        self.link_diffusion_exe()

    @logit(logger)
    def computeVariance(self) -> None:

        chdir(self.task_config.DATA)

        exec_cmd = Executable(self.task_config.APRUN_AEROGENB)
        exec_name = os.path.join(self.task_config.DATA, 'gdasapp_chem_diagb.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg(self.task_config.bmat_yaml)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

        pass

    @logit(logger)
    def computeDiffusion(self) -> None:

        chdir(self.task_config.DATA)

        exec_cmd_diffusion = Executable(self.task_config.APRUN_AEROGENB)
        exec_name_diffusion = os.path.join(self.task_config.DATA, 'gdas_fv3jedi_error_covariance_toolbox.x')
        exec_cmd_diffusion.add_default_arg(exec_name_diffusion)
        exec_cmd_diffusion.add_default_arg(self.task_config.diffusion_yaml)

        try:
            logger.debug(f"Executing {exec_cmd_diffusion}")
            exec_cmd_diffusion()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd_diffusion}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd_diffusion}")

        pass

    @logit(logger)
    def finalize(self) -> None:
        super().finalize()
        # copy full YAML from executable to ROTDIR
        src = os.path.join(self.task_config['DATA'], f"{self.task_config['CDUMP']}.t{self.task_config['cyc']:02d}z.chem_diagb.yaml")
        dest = os.path.join(self.task_config.COM_CHEM_ANALYSIS, f"{self.task_config['CDUMP']}.t{self.task_config['cyc']:02d}z.chem_diagb.yaml")
        src_diffusion = os.path.join(self.task_config['DATA'], f"{self.task_config['CDUMP']}.t{self.task_config['cyc']:02d}z.chem_diffusion.yaml")
        dest_diffusion = os.path.join(self.task_config.COM_CHEM_ANALYSIS, f"{self.task_config['CDUMP']}.t{self.task_config['cyc']:02d}z.chem_diffusion.yaml")
        yaml_copy = {
            'mkdir': [self.task_config.COM_CHEM_ANALYSIS],
            'copy': [[src, dest], [src_diffusion, dest_diffusion]]
        }
        FileHandler(yaml_copy).sync()

        # copy stddev files to ROTDIR
        logger.info('Copying std. dev. files to ROTDIR')
        template = f'{to_fv3time(self.task_config.current_cycle)}.stddev.fv_tracer.res.tile{{tilenum}}.nc'
        stddevlist = []
        # actual std dev tracer files
        for itile in range(1, self.task_config.ntiles + 1):
            tracer = template.format(tilenum=itile)
            src = os.path.join(self.task_config.DATA, 'stddev', tracer)
            dest = os.path.join(self.task_config.COM_CHEM_ANALYSIS, tracer)
            stddevlist.append([src, dest])
        # coupler file
        coupler = f'{to_fv3time(self.task_config.current_cycle)}.stddev.coupler.res'
        src = os.path.join(self.task_config.DATA, 'stddev', coupler)
        dest = os.path.join(self.task_config.COM_CHEM_ANALYSIS, coupler)
        stddevlist.append([src, dest])

        FileHandler({'copy': stddevlist}).sync()

        # Diffusion files
        diff_hz = 'diffusion_hz.nc'
        diff_vt = 'diffusion_vt.nc'
        src_hz = os.path.join(self.task_config.DATA, 'diffusion', diff_hz)
        dest_hz = os.path.join(self.task_config.COM_CHEM_ANALYSIS, diff_hz)
        src_vt = os.path.join(self.task_config.DATA, 'diffusion', diff_vt)
        dest_vt = os.path.join(self.task_config.COM_CHEM_ANALYSIS, diff_vt)
        difflist = [[src_hz, dest_hz], [src_vt, dest_vt]]
        FileHandler({'copy': difflist}).sync()

    @logit(logger)
    def link_bmatexe(self) -> None:
        """

        This method links a JEDI executable to the run directory

        Parameters
        ----------
        Task: GDAS task

        Returns
        ----------
        None
        """
        exe_src = self.task_config.BMATEXE

        # TODO: linking is not permitted per EE2.  Needs work in JEDI to be able to copy the exec.
        logger.info(f"Link executable {exe_src} to DATA/")
        logger.warn("Linking is not permitted per EE2.")
        exe_dest = os.path.join(self.task_config.DATA, os.path.basename(exe_src))
        if os.path.exists(exe_dest):
            rm_p(exe_dest)
        os.symlink(exe_src, exe_dest)

        return

    @logit(logger)
    def link_diffusion_exe(self) -> None:
        """

        This method links a JEDI (fv3jedi_error_covariance_toolbox.x)
        executable to the run directory

        Parameters
        ----------
        Task: GDAS task

        Returns
        ----------
        None
        """

        exe_src_diffusion = self.task_config.DIFFUSIONEXE

        # TODO: linking is not permitted per EE2.  Needs work in JEDI to be able to copy the exec.
        logger.info(f"Link executable {exe_src_diffusion} to DATA/")
        logger.warn("Linking is not permitted per EE2.")
        exe_dest_diffusion = os.path.join(self.task_config.DATA, os.path.basename(exe_src_diffusion))
        if os.path.exists(exe_dest_diffusion):
            rm_p(exe_dest_diffusion)
        os.symlink(exe_src_diffusion, exe_dest_diffusion)

        return

    @logit(logger)
    def get_bkg_dict(self, task_config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of model background files to copy

        This method constructs a dictionary of FV3 RESTART files (coupler, core, tracer)
        that are needed for global aerosol DA and returns said dictionary for use by the FileHandler class.

        Parameters
        ----------
        task_config: Dict
            a dictionary containing all of the configuration needed for the task

        Returns
        ----------
        bkg_dict: Dict
            a dictionary containing the list of model background files to copy for FileHandler
        """
        # NOTE for now this is FV3 RESTART files and just assumed to be fh006

        # get FV3 RESTART files, this will be a lot simpler when using history files
        rst_dir = task_config.COM_ATMOS_RESTART_PREV
        run_dir = os.path.join(task_config['DATA'], 'bkg')

        # Start accumulating list of background files to copy
        bkglist = []

        # if using IAU, we can use FGAT
        bkgtimes = []
        begintime = task_config.previous_cycle
        for fcsthr in task_config.aero_bkg_fhr:
            bkgtimes.append(add_to_datetime(begintime, to_timedelta(f"{fcsthr}H")))

        # now loop over background times
        for bkgtime in bkgtimes:
            # aerosol DA needs coupler
            basename = f'{to_fv3time(bkgtime)}.coupler.res'
            bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

            # aerosol DA only needs core/tracer
            for ftype in ['core', 'tracer']:
                template = f'{to_fv3time(bkgtime)}.fv_{ftype}.res.tile{{tilenum}}.nc'
                for itile in range(1, task_config.ntiles + 1):
                    basename = template.format(tilenum=itile)
                    bkglist.append([os.path.join(rst_dir, basename), os.path.join(run_dir, basename)])

        bkg_dict = {
            'mkdir': [run_dir],
            'copy': bkglist,
        }
        return bkg_dict
