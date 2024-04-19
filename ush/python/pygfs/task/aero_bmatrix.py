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

        _bmat_yaml = os.path.join(self.runtime_config.DATA, f"{self.runtime_config.CDUMP}.t{self.runtime_config['cyc']:02d}z.chem_diagb.yaml")

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
                'OPREFIX': f"{self.runtime_config.CDUMP}.t{self.runtime_config.cyc:02d}z.",  # TODO: CDUMP is being replaced by RUN
                'APREFIX': f"{self.runtime_config.CDUMP}.t{self.runtime_config.cyc:02d}z.",  # TODO: CDUMP is being replaced by RUN
                'GPREFIX': f"gdas.t{self.runtime_config.previous_cycle.hour:02d}z.",
                'bmat_yaml': _bmat_yaml,
            }
        )

        # task_config is everything that this task should need
        self.task_config = AttrDict(**self.config, **self.runtime_config, **local_dict)

    @logit(logger)
    def initialize(self: BMatrix) -> None:
        super().initialize()
        # stage fix files
        logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
        jedi_fix_list = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage berror files
        # copy BUMP files, otherwise it will assume ID matrix
        if self.task_config.get('STATICB_TYPE', 'identity') in ['bump']:
            FileHandler(self.get_berror_dict(self.task_config)).sync()

        # stage backgrounds
        FileHandler(self.get_bkg_dict(AttrDict(self.task_config, **self.task_config))).sync()

        # generate diagb YAML file
        logger.debug(f"Generate bmat YAML file: {self.task_config.bmat_yaml}")
        save_as_yaml(self.task_config.bmat_config, self.task_config.bmat_yaml)
        logger.info(f"Wrote bmat YAML to: {self.task_config.bmat_yaml}")

        # create output directory
        FileHandler({'mkdir': [os.path.join(self.task_config['DATA'], 'stddev')]}).sync()

        # link executable to run directory
        self.link_bmatexe()

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
    def finalize(self) -> None:
        super().finalize()
        # copy full YAML from executable to ROTDIR
        src = os.path.join(self.task_config['DATA'], f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.chem_diagb.yaml")
        dest = os.path.join(self.task_config.COM_CHEM_ANALYSIS, f"{self.task_config['CDUMP']}.t{self.runtime_config['cyc']:02d}z.chem_diagb.yaml")
        yaml_copy = {
            'mkdir': [self.task_config.COM_CHEM_ANALYSIS],
            'copy': [[src, dest]]
        }
        FileHandler(yaml_copy).sync()

        # copy stddev files to ROTDIR
        logger.info('Copying std. dev. files to ROTDIR')
        template = f'{to_fv3time(self.task_config.current_cycle)}.stddev.fv_tracer.res.tile{{tilenum}}.nc'
        inclist = []
        for itile in range(1, self.task_config.ntiles + 1):
            tracer = template.format(tilenum=itile)
            src = os.path.join(self.task_config.DATA, 'stddev', tracer)
            dest = os.path.join(self.task_config.COM_CHEM_ANALYSIS, tracer)
            inclist.append([src, dest])
        FileHandler({'copy': inclist}).sync()

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
