#!/usr/bin/env python3

import os
import glob
import gzip
import tarfile
from logging import getLogger
from typing import Dict, List, Any

from wxflow import (AttrDict,
                    FileHandler,
                    add_to_datetime, to_fv3time, to_timedelta,
                    chdir,
                    to_fv3time,
                    YAMLFile, parse_j2yaml, save_as_yaml,
                    logit,
                    Executable,
                    WorkflowException)
from pygfs.task.analysis import Analysis

logger = getLogger(__name__.split('.')[-1])


class AerosolAnalysis(Analysis):
    """
    Class for global aerosol analysis tasks
    """
    @logit(logger, name="AerosolAnalysis")
    def __init__(self, config):
        super().__init__(config)

        _res = int(self.task_config['CASE'][1:])
        _res_anl = int(self.task_config['CASE_ANL'][1:])
        _window_begin = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self.task_config['assim_freq']}H") / 2)
        _jedi_yaml = os.path.join(self.task_config.DATA, f"{self.task_config.RUN}.t{self.task_config['cyc']:02d}z.aerovar.yaml")

        # Create a local dictionary that is repeatedly used across this class
        local_dict = AttrDict(
            {
                'npx_ges': _res + 1,
                'npy_ges': _res + 1,
                'npz_ges': self.task_config.LEVS - 1,
                'npz': self.task_config.LEVS - 1,
                'npx_anl': _res_anl + 1,
                'npy_anl': _res_anl + 1,
                'npz_anl': self.task_config['LEVS'] - 1,
                'AERO_WINDOW_BEGIN': _window_begin,
                'AERO_WINDOW_LENGTH': f"PT{self.task_config['assim_freq']}H",
                'aero_bkg_fhr': map(int, str(self.task_config['aero_bkg_times']).split(',')),
                'OPREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'APREFIX': f"{self.task_config.RUN}.t{self.task_config.cyc:02d}z.",
                'GPREFIX': f"gdas.t{self.task_config.previous_cycle.hour:02d}z.",
                'jedi_yaml': _jedi_yaml,
            }
        )

        # Extend task_config with local_dict
        self.task_config = AttrDict(**self.task_config, **local_dict)

    @logit(logger)
    def initialize(self: Analysis) -> None:
        """Initialize a global aerosol analysis

        This method will initialize a global aerosol analysis using JEDI.
        This includes:
        - staging CRTM fix files
        - staging FV3-JEDI fix files
        - staging B error files
        - staging model backgrounds
        - generating a YAML file for the JEDI executable
        - creating output directories
        """
        super().initialize()

        # stage CRTM fix files
        logger.info(f"Staging CRTM fix files from {self.task_config.CRTM_FIX_YAML}")
        crtm_fix_list = parse_j2yaml(self.task_config.CRTM_FIX_YAML, self.task_config)
        FileHandler(crtm_fix_list).sync()

        # stage fix files
        logger.info(f"Staging JEDI fix files from {self.task_config.JEDI_FIX_YAML}")
        jedi_fix_list = parse_j2yaml(self.task_config.JEDI_FIX_YAML, self.task_config)
        FileHandler(jedi_fix_list).sync()

        # stage files from COM and create working directories
        logger.info(f"Staging files prescribed from {self.task_config.AERO_STAGE_VARIATIONAL_TMPL}")
        aero_var_stage_list = parse_j2yaml(self.task_config.AERO_STAGE_VARIATIONAL_TMPL, self.task_config)
        FileHandler(aero_var_stage_list).sync()

        # generate variational YAML file
        logger.debug(f"Generate variational YAML file: {self.task_config.jedi_yaml}")
        save_as_yaml(self.task_config.jedi_config, self.task_config.jedi_yaml)
        logger.info(f"Wrote variational YAML to: {self.task_config.jedi_yaml}")

    @logit(logger)
    def variational(self: Analysis) -> None:

        chdir(self.task_config.DATA)

        exec_cmd = Executable(self.task_config.APRUN_AEROANL)
        exec_name = os.path.join(self.task_config.DATA, 'gdas.x')
        exec_cmd.add_default_arg(exec_name)
        exec_cmd.add_default_arg('fv3jedi')
        exec_cmd.add_default_arg('variational')
        exec_cmd.add_default_arg(self.task_config.jedi_yaml)

        try:
            logger.debug(f"Executing {exec_cmd}")
            exec_cmd()
        except OSError:
            raise OSError(f"Failed to execute {exec_cmd}")
        except Exception:
            raise WorkflowException(f"An error occured during execution of {exec_cmd}")

        pass

    @logit(logger)
    def finalize(self: Analysis) -> None:
        """Finalize a global aerosol analysis

        This method will finalize a global aerosol analysis using JEDI.
        This includes:
        - tarring up output diag files and place in ROTDIR
        - copying the generated YAML file from initialize to the ROTDIR
        - copying the guess files to the ROTDIR
        - applying the increments to the original RESTART files
        - moving the increment files to the ROTDIR

        """
        # ---- tar up diags
        # path of output tar statfile
        logger.info('Preparing observation space diagnostics for archiving')
        aerostat = os.path.join(self.task_config.COMOUT_CHEM_ANALYSIS, f"{self.task_config['APREFIX']}aerostat")

        # get list of diag files to put in tarball
        diags = glob.glob(os.path.join(self.task_config['DATA'], 'diags', 'diag*nc4'))

        # gzip the files first
        for diagfile in diags:
            logger.info(f'Adding {diagfile} to tar file')
            with open(diagfile, 'rb') as f_in, gzip.open(f"{diagfile}.gz", 'wb') as f_out:
                f_out.writelines(f_in)

        # open tar file for writing
        with tarfile.open(aerostat, "w") as archive:
            for diagfile in diags:
                diaggzip = f"{diagfile}.gz"
                archive.add(diaggzip, arcname=os.path.basename(diaggzip))
        logger.info(f'Saved diags to {aerostat}')

        # ---- add increments to RESTART files
        logger.info('Adding increments to RESTART files')
        self._add_fms_cube_sphere_increments()

        # copy files back to COM
        logger.info(f"Copying files to COM based on {self.task_config.AERO_FINALIZE_VARIATIONAL_TMPL}")
        aero_var_final_list = parse_j2yaml(self.task_config.AERO_FINALIZE_VARIATIONAL_TMPL, self.task_config)
        FileHandler(aero_var_final_list).sync()

    def clean(self):
        super().clean()

    @logit(logger)
    def _add_fms_cube_sphere_increments(self: Analysis) -> None:
        """This method adds increments to RESTART files to get an analysis
        """
        if self.task_config.DOIAU:
            bkgtime = self.task_config.AERO_WINDOW_BEGIN
        else:
            bkgtime = self.task_config.current_cycle
        # only need the fv_tracer files
        restart_template = f'{to_fv3time(bkgtime)}.fv_tracer.res.tile{{tilenum}}.nc'
        increment_template = f'{to_fv3time(self.task_config.current_cycle)}.fv_tracer.res.tile{{tilenum}}.nc'
        inc_template = os.path.join(self.task_config.DATA, 'anl', 'aeroinc.' + increment_template)
        bkg_template = os.path.join(self.task_config.DATA, 'anl', restart_template)
        # get list of increment vars
        incvars_list_path = os.path.join(self.task_config['PARMgfs'], 'gdas', 'aeroanl_inc_vars.yaml')
        incvars = YAMLFile(path=incvars_list_path)['incvars']
        super().add_fv3_increments(inc_template, bkg_template, incvars)

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
        rst_dir = task_config.COMIN_ATMOS_RESTART_PREV
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

    @logit(logger)
    def get_berror_dict(self, config: Dict[str, Any]) -> Dict[str, List[str]]:
        """Compile a dictionary of background error files to copy

        This method will construct a dictionary of BUMP background error files
        for global aerosol DA and return said dictionary for use by the FileHandler class.
        This dictionary contains coupler and fv_tracer files
        for correlation and standard deviation as well as NICAS localization.

        Parameters
        ----------
        config: Dict
            a dictionary containing all of the configuration needed

        Returns
        ----------
        berror_dict: Dict
            a dictionary containing the list of background error files to copy for FileHandler
        """
        # aerosol static-B needs nicas, cor_rh, cor_rv and stddev files.
        b_datestr = to_fv3time(config.BERROR_DATE)
        analysis_dir = config.COMIN_CHEM_BMATRIX
        cycle_datestr = to_fv3time(config.current_cycle)
        berror_list = []

        # the stddev is computed every cycle and is available in COM
        for ftype in ['stddev']:
            coupler = f'{cycle_datestr}.{ftype}.coupler.res'
            berror_list.append([
                os.path.join(analysis_dir, coupler), os.path.join(config.DATA, 'berror', coupler)
            ])
            template = f'{cycle_datestr}.{ftype}.fv_tracer.res.tile{{tilenum}}.nc'
            for itile in range(1, config.ntiles + 1):
                tracer = template.format(tilenum=itile)
                berror_list.append([
                    os.path.join(analysis_dir, tracer), os.path.join(config.DATA, 'berror', tracer)
                ])

        # the diffusion correlation files are computed every cycle and are available in COM
        diff_hz = 'diffusion_hz.nc'
        diff_vt = 'diffusion_vt.nc'
        berror_list.append([
            os.path.join(config.COMIN_CHEM_BMATRIX, diff_hz), os.path.join(config.DATA, 'berror', diff_hz)
        ])
        berror_list.append([
            os.path.join(config.COMIN_CHEM_BMATRIX, diff_vt), os.path.join(config.DATA, 'berror', diff_vt)
        ])

        berror_dict = {
            'mkdir': [os.path.join(config.DATA, 'berror')],
            'copy': berror_list,
        }
        return berror_dict
